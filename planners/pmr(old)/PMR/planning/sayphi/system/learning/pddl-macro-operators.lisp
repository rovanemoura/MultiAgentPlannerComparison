;; DB. 2014 rewrite of the generator of macro-operators for IPSS/Prodigy
(in-package "COMMON-LISP-USER")

(defvar *macro-substitution* nil)
(defvar *use-cost-in-macros-p* nil "It f, it will assign the total cost of the macro as the lenght of the original plan")

;; Example of use:
;; 1. (domain 'blocksworld)
;; 2. (problem 'suss)
;; 3. (run)
;; 4. (learn-macro 'blocksworld)
;;  It will generate a file macros.lisp in the blocksworld directory, that
;; you can insert into the domain file (before the first operator definition) if parameter install-macro-p is t

;; I assume actions are loaded in memory (by using say-domain)
;; style can be prodigy or pddl
;; install-macro-p only works for prodigy now
;; output-format can be: file or list
(defun learn-macro (input-plan &key domain (domain-dir (format nil "~a~a/" *domains-dir* domain)) (style 'pddl)
				 (output-format 'list) (install-macro-p nil) (tracep nil)
				 (first-step-number 1) (last-step-number (length input-plan)))
  (if input-plan
      (let* ((action-defs (dom-actions *pspace*))
	     (plan (visible-plan (subseq input-plan (1- first-step-number) last-step-number) action-defs style))
	     (macro (generate-new-op plan (create-macro (car plan)) (if (eq style 'prodigy) (get-params-from-plan plan))
				     domain-dir style output-format tracep)))
	(when install-macro-p
	  (eval `(shell ,(format nil "cp -p ~adomain.lisp ~adomain-without-macros.lisp" domain-dir domain-dir)))
	  (with-open-file (istream (format nil "~amacros.lisp" domain-dir) :direction :input)
	    (do ((macro (read istream nil 'eof) (read istream nil 'eof))
		 (macros nil))
		((eq macro 'eof)
		 (with-open-file (istream2 (format nil "~adomain-without-macros.lisp" domain-dir)
					   :direction :input)
		   (with-open-file (ostream (format nil "~adomain.lisp" domain-dir)
					    :direction :output
					    :if-exists :supersede
					    :if-does-not-exist :create)
		     (do ((domain-def (read istream2 nil 'eof) (read istream2 nil 'eof))
			  (headerp t))
			 ((eq domain-def 'eof))
		       (if (and (listp domain-def)
				(eq (car domain-def) 'operator))
			   (cond (headerp (setq headerp nil)
					  (format ostream "~2%")
					  (format ostream "~(~a~)" (car macros))
					  (format ostream "~2%~(~s~)" domain-def))
				 (t (format ostream "~2%~(~s~)" domain-def)))
			   (format ostream "~2%~(~s~)" domain-def))))))
	      (push macro macros))))
	macro)))
		     

;;; *********************************************************************************
;;;     Functions for computing the visible form of a Prodigy plan
;;; *********************************************************************************

;; returns in the case of PDDL, a list of a plan (action params preconds adds dels), a plan substitution ((obj . var)*)
;; and a params-substitution ((obj . var type)*)
(defun visible-plan (solution action-defs style)
  (let ((plan nil)
	(op nil)
	(op-name nil)
	(params nil)
	(preconds nil)
	(applied-node nil)
	(adds nil)
	(dels nil)
	(var nil)
	(substitution nil)
	(plan-substitution nil)
	(params-substitution nil)
	(binding-node nil))
    (setf *macro-substitution* nil)
    (dolist (instantiated-op solution)
      (case style
	;; assuming domain is loaded by sayphi (using say-domain)
	(pddl
	 (setq op (find (car instantiated-op) action-defs :key #'action-name))
	 (setq op-name (action-name op))
	 (setq params (action-parameters op))
	 (setq substitution (mapcar #'(lambda (param object) (cons param object))
				    params (cdr instantiated-op)))
	 ;; ((obj var type)*)
	 (mapc #'(lambda (object param)
		   (setq var (intern (format nil "?~:@(~a~)" object)))
		   (pushnew (cons var object) *macro-substitution* :key #'car)
		   (pushnew (cons object var) plan-substitution :key #'car)
		   (pushnew (list object var '- (cdr param)) params-substitution :key #'car))
	       (cdr instantiated-op) params)
	 (setq preconds (sublis substitution (action-preconditions op) :test #'equal))
	 (setq adds (sublis substitution (action-adds op) :test #'equal))
	 (setq dels (sublis substitution (action-dels op) :test #'equal)))
	;; assuming action-defs have been read directly from domain file
	(pddl-no-sayphi
	 (setq op (find (car instantiated-op) action-defs :key #'cadr))
	 (setq op-name (nth 1 op))
	 (setq params (nth 3 op))
	 (setq substitution (mapcar #'(lambda (param object) (cons (car param) object))
				    params (cdr instantiated-op)))
	 (setq preconds (sublis substitution (if (eq (car (nth 5 op)) 'and) (cdr (nth 5 op)) (nth 5 op))))
	 (setq adds (sublis substitution (remove-if #'(lambda (effect) (eq (car effect) 'not))
						    (if (eq (car (nth 7 op)) 'and)
							(cdr (nth 7 op))
							(list (nth 7 op))))))
	 (setq dels (sublis substitution (remove-if-not #'(lambda (effect) (eq (car effect) 'not))
							(if (eq (car (nth 7 op)) 'and)
							    (cdr (nth 7 op))
							    (list (nth 7 op)))))))
	(prodigy
	 (setq op-name (get-applicable-op instantiated-op))
	 (setq binding-node (p4::instantiated-op-binding-node-back-pointer instantiated-op))
	 (setq preconds (preconds-from-instantiated-op binding-node))
	 (setq applied-node (applied-node-from-instantiated-op binding-node instantiated-op))
	 (setq adds (adds-from-applied-node applied-node))
	 (setq dels (dels-from-applied-node applied-node))))
      (push (list op-name params preconds adds dels) plan))
    (if (eq style 'prodigy)
	(reverse plan)
	(list (reverse plan) plan-substitution params-substitution))))

(defun preconds-from-instantiated-op (binding-node)
  (get-binding-preconds (p4::binding-node-instantiated-preconds binding-node)))

(defun applied-node-from-instantiated-op (node instantiated-op)
  (if (and (p4::applied-op-node-p node)
	   (eq (p4::applied-op-node-instantiated-op node)
	       instantiated-op))
      node
      (let ((children (p4::nexus-children node)))
	(if children
	    (or (applied-node-from-instantiated-op (car children) instantiated-op)
		(applied-node-from-instantiated-op (cdr children) instantiated-op))))))

(defun adds-from-applied-node (node)
  (get-binding-preconds (cons 'and (p4::op-application-delta-adds (car (p4::applied-op-node-applied node))))))

(defun dels-from-applied-node (node)
  (get-binding-preconds (cons 'and (p4::op-application-delta-dels (car (p4::applied-op-node-applied node))))))


;;; **********************************************************************************************
;;;   Functions for creating a triangular table from a plan and creating a macroop from it
;;; **********************************************************************************************

(defun create-macro (plan)
  (let* ((num-ops (length plan))
	 (macro-array (make-array (list (1+ num-ops)
					(1+ num-ops))
				  :initial-element nil)))
    (do* ((i 0 (1+ i))
	  (solution plan (cdr solution))
	  (op (car solution)
	      (car solution))
	  (adds-from-previous (adds-from-previous macro-array i)
			      (adds-from-previous macro-array i))
	  (dels-from-previous (dels-from-previous macro-array i)
			      (dels-from-previous macro-array i)))
	((>= i num-ops) macro-array)
      (setf (aref macro-array (1+ i) i)
	    (my-action-name op))
      (setf (aref macro-array (1+ i) (1+ i))
	    (list (my-action-adds op) (my-action-dels op)))
      (setf (aref macro-array 0 i)
	    (set-difference (my-action-preconds op) adds-from-previous :test #'equal))
      (dotimes (j i)
	(setf (aref macro-array (1+ j) (1+ i))
	      (list (set-difference (car (aref macro-array (1+ j) i))
				    (my-action-dels op)
				    :test #'equal)
		    (set-difference (cadr (aref macro-array (1+ j) i))
				    (my-action-adds op)
				    :test #'equal)))))))

;; macro-array is a triangular table
;; params is a bindings list with vars and types info in the case of prodigy
;; style can be either prodigy or pddl
(defun generate-new-op (plan macro-array params domain-dir &optional (style 'pddl) (output-format 'list) (tracep nil))
  (let ((preconds nil)
	(adds-dels nil)
	(adds nil)
	(dels nil)
	(objs-in-macro nil)
	(ops nil)
	(ops-in-plan (1- (car (array-dimensions macro-array)))))
    (when tracep
      (format t "~2%Triangular table:")
      (print-table macro-array))
    (dotimes (i ops-in-plan)
      (pushnew (aref macro-array (1+ i) i) ops :test #'equal)
      (dolist (precond (aref macro-array 0 i))
	(dolist (obj (cdr precond))
	  (pushnew obj objs-in-macro))
	(pushnew precond preconds :test #'equal))
      (setq adds-dels (aref macro-array (1+ i) ops-in-plan))
      (dolist (add (car adds-dels))
	(unless (or (member add preconds :test #'equal)
		    (member add adds :test #'equal))
	  (dolist (obj (cdr add))
	    (pushnew obj objs-in-macro))
	  (push add adds)))
      (if *use-cost-in-macros-p*
	  (if (not (member 'increase adds :key #'car))
	      (push `(increase (total-cost) ,ops-in-plan) adds)))
      (dolist (del (cadr adds-dels))
	(unless (member del dels :test #'equal)
	  (dolist (obj (cdr del))
	    (pushnew obj objs-in-macro))
	  (push del dels))))
    (make-operator domain-dir
		   :style style
		   :params params
		   :plan-substitution (cadr plan)
		   :params-substitution (caddr plan)
		   :preconds preconds
		   :adds adds
		   :dels dels
		   :ops (reverse ops)
		   :objs-in-macro objs-in-macro
		   :output-format output-format
		   :tracep tracep)))

;; This does not work: removing dels that are not in the preconds. For instance, it does not work in the rovers
;; related to calibrating and taking images
;; (dolist (del (cadr adds-dels))
;; 	(unless (or (not (member del preconds :test #'equal))
;; 		    (member del dels :test #'equal))
;; 	  (dolist (obj (cdr del))
;; 	    (pushnew obj objs-in-macro))
;; 	  (push del dels)))
(defun make-operator (domain-dir &key plan-substitution params-substitution style
				   params preconds adds dels ops objs-in-macro
				   output-format tracep)
  (let (substitution)
    (case style
      (prodigy
       (setq substitution (mapcar #'(lambda (param)
				      (cons (car param) 
					    (intern (if (eq style 'prodigy)
							(format nil "~:@(<~a-~a>~)" (car param) (cdr param))
							(format nil "~:@(?~a-~a~)" (car param) (cdr param))))))
				  params))
       (setq preconds (sublis substitution preconds))
       (setq adds (sublis substitution adds))
       (setq dels (sublis substitution dels))
       (setq params (sublis substitution params)))
      (pddl
       (setq params (mapcan #'(lambda (obj-or-param)
				(if (listp obj-or-param)
				    obj-or-param))
			    (sublis params-substitution objs-in-macro)))
       (setq preconds (sublis plan-substitution preconds))
       (setq adds (sublis plan-substitution adds))
       (setq dels (sublis plan-substitution dels))))
    (case output-format
      (file
       (with-open-file (ostream (format nil "~(~amacros.lisp~)" domain-dir)
				:direction :output
				:if-exists :append
				:if-does-not-exist :create)
	 (cond ((eq style 'prodigy)
		(format ostream "~2%(OPERATOR macro-op-~(~a~)" (gensym))
		(format ostream "~%   (params")
		(dolist (var (mapcar #'cdr (reverse substitution)))
		  (format ostream " ~(~a~)" var))
		(format ostream ")~%   (preconds (")
		(dolist (param params)
		  (format ostream "~%              (~(~a~) ~(~a~))" (car param) (cdr param)))
		(format ostream ")~%      (and")
		(dolist (precond preconds)
		  (format ostream "~%        ~(~a~)" precond))
		(format ostream "))~%   (effects nil~%      (")
		(dolist (add adds)
		  (format ostream "~%       (add ~(~a~))" add))
		(dolist (del dels)
		  (format ostream "~%       (del ~(~a~))" del))
		(format ostream ")))")
		(if tracep
		    (format t "~%It comes from: ~(~a~)" ops)))
	       (t 
		(format ostream "~2%(:action macro-op-~(~a~)~% :params ~(~a~)  :preconds (and ~(~a~))"
			(gensym) params preconds)
		(format ostream "~%   :effects (and ")
		(dolist (add adds)
		  (format ostream "~%              ~(~a~)" add))
		(dolist (del dels)
		  (format ostream "~%              ~((not ~a)~)" del))
		(format ostream "))~%")
		(if tracep
		    (format t "~%It comes from: ~(~a~)" ops))))))
      (list `(:action ,(intern (format nil "~:@(macro-op-~a~)" (gensym)))
		      :params ,params
		      :preconds (and ,@preconds)
		      :effects (and ,@(mapcar #'(lambda (del) `(not ,del)) dels) ,@adds))))))

(defun my-action-name (op)
  (nth 0 op))

(defun my-action-preconds (op)
  (nth 2 op))

(defun my-action-adds (op)
  (nth 3 op))

(defun my-action-dels (op)
  (nth 4 op))

(defun adds-from-previous (macro-array i)
  (let ((adds nil))
    (dotimes (j i)
      (dolist (add (car (aref macro-array (1+ j) i)))
	(pushnew add adds :test #'equal)))
    adds))

(defun dels-from-previous (macro-array i)
  (let ((dels nil))
    (dotimes (j i)
      (dolist (del (cadr (aref macro-array (1+ j) i)))
	(pushnew del dels :test #'equal)))
    dels))

(defun get-params-from-plan (plan)
  (let ((bindings nil))
    (dolist (instantiated-op plan)
      (dolist (obj (cdar instantiated-op))
	(if (not (assoc obj bindings))
	    (push (cons obj (get-object-type obj))
		  bindings))))
    bindings))
      

(defun print-table (table &optional (salida t))
  (terpri salida)
  (let ((dimensions (car (array-dimensions table))))
    (dotimes (j dimensions)
      (let ((fila-real (- dimensions j)))
	(dotimes (i (if (zerop j)
			fila-real
			(1+ fila-real)))
	  (format salida "~a " (aref table i (- fila-real 1))))
	(terpri salida)))))
