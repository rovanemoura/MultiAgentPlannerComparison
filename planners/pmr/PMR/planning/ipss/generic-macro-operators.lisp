(in-package "COMMON-LISP-USER")
;; for the P4 calls
(unless (find-package "PRODIGY4") (make-package "PRODIGY4" :nicknames '("P4") :use '("COMMON-LISP-USER")))

;; Example of use for IPSS:
;; 0. (setf *planner-for-learning* 'ipss)
;; 1. (domain 'blocksworld)
;; 2. (problem 'suss)
;; 3. (run)
;; 4. (learn-macro 'blocksworld)

;; Example of use for Sayphi:
;; 0. (setf *planner-for-learning* 'sayphi)
;; 1. (say-domain "blocksworld")
;; 2. (prob "suss")
;; 3. (plan)
;; 4. (learn-macro 'blocksworld)

;;  It will generate a file macros.lisp in the blocksworld directory, that
;; you can insert into the domain file (before the first operator definition) if parameter install-macro-p is t

;; Solution is either a list of operators lists (as coming from the execution of Metric-FF, or a list of sayphi search nodes)
;; In case of IPSS or Sayphi, it can access it directly. Otherwise, you will have to provide as a list of instantiated actions (lists of action name and parameters)
(defun learn-macro (domain &key install-macro-p printp
			   (solution (case *planner-for-learning*
				       (ipss (prodigy-result-solution *prodigy-result*))
				       (sayphi (solution-path *say-solution*))
				       (otherwise nil)))
			   (first-step-number 1)
			   (last-step-number (length solution)))
  (let ((plan (visible-plan (subseq solution (1- first-step-number) last-step-number)))
	(macros nil)
	(domain-directory (case *planner-for-learning*
			    (ipss (if *standard-paths-p*
				      (format nil "~(~a~a/~)" *hamlet-world-path* domain)
				      *hamlet-world-path*))
			    (otherwise (format nil "~(~a~a/~)" *domains-dir* domain)))))
    (setf *macros-range* 1)
    (generate-new-op (create-macro plan) (get-params-from-plan plan) domain-directory (if (eq *planner-for-learning* 'ipss) 'prodigy 'pddl) printp)
    ;; DANIEL: modify this for PDDL
    (when install-macro-p
      (execute-shell-command (format nil "cp -p ~adomain.lisp ~adomain-without-macros.lisp" domain-directory domain-directory))
;;       (eval `(shell ,(format nil "cp -p ~adomain.lisp ~adomain-without-macros.lisp" domain-directory domain-directory)))
      (with-open-file (istream (format nil "~amacros.lisp" domain-directory)
			       :direction :input)
	(do ((macro (read istream nil 'eof) (read istream nil 'eof))
	     (macros nil))
	    ((eq macro 'eof)
	     (with-open-file (istream2 (format nil "~adomain-without-macros.lisp" domain-directory)
				       :direction :input)
	       (with-open-file (ostream (format nil "~adomain.lisp" domain-directory)
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
    (setf *valid-macros* 1)
    ))
		     

;;; *********************************************************************************
;;;               Functions for computing the visible form of a plan
;;; *********************************************************************************

(defun visible-plan (solution)
  (case *planner-for-learning*
    (ipss (mapcar #'(lambda (instantiated-op)
		      (let* ((binding-node (p4::instantiated-op-binding-node-back-pointer instantiated-op))
			     (preconds (preconds-from-instantiated-op binding-node))
			     (applied-node (applied-node-from-instantiated-op binding-node instantiated-op))
			     (adds (adds-from-applied-node applied-node))
			     (dels (dels-from-applied-node applied-node)))
			(list (get-applicable-op instantiated-op) preconds adds dels)))
		  solution))
    (sayphi (mapcar #'(lambda (plan-step)
			(let* ((action  (if (snode-p plan-step)
					    (snode-plan-action plan-step)
					    plan-step))
			       (action-name (car action))
			       (action-struct (find action-name (dom-actions *pspace*) :key #'action-name))
			       (substitution (mapcar #'cons (action-parameters action-struct) (cdr action)))
			       (preconds (sublis substitution (action-preconditions action-struct) :test #'equal))
			       (adds (sublis substitution (action-adds action-struct) :test #'equal))
			       (dels (sublis substitution (action-dels action-struct) :test #'equal)))
			  (list action preconds adds dels)))
		    solution))
    (otherwise (mapcar #'(lambda (plan-step) (list plan-step)) solution))))

;; These functions only needed for IPSS
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
	    (plan-action-name op))
      (setf (aref macro-array (1+ i) (1+ i))
	    (list (plan-action-adds op) (plan-action-dels op)))
      (setf (aref macro-array 0 i)
	    (set-difference (plan-action-preconds op)
			    adds-from-previous
			    :test #'equal))
      (dotimes (j i)
	(setf (aref macro-array (1+ j) (1+ i))
	      (list (set-difference (car (aref macro-array (1+ j) i))
				    (plan-action-dels op)
				    :test #'equal)
		    (set-difference (cadr (aref macro-array (1+ j) i))
				    (plan-action-adds op)
				    :test #'equal)))))))

;; macro-array is a triangular table
;; params-info is a bindings list with vars and types info
;; style can be either prodigy or pddl
(defun generate-new-op (macro-array params domain-directory &optional (style 'prodigy) (printp nil))
  (let ((preconds nil)
	(adds-dels nil)
	(adds nil)
	(dels nil)
	(ops nil)
	(ops-in-plan (1- (car (array-dimensions macro-array)))))
    (when printp
      (format t "~2%Triangular table:")
      (print-table macro-array))
    (dotimes (i ops-in-plan)
      (pushnew (aref macro-array (1+ i) i) ops :test #'equal)
      (dolist (precond (aref macro-array 0 i))
	(pushnew precond preconds :test #'equal))
      (setq adds-dels (aref macro-array (1+ i) ops-in-plan))
      (dolist (add (car adds-dels))
	  (pushnew add adds :test #'equal))
      (dolist (del (cadr adds-dels))
	  (pushnew del dels :test #'equal)))
    (make-operator domain-directory
		   :style style
		   :params params
		   :preconds preconds
		   :adds (set-difference adds preconds :test #'equal)
		   :dels (remove-if-not #'(lambda (del) (member del preconds :test #'equal)) dels)
		   :ops (reverse ops)
		   :printp printp)))

(defun make-operator (domain-directory &key style params preconds adds dels ops printp)
  (let ((substitution (mapcar #'(lambda (param)
				  (cons (car param) 
					(intern (if (eq style 'prodigy)
						    (format nil "~:@(<~a-~a>~)" (car param) (cdr param))
						    (format nil "~:@(?~a-~a~)" (car param) (cdr param))))))
			      params)))
    (setq params (sublis substitution params))
    (setq preconds (sublis substitution preconds))
    (setq adds (sublis substitution adds))
    (setq dels (sublis substitution dels))
    (with-open-file (ostream (format nil "~(~amacros.lisp~)" domain-directory)
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
	     (if printp
		 (format t "~%It comes from: ~(~a~)" ops)))
	    (t (pp-macro-op params preconds adds dels ostream)
	       (if printp
		   (pp-macro-op params preconds adds dels t))
	       (if printp
		   (format t "~%It comes from: ~(~a~)" ops)))))))

(defun pp-macro-op (params preconds adds dels stream)
  (format stream "~2%(:action macro-op-~(~a~)~%   :parameters (" (gensym))
  (dolist (param params)
    (format stream "~(~a - ~a~) " (car param) (cdr param)))
  (format stream ")~%   :preconditions (and")
  (dolist (precond preconds)
    (format stream "~%                    ~(~a~)" precond))
  (format stream ")~%   :effects (and ")
  (dolist (add adds)
    (format stream "~%              ~(~a~)" add))
  (dolist (del dels)
    (format stream "~%              ~((not ~a)~)" del))
  (format stream "))~%"))

(defun plan-action-name (op)
  (car op))

(defun plan-action-preconds (op)
  (cadr op))

(defun plan-action-adds (op)
  (caddr op))

(defun plan-action-dels (op)
  (cadddr op))

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
