;; Rete implementation for PDL-style control rules
;; Daniel Borrajo. Jul 2007

;;; Main interface functions:
;; (build-rete rules &key domain problem)
;; (add-tokens literals addp &optional rete-node)

(in-package "COMMON-LISP-USER")

(defvar *conflict-set* nil)
(defvar *rete-root-node* nil)
(defvar *trace-build-rete* nil)
(defvar *trace-exec-rete* nil)
(defvar *trace-conflict-set* nil)
;; whether to print rete net nodes shortly or not
(defvar *condensed-format-p* t)

(defstruct (rete-node (:print-function pp-rete-node))
  name          ;;for printing and debugging
  type          ;; root, alpha, beta, end
  test		;;The test for any node - slot-access fn, variable binding
  left-mem	;;Stores WMe's that arrive from the left
  right-mem	;;Stores WMe's that arrive from the right
;;  output-mem	;;Stores WME's that matched and go to its output nodes.
  left-input	;;The left input is either an alpha node or a beta node
  right-input	;;The right input is always an alpha node, or NIL if this node is itself an alpha node.
  output	;;Pointer to list of nodes that take input from it, iff alpha
;;  right-output	;; Pointer to list of nodes that take input, goes to beta
  effects	;;What to do if node is reached.
;;  slots       ;;For incremental adding new things
  )

(defstruct (token (:print-function pp-token))
  type          ;; add or del
  contents      ;; the actual metapredicate literal
  children      ;; a list of pointers to the tokens created from this one. It makes deleting tokens easier
  rete-nodes    ;; a list of rete nodes that have it in their memories
  )

;; Requires a domain and problem to be loaded in Sayphi
;; If not, domain and problem are needed
(defun build-rete (rules &key domain problem)
    (setf *conflict-set* nil)
    (setf *rete-root-node* (make-rete-node :name "Root node" :type 'root))
    (dolist (rule rules)
      (push-rule-rete rule *rete-root-node* (compute-predicates-frequency :domain domain :problem problem))))

(defun compute-predicates-frequency (&key domain problem)
  (let ((predicates-frequency nil)
	(state (if (boundp '*current-problem*)
		   (problem-lit-init-state *current-problem*))))
    (if (boundp '*pspace*)
	(maphash #'(lambda (pred args)
		     (declare (ignore args))
		     (push (cons pred 0) predicates-frequency))
		 (dom-predicates *pspace*)))
    (if (not predicates-frequency)
	(with-open-file (istream domain :direction :input)
	  (setq predicates-frequency (mapcar #'(lambda (predicate)
						 (cons (car predicate) 0))
					     (find-argument (cdr (read istream)) :predicates)))))
    (if (and (not state) problem)
	(with-open-file (istream problem :direction :input)
	  (setq state (cdr (assoc :init (cdr (read istream)))))))
    (dolist (literal state)
      (incf (cdr (assoc (car literal) predicates-frequency))))
    predicates-frequency))
  
(defun push-rule-rete (rule rete-node predicates-frequency)
  (let ((preconds (cdadr (nth 2 rule)))
	(effects (nth 3 rule)))
    ;; I order according to the frequency of predicate names in problem given as training
    (add-preconds-to-rete (sort-preconds-by-frequency preconds predicates-frequency)
			  rete-node effects (nth 1 rule))))

(defun sort-preconds-by-frequency (preconds predicates-frequency)
  (sort preconds #'(lambda (x y)
		     (case (car x)
		       ((target-goal applicable-op current-goal current-operator candidate-operator) t)
		       ((type-of-object some-candidate-goals) nil)
		       (true-in-state (case (car y)
					((target-goal applicable-op current-goal current-operator candidate-operator) nil)
					((type-of-object some-candidate-goals) t)
					(true-in-state (< (cdr (assoc (caadr x) predicates-frequency))
							  (cdr (assoc (caadr y) predicates-frequency))))
					(otherwise t)))
		       (otherwise nil)))))

(defun add-preconds-to-rete (preconds rete-node effects name)
  (when preconds
    (if (cdr preconds)
	(do* ((alpha-node-binding (find-or-create-alpha-node (car preconds) rete-node))
	      (alpha-node-binding-1 (find-or-create-alpha-node (cadr preconds) rete-node))
	      (beta-node (create-beta-rete-node (cadr preconds) (car alpha-node-binding) (car alpha-node-binding-1)
						effects name (append (cdr alpha-node-binding) (cdr alpha-node-binding-1))))
	      (conds (cddr preconds) (cdr conds)))
	    ((null conds) (create-end-rete-node beta-node effects name))
	  (setq alpha-node-binding (find-or-create-alpha-node (car conds) rete-node))
	  (setq beta-node (create-beta-rete-node (car conds) beta-node (car alpha-node-binding) effects name (cdr alpha-node-binding))))
	(create-end-rete-node (find-or-create-alpha-node (car conds) rete-node)
			      effects name))))

(defun find-or-create-alpha-node (precond rete-node)
  (or (find-rete-node (make-token :type 'add :contents precond) rete-node)
      (create-alpha-rete-node precond rete-node)))

(defun find-rete-node (precond rete-node)
  (some #'(lambda (rete-child)
	    (let ((unification (alpha-test rete-child precond nil)))
	      (if (not (eq unification 'fail))
		  (cons rete-child unification))))
	   (rete-node-output rete-node)))

(defun create-alpha-rete-node (precond rete-node)
  (let ((child (make-rete-node :name (format nil "~a" precond)
			       :type 'alpha
			       :test (create-alpha-test precond)
			       :right-input rete-node)))
    (setf (rete-node-output rete-node)
	  (nconc (rete-node-output rete-node)
		 (list child)))
    (cons child nil)))

(defun create-alpha-test (precond)
  (let ((new-fn `(lambda (token binding)
		   (let ((unification (unify-any-depth (token-contents token) (sublis binding ',precond))))
		     ,(if *trace-exec-rete* `(format t "~%Unifying: ~a~% with: ~a~% and resulting: ~a"
						     (token-contents token) (sublis binding ',precond) unification))
		     (if (eq unification 'fail)
			 'fail
			 (nconc unification binding))))))
    (if *trace-build-rete* (format t "~%Creating new alpha-node test fn:~% ~a" new-fn))
    (compile nil (eval `(function ,new-fn)))))

(defmacro alpha-test (alpha-node token binding)
  `(funcall (rete-node-test ,alpha-node) ,token ,binding))

;;       (let ((contents (cadr token)))
;; 	(and (eq (car contents) (car precond))
;; 	     (case (car precond)
;; 		 ((target-goal current-goal true-in-state) (eq (caadr contents) (caadr precond)))
;; 	       ((candidate-operator current-operator) (eq (cadr contents) (cadr precond)))
;; 	       (some-candidate-goals nil)
;; 	       (otherwise nil))))
  
;; ;; I would not make it work dur to problems with bindings
;; (defun create-beta-rete-nodes (preconds rete-node alpha-nodes effects name)
;;   (if (cdr preconds)
;;       (let ((beta-node (find-beta-node preconds alpha-nodes)))
;; 	(if beta-node
;; 	    (create-beta-rete-nodes (cons (if (listp (caar beta-node))
;; 					      (append (car beta-node) (list (cadr beta-node)))
;; 					      (list (car beta-node) (cadr beta-node)))
;; 					  (remove-if #'(lambda (precond)
;; 							 (or (equal precond (car beta-node))
;; 							     (equal precond (cadr beta-node))))
;; 						     preconds))
;; 				    (nth 2 beta-node)
;; 				    (remove-if #'(lambda (alpha-node)
;; 						   (or (equal alpha-node (nth 3 beta-node))
;; 						       (equal alpha-node (nth 4 beta-node))))
;; 					       alpha-nodes
;; 					       :count 1)
;; 				    effects name)
;; 	    (create-beta-rete-node preconds (car alpha-nodes) (cdr alpha-nodes) effects name)))
;;       (create-end-rete-node rete-node preconds effects name)))

;; (defun find-beta-node (preconds alpha-nodes)
;;   (if preconds
;;       (or (find-existing-beta-node (car preconds) (cdr preconds) (car alpha-nodes) (cdr alpha-nodes))
;; 	  (find-beta-node (cdr preconds) (cdr alpha-nodes)))))
;; 
;; (defun find-existing-beta-node (precond preconds alpha-node alpha-nodes)
;;   (if preconds
;;       (or (some #'(lambda (beta-node)
;; 		    (let ((beta-node-exists-p (funcall (rete-node-test beta-node)
;; 						       (list 'add (if (listp (car precond))
;; 								      precond
;; 								      (list precond)))
;; 						       (list 'add (car preconds)) nil)))
;; ;;		      (format t "~%Checking existence of beta-node with: ~a and ~a. Result: ~a" precond (car preconds) beta-node-exists-p)
;; 		      (if beta-node-exists-p
;; 			  (list precond (car preconds) beta-node alpha-node (car alpha-nodes)))))
;; 		(intersection (rete-node-output alpha-node)
;; 			      (rete-node-output (car alpha-nodes))))
;; 	  (find-existing-beta-node precond (cdr preconds) alpha-node (cdr alpha-nodes)))))
;; 		    (let ((beta-node-exists-p (funcall (rete-node-test beta-node) (list 'add precond)
;; 						       (list 'add (car preconds)) nil)))


(defun create-beta-rete-node (next-cond rete-node alpha-node effects name binding)
  (declare (ignore effects name))
  (let* ((right-node-name (read-from-string (rete-node-name rete-node)))
	 (left-node-name (read-from-string (rete-node-name alpha-node)))
	 (child (make-rete-node :name (format nil "~a" (if (eq (rete-node-type rete-node) 'beta)
							   (append right-node-name (sublis binding (list left-node-name)))
							   (list right-node-name (sublis binding left-node-name))))
				:type 'beta
				:test (create-beta-test (if (listp (car right-node-name))
							    right-node-name
							    (list right-node-name))
							(sublis binding left-node-name))
				:right-input rete-node
				:left-input alpha-node)))
    (setf (rete-node-output rete-node)
	  (nconc (rete-node-output rete-node)
		 (list child)))
    (setf (rete-node-output alpha-node)
	  (nconc (rete-node-output alpha-node)
		 (list child)))
    child))


(defun create-beta-test (right-input left-input)
  (let ((new-fn `(lambda (right-token left-token binding)
		   (let ((unification (unify-any-depth (cons (token-contents left-token)
							     (token-contents right-token))
						       (sublis binding ',(cons left-input right-input)))))
		     ,(if *trace-exec-rete* `(format t "~%Unifying: ~a~% with: ~a~% and resulting: ~a"
						     (cons (token-contents left-token) (token-contents right-token))
						     (sublis binding ',(cons left-input right-input))
						     unification))
		     (if (eq unification 'fail)
			 'fail
			 (nconc unification binding))))))
    (if *trace-build-rete* (format t "~%Creating new beta-node test fn:~% ~a" new-fn))
    (compile nil (eval `(function ,new-fn)))))
;; 	       (let ((unification (unify-any-depth (cons (cadr left-token) (list (caadr right-token)))

(defmacro beta-test (beta-node right-token left-token binding)
  `(funcall (rete-node-test ,beta-node) ,right-token ,left-token ,binding))

(defun create-end-rete-node (rete-node effects name)
  (let ((child (make-rete-node :name (format nil "~a" name)
			       :type 'end
			       :effects (create-effects-rete-node effects)
			       :right-input rete-node)))
    (setf (rete-node-output rete-node)
	  (nconc (rete-node-output rete-node)
		 (list child)))
    child))

(defun create-effects-rete-node (effects)
  effects)

(defun pp-rete-node (node stream depth)
  (declare (ignore depth))
  (format stream "~%(Node:")
  (format stream "~% ~a: " (rete-node-type node))
  (format stream "~a" (rete-node-name node))
  (format stream "~% test: ~a" (rete-node-test node))
  (format stream "~% left-mem: ~a" (rete-node-left-mem node))
  (format stream "~% right-mem: ~a" (rete-node-right-mem node))
  ;;  (format stream "~% output-mem: ~a" (rete-node-output-mem node))
  (if *condensed-format-p*
      (format stream "~% left-input: ~a" (if (rete-node-left-input node)
					     (rete-node-name (rete-node-left-input node))))
      (format stream "~% left-input: ~a" (rete-node-left-input node)))
  (if *condensed-format-p*
      (format stream "~% right-input: ~a" (if (rete-node-right-input node)
					      (rete-node-name (rete-node-right-input node))))
      (format stream "~% right-input: ~a" (rete-node-right-input node)))
  (if *condensed-format-p*
      (dolist (child (rete-node-output node))
	  (format stream "~% output: ~a" (rete-node-name child)))
      (format stream "~% output: ~a" (rete-node-output node)))
  ;;  (format stream "~% right-output: ~a" (rete-node-right-output node))
  (format stream "~% effects: ~a)" (rete-node-effects node)))
;;  (format stream "~% slots: ~a)" (rete-node-slots node)))

(defun pp-token (token stream depth)
  (declare (ignore depth))
  (cond (*condensed-format-p*
	 (format stream "(T ~a ~a ~%" (token-type token) (token-contents token))
	 (dolist (child (token-children token))
	   (format stream ".")))
	(t (format stream "~%(Token:")
	   (format stream "~% type: ~a" (token-type token))
	   (format stream "~% contents: ~a" (token-contents token))
	   (format stream "~% children: ~a" (token-children token))
	   (format stream "~% rete nodes: ~a" (token-rete-nodes token))))
  (format stream ")"))

;;; ************************************************************************************
;;;                       Running the Rete Net
;;; ************************************************************************************

;; It can be used for adding all kinds of changes to the meta-state: literals, goals, etc.
(defun add-tokens (literals addp &optional (rete-node *rete-root-node*))
  (dolist (literal literals)
      (let ((argument (cadr literal)))
	(if (member (car literal) (list 'true-in-state 'current-goal 'target-goal))
	    (if (eq (car argument) 'not)
		(add-token (make-token :type (if addp 'del 'add) :contents (cons (car literal) (cadr argument))) rete-node nil)
		(add-token (make-token :type (if addp 'add 'del) :contents literal) rete-node nil))
	    (add-token (make-token :type (if addp 'add 'del) :contents literal) rete-node nil)))))

(defun add-token (token rete-node binding)
  (let ((new-binding nil))
    (do* ((children (rete-node-output rete-node) (cdr children))
	  (child (car children) (car children))
	  (new-token nil)
	  (found-token nil))
	((null children))
      (cond ((eq (rete-node-type child) 'alpha)
	     (setq found-token (find-if #'(lambda (token-in-mem)
					    (equal (token-contents token-in-mem)
						   (token-contents token)))
					(rete-node-left-mem child)))
	     (cond ((eq (token-type token) 'add)
		    (unless found-token
		      (setq new-binding (alpha-test child token binding))
		      (if *trace-exec-rete*
			  (format t "~2%Alpha node: ~a~%Token: ~a~%Result: ~a" (rete-node-name child) token new-binding))
		      (unless (eq new-binding 'fail)
			(add-token-to-mem token t child)
			(add-token token child new-binding))))
		   (found-token
		    (setf (token-type found-token) 'del)
		    (remove-token-from-rete found-token))
		   (t nil)))
	    (t ;; this is to handle the special case of alpha nodes whose both outputs go to the same beta node
	     (cond ((and (eq (rete-node-right-input child) rete-node)
			 (not (member child (cdr children))))
		    (setq new-token (make-token :type (token-type token) :contents (list (token-contents token))))
		    (push new-token (token-children token))
		    (propagate-token new-token child rete-node binding nil))
		   (t (propagate-token token child rete-node binding t))))))))

(defun remove-token-from-rete (token)
  (dolist (rete-node (token-rete-nodes token))
    (remove-token-from-mem token (cdr rete-node) (car rete-node))
    (if (eq (rete-node-type (car rete-node)) 'end)
	(modify-conflict-set token (rete-node-effects (car rete-node)) nil (car rete-node))))
  (dolist (child-token (token-children token))
    (setf (token-type child-token) 'del)
    (remove-token-from-rete child-token)))

;; For beta-rete-nodes
(defun propagate-token (token node parent binding leftp)
  (declare (ignore parent))
  (if *trace-exec-rete* (format t "~2%Propagating token: ~a~% to beta node: ~a~%" token node))
  (cond ((eq (rete-node-type node) 'beta)
	 (dolist (token-in-mem (tokens-in-mem node (not leftp)))
	   (let* ((right-token (if leftp token-in-mem token))
		  (left-token (if leftp token token-in-mem))
		  (new-token nil)
		  (new-binding (beta-test node right-token left-token nil)))
	     (if *trace-exec-rete* (format t "~%Checking left token: ~a~% with right token: ~a~% and binding: ~a~%resulting: ~a"
					   left-token right-token binding new-binding))
	     (unless (eq new-binding 'fail)
	       (setq new-token (make-token :type 'add :contents (append (token-contents right-token)
									(list (token-contents left-token)))))
	       (push new-token (token-children token))
	       (push new-token (token-children token-in-mem))
	       (dolist (child (rete-node-output node))
		 (propagate-token new-token child node new-binding (eq (rete-node-left-input child) node))))))
	 (add-token-to-mem token leftp node))
	(t (modify-conflict-set token (rete-node-effects node) binding node))))
;; this test does not work when the same alpha-node goes to the two inputs of a beta node
;; (eq (rete-node-left-input node) parent)


;; (defun propagate-token (token node parent binding leftp)
;;   (if *trace-exec-rete* (format t "~2%Propagating token: ~a~% to beta node: ~a~%" token node))
;;   (if (eq (rete-node-type node) 'beta)
;;       (if (find-in-mem token leftp node)
;; 	  (cond ((eq (token-type token) 'del)
;; 		 (remove-token-from-mem token leftp node)
;; 		 (dolist (child (rete-node-output node))
;; 		   (propagate-token token child node binding (eq (rete-node-left-input child) node))))
;; 		(t nil))
;; 	  (cond ((eq (token-type token) 'add)
;; 		 (dolist (token-in-mem (tokens-in-mem node (not leftp)))
;; 		   (let ((right-token (if leftp token-in-mem token))
;; 			 (left-token (if leftp token token-in-mem))
;; 			 (new-token nil)
;; 			 (new-binding (beta-test node right-token left-token nil)))
;; 		     (if *trace-exec-rete* (format t "~%Checking left token: ~a~% with right token: ~a~% and binding: ~a~%resulting: ~a"
;; 						   left-token right-token binding new-binding))
;; 		     (unless (eq new-binding 'fail)
;; 		       (setq new-token (make-token :type 'add :contents (append (token-contents right-token)
;; 										(list (token-contents left-token)))))
;; 		       (push new-token (token-children token))
;; 		       (push new-token (token-children token-in-mem))
;; 		       (pushnew (cons node leftp) (token-rete-nodes token) :test #'equal)
;; 		       (dolist (child (rete-node-output node))
;; 			 (propagate-token new-token child node new-binding (eq (rete-node-left-input child) node))))))
;; 		 (add-token-to-mem token leftp node))
;; 		(t nil)))
;;       (modify-conflict-set (token-type token) (rete-node-effects node) binding)))

;; (defun propagate-token (token node parent binding leftp)
;;   (if *trace-exec-rete* (format t "~2%Propagating token: ~a~% to beta node: ~a~%" token node))
;;   (if (eq (rete-node-type node) 'beta)
;;       (let ((new-binding nil))
;; 	(if (find-in-mem token leftp node)
;; 	    (cond ((eq (car token) 'del)
;; 		   (remove-token-from-mem token leftp node)
;; 		   (dolist (child (rete-node-output node))
;; 		     (propagate-token token child node binding (eq (rete-node-left-input child) node))))
;; 		  (t nil))
;; 	    (cond ((eq (car token) 'add)
;; 		   (dolist (token-in-mem (tokens-in-mem node (not leftp)))
;; 		     (let ((right-token (if leftp token-in-mem token))
;; 			   (left-token (if leftp token token-in-mem))
;; 			   (new-token nil))
;; 		       (setq new-binding (beta-test node right-token left-token binding))
;; 		       (if *trace-exec-rete* (format t "~%Checking left token: ~a~% with right token: ~a~% and binding: ~a~%resulting: ~a"
;; 						     left-token right-token binding new-binding))
;; 		       (unless (eq new-binding 'fail)
;; 			 (setq new-token (list 'add (append (cadr right-token) (list (cadr left-token)))))
;; 			 (dolist (child (rete-node-output node))
;; 			   (propagate-token new-token child node new-binding (eq (rete-node-left-input child) node))))))
;; 		   (add-token-to-mem token leftp node))
;; 		  (t nil))))
;;       (modify-conflict-set (car token) (rete-node-effects node) binding)))

;;   (list 'add (cons (cadr token-in-mem) (cadr token)))

;; I will implement this better as hash tables
(defun find-in-mem (token leftp node)
  (member token
	  (if leftp
	      (rete-node-left-mem node)
	      (rete-node-right-mem node))))

;; (defun find-in-mem (token leftp node)
;;   (member (token-contents token)
;; 	  (if leftp
;; 	      (rete-node-left-mem node)
;; 	      (rete-node-right-mem node))
;; 	  :test #'equal))

(defun remove-token-from-mem (token leftp node)
  (if leftp
      (setf (rete-node-left-mem node)
	    (remove token (rete-node-left-mem node)))
      (setf (rete-node-right-mem node)
	    (remove token (rete-node-right-mem node))))
  (if *trace-exec-rete*
      (format t "~2%Removing token: ~a~%from ~a of beta node: ~a~%mem after: ~a"
	      token (if leftp 'left 'right) (rete-node-name node)
	      (if leftp (rete-node-left-mem node) (rete-node-right-mem node))))
  node)

(defun tokens-in-mem (node leftp)
  (if leftp
      (rete-node-left-mem node)
      (rete-node-right-mem node)))

;;   (mapcar #'(lambda (token)
;; 	      (list 'add token))
;; 	  (if leftp
;; 	      (rete-node-left-mem node)
;; 	      (rete-node-right-mem node)))

(defun add-token-to-mem (token leftp node)
  (if leftp
      (pushnew token (rete-node-left-mem node))
      (pushnew token (rete-node-right-mem node)))
  (pushnew (cons node leftp)
	   (token-rete-nodes (if (eq (rete-node-type node) 'end) (car token) token))
	   :test #'equal)
  (if *trace-exec-rete*
      (format t "~2%Adding token: ~a~%from ~a of beta node: ~a~%mem after: ~a"
	      token (if leftp 'left 'right) (rete-node-name node)
	      (if leftp (rete-node-left-mem node) (rete-node-right-mem node)))))

;; (defun add-token-to-mem (token leftp node)
;;   (if leftp
;;       (dolist (real-token (cdr token))
;; 	(push real-token
;; 	      (rete-node-left-mem node)))
;;       (dolist (real-token (cdr token))
;; 	(push real-token
;; 	      (rete-node-right-mem node))))
;;   (if *trace-exec-rete*
;;       (format t "~2%Adding token: ~a~%from ~a of beta node: ~a~%mem after: ~a"
;; 	      token (if leftp 'left 'right) (rete-node-name node)
;; 	      (if leftp (rete-node-left-mem node) (rete-node-right-mem node)))))

;; this is highly inefficient, but will serve for now. I should implement a hash table in the output mems of rete nodes and go directly
(defun modify-conflict-set (token effects binding rete-node)
  (declare (special *conflict-set*))
  (let ((new-effects nil))
    (cond ((eq (token-type token) 'add)
	   (setq new-effects (sublis binding effects))
	   (cond ((unbound-vars-p new-effects)
		  (if *trace-conflict-set* (format t "~2%Not adding to conflict set ~a because unbound vars" new-effects)))
		 ((member new-effects *conflict-set* :test #'equal)
		  (if *trace-conflict-set*
		      (format t "~2%Repeated entry in conflict set ~a~%results in: ~a" new-effects *conflict-set*)))
		 (t (push new-effects *conflict-set*)
		    (if *trace-conflict-set*
			(format t "~2%Adding to conflict set ~a~%results in: ~a" new-effects *conflict-set*))
		    (add-token-to-mem (cons token new-effects) t rete-node))))
	  (t (setq new-effects (cdr (assoc token (rete-node-left-mem rete-node))))
	     (when new-effects
	       (setf *conflict-set* (remove new-effects *conflict-set* :test #'unify-any-depth-p))
	       (setf (rete-node-left-mem rete-node)
		     (remove-if #'(lambda (token-effects)
				    (equal (token-contents (car token-effects))
					   (token-contents token)))
				(rete-node-left-mem rete-node)))
	       (if *trace-conflict-set* (format t "~2%Deleting from conflict set ~a~%results in: ~a" new-effects *conflict-set*)))))
    *conflict-set*))

;; Change pdl for pddl when using pddl control rules
(defun unbound-vars-p (list)
  (if list
      (if (listp (car list))
	  (or (unbound-vars-p (car list))
	      (unbound-vars-p (cdr list)))
	  (or (variablep (car list) 'pdl)
	      (unbound-vars-p (cdr list))))))

(defun clear-rete (&optional (rete-node *rete-root-node*))
  (setf *conflict-set* nil)
  (when rete-node
      (setf (rete-node-left-mem rete-node) nil)
    (setf (rete-node-right-mem rete-node) nil)
    (dolist (child (rete-node-output rete-node))
	(clear-rete child))))
