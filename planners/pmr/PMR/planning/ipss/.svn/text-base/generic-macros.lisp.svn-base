;; Generic macros used for both IPSS and Sayphi learning
(in-package "COMMON-LISP-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; ******************************************************************
;;;   Macros of induction.lisp
;;; ******************************************************************

  ;; Creates a new binding list with new vars
  (defmacro create-new-binding (binding)
    ` (mapcar #'(lambda (bind-pair)
		  (cons (cdr bind-pair)
			(new-var-from-new-var (cdr bind-pair))))
	      ,binding))

  (defmacro from-var-to-cte (var)
    `(string-trim "<>" (format nil "~a" ,var)))
  
  (defmacro get-all-operators nil
    `(if (eq *planner-for-learning* 'ipss)
	 (let ((all-the-ops nil))
	   (dolist (the-op (p4::problem-space-operators *current-problem-space*))
	     (push (p4::operator-name the-op) all-the-ops))
	   (dolist (the-op (p4::problem-space-lazy-inference-rules
			    *current-problem-space*)
		    (cons '*finish* (nreverse all-the-ops)))
	     (push (p4::operator-name the-op) all-the-ops)))
	 (mapcar #'action-name (dom-actions *pspace*))))
       
  (defmacro get-all-domain-operators nil
    `(remove-if #'(lambda (op)
		    (member op (list '*finish* 'prodigy4::*finish*)))
		(get-all-operators)))

  (defmacro get-all-goals nil
    `(if (eq *planner-for-learning* 'ipss)
	 (cons 'done (p4::problem-space-all-preds *current-problem-space*))
	 (let ((list nil))
	   (maphash #'(lambda (pred args) (push pred list))
		    (dom-predicates *pspace*))
	   list)))

  (defmacro generic-predicates-info nil
    `(if (eq *planner-for-learning* 'ipss)
	 (getf (p4::problem-space-plist *current-problem-space*) 'predicates)
	 (let ((list nil))
	   (maphash #'(lambda (pred args) (push (cons pred (mapcar #'(lambda (arg) (list (car arg) (cdr arg))) args)) list))
		    (dom-predicates *pspace*))
	   list)))

  (defmacro my-union (goals1 goals2)
    `(cond ((and ,goals1 ,goals2)
	    (union ,goals1 ,goals2 :test #'equal))
	   ((and ,goals1 (null ,goals2))
	    (my-push ,goals1))
	   ((and ,goals2 (null ,goals1))
	    (my-push ,goals2))
	   (t nil)))

  (defmacro my-push (list)
    `(if (member nil ,list :test #'eq)
	 ,list
         (cons nil ,list)))

  (defmacro my-mapcar (function list)
    `(let ((result nil))
       (dolist (element ,list (nreverse result))
	 (push (funcall ,function element)
	       result))))

  ;; Returns all rules of a decision node
  (defmacro get-decision-node-rules (node)
    `(decision-node-rules ,node))

  ;; It returns the node in which a rule should be classified in the
  ;; decision tree. It is useful for finding out the node without
  ;; storing it
  (defmacro find-decision-tree-node (rule)
    `(classify-rule ,rule (get-real-preconds ,rule) (get-real-effects ,rule) *decision-tree* nil nil))

  ;;********************************************************************
  ;; Macros of hamlet.lisp
  ;;********************************************************************

  ;; Returns the list of all control rules
  (defmacro all-control-rules (&optional (rules-types `(if (eq *planner-for-learning* 'ipss) *learned-rules-types* nil)))
    `(if (eq *planner-for-learning* 'ipss)
	 (let ((result nil))
	   (dolist (access-function ,rules-types result)
	     (setq result (append (funcall access-function *current-problem-space*)
				  result))))
	 *sayphi-rules*))

  (defmacro generic-control-rule-name (cr)
    `(if (and (eq *planner-for-learning* 'ipss) (p4::control-rule-p ,cr))
	 (p4::control-rule-name ,cr)
	 (cadr ,cr)))
       
  (defmacro get-real-preconds (cr)
    `(let ((true-preconds (if (and (eq *planner-for-learning* 'ipss) (p4::control-rule-p ,cr))
			      (p4::control-rule-if ,cr)
			      (cdadr (caddr ,cr)))))
       (if (eq (car true-preconds) 'and)
	   (cdr true-preconds)
	   true-preconds)))

  (defmacro get-real-effects (cr)
    `(if (and (eq *planner-for-learning* 'ipss) (p4::control-rule-p ,cr))
	 (cdr (p4::control-rule-then ,cr))
	 (cdr (cadddr ,cr))))

  (defmacro generic-node-name (node)
    `(if (eq *planner-for-learning* 'ipss)
	 (p4::nexus-name ,node)
	 (snode-number ,node)))

  (defmacro generic-node-children (node)
    `(if (eq *planner-for-learning* 'ipss)
	 (p4::nexus-children ,node)
	 (snode-children ,node)))

  (defmacro generic-all-types nil
    `(if (eq *planner-for-learning* 'sayphi)
	 (let ((the-types (dom-domtypes *pspace*))
	       (result nil))
	   (if (listp (car the-types))
	       (dolist (the-types-1 the-types result)
		 (pushnew (car the-types-1) result)
		 (dolist (the-type (cadr the-types-1))
		   (pushnew the-type result)))
	       (reverse (cons 'object the-types))))))

  (defmacro generic-type-name-to-type (type pspace)
    `(if (eq *planner-for-learning* 'ipss)
	 (p4::type-name-to-type ,type ,pspace)
	 ,type))

  (defmacro generic-type-name (type)
    `(if (eq *planner-for-learning* 'ipss)
	 (p4::p4type-name ,type)
	 ,type))

  (defmacro generic-type-all-parents (type)
    `(if (eq *planner-for-learning* 'ipss)
	 (p4::p4type-all-parents ,type)
	 (find-all-type-parents ,type)))

  (defmacro reverse-binding (binding)
    `(mapcar #'(lambda (bind-pair)
		 (cons (cdr bind-pair) (car bind-pair)))
	     ,binding))

  ;; Tests whether it expanded the whole tree and did not find the solution
  (defmacro whole-tree-expanded-p nil
    `(and (eq *planner-for-learning* 'ipss)
	  (prodigy-result-whole-tree-expanded-p *prodigy-result*)
	  t))

  ;; Tests whether it expanded the whole tree and did not find the solution
  (defmacro whole-tree-expanded-no-solution-p nil
    `(if (eq *planner-for-learning* 'ipss)
	 (and (prodigy-result-whole-tree-expanded-p *prodigy-result*)
	      (not (prodigy-result-solutionp *prodigy-result*)))
	 (and (whole-tree-expanded-p)
	      (not (solutionp)))))

  ;; Returns t if there is a solution path
  (defmacro solutionp nil
    `(if (eq *planner-for-learning* 'ipss)
	 (prodigy-result-solutionp *prodigy-result*)
	 (solution-found *say-solution*)))

  ;; Returns the name of the control rules file
  (defmacro cr-file nil
    `(format nil "~a~a" (if (eq *planner-for-learning* 'ipss)
			    *domain-directory*
			    *domain-dir*)
	     *cr-file*))

  ;; Returns the name of the FF domain dependent control rules file
  (defmacro ff-cr-file nil
    `(format nil "~a~a" ,(if (eq *planner-for-learning* 'ipss)
			     *domain-directory*
			     *domain-dir*)
	     *ff-cr-file*))

  ;; The new macros for genericity
  (defmacro hamlet-domain-name nil
    (declare (special *current-problem-space* *pspace*))
    `(if (eq *planner-for-learning* 'ipss)
	 (p4::problem-space-name *current-problem-space*)
	 (car (dom-name *pspace*))))
  )