;;; This file contains the code for doing induction over the control rules.
;;; this version induces first over the induces rules, and then over
;;; the deduced ones

(in-package "COMMON-LISP-USER")



;;; ******************************************************************
;;;   functions for building the initial decision tree
;;; ******************************************************************
;;;
;;; for now, i didn't build the reject and prefer parts.
;;;
(defun build-fixed-decision-tree nil
  (let ((root (make-decision-node :name 'root :children-type 'effect)))
    (setf (decision-node-parent root) root)
    (setf *decision-tree* root)
    (let* ((root-children-1 (build-children root (cons 'sub-goal (if *learn-apply-p* (list 'apply))) 'prior-goal nil))
	   (root-children (build-children root '(select reject prefer) 'effect))
	   (operators (get-all-operators))
	   (goals (get-all-goals))
	   (select-children-2 (build-children (first root-children) '(instantiated-operator) 'instantiated-operator))
	   (select-children-1 (build-children (first root-children) '(bindings) 'current-operator))
	   (select-children (build-children (first root-children) '(goals operators) 'effect))
	   (select-goals-children (build-children (first select-children) goals 'prior-goal nil))
	   (select-operators-children (build-children (second select-children) operators 'current-goal))
	   (select-bindings-children (build-children (first select-children-1) operators 'current-goal))
	   (select-instantiated-children (build-children (first select-children-2) operators 'prior-goal nil))
	   (select-nietos-operators (build-nietos select-operators-children goals 'prior-goal))
	   (select-nietos-bindings (build-nietos select-bindings-children goals 'prior-goal))
;; 	   (select-nietos-instantiated (build-nietos select-instantiated-children goals 'prior-goal))
	   (reject-children-1 (build-children (second root-children) '(bindings) 'current-operator))
	   (reject-children (build-children (second root-children) '(goals operators) 'effect))
	   (reject-goals-children (build-children (first reject-children) goals 'prior-goal nil))
	   (reject-operators-children (build-children (second reject-children) operators 'current-goal))
	   (reject-bindings-children (build-children (first reject-children-1) operators 'current-goal))
	   (reject-nietos-operators (build-nietos reject-operators-children goals 'prior-goal))
	   (reject-nietos-bindings (build-nietos reject-bindings-children goals 'prior-goal))
	   (prefer-children-1 (build-children (third root-children) '(bindings) 'current-operator))
	   (prefer-children (build-children (third root-children) '(goals operators) 'effect))
	   (prefer-goals-children (build-children (first prefer-children) goals 'prior-goal nil))
	   (prefer-operators-children (build-children (second prefer-children) operators 'current-goal))
	   (prefer-bindings-children (build-children (first prefer-children-1) operators 'current-goal))
	   (prefer-nietos-operators (build-nietos prefer-operators-children goals 'prior-goal))
	   (prefer-nietos-bindings (build-nietos prefer-bindings-children goals 'prior-goal))
	   (leafs (cons 'any-goal (if *divide-by-prior-goal-p* goals))))
      (build-nietos select-goals-children leafs nil)
      (build-nietos select-nietos-operators leafs nil)
      (build-nietos select-nietos-bindings leafs nil)
      (build-nietos select-instantiated-children leafs nil)
      (build-nietos reject-goals-children leafs nil)
      (build-nietos reject-nietos-operators leafs nil)
      (build-nietos reject-nietos-bindings leafs nil)
      (build-nietos prefer-goals-children leafs nil)
      (build-nietos prefer-nietos-operators leafs nil)
      (build-nietos prefer-nietos-bindings leafs nil)
      (build-children (first root-children-1) leafs nil nil)
      (if *learn-apply-p* (build-children (second root-children-1) leafs nil nil))
      (values))))

(defun build-children (node alternatives children-type &optional (fixedp t))
  (declare (type decision-node node)
	   (atom children-type fixedp)
	   (list alternatives))
  ;;  (format t "~%creating children of ~a~% alternatives: ~a~% children type: ~a"
  ;;	  (decision-node-name node) alternatives children-type)
  (setf (decision-node-children node)
	(append (mapcar #'(lambda (alternative)
			    (make-decision-node :name alternative
						:fixedp fixedp
						:children-type children-type
						:children nil
						:parent node))
			alternatives)
		(decision-node-children node))))

(defun build-nietos (children alternatives children-type)
  (declare (atom children-type)
	   (list children alternatives))
  (let ((nietos nil))
    (dolist (child children nietos)
      (setq nietos (append (build-children child alternatives children-type (not (eq children-type 'prior-goal)))
			   nietos)))))

;;;******************************************************************
;;;  general functions for inducing new rules
;;;******************************************************************

;; if slow-induction-p is t, it will print the results in the same
;; list as it is printing. if not, it should save them separetely
(defun learn-from-induction (slow-induction-p induced-file control-rules savep initp test-negatives-p)
  (declare (string induced-file)
	   (list control-rules)
	   (atom slow-induction-p savep initp test-negatives-p))
  (init-induction initp)
  (if savep (format t "~%***************** Inducing...~%"))
  (let ((t-inicial (get-internal-run-time)))  
    (dolist (cr control-rules)
      (let ((preconds (get-real-preconds cr))
	    (effects (get-real-effects cr)))
	(classify-rule cr preconds effects *decision-tree* t test-negatives-p)))
    (setf *cr-file* induced-file)
    (save-induction (elapsed-time t-inicial) slow-induction-p savep)))

(defun init-induction (initp)
  (declare (atom initp))
  (setf *top-type* (if (eq *planner-for-learning* 'ipss)
		       (p4::type-name-to-type :top-type *current-problem-space*)
		       'object))
  (setf *cr-list* nil)
  (if initp (clear-decision-tree *decision-tree*)))

(defun clear-decision-tree (node)
  (declare (type decision-node node))
  (setf (decision-node-rules node) nil)
  (dolist (child (decision-node-children node))
    (clear-decision-tree child)))


;;; ******************************************************************
;;;   functions that traverse the tree according to the new rule
;;; ******************************************************************

;;; if storep is t, the chain of functions will try to store it, and,
;;; therefore, induce over it. if nil, it will return the leaf node of
;;; the decision tree where it belongs
(defun classify-rule (cr preconds effects node storep test-negatives-p)
  (declare (type decision-node node)
	   (atom storep test-negatives-p)
	   (list preconds effects))
  (if (or (eq (decision-node-children-type node) 'effect)  (eq (decision-node-children-type node) 'instantiated-operator))
      (classify-effect cr preconds effects node storep test-negatives-p)
      (classify-preconds cr preconds effects node storep test-negatives-p)))

(defun classify-preconds (cr preconds effects node storep test-negatives-p)
  (declare (type decision-node node)
	   (atom storep test-negatives-p)
	   (list preconds effects))
  (let* ((decision (decision-node-children-type node))
	 (fixedp (or (not (eq decision 'prior-goal))
		     (decision-node-fixedp node))))
    (if fixedp
	(let* ((precond-found (find-if #'(lambda (precond) (eq (car precond) decision))
				       preconds))
	       (rest-preconds (if precond-found (remove precond-found preconds :test #'equal)))
	       (decision-names (if precond-found
				   (case decision
				     (current-operator (cdr precond-found))
				     ((target-goal current-goal) (list (car (get-pos-assertion (cadr precond-found)))))
				     (prior-goal (mapcar #'(lambda (goal)
							     (car (get-pos-assertion goal)))
							 (cadr precond-found)))
				     (t 'error))))
	       (childs-found (if precond-found
				 (mapcar #'(lambda (name)
					     (find-decision-child name node))
					 decision-names))))
	  (if childs-found
	      (if storep
		  (dolist (child childs-found)
		    (if child
			(classify-preconds cr rest-preconds effects child storep test-negatives-p)))
		  (classify-preconds cr rest-preconds effects (find-if #'(lambda (child) child) childs-found) storep test-negatives-p))))
        (let ((child (find-decision-child 'any-goal node)))
	  (if storep
	      (classify-rest cr child test-negatives-p)
	      child)))))

		     
(defun find-decision-child (decision-name node)		     
  (declare (type decision-node node)
	   (atom decision-name))
  (find-if #'(lambda (child)
	       (eq (decision-node-name child) decision-name))
	   (decision-node-children node)))

(defun classify-effect (cr preconds effects node storep test-negatives-p)
  (declare (type decision-node node)
	   (atom storep test-negatives-p)
	   (list preconds effects))
  (let ((effect-found (find-if #'(lambda (child)
				   (eq (decision-node-name child)
				       (if (listp (car effects))
					   (caar effects)
					   (car effects))))
			       (decision-node-children node))))
    (if effect-found
	(if (eq (car effects) 'bindings)
	    (classify-rule cr preconds effects effect-found storep test-negatives-p)
	    (classify-rule cr preconds (cdr effects) effect-found storep test-negatives-p))
        'error)))


;;; ******************************************************************
;;;   functions for induction on the leafs
;;; ******************************************************************

(defun classify-rest (cr node test-negatives-p)
  (declare (type decision-node node)
	   (atom test-negatives-p))
  (if (not (decision-node-needs-language-change-p node))
      (if (and test-negatives-p
	       (includes-negative-examples-p (get-real-preconds cr) (get-real-effects cr) (decision-node-negatives node)))
	  (let* ((rule-name (generic-control-rule-name cr))
		 (le (gethash rule-name *cr-to-le-hash*)))
	    (format t "Marking ~a as deleted~%" rule-name)
	    (incf *number-deleted-rules*)
	    (if (not (recover le node nil))
		(setf (learning-episode-deletedp le) 'forever)))
	  (let ((rules (get-decision-node-rules node)))
	    (if (or (not rules)
		    (not (induce-over-induced cr (get-real-preconds cr) (get-real-effects cr) node rules)))
		(store-deduced-rule cr node))))))

;;; In case it couldn't induce, it stores the rule on the node, if it doesn't match any negative example.
(defun store-deduced-rule (cr node)
  (declare (type decision-node node))
  (let ((rule-name (generic-control-rule-name cr)))
    (if (eq (learning-episode-type (gethash rule-name *cr-to-le-hash*)) 'induced)
	(format t "Readding ~a~%" rule-name)
        (format t "Adding ~a~%" rule-name))
    (if (or (eq *probabilities-p* 'rules) 
	    (eq *probabilities-p* 'both))
	(if (not (get-number-rule rule-name))
	    (nueva-regla-prob rule-name (get-real-effects cr))))
    (if (>= (length (push cr (decision-node-rules node)))
	    (decision-node-threshold node))
	(setf *over-threshold-p* t))
    (push cr *cr-list*)))

;; Returns t if there has been an intersection, and it creates an
;; induced rule if so.
;; rules-bindings is of the form: ((rule1 rule2 other-bindings) ...)
;; last-binding has the last one, which it is going to be used for inducing
(defun induce-over-induced (cr preconds1 effects1 node rules)
  (declare (type decision-node node)
	   (list preconds1 effects1 rules))
  (if rules
      (let* ((rule (car rules))
	     (preconds2 (get-real-preconds rule))
	     (effects2 (get-real-effects rule))
	     (bindings (equal-control-rule-p preconds1 preconds2 effects1 effects2 nil))
	     (intersection (some-intersection cr preconds1 effects1 rule preconds2 effects2 node bindings)))
	(or intersection
	    (induce-over-induced cr preconds1 effects1 node (cdr rules))))))

;; It tries all possible bindings, calling again induce-over-induced
;; and returns as soon as one returns t. I do not use some because I
;; need to use the variable in the form
;; if it finds an intersection but it is equal to any of the rules,
;; then it does not learn the rule, by returning t.
;; Now, it only considers *max-number-bindings* bindings, since it is
;; very costly to consider all possible bindings. I guess, I'll have
;; to check whether at the end there are many repeated rules.
(defun some-intersection (cr preconds1 effects1 rule preconds2 effects2 node bindings)
  (declare (type decision-node node)
	   (list preconds1 effects1 preconds2 effects2 bindings))
  (do* ((rest-bindings (if (<= (length bindings) *max-number-bindings*)
			   bindings
			   (select-n-random bindings *max-number-bindings*))
		       (cdr rest-bindings))
	(relevant-intersection-p nil))
      ((or (null rest-bindings)
	   relevant-intersection-p)
       (if (eq relevant-intersection-p 'deleted-old)
	   nil
	   relevant-intersection-p))
    (let* ((new-rule (find-relevant-intersection preconds1 preconds2 cr effects1 effects2 (car rest-bindings) node t))
	   (new-intersection (car new-rule)))
      (if new-intersection
	  (if (eq new-intersection 'equal)
	      (cond ((cdr new-rule)
		     (let* ((rule-name (generic-control-rule-name rule))
			    (le (gethash rule-name *cr-to-le-hash*))
			    (deducedp (eq (learning-episode-type le) 'deduced)))
		       ;; if the rule is a deduced one we cannot delete it forever, since when re-installing we might need it
		       (delete-cr rule node le (if deducedp t 'forever))
		       (setq relevant-intersection-p 'deleted-old)))
		    (t
		     (let* ((rule-name (generic-control-rule-name cr))
			    (le (gethash rule-name *cr-to-le-hash*)))
		       (format t "Equal rule ~a deleted~%" rule-name)
		       (if (or (eq *probabilities-p* 'rules) 
			       (eq *probabilities-p* 'both))
			   (borra-regla-prob cr nil rule))
		       (incf *number-deleted-rules*)
		       (setf *deleted-rule-p* t)
		       (if node
			   (setf (decision-node-rules node)
				 (remove cr (decision-node-rules node) :test #'eq-name)))
		       (setq relevant-intersection-p
			     (setf (learning-episode-deletedp le) 'forever)))))
	      (let* ((new-effects (cadr new-rule))
		     (new-bindings (cddr new-rule))
		     (new-rules-bindings (list cr rule (cdr rest-bindings))))
		(setq relevant-intersection-p t)
		(learn-induced-rule new-rules-bindings new-intersection
				    new-effects new-bindings node t)))))))

;;; It extracts n random elements from list
(defun select-n-random (list n)
  (let ((new-list nil))
    (dotimes (i n new-list)
      (let ((elem (nth (random (length list)) list)))
	(setq list (remove elem list :test #'equal))
	(push elem new-list)))))

;;; It finds a relevant intersection by checking whether the intersection
;;; is different than the previous preconds and if it doesn't include
;;; negative examples. it returns:
;;;  (new-preconds new-effects new-binding new-types-binding)
;;; if it finds that the intersection is equal to any one of the
;;; previous preconds, it returns: (equal equal1p), where equal1p
;;; reflects whether it was the equal to the first one or not.
(defun find-relevant-intersection (preconds1 preconds2 rule1 effects1 effects2 binding node check-equality-p)
  (declare (type decision-node node)
	   (atom check-equality-p)
	   (list preconds1 effects1 preconds2 effects2 binding))
  (let* ((intersection-bindings (intersection-and-bindings preconds1 preconds2 binding))
	 (intersection (car intersection-bindings))
	 (new-types-binding (cdr intersection-bindings))
	 (new-binding (merge-bindings binding new-types-binding))
	 (new-effects (sublis new-binding (get-real-effects rule1)))
	 (new-intersection (remove-old-types intersection (get-all-vars intersection new-effects)))
	 (new-rule (list new-intersection new-effects new-binding new-types-binding)))
    (if new-intersection
	(let* ((equal1p (and (equal new-intersection (sublis new-binding preconds1))
			     (equal-effects-p effects1 effects2 new-binding)))
	       (equal2p (or equal1p
			    (and (equal-set-p (sublis new-types-binding preconds2) new-intersection)
				 (equal-effects-p effects1 effects2 new-types-binding)))))
	  (if (or equal1p equal2p)
	      (if check-equality-p
		  (cons 'equal equal1p))
	      (if (not (includes-negative-examples-p new-intersection new-effects (decision-node-negatives node)))
		  new-rule))))))

;;; Merges two binding lists, in such a way that the result will be as
;;; a sequential application of both bindings
(defun merge-bindings (binding1 binding2)
  (declare (list binding1 binding2))
  (let ((result binding1))
    (dolist (bind-pair binding2 result)
      (let ((existsp (member (car bind-pair) result :test #'eq :key #'cdr)))
	(cond (existsp
	       (setq result (remove (car existsp) result :test #'equal))
	       (push (cons (caar existsp) (cdr bind-pair)) result))
	      (t (pushnew bind-pair result :test #'equal)))))))

;; It returns a cons of the common preconds and the new bindings due
;; to new names of variables (type generalized)
;; IMPORTANT: Next, we have to compute the set of all-possible
;; prior-goals for a certain goal, and if the set on any-prior-goal is
;; equal to that one, then delete this precond.
;;
;; I changed into union in the some-candidate-goals because that is
;; the way to do. I also changed the some-candidate-goals
;; meta-predicate so that it tests whether any of the goals is a
;; pending goal
;;
;; It creates new var names, so that unification is easier
;;
;; When the number of some-candidate-goals is greater than a ratio of the
;; number of predicates in the domain, it removes the some-candidate-goals
;; metapredicate
(defun intersection-and-bindings (plain-preconds1 preconds2 binding)
  (declare (list plain-preconds1 preconds2 binding))
  (setf *rule-id* (string-trim "#:G" (gensym)))
  (setf *number-vars-in-cr* 0)
  (let ((preconds1 (sublis binding plain-preconds1))
	(new-binding (create-new-binding binding))
	(common-preconds nil)
	(new-prior-goal nil)
	(new-some-candidate-goals nil)
	(candidate-goals1 nil)
	(prior-goal1 nil)
	(rest-preconds2 preconds2)
	(common-vars nil)
	(number-true-in-state 0))
    (dolist (precond1 preconds1)
      (setq preconds2 rest-preconds2)
      (do* ((p2 preconds2 (cdr p2))
	    (equalp (equal (car p2) precond1)
		    (equal (car p2) precond1)))
	   ((or (null p2) equalp)
	    (cond (equalp
		   (push precond1 common-preconds)
		   (if (eq (car precond1) 'true-in-state)
		       (incf number-true-in-state))
		   (setq rest-preconds2 (remove (car p2) rest-preconds2 :test #'equal)))
		  (t (case (car precond1)
		       (prior-goal (setq prior-goal1 precond1))
		       (some-candidate-goals (setq candidate-goals1 precond1))
		       (type-of-object
			(let* ((var (cadr precond1))
			       (supertype (common-supertype preconds1 rest-preconds2 (cons var var))))
			  (if supertype
			      (let ((new-var (induced-var-name supertype)))
				(push var common-vars)
				;; To delete previous assignations of var
				(setq new-binding (delete-if #'(lambda (bind-pair)
								 (eq (car bind-pair) var))
							     new-binding))
				(push (cons var new-var) new-binding)
				(push `(type-of-object ,var ,supertype) common-preconds))
			      (push precond1 common-preconds))))
		       (t nil)))))))
    (when (or (not *check-number-states*)
	      (>= number-true-in-state *state-threshold*))
      (dolist (precond2 rest-preconds2)
	(case (car precond2)
	  (prior-goal
	   (let* ((goals1 (if prior-goal1 (cadr prior-goal1)))
		  (goals2 (cadr precond2))
		  (union (union goals1 goals2 :test #'equal))
		  (all-vars (mapcar #'car new-binding))
		  (new-vars nil))
	     (cond ((< (length union)
		       (* *max-ratio-some-goals* (length (get-all-goals))))
		    (dolist (goal union)
		      (dolist (var (cdr (get-pos-assertion goal)))
			(if (not (member var all-vars :test #'eq))
			    (pushnew var new-vars :test #'eq))))
		    ;;	     (setq new-vars (set-difference new-vars all-vars))
		    (setq new-binding (append new-binding (create-new-binding (mapcar #'(lambda (new-var)
											  (cons new-var new-var))
										      new-vars))))		  
		    (setq new-prior-goal `(prior-goal ,union)))
		   (t (setq new-prior-goal 'none)))))
	  (some-candidate-goals
	   (let* ((goals1 (if candidate-goals1 (cadr candidate-goals1)))
		  (goals2 (cadr precond2))
		  (all-vars (mapcar #'car new-binding))
		  (union (my-union goals1 goals2))
		  (new-vars nil))
	     (cond ((< (length union)
		       (* *max-ratio-some-goals* (length (get-all-goals))))
		    (dolist (goal union)
		      (dolist (var (cdr (get-pos-assertion goal)))
			(if (not (member var all-vars :test #'eq))
			    (pushnew var new-vars :test #'eq))))
		    ;;	     (setq new-vars (set-difference new-vars all-vars))
		    (setq new-binding
			  (append new-binding (create-new-binding (mapcar #'(lambda (new-var)
									      (cons new-var new-var))
									  new-vars))))
		    (setq new-some-candidate-goals `(some-candidate-goals ,union)))
		   (t (setq new-some-candidate-goals 'none)))))
	  (type-of-object
	   (if (not (member (cadr precond2) common-vars))
	       (push precond2 common-preconds)))
	  (t nil)))
      (cond ((eq new-prior-goal 'none)
	     (setq new-prior-goal nil))
	    ((and prior-goal1 (not new-prior-goal))
	     (setq new-prior-goal prior-goal1))
	    (t nil))
      (cond ((eq new-some-candidate-goals 'none)
	     (setq new-some-candidate-goals nil))
	    ((and candidate-goals1 (not new-some-candidate-goals))
	     (setq new-some-candidate-goals candidate-goals1))
	    (t nil))
      (build-induced-preconds new-binding common-preconds (sublis new-binding new-prior-goal) (sublis new-binding new-some-candidate-goals)))))

;;; it was push `(subset-candidate-goals...) and intersection instead
;;; of union

(defun induced-var-name (type)
  (declare (atom type))
  (incf *number-vars-in-cr*)
  (intern (format nil "~:@(<I-~a-~d-~d>~)" type *rule-id* *number-vars-in-cr*)))

;; It reverses the preconds list, substitutes the old variables for
;; the new ones (after inducing), and introduces in their right place
;; the new preconds of subset-candidate-goals and any-prior-goal in
;; case they are created.
(defun build-induced-preconds (new-binding common-preconds prior-goal some-candidate-goals)
  (declare (list new-binding common-preconds prior-goal some-candidate-goals))
  (let ((result nil)
	(introducedp nil))
    (dolist (common-precond common-preconds (cons result new-binding))
      (let ((precond (sublis new-binding common-precond)))
	(if (and (member (car precond) '(current-goal candidate-goal target-goal))
		 prior-goal)
	    (push prior-goal result))
	(when (and (not introducedp)
		   (eq (car precond) 'true-in-state)
		   some-candidate-goals)
	  (setq introducedp t)
	  (push some-candidate-goals result))
	(push precond result)))))

;; (all-vars (get-all-vars new-preconds new-effects))
;; (remove-old-types new-preconds all-vars)
(defun learn-induced-rule (crs-bindings new-preconds new-effects new-bindings node inducedp)
  (declare (type decision-node node)
	   (atom inducedp)
	   (list crs-bindings new-preconds new-effects new-bindings))
  (setq new-preconds (sort-preconds new-preconds))
  (let* ((new-name (induced-name new-effects (if (eq (cadr new-effects) 'bindings) (cadar new-preconds))))
	 (new-cr (generic-make-control-rule new-name new-preconds new-effects))
	 (cr1 (car crs-bindings))
	 (cr2 (cadr crs-bindings))
	 (binding1 (car new-bindings))
	 (binding2 (cadr new-bindings))
	 (le1 (gethash (generic-control-rule-name cr1) *cr-to-le-hash*))
	 (le2 (gethash (generic-control-rule-name cr2) *cr-to-le-hash*))
	 (prior-goal1 (learning-episode-prior-goal le1))
	 (prior-goal2 (learning-episode-prior-goal le2))
	 (prior-goals (if (eq *planner-for-learning* 'ipss) (generate-induced-prior-goals binding1 prior-goal1 binding2 prior-goal2)))
	 (all-prior-goals (mapcar #'cadr prior-goals))
	 (new-learning-episode (make-learning-episode :name (get-new-le-name)
						      :type (if inducedp 'induced 'refined)
						      :rule new-cr
						      :problem *problem-name*
						      :from nil
						      :other-bindings (if (caddr crs-bindings) crs-bindings)
						      :binding-used (list (cons cr1 binding1) (cons cr2 binding2))
						      :prior-goal (create-new-prior-goals prior-goals new-preconds)
						      :learning-steps (1+ (max (learning-episode-learning-steps le1)
									       (learning-episode-learning-steps le2)))
						      :preconds-pool nil
						      :matching-time (+ (learning-episode-matching-time le1) (learning-episode-matching-time le2))
						      :matchings (+ (learning-episode-matchings le1) (learning-episode-matchings le2))
						      :num-nodes (/ (+ (learning-episode-num-nodes le1) (learning-episode-num-nodes le2)) 2.0)
						      :firings (+ (learning-episode-firings le1) (learning-episode-firings le2)))))
    (push new-learning-episode *le-list*)
    (update-frequencies new-preconds node t)
;;     (update-frequencies (add-prior-goals all-prior-goals new-preconds) node t)
    (setf (gethash new-name *cr-to-le-hash*) new-learning-episode)
    (push new-cr *cr-list*)
    (format t "~%Inducing ~a~%" new-name)
    (when (or (eq *probabilities-p* 'rules) 
	      (eq *probabilities-p* 'both))
      (nueva-regla-prob new-name new-effects))
    (if *printp* (pp-control-rule new-cr))
    (incf *number-induced-rules*)
    (setf *deleted-rule-p* t)
    (push new-cr (decision-node-rules node))
    (if inducedp (update-decision-tree node crs-bindings new-learning-episode))
    ;; I hope this will solve the problem of not inducing over all stored rules
    (induce-over-induced new-cr new-preconds new-effects node (remove new-cr (decision-node-rules node)))
    new-learning-episode))

;; Daniel. Added on March 2001
;; Auxiliary function for computing new prior-goals of the induced rule from
;; the ones of previous rules. If there was a problem on a previous step
;; placing a nil in a rule, it removes the prior goals given that we are not
;; using them anymore.
(defun generate-induced-prior-goals (binding1 prior-goal1 binding2 prior-goal2)
  (if (and (listp prior-goal1)
	   (car prior-goal1)
	   (listp prior-goal2)
	   (car prior-goal2))
      (remove-duplicates (append (sublis binding1 prior-goal1) (sublis binding2 prior-goal2)) :test #'equal)))

;; Updates the decision-tree and the learning episodes of the old
;; rules from where it induced. The test for negatives used to be
;; equal-set-p
(defun update-decision-tree (node crs-binding new-learning-episode)
  (declare (type decision-node node)
	   (type learning-episode new-learning-episode)
	   (list crs-binding))
  (let ((from-crs nil))
    (dolist (old-cr (nreverse (butlast crs-binding)) (terpri))
      (unless (member old-cr from-crs)
	(push old-cr from-crs)
	(let ((old-le (gethash (generic-control-rule-name old-cr) *cr-to-le-hash*)))
	  (push old-le (learning-episode-from new-learning-episode))
	  (delete-cr old-cr node old-le t))))))

;;; Updates the frequencies of appearance of preconds of the state and
;;; prior goal, as attributes, for the later computation of the
;;; information gain.
;;; It supposses that run has already been executed after the last
;;; call to domain. Otherwise, not all predicates of the domain will
;;; be there.
;;; Negative goals will be handled as positive goals.
(defun update-frequencies (preconds node positivep)
  (declare (type decision-node node)
	   (atom positivep)
	   (list preconds))
  (do* ((conds preconds (cdr conds))
	(stop-p nil)
	(frequency-table-positive (or (decision-node-frequency-table-positive node)
				      (setf (decision-node-frequency-table-positive node)
					    (make-hash-table))))
	(frequency-table-negative (or (decision-node-frequency-table-negative node)
				      (setf (decision-node-frequency-table-negative node)
					    (make-hash-table))))
	(frequency-table-prior-goal-positive (or (decision-node-frequency-table-prior-goal-positive node)
						 (setf (decision-node-frequency-table-prior-goal-positive node)
						       (make-hash-table))))
	(frequency-table-prior-goal-negative (or (decision-node-frequency-table-prior-goal-negative node)
						 (setf (decision-node-frequency-table-prior-goal-negative node)
						       (make-hash-table)))))
       ((or (null conds) stop-p))
    ;;    (print (car conds))
    (case (caar conds)
      (true-in-state
       (let ((predicate (car (get-pos-assertion (cadar conds)))))
	 (if positivep
	     (if (gethash predicate frequency-table-positive)
		 (incf (gethash predicate frequency-table-positive))
		 (setf (gethash predicate frequency-table-positive) 1))
	     (if (gethash predicate frequency-table-negative)
		 (incf (gethash predicate frequency-table-negative))
		 (setf (gethash predicate frequency-table-negative) 1)))))
      (prior-goal
       (dolist (goal (cadar conds))
	 (let ((predicate (car (get-pos-assertion goal))))
	   (if positivep
	       (if (gethash predicate frequency-table-prior-goal-positive)
		   (incf (gethash predicate frequency-table-prior-goal-positive))
		   (setf (gethash predicate frequency-table-prior-goal-positive) 1))
	       (if (gethash predicate frequency-table-prior-goal-negative)
		   (incf (gethash predicate frequency-table-prior-goal-negative))
		   (setf (gethash predicate frequency-table-prior-goal-negative) 1))))))
      ((some-candidate-goals type-of-object)
       (setq stop-p t))
      (t nil))))

;;; Adds the prior-goals to the preconds if they are not already
;;; there. Prior-goals is a list of just the prior-goals, without the rules
(defun add-prior-goals (prior-goals preconds)
  (declare (list prior-goals preconds))
  (if prior-goals
      (let ((new-prior-goals (some #'(lambda (precond)
				       (if (prior-goal-meta-pred-p precond)
					   (copy-list (cadr precond))))
				   preconds)))
	(dolist (cond prior-goals)
	  (dolist (goal (cadr cond))
	    (pushnew goal new-prior-goals :test #'equal)))
	(if new-prior-goals
	    (replace-prior-goal preconds (list 'prior-goal new-prior-goals))
	    preconds))
      preconds))

(defun replace-prior-goal (preconds new-prior-goals)
  (declare (list new-prior-goals preconds))
  (cond ((null preconds) (list new-prior-goals))
	((eq (caar preconds) 'prior-goal)
	 (cons new-prior-goals (cdr preconds)))
	((member (caar preconds) (list 'true-in-state 'some-candidate-goals 'type-of-object))
	 (cons new-prior-goals preconds))
	(t (cons (car preconds) (replace-prior-goal (cdr preconds) new-prior-goals)))))

;; It adds the types of the variables that go away with the induction
(defun create-new-prior-goals (prior-goals new-preconds)
  (declare (list prior-goals new-preconds))
  (mapcar #'(lambda (cr-prior-goal-type-def)
	      (let ((cr (car cr-prior-goal-type-def))
		    (prior-goal (cadr cr-prior-goal-type-def))
		    (type-defs (caddr cr-prior-goal-type-def)))
		(list cr prior-goal
		      (get-types-of-prior-goal prior-goal new-preconds
					       (append (get-real-preconds cr)
						       type-defs)))))
	  prior-goals))
  
(defun get-new-le-name nil
  (let ((new-name (read-from-string (format nil "LE-~d" *le-index*))))
    (incf *le-index*)
    new-name))

(defun induced-name (effects operator)
  (declare (list effects)
	   (atom operator))
;;  (setf *rule-id* (string-trim "#:G" (gensym)))
  (intern (format nil "~:@(induced-~a~)" (name-cr effects operator))))

(defun my-subset-p (rule1 rule2)
  (declare (list rule1 rule2))
  (equal-control-rule-p (car rule1) (car rule2) (cdr rule1) (cdr rule2) 'subset))

;; Given the preconds and effects of a control rule, it returns all
;; vars that appear in all preconds and effects, except for the ones
;; in the type-of-object meta-predicate
(defun get-all-vars (preconds effects)
  (declare (list preconds effects))
  (let ((all-vars nil))
    (dolist (precond preconds)
      (case (car precond)
	((target-goal current-goal candidate-goal true-in-state applicable-op)
	 (dolist (var (cdadr precond))
	   (pushnew var all-vars :test #'eq)))
	((prior-goal some-candidate-goals)
	 (dolist (goal (cadr precond))
	   (dolist (var (cdr goal))
	     (pushnew var all-vars :test #'eq))))
	(t nil)))
    (case (cadr effects)
      (bindings (dolist (bind-pair (caddr effects) all-vars)
		  (pushnew (cdr bind-pair) all-vars :test #'eq)))
      ((instantiated-operator goals) (dolist (var (cdaddr effects) all-vars)
				       (pushnew var all-vars :test #'eq)))
      (t all-vars))))

;; It removes the type-of-object meta-predicates that do not have one
;; of the vars in other precond or effect
(defun remove-old-types (preconds all-vars)
  (declare (list preconds all-vars))
  (remove-if #'(lambda (precond)
		 (and (eq (car precond) 'type-of-object)
		      (not (member (cadr precond) all-vars :test #'eq))))
	     preconds))


;;; ******************************************************************
;;;       Repairing over generalization functions
;;; ******************************************************************

;;; Adds a new rule to the list of rules to be saved
(defun add-rule (new-rule test-negatives-p)
  (declare (atom test-negatives-p)
	   (list new-rule))
  (let ((all-rules *cr-list*))
    (learn-from-induction t *cr-file* (list new-rule) nil nil test-negatives-p)
    ;; some rules might have been deleted because they are
    ;; more specific than the new rule
    (dolist (rule all-rules *cr-list*)
      (let ((le (gethash (generic-control-rule-name rule) *cr-to-le-hash*)))
	(if (and le (not (learning-episode-deletedp le)))
	    (push rule *cr-list*))))))

;;; Tests whether the rule is still ok. If not, it tries to refine it.
;;; We first ask to whether it has been deleted, because it might have
;;; been deleted to be induced from.
(defun revisit-rule (rule le decision-node negatives test-inclusion-p)
  (declare (type learning-episode le)
	   (type decision-node decision-node)
	   (atom test-inclusion-p)
	   (list negatives))
  (if (and rule le
	   (or (not test-inclusion-p)
	       (includes-negative-examples-p (get-real-preconds rule) (get-real-effects rule) negatives)))
      (if (eq (learning-episode-type le) 'deduced)
	  (or (recover le decision-node nil) t)
	  (refine-rule le decision-node))))

;;; Reinduces the rules of a certain node, if the number of rules on
;;; that node is greater than its threshold.
;;; I don't know why, I'm getting a nil in between the rules of a
;;; node. Until I discover the bug, I'll delete all nils from the list
;;; of rules.
(defun restructure-node (node)
  (declare (type decision-node node))
  (let* ((rules (shuffle (remove-nil-from-rules (get-decision-node-rules node))))
	 (all-rules (set-difference *cr-list* rules :test #'eq)))
    (format t "~2%***************** Restructuring a node...~%")
    (setf (decision-node-rules node) nil)
    (learn-from-induction t *cr-file* rules nil nil nil)
    (setf *cr-list* (append *cr-list* all-rules))))

(defun remove-nil-from-rules (rules)
  (remove nil rules))

;; It assumes the rule is wrong.
;; It tries another binding. If any of the bindings works, it learns
;; another rule. If not, it calls refine-parents.
(defun refine-rule (learning-episode node)
  (declare (type learning-episode learning-episode)
	   (type decision-node node))
;;  (setf (learning-episode-deletedp learning-episode) t)
  (let ((some-other-binding-p
	 (some-other-binding-p learning-episode node (learning-episode-other-bindings learning-episode))))
    (if some-other-binding-p
	(let* ((other-bindings (car some-other-binding-p))
	       (intersection (cadr some-other-binding-p))
	       (new-effects (caddr some-other-binding-p))
	       (new-bindings (cdddr some-other-binding-p))
	       (cr1 (car other-bindings))
	       (cr2 (cadr other-bindings))
	       (new-other-bindings (list cr1 cr2 (cdaddr other-bindings)))
	       (old-cr (learning-episode-rule learning-episode))
	       (decision-leaf (find-decision-tree-node old-cr))
	       (new-learning-episode (learn-induced-rule new-other-bindings intersection new-effects new-bindings decision-leaf nil)))
	  (really-delete-rule learning-episode old-cr decision-leaf)
	  (propagate-changes learning-episode new-other-bindings)
;;	  (push learning-episode (learning-episode-from new-learning-episode))
	  (push (gethash (generic-control-rule-name cr2) *cr-to-le-hash*)
		(learning-episode-from new-learning-episode))
	  (push (gethash (generic-control-rule-name cr1) *cr-to-le-hash*)
		(learning-episode-from new-learning-episode))
	  t)
        (if (not (find-another-combination learning-episode node))
	    (refine-parents learning-episode node)))))

(defun find-another-combination (learning-episode node)
  (declare (ignore learning-episode node))
  nil)

;; It assumes the rule of the le is wrong.
;; It backs up to the previous rules. Tries to see if any of the preceding
;; rules don't cover the negative examples. If so, they are undeleted.
(defun refine-parents (le node)
  (declare (type learning-episode le)
	   (type decision-node node))
  (let ((recoverp nil))
    (dolist (from-le (learning-episode-from le))
      (setq recoverp (or (and from-le (recover-rule from-le node le))
			 recoverp)))
    (really-delete-rule le (learning-episode-rule le) node)
    recoverp))

;;; This is pretty difficult, but it is the only way I know now of
;;; doing it. The result is the union of two sets of negative examples
(defun negatives-union (negatives1 negatives2)
  (declare (list negatives1 negatives2))
  (let ((result negatives1))
    (do* ((set2 negatives2 (cdr set2))
	  (negative2 (car set2) (car set2)))
	 ((null set2) result)
      (do* ((set1 result (cdr set1))
	    (negative1 (car set1) (car set1))
	    (included-1-in-2-p nil)
	    (included-2-in-1-p nil))
	   ((or (null set1)
		(and included-1-in-2-p included-2-in-1-p))
	    (when (and (not included-1-in-2-p)
		       included-2-in-1-p)
	      (setq result (remove (car included-2-in-1-p) result :test #'equal))
	      (push (cdr included-2-in-1-p) result)))
	(if (equal-control-rule-p (car negative1) (car negative2) (cdr negative1) (cdr negative2) 'subset)
	    (setq included-1-in-2-p t))
	(if (equal-control-rule-p (car negative2) (car negative1) (cdr negative2) (cdr negative1) 'subset)
	    (setq included-2-in-1-p (cons negative1 negative2)))))))

;;; The assumption is that child-le is wrong. 
(defun recover-rule (le node child-le)
  (declare (type learning-episode le child-le)
	   (type decision-node node))
  (if (eq (learning-episode-type le) 'deduced)
      (recover child-le node le)
      (let ((rule (learning-episode-rule le)))
	;;      (setf (learning-episode-other-bindings le) nil)
	(if (includes-negative-examples-p (get-real-preconds rule)
					  (get-real-effects rule)
					  (decision-node-negatives node))
	    (refine-rule le node)
	    (try-pool-or-create-pool child-le node le t 'intersection)))))

;;; It backtracks to the point to adding preconds to the rules.
(defun recover (le node parent-le)
  (declare (type learning-episode le parent-le)
	   (type decision-node node))
  (if parent-le
      (let ((parent-rule (learning-episode-rule parent-le)))
	(if (includes-negative-examples-p (get-real-preconds parent-rule) (get-real-effects parent-rule) (decision-node-negatives node))
	    (let ((rule (learning-episode-rule le)))
	      ;; there is no need no know if it is a deduced one since
	      ;; it has a parent-le
	      (if (not (learning-episode-deletedp le))
		  (delete-cr rule node le 'forever))
	      ;; before: (refine-parents parent-le node)
	      (try-pool-or-create-pool parent-le node nil nil (if (eq (learning-episode-type parent-le) 'deduced)
								  'deduction
								  'intersection)))
	    (try-pool-or-create-pool le node parent-le t 'intersection)))
      (try-pool-or-create-pool le node nil nil 'deduction)))

;; The input invariance is that parent-le, if different than nil, does
;; not cover the negative examples.
;;
;; Uses the pool of preconds, if there is one, or creates it, if there
;; is not. From that pool, it will pull one at a time to see if added
;; to the preconds of the more general (le), does not cover the
;; negative examples. The pool will usually be the difference between
;; the preconds of the control rule and the preconds of the previous
;; learning episode (or the whole set of preconds if the control rule
;; is deduced). In the case of deduced rules, it first tries to add
;; the preferred-preconds, that are the ones referring to the goal
;; regression of the some-candidate-goals. If this does not work, then
;; it begins adding from the whole set of preconds.
;;
;; Binding-used is only used the first time it is computed.
;; Op-type is the type of the inductive operator to recover from. For
;; now, there are only three types: deduction, intersection and
;; type-hierarchy.
;;
;; We could be better by passing along the preferred-preconds from
;; deduced to induced rules
(defun try-pool-or-create-pool (le node parent-le create-pool-p op-type)
  (declare (type learning-episode le parent-le)
	   (type decision-node node)
	   (atom create-pool-p op-type))
  (let* ((poolp (if (eq op-type 'deduction)
		    (learning-episode-preferred-preconds le)
		    (learning-episode-preconds-pool le)))
	 (rule (learning-episode-rule le))
	 (old-name (generic-control-rule-name rule))
	 (parent-rule (if parent-le (learning-episode-rule parent-le)))
	 (old-name-1 (if parent-rule (generic-control-rule-name parent-rule)))
	 (preconds-parent (if parent-rule (get-real-preconds parent-rule)))
	 (type-hierarchy-p (eq op-type 'type-hierarchy))
	 (deductionp (eq op-type 'deduction))
	 (binding-used (learning-episode-binding-used le))
	 (binding (some #'(lambda (rule-binding)
			    (if (and (car rule-binding)
				     (eq (generic-control-rule-name (car rule-binding)) old-name-1))
				(cdr rule-binding)))
			binding-used))
	 (inverse-binding (if type-hierarchy-p
			      (reverse (reverse-binding binding))))
	 (second-parent (if (and parent-le type-hierarchy-p)
			    (some #'(lambda (binding-rule)
				      (if (not (eq (car binding-rule) parent-rule))
					  (car binding-rule)))
				  binding-used)))
	 (preconds (if type-hierarchy-p
		       (specialize-hierarchy (sublis inverse-binding (get-real-preconds rule))
					     preconds-parent
					     (if second-parent
						 (get-real-preconds second-parent)))
		       (get-real-preconds rule)))
	 (pool (if type-hierarchy-p
		   (different-preconds preconds-parent preconds nil)
		   (or poolp
		       (and create-pool-p
			    (eq op-type 'intersection)
			    (different-preconds preconds-parent	preconds binding)))))
	 (effects (if type-hierarchy-p
		      (sublis inverse-binding (get-real-effects rule))
		      (get-real-effects rule)))
	 (first-prior-goals (if (and (eq op-type 'intersection) parent-le)
				(find-relevant-prior-goals (learning-episode-prior-goal le) (learning-episode-prior-goal parent-le))))
	 (new-preconds
	  (if type-hierarchy-p
	      (if (includes-negative-examples-p preconds effects (decision-node-negatives node))
		  (if pool
		      (find-discriminant-preconds preconds pool first-prior-goals effects node nil))
		  preconds)
	      (if pool
		  (find-discriminant-preconds preconds pool first-prior-goals effects node deductionp))))
	 (new-binding
	  (and new-preconds
	       (setf *rule-id* (string-trim "#:G" (gensym)))
	       (create-new-binding
		(if type-hierarchy-p
		    inverse-binding
		    (if parent-le
			binding
			(mapcar #'(lambda (var)
				    (cons var var))
				(get-all-vars-from-types new-preconds))))))))
    (if *stop-repairing-p* (break))
    (cond (new-preconds
	   (let ((new-learning-step
		  (if parent-le
		      (1+ (max (learning-episode-learning-steps le)
			       (learning-episode-learning-steps
				parent-le)))
		      (1+ (learning-episode-learning-steps le)))))
	     (cond ((>= new-learning-step *max-learning-steps*)
		    (format t "Max learning steps reached for ~a~%" (generic-control-rule-name rule))
		    (really-delete-rule le rule node)
		    t)
		   (t 
		    (setq new-preconds
			  (sort-preconds (sublis new-binding new-preconds)))
		    (setq effects (sublis new-binding effects))
		    (let* ((name (rededuced-name new-preconds effects (not parent-le)))
			   (new-cr (generic-make-control-rule name new-preconds effects))
			   (prior-goals (sublis new-binding (learning-episode-prior-goal le)))
			   (all-prior-goals (mapcar #'cadr prior-goals))
			   (new-le (make-learning-episode
				    :name (get-new-le-name)
				    :type (if (not parent-le) 'deduced 'refined)
				    :rule new-cr
				    :problem *problem-name*
				    :from (if parent-le (list parent-le))
				    :binding-used
				    (if parent-rule
					(pushnew (cons parent-rule new-binding)
						 binding-used :test #'equal))
				    :prior-goal
				    (create-new-prior-goals prior-goals new-preconds)
				    :learning-steps new-learning-step
				    :preferred-preconds nil
				    :preconds-pool
				    (if deductionp
					(sublis new-binding
						(learning-episode-preconds-pool le))
					(different-preconds (sublis new-binding pool)
							    new-preconds nil))
				    :matching-time
				    (if parent-le
					(max (learning-episode-matching-time le)
					     (learning-episode-matching-time
					      parent-le))
					(learning-episode-matching-time le))
				    :matchings
				    (if parent-le
					(max (learning-episode-matchings le)
					     (learning-episode-matchings parent-le))
					(learning-episode-matchings le))
				    :num-nodes
				    (if parent-le
					(max (learning-episode-num-nodes le)
					     (learning-episode-num-nodes parent-le))
					(learning-episode-num-nodes le))
				    :firings
				    (if parent-le
					(max (learning-episode-firings le)
					     (learning-episode-firings parent-le))
					(learning-episode-firings le)))))
		      (push new-le *le-list*)
		      (incf *number-refined-rules*)
	     
		      ;; So that we give more priority to things that worked out
	     
		      (update-frequencies (add-prior-goals all-prior-goals new-preconds)
					  node t)
		      (setf (gethash name *cr-to-le-hash*) new-le)
		      (format t "~%Recovering from ~(~a~) by creating pool from:~% ~a"
			      op-type old-name)
		      (if old-name-1
			  (format t " and~% ~a~%" old-name-1)
			  (format t " and~% the whole set of preconds~%"))
		      (really-delete-rule le rule node)
		      (add-rule new-cr nil))))))
	  (type-hierarchy-p
	   (format t "~2%This cannot happen!!!~%")
	   ;; (read)
	   (really-delete-rule le rule node)
	   (undelete-cr parent-le)
	   t)
	  (deductionp
	   ;; if it did not work to add all preconds from the
	   ;; preferred set, they are pushed into the pool, and the
	   ;; slot gets deleted
	   (let ((preferred-types nil))
	     (dolist (precond (learning-episode-preferred-preconds le))
	       (if (eq (car precond) 'true-in-state)
		   (push precond (learning-episode-preconds-pool le))
		   (push precond preferred-types)))
	     (setf (learning-episode-preferred-preconds le) nil)
	     (setf (learning-episode-preconds-pool le)
		   (append (learning-episode-preconds-pool le)
			   preferred-types)))
	   (try-pool-or-create-pool le node parent-le create-pool-p
				    'intersection))
	  (t
	   (cond ((or poolp (not create-pool-p))
		  (if (not (learning-episode-deletedp le))
		      (delete-cr rule node le
				 (if (eq (learning-episode-type le) 'deduced)
				     t
				     'forever)))
		  (if create-pool-p
		      (refine-parents parent-le node)))
		 (parent-le (try-pool-or-create-pool le node parent-le
						     create-pool-p
						     'type-hierarchy))
		 (t nil))))))

(defun really-delete-rule (le rule node)
  (if (not (learning-episode-deletedp le))
      (delete-cr rule node le
		 (if (eq (learning-episode-type le) 'deduced)
		     t
		     'forever))))
	  
;; Changes the type-of-object declarations of the variables to the
;; ones without induction. Second-parent is the other parent of the
;; induction. Some variables come from that parent, so we have to
;; access their types from there
(defun specialize-hierarchy (preconds preconds-parent second-parent-preconds)
  (declare (list preconds preconds-parent second-parent-preconds))
  (let ((new-preconds nil))
    (dolist (cond preconds (nreverse new-preconds))
      (if (eq (car cond) 'type-of-object)
	  (push (or (find-type-of-precond (cadr cond) preconds-parent nil)
		    (find-type-of-precond (cadr cond) second-parent-preconds
					  t))
		new-preconds)
          (push cond new-preconds)))))

;; It returns the list of cr-prior-goal-type-def lists that were also
;; in the parent
;; cr-prior-goals is a list of the form:
;;  ((rule prior-goal type-of-object) ...)
(defun find-relevant-prior-goals (cr-prior-goals parent-prior-goals)
  (declare (list cr-prior-goals parent-prior-goals))
  (let ((prior-goals-types nil))
    (dolist (cr-prior-goal cr-prior-goals prior-goals-types)
      (if (member (car cr-prior-goal) parent-prior-goals
		  :key #'car :test #'eq)
	  (pushnew (cdr cr-prior-goal) prior-goals-types
		   :test #'(lambda (pg1 pg2)
			     (equal (get-pos-assertion pg1)
				    (get-pos-assertion pg2))))))))


;;; New version in which it computes the best predicate to add to the
;;; preconds based on the heuristic of most times appearing on the
;;; positive examples
;;;
;;; Returns the new set of preconds after adding the less number of
;;; preconds needed to not contain a negative example. Preconds
;;; are the ones of the rule, while pool is the ones we can add
;;;
;;; If use-all-preconds-p is t, it will add all preconds from new-pool
;;; (preferred-preconds) to the preconds.
;;;
(defun find-discriminant-preconds (preconds old-pool prior-goals-types effects node use-all-preconds-p)
  (declare (type decision-node node)
	   (list preconds old-pool prior-goals-types effects))
  (let* ((negatives (decision-node-negatives node))
	 (frequency-table-positive
	  (decision-node-frequency-table-positive node))
	 (frequency-table-prior-goal-positive
	  (decision-node-frequency-table-prior-goal-positive node))
	 (pool old-pool)
	 (new-pool nil)
	 (all-vars nil)
	 (type-defs nil)
	 (old-type-defs nil)
	 (before-preconds nil)
	 (after-preconds nil)
	 (after-preconds-assigned-p nil)
	 (state nil))
    (dolist (prior-goal-type prior-goals-types)
      (push (car prior-goal-type) pool)
      (setq type-defs (append type-defs (cadr prior-goal-type))))
    (dolist (cond pool)
      (case (car cond)
	(true-in-state
	 (push (cons cond
		     (or (gethash (car (get-pos-assertion (cadr cond)))
				  frequency-table-positive)
			 0))
	       new-pool))
	(prior-goal
	 (dolist (goal (cadr cond))
	   (push (cons (list 'prior-goal (list goal))
		       (or (gethash (car (get-pos-assertion goal))
				    frequency-table-prior-goal-positive)
			   0))
		 new-pool)))
	(type-of-object (push cond type-defs))
	(t nil)))
    (do ((conds preconds (cdr conds)))
	((null conds))
      (case (caar conds)
	(true-in-state (push (car conds) state))
	(prior-goal
	 (setq new-pool (remove-equal-goals new-pool conds))
	 (if (not after-preconds-assigned-p)
	     (push (car conds) before-preconds)))
	(some-candidate-goals
	 (when (not after-preconds-assigned-p)
	   (setq after-preconds conds)
	   (setq after-preconds-assigned-p t))
	 (setq new-pool (remove-equal-goals new-pool conds)))
	(type-of-object
	 (when (not after-preconds-assigned-p)
	   (setq after-preconds conds)
	   (setq after-preconds-assigned-p t))
	 (push (car conds) old-type-defs)
	 (push (cadar conds) all-vars))
	(t (if (not after-preconds-assigned-p)
	       (push (car conds) before-preconds)))))
    (setq before-preconds (nreverse before-preconds))
    (cond (new-pool
	   (setq new-pool
		 (sort new-pool
		       #'(lambda (cond1 cond2)
			   (more-significant-p cond1 cond2 all-vars))))
	   (if use-all-preconds-p
	       (try-preconds-set (mapcar #'car new-pool) before-preconds
				 state after-preconds effects negatives
				 type-defs old-type-defs)
	       (do ((i 1 (1+ i))
		    (new-preconds nil))
		   ((or (= i 3) new-preconds)
		    new-preconds)
		 (case i
		   (1 (setq new-preconds
			    (add-1-precond before-preconds state new-pool
					   after-preconds effects negatives
					   type-defs old-type-defs)))
		   (2 (setq new-preconds
			    (add-2-preconds before-preconds state new-pool
					    after-preconds effects negatives
					    type-defs old-type-defs)))
		   (t (format t "~2%I could not add only two things~%")
		      (read))))))
	  (t ;; the problem might be with the types
	   (add-type-precond before-preconds state after-preconds
			     effects negatives type-defs old-type-defs)))))

;; Deletes all prior goals that appear in the some-candidate-goals or
;; prior goals
(defun remove-equal-goals (new-pool conds)
  (declare (list new-pool conds))
  (let ((goals (cadar conds)))
    (if goals
	(remove-if #'(lambda (precond-ig)
		       (let ((precond (car precond-ig)))
			 (and (eq (car precond) 'prior-goal)
			      (member (caadr precond) goals :test #'equal))))
		    new-pool)
        new-pool)))

;; It gives preference to the ones that have more variables that are
;; already in the rule, then to the ones that have appeared more times
;; in the lhs of the learned rules, and then to the true-in-state
(defun more-significant-p (cond-frequency1 cond-frequency2 all-vars)
  (declare (list cond-frequency1 cond-frequency2 all-vars))
  (let* ((frequency-in-positives1 (cdr cond-frequency1))
	 (frequency-in-positives2 (cdr cond-frequency2))
	 (cond1 (car cond-frequency1))
	 (cond2 (car cond-frequency2))
	 (vars-in-cond1 nil)
	 (vars-in-cond2 nil))
    (cond ((eq (car cond1) (car cond2))
	   (cond ((eq (car cond1) 'true-in-state)
		  (setq vars-in-cond1 (cdadr cond1))
		  (setq vars-in-cond2 (cdadr cond2)))
		 (t (setq vars-in-cond1 (cdaadr cond1))
		    (setq vars-in-cond2 (cdaadr cond2))))
	   (let ((number-vars-precond1 (count-vars-in-preconds vars-in-cond1 all-vars))
		 (number-vars-precond2 (count-vars-in-preconds vars-in-cond2 all-vars)))
	     (cond ((> number-vars-precond1 number-vars-precond2))
		   ((= number-vars-precond1 number-vars-precond2)
		    (> frequency-in-positives1 frequency-in-positives2))
		   (t nil))))
	  ((eq (car cond1) 'true-in-state))
	  (t nil))))

(defun count-vars-in-preconds (vars vars-in-preconds)
  (declare (list vars vars-in-preconds))
  (count-if #'(lambda (var)
		(member var vars-in-preconds :test #'eq))
	    vars))



;;; It returns the preconds if they do not cover the negative examples
(defun not-covers-negatives (preconds effects negatives)
  (declare (list preconds effects negatives))
  (if (not (includes-negative-examples-p preconds effects negatives))
      preconds))


;; Computes the set difference of two sets of preconds. It has in mind
;; the binding list used in preconds2
;;; I have to fix this to allow computing a better set. F.i. in the
;;; case of some-candidate-goals, it should return the ones that are
;;; not already there. The same for any-prior-goal
(defun different-preconds (preconds1 preconds2 binding-used)
  (declare (list preconds1 preconds2 binding-used))
  (let ((difference nil))
    (dolist (precond1 preconds1 (nreverse difference))
      (let* ((real-precond (sublis binding-used precond1))
	     (new-precond (different-precond real-precond preconds2)))
	(if new-precond
	    (push new-precond difference))))))

(defun different-precond (precond1 preconds2)
  (declare (list precond1 preconds2))
  (let* ((meta-predicate1 (car precond1))
	 (new-precond
	  (some #'(lambda (precond2)
		    (case meta-predicate1
		      ((prior-goal some-candidate-goals)
		       (if (eq (car precond2) meta-predicate1)
			   (let ((different-goals
				  (set-difference (cadr precond1)
						  (cadr precond2)
						  :test #'equal)))
			     (if different-goals
				 (list meta-predicate1 different-goals)
			         (list nil)))))
		      ((true-in-state type-of-object)
		       (if (and (eq (car precond2) meta-predicate1)
				(equal (cdr precond2) (cdr precond1)))
			   (list nil)))
		      (t 
		       (if (eq (car precond2) meta-predicate1)
			   (if (equal (cdr precond2) (cdr precond1))
			       (list nil)
			       precond1)))))
		preconds2)))
    (if new-precond
	(if (car new-precond)
	    new-precond)
        precond1)))


;;; New version
;;;
;;; Returns the first preconds that do not cover the negative examples
;;; by adding one at a time state meta predicates.
;;;
(defun add-1-precond (before-preconds state new-pool after-preconds
				      effects negatives
				      type-defs old-type-defs)
  (declare (list before-preconds state new-pool after-preconds
		 effects negatives type-defs old-type-defs))
  (some #'(lambda (cond-entropy)
	    (try-preconds-set (list (car cond-entropy)) before-preconds
			      state after-preconds effects negatives
			      type-defs old-type-defs))
	new-pool))

;;
;; Returns a new set of preconds if they do not cover the negative
;; examples after adding the new-conds.
;;
(defun try-preconds-set (new-conds before-preconds state after-preconds
				   effects negatives type-defs old-type-defs)
  (declare (list new-conds before-preconds state after-preconds
		 effects negatives type-defs old-type-defs))
  (let ((new-set nil)
	(state-conds nil)
	(prior-goal-conds nil)
	(type-conds nil))
    (dolist (new-cond new-conds)
      (case (car new-cond)
	(true-in-state (push new-cond state-conds))
	(prior-goal (push new-cond prior-goal-conds))
	(type-of-object (push new-cond type-conds)
			(setq after-preconds
			      (change-type-same-var after-preconds
						    (cadr new-cond)
						    (caddr new-cond))))
	(t nil)))
    (setq new-set (append (if prior-goal-conds
			      (add-prior-goals prior-goal-conds
					       before-preconds)
			      before-preconds)
			  state-conds state after-preconds))
    (not-covers-negatives (append new-set
				  (add-type-of-new-vars
				   (append prior-goal-conds state-conds)
				   type-defs old-type-defs))
			  effects negatives)))

#|
    (setq new-preconds (not-covers-negatives new-set effects negatives))
    (if new-preconds
	(append new-preconds
		(add-type-of-new-vars (append prior-goal-conds state-conds)
				      type-defs old-type-defs)))
|#

;;
;; Removes the type-of-object meta-predicate for the var
;;
(defun change-type-same-var (preconds var type)
  (declare (atom var type)
	   (list preconds))
  (cond ((null preconds) nil)
	((and (eq (caar preconds) 'type-of-object)
	      (eq (cadar preconds) var))
	 (cons (list (caar preconds) (cadar preconds) type)
	       (cdr preconds)))
	(t (cons (car preconds)
		 (change-type-same-var (cdr preconds) var type)))))

;;;
;;; Returns the first composition of two preconds that does not cover
;;; the negative examples.
;;;
(defun add-2-preconds (before-preconds state new-pool after-preconds
				       effects negatives
				       type-defs old-type-defs)
  (declare (list before-preconds state new-pool after-preconds
		 effects negatives type-defs old-type-defs))
  (do* ((i 0 (1+ i))
	(length-pool (length new-pool))
	(precond1 (if (< i length-pool) (car (nth i new-pool)))
		  (if (< i length-pool) (car (nth i new-pool))))
	(new-preconds nil))
      ((or (= i length-pool)
	   new-preconds)
       new-preconds)
    (do* ((j 0 (1+ j))
	  (precond2 (if (and (/= i j) (< j length-pool))
			(car (nth j new-pool)))
		    (if (and (/= i j) (< j length-pool))
			(car (nth j new-pool)))))
	((or (= j length-pool)
	     new-preconds))
      (if precond2
	  (setq new-preconds
		(try-preconds-set (list precond1 precond2) before-preconds
				  state after-preconds effects negatives
				  type-defs old-type-defs))))))

;;;
;;; Returns the first preconds that do not cover the negative examples
;;; by adding one at a time the type-of-object meta-predicates.
;;;
(defun add-type-precond (before-preconds state after-preconds effects
					 negatives type-defs old-type-defs)
  (declare (list before-preconds state after-preconds
		 effects negatives type-defs old-type-defs))
  (some #'(lambda (cond)
	    (try-preconds-set (list cond) before-preconds
			      state after-preconds effects negatives
			      type-defs old-type-defs))
	type-defs))

;;
;; Returns the new type-defs for the variables of the new-conds set
;; that were not already in the rule
;;
(defun add-type-of-new-vars (new-conds type-defs old-type-defs)
  (declare (list new-conds type-defs old-type-defs))
  (let ((new-type-defs nil))
    (dolist (new-cond new-conds new-type-defs)
      (dolist (var (if (eq (car new-cond) 'true-in-state)
		       (cdadr new-cond)
		       (cdaadr new-cond)))
	(if (not (member var old-type-defs :test #'eq :key #'cadr))
	    (pushnew (car (member var type-defs :test #'eq :key #'cadr))
		     new-type-defs :test #'eq :key #'cadr))))))
  
;; Returns a new name for a refinement. If the more specific rule
;; where it comes from is deduced, the name will be rededuced-x. If
;; not, it will be reinduced-x
;;
(defun rededuced-name (preconds effects deducedp)
  (declare (atom deducedp)
	   (list preconds effects))
  (intern (format nil "~:@(re~a-~a~)"
		  (if deducedp 'deduced 'induced)
		  (name-cr effects (if (eq (cadr effects) 'bindings)
				       (cadar preconds))))))


;;;
;;; Propagates the other-bindings to the parents if the child is a
;;; refined le, and the parent is not a deduced le. Now it only
;;; propagates to the immediate parents.
;;;
(defun propagate-changes (learning-episode new-other-bindings)
  (declare (type learning-episode learning-episode)
	   (list new-other-bindings))
  (if (eq (learning-episode-type learning-episode) 'refined)
      (dolist (from-le (learning-episode-from learning-episode))
	(if (and from-le (not (eq (learning-episode-type from-le) 'deduced)))
	    (setf (learning-episode-other-bindings learning-episode)
		  new-other-bindings)))))

;;
;; It returns the list of other-bindings, plus the one that worked,
;; if any element of the list of other-bindings creates
;; a relevant intersection. It really returns:
;;  (rest-other-crs-bindings new-preconds new-effects new-binding)
;;
(defun some-other-binding-p (le node crs-binding)
  (declare (type learning-episode le)
	   (type decision-node node)
	   (list crs-binding))
  (if crs-binding
      (let* ((cr1 (car crs-binding))
	     (cr2 (cadr crs-binding))
	     (other-bindings (caddr crs-binding))
	     (some-binding-p (if other-bindings
				 (some-binding-p le node cr1 cr2
						 other-bindings))))
	(if some-binding-p
	    (cons (list cr1 cr2 (car some-binding-p))
		  (cdr some-binding-p))))))

;;
;; If any binding on the list other-bindings creates a
;; valid intersection of the preconds of rules 1 and 2, and does not
;; include any negative example, then it returns:
;;  (rest-bindings new-preconds new-effects new-bindings)
;;
(defun some-binding-p (le node rule1 rule2 other-bindings)
  (declare (type learning-episode le)
	   (type decision-node node)
	   (list other-bindings))
  (let ((new-rule (find-relevant-intersection (get-real-preconds rule1)
					      (get-real-preconds rule2)
					      rule1
					      (get-real-effects rule1)
					      (get-real-effects rule2)
					      (car other-bindings)
					      node nil)))
    (if new-rule
	(cons (cdr other-bindings) new-rule)
        (let ((rest-other-bindings (cdr other-bindings)))
	  (if rest-other-bindings
	      (some-binding-p le node rule1 rule2
			      rest-other-bindings))))))

;;;
;;; It returns t if the preconds-effects is a matching subset of any negative
;;; example
;;;
(defun includes-negative-examples-p (preconds effects neg-examples)
  (declare (list preconds effects neg-examples))
  (let ((prior-goal-p (if (eq *planner-for-learning* 'ipss) (has-prior-goal-p preconds))))
    (some #'(lambda (neg-example)
	      (equal-control-rule-p preconds
				    (if prior-goal-p
				        (car neg-example)
					(remove-prior-goal (car neg-example)))
				    effects (cdr neg-example) 'intersection))
	  neg-examples)))

;;; Returns t if the preconds have any prior-goal related meta predicate
;;; It is the efficient procedure for doing it
(defun has-prior-goal-p (preconds)
  (declare (list preconds))
  (cond ((null preconds) nil)
	((prior-goal-meta-pred-p (car preconds)) t)
	((member (caar preconds) '(true-in-state some-candidate-goals
						 type-of-object)
		 :test #'eq)
	 nil)
	(t (has-prior-goal-p (cdr preconds)))))

;;;
;;; Returns the preconds without any prior-goal related meta-predicate
;;;
(defun remove-prior-goal (preconds)
  (declare (list preconds))
  (cond ((null preconds) preconds)
	((prior-goal-meta-pred-p (car preconds))
	 (cdr preconds))
	(t (cons (car preconds) (remove-prior-goal (cdr preconds))))))

;;;
;;; Just used for fast typing while debugging
;;;
(defun fast-includes (rule-name)
  (declare (atom rule-name))
  (let ((rule (from-name-to-rule-1 rule-name)))
    (includes-negative-examples-p (get-real-preconds rule)
				  (get-real-effects rule)
				  (decision-node-negatives
				   (find-decision-tree-node rule)))))

;;;
;;; Undeletes a rule, replacing it into the appropriate structures
;;; Setting the variable *deleted-rule-p* is a trick so that it will
;;; save all rules and not only the new ones. If the number of rules
;;; is greater than the maximum number of rules allowed by the
;;; induction method, the flag *over-threshold-p* is set to t
;;;
(defun undelete-cr (le)
  (declare (type learning-episode le))
  (let* ((cr (learning-episode-rule le))
	 (node (find-decision-tree-node cr)))
    (setf *deleted-rule-p* t)
    (incf *number-undeleted-rules*)
    (push cr *cr-list*)
    (format t "Undeleting ~a~%" (generic-control-rule-name cr))
    (if (or (eq *probabilities-p* 'rules)
	    (eq *probabilities-p* 'both))
	(borra-regla-prob (generic-control-rule-name cr) nil nil))
    (setf (learning-episode-deletedp le) nil)
    (if (>= (length (push cr (decision-node-rules node)))
	    (decision-node-threshold node))
	(setf *over-threshold-p* t))
    t))

;;; High level function for undeleting a rule
(defun undelete-rule (rule)
  (if (and (eq *planner-for-learning* 'ipss) (not (p4::control-rule-p rule)))
      (setq rule (from-name-to-rule-1 rule)))
  (undelete-cr (gethash (generic-control-rule-name rule) *cr-to-le-hash*)))



;;; ******************************************************************
;;;       Auxiliar functions
;;; ******************************************************************

;;; Saves the rules if savep is t, and prints information on the
;;; output file if *printp* is t.
(defun save-induction (learning-time slow-induction-p savep)
  (declare (number learning-time)
	   (atom slow-induction-p savep))
  (if savep (save-rules))
  (if *printp*
      (with-open-file (stream *hamlet-output* :direction :output :if-exists :append :if-does-not-exist :create)
        (if slow-induction-p
	    (format stream "~%        (induced-~a ~d ~d ~d ~f)" *problem-name* *number-induced-rules* *number-refined-rules* *number-deleted-rules* learning-time)
	  (format stream "~%(setf *induction-data* '(induced-~a ~d ~d ~d ~f))" *problem-name* *number-induced-rules* *number-refined-rules* *number-deleted-rules* learning-time)))))

(defun save-refinement (time save-rules-p)
  (declare (number time)
	   (atom save-rules-p))
  (if save-rules-p (save-rules))
  (if *printp*
      (with-open-file (stream *hamlet-output* :direction :output :if-exists :append :if-does-not-exist :create)
        (format stream "~%        (refined-~a ~d)" *problem-name* time))))


(defun re-install-learning (&optional (domain 'logistics) (cr-file *cr-file*) (domain-name "domain.pddl"))
  (declare (atom domain))
  (format t "~%Recovering in the domain ~a~%" domain)
  (if (eq *planner-for-learning* 'ipss)
      (domain domain :path *hamlet-world-path*
	      :domain-filename *hamlet-domain-file-name*
	      :function :compile)
      (say-domain domain domain-name))
  (reset-domain-variables t domain)
  (setf *cr-file* cr-file)
  (if (probe-file (cr-file))
      (load (cr-file))
      (load (format nil "~arules-backup.~a" *domain-directory* *lisp-extension*)))
  (if (eq *planner-for-learning* 'ipss)
      (load-domain))
  (read-decision-tree)
  (read-learning-episodes domain)
  (when (eq *planner-for-learning* 'ipss)
    (domain domain :path *hamlet-world-path*
	    :domain-filename *hamlet-domain-file-name*)
    (load-domain)))

(defun save-decision-tree nil
  (with-open-file (stream (format nil "~adecision-tree.~a" *domain-directory* *lisp-extension*)
			  :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream ";;; Decision tree")
    (format stream "~2%(setf *decision-tree-leaves*")
    (let ((*print-pretty* t))
      (format stream "~%     '~a)" (get-all-decision-tree *decision-tree* nil)))))

(defun get-all-decision-tree (node parents)
  (declare (type decision-node node)
	   (list parents))
  (let ((new-parents (cons (decision-node-name node) parents)))
    (if (decision-node-children node)
	(mapcan #'(lambda (child)
		    (get-all-decision-tree child new-parents))
		(decision-node-children node))
        (let ((rules (decision-node-rules node)))
	  (if rules
	      (list (list (reverse new-parents)
			  (from-rules-to-names rules)
			  (decision-node-negatives node)
			  (decision-node-needs-language-change-p node)
			  (get-frequency-table node))))))))

(defun get-frequency-table (node)
  (declare (type decision-node node))
  (let* ((pos-table (decision-node-frequency-table-positive node))
	 (neg-table (decision-node-frequency-table-negative node))
	 (pos-prior-goal-table
	  (decision-node-frequency-table-prior-goal-positive node))
	 (neg-prior-goal-table
	  (decision-node-frequency-table-prior-goal-negative node))
	 (all-tables nil))
    (if (or pos-table neg-table pos-prior-goal-table neg-prior-goal-table)
	(dolist (predicate (get-all-goals) all-tables)
	  (push (list predicate
		      (gethash predicate pos-table)
		      (gethash predicate neg-table)
		      (gethash predicate pos-prior-goal-table)
		      (gethash predicate neg-prior-goal-table))
		all-tables)))))

(defun read-frequency-table (preds-list node)
  (declare (list preds-list)
	   (type decision-node node))
  (when preds-list
      (setf (decision-node-frequency-table-positive node)
	    (make-hash-table))
    (setf (decision-node-frequency-table-negative node)
	  (make-hash-table))
    (setf (decision-node-frequency-table-prior-goal-positive node)
	  (make-hash-table))
    (setf (decision-node-frequency-table-prior-goal-negative node)
	  (make-hash-table))
    (dolist (predicate-frequencies preds-list)
      (let ((predicate (car predicate-frequencies)))
	(setf (gethash predicate (decision-node-frequency-table-positive node))
	      (nth 1 predicate-frequencies))
	(setf (gethash predicate (decision-node-frequency-table-negative node))
	      (nth 2 predicate-frequencies))
	(setf (gethash predicate (decision-node-frequency-table-prior-goal-positive node))
	      (nth 3 predicate-frequencies))
	(setf (gethash predicate (decision-node-frequency-table-prior-goal-negative node))
	      (nth 4 predicate-frequencies))))))

(defun read-decision-tree nil
  (let ((decision-tree-file (format nil "~adecision-tree.~a" *domain-directory* *lisp-extension*)))
    (build-fixed-decision-tree)
    (cond ((probe-file decision-tree-file)
	   (format t "~2%Reading decision tree.....~%")
	   (load decision-tree-file)
	   (dolist (leaf *decision-tree-leaves*)
	     (let ((decision-leaf (find-decision-leaf (cdar leaf) *decision-tree*)))
	       (setf (decision-node-rules decision-leaf)
		     (remove-nil-from-rules (mapcar #'from-name-to-rule (second leaf))))
	       (setf (decision-node-negatives decision-leaf) (third leaf))
	       (setf (decision-node-needs-language-change-p decision-leaf) (fourth leaf))
	       (read-frequency-table (fifth leaf) decision-leaf))))
	  (t
 	   (format t "~2%Generating artificial decision tree.....~%")
	   (dolist (rule (all-control-rules))
	     (push rule (decision-node-rules (find-decision-tree-node rule))))))))

(defun find-decision-leaf (path-list node)
  (declare (type decision-node node)
	   (list path-list))
  (if path-list
      (find-decision-leaf (cdr path-list) (find-decision-child (car path-list) node))
      node))

(defun all-leafs (node)
  (declare (type decision-node node))
  (if (decision-node-children node)
      (mapcan #'(lambda (child) (all-leafs child))
	      (decision-node-children node))
      (list node)))


(defun from-rules-to-names (rules)
  (declare (list rules))
  (let ((names nil))
    (dolist (rule rules (reverse names))
      (if (or (listp rule) (p4::control-rule-p rule))
	  (push (generic-control-rule-name rule) names)))))

(defun pp-dn (node stream z)
  (declare (type decision-node node)
	   (stream stream)
	   (ignore z))
  (format stream "#<~a ~a" (decision-node-name node) (decision-node-children-type node))
  (if (decision-node-children node)
      (format stream " ~a" (mapcar #'(lambda (child) (decision-node-name child))
				   (decision-node-children node))))
  (let ((rules (decision-node-rules node)))
    (if rules (format stream " ~a" (from-rules-to-names rules))))
  (format stream " ~d-NEGS>" (length (decision-node-negatives node))))

(defun pp-dnt (&optional (tree *decision-tree*) (profundidad 1))
  (declare (type decision-node tree)
	   (integer profundidad))
  (format t (format nil "~~%~~~dt~~a" (* 2 profundidad)) tree)
    (dolist (child (decision-node-children tree))
      (pp-dnt child (1+ profundidad))))


;;;**************************************************
;;; Functions relative to learning episodes
;;;**************************************************

;; It saves the learning episodes in order so that it can create them
;; properly when it recovers with read-learning-episodes
(defun save-learning-episodes (&optional (file (format nil "~alearning-episodes.~a" *domain-directory* *lisp-extension*)))
  (when (probe-file (cr-file))
    (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format stream ";;; Learning episodes")
      (format stream "~2%(setf *learning-episodes*~%     '(")
      (if (eq *planner-for-learning* 'ipss)
	  (domain *domain-name* :path *hamlet-world-path* :domain-filename *hamlet-domain-file-name*)
	  (say-domain *domain-name* *hamlet-domain-file-name*))
      (load (cr-file))
      (if (eq *planner-for-learning* 'ipss)
	  (load-domain))
      (let ((le-array (make-array *le-index*))
	    (rules (all-control-rules)))
	(maphash #'(lambda (cr-name le)
		     (declare (ignore cr-name))
		     (setf (svref le-array (read-from-string (subseq (format nil "~d" (learning-episode-name le)) 3)))
			   le))
		 *cr-to-le-hash*)
	(dotimes (i *le-index*)
	  (let* ((le (svref le-array i))
		 (rule (if le (learning-episode-rule le))))
	    (if (and le rule)
		(let ((deletedp (learning-episode-deletedp le))
		      (other-binding (learning-episode-other-bindings le))
		      (binding-used (learning-episode-binding-used le)))
		  (cond ((eq deletedp 'forever) (format stream "nil~%"))
			(t (format stream
				   "(~a ~a ~a ~a~%          ~a~%          ~a"
				   (learning-episode-name le)
				   (learning-episode-type le)
				   (save-deleted-rule
				    rule
				    (or deletedp
					(not (member rule rules :test #'eq))))
				   (get-from-les-names le)
				   (if (and other-binding
					    (car other-binding)
					    (cadr other-binding)) ;; borrar
				       (list (generic-control-rule-name
					      (car other-binding))
					     (generic-control-rule-name
					      (cadr other-binding))
					     (caddr other-binding)))
				   deletedp)
			   (format stream "~%          ~a"
				   (learning-episode-preconds-pool le))
			   (format stream "~%          ~a"
				   (learning-episode-preferred-preconds le))
			   (format stream "~%          ~a"
				   (if (and binding-used
					    (caar binding-used)
					    (caadr binding-used)) ;; borrar
				       (list (cons (generic-control-rule-name
						    (caar binding-used))
						   (cdar binding-used))
					     (cons (generic-control-rule-name
						    (caadr binding-used))
						   (cdadr binding-used)))))
			   (format stream "~%          ~a"
				   (save-prior-goal le))
			   (format stream "~%          ~a ~a ~d ~d ~d ~d ~d)~%       "
				   (learning-episode-opposite-tried-p le)
				   (learning-episode-problem le)
				   (learning-episode-learning-steps le)
				   (learning-episode-matching-time le)
				   (learning-episode-matchings le)
				   (learning-episode-num-nodes le)
				   (learning-episode-firings le)))))
		(format stream "nil~%"))))
	(format stream "))~%")))))

;;
;; It recovers the learning episodes from a file. It needs the rules
;; to be already loaded and load-domain evaluated
;; :negatives (nth 7 le)

(defun read-learning-episodes (domain)
  (let ((le-file (format nil "~alearning-episodes.~a" *domain-directory* *lisp-extension*)))
    (cond ((probe-file le-file)
	   (format t "~2%Reading learning episodes.....~%")
	   (clrhash *cr-to-le-hash*)
	   (setf *le-index* 0)
	   (setf *le-list* nil)
	   (load le-file)
	   (setf *rules* (all-control-rules))
	   (setf *cr-list* (copy-list *rules*))
	   (dolist (le *learning-episodes*)
	     (let* ((deletedp (nth 5 le))
		    (cr (nth 2 le))
		    (create-deleted-rule-p (or deletedp (listp cr))))
	       (if (and le create-deleted-rule-p)
		   (let* ((cr-name (if (listp cr) (car cr) cr))
			  (other-binding (nth 4 le))
			  (binding-used (nth 8 le))
			  (new-le
			   (make-learning-episode
			    :name (nth 0 le)
			    :type (nth 1 le)
			    :rule (if create-deleted-rule-p
				      (let ((rule (create-deleted-rule cr)))
					(push rule *cr-list*)
					rule)
				      (from-name-to-rule cr-name *cr-list*))
			    :problem (nth 11 le)
			    :from (from-names-to-pointers (nth 3 le))
			    :binding-used
			    (if binding-used
				(list (cons (from-name-to-rule (caar binding-used)
							       *cr-list*)
					    (cdar binding-used))
				      (cons (from-name-to-rule (caadr binding-used)
							       *cr-list*)
					    (cdadr binding-used))))
			    :prior-goal (read-prior-goal (nth 9 le))
			    :other-bindings
			    (if other-binding
				(list (from-name-to-rule (car other-binding) *cr-list*)
				      (from-name-to-rule (cadr other-binding) *cr-list*)
				      (caddr other-binding)))
			    :preconds-pool (nth 6 le)
			    :preferred-preconds (nth 7 le)
			    :deletedp deletedp
			    :learning-steps (nth 12 le)
			    :matching-time (nth 13 le)
			    :matchings (nth 14 le)
			    :num-nodes (nth 15 le)
			    :firings (nth 16 le)
			    :opposite-tried-p (nth 10 le))))
		     (push new-le *le-list*)
		     (setf (gethash cr-name *cr-to-le-hash*) new-le)))
	       (incf *le-index*)))
	   (setf *cr-list* nil))
	  (t
	   (format t "~2%Creating artificial learning episodes.....~%")
	   (create-artificial-les domain 'deduced)))))

(defun save-prior-goal (le)
  (declare (type learning-episode le))
  (let ((output nil))
    (dolist (cr-prior-goal (learning-episode-prior-goal le) output)
      (let ((cr (car cr-prior-goal)))
	(if (and cr (eq *planner-for-learning* 'ipss) (p4::control-rule-p cr))
	    (push (cons (generic-control-rule-name cr)
			(cdr cr-prior-goal))
		  output))))))

(defun read-prior-goal (prior-goal)
  (declare (list prior-goal))
  (let ((input nil))
    (dolist (cr-prior-goal prior-goal input)
      (let ((name (from-name-to-rule (car cr-prior-goal) *cr-list*)))
	(if name
	    (push (cons name (cdr cr-prior-goal))
		  input))))))

(defun save-deleted-rule (rule &optional (deletedp 'unknown))
  (declare (atom deletedp))
  (let ((rule-name (generic-control-rule-name rule)))
    (if (eq deletedp 'unknown)
	(setq deletedp (learning-episode-deletedp (gethash rule-name *cr-to-le-hash*))))
    (if deletedp
	(list rule-name (get-real-preconds rule) (get-real-effects rule))
        rule-name)))

(defun get-from-les-names (episode)
  (declare (type learning-episode episode))
  (let ((les-names nil))
    (dolist (le (learning-episode-from episode) (nreverse les-names))
      (if le
	  (push (learning-episode-name le) les-names)))))



(defun create-deleted-rule (rule)
  (declare (list rule))
  (if (eq *planner-for-learning* 'ipss)
      (p4::make-control-rule :name (first rule)
			     :if (second rule)
			     :then (third rule))
      rule))

(defun from-name-to-rule (name &optional (rules (all-control-rules)))
  (declare (atom name)
	   (list rules))
  (find-if #'(lambda (cr)
	       (eq (generic-control-rule-name cr) name))
	   rules))

(defun from-name-to-rule-1 (name)
  (declare (atom name))
  (learning-episode-rule (gethash name *cr-to-le-hash*)))

(defun from-names-to-pointers (names-list)
  (declare (list names-list))
  (mapcar #'from-le-name-to-pointer names-list))

(defun from-le-name-to-pointer (le-name)
  (declare (atom le-name))
  (find-if #'(lambda (le)
	       (eq (learning-episode-name le) le-name))
	   *le-list*))


;;; **************************************************
;;;           Auxiliary functions
;;; **************************************************

(defun find-my-node (node name)
  (declare (atom name))
  (if (eq (generic-node-name node) name)
      node
      (some #'(lambda (child)
		(find-my-node child name))
	    (generic-node-children node))))

(defun pp-le (episode stream z)
  (declare (type learning-episode episode)
	   (stream stream)
	   (ignore z))
  (format stream "#<~a ~a ~a ~a>" (learning-episode-name episode)
	  (learning-episode-type episode)
	  (learning-episode-rule episode)
	  (get-from-les-names episode)))


(defun pp-le1 (le-name-or-le)
;;  (declare (atom le-name-or-le))
  (let ((episode (if (learning-episode-p le-name-or-le)
		     le-name-or-le
		     (from-le-name-to-pointer le-name-or-le))))
    (format t "~2%Name: ~a  Type: ~a  Deletedp: ~a Problem: ~a~%Rule: ~a"
	    (learning-episode-name episode)
	    (learning-episode-type episode)
	    (learning-episode-deletedp episode)
	    (learning-episode-problem episode)
	    (learning-episode-rule episode))
    (format t "~%From les: ~a" (get-from-les-names episode))
    (format t "~%Preferred-Preconds: ~a~%Preconds-Pool: ~a~%Binding used: ~a"
	    (learning-episode-preferred-preconds episode)
	    (learning-episode-preconds-pool episode)
	    (learning-episode-binding-used episode))
    (format t "~%Other bindings: ~a~%Learning-steps: ~a  Matching-time: ~a"
	    (learning-episode-other-bindings episode)
	    (learning-episode-learning-steps episode)
	    (learning-episode-matching-time episode))
    (format t "  Matchings: ~a Number-nodes: ~a  Firings: ~a"
	    (learning-episode-matchings episode)
	    (learning-episode-num-nodes episode)
	    (learning-episode-firings episode))))

(defun pp-cr-les-hash nil
  (maphash #'(lambda (cr-name le)
	       (format t "~%~a~55t~a ~a ~a" cr-name
		       (learning-episode-name le)
		       (learning-episode-type le)
		       (learning-episode-deletedp le)))
	   *cr-to-le-hash*))


