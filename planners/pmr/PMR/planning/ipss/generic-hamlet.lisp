;; This is a generalization of Hamlet code to learn rules also from other planners
;;; This is the new version of Minnie for Prodigy 4.0. It is called
;;; HAMLET, for Heuristics Acquisition Method by Learning from sEarch Trees.
;;; There is always a doubt in our lifes in which we ask ourselfs:
;;;   is it right or wrong to do it =
;;;   to be or not to be

(in-package "COMMON-LISP-USER")
;; for the P4 calls
(unless (find-package "PRODIGY4") (make-package "PRODIGY4" :nicknames '("P4") :use '("COMMON-LISP-USER")))

;; It can be ipss or sapyhi for now
(defvar *planner-for-learning* 'ipss)
(defvar *last-example* 0)
;; Allowable meta-predicates in the LHS of Hamlet rules. They have to keep the
;; order given that preconds are sorted according to this list
(defvar *hamlet-meta-predicates* '(candidate-operator current-operator
				   applicable-op
				   target-goal current-goal prior-goal 
				   candidate-goal true-in-state some-candidate-goals
				   subset-candidate-goals type-of-object))

;;******************************************************************
;; Functions
;;******************************************************************

;; Main learning entry point
(defun learn (&key (rules-file (cr-file)) (rules nil) (say-solution *say-solution*) (better-heuristic-p nil) (only-from-plateaus nil) (negativesp t)
	      (remove-preconds-from-rule nil) (remove-static-preds *remove-static-preds*) (re-init-p t) (rules-format 'prodigy)
	      (eager-mode 'lazy-total) (previous-cost-achieved nil) (print-data-p t) (domain (hamlet-domain-name)))
  (init-learning re-init-p)
  (if (eq *planner-for-learning* 'sayphi)
      (reset-domain-variables re-init-p domain))
  (if (not (eq rules-format 'prodigy)) (setf *inductionp* nil))
  (let ((t-inicial (get-internal-run-time)))
    (cond ((eq *planner-for-learning* 'ipss)
	   (let* ((t-inicial (get-internal-run-time))
		  (whole-tree-expanded-p (whole-tree-expanded-p))
		  (result (learn-from-tree *real-t-tree* eager-mode whole-tree-expanded-p)))
	     (setf (node-num-nodes *real-t-tree*) (caddr result))
	     (setf *number-ops* (car result))
	     (setf *best-cost-achieved* (cadr result))
	     (if (or *store-state-p*
		     (and *learnp* *optimal-learning-p* whole-tree-expanded-p
			  (or (not previous-cost-achieved) (<= *best-cost-achieved* previous-cost-achieved))
			  (solutionp)))
		 (learn-only-optimal-path *real-t-tree* t))
	     (setf (prodigy-result-all-nodes) (caddr result))
	     (if *printp* (pp-rtt))))
	  (t (setq rules (generate-rules-from-solution :rules-file rules-file :rules rules :say-solution say-solution :better-heuristic-p better-heuristic-p
						       :only-from-plateaus only-from-plateaus :remove-preconds-from-rule remove-preconds-from-rule :negativesp negativesp
						       :remove-static-preds remove-static-preds :re-init-p re-init-p :rules-format rules-format))))
    (when *learnp*
      (if *inductionp* (learn-from-induction t *cr-file* *cr-list* nil nil t))
      (if (eq rules-format 'prodigy) (save-cr)))
    (something-changed-?)
    (if print-data-p (print-learning-data (elapsed-time t-inicial)))))

;; To be called after loading the domain of before calling it with a domain name
(defun reset-domain-variables (initp &optional (domain-name (hamlet-domain-name)))
  (if initp (setf *decision-tree* nil))
  (unless (eq *domain-name* domain-name)
    (setf *domain-name* domain-name)
    (setf *domain-directory* (if *standard-paths-p*
				 (format nil "~a~(~a~)/" *hamlet-world-path* domain-name)
				 *hamlet-world-path*))
    (setf *examples-file* (format nil "~aexample-file.d" *domain-directory*))
    (setf *utility-output* (format nil "~autility.~a" *domain-directory* *lisp-extension*))
    (setf *ff-output-directory* *ipss-temporal-dir*)
;;     (setf *ff-output-directory* (format nil "~a-~(~a~)/" *ipss-temporal-dir* *domain-name*))
    (setf *hamlet-results-output* (format nil "~ahamlet-results.~a" *domain-directory* *lisp-extension*))
    (setf *prodigy-results-output* (format nil "~aprodigy-results.~a" *domain-directory* *lisp-extension*))
    (setf *hamlet-output* (format nil "~ahamlet-output.~a" *domain-directory* *lisp-extension*))
    (if *reset-problem-path*
	(setf *hamlet-probset-path* (concatenate 'string *domain-directory* "probsets/")))))

;; Initializes learning vars
(defun init-learning (&optional re-init-p)
  (if *active-learning-mode* (setf *active-learning-hash* (make-hash-table :test #'equal)))
  (when (and re-init-p (eq *planner-for-learning* 'sayphi))
    (setf *last-example* 0)
    (setf *rules* nil)
    (setf *decision-tree* nil))
  (when *learnp*
    (setf *deleted-rule-p* nil)
    (setf *initial-cr* *rules*)
    (setf *cr-list* nil))
;; The real top level node is the 2nd.
  (if (eq *planner-for-learning* 'ipss) (setf *real-t-tree* (p4::find-node 2)))
  (setf *my-current-node* nil)
;;  (setf *number-nodes* 0)
  (setf *number-deduced-rules* 0)
  (setf *number-induced-rules* 0)
  (setf *number-refined-rules* 0)
  (setf *number-deleted-rules* 0)
  (setf *number-undeleted-rules* 0))

(defun something-changed-? nil
  (setf *something-changed-p*
	(or *something-changed-p*
	    (> (+ *number-refined-rules* *number-induced-rules*) 0)
	    (/= (+ *number-deduced-rules*
		   *number-induced-rules*
		   *number-refined-rules*
		   (- *number-deleted-rules*)
		   *number-undeleted-rules*)
		0))))

;; It returns the true-in-state conds ordered by the number of
;; predicates that they share with previous conds. Next step: order
;; them according to the entropy
(defun sort-preconds (preconds)
  (do* ((conds preconds (cdr conds))
	(cond (car conds) (car conds))
	(vars-in-precond (vars-in-precond cond) (vars-in-precond cond))
	(new-preconds nil)
	(vars nil))
       ((or (null conds)
	    (eq (car cond) 'some-candidate-goals))
	(nconc (reverse new-preconds) conds))
    (case (car cond)
      (true-in-state
       (do ((rest-conds conds (cdr rest-conds))
	    (state-conds nil))
	   ((or (null (cdr rest-conds))
		(eq (caadr rest-conds) 'some-candidate-goals)
		(eq (caadr rest-conds) 'type-of-object))
	    (setq conds rest-conds)
	    (do ((rest-state-conds (if (eq (caar rest-conds) 'true-in-state)
				       (cons (car rest-conds) state-conds)
				       state-conds)))
		((null (cdr rest-state-conds))
		 (if rest-state-conds
		     (push (car rest-state-conds) new-preconds)))
	      (let ((best-state
		     (car (sort (copy-list rest-state-conds) #'>
				:key #'(lambda (scond) (count-vars-in-preconds (cdadr scond) vars))))))
		(push best-state new-preconds)
		(setq rest-state-conds (remove best-state rest-state-conds :test #'equal))
		(dolist (var (cdadr best-state))
		  (pushnew var vars :test #'eq)))))
	 (push (car rest-conds) state-conds)))
      ((current-operator candidate-operator)
       (push cond new-preconds))
      (t ;; prior-goal some-candidate-goals target-goal applicable-op current-goal candidate-goal
       (dolist (var vars-in-precond)
	 (pushnew var vars :test #'eq))
       (push cond new-preconds)))))

;; It updates the frecuencies of literals, predicates, instances and their
;; types that belonged to the regressed states
(defun update-active-learning-info (regressed-state)
  (let ((type nil))
    (dolist (literal regressed-state)
      (update-element literal 'literal)
      (update-element (car literal) 'predicate)
      (dolist (arg (cdr literal))
	(setq type (get-object-type arg))
	(update-element type 'type)
	(update-element arg type)))))

(defun update-element (element type)
  (declare (special *active-learning-hash*))
  (if (gethash (cons type element) *active-learning-hash*)
      (incf (gethash (cons type element) *active-learning-hash*))
    (setf (gethash (cons type element) *active-learning-hash*) 1))
  ; so that I also have a count per type for the roulette
  (if (gethash type *active-learning-hash*)
      (incf (gethash type *active-learning-hash*))
    (setf (gethash type *active-learning-hash*) 1)))

;; Returns a list of true-in-state meta-predicates, one for each predicate of the state
(defun instantiate-cr-state (state)
  (declare (list state))
  (mapcar #'(lambda (literal)
	      (cons 'true-in-state (list literal)))
	  state))

;; Returns all constants in a given meta-predicate or then part
(defun find-ctes (conde)
  (declare (list conde))
  (let ((ctes nil))
    (case (car conde)
      ((prior-goal some-candidate-goals)
       (dolist (pred-op (cadr conde))
	 (dolist (cte (get-pos-assertion pred-op))
	   (pushnew cte ctes :test #'eq))))
      ((target-goal true-in-state applicable-op current-goal candidate-goal)
       (dolist (cte (get-pos-assertion (cadr conde)))
	 (pushnew cte ctes :test #'eq)))
      ((select reject)
       (case (car conde)
	 (bindings (dolist (cte (mapcar #'cdr (cadr conde)))
		     (pushnew cte ctes :test #'eq)))
	 (goals (dolist (cte (cadr conde))
		  (pushnew cte ctes :test #'eq)))
	 (t nil)))
      (t nil))
    ctes))


;; Returns a list with the vars that appear in the precond
(defun vars-in-precond (precond)
  (declare (list precond))
  (cond ((null precond) nil)
	((listp (car precond))
	 (nconc (vars-in-precond (car precond))
		(vars-in-precond (cdr precond))))
	((p4::is-variable-p (car precond))
	 (cons (car precond)
	       (vars-in-precond (cdr precond))))
	(t (vars-in-precond (cdr precond)))))

;; Returns the type-of-object meta-predicate for the var
(defun find-type-of-precond (var preconds breakp)
  (declare (list preconds))
  (let ((foundp (find-if #'(lambda (precond)
			     (and (eq (car precond) 'type-of-object)
				  (eq (cadr precond) var)))
			 preconds :from-end t)))
    (or foundp
	(and breakp
	     (list 'type-of-object var (get-type-from-name var))))))

(defun get-type-from-name (var)
  (let ((name (subseq (string-right-trim "0123456789->"
					 (format nil "~a" var))
		      1)))
    (intern (format nil "~:@(<~a>~)"
		    (if (and (eq (elt name 0) #\I)
			     (eq (elt name 1) #\-))
			(subseq name 2)
			name)))))

;; Returns a list of the vars that appear as defined in the
;; type-of-object meta-predicate
(defun get-all-vars-from-types (preconds)
  (declare (list preconds))
  (let ((all-vars nil))
    (dolist (precond preconds all-vars)
      (if (eq (car precond) 'type-of-object)
	  (push (cadr precond) all-vars)))))

(defun generic-make-control-rule (name preconds effects)
  (if (eq *planner-for-learning* 'ipss)
      (make-control-rule :name new-name
			 :if (cons 'and new-preconds)
			 :then (cons 'then new-effects))
      `(control-rule ,name (if (and ,@preconds)) (then ,@effects))))

;;**********************************************************************
;;     Functions that check for subsumed rules
;;**********************************************************************

;; Calls the function deletablep to see if the new cr should be added,
;; removed, or there is any other rule that should be deleted.
(defun include-rule (new-cr control-rules)
  (declare (list control-rules))
  (let* ((name1 (generic-control-rule-name new-cr))
	 (deletablep (or (some #'(lambda (rule)
				   (let ((delp (deletablep new-cr rule)))
				     (if (not (eq delp 'include))
					 (cons delp rule))))
			       control-rules)
			 '(include)))
	 (rule (cdr deletablep))
	 (name2 (if rule (generic-control-rule-name rule))))
    (case (car deletablep)
      (include (format t "Adding new control rule: ~a~%" name1)
	       (push new-cr *cr-list*)
	       'include)
      (delete-1 (incf *number-deleted-rules*)
		(format t "~%Found subsumed rule ~a~%Deleting ~a~%" name2 name1)
		'delete-1)
      (delete-2	(format t "~%Found subsumed rule ~a" name1)
		(format t "~%Adding new control rule: ~a~%" name1)
		(incf *number-deduced-rules*)
		(push new-cr *cr-list*)
		(let ((le (gethash name2 *cr-to-le-hash*)))
		  (delete-cr rule nil le (if (eq (learning-episode-type le) 'deduced) t 'forever)))
		'delete-2)
      (t nil))))

;; Returns include if new-cr1 should be added, delete-1 if new-cr should
;; be deleted (rule2 included in rule1), and delete-2 if rule2 should
;; be deleted (rule1 included in rule2)
(defun deletablep (new-cr rule)
  (let* ((preconds1 (get-real-preconds new-cr))
	 (effects1 (get-real-effects new-cr))
	 (preconds2 (get-real-preconds rule))
	 (effects2 (get-real-effects rule)))
    (if (and (eq (car effects1) (car effects2))
	     (eq (cadr effects1) (cadr effects2))
	     (case (cadr effects1)
	       (operators (eq (caddr effects1) (caddr effects2)))
	       (goals (eq (caaddr effects1) (caaddr effects2)))
	       (t t)))
	(check-equality preconds1 preconds2 effects1 effects2)
        'include)))


;; It checks whether rule2 is included in rule1. If so, it removes rule1,
;; since it is overspecific. It returns t, so that the calling
;; function should remove it.
;; Otherwise, it checks whether rule1 is included in rule2. If so,
;; rule2 is deleted, since it is overspecific
(defun check-equality (preconds1 preconds2 effects1 effects2)
  (declare (list preconds1 preconds2 effects1 effects2))
  (cond ((<= (length preconds2) (length preconds1))
	 (if (equal-control-rule-p preconds2 preconds1 effects2 effects1 'subset)
	     'delete-1
	     'include))
	((equal-control-rule-p preconds1 preconds2 effects1 effects2 'subset)
	 'delete-2)
	(t 'include)))

;;; New version of equal-control-rule-p (much better): incremental matching
;;; It returns all the bindings that are coherent with the effects if
;;; includedp is nil. If not, it returns the first one
(defun equal-control-rule-p (preconds1 preconds2 effects1 effects2 includedp)
  (declare (list preconds1 preconds2 effects1 effects2))
  (if includedp
      (new-get-bindings-of-preconds (sort-preconds-for-matching preconds1)
				    (sort-preconds-for-matching preconds2)
				    effects1 effects2
				    (list nil) includedp)
    (let* ((binding-effects (equal-effects-p effects1 effects2 nil))
	   (bindings (if binding-effects
			 (if (or (eq t binding-effects)
				 (not (car binding-effects)))
			     (list nil)
			   (list binding-effects)))))
      (if bindings
	  (new-get-bindings-of-preconds (sort-preconds-for-matching preconds1)
					(sort-preconds-for-matching preconds2)
					effects1 effects2
					bindings includedp)))))

;;; Raquel, april 2006: i add the next "if" to control errors when precond1 or precond2 are nil
(defun sort-preconds-for-matching (preconds)
  (sort preconds #'(lambda (precond1 precond2)
		     (if (and precond1 precond2)
			 (<= (position (car precond1) *hamlet-meta-predicates*)
			     (position (car precond2) *hamlet-meta-predicates*))
		         t))))

;;; Returns the new list of bindings. There are three cases: 1. binding is
;;; more general than a certain x in all-bindings it is not added 2. binding
;;; is more specific than a set of x in all-bindings the x in this set are
;;; deleted from all-bindings and binding is added 3. Otherwise, it is added
;;;
;;; The trick is that if it finds that there is one that is more general
;;; than binding, then it should not keep testing whether binding is more
;;; general than any other, since it can not happen that there is a
;;; binding more general/specific than another one in all-bindings
(defun introduce-new-binding (binding all-bindings)
  (declare (list binding all-bindings))
  (if (car binding)
      (do* ((bindings all-bindings (cdr bindings))
	    (first-binding (car bindings) (car bindings))
	    (result nil)
	    (more-specific-p nil)
	    (more-general-p nil))
	   ((or (null bindings)
		more-general-p)
	    (if more-general-p
		all-bindings
		(cons binding result)))
	(if (and (not more-specific-p)
		 (subsetp binding first-binding :test #'eq-binding-p))
	    (setq more-general-p t)
	    (unless (subsetp first-binding binding :test #'eq-binding-p)
	      (setq more-specific-p t)
	      (push first-binding result))))
      all-bindings))

;;; It returns t if bind-pair1 is eq to bind-pair2
(defun eq-binding-p (bind-pair1 bind-pair2)
  (and (eq (car bind-pair1) (car bind-pair2))
       (eq (cdr bind-pair1) (cdr bind-pair2))))

;;; It returns t if binding1 and binding2 are equal binding lists
(defun equal-bindings-p (binding1 binding2)
  (and (= (length binding1) (length binding2))
       (subsetp binding1 binding2 :test #'eq-binding-p)))

;;       (equal-set-p binding1 binding2 :test eq-binding-p))

;; Returns a list of bindings:
;; (((<object2> . <object1>) .. (<object3> . <object2>)) ...), if
;; preconds1 is included in preconds2, or nil if not matching preconds
;;; The test-inclusion-p parameter allows to control whether the
;;; preconds1 should be less than preconds2 (included) or not.
;;; It can be:
;;;  - subset: test in get-bindings-some-or-prior-goals whether goals2
;;;            is a subset of goals1
;;;  - intersection: the same but testing whether they intersect
;;;  - nil: to know the intersection of both
(defun new-get-bindings-of-preconds (preconds1 preconds2 effects1 effects2 bindings test-inclusion-p)
  (declare (list preconds1 preconds2 bindings))
  (cond ((null preconds1) (end-matching effects1 effects2 bindings test-inclusion-p))
	((null preconds2) nil)
	(t (let ((meta-predicate (caar preconds1)))
	     (case meta-predicate
	       (true-in-state
		(cond ((eq meta-predicate (caar preconds2))
		       (new-get-bindings-of-state preconds1 preconds2 effects1 effects2 bindings test-inclusion-p))
		      (test-inclusion-p nil)
		      (t (new-get-bindings-of-preconds (cdr preconds1) preconds2 effects1 effects2 bindings test-inclusion-p))))
	       (type-of-object
		(end-matching effects1 effects2
			      (test-bindings-of-types preconds1 preconds2 bindings (if test-inclusion-p #'more-general-type-p #'common-supertype))
			      test-inclusion-p))
	       ((candidate-operator current-operator)
		(if (equal (car preconds1) (car preconds2))
		    (new-get-bindings-of-preconds (cdr preconds1) (cdr preconds2) effects1 effects2 bindings test-inclusion-p)))
	       ((current-goal candidate-goal target-goal applicable-op)
		(if (eq meta-predicate (caar preconds2))
		    (new-get-bindings-of-some-goals preconds1 preconds2 effects1 effects2 bindings test-inclusion-p)))
	       (some-candidate-goals (new-get-bindings-some-or-prior-goals preconds1 preconds2 effects1 effects2 bindings test-inclusion-p nil))
	       (prior-goal (new-get-bindings-some-or-prior-goals preconds1 preconds2 effects1 effects2 bindings test-inclusion-p t))
	       (t nil))))))

;;; Changed 13-07-95. If test-inclusion-p=nil, then it has already
;;; computed the bindings of effects at the beginning of checking
;;; whether the two rules match.
(defun end-matching (effects1 effects2 bindings test-inclusion-p)
  (and bindings
       (if test-inclusion-p
	   (some #'(lambda (binding)
		     (let ((new-binding (last-binding effects1 effects2 binding)))
		       (if new-binding (list new-binding))))
		 bindings)
	   bindings)))


(defun last-binding (effects1 effects2 binding)
  (let ((binding-effects (equal-effects-p effects1 effects2 binding)))
    (if binding-effects
	(if (or (eq t binding-effects)
		(not (car binding-effects)))
	    binding
	    (append binding-effects binding)))))
  
;; Returns the bindings in case of being a goal-based meta-predicate
(defun new-get-bindings-of-some-goals (preconds1 preconds2 effects1 effects2 bindings test-inclusion-p)
  (declare (list preconds1 preconds2 bindings))
  (if test-inclusion-p
      (do ((aux-bindings bindings (cdr aux-bindings))
	   (last-bindings nil))
	  ((or (null aux-bindings) last-bindings)
	   last-bindings)
	(let* ((binding (car aux-bindings))
	       (new-binding (get-binding-of-goals (cadar preconds1) (cadar preconds2) binding))
	       (the-append (if new-binding
			       (my-append new-binding binding #'eq-binding-p)))
	       (new-bindings (if the-append
				 (if (car the-append)
				     (list the-append)
				     the-append))))
	  (setq last-bindings
		(if new-bindings
		    (new-get-bindings-of-preconds (cdr preconds1) (cdr preconds2) effects1 effects2 new-bindings test-inclusion-p)))))
      (let ((new-bindings
	     (mapcan #'(lambda (binding)
			 (let* ((new-binding (get-binding-of-goals (cadar preconds1) (cadar preconds2) binding))
				(the-append (if new-binding
						(my-append new-binding binding #'eq-binding-p))))
			   (if the-append
			       (if (car the-append)
				   (list the-append)
				   the-append))))
		     bindings)))
	(if new-bindings
	    (new-get-bindings-of-preconds (cdr preconds1) (cdr preconds2) effects1 effects2 new-bindings test-inclusion-p)))))
				
;;; Returns a list of bindings from all the true-in-state in the preconds.
;;; This is more efficient than the clearer way of doing it by
;;; removing all meta-predicates of the preconds that are not state.
;;; The dummy thing is for the first cdr in the do.
;;; Incremental version
(defun new-get-bindings-of-state (preconds1 preconds2 effects1 effects2 bindings test-inclusion-p)
  (declare (list preconds1 preconds2 bindings))
  (let ((p1 (cons 'dummy preconds1))
	(p2 (cons 'dummy preconds2))
	state1 state2 end1 end2)
    (do ()
	((or (null p1) end1))
      (setq p1 (cdr p1))
      (if (eq (caar p1) 'true-in-state)
	  (push (cadar p1) state1)
	  (setq end1 t)))
    (do ()
	((or (null p2) end2))
      (setq p2 (cdr p2))
      (if (eq (caar p2) 'true-in-state)
	  (push (cadar p2) state2)
	  (setq end2 t)))
    (if (or (not test-inclusion-p)
	    (<= (length state1) (length state2)))
	(get-all-bindings-i state1 state2 p1 p2 effects1 effects2 bindings test-inclusion-p t))))

(defun new-get-bindings-some-or-prior-goals (preconds1 preconds2 effects1 effects2 bindings test-inclusion-p prior-goal-p)
  (declare (list preconds1 preconds2 bindings))
  (let ((meta-predicate1 (caar preconds1))
	(meta-predicate2 (caar preconds2)))
    (cond ((or (and test-inclusion-p
		    (eq meta-predicate1 meta-predicate2))
	       (and (not test-inclusion-p)
		    (or (and (not prior-goal-p)
			     (eq meta-predicate2 'some-candidate-goals))
			(and prior-goal-p
			     (eq meta-predicate2 'prior-goal)))))
	   (new-get-bindings-some-or-prior-goals-1 preconds1 preconds2 effects1 effects2 bindings test-inclusion-p))
	  ((and (not prior-goal-p)
		(eq meta-predicate2 'true-in-state))
	   (do ((preconds preconds2 (cdr preconds)))
	       ((or (null preconds)
		    (not (eq (caar preconds) 'true-in-state)))
		(new-get-bindings-some-or-prior-goals preconds1 preconds effects1 effects2 bindings test-inclusion-p prior-goal-p))))
	  (t (new-get-bindings-of-preconds (cdr preconds1) preconds2 effects1 effects2 bindings test-inclusion-p)))))

;; Incremental version
(defun new-get-bindings-some-or-prior-goals-1 (preconds1 preconds2 effects1 effects2 bindings test-inclusion-p)
  (declare (list preconds1 preconds2 bindings))
  (let ((goals1 (cadar preconds1))
	(goals2 (cadar preconds2))
	(rest-preconds1 (cdr preconds1))
	(rest-preconds2 (cdr preconds2)))
    (if (or (not test-inclusion-p)
	    (eq test-inclusion-p 'intersection)
	    (<= (length goals2) (length goals1)))
	(if (and goals1 goals2)
	    (get-all-bindings-i goals1 goals2 rest-preconds1 rest-preconds2 effects1 effects2 bindings test-inclusion-p nil)
	    (if (or (and test-inclusion-p (not goals2)
			 (or (not goals1)
			     (member nil goals1 :test #'eq)))
		    (not test-inclusion-p))
		(purge-bindings (new-get-bindings-of-preconds rest-preconds1 rest-preconds2 effects1 effects2 bindings test-inclusion-p)
				goals1))))))

(defun reverse-bindings (bindings)
  (declare (list bindings))
  (mapcar #'(lambda (binding)
	      (reverse-binding binding))
	  bindings))

(defun test-bindings-of-types (preconds1 preconds2 bindings test-function)
  (declare (list preconds1 preconds2 bindings))
;; (function test-function)
  (let ((result nil))
    (dolist (binding bindings result)
      (if (all-matching-types test-function preconds1 preconds2 binding)
	  (pushnew binding result :test #'equal-bindings-p)))))

(defun all-matching-types (test-function preconds1 preconds2 binding)
  (declare (list preconds1 preconds2 binding))
;; (function test-function)
  (or (null binding)
      (and (funcall test-function preconds1 preconds2 (car binding))
	   (all-matching-types test-function preconds1 preconds2 (cdr binding)))))

;; It returns the common supertype that the vars in the bind pair
;; have, in case they have one different than :top-type
(defun common-supertype (preconds1 preconds2 bind-pair)
  (declare (list preconds1 preconds2 bind-pair))
  (let ((type1 (generic-type-name-to-type (find-type-of-var preconds1 (car bind-pair)) (if (eq *planner-for-learning* 'ipss) *current-problem-space*)))
	(type2 (generic-type-name-to-type (find-type-of-var preconds2 (cdr bind-pair)) (if (eq *planner-for-learning* 'ipss) *current-problem-space*))))
    (if (and type1 type2)
	(if (eq type1 type2)
	    (generic-type-name type1)
	    (let* ((all-supers1 (cons type1 (generic-type-all-parents type1)))
		   (all-supers2 (cons type2 (generic-type-all-parents type2)))
		   (first-common-type (find-common-type all-supers1 all-supers2)))
	      (if first-common-type
		  (generic-type-name first-common-type)))))))

(defun find-all-type-parents (type &optional types)
  (unless (eq type 'object)
    (if (not types)
	(setq types (dom-domtypes *pspace*)))
    (if (listp (car types))
	(let ((parent (car (find type types :key #'cadr :test #'member))))
	  (cons parent (find-all-type-parents parent types)))
	(list 'object))))

(defun find-common-type (all-supers1 all-supers2)
  (declare (list all-supers1 all-supers2))
  (cond ((eq (car all-supers1) (if (eq *planner-for-learning* 'ipss) *top-type* 'object)) nil)
	((member (car all-supers1) all-supers2)
	 (car all-supers1))
	(t (find-common-type (cdr all-supers1) all-supers2))))

(defun more-general-type-p (preconds1 preconds2 bind-pair)
  (declare (list preconds1 preconds2 bind-pair))
  (let ((type1 (find-type-of-var preconds1 (car bind-pair)))
	(type2 (find-type-of-var preconds2 (cdr bind-pair))))
    (or (eq type1 type2)
	(member type1 (generic-type-all-parents (type-name-to-type type2 *current-problem-space*))
		:test #'eq :key #'(lambda (x) (generic-type-name x))))))


(defun find-type-of-var (preconds var)
  (declare (list preconds))
  (let ((precond-of-var (member var preconds :key #'cadr)))
    (if precond-of-var
	(caddar precond-of-var))))

;;
;; Given two literals with variables, it returns the same as
;; new-binding if the predicates are the same, and the are both + or -
;;
(defun get-binding-of-goals (goal1 goal2 binding)
  (declare (list goal1 goal2 binding))
  (let* ((negatedp1 (negatedp goal1))
	 (negatedp2 (negatedp goal2))
	 pos-goal1 pos-goal2
	 (matchp (cond ((and negatedp1 negatedp2)
			(setq pos-goal1 (cadr goal1))
			(setq pos-goal2 (cadr goal2))
			t)
		       ((and (not negatedp1) (not negatedp2))
			(setq pos-goal1 goal1)
			(setq pos-goal2 goal2)
			t)
		       (t nil))))
    (if (and matchp (eq (car pos-goal1) (car pos-goal2)))
	(new-binding (cdr pos-goal1) (cdr pos-goal2) binding))))

;;
;; Given two lists of vars and an initial binding, it returns
;; 1. (nil) if they match, but there are not variables
;; 2. nil if they do not match
;; 3. A binding if there is a match
;;
(defun new-binding (vars1 vars2 binding)
  (declare (list vars1 vars2 binding))
  (cond ((and (null vars1) (null vars2)) '(nil))
	((or (null vars1) (null vars2)) nil)
	(t (let ((var1 (car vars1))
		 (var2 (car vars2)))
	     (if (p4::is-variable-p var1)
		 (let ((memberp (member var1 binding :test #'eq :key #'car)))
		   (if memberp
		       (if (eq (cdar memberp) var2)
			   (new-binding (cdr vars1) (cdr vars2) binding))
		       (let ((memberp2 (member var2 binding :test #'eq :key #'cdr)))
			 (if (not memberp2)
			     (combine-bindings vars1 vars2 binding)))))
	         (if (eq var1 var2)
		     (new-binding (cdr vars1) (cdr vars2) binding)))))))

;;; This version allows to have a substitution list such as
;;; ((<x> . <y>)(<z> . <y>)), which is right according to the
;;; unification algorithm, but not according to the restriction in
;;; hamlet that the variables in the control rule all have to be different.
(defun combine-bindings (vars1 vars2 binding)
  (declare (list vars1 vars2 binding))
  (let* ((new-bind (cons (car vars1) (car vars2)))
	 (new-binding (new-binding (cdr vars1) (cdr vars2) (cons new-bind binding))))
    (if new-binding
	(if (car new-binding)
	    (cons new-bind new-binding)
	    (list new-bind)))))

;; If test-inclusion-p is t, it
;; returns a list of bindings if goals1 is a matching subset of
;; goals2, giving the binding as input
;; (((<o1> . <o2>) ...) ((<o1> . <o3>) ...) ...)
;; If nil, it returns the same if they match somehow
;; Any binding might be of the form (NIL), which means that they match
;; but there are not variables.
(defun get-bindings-of-goals (goals1 goals2 binding test-inclusion-p)
  (declare (list goals1 goals2 binding))
  (if (and goals1 goals2)
      (let ((goal1 (car goals1))
	    (new-bindings nil))
	(dolist (goal2 goals2)
	  (let ((new-binding (get-binding-of-goals goal1 goal2 binding)))
	    (if new-binding
		(let ((bindings-of-goals
		       (get-bindings-of-goals (cdr goals1) (remove goal2 goals2 :test #'equal) (my-append new-binding binding #'eq-binding-p) test-inclusion-p)))
		  (dolist (binding-of-goals bindings-of-goals)
		    (pushnew binding-of-goals new-bindings :test #'equal-bindings-p))))))
	(if test-inclusion-p
	    new-bindings
	    (let ((rest-bindings (get-bindings-of-goals (cdr goals1) goals2 binding test-inclusion-p)))
	      (if (or new-bindings rest-bindings)
		  (my-append rest-bindings new-bindings #'equal-bindings-p)))))
      (if (and goals1 test-inclusion-p (not goals2))
	  nil
	  (list binding))))

;; If test-inclusion-p is t, it
;; returns a list of bindings if goals1 is a matching subset of
;; goals2, giving the binding as input
;; (((<o1> . <o2>) ...) ((<o1> . <o3>) ...) ...)
;; If nil, it returns the same if they match somehow
;; Any binding might be of the form (NIL), which means that they match
;; but there are not variables.
;; Incremental version
(defun new-get-bindings-of-goals (goals1 goals2 rest1 rest2 effects1 effects2 binding test-inclusion-p statep)
  (declare (list goals1 goals2 binding))
  (if (and goals1 goals2)
      (if test-inclusion-p
	  (get-bindings-of-goals-i goals1 goals2 rest1 rest2 effects1 effects2 binding test-inclusion-p statep)
	  (get-bindings-of-goals-ni goals1 goals2 effects1 effects2 binding test-inclusion-p))
      (if (and goals1 test-inclusion-p (not goals2))
	  nil
          (list binding))))

(defun get-bindings-of-goals-i (goals1 goals2 rest1 rest2 effects1 effects2 binding test-inclusion-p statep)
  (do* ((rest-goals2 goals2 (cdr rest-goals2))
	(goal2 (car rest-goals2) (car rest-goals2))
	(goal1 (car goals1))
	(new-bindings nil)
	(last-bindings nil))
       ((or (null rest-goals2) last-bindings)
	last-bindings)
    (let* ((new-binding (get-binding-of-goals goal1 goal2 binding)))
      (when new-binding
	(setq new-bindings
	      (new-get-bindings-of-goals (cdr goals1) (remove goal2 goals2 :test #'equal) rest1 rest2 effects1 effects2
					 (my-append new-binding binding #'eq-binding-p) test-inclusion-p statep))
	(setq last-bindings
	      (if statep
		  (if new-bindings
		      (new-get-bindings-of-preconds rest1 rest2 effects1 effects2 new-bindings test-inclusion-p))
		  new-bindings))))))

(defun get-bindings-of-goals-ni (goals1 goals2 effects1 effects2 binding test-inclusion-p)
  (let ((goal1 (car goals1))
	(new-bindings nil))
    (dolist (goal2 goals2)
      (let ((new-binding (get-binding-of-goals goal1 goal2 binding)))
	(if new-binding
	    (let ((bindings-of-goals (new-get-bindings-of-goals (cdr goals1) (remove goal2 goals2 :test #'equal) nil nil effects1 effects2
								(my-append new-binding binding #'eq-binding-p) test-inclusion-p nil)))
	      (dolist (binding-of-goals bindings-of-goals)
		(pushnew binding-of-goals new-bindings :test #'equal-bindings-p))))))
    (let ((rest-bindings (new-get-bindings-of-goals (cdr goals1) goals2 nil nil effects1 effects2 binding test-inclusion-p nil)))
      (if new-bindings
	  (if rest-bindings
	      (if (equal rest-bindings (list binding))
		  new-bindings
		  (my-append rest-bindings new-bindings #'equal-bindings-p))
	      new-bindings)
	  rest-bindings))))
	      
;; Returns the list of bindings that can match both sets of goals
(defun get-all-bindings (goals1 goals2 bindings test-inclusion-p)
  (declare (list goals1 goals2 bindings))
  (mapcan #'(lambda (binding)
	      (get-bindings-of-goals goals1 goals2 binding test-inclusion-p))
	  bindings))

;; Returns the list of bindings that can match both sets of goals
;; Incremental version. If statep is t, we are getting the bindings of
;; the state. If not, we are getting them from some-candidate-goals or
;; prior-goal
(defun get-all-bindings-i (goals1 goals2 rest1 rest2 effects1 effects2 bindings test-inclusion-p statep)
  (declare (list goals1 goals2 bindings))
  (if test-inclusion-p
      (if statep
	  (get-all-bindings-i-state goals1 goals2 rest1 rest2 effects1 effects2 bindings test-inclusion-p)
	  (get-all-bindings-i-some goals1 goals2 rest1 rest2 effects1 effects2 bindings test-inclusion-p))
      (if statep
	  (or (mapcan #'(lambda (binding)
			  (let ((new-bindings (new-get-bindings-of-goals goals1 goals2 rest1 rest2 effects1 effects2 binding test-inclusion-p statep)))
			    (if new-bindings
				(new-get-bindings-of-preconds rest1 rest2 effects1 effects2 new-bindings test-inclusion-p))))
		      bindings)
	      bindings)
	  (let ((new-bindings (reverse-bindings (get-all-bindings goals2 goals1 (reverse-bindings bindings) nil))))
	    (if new-bindings
		(purge-bindings (new-get-bindings-of-preconds rest1 rest2 effects1 effects2 new-bindings test-inclusion-p)
				goals1))))))

(defun get-all-bindings-i-state (goals1 goals2 rest1 rest2 effects1 effects2 bindings test-inclusion-p)
  (do ((aux-bindings bindings (cdr aux-bindings))
       (last-bindings nil))
      ((or (null aux-bindings) last-bindings)
       last-bindings)
    (let* ((binding (car aux-bindings))
	   (new-bindings (new-get-bindings-of-goals goals1 goals2 rest1 rest2 effects1 effects2 binding test-inclusion-p t)))
      (setq last-bindings
	    (if new-bindings
		(new-get-bindings-of-preconds rest1 rest2 effects1 effects2 new-bindings test-inclusion-p))))))

;; This is needed only for the incremental version of the function
;; new-get-bindings-some-or-prior-goals-1
(defun get-all-bindings-i-some (goals1 goals2 rest-preconds1 rest-preconds2 effects1 effects2 bindings test-inclusion-p)
  (some #'(lambda (binding)
	    (let* ((reverse-binding (reverse-binding binding))
		   (new-bindings (new-get-bindings-of-goals goals2 goals1 rest-preconds1 rest-preconds2 effects1 effects2 reverse-binding (eq test-inclusion-p 'subset) nil))
		   (last-bindings
		    (find-if #'(lambda (new-binding)
				 (purge-bindings (new-get-bindings-of-preconds rest-preconds1 rest-preconds2 effects1 effects2 (list (reverse-binding new-binding)) test-inclusion-p)
						 goals1))
			     new-bindings)))
	      (if last-bindings
		  (list last-bindings))))
	bindings))

;;; It generates a new bind-pair for each binding list in which there is a
;;; bind pair such as (<object2> . <object1>), there is no bind pair
;;; for <object1>, and there is a goal in goals that has the variable
;;; <object1>
(defun purge-bindings (bindings goals)
  (declare (list goals bindings))
  (mapcan #'(lambda (binding)
	      (let ((new-binding binding))
		(setf *number-vars-in-cr* 0)
		(setf *rule-id* (string-trim "#:G" (gensym)))
		(dolist (goal goals (list new-binding))
		  (dolist (var (cdr (get-pos-assertion goal)))
		    (if (and (member var binding :test #'eq :key #'cdr)
			     (not (member var binding :test #'eq :key #'car)))
			(push (cons var (new-var-from-new-var var))
			      new-binding))))))
	  bindings))

(defun new-var-from-new-var (var)
  (incf *number-vars-in-cr*)
  (intern (format nil "~:@(<~a-~d-~d>~)"
		  (string-trim (cons #\- *trim-characters*) var)
		  *rule-id* *number-vars-in-cr*)))

;; Returns t if both effects are equal. Now, we check whether the
;; bindings are also compatible. Returns (nil), t, or a binding of
;; variables not appearing in binding
(defun equal-effects-p (effects1 effects2 binding)
  (declare (list effects1 effects2 binding))
  (and (eq (car effects1) (car effects2))
       (or (and (null (cdr effects1)) (null (cdr effects2)))
	   (and (eq (cadr effects1) (cadr effects2))
		(case (cadr effects1)
		  (operators (eq (caddr effects1) (caddr effects2)))
		  ((goals instantiated-operator) (get-binding-of-goals (caddr effects1) (caddr effects2) binding))
		  (bindings (new-binding (mapcar #'cdr (caddr effects1))
					 (mapcar #'cdr (caddr effects2))
					 binding))
		  (t nil))))))

(defun my-append (list1 list2 function)
  (declare (list list1 list2))
  ;; (function function)
  (cond	((and (null list1) (null list2)) nil)
	((null list1) list2)
	((null list2) list1)
	((and (car list1) (car list2))
	 (let ((result (copy-list list2)))
	   (dolist (e1 list1 result)
	     (pushnew e1 result :test function))))
	((car list2) list2)
	((car list1) list1)
	(t (cons nil nil))))

;; Deletes a new control rule
;; Deleting mark can be either t, nil, or forever (see induction.lisp
;; for explanation, at the beginning)
(defun delete-cr (cr node le deleting-mark)
  (declare (type decision-node node)
	   (type learning-episode le))
  (setf *deleted-rule-p* t)
  (incf *number-deleted-rules*)
  (setf *cr-list* (remove-nil-from-rules (remove cr *cr-list* :test #'eq-name)))
  (setf *initial-cr* (remove-nil-from-rules (remove cr *initial-cr* :test #'eq-name)))
  (format t "Deleting rule ~a~%" (generic-control-rule-name cr))
  (if (or (eq *probabilities-p* 'rules) 
	  (eq *probabilities-p* 'both))
      (borra-regla-prob cr node nil))
  (when le
    (setf (learning-episode-deletedp le) deleting-mark)
    (if node
	(setf (decision-node-rules node)
	      (remove cr (decision-node-rules node) :test #'eq-name)))))

;;; High level function for deleting a rule
(defun delete-rule (rule &optional (deleting-mark 'forever))
  (if (and (eq *planner-for-learning* 'ipss)
	   (not (p4::control-rule-p rule)))
      (setq rule (from-name-to-rule-1 rule)))
  (delete-cr rule (find-decision-tree-node rule)
	     (gethash (generic-control-rule-name rule) *cr-to-le-hash*)
	     deleting-mark))

(defun eq-name (cr1 cr2)
  (eq (generic-control-rule-name cr1)
      (generic-control-rule-name cr2)))

;; It returns a control rule in version4 format if evalp is t. If stream is
;; different than nil, it also saves the definition in a file
(defun save-prodigy-cr (cr stream)
  (pp-cr (generic-control-rule-name cr) (get-real-preconds cr) (cons 'then (get-real-effects cr)) stream))
;; 	 (cdr (p4::control-rule-if cr))
;; 	 (p4::control-rule-then cr)

;;;***************************************************************
;; Function that refines rules, checking whether they are subsumed or equal
;; I'll have to refine this because of the learning-episodes
(defun refine-rules (&optional (cr-file (cr-file)) (refined-file (cr-file)) (domain-name *domain-name*) (domain-file-name "domain.pddl"))
  (if (eq *planner-for-learning* 'ipss)
      (domain domain-name :path *hamlet-world-path* :domain-filename *hamlet-domain-file-name* :load-domain nil :function :compile)
      (say-domain domain-name domain-file-name))
  (when (probe-file cr-file)
    (cond ((eq *planner-for-learning* 'ipss)
	   (load cr-file)
	   (load-domain))
	  (t (load-rules-build-rete cr-file)))
    (setf *cr-list* nil)
    (setf *number-deduced-rules* 0)
    (setf *number-induced-rules* 0)
    (setf *number-refined-rules* 0)
    (setf *number-deleted-rules* 0)
    (setf *number-undeleted-rules* 0)
    (dolist (control-rules-access (if (eq *planner-for-learning* 'ipss)
				      *learned-rules-types*
				      (list 'all)))
      (let ((cr-list nil))
	(dolist (control-rule (if (eq *planner-for-learning* 'ipss)
				  (funcall control-rules-access *current-problem-space*)
				  (all-control-rules)))
	  (let ((includedp (include-rule control-rule cr-list)))
	    (if (or (eq includedp 'include)
		    (eq includedp 'delete-2))
		(push control-rule cr-list))))))
    (when *cr-list*
      (with-open-file (stream refined-file :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format stream ";;; Refined rules from the file ~a" cr-file)
	(format stream "~%(in-package \"COMMON-LISP-USER\")")
	;;	 (sort *cr-list* #'string-lessp :key #'(lambda (cr)
	;;						 (control-rule-name cr)))
	(dolist (frame-rc *cr-list*)
	  (save-prodigy-cr frame-rc stream)))
      t)))


;;; ***************************************************************
;;;    Some useful functions
;;; ***************************************************************

;; Saves the control rules on a file given by the global variable *cr-file*
(defun save-cr nil
  (let ((new-crs (remove-duplicates (if (or *deleted-rule-p* *bootstrapp*)
					(append *cr-list* *initial-cr*)
				        *cr-list*))))
    (if new-crs
	(with-open-file (stream (cr-file) :direction :output
				;; 				:element-type 'character
				:if-exists (if (or *deleted-rule-p* *bootstrapp*)
					       :supersede
					       :append)
				:if-does-not-exist :create)
          (format stream "~2%;;; New control rules of problem ~a" *problem-name*)
          (format stream "~%(in-package \"COMMON-LISP-USER\")")
	  (if *printp* (format t "~%New control rules learned:"))
	  (dolist (cr new-crs)
	    (save-prodigy-cr cr stream)
	    (if *printp* (pp-control-rule cr)))))
    (if *break-aprender-p* (break))
    (setf *cr-list* (if (or *deleted-rule-p* *bootstrapp*)
			new-crs
		        (delete-duplicates (append new-crs *initial-cr* nil) :test #'eq)))
    (setf *rules* *cr-list*)))

;;; prior way of computing new number of rules:
;;; 		   (- (length *cr-list*) *number-induced-rules*)

(defun print-learning-data (learning-time)
  (with-open-file (stream *hamlet-output* :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "~%        (learning-~:@(~a~) ~d ~d ~d ~d ~d ~f)"
	    *problem-name* *number-deduced-rules*
	    *number-induced-rules* *number-refined-rules*
	    *number-deleted-rules* *number-undeleted-rules*
	    learning-time)))

;;; Simpler version for saving all rules in *cr-list*
(defun save-rules (&optional (file (cr-file)))
  (declare (pathname file))
  (with-open-file (stream file :direction :output
			  ;; 			  :element-type 'character
			  :if-exists :new-version
			  :if-does-not-exist :create)
    (format stream "~2%;;; New control rules of problem ~a" *problem-name*)
    (format stream "~%(in-package \"COMMON-LISP-USER\")")
    (if (and *printp* *cr-list*) (format t "~%New control rules learned:"))
    (setf *cr-list* (remove-duplicates *cr-list* :test #'eq))
    (setf *rules* *cr-list*)
    (dolist (new-cr *cr-list*)
      (save-prodigy-cr new-cr stream)
      (if *printp* (pp-control-rule new-cr)))))

;; Reads from a file and prints the control rules on that file
(defun read-and-pprint (&optional (file (cr-file)))
  (with-open-file (stream file :direction :input :if-does-not-exist nil)
    (if stream
	(do ((regla (read stream nil 'eof) (read stream nil 'eof)))
	    ((eq regla 'eof))
	  (pprint regla)))))

;; Prints a control rule in a reasonable way
(defun pp-control-rule (cr &key (list-form-p nil) (stream t) (depth 1))
  ;;  (declare (type p4::control-rule cr))
  (let* ((rule (cond ((or (listp cr) (p4::control-rule-p cr)) cr)
		     ((stringp cr) (read-from-string cr))
		     ((atom cr) (from-name-to-rule-1 cr))
		     (t cr)))
	 (name (generic-control-rule-name cr))
	 (preconds (get-real-preconds cr))
	 (effects (cons 'then (get-real-effects cr))))
    (format stream (if list-form-p
		       (format nil "~~2%~~~dt(control-rule ~~a" depth)
		       "~%Rule ~a~%Preconds:")
	    name)
    (format stream (if list-form-p
		       (format nil "~~%~~~dt(if (and ~~(~~a~~)" (+ depth 3))
		       "~%  ~a")
	    (car preconds))
    (dolist (precond (cdr preconds))
      (format stream  (if list-form-p
			  (format nil "~~%~~~dt~~(~~a~~)" (+ depth 12))
			  "~%  ~a")
	      precond))
    (format stream "))")
    (if (and (= (length effects) 4)
	     (eq (caddr effects) 'bindings))
	(format stream (if list-form-p
			   (format nil "~~%~~~dt(~~(~~a ~~a ~~a~~%~~a~~)))" (+ depth 3) (+ depth 12))
			   "~%Effects: ~a")
		(car effects) (cadr effects) (caddr effects) (cadddr effects))
	(format stream (if list-form-p
			   (format nil "~~%~~~dt~~(~~a~~))" (+ depth 3))
			   "~%Effects: ~a")
		effects))))

(defun equal-rule-p (rule1 rule2 test-inclusion-p)
  ;;   (if (not (p4::control-rule-p rule1))
  ;;       (setq rule1 (from-name-to-rule-1 rule1)))
  ;;   (if (not (p4::control-rule-p rule2))
  ;;       (setq rule2 (from-name-to-rule-1 rule2)))
  (equal-control-rule-p (get-real-preconds rule1) (get-real-preconds rule2)
			(get-real-effects rule1) (get-real-effects rule2)
			test-inclusion-p))

(defun pr (n)
  (pp-control-rule (nth n *rules*)))

(defun ple (n)
  (pp-le1 (gethash (generic-control-rule-name (nth n *rules*))
		   *cr-to-le-hash*)))
