;; SAYPHI Planner 
;; Learning Control Rules
;; DB. 17.07.2006
;; _________________________________________________________________________________________

;; (load "(/home/dborrajo/planning/ipss/hamlet")

(in-package "COMMON-LISP-USER")

(defvar *print-all-mem* nil)
(defvar *ace-directory* (concatenate 'string *my-planning-path* "sayphi/ace/"))
(defvar *sayphi-rules* nil)
(defvar *remove-static-preds* t)
(defvar *cr-file* "rules.lisp")
;; not needed for now
;; (defvar *examples* nil)
(defvar *generate-learning-examples-p* t)
(defvar *trace-pruning* nil)
(defvar *actions-tilde-tree* nil)
(defvar *bindings-tilde-trees* nil)

(defstruct partial-plan
  (goals nil)
  (end-goals-achieved nil)
  (dependent-actions nil)
  (links nil)
  (actions nil)
  (orderings nil))

(defun ebl-sayphi-train (&key (domain "blocksworld-ipc") (domain-file "blocksworld-typed.pddl") (rules-format 'example) (probs-prefix "bw*") (probs-sufix "-typed.pddl")
			 (algorithm 'hc-bnb) (timeout 30) (output-path nil) (problems-dir "bootstrap/") (generate-extra-problems-p nil) (output-tilde-file "tilde-decision-tree.lisp"))
  (let ((re-init-p t)
	(this-sol nil)
	(count 0)
	(actions nil))
    (setf *say-output* 1)
    (setf *problem-dir* problems-dir)
    (say-domain domain domain-file)
    (setq actions (get-all-operators))
    (dolist (prob (dirfiles-sorted (format nil "~a/~a" *domain-dir* *problem-dir*) (concatenate 'string probs-prefix probs-sufix)))
      (loop do
	   (format t "~% Solving problem ~a with ~a" (pathname-name prob) algorithm)
	   (read-pddl-problem prob)
	   (incf count)
	   (setq this-sol (plan :algorithm algorithm :timeout timeout))
	   (if (not (and this-sol (eq (solution-stop-reason this-sol) :goals-reached)))
	       (setq prob (modify-problem 'relax-problem domain prob count 0 nil nil :odir problems-dir)))
	 until (and this-sol (eq (solution-stop-reason this-sol) :goals-reached)))
      (learn :rules-file (cr-file) :better-heuristic-p nil :negativesp t :only-from-plateaus nil :remove-preconds-from-rule t :remove-static-preds t :print-data-p nil
	     :re-init-p re-init-p :rules-format rules-format :domain domain)
      (setq re-init-p nil)
      (if generate-extra-problems-p
	  ;; I generate single and double goal problems from the original to have more examples
	  (dolist (newprob (modify-problem 'split domain prob 0 2 nil nil :odir problems-dir))
	    (format t "~% Solving problem ~a with ~a" (pathname-name newprob) algorithm)
	    (read-pddl-problem newprob)
	    (plan :algorithm algorithm :timeout timeout)
	    (learn :rules-file (cr-file) :better-heuristic-p nil :negativesp t :only-from-plateaus nil :remove-preconds-from-rule t :remove-static-preds t :print-data-p nil
		   :re-init-p re-init-p :rules-format rules-format))))
    (if (not output-path) (setq output-path (concatenate 'string *domain-dir* "rules/")))
    (when (probe-file (cr-file))
      (if (eq rules-format 'example) (write-to-tilde (cr-file) domain domain-file 'classify 'control-rules output-path))
      (execute-ace output-path domain actions)
      (tilde-decision-trees-to-list output-path domain actions (concatenate 'string output-path output-tilde-file)))))

(defun execute-ace (output-directory domain actions)
  (let ((command (format nil "export ACE_ILP_ROOT=~alinux; cd ~aactions/; ${ACE_ILP_ROOT}/bin/ace" *ace-directory* output-directory)))
    (dolist (action actions)
      (setq command (concatenate 'string command (format nil "; cd ~abindings-~(~a~)/; ${ACE_ILP_ROOT}/bin/ace" output-directory action))))
    #-sbcl (run-shell-command command :wait t)
    #+sbcl (progn (with-open-file (ostream (concatenate 'string output-directory "run-ace") :direction :output :if-exists :supersede :if-does-not-exist :create)
		    (format ostream "#/bin/bash/~%~a~%" command))
		    (sb-ext:run-program "/bin/bash" (list (concatenate 'string output-directory "run-ace"))))
    ))

(defun tilde-decision-trees-to-list (tilde-path domain actions decision-tree-file)
  (let ((actions-list (list (cons 'actions (tilde-output-to-list (format nil "~aactions/tilde/~(~a~)-select-action.out" tilde-path domain))))))
    (dolist (action actions)
      (push (cons action (tilde-output-to-list (format nil "~a~(bindings-~a/tilde/~a-select-bindings-~a~).out" tilde-path action domain (remove-dash action))))
	    actions-list))
    (with-open-file (ostream decision-tree-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (print (reverse actions-list) ostream))))

(defun tilde-output-to-list (tilde-file-path)
  (let ((btree-list nil)
	(btree nil)
	(line nil)
	(time-stamp))
    (with-open-file (istream tilde-file-path :direction :input)
      (loop
	 (setq line (read-line istream :eof))
	 ;;reading the time-stamp
	 (when (equal line " ** Algorithm:   Tilde")
	   ;;reading padding lines
	   (setq line (read-line istream :eof))
	   (setq line (read-line istream :eof))	     
	   (setf line (concatenate 'string "(" line))
	   (setf line (concatenate 'string line ")"))
	   (when (equal 'DATE/ (nth 1 (read-from-string (string-replace line ":" "/"))))
	     (setf time-stamp (format nil "~a-~a" (nth 2 (read-from-string (string-replace line ":" "/")))
				      (nth 3 (read-from-string (string-replace line ":" "/")))))))
	 ;;Reading the tree
	 (when (equal line "Compact notation of tree:")
	   ;;reading padding line
	   (setq line (read-line istream :eof))
	   (loop	 
	      (setq line (read-line istream :eof))
	      (when (= (length line) 0) (setf line :eof) (return))
	      (setf line (parse-tilde-format-line line))
	      (push (read-from-string line) btree-list)))
	 (when (equal line :eof) (return))))
      (reverse btree-list)))

;; It generates control rules from the solution path of a solved problem
;; If rules-file is non-nil, it will write the rules on that file
;; better-heuristic-p determines whether rules are generated only from nodes
;;                    improving the h value or from the whole path
;; only-from-plateaus learns only from plateus decisions
;; rules-format can be either: prodigy (default,control rules in prodigy format), or
;;                       example (for learning through other ILP systems)
;; if rules are given, then the new ones will be added to the previous ones
;; remove-static-preds allows to reduce the size of rules
;; re-init-p should be true when the first learning call is made and nil otherwise
;; negativesp if true will force to consider as negatives the rest of options. Not recommended in case you did not expand the whole search tree
(defun generate-rules-from-solution (&key (rules-file (cr-file)) (rules nil) (say-solution *say-solution*) (better-heuristic-p nil) (only-from-plateaus nil) (negativesp nil)
				     (remove-preconds-from-rule nil) (remove-static-preds *remove-static-preds*) (re-init-p t) (rules-format 'prodigy))
  (when (and (solution-p say-solution)
	     (solution-found say-solution))
    (if (and *inductionp* (not *decision-tree*))
	(build-fixed-decision-tree))
    (setf *cr-list* rules)
    ;;     (pp-list (pp-solution-sayphi))
    (do* ((solution (solution-path say-solution) (cdr solution))
	  (partial-plan (build-partial-plan solution))
	  (partial-plan-actions (partial-plan-actions partial-plan))
	  (actions nil)
	  (ordering-closure (compute-ordering-closure partial-plan))
	  (rule nil)
	  (name nil)
	  (i 1 (1+ i))
	  (state nil)
	  (goal-regression (compute-goal-regression partial-plan solution))
	  (real-regression (if remove-static-preds (remove-static-preds goal-regression) goal-regression))
	  (inode (car solution) (car solution)))
	 ((null solution))
      (when (= i 1)
	(dotimes (j (length partial-plan-actions))
	  (push (car (aref partial-plan-actions j)) actions))
	(setq actions (reverse actions)))
      (if (> *say-output* 1) (format t "~%Learning from node ~d, action ~a, h(n)=~2$, h(padre)=~2$" (snode-number inode)
				     (if (snode-applied-action inode) (gaction-planaction (snode-applied-action inode))) (snode-h-value inode) (snode-h-value (snode-parent inode))))
      (when (or (and better-heuristic-p
		     (< (snode-h-value inode) (snode-h-value (snode-parent inode))))
		(and only-from-plateaus
		     (>= (snode-h-value inode) (snode-h-value (snode-parent inode))))
		(and (not better-heuristic-p) (not only-from-plateaus)))
	(if (> *say-output* 1) (format t ", yes"))
	(learn-one-rule (generate-rule inode (aref real-regression i)
				       (aref (partial-plan-end-goals-achieved partial-plan) i)
				       (nth 1 (aref (partial-plan-actions partial-plan) i))
				       remove-preconds-from-rule *last-example* rules-format t)
			(length (cdr solution)) (or *inductionp* *utilityp*) t)
	(when negativesp
	  (setq state (if (snode-parent inode)
			  (give-me-nice-sayphi-state (snode-parent inode))
			  (problem-lit-init-state *current-problem*)))
	  (dolist (sibling (generate-worse-siblings inode ordering-closure actions i))
	    (learn-one-rule (generate-rule sibling state
					   (aref (partial-plan-end-goals-achieved partial-plan) i)
					   (nth 1 (aref (partial-plan-actions partial-plan) i))
					   nil *last-example* rules-format nil)
			    (length (cdr solution)) nil (eq rules-format 'example))))))
    (if (and rules-file *cr-list* (eq rules-format 'example))
	(with-open-file (ofile rules-file :direction :output :if-exists (if re-init-p :supersede :append) :if-does-not-exist :create)
	  (dolist (cr (reverse *cr-list*))
	    ;; 	    (if (eq rules-format 'prodigy)
	    ;; 		(pp-cr (nth 1 cr) (cdadr (nth 2 cr)) (nth 3 cr) ofile)
	    (pp-list (list cr) 1 ofile))
	  (terpri ofile)))
    *cr-list*))

(defun generate-rule (node regressed-state end-goals preconds remove-preconds-from-rule rule-number rules-format positivep)
  (if node
      (let ((goals (mapcar #'(lambda (literal)
			       (list 'target-goal literal))
			   end-goals))
	    (state (mapcar #'(lambda (literal)
			       (list 'true-in-state literal))
			   (if remove-preconds-from-rule
			       (remove-if #'(lambda (literal) (member literal preconds :test #'equal))
					  regressed-state)
			       regressed-state)))
	    (types nil)
	    (suitable-precond nil)
	    (effects `(,(if positivep 'select 'reject) instantiated-operator ,(snode-plan-action node))))
	(setf *new-vars-format* 'sayphi)
	(setf *rule-id* (intern (format nil (string-trim "#:G" (gensym)))))
	(setf *problem-name* (problem-name *current-problem*))
	(incf *number-deduced-rules*)
	(setf *substitution* nil)
	(find-substitution state)
	;;	(format t "~%State: ~a~%Substitution: ~a" state *substitution*)
	(find-substitution goals)
	(if (eq rules-format 'example)
	    (vars-into-constants))
	(setq goals (parametrize goals))
	(setq state (parametrize state))
	(do* ((vars-in-effect (cdr (nth 2 (parametrize effects))) (cdr vars-in-effect))
	      (var-in-effect (car vars-in-effect) (car vars-in-effect)))
	    ((null vars-in-effect))
	  (unless (p4::is-variable-p var-in-effect)
	    ;; finding the first one might not always be the best option.
	    (setq suitable-precond (find-if #'(lambda (precond)
						(member var-in-effect precond))
					    preconds))
	    ;; 	    (format t "~%Found precond: ~a" suitable-precond)
	    (when suitable-precond
	      (find-substitution (list (setq suitable-precond `(true-in-state ,suitable-precond))))
	      ;; 	      (format t "~% with new substitution: ~a" *substitution*)
	      (if (eq rules-format 'example)
		  (vars-into-constants))
	      (pushnew (parametrize suitable-precond) state :test #'equal))
	    (setq vars-in-effect (parametrize vars-in-effect))))
	(setq effects (parametrize effects))
	(setq types (mapcar #'(lambda (bind-pair)
				(list 'type-of-object (cdr bind-pair) (get-object-type (car bind-pair))))
			    *substitution*))
	(setq state (sort-preconds-by-frequency state (compute-predicates-frequency)))
	(if (eq *active-learning-mode* 'rules) (update-active-learning-info state))
	;; Avoids generating duplicate (that can be unified) rules
	(if (eq rules-format 'prodigy)
	    `(control-rule ,(name-cr effects
				     (if (eq (cadr effects) 'bindings)
					 (cadar state)))
			   (if (and ,@goals ,@state ,@types))
			   (then ,@effects))
	    `(,rule-number ,positivep (,@goals ,@state ,@types) ,effects)))))
;; 	      (setq bind-pair (cons var-in-effect (new-var-name var-in-effect)))
;; 	    (push bind-pair *substitution*)
;; 	    (format t "~%Trying to add a precond from ~a~% to the state ~a~% because free var ~a~% in effect ~a~% with substitution ~a"
;; 		    preconds state var-in-effect vars-in-effect *substitution*)

;; (intern (format nil "~:@(rule-~a~)" (string-trim "#:G"  (gensym))))
;;  (mapcar #'(lambda (literal)
;; 			       (list 'true-in-state literal))
;; 			   (give-me-nice-sayphi-state parent))
;; (give-me-nice-sayphi-goals parent)

(defun learn-one-rule (rule num-nodes inducep include-in-rules-p)
  (incf *last-example*)
  (let ((name (if (eq *planner-for-learning* 'ipss)
		  (generic-control-rule-name rule)
		  *last-example*)))
    (if inducep
	(let* ((rule-preconds (and *inductionp* (get-real-preconds rule)))
	       (new-le (make-learning-episode :name (get-new-le-name)
					      :type 'deduced
					      :rule rule
					      :problem *problem-name*
					      :from nil
					      :prior-goal nil
					      :learning-steps 1
					      :matching-time 0
					      :matchings 0
					      :num-nodes num-nodes
					      :firings 0
					      :preconds-pool nil)))
	  (push new-le *le-list*)
	  (if *inductionp*
	      (update-frequencies rule-preconds (find-decision-tree-node rule) t)
	      (if (> *say-output* 1) (format t "~%Adding new control rule: ~a" name)))
	  (setf (gethash name *cr-to-le-hash*) new-le))
	(if (> *say-output* 1) (format t "~%Adding new control rule: ~a" name)))
    (if include-in-rules-p
	(pushnew rule *cr-list* :test #'unify-any-depth-p :key #'cddr))))

(defun generate-worse-siblings (node ordering-closure actions i)
  (let ((bad-siblings nil)
	(position 0))
    (dolist (child (snode-children (snode-parent node)))
      (setq position (position (gaction-planaction (snode-applied-action child)) actions :test #'equal))
      (if (and (not (eq child node))
	       (or (not position)
		   (aref ordering-closure i position)))
	  (push child bad-siblings)))
    bad-siblings))

(defun compute-goal-regression (partial-plan solution)
  (let* ((actions (partial-plan-actions partial-plan))
	 (number-actions (- (car (array-dimensions actions)) 2))
	 (goal-regression-array (make-array (list (1+ number-actions)) :initial-element nil))
	 (links (partial-plan-links partial-plan))
	 (dependent-actions nil)
	 (state (problem-lit-init-state *current-problem*))
	 (states-list nil)
	 (current-state state))
    (unless (snode-p (car solution))
      (dotimes (i (length solution))
	(push (setq current-state (generate-new-state current-state (aref actions (1+ i)))) states-list))
      (setq states-list (reverse states-list)))
    ;; I use array[1..n] instead of array[0..n] because then the indices are = to the ones in the partial plan, where array[0]=a_0
    (do ((j 1 (1+ j)))
	((> j number-actions))
      (if (> j 1)
	  (setq state (or (give-me-nice-sayphi-state (nth (- j 2) solution))
			  (nth (- j 2) states-list))))
      (dolist (link links)
	(if (= j (nth 2 link))
	    (if (<= (nth 0 link) j)
		(dolist (precond (intersection (nth 1 (aref actions j)) state :test #'equal))
		  (pushnew precond (aref goal-regression-array j) :test #'equal))
		(dolist (goal-regression (aref goal-regression-array (nth 0 link)))
		  (pushnew goal-regression (aref goal-regression-array j) :test #'equal))))))
    ;;    (print goal-regression-array)
    (setq state (problem-lit-init-state *current-problem*))
    (do* ((i 1 (1+ i)))
	 ((> i number-actions))
      (if (> i 1)
	  (setq state (or (give-me-nice-sayphi-state (nth (- i 2) solution))
			  (nth (- i 2) states-list))))
      (setf (aref goal-regression-array i) (nth 1 (aref (partial-plan-actions partial-plan) i)))
      (dolist (dependent-action (aref (partial-plan-dependent-actions partial-plan) i))
	(dolist (literal (aref goal-regression-array dependent-action))
	  ;; note that if solutions come from lists (no sayphi nodes), then we do not augment the weakest preconds with the weakest preconds of the following actions
	  ;; at some point, I will recompute a state starting in the initial state of the problem and progressing it through the actions on the solution
	  (if (member literal state :test #'equal)
	      (pushnew literal (aref goal-regression-array i) :test #'equal)))))
    goal-regression-array))

;;; This code computes the goal regression for backward-chaining planners
;;; This is good for nothing when learning control rules for forward-chaining
;;; planners, given that the goal-regression of the state before applying the
;;; operator cannot be checked then at run time in other problems
;; (defun compute-goal-regression (partial-plan)
;;   (let* ((actions (partial-plan-actions partial-plan))
;; 	 (number-actions (- (car (array-dimensions actions)) 2))
;; 	 (goal-regression-array (make-array (list (1+ number-actions)) :initial-element nil))
;; 	 (links (partial-plan-links partial-plan)))
;;     ;; I use array[1..n] instead of array[0..n] because then the indices are = to the ones in the partial plan, where array[0]=a_0
;;     (dotimes (i number-actions)
;; 	(dolist (link links)
;; 	    (if (= (1+ i) (nth 2 link))
;; 		(if (= (nth 0 link) 0)
;; 		    (pushnew (nth 1 link) (aref goal-regression-array (1+ i)) :test #'equal)
;; 		    (dolist (goal-regression (aref goal-regression-array (1+ (nth 0 link))))
;; 			(pushnew goal-regression (aref goal-regression-array (1+ i)) :test #'equal))))))
;;     goal-regression-array))

;; Takes a partial plan structure and returns the transitive closure of the orderings relation
(defun compute-ordering-closure (partial-plan)
  (nwarshall-boolean (copy-array (partial-plan-orderings partial-plan))))

(defun remove-static-preds (goal-regression)
  (dotimes (i (length goal-regression))
    (setf (elt goal-regression i)
	  (remove-if-not #'(lambda (goal) (or (pred-true-able (car goal)) (pred-false-able (car goal))))
			  (elt goal-regression i))))
  goal-regression)

(defun vars-into-constants nil
;;  (format t "~%Substitution: ~a" *substitution*)
  (dolist (bind-pair *substitution*)
    (setf (cdr bind-pair) (intern (from-var-to-cte (cdr bind-pair))))))

;;; ************************************************************************
;;;    Generic code for building a partial plan out of a total-order plan
;;; ************************************************************************

;; Solution is either a list of operators lists (as coming from the execution of Metric-FF, or a list of sayphi search nodes)
(defun build-partial-plan (solution)
  (let* ((solution-length (length solution))
	 (partial-plan (generate-initial-partial-plan solution-length))
	 (i solution-length))
    (generate-a0-op)
    (dolist (plan-step (reverse solution))
      (setq partial-plan (add-action-to-partial-plan (if (snode-p plan-step)
							 (snode-plan-action plan-step)
							 plan-step)
						     partial-plan i (1+ solution-length)))
      (decf i))
    (setq partial-plan (add-action-to-partial-plan (list 'a0) partial-plan i (1+ solution-length)))
    (pop (dom-actions *pspace*))
    partial-plan))

(defun generate-a0-op nil
  (push (make-action :name 'a0
		     :parameters nil
		     :preconditions nil
		     :adds (pp-state (problem-init-state *current-problem*) 'list)
		     :dels nil)
	(dom-actions *pspace*)))
		     
;; I use the previous node to the end because I found out that the last node does not
;; have goals. Also, this will pose problems in 1 operator plans
(defun generate-initial-partial-plan (number-ops)
  (let ((actions (make-array (list (+ 2 number-ops)))))
    (setf (aref actions 0) (list 'a0 nil (action-adds (car (dom-actions *pspace*))) nil))
    (setf (aref actions (1+ number-ops)) (list 'ainf))
    (make-partial-plan
     :goals (mapcar #'(lambda (goal)
			(cons (1+ number-ops) goal))
		    (pp-state (problem-goals *current-problem*) 'list))
     :end-goals-achieved (make-array (list (1+ number-ops)) :initial-element nil)
     :dependent-actions (make-array (list (1+ number-ops)) :initial-element nil)
     :links nil
     :actions actions
     :orderings (make-array (list (+ 2 number-ops) (+ 2 number-ops)) :initial-element nil :element-type 'boolean))))
;; this is the generic algorithm. We do not really need this and it would be confusing for computing goal-regression
;; (cons :orderings (list (cons 0 (1+ number-ops)))))))

;; It adds an action to a partial-plan
(defun add-action-to-partial-plan (action partial-plan action-id ainf-id)
  (let* ((goals (partial-plan-goals partial-plan))
	 (actions (partial-plan-actions partial-plan))
	 (goals-achieved (partial-plan-end-goals-achieved partial-plan))
	 (action-name (car action))
	 (action-struct (find action-name (dom-actions *pspace*) :key #'action-name))
	 (substitution (mapcar #'cons (action-parameters action-struct) (cdr action)))
	 (preconds (sublis substitution (action-preconditions action-struct) :test #'equal))
	 (adds (sublis substitution (action-adds action-struct) :test #'equal))
	 (dels (sublis substitution (action-dels action-struct) :test #'equal))
	 (add-intersect (intersection goals adds :test #'(lambda (goal-pair add) (equal (cdr goal-pair) add))))
	 (del-intersect (intersection goals adds :test #'(lambda (goal-pair add) (equal (cdr goal-pair) add)))))

    (setf (aref (partial-plan-actions partial-plan) action-id)
	  (list action preconds adds dels))

    ; I would have to do something similar for dels
    (dolist (literal-pair add-intersect)
      (dolist (goal-pair (remove-if-not #'(lambda (goal-pair)
					    (equal (cdr literal-pair) (cdr goal-pair)))
					goals))
	(setf (aref (partial-plan-orderings partial-plan) action-id (car goal-pair)) t)
	;;	    (pushnew (cons action-id (car goal-pair)) (partial-plan-orderings partial-plan) :test #'equal)
	(if (and (> action-id 0) (< action-id ainf-id))
	    (cond ((= (car goal-pair) ainf-id)
		   (pushnew (cdr literal-pair) (aref (partial-plan-end-goals-achieved partial-plan) action-id) :test #'equal))
		  (t (pushnew (car goal-pair) (aref (partial-plan-dependent-actions partial-plan) action-id))
		     (dolist (dependent-action (aref (partial-plan-dependent-actions partial-plan) (car goal-pair)))
		       (pushnew dependent-action (aref (partial-plan-dependent-actions partial-plan) action-id)))
		     (dolist (goal-achieved (aref goals-achieved (car goal-pair)))
		       (pushnew goal-achieved (aref (partial-plan-end-goals-achieved partial-plan) action-id) :test #'equal)))))
	(pushnew (list action-id (cdr literal-pair) (car goal-pair))
		 (partial-plan-links partial-plan)
		 :test #'equal))
      (setf (partial-plan-goals partial-plan)
	    (remove literal-pair (partial-plan-goals partial-plan) :test #'equal)))
    (dolist (precond preconds)
	(push (cons action-id precond)
	      (partial-plan-goals partial-plan)))
; threats
    (dolist (del dels)
      (dolist (link (partial-plan-links partial-plan))
	(if (equal del (cadr link))
	    (setf (aref (partial-plan-orderings partial-plan) action-id (car link)) t))))
    ;; 		(pushnew (cons action-id (car link))
    ;; 			 (partial-plan-orderings partial-plan)
    ;; 			 :test #'equal)
					; and more threats
    (do* ((i (1+ action-id) (1+ i))
	  (action-i (aref actions i) (aref actions i)))
	((>= i ainf-id))
      (if (intersection preconds (nth 3 action-i) :test #'equal)
	  (setf (aref (partial-plan-orderings partial-plan) action-id i) t)))
;; 	  (pushnew (cons action-id i)
;; 		   (partial-plan-orderings partial-plan)
;; 		   :test #'equal)

    ;; this is the generic algorithm. We do not really need this and it would be confusing for computing goal-regression
    ;;     (push (cons action-id ainf-id) (partial-plan-orderings partial-plan))
    ;;     (push (cons 0 action-id) (partial-plan-orderings partial-plan))

    partial-plan))

;; SFA April 2013. Computation of the parallel plan 
;; A partir de las relaciones de orden entre las acciones se calcula el plan paralelo:
;; En los ordenes hay dos acciones ficticias la 0 y la de fin
;; opar: lista de pares (a1 a2), a1 se ejecuta despues de a2
;; time: lista de pares (a t), tiempo minimo en que se puede ejecutar 'a' en el plan paralelo
(defun build-parallel-plan (&optional (solution *say-solution*))
   (let* ((path  (if (solution-p solution) (solution-path solution) solution))
	  (plan  (if (solution-p solution) (mapcar #'(lambda(x) (gaction-planaction (snode-applied-action x))) path) solution))
	  (pp    (build-partial-plan path))
          (orderings (partial-plan-orderings pp))
	  (len (car (array-dimensions orderings)))
          (opar nil)
	  (time '((0 0)))
	  )
     ;;cada par representa que la primera accion del par se ejecuta despues de la segunda
     (when orderings
       (dotimes (a1 len)
	  (dotimes (a2 len)
	     (if (aref orderings a1 a2 ) (setf opar (append opar (list (list a2 a1)))))))

      ;;el tiempo de la accion 0 es 0, empezamos por la accion 1 
      (dotimes (a- (- len 1) time)
         ;;(format t "~% time=~a~%" time)
         (let* ((a (+ 1 a-))
	       ;;obtiene la lista de pares cuyo primer elemento es 'a', es decir, todas las acciones que se tienen que ejecutar antes que 'a' 
	       (aux  (remove-if-not #'(lambda(x) (eq (car x) a)) opar))
	       ;;para la lista anterior, busca el tiempo en que se ejecuta cada accion en la lista time
	       (aux2 (remove-if #'null (mapcar #'(lambda(x) (second (find (second x) time :key #'car))) aux)))
	       ;;calcula el maximo de los tiempos anteriores
	       (max (if aux2 (apply #'max aux2) most-positive-fixnum))
	       )
	   ;;el tiempo de la accion sera una unidad mas que el maximo anterior
	   (setq time (append time (list (list a (+ 1 max)))))))

      ;;(format t "~% time=~a~%" time)
    
       ;;Para calcular el plan paralelo hay que restar 1 a las acciones y al tiempo de los valores de time
       ;; y hay que ordenar time por el tiempo (segundo elemento de cada par de time)
       (mapcar #'(lambda(x) (list (- (second x) 1) (nth (- (car x) 1) plan)))
	    (sort (copy-list (butlast (cdr time))) #'< :key #'second))
       )
      
    ))
     
;;;; *******************************************************************************
;;;;         Connection with Rete
;;;; *******************************************************************************

(defun load-rules-build-rete (&optional (rules-file (concatenate 'string *domain-dir* "rules.lisp")))
  (let ((rules nil))
    (when (probe-file rules-file)
      (with-open-file (ifile rules-file :direction :input)
	(do* ((rule (read ifile nil 'eof) (read ifile nil 'eof)))
	    ((eq rule 'eof))
	  (if (eq (car rule) 'progn)
	      (setq rules (append (cdr rule) rules))
	      (push rule rules))))
      (clear-rete)
      (setf *sayphi-rules* (reverse rules))
      (setf *cr-list* (reverse rules))
      (build-rete *sayphi-rules*
		  :domain *domain-file*
		  :problem (concatenate 'string *domain-dir* *problem-dir* *complete-problem-file*)))))


(defun push-to-rete-state (node initp)
  (let ((goals (problem-lit-goals *current-problem*)))
    (setf *current-node* node)
    (cond (initp
	   (dolist (literal (problem-lit-init-state *current-problem*))
	     (add-state-token literal goals t)))
	  (t (let* ((action (snode-plan-action node))
		    (action-struct (find (car action) (dom-actions *pspace*) :key #'action-name))
		    (substitution (mapcar #'cons (action-parameters action-struct) (cdr action))))
	       (dolist (literal (sublis substitution (action-dels action-struct) :test #'equal))
		 (add-state-token literal goals nil))
	       (dolist (literal (sublis substitution (action-adds action-struct) :test #'equal))
		 (add-state-token literal goals t)))))))

(defun add-state-token (literal goals positivep)
  (if *trace-exec-rete*
      (format t "~2%~a state literal: ~a" (if positivep "Adding" "Deleting") literal))
  (add-token (make-token :type (if positivep 'add 'del)
			 :contents `(true-in-state ,literal))
	     *rete-root-node* nil)
  (if (member literal goals :test #'equal)
      (add-token (make-token :type (if positivep 'del 'add)
			     :contents `(target-goal ,literal))
		 *rete-root-node* nil)))

;; Very inefficient. It has to be done through the adds and dels of the action.
;; (defun push-to-rete-state (previous-state state)
;;   (let ((adds nil)
;; 	(dels nil)
;; 	(in-both nil))
;;     (dolist (literal state)
;; 	(if (member literal previous-state :test #'equal)
;; 	    (push literal in-both)
;; 	    (push literal adds)))
;;     (if *trace-rete*
;; 	(format t "~2%Previous state: ~a~%Next state: ~a~%Adds: ~a~%Dels: ~a" previous-state state
;; 		adds (set-difference previous-state in-both :test #'equal)))
;;     (dolist (literal (set-difference previous-state in-both :test #'equal))
;; 	(if *trace-rete*
;; 	    (format t "~2%Deleting state literal: ~a" literal))
;;       (add-token `(del (true-in-state ,literal)) *rete-root-node* nil))
;;     (dolist (literal adds)
;; 	(if *trace-rete*
;; 	    (format t "~2%Adding state literal: ~a" literal))
;;       (add-token `(add (true-in-state ,literal)) *rete-root-node* nil))))

;; (defun push-to-rete (node initp addp)
;;   (if initp
;;       (dolist (literal (pp-state (target-goals node) 'list))
;; 	  (add-token `(add (target-goal ,literal)) *rete-root-node* nil)))
;;   (if *trace-rete*
;;       (format t "~2%~a state: ~a" (if addp "Adding" "Deleting") (pp-state (snode-state node) 'list)))
;;   (dolist (literal (pp-state (snode-state node) 'list))
;;       (add-token `(,(if addp 'add 'del) (true-in-state ,literal)) *rete-root-node* nil)))

(defun push-to-rete-goals nil
  (dolist (literal (problem-lit-goals *current-problem*))
    (if *trace-exec-rete*
	(format t "~2%Adding goal: ~a" literal))
    (add-token (make-token :type 'add :contents `(target-goal ,literal))
	       *rete-root-node* nil)))
;;   (dolist (literal (pp-state (target-goals node) 'list))

(defun push-to-rete-object-types nil
  (maphash #'(lambda (type objects)
	       (dolist (object objects)
		   (if *trace-exec-rete*
		       (format t "~2%Adding type-of-object type ~a, object ~a" type object))
		 (add-token (make-token :type 'add :contents `(type-of-object ,object ,type)) *rete-root-node* nil)))
	   (problem-objects *current-problem*)))

;;;; *******************************************************************************
;;;;         Decision mechanism using rules
;;;; *******************************************************************************

(defun prune-by-rules (node)
  (let* ((rule-recommendations (mapcar #'(lambda (x) (nth 3 x)) *conflict-set*))
	 (pruned-children (remove-if-not #'(lambda (child)
					     (if *trace-pruning*
						 (format t "~%Action in child: ~a" (snode-plan-action child)))
					     (member (snode-plan-action child) rule-recommendations :test #'equal))
					 (snode-children node))))
    (if *trace-pruning*
	(format t "~2%Rules recommendations: ~a~% resulting in the following children: ~a" rule-recommendations
		(mapcar #'(lambda (child) (snode-plan-action child)) pruned-children)))
    (if pruned-children
	(setf (snode-children node) pruned-children))))

;;;; *******************************************************************************
;;;;         Auxiliary functions
;;;; *******************************************************************************

(defun give-me-nice-sayphi-goals (node)
  (if (snode-p node)
      (pp-state (target-goals node) 'list)))

(defun give-me-nice-sayphi-state (node)
  (if (snode-p node)
      (pp-state (snode-state node) 'list)))

(defun generate-new-state (state action-list)
  (let* ((dels (nth 3 action-list))
	 (new-state (remove-if #'(lambda (literal) (member literal dels :test #'equal))
			       state)))
    (dolist (add (nth 2 action-list))
      (pushnew add new-state :test #'equal))
    new-state))

;; Returns a list with solution
(defun pp-solution-sayphi (&optional (sol *say-solution*) (copyp nil))
  (if sol
      (let ((solution nil))
	(dolist (inode (solution-path sol))
	  (push (if copyp
		    (copy-list (snode-plan-action inode))
		    (snode-plan-action inode))
		solution))
	(nreverse solution))))

(defun free-mem nil
  (when *print-all-mem*
    (format t "~%Before~%")
    (room nil))
  (say-mem-free-pointers)
  #+SBCL (sb-ext:gc :full t)
  (when *print-all-mem*
    (format t "~%After~%")
    (room nil)))

(defun say-mem-free-pointers()
  (setf *h-achieved-goals* nil)
  (setf *say-hash-solutions* nil)
  (setf *say-solution* nil)
  (setf *partial-solution* nil)
  (setf *hash-nodes* nil)
  (setf *hash-nodes* (make-hash-table))
  (setf *hash-duplicates* nil)
  (setf *hash-inconsistent-h* nil)
  
  (setf *h-active-facts* nil)
  (setf *h-active-flpatterns* nil)
  (setf *h-active-fluents* nil)
  (setf *h-achieved-facts* nil)
  (setf *h-achieved-fluents* nil)
  (setf *h-active-actions* nil)
  (setf *actions-table* nil)
  (setf *action-layers* nil)
  (setf *achieved-layers* nil)
  (setf *h-achieved-layers* nil)
  (setf *focus-goals* nil)
  
  (setf *actions* nil)
  (setf *factgraph-table* nil)
  (setf *fluentgraph-table* nil)

  (setf *facts* nil)
  (setf *init-facts* nil)
  (setf *fluents* nil)
  (setf *init-fluents* nil)
  (if (and *current-problem* (problem-search-tree *current-problem*))
      (setf (problem-search-tree *current-problem*) nil))
  (setf *current-problem* nil)

)

;;;; *******************************************************************************
;;;;         Functions for matching Tilde decision trees
;;;; *******************************************************************************

;; decision-trees-dir must include the tilde subdir
;; problems require to have a -type.pddl ending
(defun ebl-sayphi-test (&key (decision-trees-dir nil) (domain "blocksworld-ipc") (domain-file "blocksworld-typed.pddl") (algorithm 'ehc-tilde)
			     (probs-prefix "bw*") (probs-sufix "-typed.pddl")
			     (timeout 30) (p 0.4) (deterministicp nil) (use-rules-p t) (out-file nil) (problems-dir "target/")
			     (knowledge-file nil) (execute-sayphi-alone-p nil) (output-level 0))
  (if (not decision-trees-dir) (setq decision-trees-dir (concatenate 'string *domain-dir* "rules/")))
  (setf *print-reuse* nil)
  (say-domain domain domain-file)
  (if (not knowledge-file) (setq knowledge-file (concatenate 'string decision-trees-dir "tilde-decision-tree.lisp")))
  (let ((result-file (init-results-files out-file "" (string-right-trim '(#\*) probs-prefix)
					 `((timeout ,timeout) (p ,p) (deterministicp ,deterministicp) (use-rules-p ,use-rules-p))))
	(decision-trees nil)
	(i 0)
	(this-sol nil)
	(init-time 0))
    (with-open-file (istream knowledge-file :direction :input)
      (setq decision-trees (read istream)))
    (setf *actions-tilde-tree* (parse-btree-for-rules (from-tilde-list-to-btree (cdr (assoc 'actions decision-trees))) 'actions))
    (setf *bindings-tilde-trees* nil)
    (dolist (action (get-all-operators))
      (push (cons action (parse-btree-for-rules (from-tilde-list-to-btree (cdr (assoc action decision-trees))) action))
	    *bindings-tilde-trees*))
;;       (push (cons action (parse-btree-for-rules (btree-load-from-tilde-file (format nil "~a~(bindings-~a/tilde/~a-select-bindings-~a~).out" decision-trees-dir action domain action))))
    (setf *say-output* output-level)
    (setf *problem-dir* problems-dir)
    (dolist (prob (dirfiles-sorted (format nil "~a~a" *domain-dir* *problem-dir*) (concatenate 'string probs-prefix probs-sufix)))
      (incf i)
      (format t "~% Solving problem ~a with ~a" (pathname-name prob) algorithm)
      (read-pddl-problem prob)
      (setq init-time (get-internal-run-time))
      (loop do (setq this-sol (plan :algorithm algorithm :timeout (- timeout (elapsed-time init-time))
				    :search-options (list (list :p p) (list :deterministicp deterministicp) (list :use-rules-p use-rules-p))))
	 until (or (> (elapsed-time init-time) timeout) (and this-sol (or (eq (solution-stop-reason this-sol) :goals-reached) (eq (solution-stop-reason this-sol) :time-bound)))))
      (if (and this-sol (eq (solution-stop-reason this-sol) :goals-reached))
	  (with-open-file (ostream (concatenate 'string *domain-dir* (pathname-name prob) ".soln") :direction :output :if-exists :supersede :if-does-not-exist :create)
	    (say-pp-solution this-sol nil ostream t)))
      (print-result-file t result-file i prob algorithm this-sol 0 p)
      (setq this-sol nil)
      (free-mem)
      (when execute-sayphi-alone-p
	(read-pddl-problem prob)
	(setq this-sol (plan :algorithm 'enforced-hill-climbing :timeout timeout :search-options (list (list :deterministicp t) (list :use-rules-p nil))))
	(print-result-file t result-file i prob 'enforced-hill-climbing this-sol 0 0)))
    (if (eq *say-solution-format* :list)
	(with-open-file (out-stream result-file :direction :output :if-exists :append :if-does-not-exist :create)
	  (format out-stream ")")))))

(defun from-tilde-list-to-btree (list)
  (let ((btree (btree-load-from-list list)))
;;    (setf (btree-time-stamp btree) time-stamp)
    btree))

(defun parse-btree-for-rules (btree action)
  (cond ((btree-p btree)
	 (setf (btree-tag btree)
	       (if (btree-tag btree)
		   (translate-tree-test (btree-tag btree))
		   (if (eq action 'actions)
		       `(then select operator <b>)
		       `(then select instantiated-operator (,action ,@(mapcar #'(lambda (arg) (intern (format nil "~@(<~a>~)" arg)))
									      (mapcar #'car (action-parameters (find-if #'(lambda (dom-action) (eq (action-name dom-action) action))
															(dom-actions *pspace*))))))))))
	 (parse-btree-for-rules (btree-first-node btree) action)
	 btree)
	((bnode-leaf-p btree))
	(t (setf (rtest-tests (bnode-element btree))
		 (mapcar #'(lambda (test) (translate-tree-test test))
			 (rtest-tests (bnode-element btree))))
	   (parse-btree-for-rules (bnode-yes-node btree) action)
	   (parse-btree-for-rules (bnode-no-node btree) action))))

(defun translate-tree-test (tag)
  (if tag
      (let ((matching-closure-meta-predicate (cl-ppcre:create-scanner "(true_in_state_|target_goal_|type_of_object_|select_action|select_bindings_)([_a-zA-Z0-9]*)"
								      :case-insensitive-mode t)))
	(multiple-value-bind (result array)
	    (cl-ppcre:scan-to-strings matching-closure-meta-predicate (format nil "~a" (car tag)))
	  (case (read-from-string (aref array 0))
	    (select_action `(then select operator ,@(translate-tree-constants (cddr tag))))
	    (select_bindings_ `(then select instantiated-operator (,(read-from-string (remove-underscore (aref array 1))) ,@(translate-tree-constants (cddr (butlast tag))))))
	    (true_in_state_ `(true-in-state (,(read-from-string (remove-underscore (aref array 1))) ,@(translate-tree-constants (cddr tag)))))
	    (target_goal_ `(target-goal (,(read-from-string (remove-underscore (aref array 1))) ,@(translate-tree-constants (cddr tag)))))
	    (type_of_object_ `(type-of-object (,@(translate-tree-constants (cddr tag)) ,(read-from-string (remove-underscore (aref array 1))))))
	    (otherwise nil))))))

(defun translate-tree-constants (args)
  (mapcar #'(lambda (arg) (intern (format nil "~@(<~a>~)" (remove-underscore arg))))
	  args))

;; Interface to making decision-trees recommendations
(defun prune-by-tilde-tree (node p deterministicp)
  (cond ((> (random 1.0) p)
	 (let* ((state (give-me-nice-sayphi-state node))
		(goals (give-me-nice-sayphi-goals node))
		(children (snode-children node))
		(actions nil))
	   (dolist (child children)
	     (pushnew (car (snode-plan-action child)) actions))
	   (if *trace-pruning* (format t "~2%Rules recommendations in node ~d ~a:~% starting with ~a" (snode-number node) (snode-plan-action node)
				       (mapcar #'(lambda (child) (snode-plan-action child)) children)))
	   (if deterministicp
	       (multiple-value-bind (action-recommendation action-votes)
		   (select-action-from-decision-node actions (match-tilde-tree state goals (btree-first-node *actions-tilde-tree*) (list nil)) t)
		 (multiple-value-bind (bindings-recommendation bindings-votes)
		     (select-bindings-from-decision-node state goals action-recommendation children t)
		   (let ((pruned-children (remove-if-not #'(lambda (child)
							     (if *trace-pruning* (format t "~%Action in child: ~a" (snode-plan-action child)))
							     (member (snode-plan-action child) bindings-recommendation :test #'equal))
							 children)))
		     (if *trace-pruning* (format t "~%  recommendation: ~a~%  resulting in the following: ~a" bindings-recommendation
						 (mapcar #'(lambda (child) (snode-plan-action child)) pruned-children)))
		     (if pruned-children
			 (setf (snode-children node) pruned-children)))))
	       ;; Here we have to do stochastic things with the votes
	       (let* ((action-recommendation (stochastic-choice (select-action-from-decision-node actions (match-tilde-tree state goals (btree-first-node *actions-tilde-tree*) (list nil))
												  nil)))
		      (bindings-recommendation (if action-recommendation (stochastic-choice (select-bindings-from-decision-node state goals action-recommendation children nil)))))
		 (if bindings-recommendation
		     ;; this implements a stochastic policy in fact
		     (let ((pruned-children (find-if #'(lambda (child)
							 (if *trace-pruning* (format t "~%Action in child: ~a" (snode-plan-action child)))
							 (equal (snode-plan-action child) bindings-recommendation))
						     children)))
		       (if *trace-pruning* (format t "~%  recommendation: ~a~%  resulting in the following: ~a" bindings-recommendation
						   (mapcar #'(lambda (child) (snode-plan-action child)) (list pruned-children))))
		       ;; Here we have to do stochastic things with the votes
		       (if pruned-children
			   (setf (snode-children node) (list pruned-children))))
		     (if *trace-pruning* (format t "~2%No recommendation on node ~d ~a" (snode-number node) (snode-plan-action node))))))))
	(t (if *trace-pruning* (format t "~2%No recommendation on node ~d ~a" (snode-number node) (snode-plan-action node)))
	   (snode-children node))))

(defun match-tilde-tree (state goals btree bindings)
  (cond ((bnode-leaf-p btree) (mapcar #'(lambda (binding) (cons (rleaf-decissions (bnode-element btree)) binding)) bindings))
	(t (let ((unification (match-tilde-test state goals (rtest-tests (bnode-element btree)) bindings)))
	     (cond ((eq unification 'fail)
		    (match-tilde-tree state goals (bnode-no-node btree) bindings))
		   ((null unification) (match-tilde-tree state goals (bnode-yes-node btree) bindings))
		   (t (mapcan #'(lambda (binding)
				  (match-tilde-tree state goals (bnode-yes-node btree) (list binding)))
			      unification)))))))

;; I assume for now that there is only one test in each branch of the tree
(defun match-tilde-test (state goals node-tests bindings)
  (mapcan #'(lambda (binding)
	      (let ((unification (match-single-test state goals (sublis binding (car node-tests)))))
		(cond ((eq unification 'fail) nil)
		      ((null unification) (list binding))
		      (t (mapcar #'(lambda (uni-binding) (append binding uni-binding)) unification)))))
	  (or bindings (list bindings))))

(defun match-single-test (state goals test)
  (case (car test)
    ((true-in-state target-goal)
     (let ((bindings (mapcan #'(lambda (literal)
				 (let ((unification (unify-any-depth literal (cadr test))))
				   (if (not (eq unification 'fail))
				       (list unification))))
			     (if (eq (car test) 'true-in-state) state goals))))
       (or bindings 'fail)))
    (type-of-object (if (not (p4::is-variable-p (cadr test)))
			(or (member (cadr test) (gethash (caddr test) (problem-objects *current-problem*)))
			    'fail)))
    (otherwise nil)))

;; decisions are (((action1 votes1) (action2 votes2)...) (var1 . cte1) (var2 . cte2) ...)
(defun select-action-from-decision-node (potential-actions decisions deterministicp)
  (let ((actions (mapcar #'(lambda (action) (cons action 0)) potential-actions))
	(max-action nil)
	(max-value 0))
    (dolist (decision decisions)
      (dolist (action (car decision))
	(if (assoc (car action) actions)
	    (incf (cdr (assoc (car action) actions)) (cadr action)))))
    (cond (deterministicp
	   (dolist (action actions)
	     (when (> (cdr action) max-value)
	       (setq max-action (car action))
	       (setq max-value (cdr action))))
	   (values max-action max-value))
	  (t (sort actions #'> :key #'cdr)))))

(defun select-bindings-from-decision-node (state goals action-recommendation children deterministicp)
  (let* ((bindings (mapcan #'(lambda (node)
			       (if (eq (car (snode-plan-action node)) action-recommendation)
				   (list (cons (snode-plan-action node) 0))))
			   children))
	 (btree (cdr (assoc action-recommendation *bindings-tilde-trees*)))
	 (action-tag (nth 3 (btree-tag btree)))
	 (max-binding nil)
	 (max-value 0))
    (dolist (binding bindings)
      (dolist (decision-binding (match-tilde-tree state goals (btree-first-node btree) (list (unify-any-depth (car binding) action-tag))))
	(setq instantiated-action (sublis (cdr decision-binding) action-tag))
	(dolist (decision (car decision-binding))
	  (if (eq (car decision) 'selected)
	      (incf (cdr (assoc instantiated-action bindings :test #'equal)) (cadr decision))
	      (decf (cdr (assoc instantiated-action bindings :test #'equal)) (cadr decision))))))
    (cond (deterministicp
	   (dolist (binding bindings)
	     (when (> (cdr binding) max-value)
	       (setq max-binding (car binding))
	       (setq max-value (cdr binding))))
	   (values max-binding max-value))
	  (t (sort bindings #'> :key #'cdr)))))

(defun stochastic-choice (decisions-votes)
  (let ((sum-votes 0)
	(real-decisions nil))
    (dolist (decision-votes decisions-votes)
      (cond ((>= (cdr decision-votes) 0)
	     (push decision-votes real-decisions)
	     (incf sum-votes (cdr decision-votes)))
	    (t nil)))
    (if real-decisions
	(if (> sum-votes 0)
	    (do* ((random (random sum-votes))
		  (decisions real-decisions (cdr decisions))
		  (acc (cdar decisions) (if decisions (+ acc (cdar decisions)))))
		((>= acc random) (caar decisions)))
	    (car (choose-one real-decisions))))))
   
(defun print-btree (btree &optional (stream t) (z 0))
  (cond ((btree-p btree)
	 (print-btree (btree-first-node btree)))
	((bnode-leaf-p btree) (format stream (format nil "~~%~~~dt~~a" z) (rleaf-decissions (bnode-element btree))))
	(t (format stream  (format nil "~~%~~~dt~~a~~%~~~dtYES:" z z) (rtest-tests (bnode-element btree)))
	   (print-btree (bnode-yes-node btree) stream (+ z 2))
	   (format stream  (format nil "~~%~~~dtNO:" z))
	   (print-btree (bnode-no-node btree) stream (+ z 2)))))

