;; =========================================================================    
;;  (C) Copyright 2006, 2008 
;;      Universidad Carlos III de Madrid
;;      Planning & Learning Group (PLG)
;; 
;; =========================================================================
;; 
;; This file is part of SAYPHI
;; 
;; 
;; SAYPHI is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; SAYPHI is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
;; ========================================================================
;; Author: Daniel Borrajo
;; Description:
;; Date: 2006.07.17
;; 
;; ========================================================================

;; (load "(/home/dborrajo/planning/ipss/hamlet")

(in-package "COMMON-LISP-USER")

(defstruct partial-plan
  (goals nil)
  (end-goals-achieved nil)
  (dependent-actions nil)
  (links nil)
  (actions nil)
  (orderings nil))

;; not needed for now
;; (defvar *examples* nil)
(defvar *generate-learning-examples-p* t)
(defvar *trace-pruning* nil)

;; It generates control rules from the solution path of a solved problem
;; If rules-file is non-nil, it will write the rules on that file
;; better-heuristic-p determines whether rules are generated only from nodes
;;                    improving the h value or from the whole path
;; only-from-plateaus learns only from plateus decisions
;; format can be either: prodigy (default,control rules in prodigy format), or
;;                       example (for learning through other ILP systems)
;; if rules are given, then the new ones will be added to the previous ones
(defun generate-rules-from-solution (&key (rules-file (concatenate 'string *domain-dir* "rules.lisp"))
					  (rules nil)
					  (say-solution *say-solution*)
					  (better-heuristic-p nil)
					  (only-from-plateaus nil)
					  (remove-preconds-from-rule nil)
					  (format 'prodigy))
  (when (and (solution-p say-solution)
	     (solution-found say-solution))
    (do* ((solution (solution-path say-solution) (cdr solution))
	  (partial-plan (build-partial-plan solution))
	  (i 1 (1+ i))
	  (goal-regression (compute-goal-regression partial-plan solution))
	  (inode (car solution) (car solution)))
	((null solution))
      (when (or (and better-heuristic-p
		     (< (snode-h-value inode) (snode-h-value (snode-parent inode))))
		(and only-from-plateaus
		     (>= (snode-h-value inode) (snode-h-value (snode-parent inode))))
		(and (not better-heuristic-p) (not only-from-plateaus)))
	(pushnew (generate-rule (snode-parent inode) inode (aref goal-regression i)
				(aref (partial-plan-end-goals-achieved partial-plan) i)
				(nth 1 (aref (partial-plan-actions partial-plan) i))
				remove-preconds-from-rule i format)
		 rules :test #'unify-any-depth-p :key #'cddr)))
    (if rules-file
	(with-open-file (ofile rules-file :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (dolist (cr rules)
	    (if (eq format 'prodigy)
		(pp-cr (nth 1 cr) (cdadr (nth 2 cr)) (nth 3 cr) ofile)
		(pp-list (list cr) 1 ofile)))
	  (terpri ofile)))
    rules))

(defun generate-rule (parent node regressed-state end-goals preconds remove-preconds-from-rule rule-number format)
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
	    (effects `(select instantiated-operator ,(snode-plan-action node))))
	(setf *new-vars-format* 'sayphi)
	(setf *rule-id* (intern (format nil (string-trim "#:G" (gensym)))))
	(setf *problem-name* (problem-name *current-problem*))
	(setf *substitution* nil)
	(find-substitution state)
	;;	(format t "~%State: ~a~%Substitution: ~a" state *substitution*)
	(find-substitution goals)
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
	      (push (parametrize suitable-precond) state))
	    (setq vars-in-effect (parametrize vars-in-effect))))
	(setq effects (parametrize effects))
	(setq types (mapcar #'(lambda (bind-pair)
				(list 'type-of-object (cdr bind-pair) (get-object-type (car bind-pair))))
			    *substitution*))
	(setq state (sort-preconds-by-frequency state (compute-predicates-frequency)))
	;; Avoids generating duplicate (that can be unified) rules
	(if (eq format 'prodigy)
	    `(control-rule ,(name-cr effects
				     (if (eq (cadr effects) 'bindings)
					 (cadar state)))
			   (if (and ,@goals ,@state ,@types))
			   (then ,@effects))
	    `(,rule-number t (,@goals ,@state ,@types) ,effects)))))
;; 	      (setq bind-pair (cons var-in-effect (new-var-name var-in-effect)))
;; 	    (push bind-pair *substitution*)
;; 	    (format t "~%Trying to add a precond from ~a~% to the state ~a~% because free var ~a~% in effect ~a~% with substitution ~a"
;; 		    preconds state var-in-effect vars-in-effect *substitution*)

;; (intern (format nil "~:@(rule-~a~)" (string-trim "#:G"  (gensym))))
;;  (mapcar #'(lambda (literal)
;; 			       (list 'true-in-state literal))
;; 			   (give-me-nice-sayphi-state parent))
;; (give-me-nice-sayphi-goals parent)

(defun compute-goal-regression (partial-plan solution)
  (let* ((actions (partial-plan-actions partial-plan))
	 (number-actions (- (car (array-dimensions actions)) 2))
	 (goal-regression-array (make-array (list (1+ number-actions)) :initial-element nil))
	 (links (partial-plan-links partial-plan))
	 (dependent-actions nil)
	 (state (problem-lit-init-state *current-problem*)))
    ;; I use array[1..n] instead of array[0..n] because then the indices are = to the ones in the partial plan, where array[0]=a_0
    (do ((j 1 (1+ j)))
	((> j number-actions))
      (if (> j 1)
	  (setq state (give-me-nice-sayphi-state (nth (- j 2) solution))))
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
	  (setq state (give-me-nice-sayphi-state (nth (- i 2) solution))))
      (setf (aref goal-regression-array i) (nth 1 (aref (partial-plan-actions partial-plan) i)))
      (dolist (dependent-action (aref (partial-plan-dependent-actions partial-plan) i))
	(dolist (literal (aref goal-regression-array dependent-action))
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
    

;;; ************************************************************************
;;;    Generic code for building a partial plan out of a total-order plan
;;; ************************************************************************

;; Solution is either a list of operators lists (as coming from the execution of Metric-FF, or a list of sayphi search nodes)
(defun build-partial-plan (solution)
  (let* ((solution-length (length solution))
	 (partial-plan (generate-initial-partial-plan solution solution-length))
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
(defun generate-initial-partial-plan (solution number-ops)
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
     :orderings nil)))
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
	    (pushnew (cons action-id (car goal-pair)) (partial-plan-orderings partial-plan) :test #'equal)
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
		(pushnew (cons action-id (car link))
			 (partial-plan-orderings partial-plan)
			 :test #'equal))))
; and more threats
    (do* ((i (1+ action-id) (1+ i))
	  (action-i (aref actions i) (aref actions i)))
	((>= i ainf-id))
      (if (intersection preconds (nth 3 action-i) :test #'equal)
	  (pushnew (cons action-id i)
		   (partial-plan-orderings partial-plan)
		   :test #'equal)))

;; this is the generic algorithm. We do not really need this and it would be confusing for computing goal-regression
;;     (push (cons action-id ainf-id) (partial-plan-orderings partial-plan))
;;     (push (cons 0 action-id) (partial-plan-orderings partial-plan))

    partial-plan))

;;;; *******************************************************************************
;;;;         Connection with Rete
;;;; *******************************************************************************

(defun load-rules-build-rete (&optional (rules-file (concatenate 'string *domain-dir* "rules.lisp")))
  (let ((rules nil))
    (when (probe-file rules-file)
      (with-open-file (ifile rules-file :direction :input)
	(do* ((rule (read ifile nil 'eof) (read ifile nil 'eof)))
	    ((eq rule 'eof))
	  (push rule rules)))
      (clear-rete)
      (build-rete (reverse rules)
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
  (pp-state (target-goals node) 'list))

(defun give-me-nice-sayphi-state (node)
  (pp-state (snode-state node) 'list))


;; Returns a list with solution
(defun pp-solution-sayphi nil
  (let ((solution nil))
    (dolist (inode (solution-path *say-solution*))
	(push (snode-plan-action inode) solution))
    (nreverse solution)))
