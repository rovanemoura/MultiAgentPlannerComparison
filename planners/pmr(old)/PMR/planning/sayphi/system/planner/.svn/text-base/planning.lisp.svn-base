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
;; along with SAYPHI.  If not, see <http://www.gnu.org/licenses/>.
;; ========================================================================

;; Author: Tomas de la Rosa 
;; Description: Planning domain functions for search algorithms
;; Date: 2006.12.16
;; 
;; ========================================================================


;; Stores node pointer to acces them by number
(defvar *hash-nodes* (make-hash-table))
(defvar *hash-duplicates* nil)
(defvar *hash-inconsistent-h* nil)

(defparameter *applicable-byte* nil)


;;Duplicates the hash table containing the maps of literals
(defun copy-state (hash-state)
  (let ((new-state (make-hash-table :test #'eq)))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-state) (copy-seq value))) 
	     hash-state)
    new-state))

;;Copy only the sets of bitmaps or the set of arrays-fluents
(defun copy-state-set (hash-state &optional (set 'facts))
  (let ((new-state (make-hash-table :test #'eq)))
    (maphash #'(lambda (key value)
		 (cond ((eq set 'facts)
			(when (bit-vector-p value)
			  (setf (gethash key new-state) (copy-seq value))))
		       ((eq set 'fluents)
			(unless (bit-vector-p value)
			  (setf (gethash key new-state) (copy-seq value))))
		       ((eq set 'real-fluents)
			(when (or (and (not (bit-vector-p value))
				       (or (find key (dom-real-functors *pspace*))
					   (find key (problem-artificial-vars *current-problem*))))
				  (eq key 'total-time))
			  (setf (gethash key new-state) (copy-seq value))))
		       ))
			
	     hash-state)
    new-state))

(defun is-metric-domain ()
  (when (and (not (null *current-problem*))
	     (not (null (problem-metric *current-problem*)))
	     (find :fluents (dom-requirements *pspace*)))
	     t))




(defmacro is-relaxed-plan-member (relaxed-plan action)
  `(not (null (find ,action ,relaxed-plan :test #'equal :key #'car))))

;; It substitutes the snode slot after representation change
(defmacro snode-plan-action (node)
  `(when (gaction-p (snode-applied-action ,node))
    (gaction-planaction (snode-applied-action ,node))))



;; It computes the left goals of the current node, when we want to deal
;; only with not achieved goals 
(defun target-goals (node &optional (goals (problem-goals *current-problem*)))
  (let ((state (if (snode-p node) (snode-state node) node))
	(left-goals (make-hash-table)))
    (maphash #'(lambda (pred goal-bitmap)
		 (when (bit-vector-p goal-bitmap)
		   (setf (gethash pred left-goals)
			 (bit-andc2 goal-bitmap (gethash pred state)))))
	     goals)
    left-goals))


;; It maps a bitvector function between to hash-states (only predicates)
(defun negated-state (state1)
  (let ((res-state (make-hash-table)))
    (maphash #'(lambda (pred bitvector1)
		 (setf (gethash pred res-state)
		       (bit-not bitvector1)))
	     state1)
    res-state))

;; It maps a bitvector function between to hash-states (only predicates)
(defun map-bitvector-state (state1 state2 bit-function)
  (let ((res-state (make-hash-table)))
    (maphash #'(lambda (pred bitvector1)
		 (setf (gethash pred res-state)
		       (funcall bit-function bitvector1 (gethash pred state2))))
	     state1)
    res-state))


(defun sortedlist-goals-reached (&key (path (solution-path *say-solution*)))
  (let ((past-goals (map-bitvector-state (problem-goals *current-problem*) (problem-init-state *current-problem*) #'bit-and))
	(goals-in-state nil) (list-goals nil) (new-goals))
    (dolist (inode path list-goals)
      (setf goals-in-state (map-bitvector-state (problem-goals *current-problem*) (snode-state inode) #'bit-and))
      (setf new-goals (map-bitvector-state past-goals goals-in-state #'bit-andc1))
      (setf list-goals (append list-goals (pp-state new-goals 'list)))
      (setf past-goals goals-in-state)
      )))


;AGO 15-03-09
(defun goals-reached-sofar (node &optional (goals (problem-goals *current-problem*)))
  (let ((state (if (snode-p node) (snode-state node) node))
	(set-goals nil))
    (maphash #'(lambda (pred goal-bitmap)
		 (let ((state-bitmap (gethash pred state)))
		   (push  (bit-and state-bitmap goal-bitmap) set-goals)))
	     goals)
    set-goals))


;;Determines if the goals are reached in the given state
;;I check the type because i do not create nodes at relax-plan
(defun num-goals-reached-sofar (node &optional (goals (problem-goals *current-problem*)))
  (let ((state (if (snode-p node) (snode-state node) node))
	(num-goals 0))
    (maphash #'(lambda (pred goal-bitmap)
		 (let ((state-bitmap (gethash pred state)))
		   (setf num-goals (+ num-goals (count 1 (bit-and state-bitmap goal-bitmap))))))
	     goals)
    num-goals))


;;Determines if the goals are reached in the given state
;;I check the type because i do not create nodes at relax-plan
(defun state-goals-reached (node &optional (goals (problem-goals *current-problem*)))
  (let ((state (if (snode-p node) (snode-state node) node))
	(are-reached t))
    (maphash #'(lambda (pred goal-bitmap)
		 (when are-reached
		   (let ((state-bitmap (gethash pred state)))
		     (unless (equal goal-bitmap (bit-and state-bitmap goal-bitmap))
		       (setf are-reached nil)))))
	     goals)
    are-reached))

(defun goals-reached (node &key (state-goals (problem-goals *current-problem*))
		                (numeric-goals (problem-numeric-goals *current-problem*)))
  (and (state-goals-reached node state-goals)
       (numeric-goals-reached node numeric-goals)))


(defun numeric-goals-reached (node &optional (numeric-goals (problem-numeric-goals *current-problem*)))
  (let ((state (if (snode-p node) (snode-state node) node))
	(numeric-are-reached t))
  (dolist (i-numgoal numeric-goals numeric-are-reached)
      (when numeric-are-reached
	(unless (funcall (car i-numgoal)  (precfun-operand-state-eval (second i-numgoal) state) 0)
	  (setf numeric-are-reached nil))))))


;;Creates a bitmap of a given literal goal.  
;;Use this only if you need to compute in bitmaps
(defun onegoal-bitmap (literal-goal)
  (let ((goal-map (make-hash-table :test #'eq))
	(pred-pos (fact-predpos (gethash literal-goal *facts*))))
    (setf (gethash (car pred-pos) goal-map)
	  (copy-seq (gethash (car pred-pos) (problem-patterns *current-problem*))))
    (change-state goal-map (car pred-pos) (cdr pred-pos) 1)
  goal-map
  ))


;;Transforms a list of literals into a state bitmap
(defun literalgoals-bitmap (literal-goals)
  (let ((goal-map (make-hash-table :test #'eq))
	(pred-pos-list (mapcar #'(lambda (lit) (fact-predpos (gethash lit *facts*))) literal-goals)))
    (dolist (i-literal literal-goals)
      (unless (hkey-present (car i-literal) goal-map)
	(setf (gethash (car i-literal) goal-map)
	      (copy-seq (gethash (car i-literal) (problem-patterns *current-problem*))))))
    (dolist (i-predpos pred-pos-list goal-map) 
      (change-state goal-map (car i-predpos) (cdr i-predpos) 1))))


(defun reset-actions-byte (actions-byte)
  (dotimes (ibit (length actions-byte))
    (setf (sbit actions-byte ibit) 0)))
  

;; It create the whole bitmap vector instead of a huge bignum byte
(defun new-actions-byte ()
  (make-array (length *actions*) 
	      :element-type 'bit 
	      :initial-element 0))
  

;; If state1 and state2 are the same
(defun equal-state (node1 node2)
  (let ((are-equal t)
	(state2 (if (snode-p node2) (snode-state node2) node2)))
    (maphash #'(lambda (pred1 pred-bitmap1)
		 (when are-equal
		   (when (or (bit-vector-p pred-bitmap1)
			     (find pred1 (problem-positive-vars *current-problem*))
			     (find pred1 (problem-negative-vars *current-problem*)))
		     (unless (equalp pred-bitmap1 (gethash pred1 state2))
			 (setf are-equal nil)))))
	     (if (snode-p node1) (snode-state node1) node1))
    are-equal))



(defmacro apply-inst-effects (state effs bitval)
  `(dolist (i-eff ,effs)
     (change-state ,state (car i-eff) (cdr i-eff) ,bitval)))



(defun apply-action-instance (state action)
  (let ((new-state (copy-state state)))
    (apply-inst-effects new-state (gaction-dels action) 0)
    (apply-inst-effects new-state (gaction-adds action) 1)
    (apply-gaction-costs action new-state :reference-state state)
    new-state))


(defun compute-duphash-code (state)
  (let* ((duphash-pred (getf (problem-plist *current-problem*) :duphash-pred))
	 (random-matcher (getf (problem-plist *current-problem*) :duphash-matcher))
	 (predicate-seq (subseq (gethash duphash-pred state) 0 (length random-matcher))))
    (bitmap-to-integer (bit-xor predicate-seq random-matcher))))



;;=============================================================================
;;Deciding if an action if applicable. 
;;If the relaxed-plangraph is called first in node, this is avoided using
;; the applicable of helpful byte of the node
(defun connect-fact (pred-pos)
  (let ((fg (fact-of-predpos pred-pos)))
    (dolist (iact (fg-precond-of fg))
      (unless (gaction-applicable iact)
	(incf (gaction-prec-gcount iact))
	(when (and (= (gaction-num-precs iact) (gaction-prec-gcount iact))
		   (= (gaction-num-precfuns iact) (gaction-precfun-gcount iact)))
	  (setf (sbit *applicable-byte* (gaction-int iact)) 1)
	  (setf (gaction-applicable iact) t)
	  )))))      


(defun connect-fluent (pred-pos state-value)
  (let ((flg (fluent-of-predpos pred-pos)))
    (dolist (iop-precfun-n (flg-precfun-of flg))
      (let* ((iop (car iop-precfun-n))
	     (precfun (nth (cdr iop-precfun-n) (gaction-precfuns iop))))
	(unless (gaction-applicable iop)
	  (when (precfun-value-eval precfun state-value)
	    (incf (gaction-precfun-gcount iop))
	      (when (and (= (gaction-num-precs iop) (gaction-prec-gcount iop))
			 (= (gaction-num-precfuns iop) (gaction-precfun-gcount iop)))
		(setf (gaction-applicable-h iop) t)
		(setf (sbit *applicable-byte* (gaction-int iop)) 1)
		)))))))




(defun reset-expand-state ()
  (if (and (simple-bit-vector-p *applicable-byte*)
	   (= (length *applicable-byte*) (length *actions*)))
      (reset-actions-byte *applicable-byte*)
      (setf *applicable-byte* (new-actions-byte)))
  (dolist (iact *actions*)
    (setf (gaction-prec-gcount iact) 0)
    (setf (gaction-precfun-gcount iact) 0)
    (setf (gaction-applicable iact) nil)))



(defun find-applicable-byte (node helpful)
  (cond 
    ((and helpful (simple-bit-vector-p (snode-helpful-byte node)))
     (snode-helpful-byte node))
    ((simple-bit-vector-p (snode-applicable-byte node)) 
     (snode-applicable-byte node))
    (t 
     (setf (snode-applicable-byte node) (set-applicable-byte node)))))


(defun set-applicable-byte (node)
  (reset-expand-state)
  (maphash #'(lambda (key statemap)
	       (cond ((simple-bit-vector-p statemap)
		      (dotimes (ibit (length statemap))
			(when (= 1 (bit statemap ibit))
			  (connect-fact (cons key ibit)))))
		     (t
		      (dotimes (ibit (length statemap))
			(when (numberp (aref statemap ibit))
			  (connect-fluent (cons key ibit) (numvar-state-value key (snode-state node) ibit))
			  )))))
		    (snode-state node))
  *applicable-byte*)


;; It marks the sucessors that are helpful when the node expansion is done without helpful action pruning
(defun mark-helpful-actions (node app-byte)
  (dolist (i-child (snode-children node))
    (when (= 1 (sbit app-byte (gaction-int (snode-applied-action i-child))))
      (setf (snode-helpful-p i-child) t))))


;;=====================================================================================
;; Node expansion function: Helpful t -> expands node only with
;; helpful actions restoring-byte -> uses an applicable-byte (see
;; above) to complete expansion of non-helpful children

(defun expand-state (node &key (helpful t)
		               (restoring-byte nil))
  (let* ((state (snode-state node)) (children nil)
	 (applicable-byte (if (null restoring-byte) (find-applicable-byte node helpful) restoring-byte))
	 (new-child nil) (child-node nil)) 
    (dotimes (ibit (length applicable-byte))
      (when (= 1 (sbit applicable-byte ibit))
	;; DB: I do not spend time on searching for the property if there was no constraint defined in the problem
	(let ((app-action (aref *actions-table* ibit))
	      (constraints (and *constraintsp* (getf (problem-plist *current-problem*) :constraints))))
	  ;; Not for now. We can add this to plan to be more efficient
	  ;; 	  (if (and constraints (not (getf (problem-plist *current-problem*) :number-constraints-true)))
	  ;; 	      (setf (getf (problem-plist *current-problem*) :number-constraints-true) 0))
	  (setf new-child (apply-action-instance state app-action))
	  (when (or (not constraints) (fulfills-constraints new-child constraints))
	    (setf (snode-expanded node) t)
	    (setf child-node (make-snode 
			      :depth (+ 1 (snode-depth node))
			      :length (+ 1 (snode-length node))
			      :parent node
			      :state new-child
			      :applied-action app-action
			      :helpful-p helpful
			      ))
	    (setf (snode-cost child-node) 
		  (action-cost-metric state new-child))
	    (setf (snode-number child-node) 
		  (incf (getf (problem-plist *current-problem*) :node-counter)))
	    (setf (gethash (snode-number child-node) *hash-nodes*) child-node)
	    (setf (snode-hash-code child-node) (compute-duphash-code new-child))
	    (push child-node children)))))
    (if (null restoring-byte)
	(setf (snode-children node) (reverse children))
	(setf (snode-children node) (nconc (reverse children) (snode-children node))))
    (when (and (not helpful) (simple-bit-vector-p (snode-helpful-byte node)))
      (mark-helpful-actions node (snode-helpful-byte node)))
))
  


(defun get-instantiated-goals (goals)
  (let ((inst-goals nil))
    (maphash #'(lambda (pred goals-bitmap)
		 (dotimes (ibit (length goals-bitmap))
		   (when (eq 1 (sbit goals-bitmap ibit))
		     (push (cons pred ibit) inst-goals))))
	     goals)
    inst-goals))


(defun build-path-to-root (node)
  (let ((path nil))
    (do ((i-node node (snode-parent i-node)))
	((null (snode-parent i-node)) path)
      (push i-node path))))


;; (defun find-tree-dead-end (&optional (node (problem-search-tree *current-problem*)))
;;   (cond ((and (numberp (snode-h-value node)) 
;; 	      (= (snode-h-value node) most-positive-fixnum)) 
;; 	 (list node))
;; 	(t 
;; 	 (mapcan #'find-dead-end (snode-children node)))))






;; Check this out first. It may not work after the new representation
;; Searchs for all applicable actions and stores candidates nodes for later creation
;; (defun expand-candidate-state (node)
;;   (let ((actions (dom-actions *pspace*))
;; 	(state (snode-state node)))
;;     (setf (snode-candidates node) nil)
;;     (dolist (iaction actions (snode-children node))
;;       (let ((applicable-map (action-applicable-byte iaction state)))
;; 	(when (> applicable-map 0)
;; 	  (dotimes (ibit (integer-length applicable-map))
;; 	    (when (logbitp ibit applicable-map)
;; 	      (setf (snode-expanded node) t)
;;  	      (setf (snode-candidates node)
;;  		    (append (snode-candidates node) (list (cons (action-name iaction) ibit))))
;; 	      )))))))
;; 
;; 
;; ;; Creates a node's child from one of its candidates	      
;; (defun create-candidate-node (node &optional (problem *current-problem*))
;;   (let ((candidate (pop (snode-candidates node)))
;; 	(new-child nil) (child-node nil))
;;     (when candidate
;;       (let ((iaction (gethash (car candidate) (getf (dom-plist *pspace*) :direct-actions)))
;; 	    (state (snode-state node)))
;; 	(setf new-child (apply-action-instance state iaction (cdr candidate)))
;; 	(setf child-node (make-snode 
;; 			  :number (incf (getf (problem-plist problem) :node-counter))
;; 			  :depth (+ 1 (snode-depth node))
;; 			  :parent node
;; 			  :state new-child
;; 			  :applied-action candidate))
;; 	(push child-node (snode-children node))))))

;; ********************************************************
;;  DB: code that checks whether problem defined constraints are fulfilled
;; ********************************************************

;; The rest of constraints to be implemented
(defun fulfills-constraints (state constraints)
  (let ((checked (check-constraint state (cadr constraints))))
    (case (car constraints)
      (always checked)
      (sometime (if checked (setf (getf (problem-plist *current-problem*) :constraints) nil))
		checked)
;;       (within (if checked (incf (getf (problem-plist *current-problem*) :number-constraints-true)))
;; 	      checked)
      (otherwise checked))))

;; Very inefficient. We will improve it by compiling into the ibits
;; This only accounts for predicates constraints. I will work on the more complex goals later.
;; I do the AND to not return the list (efficiency)
(defun check-constraint (state constraints)
  (and (case (car constraints)
	 (and (every #'(lambda (constraint)
			 (check-individual-constraint state constraint))
		     (cdr constraints)))
	 (or (some #'(lambda (constraint)
		       (check-individual-constraint state constraint))
		   (cdr constraints)))
	 (t (check-individual-constraint state constraints)))
       t))

(defun check-individual-constraint (state constraint)
  (member constraint (pp-state state 'list) :test #'equal))
