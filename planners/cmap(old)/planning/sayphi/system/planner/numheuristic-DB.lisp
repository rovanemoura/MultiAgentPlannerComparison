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
;; Description: Dealing with Fluents in preconditions and effects.
;; Date: 2006.09.28
;; 
;; ========================================================================

(defparameter *h-active-facts* nil)

(defparameter *h-active-flpatterns* nil)
(defparameter *h-active-fluents* nil)

(defparameter *h-achieved-facts* nil)
(defparameter *h-achieved-fluents* nil)
(defparameter *fluent-max-layers* nil)

(defparameter *h-active-actions* nil)
(defparameter *actions-table* nil)
(defparameter *action-layers* nil)
(defparameter *achieved-layers* nil)
(defparameter *h-achieved-layers* nil)
(defparameter *focus-goals* nil)


(defvar *h-max-distance* 1000 "stops relax-plan search")
(defvar *get-achieved-goals* nil)
(defvar *h-achieved-goals* nil)

(defstruct (plangraph 
	    (:conc-name pg-))
  (facts-mship nil)
  (numvars-mship nil)
  (goal-layers (make-hash-table))
  (numgoal-layers (make-hash-table))
  (action-layers nil)
  (achiev-goals nil)
  (achiev-numgoals nil)
  (h-achieved nil)
  (focus-goals nil)
  (relaxed-plan nil))


;; It create an array of *actions* for direct access
(defun rebuild-action-table ()
  (setf *actions-table* (make-array (length *actions*)))
  (dolist (igaction *actions*)
    (setf (aref *actions-table* (gaction-int igaction))
	  igaction)))


(defun reset-state-facts (state)
  (let ((void-state (problem-patterns *current-problem*)))
    (maphash #'(lambda (key statemap)
		 (when (bit-vector-p statemap)
		   (bit-and statemap (gethash key void-state) t)))
	     state)))

(defun reset-action-table (atable)
  (dolist (iop *actions*)
    (setf (gaction-prec-h-gcount iop) 0)
    (setf (gaction-precfun-h-gcount iop) 0)
    (setf (gaction-applicable-h iop) nil)
    
    (setf (gaction-layer iop) most-positive-fixnum))

  (dotimes (ibit (length atable))
    (setf (bit atable ibit) 0)))


(defun reset-action-layer (layer)
  (let ((action-layer (gethash layer *action-layers*)))
    (cond ((simple-bit-vector-p action-layer)
	   (dotimes (ibit (length action-layer))
	     (setf (aref action-layer ibit) 0)))
	  (t (setf (gethash layer *action-layers*) 
		   (make-array (length *actions-table*) 
			       :element-type 'bit 
			       :initial-element 0))))))


(defun build-fluent-active-table (state)
  (let ((active-fluents (make-hash-table :test #'eq)))
    (maphash #'(lambda (key statemap)
		 (unless (bit-vector-p statemap)
		   (when (fluent-needed key) 
		     (setf (gethash key active-fluents)
			   (make-array (length statemap) :element-type 'bit
				       :initial-element 0))
		     (dotimes (i (length statemap))
		       (when (numberp (aref statemap i))
			 (setf (sbit (gethash key active-fluents) i) 1))))))
	     state)
    active-fluents
    ))

			       
					       
		 

(defun init-heuristics ()
  (rebuild-action-table)
  (setf *h-active-facts* (copy-state-set (problem-patterns *current-problem*)))
  (setf *h-active-flpatterns* (build-fluent-active-table (problem-init-state *current-problem*)))
  (setf *focus-goals* (copy-state (problem-patterns *current-problem*)))
  (setf *h-active-actions* (make-array (length *actions-table*) 
					       :element-type 'bit 
					       :initial-element 0))
  (setf *achieved-layers* (make-hash-table))
  (setf *h-achieved-layers* (make-hash-table))
  (setf *fluent-max-layers* (make-hash-table))
)


(defun reset-h-factgraph (state)
  (maphash #'(lambda (key factmap)
	       (dotimes (pos (length factmap))
		 (cond ((true-in-state state key pos)
			(setf (fg-layer (aref factmap pos)) 0)
			(setf (fg-activated (aref factmap pos)) t))
		       (t
			(setf (fg-layer (aref factmap pos)) most-positive-fixnum)
			(setf (fg-activated (aref factmap pos)) nil)))))
	   *factgraph-table*))


(defun reset-plangraph (state)
  (setf *h-active-facts* (copy-state-set state 'facts))
  (setf *h-achieved-facts* (copy-state-set state 'facts))
  (setf *h-achieved-fluents* (copy-state-set state 'real-fluents))
  (setf *h-active-fluents* (copy-state *h-active-flpatterns*))
  (reset-h-factgraph state)
  (reset-state-facts *focus-goals*)
  (reset-action-table *h-active-actions*)
  (setf *action-layers* nil)
  (setf *action-layers* (make-hash-table))
  (setf *fluent-max-layers* (make-hash-table))
  (setf (gethash 0 *fluent-max-layers*) (copy-state-set state 'real-fluents))
)


(defmacro num-layers-graph ()
  `(hash-table-count *action-layers*))

(defun fluent-needed (fluent-pred)
  (when (or (find fluent-pred (problem-positive-vars *current-problem*))
	    (find fluent-pred (problem-artificial-vars *current-problem*))) t))

(defun max-needed (pred-pos)
  (flg-max-needed (fluent-of-predpos pred-pos)))


;; It returns the corresponding cost of a given numvar (pred-pos) looking through action costs
(defmacro cost-of-numvar-gaction (gaction pred-pos)
  `(find ,pred-pos (gaction-costs ,gaction) :test #'equal :key #'second))


(defun connect-fact-h (pred-pos layer)
  (let ((fg (fact-of-predpos pred-pos)))
   (dolist (iop (fg-precond-of fg))
      (unless (gaction-applicable-h iop)
	(incf (gaction-prec-h-gcount iop))
	(when (and (= (gaction-num-precs iop) (gaction-prec-h-gcount iop))
 		   (= (gaction-num-precfuns iop) (gaction-precfun-h-gcount iop)))
	  (setf (gaction-layer iop) layer)
	  (setf (gaction-applicable-h iop) t)
	  (setf (bit *h-active-actions* (gaction-int iop)) 1))
	
  ))))


(defun predpos-of-precfun (precfun-exp)
  (let ((precfun (second precfun-exp)))
    (if (listp (second precfun))
	(second precfun)
	(third precfun))))

;; changed by DB
(defun precfun-value-eval (precfun-exp value)
  (let ((comparator (car precfun-exp))
	(precfun (second precfun-exp)))
    (if (= (length precfun-exp) 3)
	(funcall comparator (funcall (car precfun)
				 (if (listp (second precfun)) value (second precfun))
				 (if (listp (third precfun)) value (third precfun))) 0)
	t)))



;; It recieves the numeric goal in the form (*comp (*op num numvar)) or (*comp (*op numvar num))
;; both equivalent to the (comp numgoal 0). It return the value of numvar that makes true the expression
(defun solve-numtask-exp (numgoal)
  (let ((expression (second numgoal)))
    (cond ((numberp (second expression))
	   (split-cut-point (car expression) (second expression)))
	  ((numberp (third expression))
	   (split-cut-point (car expression) (third expression))))))


(defun precfun-old-achieved (precfun layer)
  (funcall (car precfun)  
	   (numvar-predpos-state-value (predpos-of-precfun precfun) (gethash layer *fluent-max-layers*))
	   (solve-numtask-exp precfun)))


(defun connect-fluent-h (pred-pos layer state-value)
  (let ((flg (fluent-of-predpos pred-pos)))
    (dolist (iop-precfun-n (flg-precfun-of flg))
      (let* ((iop (car iop-precfun-n))
	     (precfun (nth (cdr iop-precfun-n) (gaction-precfuns iop))))
	(unless (gaction-applicable-h iop)
	  (when (precfun-value-eval precfun state-value)
	    (when (or (= layer 1) 
		      (not (precfun-old-achieved precfun (1- (1- layer)))))
	      (incf (gaction-precfun-h-gcount iop))
	      (when (and (= (gaction-num-precs iop) (gaction-prec-h-gcount iop))
			 (= (gaction-num-precfuns iop) (gaction-precfun-h-gcount iop)))
		(setf (gaction-layer iop) layer)
		(setf (gaction-applicable-h iop) t)
		(setf (bit *h-active-actions* (gaction-int iop)) 1))
	      
)))))))


(defmacro mark-action-graph (index layer)
  `(setf (aref (gethash ,layer *action-layers*) ,index) 1))


(defun apply-gaction-relaxed-costs (gaction h-state layer)
  (declare (ignore layer))
  (dolist (i-cost (gaction-costs gaction))
    (when (fluent-needed (car (second i-cost)))
      (let ((current-value (numvar-predpos-state-value (second i-cost) h-state))
	    (delta-value (operand-funeval (third i-cost) h-state))
	    (pred-pos (second i-cost)))
	(when (<= current-value (max-needed pred-pos))
	  (cond ((eq 'increase (car i-cost))
		 (unless (= delta-value 0)
		   (change-fun-state h-state (car pred-pos) (cdr pred-pos) (+ current-value delta-value))
		   ))
		((eq 'assign (car i-cost))
		 (unless (= delta-value current-value)
		   (change-fun-state h-state (car pred-pos) (cdr pred-pos) delta-value)
		   ))
	      ))))))
  

(defun connect-action-h (index layer)
  (let ((gaction (aref *actions-table* index)))
    (mark-action-graph index layer)
    (dolist (iadd (gaction-adds gaction))
      (let ((fg (fact-of-predpos iadd)))
	(unless (fg-activated fg)
	  (change-state *h-achieved-facts* (car iadd) (cdr iadd) 1)
	  (change-state *h-active-facts* (car iadd) (cdr iadd) 1)
	  (setf (fg-layer fg) layer)
	  (setf (fg-activated fg) t))))
    (apply-gaction-relaxed-costs gaction *h-achieved-fluents* layer) 
))



;; This function is used to determine if there is any active fact or fluent
;; for then stopping the graphplan expansion
(defun something-true-hash-state (hash-state)
  (let ((something-true nil))
    (maphash #'(lambda (pred state-map)
		 (declare (ignore pred))
		 (unless something-true
		   (when (find 1 state-map)
		     (setf something-true t))))
	     hash-state)
    something-true))

(defun something-changed-active-fluents (hash-state layer)
  (let ((something-changed nil))
    (cond ((= 0 layer) (setf something-changed t))
	  (t (maphash #'(lambda (pred state-map)
			  (unless something-changed
			    (dotimes (ibit (length state-map))
			      (when (and (= 1 (aref state-map ibit))
					 (> (numvar-state-value pred *h-achieved-fluents* ibit)
					    (numvar-state-value pred (gethash (1- layer) *fluent-max-layers*) ibit)))
				(setf something-changed t)))))
		      hash-state)))
    something-changed))

;; The layer for computing changed fluents is the previous one because this function
;; is called after increasing layer integer 
(defun expand-stop (layer)
  (cond ((not (or (something-true-hash-state *h-active-facts*)
		  (something-changed-active-fluents *h-active-fluents* (1- layer))
)) t)))



(defun expand-relaxedplangraph (state goals numeric-goals)
  (reset-plangraph state)
  (do ((layer 1 (incf layer))
       (relax-found (and (state-goals-reached *h-achieved-facts* goals)
			 (numeric-goals-reached *h-achieved-fluents* numeric-goals))
		    (and (state-goals-reached *h-achieved-facts* goals)
			 (numeric-goals-reached *h-achieved-fluents* numeric-goals))))
      ((or relax-found (expand-stop layer))
       relax-found)

    ;;Activating Facts
    (maphash #'(lambda (key statemap)
		 (dotimes (ibit (length statemap))
		   (when (= 1 (bit statemap ibit))
		     (connect-fact-h (cons key ibit) layer)
 		     (setf (bit statemap ibit) 0))))
	     *h-active-facts*)

    ;;Activating Fluents
    (maphash #'(lambda (key statemap)
		 (dotimes (ibit (length statemap))
		   (cond ((and (= 1 (bit statemap ibit))
			       (numberp (max-needed (cons key ibit))))
			  (let ((state-value (numvar-state-value key *h-achieved-fluents* ibit)))
			    (connect-fluent-h (cons key ibit) layer state-value)
			    (when (> state-value (max-needed (cons key ibit)))
			      (setf (bit statemap ibit) 0))))
			 (t (setf (bit statemap ibit) 0)))))
	     *h-active-fluents*)
  
    (reset-action-layer layer)
    (dotimes (jbit (length *h-active-actions*))
      (when (= 1 (bit *h-active-actions* jbit))
	(connect-action-h jbit layer)
	(setf (bit *h-active-actions* jbit) 0)))
  
    ;;Reapplying action cost 
    (dotimes (k-layer (1- layer))
      (let ((pastlayer-actions (gethash k-layer *action-layers*)))
	(when (integerp pastlayer-actions)
	  (dotimes (ibit (integer-length pastlayer-actions))
	    (when (logbitp ibit pastlayer-actions)
	      (apply-gaction-relaxed-costs (aref *actions-table* ibit) *h-achieved-fluents* layer))))))
    
    (setf (gethash layer *fluent-max-layers*) (copy-state-set *h-achieved-fluents* 'real-fluents))
    ))
    

;; (defmacro fact-is-member (pred-pos layer fact-membership)
;;   `(= ,layer (aref (gethash (car ,pred-pos) ,fact-membership) (cdr ,pred-pos))))
(defmacro fact-is-member (pred-pos layer)
  `(= ,layer (fg-layer (aref (gethash (car ,pred-pos) *factgraph-table*) (cdr ,pred-pos)))))


;; Returns the number of the layer in which a literal is present
(defmacro layer-membership (pred-pos)
   `(fg-layer (aref (gethash (car ,pred-pos) *factgraph-table*) (cdr ,pred-pos))))


(defmacro achieved-in-graph (layer achieved-graph pred-pos)
  `(true-in-state (gethash ,layer ,achieved-graph) (car ,pred-pos) (cdr ,pred-pos)))



;; numgoal recibida como (> (FUEL . 0) 100)
(defmacro statevar-is-true (numgoal layer numvars-membership)
  `(= ,layer (layer-from-nummembership ,numgoal ,numvars-membership)))



;; It applies the corresponding action to a state of achieved goal layer
(defun apply-to-achiev (action achiev-goals)
  (apply-inst-effects achiev-goals (gaction-adds action) 1)
  (when (is-metric-domain)
    (apply-gaction-costs action achiev-goals :relaxed t)))



(defun apply-relaxed-plan (relaxed-plan &optional (void-state nil))
  (let* ((applied-state (if void-state 
			    (copy-state void-state)
			    (copy-state (problem-patterns *current-problem*)))))
    (dolist (i-gaction relaxed-plan applied-state)
      (apply-to-achiev i-gaction applied-state))))


(defun goal-functor (numgoal)
  (cond ((numberp (second numgoal))
	 (third numgoal))
	((numberp (third numgoal))
	 (second numgoal))))


(defun instaction-difficulty (iaction)
  (let ((difficulty 0))
    (dolist (iprec (gaction-preconds iaction) difficulty)
      (setf difficulty (+ difficulty (layer-membership iprec))))))


(defmacro achievers (pred-pos)
   `(fg-added-by (aref (gethash (car, pred-pos) *factgraph-table*) (cdr ,pred-pos))))

(defun num-achievers (pred-pos)
  (remove-if #'(lambda (achiever)
		 (find (list 'decrease pred-pos) (gaction-costs achiever) 
		       :key #'(lambda (c) (subseq c 0 2)) :test #'equal))
	     (flg-changed-by (aref (gethash (car pred-pos) *fluentgraph-table*) (cdr pred-pos)))))


(defmacro action-applied-inlayer (gaction layer)
  `(= 1 (aref (gethash ,layer *action-layers*) (gaction-int ,gaction))))


(defun select-rplan-action (goal n-layer)
  (let ((current-relevant-action nil))
    (dolist (i-achiever (achievers goal) current-relevant-action)
      (when (action-applied-inlayer i-achiever n-layer)
	(let* ((this-diff (setf (gaction-h-difficulty i-achiever)
				(instaction-difficulty i-achiever))))
	  
	  (when (or (null current-relevant-action)
		    (< this-diff (gaction-h-difficulty current-relevant-action)))
	    (setf current-relevant-action i-achiever) 
	    ))))))



(defun remaining-numgoal (comparator numvar needed-layer-value)
  (let* ((simple-goal-exp (list comparator numvar needed-layer-value))
	 (goal-exp (transform-lnf-step2 simple-goal-exp)))
    (list comparator (artificial-fun-exp (second goal-exp) t))))
    

;;It returns the list of actions selected in the action-layer for achieving a numeric goal
;; together with a remaining numeric goal if it is not completely satisfied
(defun select-rplan-numeric-actions (goal-exp n-layer)
  (let* ((goal (second goal-exp))
	 (numvar (if (numberp (second goal)) (third goal) (second goal)))
	 (max-previous (numvar-predpos-state-value numvar (gethash (1- n-layer) *fluent-max-layers*)))
	 (needed-layer-value (solve-numtask-exp goal-exp))
	 (relevant-actions nil))
;;     (format t "~% TRX: numvar:~a max-previous:~a needed:~a" numvar max-previous needed-layer-value)
    (dolist (i-achiever (num-achievers numvar) (cons relevant-actions 
						     (remaining-numgoal (car goal-exp) numvar needed-layer-value)))
      (when (and (not (funcall (car goal-exp) max-previous needed-layer-value))
		 (action-applied-inlayer i-achiever n-layer))
	(let ((changing-cost (find numvar (gaction-costs i-achiever) :test #'equal :key #'second))) 

	  (push i-achiever relevant-actions)
	  (setf needed-layer-value (cond ((eq 'increase (car changing-cost))
 				  	  (- needed-layer-value  (operand-funeval (third changing-cost) 
										  (gethash n-layer *fluent-max-layers*))))
					 ))
;; 	  (format t "~% TRX: >> Selected ~a max:~a needed:~a"  i-achiever max-previous needed-layer-value)
	  )))))


;; The expression of numgoal is true when (> expression 0)
(defun achieved-in-numeric-graph (layer achiev-graph numgoal)
  (let ((goal-layer (gethash layer achiev-graph)))
    (cond ((numberp (second numgoal))
	   (> (funcall (car numgoal) (second numgoal) (numvar-state-value 
						       (car (third numgoal)) goal-layer (cdr (third numgoal))))
	      0))
	  ((numberp (third numgoal))
	  (> (funcall (car numgoal) (numvar-state-value (car (second numgoal)) goal-layer (cdr (second numgoal)))
		      (third numgoal))
	      0))
	  )))



(defun layer-num-achieved (precfun)
  (let ((pred-pos (predpos-of-precfun precfun))
	(solve-value (solve-numtask-exp precfun))
	(solve-in-layer nil))
    (dotimes (i-layer (hash-table-count *fluent-max-layers*) solve-in-layer)
      (unless solve-in-layer
	(when (funcall (car precfun)  
		       (numvar-predpos-state-value pred-pos (gethash i-layer *fluent-max-layers*))
		       solve-value)
	  (setf solve-in-layer i-layer))))))
 
    
		 
;; It takes the sub-goals from actions preconditions and push them in thier corresponding
;; layer of goals to achiev
(defun insert-prec-subgoals (action nlayer plangraph)
  (dolist (iprec-goal (gaction-preconds action))
    (unless (or (fact-is-member iprec-goal 0)
		(achieved-in-graph (1- nlayer) *achieved-layers* iprec-goal))
      (let ((layer (layer-membership iprec-goal)))
	(push iprec-goal (gethash layer (pg-goal-layers plangraph)))))))


(defun insert-numprec-subgoals (action nlayer plangraph)
  (dolist (iprec-numgoal (gaction-precfuns action))
    (cond ((listp iprec-numgoal)
	  (unless (or (= 0 (layer-num-achieved iprec-numgoal))
		      (achieved-in-numeric-graph (1- nlayer) *achieved-layers* iprec-numgoal))
	    (let ((layer (layer-num-achieved iprec-numgoal)))
	      (push iprec-numgoal (gethash layer (pg-numgoal-layers plangraph))))))
	  ((and (numberp iprec-numgoal) 
		(< iprec-numgoal 0))
	   (format t "Say-Warning Unsolvable numeric goal!")))))



			 
(defun create-achieved-membership (num-layers achieved-layers)
  (let ((ifact-layer nil))
    (dotimes (i-layer (+ num-layers 1) achieved-layers)
      (setf ifact-layer (gethash i-layer achieved-layers))
      (cond ((null ifact-layer)
	     (setf (gethash i-layer achieved-layers)
		   (copy-state (problem-patterns *current-problem*))))
	    (t (reset-state-facts ifact-layer))))))


(defun h-difficult-fraction (relevant-actions)
  (let ((num-actions (length relevant-actions))
	(difficult-sum (apply #'+ (mapcar #'gaction-h-difficulty relevant-actions))))
    (if (> difficult-sum 0)
	(+ num-actions (/ (- 1 (float (/ 1 difficult-sum))) 2))
      num-actions)))

;; Testing the opposite effect of hdiff
(defun h-difficult-increment (relevant-actions)
  (let ((num-actions (length relevant-actions))
	(difficult-sum (apply #'+ (mapcar #'gaction-h-difficulty relevant-actions))))
    (if (> difficult-sum 0)
	(+ num-actions (/ (float (/ 1 difficult-sum)) 2))
      num-actions)))



(defun h-pondered-difficulty (relevant-actions)
  (apply #'+ (mapcar #'(lambda (i-relevantaction)
			 (let ((i-diff (cadr i-relevantaction)))
			   (float (/ (+ 1 i-diff) (+ 2 i-diff)))))
		     relevant-actions)))


(defun update-numvars-membership (numvars-membership new-value pred-fun pos ilayer)
  (let* ((numvar-layer-table (gethash pred-fun numvars-membership)))
    (when (hash-table-p numvar-layer-table)
      (maphash (lambda (point layer-array)
		 (when (and (>= new-value point)
			    (< ilayer (aref layer-array pos)))
		   (setf (aref layer-array pos) ilayer)))
	     numvar-layer-table))))



;; Statements to do with the select action for the relaxded plan
;; This is done for literals and numeric subgoals
(defun with-selected-gpaction (selected-rplan-action nlayer pg focus-layer)
  (push selected-rplan-action (pg-relaxed-plan pg))
  (insert-prec-subgoals selected-rplan-action nlayer pg)
  (insert-numprec-subgoals selected-rplan-action nlayer pg)
   
  (apply-to-achiev selected-rplan-action (gethash nlayer *achieved-layers*))
  (apply-to-achiev selected-rplan-action (gethash (1- nlayer) *achieved-layers*))
  (when (= focus-layer nlayer)
      (apply-to-achiev selected-rplan-action *focus-goals*))
  (when *get-achieved-goals*
      (apply-to-achiev selected-rplan-action (gethash nlayer *h-achieved-layers*))))



(defun init-plangraph (node goals numeric-goals) 
  (when (expand-relaxedplangraph (snode-state node) goals numeric-goals)
    (create-achieved-membership (num-layers-graph) *achieved-layers*)
    (make-plangraph)
     
     ))


(defun find-helpful-byte (pg)
  (let ((fact-goals (gethash 1 (pg-goal-layers pg)))
	(fluent-goals (gethash 1 (pg-numgoal-layers pg)))
	(helpful-byte (new-actions-byte)))
;;     (format t "~%fluent-goals:~a" fluent-goals)
    (dolist (i-goal fact-goals)
      (dolist (i-achiever (fg-added-by (fact-of-predpos i-goal)))
	(when (= 1 (gaction-layer i-achiever))
	  (setf (sbit helpful-byte (gaction-int i-achiever)) 1))))
    (dolist (i-numgoal-exp fluent-goals)
      (let* ((i-numgoal (second i-numgoal-exp))
	     (numpred-pos (if (numberp (second i-numgoal)) (third i-numgoal) (second i-numgoal)))
	     (fluent-value (if (numberp (second i-numgoal)) (second i-numgoal) (third i-numgoal))))
	(dolist (i-achiever (num-achievers numpred-pos))
	  (let ((effect-cost (cost-of-numvar-gaction i-achiever numpred-pos)))
;; 	    (format t "~% PG> i-numgoal ~a " i-numgoal)
;; 	    (format t "~% PG> i-achiever:  ~a fluent ~a" i-achiever fluent-value)
	    (when (and (= 1 (gaction-layer i-achiever))
		       (or (and (eq 'increase (car effect-cost))
				(> (third effect-cost) 0))
			   (and (eq 'assign (car effect-cost))
				(>= (third effect-cost) fluent-value))))
	      (setf (sbit helpful-byte (gaction-int i-achiever)) 1))))))
      helpful-byte))



(defun metric-relaxedplan (node goals numeric-goals &key (focus-layer 1))
  (let ((inst-goals (get-instantiated-goals goals))
	(pg (init-plangraph node goals numeric-goals)))
    (when (plangraph-p pg)
      (when *get-achieved-goals*
	(setf (pg-h-achieved pg) (create-achieved-membership (num-layers-graph) *h-achieved-layers*))
 	(setf *h-achieved-goals* *h-achieved-layers*))
      
      (dolist (igoal inst-goals)
	(push igoal (gethash (layer-membership igoal) (pg-goal-layers pg))))
      (dolist (inumgoal numeric-goals)
	(push inumgoal (gethash (layer-num-achieved inumgoal) (pg-numgoal-layers pg))))

      (dotimes (i (num-layers-graph))
	(let ((i-layer (- (num-layers-graph) i)))
	  (dolist (igoal (reverse (gethash i-layer (pg-goal-layers pg))))
;; 	  (dolist (igoal (gethash i-layer (pg-goal-layers pg)))
	    (unless (achieved-in-graph i-layer *achieved-layers* igoal)
	      (with-selected-gpaction (select-rplan-action igoal i-layer)
		i-layer pg focus-layer)))
  	  (dolist (inumgoal (reverse (gethash i-layer (pg-numgoal-layers pg))))
;;  	  (dolist (inumgoal (gethash i-layer (pg-numgoal-layers pg)))
 	    (unless (achieved-in-numeric-graph i-layer *achieved-layers* inumgoal)
	      (let ((solving-numlayer (select-rplan-numeric-actions inumgoal i-layer)))
		(dolist (i-selected (car solving-numlayer))
		  (with-selected-gpaction i-selected i-layer pg focus-layer))
		(when (cdr solving-numlayer)
		  (unless (= 0 (layer-num-achieved (cdr solving-numlayer)))
		    (push (cdr solving-numlayer) (gethash (1- i-layer) (pg-numgoal-layers pg)))))
		)))))

	      
;;       (setf (snode-helpful-byte node) (find-helpful-byte *focus-goals*))
      (setf (snode-helpful-byte node) (find-helpful-byte pg))
      (setf (snode-applicable-byte node) (gethash 1 *action-layers*))
      pg
	  )))




(defun h-metric-rxplan (node &key (goals (problem-goals *current-problem*))
			          (numeric-goals (problem-numeric-goals *current-problem*)))
  (let* ((pg (metric-relaxedplan node goals numeric-goals))
	 (relaxed-plan nil) (applied-rp-state nil) (h-metric nil))
    (cond ((plangraph-p pg)
	   (setf relaxed-plan (pg-relaxed-plan pg))
	   
	   (cond ((not (null (problem-metric *current-problem*)))
		  (setf applied-rp-state (apply-relaxed-plan relaxed-plan))
		  (setf h-metric (operand-funeval (cadr (problem-metric *current-problem*)) applied-rp-state)))
		 (t
		  (setf h-metric (length relaxed-plan))))
	   (values h-metric relaxed-plan ))
	  (t (values most-positive-fixnum nil nil)))))


;; @@ Cambiar en las demas heurísticas
(defun h-relaxedplan (node &key (goals (problem-goals *current-problem*))
			          (numeric-goals (problem-numeric-goals *current-problem*)))
  (let* ((pg (metric-relaxedplan node goals numeric-goals)))
    (cond ((plangraph-p pg)
	   (values (length (pg-relaxed-plan pg)) 
		   (pg-relaxed-plan pg)))
	  (t (values most-positive-fixnum nil nil)))))


(defun h-relaxedplan-plus (node &key (goals (problem-goals *current-problem*))
			          (numeric-goals (problem-numeric-goals *current-problem*)))
  (let* ((pg (metric-relaxedplan node goals numeric-goals)))
    (cond ((plangraph-p pg)
	   (values (h-difficult-fraction (pg-relaxed-plan pg)) 
		   (pg-relaxed-plan pg) 
		   ))
	  (t (values most-positive-fixnum nil nil)))))

(defun h-relaxedplan-incdiff (node &optional (problem *current-problem*))
  (let* ((goals (problem-goals problem))
	 (pg (metric-relaxedplan node goals nil)))
    (cond ((plangraph-p pg)
	   (values (h-difficult-increment (pg-relaxed-plan pg)) 
		   (pg-relaxed-plan pg) 
		   ))
	  (t (values most-positive-fixnum nil nil)))))



;;=============================================================
;;Extras for Printing and Developing
;;=============================================================
;; (defun dev-fg-showvar (pred-pos var)
;;   (let ((fg (fact-of-predpos

(defun gaction-fullprint (ga)
  (format t "~% <Gaction>: ~a " (gaction-planaction ga))
  (format t "~%   precs: ~a " (gaction-preconds ga))
  (format t "~%   adds: ~a " (gaction-preconds ga))
  (format t "~%   dels: ~a " (gaction-preconds ga))
  (format t "~%   prec-h-gcount: ~a " (gaction-prec-h-gcount ga)))


;; (defun action-layer-print (layer)
;;   (let ((action-layer (gethash layer *action-layers*))
;; 	(action-count 1))
;;     (dotimes (ibit (length action-layer))
;;       (when (= (aref action-layer ibit)
;; 	(format t "~% ~d ~a" (aref *actions-table* ibit))
;; 	(incf action-count))))))



