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
;; Description: Domain Encoding & Instantiation 
;; Date: 2007.09.14
;; 
;; ========================================================================


;; A hash table holding the bitmap of semi-instantiated predicates
;; The value holds a cell cons (bitmap . n-initial-occurrence
(defparameter *semi-inst-pred* nil)
(defparameter *actions* nil)
(defparameter *factgraph-table* nil)
(defparameter *fluentgraph-table* nil)
(defparameter *total-gactions* 0)


(defun pred-true-able (pred)
  (cond ((predicate-is-added (gethash pred *predicates*)) t)
	(t nil)))

(defun pred-false-able (pred)
  (cond ((predicate-is-deleted (gethash pred *predicates*)) t)
	(t nil)))

(defun pred-change-able (pred)
  (or (pred-true-able pred)
      (pred-false-able pred)))


;;==============================================================
;;Multiplying parameters for finding ground actions
;;==============================================================

;; Returns the parameter position of the predicate variables
;; Constants are not substituted
;; e.g (parameters ((a . type-a) (b . type-b))
;; the predicate (pred_1 (b . type-b) contant1) => (pred_1 1 constant1)
(defun oppred-argpos (operator oppred)
  (cons (car oppred) (mapcar #'(lambda (x)
				 (if (consp x)
				     (position x (action-parameters operator) :test #'equal)
				     x))
			     (cdr oppred))))


(defun oppred-substitution (operator oppred inst-args)
  (let ((pred-argpos (mapcar #'(lambda (x)
				 (if (consp x)
				     (position x (action-parameters operator) :test #'equal)
				 x))
			     (cdr oppred))))
    (cons (car oppred) (mapcar #'(lambda (pos) (if (numberp pos)
						   (nth pos inst-args)
						   pos))
			       pred-argpos))
    ))

(defun create-oppred-argpos ()
  (dolist (iop (dom-actions *pspace*))
    (setf (action-prec-argpos iop) (mapcar #'(lambda (pred)
					      (oppred-argpos iop pred)) 
					  (action-preconditions iop)))
    (setf (action-adds-argpos iop) (mapcar #'(lambda (pred)
					      (oppred-argpos iop pred)) 
					  (action-adds iop)))
    (setf (action-dels-argpos iop) (mapcar #'(lambda (pred)
					      (oppred-argpos iop pred)) 
					  (action-dels iop)))))
    
		   
;; OP-inertia. It retursn a list of (pred pos-arg1 ... pos-argn)
;; The list of positive inertia are those predicates that are static 
;; (can not become true, unless they are in the initial state)
(defun collect-op-inertiaprec (operator)
  (let ((op-inertia nil))
    (dolist (iprec (action-prec-argpos operator) op-inertia)
      (unless (pred-true-able (car iprec))
	(push iprec op-inertia)))))


;; @@-- Verificar si el estado inicial no ha sido modificado en *facts*
(defun verify-inertia-state (inertia ground-args &optional (facts *init-facts*))
  (when (gethash (cons (car inertia)
		       (mapcar #'(lambda (posarg) (nth posarg ground-args)) (cdr inertia)))
		 facts)

    t))

(defun verify-inertia-pos-state (inertia ground-args parampos &optional (facts *init-facts*))
  (when (gethash (cons (car inertia)
		       (mapcar #'(lambda (posarg) 
				   (if (numberp posarg) (nth (- posarg parampos) ground-args) posarg))
			       (cdr inertia)))
		 facts)
    
    t))





;;Returns the fact obtained from the substitution of the predicate positions in parameters
(defun literal-posparams (posargs instances)
  (cons (car posargs)
	(mapcar #'(lambda (posarg) 
		    (if (numberp posarg) (nth posarg instances) posarg))
		(cdr posargs))))


;;Returns the pred-pos for op-ground predicates. It creates the fact if it doesn't exist
(defun set-op-ground-preds (literal)
  (let ((fact (gethash literal *facts*)))
    (if (null fact)
	 (fact-predpos (create-fact literal))
	 (fact-predpos fact))))
	  

(defun build-oppred-inst(op-preds instances)
  (let ((inst-preds nil))
    (dolist (ipred op-preds (reverse inst-preds))
      (push (set-op-ground-preds (literal-posparams ipred instances)) inst-preds))))



(defun create-gaction-noparams (operator)
  (when (= (length (action-parameters operator)) 0)
    (make-gaction
     ;; DB added the list
     :planaction (list (action-name operator))
     :preconds (mapcar #'set-op-ground-preds (action-preconditions 
operator))
     :adds (mapcar #'set-op-ground-preds (action-adds operator))
     :dels (mapcar #'set-op-ground-preds (action-dels operator))
     )))
      

(defun create-gaction (operator instances)
  (when (= (length (action-parameters operator)) (length instances))
    (make-gaction
     :planaction (cons (action-name operator) instances)
     :preconds (build-oppred-inst (action-prec-argpos operator) instances)
     :adds (build-oppred-inst (action-adds-argpos operator) instances)
     :dels (build-oppred-inst (action-dels-argpos operator) instances)
     )))


;; It veryfies if adds and dels are equal for marking the action as irrelevant
(defun find-relevant-effects (gaction)
  (let ((relevant nil))
    (cond ((= (length (gaction-adds gaction)) (length (gaction-dels gaction)))
	   (dolist (ieff (gaction-adds gaction) relevant)
	     (unless (find ieff (gaction-dels gaction) :test #'equal)
	       (setf relevant t))))
	  (t t))))
  
				

(defun add-element-multi-list (lists objects)
  (cond ((null lists) (mapcar #'list objects))
	(t
	 (let ((new-lists nil))
	   (dolist (i-ob objects new-lists)
	     (dolist (j-list lists)
	       (let ((new-inst (copy-list j-list)))
		 (push (push i-ob new-inst) new-lists))))))))

;; It removes the inertia list if the arguments are not present
(defun inertia-of-args (inertia-list total-args used-args)
  (remove-if #'(lambda (inertia) 
		 (some #'(lambda (x) (and (numberp x)
					  (< x (- total-args used-args)))) (cdr inertia)))
	     inertia-list))
  

;; It returns the object list of the specified parameter
;; I check out if it is a unary inertia for avoiding irrelevants objects
(defun objects-for-multiply (op inertia param)
  (let* ((list-objects (gethash (cdr param) (problem-inheritobjects *current-problem*))))
    (cond ((null inertia) list-objects)
	  (t 
	   (let* ((param-pos (position param (action-parameters op) :test #'equal))
		  (unary-inertia (remove-if #'(lambda (in)
				      (or  (not (= (length (cdr in)) 1))
					   (and (numberp (second in))
						(not (= (second in) param-pos))))) inertia))  
		 (relevant-obs (copy-list list-objects)))
	     (dolist (uinertia unary-inertia relevant-obs)
	       (dolist (iob list-objects)
		 (unless (gethash (list (car uinertia) iob) *init-facts*)
		   (setf relevant-obs (remove iob relevant-obs))
		      ))))))))
    
	 

(defun multiexpand-op (op inertia expinst rparams viewed-inertia)
  (cond ((null rparams) expinst)
	 (t
	  (let* ((list-objects (objects-for-multiply op inertia (car rparams)))
		 (new-expinst (add-element-multi-list expinst list-objects))
		 (expand-iter (- (length (action-parameters op)) (length (cdr rparams))))
		 (inertia-positer (length (cdr rparams)))
		 (this-inertia (remove-if #'(lambda (in) (or (find in viewed-inertia :test #'equal)
							     (= (length (cdr in)) 1)))
					  (inertia-of-args inertia (length (action-parameters op)) expand-iter)))
		 (relevant-inst nil))
;;	    (format t "~%  Dicarding IN ACTION: ~a" op)
	    (dolist (instargs new-expinst)
	      (let ((args-relevant t))
		(dolist (inertia this-inertia)
		  (unless (verify-inertia-pos-state inertia instargs inertia-positer)
;;		    (format t "~%  Dicarding action : ~a" instargs)
		    (setf args-relevant nil)))
		(when args-relevant
		  (push instargs relevant-inst))))
	    (multiexpand-op op inertia relevant-inst (cdr rparams) (append this-inertia viewed-inertia))
))))


(defun cost-operator-p (operator)
  (and (null (action-adds operator))
       (null (action-dels operator))
       (not (null (action-costs operator)))))


(defun prob-functor-p (numvar)
  (or (find (car numvar) (dom-real-functors *pspace*))
      (find (car numvar) (problem-artificial-vars *current-problem*))))

(defun add-ground-action-tospace (ground-action op)
  (when (gaction-p ground-action)
    (when (or (cost-operator-p op)
	      (find-relevant-effects ground-action))
      (setf (gaction-operator ground-action) op)
      (setf (gaction-int ground-action) *total-gactions*)
      (incf *total-gactions*)
      (setf (gaction-num-precs ground-action) (length (gaction-preconds ground-action)))
      (push ground-action *actions*))))
    
(defun multiply-inertia-operators ()
  (dolist (op (dom-actions *pspace*))
    (cond ((>= (length (action-parameters op)) 1)
	   (let* ((inertia-prec (collect-op-inertiaprec op))
		  (ground-args (multiexpand-op op inertia-prec nil (reverse (action-parameters op)) nil)))
	     (dolist (i-ground ground-args)
	       (add-ground-action-tospace (create-gaction op i-ground) op))))
	  ;;generating no-params operators as a single ground action
	    (t 
	     (add-ground-action-tospace (create-gaction-noparams op) op)))))


;;=============================================================================
;; Instantiating Numeric Preconditions & Costs
;;=============================================================================

;; This three functions return as variables the real-functor vars and substitute the constants
(defun inst-gaction-funeval (operand gaction)
  (let ((istate (problem-init-state *current-problem*))
	(state-variable nil))
    (cond ((numberp operand) operand)
	  ((hkey-present (car operand) istate)
	   (cond ((not (prob-functor-p operand))
		  (setf state-variable (fluent-state-value (oppred-substitution (gaction-operator gaction) operand
										(cdr (gaction-planaction gaction))) istate))
		  (if (numberp state-variable) state-variable 0))
		 (t
		  (let ((fluent-lit (oppred-substitution (gaction-operator gaction) 
							 operand (cdr (gaction-planaction gaction)))))
		    (fluent-predpos (gethash fluent-lit *fluents*))))))

	  (t 
	   (inst-funeval-aux operand gaction)
	   ))))


(defun map-gaction-inst-funeval (operand-list gaction)
  (mapcar (lambda (ioperand)
	    (inst-gaction-funeval ioperand gaction))
	  operand-list))


(defun inst-funeval-aux (ifun gaction)
  (let ((temp-operand (map-gaction-inst-funeval (cdr ifun) gaction)))
    (cond ((every #'numberp temp-operand)
	   (apply (car ifun) temp-operand))
 	  ((and (some #'numberp temp-operand)
		(some #'is-linearmath-operation temp-operand))
 	   (transform-fun-notation (car ifun) temp-operand))
	  (t 
	   (push (car ifun) temp-operand)))))



(defun artificial-fun-exp (expression sign-exp)
  (cond ((numberp expression)
	 (* expression (if sign-exp 1 -1)))
	((or (functor-p expression)
	     (find (car expression) (problem-negative-vars *current-problem*)))
	 (cond ((not sign-exp)
		(cond ((negative-var-p expression)
		       (cons (negate-var (car expression)) (cdr expression)))
		      (t (list '* -1 expression))))
	       (t expression)))
	((eq (car expression) '-)
	 (list '+ (artificial-fun-exp (second expression) sign-exp) 
	       (artificial-fun-exp (third expression) (not sign-exp))))
	(t
	 (list (car expression) (artificial-fun-exp (second expression)
sign-exp) 
	       (artificial-fun-exp (third expression) sign-exp)))))

;; (defun artificial-fun-exp (expression sign-exp)
;;   (cond ((numberp expression)
;; 	 (* expression (if sign-exp 1 -1)))
;; 	((or (functor-p expression)
;; 	     (find (car expression) (problem-negative-vars *current-problem*)))
;; 	 (cond ((not sign-exp)
;; 		(cond ((negative-var-p expression)
;; 		       (cons (negate-var (car expression)) (cdr expression)))
;; 		      (t (list '* -1 expression))))
;; 	       (t expression)))
;; 	((eq (car expression) '-)
;; 	 (list '+ (artificial-fun-exp (second expression) sign-exp) 
;; 	       (artificial-fun-exp (third expression) (not sign-exp))))
;; 	(t
;; 	 (list (car expression) (artificial-fun-exp (second expression) sign-exp) 
;; 	       (artificial-fun-exp (third expression) t)))))



(defun actions-artificial-exp (action-slot)
 (let ((artificial-exp-table (make-hash-table :test #'eq)))
   (dolist (iaction (dom-actions *pspace*) artificial-exp-table)
     (dolist (i-expression (funcall action-slot iaction))
       (push (artificial-fun-exp i-expression t) (gethash (action-name-id iaction) artificial-exp-table)))
     (setf (gethash (action-name-id iaction) artificial-exp-table)
	   (reverse (gethash (action-name-id iaction) artificial-exp-table))))))
 

;; I will eliminate precfuns where ( +number > 0)
;; and will give a warning with ( -number > 0)
(defun reduce-instprecfuns (inst-precfuns igaction)
  (mapcan (lambda (i-instprecfun)
	    (cond ((consp i-instprecfun) (list i-instprecfun))
		  ((and (numberp i-instprecfun)
			(<= i-instprecfun 0))
		   (format t "~%<SAYPHI Warning>~% >>Action:~a will never become true" (gaction-planaction igaction)))))
	  inst-precfuns))


;; It's done after multiplying operators, but it could be integrated in the same algorithm.
(defun instantiate-precfuns ()
  (let ((artificial-precfuns (actions-artificial-exp #'action-precond-funs)))
    (dolist (igaction *actions*)
    (setf (gaction-precfuns igaction)
	  (reduce-instprecfuns (mapcar #'(lambda (ifun)
					   (list (car ifun) (inst-gaction-funeval (second ifun) igaction)))
				       (gethash (action-name-id (gaction-operator igaction)) 
						artificial-precfuns)
				       ) igaction))
    (setf (gaction-num-precfuns igaction) (length (gaction-precfuns igaction)))
    )))


(defun change-to-negvar (expression)
  (cons (negate-var (car expression)) (cdr expression)))


;; It returns a hash-table for the action costs values for the current problem
(defun actions-normal-and-artificial-costs ()
 (let ((all-costs-table (make-hash-table :test #'eq)))
   (dolist (iaction (dom-actions *pspace*) all-costs-table)
     (dolist (icost (action-costs iaction))
       (when (find (car (second icost)) (problem-negative-vars *current-problem*))
	 (cond ((eq 'increase (car icost))
		(push (list 'decrease (change-to-negvar (second icost)) (third icost)) 
		      (gethash (action-name-id iaction) all-costs-table)))
		((eq 'decrease (car icost))
		 (push (list 'increase (change-to-negvar (second icost)) (third icost)) 
		       (gethash (action-name-id iaction) all-costs-table)))
		((eq 'assign (car icost))
		 (push (list 'assign (change-to-negvar (second icost)) (list '* -1  (third icost))) 
		       (gethash (action-name-id iaction) all-costs-table)))))
       (push icost (gethash (action-name-id iaction) all-costs-table))))))

       


(defun instantiate-costs ()
  (let ((problem-cost-table (actions-normal-and-artificial-costs)))
    (dolist (igaction *actions*)
      (setf (gaction-costs igaction)
	    (mapcar #'(lambda (icost)
			(let ((fluent-lit (oppred-substitution (gaction-operator igaction) (second icost)
							       (cdr (gaction-planaction igaction)))))
			  ;; 			(format t "~% >> Fluent: ~a" fluent-lit)
			  (list (car icost)
				(fluent-predpos (gethash fluent-lit *fluents*))
				(cond ((inst-gaction-funeval (third icost) igaction))
				      (t 0)))))
		    (gethash (car (gaction-planaction igaction)) problem-cost-table)
;; 		    (action-costs (gaction-operator igaction))
		    )))))



;; ============================================================================
;; Instantiating Numeric Goals  (like inst-gaction-funeval, but without substitution
;; ============================================================================

(defun inst-numgoal-eval (operand)
  (let ((istate (problem-init-state *current-problem*))
	(state-variable nil))
    (cond ((numberp operand) operand)
	  ((hkey-present (car operand) istate)
	   (cond ((not (prob-functor-p operand))
		  (setf state-variable (fluent-state-value operand istate))
		  (if (numberp state-variable) state-variable 0))
		 (t
		  (fluent-predpos (gethash operand *fluents*)))))
	  (t 
	   (numgoal-inst-aux operand)))))


(defun map-numgoal-inst (operand-list)
  (mapcar (lambda (ioperand) 
	    (inst-numgoal-eval ioperand))
	  operand-list))

(defun numgoal-inst-aux (ifun)
  (let ((temp-operand (map-numgoal-inst (cdr ifun))))
    (cond ((every #'numberp temp-operand)
	   (apply (car ifun) temp-operand))
 	  ((and (some #'numberp temp-operand)
		(some #'is-linearmath-operation temp-operand))
 	   (transform-fun-notation (car ifun) temp-operand))
	  (t 
	   (push (car ifun) temp-operand)))))

(defun instantiate-numgoals ()
  (setf (problem-numeric-goals *current-problem*)  
	(mapcar #'(lambda (goal-exp) 
		    (list (car goal-exp) (inst-numgoal-eval (second goal-exp))))
		(problem-numeric-goals *current-problem*)))
  (dolist (i-numgoal (problem-numeric-goals *current-problem*))
    (let ((pos-numvar (car (extract-signed-vars (second i-numgoal) 'nil (dom-functors *pspace*))))
	  (neg-numvar (car (extract-signed-vars (second i-numgoal) 't (dom-functors *pspace*)))))
      (when pos-numvar 
	(pushnew pos-numvar (problem-positive-vars *current-problem*)))
      (when (and neg-numvar (not (find neg-numvar (problem-negative-vars *current-problem*))))
	(push neg-numvar (problem-negative-vars *current-problem*))
	(push (negate-var neg-numvar) (problem-artificial-vars *current-problem*))
	(setf (gethash (negate-var neg-numvar) (dom-functors *pspace*))
	      (gethash neg-numvar (dom-functors *pspace*)))
	(update-for-artificial-numgoals neg-numvar))
      ))
  
;;   Finally, we change artificial vars in numeric goals
  (setf (problem-numeric-goals *current-problem*)
	(mapcar (lambda (numgoal)
		  (list (car numgoal) (artificial-fun-exp (second numgoal) t)))
		(problem-numeric-goals *current-problem*)))
  )

(defun update-for-artificial-numgoals (numvar)
  (let* ((negvar (negate-var numvar))
	 (count-objlist (mapcar #'(lambda (var) 
				    (length (gethash (cdr var) (problem-inheritobjects *current-problem*))))
				(gethash negvar (dom-functors *pspace*)))) 
	 (map-len (if (null count-objlist) 1 (apply #'* count-objlist))))
    (setf (gethash negvar (problem-patterns *current-problem*)) (make-array map-len :initial-element nil))
    (setf (gethash negvar (problem-init-state *current-problem*)) (make-array map-len :initial-element nil))
    ))

;;=============================================================================
;; Computing the connectivity graph
;;=============================================================================

(defun build-factgraph-table()
  (setf *factgraph-table* (make-hash-table :test #'eq))
  (setf *fluentgraph-table* (make-hash-table :test #'eq))
  (maphash #'(lambda (key statemap)
	       (cond ((bit-vector-p statemap)
		      (let ((pred-factgraph (make-array (length statemap))))
			(dotimes (i (length statemap))
			  (setf (aref pred-factgraph i) (make-factgraph)))
			
			(setf (gethash key *factgraph-table*) pred-factgraph)))
		     (t
		      (let ((pred-fluentgraph (make-array (length statemap))))
			(dotimes (i (length statemap))
			  (setf (aref pred-fluentgraph i) (make-fluentgraph)))
			(setf (gethash key *fluentgraph-table*) pred-fluentgraph)))))
	   (problem-init-state *current-problem*)))

;; Direct access to the factgraph and fluentgraph tables
(defmacro fact-of-predpos (pred-pos)
  `(aref (gethash (car ,pred-pos) *factgraph-table*) (cdr ,pred-pos)))
(defmacro fluent-of-predpos (pred-pos)
  `(aref (gethash (car ,pred-pos) *fluentgraph-table*) (cdr ,pred-pos)))

(defun fluent-of-precfun (precfun-exp)
  (let ((precfun (second precfun-exp)))
     (cond ((listp (second precfun)) (fluent-of-predpos (second precfun)))
	  ((listp (third precfun)) (fluent-of-predpos (third precfun))))))

(defun set-factgraph ()
  (dolist (iaction *actions*)
    (dolist (iprec (gaction-preconds iaction))
      (push iaction (fg-precond-of (fact-of-predpos iprec))))
    (dolist (iadd  (gaction-adds iaction))
      (push iaction (fg-added-by (fact-of-predpos iadd))))
    (dolist (idel (gaction-dels iaction))
      (push iaction (fg-deleted-by (fact-of-predpos idel))))))
						 
;; changed by DB  
(defun set-fluentgraph ()
   (dolist (iaction *actions*)
     (let ((n-precfun 0))
       (dolist (iprecfun (gaction-precfuns iaction))
	 (when (listp (second iprecfun))
	 (push (cons iaction n-precfun) (flg-precfun-of (fluent-of-precfun iprecfun)))
	 (incf n-precfun)
	 )
	 ))
     (dolist (icost  (gaction-costs iaction))
       (push iaction (flg-changed-by (fluent-of-predpos (second icost)))))))
      

;; (defun new-range-layers-table (numvar-ranges)
;;   (let ((layers-table (make-hash-table)))
;;     (dolist (i-cut numvar-ranges layers-table)
;;       (setf (gethash i-cut layers-table) most-positive-fixnum))))


;; (defun set-fluentgraph-numvars-cut()
;;   (maphash #'(lambda (numvar fluentgraph-map)
;; 	       (when (prob-functor-p (list numvar)) 
;; 		 (let ((var-range (gethash numvar (problem-numvars-range *current-problem*))))
;; 		   (dotimes (i-flg (length fluentgraph-map))
;; 		     (let ((flg (aref fluentgraph-map i-flg)))
;; 		       (setf (flg-range-layers flg) (new-range-layers-table var-range)))))))
;; 	       *fluentgraph-table*))



;;======================================================================

(defun instantiate-operators ()
  (setf *actions* nil)
  (setf *total-gactions* 0)
  (create-oppred-argpos)
  (multiply-inertia-operators)
  (build-factgraph-table)
  (set-factgraph)
  (instantiate-precfuns)
  (instantiate-costs)
  (set-fluentgraph)
;;   (split-numvars-range)
;;   (set-fluentgraph-numvars-cut)
  (set-fluent-maxneeded)
)


;;============================================================================
;; Functions for Hashing Stated for Duplicate Search
;;============================================================================


(defun total-predschemas-bits ()
  (let ((total-bits 0))
    (maphash #'(lambda (key varmap)
		 (declare (ignore key))
		 (setf total-bits (+ total-bits (length varmap))))
	     (problem-patterns *current-problem*))
    total-bits))

(defun pred-for-hashing (nbits)
  (let ((add-lists (mapcar #'action-adds (dom-actions *pspace*)))
	(pred-selected nil) (pred-maxlowerbound nil))
    (dolist (i-addlist add-lists)
      (dolist (i-add i-addlist)
	(cond ((>= (length (gethash (car i-add) (problem-patterns *current-problem*))) nbits)
	       (setf pred-selected (car i-add)))
	      (t
	       (when (or (null pred-maxlowerbound) 
			  (> (length (gethash (car i-add) (problem-patterns *current-problem*)))
			     (length (gethash pred-maxlowerbound (problem-patterns *current-problem*)))))
		      (setf pred-maxlowerbound (car i-add)))
	       
	       ))))
    (if (not (null pred-selected)) pred-selected pred-maxlowerbound)))

      
(defun create-random-hash-matcher (nbits)
  (let ((hash-matcher (make-array nbits :element-type 'bit :initial-element 0)))
    (dotimes (ibit nbits hash-matcher)
      (when (= 1 (random 2))
	(setf (aref hash-matcher ibit) 1)))))
	


(defun set-duplicate-hashing ()
  (let* ((bits-hashing (min (round (/ (total-predschemas-bits) 5)) (logcount most-positive-fixnum)))
	(pred-for-hashing (pred-for-hashing bits-hashing)))
    (setf (getf (problem-plist *current-problem*) :duphash-pred) pred-for-hashing)
    (setf (getf (problem-plist *current-problem*) :duphash-matcher) 
	  (create-random-hash-matcher (min bits-hashing (length (gethash pred-for-hashing (problem-patterns *current-problem*))))))
    (setf *hash-duplicates* (make-hash-table))
    ))
