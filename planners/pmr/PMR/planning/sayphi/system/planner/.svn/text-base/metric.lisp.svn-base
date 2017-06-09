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
;; Date: 2006.09.08
;; 
;; ========================================================================


;; Some fluents are not explicitly defined in the problem file
;; I will return NIL to indicate the abscence of the fluent-literal
;; It might be handled in other functions to assign 0 or unknown value
(defun fluent-state-value (fluent-literal state)
  (let ((fluent (gethash fluent-literal *fluents*)))
    (when (fluent-p fluent)
      (let ((pred-pos (fluent-predpos fluent)))
	(aref (gethash (car pred-pos) state) (cdr pred-pos))))))


(defun math-operation-p (expression)
  (and (listp expression)
       (not (null (find (car expression) '(+ - * /))))))


(defun precfun-operand-state-eval (iprecfun state)
  (cond ((numberp iprecfun) iprecfun)
	((hkey-present (car iprecfun) state)
	 (functor-pos-state-value (car iprecfun) (cdr iprecfun) state))
	((math-operation-p iprecfun)
	 (funcall (car iprecfun) 
		  (precfun-operand-state-eval (second iprecfun) state)
		  (precfun-operand-state-eval (third iprecfun) state)))
	))


(defun increase-total-time (state)
  (let ((current-total-time (functor-state-value 'total-time 'nil state 'nil)))
    (change-fun-state state 'total-time 0 (1+ current-total-time))))


;; The reference state is normally the parent state used to compute dynamic costs.
;; This means that all non constant costs depends on previous state
(defun apply-gaction-costs (gaction state &key (relaxed nil)
                                               (reference-state nil))
  (when (hkey-present 'total-time state) 
    (increase-total-time state))

  (dolist (i-cost (gaction-costs gaction))
    (let ((current-value (numvar-predpos-state-value (second i-cost) state))
	  (delta-value (cond (reference-state (operand-funeval (third i-cost) reference-state))
			     (relaxed (operand-funeval (third i-cost) state))
			     (t (third i-cost))))
	  (pred-pos (second i-cost)))
      (cond ((eq 'increase (car i-cost))
	     (change-fun-state state (car pred-pos) (cdr pred-pos) (+ current-value delta-value)))
	    ((eq 'assign (car i-cost))
	     (unless (= delta-value current-value)
	       (when (or (and relaxed (> delta-value current-value))
			 (not relaxed))
		 (change-fun-state state (car pred-pos) (cdr pred-pos) delta-value))))
	    ((eq 'decrease (car i-cost))
	     (when (not relaxed)
	       (change-fun-state state (car pred-pos) (cdr pred-pos) (- current-value delta-value)))))
	    )))


;; It computes the difference of the numeric variables for determining the cost of the
;; action that produced the second state
(defun diff-num-state (state next-state)
  (let ((num-state (copy-state-set (problem-patterns *current-problem*) 'real-fluents))) 
    (maphash #'(lambda (pred statemap)
		 (unless (simple-bit-vector-p statemap)
		   (let ((costmap (gethash pred num-state))
			 (nextmap (gethash pred next-state)))
		     (when (hkey-present pred num-state)
		       (dotimes (ipos (length statemap))
			 (when (numberp (aref statemap ipos))
			   (setf (aref costmap ipos)
				 (- (aref nextmap ipos) (aref statemap ipos)))))))))
	     state)
    num-state))

					   
(defun action-cost-metric (state next-state)
  (cond ((is-metric-domain)
	 (let ((cost-state (diff-num-state state next-state))
	       (metric (problem-metric *current-problem*)))
	   (operand-funeval (cadr metric) cost-state)))
	(t 1)))



(defun map-operand-funeval (operand-list state)
  (mapcar (lambda (ioperand)
	    (operand-funeval ioperand state))
	  operand-list))


(defun fun-eval (ifun state)
   (apply (car ifun) (map-operand-funeval (cdr ifun) state)))

      
(defun operand-funeval (operand state)
  (cond ((numberp operand)
	 operand)
	((predpos-p operand state)
	 (numvar-predpos-state-value operand state))
	((hkey-present (car operand) state)
	 (fluent-state-value operand state))
	(t 
	 (fun-eval operand state))))



(defun compute-total-cost (sol-path problem)
  (declare (ignore problem))
  (let ((cost 0))
    (dolist (i-node sol-path cost)
      (setf cost (+ cost (snode-cost i-node))))
))

