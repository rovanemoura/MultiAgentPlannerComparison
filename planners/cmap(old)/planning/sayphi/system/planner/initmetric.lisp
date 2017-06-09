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
;; Description: Heuristic Functions for Numeric Optimization Task
;; I put here initializing function for metric purpose
;; Date: 2006.10.25
;; ;; ========================================================================


;; Extracting the cutting point of (> (*sign var value)  0)
(defun split-cut-point (sign value)
  (cond ((eq sign '+) (* -1 value))
	((eq sign '-) value)))


(defun is-linearmath-operation (operand)
  (and (listp operand)
       (or (eq (car operand) '+)
	   (eq (car operand) '-))))


(defun mult-signos (signo1 signo2)
  (cond ((and (eq signo1 '+)(eq signo2 '+)) '+)
	((and (eq signo1 '+)(eq signo2 '-)) '-)
	((and (eq signo1 '-)(eq signo2 '+)) '-)
	((and (eq signo1 '-)(eq signo2 '-)) '+)))


;; Substitutes like sublis. Constants are retrieved from the constant poslists
(defun sayargs-sublis (inst params) 
  (mapcar #'(lambda (i-param)
	      (if (consp i-param)
		  (cons (cdr (assoc (car i-param) inst)) (cdr i-param))
		  (gethash i-param (problem-constant-poslist *current-problem*))
		  ))
	  params))


(defun fun-state-pos (args varpos)
  (cond ((null args) 0)
	(t
	 (let* ((predfun-inst (sayargs-sublis varpos args))
		(objects (problem-inheritobjects *current-problem*))
		(inst-poslist (poslist-from-typedinst predfun-inst objects)))
	   (map-position inst-poslist)))))


;; (defun insert-artificial-costs()
;;   (let ((temp-costs nil))
;;     (dolist (iaction (dom-actions *pspace*))
;;       (dolist (icost (action-costs iaction))
;; 	(when (find (car (second icost)) (problem-negative-vars *current-problem*))
;; 	  (cond ((eq 'increase (car icost))
;; 		 (push (list 'decrease (change-to-negvar (second icost)) (third icost)) temp-costs))
;; 		((eq 'decrease (car icost))
;; 		 (push (list 'increase (change-to-negvar (second icost)) (third icost)) temp-costs))
;; 		((eq 'assign (car icost))
;; 		 (push (list 'assign (change-to-negvar (second icost)) (list '* -1  (third icost))) temp-costs)))
;; 	  (unless (find (negate-var (car (second icost))) (dom-real-functors *pspace*))
;; 	    (push (negate-var (car (second icost))) (dom-real-functors *pspace*)))))
;;       (setf (action-costs iaction)
;; 	    (append (action-costs iaction) temp-costs))
;;       (setf temp-costs nil))))


;;Create the fluents for the negative-vars
;; Insert the opposite values of the negative vars into the new artificial values for relaxation
(defun update-artificial-initstate()
  (maphash #'(lambda (literal fluent)
	       (when (find (car literal) (problem-negative-vars *current-problem*))
		 (create-fluent (list (cons (negate-var (car literal)) (cdr literal)) 
				      (* -1 (fluent-init-value fluent))))))
	   *init-fluents*)
  (dolist (ivar (problem-negative-vars *current-problem*))
    (let ((negvar-values (gethash (negate-var ivar) (problem-init-state *current-problem*)))
	  (var-values (gethash ivar (problem-init-state *current-problem*)))
	  (var-patterns (gethash (negate-var ivar) (problem-patterns *current-problem*))))
      (dotimes (ipos (length var-values))
	(setf (aref negvar-values ipos) (* -1 (aref var-values ipos)))
	(setf (aref var-patterns ipos)  0)))))
	  
;;====modified for new representation
;; (defun split-numvars-range ()
;;   (let ((numvars-range (make-hash-table :test #'equal))
;; 	(range-cut nil) (range-var nil))
;;     (dolist (iaction *actions*)
;;       (dolist (i-precfun-exp (gaction-precfuns iaction))
;; 	(let ((i-instprecfun (second i-precfun-exp)))
;; 	  (when (listp i-instprecfun)
;; 	    (cond ((numberp (second i-instprecfun))
;; 		   (setf range-var (car (third i-instprecfun)))
;; 		   (setf range-cut (split-cut-point (car i-instprecfun) (second i-instprecfun))))
;; 		  ((numberp (third i-instprecfun))
;; 		   (setf range-var (car (second i-instprecfun)))
;; 		   (setf range-cut (split-cut-point (car i-instprecfun) (third i-instprecfun)))))
;; 	    (pushnew range-cut (gethash range-var numvars-range))
;; 	    ))))
;;     (dolist (inumgoal-exp (problem-numeric-goals *current-problem*))
;;       (let ((i-numgoal (second inumgoal-exp)))
;; 	(cond ((numberp (second i-numgoal))
;; 	       (setf range-var (car (third i-numgoal)))
;; 	       (setf range-cut (split-cut-point (car i-numgoal) (second i-numgoal))))
;; 	      ((numberp (third i-numgoal))
;; 	       (setf range-var (car (second i-numgoal)))
;; 	       (setf range-cut (split-cut-point (car i-numgoal) (third i-numgoal)))))
;; 	(pushnew range-cut (gethash range-var numvars-range))
;; 	))
;;     (maphash #'(lambda (key range-list)
;; 		 (setf (gethash key numvars-range) (sort range-list #'<)))
;; 	     numvars-range)
;;     (setf (problem-numvars-range *current-problem*) numvars-range)))
;; 


(defun precfun-set-maxneeded (i-instprecfun)
  (let ((flg (fluent-of-predpos (cond ((numberp (second i-instprecfun)) (third i-instprecfun))
				      ((numberp (third i-instprecfun)) (second i-instprecfun)))))
	(cut-value (split-cut-point (car i-instprecfun) (cond ((numberp (second i-instprecfun)) (second i-instprecfun))
							      ((numberp (third i-instprecfun)) (third i-instprecfun))))))
    (when (or (null (flg-max-needed flg))
	      (< (flg-max-needed flg) cut-value))
	     (setf (flg-max-needed flg) cut-value))))


(defun set-fluent-maxneeded ()
  (dolist (iaction *actions*)
    (dolist (i-precfun-exp (gaction-precfuns iaction))
      (let ((i-instprecfun (second i-precfun-exp)))
	(when (listp i-instprecfun)
	  (precfun-set-maxneeded i-instprecfun)))))
  (dolist (inumgoal-exp (problem-numeric-goals *current-problem*))
      (let ((i-numgoal (second inumgoal-exp)))
	(precfun-set-maxneeded i-numgoal)
	)))



;; This function is kind of wierd. I have not find a better way to simplify the formula with one variable
(defun transform-fun-notation (operator operands)
  (let ((fun-operand nil) (res-numbers 0)
	(res-var 0) (res-signo nil))
    (cond ((and (is-linearmath-operation (first operands))
		(numberp (second operands)))
	   (setf fun-operand (first operands))
	   (setf res-numbers (funcall (symbol-function operator) res-numbers (second operands)))
	   
	   (cond ((numberp (second fun-operand))             ;; (+ (- 5 x) 10)
		  (setf res-numbers (+ res-numbers (second fun-operand)))
		  (setf res-signo (car fun-operand))
		  (setf res-var (third fun-operand)))
		 ((numberp (third fun-operand))                    
		  (setf res-numbers (funcall (symbol-function (car fun-operand)) res-numbers (third fun-operand)))
		  (setf res-signo '+)
		  (setf res-var (second fun-operand)))))
	  ((and (is-linearmath-operation (second operands))
		(numberp (first operands)))
	   (setf fun-operand (second operands))
	   (setf res-numbers (+ res-numbers (first operands)))
	   
	   (cond ((numberp (second fun-operand))             ;; (- 10 (- 5 x))
		  (setf res-numbers (funcall (symbol-function operator) res-numbers (second fun-operand)))
		  (setf res-signo (mult-signos (car fun-operand) operator))
		  (setf res-var (third fun-operand)))
		 ((numberp (third fun-operand))                    
		  (setf res-numbers (funcall (symbol-function (mult-signos (car fun-operand) operator)) 
					     res-numbers (third fun-operand)))
		  (setf res-signo operator)
		  (setf res-var (second fun-operand))))))
    (list res-signo res-numbers res-var)))



;; This three functions return as variables the real-functor vars and substitute the constants
(defun inst-op-funeval (operand state varpos)
  (let ((real-functors (dom-real-functors *pspace*)) (state-variable nil))
    (cond ((numberp operand)
	   operand)
	  ((hkey-present (car operand) state)
	   (cond ((not (find (car operand) real-functors))
		  (setf state-variable (functor-state-value (car operand) (cdr operand) state varpos))
		  (if (numberp state-variable) state-variable 0))
		 (t
		  (cons (car operand) (fun-state-pos (cdr operand) varpos)))))
	  (t 
	   (inst-fun-eval operand state varpos)
	   ))))


(defun map-operand-inst-funeval (operand-list state varpos)
  (mapcar (lambda (ioperand)
	    (inst-op-funeval ioperand state varpos))
	  operand-list))


(defun inst-fun-eval (ifun state varpos)
  (let ((temp-operand nil))
    (setf temp-operand (map-operand-inst-funeval (cdr ifun) state varpos))
    (cond ((every #'numberp temp-operand)
	   (apply (car ifun) temp-operand))
 	  ((and (some #'numberp temp-operand)
		(some #'is-linearmath-operation temp-operand))
 	   (transform-fun-notation (car ifun) temp-operand))
	  (t 
	   (push (car ifun) temp-operand)))))


;; The variable cx_state is a placeholder to build the clousure in the instprecfun-clousure macro
(defun precfun-operand-expression (iprecfun)
  (cond ((numberp iprecfun) iprecfun)
	((hkey-present (car iprecfun) (problem-init-state *current-problem*))
	 (let ((functor (car iprecfun))
	       (funpos (cdr iprecfun)))
	   `(aref (gethash (quote ,functor) cx_state) ,funpos)))
	((math-operation-p iprecfun)
	 (list (car iprecfun) 
	       (precfun-operand-expression (second iprecfun))
	       (precfun-operand-expression (third iprecfun))))))


(defun instprecfun-clousure (iprecfuns)
  (let ((iprecgoal-exps (mapcar #'(lambda (x) (list '> (precfun-operand-expression x) 0)) 
				      iprecfuns)))
    (push 'and iprecgoal-exps)
    (eval `(lambda (cx_state) ,iprecgoal-exps))))


(defmacro instprecfun-clousure2 (iprecfuns)
  (let ((iprecgoal-exps (mapcar #'(lambda (x) (list '> (precfun-operand-expression x) 0)) 
				      iprecfuns)))
    (push 'and iprecgoal-exps)
    `(lambda (cx_state) ,iprecgoal-exps)))


