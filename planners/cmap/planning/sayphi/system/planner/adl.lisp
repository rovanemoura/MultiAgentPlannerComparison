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
;; Author: Tomas de la Rosa 
;; Description: ADL Implementation (not completed!!)
;; Date: 2007.09.14
;; 
;; ========================================================================

(defun lformula-copier (lf)
  (make-logicformula
   :connector (lformula-connector lf)
   :quantivar (lformula-quantivar lf)
   :predicate (lformula-predicate lf)
   :sons (mapcar #'lformula-copier (lformula-sons lf)))
)




;; @@Actualizar adds y dels con los negated predicates
(defun create-negated-pred (lit-predicate)
  (let ((neg-predicate (intern (format nil "_NOT_~a" (symbol-name (car lit-predicate)))))
	(predicate (gethash (car lit-predicate) *predicates*)))
    (unless (gethash neg-predicate *predicates*)
      (setf (gethash neg-predicate *predicates*)
	    (make-predicate :name neg-predicate
			    :num-args (predicate-num-args predicate) 
			    :args-params (predicate-args-params predicate)
			    :is-added (predicate-is-deleted predicate)
			    :is-deleted (predicate-is-added predicate))))
    (cons neg-predicate (cdr lit-predicate))
    ))


(defun negate-formula (lf)
  (make-logicformula :connector 'not
		     :sons (list lf)))

	    

;; Compilation Step 1: Quatifiers are expanded with available objects
(defun expand-quanti-logformula (logformula qvar xobject)
  (cond ((member (lformula-connector logformula) '(forall exists))
	 (cond ((not (null qvar))
		(expand-quanti-logformula (car (lformula-sons logformula)) qvar xobject))
	       (t
		(setf (lformula-connector logformula) 
		      (if (eq (lformula-connector logformula) 'forall) 'and 'or))
		(let ((lf-tmp (lformula-copier (car (lformula-sons logformula)))))
		  (setf (lformula-sons logformula)
			(mapcar #'(lambda (iobject)
				    (let ((lf-son-copy (lformula-copier lf-tmp )))
;; 				      (logformula-tree-print lf-son-copy)
				      (expand-quanti-logformula lf-son-copy (lformula-quantivar logformula) iobject)))
				(gethash (cdr (lformula-quantivar logformula)) *objects*))))
 		(dolist (i-lf (lformula-sons logformula))
 		  (expand-quanti-logformula i-lf nil nil))
		)))
	((member (lformula-connector logformula) '(and or not))
	 (dolist (i-lf (lformula-sons logformula))
	   (expand-quanti-logformula i-lf qvar xobject))) 
	((and (eq (lformula-connector logformula) 'predicate)
	      (not (null qvar)))
	 (setf (lformula-predicate logformula) (subst xobject qvar (lformula-predicate logformula) :test #'equal))))
  logformula)

;; Compilation Step 2: Nots Expansion ~(p or q) -> (~p and ~q)
;;                                    ~(p and q) -> (~p or ~q)
(defun expand-nots-logformula (logformula)
  (cond ((eq (lformula-connector logformula) 'predicate)
	 logformula)
	((find (lformula-connector logformula) '(and or))
	 (dolist (i-lf (lformula-sons logformula))
	   (expand-nots-logformula i-lf)))
	((eq (lformula-connector logformula) 'not)
	 (let ((neglf (car (lformula-sons logformula))))
	   (cond ((eq (lformula-connector neglf) 'not)
		  (setf (lformula-connector logformula) (lformula-connector (car (lformula-sons neglf))))
		  (setf (lformula-predicate logformula) (lformula-predicate (car (lformula-sons neglf))))
		  (setf (lformula-sons logformula) (lformula-sons (car (lformula-sons neglf)))))
		 ((find (lformula-connector neglf) '(and or))
		  (setf (lformula-connector logformula)
			(if (eq (lformula-connector neglf) 'and) 'or 'and))
		  (setf (lformula-sons logformula)
			(mapcar #'negate-formula (lformula-sons neglf)))
		  (dolist (i-lf (lformula-sons logformula))
		    (expand-nots-logformula i-lf)))
		 ((eq (lformula-connector neglf) 'predicate)
		  (setf (lformula-connector logformula) 'predicate)  
		  (setf (lformula-predicate logformula) (create-negated-pred (lformula-predicate neglf)))
		  (setf (lformula-sons logformula) nil)
		  )
		 ))))
  logformula
  )



 (defun simplify-or-and-logformula (lf)
   (when (find (lformula-connector lf) '(and or))
     (dolist (ison (lformula-sons lf))
       (simplify-or-and-logformula ison))
     (do ((i-son (car (lformula-sons lf)) (car rest-sons))
	  (rest-sons (cdr (lformula-sons lf)) (cdr rest-sons)))
	 ((null i-son) nil)
       (when (eq (lformula-connector i-son) (lformula-connector lf))
	 (setf (lformula-sons lf)
	       (append (butlast (lformula-sons lf) (length rest-sons))
		       (lformula-sons i-son)
		       (cdr rest-sons)))))))
    


(defun adl-expand-domain()
  (setf *objects* (problem-inheritobjects *current-problem*))
  (dolist (iop *operators*)
    (expand-quanti-logformula (op-preconds iop) nil nil)
    (expand-nots-logformula (op-preconds iop))
    (simplify-or-and-logformula (op-preconds iop))
    ))


(defun get-semi-instpred-vector (instpred)
;;   @@-- Cambiar por el bitmap real de un predicate semi-instantiated
  (make-array (length (gethash (car instpred) (problem-init-state *current-problem*)))
	      :element-type 'bit
	      :initial-element 1))

;; Returns n(p ->a): The number of initial literals that satisfies the unification of
;; semi-instantiated predicate
(defun num-initial-unifpred (instpred)
  (cond ((gethash instpred *semi-inst-pred*))
	(t (let* ((ipred-bitmap (get-semi-instpred-vector instpred))
		  (n-initial-unif (count 1 (bit-and ipred-bitmap 
						  (gethash (car instpred) (problem-init-state *current-problem*))))))
	     (setf (gethash instpred *semi-inst-pred*)
		   (cons ipred-bitmap n-initial-unif))))))


;; (defun inertia-process ()
;;   (setf *semi-inst-pred* (make-hash-table :test #'equal))
;;   
;;   )

(defun logformula-tree-print (lf &optional (indent 0))
  (format t  "~% ~a ~a"  (say-indent indent) (lformula-connector lf))
  (unless (null (lformula-predicate lf))
    (format t " <~a> " (lformula-predicate lf)))
  (unless (null (lformula-quantivar lf))
    (format t " <~a> " (lformula-quantivar lf)))
  (dolist (ison (lformula-sons lf))
    (logformula-tree-print ison (+ 1 indent))))



