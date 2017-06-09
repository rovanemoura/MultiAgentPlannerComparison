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
;; Description: Additional parsing functions
;; Date: 2007.09.13
;; 
;; ========================================================================

(defparameter *operators* nil)
(defparameter *ioperators* nil)
(defparameter *objects* nil)
(defparameter *predicates* nil)

;; Getting if predicates are added or deleted
(defun collect-top-inertia()
  (dolist (iop (dom-actions *pspace*))
    (dolist (iadd (action-adds iop))
      (setf (predicate-is-added (gethash (car iadd) *predicates*)) t))
    (dolist (idel (action-dels iop))
      (setf (predicate-is-deleted (gethash (car idel) *predicates*)) t))))
      


;;Revisar: Es posible que no sea necesario (dom-predicates) luego de utilizar esta estructura
(defun create-predicate-table ()
  (let ((pred-table (make-hash-table :test #'eq)))
    (maphash #'(lambda (pred args)
		 (setf (gethash pred pred-table)
		       (make-predicate :name pred
				       :num-args (length args)
				       :args-params args)))
	     (dom-predicates *pspace*))
    (setf *predicates* pred-table)
       ))



;; Equal to convert-literal, but without check-error
(defun pred-typed-params (literal parameters)
  (cons (car literal) (mapcar #'(lambda (var)
				  (let ((say-var (convert-pddl-var var)))
				    (cond ((assoc say-var parameters))
					  (t var))))
			      (cdr literal))))


;; AND OR NOT FORALL EXISTS ATOM
(defun build-logformula (formula-def params)
  (cond ((null formula-def) nil)
	((listp (car formula-def))
	 (cons (build-logformula (car formula-def) params)
	       (build-logformula (cdr formula-def) params)))
	((member (car formula-def) '(and or not))
	 (make-logicformula :connector (car formula-def)
			    :sons (build-logformula (cdr formula-def) params)))
	((member (car formula-def) '(forall exists))
	 (let ((qvar (compute-params (second formula-def))))
	   (make-logicformula :connector (car formula-def)
			      :quantivar (car qvar)
			      :sons (build-logformula (cddr formula-def) (append qvar params)))))
	(t (make-logicformula :connector 'predicate
			      :predicate (pred-typed-params formula-def params)))))


(defun parse-operators (operators-def)
  (setf *operators* nil)
  (dolist (op-def (reverse operators-def))
    (let ((params (compute-params (getf op-def :parameters))))
      (push (make-say-operator 
	     :name (getf op-def :action)
	     :params params
	     :preconds (build-logformula (append (getf op-def :precondition)
						 (getf op-def :condition)) params)
	     :effects (build-logformula (getf op-def :effect) params))
	  *operators*))))


  
      

 
  



