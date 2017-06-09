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
;; Description: Some support functions for learning systems
;; Date: 2008.05.29
;; 
;; ========================================================================


;; This recieves a list and selects randomly a subset of n elements from it

(defun choose-n (list n)
  (let* ((rnd-elements (do ((elements nil))
			   ((= n (length elements)) elements)
			 (pushnew (random (length list)) elements)))
	 (n-selected nil))
    (dolist (i rnd-elements n-selected)
      (push (nth i list) n-selected))))
       


(defun read-pddl-definition (path)
  (with-open-file (istream path :direction :input)
    (read istream)))

;; property can be either: problem, :domain, :objects, :init or :goal
(defun pddl-problem-property (property pddl-problem-description)
  (funcall (if (member property '(problem :domain :goal)) #'cadr #'cdr)
	   (assoc property (cdr pddl-problem-description))))


(defun write-pddl-file (name domain-name objects state goals file &optional metric)
  (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "(define (problem ~(~a~)) (:domain ~(~a~))" name domain-name)
    (format stream "~%  (:objects")
    (do ((instances objects (cdr instances)))
	((null instances))
      (cond ((eq (car instances) '-)
	     (format stream " - ~(~a~)~%~12T" (cadr instances))
	     (setq instances (cdr instances)))
	    (t (format stream " ~(~a~)" (car instances)))))
    (format stream ")")
    (format stream "~%  (:init")
    (pp-list state 7 stream t)
    (format stream ")")
    (format stream "~%  (:goal (and ")
    (pp-list (cond ((eq (car goals) 'and)
		    (cdr goals))
		   ((listp (car goals)) goals)
		   (t (list goals)))
	     7 stream t)
    (format stream "))")
    (if metric
	(format stream "~%  ~s" (cons :metric metric)))
    (format stream ")")))

