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
;; Description: I'm trying to compute helpful actions without evaluating the whole graphplan
;; Date: 2008.04.10
;; 
;; ========================================================================



(defparameter *hp-state* nil)  ;; This is the statemap of facts somewhere needed
(defparameter *hp-facts* nil)  ;; The list of structures holding facts info.

(defstruct (hpfact)
  (int nil)
  (literal nil)
  (predpos nil)
  (factgraph nil)
  (direct-preconds nil)
  (need-count 0))


;; (defun build-hpfacts ()
;;   (let ((hp-table (make-hash-table :test #'equal)))
;;     (maphash #'(lambda (literal i-fact)
;; 		 (when (pred-change-able (car literal))
;; 		   (setf (gethash (fact-predpos i-fact) hp-table)
;; 			 (make-hpfact :int (fact-int i-fact)
;; 				      :literal literal
;; 				      :predpos (fact-predpos i-fact) 
;; 				      :factgraph (fact-of-predpos (fact-predpos i-fact))
;; 				      ))))
;; 	     *facts*)
;;     hp-table))
;; 

;; (defun hp-init (node)
;;   (setf *hp-facts* (build-hpfacts))
;;   (hp-set-global-helpful-actions node)
;;   (setf *hp-state* (hp-set-state (problem-goals *current-problem*))))

;; (defun hp-compute-applied (gaction)
;;   (dolist (i-prec (gaction-preconds gaction)



;; (defun hp-set-state (goals)
;;   (let ((hp-state (copy-state-set (problem-patterns *current-problem*)))
;; 	(inst-goals (get-instantiated-goals goals)))
;;     (dolist (i-goal inst-goals)
;;       (change-state hp-state (car i-goal) (cdr i-goal) 1))
;;     (maphash #'(lambda (pred-pos hp-fact)
;; 		 (when (> (hpfact-need-count hp-fact) 0)
;; 		   (change-state hp-state (car pred-pos) (cdr pred-pos) 1)))
;; 	     *hp-facts*)
;;     hp-state))
;;   

;; (defun hp-set-global-helpful-actions (node &optional (goals (problem-goals *current-problem*)))
;;   (let* ((pg (metric-relaxedplan node goals)))
;;     (dolist (i-action (pg-relaxed-plan pg))
;;       (dolist (i-add (gaction-adds i-action))
;; 	(dolist (i-prec (gaction-preconds i-action))
;; 	  (when (pred-change-able (car i-prec))
;; 	    (push i-prec (hpfact-direct-preconds (gethash i-add *hp-facts*)))
;; 	    (incf (hpfact-need-count (gethash i-prec *hp-facts*)))
;; 	    (format t "~%[GH]--> Incf ~a" (hpfact-literal (gethash i-prec *hp-facts*)))
;; 	    )))
;;       
;;       (format t "~% Action: ~a" (gaction-planaction i-action))
;;       (format t "~% ADDS: ~a" (gaction-adds i-action))
;;     )))
    

;; (defun hp-print-info ()
;;   (maphash #'(lambda (literal hpfact)
;; 	       (format t "~% FACT: ~a" (hpfact-literal hpfact))
;; 	       (format t "~% Direct preconds: ~a " (hpfact-direct-preconds hpfact))
;; 	       (format t "~% Need Count: ~a " (hpfact-need-count hpfact)))
;; 	   *hp-facts*))

	   
    
(defun find-helpful-from-hpstate (current-state &optional (hp-state *hp-state*))
  (let ((helpful-byte 0))
    (maphash #'(lambda (pred statemap)
		 (cond ((simple-bit-vector-p statemap)
			(dotimes (ibit (length statemap))
			  (when (and (= 1 (sbit statemap ibit))
				     (false-in-state current-state pred ibit))
			    (dolist (i-achiever (fg-added-by (fact-of-predpos (cons pred ibit))))
			      (set-instance-map helpful-byte (gaction-int i-achiever) 1)))))))
	     
	     hp-state)
    helpful-byte))
    

;; The helpful actions computed without calling the heuristic function 
;; is the intersection between the applicable actions and the actions
;; that achieve any fact in the global helpful state
(defun compute-helpful-from-hpstate (node)
  (setf (snode-helpful-byte node)
	(logand (set-applicable-byte node)
		(find-helpful-from-hpstate (snode-state node)))))



;; After applying an action we need to decrement the need count of a 
;; global-helpful fact.
;; If it is no needed anymore it should be removed from the global helpful state
;; (defun update-gh-state-for-action (gaction &optional (gh-state *hp-state*))
;;   (dolist (i-add (gaction-adds gaction))
;;     (format t "~%[HG]> Add:~a" i-add)
;;     (let ((hpfact (gethash i-add *hp-facts*)))
;;       (dolist (i-prec (hpfact-direct-preconds hpfact))
;; 	(let ((hpfact-prec (gethash i-prec *hp-facts*)))
;; 	  (format t "~%[HG]>>> Prec needed:~a (~d)"  (hpfact-literal hpfact-prec)
;; 		  (hpfact-need-count hpfact-prec))
;; 	  (decf (hpfact-need-count hpfact-prec))
;; 	  (when (= (hpfact-need-count hpfact-prec) 0)
;; 	    (format t "~%[HG]>>>>> Fact anymore GH ~a"  (hpfact-literal hpfact-prec))	    
;; 	    (change-state gh-state (car i-prec) (cdr i-prec) 0))))))
;; )


;; ==============================================================
;; FUNCTIONS FOR TAKING ADVANTAGES OF THE RELAXEDPLAN
;; ==============================================================


(defun mark-global-helpful-facts (pg) 
  (let* ((global-h-state (copy-state-set (problem-patterns *current-problem*))))
    (maphash #'(lambda (num-layer goal-layer)
		 (declare (ignore num-layer))
		 (dolist (i-goal goal-layer)
		   (change-state global-h-state (car i-goal) (cdr i-goal) 1)))
	     (pg-goal-layers pg))
    global-h-state))


;; This is a redefinition of the heuristic function of the relaxedplan
;; just for setting the global helpful facts
(defun h-relaxedplan-setglobal (node &optional (goals (problem-goals *current-problem*)))
  (let* ((pg (metric-relaxedplan node goals)))
    (cond ((plangraph-p pg)
	   (setf *hp-state* (mark-global-helpful-facts pg))
	   (values (length (pg-relaxed-plan pg)) 
		   (pg-relaxed-plan pg)))
	  (t (values most-positive-fixnum nil nil)))))


    


;;=======================================================================
;; LANDMARKS CANDIDATES EXTRACTION (Porteus, Hoffmann, Sebastia 01)
;;=======================================================================

(defparameter *lmk-graph* nil)

(defstruct (lmk (:print-function lmk-print))
  (predpos nil)
  (literal nil)
  (factgraph nil)
  (before nil)
  (after nil))

(defun lmk-print (lmk stream z)
  (declare (type lmk lmk)
	   (stream stream)
	   (ignore z))
  (format stream "~%#<LMK ~a>" (lmk-literal lmk))
  (format stream "~%   Before:")
  (map nil #'(lambda (lit) (format stream "~a " (lmk-literal lit))) (lmk-before lmk))
  (format stream "~%   After:")
  (map nil #'(lambda (lit) (format stream "~a " (lmk-literal lit))) (lmk-after lmk)))

  

;; This finds the intersection between a set of lists, 
;; computing the results used in the cl-lisp intersection function
(defun map-intersection (lists &key (test #'eql))
  (do ((current-intersection (car lists) (intersection current-intersection (car rest-lists) :test test))
       (rest-lists (cdr lists) (cdr rest-lists)))
      ((null rest-lists) current-intersection)
    ()))


(defun create-landmark-node (pred-pos lmk-table)
  (setf (gethash pred-pos lmk-table)
	(make-lmk
	 :predpos pred-pos
	 :factgraph (fact-of-predpos pred-pos)
         :literal (literal-from-predpos pred-pos)
				     
				     )))



(defun landmark-candidates-extraction (goals)
  (let ((lmk-table (make-hash-table :test #'equal))
	(prec-intersection nil)
	(lmk-node nil)
	(top-goals (get-instantiated-goals goals)))
    (dolist (i-goal top-goals)
      (create-landmark-node i-goal lmk-table))
     
    (do ((lm-goals top-goals)
	 (next-goals nil))
	((null lm-goals) lmk-table)
      (dolist (i-goal lm-goals)
	(unless (= 0 (layer-membership i-goal))
	  (setf prec-intersection (map-intersection (mapcar #'gaction-preconds
							    (remove-if-not #'(lambda (x)
									       (action-applied-inlayer x (layer-membership i-goal)))
									   (achievers i-goal)))
						    :test #'equal))
	  (format t "~% LMK> Goal: ~a" (literal-from-predpos i-goal))
	  (format t "~% LMK> Prec Intersec: ~a" (mapcar #'literal-from-predpos prec-intersection))
	  (dolist (i-lm-candidate prec-intersection)
	    (cond ((not (hkey-present i-lm-candidate lmk-table))
		   (setf lmk-node (create-landmark-node i-lm-candidate lmk-table))
		   (push i-lm-candidate next-goals))
		  (t 
		   (setf lmk-node (gethash i-lm-candidate lmk-table))))
	    (pushnew lmk-node (lmk-after (gethash i-goal lmk-table)))
	    (pushnew (gethash i-goal lmk-table) (lmk-before lmk-node))
	    )))
      (setf lm-goals next-goals)
      (setf next-goals nil))))

 
 


;; This is a redefinition of the heuristic function of the relaxedplan
;; just for setting the landmark graph 
(defun h-relaxedplan-with-landmarks (node &optional (goals (problem-goals *current-problem*)))
  (let* ((pg (metric-relaxedplan node goals))
	 (landmark-graph nil))
    (cond ((plangraph-p pg)
	   (setf landmark-graph (landmark-candidates-extraction goals))
	   (setf *lmk-graph* landmark-graph)
	   (values (length (pg-relaxed-plan pg)) 
		   (pg-relaxed-plan pg)))
	  (t (values most-positive-fixnum nil nil)))))






;; (defun landmarks-tree-generation
    
