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
;; Description: Data Structures and Global Variables
;; Date: 2005.12.02
;; 
;; ========================================================================


(defvar *pspace* nil "For holding the global domain structure")
(defvar *current-problem* nil "Holds the current problem structure")

(defvar *debug-sayphi* nil "Prints extended data structures")
(defvar *say-output* 1) 



(defstruct (fact)
  (literal nil)
  (int nil)
  (predpos nil))

(defstruct (fluent)
  (literal nil)
  (int nil)
  (init-value nil)
  (predpos nil)
)


;;Structures for connectivity graph
(defstruct (factgraph
	    (:conc-name fg-))
  (precond-of nil)
  (added-by nil)
  (deleted-by nil)
  (layer nil)
  (activated nil)
  )

(defstruct (fluentgraph
	    (:conc-name flg-))
  (precfun-of nil)
  (changed-by nil)
  (layer nil)
  (activated nil)
  (max-needed nil)
)





;; The ground actions structures
(defstruct (gaction (:print-function gaction-print))
  (planaction nil)
  (int nil)
  (operator nil)
  (preconds nil)
  (precfuns nil)
  (adds nil)
  (dels nil)
  (costs nil)

  (num-precs nil)
  (num-precfuns nil)

  (prec-gcount 0)
  (precfun-gcount 0)
  (applicable nil)

  (prec-h-gcount 0)
  (precfun-h-gcount 0)
  (applicable-h nil)

  (layer nil)
  
  (helpful nil)
  (h-difficulty nil))
  


;;New Structures New Instantiation and ADL
(defstruct (logicformula
	     (:conc-name lformula-)
 	     (:print-function logicformula-print)
)
  (connector nil)
  (quantivar nil)
  (predicate nil)
  (sons nil))


;; Domain description Operators
(defstruct (say-operator 
	     (:conc-name op-)
	     (:print-function operator-print)
	     )
  (name nil)
  (params nil)
  (preconds nil)
  (effects nil)
  
  (relevant-appbyte nil)
  
)


(defstruct (predicate
	     (:print-function predicate-print))
  (name nil)
  (num-args nil)
  (args-params nil)
  (is-added nil)
  (is-deleted nil)
  (num-instances nil)
)



(defun gaction-print (gaction stream z)
  (declare (type gaction gaction)
	   (stream stream)
	   (ignore z))
  (format stream "[<GACTION> ~a ~a]" (gaction-int gaction) (gaction-planaction gaction)))


(defun predicate-print (pred stream z)
  (declare (type predicate pred)
	   (stream stream)
	   (ignore z))
  (format stream "[<PRED> ~a ~a]" (predicate-name pred) (predicate-args-params pred)))
     


(defun operator-print (say-operator stream z)
  (declare (type say-operator operator)
	   (stream stream)
	   (ignore z))
  (format stream  "~%[<OP ~a>  ~a]" (op-name say-operator) 
	  (op-params say-operator)))



(defun logicformula-print (logicformula stream z)
  (declare (type logicformula logicformula)
	   (stream stream)
	   (ignore z))
  (format stream  "~%[<LF> ~a]" (lformula-connector logicformula))
  (unless (null (lformula-predicate logicformula))
    (format stream "~a " (lformula-predicate logicformula)))
  (unless (null (lformula-quantivar logicformula))
    (format stream "~a " (lformula-quantivar logicformula)))
  (format stream "{")
  (dolist (ison (lformula-sons logicformula))
    (format stream  " ~a" (lformula-connector ison)))
  (format stream "}"))

;; =============================================================================

(defstruct (dom)
  (name nil)
  (requirements nil)
  (domtypes nil)
  (inheritypes nil)
  (all-types nil)
  (predicates nil)
  (functors nil)
  (constants nil)
  
  (operators nil)
  (ioperators nil)

  (actions nil)
  (pred-achievers (make-hash-table :test #'eq))
  (numeric-achievers (make-hash-table :test #'equal))
  (positive-vars nil)
  (negative-vars nil)
  (real-functors nil)
  (artificial-vars nil)

  (negative-preds nil)
  (special-preds nil)
  (plist nil))


(defstruct (action (:print-function action-print))
  (name-id nil)
  (name nil)
  (parameters nil)
  (duration nil)
  (preconditions nil)
  (precond-funs nil)
  (adds nil)
  (dels nil)
  (costs nil)
  
  (prec-argpos nil)
  (adds-argpos nil)
  (dels-argpos nil)
  (num-instances nil)
  )


(defstruct (say-problem
	(:conc-name problem-)
	(:print-function say-problem-print))
  (name nil)
  (domain nil)
  (file nil)
  (objects nil)
  (inheritobjects nil)
  (patterns nil)
  (init-state nil)
  (goals nil)
  (numeric-goals nil)
  (metric nil)
  (search-tree nil)
  (lit-init-state nil)
  (lit-goals nil)
  (constant-poslist)
  (positive-vars nil)
  (negative-vars nil)
  (artificial-vars nil)
  (plist nil))


(defstruct (snode (:print-function snode-print))
  (number nil)
  (depth nil)
  (length nil)
  (parent nil) 
  (candidates nil)
  (children nil) 
  (state nil)
  (expanded nil)
  (closed nil)
  (selected nil)
  (applied-action nil)
  (cost 0)
  (h-value nil)
  (h-plus nil)
  (g-value nil)
  (f-value nil)
  (hash-code nil)
  (helpful-p nil)
;;   (focus-goals nil)
  (relaxed-plan nil)
;;   (relax-state nil)
  (applicable-byte nil)
  (helpful-byte nil)
  (tag nil)
  (typed-relation nil)
  (recommended nil)
  (advise-weight 0)
  (advise-step nil)
  (replay-pointers nil)
  (plist nil))

(defstruct (lhnode (:include snode)
		   (:print-function lhnode-print)
		   )
   (lookahead-depth nil)
   (lookahead-plan nil)
   (lookahead-states nil)
   (lookahead-costs nil)
   (rxp-remaining nil)
   (rxp-action-failed nil)
   
   )


(defstruct (solution (:print-function solution-print))
  (found nil)
  (total-time nil)
  (pre-time nil)
  (search-time nil)
  (path nil)
  (num-nodes nil)
  (evaluated-nodes nil)
  (depth nil)
  (last-node nil)
  (stop-reason nil)
  (length nil)
  (total-cost nil))


(defun say-problem-print (problem stream z)
  (declare (type say-problem problem)
	   (stream stream)
	   (ignore z))
  (format stream "~%#<PROBLEM>")
  (format stream "~% Init State: ~a" (problem-init-state problem))
  (format stream "~% Goals: ~a" (problem-goals problem))
  )


(defun action-print (action stream z)
  (declare (type action action) 
	   (stream stream) 
	   (ignore z))
  (format stream "~%#<ACTION ~a> ~a" (action-name action) (action-parameters action)))


(defun snode-print (snode stream z)
  (declare (type snode snode)
	   (stream stream)
	   (ignore z))
  (format stream "~%#<SNODE Number: ~d >" (snode-number snode))
  (format stream "~%   Num-children:~d   Depth:~d   H-value:~a   G-value:~a   F-value:~a   H-plus:~a"  
	  (length (snode-children snode)) (snode-depth snode) (snode-h-value snode) 
	  (snode-g-value snode) (snode-f-value snode) (snode-h-plus snode))
  (format stream "~%   Action < ~a >" (snode-applied-action snode))
  (when *debug-sayphi*
    (format stream "~%~a" (snode-state snode)))
)

(defun lhnode-print (snode stream z)
  (declare (type lhnode snode)
	   (stream stream)
	   (ignore z))
  (format stream "~%#<LOOKAHEAD-NODE Number: ~d  LH-Plan-depth:~a>" (snode-number snode) (lhnode-lookahead-depth snode))
  (format stream "~%   Num-children:~d   Depth:~d   H-value:~a   G-value:~a   F-value:~a   H-plus:~a"  
	  (length (snode-children snode)) (snode-depth snode) (snode-h-value snode) 
	  (snode-g-value snode) (snode-f-value snode) (snode-h-plus snode))
  (format stream "~%   Action < ~a >" (snode-applied-action snode))
  (when *debug-sayphi*
    (format stream "~%~a" (snode-state snode)))
)



(defun solution-print (solution stream z)
  (declare (type solution solution)
	   (stream stream)
	   (ignore z))
  (format stream "~%#<SOLUTION: ~a  " (solution-found solution))
  (format stream " Length: ~d " (solution-length solution))
  (format stream " Nodes: ~d " (solution-num-nodes solution))
  (format stream " Evaluated: ~d " (solution-evaluated-nodes solution))
  (format stream " Depth: ~d >" (solution-depth solution))
  (format stream "~%       Total Time        : ~d" (solution-total-time solution))
  (format stream "~%       Instantiating Time: ~d" (solution-pre-time solution))
  (format stream "~%       Search Time       : ~d" (solution-search-time solution))
  (format stream "~%")
  (format stream "~%       Metric            : ~a" (problem-metric *current-problem*))
  (format stream "~%       Total Cost        : ~a" (solution-total-cost solution))
  )
