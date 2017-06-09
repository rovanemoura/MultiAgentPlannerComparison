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
;; Description: Lookahead State Strategies for speeding up search
;; Date: 2008.02.05
;; 
;; ========================================================================


;; Returning the inmediate past action, for those nodes that have a lookahead parent-node
(defun snode-previous-action (node)
  (if (lhnode-p node)
      (car (last (lhnode-lookahead-plan node)))
      (snode-applied-action node)))


(defun compute-lookahead-node (node lookahead visited)
  (declare (ignore lookahead))
  (unless (null (snode-relaxed-plan node))
    (do ((lh-node (create-lookahead-node node))
	 (lookahead-p nil t))
	((or (not (try-to-lookahead lh-node visited))  
	     (null (lhnode-rxp-remaining lh-node)))
	 (when (or lookahead-p 
		   (> (length (lhnode-lookahead-plan lh-node)) 1))
	   lh-node)))))


;; It takes a node and generate a lookahead child from it
;; If the lookahead depth is 1 the node belongs to the normal children list
(defun expand-node-lookahead (node lookahead visited &key (lh-function #'compute-lookahead-node)
			      (lhnodes-in-path nil))
  (let ((lh-child))
;;     (unless (null (snode-children node))
      (setf lh-child (funcall lh-function node lookahead visited))
      (when (and (lhnode-p lh-child)
		 (> (lhnode-lookahead-depth lh-child) 1))
	(setf (snode-number lh-child) 
	      (incf (getf (problem-plist *current-problem*) :node-counter)))
	(setf (gethash (snode-number lh-child) *hash-nodes*) lh-child)
	(setf (snode-cost lh-child)
	      (if (is-metric-domain)
		  (action-cost-metric (snode-state node) (snode-state lh-child))
		  (lhnode-lookahead-depth lh-child)))
	(setf (snode-hash-code lh-child) (compute-duphash-code (snode-state lh-child)))

	(when lhnodes-in-path
	  (dotimes (i-step (- (length (lhnode-lookahead-plan lh-child)) 1))
	    (when (> i-step 0)
	      (let ((lh-inpath (make-lhnode
				:number (incf (getf (problem-plist *current-problem*) :node-counter))
				:depth (+ 1 (snode-depth node))
				:length (+ (snode-length node) (1+ i-step))
				:parent node 
				:state (car (nth i-step (lhnode-lookahead-states lh-child)))
				:hash-code (cdr (nth i-step (lhnode-lookahead-states lh-child)))
				:lookahead-depth (1+ i-step)
				:lookahead-plan (subseq (lhnode-lookahead-plan lh-child) 0 (1+ i-step))
			        :lookahead-costs (subseq (lhnode-lookahead-costs lh-child) 0 (1+ i-step))
				)))
		(setf (gethash (snode-number lh-inpath) *hash-nodes*) lh-inpath)
		(setf (snode-cost lh-inpath)
		      (if (is-metric-domain)
			  (action-cost-metric (snode-state node) (snode-state lh-inpath))
			  (lhnode-lookahead-depth lh-inpath)))
		(push lh-inpath (snode-children node))
		))))
	(push lh-child (snode-children node))

)
;; )
))


(defun lookahead-update-action (node gaction lhstate-and-code &optional (remove-rx-p t))
  (let ((prev-state (if (lhnode-lookahead-plan node)
			(caar (last (lhnode-lookahead-states node)))
			(snode-state (lhnode-parent node))))
	(new-state (car lhstate-and-code)))
  (incf (lhnode-lookahead-depth node))
  (incf (snode-length node))
  (setf (lhnode-state node) new-state)
  (setf (lhnode-lookahead-plan node) 
	(append (lhnode-lookahead-plan node) (list gaction)))
  (setf (lhnode-lookahead-states node)
	(append (lhnode-lookahead-states node) (list lhstate-and-code)))
  (setf (lhnode-lookahead-costs node)
	(append (lhnode-lookahead-costs node) (list (action-cost-metric prev-state new-state))))
  (when remove-rx-p
    (setf (lhnode-rxp-remaining node) 
	  (remove gaction (lhnode-rxp-remaining node))))))



;; It takes the lhnode and the selected action to be applied, 
;; creates a copy of the state a verify if it belongs to the 
;; current solution path.
(defun verify-lookahead-equal-state(node gaction visited)
  (let* ((new-lhstate (apply-action-instance (lhnode-state node) gaction))
	 (lhstate-hash-code (compute-duphash-code new-lhstate))
	 (repeated nil)
	 )
    (setf repeated (find new-lhstate (gethash lhstate-hash-code visited) :test #'equal-state))
    (when (or (not (snode-p repeated))
	      (< (+ (snode-length node) (length (lhnode-lookahead-plan node))) (snode-length repeated)))
      (let ((lh-path-equal nil))
	(dolist (i-lh-state-and-code (lhnode-lookahead-states node))
	  (when (and (not lh-path-equal)
		     (= lhstate-hash-code (cdr i-lh-state-and-code))
		     (equal-state new-lhstate (car i-lh-state-and-code)))
	    (setf lh-path-equal t)))
	(unless lh-path-equal
	  (cons new-lhstate lhstate-hash-code))))))


(defun create-lookahead-node (node &key (child-lh nil))
  (let ((rx-plan (if child-lh (remove (snode-applied-action node)
				      (snode-relaxed-plan node) :test #'gaction-int)
		     (copy-list (snode-relaxed-plan node)))))
  (make-lhnode
     :depth (+ 1 (snode-depth node))
     :length (snode-length node)
     :parent node 
     :state (copy-state (snode-state node))
     :lookahead-depth 0
     :rxp-remaining rx-plan) 
     ))


;; Faltaria poner las precondiciones numericas
(defun this-gaction-applicable (action state)
  (let ((applicable t))
    (dolist (i-prec (gaction-preconds action) applicable)
      (when (false-in-state state (car i-prec) (cdr i-prec))
	(setf applicable nil)))))


;; It is like apply-action-instance. For this case I do not 
;; wanted to make a copy of the state for then throwing it away
(defun apply-gaction-lookahead (state action)
    (apply-inst-effects state (gaction-dels action) 0)
    (apply-inst-effects state (gaction-adds action) 1)
     (when (is-metric-domain)
       (apply-gaction-costs action state))
     state)

 

(defun try-to-lookahead (node visited)
  (let ((something-applied nil)
	(verified-lhstate nil))
  (dolist (i-rxaction (lhnode-rxp-remaining node))
    (when (this-gaction-applicable i-rxaction (lhnode-state node))
      (setf verified-lhstate (verify-lookahead-equal-state node i-rxaction visited))
      (when (consp verified-lhstate)
	(lookahead-update-action node i-rxaction verified-lhstate)
	(setf something-applied t))))
;;   (format t "~%>>L> ~a" (lhnode-lookahead-plan node))
  something-applied))


;; This takes the a list of nodes and guarantees to select a non dead-end node
(defmacro df-next-node (node-list h-fn)
  `(do ((next-node nil)
	(ok-node nil))
    ((or (snode-p ok-node)
      (null ,node-list))
     ok-node)
    (setf next-node (pop ,node-list))
    (when (snode-p next-node)
      (when (null (snode-h-value next-node))
	(store-h-extras next-node ,h-fn))
      (when (< (snode-h-value next-node) most-positive-fixnum)
	(setf ok-node next-node)))))



(defun lookahead-bfs (init-node heuristic-fn cost-fn search-options &optional  
		      (problem *current-problem*))
  (declare (ignore cost-fn))
  (let ((helpful (search-option-value :helpful search-options))
	(lookahead (search-option-value :lookahead search-options))
	(visited (make-hash-table))
	(node init-node) (open-list nil) (h-function nil)
	(sorted-candidates nil) (repeated nil)
	(max-solutions (search-option-value :max-solutions search-options))
	(lh-inpath (search-option-value :lh-inpath search-options))
	(hash-solutions (make-hash-table))
	(upper-cost-bound most-positive-fixnum))

    (setf *say-hash-solutions* hash-solutions)
    (setf h-function (if (eq lookahead :lh-repair-rx) #'h-relaxedplan-setglobal heuristic-fn))
    (store-h-extras node h-function)
    
    (do* ((stop-this (stop-multiple node problem hash-solutions max-solutions t) 
		     (stop-multiple node problem hash-solutions max-solutions t)))
	 ((car stop-this) (when-multiple-stop (cdr stop-this) node problem)) 
      
      (when (snode-closed node)  ;;Goals reached
	(when (< (solution-total-cost (gethash (hash-table-count hash-solutions) hash-solutions)) upper-cost-bound)
	  (setf *say-solution* (gethash (hash-table-count hash-solutions) hash-solutions))
	  (setf upper-cost-bound (solution-total-cost *say-solution*))
	  (when (> *say-output* 0) 
	    (format t "~% =>LBFS >> Found new solution with Cost {~a}" (solution-total-cost *say-solution*))))
	(setf node (df-next-node open-list h-function)))

      (expand-state node :helpful helpful)

      (cond ((and lh-inpath (snode-p (snode-parent node)) (lhnode-p node))
	     (dolist (i-sibling (snode-children (snode-parent node)))
	       (when (lhnode-p i-sibling)
		 (pushnew i-sibling (gethash (snode-hash-code i-sibling) visited)))))
	    (t
	     (push node (gethash (snode-hash-code node) visited))))
      
      (dolist (i-child (snode-children node))
	(setf repeated (find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state))
	(when (or (and (snode-p repeated)
		       (< (snode-length repeated) (snode-length i-child)))
		  (>= (snode-length i-child) upper-cost-bound))
	  (setf (snode-closed i-child) t))) 

      (expand-node-lookahead node t visited :lhnodes-in-path lh-inpath)

      (dolist (i-child (snode-children node))
	(when (and (lhnode-p i-child)
		   (>= (snode-length i-child) upper-cost-bound))
	  (setf (snode-closed i-child) t))) 
      
      (setf sorted-candidates (remove-if #'snode-closed (snode-children node)))

      (unless (null sorted-candidates)
	(setf open-list (nconc open-list (cdr sorted-candidates)))
	(push (car sorted-candidates) open-list))

      (setf node (df-next-node open-list h-function))
     
      (when (snode-p node)
	(print-search-node node nil)
	))))



(defun df-lookahead (init-node h-function cost-fn search-options &optional  
		     (problem *current-problem*))
  (declare (ignore cost-fn search-options))
  (let ((visited (make-hash-table)) (la-next nil)
	(node init-node))
    (store-h-extras node h-function)
    (do* ((stop-this (stop-search node problem) (stop-search node problem)))
	 (stop-this (when-stop (cdr stop-this) node problem))
      (push node (gethash (snode-hash-code node) visited))
      (expand-node-lookahead node t visited)
      (setf la-next (car (snode-children node)))
      (cond ((and (lhnode-p la-next)
		  (not (find la-next (gethash (snode-hash-code la-next) visited) :test #'equal-state)))
	     (setf node la-next)
	     (store-h-extras node h-function))
	    (t
	     (setf node nil)))
	  
      (when (snode-p node)
	(print-search-node node nil)))))    


(defun la-play (init-node h-fn cost-fn search-options)
  (let ((first-solution (df-lookahead init-node h-fn cost-fn search-options)))
    (cond ((and (solution-p first-solution)
		(solution-found first-solution))
	   first-solution)
	  ((not (eq (solution-stop-reason first-solution) :time-bound))
	   (format t "~%Changing to Lookahead-BFS at time ~3$" (solution-total-time first-solution))
	   (unless (search-option-value :w_g search-options) (push (list :w_g 1) search-options))
	   (unless (search-option-value :w_h search-options) (push (list :w_h 3) search-options))
	   (when (search-option-value :helpful search-options) 
	     (rplacd (assoc :helpful search-options) (list :bfs-helpful)))
	   (push (list :lookahead t) search-options)

	   (a-star init-node h-fn cost-fn search-options))
	  (t first-solution))))

(defun say-yahsp (init-node h-fn cost-fn search-options)
  (unless (search-option-value :w_g search-options) (push (list :w_g 1) search-options))
  (unless (search-option-value :w_h search-options) (push (list :w_h 3) search-options))
  (when (search-option-value :helpful search-options) 
    (rplacd (assoc :helpful search-options) (list :bfs-helpful)))
  (push (list :lookahead t) search-options)
  (a-star init-node h-fn cost-fn search-options))


