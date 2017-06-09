;; Mixture of EHC and random planner

(defvar *trace-hybrid-planner-p* nil)
(defvar *number-better-random* 0)
(defvar *number-better-ehc* 0)
(defvar *d-random-lookahead* 10)
(defvar *k-random-children* 1)

;; planner can be either: hybrid-planner, or ehc
;; d-random-lookahead: depth of random lookaheads
;; k-random-children: number of random searches in each iteration
(defun run-hybrid-experiments (&key (domain "driverlog") (domain-file "driverlog.pddl") (timeout 300) (d-random-lookahead-list (list 10 20)) (k-random-children-list (list 1 5)) (number-repetitions 5) (prefix "pfile"))
  ;;  (runset domain domain-file :timeout timeout :probs-prefix prefix)
  (setf *say-solution-format* :hybrid-planner)
  (say-domain domain domain-file)
  (dolist (prob-path (dirfiles-sorted (format nil "~aprobsets/" *domain-dir*) (concatenate 'string prefix  #+SBCL "*.*" #-SBCL "*")))
    (runset domain domain-file :lookahead t :timeout timeout :probs-prefix (pathname-name prob-path))
    (runset domain domain-file :lookahead nil :timeout timeout :probs-prefix (pathname-name prob-path))
    (dolist (depth d-random-lookahead-list)
      (setf *d-random-lookahead* depth)
      (dolist (k k-random-children-list)
	(setf *k-random-children* k)
	(dotimes (i number-repetitions)
	  (runset domain domain-file :algorithm 'hybrid-planner :lookahead t :timeout timeout :probs-prefix (pathname-name prob-path) :search-options (list (list :d-random-lookahead depth) (list :k-random-children k))))))))


(defun hybrid-planner (init-node h-fn cost-fn search-options &optional (problem *current-problem*))
  (declare (ignore cost-fn))
  (let ((open-nodes nil) (open-children nil)
	(node init-node) (next-node nil) (visited (make-hash-table))
	(current-h 0) (current-h-plus 0) (better-h-node nil) (discrete-h (not (is-metric-domain)))
	(lookahead (search-option-value :lookahead search-options))
	(helpful (search-option-value :helpful search-options))
	(planner (search-option-value :planner search-options))
	(d-random-lookahead (search-option-value :d-random-lookahead search-options))
	(k-random-children (search-option-value :k-random-children search-options))
	(random-node nil)
	(ehc-node nil))
    (setf *number-better-random* 0)
    (setf *number-better-ehc* 0)

    (cond (*trace-mem* 
	   (say-consed-bytes :mem-per-node (store-h-extras node h-fn))
	   (write-node-mem-info 'ehc))
	  (t (store-h-extras node h-fn)))

    (setf current-h (snode-h-value node))
    (setf current-h-plus (snode-h-plus node))
    (do* ((stop-this (stop-search node problem)
		     (stop-search node problem)))
	 (stop-this (setf *last-node* node)
		    (format t "~2%Number better random: ~d" *number-better-random*)
		    (format t "~2%Number better EHC: ~d" *number-better-ehc*)
		    (when-stop (cdr stop-this) node problem))
      
      (trace-search-extras (snode-number node))
      (expand-state node :helpful helpful)
      (when lookahead (expand-node-lookahead node lookahead visited))

      (print-search-node node nil)

      (setq ehc-node (get-enforced-heuristic node h-fn current-h visited :discrete discrete-h :current-h-plus current-h-plus))

      (if *trace-hybrid-planner-p* (format t "~%EHC action ~a" (if (and ehc-node (snode-applied-action ehc-node))
								   (gaction-planaction (snode-applied-action ehc-node))
								   "NULL")))

      (setq random-node (action-random-selection node problem h-fn visited d-random-lookahead k-random-children))
      (setq better-h-node (cond ((not ehc-node) random-node)
				((not random-node) nil)
				((>= (min (snode-h-value random-node) (snode-h-value ehc-node))
				     (snode-h-value node))
				 nil)
				((< (snode-h-value random-node) (snode-h-value ehc-node))
				 (incf *number-better-random*)
				 random-node)
				((< (snode-h-value ehc-node) (snode-h-value random-node))
				 (incf *number-better-ehc*)
				 ehc-node)
				(t (incf *number-better-ehc*)
				   ehc-node)))

      ;; We did not find a better node in this iteration
      (if (eq better-h-node node)
	  (setq better-h-node nil))
      
      (cond ((snode-p better-h-node)
	     (setf current-h (snode-h-value better-h-node))
	     (setf current-h-plus (snode-h-plus better-h-node))
	     (setf node better-h-node)
	     (setf open-nodes nil)
	     (setf open-children nil)
	     (setf visited (reset-hash-visited node))
	     )
	    (t 
	     (setf open-children (nconc (remove-if #'snode-closed (snode-children node)) 
					open-children))
	     
	     (cond ((not (snode-p (setf next-node (pop open-nodes))))
		    (when (> *say-output* 1) (format t "  ~% [Expanding Breadth Level]"))
		    (setf open-nodes (ehc-open-nodes open-children (car (find-argument search-options :children-sort))))
		    
		    (when (null open-nodes) ;;Recuperando la poda por las helpful actions
		      (dolist (i-child (restore-nonhelpful node h-fn))
			(cond ((find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state)
			       (setf (snode-closed i-child) t))
			      (t 
			       (push i-child (gethash (snode-hash-code i-child) visited))
			       (push i-child open-nodes))))
			       
		      ;; 		      (setf open-nodes (restore-nonhelpful node h-fn))
		      
		      (when (> *say-output* 1) 
			(format t "  ~%!!Restoring NON Helpful Nodes.")))
		    (setf node (pop open-nodes))
		    (setf open-children nil))
		   (t 
		    (setf node next-node)))))       
      ;; 	 (when (snode-p node)
      ;; 	   (print-search-node node nil)
      )))

(defun action-random-selection (node problem h-function visited-nodes d-random-lookahead k-random-children)
  (do ((k k-random-children (1- k))
       (best-node node))
      ((or (stop-search best-node problem)
	   (zerop k))
       (if *trace-hybrid-planner-p* (format t "~%Random action ~a; h-random: ~d; h-best: ~d"
					    (if (and (snode-p best-node) (snode-applied-action best-node))
						(gaction-planaction (snode-applied-action best-node)) "NULL")
					    (if (snode-p best-node) (snode-h-value best-node) "-")
					    (snode-h-value node)))
       best-node)
    (do* ((depth d-random-lookahead (1- depth))
	  (nodes (snode-children node) (snode-children random-node))
	  (random-node (choose-one nodes) (choose-one nodes)))
	 ((or (stop-search random-node problem)
	      (zerop depth)
	      (not (snode-p random-node))
	      (find random-node (gethash (snode-hash-code random-node) visited-nodes) :test #'equal-state)))
;;       (format t "~%Node: ~a" random-node)
;;      (format t "~%Action ~a chosen randomly in cycle ~d at depth ~d" (gaction-planaction (snode-applied-action random-node)) (- k-random-children k) (- d-random-lookahead depth))
      (trace-search-extras (snode-number random-node))
      (expand-state random-node)
      ;; 	       (expand-state node :helpful helpful)
      ;; 	       (when lookahead (expand-node-lookahead node lookahead visited))
      ;;	       (print-search-node node nil)
      (store-h-extras random-node h-function)
;;      (format t "~%h-random: ~d; h-best: ~d" (snode-h-value random-node) (snode-h-value best-node))
      (push random-node (gethash (snode-hash-code random-node) visited-nodes))
      (if (< (snode-h-value random-node) (snode-h-value best-node))
	  (setq best-node random-node)))))


;; Modified to account for hybrid-planner
(defun format-solution (solution)
  ;; see if we are running the hybrid-planner
  (case *say-solution-format*
    (:hybrid-planner
     (format nil "~a, ~4$, ~a, ~a, ~a, ~a, ~a, ~d, ~d, ~d, ~d"
	     (solution-found solution) (solution-total-time solution)
	     (solution-depth solution) (solution-num-nodes solution) (solution-evaluated-nodes solution)
	     (solution-total-cost solution) (solution-stop-reason solution) *d-random-lookahead* *k-random-children* *number-better-random* *number-better-ehc*))
;; 	     (solution-total-cost solution) (solution-stop-reason solution) (search-option-value :d-random-lookahead search-options) (search-option-value :k-random-children search-options) *number-better-random* *number-better-ehc*))
    (:values
     (format nil "~t~a, ~t~4$, ~t~a, ~t~a, ~t~a, ~t~a, ~t~a"
	     (solution-found solution) (solution-total-time solution)
	     (solution-depth solution) (solution-num-nodes solution) (solution-evaluated-nodes solution)
	     (solution-total-cost solution) (solution-stop-reason solution)))

    ;; 	(:extras
    ;; 	 ;;agregando al final de los valores el numero de aciertos a la primera y el promedio de hit
    ;; 	 (format nil "~t~a, ~t~a, ~t~a, ~t~a, ~t~a, ~t~a, ~t~a, ~t~a"
    ;; 		 (solution-found solution) (solution-total-time solution)
    ;;                  (solution-depth solution) (solution-num-nodes solution) (solution-evaluated-nodes solution)
    ;; 		 (solution-stop-reason solution)
    ;; 		 (sol-trace :first-hit) (sol-trace :average-hit)
    ;; 		 ))
    ;; 	(:cbp-extras
    ;; 	 ;;agregando al final de los valores el numero de aciertos a la primera y el promedio de hit
    ;; 	 (format nil "~t~a, ~t~4$, ~t~a, ~t~a, ~t~a, ~t~a, ~t~a, ~t~a, ~t~a"
    ;; 		 (solution-found solution) (solution-total-time solution)
    ;;                  (solution-depth solution) (solution-num-nodes solution) (solution-evaluated-nodes solution)
    ;; 		 (solution-stop-reason solution)
    ;; 		 (sol-trace :guided-nodes)   
    ;; 		 (sol-trace :cbp-first-hit)
    ;; 		 (sol-trace :cbp-average-hit)
    ;; 		 ))
    (:tags-values
     (format nil "Found:~a Time:~a Nodes:~a Depth:~a Stop:~a"
	     (solution-found solution) (solution-total-time solution)
	     (solution-num-nodes solution) (solution-depth solution)
	     (solution-stop-reason solution)))))

;; (defun run-hybrid-planner (planner problem &key (domain "blocksworld") (domain-file "domain.pddl") (d-random-lookahead 10) (k-random-children 1))
;;   (say-domain domain domain-file)
;;   (prob problem)
;;   (let ((solution (if (eq planner 'hybrid-planner)
;; 		      (plan :algorithm 'hybrid-planner :search-options (list (list :d-random-lookahead d-random-lookahead) (list :k-random-children k-random-children)))
;; 		      (plan))))
;; ;;     (pp-solution-sayphi)
;;     solution))
