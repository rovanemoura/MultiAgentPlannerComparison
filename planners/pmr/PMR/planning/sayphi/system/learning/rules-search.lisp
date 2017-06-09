;; SAYPHI Planner 
;; Forward Guided Search Algorithms
;; Td. 16.12.2006
;; _________________________________________________________________________________________


(defvar *trace-search* nil) 
(defvar *trace-search-depth* 3)
(defvar *trace-node* nil) 
(defvar *trace-listnodes* (make-hash-table))
(defvar *trace-ehc* nil)
(defvar *trace-mem* nil)


;; This are global variables for defaults planning's parameters
(defvar *say-algorithm* 'enforced-hill-climbing)
(defvar *say-heuristic* 'h-metric-rxplan)
(defvar *say-costfn* 'node-path-cost)
(defvar *say-helpful* t)
(defvar *say-depthbound* 1000)
(defvar *say-timeout* 30)
(defvar *say-h-rounded* t)

;; 0:silent 1:plan 2:search & plan
(defvar *say-solution* nil)
(defvar *say-hash-solutions* nil)
(defvar *search-failed* nil)
(defvar *say-2try* t) ;; If first algorithm fails, try a complete best-first

;;tracing CBPlanning
(defvar *hit-node* nil)
(defvar *hit-average* 0)

(defun reset-ehc-trace()
  (setf *hit-node* nil
	*hit-average* 0))


(defun build-path (node)
  (let ((plan-path nil))
    (do ((i-node node (snode-parent i-node)))
	((null (snode-parent i-node)) plan-path)
	(setf (snode-selected (snode-parent i-node)) i-node)
	(push i-node plan-path))))


(defun reach-time-bound (problem)
  (let* ((initial-search-time (get (problem-plist problem) :initial-search-time))
	 (current-time (get-internal-run-time))
	 (searching-seconds (float (/ (- current-time initial-search-time)
				      internal-time-units-per-second))))
    (when (> searching-seconds (get-sayp :say-timeout)) t)))



(defun build-solution (node problem found reason)
  (let* ((stop-time (get-internal-run-time))
	 (initial-pre-time (get (problem-plist problem) :initial-pre-time))
	 (initial-search-time (get (problem-plist problem) :initial-search-time))
	 (this-solution (make-solution 
			 :found found
			 :total-time (float (/ (- stop-time initial-pre-time)
					       internal-time-units-per-second))
			 :pre-time (float (/ (- initial-search-time initial-pre-time)
					     internal-time-units-per-second))
			 :search-time (float (/ (- stop-time initial-search-time)
						internal-time-units-per-second))
			 :depth (when (snode-p node) (snode-depth node))
			 :path (when (snode-p node) (build-path node))
			 :num-nodes (1+ (get (problem-plist problem) :node-counter)) ;;mas el nodo inicial
			 :evaluated-nodes (get (problem-plist problem) :node-evaluated)
			 :last-node node
			 :stop-reason reason
			 :total-cost (when (snode-p node) (compute-total-cost node problem)))))
    this-solution))

;; It recomputes soltuion times when a multiple solutions are found, but not are the last
;; step of the search.
(defun update-solution-for-multiple (best-solution problem)
  (let* ((stop-time (get-internal-run-time))
	 (initial-pre-time (get (problem-plist problem) :initial-pre-time))
	 (initial-search-time (get (problem-plist problem) :initial-search-time)))
    (setf (solution-total-time best-solution) (float (/ (- stop-time initial-pre-time)
							internal-time-units-per-second)))
    (setf (solution-search-time best-solution) (float (/ (- stop-time initial-search-time)
							 internal-time-units-per-second)))
    (setf (solution-num-nodes best-solution) (1+ (get (problem-plist problem) :node-counter))) ;;mas el nodo inicial
    (setf (solution-evaluated-nodes best-solution) (get (problem-plist problem) :node-evaluated))
    best-solution))



;;  I could put this in a way of signals handlers to add more functions to check each cycle
(defun stop-search (node problem)
  (cond ((null node)
	 (cons t :search-failed))
	((and (numberp (snode-h-value node))
	      (= (snode-h-value node) most-positive-fixnum))
 	 (cons t :recognized-dead-end))
	((goals-reached node)
	 (cons t :goals-reached))
	((>= (snode-depth node) (get-sayp :say-depthbound))
	 (cons t :depth-bound))
	((reach-time-bound problem)
	 (cons t :time-bound))
	((and *trace-search* (>= (snode-depth node) *trace-search-depth*))
	 (cons t :trace-search))
	(t nil)))


(defun stop-multiple (node problem hash-solutions max-sols)
  (cond ((null node)
	 (cons t :search-failed))

	((and (numberp (snode-h-value node))
	      (= (snode-h-value node) most-positive-fixnum))
	 (setf (snode-closed node) t)
 	 (cons nil :recognized-dead-end))

	((goals-reached node)
	 (setf (gethash (1+ (hash-table-count hash-solutions)) hash-solutions) 
	       (build-solution node *current-problem* t :goals-reached))
	 (setf (snode-closed node) t)
	 (cons (> (hash-table-count hash-solutions) max-sols) :goals-reached))

	((>= (snode-depth node) (get-sayp :say-depthbound))
	 (cons t :depth-bound))
	((reach-time-bound problem)
	 (cons t :time-bound))
	((and *trace-search* (>= (snode-depth node) *trace-search-depth*))
	 (cons t :trace-search))
	(t nil)))
  


(defun when-stop (reason node problem)
  (case reason
	(:goals-reached
	 (setf *say-solution* (build-solution node problem t reason))
	 (say-pp-solution *say-solution*))
	(:recognized-dead-end
	 (if (> *say-output* 0)
	     (format t "~% DEAD END - UNREACHABLE GOALS"))
	 (setf *say-solution* (build-solution node problem nil reason)))
	(:depth-bound 
	 (if (> *say-output* 0)
	     (format t "~% HIT DEPTH BOUND"))
	 (build-solution node problem nil reason))
	(:time-bound
	 (if (> *say-output* 0)
	     (format t "~% HIT TIME BOUND"))
	 (build-solution node problem nil reason))
	(:search-failed 
	 (if (> *say-output* 0)
	     (format t "~% SEARCH FAILED!!"))
	 (setf *search-failed* t)
	 (build-solution node problem nil reason))
	(:trace-search
	 (build-solution node problem nil reason)
	 (if (> *say-output* 0)
	     (format t "~% Trace search at depth ~d !!" (snode-depth node)))
	 (setf *trace-node* node))
	(:multiple-stop
	 (say-pp-solution *say-solution*))))

(defun when-multiple-stop (reason node problem)
  (cond ((solution-p *say-solution*)
;;  	 (say-pp-solution *say-solution*))
 	 (say-pp-solution (update-solution-for-multiple *say-solution* problem)))
	(t 
	 (when-stop reason node problem))))


(defun write-node-mem-info (algorithm)
  (let ((mem-trace-file (format nil "~a/result/mem-per-node.txt" *domain-dir*))
	(mem-per-node (get (problem-plist *current-problem*) :mem-per-node)))
    (with-open-file (out-stream mem-trace-file :direction :output :if-exists 
				:append :if-does-not-exist :create)
      (format out-stream "~%~a ~a ~a" algorithm *problem-file* mem-per-node))))


(defun write-mem-trace (info)
  (let ((mem-trace-file (format nil "~a/result/memtrace-~a.txt" *domain-dir* *problem-file*)))
    (with-open-file (out-stream mem-trace-file :direction :output :if-exists 
				:append :if-does-not-exist :create)
      (format out-stream "~% ~a ~a" info (say-used-memory :used-mem)))))


    
(defun trace-search-extras (info)
  (when *trace-mem* (write-mem-trace info)))

    


(defun hc-next-node (node visited sort-function)
  (stable-sort (snode-children node) sort-function :key #'snode-h-value)
  (let ((selected-node nil) (found-next nil))
    (dolist (i-child (snode-children node) selected-node)
      (unless found-next
	(unless (find i-child visited :test #'equal-state)
	  (setf selected-node i-child)
	  (setf found-next t))))))


;; Select the first open from chlis or backtrack to the next open node
(defun next-node (node sort-function)
  (let ((node-selected nil))
    (cond ((snode-p (setf node-selected 
			  (find-if #'(lambda (xnode)
				       (and (null (snode-closed xnode))
					    (funcall sort-function (snode-h-value xnode) 
						     (snode-h-value node))))
				   (snode-children node))))
	   node-selected)
	  (t
	   (setf (snode-closed node) t)
	   (cond ((not (null (snode-parent node)))
		  (next-node (snode-parent node) sort-function))
		 (t nil))))))


(defun next-open (node)
  (let ((node-selected nil))
    (cond ((snode-p (setf node-selected 
			  (find-if #'(lambda (xnode)
				       (null (snode-closed xnode)))
				   (snode-children node))))
	   node-selected)
	  (t
	   (setf (snode-closed node) t)
	   (cond ((not (null (snode-parent node)))
		  (next-open (snode-parent node)))
		 (t nil))))))


(defun store-h-extras (node h-function)
  (multiple-value-bind 
   (h-value relaxed-plan focus-goals) (funcall h-function node)
     (setf (snode-h-value node) h-value)
     (setf (snode-h-plus node) (length relaxed-plan))
     (setf (snode-relaxed-plan node) relaxed-plan)
;;SFA     (setf (snode-focus-goals node) focus-goals)
     (incf (get (problem-plist *current-problem*) :node-evaluated))

     (when (= h-value most-positive-fixnum)
       (setf (snode-closed node) t))
     ))

(defun map-heuristic (node h-function)
  (dolist (inode (snode-children node))
    (store-h-extras inode h-function)))

  
(defun store-a-star-extras (inode cost-fn)
  (setf (snode-g-value inode) (funcall cost-fn inode))
  (setf (snode-f-value inode) (+ (snode-h-value inode) (snode-g-value inode))))


(defun remove-node-by-number (parent node-number)
  (remove-if #'(lambda (ichild)
		 (when (= (snode-number ichild) node-number) t))
	     (snode-children parent)))


(defun map-astar-f (nodes cost-fn h-fn)
  (mapc #'(lambda (inode) 
	    (store-h-extras inode h-fn)
	    (setf (snode-g-value inode) (funcall cost-fn inode))
	    (setf (snode-f-value inode) (+ (snode-h-value inode) (snode-g-value inode))))
	nodes))


(defun node-path-cost (node)
  (cond ((snode-p (snode-parent node))
	 (+ (snode-g-value (snode-parent node)) (snode-cost node)))
	(t 0)))



(defun restore-nonhelpful (node h-fn)
  (setf (snode-children node) nil)
  (expand-state node :helpful nil)
  
  (map-heuristic node h-fn)
  (snode-children node)
)


(defun lessthan-f-metricplus (node1 node2)
  (cond ((< (snode-f-value node1) (snode-f-value node2)) t)
	((and (= (snode-f-value node1) (snode-f-value node2))
	      (< (snode-h-plus node1) (snode-h-plus node2))) t)
	(t nil)))


(defun recompute-relinked (node cost-fn)
  (dolist (jchild (snode-children node))
    (unless (snode-closed jchild)
      (setf (snode-depth jchild) (1+ (snode-depth (snode-parent jchild))))
      (store-a-star-extras jchild cost-fn)
      (unless (null (snode-children jchild))
	(recompute-relinked jchild cost-fn)))))


(defun relink-g-parent (node old-repeated cost-fn)
  (let ((parent (snode-parent node))
	(old-parent (snode-parent old-repeated)))
    (when (= 2 *say-output*)
      (format t "~% ============RELINKING ============================== ")
      (format t "~% New Node: ~a          Parent    : ~a" (snode-number node) (snode-number parent))
      (format t "~% Repeated: ~a          Old Parent: ~a" (snode-number old-repeated) (snode-number old-parent))
      (format t "~% ============RELINKING ============================== "))
    
    (setf (snode-closed node) t)
    
    (setf (snode-parent old-repeated) parent)
    (setf (snode-depth old-repeated) (1+ (snode-depth parent)))
    
;;;;;;;;;;     (setf (snode-children parent) (remove-node-by-number parent (snode-number node)))
    (push old-repeated  (snode-children parent))
    (setf (snode-children old-parent) (remove-node-by-number old-parent (snode-number old-repeated)))
    (recompute-relinked old-repeated cost-fn)
    ))




(defun better-g-path (repeated-node node cost-fn)
  (let ((new-g (funcall cost-fn node)))
    (cond ((< new-g (snode-g-value repeated-node)) t))))



(defun compute-inconsistent-h-nodes (node)
  (dolist (i-child (snode-children node))
    (when (numberp (snode-h-value i-child))
      (cond ((> (- (snode-h-value node) (snode-h-value i-child)) (snode-cost i-child))
	     (push i-child (gethash 'inc- *hash-inconsistent-h*) ))
	    ((> (- (snode-h-value i-child) (snode-h-value node)) (snode-cost i-child))
	     (push i-child (gethash 'inc+ *hash-inconsistent-h*) ))
	    ))))



(defun get-enforced-heuristic (node h-function current-h visited-nodes &key (discrete nil)
			       (current-h-plus 0))
  (let ((better-found nil) (node-counter 0))
    (dolist (i-child (snode-children node) better-found)
      (when (null better-found)
	(cond ((not (find i-child (gethash (snode-hash-code i-child) visited-nodes) :test #'equal-state))
	       (incf node-counter)
	       (store-h-extras i-child h-function)
	       (cond (discrete
		      (when (< (snode-h-value i-child) current-h)
			(setf better-found i-child)
			(when *trace-ehc*
			  (push node-counter *hit-node*))))
		     (t
		       (when (or (< (snode-h-value i-child) current-h)
				 (and (= (snode-h-value i-child) current-h)
				      (< (snode-h-plus i-child) current-h-plus)))
			 (setf better-found i-child))))) 
	      (t
	       (setf (snode-closed i-child) t)))
	       ))))


(defun lessthan-h-metricplus (node1 node2)
  (cond ((< (snode-h-value node1) (snode-h-value node2)) t)
	((and (= (snode-h-value node1) (snode-h-value node2))
	      (< (snode-h-plus node1) (snode-h-plus node2))) t)
	(t nil)))


(defun ehc-open-nodes (open-children sort-option)
  (cond ((equal sort-option 'h-value)
	 (stable-sort open-children #'lessthan-h-metricplus))
	(t open-children)))


(defun enforced-hill-climbing (init-node h-fn cost-fn search-options &optional (problem *current-problem*))
  (declare (ignore cost-fn))
  (let ((open-nodes nil) (open-children nil)
	(node init-node) (next-node nil) (visited (make-hash-table))
;; 	(previous-state (pp-state (snode-state init-node) 'list))
	(use-rules-p (car (find-argument search-options :use-rules-p)))
	(current-h 0) (current-h-plus 0) (better-h-node nil) (discrete-h (not (is-metric-domain))))
    (say-consed-bytes :mem-per-node (store-h-extras node h-fn))
    (when *trace-mem* (write-node-mem-info 'ehc))
    (setf current-h (snode-h-value node))
    (setf current-h-plus (snode-h-plus node))
    (when (and use-rules-p *rete-root-node*)
	(push-to-rete-goals)
	(push-to-rete-object-types)
      (push-to-rete-state init-node t))
;;       (push-to-rete-state nil previous-state))
;;     (setf open-nodes (list node))
    (do* ((stop-this (stop-search node problem) (stop-search node problem)))
	 (stop-this (when-stop (cdr stop-this) node problem))
      (trace-search-extras (snode-number node))
      (expand-state node :helpful *say-helpful*)
      (print-search-node node nil)
      (push node (gethash (snode-hash-code node) visited))	     
;;       (when *generate-learning-examples-p* (save-examples node better-h-node nil))
      (if use-rules-p
	  (prune-by-rules node))
      (setf better-h-node (get-enforced-heuristic node h-fn current-h visited :discrete discrete-h 
						  :current-h-plus current-h-plus))
      (cond ((snode-p better-h-node)
	     (setf current-h (snode-h-value better-h-node))
	     (setf current-h-plus (snode-h-plus better-h-node))
	     (setf node better-h-node)
	     (setf open-nodes nil)
	     (setf open-children nil))
	    (t
;; 	     (dolist (child (snode-children node))
;; 		 (if (not (snode-closed child))
;; 		     (push child open-children)))
	     (setf open-children (nconc (remove-if #'snode-closed (snode-children node)) 
					open-children))
	     (cond ((not (snode-p (setf next-node (pop open-nodes))))
		    (when (> *say-output* 1) (format t "  ~% [Expanding Breadth Level]"))
		    (setf open-nodes (ehc-open-nodes open-children (car (find-argument search-options :children-sort))))
		    (when (null open-nodes) ;;Recuperando la poda por las helpful actions
		      (setf open-nodes (restore-nonhelpful node h-fn))
		      (when (> *say-output* 1) 
			(format t "  ~%!!Restoring NON Helpful Nodes.")))
		    (setf node (pop open-nodes))
		    (setf open-children nil))
		   (t 
		    (setf node next-node)))))
      ;; Very inefficient way of doing it!!!! We have to call it when generating the next state and only with the adds and dels
    (if (and *rete-root-node* (car (find-argument search-options :use-rules-p)))
	(push-to-rete-state node nil))
;; 	(push-to-rete-state previous-state (setq previous-state (pp-state (snode-state node) 'list))))
      ;; 	 (when (snode-p node)
      ;; 	   (print-search-node node nil)
      )))

;; (defun enforced-hill-climbing (init-node h-fn cost-fn search-options &optional (problem *current-problem*))
;;   (declare (ignore cost-fn))
;;   (let ((open-nodes nil) (open-children nil)
;; 	(node init-node) (next-node nil) (visited (make-hash-table))
;; 	(current-h 0) (current-h-plus 0) (better-h-node nil) (discrete-h (not (is-metric-domain))))
;; 
;;     (say-consed-bytes :mem-per-node (store-h-extras node h-fn))
;;     (when *trace-mem* (write-node-mem-info 'ehc))
;; 
;;     (setf current-h (snode-h-value node))
;;     (setf current-h-plus (snode-h-plus node))
;; ;;     (setf open-nodes (list node))
;;     (do* ((stop-this (stop-search node problem) (stop-search node problem)))
;; 	 (stop-this (when-stop (cdr stop-this) node problem))
;;       
;;       (trace-search-extras (snode-number node))
;;       (expand-state node :helpful *say-helpful*)
;;       (print-search-node node nil)
;; 
;;       (push node (gethash (snode-hash-code node) visited))	     
;;       
;; ;;       (when *generate-learning-examples-p* (save-examples node better-h-node nil))
;; 
;;       (setf better-h-node (get-enforced-heuristic node h-fn current-h visited :discrete discrete-h 
;; 						  :current-h-plus current-h-plus))
;;       (cond ((snode-p better-h-node)
;; 	     (setf current-h (snode-h-value better-h-node))
;; 	     (setf current-h-plus (snode-h-plus better-h-node))
;; 	     (setf node better-h-node)
;; 	     (setf open-nodes nil)
;; 	     (setf open-children nil))
;; 	    (t 
;; 	     (setf open-children (append (remove-if #'snode-closed (snode-children node)) 
;; 				       open-children))
;; 	     
;; 	     (cond ((not (snode-p (setf next-node (pop open-nodes))))
;; 		    (when (> *say-output* 1) (format t "  ~% [Expanding Breadth Level]"))
;; 		    (setf open-nodes (ehc-open-nodes open-children (car (find-argument search-options :children-sort))))
;; 		    
;; 		    (when (null open-nodes) ;;Recuperando la poda por las helpful actions
;; 		      (setf open-nodes (restore-nonhelpful node h-fn))
;; 		      (when (> *say-output* 1) 
;; 			(format t "  ~%!!Restoring NON Helpful Nodes.")))
;; 		    (setf node (pop open-nodes))
;; 		    (setf open-children nil))
;; 		   (t 
;; 		    (setf node next-node)))))       
;;       ;; 	 (when (snode-p node)
;;       ;; 	   (print-search-node node nil)
;;       )))




(defun restore-breadth-nonhelpful (closed-level-list visited)
  (let ((new-open-list nil))
    ;;Removing from visited hash table
    (dolist (i-node closed-level-list)
      (dolist (i-child (snode-children i-node))
	(setf (snode-hash-code i-child)
	      (remove i-child (gethash (snode-hash-code i-child) visited) :key #'snode-number))))
    ;;Re-expanding with no helpful actions
    (dolist (i-node closed-level-list new-open-list)
      (expand-state i-node :helpful nil)
 
      (dolist (i-child (snode-children i-node))
	(cond ((not (find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state))
	       (push i-child (gethash (snode-hash-code i-child) visited)))
	      (t 
	       (setf (snode-closed i-child) t))))
      
      (setf new-open-list (append (remove-if #'snode-closed (snode-children i-node))
				    new-open-list)))))


;; I re-programm this algorithm to deal with a global ordering of breadth-search levels
;; The FF algorithm should be equivalent to this rather than the first Sayphi-EHC
(defun breadth-hc (init-node h-fn cost-fn search-options &optional (problem *current-problem*))
  (declare (ignore cost-fn search-options))
  (let ((node nil) (open-level-list (list init-node)) (closed-level-list nil)
	(visited (make-hash-table)) (breadth-depth 0)
	(current-h most-positive-fixnum) (current-h-plus most-positive-fixnum))

    (setf node (pop open-level-list))
    (do* ((stop-this (stop-search node problem) (stop-search node problem)))
	 (stop-this (when-stop (cdr stop-this) node problem))
      
      (store-h-extras node h-fn)
      (when (or (< (snode-h-value node) current-h)
		(and (= (snode-h-value node) current-h)
		     (< (snode-h-plus node) current-h-plus)))
	(setf current-h (snode-h-value node))
	(setf current-h-plus (snode-h-plus node))
	(setf open-level-list nil)
	(setf closed-level-list (list node))
	(setf breadth-depth 0)
	(print-search-node node nil) 
	) 
      (when (null open-level-list)
	;; Some kind of sort function for closed-level-list
	(incf breadth-depth)
	(when (> *say-output* 1) (format t "[*~d]" breadth-depth))
	(dolist (i-node closed-level-list)
	  (expand-state i-node :helpful *say-helpful*)
	  
	  (dolist (i-child (snode-children i-node))
	    (cond ((not (find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state))
		   (push i-child (gethash (snode-hash-code i-child) visited)))
		  (t 
		   (setf (snode-closed i-child) t))))
	  
	  (setf open-level-list (append (remove-if #'snode-closed (snode-children i-node))
				   open-level-list)))
	;;For restoring non-helpful actions [FF stop pruning!]
	(when (null open-level-list)
	  (when (> *say-output* 1) (format t "~% >> Restoring NON-HELPFUL Actions!!"))
	  (setf open-level-list (restore-breadth-nonhelpful closed-level-list visited))
	  (decf breadth-depth))

	;;Some kind of sort function for node ordering
	  
	(setf closed-level-list nil)
	)
      (setf node (pop open-level-list))
      (push node closed-level-list))))


      

(defun a-star (init-node h-fn cost-fn search-options)
  (declare (ignore search-options))
  (let ((open-nodes nil) (node init-node) (repeated nil) 
	(visited (make-hash-table)) (open-hash (make-hash-table))
	(current-h 0)
	(problem *current-problem*))

    (say-consed-bytes :mem-per-node (store-h-extras node h-fn))
    (when *trace-mem* (write-node-mem-info 'a-star))

    (store-a-star-extras node cost-fn)
    (setf current-h (snode-h-value node))
    (do* ((stop-this (stop-search node problem) (stop-search node problem)))
	 (stop-this (when-stop (cdr stop-this) node problem))

      (setf repeated nil)
      (push node (gethash (snode-hash-code node) visited))
      
      (trace-search-extras (snode-number node))
      (expand-state node :helpful nil)
      (dolist (i-child (snode-children node))
	(setf repeated (find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state))
       	(cond ((snode-p repeated)
;; 	       (format t "~% ========?? Repeated visited ~a" i-child)
	       (cond ((better-g-path repeated i-child cost-fn)
		      (relink-g-parent i-child repeated cost-fn))
		     (t 
		      (setf (snode-closed i-child) t)))))
	
	(setf repeated (find i-child (gethash (snode-hash-code i-child) open-hash) :test #'equal-state))
	(cond ((snode-p repeated)
;; 	       (format t "~% ========?? Repeated open-list ~a" i-child)
	       (cond ((better-g-path repeated i-child cost-fn)
		      (setf (snode-closed repeated) t))
		     (t 
		      (setf (snode-closed i-child) t)))))
       
	(unless (snode-closed i-child)
	  (store-h-extras i-child h-fn)
	  (store-a-star-extras i-child cost-fn)
	  (push i-child (gethash (snode-hash-code i-child) open-hash))
	  (push i-child open-nodes)))
	
      (setf open-nodes (stable-sort (remove-if #'snode-closed open-nodes) 
				    #'< :key #'snode-f-value))
      
;;       (pp-node-list open-nodes)
      (setf node (pop open-nodes))
      
      (when (snode-p node)
	(print-search-node node open-nodes))
      )))


		


(defun k-best-first (init-node h-fn cost-fn search-options &key (k-beam 2))
  (declare (ignore search-options))
  (let ((open-nodes nil) (node init-node) (repeated nil) (visited nil) (current-h 0)
	(beam-list nil)
	(problem *current-problem*))
    (store-h-extras node h-fn)
    (store-a-star-extras node cost-fn)
    (setf current-h (snode-h-value node))
    (do* ((stop-this (stop-search node problem) (stop-search node problem)))
	 (stop-this (when-stop (cdr stop-this) node problem))
      (setf repeated nil)
      (push node visited)
      (expand-state node :helpful nil)
      (dolist (i-child (snode-children node))
	(setf repeated (find i-child visited :test #'equal-state))
       	(cond ((snode-p repeated)
;; 	       (format t "~% ========?? Repeated visited ~a" i-child)
	       (cond ((better-g-path repeated i-child cost-fn)
		      (relink-g-parent i-child repeated cost-fn))
		     (t 
		      (setf (snode-closed i-child) t)))))
	
	(setf repeated (find i-child open-nodes :test #'equal-state))
	(cond ((snode-p repeated)
;; 	       (format t "~% ========?? Repeated open-list ~a" i-child)
	       (cond ((better-g-path repeated i-child cost-fn)
		      (setf (snode-closed repeated) t))
		     (t 
		      (setf (snode-closed i-child) t)))))
       
	(unless (snode-closed i-child)
	  (store-h-extras i-child h-fn)
	  (store-a-star-extras i-child cost-fn)
	  (push i-child open-nodes)))
      
      (when (null beam-list)
	(setf open-nodes (stable-sort (remove-if #'snode-closed open-nodes) 
				      #'< :key #'snode-f-value))
	(setf beam-list (subseq open-nodes 0 (min k-beam (length open-nodes))))
	(cond ((< k-beam (length open-nodes)) 
	       (setf open-nodes (subseq open-nodes k-beam)))
	      (t 
	       (setf open-nodes nil))))
        
      (setf node (pop beam-list))
      
      (when (snode-p node)
	(print-search-node node open-nodes))
      )))


(defun hill-climbing (init-node h-function cost-fn &key (minimize-h t) 
				(problem *current-problem*)
				(helpful *say-helpful*))
  (declare (ignore cost-fn))
  (let ((sort-function (if minimize-h #'< #'>))
	(visited nil)
	(node init-node)
	(next-node nil))
    (store-h-extras node h-function)

    (do* ((stop-this (stop-search node problem) (stop-search node problem)))
	 (stop-this (when-stop (cdr stop-this) node problem))
      (expand-state node :helpful helpful)
      (map-heuristic node h-function)
      
      (setf next-node (hc-next-node node visited sort-function))
      (cond ((snode-p next-node)
	     (push node visited)
	     (setf node next-node))
	    (t
	     (restore-nonhelpful node h-function)
	     (setf next-node (hc-next-node node visited sort-function))
	     (when (snode-p next-node)
	       (push node visited))
	     (setf node next-node)))
      (when (snode-p node)
	(print-search-node node nil))
      ;; 	 (break)
      )))

  
;; I include the evaluation to study h-relaxedplan behaviour      
(defun breadth-first (init-node h-fn cost-fn search-options)
  (declare (ignore cost-fn))
  (let ((visited (make-hash-table)) (open-nodes nil) (node init-node)
	(problem *current-problem*) (eval-h (not (find-argument search-options :not-eval))))
    
    (when eval-h (store-h-extras node h-fn))
    (do* ((stop-this (stop-search node problem) (stop-search node problem)))
	 (stop-this (when-stop (cdr stop-this) node problem))

      (expand-state node :helpful nil)
      (push node (gethash (snode-hash-code node) visited))

      (dolist (i-child (snode-children node))
	(cond ((find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state)
	       (setf (snode-closed i-child) t)
;; 	       (format t "~% >> Repeated ~a" (snode-number i-child))
	       )
	      
	      (t 
	       (when eval-h (store-h-extras i-child h-fn)))))
 
      (setf open-nodes (append open-nodes (remove-if #'snode-closed (snode-children node))))
      
      (setf node (pop open-nodes))
      (when (snode-p node)
	(print-search-node node nil)))))


;; Hill-climbing Branch & Bound It search first a solution with EHC
;; and then tries to improve the solution searching with a
;; backtracking Hill-climbing pruned by the first solution upper-cost
;; bound
(defun hc-bnb (init-node h-function cost-fn search-options &key 
				(problem *current-problem*)
				(helpful *say-helpful*))
  (when (> *say-output* 0) (format t "~% Finding EHC solution for Upper-cost Bound"))
  (let ((first-sol (enforced-hill-climbing init-node h-function cost-fn nil))
	(visited (make-hash-table)) (siblings nil)
	(hash-solutions (make-hash-table))
	(node init-node) (repeated nil)
	(upper-cost-bound nil)
	(not-prune-inconsistent (car (find-argument search-options :not-prune-inconsistent)))
	(max-solutions (car (find-argument search-options :max-solutions)))
	)
    
    
    (setf *hash-inconsistent-h* (make-hash-table))
    (setf *say-hash-solutions* hash-solutions)
    (cond ((and (solution-p first-sol)
		(solution-found first-sol) (> (solution-depth first-sol) 0))
	   (setf upper-cost-bound (solution-total-cost first-sol))
	   (setf (snode-closed (solution-last-node first-sol)) t)
	   (setf (gethash 1 hash-solutions) first-sol)

	   (store-a-star-extras node cost-fn)
	   (push node (gethash (snode-hash-code node) visited))
	   (when (> *say-output* 0) (format t "~% Running Branch & Bound [~a]" upper-cost-bound))
	   
	   (do* ((stop-this (stop-multiple node problem hash-solutions max-solutions) 
			    (stop-multiple node problem hash-solutions max-solutions)))
		((car stop-this) (when-multiple-stop (cdr stop-this) node problem)) 
;; 	     (format t "~% =====TRACE===========")
	     (when (snode-closed node)  ;;Goals reached
	       (when (< (solution-total-cost (gethash (hash-table-count hash-solutions) hash-solutions)) upper-cost-bound)
		 (setf *say-solution* (gethash (hash-table-count hash-solutions) hash-solutions))
		 (setf upper-cost-bound (solution-total-cost *say-solution*))
		 (when (> *say-output* 0) 
		   (format t "~% => BnB >> Found new solution with Cost {~a}" (solution-total-cost *say-solution*)))
		 )
	       
	       (setf node (snode-parent node)))
	     (unless (snode-expanded node) 
	       (expand-state node :helpful helpful))
	     (dolist (i-child (snode-children node))
	       (unless (snode-closed i-child)
		 (cond ((null (snode-h-value i-child))
			(setf repeated (find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state))
			(cond ((or (not (snode-p repeated))
				   (and (snode-p repeated)
					(not (better-g-path repeated i-child cost-fn))))
			       (store-h-extras i-child h-function)
			       
			       (push i-child (gethash (snode-hash-code i-child) visited)))
			      (t
			       (setf (snode-closed i-child) t)
			       (when (> *say-output* 1) 
				 (format t "~% == BnB ==>> Not evaluated repeated node ~a" (snode-number i-child)))
			       )))
		       (t
			(when (and (not (snode-closed i-child))
				   (not (find i-child (gethash (snode-hash-code i-child) visited) :key #'snode-number)))
			  (push i-child (gethash (snode-hash-code i-child) visited)))))
		 (when (and (not (snode-closed i-child)) (null (snode-f-value i-child)))
		   (store-a-star-extras i-child cost-fn))
		 ))
	     
	     (compute-inconsistent-h-nodes node)
	     (setf siblings (stable-sort (remove-if #'(lambda (i-sibling)
							(or (snode-closed i-sibling)
							    (and (>= (snode-f-value i-sibling) upper-cost-bound)
								 (or (not not-prune-inconsistent)
								     (and not-prune-inconsistent
									  (<= (- (snode-h-value i-sibling) (snode-h-value node))
									      (snode-cost i-sibling)))))))
						    (snode-children node)) 
					 #'< :key #'snode-f-value))
	     (cond ((snode-p (car siblings))
		    (setf (snode-selected (snode-parent (car siblings))) (car siblings))
		    (setf node (car siblings)))
		   (t 
		    (setf (snode-selected node) nil)
		    (setf (snode-closed node) t)
		    (when (> *say-output* 1) (format t "~% == BnB ==>> Cost Pruning at node ~a" (snode-number node)))
		    (setf node (if (snode-p (snode-parent node)) (snode-parent node) nil))
		    
		    ))
	     
	     (when (snode-p node)
	       (print-search-node node nil))
	     ))
	  ;; 	  In case of no solution was found
	  (t first-sol))))
    
    
    
;;SFA. Hay que usar esta función
(defun initialize-current-problem ()
  (let ((problem *current-problem*))
    (update-artificial-initstate)
    (instantiate-operators)
    (init-heuristics)
    
    (setf (get (problem-plist problem) :node-counter) 0)
    (setf (get (problem-plist problem) :node-evaluated) 0)

    (setf (problem-search-tree problem)
	  (make-snode :number 0
		      :depth 0
		      :state (copy-state (problem-init-state problem))))))
;; 
;; (defun initialize-current-problem ()
;;   (let ((problem *current-problem*))
;;     (update-artificial-initstate)
;;     (build-actions-instpattern)
;; 
;;     (build-actions-bytes)
;;     (build-direct-predinstance)
;;     
;;     (setf (dom-pred-achievers *pspace*) (build-pred-achievers))
;;     (setf (dom-numeric-achievers *pspace*) (build-metric-achievers))
;;     (setf (get (dom-plist *pspace*) :direct-actions) (create-actions-direct-access))
;;         
;;     (create-inst-precfuns)
;;     (create-inst-costs)
;;     (split-numvars-range)
;;     (remove-redundant-inst-action)
;;     
;;     (setf (get (problem-plist problem) :node-counter) 0)
;;     (setf (get (problem-plist problem) :node-evaluated) 0)
;;     (setf (get (problem-plist problem) :inst-goals) 
;; 	  (get-instantiated-goals (problem-goals problem)))
;;     (setf (problem-search-tree problem)
;; 	  (make-snode :number 0
;; 		      :depth 0
;; 		      :state (copy-state (problem-init-state problem))))))
;; 
;; 

(defun say-plan-defaults (plan-option)
  (case plan-option
    (heuristic (cond ((is-metric-domain) #'h-metric-rxplan)
		     (t #'h-relaxedplan)))
    (algorithm #'enforced-hill-climbing)))


(defun plan (&key (algorithm nil)
		  (heuristic nil)
		  (cost *say-costfn*)
	          (runtype nil)
		  (timeout *say-timeout*)
		  (depthbound *say-depthbound*)
	          (max-solutions most-positive-fixnum)
	          (special-prune nil)
	     	  (try2-search *say-2try*)
		  (rules-file (concatenate 'string *domain-dir* "rules.lisp"))
		  (use-rules-p nil))
  (declare (special *current-problem*)
	   (ignore try2-search))
  (let ((problem *current-problem*)
	(i-node nil)
	(algorithm-fn (if (not (null algorithm)) (symbol-function algorithm) (say-plan-defaults 'algorithm)))
	(heuristic-fn (if (not (null heuristic)) (symbol-function heuristic) (say-plan-defaults 'heuristic)))
	(cost-fn (symbol-function cost))
	(start-time (get-internal-run-time))
	(search-options nil)
	(sol nil))
    (set-sayp :say-timeout timeout)
    (set-sayp :say-depthbound depthbound)
    (reset-ehc-trace)
    (sayout-initialize)
    (setf i-node (initialize-current-problem))
    ;; this is DB trying to do sme debugging and accessing tree
    (setf *top-level-node* i-node)
    (set-duplicate-hashing)
    (setf (get (problem-plist problem) :initial-pre-time) start-time)
    (setf (get (problem-plist problem) :initial-search-time) (get-internal-run-time))
    (setf  *search-failed* nil)
    (setf *say-solution* nil)
    (case runtype
      (ehc-sort-h
       (setf search-options '((:children-sort h-value))))
      (breadth-not-eval
       (setf search-options '((:not-eval t)))))

    ;; Loads the control rules from the file and builds the rete net
    (when (and use-rules-p rules-file)
      (load-rules-build-rete rules-file)
      (push (list :use-rules-p t) search-options))
    
    (when (numberp max-solutions)
      (push (list :max-solutions max-solutions) search-options))
    (when special-prune
      (push (list :not-prune-inconsistent t) search-options))
 
    (sayout-search t algorithm heuristic cost)
    (setf sol (funcall algorithm-fn i-node heuristic-fn cost-fn search-options))

;;     Turning to best-first algorithm without helpful actions
;;     (cond ((and *search-failed* try2-search)
;; 	   (setf i-node (initialize-current-problem))
;; 	   (setf sol (best-first i-node heuristic-fn cost-fn)))
;; 	  (t sol))

))
;; (defun plan (&key (algorithm nil)
;; 		  (heuristic nil)
;; 		  (cost *say-costfn*)
;; 	          (runtype nil)
;; 		  (timeout *say-timeout*)
;; 		  (depthbound *say-depthbound*)
;; 	          (max-solutions most-positive-fixnum)
;; 	          (special-prune nil)
;; 	     	  (try2-search *say-2try*))
;;   (declare (special *current-problem*)
;; 	   (ignore try2-search))
;;   (let ((problem *current-problem*)
;; 	(i-node nil)
;; 	(algorithm-fn (if (not (null algorithm)) (symbol-function algorithm) (say-plan-defaults 'algorithm)))
;; 	(heuristic-fn (if (not (null heuristic)) (symbol-function heuristic) (say-plan-defaults 'heuristic)))
;; 	(cost-fn (symbol-function cost))
;; 	(start-time (get-internal-run-time))
;; 	(search-options nil)
;; 	(sol nil))
;; 
;; 
;;     (set-sayp :say-timeout timeout)
;;     (set-sayp :say-depthbound depthbound)
;; 
;;     (reset-ehc-trace)
;;     (sayout-initialize)
;;     (setf i-node (initialize-current-problem))
;;     (set-duplicate-hashing)
;; 
;;     (setf (get (problem-plist problem) :initial-pre-time) start-time)
;;     (setf (get (problem-plist problem) :initial-search-time) (get-internal-run-time))
;;     
;;     (setf  *search-failed* nil)
;;     (setf *say-solution* nil)
;; 
;;     (case runtype
;;       (ehc-sort-h
;;        (setf search-options '((:children-sort h-value))))
;;       (breadth-not-eval
;;        (setf search-options '((:not-eval t)))))
;;     
;;     (when (numberp max-solutions)
;;       (push (list :max-solutions max-solutions) search-options))
;;     (when special-prune
;;       (push (list :not-prune-inconsistent t) search-options))
;;  
;;     (sayout-search t algorithm heuristic cost)
;;     (setf sol (funcall algorithm-fn i-node heuristic-fn cost-fn search-options))
;; 
;; ;;     Turning to best-first algorithm without helpful actions
;; ;;     (cond ((and *search-failed* try2-search)
;; ;; 	   (setf i-node (initialize-current-problem))
;; ;; 	   (setf sol (best-first i-node heuristic-fn cost-fn)))
;; ;; 	  (t sol))
;; 
;; ))

