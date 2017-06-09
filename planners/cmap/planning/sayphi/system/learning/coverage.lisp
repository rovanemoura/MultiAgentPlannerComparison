;;; ***************************************************************************************
;;;        Coverage-based search
;;; ***************************************************************************************

;; search-options include :coverage and :length
;; anchors are the nodes from which there is a path to some tree
(defun cover-domain (init-node h-function cost-fn search-options &optional (problem *current-problem*))
  (declare (ignore cost-fn))
  (let ((helpful (search-option-value :helpful search-options))
	(visited (make-hash-table))
	(nodes (list init-node))
	(anchors (make-hash-table))
	(coverage (search-option-value :coverage search-options))
	(length (search-option-value :length search-options))
	(node nil))
    (push node (gethash (snode-hash-code node) visited))
    (dotimes (i (/ coverage length))
      (setq node (choose-one nodes))
      (push node (gethash (snode-hash-code node) anchors))
      (if (not (snode-children node))
	  (errt-expand-state node :helpful helpful))
      (dotimes (j length)
	(when node
	  (if (not (snode-children node))
	      (setq node (snode-parent node)))
	  (when node
	    (setq node (choose-one (snode-children node)))
	    (cond ((find node (gethash (snode-hash-code i-child) visited) :test #'equal-state)
		   (setq node (snode-parent node)))
		  (t (errt-expand-state node :helpful helpful)
		     (push node nodes)
		     (push node (gethash (snode-hash-code node) visited))))))))
    anchors))

;; the list of anchors should be given as search-options
(defun coverage-based-search (init-node h-function cost-fn search-options &optional (problem *current-problem*))
  (declare (ignore cost-fn))
  (let ((helpful (search-option-value :helpful search-options))
	(lookahead (search-option-value :lookahead search-options))
	(anchors (search-option-value :anchors search-options))
	(visited (make-hash-table))
	(node init-node)
	(next-node nil))
    (find-path (find-path node (closest-node-from-anchors node anchors)) goals)))
