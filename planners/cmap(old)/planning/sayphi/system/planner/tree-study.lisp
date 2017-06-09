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
;; Description: Search Tree Study Functions
;; Date: 2006.12.16
;; 
;; ========================================================================

(defvar *subtree-analysis* nil)


(defstruct (ehctree (:print-function ehctree-print))
  (sol-length 0)
  (total-valid-nodes 0)
  (total-eval-nodes 0)
  (total-expand-nodes 0)
  
  (total-estimated-nodes 0)
  (total-estimated-skips 0)
  (total-real-skips 0)
  (num-subtrees 0)
  (avg-branching-factor 0)
  (subtree-nodes nil)
  (subtree-valid-nodes nil)
  (subtree-eval-nodes nil)
  (subtree-expand-nodes nil)
  (subtree-depth nil)
  (subtree-branch-factor nil)
  (subtree-estimated-nodes nil)
  (subtree-skipped-nodes nil)
)


(defmacro st-branch (sba n-subtree)
  `(aref (ehctree-subtree-branch-factor ,sba) ,n-subtree))
(defmacro st-depth (sba n-subtree)
  `(aref (ehctree-subtree-depth ,sba) ,n-subtree))
(defmacro st-estimated (sba n-subtree)
  `(aref (ehctree-subtree-estimated-nodes ,sba) ,n-subtree))
(defmacro st-valid (sba n-subtree)
  `(aref (ehctree-subtree-valid-nodes ,sba) ,n-subtree))
(defmacro st-eval (sba n-subtree)
  `(aref (ehctree-subtree-eval-nodes ,sba) ,n-subtree))


(defmacro n-subtre-print (sba n-subtree)
  `(let ((vnode (aref (ehctree-subtree-nodes ,sba) ,n-subtree)))
    (subtree-print vnode (snode-h-value vnode) 0 t)))


;; It returns the number of nodes in a standard BFS (see Korf Book)
(defun bfs-num-nodes (b d)
  (cond ((> b 1)
	 (floor (/ (- (expt b (+ d 1)) 1)
		   (- b 1))))
	(t d)))




;; For Output file
;; 3. Length
;; 4. Num Subtrees
;; 5. Plateaus Factor
;; 6. Branching Factor
;; 7. % Evaluated Nodes
;; 8. Estimated Skips
(defun list-treestudy (count-prob file-name sba)
  (list count-prob file-name (ehctree-sol-length sba)
	(ehctree-num-subtrees sba) 
	(float (/ (ehctree-num-subtrees sba)
		  (ehctree-sol-length sba)))
	(ehctree-avg-branching-factor sba)
	(float (/ (ehctree-total-eval-nodes sba)
		  (ehctree-total-estimated-nodes sba)))
	(ehctree-total-estimated-skips sba)))
	       

(defun ehctree-print (ehctree stream z)
  (declare (type ehctree ehctree)
	   (stream stream)
	   (ignore z))
   (format stream "~% EHC SEARCH TREE ")
  (format stream "~%   Solution Length    : ~a" (ehctree-sol-length ehctree))
  (format stream "~%   Num Subtrees       : ~a" (ehctree-num-subtrees ehctree))
  (format stream "~%   Num Valid Nodes    : ~a" (ehctree-total-valid-nodes ehctree))
  (format stream "~%   Num Evaluated Nodes: ~a" (ehctree-total-eval-nodes ehctree))
  (format stream "~%   Num Expanded Nodes : ~a" (ehctree-total-expand-nodes ehctree))
  (format stream "~%   Branching Factor   : ~a" (ehctree-avg-branching-factor ehctree))
  (format stream "~%   ----------------------------------------------")
  (format stream "~%   Num Estimated Nodes: ~a" (ehctree-total-estimated-nodes ehctree))
  (format stream "~%   Num Estimated Skip : ~a" (ehctree-total-estimated-skips ehctree))
  (format stream "~%   Real Skipped Nodes : ~a" (ehctree-total-real-skips ehctree))

  (format stream "~%   ----------------------------------------------")
  (format stream "~%   Valid    : ~a" (ehctree-subtree-valid-nodes ehctree))
  (format stream "~%   Evaluated: ~a" (ehctree-subtree-eval-nodes ehctree))
  (format stream "~%   Expandedd: ~a" (ehctree-subtree-expand-nodes ehctree))
  (format stream "~%   Depth    : ~a" (ehctree-subtree-depth ehctree))
  (format stream "~%   Branching: ~a" (ehctree-subtree-branch-factor ehctree))
  (format stream "~%   Estimated: ~a" (ehctree-subtree-estimated-nodes ehctree))
  (format stream "~%   Skipped  : ~a" (ehctree-subtree-skipped-nodes ehctree))
 )
 


(defun ehc-count-valid-nodes (xnode)
  (cond ((null (snode-closed xnode))
	 (1+ (apply #'+ (mapcar #'ehc-count-valid-nodes (snode-children xnode)))))
	(t 
	 0)))


(defun ehc-count-expanded-nodes (xnode)
  (cond ((snode-expanded xnode)
	 (1+ (apply #'+ (mapcar #'ehc-count-expanded-nodes (snode-children xnode)))))
	(t 
	 0)))


;; It counts the initial node as a valid node
(defun subtree-count-valid-nodes (xnode top-h)
  (cond ((null (snode-closed xnode))
	 (cond ((and (numberp (snode-h-value xnode))
		     (>= (snode-h-value xnode) top-h))
		(1+ (apply #'+ (mapcar #'(lambda (inode)
				    (subtree-count-valid-nodes inode top-h)) (snode-children xnode)))))
	       (t 1)))
	(t 0)))

;; It counts the initial sub-tree node as evaluated
(defun subtree-count-evaluated-nodes (xnode top-h)
  (cond ((numberp (snode-h-value xnode))
	 (cond ((>= (snode-h-value xnode) top-h)
		(1+ (apply #'+ (mapcar #'(lambda (inode)
					   (subtree-count-evaluated-nodes inode top-h)) (snode-children xnode)))))
	       (t 1)))
	(t 0)))

(defun subtree-count-expanded-nodes (xnode top-h)
  (cond ((snode-expanded xnode)
	 (cond ((and (numberp (snode-h-value xnode))
		     (>= (snode-h-value xnode) top-h))
		(1+ (apply #'+ (mapcar #'(lambda (inode)
				    (subtree-count-expanded-nodes inode top-h)) (snode-children xnode)))))
	       (t 0)))
	(t 0)))


(defun subtree-indent (depth)
  (let ((indent " "))
    (dotimes (i depth indent)
      (setf indent (concatenate 'string indent "  ")))))

(defun subtree-depth (xnode)
  (do ((inode xnode (snode-selected inode))
       (subtree-depth 0))
      ((or (null inode)
	   (< (snode-h-value inode) (snode-h-value xnode)))
       subtree-depth)
    (incf subtree-depth)))



(defun subtree-print (xnode top-h depth &optional (selected nil))
  (let ((num-next-node (if (snode-p (snode-selected xnode))
			   (snode-number (snode-selected xnode)) -1))
	(mark (if selected "#" " ")))
    (cond ((null (snode-closed xnode))
	   (cond ((numberp (snode-h-value xnode))
		  (cond ((>= (snode-h-value xnode) top-h)
			 (format t "~% ~a~d >~a~a {~d}[~a]" (subtree-indent depth) depth mark 
				 (snode-plan-action xnode) (snode-number xnode) (snode-h-value xnode))
			 (dolist (inode (snode-children xnode))
			   (if (= (snode-number inode) num-next-node)
			       (subtree-print inode top-h (1+ depth) t)
			       (subtree-print inode top-h (1+ depth))
			      )))
			(t
			 (format t "~% ~a~d >~a~a {~d}[~a]" (subtree-indent depth) depth "&"
				 (snode-plan-action xnode) (snode-number xnode) (snode-h-value xnode)))))
		 (t
		  (format t "~% ~a~d @ ~a ~a:{~d}" (subtree-indent depth) depth (snode-plan-action xnode) 
			  (snode-number xnode) (snode-h-value xnode)))
		 ))
	  (t 0))))



;; This is implemented with the selected attribute of node
(defun get-num-path-improvement (xnode)
  (do ((inode xnode (snode-selected inode))
       (current-h (snode-h-value xnode))
       (num-subtrees 0))
      ((null inode) num-subtrees)
    (when (< (snode-h-value inode) current-h)
      (incf num-subtrees)
      (setf current-h (snode-h-value inode))
      )))
     

    
(defun get-avg-branching-factor (xnode)
  (float (/ (ehc-count-valid-nodes xnode) (ehc-count-expanded-nodes xnode))))


(defun ehc-next-subtree-node (xnode)
  (do* ((inode xnode (snode-selected inode))
	(top-h (snode-h-value xnode))
	(current-h (snode-h-value xnode) (if (snode-p inode) (snode-h-value inode) -1)))
       ((< current-h top-h) 
	(when (and (snode-p inode) 
		   (> (snode-h-value inode) 0))
	  inode))
    ()))




;; It extracts subtrees information and stores it in a array of list (VALID EVAL EXPAND)

(defun extract-subtree-eval-info (sba)
  (let ((num-subtrees (ehctree-num-subtrees sba))
	(subtree-node nil) (subtree-top-h nil))
    (setf (ehctree-subtree-nodes sba) (make-array num-subtrees :element-type 'snode))
    (setf (ehctree-subtree-valid-nodes sba) (make-array num-subtrees :element-type 'integer))
    (setf (ehctree-subtree-eval-nodes sba) (make-array num-subtrees :element-type 'integer))
    (setf (ehctree-subtree-expand-nodes sba) (make-array num-subtrees :element-type 'integer))
    (setf (ehctree-subtree-depth sba) (make-array num-subtrees :element-type 'integer))

    (setf subtree-node (problem-search-tree *current-problem*))
    (dotimes (i-subtree num-subtrees sba)
      (setf subtree-top-h (snode-h-value subtree-node))
      (setf (aref (ehctree-subtree-nodes sba) i-subtree) subtree-node)
      (setf (aref (ehctree-subtree-valid-nodes sba) i-subtree) 
	    (1- (subtree-count-valid-nodes subtree-node subtree-top-h)))
      (setf (aref (ehctree-subtree-eval-nodes sba) i-subtree) 
	    (1- (subtree-count-evaluated-nodes subtree-node subtree-top-h)))
      (setf (aref (ehctree-subtree-expand-nodes sba) i-subtree) 
	    (subtree-count-expanded-nodes subtree-node subtree-top-h))
      (setf (aref (ehctree-subtree-depth sba) i-subtree) 
	    (subtree-depth subtree-node))
      
      (setf subtree-node (ehc-next-subtree-node subtree-node))
)))

	  
(defun compute-subtree-info (sba)       
  (let ((num-subtrees (ehctree-num-subtrees sba)))
    (setf (ehctree-subtree-branch-factor sba) (make-array num-subtrees :element-type 'float))
    (setf (ehctree-subtree-estimated-nodes sba) (make-array num-subtrees :element-type 'integer))
    (setf (ehctree-subtree-skipped-nodes sba) (make-array num-subtrees :element-type 'integer))
    (dotimes (i-subtree num-subtrees sba)
      (setf (aref (ehctree-subtree-branch-factor sba) i-subtree)
	    (float (/ (aref (ehctree-subtree-valid-nodes sba) i-subtree)
		      (aref (ehctree-subtree-expand-nodes sba) i-subtree))))
      (setf (aref (ehctree-subtree-estimated-nodes sba) i-subtree)
	    (max (aref (ehctree-subtree-valid-nodes sba) i-subtree)
		 (1- (bfs-num-nodes (aref (ehctree-subtree-branch-factor sba) i-subtree)
				    (aref (ehctree-subtree-depth sba) i-subtree)))))
      (setf (aref (ehctree-subtree-skipped-nodes sba) i-subtree)
	    (- (aref (ehctree-subtree-estimated-nodes sba) i-subtree)
	       (aref (ehctree-subtree-eval-nodes sba) i-subtree)))
      
      (setf (ehctree-total-estimated-nodes sba)
	    (+ (ehctree-total-estimated-nodes sba) (aref (ehctree-subtree-estimated-nodes sba) i-subtree)))
      (setf (ehctree-total-estimated-skips sba)
	    (+ (ehctree-total-estimated-skips sba) (aref (ehctree-subtree-skipped-nodes sba) i-subtree)))
      )))



(defun subtree-info (sba n-subtree &key (stream t))
  (format t "~% Subtree Informations")
  (format t "~% -----------------------------")
  (format stream "~%  ~a ~%" (aref (ehctree-subtree-nodes sba) n-subtree))
  (format stream "~%   Valid    : ~a" (aref (ehctree-subtree-valid-nodes sba) n-subtree))
  (format stream "~%   Evaluated: ~a" (aref (ehctree-subtree-eval-nodes sba) n-subtree))
  (format stream "~%   Expandedd: ~a" (aref (ehctree-subtree-expand-nodes sba) n-subtree))
  (format stream "~%   Depth    : ~a" (aref (ehctree-subtree-depth sba) n-subtree))
  (format stream "~%   Branching: ~a" (aref (ehctree-subtree-branch-factor sba) n-subtree))
  (format stream "~%   Estimated: ~a" (aref (ehctree-subtree-estimated-nodes sba) n-subtree))
  (format stream "~%   Skipped  : ~a" (aref (ehctree-subtree-skipped-nodes sba) n-subtree)))



(defun ehc-searchtree-analysis ()
  (let* ((solution *say-solution*)
	 (sba (make-ehctree))
	 (init-node (problem-search-tree *current-problem*)))
    (when (solution-p solution)
      (setf (ehctree-sol-length sba) (solution-depth solution))
      (setf (ehctree-total-valid-nodes sba) (ehc-count-valid-nodes init-node))
      (setf (ehctree-total-eval-nodes sba) (solution-evaluated-nodes solution))
      (setf (ehctree-total-expand-nodes sba) (ehc-count-expanded-nodes init-node))
      (setf (ehctree-num-subtrees sba) (get-num-path-improvement init-node))
      (setf (ehctree-avg-branching-factor sba) (get-avg-branching-factor init-node))
      (setf (ehctree-total-real-skips sba) (- (ehctree-total-valid-nodes sba)
					      (ehctree-total-eval-nodes sba)))
      (extract-subtree-eval-info sba)
      (compute-subtree-info sba)
      )))




;; Removed from runsets when study-tree
(defun perform-tree-analysis ()
  (let ((ehc-tree (ehc-searchtree-analysis)))
    (study-data-analyze-depth ehc-tree 'diffset runtype (get-problem-subset))
    (study-data-analyze-depth ehc-tree 'testset runtype)
    (unless (null experiment-var)
      (push (list-treestudy count-prob (pathname-name thisprob) ehc-tree)
	    (gethash experiment-var *experiment-study*)))))


(defun ehctree-data-toanalyze (out-file &key (fields '(2 3 4 5 6 7)))
  (let ((file-path (format nil "~a/result/~a" *domain-dir* out-file))
	(test-primary (this-expresult (car *experiment-tests*))))
    (with-open-file (out-stream file-path :direction :output :if-exists 
			      :supersede :if-does-not-exist :create)
       (dotimes (i (length test-primary))
	 (format out-stream "~a ~t ~a ~t" (car (nth i test-primary)) (second (nth i test-primary)))
	 (dolist (test *experiment-tests*)
	   (dolist (i-field fields)
	       (format out-stream "~a ~t"
		       (format-plotfield (nth i-field (nth i (this-studyresult test)))))))
	 (format out-stream "~%")))))


;; ==========================================================================
;; INTERFACE PARA FICHEROS DE SALIDA
;; ==========================================================================

(defun get-max-subtree-depth (sba)
  (let ((max-depth 0))
    (dotimes (i-sb (ehctree-num-subtrees sba) max-depth)
      (when (> (aref (ehctree-subtree-depth sba) i-sb) max-depth)
	(setf max-depth (aref (ehctree-subtree-depth sba) i-sb))))))


(defun study-data-analyze-depth (sba set-type run-type &optional (nset 0))
  (let ((data-file "")
	(max-subtree (get-max-subtree-depth sba)))
    (dotimes (i-depth max-subtree)
      (cond ((eq set-type 'problem)
	     (setf data-file (format nil "~a/result/prob_~a_~a_d~d_data.txt" 
				     *domain-dir* (get-problem-filename) run-type i-depth)))
	    ((eq set-type 'diffset)
	      (setf data-file (format nil "~a/result/diffset_~a_~a_d~d_data.txt" 
				     *domain-dir* nset run-type i-depth)))
	    ((eq set-type 'testset)
	     (setf data-file (format nil "~a/result/testset_~a_d~d_data.txt" 
				      *domain-dir* run-type i-depth))))
            
      (with-open-file (out-stream data-file :direction :output :if-exists 
				  :append :if-does-not-exist :create)
	(dotimes (i-subtree (ehctree-num-subtrees sba))
	  (when (= (1+ i-depth) (st-depth sba i-subtree))
	    (format out-stream "~2$ ~t ~2$ ~%" (st-branch sba i-subtree) 
		    (float (/ (st-eval sba i-subtree) (st-estimated sba i-subtree))))

    ))))))



(defun write-analysis-plot-script (plot-name set-type algorithm datafile-prefix max-depth)
  (let ((plot-file (format nil "~a/result/~a.plt" *domain-dir* plot-name)))
    (with-open-file (out-stream plot-file :direction :output :if-exists 
				:supersede :if-does-not-exist :create)
      (format out-stream "set size square ~%")
      (format out-stream "set terminal postscript color eps 'Helvetica' 10~%")
      (format out-stream "set output '~a_study.ps'~%" plot-name)
      (format out-stream "set multiplot ~%")
      (format out-stream "~%")
      (format out-stream "set title '~a ~a' ~%" set-type algorithm)
      (format out-stream "set xlabel 'Branching Factor'~%")
      (format out-stream "set ylabel '% Eval Nodes'~%")
      (format out-stream "set xrange [1:20]~%")
      (format out-stream "set yrange [0:1]~%")
      (format out-stream "set key 13.5, 0.95~%")
      (format out-stream "~%")
      (format out-stream "plot 1/x title 'Best_Eval', (1 + x)/(2*x) title 'Avg Eval'~%")
      (format out-stream "~%")
      (format out-stream "set key 13.5,0.9 ~%")
      (format out-stream "~%plot ")
      
      (dotimes (i max-depth)
	(let ((break-line (if (= i (1- max-depth)) 
			      (format nil "~%~%")
			      (format nil ",\\~%     "))))
	  (format out-stream "'~a-d~d_data.txt' using 1:2 title 'depth ~d' ~a"
		  datafile-prefix i i break-line))))))



(defparameter *study-plot-approaches* '(ehc ehc-sort-h ehc-cbr-allob ehc-cbr-utility))
(defparameter *study-plot-customize* '((ehc (0.5 0.5 0 0 "EHC"))
				       (ehc-sort-h (0.5 0.5 0.5 0 "EHC Sorted H"))
				       (ehc-cbr-allob (0.5 0.5 0 0.5 "CBR"))
				       (ehc-cbr-utility (0.5 0.5 0.5 0.5 "CBR Utility"))))
				 
;; This automatic plot must be generated for a 4 graphics per page
(defun write-analysis-vs-plot-script (plot-name set-type datafile-prefix max-depth)
  (let ((plot-file (format nil "~a/result/~a.plt" *domain-dir* plot-name)))
    (with-open-file (out-stream plot-file :direction :output :if-exists 
				:supersede :if-does-not-exist :create)

      (format out-stream "set terminal postscript color eps 'Helvetica' 10~%")
      (format out-stream "set output '~a_study.ps'~%" plot-name)
      (format out-stream "set multiplot ~%")
      (format out-stream "~%")
      
      (dolist (i-plot *study-plot-approaches*)
	(let ((customize (cadr (assoc i-plot *study-plot-customize*))))
	  (format out-stream "set size ~1$, ~1$ ~%" (nth 0 customize) (nth 1 customize))
	  (format out-stream "set origin ~1$, ~1$ ~%"(nth 2 customize) (nth 3 customize))
	  (format out-stream "~%")
	  (format out-stream "set title '~a ~a' ~%" set-type (nth 4 customize))
	  (format out-stream "set xlabel 'Branching Factor'~%")
	  (format out-stream "set ylabel '% Eval Nodes'~%")
	  (format out-stream "set xrange [1:20]~%")
	  (format out-stream "set yrange [0:1]~%")
	  (format out-stream "set key 18.0, 0.95~%")
	  (format out-stream "~%")
	  (format out-stream "plot 1/x title 'Best_Eval', (1 + x)/(2*x) title 'Avg Eval'~%")
	  (format out-stream "~%")
	  (format out-stream "set key 18.0,0.85 ~%")
	  (format out-stream "~%plot ")
	  
	  (dotimes (i max-depth)
	    (when (probe-file (format nil "~a/result/~a_~a_d~d_data.txt" *domain-dir* datafile-prefix i-plot i))
	      (when (> i 0)
		(format out-stream ",\\~%     "))
	      (format out-stream "'~a_~a_d~d_data.txt' using 1:2 title 'depth ~d' " datafile-prefix i-plot i i)
	      ))
	  (format out-stream "~%")
	  )))))


(defun generate-analysis-plot-files (n-times &key (testset t)
				     (diffset t))
  (when diffset
    (dotimes (i n-times)
      (write-analysis-vs-plot-script 
       (format nil "plot_diffset_~a" (format-groupprob (1+ i)))
       (format nil "Difficulty Set ~a" (format-groupprob (1+ i)))
       (format nil "diffset_~a" (format-groupprob (1+ i)))
       15)
      ))
  (when testset
      (write-analysis-vs-plot-script 
       (format nil "plot_testset")
       (format nil "~a Test Set" (car (dom-name *pspace*))) 
       (format nil "testset") 15)
      ))

 
