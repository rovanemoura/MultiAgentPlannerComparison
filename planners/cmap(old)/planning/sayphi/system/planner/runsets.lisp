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
;; Description: Running sets of problems. Function for SAYPHI Experimenter
;; Date: 2006.12.16
;; 
;; ========================================================================


(defvar *learn-relational-p* nil)
(defvar *learn-helpful-tilde* nil)
(defvar *learn-lookahead-tilde* nil)
(defvar *learn-ltl-icl* nil)
(defvar *generate-learning-examples-p* nil) ;; I redefined it here, if I don't want to load ebl.lisp


;; Parameters for the experiments
(defparameter *complete-problem-file* nil)
(defparameter *this-domain-code* nil)
(defparameter *train-probs-prefix* nil)
(defparameter *train-timeout* nil)
(defparameter *test-probs-prefix* nil)
(defparameter *this-exp-tag* nil)
;; =================================


(defun format-probs-prefix (prob-prefix)
  (remove #\/ prob-prefix))

(defmacro conc-outfile (command ext)
  `(concatenate 'string *this-date* *this-domain* "_" ,command "-" 
		(format nil "~a" *this-timeout*)  "." ,ext))

(defun conc-outfile-tag (command &optional (ext ".txt"))
  (concatenate 'string *this-exp-tag* "_" command ext))


(defmacro this-studyresult (test)
  `(gethash (eval (car ,test)) *experiment-study*))


(defmacro this-expresult (test)
  `(gethash (eval (car ,test)) *experiment-results*))


(defun list-solution (count-prob file-name solution)
  (list count-prob file-name (solution-found solution) (solution-total-time solution)
	(solution-length solution) (solution-num-nodes solution) (solution-evaluated-nodes solution)
	(solution-total-cost solution) (solution-stop-reason solution)))


;; (defun sol-trace(trace-info)
;;   (case trace-info
;; 	(:first-hit (count 1 *hit-node*))
;; 	(:average-hit (float (/ (apply #'+ *hit-node*)  (length *hit-node*))))
;; 	(:guided-nodes *cbp-guided-nodes*)
;; 	(:cbp-first-hit (count 1 *cbp-hit-node*))
;; 	(:cbp-average-hit 
;; 	 (if (> (length *cbp-hit-node*) 0)
;; 	     (float (/ (apply #'+ *cbp-hit-node*)  (length *cbp-hit-node*)))
;; 	     -1))
;; ))


(defun format-solution (solution)
  (case *say-solution-format*
	(:values
	 (format nil "~t~a ~t~4$ ~t~a ~t~a ~t~a ~t~a ~t~a"
		 (solution-found solution) (solution-total-time solution)
                 (solution-length solution) (solution-num-nodes solution) (solution-evaluated-nodes solution)
		 (solution-total-cost solution) (solution-stop-reason solution)))

	(:tags-values
	 (format nil "Found:~a Time:~a Nodes:~a Depth:~a Stop:~a"
		 (solution-found solution) (solution-total-time solution)
		 (solution-num-nodes solution) (solution-length solution)
		 (solution-stop-reason solution)))))



(defun get-data-filename (data-type run-type options)
  (declare (ignore run-type))
  (cond ((eq data-type 'problem-depth)
	 (format nil "~a/result/prob_~a-~a_data.txt" *domain-dir* (get-problem-filename) options))
	((eq data-type 'diffset-depth)
	 (format nil "~a/result/diffset_~a-~a_data.txt" *domain-dir* (get-problem-filename) options))))


      
;; (defun patch-empty-plot (domain)
;;   (let ((ruta (format nil "/home/tomas/diskd/hilbert_result/20070306/~a/result/*.*" domain))
;; 	(empty-file nil))
;;     (dolist (i-file (directory ruta))
;;       (with-open-file (in-stream i-file :direction :input)
;; 	(if (= 0 (file-length in-stream ))
;; 	    (setf empty-file t)
;; 	    (setf empty-file nil)))
;;       
;;       (when empty-file
;; 	(format t "~% >> Patching ~a" i-file)
;; 	(with-open-file (out-stream i-file :direction :output :if-exists 
;; 				    :supersede :if-does-not-exist :create)
;; 	  (format out-stream "-1.0 ~t-1.0"))
;; 	))))


;; Redefine this function in the learner file. 
;; It is called after solving each problem of the runset
(defun execute-learner ()	
  (cond (*learn-helpful-tilde* (execute-learner-roller))
	(*learn-ltl-icl* (execute-learner-ltl))
	(*learn-relational-p* (write-training))
	(*learn-lookahead-tilde* (execute-learner-flare))
	
))


;; I'm trying to free some memory, so the garbage collector can claim it more
;; easily
(defun say-mem-free-pointers()
  (setf *h-achieved-goals* nil)
  (setf *say-hash-solutions* nil)
  (setf *say-solution* nil)
  (setf *partial-solution* nil)
  (setf *hash-nodes* nil)
  (setf *hash-nodes* (make-hash-table))
  (setf *hash-duplicates* nil)
  (setf *hash-inconsistent-h* nil)
  
  (setf *h-active-facts* nil)
  (setf *h-active-flpatterns* nil)
  (setf *h-active-fluents* nil)
  (setf *h-achieved-facts* nil)
  (setf *h-achieved-fluents* nil)
  (setf *h-active-actions* nil)
  (setf *actions-table* nil)
  (setf *action-layers* nil)
  (setf *achieved-layers* nil)
  (setf *h-achieved-layers* nil)
  (setf *focus-goals* nil)
  
  (setf *actions* nil)
  (setf *factgraph-table* nil)
  (setf *fluentgraph-table* nil)

  (setf *facts* nil)
  (setf *init-facts* nil)
  (setf *fluents* nil)
  (setf *init-fluents* nil)
  (if (and *current-problem* (problem-search-tree *current-problem*))
      (setf (problem-search-tree *current-problem*) nil))
  (setf *current-problem* nil)

)
    
  

;; Allows running several problems (probs-prefix) and store results
(defun runset (domain domain-file &key (algorithm nil)
		      (heuristic nil)
		      (cost *say-costfn*) 
	              (max-solutions most-positive-fixnum)
     	              (helpful *say-helpful*)
	              (w_g nil)
	              (w_h nil)
	              (timeout 5) (output-level 1)
		      (depthbound *say-depthbound*) (metric nil)
		      (probs-prefix "p") (save-result t)
		      (out-file nil) (try2-search t)
		      (experiment-var nil)
	              (lookahead nil)
	              (k-beam nil)
	              (say-learning-p nil)
		      (rules-file nil)
		      (use-rules-p nil)
                      (internal-reload nil) (internal-save nil)
;; 		      (init-learning-p nil)
	              (search-options nil))
  (declare (ignore metric))
  (setf *say-output* output-level)
  (say-domain domain domain-file)
  (let* ((probdir (format nil "~aprobsets/" *domain-dir*))
	 (count-prob 1) (this-sol nil) 
	 (runprob-tag (format nil "~a-~a-~a-~d.txt"
				  probs-prefix algorithm heuristic timeout))
	 (result-file (if (not (stringp out-file)) 
			  (format nil "~a/result/~a" *domain-dir* runprob-tag)
			(format nil "~a/result/~a" *domain-dir* out-file)))
	 (wild-char #+SBCL "*.*"
		    #-SBCL "*")
	 (results-reloaded nil))

    (when internal-reload 
      (let ((recorded-result (format nil "~a/result/internal/~a" *domain-dir* runprob-tag)))
	(when (probe-file recorded-result)
	  (say-retrieve-results recorded-result experiment-var)
	  #+SBCL  (run-program (format nil "~ascripts/copy-file.sh" *system-dir*)
			       (list recorded-result result-file))
	  (format t "~%~% EXPERIMENT SET RELOADED ~% ~a" recorded-result)
	  (setf results-reloaded t))))
      
    (when (not results-reloaded) 
      (dolist (thisprob (dirfiles-sorted probdir (concatenate 'string probs-prefix wild-char)))
	(format t "~% Solving Problem ~a" (pathname-name thisprob))
	(read-pddl-problem thisprob)
	(setf *complete-problem-file* (concatenate 'string (pathname-name thisprob) "." (pathname-type thisprob)))
	(setf *problem-file* (pathname-name thisprob))
	(setf (problem-file *current-problem*) *problem-file*)
	(setf *problem-dir* "probsets/")
	(setf this-sol (plan :algorithm algorithm :try2-search try2-search
			     :heuristic heuristic :cost cost 
			     :timeout timeout :depthbound depthbound
			     :helpful helpful :w_g w_g :w_h w_h
			     :max-solutions max-solutions
			     :lookahead lookahead :k-beam k-beam
			     :rules-file rules-file :use-rules-p use-rules-p
			     :search-options search-options
			     ))
	
	(when save-result
	  (with-open-file (out-stream result-file :direction :output :if-exists 
				      :append :if-does-not-exist :create)
	    (format out-stream "~d ~a" count-prob (pathname-name thisprob))
	    (format out-stream "~a~%" (format-solution this-sol))))
	(unless (null experiment-var)
	  (push (list-solution count-prob (pathname-name thisprob) this-sol) 
		(gethash experiment-var *experiment-results*)))
	
	;;       The function execute-learner must be redefined in the learner file
	(when say-learning-p (execute-learner))
	(incf count-prob)
	(setf this-sol nil)
	(say-mem-free-pointers)
	#+SBCL (sb-ext:gc :full t)
	
        (room nil)
	)
      (when  (and internal-save (probe-file (format nil "~a/result/internal/" *domain-dir*)))
	#+SBCL  (run-program (format nil "~ascripts/copy-file.sh" *system-dir*)
			     (list result-file (format nil "~a/result/internal/~a" *domain-dir* runprob-tag))))
    )))



(defun exp-sort-results()
  (maphash #'(lambda (test results)
	       (setf (gethash test *experiment-results*)
		     (reverse results)))
	   *experiment-results*)
  (when (hash-table-p *experiment-study*)
    (maphash #'(lambda (test studies)
		 (setf (gethash test *experiment-study*)
		       (reverse studies)))
	     *experiment-study*)
    ))



(defun num-problems-solved (out-file)
  (let ((file-path (format nil "~a/result/~a" *domain-dir* out-file)))
    (with-open-file (out-stream file-path :direction :output :if-exists 
				:append :if-does-not-exist :create)
      (format out-stream "SAYPHI EXPERIMENT - NUMBER OF PROBLEMS SOLVED ~%")
      (format out-stream "====================================================")
      (format out-stream "~% Timeout :~a" *this-timeout*)
      (format out-stream "~% Solved by all tests:~a ~%" (exp-num-of-same-solved))
      (format out-stream "====================================================")
      (dolist (test *experiment-tests*)
	(let* ((test-name (eval (car test)))
	       (nsolved (count t (gethash test-name *experiment-results*) :key #'third))
	       (percent-solved (when (hkey-present test-name *experiment-results*)
				 (* 100 (/ nsolved (length (gethash test-name *experiment-results*)))))))
	  (format out-stream "~%~a: ~a     ~1$ %" test-name nsolved percent-solved
))))))

   

(defun exp-same-solved (pos-prob)
  (let ((same-solved t))
    (maphash #'(lambda (test results)
		 (declare (ignore test))
		 (when (null (third (nth pos-prob results)))
		   (setf same-solved nil)))
	     *experiment-results*)
    same-solved
))

(defun exp-num-of-same-solved ()
  (let ((num-of-same-solved 0))
    (dotimes (i-prob (length (this-expresult (car *experiment-tests*))) num-of-same-solved)
      (when (exp-same-solved i-prob)
	(incf num-of-same-solved)))))
  
		   
(defun create-exp-accumulators(fields-list)
  (let ((accumulators (make-hash-table :test #'eq)))
    (dolist (test *experiment-tests* accumulators)
      (setf (gethash (eval (car test)) accumulators)
	    (make-array (1+ (apply #'max fields-list)) :initial-element 0)))))


;; The data-type indicates the 'normal data or the 'accumulated data

(defun data-toplot (data-type &key (fields '(3 4 5 6 7))
		    (acc-fields '(3 4 5 6 7)))
  (let* ((accumulated-p (if (eq data-type 'normal) nil t))
	 (file-sufix (if accumulated-p "_res-accdata" "_res-data"))
	 (file-name (conc-outfile-tag file-sufix))
	 (file-path (format nil "~a/result/~a" *domain-dir* file-name))
	 (acc-table (create-exp-accumulators acc-fields))
	 (test-primary (this-expresult (car *experiment-tests*)))
	 (acc-array nil))
    (with-open-file (out-stream file-path :direction :output :if-exists 
			      :supersede :if-does-not-exist :create)
       (dotimes (i (length test-primary))
	 (when (or (not accumulated-p)
		   (exp-same-solved i))
	   (format out-stream "~a ~T ~a ~T" (car (nth i test-primary)) (second (nth i test-primary)))
	   (dolist (test *experiment-tests*)
	     (setf acc-array (gethash (eval (car test)) acc-table))
	     (unless accumulated-p
	       (dolist (i-field fields)
		 (format out-stream "~a ~T" 
			 (cond ((nth 2 (nth i (this-expresult test)))
			       (format-plotfield (nth i-field (nth i (this-expresult test)))))
			       (t -1)))))
	     (when accumulated-p
	       (dolist (i-field acc-fields)
		 (setf (aref acc-array i-field)
		       (+ (aref acc-array i-field) (nth i-field (nth i (this-expresult test)))))
		 (format out-stream "~a ~T" 
			 (format-plotfield (aref acc-array i-field))))))
	   (format out-stream "~%"))))))
                  
;; Field Parameters for Plot scripts
(defvar *result-fields-info*
  '((time (3 "Time" nil))
    (logtime (3 "Time" t))
    (length (4 "Plan Length") nil)
    (nodes (5 "Nodes") nil)
    (evaluated (6 "Evaluated Nodes") nil)
    (quality (7 "Quality") nil)))

  

(defun plot-scripts-all (&key (terminal "jpeg") (graphic-ext "jpeg"))
  (dolist (i-field-info *result-fields-info*)
    (plot-scripts-problem (car i-field-info) t :terminal terminal :graphic-ext graphic-ext
			  :logscale-p (third (cadr i-field-info)))
    (plot-scripts-problem (car i-field-info) nil :terminal terminal :graphic-ext graphic-ext
			  :logscale-p (third (cadr i-field-info)))))
			  

;; Before calling this function you should load first a experiment file
(defun plot-scripts-problem (plot-type accumulated &key 
			     (terminal "jpeg")
			     (graphic-ext "jpeg")
			     (logscale-p nil)
			     (y-title nil))

  (let* ((plot-field (second (cadr (assoc plot-type *result-fields-info*))))
	 (plot-column (car (cadr (assoc plot-type *result-fields-info*))))
	 (plot-name (if accumulated 
			(conc-outfile-tag (format nil "_plot-acc-~(~a~)" plot-type) "")
			(conc-outfile-tag (format nil "_plot-~(~a~)" plot-type) "")))
	 (plot-file (format nil "~aresult/~a.plt" *domain-dir* plot-name))
	 (ylabel (if (not (null y-title)) y-title (if accumulated (format nil "Accumulated ~a" plot-field)
						      (format nil "~a" plot-field))))
	 (top-xrange (length (this-expresult (car *experiment-tests*))))
	 (file-sufix (if accumulated "_res-accdata" "_res-data"))
	 (data-file (conc-outfile-tag file-sufix)))
    
    (with-open-file (out-stream plot-file :direction :output :if-exists 
				:supersede :if-does-not-exist :create)
      (format out-stream "set terminal ~a ~%" terminal)
      (format out-stream "set output '~a.~a'~%~%" plot-name graphic-ext)
      (format out-stream "set size square~%")
      (format out-stream "set grid~%~%")
      (format out-stream "set title '~a domain' ~%" *this-domain*)
      (format out-stream "set xlabel 'Problem No.'~%")
      (format out-stream "set ylabel '~a' ~%" ylabel)
      (format out-stream "set key left box ~%")

      (when logscale-p (format out-stream "set logscale y 10 ~%"))
      (format out-stream "set xrange [1:~a] ~%~%" top-xrange)
      
      (format out-stream "plot")
      (let ((first-line t) (num-exp 0) (this-column))
	(dolist (test *experiment-tests*)
	  (cond ((not first-line)
		 (format out-stream ",\\~%     "))
		(t (setf first-line nil)))
	  (setf this-column (+ plot-column (* 5 num-exp)))
	  (format out-stream "'~a' using 1:( $~d<0 ? 1/0 : $~d) title '~a' with linesp lw 3" 
		  data-file this-column this-column (eval (car test)))
	  (incf num-exp)
	  ))
      (format out-stream "~%"))
    #+SBCL (run-program (format nil "~ascripts/exec-gnuplot.sh" *system-dir*) 
			(list (format nil "~aresult/" *domain-dir*) plot-file))
    ))


;; This function create the score for the experiment based on the IPC Learning Track
;; Evaluation Schema
(defun score-experiment (field)
  (let ((experiment-score (make-hash-table :test #'eq)))
    (maphash #'(lambda (test results)
		 (setf (gethash test experiment-score)
		       (mapcar (lambda (i-result)
				 (let ((best-score (score-best-in-exp (car i-result) field)))
				   (cond ((null (third i-result)) 0)
					 ((= (nth field i-result) 0) 1)
					 (t
					  (float (/ best-score (nth field i-result)))))))
			     results)))
	     *experiment-results*)
    experiment-score))

				 


;; It returns the best time (3) or quality (7) obtained by any test
(defun score-best-in-exp (num-prob field)    
  (let ((res-list nil))
    (maphash #'(lambda (test results)
		 (declare (ignore test))
		 (let ((this-result (assoc num-prob results)))
		   (unless (null (third this-result))
		     (push (nth field this-result) res-list))))
	     *experiment-results*)
    (unless (null res-list)
      (apply #'min res-list))))



(defun score-write-field (out-stream field-hash-scores)
  (dolist (test *experiment-tests*)
    (let ((test-name (eval (car test))))
      (format out-stream "~%~a:" test-name)
      (format out-stream "~2$" (apply #'+ (gethash test-name field-hash-scores))))))



(defun score-write-results (&optional (result-file nil))
  (let ((file-path (or result-file 
		       (format nil "~a/result/~a" *domain-dir* (conc-outfile-tag "_scores"))))
	(score-time (score-experiment 3))
	(score-quality (score-experiment 4)))
    
    (with-open-file (out-stream file-path :direction :output :if-exists 
				:supersede :if-does-not-exist :create)
      (format out-stream "SAYPHI EXPERIMENT - SCORING EXPERIMENT RESULTS ~%")
      (format out-stream "IPC LEARNING-TRACK EVALUATION SCHEMA ~%")
      (format out-stream "==================================================== ~%")
      (format out-stream "Timeout :~a ~%~%" *this-timeout*)
      (format out-stream "TIME SCORES ~% ------------- ~%")
      (score-write-field out-stream score-time)
      (format out-stream "~%~%~%QUALITY SCORES ~% ------------- ~%")
      (score-write-field out-stream score-quality)

      (format out-stream "~%~%====================================================")
      (format out-stream "~%             DETAILED TIME SCORES")
      (format out-stream "~%~%====================================================~%")
      (format out-stream "NUM. PROBLEM")
      (dolist (i-test *experiment-tests*)
	(format out-stream "~a " (eval (car i-test))))

      (dotimes (i-prob (length (this-expresult (car *experiment-tests*))))
	(format out-stream "~%~a ~a  " (car (nth i-prob (this-expresult (car *experiment-tests*))))
		                     (second (nth i-prob (this-expresult (car *experiment-tests*)))))
	(dolist (i-test *experiment-tests*)
	  (format out-stream "~2$  " (nth i-prob (gethash (eval (car i-test)) score-time)))))

      (format out-stream "~%~%====================================================")
      (format out-stream "~%             DETAILED QUALITY SCORES")
      (format out-stream "~%~%====================================================~%")
      (dolist (i-test *experiment-tests*)
	(format out-stream "~a " (eval (car i-test))))
      
      (dotimes (i-prob (length (this-expresult (car *experiment-tests*))))
	(format out-stream "~%~a  ~a " (car (nth i-prob (this-expresult (car *experiment-tests*))))
		(second (nth i-prob (this-expresult (car *experiment-tests*)))))
	(dolist (i-test *experiment-tests*)
	  (format out-stream "~2$  " (nth i-prob (gethash (eval (car i-test)) score-quality)))))
      
      
)))


	 
(defun cleanup-experiment-vars ()
  (setf *trace-mem* nil)
  (setf *experiment-train* nil)
  (setf *experiment-tests* nil)
  (setf *experiment-outputs* nil)
)

;;The option full-external is for not loading domain file in sayphi
;;some domains could be loaded by other external planners 
(defun experiment (domain exp-file 
		   &key (train-timeout 60)
		   (test-timeout 300)
                   (full-external nil)
		   (exp-tag "exp"))
		   
  (cleanup-experiment-vars)
  (sld "experiment-def")
  (setf *say-output* 1)
  
  (setf *this-date* (my-now-tag))
  (setf *this-timeout* test-timeout)
  (setf *train-timeout* train-timeout)
  
  (setf *experiment-study* (make-hash-table))

  (load (format nil "~a~a/~a" *domains-dir* domain exp-file))

  (if full-external
      (setf *domain-dir* (concatenate 'string *domains-dir* domain "/"))
      (say-domain domain *this-domain-file*))

  (setf *this-exp-tag* (concatenate 'string *this-domain-code* "_" (my-now-tag) "_" exp-tag))
  
  (dolist (train-cmd *experiment-train*)
    (format t "~% SAYPHI Training... ~a~%" train-cmd)
    (eval train-cmd))


  (setf *experiment-results* (make-hash-table))
  (dolist (test *experiment-tests*)
    (let* ((command (append (second test) (list :experiment-var (car test)))))
      (format t "~% SAYPHI Running... ~a~%" command)
      (eval command))
    #+SBCL (sb-ext:gc :full t)
    )
  (exp-sort-results)

  (dolist (output-cmd *experiment-outputs*)
    (format t "~% SAYPHI Generating Output... ~a~%" output-cmd)
    (eval output-cmd))
  #+SBCL  (run-program (format nil "~ascripts/move-results.sh" *system-dir*)
		       (list (format nil "~aresult/" *domain-dir*) *this-exp-tag*))
  )


;; Retrieves the sayphi results from a file and stores them in the experiment-var
;; of the variable *experiment-results*
(defun say-retrieve-results (result-file experiment-var)
  (with-open-file (stream result-file :direction :input)
    (do ((line (read-line stream nil 'eof)
	       (read-line stream nil 'eof)))
	  ((eq line 'eof))
      (push (read-from-string (format nil "( ~a )" line))
	    (gethash experiment-var *experiment-results*)))))


;; It writes the data files to plot the runtime distribution of solved problems.
(defun distribution-results (field)
  (let ((field-position (case field
			  (runtime #'fourth)
			  (length #'fifth)
			  (evaluated #'seventh))
	  ))
    (maphash #'(lambda (key results)
		 (let* ((sorted-results (stable-sort (remove-if-not #'(lambda (prob-result)
									(third prob-result))
								    results) #'< :key field-position))
			(expvar-tag (format nil "_distrib-~(~a~)-~(~a~)" field key))
			(file-path (format nil "~a/result/~a" *domain-dir* (conc-outfile-tag expvar-tag))))
		   (with-open-file (ostream file-path :direction :output :if-exists 
					    :supersede :if-does-not-exist :create)
		   (let ((nsolved 0))
		     (dolist (i-result sorted-results)
		       (incf nsolved)
		       (dotimes (i 7) ;; Printing only the first seven fields 
			 (format ostream "~a " (nth i i-result)))
		       (format ostream "~4$~%" (* 100 (/ nsolved (length results)))))))))
	   *experiment-results*)))




(defparameter *gnuplot-linecolor-preference* '(1 3 2 4 5 6 7 8 9 10))

;; Before calling this function you should load first a experiment file
(defun plot-field-distribution (field &key (terminal "postscript eps color")
				  (graphic-ext "eps")
				  (logscale-p t)
				  (spanish-label nil)
				  (y-title nil))
  
  (let* ((plot-name (conc-outfile-tag (format nil "_distrib-~(~a~)-plot" field) ""))
	 (plot-file (format nil "~aresult/~a.plt" *domain-dir* plot-name))
	 (ylabel (if (not (null y-title)) y-title (if spanish-label "Porcentaje" "Percentage")))
	 (plot-field-vars (case field 
			    (runtime '("CPU Time" 4 "Tiempo CPU"))
			    (length '("Plan Length" 5 "Longitud del Plan"))
			    (evaluated '("Evaluated Nodes" 7 "Nodos Evaluados"))
			    ))
	 (data-files (mapcar #'(lambda (test) (cons test (conc-outfile-tag (format nil "_distrib-~(~a~)-~(~a~)" field test))))
			     (hashkeys-to-list *experiment-results*))))
    
    (with-open-file (out-stream plot-file :direction :output :if-exists 
				:supersede :if-does-not-exist :create)
      (format out-stream "set terminal ~a ~%" terminal)
      (format out-stream "set output '~a.~a'~%~%" plot-name graphic-ext)
      (format out-stream "set size square~%")
      (format out-stream "set grid~%~%")
      (format out-stream (if spanish-label "set title 'dominio ~a' ~%" 
			     "set title '~a domain' ~%") *this-domain*)
      (format out-stream "set xlabel '~a'~%" (if spanish-label (third plot-field-vars)
						 (car plot-field-vars)))
      (format out-stream "set ylabel '~a' ~%" ylabel)
      (format out-stream "set key left box ~%")

      (when logscale-p (format out-stream "set logscale x 10 ~%"))
      (format out-stream "set yrange [0:100] ~%~%")
      
      (format out-stream "plot ")
      (let ((num-line 1))
	(dolist (i-datafile data-files)
	  (when (> num-line 1)
	    (format out-stream ",\\~%     "))
	  (format out-stream "'~a' using ~d:8 title ' ~a' with linesp lw 2 lt ~d" 
		  (cdr i-datafile) (second plot-field-vars) (car i-datafile) 
		  (nth (1- num-line) *gnuplot-linecolor-preference*))
	  (when (< num-line (length *gnuplot-linecolor-preference*))
	    (incf num-line))
	  ))
	  
      (format out-stream "~%"))
    #+SBCL (run-program (format nil "~ascripts/exec-gnuplot.sh" *system-dir*) 
			(list (format nil "~aresult/" *domain-dir*) plot-file))
    (when (equal graphic-ext "eps")
      #+SBCL (run-program (format nil "~ascripts/crop-sayphi-eps.py" *system-dir*) 
			  (list (format nil "~aresult/~a.eps" *domain-dir* plot-name))))
    ))



