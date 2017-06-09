;; SAYPHI Planner 
;; Running Sets of Problems for Experiments
;; Td. 16.12.2006
;; _________________________________________________________________________________________

(defvar *learn-relational-p* nil)


(defmacro conc-outfile (command ext)
  `(concatenate 'string *this-date* *this-domain* "_" ,command "-" 
		(format nil "~a" *this-timeout*)  "." ,ext))


(defmacro this-studyresult (test)
  `(gethash (eval (car ,test)) *experiment-study*))


(defmacro this-expresult (test)
  `(gethash (eval (car ,test)) *experiment-results*))


(defun list-solution (count-prob file-name solution)
  (list count-prob file-name (solution-found solution) (solution-total-time solution)
	(solution-depth solution) (solution-num-nodes solution) (solution-evaluated-nodes solution)
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





(defun get-data-filename (data-type run-type options)
  (declare (ignore run-type))
  (cond ((eq data-type 'problem-depth)
	 (format nil "~a/result/prob_~a-~a_data.txt" *domain-dir* (get-problem-filename) options))
	((eq data-type 'diffset-depth)
	 (format nil "~a/result/diffset_~a-~a_data.txt" *domain-dir* (get-problem-filename) options))))


      
(defun patch-empty-plot (domain)
  (let ((ruta (format nil "/home/tomas/diskd/hilbert_result/20070306/~a/result/*.*" domain))
	(empty-file nil))
    (dolist (i-file (directory ruta))
      (with-open-file (in-stream i-file :direction :input)
	(if (= 0 (file-length in-stream ))
	    (setf empty-file t)
	    (setf empty-file nil)))
      
      (when empty-file
	(format t "~% >> Patching ~a" i-file)
	(with-open-file (out-stream i-file :direction :output :if-exists 
				    :supersede :if-does-not-exist :create)
	  (format out-stream "-1.0 ~t-1.0"))
	))))



;; I'm trying to free some memory, so the garbage collector can claim it more
;; easily
(defun say-mem-free-pointers()
  (setf *h-achieved-goals* nil)
  (setf *say-hash-solutions* nil)
  (setf *say-solution* nil)
  (setf *hash-nodes* nil)
  (setf *hash-nodes* (make-hash-table))
  (setf *hash-duplicates* nil)
  (setf *hash-inconsistent-h* nil)
  (setf *current-problem* nil)
  
  (setf *actions* nil)
  (setf *factgraph-table* nil)
  (setf *fluentgraph-table* nil)

  (setf *facts* nil)
  (setf *init-facts* nil)
  (setf *fluents* nil)
  (setf *init-fluents* nil)

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
)
    
  

;; Funcion para correr juntos un conjunto de problemas y guardar los resultados
(defun runset (domain domain-file &key (algorithm nil)
		      (heuristic nil)
	              (runtype 'ehc) 
		      (cost *say-costfn*) 
	              (max-solutions most-positive-fixnum)
     	              (helpful *say-helpful*)
	              (w_g nil)
	              (w_h nil)
	              (special-prune nil)
	              (timeout 5) (output-level 1)
		      (depthbound 3000) (metric nil)
		      (probs-prefix "p") (save-result t)
		      (out-file nil) (try2-search t)
	              (analyze-tree nil)
		      (experiment-var nil)
	              (say-learning-p nil)
		      (rules-file nil)
		      (use-rules-p nil)
		      (init-learning-p nil)
		      (search-options nil)
	       )
  (declare (ignore metric))
  (setf *say-output* output-level)
  (say-domain domain domain-file)
  (setq domain (car (dom-name *pspace*)))
  (let* ((probdir (format nil "~aprobsets/" *domain-dir*))
	 (count-prob 1) (this-sol nil) 
	 (runprob-tag (format nil "~a-~a-~a-~d.txt"
				  probs-prefix algorithm heuristic timeout))
	 (result-file (init-results-files out-file "" probs-prefix (cons `(:timeout ,timeout) search-options)))
;; 	 (result-file (if (not (stringp out-file))
;; 			  (format nil "~a/result/~aset-~a-~a-~d.csv"
;; 				  *domain-dir* probs-prefix algorithm heuristic timeout)
;; 			(format nil "~a/result/~a"
;; 				  *domain-dir* out-file)))
	 (wild-char #+SBCL "*.*"
		    #-SBCL "*"))
    (dolist (thisprob (dirfiles-sorted probdir (concatenate 'string probs-prefix wild-char)))
      (format t "~% Solving Problem ~a" (pathname-name thisprob))
      (read-pddl-problem thisprob)
      (setf *complete-problem-file* (concatenate 'string (pathname-name thisprob) "." (pathname-type thisprob)))
      (setf *problem-file* (pathname-name thisprob))
      (setf *problem-dir* "probsets/")
      (setf this-sol (plan :algorithm algorithm :try2-search try2-search
			   :heuristic heuristic :cost cost :runtype runtype
			   :timeout timeout :depthbound depthbound
			   :helpful helpful :w_g w_g :w_h w_h
			   :special-prune special-prune :max-solutions max-solutions
			   :rules-file rules-file :use-rules-p use-rules-p
			   ))
      (solution-print this-sol t nil)
      (when save-result
	(with-open-file (out-stream result-file :direction :output :if-exists 
				    :append :if-does-not-exist :create)
	        (if (eq *say-solution-format* :list)
		    (format out-stream "(~d ~a" count-prob (pathname-name thisprob))
		    (format out-stream "~d, ~a," count-prob (pathname-name thisprob)))
		(format out-stream "~a~%" (format-solution this-sol))))
      (unless (null experiment-var)
	(push (list-solution count-prob (pathname-name thisprob) this-sol) 
	      (gethash experiment-var *experiment-results*)))

      (when say-learning-p
	(when *generate-learning-examples-p* (generate-rules-from-solution rules-file))
;; 	(when *learn-relational-p* (write-training))
;; 	Put here your switch variable for learning and the function call
;; 	i.e. (when *learn-relational-p* (write-training)) 
	)
      
      (when analyze-tree
	(let ((ehc-tree (ehc-searchtree-analysis)))
	  (study-data-analyze-depth ehc-tree 'diffset runtype (get-problem-subset))
	  (study-data-analyze-depth ehc-tree 'testset runtype)
	  (unless (null experiment-var)
	    (push (list-treestudy count-prob (pathname-name thisprob) ehc-tree)
		  (gethash experiment-var *experiment-study*)))))

      (if (eq *say-solution-format* :list)
	  (with-open-file (out-stream result-file :direction :output :if-exists :append :if-does-not-exist :create)
	    (format out-stream ")")))
      (incf count-prob)
      (say-mem-free-pointers)
       #+SBCL (sb-ext:gc :full t)
       
;;        (room nil)
       
    )
      (if (eq *say-solution-format* :list)
	  (with-open-file (out-stream result-file :direction :output :if-exists :append :if-does-not-exist :create)
	    (format out-stream ")")))
))




(defun exp-sort-results()
  (maphash #'(lambda (test results)
	       (setf (gethash test *experiment-results*)
		     (reverse results)))
	   *experiment-results*)
  (maphash #'(lambda (test studies)
	       (setf (gethash test *experiment-study*)
		     (reverse studies)))
	   *experiment-study*)
)



(defun num-problems-solved (out-file)
  (let ((file-path (format nil "~a/result/~a" *domain-dir* out-file)))
    (with-open-file (out-stream file-path :direction :output :if-exists 
				:append :if-does-not-exist :create)
      (format out-stream "SAYPHI EXPERIMENT - NUMBER OF PROBLEMS SOLVERD ~%")
      (dolist (test *experiment-tests*)
	(let ((test-name (eval (car test))))
	  (format out-stream "~%~a:~a" test-name
		  (count t (gethash test-name *experiment-results*) :key #'third)))))))

   

(defun exp-same-solved (pos-prob)
  (let ((same-solved t))
    (maphash #'(lambda (test results)
		 (declare (ignore test))
		 (when (null (third (nth pos-prob results)))
		   (setf same-solved nil)))
	     *experiment-results*)
    same-solved
))
		   
(defun create-exp-accumulators(fields-list)
  (let ((accumulators (make-hash-table :test #'eq)))
    (dolist (test *experiment-tests* accumulators)
      (setf (gethash (eval (car test)) accumulators)
	    (make-array (1+ (apply #'max fields-list)) :initial-element 0)))))




(defun data-toplot (out-file &key (fields '(3 4 6 7))
			         (acc-fields '(3)))
  (let ((file-path (format nil "~a/result/~a" *domain-dir* out-file))
	(acc-table (create-exp-accumulators acc-fields))
	(test-primary (this-expresult (car *experiment-tests*)))
	(acc-array nil))
    (with-open-file (out-stream file-path :direction :output :if-exists 
			      :supersede :if-does-not-exist :create)
       (dotimes (i (length test-primary))
	 (when (exp-same-solved i)
	   (format out-stream "~a ~t ~a ~t" (car (nth i test-primary)) (second (nth i test-primary)))
	   (dolist (test *experiment-tests*)
	     (setf acc-array (gethash (eval (car test)) acc-table))
	     (dolist (i-field fields)
	       (format out-stream "~a ~t" 
		       (format-plotfield (nth i-field (nth i (this-expresult test))))))
	     (dolist (i-field acc-fields)
	       (setf (aref acc-array i-field)
		     (+ (aref acc-array i-field) (nth i-field (nth i (this-expresult test)))))
	       (format out-stream "~a ~t" 
		       (format-plotfield (aref acc-array i-field)))))
	   (format out-stream "~%"))))))
                  

			    
	 
(defun cleanup-experiment-vars ()
  (setf *trace-mem* nil)
  (setf *experiment-train* nil)
  (setf *experiment-tests* nil)
  (setf *experiment-outputs* nil)
)

(defun experiment (domain dom-file &key (exp-file "experiments.lisp"))
  (cleanup-experiment-vars)
  (setf *say-output* 1)
  (setf *experiment-results* (make-hash-table))
  (setf *experiment-study* (make-hash-table))
  (say-domain domain dom-file)
  (load (format nil "~a/~a" *domain-dir* exp-file))

  (dolist (train-cmd *experiment-train*)
    (format t "~% SAYPHI Training... ~a~%" train-cmd)
    (eval train-cmd))
  
  (dolist (test *experiment-tests*)
    (let* ((command (append (second test) (list :experiment-var (car test)))))
      (format t "~% SAYPHI Running... ~a~%" command)
      (eval command)))
  (exp-sort-results)

  (dolist (output-cmd *experiment-outputs*)
    (format t "~% SAYPHI Generating Output... ~a~%" output-cmd)
    (eval output-cmd)))
