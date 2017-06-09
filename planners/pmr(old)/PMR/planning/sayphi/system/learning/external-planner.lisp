(in-package "COMMON-LISP-USER")

;; Scripts for running external planners
(defvar *lama-unit-cost-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-unit-cost.sh ")
  "We can configure how to run lama-first. I am now calling a simplified Lama that only finds the first solution using unit cost")
(defvar *lama-first-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-first.sh ")
  "We can configure how to run lama-first. I am now calling a simplified Lama that only finds the first solution using cost")
(defvar *lama-seq-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-seq.sh ")
  "We can configure how to run sequential lama.")
(defvar *lama-second-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-second.sh ")
  "We can configure how to run LAMA starting from the 2nd iteration. It assumes someone has already found a solution, so
we provide an upper bound on cost")
(defvar *lama-opt-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-opt.sh ")
  "We can configure how to run optimal lama (FDSS-1).")
(defvar *ma-fd-script* (concatenate 'string "cd " *my-planning-path* "ma-fd/src; ./run-ma-fd.sh ")
  "We can configure how to run MA-FD.")
(defvar *cgamer-script* (concatenate 'string "cd " *my-planning-path* "cgamer; ./run-cgamer.sh ")
  "We can configure how to run CGamer.")
(defvar *metric-ff-script* (concatenate 'string "cd " *my-planning-path* "Metric-FF; ./run-ff.sh ")
  "We can configure how to run Metric-FF. I am currently calling a EHC version")
(defvar *cbp-script* (concatenate 'string "cd " *my-planning-path* "Roller; ./run-cbp-estocastico.sh ")
  "We can configure how to run CBP. I am currently calling an stochastic version")
(defvar *lpg-adapt-script* (concatenate 'string *my-planning-path* "LPG-adapt/run-lpg-adapt.sh ")
  "We can configure how to run LPG-Adapt")
;; (defvar *sols* nil "Auxiliary")

(defvar *domains-dir* (concatenate 'string *my-planning-path* "sayphi/domains/") "In case, we did not load Sayphi before")
;; (defvar *domain-dir* (concatenate 'string *domains-dir* "blocksworld/") "In case, we did not load Sayphi before")
(defvar *max-time* 0 "When using MA-FD, it has the makespan of planning time among all agents.")

;; I can call now Lama-first, Lama-second, Lama-seq, Lama-opt, cgamer, Metric-FF, ERRTPlan, and Sayphi
;; max-cost for anytime algorithms
(defun the-ring (algorithm timeout domain domain-file problem-file
		 &key (previous-solution nil) (probsets-dir (concatenate 'string *domains-dir* domain "/probsets/"))
		   (domain-dir (format nil "~a~a/" *domains-dir* domain)) max-cost search-options)
  (if *trace-ma-sayphi* (format t "~%Executing problem ~a with ~a..." problem-file algorithm))
  (case algorithm
    ((lama-first lama-unit-cost lama-seq lama-second lama-opt cgamer metric-ff ma-fd cbp-estocastico)
     (if (eq algorithm 'ma-fd) (prepare-for-ma-fd domain))
     (let ((solution-list (execute-planner :planner algorithm :domain domain :domain-file domain-file
					   :problem-file problem-file :result-in-file-p nil
					   :timeout timeout :probsets-dir probsets-dir :domain-directory domain-dir
					   :max-cost (or max-cost most-positive-fixnum))))
       (if *trace-ma-sayphi* (format t "result: ~a" (if (nth 5 solution-list) 'solved 'unsolved)))
       (make-solution :found (nth 5 solution-list)
		      :total-time (nth 0 solution-list)
		      :length (nth 1 solution-list)
		      :path (nth 4 solution-list)
		      :num-nodes (nth 3 solution-list)
		      :total-cost (nth 2 solution-list))))
    (lpg-adapt
     (let ((solution-list (execute-planner :planner algorithm :domain domain :domain-file domain-file
					   :problem-file problem-file :result-in-file-p nil
					   :previous-solution previous-solution :timeout timeout
					   :probsets-dir probsets-dir :domain-directory domain-dir)))
       (if *trace-ma-sayphi* (format t "result: ~a" (if (nth 4 solution-list) 'solved 'unsolved)))
       ;; we could also report on similarity
       (make-solution :found (and (nth 4 solution-list) t)
		      :total-time (nth 0 solution-list)
		      :length (nth 1 solution-list)
		      :path (nth 4 solution-list)
		      ;; this is different from the contents of the results file where the nth 3 is the similarity
		      :num-nodes (nth 3 solution-list)
		      :total-cost (nth 2 solution-list))))
    ;; not ready yet to use probsets-dir
    (errtplan (errt-planning domain-file problem-file previous-solution :domain domain :stochastic-p nil
			     :say-timeout timeout :load-domain-p nil))
    (otherwise (plan :algorithm algorithm :timeout timeout :search-options search-options))))

(defun prepare-for-ma-fd (domain)
  (let* ((agents (if (boundp '*agents*)
		     (mapcar #'(lambda (agent) (if (agent-p agent) (agent-name agent) agent)) *agents*)))
	 (num-agents (length agents))
	 (command1 (concatenate 'string "chmod a+x " *my-planning-path* "ma-fd/src/run-ma-fd.sh"))
	 (command2 (concatenate 'string "chmod a+x " *my-planning-path* "ma-fd/src/parse-ma-fd-output.sh"))
	 (output-file (concatenate 'string *domains-dir* domain "/result/output-MA-FD"))
	 (aux-output-file (concatenate 'string output-file ".lisp"))
	 (init-tcpip (+ (random 20) 3010)))
    (when agents
      (with-open-file (ofile (concatenate 'string *my-planning-path* "ma-fd/src/run-ma-fd.sh")
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format ofile "#! /bin/bash")
	(format ofile "~%cd ~ama-fd/src/" *my-planning-path*)
	(format ofile "~%export PATH=/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/etc:/usr/sbin:/sbin:./:/usr/local/bin")
	(format ofile "~%\rm downward.tmp.* output output.sas plan_numbers_and_cost elapsed.time lama-output*")
	(format ofile "~%./translate/translate.py $1 $2")
	(format ofile "~%./preprocess/preprocess < output.sas")
	(dotimes (i num-agents)
	  (if (zerop i)
	      (format ofile "~%./search/downward --search \"eager_greedy([ff(),cea()])\" --agents ~d < output > ~a~d"
		      i output-file i)
	      (format ofile " &~%./search/downward --search \"eager_greedy([ff(),cea()])\" --agents ~d < output > ~a~d"
		      i output-file i)))
	(terpri ofile))
      (execute-shell-command command1)
      (with-open-file (ofile (concatenate 'string *my-planning-path* "ma-fd/src/agents")
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format ofile "~d" num-agents)
	(dolist (agent agents)
	  (format ofile "~%~(~a~)" agent))
	(terpri ofile))
      (with-open-file (ofile (concatenate 'string *my-planning-path* "ma-fd/src/comm")
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(dotimes (i num-agents)
	  (format ofile "127.0.0.1:~d~%" (+ i init-tcpip))))
      (with-open-file (ofile (concatenate 'string *my-planning-path* "ma-fd/src/parse-ma-fd-output.sh")
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format ofile "#! /bin/bash~%")
	(format ofile "~%\rm ~a~%" aux-output-file)
	(dotimes (i num-agents)
	  (format ofile "cat ~a~d | grep \"Solution cost is: \\|Generated [0-9]* state\\|Total time:\\|^(\" | sed -e  \"s/step(s).//\" | sed -e \"s/state(s).//\" | sed -e \"s/SOLVED!!     //\" | sed -e \"s/Solution cost is: /co /\" |  sed -e \"s/Generated /no /\" | sed -e \"s/state(s)./ /\" | sed -e \"s/Total time: /ti /\" | sed -e \"s/s//\" >> ~a~%"
		  output-file i aux-output-file)))
      (execute-shell-command command2))))
	
;; planner: lama-first, lama-second, lama-seq, lama-opt, cgamer, metric-ff
;; output-file: auxiliary file
;; results-file: accumulated info
;; example: (execute-probset-planner :planner 'lama-first :domain "rover" :domain-file "StripsRover.pddl" :problem-prefix "pfile*")
(defun execute-probset-planner (&key (planner 'lama-unit-cost) (domain "blocksworld") (domain-file "ipc-domain.pddl")
				  (problem-prefix "prob*")
				  (previous-solution nil) (timeout 300) (max-cost most-positive-fixnum)
				  (domain-directory (concatenate 'string *domains-dir* domain "/"))
				  (output-directory (concatenate 'string domain-directory "result/"))
				  (probsets-dir (concatenate 'string domain-directory "probsets/"))
				  (plan-file (concatenate 'string output-directory (format nil "plan-~a.lisp" planner)))
				  (output-file (concatenate 'string output-directory (format nil "output-~a.lisp" planner)))
				  (results-file (concatenate 'string output-directory
							     (format nil "results-~a.lisp" planner))))
  (let ((problem-file-no-dir nil))
    (with-open-file (ostream results-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format ostream ";; ((Problem Time Length Cost Nodes Solution)*)~2%("))
    (if (not (member planner '(lama-first lama-unit-cost lama-second lama-seq lama-opt ma-fd cgamer metric-ff lpg-adapt cbp-estocastico)))
	(say-domain domain domain-file nil))
    (dolist (problem (directory (concatenate 'string probsets-dir problem-prefix)))
      (setq problem-file-no-dir (if (pathname-type problem)
				    (concatenate 'string (pathname-name problem) "." (pathname-type problem))
				    (concatenate 'string (pathname-name problem))))
      (case planner
	((lama-first lama-unit-cost lama-second lama-seq lama-opt ma-fd cgamer metric-ff lpg-adapt cbp-estocastico)
	 (execute-planner :planner planner :domain domain :domain-file domain-file
			  :problem-file problem-file-no-dir :probsets-dir probsets-dir
			  :previous-solution previous-solution :timeout timeout :max-cost max-cost
			  :domain-directory domain-directory :output-directory output-directory
			  :output-file output-file :plan-file plan-file :results-file results-file))
	;; not ready yet to use probsets-dir
	(errtplan (errt-planning domain-file problem-file-no-dir previous-solution :domain domain
				 :stochastic-p nil :say-timeout timeout :load-domain-p nil))
	(otherwise (plan :algorithm planner :timeout timeout))))
    (with-open-file (ostream results-file :direction :output :if-exists :append)
      (format ostream ")"))))

;; previous-solution: list or pathname in case of ERRTPLAN; pathname in case of LPG-Adapt
;; example: (execute-planner :planner 'metric-ff :domain "blocksworld" :domain-file "ipc-domain.pddl" :problem-file "probBLOCKS-10-2.pddl" :result-in-file-p nil)
(defun execute-planner (&key (planner 'lama-unit-cost) (domain "blocksworld") (domain-file "ipc-domain.pddl")
			  (problem-file "probBLOCKS-4-0.pddl")
			  (domain-directory (concatenate 'string *domains-dir* domain "/"))
			  (previous-solution nil) (timeout 300) (max-cost most-positive-fixnum)
			  (output-directory (concatenate 'string domain-directory "result/"))
			  (probsets-dir (concatenate 'string domain-directory "probsets/"))
			  (plan-file (concatenate 'string output-directory (format nil "plan-~a.lisp" planner)))
			  (output-file (concatenate 'string output-directory (format nil "output-~a.lisp" planner)))
			  (results-file (concatenate 'string output-directory (format nil "results-~a.lisp" planner)))
			  (result-in-file-p t))
  (if (eq planner 'errtplan)
      (if (not (listp previous-solution))
	  (setq previous-solution (solution-from-file previous-solution)))
      (when (listp previous-solution)
	(with-open-file (ostream (concatenate 'string probsets-dir "previous-solution.sol")
				 :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (say-pp-solution previous-solution nil ostream t))
	(setq previous-solution (concatenate 'string probsets-dir "previous-solution.sol"))))
  ;; to empty all previous solutions
  (with-open-file (ostream plan-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (terpri ostream))
  (with-open-file (ostream output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (terpri ostream))
  (let* ((end-plan-p nil)
	 (lamap (and (member planner '(lama-first lama-unit-cost lama-second lama-seq lama-opt ma-fd)) t))
	 ;; set ma-fd output
	 (aux-output-file (if lamap
			      (concatenate 'string output-directory (format nil "output-~a-aux.lisp" planner))))
	 ;; removes previous plans
	 (command (concatenate 'string "rm " plan-file " " plan-file ".*")))
    (execute-shell-command command)
    (inner-execute-planner planner domain-directory domain-file probsets-dir problem-file plan-file
			   (if lamap aux-output-file output-file) previous-solution timeout max-cost)
    (multiple-value-bind (length cost nodes time plan solvedp)
	(parse-output-files planner plan-file lamap aux-output-file output-file timeout)
      (cond (result-in-file-p
	     (with-open-file (ostream results-file :direction :output :if-exists :append :if-does-not-exist :create)
	       (format ostream "~%(~a" problem-file)
	       (format ostream " ~,3f ~d ~,2f ~d ~% (" time length cost nodes)
	       (if (and solvedp (not plan))
		   (format ostream "~%  T")
		   (dolist (action plan)
		     (cond ((and (listp action) (not end-plan-p))
			    (format ostream "~%  ~a" action))
			   (end-plan-p
			    (format ostream "~% ~a" action))
			   (t (setq end-plan-p t)
			      (format ostream ")~% ~a" action)))))
	       (format ostream "))~%"))
	     plan)
	    (t `(,time ,length ,cost ,nodes ,plan ,solvedp))))))

(defun inner-execute-planner (planner domain-directory domain-file probsets-dir problem-file plan-file output-file
			      previous-solution timeout max-cost)
  (let ((command (case planner
		   (lama-unit-cost (concatenate 'string *lama-unit-cost-script* domain-directory domain-file
						" " probsets-dir problem-file " " plan-file " " output-file))
		   (lama-first (concatenate 'string *lama-first-script* domain-directory domain-file
					    " " probsets-dir problem-file " " plan-file " " output-file))
		   ;; it seems the most-positive-fixnum of C++ is much less the one of Lisp!!
		   (lama-second (concatenate 'string *lama-second-script* domain-directory domain-file
					     " " probsets-dir problem-file " " plan-file " " output-file
					     (format nil " ~d" (min max-cost
								    (floor (/ most-positive-fixnum 10000000000.0))))))
		   (lama-seq (concatenate 'string *lama-seq-script* domain-directory domain-file
					  " " probsets-dir problem-file " " plan-file " " output-file))
		   (lama-opt (concatenate 'string *lama-opt-script* domain-directory domain-file
					  " " probsets-dir problem-file " " plan-file " " output-file))
		   (ma-fd (concatenate 'string *ma-fd-script* domain-directory domain-file
				       " " probsets-dir problem-file))
		   (cgamer (concatenate 'string *cgamer-script* domain-directory domain-file
					" " probsets-dir problem-file " " plan-file " " output-file))
		   (metric-ff (concatenate 'string *metric-ff-script* " " domain-directory " " domain-file
					   " " probsets-dir problem-file " " plan-file))
		   (cbp-estocastico (concatenate 'string *cbp-script* " " domain-directory " " domain-file
						 " " probsets-dir problem-file " " plan-file " 0.7 " (format nil "~,2f" timeout)))
		   (lpg-adapt (concatenate 'string *lpg-adapt-script* domain-directory domain-file
					   " " probsets-dir problem-file " " plan-file
					   " " previous-solution " " (format nil "~d" timeout)))
		   (t nil)))
	(script (concatenate 'string *my-tmp-path* "run-planner")))
    (if command
	(timed-execute-shell-command command :bash-file script :timeout timeout :time-units 'real-time)
	(format t "~%Unknown planner ~a" planner))))

;; (defun inner-execute-planner (planner domain-directory domain-file probsets-dir problem-file plan-file output-file previous-solution timeout)
;;   (let ((command (case planner
;; 		   (lama-first (concatenate 'string "ulimit -t " (format nil "~d; " timeout) *lama-first-script* domain-directory domain-file " " probsets-dir problem-file " " plan-file " " output-file))
;; 		   (metric-ff (concatenate 'string "ulimit -t " (format nil "~d~%" timeout) *metric-ff-script* domain-directory domain-file " " probsets-dir problem-file " " plan-file))
;; 		   (lpg-adapt (concatenate 'string *lpg-adapt-script* domain-directory domain-file " " probsets-dir problem-file " " plan-file
;; 					   " " previous-solution " " (format nil "~d" timeout)))
;; 		   (t nil)))
;; 	(script (concatenate 'string *my-tmp-path* "run-planner")))
;;     (if command
;; 	(execute-shell-command command script)
;; 	(format t "~%Unknown planner ~a" planner))))

;; 			      "; rm -i downward.tmp.* output output.sas"))
;;  			      " | grep \"Plan length: \\|Plan cost: \\|Generated [0-9]* state\\|Total time:\\|^(\""
;;   			      "| sed -e \"s/step(s).//\""
;;   			      "| sed -e \"s/state(s).//\""
;;  			      "| sed -e \"s/Plan length: / /\""
;;  			      "| sed -e \"s/Plan cost: / /\""
;;   			      "| sed -e \"s/Generated //\""
;;  			      "| sed -e \"s/state(s)./ /\""
;;  			      "| sed -e \"s/Total time: //\""
;;  			      "| sed -e \"s/s//\""
;;  			      "> " output-file))

;; It reads plan and output files and returns as values length, cost, nodes, time and plan
(defun parse-output-files (planner plan-file lamap aux-output-file output-file timeout)
  (let ((command nil) (plan nil) (solvedp nil) (input nil)
	(length most-positive-fixnum) (cost most-positive-fixnum) (nodes 0) (time timeout) (max-time 0))
    ;; Since it generates several plans, I take the last one
    (if (or (eq planner 'lama-seq) (eq planner 'lama-second))
	(setq plan-file (car (sort (directory (concatenate 'string plan-file ".*"))
				   #'(lambda (file1 file2) (string> (pathname-type file1) (pathname-type file2))))))
	(if (eq planner 'ma-fd) ;; MA-FD does not generate a plan
	    (setq plan-file nil)))
    (when lamap
      (setq command (if (eq planner 'ma-fd)
			(concatenate 'string "cd " *my-planning-path* "ma-fd/src; ./parse-ma-fd-output.sh")
			(concatenate 'string "cd " *my-planning-path*
				     (case planner
				       ((lama-first lama-unit-cost) "fd; ./parse-lama-first-output.sh ")
				       ((lama-second lama-seq) "fd; ./parse-lama-seq-output.sh ")
				       (lama-opt "fd; ./parse-lama-opt-output.sh ")
				       (ma-fd "ma-fd; ./parse-ma-fd-output.sh ")
				       (otherwise nil))
				     aux-output-file " " output-file)))
      (execute-shell-command command))
    ;; LAMA generates a plan file and an output-file, while the other two only an output file
    (if (and plan-file (probe-file plan-file))
	(with-open-file (istream plan-file :direction :input)
	  (case planner
	    ((lama-first lama-unit-cost lama-second lama-seq lama-opt cgamer)
	     (do* ((action (read istream nil 'eof) (read istream nil 'eof)))
		  ((eq action 'eof)
		   (setq plan (nreverse plan)))
	       (push action plan)))
	    ((metric-ff cbp-estocastico)
	     (setq input (read istream nil 'eof))
	     (when (and input (not (eq input 'eof)))
	       (setq plan (car input))
	       (setq length (length plan))
	       (setq cost (length plan)) ;; to be declared. I would have to run Metric-FF in -O mode
	       (setq nodes (cadr input))
	       (setq time (caddr input))))
	    (lpg-adapt (setq input (peek-char nil istream))
		       (unless (eq input #\))
			 (setq input (read istream nil 'eof))
			 (when (and input (not (eq input 'eof)))
			   (setq plan (if (listp (nth 3 input)) (nth 3 input) (nth 4 input)))
			   (setq length (length plan))
			   (setq cost (length plan))
			   (setq nodes 0) ;; does not print it
			   (setq time (nth 1 input)))))
	    (t nil))
	  (if plan (setq solvedp t))))
    (if (probe-file output-file)
	(case planner
	  ((lama-first lama-unit-cost cgamer)
	   (with-open-file (istream output-file :direction :input)
	     (setq solvedp (or (read istream nil nil) solvedp))
	     (setq length (read istream nil '0))
	     (setq cost (read istream nil '0))
	     (setq nodes (read istream nil '0))
	     (setq time (read istream nil '0))))
	  ((lama-seq lama-second)
	   (with-open-file (istream output-file :direction :input)
	     (do ((metric (read istream nil :eof) (read istream nil :eof))
		  (read nil)
		  (new-solution-p nil))
		 ((eq metric :eof))
	       (setq read (read istream nil :eof))
	       (case metric
		 (length (if (numberp read)
			     (when (<= read length)
			       (setq new-solution-p t)
			       (setq length read))
			     0))
		 (cost (if (numberp read)
			   (when (<= read cost)
			     (setq new-solution-p t)
			     (setq cost read))
			   0))
		 (nodes (if (numberp read)
			    (if new-solution-p
				(incf nodes read)
				(setq nodes read))
			    0))
		 (time (if (numberp read)
			   (setq time read)
			   0))
		 (t nil)))))
	  (ma-fd
	   (if (boundp '*agents*)
	       (with-open-file (istream output-file :direction :input)
		 (setq time 0.0)
		 (do ((the-read (read istream nil 'eof)
				(if (not (eq the-read 'eof)) (read istream nil 'eof))))
		     ((eq the-read 'eof))
		   (case the-read
		     (co (setq the-read (read istream nil 'eof))
			 (if (numberp the-read) (setq cost the-read)))
		     (no (setq the-read (read istream nil 'eof))
			 (if (numberp the-read) (incf nodes the-read)))
		     (ti (setq the-read (read istream nil 'eof))
			 (when (numberp the-read)
			   (incf time the-read)
			   ;; I report the time, but I set the max-time, so that anyone can access it to report it
			   (setq max-time (max max-time the-read))))
		     (otherwise nil)))
		 ;; MA-FD does not report the plan, nor its length
		 (setq length cost)
		 (setq *max-time* max-time)
		 ;; plan=nil means no plan
		 (if (< cost most-positive-fixnum) (setq plan '(t))))))
	  (lama-opt)
	  (otherwise nil)))
    ;; I am not sure when it happens
    (if (> time timeout)
	(setq time timeout))
    (values length cost nodes time plan solvedp)))

;; to convert a file generated by the above call, like ((prob1 time1 length1 cost1 nodes1 solution1) ... (probn timen lengthn costn nodesn solutionn))
;; into a plot file
;; (write-data-file "/home/messier/planning/sayphi/domains/test-grid/result/summary-results-planner.lisp" "/home/messier/planning/sayphi/domains/test-grid/result/results-planner.lisp")
(defun write-data-file (data-file summary-file)
  (let ((the-sols nil))
    (with-open-file (ifile summary-file :direction :input)
      (setf the-sols (read ifile)))
    (with-open-file (ofile data-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format ofile "# 0 Prob, 1 Solved, 2 Time, 3 Length, 4 Nodes, 5 Evals, 6 Cost, 7 ReuseClosed, 8 Reuse, 9 CurrentSearch, 10 NumberBetterLength, 11 NumberBetterCost, 12 TotalTimeHeuristic, 13 Unstability (12 AvgTimeHeuristic 13 TotalTimeHeuristic 14 Unstability in PLANNER)~%")
      (do* ((sols the-sols (cdr sols))
	    (sol (car sols) (car sols))
	    (i 0 (1+ i)))
	   ((null sols))
	(format ofile "~d 1.0 ~4$ ~d ~d 0 ~4$ 0 0 0 0 0 0.0 0~%" i (nth 1 sol) (nth 2 sol) (nth 4 sol) (nth 3 sol))))))

;; it takes a list or a structure of type solution and returns the plan in a list
(defun give-me-plan-list (solution &optional (even-if-not-found-p nil))
  (if (listp solution)
      solution
      (if (and solution (solution-p solution) (or even-if-not-found-p (solution-found solution)))
	  (if (snode-p (car (solution-path solution)))
	      (pp-solution-sayphi solution)
	      (solution-path solution)))))

