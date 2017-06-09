;; #+(or allegro sbcl clisp) (asdf:oos 'asdf:load-op "cl-ppcre")
(defvar *lpg-adapt-path* (concatenate 'string *my-planning-path* "LPG-adapt/"))
(defvar *lpg-adapt-domains* (concatenate 'string *lpg-adapt-path* "domains/"))
(defvar *lpg-adapt-problems* "probsets/")
(defvar *trace-pale* nil)

;; Remember to remove (:metric ....) from problems
;; num-sols is how many solutions we want from LPG-Adapt
;; solution is either nil (no previous solution) or a string with the name of a solution file that should be in the domain dir (e.g.  "solution-tlplan.sol")
;; if no solution given, it runs LPG
;; running-mode can be speed, quality or bestquality (incremental quality)
;; example: (run-lpg-adapt :domain "test-grid" :problem-prefix "pddl-test-cup*.pddl" :iterations 1 :domain-file "domain-key.pddl" :solution "plan.sol") assumes plan.sol is in the result dir of the domain
(defun run-lpg-adapt (&key (domain "driverlog") (domain-dir (concatenate 'string *lpg-adapt-domains* domain "/")) (domain-file "domain.pddl") (solution nil)
		      (problem-prefix  "pfile-*.pddl")
		      (num-sols 1) (running-mode 'speed)
		      (iterations 5) (time-bound 1800))
  (let ((command nil) (pfile nil) (plan-file nil))
    (format t "~% Solving problems in: ~a" (concatenate 'string domain-dir *lpg-adapt-problems* problem-prefix))
    (dolist (problem (directory (concatenate 'string domain-dir *lpg-adapt-problems* problem-prefix)))
      (dotimes (i iterations)
	(setq pfile (if (pathname-type problem)
			(concatenate 'string (pathname-name problem) "." (pathname-type problem))
			(concatenate 'string (pathname-name problem))))
	(setq plan-file (format nil "~aresult/plan_~a_~d.SOL" domain-dir pfile i))
	(setq command (format nil "cd; cd ~a; ./lpg-adapt ~(~a~) -p ~a -o ~a -f ~a~a ~a -n ~d -cputime ~d -adapt_all_diff ~a"
			      *lpg-adapt-path* running-mode domain-dir domain-file *lpg-adapt-problems* pfile (if solution (concatenate 'string  "-input_plan result/" solution) "")
			      num-sols time-bound (concatenate 'string "-out " plan-file)))
	;;  -optimize_plan_differences
	(format t "~%Solving: ~a~%" command)
	(execute-shell-command command)))))
;; 	(setq command (format nil "mv ~aplan_~a_1.SOL ~a" *lpg-adapt-path* pfile plan-file))
;; 	(if *trace-pale* (format t "~%Moving: ~a" command))
;; 	(execute-shell-command command)))))

;; 	(first-regexp (cl-ppcre:create-scanner "; Time \([.\\d]+\)" :case-insensitive-mode nil))
;; 	(second-regexp (cl-ppcre:create-scanner ":" :case-insensitive-mode t))
;; 	(third-regexp (cl-ppcre:create-scanner "; Distance from input plan: \([\\d]+\)" :case-insensitive-mode nil))
(defun analyze-results (&key (domain "driverlog")
			     (result-dir (concatenate 'string *lpg-adapt-domains* domain "/result/"))
			     (summary-file (concatenate 'string result-dir "lpg-adapt-output.lisp"))
			     (data-file (concatenate 'string result-dir "lpg-adapt-output.dat"))
			     (write-data-file-p t)
			     (iterations 5))
  (let ((command nil)
	(pfile nil))
    (with-open-file (ofile summary-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format ofile ";; ((Problem Time Length Difference Solution)*)~2%("))
    (dolist (problem (directory (concatenate 'string result-dir "plan*.SOL")))
      (setq pfile (concatenate 'string (pathname-name problem) "." (pathname-type problem)))
      (setq command (concatenate 'string "cat " result-dir pfile
				 " | grep \"\\(; Problem \\|; Time\\|; Distance\\|; NrActions\\|; MetricValue\\)\""
;;				 " | grep \"\\([.0-9]\\+: \\|; Problem \\|; Time\\|; Distance\\|; NrActions\\)\""
				 " | sed -e \"s/; Problem /( /\""
;; 				 " | sed -e \"s/; Problem \\([-a-zA-Z0-9][-a-zA-Z0-9]*\\).pddl/(\\1 /\""
				 " | sed -e \"s/; Time \\([0-9][0-9]*.[0-9][0-9]*\\)/\\1 /\""
				 " | sed -e \"s/; NrActions \\([0-9][0-9]*\\)/\\1 /\""
				 " | sed -e \"s/; MetricValue \\([0-9][0-9]*.[0-9][0-9]*\\)/\\1 /\""
				 " | sed -e \"s/; Distance from input plan: \\([0-9][0-9]*\\)/ \\1/\""
;; 				 " | sed -e \"s/[0-9][0-9]*.[0-9][0-9]*://\" "
;; 				 " | sed -e \"s/\\[[0-9][0-9]*.[0-9][0-9]*\\]//\" "
;; 				 " | sed -e \"s/\\[[0-9][0-9]*\\]//\" "
;; 				 " | sed -e \"s/[0-9][0-9]*://\" "
;; 				 " | sed -e \"s/ ;; InputAct//\" "
				 ">> " summary-file))
;;      (format t "~%Cleaning file: ~a" command)
      (execute-shell-command command)
      (with-open-file (ofile summary-file :direction :output :if-exists :append)
	(format ofile ")")))
    (with-open-file (ofile summary-file :direction :output :if-exists :append)
      (format ofile ")"))
    (if write-data-file-p (write-data-file data-file summary-file iterations))))

;; To DO: check if problems are solved
(defun write-data-file (data-file summary-file iterations)
  (with-open-file (ifile summary-file :direction :input)
    (setf *sols* (read ifile)))
  (with-open-file (ofile data-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format ofile "# 0 Prob, 1 Solved, 2 Time, 3 Length, 4 Nodes, 5 Evals, 6 Cost, 7 ReuseClosed, 8 Reuse, 9 CurrentSearch, 10 NumberBetterLength, 11 NumberBetterCost, 12 TotalTimeHeuristic, 13 Unstability (12 AvgTimeHeuristic 13 TotalTimeHeuristic 14 Unstability in PLANNER)~%")
    (do* ((sols *sols* (cdr sols))
	  (sol (car sols) (car sols))
	  (i 1 (1+ i))
	  (time nil)
	  (length nil)
	  (cost nil)
	  (stability nil))
	((null sols))
      (cond ((= i 1) (format ofile "~d 1.0 ~4$ ~d 0 0 ~4$ 0 0 0 0 0 0.0 ~4$~%" 1 (nth 1 sol) (nth 2 sol) (nth 3 sol) (nth 4 sol)))
	    ((= (mod i iterations) 0)
	     (format ofile "~d 1.0 ~4$ ~d 0 0 ~4$ 0 0 0 0 0 0.0 ~4$~%" (floor i iterations) (median time iterations) (median length iterations) (median cost iterations)
		     (median stability iterations))
	     (setq time nil length nil cost nil stability nil))
	    (t (push (nth 1 sol) time)
	       (push (nth 2 sol) length)
	       (push (nth 2 sol) cost)
	       (push (nth 3 sol) stability))))))
;; (if (nth 4 sol) 1.0 0.0)

(defun test-difference (solutions solution-tlplan)
  (dolist (sol solutions)
    (format t "~%Difference 1: ~a" (set-difference (cadddr sol) solution-tlplan :test #'equal))
    (format t "~%Difference 2: ~a" (set-difference solution-tlplan (cadddr sol) :test #'equal))
    (format t "~%Difference according to LPG-adapt: ~d" (caddr sol))
    (format t "~%Our list difference: ~d, ~d"
	    (solution-difference (cadddr sol) solution-tlplan)
	    (solution-difference solution-tlplan (cadddr sol)))
    (format t "~%Our set difference: ~d, ~d"
	    (length (set-difference (cadddr sol) solution-tlplan :test #'equal))
	    (length (set-difference solution-tlplan (cadddr sol) :test #'equal)))))

(defun solution-difference (sol1 sol2 &key (count 1))
  (let ((diff 0))
    (dolist (a sol1)
      (if (member a sol2 :test #'equal)
	  (setq sol2 (remove a sol2 :test #'equal))
	  (incf diff)))
    diff))