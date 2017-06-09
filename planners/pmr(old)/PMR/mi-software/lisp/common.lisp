;;; This is the file with all this Lisp functions that I use across applications

(in-package "COMMON-LISP-USER")

(setf *basic-functions-defined* t)

(defun load-and-compile (&optional (files (if (boundp '*ipss-files*) *ipss-files*))
			 &key (load-path (concatenate 'string *my-planning-path* "ipss/"))
			      (compile-path  (concatenate 'string *my-planning-path* "ipss/"
							  #+sbcl ".sbcl/"
							  #+(and clisp unix) ".clisp/"
							  #+(and clisp win32) "dosbin/"
							  #+allegro ".allegro/"))
			      (re-load-p t)
			      (load-only-source-p nil))
  (declare (special *ipss-files* *my-planning-path*))
  (ensure-directories-exist compile-path)
  (dolist (file files)
    (if re-load-p
	(load (format nil "~a~(~a~).lisp" load-path file)))
    (unless load-only-source-p
      (compile-file (format nil "~a~(~a~)" load-path file)
		    :output-file (format nil "~a~(~a~).~a" compile-path file #+(or sbcl allegro) "fasl" #+clisp "fas"))
      (load (format nil "~a~(~a~)" compile-path file)))))

;; It returns a random element from list
(defun choose-one (list)
  (if list
      (nth (random (length list)) list)))

;; It returns a random element from list different from the one given as argument
(defun choose-diff-one (elem list)
  (let* ((number-elems (length list))
	 (new-elem (nth (random number-elems)
			list)))
    (if (and (> number-elems 1)
	     (equal new-elem elem))
	(choose-diff-one elem list)
      new-elem)))

(defun diff (x y)
  (not (eq x y)))

;; Comprueba si dos conjuntos son iguales
(defun equal-set-p (set1 set2 &key (test #'equal))
  (null (set-exclusive-or set1 set2 :test test)))

;; It returns the euclidean distance between two points
(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(defun in-between (x x1 x2)
  (and (> x x1) (< x x2)))

;; It prints the contents of a hash table
(defun pp-hash-table (hash-table)
  (maphash #'(lambda (key val)
	       (format t "~%~a: ~a" key val))
	   hash-table))

;; It prints a list of elements in a stream with a tab
(defun pp-list (list &optional (tab 1) (ostream t) lowercasep same-line-p)
  (dolist (literal list)
      (format ostream (format nil (concatenate 'string (if same-line-p " " "~~%")
					       (if lowercasep
						   "~~~dt~~(~~s~~)"
						   "~~~dt~~s"))
			      tab)
	      literal)))

(defun elapsed-time (initial-time &optional (units 'run-time))
  (float (/ (- (if (eq units 'run-time)
		   (get-internal-run-time)
		   (get-internal-real-time))
	       initial-time)
	    internal-time-units-per-second 1.0)))

(defun copy-array (array &optional (size (array-dimension array 0)))
  "It generates a copy of a square array"
  (let ((array1 (make-array (list size size) :initial-element 0)))
    (dotimes (i size)
	(dotimes (j size)
	    (setf (aref array1 i j)
		  (aref array i j))))
    array1))

(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Given two points, it returns a list of a and b, constants that define the line equation (y=ax+b)
(defun compute-line-equation (point1 point2)
  (let* ((denominator (- (car point1) (car point2)))
	 (slope (if (zerop denominator)
		    most-positive-fixnum
		    (/ (- (cadr point1) (cadr point2))
		       denominator))))
    (list slope (if (< slope most-positive-fixnum)
		    (- (cadr point1) (* slope (car point1)))
		    (car point1)))))

;; known-var can be either 'x or 'y
(defun substitute-in-equation (equation x y known-var)
  (if (< (car equation) most-positive-fixnum)
      (if (eq known-var 'x)
	  (list x (+ (* (car equation) x) (cadr equation)))
	  (list (/ (- y (cadr equation)) (car equation)) y))
      (if (eq known-var 'y)
	  (list (cadr equation) y))))

;; non-destructive version of sort
(defun my-sort (l test)
  (sort (copy-seq l) test))

;; generic (lisp-independent) shell execution
(defun execute-shell-command (command &optional (bash-file (concatenate 'string *my-tmp-path* "run-shell")))
  #-sbcl (run-shell-command command :wait t)
  #+sbcl (progn (with-open-file (ostream bash-file :direction :output :if-exists :supersede :if-does-not-exist :create)
 		  (format ostream "#/bin/bash~%~a~%" command))
		(sb-ext:run-program "/bin/bash" (list bash-file))))

(defun timed-execute-shell-command (command &key (bash-file (concatenate 'string *my-tmp-path* "run-shell")) (timeout nil) (time-units 'real-time))
  #-sbcl (run-shell-command command :wait t)
  #+sbcl (progn (with-open-file (ostream bash-file :direction :output :if-exists :supersede :if-does-not-exist :create)
		  (format ostream "#/bin/bash/~%~a~%" command))
		(do ((process (sb-ext:run-program "/bin/bash" (list bash-file) :wait nil))
		     (time (if (eq time-units 'real-time)
			       (get-internal-real-time)
			       (get-internal-run-time))))
		    ((or (>= (elapsed-time time time-units) timeout)
			 (not (eq (sb-ext::process-status process) :running)))
		     (sb-ext::process-kill process 9 :process-group)))))

(defun read-all-file (file)
  (let ((what nil))
    (with-open-file (input file :direction :input)
      (setq what (read input)))
    what))

(defun average (sample-list &optional (sample-size (length sample-list)))
  (if sample-list
      (/ (apply #'+ sample-list) sample-size 1.0)))

(defun median (sample-list sample-size)
  (declare (ignore sample-size))
  (if sample-list
      (let* ((size (length sample-list))
	     (middle-point (floor size 2))
	     (sorted-list (sort sample-list #'<=)))
	(cond ((= size 1) (car sorted-list))
	      ((evenp size)
	       (/ (+ (nth middle-point sorted-list)
		     (nth (1- middle-point) sorted-list))
		  2.0))
	      (t (nth middle-point sorted-list))))
      0))

(defun shuffle (list)
  (if (cdr list)
      (let ((elem (choose-one list)))
	(cons elem (shuffle (remove elem list))))
      list))

;; I would have to test it throughly
;; each planner returns a different output!
;; LAMA: (time length cost nodes plan)
;; SAYPHI: solution instance
;; IPSS, METRIC-FF: plan
(defun planning (planner problem-file &key (domain-directory *domains-dir*) (domain 'blocksworld) (domain-file "domain.pddl") (time-bound 300) (algorithm 'enforced-hill-climbing) (search-options nil))
  (case planner
    (ipss (hamlet-test domain-directory (list problem-file)
		       :test-time-bound time-bound :run-without-p nil
		       :multiple-sols-p nil :backtrack-with-costs-p nil)
	  (get-solution-from-planner planner nil))
    (sayphi (say-domain domain-directory domain-file)
	    (prob problem-file)
	    (plan :algorithm algorithm :timeout time-bound :search-options search-options))
    (metric-ff (execute-ff :domain-file domain-file :problem-file problem-file)
	       (get-solution-from-planner planner nil))
    (lama (execute-lama :domain-file domain-file :problem-file problem-file :domain-directory domain-directory :result-in-file-p nil))
    (t nil)))

(defun insert-in-position (element list position)
  (append (butlast list (- (length list) position))
	  (list element)
	  (nthcdr position list)))
