;; -*- Mode: common-lisp; Package: common-lisp-user -*-
;; $Id: forksmp.cl,v 1.1.2.1 2007/11/16 17:51:42 layer Exp $
;;
;; This example shows how to use excl.osi:fork on non-Windows, non-smp
;; platforms to utilize all available processors for Lisp computations.
;; It assumes that your application has compute bound parts which can be
;; run in parallel.  In the example below, the "work" is simulated with a
;; loop calling expt.
;;
;; This technique is not appropriate for problems where the granularity of
;; parallelism is very fine.  The overhead would be too large for those
;; problems.  The overhead is such that around 11,500 calls per second can
;; be made with this framework, on a 1.8GHz x86_64 machine.
;;
;; The first part of the example code is a framework for executing the work
;; units on different processors.  The second part is a specific example
;; using this framework.
;;
;; This example does not use anything fancy to pass information between the
;; parent and child processes, just the printer and reader.  The less
;; information passed the less overhead there will be.
;;
;; Terminology:
;;   task      :: a unit of work, or, in lisp terms, an expression that can
;;                be evaluated
;;   CPU       :: an actual hardware processor
;;   processor :: an entity which can do work, or in terms of this example
;;                a lisp subprocess which performs a task on a CPU
;;
;; There can (and often will) be more processors than CPUs, though if there
;; are many more processors than CPUs then tasks might take much longer
;; than expected to complete.
;;
;; In the "Example" section, there is an example which is run with varying
;; number of processors.  For each run, there is an idea of the single
;; processor time it would take to complete.  This is labeled "WORK" in the
;; test run below.  In a theoretical sense, if there was 100 seconds of
;; work to be done, 4 CPUs and the tasks the right size and independent of
;; each other, it you might get the work done in 25 seconds of real time.
;;
;; Here is an example run on 4 processor Opteron system.  Each processor is
;; running at 1.8GHz.
#|
cl-user(2): (run)
Detected 4 CPUs
Iterations 40, processors 2, WORK: 8.0 seconds, REAL TIME: 4.164
Iterations 40, processors 3, WORK: 12.0 seconds, REAL TIME: 4.207
Iterations 40, processors 4, WORK: 16.0 seconds, REAL TIME: 4.18
Iterations 40, processors 5, WORK: 20.0 seconds, REAL TIME: 5.905
Iterations 40, processors 6, WORK: 24.0 seconds, REAL TIME: 7.075
Iterations 40, processors 7, WORK: 28.0 seconds, REAL TIME: 7.811
Iterations 40, processors 8, WORK: 32.0 seconds, REAL TIME: 8.717
Iterations 40, processors 9, WORK: 36.0 seconds, REAL TIME: 9.549
Iterations 40, processors 10, WORK: 40.0 seconds, REAL TIME: 10.525
Iterations 40, processors 11, WORK: 44.0 seconds, REAL TIME: 11.529
Iterations 40, processors 12, WORK: 48.0 seconds, REAL TIME: 12.535
nil
cl-user(3): 
|#
;; We can see the work is well distributed over the actual CPUs and the
;; real time to complete the work is roughly work / cpus.
;;
;; Now, let's look at a Dual 2.4GHz Xeon system.  Due to hyperthreading the
;; Linux kernel believes there are 4 processors on this system.
#|
cl-user(2): (run)
Detected 4 CPUs
Iterations 40, processors 2, WORK: 8.0 seconds, REAL TIME: 4.318
Iterations 40, processors 3, WORK: 12.0 seconds, REAL TIME: 5.452
Iterations 40, processors 4, WORK: 16.0 seconds, REAL TIME: 12.303
Iterations 40, processors 5, WORK: 20.0 seconds, REAL TIME: 14.495
Iterations 40, processors 6, WORK: 24.0 seconds, REAL TIME: 11.512
Iterations 40, processors 7, WORK: 28.0 seconds, REAL TIME: 17.971
Iterations 40, processors 8, WORK: 32.0 seconds, REAL TIME: 19.951
Iterations 40, processors 9, WORK: 36.0 seconds, REAL TIME: 26.855
Iterations 40, processors 10, WORK: 40.0 seconds, REAL TIME: 30.471
Iterations 40, processors 11, WORK: 44.0 seconds, REAL TIME: 27.072
Iterations 40, processors 12, WORK: 48.0 seconds, REAL TIME: 33.404
nil
cl-user(3): 
|#
;; Not nearly as good as the first system, which as it happens costs about
;; 10 times as much.  For 12 processors it was close to 3 times as fast.
;;
;; As was said at the outset, this approach isn't for every application.  I
;; can be very useful without a lot of trouble.  The main benefit is that
;; in the presence of multiple processors, an application can increase
;; efficiency without a lot of work.  The downside, which can be overcome
;; with good programming techniques, is that debugging is more difficult
;; using this approach.  This is not that big a deal, since any serious
;; server application will need to employ these same error recovery
;; techniques.

(in-package :user)

;;;;;;;;;;;;;;; DEBUGGING AND TIMING CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro my-time (&body body)
  ;; We just want to know a single thing: the real time spent in BODY.
  (let ((start (gensym))
	(res (gensym))
	(time (gensym)))
    `(let* ((,start (get-internal-real-time))
	    (,res ,@body)
	    (,time (- (get-internal-real-time) ,start)))
       (format t "REAL TIME: ~s~%" (float (/ ,time 1000)))
       (force-output t)
       ,res)))

(eval-when (compile eval)
  ;; Changing the initial value of this variable to `t' will result in a
  ;; LOT of output.  Only needed when debugging problems.
  (defparameter *debug* nil))

(defmacro .debug (format-string &rest format-arguments)
  ;; We discriminate on *debug* at macroexpand time so there will be no
  ;; runtime penalty for the debugging code we've added.
  (when *debug*
    `(..debug ,format-string ,@format-arguments)))

(defvar *debug-time-base* nil)

(defun ..debug-reset-time-base ()
  (setq *debug-time-base* (excl::cl-internal-real-time)))

(..debug-reset-time-base)

(defun ..debug (format-string &rest format-arguments)
  ;; Use an undocumented (not for long, promise) function to get the
  ;; current time that includes milliseconds.  The first value is the
  ;; number of seconds between now and 2032.  Date chosen to
  ;; be a fixnum in a 32-bit lisp when the lisp you are using was
  ;; released.
  (multiple-value-bind (seconds mseconds)
      (excl::cl-internal-real-time)
    (format t "~&~2,'0d:~3,'0d " (- seconds *debug-time-base*) mseconds))
  (format t format-string format-arguments)
  (format t "~&")
  (force-output t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General framework for High-level task and processor management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The object type that holds information about available processors.
(defstruct task-manager
  ;; A list of free PROCESSOR objects.  Items on this list are free to have
  ;; tasks scheduled on them.
  processors)

(defmethod print-object ((object task-manager) stream) 
  (format stream "#<TM ~s>" (task-manager-processors object)))

(defstruct processor
  ;; The stream used to write to the processor.
  to-stream
  ;; The stream used to read from the processor.
  from-stream
  ;; The operating system process ID for the processor.  Used to terminate
  ;; the processor.
  pid)

(defmethod print-object ((object processor) stream) 
  (format stream "#<PROC TO: ~s, FROM: ~s, PID ~s>"
	  (excl::stream-output-fn (processor-to-stream object))
	  (excl::stream-input-fn (processor-from-stream object))
	  (processor-pid object)))

(defvar *task-manager* nil)

(defun sigterm-handler-for-child (&rest args)
  (declare (ignore args))
  (exit 1 :quiet t :no-unwind t))

(defun initialize-processors (child-worker &optional (n 4))
  (when *task-manager* (end-processors))
  (flet ((processor ()
	   (multiple-value-bind (from-child-read from-child-write)
	       (excl.osi:pipe)
	     (multiple-value-bind (to-child-read to-child-write)
		 (excl.osi:pipe)
	       (let ((pid (excl.osi:fork)))
		 (cond
		  ((= pid 0)
		   ;; Child
		   (excl::add-signal-handler 15 'sigterm-handler-for-child)

		   ;; These are `parent' only
		   (close from-child-read)
		   (close to-child-write)

		   (unwind-protect
		       (funcall child-worker to-child-read
				from-child-write)
		     (ignore-errors (close to-child-read))
		     (ignore-errors (close from-child-write))
		     (exit 0 :quiet t :no-unwind t)))
		  (t
		   ;; Parent
		
		   ;; These are `child' only
		   (close from-child-write)
		   (close to-child-read)
		
		   (make-processor :to-stream to-child-write
				   :from-stream from-child-read
				   :pid pid))))))))
    (setq *task-manager*
      (make-task-manager
       :processors (let ((c '())) (dotimes (i n c) (push (processor) c)))))))

(defvar *processor-lock* (mp:make-process-lock :name "free processor lock"))

(defun find-free-processor ()
  ;; In the controlling process, we need a way to allocate a task to a new
  ;; procesor.  This function finds the next available processor.  It is
  ;; important that this function be efficient.
  (declare (optimize (speed 3)))
  (unless *task-manager* (error "You must call initialize-processors first."))
  (.debug "FIND FREE PROCESSOR:")
  (let ((tm *task-manager*))
    (loop
      (if* (task-manager-processors tm)
	 then (return (mp:with-process-lock (*processor-lock*)
			(pop (task-manager-processors tm))))
	 else (mp:process-wait "waiting for processor"
			       (lambda (tm)
				 (task-manager-processors tm))
			       tm)))))

(defun release-processor (processor)
  ;; Called on a PROCESSOR that should now be considered ready for use for
  ;; another task.
  (unless *task-manager* (error "You must call initialize-processors first."))
  (.debug "RELEASE-PROCESSOR: ~s" processor)
  (mp:with-process-lock (*processor-lock*)
    ;; Put it back into the pool of free processors:
    (push processor (task-manager-processors *task-manager*))))

(defun end-processors ()
  ;; Called to shutdown the child processors.
  (unless *task-manager* (error "You must call initialize-processors first."))
  (dolist (p (task-manager-processors *task-manager*))
    (.debug "shutting down processor: ~s" p)
    (close (processor-to-stream p))
    (close (processor-from-stream p))
    (excl.osi:kill (processor-pid p) excl.osi:*sigterm*)
    (system:reap-os-subprocess :pid (processor-pid p)))
  (setq *task-manager* nil))

(defun wait-for-processor (function &rest arguments)
  ;; This is the function which does the work of communicating with child
  ;; subprocesses and implements the `PROCESSOR' abstraction.  FUNCTION
  ;; must be a symbol and ARGUMENTS must be symbols, strings or numbers.
  ;;
  ;; Efficiency is important in this function, too.  We use
  ;; WAIT-FOR-INPUT-AVAILABLE to determine when the child subprocess is
  ;; done with their task and we can read the return value.
  (assert (symbolp function))
  (let ((processor (find-free-processor)))
    (unwind-protect
	(let ((to-stream (processor-to-stream processor))
	      (from-stream (processor-from-stream processor)))
	  (write-char #\( to-stream)
	  (write-string (symbol-name function) to-stream)
	  (write-char #\space to-stream)
	  (dolist (argument arguments)
	    (.debug "WAIT-FOR-PROCESSOR: arg: ~s" argument)
	    (if* (stringp argument)
	       then (write-char #\" to-stream)
		    (write-string argument to-stream)
		    (write-char #\" to-stream)
	     elseif (symbolp argument)
	       then (write-string (symbol-name argument) to-stream)
	     elseif (integerp argument)
	       then (write-string (prin1-to-string argument) to-stream)
	       else (error "Cannot convert this argument: ~s." argument))
	    (write-char #\space to-stream))
	  (write-char #\) to-stream)
	  (force-output to-stream)

	  (mp:wait-for-input-available from-stream)
    
	  (let ((result (read from-stream nil from-stream)))
	    (if* (eq result from-stream)
	       then (error "Processor did not complete for ~s." function)
	     elseif (eq '#:error result)
	       then (warn "Processor did not complete for ~s." function)
	       else result)))
      (release-processor processor))))

(defun quiet-sigpipe-handler (sig continuable)
  ;; The default SIGPIPE handler in Allegro CL is noisy.  This one merely
  ;; says "I handled the SIGPIPE" and prints nothing.  In other words,
  ;; ignore SIGPIPE.
  (declare (ignore sig continuable))
  t)

(excl::add-signal-handler 13 'quiet-sigpipe-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example using framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *error-marker* 'sys::error-marker)

(defun child-subprocess (input-stream output-stream)
  ;; This function runs in the child subprocess and reads forms from
  ;; INPUT-STREAM, evaluates them and writes the result to OUTPUT-STREAM.
  ;;
  ;; Errors in the evaluation are passed back to the parent using a unique
  ;; symbol bound to *error-marker*.
  (let (form)
    (loop
      (setq form (read input-stream nil input-stream))
      (.debug "CHILD-SUBPROCESS: form is ~s" form)
      (when (eq input-stream form)
	(write-string "#:eof" output-stream)
	(force-output output-stream)
	(return))
      (let* ((result (handler-case (eval form)
		       (error (c)
			 (..debug "CHILD ERROR: ~a" c)
			 (cons *error-marker*
			       (format nil "~a" c))))))
	(.debug "CHILD-SUBPROCESS: result is ~s" result)
	(write result :stream output-stream)
	(write-char #\newline output-stream)
	(force-output output-stream)))))

(defun number-of-cpus ()
  (let ((default 4))
    #+linux
    (if* (probe-file "/proc/cpuinfo")
       then (let ((processors 0))
	      (excl.osi:with-command-output (line "cat /proc/cpuinfo")
		(when (match-re "^processor" line :return nil)
		  (incf processors)))
	      processors)
       else default)
    #-linux default))

(defun create-work (child-worker &key iterations
				      work-estimate-seconds
				      processors)
  (assert iterations)
  (assert processors)

  (when work-estimate-seconds
    (format t "WORK: ~s seconds, "
	    (* iterations processors work-estimate-seconds))
    (force-output t))

  (initialize-processors #'child-subprocess processors)

  (unwind-protect
      (let ((gates '()))
	(my-time
	 (progn
	   ;; For each processor, start a Lisp process that will do the
	   ;; given amount of work.  A gate per processor is used to
	   ;; determine when all the work is done, so we can shut down.
	   (dotimes (i processors)
	     (let ((gate (mp:make-gate nil)))
	       (mp:process-run-function (format nil "foo~d" i)
		 (lambda (gate i)
		   (dotimes (j iterations)
		     (let ((result (wait-for-processor child-worker j)))
		       (if* (and (consp result)
				 (eq *error-marker* (car result)))
			  then (..debug "PARENT: Got error: ~a" (cdr result))
			elseif  (/= (1+ j) result)
			  then (..debug
				"PARENT: foo~d: invalid return value: ~s"
				i result))))
		   (mp:open-gate gate))
		 gate
		 i)
	       (push gate gates)))
	   ;; This is a very efficient way to wait.
	   (dolist (gate gates)
	     (mp:process-wait "wait for processor to finish"
			      #'mp:gate-open-p gate)))))
  
    (end-processors)))

;;;; timing loop, for getting a certain amount of "work"
;;;;
(defun dummy (i) (expt i 100))
(defun timing-loop (&key seconds)
  (let ((i 0))
    (mp:with-timeout (seconds)
      (loop 
	(dummy i)
	(setq i (1+ i))))
    i))
(defun loopit (n) (dotimes (i n) (dummy i)))
;; (setq n (timing-loop :seconds 1))
;; (loopit n) => should take 1 second

;; `defvar' instead of `defparameter' so it does not change in a Lisp
;; session, if we reload this file.  That will make comparing times between
;; runs a rational thing to do.
(defvar *iterations*
    (let ((it 0))
      (dotimes (i 10 it)
	(setq it (max it (timing-loop :seconds .1))))))

(defun example-child-worker (i)
  (loopit *iterations*)
  (+ i 1))

(defun example-application (&key processors iterations)
  (create-work 'example-child-worker
	       :iterations iterations
	       :processors processors
	       :work-estimate-seconds .1))

(defvar *number-of-cpus* (number-of-cpus))

(defun run ()
  (format t "Detected ~d CPUs~%" *number-of-cpus*)
  (..debug-reset-time-base)
  (macrolet ((run-1 (&key iterations processors)
	       `(progn
		  (format t "Iterations ~d, processors ~d, "
			  ,iterations ,processors)
		  (force-output t)
		  (example-application :iterations ,iterations
				       :processors ,processors))))
    (run-1 :iterations 40 :processors 2)
    (run-1 :iterations 40 :processors 3)
    (run-1 :iterations 40 :processors 4)
    (run-1 :iterations 40 :processors 5)
    (run-1 :iterations 40 :processors 6)
    (run-1 :iterations 40 :processors 7)
    (run-1 :iterations 40 :processors 8)
    (run-1 :iterations 40 :processors 9)
    (run-1 :iterations 40 :processors 10)
    (run-1 :iterations 40 :processors 11)
    (run-1 :iterations 40 :processors 12)))

;;;;;;;;;;;;;;; Error handling example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun example-child-worker2 (i)
  (if* (eql i 50)
     then (error "foo: ~a" i)
     else (1+ i)))

(defun example-application2 (&key processors iterations)
  (create-work 'example-child-worker2
	       :iterations iterations
	       :processors processors))

(defun run2 ()
  (format t "Detected ~d CPUs~%" *number-of-cpus*)
  (..debug-reset-time-base)
  (example-application2 :iterations 100 :processors *number-of-cpus*))

#|
cl-user(6): (run2)
Detected 4 CPUs
00:037 CHILD ERROR: (foo: 50)
00:037 PARENT: Got error: (foo: 50)
00:038 CHILD ERROR: (foo: 50)
00:039 PARENT: Got error: (foo: 50)
00:039 CHILD ERROR: (foo: 50)
00:040 PARENT: Got error: (foo: 50)
00:041 CHILD ERROR: (foo: 50)
00:042 PARENT: Got error: (foo: 50)
REAL TIME: 0.033
nil
cl-user(7): 
|#
