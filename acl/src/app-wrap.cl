;;; An application wrapper.

;; This work in the Public Domain, thereby relinquishing all
;; copyrights. Everyone is free to use, modify, republish, sell or give
;; away this work without prior consent from anybody.
;;
;; This code is provided on an "as is" basis, without warranty of any
;; kind. Use at your own risk! Under no circumstances shall the author(s)
;; or contributor(s) be liable for damages resulting directly or indirectly
;; from the use or non-use of this program.

;; Set to `t' to be able to debug errors rather than have the application exit.

(defvar *debug* nil)

(defun application-top-level ()
  ;; This is the function executed when the binary image is run by the
  ;; operating system or shell.  This particular restart protects from a
  ;; wide variety of things the user might attempt to do to get into the
  ;; Lisp top level.
  (labels
      ((signal-handler (number &optional ignore)
	 ;; This function could discriminate on the signal number to do
	 ;; something fancy, like popping up a menu when an interrupt is
	 ;; received.
	 (declare (ignore ignore))
	 (when *debug* (break "got signal # ~d" number))
	 (bye 1 "received signal number ~d." number))
       (bye (&optional (code 0) format-string &rest args)
	 (when (= code 0) (exit code :quiet t :no-unwind t))
	 (when *debug* (apply #'break format-string args))
	 (format t "~&Exiting: ~?~%" format-string args)
	 (exit code :quiet t :no-unwind t)))
    (excl::add-signal-handler 2 #'signal-handler)
    (unwind-protect
	(handler-bind ((error #'(lambda (c) (bye 1 "~a" c)))
		       ;; Omit the next subform if you want warnings
		       ;; to be printed.
		       (warning #'(lambda (c)
				    (declare (ignore c))
				    (muffle-warning))))
	  (system:with-command-line-arguments ("D" debug) (args)
	    (declare (ignore args))
	    ;; Set but don't bind *debug*, because binding will only effect the
	    ;; current process under multiprocessing.
	    (setq *debug* debug)

	    (when *debug* (break "before application main loop"))

	    ;; Handle command line arguments, if any...
	    (application-main-loop))
	  (bye)))))

(defun application-main-loop ()
  ;; This demo main loop is a simple read-eval-print loop.
  ;;
  ;; The first three bindings are necessary if background streams are
  ;; used (see excl:use-background-streams).
  (let ((*terminal-io* *terminal-io*)
	(*standard-input* *terminal-io*)
	(*standard-output* *terminal-io*)
	token)
    (loop
      (format t "~%~%enter an expression to be evaluated~%=> ")
      (force-output)
      (setq token (read))
      (cond ((eq :exit token)
	     (return-from application-main-loop
	       (values t 0)))
	    ((eq :debug token)
	     (setq *debug* t)))
      (print (eval token)))))

#|

;; The following is the example code from Technical Memo #19.

(defvar *debug* nil)

(defun schedule-top-level ()
  (labels
      ((signal-handler (number &optional ignore)
         (declare (ignore ignore))
         (when *debug* (break "got signal # ~d" number))
         (bye 1 "Exiting: received signal number ~d." number))
       (bye (&optional (code 0) format-string &rest args)
         (when (= code 0) (exit code :quiet t :no-unwind t))
         (when *debug* (apply #'break format-string args))
         (format t "~&Error: ~?~%" format-string args)
         (exit code :quiet t :no-unwind t)))
    (excl:add-signal-handler 2 #'signal-handler)
    (unwind-protect
	(handler-bind ((error #'(lambda (c) (bye 1 "~a" c)))
		       ;; Omit the next subform if you want warnings
		       ;; to be printed.
		       (warning #'(lambda (c) 
				    (declare (ignore c))
				    (muffle-warning))))
	  (si:with-command-line-arguments ("mrd" mail real debug)
	      (arguments)
	    ;; Set but don't bind *debug*, because binding will only effect the
	    ;; current process under multiprocessing.
	    (setq *debug* debug)
	    (when (cdr arguments)
	      (bye 1 "extra arguments:~{ ~a~}" (cdr arguments)))
	    (when (null arguments)
	      (setq *debug* t)
	      (break "for debugging"))
	    (schedule (car arguments) :real real :mail mail)))
      (bye))))

|#
