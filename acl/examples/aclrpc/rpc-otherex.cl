
(in-package :user)

(eval-when (compile load eval)
  (require :aclrpc)
  (use-package :net.rpc)
  )


;;; Run a single second image
;;;
(defun other-example-1 (&optional (verbose :output) &aux keep)
  (unwind-protect
      (let ((client (run-other-client :verbose verbose)))

	(when (client-ready-p client)

	  (client-funcall client "print" "Hello from server")
	  (or (y-or-n-p "kill client?") (setf keep t))

	  ))
    (or keep (client-end-all))))


;;; Run several images with a simple starting function
;;;
(defun other-example-2 (&optional (n 3) delay (verbose :output) &aux all keep)
  (unwind-protect
      (let ()
	(dotimes (i n)
	  (let ((client (run-other-client 
			 :data i :start (list 'print i) :verbose verbose)))
	    (push client all)
	    (when delay (sleep delay))
	    ))
	(dolist (client all)
	  (multiple-value-bind (ready reason)
	      (client-ready-p client)
	    (if ready
		(format t "~&; Client ~A is ready.~%" (client-lisp-data client))
	      (format t "~&; Client ~A is ~A.~%" (client-lisp-data client) reason))))
	
	(or (y-or-n-p "kill clients?") (setf keep t))

	)
    (or keep (client-end-all))))


;;; Run a single second image with a more complex task.
;;;  Cleanup when done.
;;;
(defvar *example-3-done* nil)
(defun other-example-3 (&optional (n 5) keep (verbose :output))
  (setf *example-3-done* nil)
  (unwind-protect
      (let ((client (run-other-client :infile "rpc-otherex.fasl"
				      :start (list "example-3-task" n)
				      :verbose verbose)))
	(client-ready-p client)
	(loop
	 (when *example-3-done*
	   (format t "~&;Task done.")
	   (return))
	 (sleep 0.5))
	)
  (or keep (client-end-all))))

(defun example-3-task (n)
  (with-other-client-port
   (format t "~&;Starting task.")
   (rcall 'format t "~&;Starting task.")
   (sleep n)
   (format t "~&;Ending task.")
   (rcall 'format t "~&;Ending task.")
   (rcall 'set '*example-3-done* t))
  (format t "~&;Ended task.")
  )