
(in-package :user)

;; $Id: user1.cl,v 5.1 2004/02/18 19:19:03 mm Exp $

(eval-when (compile load eval)
  (require :jlinker)
  (require :jlinkent)
  (use-package :net.jlinker))

(defclass user1-base ()
  (
   ))

(defclass user1 (user1-base http-servlet)
  (
   (java-classes :initform '("User1Servlet"))
   ))

(defclass user1a (user1-base async-http-servlet)
  (
   (java-classes :initform '("User1ServletA"))
   ))

(defvar *sm* "No Message")
(defun setm (&rest args)
  ;; Utility function that may be called at the Lisp console to modify
  ;; the output of the servlet.
  (setf *sm* (format nil "~{~A ~}" args)))

(defmethod do-get ((self user1-base) request response)
  (declare (ignore request))

  ;; This method overrides the do-get method on http-servlet
  ;; Does NOT do call-next-method

  (let (out println)

    (jcall "setContentType" response "text/html")
    (setf out (jcall "getWriter" response))
    (setf println 
	  (jmethod 
	   
	   ;; Use getClass instead of jclass-of to avoid security problems
	   ;;  when getting class by name in some servlet containers.
	   (jcall "getClass" out) 

	   "println" "java.lang.String"))

    (jcall println out "<html>")
    (jcall println out "<head>")
    (jcall println out "<title>Response from Lisp</title>")
    (jcall println out "</head>")
    (jcall println out "<body>")
    (jcall println out 
	   (with-output-to-string 
	    (*initial-terminal-io*) 
	    (excl::copyright-banner)))
    (jcall println out (format nil "<p>The process is: ~A"
			       (mp:process-name mp:*current-process*)))
    (jcall println out "<p>The time is: ")
    (jcall println out 
	   (multiple-value-bind (s mn h d m y)
	       (get-decoded-time)
	     (format nil "~A/~A/~A  ~A:~A:~A" m d y h mn s)))
    (jcall println out "<p>*sm* is: ")
    (jcall println out *sm*)
    
    (jcall println out "</body>")
    (jcall println out "</html>")
    
    (jcall "close" out)
    
    ))

