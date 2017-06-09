
(in-package :user)

;; $Id: user2.cl,v 5.1 2004/02/18 19:19:03 mm Exp $

(eval-when (compile load eval)
  (require :jlinker)
  (require :jlinkent)
  (use-package :net.jlinker))

(defclass user2-base ()
  (
   ))

(defclass user2 (user2-base http-servlet)
  (
   (java-classes :initform '("User2Servlet"))
   ))

(defclass user2a (user2-base async-http-servlet)
  (
   (java-classes :initform '("User2ServletA"))
   ))

(defclass user2b (user2-base multi-async-http-servlet)
  (
   (java-classes :initform '("User2ServletB"))
   ))


(defmethod do-get ((self user2-base) request response)
  (declare (ignore request))
  (let (out println)

    (jcall "setContentType" response "text/html")
    (setf out (jcall "getWriter" response))
    (setf println (jmethod (jclass-of out) "println" "java.lang.String"))

    (jcall println out "<html>")
    (jcall println out "<head>")
    (jcall println out "<title>Talk to Lisp</title>")
    (jcall println out "</head>")
    (jcall println out "<body>")
    (jcall println out (with-output-to-string 
			  (*initial-terminal-io*) 
			 (excl::copyright-banner)))
    (jcall println out (format nil "<p>The process is: ~A"
			       (mp:process-name mp:*current-process*)))
    (jcall println out "<p>The time is: ")
    (jcall println out 
	   (multiple-value-bind (s mn h d m y)
	       (get-decoded-time)
	     (format nil "~A/~A/~A  ~A:~A:~A" m d y h mn s)))
    (jcall println out "<hr>")

    (jcall println out 
	   (format nil "<form action=~S method=POST target=user2 >"
		   (first (slot-value self 'java-classes))))
    (jcall println out "<p>Enter an expression:")
    (jcall println out "<p>")
    (jcall println out "<input type=text size=50 name=\"expression\" >")
    (jcall println out "<p>")
    (jcall println out "<input type=submit value=\"Eval\">")
    (jcall println out "</form>")

    (jcall println out "</body>")
    (jcall println out "</html>")

    (jcall "close" out)

    ))

(defmethod do-post ((self user2-base) request response)
  (let (out println e v)

    (jcall "setContentType" response "text/html")
    (setf out (jcall "getWriter" response))
    (setf println (jmethod (jclass-of out) "println" "java.lang.String"))

    (jcall println out "<html>")
    (jcall println out "<head>")
    (jcall println out "<title>Talk to Lisp</title>")
    (jcall println out "</head>")
    (jcall println out "<body>")
    (jcall println out (with-output-to-string 
			  (*initial-terminal-io*) 
			 (excl::copyright-banner)))
    (jcall println out (format nil "<p>The process is: ~A"
			       (mp:process-name mp:*current-process*)))
    (jcall println out "<p>The time is: ")
    (jcall println out 
	   (multiple-value-bind (s mn h d m y)
	       (get-decoded-time)
	     (format nil "~A/~A/~A  ~A:~A:~A" m d y h mn s)))
    (jcall println out "<hr>")


    ;;(setf e (jcall "getParameterNames" request))
    ;;(loop (when (null (jcall "hasMoreElements" e)) (return)))

    (setf v (jcall "getParameter" request "expression"))
    (jcall println out 
	   (format nil "<p>Expression was: ~A" v))
    (jcall println out 
	   (format nil "<p>Read as: ~S" 
		   (setf e (ignore-errors (read-from-string v)))))
    (multiple-value-bind (vals err)
	(ignore-errors (multiple-value-list (eval e)))
      (cond
       (err (jcall println out 
		   (format nil "<p>Eval signals error: ~A" err)))
       ((null vals) (jcall println out 
			   "<p>Eval to zero values."))
       (t (dolist (x vals)
	    (jcall println out 
		   (format nil "<p>Eval to: ~S" x))))))
    (jcall println out "<hr>")

    (jcall println out 
	   (format nil "<form action=~S method=POST target=user2 >"
		   (first (slot-value self 'java-classes))))
    (jcall println out "<p>Enter an expression:")
    (jcall println out "<p>")
    (jcall println out "<input type=text size=50 name=\"expression\" >")
    (jcall println out "<p>")
    (jcall println out "<input type=submit value=\"Eval\">")
    (jcall println out "</form>")

    (jcall println out "</body>")
    (jcall println out "</html>")

    (jcall "close" out)

    ))


