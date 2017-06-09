;; copyright (c) 2002-2015 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; simple network paging protocol
;; $Id: snpp.cl,v 1.4 2004/01/16 18:56:30 layer Exp $

(defpackage :snpp
  (:use :lisp :clos :excl :acl-socket)
  (:export #:send-page))

(in-package :snpp)

(defvar *production-mode* t) ;; for debugging make this `nil'

(defvar *response-debug* nil)
(defvar *snpp-init-file* ".snpprc")
(defvar *aliases* nil)
(defvar *usage*
    "~
Usage: snpp [-d] [-n] [-h host] -u user message...
-d        debug mode: print informative messages
          (all by itself, -d runs a primitive top-level)
-n        no-execute mode: don't really send the page
-h host   send to `host'
-u user   send to `user'
message   remaining arguments are the message.

Note: if it exists, ~~/.snpprc is used to store aliases for the -u
      argument.  The format of this file is 0 or more lists of the form
          (alias host user)
      where `alias' is given to -u and `host' and `user' are used as the
      real values of -h and -u.  For example, a .snpprc of:
          (\"devil\" \"hell.com\" 666)
      would allow this command:
          snpp -u devil you suck, dude
      instead of this:
          snpp -u 666 -h hell.com you suck, dude
")

(defun user::main (&aux alias msg)
  (handler-case
      (system:with-command-line-arguments
	  ("dh:nu:" debug-mode host no-execute user) (message)
	(when debug-mode (setq *production-mode* nil))
	(when (or (null message) (null user))
	  (if* debug-mode
	     then (break "hack away, dude...")
	     else (format t *usage*)
		  (exit 0 :quiet t)))
	(setq *response-debug* debug-mode)
	(read-snpp-init-file)
	(setq msg (format nil "~a~{ ~a~}" (car message) (cdr message)))
	(when (and user (setq alias (assoc user *aliases* :test #'string=)))
	  (setq host (second alias))
	  (setq user (third alias))
	  (when debug-mode
	    (format t "alias ~s expands to: ~a@~a~%" (first alias) user host)))
	(if* no-execute
	   then (format t "~s~%" `(send-page ,host ,user ,msg))
	   else (send-page host user msg))
	(exit 0 :quiet t))
    (error (c)
      (if* *production-mode*
	 then (format t "An error occurred: ~a.~%" c)
	      (exit 1 :quiet t)
	 else (error c)))))

(defun read-snpp-init-file ()
  (let ((init-file (merge-pathnames *snpp-init-file* (user-homedir-pathname))))
    (when (probe-file init-file)
      (with-open-file (s init-file :direction :input)
	(do ((e (read s nil s) (read s nil s)))
	    ((eq e s))
	  (push e *aliases*))))))

;; response-class gets a response from the connected program and
;; dispatches on that response.  The variable network-response
;; is bound to the complete response string from the server
;;
;; use:
;;   (response-case (stream)
;;     (2 
;;       (action-for-2xx-response))
;;     (3 
;;	 (action-for-3xx-response))
;;     (t (error "bogus response ~s" network-response)))
;;
(defmacro response-case ((stream) &rest case-clauses)
  (let ((response-class (gensym)))
    `(multiple-value-bind (,response-class network-response)
	 (wait-for-response ,stream)
       (case ,response-class
	 ,@case-clauses))))

(defun send-page (host pager message &key (port 444))
  (let ((sock (make-socket :remote-host host
			   :remote-port port)))
    (unwind-protect
	(progn
	  (response-case (sock)
	    (2 ;; ok
	     nil)
	    (t (error "connect to pager host failed: ~a"
		      network-response)))
	  (format sock "page ~a" pager)
	  (crlf sock)
    
	  (response-case (sock)
	    (2 ;; ok
	     nil)
	    (t (error "pager number illegal: ~a" network-response)))
    
	  (format sock "data")
	  (crlf sock)
    
	  (response-case (sock)
	    (3 ;; ok
	     nil)
	    (t (error "unexpected response after data: ~a"
		      network-response)))
    
	  (send-with-crlf sock message)
	  (crlf sock)
	  (format sock ".")
	  (crlf sock)
    
	  (response-case (sock)
	    (2 ;; ok
	     nil)
	    (t (error "data wasn't accepted: ~a" network-response)))
    
	  (format sock "send")
	  (crlf sock)
    
    
	  (response-case (sock)
	    (2 ;; ok
	     nil)
	    (t (error "Send Failed: ~a" network-response)))
    
	  (format sock "quit")
	  (crlf sock)
	  t)
      (close sock))))


(defun crlf (stream)
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (force-output stream))


    
		   

(defun send-with-crlf (stream string)
  ;; send the given string, but send a crlf when there is just
  ;; a linefeed in the string
  (let (ret-seen)
    (dotimes (i (length string))
      (let ((ch (aref string i)))
	(if* (eq #\return ch)
	   then (setq ret-seen t)
		(write-char ch stream)
	 elseif (eq #\linefeed ch)
	   then (if* ret-seen then (write-char #\return stream))
		(write-char ch stream)
		(setq ret-seen nil)
	   else (write-char ch stream)
		(setq ret-seen nil))))))

(defun wait-for-response (stream)
  ;; read the response of the stream
  ;; collect it all in a string.
  ;; Return two values:
  ;; 	response class
  ;;    whole string
  ;; The string should begin with a decimal digit, and that is converted
  ;; into a number which is returned as the response class.
  ;; If the string doesn't begin with a decimal digit then the
  ;; response class is -1.
  ;;
  (flet ((match-chars (string pos1 pos2 count)
	   ;; like strncmp
	   (dotimes (i count t)
	     (if* (not (eq (aref string (+ pos1 i))
			   (aref string (+ pos2 i))))
		then (return nil)))))
				 
    (let ((res (make-array 20 :element-type 'character 
			   :adjustable t 
			   :fill-pointer 0)))
      (if* (null (read-a-line stream res))
	 then ; eof encountered before end of line
	      (return-from wait-for-response (values -1 res)))
    
      ;; a multi-line response begins with line containing
      ;; a hyphen in the 4th column:
      ;; xyz-  some text
      ;; 
      ;;  and ends with a line containing the same reply code but no
      ;;  hyphen.
      ;; xyz  some text
      ;;
    
      (if* (and (>= (length res) 4) (eq #\- (aref res 3)))
	 then ;; multi line response
	      (let ((old-length (length res))
		    (new-length nil))
		(loop
		  (if* (null (read-a-line stream res))
		     then ; eof encountered before end of line
			  (return-from wait-for-response (values -1 res)))
		  (setq new-length (length res))
		  ;; see if this is the last line
		  (if* (and (>= (- new-length old-length) 4)
			    (eq (aref res (+ old-length 3)) #\space)
			    (match-chars res 0 old-length 3))
		     then (return))
		
		  (setq old-length new-length))))
    
      ;; complete response is in res
      ;; compute class and return the whole thing
      (let ((class (or (and (> (length res) 0)
			    (digit-char-p (aref res 0)))
		       -1)))
	(values class res)))))


(defun read-a-line (stream res)
  ;; read from stream and put the result in the adjust able array res
  ;; if line ends in cr-lf, only put a newline in res.
  ;; If we get an eof before the line finishes, return nil,
  ;; else return t if all is ok
  (let (ch last-ch)
    (loop
      (setq ch (read-char stream nil nil))
      (if* (null ch)
	 then ; premature eof
	      (return nil))
      
      (if* *response-debug* 
	 then (format *response-debug* "~c" ch))
      
      (if* (eq last-ch #\return)
	 then (if* (eq ch #\linefeed)
		 then (vector-push-extend #\newline res)
		      (return t)
		 else (vector-push-extend last-ch res))
       elseif (eq ch #\linefeed)
	 then ; line ends with just lf, not cr-lf
	      (vector-push-extend #\newline res)
	      (return t)
       elseif (not (eq ch #\return))
	 then (vector-push-extend ch res))
    
      (setq last-ch ch))))

;;;Local variables:
;;;eval:(put 'response-case 'fi:common-lisp-indent-hook 1)
;;;End:
