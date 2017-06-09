;; $Id: ftp.cl,v 2.12 2009/04/20 18:58:13 layer Exp $
;;
;; ftp client for lisp
;;
;; copyright (c) 1996-2015 Franz Inc, Oakland, CA - All rights reserved.

#|
- implement STOU (store unique)?
- add support for REST command?
- suggestions from jkf:
  It would be useful to let the user know what kind of error occurred
  if a connection to an ftp server failed.  It seems there are four kinds of
  errors:
  1.  bogus hostname.   retrying isn't going to be useful
  2.  connection refused.  retrying a few times may be useful
  3.  user/password not accepted.   retrying isn't going to be useful
  4.  server too full.   retrying is going to be useful
|#

(eval-when (compile eval load)
  (require :resource)
  (require :sock)
  (require :acldns)
  (require :uri))

;; for compatibility:
(defpackage :socket (:export #:open-ftp-stream #:ftp-transfer-file))

;; V2 of this module uses this new package:
(defpackage :net.ftp.client
  (:use #:excl #:common-lisp)
  (:import-from :acl-socket #:open-ftp-stream #:ftp-transfer-file)
  (:export
   #:*default-user*
   #:*default-password*
   
   #:failed-connection
	   
   #:with-open-ftp-connection
   #:connect-to-ftp-server
   #:open-ftp-stream
   #:ftp-transfer-file
   #:ftp-get
   #:ftp-stream-get
   #:ftp-put
   #:ftp-stream-put
   #:ftp-append
   #:ftp-stream-append
   #:ftp-stream-cwd
   #:ftp-mkdir
   #:ftp-stream-mkdir
   #:ftp-rmdir
   #:ftp-stream-rmdir
   #:ftp-delete
   #:ftp-stream-delete
   #:ftp-rename
   #:ftp-stream-rename
   #:ftp-size
   #:ftp-stream-size
   #:ftp-chmod
   #:ftp-stream-chmod
   #:ftp-stream-umask
   #:ftp-file-mod-time
   #:ftp-stream-file-mod-time

   #:ftp-walk				; deprecated, use:
   #:ftp-date-string-to-ut		; removed (calls error)
   #:map-over-ftp-directory))

(provide :ftp #+module-versions 2.1)

(in-package :net.ftp.client)

;; [rfe6329]
#+process-autoloads
(excl::autoload-from "code/sock-s.cl"
		     open-ftp-stream ftp-transfer-file)

(defvar *default-user* "anonymous")
(defvar *default-password* "anonymous@somewhere")

(defmacro with-ftp-response ((ftp-stream ftp-response) &rest case-clauses)
  (let ((response-class (gensym)))
    `(multiple-value-bind (,response-class ,ftp-response)
	 (wait-for-response ,ftp-stream)
       (case ,response-class
	 ,@case-clauses))))

(defvar *ftp-debug* nil)

(defmacro with-open-ftp-connection ((var host
				     &key port
					  (user *default-user*)
					  (password *default-password*))
				    &body body)
  "with-open-ftp-connection establishes a connection to an FTP server on
`host' and `port', using `user' and `password' to authenticate the
session.

The stream bound to `var' is closed after `body' is evaluated, and the
value returned is that returned by `body'.

See the documentation for connect-to-ftp-server for more information on
`host', `port', `user' and `password'.

The value of opening a connection to an FTP server using this macro is that
multiple commands can be sent to the same server without having to close
and reopen the connection.  When using a heavily used FTP server it might
be quite difficult to make a connection, so having to reestablish it might
not be a good idea.

Functions in this package whose names start with ``ftp-stream-'' operate on
open FTP sessions."
  `(with-open-stream (,var (connect-to-ftp-server ,host
						  :user ,user
						  :password ,password
						  :port ,port))
     ,@body))

(define-condition failed-connection () ())

(defun connect-to-ftp-server (host &key (user *default-user*)
					(password *default-password*)
					port
					debug
			      &aux (*ftp-debug* debug))
  "Create and return a stream connected to an FTP server on `host' and
`port', using `user' and `password' to authenticate the session.  The
default values of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

If an error occurs while establishing the connection the stream is closed
and an error is signalled."
  (let ((ftpd (socket:make-socket
	       :remote-host host
	       :remote-port (or port
				;; On Windows (at least some versions),
				;; there is no services file, which is
				;; where the mapping from "ftp" to 21
				;; exists.  So, there, just hardware it so
				;; -1 isn't used.
				#+mswindows 21
				#-mswindows "ftp")))
	(ok nil))
    (unwind-protect
	(progn
	  ;; check initial message from ftp server..
	  ;; It may be too busy to talk to us
	  (with-ftp-response (ftpd response)
	    (2 ;; success
	     nil)
	    (t (error 'failed-connection
		      :format-control "ftp ~s responded ~a"
		      :format-arguments (list host response))))

	  (ftp-command ftpd "user ~a" user)
	  (with-ftp-response (ftpd response)
	    (2 ;; we're in with no password
	     nil)
	    (3 ;; need password
	     (ftp-command ftpd "pass ~a" password)
	     (with-ftp-response (ftpd response)
	       (2 ;; we're in
		nil)
	       (3 ;; need acct,,, we don't know what that is
		(error 'failed-connection 
		       :format-control "ftp daemon needs acct info"))
	       (t (error 'failed-connection
			 :format-control "ftp ~s doesn't like the password: ~a"
			 :format-arguments (list host response)))))
	    (t (error 'failed-connection
		      :format-control
		      "ftp ~s doesn't like the user name: ~a: ~a"
		      :format-arguments (list host user response))))
	  (setq ok t)	;; all is well.. do don't close the port
	  ftpd)
      (if* (not ok) then (close ftpd)))))

(defun open-ftp-stream (file
			&key (host "localhost")
			     port
			     (user *default-user*)
			     (password *default-password*)
			     (direction :input)
			     (mode :binary)
			     (passive t)
			     debug
			&aux (*ftp-debug* debug))
  "Opens and returns a stream to or from a file on a remote host.

The name of the host is `host' using port `port'.  `user' and `password'
are used for authentication.  The default values of `user' and `password'
are taken from *default-user* and *default-password*.

`direction' is either :input, the default, :output or :directory.
A direction value of `:directory' causes the return of an input stream
containing a directory listing.

`host' and `port' default to `localhost' and the `ftp' service port.

`mode' can be either :binary, the default, or :text.  Note that in :text
mode lines end in CRLF (the standard on the Internet).

`passive', if non-nil, the default, causes passive mode transfers to occur,
which is useful to get through some IP filters and firewalls."
  (let ((ftpd (connect-to-ftp-server host :user user :password password
				     :port port)))
    (unwind-protect
	(if* passive
	   then (passive-ftp-stream ftpd file direction mode)
	   else (active-ftp-stream ftpd file direction mode))
      (close ftpd))))

(defun ftp-transfer-file (&key from-host from-port from-file
			       (from-user *default-user*)
			       (from-password *default-password*)

			       to-host to-port to-file
			       (to-user *default-user*)
			       (to-password *default-password*)

			       (mode :binary)
			       debug
			  &aux (*ftp-debug* debug))
  "Transfer `from-file' on host `from-host' to the file named `to-file' on
host `to-host'.  `from-port' and `to-port' are used to change the default
ports for FTP connections.  `from-user' and `to-user', and `from-password'
and `to-password', are used to set authentication for each connection.
The default values of both `user' and `password' keywords are taken from
*default-user* and *default-password*.

`mode' can be either :binary, the default, or :text.  Note that in :text
mode lines end of CRLF (the standard on the Internet).

The files on each host can have different names.

Note that the host on which this is executed can be different than either
the `from-host' or `to-host', making for a 3-host FTP file transfer."
  (let (to-ftpd from-ftpd)
    (unwind-protect
	(progn
	  (setq to-ftpd (connect-to-ftp-server to-host :user to-user
					       :password to-password
					       :port to-port)
		from-ftpd (connect-to-ftp-server from-host :user from-user
						 :password from-password
						 :port from-port))

	  ;; set them both in the mode
	  (ftp-type-command to-ftpd mode)
	  (ftp-type-command from-ftpd mode)

	  ;; make the 'to' machine be passive
	  (multiple-value-bind (to-ipaddr to-port) (ftp-pasv-command to-ftpd)

	    ;; tell the from machine where to contact the to machine.
	    (ftp-port-command from-ftpd to-ipaddr to-port)

	    ;; start the transfer on each side
	    (ftp-transfer-command from-ftpd (normalize-ftp-file-arg from-file)
				  :input)

	    (ftp-transfer-command  to-ftpd (normalize-ftp-file-arg to-file)
				   :output)

	    (with-ftp-response (from-ftpd response)
	      (2 nil) ;; OK
	      (t (error "ftp from failed: ~a" response)))

	    (with-ftp-response (to-ftpd response)
	      (2 nil) ;; OK
	      (t (error "ftp to failed: ~a" response)))
	    nil))

      ;; cleanup forms:
      (if* from-ftpd then (close from-ftpd))
      (if* to-ftpd   then (close to-ftpd)))))

(defun ftp-get (host remote-path local-filespec
		&key port
		     (if-exists :error)
		     (user *default-user*)
		     (password *default-password*)
		     (mode :binary)
		     debug
		&aux (*ftp-debug* debug))
  "Retrieve a file from a remote host into a local file.  The connection is
made to `host' and `port' using `user' and `password' for authentication.
The default values of `user' and `password' are taken from *default-user*
and *default-password*.

If `port' is not given then it defaults to the `ftp' service port.

`remote-path' is relative to the current directory in effect on the remote
host.

`local-filespec' is the destination for the retrieval.

`mode' can be either :binary, the default, or :text.  Note that in :text
mode lines end of CRLF (the standard on the Internet).

On success `t' is returned."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-get ftpd remote-path local-filespec
		    :if-exists if-exists :mode mode
		    :debug debug)))

(defun ftp-stream-get (ftps remote-path local-filespec
		       &key (if-exists :error)
			    (mode :binary)
			    debug
		       &aux (*ftp-debug* debug))
  "Retrieve a file from a remote host into a local file.  This function
is similar to ftp-get, except that it uses an already open FTP
connection (see with-open-ftp-connection or connect-to-ftp-server) given by
`ftps'.

`remote-path', `local-filespec', `if-exists' and `mode' have the same
meaning as for ftp-get.

On success `t' is returned."
  (setq remote-path (normalize-ftp-file-arg remote-path))
  (setq local-filespec (pathname local-filespec))
  (multiple-value-bind (ipaddr port) (ftp-pasv-command ftps)
    (ftp-type-command ftps mode)
    (let ((s (socket:make-socket :remote-host ipaddr
				 :remote-port port
				 :format :binary))
	  (orig-perms
	   (when (probe-file local-filespec)
	     (excl::filesys-permission (namestring local-filespec)))))
      (ftp-command ftps "retr ~a" (normalize-ftp-file-arg remote-path))
      (with-ftp-response (ftps response)
	((1 2) ;; transfer about to start
	 nil)
	(t (error "ftp get command failed: ~a" response)))
      (with-open-file (out local-filespec :direction :output
		       :if-exists if-exists)
	(loop with buf = (make-array 4096 :element-type '(unsigned-byte 8))
 	    as len = (read-sequence buf s)
 	    while (> len 0)
 	    do (write-sequence buf out :end len)))
      (when orig-perms
	(excl::filesys-chmod (namestring local-filespec) orig-perms))
      (close s)
      (with-ftp-response (ftps response)
	(2 nil) ;; OK
	(t (error "ftp failed: ~a" response)))
      t)))

(defun ftp-put (host local-filespec remote-path
		&key port
		     (user *default-user*)
		     (password *default-password*)
		     (mode :binary)
		     debug
		&aux (*ftp-debug* debug))
  "Copy a local file to a remote host.  The connection is made to `host'
and `port' using `user' and `password' for authentication.  The default
values of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

`remote-path' is relative to the current directory in effect on the remote
host.

`mode' can be either :binary, the default, or :text.  Note that in :text
mode lines end of CRLF (the standard on the Internet).

On success `t' is returned."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-put ftpd local-filespec remote-path :mode mode
		    :debug debug)))

(defun ftp-stream-put (ftps local-filespec remote-path
		       &key (mode :binary)
			    debug
		       &aux (*ftp-debug* debug))
  "Copy a local file to a remote host.  This function is similar to
ftp-put, except that it uses an already open FTP connection (see
with-open-ftp-connection or connect-to-ftp-server) given by `ftps'.

`remote-path', `local-filespec' and `mode' have the same meaning as for
ftp-put.

On success `t' is returned."
  (setq local-filespec (pathname local-filespec))
  (setq remote-path (normalize-ftp-file-arg remote-path))
  (multiple-value-bind (ipaddr port) (ftp-pasv-command ftps)
    (ftp-type-command ftps mode)
    (let ((s (socket:make-socket :remote-host ipaddr
				 :remote-port port
				 :format :binary)))
      (ftp-command ftps "stor ~a" (normalize-ftp-file-arg remote-path))
      (with-ftp-response (ftps response)
	((1 2) ;; transfer about to start
	 nil)
	(t (error "ftp stor command failed: ~a" response)))
      (with-open-file (in local-filespec :direction :input)
	(loop as b = (read-byte in nil nil)
	    while b
	    do (write-byte b s)))
      (close s)
      (with-ftp-response (ftps response)
	(2 nil) ;; OK
	(t (error "ftp failed: ~a" response)))
      t)))

(defun ftp-append (host local-filespec remote-path
		   &key port
			(user *default-user*)
			(password *default-password*)
			(mode :binary)
			debug
		   &aux (*ftp-debug* debug))
  "Append a local file to a file on a remote host.  The connection is made
to `host' and `port' using `user' and `password' for authentication.  The
default values of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

`remote-path' is relative to the current directory in effect on the remote
host.

`mode' can be either :binary, the default, or :text.  Note that in :text
mode lines end of CRLF (the standard on the Internet).

On success `t' is returned."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-append ftpd local-filespec remote-path :mode mode
		       :debug debug)))

(defun ftp-stream-append (ftps local-filespec remote-path
			  &key (mode :binary)
			       debug
			  &aux (*ftp-debug* debug))
  "Append a local file to a file on a remote host.  This function is
similar to ftp-append, except that it uses an already open FTP
connection (see with-open-ftp-connection or connect-to-ftp-server) given by
`ftps'.

`remote-path', `local-filespec' and `mode' have the same meaning as for
ftp-append.

On success `t' is returned."
  (setq local-filespec (pathname local-filespec))
  (setq remote-path (normalize-ftp-file-arg remote-path))
  (multiple-value-bind (ipaddr port) (ftp-pasv-command ftps)
    (ftp-type-command ftps mode)
    (let ((s (socket:make-socket :remote-host ipaddr
				 :remote-port port
				 :format :binary)))
      (ftp-command ftps "appe ~a" (normalize-ftp-file-arg remote-path))
      (with-ftp-response (ftps response)
	((1 2) ;; transfer about to start
	 nil)
	(t (error "ftp appe command failed: ~a" response)))
      (with-open-file (in local-filespec :direction :input)
	(loop as b = (read-byte in nil nil)
	    while b
	    do (write-byte b s)))
      (close s)
      (with-ftp-response (ftps response)
	(2 nil) ;; OK
	(t (error "ftp failed: ~a" response)))
      t)))

;; the output by the PWD command is probably too variable for parsing
#+ignore
(defun ftp-stream-pwd (ftps)
  (ftp-command ftps "pwd")
  (with-ftp-response (ftps response)
    (2 nil)
    (t (error "ftp pwd failed: ~a" response)))
  t)

(defun ftp-stream-cwd (ftps remote-path
		       &key debug
		       &aux (*ftp-debug* debug))
  "Change the current working directory in an already open FTP session to
`remote-path'.

On success `t' is returned."
  (setq remote-path (normalize-ftp-file-arg remote-path))
  
  (ftp-command ftps "cwd ~a" remote-path)
  (with-ftp-response (ftps response)
    (2 nil)
    (t (error "ftp cwd failed: ~a" response)))
  t)

(defun ftp-mkdir (host remote-path
		  &key port
		       (user *default-user*)
		       (password *default-password*)
		       debug
		  &aux (*ftp-debug* debug))
  "Make a directory on a remote host.  The connection is made to `host' and
`port' using `user' and `password' for authentication.  The default values
of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

`remote-path' is relative to the current directory in effect on the remote
host.

On success `t' is returned."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-mkdir ftpd remote-path :debug debug)))

(defun ftp-stream-mkdir (ftps remote-path
			 &key debug
			 &aux (*ftp-debug* debug))
  "Make a directory on a remote host.  This function is similar to
ftp-mkdir, except that it uses an already open FTP connection (see
with-open-ftp-connection or connect-to-ftp-server) given by `ftps'.

`remote-path' has the same meaning as for ftp-mkdir.

On success `t' is returned."
  (setq remote-path (normalize-ftp-file-arg remote-path))
  
  (ftp-command ftps "mkd ~a" remote-path)
  (with-ftp-response (ftps response)
    (2 nil)
    (t (error "ftp mkdir failed: ~a" response)))
  t)

(defun ftp-rmdir (host remote-path
		  &key port
		       (user *default-user*)
		       (password *default-password*)
		       debug
		  &aux (*ftp-debug* debug))
  "Remove a directory on a remote host.  The connection is made to
`host' and `port' using `user' and `password' for authentication.  The
default values of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

`remote-path' is relative to the current directory in effect on the remote
host.

On success `t' is returned."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-rmdir ftpd remote-path :debug debug)))

(defun ftp-stream-rmdir (ftps remote-path
			 &key debug
			 &aux (*ftp-debug* debug))
  "Remove a directory on a remote host.  This function is similar to
ftp-rmdir, except that it uses an already open FTP connection (see
with-open-ftp-connection or connect-to-ftp-server) given by `ftps'.

`remote-path' has the same meaning as for ftp-rkdir.

On success `t' is returned."
  (setq remote-path (normalize-ftp-file-arg remote-path))
  
  (ftp-command ftps "rmd ~a" remote-path)
  (with-ftp-response (ftps response)
    (2 nil)
    (t (error "ftp rmdir failed: ~a" response)))
  t)

(defun ftp-delete (host remote-path
		   &key port
			(user *default-user*)
			(password *default-password*)
			debug
		   &aux (*ftp-debug* debug))
  "Delete a file on a remote host.  The connection is made to `host' and
`port' using `user' and `password' for authentication.  The default values
of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

`remote-path' is relative to the current directory in effect on the remote
host.

On success `t' is returned."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-delete ftpd remote-path :debug debug)))

(defun ftp-stream-delete (ftps remote-path
			  &key debug
			  &aux (*ftp-debug* debug))
  "Delete a file on a remote host.  This function is similar to
ftp-delete, except that it uses an already open FTP connection (see
with-open-ftp-connection or connect-to-ftp-server) given by `ftps'.

`remote-path' has the same meaning as for ftp-delete.

On success `t' is returned."
  (setq remote-path (normalize-ftp-file-arg remote-path))
  
  (ftp-command ftps "dele ~a" remote-path)
  (with-ftp-response (ftps response)
    (2 nil)
    (t (error "ftp delete failed: ~a" response)))
  t)

(defun ftp-rename (host remote-from-path remote-to-path
		   &key port
			(user *default-user*)
			(password *default-password*)
			debug
		   &aux (*ftp-debug* debug))
  "Rename a file on a remote host.  The connection is made to `host' and
`port' using `user' and `password' for authentication.  The default values
of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

The name of the remote file `remote-from-path' is changed to
`remote-to-path'.  `remote-from-path' and `remote-to-path' are relative to
the current directory in effect on the remote host.

On success `t' is returned."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-rename ftpd remote-from-path remote-to-path
		       :debug debug)))

(defun ftp-stream-rename (ftps remote-from-path remote-to-path
			  &key debug
			  &aux (*ftp-debug* debug))
  "Rename a file on a remote host.  This function is similar to
ftp-rename, except that it uses an already open FTP connection (see
with-open-ftp-connection or connect-to-ftp-server) given by `ftps'.

`remote-from-path' and `remote-to-path' have the same meaning as for
ftp-rename.

On success `t' is returned."
  (setq remote-from-path (normalize-ftp-file-arg remote-from-path))
  (setq remote-to-path (normalize-ftp-file-arg remote-to-path))
  
  (ftp-command ftps "rnfr ~a" remote-from-path)
  (with-ftp-response (ftps response)
    (3 nil)
    (t (error "ftp rename from failed: ~a" response)))
  
  (ftp-command ftps "rnto ~a" remote-to-path)
  (with-ftp-response (ftps response)
    (2 nil)
    (t (error "ftp rename to failed: ~a" response)))
  t)

(defun ftp-size (host remote-path
		 &key port
		      (user *default-user*)
		      (password *default-password*)
		      debug
		 &aux (*ftp-debug* debug))
  "Return the size of a remote file.  The connection is made to `host' and
`port' using `user' and `password' for authentication.  The default values
of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

`remote-path' is relative to the current directory in effect on the remote
host.

This function uses the SIZE command.  This command is somewhat new--it
is not part of RFC959--and is probably not implemented in all FTP
servers (Allegro FTPd does implement it).  If an error occurs, this
function returns `nil' and, in some cases, a second value which is an
error code from the FTP server.  This second value will probably be
helpful in determining why the SIZE command failed.

On success the size of `remote-path' is returned, and `nil' is returned if
an error occurs or the server cannot determine the size."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-size ftpd remote-path :debug debug)))

(defun ftp-stream-size (ftps remote-path
			&key debug
			&aux (*ftp-debug* debug))
  "Return the size of a remote file.  This function is similar to
ftp-size, except that it uses an already open FTP connection (see
with-open-ftp-connection or connect-to-ftp-server) given by `ftps'.

`remote-path' has the same meaning as for ftp-size.

On success the size of `remote-path' is returned, and `nil' is returned if
an error occurs or the server cannot determine the size."
  (ftp-command ftps "size ~a" remote-path)
  (with-ftp-response (ftps response)
    (2
     (multiple-value-bind (found whole code size)
	 (match-regexp "\\(2[0-9]+\\)[ ]+\\([0-9]+\\)"
		       response)
       (declare (ignore whole))
       (if* (not found)
	  then nil
	elseif (/= 213 (setq code (parse-integer code)))
	  then (values nil code)
	  else (values (parse-integer size)))))
    (t nil)))

(defun ftp-chmod (host mode remote-path
		   &key port
			(user *default-user*)
			(password *default-password*)
			debug
		  &aux (*ftp-debug* debug))
  "Change the mode of a remote file.  The connection is made to `host' and
`port' using `user' and `password' for authentication.  The default values
of `user' and `password' are taken from *default-user* and
*default-password*.

If `port' is not given then it defaults to the `ftp' service port.

`mode' is a number and the following must be true: 0 < mode < #o777.

`remote-path' is relative to the current directory in effect on the remote
host.

On success `t' is returned."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-chmod ftpd mode remote-path :debug debug)))

(defun ftp-stream-chmod (ftps mode remote-path
			 &key debug
			 &aux (*ftp-debug* debug))
  "Change the mode of a remote file.  This function is similar to
ftp-chmod, except that it uses an already open FTP connection (see
with-open-ftp-connection or connect-to-ftp-server) given by `ftps'.

`mode' and `remote-path' have the same meaning as for ftp-chmod.

On success `t' is returned."
  (setq remote-path (normalize-ftp-file-arg remote-path))
  
  (ftp-command ftps "site chmod ~8r ~a" mode remote-path)
  (with-ftp-response (ftps response)
    (2 nil)
    (t (error "ftp site chmod failed: ~a" response)))
  t)

(defun ftp-stream-umask (ftps mode
			 &key debug
			 &aux (*ftp-debug* debug))
  "Change the umask in an already open FTP session to
`remote-path'.

`mode' should be a number, and	he following must be true: 0 < mode < #o777.

On success `t' is returned."
  (ftp-command ftps "site umask ~8r" mode)
  (with-ftp-response (ftps response)
    (2 nil)
    (t (error "ftp site umask failed: ~a" response)))
  t)

(defun ftp-file-mod-time (host remote-path
			  &key port
			       (user *default-user*)
			       (password *default-password*)
			       debug
			  &aux (*ftp-debug* debug))
  "Retrieve the file modification time on `host' and `port' of
`remote-path', using `user' and `password' for authentication of the
session.  The default values of `user' and `password' are taken from
*default-user* and *default-password*.

If `port' is not given then it defaults to the `ftp' service port.

This function uses the MDTM command.  This command is somewhat new--it
is not part of RFC959--and is probably not implemented in all FTP
servers (Allegro FTPd does implement it).  If an error occurs, this
function returns `nil' and, in some cases, a second value which is an
error code from the FTP server.  This second value will probably be
helpful in determining why the MDTM command failed.

On success, the file modification time of the remote file is returned as a
Common Lisp universal time."
  (with-open-stream (ftpd (connect-to-ftp-server host :user user
						 :password password
						 :port port))
    (ftp-stream-file-mod-time ftpd remote-path :debug debug)))

(defun ftp-stream-file-mod-time (ftps remote-path
				 &key debug
				 &aux (*ftp-debug* debug))
  "Retrieve the file modification time on `remote-path' using an already
open connection to an FTP server `ftps'.  For more information see the
ftp-file-mod-time function."
  (ftp-command ftps "mdtm ~a" (namestring remote-path :syntax :unix))
  (with-ftp-response (ftps response)
    (2
     (multiple-value-bind (found whole code year month day hour minute
			   second)
	 (match-regexp
	  #.(concatenate 'simple-string
	      "\\(2[0-9]+\\)[ ]+"
	      "\\([0-9][0-9][0-9][0-9]\\)" ; YYYY
	      "\\([0-9][0-9]\\)"	; MM
	      "\\([0-9][0-9]\\)"	; DD
	      "\\([0-9][0-9]\\)"	; hh
	      "\\([0-9][0-9]\\)"	; mm
	      "\\([0-9][0-9]\\)"	; ss
	      )
	  response)
       (declare (ignore whole))
       (if* (not found)
	  then nil
	elseif (/= 213 (setq code (parse-integer code)))
	  then (values nil code)
	  else (setq year (parse-integer year))
	       (setq month (parse-integer month))
	       (setq day (parse-integer day))
	       (setq hour (parse-integer hour))
	       (setq minute (parse-integer minute))
	       (setq second (parse-integer second))
	       (encode-universal-time second minute hour day month year 0))))
    (t nil)))

(defun ftp-walk (uri function &key (recurse t)
				   (user *default-user*)
				   (password *default-password*))
  "This function has been deprecated.  Use map-over-ftp-directory instead."
  (map-over-ftp-directory function uri
			  :recurse recurse
			  :user user
			  :password password))

(defun ftp-date-string-to-ut (date)
  "This function has been removed since it is no longer necessary.
map-over-ftp-directory passes the already converted date in universal time
format to the map function."
  (declare (ignore date))
  (error "This function has been removed since it is ~
no longer necessary.  map-over-ftp-directory passes the already ~
converted date in universal time format to the map function."))

(defun map-over-ftp-directory (function host remote-path
			       &key (recurse t)
				    port
				    (user *default-user*)
				    (password *default-password*)
				    include-directories
				    debug
				    ignore-mtime
			       &aux (*ftp-debug* debug))
  "Call `function' on each directory entry on a remote host.
The connection is made to `host' and `port' using `user' and `password' for
authentication.  The default values of both `user' and `password' keywords
are taken from *default-user* and *default-password*.

`function' is given three arguments, the filename, the length of the file
and a universal time corresponding to the last modification time of the
file.  If the `ignore-mtime' keyword is non-nil, then the modification time
is always `nil'.  This is useful when traversing large remote directories,
since a round trip to the remote FTP server is needed for each file.

If `include-directories' is non-nil, then directories will be given to
`function' as well.  However, their length and modification time will be
`nil'.

If `recurse' is non-nil, the default, then descent into subdirectories will
occur.

This function return no values."
  (setq remote-path (normalize-ftp-file-arg remote-path))
  (unless (eq (car (pathname-directory remote-path)) :absolute)
    (error "Remote path must be absolute: ~a" remote-path))
  (setq remote-path
    ;; bad things happen if the directory doesn't have a trailing slash
    (namestring (excl::pathname-as-directory remote-path)))
  (map-over-ftp-directory-1 host port user password remote-path recurse
			    include-directories function ignore-mtime debug)
  (values))

;; These regexp patterns work for some ftp servers, and are not necessarily
;; universal.

(defparameter .base-regexp.
    (concatenate 'simple-string
      ".........[ ]+"			; 9 chars for mode
      "[0-9]+[ ]+"			; links
      "[a-zA-Z0-9]+[ ]+"		; uid
      "[a-zA-Z0-9]+[ ]+"		; gid
      "\\(.+\\)[ ]+"			; len
      ".+[ ]+.+[ ]+.+[ ]+"		; date
      "\\([^
]+\\)"			; name
      ))

(defparameter .dir-regexp.
    (compile-regexp (concatenate 'simple-string "^d" .base-regexp.)))

(defparameter .file-regexp.
    (compile-regexp (concatenate 'simple-string "^-" .base-regexp.)))

(excl::defresource directory-buffer
  :constructor (lambda ()
		 (make-array #.(* 3 8192) :element-type 'character
			     :adjustable t :fill-pointer 0))
  :reinitializer (lambda (x) (setf (fill-pointer x) 0)))

(defun map-over-ftp-directory-1 (host port user password directory recurse
				 include-directories file-continuation
				 ignore-mtime debug)
  (labels
      ((collect-text (stream dirbuf)
	 (loop with ret = dirbuf
	     as c = (read-char stream nil nil)
	     while c
	     do (vector-push-extend c ret)
	     finally (return ret)))
       
       (directory-listing (host port dir)
	 (with-open-stream
	     (s (open-ftp-stream dir :direction :directory
				 :host host :port port
				 :user user :password password
				 :passive t :debug debug))
	   (excl::with-resource (dirbuf directory-buffer)
	     (collect-text s dirbuf))))
       (split-lines (str)
	 (with-input-from-string (s str)
	   (loop as line = (read-line s nil nil)
	       while line collect line)))

       (parse-directory-page (pathname str)
	 (do* ((lines (split-lines str) (cdr lines))
	       (line (car lines) (car lines))
	       (directories '())
	       (files '())
	       dirp dirname
	       filep len filename
	       ignore)
	     ((null lines) (values directories files))
	   (declare (ignore ignore))
	   (multiple-value-setq (dirp ignore ignore dirname)
	     (match-regexp .dir-regexp. line))
	   (multiple-value-setq (filep ignore len filename)
	     (match-regexp .file-regexp. line))
	   (when (and dirp (null (or (string= "." dirname)
				     (string= ".." dirname))))
	     (push (merge-pathnames
		    (make-pathname :directory (list :relative dirname))
		    pathname)
		   directories))
	   (when filep
	     (push (list (merge-pathnames (make-pathname :name filename)
					  pathname)
			 len)
		   files)))))
    (multiple-value-bind (subdirectories files)
	(parse-directory-page directory
			      (directory-listing host port directory))
      (with-open-ftp-connection (ftpd host :user user
				      :password password
				      :port port)
	(loop for (file len) in files
	    do (funcall file-continuation
			file
			(when len (ignore-errors (parse-integer len)))
			(if* ignore-mtime
			   then nil
			   else (ftp-stream-file-mod-time ftpd file))))
	(when include-directories
	  (dolist (subdirectory subdirectories)
	    (funcall file-continuation subdirectory nil nil))))
      
      (when recurse
	(dolist (subdirectory subdirectories)
	  (map-over-ftp-directory-1
	   host port user password subdirectory recurse include-directories
	   file-continuation ignore-mtime debug))))))

(defun active-ftp-stream (ftpd file direction mode)
  ;; this is the normal (non-passive) way ftp works:
  ;; the client announces a port and the server connects to it regardless
  ;; of which way the data is travelling.
  ;; The problem with this is that some companies have gateways and
  ;; firewalls that prevent outsiders from connecting to ports on machines
  ;; on the inside.  Thus there is a passive mode (see passive-ftp-stream)
  ;;
  ;; This function sets up a stream to read or write the remote file.
  ;; direction is :input or :output
  ;; mode is :text or :binary
  ;;
  (let ((datap (socket:make-socket :connect :passive)))
    (unwind-protect
	(progn

	  ;; tell the remote site about the our port
	  ;; Since the local-host of a passive socket (e.g. datap)
	  ;; has no useful info we'll use the local-host value from ftpd.
	  ;;

	  (ftp-port-command ftpd
			    (socket:local-host ftpd)
			    (socket:local-port datap))

	  ;; specify the type
	  (ftp-type-command ftpd mode)

	  ;; now retrieve or store the file
	  (ftp-transfer-command ftpd file direction)

	  ;; ftp daemon should be connecting to us
	  ;; we'll accept the connection and then close off the passive
	  ;; socket, leaving only the socket to ftp's control connection
	  ;; up and running
	  (socket:accept-connection datap))
      (close datap))))

(defun passive-ftp-stream (ftpd file direction mode)
  ;; do a passive mode file transfer
  ;; In this mode the server announces a port and we connect to it
  ;;
  (unwind-protect
      (multiple-value-bind (rem-ipaddr rem-port)
	  (ftp-pasv-command ftpd)

	;; specify the file type
	(ftp-type-command ftpd mode)
	(ftp-transfer-command ftpd file direction
			      (lambda ()
				(socket:make-socket :remote-host rem-ipaddr
						    :remote-port rem-port))))
    (close ftpd)))

(defun ftp-port-command (ftpd ipaddr port)
  ;; specify a port on which the remote machine will contact us
  (ftp-command ftpd
	       "port ~d,~d,~d,~d,~d,~d"
	       (logand #xff (ash ipaddr -24))
	       (logand #xff (ash ipaddr -16))
	       (logand #xff (ash ipaddr -8))
	       (logand #xff ipaddr)

	       (logand #xff (ash port -8))
	       (logand #xff port))
  (with-ftp-response (ftpd response)
    (2 ;; ok!
     nil)
    (t (error "ftpd server didn't like port command: ~a" response))))

(defun ftp-pasv-command (ftpd)
  ;; issue a 'passive' command and return two values:
  ;; the ipaddr and port on the remote host we should contact
  (ftp-command ftpd "pasv")
  (with-ftp-response (ftpd response)
    (2 (parse-ipaddr-port response))
    (t (error "ftp doesn't like the pasv command: ~s" response))))

(defun parse-ipaddr-port (str)
  ;; the string has the form  "..... (h1,h2.h3.h4,p1.p2)"
  ;; where h1 is the upper byte of the ipaddr and so on.
  ;; return two values: the ipaddr and the port
  ;;
  (let ((offset (position #\( str))
	(max (length str))
	(res nil))
    (do ((i (1+ offset) (1+ i))
	 (accum 0)
	 (ch))
	((>= i max))
      (setq ch (aref str i))
      (let ((v (digit-char-p ch)))
	(if* v
	   then (setq accum (+ (* accum 10) v))
	 elseif (eql ch #\,)
	   then (push accum res)
		(setq accum 0)
	 elseif (eql ch #\))
	   then (push accum res)
		;; all done:
		(return))))
    (if* (not (eql (length res) 6))
       then (error "can't find the ipaddr and port in this response: ~s"
		   str))
    ;; at this point res is reversed
    (values
     (+ (nth 2 res)
	(ash (nth 3 res) 8)
	(ash (nth 4 res) 16)
	(ash (nth 5 res) 24))
     (+ (nth 0 res)
	(ash (nth 1 res) 8)))))


(defun ftp-type-command (ftpd mode)
  ;;
  ;; issue the ftp command to set type of the transfer
  ;;
  (ftp-command ftpd "type ~a" (ecase mode
				(:text "a")
				(:binary "i")))
  (with-ftp-response (ftpd response)
    (2 ;; ok
     nil)
    (t (error "ftp server's response to type command: ~a" response))))

(defun ftp-transfer-command (ftpd file direction &optional action)
  ;;
  ;; issue the command to initiate the transfer.
  ;; optionally run an action between the sending of the command
  ;; and the checking of the result code.  This is necessary to
  ;; do a passive transfer since the control connnection wants to see
  ;; the connect before it will issue a result code.
  ;;
  (ftp-command ftpd "~a ~a"
	       (ecase direction
		 (:input "retr")
		 (:output "stor")
		 (:directory "list"))
	       (normalize-ftp-file-arg file))
  (prog1 (if* action
	    then (funcall action))

    (with-ftp-response (ftpd response)
      ((1 2)
       ;; transfer about to start
       nil)
      (t (error "ftp transfer command failed: ~a" response)))))

(defun wait-for-response (stream)
  ;; read the response of the ftp server.
  ;; collect it all in a string.
  ;; Return two values:
  ;; 	response class
  ;;    whole string
  ;; The string should begin with a decimal digit, and that is converted
  ;; into a number which is returned as the response class.
  ;; If the string doesn't begin with a decimal digit then the
  ;; response class is -1.
  ;;
  (flet ((read-a-line (stream res)
	   ;; read from stream and put the result in the adjust able array res
	   ;; if line ends in cr-lf, only put a newline in res.
	   ;; If we get an eof before the line finishes, return nil,
	   ;; else return t if all is ok
	   (let (ch last-ch)
	     (loop
	       (setq ch (read-char stream nil nil))
	       (if* (null ch)
		  then ;; premature eof
		       (return nil))

	       (if* *ftp-debug*
		  then (format *ftp-debug* "~c" ch))

	       (if* (eq last-ch #\return)
		  then (if* (eq ch #\linefeed)
			  then (vector-push-extend #\newline res)
			       (return t)
			  else (vector-push-extend last-ch res))
		elseif (eq ch #\linefeed)
		  then ;; line ends with just lf, not cr-lf
		       (vector-push-extend #\newline res)
		       (return t)
		elseif (not (eq ch #\return))
		  then (vector-push-extend ch res))

	       (setq last-ch ch))))
	 
	 (match-chars (string pos1 pos2 count)
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

(defun ftp-command (stream &rest format-args)
  ;; send a command to the ftp server
  (let ((command (apply #'format nil format-args)))
    (if* *ftp-debug*
       then (format *ftp-debug* "to ftp command: ~s~%" command))
    (write-string command stream)
    (write-char #\return stream)
    (write-char #\newline stream)
    (force-output stream)))

(defun normalize-ftp-file-arg (file)
  (when (not (or (stringp file) (pathnamep file)))
    (error "Bad file argument: ~s." file))
  (substitute #\/ #\\ (princ-to-string file)))
