;; copyright (c) 1998-2002 Franz Inc, Berkeley, CA  - All rights reserved.
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
;; A program to check for broken links in HTML documents.  It recursively
;; descends into locally referenced file documents, too.  It keeps a table
;; of visited documents, so it won't get into a loop.

(in-package :user)

(defvar *checklinks-version* "1.53")

(eval-when (compile)
  (when (and (boundp '*debug*) *debug*)
    (push :debug-checklinks-module *features*)))

(eval-when (compile eval load)
  (require :uri)
  (use-package :net.uri)
  (require :phtml (if* (probe-file "sys:code;phtml.fasl")
		     thenret
		     else "../xmlutils/phtml.fasl"))
  (require :aserve (if* (probe-file "sys:code;aserve.fasl")
		      thenret
		      else "../aserve/aserve.fasl")))

;; Some pretty intense string stuff going on here, so crank up the speed:
#-debug-checklinks-module
(declaim (optimize (speed 3)))
#+debug-checklinks-module
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *timeout*
    #+debug-checklinks-module 300000
    #-debug-checklinks-module 30)

(defvar *default-http-protocol*
    ;; work around bugs in chunking.
    ;; change to :http/1.1 to see the bug.
    :http/1.1)
(defvar *default-headers*
    ;; The following is because some sites won't allow you to
    ;; connect unless it looks like you're coming in from a
    ;; browser.
    '(("User-Agent" . "Mozilla/4.61 [en] (WinNT; U)")))

(defvar *production-mode*
    #+debug-checklinks-module nil #-debug-checklinks-module t)

(defvar *verbose* nil)

(defvar *check-all-links* nil
  "When given the -a command line argument, verify even http: links.")

(defvar *usage*
    "~
Usage: checklinks -r directory file-or-url...
-2    during the orphan finding pass (see -O), ignore non-HTML files
-A host1,host2,...
      allow specific hosts
-a    connect to remote sites to check that links exists
-c directory
      the cgi-bin directory, for checking existence of cgi scripts
-D    debug mode: print informative messages
      (all by itself, -D runs a primitive top-level)
-h host1,host2,...
      ignore specific hosts (see -a)
-i directory
      the icons directory, for checking existence of icon files
-l    follow links on the current host only
-O    do not execute the pass to find orphans
-o orphans-reference
      the file name argument is taken to be a list of relative pathnames
      with respect to the document root, and any file in this list which is
      not visited during the link check will be reported as an orphan
-r doc-root
      the document root, required
-v    verbose output (print names of files as they are scanned)
-x file
      exclude file from scan
-X dir1,dir2,...
      directories to ignore
-Y dir1,dir2,...
      directories to ignore only in the orphan finding pass
-Z uri
      A URI at which full debugging should be turned on.  Requires
      debugging code to be built into the program to fully work.
file1 file2 ...
      check the given filename arguments for bad links
")

(defvar .status.)

(defvar *break-on-bad-links* nil)

(defvar *local-only* nil)
(defvar *ignore-hosts* nil)
(defvar *allow-hosts* nil)
(defvar *debug-on-uri* nil)
(defvar *orphans-reference* nil)
(defvar *link-counter* nil)
(defvar *last-visited* nil)

(defun main ()
  (flet
      ((doit ()
	 (system:with-command-line-arguments
	     (("2" :short pass2-ignore-non-html)
	      ("A" :short allow-hosts :required)
	      ("a" :short check-all)
	      ("c" :short cgi-bin :required)
	      ("D" :short debug-mode)
	      ("h" :short ignore-hosts :required)
	      ("i" :short icons-dir :required)
	      ("l" :short local-only)
	      ("o" :short orphans-reference-file :required)
	      ("O" :short ignore-orphans)
	      ("r" :short doc-root :required)
	      ("s" :short single-file)
	      ("v" :short verbose)
	      ("V" :short last-visited :required)
	      ("x" :short ignore-files :required)
	      ("X" :short ignore-directories :required)
	      ("Y" :short pass2-ignore-directories :required)
	      ("Z" :short debug-on-uri :required))
	     (files :usage *usage*)
	   (when cgi-bin (initialize-cgi-bin cgi-bin))
	   (when icons-dir (initialize-icons-dir icons-dir))
	   (when debug-mode
	     (setq *production-mode* nil)
	     (setq *trace-print-length* nil)
	     (setq *trace-print-level* nil)
	     (trace check-links check-remote-uri-1))
	   (when debug-on-uri
	     (format t "debug on uri: ~s~%" *debug-on-uri*))
	   (when allow-hosts
	     (setq allow-hosts
	       (delimited-string-to-list allow-hosts #\,)))
	   (when ignore-hosts
	     (setq ignore-hosts
	       (delimited-string-to-list ignore-hosts #\,)))
	   (when last-visited
	     (setq *last-visited* (open last-visited :direction :output
					:if-exists :supersede
					:if-does-not-exist :create)))
	   (when orphans-reference-file
	     (with-open-file (s orphans-reference-file)
	       (let (line)
		 (setq *orphans-reference*
		   (make-hash-table :size 7001 :test #'equal))
		 (loop
		   (setq line (read-line s nil s))
		   (when (eq line s) (return))
		   (setf (gethash line *orphans-reference*)
		     t)))))
	   (when (null files)
	     (if* debug-mode
		then (break "hack away, dude...")
		else (format t *usage*)
		     (exit 0 :quiet t)))
	   (when (null doc-root)
	     (error "You must specify a document root."))
	   (when (and ignore-orphans (null *orphans-reference*))
	     (error "Orphans file (-o) not specified."))
	   (setq doc-root       (parse-uri doc-root)
		 *link-counter* 0
		 .status.       0)
	   (let* ((ignore-files 
		   (when ignore-files
		     (delimited-string-to-list ignore-files #\,)))
		  (ignore-directories
		   (when ignore-directories
		     (delimited-string-to-list ignore-directories #\,)))
		  (pass2-ignore-directories
		   (when pass2-ignore-directories
		     (delimited-string-to-list pass2-ignore-directories #\,)))
		  (args `(:document-root ,doc-root
			  :local-only ,local-only
			  :allow-hosts ,allow-hosts
			  :find-orphans ,(if* *orphans-reference*
					    then t
					    else nil)
			  :ignore-files ,ignore-files
			  :ignore-directories ,ignore-directories
			  :pass2-ignore-directories ,pass2-ignore-directories
			  :ignore-hosts ,ignore-hosts
			  :debug-on-uri ,debug-on-uri
			  :verbose ,verbose
			  :single-file ,single-file
			  :check-all ,check-all
			  :pass2 ,(when pass2-ignore-non-html
				    :ignore-non-html))))
	     (apply #'check-links files args))
	   (format t "~&~%Total documents checked: ~D.~%" *link-counter*)
	   (when *last-visited* (close *last-visited*))
	   (exit .status. :quiet t))))

    #+debug-checklinks-module (doit)
    #-debug-checklinks-module
    (if* *production-mode*
       then (handler-case (doit)
	      (excl::asynchronous-operating-system-signal (c)
		(handle-sigint c))
	      (error (c)
		(format t "An error occurred: ~a~%" c)
		(exit 1 :quiet t)))
       else (doit))))

(defvar *image-file-types*
    '("jpg" "gif" "pdf" "hlp" "png" "txt" "lsp" "Z" "gz" "zip" "el" "cl"
      "lisp" "idl" "cnt" "java" "hh" "cc" "C" "bat" "doc" "fasl" "rpm"
      "exe" "dmg" "bz2" "xml" "wav"))

(defparameter *unprocessed-uris*
    ;; We delay checking URLs we find in documents until the end of the
    ;; document, to curtail recursion a little.  It's probably easier on
    ;; the gc, too, since we can drop the data structures associated with a
    ;; document after processing it.
    nil)

(declaim (special .printed-file-name. .uri. .base. .referer.))

(defvar .document-root-pathname. nil)
(defvar .document-root. nil)

#+debug-checklinks-module
(defvar .debug-output. "checklinks.debug")

(defvar *uri-clos-hackery* nil)

(defvar *client-mode* nil)

(defvar *index-files* '("index.html" "index.htm" "index.lhtml"))

(defun check-links (files
		    &key document-root
			 single-file
			 ignore-files
			 ignore-directories
			 pass2-ignore-directories
			 ((:local-only *local-only*) nil)
			 ((:ignore-hosts *ignore-hosts*) nil)
			 ((:allow-hosts *allow-hosts*) nil)
			 (client-mode (when document-root
					(uri-scheme document-root)))
			 (find-orphans (not client-mode))
			 ((:check-all *check-all-links*) nil)
			 ((:debug-on-uri *debug-on-uri*) nil)
			 ((:verbose *verbose*) nil)
			 (pass2
			  ;; valid values: :ignore-non-html and nil
			  nil)
		    &aux (*global-gc-behavior* :auto)
			 (*print-pretty* nil)
			 (net.uri::*strict-parse* nil)
			 (*uri-clos-hackery* nil)
			 (pass2-result '()))
  (declare (ignore pass2))
  
  ;; Check the HTML links in FILE.  Recursively descend into locally
  ;; referenced file documents, and check links there, too.  Keep a table
  ;; of visited documents, so we won't get into a loop.  The :IGNORE
  ;; argument specifies bad links that we should ignore, and must be a list
  ;; of filenames without path elements.  The :DIRECTORY argument specifies
  ;; a web root that should additionally be checked: currently, files that
  ;; are not pointed to by any HTML document are highlighted.
  
  (assert (uri-p document-root))

  (setq *client-mode* client-mode)
  
  #+debug-checklinks-module
  (setq .debug-output.
    (open .debug-output. :direction :output :if-exists :supersede))

  (setq *unprocessed-uris* nil)
  (setq .status. 0)

  (unintern-uri t)

  (setq .document-root. document-root)
  (setq .document-root-pathname.
    (pathname (or (uri-path document-root) "/")))

  (setq ignore-directories
    (mapcar #'(lambda (s) (merge-uris s document-root))
	    ignore-directories))
  (setq pass2-ignore-directories
    (mapcar #'(lambda (s) (merge-uris s document-root))
	    pass2-ignore-directories))
  
  (setq *uri-clos-hackery* t)
  
  (dolist (file files)
    (check-url (intern-uri file) (list .document-root. 0)
	       :ignore-files ignore-files
	       :recurse (not single-file)
	       :ignore-directories ignore-directories))

  #+debug-checklinks-module (close .debug-output.)
  
  (format t "~&~%Found ~d bad links.~%" .status.)

  (when (and find-orphans (not single-file))
    (format t "~&~%~
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
    (format t ";; Looking for files not referenced anywhere...~%")
    (maphash
     (lambda (filespec ignore)
       (declare (ignore ignore))
       (let* ((uri (merge-uris (parse-uri filespec) .document-root.))
	      (is-index 
	       (dolist (index *index-files*)
		 (when (string= index (file-namestring filespec))
		   (return t))))
	      (uri2
	       (when is-index
		 (merge-uris (parse-uri (path-namestring filespec))
			     .document-root.))))
	 (when (or (and (not is-index)
			(not (visited-uri-p uri)))
		   (and is-index
			(not (visited-uri-p uri))
			(not (visited-uri-p uri2))))
	   (push filespec pass2-result))))
     *orphans-reference*)
    
    (dolist (result (sort pass2-result #'string-lessp))
      (format t "~a~%" result)))

  .status.)

(defun check-url (uri referer
		  &key ignore-files
		       ignore-directories
		       (recurse t)
		  &aux .uri. .base. .printed-file-name. current-file
		       timeout
		       (old-verbose *verbose*)
		       stop-after-this-uri)
  #+debug-checklinks-module
  (when (and *debug-on-uri* (stringp *debug-on-uri*))
    (format t "DEBUG ON URI: ~s~%" *debug-on-uri*)
    (setq *debug-on-uri* (intern-uri *debug-on-uri*)))
  
  (tagbody
   top
    (if* (eq uri *debug-on-uri*)
       then (setq old-verbose *verbose*)
	    (setq *verbose* :all)
	    (setq stop-after-this-uri t)
       else (setq *verbose* old-verbose))

    (setq .uri. uri)
    (setq .base. uri)
    (setq .printed-file-name. nil)
    (setq current-file (my-uri-to-pathname uri))
    (setq .referer. referer)

    (if* (processed-uri-p uri)
       then (go next)
       else (setf (processed-uri-p uri) t)
            (incf *link-counter*))

;;;;TODO: REMOVED WHY???????
    (note-visited-uri uri)
  
    (when (and ignore-files
	       (member (file-namestring current-file) ignore-files
		       :test #'equalp :key #'namestring))
      (go next))

    (when (and ignore-directories
	       (dolist (dir ignore-directories nil)
		 (when (excl::subdirectoryp current-file (uri-path dir))
		   (return t))))
      (go next))

    (when (and (pathname-type current-file)
	       (member (pathname-type current-file) *image-file-types*
		       :test #'string=))
      (if* *client-mode*
	 then (check-remote-uri nil uri .document-root.)
	      (go next)
	 else (if* (probe-file current-file)
		 then (go next)
		 else (error "do we get here???")
		      (note-bad-link nil "Link to a non-existent file"))))

    (when *verbose*
      (format t "~a (ref: ~a)...~%"
	      (enough-uri uri .document-root.)
	      (enough-uri (first referer) .document-root.)))

    (if* *client-mode*
       then (setq timeout
	      (multiple-value-bind (body code headers)
		  (mp:with-timeout (*timeout* :timed-out)
		    ;; Some sites don't allow :head, so try both ways
		    (handler-case
			(net.aserve.client:do-http-request uri
			  :method :head
			  :headers *default-headers*
			  :protocol *default-http-protocol*)
		      (error ()
			(net.aserve.client:do-http-request uri
			  :headers *default-headers*
			  :compress t
			  :protocol *default-http-protocol*))))
		(if* (eq :timed-out body)
		   then #+debug-checklinks-module
			(format t "DEBUG: TIMEOUT: uri=~s~%" uri)
			body
		   else (let ((content-type
			       (cdr (assoc :content-type headers
					   :test #'eq))))
			  (if* (and (eql 200 code)
				    (equal "text/html" content-type))
			     then #+debug-checklinks-module
				  (when (eq :all *verbose*)
				    (format t "DEBUG: uri=~s~%" uri))
				  ;; now, get the body and check it
				  (multiple-value-bind (body code headers)
				      (mp:with-timeout (*timeout* :timed-out)
					(net.aserve.client:do-http-request uri
					  :headers *default-headers*
					  :compress t
					  :protocol *default-http-protocol*))
				    (declare (ignore headers))
				    (if* (eq :timed-out body)
				       then body
				       else (assert (eql 200 code))
					    (with-input-from-string (s body)
					      (check-html
					       s uri
					       :ignore-files ignore-files
					       :ignore-directories
					       ignore-directories))))
			   elseif (not (eql 200 code))
			     then (note-bad-link nil "Bad link: ~a" uri)
			   elseif (or (equal "text/plain" content-type)
				      (equal "text/css" content-type)
				      (equal "application/octet-stream"
					     content-type)
				      (match-regexp
				       (load-time-value
					(compile-regexp "^image/"))
				       content-type))
			     thenret ;; don't check
			     else nil
				  ;; This warning has *never* been useful,
				  ;; so disable it.  10/19/07
				  #+ignore
				  (warn "~
can't scan for links: ~a: code ~d; content-type ~a"
					uri code content-type))))))
	    (if* (eq :timed-out timeout)
	       then (comment nil "Request for ~a timed out" uri)
	       else (note-visited-uri uri))
       else (when (not (file-directory-p current-file))
	      (with-open-file (s current-file :direction :input)
		(note-visited-uri uri)
		(check-html s uri
			    :ignore-files ignore-files
			    :ignore-directories ignore-directories))))
  
   next
    
    ;; We've just finished processing the file, so now is the time to check
    ;; any pending bookmark references we have to it.
    (let (from-uri bookmark)
      (dolist (item (getf (uri-plist uri) :bookmark-references))
	(setq from-uri (car item))
	(setq bookmark (cdr item))
	(when (and
	       (null (member bookmark (bookmarks uri) :test #'string=))
	       (or (null ignore-directories)
		   (dolist (dir ignore-directories t)
		     (when (excl::subdirectoryp
			    (my-uri-to-pathname uri)
			    (uri-path dir))
		       (return nil)))))
	  (note-bad-bookmark nil from-uri uri bookmark)))
      (setf (getf (uri-plist uri) :bookmark-references) nil)
      (setf (bookmarks-defined-p uri) t))
    
    (when stop-after-this-uri
      (format t "Stopping after debug-on-uri...~%")
      (exit 1))

    (when (and recurse *unprocessed-uris*)
      (let ((temp (pop *unprocessed-uris*)))
	(setq uri (first temp))
	(setq referer (second temp)))
      (go top))))

(eval-when (compile eval)
(defmacro parse-protect (position &rest form)
  `(flet ((doit () ,@form))
     (if* *production-mode*
	then (handler-case (doit)
	       (excl::asynchronous-operating-system-signal (c)
		 (handle-sigint c))
	       (parse-error (c)
		 (note-bad-link ,position "problem parsing uri: ~a." c)))
	else (doit))))
)

#+ignore ;; not yet:
(eval-when (compile eval)
(defmacro define-phtml-handler (tag var &body body)
  (let ((fun (intern (format nil "phtml-element-callback-~a" tag))))
    `(progn
       (defun ,fun (,var)
	 ,@body)
       (let ((tags (if* (consp ,tag)
		      then ,tag
		      else (list ,tag))))
	 (dolist (tag tags)
	   (setf (element-callback ,tag) #',fun))))))
)

(defun check-html (s myuri
		   &key ignore-files ignore-directories
		   &aux token type position attributes
			(tokenbuf (net.html.parser::get-tokenbuf))
			(checklist '()))
  (loop
    (tagbody
      (setq position (file-position s))
      (multiple-value-setq (token type)
	(net.html.parser::next-token s t nil nil tokenbuf nil))
      (when (eq :eof type) (return))
      
      (when (or (stringp token) (symbolp token)) (go :next))
      (when (not (consp token)) (error "read token: ~s" token))
      (setq attributes (cdr token))
      (setq token (car token))
      
      #+debug-checklinks-module
      (when *verbose*
	(format t "token ~a, attr ~a~%" token attributes))
      (case token
	(:base
	 (let ((href (second (member :href attributes :test #'eq))))
	   (when href (setq .base. (parse-uri href)))))
	
	((:overlay :fig :bgsound)
	 (check-image token attributes position :src (list myuri position) t))
	
	(:img
	 (check-image token attributes position :src (list myuri position) t)
	 (check-image token attributes position :lowsrc (list myuri position))
	 (check-image token attributes position :dynsrc (list myuri position)))

	((:li :sup :sub :p :ul :ol :div :h1 :h2 :h3 :h4 :h5 :h6)
	 (let ((bookmark (second (member :id attributes :test #'eq))))
	   (when bookmark (note-bookmark bookmark))))
	
	((:a :area :frame :span)
	 (parse-protect
	  position
	  (let ((href (second
		       (member (if* (eq :frame token)
				  then :src
				  else :href)
			       attributes
			       :test #'eq)))
		(bookmark
		 (second (member (if (eq :span token) :id :name)
				 attributes :test #'eq))))
	    (when href
	      #+debug-checklinks-module
	      (and (eq :all *verbose*) (format t "href=~a~%" href))
	      (let ((unmerged-uri (parse-uri href))
		    (uri nil))
		(handler-case
		    (setq uri (merge-uris unmerged-uri .base.))
		  (error ()
		    (note-bad-link position
				   "Bogus uri:~{ ~a~}"
				   ;; make it a list, to fool
				   ;; note-bad-link, which does something
				   ;; special of the first format argument
				   ;; is a uri...
				   (list unmerged-uri))))
		(when uri
		  #+debug-checklinks-module
		  (and (eq :all *verbose*) (format t "uri=~a~%" uri))
		  ;; merge-uris doesn't copy over the plist, so we have to
		  ;; do it by hand:
		  (setf (getf (uri-plist uri) :bookmark-references)
		    (append (getf (uri-plist unmerged-uri)
				  :bookmark-references)
			    (getf (uri-plist uri)
				  :bookmark-references)))
		  (if* (or *client-mode* (uri-scheme unmerged-uri))
		     then #+debug-checklinks-module
			  (and (eq :all *verbose*)
			       (format t "  push on checklist~%"))
			  (push (list position uri (list myuri position))
				checklist)
		     else #+debug-checklinks-module
			  (and (eq :all *verbose*)
			       (format t "  check-filespec~%"))
			  (if* (not (check-filespec uri
						    (list myuri position)
						    :ignore-files ignore-files
						    :ignore-directories
						    ignore-directories))
			     then (note-bad-link position
						 "Broken link[1]: ~a" uri)
			     else (note-link uri (list myuri position)))))))
	    (when bookmark (note-bookmark bookmark)))))

	(:link
	 (check-image token attributes position :href (list myuri position)))
	
	((:body :table)
	 (check-image token attributes position
		      :background (list myuri position))))
     :next))
  
  (dolist (item checklist)
    (check-remote-uri (first item)
		      (second item)
		      (third item))))

(defun check-image (token attributes position key referer &optional must-exist)
  (parse-protect
   position
   (let ((value (second (member key attributes :test #'eq))))
     (when (and must-exist (null value))
       (error "~s tag with no ~s." token key))
     (when value
       (check-image-url (merge-uris value .base.) position referer)))))

(defun check-image-url (uri position referer)
  (if* *client-mode*
     then (check-remote-uri position uri referer)
     else (let ((p (my-uri-to-pathname uri)))
	    (note-visited-uri uri)
	    (when (not (probe-file p))
	      (note-bad-link position "Broken link[2]: ~a" uri)))))

#+ignore
(defun check-java-script (html-buffer start end)
  (let* ((file-regexp
	  (load-time-value
	   (compile-regexp
	    (concatenate 'simple-string
	      "[" '(#\space #\tab #\newline #\return) "]*"
	      "="
	      "[" '(#\space #\tab #\newline #\return) "]*"
	      "\"\\(/[^\"]+\\)\""))))
	 (next-start start))
    (loop
      (multiple-value-bind (match ignore file-positions)
	  (match-regexp file-regexp html-buffer :return :index
			:start next-start :end end)
	(declare (ignore ignore))
	(when (null match) (return))
	(let* ((file (subseq html-buffer (car file-positions)
			     (cdr file-positions)))
	       (uri (merge-uris (parse-uri file) .base.))
	       (p (my-uri-to-pathname uri)))
	  (note-visited-uri uri)
	  (when (not (probe-file p))
	    (note-bad-link
	     (car file-positions)
	     "Javascript code references non-existent file ~a"
	     p)))
	(setq next-start (cdr file-positions))))))

(defun check-filespec (uri referer &key ignore-files ignore-directories)
  (if* (cgi-script-p uri)
     then (cgi-script-exists-p uri)
   elseif (icons-file-p uri)
     then (icons-file-exists-p uri referer)
     else (let ((p (my-uri-to-pathname uri)))
	    (or (and ignore-files
		     (member (file-namestring p) ignore-files
			     :test #'equalp :key #'namestring))
		(and ignore-directories
		     (dolist (dir ignore-directories nil)
		       (when (excl::subdirectoryp p (uri-path dir))
			 (return t))))
		(probe-file p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions

(defun my-uri-to-pathname (uri)
  (if* *client-mode*
     then (pathname (or (uri-path (merge-uris uri .document-root.))
			"/"))
     else ;; If uri is relative, just return a pathname of it.
	  ;; If uri is absolute, then return a pathname relative to
	  ;; document-root.
	  (let* ((s (uri-path uri))
		 (path (uri-parsed-path uri)))
	    (assert (or (consp path) (null path)))
	    (if* (null path)
	       then "/"
	     elseif (eq :absolute (car path))
	       then ;; absolute
		    (concat-pathnames .document-root-pathname. s)
	       else ;; (eq :relative (car path))
		    (namestring s)))))

#+ignore
(defun my-uri-to-pathname (uri)
  ;; If uri is relative, just return a pathname of it.
  ;; If uri is absolute, then return a pathname relative to document-root.
  (let* ((s (uri-path uri))
	 (path (uri-parsed-path uri)))
    (assert (or (consp path) (null path)))
    (if* (null path)
       then "/"
     elseif (eq :absolute (car path))
       then ;; absolute
	    (concat-pathnames .document-root-pathname. s)
       else ;; (eq :relative (car path))
	    (namestring s))))

;; the following two are used to calculate orphans  

(defun note-visited-uri (uri)
  (when *last-visited* (format *last-visited* "~a~%" uri))
  #+debug-checklinks-module
  (when (eq :all *verbose*) (format t "note visited: ~a~%" uri))
  #+debug-checklinks-module (format .debug-output. "~a~%" uri)
  (setf (getf (uri-plist uri) :visited) t))

(defun visited-uri-p (uri)
  (getf (uri-plist uri) :visited))

;; the following three are used in the recursion of html

(defun note-link (uri referer)
  (when (or (and (not *client-mode*)
		 (uri-scheme uri))
	    (and *client-mode*
		 (or (null *allow-hosts*)
		     (not (member (uri-host uri) *allow-hosts*
				  :test #'equalp)))
		 (not (equalp (uri-host uri) (uri-host .document-root.)))))
    #+too-verbose
    (comment nil "Ignoring URL to other host: ~a" uri)
    (return-from note-link))

  (when (and (cgi-script-p uri) (not *client-mode*))
    (when (not (cgi-script-exists-p uri))
      (note-bad-link nil "CGI script does not exist: ~a" uri))
    (return-from note-link))

  (when (icons-file-p uri)
    (when (not (icons-file-exists-p uri referer))
      (note-bad-link nil "icons file does not exist: ~a" uri))
    (return-from note-link))

  (when (not (excl::subdirectoryp (my-uri-to-pathname uri)
				  .document-root-pathname.))
    (comment nil "Ignoring file outside document root: ~a" uri)
    (return-from note-link))

  (push (list uri referer) *unprocessed-uris*))

(defun processed-uri-p (uri)
  (getf (uri-plist uri) :done))

(defun (setf processed-uri-p) (new-val uri)
  (setf (getf (uri-plist uri) :done) new-val))

;; the following are used in bookmark definition and validation

(defun bookmarks-defined-p (uri)
  (getf (uri-plist uri) :bookmarks-defined))

(defun (setf bookmarks-defined-p) (new-val uri)
  (setf (getf (uri-plist uri) :bookmarks-defined) new-val))

(defun note-bookmark-reference (from-uri to-uri bookmark)
  (let ((bookmark (uri-decode-anchor bookmark)))
    ;; href="#" is a special link which takes the user to the top of the
    ;; page and we just ignore it, because it can't be bad.
    (when (string/= "" bookmark)
      #+debug-checklinks-module
      (when *verbose* (format t "noting bookmark ref: ~a~%" bookmark))
      (if* (bookmarks-defined-p to-uri)
	 then ;; we've already processed to-uri, so we know all the bookmarks
	      ;; defined in it.
	      (when (not (member bookmark (bookmarks to-uri) :test #'string=))
		(note-bad-bookmark nil from-uri to-uri bookmark))
	 else (push (cons from-uri bookmark)
		    (getf (uri-plist to-uri) :bookmark-references))))))

(defun uri-decode-anchor (string)
  (net.uri::decode-escaped-encoding
   string
   t ;; escaped?
   (load-time-value (net.uri::reserved-char-vector nil))))

(defun note-bookmark (bookmark)
  (let ((bookmark (uri-decode-anchor bookmark)))
    #+debug-checklinks-module
    (when *verbose* (format t "noting bookmark: ~a~%" bookmark))
    (push bookmark (getf (uri-plist .uri.) :bookmarks))))

(defun bookmarks (uri)
  (getf (uri-plist uri) :bookmarks))

;; the following are used for printing nice looking messages

(defun note-bad-bookmark (location from-uri to-uri bookmark)
  (incf .status.)
  (if* (eq from-uri to-uri)
     then (comment location "Reference to non-existent bookmark ~s" bookmark)
     else (comment
	   location
	   "~
Reference to a non-extistent bookmark ~s in this file, from file ~a"
	   bookmark from-uri)))

(defun note-bad-link (location format-string &rest format-args)
  (incf .status.)
  (apply #'comment location format-string
	 (if* (uri-p (car format-args))
	    then (cons (enough-uri (car format-args) .document-root.)
		       (cdr format-args))
	    else format-args)))

(defun announce-url (&optional condition)
  (declare (ignore condition))
  (when (and (null *verbose*) (null .printed-file-name.))
    (format t "~&~%~a (ref: ~a [at file position ~d])~%" 
	(enough-uri .uri. .document-root.) (enough-uri (first .referer.) .document-root.) (second .referer.))
    (setq .printed-file-name. t)))

(defun comment (location format-string &rest format-args)
  (announce-url)
  (format t "~&")
  (let ((*print-pretty* t)
	(format-string
	 (concatenate 'simple-string
	   "- "
	   "~@<"
	   format-string
	   "~@[ [at file position ~d]~].~%"
	   "~:@>"))
	(format-args (append format-args (list location))))
    (if* *break-on-bad-links*
       then (apply #'break format-string format-args)
       else (apply #'format t format-string format-args))))

(defvar *cgi-prefix* nil)
(defvar *cgi-directory* nil)

(defun initialize-cgi-bin (string)
  (let* ((a-list (delimited-string-to-list string #\=))
	 (name (first a-list))
	 (value (second a-list)))
    (setq *cgi-prefix* (pathname-as-directory name))
    (setq *cgi-directory* (pathname-as-directory value))
    t))

(defun cgi-script-p (uri)
  (when *cgi-prefix*
    (excl::subdirectoryp (pathname (or (uri-path uri) "/")) *cgi-prefix*)))

(defun cgi-script-exists-p (uri)
  (let* ((ns (uri-path uri))
	 (len (substringp (namestring *cgi-prefix*) ns))
	 (base (when len (subseq ns len))))
    (probe-file (merge-pathnames base *cgi-directory*))))

(defvar *icons-prefix* nil)
(defvar *icons-directory* nil)

(defun initialize-icons-dir (string)
  (let* ((a-list (delimited-string-to-list string #\=))
	 (name (first a-list))
	 (value (second a-list)))
    (setq *icons-prefix* (pathname-as-directory name))
    (setq *icons-directory* (pathname-as-directory value))
    t))

(defun icons-file-p (uri)
  (when *icons-prefix*
    (and (equalp (uri-host uri) (uri-host .document-root.))
	 (excl::subdirectoryp
	  (pathname (or (uri-path uri) "/")) *icons-prefix*))))

(defun icons-file-exists-p (uri referer)
  (if* *client-mode*
     then (check-remote-uri nil uri referer)
     else (let* ((ns (uri-path uri))
		 (len (substringp (namestring *icons-prefix*) ns))
		 (base (when len (subseq ns len))))
	    (probe-file (merge-pathnames base *icons-directory*)))))

(defun local-domain-p (host ref)
  (or (equalp host ref)
;;;;TODO: parameterize this horrible hack:
      (match-re "(franzdownload|\\.?franz)\\.com$" host :return nil)))

(defun check-remote-uri (position uri referer &aux error)
  (when (visited-uri-p uri)
    #+debug-checklinks-module
    (when (eq :all *verbose*)
      (format t "check-remote-uri: already visited ~a~%" uri))
    (return-from check-remote-uri t))

  (when (or (and *ignore-hosts*
		 (member (uri-host uri) *ignore-hosts*
			 :test #'equalp))
	    (and *allow-hosts*
		 (uri-host uri)
		 (not (member (uri-host uri) *allow-hosts*
			      :test #'equalp)))
	    (and *client-mode*
		 (or (and (not *local-only*)
			  (not (eq :http (uri-scheme uri))))
		     (and *local-only*
			  (uri-host uri)
			  (not (local-domain-p (uri-host uri)
					       (uri-host .document-root.))))))
	    (and (not *client-mode*)
		 (null *check-all-links*)))
    #+debug-checklinks-module
    (when (eq :all *verbose*)
      (format t "check-remote-uri: other: ~a~%" uri))
    (return-from check-remote-uri t))

  (when (not (eq :http (uri-scheme uri)))
    #+debug-checklinks-module
    (when (eq :all *verbose*)
      (format t "check-remote-url: bail on ~a~%" uri))
    (return-from check-remote-uri t))

  #+debug-checklinks-module
  (when *verbose*
;;;; the referrer is always printed at the head of the section... why print
;;;; it again????
    #-ignore
    (comment nil "~a" (enough-uri uri .document-root.))
    #+ignore
    (comment nil "checking remote: ~a ref: ~a"
	     (enough-uri uri .document-root.)
	     (enough-uri (first referer) .document-root.)))
  (note-visited-uri uri)
  
  (if* (setq error (check-remote-uri-1 uri))
     then (if* (uri-p error)
	     then (note-bad-link position "URL ~a has moved to ~a" uri
				 (enough-uri error .document-root.))
		  nil
	     else (let (msg)
		    (when (consp error)
		      (setq msg (cdr error))
		      (setq error (car error)))
		    (case error
		      (:not-found
		       ;; Revoke the "this link is good and visited"
		       ;; status:
		       (setf (getf (uri-plist uri) :visited) nil)
		       (note-bad-link position "Bad link: ~a" uri)
		       nil)
		      (:timed-out
		       (comment position "Request for ~a timed out" uri)
		       t)
		      (:error
		       (comment position "Error processing ~a: ~a" uri msg)
		       t)
		      (t
		       ;; Revoke the "this link is good and visited"
		       ;; status: 
		       (setf (getf (uri-plist uri) :visited) nil)
		       (note-bad-link position "Bad link: ~a" uri)
		       nil))))
     else (note-link uri referer)
	  t))

(defparameter .url-transformations.
    (list (compile-re "^/franz/[^/]+(/.*)\\.html$")))

(defun check-remote-uri-1 (uri &optional (method :head) &aux path)
  (when (and uri .url-transformations.
	     (setq path (net.uri:uri-path uri)))
    (dolist (re-trans .url-transformations.)
      (multiple-value-bind (matched whole new-path)
	  (match-re re-trans path)
	(declare (ignore whole))
	(when matched
	  (setq uri (net.uri:merge-uris new-path uri))
	  (return)))))
  (flet ((doit ()
	   (multiple-value-bind (body code headers)
	    (mp:with-timeout (*timeout* :timed-out)
	      (if* (eq :head method)
		 then (handler-case
			  (net.aserve.client:do-http-request uri
			    :protocol *default-http-protocol*
			    :headers *default-headers*
			    :compress t
			    :method method)
			(error ()
			  (net.aserve.client:do-http-request uri
			    :protocol *default-http-protocol*
			    :compress t
			    :headers *default-headers*)))
		 else (net.aserve.client:do-http-request uri
			:protocol *default-http-protocol*
			:headers *default-headers*
			:compress t
			:method method)))
	  (if* (and (symbolp body) (eq :timed-out body))
	     then (if* (eq :head method)
		     then ;; for many sites, :head times out
			  (check-remote-uri-1 uri :get)
		     else body)
	   elseif (eql 200 code)
	     then nil
	   elseif (eql 301 code)
	     then (merge-uris (cdr
			       (or (assoc "location" headers :test #'equal)
				   (error "~
redirect (301) with no location header: ~s" headers)))
			      uri)
	   elseif (eql 302 code)
	     then ;; redirect
		  (check-remote-uri-1
		   (merge-uris (cdr
				(or (assoc "location" headers :test #'equal)
				    (error "~
redirect (302) with no location header: ~s" headers)))
			       uri))
	   elseif (eql 404 code)
	     then :not-found
	   elseif (or (eql 500 code)
		      (eql 501 code)
		      (eql 405 code))
	     then ;; :head failed, try :get.  Some servers are weird about
		  ;; HEAD, don't know why.
		  (check-remote-uri-1 uri :get)
	     else t))))
    (if* *production-mode*
       then (handler-case (doit)
	      (excl::asynchronous-operating-system-signal (c)
		(handle-sigint c))
	      (error (c) (cons :error (format nil "~a" c))))
       else (doit))))

;;(trace check-remote-uri-1 net.aserve.client:do-http-request)

(defun handle-sigint (c)
  #+debug-checklinks-module (break "~a" c)
  #-debug-checklinks-module
  (progn
    (format t "~a~%" c)
    (exit 1 :no-unwind t :quiet t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modified behavior of the `uri' module.

(defvar *merge-interns* t)

(defvar *temp-uri* (make-instance 'uri))
(defvar *temp-uri2* (make-instance 'uri))

net.uri::
(defmethod initialize-instance :after ((uri uri) &key)
  (when user::*uri-clos-hackery*
    (let ((user::*uri-clos-hackery* nil))
      (with-slots (scheme path query fragment) uri
	;; The following is a ugly hack, but necessary given our overall
	;; strategy.  I need the URIs /foo/ and /foo/index.html to be `eq',
	;; so the plist of a single URI is used.  Problem is, the default
	;; document is defined in the server, so I'm making a guess here.
	;; It's even worse, because the default document can usually be one
	;; of many choices.  So, this leads me to looking at the filesystem
	;; to see which of the default choices I know about are there.
	;; Ugh.
	(let (p)
	  (when (and (null user::*client-mode*)
		     (null scheme)
		     (null fragment)
		     (or (null path)
			 (and (setq p (user::my-uri-to-pathname uri))
			      (file-directory-p p))))
	    (setq path
	      (or (dolist (file user::*index-files*)
		    (let* ((user::*merge-interns* nil)
			   (temp (merge-uris
				  (copy-uri nil :path file
					    :place user::*temp-uri*
					    :class 'uri)
				  uri
				  user::*temp-uri2*))
			   (p (user::my-uri-to-pathname temp)))
		      (when (probe-file p)
			(return (uri-path temp)))))
		  (error "neither index.html nor index.htm exists!")))))

	;; If there is a fragment, then note that the bookmark was referenced,
	;; so it can be later validated.
	(if* (and query fragment)
	   thenret ;; This is most certainly an offsite link, so we ignore it.
	 elseif fragment
	   then (user::note-bookmark-reference user::.uri. uri fragment)
		;; Set the fragment to nil so that when the URI is interned, it
		;; will be `eq' not considering the fragment.
		(setq fragment nil))))))

(defvar *uri-space* (uri-space))

net.uri::
(defmethod intern-uri :around
	   ((new-uri uri) &optional (uri-space user::*uri-space*))
  ;; Merge plists in the possibly different URIs, the one given to
  ;; intern-uri and the one returned.
  (declare (ignore uri-space))
  (flet ((merge-plists (p1 p2)
	   (do* ((new (copy-list p1))
		 (p2 p2 (cddr p2))
		 tmp)
	       ((null p2) new)
	     (if* (setq tmp (cdr (member (car p2) new :test #'eq)))
		then (let* ((v1 (car tmp))
			    (v2 (cadr p2)))
		       (if* (null v1)
			  then (setf (car tmp) v2)
			elseif (null v2)
			  then (setf (car tmp) v1)
			elseif (atom v1)
			  then (when (not (equal v1 v2))
				 (error "plist values not equal: ~s, ~s."
					v1 v2))
			  else (assert (and (consp v1) (consp v2)))
			       (setf (car tmp)
				 (union v1 v2 :test #'equal))))
		else (push (cadr p2) new)
		     (push (car  p2) new)))))
    (if* user::*uri-clos-hackery*
       then (let ((user::*uri-clos-hackery* nil)
		  (old-uri (call-next-method)))
	      (when (not (eq old-uri new-uri))
		(setf (uri-plist old-uri)
		  (merge-plists (uri-plist old-uri) (uri-plist new-uri))))
	      old-uri)
       else (call-next-method))))

net.uri::
(defmethod merge-uris :around (uri base &optional place)
  ;; Define this :around method so that merge-uris automatically interns
  ;; URIs.
  (if* user::*uri-clos-hackery*
     then (let ((user::*uri-clos-hackery* nil)
		(new (user::sanity-check-uri
		      (call-next-method uri base place) uri base)))
	    (if* user::*merge-interns*
	       then (let ((res (intern-uri new user::*uri-space*)))
		      ;;(format t "merge ~a ~a -> ~a~%" uri base res)
		      res)
	       else ;;(format t "merge ~a ~a -> ~a~%" uri base new)
		    new))
     else (let ((new (call-next-method uri base place)))
	    (user::sanity-check-uri new uri base)
	    ;;(format t "merge ~a ~a -> ~a~%" uri base new)
	    new)))

(defun sanity-check-uri (result uri base)
  ;; make sure it doesn't start with ``/../'', which means there were too
  ;; many ..'s in the uri of a merge-uris.
  (when (substringp "/../" (uri-path result))
    (error "Merge of ~a and ~a yields bad result: ~a." uri base result))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these things should be elsewhere:

(defun concat-pathnames (p1 p2)
  (setq p1 (pathname p1))
  (setq p2 (pathname p2))
  (assert (and (excl::absolute-pathname-p p1)
	       (excl::absolute-pathname-p p2)))
  (let ((np2
	 (make-pathname
	  :host (pathname-host p2)
	  :device (pathname-device p2)
	  :directory (let ((d (pathname-directory p2)))
		       (cons :relative (cdr d)))
	  :name (pathname-name p2)
	  :type (pathname-type p2))))
    (merge-pathnames np2 p1)))

(defun substringp (test sequence)
  ;; return length of `test' if `sequence' starts with `test'
  (let ((test-len (length test))
	(seq-len (length sequence)))
    (when (>= test-len seq-len)
      (return-from substringp nil))
    (do* ((i 0 (1+ i)))
	((= i test-len) test-len)
      (when (not (char= (schar test i)
			(schar sequence i)))
	(return nil)))))
