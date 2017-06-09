;;
;; copyright (c) 1996-2000 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2004 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: loadgtk12.cl,v 1.4 2004/01/16 19:24:56 layer Exp $

;; Patched for bug12382

;;
;; Allegro CL GTK+ Interface loader.
;;
;; When loaded, this file attempts to load the Allegro CL GTK+ interface
;; which is assumed to be in the same directory as this file.
;;
;; Files being loaded are compiled first if necessary.  The main tricky area
;; is building/loading gtk-lib.so, the foreign stub file giving access to
;; the system GTK+ libraries.
;;
;; For building gtk-lib.so:
;; -- The gtk-config program must be available.
;; -- gtk libs must be in LD_LIBRARY_PATH when invoking the ld command.
;;
;; For loading gtk-lib.so:
;; -- gtk libs must be in LD_LIBRARY_PATH when Lisp is started.
;;
;; If gtk-lib.so is not found, Lisp restarts are used to help guide the user
;; towards building it.
;;

(in-package :excl)

(setf (named-readtable :gtk)
  (copy-readtable nil))

(when (eq *current-case-mode* :case-insensitive-upper)
  (setf (readtable-case (named-readtable :gtk)) :invert))

(labels
    ((do-load (*default-pathname-defaults*)
       (do ((gtk-lib.so (merge-pathnames "gtk12-lib.so"))
	    (gtk-lib.so-loaded nil))
	   (gtk-lib.so-loaded)		; end restart loop if success
	 (unless (probe-file gtk-lib.so)
	   (build-gtk-lib.so gtk-lib.so))
	 (restart-case			; rebuild gtk-lib.so if necessary
	     (handler-bind ((error #'(lambda (c)
				       (declare (ignore c))
				       (format t "~&~
~@<~@;Possible Error Cause: ~:I~
Lisp needs to be started with the LD_LIBRARY_PATH environment variable ~
including the gtk library path.~:@>~%"))))
	       (load gtk-lib.so)
	       (setq gtk-lib.so-loaded t))
	   (r-build-gtk-lib.so ()
	       :report (lambda (stream)
			 (format stream "Build ~a" (namestring gtk-lib.so)))
	     (build-gtk-lib.so gtk-lib.so))))

       (load (compile-file-if-needed "cdbind.cl")) ; From cbind
       ;; spr26502
       ;; too much to compile this in trial release
       (if* (featurep :allegro-cl-trial)
	  then (note t "~
Interface being loaded uncompiled for Allegro CL Trial Release")
	       ;; Copied from gtk12.cl, which has it only in eval-when compile
	       ;; environment.
	       (let ((.symbol-types. (make-hash-table)))
		 (def-fwrapper bind-c-type-symbol-types-wrapper (&rest args)
		   (destructuring-bind ((macrof-name id &rest def)
					&rest macrof-args)
		       args
		     (declare (ignore macrof-name macrof-args))
		     (let ((result (list (call-next-fwrapper))))
		       (when (and (null (cdr def))
				  (symbolp (car def)))
			 (pushnew id (gethash (car def) .symbol-types.)))
		       (dolist (prev-def (gethash id .symbol-types.))
			 (push `(ff::bind-c-type ,prev-def ,id) result))
		       (cons 'progn (nreverse result)))))

		 (fwrap 'ff::bind-c-typedef :bind-c-type-symbol-types-wrapper
			'bind-c-type-symbol-types-wrapper)
		 (fwrap 'ff::bind-c-type :bind-c-type-symbol-types-wrapper
			'bind-c-type-symbol-types-wrapper))
	       (load "gtk12.cl")
	  else (load (compile-file-if-needed "gtk12.cl")))
       (load (compile-file-if-needed "eh.cl")))

     (build-gtk-lib.so (gtk-lib.so)
       ;; For building gtk-lib.so:
       ;; -- The gtk-config must be available.
       ;; -- gtk libs must be in LD_LIBRARY_PATH during ld.
       ;; 
       (do ((gtk-lib.so-built nil)
	    (gtk-config-path "")
	    (cmd nil))
	   (gtk-lib.so-built)		; end restart loop if success
	 (setq cmd (format nil "~
env LD_LIBRARY_PATH=~a ld ~a -o ~a `\"~agtk-config\" --libs | ~
sed 's/-rdynamic//'`"
			   (sys:getenv "LD_LIBRARY_PATH")
			   #+(or sparc aix) "-G"
			   #-(or sparc aix) "-shared"
			   (namestring gtk-lib.so)
			   gtk-config-path))
	 (format t "~&Command:~% ~s~%" cmd)
	 (restart-case			; specify gtk-config path if necessary
	     (if* (zerop (shell cmd))
		then (setq gtk-lib.so-built t)
		else (error "Build failed."))
	   (r-gtk-path (gtk-path-string)
	       :interactive read-new-value
	       :report (lambda (stream)
			 (format stream "~
~@<Retry building ~a by specifying the directory containing gtk-config.~:@>"
				 (namestring gtk-lib.so)))
	     (setq gtk-config-path
	       (concatenate 'string gtk-path-string "/")))
	   (r-add-to-ld-library-path (ld-path-string)
	       :report (lambda (stream)
			 (format stream "~
~@<Retry building ~a by adding a directory to LD_LIBRARY_PATH.~:@>"
				 (namestring gtk-lib.so)))
	       :interactive read-new-value
	     (let ((ld-path (sys:getenv "LD_LIBRARY_PATH")))
	       ;; Note that this doesn't affect dynamic loading
	       ;; in current process.
	       (setf (sys:getenv "LD_LIBRARY_PATH")
		 (concatenate 'string ld-path ":" ld-path-string)))))))

     (read-new-value ()
       (format t "Enter pathname namestring: ")
       (multiple-value-list (eval (read-line)))))

  (let ((*record-source-file-info* nil)
	(*load-source-file-info* nil))
    (with-named-readtable (:gtk)
      (do-load *load-pathname*))))

(with-named-readtable (:gtk)
  (format t "~&~@<;;; ~@;~
GTK+ Interface loaded. ~2%~
Note:  ~
When loading, using compile-file, or otherwise using the Lisp reader to read ~
Lisp ~
expressions that access the GTK+ interface, the appropriate ~
readtable-case should be used.  ~
In Modern mode, use :preserve (the default standard readtable-case ~
setting for Modern mode).  ~
In ANSI mode, use :invert.  ~2%~
The :gtk named-readtable, which has the appropriate readtable-case setting
for the current mode, ~
is provided for convenience and portability.~2%~
Example: ~s~2%~
The with-named-readtable macro can also be used.
~:@>"
	  '(let ((*readtable* (named-readtable :gtk)))
	    (load (compile-file "lispex-gtk12/02.01-helloworld.cl")))))
