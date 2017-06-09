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

;; $Id: loadgtk20.cl,v 1.10 2009/04/17 17:44:43 duane Exp $

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
;; -- The pkg-config (or gtk-config) program must be available.
;; -- gtk libs must be in LD_LIBRARY_PATH when invoking the ld command.
;;
;; For loading gtk-lib.so:
;; -- gtk libs must be in LD_LIBRARY_PATH when Lisp is started.
;;
;; If gtk-lib.so is not found, Lisp restarts are used to help guide the user
;; towards building it.
;;

(in-package :excl)

#-use-in-case-mode
(setf (named-readtable :gtk)
  (copy-readtable nil))

#+remove				; bug15263
(when (eq *current-case-mode* :case-insensitive-upper)
  (setf (readtable-case (named-readtable :gtk)) :invert))


(labels
    ((do-load (*default-pathname-defaults* loadable-sofiles)
       (do ((gtk-lib.so-loaded nil))
	   (gtk-lib.so-loaded)		; end restart loop if success
         (loop for ((gtk-lib.so rebuild-function attempt-loading-p) . more-options-p) on loadable-sofiles
               until gtk-lib.so-loaded
               do (when (and (probe-file gtk-lib.so)
                             (funcall attempt-loading-p))
                    (if more-options-p
                        (ignore-errors (progn (load gtk-lib.so)
                                              (setf gtk-lib.so-loaded t)))
                        (restart-case ; rebuild gtk-lib.so if necessary
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
                            (funcall rebuild-function gtk-lib.so)))))))

       (load (compile-file-if-needed "cdbind.cl")) ; From cbind
       ;; bug12382
       ;; skip compiling since it doesn't work in Trial, and doesn't
       ;; currently buy much.
       (let ((comp::*fasl-hash-size* 500000))
	 (load (compile-file-if-needed #+ignore identity "gtk20.cl")))
       (load (compile-file-if-needed "eh.cl")))
     #+macosx
     (build-gtk-lib-using-framework (gtk-lib.so)
       (let ((cmd (format nil "~
cc~@[~a~] -bundle -flat_namespace -o ~a -framework Gtk"
                          (or #-64bit " -m32" " -m64")
                          (namestring (translate-logical-pathname gtk-lib.so)))))
         (format t "~&Command:~% ~s~%" cmd)
         (restart-case
             (if* (zerop (shell cmd))
                  then (setq gtk-lib.so-built t)
                  else (error "Build failed."))
           (r-build-gtk-lib.so-oldfashioned
               :report (lambda (stream)
                         (format stream "Build ~a using GTK in /opt/local" (namestring gtk-lib.so)))
               (build-gtk-lib.so gtk-lib.so)))))
     
     (build-gtk-lib.so (gtk-lib.so
			&aux (config-prog "pkg-config")
			     (config-arg "gtk+-2.0"))
       ;; For building gtk-lib.so:
       ;; -- The pkg-config (or gtk-config) must be available.
       ;; -- gtk libs must be in LD_LIBRARY_PATH during ld.
       ;; 
       (do ((gtk-lib.so-built nil)
	    (pkg-config-path "")
	    (gtk-lib.so (translate-logical-pathname gtk-lib.so))
	    (cmd nil))
	   (gtk-lib.so-built)		; end restart loop if success
	 (setq cmd (format nil "~
env LD_LIBRARY_PATH=~a cc~@[~a~] ~a -o ~a ~
`\"~a~a\" --libs ~a | ~
sed 's/-rdynamic//'`"
			   (sys:getenv "LD_LIBRARY_PATH")
			   #+(and 64bit macosx) " -m64"
			   #-(and 64bit macosx) ""
			   #+(or sparc aix) "-G"
			   #+macosx "-bundle -flat_namespace"
			   #-(or sparc aix macosx) "-shared"
			   (namestring gtk-lib.so)
			   pkg-config-path
			   config-prog
			   config-arg))
	 (format t "~&Command:~% ~s~%" cmd)
	 (restart-case			; specify config-prog path if necessary
	     (if* (zerop (shell cmd))
		then (setq gtk-lib.so-built t)
		else (error "Build failed."))
	   (r-gtk-path (gtk-path-string)
	       :interactive read-new-value
	       :report (lambda (stream)
			 (format stream "~
~@<Retry building ~a by specifying the directory containing ~a~:@>"
				 (namestring gtk-lib.so)
				 config-prog))
	     (setq pkg-config-path
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
       (multiple-value-list (eval (read-line))))

     (attempt-loading-framework-gtk-lib-p ()
       (probe-file "/Library/Frameworks/Gtk.framework/Gtk"))
     
     (rebuild-sofiles-if-missing (*default-pathname-defaults*)
       (let ((sofiles-to-rebuild `(#+macosx (,(parse-namestring (format nil "gtk:gtk20-fw.~a" sys::*dll-type*))
                                              ,#'build-gtk-lib-using-framework
                                              ,#'attempt-loading-framework-gtk-lib-p)
                                            (,(parse-namestring (format nil "gtk:gtk20-lib.~a" sys::*dll-type*))
                                              ,#'build-gtk-lib.so
                                              ,(constantly t)))))
         (dolist (sofile sofiles-to-rebuild)
           (unless (probe-file (car sofile))
             (funcall (cadr sofile) (car sofile))))
         sofiles-to-rebuild)))

  (let* ((*record-source-file-info* nil)
         (*load-source-file-info* nil)
         (build-dir (make-pathname :name nil :type nil :defaults (translate-logical-pathname *load-pathname*)))
         (loadable-sofiles (rebuild-sofiles-if-missing build-dir)))
    
    #-use-in-case-mode
    (with-named-readtable (:gtk)
      ;; bug14934
      (do-load build-dir loadable-sofiles))
    #+use-in-case-mode
    (do-load build-dir loadable-sofiles)))

#+remove				; bug15263
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
	    (load (compile-file "lispex-gtk20/02.01-helloworld.cl")))))
