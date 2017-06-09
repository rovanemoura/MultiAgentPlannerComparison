(in-package :user)

(dribble "update.out")

#+use-in-case-mode
(excl::in-case-mode :local)

(require :winapi)
(require :winapi-dev)

(setq excl::*restart-init-function* nil) ;; prevent IDE from starting

(ff:def-foreign-type _system_info
    (:struct (:union (dwOemId win:dword)
		     (:struct (wProcessorArchitecture win:word)
			      (wReserved win:word)))
	     (dwPageSize win:dword)
	     (lpMinimumApplicationAddress win:lpvoid)
	     (lpMaximumApplicationAddress win:lpvoid)
	     ;; (dwActiveProcessorMask win::dword_ptr)
	     (dwActiveProcessorMask (* win:dword))
	     (dwNumberOfProcessors win:dword)
	     (dwProcessorType win:dword)
	     (dwAllocationGranularity win:dword)
	     (wProcessorLevel win:word)
	     (wProcessorRevision win:word)))

(ff:def-foreign-call (get-system-info "GetSystemInfo")
    ((lpSystemInfo (* _system_info)))
  :returning :void)

(defun number-of-processors ()
  (let ((obj (ff:allocate-fobject '_system_info :c)))
    (get-system-info obj)
    (ff:fslot-value-typed '_system_info :c obj 'dwNumberOfProcessors)))

;; Keep in sync with $images in update.sh
(defvar *images-to-update*
    '("alisp.dxl" "alisp8.dxl" "mlisp.dxl" "mlisp8.dxl"
      "allegro.dxl" "allegro-ansi.dxl" "allegro-express.dxl"
      "clim.dxl" #|no composer.dxl|#))
(defvar *ignored-images* nil)

(defvar error-detected nil)
(defvar do-bundle-update t)

(format t "~%~%
This Lisp will exit after rebuilding images, unless an error occurs.~%~%")
  
(defun print-bli-form (file build dribble debug s &aux (*print-case* :downcase))
  (print
   `(handler-case
	(progn
;;;; Force loading of :build module in case there are patches to it.
;;;; This is needed because it's loaded into the IDE by default, and since
;;;; we use the `orig' images for rebuilding, a patch to build.fasl will
;;;; never get loaded if the IDE is present.  Unless, we force it:
	  (setq *modules* (delete "build" *modules* :test #'equalp))
	  (require :build)
	  #+ignore
	  (trace build-lisp-image excl::run-lisp excl::read-from-env
		 sys:getenv)
	  (setq *print-startup-message*
	    (let ((r (reverse *print-startup-message*)))
	      (reverse
	       (pushnew '(:external-format . t) r
			:test #'equal))))
	  (when (probe-file "sys:require-search-list.cl")
	    (load "sys:require-search-list.cl"))
	  (build-lisp-image
	   ,file
	   :verbose t
	   :pll-from-sys t
	   :pre-dump-form
	   `(progn
	      (dolist (sl excl::.loaded-shared-libraries.)
		(when (equalp "aclre32.dll"
			      (file-namestring (excl::shlib-name sl)))
		  (setf (excl::shlib-name sl)
		    (merge-pathnames "aclre32.dll" #p"sys:;cg;")))))
	   :build-input ,debug
	   :build-output ,build
	   #+mswindows :splash-from-file
	   #+mswindows (translate-logical-pathname "sys:allegro.dib")))
      (error () (exit 1)))
   s)
  (print '(dribble) s)
  (print '(exit 0) s)
  (terpri s))

(dolist (dxl-file (directory "*.dxl"))
  (let ((file (file-namestring dxl-file))
	(dxl-file (namestring dxl-file)))
    (if* (member file '("update2.dxl" "dcl.dxl" "dcli.dxl")
		 :test #'string=)
       thenret ;; ignore these images, they are internal to franz
     elseif (not (member file *images-to-update* :test #'string-equal))
       then ;; note for later
	    (when (not (match-re "(instsrc|old|orig)\\.dxl$" dxl-file
				 :return nil))
	      (push dxl-file *ignored-images*))
       else (format t "Making new ~a...~%" file)
	    (let ((orig-dxl (format nil "~aorig.dxl" (pathname-name file)))
		  (old-dxl (format nil "~aold.dxl" (pathname-name file)))
		  (build (format nil "~a.build" (pathname-name file)))
		  (dribble (format nil "~a.dribble" (pathname-name file)))
		  (debug (format nil "~a.debug" (pathname-name file)))
		  (lisp-exe (pathname-name file))
		  prev)

	      (when do-bundle-update 
		(setq do-bundle-update nil)
		(format t "~
Performing bundle check.
The bundle check may take several minutes.")
		(with-open-file (s "bunup-build.cl" :direction :output
				 :if-exists :supersede)
		  (format s "~
 (setq excl::*restart-init-function* nil) ;; prevent IDE from starting
 (build-lisp-image \"tmpbu.dxl\" :verbose t :build-output \"tmpbu.build\"
                   :include-devel-env t :build-input \"tmpbu.debug\"
                   :include-ide nil :restart-app-function nil
                   :restart-init-function nil)
 (exit 0)"))
		(run-shell-command (format nil "~
~a +R +B +cn -I ~a -L bunup-build.cl -qq -batch -backtrace-on-error"
					   lisp-exe file)
				   :show-window :minimized)
		(with-open-file (s "bunup-check.cl" :direction :output
				 :if-exists :supersede)
		  (format s "~
 (progn
   (setq excl::*restart-init-function* nil) ;; prevent IDE from starting
   (when (fboundp 'excl::update-bundle-check)
     (unless (excl::update-bundle-check t)
       (excl::update-bundle-files)))
   (exit 0))
"))
		(run-shell-command (format nil "~
~a +R +B +cn -I tmpbu.dxl -L bunup-check.cl -qq -batch -backtrace-on-error"
					   lisp-exe)
				   :show-window :minimized)
		(when (sys:getenv "ACL_UPDATE_SLEEP_HACK") (sleep 10))
		(when (> (number-of-processors) 1) (sleep 5))
		(delete-file "tmpbu.dxl")
		(format t "~&Finished bundle check.~%"))
		

	      (if* (not (probe-file orig-dxl))
		 then (setq prev (rename-file-raw dxl-file orig-dxl))
		 else (when (probe-file old-dxl) (delete-file old-dxl))
		      (setq prev (rename-file-raw dxl-file old-dxl)))
	      (with-open-file (s "update1temp.cl" :direction :output
			       :if-exists :supersede)
		(print-bli-form file build dribble debug s))
	      (when (/= 0
			(run-shell-command
			 (format nil
				 "~a +R +B +cn +s update1temp.cl -I ~a -qq ~a"
				 lisp-exe orig-dxl
				 "-batch -backtrace-on-error")
			 :show-window :minimized))
		(setq error-detected dxl-file)
		(when (probe-file dxl-file) (delete-file dxl-file))
		(rename-file-raw prev dxl-file))
	      (delete-file "update1temp.cl")
	      (when (or error-detected (not (probe-file file)))
		(format t "Build of ~a failed (check ~a for errors).~%"
			file build)
		(return))))))

(defvar *exit* 0)

(if* error-detected
   then (let ((msg (format nil "Update failed creating ~a" error-detected)))
	  (format t "~a.~%" msg)
	  (excl::internal-message-box msg "Allegro CL update"))
	(setq *exit* -1)
   else (format t "Done.~%"))
  
(when *ignored-images*
  (excl::internal-message-box
   (format nil "~
Warning: These images were not recognized images and were not rebuilt
         by update.exe:
~{              ~a~%~}"
	   *ignored-images*)
   "Allegro CL update"))

(dribble)
(exit *exit* :quiet t)
