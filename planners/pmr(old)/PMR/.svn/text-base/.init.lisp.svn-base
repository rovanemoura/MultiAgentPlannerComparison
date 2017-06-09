(in-package "COMMON-LISP-USER")

;; For faster code, less debugging:
(proclaim '(optimize (space 0) (compilation-speed 0) (safety 1) (speed 3) (debug 0)))
;; For faster compilation, more debugging:
;;(proclaim '(optimize (compilation-speed 3) (safety 1) (speed 3) (debug 3)))

(defvar *my-path*
  #+(or macos darwin) "/Users/dborrajo/"
;;SFA  #+linux "/home/dborrajo/"
;;  #+linux "/home/sfernandez/sayphi-daniel-ma/"
  #+linux (namestring *default-pathname-defaults*)
  #+win32 "z:/Users/dborrajo/")

(defvar *my-lisp-path* (concatenate 'string *my-path* "mi-software/lisp/"))
(defvar *my-planning-path* (concatenate 'string *my-path* "planning/"))
(defvar *my-tmp-path* (concatenate 'string *my-path* "tmp/"))
;; (defvar *sayphi-loader* (concatenate 'string *my-planning-path* "new-sayphi/loader"))
(defvar *sayphi-loader* (concatenate 'string *my-planning-path* "sayphi/loader"))
(defvar *ipss-loader* (concatenate 'string *my-planning-path* "ipss/init"))

(setf *trace-print-level* most-positive-fixnum
      *trace-print-length* most-positive-fixnum
      *print-level* most-positive-fixnum
      *print-length* most-positive-fixnum
      *print-pretty* t
      *print-circle* nil
      *print-array* t
      *print-nickname* nil
      *print-structure* t
      *random-state* (make-random-state t))

#+LISPWORKS
(setf *enter-debugger-directly* t
       (editor:variable-value "Input Format Default") '(:latin-1 :eol-style :lf)
       *describe-print-level* 20
       *step-print-level* 20
       *trace-print-level* 20
       *compile-print* 0)

#+allegro
;;; The following line causes Allegro to do a tenured (full) GC when it should
(setf excl::*global-gc-behavior* :auto
      ;; Stopping Allegro from warning about redefinitions makes loading files a
      ;; lot faster
      *enable-package-locked-errors* nil
      (mp::global-symbol-value '*redefinitions-warnings*) nil
      ;;the next four cause the compiler and loader not to generate source file and
      ;;cross-reference information which takes up much space & time:
      *record-source-file-info* nil
      *load-source-file-info* nil
      *load-xref-info* nil
      *record-xref-info* nil
      top-level:*zoom-print-level*  most-positive-fixnum
      top-level:*zoom-print-length* most-positive-fixnum
      top-level:*zoom-print-circle* nil
      top-level::*print-level* most-positive-fixnum
      top-level::*print-length* most-positive-fixnum
      *print-readably* t)

#+clisp
(setf custom:*warn-on-floating-point-contagion* nil)


;; I load all the common Lisp functions
(cond ((probe-file (concatenate 'string *my-lisp-path* "common.lisp"))
       (load (concatenate 'string *my-lisp-path* "common.lisp"))
       (compile-file (concatenate 'string *my-lisp-path* "common.lisp"))
       (load (concatenate 'string *my-lisp-path* "common")))
      (t (load (concatenate 'string *my-lisp-path* "common.lisp"))
	 (compile-file (concatenate 'string *my-lisp-path* "common"))
	 (load (concatenate 'string *my-lisp-path* "common"))))

#+CLISP (load (concatenate 'string *my-lisp-path* "asdf"))

#-WIN32 (setf asdf::*central-registry* (list *my-lisp-path*))
#+WIN32 (setf asdf:*central-registry* (list (concatenate 'string *my-lisp-path* "cl-ppcre-1.3.2/")
					    (concatenate 'string *my-lisp-path* "usocket-0.3.4/")
					    (concatenate 'string *my-lisp-path* "xmls-1.2/")
					    *my-lisp-path*))

;;; If the fasl was stale, try to recompile and load (once). Since only SBCL
;;; has a separate condition for bogus fasls we retry on any old error
;;; on other lisps. Actually, Allegro has a similar condition, but it's 
;;; unexported.  Works nicely for the ACL7 upgrade, though.
;;; CMUCL has an invalid-fasl condition as of 19c.
(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (#+sbcl sb-ext:invalid-fasl 
     #+allegro excl::file-incompatible-fasl-error
     #+lispworks conditions:fasl-error
     #+cmu ext:invalid-fasl
     #-(or sbcl allegro lispworks cmu) error ()
     (asdf:perform (make-instance 'asdf:compile-op) c)
     (call-next-method))))

(defun ipss nil (load *ipss-loader*))

(defun load-sayphi (&optional (modules (list 'ebl)))
  (load *sayphi-loader*))
;;   (setf *auto-load* nil)
;;   (load *sayphi-loader*)
;;   (inner-load-sayphi modules))

(defun load-errtplan nil
  (load *sayphi-loader*)
  (load-learner 'errt))

(defun load-ma-sayphi nil
  (load-errtplan)
  (load (concatenate 'string *my-planning-path* "sayphi/system/learning/ma-sayphi.lisp"))
  (compile-file (concatenate 'string *my-planning-path* "sayphi/system/learning/ma-sayphi.lisp"))
  (load (concatenate 'string *my-planning-path* "sayphi/system/learning/ma-execute-from-core.lisp"))
  (compile-file (concatenate 'string *my-planning-path* "sayphi/system/learning/ma-execute-from-core.lisp"))
  (load (concatenate 'string *my-planning-path* "sayphi/system/learning/ma-sayphi"))
  (load (concatenate 'string *my-planning-path* "sayphi/system/learning/pddl-macro-operators.lisp"))
  (compile-file (concatenate 'string *my-planning-path* "sayphi/system/learning/pddl-macro-operators.lisp"))
  (load (concatenate 'string *my-planning-path* "sayphi/system/learning/pddl-macro-operators")))

(defun load-ssa-dcii nil
  (load-sayphi)
  (load (concatenate 'string *my-planning-path* "ipss/generic-pddl"))
  (load (concatenate 'string *my-planning-path* "sayphi/domains/ssa-dcii/parse-and-solve.lisp"))
  (compile-file (concatenate 'string *my-planning-path* "sayphi/domains/ssa-dcii/parse-and-solve.lisp"))
  (load (concatenate 'string *my-planning-path* "sayphi/domains/ssa-dcii/parse-and-solve")))

(defun solve-ipss (domain &optional path-planning-p (world-path (concatenate 'string *my-planning-path* "prodigy/domains/")))
  (ipss)
  (if path-planning-p
      (samap nil nil))
  (load (format nil "~a~(~a~)/sigue.lisp" (setf *world-path* world-path) domain)))

(defun curso-macros nil
  (load (concatenate 'string *my-planning-path* "ipss/init"))
  (load (concatenate 'string *my-planning-path* "ipss/macro-operators.lisp"))
  (load (concatenate 'string *my-planning-path* "prodigy/domains/logistics/sigue-curso.lisp"))
  (generate-new-op (create-macro (visible-plan (prodigy-result-solution *prodigy-result*)))))

(defun curso-ebl nil
  (load (concatenate 'string *my-planning-path* "ipss/init"))
  (load (concatenate 'string *my-planning-path* "prodigy/domains/logistics/sigue-curso-ebl.lisp")))

(defun samap (&optional (load-ipss-p t) (execute-samap-p t))
  (load "/Users/dborrajo/samap/loader.lisp")
  (samap-loader load-ipss-p)
  (if execute-samap-p 
      (execute-samap)))

(defun busqueda (&key (algoritmo 'a-star) (dominio 'path-planning) (problema nil)
		      (recompilep nil))
  (load (concatenate 'string *my-lisp-path* "../busqueda/loader"))
  (daniel-cargar-algoritmo algoritmo dominio problema)
  (when recompilep
    (search-load-and-compile (list "general" (format nil "~(~a~)" algoritmo)))
    (search-load-and-compile (list (format nil "~(~a~)" dominio)) :subpath 'dominios)))

(defun pale nil
  (load (concatenate 'string *my-planning-path* "pale/loader")))

(defun lpg-adapt nil
  (load (concatenate 'string *my-planning-path* "LPG-adapt/run-adapt")))

(defun lama nil
  (load-errtplan)
  (load (concatenate 'string *my-planning-path* "fd/execute-lama.lisp"))
  (compile-file (concatenate 'string *my-planning-path* "fd/execute-lama.lisp"))
  (load (concatenate 'string *my-planning-path* "fd/execute-lama")))

(defun graficos nil
  (asdf:oos 'asdf:load-op :mcclim))
