;;; Load this file to load the OpenGL-on-GTK facility (GTKGL).

;; This emulates what CG does to load GTK.
(unless (find-package :gtk)

  (format *terminal-io* "~&  Loading GTK ...~%")
  (let* ((*load-source-file-info* nil))
    (require :gtk "sys:gtk;loadgtk20.cl"))
    
  ;; This probably should be done in the GTK module itself.
  (pushnew :gtk *features* :test #'eq)
    
  ;; Remember that *standard-output* is the build log file
  ;; such as cg/out.txt, so use *terminal-io* here.
  (format *terminal-io* "~&  Finished loading GTK.~%"))
  
;; Handle the mixed-case symbols in an ANSI lisp.
(let ((*readtable* (named-readtable :gtk)))

  ;; Load GTK function bindings that are missing from gtk20.cl.
  (load (compile-file (merge-pathnames "gtk20patch.cl" *load-pathname*)
		      :if-newer t))

  ;; Load our ffi for the pure OpenGL API.
  (load (compile-file (merge-pathnames "../linux-1.3/opengl.cl"
				       *load-pathname*)
		      :if-newer t))

  ;; Load package definitions for glx and gtkgl.
  (load (compile-file (merge-pathnames "pkg.cl" *load-pathname*)
		      :if-newer t))

  ;; Load our ffi for selected glx functions that we need.
  ;; GLX provides an interface from raw X into OpenGL.
  ;; The GLX functionality is presumably in the OpenGL library.
  (load (compile-file (merge-pathnames "../glx/glx.cl" *load-pathname*)
		      :if-newer t))

  ;; Load gtkgl itself.  This provides an API for doing pure OpenGL
  ;; in a GTK window.
  (load (compile-file (merge-pathnames "gtkgl.cl" *load-pathname*)
		      :if-newer t))

  ;; Load the simple example.
  (load (compile-file (merge-pathnames "gtkgltest.cl" *load-pathname*)
		      :if-newer t))

  ;; Load the fancy teapot example.
  (load (compile-file (merge-pathnames "gtkteapot.cl" *load-pathname*)
		      :if-newer t)))

