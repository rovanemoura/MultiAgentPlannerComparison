;;; Load this file to load the OpenGL-on-CG facility (cggl).

;;; (Avoid loading OpenGL twice if this file is loaded twice,
;;; since a segv occurs in wgl:SetPixelFormat then.)
(unless (and (find-package :opengl)
             (find-symbol (symbol-name '#:glBegin) :opengl))
  
  ;;; Load our ffi for the pure OpenGL API.
  (load (compile-file (merge-pathnames "../win32-1.1/opengl.cl"
				       *load-pathname*)
                      :if-newer t)))

;;; Load our ffi for Microsoft's WGL API for OpenGL in MS-Windows.
(load (compile-file (merge-pathnames "../win32-1.1/wgl.cl" *load-pathname*)
                    :if-newer t))

;;; Load a package definition for the new cggl package.
(load (compile-file (merge-pathnames "pkg.cl" *load-pathname*)
                    :if-newer t))

;;; Load cggl itself.  This provides the API for doing OpenGL
;;; in Common Graphics windows.
(load (compile-file (merge-pathnames "cggl.cl" *load-pathname*)
                    :if-newer t))

;;; Load the simple example.
(load (compile-file (merge-pathnames "cggltest.cl" *load-pathname*)
                    :if-newer t))

;;; Load the fancy teapot example.
(load (compile-file (merge-pathnames "cgteapot.cl" *load-pathname*)
                    :if-newer t))

