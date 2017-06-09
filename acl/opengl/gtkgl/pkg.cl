(in-package :user)

(defpackage :glx
  (:shadowing-import-from :gtk #:time)
  (:use :cl :gtk)
  (:export
   
   ;; GLX Functions
   #:glXQueryExtension
   #:glXQueryVersion
   #:glXGetCurrentContext
   #:glXGetCurrentDC
   #:glXMakeCurrent
   #:glXDestroyContext
   #:glXChooseVisual
   #:glXCreateContext
   #:glXSwapBuffers
   #:glXWaitX
   
   ;; GLX Visual Info Constants
   ;; *gtkgl-visual-attributes* could be modified
   ;; to include a subset of these.
   #:GLX_USE_GL
   #:GLX_BUFFER_SIZE
   #:GLX_LEVEL
   #:GLX_RGBA
   #:GLX_DOUBLEBUFFER
   #:GLX_STEREO
   #:GLX_AUX_BUFFERS
   #:GLX_RED_SIZE
   #:GLX_GREEN_SIZE
   #:GLX_BLUE_SIZE
   #:GLX_ALPHA_SIZE
   #:GLX_DEPTH_SIZE
   #:GLX_STENCIL_SIZE
   #:GLX_ACCUM_RED_SIZE
   #:GLX_ACCUM_GREEN_SIZE
   #:GLX_ACCUM_BLUE_SIZE
   #:GLX_ACCUM_ALPHA_SIZE
   ))

(defpackage :gtkgl
  (:shadowing-import-from :gtk #:time)
  (:use :cl :gtk :glx)
  
  ;; User functions for our OpenGL In GTK facility.
  (:export
   #:current-gtkgl-widget ;; setfable
   #:swap-buffers
   #:*use-direct-x-connection*
   #:*gtkgl-visual-attributes*
   #:gdk-window-of-gtk-widget
   #:gtk-widget-size
   #:invalidate-whole-gtk-widget
   #:exit-gtkgl
   #:glx-extension
   #:glx-version
   ))

(defpackage :user
  (:shadowing-import-from :gtk #:time)
  (:use :gtk :glx :gtkgl :opengl))

