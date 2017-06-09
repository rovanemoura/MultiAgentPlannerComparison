;; $Id: glx.cl,v 1.1.1.1 2006/08/15 18:21:14 layer Exp $
;;
;; Hand coded by Ken Cheetham.

(in-package :glx)

(ff:def-foreign-call (glXQueryExtension "glXQueryExtension")
    ((Display :foreign-address)
     (errorBase (* :int))
     (eventBase (* :int)))
  :returning :boolean
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXQueryVersion "glXQueryVersion")
    ((Display :foreign-address)
     (major (* :int))
     (minor (* :int)))
  :returning :boolean
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXGetCurrentContext "glXGetCurrentContext")
    (:void)
  :returning :foreign-address
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXGetCurrentDC "glXGetCurrentDC")
    (:void)
  :returning :foreign-address
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXMakeCurrent "glXMakeCurrent")
  ((arg0 :foreign-address)
   (arg1 :foreign-address)
   (arg2 :foreign-address))
  :returning :int
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXChooseVisual "glXChooseVisual")
    ((display :foreign-address)
     (screen :int)
     (attribute-list (* :int)))
  :returning :foreign-address
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXCreateContext "glXCreateContext")
    ((display :foreign-address)
     (visual :foreign-address)
     (share-list :int)
     (direct :int))
  :returning :int
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXDestroyContext "glXDestroyContext")
  ((arg0 :foreign-address)
   (arg1 :foreign-address))
  :returning :void
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXSwapBuffers "glXSwapBuffers")
    ((display :foreign-address)
     (drawable gint))
  :returning :void
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

(ff:def-foreign-call (glXWaitX "glXWaitX")
    (:void)
  :returning :void
  :strings-convert t
  :call-direct t
  :optimize-for-space t)

;;; Constants from /usr/include/GL/glxtokens.h

(defconstant GLX_USE_GL 1)         /* support GLX rendering */
(defconstant GLX_BUFFER_SIZE 2)    /* depth of the color buffer */
(defconstant GLX_LEVEL 3)          /* level in plane stacking */
(defconstant GLX_RGBA 4)           /* true if RGBA mode */
(defconstant GLX_DOUBLEBUFFER 5)   /* double buffering supported */
(defconstant GLX_STEREO 6)         /* stereo buffering supported */
(defconstant GLX_AUX_BUFFERS 7)    /* number of aux buffers */
(defconstant GLX_RED_SIZE 8)       /* number of red component bits */
(defconstant GLX_GREEN_SIZE 9)     /* number of green component bits */
(defconstant GLX_BLUE_SIZE 10)     /* number of blue component bits */
(defconstant GLX_ALPHA_SIZE 11)    /* number of alpha component bits */
(defconstant GLX_DEPTH_SIZE 12)    /* number of depth bits */
(defconstant GLX_STENCIL_SIZE 13)  /* number of stencil bits */
(defconstant GLX_ACCUM_RED_SIZE 14)   /* number of red accum bits */
(defconstant GLX_ACCUM_GREEN_SIZE 15) /* number of green accum bits */
(defconstant GLX_ACCUM_BLUE_SIZE 16)  /* number of blue accum bits */
(defconstant GLX_ACCUM_ALPHA_SIZE 17) /* number of alpha accum bits */
