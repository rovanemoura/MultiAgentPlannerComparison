(in-package :gtkgl)

;;; -----------------------------------------
;;; OpenGL Support on Allegro's GTK Interface

;;; -----------------------------
;;; Internal Variables and Macros

(defparameter *gtkgl-rcs* (make-hash-table :size 64))

(defmacro rendering-context (gtk-widget)
  `(gethash ,gtk-widget *gtkgl-rcs*))

;;; ---------------------------
;;; Exported User Functionality

;;; This could be set to true when the X server is on the
;;; host computer, for faster drawing.
(defparameter *use-direct-x-connection* nil)

(defun glx-extension ()
  
  ;; If this returns NIL, then the X server does not support the
  ;; GLX extensions.  Otherwise it returns two values indicating
  ;; the error-base and the event-base, whatever those are.
  (declare (optimize (speed 3)(safety 1))) ;; for stack allocation
  (ff:with-stack-fobjects ((error-base :int)(event-base :int))
    (and (glXQueryExtension
	  (gdk_x11_get_default_xdisplay)
	  (ff:fslot-address-typed :int nil error-base)
	  (ff:fslot-address-typed :int nil event-base))
	 (values (ff:fslot-value-typed :int nil error-base)
		 (ff:fslot-value-typed :int nil event-base)))))

(defun glx-version ()
  
  ;; If this returns NIL, then the X server does not support the
  ;; GLX extensions.  Otherwise it returns the major and minor
  ;; versions of the GLX support as two multiple values.
  (declare (optimize (speed 3)(safety 1))) ;; for stack allocation
  (ff:with-stack-fobjects ((major :int)(minor :int))
    (and (glXQueryVersion
	  (gdk_x11_get_default_xdisplay)
	  (ff:fslot-address-typed :int nil major)
	  (ff:fslot-address-typed :int nil minor))
	 (values (ff:fslot-value-typed :int nil major)
		 (ff:fslot-value-typed :int nil minor)))))

(defun current-gtkgl-widget ()
  
  ;; This user function returns the GTK widget that is the current
  ;; OpenGL drawing destination, or nil if no destination has been
  ;; set or the current OpenGL destination is not a GTK widget.
  
  ;; If someone has set the current rendering context of this process
  ;; by some means other then calling (setf current-gtkgl-widget),
  ;; then uncache our current-gtkgl-widget since it is no longer
  ;; the current OpenGL drawing destination.
  (let* ((widget (getf (mp:process-property-list sys:*current-process*)
                       :current-gtkgl-widget)))
    (unless (and widget
                 (GTK_WIDGET_MAPPED widget)
                 (= (glXGetCurrentContext)
                    (rendering-context widget)))
      (setq widget nil))
    
    widget))

(defun (setf current-gtkgl-widget)(gtk-widget)
  
  ;; Call this user function to specify which GTK widget should
  ;; start receiving all OpenGL output.  Typically you would
  ;; call this at the top of any expose-event callback function
  ;; that calls OpenGL functions.
  (unless (eq gtk-widget (current-gtkgl-widget))
    (let* ((rendering-context (get-rendering-context gtk-widget))
	   (gdk-window (gdk-window-of-gtk-widget gtk-widget))
	   makecurrent-rc)
      (unless gdk-window
	(error "Can't make the GTK widget ~s be the ~`
                current-gtkgl-widget ~
                because it does not have a GDK (X) window."))
      (setq makecurrent-rc (glXMakeCurrent
			    (gdk_x11_get_default_xdisplay)
			    (gdk_x11_drawable_get_xid gdk-window)
			    rendering-context))
      (unless (eq makecurrent-rc B_TRUE)
        (error "glXMakeCurrent failed to make the GTK window ~s be ~
                the current OpenGL destination, returning ~s."
	       gtk-widget makecurrent-rc)))
    (setf (getf (mp:process-property-list sys:*current-process*)
		:current-gtkgl-widget)
      gtk-widget))
  gtk-widget)

(defun swap-buffers (gtk-widget)
  
  ;; When using the GTK_DOUBLE_BUFFERED option (included by default),
  ;; call this at the end of your OpenGL code (in your expose-event
  ;; callback function) to copy the image from the memory bitmap to
  ;; the visible GTK widget.
  (glXSwapBuffers (gdk_x11_get_default_xdisplay)
		  (gdk_x11_drawable_get_xid
		   (gdk-window-of-gtk-widget gtk-widget))))

(defun exit-gtkgl ()
  
  ;; Call this user function if desired to clean up all OpenGL
  ;; resources that have been used in GTK widgets.
  ;; This is not really necessary, and only works on windows
  ;; in the current process.
  (glXMakeCurrent (gdk_x11_get_default_xdisplay) 0 0)
  (remf (mp:process-property-list sys:*current-process*)
        :current-gtkgl-widget)
  (let* (rendering-context)
    (dolist (window (getf (mp:process-property-list
                           sys:*current-process*)
                          :all-gtkgl-widgets))
      (when (setq rendering-context (rendering-context window))
        (setf (rendering-context window) nil)
        (glXDestroyContext (gdk_x11_get_default_xdisplay)
			   rendering-context)))))

(defun gtk-widget-size (gtk-widget)
  
  ;; A utility to return the interior width and height
  ;; of a GTK widget.  Useful for fitting the OpenGL viewport
  ;; to the current size of the widget where it's being drawn.
  (let* ((gdk-window (ff:fslot-value-typed
		      'GtkWidget nil gtk-widget :window)))
    (ff:with-stack-fobjects ((x '(:array :long 1))
			     (y '(:array :long 1))
			     (width '(:array :long 1))
			     (height '(:array :long 1)))
      (gdk_window_get_geometry
       gdk-window x y width height NULL)
      (values
       (ff:fslot-value-typed '(:array :long 1) nil width 0)
       (ff:fslot-value-typed '(:array :long 1) nil height 0)))))

(defun invalidate-whole-gtk-widget (gtk-widget)
  (declare (optimize (speed 3)(safety 1))) ;; for stack allocation
  
  ;; A utility to simply invalidate a whole GTK widget, so
  ;; that it will be redrawn.
  (ff:with-stack-fobject (rect 'GdkRectangle)
    (multiple-value-bind (width height)
	(gtk-widget-size gtk-widget)
      (setf (ff:fslot-value-typed 'GdkRectangle nil rect :x) 0)
      (setf (ff:fslot-value-typed 'GdkRectangle nil rect :y) 0)
      (setf (ff:fslot-value-typed 'GdkRectangle nil rect :width)
	width)
      (setf (ff:fslot-value-typed 'GdkRectangle nil rect :height)
	height)
      (gdk_window_invalidate_rect
       (gdk-window-of-gtk-widget gtk-widget) rect TRUE))))

(defun gdk-window-of-gtk-widget (gtk-widget)
  
  ;; A utility to return the GdkWindow of a GtkWidget.
  ;; A GdkWindow is a thin wrapper on a raw X window.
  (let* ((pointer (ff:fslot-value-typed
		   'GtkWidget nil gtk-widget :window)))

    ;; Return nil if there is no GdkWindow, which could
    ;; be either because this type of widget does not
    ;; use an X window, or because the widget has not
    ;; been realized.
    (and (not (eq pointer 0))
	 pointer)))

(defparameter *gtkgl-visual-attributes*
    
    ;; Modify this variable if needed to specify the desired
    ;; attributes for the X visual to be used by OpenGL.
    (list
     
     ;; Ask for a true-color visual rather than using palettes.
     GLX_RGBA
     
     ;; Specify double-buffering to eliminate flashing during
     ;; animation updates by first drawing on a memory bitmap.
     ;; (On my machine, glxChooseVisual fails without this.)
     GLX_DOUBLEBUFFER))

;;; ----------------------
;;; Internal Functionality

(defun get-rendering-context (gtk-widget)
  
  ;; Returns the OpenGL rendering context for a particular
  ;; GTK widget, creating it the first time.
  (or (rendering-context gtk-widget)
      (progn
	
	;; Now that we're giving an OpenGL rendering context
	;; to this window, add a callback for removing it
	;; when the window has been closed.
	(g_signal_connect
	 gtk-widget "destroy"
	 (ff:register-foreign-callable 'opengl-destroy) NULL)
	
	;; Create the rendering context if it doesn't exist yet.
	(setf (rendering-context gtk-widget)
	  (make-rendering-context gtk-widget)))))

(defun make-rendering-context (gtk-widget)
  
  ;; Creates an OpenGL rendering context for a GTK widget.
  (declare (optimize (speed 3)(safety 1))) ;; for stack allocation
  (let* ((attributes *gtkgl-visual-attributes*)
	 (number-of-attributes (length attributes))
	 (vector-type `(:array :int ,(1+ number-of-attributes)))
	 visualinfo context)
    (push gtk-widget (getf (mp:process-property-list
			    sys:*current-process*)
			   :all-gtkgl-widgets))
  
    ;; Fill a vector of attributes for the X visual to be used
    ;; by OpenGL.
    (ff:with-stack-fobject (attribute-vector vector-type)
      (dotimes (j number-of-attributes)
	(setf (ff:fslot-value-typed vector-type nil
				    attribute-vector j)
	  (pop attributes)))

      ;; Mark the end of the attribute list with a zero.
      (setf (ff:fslot-value-typed vector-type nil attribute-vector
				  number-of-attributes)
	0)
      
      ;; Create the X visual.  We will get the closest thing to our
      ;; request that the X server could provide.
      (setq visualinfo (glXChooseVisual
			(gdk_x11_get_default_xdisplay)
			(gdk_x11_get_default_screen)
			attribute-vector))
      (when (zerop visualinfo)
	(error "glXChooseVisual failed."))
      
      ;; Create the rendering context object.
      ;; glxcontext can be NULL when not sharing with other displays.
      ;; directp means whether to go direct to the frame buffer
      ;; for efficiency; this requires that the x server be local.
      (setq context (glXCreateContext
		     (gdk_x11_get_default_xdisplay)
		     visualinfo
		     NULL ;; glxcontext for sharing
		     (if *use-direct-x-connection* TRUE FALSE)))
      (when (zerop context)
	(error "glXCreateContext failed."))
      
      ;; Return the new rendering context.
      context)))

(ff:defun-foreign-callable opengl-destroy ((widget (* GtkWidget))
					   (data gpointer))
  (declare (ignore data))
  
  ;; When a GTK window is destroyed, be sure to unmap it
  ;; from its rendering context, because another
  ;; widget created later could have the same handle and
  ;; otherwise would still map to the existing context.
  (let* ((rendering-context (rendering-context widget)))
    (when rendering-context
      (remhash widget *gtkgl-rcs*)
      (glXDestroyContext (gdk_x11_get_default_xdisplay)
			 rendering-context))
    (delete widget
	    (getf (mp:process-property-list sys:*current-process*)
		  :all-gtkgl-widgets)
	    :test #'=))
  FALSE)

