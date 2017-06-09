(in-package :cggl)

;;; ---------------------------------
;;; OpenGL Support in Common Graphics

(defpackage :cg-user (:use :opengl))

;;; ---------------------------
;;; Exported User Functionality

(defclass cggl-mixin ()
  
  ;; Mix this class into any cg-stream subclass on which you would
  ;; like the do OpenGL.
  ((rendering-context
    :initform nil
    :accessor rendering-context)))

(defun current-cggl-stream ()
  
  ;; This user function returns the CG stream that is the current
  ;; OpenGL drawing destination, or nil if no destination has been
  ;; set or the current OpenGL destination is not a CG window.
  
  ;; If someone has set the current rendering context of this process
  ;; by some means other then calling (setf current-cggl-stream),
  ;; then uncache our current-cggl-stream since it is no longer
  ;; part of the current rendering context.
  (let* ((stream (getf (mp:process-property-list sys:*current-process*)
                       :current-cggl-stream)))
    (unless (and stream
                 (windowp stream)
                 (= (wglGetCurrentContext)
                    (rendering-context stream))
                 (= (wglGetCurrentDC)
                    (device-context stream)))
      (setq stream nil))
    
    stream))

(defun (setf current-cggl-stream)(stream)
  
  ;; Call this user function to specify which CG stream should
  ;; start receiving all OpenGL output.  Typically you would
  ;; call this at the top of any redisplay-window method
  ;; that calls OpenGL functions.
  (unless (eq stream (current-cggl-stream))
    (let* ((rendering-context (get-rendering-context stream)))
      
      ;; Internal note: Remember not to make cggl use the win
      ;; package, because then in an ANSI 6.2 there would be a
      ;; conflict between cg:true (from aclwin) and win:TRUE.
      (unless (eq win:TRUE (wglMakeCurrent (device-context stream)
                                           rendering-context))
        (error "wgl:MakeCurrent failed to make ~s be the ~
                current OpenGL destination."))))
  (setf (getf (mp:process-property-list sys:*current-process*)
              :current-cggl-stream)
    stream)
  stream)

(defun exit-cggl ()
  
  ;; Call this user function if desired to clean up all OpenGL
  ;; resources that have been used in cg streams.
  (wglMakeCurrent 0 0)
  (remf (mp:process-property-list sys:*current-process*)
        :current-cggl-stream)
  (let* (rendering-context)
    (dolist (stream (getf (mp:process-property-list
                           sys:*current-process*)
                          :all-cggl-streams))
      (when (setq rendering-context (rendering-context stream))
        (setf (rendering-context stream) nil)
        (wglDeleteContext rendering-context)))))

(defmethod pixel-format-descriptor-custom-values ((stream cggl-mixin))
  
  ;; Override this method if the default slot values
  ;; for the PIXELFORMATDESCRIPTOR that are provided
  ;; by the pixel-format-descriptor-default-values method below are
  ;; not sufficient.  Any plist values returned by this generic
  ;; function will override the ones returned by that one.
  nil)

;;; Double-Buffering --- By default, any CG stream that is
;;; visible on the screen will use WGL's double-buffering option.
;;; This eliminates flashing by drawing all content on a memory
;;; bitmap (the "second buffer") and then copying to the visible
;;; stream at the end.
;;; This could be overridden by defining a cggl-double-buffering
;;; method that returns different values for your own subclasses.

(defmethod cggl-double-buffering ((stream cg-stream)) nil)

(defmethod cggl-double-buffering ((stream basic-pane)) t)

;;; ----------------------
;;; Internal Functionality

(defmethod rendering-context ((stream t))
  (error "To do OpenGL on a CG stream, the stream's class ~
          must have cggl-mixin as a superclass."))

(defmethod redisplay-window :after ((stream cggl-mixin) &optional box)
  (declare (ignore box))
  
  ;; When double-buffering is being used, we must always copy the
  ;; memory bitmap on which the user's redisplay-window method
  ;; has actually drawn to the visible window.
  (when (cggl-double-buffering stream)
    (SwapBuffers (device-context stream))))

(defun get-rendering-context (stream)
  
  ;; Returns an OpenGL rendering context for a CG stream,
  ;; creating one for it if it doesn't have one yet.
  (or (rendering-context stream)
      (setf (rendering-context stream)
        (make-rendering-context stream))))

(defun make-rendering-context (stream)
  
  ;; Creates an OpenGL rendering context for a CG stream.
  (let* ((hdc (device-context stream))
         pixel-format-index)
    (with-stack-fobject (pfd 'ff_PIXELFORMATDESCRIPTOR)
      (fill-pixel-format-descriptor stream pfd)
      (setq pixel-format-index (ChoosePixelFormat hdc pfd))
      (when (eq pixel-format-index 0)
        (error "wgl:ChoosePixelFormat failed to return a ~
                pixel format index."))
      (unless (eq (SetPixelFormat hdc pixel-format-index pfd)
                  win:TRUE)
        (error "wgl:SetPixelFormat failed."))
      (push stream (getf (mp:process-property-list
                          sys:*current-process*)
                         :all-cggl-streams))
      (wglCreateContext hdc))))

(defmethod cggl-surface-type ((stream cg-stream)) 0)

(defmethod cggl-surface-type ((stream basic-pane))
  PFD_DRAW_TO_WINDOW)

(defmethod cggl-surface-type ((stream bitmap-stream))
  PFD_DRAW_TO_BITMAP)

(defconstant pixel-format-descriptor-slot-names
    '(:nSize
      :nVersion :dwFlags :iPixelType :cColorBits
      :cRedBits :cRedShift :cGreenBits :cGreenShift
      :cBlueBits :cBlueShift :cAlphaBits :cAlphaShift
      :cAccumBits :cAccumRedBits :cAccumGreenBits :cAccumBlueBits
      :cDepthBits :cStencilBits :cAuxBuffers :iLayerType
      :bReserved :dwLayerMask :dwVisibleMask :dwDamageMask))
             
(defmethod fill-pixel-format-descriptor ((stream cg-stream) pfd)
  (let* ((defaults (pixel-format-descriptor-default-values stream))
         (customs (pixel-format-descriptor-custom-values stream)))
    (dolist (slot-name pixel-format-descriptor-slot-names)
      (setf (fslot-value-typed
             'ff_PIXELFORMATDESCRIPTOR nil pfd slot-name)
        (or (getf customs slot-name)
            (getf defaults slot-name)
            0)))))
    
(defmethod pixel-format-descriptor-default-values ((stream cggl-mixin))
  (let* ((bits-per-pixel (bits-per-pixel (screen *system*))))
    (list :nSize (sizeof-fobject 'ff_PIXELFORMATDESCRIPTOR)
          :nVersion 1
          :dwFlags (logior (cggl-surface-type stream)
                           (if (cggl-double-buffering stream)
                               PFD_DOUBLEBUFFER
                             0)
                           PFD_SUPPORT_OPENGL)
          :iPixelType PFD_TYPE_RGBA
          :cColorBits bits-per-pixel
          :cDepthBits bits-per-pixel
          :iLayerType PFD_MAIN_PLANE)))

