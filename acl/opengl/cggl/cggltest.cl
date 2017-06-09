(in-package :cg-user)

;;; ---------------------------------------------------------
;;; Trivial Example --- A Color Triangle

;;; Call (color-triangle) to run this.

(defun color-triangle ()
  
  ;; We just make the window in our entry function.
  ;; Its redisplay-window method below will get called
  ;; automatically to do the drawing.
  (let* ((win (make-window :color-triangle
                :class 'color-triangle
                :title "A Colorful Triangle"
                :scrollbars nil
                :exterior (make-box-relative 50 200 300 300))))
    (fit-triangle win)
    win))

;;; Use cggl-mixin to make a window class that can do OpenGL.
(defclass color-triangle (cggl-mixin frame-window)())

(defmethod resize-window :after ((window color-triangle) size)
  (declare (ignore size))
  
  ;; This gets called when the user has interactively resized
  ;; the window, allowing you to re-fit the picture within it.
  (fit-triangle window)
  (invalidate window))

(defun fit-triangle (window)
  
  ;; Fit the triangle drawing to the current size of the CG window.
  
  ;; Always call this before calling OpenGL functions that are
  ;; intended for a particular CG stream.
  (setf (current-cggl-stream) window)
  
  ;; This is a common OpenGL function set the size of the drawing.
  (glViewport 0 0 (interior-width window)(interior-height window)))

(defmethod redisplay-window ((window color-triangle) &optional box)
  (declare (ignore box))
  
  ;; Always call this at the top of your redisplay-window method.
  (setf (current-cggl-stream) window)
  
  ;; Then add whatever pure OpenGL code you want!
  (glClear GL_COLOR_BUFFER_BIT)
  (glBegin GL_TRIANGLES)
  (glColor3f 1.0 0.0 0.0)
  (glVertex2i 0 1)
  (glColor3f 0.0 1.0 0.0)
  (glVertex2i -1 -1)
  (glColor3f 0.0 0.0 1.0)
  (glVertex2i 1 -1)
  (glEnd))

;;; ---------------------------------------------------------
