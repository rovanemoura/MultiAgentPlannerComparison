;;; OpenGL on GTK --- A Simple Color Triangle Example

(in-package :user)

(defun color-triangle ()
  
  ;; Create a new process for the GTK/OpenGL window, to avoid
  ;; tying up the listener process that calls color-triangle.
  (let* ((new-windows (list nil nil)))
    (mp:process-run-function "Color Triangle"
      'color-triangle-2 new-windows)
    
    ;; Return the new top-level window and the OpenGL child pane
    ;; after the other process stores them in our dummy list.
    (mp:process-wait "Waiting for window creation."
		     (lambda (list)(first list)) new-windows)
    new-windows))

(defun color-triangle-2 (list-to-return)
  
  ;; Initialize GTK (if it's not already initialized).
  (gtk_init 0 0)
  
  ;; Make a top-level window plus a child pane for the drawing.
  (let* ((window (gtk_window_new GTK_WINDOW_TOPLEVEL))
	 pane)
    (gtk_window_set_title window "OpenGL Simple Triangle Test")
    (gtk_widget_set_size_request window 600 500)
    (setq pane (gtk_drawing_area_new))
    (gtk_container_add window pane)
    
    ;; Use a tricky way to return the new window and pane
    ;; to the other process.
    (setf (first list-to-return) window)
    (setf (second list-to-return) pane)
    
    ;; Whenever the window is exposed, call triangle-expose
    ;; to do the OpenGL drawing.
    (g_signal_connect
     pane "expose_event"
     (ff:register-foreign-callable 'triangle-expose) NULL)

    ;; When the window is closed, use this callback to exit
    ;; the event-handling loop, allowing the process to exit.
    (g_signal_connect
     pane "destroy"
     (ff:register-foreign-callable 'triangle-destroy) NULL)

    ;; Since OpenGL will do its own double-buffering, we must
    ;; disable GTK's double-buffering that it does by default.
    ;; (If they both do double-buffering, things gets confused.)
    (gtk_widget_set_double_buffered pane FALSE)

    ;; Show the windows.
    (gtk_widget_show pane)
    (gtk_widget_show window)

    ;; Enter an event-handling loop.
    (gtk-main)))

(ff:defun-foreign-callable triangle-expose ((widget (* GtkDrawingArea))
					    (event (* GdkEvent))
					    (data (* gpointer)))
  (declare (ignore event data))
  
  ;; This is called whenever the GTK widget is uncovered
  ;; and needs to be redrawn.

  ;; We must make this GTK widget be the current OpenGL
  ;; drawing destination before we can draw OpenGL on it.
  (setf (current-gtkgl-widget) widget)

  ;; Make sure that we are fitting the drawing to the current
  ;; interior size of the GTK widget.
  (multiple-value-bind (width height)
      (gtk-widget-size widget)
    (glViewport 0 0 width height)

    ;; Draw whatever pure OpenGL you want here.
    ;; This is a simple multicolor triangle.
    (glClear GL_COLOR_BUFFER_BIT)
    (glBegin GL_TRIANGLES)
    (glColor3f 1.0 0.0 0.0)
    (glVertex2i 0 1)
    (glColor3f 0.0 1.0 0.0)
    (glVertex2i -1 -1)
    (glColor3f 0.0 0.0 1.0)
    (glVertex2i 1 -1)
    (glEnd)
  
    ;; If using OpenGL's double-buffering, then call this
    ;; function at the end of your OpenGL drawing code, to
    ;; copy the OpenGL memory buffer to the visible window.
    (swap-buffers widget)

    ;; Test doing some GTK drawing over the OpenGL drawing.
    ;; Just fill a black rectangle in the middle of the window.
    (let* ((gdk-window (gdk-window-of-gtk-widget widget))
	   (gc (gdk_gc_new gdk-window)))
      (unwind-protect
	  (gdk_draw_rectangle
	   gdk-window gc TRUE
	   (- (floor width 2) 30)
	   (- (floor height 2) 20)
	   60 40)
	(g_object_unref gc))))

  ;; Do not propagate this event to the parent window.
  TRUE)

(ff:defun-foreign-callable triangle-destroy ((handle (* GtkWidget))
					     (data gpointer))
  (declare (ignore handle data))
  
  ;; When the triangle window has been closed, exit the
  ;; event-handling loop, allowing the triangle process to exit.
  (gtk-main-quit)
  
  FALSE)

