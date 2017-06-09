;;; OpenGL on GTK --- A Fancy Red Teapot Example

(in-package :user)

;;; Call (red-teapot) to run this, then press arrow keys
;;; to rotate the vessel.

(defun red-teapot ()
  
  ;; Create a new process for the GTK/OpenGL window, to avoid
  ;; tying up the listener process that calls gtk-teapot.
  (let* ((new-windows (list nil nil)))
    (mp:process-run-function "Teapot"
      'red-teapot-2 new-windows)
    
    ;; Return the new top-level window and the OpenGL child pane
    ;; after the other process stores them in our dummy list.
    (mp:process-wait "Waiting for window creation."
		     (lambda (list)(first list)) new-windows)
    new-windows))

(defun red-teapot-2 (list-to-return)
  
  ;; Initialize GTK (if it's not already initialized).
  (gtk_init 0 0)
  
  ;; Make a top-level window plus a child pane for the drawing.
  (let* ((window (gtk_window_new GTK_WINDOW_TOPLEVEL))
	 pane)
    (gtk_window_set_title window "Teapot -- Press the Arrow Keys")
    (gtk_widget_set_size_request window 600 600)
    (setq pane (gtk_drawing_area_new))
    (gtk_container_add window pane)
    
    ;; Use a tricky way to return the new window and pane
    ;; to the other process.
    (setf (first list-to-return) window)
    (setf (second list-to-return) pane)
    
    ;; Whenever the window is exposed, call teapot-expose
    ;; to do the OpenGL drawing.
    (g_signal_connect
     pane "expose_event"
     (ff:register-foreign-callable 'teapot-expose) NULL)

    ;; Add a keypress callback to make the arrow keys
    ;; rotate the teapot.
    (g_signal_connect
     pane "key_press_event"
     (ff:register-foreign-callable 'teapot-key-press) NULL)

    ;; Actually the keypress event seems to be coming in to the
    ;; parent window, so add the handler there as well.
    ;; Otherwise you could add code to move the focus to the pane.
    (g_signal_connect
     window "key_press_event"
     (ff:register-foreign-callable 'teapot-key-press) NULL)

    ;; Add a callback to exit the event handler when the
    ;; widget is destroyed, allowing the teapot process to exit.
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

(defparameter rot-x 0.0)
(defparameter rot-y 0.0)
(defparameter tea-list nil)

(ff:defun-foreign-callable teapot-key-press
    ((widget (* GtkWidget))
     (event (* GdkEventKey))
     (data (* gpointer)))
  (declare (ignore data))
  
  ;; Remember that this is actually coming in to the parent
  ;; window, though invalidate-whole-gtk_widget is
  ;; invalidating all descendent widgets as well.
  (let* ((keyval (ff:fslot-value-typed 'GdkEventKey nil
				       event :keyval)))
    (case keyval
      (#.GDK_Up
       (decf rot-x 8.0)
       (invalidate-whole-gtk-widget widget))
      (#.GDK_Down
       (incf rot-x 8.0)
       (invalidate-whole-gtk-widget widget))
      (#.GDK_Left
       (decf rot-y 8.0)
       (invalidate-whole-gtk-widget widget))
      (#.GDK_Right
       (incf rot-y 8.0)
       (invalidate-whole-gtk-widget widget))))
  TRUE)

(ff:defun-foreign-callable teapot-destroy ((handle (* GtkWidget))
					     (data gpointer))
  (declare (ignore handle data))
  
  ;; When the teapot window has been closed, exit the
  ;; event-handling loop, allowing the teapot process to exit.
  (gtk-main-quit)
  
  FALSE)

(ff:defun-foreign-callable teapot-expose
    ((widget (* GtkDrawingArea))
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
  (fit-teapot widget)

  ;; Use pure OpenGL code from here on to draw the teapot.
  (initialize-teapot)
  (glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glPushMatrix)
  
  (glTranslatef 0.0 0.0 -5.0)
  (glRotatef rot-y 0.0 1.0 0.0)
  (glRotatef rot-x 1.0 0.0 0.0)
  
  (glCallList tea-list)
  
  (glPopMatrix)

  ;; If using OpenGL's double-buffering, then call this
  ;; function at the end of your OpenGL drawing code, to
  ;; copy the OpenGL memory buffer to the visible window.
  (swap-buffers widget)

  ;; Do not propagate this event to the parent window.
  TRUE)
						    
(defun fit-teapot (gtk-widget)
  (multiple-value-bind (width height)
      (gtk-widget-size gtk-widget)
    (glViewport 0 0 width height))
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho -6.0d0  6.0d0  -6.0d0  6.0d0  -1.0d0  10.0d0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

;;; ---------------------------------------------------
;;; The rest of this example defines the teapot object.

(eval-when (compile)
  (defun floatvec (&rest args)
    (make-array (length args) 
		:element-type 'single-float
		:initial-contents args))
  
  (defun skip-whitespace (string pos max)
    (loop
      (if (or (>= pos max)
	      (not (excl::whitespace-char-p (schar string pos))))
	  (return pos))
      (incf pos)))
  
  (defun convert-c-array (string &key (pos 0)
				      (max (length string))
				      type)
    ;; sanity check
    (if (char/= (schar string pos) #\{)
	(error "expected {"))
    
    (let ((char #\,)
	  res)
      (while (char/= char #\})
	;; sanity check
	(if (char/= char #\,)
	    (error "expected comma"))
	
	(incf pos)
	
	;; check to see if element is another array.
	(setf pos (skip-whitespace string pos max))
	(setf char (schar string pos))
	(if* (char= char #\{)
	   then
		(multiple-value-bind (array newpos)
		    (convert-c-array
		     string :pos pos :max max :type type)
		  (setf pos newpos)
		  (push array res))
		;; pos points to next non-whitespace char
		;; after the closing brace.
		;; which should be a comma or closing brace.
	   else
		(let ((startpos pos))
		  ;; scan for comma or close brace
		  (while (and (char/= char #\,) (char/= char #\}))
		    (incf pos)
		    (setf char (schar string pos)))
		  
		  (let ((value (read-from-string
				(subseq string startpos pos) nil :eof)))
		    (if (not (eq value :eof))
			(if (or (eq type 'single-float)
				(eq type 'float))
			    (push (coerce value 'single-float) res)
			  (push value res))))))
	
	(setf char (schar string pos)))
      
      (incf pos)
      
      (values (reverse res) (skip-whitespace string pos max))))
  
  (defun c-array-to-lisp-array (type dims string)
    (let ((array (convert-c-array string :type type)))
      (if (eq (first dims) '*)
	  (setf (first dims) (length array)))
      (make-array dims :element-type type 
		  :initial-contents array))))

(defun initialize-teapot ()
  (let ((position #.(floatvec 0.0 3.0 3.0 0.0))
        (local-view #.(floatvec 0.0))
        (ambient #.(floatvec 0.1745  0.01175 0.01175))
        (diffuse #.(floatvec 0.61424 0.04136 0.04136))
        (specular #.(floatvec 0.727811 0.626959 0.626959)))
    
    (glEnable GL_DEPTH_TEST)
    (glDepthFunc GL_LESS)		
    
    (glLightfv GL_LIGHT0 GL_POSITION position) 
    (glLightModelfv GL_LIGHT_MODEL_LOCAL_VIEWER local-view) 
    
    (glFrontFace GL_CW)			
    (glEnable GL_LIGHTING)		
    (glEnable GL_LIGHT0)			
    (glEnable GL_AUTO_NORMAL)		
    (glEnable GL_NORMALIZE)		
    
    (glMaterialfv GL_FRONT GL_AMBIENT ambient)
    (glMaterialfv GL_FRONT GL_DIFFUSE diffuse)
    (glMaterialfv GL_FRONT GL_SPECULAR specular)
    (glMaterialf GL_FRONT GL_SHININESS #.(* 0.6 128.0))
    
    (glClearColor 0.5 0.5 0.5 1.0)
    (glColor3f 1.0 1.0 1.0)	       
    
    (teapot 14)
    ))

(defparameter patchData 
    #.(c-array-to-lisp-array '(signed-byte 32) '(* 16) 
			     "{
    {102,103,104,105,4,5,6,7,8,9,10,11,12,13,14,15},
    {12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27},
    {24,25,26,27,29,30,31,32,33,34,35,36,37,38,39,40},
    {96,96,96,96,97,98,99,100,101,101,101,101,0,1,2,3,},
    {0,1,2,3,106,107,108,109,110,111,112,113,114,115,116,117},
    {118,118,118,118,124,122,119,121,123,126,125,120,40,39,38,37},
    {41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56},
    {53,54,55,56,57,58,59,60,61,62,63,64,28,65,66,67},
    {68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83},
    {80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95}
}"))


(defparameter cpData 
    #.(c-array-to-lisp-array 'single-float '(* 3)
			     "{
    {0.2,0,2.7},{0.2,-0.112,2.7},{0.112,-0.2,2.7},{0,-0.2,2.7},
    {1.3375,0,2.53125},{1.3375,-0.749,2.53125},{0.749,-1.3375,2.53125},
    {0,-1.3375,2.53125},{1.4375,0,2.53125},{1.4375,-0.805,2.53125},
    {0.805,-1.4375,2.53125},{0,-1.4375,2.53125},{1.5,0,2.4},{1.5,-0.84,2.4},
    {0.84,-1.5,2.4},{0,-1.5,2.4},{1.75,0,1.875},{1.75,-0.98,1.875},
    {0.98,-1.75,1.875},{0,-1.75,1.875},{2,0,1.35},{2,-1.12,1.35},
    {1.12,-2,1.35},{0,-2,1.35},{2,0,0.9},{2,-1.12,0.9},{1.12,-2,0.9},
    {0,-2,0.9},{-2,0,0.9},{2,0,0.45},{2,-1.12,0.45},{1.12,-2,0.45},
    {0,-2,0.45},{1.5,0,0.225},{1.5,-0.84,0.225},{0.84,-1.5,0.225},
    {0,-1.5,0.225},{1.5,0,0.15},{1.5,-0.84,0.15},{0.84,-1.5,0.15},
    {0,-1.5,0.15},{-1.6,0,2.025},{-1.6,-0.3,2.025},{-1.5,-0.3,2.25},
    {-1.5,0,2.25},{-2.3,0,2.025},{-2.3,-0.3,2.025},{-2.5,-0.3,2.25},
    {-2.5,0,2.25},{-2.7,0,2.025},{-2.7,-0.3,2.025},{-3,-0.3,2.25},
    {-3,0,2.25},{-2.7,0,1.8},{-2.7,-0.3,1.8},{-3,-0.3,1.8},{-3,0,1.8},
    {-2.7,0,1.575},{-2.7,-0.3,1.575},{-3,-0.3,1.35},{-3,0,1.35},
    {-2.5,0,1.125},{-2.5,-0.3,1.125},{-2.65,-0.3,0.9375},{-2.65,0,0.9375},
    {-2,-0.3,0.9},{-1.9,-0.3,0.6},{-1.9,0,0.6},{1.7,0,1.425},
    {1.7,-0.66,1.425},{1.7,-0.66,0.6},{1.7,0,0.6},{2.6,0,1.425},
    {2.6,-0.66,1.425},{3.1,-0.66,0.825},{3.1,0,0.825},{2.3,0,2.1},
    {2.3,-0.25,2.1},{2.4,-0.25,2.025},{2.4,0,2.025},{2.7,0,2.4},
    {2.7,-0.25,2.4},{3.3,-0.25,2.4},{3.3,0,2.4},{2.8,0,2.475},
    {2.8,-0.25,2.475},{3.525,-0.25,2.49375},{3.525,0,2.49375},
    {2.9,0,2.475},{2.9,-0.15,2.475},{3.45,-0.15,2.5125},{3.45,0,2.5125},
    {2.8,0,2.4},{2.8,-0.15,2.4},{3.2,-0.15,2.4},{3.2,0,2.4},{0,0,3.15},
    {0.8,0,3.15},{0.8,-0.45,3.15},{0.45,-0.8,3.15},{0,-0.8,3.15},
    {0,0,2.85},{1.4,0,2.4},{1.4,-0.784,2.4},{0.784,-1.4,2.4},{0,-1.4,2.4},
    {0.4,0,2.55},{0.4,-0.224,2.55},{0.224,-0.4,2.55},{0,-0.4,2.55},
    {1.3,0,2.55},{1.3,-0.728,2.55},{0.728,-1.3,2.55},{0,-1.3,2.55},
    {1.3,0,2.4},{1.3,-0.728,2.4},{0.728,-1.3,2.4},{0,-1.3,2.4},{0,0,0},
    {1.425,-0.798,0},{1.5,0,0.075},{1.425,0,0},{0.798,-1.425,0},
    {0,-1.5,0.075},{0,-1.425,0},{1.5,-0.84,0.075},{0.84,-1.5,0.075}
}"))

(defun teapot (grid)
  (let ((p (make-array '(4 4 3) :element-type 'single-float))
        (q (make-array '(4 4 3) :element-type 'single-float))
        (r (make-array '(4 4 3) :element-type 'single-float))
        (s (make-array '(4 4 3) :element-type 'single-float)))
    
    (macrolet ((negate-float (place) `(setf ,place (* ,place -1.0))))
      
      (setf tea-list 1)
      
      (glNewList tea-list GL_COMPILE)     
      (glPushMatrix)
      (glRotatef 270.0 1.0 0.0 0.0)	
      (dotimes (i 10)
        (dotimes (j 4)
          (dotimes (k 4)
            (dotimes (l 3)
	      ;; 	 p[j][k][l] = cpData[patchData[i][j*4+k]][l];
              (setf (aref p j k l)
		(aref cpData (aref patchData i (+ (* j 4) k)) l))
	      ;; q[j][k][l] = cpData[patchData[i][j*4+(3-k)]][l];
              (setf (aref q j k l)
		(aref cpData (aref patchData i (+ (* j 4) (- 3 k))) l))
              
              (if (= l 1)
		  ;; q[j][k][l] *= -1.0;
                  (negate-float (aref q j k l)))
              
              (when (< i 6)
		;; r[j][k][l] = cpData[patchData[i][j*4+(3-k)]][l];
                (setf (aref r j k l)
		  (aref cpData (aref patchData i
				     (+ (* j 4) (- 3 k))) l))
                (if (= l 0)
		    ;;     r[j][k][l] *= -1.0;
                    (negate-float (aref r j k l)))
                (setf (aref s j k l)
		  (aref cpData (aref patchData i (+ (* j 4) k)) l))
                (if (= l 0)
                    (negate-float (aref s j k l)))
                (if (= l 1)
                    (negate-float (aref s j k l)))))))
        
        (glMap2f GL_MAP2_VERTEX_3  0.0  1.0  3  4  0.0  1.0  12  4 p)
        (glEnable GL_MAP2_VERTEX_3)	;
        (glMapGrid2f grid  0.0  1.0  grid  0.0  1.0) ;
        (glEvalMesh2 GL_FILL  0  grid  0  grid) ;
        (glMap2f GL_MAP2_VERTEX_3  0.0  1.0  3  4  0.0  1.0  12  4 q)
        (glEvalMesh2 GL_FILL  0  grid  0  grid) ;
        (when (< i 6)
          (glMap2f GL_MAP2_VERTEX_3  0.0  1.0  3  4  0.0  1.0  12  4 r)
          (glEvalMesh2 GL_FILL  0  grid  0  grid) ;
          (glMap2f GL_MAP2_VERTEX_3  0.0  1.0  3  4  0.0  1.0  12  4 s)
          (glEvalMesh2 GL_FILL  0  grid  0  grid)))
      
      
      (glDisable GL_MAP2_VERTEX_3)
      (glPopMatrix )
      (glEndList)
      )))

