
;; $Id: many-applets.cl,v 5.1 2004/02/18 19:19:03 mm Exp $

(in-package :user)

(eval-when (compile load eval) 
  (require :jlinker)
  (use-package :net.jlinker))




(defun advertise-many ()

  ;; Advertise continuously and allow connections from many JVMs.
  ;; Each JVM (browser) can run one applet.

  (jlinker-listen 
   :init-args (list :lisp-file nil :lisp-port 4321 :verbose t)
   :process-function 
   #'(lambda (x) 
       (format t "~&;; Starting server ~A ~S~%" 
	       (jlinker-slot :suffix) x)
       t)
   :end-function 
   #'(lambda (x y) (declare (ignore x y) (format t "~&;; Ending Listener~%" ))
   )))

(defun make-applet-process ()
  (let* ((suffix (jlinker-slot :suffix))
	 (data (list nil nil nil)))
    (jlinker-slot :data data)
    (mp:process-run-function
     (format nil "Applet~A" suffix)
     #'(lambda (*jlinker-connection*)
	 (unwind-protect
	     (applet-body)
	   (setf (second data) nil (third data) nil))
	 )
     *jlinker-connection*
     )))



(defvar *w* 100)
(defvar *h* 100)
(defun dd (&optional (w *w*) (h *h*))
  (format t "~&;; was w=~A h=~A  - now w=~A h=~A ~%"
	  *w* *h* w h)
  (setf *w* w *h* h))

(defun init-applet (panel cons call)
  ;; called from Java init method.
  (format t "~&;; init-applet~A called from ~A.~A.~A.~%" 
	  (jlinker-slot :suffix) 
	  (jclass-of panel) cons call)
  (format t "~&;; Starting Applet process ~A~%" 
	  (jlinker-slot :suffix)) 
  (make-applet-process)
  )

(defun start-applet (panel cons call)
  (format t "~&;; start-applet~A called from ~A.~A.~A.~%" 
	  (jlinker-slot :suffix) 
	  (jclass-of panel) cons call)
  (let ((d (jlinker-slot :data)))
    (setf (first d) t (second d) panel)))

(defun stop-applet (panel cons call)
  ;; called from Java stop method.
  (format t "~&;; stop-applet~A called from ~A.~A.~A.~%" 
	  (jlinker-slot :suffix) 
	  (jclass-of panel) cons call)
  (let ((d (jlinker-slot :data)))
    (setf (car d) nil)))

(defun destroy-applet (panel cons call)
  ;; called from Java destroy method.
  (format t "~&;; destroy-applet~A called from ~A.~A.~A.~%" 
	  (jlinker-slot :suffix) 
	  (jclass-of panel) cons call)
  (let ((d (jlinker-slot :data)))
    (setf (car d) :end)))


(defun applet-body ()
  (let* ((data (jlinker-slot :data))
	 panel
	 gr
	 (x 0) (y 0) 
	 (wd 500) (ht 300)
	 (dw (truncate wd 7))
	 (dh (truncate ht 7))
	 )
    (format t "~&;; Starting Applet~A process." (jlinker-slot :suffix))
    (loop
     (or (jlinker-query nil) (return))
     (when (car data)
       (setf panel (second data))
       (setf gr (jcall "getGraphics" panel))
       (when (eq (car data) :end) (return))
       (jcall "showStatus" panel 
	      (format nil "x=~A  y=~A" x y))
       (jcall 
	(jmethod "java.awt.Graphics" "drawOval" 
		 "int" "int" "int" "int")
	gr x y *w* *h*)
       (incf x dw)
       (incf y dh)
       (when (< wd x) (decf x wd))
       (when (< ht y) (decf y ht))
       (sleep 1)
       )
     (mp:process-wait "waiting for start-applet" 
		      #'(lambda (data) (car data)) data))
    (jlinker-end)
    (format t "~&;; Leaving Applet~A process." (jlinker-slot :suffix))
    ))






(setf excl:*restart-init-function* #'(lambda () (advertise-many)))

