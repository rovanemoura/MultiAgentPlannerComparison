
;; $Id: one-applet.cl,v 5.1 2004/02/18 19:19:03 mm Exp $

(in-package :user)

(eval-when (compile load eval) 
  (require :jlinker)
  (use-package :net.jlinker))

(defun advertise () 

  ;; Advertise a single connection that allows a single Applet
  ;; to run.  If multiple applets start in the same JVM, only the
  ;; last one will be running.

  ;; advertise at fixed port 
  ;; applet seems to have trouble finding .trp file

  (jlinker-init :lisp-advertises 
		:lisp-file nil :lisp-port 4321 
		:timeout 120 :verbose t))



(defvar *run* nil)
(defvar *panel* nil)

(defun make-applet-process ()
  (let* ()
    (setf *run* nil *panel* nil)
    (mp:process-run-function
     (format nil "Applet")
     #'(lambda ()
	 (unwind-protect
	     (applet-body)
	   (setf *run* nil *panel* nil))
	 )
     )))



(defvar *w* 100)
(defvar *h* 100)
(defun dd (&optional (w *w*) (h *h*))
  (format t "~&;; was w=~A h=~A  - now w=~A h=~A ~%"
	  *w* *h* w h)
  (setf *w* w *h* h))


(defun init-applet (panel cons call)
  ;; called from Java init method.
  (format t "~&;; init-applet ~A.~A.~A called.~%" 
	  (jclass-of panel) cons call)
  (make-applet-process))

(defun start-applet (panel cons call)
  ;; called from Java start method.
  (format t "~&;; start-applet ~A.~A.~A called.~%" 
	  (jclass-of panel) cons call)
  (setf *run* t *panel* panel))

(defun stop-applet (panel cons call)
  ;; called from Java stop method.
  (format t "~&;; stop-applet ~A.~A.~A called.~%" 
	  (jclass-of panel) cons call)
  (setf *run* nil))

(defun destroy-applet (panel cons call)
  ;; called from Java destroy method.
  (format t "~&;; destroy-applet~A.~A.~A called.~%" 
	  (jclass-of panel) cons call)
  (setf *run* :end))


(defun applet-body ()
  (let* (panel gr
	       (x 0) (y 0) 
	       (wd 500) (ht 300)
	       (dw (truncate wd 7))
	       (dh (truncate ht 7))
	       )
    (loop
     (or (jlinker-query nil) (return))
     (when (eq *run* :end) (return))
     (when *run*
       (or panel (setf panel *panel*))
       (or gr (setf gr (jcall "getGraphics" panel)))
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
		      #'(lambda () *run*)))
    (format t "~&;; Leaving Applet process.")
    ))



(setf excl:*restart-init-function* 
  #'(lambda () 
      (advertise)))

