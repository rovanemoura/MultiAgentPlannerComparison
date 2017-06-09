;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2013 Franz Inc, Oakland, CA  All rights reserved.
;;
;; All rights reserved.
;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;; -=End Copyright Notice=-

;; Using carets

;; The caret facility uses the single flashing cursor provided
;; by the Windows API.  It does not require the use of a timer.

(in-package :cg-user)

(defclass my-caret-frame (non-refreshing-window)
  ())

(defclass my-caret-pane (non-refreshing-pane caret-mixin)
  ())

(defmethod default-pane-class ((window my-caret-frame))
  'my-caret-pane)

(defmethod mouse-left-down ((pane my-caret-pane) buttons data)
  (declare (ignore buttons))
  ;; Move the caret to the spot that was clicked
  (setf (caret-position pane)
    ;; Subtract half of the caret pixmap size to center it
    ;; at the moused position
    (position- data
	       (make-position (floor (caret-width pane) 2)
			      (floor (caret-height pane) 2)))))

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
;;
(defun run-carets-example ()
  
  #+gtk (error "Carets are not yet implemented on GTK.")
  
  (let* ((window1 (make-window :caret-test
                    :class 'my-caret-frame 
                    :owner (development-main-window *system*)
                    :title "X Caret"
                    :scrollbars t
                    :exterior (make-box 500 140 700 280)))
         
         (pane1 (frame-child window1))
         (window2 (make-window :caret-frame
                    :class 'my-caret-frame 
                    :owner (development-main-window *system*)
                    :title "Block Caret"
                    :scrollbars t
                    :exterior (make-box 500 300 700 440)))
         (pane2 (frame-child window2))
         
         ;; An optional custom bitmap caret (which draws an X)
         (caret-texture (make-instance 'texture
                          :bits-per-pixel 1
                          :contents
                          (make-texture-contents 32 32)))
         caret-texture-handle)
    
    ;; Fill in the X caret bitmap programmatically
    (dotimes (j 32)
      (setf (contents-ref caret-texture j j) 1)
      (setf (contents-ref caret-texture j (- 31 j)) 1))
    
    (setq caret-texture-handle
          (create-pixmap-handle (screen *system*) caret-texture
                                (make-texture-info :width 32 :height 32
                                                   :bits-per-pixel 1
                                                   :colors `#(,black ,white))))
    (setf (caret pane1) caret-texture-handle)
    (setf (caret-width pane1) 32)
    (setf (caret-height pane1) 32)
    (setf (caret pane2) :solid)
    (setf (caret-width pane2) 8)
    (setf (caret-height pane2) 16)
    
    window1))

#+run-example
(run-carets-example)
