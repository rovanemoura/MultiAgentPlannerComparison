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

;;; An animated "drawable" control.
;;; Shows an big arrowhead moving smoothly across the control.

(in-package :cg-user)

;; Each time the timer fires, call update-drawable to draw the
;; current image on the bitmap-stream and then copy it quickly
;; to the visible window.  Stop the timer if the demo window has
;; been closed.
(defun animate-drawable-on-timer (timer)
  (let* ((widget (timer-info timer)))
    (if* (windowp (parent widget))
       then (update-drawable widget)
       else (stop-timer timer))))

;; Use a single timer for multiple runs of this example.
;; Find it when needed from its name.
(defun animation-timer ()
   (or (find-timer :animate-drawable-timer)
       (make-instance 'timer
         :name :animate-drawable-timer)))

;; Turn the timer on or off to start or stop the movement
;; of the arrow.
(defun toggle-animation-timer (drawable &key (interval 15))
   (let* ((timer (animation-timer))
          (count (timer-count timer)))
      (setf (on-timer timer) 'animate-drawable-on-timer)
      (setf (interval timer) interval)
      (setf (timer-info timer) drawable)
      (setf (active timer)(not (active timer)))
      (setf (timer-count timer) count)))

;; Use the name of this function as the on-redisplay event-handler
;; property of the drawable control.  It will draw the arrow at a
;; certain position based on how many times the timer has fired
;; so far (the timer-count)
(defun animation-on-redisplay (drawable stream)
  (let* ((width (page-width drawable))
         (height (page-height drawable))
         (timer (animation-timer))
         
         ;; Move the arrow by 3 pixels each time the timer fires.
         (x (- (mod (* 3 (timer-count timer))
                    (+ width 80)) 80))
         (y (floor height 2.5)))
    
    ;; Draw a big Melvin background over which to animate the arrow.
    (with-positions-and-boxes (pos1)(box1)
      (copy-to-stream (find-pixmap :melvin) stream
                      (nmake-box box1 0 0 width height))
      
      ;; Draw the arrow at the incremented position.
      (copy-to-stream (arrow-pixmap) stream
                      (nmake-position pos1 x y)))))

;; Use the name of this function as the on-click event-handler
;; property of the drawable control.  This lets you click on the
;; control to start or stop the movement of the timer
(defun animation-on-click (dialog drawable)
   (declare (ignore dialog))
  (toggle-animation-timer drawable))

(defun run-animated-drawable-example ()
  (make-melvin)
  (let* ((drawable (make-instance 'drawable
                     :left 8 :top 8 :width 184 :height 184
                     :on-redisplay 'animation-on-redisplay
                     :use-bitmap-stream t
                     :on-click 'animation-on-click))
         (dialog (make-window :animated-drawable
                   :class 'dialog
                   :interior (make-box 200 200 400 400)
                   :resizable nil
                   :title "Click Me"
                   :widgets (list drawable))))
    (open-pixmap-handle (find-pixmap :melvin))
    (open-pixmap-handle (arrow-pixmap))
    (select-window dialog)
    dialog))

(defparameter *arrow-pixmap* nil)

;; The weird way to draw an arrow.
(defun arrow-pixmap ()
  (or *arrow-pixmap*
      (setq *arrow-pixmap*
            (let* ((width 80)
                   (height 80)
                   (pixmap (make-instance 'pixmap
                             :width width :height height
                             :bits-per-pixel 1
                             :contents
                             (make-list 80 :initial-element
                                        (make-list 80 :initial-element 0))
                             :mask-contents
                             (make-list 80 :initial-element
                                        (make-list 80 :initial-element 0))
                             :colors (vector black blue)))
                   (x1/2 (floor width 2))
                   (y1/2 (floor height 2))
                   (y1/3 (floor (* 1/3 height)))
                   (y2/3 (ceiling (* 2/3 height))))
              (dotimes (x width)
                (dotimes (y height)
                  (if* (if* (< x x1/2)
                          then (< y1/3 y y2/3)
                          else (if* (< y y1/2)
                                  then (> y (- x x1/2))
                                  else (> (- height y)(- x x1/2))))
                     then (setf (contents-ref pixmap x y) 1)
                          (setf (mask-contents-ref pixmap x y) 0)
                     else (setf (contents-ref pixmap x y) 0)
                          (setf (mask-contents-ref pixmap x y) 1))))
              pixmap))))

(defun make-melvin ()
  (cache-pixmap
   (make-instance 'pixmap
     :name :melvin
     :contents
     '((11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 11 00 08 08 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 00 00 00 00 00 00 00 00 08 08 00 00 00 00 00 00 00 00 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 00 00 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 00 00 11 11 11 0 0 0 0 0 0)
       (11 11 00 00 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 00 00 11 11 0 0 0 0 0 0)
       (11 11 00 08 08 08 08 08 00 00 00 00 00 00 00 00 00 00 08 08 08 08 08 00 11 11 0 0 0 0 0 0)
       (11 11 00 08 08 08 08 00 00 08 08 08 08 08 08 08 08 00 00 08 08 08 08 00 11 11 0 0 0 0 0 0)
       (11 11 00 08 08 08 08 00 08 08 08 08 08 08 08 08 08 08 00 08 08 08 08 00 11 11 0 0 0 0 0 0)
       (11 11 00 00 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 08 00 00 11 11 0 0 0 0 0 0)
       (11 11 11 00 00 08 08 08 08 08 00 00 08 08 00 00 08 08 08 08 08 00 00 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 00 00 00 00 08 08 00 00 08 08 00 00 08 08 00 00 00 00 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 00 00 08 08 08 08 08 08 08 08 00 00 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 00 00 08 08 08 08 08 08 00 00 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 00 08 08 08 08 08 08 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 00 08 08 08 08 08 08 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 00 08 08 08 08 08 08 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 00 08 08 08 08 08 08 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 00 00 08 08 08 08 00 00 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 00 00 08 08 00 00 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 11 00 00 00 00 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0)
       (11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 0 0 0 0 0 0))
     :width 26 :height 26 :bits-per-pixel 4
     :invert-p t
     :colors :default)))

#+run-example (run-animated-drawable-example)
