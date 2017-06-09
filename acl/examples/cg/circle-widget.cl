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

;;  Defining a simple dialog item widget in lisp.
;;  
;;  Circle: displays values 0 to 360. You drag the hands with the mouse, 
;;  when the mouse button is released, the dialog item value is updated.
;;  The arrow keys (with or without the CONTROL key) may also be used.

(in-package :cg-user)

;; The widget class.
(defclass circle-widget (lisp-widget)
  ()
  (:default-initargs
      :double-buffered t))

(defclass circle-widget-pane (lisp-widget-top-window)())

(defmethod widget-device ((item circle-widget) dialog)
  (declare (ignore dialog))
  
  ;; Tell the dialog-item class which widget-window class it
  ;; should instantiate.
  'circle-widget-pane)

(defmethod device-open ((widget-window circle-widget-pane)
                        slot-names options)
  (declare (ignore slot-names))
  
  ;; Call this function to actually create the widget's window.
  ;; Additional code could be added here to perform other
  ;; desired side effects when creating the window.
  (open-lisp-widget-window widget-window options)
  
  ;; Return true for a successful open.
  t)

(defun circle-from-box (box)
  ;; return centre and radius of the circle bounded by box
  (values 
   (position* (position+ (box-top-left box) (box-bottom-right box)) 0.5 0.5)
   (i1- (i/ (box-width box) 2))))

(defmethod redisplay-window ((window circle-widget-pane) &optional box)
  (declare (ignore box))
  (call-next-method) ;; Draw the blank background
  (with-boxes (box1)
    (multiple-value-bind (centre radius)
        (circle-from-box (visible-box window))
      (let* ((widget (dialog-item window))
             (box (nmake-box box1
                    (i- (position-x centre) 18)
                    (i- (position-y centre) 12)
                    (i+ (position-x centre) 18)
                    (i+ (position-y centre) 12))))
        
        #+no ;; use draw-circle-sector to also draw the radius line.
        (draw-circle window centre radius)
        (draw-circle-sector window centre radius
                            (- (value widget) 90) 360)
        (erase-contents-box window box)
        (draw-string-in-box window
                            (format nil "~a~a" (value widget)
                              #.(code-char 176)) ;; the "degree" symbol
                            nil nil box :center :center)
        (decf (box-right box))(decf (box-bottom box))
        (draw-box window box))))
  t)

;;; dvg    08oct96  3.3: changed to state
(defmethod widget-set-value ((window circle-widget-pane) item new-value
                             old-value recursive-p)
  (declare (ignore item new-value old-value recursive-p))
  ;; This method is called internally whenever the widget's value changes.
  ;; We just need to redraw the widget so that the new value will be shown.
  (if (not (eq (state window) :shrunk))
      (invalidate window))
  t)

(defmethod mouse-left-down ((window circle-widget-pane) buttons data)
  (declare (ignore buttons data))
  (let* ((circle-centre (circle-from-box (visible-box window))) 
         (radius (nposition- (get-line window circle-centre)
                             circle-centre)))
    (setf (value (dialog-item window))
      (mod (+ (truncate (* 360
                           (atan (float (position-y radius))
                                 (float (position-x radius))))
                        #.(* 2.0 pi))
              90)
           360))))

(defmethod resize-window :after ((window circle-widget-pane) position)
   ;; If the user resizes the circle widget, invalidate it so that it
   ;; will be completely redrawn at the new window size.
   (declare (ignore position))
   (invalidate window))

(defmethod virtual-key-down ((window circle-widget-pane) buttons data)
  ;; Use the arrow keys to increment the value.
  (case data
    ((#.vk-up #.vk-right)
     (incf (value (dialog-item window))
        
	   ;; If the control-key is included, increment by a larger amount.
	   (if (ilogtest control-key buttons) 10 1)))
    ((#.vk-down #.vk-left)
     (decf (value (dialog-item window))
	   (if (ilogtest control-key buttons) 10 1)))))

(defmethod lisp-widget-draw-focus ((widget-window circle-widget-pane)
                                   (widget circle-widget))
  ;; Don't bother drawing a focus rectangle for this simple widget.
  t)

(defmethod lisp-widget-clear-focus ((widget-window circle-widget-pane)
				    (widget circle-widget))
  t)

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
(defun run-circle-widget-example (&key circle)
  
  (if (and circle (parent circle))
      (remove-component circle (parent circle))
    
    ;; Else create circle widget.
    (setq circle (make-instance 'circle-widget
                   :name :circle-widget
                   :value 0
                   :left 10 :top 10 :width 100 :height 100 
                   :right-attachment :right
                   :bottom-attachment :bottom
                   :on-change
                   ;; this function will be called whenever the value is
                   ;; updated by widget.  
                   #'(lambda (item new-value old-value)
                       (declare (ignore item))
                       (format t "~%Circle: old-value=~a new-value=~a"
                         old-value new-value)
                       t))))
  
  ;; set the value programmatically
  (setf (value circle) 30)
  
  ;; now make the interaction quieter, change the on-change to true. 
  (setf (on-change circle) #'excl::true)
  
  ;; return the window.
  (make-window :circle-widget-test
    :widgets (list circle)
    :class 'dialog 
    :owner (development-main-window *system*)
    :interior (make-box 500 260 620 380)
    :title "Circle Test"))

#+run-example
(run-circle-widget-example)
