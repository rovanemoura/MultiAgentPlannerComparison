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

;;; A frame window with two panes separated by a draggable split-bar widget.
;;; Call (run-split-bar-example) to test.

;;; For 8.2 this example has been enhanced to use the new general split-widget
;;; rather than implementing one from scratch.

;;; First create window subclasses so that we can define custom
;;; redisplay-window methods on them.

(defclass my-frame (frame-window)
  
  ;; We will use these two slots to store application parameters.
  ((minimum-pane-width :accessor minimum-pane-width
                       :initform 80)
   (divider-width :accessor divider-width
                  :initform 8)))

(defclass my-ellipse-pane (basic-pane)())

(defclass my-box-pane (basic-pane)())

;;; Make each type of pane draw a different sort of thing.

(defmethod redisplay-window ((window my-ellipse-pane) &optional box)
  (declare (ignore box))
  (clear-page window)
  
  ;; Draw a big green ellipse in any my-ellispse-pane.
  (let* ((half-width (1- (floor (interior-width window) 2)))
         (half-height (1- (floor (interior-height window) 2))))
    (with-foreground-color (window green)
      (fill-ellipse window (make-position half-width half-height)
                    half-width half-height 0))
    (draw-ellipse window (make-position half-width half-height)
                  half-width half-height 0)))

(defmethod redisplay-window ((window my-box-pane) &optional box)
  (declare (ignore box))
  (clear-page window)
  
  ;; Draw a bunch of nested rectangles in any my-box-pane.
  (let* ((width (interior-width window))
         (height (interior-height window))
         (box-to-draw (make-box (- width)(- height)
                                (* 2 width)(* 2 height))))
    (dotimes (j 1000)
      
      ;; If we've gotten to the middle of the pane, then
      ;; stop drawing rectangles further inward.
      (unless (and (plusp (box-width box-to-draw))
                   (plusp (box-height box-to-draw)))
        (return))
      
      ;; Draw a box.
      (draw-box window box-to-draw)
      
      ;; Reduce the size of the box and loop back to draw
      ;; another window inside the most recent one.
      (inflate-box box-to-draw -9 -9))))

;;; These next methods are necessary because the redisplay-window
;;; methods above draw differently depending on the current size
;;; of the panes.

(defmethod invalidate-window-on-resize ((window my-ellipse-pane))
  
  ;; The ellipse will fill this window pane, and so resizing the
  ;; pane changes the content of the entire window (rather than
  ;; simply bringing more of the picture into view).  This method
  ;; causes the entire pane to be redrawn when it is resized,
  ;; rather than redrawing only the newly-revealed portion.
  t)

(defmethod invalidate-window-on-resize ((window my-box-pane))
  t)

(defmethod resize-window :after ((window my-ellipse-pane) size)
  (declare (ignore size))
  
  ;; This makes things smoother when the parent window is resized
  ;; by immediately redrawing the child pane.
  (update-window window))

(defmethod resize-window :after ((window my-box-pane) size)
  (declare (ignore size))
  (update-window window))

(defmethod move-window :after ((window my-box-pane) position)
  (declare (ignore position))
  
  ;; When resizing the parent horizontally only, this pane does not
  ;; resize, but it does move within its parent and needs to be redrawn,
  ;; so do so immediately to make that operation smoother.
  (update-window window))

;;; This next (admittedly odd) method prevents the user from
;;; interactively resizing the frame window so that it is
;;; too small to hold both of its panes at their minimum size.

(defmethod track-limits ((window my-frame)
                         maximized-size maximized-position
                         minimum-tracking-size maximum-tracking-size)
  
  ;; The attachments of the panes will be set up below so that resizing
  ;; the frame keeps the righthand pane at its current size.  So don't
  ;; let the user resize the window smaller than the current size
  ;; of the righthand pane plus the minimum size for the lefthand pane
  ;; plus the size of the divider between the panes.
  (setf (position-x minimum-tracking-size)
    (+ (width (find-window :right-pane window))
       (minimum-pane-width window)
       (divider-width window)))
  
  ;; Always return the four arguments from a track-limits method.
  (values maximized-size maximized-position
          minimum-tracking-size maximum-tracking-size))
                         
(defun run-split-bar-example ()
  (let* ((width 700)
         (height 300)
         (half-width (round width 2))
         (frame (make-window :frame
                  :class 'my-frame
                  :title
                  "Frame with Two Panes - Resize Me and Drag My Divider"
                  
                  ;; Define a particular interior size for the frame
                  ;; so that we can fit the panes to that interior.
                  :interior (make-box-relative 200 200 width height)
                  
                  ;; Don't allow scrolling the panes through the frame.
                  :scrollbars nil
                  
                  ;; Make the window hidden until the child panes
                  ;; are all ready, and then expose it all at the end.
                  :state :shrunk))
         (minimum-pane-width (minimum-pane-width frame))
         (divider-width (divider-width frame))
         (divider-left (- half-width (round divider-width 2)))
         (divider-right (+ divider-left divider-width))
         )
    
    (make-window :left-pane
      :class 'my-ellipse-pane
      
      ;; These two options make the pane be a child of the frame.
      :owner frame
      :child-p t
      
      ;; Don't add any kind of border to the pane.
      :title-bar nil
      :border :none
      :resizable nil
      :scrollbars nil
      
      ;; These attachments make the lefthand pane resize itself
      ;; along with its parent when the user resizes the frame.
      :right-attachment :right
      :bottom-attachment :bottom
      
      ;; This option avoids any flashing when the pane redraws,
      ;; especially as the frame is resized.
      :double-buffered t
      
      ;; Fit this pane into the left half of its parent window.
      :exterior (make-box 0 0 divider-left height))
    
    ;; Make the righthand pane.
    (make-window :right-pane
      :class 'my-box-pane
      :owner frame
      :child-p t
      :title-bar nil
      :border :none
      
      ;; When the user resizes the parent window, keep this pane
      ;; the same width but move it so that it stays at the
      ;; right edge of the parent.  If you'd rather keep the
      ;; two panes proportional, passing :scale would work for
      ;; that, but would not keep the divider area a constant
      ;; width.  So you'd need to use a resize-window :after method
      ;; on the frame class instead of using these attachments.
      :left-attachment :right
      :right-attachment :right
      
      ;; But still resize it vertically to fill the parent.
      :bottom-attachment :bottom
      
      :resizable nil
      :scrollbars t
      :double-buffered t
      :exterior (make-box divider-right 0 width height))
    
    ;; Add a split-bar widget between the two panes.
    (add-component (make-instance 'split-bar
                     :orientation :vertical
                     
                     ;; The value property of a split-bar widget
                     ;; is the position of its "front edge" in
                     ;; the direction that the bar slides.
                     :value divider-left
                     
                     ;; This indicates which sibling widgets or
                     ;; windows to resize when the split-bar slides.
                     ;; Another option is :widgets-to-move.
                     :widgets-to-resize '(:left-pane :right-pane)
                     
                     :cursor-name :vertical-splitbar-cursor
                     :bar-thickness divider-width
                     :dragging-thickness divider-width
                     :color-when-idle light-red
                     :color-under-mouse light-red
                     :color-when-dragging red
                     :min-space-before minimum-pane-width
                     :min-space-after minimum-pane-width
                     :left-attachment :right
                     :right-attachment :right)
                   frame)
    
    ;; Now that everything is ready, this call with make the
    ;; window visible, bring it to the front, and give it the
    ;; keyboard focus.
    (select-window frame)
    
    ;; Return the frame window.
    frame))

;;; Evaluate this form to run the test.
#+run-example
(run-split-bar-example)

