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

;; Customizing window behavior by adding methods to a window subclass.

;; This is a general example of the sort of method-writing you
;; typically need to do in order to create windows in your
;; application that know how to draw themselves and respond to
;; mouse and keyboard events as needed by your application.

;; -------------------------------------------------------------
;; To add our own window methods, we must first create our
;; own window subclass to specialize on.  If a window class
;; that instantiates a pane inside it is desired, we must
;; also subclass the pane class and point the frame class
;; to the pane class to instantiate.

(in-package :cg-user)

;; Create a window subclass for the frame window.
(defclass my-window (non-refreshing-window)
  ()
  (:default-initargs
      :title "My Window"
    :scrollbars t
    :page-width 1100
    :font (make-font-ex nil :arial 24)))

;; Create a pane subclass for the child pane window.
(defclass my-pane (non-refreshing-pane)
  nil)

;; Tell the frame subclass to instantiate the pane subclass.
(defmethod default-pane-class ((window my-window))
  'my-pane)

;; Above we have used a subclass of frame-with-single-child,
;; and so we needed to define a pane class to go with it.  Using
;; a pane at all is not always necessary, since you can
;; alternately instantiate and draw directly on a frame-window,
;; for example.  But using a frame-child pane comes in handy
;; if you want to add a toolbar or status bar to the frame window,
;; since these will not overlap a frame-child pane (and will not
;; interfere with scrolling the pane), or if you
;; need to use a special type of pane such as text-edit-pane
;; which has functionality not provided by the frame window itself.

;; -------------------------------------------------------------

(defparameter *test-circle-radius* 27)

(defparameter *toolbar-test-colors*
  '(red green blue yellow cyan gray light-gray white black))

(defmethod redisplay-window ((pane my-pane) &optional box-to-redisplay)
  
  ;; This method will be called whenever all or part of the pane
  ;; is uncovered and needs to be redisplayed, or whenever you call
  ;; invalidate explicitly to mark all or part of the window
  ;; as in need of redisplay.
  ;; Note that this method should be specialized on the pane rather than
  ;; on the parent frame window.  The pane covers the interior of
  ;; the frame, so anything drawn on the frame itself will not be seen.
  (call-next-method) ;; Draw the blank background
  
  ;; Retrieve the keyboard event that we stored on the window when
  ;; the most recent keypress occurred, in order to draw it in the window.
  (let* ((keyboard-event (get-stream-prop pane :keyboard-event))
         (string-box (center-box-on-window pane 60 30))
         entry position color)
    
    ;; Draw a circle at each position where the user has clicked the mouse.
    ;; Refer to the list of circle positions that we have built as the
    ;; user clicks the mouse on the window.
    (do* ((entries (get-stream-prop pane :click-positions)(rest entries)))
         ((null entries))
      (setq entry (first entries))
      (setq position (first entry))
      (setq color (second entry))
      
      ;; For efficiency, redraw only those circles that overlap the
      ;; area that needs to be redisplayed.  The OS clips outside this
      ;; region anyway, but we can save additional effort by not
      ;; even calling the drawing code at all when it would be clipped.
      (when (or (null box-to-redisplay)
                
                ;; Using with-boxes around the code that uses a
                ;; temporary box is a technique to avoid consing the box.
                (with-boxes (box1)
                  (box-intersect-p
                   box-to-redisplay
                   (nmake-box box1
                     (i- (position-x position) *test-circle-radius*)
                     (i- (position-y position) *test-circle-radius*)
                     (i+ 1 (position-x position) *test-circle-radius*)
                     (i+ 1 (position-y position)
                         *test-circle-radius*)))))
        
        (with-foreground-color (pane color)
          (fill-circle pane position *test-circle-radius*))
        (draw-circle pane position *test-circle-radius*)))
    
    ;; Draw the most recent keyboard event in a box at the center of the window.
    (when (and keyboard-event
               (or (null box-to-redisplay)
                   (box-intersect-p box-to-redisplay string-box)))
      (erase-contents-box pane string-box)
      (draw-string-in-box pane (format nil "~a" keyboard-event)
                          nil nil string-box :center :top)
      (decf (box-right string-box))
      (decf (box-bottom string-box))
      (draw-box pane string-box))))

(defmethod mouse-left-down ((pane my-pane) buttons data)
  (declare (ignore buttons))
  
  ;; This method will be called whenever the user left-clicks
  ;; in the interior of our window.  The position that was clicked
  ;; relative to the upper left corner of the window is passed as
  ;; the DATA argument.  (The DATA argument to EVENT is interpreted
  ;; differently depending on the particular MESSAGE.)
  
  ;; Draw a circle around the clicked position.
  ;; Use the selected color from the toolbar if any, or else a random color
  (let* ((frame (parent pane))
         (color-name (first (value (find-component :color
                                                   (first (toolbars frame))))))
         (color (symbol-value 
                 (or (and (not (keywordp color-name)) ;; not :random
                          color-name)
                     
                     ;; If the RANDOM button or no buttons are pressed,
                     ;; select a random color
                     (elt *toolbar-test-colors*
                          (random (length *toolbar-test-colors*)))))))
    (window-message
        frame
        "~a at ~a, ~a" 
      (or (find-if  #'(lambda (sym)(rgb-equal (symbol-value sym) color))
                   *toolbar-test-colors*)
          color)
      (position-x data)
      (position-y data))
    
    ;; Rather than actually drawing the new circle in this click method,
    ;; store each position that is clicked so that our redisplay-window
    ;; method above can redraw the circles whenever the window is uncovered.
    ;; We must copy the position structure since this argument is
    ;; destructively modified on each call.
    (push (list (copy-position data) color)
          (get-stream-prop pane :click-positions))
    
    ;; Cause the area of the new circle to be redrawn.  Calling
    ;; invalidate tells the OS to call our redisplay-window
    ;; method after all other events have been processed.  This
    ;; delayed approach can eliminate duplicate redraws.  Note that
    ;; the new circle will be drawn BELOW any overlapping circles.
    (invalidate pane
                :box (nmake-box #.(make-box 0 0 0 0)
                       (i- (position-x data) *test-circle-radius*)
                       (i- (position-y data) *test-circle-radius*)
                       (i+ 1 (position-x data) *test-circle-radius*)
                       (i+ 1 (position-y data) *test-circle-radius*)))))

(defmethod virtual-key-down ((pane my-pane) buttons data)
  (declare (ignore buttons))
  
  ;; This method will be called whenever a key on the keyboard is pressed
  ;; while our pane has the keyboard focus.
  (window-message (parent pane)
      "You pressed key number ~a" data)
  
  ;; Store the key that was pressed on the pane so that the redisplay-window
  ;; method can draw what the event was.
  (setf (get-stream-prop pane :keyboard-event) data)
  
  ;; Mark the part of the pane that displays the keyboard event
  ;; as in need of redisplay, so that our redisplay-window method
  ;; will automatically be called to draw the event.
  (invalidate pane :box (center-box-on-window pane 60 30))
  
  ;; Call the default method, which may translate this key-down event
  ;; into a CHARACTER event, which we will handle below.
  (call-next-method))

(defmethod character-message ((pane my-pane) buttons data)
  (declare (ignore buttons))
  
  ;; This method will be called for any printing characters
  ;; that are pressed on the keyboard, unless a virtual-key-down
  ;; method intercepts the keyboard event first.
  (window-message (parent pane)
      "You typed the character ~c" data)
  (setf (get-stream-prop pane :keyboard-event) data)
  (invalidate pane :box (center-box-on-window pane 60 30)))
   
(defmethod user-close ((window my-window))
  
  ;; This is called when the user tries to close the window interactively.
  ;; The window will actually be closed only if we call call-next-method
  ;; or we call CLOSE directly.
  (case (pop-up-message-dialog
         window "Close" 
         (format nil "Save changes to ~A?  (Just a test)" (title window))
         warning-icon "~Save" "~Discard" "Cancel")
    (1 (window-message window "Saved changes (not really)")
     (call-next-method)) ;; Really close the window
    (2 (window-message window "Discarded changes (not really)")
     (call-next-method)) ;; Really close the window
    (3 (window-message window "Closing cancelled"))))

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
(defun run-window-methods-example ()
  
  (let* ((frame (make-window :window-methods-test
                  :class 'my-window 
                  :owner (development-main-window *system*)
                  :title "Click me, press keys, and close me"
                  :exterior (make-box 300 100 920 580)
                  :double-buffered t
                  :state :shrunk))
         (toolbar (add-toolbar frame))
         (multipic-right 100)
         
         ;; Add a multi-picture-button with miscellaneous commands
         ;; to the toolbar.
         (multipic1 (make-instance 'multi-picture-button
                      :name :command
                      :box (make-box *toolbar-margin* *toolbar-margin*
                                     multipic-right
                                     (- (interior-height toolbar)
                                        *toolbar-margin*))
                      :on-change 'window-methods-multipic1-on-change
                      :on-set-focus 'window-methods-multipic-on-set-focus
                      :bottom-attachment :bottom
                      :range (list
                              (make-instance 'button-info
                                :name :erase-new
                                :pixmap-name :my-untrace
                                :tooltip "Erase Newest"
                                :help-string "Erases the newest circle")
                              (make-instance 'button-info
                                :name :erase-old
                                :pixmap-name :my-untrace
                                :tooltip "Erase Oldest"
                                :help-string "Erases the oldest circle")
                              (make-instance 'button-info
                                :name :erase-all
                                :pixmap-name :my-untrace-all
                                :tooltip "Erase All"
                                :help-string "Erases all the circles"))))
         
         ;; Add a second multi-picture-button for selecting the
         ;; current drawing color.
         (multipic2 (make-instance 'multi-picture-button 
                      :name :color
                      :button-3d-border t ;; not :when-pressed for "wells"
                      :left (+ multipic-right *toolbar-margin* 8)
                      :top *toolbar-margin*
                      :width (- (interior-width toolbar)
                                (* 2 *toolbar-margin*)
                                multipic-right 8)
                      :height (interior-height toolbar)
                      :on-set-focus 'window-methods-multipic-on-set-focus
                      :right-attachment :right
                      :bottom-attachment :bottom
                      :value '(:random)
                      :recessed t   ;; draw sunken "color wells"
                      :range (append 
                              (mapcar #'(lambda (color)
                                          (make-instance 'button-info
                                            :name color
                                            :color color
                                            :tooltip (format nil
                                                         "Make ~a circles"
                                                       color)))
                                *toolbar-test-colors*)
                              (list (make-instance 'button-info
                                      :name :random
                                      :width 75
                                      :title "Random"
                                      :tooltip
                                      "Alternate colors randomly"))))))
    (add-status-bar frame)
    (add-component multipic1 toolbar)
    (add-component multipic2 toolbar)
    
    ;; We created the window in :shrunk (hidden) state so that we could
    ;; add the toolbar and window and then expose the completed window
    ;; all at once.  So expose it and focus on it now.
    (select-window frame)
    
    frame))

(defun window-methods-multipic1-on-change (multipic new-value old-value)
  (declare (ignore old-value))
  (let* (;; Find the drawing pane from the multipic widget
         (frame (parent (parent multipic)))
         (pane (frame-child frame))
         (circle-items (get-stream-prop pane :click-positions))
         (value (first new-value)) ;; value is a list of button names
         pos box)
    (case value
      ((:erase-new :erase-old)
       (cond ((rest circle-items)
              (setq pos (first (first (if (eq value :erase-old)
                                          (last circle-items)
                                        circle-items))))
              (setq box (make-box
                         (- (position-x pos) *test-circle-radius*)
                         (- (position-y pos) *test-circle-radius*)
                         (+ (position-x pos) *test-circle-radius* 1)
                         (+ (position-y pos) *test-circle-radius* 1)))
              (if (eq value :erase-old)
                  (setf (rest (nthcdr (- (length circle-items) 2)
                                      circle-items)) nil)
                (pop (get-stream-prop pane :click-positions)))
              (invalidate
               pane
               ;; For efficiency, redraw only the area of the erased circle
               :box box))
             (circle-items
              (setf (get-stream-prop pane :click-positions) nil)
              (invalidate pane))
             (t (pop-up-message-dialog
                 (screen *system*) "Whoa There"
                 "No circles to erase." warning-icon :~ok))))
      (:erase-all
       (cond (circle-items
              (setf (get-stream-prop pane :click-positions) nil)
              (invalidate pane))
             (t (pop-up-message-dialog
                 (screen *system*) "Whoa There"
                 "No circles to erase." warning-icon :~ok)))))
    ;; Always keep these "pushbuttons" turned off
    (when new-value
      (setf (value multipic) nil))
    
    ;; Return non-NIL to accept the click on the multi-picture-button
    t))

(defun window-methods-multipic-on-set-focus (multipic)
  
  ;; The keypresses get sent to the front window, so after the toolbar is
  ;; exposed by clicking on the multi-picture-button, re-expose the main pane
  (bring-window-to-front
   
   ;; This expression maps from the multi-picture-buttons widget's window
   ;; to the drawing pane.
   (frame-child (parent (parent multipic))))
  t)

(cache-pixmap
 (make-instance 'pixmap
   :name :my-untrace :bits-per-pixel 4 :width 16
   :height 16 :invert-p nil :source nil :colors :default
   :contents
   '((7 0 0 7 0 0 7 7 7 7 7 7 7 7 7 7)
     (7 0 0 0 0 0 7 7 7 7 7 7 7 7 7 7)
     (7 7 0 0 0 7 7 7 7 7 7 7 7 7 7 7)
     (7 0 0 0 0 0 7 7 7 7 7 8 0 0 8 7)
     (7 0 0 7 0 0 7 7 7 7 7 0 5 5 0 7)
     (7 7 7 7 7 7 7 7 7 7 0 0 0 5 0 7)
     (7 7 7 7 7 7 7 7 7 7 0 15 11 0 0 7)
     (7 7 7 7 7 7 7 7 7 0 15 11 7 0 7 7)
     (7 7 7 7 7 7 7 7 7 0 11 7 11 0 7 7)
     (7 7 7 7 7 7 7 7 0 15 11 11 0 7 7 7)
     (7 7 7 7 7 7 7 7 0 15 15 7 0 7 7 7)
     (7 7 7 7 7 7 7 7 0 15 15 0 7 7 7 7)
     (8 8 8 8 8 8 8 8 0 12 0 7 7 7 7 7)
     (7 7 7 7 7 7 7 7 0 0 7 7 7 7 7 7)
     (12 12 12 12 12 12 12 12 0 7 7 7 7 7 7 7)
     (7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7))
   :mask-contents nil))

(cache-pixmap
 (make-instance 'pixmap
   :name :my-untrace-all :bits-per-pixel 4 :width
   16 :height 16 :invert-p nil :source nil :colors
   :default :contents
   '((7 0 0 7 0 0 7 7 7 7 7 7 7 7 7 7)
     (7 0 0 0 0 0 7 7 7 7 7 7 7 7 7 7)
     (7 7 0 0 0 7 7 7 7 7 7 7 7 7 7 7)
     (7 0 0 0 0 0 7 7 7 7 7 8 0 0 8 7)
     (7 0 0 7 0 0 7 7 7 7 7 0 5 5 0 7)
     (7 7 7 7 7 7 7 7 7 7 0 0 0 5 0 7)
     (7 0 0 7 0 0 7 7 7 7 0 15 11 0 0 7)
     (7 0 0 0 0 0 7 7 7 0 15 11 7 0 7 7)
     (7 7 0 0 0 7 7 7 7 0 11 7 11 0 7 7)
     (7 0 0 0 0 0 7 7 0 15 11 11 0 7 7 7)
     (7 0 0 7 0 0 7 7 0 15 15 7 0 7 7 7)
     (7 7 7 7 7 7 7 7 0 15 15 0 7 7 7 7)
     (8 8 8 8 8 8 8 8 0 12 0 7 7 7 7 7)
     (7 7 7 7 7 7 7 7 0 0 7 7 7 7 7 7)
     (12 12 12 12 12 12 12 12 0 7 7 7 7 7 7 7)
     (7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7))
   :mask-contents nil))

#+run-example
(run-window-methods-example)

