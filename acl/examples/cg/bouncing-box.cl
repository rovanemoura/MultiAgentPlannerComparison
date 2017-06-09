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

;;;; Graphics and events (bouncing boxes)

(in-package :cg-user)

;; Introduction 
;; 
;; We define a new type of window BOXES-WINDOW that depicts 2 colorful boxes.
;; Whenever our window gets a message it moves the boxes by one pixel. 
;; They bounce off the borders of the window and each other.

;; In addition to moving the boxes on mouse and keyboard events, a timer
;; is also set up to move the boxes automatically by one pixel every
;; one-hundredth of a second (every 10 milliseconds).

;; Several BOXES-WINDOWs can be open at once with out interference.
;; The timer messages will be sent to all of the open BOXES-WINDOWs, while
;; each window responds independently to other events that occur within it.

;;;; Try doing the following: 
;;;; 
;;;;    1) Run this example to open a BOXES-WINDOW 
;;;;          The boxes will move as timer events are directed to the selected 
;;;;          window 
;;;; 
;;;;    2) Click on some other window to select it 
;;;;          The boxes stop moving because the timer events are going to the 
;;;;          text window 
;;;; 
;;;;    3) Move the mouse in circles over the BOXES-WINDOW 
;;;;          The boxes start moving because mouse-moved events are directed to 
;;;;          the BOXES-WINDOW 
;;;; 
;;;;    4) Click on the boxes window
;;;;          The boxes will move fast because a click is programmed to move
;;;;          300 steps at once, instead of 1 step as with other events.

(defclass boxes-window (frame-with-single-child)())

(defclass boxes-pane (bitmap-pane)
    ((box1 :accessor box1)
     (box2 :accessor box2)
     (step1 :accessor step1)
     (step2 :accessor step2)
     (previous-message-time
      :initform 0
      :accessor previous-message-time)
     (previous-boink-time
      :initform 0
      :accessor previous-boink-time)))

(defmethod default-pane-class ((w boxes-window)) 'boxes-pane)

(defmethod device-open ((window boxes-pane) slot-names options)
  (declare (ignore slot-names options))
  (when (call-next-method) ;; open the window
    ;; add the boxes
    (setf (box1 window) (make-box-relative 0 0 30 30))
    (setf (box2 window) (make-box-relative 100 60 30 30))
    (setf (step1 window) (make-position 1 1)) 
    (setf (step2 window) (make-position 1 1))
    (move-boxes-in-window window)
    t))

;; A place to cache a single timer object for this application.
(defvar *bouncing-boxes-timer* nil)

;; A function to return our timer, creating it the first time it's needed.
(defun bouncing-boxes-timer ()
   (or *bouncing-boxes-timer*
       (setq *bouncing-boxes-timer*
         (make-instance 'timer
           :name :bouncing-boxes-timer
           :interval 10 ;; milliseconds
           :on-timer 'bouncing-boxes-on-timer))))

;; The on-timer event-handler for our timer, which will be invoked
;; each time our timer fires.
(defun bouncing-boxes-on-timer (timer)
  ;; We cached the bouncing-boxes panes in the timer-info slot of
  ;; the timer so that we can retrieve them here when the asynchronous
  ;; timer events occur, and then pass each pane to our main function.
  (dolist (window (timer-info timer))
    (bouncing-boxes-event window 'timer)))

;; Move the boxes as the user moves the mouse over the window.
(defmethod mouse-moved :after ((window boxes-pane) buttons data)
   (declare (ignore buttons data))
   (bouncing-boxes-event window 'mouse-moved))

;; Move the boxes as the user presses keys while the window has
;; the keyboard focus.
(defmethod virtual-key-down :after ((window boxes-pane) buttons data)
   (declare (ignore buttons data))
   (bouncing-boxes-event window 'virtual-key-down))

(defun bouncing-boxes-event (window message-name)
  
  ;; When selected mouse, keyboard, and timer events occur,
  ;; advance the moving boxes by one pixel and report on the type
  ;; of event that the window has received.
  (let* ((statbar (status-bar (parent-or-owner window)))
         (new-time (get-internal-real-time))
         count)
    
    ;; Don't cover non-timer messages with timer messages
    ;; until a second has passed.
    (unless (or (and (eq message-name 'timer)
                     (> (+ (previous-message-time window)
                           internal-time-units-per-second)
                        new-time))
                ;; And NEVER cover up a Boink! message within a second.
                (> (+ (previous-boink-time window)
                      internal-time-units-per-second)
                   new-time))
      (unless (eq message-name 'timer)
        (setf (previous-message-time window) new-time))
      
      ;; Show the message name in the left compartment of the status bar.
      (status-bar-message statbar
                          (princ-to-string message-name) :part 0)
      
      ;; Show the count for this type of message in the right compartment.
      (setf (getf (plist message-name) :bouncing-box-count)
        (setq count (1+ (or (getf (plist message-name)
                                  :bouncing-box-count)
                            0))))
      (status-bar-message statbar 
                          (princ-to-string count) :part 1))
    
    ;; Move the boxes a single increment for each incoming event.
    (move-boxes-in-window window)))

(defmethod mouse-left-down ((window boxes-pane) buttons data)
  (declare (ignore buttons data))
  
  (dotimes (j 1000)
    (move-boxes-in-window window)))

(defmethod do-boink ((window boxes-pane))
  (setf (previous-boink-time window) (get-internal-real-time))
  (window-warning window (if (zerop (random 20)) "Wah!" "Boink!")))

(defmethod move-boxes-in-window ((window boxes-pane))
  (let ((box1 (box1 window))
        (box2 (box2 window)) 
        (step1 (step1 window))
        (step2 (step2 window))
        (top-limit 0) (left-limit 0) 
        (right-limit (visible-box-width window)) 
        (bottom-limit (visible-box-height window))
        (old-box1 #.(make-box 0 0 0 0))
        (old-box2 #.(make-box 0 0 0 0)))
    (ncopy-box old-box1 box1)
    (ncopy-box old-box2 box2)
    (when (i<= (box-top box1) top-limit) 
      (nmake-position step1 (position-x step1) 1))
    (when (i>= (box-bottom box1) bottom-limit) 
      (nmake-position step1 (position-x step1) -1))
    (when (i<= (box-left box1) left-limit) 
      (nmake-position step1 1 (position-y step1)))
    (when (i>= (box-right box1) right-limit) 
      (nmake-position step1 -1 (position-y step1)))
    (when (i<= (box-top box2) top-limit) 
      (nmake-position step2 (position-x step2) 1))
    (when (i>= (box-bottom box2) bottom-limit) 
      (nmake-position step2 (position-x step2) -1))
    (when (i<= (box-left box2) left-limit) 
      (nmake-position step2 1 (position-y step2)))
    (when (i>= (box-right box2) right-limit) 
      (nmake-position step2 -1 (position-y step2)))
    (when (box-intersect-p (inflate-box
                            (ncopy-box #.(make-box 0 0 0 0) box1)
                            1 1)
                           box2)
      (when (i<= (iabs (i- (box-top box2) (box-bottom box1))) 2)
        (do-boink window)
        (nmake-position step1 (position-x step1) -1)
        (nmake-position step2 (position-x step2) 1))
      (when (i<= (iabs (i- (box-bottom box2) (box-top box1))) 2) 
        (do-boink window)
        (nmake-position step1 (position-x step1) 1)
        (nmake-position step2 (position-x step2) -1))
      (when (i<= (iabs (i- (box-left box2) (box-right box1))) 2) 
        (do-boink window)
        (nmake-position step1 -1 (position-y step1))
        (nmake-position step2 1 (position-y step2)))
      (when (i<= (iabs (i- (box-right box2) (box-left box1))) 2) 
        (do-boink window)
        (nmake-position step1 1 (position-y step1))
        (nmake-position step2 -1 (position-y step2))))
    
    ;; Draw the boxes in their new positions.
    ;; Use a little trick to erase and draw only on the backing-store
    ;; bitmap of the bitmap-pane by using with-delayed-redraw.  When
    ;; this form exits, the window is invalidated which causes the new
    ;; scene to be quickly copied from the backing-store bitmap to the
    ;; visible window all at once.  This reduces unpleasant flashing.
    ;; An alternative is to turn on the double-buffered property of
    ;; a regular windowd (not a bitmap-pane).
    (with-delayed-redraw (window :invalidate t)
      (erase-contents-box window (nvisible-box window #.(make-box 0 0 0 0)))
      (with-foreground-color (window green)
        (fill-box window (nbox-move box1 step1)))
      (with-foreground-color (window yellow)
        (fill-box window (nbox-move box2 step2)))
      (draw-box window box1)
      (draw-box window box2))
    (update-window window)
    ))

;; This function that runs this example has been written so that
;; it can be used as the on-initialization function of a standalone
;; application.  To make this work, it returns a window such that
;; when the window is closed, the example is considered to be terminated.

(defun run-bouncing-box-example ()
   (let* ((frame (make-window :boxes-window
                   
                   :class 'boxes-window
                   :owner (development-main-window *system*)
                   :title "Bouncing Boxes"
                   :scrollbars nil
                   :page-width 222
                   :page-height 180
                   :exterior (make-box 520 240 742 420)
                   :state :shrunk))
          (statbar (add-common-status-bar frame))
          (timer (bouncing-boxes-timer)))
      
      ;; Save the application window on the timer for later retrieval,
      ;; and start the timer running.
      (push (frame-child frame)(timer-info timer))
      (start-timer timer)
      
      ;; Use a two-part status-bar for informational messages.
      (setf (parts statbar) '(t 60))
      
      ;; Expose the application window now that everything is ready.
      (select-window frame)
      frame))

;; Before closing each bouncing boxes window, remove it from the
;; list of windows that are to receive our timer messages.
(defmethod close :before ((window boxes-window) &key abort)
   (declare (ignore abort))
   (let* ((timer (bouncing-boxes-timer)))
      (setf (timer-info timer)
            (delete (frame-child window)(timer-info timer)))
      
      ;; Stop the timer after the last bouncing boxes window
      ;; has been closed.
      (unless (timer-info timer)
         (stop-timer timer))))

#+run-example
(run-bouncing-box-example)
