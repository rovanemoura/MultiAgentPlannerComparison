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

;; How to Define a Lisp Widget --- Vertical Gauge Example

;; This file implements a "vertical gauge" widget as an example
;; of defining a new widget completely in lisp.  The gauge draws
;; a vertical bar beside a numeric scale, and the user can click
;; or drag to move the bar to a new value.  You can use this sample
;; code as a template for defining your own widgets, using arbitrary
;; lisp code to tell the widget how to draw itself and how to
;; respond to mouse clicks and keypresses.

;; While it is possible to make a generic window act like a widget
;; by simply giving it suitable drawing and mousing behavior, we
;; have set up the classes lisp-widget and lisp-widget-window
;; that you can subclass in order to create a common graphics-style
;; widget, where a dialog-item object is associated with a window
;; object, the dialog-item's on-set-focus is called automatically
;; at the appropriate time, and so on.

;; The steps that you must take include:
;; * Define your dialog-item and widget-window classes.
;; * Write a device-open method that sets up the widget-window initially.
;; * Define a redisplay-window method that draws the widget.
;; * Define event-handling methods to handle mouse clicks and keypresses.
;; These steps are detailed in the code below.

;; This file defines the widget for programmatic creation.

(in-package :cg-user)

;; ---------------------------------------------------------------------
;; Define the widget classes.

;; Define the new dialog-item class.  Define slots as needed to
;; hold any custom widget attributes or internal information.

;; The defcomponent definer is like defclass except that it also
;; allows specification of properties
(defclass vertical-gauge (lisp-widget)
  
  ;; The domain increment between major tic marks
  ((major-increment 
    :initarg :major-increment
    :initform 10
    :accessor major-increment)
   
   ;; The domain increment between minor tic marks
   (minor-increment 
    :initarg :minor-increment
    :initform 5
    :accessor minor-increment)
   
   ;; The value to round by after the user clicks or drags
   (resolution 
    :initarg :resolution
    :initform 1
    :accessor resolution)
   
   ;; Internal slots below here
   
   ;; Cache the computed pixels-per-domain-unit here
   (scale 
    :initform 1)
   
   ;; The region of the widget to invalidate to force a redraw
   ;; of the bar but not the scale (for efficiency)
   (bar-clip-box 
    :initform (make-box 0 0 0 0))
   
   ;; A pixel value to which the bar currently extends, to use for
   ;; efficiently lengthening or shortening the bar during a drag.
   (bar-top 
    :initform nil)
   
   ;; Make every instance of this class use a standard margin and
   ;; color that could be overridden by a subclass.  These could be
   ;; made into instance slots to allow individual widget variation.
   (margin 
    :allocation :class
    :initform 10)
   (bar-color 
    :allocation :class
    :initform blue)
   
   ;; Keep track of when we need to draw the focus rectangle.
   (has-focus-p 
    :initform nil)
   ) ;; end of slots
  (:default-initargs
      :border :static
    :double-buffered t
    
    ;; Use the inherited range attribute that all dialog-items have
    ;; to specify the minimum and maximum values on the scale.
    :range (list 0 100)))
 
;; Define some properties for the widget, which the user can modify
;; in the inspector to see immediate side effects.  These properties
;; are read and written using the slot accessor defined
;; above for the slots with the same names
(defproperties vertical-gauge
  (major-increment
   :type integer
   :help-string "The domain increment between major tic marks"
   :editor-type :short-expression)
  (minor-increment
   :type integer
   :help-string "The domain increment between minor tic marks"
   :editor-type :short-expression)
  (resolution
   :type integer
   :help-string "The value to round by after the user clicks or drags"
   :editor-type :short-expression)
  (value ;; <517>
   :help-string "Value indicated by the scroll position"
   :editor-type :long-expression)
  (range
   :type cg::integer-range
   :help-string "List of possible values"
   :editor-type :long-expression)
  (background-color
   :type cg::widget-color
   :help-string "Color of the background of the widget"
   :editor-type :color))
 
;; Define the widget-window class to associate with the dialog item.
;; If your widget uses a hierarchy of windows, make child windows be
;; subclasses of lisp-widget-window, but make the topmost window
;; be a subclass of lisp-widget-top-window only.  (Any subwindows could
;; be created in the device-open method for the topmost window.)
 
(defclass vertical-gauge-pane (lisp-widget-top-window)
  ())

;; Tell the dialog-item class which widget-window class to instantiate.

(defmethod widget-device ((widget vertical-gauge)(dialog basic-pane))
  'vertical-gauge-pane)

;; ---------------------------------------------------------------------
;; When the user changes these properties, redisplay the widget to
;; to reflect the changes as a side effect.

(defmethod (setf major-increment) :after (value (widget vertical-gauge))
  (declare (ignore value))
  (invalidate widget))

(defmethod (setf minor-increment) :after (value (widget vertical-gauge))
  (declare (ignore value))
  (invalidate widget))

;; ---------------------------------------------------------------------
;; Creating the widget-window instance

;; This method will be called when the lisp-widget-window stream
;; is opened (inside the call to make-window or add-component).

(defmethod device-open ((widget-window vertical-gauge-pane)
                        slot-names options)
  (declare (ignore slot-names))
  (let* ((widget (dialog-item widget-window))
         (range (getf options :range)))
    
    ;; Set the initial value of the widget
    ;; Pass recursive-p to not draw the widget yet
    (setf (value widget t)
      (or (value widget)
          (first range)
          0)) 
    
    ;; Set the initial range of the widget
    (when range
      (setf (range widget) range)) 
    
    ;; Create the actual window on the screen.
    (open-lisp-widget-window widget-window options)
    
    ;; Return true for a successful open.
    t))

;; ---------------------------------------------------------------------
;; Updating the widget's value and range

;; This method will be called whenever the widget's value is changed
;; either interactively or programmatically, to invoke any needed
;; side effects.

(defmethod widget-set-value ((widget-window vertical-gauge-pane)
                             (widget vertical-gauge)
                             new-value old-value recursive-p)
  
  ;; This fancy widget-set-value method is supplied to draw only
  ;; the minimum area of the widget that is needed to update the
  ;; widget's image, and only when necessary at all.  This eliminates
  ;; flashiness that would otherwise occur while dragging the slider
  ;; as the widget is redrawn entirely at each step.
  
  ;; Reject an invalid value.
  (unless (numberp new-value)
    (return-from widget-set-value nil))
  
  (let* ((clip-box (slot-value widget 'bar-clip-box))
         (range (range widget)))
    
    ;; Disallow values outside the widget's range.
    (unless (<= (first range)
                new-value
                (second range))
      (return-from widget-set-value nil))
    
    ;; Do nothing if the value is being restored after
    ;; an attempt to put in an invalid value
    (unless (and (numberp old-value)
                 (<= (first range)
                     old-value
                     (second range)))
      
      ;; Return non-NIL to allow the old value to be restored
      (return-from widget-set-value t))
    
    ;; We cache the bar-clip-box anytime we draw the widget, so
    ;; if this is not yet cached then there's no need for us
    ;; to redraw the widget to update the displayed value.
    (when clip-box
      
      ;; Update the bar length to reflect the new-value.
      ;; Using the old value, find what section of the bar
      ;; needs to be drawn or erased, to avoid the flash
      ;; that would come by erasing and redrawing the whole bar.
      (let* ((box (ncopy-box #.(make-box 0 0 0 0)
                             clip-box))
             (new-pos (vertical-gauge-y-from-value
                       widget-window new-value))
             (old-pos (vertical-gauge-y-from-value
                       widget-window old-value))
             )
        (setf (box-top box)(min new-pos old-pos))
        (setf (box-bottom box)(max new-pos old-pos))
        
        ;; We pass the recursive-p flag internally when we
        ;; want to avoid redundant redrawing.
        (unless recursive-p
          
          ;; Force only the changed part of the bar to redraw.
          (invalidate widget :box box))))
    
    ;; Return non-NIL to accept the new value
    t))

;; This method will be called for side effects after the user
;; programmatically changes the range of the widget.

(defmethod widget-set-range ((widget-window vertical-gauge-pane)
                             (widget vertical-gauge)
                             new-value old-value recursive-p)
  (declare (ignore old-value))
  
  ;; Reject an invalid range.
  (unless (and (consp new-value)
               (numberp (first new-value))
               (numberp (second new-value))
               (> (second new-value)(first new-value)))
    (return-from widget-set-range nil))
  
  ;; Force the current value to be within the new range.
  (unless (<= (first new-value)
              (value widget)
              (second new-value))
    
    ;; Don't redisplay in case the new value is not in the old range.
    (setf (value widget t)
      (max (first new-value)
           (min (value widget)
                (second new-value))))) 
  
  ;; We use the recursive-p flag when we internally want to
  ;; change the range without redrawing.
  (unless recursive-p
    
    ;; Force the whole widget to be redrawn to display the new
    ;; scale and to adjust the bar's position to that scale.
    (invalidate widget))
  
  ;; Return non-NIL to accept the new range
  t)

;; ---------------------------------------------------------------------
;; Drawing the widget

;; This method will be called whenever part or all of the widget
;; needs to be redrawn (due to uncovering it or changing its value
;; or range).

(defmethod redisplay-window ((widget-window vertical-gauge-pane)
                             &optional box)
  
  ;; The default method for basic-pane will draw the blank background
  (call-next-method)
  
  ;; Read parameters needed for drawing from the widget's slots
  (let* ((widget (dialog-item widget-window))
         (range (range widget))
         (min-value (first range))
         (max-value (second range))
         (major-increment (major-increment widget))
         (minor-increment (minor-increment widget))
         (margin (slot-value widget 'margin))
         (range-size (- max-value min-value))
         (usable-size (i- (interior-height widget-window)
                          (i* 2 margin)))
         (scale (/ usable-size range-size))
         (labels nil)
         (max-label-width 0)
         (font (font widget-window))
         (font-height (font-size font))
         (half-font-height (ceiling font-height 2))
         (right-margin (i- (interior-width widget-window)
                           margin))
         (bottom-margin (i- (interior-height widget-window)
                            margin))
         long-tic-x short-tic-x bar-x)
    
    ;; Cache this computed value away for use during click-dragging.
    (setf (slot-value widget 'scale) scale)
    
    ;; Format all of the major tic labels ahead of time so that
    ;; we can see how wide the widest label is and reserve that
    ;; much of the widget width for the tic labels.
    (do* ((value min-value (+ value major-increment))
          label)
         ((> value max-value))
      (setq label (format nil "~a" value))
      (setq labels (nconc labels (list label)))
      (setq max-label-width (max max-label-width
                                 (stream-string-width 
                                  widget-window label))))
    
    ;; Divide the remaining width of the widget between the
    ;; bar width and the major tic length, and make the minor
    ;; tics be half the length of the major tics.
    (setq long-tic-x (i+ margin max-label-width))
    (setq bar-x (floor (i+ long-tic-x right-margin)
                       2))
    (setq short-tic-x (floor (i+ long-tic-x bar-x) 2))
    
    ;; Avoid even trying to display the text and lines if the
    ;; clipping region that was passed in indicates that only
    ;; the bar needs to be redrawn, as when using the arrow
    ;; keys to change the value.  This will speed up redraw
    ;; whenever the value but not the range has changed.
    (when (< (box-left box) bar-x)
      
      ;; Draw each major tic mark and its label
      (do* ((value min-value (+ value major-increment))
            (labels labels (cdr labels))
            y)
           ((null labels))
        (setq y (i+ margin 
                    (- usable-size
                       (round (* (- value min-value) scale)))))
        
        ;; Draw the label on the major tic mark
        (draw-string-in-box widget-window (first labels)
                            0 (length (first labels))
                            (nmake-box #.(make-box 0 0 0 0)
                              margin (i- y half-font-height)
                              long-tic-x (i+ y half-font-height))
                            :right :center)
        
        ;; Draw the major tic mark itself
        (draw-line widget-window
                   
                   ;; This #. technique is a handy way to cons a position
                   ;; at compile time to reduce runtime consing, but you
                   ;; must be certain that the position is not going to
                   ;; be overwritten while its previous value is still
                   ;; being used
                   (nmake-position #.(make-position 0 0)
                     long-tic-x y)
                   (nmake-position #.(make-position 0 0)
                     bar-x y))
        
        ;; Draw the minor tic marks between this major
        ;; tic mark and the next major tic mark.
        (do* ((next-major-value (+ value major-increment))
              (minor-value (+ value minor-increment)
                           (+ minor-value minor-increment)))
             ((or (>= minor-value next-major-value)
                  (> minor-value max-value)))
          (setq y (i+ margin
                      (- usable-size
                         (round (* (- minor-value min-value)
                                   scale)))))
          (draw-line widget-window
                     (nmake-position #.(make-position 0 0)
                       short-tic-x y)
                     (nmake-position #.(make-position 0 0)
                       bar-x y)))))
    
    ;; Draw the main bar that indicates the current widget value
    (with-foreground-color (widget-window (slot-value widget 'bar-color))
      (fill-box widget-window
                (nmake-box #.(make-box 0 0 0 0)
                  bar-x 
                  (max margin
                       (setf (slot-value widget 'bar-top)
                         (i+ margin 
                             (- usable-size
                                (round (* (- (value widget)
                                             min-value)
                                          scale))))))
                  (i1+ right-margin)
                  (i1+ bottom-margin)
                  )))
    
    ;; Draw the focus rectangle near the outer edge of the widget
    ;; if the widget currently has the keyboard focus
    (vertical-gauge-draw-focus widget-window
                               (slot-value widget 'has-focus-p))
    
    ;; Cache the region occupied by the bar for efficient
    ;; lengthening and shortening of it during click-dragging
    (setf (slot-value widget 'bar-clip-box)
      (nmake-box (slot-value widget 'bar-clip-box)
        bar-x margin
        (i1+ right-margin)
        (i1+ bottom-margin)))
    ))

;; Draw a rectangle to show when this widget has the keyboard focus.
(defun vertical-gauge-draw-focus (widget-window has-focus)
  (let* ((box (nvisible-box widget-window
                            #.(make-box 0 0 0 0))))
    
    ;; Shrink the box to draw a focus rectangle that has
    ;; a small margin from the interior edge.
    ;; Decrement the right and bottom sides by one more pixel
    ;; since draw-box draws those side at the pixel just beyond
    ;; the width and height of the specified box.
    (incf (box-left box) 2)
    (incf (box-top box) 2)
    (decf (box-right box) 3)
    (decf (box-bottom box) 3)
    
    ;; Use invert (xor) drawing mode to draw the focus rectangle,
    ;; so that it can be erased simply by drawing it again rather
    ;; than drawing the whole widget when the keyboard focus moves
    (with-foreground-color
        (widget-window (if has-focus
                           gray
                         (effective-background-color widget-window)))
      (draw-box widget-window box))))

;; ---------------------------------------------------------------------
;; Mouse clicking and dragging

;; Handle a left click to snap or drag the bar to a new position

(defmethod mouse-left-down ((widget-window vertical-gauge-pane)
                            buttons data)
  (declare (ignore buttons data))
  (let* ((widget (dialog-item widget-window))
         (clip-box (slot-value widget 'bar-clip-box))
         (bar-top (slot-value widget 'bar-top))
         (left (box-left clip-box))
         (top (box-top clip-box))
         (right (box-right clip-box))
         (bottom (box-bottom clip-box))
         (previous-y nil)
         y round-y)
    
    ;; Loop for each mouse-position during a drag.
    (loop
      
      ;; Find where the mouse cursor is within the bar's range.
      ;; If the user clicks or drags outside the range, constrain
      ;; the value to be within the bar's allowed range.
      (setq y (min bottom
                   (max top
                        (position-y
                         (ncursor-position widget-window
                                           #.(make-position 0 0))))))
      
      ;; To avoid consing, just loop idly if the user has not
      ;; moved the mouse since the previous time that we checked.
      (unless (eq y previous-y)
        (setq previous-y y)
        
        ;; Map the mouse pixel position to a domain value,
        ;; round it off by the resolution of the widget, and
        ;; map the rounded domain value back to a pixel value.
        (setq round-y (vertical-gauge-round-y widget-window y))
        
        ;; Rather than erasing and redrawing the whole bar
        ;; (which would flash), draw up from the previous
        ;; bar value (or erase down from the previous value)
        ;; to draw only the changed section of the bar.
        ;; Draw nothing if the rounded value is the same as
        ;; the previous rounded value.
        (cond ((< round-y top)) ;; rounded move is off the top of scale
              ((> round-y bar-top)
               (erase-contents-box widget-window
                                   (nmake-box #.(make-box 0 0 0 0)
                                     left (max top bar-top) right round-y)))
              ((< round-y bar-top)
               (with-foreground-color (widget-window
                                       (slot-value widget 'bar-color))
                 (fill-box widget-window
                           (nmake-box #.(make-box 0 0 0 0)
                             left (max top round-y)
                             right bar-top)))))
        (setq bar-top round-y))
      
      ;; On GTK this is necessary when checking mouse button state
      ;; in a loop.
      #+gtk (process-pending-events)

      ;; Exit once the user has released the left mouse button.
      (unless (key-is-down-p vk-lbutton)
        
        ;; Recache the current position of the bar for next time.
        (setf (slot-value widget 'bar-top) round-y)
        
        ;; Store the new official value of the widget
        ;; Pass the recursive-p flag to tell ourselves not to
        ;; redraw the new value, since we've done that here.
        (setf (value widget t)
          (vertical-gauge-value-from-y widget-window round-y))
        (return))
      ))
  t)

;; ---------------------------------------------------------------------
;; Utility computation functions

(defun vertical-gauge-value-from-y (widget-window y)
  "Map from a domain value to a pixel value"
  (let* ((widget (dialog-item widget-window))
         (margin (slot-value widget 'margin))
         (scale (slot-value widget 'scale))
         (resolution (resolution widget))
         (min-value (first (range widget)))
         )
    (+ (* resolution
          (round (/ (i- (i- (interior-height widget-window) y)
                        margin)
                    scale)
                 resolution))
       min-value)))

(defun vertical-gauge-y-from-value (widget-window value)
  "Map from a pixel value to a domain value"
  (let* ((widget (dialog-item widget-window))
         (margin (slot-value widget 'margin))
         (scale (slot-value widget 'scale))
         (min-value (first (range widget)))
         )
    (i- (interior-height widget-window)
        (i+ margin
            (round (* (- value min-value)
                      scale))))))

(defun vertical-gauge-round-y (widget-window y)
  "Temporarily convert a pixel-value to a domain value to round by the resolution"
  (let* ((widget (dialog-item widget-window))
         (margin (slot-value widget 'margin))
         (scale (slot-value widget 'scale))
         (window-height (interior-height widget-window))
         (resolution (resolution widget))
         )
    (i- window-height
        (i+ margin
            (round (* resolution
                      (round (/ (i- (i- window-height y)
                                    margin)
                                scale)
                             resolution)
                      scale))))))

;; ---------------------------------------------------------------------
;; Handle keypresses

(defmethod virtual-key-down ((widget-window vertical-gauge-pane)
                             buttons data)   
  (case data
    
    ;; Make the DOWN arrow key move the value down by
    ;; the resolution amount, or by the major-increment amount
    ;; if the control key is held down.
    (#.vk-down
     (let* ((widget (dialog-item widget-window)))
       (decf (value widget)
             (if (logtest control-key buttons)
                 (major-increment widget)
               (resolution widget)))))
    
    ;; Make the UP arrow key move the value up by
    ;; the resolution amount, or by the major-increment amount
    ;; if the control key is held down.
    (#.vk-up
     (let* ((widget (dialog-item widget-window)))
       (incf (value widget)
             (if (logtest control-key buttons)
                 (major-increment widget)
               (resolution widget)))))
    
    ;; Make TAB move the keyboard focus to the next widget.
    (#.vk-tab
     (if (logtest alt-key buttons)
         (call-next-method) ;; Let alt-tab do the usual thing
       (tab-to-next (dialog-item widget-window) nil)))
    
    ;; Make SHIFT-TAB move the keyboard focus to the previous widget.
    (#.vk-backtab
     (if (logtest alt-key buttons)
         (call-next-method) ;; Let alt-tab do the usual thing
       (tab-to-next (dialog-item widget-window) t)))
    (t
     (call-next-method))))

;; ---------------------------------------------------------------------
;; Miscellaneous methods

(defmethod resize-window :after ((window vertical-gauge-pane) position)
  (declare (ignore position))
  
  ;; Redraw the widget at its new dimensions after resizing the widget.
  (invalidate window))

(defmethod lisp-widget-draw-focus ((widget-window vertical-gauge-pane)
                                   (widget vertical-gauge))
  
  ;; A standard approach to maintain the focus rectangle
  ;; only when the widget has the keyboard focus.
  (setf (slot-value widget 'has-focus-p) t)
  (vertical-gauge-draw-focus widget-window t)
  t)

(defmethod lisp-widget-clear-focus ((widget-window vertical-gauge-pane)
                                    (widget vertical-gauge))
  
  ;; A standard approach to maintain the focus rectangle
  ;; only when the widget has the keyboard focus.
  (setf (slot-value widget 'has-focus-p) nil)
  (vertical-gauge-draw-focus widget-window nil)
  t)

;; ---------------------------------------------------------------------
;; A tester function

;; Create a sample vertical-gauge "application" in a dialog.

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
(defun run-vertical-gauge-example ()
  (let* ((dialog
          (make-window :vertical-gauge-dialog
            :class 'dialog 
            :owner (development-main-window cg::*system*)
            :title "Vertical Gauge"
            :resizable t
            :interior (make-box 500 200 700 360)
            :widgets
            (list (make-instance 'vertical-gauge
                    :left 10 :top 10 :width 80 :height 140
                    :name :gauge
                    :value 350
                    :range '(0 500)
                    :major-increment 100
                    :minor-increment 50
                    :resolution 25
                    :font (make-font-ex nil :arial 11)
                    :right-attachment :right
                    :bottom-attachment :bottom)
                  
                  (make-instance 'static-text
                    :left 110 :top 10 :width 80 :height 24
                    :name :max-label
                    :value "~Maximum"
                    :left-attachment :right
                    :right-attachment :right)
                  
                  ;; Use a single-item-list to change the range
                  ;; of the gauge above.
                  (make-instance 'single-item-list
                    :left 110 :top 35 :width 80 :height 115
                    :name :max
                    :value 500
                    :range (list 250 500 750 1000)
                    :on-change
                    '(lambda (widget new-value old-value)
                       (setf (range (find-sibling :gauge widget))
                         (list 0 new-value)))
                    :left-attachment :right
                    :right-attachment :right
                    :bottom-attachment :bottom)
                  ))))
    (set-focus-component (find-component :gauge dialog))
    dialog))

#+run-example
(run-vertical-gauge-example)
