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

(in-package :cg-user)

;;; Scrolling by logical objects rather than by pixels.
;;; This requires a "custom scrolling scheme" where
;;; several default methods are overridden by the application.

#|
In a typical custom scrolling scheme, an application does
not need to worry about how much data is above the current
scroll position, and simply starts drawing whatever is
currently scrolled to the top of the window.  This means
that the application does not have to compute and/or cache
the pixel sizes of everything above the scroll-position,
which simplifies the coding and can also speed scrolling up
quite a bit when there is a huge amount of data to be scrolled.

The drawbacks are that (1) this approach may be applicable
only if data is entirely arranged into "rows" or "columns",
(2) scrolling is not as visually smooth since an individual
data object is always aligned with the top and/or left side
of the window, and (3) custom methods need to be written to
override several CG generic functions that are exported for
this purpose.  The generic functions to override include
scroll-to, nscroll-position, scroll-range, set-scroll-range,
update-scroll-bars-for-new-window-size, and user-scroll.
Custom methods may not be needed for all of these, depending
on how the scrolling scheme is designed.
|#

;; A custom scrolling example.

;; Define a window that displays a number of boxes in a row
;; and keeps track of how many boxes there are, their average
;; size, and how many average-size boxes fit into the window.

(defclass my-custom-scrolling-window (frame-window)
  ((side-length :initarg :side-length
                :accessor side-length
                :initform nil)
   (num-boxes :initarg :num-boxes
              :accessor num-boxes
              :initform 9)
   (boxes-scrolled-off :accessor boxes-scrolled-off
                       :initform 0)
   (box-at-left :accessor box-at-left
                :initform 0)))

;;; Since our custom scrolling scheme scrolls by our logical
;;; boxes rather than pixels and always positions one box
;;; flush against the left side of the window interior,
;;; it doesn't need to know the size of all the boxes scrolled
;;; off the left side (even though they are different sizes),
;;; and can simply (and efficiently) begin drawing with the
;;; box to which the window is currently scrolled.

(defmethod redisplay-window ((window my-custom-scrolling-window)
                             &optional box)
  (declare (ignore box))
  (call-next-method) ;; clear window
  (let* ((side (side-length window))
	 
	 ;; Side-adjustment is the amount by which to make some
	 ;; boxes smaller then average, and other boxes larger.
	 ;; We're using one-fourth of the average size.
         (side-adjustment (floor side 4))
	 
         (num-boxes (num-boxes window))
         (box-at-left (box-at-left window))
         (boxes-that-fit (ceiling (+ (visible-box-width window)
                                     side-adjustment)
                                  side))
         string box k left adjusted-side)
    
    ;; Draw only the boxes starting with the leftmost one
    ;; through how many ever can fit in the window.
    (setq left 0)
    (dotimes (j (min boxes-that-fit 
                     
                     ;; But don't draw past the end of all the boxes
                     (- num-boxes box-at-left)))
      
      ;; K is the logical box to draw, while J is its position
      ;; in the physical window (ignoring the scroll position)
      (setq k (+ j box-at-left))
      (setq adjusted-side (case (mod k 3)
                            (0 (- side side-adjustment))
                            (1 side)
                            (2 (+ side side-adjustment))))
      (setq box (make-box-relative left 0 adjusted-side side))
      (incf left adjusted-side)
      (with-foreground-color (window (case (mod k 3)
                                       (0 yellow)
                                       (1 cyan)
                                       (2 green)))
        (fill-box window box))
      (setq string (format nil "~:(~r~)" (1+ k)))
      (draw-string-in-box window string 0 (length string)
                          box :center :center)
      (with-foreground-color (window red)
        (draw-box window box)))))

;;; We must override several default CG methods that scroll
;;; by pixels in order to scroll by our custom boxes.
;;; The x component of our window's scroll-position will
;;; be the index of one our boxes in the list of boxes, rather
;;; than a number of pixels.

(defmethod nscroll-position ((window my-custom-scrolling-window)
                             position)
  (nmake-position position (box-at-left window) 0))

;;; And the horizontal scroll-range is the length of the
;;; list of boxes that can be scrolled into the window.

(defmethod scroll-range ((window my-custom-scrolling-window))
  (values (boxes-scrolled-off window) 0))

;;; When our window is resized, we must refigure how many
;;; average-size boxes now fit into the window, so that we
;;; can set the scroll-range to be the total number of boxes
;;; minus the number that fit into the window.  (The
;;; scroll-range indicates the amount of stuff outside the
;;; window that can be scrolled into it.)

(defmethod update-scroll-bars-for-new-window-size
    ((window my-custom-scrolling-window))
  (let* ((num-boxes (num-boxes window))
         (side (side-length window))
         (width (visible-box-width window))
         (boxes-that-fit (floor width side))
         (boxes-scrolled-off (- num-boxes boxes-that-fit)))
    (setf (boxes-scrolled-off window) boxes-scrolled-off)
    (set-scroll-range window boxes-scrolled-off 0
                      boxes-that-fit 0)))

;;; To scroll to a particular box position, we simply store
;;; that box as the box to which we are currently scrolled,
;;; and then redraw the window starting with that box.

(defmethod scroll-to ((window my-custom-scrolling-window)
                      position &key delay-redraw)
  (declare (ignore delay-redraw)) ;; update done by the around method
  (setf (box-at-left window)(position-x position))
  (invalidate window)
  position)

;;; The user-scroll method must be written to interpret each
;;; of the standard interactive scrolling gestures.  Here we
;;; make clicking on a scroll-bar arrow (where scroll-type
;;; is passed as :character) scroll by one box, and clicking
;;; in the body of the scroll-bar (where scroll-type is :page)
;;; scroll by two boxes.  When dragging the scroll-bar, each
;;; incremental position is passed with the x component as
;;; an index into our list of boxes because our scroll-range
;;; method above defined the scroll-range as the number of
;;; boxes in the list.

(defmethod user-scroll ((window my-custom-scrolling-window)
                        scroll-type new-position)
  (case scroll-type
    ((:thumb-continuous :thumb-finished) 
     (scroll-to window new-position))
    (t 
     (let* ((horizontal-p 
             (or (eq scroll-type :left) (eq scroll-type :right)))
            (scroll-distance 
             (case new-position
               (:character 1)
               (:page 2))))
       (when horizontal-p 
         (when (eq scroll-type :left)
           (setq scroll-distance (- scroll-distance)))
         (scroll window 
                 (make-position scroll-distance 0)))))))

;;; UPDATE:  user-scroll will not be called on the GTK platform.
;;; To control the scrolling distance, you can now instead write
;;; scroll-increment methods, which will be called on both
;;; GTK and Windows.  If the scrolling distance is the only
;;; customization that was done by your user-scroll method, then
;;; you don't need a user-scroll method on Windows either if you
;;; supply a (simpler) scroll-increment method instead.

(defmethod scroll-increment ((window my-custom-scrolling-window)
			     type direction)
  (declare (ignore direction))
  (case type
    
    ;; Scroll by a single block when the user clicks on a
    ;; scrollbar arrow.
    (:line 1)
    
    ;; Scroll by two blocks when the user clicks in the
    ;; body of the scrollbar.
    (:page 2)))

(defun run-custom-scrolling-example ()
  (let* ((side (floor (interior-width (screen *system*)) 12))
         (left 100)(top 200)
         (window (make-window :custom-scrolling
                   :class 'my-custom-scrolling-window
                   :title "Custom Scrolling"
                   :scrollbars t
                   :side-length side
                   :page-width (1+ (* side 4))
                   :page-height side
                   :double-buffered t
                   :state :shrunk
                   :interior (make-box-relative
                              left top
                              
                              ;; The two 1+'s below are to include
                              ;; the right and bottom edges of all boxes
                              (1+ (* 3 side))
                              (+ side 1
                                 (horizontal-scrollbar-thickness))))))
    (setf (font window)(make-font-ex nil "Arial" 14 '(:bold)))
    (setf (transparent-character-background window) t)
    (scroll-to window (make-position 0 0))
    (select-window window)
    window))

#+run-example (run-custom-scrolling-example)
