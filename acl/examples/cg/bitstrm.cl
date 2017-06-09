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
;; The bitmap-stream example demonstrates using a bitmap-stream to
;; draw onto a bitmap in memory, and then saving the drawing there
;; to be used for quickly redisplaying windows as they are uncovered.

;; A bitmap-window (or bitmap-pane) essentially has a bitmap-stream built
;; into it, which automatically saves whatever is drawn on the window.
;; The bitmap-stream class is provided if you want to handle a
;; backing-store bitmap yourself in some other way, such as to
;; use a single bitmap-stream as backing-store for multiple windows,
;; or to use multiple bitmap-streams to hold multiple drawings to
;; copy to a single window under different conditions.

;; This simple example simply mimics a bitmap-pane by associating
;; a single bitmap-stream with a single frame-window to redisplay
;; a drawing quickly whenever the frame-window is uncovered.

(in-package :cg-user)

;; Define a subclass of frame-window so that we can add a
;; redisplay-window method to it
(defclass bitmap-stream-tester (frame-window)())

(defun run-bitmap-stream-example ()
  (let* ((left 300)(top 200)(width 500)(height 200)
         (right (+ left width))(bottom (+ top height))
         (number-of-lines 40)
         
         ;; Make a bitmap stream to save a drawing in memory.
         (bitmap-stream (open-stream 'bitmap-stream nil nil
                          :page-width width :page-height height
                          :background-color black))
         
         ;; Here's the window to display the drawing on the screen.
         (window (make-window :test-bitmap-stream
                   :class 'bitmap-stream-tester
                   :title "I Redisplay this Pattern Quickly When I'm Uncovered"
                   :interior (make-box left top right bottom)
                   :scrollbars nil
                   :resizable nil
                   :maximize-button nil
                   :state :shrunk)) ;; Expose it after it's all ready
         x y p-left p-top p-right p-bottom)
    
    ;; Point the window to the backing-store bitmap-stream, so
    ;; that our redisplay-window method can find the bitmap-stream.
    (setf (getf (plist window) :bitmap-stream) bitmap-stream)
    
    ;; Draw a pattern a single time onto the bitmap-stream, so
    ;; that the window can quickly redraw itself later whenever it's
    ;; uncovered by simply copying the bitmap-stream to the window.
    
    ;; Draw the background color
    (clear-page bitmap-stream)
    
    ;; Draw some unimaginative "string art" onto the bitmap stream.
    (dotimes (j number-of-lines)
      (setq x (floor (* j width) number-of-lines))
      (setq y (floor (* j height) number-of-lines))
      (setq p-left (make-position 0 (- height y)))
      (setq p-top (make-position x 0))
      (setq p-right (make-position width y))
      (setq p-bottom (make-position (- width x) height))
      (with-foreground-color (bitmap-stream green)
        (draw-line bitmap-stream p-left p-top)
        (draw-line bitmap-stream p-right p-bottom))
      (with-foreground-color (bitmap-stream blue)
        (draw-line bitmap-stream p-top p-right)
        (draw-line bitmap-stream p-bottom p-left))
      )
    
    ;; Expose the window now that the bitmap-stream has the
    ;; intended drawing on it for the window to redisplay.
    (select-window window)
    
    ;; Return the main window, so that a standalone executable
    ;; made from this example will continue to run until this
    ;; window is closed.
    window))

;; Redisplay-window is called whenever part of the window is uncovered
;; or when invalidate is called on it.  To redisplay our window
;; quickly, simply copy from the pre-drawn bitmap-stream to the window.
;; If we had used a bitmap-pane (or bitmap-window) instead, then we
;; we not need to write this redisplay-window method, and could simply
;; draw directly on the window to achieve the same result, since this
;; backing-store behavior is hardwired into the bitmap-pane class.
(defmethod redisplay-window ((window bitmap-stream-tester) &optional box)
   (unless box (setq box (nvisible-box window #.(make-box 0 0 0 0))))
   (copy-stream-area window (getf (plist window) :bitmap-stream)
     box box po-replace))

#+run-example
(run-bitmap-stream-example)
