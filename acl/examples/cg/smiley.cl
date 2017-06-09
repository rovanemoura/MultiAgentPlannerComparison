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

;; An amusing demonstration of the Runtime System.
;; (A standalone application can be created with run-smiley-example as the
;; entry function.)

(in-package :cg-user)

(defparameter *smiley-moves-per-second* 150)

(defparameter *smiley-background-color* (make-rgb :red 216 :green 224 :blue 232))

(defconstant smiley-radius 50)
(defconstant smiley-center
    (make-position (+ smiley-radius 3) (+ smiley-radius 3)))
(defconstant smiley-box-side (+ (* smiley-radius 2) 7))

(defun smiley-bitmap-stream ()
  
  ;; Draw Smiley a single time on a bitmap-stream, so that he
  ;; can be quickly copied from there repeatedly to the window.
  (let* ((stream (open-stream 'bitmap-stream nil nil
                   :background-color *smiley-background-color*
                   :page-width smiley-box-side
                   :page-height smiley-box-side))
         (left-eye (position+ smiley-center (make-position -21 -11)))
         (right-eye (position+ smiley-center (make-position 21 -11)))
         (left-pupil (position+ smiley-center (make-position -21 -6)))
         (right-pupil (position+ smiley-center (make-position 21 -6)))
         (*antialiasing* t))
    (with-foreground-color (stream red)
      (fill-circle stream smiley-center smiley-radius))
    (with-foreground-color (stream white)
      (with-line-width (stream 7)
        (erase-circle-arc stream smiley-center 35 30 120))
      (fill-circle stream left-eye 10)
      (fill-circle stream right-eye 10))
    (with-foreground-color (stream black)
      (fill-circle stream left-pupil 5)
      (fill-circle stream right-pupil 5))
    stream))

(defun next-center (center smiley-stream)
  
  ;; This shifts the position of Smiley by one pixel in the
  ;; direction that he's currently moving.  It also detects
  ;; when Smiley bounces off a wall and changes direction.
  (with-boxes (box1)
    (let* ((step (load-time-value (make-position 1 1)))
           (bounding-box (nvisible-box smiley-stream box1))
           (vertical-bounce?
            (cond
             ((minusp (box-top center))
              (setq step (nmake-position step (position-x step) 1)))
             ((> (box-bottom center) (box-bottom bounding-box))
              (setq step (nmake-position step (position-x step) -1)))))
           (horizontal-bounce?
            (cond
             ((minusp (box-left center))
              (setq step (nmake-position step 1 (position-y step))))
             ((> (box-right center) (box-right bounding-box))
              (setq step (nmake-position step -1 (position-y step)))))))
      (when (or horizontal-bounce? vertical-bounce?) 
        #+possibly ;; if not too annoying for you
        (beep smiley-stream))
      (nbox-move center step))))
  
(defun run-smiley-example ()
  (declare (optimize (speed 3)(safety 1)))
  
  ;; Call this function to run the Smiley example.
  (let* ((accelerating t)
         (max-busy-loops 0)
         time previous-time count time1 time2 busy-loops)
    
    ;; This first section determines how to control the speed of Smiley.
    ;; If we ran at full speed, Smiley would move too fast to see the
    ;; smooth motion with the human eye (with typical video adapters).
    ;; On the other hand, if we used a timer or we checked whenever
    ;; get-internal-real-time returns a larger value, there would
    ;; be a maximum resolution of around 15 milliseconds, allowing
    ;; Smiley to move only about 65 times a second, which is rather
    ;; slow when moving one pixel at a time.  So what we do here
    ;; is to experiment to find out how many times we must iterate
    ;; through a simple busy loop that calls get-internal-real-time
    ;; in order to move Smiley at some desired frequency.  The value
    ;; max-busy-loops that we calculate here is the number of busy
    ;; loops to do before incrementing Smiley's position once more,
    ;; in order for Smiley to move *smiley-moves-per-second* times
    ;; each second.
    (process-pending-events)
    (gc t)
    (dotimes (test 12)
      (setq previous-time (get-internal-real-time))
      (dotimes (pass 2)
        (dotimes (j 1000000)
          (setq time (get-internal-real-time))
          (unless (= time previous-time)
            (case pass
              (0 (setq time1 time))
              (1 (setq time2 time)
               (setq count j)))
            (setq previous-time time)
            (return))))
      (setq busy-loops (floor (* count internal-time-units-per-second)
                              (* *smiley-moves-per-second*
                                 (- time2 time1))))
      #+debug
      (pprint (list :time1 time1 :time2 time2 :time-delta (- time2 time1)
                    :count count :busy-loops busy-loops))
      
      ;; There can be a wide variation in the number of loops that
      ;; occur between increments in the get-internal-real-time value,
      ;; so do the test several times and use the maximum result.
      ;; This gives Smiley a more consistent speed on multiple runs.
      (setq max-busy-loops (max max-busy-loops busy-loops)))
    (setq busy-loops max-busy-loops)
    
    ;; Now that we know how to regulate Smiley's speed, create a window
    ;; to draw Smiley in.
    (with-open-stream
        (smiley-stream (make-window :smiley
                         :class 'frame-window
                         :owner (screen *system*)
                         :background-color *smiley-background-color*
                         :title "Smiley" 
                         :scrollbars nil))
      (select-window smiley-stream)
      (set-foreground-window smiley-stream)
      
      ;; Here is the main loop for moving Smiley.  This increments the
      ;; position of Smiley by one pixel each time through the do loop.
      (do* ((to-box (make-box-relative 10 10 smiley-box-side smiley-box-side))
            (smiley-bitmap-stream (smiley-bitmap-stream))
            (smiley-box (make-box 0 0 smiley-box-side smiley-box-side))
            (count 0))
           ((not (windowp smiley-stream)))
        
        ;; Here's the busy loop that waits long enough to slow down
        ;; Smiley so that his motion looks smooth to the human eye.
        ;; Call get-internal-real-time only to match the loop above
        ;; where we computed the number of required busy loops.
        (dotimes (count busy-loops)
          (get-internal-real-time))
        
        ;; Smiley starts out rather slow, so just to show what he can
        ;; actually do, gradually speed Smiley up until he reaches the
        ;; maximum speed of the video adapter.
        (when (>= (incf count) *smiley-moves-per-second*)
          (setq count 0)
          (cond (accelerating
                 (setq busy-loops (max 0 (min (1- busy-loops)
                                              (round (* busy-loops 0.97)))))
                 
                 ;; On reaching maximum speed, switch to decelerating.
                 (when (zerop busy-loops)
                   (setq accelerating nil)))
                
                ;; This other mode gradually slows Smiley down to
                ;; his original speed.
                (t
                 (setq busy-loops (max (1+ busy-loops)
                                       (round (* busy-loops 1.03))))
                 (when (> busy-loops max-busy-loops)
                   (setq accelerating t)))))
        
        ;; Draw Smiley at the new position.  This uses a bit of a trick,
        ;; where Smiley has a bit of whitespace around his edges, and
        ;; so drawing him just one pixel over from where he was
        ;; will automatically erase him at his previous position.
        ;; In a more general case, you would achieve smooth refreshes
        ;; by turning on the double-buffered property of the window,
        ;; and then calling invalidate-window each time the scene
        ;; needs to be refreshed, and draw the whole scene somewhat
        ;; differently in a redisplay-window method.
        ;; See the doc page for the symbol double-buffered.
        (copy-stream-area smiley-stream smiley-bitmap-stream 
                          (next-center to-box smiley-stream)
                          smiley-box
                          po-replace)
        
        ;; Allow other user gestures to be handled while Smiley is
        ;; moving around.  Especially to allow the user to close
        ;; the window to end this demonstration.
        (process-pending-events)))
    
    ;; If the window has been closed and we're running a generated
    ;; standalone application for Smiley, then exit the application.
    (when (standalone-application (app *system*))
      (exit 0))))

#+run-example
(run-smiley-example)

