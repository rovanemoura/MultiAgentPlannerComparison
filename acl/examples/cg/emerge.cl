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


;;; ---------------------------------------------------------------------------
;;; Emerge   by Ken Cheetham   01 January 1999
;;; An example provided for the amusement of all
;;; October 1999: Enhancements for background area and better color selection

(in-package :cg-user)

(defun emerge (&key window resume width height
                    orientation vertical-mode
                    color-list color-cycle-speed starter-pixmap
                    region-bias-percent
                    fade-cutoff separation-cutoff separation-goal
                    move-to-group-of-4-percent-chance
                    move-to-group-of-3-percent-chance
                    fluctuation-limit
                    background-color min-on-percent max-on-percent
                    initial-layout on-off-type connect-threads
                    reach (refresh-period 4000))
  
  ;; pass a WINDOW to use an existing emerge window
  ;; pass RESUME as non-nil (along with a WINDOW) to continue the
  ;;   current drawing in WINDOW rather than starting a new drawing
  ;; WIDTH and HEIGHT specify the interior size of the image pane
  ;; ORIENTATION may be :portrait, :landscape, or nil (unspecified)
  ;;   and is used only when HEIGHT is nil to specify that the height
  ;;   should be in the "golden ratio" (the square root of 2) to width
  ;;   where the image is taller than it is wide in :portrait mode
  ;;   and wider than it is tall in :landscape mode
  ;; VERTICAL-MODE is one of :wrap, :no-wrap, or nil (unspecified)
  ;;   use :wrap to create tiling pixmaps, as for desktop backgrounds
  ;; COLOR-LIST is a list of RGB colors.  About 3 to 8 colors seems best.
  ;;   A circle of 230 colors are are interpolated between these colors.
  ;; COLOR-CYCLE-SPEED causes each pixel color change to be biased
  ;;   in one direction around the color circle; if NIL, the program
  ;;   chooses an integer from 0 through 9 or so, biased toward 0
  ;; A STARTER-PIXMAP may be supplied to use instead of random pixels.
  ;; REGION-BIAS-PERCENT indicates the degree to which the initial
  ;;   random color assigned to each pixel is biased according to the
  ;;   pixel's location within the window.  0 to 20 is suggested.
  ;; FADE-CUTOFF indicates how small the smallest subsection of the color
  ;;   circle in which all of the neighbor pixel colors currently exist
  ;;   must be before they are all considered a region of a single color
  ;;   to be blended together.  0 to 50 is suggested.
  ;; SEPARATION-CUTOFF indicates how big the second largest gap between
  ;;   groups of neighboring pixels must be before the current pixel
  ;;   change will be biased away from the other group, so as to cause
  ;;   the two groups of pixels to diverge, forming a sharp border.
  ;;   0 to 25 is suggested.
  ;; SEPARATION-GOAL is how far apart two groups of neighboring pixels
  ;;   must get from each other before the above bias is no longer done.
  ;;   Suggested is from the separation-cutoff to 60.
  ;; MOVE-TO-GROUP-OF-4-PERCENT-CHANCE
  ;; MOVE-TO-GROUP-OF-3-PERCENT-CHANCE
  ;; FLUCTUATION-LIMIT
  ;; REFRESH-PERIOD
  
  (declare (optimize (speed 3)(safety 1)))
  
  ;; This is necessary to prevent the standalone executable from
  ;; producing the same initial drawing every time, since the initial value
  ;; of *random-state* is typically the same from one lisp session to another,
  ;; whereas the first random state created by (make-random-state t) is
  ;; typically NOT the same from one lisp session to another.
  (setq *random-state* (make-random-state t))
  
  (when starter-pixmap
    (setq width (width starter-pixmap))
    (setq height (height starter-pixmap)))
  (let* ((portrait (case orientation
                     (:portrait t)
                     (:landscape nil)
                     (t (> (random 100) 60)))))
    (unless width (setq width (if window
                                  (slot-value window 'work-width)
                                (+ (if portrait 200 250)
                                   (random (if portrait 150 300))))))
    (unless height (setq height (if window
                                    (slot-value window 'work-height)
                                  (round (* width
                                            (if portrait 1 0.5)
                                            (sqrt 2)))))))
  (let* ((window (or (and (windowp window)
                          window)
                     (progn (setq resume nil)
                       (make-the-emerge-window 20 180 width height))))
         (pane (frame-child window))
         (toolbar (first (toolbars window)))
         (multipic (find-component :multipic toolbar))
         (pause-button (find :pause (range multipic) :key #'name :test #'eq))
         (num-interpolated-colors 230)
         (original-colors color-list)
         (half-width (ceiling width 2))
         (region-bias-percent (or region-bias-percent (random 20)))
         
         (background-index num-interpolated-colors)
         (num-pixels (* half-width height))
         (num-pixels-on (if resume
                            (slot-value window 'num-pixels-on)
                          0))
         (min-on-percent (or min-on-percent
                             (+ 30 (random 40))))
         (max-on-percent (or max-on-percent
                             (let* ((percent
                                     (+ min-on-percent
                                        (random (- 90 min-on-percent)))))
                               (when (> percent 80)
                                 (setq percent 100)
                                 (setq min-on-percent 100))
                               percent)))
         (min-on-fraction (/ min-on-percent 100.0))
         (max-on-fraction (/ max-on-percent 100.0))
         (min-on (if resume
                     (slot-value window 'min-on)
                   (setf (slot-value window 'min-on)
                     (floor (* num-pixels min-on-fraction)))))
         (max-on (if resume
                     (slot-value window 'max-on)
                   (setf (slot-value window 'max-on)
                     (floor (* num-pixels max-on-fraction)))))
         (median-on (floor (+ min-on max-on) 2))
         (pixel-states (if resume
                           (slot-value window 'pixel-states)
                         (setf (slot-value window 'pixel-states)
                           (make-array (list height half-width)
                                       :element-type 'bit
                                       :initial-element
                                       (if starter-pixmap 1 0)))))
         (initial-layout (if resume
                             (slot-value window 'initial-layout)
                           (setf (slot-value window 'initial-layout)
                             (or initial-layout
                                 (let* ((num (random 100)))
                                   (cond ((< num 20) :simple-spread)
                                         ((< num 40) :cluster-spread)
                                         ((< num 70) :simple-path)
                                         (t :crossing-path)))))))
         (on-off-type (if resume
                          (slot-value window 'on-off-type)
                        (setf (slot-value window 'on-off-type)
                          (or on-off-type
                              (let* ((num (random 100)))
                                (cond ((< num 50) :one)
                                      (t :two)))))))
         (connect-threads (if resume
                              (slot-value window 'connect-threads)
                            (setf (slot-value window 'connect-threads)
                              (or connect-threads
                                  (< (random 100) 33)))))
         
         color-vector color pixmap array)
    (setf (stop-emerge-now window) nil)
    (setf (emerge-running window) t)
    (setf (title window) "Emerging")
    (setf (pixmap-name pause-button) :user-stop)
    (setf (unavailable-buttons multipic)
      (list :new :save))
    
    (when resume
      (setf original-colors (slot-value window 'original-colors)))
    (unless original-colors
      (let* ((num-colors (+ 2 (random 4))))
        (dotimes (j num-colors)
          (loop
            (setq color (make-rgb :red (random-color-coordinate)
                                  :green (random-color-coordinate)
                                  :blue (random-color-coordinate)))
            
            ;; Keep one or two of the RGB components the same as
            ;; the previous color.
            (dotimes (k (let* ((it (random 100)))
                          (cond ((eq it 0) 0)
                                ((< it 10) 1)
                                ((< it 50) 2)
                                (t 3))))
              (when original-colors
                (case (random 3)
                  (0 (setf (rgb-red color)
                       (rgb-red (first original-colors)))
                   (when (eq j (1- num-colors))
                     (setf (rgb-green color)
                       (rgb-green (first (last original-colors))))))
                  (1 (setf (rgb-green color)
                       (rgb-green (first original-colors)))
                   (when (eq j (1- num-colors))
                     (setf (rgb-blue color)
                       (rgb-blue (first (last original-colors))))))
                  (2 (setf (rgb-blue color)
                       (rgb-blue (first original-colors)))
                   (when (eq j (1- num-colors))
                     (setf (rgb-red color)
                       (rgb-red (first (last original-colors))))))
                  )))
            
            ;; Accept this new random color only when it is
            ;; sufficiently different from each of the other colors.
            (when (and (> (let* ((diff most-positive-fixnum))
                            (dolist (other original-colors diff)
                              (setq diff
                                    (min diff
                                         (+ (abs (- (rgb-red color)
                                                    (rgb-red other)))
                                            (abs (- (rgb-green color)
                                                    (rgb-green other)))
                                            (abs (- (rgb-blue color)
                                                    (rgb-blue other))))))))
                          96)
                       
                       ;; And when the brightness of the brightest
                       ;; component is sufficiently different from
                       ;; the previous color
                       (let* ((prev-color (first original-colors)))
                         (or (null prev-color)
                             (> (abs (- (+ (rgb-red prev-color)
                                           (rgb-green prev-color)
                                           (rgb-blue prev-color))
                                        (+ (rgb-red color)
                                           (rgb-green color)
                                           (rgb-blue color))))
                                300))))
              (return)))
          (push color original-colors))))
    (setf (slot-value window 'original-colors) original-colors)
    (set-up-emerge-color-dialog
     window (1+ (length original-colors)))
    (cond (resume
           (setq color-vector (slot-value window 'color-vector))
           (setq background-color
                 (slot-value window 'background-color)))
          (t
           (setf (slot-value window 'color-vector)
             (setq color-vector (setup-emerge-colors original-colors
                                                     num-interpolated-colors)))
           (setf (slot-value window 'background-color)
             (setf (aref color-vector background-index)
               (setq background-color
                     (or background-color
                         (find-contrasting-color original-colors)))))))
    (cond (resume
           (setq pixmap (slot-value window 'pixmap))
           (setq array (texture-array (texture pixmap)))
           (setq width (slot-value window 'work-width))
           (setq height (slot-value window 'work-height)))
          (t
           (setq pixmap
                 (make-instance 'pixmap
                   :bits-per-pixel 8
                   :width width :height height
                   :colors color-vector
                   :texture (make-instance 'texture
                              :bits-per-pixel 8
                              :width width :height height
                              :texture-array
                              (make-array (list height
                                                ;; Pad the width to 32 bits
                                                (* 4 (ceiling width 4)))
                                          :element-type '(unsigned-byte 8)))))
           (setq array (texture-array (texture pixmap)))
           (setf (slot-value window 'work-width) width)
           (setf (slot-value window 'work-height) height)
           (replace-palette window color-vector)
           (draw-string-in-box window "One moment please ..." nil nil
                               (box-move (visible-box window)(make-position 0 20))
                               :center :center nil t)
           (with-hourglass
             (unless starter-pixmap
               (case initial-layout
                 ((:simple-path :crossing-path)
                  
                  ;; Turn some of the pixels on by drawing a randomg
                  ;; path until enough pixels are on.
                  (let* ((x (floor half-width 1.33))
                         (y (floor height 2))
                         try dx dy)
                    (loop 
                      (when (eq (aref pixel-states y x) 0)
                        (setf (aref pixel-states y x) 1)
                        (incf num-pixels-on)
                        #+maybe ;; shows the path of on pixels (in wrong colors)
                        (setf (pixel-x-y window x y)(aref array y x))
                        #+maybe
                        (setf (pixel-x-y window (- width x 1) y)
                          (aref array y x))
                        )
                      (when (>= num-pixels-on median-on)(return))
                      (setq dx (if (eq orientation :portrait)
                                   (case (random 5)
                                     (0 -1)
                                     (4 1)
                                     (t 0))
                                 (1- (random 3))))
                      (setq dy (if (eq orientation :portrait)
                                   (- (random 5) 2)
                                 (1- (random 3))))
                      (setq try 0)
                      (loop
                        (setq x (mod (+ x dx) half-width))
                        (setq y (mod (+ y dy) height))
                        (when (or (eq initial-layout :simple-path)
                                  (eq (aref pixel-states y x) 0)
                                  (> (incf try) half-width))
                          (return))))))
                 
                 (:simple-spread
                  (let* (x y)
                    (loop
                      (setq x (random half-width)
                          y (random height))
                      (unless (eq (aref pixel-states y x) 1)
                        (setf (aref pixel-states y x) 1)
                        (when (>= (incf num-pixels-on)
                                  median-on)
                          (return))))))
                 
                 ;; This turns them on with a greater
                 ;; probability when they're nearer to the bottom center
                 (:cluster-spread
                  (let* (this-pixel-on)
                    (dotimes (x half-width)
                      (dotimes (y height)
                        (setq this-pixel-on
                              (< (random 1000)
                                 (floor
                                  (* 1000 
                                     (- max-on-fraction
                                        (* (- max-on-fraction
                                              min-on-fraction)
                                           (/ (+ (- half-width x)
                                                 (- height y))
                                              (+ half-width height))))))))
                        (setf (aref pixel-states y x)
                          (if this-pixel-on 1 0))
                        (when this-pixel-on
                          (incf num-pixels-on))))))))
             
             (cond (starter-pixmap
                    (copy-to-stream starter-pixmap pane
                                    (make-box 0 0 half-width height)
                                    (make-box 0 0 half-width height))
                    (setq num-pixels-on (* half-width height))
                    (dotimes (x half-width)
                      (dotimes (y height)
                        (setf (aref array y x)
                          (pixel-x-y pane x y))
                        (setf (aref array y (- width x 1))
                          (aref array y x))
                        (when (eq (aref array y x)
                                  background-index)
                          (setf (aref pixel-states y x) 0)
                          (decf num-pixels-on))
                        )))
                   (t
                    (initialize-random-colors window color-vector array
                                              width height region-bias-percent
                                              pixel-states background-index)))
             )
           (setf (slot-value window 'pixmap) pixmap)))
    (select-window window)
    (realize-palette window t t)
    
    (emerge-loop window width height pixmap array original-colors
                 num-interpolated-colors refresh-period
                 fluctuation-limit color-cycle-speed
                 fade-cutoff separation-cutoff separation-goal
                 move-to-group-of-4-percent-chance
                 move-to-group-of-3-percent-chance
                 region-bias-percent vertical-mode resume
                 pixel-states background-color background-index
                 min-on-percent max-on-percent
                 min-on max-on median-on num-pixels-on
                 initial-layout on-off-type connect-threads
                 reach)
    
    ;; Return the main window, so that if this example is made into
    ;; a standalone executable, the executable won't exit until this
    ;; window is closed.
    window))

(defun emerge-loop (window width height pixmap array original-colors
                           num-interpolated-colors refresh-period
                           fluctuation-limit color-cycle-speed
                           fade-cutoff separation-cutoff separation-goal
                           move-to-group-of-4-percent-chance
                           move-to-group-of-3-percent-chance
                           region-bias-percent vertical-mode resume
                           pixel-states background-color background-index
                           min-on-percent max-on-percent
                           min-on max-on median-on num-pixels-on
                           initial-layout on-off-type connect-threads
                           reach)
  (let* ((pane (frame-child window))
         (toolbar (first (toolbars window)))
         (multipic (find-component :multipic toolbar))
         (pause-button (find :pause (range multipic) :key #'name :test #'eq))
         (half-width (ceiling width 2))
         (vector (make-array 9 :fill-pointer t))
         (third-colors (floor num-interpolated-colors 3))
         #+huh? ;; 26may00
         (border-cutoff (floor num-interpolated-colors 3))
         (num-original-colors (length original-colors))
         (refresh-counter 0)
         (neighbor-diffs (make-array 9 :element-type '(unsigned-byte 8)))
         (span-of-each-color (floor num-interpolated-colors num-original-colors))
         
         ;; Randomized stuff for the whole run.
         (vertical-mode (or vertical-mode
                            (if resume
                                (slot-value window 'vertical-mode)
                              (if (< (random height) 320) :wrap :no-wrap))))
         (wrapping (eq vertical-mode :wrap))
         (color-cycle-speed (or color-cycle-speed
                                (if resume
                                    (slot-value window 'color-cycle-speed)
                                  (- 7 (floor (sqrt (random 64)))))))
         (biggest-fluctuation (or fluctuation-limit
                                  (if resume
                                      (slot-value window 'biggest-fluctuation)
                                    (random 7))))
         (fluctuation-range (1+ (* 2 biggest-fluctuation)))
         (move-to-group-of-4-percent-chance
          (or move-to-group-of-4-percent-chance
              (if resume
                  (slot-value window 'move-to-group-of-4-percent-chance)
                (+ 7 (random 60)))))
         (move-to-group-of-3-percent-chance
          (or move-to-group-of-3-percent-chance
              (if resume
                  (slot-value window 'move-to-group-of-3-percent-chance)
                (random move-to-group-of-4-percent-chance))))
         (fade-cutoff-limit (+ 10 (floor num-interpolated-colors
                                         (+ num-original-colors
                                            2)))) ;; 26may00 was 0
         (fade-cutoff (or fade-cutoff
                          (if resume
                              (slot-value window 'fade-cutoff)
                            (+ 9 (random (- fade-cutoff-limit 9))))))
         (separation-cutoff (or separation-cutoff
                                (if resume
                                    (slot-value window 'separation-cutoff)
                                  (+ 4 (random (floor fade-cutoff 2))))))
         (separation-goal (or separation-goal
                              (if resume
                                  (slot-value window 'separation-goal)
                                (+ (min span-of-each-color third-colors)
                                   (random (max 1
                                                (abs (- third-colors
                                                        span-of-each-color))))))))
         (reach (or reach
                    (if resume
                        (slot-value window 'reach)
                      (1+ (random 4)))))
         reach-now
         (polishing nil)
         
         biggest-diff next-biggest-diff
         biggest-diff-index next-biggest-diff-index
         upper-diff-index lower-diff-index
         x y num-on-neighbors last-neighbor
         num-off-neighbors this-pixel-on
         num-in-inner-group num-in-outer-group
         move-to-inner-group do-a-fade
         sum average num-being-averaged debug-flag start call
         top-left top-middle top-right
         middle-left middle-right
         bottom-left bottom-middle bottom-right)
    (declare (fixnum width height half-width
                     num-original-colors
                     fade-cutoff separation-cutoff
                     color-cycle-speed reach
                     refresh-counter biggest-diff next-biggest-diff
                     biggest-diff-index next-biggest-diff-index
                     upper-diff-index lower-diff-index x y num-on-neighbors last-neighbor
                     num-in-inner-group num-in-outer-group
                     sum average num-being-averaged start
                     num-pixels-on min-on max-on background-index))
    
    ;; Store these to re-use them if we pause and resume
    ;; by calling emerge again with the old window.
    (unless resume
      (setf (slot-value window 'color-cycle-speed) color-cycle-speed)
      (setf (slot-value window 'biggest-fluctuation) biggest-fluctuation)
      (setf (slot-value window 'fade-cutoff) fade-cutoff)
      (setf (slot-value window 'move-to-group-of-4-percent-chance)
        move-to-group-of-4-percent-chance)
      (setf (slot-value window 'move-to-group-of-3-percent-chance)
        move-to-group-of-3-percent-chance)
      (setf (slot-value window 'separation-cutoff) separation-cutoff)
      (setf (slot-value window 'separation-goal) separation-goal)
      (setf (slot-value window 'vertical-mode) vertical-mode)
      (setf (slot-value window 'reach) reach)
      )
    (cond (resume
           (setf call (slot-value window 'call)))
          (t
           (setq call
                 (list 'emerge
                       :width width :height height
                       :vertical-mode vertical-mode
                       :color-list (cons 'list original-colors)
                       :color-cycle-speed color-cycle-speed
                       :region-bias-percent region-bias-percent
                       :fade-cutoff fade-cutoff
                       :separation-cutoff separation-cutoff
                       :separation-goal separation-goal
                       :move-to-group-of-4-percent-chance
                       move-to-group-of-4-percent-chance
                       :move-to-group-of-3-percent-chance
                       move-to-group-of-3-percent-chance
                       :fluctuation-limit biggest-fluctuation
                       :background-color background-color
                       :min-on-percent min-on-percent
                       :max-on-percent max-on-percent
                       :initial-layout initial-layout
                       :on-off-type on-off-type
                       :connect-threads connect-threads
                       :reach reach
                       ))
           (setf (slot-value window 'call) call)
           (pprint call)))
    
    (loop
      (setq debug-flag nil)
      
      ;; Pick a random pixel each time through the loop.
      (setq x (random half-width))
      (setq y (random height))
      (setf (fill-pointer vector) 9)
      
      (setq top-left nil top-middle nil top-right nil
          middle-left nil middle-right nil
          bottom-left nil bottom-middle nil bottom-right nil)
      
      ;; Find all of the immeditate neighbors of the current pixel.
      ;; Usually nine (including the pixel itself), but fewer for
      ;; edge and corner pixels.
      (setf reach-now 1)
      (loop
        (do* ((room-on-left (>= x reach-now))
              (ox (if* room-on-left
                     then (- x reach-now)
                     else x)
                  (+ ox reach-now))
              (dx (if room-on-left -1 0)(1+ dx))
              (on-index 0)
              (off-index 0)
              ty)
             ((> ox (min (+ x reach-now)(1- half-width)))
              (setf num-off-neighbors off-index)
              (setf (fill-pointer vector)
                (setq num-on-neighbors on-index)))
          (do* ((room-on-top (>= y reach-now))
                (oy (if* (or wrapping room-on-top)
                       then (- y reach-now)
                       else y)
                    (+ oy reach-now))
                (dy (if room-on-top -1 0)(1+ dy)))
               ((> oy (min (+ y reach-now)
                           (if wrapping
                               most-positive-fixnum
                             (1- height)))))
            (setq ty oy)
            (when wrapping
              (cond ((minusp ty)
                     (setq ty (+ height ty)))
                    ((> ty (1- height))
                     (setq ty (- ty height)))))
            (if* (eq (aref pixel-states ty ox) 1)
               then (setf (aref vector on-index)
                      (aref array ty ox))
                    (incf on-index)
                    (case dy
                      (-1 (case dx
                            (-1 (setq top-left t))
                            (0 (setq top-middle t))
                            (1 (setq top-right t))))
                      (0 (case dx
                           (-1 (setq middle-left t))
                           (1 (setq middle-right t))))
                      (1 (case dx
                           (-1 (setq bottom-left t))
                           (0 (setq bottom-middle t))
                           (1 (setq bottom-right t)))))
               else (incf off-index))
            ))
        (if* (or (>= reach-now (1+ (random reach)))
                 (> (abs (- num-on-neighbors num-off-neighbors)) 1))
           then (return)
           else (incf reach-now)))
      
      (setq this-pixel-on
            (cond 
             (polishing
              (cond
               ((and connect-threads
                     (or (and top-left bottom-right)
                         (and top-middle bottom-middle)
                         (and top-right bottom-left)
                         (and middle-left middle-right))
                     #+maybe
                     (or (and (or top-left top-middle)
                              (or bottom-middle bottom-right)
                              (not middle-left)
                              (not middle-right))
                         (and (or top-middle top-right)
                              (or bottom-left bottom-middle)
                              (not middle-left)
                              (not middle-right))
                         (and (or top-left middle-left)
                              (or middle-right bottom-right)
                              (not top-middle)
                              (not bottom-middle))
                         (and (or middle-left bottom-left)
                              (or top-right middle-right)
                              (not top-middle)
                              (not bottom-middle))
                         )))
               (t (> num-on-neighbors num-off-neighbors))))
             (t (cond 
                 ((< num-on-neighbors 3) nil)
                 ((< num-off-neighbors 3) t)
                 ((< num-pixels-on min-on) t)
                 ((> num-pixels-on max-on) nil)
                 ((and connect-threads
                       (or (and top-left bottom-right)
                           (and top-middle bottom-middle)
                           (and top-right bottom-left)
                           (and middle-left middle-right))
                       #+maybe
                       (or (and (or top-left top-middle)
                                (or bottom-middle bottom-right)
                                (not middle-left)
                                (not middle-right))
                           (and (or top-middle top-right)
                                (or bottom-left bottom-middle)
                                (not middle-left)
                                (not middle-right))
                           (and (or top-left middle-left)
                                (or middle-right bottom-right)
                                (not top-middle)
                                (not bottom-middle))
                           (and (or middle-left bottom-left)
                                (or top-right middle-right)
                                (not top-middle)
                                (not bottom-middle))
                           )))
                 (t
                  (case on-off-type
                    (:one
                     (< (random 100)
                        (if (> num-pixels-on median-on)
                            40 60)))
                    (:two
                     (< (random (+ num-on-neighbors
                                   num-off-neighbors
                                   (if (> num-pixels-on median-on)
                                       1 -1)))
                        num-on-neighbors))))))))
      (if* this-pixel-on
         then (when (eq (aref pixel-states y x) 0)
                (incf num-pixels-on))
         else (when (eq (aref pixel-states y x) 1)
                (decf num-pixels-on)))
      (setf (slot-value window 'num-pixels-on) num-pixels-on)
      (setf (aref pixel-states y x)
        (if this-pixel-on 1 0))
      
      (cond (this-pixel-on
             
             ;; Sort the neighbors by color index, so that we can find
             ;; the two largest gaps between colors below.
             (sort vector #'<)
             
             (setq last-neighbor (1- num-on-neighbors))
             
             ;; Find the two largest gaps between colors so that we
             ;; can consider them to be in two clumps and then choose
             ;; to place the current pixel in one clump or the other.
             ;; This allows sharp borders between areas to stay around.
             (setq biggest-diff -1)
             (setq next-biggest-diff -1)
             (setq biggest-diff-index 0)
             (setq next-biggest-diff-index 0)
             (setq start (random num-on-neighbors))
             (dotimes (j num-on-neighbors)
               (setf (aref neighbor-diffs j)
                 (if (eq j last-neighbor)
                     (+ (aref vector 0)
                        (- num-interpolated-colors
                           (aref vector last-neighbor)))
                   (- (aref vector (1+ j))
                      (aref vector j)))))
             (dotimes (j (- num-on-neighbors start))
               (when (> (aref neighbor-diffs (+ start j)) biggest-diff)
                 (setq biggest-diff (aref neighbor-diffs (+ start j)))
                 (setq biggest-diff-index (+ start j))))
             (dotimes (j start)
               (when (> (aref neighbor-diffs j) biggest-diff)
                 (setq biggest-diff (aref neighbor-diffs j))
                 (setq biggest-diff-index j)))
             (setq start (random num-on-neighbors))
             (dotimes (j (- num-on-neighbors start))
               (when (and (not (eq (+ start j) biggest-diff-index))
                          (> (aref neighbor-diffs (+ start j)) next-biggest-diff))
                 (setq next-biggest-diff (aref neighbor-diffs (+ start j)))
                 (setq next-biggest-diff-index (+ start j))))
             (dotimes (j start)
               (when (and (not (eq j biggest-diff-index))
                          (> (aref neighbor-diffs j) next-biggest-diff))
                 (setq next-biggest-diff (aref neighbor-diffs j))
                 (setq next-biggest-diff-index j)))
             (setq upper-diff-index
                   (max biggest-diff-index next-biggest-diff-index))
             (setq lower-diff-index
                   (min biggest-diff-index next-biggest-diff-index))
             (setq num-in-inner-group (- upper-diff-index
                                         lower-diff-index))
             (setq num-in-outer-group (- num-on-neighbors
                                         num-in-inner-group))
             
             ;; By default, move the current pixel to whichever of the
             ;; two pixel clumps has more pixels in it now.
             (setq move-to-inner-group 
                   (or (> num-in-inner-group num-in-outer-group)
                       (and (eq num-in-inner-group num-in-outer-group)
                            (< (random 1000) 500))))
             
             ;; Main heuristics
             (setq sum 0)
             (setq do-a-fade nil)
             (cond ((and (> biggest-diff (- num-interpolated-colors fade-cutoff))
                         (< next-biggest-diff separation-cutoff)) ;; 26may00
                    (setq do-a-fade t))
                   (t
                    (when (or (and (< (abs (- num-in-inner-group ;; 26may00
                                              num-in-outer-group))
                                      3)
                                   (< (random 100)
                                      move-to-group-of-4-percent-chance))
                              (and (< (abs (- num-in-inner-group
                                              num-in-outer-group))
                                      5)
                                   (< (random 100)
                                      move-to-group-of-3-percent-chance)))
                      (setq move-to-inner-group
                            (not move-to-inner-group)))))
             (cond (do-a-fade
                       (setq num-being-averaged num-on-neighbors)
                     (dotimes (j num-on-neighbors)
                       (incf sum (aref vector j))
                       (when (<= j biggest-diff-index)
                         (incf sum num-interpolated-colors))))
                   (move-to-inner-group
                    (setq num-being-averaged num-in-inner-group)
                    (dotimes (j num-in-inner-group)
                      (incf sum (aref vector (+ lower-diff-index j 1)))))
                   (t ;; move to outer group
                    (setq num-being-averaged (- num-on-neighbors num-in-inner-group))
                    (dotimes (j (1+ lower-diff-index))
                      (incf sum (+ (aref vector j) num-interpolated-colors)))
                    (dotimes (j (- num-on-neighbors (1+ upper-diff-index)))
                      (incf sum (+ (aref vector (+ upper-diff-index j 1)))))))
             
             (setq average (round sum num-being-averaged))
             
             ;; This tries to keep separate clumps that are near
             ;; each other from converging, or to spawn new clumps
             ;; from the random fluctuation below.
             (unless (or do-a-fade
                         (< next-biggest-diff separation-cutoff)
                         (> next-biggest-diff separation-goal))
               (if (> average next-biggest-diff-index)
                   (incf average 4)
                 (decf average 4))
               
               ;; This increment will slowly shift all the colors around
               ;; the color circle.
               (incf average color-cycle-speed))
             
             ;; Introduce some general fluctuation.
             (unless (eq biggest-fluctuation 0)
               (setq average (+ average (- (random fluctuation-range)
                                           biggest-fluctuation))))
             (setq average (mod average num-interpolated-colors)))
            (t ;; not this-pixel-on
             (setq average background-index)))
      
      ;; Print debugging messages when requested.
      (when (and debug-flag 
                 (eq num-on-neighbors 9))
        (format t "~&~%flag ~a~%~
                         ~3d   ~3d   ~3d   new=~a~a   type ~a~%~
                         ~3d   ~3d   ~3d   diffs are ~a & ~a at ~a and ~a~%~
                         ~3d   ~3d   ~3d   inner-num ~a~%" 
          debug-flag
          (pixel-x-y pane (1- x)(1- y))
          (pixel-x-y pane x (1- y))
          (pixel-x-y pane (1+ x)(1- y))
          average
          (if (eq (pixel-x-y pane x y) average)
              ""
            (format nil " (add ~a)" 
              (- average (pixel-x-y pane x y))))
          (if do-a-fade
              "Fade"
            (if move-to-inner-group
                "Inner" "Outer"))
          (pixel-x-y pane (1- x) y)
          (pixel-x-y pane x y)
          (pixel-x-y pane (1+ x) y)
          biggest-diff next-biggest-diff
          biggest-diff-index next-biggest-diff-index
          (pixel-x-y pane (1- x)(1+ y))
          (pixel-x-y pane  x (1+ y))
          (pixel-x-y pane (1+ x)(1+ y))
          num-in-inner-group
          ))
      
      ;; Exit if the user closes the demo window
      ;;or clicks the pause button.
      (when (or (not (windowp window))
                (stop-emerge-now window))
        (when (windowp pane)
          (setf (stop-emerge-now window) nil)
          (setf (emerge-running window) nil)
          (redisplay-window pane)
          (setf (pixmap-name pause-button) :run-project)
          (setf (unavailable-buttons multipic) nil)
          (setf (slot-value window 'polishing) nil)
          (setf (title window) "Resting"))
        (return))
      
      (when (polish-it window)
        (case (polish-it window)
          (:on
           (setq polishing t)
           (setf (title window) "Polishing")
           (setq color-cycle-speed 0)
           (setq biggest-fluctuation 0)
           (setq fluctuation-range 0)
           (setq move-to-group-of-4-percent-chance 0)
           (setq move-to-group-of-3-percent-chance 0)
           ;#+maybe
           (setq fade-cutoff span-of-each-color)
           ;#+maybe
           (setq separation-cutoff (floor fade-cutoff 2))
           ;#+maybe
           (setq separation-goal fade-cutoff)
           )
          (:off
           (setq polishing nil)
           (setf (title window) "Emerging")
           (setq color-cycle-speed (slot-value window 'color-cycle-speed))
           (setq biggest-fluctuation (slot-value window 'biggest-fluctuation))
           (setq fluctuation-range (1+ (* 2 biggest-fluctuation)))
           (setq move-to-group-of-4-percent-chance
                 (slot-value window 'move-to-group-of-4-percent-chance))
           (setq move-to-group-of-3-percent-chance
                 (slot-value window 'move-to-group-of-3-percent-chance))
           ;#+maybe
           (setq fade-cutoff (slot-value window 'fade-cutoff))
           ;#+maybe
           (setq separation-cutoff (slot-value window 'separation-cutoff))
           ;#+maybe
           (setq separation-goal (slot-value window 'separation-goal))
           ))
        (setf (polish-it window) nil))
      
      
      
      (when (mix-it-up window)
        (setf (mix-it-up window) nil)
        (setq color-cycle-speed (floor (sqrt (random 11))))
        (setq biggest-fluctuation (random 7))
        (setq move-to-group-of-4-percent-chance (random 100))
        (setq move-to-group-of-3-percent-chance (random 50))
        (setq fade-cutoff (random (max 1 fade-cutoff-limit)))
        (setq separation-cutoff (+ 8 (random fade-cutoff))))
      
      (setf (aref array y x) average)
      (setf (aref array y (- width x 1)) average)
      (when (> (incf refresh-counter) refresh-period)
        (copy-to-stream pixmap pane
                        (nmake-box #.(make-box 0 0 0 0) 0 0 width height)
                        (nmake-box #.(make-box 0 0 0 0) 0 0 width height))
        (setq refresh-counter 0))
      
      ;; Allow the IDE to be used while this demo is running
      ;; by processing any pending events after every pixel change
      (process-pending-events)
      )))

(defun replace-palette (window color-vector)
  (let* ((old-palette (palette window)))
    (setf (palette window)(open-palette window color-vector))
    (close-palette window old-palette)
    color-vector))

(defvar *current-emerge-save-directory* nil)

(defvar *current-emerge-save-filename* nil)

(defun emerge-multipic-on-change (multipic new old)
  (let* ((toolbar (parent multipic))
         (window (parent toolbar))
         (command (and (> (length new)(length old))
                       (first (value multipic))))
         (running (emerge-running window))
         path)
    (case command
      (:pause
       (cond (running
              (setf (emerge-running window) nil)
              (setf (stop-emerge-now window) t))
             (t
              (setf (value multipic) nil)
              (emerge :window window :resume t))))
      (:new
       (if (stop-emerge-now window)
           (format t "~&Try again after it pauses ...~%")
         (when t #+maybe (y-or-n-p "Are you sure you want to wipe out the ~
                       current drawing and start over?")
           (process-pending-events) ;; Undraw the modal dialog
           (setf (value multipic) nil)
           (emerge :window window))))
      (:polish
       (setf (polish-it window)
         (if (setf (slot-value window 'polishing)
               (not (slot-value window 'polishing)))
             :on
           :off))
       (unless running
         (setf (value multipic) nil)
         (emerge :window window :resume t)))
      (:colors
       (show-emerge-color-dialog))
      (:save
       (when running
         (setf (stop-emerge-now window) t))
       (when (setq path (ask-user-for-new-pathname 
                         "Save a .bmp pixmap file to where?"
                         :allowed-types '(("Pixmap Files" . "*.bmp"))
                         :host *current-emerge-save-directory*
                         :initial-name *current-emerge-save-filename*
                         ))
         (setq *current-emerge-save-directory*
               (excl::path-namestring path))
         (setq *current-emerge-save-filename*
               (pathname-name path))
         (let* ((code-path (merge-pathnames
                            (make-pathname :type "emr") path)))
           (with-open-file (out code-path
                                :direction :output
                                :if-exists :supersede)
             (pprint (append (slot-value window 'call)
                             (list :starter-pixmap (namestring path)))
                     out))
           (save-pixmap (slot-value window 'pixmap) path)
           (format t "~&SAVED pixmap to   ~a     and the calling form to   ~a.~%"
                         (namestring path)(namestring code-path)))))
      )
    (setf (value multipic) nil) ;; turn the button back off
    t)) ;; keep the buttons unpressed
      
(defun make-the-emerge-window (left top interior-width interior-height)
  (let* ((window (make-window :emerge
                   :class 'emerge-window
                   :interior (make-box-relative
                              left top interior-width
                              (+ interior-height
                                 (+ *toolbar-icon-height*
                                    (* 2 *toolbar-margin*))
                                 2)) ;; toolbar border?
                   :scrollbars nil
                   :resizable nil
                   :border :frame-window
                   :maximize-button nil
                   :minimize-button nil
                   :title "Emerging"
                   ))
         (toolbar (add-toolbar window))
         )
    (add-component (make-instance 'multi-picture-button
                     :name :multipic
                     :left 0 :top 0
                     :width interior-width :height 22
                     :on-change 'emerge-multipic-on-change
                     :range
                     (list
                      (make-instance 'button-info
                        :name :new
                        :pixmap-name 'static-picture
                        :tooltip "New Picture"
                        :available nil)
                      (make-instance 'button-info
                        :name :pause
                        :tooltip "Pause or Resume"
                        :pixmap-name :user-stop)
                      (make-instance 'button-info
                        :name :polish
                        :pixmap-name :polish
                        :tooltip "Toggle Polishing")
                      (make-instance 'button-info
                        :name :colors
                        :pixmap-name :color
                        :available t)
                      (make-instance 'button-info
                        :name :save
                        :pixmap-name :save
                        :available nil)
                      ))
                   toolbar)
    window))

(defun random-color-coordinate ()
  ;; Pick a color value from 0 to 255 with the values
  ;; clustering toward the extremes (0 and 255) so that
  ;; things don't get too grayed out
  (let* ((num (floor (sqrt (random #.(expt 128 2))))))
    (if (evenp num) (+ 129 num)(- 127 num))))

#+test ;; test the random-color-coordinate distribution
(defun foo ()
  (let* ((vec (make-array 256 :initial-element 0)))
    (dotimes (j 100000)
      (incf (aref vec (random-color-coordinate))))
    (dotimes ( j 256)
      (print (list j (aref vec j))))))

(defun find-contrasting-color (color-list)
  ;; This now finds a gray shade (red, green, and blue
  ;; within 32 of each other)
  (let* ((max-nongray 64)
         (half-max-nongray (floor max-nongray 2))
         (max-diff 0)
         min-diff color best-color val)
    (dotimes (j 300)
      (setq val (+ half-max-nongray
                   (random (- 256 max-nongray
                              16)))) ;; not too totally white
      (setq color
            (make-rgb
             :red (max 0 (min 255 (+ val
                                     (- (random max-nongray)
                                        (1- half-max-nongray)))))
             :green (max 0 (min 255 (+ val
                                       (- (random max-nongray)
                                          (1- half-max-nongray)))))
             :blue (max 0 (min 255 (+ val
                                      (- (random max-nongray)
                                         (1- half-max-nongray)))))))
      (setq min-diff most-positive-fixnum)
      (dolist (other color-list)
        (setq min-diff (min min-diff
                            (+ (abs (- (rgb-red color)
                                       (rgb-red other)))
                               (abs (- (rgb-green color)
                                       (rgb-green other)))
                               (abs (- (rgb-blue color)
                                       (rgb-blue other)))))))
      (when (> min-diff max-diff)
        (setq max-diff min-diff)
        (setq best-color color)))
    best-color))
    
(defun setup-emerge-colors (starter-list final-number &key vector)
  (let* ((vector (or vector (make-array (1+ final-number))))
         (starter-number (length starter-list))
         (number-per-color (/ final-number starter-number))
         )
    (do* ((final-index 0 (1+ final-index))
          (starter-index 0)
          (color1 (nth starter-index starter-list))
          (color2 (nth (1+ starter-index) starter-list))
          (offset 0 (1+ offset))
          )
         ((>= final-index final-number)
          vector)
      (when (>= offset number-per-color)
        (incf starter-index)
        (setq offset 0)
        (setq color1 (nth starter-index starter-list))
        (setq color2 (or (nth (1+ starter-index) starter-list)
                         (first starter-list)))
        )
      (setf (aref vector final-index)
        (interpolate-color color1 color2 offset number-per-color))
      )))

(defun update-emerge-colors (emerge-window color-list background-color)
  (let* ((num-interpolated-colors 230) ;; defined in multiple places ???
         (vector (slot-value emerge-window 'color-vector))
         (pixmap (slot-value emerge-window 'pixmap))
         )
    (when color-list
      (setup-emerge-colors color-list num-interpolated-colors
                           :vector vector))
    (when background-color
      (setf (aref vector num-interpolated-colors) background-color))
    (setf (colors pixmap) vector)
    (replace-palette emerge-window vector)
    (copy-to-stream pixmap emerge-window #.(make-position 0 0))
    ))
    
(defun interpolate-color (color1 color2 offset difference)
   (make-rgb
    :red (interpolate-color-each
          (rgb-red color1)(rgb-red color2) offset difference)
    :green (interpolate-color-each
          (rgb-green color1)(rgb-green color2) offset difference)
    :blue (interpolate-color-each
          (rgb-blue color1)(rgb-blue color2) offset difference)))

(defun interpolate-color-each (v1 v2 offset difference)
   (round (+ (* v1 (- difference offset))
             (* v2 offset))
     difference))
    
(defun initialize-random-colors
    (window colors array width height region-bias-percent
            pixel-states background-index)
  (let* ((half-width (ceiling width 2))
         (num-colors (length colors))
         (half-colors (ceiling num-colors 2))
         (third-colors (ceiling num-colors 3))
         (region-bias-extra-colors (floor (* region-bias-percent
                                             num-colors)
                                          100))
         (region-bias-half-extra-colors (floor region-bias-extra-colors 2))
         num)
    (declare (fixnum half-colors third-colors))
    (with-delayed-redraw ((frame-child window))
      (dotimes (x half-width)
        (dotimes (y height)
          (setq num (random (+ num-colors region-bias-extra-colors)))
          ;; Make various sections of the window be biased toward one
          ;; particular color or another (horizontally and vertically)
          (cond ((<= num-colors num (+ num-colors
                                       region-bias-half-extra-colors))
                 (setq num (floor (* x third-colors) half-width)))
                ((> num (+ num-colors
                           region-bias-half-extra-colors))
                 (setq num (+ half-colors
                              (floor (* y third-colors) height)))))
          (when (eq (aref pixel-states y x) 0)
            (setq num background-index))
          (setf (aref array y x) num)
          (setf (aref array y (- width x 1)) num)
          (setf (pixel-x-y window x y) num)
          (setf (pixel-x-y window (- width x 1) y) num))))))

(defclass emerge-window (bitmap-window)
    ((running :initform nil
       :accessor emerge-running)
     (stop-now :initform nil
      :accessor stop-emerge-now)
     (mix-it-up :initform nil
      :accessor mix-it-up)
     (polish-it :initform nil
      :accessor polish-it)
     (polishing :initform nil)
     (work-width :initform nil)
     (work-height :initform nil)
     (original-colors :initform nil)
     (color-vector :initform nil)
     (pixmap :initform nil)
     (color-cycle-speed :initform nil)
     (biggest-fluctuation :initform nil)
     (move-to-group-of-3-percent-chance :initform nil)
     (move-to-group-of-4-percent-chance :initform nil)
     (fade-cutoff :initform nil)
     (separation-cutoff :initform nil)
     (separation-goal :initform nil)
     (vertical-mode :initform nil)
     (call :initform nil)
     (num-pixels-on :initform nil)
     (pixel-states :initform nil)
     (min-on :initform nil)
     (max-on :initform nil)
     (background-color :initform nil)
     (initial-layout :initform nil)
     (on-off-type :initform nil)
     (connect-threads :initform nil)
     (reach :initform nil)
     ))

(defclass emerge-pane (bitmap-pane)
    ())

(defmethod default-pane-class ((window emerge-window))
   'emerge-pane)

;; --------------------------------------------------------------
;; The color shifting dialog, using a grid-widget

(defclass emerge-color-grid-row (grid-row)())

(defclass emerge-color-grid-column (grid-column)())

(defclass emerge-color-grid-left-column (emerge-color-grid-column)())

(defclass emerge-color-grid-right-column (emerge-color-grid-column)())

(defclass emerge-color-dialog (dialog)
  ((emerge-window
    :initarg :emerge-window
    :initform nil
    :accessor emerge-window)))

(defmethod user-close ((dialog emerge-color-dialog))
  (shrink-window dialog t))

(defmethod close :before ((window emerge-window) &key abort)
  (declare (ignore abort))
  (let* ((color-dialog (find-window :emerge-color-dialog)))
    (when (windowp color-dialog)
      (close color-dialog))))

(defun make-emerge-color-dialog ()
  (let* ((width 200)
         (grid-height 250)
         (static-height 80)
         (grid
          (make-instance 'grid-widget
            :name :color-grid
            :left 0 :top 0
            :width width
            :right-attachment :right
            :bottom-attachment :bottom
            :height grid-height
            :column-sections
            (list
             (make-instance 'grid-column-section
               :name :left
               :size 100
               :proportional t
               :scrollbars nil
               :subsections
               (list
                (make-instance 'emerge-color-grid-left-column
                  :name :rgb
                  :proportional t)))
             (make-instance 'grid-column-section
               :name :right
               :size 200
               :proportional t
               :scrollbars nil
               :subsections
               (list
                (make-instance 'emerge-color-grid-right-column
                  :name :red
                  :proportional t)
                (make-instance 'emerge-color-grid-right-column
                  :name :green
                  :proportional t)
                (make-instance 'emerge-color-grid-right-column
                  :name :blue
                  :proportional t))))
            :row-sections
            (list
             (make-instance 'grid-row-section
               :name :body
               :proportional t
               :scrollbars nil
               :subsections
               (list
                (make-instance 'emerge-color-grid-row
                  :name :body
                  :border-size 1
                  :proportional t))))))
         (static (make-instance 'static-text
                   :left 0 :top grid-height
                   :width width :height static-height
                   :top-attachment :bottom
                   :right-attachment :right
                   :bottom-attachment :bottom
                   :value #.(format nil "Click the RGB components ~
 in the right section of the grid and drag up or down ~
 to alter the main colors of the current drawing.")))
         (dialog (make-window :emerge-color-dialog
                   :title "Emerge Colors"
                   :class 'emerge-color-dialog
                   :state :shrunk
                   :interior (make-box-relative
                              0 0 width
                              (+ grid-height static-height))
                   :widgets (list grid static)))
         )
    dialog))

(eval-when (compile load eval)
  (let* ((emerge-color-dialog nil))
    (defun emerge-color-dialog ()
      (if* (windowp emerge-color-dialog)
         then emerge-color-dialog
         else (setq emerge-color-dialog
                    (make-emerge-color-dialog))))))

(defun show-emerge-color-dialog ()
  (select-window (emerge-color-dialog)))

(defun set-up-emerge-color-dialog (emerge-window num-colors)
  (let* ((dialog (emerge-color-dialog))
         (grid (find-component :color-grid dialog))
         (row-section (row-section grid :body))
         (column-section (column-section grid :right))
         (row (subsection row-section :body))
         (column (subsection column-section :red))
         (emerge-box (exterior emerge-window)))
    (setf (emerge-window dialog) emerge-window)
    (move-window dialog
                 (make-position
                  (box-right emerge-box)
                  (box-top emerge-box)))
    (setf (section-count row) num-colors)
    (setf (section-size row)(section-size (parent row)))
    (set-focus-cell grid row-section column-section row column)
    dialog))

(defmethod cell-click ((grid-widget grid-widget) buttons
                       (column-section grid-column-section)
                       (column-section-border-p (eql nil))
                       (column emerge-color-grid-right-column)
                       column-num (column-border-p (eql nil)) x
                       (row-section grid-row-section)
                       (row-section-border-p (eql nil))
                       (row emerge-color-grid-row)
                       row-num (row-border-p (eql nil)) y
                       &optional trigger-key)
  (declare (ignore buttons column-num x y trigger-key))
  (let* ((old-y (position-y (cursor-position (screen *system*))))
         (emerge-window (emerge-window (parent grid-widget)))
         (color-list (slot-value emerge-window 'original-colors))
         (num-colors (length color-list))
         (background-color (slot-value emerge-window 'background-color))
         (rgb (if* (eq row-num num-colors)
                 then background-color
                 else (nth row-num color-list)))
         (rgb-column (subsection (column-section grid-widget :left)
                                 :rgb))
         (old-diff 0)
         (old-value (case (name column)
                      (:red (rgb-red rgb))
                      (:green (rgb-green rgb))
                      (:blue (rgb-blue rgb))))
         diff)
    (loop (when (eq (mouse-button-state) 0)(return))
          (process-pending-events)
          (setq diff (- (position-y (cursor-position (screen *system*)))
                        old-y))
          (unless (eq diff old-diff)
            (setq old-diff diff)
            (case (name column)
              (:red (setf (rgb-red rgb)
                      (max 0 (min 255 (- old-value diff)))))
              (:green (setf (rgb-green rgb)
                        (max 0 (min 255 (- old-value diff)))))
              (:blue (setf (rgb-blue rgb)
                       (max 0 (min 255 (- old-value diff))))))
            (if* (eq row-num num-colors)
               then (update-emerge-colors
                     emerge-window nil rgb)
               else (update-emerge-colors emerge-window color-list nil))
            (invalidate-cell row column row-num)
            (invalidate-cell row rgb-column row-num)))))

(defmethod draw-cell ((row emerge-color-grid-row)
                      (column emerge-color-grid-left-column)
                      row-num column-num cell-box stream)
  (declare (ignore column-num))
  (let* ((grid-widget (parent (parent row)))
         (emerge-window (emerge-window (parent grid-widget)))
         (color-list (slot-value emerge-window 'original-colors))
         (num-colors (length color-list))
         (rgb (if* (eq row-num num-colors)
                 then (slot-value emerge-window 'background-color)
                 else (nth row-num color-list))))
    (with-foreground-color
        (stream rgb)
      (fill-box stream cell-box))))

(defmethod draw-cell ((row emerge-color-grid-row)
                      (column emerge-color-grid-right-column)
                      row-num column-num cell-box stream)
  (declare (ignore column-num))
  (let* ((grid-widget (parent (parent row)))
         (emerge-window (emerge-window (parent grid-widget)))
         (color-list (slot-value emerge-window 'original-colors))
         (num-colors (length color-list))
         (rgb (if* (eq row-num num-colors)
                 then (slot-value emerge-window 'background-color)
                 else (nth row-num color-list))))
    (with-foreground-color
        (stream (case (name column)
                  (:red (make-rgb :red (rgb-red rgb)))
                  (:green (make-rgb :green (rgb-green rgb)))
                  (:blue (make-rgb :blue (rgb-blue rgb)))))
      (fill-box stream cell-box))))

;; ----------------------------------------------------------------
                         
(cache-pixmap
 (make-instance 'pixmap
   :name :polish
   :contents
   '((07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 04 04 04 04 07 07 07 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 07 07 07 07 07 07)
     (07 07 07 04 04 04 04 04 04 04 04 07 07 07 07 07)
     (07 07 07 04 04 04 04 04 04 04 04 07 07 07 07 07)
     (07 07 07 04 04 04 04 04 04 04 04 07 07 07 07 07)
     (07 07 07 04 04 04 04 04 04 04 04 07 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 07 07 07 07 07 07)
     (07 07 07 07 07 04 04 04 04 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07))))

(cache-pixmap
 (make-instance 'pixmap
   :name :run-project
   :contents
   '((07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 04 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 04 04 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 04 04 04 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 04 04 04 04 07 07 07 07 07 07)
     (07 07 07 07 07 07 04 04 04 04 04 07 07 07 07 07)
     (07 07 07 07 07 07 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 07 07 04 04 04 04 04 07 07 07 07 07)
     (07 07 07 07 07 07 04 04 04 04 07 07 07 07 07 07)
     (07 07 07 07 07 07 04 04 04 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 04 04 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 04 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07))))

(cache-pixmap
 (make-instance 'pixmap
   :name :user-stop
   :contents
   '((07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 04 04 04 04 04 04 04 04 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07))))

(cache-pixmap
 (make-instance 'pixmap
   :name :save
   :contents
   '((8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8)
     (0 6 0 15 15 15 15 15 15 15 15 15 15 0 6 0)
     (0 6 0 15 15 15 15 15 15 15 15 15 15 0 6 0)
     (0 6 0 15 15 15 15 15 15 15 15 15 15 0 6 0)
     (0 6 0 15 15 15 15 15 15 15 15 15 15 0 6 0)
     (0 6 0 15 15 15 15 15 15 15 15 15 15 0 6 0)
     (0 6 0 15 15 15 15 15 15 15 15 15 15 0 6 0)
     (0 6 0 15 15 15 15 15 15 15 15 15 15 0 6 0)
     (0 6 8 0 0 0 0 0 0 0 0 0 0 8 6 0)
     (0 6 6 6 6 6 6 6 6 6 6 6 6 6 6 0)
     (0 6 6 6 6 0 0 0 0 0 0 0 6 6 6 0)
     (0 6 6 6 0 15 7 7 7 7 7 7 0 6 6 0)
     (0 6 6 6 0 15 0 0 7 7 7 7 0 6 6 0)
     (0 6 6 6 0 15 0 0 7 7 7 7 0 6 6 0)
     (7 0 6 6 0 15 7 7 7 7 7 7 0 6 6 0)
     (7 7 0 0 0 0 0 0 0 0 0 0 0 0 0 8))))

(cache-pixmap
 (make-instance 'pixmap
   :name 'static-picture
   :contents
   '((07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 08 00 00 00 00 00 00 00 00 00 08 07 07 07)
     (07 07 07 07 00 15 00 15 15 15 00 07 07 07 07 07)
     (07 07 07 07 00 15 00 15 00 15 00 07 07 07 07 07)
     (07 07 07 07 00 15 15 15 15 15 00 07 07 07 07 07)
     (07 07 07 07 07 00 15 15 15 00 07 07 07 07 07 07)
     (07 07 07 07 07 07 00 15 00 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 00 07 07 07 11 11 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 11 11 11 11 07 07)
     (07 07 07 07 07 07 07 07 07 07 11 11 11 11 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 11 11 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07)
     (07 07 07 07 07 07 07 07 07 07 07 07 07 07 07 07))
   :texture-info (default-inverted-texture-info *system*)))

(defun run-emerge-example ()
  
  #+gtk;; ggg temporary
  (error "The Emerge example won't work on GTK until palettes are implemented.")
  
  ;; This is the on-initialization function of the project, which
  ;; will be run when this example is made into a standalone executable.
  (let* ((window (find-window :emerge)))
    (cond ((windowp window)
           (select-window window))
          (t (setq window (emerge))))
    
    ;; Return the main Emerge window, so that the application will
    ;; exit whenever the user closes this window.
    window))

;; Try to make sure this example can fit... cac 7jun99 bug8071
(when (featurep :allegro-cl-lite)
  (sys:resize-areas)
  (gc t))

#+run-example (run-emerge-example)
