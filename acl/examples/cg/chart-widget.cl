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

#|

This example draws a new line graph or bar chart each time you press
the "New" button.  Its purpose is to illustrate the variety of
chart-widget styles, rather than to illustrate how you would actually
write a chart-widget in an application.  See the chart-widget tutorial
for an introduction to writing chart code.

It still may be useful to study the initargs that are passed here to
the make-instance for the chart-widget, and how set-chart-value is called.

|#

(in-package :cg-user)

(defun random-chart (&key (item-count 100)(slow-at-first nil)(incremental nil)
                          window)
  (let* ((old-chart (and window
                         (find-component :chart window)))
         (width (if old-chart (width old-chart) 500))
         (height (if old-chart (height old-chart) 440))
         (bar-width (+ 4 (random 12)))
         (value-spacing (+ 4 (random 24)))
         
         ;; Sometimes show descriptive month & year tic labels,
         ;; and other times show simple successive integers.
         (items-are-months (zerop (random 2)))
         
         ;; This will cause the year to be displayed in the middle
         ;; of the set of twelve monthly values for that year.
         (year-in-middle (and items-are-months
                              (zerop (random 3))))
         
         ;; Pick a random range within which to generate random
         ;; values to push onto the chart.  Sometimes use a
         ;; floating point range between 0 and 1, but most of
         ;; the time use a larger integer range.
         (ballpark-max (expt 10 (- (random 6) 2)))
         (positive-max (* (1+ (random 10)) ballpark-max))
         (max (if (zerop (random 6))
                  (- positive-max)
                positive-max))
         (min (if* (and (plusp max)
                        (zerop (random 2)))
                 then 0
                 else (- max
                         (+ ballpark-max
                            (* (random
                                (abs (* 2 (if (integerp max)
                                              max
                                            (float max))))))))))
         (official-range (- max min))
         
         ;; This odd bit is due to not being able to pass
         ;; a non-integer rational number like 2/3 to cl:random.
         (range (if (integerp official-range)
                    official-range
                  (float official-range)))
         (official-max-fluctuation (/ official-range 10))
         (max-fluctuation (if (integerp official-max-fluctuation)
                              official-max-fluctuation
                            (float official-max-fluctuation)))
         (fluctuation-range (+ (* max-fluctuation 2)
                               (if (integerp max-fluctuation)
                                   1
                                 0)))
         (max-bar-length (* 2 fluctuation-range))
         (official-max-bar-length-fluctuation (/ max-bar-length 5))
         (max-bar-length-fluctuation (if (integerp official-max-bar-length-fluctuation)
                                         official-max-bar-length-fluctuation
                                       (float official-max-bar-length-fluctuation)))
         (bar-length-fluctuation-range (+ (* max-bar-length-fluctuation 2)
                                          (if (integerp max-bar-length-fluctuation)
                                              1
                                            0)))
         (bar-chart-is-stacked (not (zerop (random (if year-in-middle 4 2)))))
         (number-of-objects (if* (zerop (random 5))
                               then 1
                               else (+ 2 (random 6))))
         (objects (let* ((vec (make-array number-of-objects)))
                    (dotimes (j number-of-objects)
                      (setf (aref vec j)(aref #((:id :doris)
                                                (:id :cloyd)
                                                (:id :eunice)
                                                (:id :hobe)
                                                (:id :tal)
                                                (:id :myrtle)
                                                (:id :ruth))
                                              j)))
                    vec))
         (values (make-array number-of-objects))
         (bar-lengths (make-array number-of-objects))
         (previous-was-empty (make-array number-of-objects))
         (chart-view (case (random 2)
                       (0 :line)
                       (1 :bar)))
         (orientation (if (zerop (random 6))
                          :horizontal
                        :vertical))
         (add-from-values (and (eq chart-view :bar)
                               (zerop (random 2))))
         (bar-chart-base-value (if* add-from-values
                                  then min
                                elseif (zerop (random 4))
                                  then (- max
                                          (random (if (and (integerp max)(integerp min))
                                                      (* 2 (- max min))
                                                    (float (- max min)))))
                                  else 0))
         (include-high-low-values
          (case chart-view
            (:bar
             (and (not (and bar-chart-is-stacked
                            (not add-from-values)))
                  (>= bar-width 8)
                  (zerop (random 2))))
            (:line
             (and (>= value-spacing 8)
                  (zerop (random 2))))))
         (item-minor-tic-increment
          (if* items-are-months
             then (if* (zerop (random 2))
                     then 3
                     else 1)
             else (if* (zerop (random 2))
                     then 1
                     else (1+ (random 5)))))
         (item-minor-tics-per-major-tic
          (if* year-in-middle
             then (floor 12 item-minor-tic-increment)
           elseif (zerop (random 2))
             then 1
             else (if* items-are-months
                     then (case item-minor-tic-increment
                            (1 (case (random 4)
                                 (0 12)
                                 (1 6)
                                 (2 4)
                                 (t (1+ (random 2)))))
                            (3 (* 2 (1+ (random  2)))))
                     else (if (zerop (random 3))
                              5
                            (1+ (random 4))))))
         (item-major-label-frequency (if* year-in-middle
                                        then 1
                                      elseif items-are-months
                                        then (case (* item-minor-tic-increment
                                                      item-minor-tics-per-major-tic)
                                               (12 1)
                                               (6 (1+ (random 2)))
                                               (3 (* 2 (1+ (random 2))))
                                               (t 1))
                                        else (if (< (random 10) 6)
                                                 1
                                               (if items-are-months
                                                   (* 2 (1+ (random 2)))
                                                 (1+ (random 3))))))
         (item-axis-draw-major-labels (if* year-in-middle
                                         then :after-nestled
                                       elseif (and (eq chart-view :line)
                                                   (>= item-minor-tics-per-major-tic 3))
                                         then (case (random 10)
                                                (0 nil)
                                                (1 :before-nestled)
                                                ((2 3 4)
                                                 (if items-are-months
                                                     t
                                                   :after-nestled))
                                                (t t))
                                         else (< (random 10) 8)))
         (value-axis-draw-major-labels (< (random 10) 8))
         (value-major-label-frequency (if (< (random 10) 9)
                                          1
                                        (1+ (random 3))))
         (line-graph-draw-icons (< (random 3) 2))
         (skip-some (zerop (random 3)))
         (variable-icon-sizes (zerop (random 3)))
         (use-icon-pixmaps (zerop (random 4)))
         (large-icons (and (eq chart-view :line)
                           (or variable-icon-sizes use-icon-pixmaps)))
         (draw-minor-item-labels (and (eq item-axis-draw-major-labels t)
                                      (eq item-major-label-frequency 1)
                                      (zerop (random 2))))
         (item-minor-label-frequency (case item-minor-tics-per-major-tic
                                       (4 (1+ (random 2)))
                                       (6 (1+ (random 3)))
                                       (t 1)))
         (item-label-angle (cond ((and draw-minor-item-labels
                                       (eq item-minor-tic-increment 1)
                                       (eq item-minor-label-frequency 1)
                                       (zerop (random 2)))
                                  (nth (random 9)
                                       '(-90 -60 -45 -30 0 30 45 60 90)))
                                 (t 0)))
         (draw-minor-value-labels (and value-axis-draw-major-labels
                                       (eq value-major-label-frequency 1)
                                       (zerop (random (if (eq orientation :vertical)
                                                          2
                                                        10)))))
         (value-minor-label-frequency (1+ (random 2)))
         (value-label-angle (cond ((and draw-minor-value-labels
                                        (eq value-minor-label-frequency 1)
                                        (zerop (random 2)))
                                   (nth (random 9)
                                        '(-90 -60 -45 -30 0 30 45 60 90)))
                                  (t 0)))
         (chart (make-instance 'chart-widget
                  :name :chart
                  :left 0 :top 0 :width width :height height
                  :right-attachment :right
                  :bottom-attachment :bottom
                  :scroll-on-drag-factor 8
                  :chart-view chart-view
                  :chart-orientation orientation
                  :chart-objects objects
                  :extend-data-to-middle-of-border (zerop (random 2))
                  :fit-chart-items nil
                  :title "Random Chart"
                  :subtitle "Click and drag the body to scroll"
                  :antialias-text (zerop (random 2))
                  
                  :item-axis
                  (make-instance 'item-axis
                    :axis-label (if items-are-months "Month" "Item")
                    :invert-axis (zerop (random 16))
                    :draw-major-tics (or year-in-middle
                                         (< (random 10) 7))
                    :draw-minor-tics (< (random 10) 6)
                    :draw-major-labels item-axis-draw-major-labels
                    :draw-minor-labels draw-minor-item-labels
                    :draw-major-grid-lines
                    (if* year-in-middle
                       then (not (zerop (random 3)))
                     elseif (and (eq chart-view :bar)
                                 (not bar-chart-is-stacked)
                                 (= item-minor-tics-per-major-tic 1))
                       then (and (< (random 10) 8)
                                 :between)
                     elseif (eq item-axis-draw-major-labels :after-nestled)
                       then t
                       else (if (< (random 10) 3)
                                :between
                              t))
                    :draw-minor-grid-lines
                    (and (< (random 10) 3)
                         (if* (and (not year-in-middle)
                                   (< (random 10)
                                      (if (and (eq chart-view :bar)
                                               (not bar-chart-is-stacked))
                                          8
                                        2)))
                            then :between
                            else t))
                    :major-tic-length (if large-icons
                                          12
                                        (+ 4 (random 7)))
                    :minor-tic-length (if large-icons
                                          8
                                        (+ 2 (random 3)))
                    :major-label-frequency item-major-label-frequency
                    :minor-label-frequency item-minor-label-frequency
                    :major-grid-line-width (if (zerop (random 6))
                                               (1+ (random 3))
                                             1)
                    :minor-grid-line-dashing (if (zerop (random 4))
                                                 :solid
                                               :dot)
                    :minor-tic-increment item-minor-tic-increment
                    :minor-tics-per-major-tic item-minor-tics-per-major-tic
                    :major-label-angle item-label-angle
                    :minor-label-angle (and (zerop (random 4)) 0)
                    :on-print-major-label
                    (if* items-are-months
                       then (if* year-in-middle
                               then (lambda (value)
                                      (princ-to-string (+ 1999 (floor value 12))))
                               else (lambda (value)
                                      (format nil "~:(~a~) ~a"
                                        (aref #(:jan :feb :mar :apr :may :jun
                                                     :jul :aug :sep :oct :nov :dec)
                                              (mod value 12))
                                        (+ 1999 (floor value 12)))))
                       else (if* (zerop (random 8))
                               then (lambda (value)
                                      (format nil "~r" value))
                               else 'princ-to-string))
                    :on-print-minor-label
                    (if* items-are-months
                       then (lambda (value)
                              (format nil "~:(~a~)"
                                (aref #(:jan :feb :mar :apr :may :jun
                                             :jul :aug :sep :oct :nov :dec)
                                      (mod value 12))))
                       else 'princ-to-string)
                    #| really slow?
                    :end-margin (and (zerop (random 2))
                                     (+ 8 (random 24)))
                    |#
                    )
                  
                  :value-axis
                  (make-instance 'value-axis
                    :axis-label "Value"
                    :invert-axis (zerop (random 12))
                    :draw-major-tics (< (random 10) 8)
                    :draw-minor-tics (< (random 10) 7)
                    :draw-major-labels value-axis-draw-major-labels
                    :draw-minor-labels draw-minor-value-labels
                    :draw-major-grid-lines (< (random 10) 7)
                    :draw-minor-grid-lines (< (random 10) 4)
                    :major-tic-length (if large-icons
                                          12
                                        (+ 4 (random 7)))
                    :minor-tic-length (if large-icons
                                          8
                                        (+ 2 (random 3)))
                    :major-label-frequency value-major-label-frequency
                    :minor-label-frequency value-minor-label-frequency
                    :major-grid-line-width (if (zerop (random 6))
                                               (1+ (random 3))
                                             1)
                    :minor-grid-line-dashing (if (zerop (random 4))
                                                 :solid
                                               :dot)
                    :on-print-major-label
                    (if* (zerop (random 8))
                       then (lambda (value)
                              (if (integerp value)
                                  (format nil "~r" value)
                                (princ-to-string value)))
                       else (lambda (value)
                              (if (floatp value)
                                  (format nil "~,3f" value)
                                (princ-to-string value))))
                    :on-print-minor-label (lambda (value)
                                            (if (floatp value)
                                                (format nil "~,3f" value)
                                              (princ-to-string value)))
                    )
                  
                  :bar-chart-view
                  (make-instance 'bar-chart-view
                    :values-are-stacked bar-chart-is-stacked
                    :base-value bar-chart-base-value
                    :base-line-drawn (not add-from-values)
                    :bar-width bar-width
                    :bar-spacing (if (zerop (random 6))
                                     0
                                   (+ 4 (random 12)))
                    :high-low-value-style (aref #(:diamond :bar :tee)
                                                (random 3))
                    :bar-fill-textures (and (> number-of-objects 1)
                                            (>= bar-width 12)
                                            (zerop (random 2))
                                            '(:diagonals
                                              :dots :spots :hexagons
                                              :squares :eksuz
                                              :abort lamp :grayback-opened
                                              :italic :breakpoint))
                    )
                  
                  :line-graph-view
                  (make-instance 'line-graph-view
                    :values-are-stacked (zerop (random 3))
                    :line-widths (case (random 4)
                                   (0 '(2))
                                   (1 '(1 2 3))
                                   (t '(1)))
                    :line-dashings (if (< (random 4) 3)
                                       '(:solid)
                                     '(:solid :dot :dash :dash-dot :dash-double-dot))
                    :draw-icons line-graph-draw-icons
                    :draw-lines (or (not line-graph-draw-icons)
                                    (< (random 3) 2))
                    :icon-images (if* use-icon-pixmaps
                                    then '(:note :heart :gear :human
                                                 :star :plus :circles :cube)
                                                 #+old ;; 10aug09
                                         '(:default-closed-with-mask
                                              :default-opened-with-mask
                                              :default-leaf-with-mask
                                            :methods :key :reload :precedence)
                                    else '(:circle :square :triangle :diamond))
                    :icons-filled (aref #(t t :hollow :hollow nil :without-border)
                                        (random 6))
                    :icon-sizes (case (random 6)
                                  (0 '(3 4 6 8))
                                  (1 '(3))
                                  (2 '(6))
                                  (3 '(8))
                                  (t '(4)))
                    :span-missing-items (zerop (random 4))
                    :value-spacing value-spacing
                    :high-low-value-style (aref #(:tee :tee :tee :diamond :bar)
                                                (random 5))
                    )
                  :major-label-angle value-label-angle
                  :minor-label-angle (and (zerop (random 4)) 0)
                  :draw-legend (not (zerop (random 4)))
                  :chart-legend
                  (make-instance 'chart-legend
                    :draw-legend-border (not (zerop (random 4)))
                    :on-print-chart-object 'capitalize-object)
                  
                  ))
         (value-count 0)
         value from-value bar-length)
    #+deubg
    (format t "~&min ~a   max ~a   base ~a~%"
      min max bar-chart-base-value)
    (unless window
      (setq window (make-random-chart-window
                    width height incremental item-count)))
    (add-component chart window)
    (when old-chart (close old-chart))
    (setf (getf (plist window) :incremental) incremental)
    (setf (getf (plist window) :item-count) item-count)
    (select-window window)
    (dotimes (j number-of-objects)
      (setf (aref values j)(+ min (random range)))
      (setf (aref bar-lengths j)(random fluctuation-range))
      (setf (aref previous-was-empty j) nil))
    (dotimes (j (1+ item-count))
      (unless (windowp (window chart))(return-from random-chart))
      (dotimes (k number-of-objects)
        (incf value-count)
        (when slow-at-first
          (cond ((< value-count 6)(sleep 1))
                ((< value-count 20)(sleep 0.2))
                ((< value-count 40)(sleep 0.1))))
        (setq value (and (or (not skip-some)
                             (if (aref previous-was-empty k)
                                 (zerop (random 4))
                               (not (zerop (random 50)))))
                         (min max (max min (+ (aref values k)
                                              (- (random fluctuation-range)
                                                 max-fluctuation))))))
        (when value
          (setf (aref values k) value))
        (setf (aref previous-was-empty k)(not value))
        (setq from-value nil)
        (when (and add-from-values value)
          (setq from-value value)
          (setq bar-length (max 0 (min max-bar-length
                                       (+ (aref bar-lengths k)
                                          (- (random bar-length-fluctuation-range)
                                             max-bar-length-fluctuation)))))
          (setq value (+ from-value bar-length))
          (setf (aref bar-lengths k) bar-length))
        (set-chart-value chart
                         :item-index (if (zerop k) :new :last)
                         :object-index k
                         :value value
                         :from-value from-value
                         :low-value (and value
                                         include-high-low-values
                                         (not (zerop value))
                                         (zerop (random 2))
                                         (- value
                                            (/ (abs value) 16)
                                            (random (float (/ (abs value) 8)))))
                         :high-value (and value
                                          include-high-low-values
                                          (not (zerop value))
                                          (zerop (random 2))
                                          (+ value
                                             (/ (abs value) 16)
                                             (random (float (/ (abs value) 8)))))
                         :icon-size (and variable-icon-sizes
                                         (+ 3 (random 10)))
                         :update-now (and incremental
                                          (= k (1- number-of-objects))
                                          (or (not slow-at-first)
                                              (>= value-count 40)))
                         )))
    
    ;; This is just in case the user holds down alt-N to generate
    ;; charts as quickly as possible, and they don't generate
    ;; and display as fast as the key repeat speed.
    (update-window (window chart))))

(defun make-random-chart-window (width height incremental item-count)
  (let* ((button-width 80)
         (button-height 24)
         (button-margin 8)
         (button-top (+ height button-margin))
         (check-box-width 120)
         (label-width 50)
         (new-button (make-instance 'button
                       :name :new
                       :title "~New"
                       :tooltip "Generate a New Chart"
                       :help-string
                       "Generates a new random line graph or bar chart."
                       :left button-margin
                       :top button-top
                       :width button-width
                       :height button-height
                       :top-attachment :bottom
                       :bottom-attachment :bottom
                       :on-change (lambda (wij new old)
                                    (declare (ignore new old))
                                    (let* ((dialog (parent wij)))
                                      (random-chart
                                       :incremental
                                       (getf (plist dialog) :incremental)
                                       :item-count
                                       (getf (plist dialog) :item-count)
                                       :window dialog))
                                    t)))
         (inspect-button (make-instance 'button
                           :name :inspect
                           :title "~Inspect"
                           :tooltip "Inspect and Modify"
                           :help-string
                           "Inspects the current chart to let you modify its properties."
                           :left (+ (* 2 button-margin)
                                    button-width)
                           :top button-top
                           :width button-width
                           :height button-height
                           :top-attachment :bottom
                           :bottom-attachment :bottom
                           :on-change (lambda (wij new old)
                                        (declare (ignore new old))
                                        (inspect (find-sibling
                                                  :chart wij))
                                        t)))
         (incremental-button (make-instance 'check-box
                               :name :incremental
                               :title "Incre~mental"
                               :tooltip "Add Items Gradually"
                               :help-string
                               #.(format nil "Redraws a new chart after it adds ~
                                              each chart item incrementally.")
                               :left (+ (* 5 button-margin)
                                        (* 2 button-width))
                               :top button-top
                               :width check-box-width
                               :height button-height
                               :top-attachment :bottom
                               :bottom-attachment :bottom
                               :value incremental
                               :on-change (lambda (wij new old)
                                            (declare (ignore old))
                                            (setf (getf (plist (parent wij))
                                                        :incremental)
                                              new)
                                            t)))
         (count-label (make-instance 'static-text
                        :name :count-label
                        :value "~Count"
                        :justification :right
                        :left (+ (* 6 button-margin)
                                 (* 2 button-width)
                                 check-box-width)
                        :top button-top
                        :width label-width
                        :height button-height
                        :top-attachment :bottom
                        :bottom-attachment :bottom))
         (count (make-instance 'lisp-text
                  :name :item-count
                  :tooltip "Number of Items"
                  :help-string "The number of chart items to add to a new chart."
                  :value item-count
                  :justification :right
                  :left (+ (* 7 button-margin)
                           (* 2 button-width)
                           check-box-width
                           label-width)
                  :top button-top
                  :width button-width
                  :height button-height
                  :top-attachment :bottom
                  :bottom-attachment :bottom
                  :on-change
                  (lambda (wij new old)
                    (declare (ignore old))
                    (if* (and (integerp new)
                              (plusp new))
                       then (setf (getf (plist (parent wij))
                                        :item-count)
                              new)
                            t
                       else (warn "Enter a positive integer only.")
                            nil))
                  ))
         )
    (make-window :random-chart
      :class 'dialog
      :widgets (list new-button inspect-button incremental-button
                     count-label count)
      :title "Random Chart"
      :interior (make-box-relative 450 60 width
                                   (+ height button-height
                                      (* 2 button-margin)))
      :resizable t)))

;;; ---------------------------------------------------------
;;; Define some fill textures for filling bars in bar graphs.

(cache-pixmap
 (make-instance 'pixmap
   :name :diagonals
   :bits-per-pixel 8
   :colors (vector light-green dark-green)
   :contents '((1 0 0 0 0 1)
               (0 0 0 0 1 1)
               (0 0 0 1 1 0)
               (0 0 1 1 0 0)
               (0 1 1 0 0 0)
               (1 1 0 0 0 0))))

(cache-pixmap
 (make-instance 'pixmap
   :name :dots
   :bits-per-pixel 8
   :colors (vector light-yellow red)
   :contents '((0 0 0 1 0 0 0 0)
               (0 0 0 0 0 0 0 1)
               (0 1 0 0 0 0 0 0)
               (0 0 0 0 0 1 0 0))))

(cache-pixmap
 (make-instance 'pixmap
   :name :spots
   :bits-per-pixel 8
   :colors (vector cyan dark-blue)
   :contents '((1 0 0 0 0 0 0 1 1 1)
               (0 0 0 1 1 0 0 0 1 1)
               (0 0 1 1 1 1 0 0 0 0)
               (0 0 1 1 1 1 0 0 0 0)
               (0 0 1 1 1 1 0 0 1 1)
               (1 0 0 1 1 0 0 1 1 1))))

(cache-pixmap
 (make-instance 'pixmap
   :name :hexagons
   :bits-per-pixel 8
   :colors (vector cyan dark-blue)
   :contents '((0 0 0 1 1 1 1 0 0 0 0 0)
               (0 0 1 0 0 0 0 1 0 0 0 0)
               (0 1 0 0 0 0 0 0 1 0 0 0)
               (1 0 0 0 0 0 0 0 0 1 1 1)
               (0 1 0 0 0 0 0 0 1 0 0 0)
               (0 0 1 0 0 0 0 1 0 0 0 0))))

(cache-pixmap
 (make-instance 'pixmap
   :name :squares
   :bits-per-pixel 8
   :colors (vector yellow dark-yellow)
   :contents '((1 0 0 0 0 0)
               (1 0 0 0 0 0)
               (1 0 0 0 0 0)
               (1 0 0 0 0 0)
               (1 0 0 0 0 0)
               (1 1 1 1 1 1))))

(cache-pixmap
 (make-instance 'pixmap
   :name :eksuz
   :bits-per-pixel 8
   :colors (vector light-gray red)
   :contents '((1 0 0 0 0 0 1)
               (0 1 0 0 0 1 0)
               (0 0 1 0 1 1 0)
               (0 0 0 1 0 0 0)
               (0 0 1 0 1 0 0)
               (0 1 0 0 0 1 0)
               (1 0 0 0 0 0 1))))

;;; -------------------------------------------------
;;; Define some pixmaps to serve as line graph icons.

;;; The following sample pixmaps are from http://awicons.com
;;; where you should purchase them (cheap!) if you want to use them in your own apps.

(cache-pixmap (make-instance 'pixmap :name :note :bits-per-pixel 24 :width 16 :height 16 :invert-p t :source nil :colors nil :contents
                '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 3648055 39168 39168 39168 2729257 0 0 0 0 0 0 0 0 0) (0 2138912 3979303 5095979 7459911 7197508 3388444 630020 0 0 0 0 0 0 0 0)
                  (0 1286163 9952386 6867279 7065163 6999370 5620282 5029172 39168 0 0 0 0 0 0 0) (0 3648055 8179062 11264160 11395489 11395489 11264160 11526818 2402084 0 0 0 0 0 0 0)
                  (0 0 1810715 6340702 11657644 13495495 11657644 6668644 2533414 0 0 0 0 0 0 0) (0 0 0 8175484 3648055 3648055 4566597 1680153 3386675 0 0 5485139 892173 0 0 0)
                  (0 0 0 0 0 0 0 1680153 3386675 0 0 0 39168 8175484 0 0) (0 0 0 0 0 0 0 1680153 3386675 0 0 0 39168 3648055 0 0)
                  (0 0 0 0 0 0 0 1680153 3386675 0 0 0 1746445 3648055 0 0) (0 0 0 0 0 0 0 1680153 3386675 0 0 1810715 892935 5485139 0 0)
                  (0 0 0 0 0 0 0 1680153 3386675 0 4566597 39168 892173 0 0 0) (0 0 0 0 0 0 0 1680153 3386675 1810715 564741 39168 0 0 0 0)
                  (0 0 0 0 0 0 0 1680153 9366128 5492791 564741 6403681 0 0 0 0) (0 0 0 0 0 0 0 1680153 9497457 630534 6403681 0 0 0 0 0)
                  (0 0 0 0 0 0 0 432902 826892 9094026 0 0 0 0 0 0))
                :mask-contents
                '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 0 0 0 0 0 0 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 0 0 1 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(cache-pixmap (make-instance 'pixmap :name :heart :bits-per-pixel 24 :width 16 :height 16 :invert-p t :source nil :colors nil :contents
                '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 12338758 11403522 12338758 0 0 0 0 0 0 0)
                  (0 0 0 0 13139325 11604499 12658729 13779780 12658985 11604499 13139325 0 0 0 0 0)
                  (0 0 0 12538708 11735314 12926527 12793399 12262943 12793656 12926783 11668750 12538708 0 0 0 0)
                  (0 0 12138552 11934235 12993862 12460836 11995145 12060681 11995145 12395043 12993861 11934234 12138552 0 0 0)
                  (0 12738914 11933464 13387591 12852253 12915476 13047062 13047062 12981269 12784147 12720667 13256006 11933207 12738914 0 0)
                  (0 11535622 13650507 13577004 13704478 13967393 14164515 14164772 14098979 13901600 13572892 13445418 13519177 11535622 0 0)
                  (12538708 12789544 14371403 14361894 14821931 15150639 15282482 15348018 15282225 15019310 14624809 14164515 14239816 12658215 12538708 0)
                  (11537935 14174024 15023936 15413811 15939641 16268606 16400706 16466499 16400449 16137020 15742519 15216432 14761021 13976902 11537935 0)
                  (11403522 15357270 15877188 16400706 16600912 16669789 16672101 16672359 16671331 16668761 16599626 16203070 15614017 15094355 11403522 0)
                  (11403522 16014942 16537175 16670303 16675699 16679555 16682124 16682639 16681354 16678526 16673901 16667990 16404561 15752027 11403522 0)
                  (11738141 15552079 16678013 16678527 16684694 16689320 16758711 16761795 16692405 16687778 16682638 16675957 16675700 15419465 11738141 0)
                  (12738914 13380139 16686237 16689320 16759739 16765394 14982043 12266026 14981529 16763852 16691890 16686751 16683923 13379111 12738914 0)
                  (0 11537935 14044751 16365752 16040385 13591138 11869727 13339531 11803163 13590625 16038330 16363439 14043465 11537935 0 0)
                  (0 0 12138552 11403522 11403522 12538708 0 0 0 12538708 11403522 11403522 12138552 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                :mask-contents
                '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(cache-pixmap (make-instance 'pixmap :name :gear :bits-per-pixel 24 :width 16 :height 16 :invert-p t :source nil :colors nil :contents
                '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 4102878 4102878 4102878 0 0 0 0 0 0 0) (0 0 0 7975645 0 0 6139366 12445694 6139366 0 0 7319261 0 0 0 0)
                  (0 0 4693469 6007782 4693469 5284317 8175598 11263228 7650285 5218781 4693469 6467816 4693469 0 0 0)
                  (0 7319261 6402024 11197182 9029620 8175853 11986171 12248829 11262971 7584493 9883640 11197181 5941990 7975645 0 0)
                  (0 0 4693469 9489144 12183292 14351615 12840442 11000822 11657466 12182783 11065853 8700916 4693469 0 0 0)
                  (0 0 5284317 7715821 13562879 10278130 4102878 6531549 4102878 9423858 12182783 7387117 5284317 0 0 0)
                  (4102878 5547750 6795757 11065851 11985914 4102878 0 0 0 4102878 11657466 10737403 7124206 5547750 4102878 0)
                  (4102878 10407934 9882876 11591421 10474998 6531549 0 0 0 6531549 11000822 11985661 9817084 10342398 4102878 0)
                  (4102878 7387118 7649775 11328251 12050682 4102878 0 0 0 4102878 13299450 12313851 7124461 5744870 4102878 0)
                  (0 0 5284317 8503277 13363967 10079730 4102878 6531549 4102878 10868466 15138303 8963309 5284317 0 0 0)
                  (0 0 4693469 11261941 13560573 13954559 12903930 11853046 13363962 14940415 14086397 12444153 4693469 0 0 0)
                  (0 7975645 6795494 14611455 13100794 9094381 13757436 14611198 13954556 9225965 11787254 14611454 7386856 7319261 0 0)
                  (0 0 4693469 7583720 4693469 5284317 9619694 15202046 10342128 5284317 4693469 6861286 4693469 0 0 0) (0 0 0 7319517 0 0 7058150 15990015 7058150 0 0 7975645 0 0 0 0)
                  (0 0 0 0 0 0 4102878 4102878 4102878 0 0 0 0 0 0 0))
                :mask-contents
                '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 0 1 1 0 0 0 1 1 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 0 1 1 0 0 0 1 1 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(cache-pixmap (make-instance 'pixmap :name :human :bits-per-pixel 24 :width 16 :height 16 :invert-p t :source nil :colors nil :contents
                '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 4298551 2592792 2592792 3510314 0 0 0 0 0 0) (0 0 0 0 0 2657053 6670397 8379722 8840276 7066182 3578151 0 0 0 0 0)
                  (0 0 0 0 0 2592792 8314186 7391802 8247116 8052814 3513890 0 0 0 0 0) (0 0 0 0 0 1869329 2987291 1671951 1671951 2790426 2132243 0 0 0 0 0)
                  (0 0 0 0 2459675 4960558 7196224 8708944 9037911 7394377 3909157 3247656 0 0 0 0) (0 0 0 0 3250719 8511566 7457849 7589183 8247115 8445778 7397191 2592792 0 0 0 0)
                  (0 0 0 0 3513890 8182345 7128628 7589183 8247115 8182351 7199813 2592792 0 0 0 0) (0 0 0 0 3513890 8182345 7128628 7589183 8247115 8182351 7199813 2592792 0 0 0 0)
                  (0 0 0 0 3513890 9104472 9697119 10092390 10092390 9763426 8251985 2592792 0 0 0 0) (0 0 0 0 4237353 9566048 3777060 1671951 1671951 5026864 9895011 3448097 0 0 0 0)
                  (0 0 0 0 6990945 2724377 4239912 7530313 7004228 3450144 2724377 7778670 0 0 0 0) (0 0 0 0 0 1671951 3391518 5425727 4966456 2732054 3247656 0 0 0 0 0)
                  (0 0 0 0 0 1671951 7654742 9822854 9888133 5683769 3247656 0 0 0 0 0) (0 0 0 0 0 4888898 5416513 12904368 12181669 3314468 8106102 0 0 0 0 0)
                  (0 0 0 0 0 0 4888898 1671951 1671951 7318121 0 0 0 0 0 0))
                :mask-contents
                '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(cache-pixmap (make-instance 'pixmap :name :star :bits-per-pixel 24 :width 16 :height 16 :invert-p t :source nil :colors nil :contents
                '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 10053120 0 0 0 10053120 0 0 0 0 0 0) (0 0 0 0 0 10447878 10317069 0 10053120 10842636 0 0 0 0 0 0)
                  (0 0 0 0 11967073 13408563 14197305 10053120 14197305 12553254 11702867 0 0 0 0 0)
                  (0 11967073 10447878 11697945 10053120 13408563 16754709 15053375 16754709 13408563 10447878 11697945 10053120 12296558 0 0)
                  (0 0 10317069 15053388 16758836 16106035 16501261 16764942 16501261 16302643 16758836 15052614 10317069 0 0 0)
                  (0 0 11438917 11697945 16498739 16767274 16767274 16767274 16767274 16767274 16498739 11697945 11438917 0 0 0)
                  (0 10581019 10842636 14197299 16107831 16769607 16769607 16769607 16769607 16769607 16107831 14197299 10842636 10581019 0 0)
                  (10581019 10053120 14198079 16760116 15911224 16771939 16771939 16771939 16771939 16771939 15911224 16761152 14198079 10053120 10581019 0)
                  (0 12824714 10581019 11303187 15713611 16774272 16774272 16774272 16774272 16774272 15713611 11303187 10581019 12824714 0 0)
                  (0 0 11174711 13408563 16497427 14859579 16314243 16776086 16314243 14662196 16497171 13408044 11174711 0 0 0)
                  (0 0 10053120 14658630 13803321 14856006 15248902 14395423 15248902 15250764 13408563 14658630 10447878 0 0 0)
                  (0 11174711 10053120 11174711 10581019 13408563 16302662 12553254 16302662 13408563 10845225 11174711 10317069 10581019 0 0)
                  (0 0 0 0 12824714 11697945 11303187 10581019 11303187 12092703 12560764 0 0 0 0 0) (0 0 0 0 0 10053120 10845225 0 11174711 10053120 0 0 0 0 0 0)
                  (0 0 0 0 0 10845225 0 0 0 10845225 0 0 0 0 0 0))
                :mask-contents
                '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 1 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 1 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(cache-pixmap (make-instance 'pixmap :name :plus :bits-per-pixel 24 :width 16 :height 16 :invert-p t :source nil :colors nil :contents
                '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 1671951 1671951 1671951 1671951 1671951 0 0 0 0 0 0) (0 0 0 0 0 2788385 10937991 10937991 10347136 2788385 0 0 0 0 0 0)
                  (0 0 0 0 0 2788385 9887861 7788363 9428336 2788385 0 0 0 0 0 0) (0 0 0 0 0 2788385 9231213 7000385 8968554 2788385 0 0 0 0 0 0)
                  (0 0 0 0 0 2788385 8574565 6081079 8377443 2788385 0 0 0 0 0 0)
                  (1671951 1671951 1671951 1671951 1671951 2788385 7917917 5227308 7720795 2788385 1671951 1671951 1671951 1671951 1671951 0)
                  (1671951 7523166 7523166 7720033 8245353 8770417 7983203 5357876 7851617 8573551 8048231 7720033 7523166 7523166 1671951 0)
                  (1671951 7785570 5554236 6145095 6145095 6276168 6341961 6341961 6276168 6145095 6145095 6145095 5685567 8048231 1671951 0)
                  (1671951 7785571 7916901 7916901 7916901 7916901 8048232 7129432 8048232 7916901 7916901 7916901 7916901 8113768 1671951 0)
                  (1671951 2788385 2788385 2788385 2788385 3707695 8507503 8114025 8507503 3707695 2788385 2788385 2788385 2788385 1671951 0)
                  (0 0 0 0 0 2788385 8835701 9032825 8835701 2788385 0 0 0 0 0 0) (0 0 0 0 0 2788385 9295229 10017162 9295229 2788385 0 0 0 0 0 0)
                  (0 0 0 0 0 2788385 9688964 11001755 9688964 2788385 0 0 0 0 0 0) (0 0 0 0 0 2788385 9557634 11461027 9557634 2788385 0 0 0 0 0 0)
                  (0 0 0 0 0 1934611 2788385 2788385 2788385 1934611 0 0 0 0 0 0))
                :mask-contents
                '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(cache-pixmap (make-instance 'pixmap :name :circles :bits-per-pixel 24 :width 16 :height 16 :invert-p t :source nil :colors nil :contents
                '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 6400729 886999 33751 886999 6400729 0 0 0 0 0 0)
                  (0 0 0 0 2724824 496099 1354230 1618430 1288438 496099 2724824 0 0 0 0 0) (0 0 0 5481688 364515 960510 565759 434175 499967 763135 298723 5481688 0 0 0 0)
                  (0 0 0 886998 367610 105215 39423 39423 39423 39423 170234 886998 0 0 0 0) (0 0 0 34268 39423 498943 695807 958207 827135 498943 39423 34009 0 0 0 0)
                  (0 0 12753427 33751 1352191 3058943 4437503 5094143 4831487 3649791 1483263 33751 1738269 0 0 0)
                  (0 13605894 15514947 1936068 3648245 6472447 8179199 9032703 8638975 7260159 4173301 559296 3779360 1738255 0 0)
                  (13741636 15385159 16174429 8758662 3317217 9032189 10936319 12118015 11592959 9754365 3448289 2202224 3715356 2662419 5611086 0)
                  (13474048 16442489 16374896 15912797 5807525 4170465 9227249 12707834 9555441 4236257 1282448 3451931 3256343 3190549 1671951 0)
                  (13474048 16709266 16641680 16376962 15850355 9877629 1279615 1019306 1873291 3056212 4173865 4437292 4634926 3650844 1671951 0)
                  (13607195 15719794 16710571 16644005 16315037 14081925 2328079 4895283 5486912 5684034 5947206 6144584 6341706 4304174 3247656 0)
                  (14007150 14332984 16643247 16710069 16644532 16315823 5414711 5417797 7588193 7719523 7851108 7916902 7916646 4102963 8106102 0)
                  (0 13674544 14926948 16312747 16708539 16444850 13551472 3575328 7912559 9360256 9492097 9293952 6926944 4429627 0 0)
                  (0 0 14007150 13871925 13870626 13937199 13808211 0 6464604 4495419 3707695 4758080 8106102 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                :mask-contents
                '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 1 0 0 0 0 0 1 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(cache-pixmap (make-instance 'pixmap :name :cube :bits-per-pixel 24 :width 16 :height 16 :invert-p t :source nil :colors nil :contents
                '((0 0 0 0 0 4557125 222722 2717993 5476435 8169340 0 0 0 0 0 0) (0 0 0 6396001 26112 230145 4636979 4767273 3450909 2462740 1211146 223233 3637559 6396001 0 0)
                   (0 7250030 945677 492035 695044 564483 6080846 5621049 4374563 4768807 5294638 5491760 4504358 3319324 2331411 1076495)
                   (2717993 886021 1351689 1221384 1024262 827141 6343504 7064659 4176165 3322902 3322902 3585817 3980062 4374563 5163308 1604624)
                   (25344 2074893 1812236 1549578 1352456 1090055 6606163 7655515 6670157 3782428 3322902 3322902 3322902 3322902 4374563 1604624)
                   (25344 2469136 2206222 1943564 1680907 1418249 6868822 8180835 7655004 5686076 3716891 3388439 3322902 3322902 4374563 1604624)
                   (25344 2863123 2600465 2272015 2009357 1746699 7131481 8771435 8245861 7983202 4635944 3848220 3519768 3322902 4374563 1604624)
                   (25344 3191573 2994451 2666258 2403344 2140686 7394140 9296754 8771180 8508522 6933327 4373538 4045086 3651098 4440099 1604624)
                   (25344 3322902 3257109 3125780 2797586 2534672 7656543 9756281 9296244 9099121 8836719 5358128 4504868 4176416 4834088 1604624)
                   (25344 3322902 3322902 3257366 3191573 2928915 7984994 10281344 9821563 9624441 9361782 7458645 5030186 4636198 5228333 1604624)
                   (25344 3322902 3322902 3322902 3322902 3257109 8247653 10741127 10346627 10149761 9952638 9230706 5489968 5161516 5687858 1604624)
                   (25344 3322902 3322902 3322902 3322902 3322902 8904048 11200654 10871946 10609288 10412165 10215042 6540095 5686834 6081847 1604624)
                   (25344 3322902 3322902 3716894 5292602 6605906 8181861 9364331 10545536 10808195 11332241 11135119 8445276 6804031 6936129 1406990)
                   (25344 4635950 6146378 6343248 5029946 3453982 2797586 2863123 2863123 2863123 3782942 4111393 5030448 5162285 5028398 222722)
                   (25344 3448877 2399518 1086473 823813 958470 958470 1024262 1024262 1023494 955142 754693 422915 879117 4557125 0)
                   (0 7250030 7250030 3637559 3637559 3637559 1798683 25344 25344 2717993 5476435 9088906 0 0 0 0))
                :mask-contents
                '((1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

;;; ----------------
;;; Run the example.

(defun run-chart-widget-example ()
  (random-chart))

