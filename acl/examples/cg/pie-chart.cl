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

;; The following program allows a user to draw a 3-D pie chart. The user uses a 
;; pie-chart-editor to enter the relevant information needed to draw the pie-chart.


;; To run the example load the code into lisp and run

;; (run-3d-pie-chart-example)

;; The pie-chart editor is a grid widget which displays the information needed to
;; draw the pie-chart.

;; The rows of the grid widget are identified by each sector. To change the 
;; number of sectors you want to display in the pie-chart mouse-left click 
;; on the header - sector - . An editable text widget will be displayed and the
;; user enters the number of sectors in the pie-chart.

;;The columns identify the properties of each sector. They are

;; %  the percentage of each sector. If you click on the particular % cell 
;;    which identifies the particular sector you can (keeping the left mouse down) 
;;    adjust the % by moving the mouse up or down in the cell. The number displayed
;;    in the cell for each sector is tabulated and the total displayed in the
;;    editable text widget which is displayed when adjusting the
;;    %. The total % must add up to 100 and the user can't increase
;;    any cell value if the total is 100%.
 
;; color - is the color you choose for the particular
;;         sector. Left-click on the color cell for the particular sector and
;;         choose from the color panel that is displayed the color you
;;         want. The particular cell will be colored in the color you choose

;; label - left click in the label cell and enter (in the provided
;;         editable-text widget) the text you want associated with the
;;         particular sector. Up to 6 characters.  click draw-3d-pie-chart to
;;         draw the pie-chart on the screen and print-3d-pie-chart to print
;;         the pie-chart

(defparameter row-list '(nil nil nil))
(defparameter check-sum 0)

(defclass 3d-pie-grid (grid-widget)
  ((row-list :initform nil :accessor row-list)
   (color-list :initform nil :accessor color-list)
   (%-list :initform nil :accessor %-list)
   (label-list :initform nil :accessor label-list)))

(defclass 3d-pie-row (grid-row)())

(defclass 3d-pie-column (grid-column) ())

(defclass 3d-pie-sector (3d-pie-column)
  ((header-title :initarg :header-title
                 :accessor header-title
                 :initform nil)))

(defclass 3d-pie-% (3d-pie-column)
  ((header-title :initarg :header-title
                 :accessor header-title
                 :initform nil)))

(defclass 3d-pie-color (3d-pie-column)
  ((header-title :initarg :header-title
                 :accessor header-title
                 :initform nil)))

(defclass 3d-pie-label (3d-pie-column)
  ((header-title :initarg :header-title
                 :accessor header-title
                 :initform nil)))

(defclass 3d-pie-dialog (bitmap-window) ())

(defclass 3d-column-header-row-section (row-section-with-sort-gadget-mixin
                                        grid-row-section) ())
(defclass 3d-column-header-row (column-header-row) ())


(defun make-3d-pie-chart-dialog ()
  (let* ((static (make-instance 'button
                   :title "Draw 3D Pie Chart"
                   :top 200 :left 225  
                   :height 25 :width 125 
                   :border :plain
                   :justification :center
                   :on-change 'draw-3d-pie-chart))
         (static-print (make-instance 'button
                         :title "Print 3D Pie Chart"
                         :top 200 :left 370 :height 25 :width 125 
                         :border :plain
                         :justification :center
                         :on-change 'prntpage))
         (width 200)
         (grid-height 250)
         (grid
          (make-instance '3d-pie-grid
            :name :3d-pie-grid
            :left 0 :top 0
            :width width
            :height grid-height
            :column-sections
            (list
             (make-instance 'grid-column-section
               :name :only
               :size 400
               :proportional t
               :scrollbars nil
               :subsections
               (list
                (make-instance '3d-pie-sector
                  :header-title "SECTOR"
                  :font (make-font :modern nil 12) 
                  :name :sector
                  :proportional t)
                (make-instance '3d-pie-%
                  :header-title "%"
                  :name :%
                  :font (make-font :modern nil 12) 
                  :justification :center
                  :proportional t)
                (make-instance '3d-pie-color
                  :header-title "COLOR"
                  :name :color
                  :font (make-font :modern nil 12) 
                  :proportional t)
                (make-instance '3d-pie-label
                  :header-title "LABEL"
                  :name :label
                  :font (make-font :modern nil 12) 
                  :proportional t))))
            :row-sections
            (list
             (make-instance '3d-column-header-row-section
               :name :header
               :scrollbars nil
               :proportional nil
               :size 30
               :subsections
               (list (make-instance '3d-column-header-row
                       :name :header
                       :selectable nil
                       :size 30
                       :proportional t)))             
             (make-instance 'grid-row-section
               :name :body
               :proportional nil
               :scrollbars t
               :subsections
               (list
                (make-instance '3d-pie-row
                  :name :body
                  :border-size 1
                  :proportional nil))))))
         
         (dialog (make-window :3d-pie-dialog
                   :title "3D-PIE-CHART-EDITOR"
                   :name :3d-pie-dialog
                   :class '3d-pie-dialog
                   :background-color (system-dialog-background-color)
                   :interior (make-box-relative
                              250 200 500 grid-height)
                   :widgets (list grid static static-print))))   
    dialog))

(defmethod draw-cell ((row 3d-pie-row)
                      (column 3d-pie-sector)
                      row-num column-num cell-box stream)
  (declare 
   (ignore-if-unused row-num column-num))
  (let* ((grid-widget (parent (parent row)))
         (num (nth row-num (row-list grid-widget))))
    (grid-draw-string stream 
                      num
                      cell-box
                      (cell-wrapped-p row column)
                      (cell-horizontal-justification 
                       row column)
                      (cell-vertical-justification 
                       row column))))

(defmethod draw-cell ((row 3d-pie-row)
                      (column 3d-pie-label)
                      row-num column-num cell-box stream) 
  (declare 
   (ignore-if-unused row-num column-num))
  (let* ((grid-widget (parent (parent row)))
         (num (nth row-num (label-list grid-widget))))
    (grid-draw-string stream 
                      num
                      cell-box
                      (cell-wrapped-p row column)
                      (cell-horizontal-justification 
                       row column)
                      (cell-vertical-justification 
                       row column))))


(defmethod draw-cell ((row 3d-pie-row)
                      (column 3d-pie-%)
                      row-num column-num cell-box stream)
  (declare 
   (ignore-if-unused row-num column-num))
  (let* ((grid-widget (parent (parent row)))
         (num (nth row-num (%-list grid-widget))))
    (draw-string-in-box stream 
                        num
                        0
                        (length num)
                        cell-box
                        :center
                        :center)))

(defmethod draw-cell ((row 3d-pie-row)
                      (column 3d-pie-color)
                      row-num column-num cell-box stream)
  (declare 
   (ignore-if-unused row-num column-num))
  (let* ((grid-widget (parent (parent row)))         
         (color (nth row-num (color-list grid-widget))))
    (with-foreground-color (stream color)
      (fill-box stream cell-box))))


(defmethod draw-cell ((row 3d-column-header-row)
                      (column 3d-pie-sector)
                      row-num column-num cell-box stream)
  (declare (ignore-if-unused row-num column-num))
  (draw-string-in-box stream (header-title column)                      
                      nil nil cell-box
                      (cell-horizontal-justification row column)
                      (cell-vertical-justification row column)
                      nil (cell-wrapped-p row column)))

(defmethod draw-cell ((row 3d-column-header-row)
                      (column 3d-pie-%)
                      row-num column-num cell-box stream)
  (declare (ignore-if-unused row-num column-num))
  (draw-string-in-box stream (header-title column)                       
                      nil nil cell-box
                      (cell-horizontal-justification row column)
                      (cell-vertical-justification row column)
                      nil (cell-wrapped-p row column)))

(defmethod draw-cell ((row 3d-column-header-row)
                      (column 3d-pie-color)
                      row-num column-num cell-box stream)
  (declare (ignore-if-unused row-num column-num))
  (draw-string-in-box stream (header-title column)                      
                      nil nil cell-box
                      (cell-horizontal-justification row column)
                      (cell-vertical-justification row column)
                      nil (cell-wrapped-p row column)))

(defmethod draw-cell ((row 3d-column-header-row)
                      (column 3d-pie-label)
                      row-num column-num cell-box stream)
  (declare (ignore-if-unused row-num column-num))
  (draw-string-in-box stream (header-title column) 
                      
                      nil nil cell-box
                      (cell-horizontal-justification row column)
                      (cell-vertical-justification row column)
                      nil (cell-wrapped-p row column)))

(defmethod cell-click ((grid-widget 3d-pie-grid) buttons
                       (column-section grid-column-section)
                       (column-section-border-p (eql nil))
                       (column 3d-pie-sector)
                       column-num (column-border-p (eql nil)) x
                       (row-section 3d-column-header-row-section)
                       (row-section-border-p (eql nil))
                       (row 3d-column-header-row)
                       row-num (row-border-p (eql nil)) y
                       &optional trigger-key) 
  (declare 
   (ignore-if-unused trigger-key x y buttons row-num column-num))
  (enter-num-sector-dialog))


(defmethod cell-click ((grid-widget 3d-pie-grid) buttons
                       (column-section grid-column-section)
                       (column-section-border-p (eql nil))
                       (column 3d-pie-label)
                       column-num (column-border-p (eql nil)) x
                       (row-section grid-row-section)
                       (row-section-border-p (eql nil))
                       (row 3d-pie-row)
                       row-num (row-border-p (eql nil)) y
                       &optional trigger-key)
  (declare 
   (ignore-if-unused trigger-key x y row-num column-num buttons))
  (invalidate-cell row column row-num column-num t)
  (let* ((grid (find-component :3d-pie-grid :3d-pie-dialog))
         (cell-box (cell-box row-section column-section 
                             row column
                             row-num column-num))
         (label 
          (multiple-value-bind (string1 string2 button accepted)
              (ask-user-for-string "LABEL" 
                                   (nth row-num (label-list grid)) 
                                   "OK" "CANCEL")
            (declare (ignore string2 button))
            (and accepted string1))))
    (when label
      (erase-contents-box (grid-drawing-pane (window grid)) cell-box)
      (draw-string-in-box (grid-drawing-pane (window grid)) 
                          label
                          0
                          (length label)
                          cell-box
                          :center
                          :center
                          t
                          t)
      (setf  (nth  row-num  (label-list grid)) label))))

(defmethod cell-click ((grid-widget 3d-pie-grid) buttons
                       (column-section grid-column-section)
                       (column-section-border-p (eql nil))
                       (column 3d-pie-color)
                       column-num (column-border-p (eql nil)) x
                       (row-section grid-row-section)
                       (row-section-border-p (eql nil))
                       (row 3d-pie-row)
                       row-num (row-border-p (eql nil)) y
                       &optional trigger-key)
  (declare (ignore-if-unused x y buttons trigger-key)) 
  (let* ((cell-box (cell-box row-section column-section 
                             row column
                             row-num column-num))
         (grid (find-component :3d-pie-grid :3d-pie-dialog))
         (color (ask-user-for-color))
         (length-orig-color-list (length (color-list grid))))
    (if color
        (with-foreground-color ((grid-drawing-pane (window grid)) color)     
          (fill-box (grid-drawing-pane (window grid)) cell-box)))
    (dotimes (i (-  (length row-list) length-orig-color-list))
      (setf (color-list grid) (append (color-list grid) (list nil))))
    (if color
        (setf (nth row-num (color-list grid)) color))))

(defmethod cell-click ((grid-widget 3d-pie-grid) buttons
                       (column-section grid-column-section)
                       
                       ;; Don't do this if the user clicks a border.
                       (column-section-border-p (eql nil))
                       
                       ;; Do this operation in the righthand side
                       ;; of the grid only.
                       (column 3d-pie-%)
                       
                       column-num (column-border-p (eql nil)) x
                       (row-section grid-row-section)
                       (row-section-border-p (eql nil))
                       (row 3d-pie-row)
                       row-num (row-border-p (eql nil)) y
                       &optional trigger-key)
  (declare (ignore-if-unused buttons column-num x y trigger-key))
  (call-next-method)
  (let* ((old-y (position-y (cursor-position (screen *system*))))
         (%-list (%-list grid-widget))
         
         ;; If the user clicked the second sub-row, then we
         ;; should edit the second % in the % list.
         (% (read-from-string (nth row-num %-list)))
         (diff 0)
         (old-diff 0)
         (old-value   %)
         
         (scratch-board (make-window :scratch-board
                          :owner (parent
                                  (find-component :3d-pie-grid 
                                                  :3d-pie-dialog))
                          :title "TOTAL %"  
                          :name :scratch-board
                          :interior (make-box-relative
                                     225 50 226 60)
                          :dialog-items (list                                         
                                         (make-instance 'editable-text
                                           :name :my-total
                                           :value "0"
                                           )))))
    (declare (ignore-if-unused scratch-board))
    
    
    
    ;; Temporarily stop drawing the focus rectangle in the cell
    ;; while dragging, to avoid annoying flashing.
    
    (setf (show-focus grid-widget) nil)
    
    
    (unwind-protect
        (loop
          ;; Adjust the % sector until the user releases all mouse buttons.
          (when (eq (mouse-button-state) 0)(return))
          ;; Allow events to be handled promptly while doing the drag;
          ;; this includes the mouse button release to stop the drag.
          (process-pending-events)
          
          ;; Find out how far the mouse has moved vertically since
          ;; the last time we checked.  If it hasn't moved at all,
          ;; just keep waiting to avoid flashing.
          
          
          (setq diff (- (position-y (cursor-position (screen *system*)))
                        old-y))
          
          (unless (eq diff old-diff)
            
            (cond ((and (>= (check-my-sum %-list old-value diff row-num) 100)
                        (>= 
                         (max 0 (min 100 (- old-value diff))) 
                         (read-from-string (nth row-num 
                                                (%-list grid-widget)) )))
                   (progn
                     (unless  (eql (read-from-string (nth row-num 
                                                          (%-list grid-widget)))
                                   (- (read-from-string (nth row-num 
                                                             (%-list grid-widget))) 
                                      (- (check-my-sum %-list old-value diff row-num) 100)))                      
                       (setf (nth row-num (%-list grid-widget)) 
                         (format nil "~a"                              
                           (- (read-from-string (nth row-num 
                                                     (%-list grid-widget))) 
                              (- (check-my-sum %-list old-value diff row-num) 100))))
                       (invalidate-cell row column row-num))))
                  (t   
                   (progn                     
                     (setf (nth row-num (%-list grid-widget)) 
                       (format nil "~d"                                            
                         (max 0 (min 100 (- old-value diff)))))
                     (setq old-diff diff)  
                     (invalidate-cell row column row-num)))))
          (setf (show-focus grid-widget) t)
          (let ((sum 0))            
            (dolist (elt (%-list grid-widget))
              (setf sum (+ (read-from-string elt) sum)))
            
            (setf (value (find-component
                          :my-total 
                          (first (windows (parent 
                                           (find-component
                                            :3d-pie-grid 
                                            :3d-pie-dialog))))))
              (format nil "~d"  sum)))))
    (user-close (parent (find-component
                         :my-total
                         (first (windows (parent 
                                          (find-component
                                           :3d-pie-grid
                                           :3d-pie-dialog)))))))
    (setf (show-focus grid-widget) t)))

(defun edit-row (row-list)
  (let* ((grid (find-component :3d-pie-grid :3d-pie-dialog))
         (row-section (row-section grid :body))
         (row (subsection row-section :body)))    
    (setf (row-list grid) row-list)
    (setf (section-count row) (length row-list))
    (setf (section-size row) 70)
    (edit-color (color-list grid))
    (edit-% (%-list grid))
    (edit-label (label-list grid))))

(defun edit-color (color-list)
  (let ((grid (find-component :3d-pie-grid :3d-pie-dialog)))    
    (if (> (length color-list) (length row-list))
        (loop for i from (length row-list) to 
              (length color-list) do 
              (setf color-list  (remove (nth  (- i 1) color-list)
                                        color-list))
              (setf (color-list grid) color-list)))   
    (if (< (length color-list) (length row-list))
        (loop for i from (length color-list) to
              (- (length  row-list) 1) do    
              (setf color-list (append color-list (list nil)))
              (setf (color-list grid) color-list)))))      

(defun edit-% (%-list)  
  (let ((grid (find-component :3d-pie-grid :3d-pie-dialog))
        (length% (length %-list)))   
    (if (> (length %-list) (length row-list))
        (loop for i from 1 to 
              length% do 
              (setf %-list  (remove (nth (- length% i) %-list) %-list))
              (setf (%-list grid) %-list)))    
    (if (< (length %-list) (length row-list))
        (loop for i from (length %-list)  to
              (- (length  row-list) 1) do
              (setf %-list (append %-list (list "0"))) 
              (setf (%-list grid) %-list)))))


(defun edit-label (label-list)  
  (let ((grid (find-component :3d-pie-grid :3d-pie-dialog)))    
    (if (> (length label-list) (length row-list))
        (loop for i from (length row-list) to 
              (length label-list) do 
              (setf label-list  (remove (nth  (- i 1) label-list)
                                        label-list))
              (setf (label-list grid) label-list)))
    (if (< (length label-list) (length row-list))
        (loop for i from (length label-list) to
              (- (length  row-list) 1) do
              (setf label-list (append label-list (list nil))) 
              (setf (label-list grid) label-list)
              ))))

(defun run-3d-pie-chart-example ()
  (let* ((dialog (make-3d-pie-chart-dialog)))
    (setf row-list (list "1" "2" "3"))
    (setf (%-list (find-component :3d-pie-grid :3d-pie-dialog))
      (list "0" "0" "0"))
    (edit-row row-list)
    dialog))

(defun enter-num-sector-dialog ()
  (let ((scratch-board (make-window :scratch-board
                         :class 'dialog
                         :owner (parent
                                 (find-component :3d-pie-grid 
                                                 :3d-pie-dialog))
                         :title "ENTER NUMBER OF SECTORS"                           
                         :interior (make-box-relative
                                    225 50 230 75)
                         :dialog-items (list                                         
                                        (make-instance 'default-button
                                          :state :shrunk
                                          :on-change 'my-close-window)
                                        (make-instance 'editable-text
                                          :name :my-text
                                          :on-change
                                          'num-of-sectors )))))
    (declare (ignore-if-unused scratch-board))))

(defun num-of-sectors (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setf row-list '())
  (dotimes (i (read-from-string new-value))
    (setf row-list (append row-list (list (format nil "~s" (+ i 1))))))
  (setf (row-list (find-component :3d-pie-grid :3d-pie-dialog)) row-list)
  (edit-row row-list)
  
  t) ; Accept the new value

(defun my-close-window (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (user-close (parent widget)))

(defun draw-3d-pie-chart (dialog-item new-value old-value)
  (declare (ignore new-value old-value))
  (let* ((dialog (parent dialog-item))
         (row (row-list (find-component :3d-pie-grid dialog)))
         (color (color-list (find-component :3d-pie-grid dialog)))
         (label (label-list (find-component :3d-pie-grid dialog)))
         (% (%-list (find-component :3d-pie-grid dialog)))
         (suma 0)
         (sumb 0)
         (sumc 0)
         (num 0))
    (erase-contents-box dialog (make-box 200 0 550 250))
    (draw-ellipse dialog (make-position 350 75) 100 40 0)
    (draw-line dialog (make-position 250 75) (make-position 250 115))
    (draw-line dialog (make-position 450 75) (make-position 450 115))
    (draw-ellipse-arc dialog (make-position 350 115) 100 40 0 0 180)
    
    
    
    (dotimes (j (length row))
      
      (setf suma 0)
      (setf sumb 0)
      (setf num j)
      
      (dotimes (i (+ num 1 ))
        (if (eql i 0) (setf sumc 0)
          (setf sumc (+ sumc  (* (/ (read-from-string (nth (- i 1) %))
                                    100)
                                 360)))))
      
      (dotimes (i (+ num 1))
        (setf suma (+ (* (/ (read-from-string (nth i %)) 100) 360)
                      suma)))
      
      
      (if (eql j 0)
          (setf sumb (* (/ (read-from-string (nth 0 %)) 100) 360))   
        (dotimes (i num)           
          (setf sumb (+ (* (/ (read-from-string (nth i %)) 100) 360)
                        sumb))))
      
      (unless (eq (read-from-string (nth j %)) 0) 
        (draw-ellipse-sector dialog (make-position 350 75) 100 40 0
                             (round sumb )
                             (round (- suma sumb)))
        
        (multiple-value-bind (start-x start-y end-x end-y)
            (ellipse-start-and-end (make-position 350 75)
                                   100 40 0
                                   (round sumb)
                                   (round (- suma 360 sumb 360)))
          (declare (ignore start-x start-y))
          (multiple-value-bind (start-x start-y enda-x endb-y)
              (ellipse-start-and-end (make-position 350 115)
                                     100 40 0 (round sumb)
                                     (round (- suma sumb 360)))
            (declare (ignore start-x start-y))
            
            
            (unless (>= (float sumc) 176.4)
              (if (and (>= end-x 250)
                       (<= end-y 75))
                  (progn
                    (setf end-x 250)
                    (setf enda-x 250)
                    (setf end-y 75)
                    (setf endb-y 115)))
              (draw-line dialog (make-position end-x end-y) 
                         (make-position enda-x endb-y))  
              (with-foreground-color  (dialog (nth j color))                              
                (flood-fill dialog (make-position (+ 1 end-x)
                                                  (+ 10 end-y))))
              )            
            (if (eql j 0 ) (setf sumb 0))
            (with-foreground-color (dialog (nth j color))
              (fill-ellipse-sector  dialog (make-position 350 75)
                                   100 40 0 (round sumb)
                                   (round (- suma sumb))))
            
            (with-line-width (dialog 2)  
              (draw-ellipse dialog (make-position 350 75) 100 40 0)
              (draw-line dialog (make-position 250 75)
                         (make-position 250 115))
              (draw-line dialog (make-position 450 75)
                         (make-position 450 115))
              (draw-ellipse-arc dialog (make-position 350 115)
                                100 40 0 0 180))        
            (draw-label sumb suma (nth j label) black
                        dialog)))))))

(defun draw-label (start-angle end-angle label color dialog)
  
  (if  (<= (+ (/ (- end-angle start-angle) 2) start-angle) 90.0)
      (progn 
        (multiple-value-bind (start-x start-y end-x end-y)
            (ellipse-start-and-end
             (make-position 350 115) 100 40 0 start-angle
             (/ (- end-angle 360 start-angle 360) 2 ))
          (declare (ignore start-x start-y))
          (with-foreground-color (dialog color)
            (draw-string-in-box
             dialog label 0 (length label) 
             (make-box end-x end-y (+ end-x 40) (+ end-y 20))                              
             :center :center)))))
  
  (if (and (> (+ (/ (- end-angle start-angle) 2) start-angle)90.0)
           (<= (+ (/ (- end-angle start-angle) 2) start-angle) 180.0))
      (progn
        (multiple-value-bind (start-x start-y end-x end-y)
            (ellipse-start-and-end
             (make-position 350 115) 100 40 0 start-angle
             (/ (- end-angle 360 start-angle 360) 2 ))  
          (declare (ignore start-x start-y))
          (with-foreground-color (dialog color)
            (draw-string-in-box
             dialog label 0 (length label) 
             (make-box (- end-x 40) end-y end-x (+ end-y 20) )                              
             :center :center)))))
  
  (if (and (> (+ (/ (- end-angle start-angle) 2) start-angle) 180.0)
           (<= (+ (/ (- end-angle start-angle) 2) start-angle)290.0))
      (progn
        (multiple-value-bind (start-x start-y end-x end-y)
            (ellipse-start-and-end
             (make-position 350 75) 100 40 0 start-angle
             (/ (- end-angle 360 start-angle 360) 2))
          (declare (ignore start-x start-y))
          (with-foreground-color (dialog color)
            (draw-string-in-box
             dialog label 0 (length label) 
             (make-box (- end-x 40) (- end-y 20) end-x end-y)
             :center :center)))))
  
  (if (and (> (+ (/  (- end-angle start-angle) 2) start-angle) 290.0)
           (<= (+ (/ (- end-angle start-angle) 2) start-angle) 360.0))
      (progn        
        (multiple-value-bind (start-x start-y end-x end-y)
            (ellipse-start-and-end
             (make-position 350 75) 100 40 0 start-angle
             (/ (- end-angle 360 start-angle 360) 2))  
          (declare (ignore start-x start-y))
          (with-foreground-color (dialog color)
            (draw-string-in-box
             dialog label 0 (length label) 
             (make-box end-x (- end-y 20) (+ end-x 40) end-y)                              
             :center :center))))))

(defun prntpage (widget new-value old-value)
  (declare (ignore new-value old-value))
  (let ((dialog (parent widget))
        (prn nil))
    (setf prn (open-stream 'printer 'null-location :output))
    (when prn
      (with-hourglass
        (sleep 5)        
        (setf (stream-units-per-inch prn)
          (stream-units-per-inch (screen *system*)))
        (copy-stream-area prn dialog
                          (make-box 0 0 500 250) 
                          (make-box 0 0 500 250)
                          po-replace))
      (close prn)))) ;; bug13607

(defun check-my-sum (check-%-list old-value diff row-num)
  (setf check-sum 0)
  (dolist (elt check-%-list)
    (if (eq elt row-num)
        (setf check-sum (+ (format nil "~d" (max 0 (min 100
                                                        (- old-value
                                                           diff))))
                           check-sum))
      (setf check-sum (+ (read-from-string elt) check-sum))))
  check-sum)

#+run-example
(run-3d-pie-chart-example)

