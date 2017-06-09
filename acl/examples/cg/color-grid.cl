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

;; Color Editor --- A fairly simple grid-widget example.

;; Displays a set of colors as separate red, green, and blue
;; components, one grid row for each color.  The user drags
;; component cells vertically to adjust the components of the colors.
;; A version of this grid is used by the Emerge example.

(in-package :cg-user)

;; Subclass some basic grid components so that we can customize them

(defclass color-grid (grid-widget)
  ((color-list :initform nil :accessor color-list)))

(defclass color-grid-row (grid-row)())

(defclass color-grid-column (grid-column)())

(defclass color-grid-left-column (color-grid-column)())

(defclass color-grid-right-column (color-grid-column)())

(defclass color-dialog (dialog)())

;; Make a color-grid with a standard set of rows and columns.
(defun make-color-dialog ()
  (let* ((width 200)
         (grid-height 250)
         (static-text-height 160)
         (grid
          (make-instance 'color-grid
            :name :color-grid
            :left 0 :top 0
            :width width
            :right-attachment :right
            :bottom-attachment :bottom
            :height grid-height
            :column-sections
            (list
             
             ;; The column-section on the left side is for the
             ;; actual color resulting from the red, green,
             ;; and blue components that the user edits.
             (make-instance 'grid-column-section
               :name :left
               :size 100
               :proportional t
               :scrollbars nil
                
               ;; This column section has only a single column in it.
               :subsections
               (list
                (make-instance 'color-grid-left-column
                  :name :rgb
                  :proportional t)))
             
             ;; The column-section on the right side has three
             ;; columns, one for each of the red, green, and
             ;; blue components of each color.
             (make-instance 'grid-column-section
               :name :right
               :size 200
               :proportional t
               :scrollbars nil
               :subsections
               (list
                (make-instance 'color-grid-right-column
                  :name :red
                  :proportional t)
                (make-instance 'color-grid-right-column
                  :name :green
                  :proportional t)
                (make-instance 'color-grid-right-column
                  :name :blue
                  :proportional t))))
            
            ;; The grid has a single row-section, with a
            ;; single row in it.  (This "single row" will later be
            ;; divided into multiple "sub-rows" for each color.)
            :row-sections
            (list
             (make-instance 'grid-row-section
               :name :body
               :proportional t
               :scrollbars nil
               :subsections
               (list
                (make-instance 'color-grid-row
                  :name :body
                  :border-size 1
                  :proportional t))))))
         
         ;; A text widget below the grid for messages.
         (static (make-instance 'static-text
                   :left 0 :top grid-height
                   :width width :height static-text-height
                   :top-attachment :bottom
                   :right-attachment :right
                   :bottom-attachment :bottom
                   :value #.(format nil "Click the RGB components ~
 in the righthand section of the grid and drag up or down ~
 to edit the colors displayed in the lefthand column.  Or press ~
 the F and D keys to increment the component with the keyboard focus.")))
         
         ;; The window to place the grid-widget in.
         (dialog (make-window :color-dialog
                   :title "Color Editor"
                   :class 'color-dialog
                   :state :shrunk
                   :interior (make-box-relative
                              500 200 width
                              (+ grid-height static-text-height))
                   :dialog-items (list grid static)))
         )
    dialog))

;; Draw each actual color in the left column of the grid.
;; Draw-cell is called automatically whenever a cell needs to be drawn.
(defmethod draw-cell ((row color-grid-row)
                      (column color-grid-left-column)
                      row-num column-num cell-box stream)
  (declare (ignore column-num))
  (let* ((grid-widget (parent (parent row)))
         (color (nth row-num (color-list grid-widget))))
    (with-foreground-color (stream color)
      (fill-box stream cell-box))))

;; Draw the individual red, green, and blue components
;; of each color in the righthand section of the grid.
(defmethod draw-cell ((row color-grid-row)
                      (column color-grid-right-column)
                      row-num column-num cell-box stream)
  (declare (ignore column-num))
  (let* ((grid-widget (parent (parent row)))
         (color-list (color-list grid-widget))
         (color (nth row-num color-list))
         intensity)
    
    ;; Fill the component cell with a color consisting of just
    ;; this red, green, or blue component, so that the intensity of
    ;; the color indicates how much of that component is mixed in.
    (with-foreground-color
        (stream
         (case (name column)
           (:red (make-rgb :red (setq intensity (rgb-red color))))
           (:green (make-rgb :green (setq intensity (rgb-green color))))
           (:blue (make-rgb :blue (setq intensity (rgb-blue color))))))
      (fill-box stream cell-box))
    
    ;; Print the actual value of the red, green, or blue component
    ;; of the color in the cell for reference.  Use a color that
    ;; contrasts with the background color of the cell for readability.
    (with-font (stream (make-font-ex nil :arial 16 '(:bold)))
      (with-foreground-color (stream (if (> intensity 196) black white))
        (draw-string-in-box stream (prin1-to-string intensity)
                            nil nil cell-box :center :center)))))

;; Here's the fancy method to handle mouse clicks in our grid.
;; This code waits for the user to drag the mouse up or down
;; from one of the color component cells, adjusting that
;; component of the color based on how far the mouse is dragged.

(defmethod cell-click ((grid-widget color-grid) buttons
                       (column-section grid-column-section)
                       
                       ;; Don't do this if the user clicks a border.
                       (column-section-border-p (eql nil))
                       
                       ;; Do this operation in the righthand side
                       ;; of the grid only.
                       (column color-grid-right-column)
                       
                       column-num (column-border-p (eql nil)) x
                       (row-section grid-row-section)
                       (row-section-border-p (eql nil))
                       (row color-grid-row)
                       row-num (row-border-p (eql nil)) y
                       &optional trigger-key)
  (declare (ignore buttons column-num x y trigger-key))
  (call-next-method)
  (let* ((old-y (position-y (cursor-position (screen *system*))))
         (color-list (color-list grid-widget))
         
         ;; If the user clicked the second sub-row, then we
         ;; should edit the second color in the color list.
         (color (nth row-num color-list))
         
         (rgb-column (subsection (column-section grid-widget :left)
                                 :rgb))
         (old-diff 0)
         
         ;; Find which component of the color to edit from the
         ;; name of the column that the user clicked.
         (old-value (case (name column)
                      (:red (rgb-red color))
                      (:green (rgb-green color))
                      (:blue (rgb-blue color))))
         
         diff)
    
    ;; Temporarily stop drawing the focus rectangle in the cell
    ;; while dragging, to avoid annoying flashing.
    (setf (show-focus grid-widget) nil)
    
    (unwind-protect
        (loop
          
          ;; Adjust the color until the user releases all mouse buttons.
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
            (setq old-diff diff)
            
            ;; Adjust the appropriate component of the color.
            (case (name column)
              (:red (setf (rgb-red color)
                      (max 0 (min 255 (- old-value diff)))))
              (:green (setf (rgb-green color)
                        (max 0 (min 255 (- old-value diff)))))
              (:blue (setf (rgb-blue color)
                       (max 0 (min 255 (- old-value diff))))))
            
            (invalidate-cell row column row-num)
            (invalidate-cell row rgb-column row-num))))
    (setf (show-focus grid-widget) t)))

;;; Make the F and D keys increment or decrement
;;; the color component cell that has the keyboard focus.
(defmethod cell-key-down ((grid-widget color-grid)
                          row-section column-section row
                          (column color-grid-right-column)
                          row-num column-num buttons key-code)
  (declare (ignore row-section column-section column-num buttons))
  (when (or (eq key-code #.(char-int #\F))
            (eq key-code #.(char-int #\D)))
    (let* ((color (nth row-num (color-list grid-widget)))
           (increment (if (eq key-code #.(char-int #\F)) 8 -8))
           (rgb-column (subsection (column-section grid-widget :left)
                                   :rgb)))
      ;; Adjust the appropriate component of the color.
      (case (name column)
        (:red (setf (rgb-red color)
                (max 0 (min 255 (+ (rgb-red color) increment)))))
        (:green (setf (rgb-green color)
                  (max 0 (min 255 (+ (rgb-green color) increment)))))
        (:blue (setf (rgb-blue color)
                 (max 0 (min 255 (+ (rgb-blue color) increment))))))
      
      ;; Redraw this color component cell.
      (invalidate-cell row column row-num)
      
      ;; Redraw the cell that shows the actual color of which 
      ;; this red, green, or blue component makes up a part.
      (invalidate-cell row rgb-column row-num))))
     
;;; Avoid flashing by overriding the default method to clear
;;; a grid-section (called by draw-grid).  We are filling the
;;; whole cells with color, so they don't need to be cleared first.
(defmethod clear-grid-section ((grid color-grid) row-section
                               column-section drawing-pane cell-box)
  (declare (ignore row-section column-section drawing-pane cell-box))
  nil)

;; This returns the color dialog, creating it when needed.
(eval-when (compile load eval)
  (let* ((color-dialog nil))
    (defun color-dialog ()
      (if* (windowp color-dialog)
         then color-dialog
         else (setq color-dialog
                    (make-color-dialog))))))

;; The entry-point function for editing a set of colors.
;; The list of colors passed in will be destructively modified.

(defun edit-colors (color-list)
  (let* ((dialog (color-dialog))
         (grid (find-component :color-grid dialog))
         (row-section (row-section grid :body))
         (column-section (column-section grid :right))
         (row (subsection row-section :body))
         (column (subsection column-section :red)))
    
    ;; Store our color data on the grid for future reference.
    (setf (color-list grid) color-list)
    
    ;; Make one "sub-row" for each color.
    (setf (section-count row)(length color-list))
    
    ;; Focus on the red component of the first sub-row.
    ;; The cell-key-down method will affect the focused cell.
    (set-focus-cell grid row-section column-section row column)
    (select-window dialog)
    dialog))

(defun run-color-grid-example ()
  (edit-colors (list (make-rgb :red 196)
                     (make-rgb :red 128 :green 64 :blue 196)
                     (make-rgb :red 255 :green 232))))

#+run-example
(run-color-grid-example)
