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

;;; Simple Grid Example

;;; The grid-widget is very versatile and has a significant learning
;;; curve, so here is the simplest possible grid example to introduce
;;; the widget.  See the documentation page for the grid-widget class
;;; for an overview of all grid-widget functionality.

;;; This grid simply draws or erases an X in a cell whenever the
;;; user clicks the cell.  It uses a custom draw-cell method to
;;; draw the X, and a custom cell-click method to toggle the
;;; official value for a cell when the user clicks it.

;;; This example stores its data to display in a single array
;;; that's stored on the grid-widget.  This is an array
;;; of boolean values indicating which cells should have an
;;; X drawn in them.  In a real application, your data would
;;; probably be stored elsewhere in your application; there
;;; is additional grid functionality for conveniently reading
;;; the data to display from CLOS objects.

;;; This grid has a single scrolling section.  In a real application
;;; you would probably want to add additional sections, such as for
;;; row and column headers so that the body section can scroll
;;; independently of the header sections.

;;; This grid uses a single grid-row object and a single grid-column
;;; object.  These objects have a "count" property to draw several
;;; visible rows or columns using the single row or column object.
;;; This is possible when it's OK for all of the visible rows or
;;; columns to look and act exactly the same.  If you want a
;;; different appearance or different behavior in various rows or
;;; columns, then you will need to make a separate grid-row or
;;; grid-column object for each one.

;;; This grid does not use any of the handy built-in cell types
;;; such as the ones for letting the user type a string into a
;;; cell or select a value from a drop-down list.  Other grid
;;; examples demonstrate those built-in cell types.

(defparameter *simple-grid-rows* 20)

(defparameter *simple-grid-columns* 15)

;;; Make subclasses of grid-widget, grid-row, and grid-column
;;; so that we can supply methods that apply only to our grids.
(defclass simple-grid (grid-widget)
  
  ;; This array holds the domain data to display.
  ((grid-data :initform (make-array (list *simple-grid-rows*
                                          *simple-grid-columns*)
                                    :initial-element nil)
              :accessor grid-data)))

(defclass simple-grid-row (grid-row)())

(defclass simple-grid-column (grid-column)())

;;; Call this function to run this example.
(defun run-simple-grid-example ()
  (let* ((width 300)
         (height 300)
         (grid (make-instance 'simple-grid
                 :name :simple-grid
                 :left 0 :top 0
                 :width width
                 :height height
                 :right-attachment :right
                 :bottom-attachment :bottom
                 
                 :column-sections
                 (list
                  (make-instance 'grid-column-section
                    :name :body
                    :size width
                    :proportional t
                    :scrollbars t
                    :subsections
                    (list
                     (make-instance 'simple-grid-column
                       :name :body
                       :count *simple-grid-columns*
                       :proportional t))))
                 
                 :row-sections
                 (list
                  (make-instance 'grid-row-section
                    :name :body
                    :proportional t
                    :scrollbars t
                    :subsections
                    (list
                     (make-instance 'simple-grid-row
                       :name :body
                       :count *simple-grid-rows*
                       :proportional t)))))))
    
    (make-window :simple-grid-dialog
      :class 'frame-window
      :title "Simple Grid - Click the Cells"
      :scrollbars nil
      :interior (make-box-relative
                 400 200 width height)
      :dialog-items (list grid))))

(defmethod draw-cell ((row simple-grid-row)
                      (column simple-grid-column)
                      row-num column-num cell-box stream)
  
  ;; Draw the contents of each cell of this grid-widget.
  ;; This method is called automatically whenever a
  ;; cell needs to be drawn.
  
  (let* ((grid (parent (parent row)))
         (value (aref (grid-data grid) row-num column-num)))
    
    ;; If this cell's value is currently t rather than nil,
    ;; then draw an X in the cell.  When nil, leave it blank.
    (when value
      (draw-line stream (box-top-left cell-box)
                 (box-bottom-right cell-box))
      (draw-line stream (box-top-right cell-box)
                 (box-bottom-left cell-box)))))

(defmethod cell-click ((grid-widget simple-grid) buttons
                       (column-section grid-column-section)
                       
                       ;; Don't do this if the user clicks a border
                       ;; rather than the interior of a cell.
                       (column-section-border-p (eql nil))
                       
                       (column simple-grid-column)
                       column-num (column-border-p (eql nil)) x
                       (row-section grid-row-section)
                       (row-section-border-p (eql nil))
                       (row simple-grid-row)
                       row-num (row-border-p (eql nil)) y
                       &optional trigger-key)
  (declare (ignore buttons x y trigger-key))
  
  ;; This method is called when the user clicks on a cell
  ;; of our grid.  Here we tell the grid to draw
  ;; or erase the X that it displays.
  
  ;; (Pressing the spacebar or ENTER key will also toggle the
  ;; X due to behaviour that's built into all grid-widgets to
  ;; make those two keys emulate a left-click by default.)
  
  ;; Let the default method move the keyboard focus to the
  ;; cell that the user clicked.
  (call-next-method)
  
  ;; Toggle the value for the clicked cell so that it will
  ;; either start or stop drawing an X.
  (let* ((array (grid-data grid-widget)))
    (setf (aref array row-num column-num)
      (not (aref array row-num column-num))))
    
  ;; Invalidate the clicked cell so that our draw-cell
  ;; method will be called to draw its new value.
  (invalidate-cell row column row-num column-num))

#+run-example
(run-simple-grid-example)

