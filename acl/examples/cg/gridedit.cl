;; -=Begin Copyright Notice=- draw-cell
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

(defparameter adder-array-width 26)
(defparameter adder-array-height 40)

(defparameter adder-cell-height 24)
(defparameter adder-header-cell-width 40)
(defparameter adder-body-cell-width 80)
(defparameter adder-totals-cell-width 100)

(defparameter adder-grid-width 800)
(defparameter adder-grid-height 500)

;;; This is the single array that holds all of the data for the table.
;;; Make it one bigger than the official table size in order to store
;;; the totals in the extra element at the end of each row or column.
(defparameter adder-array
    (make-array (list (1+ adder-array-height)
                      (1+ adder-array-width))
                :initial-element 0))

;;; Define some grid classes so that we can specialize on them below.
(defclass adder-row (grid-row)())
(defclass adder-header-row (column-sizing-row-mixin adder-row)())
(defclass adder-body-row (widget-row-mixin adder-row)())
(defclass adder-totals-row (adder-row)())

(defclass adder-column (grid-column)())
(defclass adder-header-column (row-sizing-column-mixin
                               adder-column)
  ())

(defclass adder-body-column (editable-text-column-mixin adder-column)
  ()
  (:default-initargs
      :edit-start-trigger :begin-typing
    :edit-end-trigger :enter-or-arrow-keys))

(defclass adder-totals-column (adder-column)())


(defmethod read-cell-value ((row adder-body-row)(column adder-body-column)
                            row-number column-number)
  
  ;;; This is the general method for returning a cell value from our
  ;;; array of application data so that the grid can display it
  ;;; in a grid-widget cell.
  (aref adder-array row-number column-number))

(defmethod write-cell-value ((row adder-body-row)(column adder-body-column)
                             row-number column-number value)
  
  ;;; This is the general method that takes a value that the user has
  ;;; typed into an editable-text cell of the grid and stores it into
  ;;; our array of application data.
  
  ;;; Convert the typed string into a number to store.
  (let* ((lisp-value (ignore-errors (read-from-string value))))
    
    ;; If the user entered a valid value, then store it in our array
    ;; of domain data.  This is the main purpose of write-cell-value.
    (if* (numberp lisp-value)
       then (setf (aref adder-array row-number column-number)
              lisp-value)
            
            ;; Now do the side effects for this particular application,
            ;; which is to recompute the totals and redisplay them.
            ;; First find the grid cells to update, which are over in
            ;; other grid sections (so that the totals sections don't
            ;; scroll with the main section).
            (let* ((totals-row
                    (subsection (find-sibling :totals (parent row))
                                :totals))
                   (totals-column
                    (subsection (find-sibling :totals (parent column))
                                :totals)))
            
              ;; These calls will find the new sum for the row of
              ;; the modified cell, and invalidate the cell in the
              ;; totals column for that value so that it gets redrawn
              ;; to display the newly-stored total.
              ;; Pass the redraw-now-p argument as t so that the
              ;; individual cell will be redrawn rather than adding
              ;; to an encompassing invalid region that would draw more.
              (refresh-row-sum row-number)
              (invalidate-cell row totals-column row-number 0 t)
              
              ;; These lines do the same for the column of the
              ;; modified cell.
              (refresh-column-sum column-number)
              (invalidate-cell totals-row column 0 column-number t)
            
            ;; These calls will add all of the column totals to
            ;; find the overall total, and refresh the lower-right cell.
            (refresh-row-sum adder-array-height)
            (invalidate-cell totals-row totals-column 0 0 t))
            
            ;; If the user typed in an invalid value, avoiding
            ;; corrupting our array of application data by doing nothing
            ;; here other than beeping.
       else (beep))))

(defun refresh-row-sum (row-number)
  "Finds and stores the current total for a whole row."
  (do* ((sum 0)
        (column-number 0 (1+ column-number)))
       ((>= column-number adder-array-width)
        (setf (aref adder-array row-number adder-array-width) sum))
    (incf sum (aref adder-array row-number column-number))))
       
(defun refresh-column-sum (column-number)
  "Finds and stores the current total for a whole column."
  (do* ((sum 0)
        (row-number 0 (1+ row-number)))
       ((>= row-number adder-array-height)
        (setf (aref adder-array adder-array-height column-number) sum))
    (incf sum (aref adder-array row-number column-number))))

;;; These next methods read and write data for the cells in the
;;; rows and columns for headers and totals.

(defmethod read-cell-value ((row adder-body-row)
                            (column adder-totals-column)
                            row-number column-number)
  (declare (ignore column-number))
  
  ;; Return a value to display in a cell of the totals column
  ;; (wherever a body row crosses the totals column).
  ;; We store these in the last column of our array.
  (aref adder-array row-number adder-array-width))

(defmethod read-cell-value ((row adder-totals-row)
                            (column adder-body-column)
                            row-number column-number)
  (declare (ignore row-number))
  
  ;; Return a value to display in a cell of the totals row
  ;; (wherever a body column crosses the totals row).
  ;; We store these in the last row of our array.
  (aref adder-array adder-array-height column-number))

(defmethod read-cell-value ((row adder-totals-row)
                            (column adder-totals-column)
                            row-number column-number)
  (declare (ignore row-number column-number))
  
  ;; Return the value to display in the lower-right cell for the
  ;; full total (where the totals row crosses the totals column).
  (aref adder-array adder-array-height adder-array-width))

(defmethod read-cell-value ((row adder-header-row) 
                            (column adder-body-column)
                            row-num column-num)
  (declare (ignore row-num))
  
  ;; Return a letter to display in a cell of the header row.
  (format nil "~a" (code-char (+ column-num 97))))

(defmethod read-cell-value ((row adder-body-row) 
                            (column adder-header-column)
                            row-num column-num)
  (declare (ignore column-num))
  
  ;; Return a number to display in a cell of the header column.
  row-num)


(defun run-grid-edit-example ()
  
  ;; This is the entry point function that makes the grid and
  ;; the window that it's in.
  
  ;; These first two objects define the main body section of the grid
  ;; where users type in numbers to be added up.
  (let* ((body-row-section
          (make-instance 'grid-row-section
            :name :body
            :scrollbars t
            :proportional t
            :resizable nil
            :border-width 2
            :subsections
            (list (make-instance 'adder-body-row
                    :name :body
                    :size adder-cell-height
                    
                    ;; This argument replicates the single row object.
                    :section-count adder-array-height))))
         (body-column-section
          (make-instance 'grid-column-section
            :name :body
            :scrollbars t
            :proportional t
            :resizable nil
            :border-width 2
            :subsections
            (list (make-instance 'adder-body-column
                    :name :body
                    :size adder-body-cell-width
                    :section-count adder-array-width))))
         
         ;; Define a grid with the above body section plus other
         ;; small sections for headers and totals.
         (grid (make-instance 'grid-widget
                 :name :adder-grid
                 :left 0 :top 0
                 :width adder-grid-width :height adder-grid-height
                 :border-width 0
                 :default-section-border-color dark-blue
                 :right-attachment :right
                 :bottom-attachment :bottom
                 :row-sections
                 (list (make-instance 'grid-row-section
                         :name :header
                         :scrollbars nil
                         
                         ;; Tell the headers and footers not to stretch,
                         ;; so that resizing the window will stretch
                         ;; only the main body section.
                         :proportional nil
                         
                         :size adder-cell-height
                         :border-width 2
                         :subsections
                         (list (make-instance 'adder-header-row
                                 :name :header
                                 :proportional t)))
                       body-row-section
                       (make-instance 'grid-row-section
                         :name :totals
                         :scrollbars nil
                         :proportional nil
                         :size adder-cell-height
                         :subsections
                         (list (make-instance 'adder-totals-row
                                 :name :totals
                                 :proportional t))))
                 
                 :column-sections
                 (list (make-instance 'grid-column-section
                         :name :header
                         :scrollbars nil
                         :proportional nil
                         :size adder-header-cell-width
                         :border-width 2
                         :subsections
                         (list (make-instance 'adder-header-column
                                 :name :header
                                 :proportional t)))
                       body-column-section
                       (make-instance 'grid-column-section
                         :name :totals
                         :scrollbars nil
                         :proportional nil
                         :size adder-totals-cell-width
                         :subsections
                         (list (make-instance 'adder-totals-column
                                 :name :totals
                                 :proportional t))))))
         (window (make-window :adder-grid
                   :title "Replicated Editable-Text-Column Example --- Enter numbers to see sums in two directions"
                   :class 'frame-window
                   :interior (make-box-relative
                              100 200
                              adder-grid-width adder-grid-height)
                   :dialog-items (list grid)
                   :scrollbars nil
                   :resizable t))
         )
    
    ;; Instead of focusing by default on the top-left do-nothing
    ;; header cell, initially focus in the upper-left cell of the
    ;; body section, so that the user can immediately use the arrow
    ;; keys or tab key to move around in the data.
    (set-focus-cell grid body-row-section body-column-section
                    (subsection body-row-section :body)
                    (subsection body-column-section :body)
                    0 0)
    
    ;; Now that everything is set up, show the window.
    (select-window window)
    
    ;; Return the window so that this function can be the
    ;; on-initialization function of a project for a standalone app.
    window))


;;; And now some special colors and fonts for header sections
;;; and totals sections, to distinguish them from the body.

(defmethod cell-background-color ((row adder-body-row)
                                  (column adder-header-column))
  
  ;; Return the background-color for the header column
  ;; (where the body rows and the header column overlap).
  #.(make-rgb :red 224 :green 224 :blue 255))

(defmethod cell-background-color ((row adder-header-row)
                                  (column adder-body-column))
  
  ;; Return the background-color for the header row
  ;; (where the header row and the body column overlap).
  #.(make-rgb :red 224 :green 224 :blue 255))
  
(defmethod cell-font ((row adder-body-row)
                      (column adder-header-column))
  
  ;; Return the font for the header column
  ;; (where the body rows and the header column overlap).
  ;; Just make the usual font be bold.
  (vary-font (call-next-method) :style '(:bold)))

(defmethod cell-font ((row adder-header-row)
                      (column adder-body-column))
  
  ;; Return the font for the header row
  ;; (where the header row and the body column overlap).
  ;; Just make the usual font be bold.
  (vary-font (call-next-method) :style '(:bold)))
  
(defmethod cell-background-color ((row adder-body-row)
                                  (column adder-totals-column))
  
  ;; Return the background-color for the totals column
  ;; (where the body rows and the totals column overlap).
  #.(make-rgb :red 255 :green 224 :blue 224))

(defmethod cell-background-color ((row adder-totals-row)
                                  (column adder-body-column))
  
  ;; Return the background-color for the totals row
  ;; (where the totals row and the body column overlap).
  #.(make-rgb :red 255 :green 224 :blue 224))
  
(defmethod cell-background-color ((row adder-totals-row)
                                  (column adder-totals-column))
  
  ;; Return the background-color for the lower-right full total cell
  ;; (where the totals row and the totals column overlap).
  #.(make-rgb :red 255 :green 196 :blue 196))

#+run-example
(run-grid-edit-example)

