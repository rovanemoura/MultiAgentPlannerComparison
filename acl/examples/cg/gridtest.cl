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

;; The magnificent employee grid example.

;; A sample application using the grid-widget.
;; Shows a list employees with various modifiable attributes.

(in-package :cg-user)

;; ---------------------------------------------------------------
;; Define a class for domain employee objects to display on each grid row

(defclass employee ()
  ;; The user domain object for an employee grid,
  ;; whose values will be represented in a grid row.
  (;; The name of the employee
   (name :initarg :name
         :accessor name
         :initform nil)
   
   ;; Whether the employee works full-time
   (full-time-p :initarg :full-time-p
                :accessor full-time-p
                :initform nil)
   
   ;; Current department
   (department :initarg :department
               :accessor department
               :initform nil)
   
   ;; Departments to choose from
   (departments :initarg :departments
                :accessor departments
                :initform (list :tech :sales :marketing :mngmnt
                                :admin :none_of_the_above))
   
   ;; Current bar color
   (bar-color :initarg :bar-color
              :accessor bar-color
              :initform 'red)
   
   ;; Bar colors to choose from
   (bar-colors :initarg :bar-colors
               :accessor bar-colors
               :initform '(red yellow green blue))
   
   ;; Full, Partial, or No Access
   (access :initarg :access
           :accessor access
           :initform nil)
   
   ;; Stressed or Not
   (stressed :initarg :stressed
             :accessor stressed
             :initform nil)
   
   ;; Job Description
   (duties :initarg :duties
           :accessor duties
           :initform "None")
   
   ;; Experience
   (experience :initarg :experience
               :accessor experience
               :initform "None")
   
   ;; Generic monthly data for this employee
   (bar-values :initarg :bar-values
               :accessor bar-values
               :initform nil)))

(defparameter *example-employees*
  (list
   (make-instance 'employee
     :name "Meredith"
     :department :admin
     :access nil
     :stressed t
     :duties "Performs any activity that everyone else says is not in their job description"
     :experience "Has done lots of differents sorts of things for various employers"
     :bar-values (list 3 4 5 6 7 8 9 8 7 6 5 4))
   (make-instance 'employee
     :name "Forest"
     :department :shipping
     :access :partial
     :duties "Labels, lifts, and lugs large loads"
     :experience "Has lifted over 1800 tons of boxes"
     :bar-color 'yellow
     :bar-values (list 2 4 3 5 4 6 5 7 6 8 7 9 6 8 5 7))
   (make-instance 'employee
     :name "Trent"
     :full-time-p t
     :department :mngmnt
     :access nil
     :stressed :extremely
     :duties "Locks programmers in small rooms until their code works"
     :experience "Has allowed only 3 escapes in 15 years of experience"
     :bar-color 'blue
     :bar-values
     (list 5 3 6 4 7 5 8 6 9 7 8 6 7 5 6 4 3 1))
   (make-instance 'employee
     :name "Fiona"
     :full-time-p t
     :department :tech
     :access :full
     :duties "Entertains the other employess so they can motivate themselves to come to work"
     :experience "Has sung in 37 different showers, and has only been kicked out of a few of them"
     :bar-color 'green
     :bar-values
     (list 8 9 7 9 10 8 9 7 9 10 7 9 8))
   (make-instance 'employee
     :name "Sublime"
     :department :tech
     :access :full
     :duties "Comes in late at night and corrects everyone's mistakes"
     :experience "Two generations of mistake correction"
     :bar-values
     (list 2 7 2 7 2 7 2 7 2 7 2 7))
   (make-instance 'employee
     :name "The Frog"
     :department :none
     :access nil
     :duties "Sleeps; eats; provides moral support"
     :experience "Has consumed much food and learned to sleep in most any location in previous and current lives"
     :bar-color 'yellow
     :bar-values
     (list 2 4 3 5 4 6 5 7 6 8 7 9 6 8 5 7))
   (make-instance 'employee
     :name "Dr. Fegg"
     :full-time-p t
     :departments '(:sales :shipping :support) ;; Fewer possibilities for Fegg
     :department :sales
     :access nil
     :stressed :extremely
     :duties "Frightens customers into purchasing product"
     :experience "Has sat in an attic and ponderred doing many remarkle things"
     :bar-color 'green
     :bar-values
     (list 8 9 7 9 10 8 9 7 9 10 7 9 8))
   (make-instance 'employee
     :name "Boof"
     :department :marketing
     :access :partial
     :duties "Creates silly sentences for use in demos"
     :experience "Has answered the question \"Boof?  What kind of name is Boof?  Is that some kind of joke?\" frequently for many years"
     :bar-color 'blue
     :bar-values
     (list 1 10 2 9 3 7 4 6 5 5 6 4 7 3))
   (make-instance 'employee
     :name "Bjork"
     :department :shipping
     :access :partial
     :stressed t
     :duties "Invents new sounds with mouth"
     :experience "Has been here and there"
     :bar-color 'yellow
     :bar-values (list 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2))
   (make-instance 'employee
     :name "Ellis"
     :department :marketing
     :access nil
     :duties "Increases name recognition"
     :experience "Name has been overheard in many department stores"
     :bar-color 'red
     :bar-values (list 9 8 7 6 5 4 3 2 1 2 3 4 5 6 7 8))
   (make-instance 'employee
     :name "Meredith2"
     :department :admin
     :access :full
     :stressed t
     :duties "Performs any activity that everyone else says is not in their job description"
     :experience "Has done lots of differents sorts of things for various employers"
     :bar-values (list 3 4 5 6 7 8 9 8 7 6 5 4))
   (make-instance 'employee
     :name "Forest2"
     :department :shipping
     :access nil
     :duties "Labels, lifts, and lugs large loads"
     :experience "Has lifted over 1800 tons of boxes"
     :bar-color 'yellow
     :bar-values (list 2 4 3 5 4 6 5 7 6 8 7 9 6 8 5 7))
   (make-instance 'employee
     :name "Trent2"
     :full-time-p t
     :department :mngmnt
     :access :partial
     :stressed :extremely
     :duties "Locks programmers in small rooms until their code works"
     :experience "Has allowed only 3 escapes in 15 years of experience"
     :bar-color 'blue
     :bar-values
     (list 5 3 6 4 7 5 8 6 9 7 8 6 7 5 6 4 3 1))
   (make-instance 'employee
     :name "Fiona2"
     :full-time-p t
     :department :tech
     :access :full
     :duties "Entertains the other employess so they can motivate themselves to come to work"
     :experience "Has sung in 37 different showers, and has only been kicked out of a few of them"
     :bar-color 'green
     :bar-values
     (list 8 9 7 9 10 8 9 7 9 10 7 9 8))
   (make-instance 'employee
     :name "Sublime2"
     :department :tech
     :access :partial
     :duties "Comes in late at night and corrects everyone's mistakes"
     :experience "Two generations of mistake correction"
     :bar-values
     (list 2 7 2 7 2 7 2 7 2 7 2 7))
   (make-instance 'employee
     :name "The Frog2"
     :department :none
     :access nil
     :duties "Sleeps; eats; provides moral support"
     :experience "Has consumed much food and learned to sleep in most any location in previous and current lives"
     :bar-color 'yellow
     :bar-values
     (list 2 4 3 5 4 6 5 7 6 8 7 9 6 8 5 7))
   (make-instance 'employee
     :name "Dr. Fegg2"
     :full-time-p t
     :departments '(:sales :shipping :support) ;; Fewer possibilities for Fegg
     :department :sales
     :access nil
     :stressed :extremely
     :duties "Frightens customers into purchasing product"
     :experience "Has sat in an attic and ponderred doing many remarkle things"
     :bar-color 'green
     :bar-values
     (list 8 9 7 9 10 8 9 7 9 10 7 9 8))
   (make-instance 'employee
     :name "Boof2"
     :department :marketing
     :access :full
     :duties "Creates silly sentences for use in demos"
     :experience "Has answered the question \"Boof?  What kind of name is Boof?  Is that some kind of joke?\" frequently for many years"
     :bar-color 'blue
     :bar-values
     (list 1 10 2 9 3 7 4 6 5 5 6 4 7 3))
   (make-instance 'employee
     :name "Bjork2"
     :department :shipping
     :access :partial
     :stressed t
     :duties "Invents new sounds with mouth"
     :experience "Has been here and there"
     :bar-color 'yellow
     :bar-values (list 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2))
   (make-instance 'employee
     :name "Ellis2"
     :department :marketing
     :access nil
     :duties "Increases name recognition"
     :experience "Name has been overheard in many department stores"
     :bar-color 'red
     :bar-values (list 9 8 7 6 5 4 3 2 1 2 3 4 5 6 7 8))
   ))

;; A grid row for each employee
(defun grid-example-rows ()
  (let* ((people *example-employees*))
    (mapcar #'(lambda (employee)
                (make-instance 'employee-row
                  :data-object employee
                  :proportional nil
                  :size 40))
      people)))

;; ---------------------------------------------------------------
;; Make our own grid section subclasses, so that we can 
;; define methods that apply only to our own grids

(defclass employee-row-header-column (row-header-column) ())

;; Make a subclass of grid-column for storing column titles in the
;; worksheet section of our grid
(defclass worksheet-column (grid-column)
  ((header-title :initarg :header-title
                 :accessor header-title
                 :initform nil)
   (header-horizontal-justification
    :initarg :header-horizontal-justification
    :accessor header-horizontal-justification
    :initform :center)
   (header-font :initarg :header-font
                :accessor header-font
                :initform nil)))

(defclass name-column (editable-text-column-mixin
                       worksheet-column)
  ()
  (:default-initargs
      :data-reader 'name
    :data-writer '(setf name)
    :on-sort-predicate 'string<))

(defclass full-time-p-column (check-box-column-mixin worksheet-column)
  ()
  (:default-initargs
      :data-reader 'full-time-p
    :data-writer '(setf full-time-p)
    :title-reader 'full-time-p-title-reader
    :on-sort-predicate #'(lambda (a b)(and a (not b)))))

(defmethod cell-vertical-justification 
    ((row grid-row)(column full-time-p-column))
  
  ;; Make our check-boxes be center-justified vertically
  ;; instead of top-justified (the default).
  :center)

(defun full-time-p-title-reader (employee)
  ;; An example of returning custom strings for a column of check-boxes
  (format nil "~:(~a~)'s full-time"
    (name employee)))

(defun make-department-symbol (string)
  ;; This will convert a string that the user types into a department
  ;; cell into a keyword symbol to write back to the employee
  ;; object.  This will also get called for a symbol selected from
  ;; the drop-down list, and simply keep that symbol as is.
  (intern (change-case-like-reader string) :keyword))

(defclass department-column
    (combo-box-column-mixin worksheet-column)
  ()
  (:default-initargs
      :data-reader 'department
    :data-writer '(setf department)
    :edit-end-trigger :enter-or-arrow-keys
    
    ;; The on-print formats values for the drop-down list of choices.
    :on-print 'capitalize-if-symbol
    
    ;; The data-read-converter formats values for the cell itself.
    :data-read-converter 'capitalize-if-symbol
    
    ;; The data-write-converter converts a newly-entered value to the
    ;; type of value that should be written back to the domain object.
    :data-write-converter 'make-department-symbol
    
    :range-reader 'departments
    :on-sort-predicate 'string<
    :typable t))

(defclass inspect-column
    (static-text-and-button-column-mixin worksheet-column)
  ()
  (:default-initargs
      :sortable nil
    :button-fills-cell t
    :data-reader 'identity
    :data-read-converter 'employee-inspect-string
    :button-function 'inspect-button-function))

(defun employee-inspect-string (employee)
  
  ;; An example of returning custom strings for a column of buttons
  (and employee
       (format nil "Inspect ~:(~a~)"
         (name employee))))

;;; chee   25oct05 rfe6346 add row-number and column-number arguments
;;;        to a button cell's button function for consistency
(defun inspect-button-function
    (grid-widget data-object row column row-number column-number)
  (declare (ignore grid-widget row column row-number column-number))
  (inspect data-object))

(defclass bar-color-column (combo-box-column-mixin worksheet-column)
  ()
  (:default-initargs
      :data-reader 'bar-color
    :data-writer '(setf bar-color)
    :on-print 'capitalize-if-symbol
    :data-read-converter 'capitalize-if-symbol
    :range-reader 'bar-colors
    :on-sort-predicate 'string<
    :use-real-combo-box t
    :typable nil))

(defclass access-column (pixmap-column-mixin worksheet-column)
  ()
  (:default-initargs
      :pixmap-alist '((nil nil)
                      (:partial :key)
                      (:full :key-special))
    :data-reader 'access
    :data-writer '(setf access)))

(defclass duties-column (editable-text-column-mixin worksheet-column)
  ()
  (:default-initargs
      :data-reader 'duties
    :data-writer '(setf duties)
    :on-sort-predicate 'string<))

(defclass stressed-column (lamp-column-mixin worksheet-column)
  ())

(defclass bar-chart-column (grid-column) ()
  (:default-initargs
      :sortable nil
    :data-reader 'bar-values))

(defclass bar-total-column (worksheet-column)()
  (:default-initargs
      :data-reader 'bar-total
    :on-sort-predicate '<))

;; Define a row-section class to serve as our column headers.
;; Give it the row-section-with-sort-gadget-mixin so that it will
;; display the gadget for sorting the grid in its (empty) scrollbar area.
(defclass employee-column-header-row-section
    (row-section-with-sort-gadget-mixin grid-row-section) ())

;; Define a grid-row class for the single row in the column-header section.
(defclass employee-column-header-row (column-header-row) ())

;; Define a grid-row class to use for each employee in the grid body.
(defclass employee-row (widget-row-mixin grid-row) ())

;; ---------------------------------------------------------------
;; Define our custom version of the grid widget
(defclass employee-grid-widget (grid-widget)
  ;; The employee grid subclass
  ()
  (:default-initargs
      :name :employee-1
    :title "Employee Info"
    :font (make-font-ex nil :arial 13)
    
    :column-sections
    (list
     
     ;; The first column section is the row header, simply displaying
     ;; a number for each row
     (make-instance 'grid-column-section
       :name :row-header
       :scrollbars nil
       :proportional nil
       :size 20
       :subsections
       (list (make-instance 'employee-row-header-column
               :name :row-number
               :selectable nil
               :size 20
               :proportional t)))
     
     ;; The second column section has a single column, which is the
     ;; editable name of each employee
     (make-instance 'grid-column-section
       :name :name
       :scrollbars nil
       :proportional nil
       :size 80
       :subsections
       (list (make-instance 'name-column
               :name :name
               :header-title "Name"
               :header-font (make-font-ex nil :arial 13 '(:bold))
               :font (make-font-ex nil :arial 15 '(:bold :italic))
               :size 80
               :proportional t)))
     
     ;; The third columnm section is a scrollable set of cells
     ;; conveying miscellaneous info about each employee
     (make-instance 'grid-column-section
       :name :worksheet
       :scrollbars t
       :proportional t
       :size 240
       :subsections
       (list 
        (make-instance 'full-time-p-column
          :name :full-time-p
          :header-title "Full Time?"
          :header-font (make-font-ex nil :arial 13 '(:bold))
          :size 100
          :proportional nil)
        (make-instance 'access-column
          :name :access
          :header-title "Ac"
          :header-font (make-font-ex nil :arial 13 '(:bold))
          :size 24
          :resizable nil
          :proportional nil)
        (make-instance 'department-column
          :name :department
          :header-title "Department"
          :header-font (make-font-ex nil :arial 13 '(:bold))
          :size 100
          :proportional nil)
        (make-instance 'inspect-column
          :name :inspect
          :header-font (make-font-ex nil :arial 13 '(:bold))
          :header-title "Inspect"
          :size 75
          :proportional nil)
        (make-instance 'bar-color-column
          :name :bar-color
          :header-title "Bar Color"
          :header-font (make-font-ex nil :arial 13 '(:bold))
          :size 75
          :resizable nil
          :proportional nil)
        (make-instance 'duties-column
          :name :duties
          :header-title "Duties"
          :header-font (make-font-ex nil :arial 13 '(:bold))
          :size 250
          :proportional nil
          :data-reader 'duties)
        (make-instance 'stressed-column
          :name :stressed
          :header-title "Stressed?"
          :header-font (make-font-ex nil :arial 13 '(:bold))
          :size 110
          :proportional t
          :data-reader 'stressed
          :data-read-converter (lambda (user-value)
                                 (case user-value
                                   (:extremely :blinking)
                                   (t user-value)))
          :title-reader (lambda (employee)
                          (format nil "Is ~:(~a~) stressed?"
                            (name employee)))
          :sort-predicate (lambda (a b)
                            (or (eq a :extremely)
                                (and a (not b))))
          :lamp-widget (make-instance 'lamp
                         :outer-horizontal-margin 8
                         :inner-horizontal-margin 10
                         :wrapping t))
        ))
     
     ;; The fourth column section is a bar graph.
     ;; Add the actual bar columns later, just to show the other
     ;; way of adding columns.
     (make-instance 'grid-column-section
       :name :bar-chart
       :scrollbars t
       :proportional t
       :resizable nil
       :size 200
       :subsections nil))
    
    :row-sections
    (list
     
     ;; The first row section consists of the column headers
     (make-instance 'employee-column-header-row-section
       :name :header
       :scrollbars nil
       :proportional nil
       :resizable t
       :size 30
       :subsections
       (list (make-instance 'employee-column-header-row
               :name :header
               :selectable nil
               :size 30
               :proportional t)))
     
     ;; The second row section contains the actual employee rows.
     ;; Add the data rows to this row section at runtime individually,
     ;; since the domain data will typically vary from one run to the
     ;; next.
     (make-instance 'grid-row-section
       :name :body
       :scrollbars t
       :proportional t
       :resizable nil
       :subsections (grid-example-rows)))))

;; ---------------------------------------------------------------
;; Define methods that determine how each cell is drawn.
;; These multimethods specialize on both the row and the column

;; The font for the column header cells
(defmethod cell-font
    ((row employee-column-header-row)(column worksheet-column))
  (or (header-font column)
      (call-next-method)))

;; The jusitification for the column header cells
(defmethod cell-horizontal-justification
    ((row employee-column-header-row)(column worksheet-column))
  (or (header-horizontal-justification column)
      (call-next-method)))

;; The jusitification for the row header cells
(defmethod cell-horizontal-justification
    ((row grid-row)(column employee-row-header-column))
  :right)

;; Row numbers for the row header cells
(defmethod draw-cell ((row employee-row)(column employee-row-header-column)
                      row-num column-num cell-box stream)
  (declare (ignore row-num column-num))
  (draw-string-in-box
   stream
   (princ-to-string
    (1+ (position row (subsections (parent row))
                  :test #'eq)))
   nil nil
   (inflate-box cell-box
                (- (cell-horizontal-padding row column))
                (- (cell-vertical-padding row column)))
   (cell-horizontal-justification row column)
   (cell-vertical-justification row column)))

;; Column header color
(defmethod cell-background-color ((row employee-column-header-row)
                                  (column grid-column))
  (system-dialog-background-color))

;; Row header color
(defmethod cell-background-color ((row grid-row)
                                  (column employee-row-header-column))
  (system-dialog-background-color))

;; Draw the top-left cell where the column-header and row-header intersect
(defmethod draw-cell ((row employee-column-header-row)
                      (column employee-row-header-column)
                      row-num column-num cell-box stream)
  (declare (ignore row-num column-num))
  (draw-string-in-box
   stream "#" nil nil
   (inflate-box cell-box
                (- (cell-horizontal-padding row column))
                (- (cell-vertical-padding row column)))
   (cell-horizontal-justification row column)
   (cell-vertical-justification row column)))

;; Justification for the data cells in the employee name column
(defmethod cell-horizontal-justification
    ((row employee-row)(column name-column)) 
  :left)

(defmethod cell-vertical-justification
    ((row employee-row)(column name-column)) 
  :center)

;; Vertically center justify all header cell text.
(defmethod cell-vertical-justification 
    ((row employee-column-header-row)(column grid-column))
  :center)

;; Left-justify the "Duties" text.
(defmethod cell-horizontal-justification
    ((row employee-row)(column worksheet-column))
  :left)

;; Draw the worksheet column headers
(defmethod draw-cell ((row employee-column-header-row)
                      (column worksheet-column)
                      row-num column-num cell-box stream)
  (declare (ignore row-num column-num))
  (draw-string-in-box stream (or (header-title column) "")
                      nil nil cell-box
                      (cell-horizontal-justification row column)
                      (cell-vertical-justification row column)
                      nil (cell-wrapped-p row column)))

;; Give the worksheet data cells a 3D border
#+maybe
(defmethod cell-3d-border ((row employee-row)(column worksheet-column))
  t)

;; Font for the total column
(defmethod cell-font ((row employee-row)(column bar-total-column))
  (make-font-ex nil :arial 15 '(:bold)))

;; Font for the bars
(defmethod cell-font ((row employee-row)(column bar-chart-column))
  (make-font-ex nil :arial 9))

(defparameter *grid-bar-cell-margin* 4)
(defparameter *grid-example-bar-max* 10)
(defparameter *grid-example-bar-middle* 4)

(defun bar-total (employee)
  (apply #'+ (bar-values employee)))

;; Show the total of all 12 months for each employee
(defmethod draw-cell ((row employee-row)(column bar-total-column)
                      row-num column-num cell-box stream)
  (declare (ignore row-num column-num))
  (and (data-object row)
       (draw-string-in-box
        stream (prin1-to-string (bar-total (data-object row)))
        nil nil cell-box :center :center)))

;; Draw each employee bar chart cell
(defmethod draw-cell ((row employee-row)(column bar-chart-column)
                      row-num column-num cell-box stream)
  (unless (data-object row)
    (return-from draw-cell))
  (with-boxes (temp-box)
    (ncopy-box temp-box cell-box)
    (let* ((employee (data-object row))
           (bar-values (read-cell-value row column row-num column-num))
           (max *grid-example-bar-max*)
           (bar-value (nth column-num bar-values)))
      
      ;; Leave left and right margins within the cell for the bar
      (incf (box-left temp-box) *grid-bar-cell-margin*)
      (decf (box-right temp-box) *grid-bar-cell-margin*)
      
      ;; Compute the height of the bar relative to the cell height
      (setf (box-top temp-box)
        (- (box-bottom temp-box)
           (floor (* (- (box-height cell-box)
                        *grid-bar-cell-margin*)
                     (min max (max 0
                                   (or bar-value
                                       (floor max 2)))))
                  max)))
      
      ;; Due to the draw-box below, decrement the right and bottom
      ;; sides of the box, because draw-box actually draws the row
      ;; of pixels just AFTER the end of the area in the box that
      ;; you pass to it.  Since we are drawing right to the bottom
      ;; of the row, this would otherwise sometimes clip the bottom
      ;; draw-box border off.
      (decf (box-right temp-box))
      (decf (box-bottom temp-box))
      
      ;; Draw the interior of the bar
      (setf (foreground-color stream)
        (symbol-value (bar-color employee)))
      (fill-box stream temp-box)
      
      ;; Draw the border of the bar
      (setf (foreground-color stream)
        (cell-foreground-color row column))
      (draw-box stream temp-box)
      (draw-string-in-box
       stream (prin1-to-string bar-value) nil nil cell-box
       :center (if (< bar-value *grid-example-bar-middle*)
                   :center
                 :bottom)))))

;; Draw the bar chart column headers
(defmethod draw-cell ((row employee-column-header-row)
                      (column bar-chart-column)
                      row-num column-num cell-box stream)
  (draw-string-in-box
   stream (read-cell-value row column row-num column-num)
   nil nil cell-box
   (cell-horizontal-justification row column)
   (cell-vertical-justification row column)))

(defparameter *months* #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                         "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defmethod read-cell-value ((row employee-column-header-row)
                            (column bar-chart-column)
                            row-num column-num)
  (declare (ignore row-num))
  (aref *months* column-num))


;;; These two methods (along with the button-fills-cell property)
;;; will cause the buttons in the month header cells to completely
;;; fill the cell, with no margin inside the cell borders.
(defmethod cell-horizontal-padding ((row employee-column-header-row)
                                    (column bar-chart-column))
  0)

(defmethod cell-vertical-padding ((row employee-column-header-row)
                                  (column bar-chart-column))
  0)

;; ---------------------------------------------------------------
;; Adding cell-widget methods is a straightforward way to use
;; the built-in cell widgets in arbitrary cells.

(defparameter *long-months*
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

;; This method turns the month header cells into buttons
;; that show a message dialog.
(defmethod cell-widget ((row employee-column-header-row)
                        (column bar-chart-column)
                        row-number column-number)
  (declare (ignore row-number column-number))
  (values :button ;; make this cell be a button
          
          ;; Run this function when the button is clicked.
          (lambda (grid-widget object row column row-num col-num)
            (declare (ignore object row column row-num))
            (pop-up-message-dialog
             (window grid-widget) "Month Index"
             (format nil "~:(~a~) is the ~:r month of the year."
               (aref *long-months* col-num)(1+ col-num))
             information-icon :~OK))
          
          ;; Whether the button fills the cell, or is small
          ;; in the lower right corner (see button-fills-cell).
          t
          
          ;; The string or pixmap to draw (nil defaults to the value
          ;; returned by read-cell-value).
          nil ;; this would draw a pixmap: (find-pixmap 'lamp)
          
          ;; The button's background color (nil defaults
          ;; the value returned by cell-background-color).
          nil))

;; This method illustrates making an arbitrary cell
;; be a button widget.  This cell-widget method will
;; override the widget-row-mixin functionality.
(defmethod cell-widget ((row employee-row)
                        (column name-column)
                        row-number column-number)
  (cond ((string-equal (read-cell-value
                        row column row-number column-number)
                       "Trent")
         (values :button ;; make this cell be a button
                 
                 ;; Run this function when the button is clicked.
                 (lambda (grid-widget object row column row-num col-num)
                   (declare (ignore object row column row-num col-num))
                   (pop-up-message-dialog
                    (window grid-widget) "Don't Mess with Trent"
                    "Trent won't let you change his name."
                    warning-icon :~OK))
                 
                 nil)))) ;; button-fills-cell

;; ---------------------------------------------------------------
;; Methods to implement grid cell constraints, where changing one cell
;; causes changes to other cells or other arbitrary side effects

;; After changing the employee name, redraw the row of
;; worksheet cells that display that name in various ways
(defmethod write-cell-value :around ((row employee-row)
                                     (column name-column)
                                     row-number column-number
                                     value)
  (declare (ignore row-number column-number))
  (let ((old-value (read-cell-value row column 0 0)))
    (call-next-method)
    (unless (string= value old-value)
      
      ;; Invalidate this section so that its cells will be redrawn
      ;; using the new employee name
      (invalidate-section row
                          (find-sibling :worksheet (parent column)))
      (window-message (parent (parent (parent row)))
          "~:(~a~) now prefers to be called \"~:(~a~)\"."
        old-value value)
      )))

(defmethod write-cell-value :around ((row employee-row)
                                     (column full-time-p-column)
                                     row-number column-number
                                     value)
  (declare (ignore row-number column-number))
  (call-next-method)
  (window-message (parent (parent (parent row)))
      (if value
          "~:(~a~) is broke." 
        "~:(~a~) can't take it anymore.")
    (name (data-object row))))

(defmethod write-cell-value :around ((row employee-row)
                                     (column duties-column)
                                     row-number column-number
                                     value)
  (declare (ignore row-number column-number value))
  (call-next-method)
  (window-message (parent (parent (parent row)))
      "~:(~a~) is now a more valuable employee (more work; same pay)."
    (name (data-object row))))

;; After changing the bar color via the combo-box,
;; redraw the bars in that color
(defmethod write-cell-value :around ((row employee-row)
                                     (column bar-color-column)
                                     row-number column-number
                                     value)
  (declare (ignore row-number column-number))
  (let ((former-color (read-cell-value row column 0 0)))
    (call-next-method)
    (unless (eq former-color value)
      (invalidate-section
       row
       (subsection (find-sibling :bar-chart (parent column)) :bars))
      (window-message (parent (parent (parent row)))
          "~:(~a~) likes ~a better than ~a."
        (name (data-object row))
        value former-color))))

;; If change department to :tech, then make the employee full-time
(defmethod write-cell-value :after ((row employee-row)
                                    (column department-column)
                                    row-number column-number
                                    value)
  (when (eq :tech value)
    (let* ((full-time-p-column (find-sibling :full-time-p column)))
      (unless (read-cell-value row full-time-p-column
                               row-number column-number)
        (write-cell-value row full-time-p-column
                          row-number column-number t)
        (invalidate-cell row full-time-p-column 0 0 nil t))))
  (window-message (parent (parent (parent row)))
      "~:(~a~) has been shunted over to the ~a department."
    (name (data-object row)) value))
   
(defmethod write-cell-value :after ((row employee-row)
                                    (column access-column)
                                    row-number column-number
                                    value)
  (declare (ignore row-number column-number))
  (window-message (parent (parent (parent row)))
      "~:(~a~) now has ~a access."
    (name (data-object row))
    (case value ((nil) "no")(:partial "partial")(:full "full"))))

;; Don't allow inspection of Trent or Dr. Fegg
(defmethod cell-available ((row employee-row)(column inspect-column))
   (and (data-object row)
        (not (member (name (data-object row))
               '("Trent" "Dr. Fegg")
               :test #'string=))))

(defmethod cell-click (grid-widget (buttons (eql left-mouse-button))
                                   column-section (column-section-border-p (eql nil))
                                   (column bar-chart-column)
                                   column-num (column-border-p (eql nil)) x
                                   row-section (row-section-border-p (eql nil))
                                   (row employee-row)
                                   row-num (row-border-p (eql nil)) y
                                   &optional trigger-key)
  (declare (ignore column-section x y trigger-key))
  ;; Allow the user to change bar values by clicking or dragging them
  (unless (and (cell-and-sections-available row column)
               (data-object row))
    (return-from cell-click))
  (do* ((employee (data-object row))
        (drawing-pane (grid-drawing-pane (window grid-widget)))
        (top (- (+ (edge-position row-section)
                   (edge-position row)
                   *grid-bar-cell-margin*)
                (scroll-position row-section)))
        (size (- (section-size row)
                 *grid-bar-cell-margin*))
        (values (bar-values employee))
        (original-value (nth column-num values))
        (previous-value original-value value)
        (value previous-value
               (round (* *grid-example-bar-max*
                         (- size
                            (- (position-y (ncursor-position
                                            drawing-pane
                                            #.(make-position 0 0)))
                               top)))
                      size)))
       
       ;; Stop dragging when no mouse button is still down.
       ((zerop (logand (mouse-button-state)
                       #.(logior left-mouse-button middle-mouse-button
                                 right-mouse-button)))
        
        ;; Redisplay the total now that we have stop dragging the bar
        (unless (eq value original-value)
          (invalidate-cell row (find-sibling :total column)))
        ;; Set focus to this cell
        (call-next-method))
    ;; Allow the up-click to be processed so that we can get out
    ;; of this loop
    (process-pending-events)
    
    ;; Redisplay this cell each time the bar is incremented
    (unless (eq value previous-value)
      (setf (nth column-num values) value)
      (invalidate-cell row column row-num column-num))))

;;; This method allows the user to increment or decrement a bar-chart
;;; bar by pressing the plus or minus key while the keyboard focus
;;; is on a bar.  vk-add and vk-subtract are for the keys on the
;;; numeric keypad, while vk-plus and vk-minus are the for the keys
;;; on the main part of the keyboard.
(defmethod cell-key-down ((grid-widget grid-widget)
                          (row-section grid-row-section)
                          (column-section grid-column-section)
                          (row employee-row)
                          (column bar-chart-column)
                          row-num column-num buttons key-code)
  (declare (ignore buttons))
  (let* ((increment (case key-code
                      ((#.vk-add #.vk-plus) 1)
                      ((#.vk-subtract #.vk-minus) -1))))
    (if* increment
            
            ;; Change the official value in the domain data,
            ;; which is in a list on the employee object.
       then (incf (nth column-num (bar-values (data-object row)))
                  increment)
            
            ;; Cause the cell to be redrawn, at which time draw-cell will
            ;; fetch the new domain value from the employee object, and
            ;; draw the bar accordingly.
            (invalidate-cell row column row-num column-num)
       else (call-next-method))))
    
  
;;; This method effectively removes the horizontal cell borders
;;; from the body of the bar-chart section by making them be the
;;; same color as the grid background.
(defmethod cell-horizontal-border-color ((row employee-row)
                                         (column bar-chart-column))
  t)

;;; This method does the same for the vertical borders in the
;;; header row of the bar chart.
(defmethod cell-vertical-border-color ((row employee-column-header-row)
                                       (column bar-chart-column))
  t)

;;; These two methods change the colors used for the bar-chart column
;;; when a row is selected.  This first method finds a color that is
;;; a lighter version of the usual background color, and cached it
;;; so that it doesn't cons a new RGB object every time a cell is drawn.
(let* ((cached-color nil))
  (defmethod cell-selected-background-color ((row employee-row)
                                             (column bar-chart-column))
    (or cached-color
        (let* ((color (system-dialog-background-color)))
          (setq cached-color
                (make-rgb :red (min 255 (+ (rgb-red color) 16))
                          :green (min 255 (+ (rgb-green color) 16))
                          :blue (min 255 (+ (rgb-blue color) 16))))))))

(defmethod cell-selected-foreground-color ((row employee-row)
                                           (column bar-chart-column))
  (system-foreground-color))

;;; This method shows a message in the IDE status-bar each time
;;; the keyboard focus moves to a different cell in the worksheet area.
(defmethod set-focus-cell :after ((grid-widget grid-widget)
                                  (row-section grid-row-section)
                                  (column-section grid-column-section)
                                  (row employee-row)
                                  (column worksheet-column)
                                  &optional row-index column-index)
  (declare (ignore row-index column-index))
  (window-message (window grid-widget)
      "Focus is now on the ~a cell for ~a."
    (name column)(name (data-object row))))

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
;;
(defun run-grid-test-example ()
  ;; An example creation of a employee grid.
  (let* ((dialog (make-window :employee-chart
                   :class 'dialog 
                   :owner (development-main-window cg::*system*)
                   :title "Employee Information"
                   :font (make-font-ex nil :arial 13 '(:bold))
                   :resizable t
                   :exterior (make-box 337 160 987 550)))
         (status-bar (add-common-status-bar dialog 
                                            ;; :font (make-font-ex nil :arial 12)
                                            ))
         (grid-widget
          (make-instance 'employee-grid-widget
            :name :employee-grid
            :border-width 3
            :left 0 :top 72
            :width (interior-width dialog)
            :height (- (top status-bar) 72)
            :right-attachment :right
            :bottom-attachment :bottom
            :state :shrunk))
         (static-text-widget
          (make-instance 'static-text
            :name :description
            :left 10 :top 8
            :width (- (interior-width dialog) 20)
            :height 60
            :value (format nil "Here's a sample grid-widget.  Try ~
mousing various cells, dragging header cells and borders, and ~
moving around with the arrow keys.  Control-left-click to see ~
names of the clicked grid sections in the IDE status bar.")))
         grid-top-window)
    
    ;; Add the bar columns at runtime
    (add-column (column-section grid-widget :bar-chart)
                (make-instance 'bar-total-column
                  :name :total
                  :size 44
                  :selectable nil
                  :header-title "Total"
                  :header-font (make-font-ex nil :arial 13 '(:bold))
                  :proportional nil))
    (add-column (column-section grid-widget :bar-chart)
                (make-instance 'bar-chart-column
                  :name :bars
                  :size 36
                  :section-count 12
                  :border-dashing :dot
                  :selectable nil
                  :proportional nil)
                1)
    
    ;; Put the grid widget onto the dialog and show it.
    (add-component grid-widget dialog)
    (add-component static-text-widget dialog)
    (setq grid-top-window (window grid-widget))
    (expand-window grid-top-window)
    (update-window grid-top-window)
    (set-focus-component grid-widget)
    dialog))

;;; -------------------------------------------------------------------
;;; This next set of functions and methods demonstrate a fancy option.
;;; When the Department column is just to the right of the Full-Time-P
;;; column, and the employee is in the tech department but somehow
;;; is not full-time, then the vertical border between the two cells
;;; that indicate that are removed (by returning t from 
;;; cell-vertical-border-color) and a red question mark is shown
;;; where the border normally would be.

;;; So this section demonstrates how to merge two contiguous cells
;;; into a single larger cell.  Handling all the necessary refreshes
;;; is tricky when the merging is dependent on the changable values
;;; of the cells, but this section shows how to do it nevertheless.

;;; When changing the order of these columns, this can still leave part
;;; of the red question mark either drawn or not drawn when it should be;
;;; the reason is that normally this type of merging of cells would not
;;; be done in the same grid section where the order of the columns
;;; and rows can be changed by the user.  But this example program
;;; tries to demonstrate everything at once.

(defun part-time-techy-with-dept-to-right-of-full-time
    (employee-row full-time-p-column)
  
  ;; This function returns true if the column just to the right
  ;; of this full-time-p column is the department column, and if the
  ;; row's employee is in the tech department but is not full time.
  (let* ((employee (data-object employee-row))
         (columns-in-section (subsections (parent full-time-p-column)))
         columns-to-the-right)
    (and (eq (department employee) :tech)
         (not (full-time-p employee))
         (setq columns-to-the-right
               (rest (member full-time-p-column columns-in-section
                             :test #'eq)))
         (eq (name (first columns-to-the-right)) :department))))

(defmethod cell-vertical-border-color ((row employee-row)
                                       (column full-time-p-column))
  (if* (part-time-techy-with-dept-to-right-of-full-time row column)
     then t
     else (call-next-method)))

(defmethod draw-cell :after ((row employee-row)(column full-time-p-column)
                             row-num column-num cell-box stream)
  (declare (ignore row-num column-num))
  (when (part-time-techy-with-dept-to-right-of-full-time row column)
    (draw-question-mark-at-border stream cell-box)))

(defun draw-question-mark-at-border (stream cell-box)
  (with-boxes (box-at-border)
    (nmake-box box-at-border
      (- (box-right cell-box) 20)(box-top cell-box)
      (+ (box-right cell-box) 20)(box-bottom cell-box))
    (with-foreground-color (stream red)
      (with-font (stream (make-font-ex nil "Arial" 24 '(:bold)))
        (draw-string-in-box stream "?" nil nil box-at-border
                            :center :center)))))

;;; This last method is needed so that if a redisplay is done that
;;; includes the department column but not the full-time-p column,
;;; then the part of the question mark in the department column will
;;; still get drawn.  In other words, if a drawn thing straddles
;;; two columns, then the draw-cell methods for BOTH of those columns
;;; needs to draw that thing, or else it will not always appear fully
;;; during scrolling or uncovering parts of the grid.
(defmethod draw-cell :after ((row employee-row)(column department-column)
                             row-num column-num cell-box stream)
  (declare (ignore row-num column-num))
  (let* ((columns (subsections (parent column)))
         (column-index (position column columns :test #'eq))
         (other-column (and column-index
                            (plusp column-index)
                            (nth (1- column-index) columns))))
    (when (and other-column
               (eq (name other-column) :full-time-p)
               (part-time-techy-with-dept-to-right-of-full-time
                row other-column))
      (with-boxes (other-cell-box)
        (cell-box (parent row)(parent other-column)
                  row other-column 0 0 nil other-cell-box)
        (with-clipping-box (stream cell-box)
          (draw-question-mark-at-border stream other-cell-box))))))

(defmethod write-cell-value :after ((row employee-row)
                                    (column full-time-p-column)
                                    row-num column-num value)
  
  ;; When the user toggles the full-time-p state of an employee
  ;; in the tech department, this method ensures that both the
  ;; full-time-p and the department cells are redrawn to either
  ;; remove or draw the red question mark that appears for
  ;; part-time techies.
  (declare (ignore row-num column-num value))
  (let* ((employee (data-object row)))
    (when (eq (department employee) :tech)
      
      ;; We need to invalidate the very cell that the user changed
      ;; because check-box cells are normally optimized to redraw
      ;; only the check gadget.  Also pass the flag to invalidate
      ;; the border as well, since this will also go away or appear.
      (invalidate-cell row column 0 0 nil nil t)
      
      ;; Invalidate the department cell even though our special
      ;; drawing code for the full-time-p cell would draw the 
      ;; whole question mark, because if the department column
      ;; is not in the clipping region then the full-time-p draw-cell
      ;; method won't be able to draw the part of the question mark
      ;; that's in that cell.
      (invalidate-cell row (find-sibling :department column)))))
  
(defmethod write-cell-value :around ((row employee-row)
                                     (column department-column)
                                     row-num column-num value)
  
  ;; When the user changes the department of an employee either
  ;; from :tech or to :tech, and the employee is part time, then
  ;; this method causes the full-time-p cell for that employee
  ;; to be redrawn, in order to update whether the red question
  ;; mark and cell border are drawn above.
  (declare (ignore row-num column-num))
  (let* ((employee (data-object row))
         (old-department (department employee)))
    (prog1 (call-next-method)
      (when (and (not (eq value old-department))
                 (not (full-time-p employee))
                 (or (eq value :tech)
                     (eq old-department :tech)))
        (invalidate-cell row column)
        (invalidate-cell row (find-sibling :full-time-p column)
                         0 0 nil nil t)))))
  
(defmethod cell-click :before ((grid-widget employee-grid-widget)
                               buttons
                               column-section column-section-border-p
                               column column-num column-border-p x
                               row-section row-section-border-p
                               row row-num row-border-p y
                               &optional trigger-key)
  (declare (ignore trigger-key))
  
  ;; This shows information about each click on the grid-widget
  ;; when the IDE is present.
  
  ;; Called when the user clicks a cell on the grid.
  ;; X and Y are the clicked position within the cell.
  ;; The border-p arguments are non-NIL if the click was within
  ;; *grid-border-mouse-slack* of the right or bottom border.
  ;; Unless using a built-in widget column-type, you must supply the
  ;; click functionality yourself by modifying this method.
  (grid-mouse-message grid-widget buttons
                      column-section column-section-border-p
                      column column-num column-border-p x
                      row-section row-section-border-p
                      row row-num row-border-p y))

(defmethod grid-mouse-message (grid-widget
                               buttons column-section
                               column-section-border-p
                               column column-num column-border-p x
                               row-section row-section-border-p
                               row row-num row-border-p y)
  (declare (ignore buttons))
  
  ;; A default method to print information during debugging.
  ;; Don't do a short-global-status-message here, to avoid
  ;; consing up this long string in a standalone app.
  (window-message (parent grid-widget)
      "Column  ~s~a  ~s~a~a     Row  ~s~a  ~s~a~a     At ~a,~a"
    (if column-section (name column-section) "")
    (if column-section-border-p " (border)" "")
    (if column (name column) "")
    (if (and column-num (plusp column-num))
        (format nil " ~a" column-num)
      "")
    (if column-border-p " (border)" "")
    (if row-section (name row-section) "")
    (if row-section-border-p " (border)" "")
    (if row (name row) "")
    (if (and row-num (plusp row-num))
        (format nil " ~a" row-num)
      "")
    (if row-border-p " (border)" "")
    x y))

;;; -------------------------------------------------------------------

#+run-example
(run-grid-test-example)
 
