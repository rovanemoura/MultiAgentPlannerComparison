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

;; An example of writing cell-widget methods to use the built-in
;; grid cell widgets in arbitrary cells (rather than using the
;; grid-column mixins such as editable-text-column-mixin along with
;; widget-row-mixin ).

(in-package :cg-user)

(defparameter *my-values* nil)

;;; Define row and column subclasses that allow the user to
;;; stretch them, and which center-justify their strings, and
;;; which have a fairly large margin.

(defclass my-row (column-sizing-row-mixin grid-row)())

(defclass my-column (row-sizing-column-mixin grid-column)())

(defmethod cell-horizontal-justification ((row my-row)(column my-column))
  :center)

(defmethod cell-vertical-justification ((row my-row)(column my-column))
  :center)

(defmethod cell-horizontal-padding ((row my-row)(column my-column))
  8)

(defmethod cell-vertical-padding ((row my-row)(column my-column))
  6)

;;; For this simple example, we save the cell data into
;;; a nested plist.

(defmethod read-cell-value ((row my-row)(column my-column)
                            row-number column-number)
  (declare (ignore row-number column-number))
  (getf (getf *my-values* (name row))(name column)))

(defmethod write-cell-value ((row my-row)(column my-column)
                             row-number column-number value)
  (declare (ignore row-number column-number))
  (setf (getf (getf *my-values* (name row))(name column)) value))

;;; Here is the main cell-widget method that returns a
;;; a different kind of built-in widget for each cell, based
;;; on the name of the row and column.  More typically you might
;;; specialize multiple methods on row and column subclasses
;;; that you have defined.  When returning a type of widget
;;; from a cell-widget method, you must return a particular set
;;; of multiple values that are different for each type of widget.

(defmethod cell-widget ((row my-row)(column my-column)
                        row-number column-number)
  (declare (ignore row-number column-number))
  (case (name row)
    (:row1 (case (name column)
             (:column1
              (values
               
               ;; This first return value says to use a static-text
               ;; widget in the cell that's in the row that's named
               ;; :row1 and in the column that's named :column1.
               :static-text
               
               ;; This second value says to always display this
               ;; particular string in the cell.
               #.(format nil "This is a static text cell.  ~
                              It simply displays this text.")
               ))
             (:column2
              (values
               
               ;; Make the next cell be another static-text.
               :static-text
               
               ;; But this time return nil for the string to display,
               ;; to defer to whatever the read-cell-value method
               ;; for the cell returns.
               nil
               ))
             (:column3
              (values
               
               ;; This first return value says that this particular
               ;; cell should be a button cell.
               :button
               
               ;; The second return value for a button cell
               ;; is the function to run when the button is pressed.
               ;; It could be the name of a function, or function
               ;; object such as the one returned by this lambda form.
               ;; In either case, it should have this particular
               ;; parameter list.  This corresponds to the
               ;; button-function property of a
               ;; static-text-and-button-column-mixin grid-column .
               (lambda (grid data-object row column row-num col-num)
                 (declare (ignore data-object grid row-num col-num))
                 (let* ((middle-column (subsection (parent column)
                                                   :column2)))
                   (write-cell-value row middle-column
                                     0 0 (format nil "~:(~r~)"
                                           (random 1000)))
                   (invalidate-cell row middle-column)))
               
               ;; The third return value for a button cell
               ;; indicates whether the button fills the cell,
               ;; rather than being a small button beside the text.
               ;; See the button-fills-cell grid column property.
               t
               
               ;; Here we return a static string to display on
               ;; the button.  Alternately it could be a pixmap,
               ;; or else nil to defer to whatever the read-cell-value
               ;; method for the cell returns.
               "I write a random number into the cell to my left."
               
               ;; The background color of the button.  Nil defers
               ;; to whatever the cell-background-color method for
               ;; the cell returns, but you could return an RGB
               ;; color object instead.
               nil
               ))
             (:column4
              (values
               :button
               
               ;; This button makes its row and column larger.
               (lambda (grid data-object row column row-num col-num)
                 (declare (ignore data-object grid row-num col-num))
                 (incf (section-size row) 12)
                 (incf (section-size column) 12))
               
               t ;; Fill the cell.
               
               ;; This button cell displays the pixmap that's
               ;; defined below.
               (find-pixmap :creature)
               nil ;; cell background color
               t ;; the pixmap should stretch
               ))
             ))
    (:row2 (case (name column)
             (:column1
              (values
               
               ;; This cell is a check-box widget.  It will be
               ;; checked if the cell's read-cell-value method
               ;; returns non-nil.  Checking the widget will call
               ;; the cell's write-cell-value method to let you
               ;; store the new value somewhere.
               :check-box
               
               ;; Display this static label in the widget.
               ;; You MUST return a non-nil value here.
               "Click My Gadget or Double-Click the Cell"
               
               ;; Whether the user must click on the gadget, or anywhere
               ;; in the grid cell.  See click-must-be-on-gadget .
               t
               ))
             (:column2
              (values
               
               ;; This cell displays a lamp widget.
               :lamp
               
               ;; For a lamp cell, return a single data value, which
               ;; is an actual lamp widget.  This lamp widget doesn't
               ;; need to be on a dialog by itself, but will act just
               ;; like a standlone lamp inside this grid cell.
               ;; Let's use load-time-value here to avoid creating
               ;; a lamp widget every time this grid cell is drawn,
               ;; since this code is run every time.  And let's show
               ;; a fancy array of LEDs in this lamp cell, with
               ;; a separate on-color for each LED even.
               (load-time-value
                (make-instance 'lamp
                  :value #2a((nil t nil)
                             (t nil :blinking)
                             (:blinking t t))
                  :on-color #2a((#.green #.red #.blue)
                                (#.green #.red #.green)
                                (#.yellow #.yellow #.yellow))
                  :row-labels '("One" "Two" "Three")
                  :column-labels '("A" "B" "C")
                  :column-label-orientation :horizontal
                  :lamp-shape :rectangle
                  :lamp-height 14
                  :lamp-width 10
                  :row-label-margin 10
                  :column-label-margin 6
                  :column-label-offset 1
                  :inner-horizontal-margin 8
                  :center-all t))
               
               ))
             (:column3
              (values
               
               ;; A combo-box cell.  You must also supply a
               ;; read-cell-value method for the cell that returns
               ;; the current value to display, and a write-cell-value
               ;; method that stores a value that the user selects.
               :combo-box
               
               ;; The choices to list in the drop-down box.
               ;; You MUST return a non-null list here.
               ;; This corresponds to the range-reader property
               ;; of a combo-box-column-mixin grid-column .
               '(:one :two :three)
               
               nil     ;; The user can't type a value in.
               nil     ;; Use a pop-up-menu for the drop-down list.
               nil     ;; click-must-be-on-gadget (MUST be supplied)
               :click  ;; edit-start-trigger (MUST be supplied)
               ))
             (:column4
              (values
               
               ;; Another combo-box cell.
               :combo-box
               '(:four :five :six :seven)
               
               t ;; The user CAN type a value into this one.
               t ;; Use a "real" combo-box for the drop-down list.
               t ;; click-must-be-on-gadget (MUST be supplied)
               :begin-typing ;; edit-start-trigger (MUST be supplied)
               :enter-or-arrow-keys ;; edit-end-trigger
               ))))
    
    ;; This final row contains three editable-text cells.  They
    ;; differ in the way that the user can begin and end the
    ;; editing of the string inside.  You must also supply
    ;; read-cell-value and write-cell-value methods to handle
    ;; the storage of the data somewhere in your application.
    (:row3 (case (name column)
             (:column1
              (values
               :pixmap
               '((:one :default-opened-with-mask)(:two :key-special)
                 (:three :bug)(:four :creature))
               t ;; stretching
               t ;; user-modifiable
               ))
             (:column2
              (values
               :editable-text
               :get-focus           ;; edit-start-trigger
               :enter-or-arrow-keys ;; edit-end-trigger
               ))
             (:column3
              (values
               :editable-text
               :begin-typing        ;; edit-start-trigger
               :enter-or-arrow-keys ;; edit-end-trigger
               ))
             (:column4
              (values
               :button
               (lambda (grid data-object row column row-num col-num)
                 (declare (ignore data-object grid row-num col-num))
                 (write-cell-value
                  row column 0 0
                  (format nil "I like the number ~(~r~)."
                    (random 10)))
                 (invalidate-cell row column))
               
               nil ;; This button does NOT fill the grid cell.
               ))
             ))))
            
(defun run-cell-widgets-example ()
  (setq *my-values*
        (list
         :row1 (list :column2 "The button to the right changes me.")
         :row2 (list :column3 "Click anywhere to show my choices."
                     :column4 "Click my arrow or start typing into me.")
         :row3 (list :column1 :one
                     :column2 "Click me or tab the focus to me to edit my text."
                     :column3 "Just start typing into me."
                     :column4
                     "Click the button or double-click the cell.")))
  (let* ((width 650)
         (height 450)
         (grid (make-instance 'grid-widget
                 :left 0 :top 0 :width width :height height
                 :right-attachment :right
                 :bottom-attachment :bottom
                 :column-sections
                 (list
                  (make-instance 'grid-column-section
                    :name :main
                    :proportional t
                    :scrollbars nil
                    :resizable nil
                    :subsections
                    (mapcar (lambda (name)
                              (make-instance 'my-column
                                :name name
                                :proportional t))
                      '(:column1 :column2 :column3 :column4))))
                 :row-sections
                 (list
                  (make-instance 'grid-row-section
                    :name :main
                    :proportional t
                    :scrollbars nil
                    :resizable nil
                    :subsections
                    (mapcar (lambda (info)
                              (make-instance 'my-row
                                :name (first info)
                                :size (second info)
                                :proportional t))
                      '((:row1 100)(:row2 80)(:row3 100))))))))
    (make-window :cell-widget-test
      :class 'dialog
      :dialog-items (list grid)
      :title "Using a cell-widget method"
      :interior (make-box-relative 100 50 width height))))



;;; An example pixmap to use for the upper-right cell button.
(cache-pixmap
  (make-instance 'pixmap
    :name :creature
    :bits-per-pixel 8
    :invert-p t
    :colors (vector black (make-rgb :red 59 :green 49 :blue 194)
                    (make-rgb :red 58 :green 53 :blue 193)
                    (make-rgb :red 56 :green 58 :blue 192)
                    (make-rgb :red 55 :green 62 :blue 191)
                    (make-rgb :red 54 :green 67 :blue 190)
                    (make-rgb :red 53 :green 72 :blue 189)
                    (make-rgb :red 51 :green 76 :blue 188)
                    (make-rgb :red 50 :green 81 :blue 187)
                    (make-rgb :red 49 :green 85 :blue 186)
                    (make-rgb :red 48 :green 90 :blue 185)
                    (make-rgb :red 47 :green 94 :blue 184)
                    (make-rgb :red 45 :green 99 :blue 182)
                    (make-rgb :red 44 :green 104 :blue 181)
                    (make-rgb :red 43 :green 108 :blue 180)
                    (make-rgb :red 42 :green 113 :blue 179)
                    (make-rgb :red 41 :green 117 :blue 178)
                    (make-rgb :red 39 :green 122 :blue 177)
                    (make-rgb :red 38 :green 127 :blue 176)
                    (make-rgb :red 37 :green 131 :blue 175)
                    (make-rgb :red 36 :green 136 :blue 174)
                    (make-rgb :red 34 :green 140 :blue 173)
                    (make-rgb :red 33 :green 145 :blue 172)
                    (make-rgb :red 32 :green 150 :blue 171)
                    (make-rgb :red 31 :green 154 :blue 170)
                    (make-rgb :red 30 :green 159 :blue 169)
                    (make-rgb :red 28 :green 163 :blue 168)
                    (make-rgb :red 27 :green 168 :blue 167)
                    (make-rgb :red 26 :green 172 :blue 166)
                    (make-rgb :red 25 :green 177 :blue 165)
                    (make-rgb :red 23 :green 182 :blue 164)
                    (make-rgb :red 22 :green 186 :blue 163)
                    (make-rgb :red 21 :green 191 :blue 162)
                    (make-rgb :red 20 :green 195 :blue 161)
                    (make-rgb :red 19 :green 200 :blue 160)
                    (make-rgb :red 17 :green 205 :blue 158)
                    (make-rgb :red 16 :green 209 :blue 157)
                    (make-rgb :red 15 :green 214 :blue 156)
                    (make-rgb :red 14 :green 218 :blue 155)
                    (make-rgb :red 13 :green 223 :blue 154)
                    (make-rgb :red 11 :green 227 :blue 153)
                    (make-rgb :red 10 :green 232 :blue 152)
                    (make-rgb :red 9 :green 237 :blue 151)
                    (make-rgb :red 8 :green 241 :blue 150)
                    (make-rgb :red 6 :green 246 :blue 149)
                    (make-rgb :red 5 :green 250 :blue 148)
                    (make-rgb :red 4 :green 255 :blue 147)
                    (make-rgb :red 9 :green 250 :blue 146)
                    (make-rgb :red 13 :green 246 :blue 144)
                    (make-rgb :red 18 :green 241 :blue 143)
                    (make-rgb :red 23 :green 237 :blue 141)
                    (make-rgb :red 27 :green 232 :blue 140)
                    (make-rgb :red 32 :green 227 :blue 138)
                    (make-rgb :red 37 :green 223 :blue 137)
                    (make-rgb :red 41 :green 218 :blue 135)
                    (make-rgb :red 46 :green 214 :blue 134)
                    (make-rgb :red 51 :green 209 :blue 132)
                    (make-rgb :red 55 :green 205 :blue 131)
                    (make-rgb :red 60 :green 200 :blue 130)
                    (make-rgb :red 65 :green 195 :blue 128)
                    (make-rgb :red 69 :green 191 :blue 127)
                    (make-rgb :red 74 :green 186 :blue 125)
                    (make-rgb :red 79 :green 182 :blue 124)
                    (make-rgb :red 83 :green 177 :blue 122)
                    (make-rgb :red 88 :green 172 :blue 121)
                    (make-rgb :red 93 :green 168 :blue 119)
                    (make-rgb :red 97 :green 163 :blue 118)
                    (make-rgb :red 102 :green 159 :blue 116)
                    (make-rgb :red 107 :green 154 :blue 115)
                    (make-rgb :red 112 :green 150 :blue 114)
                    (make-rgb :red 116 :green 145 :blue 112)
                    (make-rgb :red 121 :green 140 :blue 111)
                    (make-rgb :red 126 :green 136 :blue 109)
                    (make-rgb :red 130 :green 131 :blue 108)
                    (make-rgb :red 135 :green 127 :blue 106)
                    (make-rgb :red 140 :green 122 :blue 105)
                    (make-rgb :red 144 :green 117 :blue 103)
                    (make-rgb :red 149 :green 113 :blue 102)
                    (make-rgb :red 154 :green 108 :blue 100)
                    (make-rgb :red 158 :green 104 :blue 99)
                    (make-rgb :red 163 :green 99 :blue 97)
                    (make-rgb :red 168 :green 94 :blue 96)
                    (make-rgb :red 172 :green 90 :blue 95)
                    (make-rgb :red 177 :green 85 :blue 93)
                    (make-rgb :red 182 :green 81 :blue 92)
                    (make-rgb :red 186 :green 76 :blue 90)
                    (make-rgb :red 191 :green 72 :blue 89)
                    (make-rgb :red 196 :green 67 :blue 87)
                    (make-rgb :red 200 :green 62 :blue 86)
                    (make-rgb :red 205 :green 58 :blue 84)
                    (make-rgb :red 210 :green 53 :blue 83)
                    (make-rgb :red 214 :green 49 :blue 81)
                    (make-rgb :red 219 :green 44 :blue 80)
                    (make-rgb :red 220 :green 49 :blue 80)
                    (make-rgb :red 221 :green 53 :blue 80)
                    (make-rgb :red 221 :green 58 :blue 80)
                    (make-rgb :red 222 :green 62 :blue 80)
                    (make-rgb :red 223 :green 67 :blue 80)
                    (make-rgb :red 224 :green 72 :blue 80)
                    (make-rgb :red 224 :green 76 :blue 80)
                    (make-rgb :red 225 :green 81 :blue 80)
                    (make-rgb :red 226 :green 85 :blue 80)
                    (make-rgb :red 227 :green 90 :blue 80)
                    (make-rgb :red 228 :green 94 :blue 80)
                    (make-rgb :red 228 :green 99 :blue 80)
                    (make-rgb :red 229 :green 104 :blue 80)
                    (make-rgb :red 230 :green 108 :blue 80)
                    (make-rgb :red 231 :green 113 :blue 80)
                    (make-rgb :red 232 :green 117 :blue 80)
                    (make-rgb :red 232 :green 122 :blue 80)
                    (make-rgb :red 233 :green 127 :blue 80)
                    (make-rgb :red 234 :green 131 :blue 80)
                    (make-rgb :red 235 :green 136 :blue 80)
                    (make-rgb :red 235 :green 140 :blue 80)
                    (make-rgb :red 236 :green 145 :blue 80)
                    (make-rgb :red 237 :green 150 :blue 80)
                    (make-rgb :red 238 :green 154 :blue 80)
                    (make-rgb :red 239 :green 159 :blue 80)
                    (make-rgb :red 239 :green 163 :blue 80)
                    (make-rgb :red 240 :green 168 :blue 80)
                    (make-rgb :red 241 :green 172 :blue 80)
                    (make-rgb :red 242 :green 177 :blue 80)
                    (make-rgb :red 242 :green 182 :blue 80)
                    (make-rgb :red 243 :green 186 :blue 80)
                    (make-rgb :red 244 :green 191 :blue 80)
                    (make-rgb :red 245 :green 195 :blue 80)
                    (make-rgb :red 246 :green 200 :blue 80)
                    (make-rgb :red 246 :green 205 :blue 80)
                    (make-rgb :red 247 :green 209 :blue 80)
                    (make-rgb :red 248 :green 214 :blue 80)
                    (make-rgb :red 249 :green 218 :blue 80)
                    (make-rgb :red 250 :green 223 :blue 80)
                    (make-rgb :red 250 :green 227 :blue 80)
                    (make-rgb :red 251 :green 232 :blue 80)
                    (make-rgb :red 252 :green 237 :blue 80)
                    (make-rgb :red 253 :green 241 :blue 80)
                    (make-rgb :red 253 :green 246 :blue 80)
                    (make-rgb :red 254 :green 250 :blue 80)
                    (make-rgb :red 255 :green 255 :blue 80)
                    (make-rgb :red 251 :green 254 :blue 80)
                    (make-rgb :red 247 :green 252 :blue 80)
                    (make-rgb :red 242 :green 251 :blue 80)
                    (make-rgb :red 238 :green 249 :blue 80)
                    (make-rgb :red 234 :green 248 :blue 80)
                    (make-rgb :red 230 :green 247 :blue 80)
                    (make-rgb :red 225 :green 245 :blue 80)
                    (make-rgb :red 221 :green 244 :blue 80)
                    (make-rgb :red 217 :green 242 :blue 80)
                    (make-rgb :red 213 :green 241 :blue 80)
                    (make-rgb :red 208 :green 240 :blue 80)
                    (make-rgb :red 204 :green 238 :blue 80)
                    (make-rgb :red 200 :green 237 :blue 80)
                    (make-rgb :red 196 :green 236 :blue 80)
                    (make-rgb :red 191 :green 234 :blue 80)
                    (make-rgb :red 187 :green 233 :blue 80)
                    (make-rgb :red 183 :green 231 :blue 80)
                    (make-rgb :red 179 :green 230 :blue 80)
                    (make-rgb :red 174 :green 229 :blue 80)
                    (make-rgb :red 170 :green 227 :blue 80)
                    (make-rgb :red 166 :green 226 :blue 80)
                    (make-rgb :red 162 :green 224 :blue 80)
                    (make-rgb :red 158 :green 223 :blue 80)
                    (make-rgb :red 153 :green 222 :blue 80)
                    (make-rgb :red 149 :green 220 :blue 80)
                    (make-rgb :red 145 :green 219 :blue 80)
                    (make-rgb :red 141 :green 217 :blue 80)
                    (make-rgb :red 136 :green 216 :blue 80)
                    (make-rgb :red 132 :green 215 :blue 80)
                    (make-rgb :red 128 :green 213 :blue 80)
                    (make-rgb :red 124 :green 212 :blue 80)
                    (make-rgb :red 119 :green 210 :blue 80)
                    (make-rgb :red 115 :green 209 :blue 80)
                    (make-rgb :red 111 :green 208 :blue 80)
                    (make-rgb :red 107 :green 206 :blue 80)
                    (make-rgb :red 102 :green 205 :blue 80)
                    (make-rgb :red 98 :green 204 :blue 80)
                    (make-rgb :red 94 :green 202 :blue 80)
                    (make-rgb :red 90 :green 201 :blue 80)
                    (make-rgb :red 85 :green 199 :blue 80)
                    (make-rgb :red 81 :green 198 :blue 80)
                    (make-rgb :red 77 :green 197 :blue 80)
                    (make-rgb :red 73 :green 195 :blue 80)
                    (make-rgb :red 68 :green 194 :blue 80)
                    (make-rgb :red 64 :green 192 :blue 80)
                    (make-rgb :red 60 :green 191 :blue 80)
                    (make-rgb :red 60 :green 188 :blue 82)
                    (make-rgb :red 60 :green 185 :blue 85)
                    (make-rgb :red 60 :green 181 :blue 88)
                    (make-rgb :red 60 :green 178 :blue 90)
                    (make-rgb :red 60 :green 175 :blue 92)
                    (make-rgb :red 60 :green 172 :blue 95)
                    (make-rgb :red 60 :green 169 :blue 98)
                    (make-rgb :red 60 :green 165 :blue 100)
                    (make-rgb :red 60 :green 162 :blue 102)
                    (make-rgb :red 60 :green 159 :blue 105)
                    (make-rgb :red 60 :green 156 :blue 108)
                    (make-rgb :red 60 :green 153 :blue 110)
                    (make-rgb :red 60 :green 149 :blue 112)
                    (make-rgb :red 60 :green 146 :blue 115)
                    (make-rgb :red 60 :green 143 :blue 118)
                    (make-rgb :red 60 :green 140 :blue 120)
                    (make-rgb :red 60 :green 137 :blue 122)
                    (make-rgb :red 60 :green 133 :blue 125)
                    (make-rgb :red 60 :green 130 :blue 128)
                    (make-rgb :red 60 :green 127 :blue 130)
                    (make-rgb :red 60 :green 124 :blue 132)
                    (make-rgb :red 60 :green 121 :blue 135)
                    (make-rgb :red 60 :green 118 :blue 138)
                    (make-rgb :red 60 :green 114 :blue 140)
                    (make-rgb :red 60 :green 111 :blue 142)
                    (make-rgb :red 60 :green 108 :blue 145)
                    (make-rgb :red 60 :green 105 :blue 148)
                    (make-rgb :red 60 :green 102 :blue 150)
                    (make-rgb :red 60 :green 98 :blue 152)
                    (make-rgb :red 60 :green 95 :blue 155)
                    (make-rgb :red 60 :green 92 :blue 158)
                    (make-rgb :red 60 :green 89 :blue 160)
                    (make-rgb :red 60 :green 86 :blue 162)
                    (make-rgb :red 60 :green 82 :blue 165)
                    (make-rgb :red 60 :green 79 :blue 168)
                    (make-rgb :red 60 :green 76 :blue 170)
                    (make-rgb :red 60 :green 73 :blue 172)
                    (make-rgb :red 60 :green 70 :blue 175)
                    (make-rgb :red 60 :green 66 :blue 178)
                    (make-rgb :red 60 :green 63 :blue 180)
                    (make-rgb :red 60 :green 60 :blue 182)
                    (make-rgb :red 60 :green 57 :blue 185)
                    (make-rgb :red 60 :green 54 :blue 188)
                    (make-rgb :red 60 :green 50 :blue 190)
                    (make-rgb :red 60 :green 47 :blue 192)
                    (make-rgb :red 174 :green 208 :blue 228))
    :contents
    '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (41 40 37 0 0 32 123 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 123 32 0 0 37 40 41)
      (40 39 40 38 124 124 124 123 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 123 124 124 124 38 40 39 40)
      (38 39 38 37 124 124 124 123 123 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 123 123 124 124 124 37 38 39 38)
      (37 36 35 33 30 124 123 123 123 123 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 123 123 123 123 124 30 33 35 36 37)
      (33 32 31 30 27 24 123 123 123 124 124 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 124 124 123 123 123 24 27 30 31 32 33)
      (28 27 26 25 23 20 18 124 124 124 125 126 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 126 125 124 124 124 18 20 23 25 26 27 28)
      (22 22 21 20 19 17 15 14 125 125 126 127 128 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 128 127 126 125 125 14 15 17 19 20 21 22 22)
      (16 16 16 15 14 13 12 11 126 126 127 128 129 130 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 130 129 128 127 126 126 11 12 13 14 15 16 16 16)
      (10 10 10 10 9 9 8 8 127 127 128 129 130 131 132 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       132 131 130 129 128 127 127 8 8 9 9 10 10 10 10)
      (5 5 5 5 5 5 5 4 128 128 129 130 131 132 133 134 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 134
       133 132 131 130 129 128 128 4 5 5 5 5 5 5 5)
      (1 1 1 1 1 1 1 1 129 129 130 131 132 133 134 134 135 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 135
       134 134 133 132 131 130 129 129 1 1 1 1 1 1 1 1)
      (228 228 228 228 228 228 228 227 130 130 131 132 133 133 134 135
       135 136 136 136 136 0 213 213 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 213 213 0 136 136 136 136 135 135 134 133 133 132 131
       130 130 227 228 228 228 228 228 228 228)
      (225 225 225 225 225 225 225 225 224 131 132 133 133 134 134 135
       135 136 136 136 136 214 214 214 214 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 214 214 214 214 136 136 136 136 135 135 134 134 133
       133 132 131 224 225 225 225 225 225 225 225 225)
      (223 223 223 223 223 223 223 223 223 132 133 133 133 134 134 135
       135 136 136 136 136 214 214 214 214 214 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 214 214 214 214 214 136 136 136 136 135 135 134 134
       133 133 133 132 223 223 223 223 223 223 223 223 223)
      (222 222 222 222 222 222 222 222 222 133 133 133 133 133 134 134
       135 135 136 136 136 214 214 214 214 214 213 213 0 83 0 82 0 79
       77 75 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       75 77 79 0 82 0 83 0 213 213 214 214 214 214 214 136 136 136 135
       135 134 134 133 133 133 133 133 222 222 222 222 222 222 222 222
       222)
      (222 222 222 222 222 222 222 222 222 133 133 133 133 133 133 133
       134 134 135 136 136 213 213 213 213 213 213 212 212 84 83 82 81
       79 77 75 73 69 66 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       66 69 73 75 77 79 81 82 83 84 212 212 213 213 213 213 213 213
       136 136 135 134 134 133 133 133 133 133 133 133 222 222 222 222
       222 222 222 222 222)
      (222 222 222 223 223 223 222 222 222 132 132 132 132 132 132 132
       132 133 134 135 212 212 212 213 213 213 212 212 212 85 84 83 81
       79 77 75 72 69 66 62 57 53 49 46 43 40 37 35 34 34 34 34 34 35
       37 40 43 46 49 53 57 62 66 69 72 75 77 79 81 83 84 85 212 212
       212 213 213 213 212 212 212 135 134 133 132 132 132 132 132 132
       132 132 222 222 222 223 223 223 222 222 222)
      (223 223 223 223 223 223 223 223 223 223 131 131 131 131 131 131
       131 131 132 211 211 211 212 212 212 212 212 212 87 86 85 83 81
       79 77 74 71 68 64 61 57 53 49 45 42 40 37 35 34 34 34 34 34 35
       37 40 42 45 49 53 57 61 64 68 71 74 77 79 81 83 85 86 87 212 212
       212 212 212 212 211 211 211 132 131 131 131 131 131 131 131 131
       223 223 223 223 223 223 223 223 223 223)
      (225 225 225 225 224 224 224 224 224 224 129 129 129 129 129 129
       129 129 129 210 211 211 211 211 211 211 211 211 88 87 86 84 82
       79 77 74 71 67 63 59 56 52 48 45 42 39 37 35 34 34 34 34 34 35
       37 39 42 45 48 52 56 59 63 67 71 74 77 79 82 84 86 87 88 211 211
       211 211 211 211 211 211 210 129 129 129 129 129 129 129 129 129
       224 224 224 224 224 224 225 225 225 225)
      (227 227 227 226 226 225 225 225 225 127 127 127 127 127 127 127
       127 126 125 210 210 210 211 211 211 210 210 90 90 88 87 85 82 80
       77 74 70 66 62 58 54 51 47 44 41 39 37 35 34 34 34 34 34 35 37
       39 41 44 47 51 54 58 62 66 70 74 77 80 82 85 87 88 90 90 210 210
       211 211 211 210 210 210 125 126 127 127 127 127 127 127 127 127
       225 225 225 225 226 226 227 227 227)
      (229 229 228 228 227 227 226 226 226 125 125 125 125 125 125 125
       124 123 122 120 210 210 210 210 210 210 210 92 91 90 88 86 83 80
       77 73 69 65 61 57 53 49 46 43 40 38 36 35 34 33 33 33 34 35 36
       38 40 43 46 49 53 57 61 65 69 73 77 80 83 86 88 90 91 92 210 210
       210 210 210 210 210 120 122 123 124 125 125 125 125 125 125 125
       226 226 226 227 227 228 228 229 229)
      (0 0 0 229 229 228 228 227 227 122 123 123 123 123 123 123 122
       121 120 118 116 210 210 210 210 210 95 94 92 91 89 87 84 81 77
       73 69 64 60 56 52 48 44 41 39 37 35 34 33 32 32 32 33 34 35 37
       39 41 44 48 52 56 60 64 69 73 77 81 84 87 89 91 92 94 95 210 210
       210 210 210 116 118 120 121 122 123 123 123 123 123 123 122 227
       227 228 228 229 229 0 0 0)
      (1 1 1 1 0 0 229 228 119 120 121 121 121 121 121 121 120 119 118
       116 115 113 210 210 210 99 97 95 94 93 91 89 86 82 78 74 69 64
       59 55 50 46 43 40 37 35 34 33 32 31 31 31 32 33 34 35 37 40 43
       46 50 55 59 64 69 74 78 82 86 89 91 93 94 95 97 99 210 210 210
       113 115 116 118 119 120 121 121 121 121 121 121 120 119 228 229
       0 0 1 1 1 1)
      (2 3 3 2 2 2 1 118 118 119 119 120 120 120 120 119 118 117 116
       115 114 112 110 108 104 101 99 97 96 95 93 91 88 84 80 75 70 64
       59 53 49 45 41 38 36 34 32 31 31 30 30 30 31 31 32 34 36 38 41
       45 49 53 59 64 70 75 80 84 88 91 93 95 96 97 99 101 104 108 110
       112 114 115 116 117 118 119 120 120 120 120 119 119 118 118 1 2
       2 2 3 3 2)
      (4 4 4 4 4 4 4 118 118 118 118 119 119 119 118 118 117 116 115
       114 113 111 110 108 105 103 101 100 98 97 96 93 91 87 83 77 71
       65 58 52 47 43 39 36 34 32 31 30 29 29 28 29 29 30 31 32 34 36
       39 43 47 52 58 65 71 77 83 87 91 93 96 97 98 100 101 103 105 108
       110 111 113 114 115 116 117 118 118 119 119 119 118 118 118 118
       4 4 4 4 4 4 4)
      (6 6 6 6 6 6 6 117 117 117 117 118 118 118 117 117 116 115 114
       113 112 111 109 108 106 105 103 102 101 100 98 96 93 90 86 81 73
       65 57 50 45 41 37 34 32 30 29 28 27 27 26 27 27 28 29 30 32 34
       37 41 45 50 57 65 73 81 86 90 93 96 98 100 101 102 103 105 106
       108 109 111 112 113 114 115 116 117 117 118 118 118 117 117 117
       117 6 6 6 6 6 6 6)
      (7 7 7 7 7 7 7 116 116 116 117 117 117 117 117 116 115 114 113
       112 111 110 109 108 107 106 105 104 103 102 101 99 97 94 91 86
       77 66 55 48 42 38 35 32 30 28 27 26 25 24 24 24 25 26 27 28 30
       32 35 38 42 48 55 66 77 86 91 94 97 99 101 102 103 104 105 106
       107 108 109 110 111 112 113 114 115 116 117 117 117 117 117 116
       116 116 7 7 7 7 7 7 7)
      (8 8 8 8 8 7 7 116 116 116 116 116 116 116 116 115 114 113 112
       111 110 110 109 108 108 107 107 106 105 104 103 101 99 97 95 96
       90 61 51 44 39 35 32 30 28 26 25 24 23 22 22 22 23 24 25 26 28
       30 32 35 39 44 51 61 90 96 95 97 99 101 103 104 105 106 107 107
       108 108 109 110 110 111 112 113 114 115 116 116 116 116 116 116
       116 116 7 7 8 8 8 8 8)
      (8 8 8 8 8 8 117 117 117 117 116 116 116 116 115 115 114 112 111
       110 110 109 109 109 108 108 108 108 107 106 105 103 101 98 97
       100 52 52 45 40 36 32 30 28 26 24 23 22 21 20 20 20 21 22 23 24
       26 28 30 32 36 40 45 52 52 100 97 98 101 103 105 106 107 108 108
       108 108 109 109 109 110 110 111 112 114 115 115 116 116 116 116
       117 117 117 117 8 8 8 8 8 8)
      (8 8 8 8 8 8 118 118 118 117 117 116 116 116 115 114 113 112 111
       110 109 109 109 109 109 109 109 109 108 108 107 105 36 36 38 45
       43 42 39 36 32 29 27 25 23 22 21 20 19 19 18 19 19 20 21 22 23
       25 27 29 32 36 39 42 43 45 38 36 36 105 107 108 108 109 109 109
       109 109 109 109 109 110 111 112 113 114 115 116 116 116 117 117
       118 118 118 8 8 8 8 8 8)
      (8 8 8 8 8 8 120 119 119 118 118 117 116 116 115 114 113 112 111
       110 109 109 109 109 110 110 110 110 110 109 108 34 34 35 36 36
       37 36 34 31 28 26 24 22 21 20 19 18 18 17 17 17 18 18 19 20 21
       22 24 26 28 31 34 36 37 36 36 35 34 34 108 109 110 110 110 110
       110 109 109 109 109 110 111 112 113 114 115 116 116 117 118 118
       119 119 120 8 8 8 8 8 8)
      (8 8 8 8 8 8 122 121 120 120 119 118 117 116 115 114 113 112 111
       110 110 109 109 110 110 111 111 111 111 110 33 33 34 33 33 33 33
       31 29 27 25 23 21 20 19 18 17 17 16 16 16 16 16 17 17 18 19 20
       21 23 25 27 29 31 33 33 33 33 34 33 33 110 111 111 111 111 110
       110 109 109 110 110 111 112 113 114 115 116 117 118 119 120 120
       121 122 8 8 8 8 8 8)
      (8 8 8 8 8 127 125 123 122 121 120 119 118 117 116 115 114 113
       112 111 110 110 110 110 111 111 112 112 112 112 31 31 31 31 30
       30 29 27 25 23 22 20 19 18 17 16 16 15 15 14 14 14 15 15 16 16
       17 18 19 20 22 23 25 27 29 30 30 31 31 31 31 112 112 112 112 111
       111 110 110 110 110 111 112 113 114 115 116 117 118 119 120 121
       122 123 125 127 8 8 8 8 8)
      (8 8 8 8 131 129 127 125 124 122 121 120 119 118 117 116 115 114
       113 112 111 111 111 111 111 112 112 113 113 113 29 29 29 28 27
       26 25 23 21 20 19 18 16 15 15 14 14 13 13 12 12 12 13 13 14 14
       15 15 16 18 19 20 21 23 25 26 27 28 29 29 29 113 113 113 112 112
       111 111 111 111 111 112 113 114 115 116 117 118 119 120 121 122
       124 125 127 129 131 8 8 8 8)
      (140 140 138 136 133 131 129 127 125 124 122 121 120 119 118 117
       116 115 114 113 112 112 112 112 112 112 113 113 113 113 26 25 25
       25 24 22 21 19 18 17 16 15 14 13 12 12 11 11 10 10 10 10 10 11
       11 12 12 13 14 15 16 17 18 19 21 22 24 25 25 25 26 113 113 113
       113 112 112 112 112 112 112 113 114 115 116 117 118 119 120 121
       122 124 125 127 129 131 133 136 138 140 140)
      (140 140 139 137 135 133 131 129 127 125 124 122 121 121 120 119
       118 117 115 114 114 113 113 113 113 113 113 113 113 113 21 22 21
       21 20 19 17 16 15 14 13 12 11 10 10 9 9 8 8 8 8 8 8 8 9 9 10 10
       11 12 13 14 15 16 17 19 20 21 21 22 21 113 113 113 113 113 113
       113 113 113 114 114 115 117 118 119 120 121 121 122 124 125 127
       129 131 133 135 137 139 140 140)
      (141 141 140 138 137 135 133 131 129 127 125 124 123 122 121 120
       119 118 117 116 115 115 114 114 114 114 114 113 113 16 17 18 18
       17 16 15 14 13 12 12 11 10 9 8 7 7 6 6 6 5 5 5 6 6 6 7 7 8 9 10
       11 12 12 13 14 15 16 17 18 18 17 16 113 113 114 114 114 114 114
       115 115 116 117 118 119 120 121 122 123 124 125 127 129 131 133
       135 137 138 140 141 141)
      (142 142 141 140 138 137 135 133 131 129 127 126 125 124 122 121
       120 119 118 118 117 116 115 115 115 115 114 11 13 14 15 15 15 14
       13 12 11 11 10 9 8 7 6 5 5 4 4 4 3 3 3 3 3 4 4 4 5 5 6 7 8 9 10
       11 11 12 13 14 15 15 15 14 13 11 114 115 115 115 115 116 117 118
       118 119 120 121 122 124 125 126 127 129 131 133 135 137 138 140
       141 142 142)
      (143 143 142 141 140 138 137 135 133 131 129 128 126 125 124 122
       121 120 120 119 118 117 117 116 116 115 10 11 11 12 12 12 12 11
       10 9 9 8 8 7 6 5 4 3 2 2 2 2 1 1 1 1 1 2 2 2 2 3 4 5 6 7 8 8 9 9
       10 11 12 12 12 12 11 11 10 115 116 116 117 117 118 119 120 120
       121 122 124 125 126 128 129 131 133 135 137 138 140 141 142 143
       143)
      (144 144 143 142 141 140 138 137 135 133 131 129 128 126 125 123
       122 121 121 120 119 119 118 117 116 10 10 10 10 10 10 10 10 9 8
       7 6 6 5 5 4 3 2 1 0 0 0 0 0 229 229 229 0 0 0 0 0 1 2 3 4 5 5 6
       6 7 8 9 10 10 10 10 10 10 10 10 116 117 118 119 119 120 121 121
       122 123 125 126 128 129 131 133 135 137 138 140 141 142 143 144
       144)
      (145 145 144 143 142 141 140 138 136 135 133 131 129 127 125 124
       123 122 121 121 120 120 119 118 10 10 9 9 9 9 9 8 8 7 6 5 4 3 3
       3 2 1 0 229 229 228 228 228 228 228 228 228 228 228 228 228 229
       229 0 1 2 3 3 3 4 5 6 7 8 8 9 9 9 9 9 10 10 118 119 120 120 121
       121 122 123 124 125 127 129 131 133 135 136 138 140 141 142 143
       144 145 145)
      (146 145 145 144 143 142 141 139 138 136 134 132 130 128 126 125
       123 122 122 121 121 120 13 11 10 10 9 8 8 8 7 7 6 5 4 3 2 1 1 1
       0 229 229 228 227 227 227 227 227 227 227 227 227 227 227 227
       227 228 229 229 0 1 1 1 2 3 4 5 6 7 7 8 8 8 9 10 10 11 13 120
       121 121 122 122 123 125 126 128 130 132 134 136 138 139 141 142
       143 144 145 145 146)
      (146 146 145 145 144 143 141 140 139 137 135 133 131 129 127 125
       124 123 122 122 121 14 13 12 11 10 9 8 7 6 6 5 4 3 2 1 0 0 229
       229 228 228 227 227 226 226 226 226 226 226 226 226 226 226 226
       226 226 227 227 228 228 229 229 0 0 1 2 3 4 5 6 6 7 8 9 10 11 12
       13 14 121 122 122 123 124 125 127 129 131 133 135 137 139 140
       141 143 144 145 145 146 146)
      (146 146 146 145 144 143 142 141 139 138 136 134 132 130 128 126
       124 123 122 122 14 14 13 12 11 10 8 7 6 5 4 3 2 1 0 0 229 228
       228 227 227 226 226 226 226 225 225 225 225 225 226 225 225 225
       225 225 226 226 226 226 227 227 228 228 229 0 0 1 2 3 4 5 6 7 8
       10 11 12 13 14 14 122 122 123 124 126 128 130 132 134 136 138
       139 141 142 143 144 145 146 146 146)
      (68 68 68 67 66 65 142 141 140 138 136 135 133 131 128 126 124
       123 122 16 15 14 13 12 11 9 8 6 5 4 3 2 1 0 229 228 228 227 227
       226 226 225 225 225 225 225 225 225 225 225 225 225 225 225 225
       225 225 225 225 225 226 226 227 227 228 228 229 0 1 2 3 4 5 6 8
       9 11 12 13 14 15 16 122 123 124 126 128 131 133 135 136 138 140
       141 142 65 66 67 68 68 68)
      (68 68 67 67 65 64 62 141 140 138 136 135 134 34 31 28 26 24 20
       18 16 15 14 12 11 9 7 6 4 3 2 1 0 229 228 227 227 227 226 226
       225 225 224 224 225 225 225 225 225 225 225 225 225 225 225 225
       225 224 224 225 225 226 226 227 227 227 228 229 0 1 2 3 4 6 7 9
       11 12 14 15 16 18 20 24 26 28 31 34 134 135 136 138 140 141 62
       64 65 67 67 68 68)
      (68 67 67 66 65 63 61 59 55 51 47 43 38 34 31 29 26 24 21 19 17
       16 14 13 11 9 7 5 4 2 1 0 229 228 227 227 226 226 226 225 225
       224 224 224 224 225 225 225 225 225 225 225 225 225 225 225 224
       224 224 224 225 225 226 226 226 227 227 228 229 0 1 2 4 5 7 9 11
       13 14 16 17 19 21 24 26 29 31 34 38 43 47 51 55 59 61 63 65 66
       67 67 68)
      (67 67 66 65 64 62 60 58 55 51 47 43 39 35 32 29 27 24 22 20 18
       16 15 13 11 9 7 5 3 2 0 229 228 227 227 226 226 226 225 225 224
       224 224 224 224 225 225 225 225 225 225 225 225 225 225 225 224
       224 224 224 224 225 225 226 226 226 227 227 228 229 0 2 3 5 7 9
       11 13 15 16 18 20 22 24 27 29 32 35 39 43 47 51 55 58 60 62 64
       65 66 67 67)
      (66 66 65 64 63 61 59 57 54 51 47 43 40 36 33 30 27 25 23 21 19
       17 15 13 11 9 7 5 3 1 0 229 228 227 226 226 226 226 225 224 224
       224 224 224 225 225 226 226 225 225 225 225 225 226 226 225 225
       224 224 224 224 224 225 226 226 226 226 227 228 229 0 1 3 5 7 9
       11 13 15 17 19 21 23 25 27 30 33 36 40 43 47 51 54 57 59 61 63
       64 65 66 66)
      (65 65 64 63 62 60 58 56 53 50 47 44 40 37 34 31 28 26 24 22 20
       18 16 14 12 9 7 5 3 1 0 229 228 227 226 226 226 226 225 224 224
       224 224 225 225 226 226 226 226 225 225 225 226 226 226 226 225
       225 224 224 224 224 225 226 226 226 226 227 228 229 0 1 3 5 7 9
       12 14 16 18 20 22 24 26 28 31 34 37 40 44 47 50 53 56 58 60 62
       63 64 65 65)
      (64 63 63 62 60 59 57 55 52 50 47 44 41 37 34 32 29 27 25 23 21
       19 17 15 12 10 8 5 3 2 0 229 228 227 227 226 226 226 225 225 224
       224 224 225 226 226 226 226 226 225 225 225 226 226 226 226 226
       225 224 224 224 225 225 226 226 226 227 227 228 229 0 2 3 5 8 10
       12 15 17 19 21 23 25 27 29 32 34 37 41 44 47 50 52 55 57 59 60
       62 63 63 64)
      (62 62 61 60 59 57 56 54 52 49 47 44 41 38 35 32 30 28 26 24 22
       20 18 16 13 11 8 6 4 2 1 0 229 228 227 227 226 226 226 225 225
       225 225 152 152 152 153 226 226 225 225 225 226 226 153 152 152
       152 225 225 225 225 226 226 226 227 227 228 229 0 1 2 4 6 8 11
       13 16 18 20 22 24 26 28 30 32 35 38 41 44 47 49 52 54 56 57 59
       60 61 62 62)
      (60 60 59 58 57 56 54 53 51 49 46 44 41 38 35 33 30 28 27 25 23
       21 19 17 14 12 9 7 5 3 2 1 0 229 228 227 227 227 226 226 226 225
       152 152 152 152 153 153 152 152 152 152 152 153 153 152 152 152
       152 225 226 226 226 227 227 227 228 229 0 1 2 3 5 7 9 12 14 17
       19 21 23 25 27 28 30 33 35 38 41 44 46 49 51 53 54 56 57 58 59
       60 60)
      (58 57 57 56 55 54 53 52 50 48 46 43 41 38 35 33 31 29 27 26 24
       23 21 18 16 13 11 8 6 5 3 2 1 0 229 228 227 227 227 226 226 152
       152 152 152 153 153 153 152 152 152 152 152 153 153 153 152 152
       152 152 226 226 227 227 227 228 229 0 1 2 3 5 6 8 11 13 16 18 21
       23 24 26 27 29 31 33 35 38 41 43 46 48 50 52 53 54 55 56 57 57
       58)
      (55 55 55 54 53 53 52 50 49 47 45 43 41 38 36 33 31 30 28 27 26
       24 22 20 17 15 12 10 8 6 5 4 2 1 0 229 228 227 227 226 152 152
       153 153 153 153 153 153 153 152 152 152 153 153 153 153 153 153
       153 152 152 226 227 227 228 229 0 1 2 4 5 6 8 10 12 15 17 20 22
       24 26 27 28 30 31 33 36 38 41 43 45 47 49 50 52 53 53 54 55 55
       55)
      (53 53 53 52 52 51 50 49 48 47 45 43 41 38 36 34 32 30 29 28 27
       25 23 21 19 16 14 12 10 8 7 6 4 2 1 0 228 227 227 153 153 153
       153 154 154 154 154 154 153 153 153 153 153 154 154 154 154 154
       153 153 153 153 227 227 228 0 1 2 4 6 7 8 10 12 14 16 19 21 23
       25 27 28 29 30 32 34 36 38 41 43 45 47 48 49 50 51 52 52 53 53
       53)
      (51 51 51 51 50 50 49 48 47 46 45 43 41 39 36 34 33 31 30 29 27
       26 24 22 20 18 16 14 12 11 9 8 6 4 2 154 154 154 154 154 154 154
       154 155 155 155 155 154 154 154 154 154 154 154 155 155 155 155
       154 154 154 154 154 154 154 154 2 4 6 8 9 11 12 14 16 18 20 22
       24 26 27 29 30 31 33 34 36 39 41 43 45 46 47 48 49 50 50 51 51
       51 51)
      (50 50 50 50 49 49 48 47 46 45 44 43 41 39 37 35 33 32 31 29 28
       27 25 23 21 20 18 16 15 13 12 10 9 7 155 154 154 154 154 154 155
       155 155 156 156 156 156 155 155 155 155 155 155 155 156 156 156
       156 155 155 155 154 154 154 154 154 155 7 9 10 12 13 15 16 18 20
       21 23 25 27 28 29 31 32 33 35 37 39 41 43 44 45 46 47 48 49 49
       50 50 50 50)
      (48 49 49 48 48 48 47 46 45 44 43 42 41 39 38 36 34 33 31 30 29
       28 26 24 23 21 20 19 17 16 15 13 12 12 155 155 154 154 155 155
       155 156 156 157 157 157 157 156 156 156 156 156 156 156 157 157
       157 157 156 156 155 155 155 154 154 155 155 12 12 13 15 16 17 19
       20 21 23 24 26 28 29 30 31 33 34 36 38 39 41 42 43 44 45 46 47
       48 48 48 49 49 48)
      (47 47 47 47 47 47 46 45 44 43 42 41 40 39 38 37 35 34 32 31 30
       29 27 26 24 23 22 21 20 19 18 16 15 15 156 155 155 155 155 156
       156 157 157 158 158 158 158 158 157 157 157 157 157 158 158 158
       158 158 157 157 156 156 155 155 155 155 156 15 15 16 18 19 20 21
       22 23 24 26 27 29 30 31 32 34 35 37 38 39 40 41 42 43 44 45 46
       47 47 47 47 47 47)
      (46 46 46 46 46 45 45 44 43 42 41 40 39 39 38 37 36 34 33 32 31
       30 29 27 26 25 25 24 23 22 21 20 18 18 156 156 156 156 156 157
       158 158 159 159 160 160 159 159 159 158 158 158 159 159 159 160
       160 159 159 158 158 157 156 156 156 156 156 18 18 20 21 22 23 24
       25 25 26 27 29 30 31 32 33 34 36 37 38 39 39 40 41 42 43 44 45
       45 46 46 46 46 46)
      (44 44 44 44 44 44 44 43 42 42 41 40 39 38 38 37 36 35 34 33 32
       31 30 29 28 28 27 27 26 25 24 23 22 157 157 157 157 157 158 158
       159 160 160 161 161 161 161 160 160 160 160 160 160 160 161 161
       161 161 160 160 159 158 158 157 157 157 157 157 22 23 24 25 26
       27 27 28 28 29 30 31 32 33 34 35 36 37 38 38 39 40 41 42 42 43
       44 44 44 44 44 44 44)
      (43 43 43 43 43 43 43 42 42 41 40 39 38 38 37 37 36 36 35 34 33
       32 31 31 30 30 30 29 28 27 27 26 25 157 157 158 158 159 159 160
       161 162 162 162 162 162 162 162 162 162 162 162 162 162 162 162
       162 162 162 162 161 160 159 159 158 158 157 157 25 26 27 27 28
       29 30 30 30 31 31 32 33 34 35 36 36 37 37 38 38 39 40 41 42 42
       43 43 43 43 43 43 43)
      (42 42 42 42 42 42 42 42 41 41 40 39 38 37 37 37 37 36 36 35 34
       33 33 32 32 32 32 31 30 30 29 28 158 158 158 158 159 160 161 162
       163 163 164 164 163 163 163 163 163 163 163 163 163 163 163 163
       163 164 164 163 163 162 161 160 159 158 158 158 158 28 29 30 30
       31 32 32 32 32 33 33 34 35 36 36 37 37 37 37 38 39 40 41 41 42
       42 42 42 42 42 42 42)
      (41 41 41 41 41 41 41 41 41 40 40 39 38 37 37 37 37 37 36 36 35
       35 34 34 34 34 34 33 33 32 32 32 158 158 159 159 160 161 162 163
       164 165 165 165 165 164 164 164 164 164 164 164 164 164 164 164
       165 165 165 165 164 163 162 161 160 159 159 158 158 32 32 32 33
       33 34 34 34 34 34 35 35 36 36 37 37 37 37 37 38 39 40 40 41 41
       41 41 41 41 41 41 41)
      (40 40 40 40 40 40 40 40 40 40 39 38 38 37 37 37 37 37 37 37 36
       36 36 36 36 36 36 36 35 35 35 35 35 159 159 160 161 162 163 164
       165 166 166 166 166 166 166 166 166 166 166 166 166 166 166 166
       166 166 166 166 165 164 163 162 161 160 159 159 35 35 35 35 35
       36 36 36 36 36 36 36 36 37 37 37 37 37 37 37 38 38 39 40 40 40
       40 40 40 40 40 40 40)
      (40 40 40 40 40 40 40 40 40 39 39 38 37 37 37 37 37 38 38 37 37
       37 37 37 38 38 38 38 37 37 37 37 37 160 160 161 162 163 165 166
       167 167 168 168 167 167 168 168 168 168 168 168 168 168 168 167
       167 168 168 167 167 166 165 163 162 161 160 160 37 37 37 37 37
       38 38 38 38 37 37 37 37 37 38 38 37 37 37 37 37 38 39 39 40 40
       40 40 40 40 40 40 40)
      (40 40 40 40 39 39 39 39 39 39 38 38 37 37 37 37 38 38 38 38 38
       38 38 39 39 40 40 40 40 39 39 40 40 161 161 162 163 165 166 167
       168 169 169 169 169 169 169 170 170 170 170 170 170 170 169 169
       169 169 169 169 168 167 166 165 163 162 161 161 40 40 39 39 40
       40 40 40 39 39 38 38 38 38 38 38 38 37 37 37 37 38 38 39 39 39
       39 39 39 40 40 40 40)
      (40 40 40 39 39 38 38 38 38 38 38 37 37 37 37 37 38 39 39 39 39
       39 39 40 41 41 41 42 42 42 42 42 43 44 162 163 165 166 167 168
       169 170 171 171 171 171 171 171 172 172 172 172 172 171 171 171
       171 171 171 170 169 168 167 166 165 163 162 44 43 42 42 42 42 42
       41 41 41 40 39 39 39 39 39 39 38 37 37 37 37 37 38 38 38 38 38
       38 39 39 40 40 40)
      (39 39 39 39 38 37 37 37 37 37 37 37 37 37 37 38 38 39 40 40 40
       40 40 41 42 42 43 43 44 44 44 44 45 46 47 165 166 167 168 170
       171 172 172 173 173 173 173 173 174 174 174 174 174 173 173 173
       173 173 172 172 171 170 168 167 166 165 47 46 45 44 44 44 44 43
       43 42 42 41 40 40 40 40 40 39 38 38 37 37 37 37 37 37 37 37 37
       37 38 39 39 39 39)
      (38 38 38 38 37 36 36 36 36 36 36 36 36 37 37 38 39 40 40 41 41
       41 41 42 43 43 44 45 45 45 46 46 47 48 49 167 167 168 170 171
       172 173 174 174 175 175 175 175 175 176 176 176 175 175 175 175
       175 174 174 173 172 171 170 168 167 167 49 48 47 46 46 45 45 45
       44 43 43 42 41 41 41 41 40 40 39 38 37 37 36 36 36 36 36 36 36
       36 37 38 38 38 38)
      (38 38 37 37 36 36 35 35 35 35 35 35 36 36 37 38 39 40 41 41 42
       42 42 43 44 44 45 46 46 47 47 48 49 50 51 52 169 170 171 172 174
       175 175 176 177 177 177 177 177 177 177 177 177 177 177 177 177
       176 175 175 174 172 171 170 169 52 51 50 49 48 47 47 46 46 45 44
       44 43 42 42 42 41 41 40 39 38 37 36 36 35 35 35 35 35 35 36 36
       37 37 38 38)
      (38 37 37 36 36 35 35 35 35 35 35 35 35 36 37 38 39 40 41 42 42
       42 43 44 44 45 46 47 48 48 49 50 51 52 53 54 171 172 173 174 175
       176 177 178 179 179 179 179 179 179 178 179 179 179 179 179 179
       178 177 176 175 174 173 172 171 54 53 52 51 50 49 48 48 47 46 45
       44 44 43 42 42 42 41 40 39 38 37 36 35 35 35 35 35 35 35 35 36
       36 37 37 38)
      (38 37 36 36 35 35 35 35 35 35 35 35 35 36 37 38 39 40 41 42 42
       43 44 45 45 46 47 48 49 50 51 52 53 54 55 56 58 173 174 176 177
       178 179 180 180 181 181 181 181 180 180 180 181 181 181 181 180
       180 179 178 177 176 174 173 58 56 55 54 53 52 51 50 49 48 47 46
       45 45 44 43 42 42 41 40 39 38 37 36 35 35 35 35 35 35 35 35 35
       36 36 37 38)
      (38 37 36 35 35 35 35 35 35 35 35 35 36 36 37 38 39 40 41 42 43
       44 45 46 47 47 48 49 51 52 53 54 55 56 57 58 60 61 176 178 179
       179 180 181 182 182 183 183 182 182 182 182 182 183 183 182 182
       181 180 179 179 178 176 61 60 58 57 56 55 54 53 52 51 49 48 47
       47 46 45 44 43 42 41 40 39 38 37 36 36 35 35 35 35 35 35 35 35
       35 36 37 38)
      (132 132 133 35 34 34 34 35 35 35 35 36 36 37 38 39 40 41 42 43
       44 45 46 47 48 49 50 51 52 54 55 56 57 58 59 60 61 62 63 180 180
       181 181 182 183 184 184 184 184 184 184 184 184 184 184 184 183
       182 181 181 180 180 63 62 61 60 59 58 57 56 55 54 52 51 50 49 48
       47 46 45 44 43 42 41 40 39 38 37 36 36 35 35 35 35 34 34 34 35
       133 132 132)
      (132 133 133 134 34 34 34 34 35 35 36 36 37 38 39 40 41 42 43 44
       46 47 48 49 50 51 52 53 54 56 57 58 59 60 61 62 63 63 64 182 182
       182 183 184 185 185 186 186 186 186 186 186 186 186 186 185 185
       184 183 182 182 182 64 63 63 62 61 60 59 58 57 56 54 53 52 51 50
       49 48 47 46 44 43 42 41 40 39 38 37 36 36 35 35 34 34 34 34 134
       133 133 132)
      (133 133 134 134 134 34 34 34 35 36 36 37 38 39 40 41 42 43 45 46
       47 49 50 51 52 53 54 55 56 58 59 60 61 62 63 64 64 65 65 183 183
       184 184 185 186 187 187 188 188 188 188 188 188 188 187 187 186
       185 184 184 183 183 65 65 64 64 63 62 61 60 59 58 56 55 54 53 52
       51 50 49 47 46 45 43 42 41 40 39 38 37 36 36 35 34 34 34 134 134
       134 133 133)
      (134 134 134 134 134 34 34 34 35 36 37 38 39 40 41 43 44 45 46 48
       49 51 52 53 54 55 56 57 59 60 61 62 63 64 64 65 66 66 66 185 185
       185 186 186 187 188 189 189 190 190 190 190 190 189 189 188 187
       186 186 185 185 185 66 66 66 65 64 64 63 62 61 60 59 57 56 55 54
       53 52 51 49 48 46 45 44 43 41 40 39 38 37 36 35 34 34 34 134 134
       134 134 134)
      (134 134 134 134 34 34 34 35 35 36 37 38 40 41 43 45 46 47 48 50
       52 53 55 56 57 58 59 60 61 62 63 64 65 65 66 66 67 67 187 187
       187 187 187 188 188 189 190 191 191 191 191 191 191 191 190 189
       188 188 187 187 187 187 187 67 67 66 66 65 65 64 63 62 61 60 59
       58 57 56 55 53 52 50 48 47 46 45 43 41 40 38 37 36 35 35 34 34
       34 134 134 134 134)
      (0 134 34 0 34 34 35 35 36 36 37 39 41 43 45 46 48 49 51 52 54 56
       58 59 60 61 62 63 64 65 66 66 67 67 67 68 68 68 189 189 188 188
       189 189 189 190 191 192 192 192 192 192 192 192 191 190 189 189
       189 188 188 189 189 68 68 68 67 67 67 66 66 65 64 63 62 61 60 59
       58 56 54 52 51 49 48 46 45 43 41 39 37 36 36 35 35 34 34 0 34
       134 0)
      (0 0 0 0 0 0 0 37 36 36 38 40 42 44 46 48 50 52 53 55 57 59 61 62
       63 64 66 67 68 68 69 69 69 69 69 69 69 192 191 190 190 190 190
       190 190 191 192 192 193 193 193 193 193 192 192 191 190 190 190
       190 190 190 191 192 69 69 69 69 69 69 69 68 68 67 66 64 63 62 61
       59 57 55 53 52 50 48 46 44 42 40 38 36 36 37 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 44 46 48 50 52 54 56 58 60 62 63 65 66
       68 69 70 71 72 72 72 72 72 72 71 194 193 192 192 191 191 191 191
       191 192 192 193 193 193 193 193 193 193 192 192 191 191 191 191
       191 192 192 193 194 71 72 72 72 72 72 72 71 70 69 68 66 65 63 62
       60 58 56 54 52 50 48 46 44 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 49 50 52 54 57 59 61 63 65 66 68 69 71
       72 73 74 75 75 76 76 76 75 196 196 195 194 193 193 192 192 192
       192 193 193 193 193 193 193 193 193 193 193 193 192 192 192 192
       193 193 194 195 196 196 75 76 76 76 75 75 74 73 72 71 69 68 66
       65 63 61 59 57 54 52 50 49 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 52 52 54 57 60 62 64 66 68 69 71 72 74
       75 77 78 78 79 80 80 80 80 197 197 197 196 195 194 194 193 193
       194 194 194 194 194 193 193 193 194 194 194 194 194 193 193 194
       194 195 196 197 197 197 80 80 80 80 79 78 78 77 75 74 72 71 69
       68 66 64 62 60 57 54 52 52 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 57 60 63 65 67 69 71 73 74 75 77
       79 80 81 82 83 84 84 85 86 198 198 198 198 197 196 195 195 195
       195 195 195 195 194 194 194 194 194 195 195 195 195 195 195 195
       196 197 198 198 198 198 86 85 84 84 83 82 81 80 79 77 75 74 73
       71 69 67 65 63 60 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 63 66 68 70 72 74 76 77 79 80 82
       83 85 86 87 88 88 89 90 90 200 200 199 199 198 197 196 196 196
       196 196 195 195 194 194 194 195 195 196 196 196 196 196 197 198
       199 199 200 200 90 90 89 88 88 87 86 85 83 82 80 79 77 76 74 72
       70 68 66 63 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 69 71 73 75 77 79 81 82 84 85
       87 88 90 91 91 92 92 92 92 202 201 201 200 200 199 198 197 197
       196 196 196 195 195 194 195 195 196 196 196 197 197 198 199 200
       200 201 201 202 92 92 92 92 91 91 90 88 87 85 84 82 81 79 77 75
       73 71 69 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 70 74 76 78 80 82 84 85 87 89
       90 92 93 94 95 94 94 94 204 204 203 202 202 201 200 199 198 197
       197 196 196 196 195 195 195 196 196 196 197 197 198 199 200 201
       202 202 203 204 204 94 94 94 95 94 93 92 90 89 87 85 84 82 80 78
       76 74 70 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 77 79 81 83 85 86 88 90 92
       93 94 96 96 97 96 96 203 204 204 204 203 203 202 201 200 199 198
       197 197 196 196 195 195 195 196 196 197 197 198 199 200 201 202
       203 203 204 204 204 203 96 96 97 96 96 94 93 92 90 88 86 85 83
       81 79 77 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 82 82 83 85 87 89 91 93 94
       96 97 98 99 99 98 201 202 203 204 204 204 203 203 202 200 199
       198 197 197 196 196 195 195 195 196 196 197 197 198 199 200 202
       203 203 204 204 204 203 202 201 98 99 99 98 97 96 94 93 91 89 87
       85 83 82 82 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 84 84 85 87 89 91 93 95 97
       98 99 100 100 100 200 200 201 202 203 204 204 204 203 202 201
       199 198 197 196 196 195 195 195 195 195 196 196 197 198 199 201
       202 203 204 204 204 203 202 201 200 200 100 100 100 99 98 97 95
       93 91 89 87 85 84 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 84 85 86 88 90 92 94 97 99
       100 101 102 102 200 200 200 201 202 203 204 204 204 203 202 201
       199 198 197 196 195 195 195 194 195 195 195 196 197 198 199 201
       202 203 204 204 204 203 202 201 200 200 200 102 102 101 100 99
       97 94 92 90 88 86 85 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 208 208 208 207 207 207 207
       98 100 101 103 103 103 200 200 200 201 202 203 203 203 203 202
       201 200 199 198 197 196 195 194 194 194 194 194 195 196 197 198
       199 200 201 202 203 203 203 203 202 201 200 200 200 103 103 103
       101 100 98 207 207 207 207 208 208 208 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 209 208 208 207 207 207 207
       207 207 102 103 104 200 200 200 200 201 201 202 203 203 202 202
       201 200 198 197 196 195 194 194 193 193 193 194 194 195 196 197
       198 200 201 202 202 203 203 202 201 201 200 200 200 200 104 103
       102 207 207 207 207 207 207 208 208 209 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 209 209 209 208 207 207 207
       207 207 206 205 204 202 201 200 200 200 200 201 202 202 202 202
       201 200 199 197 196 195 194 194 193 193 193 193 193 194 194 195
       196 197 199 200 201 202 202 202 202 201 200 200 200 200 201 202
       204 205 206 207 207 207 207 207 208 209 209 209 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 208 209 209 208 208 207 207 206
       206 206 205 205 204 203 202 201 200 200 201 201 201 201 201 201
       200 199 197 196 195 194 194 193 193 193 193 193 193 193 194 194
       195 196 197 199 200 201 201 201 201 201 201 200 200 201 202 203
       204 205 205 206 206 206 207 207 208 208 209 209 208 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 205 207 208 208 208 208 207 206
       206 205 205 205 204 203 203 202 201 201 201 201 201 201 201 200
       199 199 197 196 195 194 194 193 193 193 193 192 193 193 193 193
       194 194 195 196 197 199 199 200 201 201 201 201 201 201 201 202
       203 203 204 205 205 205 206 206 207 208 208 208 208 207 205 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 205 206 207 208 207 207 206 205
       205 204 204 203 203 202 202 202 201 201 201 201 200 200 200 199
       198 197 196 195 194 193 193 193 193 193 192 192 192 193 193 193
       193 193 194 195 196 197 198 199 200 200 200 201 201 201 201 202
       202 202 203 203 204 204 205 205 206 207 207 208 207 206 205 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 202 204 205 206 207 207 206 205 204
       203 203 202 202 201 201 201 201 201 200 200 200 200 199 199 198
       197 196 195 194 193 193 192 192 192 192 192 192 192 192 192 192
       192 193 193 194 195 196 197 198 199 199 200 200 200 200 201 201
       201 201 201 202 202 203 203 204 205 206 207 207 206 205 204 202
       0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 197 200 202 204 206 205 205 204 203
       202 201 201 200 200 200 200 200 200 199 199 199 199 199 198 198
       197 196 195 194 193 193 192 192 192 191 191 191 191 191 191 191
       192 192 192 193 193 194 195 196 197 198 198 199 199 199 199 199
       200 200 200 200 200 200 201 201 202 203 204 205 205 206 204 202
       200 197 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 168 0 0 0 190 194 197 200 202 203 203 202 202
       201 200 199 198 198 198 198 198 198 198 198 198 198 198 197 197
       196 196 195 194 193 193 192 192 191 191 191 191 191 190 191 191
       191 191 191 192 192 193 193 194 195 196 196 197 197 198 198 198
       198 198 198 198 198 198 198 198 199 200 201 202 202 203 203 202
       200 197 194 190 0 0 0 168 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 161 162 165 0 181 186 190 194 196 199 200 201 200
       199 198 198 197 196 195 195 196 196 196 196 197 197 197 196 196
       196 195 195 194 193 193 192 192 191 191 191 191 191 190 190 190
       191 191 191 191 191 192 192 193 193 194 195 195 196 196 196 197
       197 197 196 196 196 196 195 195 196 197 198 198 199 200 201 200
       199 196 194 190 186 181 0 165 162 161 0 0 0 0 0 0 0)
      (0 0 150 152 0 0 156 158 161 164 170 177 182 186 190 193 195 197
       197 197 196 196 195 194 193 193 193 193 194 194 195 195 195 195
       195 195 195 195 194 194 193 192 192 191 191 191 191 191 191 190
       190 190 191 191 191 191 191 191 192 192 193 194 194 195 195 195
       195 195 195 195 195 194 194 193 193 193 193 194 195 196 196 197
       197 197 195 193 190 186 182 177 170 164 161 158 156 0 0 152 150
       0 0)
      (146 147 148 149 150 152 154 157 159 163 168 173 177 182 185 189
       191 193 193 193 193 193 192 191 191 191 191 191 192 193 193 194
       194 195 195 195 195 194 194 193 193 192 192 191 191 191 191 191
       190 190 190 190 190 191 191 191 191 191 192 192 193 193 194 194
       195 195 195 195 194 194 193 193 192 191 191 191 191 191 192 193
       193 193 193 193 191 189 185 182 177 173 168 163 159 157 154 152
       150 149 148 147 146)
      (146 146 147 148 149 151 153 155 158 161 165 169 173 177 181 184
       186 188 189 190 190 189 189 188 188 188 188 189 190 191 192 193
       194 195 0 196 0 0 0 0 193 193 0 192 191 191 191 190 190 190 190
       190 190 190 191 191 191 192 0 193 193 0 0 0 0 196 0 195 194 193
       192 191 190 189 188 188 188 188 189 189 190 190 189 188 186 184
       181 177 173 169 165 161 158 155 153 151 149 148 147 146 146)
      (146 146 146 147 149 150 152 154 156 159 162 165 169 173 176 179
       182 184 186 186 186 186 185 185 185 185 186 187 188 190 191 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 191 190 190 190 190 190 190 190 190 190
       191 0 0 0 0 0 0 0 0 0 0 0 0 0 0 191 190 188 187 186 185 185 185
       185 186 186 186 186 184 182 179 176 173 169 165 162 159 156 154
       152 150 149 147 146 146 146)
      (145 145 146 147 148 149 151 152 154 157 159 162 165 169 172 175
       177 180 181 182 182 182 182 182 182 183 184 185 187 188 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 188 187 185 184 183 182 182 182 182 182 182 181 180
       177 175 172 169 165 162 159 157 154 152 151 149 148 147 146 145
       145)
      (145 145 145 146 147 148 149 151 153 155 157 159 162 165 168 171
       173 175 177 178 178 179 179 179 180 181 182 184 185 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 185 184 182 181 180 179 179 179 178 178 177 175 173
       171 168 165 162 159 157 155 153 151 149 148 147 146 145 145 145)
      (145 145 145 145 146 147 148 150 151 153 155 157 159 162 165 167
       169 171 173 174 175 175 176 177 178 179 180 182 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 182 180 179 178 177 176 175 175 174 173 171 169 167
       165 162 159 157 155 153 151 150 148 147 146 145 145 145 145)
      (144 144 144 144 145 146 147 148 150 151 153 155 157 159 162 164
       166 168 169 170 171 172 173 175 176 177 179 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 179 177 176 175 173 172 171 170 169 168 166 164 162
       159 157 155 153 151 150 148 147 146 145 144 144 144 144)
      (143 143 143 143 144 145 146 147 148 150 151 153 155 157 159 161
       163 165 166 167 169 170 171 172 174 175 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 175 174 172 171 170 169 167 166 165 163 161 159 157
       155 153 151 150 148 147 146 145 144 143 143 143 143)
      (142 142 142 143 143 144 145 146 147 148 150 152 154 155 157 159
       160 162 163 165 167 168 169 170 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 171 170 169 168 167 165 163 162 160 159 157 155 154
       152 150 148 147 146 145 144 143 143 142 142 142)
      (141 141 141 142 143 143 144 145 146 147 149 151 152 154 155 157
       158 160 161 163 165 166 167 168 169 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 169 168 167 166 165 163 161 160 158 157 155 154 152
       151 149 147 146 145 144 143 143 142 141 141 141)
      (140 140 141 141 142 143 143 144 145 147 148 150 151 153 154 155
       157 158 160 161 163 165 166 167 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 167 166 165 163 161 160 158 157 155 154 153 151 150
       148 147 145 144 143 143 142 141 141 140 140)
      (140 140 140 141 142 142 143 144 145 146 148 149 150 152 153 154
       156 157 158 160 162 164 165 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 165 164 162 160 158 157 156 154 153 152 150 149 148
       146 145 144 143 142 142 141 140 140 140)
      (140 140 141 0 142 142 143 144 145 146 147 148 150 151 152 154
       155 156 157 159 161 162 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 162 161 159 157 156 155 154 152 151 150 148 147 146
       145 144 143 142 142 0 141 140 140)
      (0 0 0 0 0 0 0 145 145 146 147 148 149 151 152 153 154 155 157
       158 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       160 158 157 155 154 153 152 151 149 148 147 146 145 145 0 0 0 0
       0 0 0)
      (0 0 0 0 0 0 0 146 146 146 147 148 149 151 152 153 154 155 156
       158 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       158 156 155 154 153 152 151 149 148 147 146 146 146 0 0 0 0 0 0
       0)
      (0 0 0 0 0 0 0 0 0 148 0 148 150 151 152 153 154 155 156 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 156 155
       154 153 152 151 150 148 0 148 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 152 0 153 0 155 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 155 0 153 0 152 0 0
       0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0))
    :mask-contents
    '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0
       1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0
       1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 0 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
       1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 1 0 1 1 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1
       1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 0 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
       1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0))))



#+run-example
(run-cell-widgets-example)
 
