;;; object-editor-example

#|

Creating an Object-Editor Dialog or Class-Grid Programmatically
 
Common Graphics includes two types of interface objects that make
it easy to browse and edit instances of classes.  The
instances are typically stored in databases, but they do not need to be.

One of these interface objects is the class-grid widget.  This
is a specialized grid-widget that has a column for each application slot
of interest and a row for each instance that you tell it to display.

The other interface object is the object-editor dialog.
It has a standalone widget for each edited slot of a single
currently-displayed instance,
as well as a number of standard command buttons for displaying
different instances and saving edits to them and so on.

To create these interface objects, you do not need to create the
individual grid columns or widgets from scratch.  Instead, you
specify a special expression as the edited-slots property of the
class-grid or object-editor.  The edited-slots property is list
of slot specifications.  Each slot specification is a list where
the first member is the name of a slot to edit, and the
rest of the list is a plist of attributes that indicate what
sort of values are in the slot and what sort of widgets should
be used to edit them.  A class-grid will automatically create
its grid columns from this specification, and an object-editor
will automatically create and arrange its widgets.

Typically you would pass an edited-slots list when creating either
a class-grid or an object-editor, something like this:

  :edited-slots
  '((date-of-birth
     :edited-type (:date :yyyy/mm/dd)
     :label "Birth Date")
    (favorite-color
     :edited-type (:single-choice (:red :green :blue :yellow))
     :on-print capitalize-object
     :label "Fave Color")
    (best-friend
     :edited-type (:class-instance patient)
     :width 140))

Though writing a single edited-slots expression is easier than
defining a set of widgets or grid columns, there are still a
fair number of options to remember (or look up).  To avoid even
writing an edited-slots expression, an alternative is to use the IDE's
Class Interface Editor to specify the edited slots interactively.
This document covers the programmatic approach; for the
interactive approach see the doc page for the Class Interface
Editor dialog.

The code below illustrates making a standalone class-grid and
then an object-editor that contains a class-grid.  Most of
the code here is actually for setting up some sample classes
with instances.  This example uses an AllegroCache database,
though that's not necessary.

|#

(in-package :cg-user)

;;; This will load the CG module that uses AllegroCache (which is
;;; not present in the IDE by default) if it's not loaded
;;; already, and also load the newest version of acache itself
;;; as well if needed.
(require-cg-acache)

;;; This will create a new AllegroCache database.
;;; An open database is needed before we create instances
;;; of persistent classes below.  You could use a different
;;; path for the database if desired.
;;; (This code would use an already-open database if there is one,
;;; so first make sure that you do not have an unrelated
;;; database open.)
(unless db.ac:*allegrocache*
  (db.ac:create-file-database
   (merge-pathnames "tempdb/" (sys:temporary-directory))))

;;; We need to do a rollback if running through this demo a
;;; second time, because otherwise the previous set of instances
;;; will still exist.  But it doesn't hurt to do it the first time
;;; too.  cg-rollback calls db.ac:rollback and then handles
;;; any needed side effects in any object-editors and class-grids.
(cg-rollback :confirm nil)

;;; Let's define a test class for patients.
(defclass patient ()
    ((last-name :accessor last-name
                :initarg :last-name
                :index :any
                :initform "")
     (first-name :accessor first-name
                 :initarg :first-name
                 :initform "")
     (access :accessor access
             :initarg :access
             :initform nil)
     (date-of-birth :accessor date-of-birth
                    :initarg :date-of-birth
                    :initform "")
     (unpaid-balance :accessor unpaid-balance
                     :initarg :unpaid-balance
                     :initform "0.00")
     (friendly :accessor friendly
                :initarg :friendly
                :initform t)
     (favorite-color :accessor favorite-color
                     :initarg :favorite-color
                     :initform "")
     (static-note :accessor static-note
                  :initarg :static-note
                  :initform "")
     (best-friend :accessor best-friend
                  :initarg :best-friend
                  :initform nil)
     (prescriptions :accessor prescriptions
                    :initarg :prescriptions
                    :initform nil)
     )
  (:metaclass db.ac:persistent-class))

;;; And make some test instances of the patient class.
(defparameter *patients*
  (list (make-instance 'patient
          :first-name "Sarah" :last-name "Somebody"
          :date-of-birth "1963/01/15"
          :unpaid-balance "34.89"
          :friendly t :favorite-color :blue
          :static-note "Sarah is the first patient, but will get sorted toward the end."
          )
        (make-instance 'patient
          :first-name "Andy" :last-name "Anybody"
          :access :partial
          :date-of-birth "1971/03/02"
          :unpaid-balance ".09"
          :friendly nil :favorite-color :yellow
          :static-note "This description of Andy is so long that it may cause a scrolling-static-text widget that holds it to scroll."
          )
        (make-instance 'patient
          :first-name "Alice" :last-name "Loom"
          :access :partial
          :date-of-birth "1967/05/17"
          :unpaid-balance "729.95"
          :friendly nil :favorite-color :blue
          :static-note "This is a rather unimportant note that's not really about Alice at all."
          )
        (make-instance 'patient
          :first-name "Helen" :last-name "Nurble"
          :date-of-birth "1959/04/29"
          :unpaid-balance "8844.22"
          :friendly t :favorite-color :red
          :static-note "This field displays UNEDITABLE text."
          )
        (make-instance 'patient
          :first-name "Bill" :last-name "Nurby"
          :date-of-birth "1948/04/29"
          :unpaid-balance "42000.03"
          :friendly t :favorite-color :red
          :static-note "It's later than it's ever been."
          )
        (make-instance 'patient
          :first-name "Higgledy" :last-name "Piggledy"
          :access :full
          :date-of-birth "1968/06/12"
          :unpaid-balance "123.45"
          :friendly nil :favorite-color :blue
          :static-note "I suspect that this is not a real person."
          )
        (make-instance 'patient
          :first-name "Oof" :last-name "Noof"
          :access :partial
          :date-of-birth "1958/02/15"
          :unpaid-balance "3600.00"
          :friendly nil :favorite-color :red
          :static-note "What sort of name is Oof anyway?"
          )
        ))

;;; Now we make each patient be the best friend of the previous
;;; patient.  This points objects to each other by putting actual
;;; first-class objects (class instances) into the slots of other
;;; objects.  class-grids and object-editors have some interesting
;;; interface options for slots that contain class instances.
(do* ((patients *patients* (rest patients)))
     ((null patients))
  (setf (best-friend (first patients))
    (or (second patients)(first *patients*))))

;;; This bit of code specifies two slots of the patient class
;;; that should be used to derive a "pretty name" for each
;;; instance of the class.  These pretty names will be
;;; printed in menus and in widgets that hold class instances.
;;; If these slots are not set up, then objects will be print
;;; according to their print-object methods as usual, though
;;; these properties allow sorting by string slots without
;;; a lot of consing.
(progn
  (setf (primary-name-slot 'patient) 'last-name)
  (setf (secondary-name-slot 'patient) 'first-name))

;;; Now that we've set up a class with some instances, let's
;;; create a class-grid widget for editing the instances.

;;; This form makes a standalone class-grid that edits the patient
;;; class.  The :edited-slots specification here describes how to
;;; create a grid column for each application slot to display.
;;; See the edited-slots property for a full list of the options.

(let* ((width 600)
       (height 400)
       (grid (make-instance 'class-grid
               :column-header-height 40
               :edited-class 'patient
               
               :edited-slots
               `(
                 
                 ;; These columns edit simple strings of any length.
                 (first-name :edited-type (:variable-char)
                             :width-in-chars 8)
                 (last-name :edited-type (:variable-char)
                            :width 120
                            :sortable t
                            
                            ;; The column header label would default
                            ;; to "Last Name", so let's change this one.
                            :label "Family Name")
                 
                 ;; This pixmap column has an alist that maps application
                 ;; values like :partial to names of pixmaps like
                 ;; :key to display for each application value.
                 (access :edited-type (:pixmap ((nil nil)
                                                (:partial :key)
                                                (:full :key-special)))
                         :sortable t)
                 
                 ;; These columns use CG's template-string facility
                 ;; to restrict input to strings of a certain length
                 ;; and with certain types of characters at each position.
                 (date-of-birth :edited-type (:date :yyyy/mm/dd)
                                :label "Birth Date")
                 (unpaid-balance :edited-type (:fixed-numeric 6 2)
                                 :template-allows-sign t
                                 :label "Unpaid Balance")
                 
                 ;; A boolean type will be shown as a check-box.
                 (friendly :edited-type (:boolean)
                           :sortable t
                           :label "Friendly?")
                 
                 ;; Here's a list of hardcoded choices that will appear
                 ;; in a combo-box.
                 (favorite-color
                  :edited-type (:single-choice (:red :green :blue :yellow))
                  :on-print capitalize-object
                  :label "Fave Color")
                 
                 ;; This column displays a class instance, with a list
                 ;; of other selectable instances in a combo-box.
                 ;; Each instance will display its pretty name according
                 ;; to the primary- and secondary-name-slot that we
                 ;; specified earlier.
                 (best-friend
                  :edited-type (:class-instance patient)
                  
                  ;; This is the list of patients that the user can
                  ;; select as a new best friend.
                  ;; Alternative:  Don't pass this to make it offer ALL
                  ;; patients that exist.  (That requires an AllegroCache
                  ;; class so that all instances can be found.)
                  :edited-instances ,*patients*
                  
                  :width 140)
                 
                 ;; This column will indirectly access the birth date
                 ;; of the best friend of the patient on each row.
                 ;; The slot indicator is therefore a list of
                 ;; slot names rather than a single slot name.
                 ((best-friend date-of-birth)
                  :edited-type (:date :yyyy/mm/dd)
                  
                  ;; A subtle point:  This will make the column exactly
                  ;; fit the fixed-width string, rather than possibly
                  ;; defaulting it larger to make the column header string
                  ;; fit on one line.
                  :width-in-chars 10
                  
                  :label "Best Friend's Birth Date")
                 
                 ;; This column displays text that the end user
                 ;; cannot edit.
                 (static-note
                  :edited-type (:static-text)
                  :width 160)
                 )
               
               ;; This alternative would set up the rows when creating
               ;; the grid-widget, instead of adding them afterward
               ;; as is done below.
               ;; :value *patients*
               
               :right-attachment :right
               :bottom-attachment :bottom
               :width width
               :height height))
       (dialog (make-window :class-grid-example
                 :class 'dialog
                 :dialog-items (list grid)
                 :interior (make-box-relative 200 200 width height))))
  (select-window dialog)
  
  ;; Add some class instances to the grid.  This set of instances
  ;; can be changed at any time.  Individual instances could be
  ;; added incrementally by calling add-class-grid-row.
  (setf (value grid) *patients*)
  
  dialog)

;;; ----------------------------------------------------------
;;; An alternative to a class-grid is an object-editor dialog.
;;; The one that we'll make will include more data than that
;;; class-grid, so first we need to set up a few more application
;;; classes with instances.

;;; Define a second class for medications.
(defclass medication ()
  ((drug-name :accessor drug-name
              :initarg :drug-name
              :initform "")
   (drug-price :accessor drug-price
               :initarg :drug-price
               :initform "0.00")
   (drug-warnings :accessor drug-warnings
                  :initarg :drug-warnings
                  :initform ""))
  (:metaclass db.ac:persistent-class))

;;; Make some medication instances.  Each of these represents a product.
(defparameter *medications*
  (list
   (make-instance 'medication
     :drug-name "Lispitol"
     :drug-price "23.89"
     :drug-warnings "Associated with computer addiction.")
   (make-instance 'medication
     :drug-name "Franzeril"
     :drug-price "1.95"
     :drug-warnings "May give Bill Gates a headache.")
   (make-instance 'medication
     :drug-name "Objectifin"
     :drug-price "29.95"
     :drug-warnings "Sometimes leads to modularity.")
   (make-instance 'medication
     :drug-name "Dynamicine"
     :drug-price "299.00"
     :drug-warnings "May increase free time to kill.")
   (make-instance 'medication
     :drug-name "Parenthezol"
     :drug-price "123.45"
     :drug-warnings "Can reduce concern about syntax.")
   (make-instance 'medication
     :drug-name "Macrozine"
     :drug-price "14.89"
     :drug-warnings "Causes feelings of superiority.")
   ))

;;; Use the drug-name slot for the pretty name of a medication.
(setf (primary-name-slot 'medication) 'drug-name)

;;; Define a prescription class, where an instance will point to
;;; the medication that it's for and also to the patient for
;;; which it is prescribed.
(defclass prescription ()
  ((medication :accessor medication
               :initarg :medication
               :initform nil)
   (patient :accessor patient
            :initarg :patient
            :initform nil)
   (refills :accessor refills
            :initarg :refills
            :initform "0"))
  (:metaclass db.ac:persistent-class))

;;; Generate some random prescriptions for each patient.
;;; (Don't worry about how this works; we're just setting up
;;; sample data here.)
(do* ((count 0 (1+ count))
      (patients *patients* (rest patients))
      patient)
     ((null patients))
  (setq patient (first patients))
  (do* ((medications *medications* (rest medications))
        (medication (first medications)(first medications))
        (prescriptions nil))
       ((null medications))
    (when (or (zerop (random 2))
              
              ;; Make sure that each person has at least
              ;; two prescriptions.
              (<= (+ (length medications)
                     (length prescriptions))
                  2))
      
      (push (make-instance 'prescription
              :medication medication
              :patient patient
              :refills (random 3))
            prescriptions))
    (setf (prescriptions patient) prescriptions)))

;;; And now the astounding finale.  This last form creates
;;; an object-editor dialog with a separate widget for
;;; editing each interesting slot of its edited class.
;;; The :edited-class and :edited-slots arguments here are
;;; basically the same as what were passed for the
;;; standalone class-grid above; so this is a different
;;; view of the same data, but with some additional data.

;;; This dialog also contains a class-grid for
;;; editing the list of prescriptions for the currently
;;; displayed patient.  The grid's columns are specified
;;; here as a nested :edited-slots specification inside
;;; the :edited-slots expression for the whole dialog.
;;; (See the end of this form for the prescriptions.)

(make-window :test
  :class 'object-editor
  :scrollbars :vertical
  :exterior (make-box-relative
             60 60 700
             
             ;; This dialog height will be overridden
             ;; by a computed height that makes
             ;; all of the widgets fit vertically.
             600)
  
  ;; A layout-spacing object can be used to override some of
  ;; the default spacing parameters.
  :layout-spacing (make-instance 'layout-spacing
                    :layout-widget-spacing 4   ;; this is the default
                    :layout-outer-margin 12)
  
  ;; Change this value to t to include a special grid at the bottom
  ;; of the object-editor dialog that lists all of the edited class'
  ;; instances.
  :include-table-of-all-instances nil
  
  ;; You can pass any subset of these standard buttons here.
  ;; Or you could add your own widgets that call the exported
  ;; functions that these built-in buttons call.
  :command-buttons
  '(:first-button :previous-button :next-button :last-button
                  :select-button :search-button
                  :save-button :revert-button :new-button :delete-button
                  :commit-button :rollback-button)
  
  ;; This dialog will edit instances of the patient class.
  :edited-class 'patient
  
  ;; This tells the dialog to edit a specific set of patient instances.
  ;; Alternative:  Comment out this line to edit all instances
  ;; (which would require an AllegroCache database so that
  ;; all instances can be found).
  :edited-instances *patients*
  
  ;; Each entry in the large edited-slots property will create
  ;; a widget to edit the value in the named slot.
  :edited-slots
  `(
    (first-name :edited-type (:variable-char))
    (last-name :edited-type (:variable-char)
               :width 200
               :label "Family Name")
    (access :edited-type (:pixmap ((nil nil)
                                   (:partial :key)
                                   (:full :key-special))))
    (date-of-birth :edited-type (:date :yyyy/mm/dd)
                   :label "Birth Date")
    (unpaid-balance :edited-type (:fixed-numeric 6 2)
                    :template-allows-sign t
                    :fixed-width-font t
                    :label "Unpaid Balance")
    (friendly :edited-type (:boolean)
              :width 120
              :label "Friendly?")
    (favorite-color
     
     ;; A new column of widgets will begin with this widget,
     ;; to save space vertically.
     :new-column t
     
     :edited-type (:single-choice (:red :green :blue :yellow))
     :on-print capitalize-object
     :label "Fave Color")
    (static-note
     :height 44
     :edited-type (:static-text-multi-line))
    
    ;; This :class-instance case will create a combo-box
    ;; whose range is a list of instances of the patient class.
    (best-friend
     :edited-type (:class-instance patient)
     
     ;; If *patients* is specified here, then that specific
     ;; list of instances will be the choices, and otherwise all
     ;; instances of the class will be used.
     ;; Alternative: Comment this line out to make it
     ;; offer ALL patients as the choices.
     :edited-instances ,*patients*
     )
    
    ;; An indirection to a slot of the object in the best-friend slot.
    ((best-friend date-of-birth)
     :edited-type (:date)
     :fixed-width-font t
     :label "Friend's Bday")
    
    ;; The prescriptions slot holds a list of the prescriptions
    ;; for a patient, so we can shows those prescriptions in a
    ;; class-grid by specifying the edited type as a
    ;; table of prescription instances.
    (prescriptions
     :edited-type (:table-of-class-instances prescription)
     
     ;; These additional button widgets will allow the end user
     ;; to create new prescriptions for a patient, or to delete
     ;; some of a patient's prescriptions.
     :include-create-button t
     :include-delete-button t
     
     ;; This nested edited-slots form determines which prescription slots
     ;; should be grid columns in this class-grid on this object-editor.
     :edited-slots
     ((medication :edited-type (:class-instance medication)
                  :edited-instances ,*medications*)
      (refills :edited-type (:fixed-numeric 1 0)
               :horizontal-justification :center
               :label "Num Refills")
      
      ;; These entries indirectly access slots of the medication of
      ;; a prescription.  So the grid columns will show attributes
      ;; of the medication of a prescription for the patient that's
      ;; currently displayed on the object-editor dialog.
      ((medication drug-price)
       :edited-type (:fixed-numeric 6 2))
      ((medication drug-warnings)
       :edited-type (:variable-char)
       :width 200)))
    
    ))

