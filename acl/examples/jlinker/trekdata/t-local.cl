

;; $Id: t-local.cl,v 5.1 2004/02/18 19:19:03 mm Exp $

(in-package :user)
(eval-when (compile load eval)
  (require :jlinker)
  (use-package :net.jlinker)
  (load (compile-file-if-needed "trek.cl"))
  (load (compile-file-if-needed "t-data.cl"))
  (provide :t-java)
  ;;(pushnew :annotator-timer *features*)
  )

(load "jl-config.cl")

;; Sample DVC application with applet interface.
;; this is the raw Java version



(defvar *all-annotators* nil)
(defvar *annotator* nil)
(defvar *a-verbose* nil)
(defvar *a-colors* 
  '((:outer 0 0 250) (:cell 230 230 230) (:input 230 200 230)
    (:button 0 200 200)
    ))

(defun a-msg (where fmt &rest args)
  (when (or (eq where :error)
	    (and *a-verbose*
		 (or (atom *a-verbose*)
		     (member where *a-verbose*))))
    (format t "~&;;process ~A - ~A~%"
	    (mp:process-name mp:*current-process*)
	    (apply #'format nil fmt args))))





(def-java-class (gbc "java.awt.GridBagConstraints") () 
  ;; static slots
  ()
  ;; class slots
  (
   ("gridx" :accessor grid-x)
   ("gridy" :accessor grid-y)
   ("gridwidth" :accessor grid-width)
   ("gridheight" :accessor grid-height)
   ("weightx" :accessor weight-x)
   ("weighty" :accessor weight-y)
   ("fill"    :accessor grid-fill)
   ("insets"  :accessor insets)
   )
  ((x :initform nil) (y :initform nil) 
   (w :initform nil) (h :initform nil) 
   (wx :initform nil) (wy :initform nil) 
   (gf :initform nil) (in :initform nil)
   (rel :initform nil) (rem :initform nil)
   (hor :initform nil) (vert :initform nil)
   (both :initform nil) (none :initform nil)
   ))
  
(defmethod set-constraints ((c gbc) 
			    &key x y width height weight-x weight-y 
			    grid-fill insets
			    &aux done)
  (macrolet ((set-slot 
	      (arg slot default jslot &rest tail)
	      `(or (null ,arg)
		   (let ((v (slot-value c ,slot)))
		     (if (null v)
			 (eql ,arg ,default)
		       (eql v ,arg)))
		   (setf (slot-value c ,slot) ,arg
			 done t
			 (,jslot c)
			 (case ,arg
			   ,@(mapcar 
			      #'(lambda (one)
				  (list 
				   (first one) 
				   (list 
				    'or
				    (list 'slot-value 
					  'c
					  (second one))
				    (list 'setf
					  (list 'slot-value 
						'c
						(second one))
					  (list 'jfield
						"java.awt.GridBagConstraints" 
						(third one)
						)))))
			      tail)
			   (otherwise ,arg))))))
    (set-slot x 'x :relative grid-x (:relative 'rel "RELATIVE"))
    (set-slot y 'y :relative grid-y (:relative 'rel "RELATIVE"))
    (set-slot width 'w 1  grid-width 
	      (:relative 'rel "RELATIVE") 
	      (:remainder 'rem "REMAINDER"))
    (set-slot height 'h 1 grid-height 
	      (:relative 'rel "RELATIVE") 
	      (:remainder 'rem "REMAINDER"))
    (set-slot weight-x 'wx 0 weight-x)
    (set-slot weight-y 'wy 0 weight-y)
    (case grid-fill
      ((:horizontal :vertical :both nil) nil)
      (otherwise (setf grid-fill :none)))
    (set-slot grid-fill 'gf :none grid-fill
	      (:horizontal 'hor  "HORIZONTAL")
	      (:vertical   'vert "VERTICAL")
	      (:both       'both "BOTH")
	      (:none 'none "NONE"))
    (or (null insets)
	(typecase insets
	  ((integer 0 *) (setf insets (list insets insets insets insets)) nil)
	  (cons (setf insets (list (or (first insets) 0)
				   (or (second insets) 0)
				   (or (third insets) 0)
				   (or (fourth insets) 0)))
		nil)
	  (otherwise (setf insets (slot-value c 'in)) nil))
	(equal (slot-value c 'in) insets)
	(and (null (slot-value c 'in)) (equal insets '(0 0 0 0)))
	(setf (slot-value c 'in) insets 
	      done t
	      (insets c)
	      (jnew "java.awt.Insets" 
		    (first insets) (second insets) 
		    (third insets) (fourth insets))))
    done))




(defclass a-data ()
  ((db :accessor data-block)
   (dl :accessor data-layout)
   (ix :accessor index)
   (nm :accessor name)
   (rows :accessor rows)
   (cols :accessor cols)
   (ic :accessor item-count)
   (cn :accessor constraints)
   (co :accessor cell-outer)
   (cc :accessor cell-color)
   (incl :accessor input-color)
   (cs :accessor cell-constraints)
   (cb :accessor cell-block-constraints)
   (os :accessor outer-constraints)
   (cells :accessor cell-array)
   (ms :accessor modify-state)
   (rb :accessor reset-button)
   (fb :accessor confirm-button)
   (sa :accessor status-area)
   (st :accessor search-text)
   (lk :accessor scroll-lock :initform 0)
   (fs :accessor frame-status :initform nil)
   (fg :accessor frame-gate :initform nil)
   ))

(def-java-class (a-frame "AnnotatorPanel") (a-data)
  () () ())

(def-java-class (a-applet1 "TrekAppletLocal") (a-data)
  () () ())





(defmethod add-constrained (container layout (constraints gbc) item &rest keys)
  (apply #'set-constraints constraints keys)
  (jcall (jmethod "java.awt.GridBagLayout" "setConstraints"
		"java.awt.Component" "java.awt.GridBagConstraints")
	 layout item constraints)
  (jcall (jmethod "java.awt.Container" 
		"add" "java.awt.Component"  "java.lang.Object") 
	 container
	 item
	 constraints)
  item)
  

(defun make-annotator-login (frame)
  (a-msg :progress "Begin make login.")
  (let* ((fr-layout (jnew "java.awt.GridBagLayout"))
	 (gbc (jnew "java.awt.GridBagConstraints"))
	 login go message)

    (jcall (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager") 
	   frame fr-layout)
    (setf (frame-gate frame) (mp:make-gate nil)
	  (item-count frame) (get-annotator-data :item)
	  (constraints frame) gbc
	  (cell-constraints frame) (jnew "java.awt.GridBagConstraints") 
	  (cell-block-constraints frame) (jnew "java.awt.GridBagConstraints") 
	  (outer-constraints frame) (jnew "java.awt.GridBagConstraints") 
	  (cell-outer frame) (colors :outer :get)
	  (cell-color frame) (colors :cell  :get)
	  (input-color frame) (colors :input :get)
	  )
    
    (add-constrained frame fr-layout gbc
		     (jnew "java.awt.Label" 
			   "Please enter a login name (no spaces):")
		     :x :relative :y :relative :width :remainder :height 1
		     :insets (list 10 10 0 10)
		     :grid-fill :none)
    (add-constrained frame fr-layout gbc
		     (setf login (jnew (jconstructor "java.awt.TextField" "int") 20))
		     :insets (list 3 10 3 10))
    (add-constrained frame fr-layout gbc
		     (setf go (jnew "java.awt.Button" "Begin"))
		     :insets (list 3 10 3 10))
    (add-constrained frame fr-layout gbc
		     (setf message (jnew (jconstructor "java.awt.TextField" "int") 40))
		     :insets (list 3 10 10 10))
    (jcall "setEditable" message (make-immediate-object nil :boolean))
    (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	   login (input-color frame))
    (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	   message (cell-color frame))
    (setf (search-text frame) login)
    (jstatic 
     (jmethod "com.franz.jlinker.JLActionListener" "addTo" "java.awt.Button")
     nil go)
    (jregister-handler go :actionPerformed
		       #'(lambda (&rest x &aux (frame (first x))) 
			   (setf (name frame)
				 (string-trim 
				  " " 
				  (jcall 
				   (jmethod "java.awt.TextField" "getText")
				   (search-text frame))))
			   (mp:open-gate (frame-gate frame)))
		       :data frame)
    (setf (data-block frame) message
	  (data-layout frame) go)    
    t))

(defun make-annotator (frame &optional aname)
  (a-msg :progress "make-annotator waiting for login")
  (let* (buttons exit-button
	 (start1 (get-internal-real-time))
	 (start2 (get-internal-run-time))
	 (fr-layout (jnew "java.awt.GridBagLayout"))
	 (data-layout (jnew "java.awt.GridBagLayout"))
	 (data-block (jnew "java.awt.Panel" data-layout))
	 (rows 3) (cols 1))

    (let ((message (data-block frame))
	  (go (data-layout frame))
	  m)
      ;; first we wait for a name to be entered
      (loop
       (mp:close-gate (frame-gate frame))
       (mp:with-timeout 
	(2 :timeout)
	(mp:process-wait 
	 "login" #'mp:gate-open-p (frame-gate frame))
	(when (eq :exit (frame-status frame))
	  (return-from make-annotator nil))
	(when (mp:gate-open-p (frame-gate frame))
	  (if (or (and (equal (name frame) "")
		       (setf m "Login field is blank!"))
		  (and (not (every #'alpha-char-p (name frame)))
		       (setf m "Login name should be all letters!"))
		  (and (assoc (name frame) *all-annotators* :test #'equalp)
		       (setf m "Login is already active, please pick another."))
	       
		  )
	      (jcall (jmethod "java.awt.TextField" "setText" 
			    "java.lang.String")
		     message m)
	    (return))))
       (or (ignore-errors (jlinker-query)) (return-from make-annotator nil))
       )
      (jcall "removeAll" frame)
      (jregister-handler go :actionPerformed nil)
      (when aname
	(push (list (name frame) aname frame) *all-annotators*)
	)
      )
    
    (jcall (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager") 
	   frame fr-layout)
    (setf (data-block frame) data-block
	  (data-layout frame) data-layout
	  (rows frame) rows
	  (cols frame) cols
	  (cell-array frame) (make-array (list rows cols))
	  )

    (multiple-value-setq (buttons exit-button) (make-annotator-controls frame))

    (a-msg :progress "Begin data-block.")
    (fill-data-block frame :cell-generator #'make-annotator-cell
		     :index -1)
    (setf (scroll-lock frame) 0)
    (a-msg :progress "  End data-block.")

    (make-annotator-body frame fr-layout buttons data-block)
     
    (a-msg :time "Build: real=~A run=~A"
	   (- (get-internal-real-time) start1)
	   (- (get-internal-run-time) start2))
    (a-msg :progress "  End make.")
    exit-button))


(defmethod make-annotator-controls ((frame a-data))
  (a-msg :progress "Begin make-controls.")
  (let* ((gbc (constraints frame))
	 (button-layout (jnew "java.awt.GridBagLayout"))
	 (buttons (jnew "java.awt.Panel" button-layout))
	 button area exit-button
	 (bg (colors :button :get))
	 )
    (flet ((add-button (label action status1 &optional status2)
		       (add-constrained
			buttons button-layout gbc
			(setf button (jnew "java.awt.Button" label)))
		       (jstatic 
			(jmethod "com.franz.jlinker.JLActionListener" 
			       "addTo" "java.awt.Button")
			nil button)
		       (jregister-handler 
			button :actionPerformed action :data frame)
		       (add-status-line frame button status1 status2)
		       (jcall (jmethod "java.awt.Component" "setBackground" 
				     "java.awt.Color")
			      button bg)
		       ))

      (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	     buttons (cell-color frame))
      (add-constrained
       buttons button-layout gbc
       (jnew "java.awt.Label" "Controls:")
       :x :relative :y :relative :height 1 :width :remainder 
       :grid-fill :none
       :insets (list 5 5 5 5 ))

      (add-button "FirstPage" 
		  #'(lambda (&rest x) (handle-page-flip (first x) -1))
		  "Go back to first page in database.")
      (add-button "PrevPage"
		  #'(lambda (&rest x) (handle-page-flip (first x) :prev))
		  "Go back one page in database.")
      (add-button "NextPage"
		  #'(lambda (&rest x) (handle-page-flip (first x) :next))
		  "Go forward one page in database.")
      
      (add-constrained
       buttons button-layout gbc
       (setf area (jnew (jconstructor "java.awt.TextField" "int") 15)))
      (setf (search-text frame) area)
      (add-status-line frame area "Type a search string here.")
      (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	     area (input-color frame))

      (add-button "Search"
		  #'(lambda (&rest x) (search-a-data (first x)))
		  "Search for string in captions.")
      (add-button "Confirm"
		  #'(lambda (&rest x) (confirm-changes (first x)))
		  "Confirm changes to local captions."
		  "There are no changes. Refresh the screen.")
      (setf (confirm-button frame) button)
      (add-button "Reset"
		  #'(lambda (&rest x) (handle-page-flip (first x) :this))
		  "Discard changes to local captions."
		  "There are no changes. Refresh the screen.")
      (setf (reset-button frame) button)
      (add-button "Save"
		  #'(lambda (&rest x) (save-changes (first x)))
		  "Save local captions to file.")
      (add-button "Exit"
		  #'(lambda (&rest x) 
		      (setf (frame-status (first x)) :exit)
		      (jcall "removeAll" (first x)))
		  "Disconnect from database.")
      (setf exit-button button)

      (a-msg :progress "  End make-controls.")
      (values buttons exit-button))))

(defmethod make-annotator-body ((frame a-data) fr-layout buttons data-block)
  (let* ((gbc (constraints frame)) w)
    (a-msg :progress "Begin make-body.")

    (jcall (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager") 
	   frame fr-layout)

    (set-constraints gbc :insets (list 0 0 0 0))
    (add-constrained
     frame fr-layout gbc
     (jnew "java.awt.Label" (format nil "Annotator for user ~A" (name frame))) 
     :x 0 :y 0 :width :remainder :height 1 :weight-y 0.0)

    (add-constrained
     frame fr-layout gbc data-block :x 0 :y 1 :width :relative :height 1 
     :weight-x 1.0 :weight-y 1.0 :grid-fill :both)
      
    (add-constrained
     frame fr-layout gbc buttons
     :x 1 :y 1 :width 1 :height 1 :weight-x 0.0 :grid-fill :none)
      
    (add-constrained
     frame fr-layout gbc (setf w (jnew "java.awt.TextField"))
     :x 0 :y 2 :width :remainder :height 1 :weight-y 0.0
     :weight-x 1.0 :grid-fill :horizontal :insets 2)
    (jcall (jmethod "java.awt.TextComponent" "setEditable" "boolean")
	   w (make-immediate-object nil :boolean))
    (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	   w (cell-color frame))
    (setf (status-area frame) w)
  
    (a-msg :progress "  End make-body.")
    ))

(defmethod search-a-data ((frame a-data))
  (let (new)
    (if (setf new (get-annotator-data 
		   :search
		   (string-trim " " (jcall (jmethod "java.awt.TextField" "getText")
					   (search-text frame)))
		   :user (name frame) :index (index frame)))
	(progn (handle-page-flip frame new)
	       (show-status frame nil nil "Search string found."))
      (show-status frame t nil "Search failed."))
    new))

(defmethod add-status-line ((frame a-data) component &rest text)
  (let ()
    (jstatic "addTo" "com.franz.jlinker.JLMouseAdapter" component)
    (jregister-handler component
		      :mouseEntered
		      #'fill-status-line
		      :data (list* frame text))))

(defun fill-status-line (&rest x)
  (let* ((y (first x))
	 (an (first y))
	 (sa (status-area an))
	 (text (cdr y)))
    (jcall (jmethod "java.awt.TextField" "setText" "java.lang.String")
	   sa
	   (if (modify-state an)
	       (first text)
	     (or (second text) (first text))))))






(defmethod handle-page-flip (layout name)
  (declare (ignore button event descriptor))
  (case name (:this (setf (scroll-lock layout) 0)))
  (if (not (eql 0 (scroll-lock layout)))
      (show-status layout t nil "Slow down!!!")
    (let* (new
	   (start1 (get-internal-real-time))
	   (start2 (get-internal-run-time))
	   (rows (rows layout))
	   (cols (cols layout))
	   (index (index layout))
	   (res t))
      (setf new (if (numberp name) 
		    name
		  (+ index (* (case name 
				(:prev -1) 
				(:next +1) 
				(otherwise 0))
			      (1- rows) cols))))
      (setf new (max -1 new))
      (setf new (min new (1- (item-count layout))))
      (when (or (eq name :this) 
		(and (not (eql new index))
		     (if (modify-state layout)
			 (show-status 
			  layout t (setf res nil)
			  "Confirm or Reset modified captions first.")
		       t)))
	(fill-data-block layout :cell-generator #'make-annotator-cell
			 :index new)
	;; call validate here (pack is only for first time to 
	;;  pick a reasonable initial size)
	(jcall (jmethod "java.awt.Frame" "validate") layout)
	)
      (a-msg :time "Refresh: real=~A run=~A"
	     (- (get-internal-real-time) start1)
	     (- (get-internal-run-time) start2))
      res)))

(defmethod show-status ((frame a-data) beep result text1 &optional text2)
  (when text1
    (jcall (jmethod "java.awt.TextField" "setText" "java.lang.String")
	   (status-area frame) 
	   (if (modify-state frame)
	       text1
	     (or text2 text1))))
  (when beep
    (jcall (jmethod "java.awt.Toolkit" "beep") 
	   (jcall (jmethod "AnnotatorPanel" "getToolkit") frame)))
  result)

(defmethod make-annotator-cell ((frame a-data) index row col)
  (let* ((name (name frame))
	 (user name)
	 (top (if (eql row 0) 3 0))
	 (left (if (eql col 0) 3 0))
	 (bot 3)
	 (right 3)
	 (cells (cell-array frame))
	 (cell-parts (aref cells row col))
	 outer)

    ;;	 :title          line 1
    ;;	 :star-dates     line 1
    ;;	 :absolute-number   line 1
    ;;	 :season   line 1
    ;;	 :position-in-season   line 1
    ;;	 :cast           line 2
    ;;	 :locations      line 2
    ;;	 :ships   line 2
    ;;	 :characters     text area
    ;;	 :abstract       text area
    ;;   :director     line 3
    ;;	 :author     line 3

    (when (null cell-parts)
      (let* ((outer-layout (jnew "java.awt.GridBagLayout"))
	     (cell-layout (jnew "java.awt.GridBagLayout"))
	     (cell (jnew "java.awt.Panel" cell-layout))
	     (o (outer-constraints frame))
	     (c (cell-constraints frame))
	     )
	(setf outer (jnew "java.awt.Panel" outer-layout))

	(add-constrained 
	 outer outer-layout o cell
	 :width :remainder :height :remainder
	 :insets (list top left bot right) :grid-fill :both
	 :weight-x 1.0 :weight-y 1.0)
	
	(jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	       outer (cell-outer frame))
	(jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	       cell (cell-color frame))

	(setf cell-parts
	      (list
	       ;; elt 0
	       (add-constrained 
		cell cell-layout c (jnew "java.awt.Label")
		:width :remainder :height 1 :insets (list 0 3 0 3) 
		:grid-fill :both :weight-x 1.0 :weight-y 0.0)
	       ;; elt 1
	       (add-constrained
		cell cell-layout c (jnew "java.awt.Label")
		:width :remainder)
	       ;; elt 2
	       (add-constrained
		cell cell-layout c (jnew "java.awt.Label")
		:width :remainder)
	       ;; elt 3
	       (add-constrained
		cell cell-layout c 
		(jnew "java.awt.TextArea" " " 3 30 1) 
		:width :remainder :grid-fill :both
		:insets (list 3 3 3 3) :weight-x 1.0 :weight-y 1.0)
	       ;; elts 4 5 6 7
	       row col nil nil
	       ;; elt 8
	       (add-constrained
		cell cell-layout c (jnew "java.awt.Label" "-")
		:insets (list 0 3 0 0) 
		:width 1 :grid-fill :none :weight-x 0.0 :weight-y 0.0)
	       ;; elt 9
	       (add-constrained
		cell cell-layout c 
		(jnew "java.awt.TextField" " " 30) 
		:insets (list 0 0 3 3) :weight-y 0.0
		:width :remainder :grid-fill :horizontal :weight-x 1.0)
	       ))

	(add-status-line frame (elt cell-parts 0) "Episode description.")
	(add-status-line frame (elt cell-parts 1) "Episode cast and location.")
	(add-status-line frame (elt cell-parts 2) "Episode characters.")
	(add-status-line frame (elt cell-parts 3) "Abstract of episode.")
	(jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	       (elt cell-parts 3) (cell-color frame))
	
	(add-status-line 
	 frame (elt cell-parts 8) 
	 "Database item: B - Base Value, User Value, ** modified user.")
	(add-status-line frame (elt cell-parts 9) "User comments.")
	(jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	  (elt cell-parts 9)  (input-color frame))

	(jcall "setEditable" (elt cell-parts 3) 
	       (make-immediate-object nil :boolean))
	(jstatic "addTo" "JLTextListener" (elt cell-parts 9))
	(jregister-handler (elt cell-parts 9) 
			   :textValueChanged
			   #'(lambda (&rest x &aux an)
			       (setf x (first x))
			       (setf an (first x))
			       (setf x (second x))
			       (if (eql 0 (elt x 7))
				   (progn (decf (scroll-lock an))
					  (setf (elt x 7) nil))
				 (when (null (elt x 7))
				   (cell-modified an t)
				   (jcall (jmethod "java.awt.Label" "setText" 
						 "java.lang.String") 
					  (elt x 8) "**")
				   (setf (elt x 7) t))))
			   :data (list frame cell-parts))
	
	(setf (aref cells row col) cell-parts)))

    (setf (elt cell-parts 6) index)
    (setf (elt cell-parts 7) 0)
    (jcall (jmethod "java.awt.Label" "setText" "java.lang.String")
	   (elt cell-parts 0) 
	   (format 
	    nil "Episode ~A(~A in season ~A)    ~A    Stardate ~A"
	    (get-annotator-data 
	     :item :index index :user user :prop :absolute-number)
	    (get-annotator-data 
	     :item :index index :user user :prop :position-in-season)
	    (get-annotator-data 
	     :item :index index :user user :prop :season)
	    (get-annotator-data :item :index index :user user :prop :title)
	    (get-annotator-data :item :index index :user user :prop :star-dates)
	    ))
    (jcall (jmethod "java.awt.Label" "setText" "java.lang.String")
	   (elt cell-parts 1)
	   (format nil "Author: ~A           Director: ~A"
		   (format-author
		    (reverse (get-annotator-data 
			      :item :index index :user user :prop :author)))
		   (format-list
		    (get-annotator-data 
		     :item :index index :user user :prop :director))))
    (jcall (jmethod "java.awt.Label" "setText" "java.lang.String")
	   (elt cell-parts 2) 
	   (format nil "Cast: ~A"
		   (format-cast (get-annotator-data 
				 :item :index index :user user :prop :cast))
		   ))
    (jcall (jmethod "java.awt.TextComponent" "setText" "java.lang.String")
	   (elt cell-parts 3) 
	   (format-list 
	    (list*
	     (let ((s (get-annotator-data 
		       :item :index index :user user :prop :characters)))
	       (if s 
		   (format nil "Characters: ~A. " (format-commas s))
		 ""))
	     (let ((s (get-annotator-data 
		       :item :index index :user user :prop :ships)))
	       (if s (format nil "  Ships: ~A. " (format-list s)) ""))
	     (let ((s (get-annotator-data 
		       :item :index index :user user :prop :locations)))
	       (if s (format nil "  Location: ~A. " (format-list s)) ""))
	     (get-annotator-data 
	      :item :index index :user user :prop :abstract))))
      
    (multiple-value-bind (item uval)
	(get-annotator-data :item :index index :user user :prop :comment)
      (jcall (jmethod "java.awt.Label" "setText" "java.lang.String")
	     (elt cell-parts 8) (if uval "U" "B"))
      (jcall (jmethod "java.awt.TextComponent" "setText" "java.lang.String")
	     (elt cell-parts 9) (format-comment (or uval item "---"))))
      
    outer))

(defun format-comment (c)
  (if (and (consp c) (consp (car c)) (stringp (caar c))
	   (null (cdar c)) (stringp (cadr c)) (null (cddr c)))
      (format nil "~A said: ~A" (caar c) (cadr c))
    (format-list c)))

(defun format-cast (c)
  (cond ((and (consp c) (consp (car c)))
	 (format nil "~A~{, ~A~}" 
		 (format-cast (car c)) 
		 (mapcar #'format-cast (cdr c))))
	((and (consp c) (consp (cdr c)))
	 (format nil "~A:~{ ~A~}" (car c) (cdr c)))
	(t (format-list c))))

(defun format-author (c)
  (if (and (consp c) (cdr c))
      (format nil "~A ~A~{, ~A ~A~}" (first c) (second c) (cddr c))
    (format-list c)))

(defun format-commas (c)
  (cond ((stringp c) c)
	((null c) " --- ")
	((consp c) (format nil "~A~{, ~A~}" (car c) (cdr c)))
	(t (format nil "~A" c))))

(defun format-list (c)
  (cond ((stringp c) c)
	((null c) " --- ")
	((consp c) (format nil "~A~{ ~A~}" (car c) (cdr c)))
	(t (format nil "~A" c))))

(defmethod cell-modified ((frame a-data) state)
  (let ((ref (reset-button frame))
	(conf (confirm-button frame)))
    (setf (modify-state frame) state)
    (jcall (jmethod "java.awt.Button" "setLabel" "java.lang.String")
	   ref (if state "Reset" "Refresh"))
    (jcall (jmethod "java.awt.Button" "setLabel" "java.lang.String")
	   conf (if state "Confirm" "Refresh"))
    ))

(defmethod confirm-changes ((frame a-data))
  (let* ((cells (cell-array frame))
	 (rows (rows frame))
	 (cols (cols frame))
	 cell)
    (dotimes (row rows)
      (dotimes (col cols)
	(setf cell (aref cells row col))
	(when (and (elt cell 7) (not (eql 0 (elt cell 7))))
	  (get-annotator-data
	   :item
	   :index (elt cell 6) :prop :comment
	   :user (name frame)
	   :value (jcall (jmethod "java.awt.TextField" "getText") 
			 (elt cell 9))))))
    (handle-page-flip frame :this)))

(defmethod save-changes ((frame a-data))
  (if (modify-state frame)
      (show-status 
       frame t nil "Confirm or Reset modified captions first.")
    (let ((fl (get-annotator-data :write :user (name frame))))
      (show-status 
       frame nil nil
       (if fl
	   (format nil "Wrote file ~A." fl)
	 "There were no changes to save.")))))



(defmethod fill-data-block ((frame a-data)
			    &key cell-generator (index -1))
  ;; data-generator returns some innocuous value when index too high
  (let ((data-block (data-block frame))
	(data-layout (data-layout frame))
	(row 0) (col 0)
	(rows (rows frame)) (cols (cols frame))
	data-cell 
	#-dvc (c (cell-block-constraints frame))
	(begin index))
    (cell-modified frame nil)
    (setf (scroll-lock frame) (* rows cols))
    (loop

     (when (setf data-cell 
		 (funcall cell-generator frame index row col))
       (add-constrained data-block data-layout c data-cell 
			:x col :y row
			:width 1 ;; (if (eql col (1- cols)) :remainder 1)
			:weight-x 1.0 :weight-y 1.0 :grid-fill :both
			:insets (list 0 2 0 0)
			))

     (incf index)
     (incf col) 
     (when (= col cols) 
       (incf row) 
       (when (= row rows) (return))
       (setf col 0)))
    (setf (index frame) begin)
    ))



;;;
;;;
;;; Dedicated interface

(defun run (&optional reload verbose)
  (setf *jlinker-verbose* verbose)
  (setf *a-verbose* verbose)
  (when reload
      (load (compile-file-if-needed "t-java.cl"))
      )
  (or (jlinker-query :verify) 
      (and
       (jlinker-init)
       (let ((frame (jnew (jconstructor "java.awt.Frame" "java.lang.String") "dummy")))
	 (jcall (jmethod "java.awt.Frame" "validate") frame)
	 (jcall (jmethod "java.awt.Frame" "show") frame)
	 (jcall (jmethod "java.awt.Frame" "dispose") frame))      
       ))
  (when *annotator*
    (ignore-errors (jcall "dispose" (first *annotator*)))
    (discard-in-java (first *annotator*))
    (discard-in-java (second *annotator*))
    (setf *annotator* nil))
  (run-annotator)
  (format t "~&;;gen-preload: ~A~%" (jlookup :gen-preload t))
  *annotator*)

(defun run-annotator ()
  ;; run a single annotator in a dedicated Java server
  (or (jlinker-query :verify)
      (jlinker-init :start-java))
  (let* ((true-frame (jnew (jconstructor "java.awt.Frame" "java.lang.String") 
		      "Allegro CL Annotator"))
	 (fr-layout (jnew "java.awt.GridBagLayout"))
	 (gbc (jnew "java.awt.GridBagConstraints"))
	 (frame (jnew "AnnotatorPanel"))
	 exit-button)

    (jstatic "addTo" "com.franz.jlinker.JLWindowAdapter" true-frame)
    (jregister-handler true-frame :windowClosing 
		       #'(lambda (&rest x)
			   (setf (frame-status (first x)) :exit)
			   (mp:open-gate (frame-gate (first x)))
			   (jcall (jmethod "java.awt.Frame" "dispose") 
				  (second x)))
		       :data frame)
    (jcall (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager") 
	   true-frame fr-layout)
    (add-constrained true-frame fr-layout gbc frame 
		     :width :remainder :height :remainder :grid-fill :both
		     :insets (list 5 5 5 5)
		     :weight-x 1.0 :weight-y 1.0)
    (make-annotator-login frame)
    (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	   true-frame (cell-outer frame))
    (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	   frame (cell-color frame))
    (jcall (jmethod "java.awt.Window" "pack") true-frame)

    ;;(jcall (jmethod "java.awt.Component" "setSize" "int" "int") 
    ;;       true-frame  600 350)
    ;;(jcall (jmethod "java.awt.Frame" "validate") true-frame)

    (jcall (jmethod "java.awt.Frame" "show") true-frame)
    (setf exit-button (make-annotator frame))
    (jcall (jmethod "java.awt.Window" "pack") true-frame)
    (jregister-handler 
     exit-button :actionPerformed 
     #'(lambda (&rest x)
	 (jcall (jmethod "java.awt.Frame" "dispose") 
		(first x)))
     :data true-frame)
    
    (setf *annotator* (list true-frame frame))
    ))


;;;
;;;
;;; Applet interface


(defun enable-applets (&key (lisp-port 4321) (java-port -4331) 
			    verbose lisp-file)
  (setf *jlinker-verbose* verbose)
  (setf *a-verbose* verbose)
  ;; fill data base first to avoid process locks
  (setf *data* (make-instance 'annotator-data))
  (jlinker-listen :init-args (list :lisp-file lisp-file :lisp-port lisp-port
				    :java-port java-port
				    ;;:preload nil
				    )))


(defmethod start-applet ((panel a-applet1) aname user)
  (a-msg :progress "start-applet ~A.~A called.~%" aname user)
  (begin-annotator panel aname user))

(defun make-aname (aname name-string)
  (format nil "~A.~A" aname name-string))

(defun begin-annotator (frame aname name-string)
  ;; called from Java client via JavaLinkDist.invoke
  ;;  - start the main application
  (let ((name (make-aname aname name-string)))
    (mp:process-run-function 
	    (format nil "Annotator:~A" name)
	    #'(lambda (*jlinker-connection* name)
		(annotator-process-function frame name))
	    *jlinker-connection* name)))

(defvar *a-server* nil)
(defvar *a-error* nil)
(defun annotator-process-function (frame name &aux v)
  (a-msg :progress "Begin process")
  (setf *a-server* *jlinker-connection*)
  (multiple-value-setq (v *a-error*)
    (ignore-errors
      (when (make-annotator-login frame)
	(a-msg :progress "Calling validate.")
	(jcall (jmethod "java.awt.Panel" "validate") frame)
	(when (make-annotator frame name)
	  (a-msg :progress "Calling validate.")
	  (jcall (jmethod "java.awt.Panel" "validate") frame)))))
  (and (null v) *a-error*
       (a-msg :error "Look in *a-error*."))
  ;; allow only one applet per connection per browser
  (unwind-protect
      (loop
       (sleep 2)
       (when (or (eq (frame-status frame) :exit)
		 (null (ignore-errors (jlinker-query :verify))))
	 (let ((item (dolist (a *all-annotators*)
		       (when (eq frame (third a))
			 (return a)))))
	   (when item
	     (mp:without-scheduling
	      (setf *all-annotators* (remove item *all-annotators*))
	      )))
	 (a-msg :progress "Exiting.")
	 (return)))
    (jlinker-end)))

(defmethod stop-applet ((panel a-applet1) aname uname)
  (a-msg :progress "stop-applet ~A~A called.~%" aname uname)
  nil)

(defmethod destroy-applet ((panel a-applet1) aname uname)
  (let* ((name (make-aname aname uname))
	 (item (dolist (a *all-annotators*)
		 (when (equalp name (second a))
		   (return a)))))
    (a-msg :progress "destroy-applet ~A called.~%" name)
    (if item
	(mp:without-scheduling
	 (a-msg :progress "destroy-applet found entry for ~A.~%" name)
	 (setf (frame-status (third item)) :exit)
	 (mp:open-gate (frame-gate (third item))))
      ;; if cannot find the entry, schedule a disconnect later
      (mp:process-run-function 
       "killer"
       #'(lambda (*jlinker-connection*)
	   (sleep 1) (jlinker-end))
       *jlinker-connection*)
      )))



(defun verbose (&optional (arg nil argp))
  (when argp  
    (setf *jlinker-verbose* arg)
    (setf *a-verbose* arg))
  *a-verbose*)

(defun colors (&optional name r g b)
  (let (old)
    (when name (setf old (assoc name *a-colors*)))
    (and old
	 (typep r '(integer 0 255))
	 (typep g '(integer 0 255))
	 (typep b '(integer 0 255))
	 (setf (cdr old) (list r g b)))
    (cond ((and old (eq r :get) (jlinker-query))
	   (apply #'jmake-new 
		  (jconstructor "java.awt.Color" "int" "int" "int")
		  (cdr old)))
	  (old)
	  (t *a-colors*))))
    
	     




