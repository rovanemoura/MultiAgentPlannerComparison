
;; $Id: t-inet.cl,v 5.1 2004/02/18 19:19:03 mm Exp $

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

;;; The web-optimized version of the demo

;;; jLinker patch (to avoid fetching class when name will do)

(in-package :net.jlinker)

(defun jclass-string (x) 
  (etypecase x
    (string x)
    ((satisfies dist-object-p) (jclass-name x))))
    
(defun jget-one-cons (class args)
  (jlookup 
   (when (jlookup :query-lookup)
     (list* :constructor (jclass-string class)
	    (mapcar #'jclass-string args)))
   #'(lambda ()
       (apply #'invoke-in-java "getConstructor" class args))))

(defun jget-one-meth (class name args)
  (jlookup
   (when (jlookup :query-lookup)
     (list* :method (jclass-string class) name 
	    (mapcar #'jclass-string args)))
   #'(lambda ()
       (apply #'invoke-in-java "getMethod" class name args))))

(in-package :user)




;; Sample DVC application with applet interface.
;; this is the raw Java version



(defvar *all-annotators* nil)
(defvar *annotator* nil)
(defvar *a-verbose* nil)
(defvar *a-colors* 
  '((:outer 0 0 250) (:cell 230 230 230) (:input 230 200 230)
    (:button 0 200 200)
    ))


(defvar *a-msg-alist* nil)
(defun a-msg (where &rest args &aux place real run)
  (when (or (eq where :error)
	    (and *a-verbose*
		 (or (atom *a-verbose*)
		     (member where *a-verbose*))))
    (case (first args)
      (:begin (or (setf place (assoc (second args) *a-msg-alist*))
		  (push (setf place (list (second args) nil nil)) *a-msg-alist*))
	      (setf (second place) (get-internal-real-time))
	      (setf (third place) (get-internal-run-time))
	      (pop args) (pop args))
      (:end   (when (setf place (assoc (second args) *a-msg-alist*))
		(setf real (- (get-internal-real-time) (second place)))
		(setf run (- (get-internal-run-time) (third place)))
		(format t "~&;;process ~A ~A-~A RealTime=~A   RunTime=~A~%"
			(mp:process-name mp:*current-process*)
			(pop args) (pop args) real run))))
    (when args
      (format t "~&;;process ~A - ~A~%"
	      (mp:process-name mp:*current-process*)
	      (apply #'format nil args)))))





(defclass gbc () 
  ((x :initform nil) (y :initform nil) 
   (w :initform nil) (h :initform nil) 
   (wx :initform nil) (wy :initform nil) 
   (gf :initform nil) (in :initform nil)
   (anc :initform nil)
   (rel :initform nil :allocation :class) (rem :initform nil :allocation :class)
   (hor :initform nil :allocation :class) (vert :initform nil :allocation :class)
   (both :initform nil :allocation :class) (none :initform nil :allocation :class)
   (cen  :initform nil :allocation :class)
   (gridx :accessor grid-x)
   (gridy :accessor grid-y)
   (gridwidth :accessor grid-width)
   (gridheight :accessor grid-height)
   (weightx :accessor weight-x)
   (weighty :accessor weight-y)
   (fill   :accessor grid-fill)
   #+ignore (insets  :accessor insets)
   (anchor  :accessor anchor)
   ))


  
(defmethod set-constraints ((c gbc) 
			    &key x y width height weight-x weight-y 
			    grid-fill insets anchor
			    &aux done)
  (macrolet ((set-slot 
	      (arg slot default jslot &rest tail)
	      `(or (and ,arg
			(let ((v (slot-value c ,slot)))
			  (if (null v)
			      nil
			    (eql v ,arg))))
		   (and (null ,arg) (slot-value c ,slot))
		   (setf (slot-value c ,slot) (or ,arg ,default)
			 done t

			 (,jslot c)
			 (case (or ,arg ,default)
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
			   (otherwise (or ,arg ,default)))))))
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
    (set-slot anchor 'anc :center anchor (:center 'cen "CENTER"))
    (case grid-fill
      ((:horizontal :vertical :both nil) nil)
      (otherwise (setf grid-fill :none)))
    (set-slot grid-fill 'gf :none grid-fill
	      (:horizontal 'hor  "HORIZONTAL")
	      (:vertical   'vert "VERTICAL")
	      (:both       'both "BOTH")
	      (:none 'none "NONE"))
    (or 
	(typecase insets
	  ((integer 0 *) (setf insets (list insets insets insets insets)) nil)
	  (cons (setf insets (list (or (first insets) 0)
				   (or (second insets) 0)
				   (or (third insets) 0)
				   (or (fourth insets) 0)))
		nil)
	  (otherwise (setf insets (or (slot-value c 'in) (list 0 0 0 0))) nil))
	(equal (slot-value c 'in) insets)
	#+ignore (and (null (slot-value c 'in)) (equal insets '(0 0 0 0)))
	(setf (slot-value c 'in) insets 
	      done t
	      #+ignore (insets c)
	      #+ignore (jnew "java.awt.Insets" 
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
   (sa :accessor status-area :initform nil)
   (st :accessor search-text)
   (lk :accessor scroll-lock :initform 0)
   (fs :accessor frame-status :initform nil)
   (fg :accessor frame-gate :initform nil)
   ))

(def-java-class (a-frame "AnnotatorPanel") (a-data)
  () () ())

(def-java-class (a-trek-applet "TrekAppletInet") (a-data)
  () () ())





(defmethod add-constrained0 (lb iv container layout (constraints gbc) item &rest keys)
  (apply #'set-constraints constraints keys)
  (jstatic (jmethod "TrekAppletInet" "addConstrained" "java.awt.Label" "int"
		  "java.awt.Container" "java.awt.GridBagLayout" "java.awt.Component" 
		  "int" "int" "int" "int" "double" "double" 
		  "int" "int")
	   nil
	   lb iv container layout item
	   (grid-x constraints) (grid-y constraints) (grid-width constraints)
	   (grid-height constraints) (weight-x constraints) (weight-y constraints)
	   #+ignore (anchor constraints) 
	   (grid-fill constraints)
	   (+ (* 1000000 (elt (slot-value constraints 'in) 0))
	      (*   10000 (elt (slot-value constraints 'in) 1))
	      (*     100 (elt (slot-value constraints 'in) 2))
	      (elt (slot-value constraints 'in) 3))
	   )
  item)

(defmethod add-constrained1 (container layout (constraints gbc) item &rest keys)
  (apply #'set-constraints constraints keys)
  (jstatic (jmethod "TrekAppletInet" "addConstrained"
		  "java.awt.Container" "java.awt.GridBagLayout" "java.awt.Component" 
		  "int" "int" "int" "int" "double" "double" 
		  "int" "int")
	   nil
	   container layout item
	   (grid-x constraints) (grid-y constraints) (grid-width constraints)
	   (grid-height constraints) (weight-x constraints) (weight-y constraints)
	   #+ignore (anchor constraints) 
	   (grid-fill constraints)
	   (+ (* 1000000 (elt (slot-value constraints 'in) 0))
	      (*   10000 (elt (slot-value constraints 'in) 1))
	      (*     100 (elt (slot-value constraints 'in) 2))
	      (elt (slot-value constraints 'in) 3))
	   )
  item)

(defmethod add-constrained2 ((constraints gbc) item &rest keys)
  (apply #'set-constraints constraints keys)
  (jstatic (jmethod "TrekAppletInet" "addConstrained"
		  "java.awt.Component" 
		  "int" "int" "int" "int" "double" "double" 
		  "int" "int")
	   nil
	   item
	   (grid-x constraints) (grid-y constraints) (grid-width constraints)
	   (grid-height constraints) (weight-x constraints) (weight-y constraints)
	   #+ignore (anchor constraints)
	   (grid-fill constraints)
	   (+ (* 1000000 (elt (slot-value constraints 'in) 0))
	      (*   10000 (elt (slot-value constraints 'in) 1))
	      (*     100 (elt (slot-value constraints 'in) 2))
	      (elt (slot-value constraints 'in) 3))
	   )
  item)

(defmethod add-constrained3 ((constraints gbc) item s1 s2 &rest keys)
  (apply #'set-constraints constraints keys)
  (jstatic (jmethod "TrekAppletInet" "addConstrained"
		  "java.awt.Component" 
		  "int" "int" "int" "int" "double" "double" 
		  "int" "int"
		  "java.lang.String" "java.lang.String"
		  )
	   nil
	   item
	   (grid-x constraints) (grid-y constraints) (grid-width constraints)
	   (grid-height constraints) (weight-x constraints) (weight-y constraints)
	   #+ignore (anchor constraints)
	   (grid-fill constraints)
	   (+ (* 1000000 (elt (slot-value constraints 'in) 0))
	      (*   10000 (elt (slot-value constraints 'in) 1))
	      (*     100 (elt (slot-value constraints 'in) 2))
	      (elt (slot-value constraints 'in) 3))
	   (or s1 "") (or s2 "")
	   )
  item)
  

(defun make-annotator-login (outer)
  (a-msg :progress :begin 'make-annotator-login "make login")
  (let* ((outer-layout (jnew "java.awt.GridBagLayout"))
	 (gbc (make-instance 'gbc))
	 fr-layout
	 frame
	 login go message lb)

    (jcall (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager") 
	   outer outer-layout)
    (add-constrained1 outer outer-layout gbc
		      (setf lb (jnew (jcons "java.awt.Label" "java.lang.String") 
				"Preparing login screen, please be patient..."))
		      :x :relative :y :relative :width :remainder :height :remainder
		      :insets 0 :grid-fill :both)
    (jcall (jmethod "java.awt.Panel" "validate") outer)

    (setf (frame-gate outer) (mp:make-gate nil)
	  (item-count outer) (get-annotator-data :item)
	  (constraints outer) gbc
	  (cell-outer outer) (colors :outer :get)
	  (cell-color outer) (colors :cell  :get)
	  (input-color outer) (colors :input :get)
	  )
    (setf fr-layout (jnew "java.awt.GridBagLayout"))
    (setf frame (jnew (jcons "java.awt.Panel" "java.awt.LayoutManager")
		      fr-layout))
    (add-constrained0 lb 32 frame fr-layout gbc
		     (jnew (jcons "java.awt.Label" "java.lang.String") 
			   "Please enter a login name (no spaces):")
		     :x :relative :y :relative :width :remainder :height 1
		     :insets (list 10 10 0 10)
		     :grid-fill :none)

    (setf login (jnew (jcons "java.awt.TextField" "int") 20))
    (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	   login (input-color outer))
    (add-constrained2 gbc login :insets (list 3 10 3 10))

    (setf go (jnew (jcons "java.awt.Button" "java.lang.String") "Begin"))
    (jregister-handler go :actionPerformed
		       #'(lambda (&rest x &aux (frame (first x))) 
			   (setf (name frame)
				 (string-trim 
				  " " 
				  (jcall 
				   (jmethod "java.awt.TextField" "getText")
				   (search-text frame))))
			   (mp:open-gate (frame-gate frame)))
		       :data outer)
    (add-constrained2 gbc go :insets (list 3 10 3 10))

    (setf message (jnew (jcons "java.awt.TextField" "int") 40))
    (jcall (jmethod "java.awt.TextComponent" "setEditable" "boolean")
	   message (make-immediate-object nil :boolean))
    (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	   message (cell-color outer))
    (add-constrained2 gbc message :insets (list 3 10 10 10))

    
    (setf (search-text outer) login)
    (jstatic 
     (jmethod "com.franz.jlinker.JLActionListener" "addTo" "java.awt.Button")
     nil go)
    
    (setf (data-block outer) message
	  (data-layout outer) go)  
    (jcall (jmethod "java.awt.Container" "removeAll") outer)
    (add-constrained1 outer outer-layout gbc frame
		      :x :relative :y :relative :width :remainder :height :remainder
		      :insets 0 :grid-fill :both)
    (a-msg :progress :end 'make-annotator-login)
    t))

(defun make-annotator (frame &optional aname)
  (a-msg :progress  "make-annotator waiting for login")
  (let* (buttons fr-layout data-layout data-block
		 (rows 3) (cols 1) exit-button lb)

    (let ((message (data-block frame))
	  (go (data-layout frame))
	  m)
      ;; first we wait for a name to be entered
      (mp:close-gate (frame-gate frame))
      (loop
       (when (eq :exit (frame-status frame))
	 (return-from make-annotator nil))
       (if (mp:gate-open-p (frame-gate frame))
	   (if (or (and (equal (name frame) "")
			(setf m "Login field is blank!"))
		   (and (not (every #'alpha-char-p (name frame)))
			(setf m "Login name should be all letters!"))
		   (and (assoc (name frame) *all-annotators* :test #'equalp)
			(setf m "Login is already active, please pick another."))
		   )
	       (progn 
		 (jcall (jmethod "java.awt.TextField" "setText" 
			       "java.lang.String")
			message m)
		 (mp:close-gate (frame-gate frame)))
	     (return))
	 ;; do something useful while waiting
	 (or (and (null fr-layout)
		  (setf fr-layout (jnew "java.awt.GridBagLayout")))
	     (and (null data-layout)
		  (setf data-layout (jnew "java.awt.GridBagLayout")))
	     (and (null data-block)
		  (setf data-block (jnew (jcons "java.awt.Panel" 
						"java.awt.LayoutManager")
					 data-layout)))
	     (mp:with-timeout 
	      (2 :timeout)
	      (mp:process-wait 
	       "login" #'mp:gate-open-p (frame-gate frame)))
	     )
	 )
       (or (ignore-errors (jlinker-query)) (return-from make-annotator nil))
       )
      (jregister-handler go :actionPerformed nil))

    (when aname
      (push (list (name frame) aname frame) *all-annotators*))
    (a-msg :connect "User login accepted: ~A" (name frame))
    (a-msg :progress :begin 'make-annotator "make main frame.")
    
    (jcall (jmethod "java.awt.Container" "removeAll") frame)

    (or (and (null fr-layout)
	     (setf fr-layout (jnew "java.awt.GridBagLayout")))
	(and (null data-layout)
	     (setf data-layout (jnew "java.awt.GridBagLayout")))
	(and (null data-block)
	     (setf data-block (jnew (jcons "java.awt.Panel" 
					   "java.awt.LayoutManager")
				    data-layout))))
    
    (jcall (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager") 
	   frame fr-layout)
    (setf (data-block frame) data-block
	  (data-layout frame) data-layout
	  (rows frame) rows
	  (cols frame) cols
	  (cell-array frame) (make-array (list rows cols))
	  (cell-constraints frame) 
	  (make-instance 'gbc)
	  (cell-block-constraints frame) 
	  (make-instance 'gbc)
	  (outer-constraints frame) 
	  (make-instance 'gbc)
	  )

    (setf lb
	  (jnew (jcons "java.awt.Label" "java.lang.String")
		"Preparing viewing screen, please be patient..."))
    (add-constrained0
     lb 103 frame fr-layout (make-instance 'gbc)
     lb 
     :x 0 :y 0 :width :remainder :height 1 :weight-y 0.0)
    (jcall (jmethod "java.awt.Panel" "validate") frame)

    (multiple-value-setq (buttons exit-button) (make-annotator-controls frame))

    (a-msg :progress :begin 'data-block "data-block")
    (fill-data-block frame :cell-generator #'make-annotator-cell
		     :index -1)
    (setf (scroll-lock frame) 0)
    (a-msg :progress :end 'data-block)

    (make-annotator-body frame fr-layout buttons data-block lb)
     
    (jcall (jmethod "java.awt.Label" "setText" "java.lang.String")
	   lb (format nil "Annotator for user ~A" (name frame)))
    (a-msg :progress :end 'make-annotator)
    exit-button))


(defmethod make-annotator-controls ((frame a-data))
  (a-msg :progress :begin 'make-controls "make-controls")
  (let* ((gbc (constraints frame))
	 (button-layout (jnew "java.awt.GridBagLayout"))
	 (buttons (jnew (jcons "java.awt.Panel" "java.awt.LayoutManager")
			button-layout))
	 button area exit-button
	 (bg (colors :button :get))
	 )
    (flet ((add-button (label action status1 &optional status2)
		       (add-constrained3
			gbc
			(setf button (jnew (jcons "java.awt.Button" "java.lang.String")
					   label))
			status1 status2
			)
		       (jstatic 
			(jmethod "com.franz.jlinker.JLActionListener" 
			       "addTo" "java.awt.Button")
			nil button)
		       (jregister-handler 
			button :actionPerformed action :data frame)
		       (jcall (jmethod "java.awt.Component" "setBackground" 
				     "java.awt.Color")
			      button bg)
		       ))

      (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	     buttons (cell-color frame))
      (add-constrained1
       buttons button-layout gbc
       (jnew (jcons "java.awt.Label" "java.lang.String") "Controls:")
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
      
      (add-constrained3
       gbc
       (setf area (jnew (jcons "java.awt.TextField" "int") 15))
       "Type a search string here." "")
      (setf (search-text frame) area)
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
		      (jcall (jmethod "java.awt.Container" "removeAll") (first x)))
		  "Disconnect from database.")
      (setf exit-button button)

      (a-msg :progress :end 'make-controls)
      (values buttons exit-button))))

(defmethod make-annotator-body ((frame a-data) fr-layout buttons data-block lb)
  (let* ((gbc (constraints frame)) w)
    (a-msg :progress :begin 'make-body "make-body.")

    (jcall (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager") 
	   frame fr-layout)

    (set-constraints gbc :insets (list 0 0 0 0))

    (add-constrained1
     frame fr-layout gbc data-block :x 0 :y 1 :width :relative :height 1 
     :weight-x 1.0 :weight-y 1.0 :grid-fill :both)
      
    (add-constrained2
     gbc buttons
     :x 1 :y 1 :width 1 :height 1 :weight-x 0.0 :grid-fill :none)
      
    (add-constrained2
     gbc (setf w (jnew "java.awt.TextField"))
     :x 0 :y 2 :width :remainder :height 1 :weight-y 0.0
     :weight-x 1.0 :grid-fill :horizontal :insets 2)
    (jcall (jmethod "java.awt.TextComponent" "setEditable" "boolean")
	   w (make-immediate-object nil :boolean))
    (jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	   w (cell-color frame))
    (setf (status-area frame) w)
    (jfield "TrekAppletInet" "statusArea" nil w)
    (add-status-line lb "Main label.")
  
    (a-msg :progress :end 'make-body)
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

(defun add-status-line (component &rest text)
  (jstatic (jmethod "TrekAppletInet" "addStatusLine" 
		  "java.awt.Component"
		  "java.lang.String" "java.lang.String")
	   nil
	   component (or (first text) "") (or (second text) "")))







(defmethod handle-page-flip (layout name)
  (declare (ignore button event descriptor))
  (case name (:this (setf (scroll-lock layout) 0)))
  (if (not (eql 0 (scroll-lock layout)))
      (show-status layout t nil "Slow down!!!")
    (let* (new
	   (rows (rows layout))
	   (cols (cols layout))
	   (index (index layout))
	   (res t))
      (a-msg :progress :begin 'handle-page-flip)
      (jcall (jmethod "TrekAppletInet" "setBusy") nil)
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
	(jcall (jmethod "TrekAppletInet" "setReady") nil)
	(setf (scroll-lock layout) 0)
	(jcall (jmethod "java.awt.Frame" "validate") layout)
	)
      (a-msg :progress :end 'handle-page-flip)
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
	     (cell (jnew (jcons "java.awt.Panel" "java.awt.LayoutManager")
			 cell-layout))
	     (o (outer-constraints frame))
	     (c (cell-constraints frame))
	     )
	(setf outer (jnew (jcons "java.awt.Panel" "java.awt.LayoutManager")
			  outer-layout))

	(add-constrained1 
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
	       (add-constrained1
		cell cell-layout c (jnew "java.awt.Label")
		:width :remainder :height 1 :insets (list 0 3 0 3) 
		:grid-fill :both :weight-x 1.0 :weight-y 0.0)
	       ;; elt 1
	       (add-constrained3
		c (jnew "java.awt.Label") "Episode cast and location." ""
		:width :remainder)
	       ;; elt 2
	       (add-constrained3
		c (jnew "java.awt.Label") "Episode description." ""
		:width :remainder)
	       ;; elt 3
	       (add-constrained3
		c 
		(jnew (jcons "java.awt.TextArea" 
			     "java.lang.String" "int" "int" "int")
		      " " 3 30 1) 
		"Abstract of episode." ""
		:width :remainder :grid-fill :both
		:insets (list 3 3 3 3) :weight-x 1.0 :weight-y 1.0)
	       ;; elts 4 5 6 7
	       row col nil nil
	       ;; elt 8
	       (add-constrained3
		c (jnew (jcons "java.awt.Label" "java.lang.String")
			"-")
		"Database item: B - Base Value, User Value, ** modified user." ""
		:insets (list 0 3 0 0) 
		:width 1 :grid-fill :none :weight-x 0.0 :weight-y 0.0)
	       ;; elt 9
	       (add-constrained3
		c 
		(jnew (jcons "java.awt.TextField" "java.lang.String" "int")
		      " " 30) 
		"User comments." ""
		:insets (list 0 0 3 3) :weight-y 0.0
		:width :remainder :grid-fill :horizontal :weight-x 1.0)
	       ))

	(add-status-line (elt cell-parts 0) "Episode description.")

	(jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	       (elt cell-parts 3) (cell-color frame))
	
	(jcall (jmethod "java.awt.Component" "setBackground" "java.awt.Color")
	  (elt cell-parts 9)  (input-color frame))

	(jcall (jmethod "java.awt.TextComponent" "setEditable" "boolean")
	       (elt cell-parts 3) 
	       (make-immediate-object nil :boolean))
	(jstatic (jmethod "JLTextListener" "addTo" "java.awt.TextComponent") nil
		 (elt cell-parts 9))
	(jregister-handler (elt cell-parts 9) 
			   :textValueChanged
			   #'(lambda (&rest x &aux an)
			       (setf x (first x))
			       (setf an (first x))
			       (setf x (second x))
			       (if (eql 0 (elt x 7))
				   (setf (elt x 7) nil)
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
      
    (multiple-value-bind (item uval)
	(get-annotator-data :item :index index :user user :prop :comment)
      (jcall (jmethod "TrekAppletInet" "setCellText"
		    "java.awt.Label" "java.lang.String"
		    "java.awt.Label" "java.lang.String"
		    "java.awt.Label" "java.lang.String"
		    "java.awt.TextComponent" "java.lang.String"
		    "java.awt.Label" "java.lang.String"
		    "java.awt.TextComponent" "java.lang.String"
		    )
	     nil
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
	      )

	     (elt cell-parts 1)
	     (format nil "Author: ~A           Director: ~A"
		     (format-author
		      (reverse (get-annotator-data 
				:item :index index :user user :prop :author)))
		     (format-list
		      (get-annotator-data 
		       :item :index index :user user :prop :director)))

	     (elt cell-parts 2)
	     (format nil "Cast: ~A"
		     (format-cast (get-annotator-data 
				   :item :index index :user user :prop :cast))
		     )
	   
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
		:item :index index :user user :prop :abstract)))

	     (elt cell-parts 8) (if uval "U" "B")
	     (elt cell-parts 9) (format-comment (or uval item "---"))
	     ))
      
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
    (jstatic (jmethod "TrekAppletInet" "cellModified"
		    "int" "java.awt.Button" "java.awt.Button")
	     nil
	     (if state 1 0) ref conf)
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
    (setf (scroll-lock frame) 1)
    (loop
     (when (setf data-cell 
		 (funcall cell-generator frame index row col))
       ;; cannot use add-constrained2 here because cell-generator
       ;;  uses it too
       (add-constrained1 data-block data-layout c data-cell 
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
       (let ((frame (jnew (jcons "java.awt.Frame" "java.lang.String") "dummy")))
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
      (jlinker-init :start-java :preload nil))
  (a-msg :progress :begin :preload)
  (jlookup :preload)
  (a-msg :progress :end :preload)
  (let* ((true-frame (jnew (jcons "java.awt.Frame" "java.lang.String") 
		      "Allegro CL Annotator"))
	 (fr-layout (jnew "java.awt.GridBagLayout"))
	 (gbc (make-instance 'gbc))
	 (frame (jnew "AnnotatorPanel"))
	 exit-button)

    (jstatic (jmethod "com.franz.jlinker.JLWindowAdapter" "addTo" "java.awt.Window")
	     nil true-frame)
    (jregister-handler true-frame :windowClosing 
		       #'(lambda (&rest x)
			   (setf (frame-status (first x)) :exit)
			   (mp:open-gate (frame-gate (first x)))
			   (jcall (jmethod "java.awt.Frame" "dispose") 
				  (second x)))
		       :data frame)
    (jcall (jmethod "java.awt.Container" "setLayout" "java.awt.LayoutManager") 
	   true-frame fr-layout)
    (make-annotator-login frame)
    (add-constrained1 true-frame fr-layout gbc frame 
		     :width :remainder :height :remainder :grid-fill :both
		     :insets (list 5 5 5 5)
		     :weight-x 1.0 :weight-y 1.0)
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
				    :preload nil
				    )))


(defmethod start-applet ((panel a-trek-applet) aname user)
  (a-msg :progress "start-applet ~A.~A called.~%" aname user)
  (begin-annotator panel aname user))

(defun make-aname (aname name-string)
  (format nil "~A.~A" aname name-string))

(defvar *a-counter* 0)
(defun begin-annotator (frame aname name-string)
  ;; called from Java client via JavaLinkDist.invoke
  ;;  - start the main application
  (let ((name (make-aname aname name-string)))
    (mp:process-run-function 
	    (format nil "an~A" (incf *a-counter*))
	    #'(lambda (*jlinker-connection* name *a-msg-alist*)
		(annotator-process-function frame name))
	    *jlinker-connection* name nil)))

(defvar *a-server* nil)
(defvar *a-error* nil)
(defun annotator-process-function (frame name &aux v)
  (a-msg :connect "Begin process ~A" name)
  (setf *a-server* *jlinker-connection*)
  (when (< 5 *jlinker-retry-delay*) (sleep 1))

  (let* ((sr (jlinker-slot :server))
	 (so (when sr (net.jlinker::socket sr)))
	 (host (when so (socket:remote-host so)))
	 (hname (when host 
		  (or (ignore-errors (socket:ipaddr-to-hostname host))
		      (ignore-errors (socket:ipaddr-to-dotted host))
		      (format nil "~X" host))))
	 (port (when so (socket:remote-port so))))
    (a-msg :connect "...connected from ~A at port ~A"
	   (or hname host) port)
    (multiple-value-bind (sec min hour) (get-decoded-time)
      (a-msg :connect "...at ~2,'0D:~2,'0D:~2,'0D" hour min sec)))

  ;;(a-msg :progress :begin :preload "Begin preload")
  ;;(jlookup :preload)
  ;;(a-msg :progress :end :preload)
  (jlookup :new) ;; need to turn on tables if preload=nil

  (multiple-value-setq (v *a-error*)
    (ignore-errors
      (when (make-annotator-login frame)
	(a-msg :progress "Calling validate.")
	(jcall (jmethod "java.awt.Panel" "validate") frame)
	(when (make-annotator frame name)
	  (a-msg :progress "Calling validate.")
	  (jcall (jmethod "java.awt.Panel" "validate") frame)))))
  (when *a-error*
       (a-msg :error "Error: ~A" *a-error*)
       (a-msg :error "Look in *a-error*. v=~S" v))
  ;; allow only one applet per connection per browser
  (unwind-protect
      (loop
       (sleep 2)
       (when (or (eq (frame-status frame) :exit)
		 (null (ignore-errors (jlinker-query))))
	 (let ((item (dolist (a *all-annotators*)
		       (when (eq frame (third a))
			 (return a)))))
	   (when item
	     (mp:without-scheduling
	      (setf *all-annotators* (remove item *all-annotators*))
	      )))
	 (a-msg :connect "Exiting.")
	 (return)))
    (jlinker-end)))

(defmethod stop-applet ((panel a-trek-applet) aname uname)
  (a-msg :progress "stop-applet ~A~A called.~%" aname uname)
  nil)

(defmethod destroy-applet ((panel a-trek-applet) aname uname)
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
		  (jcons "java.awt.Color" "int" "int" "int")
		  (cdr old)))
	  (old)
	  (t *a-colors*))))
    
	     




