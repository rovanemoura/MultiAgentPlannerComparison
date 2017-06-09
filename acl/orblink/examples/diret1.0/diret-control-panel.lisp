#|
==============================
File: diret-control-panel.lisp 
Authors: Vladimir Kulyukin,
         Alex Lifshits 
Description: Diret's GUI.

Acknowledgments: Uses techniques from
several example files provided with
the distribution of ACL 5.0 Enterprise Edition.

Comments and bugs to vkulyukin@cs.depaul.edu 
============================== 
|# 
 
(in-package :cg-user)


(defparameter *diret-bitmap*
  "C:\\Program Files\\acl50\\ex\\cg\\misc\\Franz.bmp")

(defparameter *diret-cp* nil)

(defun diret-control-panel ()
   "Create Diret 1.0 Control Panel"
   (find-or-make-application-window :diret-form 'make-diret-control-panel))
 
(defun make-diret-control-panel (&key (parent (development-main-window *system*))
                                      (exterior (make-box 225 154 929 521)) 
                                      (name :diret-form)
                                      (title "DIRET 1.0: Control Panel") form-p)
   (let* ((diret-form (make-diret-form name parent exterior title form-p))
          (diret-tool-bar (make-diret-toolbar diret-form)))
      diret-form))

(defun make-diret-form (name parent exterior title form-p)
   (make-window name
     :parent parent
     :device 'dialog
     :widgets
     (list (make-instance 'static-picture
             :height 137
             :left 498
             :name :static-picture-2
             :pixmap-name nil
             :pixmap-source *diret-bitmap*
             :pixmap-use-handle nil
             :top 159
             :unavailable-color-mapper
             (list (cons black gray) (cons dark-gray gray)
               (cons dark-blue gray) (cons dark-green gray)
               (cons dark-red gray) (cons dark-cyan gray)
               (cons dark-yellow gray)
               (cons dark-magenta gray) (cons red dark-red)
               (cons green dark-green) (cons blue dark-blue)
               (cons yellow dark-yellow)
               (cons cyan dark-cyan)
               (cons magenta dark-magenta))
             :width 158
             :font '#.(make-font-ex nil :|MS SANS SERIF| 13 nil))
       (make-instance 'static-text
         :background-color white
         :border :plain
         :font
         (make-font-ex :swiss :|TW CEN MT| 15 nil)
         :height 264
         :left 32
         :name :diret-message
         :top 40
         :value "Welcome to DIRET 1.0!"
         :width 432
         :hidden-p nil
         :old-window nil
         :scrollbars nil))
     :exterior exterior
     :background-color light-gray
     :border :frame
     :close-button t
     :cursor-name :arrow-cursor
     :maximize-button t
     :minimize-button t
     :name :diret-form
     :package-name :common-graphics-user
     :pop-up nil
     :resizable t
     :scrollbars t
     :state :shrunk
     :status-bar nil
     :system-menu t
     :title title
     :title-bar t
     :toolbar t
     :form-p form-p
     :help-string nil
     :package-name :common-graphics-user))

(defun make-diret-toolbar (parent)
   (make-window :toolbar
     :parent parent
     :device 'toolbar
     :pop-up nil
     :widgets
     (list 
       (make-instance 'button
         :height 25
         :left 368
         :name :subscription
         :title "Subscription"
         :on-click 'invoke-diret-subscription
         :top 5
         :width 89)
       (make-instance 'button
         :height 25
         :left 250
         :name :retrieval
         :title "Retrievals"
         :on-click 'get-retrievals
         :top 5
         :width 89)
       (make-instance 'button
         :height 25
         :left 133
         :name :run
         :on-click 'invoke-diret-editor
         :title "Editor"
         :top 5
         :width 89)
       (make-instance 'button
         :height 25
         :left 12
         :name :display
         :on-click 'invoke-document-processor
         :title "Doc Processor"
         :top 5
         :width 89))
     :exterior
     (make-box-relative 0 0 696 35)
     :border :static
     :close-button nil
     :cursor-name :arrow-cursor
     :maximize-button nil
     :minimize-button nil
     :name :toolbar
     :resizable nil
     :scrollbars nil
     :state :normal
     :status-bar nil
     :system-menu nil
     :title "Toolbar"
     :title-bar nil
     :toolbar nil))

(defclass diret-doc-dialog (devel::shrinking-dialog) ())

(defun make-diret-doc-dialog (&key (parent (development-main-window *system*)) 
                               window-interior
                               (name :dir-doc-dialog) 
                               (title "DIRET 1.0: User Documents")
                               &allow-other-keys)
  (declare (ignore window-interior))
  (let ((window-0 
         (make-window name
           :device 'diret-doc-dialog
           :parent parent 
           :title title 
           :state  :normal
           :border :frame 
           :left-attachment nil 
           :top-attachment  nil 
           :right-attachment  nil 
           :bottom-attachment nil 
           :movable t 
           :resizable nil 
           :closable t 
           :minimize-button t 
           :scrollbars nil 
           :overlapped nil 
           :pop-up nil 
           :height 270		      
           :width  345
           :winhelp-string "DIRET 1.0: User Documents"
           :widgets (doc-dialog-widgets))))
     window-0))

(defun doc-dialog-widgets ()
  (list
   (make-instance 'default-button 
		  :name :subscribe
		  :title "~Process Document" 
		  :left 117 :top 200 :width 110 :height 35 
		  :tabstop nil 
		  :groupstart nil
		  :top-attachment :bottom 
		  :bottom-attachment :bottom 
		  :state :normal 
		  :on-change 'process-saved-document
		  )
   (make-instance 'single-item-list 
     :name :document-list 
     :title "Document List" 
     :value nil
     :left 10 :top 39 :width 320 :height 150 
     :tabstop t 
     :groupstart t 
     :right-attachment :right 
     :bottom-attachment :scale 
     :state :normal
     :on-change 'document-list-on-change 
     :key 'document-list-key 
     :range nil)
   ))



;;; end-of-file

                        