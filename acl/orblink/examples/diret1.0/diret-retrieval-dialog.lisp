#|
======================
File: diret-retrieval-dialog.lisp
Authors: Vladimir Kulyukin, Alex Lifshits 
Description: GUI for accessing retrievals. 
Copyright (c) 1999 

Comments and bugs to vkulyukin@cs.depaul.edu 
======================
|#

(in-package :cg-user)

(defclass diret-retrieval-dialog (devel::shrinking-dialog) ())

(defun make-diret-retrieval-window (&key (parent (development-main-window *system*)) 
                                     window-interior
                                     (name :dir-retrievals-window) 
                                     (title "DIRET 1.0: Retrievals")
                                     &allow-other-keys)
  (declare (ignore window-interior))
  (let ((window-0 
          (make-window name
            :device 'diret-retrieval-dialog
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
            :height 370		      
            :width  445
            :winhelp-string "DIRET 1.0: Retrievals"
            :widgets (retrieval-widgets))))
     window-0))

(defun retrieval-widgets ()
  (list
   (make-instance 'button 
     :name :examine
     :title "~Examine" 
     :left 215 :top 270 :width 82 :height 27 
     :tabstop nil 
     :groupstart nil
     :top-attachment :bottom 
     :bottom-attachment :bottom 
     :state :normal 
     :on-change 'examine-retrieval
     )
   (make-instance 'default-button 
     :name :describe
     :title "~Describe"
     :value t 
     :left 115 :top 270 :width 82 :height 27 
     :tabstop nil 
     :groupstart nil 
     :top-attachment :bottom 
     :bottom-attachment :bottom 
     :state :normal
     :on-change 'describe-retrieval
     )
   (make-instance 'multi-line-editable-text 
     :name :retrieval-description 
     :value "" 
     :left 10 :top 180 :width 420 :height 75 
     :border :static
     :tabstop nil 
     :groupstart t 
     :top-attachment :scale 
     :right-attachment :right 
     :bottom-attachment :bottom 
     :read-only t 
     :border :static 
     :scrollbars :vertical)
   (make-instance 'single-item-list 
     :name :retrieval-list 
     :title "Retrieval List" 
     :value nil
     :left 10 :top 39 :width 420 :height 124 
     :tabstop t 
     :groupstart t 
     :right-attachment :right 
     :bottom-attachment :scale 
     :state :normal
     :range nil)
   ))

(defun make-diret-examine-window (&key (parent (development-main-window *system*)) 
                                   window-interior
                                   (name :dir-examine-window) 
                                   (title "DIRET 1.0: Examine Retrieval")
                                   &allow-other-keys)
  (declare (ignore window-interior))
  (let ((window-0 
          (make-window name
            :device 'diret-retrieval-dialog
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
            :height 370		      
            :width  445
            :winhelp-string "DIRET 1.0: Retrievals"
            :widgets (examine-widgets))))
     window-0))

(defun examine-widgets ()
  (list
   (make-instance 'multi-line-editable-text 
     :name :examine-retrieval 
     :value "" 
     :left 10 :top 10 :width 420 :height 300 
     :border :static
     :tabstop nil 
     :groupstart t 
     :top-attachment :scale 
     :right-attachment :right 
     :bottom-attachment :bottom 
     :read-only t 
     :border :static 
     :scrollbars :vertical)
   ))

;;; end-of-file