#|
======================
File: diret-subscription.lisp
Author: Vladimir Kulyukin
Copyright (c) 1999

Comments and bugs to vkulyukin@cs.depaul.edu 
======================
|#
 
(in-package :cg-user)

(export '(diret-subscription-dialog))

(defclass diret-subscription-dialog (devel::shrinking-dialog) ())

(defun make-diret-subscription-window (&key (parent (development-main-window *system*)) 
                                        window-interior
                                        (name :dir-subscription-window) 
                                        (title "DIRET 1.0: Source Subscription")
                                        &allow-other-keys)
  (declare (ignore window-interior))
  (let ((window-0 
         (make-window name
           :device 'diret-subscription-dialog
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
           :winhelp-string "DIRET 1.0: Source Subscription"
           :widgets (subscription-widgets))))
     window-0))

(defun subscription-widgets ()
  (list
   (make-instance 'button 
		  :name :subscribe
		  :title "~Unsubscribe" 
		  :left 215 :top 270 :width 82 :height 27 
		  :tabstop nil 
		  :groupstart nil
		  :top-attachment :bottom 
		  :bottom-attachment :bottom 
		  :state :normal 
		  :on-change 'unsubscribe-source
		  )
   (make-instance 'default-button 
		  :name :unsubscribe
		  :title "~Subscribe"
		  :value t 
		  :left 115 :top 270 :width 82 :height 27 
		  :tabstop nil 
		  :groupstart nil 
		  :top-attachment :bottom 
		  :bottom-attachment :bottom 
		  :state :normal
		  :on-change 'subscribe-source
		  )
   (make-instance 'multi-line-editable-text 
		  :name :source-description 
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
		  :name :source-list 
		  :title "Source List" 
		  :value nil
		  :left 10 :top 39 :width 420 :height 124 
		  :tabstop t 
		  :groupstart t 
		  :right-attachment :right 
		  :bottom-attachment :scale 
		  :state :normal
		  :on-change 'source-list-on-change 
		  :key 'source-list-key 
		  :range nil)
   ))

;;; end-of-file

