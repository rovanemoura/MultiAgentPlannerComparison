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
;; Define the dialog :Car-Payments
 
(in-package :cg-user)
 
;; Return the window, creating it the first time or when it's closed.
;; Use only this function if you need only one instance.
(defun car-payments ()
   (find-or-make-application-window :car-payments
    'make-car-payments))
 
;; Create an instance of the window.
;; Use this if you need more than one instance.
(defun make-car-payments ()
   (let ((window-0 
           (make-window :car-payments
             :class 'dialog 
             :owner (development-main-window *system*)
             :title "Car Payments" 
             :state :normal 
             :border nil 
             :left-attachment nil 
             :top-attachment nil 
             :right-attachment nil 
             :bottom-attachment nil 
             :title-bar t 
             :resizable nil 
             :close-button t
             :minimize-button t 
             :scrollbars nil 
             :interior (make-box 500 184 920 455)
             :widgets             
             (list 
               (make-instance 'static-text 
                 :name :total-viewer 
                 :value "1991.28" 
                 :left 234 :top 224 :width 120 :height 32 
                 :help-string "" 
                 :justification :right
                 :font (make-font-ex nil "Courier New" 18 '(:bold)))
               (make-instance 'static-text 
                 :name :static-text24 
                 :value "Total Interest"
                 :left 58 :top 224 :width 133 :height 23 
                 :help-string "" 
                 :font (make-font-ex nil :arial 14 nil))
               (make-instance 'static-text 
                 :name :static-text2 
                 :value "Monthly Payments" 
                 :left 58 :top 195 :width 167 :height 23 
                 :help-string "" 
                 :font (make-font-ex nil :arial 14 nil))
               (make-instance 'static-text 
                 :name :static-text464748 
                 :value "dollars" 
                 :left 163 :top 164 :width 68 :height 17
                 :justification :center
                 :help-string "")
               (make-instance 'static-text 
                 :name :static-text4647 
                 :value "years" 
                 :left 163 :top 100 :width 68 :height 17
		 :justification :center
                 :help-string "")
               (make-instance 'static-text 
                 :name :static-text46 
                 :value "percent" 
                 :left 163 :top 37 :width 68 :height 17
		 :justification :center
                 :help-string "")
               (make-instance 'static-text 
                 :name :payment-viewer 
                 :value "174.23" 
                 :left 234 :top 195 :width 120 :height 32 
                 :justification :right
                 :help-string "" 
                 :font (make-font-ex nil "Courier New" 18 '(:bold)))
               (make-instance 'static-text 
                 :name :static-text37404243 
                 :value 30000 
                 :left 265 :top 164 :width 56 :height 17 
                 :help-string "")
               (make-instance 'static-text 
                 :name :static-text374042 
                 :value 0 
                 :left 90 :top 164 :width 23 :height 17 
                 :help-string "")
               (make-instance 'static-text 
                 :name :static-text374041 
                 :value 20 
                 :left 279 :top 100 :width 23 :height 17 
                 :help-string "")
               (make-instance 'static-text 
                 :name :static-text3740 
                 :value 0 
                 :left 90 :top 100 :width 23 :height 17 
                 :help-string "")
               (make-instance 'static-text 
                 :name :static-text3739 
                 :value 20 
                 :left 279 :top 37 :width 23 :height 17 
                 :help-string "")
               (make-instance 'static-text 
                 :name :static-text37 
                 :value 0 
                 :left 90 :top 37 :width 23 :height 17 
                 :help-string "")
               (make-instance 'editable-text 
                 :name :principal-viewer 
                 :value ""
                 :left 338 :top 130 :width 77 :height 24
                 :help-string "" 
                 :on-change 'set-principal-widget 
                 :delayed nil)
               (make-instance 'editable-text 
                 :name :term-viewer 
                 :value ""
                 :left 338 :top 67 :width 77 :height 24
                 :help-string "" 
                 :on-change 'set-term-widget 
                 :delayed nil)
               (make-instance 'editable-text 
                 :name :interest-viewer 
                 :value ""
                 :left 338 :top 9 :width 77 :height 24
                 :help-string "" 
                 :on-change 'set-interest-viewer 
                 :delayed nil)
               (make-instance 'static-text 
                 :name :principal-label 
                 :value "~Principal" 
                 :left 4 :top 132 :width 66 :height 20
		 :justification :right
                 :help-string "")
               (make-instance 'static-text 
                 :name :term-label 
                 :value "~Term" 
                 :left 26 :top 67 :width 44 :height 20 
		 :justification :right
                 :help-string "")
               (make-instance 'static-text 
                 :name :interest-label 
                 :value "~Interest" 
                 :left 11 :top 10 :width 59 :height 20 
		 :justification :right
                 :help-string "")
               (make-instance 'horizontal-scroll-bar 
                 :name :principal-bar 
                 :value 8637 
                 :left 78 :top 133 :width 241 :height 19 
		 :delayed nil
                 :help-string "" 
                 :on-change 'set-principal-widget 
                 :direction :down
		 :increment 100
		 :page-increment 1000
                 :range (list 0 30000))
               (make-instance 'horizontal-scroll-bar 
                 :name :term-bar 
                 :value 5 
                 :left 78 :top 68 :width 241 :height 19 
		 :delayed nil
                 :help-string "" 
                 :on-change 'set-term-widget 
                 :direction :down
		 :increment 1
		 :page-increment 3
                 :range (list 0 20))
               (make-instance 'horizontal-scroll-bar 
                 :name :interest-bar 
                 :value 90 
                 :left 78 :top 11 :width 241 :height 19
		 :delayed nil
                 :help-string "" 
                 :on-change 'set-interest-bar 
                 :direction :down
		 :increment 1
		 :page-increment 10
                 :range (list 0 200))))))
      window-0))
