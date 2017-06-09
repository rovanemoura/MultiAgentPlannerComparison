;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2012 Franz Inc, Oakland, CA  All rights reserved.
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


;;;; Runtime Analyzer Dialog

(in-package :cg-user)

(eval-when (compile load eval)
  (require :prof))

(defun run-profile-example ()
  
  (when (standalone-application (app *system*))
    (pop-up-message-dialog 
     (screen *system*) "Requires the IDE"
     #.(format nil "This example uses the runtime analyzer and other facilities ~
                    that exist only in the IDE, and so it cannot run ~
                    without the IDE running.")
     error-icon :~ok)
    (exit -1))
  
  ;; Run the test function below to create and resize a window,
  ;; calling it inside a with-profiling macro call to profile
  ;; all functions called within that dynamic extent.
  (prof:with-profiling (:type :time :count t)
    
    #+maybe ;; This doesn't produce many nodes
    (let* ((window (make-window :profile-test
                     :class 'frame-window
                     :owner (development-main-window *system*)
                     :exterior (make-box 20 420 400 450)
                     :title "This activity is being profiled ...")))
      
      ;; Resize the window a bunch of times.    
      (dotimes (k 15)  
        (dotimes (j 20)
          (resize-window
           window
           (make-position (+ 300 (* j 5))
                          (+ 30 (* j 3))))
          (update-window window))
        (dotimes (j 20)
          (resize-window
           window
           (make-position (+ 300 (* (- 20 j) 5))
                          (+ 30 (* (- 20 j) 3))))
          (update-window window))))
    
    (dotimes (j 40)
      (ide.class-browser:display-class-outline (find-class 'menu))
      (update-window (ide.class-browser::class-browser))))
  
  ;; Show the runtime analyzer results dialog and "click" its Update
  ;; button to show the results of the profile.
  (let* ((dialog (ide.base:profile-dialog-command (screen *system*))))
    (toggle-button (find-component :show dialog))))

#+run-example (run-profile-example)

