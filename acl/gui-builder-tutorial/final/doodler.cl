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
;; Code for the dialog :form2

(in-package :cg-user)

;; added ch1 step 10
(defclass doodler (bitmap-window) ; ch4 step 16
    ;; added ch3 step24
    ((doodler-curve-dialog
      :accessor doodler-curve-dialog
      :initform nil)
     ;; added ch6 step 55
     (doodler-background-palette
      :initform nil
      :accessor doodler-background-palette)))

;; added ch3 step 29
(defun doodler-toolbar-click (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   ;; Do the action only when a button is being pressed (not unpressed)
   (when new-value
     (let ((doodler (parent (parent widget))))
        (case (first new-value)
          (:erase ; ch4 step 23
            (erase-window doodler)) ; ch4 step 23
          (:curve
            (show-curve-dialog doodler))
          (:scroll-to-center ; ch 4 step 20
            (scroll-to-center doodler)) ; ch4 step 20
          (t
            nil))))
   (not new-value))

;; added ch3 step 30
;;; chee   14mar00 don't position the curve dialog
;;;        off the left edge of the screen
(defmethod show-curve-dialog ((window doodler))
  (let* ((dialog (doodler-curve-dialog window))
         (curve-list nil)) ; ch3 step 50
    (when (or (not dialog)
              (not (windowp dialog)))
      (setf dialog (make-curve-dialog :owner window))
      (setf (doodler-curve-dialog window) dialog)
      (setf curve-list (find-component :curve-list dialog)) ; ch3 step 50
      (setf (range curve-list) ; ch3 step 50
        (list (make-instance 'cycloidal-curve))) ; ch3 step 50
      ;; ch3 step 36
      ;; Position the dialog to the left of the main window.
      (let* ((pos (window-to-screen-units window
                    (make-position (- (+ (exterior-width dialog) 10))
                                   40))))
        ;; But don't let it go off the left edge of the screen.
        (setf (position-x pos)(max 0 (position-x pos)))
        (move-window dialog pos)))
    (select-window dialog)))
  
;; added ch3 step30
(defmethod close :before ((window doodler) &key)
  (let ((curve-dialog (doodler-curve-dialog window))
        ;; added ch6 step 57
        (background-palette
         (doodler-background-palette window)))
    (when (and (windowp curve-dialog)
               curve-dialog)
      (close curve-dialog))
    (setf (doodler-curve-dialog window) nil)
    ;; added ch6 step57
    (when (and (windowp background-palette)
               background-palette)
      (close background-palette))
    (setf (doodler-background-palette window) nil)))

;; function added by chapter 4 step 25
;;; chee   03mar00 change erase-contents to erase-contents-box
(defmethod erase-window ((window doodler))
   (let ((pane (frame-child window)))
      (erase-contents-box pane (page-box pane))))

;; added by ch 5 step 64
(defmethod user-close ((window doodler))
   (let ((modal-dialog (modal-window)))
      (if* modal-dialog then 
        (beep window)
        (pop-up-message-dialog window "Doodler"
          (format nil "~
Close the ~a modal dialog before closing the Doodler."
            (title modal-dialog))
          warning-icon "~OK")
        else
        (call-next-method))))


;; added ch6 step 54
(defmethod initialize-instance :after ((window doodler)
                                       &rest initargs)
   (declare (ignore initargs))
   (show-background-palette window)
   (select-window window))


;; added ch6 step 56
(defmethod show-background-palette ((window doodler))(let ((palette (doodler-background-palette window)))
      (unless palette
         (setf palette
               (make-background-palette :owner window))
         (select-window palette)
         (setf (doodler-background-palette window) palette))
      (select-window palette)))
