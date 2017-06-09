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
;; Code for the dialog :curve-dialog

(in-package :cg-user)

(defclass curve-dialog (dialog)
    ((curve-coefficient-dialog ; ch5 step 59
      :initform nil ; ch5 step 59
      :accessor curve-coefficient-dialog))) ;ch5 step 59

; function added ch5 step 60
(defmethod close :before ((dialog curve-dialog) &key)
   (let ((coefficient-dialog (curve-coefficient-dialog dialog)))
      (when (and (windowp coefficient-dialog)
                 coefficient-dialog)
         (close coefficient-dialog))
      (setf (curve-coefficient-dialog dialog) nil)))

(defun curve-dialog-draw-all-button-on-change (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   (when new-value ; ch4 step 4
      (draw-all (parent widget)))
   (not new-value))

;;; chee   10sep99 bug8144 had to change a call to parent here
;;;        to call owner instead, now that parent of a pop-up
;;;        dialog returns the screen for general consistency
(defmethod draw-all ((dialog curve-dialog))
   (let* ((curve-list (find-component :curve-list dialog))
          (pane (frame-child (owner dialog))))
      (dolist (curve (range curve-list))
         (draw-curve pane curve))))

;; added by ch5 step 68
(defun curve-dialog-add-button-on-change (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   (when new-value
      (add-curve (parent widget)))
   (not new-value))

;; added by ch5 step 69
(defmethod add-curve ((dialog curve-dialog))
   (let* ((curve-list (find-component :curve-list dialog))
          (default-curve (value curve-list))
          (curve (show-coefficient-dialog dialog
                  (if default-curve
                     (copy-object default-curve)
                     ;; else
                     (make-instance 'cycloidal-curve)))))
      (when curve
         (setf (range curve-list)
               (append (range curve-list) (list curve))))))

;; added by ch5 step 69
(defmethod get-coefficient-dialog ((dialog curve-dialog))
   (let ((coefficient-dialog (curve-coefficient-dialog dialog)))
      (when (or (not coefficient-dialog)
                (not (windowp coefficient-dialog)))
         (setf coefficient-dialog
               (make-coefficient-dialog :owner dialog))
         (setf (curve-coefficient-dialog dialog)
               coefficient-dialog))
      coefficient-dialog))

;; added by ch5 step 69
(defmethod show-coefficient-dialog ((dialog curve-dialog)
                                    &optional (curve (make-instance 
                                                         'cycloidal-curve)))
  (let* ((coefficient-dialog (get-coefficient-dialog dialog))
         (a-widget (find-component :a-coefficient-control
                                coefficient-dialog))
         (b-widget (find-component :b-coefficient-control
                                coefficient-dialog))
         (c-widget (find-component :c-coefficient-control
                                coefficient-dialog))
         ;; ch6 step 22
         (color-list (find-component :color-list
                                  coefficient-dialog))
         ;; ch6 step 22
         (color-name (find-component :color-name
                                  coefficient-dialog)))
    (move-window coefficient-dialog
                 (window-to-screen-units dialog (make-position 10 10)))
    
    ;;; initialize the value of the widgets
    (setf (value a-widget) (a-coefficient curve))
    (setf (value b-widget) (b-coefficient curve))
    (setf (value c-widget) (c-coefficient curve))
    ;; ch6 step 22
    (setf (value color-list)
      (when color-name
        (list color-name)))
    
    ;;; display the dialog as modal
    (when (pop-up-modal-dialog coefficient-dialog
            :stream (owner dialog))
      ;;; if the user clicks on OK, change the new curve to
      ;;; reflect the values shown in the dialog
      (setf (a-coefficient curve) (value a-widget))
      (setf (b-coefficient curve) (value b-widget))
      (setf (c-coefficient curve) (value c-widget))
      ;; ch6 step 22
      (setf (color curve)
        (current-color coefficient-dialog))
      curve)))

;; added by ch5 step 77
(defun curve-dialog-edit-button-on-change (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   (when new-value
      (edit-curve (parent widget)))
   (not new-value))


;; added by ch5 step 78
(defmethod edit-curve ((dialog curve-dialog))
   (let* ((curve-list (find-component :curve-list dialog))
          (curve (value curve-list))
          (range (range curve-list)))
      ;; modified by ch5 step 83
      (if curve
         (when (show-coefficient-dialog dialog curve)
            ;; reset the range to force the scrolling list
            ;; to redisplay its curve information
            (setf (range curve-list) nil)
            (setf (range curve-list) range))
         ;; else
         (select-curve-warning dialog))))

;; added by ch5 step 82
(defmethod select-curve-warning ((dialog curve-dialog))
   (pop-up-message-dialog dialog "Doodler"
     "Select a curve first"
     warning-icon "~OK"))

;; added by ch5 step 90
(defun curve-dialog-delete-on-change (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   (when new-value
      (delete-curve (parent widget)))
   (not new-value))

;; added by ch5 step 91
;;; chee   10sep99 bug8144 had to change a call to parent here
;;;        to call owner instead, now that parent of a pop-up
;;;        dialog returns the screen for general consistency
(defmethod delete-curve ((dialog curve-dialog))
   (let* ((curve-list (find-component :curve-list dialog))
          (curve (value curve-list)))
      (if* curve then
        (setf (range curve-list)
              (remove curve (range curve-list)))
        (setf (value curve-list) nil)
        (erase-window (owner dialog))
        (draw-all dialog)
        else
        (select-curve-warning dialog))))


;; added by ch5 step 98
(defun curve-dialog-curve-list-on-double-click (dialog widget)
   (declare (ignore-if-unused dialog widget))
   (curve-list-double-click dialog widget)
   t)

;; added by ch5 step 99
(defmethod curve-list-double-click ((dialog curve-dialog) widget)
   (declare (ignore widget))
   (edit-curve dialog))
