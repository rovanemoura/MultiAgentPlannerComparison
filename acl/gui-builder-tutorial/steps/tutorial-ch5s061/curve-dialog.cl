;; Code for the dialog :curve-dialog

(in-package :cg-user)

;; Chapter 3, Step 8
;; Chapter 5, Step 59
(defclass curve-dialog (dialog)
  ((curve-coefficient-dialog
    :initform nil
    :accessor curve-coefficient-dialog)))

;; Chapter 4, Step 4
(defun curve-dialog-draw-all-button-on-change 
    (widget new-value old-value)
  (declare 
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (draw-all (parent widget)))
  (not new-value))

;; Chapter 4, Step 5
(defmethod draw-all ((dialog curve-dialog))
  (let ((curve-list 
         (find-component :curve-list dialog))
        (pane (frame-child (owner dialog))))
    (dolist (curve (range curve-list))
      (draw-curve pane curve))))

;; Chapter 5, Step 60
(defmethod close :before ((dialog curve-dialog) &key)
  (let ((coefficient-dialog
         (curve-coefficient-dialog dialog)))
    (when (and (windowp coefficient-dialog)
               coefficient-dialog)
      (close coefficient-dialog))
    (setf (curve-coefficient-dialog dialog) nil)))