;; Code for the dialog :curve-dialog

(in-package :cg-user)

;; Chapter 3, Step 8
(defclass curve-dialog (dialog)
  ())

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