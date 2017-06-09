;; Code for the dialog :form2

(in-package :cg-user)

;; chapter 1, step 10
;; chapter 3, step 24
(defclass doodler (non-refreshing-window)
    ((doodler-curve-dialog
      :accessor doodler-curve-dialog
      :initform nil)))

;; chapter 3, step 29
(defun doodler-toolbar-click 
    (widget new-value old-value)
  (declare 
   (ignore-if-unused widget new-value old-value))
  ;; Do the action only when a button is 
  ;; being pressed (not unpressed)
  (when new-value
    (let ((doodler (parent (parent widget))))
      (case (first new-value)
        (:curve 
         (show-curve-dialog doodler))
        (t nil)))
    )
  (not new-value))


;; chapter 3, step 30
(defmethod show-curve-dialog ((window doodler))
   (let* ((dialog (doodler-curve-dialog  window)))
      (when (or (not dialog) 
                (not (windowp dialog))) 
         (setq dialog 
               (make-curve-dialog :owner window))
         (setf (doodler-curve-dialog window) dialog)) 
      (select-window dialog)))

;; chapter 3, step 30
(defmethod close :before ((window doodler) &key)
   (let ((curve-dialog (doodler-curve-dialog window)))
      (when (and (windowp curve-dialog) 

                 curve-dialog)
         (close curve-dialog))
      (setf (doodler-curve-dialog window) nil)))
