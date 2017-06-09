;; Code for the dialog :form2

(in-package :cg-user)

;; chapter 1, step 10
;; chapter 3, step 24
;; chapter 4, step 16
(defclass doodler (bitmap-window)
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
;; chapter 3, step 36
;; chapter 3, step 50
(defmethod show-curve-dialog ((window doodler)) 
   (let ((dialog (doodler-curve-dialog window))
         (curve-list nil))               ; step 50
      (when (or (not dialog) 
                (not (windowp dialog))) 
         (setq dialog 
               (make-curve-dialog :owner window)) 
         (setf (doodler-curve-dialog window) dialog)
         (setq curve-list                ; step 50
               (find-component :curve-list dialog))
         (setf (range curve-list)        ; step 50

               (list 
                 (make-instance 'cycloidal-curve)))
      ;; Position the dialog to the 
      ;; left of the main window.
      (let* ((pos (window-to-screen-units 
                    window
                    (make-position 
                     (- (+ 
                        (exterior-width dialog) 10))
                         40))))
        ;; But don't let it go off the left 
        ;; edge of the screen.
        (setf (position-x pos)

              (max 0 (position-x pos)))
        (move-window dialog pos)))
      (select-window dialog)))

;; chapter 3, step 30
(defmethod close :before ((window doodler) &key)
   (let ((curve-dialog (doodler-curve-dialog window)))
      (when (and (windowp curve-dialog) 

                 curve-dialog)
         (close curve-dialog))
      (setf (doodler-curve-dialog window) nil)))
