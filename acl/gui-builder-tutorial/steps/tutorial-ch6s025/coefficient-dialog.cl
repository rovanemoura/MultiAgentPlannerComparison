;; Code for the dialog :coefficient-dialog

(in-package :cg-user)

;; Chapter 5, Step 8
;; Chapter 6, Step 17
(defclass coefficient-dialog (color-mixin dialog)
  ())

;; Chapter 5, Step 50
(defun coefficient-dialog-test-button-on-change
    (widget new-value old-value)
  (declare 
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (test-curve (parent widget)))
  (not new-value))

;; Chapter 5, Step 52
;; Chapter 6, Step 24
(defmethod test-curve ((dialog coefficient-dialog))
  (let* ((curve 
          (make-instance 'cycloidal-curve
            :a-coefficient 
            (value (find-component 
                    :a-coefficient-control
                    dialog))
            :b-coefficient
            (value (find-component 
                    :b-coefficient-control
                    dialog))
            :c-coefficient 
            (value (find-component 
                    
                    :c-coefficient-control
                    dialog))
            :color (current-color dialog)))
         (curve-dialog (owner dialog))
         (doodler (owner curve-dialog)))
    (draw-curve (frame-child doodler) curve)))