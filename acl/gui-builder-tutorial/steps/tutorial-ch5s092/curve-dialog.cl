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

;; Chapter 5, Step 68
(defun curve-dialog-add-button-on-change 
    (widget new-value old-value)
  (declare 
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (add-curve (parent widget)))
  (not new-value))

;; Chapter 5, Step 69
(defmethod add-curve ((dialog curve-dialog))
  (let* ((curve-list 
          (find-component :curve-list dialog))
         (default-curve (value curve-list))
         (curve (show-coefficient-dialog dialog
                                         (if default-curve
                                             (copy-object default-curve)
                                           ;; else
                                           (make-instance 
                                               'cycloidal-curve)))))
    (when curve
      (setf (range curve-list)
        (append (range curve-list) 
                
                (list curve))))))

;; Chapter 5, Step 69
(defmethod get-coefficient-dialog 
    ((dialog curve-dialog))
  (let ((coefficient-dialog 
         (curve-coefficient-dialog dialog)))
    (when (or (not coefficient-dialog)
              (not (windowp coefficient-dialog)))
      (setq coefficient-dialog
            (make-coefficient-dialog :owner dialog))
      (setf (curve-coefficient-dialog dialog)
        coefficient-dialog))
    coefficient-dialog))

;; Chapter 5, Step 69
(defmethod show-coefficient-dialog 
    ((dialog curve-dialog) 
     &optional (curve 
                (make-instance 'cycloidal-curve)))
  (let* ((coefficient-dialog 
          (get-coefficient-dialog dialog))
         (a-widget 
          (find-component :a-coefficient-control
                       coefficient-dialog))
         (b-widget 
          (find-component :b-coefficient-control
                       coefficient-dialog))
         (c-widget 
          (find-component :c-coefficient-control
                       
                       coefficient-dialog)))
    (move-window coefficient-dialog 
                 (window-to-screen-units 
                     dialog (make-position 10 10)))
    
    ;; initialize the value of the widgets
    (setf (value a-widget) (a-coefficient curve))
    (setf (value b-widget) (b-coefficient curve))
    (setf (value c-widget) (c-coefficient curve))
    ;; display the dialog as modal
    (when (pop-up-modal-dialog coefficient-dialog
            :stream (owner dialog))
      ;; if the user clicks on OK, change 
      
      ;; the new curve to
      ;; reflect the values shown in the dialog
      (setf (a-coefficient curve) (value a-widget))
      (setf (b-coefficient curve) (value b-widget))
      (setf (c-coefficient curve) (value c-widget))
      curve)))

;; Chapter 5, Step 77
(defun curve-dialog-edit-button-on-change 
    (widget new-value old-value)
  (declare 
   (ignore-if-unused widget new-value old-value))
  (when new-value 
    (edit-curve (parent widget)))
  (not new-value))

;; Chapter 5, Step 78
;; Chapter 5, Step 83
(defmethod edit-curve ((dialog curve-dialog))
  (let* ((curve-list 
          (find-component :curve-list dialog))
         (curve (value curve-list))
         (range (range curve-list)))
    (if curve
        (when (show-coefficient-dialog dialog curve)
          ;; reset the range to force the 
          ;; scrolling list to redisplay
          ;; its curve information
          (setf (range curve-list) nil)
          (setf (range curve-list) range))
      
      ;; else
      (select-curve-warning dialog))))

;; Chapter 5, Step 82
(defmethod select-curve-warning 
    ((dialog curve-dialog))
  (pop-up-message-dialog dialog "Doodler"
                         "Select a curve first"
                         warning-icon "~OK"))

;; Chapter 5, Step 90
(defun curve-dialog-delete-button-on-change 
    (widget new-value old-value)
  (declare 
   (ignore-if-unused widget new-value old-value))
  (when new-value 
    (delete-curve (parent widget)))
  (not new-value))

;; Chapter 5, Step 91
(defmethod delete-curve ((dialog curve-dialog))
  (let* ((curve-list 
          (find-component :curve-list dialog))
         (curve (value curve-list)))
    (if* curve then
            (setf (range curve-list)
              (remove curve (range curve-list)))
            (setf (value curve-list) nil)
            (erase-window (owner dialog))
            (draw-all dialog)
       else
            (select-curve-warning dialog))))
