;; Code for the dialog :background-palette

(in-package :cg-user)

;; Chapter 6, Step 39
(defclass background-palette (color-mixin dialog)
  ())

;; Chapter 6, Step 46
(defun background-palette-color-list-on-change 
    (widget new-value old-value)
  (declare 
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (change-background-color (parent widget)))
  (not new-value))

;; Chapter 6, Step 47
(defmethod change-background-color 
    ((palette background-palette))
  (let ((color (current-color palette))
        (doodler (owner palette)))
    (setf 
     (background-color (frame-child doodler)) 
     color)
    (erase-window doodler)))

;; Chapter 6, Step 51
(defun background-palette-color-button-on-change 
    (widget new-value old-value)
  (declare 
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (add-other-color (parent widget)))
  (not new-value))

;; Chapter 6, Step 52
(defmethod initialize-instance :after 
  ((palette background-palette) &rest initargs)
  (declare (ignore initargs))
  (let* ((pane (frame-child (owner palette)))
         (color-name (find-color-name palette
                                      (or (background-color pane)
                                          (default-background-color
                                              pane)))))
    (initialize-value 
     (find-component :color-list palette)
     (list color-name))))