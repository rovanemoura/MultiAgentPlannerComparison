;; Chapter 6, Step 4
(in-package :cg-user)

;; Chapter 6, Step 4
(defclass color-mixin ()
  ())

;; Chapter 6, Step 14
(defmethod initialize-instance :after 
  ((dialog color-mixin) &rest initargs)
  (declare (ignore initargs))
  (let ((color-list 
         (find-component :color-list dialog)))
    (when color-list
      (setf (range color-list) 
        (default-color-range))
      (setf (recessed color-list) t))))

;; Chapter 6, Step 14
(defun default-color-range ()
  (list 
   (make-instance 'button-info 
     :name :black 
     :image #S(rgb red 0 green 0 blue 0))
   (make-instance 'button-info 
     
     :name :red 
     :image #S(rgb red 255 green 0 blue 0))
   (make-instance 'button-info 
     :name :green 
     :image #S(rgb red 0 green 255 blue 0))
   (make-instance 'button-info 
     :name :blue 
     :image #S(rgb red 0 green 0 blue 255))
   (make-instance 'button-info 
     :name :cyan 
     :image #S(rgb red 0 green 255 blue 255))
   (make-instance 'button-info 
     :name :magenta 
     :image #S(rgb red 255 green 0 blue 255))
   
   (make-instance 'button-info 
     :name :yellow 
     :image #S(rgb red 255 green 255 blue 0))
   (make-instance 'button-info 
     :name :white 
     :image #S(rgb red 255 green 255 blue 255))))

;; Chapter 6, Step 21
(defmethod draw-curve :around 
  ((window basic-pane)
   (curve cycloidal-curve))
  (with-foreground-color (window (color curve))
    (call-next-method)))

;; Chapter 6, Step 23
(defmethod current-color ((dialog color-mixin))
  (let* ((color-list 
          (find-component :color-list dialog))
         (value (first (value color-list))))
    (if value
        (color (find value (range color-list)
                     :key #'name))
      ;; else
      black)))

;; Chapter 6, Step 23
(defmethod find-color-name ((dialog color-mixin) 
                            &optional (color black))
  (let ((color-list 
         (find-component :color-list dialog)))
    
    (name (find color (range color-list)
                :key #'color
                :test #'rgb-equal))))