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