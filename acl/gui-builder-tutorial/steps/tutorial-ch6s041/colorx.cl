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

;; Chapter 6, Step 32
(defmethod add-other-color ((dialog color-mixin))
  (let* ((new-color (ask-user-for-color
                     :initial-color
                     (current-color dialog)))
         (color-list 
          (find-component :color-list dialog))
         (color-name nil))
    ;; do nothing if user canceled
    (when new-color
      ;; do not add color if it already 
      ;; is on the list.
      (when 
          (not 
           (setf color-name 
             
             (find-color-name dialog new-color)))
        (setf color-name (new-color-name dialog))
        
        (let* ((colors (range color-list)))
          (setf (range color-list)
            (append colors
                    (list 
                     (make-instance 'button-info
                       :name color-name
                       :image new-color))))))
      
      ;; change which color is pressed
      (setf (value color-list) 
        
        (list color-name)))))

;; Chapter 6, Step 32
(defmethod new-color-name ((dialog color-mixin))
  (let ((range 
         (range (find-component :color-list dialog)))
        (name nil))
    (do ((index 1 (1+ index)))
        (nil)
      (setf name (intern
                  (format nil 
                      "CUSTOM-COLOR-~d" index)
                  (find-package :keyword)))
      ;; make sure no other colors already have
      ;; the same name
      
      (unless (find name range :key #'name)
        (return name)))))