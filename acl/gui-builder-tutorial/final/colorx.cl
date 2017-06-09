;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2012 Franz Inc, Oakland, CA  All rights reserved.
;;
;; All rights reserved.
;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;; -=End Copyright Notice=-
(in-package :cg-user)

;; added ch6 step 4
(defclass color-mixin ()
    ())

;; added ch6 step 14
(defmethod initialize-instance :after ((dialog color-mixin)
                                       &rest initargs)
   (declare (ignore initargs))
   (let ((color-list (find-component :color-list dialog)))
      (when color-list
         (setf (range color-list) (default-color-range))
         (setf (recessed color-list) t))))

;; added ch6 step 14
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

;; added ch6 step 21
(defmethod draw-curve :around ((window basic-pane)
                               (curve cycloidal-curve))
   (with-foreground-color (window (color curve))
     (call-next-method)))

;; added ch6 step 23
(defmethod current-color ((dialog color-mixin))
   (let* ((color-list (find-component :color-list dialog))
          (value (first (value color-list))))
      (if value
         (color (find value (range color-list)
                  :key #'name))
         ;; else
         black)))

;; added ch6 step 23
(defmethod find-color-name ((dialog color-mixin)
                            &optional (color black))
   (let ((color-list (find-component :color-list dialog)))
      (name (find color (range color-list)
              :key #'color
              :test #'rgb-equal))))

;; added ch6 step 32
(defmethod add-other-color ((dialog color-mixin))
   (let* ((new-color (ask-user-for-color
                       :initial-color (current-color
                                        dialog)))
          (color-list (find-component :color-list dialog))
          (color-name nil))
      ;; do nothing if user canceled
      (when new-color
         ;; do not add color if it already is on the list.
         (when (not (setf color-name (find-color-name dialog
                                       new-color)))
            (setf color-name (new-color-name dialog))
            (let* ((colors (range color-list)))
               (setf (range color-list)
                     (append colors
                       (list (make-instance 'button-info
                               :name color-name
                               :image new-color
                               :string nil
                               :height nil
                               :ToolTip nil
                               :help-string nil))))))
         
         ;; change which color is pressed
         (setf (value color-list) (list color-name)))))

;; added ch6 step 32
(defmethod new-color-name ((dialog color-mixin))
   (let ((range (range (find-component :color-list dialog)))
         (name nil))
      (do ((index 1 (1+ index)))
          (nil)
         (setf name (intern
                      (format nil "CUSTOM-COLOR-~d" index)
                      (find-package :keyword)))
         ;; make sure no other colors already have
         ;; the same name
         (unless (find name range :key #'name)
            (return name)))))
