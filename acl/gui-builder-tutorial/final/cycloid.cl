;;; copyright (c) 1986-2012 Franz Inc, Oakland, CA
;;; All rights reserved.
;;; 
;;; Permission is granted only to any individual or institution which has
;;; current Allegro CL license(s) to use, copy, or modify this software,
;;; provided any reproduction or distribution of binary versions of this
;;; software are compiled with a licensed Allegro CL, and provided
;;; that this complete copyright and permission notice is maintained, intact,
;;; in all copies and supporting documentation. 
;;; 
;;; Franz Incorporated provides this software "as is" without
;;; express or implied warranty.
;;; 
;;; Restricted Rights Legend
;;; ------------------------
;;; Use, duplication, and disclosure of the software, data and information
;;; contained herein by any agency, department or entity of the U.S.
;;; Government are subject to restrictions of Restricted Rights for
;;; Commercial Software developed at private expense as specified in FAR
;;; 52.227-19 or DOD FAR Supplement 252 52.227-7013 (c) (1) (ii), as
;;; applicable.

(in-package :cg-user)

(defclass cycloidal-curve ()
    ((a-coefficient
       :initform 200
      :initarg :a-coefficient
      :accessor a-coefficient)
     (b-coefficient
       :initform 70
      :initarg :b-coefficient
      :accessor b-coefficient)
     (c-coefficient
       :initform 65
      :initarg :c-coefficient
      :accessor c-coefficient)
     (color
      :initform blue
      :accessor color
      :initarg :color)
     ))

(defmethod print-object ((object cycloidal-curve) stream)

   (format stream "#<cycloidal-curve a=~d, b=~d, c=~d>"
     (a-coefficient object) (b-coefficient object) (c-coefficient object)))

(defun highest-common-factor (number-1 number-2)
   "Return the highest common factor using Euclid's algorithm."
   (assert (integerp number-1))
   (assert (integerp number-2))   
 (let ((index (max number-1 number-2))
       (factor (min number-1 number-2))
       )
    (do ((j (mod index factor) (mod index factor)))
        ((= j 0) factor)

       (setf index factor)
       (setf factor j))
    ))

(defmethod draw-curve ((window basic-pane) (curve cycloidal-curve))
   (let* ((a (a-coefficient curve))
          (b (b-coefficient curve))
          (c (c-coefficient curve))
          (rab (- a b))
          (alpha 0.0)
          (number-of-angle-segments 100)
          (number-of-line-segments 200)
          (adif (/ pi number-of-angle-segments))
          (aoverb (/ a b))
          (lines (* number-of-line-segments
                    (/ b (highest-common-factor a b))))
          (center (center (page-box window)))
          (center-x (position-x center))
          (center-y (position-y center))
          (position (make-position 0 0))
          beta xpt ypt)
      (move-to-x-y window (truncate (+ center-x
                                       (+ rab c))) center-y)
      (dotimes (i lines)
         (incf alpha adif)
         (setf beta (* alpha aoverb))
         (setf xpt (+ (round (+ (* rab (cos alpha))
                                (* c (cos beta))))
                      center-x))
         (setf ypt (+ (round (- (* rab (sin alpha))
                                (* c (sin beta))))
                      center-y))
         (draw-to window (nmake-position position xpt ypt)))
      ))

(defmethod copy-object ((curve cycloidal-curve) 
                        &optional (to (make-instance 'cycloidal-curve)))
   (setf (a-coefficient to) (a-coefficient curve))
   (setf (b-coefficient to) (b-coefficient curve))

   (setf (c-coefficient to) (c-coefficient curve))
   (setf (color to) (color curve))
   to)
