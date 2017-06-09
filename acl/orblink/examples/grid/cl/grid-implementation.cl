(in-package :user)

;; This file contains the implementation of the grid interface

(defclass grid-implementation (example:grid-servant)
  (
   (op:width :initform 4)          ;; Set the default width and height
   (op:height :initform 5)         ;; Note that the ORB defines the slots op:width and op:height already,
                                   ;; what these lines do is simply make sure that the slots have default
                                   ;; initial values of 4 and 5 respectively
   (array)))                       ;; and array of actual elements in the grid

(defmethod initialize-instance :after ((this grid-implementation) &rest args)
  (declare (ignore args))
  (setf (slot-value this 'array)
    (make-array `(,(op:width this) ,(op:height this)) :initial-element "Initial")))

(corba:define-method set ((this grid-implementation) row column value)
  (setf (aref (slot-value this 'array) row column) value))

(corba:define-method get ((this grid-implementation) row column)
  (aref (slot-value this 'array) row column))

