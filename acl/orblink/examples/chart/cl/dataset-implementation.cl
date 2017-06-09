(in-package :user)

(defclass dataset-implementation (chart:dataset-servant)
  ((values :accessor get-values :initarg :values)
   (listeners :accessor get-listeners :initform nil))
  )

(corba:define-method number_entries ((this dataset-implementation))
  (length (get-values this)))

(corba:define-method get ((this dataset-implementation) i)
  (aref (get-values this) i))

(corba:define-method register_listener ((this dataset-implementation) listener)
  (push listener (get-listeners this)))

(corba:define-method  adjust_requested ((this dataset-implementation) index value)
  (setf (aref (get-values this) index) value)
  (loop for listener in (get-listeners this)
      do
	(ignore-errors (op:dataset_was_adjusted listener index value))))

