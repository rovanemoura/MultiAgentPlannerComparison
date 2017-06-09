(in-package :user)

(defparameter dataset-invoke-root (merge-pathnames ".." *load-pathname*))
(corba:idl (merge-pathnames "chart.idl" dataset-invoke-root))
(load (merge-pathnames "cl/dataset-implementation.cl" dataset-invoke-root))

(defvar dataset nil)
(defun dataset-server(&optional filename)
  (setq filename (or filename (merge-pathnames "dataset.ior" dataset-invoke-root)))
  (let* ((min -100.0)
	 (max 100.0)
	 (number-entries 20)
	 (values (make-array number-entries)))
    (loop for i below number-entries
	do
	  (setf (aref values i)
	    (+ min
	       (* i (/ (- max min) number-entries))))
	finally
	  (setf (aref values (1- number-entries)) max))
    (setq dataset (make-instance 'dataset-implementation :values values))
    (orblink:write-ior-to-file dataset filename))
)

(defun dataset-perturb (dataset &key (range 20.0) (wait 1.0) (times 5))
  (loop
      with numentries = (op:number_entries dataset)
      for time below times
      do
	(let* ((entry (random (- numentries 2)))
	       (actual-entry (+ entry 1))
	       (increment (random range))
	       (actual-increment
		(if (> (random 2) 0)
		    increment
		  (- increment)))
	       (original-value (op:get dataset actual-entry))
	       (new-value (+ original-value actual-increment)))
	  (op:adjust_requested dataset actual-entry new-value)
	  (sleep wait))))



