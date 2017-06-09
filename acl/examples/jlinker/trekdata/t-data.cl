
;; $Id: t-data.cl,v 5.0 2004/01/14 18:31:35 layer Exp $

(in-package :user)

;; The data-handling component of the demo

(defclass annotator-data ()
  ((home :initform nil :initarg :home)
   (data-type :initform nil :initarg :data-type)
   (text-type :initform nil :initarg :text-type)
   (base-data :initform nil)
   (user-data :initform nil)
   ))


(defvar *data* nil)
(defun get-annotator-data (key &rest rest)
  (or *data* (setf *data* (make-instance 'annotator-data)))
  (ecase key
    (:item (apply #'get-data *data* rest))
    (:write (apply #'write-data *data* rest))
    ;;(:scatter (apply #'scatter-data *data* rest))
    (:search (apply #'search-data *data* rest))))



(defmethod initialize-instance :after ((self annotator-data)
				       &key home data-type text-type
				       &allow-other-keys)
  (or home (setf home "userdata/"))
  (or data-type (setf data-type "bmp"))
  (or text-type (setf text-type "txt"))
  (setf (slot-value self 'home) home
	(slot-value self 'data-type) data-type
	(slot-value self 'text-type) text-type)
  self)

(defmethod get-data ((self annotator-data)
		     &key index prop user value)
  ;;       -> (user-name (index string ...) ...)
  (when (null (slot-value self 'base-data))
    (collect-data)
    (setf (slot-value self 'base-data) t))
  (let* ((home (slot-value self 'home))
	 (text-type (slot-value self 'text-type))
	 (len (get-trek :items))
	 uplace vplace pplace uval ufile)
    (when user
      (or (setf uplace (assoc user (slot-value self 'user-data)
			      :test #'equalp))
	  (progn
	    (setf uplace 
		  (if (setf ufile 
			    (probe-file (concatenate 'string home user 
						     "." text-type)))
		      (with-open-file (s ufile) (read s nil nil))
		    (list user)))
	    (push uplace (slot-value self 'user-data))))
      (or (setf vplace (assoc index (cdr uplace)))
	  (null value)
	  (< index 0)
	  (not (< index len))
	  (push (setf vplace (cons index nil)) (cdr uplace)))
      (or (null prop)
	  (null vplace)
	  (setf pplace (assoc prop (cdr vplace)))
	  (push (setf pplace (list prop)) (cdr vplace)))
      (and value pplace (setf (cdr pplace) value))
      (and pplace (setf uval (cdr pplace))))
    (or pplace (null prop)
	;; fetch a random user value if none for this one
	(dolist (u (slot-value self 'user-data))
	  (and (setf vplace (assoc index (cdr u)))
	       (setf pplace (assoc prop (cdr vplace)))
	       (setf uval (list (list (car u)) (cdr pplace))))))
    (values 
     (cond ((null index) len)
	   ((null prop) (get-trek :props))
	   (t (get-trek index prop)))
     uval)))

(defmethod write-data ((self annotator-data) &key user)
  (let* ((home (slot-value self 'home))
	 (text-type (slot-value self 'text-type))
	 (uplace (assoc user (slot-value self 'user-data) :test #'equalp))
	 file)
    (when (cdr uplace)
      (with-open-file
       (s (setf file (concatenate 'string home user "." text-type))
	  :direction :output :if-exists :supersede)
       (format s "~%(~S~%" (car uplace))
       (dolist (x (cdr uplace))
	 (format s "  ~S~%" x))
       (format s "  )~%")
       file))))

#+ignore
(defmethod scatter-data ((self annotator-data)
		     &key user
		     &aux file ufile
		     (home (slot-value self 'home))
		     (text-type (slot-value self 'text-type))
		     )
  (when user
    (dotimes (i (get-data self))
      (multiple-value-bind (item uval)
	  (get-data self :index i :user user)
	(when (and item uval)
	  (setf file (concatenate 'string home (first item) "." text-type))
	  (when (probe-file file)
	    (rename-file-raw file
			     (concatenate 'string home (first item) ".old")))
	  (with-open-file
	   (s file :direction :output)
	   (format s "~A~%" uval)))))
    (when (probe-file 
	   (setf ufile (concatenate 'string home user "." text-type)))
      (rename-file-raw ufile (concatenate 'string home user "." "old")))
    (setf (slot-value self 'base-data) nil)
    (and (setf ufile (assoc user (slot-value self 'user-data) :test #'equalp))
	 (setf (slot-value self 'user-data)
	       (delete ufile (slot-value self 'user-data))))
    (get-data self)
    (when file home)))

(defmethod search-data ((self annotator-data) (text string) 
			&key user index prop)
  (dotimes (i (get-data self))
    (when (< index i)
      (dolist (prop (if prop (list prop) (get-trek :props)))
	(multiple-value-bind (item uval)
	    (get-data self :index i :user user :prop prop)
	  (when (or (search-prop text item :test #'string-equal)
		    (and uval
			 (search-prop text uval :test #'string-equal)))
	    (return-from search-data i)))))))

(defun search-prop (text item &key test string)
  (or string (setf string (typecase text
			    (string text)
			    (symbol (symbol-name text))
			    (cons (format nil "~A~{ ~A~}" 
					  (car text) (cdr text)))
			    (otherwise (format nil "~A" text)))))
  (typecase item
    (cons (dolist (x item nil)
	    (when (search-prop text x :test test :string string)
	      (return t))))
    (null nil)
    ((or string symbol) (search string (string item) :test test))
    (number (or (equalp text item) 
		(search string (format nil "~A" item) :test test)))
    (otherwise (search string (format nil "~A" item) :test test))
    ))



