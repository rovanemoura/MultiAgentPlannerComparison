
;; $Id: bean1.cl,v 5.1 2004/02/18 19:19:03 mm Exp $

(in-package :user)

;; This file contains the Lisp implementation of the methods
;; in the Java classes LispButton, LispBean and LispBeanBeanInfo

(defmethod fire-action ((self lisp-button))
  ;; This is called as a one-way method to free up the 
  ;;  Java-to-Lisp channel,  but the work must
  ;;  be done in a separate Lisp process to free up the 
  ;;  Lisp socket reader.
  (mp:process-run-function
   "fireAction"
   #'(lambda (self)
       (when (jfield "debug" self)
	 (format t "~&;; Button ~A pressed." (jcall "getLabel" self)))
       (let ((targets (jfield "pushListeners" self))
	     (actionevt (jnew "java.awt.event.ActionEvent" self 0 nil))
	     )
	 (setf targets (jcall "clone" targets))
	 (dotimes (i (jcall "size" targets))
	   (jcall "actionPerformed"
		  (jcall "elementAt" targets i)
		  actionevt :oneway
		  ))))
   self))

(defmethod set-debug ((self lisp-button) bool)
  (let ((old (jfield "debug" self)))
    (jfield "debug" self (make-immediate-object bool :boolean))
    (jcall (jmethod "java.beans.PropertyChangeSupport" "firePropertyChange"
		  "java.lang.String" "java.lang.Object" "java.lang.Object")
	   (jfield "changes" self)
	   "debug"
	   (jnew "java.lang.Boolean" (make-immediate-object old :boolean))
	   (jnew "java.lang.Boolean" (make-immediate-object bool :boolean)))))

(defmethod get-debug ((self lisp-button))
  (make-immediate-object (jfield "debug" self) :boolean))

(defmethod set-large-font ((self lisp-button) bool)
  (if (is-large-font self)
      (when bool (return-from set-large-font nil))
    (or bool (return-from set-large-font nil)))
  (let ((old (jcall "getFont" self))
	(size (if bool 18 12))
	)
    (jcall "setFont" self
	   (jnew "java.awt.Font" 
		 (jcall "getName" old)
		 (jcall "getStyle" old)
		 size))
    (jcall (jmethod "java.beans.PropertyChangeSupport" "firePropertyChange"
		  "java.lang.String" "java.lang.Object" "java.lang.Object")
	   (jfield "changes" self)
	   "largeFont"
	   (make-immediate-object (not bool) :boolean)
	   (make-immediate-object bool :boolean))))

(defmethod is-large-font ((self lisp-button))
  (>= (jcall "getSize" (jcall "getFont" self)) 18))

(defmethod set-font-size ((self lisp-button) x)
  (let ((old (jcall "getFont" self)))
    (jcall "setFont" self (jnew "java.awt.Font"
				(jcall "getName" old)
				(jcall "getStyle" old)
				x))
    (jcall (jmethod "java.beans.PropertyChangeSupport" "firePropertyChange"
		  "java.lang.String" "java.lang.Object" "java.lang.Object")
	   (jfield "changes" self)
	   "fontSize"
	   (jnew (jconstructor "java.lang.Integer" "int") (jcall "getSize" old))
	   (jnew (jconstructor "java.lang.Integer" "int") x))))

(defmethod get-font-size ((self lisp-button))
  (jcall "getSize" (jcall "getFont" self)))

(defmethod set-label ((self lisp-button) new)
  (let ((old (jfield "label" self)))
    (jfield "label" self new)
    (jcall "sizeToFit" self)
    (jcall (jmethod "java.beans.PropertyChangeSupport" "firePropertyChange"
		  "java.lang.String" "java.lang.Object" "java.lang.Object")
	   (jfield "changes" self) "label" old new)))
	   



(defvar *lisp-bean-debug* t)
(defvar *lisp-bean-rate* 100)
(defvar *lisp-bean-prop* "never set")
(defvar *lisp-bean-process* nil)
(defvar *lisp-bean-control* nil)

(defun lisp-bean-debug (&rest args)
  (when *lisp-bean-debug*
    (format t "~&~%;;LispBean: ~A~%"
	    (apply #'format nil args))))

(defmethod get-debug ((self lisp-bean))
  (lisp-bean-debug "get-debug called, returning ~A" *lisp-bean-debug*)
  (make-immediate-object *lisp-bean-debug* :boolean))

(defmethod set-debug ((self lisp-bean) x)
  (lisp-bean-debug "set-debug called, was ~A, set to ~A."
		   *lisp-bean-debug*  x)
  (setf *lisp-bean-debug* x))

(defmethod get-rate ((self lisp-bean) &optional x)
  (if x
      (setf *lisp-bean-rate* x)
    *lisp-bean-rate*))

(defmethod get-rate :around ((self lisp-bean) &optional x)
  (lisp-bean-debug "get-rate called, arg = ~S" x)
  (let ((v (call-next-method)))
    (lisp-bean-debug "get-rate returning, val = ~S" v)
    v))


(defmethod get-prop ((self lisp-bean) &optional x)
  (if x
      (setf *lisp-bean-prop* x)
    *lisp-bean-prop*))

(defmethod get-prop :around ((self lisp-bean) &optional x)
  (lisp-bean-debug "get-prop called, arg = ~S" x)
  (let ((v (call-next-method)))
    (lisp-bean-debug "get-prop returning, val = ~S" v)
    v))

(defmethod stop-action ((self lisp-bean) x)
  (lisp-bean-debug "stop-action called. arg=~S" x) 
  (setf *lisp-bean-control* nil))

(defmethod start-action ((self lisp-bean) x)
  (lisp-bean-debug "start-action called. arg=~S" x)
  (if *lisp-bean-process*
      (setf *lisp-bean-control* t)
    (let ()
      (setf *lisp-bean-process* 
	    (mp:process-run-function
	     "LBP"
	     #'(lambda ()
		 (unwind-protect
		     (let ()
		       (format t "~&~%LispBeanProcess starting.~%")
		       (setf *lisp-bean-control* t)
		       (loop
			(when (null *lisp-bean-control*)
			  (return))
			(lisp-bean-debug "LispBeanProcess running")
			(sleep (/ *lisp-bean-rate* 100.0))
			(format t ".")))
		   (setf *lisp-bean-process* nil)
		   (format t "~&~%LispBeanProcess exiting.~%")))))
      )))

(defmethod get-prop-index ((self lisp-bean-info)) 2)

(defmethod get-prop-descs ((self lisp-bean-info))
  (let ((array (jnew-array "java.beans.PropertyDescriptor" 3))
	(class (bean-class self))
	)
    (setf (jarray-ref array 0) (jnew "java.beans.PropertyDescriptor"
				     "debug" class))
    (setf (jarray-ref array 1) (jnew "java.beans.PropertyDescriptor"
				     "rate" class))
    (setf (jarray-ref array 2) (jnew "java.beans.PropertyDescriptor"
				     "string" class "fetchProp" "storeProp"))
    array))



