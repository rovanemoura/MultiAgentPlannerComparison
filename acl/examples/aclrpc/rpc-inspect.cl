;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2000-2005 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2002-2015 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: rpc-inspect.cl,v 4.4 2007/04/17 21:51:29 layer Exp $

(in-package #:user)

(eval-when (compile load eval) (require :aclrpc "aclrpc.fasl"))

(defpackage #:user 
  (:use #:net.rpc)
  (:import-from #:inspect #:make-field-def)
  )



;;; Code at the inspector end

;; [bug12959]: Move property to internal in sys package
(defun (:property rpc-remote-ref sys::inspector-function) (ob)
  (rfields ob))

(defun one (x) (declare (ignore x)) 1)

(defmethod rfields ((ob rpc-remote-ref))
  (let ((port (rpc-open-p (rr-home ob))))
    (list (make-field-def "RefType"
			  #'rr-base 
			  :value)
	  (make-field-def "RemoteHome"
			  #'rr-home 
			  :value)
	  (make-field-def "RemoteType"
			  #'rr-type 
			  :value)
	  (make-field-def (if port
			      "RemoteParts"
			    "Cannot connect to remote Lisp")
			  (if port
			      #'(lambda (ob) 
				  (make-instance 'remote-field-defs 
						 :rref ob
						 :offset 0
						 :port port
						 ))
			    #'(lambda (ob) 
				(declare (ignore ob)) nil))
			  :value))))

(defclass remote-field-defs () 
  (
   (rref :initarg :rref)
   (offset :initarg :offset)
   (last   :initform nil)
   (length :initarg :length :initform nil)
   (port   :initarg :port   :initform nil)
   ))

;; [bug12959]: Move property to internal in sys package
(defun (:property remote-field-defs sys::inspector-function) (ob)
  (remote-field-defs ob))

(defmethod remote-field-defs ((refs remote-field-defs))
  (let* ((rref (slot-value refs 'rref))
	 (offset (slot-value refs 'offset))
	 (length (slot-value refs 'length))
	 (port  (slot-value refs 'port))
	 defs parts last more j)
    (if* (rpc-enable-port port nil nil)
	 then
	 (with-remote-port 
	  (port)
	  (setf parts (multiple-value-list (rcall 'rfield-count rref offset)))
	  (setf length (pop parts))
	  (setf (slot-value refs 'length) length)
	  (if (< (+ offset 10) length)
	      (setf last (+ offset 9) more (+ offset 10))
	    (setf last (1- length) more nil))
	  (setf (slot-value refs 'last) last)

	  (push (make-field-def "RemoteObject"
				(list (setf j 0) 'one
				      #'(lambda (refs i) 
					  (declare (ignore ob i))
					  (slot-value refs 'rref))
				      nil)
				:indirect)
		defs)
	  (push (make-field-def "Size"
				(list (incf j) 'one
				      #'(lambda (refs i) 
					  (declare (ignore ob i))
					  (slot-value refs 'length))
				      nil)
				:indirect)
		defs)
	  (push (make-field-def "LookingAt"
				(list (incf j) 'one
				      #'(lambda (refs i) 
					  (declare (ignore ob i))
					  (list (slot-value refs 'offset) 
						(slot-value refs 'last)))
				      nil)
				:indirect)
		defs)
	  (dotimes (i (- last offset -1))
	    (push (make-field-def (pop parts)
				  (list (incf j) 'one
					(make-closure1 i)
					nil)
				  :indirect)
		  defs))
	  (when more
	    (push
	     (make-field-def "MoreParts"
			     (list (incf j) 'one
				   (make-closure2 port more)
				   nil)
			     :indirect)
	     defs))
	  )
	 (reverse defs)
	 else
	 (list (make-field-def "RemoteObject"
			#'(lambda (ob) (declare (ignore ob)) "Unreachable")
			:value)))))

(defun make-closure1 (ix)
  #'(lambda (refs i) (declare (ignore i)) (remote-field-ref refs ix)))

(defun make-closure2 (port more)
  #'(lambda (refs i) 
      (declare (ignore i))
      (make-instance 'remote-field-defs 
		     :port port
		     :rref (slot-value refs 'rref)
		     :offset more
		     )))


(defmethod remote-field-ref ((refs remote-field-defs) i)
  (let* ((rref (slot-value refs 'rref))
	 (offset (slot-value refs 'offset))
	 (port  (slot-value refs 'port))
	 )
    (if* (rpc-enable-port port nil nil)
	 then
	 (with-remote-port 
	  (port)
	  (rcall 'rfield-ref rref (+ offset i))
	  )
	 else "Unreachable")))





;;; Code at the inspectee end

(defmethod rfield-count ((ob t) offset)
  (typecase ob
    (structure (rslot-count ob offset))
    (cons      (do ((tl ob (cdr tl)) (n 0))
		   ((atom tl) (if tl (1+ n) n))
		 (incf n)))
    (sequence (length ob))
    (array    (apply #'* (array-dimensions ob)))
    (otherwise 0)))

(defmethod rfield-count ((ob standard-object) offset)
  (rslot-count ob offset))

(defun rslot-count (ob offset)
  (let* ((class (class-of ob))
	 (slots (mop:class-slots class))
	 (len (length slots))
	 (i 0) parts)
    (dolist (s slots)
      (if* (< i offset)
	   then nil
	   elseif (< i (+ offset 10))
	   then
	   (push (symbol-name (mop:slot-definition-name s)) parts)
	   else (return))
      (incf i))
    (values-list
     (list* len (reverse parts)))))


(defmethod rfield-ref ((ob t) offset)
  (typecase ob
    (structure (rslot-ref ob offset))
    (cons      (do ((tl ob (cdr tl)) (n 0))
		   ((atom tl) tl)
		 (when (eql n offset) (return (car tl)))
		 (incf n)))
    (sequence (elt ob offset))
    (otherwise nil)))

(defmethod rfield-ref ((ob standard-object) offset)
  (rslot-ref ob offset))

(defun rslot-ref (ob offset)
  (let* ((class (class-of ob))
	 (slots (mop:class-slots class))
	 (i 0))
    (dolist (s slots)
      (if* (< i offset)
	   then nil
	   elseif (= i offset)
	   then
	   (return (if (slot-boundp ob (mop:slot-definition-name s))
		       (slot-value ob (mop:slot-definition-name s))
		     "Unbound"))
	   else (return))
      (incf i))))



;;; Some convenience functions and sample data

(defun server (&optional (port 4567))
  (make-rpc-server nil :local-port port :name "inspector" :open :listener
    :re-connect t
    ))

(defclass myclass () 
  ((a :initform 123) (b :initform 455)))

(defstruct mystruct
  a b c d e f g h i j k l m n o q)


(defvar *var*)
(defun client (&optional (port 4567))
  (let ((port (make-rpc-client nil 
		:remote-port port 
		;; :remote-host "host-if-not-localhost" 
		:home "inspectee" 
		:re-connect :demand)))
    (when port 
      (setf *rpc-port* port)
      (rcall 'set '*var* 
	     (list
	      (make-instance 'myclass)
	      (make-mystruct)
	      (make-array 57 :initial-element (list :a :b))
	      (make-array '(5 7) :initial-element (list :aa :bb))
	      (list* 1 2 3 4 5)
	      )))))
	      
