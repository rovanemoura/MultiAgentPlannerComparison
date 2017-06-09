; -*- mode: common-lisp; package: foreign-functions -*-
;;
;; copyright (c) 1996-2000 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2004 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: cdbind.cl,v 1.5 2004/01/16 19:24:53 layer Exp $

(in-package :foreign-functions)

;; This file is needed in order to compile the output of the binder
;; but it should not need any other parts of the binder.

;; This macro is emitted by the binder to allow customization of the
;; generated code.
;;
;; The following macro and functions may be modified as needed to
;; obtain the desired output.


;; This defpackage is the union of the defpackage calls in
;; cmnbind  cbind  and  cplbind 
(defpackage #:ff (:export 
		  
		  #:*export-foreign-symbols*
		  
		  #:build-c-binding
		  #:build-c++binding

		  #:bind-c-export
		  #:bind-c-constant
		  #:bind-c-type
		  #:bind-c-typedef
		  #:bind-c-function
		  #:bind-c-alternate
		  #:bind-c-sizeof
		  
		  #:*c-compiler-macro-names*
		  #:*default-foreign-symbol-package*
		  #:*c-parser-cpp*
		  #:*c-parser-cc1*
		  #:*c-parser-cc1plus*
		  
		  #:dummy-forward-struct
		  #:dummy-forward-class
		  #:opaque-c++class
		  
		  #:*decode-intern-dash*
		  #:*decode-intern-case*
		  #:*decode-intern-hyphen*
		  #:*decode-intern-res*
		  
		  #:decode-intern
		  
		  #:make-hold
		  #:hold-signed-long
		  #:hold-unsigned-long
		  #:hold-unsigned-short
		  #:hold-unsigned-byte
		  
		  
		  #:long-long
		  #:unsigned-long-long
		  #:long-double
		  #:function-pointer
		  
		  )
	    #+remove			; bug13705
	    (:import-from         ;; <25>
	     :excl
	     #:ce-get)
	    )

(defvar *export-foreign-symbols* t)

;; Some extensions to the built-in foreign types
(def-foreign-type long-long (:struct (nil :long) (nil :long)))
(def-foreign-type unsigned-long-long (:struct (nil :long) (nil :long)))
(def-foreign-type long-double (:struct (nil :double) (nil :double)))
(def-foreign-type function-pointer (* :void))
(def-foreign-type dummy-forward-struct (:struct (nil (:array :int 1))))


(defmacro bind-c-export (id)
  ;; emitted only by macro defs in this file
  (when *export-foreign-symbols*
    `(eval-when (compile load eval)
       (export ',id))
    #+old
    `(defpackage ,(intern (package-name *package*) (find-package :keyword)) 
       ;; defpackage seems to require this arg, otherwise it tries
       ;; to add common-lisp
       (:use . ,(mapcar #'(lambda (x)
			    (intern (package-name x) (find-package :keyword)))
			(package-use-list *package*)))
       (:export ,id))))


(defmacro bind-c-constant  (id val)
  ;; emitted in cmnbind by decode-push-defconstant, decode-gen-defconstant
  `(progn
     (bind-c-export ,id)
     (defparameter ,id ,val)))

(defmacro bind-c-alternate (id &rest def)
  ;; emitted by bind-c-function
  ;; cmnbind: decode-macro-defs-to-lisp
  `(progn
     (bind-c-export ,id)
     (defmacro ,id ,@def)))

(defmacro bind-c-type (id &rest def)
  ;; emitted by
  ;; cmnbind: decode-push-struct-parts
  ;; cbind: decode-c-statements
  ;; cplbind: decode-c++typedef 
  `(progn
     (bind-c-export ,id)
     (def-foreign-type ,id ,@def)
     ))

;;97Dec01 - New function
(defun bind-c-sizeof (type)
  ;; emitted by
  ;; cmnbind: decode-expression
  (case type
    ((:char :unsigned-char) 
     1)
    ((:short :unsigned-short) 
     2)
    ((:int :unsigned-int
	   :long :unsigned-long
	   :float) 
     4)
    ((:double) 
     8)
    (otherwise (sizeof-fobject type))))

(defmacro bind-c-class (id &rest def)
  ;; emitted by
  ;; cplbind: decode-push-slot-accessors
  `(progn
     (bind-c-export ,id)
     
     ;; This is called only to define opaque-C++class, so we want to
     ;; suppress all constructors and accessors
     ;; Otherwise, C++ constructor gets redefined!
     
     (def-foreign-type ,id ,@def)
     ))

(defmacro bind-c-typedef (id &rest def)
  ;; emitted by 
  ;; cmnbind: decode-push-fwd-def, decode-push-final-def, 
  ;;          decode-macro-defs-to-lisp
  ;; cbind: decode-c-enum-def
  ;; cplbind: decode-c++enum
  `(progn
     (bind-c-export ,id)
     (def-foreign-type ,id ,@def)
     ))


(defmacro bind-c-function (id &rest args &key 
					 all-names
					 unconverted-entry-name 
					 ;;c-return-type
					 return-type
					 ;;c-arg-types
					 c-arg-names
					 arguments
					 ;; call-style
					 (strings-convert 
					  t strings-convert-p)	  
			   &allow-other-keys)
  ;; emitted by
  ;; cmnbind: decode-push-defforeign
  (setf id (if all-names (caar all-names) id))
  `(progn
     (bind-c-export ,id)
     (def-foreign-call
	       (,id ,unconverted-entry-name)
	       ,(mapcar #'(lambda (name type) (list (intern name) type))
			c-arg-names arguments)
	     :returning ,(if (consp return-type)
			     (list return-type)
			   return-type)
	     ;; :convention ???
	     :callback t
	     :call-direct nil
	     :arg-checking t
	     ,@(when strings-convert-p
		 `(:strings-convert ,strings-convert))
	     )
     
     ,@(mapcan
	#'(lambda (x)
	    (unless (eq id (car x))
	      (list
	       (list 'bind-c-alternate (car x) '(&rest args)
		     (list 'list* (list 'quote id) 'args))
	       )))
	all-names)
     ))


