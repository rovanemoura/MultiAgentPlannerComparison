;; This is an automatically generated file.  Make changes in
;; the definition file, not here.

(defpackage :swig.wgl
  (:use :common-lisp :ff :excl)
  (:export #:*swig-identifier-converter* #:*swig-module-name*
           #:*void* #:*swig-expoert-list*))
(in-package :swig.wgl)

(eval-when (compile load eval)
  (defparameter *swig-identifier-converter* 'identifier-convert-null)
  (defparameter *swig-module-name* :wgl))

(defpackage :wgl
  (:use :common-lisp :swig.wgl :ff :excl))


;; $Id: wgl.cl,v 1.2 2009/04/20 18:34:46 layer Exp $

(eval-when (compile load eval)

;;; You can define your own identifier converter if you want.
;;; Use the -identifier-converter command line argument to
;;; specify its name.

(eval-when (:compile-toplevel :load-toplevel :execute)
   (cl::defparameter *swig-export-list* nil))

(cl::defconstant *void* :..void..)

;; parsers to aid in finding SWIG definitions in files.
(cl::defun scm-p1 (form)
  (let* ((info (cl::second form))
	 (id (car info))
	 (id-args (if (eq (cl::car form) 'swig-dispatcher)
		      (cl::cdr info)
		      (cl::cddr info))))
    (cl::apply *swig-identifier-converter* id 
	   (cl::progn (cl::when (cl::eq (cl::car form) 'swig-dispatcher)
		    (cl::remf id-args :arities))
		  id-args))))

(cl::defmacro defswig1 (name (&rest args) &body body)
  `(cl::progn (cl::defmacro ,name ,args
	    ,@body)
	  (excl::define-simple-parser ,name scm-p1)) )

(cl::defmacro defswig2 (name (&rest args) &body body)
  `(cl::progn (cl::defmacro ,name ,args
	    ,@body)
	  (excl::define-simple-parser ,name second)))

(defun read-symbol-from-string (string)
  (cl::multiple-value-bind (result position)
      (cl::read-from-string string nil "eof" :preserve-whitespace t)
    (cl::if (cl::and (cl::symbolp result)
    	             (cl::eql position (cl::length string)))
        result
	(cl::multiple-value-bind (sym)
	    (cl::intern string)
	  sym))))

(cl::defun full-name (id type arity class)
  (cl::case type
    (:getter (cl::format nil "~@[~A_~]~A" class id))
    (:constructor (cl::format nil "new_~A~@[~A~]" id arity))
    (:destructor (cl::format nil "delete_~A" id))
    (:type (cl::format nil "ff_~A" id))
    (:slot id)
    (:ff-operator (cl::format nil "ffi_~A" id))
    (otherwise (cl::format nil "~@[~A_~]~A~@[~A~]"
                       class id arity))))
  
(cl::defun identifier-convert-null (id &key type class arity)
  (cl::if (cl::eq type :setter)
      `(cl::setf ,(identifier-convert-null
               id :type :getter :class class :arity arity))
      (read-symbol-from-string (full-name id type arity class))))

(cl::defun identifier-convert-lispify (cname &key type class arity)
  (cl::assert (cl::stringp cname))
  (cl::when (cl::eq type :setter)
    (cl::return-from identifier-convert-lispify
      `(cl::setf ,(identifier-convert-lispify
               cname :type :getter :class class :arity arity))))
  (cl::setq cname (full-name cname type arity class))
  (cl::if (cl::eq type :constant)
      (cl::setf cname (cl::format nil "*~A*" cname)))
  (cl::setf cname (excl::replace-regexp cname "_" "-"))
  (cl::let ((lastcase :other)
       	    newcase char res)
    (cl::dotimes (n (cl::length cname))
      (cl::setf char (cl::schar cname n))
      (excl::if* (cl::alpha-char-p char)
         then
              (cl::setf newcase (cl::if (cl::upper-case-p char) :upper :lower))

              (cl::when (cl::or (cl::and (cl::eq lastcase :upper)
	      				 (cl::eq newcase :lower))
                                (cl::and (cl::eq lastcase :lower)
					 (cl::eq newcase :upper)))
                ;; case change... add a dash
                (cl::push #\- res)
                (cl::setf newcase :other))

              (cl::push (cl::char-downcase char) res)

              (cl::setf lastcase newcase)

         else
              (cl::push char res)
              (cl::setf lastcase :other)))
    (read-symbol-from-string (cl::coerce (cl::nreverse res) 'string))))

(cl::defun id-convert-and-export (name &rest kwargs)
  (cl::multiple-value-bind (symbol package)
      (cl::apply *swig-identifier-converter* name kwargs)
    (cl::let ((args (cl::list (cl::if (cl::consp symbol)
    	     	    	         (cl::cadr symbol) symbol)
                      (cl::or package cl::*package*))))
      (cl::apply #'cl::export args)
      (cl::pushnew args *swig-export-list*))
    symbol))

(cl::defmacro swig-insert-id (name namespace &key (type :type) class)
  `(cl::let ((cl::*package* (cl::find-package ,(package-name-for-namespace namespace))))
    (id-convert-and-export ,name :type ,type :class ,class)))

(defswig2 swig-defconstant (string value)
  (cl::let ((symbol (id-convert-and-export string :type :constant)))
    `(cl::eval-when (compile load eval)
       (cl::defconstant ,symbol ,value))))

(cl::defun maybe-reorder-args (funcname arglist)
  ;; in the foreign setter function the new value will be the last argument
  ;; in Lisp it needs to be the first
  (cl::if (cl::consp funcname)
      (cl::append (cl::last arglist) (cl::butlast arglist))
      arglist))

(cl::defun maybe-return-value (funcname arglist)
  ;; setf functions should return the new value
  (cl::when (cl::consp funcname)
    `(,(cl::if (cl::consp (cl::car arglist))
           (cl::caar arglist)
           (cl::car arglist)))))

(cl::defun swig-anyvarargs-p (arglist)
  (cl::member :SWIG__varargs_ arglist))

(defswig1 swig-defun ((name &optional (mangled-name name)
                            &key (type :operator) class arity)
                      arglist kwargs
		      &body body)
  (cl::let* ((symbol (id-convert-and-export name :type type
                          :arity arity :class class))
             (mangle (excl::if* (cl::string-equal name mangled-name)
                      then (id-convert-and-export 
				    (cl::cond
					  ((cl::eq type :setter) (cl::format nil "~A-set" name))
					  ((cl::eq type :getter) (cl::format nil "~A-get" name))
					  (t name))
				    :type :ff-operator :arity arity :class class)
                      else (cl::intern mangled-name)))
         (defun-args (maybe-reorder-args
                      symbol
		      (cl::mapcar #'cl::car (cl::and (cl::not (cl::equal arglist '(:void)))
					 (cl::loop as i in arglist
					       when (cl::eq (cl::car i) :p+)
					       collect (cl::cdr i))))))
	 (ffargs (cl::if (cl::equal arglist '(:void))
	 	      arglist
		    (cl::mapcar #'cl::cdr arglist)))
	 )
    (cl::when (swig-anyvarargs-p ffargs)
      (cl::setq ffargs '()))
    `(cl::eval-when (compile load eval)
       (excl::compiler-let ((*record-xref-info* nil))
         (ff:def-foreign-call (,mangle ,mangled-name) ,ffargs ,@kwargs))
       (cl::macrolet ((swig-ff-call (&rest args)
                      (cl::cons ',mangle args)))
         (cl::defun ,symbol ,defun-args
           ,@body
           ,@(maybe-return-value symbol defun-args))))))

(defswig1 swig-defmethod ((name &optional (mangled-name name)
	  	                &key (type :operator) class arity)
                          ffargs kwargs
                          &body body)
  (cl::let* ((symbol (id-convert-and-export name :type type
                          :arity arity :class class))
         (mangle (cl::intern mangled-name))
         (defmethod-args (maybe-reorder-args
                          symbol
                          (cl::unless (cl::equal ffargs '(:void))
                            (cl::loop for (lisparg name dispatch) in ffargs
			    	  when (eq lisparg :p+)
                                  collect `(,name ,dispatch)))))
         (ffargs (cl::if (cl::equal ffargs '(:void))
                     ffargs
                     (cl::loop for (nil name nil . ffi) in ffargs
                           collect `(,name ,@ffi)))))
    `(cl::eval-when (compile load eval)
       (excl::compiler-let ((*record-xref-info* nil))
         (ff:def-foreign-call (,mangle ,mangled-name) ,ffargs ,@kwargs))
       (cl::macrolet ((swig-ff-call (&rest args)
                      (cl::cons ',mangle args)))
         (cl::defmethod ,symbol ,defmethod-args
           ,@body
           ,@(maybe-return-value symbol defmethod-args))))))

(defswig1 swig-dispatcher ((name &key (type :operator) class arities))
  (cl::let ((symbol (id-convert-and-export name
                         :type type :class class)))
    `(cl::eval-when (compile load eval)
       (cl::defun ,symbol (&rest args)
         (cl::case (cl::length args)
           ,@(cl::loop for arity in arities
                   for symbol-n = (id-convert-and-export name
                                           :type type :class class :arity arity)
                   collect `(,arity (cl::apply #',symbol-n args)))
	   (t (cl::error "No applicable wrapper-methods for foreign call ~a with args ~a of classes ~a" ',symbol args (cl::mapcar #'(cl::lambda (x) (cl::class-name (cl::class-of x))) args)))
	   )))))

(defswig2 swig-def-foreign-stub (name)
  (cl::let ((lsymbol (id-convert-and-export name :type :class))
	    (symbol (id-convert-and-export name :type :type)))
    `(cl::eval-when (compile load eval)
	(ff:def-foreign-type ,symbol (:class ))
	(cl::defclass ,lsymbol (ff:foreign-pointer) ()))))

(defswig2 swig-def-foreign-class (name supers &rest rest)
  (cl::let ((lsymbol (id-convert-and-export name :type :class))
	    (symbol (id-convert-and-export name :type :type)))
    `(cl::eval-when (compile load eval)
       (ff:def-foreign-type ,symbol ,@rest)
       (cl::defclass ,lsymbol ,supers
	 ((foreign-type :initform ',symbol :initarg :foreign-type
			:accessor foreign-pointer-type))))))

(defswig2 swig-def-foreign-type (name &rest rest)
  (cl::let ((symbol (id-convert-and-export name :type :type)))
    `(cl::eval-when (compile load eval)
       (ff:def-foreign-type ,symbol ,@rest))))

(defswig2 swig-def-synonym-type (synonym of ff-synonym)
  `(cl::eval-when (compile load eval)
     (cl::setf (cl::find-class ',synonym) (cl::find-class ',of))
     (ff:def-foreign-type ,ff-synonym (:struct ))))

(cl::defun package-name-for-namespace (namespace)
  (excl::list-to-delimited-string
   (cl::cons *swig-module-name*
         (cl::mapcar #'(cl::lambda (name)
                     (cl::string
                      (cl::funcall *swig-identifier-converter*
                               name
                               :type :namespace)))
                 namespace))
   "."))

(cl::defmacro swig-defpackage (namespace)
  (cl::let* ((parent-namespaces (cl::maplist #'cl::reverse (cl::cdr (cl::reverse namespace))))
             (parent-strings (cl::mapcar #'package-name-for-namespace
                                 parent-namespaces))
             (string (package-name-for-namespace namespace)))
    `(cl::eval-when (compile load eval)
      (cl::defpackage ,string
        (:use :swig :ff #+ignore '(:common-lisp :ff :excl)
              ,@parent-strings ,*swig-module-name*)
	(:import-from :cl :* :nil :t)))))

(cl::defmacro swig-in-package (namespace)
  `(cl::eval-when (compile load eval)
    (cl::in-package ,(package-name-for-namespace namespace))))

(defswig2 swig-defvar (name mangled-name &key type)
  (cl::let ((symbol (id-convert-and-export name :type type)))
    `(cl::eval-when (compile load eval)
      (ff:def-foreign-variable (,symbol ,mangled-name)))))

) ;; eval-when

(cl::eval-when (compile eval)
  (cl::flet ((starts-with-p (str prefix)
              (cl::and (cl::>= (cl::length str) (cl::length prefix))
                (cl::string= str prefix :end1 (cl::length prefix)))))
    (cl::export (cl::loop for sym being each present-symbol of cl::*package*
                  when (cl::or (starts-with-p (cl::symbol-name sym) (cl::symbol-name :swig-))
                           (starts-with-p (cl::symbol-name sym) (cl::symbol-name :identifier-convert-)))
                  collect sym))))


(in-package :wgl)
(swig-def-foreign-type "PROC"
  (* :void))
(swig-def-foreign-type "CHAR"
  :char)
(swig-def-foreign-type "SHORT"
  :short)
(swig-def-foreign-type "LONG"
  :long)
(swig-def-foreign-type "PVOID"
  (* :void))
(swig-def-foreign-type "HANDLE"
  #.(swig-insert-id "PVOID" () :type :type))
(swig-def-foreign-type "PCHAR"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "LPCH"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "PCH"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "LPCCH"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "PCCH"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "NPSTR"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "LPSTR"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "PSTR"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "LPCSTR"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "PCSTR"
  (* #.(swig-insert-id "CHAR" () :type :type)))
(swig-def-foreign-type "HDC"
  #.(swig-insert-id "HANDLE" () :type :type))
(swig-def-foreign-type "HGLRC"
  #.(swig-insert-id "HANDLE" () :type :type))
(swig-def-foreign-type "HENHMETAFILE"
  #.(swig-insert-id "HANDLE" () :type :type))
(swig-def-foreign-type "DWORD"
  :unsigned-long)
(swig-def-foreign-type "BOOL"
  :int)
(swig-def-foreign-type "BYTE"
  :unsigned-char)
(swig-def-foreign-type "WORD"
  :unsigned-short)
(swig-def-foreign-type "FLOAT"
  :float)
(swig-def-foreign-type "PFLOAT"
  (* #.(swig-insert-id "FLOAT" () :type :type)))
(swig-def-foreign-type "PBOOL"
  (* #.(swig-insert-id "BOOL" () :type :type)))
(swig-def-foreign-type "LPBOOL"
  (* #.(swig-insert-id "BOOL" () :type :type)))
(swig-def-foreign-type "PBYTE"
  (* #.(swig-insert-id "BYTE" () :type :type)))
(swig-def-foreign-type "LPBYTE"
  (* #.(swig-insert-id "BYTE" () :type :type)))
(swig-def-foreign-type "PINT"
  (* :int))
(swig-def-foreign-type "LPINT"
  (* :int))
(swig-def-foreign-type "PWORD"
  (* #.(swig-insert-id "WORD" () :type :type)))
(swig-def-foreign-type "LPWORD"
  (* #.(swig-insert-id "WORD" () :type :type)))
(swig-def-foreign-type "LPLONG"
  (* :long))
(swig-def-foreign-type "PDWORD"
  (* #.(swig-insert-id "DWORD" () :type :type)))
(swig-def-foreign-type "LPDWORD"
  (* #.(swig-insert-id "DWORD" () :type :type)))
(swig-def-foreign-type "LPVOID"
  (* :void))
(swig-def-foreign-type "LPCVOID"
  (* :void))
(swig-def-foreign-type "INT"
  :int)
(swig-def-foreign-type "UINT"
  :unsigned-int)
(swig-def-foreign-type "PUINT"
  (* :unsigned-int))
(swig-def-foreign-type "COLORREF"
  #.(swig-insert-id "DWORD" () :type :type))
(swig-def-foreign-type "LPCOLORREF"
  (* #.(swig-insert-id "DWORD" () :type :type)))
(swig-def-foreign-class "POINTFLOAT"
 (ff:foreign-pointer)
  (:struct
   (#.(swig-insert-id "x" () :type :slot :class "POINTFLOAT") #.(swig-insert-id "FLOAT" () :type :type))
   (#.(swig-insert-id "y" () :type :slot :class "POINTFLOAT") #.(swig-insert-id "FLOAT" () :type :type))
   ))

(swig-def-foreign-type "PPOINTFLOAT"
  (* #.(swig-insert-id "POINTFLOAT" () :type :type)))
(swig-def-foreign-class "GLYPHMETRICSFLOAT"
 (ff:foreign-pointer)
  (:struct
   (#.(swig-insert-id "gmfBlackBoxX" () :type :slot :class "GLYPHMETRICSFLOAT") #.(swig-insert-id "FLOAT" () :type :type))
   (#.(swig-insert-id "gmfBlackBoxY" () :type :slot :class "GLYPHMETRICSFLOAT") #.(swig-insert-id "FLOAT" () :type :type))
   (#.(swig-insert-id "gmfptGlyphOrigin" () :type :slot :class "GLYPHMETRICSFLOAT") #.(swig-insert-id "POINTFLOAT" () :type :type))
   (#.(swig-insert-id "gmfCellIncX" () :type :slot :class "GLYPHMETRICSFLOAT") #.(swig-insert-id "FLOAT" () :type :type))
   (#.(swig-insert-id "gmfCellIncY" () :type :slot :class "GLYPHMETRICSFLOAT") #.(swig-insert-id "FLOAT" () :type :type))
   ))

(swig-def-foreign-type "PGLYPHMETRICSFLOAT"
  (* #.(swig-insert-id "GLYPHMETRICSFLOAT" () :type :type)))
(swig-def-foreign-type "LPGLYPHMETRICSFLOAT"
  (* #.(swig-insert-id "GLYPHMETRICSFLOAT" () :type :type)))
(swig-def-foreign-class "LAYERPLANEDESCRIPTOR"
 (ff:foreign-pointer)
  (:struct
   (#.(swig-insert-id "nSize" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "WORD" () :type :type))
   (#.(swig-insert-id "nVersion" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "WORD" () :type :type))
   (#.(swig-insert-id "dwFlags" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "DWORD" () :type :type))
   (#.(swig-insert-id "iPixelType" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cColorBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cRedBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cRedShift" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cGreenBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cGreenShift" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cBlueBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cBlueShift" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAlphaBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAlphaShift" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumRedBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumGreenBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumBlueBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumAlphaBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cDepthBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cStencilBits" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAuxBuffers" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "iLayerPlane" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "bReserved" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "crTransparent" () :type :slot :class "LAYERPLANEDESCRIPTOR") #.(swig-insert-id "COLORREF" () :type :type))
   ))

(swig-def-foreign-type "PLAYERPLANEDESCRIPTOR"
  (* #.(swig-insert-id "LAYERPLANEDESCRIPTOR" () :type :type)))
(swig-def-foreign-type "LPLAYERPLANEDESCRIPTOR"
  (* #.(swig-insert-id "LAYERPLANEDESCRIPTOR" () :type :type)))
(swig-def-foreign-class "PIXELFORMATDESCRIPTOR"
 (ff:foreign-pointer)
  (:struct
   (#.(swig-insert-id "nSize" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "WORD" () :type :type))
   (#.(swig-insert-id "nVersion" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "WORD" () :type :type))
   (#.(swig-insert-id "dwFlags" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "DWORD" () :type :type))
   (#.(swig-insert-id "iPixelType" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cColorBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cRedBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cRedShift" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cGreenBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cGreenShift" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cBlueBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cBlueShift" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAlphaBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAlphaShift" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumRedBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumGreenBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumBlueBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAccumAlphaBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cDepthBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cStencilBits" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "cAuxBuffers" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "iLayerType" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "bReserved" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "BYTE" () :type :type))
   (#.(swig-insert-id "dwLayerMask" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "DWORD" () :type :type))
   (#.(swig-insert-id "dwVisibleMask" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "DWORD" () :type :type))
   (#.(swig-insert-id "dwDamageMask" () :type :slot :class "PIXELFORMATDESCRIPTOR") #.(swig-insert-id "DWORD" () :type :type))
   ))

(swig-def-foreign-type "PPIXELFORMATDESCRIPTOR"
  (* #.(swig-insert-id "PIXELFORMATDESCRIPTOR" () :type :type)))
(swig-def-foreign-type "LPPIXELFORMATDESCRIPTOR"
  (* #.(swig-insert-id "PIXELFORMATDESCRIPTOR" () :type :type)))

(swig-in-package ())

(swig-defun ("wglCopyContext" "wglCopyContext")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HGLRC" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "HGLRC" () :type :type) )
   (:p+ PARM2_arg3  #.(swig-insert-id "UINT" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2)))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglCreateContext" "wglCreateContext")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) ))
  (:returning (#.(swig-insert-id "HGLRC" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0)))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglCreateLayerContext" "wglCreateLayerContext")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  :int ))
  (:returning (#.(swig-insert-id "HGLRC" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglDeleteContext" "wglDeleteContext")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HGLRC" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0)))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglGetCurrentContext" "wglGetCurrentContext")
  (:void)
  (:returning (#.(swig-insert-id "HGLRC" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::setq ACL_ffresult (swig-ff-call))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglGetCurrentDC" "wglGetCurrentDC")
  (:void)
  (:returning (#.(swig-insert-id "HDC" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::setq ACL_ffresult (swig-ff-call))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglGetProcAddress" "wglGetProcAddress")
  ((:p+ PARM0_arg1  #.(swig-insert-id "LPCSTR" () :type :type) ))
  (:returning (#.(swig-insert-id "PROC" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0)))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglMakeCurrent" "wglMakeCurrent")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "HGLRC" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglShareLists" "wglShareLists")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HGLRC" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "HGLRC" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglUseFontBitmapsA" "wglUseFontBitmapsA")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM2_arg3  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM3_arg4  #.(swig-insert-id "DWORD" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::let ((SWIG_arg3 PARM3_arg4))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2 SWIG_arg3))))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglUseFontBitmapsW" "wglUseFontBitmapsW")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM2_arg3  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM3_arg4  #.(swig-insert-id "DWORD" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::let ((SWIG_arg3 PARM3_arg4))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2 SWIG_arg3))))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("SwapBuffers" "SwapBuffers")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0)))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defconstant "WGL_FONT_LINES" 0)
(swig-defconstant "WGL_FONT_POLYGONS" 1)
(swig-defun ("wglUseFontOutlinesA" "wglUseFontOutlinesA")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM2_arg3  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM3_arg4  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM4_arg5  #.(swig-insert-id "FLOAT" () :type :type) )
   (:p+ PARM5_arg6  #.(swig-insert-id "FLOAT" () :type :type) )
   (:p+ PARM6_arg7  :int )
   (:p+ PARM7_arg8  #.(swig-insert-id "LPGLYPHMETRICSFLOAT" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::let ((SWIG_arg3 PARM3_arg4))
  (cl::let ((SWIG_arg4 PARM4_arg5))
  (cl::let ((SWIG_arg5 PARM5_arg6))
  (cl::let ((SWIG_arg6 PARM6_arg7))
  (cl::let ((SWIG_arg7 PARM7_arg8))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2 SWIG_arg3 SWIG_arg4 SWIG_arg5 SWIG_arg6 SWIG_arg7))))))))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglUseFontOutlinesW" "wglUseFontOutlinesW")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM2_arg3  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM3_arg4  #.(swig-insert-id "DWORD" () :type :type) )
   (:p+ PARM4_arg5  #.(swig-insert-id "FLOAT" () :type :type) )
   (:p+ PARM5_arg6  #.(swig-insert-id "FLOAT" () :type :type) )
   (:p+ PARM6_arg7  :int )
   (:p+ PARM7_arg8  #.(swig-insert-id "LPGLYPHMETRICSFLOAT" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::let ((SWIG_arg3 PARM3_arg4))
  (cl::let ((SWIG_arg4 PARM4_arg5))
  (cl::let ((SWIG_arg5 PARM5_arg6))
  (cl::let ((SWIG_arg6 PARM6_arg7))
  (cl::let ((SWIG_arg7 PARM7_arg8))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2 SWIG_arg3 SWIG_arg4 SWIG_arg5 SWIG_arg6 SWIG_arg7))))))))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defconstant "LPD_DOUBLEBUFFER" #x00000001)
(swig-defconstant "LPD_STEREO" #x00000002)
(swig-defconstant "LPD_SUPPORT_GDI" #x00000010)
(swig-defconstant "LPD_SUPPORT_OPENGL" #x00000020)
(swig-defconstant "LPD_SHARE_DEPTH" #x00000040)
(swig-defconstant "LPD_SHARE_STENCIL" #x00000080)
(swig-defconstant "LPD_SHARE_ACCUM" #x00000100)
(swig-defconstant "LPD_SWAP_EXCHANGE" #x00000200)
(swig-defconstant "LPD_SWAP_COPY" #x00000400)
(swig-defconstant "LPD_TRANSPARENT" #x00001000)
(swig-defconstant "LPD_TYPE_RGBA" 0)
(swig-defconstant "LPD_TYPE_COLORINDEX" 1)
(swig-defconstant "WGL_SWAP_MAIN_PLANE" #x00000001)
(swig-defconstant "WGL_SWAP_OVERLAY1" #x00000002)
(swig-defconstant "WGL_SWAP_OVERLAY2" #x00000004)
(swig-defconstant "WGL_SWAP_OVERLAY3" #x00000008)
(swig-defconstant "WGL_SWAP_OVERLAY4" #x00000010)
(swig-defconstant "WGL_SWAP_OVERLAY5" #x00000020)
(swig-defconstant "WGL_SWAP_OVERLAY6" #x00000040)
(swig-defconstant "WGL_SWAP_OVERLAY7" #x00000080)
(swig-defconstant "WGL_SWAP_OVERLAY8" #x00000100)
(swig-defconstant "WGL_SWAP_OVERLAY9" #x00000200)
(swig-defconstant "WGL_SWAP_OVERLAY10" #x00000400)
(swig-defconstant "WGL_SWAP_OVERLAY11" #x00000800)
(swig-defconstant "WGL_SWAP_OVERLAY12" #x00001000)
(swig-defconstant "WGL_SWAP_OVERLAY13" #x00002000)
(swig-defconstant "WGL_SWAP_OVERLAY14" #x00004000)
(swig-defconstant "WGL_SWAP_OVERLAY15" #x00008000)
(swig-defconstant "WGL_SWAP_UNDERLAY1" #x00010000)
(swig-defconstant "WGL_SWAP_UNDERLAY2" #x00020000)
(swig-defconstant "WGL_SWAP_UNDERLAY3" #x00040000)
(swig-defconstant "WGL_SWAP_UNDERLAY4" #x00080000)
(swig-defconstant "WGL_SWAP_UNDERLAY5" #x00100000)
(swig-defconstant "WGL_SWAP_UNDERLAY6" #x00200000)
(swig-defconstant "WGL_SWAP_UNDERLAY7" #x00400000)
(swig-defconstant "WGL_SWAP_UNDERLAY8" #x00800000)
(swig-defconstant "WGL_SWAP_UNDERLAY9" #x01000000)
(swig-defconstant "WGL_SWAP_UNDERLAY10" #x02000000)
(swig-defconstant "WGL_SWAP_UNDERLAY11" #x04000000)
(swig-defconstant "WGL_SWAP_UNDERLAY12" #x08000000)
(swig-defconstant "WGL_SWAP_UNDERLAY13" #x10000000)
(swig-defconstant "WGL_SWAP_UNDERLAY14" #x20000000)
(swig-defconstant "WGL_SWAP_UNDERLAY15" #x40000000)
(swig-defun ("wglDescribeLayerPlane" "wglDescribeLayerPlane")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  :int )
   (:p+ PARM2_arg3  :int )
   (:p+ PARM3_arg4  #.(swig-insert-id "UINT" () :type :type) )
   (:p+ PARM4_arg5  #.(swig-insert-id "LPLAYERPLANEDESCRIPTOR" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::let ((SWIG_arg3 PARM3_arg4))
  (cl::let ((SWIG_arg4 PARM4_arg5))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2 SWIG_arg3 SWIG_arg4)))))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglSetLayerPaletteEntries" "wglSetLayerPaletteEntries")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  :int )
   (:p+ PARM2_arg3  :int )
   (:p+ PARM3_arg4  :int )
   (:p+ PARM4_arg5  (* #.(swig-insert-id "COLORREF" () :type :type)) ))
  (:returning (:int )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::let ((SWIG_arg3 PARM3_arg4))
  (cl::let ((SWIG_arg4 PARM4_arg5))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2 SWIG_arg3 SWIG_arg4)))))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglGetLayerPaletteEntries" "wglGetLayerPaletteEntries")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  :int )
   (:p+ PARM2_arg3  :int )
   (:p+ PARM3_arg4  :int )
   (:p+ PARM4_arg5  (* #.(swig-insert-id "COLORREF" () :type :type)) ))
  (:returning (:int )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::let ((SWIG_arg3 PARM3_arg4))
  (cl::let ((SWIG_arg4 PARM4_arg5))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2 SWIG_arg3 SWIG_arg4)))))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglRealizeLayerPalette" "wglRealizeLayerPalette")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  :int )
   (:p+ PARM2_arg3  #.(swig-insert-id "BOOL" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2)))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("wglSwapLayerBuffers" "wglSwapLayerBuffers")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "UINT" () :type :type) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defconstant "ERROR_INVALID_PIXEL_FORMAT" 2000)
(swig-defconstant "ERROR_BAD_DRIVER" 2001)
(swig-defconstant "ERROR_INVALID_WINDOW_STYLE" 2002)
(swig-defconstant "ERROR_METAFILE_NOT_SUPPORTED" 2003)
(swig-defconstant "ERROR_TRANSFORM_NOT_SUPPORTED" 2004)
(swig-defconstant "ERROR_CLIPPING_NOT_SUPPORTED" 2005)
(swig-defconstant "PFD_TYPE_RGBA" 0)
(swig-defconstant "PFD_TYPE_COLORINDEX" 1)
(swig-defconstant "PFD_MAIN_PLANE" 0)
(swig-defconstant "PFD_OVERLAY_PLANE" 1)
(swig-defconstant "PFD_UNDERLAY_PLANE" -1)
(swig-defconstant "PFD_DOUBLEBUFFER" #x00000001)
(swig-defconstant "PFD_STEREO" #x00000002)
(swig-defconstant "PFD_DRAW_TO_WINDOW" #x00000004)
(swig-defconstant "PFD_DRAW_TO_BITMAP" #x00000008)
(swig-defconstant "PFD_SUPPORT_GDI" #x00000010)
(swig-defconstant "PFD_SUPPORT_OPENGL" #x00000020)
(swig-defconstant "PFD_GENERIC_FORMAT" #x00000040)
(swig-defconstant "PFD_NEED_PALETTE" #x00000080)
(swig-defconstant "PFD_NEED_SYSTEM_PALETTE" #x00000100)
(swig-defconstant "PFD_SWAP_EXCHANGE" #x00000200)
(swig-defconstant "PFD_SWAP_COPY" #x00000400)
(swig-defconstant "PFD_SWAP_LAYER_BUFFERS" #x00000800)
(swig-defconstant "PFD_GENERIC_ACCELERATED" #x00001000)
(swig-defconstant "PFD_SUPPORT_DIRECTDRAW" #x00002000)
(swig-defconstant "PFD_DEPTH_DONTCARE" #x20000000)
(swig-defconstant "PFD_DOUBLEBUFFER_DONTCARE" #x40000000)
(swig-defconstant "PFD_STEREO_DONTCARE" #x80000000)
(swig-defun ("ChoosePixelFormat" "ChoosePixelFormat")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  (* #.(swig-insert-id "PIXELFORMATDESCRIPTOR" () :type :type)) ))
  (:returning (:int )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("DescribePixelFormat" "DescribePixelFormat")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  :int )
   (:p+ PARM2_arg3  #.(swig-insert-id "UINT" () :type :type) )
   (:p+ PARM3_arg4  #.(swig-insert-id "LPPIXELFORMATDESCRIPTOR" () :type :type) ))
  (:returning (:int )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::let ((SWIG_arg3 PARM3_arg4))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2 SWIG_arg3))))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("GetEnhMetaFilePixelFormat" "GetEnhMetaFilePixelFormat")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HENHMETAFILE" () :type :type) )
   (:p+ PARM1_arg2  #.(swig-insert-id "UINT" () :type :type) )
   (:p+ PARM2_arg3  (* #.(swig-insert-id "PIXELFORMATDESCRIPTOR" () :type :type)) ))
  (:returning (#.(swig-insert-id "UINT" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2)))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("GetPixelFormat" "GetPixelFormat")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) ))
  (:returning (:int )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0)))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))


(swig-defun ("SetPixelFormat" "SetPixelFormat")
  ((:p+ PARM0_arg1  #.(swig-insert-id "HDC" () :type :type) )
   (:p+ PARM1_arg2  :int )
   (:p+ PARM2_arg3  (* #.(swig-insert-id "PIXELFORMATDESCRIPTOR" () :type :type)) ))
  (:returning (#.(swig-insert-id "BOOL" () :type :type) )
   :strings-convert t)
  (cl::let ((ACL_ffresult swig.wgl:*void*)
        ACL_result)
  (cl::let ((SWIG_arg0 PARM0_arg1))
  (cl::let ((SWIG_arg1 PARM1_arg2))
  (cl::let ((SWIG_arg2 PARM2_arg3))
  (cl::setq ACL_ffresult (swig-ff-call SWIG_arg0 SWIG_arg1 SWIG_arg2)))))
  (cl::if (cl::eq ACL_ffresult swig.wgl:*void*)
    (cl::values-list ACL_result)
   (cl::values-list (cl::cons ACL_ffresult ACL_result)))))



(in-package :swig.wgl)

(macrolet ((swig-do-export ()
                 `(dolist (s ',*swig-export-list*)
                    (apply #'export s))))
   (swig-do-export))

(setq *swig-export-list* nil)

