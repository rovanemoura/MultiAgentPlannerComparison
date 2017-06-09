;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2001-2002 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2002-2007 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: xml-rpc-examples.cl,v 2.2 2007/04/17 21:48:16 layer Exp $


(in-package :user)

(eval-when (compile load eval)

  (require :xml-rpc "xml-rpc.fasl")

  (use-package :net.xml-rpc)
  (use-package :net.aserve)
  )


;;;
;;; Some sample applications
;;;

;;; The next 4 definitions illustrate different calling styles
;;;  for a simple method of zero arguments.

(defun try1a ()
  (call-xml-rpc-server 
   '(:url "http://time.xmlrpc.com:80/RPC2") "currentTime.getCurrentTime"))

(defun try1b ()
  (let ((*xml-rpc-url* "http://time.xmlrpc.com:80/RPC2"))
    (call-xml-rpc-server nil "currentTime.getCurrentTime")))

(defun try1c ()
  (xml-rpc-call 
   (encode-xml-rpc-call "currentTime.getCurrentTime")
   :url "http://time.xmlrpc.com:80/RPC2"
   ))

(defun try1d ()
  (let ((*xml-rpc-url* "http://time.xmlrpc.com:80/RPC2"))
    (xml-rpc-call (encode-xml-rpc-call "currentTime.getCurrentTime"))))

;;;
;;; More calls to some public servers

(defun try2 ()
  (xml-rpc-call 
   (encode-xml-rpc-call "system.listMethods")
   :url "http://www.oreillynet.com/meerkat/xml-rpc/server.php"
   ))

(defun try3 ()
  (xml-rpc-call 
   (encode-xml-rpc-call "meerkat.getCategories")
   :url "http://www.oreillynet.com/meerkat/xml-rpc/server.php"
   ))

(defun try4 (&optional (x "system.methodSignature"))
  (xml-rpc-call 
   (encode-xml-rpc-call "system.methodSignature" x)
   :url "http://www.oreillynet.com/meerkat/xml-rpc/server.php"
   ))

(defun try5 (&optional (x "system.methodSignature"))
  (xml-rpc-call 
   (encode-xml-rpc-call "system.methodHelp" x)
   :url "http://www.oreillynet.com/meerkat/xml-rpc/server.php"
   ))





;;;
;;; The following function makes a simple XML-RPC server that exports 
;;;     the Lisp append function.

(defvar *try10*)
(defvar *try10url* "http://localhost:7192/RPC2")
(defun try10 (&optional (port 7192))
  (let ((s (make-xml-rpc-server 
	     :start (list 
		     ;; :host "locahost" 
		     :port port))))
    (export-xml-rpc-method s 'append :array :array :array)
    (setf *try10* s)))

;;; and these make calls to the above server...

(defun try10c (host &optional (port 7192))
  (setf *try10url* (format nil "http://~A:~A/RPC2" host port)))

(defun try11 (&optional (a '(1 2 3)) (b '(4 5 6)))
  ;; Call the append function in the above server
  (xml-rpc-call 
   (encode-xml-rpc-call "append" a b)
   :url *try10url*))
  
(defun try12 ()
  ;; Try introspection on the above server
  (xml-rpc-call 
   (encode-xml-rpc-call "system.listMethods")
   :url *try10url*))

(defun try13 (&optional (method "system.listMethods"))
  ;; Try introspection on the above server
  (xml-rpc-call 
   (encode-xml-rpc-call "system.methodSignature" method)
   :url *try10url*))

(defun try14 (&optional (method "system.listMethods"))
  ;; Try introspection on the above server
  (xml-rpc-call 
   (encode-xml-rpc-call "system.methodHelp" method)
   :url *try10url*))


;;; The server (of custom class)  method sends a custom faultCode
(defclass try15-server (xml-rpc-server)
  ((fault-code :initarg :fault-code)))

(defvar *try15*)
(defvar *try15url* "http://localhost:7192/RPC2")
(defun try15 (&optional (port 7192))
  (let ((s (make-xml-rpc-server 
	    :class `(try15-server :fault-code 123)
	     :start (list 
		     ;; :host "locahost" 
		     :port port))))
    (export-xml-rpc-method s 'try15err :int)
    (setf *try15* s)))

(defun try15c (host &optional (port 7192))
  (setf *try15url* (format nil "http://~A:~A/RPC2" host port)))

(defun try15client ()
  ;; Call the append function in the above server
  (xml-rpc-call 
   (encode-xml-rpc-call "try15err")
   :url *try15url*))

(defun try15err ()
  (error 'xml-rpc-fault 
	 :fault-code (slot-value *xml-rpc-server* 'fault-code)
	 :fault-string "try15err"))



;;;
;;; Another sample application: a bignum server

(defvar *bignum-url* "http://localhost:8081/ACL-bignum-RPC2")
(defun make-bignum-server (&optional (start t) (port 8081))

  (let ((s (make-xml-rpc-server 
	     :start nil :enable t
	     :publish '(:path "/ACL-bignum-RPC2"))))

    (export-xml-rpc-method s 
      '("franz.com.bignumOp" bignum-op t
	"bignumOp(stringOp, stringNum1, ... stringNum4) 
or bignumOp(stringOp, arrayOfStringNums)  returns stringNum.
operation may be  +  -  /  *  ^or** ")
      :string :string)
    (export-xml-rpc-method s 
      '("franz.com.bignumOp" bignum-op t)
      :string :string :string)
    (export-xml-rpc-method s 
      '("franz.com.bignumOp" bignum-op t)
      :string :string :string :string)
    (export-xml-rpc-method s 
      '("franz.com.bignumOp" bignum-op t)
      :string :string :string :string :string)
    (export-xml-rpc-method s 
      '("franz.com.bignumOp" bignum-op t)
      :string :string :string :string :string :string)
    (export-xml-rpc-method s
      '("franz.com.bignumOp" bignum-o[ t)
      :string :string :array)

    (when start (start :port port))
    (enable-xml-rpc-server s)
    s))

(defun bignum-op (op &rest nums)
  (when (consp (first nums)) (setf nums (first nums)))
  (setf nums (mapcar #'(lambda (x) (parse-integer x :junk-allowed t)) nums))
  (let ((*package* (find-package :keyword)))
    (setf op (read-from-string op nil nil)))
  (case op
    (:+ (apply #'+ nums))
    (:- (apply #'- (or nums (list 0))))
    (:* (apply #'* nums))
    (:/ (truncate (or (first nums) 0) (or (second nums) 1)))
    ((:^ :**) (expt (or (first nums) 0) (or (second nums) 1)))
    (otherwise (error "Unknown operation ~A" op))))


(defun try-bignum (&optional op &rest nums &aux (url *bignum-url*))
  (if* (null op)
       then (xml-rpc-call (encode-xml-rpc-call "system.listMethods")
			       :url url)
       else (case (length nums)
	      (0 (xml-rpc-call (encode-xml-rpc-call "franz.com.bignumOp" op)
				    :url url))
	      ((1 2 3 4) 
	       (xml-rpc-call (apply #'encode-xml-rpc-call 
					 "franz.com.bignumOp"
					 op
					 (mapcar #'(lambda (x) (format nil "~A" x))
						 nums))
				  :url url))
	      (otherwise
	       (xml-rpc-call (encode-xml-rpc-call 
				   "franz.com.bignumOp"
				   op
				   (mapcar #'(lambda (x) (format nil "~A" x))
					   nums)))))))


;;;
;;; The remainder of this file defines a server that may be called 
;;;     from one of the XML-RPC public sites to validate the Lisp
;;;     implementation.
;;;
;;; The validation page is at: http://validator.xmlrpc.com/
;;;     
;;;  1. Sart the Lisp server
;;;  2. Go to the validation page
;;;  3. Enter the host and port information for the server
;;;  4. Wait for the results
;;;
;;; Or, use the Lisp client below  --- validator1-client

(defun make-validator1-server (&optional (port 8080))
  (let ((s (make-xml-rpc-server 
	     :start nil :enable t
	     :publish '(:path "/ACL-XML-RPC2"))))

    (export-xml-rpc-method s 
      '("validator1.arrayOfStructsTest" validator1-array-of-struct)
      :int :array)

    (export-xml-rpc-method s 
      '("validator1.countTheEntities" validator1-count)
      :struct :string)

    (export-xml-rpc-method s 
      '("validator1.easyStructTest" validator1-easy-struct)
      :int :struct)

    (export-xml-rpc-method s 
      '("validator1.echoStructTest" validator1-echo-struct)
      :struct :struct)

    (export-xml-rpc-method s 
      '("validator1.manyTypesTest" validator1-many-types)
      :array :int :boolean :string :double :date :base64)

    (export-xml-rpc-method s 
      '("validator1.moderateSizeArrayCheck" validator1-moderate-array)
      :string :array)

    (export-xml-rpc-method s 
      '("validator1.nestedStructTest" validator1-nested-struct)
      :int :struct)

    (export-xml-rpc-method s 
      '("validator1.simpleStructReturnTest" validator1-simple-struct)
      :struct :int)
    
    (start :port port)
    (enable-xml-rpc-server s)
    s))


(defun try-validator1 (&optional (port 8080) (host "localhost"))
  ;; this function is to test that the server is responding locally
  (xml-rpc-call 
   (encode-xml-rpc-call "system.listMethods")
   :url (format nil "http://~A:~A/ACL-XML-RPC2" host port)
   ))



;;;
;;; These are the method implementations expected by the validation tester.

(defun validator1-array-of-struct (array)
  ;; validator1.arrayOfStructsTest -- returns :int
  ;;
  ;; This handler takes a single parameter, an array of structs, 
  ;;  each of which contains at least three elements 
  ;;  named moe, larry and curly, all <i4>s. 
  ;; Your handler must add all the struct elements named curly and return the result.

  (let ((sum 0))
    (dolist (a array)
      (incf sum (xml-rpc-slot-value a "curly"))
      )
    sum))

(defun validator1-count (string)
  ;; validator1.countTheEntities   -- returns :struct
  ;; 
  ;; This handler takes a single parameter, a string, 
  ;;  that contains any number of predefined entities,
  ;;  namely <, >, &, ' and ".
  ;; Your handler must return a struct that contains five fields, all numbers: 
  ;;  ctLeftAngleBrackets,  ctRightAngleBrackets, ctAmpersands, ctApostrophes, ctQuotes. 

  (make-xml-rpc-struct
   "ctLeftAngleBrackets"  (count #\< string)
   "ctRightAngleBrackets" (count #\> string)
   "ctAmpersands"         (count #\& string)
   "ctApostrophes"        (count #\' string)
   "ctQuotes"             (count #\" string)))

(defun validator1-easy-struct (struct)
  ;; validator1.easyStructTest     -- returns :int
  ;;
  ;; This handler takes a single parameter, 
  ;;  a struct, containing at least three elements named moe, larry and curly, 
  ;;  all <i4>s. 
  ;; Your handler must add the three numbers and return the result.

  (+ (xml-rpc-slot-value struct "moe")
     (xml-rpc-slot-value struct "larry")
     (xml-rpc-slot-value struct "curly")))

(defun validator1-echo-struct (struct)
  ;; validator1.echoStructTest     -- returns :struct
  ;; 
  ;; This handler takes a single parameter, a struct. 
  ;;  Your handler must return the struct.

  struct)

(defun validator1-many-types (int bool string double date base64)
  ;; validator1.manyTypesTest      -- returns :array
  ;;
  ;; This handler takes six parameters, 
  ;;  and returns an array containing all the parameters.

  (list int 
	
	;; Here we make an explicit encoding because array elements
	;; are encoded with default encoding based on type.
	(make-xml-rpc-encoding bool :boolean) 
	
	string double

	;; Same for the next two.
	(make-xml-rpc-encoding date :date)
	(make-xml-rpc-encoding base64 :base64)))

(defun validator1-moderate-array (array)
  ;; validator1.moderateSizeArrayCheck  -- returns :string
  ;; 
  ;; This handler takes a single parameter, 
  ;;  which is an array containing between 100 and 200 elements.
  ;;  Each of the items is a string, your handler must return a string 
  ;;  containing the concatenated text of the first and last elements.

  (concatenate 'string (first array) (first (last array))))

(defun validator1-nested-struct (struct)
  ;; validator1.nestedStructTest        -- returns :int
  ;;
  ;; This handler takes a single parameter, a struct, 
  ;;  that models a daily calendar. 
  ;;  At the top level, there is one struct for each year. 
  ;;  Each year is broken down into months, and months into days. 
  ;;  Most of the days are empty in the struct you receive, 
  ;;  but the entry for April 1, 2000 contains a least three elements 
  ;;  named moe, larry and curly, all <i4>s. 
  ;;  Your handler must add the three numbers and return the result.

  (let* ((y2000 (xml-rpc-slot-value struct "2000"))
	 (april (xml-rpc-slot-value y2000 "04"))
	 (april1 (xml-rpc-slot-value april "01"))
	 (moe (xml-rpc-slot-value april1 "moe"))
	 (larry (xml-rpc-slot-value april1 "larry"))
	 (curly (xml-rpc-slot-value april1 "curly")))
    (+ moe larry curly)))

(defun validator1-simple-struct (int)
  ;; validator1.simpleStructReturnTest   -- returns :struct
  ;;
  ;; This handler takes one parameter, and returns a struct containing 
  ;;  three elements, times10, times100 and times1000, 
  ;;  the result of multiplying the number by 10, 100 and 1000.

  (make-xml-rpc-struct
   "times10" (* int 10)
   "times100" (* int 100)
   "times1000" (* int 1000)))


;; A Lisp client that calls the validator.

(defun validator1-client (call &key (port 8080) (host "localhost")
			       &aux (url (format nil "http://~A:~A/ACL-XML-RPC2" host port))
			       (base64 *xml-rpc-base64*))
  (let ((*xml-rpc-base64* base64))
    (xml-rpc-call 
     (case call
       (:structs
	(encode-xml-rpc-call "validator1.arrayOfStructsTest"
			     (list
			      (make-xml-rpc-struct "moe" 11 "larry" 12 "curly" 13) 
			      (make-xml-rpc-struct "moe" 21 "larry" 22 "curly" 23) 
			      (make-xml-rpc-struct "moe" 31 "larry" 32 "curly" 33) 
			      )))
       (:count
	(encode-xml-rpc-call "validator1.countTheEntities" "<<77''\"\">>"))
       (:echo
	(encode-xml-rpc-call "validator1.echoStructTest"
			     (make-xml-rpc-struct "moe" 11 "larry" 12 "curly" 13)))
       (:easy
	(encode-xml-rpc-call "validator1.easyStructTest"
			     (make-xml-rpc-struct "moe" 11 "larry" 12 "curly" 13)))
       (:many
	(encode-xml-rpc-call "validator1.manyTypesTest"
			     16 (make-xml-rpc-encoding nil :boolean) "string" 3.1415 
			     (make-xml-rpc-encoding (list 0 0 12 11 11 2010) :date)
			     (make-xml-rpc-encoding "Foo" :base64)))
       (:many-a
	(setq *xml-rpc-base64* :array)
	(encode-xml-rpc-call "validator1.manyTypesTest"
			     16 (make-xml-rpc-encoding nil :boolean) "string" 3.1415 
			     (make-xml-rpc-encoding (list 0 0 12 11 11 2010) :date)
			     (make-xml-rpc-encoding 
			      (make-array 3 :element-type '(unsigned-byte 8)
					  :initial-element 17)
			      :base64)))
       (:mod
	(encode-xml-rpc-call "validator1.moderateSizeArrayCheck"
			     (make-array 123 :initial-element "x")))
       (:nest
	(encode-xml-rpc-call "validator1.nestedStructTest"
			     (make-xml-rpc-struct
			      "1999" nil
			      "2000" (make-xml-rpc-struct
				      "02" nil "03" nil
				      "04" (make-xml-rpc-struct
					    "01" (make-xml-rpc-struct
						  "moe" 11 "larry" 12 "curly" 13)
					    "02" nil))
			      "2010" nil)))
       (:simple
	(encode-xml-rpc-call "validator1.simpleStructReturnTest" 17))
       )
     :url url)))
  

(defun validator1-client-tests (&key (port 8080) (host "localhost") &aux res)
  (macrolet ((client-test
	      (name val &optional (rform 'res))
	      `(progn
		 (setq res (validator1-client ,name :port port :host host))
		 (or (equalp ',val ,rform)
		     (format t "~&Test ~S failed: ~S~%" ,name res)))))
    (client-test :structs 69)
    (client-test :count "#<xml-rpc-struct :ctQuotes 2:ctApostrophes 2:ctAmpersands 0:ctRightAngleBrackets 2:ctLeftAngleBrackets 2>" (format nil "~S" res))
    (client-test :echo "#<xml-rpc-struct :curly 13:larry 12:moe 11>" (format nil "~S" res))
    (client-test :easy 36)
    (client-test :many (16 nil "string" 3.1415d0 3498465600 "Foo"))
    (client-test :many-a (16 nil "string" 3.1415d0 3498465600 #(17 17 17)))
    (client-test :mod "xx")
    (client-test :nest 36)
    (client-test :simple "#<xml-rpc-struct :times1000 17000:times100 1700:times10 170>" (format nil "~S" res))
    ))