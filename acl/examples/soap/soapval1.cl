;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2003 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2003-2015 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: soapval1.cl,v 2.7 2007/04/17 21:50:41 layer Exp $

;; SOAP server example
;; This example may be submitted to the SOAP validator at http://www.soapware.org/

(in-package :user)

(eval-when (compile load eval) (require :soap))

(defpackage :user (:use :net.xmp.soap)) 

(defpackage :net.xmp.schema (:use) (:nicknames :xs :xsd))
(defpackage :net.xmp.schema-instance (:use) (:nicknames :xsi))
(defpackage :net.xmp.soap.none (:use) (:nicknames :none))
(defpackage :net.xmp.soap.envelope (:use) (:nicknames :env))
(defpackage :net.xmp.soap.encoding (:use) (:nicknames :enc))


;;;
;;; The validation page was at: http://validator.soapware.org/
;;;     
;;;  1. Sart the Lisp server
;;;  2. Go to the validation page
;;;  3. Enter the host, port and path information for the server
;;;  4. Wait for the results
;;;
;;; Top level functions:
;;;  
;;;  make-validator1-server &key port
;;;  try-validator1 &key index port host path

(define-namespace :keyword "vv" "http://validator.soapware.org")

(define-soap-element nil "countTheEntities" 
  '(:complex (:seq (:element "s" xsd:|string|))))

(define-soap-element nil "countTheEntitiesResponse"
  '(:complex
    (:seq
     (:element
      "struct1"
      (:complex
       (:seq
	(:element "ctLeftAngleBrackets" enc:|int|)
	(:element "ctRightAngleBrackets" enc:|int|)
	(:element "ctAmpersands" enc:|int|)
	(:element "ctApostrophes" enc:|int|)
	(:element "ctQuotes" enc:|int|)
	))))))


(define-soap-element nil "easyStructTest"
  '(:complex
    (:seq
     (:element
      "stooges"
      (:complex
       (:set
	(:element "moe" enc:|int|)
	(:element "larry" enc:|int|)
	(:element "curly" enc:|int|)))))))

(define-soap-element nil "easyStructTestResult"
  '(:complex (:seq (:element "number" (:simple enc:|int|)))))


(define-soap-element nil :|myStruct1|
  '(:complex (:seq* (:element nil (:complex (:seq* (:any)))))))

(define-soap-element nil "echoStructTest"
  '(:complex (:seq :|myStruct1|)))

(define-soap-element nil "echoStructTestResult"
  '(:complex (:seq :|myStruct1|)))


(define-soap-element nil "manyTypesTest"
  '(:complex
    (:seq
     (:element "num" xsd:|int|)
     (:element "bool" xsd:|boolean|)
     (:element "state" xsd:|string|)
     (:element "doub"  xsd:|float|)
     (:element "dat"   xsd:|string|)
     (:element "bin"   xsd:|string|))))

(define-soap-element nil "manyTypesTestResult"
  '(:complex
    (:seq
     (:element
      "Result1"
      (:array
       xsd:|ur-type|
       :array-item  (:element "item" :send-type t :argument :type-and-arg)
       :send-type enc:|Array|
       :send-atype xsd:|ur-type|
       :send-asize t
       )
      ))))


(define-soap-element nil "moderateSizeArrayCheck"
  '(:complex (:seq (:element "myArray" (:array xsd:|string|)))))

(define-soap-element nil "moderateSizeArrayCheckResult"
  '(:complex (:seq (:element "result2" (:simple xsd:|string|)))))


(define-soap-element nil "nestedStructTest"
  '(:complex
    (:seq
     (:element
      :|myStruct2|
      (:complex
       (:seq*
	(:element
	 ("year2000" "year2001" "year2002")
	 (:complex
	  (:seq*
	   (:element
	    ("month01" "month02" "month03" "month04")
	    (:complex
	     (:seq* (:element
		     ("day01" "day02" "day03" "day04" "day05" "day06"
		      "day07" "day08" "day09" "day10" "day11" "day12"
		      "day13" "day14" "day15" "day16" "day17" "day18"
		      "day19" "day20" "day21" "day22" "day23" "day24"
		      "day25" "day26" "day27" "day28" "day29" "day30"
		      "day31"
		      )
		     (:complex
		      (:set
		       (:element "moe" enc:|int|)
		       (:element "larry" enc:|int|)
		       (:element "curly" enc:|int|)
		       )))))))))))))))

(define-soap-element nil "nestedStructTestResult"
  '(:complex (:seq (:element "result3" (:simple enc:|int|)))))



(define-soap-element nil "simpleStructReturnTest"
  '(:complex (:seq (:element "myNumber" (:simple enc:|int|)))))

(define-soap-element nil "simpleStructReturnTestResult"
  '(:complex
    (:seq
     (:element
      "struct2"
      (:complex
       (:seq (:element "times10" enc:|int|)
	     (:element "times100" enc:|int|)
	     (:element "times1000" enc:|int|)))))))


(define-soap-element nil "whichToolkit" '(:complex (:seq)))

(define-soap-element nil "whichToolkitResult"
  '(:complex
    (:seq
     (:element
      "struct3"
      (:complex
       (:seq
	(:element "toolkitDocsUrl" xsd:|string|)
	(:element "toolkitName" xsd:|string|)
	(:element "toolkitVersion" xsd:|string|)
	(:element "toolkitOperatingSystem" xsd:|string|)))))))


(defparameter *validator1-path* "/validator1")

(defun make-validator1-server (&key (port 8080) debug)
  (let* ((host "localhost") (path *validator1-path*)
	 (s (soap-message-server
	     :start (list :port port) :enable :start
	     :publish (list :path path)
	     :action :none
	     :lisp-package :keyword :soap-debug debug
	     :url (format nil "http://~A:~A~A" host port path)
	     :message-dns '(nil (:keyword))
	     :decode-flag nil
	     )))

    (soap-export-method s "countTheEntities" (list "s")
			:lisp-name 'validator1-count
			:return "countTheEntitiesResponse")

    (soap-export-method s "easyStructTest" (list "stooges")
			:lisp-name 'validator1-easy-struct
			:return "easyStructTestResult")
    
    (soap-export-method s "echoStructTest" (list :|myStruct1|)
			:lisp-name 'validator1-echo-struct
			:return "echoStructTestResult")

    (soap-export-method s "manyTypesTest" 
			(list "num" "bool" "state" "doub" "dat" "bin")
			:lisp-name 'validator1-many-types
			:return "manyTypesTestResult")

    (soap-export-method s "moderateSizeArrayCheck" (list "myArray")
			:lisp-name 'validator1-array-check
			:return "moderateSizeArrayCheckResult")

    (soap-export-method s "nestedStructTest" (list :|myStruct2|)
			:lisp-name 'validator1-nested-struct
			:return "nestedStructTestResult")

    (soap-export-method s "simpleStructReturnTest" (list "myNumber")
			:lisp-name 'validator1-struct-return
			:return "simpleStructReturnTestResult")

    (soap-export-method s "whichToolkit" ()
			:lisp-name 'validator1-toolkit
			:return "whichToolkitResult")

    ;; Additonal tests not called from web validator
    (soap-export-method s "manyTypesTest2" 
			(list "num1" "bool" "state" "doub" "dat" "bin" "qname")
			:lisp-name 'validator1-many-types2
			:return "manyTypesTestResult")
    (soap-export-method s "manyTypesTest2" 
			(list "num2" "bool" "state" "doub" "dat" "bin" "qname")
			:lisp-name 'validator1-many-types2
			:return "manyTypesTestResult")

    s))


(defun try-validator1 (&key index (port 8080)
			    (host "localhost")
			    (path *validator1-path*))

  ;; this function is to test that the server is responding locally

  (let ((client (soap-message-client :url (format nil "http://~A:~A~A"
						  host port path)
				     :lisp-package :keyword
				     :message-dns '(nil (:keyword))
				     )))
    (flet ((call (this name &rest args)
		 (cond
		  ((null index)
		   (multiple-value-bind (v e)
		       (ignore-errors
			 (multiple-value-list
			  (apply 'call-soap-method client name args)))
		     (if e 
			 (list* this name e (format nil "~A" e))
		       (list* this name v))))
		  ((eql index this)
		   (apply 'call-soap-method client name args)))))
      (list
       client
       (call 0 "countTheEntities" :|s| "abcd<>&'\"efg")
       (call 1 "easyStructTest"
	     :|stooges| (list :|moe| 1 :|larry| 2 :|curly| 3))
       (call 2 "echoStructTest"
	     :|myStruct1|
	     (list :|substruct0| (list :|moe| 1 :|larry| 2 :|curly| 3)
		   :|substruct1| (list :|moe| 11 :|larry| 22 :|curly| 33)))
       (call 3 "manyTypesTest"
	     :|num| 17 :|bool| t :|state| "a string" :|doub| 4.5 :|dat| 12345
	     :|bin| "string to encode")
       (call 4 "moderateSizeArrayCheck"
	     :|myArray| (list "a" "b" "c" "d"))
       (call 5 "nestedStructTest"
	     :|myStruct2|
	     (list :|year2000|
		   (list
		    :|month04| (list :|day01| (list :|moe| 1 :|larry| 2 :|curly| 3)))))
       (call 6 "simpleStructReturnTest" :|myNumber| 123)
       (call 7 "whichToolkit")

       (call 8 "manyTypesTest2"
	     :|num1| 17 :|bool| t :|state| "a string" :|doub| 4.5 :|dat| 12345
	     :|bin| "string to encode"
	     :|qname| 'net.xmp.soap.envelope:|Body|
	     )
       ))))



(defun validator1-count (&key |s| &aux (string |s|))
  ;; countTheEntities   -- returns :struct
  ;; 
  ;; This handler takes a single parameter, a string, 
  ;;  that contains any number of predefined entities,
  ;;  namely <, >, &, ' and ".
  ;; Your handler must return a struct that contains five fields, all numbers: 
  ;;  ctLeftAngleBrackets,  ctRightAngleBrackets, ctAmpersands, ctApostrophes, ctQuotes. 

  (when (listp string) (setf string (apply #'concatenate 'string string)))
  (list "struct1"
	(list
	 "ctLeftAngleBrackets"  (count #\< string)
	 "ctRightAngleBrackets" (count #\> string)
	 "ctAmpersands"         (count #\& string)
	 "ctApostrophes"        (count #\' string)
	 "ctQuotes"             (count #\" string))))

(defun validator1-easy-struct (&key |stooges| &aux item (result 0))
  (dolist (key (list :|moe| :|larry| :|curly|)
	       (list "number" result))
    (if (setf item (soap-sub-element-content |stooges| key))
	(incf result item)
      (error "did not find element ~S" key))))

(defun validator1-echo-struct (&key |myStruct1|) 
  (list :|myStruct1| (soap-alist-to-plist |myStruct1| t)))

(defun validator1-many-types (&key |num| |bool| |state| |doub| |dat| |bin|)
  (list "Result1"
	(list
	 (list 'xsd:|int|      |num|)
	 (list 'xsd:|boolean|  |bool|)
	 (list 'xsd:|string|   |state|)
	 (list 'xsd:|float|    |doub|)
	 (list 'xsd::|timeInstant| |dat|)
	 (list 'xsd:|string|       |bin|)
	 )))

(defun validator1-array-check (&key |myArray|)
  (list "result2"
	(concatenate 'string
		     (aref |myArray| 0)
		     (aref |myArray| (1- (length |myArray|))))))

(defun validator1-nested-struct (&key |myStruct2|)
  (list "result3"
	(let* ((year (assoc :|year2000| |myStruct2|))
	       (april (assoc :|month04| (cdr year)))
	       (day   (assoc :|day01| (cdr april)))
	       (moe   (second (assoc :|moe| (cdr day))))
	       (larry (second (assoc :|larry| (cdr day))))
	       (curly (second (assoc :|curly| (cdr day)))))
	  (+ moe larry curly))))


(defun validator1-struct-return (&key |myNumber|)
  (list :|struct2|
	(list :|times10| (* 10 |myNumber|) 
	      :|times100| (* 100 |myNumber|) 
	      :|times1000| (* 1000 |myNumber|))))
  
(defun validator1-toolkit ()
  (list :|struct3|
	(list :|toolkitDocsUrl| "http://franz.com"
	      :|toolkitName|    "Allegro Common Lisp SOAP"
	      :|toolkitVersion| "x.y"
	      :|toolkitOperatingSystem| "Windows and Unix")))





;;; ADDITIONAL TESTS not called from web validator

(define-soap-element nil "manyTypesTest2"
  '(:complex
    (:seq
     
     ;;(:element "num" xsd:|int|)
     (:or (:element "num1" xsd:|int|)
	  (:element "num2" xsd:|int|))

     (:element "bool" xsd:|boolean|)
     (:element "state" xsd:|string|)
     (:element "doub"  xsd:|float|)
     (:element "dat"   xsd:|string|)
     (:element "bin"   xsd:|string|)
     (:element "qname" xsd:|QName|)     ;;; test [bug15454]
     )))
(define-soap-type nil :struct-of-2 '(:complex (:set (:element "foo" xsd:|string|)
						    (:element "bar" xsd:|string|))))

(defun validator1-many-types2 (&key |num1| |num2| |bool| |state| |doub| |dat| |bin|
				    |qname|
				    )
  (list "Result1"
	(list
	 (list 'xsd:|int|      (or |num1| |num2|))
	 (list 'xsd:|boolean|  |bool|)
	 (list 'xsd:|string|   |state|)
	 (list 'xsd:|float|    |doub|)
	 (list 'xsd::|timeInstant| |dat|)
	 (list 'xsd:|string|       |bin|)
	 (list 'xsd:|QName| |qname|)
	 (list 'xsd:|string| (format nil "~S" |qname|))
	 (list :struct-of-2
	       (list :foo "abc" :bar "def"))
	 )))


