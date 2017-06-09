
;; This work in the Public Domain, thereby relinquishing all
;; copyrights. Everyone is free to use, modify, republish, sell or give
;; away this work without prior consent from anybody.

;; This code is provided on an "as is" basis, without warranty of any
;; kind. Use at your own risk! Under no circumstances shall the author(s)
;; or contributor(s) be liable for damages resulting directly or indirectly
;; from the use or non-use of this program.

(in-package :user)

(eval-when (compile load eval)

  ;; This form is needed to make the SOAP module avaialble
  (require :soap)

  ;; This form is to let us mention exported symbols of the SOAP module 
  ;; without a package qualifier.
  (use-package :net.xmp.soap)

  )

;; This form is to allow a short package prefix for Schema symbols
(defpackage :net.xmp.schema (:use) (:nicknames :xsd))



;;;
;;; The Lisp part of this application
;;;

;; A Lisp function to translate
;;   a string to a function name symbol
;;   a second string to an arbitrarily large integer
;;   a third string to a second integer
;; The result is a string of digits representing the answer.
(defun calculate (op arg1 arg2)
  (let* ((opsym (read-from-string (format nil "~A" op)))
	 (num1 (parse-integer arg1))
	 (num2 (parse-integer arg2)))
    (case opsym
      ((+ - * ash truncate ceiling expt factorial gcd rem)
       (format nil "~A" (funcall opsym num1 num2)))
      (otherwise (error "Unknown operation"))
      )))

;; A Lisp function to translate a string of digits into a list
;;   of digits in some arbitrary base.
;; This function allows a foreign language caller to view the
;;   large integer as a vector of integers within the integer
;;   range of the foreign language.
(defun decode-num (string base)
  (let ((num (parse-integer string)) rem res)
    (loop
     (multiple-value-setq (num rem) (truncate num base))
     (push rem res)
     (when (zerop num) (return)))
    res))
	
;; A Lisp function to translate a sequence of digits into
;;   a string representing the actual number.
;; Each digit appears as the list (:item digit)
(defun encode-num (seq base &aux (res 0))
  (dotimes (i (length seq) (format nil "~A" res))
    (setf res (+ (* res base) (second (elt seq i))))))

;; The most popular function to generate really big integers.
(defun factorial (n dummy &aux (r 1))
  (declare (ignore dummy))
  (loop
   (when (< n 2) (return r))
   (setf r (* r n))
   (decf n)))



;;;
;;; The SOAP interface to the application
;;;

;; Create a package to hold the namespace-qualified symbols in this 
;; application
(defpackage :bn (:use))

;; Define one namespace-to-package mapping
(define-namespace :bn "bn" "urn:bignumserver")

;; Define a named namespace-to-package mapping
(define-namespace-map :bnm nil '(:bn))


(define-soap-element nil 'bn::|calculate|
  '(:complex
    (:seq 
     (:element "opname" xsd:|string|)
     (:element "num1"   xsd:|string|)
     (:element "num2"   xsd:|string|))
    :action "calculate"
    :namespaces :bnm
    ))

(define-soap-element nil "calculateResponse"
  `(:complex
    (:seq
     (:element "calcResult" xsd:|string|))))


(define-soap-element nil  'bn::|decodeNum|
  '(:complex
    (:seq
     (:element "num" xsd:|string|)
     (:element "base" xsd:|int|))
    :action "decodeNum"
    :namespaces :bnm
    ))

(define-soap-type nil 'bn::|arrayOfBigits|
  '(:complex (:seq* (:element "item" xsd:|int|))))

(define-soap-element nil "decodeNumResponse"
  '(:complex
    (:seq
     (:element "decResult" bn::|arrayOfBigits|))))


(define-soap-element nil 'bn::|encodeNum|
  `(:complex
    (:seq
     (:element "bigits" bn::|arrayOfBigits|)
     (:element "base"  xsd:|int|))
    :action "encodeNum"
    :namespaces :bnm
    ))

(define-soap-element nil "encodeNumResponse"
  `(:complex
    (:seq
     (:element "encResult" xsd:|string|))))



(defvar *bn-server* nil)
(defun start-server (&key debug (port 1776) (host "localhost"))
  (let ((server (soap-message-server :start (list :port port)
				     :lisp-package :keyword
				     :message-dns :bnm
				     :url (format nil "http://~A:~A/SOAP" host port)
				     :service-name "BigNumService"
				     :soap-debug debug
				     )))
    (soap-export-method
     server 'bn::|calculate| '("opname" "num1" "num2")
     :lisp-name 'calculate-method
     :return "calculateResponse"
     :action "calculate"
     )
    (soap-export-method
     server 'bn::|decodeNum| '("num" "base")
     :lisp-name 'decode-num-method
     :return "decodeNumResponse"
     :action "decodeNum"
     )
    (soap-export-method
     server 'bn::|encodeNum| '("bigits" "base")
     :lisp-name 'encode-num-method
     :return "encodeNumResponse"
     :action "encodeNum"
     )
    (setf *bn-server* server)))

(defun calculate-method (&key ((:|opname| op)) ((:|num1| n1)) ((:|num2| n2))) 
  (list "calcResult" (calculate op n1 n2)))

(defun decode-num-method (&key ((:|num| num)) ((:|base| base)))
  (list "decResult" 
	(mapcan #'(lambda (n) (list "item" n)) (decode-num num base))))

(defun encode-num-method (&key ((:|bigits| bigits)) ((:|base| base)))
  (list "encResult" (encode-num bigits base)))





(defun try-server (&key debug (port 1776) (host "localhost"))
  (let ((client (soap-message-client 
		 :url (format nil "http://~A:~A/SOAP" host port)
		 :soap-debug debug
		 :lisp-package :keyword
		 )))
    (values
     (list
      (call-soap-method
       client 'bn::|calculate| "opname" "factorial" "num1" "17" "num2" "0")
      (call-soap-method
       client 'bn::|encodeNum| "bigits" (list "item" 1 "item" 2 "item" 2) "base" 3)
      (call-soap-method
       client 'bn::|decodeNum| "num" "12345" "base" 10))
     client)))

(defun try-calc (op n1 n2 &key debug (port 1776) (host "localhost"))
  (let ((client (soap-message-client 
		 :url (format nil "http://~A:~A/SOAP" host port)
		 :soap-debug debug
		 :lisp-package :keyword
		 )))
    (call-soap-method
     client 'bn::|calculate| "opname" op "num1" n1 "num2" n2)))


(defun bn-wsdl ()
  (encode-wsdl-file 
   "bn.wsdl"
   :servers *bn-server*
   :target "urn:bignumserver"
   :namespaces '(nil
		 :bnm
		 :schema
		 )))

;;; To generate a client from the WSDL file:
;;;
;;; (setq d (decode-wsdl-file "bn.wsdl"))
;;; (make-client-interface d 0 "bn-client.cl")
;;;
;;; (make-server-interface d 0 "bn-server.cl")
