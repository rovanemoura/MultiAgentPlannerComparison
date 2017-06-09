;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2004-2015 Franz Inc, Oakland, CA
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

;; $Id: dom1-code.cl,v 1.3 2007/04/17 21:47:35 layer Exp $

(in-package :user)

(eval-when (compile load eval)
  (if (and (probe-file "dom1.fasl")
	   (not (member :dom1 *modules* :test #'string-equal)))
      (load "dom1.fasl")
    (require :dom1))
  )

(defpackage :user (:use :net.xml.dom))

;;;
;;; Some programming examples using the DOM Level 1 API
;;;


(defparameter *dom-ex1-data*

  ;; Sample file noDTDXMLfile.xml from domtest-ecmascript-120499.zip 
  ;; at http://www.w3.org/DOM/Test/

"<?xml version='1.0'?>
<staff>
 <employee>
  <employeeId>EMP0001</employeeId>
  <name>Margaret Martin</name>
  <position>Accountant</position>           
  <salary>56,000</salary>
  <gender>Female</gender>
  <address domestic='Yes'>1230 North Ave. Dallas, Texas 98551</address>
 </employee>
 </staff>

"
)

(defun dom-ex1 ()
  
  ;; Parse an XML document and walk the resulting DOM tree

  (let* ((doc (parse-to-dom *dom-ex1-data*))
	 (root (dom-document-element doc))
	)
    (format t "~%The root element is ~A~%" (dom-tag-name root))

    (dolist (c (dom-child-node-list root))
      (format t "~&  Child node: ~S~%" c))

    (let* ((addr (dom-list-elements-by-tag-name root "address"))
	   (attrs (dom-attributes (first addr))))
      (format t "~&  The address element has ~S attributes~%" (dom-length attrs)))
      
    doc))



(defun dom-ex2 (&optional (file t))

  ;; Build a DOM tree from parts and print the result

  (let ((doc (dom-create-document))
	staff emp)
    (dom-append-child doc (setf staff (dom-create-element doc "staff")))
    (dom-append-child staff (setf emp (dom-create-element doc "employee")))

    (flet ((add (parent doc name text &aux child)
		(dom-append-child parent (setf child (dom-create-element doc name)))
		(dom-append-child child (dom-create-text-node doc text))
		child))
      (add emp doc "employeeId" "EMP00002")
      (add emp doc "name" "Alter Ego")
      (add emp doc "position" "Auditor")
      (add emp doc "salary" "65,000")
      (add emp doc "gender" "Male")
      (dom-set-attribute 
       (add emp doc "address" "Port Ludlow, WA")
       "domestic" "Yes")
      )

    (dom-print doc file)

    doc))

