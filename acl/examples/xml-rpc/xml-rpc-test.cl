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

;; $Id: xml-rpc-test.cl,v 2.4 2007/04/17 21:48:16 layer Exp $


(eval-when (compile load eval)

  (require :xml-rpc "xml-rpc")

  )

(in-package :net.xml-rpc)


;;;
;;; Some test cases for the parsers and decoders

(defun test1 ()
  (decode-xml-parse '(array (data (value (int 1)) (value (int 2)) (value (int 3))))
		    '("array" ("data" (:* :elements :value)))))

(defun test2 ()
  (decode-xml-parse '(array ("   " data "   " 
			     (value "   " (int 1)) 
			     "   "
			     ("   " value (int 2)) 
			     (value (int 3))))
		    '("array" ("data" (:* :elements :value)))
		    nil :ignore-whitespace
		    ))

(defun test3 ()
  (list (encode-xml-rpc-value (encode-universal-time 1 1 1 1 1 1) :date)
	(encode-xml-rpc-value (list 1 1 1 1 1 1) :date)
	(encode-xml-rpc-value (list 1 1 1 1 1 -1) :date)
	(encode-xml-rpc-value 0e1)
	(encode-xml-rpc-value 0d1)
	))

(defun test4 ()
  (let ((in
	 "<value><base64>R0lGODlhFgASAJEAAP/////OnM7O/wAAACH5BAEAAAAALAAAAAAWABIAAAJAhI+py40zDIzujEDBzW0n74AaFGChqZUYylyYq7ILXJJ1BU95l6r23RrRYhyL5jiJAT/Ink8WTPoqHx31im0UAAA7</base64></value>"
	 )
	out)
    (multiple-value-bind (val type)
	(decode-xml-rpc-value (first (parse-xml in :content-only t)))
      (if* (and (stringp val) (eq :base64 type))
	   then
	   (setf out (encode-xml-rpc-value val type))
	   (if (equal in out)
	       :ok
	     (list in out))
	   else
	   (list type val)))))



(defun test5 ()
  (let ((in
	 "<value><string></string></value>"
	 )
	out)
    (multiple-value-bind (val type)
	(decode-xml-rpc-value (first (parse-xml in :content-only t)))
      (if* (and (stringp val) (eq :string type))
	   then
	   (setf out (encode-xml-rpc-value val type))
	   (if (equal in out)
	       :ok
	     (list in out))
	   else
	   (list type val)))))

(defun test6 ()
  (let ((in
	 "<value><base64></base64></value>"
	 )
	out)
    (multiple-value-bind (val type)
	(decode-xml-rpc-value (first (parse-xml in :content-only t)))
      (if* (and (stringp val) (eq :base64 type))
	   then
	   (setf out (encode-xml-rpc-value val type))
	   (if (equal in out)
	       :ok
	     (list in out))
	   else
	   (list type val)))))

(defun test7 ()
  (let ((in "<value></value>"))
    (multiple-value-bind (val type)
	(decode-xml-rpc-value (first (parse-xml in :content-only t)))
      (if* (and (stringp val) (eq :string type))
	   then
	   :ok
	   else
	   (list type val)))))

(defun test8 ()
  (let ((in "<value>foo</value>"))
    (multiple-value-bind (val type)
	(decode-xml-rpc-value (first (parse-xml in :content-only t)))
      (if* (and (stringp val) (eq :string type))
	   then
	   (values :ok val)
	   else
	   (list type val)))))

(defun test9 ()
  (let ((in
	 "<value><string>   </string></value>"
	 )
	out)
    (multiple-value-bind (val type)
	(decode-xml-rpc-value (first (parse-xml in :content-only t)))
      (if* (and (stringp val) (eq :string type))
	   then
	   (setf out (encode-xml-rpc-value val type))
	   (if (equal in out)
	       :ok
	     (list in out))
	   else
	   (list type val)))))

(defun test10 ()
  (unwind-protect
      (let ()
	(trace (do-http-request :break-before t))
	(xml-rpc-call :encoded :url "http://test" 
		      :redirect 17
		      :proxy "http:/proxy")
	)
    (untrace do-http-request)))

(defun test11 ()
  (unwind-protect
      (let ()
	(trace (do-http-request :break-before t))
	(multiple-value-bind (v e)
	    (ignore-errors
	      (xml-rpc-call :encoded :url "http://test" 
			    :content "abc"
			    :proxy "http:/proxy"))
	  (if (search "not allowed in xml-rpc-call"
		      (format nil "~A" e))
	      :ok
	    (values nil v e)))
	)
    (untrace do-http-request)))

