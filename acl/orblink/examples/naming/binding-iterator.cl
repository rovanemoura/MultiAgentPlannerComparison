;; copyright (c) 1997-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2012 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: binding-iterator.cl,v 2.6 2007/04/17 21:54:39 layer Exp $

(in-package :orblink)

(in-package :orblink/naming)
(defclass BindingIterator (CosNaming:BindingIterator-servant)
  ((list :accessor get-list :initarg :list :initform nil)))

(defmethod print-object ((binding-iterator BindingIterator) stream)
  (print-unreadable-object (binding-iterator stream :type t :identity *print-escape*)
    (format stream "~s" (get-list binding-iterator))))

(corba:define-method next_one ((binding-iterator BindingIterator))
  (let ((list (get-list binding-iterator)))
    (if list
	(values
	 T
	 (pop (get-list binding-iterator)))
      nil)))

(corba:define-method next_n ((binding-iterator BindingIterator) how_many)
  (let* ((list (get-list binding-iterator))
	 (number-returned (min how_many (length list))))
    (setf (get-list binding-iterator) (nthcdr number-returned list))
    (and
     list
     (values T
	     (subseq list 0 number-returned)))))

(corba:define-method destroy ((binding-iterator BindingIterator)))

