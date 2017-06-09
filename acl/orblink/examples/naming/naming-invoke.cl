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
;; $Id: naming-invoke.cl,v 2.6 2007/04/17 21:54:39 layer Exp $

(in-package :user)

(defpackage :orblink/naming (:use :common-lisp)
	    (:export
	     :*default-naming-port*
	     :*naming-marker*
	     :NamingContext
	     :BindingIterator
	     :start-nameserver
	     :nameservice-ior
	     )
	    )

(defparameter orblink/naming::naming-root *load-pathname*)

(excl:defsystem :naming
    (:default-pathname #.orblink/naming::naming-root)
  (:serial
   "naming-accessors"
   "naming"
   "binding-iterator"
   "naming-utility"
   "naming-administration"
   ))


(defun load-naming ()
  (corba:idl (merge-pathnames "CosNaming.idl" orblink/naming::naming-root))
  (excl:compile-system :naming :recompile t)
  (excl:load-system :naming))












