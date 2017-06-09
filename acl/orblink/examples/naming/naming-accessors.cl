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
;; $Id: naming-accessors.cl,v 2.6 2007/04/17 21:54:39 layer Exp $

(in-package :orblink)

(in-package :orblink/naming)

(defmacro entry-to-value (entry)
  `(cadr ,entry))

(defmacro entry-to-binding-type (entry)
  `(cddr ,entry))

(defun entry-to-binding (entry)
  (CosNaming:Binding :binding_name (list (entry-to-name-component entry))
		     :binding_type (entry-to-binding-type entry)))

(defmacro entry-to-name-component (entry)
  `(car ,entry))

(defun new-entry (component value binding-type)
  (unless
      (and
       (typep component 'CosNaming:NameComponent)
       (typep binding-type 'CosNaming:BindingType))
    (error "new-entry: Invalid arguments of: ~s ~s ~s" component value binding-type))
  `(,component . (,value . ,binding-type)))
