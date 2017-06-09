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
;; $Id: naming.cl,v 2.6 2007/04/17 21:54:39 layer Exp $

; (in-package :orblink)

(in-package :orblink/naming)

;; names is an alist keyed by NameComponent.

;; Each entry in a NamingContext is cons whose car is the given NameComponent, whose
;; cdr is a cons whose car is the value bound to and whose cdr is one of :nobject or
;; :ncontext

(defclass NamingContext (CosNaming:NamingContext-servant)
  ((entries :accessor get-entries :initform nil :initarg :entries))
  )

;; orblink/naming:BindingIterator implements CosNaming::BindingIterator. It wraps a list of CosNaming::Binding objects


;; Operations on NamingContext
;; bind, rebind, rebind_context, bind_context, resolve, unbind
;; These methods each implement the corresponding operation in
;; the CosNaming::NamingContext interface. They all have the same pattern:
;; They coerce their "name" argument to a list and they use
;; the internal helper function naming-helper with appropriate keyword
;; arguments actually to execute the operation.
;; These operations are all declared :synchronized to avoid the potential for race conditions


(corba:define-method rebind ((namingcontext NamingContext) (name :coerce list) obj)
  (naming-helper namingcontext name
		 :command #'op:rebind
		 :must-exist t
		 :alter-value t
		 :binding-must-be-nobject t
		 :set-to-binding-type :nobject
		 :value obj))

(corba:define-method rebind_context ((namingcontext NamingContext) (name :coerce list) obj)
  (naming-helper namingcontext name
		 :command #'op:rebind_context
		 :must-exist t
		 :alter-value t
		 :binding-must-be-ncontext t
		 :set-to-binding-type :ncontext
		 :value obj))

(corba:define-method bind ((namingcontext NamingContext) (name :coerce list) obj)
  (naming-helper namingcontext name
		 :must-not-exist t
		 :command #'op:bind
		 :set-to-binding-type :nobject
		 :value obj))

(corba:define-method bind_context ((namingcontext NamingContext) (name :coerce list) obj)
  (naming-helper namingcontext name
		 :must-not-exist t
		 :command #'op:bind_context
		 :set-to-binding-type :ncontext
		 :value obj))

(corba:define-method unbind ((namingcontext NamingContext) (name :coerce list))
  (naming-helper namingcontext name
		 :must-exist t
		 :command #'op:unbind
		 :delete t))

(corba:define-method resolve ((namingcontext NamingContext) (name :coerce list))
  (naming-helper namingcontext name
		 :must-exist t
		 :return-value t
		 :command #'op:resolve))

;;;;;;;;;;Miscellaneous methods in the CosNaming::NamingContext interface: new_context, bind_new_context, destroy

;; new_context just creates a new empty naming context
(corba:define-method new_context ((namingcontext NamingContext))
  (make-instance 'namingcontext))

;; This creates a new empty naming context and binds it to name in the current context
(corba:define-method bind_new_context ((namingcontext NamingContext) (name :coerce list))
  (op:bind_context namingcontext name (op:new_context namingcontext)))

;;Destroys a naming context
(corba:define-method destroy ((namingcontext NamingContext))
  (when (get-entries namingcontext)
    (error 'CosNaming:NamingContext/NotEmpty)))

(corba:define-method list ((namingcontext NamingContext) how_many)
  (let*
      ((binding-list (mapcar #'entry-to-binding (get-entries namingcontext)))
       (length (length binding-list))
       (elements-returned (min length how_many))
       (all-elements-returned (= elements-returned length))
       (prefix (subseq binding-list 0 elements-returned))
       (remainder (if all-elements-returned nil
		    (subseq binding-list elements-returned)))
       (binding-iterator
	(and remainder (make-instance 'CosNaming:BindingIterator :list remainder))))
    (values prefix binding-iterator)))



