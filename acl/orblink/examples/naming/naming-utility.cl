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
;; $Id: naming-utility.cl,v 2.6 2007/04/17 21:54:39 layer Exp $

(in-package :orblink)

(in-package :orblink/naming)

(defmethod print-object ((nc NamingContext) stream)
  (print-unreadable-object (nc stream :type t :identity *print-escape*)
    (format stream "~s" (get-entries nc))))


(defun lookup-entry (namingcontext component)
  (unless (and
	   (typep namingcontext 'NamingContext)
	   (typep component 'CosNaming:NameComponent)
	   (stringp (op:id component)))
    (error "Lookup-entry: invalid arguments of: ~s ~s" namingcontext component))
  (assoc (op:id component) (get-entries namingcontext)
	 :test #'equal :key #'op:id))


(defun naming-helper (namingcontext name &rest args &key command (value nil value-supplied) &allow-other-keys)
  (unless
      (and (typep namingcontext 'NamingContext)
	   (typep name 'CosNaming:Name)
	   name
	   )
    (error "naming-helper: internal error on arguments: ~s ~s ~s" namingcontext name args))
  (cond
   ((> (length name) 1)
    (let* ((entry (lookup-entry namingcontext (car name))))
      (when
	  (or (not entry)
	      (not (eq (entry-to-binding-type entry) :ncontext)))
	(error 'CosNaming:NamingContext/NotFound
		:why :missing_node
		:rest_of_name name))
      (let* ((next-naming-context (entry-to-value entry)))
	(unless (typep next-naming-context 'CosNaming:NamingContext)
	  (error (CosNaming:NamingContext/CannotProceed
		  :cxt namingcontext
		  :rest_of_name name)))
	(apply command
	       next-naming-context
	       (cdr name)
	       (unless value-supplied `(,value))))))
   ((= (length name) 1)
    (apply #'act-on-name-component namingcontext (car name) args))))



;;This method accepts a namingcontext and namecomponent and performs certain actions
;; depending on the values of the other keywords. If set-to-binding-type is non-nil then
;; the act-on-name-component is interpreted as being an instruction to
;; set the new value of the designator which is set to value and the new binding type.
;; The must-exist and must-not-exist keywords tell
;; whether the designatee must or must not exist. The binding-must-be-nobject or
;; ncontext.
;; If set-to-binding-type is nil, the function returns the bound value (assuming must-exist is T)
;; and the binding type.

(defmethod act-on-name-component ((namingcontext NamingContext) name-component
				  &key value
				       must-exist
				       must-not-exist
				       binding-must-be-nobject
				       binding-must-be-ncontext
				       delete
				       return-value
				       alter-value
				       set-to-binding-type
				       command
				       )
  (declare (ignore command))
  (let* ((entry (lookup-entry namingcontext name-component)))
    (cond
     ((and entry must-not-exist (error 'CosNaming:NamingContext/AlreadyBound)))
     ((and (not entry) must-exist (error 'CosNaming:NamingContext/NotFound
			       :why :missing_node
			       :rest_of_name (list name-component))))
     ((and (not entry) must-not-exist)
      (push (new-entry name-component value set-to-binding-type)
	    (get-entries namingcontext)))
     ((and entry must-exist
	   binding-must-be-ncontext
	   (not (eq (entry-to-binding-type entry) :ncontext)))
      (error 'CosNaming:NamingContext/NotFound
	     :why :not_context
	     :rest_of_name `(,name-component)))
     ((and entry must-exist
	   binding-must-be-nobject
	   (not (eq (entry-to-binding-type entry) :nobject)))
      (error 'CosNaming:NamingContext/NotFound
	     :why :not_object
	     :rest_of_name `(,name-component)))
     ((and entry must-exist delete)
      (setf (get-entries namingcontext)
	(remove entry (get-entries namingcontext))))
     ((and entry must-exist return-value)
	   (entry-to-value entry))
     ((and entry must-exist alter-value)
      (setf (entry-to-value entry) value
	    (entry-to-binding-type entry) set-to-binding-type))
     (T (error "Naming Context: act-on-name-component: Internal error with arguments: ~s ~s" namingcontext name-component)))))

