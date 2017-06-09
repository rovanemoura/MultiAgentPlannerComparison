#|
========================
File: frame-manager.lisp
Author: Vladimir Kulyukin
Description: CLOS frame system
Copyright (c) 1994, 1997-98

Acknowledgments: 

- Charles Martin deserves many thanks for explaining 
to me various key ideas behind frames, and how they 
could be implemented in Common Lisp.

- Chris Riesbeck's code from "Inside Case-Based Reasoning"
was a most definite influence, both conceptually and
programmatically.

Comments and bugs to vkulyukin@cs.depaul.edu
========================
|#


(eval-when (load eval compile)
  (unless (find-package :ir)
    (make-package :ir)))

(in-package  :ir)
(use-package :cl-user)

(export '(make-frame-manager frame-and-attribute-to-inherited-value
          define-frame-from-slot-list define-frame-memory get-all-absts 
          abst-and-slots-to-frame frame-to-slots abstp make-slot 
          clear-frame-memory frame same-frame-p part-of-p whole-of-p 
          abstp specp abst-or-whole-of-p spec-or-part-of-p
          ))

(import '(cl-user::deftable cl-user::mappend))

;;; Each frame has slots, i.e., a list of attribute-value pairs,
;;; a list of immediate abstractions, and a list of all abstractions,
;;; and a list of specializations.

(defvar *fm* nil)

(deftable frame-slots)
(deftable frame-absts)
(deftable frame-all-absts)
(deftable frame-specs)

(defclass frame-manager ()
  ((frame-to-slots :initarg :frame-to-slots 
		   :initform *frame-slots-table*
		   :accessor frame-to-slots)
   (frame-to-absts :initarg :frame-to-absts 
		   :initform *frame-absts-table*
		   :accessor frame-to-absts)
   (frame-to-all-absts :initarg :frame-to-all-absts
		       :initform *frame-all-absts-table*
		       :accessor frame-to-all-absts)
   (frame-to-specs :initarg :frame-to-specs
		   :initform *frame-specs-table*
		   :accessor frame-to-specs)))

(defun make-frame-manager () 
  (make-instance 'frame-manager))

(defclass slot ()
  ((attribute :initarg :attribute :accessor attribute)
   (value :initarg :value :initform nil :accessor value)))

(defmethod print-object ((slot slot) stream)
  (with-slots (attribute value) slot
    (print-unreadable-object
      (slot stream :type t :identity t)
      (format stream "~S ~S" attribute value))))

(defun make-slot (attr val)
  (make-instance 'slot :attribute attr :value val))

(defun make-slots (list-of-valued-attributes)
  (loop for (attr val) in list-of-valued-attributes
	collect (make-slot attr val)))

(defun init-frame-manager (&key frames)
  (setf *fm* (make-frame-manager))
  (load frames)
  *fm*)

(defmethod get-slots ((fm frame-manager) (frame t))
  (multiple-value-bind (slots t-or-nil)
    (get-frame-slots frame (frame-to-slots fm))
    (if t-or-nil
	slots
      nil)))

(defmethod set-slots ((fm frame-manager) (frame t) (slots list))
  (setf (get-frame-slots frame (frame-to-slots fm)) slots))

(defmethod add-slots ((fm frame-manager) (frame t) (slots list))
  (setf (get-frame-slots frame (frame-to-slots fm))
	(append slots (get-slots fm frame))))

(defmethod add-slot ((fm frame-manager) (frame t) (attr t) (val t))
  (push (make-slot attr val) (get-slots fm frame)))

(defmethod frame-and-attribute-to-slot ((fm frame-manager) 
					(frame t) 
					(attr t))
  (find attr (get-slots fm frame) :key #'attribute))

(defmethod frame-and-attribute-to-value ((fm frame-manager)
					 (frame t)
					 (attr t))
  (let ((slot (frame-and-attribute-to-slot fm frame attr)))
    (if slot
	(value slot)
      nil)))

(defmethod frame-and-attribute-to-inherited-value ((fm frame-manager)
						   (frame t)
						   (attr t))
  (let ((val (frame-and-attribute-to-value fm frame attr)))
    (if val
	val
      (some #'(lambda (abst)
		(let ((val (frame-and-attribute-to-inherited-value 
			    fm abst attr)))
		  (if val val nil)))
	    (get-absts fm frame)))))

(defmethod set-slot-value ((fm frame-manager) (frame t) 
			   (attr t) (val t))
  (let ((slot (frame-and-attribute-to-slot fm frame attr)))
    (if slot
	(setf (value slot) val)
      (add-slot fm frame attr val))))

(defmethod get-all-absts ((fm frame-manager) (frame t))
  (multiple-value-bind (absts t-or-nil)
    (get-frame-all-absts frame (frame-to-all-absts fm))
    (if t-or-nil absts (list frame))))

(defmethod get-absts ((fm frame-manager) (frame t))
  (multiple-value-bind (absts t-or-nil)
    (get-frame-absts frame (frame-to-absts fm))
    (if t-or-nil absts '())))

(defmethod get-specs ((fm frame-manager) (frame t))
  (multiple-value-bind (specs t-or-nil)
    (get-frame-specs frame (frame-to-specs fm))
    (if t-or-nil specs '())))

(defmethod add-abst ((fm frame-manager) (spec-frame t) (abst-frame t))
  (pushnew abst-frame (get-frame-absts spec-frame (frame-to-absts fm))))

(defmethod add-spec ((fm frame-manager) (abst-frame t) (spec-frame t))
  (pushnew spec-frame (get-frame-specs abst-frame (frame-to-specs fm))))

(defmethod recompute-specs ((fm frame-manager) (frame t))
  (dolist (abst (get-absts fm frame))
    (add-spec fm abst frame)))

(defmethod recompute-absts ((fm frame-manager) (frame t))
  (setf (get-frame-all-absts frame (frame-to-all-absts fm)) 
	(compute-all-absts fm frame))
  (dolist (spec (get-specs fm frame))
    (recompute-absts fm spec)))

(defmethod compute-all-absts ((fm frame-manager) (frame t))
  (labels ((compute-all-absts-local (frame)
	     (let ((absts (get-absts fm frame)))
	       (if (null absts)
		   '()
		 `(,@absts ,@(mappend #'compute-all-absts-local absts))))))
	  `(,frame ,@(remove-duplicates (compute-all-absts-local frame)))))

(defmethod abstp ((fm frame-manager) (abst-frame t) (spec-frame t))
  (if (find abst-frame (get-all-absts fm spec-frame))
      t
    nil))

(defmethod specp ((fm frame-manager) (spec-frame t) (abst-frame t))
  (abstp fm abst-frame spec-frame))

(defmethod part-of-p ((fm frame-manager) (part-frame t) 
		      (whole-frame t))
  (if (member part-frame (compute-all-slots fm whole-frame)
	      :key #'value)
      t
    nil))

(defmethod whole-of-p ((fm frame-manager) (whole-frame t) 
		       (part-frame t))
  (part-of fm whole part))

(defmethod abst-or-whole-of-p ((fm frame-manager) (big-frame t)
			       (small-frame t))
  (or (abstp fm big-frame small-frame)
      (part-of-p fm small-frame big-frame)))

(defmethod spec-or-part-of-p ((fm frame-manager) (small-frame t)
			      (big-frame t))
  (or (specp fm small-frame big-frame)
      (part-of-p fm small-frame big-frame)))

(defmethod frame-symbol-p ((fm frame-manager) (frame t))
  (with-slots (frame-to-slots) fm
    (multiple-value-bind (val bool)
       (get-frame-slots frame frame-to-slots)
       (declare (ignore val))
       bool)))

(defmethod clear-frame-memory ((fm frame-manager))
  (clear-frame-slots (frame-to-slots fm))
  (clear-frame-absts (frame-to-absts fm))
  (clear-frame-all-absts (frame-to-all-absts fm))
  (clear-frame-specs (frame-to-specs fm)))

(defmacro defframe (frame absts av-list)
  `(define-frame-from-attr-val-list ,*fm*
     :frame ',frame
     :absts ',absts
     :attr-val-list ',av-list))

(defmethod define-frame-from-attr-val-list 
  ((fm frame-manager) &key frame (absts nil) (attr-val-list nil))
  (setf (get-frame-absts frame (frame-to-absts fm))
	absts)
  (setf (get-frame-slots frame (frame-to-slots fm))
	(make-slots attr-val-list))
  (recompute-specs fm frame)
  (recompute-absts fm frame)
  frame)

(defmethod define-frame-from-slot-list 
  ((fm frame-manager) &key frame (absts nil) (slots nil))
  (setf (get-frame-absts frame (frame-to-absts fm))
	absts)
  (setf (get-frame-slots frame (frame-to-slots fm))
	slots)
  (recompute-specs fm frame)
  (recompute-absts fm frame)
  frame)

(defmethod compute-all-slots ((fm frame-manager) (frame t))
  (remove-duplicates
   `(,@(loop for abst in (get-absts fm frame)
	     append (compute-all-slots fm abst))
       ,@(get-slots fm frame))
   :key #'attribute))

(defmethod abst-and-slots-to-frame ((fm frame-manager) (abst t) (slots list))
  (if (null slots)
      abst
    (let* ((specs (specialize-frame fm abst slots))
	   (spec  (first specs)))
      (if (and (null (rest specs))
	       (slots-subset-p fm spec slots))
	  spec
	(define-frame-from-slot-list fm 
	  (generate-frame-name spec) specs slots)))))

(defmethod specialize-frame ((fm frame-manager) (abst t) (slots list))
  (let* ((all-specs (loop for spec in (get-specs fm abst)
			  when (slots-abst-p fm spec slots)
			  append (specialize-frame fm spec slots)))
	 (specs (remove-duplicates all-specs)))
    (if (null specs) (list abst) specs)))

(defmethod slots-subset-p ((fm frame-manager) (abst-frame t) (slots list))
  (subsetp slots (compute-all-slots fm abst-frame)
	   :test #'(lambda (slot1 slot2)
		     (and (eql (attribute slot1)
			       (attribute slot2))
			  (eql (value slot1)
			       (value slot2))))))

(defmethod slots-abst-p ((fm frame-manager) (abst-frame t) (slots list))
  (every #'(lambda (slot)
	     (with-slots (attribute value) slot
	       (abstp fm
		      (frame-and-attribute-to-inherited-value
		       fm abst-frame attribute)
		      value)))
	 slots))

(defmethod same-frame-p ((fm frame-manager) (frame1 t) (frame2 t))
  (declare (ignore fm))
  (eq frame1 frame2))

(defun generate-frame-name (frame)
  (gentemp (format nil ":~A-" frame)))

;;; end-of-file

