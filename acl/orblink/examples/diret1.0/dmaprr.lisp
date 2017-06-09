#|
========================

File: dmaprr.lisp
Author: Vladimir Kulyukin
Description: Direct Memory Access Parsing with Ranked Retrieval

Copyright (c) 1994-95, 1998-99 

Comments and bugs to vkulyukin@cs.depaul.edu
========================
|#

(eval-when (load eval compile)
  (unless (find-package :ir)
    (make-package :ir)))

(in-package  :ir)
(use-package :cl-user)

(export '(retrieve make-dmaprr clear-dmaprr ))
(import '(cl-user::deftable cl-user::to-symbols))

(deftable target-token)
(deftable token-to-token-index)
(deftable token-to-target-token)
(deftable token-target-freq)
(deftable sim-score-weight)

(defvar *dmaprr* nil)

(defclass token-index ()
  ((target-token :initarg :target-token :initform nil :accessor target-token)
   (token-seq :initarg :token-seq :initform nil :accessor token-seq)))

(defun make-token-index (&key target-token token-seq)
  (make-instance 'token-index
		 :target-token target-token
		 :token-seq token-seq))

(defmethod print-object ((self token-index) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~s" (target-token self))
    (format stream "~s" (token-seq self))))

(defclass dmaprr (dmap)
  ((retrievals :initarg :retrievals :initform nil :accessor retrievals)
   (extracted-tokens :initarg :extracted-tokens :initform nil
                     :accessor extracted-tokens)
   (extracted-token-indices :initarg :extracted-token-indices 
                            :initform nil
			          :accessor extracted-token-indices)
   (static-token-indices :initarg :static-token-indices :initform nil
			       :accessor static-token-indices)
   (target-token-table :initarg :target-token-table 
		           :initform *target-token-table*
		           :accessor target-token-table)
   (token-to-token-index-table :initarg :token-to-token-index-table 
			             :initform *token-to-token-index-table*
			             :accessor token-to-token-index-table)
   (token-to-target-token-table :initarg :token-to-target-token-table
				        :initform *token-to-target-token-table*
				        :accessor token-to-target-token-table)
   (token-target-freq-table :initarg :token-target-freq-table
			          :initform *token-target-freq-table*
			          :accessor token-target-freq-table)
   (sim-score-weight-table :initarg :sim-score-weight-table
			         :initform *sim-score-weight-table*
			         :accessor sim-score-weight-table)
   (sim-score-total-weight :initarg :sim-score-total-weight
			         :initform 0.0
			         :accessor sim-score-total-weight)
   (sim-score-threshold :initarg :sim-score-threshold
                        :initform 0.0
                        :accessor sim-score-threshold)
   (num-target-tokens :initarg :num-target-tokens
		          :initform 0
		          :accessor num-target-tokens)))

;;; ================ DMAPRR Administration ======================

(defmethod read-target-token ((drr dmaprr) (target-token t))
  (with-accessors ((table target-token-table)) drr
    (get-target-token target-token table)))

(defmethod write-target-token ((drr dmaprr) (target-token t) (val t))
  (with-accessors ((table target-token-table)) drr
    (put-target-token target-token val table)))

(defmethod remove-target-token ((drr dmaprr) (tt t))
   (let ((tinx (read-static-token-index drr tt)))
      (assert (not (null tinx)) (list tinx) "remove-target-token: assert 0")
      (loop with ts = (token-seq tinx)
            for token being each hash-key of ts
            do (remove-token-to-token-index drr token tinx)
               (remove-token-to-target-token drr token tt)
               (decf-token-target-freq drr token))
      (rem-target-token tt (target-token-table drr))
      (remove-static-token-index drr tt)
      (decf (num-target-tokens drr))))

(defmethod target-token-p ((drr dmaprr) (tt t))
   (multiple-value-bind (x bool)
       (read-target-token drr tt)
      (declare (ignore x))
      bool))

(defmethod add-target-token ((drr dmaprr) (target-token t))
  (with-accessors ((table target-token-table)) drr
    (multiple-value-bind (val bool)
      (get-target-token target-token table)
      (unless bool
	(write-target-token drr target-token t)
	(incf (num-target-tokens drr))))))

(defmethod read-token-to-token-index ((drr dmaprr) (token t))
  (with-accessors ((table token-to-token-index-table)) drr
    (get-token-to-token-index token table)))

(defmethod add-token-to-token-index ((drr dmaprr) (token t) (val token-index))
   (with-accessors ((table token-to-token-index-table)) drr
     (pushnew val (get-token-to-token-index token table)
                  :test #'token-index-equal-p)))

(defmethod remove-token-to-token-index ((drr dmaprr) (token t) (tx token-index))
   (with-accessors ((table token-to-token-index-table)) drr
     (multiple-value-bind (tinxs bool)
         (read-token-to-token-index drr token)
        (declare (ignore bool))
        (setf tinxs (remove tx tinxs :test #'eq))
        (if (null tinxs)
           (rem-token-to-token-index token table)
           (put-token-to-token-index token tinxs table)))))
              
(defmethod read-token-to-target-token ((drr dmaprr) (token t))
   (with-accessors ((table token-to-target-token-table)) drr
     (get-token-to-target-token token table)))

(defmethod add-token-to-target-token ((drr dmaprr) (token t) (target-token t))
  (with-accessors ((table token-to-target-token-table)) drr
    (unless (find target-token (get-token-to-target-token token table)
		                   :test #'token-equal-p)
      (push target-token (get-token-to-target-token token table))
      (incf-token-target-freq drr token))))

(defmethod add-token-to-target-token ((drr dmaprr) (token string) (target-token t))
   (multiple-value-bind (tok pos)
       (read-from-string token pos)
      (assert (> pos 0))
      (add-token-to-target-token drr tok target-token)))

(defmethod remove-token-to-target-token ((drr dmaprr) (token t) (target-token t))
   (with-accessors ((table token-to-target-token-table)) drr
     (setf (get-token-to-target-token token table)
           (remove target-token (get-token-to-target-token token table)
             :test #'token-equal-p))
     (multiple-value-bind (targets bool)
         (read-token-to-target-token drr token)
        (declare (ignore bool))
        (when (null targets)
           (rem-token-to-target-token token table)))))
   
(defmethod read-token-target-freq ((drr dmaprr) (token t))
  (with-accessors ((table token-target-freq-table)) drr
    (get-token-target-freq token table)))

(defmethod write-token-target-freq ((drr dmaprr) (token t) (freq fixnum))
  (with-accessors ((table token-target-freq-table)) drr
    (put-token-target-freq token freq table)))

(defmethod remove-token-target-freq ((drr dmaprr) (token t))
   (rem-token-target-freq token (token-target-freq-table drr)))

(defmethod incf-token-target-freq ((drr dmaprr) (token t))
  (multiple-value-bind (freq bool)
    (read-token-target-freq drr token)
    (if bool 
	(write-token-target-freq drr token (1+ freq))
      (write-token-target-freq drr token 1))))

(defmethod decf-token-target-freq ((drr dmaprr) (token t))
   (multiple-value-bind (freq bool)
       (read-token-target-freq drr token)
      (assert (and bool (> freq 0)))
      (if (zerop (1- freq))
         (remove-token-target-freq drr token)
         (write-token-target-freq drr token (1- freq)))))

(defmethod read-sim-score-weight ((drr dmaprr) (score t))
  (with-accessors ((table sim-score-weight-table)) drr
    (get-sim-score-weight score table)))

(defmethod write-sim-score-weight ((drr dmaprr) (score t) (w single-float))
  (with-accessors ((table sim-score-weight-table)) drr
    (put-sim-score-weight score w table)))

(defmethod write-sim-score-weight :after ((drr dmaprr) (score t) (w single-float))
  (recompute-sim-score-total-weight drr))

(defmethod compute-sim-score-total-weight ((drr dmaprr))
  (with-accessors ((table sim-score-weight-table)) drr
    (let ((total-weight 0.0))
      (map-sim-score-weight #'(lambda (name w)
				(declare (ignore name))
				(incf total-weight w))
			    table)
      total-weight)))

(defmethod read-sim-score-threshold ((drr dmaprr))
   (sim-score-threshold drr))

(defmethod write-sim-score-threshold ((drr dmaprr) (thresh number))
   (setf (sim-score-threshold drr) thresh))

;;; ================ Indexing ====================

(defmacro defdrrtokseq (target &rest tokens)
  `(with-slots (frame-manager) ,*dmaprr*
     (unless (frame-symbol-p frame-manager ',target)
       (define-frame-from-attr-val-list frame-manager 
	 :frame ',target :absts '(:m-root)))
     (add-token-index ,*dmaprr* ',target ',tokens)))

(defmethod add-token-index :before ((drr dmaprr) (target-token t) (toksen-seq list))
  (add-target-token drr target-token))

(defmethod add-token-index ((drr dmaprr) (target-token t) (toksen-seq list))
  (let ((tinx (make-token-index :target-token target-token
				        :token-seq token-seq)))
    (loop for token in token-seq
	    do (add-token-to-target-token drr token target-token)
             (add-token-to-token-index drr token tinx))
     (add-static-token-index drr tinx)
     target-token))

(defmethod add-static-token-index ((drr dmaprr) (tinx token-index))
   (pushnew tinx (static-token-indices drr) :test #'token-index-equal-p))

(defmethod read-static-token-index ((drr dmaprr) (target-token t))
   (find target-token (static-token-indices drr) :test #'token-equal-p
     :key #'target-token))

(defmethod remove-static-token-index ((drr dmaprr) (target-token t))
   (setf (static-token-indices drr)
         (remove target-token (static-token-indices drr)
           :key #'target-token)))

(defmethod remove-token ((drr dmaprr) (token t))
   (if (target-token-p drr token)
      (remove-target-token drr token)
      (progn
       (rem-token-to-token-index token (token-to-token-index-table drr))
       (rem-token-to-target-token token (token-to-target-token-table drr))
       (rem-token-target-freq token (token-target-freq-table drr)))))
   
;;; ============= Retrieval =============

(defclass retrieval ()
  ((score :initarg :score :initform 0.0 :accessor score)
   (target-token :initarg :target-token :initform nil
		     :accessor target-token)
   (token-seq :initarg :token-seq :initform nil
	        :accessor token-seq)
   (target-description :initarg :target-description :initform nil
		           :accessor target-description)))

(defun make-retrieval (&key score target-token token-seq)
  (make-instance 'retrieval
    :score        score
    :target-token target-token
    :token-seq    token-seq))

(defmethod print-object ((self retrieval) stream)
  (with-slots (score target-token token-seq) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~4,2F ~S ~S" score target-token token-seq))))

(defmethod score ((retrieval null)) 
  (declare (ignore retrieval))
  0)

(defmethod target-token ((retrieval null))
  (declare (ignore retrieval))
  nil)

(defmethod target-concept ((retrieval null))
  (declare (ignore retrieval))
  nil)

(defmethod retrieve :before ((drr dmaprr) (data list))
  (setf (retrievals drr) nil))

(defmethod retrieve ((drr dmaprr) (data list))
  (extract-tokens drr data)
  (extracted-tokens-to-token-indices  drr)
  (extracted-token-indices-to-ranked-retrievals drr)
  (retrievals drr))

(defmethod retrieve ((drr dmaprr) (data string))
  (retrieve drr (to-symbols data :ir)))

(defmethod retrieve :after ((drr dmaprr) (data list))
  (setf (extracted-tokens drr) nil)
  (setf (extracted-token-indices drr) nil))

;;; ============= Token Extraction =============

(defmethod extract-tokens :before ((drr dmaprr) (data list))
  (clear-expectations drr :dynamic))
  
(defmethod extract-tokens ((drr dmaprr) (data list))
  (process-tokens drr data))

(defmethod extracted-tokens-to-token-indices ((drr dmaprr) 
                                              &optional (tinx-equal-test #'token-index-equal-p))
   (with-accessors ((extoks extracted-tokens)) drr
     (loop with result-tokinx = '()
           for extok in extoks
           do (multiple-value-bind (tokinx bool)
                  (read-token-to-token-index drr extok)
                 (when bool
                    (loop for tinx in tokinx
                          do (pushnew tinx result-tokinx :test tinx-equal-test))))
           finally (setf (extracted-token-indices drr) result-tokinx))))

(defmethod extracted-token-indices-to-ranked-retrievals ((drr dmaprr))
  (with-accessors ((ets    extracted-tokens)
                   (etis   extracted-token-indices)
                   (rets   retrievals)
                   (thresh sim-score-threshold)) drr
    (loop for tinx in etis
	    as  sim-score = (score-token-index drr tinx)
	    unless (or (find (target-token tinx) rets :test #'token-equal-p)
		         (< sim-score thresh))
	    do (push (make-retrieval :target-token (target-token tinx)
				           :token-seq (token-seq tinx)
				           :score sim-score)
                   rets))))
          
(defmethod token-equal-p ((x t) (y t))
  (eql x y))

(defmethod token-index-equal-p ((x token-index) (y token-index))
  (and (token-equal-p (target-token x) (target-token y))
       (equal (token-seq x) (token-seq y))))

;;; =============== Similarity Scoring ===================

(defmethod score-token-index ((drr dmaprr) (tokinx token-index))
  (with-accessors ((table sim-score-weight-table)) drr
    (let ((score 0.0))
      (map-sim-score-weight
       #'(lambda (sim-score weight)
	   (unless (zerop weight)
	     (incf score
	       (apply-sim-score drr
				(symbol-function sim-score)
				weight
				tokinx))))
       table)
      score)))

(defmethod apply-sim-score ((drr dmaprr) (sim-score function) 
			    (w float) (x token-index)) 
  (/ (* w (funcall sim-score drr x))
     (sim-score-total-weight drr)))

(defmethod expected-tokens-found-score ((drr dmaprr) (tinx token-index))
  (with-accessors ((fm frame-manager)
		   (extoks extracted-tokens)) drr
    (with-accessors ((ts token-seq)) tinx
      (let ((expected-tokens-found
	     (intersection ts extoks
			   :test #'(lambda (x y)
				     (abst-or-whole-of-p
				      fm x y)))))
	(/ (iv-sum drr expected-tokens-found) (iv-sum drr ts))))))

(defmethod unexpected-extracted-tokens-score ((drr dmaprr) (tinx token-index))
  (with-accessors ((fm frame-manager) 
		   (extoks extracted-tokens)) drr
    (with-accessors ((ts token-seq)) tinx
      (let ((unexpected-extracted-tokens
	     (set-difference extoks ts
			     :test #'(lambda (x y)
				       (spec-or-part-of-p 
					fm x y)))))
	(- 1.0 (/ (iv-sum drr unexpected-extracted-tokens) 
		  (iv-sum drr extoks)))))))

(defmethod missed-expected-tokens-score ((drr dmaprr) (tinx token-index))
  (with-accessors ((fm frame-manager) (extoks extracted-tokens)) drr
    (with-accessors ((ts token-seq)) tinx
      (let ((missed-expected-tokens 
             (set-difference ts extracted-tokens
               :test #'(lambda (x y)
                         (abst-or-whole-of-p
                          fm x y)))))
         (- 1.0 (/ (iv-sum drr missed-expected-tokens)
                   (iv-sum drr ts)))))))

(defmethod iv-sum ((drr dmaprr) (token-seq list))
  (loop for tok in token-seq
	  sum (iv drr tok)))

(defmethod iv ((drr dmaprr) (tok t))
  (multiple-value-bind (freq bool)
    (read-token-target-freq drr tok)
    (if bool
	(information-value 
	 (probability freq (num-target-tokens drr)))
      0.0)))

(defun information-value (prob)
  (- (log prob 2)))

(defun probability (freq total-freq)
  (/ (coerce freq 'float)
     (coerce total-freq 'float)))

;;; ============ Startups and Cleanups ==================

;;; there should be the vstokseqs argument
(defun make-dmaprr (&key frames tokseqs)
  (declare (special *dmaprr*))
  (let ((dmap (init-dmap :frames frames :tokseqs tokseqs)))
    (with-slots (frame-manager) dmap
      (setf *dmaprr* (make-instance 'dmaprr :frame-manager frame-manager))
      (add-extracted-tokens-callbacks *dmaprr*)
      (assign-sim-score-weights *dmaprr*)
      *dmaprr*)))

(defmethod add-extracted-tokens-callbacks ((drr dmaprr) &optional (stream t))
  (add-callback drr 
		#'(lambda (token)
		    (format stream "extracting ~s~%" token)
		    (pushnew token (extracted-tokens drr)
			     :test #'token-equal-p))))

(defmethod clear-dmaprr ((drr dmaprr))
  (clear-dmap drr)
  (clear-target-token (target-token-table drr))
  (clear-token-to-token-index (token-to-token-index-table drr))
  (clear-token-to-target-token (token-to-target-token-table drr))
  (clear-token-target-freq (token-target-freq-table drr))
  (clear-sim-score-weights drr))

(defmethod assign-sim-score-weights ((drr dmaprr) 
				     &key (etfs 2.0) (uets 0.0) (mets 0.0))
  (clear-sim-score-weights drr)
  (write-sim-score-weight drr 'expected-tokens-found-score etfs)
  (write-sim-score-weight drr 'unexpected-extracted-tokens-score uets)
  (write-sim-score-weight drr 'missed-expected-tokens-score mets))

(defmethod clear-sim-score-weights ((drr dmaprr))
  (with-accessors ((table sim-score-weight-table)) drr
    (setf (sim-score-total-weight drr) 0.0)
    (clear-sim-score-weight table)))

;;; end-of-file






