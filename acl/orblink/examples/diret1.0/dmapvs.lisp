#|
========================
File: dmapvs.lisp
Author: Vladimir Kulyukin
Description: Direct Memory Access Parsing with Vector Space
Copyright (c) 1998-99 
========================
|#

(eval-when (load eval compile)
  (unless (find-package :ir)
    (make-package :ir)))

(in-package  :ir)
(use-package :cl-user)

(export '(retrieve make-dmapvs clear-dmapvs index-collection make-vs-token-index))
(import '(cl-user::deftable cl-user::to-symbols cl-user::hash-table-to-list))

(defparameter *dmapvs* nil)

(deftable token-to-path)
(deftable stoplist)

(defclass dmapvs (dmaprr)
  ((token-to-path-table :initarg :token-to-path-table
			:initform *token-to-path-table*
			:accessor token-to-path-table)
   (stoplist-table :initarg :stoplist-table
		   :initform *stoplist-table*
		   :accessor stoplist-table)
   (similarity-metric :initarg :similarity-metric
                      :initform :cosine
                      :accessor similarity-metric)))

(defclass vs-token-index (token-index)
  ((norm :initarg :norm :initform 0.0 :accessor norm)))

(defmethod print-object ((self vs-token-index) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~s~%" (target-token self))
    (format stream "~a~%" (if (hash-table-p (token-seq self))
                             (hash-table-to-list (token-seq self))
                             nil))
    (format stream "~f~%" (if (numberp (norm self))
                             (norm self)
                             0.0))))

;;; ========== DMAPVS Administration =============

(defun make-dmapvs (&key (similarity-metric :cosine)) 
   (make-instance 'dmapvs :similarity-metric similarity-metric))

(defun make-vs-token-index (&key target-token token-seq norm)
   (make-instance 'vs-token-index
     :target-token target-token
     :token-seq token-seq
     :norm norm))

(defmethod read-token-to-path ((dvs dmapvs) (token t))
   (get-token-to-path token (token-to-path-table dvs)))

(defmethod write-token-to-path ((dvs dmapvs) (token t) (path string))
   (put-token-to-path token path (token-to-path-table dvs)))

(defmethod read-similarity-metric ((dvs dmapvs))
   (similarity-metric dvs))

(defmethod write-similarity-metric ((dvs dmapvs) (sim-met t))
   (setf (similarity-metric dvs) sim-met))

(defmethod write-similarity-metric :after ((dvs dmapvs) (sim-met t))
   (when (eql sim-met :cosine)
      (set-token-index-norms dvs)))

(defmethod add-stop-token ((dvs dmapvs) (token t))
   (put-stoplist token t (stoplist-table dvs)))

(defmethod stoplist-tokens ((dvs dmapvs) (data list))
   (remove-if #'(lambda (token) (stop-token-p dvs token)) data))

(defmethod remove-stop-token ((dvs dmapvs) (token t))
   (rem-stoplist token (stoplist-table dvs)))

(defmethod stop-token-p ((dvs dmapvs) (token t))
  (multiple-value-bind (val bool)
    (get-stoplist token (stoplist-table dvs))
    (declare (ignore val))
    bool))

(defmethod install-stoplist ((dvs dmapvs) (stoplist-path string))
  (with-open-file (in stoplist-path :direction :input)
    (do ((stop-token-string (read-line in nil :eof) (read-line in nil :eof)))
	((eq stop-token-string :eof))
      (add-stop-token dvs (first (to-symbols stop-token-string :ir))))))

(defmethod read-norm ((tinx vs-token-index))
  (norm tinx))

(defmethod write-norm ((tinx vs-token-index) (n number))
  (setf (norm tinx) n))

(defmethod clear-dmapvs ((dvs dmapvs))
  (clear-token-to-path (token-to-path-table dvs))
  (clear-target-token (target-token-table dvs))
  (clear-token-to-token-index (token-to-token-index-table dvs))
  (clear-token-to-target-token (token-to-target-token-table dvs))
  (clear-stoplist (stoplist-table dvs))
  (setf (num-target-tokens dvs) 0)
  (setf (static-token-indices dvs) nil)
  (clear-token-target-freq (token-target-freq-table dvs)))

;;; ================ Indexing ======================

(defmethod index-collection ((dvs dmapvs) (collection-dir string)
                             (list-of-files list))
   (loop as id = 0 then (1+ id)
         for f in list-of-files
         as fp = (concatenate 'string collection-dir f)
         do (add-token-index dvs id fp)))

(defmethod index-collection :after ((dvs dmapvs) (coldir string)
                                    (list-of-files list))
   (when (eql (similarity-metric dvs) :cosine)
      (set-token-index-norms dvs)))
         
(defmethod add-token-index :before ((dvs dmapvs) (target-token t) 
                                    (token-seq-path string))
   (when (target-token-p dvs target-token)
      (remove-token dvs target-token))
   (add-target-token dvs target-token)
   (write-token-to-path dvs target-token token-seq-path))

(defmethod add-token-index ((dvs dmapvs) (target-token t) 
                            (token-seq-path string))  
   (with-open-file (in token-seq-path :direction :input)
     (let ((tinx (make-vs-token-index :target-token target-token
                   :token-seq (make-hash-table :test #'equal))))
        (loop as line = (read-line in nil :eof)
              until (eql line :eof)
              do (loop for token in (process-tokens dvs line)
                       do (add-token-to-target-token dvs token target-token)
                          (update-token-weight-in-token-index token tinx)))
        (add-static-token-index dvs tinx)
        (loop for token being each hash-key in (token-seq tinx)
              do (add-token-to-token-index dvs token tinx))
        target-token)))

(defmethod add-token-index :after ((dvs dmapvs) (target-token t)
                                   (token-seq-path string))
   (when (eql (read-similarity-metric dvs) :cosine)
      (set-token-index-norms dvs)))

(defmethod add-token-index :before ((dvs dmapvs) (target-token t) 
                                    (vs-tinx vs-token-index))
   (when (target-token-p dvs target-token)
      (remove-token dvs target-token))
   (add-target-token dvs target-token)
   (ensure-symbol-tokens vs-tinx))

(defmethod ensure-symbol-tokens ((tinx vs-token-index))
   (with-accessors ((ts token-seq)) tinx
     (unless (cl-user::every-hash-key #'symbolp ts)
        (let ((new-ts (cl-user::copy-hash-table ts 
                       :test #'eql
                       :key-function #'(lambda (k)
                                         (cond
                                          ((symbolp k) k)
                                          ((stringp k)
                                           (multiple-value-bind (ks pos)
                                               (read-from-string k nil :eos)
                                              (declare (ignore pos))
                                              ks)))))))
           (setf ts new-ts)))))
                                                                           
(defmethod add-token-index ((dvs dmapvs) (target-token t) (vs-tinx vs-token-index))
  (let ((ts (token-seq vs-tinx)))
     (loop for token being each hash-key in ts
           do (add-token-to-target-token dvs token target-token))
     (add-static-token-index dvs vs-tinx)
     (loop for token being each hash-key in ts
           do (add-token-to-token-index dvs token vs-tinx))
     (target-token vs-tinx)))

(defmethod add-token-index :after ((dvs dmapvs) (target-token t) (vs-tinx vs-token-index))
   (when (eql (read-similarity-metric dvs) :cosine)
      (set-token-index-norms dvs)))

(defmethod remove-token :after ((dvs dmapvs) (target-token t))
   (set-token-index-norms dvs))
   
(defmethod set-token-index-norms ((dvs dmapvs))
  (loop for tinx in (static-token-indices dvs)
	  do (setf (norm tinx) (compute-token-index-norm dvs tinx))))

(defmethod compute-token-index-norm ((dvs dmapvs) (tinx vs-token-index))
  (let ((sum 0.0) (num-targets (num-target-tokens dvs)))
    (maphash #'(lambda (token freq)
		 (multiple-value-bind (target-freq bool)
		   (read-token-target-freq dvs token)
		   (assert bool (list bool) "compute-token-index-norm: assert 0")
		   (let ((tw (token-weight dvs token freq target-freq
					   num-targets)))
		     (incf sum (* tw tw)))))
		 (token-seq tinx))
    (sqrt sum)))

(defmethod compute-centroid-token-index ((dvs dmapvs))
  (let ((ctinx (make-vs-token-index :target-token :centroid
				    :token-seq (make-hash-table :test #'equal))))
    (map-token-target-freq #'(lambda (token freq)
                               (declare (ignore freq))
                               (let ((aw (average-token-weight dvs token)))
                                  (when (> aw 0.0)
                                     (write-token-weight-in-token-index token ctinx aw)))))
     ctinx))

(defmethod process-tokens ((dvs dmapvs) (tokens string)
                           &optional (package :ir))
  (stoplist-tokens dvs (to-symbols tokens package)))

(defmethod fetch-data-item ((dvs dmapvs) (id string))
  (multiple-value-bind (id pos)
    (read-from-string id)
    (declare (ignore pos))
    (multiple-value-bind (path bool)
      (read-token-to-path dvs id)
      (assert bool (list bool) "fetch-data-item: assert 0")
      (with-open-file (in path :direction :input)
        (loop with text = "" 
              with nls  = (string #\newline)
              as line   = (read-line in nil :eof)
              until (eq line :eof)
              do (if (eq (length line) 0)
                    (setf text (concatenate 'string text nls))
                    (setf text (concatenate 'string text line nls)))
              finally (return text))))))

(defmethod describe-data-item ((dvs dmapvs) (id string) &optional (num-lines 1))
  (multiple-value-bind (id pos)
    (read-from-string id)
    (declare (ignore pos))
    (multiple-value-bind (path bool)
      (read-token-to-path dvs id)
      (assert bool (list bool) "describe-data-item: assert 0")
      (with-open-file (in path :direction :input)
        (loop with text = ""
              with nls  = (string #\newline)
              with nlines = num-lines
              as line = (read-line in nil :eof)
              until (or (zerop nlines) (eql line :eof))
              do (unless (eq (length line) 0)
		       (setf text (concatenate 'string text line nls))
                   (decf nlines))
              finally (return text))))))

(defmethod update-token-weight-in-token-index ((token t) (tinx token-index))
  (with-accessors ((ts token-seq)) tinx
    (multiple-value-bind (val bool)
      (gethash token ts)
      (if bool
	  (incf (gethash token ts) 1.0)
	(setf (gethash token ts) 1.0)))))

(defmethod read-token-weight-in-token-index ((token t) (tinx token-index))
  (multiple-value-bind (w bool)
    (gethash token (token-seq tinx))
    (if bool w 0.0)))

(defmethod write-token-weight-in-token-index ((token t) (tinx token-index) (tw number))
  (with-accessors ((ts token-seq)) tinx
    (setf (gethash token ts) tw)))

(defmethod add-token-to-token-index ((dvs dmapvs) (token t) (val token-index))
  (with-accessors ((table token-to-token-index-table)) dvs
    (push val (get-token-to-token-index token table))))

(defmethod retrieve ((dvs dmapvs) (query list))
  (extract-tokens dvs query)
  (extracted-tokens-to-token-indices dvs #'eq)
  (extracted-token-indices-to-ranked-retrievals dvs)
  (describe-retrievals dvs) 
  (setf (retrievals dvs)
	(sort (retrievals dvs) #'> :key #'score))
  (retrievals dvs))

(defmethod extract-tokens ((dvs dmapvs) (data list))
  (setf (extracted-tokens dvs) (stoplist-tokens dvs data)))

(defmethod extracted-token-indices-to-ranked-retrievals :before ((dvs dmapvs))
  (setf (extracted-tokens dvs)
        (extracted-tokens-to-vs-token-index dvs (extracted-tokens dvs))))

(defmethod extracted-token-indices-to-ranked-retrievals ((dvs dmapvs))
   (with-accessors ((ets extracted-tokens)
                    (etis extracted-token-indices)
                    (rets retrievals)
                    (thresh sim-score-threshold)) dvs
     (loop for tinx in etis
           as  sim-score = (score-token-index dvs tinx)
           unless (or (find (target-token tinx) rets :test #'token-equal-p)
                      (< sim-score thresh))
           do (push (make-retrieval 
                      :target-token (target-token tinx)
                      :score sim-score
                      :token-seq (token-seq tinx))
                     rets))))

(defmethod extracted-tokens-to-vs-token-index ((dvs dmapvs) (data list))
  (let ((tinx (make-vs-token-index :target-token (gensym "data")
				   :token-seq (make-hash-table :test #'eql))))
    (loop for token in data
	    do (update-token-weight-in-token-index token tinx)
	    finally (progn
                    (remove-zero-weight-tokens dvs tinx)
                    (when (eql (similarity-metric dvs) :cosine)
                       (setf (norm tinx) 
                             (compute-token-index-norm dvs tinx)))
                    (return tinx)))))

(defmethod remove-zero-weight-tokens ((dvs dmapvs) (tinx vs-token-index))
  (let ((num-targets (num-target-tokens dvs))
	(tseq (token-seq tinx))
	(zw-tokens '()))
    (maphash #'(lambda (token tw)
		 (multiple-value-bind (target-freq bool)
		   (read-token-target-freq dvs token)
		   (unless (and bool 
				(not (zerop target-freq))
				(not (zerop (token-weight dvs token tw 
							  target-freq num-targets))))
		     (push token zw-tokens))))
	     tseq)
    (dolist (zwt zw-tokens) (remhash zwt tseq))))

(defmethod describe-retrievals ((dvs dmapvs))
  (retrievals-to-paths dvs))

(defmethod retrievals-to-paths ((dvs dmapvs))
  (dolist (r (retrievals dvs))
    (setf (target-description r)
	  (read-token-to-path dvs (target-token r)))))

(defmethod score-token-index ((dvs dmapvs) (tinx token-index))
  (ecase (similarity-metric dvs)
    (:dot-product (dot-product dvs tinx))
    (:cosine      (cosine dvs tinx))))

(defmethod dot-product ((dvs dmapvs) (tinx token-index))
  (let ((input-tokens  (token-seq (extracted-tokens dvs)))
	(static-tokens (token-seq tinx))
	(num-targets   (num-target-tokens dvs))
	(score         0.0))
    (maphash #'(lambda (input-token input-freq)
		 (multiple-value-bind (static-freq bool)
		   (gethash input-token static-tokens)
		   (unless (null bool)
		     (multiple-value-bind (target-freq bool)
		       (read-token-target-freq dvs input-token)
		       (assert bool (list bool) "dot-product: assert 0")
		       (incf score 
			     (* (token-weight dvs input-token input-freq  
					      target-freq num-targets)
				(token-weight dvs input-token static-freq 
					      target-freq num-targets)))))))
	     input-tokens)
    score))

(defmethod cosine ((dvs dmapvs) (tinx token-index))
  (let ((input-tokens  (token-seq (extracted-tokens dvs)))
        (static-tokens (token-seq tinx))
        (num-targets (num-target-tokens dvs))
        (dot-product 0.0))
    (maphash 
      #'(lambda (input-token input-freq)
          (multiple-value-bind (static-freq bool)
              (gethash input-token static-tokens)
             (unless (null bool)
                (multiple-value-bind (target-freq bool)
                    (read-token-target-freq dvs input-token)
                   (assert bool (list bool) "cosine: assert 0")
                   (incf dot-product
                     (* (token-weight dvs input-token input-freq  
                          target-freq num-targets)
                        (token-weight dvs input-token static-freq 
                          target-freq num-targets)))))))
      input-tokens)
    (/ dot-product (* (norm (extracted-tokens dvs))
                      (norm tinx)))))

(defmethod token-weight ((dvs dmapvs) (token t) (token-freq number)
			 (token-target-freq number) (num-targets number))
  (declare (ignore token))
  (* token-freq (- (log (probability token-target-freq num-targets) 2))))

(defmethod average-token-weight ((dvs dmapvs) (token t))
  (loop with num-targets = (num-target-tokens dvs)
	for tinx in (static-token-indices dvs)
	sum (token-weight dvs token 
			  (read-token-weight-in-token-index token tinx)
			  (read-token-target-freq dvs token)
			  num-targets) 
	into total-token-weight
	finally (return (/ total-token-weight num-targets))))

;;; ===== debugging tools ==============

(defmethod token-list ((dvs dmapvs))
  (let ((token-list '()))
    (map-token-to-token-index #'(lambda (token token-index)
				  (declare (ignore token-index))
				  (pushnew token (rest token-list) :test #'eql))
			      (token-to-token-index-table dvs))
    token-list))

(eval-when (compile eval load)
    (pushnew :dmapvs *features*)
    )

;;; end-of-file
