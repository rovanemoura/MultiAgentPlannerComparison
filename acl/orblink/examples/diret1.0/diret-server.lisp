#|
==============================
File: diret-server.lisp
Author: Vladimir Kulyukin
Description: CORBA-based information server
Copyright (c) 1999

Comments and bugs to vkulyukin@cs.depaul.edu
==============================
|#

(in-package :cl-user)

(eval-when (compile load eval)

   (unless (find :diret-params *features*)
     (load "diret"))

   (unless (find :dmapvs *features*)
     (load "dmapvs-loader"))

   )

(require :orblink)

(eval-when (compile eval load)

  (unless (find-package :DIRET)
     (corba:idl "diret.idl"))
  
  (pushnew :diret-server *features*)

  )

(export '(*DIRET-SERVER* MAKE-DIRET-SERVER START-SERVER
           RESPOND-TO-RETRIEVE RESPOND-TO-COMPUTECENTROID
	   RESPOND-TO-CLEAR-DMAPVS RESPOND-TO-FETCHDATAITEM 
	   RESPOND-TO-DESCRIBEDATAITEM RESPOND-TO-DESCRIBEDATAITEMFULLY
	   RESPOND-TO-DESCRIBESELF RESPOND-TO-WRITE-SIMILARITY-METRIC))

(defparameter *diret-server*     nil)
(defparameter *self-description* nil)

(defclass diret-indexing-server  (DIRET:Indexing-Servant)  ())
(defclass diret-retrieval-server (DIRET:Retrieval-Servant) ())
(defclass diret-server ()
  ((indexer   :initarg :indexer   :initform nil :accessor indexer)
   (retriever :initarg :retriever :initform nil :accessor retriever)))
   
(defun make-diret-server (&key indexer retriever)
  (make-instance 'diret-server 
    :indexer   indexer
    :retriever retriever))

(corba:define-method computeCentroid ((self diret-indexing-server))
  (multiple-value-bind (ctinx condition)
    (ignore-errors (ir::compute-centroid-token-index ir::*dmapvs*))
    (loop with wtoks = (make-array '(0) :adjustable t 
				   :element-type 'diret:wtok
				   :fill-pointer 0)
	  initially (when condition (return wtoks))
	  for (token weight) in (hash-table-to-list (ir::token-seq ctinx))
	  do (vector-push-extend (make-instance 'diret:wtok
						:weight weight
						:token (write-to-string token))
				 wtoks)
	  finally (return wtoks))))

(corba:define-method retrieve ((self diret-retrieval-server) query)
  (labels ((retrieval-to-wdoc (r)
	     (make-instance 'diret:wdoc
			    :simco (ir::score r)
			    :id (write-to-string (ir::target-token r))
			    :descrip (ir::target-description r))))
    (multiple-value-bind (rets condition)
      (ignore-errors (ir::retrieve ir::*dmapvs* query))
      (declare (ignore condition))
      (make-array `(,(length rets)) 
		  :element-type 'diret:wdoc
		  :initial-contents (mapcar #'retrieval-to-wdoc rets)))))

(corba:define-method describeDataItem ((self diret-retrieval-server) id)
  (multiple-value-bind (descrip condition)
    (ignore-errors
     (ir::describe-data-item ir::*dmapvs* id 1))
    (if condition "" descrip)))

(corba:define-method describeDataItemFully ((self diret-retrieval-server) id)
  (multiple-value-bind (descrip condition)
    (ignore-errors 
     (ir::describe-data-item ir::*dmapvs* id 4))
    (if condition "" descrip)))

(corba:define-method fetchDataItem ((self diret-retrieval-server) id)
  (ir::fetch-data-item ir::*dmapvs* id))

(corba:define-method describeSelf ((self diret-retrieval-server))
  *self-description*)

(defmethod install-stoplist ((dirs diret-server) (path string))
  (ir::install-stoplist ir::*dmapvs* path))

(defmethod index-collection ((dirs diret-server) (path string) (files list))
  (ir:index-collection ir::*dmapvs* path files))

(defmethod write-indexer-ior-to-file ((dirs diret-server) (path string))
  (orblink:write-ior-to-file (indexer dirs) path))

(defmethod write-retriever-ior-to-file ((dirs diret-server) (path string))
  (orblink:write-ior-to-file (retriever dirs) path))

;;; respond-to- is a prefix used to sugarcode the op: methods.
;;; respond-to- methods are intendended for the dir-server testers
;;; and GUI developers. All GUI developers need to know is the 
;;; input-output behavior of the dir-server. They do not need to
;;; know that the dir-server consists of two components: indexer
;;; and retriever. Nor do they need to know which component is
;;; used to support which operation. They same is true for any
;;; developer who wants to interface an application to a dir-server.

(defmethod respond-to-retrieve ((dirs diret-server) (query string))
  (op:retrieve (retriever dirs) query))

(defmethod respond-to-computeCentroid ((dirs diret-server))
  (op:computeCentroid (indexer dirs)))

(defmethod respond-to-clear-dmapvs ((dirs diret-server))
  (ir::clear-dmapvs ir::*dmapvs*))

(defmethod respond-to-fetchDataItem ((dirs diret-server) (id string))
  (op:fetchDataItem (retriever dirs) id))

(defmethod respond-to-describeDataItem ((dirs diret-server) (id string))
   (op:describeDataItem (retriever dirs) id))

(defmethod respond-to-describeDataItemFully ((dirs diret-server) (id string))
   (op:describeDataItemFully (retriever dirs) id))

(defmethod respond-to-describeSelf ((dirs diret-server))
   (op:describeSelf (retriever dirs)))

(defmethod respond-to-write-similarity-metric ((dirs diret-server) (sm t))
   (assert (or (eql sm :dot-product) (eql sm :cosine)))
   (ir::write-similarity-metric ir::*dmapvs* sm))

(defparameter *sim-thresh* 0.1)

(defun start-server (source)
  (unless (null ir::*dmapvs*) (ir::clear-dmapvs ir::*dmapvs*))
  (setf ir::*dmapvs* (ir:make-dmapvs))
  (ir::write-sim-score-threshold ir::*dmapvs* *sim-thresh*)
  (let* ((indexer   (make-instance 'diret-indexing-server))
	 (retriever (make-instance 'diret-retrieval-server))
	 (server    (make-diret-server :indexer indexer :retriever retriever)))
    (install-stoplist server *stoplist-path*)
    (ecase source
	   (:stocks 
	    (index-collection server *coldir* *stock-funds*)
	    (write-indexer-ior-to-file server *stocks-dir-inx*)
	    (write-retriever-ior-to-file server *stocks-dir-ret*)
	    (setf *self-description* "Collection of documents on stock mutual funds."))
	   (:bonds 
	    (index-collection server *coldir* *bond-funds*)
	    (write-indexer-ior-to-file server *bonds-dir-inx*)
	    (write-retriever-ior-to-file server *bonds-dir-ret*)
	    (setf *self-description* "Collection of documents on bond mutual funds."))
	   (:cash 
	    (index-collection server *coldir* *cash-funds*)
	    (write-indexer-ior-to-file server *cash-dir-inx*)
	    (write-retriever-ior-to-file server *cash-dir-ret*)
	    (setf *self-description* "Collection of documents on cash management mutual funds.")))
    (setf *diret-server* server)
    *diret-server*))

;;; end-of-file
