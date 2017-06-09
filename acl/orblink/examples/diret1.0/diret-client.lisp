#|
========================
File: diret-client.lisp
Author: Vladimir Kulyukin 
Description: Dir-client acting on two proxies.
Copyright (c) 1999 

Comments and bugs to vkulyukin@cs.depaul.edu 
========================
|#

(eval-when (load eval compile)
  
  (unless (find :diret-params *features*)
     (load "diret"))

  (unless (find :dmapvs *features*)
     (load "dmapvs-loader"))
  )

(require :orblink)

(in-package :cl-user)

(eval-when (compile eval load)
  (unless (find-package :diret)
     (corba:idl "diret.idl"))
  
  (pushnew :diret-client *features*)
  
  (intern 'diret-subscription-dialog :cg-user)
  (intern 'diret-cp-message :cg-user)  
  )

(export '(*DIRET-CLIENT* MAKE-DIRET-CLIENT ACTIVATE-SOURCE-PROXY DEACTIVATE-SOURCE-PROXY
           ACTIVATE-USER-PROFILE))

(defparameter *diret-client* nil)
(defparameter *p-sim-thresh* 0.1)

;;; ==================== DIRET SOURCE PROXY ===================
;;; single-client is a proxy to exactly one source.

(defclass diret-source-proxy ()
    ((source-id :initform nil :initarg :source-id :accessor source-id)
     (indexer   :initform nil :initarg :indexer   :accessor indexer)
     (retriever :initform nil :initarg :retriever :accessor retriever)))

(defun make-diret-source-proxy (&key retriever indexer source-id)
   (make-instance 'diret-source-proxy 
     :source-id source-id 
     :indexer   indexer
     :retriever retriever))

(defmethod respond-to-retrieve ((dsp diret-source-proxy) (query string))
   (op:retrieve (retriever dsp) query))

(defmethod respond-to-computeCentroid ((dsp diret-source-proxy))
   (op:computeCentroid (indexer dsp)))

(defmethod respond-to-fetchDataItem ((dsp diret-source-proxy) (id string))
   (op:fetchDataItem (retriever dsp) id))

(defmethod respond-to-describeDataItem ((dsp diret-source-proxy) (id string))
   (op:describeDataItem (retriever dsp) id))

(defmethod respond-to-describeDataItemFully ((dsp diret-source-proxy) (id string))
   (op:describeDataItemFully (retriever dsp) id))

(defmethod respond-to-describeSelf ((dsp diret-source-proxy))
   (op:describeSelf (retriever dsp)))

;;; =================== DIRET CLIENT ========================

(defclass diret-client ()
    ((source-proxies  :initform (make-hash-table) 
                      :initarg :source-proxies 
                      :accessor source-proxies)
     (p-centroids     :initform (make-hash-table) 
                      :initarg :p-centroids 
                      :accessor p-centroids)
     (dmapvs          :initform (ir:make-dmapvs)
                      :initarg :dmapvs
                      :accessor dmapvs)
     (p-sim-threshold :initarg :p-sim-threshold
                      :initform *p-sim-thresh*
                      :accessor p-sim-threshold)))

(defun make-diret-client () 
   (make-instance 'diret-client))

(defmethod initialize-instance :after ((dc diret-client) &rest args)
   (ir::write-sim-score-threshold (dmapvs dc) (p-sim-threshold dc)))

(defmethod read-p-sim-threshold ((dc diret-client))
   (p-sim-threshold dc))

(defmethod write-p-sim-threshold ((dc diret-client) (x number))
   (setf (p-sim-threshold dc) x)
   (ir::write-sim-score-threshold (dmapvs dc) x))

(defmethod write-similarity-metric ((dc diret-client) (sm t))
   (ir::write-similarity-metric (dmapvs dc) sm))

(defmethod activate-source-proxy ((dc diret-client) (id symbol))
   (add-source-proxy dc id)
   (add-source-p-centroid dc id))

(defmethod deactivate-source-proxy ((dc diret-client) (id symbol))
   (delete-source-proxy dc id)
   (delete-source-p-centroid dc id))

(defmethod clear-dmapvs ((dc diret-client))
   (ir:clear-dmapvs (dmapvs dc)))

(defmethod add-source-proxy :before ((dc diret-client) (source-id symbol))
   (unless (get-source-proxy dc source-id)
      (with-open-file (out (user-profile-path) :direction :output
                       :if-exists :append :if-does-not-exist :create)
        (format out "~S~%" source-id)
        (force-output out))))

(defmethod add-source-proxy ((dc diret-client) (source-id symbol))
   (multiple-value-bind (indexer-proxy retriever-proxy)
       (get-source-iors dc source-id)
      (let ((new-source-proxy (make-diret-source-proxy 
                               :source-id source-id
                               :indexer indexer-proxy
                               :retriever retriever-proxy)))
         (setf (gethash source-id (source-proxies dc))
               new-source-proxy))))

(defmethod add-source-p-centroid :around ((dc diret-client) (source-id symbol))
   (when (get-source-proxy dc source-id)
      (call-next-method)))

(defmethod add-source-p-centroid ((dc diret-client) (source-id symbol))
   (let* ((c (compute-centroid dc source-id))
          (vstinx (centroid-to-vs-token-index c source-id)))
      (setf (gethash source-id (p-centroids dc)) c)
      (ir::add-token-index (dmapvs dc) source-id vstinx)))

;;; VK: This also has to work on the user-profile and the centroid.
(defmethod delete-source-proxy ((dc diret-client) (source-id symbol))
   (remhash source-id (source-proxies dc)))
   
(defmethod delete-source-proxy :after ((dc diret-client) (source-id symbol))
   (let ((id-list '()))
      (with-open-file (in (user-profile-path) :direction :input)
        (loop as line = (read-line in nil :eof)
              until (eql line :eof)
              do (multiple-value-bind (id pos)
                     (read-from-string line nil :eos)
                    (declare (ignore pos))
                    (unless (or (eql id source-id) (eql id :eos))
                       (push id id-list)))))
      (with-open-file (out (user-profile-path) :direction :output
                        :if-exists :supersede)
        (loop for id in id-list
              do (format out "~s~%" id)
              finally (force-output out)))))

(defmethod delete-source-p-centroid ((dc diret-client) (id symbol))
   (remhash id (p-centroids dc))
   (ir::remove-token (dmapvs dc) id))

(defmethod get-source-iors ((dc diret-client) (source-id symbol))
   (let ((inx (ecase source-id
                (:stocks *stocks-dir-inx*)
                (:bonds  *bonds-dir-inx*)
                (:cash   *cash-dir-inx*)))
         (ret (ecase source-id
                (:stocks *stocks-dir-ret*)
                (:bonds  *bonds-dir-ret*)
                (:cash   *cash-dir-ret*))))
      (values (orblink:read-ior-from-file inx)
              (orblink:read-ior-from-file ret))))

(defmethod get-source-proxy ((dc diret-client) (source-id symbol))
   (multiple-value-bind (proxy bool)
       (gethash source-id (source-proxies dc))
      (if bool proxy bool)))

(defmethod get-source-p-centroid ((dc diret-client) (source-id symbol))
   (multiple-value-bind (pc bool)
       (gethash source-id (p-centroids dc))
      (if bool pc bool)))

(defmethod retrieve-from-proxy ((dc diret-client) (query string) (source-id symbol)
                                (dsp null))
   nil)

(defmethod retrieve-from-proxy ((dc diret-client) (query string) (source-id symbol)
                                (dsp diret-source-proxy))
   (respond-to-retrieve dsp query))
   
(defmethod compute-centroid ((dc diret-client) (source-id symbol))
   (let ((proxy (get-source-proxy dc source-id)))
      (if proxy
         (respond-to-computeCentroid proxy)
         nil)))

;;; VK: negative centroids will be added in the future.
(defmethod retrieve-sources ((dc diret-client) (query string))
   (mapcar #'ir::target-token (ir::retrieve (dmapvs dc) query)))

;;; VK: obtains retrievals from remote sources and commits
;;; them to the database.
(defmethod query-to-retrievals ((dc diret-client) (query string))
   (dolist (sid (retrieve-sources dc query))
      (let* ((dsp (get-source-proxy dc sid))
             (rets (retrieve-from-proxy dc query sid dsp)))
         (commit-retrievals-to-database dc query sid dsp rets))))

(defmethod query-to-retrievals ((dc diret-client) (query null))
   nil)

(defmethod centroid-to-vs-token-index ((c vector) (id symbol))
   (loop with tinx = (ir:make-vs-token-index :target-token id 
                                             :token-seq (make-hash-table :test #'equal))
         with tseq = (ir::token-seq tinx)
         for wtok across c
         do (setf (gethash (op:token wtok) tseq) (op:weight wtok))
         finally (return tinx)))
         
(defmethod fetch-data-item ((dc diret-client) (id string) (source-id symbol))
   (let ((proxy (get-source-proxy dc source-id)))
      (if proxy
         (respond-to-fetchDataItem proxy id)
         nil)))

(defmethod describe-data-item ((dc diret-client) (id string) (source-id symbol))
   (let ((proxy (get-source-proxy dc source-id)))
      (if proxy
         (respond-to-describeDataItem proxy id)
         nil)))

(defmethod describe-data-item-fully ((dc diret-client) (id string) (source-id symbol))
   (let ((proxy (get-source-proxy dc source-id)))
      (if proxy
         (respond-to-describeDataItemFully proxy id)
         nil)))

(defmethod describe-self ((dc diret-client) (source-id symbol))
   (let ((proxy (get-source-proxy dc source-id)))
      (if proxy
         (respond-to-describeSelf proxy)
         nil)))

(defmethod activate-user-profile :around ((dc diret-client))
   (let ((p (user-profile-path)))
      (unless (or (null (probe-file p))
                  (zerop (cg-user::file-size p)))
         (call-next-method))))

;;; VK: In the future, we should have a safe behavior in case
;;; the user did not subscribe to any sources.
(defmethod activate-user-profile ((dc diret-client))
   (with-open-file (in (user-profile-path) :direction :input)
     (loop as line = (read-line in nil :eof)
           until (eql line :eof)
           do (multiple-value-bind (source-id pos)
                  (read-from-string line nil :eos)
                 (declare (ignore pos))
                 (unless (eql source-id :eos)
                    (add-source-proxy dc source-id)
                    (add-source-p-centroid dc source-id))))))

(defmethod activate-user-profile :after ((dc diret-client))
   (when (zerop (hash-table-count (source-proxies dc)))
      (cg-user::diret-cp-message "===================" nil)
      (cg-user::diret-cp-message "Unable to activate your
information sources. Send e-mail to vkulyukin@cs.depaul.edu.")))

(defmethod subscribed-source-p ((dc diret-client) (id symbol))
   (with-open-file (in (user-profile-path) :direction :input)
     (loop as line = (read-line in nil :eof)
           until (eql line :eof)
           do (multiple-value-bind (sid pos)
                  (read-from-string line nil :eos)
                 (declare (ignore pos))
                 (when (eql sid id)
                    (return t)))
           finally (return nil))))

(defmethod get-source-ids ((dc diret-client))
   (loop for id being each hash-key in (source-proxies dc)
         collect id))

(defun start-diret ()
   (if *diret-client* 
      (clear-diret-client *diret-client*)
      (setf *diret-client* (make-diret-client)))
   (erase-user-retrievals *diret-client*)
   (activate-user-profile *diret-client*))

(defmethod clear-diret-client ((dc diret-client))
   (loop for id in (get-source-ids dc)
         do (deactivate-source-proxy dc id)
         finally (clear-dmapvs dc)
                 (erase-user-profile dc)
                 (erase-user-retrievals dc)))

(defmethod erase-user-profile ((dc diret-client))
   (with-open-file (out (user-profile-path) :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
     t))

(defmethod erase-user-retrievals ((dc diret-client))
   (with-open-file (out (user-retrievals-path) :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
     t))

(defmethod commit-retrievals-to-database ((dc diret-client) (query string)
                                          (source-id symbol) 
                                          (dsp diret-source-proxy)
                                          (retrievals null))
   nil)

(defmethod commit-retrievals-to-database ((dc diret-client) (query string)
                                          (source-id symbol)
                                          (dsp diret-source-proxy)
                                          (retrievals vector))
   (with-open-file (out (user-retrievals-path) :direction :output
                     :if-exists :append :if-does-not-exist :create)
     (loop initially (format out "(~S ~S~%" source-id query)
           for r across retrievals
           do (format out "(~S ~F~%~S)~%" (op:id r) (op:simco r)
                (respond-to-describeDataItem dsp (op:id r)))
           finally (format out ")~%")
                   (force-output out))))

(defmethod get-retrievals-from-source ((dc diret-client) (source-id symbol))
   (with-open-file (in (user-retrievals-path) :direction :input)
     (loop with rets = '()
           as r = (read in nil :eof)
           until (eql r :eof)
           when (eql (retrieval-source r) source-id)
           do (setf rets (append (retrieval-data r) rets))
           finally (setf rets (sort rets #'> :key #'retrieval-score))
                   (setf rets (remove-duplicates rets :key #'data-item-id
                                                      :test #'string=))
                   (return rets))))

(defmethod get-retrievals-from-all-sources ((dc diret-client))
   (let ((ht-rets (make-hash-table)))
      (with-open-file (in (user-retrievals-path) :direction :input)
        (loop as r = (read in nil :eof)
              until (eql r :eof)
              do (setf (gethash (retrieval-source r) ht-rets)
                       (append (retrieval-data r)
                               (gethash (retrieval-source r) ht-rets)))
              finally (let ((ht-rets2 (make-hash-table :test #'equal)))
                         (maphash #'(lambda (sid rets)
                                      (setf rets (sort rets #'> :key #'data-item-score))
                                      (setf rets (remove-duplicates rets
                                                   :key #'data-item-id
                                                   :test #'string=))
                                      (dolist (r rets)
                                         (let ((descrip (remove #\newline
                                                                (data-item-descrip r)
                                                                :test #'char=)))
                                            (setf (gethash descrip ht-rets2)
                                                  (cons (data-item-id r) sid)))))
                                  ht-rets)
                         (return ht-rets2))))))
                                                                         
(defun retrieval-source (r)
   (first r))

(defun retrieval-data (r)
   (rest (rest r)))

(defun data-item-id (d)
   (first d))

(defun data-item-descrip (d)
   (third d))

(defun data-item-score (d)
   (second d))

(eval-when (load compile eval)
  
  (unless (find :diret-gui *features*)
     (load "diret-gui-loader"))
  
  )

;;; end-of-file
