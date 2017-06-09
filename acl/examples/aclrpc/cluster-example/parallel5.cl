

(declaim (optimize (speed 3) (saftey 1) (space 0) (debug 0)))


(in-package :user)


;; Self-contained version of medline distance code, 
;; extracted from Larry Hunter's geneinfo suite. 

;; Copyright Larry Hunter and the Regents of the University of Colorado, 2005.
;;  All rights reserved.  Do not distribute.

;; This code calculates all the pairwise distances between a list of genes, 
;; based on comentions in articles in medline. 
;; Works fine for all 25,000 or so human genes -- try 

;  (calculate-all-entrez-gene-distances (human-genes-with-pmids))

;; However, the calculation is inherently O(n^2) and takes impractically long 
;; for the entire entrez gene database, which has over a million genes.
;; Doing it on a parallel cluster should make this practical. 

;; (calculate-all-entrez-gene-distances (all-genes-with-pmids))

;;; Here's the code.  
;;; Please email if you need help understanding what I meant to do. 
;;; This code is extracted from a larger system, and was not really 
;;; intended for others to read. 


;; A macro that displays how long the body takes.
;; Useful in determining which parts to parallelize.
(defmacro with-real-time (name &body body &aux (nvar (gensym)) (tvar (gensym)))
  `(let ((,nvar ,name) (,tvar (get-universal-time)))
     (multiple-value-prog1
      (progn ,@body)
      (or (< (setf ,tvar (- (get-universal-time) ,tvar)) 2)
	  (format t "~&; ~A ~A seconds.~%" ,nvar ,tvar)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Medline (Google) distance.  See http://www.arxiv.org/abs/cs.CL/0412098
 
;; The function from the Google paper.  

(defun ngd (count1 count2 count-and total)
  (if (= 0 count-and)
      'infinite
    (/ (- (max (log count1) (log count2)) (log count-and))
       (- (log total) (min (log count1) (log count2))))))

;; Tools to apply medline distance to genes.

;; Calculate all distances among pairs of genes supplied as a list of Entrez gene ids. 
;; This version uses the pmids associated with the genes in Entrez Gene 
;; (not including the GeneRIF pmids). There are likely to be many more instances 
;; of gene references in the literature, but these are human curated. 

;; Insist on at least min-hits (default 2) publications per gene to produce
;; a distance, and don't count "promiscuous" publications that are linked
;; to too many genes (default 100 or more).
;; Only finite distances are returned.
;; If one (or both) of the genes have too few non-promiscuous publications, or
;; if the gene pair has no overlapping non-promiscuous publications,
;; there is no entry in the table (distance returns nil).

;; To calculate all distances efficiently, first make a list of all 
;; gene/pubmed ids that pass the minimum tests, and calculate/store
;; only meaningful distances.  For human, there are about 130k of
;; these.  But the numbers are always going up...

(defvar *gene-entrez-pmid-distance-table* (make-hash-table :test #'eql :size 150000))

;; Make noise when reading/parsing large files

(defvar *peep-freq* 100000)
(defvar *pairs* 0)
(defvar *total* 0)
(defvar *data-limit* nil)


(defun calculate-all-entrez-gene-distances
  (gene-ids &key (min-hits 2) (max-genes-per-pmid 100)
	    (peep-freq 1000000)
	    (report-stream t)
	    (region 
	     ;; nil -- do it all here
	     ;;  n  -- break up into n regions on n processors
	     ;; cons -- do only the region specified
	     ;;         cons=(:begin begin :end end :name index :max max :batch batch )
	     nil)
	    host
	    (max-records *data-limit*)
	    remote-hosts batch
	    )

  ;; This variable is not created until gene2pubmed is read in. 
  (declare (special *gene2pubmed-pmid-index*))

  (let* ((bad-pmids nil)
	 (bad-gene-count 0)
	 ;;(good-gene-ids nil)
	 (total-gene-ids (length gene-ids))
	 (good-gene-ids (make-array total-gene-ids))
	 (good-gene-index 0)
	 (total-good-pmids 0)
	 (gene-id-to-good-pmid-table (make-hash-table :test #'eql :size 24000)))
    (setf *peep-freq* peep-freq)
    (format
     report-stream
     "~&% Calculating all Entrez gene pubmed distances for ~d ids (up to ~d distances)~%"
     total-gene-ids (/ (* total-gene-ids (1- total-gene-ids)) 2))
    
    ;; Ensure we've downloaded the gene2pubmed records.. 
    ;; This will read them if they haven't been already.
    (assert (> (length (gene2pubmed-records)) 0))

    ;; Identify promiscuous pmids

    (with-real-time
     "Identify promiscuous pmids"
     (maphash (lambda (pmid records)
		(if (> (length records) max-genes-per-pmid)
		    (push pmid bad-pmids)
		  (incf total-good-pmids)))
	      *gene2pubmed-pmid-index*)
     )
    (format
     report-stream
     "~&% Removing ~d promiscuous pmids(more than ~d associated genes).  Pmid universe is ~d.~%% Removing promiscuous pmids from gene records~%" 
     (length bad-pmids) max-genes-per-pmid total-good-pmids)

    ;; Identify genes with enough non-promiscuous pmids
    ;;  to be worth calculating a distance for

    (with-real-time
     "Identify genes with enough non-promiscuous pmids"
     (dolist (gene-id gene-ids)
       (let ((good-pmids (set-difference 
			  (mapcar #'gene2pubmed-pmid
				  (lookup-gene2pubmed-by-gene-id gene-id))
			  bad-pmids :test #'=)))
	 (if (< (length good-pmids) min-hits)
	     (incf bad-gene-count)
	   (progn 
	     (setf (aref good-gene-ids good-gene-index) gene-id)
	     (incf good-gene-index)
	     (setf (gethash gene-id gene-id-to-good-pmid-table) good-pmids)))))
     )
    (format report-stream
	    "~&% Removed ~d genes that had ~d or fewer non-promiscuous pmids~%"
	    bad-gene-count (1- min-hits)) 

;;;; vvvv **PARALLELIZATION WOULD HAPPEN HERE** vvvvvv

    ;; Iterate through all pairs to set finite distances.  
    (format report-stream "~&% Now calculating ~d distances for ~d genes...~%" 
	       (setf *total* (/ (* good-gene-index (1- good-gene-index)) 2))
	       good-gene-index)

    ;; When there is too little data, skip any attempts to split the task.
    (when (< good-gene-index 1000) (setf region nil))

    (or host (setf host (short-site-name)))

    (with-real-time
     "Iterate through all pairs"
     (let (
	   ;;(count 0)
	   (finite 0)
	   )
       (setf *pairs* 0)
       
       (typecase region
	 (null
	  
	  ;; Simple case - do it all here and now

	  (with-real-time 
	   "all in one"
	   (setf finite (iterate-over-triangle
			 good-gene-ids 0 (1- good-gene-index)
			 gene-id-to-good-pmid-table total-good-pmids report-stream)))
	  (format report-stream "~&% Found ~d finite distances in ~A pairs."
		  finite *pairs*))


	 ((integer 1 2000)

	  ;; Partition the task into equal regions 
       
	  (let* (nr regions scatter)
	    (if (< region 1000)
		(setf nr region scatter t)
	      (setf nr (- region 1000) scatter nil))
	    (setf regions (cut-up nr good-gene-index))
	    (if scatter
		(run-worker-tasks
		 (regions-to-work regions batch max-records)
		 report-stream host remote-hosts)
	      (run-all-regions-locally
	       regions report-stream good-gene-index good-gene-ids
	       gene-id-to-good-pmid-table total-good-pmids))))
	 (cons

	  ;; we are only doing a part of the task here
	  
	  (run-one-region
	   region good-gene-index good-gene-ids 
	   gene-id-to-good-pmid-table total-good-pmids report-stream)
	  )
	    

	 ) ;;;end typecase region

       )) ;;; end with-real-time "Iterate through all pairs"
    ))

;; ^^^ END PARALLELIZE THIS SECTION ^^^^

;; A trick to make this faster is to avoid using conses.  
;; The largest gene-id at the moment is 3371220,
;;  so we are pretty safe making a numeric id for a pair 
;;  as 10,000,000xgene-id1+gene-id2.  Bonus is to
;;  normalize by always making the larger id the second one. 

(defun gene-pair-id (id1 id2)
  (+ (* 10000000 (min id1 id2)) (max id1 id2)))


(defun id-to-gene-pair (id)
  (floor id 10000000))

;;; To get the data, you need the functions
;;; gene2pubmed-pmid and lookup-gene2pubmed-by-gene-id.  
;;; You also need (human-genes-with-pmids) and (all-genes-with-pmids)
;;; to create the gene-lists to input.
;;; The defining functions come from a fairly hairy macro called defncbi-ftpfile.



;; Return non-redundant list of gene-ids that are assocated with PMIDs

(defun all-genes-with-pmids ()
  (let ((table (make-hash-table :test #'eql :size 1000000))
	(geneids nil)
	(all (gene2pubmed-records))
	)
    (with-real-time
     'all-genes-with-pmids
     (dolist (record all)
       (setf (gethash (gene2pubmed-gene-id record) table) t))
     (maphash (lambda (key value)
		(declare (ignore value))
		(push key geneids))
	      table)
     geneids)))


;; Same, but just for human genes. NCBI's taxonomy id for human is 9606.

(defun human-genes-with-pmids ()
  (let ((human-taxid 9606)
	(table (make-hash-table :test #'eql :size 25000))
	(geneids nil)
	(all (gene2pubmed-records))
	)
    (with-real-time
     'human-genes-with-pmids
     (dolist (record all)
       (when (= human-taxid (gene2pubmed-tax-id record))
	 (setf (gethash (gene2pubmed-gene-id record) table) t)))
     (maphash (lambda (key value)
		(declare (ignore value))
		(push key geneids))
	      table)
     geneids)))
    

;; The below is overkill for this demonstration, 
;; but I wasn't about to try to trim it down.  All it
;; really does is grab the ftp file, parse it, and create hash table indices.



;; DEFNCBI-FTPFILE  builds internal representations of NCBI ftp files
;;
;; It takes as arguments a class name,
;;    a string with the file location on the ftp.ncbi.nlm.nih.gov web site,
;;    and a list of field specifications.
;; Each fieldspec has a slot-name (for the class),
;;    a type specification (must be a lisp typespec)
;;    and an optional flag to build an index on that field. 

;; The macro creates the class (with the slots),
;;    a function called (<name>-records) which returns
;;    instances of the class for all entites in the file
;;    (downloading when necessary, or when its optional :reload? argument is true,
;;    and doing basic type conversions).
;; For each slot that is indexed,
;;    a function called (lookup-<name>-by-<slot>) is defined;
;;    these lookups always return lists of instances.

;; For example:

;;  (defncbi-ftpfile gene2go "/gene/DATA/gene2go.gz"
;;     (gene-id integer t)
;;     (go-id string t)
;;     (evidence-code symbol))
;;
;; Means that (gene2go-records) will return a list of instances of a gene2go class
;; (with slots gene-id, go-id and evidence-code),
;; and (lookup-gene2go-by-gene-id <id>) and (lookup-gene2go-by-go-id <id>)
;; do the right thing. 

;; Assumes that the files are all one line per "record" and tab delimited. 
;; Fields that contain lists are | delimited.  Internal testing stuff 
;; includes download-<filename>-records, which takes a :max-records argument,
;; and *peep-freq* which offers to make some indication of progress.

;; Still to do is to create some integrated view of all of this data.
;; Also, would be nice to have reasonable initial sizes specified 
;; for the hash tables, instead of the arbitrary 100k

(eval-when (:compile-toplevel :load-toplevel :exec)


 (defvar *entrez-gene-ftp-site* "ftp.ncbi.nlm.nih.gov")

 (defparameter *anon-ftp-password* "Larry.Hunter@uchsc.edu")


 (defmacro defncbi-ftpfile (name fileloc &rest fieldspecs)
  (flet ((make-name (format-string &rest args)
	   (intern (apply #'format `(nil ,format-string ,@args)))))
    (let* ((indexed-slots nil)
	   (record-list-fs "*~a-records*")
	   (record-list-fn-fs "~a-records")
	   (download-fn-fs "download-~a-records")
	   (parse-line-fn-fs "~a-parse-lines")
	   (index-fs "*~a-~a-index*")
	   (index-fn-fs "lookup-~a-by-~a")
	   (slot-defs (delete nil
			      (mapcar
			       (lambda (spec)
				 (destructuring-bind (slot-name type-spec &optional index?) spec
				   (unless (eq 'ignore type-spec)
				     (when index? (push spec indexed-slots))
				     `(,slot-name :initarg ,(intern slot-name :keyword)
						  :type ,(if (symbolp type-spec)
							     type-spec
							   (cons 'cons type-spec))
						  :accessor ,(intern
							      (format nil "~a-~a" name slot-name))
						  ))))
				      fieldspecs)))
	   (record-list-name (make-name record-list-fs name))
	   (record-list-function-name (make-name record-list-fn-fs name))
	   (download-function-name (make-name download-fn-fs name))
	   (parse-line-function-name (make-name parse-line-fn-fs name)))
    `(progn
       (defclass ,name nil ,slot-defs)
       (defvar ,record-list-name nil)
       ,@(mapcar (lambda (spec)
		   (destructuring-bind (slot-name type-spec index?) spec
		     (declare (ignore index?))
		     (let ((index-name (make-name index-fs name slot-name))
			   (fn-name (make-name index-fn-fs name slot-name))
			   (test (if (eq type-spec 'string) 'equalp 'eql)))
		       `(progn 
			  (defparameter ,index-name (make-hash-table :test ',test :size 100000))
			  (defun ,fn-name (key) 
			    (,record-list-function-name)  ; ensures that the index is built
			    (gethash key ,index-name))))))
		indexed-slots)
       (defun ,record-list-function-name (&key remote? reload? (max-records *data-limit*))
	 (with-real-time 
	  ',record-list-function-name
	  (or (and (not reload?) ,record-list-name)
	      (,download-function-name :remote? remote? :max-records max-records))))
       (defun ,download-function-name (&key remote? max-records
					    &aux
					    (local (format nil "~A.data" ',name))
					    (local? (and (null remote?) (probe-file local))))
	 (if local?
	     (format t "~&Reading ~a from local file...~%" ',name)
	   (format t "~&Downloading ~a from NCBI...~%" ',name))
	(let ((old-gc excl:*global-gc-behavior*))
	   (setq excl:*global-gc-behavior* nil)
	   (setq ,record-list-name nil)
	   ,@(mapcar (lambda (slotspec)
		       `(clrhash ,(make-name index-fs name (first slotspec)))) 
		     indexed-slots)
	   (if local?
	       (with-open-file 
		(stream local)
		(do ((line (read-line stream nil 'eof) (read-line stream nil 'eof))
		     (count 1 (incf count)))
		    ((or (eq 'eof line) (and max-records (> count max-records))) 'done)
		  (when (and *peep-freq*  
			     (= 0 (rem count *peep-freq*)))
		    (format t " ~d~%" count))
		  (,parse-line-function-name line))			       

		)
	     (with-open-stream
	      (raw-stream (socket:open-ftp-stream ,fileloc 
						  :host *entrez-gene-ftp-site*
						  :password *anon-ftp-password*))
	      (with-open-stream
	       (stream (if (string-equal "gz" (pathname-type (pathname ,fileloc)))
			   (progn 
			     (util.zip:skip-gzip-header raw-stream)
			     (make-instance 'util.zip:inflate-stream 
					    :input-handle raw-stream))
			 raw-stream))
	       (with-open-file
		(out local :direction :output :if-exists :supersede)
	       (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof))
		    (count 1 (incf count)))
		   ((or (eq 'eof line) (and max-records (> count max-records))) 'done)
		 (write-line line out)
		 (when (and *peep-freq*  
			    (= 0 (rem count *peep-freq*)))
		   (format t " ~d~%" count))
		 (,parse-line-function-name line))))))
	   (setf *data-limit* (when max-records (length ,record-list-name)))
	   (gc t)
	   (setq excl:*global-gc-behavior* old-gc)
	   ,record-list-name))
       (defun ,parse-line-function-name (line)
	(declare (special ,@(mapcan (lambda (slotspec)
				       (destructuring-bind (slot-name type-spec &optional index?)
					   slotspec
					(declare (ignore type-spec))
					(when index?
					   (list (make-name index-fs name slot-name)))))
				     fieldspecs)))
	(let ((data (utils:lex-string line '(#\tab)))
	       (object (make-instance ',name)))
	   (when (= (length data) ,(length fieldspecs))
	     ,@(mapcar (lambda (spec)
			(destructuring-bind (slot-name type-spec &optional index?) spec
			   `(let ((datum (convert-ncbi-to (pop data) ',type-spec)))
			      (unless (eq ',type-spec 'ignore)
				(setf (slot-value object ',slot-name) datum)
				,@(when index?
				    `((push object (gethash datum ,(make-name index-fs name slot-name)))))))))
		       fieldspecs)
	     (push object ,record-list-name))))))))

) ;end eval-when


(defncbi-ftpfile gene2pubmed "/gene/DATA/gene2pubmed.gz"
  (tax-id integer)
  (gene-id integer t)
  (pmid integer t))

;; Helpers...

;; The site address

;; Simple conversions.

(defun convert-ncbi-to (datum typespec)
      (if (string= datum "-") 
	nil
	(if (listp typespec)
	    (let ((data (utils:lex-string datum '(#\|))))
	      (mapcar (lambda (subdatum)
			(convert-ncbi-to subdatum (first typespec)))
		      data))
	  (case typespec
	    ((ignore) nil)
	    ((integer) (parse-integer datum))
	    ((string) datum)
	    ((symbol) (intern (string-downcase datum)))
	    (else datum)))))


;; util.zip functions are from franz's inflate-stream. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ADDITIONS
;;;
;;;


;;; iterate over sub-regions of the data

(defun iterate-one (id1 id2 table total)
  (let* ((gene1-pmids (gethash id1 table))
	 (gene2-pmids (gethash id2 table))
	 (distance (ngd (length gene1-pmids) (length gene2-pmids)
			(length (intersection gene1-pmids gene2-pmids :test #'=))
			total)))
    (incf *pairs*)
    (if (numberp distance)
	(progn (record-one-result (gene-pair-id id1 id2) distance *pairs*)
	       1)
      0)))

  
(defun iterate-over-triangle (ids begin end table total report-stream
				  &aux (h (- end begin)) ix jx (w h)
				  (finite 0) (count 0))
  ;; the first index, ix, varies from begin through end-1
  ;; the second index, jx, varies from ix+1 through end
  ;; if begin=0 and end=4 we iterate over the following triangle:
  ;;    0,1  0,2  0,3  0,4
  ;;         1,2  1,3  1,4
  ;;              2,3  2,4
  ;;                   3,4
  (dotimes (i h)
    (setf ix (+ begin i))
    (dotimes (j w)
      (setf jx (+ ix 1 j))
      (incf count)
      (incf finite 
	    (iterate-one (aref ids ix) (aref ids jx) table total))
      (when (and *peep-freq* (< *peep-freq* count))
	(format
	 report-stream
	 "~&; iterate-over-triangle ~A times, ~A results. Scanned ~A(~A%)~%"
	 count finite *pairs* (truncate (* *pairs* 100) *total*)
	 )
	(setf count 0))
      )
    (decf w))
  finite)
 
(defun iterate-over-rectangle (ids begin1 end1 begin2 end2 table total report-stream
				   &aux (finite 0) (count 0))
  ;; ASSUME that the two ranges do not overlap!
  (dotimes (i1 (- end1 begin1))
    (dotimes (i2 (- end2 begin2))
      (incf count)
      (incf finite 
	    (iterate-one (aref ids (+ begin1 i1)) (aref ids (+ begin2 i2)) table total))
      (when (and *peep-freq* (< *peep-freq* count))
	(setf count 0)
	(format
	 report-stream
	 "~&; iterate-over-rectangle ~A times, ~A results.~%"
	 *peep-freq* finite))
      ))
  finite)



;;; Partition the data into equal size regions

(defun cut-up (r n)

  ;; N=good-gene-index   P=all pairs=N(N-1)/2      R=number of regions
  ;; each region size A=P/R
  ;; First region: h high w wide  
  ;; ...
  ;; Last region:            triangle

  (flet ((trs (n) (/ (* n (1+ n)) 2)))
    (let* ((p (trs (1- n)))
	   (a (/ p r))
	   (width 0)
	   h1 ;; h2
	   (height (1- n))
	   new (all 0)
	   regions ;;  ((height total-size triangle rectangle) ...)
	   )
      (flet ((new-region (h w &aux (tr (trs h)) (rec (* h w)))
			 (list h (+ tr rec) tr rec)))
	(loop
	 (when (eql r 1)
	   (push (setf new (new-region height width)) regions)
	   (incf all (second new))
	   (return))

	 (setf h1 (round (* 0.5 (- (sqrt (+ (* 8 a) (* (+ 1 width width)
						       (+ 1 width width))))
				   (+ 1 width width)))))

	 (push (setf new (new-region h1 width)) regions)
	 (incf all (second new))
	 (decf r)
	 (decf height h1)
	 (incf width h1))

	(format t "~&; ~S ~%" (list* p all regions))

	regions))))



;;; TASK MANAGEMENT

(defun run-all-regions-locally (regions report-stream good-gene-index good-gene-ids
					gene-id-to-good-pmid-table total-good-pmids
					&aux (org 0) h pct (finite 0))
  ;; Do all regions serially in this process to verify that 
  ;; partitioning is correct.
  (dolist (r regions
	     (format report-stream
		     "~&% Found ~d finite distances in ~A pairs."
		     finite *pairs*))
    (setf h (first r))
    (with-real-time
     (format nil "slice height ~A  size ~A ~A   " h (second r) (cddr r))
     (with-real-time
      "    triangle "
      (setf pct *pairs*)
      (incf finite (iterate-over-triangle
		    good-gene-ids
		    org (+ org h)
		    gene-id-to-good-pmid-table total-good-pmids report-stream))
      (format report-stream "~&;   triangle count: ~A~%" (- *pairs* pct))
      )
     (with-real-time
      "    rectangle "
      (setf pct *pairs*)
      (incf finite (iterate-over-rectangle 
		    good-gene-ids
		    org (+ org h) (+ org h 1) good-gene-index
		    gene-id-to-good-pmid-table total-good-pmids report-stream))
      (format report-stream "~&;   rectangle count: ~A~%" (- *pairs* pct))
      )
     )
    (incf org h)
    ))


(defun run-one-region (region good-gene-index good-gene-ids
			      gene-id-to-good-pmid-table total-good-pmids report-stream)

  ;; This function is called in the worker images started by run-worker-tasks.

  (let ((finite 0) 
	(begin (getf region :begin))
	(total (getf region :total))
	(end (getf region :end)))
    (when total (setf *total* total))
    (incf finite (iterate-over-triangle
		  good-gene-ids begin end 
		  gene-id-to-good-pmid-table total-good-pmids report-stream))
    (incf finite (iterate-over-rectangle 
		  good-gene-ids begin end (1+ end) good-gene-index
		  gene-id-to-good-pmid-table total-good-pmids report-stream))
    finite))


;;; Application-specific methods for generic functions 
;;; called by task management layer.

(defmethod report-one (item value)
  (setf (gethash item *gene-entrez-pmid-distance-table*) value))

(defmethod application-body (work &aux (max-records (getf work :max)))
  (setf *data-limit*  max-records)
  (calculate-all-entrez-gene-distances 
    (human-genes-with-pmids) :region work :max-records max-records))


(defun regions-to-work (regions batch max-records &aux work (org 0) (i 0))
  (dolist (r regions)
    (push (list :begin org :end (+ org (first r)) :name i 
		:batch batch :max max-records :total (second r)
		)
	  work)
    (incf i) (incf org (first r)))
  work)


  