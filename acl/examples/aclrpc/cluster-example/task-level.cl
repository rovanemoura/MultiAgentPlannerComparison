;; copyright (c) 2005-2015 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: task-level.cl,v 1.3 2007/04/17 21:51:29 layer Exp $

(in-package :user)

;;; Methods defined in the application program:
;;;
;;;   application-body work-descriptor
;;;         This function is called once in each sub-process started by
;;;         the task manager.
;;;
;;;   report-one key value
;;;         This function is called in the control process to make
;;;         the final recording of a result.
;;;
;;; Functions called by application program:
;;;
;;;   run-worker-tasks work-lists report-stream host remote-hosts
;;;         The application calls this function when the task has been
;;;         partitioned into sub-tasks.
;;;       work-list is a list of work-descriptors
;;;       report-stream is nil, t, or a stream
;;;       host is nil for (short-site-name), or a string hostname
;;;       remote-hosts is a list of host descriptors
;;;
;;;   record-one-result key value progress
;;;          The application calls this function for ach result that is 
;;;          computed.
;;;
;;; work-descriptor -> (:name index [:batch integer] [:total integer] ...)
;;; host-descriptor -> (hostname command-head)
;;;            

(defvar *work* nil)
(defvar *lock* (mp:make-process-lock :name "work lock"))
(defvar *dispatched* nil)
(defvar *recorded* nil)
(defvar *workers* nil)
(defvar *finite*)
(defvar *done*)
(defvar *task-descriptor* nil)



(defun get-work (from &aux w)
  ;; This function is called once from each remote worker task.
  (mp:with-process-lock 
   (*lock*)
   (setf w (pop *work*))
   (push (cons from w) *dispatched*)
   (push (list* :port *rpc-port* :worker from w) *workers*)
   )
  (values-list w))

(defun get-worker (key value prop)
  (dolist (w *workers*)
    (when (equal value (getf w key)) (return (getf w prop)))))

;; Defined in application
(defgeneric report-one (item value))

(defun report-work-list (name progress &rest results &aux (n 0))
  (mp:with-process-lock 
   (*lock*)
   (do ((tl results (cddr tl))) ((atom tl))
     (incf n) (incf *finite*)
     (report-one (first tl) (second tl)))
   (push (list n name nil progress) *recorded*)
   ))

(defun report-work-done (finite name from)
  (mp:with-process-lock
   (*lock*)
   (push (list finite name from) *recorded*) (incf *done*)))

;; Defined in application
(defgeneric application-body (work))

(defclass task-descriptor ()
  ((count    :accessor task-descriptor-count    :initarg :count :initform 0)
   (batch    :accessor task-descriptor-batch    :initarg :batch :initform nil)
   (port     :accessor task-descriptor-port     :initarg :port)
   (name     :accessor task-descriptor-name     :initarg :name)
   (work     :accessor task-descriptor-work     :initarg :work  :initform nil)
   (progress :accessor task-descriptor-progress :initform nil)
   (data     :accessor task-descriptor-data     :initarg :data  :initform nil)))
   
(defmethod task-descriptor-get (prop)
  (getf (task-descriptor-work *task-descriptor*) prop))


(defun run-one-sub-task ()

  ;; This function is called when a worker image is started.
  ;; It expects command line arguments:
  ;;   -- -host hostname -port portnum 
  ;; Other parameters are obtained by calling the control image.

  (let* ((cmd (cdr (sys:command-line-arguments)))
	 (host (second (member "-host" cmd :test #'equalp)))
	 (port (parse-integer (second (member "-port" cmd :test #'equalp))))
	 (r (make-rpc-client nil :remote-host host :remote-port port))
	 batch work finite)
    (with-remote-port
     (r)
     ;; work -> (:begin start-index :end end-index :name worker-index
     ;;                             :max max-records ...)
     (setf work (multiple-value-list
		 (rcall "get-work" (short-site-name)))))
    (setf batch (or (getf work :batch) 1000))
    (setf batch (min batch (ash call-arguments-limit -1)))
    (setf *task-descriptor* (make-instance 'task-descriptor :batch batch
					   :port r
					   :name (getf work :name)
					   :work work
					   ))
    (format t "~&; begin worker ~A~%" work)
    (setq finite (application-body work))
    (call-report-work-list)
    (with-remote-port
     (r :close t :final t)
     (rcall "report-work-done"
	    finite (getf work :name) (short-site-name)))
    ))

(defparameter *remote-command* "rsh")
(defun start-lisp-image (&key remote-host local-host port index (program "load.cl"))

  ;; remote-host -> hostname
  ;;             -> ( hostname start-lisp-command [image-name-or-nil] )
  ;;  if rpc-count is specified, it must be a positive integer
  ;;         transmit results in batches of rpc-count instead of writing to a file

  (let* ((cmd (sys:command-line-arguments :application nil))
	 (acl (if (consp remote-host) (second remote-host) (first cmd)))
	 (image (if (consp remote-host) 
		    ;; Mention image on remote host only if explicitly asked for.
		    (third remote-host)
		  (when (null remote-host)
		    ;; If running second image on this host, use the same image.
		    (second (member "-I" cmd :test #'equal)))))
	 
	 (rname (if (consp remote-host) (first remote-host) remote-host))
	 )
    (if remote-host
	(run-shell-command
	 (format nil
		 "~A ~A ~A ~A ~A -q -L ~A ~A -- -host ~S -port ~A"
		 ;;1  2  3  4  5        6  7           8        9
		 *remote-command*      ;;; 1
		 rname                 ;;; 2
		 acl                   ;;; 3
		 (if image "-I" "")    ;;; 4
		 (or image "")         ;;; 5
		 program               ;;; 6
		 "-backtrace-on-error" ;;; 7
		 local-host            ;;; 8
		 port                  ;;; 9
		 )
	 :output (format nil "~A~A.log" rname index)
	 :if-output-exists :supersede
	 :error-output (format nil "~A~Aerr.log" rname index)
	 :if-error-output-exists :supersede
	 :wait nil)
      (run-shell-command
       (format nil
	       "~A ~A ~A -q -L ~A ~A  -- -host ~S -port ~A"
	       acl (if image "-I" "") (or image "") program "-backtrace-on-error"
	       local-host port
	       )
       :output (format nil "local~A.log" index)
       :if-output-exists :supersede
       :error-output  (format nil "local~Aerr.log" index)
       :if-error-output-exists :supersede
       :wait nil))))


;; Called by application
(defun run-worker-tasks (work-list report-stream host remote-hosts)
  (let (server port (time 0) n)
    (unwind-protect
	(let ()
	  (setf *finite* 0 *work* nil *done* 0 *recorded* nil *dispatched* nil
		*workers* nil
		)
	  (setf *work* work-list)
	  (setf server (make-rpc-server nil :open :listener))
	  (setf port (third (rpc-open-p server)))
	  (dotimes (j (setf n (length *work*)))
	    (start-lisp-image :remote-host (pop remote-hosts)
			      :index j :local-host host :port port))
		  
	  (loop
	   ;; Wait in this loop until all the work is done
	   ;;  and the results are recorded.
	   (when (eql *done* n) (return))
	   (sleep 1)
	   (incf time)
	   (when (and *work* (< (* n 120) time))
	     (cerror "wait some more" "All the work is still not dispatched ~S" *work*)
	     (setf time 0))
	   (when  *dispatched*
	     (mp:with-process-lock 
	      (*lock*)
	      (dolist (d *dispatched*)
		(format t "~&; dispached work to ~A ~S~%" (first d) (cdr d)))
	      (setf *dispatched* nil)))
	   (when  *recorded*
	     (mp:with-process-lock 
	      (*lock*)
	      (dolist (r (reverse *recorded*))
		(format t "~&; recorded ~A results from ~A ~A ~A~%"
			(first r) (second r)
			(or (third r) "")
			(if (fourth r)
			    (let* ((where (fourth r))
				   (total (get-worker :name (second r) :total))
				   (pct (and (numberp where) (numberp total)
					     (truncate (* where 100) total)))
				   )
			      (if total
				  (format nil "after ~A of ~A ~A"
					  (fourth r) total
					  (if pct (format nil "(~A%)" pct) ""))
				(format nil "after ~A" (fourth r))))
			  "")))
	      (setf *recorded* nil)))
	   )

	  (format report-stream "~&% Found ~d finite distances." *finite*)	
	  *finite*)

      ;; Clean up on exit.

      (when server
	(dolist (w *workers*)
	  (let ((port (getf w :port)))
	    (case (rpc-open-p port)
	      (:connected
	       (format report-stream "~&; try to clean up worker ~S~%" w)
	       (or
		(ignore-errors
		  (with-remote-port
		   (port :close t :final t)
		   (or (ignore-errors (rcall-one-way "excl:exit" 0) t)
		       (format report-stream "~&; failed to shutdown worker ~S~%" w))
		   (sleep 1))
		  t)
		(format report-stream "~&; failed to clean up worker ~S~%" w)))
	      ((nil :closed) nil)
	      (otherwise
	       (format report-stream "~&; worker in odd state ~S~%" w))
	      ))))
      (rpc-close server :stop :final :all t :wait t))))


(defun call-report-work-list ()
  (with-remote-port ((task-descriptor-port *task-descriptor*))
		    (rapply "report-work-list"
			    (task-descriptor-name *task-descriptor*)
			    (task-descriptor-progress *task-descriptor*)
			    (task-descriptor-data *task-descriptor*)))
  (setf (task-descriptor-count *task-descriptor*) 0
	(task-descriptor-data *task-descriptor*) nil))

(defun add-to-work-list (key value progress)
  (push value (task-descriptor-data *task-descriptor*))
  (push key (task-descriptor-data *task-descriptor*))
  (incf (task-descriptor-count *task-descriptor*))
  (setf (task-descriptor-progress *task-descriptor*) progress)
  (when (not (< (task-descriptor-count *task-descriptor*)
		(task-descriptor-batch *task-descriptor*)))
    (call-report-work-list)))


;; Called by application
(defun record-one-result (key value progress)
  (if *task-descriptor*
      ;; in a sub-task working on a fragment, we send the 
      ;;    result to the control process periodically
      (add-to-work-list key value progress)
    ;; in the control task, we call the application-specific
    ;;    method to record the result
    (report-one key value)))


