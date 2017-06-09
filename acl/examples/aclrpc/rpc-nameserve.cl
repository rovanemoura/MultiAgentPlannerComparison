;; -*- mode: common-lisp; package: user -*-
;;
;; copyright (c) 2000-2005 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 2002-2015 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: rpc-nameserve.cl,v 4.4 2007/04/17 21:51:29 layer Exp $

(in-package #:user)

(eval-when (compile load eval) (require :aclrpc))

;;; A code example using ACLRPC Datagram socket connections
;;;   to implement a simple name server.
;;;
;;; Top-level interface:
;;;
;;; start-name-server &key host port verbose debug    Function
;;;
;;; *name-server-host*                                Variable
;;;    Default host for server.  
;;;    Initial value is (short-site-name)
;;; 
;;; *name-server-port*                                Variable
;;;    Default port number for server.
;;;    Initial value is 7260.
;;;
;;; set-name-server &key host port timeout            Function
;;;                      verbose debug
;;;    Set values of global variables.
;;;
;;; name-register name &rest values                   Function
;;;    Returns :done, :duplicate or :timeout-or-error
;;;
;;; name-lookup name                                  Function
;;;    Returns as multiple values the values registered
;;;    for the name argument.

(defpackage #:user 
  (:use #:net.rpc)
  )

(defvar *name-server-port* 7260)
(defvar *name-server-host* (short-site-name))
(defvar *name-server-verbose* nil)
(defvar *name-server-debug* nil)
(defvar *name-server-timeout* 15)


;;; This is the top-level function in the server image

(defclass rpc-name-server-port (rpc-datagram-port) ())
(defclass rpc-name-server (rpc-datagram-server)
  ((port-instance :initform (make-instance 'rpc-name-server-port))))

(defun start-name-server (&key (host *name-server-host*) 
			       (port *name-server-port*)
			       verbose debug)
  (make-rpc-server 'rpc-name-server 
    :limit nil :open :listener
    :local-host host :local-port port
    :verbose verbose :debug debug
    ))

(defmethod rpc-do-invoke ((port rpc-name-server-port) op args)
  (when (symbolp op) (setf op (symbol-function op)))
  (if (or (eq op #'do-name-register)
	  (eq op #'do-name-lookup))
      (apply op args)
    (error "Unsupported operation")))


;;; These are top-level function in the client image(s)

(defun set-name-server (&key (host *name-server-host*) 
				  (port *name-server-port*)
				  (timeout *name-server-timeout*)
				  verbose debug)
  (list 
   (setf *name-server-host* host)
   (setf *name-server-port* port)
   (setf *name-server-verbose* verbose)
   (setf *name-server-debug* debug)
   (setf *name-server-timeout* timeout)
   ))

(defun make-name-client (&key (timeout *name-server-timeout*)
			      (verbose *name-server-verbose*)
			      (debug *name-server-debug*)
			      )
  (make-rpc-client 'rpc-datagram-port
    :remote-host *name-server-host*
    :remote-port *name-server-port*
    :message-timeout timeout
    :verbose verbose :debug debug
    ))

(defun close-name-client (port)
  (or (eq t (rpc-close port :final t))
      (rpc-close port :final t :abort t)))

(defun name-register (name &rest values)
  (let ((port (make-name-client)))
    (unwind-protect
	(dotimes (i 5 (values nil :timeout-or-error))
	  (case (ignore-errors (rpc-invoke port 'do-name-register (list* name values)
					   :wait t :error-p nil))
	    (:done (return :done))
	    (:duplicate (return (values nil :duplicate)))
	    ))
      (close-name-client port))))
	    


(defun name-lookup (name)
  (let ((port (make-name-client)))
    (unwind-protect
	(dotimes (i 5 (values nil :timeout))
	  (let* ((m (rpc-invoke port 'do-name-lookup (list name)
				:wait nil)))
	    (rpc-wait port m)
	    (case (rpc-query port m)
	      (:rms-done (return (rpc-get port m)))
	      ((:rms-error :rms-throw) (return (rpc-get port m))))))
      (close-name-client port))))



;;; This is the code that does the work in the server image

(defvar *name-registry* (make-hash-table :test #'equalp))

(defun do-name-register (name &rest values)
  (if* (gethash name *name-registry*)
       then :duplicate
       else
       (setf (gethash name *name-registry*) values)
       :done))

(defun do-name-lookup (name) 
  (values-list (gethash name *name-registry*)))
  



				    


