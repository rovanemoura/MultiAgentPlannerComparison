#|
========================
File: compile-diret.lisp
Author: Vladimir Kulyukin 
Description: compilation utility for DIRET 1.0
Copyright (c) 1999 

Comments and bugs to vkulyukin@cs.depaul.edu 
========================
|#

(in-package :cl-user)

(defun compile-diret ()
  (compile-file "diret.lisp")
  (load "diret")
  (ensure-support)
  (ensure-dmapvs)
  (ensure-diret-proper))

(defparameter *support* '("utils" "deftable"))

(defparameter *dmapvs* 
  '("frame-manager"
    "dmap"
    "dmaprr"
    "dmapvs"))

(defparameter *diret-proper*
  '("diret"
    "diret-server"
    "diret-client"
    "diret-control-panel"
    "diret-control-panel-functions"
    "diret-subscription"
    "diret-subscription-functions"
    "diret-retrieval-dialog"
    "diret-retrieval-functions"))

(defun compile-and-load (dir f)
  (let ((fp (concatenate 'string dir f)))
    (compile-file fp)
    (load fp)))

(defun ensure-support ()
  (mapc #'(lambda (f)
	    (compile-and-load *diret-dir* f))
	*support*))

(defun ensure-dmapvs ()
  (mapc #'(lambda (f)
	    (compile-and-load *diret-dir* f))
	*dmapvs*))

(defun ensure-diret-proper ()
  (mapc #'(lambda (f)
	    (compile-and-load *diret-dir* f))
	*diret-proper*))

;;; end-of-file