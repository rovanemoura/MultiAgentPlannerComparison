
(in-package :user)

;; $Id: run.cl,v 5.0 2004/01/14 18:31:35 layer Exp $

(require :jlinker)
(use-package :net.jlinker)
(require :jlinkent)

(load (compile-file-if-needed "user1.cl"))
(load (compile-file-if-needed "user2.cl"))

(defun run ()
  (jlinker-listen :init-args 
		  (list :lisp-file nil :lisp-port 4321 :verbose t)))

(setf  excl:*restart-init-function* #'run)

