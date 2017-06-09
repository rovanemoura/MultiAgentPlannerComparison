
(in-package :user)

;; $Id: run.cl,v 5.0 2004/01/14 18:31:35 layer Exp $

(require :jlinker)
(use-package :net.jlinker)
(require :jlinkent)

;; Disable Java code generation
(gen-output-lang :none)
;;  and load the Java class definitions
(load "bean1gen")

;; Load the method implementations
(load (compile-file-if-needed "bean1.cl"))

(defun run ()
  (format t "~2%;;jlinker-init = ~S~2%"
	  (jlinker-init :lisp-advertises :lisp-file nil :lisp-port 4323
			:verbose t)
	  ))

(setf  excl:*restart-init-function* #'run)

