
(in-package :user)

;; $Id: prep.cl,v 5.0 2004/01/14 18:31:35 layer Exp $

(require :jlinker)
(use-package :net.jlinker)
(require :jlinkent)

(gen-output-lang :java)
;;(compile-file "test-gen.cl")
(load (compile-file "bean1gen.cl"))
(compile-file "bean1.cl")

(defun run ()
  (exit 1))

(setf  excl:*restart-init-function* #'run)

