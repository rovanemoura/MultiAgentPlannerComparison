
;; $Id: tryjtol.cl,v 5.0 2004/01/14 18:31:35 layer Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :jlinker)
  (use-package :net.jlinker))

(defun advertise (&optional (port 4326) (host "localhost"))
  (jlinker-init :lisp-advertises 
		:lisp-host host 
		:lisp-port port
		:lisp-file nil
		:verbose t
		))

(defun test-work (num)
  (+ num 17))

