
;; $Id: run-slave.cl,v 5.0 2004/01/14 18:31:35 layer Exp $

(in-package :user)

(load (compile-file-if-needed "t-local"))
(load "jl-config")


(defun begin (&optional (timeout 15))
  (mp:start-scheduler)
  (format t "~3%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
  (format t "~%")
  (format t "(run) to run Java slave~%")
  (format t "~%")
  (format t " In 15 seconds, we will call (run) by default.~%")
  (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
  (format t "~%")
  (format t "~%")
  (let ((*jlinker-verbose* t))
    (eval
     (mp:with-timeout 
      (timeout '(run))
      (if (y-or-n-p "Say Y to run Java slave: ")
	  '(run)
	nil)))))

(setf excl:*restart-init-function* #'begin)
