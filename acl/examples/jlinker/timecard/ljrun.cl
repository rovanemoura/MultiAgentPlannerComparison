


(in-package :user)

(require :jlinker)
(load "jl-config.cl")

(load (compile-file-if-needed "main.cl"))

;; Uncomment the following to make a preload file from a 
;;  command line argument.
;;(setf  excl:*restart-init-function* #'(lambda () (make-preload) (exit 0)))

;; Uncomment the following to start the application fromthe command line.
;;(setf  excl:*restart-init-function* #'main)

