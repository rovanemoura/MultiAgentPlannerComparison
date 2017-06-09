(in-package :cg-user)

(defpackage :common-graphics-user (:export) (:use :cl :excl :cg))

(define-project :name :basic-window-example
  :modules (list (make-instance 'module :name "basic-window"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg.base)
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :on-initialization 'run-basic-window-example
  :on-restart 'do-default-restart)

;; End of Project Definition

