;; -*- lisp-version: "7.0.pre-alpha.11 [Windows] (Jun 30, 2003 16:51)"; cg: "1.32"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user (:export) (:use :cl :excl :cg))

(define-project :name :car-demo-example
  :modules (list (make-instance 'module :name "cardlg")
                 (make-instance 'module :name "car"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :project-package-name :cg-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.common-control
                     :cg.dialog-item :cg.editable-text :cg.os-widget
                     :cg.scroll-bar :cg.scroll-bar-mixin
                     :cg.static-text :cg.text-or-combo :cg.text-widget)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:top-level :debugger)
  :build-flags '(:allow-runtime-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-car-demo-example
  :on-restart 'do-default-restart)

;; End of Project Definition
