;; -*- lisp-version: "7.0.pre-alpha.11 [Windows] (Jun 30, 2003 16:51)"; cg: "1.32"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user (:export) (:use :cl :excl :cg))

(define-project :name :printer-resolution-example
  :modules (list (make-instance 'module :name "printing"))
  :projects nil
  :libraries nil
  :distributed-files '("pixmaps/example.bmp")
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.button :cg.curve
                     :cg.dialog-item :cg.os-widget :cg.pixmap
                     :cg.pixmap.file-io :cg.printing :cg.scaling-stream
                     :cg.toggling-widget)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:debugger :top-level)
  :build-flags '(:allow-runtime-debug :exit-after-build :allow-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-printer-resolution-example
  :on-restart 'do-default-restart)

;; End of Project Definition
