;; -*- lisp-version: "8.0.beta [Windows] (Oct 4, 2005 10:48)"; cg: "1.70.2.1"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user)

(define-project :name :split-bar-example
  :modules (list (make-instance 'module :name "panes-with-split-bar"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.bitmap-stream
                     :cg.curve)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:top-level :debugger)
  :build-flags '(:allow-runtime-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+M +t \"Console for Debugging\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-split-bar-example
  :on-restart 'do-default-restart)

;; End of Project Definition
