;; -*- lisp-version: "8.2.pre-beta.5 [Windows] (Dec 20, 2007 18:51)"; cg: "1.120"; -*-

(in-package :cg-user)

(define-project :name :fonts-example
  :modules (list (make-instance 'module :name "fonts"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules (list :cg-dde-utils :cg.base :cg.bitmap-stream)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :top-level :debugger)
  :build-flags (list :allow-runtime-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :build-number 1
  :on-initialization 'run-fonts-example
  :on-restart 'do-default-restart)

;; End of Project Definition
