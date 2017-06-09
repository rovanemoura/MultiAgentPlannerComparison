;; -*- lisp-version: "7.0.pre-final.5 [Windows] (Jul 6, 2004 22:04)"; cg: "1.54.2.5"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user)

(define-project :name :smiley-example
  :modules (list (make-instance 'module :name "smiley"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.bitmap-pane
                     :cg.bitmap-stream :cg.curve :cg.pixmap)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name
                                   "z:\\cheetham\\icons\\processgenius.ico")
  :include-flags '(:top-level :debugger)
  :build-flags '(:allow-runtime-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-smiley-example
  :on-restart 'do-default-restart)

;; End of Project Definition
