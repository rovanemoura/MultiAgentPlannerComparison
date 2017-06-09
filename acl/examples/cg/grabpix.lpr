;; -*- lisp-version: "7.0.pre-alpha.11 [Windows] (Jun 30, 2003 16:51)"; cg: "1.32"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user (:export) (:use :cl :excl :cg))

(define-project :name :get-screen-pixmap-example
  :modules (list (make-instance 'module :name "grabpix"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :project-package-name :cg-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.bitmap-pane
                     :cg.bitmap-stream :cg.button :cg.dialog-item
                     :cg.file-dialog :cg.get-pixmap :cg.get-position
                     :cg.os-widget :cg.palette :cg.pixmap
                     :cg.pixmap.file-io :cg.toggling-widget :cg.toolbar
                     :cg.utility-dialog)
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
  :on-initialization 'run-get-screen-pixmap-example
  :on-restart 'do-default-restart)

;; End of Project Definition
