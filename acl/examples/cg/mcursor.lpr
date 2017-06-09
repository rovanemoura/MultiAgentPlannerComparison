;; -*- lisp-version: "8.0.beta [Linux (x86)] (Dec 6, 2005 13:32)"; cg: "1.70.2.13"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user)

(define-project :name :mouse-cursor-example
  :modules (list (make-instance 'module :name "mcursor"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.bitmap-stream
                     :cg.cursor-pixmap :cg.fill-texture :cg.get-pixmap
                     :cg.pixmap)
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
  :on-initialization 'run-mouse-cursor-example
  :on-restart 'do-default-restart)

;; End of Project Definition
