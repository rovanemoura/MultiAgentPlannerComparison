;; -*- lisp-version: "8.1.pre-beta.2 [Windows] (Apr 21, 2006 14:03)"; cg: "1.86"; -*-

(in-package :cg-user)

(define-project :name :chart-widget-example
  :modules (list (make-instance 'module :name "chart-widget"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules (list :cg-dde-utils :cg.base :cg.button
                         :cg.chart-or-plot :cg.chart-widget
                         :cg.check-box :cg.dialog-item :cg.fill-texture
                         :cg.lisp-widget :cg.os-widget :cg.pixmap
                         :cg.toggling-widget)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :debugger :top-level)
  :build-flags (list :allow-runtime-debug :exit-after-build)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-chart-widget-example
  :on-restart 'do-default-restart)

;; End of Project Definition
