;; -*- lisp-version: "8.0 [Windows] (Dec 3, 2005 9:41)"; cg: "1.70.2.12"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user)

(define-project :name :color-demo-example
  :modules (list (make-instance 'module :name "colorcod")
                 (make-instance 'module :name "colordef"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg.static-text :cg-dde-utils :cg.base :cg.button
                     :cg.common-control :cg.dialog-item
                     :cg.editable-text :cg.item-list
                     :cg.keyboard-shortcuts :cg.menu :cg.os-widget
                     :cg.radio-button :cg.scroll-bar
                     :cg.scroll-bar-mixin :cg.text-or-combo
                     :cg.text-widget :cg.toggling-widget)
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
  :on-initialization 'run-color-demo-example
  :on-restart 'do-default-restart)

;; End of Project Definition
