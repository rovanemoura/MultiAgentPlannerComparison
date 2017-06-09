;; -*- lisp-version: "7.0.pre-alpha.11 [Windows] (Jun 30, 2003 16:51)"; cg: "1.32"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user (:export) (:use :cl :excl :cg))

(define-project :name :3d-pie-chart-example
  :modules (list (make-instance 'module :name "pie-chart"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.bitmap-pane
                     :cg.bitmap-stream :cg.button :cg.caret
                     :cg.clipboard :cg.clipboard-stack
                     :cg.clipboard.pixmap :cg.color-dialog
                     :cg.combo-box :cg.common-control :cg.comtab
                     :cg.cursor-pixmap :cg.curve :cg.dialog-item
                     :cg.edit-in-place :cg.editable-text
                     :cg.fill-texture :cg.get-position :cg.grid-widget
                     :cg.icon :cg.item-list :cg.keyboard-shortcuts
                     :cg.lisp-edit-pane :cg.lisp-widget :cg.menu
                     :cg.message-dialog :cg.multi-line-editable-text
                     :cg.os-widget :cg.picture-widget :cg.pixmap
                     :cg.pixmap-widget :cg.pixmap.file-io
                     :cg.scroll-bar :cg.scroll-bar-mixin
                     :cg.static-text :cg.string-dialog
                     :cg.text-edit-pane :cg.text-or-combo
                     :cg.text-widget :cg.toggling-widget
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
  :on-initialization 'run-3d-pie-chart-example
  :on-restart 'do-default-restart)

;; End of Project Definition
