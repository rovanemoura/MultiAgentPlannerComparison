;; -*- lisp-version: "7.0.pre-alpha.11 [Windows] (Jun 30, 2003 16:51)"; cg: "1.32"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user (:export) (:use :cl :excl :cg))

(define-project :name :emerge-example
  :modules (list (make-instance 'module :name "emerge"))
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
                     :cg.clipboard.pixmap :cg.combo-box
                     :cg.common-control :cg.comtab :cg.cursor-pixmap
                     :cg.dialog-item :cg.edit-in-place
                     :cg.editable-text :cg.file-dialog :cg.fill-texture
                     :cg.get-position :cg.grid-widget :cg.icon
                     :cg.item-list :cg.keyboard-shortcuts
                     :cg.lisp-edit-pane :cg.lisp-widget :cg.menu
                     :cg.multi-line-editable-text
                     :cg.multi-picture-button :cg.os-widget :cg.palette
                     :cg.picture-widget :cg.pixmap :cg.pixmap-widget
                     :cg.pixmap.file-io :cg.scroll-bar
                     :cg.scroll-bar-mixin :cg.static-text
                     :cg.text-edit-pane :cg.text-or-combo
                     :cg.text-widget :cg.timer :cg.toggling-widget
                     :cg.toolbar :cg.utility-dialog)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:debugger :top-level)
  :build-flags '(:allow-build-debug :allow-runtime-debug
                 :suppress-systray-icon :exit-after-build :allow-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cn +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-emerge-example
  :on-restart 'do-default-restart)

;; End of Project Definition
