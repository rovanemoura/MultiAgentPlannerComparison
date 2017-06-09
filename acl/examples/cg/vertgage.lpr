;; -*- lisp-version: "7.0.pre-alpha.11 [Windows] (Jun 30, 2003 16:51)"; cg: "1.32"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user (:export) (:use :cl :excl :cg))

(define-project :name :vertical-gauge-example
  :modules (list (make-instance 'module :name "vertgage"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.bitmap-pane
                     :cg.bitmap-stream :cg.button :cg.caret
                     :cg.check-box :cg.clipboard :cg.clipboard-stack
                     :cg.clipboard.pixmap :cg.color-dialog
                     :cg.combo-box :cg.common-control :cg.comtab
                     :cg.cursor-pixmap :cg.dialog-item :cg.drawable
                     :cg.dropping-outline :cg.edit-in-place
                     :cg.editable-text :cg.file-dialog :cg.fill-texture
                     :cg.font-dialog :cg.get-position :cg.grid-widget
                     :cg.group-box :cg.header-control :cg.icon
                     :cg.item-list :cg.keyboard-shortcuts
                     :cg.lisp-edit-pane :cg.lisp-widget :cg.list-view
                     :cg.menu :cg.message-dialog
                     :cg.multi-line-editable-text
                     :cg.multi-line-lisp-text :cg.multi-picture-button
                     :cg.os-widget :cg.outline :cg.picture-widget
                     :cg.pixmap :cg.pixmap-widget :cg.pixmap.file-io
                     :cg.printing :cg.progress-indicator :cg.property
                     :cg.radio-button :cg.rich-edit :cg.rich-edit-pane
                     :cg.rich-edit-pane.clipboard
                     :cg.rich-edit-pane.printing :cg.scaling-stream
                     :cg.scroll-bar :cg.scroll-bar-mixin
                     :cg.selected-object :cg.static-text :cg.status-bar
                     :cg.tab-control :cg.text-edit-pane
                     :cg.text-or-combo :cg.text-widget :cg.timer
                     :cg.toggling-widget :cg.toolbar :cg.trackbar
                     :cg.up-down-control :cg.utility-dialog)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:top-level :debugger)
  :build-flags '(:allow-runtime-debug :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-vertical-gauge-example
  :on-restart 'do-default-restart)

;; End of Project Definition
