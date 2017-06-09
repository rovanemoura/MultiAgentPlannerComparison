;; -*- lisp-version: "7.0 [Windows] (Oct 20, 2004 16:56)"; cg: "1.54.2.17"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user)

(define-project :name :disk-usage-example
  :modules (list (make-instance 'module :name "diskuse"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.button :cg.caret
                     :cg.clipboard :cg.clipboard-stack
                     :cg.clipboard.pixmap :cg.common-control :cg.comtab
                     :cg.dialog-item :cg.directory-dialog-os
                     :cg.editable-text :cg.icon :cg.keyboard-shortcuts
                     :cg.lisp-widget :cg.message-dialog
                     :cg.multi-line-editable-text :cg.os-widget
                     :cg.outline :cg.picture-widget :cg.pixmap
                     :cg.pixmap-widget :cg.pixmap.file-io
                     :cg.static-text :cg.string-dialog
                     :cg.text-edit-pane :cg.text-or-combo
                     :cg.text-widget :cg.toggling-widget
                     :cg.utility-dialog)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:debugger :top-level)
  :build-flags '(:allow-runtime-debug :exit-after-build :allow-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-disk-usage-example
  :on-restart 'do-default-restart)

;; End of Project Definition
