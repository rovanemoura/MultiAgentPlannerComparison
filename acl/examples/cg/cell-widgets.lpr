;; -*- lisp-version: "8.0.beta [Windows] (Oct 4, 2005 10:48)"; cg: "1.70.2.2"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user)

(define-project :name :cell-widgets-example
  :modules (list (make-instance 'module :name "cell-widgets"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.bitmap-stream :cg.caret
                     :cg.clipboard :cg.clipboard-stack
                     :cg.clipboard.pixmap :cg.combo-box
                     :cg.common-control :cg.comtab :cg.cursor-pixmap
                     :cg.dialog-item :cg.edit-in-place
                     :cg.editable-text :cg.fill-texture
                     :cg.get-position :cg.grid-widget :cg.item-list
                     :cg.keyboard-shortcuts :cg.lisp-edit-pane
                     :cg.lisp-widget :cg.menu
                     :cg.multi-line-editable-text :cg.os-widget
                     :cg.pixmap :cg.pixmap-widget :cg.scroll-bar
                     :cg.scroll-bar-mixin :cg.text-edit-pane
                     :cg.text-or-combo :cg.text-widget)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:top-level :debugger)
  :build-flags '(:allow-build-debug :allow-runtime-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-cell-widgets-example
  :on-restart 'do-default-restart)

;; End of Project Definition
