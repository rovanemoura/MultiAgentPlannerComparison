;; -*- lisp-version: "10.0.beta.5 [64-bit Windows] (Apr 9, 2015 23:23)"; cg: "9.0"; -*-

(in-package :cg-user)

(define-project :name :interface-builder-tutorial
  :modules (list (make-instance 'form-module :name "doodler" :finder-function 'doodler
                                :has-pixmap-file t)
                 (make-instance 'module :name "util")
                 (make-instance 'module :name "cycloid")
                 (make-instance 'form-module :name "curve-dialog" :finder-function
                                'curve-dialog :has-pixmap-file nil)
                 (make-instance 'form-module :name "coefficient-dialog" :finder-function
                                'coefficient-dialog :has-pixmap-file nil)
                 (make-instance 'module :name "colorx")
                 (make-instance 'form-module :name "background-palette" :finder-function
                                'background-palette :has-pixmap-file nil))
  :projects nil
  :libraries nil
  :editable-files nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form 'doodler
  :compilation-unit t
  :concatenate-project-fasls nil
  :verbose nil
  :runtime-modules (list :cg-dde-utils :cg.base :cg.bitmap-pane :cg.bitmap-pane.clipboard
                         :cg.bitmap-stream :cg.button :cg.caret :cg.clipboard
                         :cg.clipboard-stack :cg.clipboard.pixmap :cg.color-dialog
                         :cg.common-control :cg.comtab :cg.dialog-item :cg.editable-text
                         :cg.group-box :cg.icon :cg.item-list :cg.keyboard-shortcuts
                         :cg.lisp-widget :cg.message-dialog :cg.multi-line-editable-text
                         :cg.multi-picture-button :cg.multi-picture-button.tooltip
                         :cg.os-widget :cg.picture-widget :cg.pixmap :cg.pixmap-widget
                         :cg.pixmap.file-io :cg.scroll-bar-mixin :cg.static-text
                         :cg.text-edit-pane :cg.text-or-combo :cg.text-widget :cg.timer
                         :cg.toggling-widget :cg.toolbar :cg.tooltip :cg.up-down-control
                         :cg.utility-dialog)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :debugger :top-level)
  :build-flags (list :allow-runtime-debug :exit-after-build)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+cx +t \"Initializing\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :build-number 13
  :run-with-console nil
  :project-file-version-info nil
  :on-initialization 'default-init-function
  :default-error-handler-for-delivery 'report-unexpected-error-and-exit
  :on-restart 'do-default-restart)

;; End of Project Definition
