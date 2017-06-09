;; -*- lisp-version: "8.1 [Windows] (Jul 23, 2007 16:19)"; cg: "1.112"; -*-

(in-package :cg-user)

(define-project :name :google-maps-example
  :modules (list (make-instance 'module :name "google-maps"))
  :projects nil
  :libraries nil
  :distributed-files nil
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules (list :cg-dde-utils :cg.base :cg.bitmap-stream
                         :cg.button :cg.clipboard :cg.clipboard-stack
                         :cg.clipboard.pixmap :cg.dialog-item
                         :cg.editable-text :cg.html-widget :cg.icon
                         :cg.lisp-widget :cg.message-dialog
                         :cg.os-widget :cg.picture-widget :cg.pixmap
                         :cg.pixmap-widget :cg.pixmap.file-io
                         :cg.scrolling-static-text :cg.static-text
                         :cg.text-or-combo :cg.text-widget
                         :cg.toggling-widget :cg.utility-dialog)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :debugger)
  :build-flags (list :allow-runtime-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+cx +M +t \"Initializing\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'run-google-maps-example
  :on-restart 'do-default-restart)

;; End of Project Definition
