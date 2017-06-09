;; -*- lisp-version: "6.0.pre-final.9 [Windows] (Oct 18, 2000 13:49)"; common-graphics: "1.389.2.34"; -*-

(in-package :cg-user)

(defpackage :cg-user (:export))

(define-project :name :interface-builder-tutorial
  :application-type (intern "Standard EXE" (find-package :keyword))
  :modules (list (make-instance 'form-module :name "doodler"
                                :finder-function 'doodler
                                :has-pixmap-file t :create-on-open t)
                 (make-instance 'module :name "util")
                 (make-instance 'module :name "cycloid")
                 (make-instance 'form-module :name "curve-dialog"
                                :finder-function 'curve-dialog
                                :has-pixmap-file nil :create-on-open t)
                 (make-instance 'form-module :name "coefficient-dialog"
                                :finder-function 'coefficient-dialog
                                :has-pixmap-file nil :create-on-open
                                t))
  :projects nil
  :libraries nil
  :package-name :cg-user
  :main-form 'doodler
  :compilation-unit t
  :verbose nil
  :program-name "Allegro.Program"
  :readable-program-name "Allegro Program"
  :runtime-modules '(:cg :drag-and-drop :lisp-widget
                     :multi-picture-button :common-control
                     :edit-in-place :outline :grid :lisp-group-box
                     :header-control :progress-indicator-control
                     :common-status-bar :tab-control :trackbar-control
                     :up-down-control :dde :mci :carets :hotspots
                     :menu-selection :choose-list :directory-list
                     :color-dialog :find-dialog :font-dialog
                     :string-dialog :yes-no-list-dialog
                     :list-view-control :rich-edit :drawable :ole
                     :ole-server :www :aclwin302)
  :help-file-module (make-instance 'build-module :name "")
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:compiler :top-level)
  :build-flags '(:exit-after-build :allow-debug :purify)
  :full-recompile-for-runtime-conditionalizations nil
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'default-init-function)

;; End of Project Definition
