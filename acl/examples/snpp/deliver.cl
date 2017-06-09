;; $Id: deliver.cl,v 1.5 2002/07/09 20:52:49 layer Exp $

(in-package :user)

(defvar *purify* nil) ;; set in Makefile
(defvar *runtime-type* nil) ;; set in Makefile

(let ((*record-source-file-info* nil)
      (*load-source-file-info* nil)
      (excl::*break-on-warnings* t))
  (compile-file "snpp.cl"))

(generate-application
 "snpp" "snpp/" '(;; include these because we're not sure if they're used:
		  :list2 :seq2
		  ;; include these because we know we use them:
		  :sock :process "snpp.fasl")
 :restart-init-function 'main
 :application-administration '(:resource-command-line
			       ;; Quiet startup:
			       "-Q")

 :read-init-files nil
 :print-startup-message nil

 :purify *purify*
 :runtime *runtime-type*

 :include-compiler nil
 :include-devel-env nil
 :include-debugger nil
 :include-tpl nil
 :include-ide nil
 :discard-arglists t
 :discard-local-name-info t
 :discard-source-file-info t
 :discard-xref-info t
 
 ;; debugging:
 :build-input "build.in"
 :build-output "build.out"
 
 ;; dumplisp arguments:
 :ignore-command-line-arguments t
 :suppress-allegro-cl-banner t)
