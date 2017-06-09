;; $Id: deliver.cl,v 1.14 2002/07/09 22:16:57 layer Exp $

(in-package :user)

(defvar *debug* nil)

(let ((*record-source-file-info* nil)
      (*load-source-file-info* nil)
      (excl::*break-on-warnings* t))
  (compile-file "checklinks.cl"))

(generate-application
 "checklinks"
 "checklinks/"
 (append (if* *debug*
	    then '(:streamc :inspect)
	    else nil)
	 '(:list2 :seq2 :trace :excl :sock :process :fileutil :acldns :ssl
	   "checklinks.fasl"))
 :restart-init-function 'main
 #-mswindows
 :application-administration
 #-mswindows ;; Quiet startup (See below for Windows version of this.)
 '(:resource-command-line "-Q")
 :read-init-files nil
 :print-startup-message nil

 :purify nil

 :include-debugger t ;;*debug*
 :include-tpl t ;;*debug*
 :include-compiler nil
 :include-devel-env nil
 :include-ide nil
 :discard-arglists (null *debug*)
 :discard-local-name-info (null *debug*)
 :discard-source-file-info (null *debug*)
 :discard-xref-info (null *debug*)
 
 ;; debugging:
 :verbose *debug*
 :build-input "build.in"
 :build-output "build.out"
 
 ;; dumplisp arguments:
 :ignore-command-line-arguments t
 :suppress-allegro-cl-banner t)

#+mswindows ;; Quiet startup:
(run-shell-command
 (format nil "\"~a\" -o checklinks/checklinks.exe -Q"
	 (translate-logical-pathname "sys:bin;setcmd.exe"))
 :show-window :hide)

(exit 0)
