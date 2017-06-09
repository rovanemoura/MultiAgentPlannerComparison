;; This work in the Public Domain, thereby relinquishing all
;; copyrights. Everyone is free to use, modify, republish, sell or give
;; away this work without prior consent from anybody.
;;
;; This code is provided on an "as is" basis, without warranty of any
;; kind. Use at your own risk! Under no circumstances shall the author(s)
;; or contributor(s) be liable for damages resulting directly or indirectly
;; from the use or non-use of this program.
;;
;; build-lisp-image uses this file to load the "development environment"
;; (the :include-devel-env keyword).
;;
;; If you are using Allegro CL Runtime or Dynamic Runtime, you cannot
;; include any of the following modules:
;;   :xref
;;   :step
;;   :disasm
;;   :prof

(when (or (probe-file "9.0.bootstrap.building")
	  (probe-file "9.1.bootstrap.building"))
  (push :smp-bootstrap *features*))

(require :list2)
(require :seq2)
(require :safeseq)
(require :regexp)
;;(require :streama)
(require :srecord)
(require :tpl-debug)
(require :tpl-proc)
(require :defsys)
(require :foreign)
(require :defftype)
(require :process)
;; The features in MS Windows do not exist for us to implement the SIGIO
;; facility:
#-(or smp-bootstrap mswindows) (require :sigio)
(require :excl)
#-smp-bootstrap
(require :eli)
#-smp-bootstrap
(require :emacs)
#-smp-bootstrap
(require :lze)
#-smp-bootstrap
(require :lep)
#-smp-bootstrap
(require :scm)
#-smp-bootstrap
(require :xref) ;; not allowed in runtime images
;;(require :walker)
#-smp-bootstrap
(require :trace)
#-smp-bootstrap
(require :prof) ;; not allowed in runtime images
#-smp-bootstrap
(require :inspect)
#-smp-bootstrap
(require :disasm) ;; not allowed in runtime images
#-smp-bootstrap
(require :sock)
(require :loop)
(require :regexp)
(require :constructor)
(require :mcombin)
#-smp-bootstrap
(require :uri)

(provide :develenv)
