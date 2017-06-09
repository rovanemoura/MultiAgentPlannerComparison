#|
=======================
File: diret-gui-loader.lisp
Author: Vladimir Kulyukin
Description: loader for DIRET 1.0 GUI.
Copyright (c) 1999 

Comments and bugs to vkulyukin@cs.depaul.edu 
======================= 
|# 

(in-package :cl-user)

(eval-when (load compile eval)
  (unless (find :DIRET-PARAMS *features*)
    (load "diret-params"))
  )

(mapc #'(lambda (f)
          (load (concatenate 'string *diret-dir* f)))
  '(
    "diret-control-panel"
    "diret-control-panel-functions"
    "diret-subscription"
    "diret-subscription-functions"
    "diret-retrieval-dialog"
    "diret-retrieval-functions"
    ))

(eval-when (load compile eval)
  (pushnew :diret-gui *features*)
  )

;;; end-of-file


