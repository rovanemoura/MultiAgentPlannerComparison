#|
=======================
File: diret-gui-loader.lisp
Author: Vladimir Kulyukin
Description: loader for DMAPVS.
Copyright (c) 1999 

Comments and bugs to vkulyukin@cs.depaul.edu
======================= 
|# 

(in-package :cl-user)

(mapc #'(lambda (f)
          (load (concatenate 'string *diret-dir* f)))
  '(
    "utils"
    "deftable"
    ))

(mapc #'(lambda (f)
          (load (concatenate 'string *diret-dir* f)))
  '(
    "frame-manager"
    "dmap"
    "dmaprr"
    "dmapvs"
    ))

;;; end-of-file	  
