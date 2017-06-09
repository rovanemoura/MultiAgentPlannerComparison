;; $Id: load.cl,v 1.2 2009/04/20 18:41:44 layer Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :aclrpc)
  (use-package :net.rpc)
  (require :inflate)
  (load (compile-file-if-needed "lex-string.cl"))
  (load (compile-file-if-needed "task-level.cl"))
  (load (compile-file-if-needed "parallel5.cl"))
  (load (compile-file-if-needed "top-level.cl"))
  )

