(in-package :user)

(eval-when (compile load eval)
  (require :jlinker)
  (use-package :net.jlinker)
  (defpackage :net.jlinker (:nicknames :jl)))

;; Make sure the required files are locally visible
;;                    [Allegro directory]/jlinker/jlinker.jar


(def-java-class (string-tokenizer "java.util.StringTokenizer") ()
  () () ())


(def-java-constructor make-tokenizer 
  (string-tokenizer  "java.lang.String" "java.lang.String"))

(defun new-tokenizer (&optional (string "A B C D ")
                                (delimiters " "))
  (make-tokenizer string delimiters))

(def-java-method (next-token "nextToken") (string-tokenizer))
(def-java-method (count-tokens "countTokens") (string-tokenizer))

(defun connect (&optional (port 7171))
  (or (jlinker-query)
      (jlinker-init :lisp-advertises
		    :lisp-file nil
		    :lisp-port port
		    )))

(defun run-tokenizer (&optional (string "A B C D ")
                                (delimiters " "))

  (connect)
  
  (let ((inst (new-tokenizer string delimiters))
        res)
    
    (dotimes (i (count-tokens inst))
      (push (next-token inst)
            res))
    
    (values inst (reverse res))))


