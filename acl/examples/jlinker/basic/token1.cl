(in-package :user)

(eval-when (compile load eval)
  (require :jlinker)
  (use-package :net.jlinker)
  (defpackage :net.jlinker (:nicknames :jl)))

;; Make sure the required files are locally visible
;; customized copy of [Allegro directory]/jlinker/jl-config.cl
;;                    [Allegro directory]/jlinker/jlinker.jar

(load "jl-config")



(defun new-tokenizer (&optional (string "A B C D ")
                                (delimiters " "))
  (jnew (jconstructor "java.util.StringTokenizer" 
		      "java.lang.String" "java.lang.String") 
        string delimiters))

(defun next-token (inst)
  (jcall (jmethod "java.util.StringTokenizer" "nextToken")
	 inst))

(defun run-tokenizer (&optional (string "A B C D ")
                                (delimiters " "))

  (or (jlinker-query) (jlinker-init))
  
  (let ((inst (new-tokenizer string delimiters))
        res)
    
    (dotimes (i (jcall (jmethod "java.util.StringTokenizer" "countTokens") 
                       inst))
      (push (next-token inst)
            res))
    
    (values inst (reverse res))))


