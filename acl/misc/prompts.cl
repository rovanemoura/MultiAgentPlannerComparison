;;; $Header: /repo/cvs.copy/cl/release/prompts.cl,v 1.5 1996/10/03 17:14:33 layer Exp $
;;;
;;; Interesting top level prompts.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following makes the prompt look as it did in Allegro CL versions
;;; previous to 4.1.

(setq tpl:*prompt* "~&~
~@[[Current process: ~a]~%~]~
~@[[Current process focus: ~a]~%~]~
~:[~
     ~:[~
        ~2*~
     ~;~
        [~:*~d~:[~;c~]~:[~;i~]] ~
     ~]~
~;~
     [step] ~3*~
~]~
<cl> ~2*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following prompt example makes the prompt sensitive to
;;; *print-alternate-package-name*, when printing the package name portion of the prompt.
;;; In every other respect, the prompt prints the same as the default prompt.

#+(version>= 4 1)
(defun user::my-package-name-printer (stream arg colon atsign)
  (declare (ignore colon atsign))
  (format stream "~a"
          (or (and *print-alternate-package-name* ;; [rfe13337]
		   (car (package-nicknames *package*)))
	      (package-name *package*))))


#+(version>= 4 1)
(setq tpl:*prompt* "~&~
~@[[Current process: ~a]~%~]~
~@[[Current process focus: ~a]~%~]~
~:[~
     ~:[~
        ~2*~
     ~;~
        [~:*~d~:[~;c~]~:[~;i~]] ~
     ~]~
~;~
     [step] ~3*~
~]~
~/user::my-package-name-printer/(~d): ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following is equivalent to the default prompt, except is does it
;;; with a function rather than a format string.

(setq tpl:*prompt*
  #'(lambda (stream process focus stepping break-level continuable
	     inspecting package-name command-number)
      (fresh-line stream)
      (when process
	(format stream "[Current process: ~a]~%" process-name))
      (when focus
	(format stream "[Current process focus: ~a]~%" focus))
      (if* stepping
	 then (format stream "[step] ")
	 else (when break-level
		(princ "[" stream)
		(princ break-level stream)
		(when continuable (princ "c" stream))
		(when inspecting (princ "i" stream))
		(princ "] " stream)))
      (princ package-name stream)
      (format stream "(~d): " command-number)))
