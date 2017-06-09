;; copyright (c) 1985 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2001-2013 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

;; Bidirectional (dual channel) rot13 encapsulator

(in-package :user)

(eval-when (compile)
  (require :iodefs))

;; ==============
;; Example: rot13 (bidirectional):

(def-stream-class rot13-bidirectional-stream (bidirectional-character-encapsulating-stream)
  ())

(defmethod device-read ((stream rot13-bidirectional-stream) buffer start end blocking)
  (when (and (null buffer) (not (eq start end)))
    (with-stream-class (rot13-bidirectional-stream)
      (setq buffer (sm excl::buffer stream)
	    end (length buffer))))
  (with-stream-class (rot13-bidirectional-stream stream)
    (let ((base-stream (sm excl::input-handle stream)))
      (if (and base-stream (>= end start))
	  (let ((res 0))
	    (when (or blocking (stream-listen base-stream))
	      (unless blocking (setq res -3))
	      (when (> end start)
		(setq res (read-vector buffer base-stream :start start :end end))
		(when (> res 0)
		  (loop for i from start upto (+ start res)
		      do (setf (char buffer i)
			   (rotate-char (char buffer i)))))))
	    (setf (sm excl::buffer-ptr stream) res))
	0))))

(defmethod device-write ((stream rot13-bidirectional-stream) buffer start end blocking)
  (let ((flush (eq buffer :flush))
	(res 0))
    (when (and (or flush (null buffer)) (not (eq start end)))
      (with-stream-class (rot13-bidirectional-stream)
	(setq buffer (sm excl::out-buffer stream))))
    (with-stream-class (rot13-bidirectional-stream stream)
      (let ((base-stream (sm excl::output-handle stream)))
	(when (and base-stream (> end start))
	  (setq res (- end start))
	  (loop for i from start upto end
	      do (setf (char buffer i)
		   (rotate-char (char buffer i))))
	  (write-string buffer base-stream :start start :end end)
	  (when flush
	    (if blocking (finish-output base-stream) (force-output base-stream))))))
    res))

(defun rotate-char (char)
  (unless (alpha-char-p char)
    (return-from rotate-char char))
  (let ((code (char-code char)))
    (cond ((<= #.(char-code #\A) code #.(char-code #\Z))
	   (code-char
	    (+ #.(char-code #\A)
	       (mod (+ 13 (- code #.(char-code #\A))) 26))))
	  (t
	   ;; assume ascii; must be lowercase letter
	   (code-char
	    (+ #.(char-code #\a)
	       (mod (+ 13 (- code #.(char-code #\a))) 26)))))))
