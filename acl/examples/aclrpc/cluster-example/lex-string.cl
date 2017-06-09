;; copyright (c) 2005-2015 Franz Inc, Oakland, CA - All rights reserved.
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
;;
;; $Id: lex-string.cl,v 1.2 2007/04/17 21:51:29 layer Exp $

(defpackage :utils (:export lex-string))
(in-package :utils)

;; LEX-STRING separates a STRING into tokens at WHITESPACE-CHARS (by default, space
;; tab, newpage, backspace, return, linefeed and newline).

(defconstant +whitespace-characters+
  '(#\space #\tab #\page #\newline #\backspace #\return #\linefeed #\no-break_space)
  "The whitespace characters")

(defun lex-string (string &optional (whitespace-chars +whitespace-characters+))
  "Separates a string at whitespace and returns a list of strings"
  (assert (and (stringp string) (every #'characterp whitespace-chars)))
  (flet ((whitespace-char? (char) (find char whitespace-chars :test #'char=)))
    (let ((tokens nil)
	  token-end)
      (do ((token-start
	    (position-if-not #'whitespace-char? string) 
	    (when token-end
	      (position-if-not #'whitespace-char? string :start (1+ token-end)))))
	  ((null token-start) (nreverse tokens))
	(setq token-end
	      (when token-start
		(position-if #'whitespace-char? string :start token-start)))
	(push (subseq string token-start token-end) tokens)))))
