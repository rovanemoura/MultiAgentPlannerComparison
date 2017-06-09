;; copyright (c) 2002-2015 Franz Inc, Oakland, CA - All rights reserved.
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
;; $Id: update2.cl,v 1.8 2007/04/17 21:27:32 layer Exp $

(setq excl::*restart-init-function* nil) ;; prevent IDE from starting

(with-open-file (s
		 #+allegro-cl-trial "TRIAL"
		 #-allegro-cl-trial "NON-TRIAL"
		 :direction :output :if-exists :supersede)
  (format s "t~%"))

(force-output)
(exit 0 :quiet t)
