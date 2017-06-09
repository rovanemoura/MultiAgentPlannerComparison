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
;; $Id: top-level.cl,v 1.3 2009/04/20 18:41:44 layer Exp $

(in-package :user)

;;; Some top-level functions

(defun download (&optional max)
  (gene2pubmed-records :remote? t :reload? t :max-records max))

(defun run (&key (r nil) (max 100000) (batch 1000) (reload t) remote host
		 (peep-freq *peep-freq*)

		 ;; this arg must be a closure because it must run
		 ;;      after gene2pubmed-records
		 (gen-ids #'(lambda () (human-genes-with-pmids)))
		 )

  ;; run one or more partitions on this one processor

  ;;(dribble "para4.log")
  (gene2pubmed-records :remote? remote :reload? reload :max-records max)
  (calculate-all-entrez-gene-distances
   (funcall gen-ids)
   :region r :batch batch :host host :peep-freq peep-freq
   )
  ;;(dribble)
  )


(defparameter *remote-hosts*
  '(
    ;; (hostname start-lisp-command [image-name-or-nil])
    ;;  start-lisp-command must be a script that starts lisp in a directory where
    ;;        this compiled program file is visible
    ;;        and also the gene2pubmed data file

    ;; In this example, we run each host in a separate directory because
    ;; the hosts are not all the same architecture and thus require
    ;; separate fasl files.

    ("foray" "~/tmp/cluster-ex-foray/run")
    ("moe"   "~/tmp/cluster-ex-moe/run")
    ("edge"  "~/tmp/cluster-ex-edge/run")
    ))

(defun prun (&key (max 100000) (batch 1000) (reload t) host (peep-freq *peep-freq*)
		  (remote-hosts *remote-hosts*)
		  (gen-ids #'(lambda () (human-genes-with-pmids))))

  ;; run on several hosts in parallel

  (gene2pubmed-records :reload? reload :max-records max)
  (calculate-all-entrez-gene-distances
   (funcall gen-ids)
   :peep-freq peep-freq
   :region (length *remote-hosts*)
   :remote-hosts remote-hosts
   :max-records max :batch batch :host host
   ))




(setf *restart-init-function* #'(lambda () (run-one-sub-task) (exit)))




