;;
;; copyright (c) 1996-2000 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2004 Franz Inc, Oakland, CA - All rights reserved.
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

;; $Id: eh.cl,v 1.5 2005/08/03 05:07:56 layer Exp $

;; Extensions to Lisp gtk+ interface which allow multi-processing integration.
;;
;; gtk:gtk-main is meant to be a substitute for gtk:gtk_main.
;; gtk:gtk-main-quit is meant to be a substitute for gtk:gtk_main_quit.
;;
;; gtk:gtk-timeout specifies timeouts for gtk.  This function is setf-able.
;; Its use is recommended when using gtk_timeout* functions in gtk.  Remember
;; to (setf (gtk:gtk-timeout) nil) when timers are no longer needed (i.e.,
;; all gtk_timeout objects have been removed).
;;
;; gtk:gtk-timeout values are nil, meaning no timeout, or n, where timeout
;; occurs every (/ n 1000) seconds.

#+use-in-case-mode
(excl::in-case-mode :ww)

(defpackage :gtk
  #+use-in-case-mode (:mode :independent)
  (:use :common-lisp)
  (:export #:gtk-events-pending
	   #:gtk-main
	   #:gtk-main-quit
	   #:gtk-timeout))

(in-package :gtk)

(defun gtk-events-pending ()
  (not (eq gtk:NULL (gtk:gtk_events_pending))))

;; fixed for bug12382
(ff:def-foreign-call (XConnectionNumber "XConnectionNumber") ((x (* :void)))
  :returning :int)

(let ((.gtk-main-counter. 0)
      (event-poll-fd (ff:get-entry-point "gdk_display"))
      (timeout nil))

  (defun gtk-main ()
    (let ((gtk-main-counter (incf .gtk-main-counter.)))
      (loop
	(if* (gtk-events-pending)
	   then (gtk:gtk_main_iteration_do gtk:NULL)
	 elseif (<= gtk-main-counter .gtk-main-counter.)
	   then (mp:wait-for-input-available
		 (XConnectionNumber
		  (ff:fslot-value-typed
		   '(* :void) :c
		   event-poll-fd))
		 :timeout timeout)
	   else (return-from gtk-main)))))

  (defun gtk-main-quit ()
    (decf .gtk-main-counter.))

  ;; Special Case for timers.
  ;;
  ;; A gtk timer's timeout does not signal an X event, so unless we use a
  ;; timeout in Lisp, the gtk timer's timeout is not recognized until an
  ;; X-related event is handled.
  ;;

  (defun gtk-timeout ()
    (when (numberp timeout)
      (* timeout 1000)))

  (defun (setf gtk-timeout) (val)
    (if* (or (null val)
	     (zerop val))
       then (setq timeout nil)
       else (setq timeout (/ val 1000)))
    val)
  )
