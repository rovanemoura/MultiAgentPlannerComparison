;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2013 Franz Inc, Oakland, CA  All rights reserved.
;;
;; All rights reserved.
;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;; -=End Copyright Notice=-

;; Car Payments Example

(in-package :cg-user)

;; Features the use of scroll-bar widgets

(defparameter *term* 3) ;;; in years
(defparameter *interest* 9.0) ;; percent per year
(defparameter *principal* 20000)  ;; principal

(defun monthly-payments ()
   (if (zerop *interest*)
      (/ *principal* (1+ (* *term* 12)))
      (let ((r (expt (1+ (/ *interest* 100)) -1/12)))
	 (/ (* *principal* (1- r))
	    (1- (expt r (1+ (* 12 *term*))))))))

(defvar *payment* (monthly-payments))

(defvar interest-bar)
(defvar interest-viewer)
(defvar term-bar)
(defvar term-viewer)
(defvar principal-bar)
(defvar principal-viewer)
(defvar payment-viewer)
(defvar total-viewer)

(defun set-interest-viewer (widget new-value old-value)
   (declare (ignore old-value))
   (when (stringp new-value)
      (setq new-value (read-from-string-safely new-value)))
   (when (numberp new-value)
      (setq *interest* new-value)
      (make-fields-consistent widget))
   t)

(defun set-interest-bar (widget new-value old-value)
   (declare (ignore old-value))
   (setq *interest* (/ new-value 10.0))
   (make-fields-consistent widget)
   t)

(defun set-term-widget (widget new-value old-value)
   (declare (ignore old-value))
   (when (stringp new-value)
      (setq new-value (read-from-string-safely new-value)))
   (when (numberp new-value)
      (setq *term* new-value)
      (make-fields-consistent widget))
   t)

(defun set-principal-widget (widget new-value old-value)
   (declare (ignore old-value))
   (when (stringp new-value)
      (setq new-value (read-from-string-safely new-value)))
   (when (numberp new-value)
      (setq *principal* new-value)
      (make-fields-consistent widget))
   t)
   
(defun initialize-car-demo ()
  (setq interest-bar (find-named-object :interest-bar (car-payments)))
  (setq interest-viewer (find-named-object :interest-viewer (car-payments)))
  (setq term-bar (find-named-object :term-bar (car-payments) ))
  (setq term-viewer (find-named-object :term-viewer (car-payments) ))
  (setq principal-bar (find-named-object :principal-bar (car-payments) ))
  (setq principal-viewer (find-named-object :principal-viewer (car-payments) ))
  (setq payment-viewer (find-named-object :payment-viewer (car-payments)))
  (setq total-viewer (find-named-object :total-viewer (car-payments)))
  (select-window (car-payments)))

(defun make-fields-consistent (&optional widget-being-set)
   (setq *payment* (and *term* *interest*
                        *principal* (monthly-payments)))
   (setf (value payment-viewer)
         (and *payment* (format nil "~,2F" *payment*)))
   (setf (value total-viewer)
         (and *payment* *term* *principal*
              (format nil "~,2F"
                (- (* *payment* (1+ (* *term* 12)))
                   *principal*))))
   (initialize-value interest-bar (and *interest* (round (* *interest* 10))))
   (unless (eq interest-viewer widget-being-set)
      (initialize-value interest-viewer (princ-to-string *interest*)))
   (initialize-value term-bar *term*)
   (unless (eq term-viewer widget-being-set)
      (initialize-value term-viewer (princ-to-string *term*)))
   (initialize-value principal-bar *principal* )  
   (unless (eq principal-viewer widget-being-set)
      (initialize-value principal-viewer (princ-to-string *principal*))))

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
;;
(defun run-car-demo-example ()
   (initialize-car-demo)
   (make-fields-consistent)
   (car-payments))

#+run-example (run-car-demo-example)

