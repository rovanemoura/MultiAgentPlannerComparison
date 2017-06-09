;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2012 Franz Inc, Oakland, CA  All rights reserved.
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
;; Code for the dialog :coefficient-dialog

(in-package :cg-user)

; added in ch 5 step 8
;; modified in ch 5 step 17
(defclass coefficient-dialog (color-mixin dialog)
    ())

; added ch5 step 50
(defun coefficient-dialog-test-button-on-change (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   (when new-value
      (test-curve (parent widget)))
   (not new-value))

; initially added from ch5 step 52
(defmethod test-curve ((dialog coefficient-dialog))
   (let* ((curve (make-instance 'cycloidal-curve
                   :a-coefficient (value
                                    (find-component :a-coefficient-control 
                                      dialog))
                   :b-coefficient (value
                                    (find-component :b-coefficient-control 
                                      dialog))
                   :c-coefficient (value
                                    (find-component :c-coefficient-control 
                                      dialog))
                   ;; added ch6 step 24
                   :color (current-color dialog)))
          (curve-dialog (owner dialog))
          (doodler (owner curve-dialog)))
      (draw-curve (frame-child doodler) curve)))

;; added ch6 step 30
(defun coefficient-dialog-color-button-on-change (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   (when new-value
      (add-other-color (parent widget)))
   (not new-value))

