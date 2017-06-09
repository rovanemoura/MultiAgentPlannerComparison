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
;; Code for the dialog :background-palette

(in-package :cg-user)

;; added ch6 step 39
(defclass background-palette (color-mixin dialog)
    ())

;; added ch6 step 46
;;; chee   21jan99 bug7513 returning (not new-value) here to unpress every
;;;        button press is inconsistent with setting the initial value of
;;;        the multipic to the initial background color (white); see the
;;;        initialize-instance method just below
(defun background-palette-color-list-on-change (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   ;; Do the action only when a button is being pressed (not unpressed)
   (when new-value
     (change-background-color (parent widget)))
   t) ;; <bug7513>

;; added ch6 step 47
;;; chee   10sep99 bug8144 had to change a call to parent here
;;;        to call owner instead, now that parent of a pop-up
;;;        dialog returns the screen for general consistency
(defmethod change-background-color ((palette background-palette))
   (let ((color (current-color palette))
         (doodler (owner palette)))
      (setf (background-color (frame-child doodler)) color)
      (erase-window doodler)))

;; added ch6 step51
(defun background-palette-color-button-on-change (widget new-value old-value)
   (declare (ignore-if-unused widget new-value old-value))
   (when new-value 
      (add-other-color (parent widget)))
   (not new-value))

;; added ch6 step 52
;;; chee   10sep99 bug8144 had to change a call to parent here
;;;        to call owner instead, now that parent of a pop-up
;;;        dialog returns the screen for general consistency
(defmethod initialize-instance :after ((palette background-palette)
                                       &rest initargs)
   (declare (ignore initargs))
   (let* ((pane (frame-child (owner palette)))
          (color-name (find-color-name palette
                        (or (background-color pane)
                            (default-background-color pane)))))
      (initialize-value (find-component :color-list palette)
        (list color-name))))
