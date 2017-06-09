;;; copyright (c) 1986-2012 Franz Inc, Oakland, CA
;;; All rights reserved.
;;; 
;;; Permission is granted only to any individual or institution which has
;;; current Allegro CL license(s) to use, copy, or modify this software,
;;; provided any reproduction or distribution of binary versions of this
;;; software are compiled with a licensed Allegro CL, and provided
;;; that this complete copyright and permission notice is maintained, intact,
;;; in all copies and supporting documentation. 
;;; 
;;; Franz Incorporated provides this software "as is" without
;;; express or implied warranty.
;;; 
;;; Restricted Rights Legend
;;; ------------------------
;;; Use, duplication, and disclosure of the software, data and information
;;; contained herein by any agency, department or entity of the U.S.
;;; Government are subject to restrictions of Restricted Rights for
;;; Commercial Software developed at private expense as specified in FAR
;;; 52.227-19 or DOD FAR Supplement 252 52.227-7013 (c) (1) (ii), as
;;; applicable.

(in-package :cg-user)

(defmethod center ((box box))
   (make-position (truncate (/ (box-width box) 2))
     (truncate (/ (box-height box) 2))))

;;; chee   14mar00 call scroll-range and scroll-to on the frame-child
;;;        rather than on the frame, which itself does not scroll
(defmethod scroll-to-center ((window basic-pane))
  (multiple-value-bind (scroll-width scroll-height)
      (scroll-range (frame-child window))
    (scroll-to (frame-child window)
               (make-position (truncate (/ scroll-width 2))
                              (truncate (/ scroll-height 2))))
    ))

;; end of file
