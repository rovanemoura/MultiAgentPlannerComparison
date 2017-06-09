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

;; A test of copy-pixels-to-stream-with-mask.
;; Bitblits an octagon pixmap in different colors.

(in-package :cg-user)

(defparameter *octagon-size* 32)

(defparameter *octagon-colors* `#(,black ,cyan))

(defparameter *octagon-color-choices* `#(,red ,green ,blue ,cyan ,yellow
					 ,gray))

(defparameter *octagon-pixmap*
  
  ;; The "1" bits here will use the color at index 1 in the color vector
  ;; of the associated texture-info above.  We change the color by
  ;; changing the entry at position 1 in this color vector.  The layer of
  ;; zeros just around the ones will draw the black border (color index
  ;; 0), and the rest of the zeros will not draw at all because they are
  ;; defined as transparent in the mask bitmap below.
  (make-instance 'pixmap
    :bits-per-pixel 4
    :colors *octagon-colors*
    :contents
    '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
      (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0) 
      (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0) 
      (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0) 
      (0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0) 
      (0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0) 
      (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0) 
      (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
      (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0) 
      (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0) 
      (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0) 
      (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0) 
      (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0) 
      (0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0) 
      (0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0) 
      (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0) 
      (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0) 
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

  
    ;; This mask bitmap defines the areas of the image that are to be
    ;; transparent.  That is, the "1" pixels here will not overwrite
    ;; whatever was on the screen already.
    :mask-contents
    '(#*11111111110000000000000111111111 
      #*11111111100000000000000011111111 
      #*11111111000000000000000001111111 
      #*11111110000000000000000000111111 
      #*11111100000000000000000000011111 
      #*11111000000000000000000000001111 
      #*11110000000000000000000000000111 
      #*11100000000000000000000000000011 
      #*11000000000000000000000000000001 
      #*10000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*00000000000000000000000000000000 
      #*10000000000000000000000000000000 
      #*11000000000000000000000000000001 
      #*11100000000000000000000000000011 
      #*11110000000000000000000000000111 
      #*11111000000000000000000000001111 
      #*11111100000000000000000000011111 
      #*11111110000000000000000000111111 
      #*11111111000000000000000001111111 
      #*11111111100000000000000011111111)))

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
;;
(defun run-octagons-example ()
  (let* ((window (make-window :octagon-pixmap
                   :class 'bitmap-window 
                   :owner (development-main-window *system*)
                   :title "Octafun"
                   :scrollbars nil
                   :resizable nil
                   :interior (make-box 500 200 700 400)))
         x y)
    
    ;; Draw some background lines.
    (with-foreground-color (window gray)
      (dotimes (j 40)
        (draw-line (frame-child window)
                   (make-position 0 (* j 5))
                   (make-position 200 (* j 5)))))
    
    ;; Draw a bunch of octagons in different colors.
    (dotimes (j 30)
      (setq x (random (- 200 *octagon-size*)))
      (setq y (random (- 200 *octagon-size*)))
      
      ;; Change the color of the pixmap by changing its
      ;; colors vector.
      ;; ggg This isn't working on GTK.  Unclear what's happening.
      (setf (aref (colors *octagon-pixmap*) 1)
        (aref *octagon-color-choices* (random 6)))
      
      (copy-to-stream *octagon-pixmap*
                      (frame-child window)
                      (make-box x y (+ x *octagon-size*)(+ y *octagon-size*))
                      (make-box 0 0 *octagon-size* *octagon-size*)))
    window))

#+run-example
(run-octagons-example)
