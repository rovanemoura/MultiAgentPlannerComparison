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

;;; A basic printing example, scaling a drawing to fit a printed
;;; page with margins.

(in-package :cg-user)

(defun run-basic-printing-example ()
  
  #+gtk (error "There is no support for printing on GTK at this time.")
  
  ;; Open a printer stream.  This will show the print job dialog
  ;; (unless :no-dialog-p is passed as true) and then set the
  ;; variable "printer" to a printer stream whose properties are
  ;; initialized according the end user's selections on the print
  ;; job dialog.  When this with-output-to-printer form exits, the
  ;; printer stream will be closed and the pages will physically print.
  (with-output-to-printer (printer)
    
    (let* ((original-drawing-width 1024)
           (original-drawing-height 768)
           
           ;; Here are some points that might originally have been drawn
           ;; to a window whose interior is 1024 by 768 pixels.  We will
           ;; map these cooridnates to a full sheet of printer paper below.
           (original-drawing-points
            (mapcar #'(lambda (pair)(apply #'make-position pair))
              '((1024 0)(1024 768)(0 768)(1024 0)
                (768 512)(512 512)(1024 0))))
           
           ;; Find the printer resolution, hard-wired margin,
           ;; and paper size.
           (pixels-per-inch (stream-units-per-inch printer))
           (physical-margin (printer-physical-offset printer))
           (physical-size (printer-physical-size printer))
           
           ;; Arbitrarily use a 3/4-inch margin on all sides.
           (total-left-margin (* 0.75 pixels-per-inch))
           (total-top-margin (* 0.75 pixels-per-inch))
           
           ;; Determine the margins by which we need to offset
           ;; our drawing in order to achieve 3/4-inch margins.
           ;; Since a Windows printer driver will always draw
           ;; the position (zero, zero) at the printer's built-in
           ;; "physical" margins, we must subtract the physical
           ;; margins from the margins by which we will offset.
           ;; (To use the margins that the user has selected on
           ;; the print job dialog instead, just use the margin
           ;; values that are set up automatically for the printer,
           ;; retrievable with (left-margin printer) and so on.
           ;; Those margin properties are initialized with the
           ;; physical margin already subtracted out, as we are
           ;; computing our arbitrary margin here.)
           (left-margin (round ;; must pass integers to CG
                         (- total-left-margin
                            (position-x physical-margin))))
           (top-margin (round (- total-top-margin
                                 (position-y physical-margin))))
           
           ;; Find the width and height of the drawable area
           ;; within the margins.
           (printable-width (- (position-x physical-size)
                               (* 2 total-left-margin)))
           (printable-height (- (position-y physical-size)
                                (* 2 total-top-margin)))
           
           ;; Find the factor by which we must multiply our
           ;; original window coordinates to find the corresponding
           ;; coordinates on the printed page.
           (scale-factor-x (/ printable-width
                              original-drawing-width))
           (scale-factor-y (/ printable-height
                              original-drawing-height))
           
           line-width box string)
      
      ;; Make our lines be 1/30th of an inch thick.
      (setf (line-width printer)
        (setq line-width
              (round pixels-per-inch 30)))
      
      ;; Map the arbitrary points so that the are scaled to
      ;; fit just within the printed page's margins.
      ;; Be sure to convert the final values to integers, as with
      ;; any numbers passed to Common Graphics functions.
      (dolist (point original-drawing-points)
        (setf (position-x point)
          (+ left-margin
             (round (* (position-x point)
                       scale-factor-x))))
        (setf (position-y point)
          (+ top-margin
             (round (* (position-y point)
                       scale-factor-y)))))
      
      ;; Draw lines on the printer stream to connect the points.
      (move-to printer (first original-drawing-points))
      (dolist (point (rest original-drawing-points))
        (draw-to printer point))
        
      ;; Set the line style so that the box will draw the
      ;; thickness of each line toward the center of the
      ;; box rather than centering the line thickness around
      ;; each specified line position.
      (setf (line-dashing printer) :inside-frame)
      
      ;; Use a font whose line-height is one-sixth of an inch.
      (setf (font printer)
        (make-font-ex nil :arial (round pixels-per-inch 6)
                      nil t)) ;; t means size is cell-height
      
      ;; Draw a box that has its left and top edges at our
      ;; 3/4-inch margins, and has a width and height of two inches.
      (setq box (make-box-relative left-margin top-margin
                                   (* 2 pixels-per-inch)
                                   (* 2 pixels-per-inch)))
      (draw-box printer box)
      
      ;; Draw a string inside the box that notes the size of the paper.
      ;; First make the box smaller to provide a margin within the box.
      (inflate-box box (* -3 line-width)(* -3 line-width))
      (setq string (format nil "This paper is ~a inches wide ~
                                           by ~a inches tall.  ~
                                This box is 2 inches square with its ~
                                upper-left corner at the 0.75-inch margins."
                     (round (position-x physical-size) pixels-per-inch)
                     (round (position-y physical-size) pixels-per-inch)))
      (draw-string-in-box printer string 0 (length string)
                          box :center :top nil t)
      )))


#+run-example
(run-basic-printing-example)
