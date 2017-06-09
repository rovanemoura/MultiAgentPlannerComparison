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

;; Printing a window's various graphics at the same size as in the window
;; but exploiting the better resolution of a laser printer.

(in-package :cg-user)

(defclass my-printer-test-dialog (dialog)
  ())

(defparameter *printer-test-pixmap* nil)

(defmethod redisplay-window ((dialog my-printer-test-dialog)
                             &optional box-to-redisplay)
  (declare (ignore box-to-redisplay))
  (call-next-method) ;; Draw the blank background
  (draw-stuff-on-window-or-printer dialog))

(defparameter *printer-resolution-directory* *load-pathname*)

(defun draw-stuff-on-window-or-printer (stream)
  
  ;; Load the big example pixmap from file if it has not been loaded yet.   
  (unless *printer-test-pixmap*
    (setq *printer-test-pixmap*
          (load-pixmap (merge-pathnames "pixmaps/example.bmp"
                                        *printer-resolution-directory*))))
           
  ;; Copy the pixmap image from the lisp pixmap array to either
  ;; the window or the printer.
  (let* ((width (width *printer-test-pixmap*))
         (height (height *printer-test-pixmap*))
         (left-margin 20)
         (top-margin 20))
    (copy-to-stream *printer-test-pixmap* stream
                    ;; destination box --- scoot the pixmap away
                    ;; from the edge of our window 
                    (make-box left-margin top-margin 
                              (+ left-margin width)
                              (+ top-margin height))
                    ;; source box --- copy the whole pixmap image
                    (make-box 0 0 width height)))
  
  ;; Draw some other stuff to show better resolution on the printer
  ;; then you get in the window or from the large pixmap.
  (dotimes (j 12)
    (draw-circle stream 
                 
                 ;; This line avoids consing a position structure
                 (nmake-position #.(make-position 0 0)
                   (+ 70 (* j 35)) ;; center-x
                   380)            ;; center-y
                 50))              ;; radius
  (draw-polygon stream (list #.(make-position 20 380)
                             #.(make-position 315 360)
                             #.(make-position 505 380)
                             #.(make-position 315 400)))
  
  (let* ((string-box #.(make-box 450 20 600 380)))
    (with-font (stream (make-font nil :arial 22 #+maybe '(:bold)))
      (draw-string-in-box
       stream
       (format nil "This text should have a higher resolution on the printer ~
 than it does in the window, since we are drawing it directly on the ~
 printer stream rather than simply copying the pixels from the window.")
       nil nil ;; start and end can be NIL to draw the whole string
       string-box :center :center nil t))
    
    ;; Draw-string-in-box doesn't draw an actual box; it just fits a
    ;; string into a box area.  But we want to see the box anyway,
    ;; so draw it now.
    (draw-box stream string-box)))

(defun printer-resolution-test-printer-button-on-change
    (button new-value old-value)
  (declare (ignore button new-value old-value))
  (with-output-to-printer (stream :orientation :landscape
                                  ;; This would skip the print job dialog.
                                  ;; :no-dialog-p t
                                  )
    
    ;; Set the scale of the printer stream to be roughly the same
    ;; as the monitor, so that the same coordinates can be used
    ;; to draw the picture at nearly the same size on the printer
    ;; as it appears on the screen.
    (setf (stream-units-per-inch stream)
      (round (*
              0.85 ;; fudge this since the OS understimates screen sizes
              (stream-units-per-inch (screen *system*)))))
    
    ;; Draw a box to show where the user has set the printer margins.
    (draw-box stream (make-box (left-margin stream)
                               (top-margin stream)
                               (right-margin stream)
                               (bottom-margin stream)))
    
    ;; A printer stream will let you try to draw to its very edge (though
    ;; a laser printer will typically clip off about a quarter inch).
    ;; So to scoot our drawing out of the margin, set the stream-origin
    ;; to the margin.
    (setf (stream-origin stream)
      (make-position (left-margin stream)(top-margin stream)))
    
    ;; Draw the same stuff that we draw on the window.
    (draw-stuff-on-window-or-printer stream)))

;; This is the on-initialization function for the project.  As such,
;; it returns a window such that when the window is closed, the
;; standalone application or Run Project thread will be terminated.
(defun run-printer-resolution-example ()
  
  #+gtk (error "There is no support for printing on GTK at this time.")
  
   (make-window :printer-resolution-test
     :class 'my-printer-test-dialog 
     :owner (development-main-window *system*)
     :title "Printer Resolution Test"
     :interior #.(make-box 250 180 880 640)
     :resizable nil
     :maximize-button nil
     :widgets
     (list (make-instance 'button
             :name :print-button 
             :on-change
             'printer-resolution-test-printer-button-on-change
             :left 520 :top 400 :width 80 :height 30
             :title "~Print"))))

#+run-example
(run-printer-resolution-example)
