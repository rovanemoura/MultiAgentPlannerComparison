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

;;;; An Example to Show All Fonts

(in-package :cg-user)

(defclass fonts-example-window (frame-window)
  ((example-string :accessor example-string
                   :initform nil)
   (example-font :accessor example-font
                 :initform nil)))

(defmethod redisplay-window ((window fonts-example-window) &optional box)
  (declare (ignore box))
  (clear-page window)        ;; Erase the previous font.
  (let* ((string (example-string window))
         (font (example-font window)))
    (when (and string font)
      (move-to window #.(make-position 12 12))
      (setf (font window) font)
      (format window string))))

(defun run-fonts-example ()
  (let* ((my-window (make-window :font-example   ; A test window
                      :class 'fonts-example-window
                      :exterior (make-box 50 100 700 250)
                      :owner (development-main-window *system*)
                      :scrollbars nil
                      :double-buffered t
                      :title "All Fonts"))
         (initial-font (font my-window))   ; Preserve the original font.
         (fonts-list (font-faces           ; Get the list of all font faces. 
                      (screen *system*))))
    
    (dolist (name fonts-list)             ; Run through all of the font faces. 
      (let* ((sizes-list                 ; Get all the sizes for this face. 
              (font-sizes (screen *system*) name))
             (truetype (and sizes-list (zerop (first sizes-list))))
             
             ;; Call princ-to-string here because otherwise we would
             ;; modify a string constant below, which is not allowed.
             (test-string (princ-to-string
                           "ABCDE   abcde   xxxxx   xxxxx")))
        
        ;; Add some non-ascii characters (high bit turned on)
        ;; to the test-string so that the ethnic charset differences
        ;; in the non-ascii range will be seen.
        (dotimes (j 5)
          (setf (aref test-string (+ 16 j))
            (code-char (+ 128 #.(char-code #\A) j)))
          (setf (aref test-string (+ 24 j))
            (code-char (+ 128 #.(char-code #\a) j))))
        
        ;; If this is a truetype font, which can display any size,
        ;; then use an arbitrary large size.
        (when truetype
          (setq sizes-list '(32)))
        
        ;; Remove fonts that are too small to display.
        (setq sizes-list (delete-if #'(lambda (size)(< size 5)) sizes-list))
        
        ;; Run through all the sizes of this font face.
        (dolist (size sizes-list)
          (setf (example-font my-window)(make-font-ex nil name size))
          (setf (example-string my-window) test-string)
          (invalidate my-window)
          (setf (title my-window)
            (format nil "~s (~a) ~a" name
              (if truetype "vector" "raster")
              size))
          (sleep                         ; Pause a bit to view each font.
           (min (/ 4 3)(/ 3 (min 4 (length sizes-list)))))
          
          ;; A standalone application needs this call, or else the
          ;; single process will not handle the paint messages to redraw
          ;; the window, or handle a window-closing gesture.
          ;; (Actually an os-threads platform would handle messages
          ;; in the call to sleep above.)
          (process-pending-events-if-event-handler)
          
          ;; Quit if user kills the window.
          ;; Returning nil will cause a standalone app generated
          ;; from this project to exit at that point.
          (unless (windowp my-window)
            (return-from run-fonts-example nil))
          )))
    (setf (font my-window) initial-font)  ; Restore the original font. 
    (setf (title my-window) "Done!")      ; Trigger emotional closure.
    
    ;; In a standalone app, don't exit immediately.
    (sleep 2)
    
    ;; Return nil so a standalone app generated from this project
    ;; will demonstrating exiting automatically when this
    ;; on-initialization function returns.  Return my-window instead
    ;; to leave the window there until the user closes it.
    nil))

#+run-example
(run-fonts-example)

