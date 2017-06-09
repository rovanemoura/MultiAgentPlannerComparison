
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

;;;; Example of making a mouse cursor from a bitmap that was created
;;;; from a line drawing.

(in-package :cg-user)

(defclass fancy-cursor-window (frame-window)())

(defmethod redisplay-window ((window fancy-cursor-window) &optional box)
  (declare (ignore box))
  (call-next-method)
  (move-to window #.(make-position 0 0))
  (dotimes (j 6)
    (format window "~&  I've got a mouse cursor ~
                        made from a line drawing.~%")))

(defun run-mouse-cursor-example ()
  (let* (width height half-width half-height
               bitmap-stream bitmap mask pixmap
               cursor window)
    
    ;; Ask the OS for the official cursor size.
    (multiple-value-setq (width height)
      (cursor-size (screen *system*)))
    (setq half-width (floor width 2)
        half-height (floor height 2))
    
    ;; Open a bitmap-stream, which can be drawn to but which never
    ;; appears on the screen as a window.
    (setq bitmap-stream (open-stream 'bitmap-stream nil nil
                          :page-width width
                          :page-height height
                          :foreground-color black
                          :background-color white))
    
    ;; Ensure that the bitmap-stream is closed when unwinding
    (with-open-stream (bitmap-stream bitmap-stream)
      
      ;; ---------------------------------------------------------------------
      ;; Make the main cursor bitmap.
      
      ;; Erase the background.
      (clear-page bitmap-stream)
      
      ;; Draw a big thick plus sign on the bitmap-stream to define the white
      ;; main body of the plus.  Stop a few pixels short of each edge
      ;; to leave room for the rounded ends of the lines.
      ;; (If you want a smaller cursor, just draw within a smaller area
      ;; of the bitmap.)
      (with-line-width (bitmap-stream 4)
        (draw-line bitmap-stream
                   (make-position 3 half-height)
                   (make-position (- width 4) half-height))
        (draw-line bitmap-stream
                   (make-position half-width 3)
                   (make-position half-width (- height 4))))
      
      ;; You can make a cursor only from a pixmap object rather than from
      ;; a bitmap-stream, so use get-pixmap to retrieve the bitmap stream's
      ;; contents into a pixmap.
      (setq pixmap (get-pixmap bitmap-stream
                               :box (make-box 0 0 width height)))
      
      ;; Get-pixmap always creates a pixmap of the same depth (bits-per-pixel)
      ;; as the screen, according to the color depth selected in the Windows
      ;; control panel.  But cursors are monochrome and must be created
      ;; from a bit array (bitmap).  So we must make a bit array from the
      ;; "deeper" array returned by get-pixmap.  The bitmap for a cursor
      ;; should have a zero whereever the cursor should draw black or be
      ;; transparent, and a one whereever it should draw white (whereas the 
      ;; bitmap-stream by default will use the system palette where black
      ;; is at index zero, so map zeroes in the pixmap to ones in the bitmap).
      (setq bitmap (make-instance 'texture
                     :bits-per-pixel 1
                     :contents (make-texture-contents width height)))
      
      ;; Whereever we drew a black pixel (value 0) onto the bitmap-stream,
      ;; set the main bitmap's pixel to 1 to define the black region.
      (dotimes (y height)
        (dotimes (x width)
          (when (eq 0 (contents-ref pixmap x y))
            (setf (contents-ref bitmap x y) 1))))
      
      ;; ---------------------------------------------------------------------
      ;; Make a separate mask bit array to define the transparent area of
      ;; the cursor.  The main bitmap and mask bitmap together determine
      ;; the colors and transparency of the cursor as follows:
      
      ;; main bitmap 1     mask 0     cursor draws white
      ;; main bitmap 0     mask 0     cursor draws black
      ;; main bitmap 1     mask 1     cursor inverts the screen
      ;; main bitmap 0     mask 1     cursor is transparent
      
      ;; Erase the background to re-use the bitmap stream for the mask
      (clear-page bitmap-stream)
      
      ;; Draw a big thick plus on the bitmap-stream for the mask, to define the
      ;; opaque part of the cursor.  Draw it even thicker than the main bitmap
      ;; to define an extra opaque black border around the white interior
      ;; of the plus sign.
      (with-line-width (bitmap-stream 6)
        (draw-line bitmap-stream
                   (make-position 3 half-height)
                   (make-position (- width 4) half-height))
        (draw-line bitmap-stream
                   (make-position half-width 3)
                   (make-position half-width (- height 4))))
      
      ;; Retrieve the pixels into a pixmap, and convert to a bit array,
      ;; as with the main bitmap above.
      (setq pixmap (get-pixmap bitmap-stream
                               :box (make-box 0 0 width height)))
      (setq mask (make-instance 'texture
                   :bits-per-pixel 1
                   ;; Default bits to 1, since most of them will be one for
                   ;; the transparent background.
                   :contents (make-texture-contents width height 1)))
      
      ;; Whereever we drew a black (value 0) pixel on the bitmap-stream,
      ;; change the initial 1 in the mask to a 0 to define the opaque X area.
      (dotimes (y height)
        (dotimes (x width)
          (when (eq 0 (contents-ref pixmap x y))
            (setf (contents-ref mask x y) 0))))
      
      ;; ---------------------------------------------------------------------
      ;; Make the lisp object for the cursor.
      (setq cursor (make-instance 'cursor
                     :name :fat-arrow-cursor
                     :texture bitmap
                     :mask mask
                     
                     ;; Make the cursor's "hotspot" (active point)
                     ;; be the center of the plus sign.
                     :click-position (make-position (floor width 2)
                                                    (floor height 2))))
      
      ;; Make a test window to use the new cursor.
      (setq window (make-window :mouse-cursor-test
                     :class 'fancy-cursor-window
                     :exterior (make-box 500 300 900 450)
                     :title "Mouse Cursor Example"))
      
      ;; Assign the plus cursor to the test window.
      (setf (cursor window) cursor)
      
      ;; Return the main window, so that a standalone executable
      ;; made from this example will continue to run until this
      ;; window is closed.
      window)))
                     
#+run-example
(run-mouse-cursor-example)
