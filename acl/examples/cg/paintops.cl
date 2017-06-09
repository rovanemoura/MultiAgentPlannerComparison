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

;;; Paint Operations Example
;;; Load this file to see a demonstration of the paint-operation property
;;; of a window.  (The final form in this file runs the demo.)

(defparameter paint-op-window
    (make-window :paint-op-window
      :class 'bitmap-window 
      :owner (development-main-window *system*) 
      :title "Demo Paint Ops"
      :interior #.(make-box 10 47 210 247)))

(defparameter *vertical-bar-texture*
  (let* ((texture (make-instance 'texture
                    :bits-per-pixel 1
                    :contents (make-texture-contents 32 32)))
         (array (texture-array texture)))
    (dotimes (j 16) (dotimes (i 32) (setf (aref array i j) 1))) 
    texture)
  "Defines bitmap which is a square whose left half is filled with black.")

(defparameter *horizontal-bar-texture*
  (let* ((texture (make-instance 'texture
                    :bits-per-pixel 1
                    :contents (make-texture-contents 32 32)))
         (array (texture-array texture)))
    (dotimes (j 32) (dotimes (i 16) (setf (aref array i j) 1))) 
    texture)
  "Defines a bitmap whose upper half is filled with black.")

(defun painting-operations (stream)
   "Returns a list of the supported paint operations in stream."
   (let ((initial-operation		;Save the default setting.
           (paint-operation stream))
         (ops))
      (dotimes (i 256)			;Loop over all 256 values.
         (when				;When attempt to set paint opn.
             (eql				;to counter returns same value
               i				;=> operation is supported.
               (setf (paint-operation stream)
                     i))
            (push i ops)))
      (setf (paint-operation		;Restore initial operation.
              stream)
            initial-operation)
      ops))

(defun display-the-operations 
    (stream &optional (wait-fn #'(lambda (stream) (sleep 2) t)))
  (select-window stream)
  (process-pending-events)
  (dolist (op (painting-operations stream))
    (clear-page stream)
    (setf (paint-operation stream) po-replace)
    (draw-box stream #.(make-box 19 39 52 72))
    (draw-box stream #.(make-box 119 39 152 72))
    (draw-box stream #.(make-box 89 139 122 172))
    (move-to stream #.(make-position 20 0))
    (princ "Source" stream)
    (copy-pixels-to-stream
     stream *vertical-bar-texture* nil #.(make-box 20 40 52 72)
     #.(make-box 0 0 32 32) po-replace)
    (move-to stream #.(make-position 100 0))
    (princ "Destination" stream)
    (copy-pixels-to-stream
     stream *horizontal-bar-texture* nil #.(make-box 120 40 152 72)
     #.(make-box 0 0 32 32) po-replace)
    (move-to stream #.(make-position 20 100))
    (format stream "Operation ~A " op)
    (case op				;looks for particular values.
      (48 (princ "PO-ERASE" stream))
      (60 (princ "PO-INVERT" stream))
      (170 (princ "PO-FILL" stream))
      (195 (princ "PO-XOR" stream))
      (204 (princ "PO-REPLACE" stream))
      (252 (princ "PO-PAINT" stream)))
    ;lay down the horizontal one:
    (copy-pixels-to-stream
     stream *horizontal-bar-texture* nil #.(make-box 90 140 122 172)
     #.(make-box 0 0 32 32) po-replace)
    ;;and now cover over with the vertical one, using the paint operation:
    (copy-pixels-to-stream
     stream *vertical-bar-texture* nil #.(make-box 90 140 122 172)
     #.(make-box 0 0 32 32) op)
    ;pause to let user see it
    (if (null (funcall wait-fn stream)) (return))))

(display-the-operations (frame-child paint-op-window)
                        #'(lambda (stream) (y-or-n-p "Next?")))

;;;end of file
