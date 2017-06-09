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
;; Using .bmp bitmap files.

;; This example demonstrates three ways to draw pixmaps that are
;; contained in .bmp bitmap files.  Calling load-pixmap-test
;; with no arguments demonstrates using load-pixmap to create
;; a lisp pixmap array and texture-info-structure from a .bmp
;; file, and then drawing the image from the lisp pixmap array.
;; Note that load-pixmap is much faster for aclwin release 3.0.2.
;; Passing the :make-pixmap-handles argument as non-NIL further
;; creates "device-dependent" pixmaps from the lisp arrays; drawing
;; from the pixmap handle is much faster but uses OS resources and
;; takes longer to initially prepare the pixmap.  
;; Passing :from-file as non-NIL draws images from .bmp files
;; directly without creating an intermediate lisp data object
;; for the bits.

(in-package :cg-user)

(defclass load-pixmap-frame (non-refreshing-window)())

(defclass load-pixmap-pane (basic-pane)())

(defmethod default-pane-class ((window load-pixmap-frame))
  'load-pixmap-pane)

(defun load-pixmap-test (&key from-file make-pixmap-handles)
  (let* ((window
          (make-window
              :load-pixmap-test
            :class 'load-pixmap-frame
            :owner (development-main-window *system*)
            :state :shrunk
            :scrollbars nil
            :double-buffered t
            :title (if* from-file
                      then "Pixmaps Drawn Directly From Files (Click to Cycle)"
                    elseif make-pixmap-handles
                      then "Fast Bitblting with Pixmap Handles (Click to Cycle)"
                      else "Pixmaps Loaded as Lisp Arrays (Click to Cycle)")
            :exterior (make-box 400 170 960 630)))
         (pane (frame-child window))
         
         ;; Find all the .bmp pixmap files in the Windows directory
         (files
          (mapcar
              #'namestring
            (append
             
             ;; Show any pixmaps that are in the Windows directory,
             #+mswindows
             (directory (merge-pathnames
                         "*.bmp"
                         ;; rrrask Shouldn't this be exported from
                         ;; the windows (winapi) directory?
                         (sys::windows-directory)))
             
             ;; plus any pixmaps from the CG examples directory,
             (directory (merge-pathnames
                         "examples/cg/pixmaps/"
                         (path (app *system*))))
             
             ;; plus any pixmaps in the main Allegro directory
             ;; or the main directory of the standalone application
             ;; that's made from this project
             (directory (merge-pathnames
                         "*.bmp" (path (app *system*)))))))
         
         ;; Draw the larger pixmaps first so that they don't cover the
         ;; smaller ones 
         (xy-sizes nil)
         (sorted-files nil))
    (add-status-bar window nil 2)
    
    ;; Store this parameter on the window so that we can check it later.
    (set-stream-prop pane :make-pixmap-handles make-pixmap-handles)
    
    ;; Remove any compressed pixmaps
    (setq files (remove-if-not  
                 #'(lambda (path)
                     ;; check for compressed pixmaps
                     (errorset
                      (texture-info-bits-per-pixel
                       (load-texture-info path))))
                 files))
    
    ;; Find the screen size of each pixmap, accounting for the different
    ;; numbers of bits-per-pixel
    (setq xy-sizes (mapcar '(lambda (path)
                              (list path
                                    (floor (file-length path)
                                           (texture-info-bits-per-pixel
                                            (load-texture-info path)))))
                     files))
    
    ;; Sort files by size
    (setq sorted-files
          (sort files
                #'(lambda (path1 path2)
                    (> (second (assoc path1 xy-sizes :test #'string=))
                       (second (assoc path2 xy-sizes :test #'string=))))))
    
    ;; Use only the first ten larger pixmaps.
    (when (> (length sorted-files) 20)
      (setf (rest (nthcdr 19 sorted-files)) nil))
    
    ;; Store the list of pixmaps on the window for use by the redisplay-window
    ;; method below.  The plist of a window is a handy place to hang arbitrary
    ;; information when you don't want to subclass the window class and add
    ;; slots to it.  Get-stream-prop is a shortcut for accessing the plist 
    ;; of a window or other stream.
    (setf (get-stream-prop pane :pixmaps)
      (if* from-file
         then sorted-files
         else (with-hourglass
                (mapcar #'load-pixmap sorted-files))))
    
    ;; Expose the window now that we are prepared to draw its contents.
    (expand-window window)
    (bring-window-to-front window)
    (set-focus pane)
    
    ;; Assign the palette for the first pixmap to the window, so that
    ;; when the window initially appears the first pixmap will look correct.
    ;; This should only be necessary and useful if Windows is being run
    ;; in 256-color mode, so this bit is no longer included by default.
    #+old
    (pixmap-example-set-palette window 
                                (first (get-stream-prop window :pixmaps)))
    (invalidate pane)
    
    (select-window window)
    window))

(defmethod redisplay-window ((window load-pixmap-pane) &optional invalid-box)
  (declare (ignore invalid-box))
  (call-next-method) ;; Draw the blank window background
  (redisplay-each-pixmap
   window (first (get-stream-prop window :pixmaps))))

(defun pixmap-example-message (window path texture-info)
  (let* ((minimum-size 128)
         (width (texture-info-width texture-info))
         (height (texture-info-height texture-info))
         (file-namestring (file-namestring path))
         (true-color (>= (texture-info-bits-per-pixel texture-info) 16)))
    (window-message window
        "~& ~a is ~s wide ~aby ~a tall ~aby ~a ~
         bit~:*~p deep using ~a color~a.~%"
      (subseq file-namestring 0 (position #\. file-namestring))
      width
      (if* (< width minimum-size)
         then "(stretched) "
         else "")
      height
      (if (< height minimum-size) "(stretched) " "")
      (texture-info-bits-per-pixel texture-info)
      (if* true-color
         then "true"
         else (length (texture-info-colors texture-info)))
      (if* true-color
         then ""
         else "s"))))

(defun redisplay-each-pixmap (window pixmap)
  (let* ((box #.(make-box 0 0 0 0))
         (minimum-size 128)
         (width nil)
         (height nil))
    (cond ((stringp pixmap)
           
           ;; For the direct-from-file test, redisplay the window each time
           ;; from the original .bmp files
           (let* ((info (load-texture-info pixmap)))
             (copy-pixels-to-stream-from-file
              pixmap window
              
              ;; Center the pixmap in the window
              (make-position (floor (- (interior-width window)
                                       (texture-info-width info))
                                    2)
                             (floor (- (interior-height window)
                                       (texture-info-height info))
                                    2)))
             (pixmap-example-message
              window pixmap (load-texture-info pixmap))))
          
          ;; For the load-into-lisp-arrays test, redisplay the window from
          ;; the pixmap arrays that we created with load-pixmap
          (pixmap
           (setq width (width pixmap))
           (setq height (height pixmap))
           (setf (box-right box) width)
           (setf (box-bottom box) height)
           (copy-to-stream
            pixmap window
            (center-box-on-window
             window
             ;; If a pixmap is less than 128 pixels wide or tall,
             ;; stretch it to that size.
             (max width minimum-size)(max height minimum-size))
            box po-replace)
           (pixmap-example-message
            window (source pixmap) (texture-info pixmap)))
          (t (draw-string-in-box
              window
              "No pixmaps were found to display."
              nil nil (visible-box window) :center :center))
          )))

(defun cycle-pixmaps (stream)
  
  ;; When the user clicks the window, display the pixmaps in a different
  ;; order, and use the colors that are correct for the pixmap in front.
  (let* ((pixmaps (get-stream-prop stream :pixmaps))
         (pixmap nil)
         (true-color (>= (bits-per-pixel (screen *system*)) 16)))
    
    (unless pixmaps (return-from cycle-pixmaps))
    
    ;; Move the backmost pixmap to the front.
    (setf (cdr (last pixmaps))(list (first pixmaps)))
    (pop (get-stream-prop stream :pixmaps))
    
    ;; Clear the window before changing to the palette for the next pixmap
    ;; so that we don't momentarily see the old pixmap with the new palette.
    ;; This should only be necessary and useful if Windows is being run
    ;; in 256-color mode, so this bit is no longer included by default.
    #+old
    (clear-page stream)
    
    (setq pixmap (first (get-stream-prop stream :pixmaps)))
    
    ;; Tell the window to use the palette that goes with the pixmap
    ;; that is now in front (drawn last).
    ;; This should only be necessary and useful if Windows is being run
    ;; in 256-color mode, so this bit is no longer included by default.
    #+old
    (pixmap-example-set-palette stream pixmap)
    
    ;; The following note applies only when Windows is not being
    ;; run in true-color mode.
    ;; If this is the pixmap-handle demo, then make a pixmap-handle
    ;; from the texture-array and texture-info.
    ;; When creating a pixmap handle using a custom palette (rather than the
    ;; default system palette of 20 colors), the pixel values will be mapped
    ;; according to the current palette of the window passed in.
    ;; So each time we are using a different palette in the window (which
    ;; we do for every pixmap in this demo), we must create the pixmap
    ;; handle again using the window's current palette.  So each pixmap
    ;; may not draw faster the first time it is drawn, but if the window
    ;; is covered and then uncovered, the current pixmap should redraw
    ;; extra fast by using the existing pixmap-handle for it.
    (when (get-stream-prop stream :make-pixmap-handles)
      (open-pixmap-handle pixmap
                          :recreate (not true-color)
                          :window stream))
    
    ;; If the window was using a palette already, then close it to conserve
    ;; OS resources.  Common Graphics doesn't close these automatically since
    ;; you may be using a palette in other windows or at other times, so
    ;; we close it ourselves because we know that we are through with it.
    ;; This should only be necessary and useful if Windows is being run
    ;; in 256-color mode, so this bit is no longer included by default.
    #+old
    (when (and old-palette
               (not (symbolp old-palette))) ;; not if palette is :rgb
      (close-palette stream old-palette))
    
    ;; Tell the OS that this window needs to be redisplayed.
    ;; After all other events are processed, the OS will tell us
    ;; to repaint the window.
    (invalidate stream) 
    
    ;; Except that this line tells the OS to send us the paint request
    ;; immediately so that we don't have to wait for the window to update.
    (update-window stream)))
         
;; This should only be necessary and useful if Windows is being run
;; in 256-color mode, so this bit is no longer included by default.
#+old
(defun pixmap-example-set-palette (stream pixmap)
  ;; Create the actual palette and assign it to the window.
  (let ((colors (if* (stringp pixmap) ;; the copy-from-file example
                   then (texture-info-colors (load-texture-info pixmap))
                   else (colors pixmap))))
    (when colors
      (setf (palette stream)
        (open-palette stream colors nil)))))

(defmethod mouse-left-down ((stream load-pixmap-pane) buttons data)
  (declare (ignore buttons data))
  ;; Click the window to cycle through the list of pixmaps.
  (cycle-pixmaps stream))

(defmethod mouse-double-click ((stream load-pixmap-pane) buttons data)
  (declare (ignore buttons data))
  ;; Do the same thing for a double-click so that it will happen for every
  ;; click of a rapid-fire sequence of clicks.
  (cycle-pixmaps stream))

(defmethod mouse-right-down ((stream load-pixmap-pane) buttons data)
  (declare (ignore buttons data))
  (cycle-pixmaps stream))

(defmethod mouse-right-double-click ((stream load-pixmap-pane) buttons data)
  (declare (ignore buttons data))
  (cycle-pixmaps stream))

;; Since we center the pixmaps in the window (in the load-as-lisp-array test),
;; cause a full redisplay after resizing the window in order to re-center.
(defmethod resize-window :after ((window load-pixmap-pane) position)
  (declare (ignore position))
  (invalidate window))

;; If we are running the really fast version of this example where
;; create-pixmap-handle is called to created a device-dependent pixmap
;; (pixmap handle) for each pixmap, then close each pixmap upon closing
;; the demo window in order to free Windows OS resources.  This would be
;; done anyway when lisp exits, but we may need the resources in the meantime.
(defmethod device-close :after ((window load-pixmap-pane) abort)
  (declare (ignore abort))
  (dolist (pixmap (get-stream-prop window :pixmaps))
    (when (typep pixmap 'pixmap)
      (close-pixmap-handle pixmap))))

;; These are the on-initialization functions.  As such, each returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.


(defun run-array-blit-example ()
  (load-pixmap-test))

(defun run-fast-blit-example ()
  (load-pixmap-test :make-pixmap-handles t))

(defun run-file-blit-example ()
  (load-pixmap-test :from-file t))

#+run-example
(run-array-blit-example)
