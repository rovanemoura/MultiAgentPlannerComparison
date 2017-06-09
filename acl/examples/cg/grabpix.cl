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


;;; Sample Application for get-screen-pixmap

(in-package :cg-user)

;;; Define a class for a tester window that grabs and displays pixmaps.
(defclass get-screen-pixmap-window (bitmap-window)
    ((current-pixmap :initform nil
       :accessor current-pixmap)))

(defparameter *get-screen-pixmap-dialog-has-been-shown* nil)

;;; Define an on-change event-handler for a "Grab" button
(defun test-get-screen-pixmap-button-on-change (wij new old)
  (declare (ignore new old))
  (let* ((window (parent (parent wij)))
         (old-width (exterior-width window))
         (old-height (exterior-height window))
         pixmap box old-palette)
    
    ;; Hide the tester window to get it out of the way
    ;; while the user is grabbing a screen area
    (shrink-window window t)
    
    ;; Prompt the user for a screen area, and copy the screen pixels from there.
    ;; (To grab a pixmap without prompting the user, call get-pixmap instead.)
    (setq pixmap (get-screen-pixmap
                  
                  ;; Show the explanatory dialog the first time only.
                  :no-dialog *get-screen-pixmap-dialog-has-been-shown*))
    (setq *get-screen-pixmap-dialog-has-been-shown* t)
    
    ;; Make the tester window just large enough to show the current pixmap
    ;; (but always at least a minimum size for showing the buttons and so on)
    (setf (height window)
      (max 50 (if pixmap
                  (+ (- (exterior-height window)
                        (interior-height (frame-child window)))
                     (height pixmap))
                old-height)))
    (setf (width window)
      (max 154 (if pixmap
                   (+ (- (exterior-width window)
                         (interior-width (frame-child window)))
                      (width pixmap))
                 old-width)))
    
    (if* pixmap
       then (window-message window "GRABBED a ~a by ~a pixmap of depth ~a."
              (width pixmap)(height pixmap)(bits-per-pixel pixmap))
       else (window-message window "Pixmap grabbing CANCELED."))
    
    ;; As long as the user did not cancel, copy the pixmap to the
    ;; tester window to see that our copy is a good one.
    (when pixmap
      (setq box (make-box 0 0 (width pixmap)(height pixmap)))
      (clear-page window)
      
      ;; If Windows is running in 16, 256, or 64k colors, then the
      ;; pixmap will have a colormap, so assign it to the tester window
      ;; before copying the pixmap to the window.
      (when (and (colors pixmap)
                 (< (bits-per-pixel (screen *system*)) 16))
        (setq old-palette (palette window))
        (setf (palette window)
          (open-palette window (colors pixmap) nil))
        (unless (symbolp old-palette)
          (close-palette window old-palette)))
      
      ;; Display the grabbed pixmap in the tester window.
      (copy-to-stream pixmap (frame-child window) box box)
      
      ;; Tell the tester window to remember the pixmap that it is displaying
      ;; so that the Return Selected Object command can return it
      (setf (current-pixmap window) pixmap))
    
    ;; Reveal the tester window now that it's all ready.
    (select-window window))
  
  ;; Return true from any on-change function to accept the new value.
  t)

;;; Define an on-change event-handler for a "Save" button
(defun test-save-screen-pixmap-button-on-change (wij new old)
   (declare (ignore new old))
   ;; Let the user save the grabbed pixmap to a .bmp bitmap file.
   (let* ((window (parent (parent wij)))
          (pixmap (current-pixmap window))
          path)
      (cond (pixmap
             (when (setq path (ask-user-for-new-pathname
                               "Save pixmap to where?"
                               :allowed-types (list (cons "Bitmap File" "*.bmp"))
                               ))
                (save-pixmap pixmap path)))))
   t)

;;; Tell the IDE to invoke commands on the pixmap being displayed.
(defmethod selected-object ((window get-screen-pixmap-window))
   (current-pixmap window))

;;; The entry point for this example.
(defun run-get-screen-pixmap-example ()
  (let* ((bwin (make-window :test-get-screen-pixmap
                 :class 'get-screen-pixmap-window
                 :title "Get-Screen-Pixmap Test"
                 :resizable nil
                 :width 200
                 :height 100
                 ))
         (grab-button (make-instance 'button
                        :name :test-get-screen-pixmap-button
                        :title "~Grab"
                        :box (make-box 8 2 68 24)
                        :on-change 'test-get-screen-pixmap-button-on-change
                        ))
         (save-button (make-instance 'button
                        :name :test-save-screen-pixmap-button
                        :title "~Save"
                        :box (make-box 76 2 136 24)
                        :on-change 'test-save-screen-pixmap-button-on-change
                        ))
         (toolbar (add-toolbar bwin))
         )
    (add-component grab-button toolbar)
    (add-component save-button toolbar)
    (select-window bwin)
    
    ;; Focus on the toolbar pane so that the Alt-G and Alt-S
    ;; keyboard shortcuts will work to activate the button widgets.
    (set-focus-component grab-button)
    
    ;; Return the main window of the application from this
    ;; on-initialization function, so that the application will
    ;; exit when the user closes this window (and not until then).
    bwin))

#+run-example
(run-get-screen-pixmap-example)
