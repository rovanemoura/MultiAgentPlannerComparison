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

;;; Passing text and pixmaps on the OS clipboard and the lisp clipboard.

(in-package :cg-user)

;; Use our own subclass of frame-window for our main window,
;; so that we can specialize methods on it.
(defclass clipboard-example-frame (frame-window)
  
  ;; Remember which child window was most recently selected,
  ;; and apply menu-bar commands to that child.
  ((current-child :initform nil
                  :accessor current-child)))

;; Call this function to run the test.
(defun run-clipboard-example ()
  (let* ((parent (make-window :clipboard-test
                   :class 'clipboard-example-frame
                   :menu (my-menu-bar)
                   :scrollbars nil
                   :exterior (make-box
                              0 150
                              (round (* (interior-width (screen *system*)) .7))
                              (round (* (interior-height (screen *system*)) .7)))))
         (width (interior-width parent))
         (height (interior-height parent))
         (margin 20))
    
    ;; Make a child window to paste strings into.
    (make-window :text-window
      :class 'my-text-window
      :owner parent
      :scrollbars :vertical
      :exterior (make-box margin margin (floor (- width margin) 2)
                          (- height margin)))
    
    ;; Make a child window to paste pixmaps into.
    (make-window :pixmap-window
      :class 'my-pixmap-window
      :owner parent
      :scrollbars t
      :exterior (make-box (floor (+ width margin) 2)
                          margin (- width margin)(- height margin)))
    
    ;; Place an object of each type initially onto the clipboard.
    (push-lisp-clipboard :text "Initial Clipboard Text")
    (push-lisp-clipboard :pixmap (find-pixmap :gray-drop-arrow))
    
    parent))
    
;; -------------------------------------------------------------------

;; Define a class of windows that know how to paste text.
(defclass my-text-window (frame-window)
  ((my-string :accessor my-string :initform "Nothing pasted yet"))
  (:default-initargs
   :font (make-font-ex nil "Arial" 24)
   :title "Paste strings here to see them large"))

(defmethod set-focus :after ((window my-text-window))
  
  ;; Remember that this child window MOST RECENTLY had the
  ;; keyboard focus, for directing keyboard commands to this window.
  (setf (current-child (parent window)) window))

(defmethod default-clipboard-format ((window my-text-window))
  
  ;; This tells our "paste" code to grab the
  ;; current :text object off of the clipboard, if any.
  :text)

(defmethod cut-selection ((window my-text-window))
  (let* ((string (my-string window)))
    (setf (my-string window) nil)
    
    ;; Cause the window to erase the string that was cut.
    (invalidate window)
    (values string :text)))

(defmethod copy-selection ((window my-text-window))
  
  ;; We have only one thing to copy to the clipboard, so return it.
  ;; A copy-selection or cut-selection method must return the window
  ;; and the object to be placed on the clipboard.
  (values (my-string window) :text))

(defmethod paste-selection ((window my-text-window) object)
  
  ;; Just record the current string to draw here, so that all
  ;; drawing is encapsulated in the redisplay-window method, which
  ;; is called whenever the window is uncovered or otherwise invalidated.
  (when object
    (let* ((string (princ-to-string object))
           (text-height (draw-wrapped-string
                         window string
                         :newline-spacing-in-lines .2
                         :just-return-text-height-p t)))
      (setf (my-string window) string)
      (when text-height
        (setf (page-height window) text-height)))
    (invalidate window)
    (values object :text)))

(defmethod redisplay-window ((window my-text-window) &optional box)
  (declare (ignore box))
  (call-next-method) ;; clear the window
  
  ;; Whenever the window is uncovered, draw its current string
  ;; wrapped to fit the window's width.
  (let* ((string (my-string window)))
    (when string
      (move-to-x-y window 0 0)
      (setf (right-margin window)(interior-width window))
      (draw-wrapped-string window string
                           :newline-spacing-in-lines .2))))

;; -------------------------------------------------------------------

;; Define a class of windows that know how to paste pixmaps.
(defclass my-pixmap-window (frame-window)
  ((my-pixmap :accessor my-pixmap :initform (find-pixmap :melvin)))
  (:default-initargs
      :title "Paste pixmaps here to see them double size"))

(defmethod set-focus :after ((window my-pixmap-window))
  
  ;; Remember that this child window MOST RECENTLY had the
  ;; keyboard focus, for directing keyboard commands to this window.
  (setf (current-child (parent window)) window))

(defmethod default-clipboard-format ((window my-pixmap-window))
  ;; This tells our "paste" code to grab the
  ;; current :pixmap object off of the clipboard, if any.
  :pixmap)

(defmethod copy-selection ((window my-pixmap-window))
  (values (my-pixmap window) :pixmap))

(defmethod paste-selection ((window my-pixmap-window) object)
  (when (typep object 'pixmap)
    (setf (my-pixmap window) object)
    (set-page-size window (* 2 (width object))(* 2 (height object)))
    (invalidate window)
    (values object :pixmap)))

(defmethod redisplay-window ((window my-pixmap-window) &optional box)
  (declare (ignore box))
  (call-next-method) ;; clear the window
  
  ;; Whenever the window is uncovered, draw its current pixmap
  ;; centered in the window, scaling it by a factor of two.
  (when (my-pixmap window)
    (let* ((pixmap (my-pixmap window))
           (pixmap-width (* 2 (width pixmap)))
           (pixmap-height (* 2 (height pixmap)))
           (window-width (interior-width window))
           (window-height (interior-height window)))
      (with-boxes (box1)
        (copy-to-stream
         pixmap window
         (nmake-box-relative box1
           (max 0 (floor (- window-width pixmap-width) 2))
           (max 0 (floor (- window-height pixmap-height) 2))
           pixmap-width pixmap-height))))))
  
;; -------------------------------------------------------------------

;; Create a menu-bar that calls our cut-and-paste functionality.
(defun my-menu-bar ()
  ;; Return a menu-bar with Cut, Copy, and Paste commands on it.
  ;; The menu-bar has a single menu-item whose value is the
  ;; Edit pull-down menu.
  (open-menu
   (list (make-instance 'menu-item
           :name :edit-menu
           :title "~Edit"
           :value
           (open-menu
            (list (make-instance 'menu-item
                    :name :copy
                    :title "C~opy"
                    :value 'copy-from-child-with-focus
                    :event-synonym '(control-key #\C)
                    :help-string "Copy to clipboard")
                  (make-instance 'menu-item
                    :name :paste
                    :title "~Paste"
                    :value 'paste-to-child-with-focus
                    :event-synonym '(control-key #\V)
                    :help-string "Draw from clipboard"))
            'pull-down-menu (screen *system*)
            :name :edit-menu
            :on-click 'funcall-menu-item-with-window)))
   'menu-bar (screen *system*)
   :name :my-menu-bar))

(defmethod copy-from-child-with-focus ((stream cg-stream))
  
  ;; This function is called by the "Copy" menu-item.
  ;; If one of our two child windows has been selected
  ;; when the user invokes this menu item, then copy a
  ;; value from this window to the lisp clipboard stack.
  (let* ((child-window (current-child stream)))
    (when (windowp child-window) ;; (child hasn't been closed)
      (copy-command child-window))))

(defmethod paste-to-child-with-focus ((stream cg-stream))
  
  ;; This function is called by the "Paste" menu-item.
  ;; If one of our two child windows has been selected
  ;; when the user invokes this menu item ...
  (let* ((child-window (current-child stream)))
    (when (windowp child-window) ;; (child hasn't been closed)
      
      ;; ... then paste a value from the lisp clipboard stack
      ;; to this window, selecting the value of the particular
      ;; clipboard format (:text or :pixmap) that this window
      ;; knows how to handle.
      ;; Actually (paste-command child-window) would do this.
      (paste-selection
       child-window (top-clipboard-value-of-type
                     (default-clipboard-format child-window))))))

;; These methods cause the windows to be redrawn when resized,
;; since they will then fit their content differently then
(defmethod invalidate-window-on-resize ((window my-text-window))
  t)
(defmethod invalidate-window-on-resize ((window my-pixmap-window))
  t)

;; -------------------------------------------------------------------
#+run-example
(run-clipboard-example)
