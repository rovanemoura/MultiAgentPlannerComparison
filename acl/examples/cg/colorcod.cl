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

;; Color widgets demo.
;; This file contains the user-written application code.  
;; The file colordef.cl contains the code that was
;; generated automatically by the interface builder to recreate the
;; dialog window and widgets.

(in-package :cg-user)

;;; See also the file colordef.cl for the code that creates the
;;; dialog and its widgets.

;;; A custom subclass for our dialog.
(defclass color-demo (dialog)
  ())

;;; The menu selection function for the color menu.
(defun change-menu-item-color (menu menu-item stream)
  (declare (ignore menu stream))
  (change-color-now (value menu-item)))

(defparameter *color-demo-speed* :slow)

;;; The menu selection function for the "Speed" menu.
(defun change-menu-item-speed (menu menu-item stream)
  (declare (ignore stream))
  (dolist (item (menu-items menu))
    (setf (selected item) nil))
  (setf (selected menu-item) t)
  (setq *color-demo-speed* (value menu-item)))

;;; The menu selection function for the "Edit Scrollbar" menu
(defun change-menu-item-scrollbar (menu menu-item stream)
  (declare (ignore menu stream))
  (let* ((scrollbar (find-component :color-scroller (color-demo)))
         (box (box scrollbar)))
    (setf (box-right box)
      (max (+ (box-left box) 5)
           (+ (box-right box)
              (value menu-item))))
    (setf (box scrollbar) box)))

;;; The on-change for the list widget.
;;; Runs when a color is selected from the list-box.
(defun color-list-function (widget new-value old-value)
  (declare (ignore widget old-value))
  (change-color-now new-value)
  ;; Return non-NIL to accept the new value.
  t)

;;; This runs when the "Red" radio-button is clicked on.
(defun change-red-fn (button new old)
  (declare (ignore button old))
  ;; only do it if the button is being turned "on"
  (when new				
    (change-color-now 'red))
  t)

;;; This runs when the "Blue" radio-button is clicked on.
(defun change-blue-fn (button new old)
  (declare (ignore button old))
  (when new
    (change-color-now 'blue))
  t)

;;; This runs when the scroll bar is clicked on.
(defun scroll-bar-function (scroll-bar new-value old-value)
  (declare (ignore old-value))
  (when new-value
    (change-color-now (nth new-value
                           (range
                            (find-sibling :color-list scroll-bar)))))
  t)

;;; This function modifies widgets to reflect the new color.
(defun change-color-now (color-name)
  (let* ((dialog (find-window :color-demo))
         (color-list (find-component :color-list dialog))
         (scroll-bar (find-component :color-scroller dialog))
         (radio-button? (find-component color-name dialog))
         (box (find-component :box dialog))
         (message-box (find-component :message dialog))
         (color (symbol-value color-name))
         dark-color-name)
    
    ;; Find the color in the list-box.
    (setf (value color-list) color-name)
    
    ;; Select the radio button for this color if there is one,
    ;; or else select the "Other" radio button.
    (if radio-button?
        (setf (value radio-button?) t)
      (setf (value (find-component :other dialog)) t))
    
    ;; Set the color and text of the small rectangle.
    (setf (background-color box) color)
    (setf (value box)(symbol-name color-name))
    
    ;; Set the text color and string of the message box.
    ;; Draw the text in the "dark" version of each color
    ;; so that it shows up better.
    (setq dark-color-name (intern (concatenate 'string
                                    (change-case-like-reader "dark-")
                                    (symbol-name color-name))
                                  :cg))
    (setf (foreground-color message-box)
      (if (boundp dark-color-name)
          (symbol-value dark-color-name)
        color))
    
    ;; Set the scroll-bar to the currently selected color.
    (setf (value scroll-bar)
      (list-widget-get-index color-list))
    
    ;; Show a message for the currently selected color.
    (setf (value message-box)
      (case color-name
        (red "Now there's a sexy color.")
        (cyan "My personal favorite.")
        (green "What a bright happy color.")
        (yellow "Can't go wrong with yellow.")
        (blue "Ah.  How soothing.")
        (magenta "Ack!  Anything but magenta!")
        (t "You win the special prize!")))))

(defparameter *going* nil)

;;; This runs when the Go/Stop button is clicked on.
(defun go-button-function (button new old)
  (declare (ignore new old))
  (cond (*going*
         (setf (title button) "~Go")
         (setq *going* nil))
        (t
         (setf (title button) "~Stop")
         (do* ((dialog (parent button))
               (list-box (find-component :color-list dialog))
               (colors (range list-box))
               (*going* t))
              ((or (not *going*)
                   ;; Quit if the user has closed the dialog
                   (not (windowp dialog)))
               t)
           (setf (value list-box)
             (nth (mod (1+ (or (list-widget-get-index list-box) 1))
                       (length colors))
                  colors))
           (process-pending-events)
           (case *color-demo-speed*
             (:slow (sleep 1))
             (:medium (sleep .2))
             (:fast nil))))))

;;; Add this method to determine where the window will appear
;;; on the screen when no position is passed to make-window.
(defmethod default-top-left ((window color-demo) width height)
   (declare (ignore width height))
   (make-position 500 200))

;;; Write out the title in the bottom text area.
(defmethod menu-item-highlighted ((dialog color-demo) menu menu-item menu-bar-p)
  (declare (ignore menu menu-bar-p))
  (setf (value (find-component :message dialog))
    (princ-to-string (remove #\~ (title menu-item)))))
                                   
;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
;;
(defun run-color-demo-example ()
  (let ((w (color-demo)))
    (select-window w)
    w))

#+run-example (run-color-demo-example)
