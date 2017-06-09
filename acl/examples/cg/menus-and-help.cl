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

;; A demo of menu-bars, pop-up-menus, and various options for
;; triggering the display of quick help.

(in-package :cg-user)

;;; Define our own window and widget subclasses so that
;;; we can add mouse-right-down methods to them below.

(defclass my-main-window (frame-window)())

(defclass my-single-item-list (single-item-list)())

;; Call this function to create the example's window
;; and give it a menu-bar and a common-status-bar.

(defun run-menus-and-help-example ()
  (let* ((window (make-window :my-main-window
                   :class 'my-main-window
                   :owner (screen *system*)
                   :title "Menus and Help Options"
                   :exterior (make-box 400 200 900 500)
                   :scrollbars nil
                   :dialog-items
                   (list (make-instance 'my-single-item-list
                           :name :list
                           :range '(one two three)
                           :value 'one
                           :tooltip "The first three."
                           :help-string "Pick a number.  Any number."
                           :on-mouse-in 'show-the-help-string
                           :on-mouse-out 'clear-the-status-bar
                           :on-change 'my-on-change
                           :left 20 :top 60
                           :width 100 :height 80)))))
    (add-common-status-bar window)
    
    ;; Give the window a menu-bar that has a single Help pull-down
    ;; menu, which in turn has two help commands on it.
    (open-menu
     
     ;; This is the menu-bar's menu-item for the Help pull-down menu.
     (list (make-instance 'menu-item
             :name :help-menu
             :title "~Help"
             :help-string "This pull-down menu contains help commands."
             :value (open-menu
                     
                     ;; This is the Help pull-down menu itself.
                     (list (make-instance 'menu-item
                             :name :help-on-value
                             :title "~Help on Value"
                             :value 'help-on-value
                             :help-string "Shows help on the selected value."
                             :event-synonym 'vk-f1)
                           (make-instance 'menu-item
                             :name :help-on-dialog
                             :title "Help on ~Dialog"
                             :value 'help-on-dialog
                             :help-string "Shows help on the selected window."
                             :event-synonym '(control-key vk-f1)))
                     'pull-down-menu (screen *system*)
                     :on-click 'funcall-menu-item-with-window)))
     'menu-bar window)
    window))

(defun help-on-value (window)
  "Called by the Help on Value menu command."
  
  ;; Find the selected value in the single-item-list on the main window.
  (let* ((value (selected-object (dialog-item (selected-window window)))))
    
    ;; Display information about this value in the window's status-bar.
    (window-message window "~a --- ~a"
      value
      (case value
        (one "The loneliest number.")
        (two "The second loneliest number.  Sometimes as bad as one.")
        (three "The number of dogs required for adequate warmth tonight.")
        (t "No value selected.")))
    
    #+later ;; This could be used if the HTML pages existed.
    (invoke-html-browser
     (merge-pathnames (format nil "help-dir/~a.html" value)
                      
                      ;; This returns the directory of the running
                      ;; executable lisp (IDE or standalone app).
                      (acl-directory)))))

(defun help-on-dialog (window)
  "Called by the Help on Dialog menu command."
  (window-message window "~a"
    "This window implements the famous CG Help example."))

(defun show-the-help-string (widget buttons mouse-out-window)
  "Called when the mouse moves into WIDGET."
  (declare (ignore buttons mouse-out-window))
  (window-message (parent widget) "~a" (help-string widget)))

(defun clear-the-status-bar (widget buttons mouse-in-window)
  "Called when the mouse moves out of WIDGET."
  (declare (ignore buttons mouse-in-window))
  (window-message (parent widget) ""))

(defun my-on-change (widget new-value old-value)
  "Called when the value of WIDGET has changed."
  (window-message (parent widget) "Widget ~s goes from ~s to ~s."
    (name widget) old-value new-value)
  t)

(defmethod mouse-right-down ((window my-main-window)
                             buttons cursor-position)
  ;; This is called when the user right-clicks the main window interior.
  (my-mouse-right-down window buttons cursor-position))

(defmethod mouse-right-down ((control my-single-item-list)
                             buttons cursor-position)
  ;; This is called when the user right-clicks the single-item-list.
  (my-mouse-right-down (parent control) buttons cursor-position))

(defun my-mouse-right-down (window buttons cursor-position)
  "Pops up a shortcut menu for this application."
  (declare (ignore buttons cursor-position))
  (let* ((menu (open-menu
                (list (make-instance 'menu-item
                        :name :help-on-value
                        :title "~Help on Value"
                        :value 'help-on-value
                        :help-string "Shows help on the selected value.")
                      (make-instance 'menu-item
                        :name :help-on-dialog
                        :title "Help on ~Dialog"
                        :value 'help-on-dialog
                        :help-string "Shows help on the selected window."))
                'pop-up-menu (screen *system*)))
         value)
    (unwind-protect
        
        ;; If the user selected a choice from the menu, call the
        ;; function which is the select menu-item's value.
        (when (setq value (pop-up-menu menu window))
          (funcall value window))
      
      ;; Be sure to close temporarily-used menus to free up
      ;; operating system menu resources.
      (close menu))))

(defmethod redisplay-window ((window my-main-window) &optional box)
  (declare (ignore box))
  (call-next-method) ;; draw the background
  
  ;; Draw this string in the window whenever it is uncovered.
  (draw-string-in-box
   window "Right-click to see the shortcut menu."
   nil nil (make-box 40 180 400 200) :left :top))

#+run-example (run-menus-and-help-example)
