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

;;; chee   24mar05 convert to using forward slashes in file namestrings
;;;        for the GTK port, which presumably work back on the Windows side

(in-package :user)

;;; Ensure that this module is included in a standalone app,
;;; as needed for deleting files and directories.
(eval-when (compile load eval)
  (require :fileutil))

;;; ------------------------------------------------------------------
;;; Non-Common Graphics Version
;;; Call user::disk-usage in any listener to report disk usage as
;;; text output to *terminal-io*.

(defun disk-usage (&key (root "c:/")(threshold 1000000))
  ;; ROOT should be the namestring of a directory to analyze.
  ;; THRESHOLD is a number of bytes.  Only directories and files
  ;; accounting for at least that many bytes will appear in the output.
  (setq root (namestring root))
  (let* ((last-char (aref root (1- (length root)))))
    ;; Make sure that the directory name ends in a final slash.
    (unless (member last-char '(#\\ #\/))
      (setq root (concatenate 'string root "/"))))
  (disk-usage-print (disk-usage-recursive root threshold) 0))

(defun disk-usage-recursive (root threshold)
  ;; Recursively sum the size of the contents of each subdirectory.
  (let* ((items nil)
         (sum 0)
         (item nil)
         (size nil))
    (dolist (file (sort
                   ;; bug7932 16apr99
                   (handler-case
                       (directory root :directories-are-files t)
                     (error (c)
                       (declare (ignore c))
                       #+mswindows
                       (with-native-string (msg (format nil "~
Error occurred reading directory ~a.  ~
Will treat as having zero size."
                                                  root))
                         (with-native-string (title "Directory Read Error")
                           (win:MessageBox 0 msg title win:MB_OK)))
                       nil))
                   #'(lambda (pathname1 pathname2)
                       (string-lessp (namestring pathname1)
                                     (namestring pathname2)))))
      (cond ((file-directory-p file)
             (setq item (disk-usage-recursive 
                         (concatenate 'string
                           (namestring file) "/")
                         threshold))
             (incf sum (second item))
             (when (> (second item) threshold)
               (push item items)))
            (t
             (setq size (file-length file))
             (when size
               (incf sum size)
               (when (> size threshold)
                 (push (list file size nil) items))))))
    (list root sum (sort items #'> :key #'second))))

(defun disk-usage-print (tree indent)
  ;; Show the directory data as indented plain text.
  (format t "~&~%        KB  Directory or File~%")
  (disk-usage-print-recursive tree indent))

(defun disk-usage-print-recursive (tree indent)
  ;; Recursively print each deeper level of data.
  (let* ((namestring (namestring (first tree)))
         (length (length namestring)))
    (when (eql (aref namestring (1- length)) #\/)
      (setf namestring (subseq namestring 0 (1- length))))
    (format t "~10:d  ~a~a~%"
      (round (second tree) 1000)
      (make-string indent :initial-element #\space)
      (or (file-namestring namestring)
          namestring)) ;; the root dir
    (dolist (item (third tree))
      (disk-usage-print-recursive item (+ indent 2)))))

;;; ------------------------------------------------------------------
;;; Common Graphics Version
;;; When Common Graphics is present, call cg-user::disk-usage to report
;;; disk usage in an outline widget in a dialog.

(defun cg-user::run-disk-usage-example ()
  (cg-user::disk-usage))

(defun cg-user::disk-usage (&key root threshold open-branch-threshold
                                 (use-outline t))
  
  ;; ROOT should be the namestring of a directory to analyze.
  
  ;; THRESHOLD is a number of bytes.  Only directories and files
  ;; accounting for at least that many bytes will appear in the output.
  
  ;; If USE-OUTLINE is non-NIL, then the output is displayed in an outline
  ;; control.  If it is NIL, then the output is written as text.
  
  ;; OPEN-BRANCH-THRESHOLD is a number of bytes.  Directories that are
  ;; smaller than this will initially be shown as a "closed" outline-item,
  ;; which the user must click on to see finer detail within that directory.
  
  (unless root
    (setq root (cg:ask-user-for-directory
                :prompt "Root Directory to Report On")))
  (unless root
    (return-from cg-user::disk-usage))
  (unless threshold
    (multiple-value-bind (s1 s2 bs ok)
        (cg:ask-user-for-string ;; bug13608
         #.(format nil "Minimum size for files and directories ~
                        to include (in bytes).")
         "1000000" "~OK" :~Cancel nil nil
         "Enter Threshold"
         (cg:development-main-window cg:*system*))
      (declare (ignore s2 bs))
      (when ok (setq threshold (read-from-string s1)))))
  (unless threshold
    (return-from cg-user::disk-usage))
  (unless open-branch-threshold
    (multiple-value-bind (s1 s2 bs ok)
        (cg:ask-user-for-string ;; bug13608
         #.(format nil "Minimum size for a directory ~
                        to be shown \"open\" (in bytes).")
         "10000000" "~OK" :~Cancel nil nil
         "Enter Threshold"
         (cg:development-main-window cg:*system*))
      (declare (ignore s2 bs))
      (when ok (setq open-branch-threshold (read-from-string s1)))))
  (unless open-branch-threshold
    (return-from cg-user::disk-usage))
  (let* ((window nil))
    (when root
      (cg:with-hourglass
        (let* ((tree (disk-usage-recursive root threshold)))
          (if use-outline
              (setq window (disk-usage-outline tree threshold
                                               open-branch-threshold))
            (disk-usage-print tree 0)))))
    
    ;; Return the window from this project's on-initialization
    ;; function so that a standalone executable can know to exit 
    ;; when the window is closed.
    window))

;;; Subclass the outline control and its associated window class
;;; so that we can define a method on the window class below.
(defclass big-file-outline (cg:outline)())
(defclass big-file-outline-pane (cg:outline-top-pane)())
(defmethod cg:widget-device ((dialog-item big-file-outline) dialog)
  (declare (ignore dialog))
  'big-file-outline-pane)

;;; Make the delete key offer to delete the selected file or directory.
(defmethod cg:delete-selection ((window big-file-outline-pane))
  (let* ((outline (cg:dialog-item window))
         (values (cg:value outline))
         (files (mapcar #'first values))
         (type (and files
                    (if (some #'file-directory-p files)
                        (if (every #'file-directory-p files)
                            (if (rest files)
                                "directories"
                              "directory")
                          "files and directories")
                      (if (rest files) "files" "file")))))
    (when files
      (when (cg:y-or-n-dialog "Really delete the ~a ~s?" type files)
        (dolist (value values)
          (when (probe-file (first value))
            (if* (file-directory-p (first value))
               then (delete-directory-and-files (first value))
               else (delete-file (first value)))
            (cg:remove-outline-item-value outline value)))))))

;;; A more general way to define keystrokes on the outline window
(defmethod cg:virtual-key-down ((window big-file-outline-pane)
                                buttons key-code)
  (declare (ignore buttons))
  (case key-code
    (#.cg:vk-return
     (let* ((file (caar (cg:value (cg:dialog-item window)))))
       (when (cg:y-or-n-dialog "Call run-shell-command on ~a?" file)
         (funcall 'run-shell-command file))))
    (#.cg:vk-delete (cg:delete-selection window))
    (t (call-next-method))))

(defun disk-usage-outline (tree threshold open-branch-threshold)
  
  ;; Make a dialog with an outline control to display the directory data.
  (let* ((window (cg:make-window :disk-usage
                   :class 'cg:dialog
                   :left 100
                   :width (- (cg:interior-width (cg:screen cg:*system*))
                             100)
                   :height 500
                   :title (format nil "~
            Size in Kilobytes of Files & Directories Over ~a KB ~
            in ~a     The DELETE key deletes the selected items ~
            (if confirmed)."
                            (round threshold 1000)
                            (first tree))
                   :state :shrunk))
         
         ;; Make the outline control.
         (outline (make-instance 'big-file-outline
                    :name :outline
                    :range nil
                    :multiple-selections t
                    
                    ;; Tell delete-selection that it's OK to
                    ;; delete things from this outline.
                    :cuttable t
                    
                    ;; Make the outline display the second thing
                    ;; in the list for each value, so that we can
                    ;; remember the whole pathname as the first thing
                    ;; in the list
                    :on-print #'second
                    
                    ;; Specify the pixmaps to be used in the outline.
                    ;; These file-folder pixmaps are actually the default,
                    ;; but are specified here anyway for reference
                    :opened-pixmap (cg:find-pixmap :default-opened)
                    :closed-pixmap (cg:find-pixmap :default-closed)
                    :leaf-pixmap (cg:find-pixmap :default-leaf)
                    
                    ;; These pixmaps show the blue arrows, as in
                    ;; the IDE's Class Browser and Trace Dialog
                    ;; :opened-pixmap (cg:find-pixmap :opened)
                    ;; :closed-pixmap (cg:find-pixmap :closed)
                    ;; :leaf-pixmap (cg:find-pixmap :leaf)
                    
                    :left 0 :top 0
                    :width (cg:interior-width window)
                    :height (cg:interior-height window)
                    :right-attachment :right
                    :bottom-attachment :bottom)))
    
    ;; Put our computed tree of outline-items into the outline control.
    (setf (cg:range outline)
      (list (disk-usage-outline-recursive tree open-branch-threshold)))
    
    ;; Place the completed outline control onto the dialog, and
    ;; show the dialog.
    (cg:add-component outline window)
    (cg:select-window window)
    
    ;; Return the window so that a standalone executable can
    ;; know to exit when the window is closed.
    window))

(defun disk-usage-outline-recursive (tree open-branch-threshold)
  
  ;; Recursively compute the tree of outline-items to display
  ;; the tree of directory data that we have computed already.
  (let* ((namestring (namestring (first tree)))
         (length (length namestring))
         write-date)
    
    ;; Make sure that each directory namestring ends 
    ;; with a closing slash.
    (when (eql (aref namestring (1- length)) #\/)
      (setf namestring (subseq namestring 0 (1- length))))
    (setq write-date (file-write-date namestring))
    
    ;; Make an outline-item for a particular directory or file.
    (make-instance 'cg:outline-item
      :value (list namestring
                   (format nil "~8:d   ~a   ~a"
                     (round (second tree) 1000)
                     (or (file-namestring namestring)
                         namestring)
                     (if* write-date
                        then (format-universal-time-for-file-write-date
                              write-date
                              :date-only t
                              :include-weekday nil
                              :include-reference-to-now t)
                        else "")))
      
      ;; If this is a big directory, go ahead and show its subdirectories
      ;; and files; otherwise, save space and require the user to open it.
      :state (if (>= (second tree) open-branch-threshold)
                 :open :closed)
      
      ;; If this is a directory, show a file folder icon for it
      ;; even if it is a "leaf" of this outline due to listing
      ;; no files that are in it.
      :leaf-pixmap (and (file-directory-p namestring)
                        (cg:find-pixmap :default-opened))
      
      ;; Make this outline-item point to a list of outline-items
      ;; for the subdirectories and files of this directory.
      :range (mapcar #'(lambda (tree)
                         (disk-usage-outline-recursive
                          tree open-branch-threshold))
               (third tree)))))

(defun format-universal-time-for-file-write-date
    (universal-time &key include-reference-to-now date-only (include-weekday t))
  (multiple-value-bind (second minute hour date month year weekday)
      (decode-universal-time universal-time)
    (multiple-value-bind (now-second now-minute now-hour now-date
                                     now-month now-year)
        (decode-universal-time (get-universal-time))
      (declare (ignore now-hour now-minute now-second))
      (format nil "~a~a ~a ~a~a~a"
        (if* include-weekday
           then (aref #("Monday " "Tuesday " "Wednesday " "Thursday "
                        "Friday " "Saturday " "Sunday ") weekday)
           else "")
        date
        (aref #("Dummy" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)
        year
        (if* date-only
           then ""
           else (format nil " at ~d:~2,'0d:~2,'0d ~a"
                  (1+ (mod (1- hour) 12)) minute second
                  (if* (> hour 12)
                     then "PM"
                     else "AM")))
        (if* include-reference-to-now
           then (format nil " (~a)"
                  (case (- now-year year)
                    (0
                     (case (- now-month month)
                       (0
                        (case (- now-date date)
                          (0 "today")
                          (1 "yesterday")
                          (2 "day before yesterday")
                          (t (format nil "~r days ago"
                               (- now-date date)))))
                       (1 "last month")
                       (t (format nil "~r months ago"
                            (- now-month month)))))
                    (1 "last year")
                    (t (format nil "~r years ago"
                         (- now-year year)))))
           else "")))))

