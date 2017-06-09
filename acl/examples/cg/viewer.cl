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


(in-package :cg-user)

(defclass pixmap-viewer (bitmap-window)
  ((draw-mode
    :initarg :draw-mode
    :initform :center
    :accessor draw-mode)
   (pixmap-directory
    :initarg :directory
    :initform nil
    :accessor pixmap-directory)
   (pixmap-paths
    :initform nil
    :accessor pixmap-paths)
   (reverse-pixmap-paths
    :initform nil
    :accessor reverse-pixmap-paths)
   (pixmap-path
    :initform nil
    :accessor pixmap-path)
   ))

(defparameter *random-mode* nil)

(defparameter *pixmap-viewer-timer* nil)

(defparameter *pixmap-viewer-interval* 5200)

(defclass pixmap-viewer-pane (bitmap-pane)())

(defmethod default-pane-class ((window pixmap-viewer))
  'pixmap-viewer-pane)

(defun run-viewer-example ()
  
  ;; Set the initial directory to browse to be the Windows directory
  ;; rather than an IDE directory, since the latter won't exist
  ;; in a standalone application.
  (pixmap-viewer :directory
                 #+gtk (cg.base:canonical-path ".")
                 #-gtk (sys::windows-directory)))

(defun pixmap-viewer (&key directory)
  (when directory
    (setq directory (pathname-as-directory directory)))
  (borrow-some-ide-pixmaps)
  (let* ((frame (make-window :pixmap-viewer
                  :class 'pixmap-viewer
                  :title "Pixmap Viewer"
                  :interior (make-box 20 200 780 680)
                  :resizable t
                  :scrollbars nil
                  :state :shrunk))
         (multipic-width 460)
         (multipic (make-instance 'multi-picture-button
                     :name :multipic
                     :left *toolbar-margin*
                     :top *toolbar-margin*
                     :width multipic-width :height 24
                     :on-change 'pixmap-viewer-multipic-on-change
                     :multiple-selections t
                     :value (list :center)
                     :range
                     (list
                      (make-instance 'button-info
                          :name :directory
                          :tooltip "Browse New Directory"
                          :pixmap-name :find-in-file)
                      (make-instance 'button-info
                          :name :browse-sibling-directory
                          :pixmap-name :right)
                      (make-instance 'button-info
                          :name :browse-parent-directory
                          :pixmap-name :up)
                      (make-instance 'button-info
                          :name :browse-subdirectory
                          :pixmap-name :down)
                      :gap
                      (make-instance 'button-info
                          :name :pack
                          :tooltip "Pack (P)"
                          :string "Pk")
                      (make-instance 'button-info
                          :name :center
                          :tooltip "Center (C)"
                          :string "Cn")
                      (make-instance 'button-info
                          :name :replicate
                          :tooltip "Replicate (R)"
                          :string "Rp")
                      (make-instance 'button-info
                          :name :strip
                          :tooltip "Horizontal Strip (H)"
                          :string "Ho")
                      (make-instance 'button-info
                          :name :stretch
                          :tooltip "Stretch (S)"
                          :string "Str")
                      :gap
                      (make-instance 'button-info
                          :name :delete-file
                          :pixmap-name :abort)
                      (make-instance 'button-info
                          :name :move-to-sibling-directory
                          :tooltip "Move Pixmap to Sibling Directory"
                          :pixmap-name :right)
                      (make-instance 'button-info
                          :tooltip "Move Pixmap to Parent Directory"
                          :name :move-to-parent-directory
                          :pixmap-name :up)
                      (make-instance 'button-info
                          :name :move-to-subdirectory
                          :tooltip "Move Pixmap to Subdirectory"
                          :pixmap-name :down)
                      (make-instance 'button-info
                          :name :rename
                          :tooltip "Rename File (N)"
                          :pixmap-name 'static-text)
                      :gap
                      (make-instance 'button-info
                          :name :previous-pixmap
                          :tooltip
                          "Show Previous Pixmap (D or up arrow or right click)"
                          :pixmap-name :back)
                      (make-instance 'button-info
                          :name :next-pixmap
                          :tooltip
                          "Show Next Pixmap (F or down arrow or left click)"
                          :pixmap-name :forward)
                      :gap
                      (make-instance 'button-info
                          :name :auto-run
                          :tooltip "Auto-Run (G)"
                          :pixmap-name :unprofile)
                      (make-instance 'button-info
                          :name :random
                          :width 40
                          :tooltip "Random or Alphabetical Order (X)"
                          :title "Alpha")
                      )))
         (file-combo (make-instance 'combo-box
                       :name :file-combo
                       :left (+ *toolbar-margin*
                                multipic-width
                                6)
                       :top *toolbar-margin*
                       :width 250
                       :height 500
                       :on-change 'pixmap-viewer-file-combo-on-change
                       :on-print #'(lambda (value)
                                     (if* value
                                        then (file-namestring value)
                                        else ""))
                       :range nil
                       :value nil))
         (toolbar (make-window :toolbar
                    :class 'toolbar
                    :owner frame
                    :widgets (list multipic
                                   file-combo)))
         )
    (setq *random-mode* nil)
    (add-toolbar frame toolbar)
    (add-common-status-bar frame)
    (setf (stretch-mode (frame-child frame)) :halftone)
    (pixmap-viewer-set-directory frame directory)
    (unless (pixmap-directory frame)
      (close frame)
      (return-from pixmap-viewer))
    (select-window frame)
    frame))

(defmethod resize-window :after ((window pixmap-viewer) position)
  (declare (ignore position))
  (let* ((pane (frame-child window)))
    (when (windowp pane)
      (set-page-size pane
                     (interior-width pane)
                     (interior-height pane))
      (pixmap-viewer-refresh window))))

(defmethod set-focus ((window pixmap-viewer))
  (set-focus (frame-child window)))

(defun pixmap-viewer-multipic-on-change (multipic new old)
  (let* ((window (parent (parent multipic)))
         (pane (frame-child window)))
    (set-focus pane)
    
    ;; Take an action only if the user has pressed a button,
    ;; and not when we programmatically unpress some incompatible
    ;; buttons here.
    (or (<= (length new)(length old))
        
        (case (first new)
          (:directory
           (pixmap-viewer-set-directory window nil)
           nil) ;; release the button
          (:browse-sibling-directory
           (pixmap-viewer-browse-sibling-directory window)
           nil)
          (:browse-parent-directory
           (pixmap-viewer-browse-parent-directory window)
           nil)
          (:browse-subdirectory
           (pixmap-viewer-browse-subdirectory window)
           nil)
          (:center
           (setf (draw-mode window) :center)
           (setf (value multipic)
             (set-difference new '(:pack :replicate :strip :stretch)))
           (pixmap-viewer-refresh window)
           t)
          (:pack
           (setf (draw-mode window) :pack)
           (setf (value multipic)
             (set-difference new '(:center :replicate :strip :stretch)))
           (pixmap-viewer-refresh window)
           t)
          (:replicate
           (setf (draw-mode window) :replicate)
           (setf (value multipic)
             (set-difference new '(:center :pack :strip :stretch)))
           (pixmap-viewer-refresh window)
           t)
          (:strip
           (setf (draw-mode window) :strip)
           (setf (value multipic)
             (set-difference new '(:center :pack :replicate :stretch)))
           (pixmap-viewer-refresh window)
           t)
          (:stretch
           (setf (draw-mode window) :stretch)
           (setf (value multipic)
             (set-difference new '(:center :pack :replicate :strip)))
           (pixmap-viewer-refresh window)
           t)
          (:delete-file
           (pixmap-viewer-delete window)
           nil)
          (:move-to-sibling-directory
           (pixmap-viewer-move-to-sibling-directory window)
           nil)
          (:move-to-parent-directory
           (pixmap-viewer-move-to-parent-directory window)
           nil)
          (:move-to-subdirectory
           (pixmap-viewer-move-to-subdirectory window)
           nil)
          (:rename
           (pixmap-viewer-rename window)
           nil)
          (:next-pixmap
           (pixmap-viewer-keyboard-command pane vk-down)
           nil)
          (:previous-pixmap
           (pixmap-viewer-keyboard-command pane vk-up)
           nil)
          (:auto-run
           (pixmap-viewer-keyboard-command pane #.(char-code #\G))
           nil)
          (:random
           (pixmap-viewer-keyboard-command pane #.(char-code #\X))
           nil)
          ))))

(defun remove-current-pixmap-from-current-set (window)
  (let* ((toolbar (find-window :toolbar window))
         (combo (find-component :file-combo toolbar))
         (index (list-widget-get-index combo))
         (range (range combo))
         (path (nth index range)))
    
    ;; If we're looking at the last pixmap, then move to
    ;; the previous one; otherwise move to the next one
    (if* (and (plusp index)
              (>= index (1- (length range))))
       then (setf (value combo)(nth (1- index) range))
       else (setf (value combo)(nth (1+ index) range)))
    
    ;; Remove the current pixmap
    (list-widget-remove-item combo index)
    (setf (reverse-pixmap-paths window)
      (delete path (reverse-pixmap-paths window)
              :key #'first
              :test #'excl::pathname-equalp))
    ))

(defun emerge-call-path (pixmap-path)
  (probe-file (merge-pathnames (make-pathname :type "emr")
                               pixmap-path)))

(defun pixmap-viewer-delete (window)
  (let* ((pixmap-path (pixmap-path window)))
    (when (and (probe-file pixmap-path)
               (y-or-n-dialog "Really delete the file ~a?"
                              pixmap-path))
      (remove-current-pixmap-from-current-set window)
      (delete-file pixmap-path)
      (window-message window "DELETED pixmap file ~a." pixmap-path)
      (let* ((emr-path (emerge-call-path pixmap-path)))
        (when emr-path
          (delete-file emr-path))))))

(defun ask-user-for-subdirectory
    (path &key
          (title "Choose Directory")
          (stream (or (selected-window (screen *system*))
                      (screen *system*)))
          (directory-choices
           (cg.directory-dialog:directory-subdirectories-plat
            (excl::path-pathname path)))
          initial-value)
  (declare (ignore initial-value))
  (window-message stream title)
  (if* directory-choices
     then
          (let* ((choice (pop-up-lettered-menu
                          (mapcar #'namestring directory-choices)
                          :sortp t :sort-predicate #'string-lessp)))
            (and choice
                 (pathname choice)))
     else
          #-runtime-system
          (window-message stream "There are no subdirectories.")
          nil))

(defun pixmap-viewer-move-to-directory (pixmap-path directory window)
  (when directory
    (rename-file pixmap-path directory)
    (window-message window "MOVED ~a into ~a." pixmap-path directory)
    (let* ((emr-path (emerge-call-path pixmap-path)))
      (when emr-path
        (rename-file emr-path directory)))))
    
(defun pixmap-viewer-move-to-sibling-directory (window)
  (let* ((pixmap-path (pixmap-path window))
         (directory (pixmap-viewer-ask-for-sibling-directory
                     window
                     "Sibling Directory to Move the Pixmap Into")))
    (when directory
      (remove-current-pixmap-from-current-set window)
      (pixmap-viewer-move-to-directory
       pixmap-path directory window))))

(defun pixmap-viewer-ask-for-sibling-directory (window title)
  (let* ((directory (pixmap-directory window)))
    (ask-user-for-subdirectory
     (make-pathname
      :host (pathname-host directory)
      :device (pathname-device directory)
      :directory (butlast
                  (pathname-directory directory)
                  1))
     :initial-value directory
     :title title)))

(defun pixmap-viewer-move-to-parent-directory (window)
  (let* ((pixmap-path (pixmap-path window)))
    (when (and (probe-file pixmap-path)
               (y-or-n-dialog "Move the pixmap ~a to its parent directory?"
                              pixmap-path))
      (remove-current-pixmap-from-current-set window)
      (pixmap-viewer-move-to-directory
       pixmap-path (pixmap-viewer-parent-directory pixmap-path)
       window))))

(defun pixmap-viewer-parent-directory (path)
  (make-pathname
   :host (pathname-host path)
   :device (pathname-device path)
   :directory (butlast (pathname-directory path) 1)))

(defun pixmap-viewer-move-to-subdirectory (window)
  (let* ((pixmap-path (pixmap-path window))
         (directory (pixmap-viewer-ask-for-subdirectory
                     window
                     "Subirectory to Move Pixmap Into")))
    (when directory
      (remove-current-pixmap-from-current-set window)
      (pixmap-viewer-move-to-directory
       pixmap-path directory window))))

(defun pixmap-viewer-ask-for-subdirectory (window title)
  (ask-user-for-subdirectory
   (excl::path-pathname (pixmap-directory window))
   :title title))

(defun pixmap-viewer-rename (window)
  (let* ((pixmap-path (pixmap-path window))
         (toolbar (find-window :toolbar window))
         (combo (find-component :file-combo toolbar))
         new-path index)
    (when pixmap-path
      (multiple-value-bind (new-name other-string button-label)
          (ask-user-for-string
           "New file name?  (Do not include the path or extension.)"
           (pathname-name pixmap-path)
           "~OK" "~Cancel")
        (declare (ignore other-string))
        (when (and (string= button-label "~OK")
                   (plusp (length new-name)))
          (setq new-path (merge-pathnames new-name pixmap-path))
          (when (or (not (probe-file new-path))
                    (y-or-n-p "The file ~a already exists.  ~
                               Replace it?"
                              new-path))
            (rename-file pixmap-path new-path)
            (let* ((emr-path (emerge-call-path pixmap-path)))
              (when emr-path
                (rename-file emr-path (merge-pathnames new-name
                                                       emr-path))))
            (window-message window "Renamed ~a to ~a."
              pixmap-path new-path)
            (setq index (position pixmap-path (range combo)
                                  :test #'excl::pathname-equalp))
            (when index
              (setf (pixmap-path window) new-path)
              (list-widget-replace-item combo new-path index))
            ))))))
 
(defun pixmap-viewer-file-combo-on-change (combo new old)
  (declare (ignore old))
  (let* ((window (parent (parent combo))))
    (pixmap-viewer-view-file window new)
    (set-focus (frame-child window)))
  t)

(defvar *pixmap-viewer-path-to-pixmap* nil)

(defun pixmap-viewer-set-directory (window directory)
  (unless directory
    (setq directory
          (ask-user-for-directory
           :stream window
           :prompt
           "Select a directory with pixmap files in it.")))
  (unless directory
    (return-from pixmap-viewer-set-directory nil))
  (setf (title window)
    (format nil "Pixmap Viewer     ~a"
      (lowercase-object (namestring directory))))
  (let* ((toolbar (find-window :toolbar window))
         (file-combo (find-component :file-combo toolbar))
         (raw-files
          (remove-if-not (lambda (pathname)
                           (member (pathname-type pathname)
                                   '("bmp" "png" "jpg" "jpeg" "gif" "ico"
                                     "tif" "tiff" "exif" "wmf" "emf")
                                   :test #'string-equal))
                         (if* (and directory
                                   (pixmap-directory window)
                                   (excl::pathname-equalp
                                    directory
                                    (pixmap-directory window)))
                            then (range file-combo)
                            else (directory (merge-pathnames
                                             "*.*" directory)))))
         (files (if* *random-mode*
                   then (setq *random-state* (make-random-state t))
                        (mapcar #'first
                          (sort (mapcar #'(lambda (file)
                                            (list file (random 1000000)))
                                  raw-files)
                                #'< :key #'second))
                   else (sort (copy-list raw-files)
                              #'(lambda (p1 p2)
                                  (string-lessp (namestring p1)
                                                (namestring p2))))))
         )
    (setf (pixmap-directory window) directory)
    (setf (reverse-pixmap-paths window)
      (reverse
       (setf (pixmap-paths window)
         (mapcar #'(lambda (path)
                     (list path (load-texture-info path nil t)))
           files))))
    (setf (range file-combo) files)
    (setf (value file-combo) (first files))
    ))

(defun pixmap-viewer-browse-sibling-directory (window)
  (let* ((directory (pixmap-viewer-ask-for-sibling-directory
                     window
                     "Sibling Directory to Browse")))
    (when directory
      (pixmap-viewer-set-directory window directory))))

(defun pixmap-viewer-browse-parent-directory (window)
  (let* ((directory (pixmap-directory window))
         (parent-directory (pixmap-viewer-parent-directory
                            directory)))
    (when t #+maybe (y-or-n-dialog "Browse the parent directory ~a?"
                                   directory)
      (pixmap-viewer-set-directory window parent-directory))))

(defun pixmap-viewer-browse-subdirectory (window)
  (let* ((directory (pixmap-viewer-ask-for-subdirectory
                     window "Subirectory to Browse")))
    (when directory
      (pixmap-viewer-set-directory window directory))))

(defun pixmap-viewer-get-pixmap (path)
  (load-pixmap path :use-gdi-plus t)
 
  #+maybe
  ;; This caches pixmaps so each one only gets loaded
  ;; a single time, but this uses a lot of memory to
  ;; hold all of the pixmaps.
  (or (second (assoc path *pixmap-viewer-path-to-pixmap*
                     :test #'excl::pathname-equalp))
      (let* ((pixmap (load-pixmap path :use-gdi-plus t)))
        (push (list path pixmap) *pixmap-viewer-path-to-pixmap*)
        pixmap)))

(defun pixmap-viewer-refresh (window)
  (let* ((path (pixmap-path window)))
    (when path
      (pixmap-viewer-view-file window path))))

(defun pixmap-viewer-view-file (window path)
  (unless path
    (clear-page (frame-child window))
    (window-message
        window
        "There are no pixmap files in this directory.")
    (return-from pixmap-viewer-view-file))
  (let* ((pane (frame-child window))
         (page-width (page-width pane))
         (page-height (page-height pane))
         (draw-mode (draw-mode window))
         (pack-mode (eq draw-mode :pack))
         (pixmap (if* pack-mode
                    then (load-texture-info path nil t)
                    else (pixmap-viewer-get-pixmap path)))
         (width (width pixmap))
         (height (height pixmap))
         (colors (colors pixmap))
         (centering-left (floor (- page-width width) 2))
         (centering-top (floor (- page-height height) 2)))
    (when (< (bits-per-pixel (screen *system*)) 16)
      (close-palette pane (palette pane))
      (setf (palette pane)
        (if* colors
           then (open-palette pane colors)
           else :rgb)))
    (window-message
        window "Width ~a   Height ~a   Depth ~a   Colors ~a"
      width height (bits-per-pixel pixmap)
      (if colors (length colors) "True"))
    (setf (pixmap-path window) path)
    (case draw-mode
      (:pack
       (let* ((pics (pack-pixmaps
                     (member path (reverse-pixmap-paths window)
                             :key #'first
                             :test #'excl::pathname-equalp)
                     (interior-width pane)
                     (interior-height pane))))
         (if* pics
            then (with-delayed-redraw (pane :invalidate t)
                   (clear-page pane)
                   (dolist (pic pics)
                     (copy-to-stream (fourth pic) pane
                                     (make-position (second pic)
                                                    (third pic)))))
            else (copy-to-stream pixmap pane #.(make-position 0 0)))))
      (:stretch
       (clear-page pane)
       (let* ((x-factor (/ page-width width))
              (y-factor (/ page-height height))
              (factor (if (> x-factor y-factor) y-factor x-factor))
              (bwidth (floor (* width factor)))
              (bheight (floor (* height factor))))
         (copy-to-stream
          pixmap pane
          (make-box-relative (round (- page-width bwidth) 2)
                             (round (- page-height bheight) 2)
                             bwidth bheight))))
      (:center
       (clear-page pane)
       (copy-to-stream pixmap pane
                       (make-position centering-left
                                      centering-top)))
      (:replicate
       (with-delayed-redraw (pane :invalidate t)
         (do* ((x (- centering-left
                     (* width (ceiling centering-left width)))
                  (+ x width)))
              ((>= x page-width))
           (do* ((y (- centering-top
                       (* height (ceiling centering-top height)))
                    (+ y height)))
                ((>= y page-height))
             (copy-to-stream pixmap pane
                             (make-position x y))))))
      (:strip
       (with-delayed-redraw (pane :invalidate t)
         (clear-page pane)
         (do* ((x (- centering-left
                     (* width (ceiling centering-left width)))
                  (+ x width)))
              ((>= x page-width))
           (copy-to-stream pixmap pane
                           (make-position x centering-top)))))
      )))

(defmethod virtual-key-down ((pane pixmap-viewer-pane)
                             buttons key)
  (declare (ignore buttons))
  (pixmap-viewer-keyboard-command pane key))

(defun pixmap-viewer-keyboard-command (pane key)
  (let* ((window (parent pane))
         (toolbar (find-window :toolbar window))
         (multipic (find-component :multipic toolbar))
         (file-combo (find-component :file-combo toolbar))
         (current-value (value file-combo))
         (all-values (range file-combo)))
    (case key
      ((#.vk-up #.(char-code #\D))
       (setf (value file-combo)
         (and current-value
              (let* ((new-pos (1- (position current-value all-values
                                            :test #'eq))))
                (if* (minusp new-pos)
                   then (first (last all-values))
                   else (nth new-pos all-values)))))
       #+maybe ;; doesn't fire the on-change
       (list-widget-set-index
        file-combo
        (1+ (list-widget-get-index file-combo))))
      ((#.vk-down #.(char-code #\F))
       (setf (value file-combo)
         (and current-value
              (let* ((new-pos (1+ (position current-value all-values
                                            :test #'eq))))
                (if* (< new-pos (length all-values))
                   then (nth new-pos all-values)
                   else (first all-values)))))
       #+maybe ;; doesn't fire the on-change
       (list-widget-set-index
        file-combo
        (1- (list-widget-get-index file-combo))))
      (#.vk-home
       (setf (value file-combo)
         (nth 0 (range file-combo))))
      (#.vk-end
       (setf (value file-combo)
         (nth (max 0 (1- (length all-values)))
              all-values)))
      (#.(char-code #\C)
          (pixmap-viewer-push-multipic-button multipic :center))
      (#.(char-code #\S)
          (pixmap-viewer-push-multipic-button multipic :stretch))
      (#.(char-code #\R)
          (pixmap-viewer-push-multipic-button multipic :replicate))
      (#.(char-code #\H)
          (pixmap-viewer-push-multipic-button multipic :strip))
      (#.(char-code #\P)
          (pixmap-viewer-push-multipic-button multipic :pack))
      
      (#.(char-code #\N)
          (pixmap-viewer-rename window))
      (#.(char-code #\G)
          (let* ((timer (pixmap-viewer-timer pane)))
            (unless (active timer)
              (multiple-value-bind (interval string2 button proceed)
                  (ask-user-for-string 
                   "Enter milliseconds to pause on each view."
                   (princ-to-string *pixmap-viewer-interval*)
                   "~OK" "~Cancel" nil nil "Timer Interval" window)
                (declare (ignore string2 button))
                (when proceed
                  (setq interval (ignore-errors
                                  (read-from-string interval)))
                  (when (and (integerp interval)
                             (plusp interval))
                    (setf *pixmap-viewer-interval* interval)
                    (setf (interval timer) *pixmap-viewer-interval*)))
                (pixmap-viewer-keyboard-command
                 pane #.(char-code #\F))))
            (setf (active timer)(not (active timer)))
            (setf (pixmap (find :auto-run (range multipic)
                                :key #'name :test #'eq))
              (find-pixmap
               (if (active timer) :profile :unprofile)))))
      (#.(char-code #\X)
          (setq *random-mode* (not *random-mode*))
          (setf (title (find :random (range multipic)
                             :key #'name :test #'eq))
            (if *random-mode* "Rand" "Alpha"))
          (pixmap-viewer-set-directory
           window (pixmap-directory window)))
      )))

(defun pixmap-viewer-push-multipic-button (multipic button-name)
  (let* ((multipic-value (value multipic)))
    (pixmap-viewer-multipic-on-change
     multipic
     (cons button-name multipic-value)
     multipic-value)))

(excl:without-package-locks
 (defmethod mouse-left-down ((pane pixmap-viewer-pane)
                             buttons cursor-position)
   (declare (ignore buttons cursor-position))
   (pixmap-viewer-keyboard-command pane vk-down)))

(excl:without-package-locks
 (defmethod mouse-double-click ((pane pixmap-viewer-pane)
                                buttons cursor-position)
   (declare (ignore buttons cursor-position))
   (pixmap-viewer-keyboard-command pane vk-down)))

(excl:without-package-locks
 (defmethod mouse-right-down ((pane pixmap-viewer-pane)
                              buttons cursor-position)
   (declare (ignore buttons cursor-position))
   (pixmap-viewer-keyboard-command pane vk-up)))

(excl:without-package-locks
 (defmethod mouse-right-double-click ((pane pixmap-viewer-pane)
                                      buttons cursor-position)
   (declare (ignore buttons cursor-position))
   (pixmap-viewer-keyboard-command pane vk-up)))

(defvar *old-pics* nil)

(defun pack-pixmaps (path-list window-width window-height
                               &key (max-to-check 24))
 
  ;; This packer does not bother to look ahead, instead just
  ;; placing each one and then proceeding to the next one.
  ;; Still does a fair job though.
  (let* ((answer nil)
         (picture-boxes nil)
         (rights (list 0))
         (bottoms (list 0))
         (old-pics nil)
         (first-one t)
         (misfits nil)
         texture-info test-box left top width height
         picture-box pixmap filename path path-thing)
    (do* ((path-list path-list (rest path-list))
          (j 0 (1+ j)))
         ((or (null path-list)
              (>= j max-to-check)))
      (setq path-thing (first path-list))
      (setq path (first path-thing))
      (setq texture-info (second path-thing))
      (setq width (texture-info-width texture-info))
      (setq height (texture-info-height texture-info))
      (setq left nil top nil)
      
      ;; If this pixmap is larger in both directions
      ;; than an earlier pixmap that didn't fit, than
      ;; we know that this one won't fit either.
      (unless (dolist (pair misfits)
                (and (> width (first pair))
                     (> height (second pair))))
        
        (dolist (right rights)
          (dolist (bottom bottoms)
            (setq test-box (make-box-relative
                            right bottom width height))
            (unless (or
                     
                     ;; If this picture extends off the right or
                     ;; bottom edge of the window interior, then
                     ;; it doesn't fit in this position.
                     (and (not first-one)
                          (or (> (box-right test-box) window-width)
                              (> (box-bottom test-box) window-height)))
                     
                     ;; If this pixmap overlaps an already-placed pixmap,
                     ;; then it doesn't fit.
                     (dolist (box picture-boxes)
                       (when (box-intersect-p box test-box)
                         (return t)))
                     
                     ;; If this pixmap fits here, but is further away from
                     ;; the upper left corner than another position where
                     ;; it fits, then use the other position instead.
                     (and left top
                          (> (+ right bottom)
                             (+ left top))))
              
              ;; This pixmap fits, so add to the set to show.
              (setq picture-box test-box)
              (setq left right top bottom)))))
      
      (if* left
         then (push picture-box picture-boxes)
              (push (+ left width) rights)
              (push (+ top height) bottoms)
              (setq filename (pathname-name path))
              (setq pixmap (or (second (assoc filename *old-pics*
                                              :test #'string=))
                               (load-pixmap path :use-gdi-plus t)))
              (push (list path left top pixmap) answer)
              (push (list filename pixmap) old-pics)
              
         else ;; This pixmap doesn't fit anywhere.
              (unless (dolist (misfit misfits)
                        (when (and (>= width (first misfit))
                                   (>= height (second misfit)))
                          (return t)))
                (push (list width height) misfits)))
      (setq first-one nil))
    (setq *old-pics* old-pics)
    answer))

(defun pixmap-viewer-timer (pane)
  (unless *pixmap-viewer-timer*
    (setq *pixmap-viewer-timer*
          (make-instance 'timer
            :name :pixmap-viewer-timer
            :on-timer 'pixmap-viewer-on-timer
            :interval *pixmap-viewer-interval*)))
  (setf (timer-info *pixmap-viewer-timer*) pane)
  *pixmap-viewer-timer*)

(defun pixmap-viewer-on-timer (timer)
  (let* ((pane (timer-info timer)))
    (if* (windowp pane)
       then (pixmap-viewer-keyboard-command
             pane #.(char-code #\F))
       else (stop-timer timer))))

(eval-when (compile load eval)
  (defparameter *ide-pixmaps-to-borrow*
    '(:find-in-files :right :up :down :abort static-text
                     :back :forward :unprofile)))

;;; Here's a little trick for making a standalone CG app borrow 
;;; some IDE pixmaps that aren't otherwise in a CG app.  Edit
;;; the defparameter above to include the names of the desired
;;; IDE pixmaps, and then include this function in your app.
;;; The code inside the #. reader macro form will cause the IDE
;;; pixmaps to be embedded in this function when the application
;;; code is is compiled in the IDE where the pixmaps are present.
;;; Then the image is built with this function in it
;;; that has the embedded the pixmap objects.  (This is possible
;;; because there is a make-load-form method for the pixmap class.)
;;; Running the generated image calls this function to cache
;;; the pixmap objects (by calling restore-pixmasp) so that code
;;; above can find the pixmaps from their names.
(defun borrow-some-ide-pixmaps ()
  (apply #'restore-pixmaps
         '#.(mapcar #'(lambda (name)(find-pixmap name))
              *ide-pixmaps-to-borrow*)))

#+demo
(run-viewer-example)
