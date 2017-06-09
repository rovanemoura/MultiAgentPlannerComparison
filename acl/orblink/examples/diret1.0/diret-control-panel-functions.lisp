#|
==============================
File: diret-control-panel.lisp 
Authors: Vladimir Kulyukin, Alex Lifshits 
Description: Support functions for Diret's Control Panel.
             Uses techniques from several example files 
             provided with the distribution of ACL 5.0 
             Enterprise Edition.

Comments and bugs to vkulyukin@cs.depaul.edu 
============================== 
|# 

(in-package :cg-user)

(eval-when (compile eval load)
  (intern 'show-retrievals :cg-user)
  )

(defparameter *entered-text* (make-hash-table))

(defclass diret-edit-window (text-edit-window) ())

(defclass diret-edit-pane (text-edit-pane) ())

(defmethod default-pane-class ((window diret-edit-window))
  'diret-edit-pane)

(defun diret ()
   (let ((cp (diret-control-panel)))
      (setf *diret-cp* cp)
      (select-window *diret-cp*)
      (stop-query-engine)
      (cl-user::start-diret)
      t))

(defparameter *my-comtab* 
  (make-instance 'comtab
    :inherit-from (raw-text-edit-comtab *system*)))

(defmethod device-open ((window diret-edit-pane) slot-names initargs)
   (declare (ignore slot-names initargs))
   (when (call-next-method)
      (setf (comtab window) *my-comtab*))
   t)

(defun read-diret-text (pane)
   "Reads text line by line and saves each line in *entered-text*."
   (let ((input nil))
      (setf input (text-line pane (current-line-number pane)))
      (setf (gethash (current-line-number pane) *entered-text*) input)
      (window-message (parent pane) "Entered: ~s" input)
      (te::end-of-line pane)
      (terpri pane)))

(defun entered-text (ht)
   (loop with total-text = ""
         with new-line   = (string #\newline)
         for line-number being the hash-key of ht
         for text being the hash-value of ht
         do (setf total-text
                  (concatenate 'string total-text new-line text))
         finally (return total-text)))

(set-event-function *my-comtab* pc:vk-return 'read-diret-text)

(defmethod virtual-key-down ((pane diret-edit-pane) buttons data)
   (declare (ignore buttons))
   (if (eq data vk-enter)
      (read-diret-text pane)
      (call-next-method)))

(defun display-entered-text (dialog widget)
   (declare (ignore-if-unused dialog widget))
   (setf (value (find-component :stxt :diret-form))
         (entered-text *entered-text*))
   t)

(defun invoke-document-processor (dialog widget)
   (declare (ignore-if-unused dialog widget))
   (diret-doc-dialog))

(defun invoke-diret-editor (dialog widget)
   (declare (ignore-if-unused dialog widget))
   (run-diret-editor)
   t)

(defparameter *editor-window* nil)

(defun run-diret-editor ()
   (let ((frame (make-window :window-read-test
                  :device 'diret-edit-window 
                  :parent (development-main-window *system*)
                  :title "Diret 1.0 Editor"
                  :exterior (make-box 520 240 950 620)
                  :state :normal)))
      (setf *editor-window* frame)
      (clear-entered-text)
      (select-window *editor-window*)
      (stop-query-engine)
      (start-query-engine)))


(defun generate-query ()
   (do ()
       ()
      (cl-user::query-to-retrievals 
       cl-user::*diret-client*
       (sample-entered-text))
      (mp:process-sleep (read-sampling-interval))))

(defun start-query-engine ()
   (mp:process-run-function "Diret1.0 Query Engine"
     #'generate-query))

(defun stop-query-engine ()
   (loop for p in sys:*all-processes*
         when (string= (mp:process-name p) "Diret1.0 Query Engine")
         do (mp:process-kill p)
            (return t)
         finally (return nil)))

(defun entered-text-size ()
   (hash-table-count *entered-text*))

(defun clear-entered-text ()
   (clrhash *entered-text*))

(defun pick-entered-text-sample (sample-size curr-size)
   (assert (> curr-size sample-size))
   (let ((start-line (random (- (1- curr-size) sample-size))))
      (loop with sample  = ""
            with space   = " "
            for ln from start-line upto (1- (+ start-line sample-size))
            do (multiple-value-bind (str bool)
                   (gethash ln *entered-text*)
                  (declare (ignore bool))
                  (setf sample (concatenate 'string sample space str)))
            finally (return sample))))

(let ((text-limit  1000)
      (sample-size 5)
      (sampling-interval 5))
   
   (defun sample-entered-text ()
      (let* ((text-size (entered-text-size))
             (sample (if (> (- (1- text-size) sample-size) 0)
                        (pick-entered-text-sample sample-size text-size)
                        nil))) 
         (when (> text-size text-limit)
            (clear-entered-text))
         sample))
         
   (defun read-text-delta () text-delta)
   
   (defun write-text-delta (td)
      (assert (> td 0))
      (setf text-delta td))
   
   (defun read-text-limit () text-limit)
   
   (defun write-text-limit (tlim)
      (assert (> tlim 0))
      (setf text-limit tlim))
   
   (defun read-sample-size () sample-size)
   
   (defun write-sample-size (ss)
      (assert (> ss 0))
      (setf sample-size ss))
   
   (defun read-sampling-interval ()
      sampling-interval)
   
   (defun write-sampling-interval (si)
      (assert (> si 0))
      (setf sampling-interval si))
   
   )
               
(defun invoke-diret-subscription (dialog widget)
   (declare (ignore-if-unused dialog widget))
   (diret-subscription-dialog))

(defun read-retrivals (path)
   (let ((fileline ""))
      (with-open-file (in path :diretion :input)
        (do ((line (read-line in) (read-line in nil 'eof)))
            ((eq line 'eof)) 
           (setf fileline (concatenate 'string fileline line (string #\newline))))
        fileline)))

(defun toolbar-retrieval-on-click (dialog widget)
   (declare (ignore-if-unused dialog widget))   
   (let ((str (read-retrivals "retrievals.txt")))   
      (setf str (remove-if-not #'(lambda (ch) 
                                   (or (char= ch #\space) 
                                       (alphanumericp ch) (char= ch #\newline))) str))
      (setf (value (find-component :stxt :form1)) str)
      (format t "~A" str))
   t)

(defun diret-cp-message (msg &optional (erase-contents t))
   (let ((dm (find-component :diret-message *diret-cp*)))
      (if erase-contents
         (setf (value dm) msg)
         (setf (value dm)
               (concatenate 'string
                 (value dm)
                 (string #\newline)
                 msg)))))

(defparameter *diret-document-dialog* nil)

(defun diret-sample-documents ()
   '(:sample1 :sample2 :sample3))

(defun document-list-key (item)
  (document-name item))

(defun document-name (name)
  (case name
    (:sample1 "sample-document1.txt")
    (:sample2 "sample-document2.txt")
    (:sample3 "sample-document3.txt")
    ))
    
(defun document-list-on-change (widget new old)
  (declare (ignore old))
  (let ((dialog (parent widget)))
     t))

(defun diret-doc-dialog ()
  (let* ((sw (make-diret-doc-dialog))
         (document-list (find-component :document-list sw)))
     (setf *diret-document-dialog* sw)
     (select-window *diret-document-dialog*)
     (setf (range document-list) (diret-sample-documents)
           (value document-list) (first (range document-list)))))

(defun process-saved-document (button new old)
   (declare (ignore old))
   (let* ((diret-sw (parent button))
          (item (value (find-component :document-list diret-sw)))
          (input nil))
      (cl-user::erase-user-retrievals cl-user::*diret-client*)
      (run-diret-editor)
      (with-open-file (in (saved-doc-path item) :direction :input)
        (do ((ln 0 (1+ ln))
             (line (read-line in nil :eof) (read-line in nil :eof)))
            ((eql line :eof))
           (format *editor-window* "~A" line)
           (force-output *editor-window*)
           (setf input (text-line *editor-window* (current-line-number *editor-window*)))
           (setf (gethash (current-line-number *editor-window*) *entered-text*) input)
           (format *editor-window* "~%")
           (force-output *editor-window*)
           (when (and (> ln 9) (zerop (mod ln 10))
                      (yes-or-no-p "Would you like to take a look at retrievals?"))
              (show-retrievals))
           (sleep 3)))))

(defun saved-doc-path (item)
   (case item
     (:sample1 (concatenate 'string cl-user::*coldir*
                 "sample-document1.txt"))
     (:sample2 (concatenate 'string cl-user::*coldir*
                 "sample-document2.txt"))
     (:sample3 (concatenate 'string cl-user::*coldir*
                 "sample-document3.txt"))
     ))

;;; end-of-file