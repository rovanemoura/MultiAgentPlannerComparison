#|
======================
File: diret-subscription-functions.lisp
Authors: Vladimir Kulyukin
Copyright (c) 1999

Comments and bugs to vkulyukin@cs.depaul.edu  
======================
|#

(in-package :cg-user)

(defun subscribe-source (button new old)
   (declare (ignore old))
   (let* ((diret-sw (parent button))
          (item (value (find-component :source-list diret-sw))))
      (if (cl-user::subscribed-source-p cl-user:*diret-client* item)
         (source-message diret-sw "This source is already subscribed to.")
         (progn
          (cl-user:activate-source-proxy cl-user:*diret-client* item)
          (source-message diret-sw "Subscription has been accepted.")))))

(defun unsubscribe-source (button new old)
   (declare (ignore old))
   (let* ((diret-sw (parent button))
          (item (value (find-component :source-list diret-sw))))
      (cl-user:deactivate-source-proxy cl-user:*diret-client* item)
      (source-message diret-sw "Subscription has been removed.")))

(defun diret-sources ()
  `(:stocks :bonds :cash))

(defun source-list-on-change (widget new old)
  (declare (ignore old))
  (let ((dialog (parent widget)))
    (when new
	  (source-message dialog
	     (remove #\return (source-description new))))
    t))

(defun source-message (dialog message)
  (setf (value (find-component :source-description dialog))
        (substitute #\space #\newline message)))

(defun source-description (name)
  (case name
    (:stocks "Collection of documents about investing
in stock mutual funds.")
    (:bonds "Collection of documents about investing
in bond mutual funds.")
    (:cash "Collection of documents about investing
in mutual funds that manage cash.")))

(defun source-list-key (item)
  (source-short-description item))

(defun source-short-description (name)
  (case name
    (:stocks "Stock mutual funds")
    (:bonds  "Bond  mutual funds")
    (:cash   "Cash  mutual funds")))

(defun diret-subscription-dialog ()
  (let* ((sw (make-diret-subscription-window))
         (source-list (find-component :source-list sw)))
     (select-window sw)
     (setf (range source-list) (diret-sources)
           (value source-list) (first (range source-list)))))

;;; end-of-file







