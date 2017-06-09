#|
======================
File: diret-retrieval-functions.lisp
Authors: Vladimir Kulyukin, Alex Lifshits
Description: Functions to support retrieval GUI. 
Copyright (c) 1999  

Comments and bugs to vkulyukin@cs.depaul.edu 
======================
|#

(in-package :cg-user)

(defparameter *retrievals* nil)

(defun get-retrievals (button new &optional old)
   (let* ((rets-window (make-diret-retrieval-window))
          (rets-list (find-component :retrieval-list rets-window))
          (rets (cl-user::get-retrievals-from-all-sources
                 cl-user::*diret-client*)))
      (setf (range rets-list)
            (loop for k being each hash-key in rets
              collect k)
            (value rets-list)
            (first (range rets-list))
            *retrievals* rets)))

(defun show-retrievals ()
   (let* ((rets-window (make-diret-retrieval-window))
          (rets-list (find-component :retrieval-list rets-window))
          (rets (cl-user::get-retrievals-from-all-sources
                 cl-user::*diret-client*)))
      (setf (range rets-list)
            (loop for k being each hash-key in rets
              collect k)
            (value rets-list)
            (first (range rets-list))
            *retrievals* rets)))

(defun describe-retrieval (button new old)
   (declare (ignore old))
   (let* ((diret-rw (parent button))
          (item (value (find-component :retrieval-list diret-rw))))
      (multiple-value-bind (sid-retid bool)
          (gethash item *retrievals*)
         (declare (ignore bool))
         (setf (value (find-component :retrieval-description diret-rw))
               (cl-user::describe-data-item-fully cl-user::*diret-client*
                (first sid-retid) (rest sid-retid))))))

(defun examine-retrieval (button new old)
   (declare (ignore old))
   (let* ((diret-rw (parent button))
          (item (value (find-component :retrieval-list diret-rw))))
      (multiple-value-bind (sid-retid bool)
          (gethash item *retrievals*)
         (declare (ignore bool))
         (let ((exw (make-diret-examine-window))
               (d (cl-user::fetch-data-item cl-user::*diret-client*
                   (first sid-retid)
                   (rest sid-retid))))
            (select-window exw)
            (setf (value (find-component :examine-retrieval exw))
                  d)))))
                  
;;; end-of-file




