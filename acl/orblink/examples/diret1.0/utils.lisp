#|
========================

File: utils.lisp
Author: Vladimir Kulyukin
Description: Utility methods and functions.
 
Copyright (c) 1994, 1996, 1998-99 

The generic function to-symbols is borrowed from
William Fitzgerald with slight modifications.

Comments and bugs to vkulyukin@cs.depaul.edu 
========================
|#

(in-package :cl-user)

(defmethod to-symbols ((x string) &optional (package *package*))
   "Converts a string to symbols interned in package."
   (to-list x
    :post-process #'(lambda (str)
                      (intern (nstring-upcase str) package))
    :test #'(lambda (ch) (or (alphanumericp ch)
                             (char= ch #\-)))))

(defmethod to-symbols ((x null) &optional (package *package*))
  (declare (ignore package)) nil)

(defmethod to-list ((self string) &key
                    (start 0)
                    (break-chars '(#\space))
                    (test #'(lambda (ch) 
                              (not (find ch break-chars :test #'char=))))
                    (post-process #'identity))
   "Converts a string into a list."
   (labels ((next-break-position (self pos)
             (position-if-not test self :start pos))

            (next-char-position (self pos)
             (unless (null pos)
                (position-if test self :start pos)))

            (to-list-local (position)
             (let* ((break-pos (next-break-position self position))
                    (char-pos  (next-char-position self break-pos)))
                (if break-pos
                   (if char-pos
                      (cons (funcall post-process 
                                     (subseq self position break-pos))
                            (to-list-local char-pos))
                      (list (funcall post-process 
                              (subseq self position break-pos))))
                   (list (funcall post-process (subseq self position)))))))
     (let ((char-pos (next-char-position self start)))
        (if char-pos 
           (to-list-local char-pos) 
           nil))))

(defun hash-table-to-list (ht &key (key-fun #'identity) (val-fun #'identity))
   "Converts a hash table to an assoc-list of keys and values."
   (loop for k being each hash-key in ht
         for v being each hash-value in ht
         collect (list (funcall key-fun k)
                       (funcall val-fun v))))

(defun copy-hash-table (ht &key (test #'eql)
                                (key-function #'identity)
                                (val-function #'identity))
   "Creates a new hash table where each key is the result of
applying key-function to the corresponding key in ht and each
value is the result of applying val-function to the corresponding
value. The test of the new table is specified by test"
   (loop with copy-ht = (make-hash-table :test test)
         for k being each hash-key in ht
         for v being each hash-value in ht
         do (setf (gethash (funcall key-function k) copy-ht)
                  (funcall val-function v))
         finally (return copy-ht)))

(defun every-hash-key (predicate ht)
   "Returns t if every key in hash-table ht satisfies predicate."
   (loop for k being each hash-key in ht
         unless (funcall predicate k)
         do (return nil)
         finally (return t)))

(defun every-hash-value (predicate ht)
   "Returns t if every value in hash-table ht satisfies predicate."
   (loop for v being each hash-value in ht
         unless (funcall predicate v)
         do (return nil)
         finally (return t)))

;;; end-of-file

