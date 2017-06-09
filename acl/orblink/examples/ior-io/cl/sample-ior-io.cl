;; copyright (c) 1997-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2012 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: sample-ior-io.cl,v 2.6 2007/04/17 21:54:39 layer Exp $

(in-package :orblink)

;; This file contains some very simple functions to read and write an IOR from or to a file.
;; The definitions in this file are accessible when ORBLink is loaded; this file is included for
;; explanatory purposes only.

;; If non-nil, this should be a pathname that denotes the default directory into which
;; the IOR routines should look.

(defparameter orblink:*default-ior-directory* nil)

(defun orblink:write-ior-to-file (object pathname)
  "Writes the IOR of object, the first argument, to the file denoted by pathname, the
  second argument. Because this routine is primary explanatory, little error checking is
  performed. If *default-ior-directory* is non-nil, pathname is first merged with
  *default-ior-directory*"
  (when *default-ior-directory*
    (setq pathname (merge-pathnames pathname *default-ior-directory*)))
  (ensure-directories-exist pathname)	; Create intermediate directories if necessary
  (with-open-file
      (stream pathname :direction :output :if-exists :supersede)
    (format stream "~A" (op:object_to_string corba:orb object))
    (format t "Wrote ior to file: ~a~%" pathname)
    (force-output)
    t))


(defun orblink:read-ior-from-file (pathname)
  "Reads an object from the file denoted by pathname, merging the pathname with
  *default-ior-directory* if it is non-nil. The file is expected to contain a single line
  containing the stringified IOR of the object in question.  In order to simplify the
  presentation, this routine intentionally performs little or no error checking."

  (when *default-ior-directory* (setq pathname (merge-pathnames pathname *default-ior-directory*)))
  (unless (probe-file pathname)
    (error "read-ior-from-file: unable to locate file: ~a" pathname))
  (with-open-file (stream pathname :direction :input)
    (op:string_to_object
     corba:orb
     (string-trim '(#\return) (read-line stream)) ; We delete trailing carriage returns to avoid potential problems with PC programs that write the file
     )))

