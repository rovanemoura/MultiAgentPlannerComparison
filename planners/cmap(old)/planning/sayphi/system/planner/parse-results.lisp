;; =========================================================================    
;;  (C) Copyright 2006, 2008 
;;      Universidad Carlos III de Madrid
;;      Planning & Learning Group (PLG)
;; 
;; =========================================================================
;; 
;; This file is part of SAYPHI
;; 
;; 
;; SAYPHI is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; SAYPHI is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with SAYPHI.  If not, see <http://www.gnu.org/licenses/>.
;; ========================================================================

;; Author: Tomas de la Rosa 
;; Description: Running sets of problems. Function for SAYPHI Experimenter
;; Date: 2006.12.16
;; 
;; ========================================================================
(defparameter *my-cl-ppcre-pathname* (namestring (truename "./system/cl-ppcre-1.3.2/")))
(unless (find-package 'cl-ppcre)
  (load (format nil "~a~a"*my-cl-ppcre-pathname* "load.lisp")))

(defparameter *exp-string-tag-hc* "hdiff_hc")
(defparameter *exp-string-tag-ehc* "hdiff_ehc")
(defparameter *exp-string-tag-wbfs* "hdiff_wbfs")

(defparameter *exp-tags-hc* '("FFD-HC" "FFD-HC-HDIFF" "FFD-HC-DIFFOP"))  
(defparameter *exp-tags-ehc* '("FFD-EHC" "FFD-EHC-HDIFF" "FFD-EHC-DIFFOP"))  
(defparameter *exp-tags-wbfs* '("FFD-WBFS" "FFD-WBFS-HDIFF" "FFD-WBFS-DIFFOP"))  

(defun hdiff-result-compilation (path string-tag exp-tags)
  (let ((output-file (concatenate 'string path "result-compilation" string-tag ".txt")))
    (with-open-file (ofile output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (dir (directory (concatenate 'string path "*")))
	(let* ((dir-string (format nil "~a" dir))
	       (idomain (cl-ppcre::regex-replace path dir-string ""))
	       (my-res-dir nil)
	       )
	  (setf idomain (filename-of-stringpath (subseq dir-string 0 (1- (length dir-string)))))
	  (format ofile "~%~a " idomain)
	  (dolist (res-dir (directory (concatenate 'string dir-string "*")))
	    (let ((res-string-dir (format nil "~a" res-dir)))
;;	      (format t "~%Looking in ~a"  res-string-dir)
	      (cond ((cl-ppcre::scan string-tag res-string-dir)
		     (setf my-res-dir res-string-dir)))))
	  (when my-res-dir
	    (format t "~%OK -- ~a" my-res-dir)
	    (dolist (res-file (directory (concatenate 'string my-res-dir "*.*")))
	      (let ((res-string-file (format nil "~a" res-file)))
		(cond ((cl-ppcre::scan "probsolved" res-string-file)
		       (grab-probsolved res-string-file ofile exp-tags)))))
	    (dolist (res-file (directory (concatenate 'string my-res-dir "*.*")))
	      (let ((res-string-file (format nil "~a" res-file)))
		(cond ((cl-ppcre::scan "accdata" res-string-file)
		       (grab-accumulated res-string-file ofile)))))

	      ))))))


(defun grab-accumulated (res-file ofile)
  (let ((last-line nil) (num-common 0) (res-data nil))
  (with-open-file (istream res-file :direction :input)
    (do ((line (read-line istream nil :eof) (read-line istream nil :eof)))
	((equal line :eof) nil)
      (setf last-line line)
      (incf num-common)
      )
    (setf res-data (read-from-string (concatenate 'string "(" last-line ")")))
    (dolist (idata (cdr (cdr res-data)))
      (when (and (numberp idata)
		 (> idata 0))
	(format ofile "~2$ " (/ idata num-common)))))))
      

(defun grab-probsolved (res-file ofile exp-tags)
  (let ((common-solved nil))
    (with-open-file (istream res-file :direction :input)
      (do ((line (read-line istream nil :eof) (read-line istream nil :eof)))
	((equal line :eof) nil)
	(when (cl-ppcre::scan "Solved by" line)
	  (setf common-solved (cl-ppcre::regex-replace " Solved by all tests:" line "")))
	(dolist (i-tag exp-tags)
	  (when (cl-ppcre::scan (concatenate 'string i-tag ":") line)
	    (format ofile "~a " (second (cl-ppcre::split "[ ]+" line))))))
	(format ofile "~a " common-solved)  
	)))

	      