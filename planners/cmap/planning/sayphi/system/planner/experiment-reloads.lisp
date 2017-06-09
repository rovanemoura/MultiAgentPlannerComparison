;; =========================================================================    
;;  (C) Copyright 2006 - 2008 
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
;; Description: SAYPHI Experimenter Recomputations
;; 
;; ========================================================================

(defparameter *my-cl-ppcre-pathname* (namestring (truename "./system/cl-ppcre-1.3.2/")))
(load (format nil "~a~a"*my-cl-ppcre-pathname* "load.lisp"))
(sld "experiment-def")


;; This takes the result file of an experiment execution 
;; to not perform the experiment summary computation again
(defun read-experiment-results (experiment-var result-file)
  (let ((correcta))
    (with-open-file (stream result-file :direction :input)
      (do ((line (read-line stream nil 'eof)
		 (read-line stream nil 'eof)))
	  ((eq line 'eof))
	
	(setf correcta (cl-ppcre::scan "#" line))
	(if (and (not correcta) (not (equal "" line)))
	    (progn 
	      (setf line (concatenate 'string "(" line))
	      (setf line (concatenate 'string line ")"))	      
	      (setf line (read-from-string line))
	      
	      (unless (null experiment-var)
		(push (list (first line) (second line) (third line) (fourth line) (fifth line) -1 -1 -1 -1)
		      (gethash experiment-var *experiment-results*)))))))))


(defun cbth-expvar-name (result-file)
  (intern (string-upcase (format nil "~a" (subseq (pathname-name result-file) 18)))))


;; This receives a list of file-paths to 
;; compute again the experiments summary
(defun reload-experiment-results (files  &key (fn-expvar-name #'cbth-expvar-name))
  (setf *experiment-results* (make-hash-table))
  (setf *experiment-tests* nil)
  (dolist (ifile files)
    (let ((i-expvarname (funcall fn-expvar-name ifile)))
      (push (create-experiment-test i-expvarname) *experiment-tests*)
      (read-experiment-results i-expvarname ifile)))
  
  (exp-sort-results)
  (setf *experiment-tests* (reverse *experiment-tests*)))

    
(defparameter *cbth-result-path* "/home/tomas/diskd/Tesis_Track/resultados/gandalf/")
(defparameter *cbth-benchmarks* nil)

(setf *benchmarks*
      '(
     	"blocksworld" 
;;     	"parking"
;;     	"matching-bw"
;;     	"satellite"
;;     	"rovers" 
     	"logistics"
;;    	"portcrane"  
       	"mprime"
	))


(defun cbth-compute-score-reloaded ()
  (dolist (i-domain *benchmarks*)
    (let* ((dom-path (concatenate 'string *cbth-result-path* "common/" i-domain "/"))
           (result-files (dirfiles-sorted dom-path "*.*"))
	   (summary-file (concatenate 'string *cbth-result-path* "common/" i-domain "-global-scores.txt"))
	   )
      (reload-experiment-results result-files)
      (score-write-results summary-file)))) 
