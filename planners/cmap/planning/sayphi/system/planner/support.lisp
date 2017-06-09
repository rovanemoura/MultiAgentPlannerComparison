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
;; Description: Support function for command user interface
;; Date: 2006.01.10
;; 
;; ========================================================================


(defvar *problem-dir* "probsets/")
(defvar *result-dir* "result/")
(defvar *domain-subdirs* '("probsets" "result" "rules" "cases" "roller"))
(defvar *problem-file* "")
(defvar *domain-file* "")
(defvar *domain-name* nil)

;; DB: domain-dependent heuristic computation variables.
(defvar *domain-dependent-heuristic* nil)
(defvar *trace-heuristic* nil)

(defvar *say-solution-format* :values)
(defvar *experiment-train* nil)
(defvar *experiment-results* nil)
(defvar *experiment-tests* nil)
(defvar *experiment-outputs* nil)
(defvar *experiment-study* nil)


;;Variables to control the experiment of each domain
(defvar *this-domain* nil)
(defvar *this-domain-file* nil)
(defvar *this-timeout* nil)
(defvar *this-date* nil)

(defparameter *say-knowledge-path* nil)

;;=============================================================
;; SOME FUNCTIONS USED TO UPDATE VARIABLES IN EXECUTABLES VERSIONS
;;=============================================================

(defun reload-vars-for-execore()  
  (setf *sayphi-root* (namestring (truename "./")))
  (setf *system-dir* (concatenate 'string *sayphi-root* "system/"))
  (setf *bin-dir* (concatenate 'string *system-dir* 
			       #+ALLEGRO "allegro-bin/"
			       #+CLISP "compiled/"
			       #+SBCL "sbcl-bin/"))
  (setf *domains-dir* (concatenate 'string *sayphi-root* "domains/"))
)



(defun sayload-for-exe ()
  (reload-vars-for-execore)
  (read-pddl-domain (nth 1 *posix-argv*))
  (format t "~% domain loaded!")
  (read-pddl-problem (nth 2 *posix-argv*))
  (setf *problem-file* (filename-of-stringpath (nth 2 *posix-argv*)))
  (format t "~% ~a problem loaded!" *problem-file*))
  

(defun sayphi-from-exe ()
  (let ((solution nil))
    (sayload-for-exe)
    (when (read-from-string (nth 7 *posix-argv*))  ;; Setting off the output if it is XML format
      (setf *say-output* 0))

    (setf solution (runplan (intern (string-upcase (nth 4 *posix-argv*))) (read-from-string (nth 5 *posix-argv*))))
    (when (solution-p solution)
      (cond ((read-from-string (nth 7 *posix-argv*))
	     (format t "~%~%")
	     (all-solutions-to-xml t))
	    (t
	     (format t "~%~%~a~%" solution)))
      (when (and (solution-found solution) (not (read-from-string (nth 6 *posix-argv*))))
	 (cond ((read-from-string (nth 7 *posix-argv*))
	        (write-xml-solutions-for-exe))
	       (t
		(write-solution-for-exe solution)
		(write-multisolution-for-exe)))
	))
;;     (quit)
    ))


(defmacro sayrun (&rest body)
  `(prog1 
    (multiple-value-bind (result condition) (ignore-errors ,@body)
      (cond ((not (null condition))
		   (format t "~%[SAYPHI]:Exception >> ~a~%" condition))
		  (t result)))
    (quit)
    ))


	   
	   



(defun write-solution-for-exe (solution &optional index)
  (with-open-file (out-stream (if index (format nil "~a.soln~a" *problem-file* index) 
                                        (format nil "~a.soln" *problem-file*))
			      :direction :output :if-exists :supersede :if-does-not-exist :create)
    (say-print-solution-ipc solution out-stream)))


(defun write-xml-solutions-for-exe ()
  (with-open-file (out-stream (format nil "~a.xml" *problem-file*)
			      :direction :output :if-exists :supersede :if-does-not-exist :create)
  (all-solutions-to-xml out-stream)))



(defun write-multisolution-for-exe ()
  (when (hash-table-p *say-hash-solutions*)
    (maphash #'(lambda (index solution)
		 (write-solution-for-exe solution index))
	     *say-hash-solutions*)))





(defun load-domain-and-prob-and-plan-and-quit ()
  (say-domain (nth 1 *posix-argv*))
  (prob	(nth 2 *posix-argv*))
  (plan :algorithm (intern (string-upcase (nth 4 *posix-argv*)))
        :timeout (read-from-string (nth 5 *posix-argv*)))
  (quit))


;;=============================================================



(defun say-plan (domain-dir domain-file problem-file)
  (say-domain domain-dir domain-file)
  (prob problem-file))


(defun say-domain (domain-dir &optional (domain-file "domain.pddl"))
  (setf *domain-name* domain-dir)
  (setf *domain-dependent-heuristic* (intern (format nil "~@:(h-~a~)" *domain-name*)))
  (setf *domain-dir* (concatenate 'string *domains-dir* domain-dir "/"))
  (setf *domain-file* (concatenate 'string *domain-dir* domain-file))
  (read-pddl-domain *domain-file*)
  (if (> *say-output* 0)
      (format t "~% ~a domain loaded!" domain-dir))
  t)


(defun prob (problem-file)
  (cond ((null *domain-dir*)
	 (error (msg-error 'no_domain_dir 'problem)))
	(t
	 (read-pddl-problem (concatenate 'string *domain-dir* *problem-dir* problem-file))
	 (setf *problem-file* problem-file)
	 (setf *complete-problem-file* problem-file)
	 (format t "~% ~a problem loaded!" problem-file)
	 t)))


(defun prob-fromset (problem-file)
  (setf *problem-file* problem-file)
  (cond ((null *domain-dir*)
	 (error (msg-error 'no_domain_dir 'problem)))
	(t
	 (read-pddl-problem (concatenate 'string *domain-dir* "probsets/" problem-file))
	 (format t "~% ~a problem loaded!" problem-file)
	 t)))


(defun sayphi-compile (file-name)
  (compile-file (concatenate 'string *system-dir* "planner/" file-name  *say-lisp-extension*)
       :output-file (concatenate 'string *bin-dir* file-name *bin-extension*)))


(defun sld (file-name)
  (load (concatenate 'string *system-dir* "planner/" file-name *say-lisp-extension*)))


(defun lcbr (file-name)
  (load (concatenate 'string *system-dir* "cbr/" file-name *say-lisp-extension*)))


;;Function para cargar dominio y fichero con ruta absoluta
(defun sayphi (domain-path prob-path &key (path "")
	                                  (plan-args nil))
    (read-pddl-domain (concatenate 'string path domain-path))
    (format t "~% domain loaded!")
    (read-pddl-problem (concatenate 'string path prob-path))
    (setf *problem-file* (filename-of-stringpath prob-path))
    (format t "~% ~a problem loaded!" *problem-file*)
    (apply #'plan plan-args)
)


(defun filename-of-stringpath (strpath)
  (if (find #\/ strpath)
      (subseq strpath (1+ (position #\/ strpath :from-end t)))
      strpath))
 

;; Retorna una lista ordenada de los archivos del directorio
(defun dirfiles-sorted (path wild-char)
   (let* ((dirfiles (directory (concatenate 'string path wild-char)))
          (file-names (mapcar #'(lambda (x-name)
				 (file-namestring x-name))
			      dirfiles))
          (sorted-names (sort file-names #'string-lessp)))
       (mapcar #'(lambda (x-name)
		   (concatenate 'string path x-name)) sorted-names)))
 
;;the domain file must be loaded, in order to have something in *domain-dir*
(defun get-domain-string()
  (let* ((dom-string (subseq *domain-dir* 0 (- (length *domain-dir*)  1)))
	 (slash-pos (position #\/ dom-string :from-end t)))
    (subseq dom-string (1+ slash-pos))))


    

(defun get-problem-filename ()
  (cond ((find #\. *problem-file*)
	 (subseq *problem-file* 0 (- (length *problem-file*) 5)))
	(t *problem-file*)))


(defun get-problem-subset ()
  (let ((prob-name (get-problem-filename)))
    (when (> (length prob-name) 8)
      (subseq prob-name (- (length prob-name) 6) (- (length prob-name) 4)))))


(defun format-plotfield(value)
  (cond ((floatp value)
	 (format nil "~4$" value))
	(t
	 (format nil "~a" value))))
	


;;======================================================================
;; Basic Functions - Missing in SBCL or Common Lisp
;;======================================================================

;; It converts a string into a integer
(defun s_string-to-integer (strnumber)
  (let ((num 0) (decimal 0) (cut-num nil))
    (when (stringp strnumber)
      (dotimes (i (length strnumber) num)
 (setf decimal (- (char-int (aref strnumber (- (1- (length strnumber)) i))) 48)) 
 (cond ((and (not cut-num) 
      (>= decimal 0)
      (<= decimal 9))
        (setf num (+ num (* (expt 10 i) decimal))))
       (t (setf cut-num t))
       ))
 
)))

;;======================================================================
;; Functions for dealing with SBCL Analysis
;;======================================================================

;; It runs the form and stores in problem plist the consed bytes of the form
(defmacro say-consed-bytes (byte-cons-property form)
  ` (let ((trace-string (make-array 300 :element-type 'character 
				    :fill-pointer 0 :adjustable t))
	  (bytes-consed 0))
      (with-output-to-string 
	  (*trace-output* trace-string)
	#+SBCL (prog1 
		   (unwind-protect 
			(time ,form)
		     (setf bytes-consed (subseq trace-string (+ 6 (search "and" trace-string :from-end t))
						(- (search "bytes" trace-string :from-end t) 1)))
		     (format t "~%~a ~a" bytes-consed trace-string)
		     (setf (getf (problem-plist *current-problem*) ,byte-cons-property) 
		       (s_string-to-integer (remove #\, bytes-consed))
		       )))
	#-SBCL (time ,form)
	)))


(defun say-used-memory (room-space-property)
  #+SBCL (let ((trace-string (make-array 100 :element-type 'character 
					:fill-pointer 3 :adjustable t))
	      (memory-used 0))
	  (with-output-to-string 
	(*standard-output* trace-string)
	    (progn 
	      (room nil)
	      (setf memory-used (subseq trace-string (+ 3 (search ":" trace-string ))
					(- (search "bytes" trace-string) 1)))
	      (setf (getf (problem-plist *current-problem*) room-space-property) 
		    (s_string-to-integer (remove #\, memory-used))
 	      )
	      )))
  #-SBCL 	      (setf (getf (problem-plist *current-problem*) room-space-property) 0)
  )
    
  


;;=======================================================================
;; Macro Utilities for Developing & Debugging
;;=======================================================================

(defmacro s_action_val (action &rest vars)
  `(let ((x_action (find ,action (dom-actions *pspace*) :key #'action-name)))
    (format t "~% <Action: ~a>" (action-name x_action))
    ,@(mapcar #'(lambda (x) `(format t "~% ~a: ~a" (quote ,x) (,x x_action))) vars)))
    
(defmacro s_allaction_val (&rest vars)
  `(dolist (iaction (dom-actions *pspace*))
     (format t "~%~% <Action: ~a>" (action-name iaction))
     ,@(mapcar #'(lambda (x) `(format t "~% ~a: ~a" (quote ,x) (,x iaction))) vars)))

;;It Gets the number of instances of each predicate in the state
(defun s_predschema_count ()
  (let ((pattern (problem-search-tree *current-problem*)))
    (format t "~% == STATE PREDICATES SCHEMAS ==")
    (maphash #'(lambda (pred varmap)
		 (cond ((bit-vector-p varmap)
			(format t "~% ~a: ~d (~a)" pred (length varmap) (count 1 varmap)))
		       (t
			(format t "~% ~a: ~d" pred (length varmap)))))
	     (snode-state pattern))))


(defmacro sg_allaction_val (&rest vars)
  `(dolist (iaction *actions*)
     (format t "~%~% <Gaction: ~a>" (gaction-planaction iaction))
     ,@(mapcar #'(lambda (x) `(format t "~% ~a: ~a" (quote ,x) (,x iaction))) vars)))


;;=======================================================================
;; DATE & TIME FUNCTIONS
;;=======================================================================

(defun format-digits-2 (number)
  (cond ((< number 10) (format nil "0~d" number))
	(t (format nil "~d" number))))


;; Returns a string tag useful for naming files
;; The format is yyyymmdd
(defun my-today-tag()
  (multiple-value-bind (sec minute hour day month year) (get-decoded-time)
    (declare (ignore sec minute hour))
    (format nil "~a~a~a" year 
	    (format-digits-2 month)
	    (format-digits-2 day))))

;; Returns a string tag useful for naming files
;; The format is yyyymmdd-hhmm
(defun my-now-tag()
  (multiple-value-bind (sec minute hour day month year) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~a~a~a-~a~a" year 
	    (format-digits-2 month)
	    (format-digits-2 day)
	    (format-digits-2 hour)
	    (format-digits-2 minute))))



;; This call the script for cleaning up the result directory of domains
(defun say-script-cleanup-results ()
  #+SBCL (run-program (format nil "~ascripts/cleanup-results.sh" *system-dir*) nil))

;; This call the script for cleaning up the roller directory of domains
(defun say-script-cleanup-roller ()
  #+SBCL (run-program (format nil "~ascripts/cleanup-roller.sh" *system-dir*) nil))

(defun say-script-make-dir (dir)
  #+SBCL (run-program (format nil "~ascripts/make-dir.sh" *system-dir*) (list dir)))

(defun say-script-execsh (script &rest parameters)
  #+SBCL (run-program (format nil "~ascripts/~a" *system-dir* script)  
		      (mapcar #'(lambda (arg) (format nil "~a" arg)) parameters)))




;;=====================================================
;; SOME FUNCTIONS USED BY DOMAIN PROBLEM GENERATORS 
;;=====================================================

(defun format-numprob (number)
  (cond ((< number 10)
	 (format nil "00~d" number))
	((< number 100)
	 (format nil "0~d" number))
	(t
	 (format nil "~d" number))))

(defun format-groupprob (number)
  (cond ((< number 10)
	 (format nil "0~d" number))
	(t
	 (format nil "~d" number))))


;;=================================================================
;; Counting problem parameters like number of objects, goals, etc.
;;=================================================================

(defun s_problem_parameters ()
  (list (cons 'num-objects (length (get-all-objects)))
	(cons 'num-literals (length (pp-state (problem-init-state *current-problem*) 'list)))
	(cons 'num-goals (length (pp-state (problem-goals *current-problem*) 'list)))))



;; Domain must be loaded first
(defun s_probset_properties (probprefix)
  (let ((wild-char #+SBCL "*.*" #-SBCL "*")
	(probset-dir (format nil "~aprobsets/" *domain-dir*))
	(probset-properties nil))
    (dolist (thisprob (dirfiles-sorted  probset-dir (concatenate 'string probprefix wild-char))
	     probset-properties)
      (read-pddl-problem thisprob)
      (push (list (pathname-name thisprob)
		  (s_problem_parameters))
	    probset-properties))))


;; Renaming probsets for prette prefix
(defun rename-probsets (probset-path new-prefix)
  (let ((wild-char #+SBCL "*.*" #-SBCL "*")
	(i 1))
    (dolist (thisprob (dirfiles-sorted probset-path wild-char))
      (say-script-execsh "copy-file.sh" (format nil "~a" thisprob) 
			 (format nil "~a/~a~a.pddl" probset-path new-prefix (format-numprob i)))
      (incf i)
      )
    ))

;; when some learning process fail and the root directory is changed
(defun say-restore-root-directory ()
  ;; DB: I have to comment it out, since sb-posix does not work in current SBCL
;; #+SBCL  (sb-posix:chdir *sayphi-root*)
#+CLISP (ext::cd *sayphi-root*)
  (setf *default-pathname-defaults* (pathname *sayphi-root*))
)

;; DB. It allows the computation of a domain dependent heuristic
(defun h-domain-dependent (node &optional (pgoals (problem-goals *current-problem*)))
  (let ((state (give-me-nice-sayphi-state node))
	(goals (pp-state pgoals 'list)))
    (when *trace-heuristic*
      (format t "~%State:")
      (pp-list state 1 t t t)
      (format t "~%Goals:")
      (pp-list goals 1 t t t))
    (let ((h (funcall *domain-dependent-heuristic* state goals)))
      (when *trace-heuristic*
	(format t "~%Heuristic: ~a" h)
	(format t "~%FF heuristic: ~a" (h-relaxedplan node pgoals)))
      (values h nil))))

;; DB. It allows to easily define a domain dependent heuristic by abstracting the details of the state representation. It returns the object in the arg-position
;; in the state literal where object is the second argument
(defun check-truth (predicate object state arg-position)
  (nth arg-position (find-if #'(lambda (literal)
				 (and (eq (car literal) predicate)
				      (eq object (cadr literal))))
			     state)))

;; DB: returns a list of literals in the goals
(defun give-me-nice-sayphi-goals (node)
  (if (snode-p node)
      (pp-state (target-goals node) 'list)))

;; DB: returns a list of literals true in the state in node
(defun give-me-nice-sayphi-state (node)
  (if (snode-p node)
      (pp-state (snode-state node) 'list)))