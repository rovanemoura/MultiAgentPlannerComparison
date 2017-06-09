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
;; Description: SAYPHI lisp loader
;; Date: 2005.09.12
;; 
;; ========================================================================

(in-package "COMMON-LISP-USER")
(unless (find-package "PRODIGY4") (make-package "PRODIGY4" :nicknames '("P4"))) ;; this does not work. Do not ask me why :use '("COMMON-LISP-USER")))
(provide 'sayphi)

;;You should change this path to your work directory for SAYPHI 
;;The default is the directory in which the LISP system is called
(defvar *sayphi-root* (concatenate 'string *my-planning-path* "sayphi/"))
;; (defvar *sayphi-root* (namestring (truename "./")))

;; For loading correctly the ASDF for the IPC distribution
;; (pushnew (concatenate 'string *sayphi-root*) asdf:*central-registry* :test #'equal)

(defvar *system-dir* (concatenate 'string *sayphi-root* "system/"))
(defvar *bin-dir* (concatenate 'string *system-dir* 
			       #+ALLEGRO "allegro-bin/"
			       #+CLISP "compiled/"
			       #+SBCL "sbcl-bin/"))

;; (defparameter *binary-dir*
;;   #+(and CLISP UNIX) ".clisp/"
;;   #+(and ALLEGRO UNIX) ".acl/"
;;   #+SBCL ".sbcl/"
;;   #+(and (OR DOS WIN32) CLISP) "dosbin/"
;;   #+(and ALLEGRO MSWINDOWS) "allegro-bin/"
;;   "Directory where IPSS binaries are")

(defvar *domains-dir* (concatenate 'string *sayphi-root* "domains/"))
(defvar *domain-dir* nil)

(defvar *say-lisp-extension* ".lisp")
(defvar *bin-extension*
  #+(or ALLEGRO SBCL) ".fasl"
  #+CLISP ".fas")

(defvar *auto-load* t)
(defvar *load-full* nil)

(defvar *sayphi-modules*
  '(("planner/" ("trace-error" "structures" "genericfuns" "parse-domain" "read-domain" 
		 "read-problem" "instantiate" "initmetric" "metric" "planning" "numheuristic" 
		 "helpful" "lookahead" "print"  "search"  "support" "runsets" "learnsupport" 
;; 		 "validate"
		 ))))


(defparameter *sayphi-modules-extra*
  '(("planner/" ("tree-study" "relational-search" "oversubscription"))))
  

(defvar *cbp-sayphi-modules*
  '(("cbr/" ("cbr-structures" "cbrgeneralfuns"  "casefs_interface" "property"
	     "utility-cases" "merge-case" "store-case" "select-replay"  "unify-typeseq"  
 	     "replay-case" "cbr-runsets" "cabala" "inclearn-cabala" "refine-types"))))


;;ROLLER Learner: Learning Decision Trees from Helpful Contexts
(defvar *roller-modules*
  '(("roller/" ("btree" "learn-helpful" "roller" "rolahead" "active-learning" "learn-lookahead" "flare-search"))))

;; REPLICA Learner: Relational-based policy learning
(defvar *replica-modules* '(("replica/" ("ribl-distance" "relational-search"))))

;; For EBL learning
(defvar *ebl-modules* '(("learning/" ("btree" "matrix" "ebl" "extras-daniel-search" "modify-problems" "run-rules" "daniel-runsets" "results"))))
;; (defvar *ebl-modules* '(("learning/" ("btree" "matrix" "pddl2tilde" "ebl" "extras-daniel-search" "results" "modify-problems" "run-rules"))))

;; ERRT-Plan.  "constraints" not longer needed
(defvar *errt-modules* '(("learning/" ("matrix" "ebl" "errt" "extras-daniel-search" "results" "modify-problems" "support-errtplan" "external-planner"))))

;;LTL Learner: Learning Linear Temporal Logic Formulas for TLPlan

(defvar *ltl-modules*
  '(("ltl/" ("pl-support" "learn-ltl" "ltl-rules" "bg-common" "bg-discover" "bg-creation"))))

;; Domain-dependent heuristics. No longer needed. Integrated with Sayphi core
;; (defvar *ddh-modules* '(("learning/" ("common-heuristics"))))

(defun load-sayphi (&key (load-source nil)
			 (modules *sayphi-modules*))
  (dolist (module modules)
    (dolist (file (cadr module))
      (if load-source
	  (load (concatenate 'string *system-dir* (car module) file *say-lisp-extension*))
	(load (concatenate 'string *bin-dir* file *bin-extension*))))))

(defun compile-all (&key (modules *sayphi-modules*))
  (dolist (module modules)
    (dolist (file (cadr module))
      (compile-file (concatenate 'string *system-dir* (car module) file  *say-lisp-extension*)
       :output-file (concatenate 'string *bin-dir* file *bin-extension*)))))


(defun load-learner (learner &key (load-source t))
  (let ((modules (case learner
		   (ddh *ddh-modules*)
		   (cbr *cbp-sayphi-modules*)
		   (roller *roller-modules*)
		   (learner *replica-modules*)
		   (ltl *ltl-modules*)
		   (ebl (setf *planner-for-learning* 'sayphi) ;; this is needed for the right compilation of generic-macros
			(asdf:oos 'asdf:load-op "cl-ppcre")
			(load-and-compile '("defaults" "generic-macros" "generic-hamlet" "generic-induction" "rete" "generic-rules" "generic-pddl" "execute-ff"))
;; 			(load "/home/dborrajo/mi-software/tilde2lisp/pddl2tilde.lisp")
;; 			(compile-file "/home/dborrajo/mi-software/tilde2lisp/pddl2tilde.lisp")
;; 			(load "/home/dborrajo/mi-software/tilde2lisp/pddl2tilde.lisp")
			*ebl-modules*)
		   (errt (asdf:oos 'asdf:load-op "cl-ppcre")
			 ;; I need this files from Hamlet-IPSS
			 (load-and-compile (list "generic-pddl" "execute-ff"))
			 *errt-modules*)
		   (otherwise nil))))
    (when load-source
      (load-sayphi :load-source t :modules modules) 
      (compile-all :modules modules))
    (load-sayphi :load-source nil :modules modules)
    ))


(when *auto-load*
  (progn 
    (setf *load-verbose* t)
    (load-sayphi :load-source t)
    (compile-all)
    (setf *load-verbose* nil)
    (load-sayphi :load-source nil)
    
    (when *load-full* 
      (setf *load-verbose* t)
      (load-sayphi :load-source t :modules *sayphi-modules-extra*)
      (compile-all :modules *sayphi-modules-extra*)
      (setf *load-verbose* nil)
      (load-sayphi :load-source nil :modules *sayphi-modules-extra*))
    
    (pp-welcome)
))
