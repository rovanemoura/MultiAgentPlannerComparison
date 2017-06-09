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
;; Description: PDDL Problem Parser
;; Date: 2005.12.04
;; 
;; ========================================================================

(defparameter *facts* nil)
(defparameter *init-facts* nil)
(defparameter *fluents* nil)
(defparameter *init-fluents* nil)
(defvar *constraintsp* nil)


;; This function creates real instances of objects for domain constants defined in domain file
(defun update-constant-objects (objects)
  (maphash #'(lambda (type constants)
	       (setf (gethash type objects)
		     (remove-duplicates (append (gethash type objects) constants))))
	   (dom-constants *pspace*)))


(defun get-inheritobjects (abs-type inheritypes objects)
  (let ((inheritobjects nil)
	(subtypes (get-vals abs-type inheritypes)))
    (dolist (isubtype subtypes inheritobjects)
      (setf inheritobjects (append inheritobjects (gethash isubtype objects))))))


(defun build-patterns (predicates functors objects)
  (let ((pattern (make-hash-table :test #'eq)))
    (maphash 
     #'(lambda (keypred argspred)
	 (let* ((count-objlist (mapcar #'(lambda (var) (length (gethash (cdr var) objects)))
				       argspred))
		(bitmap-len (apply #'* count-objlist)))
	   (setf (gethash keypred pattern) (make-array bitmap-len :element-type 'bit 
						       :initial-element 0))))
     predicates)
    (when (hash-table-p functors)
      (maphash 
       #'(lambda (keyfun argsfun)
	   (let* ((count-objlist (mapcar #'(lambda (var) (length (gethash (cdr var) objects)))
					 argsfun))
		  (map-len (if (null count-objlist) 
			       1
			     (apply #'* count-objlist))))
	     (setf (gethash keyfun pattern) (make-array map-len :initial-element nil))))
       functors))
    pattern
))


(defun translate-state (state-def objects predicates functors state-pattern)
  (let ((hash-state (copy-state state-pattern))
	(lit-pos nil))
    (when (hkey-present 'total-time (dom-functors *pspace*))
      (setf (svref (gethash 'total-time hash-state) 0) 0))
    (dolist (iliteral state-def hash-state)
      (cond ((eq '= (car  iliteral))
	     (let ((ifunctor (cadr iliteral))
		   (func-val (third iliteral)))
	       (if (hkey-present (car ifunctor) functors)
		   (progn
		     (if (cdr ifunctor)
			 (setf lit-pos (map-position 
					(literal-args-pos (cdr ifunctor) 
						    (gethash (car ifunctor) functors) objects)))
			 (setf lit-pos 0))
		     (setf (svref (gethash (car ifunctor) hash-state) lit-pos) func-val))
		   (error "State function not in domain description [~S]" ifunctor)
		   )))
	    (t
	     (if (hkey-present (car iliteral) predicates)
		 (progn
		   (if (cdr iliteral)
		       (setf lit-pos (map-position 
				    (literal-args-pos (cdr iliteral) 
						      (gethash (car iliteral) predicates) objects)))
		       (setf lit-pos 0))
		   (setf (sbit (gethash (car iliteral) hash-state) lit-pos) 1))
		 (error "State predicate not in domain description [~S]" iliteral))))
    )))
	  
(defun simple-numgoal (expressions functors)
  (and (= (length expressions) 2)
       (or (numberp (first expressions))
	   (and (hkey-present (car (first expressions)) functors)
		(null (cdr (first expressions)))))
       (or (numberp (second expressions))
	   (and (hkey-present (car (second expressions)) functors)
		(null (cdr (second expressions)))))))

	   

(defun translate-goals (goals-def objects predicates functors pattern)
  (let ((hash-goals  (make-hash-table :test #'eq))
        (numgoals nil)
	(goals (if (eq 'and (car goals-def))
		   (cdr goals-def)
		 (list goals-def)))
	(lit-pos nil)
	(map-len 0))
    (dolist (igoal goals)
      (cond ((hkey-present (car igoal) predicates)
	     (unless (gethash (car igoal) hash-goals)
	       (setf map-len (array-dimension (gethash (car igoal) pattern) 0))
	       (setf (gethash (car igoal) hash-goals) (make-array map-len :element-type 'bit 
								  :initial-element 0)))
	     (if (cdr igoal)
		 (setf lit-pos (map-position (literal-args-pos 
					      (cdr igoal) (gethash (car igoal) predicates) objects)))
		 (setf lit-pos 0))
	     (setf (sbit (gethash (car igoal) hash-goals) lit-pos) 1))
	    ((find (car igoal) '(= > >= < <=))
	     (cond ((simple-numgoal (cdr igoal) functors)
		    (push (transform-lnf-step2 (transform-lnf-step1 igoal)) numgoals))
		   (t (error "Complex numeric goals not allowed [~a]" igoal))))
  	    (t (error "Goal predicate not in domain description [~S]" (car igoal)))
	    ))
    (cons hash-goals numgoals)))


;; It appends the inheritobjects to the existing list of objects because some domains definitions
;; has an open hierarchy that defines objects of abstract types.
(defun buildall-inheritobjects (objects inheritypes)
  (update-constant-objects objects)
  (let ((inheritobjects (make-hash-table :test #'eq)))
    (maphash #'(lambda (ob-type inst-objects)
		 (setf (gethash ob-type inheritobjects) inst-objects))
	     objects)
    (dolist (itype inheritypes inheritobjects)
      (setf (gethash (car itype) inheritobjects)
	    (remove-duplicates (append (gethash (car itype) inheritobjects)
				       (get-inheritobjects (car itype) inheritypes objects)))))))


;; Creates a hash table with the conses-poslist (position . type)
;; for domain constants. This is stored in problem because objects may have constants and real objects
(defun build-constant-poslist (objects)
  (let ((constant-poslist (make-hash-table :test #'eq)))
    (maphash (lambda (key constants)
	       (dolist (i-const constants)
		 (setf (gethash i-const constant-poslist)
		       (cons (position i-const (gethash key objects)) key))))
	     (dom-constants *pspace*))
    constant-poslist))


;; It sets in state-patterns numvars to 0 after init state is created
;; This is performed since some of the numvars (= (pred) val) may not be defined in the state definition
(defun pattern-functor-update (patterns state)
  (maphash (lambda (key varmap)
	     (unless (simple-bit-vector-p varmap)
	       (let ((pattern-map (gethash key patterns)))
		 (dotimes (iposvar (length varmap))
		   (when (numberp (aref varmap iposvar))
		     (setf (aref pattern-map iposvar) 0))))))
	   state))


(defun literal-predpos (literal &optional (fluent nil))
  (let ((predicates (if fluent (dom-functors *pspace*)
			(dom-predicates *pspace*)))
	(objects (problem-inheritobjects *current-problem*)))
    (cons (car literal)
	  (map-position (literal-args-pos (cdr literal) (gethash (car literal) predicates) objects)))))
  
(defun create-fact (fact)
  (setf (gethash fact *facts*)
	(make-fact :literal fact
		   :int (1+ (hash-table-count *facts*))
		   :predpos (literal-predpos fact)
		   ))
  )


(defun create-fluent (fluent)
  (setf (gethash (car fluent) *fluents*)
	(make-fluent :literal (car fluent)
		     :int  (1+ (hash-table-count *fluents*))
		     :predpos (literal-predpos (car fluent) t)
		     :init-value (second fluent))))

			       

;; This structures are for the new representation used in the instantiation algorithm
;; Artificial preds (not and equality) should be loaded here to
(defun parse-initial-state (state-def)
  (setf *facts* (make-hash-table :test #'equal))
  (setf *init-facts* (make-hash-table :test #'equal))
  (setf *fluents* (make-hash-table :test #'equal))
  (setf *init-fluents* (make-hash-table :test #'equal))
  (create-fluent '((total-time) 0))
  (dolist (iliteral state-def)
    (cond ((eq '= (car  iliteral))
	   (unless (gethash (cadr iliteral) *fluents*)
	     (setf (gethash (cadr iliteral) *init-fluents*) (create-fluent (cdr iliteral)))
	     ))
	  (t
	   (unless (gethash iliteral *facts*)
	     (setf (gethash iliteral *init-facts*) (create-fact iliteral))))))
  (maphash #'(lambda (pred bitmap)
	       (when (or (and (negative-pred-p pred)
			      (not (hkey-present pred (dom-functors *pspace*))))
			 (equality-pred-p pred))
		 (let ((instances (realinstances-from-bitmap bitmap (gethash pred (dom-predicates *pspace*))
							     (problem-inheritobjects *current-problem*))))
		   (dolist (i-instance instances)
		     (setf (gethash (cons pred i-instance) *init-facts*)
			   (create-fact (cons pred i-instance)))))))
	   (problem-init-state *current-problem*)))



(defun parse-initial-goals (literal-goals)
  (dolist (igoal literal-goals)
    (unless (find (car igoal) '(= > >= < <=))
      (unless (gethash igoal *facts*)
	(create-fact igoal)))))
  

;; DEALING WITH NOTs AND EQUALITY
;; Sets the state bitmaps for artificial _nots predicates
;; Initial State must be loaded first
(defun update-initstate-for-nots ()
  (let ((init-state (problem-init-state *current-problem*)))
    (dolist (i-pred (dom-negative-preds *pspace*))
      (setf (gethash (negate-var i-pred) init-state)
	    (bit-not (gethash i-pred init-state))))))

(defun update-initstate-for-equality ()
  (let ((init-state (problem-init-state *current-problem*)))
    (dolist (i-type (dom-special-preds *pspace*))
      (let* ((equality-pred (equality-var i-type))
	     (num-objects (length (gethash i-type (problem-inheritobjects *current-problem*)))))
	(dotimes (i num-objects)
	  (setf (sbit (gethash equality-pred init-state) 
		      (map-position (list (list i num-objects) (list i num-objects)))) 1))
	(setf (gethash (negate-var equality-pred) init-state) 
	      (bit-not (gethash equality-pred init-state)))))))
	      

;; This function copies domain numvars lists to problem numvars lists, in order to handle
;; specific list created for numeric goals
(defun set-numvars-artificial ()
  (setf (problem-positive-vars *current-problem*) (dom-positive-vars *pspace*))
  (setf (problem-negative-vars *current-problem*) (dom-negative-vars *pspace*))
  (setf (problem-artificial-vars *current-problem*) (dom-artificial-vars *pspace*)))

;; ________________________________________________________________________________

  
(defun parse-problem (problem-def)
  (let* ((prob-domain (car (find-argument problem-def :domain)))
	 (problem (car (find-argument problem-def 'problem)))
	 (objects (translate-objects-def (find-argument problem-def :objects)))
	 (inheritypes (dom-inheritypes *pspace*))
	 (predicates (dom-predicates *pspace*))
	 (functors (dom-functors *pspace*))
	 (inheritobjects (buildall-inheritobjects objects inheritypes))
	 (patterns (build-patterns predicates functors inheritobjects))
	 (init-def (find-argument problem-def :init))  
	 (init-state (translate-state init-def inheritobjects predicates functors patterns))
	 (goals-def (find-argument problem-def :goal))
	 (goals (translate-goals (car goals-def) inheritobjects predicates functors patterns))
	 (metric (find-argument problem-def :metric))
	 (constraints (car (find-argument problem-def :constraints))))
    (pattern-functor-update patterns init-state)
    (setf *current-problem* (make-say-problem :name problem
					  :domain prob-domain
					  :objects objects
					  :inheritobjects inheritobjects
					  :patterns patterns
					  :init-state init-state
					  :goals (car goals)
					  :numeric-goals (cdr goals)
					  :metric metric
					  :lit-init-state init-def
					  :lit-goals (if (eq 'and (caar goals-def))
							 (cdar goals-def)
						       goals-def)
					  :constant-poslist (build-constant-poslist inheritobjects)
					  ))
    ;; for constraints handling
    (setf (getf (problem-plist *current-problem*) :constraints) constraints)
    (setf *constraintsp* (and constraints t))
    ;;for new representation
    (update-initstate-for-nots)
    (update-initstate-for-equality)
    (parse-initial-state init-def)
    (parse-initial-goals (problem-lit-goals *current-problem*))
    (set-numvars-artificial)
    (instantiate-numgoals)
    ))


(defun read-pddl-problem (file)
  (cond ((dom-p *pspace*)
	 (with-open-file (istream file :direction :input)
	   (let ((pddl-def (read istream)))
	     (if (listp pddl-def)
		 (cond ((eq 'problem (car (second pddl-def)))
			(parse-problem (cdr pddl-def)))
		       (t
			(error (msg-error 'not_pddl_problem 'read-pddl-problem))))
		 (error (msg-error 'bad_list_construction 'read-pddl-problem))))))
	(t (error "SAYPHI Error: There is no loaded domain! [read-pddl-problem]"))))
