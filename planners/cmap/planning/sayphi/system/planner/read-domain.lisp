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
;; Description: PDDL Domain Parser
;; Date: 2005.12.04
;; 
;; ========================================================================

(defvar *say-parse-errors* nil)
(defparameter *pddl-vars* nil)

(defmacro negative-var-p (var)
  `(when (listp ,var) 
    (or (find (car ,var) (dom-negative-vars *pspace*))
	(and (say-problem-p *current-problem*)
	     (find (car ,var) (problem-negative-vars *current-problem*))))))


(defmacro functor-p (var)
  `(and (listp ,var) (hkey-present (car ,var) (dom-functors *pspace*))))


(defmacro find-argument (pddl-def argument)
  `(cdr (assoc ,argument ,pddl-def)))


(defmacro negate-var (functor)
  `(intern (format nil "_~a" (symbol-name ,functor))))

(defmacro equality-var (functor)
  `(intern (format nil "=~a" (symbol-name ,functor))))

(defun find-all-argument (pddl-def argument)
  (remove-if-not #'(lambda (definition)
		     (eq (car definition) argument))
		 pddl-def))

(defun show-parse-errors ()
  (dolist (ierr *say-parse-errors*)
    (format t "~% Parse Error: ~a" ierr)))


;; PDDL Vars are stored in a hash table because a profiling trace showed
;; that the contiuous repetition of interns degrade the parsing performance
(defun convert-pddl-var (pddl-var)
  (or (gethash pddl-var *pddl-vars*)
      (setf (gethash pddl-var *pddl-vars*)
	    (intern (string-left-trim '(#\?) (format nil "~a" pddl-var))))))



(defun equality-pred-p (var)
  (eq #\= (elt (format nil "~a" var) 0)))
(defun negative-pred-p (var)
  (eq #\_ (elt (format nil "~a" var) 0)))

(defun convert-special-var (var)
  (intern (subseq (format nil "~a" var) 1)))

;; It checks if an argument of the literal is a cons of var-param or a domain constant
(defun literal-arg-p (arg arg-var)
  (let ((found-constant)
	(vartype (cdr arg-var)))
    (cond ((consp arg) t)
	  ((find arg (gethash vartype (dom-constants *pspace*))) t)
	  ((dom-inheritypes *pspace*)
	   (dolist (itype (second (assoc vartype (dom-inheritypes *pspace*))) found-constant)
	     (when (find arg (gethash itype (dom-constants *pspace*)))
	       (setf found-constant t))))
	  (t (push (format nil "! ~a is not a domain constant" arg)
*say-parse-errors*)))
	   ))

;; (defun literal-arg-p (arg arg-var)
;;   (cond ((consp arg))
;; 	(t 
;; 	 (cond ((find arg (gethash (cdr arg-var) (dom-constants *pspace*))) t)
;; 	       (t (push (format nil "! ~a is not a domain constant" arg) *say-parse-errors*)))
;; 	       )))
	


(defun convert-literal (literal parameters &optional (litype :predicate))
  (let ((litschema (cond ((eq litype :predicate) (dom-predicates *pspace*))
			 ((eq litype :functors) (dom-functors *pspace*))))
	(literal-args (mapcar #'(lambda (var)
				  (let ((say-var (convert-pddl-var var)))
				    (cond ((assoc say-var parameters))
					  (t var))))
			      (cdr literal))))
    (cond ((hkey-present (car literal) litschema)
	   (when (every #'literal-arg-p literal-args (gethash (car literal) litschema)) 
	     (cons (car literal) literal-args)))
	  (t (push (format nil " - ~a - is not a ~a in the domain" (car literal) litype) *say-parse-errors*) 
	     nil)
	  )))


(defun get-inherit-types (dtype types-def)
  (let ((dtype-def (car (find-argument types-def dtype)))
	(result-types (list dtype)))
      (dolist (this-type dtype-def result-types)
	(if (find-argument types-def this-type)
	    (setf result-types (append result-types 
				       (get-inherit-types this-type types-def)))
	    (setf result-types (append result-types (list this-type)))))))


(defun buildall-inherit-types (types-def)
  (let ((inherit-types nil))
    (dolist (itype types-def inherit-types)
      (when (listp itype)
	(push (list (car itype) (get-inherit-types (car itype) (remove-if-not #'listp types-def))) inherit-types)))))



(defun translate-types-def (types)
  (let ((read-types nil) (sub-types nil) 
	(top-types (remove '- types)))
  (do* ((prev-type (car types) type)
	(rest-types (cdr types) (cdr rest-types))
	(type (car rest-types) (car rest-types)))
      ((null rest-types) nil)
    (cond ((eq prev-type '-)
	   (push (cons type (list (reverse read-types))) sub-types)
	   (setf top-types (remove type top-types))
	   (setf read-types nil)
	   (setf rest-types (cdr rest-types))
	   (setf type (car rest-types)))
	  (t
	   (push prev-type read-types)
	   (setf top-types (remove prev-type top-types)))))
;;   (format t "~% sub-types ~a read-types ~a top-types ~a" sub-types read-types top-types)
  (append (reverse sub-types) top-types read-types)
;;   (if sub-types
;;       (reverse sub-types)
;;     (append top-types read-types))
))


;; Like translate-types-def. In this case some definitions could be repeat, so i put them in a 
;; hash table directly
(defun translate-objects-def (objects-def)
  (let ((ht-objects (make-hash-table :test #'eq))
	(read-objects nil))
  (do* ((prev-object (car objects-def) object)
	(rest-objects (cdr objects-def) (cdr rest-objects))
	(object (car rest-objects) (car rest-objects)))
      ((null rest-objects) ht-objects)
    (cond ((eq prev-object '-)
	   (setf (gethash object ht-objects)
		 (append (gethash object ht-objects) (reverse read-objects)))
	   (setf read-objects nil)
	   (setf rest-objects (cdr rest-objects))
	   (setf object (car rest-objects)))
	  (t
	   (push prev-object read-objects))))))


(defun compute-params (params-def)
  (let ((translated-params (translate-types-def params-def))
	(params-types nil))
    (dolist (i-param translated-params (nreverse params-types))
      (dolist (i-var (cadr i-param))
	(push (cons (convert-pddl-var i-var) (car i-param))
	      params-types)))))


(defun parse-predicates (preds-def)
  (let ((thash (make-hash-table :test #'eq)))
    (mapc #'(lambda (ipred) 
	      (setf (gethash (car ipred) thash) (compute-params (cdr ipred))))
	  preds-def)
    thash))


(defun convert-function (function parameters functors)
  (mapcar #'(lambda (formula) (cond ((or (atom formula) (numberp formula))
				     formula)
				    ((gethash (car formula) functors)
				     (convert-literal formula parameters :functors))
				    (t
				     (convert-function formula parameters functors))))
	  function))


(defun parse-preconds (preconds this-action predicates functors)
  (cond ((null preconds) nil)
	((or (and (eq (car preconds) 'over)
		  (eq (cadr preconds) 'all))
	     (and (eq (car preconds) 'at)
		  (or (eq (cadr preconds) 'start)
		      (eq (cadr preconds) 'end))))
	 (parse-preconds (caddr preconds) this-action predicates functors))
	
	((member (car preconds) '(and or))
	  (dolist (iprecond (cdr preconds))
	    (parse-preconds iprecond this-action predicates functors)))

	((hkey-present (car preconds) predicates)
	 (push (convert-literal preconds (action-parameters this-action))
	       (action-preconditions this-action)))
	
;; 	negative preconditions and equality
	((and (eq (car preconds) 'not) (listp (second preconds)))
	 (cond ((hkey-present (car (second preconds)) predicates)
		(let ((neg-predicate (negate-var (car (second preconds)))))
		  (unless (hkey-present neg-predicate predicates)
		    (push (car (second preconds)) (dom-negative-preds *pspace*))
		    
		    (setf (gethash neg-predicate predicates)
			  (gethash (car (second preconds)) predicates)))
		  (push (convert-literal (cons neg-predicate (cdr (second preconds))) (action-parameters this-action))
			(action-preconditions this-action))))
	       ((eq (car (second preconds)) '=)
		(let* ((equality-vars (mapcar #'convert-pddl-var (cdr (second preconds))))
		       (equality-type (cdr (assoc (car equality-vars) (action-parameters this-action))))
		       (equality-pred (equality-var equality-type)))
		  (unless (hkey-present equality-pred predicates)
		    (push equality-type (dom-special-preds *pspace*))
		    (setf (gethash equality-pred predicates)
			  (list (cons 'x1 equality-type) (cons 'x2 equality-type)))
		    (setf (gethash (negate-var equality-pred) predicates)
			  (list (cons 'x1 equality-type) (cons 'x2 equality-type))))
		  (push (convert-literal (cons (negate-var equality-pred) (cdr (second preconds))) (action-parameters this-action))
			(action-preconditions this-action))))))

	((member (car preconds) '(> < >= <= =))
	 (push (convert-function preconds (action-parameters this-action) functors)
	       (action-precond-funs this-action)))
	(t (error "Precondition literal not in predicate definition [~a]" preconds))  
))



(defun parse-effects (effects this-action predicates functors)
  (cond ((null effects) nil)
	((or (and (eq (car effects) 'over)
		  (eq (cadr effects) 'all))
	     (and (eq (car effects) 'at)
		  (or (eq (cadr effects) 'start)
		      (eq (cadr effects) 'end))))
	 (parse-effects (caddr effects) this-action predicates functors))

	((member (car effects) '(and or))
	 (dolist (ieffect (cdr effects))
	   (parse-effects ieffect this-action predicates functors)))
			 
	((eq (car effects) 'not)
	 (if (hkey-present (caadr effects) predicates)
	     (push (convert-literal (cadr effects) (action-parameters this-action))
		   (action-dels this-action))))

	((hkey-present (car effects) predicates)
	 (push (convert-literal effects (action-parameters this-action))
		   (action-adds this-action)))
	
	((member (car effects) '(increase decrease assign))
	 (push (convert-function effects (action-parameters this-action) functors)
	       (action-costs this-action)))
	
	(t nil)
))


(defun build-action (action-def predicates functors this-action)
    (let* ((params (compute-params (getf action-def :parameters)))
	   (preconds-def (append (getf action-def :precondition)
				 (getf action-def :condition)))
	   (effects-def (getf action-def :effect)))
      (setf (action-parameters this-action) params)
      (parse-preconds preconds-def this-action predicates functors)
      (parse-effects effects-def this-action predicates functors))
    this-action)

(defun check-action-elements (action-def)
  (dolist (element '(:precondition :effect) t)
    (unless (getf action-def element)
      (error "Action definition without ~S [in ~S]" element action-def)))) 

;; when spliting disjunction or conditional effects, actions might have some identifier
;; for instantiation
(defun create-action-name-id (action-name keys-table)
  (cond ((hkey-present action-name keys-table)
	 (incf (gethash action-name keys-table))
	 (intern (format nil "~a~a" action-name (gethash action-name keys-table))))
	(t
	 (setf (gethash action-name keys-table) 1)
	 action-name)))


(defun parse-actions (actions-def predicates functors)
  (let ((this-action nil) 
	(name-keys (make-hash-table :test #'eq))
	(this-name-id nil))
    (dolist (iaction (reverse actions-def))
      (setf this-name-id (create-action-name-id (getf iaction :action) name-keys))
      (setf this-action (make-action :name (getf iaction :action)
				     :name-id this-name-id))
      (push (build-action iaction predicates functors this-action) (dom-actions *pspace*))
)))


;; ===========================================================================
;; Some things for ORs and NOTs in preconditions
(defun update-for-nots ()
  (dolist (i-pred (dom-negative-preds *pspace*))
    (let ((neg-predicate (negate-var i-pred))
	  (relevant-effect nil))
      (dolist (i-action (dom-actions *pspace*))
	(setq relevant-effect (find i-pred (action-adds i-action) :key #'car))
	(when relevant-effect
	  (push (cons neg-predicate (cdr relevant-effect)) (action-dels i-action)))	
	(setq relevant-effect (find i-pred (action-dels i-action) :key #'car))
	(when relevant-effect
	  (push (cons neg-predicate (cdr relevant-effect)) (action-adds i-action)))))))



(defun split-disjunctions (operators predicates)
  (dolist (iop operators)
    (check-action-elements iop))
  (mapcan #'(lambda (action-def)
	      (let* ((name-def (getf action-def :action))
		     (params-def (getf action-def :parameters))
		     (preconds-def (convert-to-dnf (getf action-def :precondition)  predicates))
		     (effects-def (getf action-def :effect)))
		(cond ((or (eq (car preconds-def) 'and)
			   (simple-precond-p preconds-def predicates))
		       (list action-def))
		      ((eq (car preconds-def) 'or)
		       (mapcar #'(lambda (cnf-precond)
				   (list :action name-def
					 :parameters params-def
					 :precondition cnf-precond
					 :effect effects-def))
			       (cdr preconds-def))))))
	  operators))
					 

(defun convert-to-dnf (formula predicates)
  (cond ((precond-dnf-p formula predicates) formula)
	((eq (car formula) 'and)
	 (cons 'or (mapcar (lambda (fml) 
			     (cons 'and fml))
			   (prec-and-to-dnf (cdr formula) predicates))))))



(defun group-of-merge-cnf (formula predicates)
  (cond ((simple-precond-p formula predicates) (list (list formula)))
	((eq (car formula) 'or) (mapcar #'list (cdr formula)))))
 
	
;; This receives a list of sub-formulas of a conjunction
(defun prec-and-to-dnf (formula predicates)
  (cond ((endp (cdr formula)) (group-of-merge-cnf (car formula) predicates))
	(t (merge-cnf (group-of-merge-cnf (car formula) predicates)
		       (prec-and-to-dnf (cdr formula) predicates)))))



;; It permuts the formulas in this way
;; (A) and (b) -> (A B)
;; (A) or (C B) -> (A C) or (A B)
;; (A B) or (C D) -> (A C) or (A D) or (B C) or (B D)
(defun merge-cnf (actual new)
  (let ((result nil))
    (dolist (item actual result)
      (setf result (append result (mapcar (lambda (x) (append item x)) new))))))




(defun precond-dnf-p (formula predicates)
  (or (simple-precond-p formula predicates)
      (and (eq (car formula) 'and)
	   (every (lambda (fml) (simple-precond-p fml predicates)) (cdr formula)))
      (and (eq (car formula) 'or)
	   (every (lambda (fml) (precond-dnf-p fml predicates)) (cdr formula)))))
      

;; (defun simple-precond-p (formula predicates)
;;   (and (consp formula)
;;        (or (hkey-present (car formula) predicates)
;; 	   (and (eq (car formula) 'not)
;; 		(simple-precond-p (second formula) predicates))
;; 	   (find (car formula) '(> < >= <= =))
;; 	   )))
(defun simple-precond-p (formula predicates)
  (declare (ignore predicates))
  (and (consp formula)
       (or (every #'symbolp formula)
	   (find (car formula) '(not > < >= <= =)))))


;; ===========================================================================
;; Vidal - Split operators in two when they have conditional effects
;; DB: transformation into iteration. Recursive def had problems in domains with many actions (23600)
(defun split-whens (operators)
  (mapcan #'(lambda (action-def)
	      (let* ((name-def (getf action-def :action))
		     (params-def (getf action-def :parameters))
		     (preconds-def (append (getf action-def :precondition) (getf action-def :condition)))
		     (effects-def (getf action-def :effect)))
		(if (eq (car effects-def) 'when)
		    (list (list :action name-def 
				:parameters params-def 
				:precondition (add-clauses preconds-def (second effects-def))
				:effect (third effects-def)))
		    (let ((conditional-clause (get-conditional-clause effects-def)))
		      (cond (conditional-clause
			     (split-whens (list (list :action name-def 
						      :parameters params-def 
						      :precondition (add-clauses preconds-def (negate-clauses (second conditional-clause)))
						      :effect (remove conditional-clause effects-def))
						(list :action name-def 
						      :parameters params-def 
						      :precondition (add-clauses preconds-def (second conditional-clause) ) 
						      :effect (add-clauses (remove conditional-clause effects-def) (third conditional-clause))))))
			    (t (list action-def)))))))
	  operators))

;; (defun split-whens (operators)
;;   (let* ((action-def (car operators))
;; 	(name-def (getf action-def :action))
;; 	 (params-def (getf action-def :parameters))
;; 	 (preconds-def (append (getf action-def :precondition) (getf action-def :condition)))
;; 	 (effects-def (getf action-def :effect)))
;;     (if	(eq (car effects-def) 'when)	; only one effect and it is conditional
;; 	(cons (list ':action name-def 
;; 		    ':parameters params-def 
;; 		    ':precondition (add-clauses preconds-def (second effects-def) ) 
;; 		    ':effect (third effects-def)) 
;; 	      (split-whens (cdr operators)))
;; 	
;; 		; otherwise
;; 	(let ((conditional-clause (get-conditional-clause effects-def)))
;; 	  (cond	((endp operators) nil)
;; 		((identity conditional-clause)
;; 		 (split-whens (append (cdr operators) 
;; 				      (list (list ':action name-def 
;; 						  ':parameters params-def 
;; 						  ':precondition (add-clauses preconds-def (negate-clauses (second conditional-clause)))
;; 						  ':effect (remove conditional-clause effects-def)))
;; 				      (list (list ':action name-def 
;; 						  ':parameters params-def 
;; 						  ':precondition (add-clauses preconds-def (second conditional-clause) ) 
;; 						  ':effect (add-clauses (remove conditional-clause effects-def) (third conditional-clause)) ) )
;; 						)
;; 			      )
;; 		  )
;; 		(t (cons action-def (split-whens (cdr operators))))
;; 		)
;; 	  )
;; 	)
;;     )
;; )

(defun get-conditional-clause (effects)
	(cond	((endp effects) nil)
		((member (car effects) '(and or))
			(get-conditional-clause (cdr effects)))
		((not (listp (car effects))) nil)
		((eq (caar effects) 'when)
			(car effects))
		(t (get-conditional-clause (cdr effects)))
	)
)

(defun add-clauses-aux (list-clauses clauses)
	(if (eq (car list-clauses) 'and)
		(if (eq (car clauses) 'and)
			(append list-clauses (cdr clauses))
			(append list-clauses (list clauses))
		)
		(if (eq (car clauses) 'and)
			(append clauses (list list-clauses))
			(list 'and list-clauses clauses)
		)
	)
)

(defun add-clauses (list-clauses clauses)
  (remove-duplicates (add-clauses-aux list-clauses clauses) :test #'equal))


(defun negate-clauses (clauses)
	(cond ((eq (car clauses) 'and) (cons 'or (mapcar #'negate-clauses (cdr clauses))) )
	      ((eq (car clauses) 'or) (cons 'and (mapcar #'negate-clauses (cdr clauses))) )
	      ((eq (car clauses) 'not) (cadr clauses) )
	      ((eq (car clauses) '<) (cons '>= (cdr clauses)) )
	      ((eq (car clauses) '<=) (cons '> (cdr clauses)) )
	      ((eq (car clauses) '>) (cons '<= (cdr clauses)) )
	      ((eq (car clauses) '>=) (cons '< (cdr clauses)) )
	      (t (list 'not clauses))
	)
)


;; ==================================================================
;; Additional parse-domain functions to deal with metrics
;; I put this in here rather than metric file because is easier 
;; to understand what i am doing here

(defun set-metric-total-time()
  (when (and (find :fluents (dom-requirements *pspace*))
	     (not (hkey-present 'total-time (dom-functors *pspace*))))
    (setf (gethash 'total-time (dom-functors *pspace*)) nil)))


(defun get-real-functors ()
  (let ((functors (dom-functors *pspace*))
	(real-functors nil))
    (dolist (iaction (dom-actions *pspace*) real-functors)
      (dolist (icost (action-costs iaction))
	(when (hkey-present (car (second icost)) functors)
	  (unless (find (car (second icost)) real-functors)
	    (push (car (second icost)) real-functors)))))))


;; Calculate positive or negative vars for a generic precond-function
(defun extract-signed-vars (precfun sign functors)
  (cond ((atom precfun)
	 nil)
	((hkey-present (car precfun) functors)
	 (unless sign
	   (list (car precfun))))
	((eq '- (car precfun))
	 (append (extract-signed-vars (second precfun) sign functors)
		 (extract-signed-vars (third precfun) (not sign) functors)))
	(t
	 (append (extract-signed-vars (second precfun) sign functors)
		 (extract-signed-vars (third precfun) sign functors)))))

;; The step 1 changes (exp <[<=] exp')
(defun transform-lnf-step1 (prec-fun)
  (cond ((eq '< (car prec-fun))
	 (list '> (third prec-fun) (second prec-fun)))
	((eq '<= (car prec-fun)) 
	 (list '>= (third prec-fun) (second prec-fun)))
	(t prec-fun)))


;; The step 2 changes (exp >[>=] exp') to (exp - exp' >[>=] 0)
(defun transform-lnf-step2 (prec-fun)
  (cond ((or (eq '> (car prec-fun))
	     (eq '>= (car prec-fun)))
	 (list (car prec-fun) (list '- (second prec-fun) (third prec-fun)) 0))
	(t prec-fun)))


;; This set of functions translate preconditions-functions to de LNF (linear function)
;; in their normal-form explained by Hoffman.
(defun translate-precfuns()
  (let ((actions (dom-actions *pspace*)))
    (dolist (iaction actions)
      (unless (null (action-precond-funs iaction))
	(setf (action-precond-funs iaction)
	      (mapcar #'(lambda (i-precfn)
			  (transform-lnf-step2 (transform-lnf-step1 i-precfn)))
		      (action-precond-funs iaction)))))))




;; Calculates the positive and negative vars for relaxing the planning graph with costs
;; This should be made after the LNF transformation
(defun separate-functors-torelax ()
  (let ((positive-vars nil) 
	(negative-vars nil)
	(functors (dom-functors *pspace*))
	(real-functors (get-real-functors))) 
    (dolist (iaction (dom-actions *pspace*))
      (dolist (i-precfun (action-precond-funs iaction))
	(setf positive-vars 
	      (union positive-vars (extract-signed-vars (second i-precfun) 'nil functors)))
	(setf negative-vars 
	      (union negative-vars (extract-signed-vars (second i-precfun) 't functors)))))
    (setf (dom-real-functors *pspace*) real-functors)
    (setf (dom-positive-vars *pspace*)
	  (mapcan #'(lambda (x) 
		      (when (find x real-functors) (list x))) 
		  positive-vars))
    (setf (dom-negative-vars *pspace*)
	  (mapcan #'(lambda (x) 
		      (when (find x real-functors) 
			(list x))) 
		  negative-vars))))


;; It takes negative vars and introduce artificial variables to hold the opposite values
;; in the state for the relaxing phase
(defun create-artificial-vars()
  (dolist (ivar (dom-negative-vars *pspace*))
    (let ((inegvar (negate-var ivar)))
      (push inegvar (dom-artificial-vars *pspace*))
      (setf (gethash inegvar (dom-functors *pspace*)) (gethash ivar (dom-functors *pspace*)))
      )))
   



;; (defun update-for-artificial-preconds()
;;   (dolist (iaction (dom-actions *pspace*))
;;     (dolist (iprecfun (action-precond-funs iaction))
;;       (setf (second iprecfun) (artificial-fun-exp (second iprecfun) t)))))


      

;; ==================================================================


;; The domain structure is assigned to the global variable *pspace*
(defun parse-domain (arguments)
  (setf *say-parse-errors* nil)
  (setf *pddl-vars* (make-hash-table))
  (let* ((domain (find-argument arguments 'domain))
	 (requirements (find-argument arguments :requirements))
	 (dtypes (translate-types-def (find-argument arguments :types)))
	 (inheritypes (buildall-inherit-types dtypes))
	 (predicates (parse-predicates (find-argument arguments :predicates)))
	 (functors (parse-predicates (find-argument arguments :functions)))
	 (actions-def (append (find-all-argument arguments :action)
			  (find-all-argument arguments :durative-action)))
	 )
    (setf *pspace* (make-dom :name domain
			     :requirements requirements
			     :domtypes dtypes
			     :inheritypes inheritypes
			     :predicates predicates
			     :functors functors
                             ))
    (setf (dom-constants *pspace*)
	  (translate-objects-def (find-argument arguments :constants)))
    (parse-operators actions-def)
    (parse-actions (split-whens (split-disjunctions actions-def predicates)) predicates functors)
;;     (parse-actions actions-def predicates functors)
    (update-for-nots)
    (create-predicate-table)
    (collect-top-inertia)
    (translate-precfuns)
    (set-metric-total-time)
    (separate-functors-torelax)
    (create-artificial-vars)
;;     (update-for-artificial-preconds)
;;     (insert-artificial-costs)
))


(defun read-pddl-domain (file)
    (with-open-file (istream file :direction :input)
	(let ((pddl-def (read istream)))
	  (if (listp pddl-def)
	      (cond ((eq 'domain (car (second pddl-def)))
		     (parse-domain (cdr pddl-def))
		     (unless (null *say-parse-errors*)
		       (setf *pspace* nil)
		       (show-parse-errors))
		     ))))))


(defun read-file (file)
  (with-open-file (istream file :direction :input)
    (read istream)))




      
      
