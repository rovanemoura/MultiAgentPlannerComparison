;;; This file contains planner independent code related to control rules.

(in-package "COMMON-LISP-USER")
(unless (find-package "PRODIGY4") (make-package "PRODIGY4" :nicknames '("P4") :use '("COMMON-LISP-USER")))

;;;***************************************************************
;; Returns the substitution of vars for constants to be applied to the cr from the
;; preconds and the effects
;;
(DEFUN find-substitution (condes)
  (declare (list condes))
  (dolist (conde condes)
    (case (car conde)
      ((prior-goal some-candidate-goals)
       (dolist (pred-op (cadr conde))
	 (sustituir-pred-op pred-op)))
      ((target-goal true-in-state applicable-op current-goal candidate-goal)
       (sustituir-pred-op (cadr conde)))
      ((select reject)
       (case (cadr conde)
	 (bindings (dolist (cte (mapcar #'cdr (caddr conde)))
		     (push-new-bind-pair cte)))
	 (goals (sustituir-pred-op (caddr conde)))
	 (t nil)))
      (t nil))))

;;
;; For all constants in the pred-op (predicate or operator), it adds a
;; binding for it and pushes it into the *substitution* list
;;
(DEFUN sustituir-pred-op (pred-op)
  (dolist (cte (cdr (get-pos-assertion pred-op)))
    (push-new-bind-pair cte)))

;; Returns the positive literal
(DEFUN get-pos-assertion (assertion)
  (if (negatedp assertion)
      (cadr assertion)
      assertion))

(DEFUN negatedp (condition)
    (eq (car condition) `~))


;;
;; Pushes the pair (new-var . cte) in the list of substitutions
;;
(DEFUN push-new-bind-pair (cte)
  (if (not (member cte *substitution* :key #'car :test #'eq))
      (push (cons cte (new-var-name cte)) *substitution*)))

;; Returns a new name for a variable in the control rule.
(DEFUN new-var-name (cte)
  (case *new-vars-format*
      (prodigy (incf *number-vars-in-cr*)
	       (let ((objectp (and (symbolp cte)
				   (p4::object-name-to-object cte
							      *current-problem-space*))))
		 (intern (format nil "~:@(<~a-~d-~d>~)"
				 (cond ((numberp cte) 'infinite)
				       (objectp (get-object-type cte))
				       (t 'dummy))
				 *rule-id*
				 *number-vars-in-cr*))))
    (pdl2mtarff (intern (format nil "~(~a~)" (code-char (incf *var-code*)))))
    (sayphi (intern (format nil "~:@(<~a>~)" cte)))
    (otherwise (intern (concatenate 'string "var" (format nil (string-trim "#:G" (gensym))))))))

;;;***************************************************************
;;
;; Returns the name of a new rule, according to the day, month, type
;; and gensym, and the problem. This comes from Juan Pedro's code
;;
(DEFUN name-cr (effects operator)
  (multiple-value-bind (sec min hor dia mes ano) (get-decoded-time)
    (declare (ignore sec min hor ano))
    (let ((car1 (case (truncate dia 10)
		  ( 0 '|a|)
		  ( 1 '|e|)
		  ( 2 '|o|)
		  ( 3 '|i|)))
	  (car2 (case (mod dia 10)
		  ( 0 '|l|)
		  ( 1 '|n|)
		  ( 2 '|r|)
		  ( 3 '|s|)
		  ( 4 '|b|)
		  ( 5 '|f|)
		  ( 6 '|k|)
		  ( 7 '|p|)
		  ( 8 '|t|)
		  ( 9 '|x|)))
	  type subtype)
      (cond ((cdr effects)
	     (setq type (car effects))
	     (setq subtype (case (cadr effects)
			     (bindings (format nil "bind-~a" operator))
			     (goals (caaddr effects))
			     (operators (caddr effects))
			     (instantiated-operator (caaddr effects)))))
	    (t (setq type 'decide)
	       (setq subtype (car effects))))
      (intern (format nil "~:@(~a-~a-~a-~a~a~a-~a~)" type subtype
		      *problem-name* (month-string mes)
		      car1 car2 *rule-id*)))))

;;;
;;; Funcion para devolver tres letras del mes
;;;
(defun month-string (mes)
  (nth (1- mes) '("Ene" "Feb" "Mar" "Abr" "May" "Jun" "Jul" "Ago" "Sep" "Oct" 
		  "Nov" "Dec")))

;; Para el TI Explorer: (substring (time:month-string mes) 0 3)
;; Para el TI Explorer: (substring (gensym) 1)

;; Taken from prodigy, so that we do not have to load the whole prodigy for this
(defun p4::is-variable-p (sym)
   (and (symbolp sym)
	(char= #\< (char (symbol-name sym) 0))))

;;;***************************************************************
;; It parametrizes by substitution of vars for constants
;;;***************************************************************
(DEFUN parametrize (expression)
  (declare (list expression))
  (sublis *substitution* expression))


;;;***************************************************************
;;       Several general unification algorithms
;;;***************************************************************

;;; Standard unification algorithm according to Rich&Knight
;;; var-language can be either PDL (<x>), or PDDL (?x) for now.
(DEFUN unify (exp1 exp2 &optional (var-language 'pdl))
  (if (or (atom exp1) (atom exp2))
      (cond ((eq exp1 exp2) nil)
	    ((variablep exp1 var-language)
	     (list (cons exp1 exp2)))
	    ((variablep exp2 var-language)
	     (list (cons exp2 exp1)))
	    (t 'fail))
      (if (eq (car exp1) (car exp2))
	  (if (= (length exp1) (length exp2))
	      (let ((substitution nil))
		(do ((e1 exp1 (cdr e1))
		     (e2 exp2 (cdr e2)))
		    ((or (null e1)
			 (eq substitution 'fail))
		     substitution)
		  (let ((binding (unify (car e1) (car e2))))
		    (cond ((eq binding 'fail)
			   (setq substitution 'fail))
			  (t (setq e1 (sublis binding e1))
			     (setq e2 (sublis binding e2))
			     (setq substitution (append binding
							substitution)))))))
	      'fail)
	  'fail)))

;;;
;;; Standard unification algorithm according to Rich&Knight, modified
;;; so that Prodigy variables are constants
;;;
(DEFUN new-unify (exp1 exp2)
  (cond ((or (atom exp1) (atom exp2))
	 (if (eq exp1 exp2) nil 'fail))
	((and (listp (car exp1)) (listp (car exp2)))
	 (let ((binding (new-unify (car exp1) (car exp2))))
	   (if (eq binding 'fail)
	       'fail
	       (new-unify (sublis binding (cdr exp1))
			  (sublis binding (cdr exp2))))))
	((eq (car exp1) (car exp2))
	 (if (= (length exp1) (length exp2))
	     (let ((substitution nil))
	       (do ((e1 exp1 (cdr e1))
		    (e2 exp2 (cdr e2)))
		   ((or (null e1)
			(eq substitution 'fail))
		    substitution)
		 (let ((binding (new-unify (car e1) (car e2))))
		   (cond ((eq binding 'fail)
			  (setq substitution 'fail))
			 (t (setq e1 (sublis binding e1))
			    (setq e2 (sublis binding e2))
			    (setq substitution
				  (append binding substitution)))))))
	     'fail))
	(t 'fail)))

;;; Unification algorithm similar to Rich&Knight, modified so that expressions
;;; can have any depth, and supposing that exp2 does not have constants where
;;; exp1 has variables. It returns a substitution to be applied to exp2
;;; var-language can be either PDL (<x>), or PDDL (?x)
(DEFUN unify-any-depth (exp1 exp2 &optional (var-language 'pdl))
  (if (or (atom exp1) (atom exp2))
      (cond ((eq exp1 exp2) nil)
	    ((variablep exp2 var-language)
	     (list (cons exp2 exp1)))
	    (t 'fail))
      (if (= (length exp1) (length exp2))
	  (do ((substitution nil)
	       (binding nil)
	       (e1 exp1 (cdr e1))
	       (e2 exp2))
	      ((or (eq substitution 'fail)
		   (null e1))
	       substitution)
	    (setq binding (unify-any-depth (car e1) (car e2)))
	    (cond ((eq binding 'fail)
		   (setq substitution 'fail))
		  (t (setq e2 (sublis binding (cdr e2)))
		     (setq substitution (nconc substitution binding)))))
	  'fail)))

;; Useful for knowing whether they unify or not
(defun unify-any-depth-p (exp1 exp2)
  (not (eq 'fail (unify-any-depth exp1 exp2))))

(defun variablep (expression var-language)
  (if (eq var-language 'pdl)
      (p4::is-variable-p expression)
      (char= #\? (elt (format nil "~a" expression) 0))))


;;; *******************************************************************
;;;                 pprinting functions
;;; *******************************************************************

(defun pp-cr (name preconds effects stream)
  (format stream "~2%(control-rule ~a" name)
  (format stream "~%  (if (and ~(~a~)" (car preconds))
  (dolist (precond (cdr preconds))
      (format stream "~%           ~(~a~)" precond))
  (format stream "))")
  (if (and (= (length effects) 4)
	   (or (eq (caddr effects) 'bindings)
	       (eq (caddr effects) 'instantiated-operator)))
      (format stream "~%  (~(~a ~a ~a~%        ~a~)))"
	      (car effects) (cadr effects) (caddr effects) (cadddr effects))
      (format stream "~%  ~(~a~))" effects)))
