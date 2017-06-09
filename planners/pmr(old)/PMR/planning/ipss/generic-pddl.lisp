(in-package "COMMON-LISP-USER")
#+(or allegro sbcl clisp) (asdf:oos 'asdf:load-op "cl-ppcre")
;;  (require 'cl-ppcre)

;;; **************************************************************************************
;;;              Writing PDDL problem and domain files
;;; **************************************************************************************

(defun write-domain-pddl-file (name requirements types predicates functions actions file &optional constants)
  (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "(define (domain ~(~a~))" name)
    (if requirements (format stream "~%  ~(~s~)" `(:requirements ,@requirements)))
    (if types (format stream "~%  ~(~s~)" `(:types ,@types)))
    (if constants (format stream "~%  ~(~s~)" `(:constants ,@constants)))
    (if predicates (format stream "~%  ~(~s~)" `(:predicates ,@predicates)))
    (if functions (format stream "~%  ~(~s~)" `(:functions ,@functions)))
    (dolist (action actions)
      (if (nth 3 action)
	  (format stream "~2%  ~(~s~)" action)
	  (format stream "~2%  (:action ~(~s~)~%   :parameters ()~%   :precondition ~(~s~)~%    :effect ~(~s~))" (nth 1 action) (nth 5 action) (nth 7 action))))
    ;; because I do not want parameters to be saved as nil
    ;;    (pp-list actions 2 stream t nil)
    (format stream "~%)"))
  file)

(defun write-pddl-file (name domain-name objects state goals file &optional metric constraints comment)
  (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (if comment (format stream "~a~2%" comment))
    (format stream "(define (problem ~(~a~)) (:domain ~(~a~))" name domain-name)
    (format stream "~%  (:objects")
    (do ((instances objects (cdr instances)))
	((null instances))
      (cond ((eq (car instances) '-)
	     (format stream " - ~(~a~)~%~12T" (cadr instances))
	     (setq instances (cdr instances)))
	    (t (format stream " ~(~a~)" (car instances)))))
    (format stream ")")
    (format stream "~%  (:init")
    (pp-list state 7 stream t)
    (format stream ")")
    (format stream "~%  (:goal (and ")
    (pp-list (cond ((eq (car goals) 'and)
		    (cdr goals))
		   ((listp (car goals)) goals)
		   (t (list goals)))
	     11 stream t)
    (format stream "))")
    (if metric
	(format stream "~%  ~s" (cons :metric metric)))
    (if constraints
	(format stream "~%  ~s" (list :constraints constraints)))
    (format stream ")")))

;;; **************************************************************************************
;;;              Getting information from PDDL definitions
;;; **************************************************************************************


;; property can be either: problem, :domain, :objects, :init or :goal
(defun pddl-problem-property (property pddl-problem-description)
  (funcall (if (member property '(problem :domain :goal)) #'cadr #'cdr)
	   (assoc property (cdr pddl-problem-description))))

;; More or less the same as before. I will merge them.
(defun find-argument (domain argument)
  (cdr (assoc argument domain)))

(defun find-all-argument (domain argument)
  (remove-if-not #'(lambda (definition)
		     (eq (car definition) argument))
		 domain))

(defun give-me-all-goals (problem-def)
  (let ((goals-in-problem (find-argument problem-def :goal)))
    (if (eq (caar goals-in-problem) 'and)
	(cdar goals-in-problem)
	goals-in-problem)))

(defun give-me-all-actions (domain-def)
  (append (find-all-argument domain-def :action)
	  (find-all-argument domain-def :durative-action)))

;;; **************************************************************************************
;;;              Processing instances, parameters, types
;;; **************************************************************************************

;; returns a list of sublists of the form ((type1 instance1 ... instanceN) ... (typeM instance1 ... instanceR))
(defun process-instances (dirty-instances)
  (do ((instances (cleanup-params dirty-instances))
       (current-instances nil)
       (final-instances nil)
       (instance nil)
       (type nil)
       (x 0 (1+ x)))
      ((> x (length instances)) final-instances)
    (setq instance (nth x instances))
    (cond ((or (eq instance '-) (null instance))
	   (when (or instance current-instances)
	     (setq type (if (null instance)
			    'object
			    (nth (incf x) instances)))
	     (if (assoc type final-instances)
		 (dolist (current-instance current-instances)
		   (push current-instance (cdr (assoc type final-instances))))
		 (push (cons type (reverse current-instances))
		       final-instances))
	     (setq current-instances nil)))
	  (t (push instance current-instances)))))

(defun cleanup-params (parameters)
  (do* ((params parameters (cdddr params))
	(new-params nil))
       ((null params) (nreverse new-params))
    (cond ((eq (cadr params) '-)
	   (push (car params) new-params)
	   (push (cadr params) new-params)
	   (push (caddr params) new-params))
	  (t (do* ((rest-params params (cdr rest-params))
		   (vars nil))
		  ((or (null rest-params) (eq (cadr rest-params) '-))
		   (cond ((null rest-params)
			  (dolist (var (reverse vars))
			    (push var new-params))
			  (setq params nil))
			 (t (dolist (var (reverse (cons (car rest-params) vars)))
			      (push var new-params)
			      (push '- new-params)
			      (push (caddr rest-params) new-params))
			    (setq params rest-params))))
	       (push (car rest-params) vars))))))

(defun flatten-instances (instances-list)
  (let ((instances nil))
    (dolist (type-instances instances-list)
      (setq instances (append instances (cdr type-instances) (list '- (car type-instances)))))
    instances))

;; I assume both sets have been processed with process-instances, and they could later by handled by flatten-instances
(defun merge-instances (instances old-instances)
  (dolist (instance-def instances old-instances)
    (if (assoc (car instance-def) old-instances)
	(setf (cdr (assoc (car instance-def) old-instances))
	      (union (cdr (assoc (car instance-def) old-instances))
		     (cdr instance-def) :test #'eq))
	(push instance-def old-instances))))

;; It takes as input the types directly from the domain file and returns a list of sublists
;; ((object supertype1 ...) (supertype1 subtype1 ...) ...)
(defun process-types (types)
  (let ((type-hierarchy (process-instances types))
	(object-def nil))
    (cond ((not type-hierarchy)
	   (setq type-hierarchy (list (list 'object))))
	  ((not (eq (caar type-hierarchy) 'object))
	   (setq object-def (assoc 'object type-hierarchy))
	   (setq type-hierarchy (cons object-def (remove object-def type-hierarchy :test #'equal))))
	  (t nil))
    (dolist (type-def (cdr type-hierarchy))
      (if (not (direct-super-type (car type-def) type-hierarchy))
	  (setf (cdar type-hierarchy)
		(cons (car type-def) (cdar type-hierarchy)))))
    type-hierarchy))

;; types-def should be the output of process-types
(defun direct-super-type (type type-defs)
  (caar (member type type-defs :test #'(lambda (the-type type-def) (member the-type (cdr type-def))))))

;; types-def should be the output of process-types
(defun super-types (type types-def)
  (if (eq type 'object)
	(list 'object)
	(let ((direct (direct-super-type type types-def)))
	  (if (eq direct 'object)
	      (list direct)
	      (cons direct (super-types direct types-def))))))

;; same as above but receives a list of types as input, instead of only one type
;; types-def should be the output of process-types
(defun super-types-of-set (types types-def)
  (if types
      (union types (reduce #'union (mapcar #'(lambda (atype) (super-types atype types-def)) types)))))

;; Computes the set of types that are subtypes of types in types-def
;; Assumes types-def has been computed with process-types
(defun sub-types (types types-def)
  (do* ((the-types types-def (if changep types-def (cdr the-types)))
	(changep nil nil)
	(new-sub-types types))
       ((null the-types) (set-difference new-sub-types types))
    (when (and (member (caar the-types) new-sub-types)
	       (not (subsetp (cdar the-types) new-sub-types)))
      (setq new-sub-types (union (cdar the-types) new-sub-types))
      (setq changep t))))

;; Computes the leaf types of a hierarchy of types
(defun leaf-types (types-def)
  (set-difference (reduce #'append types-def :key #'cdr) (mapcar #'car types-def)))

;; I assume object def is the first one (according to what process-types returns)
(defun flatten-types (types)
  (flatten-instances (append (cdr types) (list (car types)))))

(defun merge-types (types1 types2)
  (merge-instances types1 types2))

;;; **************************************************************************************
;;;              Managing solutions
;;; **************************************************************************************

;; Reimplementation from pale/planner-client.lisp
(defun get-solution-from-planner (planner &optional (from-file-p t) (solution-file "solution.txt"))
  (if from-file-p
      (get-solution-from-file planner solution-file)
      (case planner
	(ipss (car (pp-solution :format 'list :include-pp-solution-p t)))
	(sayphi (pp-solution-sayphi))
	(metric-ff (car (solutions-ff :ff-results-file solution-file)))
	(t nil))))

(defun get-solution-from-file (planner solution-file)
  (if (or (eq planner 'sayphi) (eq planner 'ipss))
      (let ((solution nil))
	(with-open-file (istream solution-file :direction :input)
	  (setq solution (read istream)))
	solution)
      (pddl-solution-to-lisp solution-file)))

(defun solution-from-file (solution-file)
  (let ((solution nil)
	(*readtable* (copy-readtable nil)))
    (with-open-file (istream solution-file :direction :input)
      (set-macro-character #\: #'skip-it)
      (setq solution (read istream)))
    (if (listp solution)
	solution
	(pddl-solution-to-lisp solution-file))))

(defun skip-it (stream char)
  char)

(defun pddl-solution-to-lisp (solution-file)
  (let ((matching-closure-actions (cl-ppcre:create-scanner "([0-9])+: ([-_ ()0-9a-zA-Z]+)" :case-insensitive-mode t))
	(*readtable* (copy-readtable nil))
	(solution nil))
    (set-macro-character #\: #'skip-it)
    (with-open-file (istream solution-file :direction :input)
      (do* ((line (read-line istream nil 'eof)
		  (read-line istream nil 'eof)))
	   ((eq line 'eof))
;; 	(format t "~%Line: ~a" line)
	(multiple-value-bind (result array)
	    (cl-ppcre:scan-to-strings matching-closure-actions line)
	  (when array
	    (do* ((action (aref array 1))
		  (action-parameter nil)
		  (start 0)
		  (action-list nil))
		 ((eq action-parameter 'eof)
		  (if (listp (car action-list))
		      (setq solution (append action-list solution))
		      (push (nreverse action-list) solution)))
	      (multiple-value-setq (action-parameter start)
		(read-from-string action nil 'eof :start start))
	      (if (and action-parameter (not (eq action-parameter 'eof)))
		  (push action-parameter action-list)))))))
    (nreverse solution)))



