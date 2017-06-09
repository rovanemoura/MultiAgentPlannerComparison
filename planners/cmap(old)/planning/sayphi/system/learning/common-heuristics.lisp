(in-package "COMMON-LISP-USER")

(defvar *domain-name* "")
(defvar *domain-dependent-heuristic* nil)
(defvar *trace-heuristic* nil)

(defmacro type-of-object (object type)
  `(member ,object (gethash ,type (problem-objects *current-problem*))))

(defun say-domain (domain-dir &optional (domain-file "domain.pddl"))
  (setf *domain-name* domain-dir)
  (setf *domain-dependent-heuristic* (intern (format nil "~@:(h-~a~)" *domain-name*)))
  (setf *domain-dir* (concatenate 'string *domains-dir* domain-dir "/"))
  (setf *domain-file* (concatenate 'string *domain-dir* domain-file))
  (read-pddl-domain *domain-file*)
  (if (> *say-output* 0)
      (format t "~% ~a domain loaded!" domain-dir))
  t)

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

(defun check-truth (predicate object state arg-position)
  (nth arg-position (find-if #'(lambda (literal)
				 (and (eq (car literal) predicate)
				      (eq object (cadr literal))))
			     state)))

(defun give-me-nice-sayphi-goals (node)
  (if (snode-p node)
      (pp-state (target-goals node) 'list)))

(defun give-me-nice-sayphi-state (node)
  (if (snode-p node)
      (pp-state (snode-state node) 'list)))
