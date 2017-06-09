;; converts problem files from Valencia or RazNissim to standard PDDL

(defvar *domains-dir* (concatenate 'string *my-planning-path* "sayphi/domains/")
  "Just in case it has not been defined elsewhere")

(defun translate-ma-probset (directory)
  (dolist (dir (directory directory))
    (translate-ma-problems (concatenate 'string (namestring dir) "*.*"))))

;; (translate-ma-problems "/Users/dborrajo/Desktop/ECAIDomains/logistics/NissimPfile0/*.*")
(defun translate-ma-problems (directory &optional (problem-name (car (last (pathname-directory directory)))))
  (let (problem-def new-state new-goals objects metric domain name)
    (dolist (problem (directory directory))
      (setq problem-def (cdr (read-all-file problem)))
      (when (eq (caar problem-def) 'problem)
	(setq name (or name (car (find-argument problem-def 'problem))))
	(setq domain (or domain (car (find-argument problem-def :domain))))
	(setq objects (or objects (find-argument problem-def :objects)))
	(setq metric (or metric (find-argument problem-def :metric)))
	(setq new-state (translate-ma-state (find-argument problem-def :init) new-state))
	(setq new-goals (translate-ma-state (cdar (find-argument problem-def :global-goal)) new-goals))))
    (write-pddl-file name domain objects new-state new-goals ;; "/Users/dborrajo/tmp/pddl-problem.pddl"
		     (format nil "/Users/dborrajo/tmp/~a.pddl" problem-name)
;; 		     (format nil "~a~a/probsets/~a.pddl" *domains-dir* domain problem-name)
		     metric)))

(defun translate-ma-state (state new-state)
  (dolist (literal state)
    (case (car literal)
      (= (pushnew (append (cadr literal) (cddr literal)) new-state :test #'equal))
      ((myAgent not) nil)
      (otherwise (pushnew literal new-state :test #'equal))))
  new-state)