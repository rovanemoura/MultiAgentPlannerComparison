;; Code to handle constraints of PDDL3.0. We start with the always

(in-package "COMMON-LISP-USER")

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
	 (goals (translate-goals (car goals-def) inheritobjects predicates patterns))
	 (metric (find-argument problem-def :metric))
	 (constraints (car (find-argument problem-def :constraints))))
    (pattern-functor-update patterns init-state)
    (setf *current-problem* (make-say-problem :name problem
					  :domain prob-domain
					  :objects objects
					  :inheritobjects inheritobjects
					  :patterns patterns
					  :init-state init-state
					  :goals goals
					  :metric metric
					  :lit-init-state init-def
					  :lit-goals (if (eq 'and (caar goals-def))
							 (cdar goals-def)
						       goals-def)
					  :constant-poslist (build-constant-poslist inheritobjects)
					  ))
    ;;for new representation
    (setf (getf (problem-plist *current-problem*) :constraints) constraints)
    (parse-initial-state init-def)
    (parse-initial-goals (problem-lit-goals *current-problem*))
    ))

(defun expand-state (node &key (helpful t) (restoring-byte nil))
  (let* ((state (snode-state node)) (children nil)
	 (applicable-byte (if (null restoring-byte) (find-applicable-byte node helpful) restoring-byte))
	 (new-child nil) (child-node nil))
    (dotimes (ibit (length applicable-byte))
      (when (= 1 (sbit applicable-byte ibit ))
	(let ((app-action (aref *actions-table* ibit))
	      (constraints (getf (problem-plist *current-problem*) :constraints)))
	  ;; Not for now. We can add this to plan to be more efficient
	  ;; 	  (if (and constraints (not (getf (problem-plist *current-problem*) :number-constraints-true)))
	  ;; 	      (setf (getf (problem-plist *current-problem*) :number-constraints-true) 0))
	  (setf new-child (apply-action-instance state app-action))
	  (when (or (not constraints) (fulfills-constraints new-child constraints))
	    (setf (snode-expanded node) t)
	    (setf child-node (make-snode 
			      :depth (+ 1 (snode-depth node))
			      :length (+ 1 (snode-length node))
			      :parent node
			      :state new-child
			      :applied-action app-action
			      :helpful-p helpful
			      ))
	    (setf (snode-cost child-node) 
		  (action-cost-metric state new-child))
	    (setf (snode-number child-node) 
		  (incf (getf (problem-plist *current-problem*) :node-counter)))
	    (setf (gethash (snode-number child-node) *hash-nodes*) child-node)
	    (setf (snode-hash-code child-node) (compute-duphash-code new-child))
	    (push child-node children)))))
    (if (null restoring-byte)
	(setf (snode-children node) (reverse children))
	(setf (snode-children node) (nconc (reverse children) (snode-children node))))))

;; The rest of constraints to be implemented
(defun fulfills-constraints (state constraints)
  (let ((checked (check-constraint state (cadr constraints))))
    (case (car constraints)
      (always checked)
      (sometime (if checked (setf (getf (problem-plist *current-problem*) :constraints) nil))
		checked)
;;       (within (if checked (incf (getf (problem-plist *current-problem*) :number-constraints-true)))
;; 	      checked)
      (otherwise checked))))

;; Very inefficient. We will improve it by compiling into the ibits
;; This only accounts for predicates constraints. I will work on the more complex goals later.
;; I do the AND to not return the list (efficiency)
(defun check-constraint (state constraints)
  (and (case (car constraints)
	 (and (every #'(lambda (constraint)
			 (check-individual-constraint state constraint))
		     (cdr constraints)))
	 (or (some #'(lambda (constraint)
		       (check-individual-constraint state constraint))
		   (cdr constraints)))
	 (t (check-individual-constraint state constraints)))
       t))

(defun check-individual-constraint (state constraint)
  (member constraint (pp-state state 'list) :test #'equal))

