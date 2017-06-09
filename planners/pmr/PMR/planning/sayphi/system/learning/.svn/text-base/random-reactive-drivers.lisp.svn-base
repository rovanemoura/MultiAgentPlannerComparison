;;; It is not exactly what we are trying to do, given that it should also perform actions related to movement (which do not decrease a drive)

(require 'sayphi *sayphi-loader*)

(defvar *max-drive* 3 "Maximum value of a drive to be considered normal")
;; These names have to do with negated functions in Sayphi
(defvar *drives* (list '_hunger '_thirst '_boredom '_tiredness '_dirtiness) "List of drives")
(defvar *drives-actions* '((_hunger eat) (_thirst drink) (_boredom play) (_sleepiness sleep) (_dirtiness bath)))
;; (defvar *drives* (list 'hunger 'thirst 'boredom 'tiredness 'dirtiness) "List of drives")
;; (defvar *drives-actions* '((hunger eat) (thirst drink) (boredom play) (sleepiness sleep) (dirtiness bath)))
(defvar *last-node* nil "So that we can inspect it at the end")

;; planner: random-planner, reactive-planner, sayphi
;; Use as: (run-sims-planner 'random-planner "n-sims-1-b.pddl") or (run-sims-planner 'reactive-planner "n-sims-1-b.pddl")
(defun run-sims-planner (planner problem &key (domain "sims") (domain-file "domain.pddl") (drives-actions *drives-actions*) (drives *drives*) (iterations 100))
  (say-domain domain domain-file)
  (prob problem)
  (let ((solution (plan :algorithm 'sims-planner :depthbound iterations :helpful nil
			:search-options (list (list :planner planner) (list :drives-actions drives-actions) (list :drives drives)))))
    (pp-solution-sayphi)
    (format t "~%Final values of drives:~%~a" (mapcan #'(lambda (literal)
							  (and (eq (car literal) '=)
							       (member (caadr literal) (cons 'met-val drives))
							       (list (caadr literal) (caddr literal))))
						      (pp-state (snode-state *last-node*) 'list)))
    solution))

;; from enforced-hill-climbing
(defun sims-planner (init-node h-fn cost-fn search-options &optional (problem *current-problem*))
  (declare (ignore cost-fn))
  (let ((open-nodes nil) (open-children nil)
	(node init-node) (next-node nil) (visited (make-hash-table))
	(current-h 0) (current-h-plus 0) (better-h-node nil) (discrete-h (not (is-metric-domain)))
	(lookahead (search-option-value :lookahead search-options))
	(helpful (search-option-value :helpful search-options))
	(planner (search-option-value :planner search-options))
	(drives-actions (search-option-value :drives-actions search-options))
	(drives (search-option-value :drives search-options)))

    (cond (*trace-mem* 
	   (say-consed-bytes :mem-per-node (store-h-extras node h-fn))
	   (write-node-mem-info 'ehc))
	  (t (store-h-extras node h-fn)))

    (setf current-h (snode-h-value node))
    (setf current-h-plus (snode-h-plus node))
    (do* ((stop-this (>= (snode-depth node) (get-sayp :say-depthbound)) (>= (snode-depth node) (get-sayp :say-depthbound))))
	 (stop-this (setf *last-node* node)
		    (build-solution node problem nil :depth-bound))
      
      (trace-search-extras (snode-number node))
      (expand-state node :helpful helpful)
      (when lookahead (expand-node-lookahead node lookahead visited))

      (print-search-node node nil)

      ;; just in case I need it for some kind of obscure reason
;;      (setf better-h-node (get-enforced-heuristic node h-fn current-h visited :discrete discrete-h :current-h-plus current-h-plus))
      (setq better-h-node (case planner
			    (random-planner (action-random-selection (snode-children node)))
			    (reactive-planner (action-max-drive-selection node (snode-children node) drives-actions drives))
			    (sayphi (get-enforced-heuristic node h-fn current-h visited :discrete discrete-h :current-h-plus current-h-plus))
			    (otherwise nil)))
      (cond ((snode-p better-h-node)
	     (setf current-h (snode-h-value better-h-node))
	     (setf current-h-plus (snode-h-plus better-h-node))
	     (setf node better-h-node)
	     (setf open-nodes nil)
	     (setf open-children nil)
 	     (setf visited (reset-hash-visited node))
	     )
	    (t 
	     (setf open-children (nconc (remove-if #'snode-closed (snode-children node)) 
					open-children))
	     
	     (cond ((not (snode-p (setf next-node (pop open-nodes))))
		    (when (> *say-output* 1) (format t "  ~% [Expanding Breadth Level]"))
		    (setf open-nodes (ehc-open-nodes open-children (car (find-argument search-options :children-sort))))
		    
		    (when (null open-nodes) ;;Recuperando la poda por las helpful actions
		      (dolist (i-child (restore-nonhelpful node h-fn))
			(cond ((find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state)
			       (setf (snode-closed i-child) t))
			      (t 
			       (push i-child (gethash (snode-hash-code i-child) visited))
			       (push i-child open-nodes))))
			       
		      ;; 		      (setf open-nodes (restore-nonhelpful node h-fn))
		      
		      (when (> *say-output* 1) 
			(format t "  ~%!!Restoring NON Helpful Nodes.")))
		    (setf node (pop open-nodes))
		    (setf open-children nil))
		   (t 
		    (setf node next-node)))))       
      ;; 	 (when (snode-p node)
      ;; 	   (print-search-node node nil)
      )))

(defun action-random-selection (nodes)
  (let ((node (choose-one nodes)))
    (format t "~%Action ~a chosen randomly" (gaction-planaction (snode-applied-action node)))
    node))

;;   (let ((action (choose-one (cdr (choose-one drives-actions)))))
;;     (choose-one (or (remove-if-not #'(lambda (node) (eq action (gaction-planaction (snode-applied-action node))))
;; 				   nodes)
;; 		    nodes)))

(defun action-max-drive-selection (node nodes drives-actions drives)
  (let* ((max-drive (find-max-drive (pp-state (snode-state node) 'list) drives))
	 (action (choose-one (cdr (assoc max-drive drives-actions))))
	 (children (remove-if-not #'(lambda (node) (eq action (gaction-planaction (snode-applied-action node))))
				  nodes))
	 (best (choose-one (or children nodes))))
    (if children
	(format t "~%Action ~a chosen from max drive ~a" (gaction-planaction (snode-applied-action best)) max-drive)
	(format t "~%Action ~a chosen randomly" (gaction-planaction (snode-applied-action best))))
    best))

(defun find-max-drive (state drives)
  (let ((max-drive 0)
	(drive (car *drives*)))
    (dolist (literal state)
      (when (and (eq (car literal) '=)
		 (member (caadr literal) drives)
		 (> (caddr literal) max-drive))
	(setq max-drive (caddr literal))
	(setq drive (caadr literal))))
    drive))
;;  :test #'(lambda (drive1 drive2) (eq (intern (format nil "_~a" drive1)) drive2))

#|
;;; *********************************************************************
;;;         Planning without planner
;;; *********************************************************************

;; (defun run-sims-planner (planner &key (domain "sims") (domain-file "domain.pddl") (drives-actions *drives-actions*) (drives *drives*)
;; 			 (iterations 100) (initial-state (generate-random-initial-state drives)))
;;   (say-domain domain domain-file)
;;   (case planner
;;     (random (random-planner actions drives-actions drives iterations initial-state))
;;     (reactive (reactive-planner actions drives-actions drives iterations initial-state))
;;     (otherwise nil)))

;;; *********************************************************************
;;;         Random planner
;;; *********************************************************************

;; drives-actions: ((drive action+)+)
(defun random-planner (actions drives-actions drives iterations initial-state)
  (let ((state initial-state)
	(i 0)
	(finishp nil))
    (loop (setq state (execute-action (choose-one (cdr (choose-one drives-actions))) state))
       (incf i)
       (setq finishp (finishp state))
       until (or finishp (>= i iterations)))
    finishp))

(defun generate-random-initial-state (drives)
  (mapcar #'(lambda (drive) (list drive (random 10))) drives))

(defun execute-action (action state)
  (format t "~%Executing action: ~a~% in state: ~a" action state)
  (let ((action-name (car action))
	(action-struct (find action-name (dom-actions *pspace*) :key #'action-name))
	(substitution (mapcar #'cons (action-parameters action-struct) (cdr action)))
	(preconds (sublis substitution (action-preconditions action-struct) :test #'equal))
	(adds (sublis substitution (action-adds action-struct) :test #'equal))
	(dels (sublis substitution (action-dels action-struct) :test #'equal)))
    (dolist (del dels)
      (setq state (remove del state :test #'equal)))
    (dolist (add adds)
      (pushnew add state :test #'equal))
    state))

(defun finishp (state)
  (notany #'(lambda (literal) (> (cdr literal) *max-drive*)) state))

;;; *********************************************************************
;;;        Reactive planner
;;; *********************************************************************

(defun reactive-planner (actions drives-actions &key (drives *drives*) (iterations 100) (initial-state (generate-random-initial-state drives)))
  (let ((state initial-state)
	(i 0)
	(finishp nil)
	(max-drive nil))
    (loop (setq max-drive (find-max-drive state))
       (setq state (execute-action (choose-one (cdr (assoc max-drive drives-actions))) state))
       (incf i)
       (setq finishp (finishp state))
       until (or finishp (>= i iterations)))
    finishp))

;; (defun find-max-drive (state)
;;   (let ((max-drive 0)
;; 	(drive (car *drives*)))
;;     (dolist (literal state)
;;       (when (> (cdr literal) max-drive)
;; 	(setq max-drive (cdr literal))
;; 	(setq drive (car literal))))
;;     drive))
|#