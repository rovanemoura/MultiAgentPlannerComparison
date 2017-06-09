(in-package "COMMON-LISP-USER")

(defvar *trace-heuristic* nil)



(defun give-me-nice-sayphi-goals (node)
  (if (snode-p node)
      (pp-state (target-goals node) 'list)))

(defun give-me-nice-sayphi-state (node)
  (if (snode-p node)
      (pp-state (snode-state node) 'list)))



(defun h-blocksworld (node &optional (pgoals (problem-goals *current-problem*)))
  (let* ((state (give-me-nice-sayphi-state node))
	 (goals (pp-state pgoals 'list))
	 (h (blocksworld-h-literals state goals)))
    (when *trace-heuristic*
      (format t "~%Heuristic: ~a" h)
      (format t "~%FF heuristic: ~a" (h-relaxedplan node pgoals)))
    ;;    (setq h (read))
    (values h nil)))
;; 	(on (incf h (if (member (cadr literal) goals
;; 				:test #'(lambda (block goal)
;; 					  (and (eq (car goal) 'on)
;; 					       (equal (cdr goal) (cdr literal)))))
;; 			0
;; 			2)))

(defun blocksworld-h-literals (state goals)
  (let ((h 0)
	(cache (make-hash-table)))
    (when *trace-heuristic*
      (format t "~%State:")
      (pp-list state 1 t t t)
      (format t "~%Goals:")
      (pp-list goals 1 t t t))
    (dolist (literal state)
;;       (pp-hash-table cache)
      (case (car literal)
	(holding (incf h))
	((clear arm-empty))
	((ontable on-table)
	 (incf h (if (on (cadr literal) goals)
		     2
		     0)))
	(on (let* ((block (cadr literal))
		   (under-goal (on block goals)))
	      (incf h (if under-goal
			  (if (cache-good-tower under-goal state goals cache)
			      (if (eq under-goal (caddr literal))
				  0
				  2)
			      (if (above block under-goal state)
				  4
				  ;; it might be bigger when block is above a block that has to be under under-goal
				  2))
			  (if (cache-good-tower (caddr literal) state goals cache)
			      0
			      2)))))
	(otherwise nil)))
    h))
;; 			  (if (eq under-goal under-state)
;; 			      (if (good-tower under-state state goals)
;; 				  0
;; 				  4)
;; 			      (if (above block under-goal state)
;; 				  4
;; 				  2))

(defun cache-good-tower (block state goals cache)
  (let ((cached (gethash block cache)))
    (if (eq cached 'fail)
	nil
	(or cached
	    (cond ((good-tower block state goals cache)
		   (setf (gethash block cache) t))
		  (t (setf (gethash block cache) 'fail)
		     nil))))))

(defun good-tower (block state goals cache)
  (let ((under-state (on block state)))
    (if under-state
	(let ((under-goal (on block goals)))
	  (and (or (eq under-state under-goal)
		   (not under-goal))
	       (cache-good-tower under-state state goals cache)))
	(not (on block goals)))))

(defun above (block1 block2 literals)
  (if (on-table block1 literals)
      nil
      (let ((under (on block1 literals)))
	(or (eq under block2)
	    (above block1 under literals)))))
  
(defun on (block state)
  (caddar (member block state
		  :test #'(lambda (bl literal)
			    (and (eq (car literal) 'on)
				 (eq bl (cadr literal)))))))

(defun on-table (block literals)
  (member block literals
	  :test #'(lambda (bl literal)
		    (and (member (car literal) (list 'ontable 'on-table))
			 (eq bl (cadr literal))))))

(defun mp (prob)
  (prob prob)
  (format t "~%EHC+Heuristica~%")
  (solution-print (plan :heuristic 'h-blocksworld :algorithm 'enforced-hill-climbing) *standard-output* 1)
  (format t "~%A*+Heuristica~%")
  (solution-print (plan :heuristic 'h-blocksworld :algorithm 'a-star) *standard-output* 1)
  (format t "~%EHC+Relaxed plan~%")
  (solution-print (plan :heuristic 'h-relaxedplan) *standard-output* 1))


