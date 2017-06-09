(defun plan-ebl-sayphi (&key (algorithm 'ehc-tilde)
			(timeout 30) (p 0.4)
			(deterministicp nil) (use-rules-p t)
			(knowledge-file nil))
  
;;   (if (not decision-trees-dir) (setq decision-trees-dir (concatenate 'string *domain-dir* "rules/")))
  (setf *print-reuse* nil)
  (if (not knowledge-file) (setq knowledge-file (concatenate 'string *domain-dir* "rules/tilde-decision-tree.lisp")))
  (let ((decision-trees nil)
	(i 0)
	(this-sol nil)
	(init-time (get-internal-run-time)))
    (with-open-file (istream knowledge-file :direction :input)
      (setq decision-trees (read istream)))
    (setf *actions-tilde-tree* (parse-btree-for-rules (from-tilde-list-to-btree (cdr (assoc 'actions decision-trees))) 'actions))
    (setf *bindings-tilde-trees* nil)
    (dolist (action (get-all-operators))
      (push (cons action (parse-btree-for-rules (from-tilde-list-to-btree (cdr (assoc action decision-trees))) action))
	    *bindings-tilde-trees*))
;;     (plan :algorithm algorithm :timeout timeout :search-options (list (list :p p) (list :deterministicp deterministicp) (list :use-rules-p use-rules-p)))
    (loop do (setq this-sol (plan :algorithm algorithm :timeout timeout :search-options (list (list :p p) (list :deterministicp deterministicp) (list :use-rules-p use-rules-p))))
       until (or (> (elapsed-time init-time) timeout) (and this-sol (or (eq (solution-stop-reason this-sol) :goals-reached) (eq (solution-stop-reason this-sol) :time-bound)))))
    this-sol))



;;For calling Sayphi-rules with relative path from the sayphi-core
(defun sayphi-rules-from-exe ()
  (let ((solution nil)
	(knowledge-file (nth 3 *posix-argv*))
	)
    (sayload-for-exe)
    (cond ((read-from-string knowledge-file) 
	   (setf solution (plan-ebl-sayphi :timeout (read-from-string (nth 5 *posix-argv*))
					   :p (read-from-string (nth 4 *posix-argv*))
					   :knowledge-file knowledge-file
					   )))
	  (t
	   (setf solution (plan :timeout (read-from-string (nth 5 *posix-argv*))))))
	  
    (when (solution-p solution)
      (format t "~%~%~a~%" solution)
      (when (and (solution-found solution) (not (read-from-string (nth 6 *posix-argv*))))   
	(write-solution-for-exe solution)))
   ))


	
;;Function para cargar dominio y fichero con ruta absoluta
(defun sayphi-rules-training-from-exe ()
  (reload-vars-for-execore)
  (let ((domain-dir (nth 1 *posix-argv*))
	(domain-file (nth 2 *posix-argv*))
	(train-probprefix (nth 3 *posix-argv*))
	(train-probsufix (nth 4 *posix-argv*))
	(train-probsdir (nth 5 *posix-argv*))
	(timeout (read-from-string (nth 6 *posix-argv*)))
	(extra-probs (read-from-string (nth 7 *posix-argv*))))
    
    (ebl-sayphi-train :domain domain-dir :domain-file domain-file
		      :probs-prefix train-probprefix
      		      :probs-sufix train-probsufix
		      :problems-dir train-probsdir      
		      :timeout timeout
		      :generate-extra-problems-p extra-probs
		      )
    

    (format t "~%~%[SAYPHI-RULES] Message... Training Complete!")
    (format t "~%[SAYPHI-RULES] Message... Knowledge File in ~arules/tilde-decision-tree.lisp ~%" *domain-dir*)
    (quit)
    ))
