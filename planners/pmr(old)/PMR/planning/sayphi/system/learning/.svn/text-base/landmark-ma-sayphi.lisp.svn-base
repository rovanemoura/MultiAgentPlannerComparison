;;; This is the version that computes landmarks as additional subgoals in order to solve problems in domains as logistics where each agent cannot by itself
;;; solve almost any goal
(in-package "COMMON-LISP-USER")

(defvar *compute-landmarks-p* nil)
(defvar *landmarks-file* (concatenate 'string  *my-planning-path* "fd-landmarks/landmarks.txt"))
(defvar *landmarks* nil)
(defvar *trace-ma-sayphi* nil)
(defvar *union-preconds* nil)
(defvar *union-effects* nil)
(defvar *plan-constants* nil)
(defvar *internal-instances* nil)
(defvar *original-actions* nil)
(defvar *max-planning-iterations* 3 "If an stochastic replanner does not solve a problem, I can run it again up to this number of iterations")
(defvar *anon-index* 0)
(defvar *anonymize-alist* nil)

(setf *say-output* 0)

;; algorithms: lama, metric-ff, a-star, enforced-hill-climbing, ...
;; replanning-algorithm can be: errtplan, lpg-adapt, or any of the algorithms
;; goal-selection can be:
;;   all-achievable: each agent will receive as goals all goals that can potentially be achieved by it
;;   rest-achievable: each agent will receive as goals all goals that can potentially be achieved by it after removing the ones already assigned to a previous agent
;;   load-balance: goals are assigned so that they are as balanced as possible
;;   best-cost: assigns each goal to the agent that potentially can achieve it with less cost: h(estado-inicial)
;; internal-types is a list of types that are internal to agents
;; if run-original-problem-p=T, it also runs the centralized approach to compare
;; if add-goals-p=T, it will add the goals of one agent to the goals of the next one. Otherwise, we would have to pass the end state of the previous agent
;; iterations: for stochastic algorithms, how many times to run each configuration

;; (run-ma-experiments "satellite" "pfile*.pddl" '(satellite) '(supports calibration_target on_board pointing power_avail calibrated power_on) '(instrument mode) :domain-file "satellite.pddl" :goal-selections  '(all-achievable rest-achievable best-cost load-balance) :run-original-problem-p t :algorithms '(lama) :replanning-algorithms '(lama) :timeout 600)
;; (run-ma-experiments "rovers" "pfile*" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '(camera store) :domain-file "StripsRover.pddl" :goal-selections '(all-achievable rest-achievable best-cost load-balance) :run-original-problem-p t :algorithms '(lama) :replanning-algorithms '(lama lpg-adapt) :timeout 600)
;; (run-ma-experiments "rovers" "p3*.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '(camera store) :domain-file "StripsRover.pddl" :goal-selections '(all-achievable rest-achievable best-cost load-balance) :run-original-problem-p t :algorithms '(lama) :replanning-algorithms '(lama lpg-adapt) :timeout 600)
;; (run-ma-experiments "rovers" "Nissim*.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '(camera store) :domain-file "StripsRover.pddl" :goal-selections '(best-cost load-balance) :run-original-problem-p t :algorithms '(lama metric-ff) :replanning-algorithms '(lama lpg-adapt) :timeout 100)
;; (run-ma-experiments "port" "pfile*.pddl" '(hoist) '(lifting assigned available on-ship) '(pallet-ship) :domain-file "domain.pddl" :goal-selections '(rest-achievable all-achievable best-cost load-balance) :run-original-problem-p t :algorithms '(lama) :replanning-algorithms '(lama lpg-adapt) :timeout 900)
(defun run-ma-experiments (domain problems-regexp agent-types internal-predicates internal-types
				  &key (probsets-dir "probsets/") (domain-file "domain.pddl") (output-file-prefix "ma-result-")
				  (goal-selections '(best-cost)) (algorithms '(enforced-hill-climbing)) (replanning-algorithms '(errtplan))
				  (timeout 300) (run-original-problem-p t) (add-goals-p t) (iterations 1) (only-one-iteration-p nil) (max-iterations 5))
  (let ((output-file "")
	(problems (mapcar #'(lambda (pathname)
			      (if (pathname-type pathname)
				  (concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
				  (pathname-name pathname)))
			  (directory (format nil "~a~a/~a~a" *domains-dir* domain probsets-dir problems-regexp)))))
    (dolist (goal-selection goal-selections)
      (dolist (algorithm algorithms)
	(dolist (replanning-algorithm replanning-algorithms)
	  (dotimes (i iterations)
	    (with-open-file (ostream (setq output-file (format nil "~a~a/result/~a~a-~(~a~)-~(~a~)-~(~a~)-~d.lisp"
							       *domains-dir* domain output-file-prefix (pathname-name (remove #\* problems-regexp))
							       algorithm replanning-algorithm goal-selection iterations))
				     :direction :output :if-exists :supersede :if-does-not-exist :create)
	      (format ostream ";; approach problem solvedp #solved #problems/agents time nodes length~2%("))
	    (dolist (problem problems)
	      (format t "~%Running ~(~a~) ~(~a~) ~(~a~) ~d" algorithm replanning-algorithm goal-selection iterations)
	      (run-ma domain problem agent-types internal-predicates internal-types :probsets-dir probsets-dir :domain-file domain-file
		      :goal-selection goal-selection :timeout timeout
		      :algorithm algorithm :replanning-algorithm replanning-algorithm
		      :run-original-problem-p run-original-problem-p :add-goals-p add-goals-p :output-file output-file
		      :only-one-iteration-p only-one-iteration-p :max-iterations max-iterations))
	    (with-open-file (ostream output-file :direction :output :if-exists :append :if-does-not-exist :create)
	      (format ostream "~%)"))))))))

;; This does not work because of being the logistics domain:
;; (run-ma "logistics" "NissimPfile0.pddl" '(truck airplane) '(at) '() :domain-file "ma-logistics.pddl" :goal-selection 'best-cost :run-original-problem-p t :algorithm 'lama :replanning-algorithm 'lama :timeout 10 :run-original-problem-p t)
;; (run-ma "port" "pfile.pddl" '(hoist) '(lifting assigned available on-ship) '(pallet-ship) :domain-file "domain.pddl" :goal-selection 'all-achievable :run-original-problem-p t :algorithm 'lama :timeout 200)
;; (run-ma "floortile" "seq-p01-002.pddl" '(robot) '(robot-at robot-has free-color) '() :domain-file "domain.pddl" :goal-selection 'load-balance :run-original-problem-p t :algorithm 'lama :replanning-algorithm 'lpg-adapt :timeout 200)
;; (run-ma "zenotravel" "pfile02-ma" '(aircraft) '(at-airplane fuel-level) '() :domain-file "ma-zenotravel.pddl" :goal-selection 'load-balance :run-original-problem-p t :algorithm 'lama :replanning-algorithm 'lpg-adapt :timeout 100)
;; (run-ma "satellite" "pfile03.pddl" '(satellite) '(supports calibration_target on_board pointing power_avail calibrated power_on) '(instrument mode) :domain-file "satellite.pddl" :goal-selection 'load-balance :run-original-problem-p t)
;; problem p03.pddl:
;; (run-ma "ma-rovers" "p03.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '(camera store) :domain-file "original-domain.pddl" :goal-selection 'best-cost)
;; problem p30.pddl:
;; (run-ma "ma-rovers" "p30.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '(camera store) :domain-file "original-domain.pddl" :goal-selection 'load-balance :run-original-problem-p t :algorithm 'enforced-hill-climbing :timeout 100)
;; (run-ma "ma-rovers" "p07.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '(camera store) :domain-file "original-domain.pddl" :goal-selection 'load-balance :run-original-problem-p t :algorithm 'enforced-hill-climbing :timeout 100)
;; (run-ma "ssa-dcii" "prob-110-1000-17280-5.pddl" '(sensor) '(visible available quality-observation) nil :domain-file "domain6.pddl" :goal-selection 'best-cost :run-original-problem-p nil :timeout 100 :add-goals-p nil)
;; output-file: nil (no output in file) or a absolute pathname (where to save results)
;; only-one-iteration-p: if T, it will only iterate once over all agents
(defun run-ma (domain problem agent-types internal-predicates internal-types &key (probsets-dir "probsets/") (domain-file "original-domain.pddl") (goal-selection 'all-achievable)
	       (timeout 300) (algorithm 'enforced-hill-climbing) (replanning-algorithm 'errtplan) (run-original-problem-p t) (add-goals-p t)
	       (only-one-iteration-p nil) (max-iterations 5)
	       (output-file (format nil "~a~a/result/ma-result-~(~a~)-~(~a~)-~(~a~).lisp" *domains-dir* domain algorithm replanning-algorithm goal-selection)))
  (setf *internal-instances* nil)
  (setf *domain-dir* (concatenate 'string *domains-dir* domain "/"))
  (setf *anon-index* 0)
  (if (and output-file (not (probe-file output-file)))
      (with-open-file (ostream (format nil "~a~a/result/ma-sayphi-~(~a~)-~(~a~)-~(~a~).lisp" *domains-dir* domain algorithm replanning-algorithm goal-selection)
			       :direction :output :if-exists :supersede :if-does-not-exist :create)))
  (let* ((init-time (get-internal-real-time))
	 (problems (write-ma-problem domain problem agent-types internal-predicates internal-types :probsets-dir probsets-dir :domain-file domain-file :goal-selection goal-selection))
	 ;; time-per-agent
	 (solved 0) (total-time 0) (time 0) (nodes 0) (length 0) (complete-plan nil) (num-problems (length problems))
	 (agents (mapcar #'car problems))
	 (augmented-solution nil) (solution nil) (centralized-solution nil))
    (setf *anonymize-alist* (mapcar #'(lambda (agent) (list agent nil nil)) agents))
    (cond ((> (- timeout (elapsed-time init-time 'real-time)) 0)
	   (format t "~%Running Multi-agent~%")
	   ;; if I did not find a solution, and I have still time, I repeat from the first agent
	   (do* ((last-problem-p nil (null (cdr the-problems)))
		 (iteration 0 (if last-problem-p (1+ iteration) iteration))
		 (first-iteration-p t (if first-iteration-p (not last-problem-p) nil))
		 (the-problems problems (if last-problem-p problems (cdr the-problems)))
		 (new-problem (car the-problems) (car the-problems))
		 (remaining-time (- timeout (elapsed-time init-time 'real-time)) (- timeout (elapsed-time init-time 'real-time)))
		 (last-correct-augmented-solution (list nil 0 0 0 nil nil nil nil nil nil))
		 (timeoutp (<= remaining-time 0.0) (<= remaining-time 0.0)))
	       ((or timeoutp
		    (>= iteration max-iterations)
		    (and (or last-problem-p (and add-goals-p (not first-iteration-p))) solution)
		    (and last-problem-p only-one-iteration-p)))
	     (if last-problem-p (format t "~%Restarting from the first agent"))
	     ;; each cycle is the computation of a new agent. They only know about previous anonymized plans, predicates and constants
	     (setq agent (car new-problem))
	     ;; I call every agent until it solves the problem or we run out of time (given stochastic behavior)
	     ;;	     (setq time-per-agent (/ remaining-time (length problems)))
	     (format t "~2%Running agent ~a with time ~,2f and remaining total time ~,2f~%" agent remaining-time remaining-time)
	     (setq augmented-solution (call-agent agent domain domain-file (cdr new-problem) augmented-solution internal-predicates (cdr (assoc agent *internal-instances*))
						  algorithm remaining-time replanning-algorithm probsets-dir add-goals-p))
	     (setq solution (nth 0 augmented-solution))
	     (cond (solution
		    (setq last-correct-augmented-solution augmented-solution)
		    (if *trace-ma-sayphi* (format t "~%Partial solution of agent ~a: ~%~a" agent solution))
		    (setq complete-plan (if add-goals-p
					    (list (list (car new-problem) solution))
					    (append complete-plan (list (list (car new-problem) solution)))))
		    (incf solved)
		    (incf time (nth 1 augmented-solution))
		    (incf nodes (nth 2 augmented-solution))
		    (if add-goals-p
			(setf length (nth 3 augmented-solution))
			(incf length (nth 3 augmented-solution)))
		    (format t "~%Problem: ~a, Solved: T, Time: ~,2f, Nodes: ~d, Length: ~d" (cdr new-problem) time nodes length))
		   ;; 			new-predicates :key #'car)))
		   (t (setq augmented-solution last-correct-augmented-solution)
		      (format t "~%Problem: ~a, Solved: NIL" (cdr new-problem)))))
	   (setq total-time (elapsed-time init-time 'real-time))
	   (when run-original-problem-p
	     (format t "~%Running centralized~%")
	     (free-mem)
	     (unless (member algorithm '(lama lpg-adapt metric-ff))
	       (say-domain domain domain-file)
	       (prob problem))
	     (setq centralized-solution (the-ring algorithm timeout domain domain-file problem))
	     (if (and centralized-solution (solution-found centralized-solution) (> (solution-length centralized-solution) 0))
		 (if output-file
		     (with-open-file (ostream output-file :direction :output :if-exists :append :if-does-not-exist :create)
		       (format ostream "~%(centralized ~a T 1 1 ~,2f ~d ~d)" (pathname-name problem)
			       (solution-total-time centralized-solution) (solution-num-nodes centralized-solution) (solution-length centralized-solution)))
		     (format t "~%Summary centralized: Problem ~a, Solved: T, Time: ~,2f, Nodes: ~d, Length: ~d" problem
			     (solution-total-time centralized-solution) (solution-num-nodes centralized-solution) (solution-length centralized-solution)))
		 (if output-file
		     (with-open-file (ostream output-file :direction :output :if-exists :append :if-does-not-exist :create)
		       (format ostream "~%(centralized ~a nil 0 1 0.0 0 0)" (pathname-name problem)))
		     (format t "~%Problem: ~a, Solved: NIL" problem))))
	   (if output-file
	       (with-open-file (ostream output-file :direction :output :if-exists :append :if-does-not-exist :create)
		 (format ostream "~%(multi-agent ~a ~a ~d ~d ~,2f ~d ~d)" (pathname-name problem) (if solution t) solved num-problems total-time nodes length))
	       (format t "~%Summary multi-agent: Problem ~a, Solved: ~d/~d, Time: ~,2f, Nodes: ~d, Length: ~d, Total Time: ~,2f, #Agents: ~d"
		       problem solved num-problems time nodes length total-time num-problems))
	   ;; if last solution is not nil, it should be a valid plan, since we add goals from one problem to the next
	   (if (and solution (or add-goals-p (= solved num-problems)))
	       (if add-goals-p
		   (cadar complete-plan)
		   complete-plan)
	       'incomplete-plan))
	  (t (format t "~%Too much time on computing subproblems")
	     'no-time))))
;;     (if (= solved num-problems)
;; 	complete-plan
;; 	'no-complete-solution)))
;; (run-ma "ma-rovers" "p03.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '((rover0 camera0 rover0store) (rover1 camera1 rover1store)) :original-domain-file "original-domain.pddl" :goal-selection 'best-cost)
;; problem p30.pddl:
;; (run-ma "ma-rovers" "p30.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '((rover0 camera0 rover0store) (rover1 camera6 camera7 rover1store) (rover2 camera12 rover2store) (rover3 camera1 rover3store) (rover4 camera8 rover4store) (rover5 camera2 camera4 camera9 rover5store) (rover6 camera3 camera5 rover6store) (rover7 camera13 rover7store) (rover8 camera11 rover8store) (rover9 camera10 rover9store) ) :original-domain-file "original-domain.pddl" :goal-selection 'load-balance :run-original-problem-p t :algorithm 'enforced-hill-climbing :timeout 100)
;; (run-ma "ma-rovers" "p07.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board visible_from calibration_target) '((rover0 camera0 rover0store) (rover1 rover1store) (rover2 camera2 rover2store)) :original-domain-file "original-domain.pddl" :goal-selection 'load-balance :run-original-problem-p t :algorithm 'enforced-hill-climbing :timeout 100)

;; info-previous-agents is a list of the form (everything instantiated with the external information and the anonymized information of the previous agents
;; (solution-list time nodes length predicates functions constants new-actions state goals)
;; There are two alternatives: pass the previous internal/external goals or not. if not, then we would have to cycle through agents and make sure we do
;; not loop by giving again and again the same solutions and the other agents applying actions that remove those previous goals. if we pass the internal
;; goals, planning takes more time.
(defun call-agent (agent domain domain-file problem info-previous-agents internal-predicates internal-instances algorithm timeout replanning-algorithm
		   probsets-dir add-goals-p)
  (let* ((init-time (get-internal-real-time))
	 (actions (nth 7 info-previous-agents))
	 (anonymous-internal-predicates-alist (anonymize agent internal-predicates :predicates))
	 (anonymous-internal-instances-alist (anonymize agent internal-instances :objects))
	 (domain-def (cdr (read-all-file (concatenate 'string *domains-dir* domain "/" domain-file))))
	 (domain-name (car (find-argument domain-def 'domain)))
	 (predicates (union (nth 4 info-previous-agents) (find-argument domain-def :predicates) :test #'equal))
	 (functions (union (nth 5 info-previous-agents) (find-argument domain-def :functions) :test #'equal))
	 (previous-constants (constants-union (nth 6 info-previous-agents) (process-instances (find-argument domain-def :constants))))
	 (new-domain-file (format nil "~(~a~)-domain.pddl" agent))
	 (problem-def (cdr (read-all-file (concatenate 'string *domain-dir* probsets-dir problem))))
	 (new-problem (if (pathname-type problem)
			  (concatenate 'string (pathname-name problem) "-aux." (pathname-type problem))
			  (concatenate 'string (pathname-name problem) "-aux")))
	 (state (union (nth 8 info-previous-agents) (find-argument problem-def :init) :test #'equal))
	 (goals (if add-goals-p
		    (union (nth 9 info-previous-agents) (cdar (find-argument problem-def :goal)) :test #'equal)
		    (cdar (find-argument problem-def :goal))))
	 constants effects agent-solution plan solution result predicates-in-plan new-goals)
    (cond (info-previous-agents
	   (write-domain-pddl-file domain-name (find-argument domain-def :requirements)  (find-argument domain-def :types)
				   predicates functions
				   (append actions (find-all-argument domain-def :action) (find-all-argument domain-def :durative-action))
				   (concatenate 'string *domains-dir* domain "/" new-domain-file)
				   (flatten-instances previous-constants))
	   (free-mem)
	   (say-domain domain new-domain-file)
	   (write-pddl-file (pathname-name new-problem) domain-name (remove-previous-constants (find-argument problem-def :objects) previous-constants) state goals
			    (concatenate 'string *domain-dir* probsets-dir new-problem)
			    (find-argument problem-def :metric)))
	  (t (parse-domain domain-def)
	     (setq new-problem problem)))
    ;;    (parse-problem problem-def)
    (prob new-problem)
    ;; I repeat until I run out of time, or solution found
;;  (break)
    (do ((timeoutp nil)
	 (remaining-time 0)
	 (iterations 0 (1+ iterations))
	 (planner-to-use (if (nth 0 info-previous-agents) replanning-algorithm algorithm)
			 (if (nth 0 info-previous-agents) replanning-algorithm algorithm))
	 (triedp nil t))
	((or (and solution (solution-p solution) (solution-found solution))
	     (>= iterations *max-planning-iterations*)
	     timeoutp (and triedp (not (eq planner-to-use 'errtplan)) (not (eq planner-to-use 'lpg-adapt)))))
      (setq remaining-time (- timeout (elapsed-time init-time 'real-time)))
;;      (setq remaining-time (- timeout (elapsed-time init-time 'real-time) (if (or (eq planner-to-use 'lama) (eq planner-to-use 'lpg-adapt)) 'real-time 'run-time))))
      (format t "~%Remaining time for repeated execution: ~,2f" remaining-time)
      (cond ((>= remaining-time 0)
	     (setq solution (the-ring planner-to-use timeout
				      domain (if info-previous-agents new-domain-file domain-file) new-problem
				      (nth 0 info-previous-agents))))
	    (t (setq timeoutp t))))
    ;; HERE I SHOULD RECOMPUTE LANDMARKS (removing those that were achieved by the solution by progressing the solution over the initial state - see
    ;; validate-plan), AND THUS GENERAL GOALS OF THE REST OF AGENTS
    ;; I use it to remove predicates not in the preconds/effects and to know what are the predicates to add to the domain definition of the next agent
    (setf *union-preconds* nil)
    (setf *union-effects* nil)
    (setf *plan-constants* nil)
    (setq plan (if (listp solution)
		   solution
		   (if (and solution (solution-found solution))
		       (if (snode-p (car (solution-path solution)))
			   (pp-solution-sayphi solution)
			   (solution-path solution)))))
    (setq agent-solution (agent-solution plan anonymous-internal-predicates-alist anonymous-internal-instances-alist agent))
    (setq actions (remove-if-not #'(lambda (action)
				     (member (cadr action) agent-solution :key #'caar))
				 actions))
    (dolist (augmented-action agent-solution)
      (setq effects (caddr augmented-action))
      (dolist (del (cadddr augmented-action))
	(push (list 'not del) effects))
      (pushnew `(:action ,(caar augmented-action) :parameters () :precondition (and ,@(cadr augmented-action)) :effect (and ,@effects))
	       actions :test #'equal))
    (setq new-goals (sublis anonymous-internal-instances-alist (replace-predicate-names anonymous-internal-predicates-alist goals)))
    (setq predicates-in-plan (union *union-preconds* *union-effects* :test #'equal))
    (dolist (goal new-goals)
      (dolist (arg (cdr goal))
	(pushnew (list (get-object-type arg) arg)
		 *plan-constants* :key #'cadr)))
    ;;     (if *trace-ma-sayphi*
    ;; 	(format t "~%Predicates: ~a~%Constants: ~a~%Actions: ~a~%Anonymous predicates: ~a~%Union preconds: ~a~%Union effects: ~a" predicates *plan-constants* actions anonymous-internal-predicates-alist *union-preconds* *union-effects*))
    (if (and solution (solution-found solution))
	(list (sublis anonymous-internal-instances-alist (mapcar #'car agent-solution)) (solution-total-time solution) (solution-num-nodes solution) (solution-length solution)
	      (remove-irrelevant-defs (replace-predicate-names anonymous-internal-predicates-alist predicates) (union predicates-in-plan new-goals :test #'equal)
				      anonymous-internal-predicates-alist (nth 4 info-previous-agents))
	      (remove-irrelevant-defs (replace-predicate-names anonymous-internal-predicates-alist functions) predicates-in-plan
				      anonymous-internal-predicates-alist (nth 5 info-previous-agents))
	      (join-constants *plan-constants*) actions
	      ;;;; IMPORTANT: REMOVE PRECONDS THAT ARE ADDED BY SOME OTHER ACTION
	      (remove-irrelevant-state (intersection *union-preconds* (sublis anonymous-internal-instances-alist (replace-predicate-names anonymous-internal-predicates-alist state))
						     :test #'equal)
				       anonymous-internal-predicates-alist (nth 8 info-previous-agents)) ;;  (mapcar #'cadr constants)
	      new-goals)
	;; I HAVE TO SEE WHAT TO DO WHEN NO SOLUTION IS FOUND:
	;; - IF IT USED ANOTHER ROVER TO SOLVE ONE OF THE TASKS (OR IF IT DID IT IN THE WRONG ORDER, MOST PROBABLY), THEN SINCE IT CANNOT MOVE ANOTHER ROVER WHILE PLANNING FOR ANOTHER ONE, IT MIGHT FAIL DUE TO UNREACHABLE GOALS.
	(list nil 0 0 0 (nth 4 info-previous-agents) (nth 5 info-previous-agents) (nth 6 info-previous-agents) (nth 7 info-previous-agents) (nth 8 info-previous-agents) (if add-goals-p goals)))))
    ;; DEBERIA QUITAR TAMBIEN LOS PREDICADOS/FUNCIONES ANONIMOS QUE NO UTILIZA NINGUNA ACCION EN EL PLAN.
    ;; TAMBIEN DEBO QUITAR LAS ACCIONES QUE SE QUEDAN SOLO CON PREDICADOS ANONIMOS?
;;    (setq constants (collect-constants agent-solution))
;;     (setq constants (union previous-constants (collect-constants agent-solution) :test #'equal))
;; 	   (setq domain-constants (add-actions-to-domain solution domain agent new-predicates constants :original-domain-file original-domain-file))
;; 	   (setq new-domain-file (car domain-constants))
;; 			    ;; I am not sure I can do the sublis since that is private to the previous agent
;; 			    (add-predicates-to-problem domain (cdr new-problem) prev-state
;; 						       (if add-goals-p (sublis instances (sublis predicates (problem-lit-goals *current-problem*))))
;; 						       new-domain-file probsets-dir)))

(defun remove-irrelevant-defs (literals-defs literals-in-plan anonymous-internal-literals-alist literals-others)
  (remove-if-not #'(lambda (literal-def)
		     (and (member (car literal-def) literals-in-plan :key #'car)
			  (or (member (car literal-def) anonymous-internal-literals-alist :key #'cdr)
			      (member (car literal-def) literals-others :key #'car))))
		 literals-defs))

(defun remove-irrelevant-state (literals anonymous-internal-predicates-alist predicates-others)
  (remove-if-not #'(lambda (literal)
		     (or (member (car literal) anonymous-internal-predicates-alist :key #'cdr)
			 (member (car literal) predicates-others :key #'car)))
		 literals))
;; 		     (not (intersection constants (cdr literal)))))

;; sublis is not enough because I only want to change the cars of predicates-def. Otherwise, there could be a type with the same name of a predicate and I do not want to change that.
(defun replace-predicate-names (anonymous-internal-predicates-alist predicates-def)
  (mapcar #'(lambda (predicate-def)
	      (let ((found (cdr (assoc (car predicate-def) anonymous-internal-predicates-alist))))
		(if found
		    (cons found (cdr predicate-def))
		    predicate-def)))
	  predicates-def))

(defun remove-previous-constants (constants previous-constants)
  (flatten-instances (mapcan #'(lambda (type-def)
				 (let ((difference (set-difference (cdr type-def) (cdr (assoc (car type-def) previous-constants)))))
				   (if difference
				       (list (cons (car type-def) difference)))))
			     (process-instances constants))))

(defun join-constants (constants)
  (let ((new-constants nil))
    (dolist (constant-def constants)
      (if (assoc (car constant-def) new-constants)
	  (push (cadr constant-def) (cdr (assoc (car constant-def) new-constants)))
	  (push constant-def new-constants)))
    new-constants))

(defun constants-union (constants1 constants2)
  (let ((new-constants (mapcar #'(lambda (constant-def) (copy-list constant-def)) constants1)))
    (dolist (constant-def constants2)
      (if (assoc (car constant-def) new-constants)
	  (setf (cdr (assoc (car constant-def) new-constants))
		(union (cdr (assoc (car constant-def) new-constants)) (cdr constant-def)))
	  (push constant-def new-constants)))
    new-constants))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Generating a solution list augmented with preconds, effects and typed parameters. It also removes internal predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It takes a solution from an agent (agent-name) and a set of its internal-predicates and generates an external version of the solution for other agents
;; solution can be either a Sayphi structure or a list
;; internal-predicates is a list of predicate names that are internal to the agent
;; internal-instances is a list of agent specific instances (note that internal-instances in the other functions is an alist of internal instances of all agents while here is a list of this agents internal instances).
;; it requires to have the domain loaded through Sayphi
;; it returns a list of the form:
;; ;; ((action1 external-preconds1 external-adds1 external-dels1 constants-defs) ...)
;; As a side effect, it computes the union of all preconds, so that we can compute its intersection with the state in order to pass it anonymized to the following agents
(defun agent-solution (solution anonymous-internal-predicates-alist anonymous-internal-instances-alist agent-name)
  (mapcan #'(lambda (step)
	      (let ((action-step (agent-step step anonymous-internal-predicates-alist anonymous-internal-instances-alist agent-name)))
		(if action-step
		    (list action-step))))
	  solution))

;; both anonymous-internal-X are substitutions for internal predicates and instances, respectively
;; I will have to work out why sometimes action-name is not part of the domain actions
(defun agent-step (plan-step anonymous-internal-predicates-alist anonymous-internal-instances-alist agent-name)
  (let* ((action (if (snode-p plan-step)
		     (snode-plan-action plan-step)
		     plan-step))
	 (action-name (car action))
	 (action-struct (find action-name (dom-actions *pspace*) :key #'action-name)))
    (if action-struct
	(let* ((parameters (action-parameters action-struct))
	       (substitution (mapcar #'cons parameters (cdr action)))
	       (constants (mapc #'(lambda (action-constant)
				    (pushnew (list (get-object-type action-constant)
						   (sublis anonymous-internal-instances-alist (sublis substitution action-constant)))
					     *plan-constants* :test #'equal))
				(cdr action)))
	       (preconds (external-exp (action-preconditions action-struct) substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist t))
	       (adds (external-exp (action-adds action-struct) substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist nil))
	       (dels (external-exp (action-dels action-struct) substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist nil))
	       (new-action-name (if (member (car action) *original-actions*)
				    (format nil "~a-~a~{-~a~}" (car action) agent-name (sublis anonymous-internal-instances-alist (cdr action)))
				    (car action))))
	  (list (cons (if (stringp new-action-name) (intern new-action-name) new-action-name) (cdr action)) preconds adds dels)))))
;; 	       (constants (mapc #'(lambda (action-constant parameter)
;; 				    (pushnew (list (cdr parameter)
;; 						   (sublis anonymous-internal-instances-alist (sublis substitution action-constant)))
;; 					     *plan-constants* :test #'equal))
;; 				(cdr action) parameters))

;;; I cannot remove literals, since it would apply any action in almost any order. i have to substitute the internal predicates for dummy names all over
;;; and add the internal initial state of an agent into the initial state of the next one, again substituting the real predicates and args for dummies
;; It takes an expression (precond or effect) and substitutes anonymous-internal-predicates-alist anonymous-internal-instances-alist for their gensym equivalents
;; if precondsp=t, it adds the individual literals to the *union-preconds* list
(defun external-exp (exp substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist precondsp)
  (if exp
      (case (car exp)
	((and or) (let ((result (external-exp (cdr exp) substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist precondsp)))
		    (if result (cons (car exp) result))))
	(not (let ((result (external-exp (cadr exp) substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist precondsp)))
	       (if result (cons (car exp) (list result)))))
	((increase assign <= >= + * - / < >)
	 (let ((result1 (external-exp (cadr exp) substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist precondsp))
	       (result2 nil))
	   (when result1
	     (setq result2 (external-exp (caddr exp) substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist precondsp))
	     (if result2
		 (list (car exp) result1 result2)))))
	((forall exists) (format t "~%Warning: I am not processing yet forall, exists"))
	(otherwise ;; I hope list of literals or individual literal
	 (if (listp (car exp))
	     (mapcan #'(lambda (precond)
			 (let ((result (external-exp precond substitution anonymous-internal-predicates-alist anonymous-internal-instances-alist precondsp)))
			   (if result (list result))))
		     exp)
	     (let* ((internal-predicate (member (car exp) anonymous-internal-predicates-alist :key #'car))
		    (result (if internal-predicate
				(cons (cdar internal-predicate) (sublis anonymous-internal-instances-alist (sublis substitution (cdr exp) :test #'equal)))
				(sublis anonymous-internal-instances-alist (sublis substitution exp :test #'equal)))))
	       (dolist (arg (cdr result))
		 (pushnew (list (or (get-object-type arg)
				    (get-object-type (car (find arg anonymous-internal-instances-alist :key #'cdr))))
				arg)
			  *plan-constants* :key #'cadr))
	       (if precondsp
	      ;;;; IMPORTANT: REMOVE PRECONDS THAT ARE ADDED BY SOME OTHER ACTION. GOAL-REGRESSION
		   (pushnew result *union-preconds* :test #'equal)
		   (pushnew result *union-effects* :test #'equal))
	       result))))))

;; It takes an agent, a list of predicates or objects and whether those are predicates or objects and returns a substitution (alist) with anonymous names for the elements in the list
(defun anonymize (agent list list-type)
  (or (if (eq list-type :predicates)
	  (cadr (assoc agent *anonymize-alist*))
	  (caddr (assoc agent *anonymize-alist*)))
      (if (eq list-type :predicates)
	  (setf (cadr (assoc agent *anonymize-alist*))
		(mapcar #'(lambda (element)
			    (cons element (intern (format nil "ANON~d" (incf *anon-index*)))))
			list))
	  (setf (caddr (assoc agent *anonymize-alist*))
		(mapcar #'(lambda (element)
			    (cons element (intern (format nil "ANON~d" (incf *anon-index*)))))
			list)))))
		    ;; 	       (cons element (gensym "anon")))
		    ;; 	       (multiple-value-bind (sec min hor dia mes ano)
		    ;; 		   (get-decoded-time)
		    ;; 		 (declare (ignore dia mes ano))
		    ;; 		 (cons element (intern (format nil "ANON~d-~d-~d-~d" (random 100000) hor min sec)))))

;; I add the anonymized predicates into the domain of the next agent
(defun anonymize-state (state predicates new-predicates)
  ;; 	     (dolist (literal *union-preconds*)
  (let ((new-predicate nil))
    (dolist (literal state)
      (if (setq new-predicate (member (car literal) predicates :key #'cdr))
	  (pushnew (cons (car literal)
			 (mapcan #'(lambda (var-type) (list (intern (format nil "?~a" (car var-type))) '- (cdr var-type)))
				 (gethash (caar new-predicate)
					  (dom-predicates *pspace*))))
		   new-predicates :test #'equal)))
    new-predicates))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Adds the actions in the solution of a previous agent (agent-solution) into a new domain definition of agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; agent-solution is the augmented solution of the previous agent by using agent-solution function. It is a list of the form:
;; ((action1 external-preconds1 external-adds1 external-dels1 constants-defs) ...)
;; agent is the name of the next agent
;; new-domain-file is the new domain to be generated
;; original-domain-file is the original single agent domain
;; It returns the pathname of the new domain file
;; (defun add-actions-to-domain (agent-solution domain agent new-predicates previous-constants &key (new-domain-file (format nil "ma-domain-~(~a~).pddl" agent)) (original-domain-file "original-domain.pddl"))
;;   (let* ((domain-def (cdr (read-all-file (concatenate 'string *domains-dir* domain "/" original-domain-file))))
;; 	 (name (car (find-argument domain-def 'domain)))
;; 	 (requirements (find-argument domain-def :requirements))
;; 	 (types (find-argument domain-def :types))
;; 	 (predicates (append (find-argument domain-def :predicates) new-predicates))
;; 	 (functions (find-argument domain-def :functions))
;; 	 (actions-def (append (find-all-argument domain-def :action)
;; 			      (find-all-argument domain-def :durative-action)))
;; 	 (constants-alist (union previous-constants (collect-constants agent-solution) :test #'equal))
;;  	 (constants (flatten-constants constants-alist))
;; 	 (effects nil))
;;     (dolist (augmented-action agent-solution)
;;       (setq effects (caddr augmented-action))
;;       (dolist (del (cadddr augmented-action))
;; 	(push (list 'not del) effects))
;;       (pushnew `(:action ,(caar augmented-action) :parameters () :precondition (and ,@(cadr augmented-action)) :effect (and ,@effects))
;; 	       actions-def :test #'equal))
;;     (write-domain-pddl-file name requirements types predicates functions actions-def (concatenate 'string *domains-dir* domain "/" new-domain-file) constants)
;;     (cons new-domain-file constants-alist)))

(defun collect-constants (agent-solution)
  (let ((constants nil))
    (dolist (step agent-solution)
      (setq constants (union (mapcar #'(lambda (constant-def) (list (cdr constant-def) (car constant-def)))
				     (nth 4 step))
			     constants :test #'equal)))
    constants))

(defun flatten-constants (constants)
  (mapcan #'(lambda (constant-def)
	      (list (car constant-def) '- (cdr constant-def)))
	  constants))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Adds the init state and goals from the problem of a previous agent into a new domain definition of agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun add-predicates-to-problem (domain problem anonymous-state anonymous-goals domain-file probsets-dir)
;;   (declare (special *domain-dir*))
;;   (let ((problem-def (cdr (read-all-file (concatenate 'string *domain-dir* probsets-dir problem)))))
;;     (write-pddl-file (pathname-name problem) domain (find-argument problem-def :objects)
;; 		     (union anonymous-state (find-argument problem-def :init) :test #'equal)
;; 		     (union anonymous-goals (cdar (find-argument problem-def :goal)) :test #'equal)
;; 		     (concatenate 'string *domain-dir* probsets-dir problem)
;; 		     (find-argument problem-def :metric))))
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Given a single agent problem and domain and a list of agent-types, it generates a problem for each agent of agent-type in the problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; internal-predicates is a list of predicates/functions that are internal to each agent. In the future, I will compute them automatically
;; internal-instances-others is an alist of instances that are internal each agent, so we have to remove them from the other agents problems. In the future, I will try to compute them automatically
;;    e.g. ((rover0 camera0 rover0store objective0) (rover1 camera1 rover1store objective1) ...)
;; domain-file has to be the original version of the domain. E.g. StripsRover.pddl and not the multi-agent version
;; agent-types will be a list of the types to convert into agents. E.g. rover, truck, package, ...

;; example: (write-ma-problem "ma-rovers" "original-p03.pddl" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of calibration_target on_board visible_from) '((rover0 camera1 rover0store) (rover1 camera0 rover1store)) :domain-file "original-domain.pddl")
(defun write-ma-problem (domain problem agent-types internal-predicates internal-types &key (probsets-dir "probsets/") (domain-file "original-domain.pddl")
											 (goal-selection 'all-achievable))
  (declare (special *current-problem*))
  (say-domain domain domain-file)
  (setf *original-actions* (mapcar #'action-name (dom-actions *pspace*)))
  (let* ((init-time (get-internal-real-time))
	 (domain-name (car (dom-name *pspace*)))
	 (problem-file (concatenate 'string *domain-dir* probsets-dir problem))
	 (problem-def (cdr (read-all-file problem-file)))
	 (name (pathname-name problem))
	 (instances (find-argument problem-def :objects))
	 (instances-list (process-instances instances))
	 (agents (mapcan #'(lambda (agent-type) (cdr (assoc agent-type instances-list))) agent-types))
	 (state (find-argument problem-def :init))
	 (goals (if *compute-landmarks-p*
		    (compute-landmarks (cdar (find-argument problem-def :goal)) state *domain-file* problem-file)
		    (cdar (find-argument problem-def :goal))))
	 (global-method-p (or (eq goal-selection 'load-balance) (eq goal-selection 'best-cost)))
	 (goals-array (if global-method-p (make-array (list (length goals)) :initial-element nil)))
	 (agents-array (if global-method-p (make-array (list (length agents))  :initial-element nil)))
	 (metric (find-argument problem-def :metric))
	 new-problem new-name new-instances new-state new-goals new-problems internal-instances-others)
;; 	 (list-internal-instances (reduce #'append internal-instances)))
;;    (say-domain domain new-domain-file)
    (parse-problem problem-def)
    (setf *internal-instances* (compute-internal-instances internal-types state agents))
    (do* ((the-agents agents (cdr the-agents))
	  (agent (car the-agents) (car the-agents))
	  (i 0 (1+ i)))
	((null the-agents))
      (setq new-name (format nil "~(~a~)-~(~a~).pddl" agent name))
      (setq internal-instances-others (compute-others-instances agent *internal-instances*))
      (setq new-instances (common-instances instances-list internal-instances-others))
;;       (setq new-instances (filter-agent-instances agent agent-type instances-list internal-instances-others))
      (setq new-state (common-literals state internal-predicates internal-instances-others))
      (setq new-goals (common-literals goals internal-predicates internal-instances-others))
      (setq new-problem (concatenate 'string *domain-dir* probsets-dir new-name))
      ;; I might go without actually writing a file. I might only need to call the corresponding function parse-problem with a list as argument
      ;; with the appropriate syntax (the cdr of the same reading of the problem files)
      (write-pddl-file name domain-name new-instances new-state new-goals new-problem metric)
      (load-prob new-problem)
      ;; se que es una chapuza que el contenido de goals-array y agents-array cambie segun goal-selection, pero algun dia lo arreglare
      (case goal-selection
	(all-achievable (setq new-goals (filter-agent-goals agent goals internal-predicates internal-instances-others)))
	(rest-achievable (setq new-goals (filter-agent-goals agent goals internal-predicates internal-instances-others))
			 (setq goals (set-difference goals new-goals :test #'equal)))
	(load-balance (setq new-goals (filter-agent-goals agent goals internal-predicates internal-instances-others))
		      (setf (aref agents-array i)
			    (list i new-name new-goals)))
	(best-cost (setq goals-array (filter-agent-goals agent goals internal-predicates internal-instances-others goals-array))
		   (setf (aref agents-array i) (list new-name)))
	(t nil))
      ;; it might be that some problems have empty goals
      (when (and new-goals (not global-method-p))
	(if *trace-ma-sayphi* (format t "~%Agent ~a~%Assigned goals: ~a" agent new-goals))
	(write-pddl-file name domain-name new-instances new-state new-goals new-problem metric)
	(push (cons agent new-name) new-problems)))
    (if (and *trace-ma-sayphi* global-method-p) (format t "~%Goals array: ~a~%Agents array: ~a" goals-array agents-array))
    ;; I assume goals do not include agents names (do not have to be filtered)
    (setq new-problems (case goal-selection
			 (best-cost (best-cost-problems goals goals-array agents agents-array domain-name probsets-dir))
			 (load-balance (load-balance-problems goals goals-array agents agents-array domain-name probsets-dir))
			 (otherwise (nreverse new-problems))))
    (format t "~%Goals assignment time: ~,2f" (elapsed-time init-time 'real-time))
    new-problems))
;;     ;; it returns an augmented solution
;;     `(,new-problems (nil ,(common-instances instances-list internal-instances)
;; 			 ,(common-literals state internal-predicates list-internal-instances)
;; 			 ,(common-literals goals internal-predicates list-internal-instances)
;; 			 ,metric))))

;; Returns Goals+(Landmarks-InitState)
(defun compute-landmarks (goals state domain-file problem-file)
  (let ((landmarks nil))
;; 	(readtable (copy-readtable)))
;;     (setf *readtable* (copy-readtable nil))
;;     (set-macro-character #\, #'(lambda (stream char) (declare (ignore stream char)) (values)))
    (execute-shell-command (concatenate 'string  "PATH=$PATH:/opt/local/bin/; cd " *my-planning-path* "fd-landmarks; ./compute-landmarks.sh " domain-file " " problem-file)
			   (concatenate 'string *my-tmp-path* "run-planner"))
    (with-open-file (istream *landmarks-file* :direction :input)
      (do ((landmark (read istream nil :eof) (read istream nil :eof)))
	  ((eq landmark :eof))
	(push landmark landmarks)))
    (when *trace-ma-sayphi*
      (format t "~%Landmarks detected: ~a~%" (length landmarks))
      (pp-list landmarks 3)
      (format t "~%Goals: ~a~%" (length goals))
      (pp-list goals 3)
      (format t "~%Result: ~a~%" (length (union goals (set-difference landmarks state :test #'equal) :test #'equal)))
      (pp-list (union goals (set-difference landmarks state :test #'equal) :test #'equal) 3))
;;     (setf *readtable* readtable)
    (union goals (setf *landmarks* (set-difference landmarks state :test #'equal)) :test #'equal)))


;; it returns the type definition of common instances (those that do not contain an internal instance of the rest of agents)
(defun common-instances (instances-list internal-instances)
  (flatten-instances (mapcan #'(lambda (type-def)
				 (let ((common-instances (remove-if #'(lambda (instance)
									(member instance internal-instances))
;; 										:test #'(lambda (the-instance the-internal-instances)
;; 											  (member the-instance the-internal-instances))))
								    (cdr type-def))))
				   (if common-instances
				       (list (cons (car type-def) common-instances)))))
			     instances-list)))

;; it removes internal predicates/functions from a set of literals (state, goals, ...)
(defun common-literals (literals internal-predicates internal-instances)
  (mapcan #'(lambda (literal)
	      (if (eq (car literal) '=)
		  (let ((result (remove-internal-from-literal (cadr literal) internal-predicates internal-instances)))
		    (if result
			`((= ,(car result) ,(caddr literal)))))
		  (remove-internal-from-literal literal internal-predicates internal-instances)))
	  literals))

;; I assume the internal-predicates predicates do not contain another different agent
;; So there shouldn't be an internal-predicate as (communicating rover0 rover1). It should be external
(defun remove-internal-from-literal (literal internal-predicates internal-instances)
  (if (not (intersection (cdr literal) internal-instances :test #'eq))
      (list literal)))

(defun load-balance-problems (goals goals-array agents agents-array domain-name probsets-dir)
  (let* ((ordered-agents (remove-if #'(lambda (agent-goals) (< (length agent-goals) 3))
				    (sort (coerce agents-array 'list) #'< :key #'length)))
	 (balance (if (> (length ordered-agents) 0)
		      (/ (length goals) (length ordered-agents) 1.0)
		      0))
	 (ordered-goals nil)
	 (assignments (make-array (length agents) :initial-element nil))
	 (value 0) (best-value 0) (best 0)
	 problem name new-problems problem-def new-goals)
    (if *trace-ma-sayphi* (format t "~%Ordered agents: ~a" ordered-agents))
    (if *trace-ma-sayphi* (format t "~%Balance: ~a" balance))
    (cond ((> balance 0)
	   ;; first compute which agents can achieve each goal. goals-array should be empty
	   (dotimes (i (length goals))
	     (setf (aref goals-array i)
		   ;; I add i because I will order them later and to be able to access it after reordering
		   (cons i
			 (mapcan #'(lambda (agent)
				     (if (member (nth i goals) (caddr agent) :test #'equal)
					 (list (list (car agent) (cadr agent)))))
				 ordered-agents)))
	     (if (null (aref goals-array i))
		 (format t "~%Careful: goal ~a cannot be achieved by any agent" (nth i goals))))
	   (setq ordered-goals (sort (coerce goals-array 'list) #'< :key #'length))
	   (if *trace-ma-sayphi* (format t "~%Goals array: ~a" goals-array))
	   (if *trace-ma-sayphi* (format t "~%Ordered goals: ~a" ordered-goals))
	   ;; greedy algorithm to assign goals to less occupied agents
	   (dolist (goal-agents ordered-goals)
	     (setq best-value most-positive-fixnum)
	     (setq best nil)
	     (dolist (agent (cdr goal-agents))
	       (when (< (setq value (length (aref assignments (car agent))))
			best-value)
		 (setq best-value value)
		 (setq best agent)))
	     (push (car goal-agents) (aref assignments (car best))))
	   (if *trace-ma-sayphi* (format t "~%Assignments: ~a" assignments)))
	  (t (format t "~%BIG PROBLEM: no goal can be achieved by any agent")))
    ;; now, we save in the agents files
    (dotimes (i (length agents))
      (when (aref assignments i)
	(setq problem (cadr (aref agents-array i)))
	(push (cons (nth i agents) problem) new-problems)
	(setq problem-def (cdr (read-all-file (concatenate 'string *domain-dir* probsets-dir problem))))
	;;	(load-prob (concatenate 'string *domain-dir* probsets-dir problem))
	(setq name (pathname-name problem))
	(setq new-goals (mapcar #'(lambda (goal-index) (nth goal-index goals))
				(aref assignments i)))
	(if *trace-ma-sayphi* (format t "~%Agent ~a~%Assigned goals: ~a" (nth i agents) new-goals))
	(write-pddl-file name domain-name (find-argument problem-def :objects) (find-argument problem-def :init)
			 new-goals
			 (concatenate 'string *domain-dir* probsets-dir problem)
			 (find-argument problem-def :metric))))
      new-problems))

(defun best-cost-problems (goals goals-array agents agents-array domain-name probsets-dir)
  (dotimes (i (length goals))
    (if (aref goals-array i)
	(push (nth i goals)
	      (cdr (aref agents-array (position (car (aref goals-array i)) agents))))
	(format t "~%Careful: goal ~a cannot be achieved by any agent" (nth i goals))))
  (if *trace-ma-sayphi* (format t "~%Goals array: ~a~%Agents array: ~a" goals-array agents-array))
  (let* (problem name new-problems problem-def)
    (dotimes (i (length agents-array))
      (when (cdr (aref agents-array i)) ;; it is the best agent for some goal
	(setq problem (car (aref agents-array i)))
	(push (cons (nth i agents) problem) new-problems)
	(setq problem-def (cdr (read-all-file (concatenate 'string *domain-dir* probsets-dir problem))))
;;	(load-prob (concatenate 'string *domain-dir* probsets-dir problem))
	(setq name (pathname-name problem))
	(if *trace-ma-sayphi* (format t "~%Agent ~a~%Assigned goals: ~a" (nth i agents) (cdr (aref agents-array i))))
	(write-pddl-file name domain-name (find-argument problem-def :objects) (find-argument problem-def :init)
			 (cdr (aref agents-array i))
			 (concatenate 'string *domain-dir* probsets-dir problem)
			 (find-argument problem-def :metric))))
    new-problems))

(defun compute-others-instances (agent internal-instances)
  (let ((instances nil))
    (dolist (internal internal-instances)
      (if (not (eq (car internal) agent))
	  (setq instances (union instances internal))))
    instances))

;; it is substitued by common-instances
;; (defun filter-agent-instances (agent agent-type instances-list internal-instances-others)
;;   (flatten-instances (mapcan #'(lambda (type-def)
;; 				 (if (eq (car type-def) agent-type)
;; 				     (list (list (car type-def) agent))
;; 				     (let ((new-instances (remove-if #'(lambda (instance) (member instance internal-instances-others))
;; 								     (cdr type-def))))
;; 				       (if new-instances
;; 					   (list (cons (car type-def) new-instances))))))
;; 			     instances-list)))

(defun filter-agent-goals (agent goals internal-predicates internal-instances-others &optional goals-array)
  (declare (special *current-problem*))
  (set-duplicate-hashing)
  (let ((i-node (initialize-current-problem))
	(new-goals nil))
    (if *trace-ma-sayphi* (format t "~%heuristic all goals: ~,2f" (h-metric-rxplan i-node)))
    (do* ((all-goals goals (cdr all-goals))
	  (goal nil)
	  (i 0 (1+ i))
	  (heuristic 0))
	 ((null all-goals)
	  (or goals-array new-goals))
      (setq goal (car (remove-internal-from-literal (car all-goals) internal-predicates internal-instances-others)))
      (when goal
	(setf (problem-goals *current-problem*) (literalgoals-bitmap (list goal)))
	(setq heuristic (h-metric-rxplan i-node))
	;;      (setq heuristic (funcall (say-plan-defaults 'heuristic) i-node))
	(if *trace-ma-sayphi* (format t "~%goal: ~a, heuristic: ~,2f, agent: ~a" goal heuristic agent))
	(if (< heuristic most-positive-fixnum)
	    (if goals-array
		(if (or (null (aref goals-array i)) (< heuristic (cdr (aref goals-array i))))
		    (setf (aref goals-array i) (cons agent heuristic)))
		(push goal new-goals)))))))

(defun compute-internal-instances (internal-types state agents)
  (let ((internal-instances (mapcar #'list agents))
	(agent nil))
    (dolist (literal state)
      (setq agent (intersection (cdr literal) agents))
      (if (= (length agent) 1)
	  (dolist (arg (cdr literal))
	    (if (member (get-object-type arg) internal-types)
		(pushnew arg (cdr (assoc (car agent) internal-instances)))))))
    internal-instances))

;;;;;;;;;;;;;;;;
;;; Aux fns.
;;;;;;;;;;;;;;;;

(defun ediff-problems (problem1 problem2 domain)
  (let ((problem-def1 (cdr (read-all-file (concatenate 'string *domain-dir* "probsets/" problem1))))
	(problem-def2 (cdr (read-all-file (concatenate 'string *domain-dir* "probsets/" problem2)))))
    (format t "~%State in ~a and not in ~a:~% ~a" problem1 problem2 (set-difference (find-argument problem-def1 :init) (find-argument problem-def2 :init) :test #'equal))
    (format t "~%State in ~a and not in ~a:~% ~a" problem2 problem1 (set-difference (find-argument problem-def2 :init) (find-argument problem-def1 :init) :test #'equal))
    (format t "~%Goals in ~a and not in ~a:~% ~a" problem1 problem2 (set-difference (cdar (find-argument problem-def1 :goal)) (cdar (find-argument problem-def2 :goal)) :test #'equal))
    (format t "~%Goals in ~a and not in ~a:~% ~a" problem2 problem1 (set-difference (cdar (find-argument problem-def2 :goal)) (cdar (find-argument problem-def1 :goal)) :test #'equal))))

(defun validate-sol (&key (solution *say-solution*) (domain nil) (domain-file nil) (problem-file nil))
  (if (and domain domain-file)
      (say-domain domain domain-file))
  (if problem-file
      (prob problem-file))
  (do* ((state (problem-lit-init-state *current-problem*))
	(plan (if (solution-p solution) (solution-path solution) solution) (cdr plan))
	(plan-step (car plan) (car plan))
	(invalid nil))
      ((or (null plan) invalid)
       (and (not invalid)
	    (every #'(lambda (goal) (member goal state :test #'equal))
		   (problem-lit-goals *current-problem*))))
    (let* ((action (if (snode-p plan-step)
		       (snode-plan-action plan-step)
		       plan-step))
	   (action-name (car action))
	   (action-struct (find action-name (dom-actions *pspace*) :key #'action-name))
	   (parameters (action-parameters action-struct))
	   (substitution (mapcar #'cons parameters (cdr action))))
      (cond ((every #'(lambda (precond) (member precond state :test #'equal))
		    (sublis substitution (action-preconditions action-struct)))
	     (setq state (union (set-difference state (sublis substitution (action-dels action-struct)) :test #'equal)
				(sublis substitution (action-adds action-struct)) :test #'equal)))
	    (t (format t "~2%Invalid action: ~a~%in state: ~a" action state)
	       (setq invalid t))))))

(defun generate-plot-ma (file ofile)
  (let ((all-results (read-all-file file))
	(num-solved-centralized 0)
	(num-solved-distributed 0))
    (with-open-file (ostream ofile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (do* ((results all-results (cddr results)))
	   ((null results))
	(cond ((nth 2 (caar results))
	       (incf num-solved-centralized)
	       (format ostream "~%~,2f ~d ~d" (nth 5 (caar results)) (nth 6 (caar results)) (nth 7 (caar results))))
	      (t (format ostream "~%0.0 0 0")))
	(cond ((nth 2 (cadar results))
	       (incf num-solved-distributed)
	       (format ostream " ~,2f ~d ~d" (nth 5 (caadr results)) (nth 6 (caadr results)) (nth 7 (caadr results))))
	      (t (format ostream " 0.0 0 0")))))
    (format t "~2%#solved centralized: ~d, #solved distributed: ~d" num-solved-centralized num-solved-distributed)))
  
