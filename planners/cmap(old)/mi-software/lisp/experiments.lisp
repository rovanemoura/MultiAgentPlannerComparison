(load-errtplan)

(defun heuristic-test (&key (domain "n-puzzle") (domain-file "n-puzzle-typed.pddl") (algorithm 'a-star) (heuristic 'h-domain-dependent) (probs-prefix "boot") (timeout 60))
  (load-sayphi)
  ;; not needed
;;   (load-learner 'ddh)
  (load (concatenate 'string *domains-dir* domain "/heuristics.lisp"))
  (compile-file (concatenate 'string *domains-dir* domain "/heuristics.lisp"))
  (load (concatenate 'string *domains-dir* domain "/heuristics"))
  (runset domain domain-file :algorithm algorithm :heuristic heuristic :timeout timeout :probs-prefix probs-prefix :output-level 3))

(defun test-invented-linkability nil
  (errt-runset :domain "invented" :domain-file "invented-linkability.pddl" :prefix "train-invented-linkability" :modify-problem 'from-probset
	       :modified-probset-prefix "test-invented-linkability"
	       :sayphi-algorithm 'ff :say-timeout 60 :number-repetitions 5 :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0)
	       :initial-solutions (list (concatenate 'string *my-planning-path* "sayphi/domains/invented/probsets/solution-invented.lisp"))))

(defun test-gold-miner nil
  ;; max-modifications of the state (though it really is where the goal is, so we are modifying the gold position) should be less than 4 (connected locations)
  (errt-runset :domain "gold-miner" :domain-file "gold-miner-typed.pddl" :prefix "gold-miner-target" :modify-problem 'modify-state :modified-probset-prefix "mod-gold-miner-"
               :number-repetitions 10 :max-modifications 3 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600))

(defun test-cup nil
  (let ((results-file (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-cup" :modify-problem 'from-probset :modified-probset-prefix "pddl-test-cup"
				   :number-repetitions 5 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600)))
    (summary-results results-file :domain "test-grid" :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :number-repetitions 5 :number-modifications 1 :x-labels (list "0" "10" "20" "30" "40" "50" "60" "70" "80" "90" "100" "110" "120" "130" "140" "150" :accumulated-p nil))))

(defun test-l nil
    (let ((results-file (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-l" :modify-problem 'from-probset :modified-probset-prefix "rev-pddl-new-test-l"
				     :number-repetitions 5 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600)))
      (summary-results results-file :domain "test-grid" :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :number-repetitions 5 :number-modifications 1
		       :x-labels (list "0" "10" "20" "30" "40" "50" "60" "70" "80" "90" "100" "110" "120" "130" "140" "150" :accumulated-p nil))))

(defun test-key nil
  "Test key domain"
  (errt-runset :domain "key-grid" :domain-file "domain-key.pddl" :prefix "train-" :modify-problem 'from-probset :modified-probset-prefix "test-key"
               :number-repetitions 3 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 200))

(defun test-two-keys nil
  "Test two keys domain"
  (errt-runset :domain "two-keys" :domain-file "domain.pddl" :prefix "train-" :modify-problem 'from-probset :modified-probset-prefix "test-"
               :number-repetitions 3 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 200))

(defun test-cup-sc nil
  "Experiments to see how changing the initial state of the robot in 1 position affects"
  (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-cup" :modify-problem 'from-probset :modified-probset-prefix "pddl-test-cup-sc-"
               :number-repetitions 10 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 300))

(defun test-cup-2sc nil
  "Experiments to see how changing the initial state of the robot in 2 positions affects"
  (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-cup" :modify-problem 'from-probset :modified-probset-prefix "pddl-test-cup-2sc-"
               :number-repetitions 5 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600))

(defun test-cup-gc nil
  "Experiments to see how changing the goal of the robot in 1 position affects"
  (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-cup" :modify-problem 'from-probset :modified-probset-prefix "pddl-test-cup-gc"
               :number-repetitions 5 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600))

;; (defun test-l nil
;;   (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-l" :modify-problem 'from-probset :modified-probset-prefix "pddl-new-test-l"
;; 	       :number-repetitions 50 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 1800))

;; we can try this out with training with blocks06 and so on
(defun test-plateaux nil
  (errt-runset :domain "blocksworld" :domain-file "domain.pddl" :prefix "train-blocks05" :number-repetitions 10 :sayphi-algorithm 'ff
	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600 :max-modifications 1
	       :modify-problem 'from-probset :modified-probset-prefix "blocks"))

;; starting with the hardest problem solved by FF (block08)
(defun test-plateaux-1 nil
  (errt-runset :domain "blocksworld" :domain-file "domain.pddl" :prefix "train-blocks08" :number-repetitions 10 :sayphi-algorithm 'ff
	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 900 :max-modifications 1
	       :modify-problem 'from-probset :modified-probset-prefix "blocks"))

;; starting with a different problem (block on top of initial tower starts on table)
(defun test-plateaux-2 nil
  (errt-runset :domain "blocksworld" :domain-file "domain.pddl" :prefix "train-blocks05-sc" :number-repetitions 10 :sayphi-algorithm 'ff
	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600 :max-modifications 1
	       :modify-problem 'from-probset :modified-probset-prefix "blocks"))

(defun test-difficulty nil
  (errt-runset :domain "driverlog" :domain-file "driverlog.pddl" :prefix "pfile" :number-repetitions 10 :sayphi-algorithm 'ff
	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 300 :max-relaxations 15 :max-modifications 5))

(defun test-solving-fast nil
  (errt-runset :domain "driverlog" :domain-file "driverlog.pddl" :prefix "pfile" :number-repetitions 10 :sayphi-algorithm 'ff
	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 1 :max-relaxations 15 :max-modifications 5 :start-with-subgoal t :max-starts 15))

(defun test-domain (domain domain-file prefix)
  (let ((results-file (errt-runset :domain domain :domain-file domain-file :prefix prefix :number-repetitions 5 :sayphi-algorithm 'ff :max-relaxations 0
				   :max-modifications 3 :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600)))
    (summary-results (concatenate 'string (pathname-name results-file) (pathname-extension results-file)) :domain domain :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :number-repetitions 5 :number-modifications 3
		     :accumulated-p nil)))

(defun test-blocks nil (test-domain "blocksworld" "blocksworld.pddl" "probBLOCKS"))
(defun test-gripper nil (test-domain "gripper" "domain.pddl" "prob"))

(defun test-driverlog nil (test-domain "driverlog" "driverlog.pddl" "pfile"))
(defun test-driverlog-17 nil (test-domain "driverlog" "driverlog.pddl" "pfile17"))

(defun test-complex nil (test-domain "driverlog" "driverlog.pddl" "pfile20"))

(defun test-depots nil (test-domain "depots" "Depots.pddl" "pfile"))

(defun test-rovers nil (test-domain "rovers" "StripsRover.pddl" "pfile"))

(defun test-rovers-1 nil (test-domain "rovers" "StripsRover.pddl" "p3"))

(defun test-parking nil (test-domain "parking2011" "parking.pddl" "pfile"))

(defun test-freecell nil (test-domain "freecell" "freecell.pddl" "new-probfreecell"))

(defun test-satellite nil (test-domain "satellite" "satellite.pddl" "pfile"))

(defun test-zenotravel nil (test-domain "zenotravel" "ZenoStrips.pddl" "pfile"))

(defun test-stability nil
  (errt-runset :domain "driverlog" :domain-file "driverlog.pddl" :prefix "pfile20" :modify-problem 'from-probset :modified-probset-prefix "pfile-I"
	       :sayphi-algorithm 'ff :say-timeout 60 :number-repetitions 5 :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0)
	       :initial-solutions (list "/Users/dborrajo/planning/sayphi/domains/driverlog/probsets/solution.lisp")))

(defun test-stability-log nil
  (errt-runset :domain "logistics" :domain-file "domain.pddl" :prefix "logistics41" :modify-problem 'from-probset :modified-probset-prefix "pfile-I"
	       :sayphi-algorithm 'ff :say-timeout 60 :number-repetitions 5 :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0)
	       :initial-solutions (list "/Users/dborrajo/planning/sayphi/domains/logistics/probsets/solution.lisp")))

(defun test-stability-rovers nil
;;   (errt-runset :domain "rovers" :domain-file "StripsRover.pddl" :prefix "pfile20" :modify-problem 'add-goal
;; 	       :number-repetitions 5  :sayphi-algorithm 'ff :max-relaxations 0
;; 	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 60
;; 	       :max-goals 1 :randomp nil :max-modifications 3)
;;   (errt-runset :domain "rovers" :domain-file "StripsRover.pddl" :prefix "pfile20" :modify-problem 'add-goal
;; 	       :number-repetitions 5  :sayphi-algorithm 'ff :max-relaxations 0
;; 	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 60
;; 	       :max-goals 2 :randomp nil :max-modifications 3)
;;   (errt-runset :domain "rovers" :domain-file "StripsRover.pddl" :prefix "pfile20" :modify-problem 'add-goal
;; 	       :number-repetitions 5  :sayphi-algorithm 'ff :max-relaxations 0
;; 	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 60
;; 	       :max-goals 3 :randomp nil :max-modifications 3)
;;   (errt-runset :domain "rovers" :domain-file "StripsRover.pddl" :prefix "pfile20" :modify-problem 'add-goal
;; 	       :number-repetitions 5  :sayphi-algorithm 'ff :max-relaxations 0
;; 	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 60
;; 	       :max-goals 4 :randomp nil :max-modifications 3)
;;   (errt-runset :domain "rovers" :domain-file "StripsRover.pddl" :prefix "pfile20" :modify-problem 'add-goal
;; 	       :number-repetitions 5  :sayphi-algorithm 'ff :max-relaxations 0
;; 	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600
;; 	       :max-goals 5 :randomp nil :max-modifications 3)
  (errt-runset :domain "rovers" :domain-file "StripsRover.pddl" :prefix "p33" :modify-problem 'add-goal
	       :number-repetitions 5  :sayphi-algorithm 'ff :max-relaxations 0
	       :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600
	       :max-goals 5 :randomp nil :max-modifications 3))

(defun test-mini-stability nil
  (errt-runset :domain "driverlog" :domain-file "driverlog.pddl" :prefix "pfile20" :modify-problem 'from-probset :modified-probset-prefix "pfile-I0-G1-n1"
	       :sayphi-algorithm 'ff :say-timeout 120 :number-repetitions 3 :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0)
	       :initial-solutions (list "/Users/dborrajo/planning/sayphi/domains/driverlog/probsets/solution.lisp")))

(defun test-blocks-stability nil
  ;; set to T if you want to run FF/any other planner
  (setf *run-plain-sayphi-p* nil)
  (errt-runset :domain "blocksworld" :domain-file "domain.pddl" :prefix "blocks29" :modify-problem 'from-probset :modified-probset-prefix "blocks30"
	       :sayphi-algorithm 'ff :say-timeout 300 :number-repetitions 1 :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :max-relaxations 1
	       :initial-solutions (list "/Users/dborrajo/planning/sayphi/domains/blocksworld/probsets/solution-29.lisp")))

(defun test-floortile-stability nil
  ;; set to T if you want to run FF/any other planner
  (setf *run-plain-sayphi-p* nil)
  (errt-runset :domain "floortile" :domain-file "domain.pddl" :prefix "seq-p01-001" :modify-problem 'from-probset :modified-probset-prefix "daniel"
	       :sayphi-algorithm 'ff :say-timeout 200 :number-repetitions 1 :p (list 0.0 0.3) :r (list 0.3 0.7) :max-relaxations 1
	       :initial-solutions (list "/Users/dborrajo/planning/sayphi/domains/floortile/probsets/solution-ft-001.lisp")))

(defun test-floortile-add-goal nil
  ;; set to T if you want to run FF/any other planner
  ;;  (setf *run-plain-sayphi-p* nil)
  ;;  (trace errt-runset)
  (dotimes (goals 4)
    (dolist (prob (list "seq-p01-001" "seq-p01-002")) ;;  (list "seq-p01-001" "seq-p01-002")   "seq-p02-003" "seq-p02-004"
      (dolist (stochastic-p (list nil t))
	(if stochastic-p
	    (errt-runset :domain "floortile" :domain-file "domain.pddl" :prefix prob :modify-problem 'from-probset
			 :modified-probset-prefix (concatenate 'string "mod*-" prob) :say-timeout 600
			 :number-repetitions 1  :sayphi-algorithm 'ff :max-relaxations 0  :max-modifications 3
			 :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :stochastic-p stochastic-p
			 :initial-solutions (list (concatenate 'string "/Users/dborrajo/planning/sayphi/domains/floortile/probsets/sol-" prob ".lisp"))
			 :max-goals (1+ goals) :randomp nil)
	    (errt-runset :domain "floortile" :domain-file "domain.pddl" :prefix prob :modify-problem 'add-goal :say-timeout 600
			 :number-repetitions 1  :sayphi-algorithm 'ff :max-relaxations 0  :max-modifications 3
			 :stochastic-p nil
			 :initial-solutions (list (concatenate 'string "/Users/dborrajo/planning/sayphi/domains/floortile/probsets/sol-" prob ".lisp"))
			 :max-goals (1+ goals) :randomp nil))))))

(defun test-floortile-stochastic nil
  (dolist (stochastic-p (list t nil))
    (errt-runset :domain "floortile" :domain-file "domain.pddl" :prefix "seq-p01-001" :modify-problem 'from-probset
		 :modified-probset-prefix "mod0-1-seq-p01-001"
		 :sayphi-algorithm 'ff :say-timeout 100 :number-repetitions 1 :p (list 0.0 0.3) :r (list 0.3 0.7) :max-relaxations 0
		 :stochastic-p stochastic-p)
    (errt-runset :domain "floortile" :domain-file "domain.pddl" :prefix "seq-p01-001" :modify-problem 'from-probset
		 :modified-probset-prefix "mod1-1-seq-p01-001"
		 :sayphi-algorithm 'ff :say-timeout 100 :number-repetitions 1 :p (list 0.0 0.3) :r (list 0.3 0.7) :max-relaxations 0
		 :stochastic-p stochastic-p)
    (errt-runset :domain "floortile" :domain-file "domain.pddl" :prefix "seq-p01-002" :modify-problem 'from-probset
		 :modified-probset-prefix "mod0-1-seq-p01-002"
		 :sayphi-algorithm 'ff :say-timeout 100 :number-repetitions 1 :p (list 0.0 0.3) :r (list 0.3 0.7) :max-relaxations 0
		 :stochastic-p stochastic-p)
    (errt-runset :domain "floortile" :domain-file "domain.pddl" :prefix "seq-p01-002" :modify-problem 'from-probset
		 :modified-probset-prefix "mod1-1-seq-p01-002"
		 :sayphi-algorithm 'ff :say-timeout 100 :number-repetitions 1 :p (list 0.0 0.3) :r (list 0.3 0.7) :max-relaxations 0
		 :stochastic-p stochastic-p)))

(defun test-floortile-complex nil
  ;; set to T if you want to run FF/any other planner
  ;;  (setf *run-plain-sayphi-p* nil)
;;  (trace errt-runset)
  (errt-runset :domain "floortile" :domain-file "domain.pddl" :prefix "seq-p01-001" :modify-problem 'from-probset :modified-probset-prefix "daniel"
	       :sayphi-algorithm 'ff :say-timeout 200 :number-repetitions 1 :p (list 0.0 0.3) :r (list 0.3 0.7) :max-relaxations 1
	       :initial-solutions (list "/Users/dborrajo/planning/sayphi/domains/floortile/probsets/solution-ft-001.lisp")))

(defun test-stochastic (domain domain-file prob)
  (dolist (goals (list 1 5 10)) ;; 1
    (dolist (stochastic-p (list nil t))
      (cond (stochastic-p
	     (errt-runset :domain domain :domain-file domain-file :prefix prob :modify-problem 'from-probset
			  :modified-probset-prefix (concatenate 'string "mod*-" goals "-" prob) :say-timeout 600
			  :number-repetitions 1  :sayphi-algorithm 'ff :max-relaxations 0  :max-modifications 3
			  :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :stochastic-p stochastic-p
			  :max-goals goals :randomp nil))
	    (t (errt-runset :domain domain :domain-file domain-file :prefix prob :modify-problem 'add-goal :say-timeout 600
			    :number-repetitions 1  :sayphi-algorithm 'ff :max-relaxations 0  :max-modifications 3
			    :stochastic-p nil
			    :max-goals goals :randomp nil))))
    (execute-shell-command (concatenate 'string "mkdir " *domain-dir* "probsets/mprobs-"
					(format nil "~d" goals) "; mv " *domain-dir* "/probsets/mod*-"
					*domain-dir* "/probsets/mprobs-" (format nil "~d" goals) "/"))))

(defun test-rovers-stochastic nil (test-stochastic "rovers" "StripsRover.pddl" "pfile20"))
(defun test-satellite-stochastic nil (test-stochastic "satellite" "satellite.pddl" "pfile20"))
(defun test-depots-stochastic nil (test-stochastic "depots" "Depots.pddl" "pfile22"))
(defun test-parking-stochastic nil
  (dolist (prob (list "pfile08-031" "pfile08-032")) ;;  "pfile09-033"  "pfile09-034"
    (test-stochastic "parking2011" "parking.pddl" prob)))

(defun test-bridge nil
  (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-bridge" :modify-problem 'from-probset :modified-probset-prefix "pddl-test-bridge"
               :number-repetitions 10 :sayphi-algorithm 'ff :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 200))

(defun summarize nil
  ;; first, generate two files: one with stochastic-p=t version and the other one with stochastic-p=nil
  ;; depots
;; (summary-results "pfile22-600-nil.lisp" :domain "depots" :number-repetitions 1 :number-modifications 9 :p 0.2 :r 0.2 :accumulated-p nil :output-file (concatenate 'string *domains-dir* "depots/result/errt-results-nil.dat") :plot-planner-p nil)
;; (summary-results "pfile22-600-t.lisp" :domain "depots" :number-repetitions 1 :number-modifications 9 :accumulated-p nil :x-labels '("0" "mod0-1" "mod1-1" "mod2-1" "mod0-5" "mod1-5" "mod2-5" "mod0-10" "mod1-10" "mod2-10") :second-results '(("errt-results-nil.dat" "DERRTPlan")))

  ;; satellite
;;   (summary-results "pfile20-600-nil.lisp" :domain "satellite" :number-repetitions 1 :number-modifications 9 :p 0.2 :r 0.2 :accumulated-p nil :output-file (concatenate 'string *domains-dir* "satellite/result/errt-results-nil.dat") :plot-planner-p nil)
;;   (summary-results "pfile20-600-t.lisp" :domain "satellite" :number-repetitions 1 :number-modifications 9 :accumulated-p nil :x-labels '("0" "mod0-1" "mod1-1" "mod2-1" "mod0-5" "mod1-5" "mod2-5" "mod0-10" "mod1-10" "mod2-10") :second-results '(("errt-results-nil.dat" "DERRTPlan")))

  ;; rovers
;;   (summary-results "pfile20-600-nil.lisp" :domain "rovers" :number-repetitions 1 :number-modifications 3 :p 0.2 :r 0.2 :accumulated-p nil :output-file (concatenate 'string *domains-dir* "rovers/result/errt-results-nil.dat") :plot-planner-p nil)
;;   (summary-results "pfile20-600-t.lisp" :domain "rovers" :number-repetitions 1 :number-modifications 3 :accumulated-p nil :x-labels '("0" "mod0-1" "mod1-1" "mod2-1") :second-results '(("errt-results-nil.dat" "DERRTPlan")))

  ;; parking
;;   (summary-results "pfile08-31-600-nil.lisp" :domain "parking2011" :number-repetitions 1 :number-modifications 9 :p 0.2 :r 0.2 :accumulated-p nil :output-file (concatenate 'string *domains-dir* "parking2011/result/errt-results-nil.dat") :plot-planner-p nil)
;;   (summary-results "pfile08-31-600-t.lisp" :domain "parking2011" :number-repetitions 1 :number-modifications 9 :accumulated-p nil
;; 		   :x-labels '("0" "mod0-1" "mod1-1" "mod2-1" "mod0-5" "mod1-5" "mod2-5" "mod0-10" "mod1-10" "mod2-10") :second-results '(("errt-results-nil.dat" "DERRTPlan")))
  ;; do not run one after the other, since it will rewrite the previous results
;;   (summary-results "pfile08-32-600-nil.lisp" :domain "parking2011" :number-repetitions 1 :number-modifications 9 :p 0.2 :r 0.2 :accumulated-p nil :output-file (concatenate 'string *domains-dir* "parking2011/result/errt-results-nil.dat") :plot-planner-p nil)
;;   (summary-results "pfile08-32-600-t.lisp" :domain "parking2011" :number-repetitions 1 :number-modifications 9 :accumulated-p nil
;; 		   :x-labels '("0" "mod0-1" "mod1-1" "mod2-1" "mod0-5" "mod1-5" "mod2-5" "mod0-10" "mod1-10" "mod2-10") :second-results '(("errt-results-nil.dat" "DERRTPlan")))

  ;; floortile
    (summary-results "seq-p01-001-600-nil.lisp" :domain "floortile" :number-repetitions 1 :number-modifications 6 :p 0.2 :r 0.2 :accumulated-p nil :output-file (concatenate 'string *domains-dir* "floortile/result/errt-results-nil.dat") :plot-planner-p nil)
  (summary-results "seq-p01-001-600-t.lisp" :domain "floortile" :number-repetitions 1 :number-modifications 6 :accumulated-p nil
		   :x-labels '("0" "mod0-1" "mod1-1" "mod2-1" "mod0-2" "mod1-2" "mod2-2") :second-results '(("errt-results-nil.dat" "DERRTPlan")))

  )

;; For ERRTPlan experiments
;; (load-sayphi)
;; (load-learner 'errt)

;; I think I only need the definition of give-me-nice-state/goals for heuristic tests...
;; (load-learner 'ebl)
;; (load-learner 'ddh)

#|
;; ICAPS08
;; (defun test-cup nil
;;   (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-cup" :modify-problem 'from-probset :modified-probset-prefix "pddl-test-cup-"
;;                :number-repetitions 3 :sayphi-algorithm 'a-star :p (list 0.0 0.3 0.6) :r (list 0.0 0.3 0.6) :say-timeout 600))

;; ICAPS08  
;;   (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-l" :modify-problem 'from-probset :modified-probset-prefix "rev-pddl-new-test-l"
;; 	       :number-repetitions 3 :sayphi-algorithm 'ff :p (list 0.3 0.7 1.0) :r (list 0.3 0.7 1.0) :say-timeout 300))
;;   (errt-runset :domain "test-grid" :domain-file "domain-key.pddl" :prefix "pddl-train-l" :modify-problem 'from-probset :modified-probset-prefix "rev-pddl-new-test-l"
;;                :number-repetitions 3 :sayphi-algorithm 'a-star  :p (list 0.0 0.3 0.6) :r (list 0.0 0.3 0.6) :say-timeout 600))

(defun test-blocks nil
  (errt-runset :domain "blocksworld" :domain-file "blocksworld.pddl" :prefix "probBLOCKS" :number-repetitions 3 :p (list 0.0 0.3 0.6) :r (list 0.0 0.3 0.6) :say-timeout 400))

;; ICAPS08
;; (defun test-driverlog nil
;;   (errt-runset :domain "driverlog" :domain-file "driverlog.pddl" :prefix "pfile" :number-repetitions 3 :say-timeout 60))

(defun test-complex nil
  (errt-runset :domain "driverlog" :domain-file "driverlog.pddl" :prefix "pfile20" :number-repetitions 3 :say-timeout 1800))

;; ICAPS'08
;; (defun test-depots nil
;;   (errt-runset :domain "depots" :domain-file "Depots.pddl" :prefix "pfile" :number-repetitions 3 :say-timeout 400))

(defun test-rovers nil
  (errt-runset :domain "rovers" :domain-file "StripsRover.pddl" :prefix "pfile" :number-repetitions 3 :say-timeout 400))

(defun test-freecell nil
  (errt-runset :domain "freecell" :domain-file "freecell.pddl" :prefix "new-probfreecell" :number-repetitions 3 :say-timeout 400))

(defun test-satellite nil
  (errt-runset :domain "satellite" :domain-file "satellite.pddl" :prefix "pfile" :number-repetitions 3 :say-timeout 400 :p (list 0.0 0.3 0.6) :r (list 0.0 0.3 0.6)))

(defun test-zenotravel nil
  (errt-runset :domain "zenotravel" :domain-file "ZenoStrips.pddl" :prefix "pfile" :number-repetitions 3 :say-timeout 400 :p (list 0.0 0.3 0.6) :r (list 0.0 0.3 0.6)))

(defun test-stability nil
  (errt-runset :domain "driverlog" :domain-file "driverlog.pddl" :prefix "pfile20" :modify-problem 'from-probset :modified-probset-prefix "pfile-I" :say-timeout 600
	       :p (list 0.3 0.6) :r (list 0.3 0.6) :number-repetitions 3
	       :initial-solutions (list "/Users/dborrajo/planning/sayphi/domains/driverlog/probsets/solution.lisp")))

(defun test-stability-2 nil
  (errt-runset :domain "driverlog" :domain-file "driverlog.pddl" :prefix "pfile20" :modify-problem 'from-probset :modified-probset-prefix "pfile-I1" :say-timeout 600
	       :initial-solutions (list "/Users/dborrajo/planning/sayphi/domains/driverlog/probsets/solution.lisp")))

(defun test-plateaux (&optional (problems-list (list "blocks05" "blocks06" "blocks07" "blocks08" "blocks09" "blocks10" "blocks11" "blocks12" "blocks13" "blocks14")))
  (do ((problems problems-list (cdr problems)))
      ((null (cdr problems)))
    (errt-runset :domain "blocksworld" :domain-file "domain.pddl" :prefix (car problems) :number-repetitions 10 :sayphi-algorithm 'ff
		 :p (list 0.0 0.3 0.7 1.0) :r (list 0.0 0.3 0.7 1.0) :say-timeout 600 :max-modifications 1
		 :modify-problem 'from-probset :modified-probset-prefix (cadr problems))))
|#