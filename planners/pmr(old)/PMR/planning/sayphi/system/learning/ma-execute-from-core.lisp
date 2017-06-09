
;;It is not used in the competition CoDMAP. The ma-pddl includes this information
;;It returns 3 list: agent-types internal-predicates internal-types per domain      
(defun agentificate-domain (dom)
  (cond 
    ((eq 'zenotravel dom) (list '(aircraft) '(at-airplane fuel-level in penalty-aircraft-at penalty-aircraft-at-airplane) '()))
    ((eq 'zeno-travel dom) (list '(aircraft) nil '()))
    ((eq 'driverlog dom)  (list '(driver) '(at-driver driving) '())) 
    ((eq 'transport dom)  (list '(vehicle) '(at-truck in capacity penalty-vehicle-at) '()))
    ((eq 'ma-blocksworld dom) (list '(robot) '(handempty holding) '()))
    ((eq 'gripper dom) (list '(gripper) '(free at-robby carry) '()))
    ((eq 'satellite dom) (list '(satellite)
        '(supports calibration_target on_board pointing power_avail calibrated power_on penalty-satellite-pointing penalty-satellite-have_image)
        '(instrument)))
    ((or (eq 'rover dom) (eq 'rovers dom)) (list '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty
						have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board
						calibration_target penalty-rover-communicated_soil_data penalty-rover-communicated_rock_data penalty-rover-communicated_image_data)
		    '(camera store) ))
;;    '(hoist) '(lifting assigned available on-ship) '(ship) 
    ((eq 'port dom) (list '(hoist) '(lifting assigned available on-ship at-ship) '(ship)))
    ((eq 'elevators dom) (list '(slow-elevator fast-elevator elevator) '(boarded lift-at reachable-floor passengers can-hold) '()))
    ((eq 'elevators-fast dom) (list '(fast-elevator) '(boarded-fast lift-at-fast reachable-floor-fast passengers-fast can-hold-fast) nil))
    ((eq 'floortile dom) (list '(robot) '(robot-at robot-has free-color) '()))
    ((eq 'logistics dom) (list '(truck airplane vehicle) '(inside at) nil))
    ((eq 'logistics-air dom) (list '(airplane) '(inside-airplane at-airplane) nil))
    ;;Agentificacion Valencia: agentes truck y depot y  todo privado exepto posicion paquetes y trucks, no tiene sentido.
    ;;Dejo agentes de ellos y pongo privado lo relativo a esos agentes y a los hoist. Da problemas
    ;;((or (eq 'depots dom) (eq 'depot dom)) (list '(truck depot) '(in at-truck at-hoist available) nil))    
    ;;Nuestra agentificacion
    ((or (eq 'depots dom) (eq 'depot dom)) (list '(truck) '(in at-truck) nil))
     ;;Otra agentificacion: hoist pero solo para cmap
;;    ((or (eq 'depots dom) (eq 'depot dom)) (list '(hoist) '(at-hoist lifting available) nil))
    
    ((or (eq 'sokoban-sequential dom) (eq 'sokoban dom)) (list '(player) '(at-player) nil))
   ;; ((eq 'logistics dom) (list '(truck airplane) '(in) nil))
    ((eq 'woodworking dom) (list '(machine highspeed-saw glazer grinder immersion-varnisher planer saw spray-varnisher) '(empty has-colour in-highspeed-saw)))
    ((eq 'depots-robots dom) (list '(robot) '(at-robot carries free) nil))
    ((eq 'boxes dom) (list '(robot) nil nil))

    ;;Public domain without private predicates and types
    ((eq 'zenotravel-pub dom) (list '(aircraft) nil nil))
    ((eq 'driverlog-pub dom)  (list '(driver) nil nil))
    ((eq 'transport-pub dom)  (list '(vehicle) nil nil))
    ((eq 'satellite-pub dom) (list '(satellite) nil nil))
    ((eq 'rover-pub dom) (list '(rover) nil nil))
    ((eq 'port-pub dom) (list '(hoist) nil nil))
    ((eq 'elevators-fast-pub dom) (list '(fast-elevator) nil nil))
    ((eq 'logistics-air-pub dom) (list '(airplane) nil nil))
    ((eq 'depots-pub dom) (list '(truck) nil nil))
    ((eq 'depots-robots-pub dom) (list '(robot) nil nil))

    (t (format t "~%Please, add agentification information in fuction agentificate-domain for domain ~a ~%" dom ))
     
  ))

;;DEPRECATE
;;Mejor hacer la llamada directamente a la funcion execute-map
(defmacro maprun (&rest body)
  `(prog1
    (multiple-value-bind (result condition) (ignore-errors ,@body)
      (cond ((not (null condition))
	     (format t "~%[MAP]:Exception >> ~a~%" condition))
	    (t result)))
      (quit)
     ))


;;DEPRECATE
;;Mejor hacer la llamada directamente a la funcion execute-map
;;Los ultimos parametros de execute-map NO ESTAN contemplados en esta funcion
;;Usage: ./map-core --eval '(maprun (map-from-exe))' $DOMAIN $PROBLEM $OUTPUT $GOAL $REPLANNING $TIME $CENTRALIZED $MAPR $CMAP $SORT
;;El algorithm es siempre 'lama-first, no se pasa en el script
(defun map-from-exe ()
  (let* ((domain-file  (nth 1 *posix-argv*))
	(problem  (nth 2 *posix-argv*))
	(output   (nth 3 *posix-argv*))
	;;(domain-name nth 4 *posix-argv*))
	(goal-selection-s (nth 4 *posix-argv*))
	(replanning-algorithm-s (nth 5 *posix-argv*))
	(time-s     (nth 6 *posix-argv*))
	(time (if (and (stringp time-s) (not (string-equal (string-downcase time-s) "nil")))  (parse-integer time-s)))
	(centralized-s (nth 7 *posix-argv*))
	(centralized (if (stringp centralized-s) (string-equal (string-downcase centralized-s) "t")) )
	(mapr-s (nth 8 *posix-argv*))
	(mapr (if (stringp mapr-s) (string-equal (string-downcase mapr-s) "t")) )
	(cmap-s (nth 9 *posix-argv*))
	(cmap (if (stringp cmap-s) (string-equal (string-downcase cmap-s) "t")) )
	(sort-agents-s (nth 10 *posix-argv*)) 
	(errorp nil)
	 goal-selection replanning-algorithm  sort-agents 
	)
 
;;      (format t "~%Map from exe: 1=~a 2=~a 3=~a 4=~a 5=~a 6=~a 7=~a 8=~a 9=~a 10=~a~%" (nth 1 *posix-argv*) (nth 2 *posix-argv*) (nth 3 *posix-argv*) (nth 4 *posix-argv*) (nth 5 *posix-argv*) (nth 6 *posix-argv*) (nth 7 *posix-argv*) (nth 8 *posix-argv*) (nth 9 *posix-argv*) (nth 10 *posix-argv*))
    
    (cond
      ((string-equal goal-selection-s "all-achievable")   (setf goal-selection 'all-achievable))
      ((string-equal goal-selection-s "rest-achievable") (setf goal-selection 'rest-achievable))
      ((string-equal goal-selection-s "load-balance")    (setf goal-selection 'load-balance))
      ((string-equal goal-selection-s "best-cost")       (setf goal-selection 'best-cost))
      ((string-equal goal-selection-s "subsets")       (setf goal-selection 'subsets))
      ((string-equal goal-selection-s "contract-net")       (setf goal-selection 'contract-net))
      ((string-equal goal-selection-s "all")       (setf goal-selection 'all))
      (t (setf errorp t)
	 (format t "~%Check GOAL-SELECTION param in plan script: value ~a not allowed. Allowed values are all-achievable rest-achievable load-balance best-cost subsets contract-net: ~%" goal-selection-s))
      )

    (cond
      ((string-equal replanning-algorithm-s "lama")        (setf replanning-algorithm 'lama-unit-cost))
      ((string-equal replanning-algorithm-s "lama-unit-cost")        (setf replanning-algorithm 'lama-unit-cost))
      ((string-equal replanning-algorithm-s "lama-first")  (setf replanning-algorithm 'lama-first))
      ((string-equal replanning-algorithm-s "lama-seq")    (setf replanning-algorithm 'lama-seq))
      ((string-equal replanning-algorithm-s "lama-opt")    (setf replanning-algorithm 'lama-opt))
      ((string-equal replanning-algorithm-s "cgamer")      (setf replanning-algorithm 'cgamer))
      ((string-equal replanning-algorithm-s "lpg-adapt")   (setf replanning-algorithm 'lpg-adapt))
      ((string-equal replanning-algorithm-s "errtplan")    (setf replanning-algorithm 'errtplan))
      )

    (cond
      ((string-equal sort-agents-s "name")       (setf sort-agents 'name))
      ((string-equal sort-agents-s "random")     (setf sort-agents 'random))
      ((string-equal sort-agents-s "maxgoals")   (setf sort-agents 'maxgoals))
      ((string-equal sort-agents-s "mingoals")   (setf sort-agents 'mingoals))
      )

    
     (unless errorp (execute-map (truename domain-file)  (truename problem) output  
				  :goal-selection goal-selection :sort-agents sort-agents
				  :timeout time :algorithm 'lama-unit-cost :replanning-algorithm replanning-algorithm
				  :run-original-centralized-p centralized :run-cmap-p cmap :run-mapr-p mapr
				  ;;SFA: prueba paralelizacion
				  :parallel-plan 'public
				  ))
  )) 


;; Para evitar funcion map-from-exe
;; Usage: ./map-core --eval "(progn (execute-map \"$DOMAIN\" \"$PROBLEM\" \"$OUTPUT\" :goal-selection '$GOAL :timeout $TIME :algorithm '$ALGORITHM :replanning-algorithm '$REPLANNING :run-original-centralized-p $CENTRALIZED :run-cmap-p $MAPR :run-mapr-p $CMAP :sort-agents '$SORT) (quit))"

;;La funcion run-ma necesita como parametro el nombre del dominio como secuencia. Para evitar tener que pasar un parametro mas de ejecucion,
;;se lee el nombre del dominio del fichero PDDL, el problema es que lo captura como un symbol y al pasarlo a string lo pone siempre en mayusculas.
;;Se usa la funcion string-downcase para pasarlo a minusculas. Por eso,
;;los DIRECTORIOS DONDE ESTAN LOS DOMINIOS TIENE QUE ESTAR CON TODAS LAS LETRAS EN MINUSCULAS. 
;;Por otro problema con la implementacion, los ficheros de problemas PDDL tienen que estar en el mismo directorio donde esta el fichero de dominio
;;o en un subdirectorio 
;;
;; Guarda los dos planes, el plan paralelo en el fichero output y el plan secuencial en el fichero output.seq
;; El parametro parallel-plan puede valer nil, si no se quiere el plan paralelo, private si se quiere calcular el plan paraledo preservando privacidad
;; or public si se calcula el plan paralelo sin privacidad. El plan paralelo se guarda en el fichero output.pp
;; La funcion run-ma admite como parametro parallel-plan-p en caso de ser t calcula el plan paralelo para almacenar en la salida su makespan
;; Como execute-map almacena tb. el plan paralelo mejor lo controlamos con este parametro y calculamos el plan paralelo en la funcion execute-map.
;; La llamada a run-ma la haremos siempre con parallel-plan-p a nil para que no se haga la llamada al plan paralelo dos veces
;; Cuando statisticals es true guarda un fichero con algunos datos de los problemas: numero de agentes usados, metas publicas y el total de metas del problema

;;(execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/driverlog/ma-driverlog.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/driverlog/probsets/pagb-00.pddl" "kk.soln" :run-mapr-p t)
;;(execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/rover/domain.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/rover/probsets/ipc3-pfile07" "kk.soln" :run-mapr-p t :parallel-plan 'public)
;; (execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/driverlog/ma-driverlog.pddl"
;;"/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/driverlog/probsets/ag-ma-agb01-001.pddl" "kk-d" :run-mapr-p t :replanning-algorithm 'lpg-adapt)
;;(execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/Boxes/domain-ma-ja.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/Boxes/probsets/p01-ma-ja.pddl" "kk" :run-cmap-p t)

;; (run-ma-experiments "rover" "ipc3-pfile*" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty
;;                                have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board
;;                                calibration_target)
;;                '(camera store) :domain-file "domain.pddl"
;;                :goal-selections '(all-achievable)
;;                :run-original-centralized-p nil :run-mapr-p t :run-cmap-p nil :solve-for-merging-p nil :use-macros-p t
;;                :algorithms '(lama-unit-cost) :replanning-algorithms '(lama-unit-cost) :timeout 600) 

;; (run-ma-experiments "rover" "ipc3-pfile06" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty
;;  						have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board
;;  						calibration_target)
;;  		    '(camera store) :domain-file "domain.pddl" :goal-selections '(rest-achievable )
;;  		    :run-original-centralized-p nil :run-mapr-p t :run-cmap-p nil
;;  		    :algorithms '(lama-unit-cost) :replanning-algorithms '(lama-unit-cost) :timeout 400 :anytime-p t)
;; 

;; (run-ma-experiments "rover" "ipc3-pfile04" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty
;;  						have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board
;;  						calibration_target)
;;  		    '(camera store) :domain-file "domain.pddl" :goal-selections '(rest-achievable ) :sort-agents '(mingoals)
;;  		    :run-original-centralized-p nil :run-mapr-p t :run-cmap-p nil
;;  		    :algorithms '(lama-first) :replanning-algorithms '(lama-first) :timeout 400 :anytime-p t)
;; 

;; (run-ma-experiments "logistics" "ma-Pfile1" '(truck airplane vehicle) '(inside at) nil
;;                  :domain-file "ma-logistics.pddl" :goal-selections '(subsets ) 
;;  		    :run-original-centralized-p nil :run-mapr-p nil :run-cmap-p t
;;  		    :algorithms '(lama-first) :replanning-algorithms '(lama-first) :timeout 400 :anytime-p t)
;;

;;(run-ma-experiments "codmap-logistics" "prob*.pddl" nil nil nil :ma-pddl-p t
;;                  :domain-file "domain.pddl" :goal-selections '(subsets) 
;;  		    :run-original-centralized-p nil :run-mapr-p nil :run-cmap-p t
;;  		    :algorithms '(lama-first) :replanning-algorithms '(lama-first) :timeout 400 :anytime-p nil)
;; 



;; (execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/rover/domain.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/rover/probsets/ipc3-pfile20" "rover-20.plan" :goal-selection 'load-balance  :algorithm 'lama-unit-cost :replanning-algorithm 'lama-unit-cost :run-mapr-p t :sort-agents 'mingoals :use-macros-p t )

;; (execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/depots/ma-depots.pddl"
;; "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/depots/probsets/pfile04-ma.pddl" "kk.plan" :goal-selection 'all  :algorithm 'adp
;; :replanning-algorithm 'lama-unit-cost :run-cmap-p t)

;; (execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/rover/domain.pddl"
;; "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/rover/probsets/ipc3-pfile06" "rover-06.plan" :goal-selection 'rest-achievable  :algorithm
;; 'lama-unit-cost :replanning-algorithm 'lama-unit-cost :run-mapr-p t :sort-agents 'name :anytime-p t  )

;; (run-ma-experiments "rover" "ipc3-pfile05" '(rover) '(at can_traverse equipped_for_soil_analysis equipped_for_rock_analysis equipped_for_imaging empty
;;   						have_rock_analysis have_soil_analysis full calibrated supports available have_image store_of on_board
;;   						calibration_target)
;;   		    '(camera store) :domain-file "domain.pddl" :goal-selections '(rest-achievable ) :sort-agents '(mingoals)
;;   		    :run-original-centralized-p nil :run-mapr-p t :run-cmap-p nil
;;   		    :algorithms '(lama-first) :replanning-algorithms '(lama-unit-cost) :timeout 400 :anytime-p t)

;; (execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/elevatorsuc-fast/domain-uc.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/elevatorsuc-fast/probsets/p12.pddl" "kk-ele12.plan" :run-mapr-p t :sort-agents 'mingoals :parallel-planning-p nil :anytime-p t :goal-selection 'best-cost)

;; (setf ag (agentificate-domain 'zenotravel))
;; (run-ma-experiments "zenotravel" "pfile10" (car ag) nil nil :ma-pddl-p t :goal-selections '(rest-achievable ) :sort-agents '(mingoals)
;;    		    :run-original-centralized-p nil :run-mapr-p t :run-cmap-p nil
;;    		    :algorithms '(lama-first) :replanning-algorithms '(lama-unit-cost) :timeout 400 :anytime-p nil)
;;
;;para que funcione sin tipos hay que quitar on-ship de los predicados privados
;; (run-ma-experiments "port" "pfile-02-5.pddl" '(hoist) '(lifting assigned available at-ship) nil
;; (run-ma-experiments "port" "pfile-02-5.pddl" (car ag) (second ag) nil :ma-pddl-p t :goal-selections '(rest-achievable ) :sort-agents '(mingoals)
;;     		    :run-original-centralized-p nil :run-mapr-p t :run-cmap-p nil
;;     		    :algorithms '(lama-unit-cost) :replanning-algorithms '(lama-unit-cost) :timeout 400 :anytime-p nil)


;;(execute-map "planning/saycodmap-logistics" "prob*.pddl" nil nil nil :ma-pddl-p t
;;                  :domain-file "domain.pddl" :goal-selections '(subsets) 
;;  		    :run-original-centralized-p nil :run-mapr-p nil :run-cmap-p t
;;  		    :algorithms '(lama-first) :replanning-algorithms '(lama-first) :timeout 400 :anytime-p nil)
;; 


;; (execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/codmap-logistics/domain.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/codmap-logistics/probsets/probLOGISTICS-10-0.pddl" "log-10.plan" :run-mapr-p t :parallel-planning-p nil :ma-pddl-p t :algorithm 'lama-unit-cost :replanning-algorithm 'lama-unit-cost :use-macros-p t :only-one-macro-p t)

;;(execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/codmap-elevators/domain.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/codmap-elevators/probsets/p04.pddl" "kk" :run-mapr-p t :parallel-planning-p nil  :ma-pddl-p t :algorithm 'lama-unit-cost :replanning-algorithm 'lama-unit-cost :use-macros-p t :only-one-macro-p t :goal-selection 'subgoals)

;;(execute-map "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/codmap-depot/domain.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/codmap-depot/probsets/pfile06" "kk" :run-cmap-p t :parallel-planning-p nil :ma-pddl-p t :parallel-plan nil :anytime-p t :timeout 300 :goal-selection 'subsets)

(defun execute-map (domain problem output  &key (init-problems nil) (goal-selection 'rest-achievable) 
	       (timeout 300) (algorithm 'lama-first) (replanning-algorithm 'lama-first) (run-original-centralized-p nil) (run-cmap-p nil) (run-mapr-p nil)
               (validate-obfuscated-p nil) (validate-mapr-p nil) (sort-agents 'mingoals) (solve-for-merging-p nil) (parallel-plan 'public)
	       (parallel-planning-p t) (anytime-p nil) (anytime-algorithm 'lama-second) (use-macros-p nil) (only-one-macro-p nil)
	       (statisticals nil) (ma-pddl-p nil))

  (let* ((solution nil) (obf-solution nil) 
	(domain-dir (directory-namestring domain))
        (domain-file (file-namestring domain))
	(problem-dir (directory-namestring problem))
	(problem-file (file-namestring problem))
	(domain-def (cdr (read-all-file domain)))
	(name (car (find-argument domain-def 'domain)))
	(dom-s (string-downcase (format nil "~a" name))) ;;run-ma necesita el nombre del dominio como sequencia
	(agent-info (unless ma-pddl-p (agentificate-domain name)))
	(agent-types (car agent-info))
	(internal-predicates (second agent-info))
	(internal-types (third agent-info))
	(init-time (get-internal-real-time))
        (unserialized-init-problems-stream nil)
        (unserialized-init-problems nil)
	(pp nil) ;parallel-plan 
	)
    
    ;;Hay que redefinir las variables globales referentes a los planificadores externos
    (setf *my-path* (namestring *default-pathname-defaults*))
    (setf *my-tmp-path* (concatenate 'string *my-path* "tmp/"))
    (setf *my-planning-path* (concatenate 'string *my-path* "planning/"))
    (setf *lpg-adapt-script* (concatenate 'string *my-planning-path* "LPG-adapt/run-lpg-adapt.sh "))
    (setf *lama-first-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-first.sh "))
    (setf *lama-unit-cost-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-unit-cost.sh "))
    ;;(setf *lama-first-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-unit-cost.sh "))
    (setf *lama-seq-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-seq.sh "))
    (setf *lama-opt-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-opt.sh "))
    (setf *cgamer-script* (concatenate 'string "cd " *my-planning-path* "cgamer; ./run-cgamer.sh "))
    (setf *lama-second-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-second.sh "))
    (setf *ma-fd-script* (concatenate 'string "cd " *my-planning-path* "ma-fd/src; ./run-ma-fd.sh ")) ;;falta el codigo de ma-fd
    (setf *adp-script* (concatenate 'string "cd " *my-planning-path* "adp; ./run-adp.sh "))

    
    (setf *solution-mapr* nil)
    (setf *solution-cmap* nil)
    (setf *solution-centralized* nil)
    (setf *merged-domain* "merged-obfuscated-domain.pddl")
    (setf *merged-problem* "merged-obfuscated-problem.pddl")
    (setf *obfuscated-domain*  "final-obfuscated-domain.pddl")
    (setf *obfuscated-problem* "final-obfuscated-problem.pddl")

    (if init-problems
      (with-open-file (unserialized-init-problems-stream init-problems :direction :INPUT :if-does-not-exist :ERROR)
        (setq unserialized-init-problems (read unserialized-init-problems-stream))
      )
    )


;;   (format t "~% run-ma dom-s:~a problem-file:~a problem-dir:~a domain-dir:~a~%" dom-s problem-file problem-dir domain-dir)
    (setq obf-solution
;;      (run-ma dom-s problem-file agent-types internal-predicates internal-types :probsets-dir (concatenate 'string problem-dir "/")
      (run-ma dom-s problem-file agent-types internal-predicates internal-types 
	    :domain-dir domain-dir :probsets-dir problem-dir :domain-file domain-file
	    ;;SFA: con los ultimos cambios se ha quitado este parametro. Preguntar a Jesus como le afecta
;;            :init-problems unserialized-init-problems. Hay un :init-agents que no se si sera equivalente
	    :goal-selection goal-selection :timeout timeout :sort-agents sort-agents
	    :algorithm algorithm :replanning-algorithm replanning-algorithm
	    :run-original-centralized-p run-original-centralized-p :run-cmap-p run-cmap-p :run-mapr-p run-mapr-p
	    :validate-obfuscated-p validate-obfuscated-p :validate-mapr-p validate-mapr-p
	    :solve-for-merging-p solve-for-merging-p :output-file (if solve-for-merging-p output nil)
;;            :obfuscated-domain *obfuscated-domain* :obfuscated-problem *obfuscated-problem*
	    :parallel-plan-p nil ;;ponerlo siempre a nil para no calcular el plan paralelo dos veces
	    
	    :parallel-planning-p parallel-planning-p
	    :anytime-p anytime-p :anytime-algorithm anytime-algorithm
	    :use-macros-p use-macros-p
	    :only-one-macro-p (and use-macros-p only-one-macro-p)

	    :ma-pddl-p ma-pddl-p
	    :plan-file output

	    ))

;;    (format t "~% SOLUTION: ~a~%" (solution-path obf-solution))
    ;;Cuando solve-for-merging-p calcula el plan paralelo y lo salva en output.pp. El plan secuencial en formato ipc-num se guarda en output
    ;;obf-solution es de tipo solution, es decir en solution-path esta el plan solucion en formato lista
    (if solve-for-merging-p
	(if (solution-found obf-solution)
	    (parallel-plan (concatenate 'string domain-dir *merged-domain*) (concatenate 'string problem-dir *merged-problem*)
			   (solution-path obf-solution) (concatenate 'string output ".pp") :format 'sol );;:ma-pddl-p ma-pddl-p)
	    ;;Si no esta ofuscado
;;	    (parallel-plan domain problem (solution-path obf-solution) (concatenate 'string output ".pp") :format 'sol)
	    ;;Si se lee directamente de la salida output la llamada es
;;	    (parallel-plan domain problem output (concatenate 'string output ".pp") :format 'ipc-num )
	    (format t "~% Solve-for-merging does not find solution~%")))
    
    (setq solution
      (cond (run-cmap-p (des-obfuscate *solution-cmap*))
	    (run-mapr-p *solution-mapr*)
 	    (run-original-centralized-p  *solution-centralized*)))

    ;;Si se calcula el plan paralelo solo se guarda la solucion con el plan paralelo
    (when solution
        (let ((domain-p  (if ma-pddl-p (concatenate 'string domain-dir "codmap-" domain-file) domain))
	      (problem-p (if ma-pddl-p
                              (if (pathname-type problem)
			           (concatenate 'string problem-dir "codmap-" problem-file)
				   (concatenate 'string problem-dir "codmap-" problem-file "."))
			     problem))
	      )
;;	  (format t "~% SFA: domain-p ~a problem-p ~a~%" domain-p problem-p)
      ;;(parallel-plan domain problem output (concatenate 'string output ".pp"))
       ;;Quiza lo mejor sea calcular el plan paralelo ofuscado pero antes de guardar el fichero  desofuscar
       (cond
	 ((or (eq parallel-plan 'public) (and run-original-centralized-p parallel-plan))
	      (parallel-plan domain-p problem-p solution output :format 'sol :ma-pddl-p ma-pddl-p))
	 ;;Solo funciona con cmap. Por las macros en mapr no se puede hacer de forma privada 
         ((and run-cmap-p (eq parallel-plan 'private))
	     (private-parallel-plan (concatenate 'string domain-dir *obfuscated-domain*) (concatenate 'string problem-dir *obfuscated-problem*)
		      *solution-cmap* output :format 'sol :ma-pddl-p ma-pddl-p :only-one-macro-p (and use-macros-p only-one-macro-p)))
	 (t (format t "~% No parallel plan calculation (~a) " parallel-plan)
	    (if ma-pddl-p (setq solution(pddl-to-ma-pddl-solution solution)))
	     ;;Guardo el plan secuencial 
             (with-open-file (stream output :direction :output :if-does-not-exist :create :if-exists :supersede)
	         (format stream ";;~a, " (if run-cmap-p 'cmap (if run-mapr-p 'mapr 'centralized)))
                 (format stream "~a, ~a, ~a, ~a, ~a~%" timeout goal-selection sort-agents algorithm replanning-algorithm )
	         (format stream ";;Runtime:~,2f length:~a~%" (elapsed-time init-time 'real-time) (length solution))
	         (format stream ";;Total-time:~,2f, assignment-time:~,2f, planning-agents:~d, public-goals:~d~%"
		       *total-time* *goals-assignment-time* (length *agents*) (length *public-goals*))
                 (mapcar #'(lambda (x) (format stream "~a~%" x)) solution))
             (format t "~% SEQUENTIAL SOLUTION FOUND!. See file ~a~%" output))
       )))


;;Se guarda siempre el plan sequencial y el plan paralelo con extension .pp
;;         (when solution
;;        ;;Guardo el plan secuencial 
;;        (with-open-file (stream output :direction :output :if-does-not-exist :create :if-exists :supersede)
;; 	  (format stream ";;~a, " (if run-cmap-p 'cmap (if run-mapr-p 'mapr 'centralized)))
;;             (format stream "~a, ~a, ~a, ~a, ~a~%" timeout goal-selection sort-agents algorithm replanning-algorithm )
;; 	  (format stream ";;Runtime:~,2f length:~a~%" (elapsed-time init-time 'real-time) (length solution))
;; 	  (format stream ";;Total-time:~,2f, assignment-time:~,2f, planning-agents:~d, public-goals:~d~%"
;; 		  *total-time* *goals-assignment-time* (length *agents*) (length *public-goals*))
;;           (mapcar #'(lambda (x) (format stream "~a~%" x)) solution))
;;        (format t "~% SEQUENTIAL SOLUTION FOUND!. See file ~a~%" output)
;;         ;;Guardamos tambien el plan paralelo
;;        ;;(parallel-plan domain problem output (concatenate 'string output ".pp"))
;;        ;;OJO. Quiza lo mejor sea calcular el plan paralelo ofuscado pero antes de guardar el fichero .pp desofuscar
;;        (cond
;; 	 ((or (eq parallel-plan 'public) (and run-original-centralized-p parallel-plan))
;; 	     (parallel-plan domain problem solution (concatenate 'string output ".pp") :format 'sol))
;;          ((eq parallel-plan 'private)
;; 	     (private-parallel-plan (concatenate 'string domain-dir *obfuscated-domain*) (concatenate 'string problem-dir *obfuscated-problem*)
;; 		      obf-solution (concatenate 'string output ".pp") :format 'sol))
;; 	 (t (format t "~% No parallel plan calculation (~a) " parallel-plan))
;;        ))

    ;;Cuando no encuentra la solucion, guardamos un fichero con las estadisticas
    ;;Con el codigo de la IPC muchas veces no le da tiempo a llegar aqui porque lo mata antes, hay que ejecutarlo manualmente
    ;;Lo quito porque el codigo de la IPC a veces da problemas al validar los planes
   (when statisticals
      (let* ((problem-def (read-all-file problem))
	     (goals (car (find-argument (cdr problem-def) ':goal)))
	     )
       (with-open-file (stream (concatenate 'string output "-sta")  :direction :output :if-does-not-exist :create :if-exists :supersede)
	    (format stream ";;~a, " (if run-cmap-p 'cmap (if run-mapr-p 'mapr 'centralized)))
                (format stream "~a, ~a, ~a, ~a, ~a~%" timeout goal-selection sort-agents algorithm replanning-algorithm)
	    (format stream ";;Domain=~a, problem=~a~%"	dom-s problem-file)
	    (format stream ";;Total-time:~,2f, assignment-time:~,2f~%"
		  *total-time* *goals-assignment-time* )
	    (format stream ";;All-goals:~d, Public-goals:~d~%"
		   (1- (length goals)) (length *public-goals*) )
	    (format stream ";;Planning-agents:~d~%"
		  (length *agents*)) 
        )))
    ))

(defun write-solution-in-output-file (nfile solution-i timeout goal-selection replanning-algorithm run-cmap-p run-mapr-p sort-agents par-time
				      &key (ma-pddl-p nil))
  (let* ((solution (if ma-pddl-p (pddl-to-ma-pddl-solution solution-i) solution-i))
	 (makespan (+ 1 (caar (last solution))))
        (len (length solution))
	)
    ;(format t "~%Makespan: ~a, longitud del plan ~a ~%" makespan len)
    (with-open-file (stream nfile
          :direction :output  :if-exists :supersede :if-does-not-exist :create)

      (format stream ";;~a, " (if run-cmap-p 'cmap (if run-mapr-p 'mapr)))
        (format stream "~a, ~a, ~a, ~a~%" timeout goal-selection replanning-algorithm sort-agents)
      (format stream ";;Makespan: ~a~%" makespan)
      (format stream ";;Plan length: ~a~%" len)
      (format stream ";;Parallelization time: ~,2f~%" par-time)
      (format stream ";;Total-time:~,2f, assignment-time:~,2f, planning-agents:~d, public-goals:~d~%"
	      *total-time* *goals-assignment-time* (length *agents*) (length *public-goals*))
      (dolist (act solution)
         (format stream "~3d: ~a~%" (car act) (cadr act)))
   )
   (if solution
     (format t " SOLUTION FOUND!. See file ~a~%" nfile)
     (format t " NOT solution found. See file ~a~%" nfile)
     )
  ))


(defun read-plan-from-file (solution-file)
  (let ((plan nil))
  (with-open-file (ifile solution-file :direction :input)
	(do* ((action (read ifile nil 'eof) (read ifile nil 'eof)))
	     ((eq action 'eof) plan)
	  (setf plan (append plan (list action)))))))


;;Convierte el plan secuencial guardado en solution-file en un plan paralelo y lo salva en output.
;;solution-file puede ser un fichero con el formato 'ipc-num, cada linea representa una accion del plan con el siguiente formato:
;; 0: (CALIBRATE ROVER0 CAMERA0 OBJECTIVE1 WAYPOINT3) [1]
;;un fichero como:
;; ;;Runtime:0.116 length:10
;; (CALIBRATE ROVER0 CAMERA0 OBJECTIVE1 WAYPOINT3)
;; (TAKE_IMAGE ROVER0 WAYPOINT3 OBJECTIVE1 CAMERA0 HIGH_RES)
;; o una solucion directamente  (si format 'sol
;;domain y problem son los ficheros pddl del dominio y problema con la ruta completa
;;(parallel-plan "/home/sfernandez/svn-nerea-all/domains/rover/domain.pddl" "/home/sfernandez/svn-nerea-all/domains/rover/ipc3-pfile07"
;;"/home/sfernandez/svn-nerea-all/mapr/rover07.soln.seq" "rover07.pp")
;; 
;; (parallel-plan "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/codmap-logistics/codmap-domain.pddl" "/home/sfernandez/sayphi-multiagent/planning/sayphi/domains/codmap-logistics/probsets/codmap-probLOGISTICS-10-0.pddl" "/home/sfernandez/sayphi-multiagent/log-10.plan" "log-10.plan.pp" :ma-pddl-p t)
(defun parallel-plan (domain problem solution-file output &key (format 'ipc) (savep t) (ma-pddl-p nil))
    (let* ((parallel-plan nil)
	   (init-time (get-internal-real-time))
	   (solution (case format (sol solution-file)
			   (ipc-num (solution-from-file solution-file))
			   (ipc (read-plan-from-file solution-file))))
;;	   (solution (if (eq format 'ipc-num) (solution-from-file solution-file) (read-plan-from-file solution-file)))
	   )

       ;;Para buscar el plan paralelo primero hay que cargar el dominio y el problema en sayphi
       (read-pddl-domain domain)
       (read-pddl-problem problem)

       (setq parallel-plan (build-parallel-plan solution))
       ;;In case the plan is obfuscated we need to desofuscated it
;;        (setq parallel-plan
;; 	     (mapcar #'(lambda(x) (cons (car x) (des-obfuscate (cdr x))))
;; 	             (build-parallel-plan solution)))

       (when savep
         ;;Hay que escribir la solucion en el outputfile
         (write-solution-in-output-file output  parallel-plan nil nil nil nil nil nil
				     (elapsed-time init-time 'real-time) :ma-pddl-p ma-pddl-p)
         (format t "~%Parallel-plan save in ~a~%" output)
	 )
       parallel-plan
      )
    )


(defun private-parallel-plan (domain problem solution-file output &key (format 'ipc) (ma-pddl-p nil) (only-one-macro-p nil))
  (let* ((init-time (get-internal-real-time))
	 (solution (if (and (eq format 'sol) only-one-macro-p) (des-obfuscate solution-file *agents* t) solution-file))
	 (pp (parallel-plan domain problem solution output :format format :savep nil))
	 (times (mapcar #'car pp))
	 (actions (mapcar #'second pp))
	 (despp (des-obfuscate (des-obfuscate actions)))
	 )


	 (write-solution-in-output-file output  (mapcar #'list times despp) nil nil nil nil nil nil
				     (elapsed-time init-time 'real-time) :ma-pddl-p ma-pddl-p)
	 ))
