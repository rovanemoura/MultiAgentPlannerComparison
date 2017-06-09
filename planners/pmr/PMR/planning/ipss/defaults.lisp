;;; This file contains all variable definitions for HAMLET.

;;; **********************************************************************
;;;   From Prodigy
;;; **********************************************************************

;; Svegas.??
;; (setf p4::*redirected-output* nil)

(in-package "COMMON-LISP-USER")

;; So that it does not ask whether to remove the domain when loading it again
(if (and (boundp '*planner-for-learning*) (eq *planner-for-learning* 'ipss)) (setf p4::*always-remove-p* t))


;;; **********************************************************************
;;;   From hamlet.lisp
;;; **********************************************************************

;;;
;;; The path to the code of hamlet
;;;
(defvar *hamlet-files*
    (list 'defaults 'macros 'hamlet
	  'load-problems 'induction
	  'quality 'probabilities 'utility
	  'problem-generator
	  'follow-ff-behavior
	  "footprint"
	  "preconds"
	  "print-rules"
	  "apply-footprint"
	  'contrib-daniel))

;;;
;;; The path to the code of IPSS
;;;
(defvar *ipss-files*
  (list	'defaults 'macros
	'hamlet	'induction 'generic-rules
	'load-problems 'contrib-daniel
	'footprint 'preconds 'print-rules 'apply-footprint
	'probabilities 'utility 'quality 'rete
	'problem-generator
	'generic-pddl 'follow-ff-behavior 'execute-ff
	'structures 'resource 'parallel	'applied
	'datatemp 'tm-tools 'tm-memo 'propagat 'bell-for
	'remove	'output
	'ipss2pddl 'pddl2ipss 'pdl2foil 'pdl2clips 'pdl2mtarff
	'ipss2path-planning
	'macro-operators))

;; In case we load this file from Sayphi
(defvar *world-path* *domains-dir*)
(defvar *domain-directory* (format nil "~alogistics/" *world-path*))

;; This is only for domains files (domain, function, control rules, probsets)
;; In case it is a system file (Prodigy or Hamlet) *source-extension* is used
(defvar *lisp-extension* "lisp")

;; (defvar *lisp-extension*
;;   #-CLISP "lisp"
;;   #+CLISP "lsp")

(defvar *hamlet-world-path* *world-path*)
(defvar *hamlet-domain-file-name* "domain.lisp")
(defvar *hamlet-probset-path* (concatenate 'string *domain-directory* "probsets/"))
(defvar *reset-problem-path* t)
(defvar *standard-paths-p* t)

(defvar *hamlet-output* (format nil "~ahamlet-output.~a"
				*domain-directory* *lisp-extension*)
  "Where the output of the learning method goes (not the rules)")

(defvar *hamlet-results-output* (format nil "~ahamlet-results.~a"
					*domain-directory* *lisp-extension*)
  "Where the results of the tests with Hamlet learned rules will go")

(defvar *prodigy-results-output* (format nil "~aprodigy-results.~a"
					 *domain-directory* *lisp-extension*)
  "Where the results of the tests of Prodigy alone will go")

(defvar *hamlet-output-format* 'lisp
  "The way in which results (especially for tests) will be saved. It can be spaces, lisp or csv. Spaces means data separated by spaces, Lisp means it can be read from Lisp and csv means it is separated by commas")

(defvar *ipss-results* nil)

(defvar *solutions-file* (concatenate 'string "solutions." *lisp-extension*))

;;; Esta variable se establece a 1 si el dominio que se trata de cargar
;;; es un dominio valido. Variable necesaria para la posterior gestion
;;; de errores
(defvar *valid-domain* 0)

;;; Esta variable se establece a 1 si el problema que se intenta ejecutar
;;; es un problema valido. Variable necesaria para la posterior gestion de
;;; de errores
(defvar *valid-problem* 0)

;;; Esta variable es un contador que determina el problema que esta siendo
;;; ejecutado
(defvar *problem-number* 0)

;;; Esta variable se emplea para determinar si la planificacion se ha realizado 
;;; satisfactoriamente y sin errores.
(defvar *valid-planner* 0)

;;; Esta variable se emplea para determinar si el aprendizaje de reglas se
;;; ha realizado satisfactoriamente y sin errores.
(defvar *valid-control-rules* 0)

;;; Variable que sirve para determinar si los rangos introducidos en el
;;; aprendizaje de macro-operadores son los correctos.
(defvar *macros-range* 0)

;;; Variable que se emplea para determinar si el aprendizaje de macros
;;; ha finalidado sin errores.
(defvar *valid-macros* 0)

(defvar *unsolved-problems* nil)

;;; If t, it will prefer more specific bindings when intersecting two
;;; rules
(defvar *more-specific-bindings-bias-p* t)


(defvar *learnp* t "self explained")
;;; If t, it will learn rules with the prior goal
(defvar *learn-prior-goal-p* nil)
(defvar *learn-apply-p* nil
  "Controls whether we want to learn decide apply rules or not. The default is nil")
(defvar *optimal-learning-p* nil
  "If t, it will only learn from optimal paths. If nil, if will learn from any successful path")
;;; If t, it will consider wrong to expand a tree with the rules in more
;;; time than time(without)+*max-time-difference*
(defvar *test-time-difference-p* t)
(defvar *max-time-difference* 50)
;;; If t, it will not expand the whole trees, but using :shorter
(defvar *fast-learning-p* t)

(defvar *use-preferred-preconds-p* t
  "If, it will separate the preconds relative to the regressed states of the some-candidate-goals of the node")

;;; Contains the last node expanded by the search
(defvar *last-node* nil)

;;; The top level type of the hierarchy
(defvar *top-type* nil)

(defvar *domain-name* nil
  "The name of the domain we are using")

(defvar *cr-file* (format nil "induced-rules.~a" *lisp-extension*)
  "The file where to save the rules")

;;; If t it will print all sort of things from the learning method.
(defvar *printp* nil)

;; If t, it will print in a separate file all solutions to problems.
(defvar *print-solutions-p* nil)

(defvar *result* nil "Contains the result of solving the problem")
(defvar *break-aprender-p* nil
  "T if we want to break before writing the rules on the output file")

(defvar *real-t-tree* nil "Pointer to the root of the search tree")
(defvar *old-tree* nil "Pointer to the root of the previous search tree")

;;; Rules vars
(defvar *rules* nil "The list of active rules")
(defvar *cr-list* nil "The list of new crs (instances of new-cr)")
(defvar *initial-cr* nil "Crs before learning")
(defvar *store-state-p* nil "Whether to store the state in the nodes of the best path of the search tree")

;;(defvar *initial-state* nil "self explained. list of visible forms")

(defvar *substitution* nil "For each cr, the substitution of var . cte")

;;; Used for control and output
(defvar *number-deduced-rules* 0 "self explained")
(defvar *number-induced-rules* 0 "self explained")
(defvar *number-refined-rules* 0 "self explained")
(defvar *number-deleted-rules* 0 "self explained")
(defvar *number-undeleted-rules* 0 "self explained")

(defvar *deleted-rule-p* nil "T if an old rule has been deleted")
(defvar *number-vars-in-cr* 0 "internal")
(defvar *rule-id* nil "internal")
(defvar *number-ops* 0)
(defvar *old-number-ops* 0)
;;(defvar *number-nodes* 0)
;;(defvar *number-all-nodes* 0)
;;(defvar *number-nodes-solution-path* 0)

;;(defvar *problem-file* (format nil "~aprobs/p1.lisp" *domain-directory*))
(defvar *problem-name* nil)
;; Main variable of the learning method that holds the distinguished
;; meta-predicates that can hold main vars. Note that
;; candidate-operator, and current-operator are not
;; here, since they cannot have vars on them.
(defvar *main-meta-predicates* '(current-goal common-preconds prior-goal
				 candidate-goal some-candidate-goals
				 subset-candidate-goals target-goal
				 applicable-op))

;;; not used any more
;; (defvar *problems* '(p1 p2 p3 p4 p5 p6 p7 p8 p10 p11 p12 p13 p14 p15
;; 			p16 p17 p18 p19 p20 p21 p22 p23 p24 p25))
(defvar *my-current-node* nil)

(when (and (boundp '*planner-for-learning*) (eq *planner-for-learning* 'ipss))
  (defvar *learned-rules-types*
    (list 'p4::problem-space-select-goals 'p4::problem-space-select-operators
	  'p4::problem-space-select-bindings
	  'p4::problem-space-reject-goals 'p4::problem-space-reject-operators
	  'p4::problem-space-reject-bindings
	  'p4::problem-space-prefer-goals 'p4::problem-space-prefer-operators
	  'p4::problem-space-prefer-bindings
	  'p4::problem-space-apply-or-subgoal))

  (defvar *prefer-rules-types*
    (list 'p4::problem-space-prefer-goals 'p4::problem-space-prefer-operators
	  'p4::problem-space-prefer-bindings
	  'p4::problem-space-apply-or-subgoal)))

(defvar *trim-characters* '(#\< #\> #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

;; For learning with other tools, one can save the individual examples that
;; Hamlet used to learn.
(defvar *save-examples-p* nil)
;; Format for saving the examples:
;; - ebl: only the regressed state is saved
;; - complete: the whole state is saved
(defvar *save-examples-format* 'complete)
(defvar *examples-file* (format nil "~aexample-file.lisp" *domain-directory*)
  "Where the positive and negative examples go")
(defvar *example-number* 0)

(defvar *new-vars-format* 'prodigy
  "It can be either: prodigy, when learning crs from Prodigy; mtarff, when translating from PDDL to mtarff; or sayphi, when learning crs from sayphi")

;;; **********************************************************************
;;;   From induction.lisp
;;; **********************************************************************

;;; If t, it will try to induce and refine the rules
(defvar *inductionp* t)

(defvar *divide-by-prior-goal-p* nil "If t, it will consider more target concepts depending on the prior goals")
(defvar *re-use-prior-goal-p* nil "If t, it will use prior goal for recovering from induction")

;;; Number of times it will restructure a node
(defvar *number-restructures* 0)

(defvar *refine-all-p* t "If t, it will revisit all the rules of the same target concept that has just learnt a negative example")

(defvar *refined-target-concepts* nil "This is really don't needed. I could do this with a bit of programming")

(defvar *over-threshold-p* nil "If t, the number of crs in a node of the decision tree is greater than the threshold in that node, and there is a need to restructure that node")
(defvar *state-threshold* 0 "If *check-number-states* is t, then it will induce only if the number of true-in-state in the rule is greater-than-or-equal this number")

(defvar *check-number-states* t)

(defvar *max-number-bindings* 4
  "It specifies the maximum number of bindings for which Hamlet will try to intersect two rules")

;; It has a big value, though usually one will cut down that number
(defvar *max-ratio-some-goals* 0.4
  "Maximum ratio of goals in the some-candidate-goals with respect to the number of predicates in the domain")

(defvar *decision-tree-leaves* nil)
(defvar *decision-tree* nil)

;; Not used anymore
;;(defvar *domain-complete-goal-pairs* '((on on-table same-first-var)))

(defvar *le-index* 0)
(defvar *le-list* nil)
(defvar *learning-episodes* nil)
(defvar *cr-to-le-hash* (make-hash-table :test #'eq :size 400
					 :rehash-size 1.5))

(defvar *stop-repairing-p* nil)

;; This is to control the number of times that a rule is specialized 
(defvar *max-learning-steps* 4)

;;; **********************************************************************
;;;  From load-problems.lisp
;;; **********************************************************************

;; max search depth
(defvar *max-depth* most-positive-fixnum)
(defvar *max-cost* most-positive-fixnum)

(defvar *ipss-time* 0)

;; default output-level for my-run
(defvar *hamlet-output-level* 0)

;; if t increments the time-bound with rules to avoid utility problem
(defvar *increment-time-p* nil)
(defvar *dynamic-time-bound* nil)
(defvar *stop-after-problem-p* nil)
;; just as a way of externally controlling the repetition of a
;; search
(defvar *repeatp* nil)
(defvar *fully-interactive-p* t)
;;(defvar *not-trace-trees-p* t)

;; if t, it never repeats a problem
(defvar *never-repeat-p* t)

;; maximum number of repetitions over a problem
(defvar *max-number-cycles* 4)

;; to repeat the execution with the rules until they are correct
(defvar *repeat-problem-p* nil)

;; to repeat if the execution with rules was shorter than without, and
;; it did not expand the whole tree in without
(defvar *repeat-if-shorter-p* nil)

;; to repeat the execution of the set of problems that were solved
;; completely
(defvar *repeat-probset-p* nil)

(defvar *max-number-probset-repeats* 10
  "Maximum number of times that a probset is repeated")

;; list of solved problems with their number of steps
(defvar *solved-problems* nil)

;; if t, there was a change in the control rules
(defvar *something-changed-p* nil)

;; To repeat the execution with the rules if the rules lead to a
;; longer path than the shortest one
(defvar *re-refine-p* t)
(defvar *number-fired-crs* 0)
;;(defvar *acumulated-time* 0)
;;(defvar *acumulated-nodes* 0)
;;(defvar *acumulated-all-nodes* 0)
;;(defvar *acumulated-nodes-solution-path* 0)
(defvar *all-problems* nil)
(defvar *learn-probs* nil)

(defvar *to-do* nil)

;; To implement random behavior
(defvar *random-behavior-p* nil)
(defvar *number-repetitions-random-behavior* 5)

;; To perform dichotomic search within the space of makespans
(defvar *perform-makespan-dichotomic-search* nil)
(defvar *number-repetitions-makespan-dichotomic-search* 3)
(defvar *lower-bound-makespan-search* 0)
(defvar *upper-bound-makespan-search* most-positive-fixnum)

(defvar *number-training-problems* 200
  "Number of training problems to be generated when learning-type is active")

;;; Vars to follow FF solutions
(defvar *follow-ff-p* nil
  "Whether to use FF to generate alternative search trees when no solution was found by Prodigy when learning")

(defvar *ff-cr-file* (format nil "ff-control-rules.~a" *lisp-extension*)
  "The file where the FF domain dependent rules are")

;; In case it is loaded from Sayphi
(defvar *ipss-temporal-dir* (concatenate 'string *my-planning-path* "ipss/tmp/"))

(defvar *ff-output-directory* *ipss-temporal-dir*
  "The directory where FF inputs are supposed to be and output will go")

;;  (if *domain-name* 
;;      (format nil "~a-~(~a~)/" *ipss-temporal-dir* *domain-name*)
;;      *ipss-temporal-dir*)

(defvar *ff-solution* nil
  "It holds a list of operator instantiations, plan, generated by FF")

(defvar *prodigy-ff-solution* nil
  "It allows to follow the solution by FF during Prodigy reasoning")

(defvar *ops-in-ff-solution* nil
  "List of operators names in FF solution")

;;; Vars for active learning
(defvar *active-learning-parameters* nil)
(defvar *init-active-learning-parameters*
  (list (cons 'no-goals 1) (cons 'max-goals 5)
	(cons 'generated-problems 0) (cons 'valid-problems 0)))
(defvar *active-learning-trace* nil)
;; Domain dependent rules. It is a list of rules, each rule a list of sublists of the form:
;; (type-to-increase quantity). For instance, in logistics:
;; (((object 2)) ((city 1) (airplane 1) (truck 1)))
(defvar *init-active-learning-rules* nil)
(defvar *al-saving-rate* 10
  "After how many valid problems it will generate a new rules file, for convergence results")

(defvar *active-learning-mode* nil
  "In can be: nil (no active learning); problem (it will generate the next problem as similar as possible 
to the previous one; or rules (updates frecuencies based on regressed-state of rules learned)")
(defvar *active-learning-hash* nil
  "Hash-table with info on frecuencies of regressed-states, predicates and instances")
(defvar *last-problem* nil
  "The last generated problem for active-learning")
(defvar *changeable-types* nil
  "The list of types whose state can change from one problem to the next one.")
(defvar *finite-states* nil
  "Assoc list ((type1 predicate1 .. predicaten) .. (typem predicate1 .. predicatep))")
(defvar *domain-predicates* nil
  "List of predicate definitions (predicate type-arg-1 ... type-arg-n)")
(defvar *al-change-state-probability* 0.6
  "Probability of changing the state (against changing the goals)")

(defvar *bootstrapp* nil
  "If t, it will use the rules in (cr-file) when generating random problems for bootstraping")

;; Vars related to path-planning

(defvar *use-path-planning-p* nil
  "If t, it will load path-planner before running problem")

(defvar *re-compute-path-planning-p* nil
  "If t, it will translate the path planning problem for each problem. If is always the same, it is more efficient to do it only once at the beginning, and then reuse it by setting this flag to nil.")

(defvar *path-planning-dir* "/home/dborrajo/docencia/ejercicios/ias/codigo-busqueda-lisp/problemas/")

(defvar *pp-translator-rules* nil)

(defvar *path-planning-ops* '(move)
  "List of prodigy ops that relate to path planning")

;;; **********************************************************************
;;;  From contrib-daniel.lisp
;;; **********************************************************************

;; if t it will not ask whether we want more solutions...
(defvar *dont-ask-multiple-sols-p* t)

;; this will be the default value if *dont-ask-multiple-sols-p* is t
(defvar *multiple-solutions-flg* :shorter)


;;; if t, it will select randomly among the results of the select
;;; control rules
;;(defvar *random-selection-p* t)
;;; if t, it will keep all selections of control rules. If nil, it
;;; will only pick up the first one or randomly (depending on
;;; *first-selection-p* and *random-selection-p*)
(defvar *keep-all-selections-p* t)

;; if t, it will make a select of the default choices (no bactracking
;; over default)
(defvar *keep-default-only-p* nil)

;; if t, it will print all rules firing
(defvar *print-rules-fired-p* nil)

;; if t, it will store the fired control rules
(defvar *store-fired-crs-p* t)

;;; this is for handling correctly the unbound variables in the
;;; type-of-object meta-predicate
(defvar *current-control-rule* nil)

(defvar *use-rnpc* nil
  "If t, it will ask a learned model by RNPC or IBL how to make a decision")

(defvar *mtarff-directory* "/home/dborrajo/tmp/"
  "For the translation of PDL crs into relational Weka")

(defvar *var-code* 96
  "Char code of A")

(in-package "P4")

(when (and (boundp '*planner-for-learning*) (eq *planner-for-learning* 'ipss))

;;; this is to have the same behavior as usual Prodigy4.0
  (defvar *first-selection-p* nil)

;;; from Manuela for her code. If t, it will not have goal choices when
;;; applying operators
  (defvar *no-goal-choices-if-apply-p* nil)

  ;; hash table to relate vars of operators to types
  (defvar *vars-types* nil))

;;; **********************************************************************
;;;           From quality.lisp
;;; **********************************************************************

(in-package "COMMON-LISP-USER")

(defvar *use-quality-p* nil)
(defvar *best-cost* most-positive-fixnum)
(defvar *best-cost-achieved* nil)
(defvar *this-cost* 0)
(defvar *cost-label* 'default)
(defvar *costs-declared* nil)
(defvar *backtrack-with-costs-p* nil)
(defvar *compute-f-at-bindings* nil
  "If true, it will compute cost values for binding nodes, instead of only computing them at applied nodes")

(defvar *best-first-search-p* nil
  "If true, it will implement a best-first-search, computing F=G+H at all nodes")

(defvar *lower-bound-h* 0)

;;; **********************************************************************
;;;           From parallel.lsp
;;; **********************************************************************

(in-package "COMMON-LISP-USER")

(defvar *applied-operators* '(S-OP)) ; => Variable a eliminar tras pruebas.
(defvar *first-op* t)
(defvar *abs-duration* nil)
(defvar *op-durations* nil)
(defvar *list-TPs* nil)
(defvar *last-path* nil)         ;esta variable almacena el path del operador aplicado anterior
(defvar *last-op-consistence* t) ;esta variable es usada para saber si la aplicacion del ultimo operador ha sido consistente o no
(defvar *consistence* t)
(defvar *applied-operators-id* '(0))
(defvar *no-finish* t)


;;; **********************************************************************
;;;           From applied.lsp
;;; **********************************************************************

;;Inicializacion  para la Red Temporal
(defvar *rete* nil)
(defvar *num* 500)               ; numero de TP's maximo = 500  operadores en el plan
(defvar *list-links*  nil)       ; All links of all added operators
(defvar *list-TPop* nil)         ; Lista con todos los operadores que son consistentes
(defvar *lista-de-estados* nil)  ; Visited states after adding operators
(defvar *mi-links-temp*  nil)    ; Lista de links para cada operador
(defvar *list-goal* nil)	      ; Lista de links que se establecen con el ultimo operador
(defvar *Final* nil)             ; Timepoint del operador F-OP

;;Estas dos variables serviran para establecer el makespan. Si es nil es que sera infinito, si contiene algun valor se debera
;;imponer entre el origen y el horizonte dichos valores. Si ambos contienen el mismo valor es porque es un valor fijo [20,20]
;;Estas variables se pasaran desde prodigy (sintaxis PDDL2.1 traducida)
(defvar *DurationMin*  0)   ; Minimo
(defvar *DurationMax*  220) ; Maximo
(defvar *MakeSpan* 0)       ;El final
(defvar *make-span* nil)    ;Variable con el valor de make-span a t/nil

;; Se deberia pasar una lista con la distancia max. y min entre cada 2 operadores. Ahora considero infinito
(defvar *Max-Dist-Ops* 10000)
(defvar *Min-Dist-Ops* 0)

;; Para el manejo de Recursos. Added 9/2/2003. Sujeto a modificaciones ...
(defvar *num-r* 0)   ; Numero de recursos
(defvar *num-op* 0)  ; Numero de operadores
(defvar *M-Resources* nil) ; Matriz con la capacidad global de cada recurso (1 linea) y consumo de cada op. (Template operador)
(defvar *list-links-res*  nil)       ; Lista con todos los links de todos los operadores added debido a los recursos.
(defvar *list-obj* nil) ; Contiene la lista de objetos correspondientes al recurso.
(defvar *Resorce-Consumed* nil) ;contine la matriz con el number de veces que ese recurso se ha consumido durante la search
(defvar *creado* nil)  ; Si se quiere leer el recurso del fichero o de la pantalla.
(defvar *calculado* nil) ; Si ya se ha calculado el recurso con el que se debe hacer el binding
(defvar *recurso-temp* nil)

;; ----------- PARA hamlet-test ------------------------------------------
(defvar *calculate-resources* nil) ;; Ver si se calculan o no los recursos .MD Marzo 2003
(defvar *make-span-p* nil)  ;Variable que se utiliza in Hamlet-test para especificar si se quiere o no utilizar make-span
(defvar *MS-value* 0);Variable para hamlet-test que almacena el valor del MakeSpan
(defvar *Res-value* nil) ; Si hay o no recurso
(defvar *numTP* 500)   ; Numero de TP
(defvar *Acti-dist* 10000)  ; Las restricciones entre actividades



;;; **********************************************************************
;;;           From utility.lisp
;;; **********************************************************************

(defvar *utilityp* nil
  "If t, it will store time spent on matching each rule, the number of times it was tried to be matched, and the number of times it matched (it was fired)")

(defvar *utility-threshold* 0
  "Threshold for removing a rule after utility analysis")

(defvar *utility-data* nil
  "Variable that contains the data for the utility of each rule")

(defvar *time-per-node* 0.0019
  "Time spent to evaluate each node. Needed to compute time saved by the application of a control rule")

(defvar *print-utility-p* nil
  "If t, it will print utility analysis after each run")

(defvar *utility-output* (format nil "~autility.~a" *domain-directory* *lisp-extension*)
  "Where the output of the utility storing goes")

;;; **********************************************************************
;;;           From probabilities.lisp
;;; **********************************************************************

(defvar *prob-operator* nil)
(defvar *prob-goals* nil)
(defvar *prob-subgoal* nil)
(defvar *probabilities-p* t)
(defvar *use-probabilities* nil)
(defvar *umbral-subgoal* 0.5)
(defvar *umbral-prob* 0)
(defvar *all-goals* nil)
(defvar *all-operators* nil)
(defvar *prob-rules* nil)
(defvar *number-rules-probs* 0)
(defvar *priority-rules* t)
(defvar *number-operators-decision* 0)
(defvar *number-goals-decision* 0)
(defvar *number-bindings-decision* 0)
(defvar *number-subgoal-decision* 0)
(defvar *peso-reglas* 0.8)
(defvar *peso-probs* 0.2)


;;; **********************************************************************
;;;   From problem-generator.lsp
;;; **********************************************************************

(defvar *problem-generator-alist* nil)
(defvar *types-info* nil)
(defvar *stop-during-problem-generation-p* nil)

;;; **********************************************************************
;;;   From pltool.lisp
;;; **********************************************************************

(defvar *using-pltool-p* nil)
(defvar *store-plans-p* nil)
(defvar *all-solutions* nil)
(defvar *all-trees* nil)
;; if t, it will store the fired control rules
(defvar *interface-store-fired-crs-p* nil)
(defvar *interface-probset-p* nil)
(defvar *interface-printp* nil)
(defvar *which-rules-window* 0)

;; tree-views
(defvar *tree-view-with-rules* nil)
(defvar *tree-view-without-rules* nil)
(defvar *rules-view* nil)
(defvar *plans-view* nil)
(defvar *rules-model* nil)
(defvar *mouse-screen* nil)
(defvar *popup* nil)
(defvar *muestra-popup* nil)
(defvar *rules-view1* nil)
(defvar *rules-view2* nil)
(defvar *rules-model1* nil)
(defvar *rules-model2* nil)

;; sayphi
(defvar *say-heuristic* nil)
(defvar *say-algorithm* nil)

;;; *********************************************************************
;;;            From pddl2ipss.lisp
;;; *********************************************************************

(defvar *new-vars* nil)
(defvar *new-effects* nil)

;;; *********************************************************************
;;;            From ipss2path-planning.lisp
;;; *********************************************************************

(defvar *pp-utility* nil)
(defvar *pp-cost* nil)
(defvar *pp-time* nil)
(defvar *pp-distance* nil)
(defvar *pp-other-metric* nil)

;;-----------------------------------inicio parte de david montero-----------------------------------------------------------

;;; *********************************************************************
;;;            Para guardar el path completo de los archivos
;;; *********************************************************************

(defvar *dominio* nil)
(defvar *problemas* nil)
(defvar *reglas* nil)

;;; *********************************************************************
;;;            Para guardar los planes y los arboles
;;; *********************************************************************

(defvar *estructura-arbol* nil)
(defvar *estructura-arbol2* nil)
(defvar *estructura-arbol3* nil)
(defvar *actual-lvl-arbol* nil)
(defvar *tipo-lvl* nil)

;;; *********************************************************************
;;;            variables para la parte de traduccion
;;; *********************************************************************

(defvar *fichero-temporal* nil)
(defvar *probs-directory* nil)
(defvar *probset-directory* nil)
(defvar *domain-directory-trans* *domain-directory*)

;;; *********************************************************************
;;;            variables para la parte del experimenter
;;; *********************************************************************

(defvar *fichero-experimenter* nil)
(defvar *numero-dominios-resueltos* nil)
(defvar *tipo-dominio* nil)
(defvar *modo-ejecucion* 1)
(defvar *almacenar-planes* nil)
(defvar *dominio-experimenter* nil)
(defvar *problema-experimenter* nil)
(defvar *reglas-experimenter* nil)
(defvar *arbol-dominios* nil)
(defvar *veces-experimenter* 1)
(defvar *arbol-algoritmos* nil)
(defvar *lista-dominios* nil)
(defvar *lista-algoritmos* nil)
(defvar *datos-experimenter-tiempo* nil)
(defvar *datos-experimenter-expandidos* nil)
(defvar *datos-experimenter-longitud* nil)
(defvar *datos-experimenter-coste* nil)
(defvar *datos-experimenter-longitud-actuales* nil)
(defvar *datos-experimenter-coste-actuales* nil)
(defvar *datos-experimenter-veces-ganadas-tiempo*)
(defvar *datos-experimenter-veces-ganadas-expandidos*)
(defvar *datos-experimenter-tiempo-actuales*)
(defvar *datos-experimenter-expandidos-actuales*)

;;---------------------------------------fin parte david montero----------------------------------------------------------

;;; *********************************************************************
;;;            Structures
;;; *********************************************************************

#|
(defstruct decision-node
  (name nil)              ; the decision to be taken
  (children-type nil)     ; effect or any kind of meta-predicate
  (children nil)          ; list of children
  (parent nil)            ; the parent
  (fixedp t)              ; whether we can do induction over it or not
  (negative-examples nil) ; negative examples of all rules in the same
			  ; branch (only in the leafs)
  (threshold 3)           ; number of rules above which the induction
			  ; will restructure the node (induce again)
  (negatives nil)         ; the set of negative examples of that
			  ; target concept
  (rules nil)             ; the set of rules of that target concept
  (frequency-table-state nil)   ; predicates vs. # of + and - examples that
			  ; have that predicate. Hash table accessed with the
			  ; predicate, and the value is a cons of the
			  ; frequencies for + and -
  (frequency-table-prior-goal nil)   ; predicates vs. # of + and - examples
				     ; that have that predicate. hash table
				     ; accessed with the predicate, and the
				     ; value is a cons of the frequencies
				     ; for + and -
  (needs-language-change-p nil) ; whether or not the unclassification function was called
)
|#

(defstruct (decision-node (:print-function pp-dn))
  (name nil)
  (children-type nil)
  (children nil)     
  (parent nil)
  (fixedp t)
  (threshold 3)
  (negatives nil)
  (rules nil)
  (frequency-table-positive nil)
  (frequency-table-negative nil)
  (frequency-table-prior-goal-positive nil)
  (frequency-table-prior-goal-negative nil)
  (needs-language-change-p nil))


#|
(defstruct learning-episode
  (name nil)            ; le-<index>
  (type nil)            ; it can be deduced, induced, refined, or artificial (in case it is generated just for testing)
  (problem nil)         ; the problem in which it learnt the rule
  (rule nil)            ; the new rule from the learning episode
  (from nil)            ; list of previous learning-episodes if
			; induced or nil if deduced
  (binding-used nil)    ; binding used to do the induction. they are
			; of the form: ((cr1 . bindings) (cr2 . nil))
  (other-bindings nil)  ; list of lists of learning-episodes and
			; bindings so that it can backtrack if induced
  (preconds-pool nil)   ; list of preconds to be used to specialize
  (preferred-preconds nil) ; list of preferred preconds to specialize
  (deletedp nil)        ; t if after refining, it gets deleted
			; forever if it fails in any problem
			; nil if it is active
  (prior-goal nil)      ; prior goals of the deduced rules where a
			; rule came from. it will be of the form:
			; ((rule1 prior-goal types-of-prior-goal)*)
  (learning-steps 0)    ; for each inductive, refinement step, we will
			; increment this slot by one. it allows to
			; compute the confidence on the control rule
  (opposite-tried-p nil) ; t if it has already tried the opposite combination
  (matching-time 0)     ; total matching time spent on rule
  (matchings 0)         ; number of times the rule was matched
  (num-nodes 0)         ; estimated average of number of nodes that one saves
			; with rule firing
  (firings 0)           ; number of times it was fired
)
|#

(defstruct (learning-episode (:print-function pp-le))
  (name nil)
  (type nil)
  (rule nil)
  (problem nil)
  (from nil)
  (binding-used nil)
  (other-bindings nil)
  (preconds-pool nil)
  (preferred-preconds nil)
  (deletedp nil)
  (prior-goal nil)
  (learning-steps 1)
  (opposite-tried-p nil)
  (matching-time 0)
  (matchings 0)
  (num-nodes 0)
  (firings 0))
