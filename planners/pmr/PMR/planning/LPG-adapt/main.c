/*********************************************************************
 * (C) Copyright 2002  Universita' degli Studi di Brescia
 *     Dipartimento di Elettronica per l'Automazione
 *     Via Branze 38, 25123 Brescia, Italy
 *
 * All rights reserved. Use of this software is permitted ONLY for
 * non-commercial research purposes, and it may be copied only
 * for that use only. All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the University of Brescia make any warranty about the
 * software or its performance.
 *
 *********************************************************************/



/********************************************************************
 * File: main.c 
 * Description:  Main routins of LPG.
 *
 *   PDDL 2.1 version without conditional and quantified effects 
 *
 * Authors: Alfonso Gerevini, Marco Lazzaroni, Alessandro Saetti, 
 *          Ivan Serina, Sergio Spinoni
 *
 *********************************************************************/ 



#include <math.h>
#include <string.h>

#include <sys/time.h>

#define MAXINT 2147483647

#ifdef __WINLPG__
#include <time.h>
#endif

#include "lpg.h"
#include "parse.h"
#include "inst_easy.h"
#include "inst_hard.h"
#include "inst_pre.h"
#include "inst_final.h"
#include "inst_utils.h"
#include "check.h"
#include "utilities.h"
#include "numeric.h"
#include "LpgOutput.h"
#include "LpgTime.h"
#include "output.h"
#include "mutex.h"
#include "LocalSearch.h"
#include "ActionSubgraph.h"
#include "ComputeReachInf.h"
#include "search.h"
#include "relax.h"
#include "memory.h"
#include "mutex.h"

#include "derivedpred.h"

#include "variables.h"


/********************************************************************
 *                           HEADERS FOR PARSING                    *
 ********************************************************************/

void load_ops_file (char *filename);
void load_fct_file (char *filename);



/*****************************************************************
 *                          MAIN ROUTINE                         *
 *****************************************************************/







int main (int argc, char *argv[])
{
 
  /* 
   * Nome per il file del dominio
   **
   * resulting name for ops file 
   */
  char ops_file[MAX_LENGTH] = "";
  /* 
   * Nome del file del problema
   **
   * same for fct file 
   */
  char fct_file[MAX_LENGTH] = "";

  char sol_file[MAX_LENGTH] = "";
  float plan_time = 0.0; 

#ifndef __WINLPG__
  struct tms start, end;
#else
  clock_t start, end;
#endif


  struct timeval tv;
  struct timezone tz;

  State current_start, current_end;
  int i, j, k, optimize;
  int num_splitted = 0;
  Bool found_plan=0;


#ifdef __EFENCE__
  extern int EF_ALLOW_MALLOC_0;
  EF_ALLOW_MALLOC_0 = 1;
#endif

#ifndef __SUN__
  /* Daniel commented out
#ifndef __WINLPG__
  so_signal_management();
#endif
  */
#endif

  
  gbracket_count = 0;
  inertial_facts = NULL;

  /*
   * Inizializzazione delle variabili globali per lo stato iniziale e finale
   **
   * Init global State variables 
   */
  ginitial_state.F = (int *)calloc(MAX_STATE, sizeof(int));
  ggoal_state.F = (int *)calloc(MAX_STATE, sizeof(int));
  current_start.F = (int *)calloc(MAX_STATE, sizeof(int));
  current_start.V=NULL;
  current_end.F =  (int *)calloc(MAX_STATE, sizeof(int));
  current_end.V=NULL;
  ginitial_state.num_F = ggoal_state.num_F =  current_start.num_F = current_end.num_F = 0;

  for (i = 0; i <= MAX_PLAN_LENGTH; i++)
    {
      gplan_states[i].F = (int *)calloc(MAX_STATE, sizeof(int));
      gplan_states[i].num_F = 0;
    }

  strcpy (gcomm_line, "");
  for (i = 0; i < argc; i++)
    {
      strcat (gcomm_line, argv[i]);
      strcat (gcomm_line, " ");
    }
  get_path (*argv, glpg_path);
  initialize_preset_values ();

  //  init_statistic è la funzione che ha il compito di aprire tutti i file per la media e la varianza

#ifdef __STATISTIC_LM__
  init_statistic();
#endif

  /*
   * Reset delle hash table per le variabili numeriche
   **
   * Reset  hash-table
   */
  reset_cvar_hash_table();
  reset_cvar_hash_table_effects();
  
  /* 
   * Inizializzazione del seed per le scelte casuali
   **
   * Initialize random seed
   */
  gettimeofday (&tv, &tz);
  seed = ((tv.tv_sec & 0177) * 1000000) + tv.tv_usec;


  /* 
   * Analisi dei parametri della linea di comando
   ** 
   * command line treatment
   */
  if (argc == 1 || (argc == 2 && *++argv[0] == '?'))
    {
      lpg_usage ();
      exit (1);
    }
  if (!process_command_line (argc, argv))
    {
      lpg_usage ();
      exit (1);
    }

  /* 
   * Costruzione dei nomi dei file dalla linea di comando 
   ** 
   * make file names
   */

  /* 
   * Controllo che siano stati definiti sia il file
   * del il dominio che quello del problema
   **
   * one input name missing
   */
  if (!gcmd_line.ops_file_name || !gcmd_line.fct_file_name)
    {
      fprintf (stdout, "\n%s: two input files needed\n\n", NAMEPRG);
      lpg_usage ();
      exit (1);
    }

  /* 
   * Costruzione dei nomi dei file con il path completo
   **
   * add path info, complete file names will be stored in
   * ops_file and fct_file
   */

#ifndef __WINLPG__
  sprintf (ops_file, "%s%s", gcmd_line.path, gcmd_line.ops_file_name);
  sprintf (fct_file, "%s%s", gcmd_line.path, gcmd_line.fct_file_name);

  strcpy (gops_file_name, ops_file);
  strcpy (gfct_file_name, fct_file);
  sprintf (sol_file, "%s%s", gcmd_line.path, gcmd_line.sol_file_name);
#else
  sprintf (ops_file, "%s", gcmd_line.ops_file_name);
  sprintf (fct_file, "%s", gcmd_line.fct_file_name);

  /**
  if (strchr(ops_file,'/') || ops_file[0] == '/')
    strcpy(gops_file_name, (strrchr(ops_file, '/')+sizeof(char)));
  else
    strcpy(gops_file_name,ops_file);

  if (strchr(fct_file,'/') || fct_file[0] == '/')
    strcpy(gfct_file_name, (strrchr(fct_file, '/')+sizeof(char)));
  else
    strcpy(gfct_file_name,fct_file);

  if(gpath_sol_file_name[strlen(gpath_sol_file_name)-1] != '/' && gpath_sol_file_name[0]!='\0')
    strcat(gpath_sol_file_name, "/");
  **/


  if (strchr(ops_file,'\\') || ops_file[0] == '\\')
    strcpy(gops_file_name, (strrchr(ops_file, '\\')+sizeof(char)));
  else
    strcpy(gops_file_name,ops_file);

  if (strchr(fct_file,'\\') || fct_file[0] == '\\')
    strcpy(gfct_file_name, (strrchr(fct_file, '\\')+sizeof(char)));
  else
    strcpy(gfct_file_name,fct_file);

  if(gpath_sol_file_name[strlen(gpath_sol_file_name)-1] != '\\' && gpath_sol_file_name[0]!='\0')
    strcat(gpath_sol_file_name, "\\");
#endif



  //  if(DEBUG1)
    {
     printf ("\n\n; Command line: %s  \n\n", gcomm_line);

    }

  /*
   * Parsing dei file di input
   ** 
   * parse the input files
   */

  /* 
   * Inizializzazione del timer per il tempo di istanziazione
   **
   * start parse & instantiation timing
   */
  times (&glob_start_time);
  times (&start);

  /*
   * Analisi del file del domini
   **
   * domain file (ops)
   */

  printf ("\nParsing domain file: ");

  /* 
   * Il file del dominio deve essere letto prima della definizione del
   * problema
   ** 
   * it is important for the pddl language to define the domain before
   * reading the problem
   */
  load_ops_file (ops_file);

 /*
  * serve ad ottenere una seconda copia degli operatori (dominio)
  **
  * dirty trick to get another copy of gloaded_ops 
  */
  gloaded_pl2ops = gloaded_ops;
  gloaded_ops = NULL;
  gdomain_name = NULL;
  gorig_initial_facts = NULL;
  gorig_goal_facts = NULL;
  gmetric_exp = NULL;
  gloaded_axioms = NULL;
  gparse_types = NULL;
  gparse_constants = NULL;
  gparse_predicates = NULL;
  gparse_functions = NULL;
  gparse_objects = NULL;
  gorig_constant_list = NULL;
  gpredicates_and_types = NULL;
  gfunctions_and_types = NULL;

  //free_PlOperator(gderived_predicates);
  gderived_pl2predicates = gderived_predicates;
  gderived_predicates = NULL;
  gnum_derived_predicates = 0;
  load_ops_file (ops_file);

  /*
   * Parsing del file del problema
   ** 
   * problem file (facts)
   */
  if (gcmd_line.display_info >= 1)
    {
      printf (" ... done.\nParsing problem file: ");
    }

  load_fct_file (fct_file);

  GpG.has_timed_preconds = NULL;
  GpG.fact_is_timed = NULL;

  /* 
   * Se ci sono fatti temporizzati attivo il flag corrispondente
   * e quello per piani temporali
   **
   * If there are some timed initial literal set the GpG.timed_facts flag
   * and the temporal_plan flag.
   */ 
  if (gnum_tmd_init_fcts) {
    GpG.timed_facts_present = TRUE;
    GpG.temporal_plan = TRUE;
  }
  
  /*
   * Se ci sono predicati derivati attivo il flag corrispondente
   **
   * If there are some derived predicates activate the derived_predicates flag
   */
  if (gnum_derived_predicates) 
    GpG.derived_predicates = TRUE;

  /*
   * Aggiungo effetti "dummy" agli operatori privi di effetti booleani
   **
   * add dummy effect to operators without boolean effects 
   */
  add_dummy_effects (gloaded_ops);
  add_dummy_effects (gloaded_pl2ops);
  if (GpG.derived_predicates) {
    add_dummy_effects(gderived_predicates);
    add_dummy_effects(gderived_pl2predicates);
  }
  add_and_effect(gloaded_ops);
  add_and_effect(gloaded_pl2ops);

  /*
   * Conto effetti e precondizioni numeriche
   ** 
   * counts numeric preconds and effects 
   */
  count_num_preconds_and_effects ();
  GpG.gplan_actions = NULL;
  GpG.plan_actions_for_quality_mode = NULL;
  GpG.fixpoint_plan_length = 0;

  /* 
   * Elimino i timed facts dai fatti iniziali (vengono spostati in una
   * lista separata)
   ** 
   * Move timed initial literals from the list of initial facts to
   * one separated list
   */
  if (GpG.timed_facts_present)
    {
      clear_Timed_Fact_Nodes();
    }

  if (gcmd_line.display_info >= 1)
    printf (" ... done.\n\n");

  allocate_after_parser();

  /* 
   * Elimino i nodi numerici e durativi (riduco al pddl1)
   **
   * delete numeric and durative nodes (reduce the domain
   * to pddl1)
   */
  reduce_pddl2_to_pddl1 ();

  /* 
   * Costruisce la lista di tipi e costanti che compainono nel dominio
   **
   * This is needed to get all types.
   */
  build_orig_constant_list ();

  /* 
   * Controllo che il dominio sia supportato (ADL)
   ** 
   * last step of parsing: see if it's an ADL domain!
   */

  if (!make_adl_domain ())
    {
      printf ("\n%s: this is an ADL problem!", NAMEPRG);
      printf ("\n     can't be handled by this version.\n\n");
      exit (1);
    }

  /*
   * Instanziazione degli operatori 
   **
   * now instantiate operators;
   */
  

  /**************************
   * first do PREPROCESSING *
   **************************/


  /*
   * Colleziono tutte le stringhe (nomi di costanti, tipi, predicati, ecc) e
   * codifico il dominio in interi
   **
   * start by collecting all strings and thereby encoding
   * the domain in integers.
   */
  encode_domain_in_integers ();

  /* 
   * Preprocessing delle informazioni inerziali
   **
   * inertia preprocessing, first step:
   *   - collect inertia information
   *   - split initial state into
   *        _ arrays for individual predicates
   *        - arrays for all static relations
   *        - array containing non - static relations
   */
  do_inertia_preprocessing_step_1 ();

  /* 
   * Normalizzazione delle formule nella descrizione del dominio
   *   - semplificazione (elimina le tautologie)
   *   - espansione delle variabili quantificate
   *   - spostamento dei NOT sugli atomi
   ** 
   * normalize all PL1 formulae in domain description:
   * (goal, preconds and effect conditions)
   *   - simplify formula
   *   - expand quantifiers
   *   - NOTs down
   */
  normalize_all_wffs ();

  /* translate negative preconds: introduce symmetric new predicate
   * NOT-p(..) (e.g., not-in(?ob) in briefcaseworld)
   */
  translate_negative_preconds ();

  /* 
   * Suddivide gli operatori in easy (le precondizioni sono disgiunzioni
   * di congiunzioni) e hard (precondizioni non DNF)
   ** 
   * split domain in easy (disjunction of conjunctive preconds)
   * and hard (non DNF preconds) part, to apply
   * different instantiation algorithms
   */
  split_domain ();


  /***********************************************
   * PREPROCESSING FINISHED                      *
   *                                             *
   * NOW MULTIPLY PARAMETERS IN EFFECTIVE MANNER *
   ***********************************************/

  /*
   * pre-instanziazione degli operatori easy
   **
   * pre-instantiate easy operators
   */
  build_easy_action_templates ();

  /*
   * pre-instanziazione dei predicati derivati easy
   **
   * pre-instantiate easy derived predicates
   */
  if (GpG.derived_predicates)
    build_easy_derived_predicates_templates();
  
  
  /*
   * pre-nstanziazione degli operatori hard
   **
   * pre-instantiate hard operators
   */
  build_hard_action_templates ();

  /*
   * pre-instanziazione dei predicati derivati hard
   **
   * pre-instantiate hard derived predicates
   */
  if (GpG.derived_predicates)
    build_hard_derived_predicates_templates();

  
  times (&end);
  TIME (gtempl_time);
  times (&start);


  check_time_and_length (0);	// con zero non controlla la lunghezza

  srandom(seed);

#ifdef __MY_OUTPUT__
#ifndef __PARSER_ONLY__
  printf ("\nSeed %d  \n", seed);
#endif
#endif

#ifndef __PARSER_ONLY__
  if (GpG.mode == INCREMENTAL)
    printf("\n\nModality: Incremental Planner\n\n");
  else
    if (GpG.mode == SPEED)
      printf("\n\nModality: Fast Planner\n\n");
    else
      if (GpG.mode == QUALITY)
	printf("\n\nModality: Quality Planner\n\n");
#endif

  /* 
   * analisi di raggiungibilià intermini di raggiungimento del
   * fixpoint rilassato (no mutex, no effetti cancellanti, non
   * considera effetti e precondizioni numeriche)
   **
   * perform reachability analysis in terms of relaxed
   * fixpoint
   */

  perform_reachability_analysis ();

  times (&end);
  TIME (greach_time);
  times (&start);

  check_time_and_length (0);	// con zero non controlla la lunghezza


  /* 
   * Colleziona i fatti rilevanti e costruisce la rappresentazione
   * finale di dominio e problema.
   ** 
   * collect the relevant facts and build final domain
   * and problem representations.
   */

  collect_relevant_facts ();

  times (&end);
  TIME (grelev_time);
  times (&start);

  check_time_and_length (0);	// con zero non controlla la lunghezza 


  /* 
   * Termina l'instanziazione costruendo il grafo fatti-azioni
   ** 
   * now build globally accessable connectivity graph
   */
  build_connectivity_graph ();

  /*
   * Rappresentazione finale dei predicati derivati
   **
   * Build final representation for derived predicates
   */
  if (GpG.derived_predicates) 
    create_final_derived_predicates();

  times (&end);
  TIME (gconn_time);
  times (&start);

  check_time_and_length (0);	// con zero non controlla la lunghezza 

  /* 
   * associa ad ogni gef_conn[i] il ploperator completo corrispondente 
   **
   * associate the complete ploperator to each EfConn
   */
  associate_PlOperator_with_EfConn ();

  /* 
   * aggiunge le grandezze numeriche e completa le azioni con precondizioni
   * ed effetti numerici
   **
   * add numerical variables, preconditions and effects
   */
  add_composite_vars (0, gfirst_suspected_ef_conn);

  /* 
   * elimina le azioni inutili
   **
   * delete dummy actions (action with no useful effects)
   */ 
  check_actions_utility();

  make_numgoal_state(GpG.numeric_goal_PlNode);

  /*
   * rende costantemente falsi i confronti tra grandezze numeriche non inizializzate
   * serve per quei casi tipo zenotravel in cui le connessioni vengono fissate dalla presenza o meno dell'inizializzazione di 'distance'
   **
   * force all tests involving non initialized variables to result with "false"
   */
  make_false_all_checks_on_not_init ();

  /* 
   * Semplificazione delle variabili inerziali
   **
   * Semplification for inertial vars
   */
  propagate_inertias ();

#ifdef __MY_OUTPUT__
  if (DEBUG0)
    if (GpG.non_strips_domain)
      {
	//printf("\nThis is a non-strips domain");
	if (GpG.variable_duration)
	  printf ("\n\nAction durations have been computed");
	else
	  printf ("\n\nThere is no action duration to compute\n");
      }
#endif
  
  /* 
   * Inizializza i pesi per il costo e il tempo del piano, in accordo con la metrica
   **
   * Set vars orig_weight_cost and orig_weight_time according with plan evaluation metric
   */
  if (goptimization_exp != -1)
    set_cost_and_time_coeffs ();

  /* 
   * Costruisce il vettore gli effetti numerici delle azioni
   **
   * Make numeric effects structure
   */
  create_descnumeff_of_efconns ();

  /* 
   * Copia il valore iniziale delle variabili numeriche nello stato iniziale 
   **
   * Copy initial values for numeric variables in the initial_state
   */  
  ginitial_state.V = (float *) calloc(max_num_value, sizeof(float));
  memcpy(ginitial_state.V, gcomp_var_value, gnum_comp_var * sizeof(float));
  
  /* 
   * Controlla se è utile ed eventualmente spezza le azioni durative
   **
   * split actions 
   */
  if (GpG.durative_actions_in_domain) {

    EfConn *contraddicting_ef_conns = NULL;
    int num = gnum_ef_conn -  gfirst_suspected_ef_conn;
    
    // Se ci sono azioni con effetti contraddittori le tolgo momentaneamente del gef_conn
    if (num > 0)
      {
	contraddicting_ef_conns = (EfConn *)calloc((num + 1), sizeof(EfConn));
	memcpy(contraddicting_ef_conns, &gef_conn[gfirst_suspected_ef_conn], num * sizeof(EfConn));
	gnum_ef_conn = gfirst_suspected_ef_conn;
      }
    
    if (GpG.perform_split)
      split_actions();
    
    if (GpG.splitted_actions) {
      num_splitted = (gextended_ef_conn - gnum_ef_conn) / 2;
      gnum_ef_conn = gextended_ef_conn;
      gnum_ft_conn = gextended_ft_conn;
      gnum_ef_block = gextended_ef_block;
      gnum_ft_block = (gnum_ft_conn >> 5) + 1;
      gfirst_suspected_ef_conn = gnum_ef_conn;

      // reinserisco le eventuali azioni con effetti contraddittori in coda al gef_con
      if (num > 0)
	{
	  while ((gnum_ef_conn + num) >= max_num_efconn) 
	    {
	      max_num_efconn += MAX_EF_FT_INCREASE;
	      gef_conn = (EfConn *)realloc(gef_conn, max_num_efconn * sizeof(EfConn));
	      memset(&gef_conn[gnum_ef_conn], 0, max_num_efconn - gnum_ef_conn);
	    }
	  
	  memcpy(&gef_conn[gnum_ef_conn], contraddicting_ef_conns,  num * sizeof(EfConn));
	}
    }
    
    gnum_ef_conn += num;
    gnum_ef_block = (gnum_ef_conn >> 5) + 1;
  }

 
  if (GpG.timed_facts_present) {
    make_timed_fct_vector();
    extract_timed_preconditions();

    if (!GpG.timed_preconditions) {
#ifdef __MY_OUTPUT__
      printf("\n\nNo timed in preconditions : disable timed");
#endif
      GpG.timed_facts_present = FALSE;
    }

  }


  if (DEBUG0)
    {
      printf ("\n\n\nAnalyzing Planning Problem:");
      printf ("\n\tTemporal Planning Problem: %s", GpG.temporal_plan ? "YES" : "NO");
      printf("\n\tNumeric Planning Problem: %s", (GpG.is_domain_numeric)?"YES":"NO");
      printf("\n\tProblem with Timed Initial Literals: %s", GpG.timed_preconditions ? "YES" : "NO");
      //      printf("\n\tProblem with Timed Initial Litearals: %s", GpG.timed_facts_present ? "YES" : "NO");
      printf("\n\tProblem with Derived Predicates: %s", (GpG.derived_predicates)?"YES":"NO");
      if (GpG.derived_predicates)
	printf("\n\tDerived predicates in actions' preconditions: %s\n", GpG.derived_pred_in_preconds?"YES":"NO");

#ifndef __PARSER_ONLY__
      printf("\n\nEvaluation function weights:\n     Action duration %.2f; Action cost %.2f\n\n", GpG.orig_weight_time, GpG.orig_weight_cost);
#endif 

    }

  if(DEBUG1) {
    printf("\n\tSplitted actions: %s\n", (GpG.splitted_actions)?"YES":"NO");
    if (GpG.splitted_actions)
      printf("\nNum extended actions (normal + splitted): %d (%d actions have been splitted)\n", gextended_ef_conn, num_splitted); 
  }

  times (&end);
  TIME (gnum_time);
  times (&start);


  /* 
   * Stampa informazioni relative all'instanziazione delle azioni
   **
   * Print information about action istantiation
   */
  print_parser_info_for_debug();

#ifdef __PARSER_ONLY__
  if ((gnum_ef_conn == gfirst_suspected_ef_conn) || !GpG.try_suspected_actions)
    {
      printf("\n\nDomain and problem analysis successfully ended\n");
      
      if (GpG.compile)
	{
	  store_compiled_domain("compiled-domain.pddl");
	  store_compiled_problem("compiled-problem.pddl");
	}

      fflush(stdout);
      exit(0);
    }
  else
    printf("\n\nEvaluating mutex and reachability info to define contraddicting actions instantiations...");
#endif
  
  //  if (GpG.numrestart > 0 && GpG.numtry > 0) {
  
  if (DEBUG0 && !DEBUG1) {
    printf ("\nComputing mutex... ");
    fflush (stdout);
  }
  if (DEBUG1)
    printf ("\n\n--- COMPUTE MUTEX BETWEEN FACTS ---\n");
  
  if (GpG.accurate_cost >= 0)
    {	
      allocate_reachability_information_data();
      
      
      allocate_reachability_compvar_information_data();
    }
  
  /* 
   * Valuta le relazioni mutex tra i fatti
   **
   * Compute mutex between facts
   */
  
  calc_mutex (&ginitial_state);
  
  //  }
  
  
  times (&end);
  TIME (gmutex_ft_time);
  
  if (DEBUG2)
    printf ("\n");
  if (DEBUG1)
    printf ("\n   --> Compute mutex between facts TOTAL TIME: %12.2f",gmutex_ft_time);
  
  times (&start);
  //  if (GpG.numrestart > 0 && GpG.numtry > 0) {
  if (DEBUG1)
    printf ("\n\n--- COMPUTE MUTEX BETWEEN ACTIONS ---\n");
  
  /*
   * Valuta le relazioni mutex tra le azioni e tra azioni e fatti.
   **
   * Compute action-action, action_fact, fact-action mutex
   */
  
  //calc_mutex_derived();
  
  calc_mutex_ops ();
  //  }
  
  
  times (&end);
  TIME (gmutex_ops_time);
  
  if (DEBUG1)
    printf ("\n   --> Compute mutex between actions TOTAL TIME: %12.2f\n",gmutex_ops_time);

  

  times (&start);

  clean_numeric_preconditions();

  /*
   * Individua e marca gli effetti continui (dipendenti dal tempo in cui vengono realizzati)
   **
   * Search and mark continuous effects
   */
  set_continuous_effects();

  //  if (GpG.numrestart > 0 && GpG.numtry > 0) {
    if (DEBUG1)
      printf ("\n\n--- COMPUTE MUTEX BETWEEN NUMERIC FACTS ---\n");
    
    /* 
     * Valuta le relazioni mutex dovute a effetti/precondizioni numeriche
     **
     * Compute mutex between action with numeric effects
     */
    calc_mutex_num_efs ();
    
    //#ifdef __TEST_REACH__

    /*
     * Analisi di raggiungibilità di fatti e azioni
     **
     * Facts and actions reachability analisys 
     */
    compute_reachability(&ginitial_state);

    //#endif
    //  }

#ifdef __PARSER_ONLY__
    if (TRUE)
      {
	printf("\n\nDomain and problem analysis successfully ended\n");
	
	if (GpG.compile)
	  {
	    store_compiled_domain("compiled-domain.pddl");
	    store_compiled_problem("compiled-problem.pddl");
	  }
	
	fflush(stdout);
	exit(0);
      }
#endif


  if (!are_goal_reachable_and_non_mutex ()) {

    if (!GpG.inst_duplicate_param)
      {
#ifdef __MY_OUTPUT__
	printf("\n%s: create_final_goal_state(): goal can be simplified to FALSE. \n    Please run %s with option  '-inst_with_contraddicting_objects' \n\n", NAMEPRG, NAMEPRG);
#else
	printf("\nGoals of the planning problem can not be reached.\nPlease try to run with '-inst_with_contraddicting_objects'\n\n");
#endif
      }    
    else 
      {
	printf ("\nThe problem is unsolvable since at the fixpoint level the goals are mutex or not reachable\n\n");   
      }

    GpG.num_solutions++;

    store_plan(-1.0);

    exit (0);
  }

  times (&end);
  TIME (gmutex_num_time);

  if (DEBUG1)
    printf("\n   --> Compute mutex between numeric facts TOTAL TIME: %12.2f\n",gmutex_num_time);
  if (DEBUG2)
    print_mutex_result ();



  if (DEBUG0 && !DEBUG1) {
    printf ("done");
    fflush (stdout);
  }

  times (&start);

  if (DEBUG6 && !GpG.lowmemory)
    print_matrs ();

  gmutex_total_time = gmutex_ft_time + gmutex_ops_time + gmutex_num_time;

  printf ("\n");

  if (strlen (gcmd_line.sol_file_name) > 0)
    load_pddl2_plan (sol_file, &GpG.gplan_actions, 0);


  GpG.max_num_actions = gnum_ef_conn;
  GpG.max_num_facts = max_state_facts = gnum_ft_conn;
  GpG.max_num_ft_block = gnum_ft_block;



  /***********************************************************
   * we are finally through with preprocessing and can worry *
   * about finding a plan instead.                            *
   ***********************************************************/


  /* another quick preprocess: approximate goal orderings and split
   * goal set into sequence of smaller sets, the goal agenda
   */

  source_to_dest (&(gplan_states[0]), &ginitial_state);
  source_to_dest (&current_start, &ginitial_state);
  source_to_dest (&current_end, &ggoal_state);

  remove_unappliable_actions ();

  //  if (GpG.search_type == LOCAL && GpG.numrestart > 0 && GpG.numtry > 0) {
      k = MAX (GpG.input_plan_lenght,GpG.fixpoint_plan_length+1);
      for (i = 0; i < k; i++)
	{
	  if (i <GpG.fixpoint_plan_length )
	    create_vectlevel (0);
	  else
	    create_vectlevel (1);
	}
      allocate_data_for_local_search();
      create_all_min_array ();
      gmutex_level=GpG.fixpoint_plan_length;
      GpG.curr_goal_state =  &current_end;

      //    }

#ifdef __cplusplus 

      if(GpG.OR_perconds>0)
	{
	  int curr_init=GpG.initialize;
	  GpG.initialize=INIT_EMPTY_PLAN;
	  initialize(&ginitial_state,&ggoal_state, 0);
	  //	  GpG.info_search=5;
	  double exec_simul_cost=define_adaptation_cost_for_input_plan(GpG.fixpoint_plan_length, &ggoal_state,GpG.gplan_actions, 10000.0);
	  GpG.initialize=curr_init;
	  //exit(1);
	}
#endif

  if (DEBUG1) {
    printf ("\n\nTime spent for preprocessing:");
    printf ("\n Instantiating:     %7.2f seconds", gtempl_time + greach_time + grelev_time + gconn_time + gsearch_time);
    printf ("\n Mutex relations:   %7.2f seconds", gmutex_total_time);
    printf ("\n Numeric relations: %7.2f seconds", gnum_time);
  }
  if (DEBUG0) {
    times (&glob_end_time);

#ifndef __WINLPG__
    gtotal_time = (float) ((glob_end_time.tms_utime - glob_start_time.tms_utime + glob_end_time.tms_stime - glob_start_time.tms_stime) / 100.0);
#else
    gtotal_time = DeltaTime(glob_start_time, glob_end_time);
#endif

    printf ("\nPreprocessing total time: %.2f seconds",gtotal_time);
  }


#ifdef __TEST__
  printf ("\n\ninitial state is:\n\n");
  for (i = 0; i < ginitial_state.num_F; i++)
    {
      print_ft_name (current_start.F[i]);
      printf ("\n");
    }
  printf ("\n\ngoal state is:\n\n");
  for (i = 0; i < current_end.num_F; i++)
    {
      print_ft_name (current_end.F[i]);
      printf ("\n");
    }

  for (i = 0; i < gnum_op_conn; i++)
  {
    print_op_name(i);
    printf(" -- %f \n", get_action_cost (i,-1,NULL));
  }
#endif
  if (GpG.do_best_first == TRUE && GpG.numrestart==0)
    GpG.search_type=BEST_FIRST;

  /*
   * Ricerca della soluzione, fino a quando non sia stata raggiunta la condizione di terminazione
   * (definita in 'is_term_condition_reached')
   **
   * Search untill it is not reached termination condition (given by the function 'is_term_condition_reached')
   */

  times (&search_start);

  while(!is_terminated)
    {
      /* 
       * Diversi tipi di ricerca locale
       **
       * Different types of local search
       */
      switch(GpG.search_type)
	{
	  /* 
	   * Ricerca locale utilizzata per default in LPG
	   **
	   * Local Search usually used in LPG
	   */
	case LOCAL:
	  /* 
	   * Inizia la ricerca locale
	   **
	   * Do Local Search
	   */
	  LocalSearch (&current_start, &current_end, &subplan_actions);
	  
	  /* 
	   * Memorizza il piano in GpG.gplan_actions
	   **
	   * Store plan in GpG.gplan_actions
	   */
	  GpG.gplan_actions = subplan_actions;
	  subplan_actions = NULL;
	  
	  /* 
	   * Controlla se è stata raggiunta la condizione di terminazione
	   **
	   * Control if the termination condition is reached
	   */
	  is_terminated= is_term_condition_reached();

	  break;
	  
	  /* 
	   * Ricerca Best First (J. Hoffmann FF-v2.3)
	   **
	   * Best First Search implemented by J. Hoffmann (FF-v2.3)
	   */
	case BEST_FIRST:

	  if (DEBUG0)
	    printf("\n\nSwitching to Best-first Search: ( code from J. Hoffmann's package FF-v2.3 ) \n");
	  check_time_and_length (0);	/* con zero non controlla la lunghezza */
	  /* Return solution if reached, FALSE otherwise
	   */
	  found_plan = do_best_first_search ();
	  
	  /* If a solution was found in best first search print solution
	   */
	  if (found_plan)
	    {
	      
#ifdef __MY_OUTPUT__
	      printf ("\nFFGGHH::%.2f::%d\n", gtotal_time, gnum_plan_ops);
#endif
	      reset_plan(GpG.curr_plan_length);
	      GpG.store_plan = TRUE;
	      GpG.temporal_plan = TRUE;

	      printf("\nimproving the parallelism of the plan");
	      build_temporal_plan ();
	      optimize = is_plan_better();
	      if (optimize)
		{
		  save_curr_plan (GpG.curr_plan_length, &subplan_actions);

		  GpG.num_solutions++;

		  times (&search_end);

		  plan_time = DeltaTime (search_start, search_end);


		  times (&end);
		  TIME (gsearch_time);
		  
		  times (&end);
		  times (&glob_end_time);

#ifndef __WINLPG__
		  gtotal_time = (float) ((glob_end_time.tms_utime - glob_start_time.tms_utime + glob_end_time.tms_stime - glob_start_time.tms_stime) / 100.0);
#else
		  gtotal_time = DeltaTime(glob_start_time, glob_end_time);
#endif

		  store_plan(plan_time);

#ifdef __ONLY_ONE_PLANNER__
		  GpG.save_quality_plan_with_different_name = 1;
#endif
			  

#ifndef __ONLY_ONE_PLANNER__
		  if (GpG.mode == QUALITY)
#endif
		    {
		      GpG.time_lastsol = gtotal_time;
		      plan_info_for_quality_mode.num_actions = GpG.num_actions;
		      plan_info_for_quality_mode.total_cost = GpG.total_cost_from_metric;
		      plan_info_for_quality_mode.total_time = GpG.total_time; 
		      plan_info_for_quality_mode.metricvalue = GpG.total_cost_from_metric * GpG.orig_weight_cost + GpG.total_time * GpG.orig_weight_time;
		    }
		      
		  store_plan(plan_time);
		  
#ifdef __ONLY_ONE_PLANNER__
		  GpG.save_quality_plan_with_different_name = 2;
#endif

		  
		  
		  if (GpG.mode == INCREMENTAL)
		    {
		      printf ("\n\nSolution number: %d\nTotal time:      %.2f\nSearch time:     %.2f\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f", GpG.num_solutions, gtotal_time, gsearch_time, GpG.num_actions, GpG.total_cost, GpG.total_time, GpG.total_cost * GpG.orig_weight_cost + GpG.total_time * GpG.orig_weight_time );
		    }
		  else
		    {
		      printf ("\n\nSolution found: \nTotal time:      %.2f\nSearch time:     %.2f\nActions:         %d\nExecution cost:  %.2f\nDuration:        %.3f\nPlan quality:    %.3f", gtotal_time, gsearch_time, GpG.num_actions, GpG.total_cost, GpG.total_time, GpG.total_cost * GpG.orig_weight_cost + GpG.total_time * GpG.orig_weight_time );
		    }
		  
		  if (!GpG.noout)
		    {
		      printf ("\n     Plan file:");
		      if (GpG.out_file_name)
			printf ("       %s_%d.SOL", gcmd_line.out_file_name,
				GpG.num_solutions);
		      
		      else
			printf ("       plan_%s_%d.SOL", gcmd_line.fct_file_name,
				GpG.num_solutions);
		    }
		}
	      else
		if (DEBUG0 && !DEBUG1)
		  {
		    if (optimize==FALSE)
		      printf (" found solution of bad quality.");
		  }


	      if (DEBUG1)
		output_planner_info ();

	      /* Control if the termination condition is reached
	       */
	    }

	  is_terminated= is_term_condition_reached();
	  break;
	  
	  /*
	   * Ricerca hill climbing
	   **
	   * Hill Climbing Search
	   */
	case   HILL_CLIMBING:
	  
	  if (do_enforced_hill_climbing (&current_start, &current_end))
	    source_to_dest (&current_start, &(gplan_states[gnum_plan_ops]));
	  /* Control if the termination condition is reached
	   */
	  is_terminated= is_term_condition_reached();
	  break;
	  
	default:
	  /* Control if the termination condition is reached
	   */
	  is_terminated= is_term_condition_reached();
	  break;

	}  

	  
      if (DEBUG2)
	{
	  printf ("\n\nInitial state is:\n\n");
	  for (j = 0; j < ginitial_state.num_F; j++)
	    {
	      print_ft_name (current_start.F[j]);
	      printf ("\n");
	    }
	  printf ("\n\nGoal state is:\n\n");
	  for (j = 0; j < current_end.num_F; j++)
	    {
	      print_ft_name (current_end.F[j]);
	      printf ("\n");
	    }
	}
    }


#ifdef __MY_OUTPUT__

  print_num_tuple();

#endif



  printf ("\n\n");
  exit (0);

}
