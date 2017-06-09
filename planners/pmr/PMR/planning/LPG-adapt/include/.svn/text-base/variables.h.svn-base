#ifndef __VARIEBLES_H

#define __VARIEBLES_H

/* in ff.h */
/******************************************************************************
 *                             GLOBAL VARIABLES                               *
 ******************************************************************************/

/**
 * PARSING
 **/

/* used for pddl parsing, flex only allows global variables */
int gbracket_count;
char *gproblem_name;

/* The current input line number */
int lineno = 1;

/* The current input filename */
char *gact_filename;

/* The pddl domain name */
char *gdomain_name = NULL;

/* loaded, uninstantiated operators */
PlOperator *gloaded_ops = NULL;
PlOperator *gloaded_pl2ops = NULL;

/* loaded, uninstantiated derived predicates */
PlOperator *gderived_predicates = NULL;
PlOperator *gderived_pl2predicates = NULL;

/* stores initials as fact_list */
PlNode *gorig_initial_facts = NULL;

/* not yet preprocessed goal facts  */

PlNode *gorig_goal_facts = NULL;

/* metric for the plan*/
PlNode *gmetric_exp = NULL;

/* axioms as in UCPOP before being changed to ops */
PlOperator *gloaded_axioms = NULL;

/* the types, as defined in the domain file */
TypedList *gparse_types = NULL;

/* the constants, as defined in domain file */
TypedList *gparse_constants = NULL;

/* the predicates and their arg types, as defined in the domain file */
TypedListList *gparse_predicates = NULL;

/* PDDL2--*/
TypedListList *gparse_functions = NULL;

/* the objects, declared in the problem file */
TypedList *gparse_objects = NULL;

/* connection to instantiation ( except ops, goal, initial ) */

/* all typed objects  */
FactList *gorig_constant_list = NULL;

/* the predicates and their types */
FactList *gpredicates_and_types = NULL;

FactList *gfunctions_and_types = NULL;



/**
 * INSTANTIATING
 **/

/* global arrays of constant names,
 *               type names (with their constants),
 *               predicate names,
 *               predicate aritys,
 *               defined types of predicate args
 */
Token gconstants[MAX_CONSTANTS];
int gnum_constants = 0;
Token gtype_names[MAX_TYPES];
int gtype_consts[MAX_TYPES][MAX_TYPE];
Bool gis_member[MAX_CONSTANTS][MAX_TYPES];
int gtype_size[MAX_TYPES];
int gnum_types = 0;

Token gpredicates[MAX_PREDICATES];
int garity[MAX_PREDICATES];
int gpredicates_args_type[MAX_PREDICATES][MAX_ARITY];
int gnum_predicates = 0;

int gnum_derived_predicates = 0;
int gpredicates_type[MAX_PREDICATES];

/*miamod per functions*/
Token gfunctions[MAX_FUNCTIONS];
int gfunarity[MAX_FUNCTIONS];
int gfunctions_args_type[MAX_FUNCTIONS][MAX_ARITY];
int gnum_functions = 0;
/*finemiamod per functions*/


/* the domain in integer (Fact) representation
 */
Operator_pointer goperators[MAX_OPERATORS];

Operator_pointer gderivedpred[MAX_DERIVED_PREDICATES];

int gnum_operators = 0;
Fact gfull_initial[MAX_INITIAL];
int gnum_full_initial = 0;

/* Timed initial facts
 */
// Conta i timed initial facts 
int gnum_tmd_init_fcts = 0;
/* Intervalli associati ai timed facts
 */

int *gnum_tmd_interval = NULL;
int *gnum_tmd_interval_block = NULL;

Timed_fct **gtimed_fct_vect = NULL;
int **gtimed_fct_bitarray = NULL;

int *ginterval_idx = NULL;
int gnum_timed_facts = 0;

int *temp_PC_int = NULL;

neighb shifted_act[MAX_SHIFTED];
int num_shifted_act = 0; 

//NumVar *gfullnum_initial[MAX_NUM_INITIAL];
NumVar **gfullnum_initial = NULL;
int gnum_fullnum_initial = 0;
int gnum_fullnum_blocks;

int max_num_value = MAX_NUM_INITIAL;
int max_fullnum_initial = MAX_NUM_INITIAL;

int gnum_comp_var = 0;
int gnum_comp_var_effects = 0;
int gnum_block_compvar;
int *gis_inertial = NULL;
int goptimization_exp = -1;
int *gis_not_appliable;

float gmax_cpu_time_for_quality_mode;

WffNode *ggoal = NULL;


/* stores inertial - information: is any occurence of the predicate
 * added / deleted in the uninstantiated ops ?
 */
Bool gis_added[MAX_PREDICATES];
Bool gis_deleted[MAX_PREDICATES];



/* splitted initial state:
 * initial non static facts,
 * initial static facts, divided into predicates
 * (will be two dimensional arrays, allocated directly before need)
 */
Facts *ginitial = NULL;
int gnum_initial = 0;
Fact **ginitial_predicate;
int *gnum_initial_predicate;

PlNode *timed_nodes = NULL;
WffNode *timed_wff = NULL;
TimedFtConn *ginitial_timed = NULL;
int gnum_timed_initial = 0;

/* the type numbers corresponding to any unary inertia
 */
int gtype_to_predicate[MAX_PREDICATES];
int gpredicate_to_type[MAX_TYPES];

/* (ordered) numbers of types that new type is intersection of
 */
TypeArray gintersected_types[MAX_TYPES];
int gnum_intersected_types[MAX_TYPES];



/* splitted domain: hard n easy ops
 */
Operator_pointer *ghard_operators;
int gnum_hard_operators;
NormOperator_pointer *geasy_operators;
int gnum_easy_operators;

/* derived predicates
 */
Operator_pointer *ghard_derivedpred;
int gnum_hard_derivedpred;
NormOperator_pointer *geasy_derivedpred;
int gnum_easy_derivedpred;



/* so called Templates for easy ops: possible inertia constrained
 * instantiation constants
 */
EasyTemplate *geasy_templates = NULL;
int gnum_easy_templates = 0;

EasyTemplate *gsuspected_easy_templates = NULL;
int gnum_suspected_easy_templates = 0;

/* templates per i Predicati derivati
 */
EasyTemplate *geasy_dp_templates = NULL;
int gnum_easy_dp_templates = 0;


/* first step for hard ops: create mixed operators, with conjunctive
 * precondition and arbitrary effects
 */
MixedOperator *ghard_mixed_operators;
int gnum_hard_mixed_operators;



/* hard ''templates'' : pseudo actions
 */
PseudoAction_pointer *ghard_templates;
int gnum_hard_templates;

/* hard templates per i Predicati derivati
 */
PseudoAction_pointer *ghard_dp_templates;
int gnum_hard_dp_templates;

/* store the final "relevant facts"
 */
Fact grelevant_facts[MAX_RELEVANT_FACTS];
int gnum_relevant_facts;
int gnum_pp_facts;



/* the final actions and problem representation
 */
Action *gactions;
int gnum_actions;
State ginitial_state;
State ggoal_state;

/* Predicati derivati
 */
Action *gdpactions;
int gnum_dp_actions;

path_set  gdp_path;

indexed_int_list *numvar_hash_table[HASH_SIZE];
int cvar_hash_table[HASH_SIZE];
int cvar_hash_table_effects[HASH_SIZE];
int tot = 0, complete = 0;
CompositeNumVar *gcomp_var_effects;

//char *lvar_names[MAX_VARS];
//int lvar_types[MAX_VARS];
bit_table l_vals, lstar_vals, r_vals, tested_vars;

/* for facts and mutex
*/
int *F;			/*[MAX_RELEVANT_FACTS/32+1]; */

/* Variabili per il calcolo delle mutex */
dme *deleted_me;
int num_dme = 0;
int max_dme = 500;

/*  Gestione delle azioni spezzate */
int max_num_efconn = 0;
int gextended_ef_conn = 0;
int gextended_ef_block = 0;
int max_num_ftconn = 0;
int gextended_ft_conn = 0;

int max_state_facts = 0;

const char *goperator_table[] = {
  "MUL_OP",
  "DIV_OP",
  "MINUS_OP",
  "UMINUS_OP",
  "PLUS_OP",

  "FIX_NUMBER",
  "VARIABLE_OP",

  "INCREASE_OP",
  "DECREASE_OP",
  "SCALE_UP_OP",
  "SCALE_DOWN_OP",
  "ASSIGN_OP",

  "LESS_THAN_OP",
  "LESS_THAN_OR_EQUAL_OP",
  "EQUAL_OP",
  "GREATER_THAN_OP",
  "GREATER_OR_EQUAL_OP",

  "MINIMIZE_OP",
  "MAXIMIZE_OP"
};




/**
 * CONNECTIVITY GRAPH
 **/


/* one ops (actions) array ... */
OpConn *gop_conn;
int gnum_op_conn;



/* one effects array ... */
EfConn *gef_conn;
int gnum_ef_conn;

int gfirst_suspected_ef_conn;

/* one conditional effects array ... */
CondEfConn * gcondef_conn;
int gnum_condef_conn;



/* one facts array. */
FtConn *gft_conn;
int gnum_ft_conn;

/* Derived predicates (op) final representation */
DpConn *gdp_conn;
int gnum_dp_conn;

FtConn *gnoop_conn;

int gnum_ft_block;
int gnum_ef_block;

int gnum_dp_block;

int gnum_base_ft_conn = 0;
int gnum_base_ft_block = 0;



/**
 * FF SEARCHING NEEDS
 **/


/* byproduct of fixpoint: applicable actions */
int *gA;
int gnum_A;



/* communication from extract 1.P. to search engines:
 * 1P action choice */
int *gH;
int gnum_H;



/* the effects that are considered true in relaxed plan */
int *gin_plan_E;
int gnum_in_plan_E;


/* always stores (current) serial plan */
int gplan_ops[MAX_PLAN_LENGTH];
int gnum_plan_ops = 0;
int gtot_plan_ops = 0;


/* stores the states that the current plan goes through
 * ( for knowing where new agenda entry starts from ) */
State gplan_states[MAX_PLAN_LENGTH + 1];

PlanAction *subplan_actions = NULL;



/**
 * LPG LOCAL SEARCH
 **/

last_cost_list last_best_act_cost = NULL;

int num_try;
int return_count;
unsigned int seed;


constraints_list treated_c_l[MAX_FALSE];
constraints_list unsup_fact[MAX_FALSE];
constraints_list unsup_num_fact[MAX_FALSE];
constraints_list unsup_tmd_facts[MAX_FALSE];

neighb_list neighb_vect[MAX_MAX_NODES];
int num_neighborhood;

/* final sort of actions in temp_vect */  
int *pos_temp_vect;//[MAX_MAX_NODES];

def_level * vectlevel[MAX_PLAN_LENGTH + 1];
def_level * temp_vectlevel[MAX_PLAN_LENGTH + 1];


ActNode_list *remove_act_chain; //[MAX_PLAN_LENGTH];
ActNode_list *remove_act_chain_next_step;
int ind_remove_act_chain;
int ind_remove_act_chain_next_step;

/* Used for action <--> noop mutex
 */
noop_not_in *noop_free_list; 

unsigned long tot_alloc_mem_size;

char fct_file[MAX_LENGTH];

/* Statistical data about Lagrange multipliers
*/
#ifdef __STATISTIC_LM__

 /* global variables used to compute average, total maximum value, minimum value of
   Lagrange multipliers for preconditions and mutex 
  */
 
 float average_prec_final = 0.0;
 float average_me_final = 0.0;
 float var_prec_final = 0.0;
 float var_me_final = 0.0;

 float lm_prec_min_final,lm_prec_max_final,lm_me_min_final,lm_me_max_final;
 
/*Vars used for files
 */

 FILE *file_average_prec;
 FILE *file_var_prec;
 FILE *file_average_me;
 FILE *file_var_me;

#endif // end __STATISTIC_LM__



/**
 * COMPUTE MUTEX
 **/


/* Number of set mutex and level
 */
int gnum_mutex;
int gmutex_level;
/* Total number of fact-action mutex, action-fact mutex, 
   action-action mutex, fact-fact mutex 
 */
int total_ft_ef_mutex = 0;
int total_ef_ft_mutex = 0;
int total_ef_ef_mutex = 0;
int total_ft_ft_mutex = 0;

/* fact-fact mutex matrix
 */
int **FT_FT_mutex = NULL;
/* fact-action mutex matrix
 */
int **FT_EF_mutex = NULL;
/* action-action mutex matrix
 */
int **EF_EF_mutex = NULL;
/* action-fact mutex matrix
 */
int **EF_FT_mutex = NULL;


/**
 * NUMERIC PLANNING
 **/

/* Structure for numeric vars
 */

CompositeNumVar *gcomp_var;
float  *gcomp_var_value;
float  *gcomp_var_value_before;

int value_start_size = 0;
float  *gcomp_var_value_start = NULL;
int temp_value_size = 0;
float  *temp_value = NULL;


/**
 * TEMPORAL PLANNING
 **/

char **mat_ord;
ActNode_list *act_ord_vect;
int num_act_ord;
short *prop_level_index;

float slack_vect[MAX_PLAN_LENGTH + 1];

int *splitted_level;

/**
 * CPU TIME MANAGEMENT
 **/

#ifndef __WINLPG__
struct tms start_time;
struct tms glob_start_time;
struct tms glob_end_time;
struct tms search_start;
struct tms search_end;
#else
clock_t cpu_start_time;
clock_t glob_start_time;
clock_t glob_end_time;
clock_t search_start;
clock_t search_end;
#endif

float gtotal_time;
char gcomm_line[MAX_LENGTH * 2];
char gops_file_name[MAX_LENGTH];
char gfct_file_name[MAX_LENGTH];
char gpath_sol_file_name[MAX_LENGTH];
char glpg_path[MAX_LENGTH];


/**
 * MISCELLANEUS
 **/

/* used to time the different stages of the planner
 */
float gtempl_time = 0, greach_time = 0, grelev_time = 0, gconn_time = 0, 
  gnum_time = 0, gmutex_total_time = 0, gmutex_ft_time = 0, 
  gmutex_ops_time = 0, gmutex_num_time = 0;
float gsearch_time = 0;

float build_ad_time, fixpoint_time;

/* the command line inputs
 */
struct _command_line gcmd_line;

/* number of states that got heuristically evaluated
 */
int gevaluated_states = 0;

/* maximal depth of breadth first search
 */
int gmax_search_depth = 0;

char temp_name[256];

char temp_name2[256];

node_cost *fact_costs; //[MAX_MAX_NODES];
/* Bitvector used by remove_temp_action to find facts that 
   become TRUE after it is removed
*/
int *new_true_facts;
/* Bitvector used by remove_temp_action to find facts that 
   become FALSE after it is removed
*/
int *new_false_facts;	

/* TRUE if termination condition is reached
 */
Bool is_terminated=FALSE;

/* one facts array in conditional action.*/
CondFtConn  *gcondft_conn;
int gnum_condft_conn;


/* In lpg.h */

struct _HVAR Hvar;
struct _NUMERIC  Numeric;
struct _GPG GpG;

#ifdef __STATISTICS__
    struct _STAT Statistics;
#endif

struct _PLAN_INFO_FOR_QUALITY_MODE  plan_info_for_quality_mode;
struct _LOCAL_SEARCH local_search;

// Used in inst_utils

/* local globals for this part
 */
char *lvar_names[MAX_VARS];
int lvar_types[MAX_VARS];

token_list inertial_facts;
int correct_vect[MAX_PLAN_LENGTH + 1];

IntList *old_list = NULL;

float *val_max = NULL, *val_min = NULL;



#ifdef  __ADAPT__

/* in strips-adapt.h */



float total_parsing_time;


struct __CRON  cron; 
__ADAPT adapt;

__causal_link_list causal_links;


__gpg_struct *adapt_ft_conn;
__gpg_struct *adapt_ef_conn;



__hash_table *cbp_ft_conn[EHC_HASH_BITS];
__hash_table *cbp_ef_conn[EHC_HASH_BITS]; 

__adapt_action *stored_act_vect[MAX_PLAN_LENGTH + 1];
__adapt_action *act_vect[MAX_PLAN_LENGTH + 1];



// LORINI

int gnum_inertials;
State gpg_store_initial_facts;
State gpg_store_goal_facts;
State cbp_facts;
State cbp_goals;  
//DEF in adj.c 


int gcbp_level;
int gline_pointer;
float gmin_match_value;


__candidates *candidates_cbp_facts;
//candidates *candidates_initial_facts;
int *bit_vect_candidate;
int *bit_vect_cbp_facts;
int *bit_vect_prob_facts;
int gnum_candidates_goal_facts;
int gnum_candidates_initial_facts;
int upper_bound_num_actions;
float upper_bound_cost_actions;
int max_bound_num_actions;
float max_bound_cost_actions;

int gnum_actual_candidates_goal_facts;
int gnum_actual_candidates_initial_facts;

/* planners.h */


strplanner planner;

char PLANNER_COMM_LINE[MAX_LENGTH];
char TEMP_DIR [MAX_LENGTH];

float gff_time;



/* constraints.h */
invtype Head;

/* for VAL*/
ostream * VAL::report = &cout;


#endif


#endif
