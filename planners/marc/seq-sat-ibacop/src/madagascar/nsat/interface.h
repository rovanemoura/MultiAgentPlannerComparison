
/*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

#if defined(__LP64__)
#define PTRINT long
#else
#define PTRINT int
#endif

#define SVAR(v) (v)
#define ACTVAR(a) ((a)+(sati->nOfSVars))
#define TVAR(v,t) ((v)+(t)*(sati->nOfVarsPerTime))

typedef struct _nintlist { int hd; struct _nintlist *tl; } nintlist;

typedef struct _variable {
  short val;		/* Current value of the variable */
#ifdef VSIDS
  short status;
	/* These are the status bits.
           bit 0      is phase there?
           bit 1      phase
           bit 2      dirty? (whether inferred with the goal clauses)
	*/
#endif
  PTRINT reason;		/* Reasons for current variable value */
  int dlevel;		/* Decision level of variables */
} variable;

typedef struct _literal {
  int *watches;
#ifdef VSIDS
  int score;		/* Scores for choosing decision variables */
#endif
} literal;

/* Heap for keeping track of the highest score variables. */

typedef struct {
  int k;
  int v;
} pair;

typedef struct _heap {
  int els;
  int maxels;
  pair *array;
} *heap;

typedef struct _gap {
  int lit;
  int t;
} gap;

typedef struct _undo {
  int level;
  int *addr;
  int value;
} undo;

#define MAXUCC 10000

typedef struct _satinstance {
  int id;	/* Unique integer id for the instance. */
  int value;
  int nOfVars;	/* Number of propositional variables */
  int nOfVarsPerTime;	/* Number of vars per time point */
  int nOfTPoints;	/* Number of time points */

  nintlist **l2its;	/* Array for 2-literal clauses (for all time points), non-changing */
  int **al2its;		/* Array for 2-literal clauses (for all time points), non-changing */

#ifdef SPREAD
  int **al2itsT;	/* Array for 2-literal clauses (for every time point), non-changing */
#endif

  literal *lits;	/* Data structure for literals */
  variable *vars;	/* Data structure for variables */

  int *initialunittable;
  int maxinitialunits;
  int initialunits;
#ifdef GLOBALUS
#else
  int *unitstack;	/* Stack of assignments made */
  int endunitstack,startunitstack;
#endif
  int nOfSVars;		/* Planning specific: number of state variables / t */
  int nOfActions;	/* Planning specific: number of actions / t */
  int decisions;	/* How many decision made. */
  int conflicts;	/* How many conflicts. */
  int decaysteps;	/* Counter for variable weight decay */
  int NofUCClauses;
  int complete;
  /* Fields for the heuristics */
  int pheuristic;	/* Which planning-based heuristic to use. */
  int heuristic;	/* Which branching heuristic to use. */
#ifdef VSIDS
  int *hindex;		/* Index of each variable in the heap. */
  heap scoreheap;	/* Literals ordered according to their score */
  int VSIDSround;	/* Alternation VSIDS, the planning heuristic. */
#endif
  int dlevel;		/* Current decision level of the SAT instance. */
  int UCC[MAXUCC];
  int Ngaps;		/* Number of active gaps */
  int MAXgaps;		/* Number of elements in the gaps array */
  gap *gaps;		/* The array for storing the gaps */
  int notcalledbefore;	/* true if solve0 not called yet. */
  /* Variables for the planning heuristic */
  int heuristic_mode;	/* This is 0 for actions, 1 for inertia, 2 for noops. */
  int heuristic_time;	/* The next two are for inertia and noops. */
  int heuristic_index;
} *satinstance;

typedef enum { InitC, FinalC, TransC } clausetype;

satinstance newinstance(int,int,int,int,int);
void freeinstance(satinstance);

void addnewclause(satinstance,int,clausetype,int); /* Number of literals in the clause (> 2) */
void addliteral(satinstance,int,int); /* Put literal to the given loc in the clause */
int *finishclause(satinstance); /* Finish adding the clause */

int add1clause(satinstance,int,clausetype); /* Add a 1-literal clause */
void add2clause(satinstance,int,int,clausetype); /* Add a 2-literal clause */

void instancecomplete();

void planningparameters(satinstance,int,int);
void setheuristic(satinstance,int);
void setpheuristic(satinstance,int);

int solve(satinstance);
int solve0(satinstance,int);

int noT2clauses;
