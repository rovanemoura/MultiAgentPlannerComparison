
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

/* ground formulae */

typedef enum { patom, natom, conj, disj, TRUE, FALSE } fmatype;

typedef struct _fma {
  fmatype t;
  union {
    int a;
    struct _fmalist0 { struct _fma *hd; struct _fmalist0 *tl; } *juncts;
  };
} fma;


/* ground effects */

typedef struct _eff {
  fma *condition;
  intlist *poseffects;
  intlist *negeffects;
  struct _eff *tl;
} eff;

typedef struct _fmalist { fma *hd; struct _fmalist* tl; } fmalist;

fmalist *fmacons(fma *,fmalist *);

fma *Fconj(fmalist *);
fma *Fdisj(fmalist *);
fma *Fconj2(fma *,fma *);
fma *Fdisj2(fma *,fma *);
fma *Fatom(int);
fma *Fnatom(int);
fma *Fneg(fma *);
fma *Fimpl(fma *,fma *);
fma *Ftrue();
fma *Ffalse();

typedef struct _action {
  intlist *name;
  fma *precon;
  eff *effects;
} action;

int nOfActions;
int maxActions;
action *actions;

int disjunctivep(fma *);
void initactions();

int ptruep(fma *,int *);	/* Test for truth of a formula in a state. */
int execute(int,int *,int *);	/* Execute an action. */
void executeNOprecon(int,int *,int *);	/* Execute an action without testin precondition. */

int fprintactionname(FILE *,int);
int printactionname(int);
int fprintactionnameIPC(FILE *,int);
int printactionnameIPC(int);
void printaction(int);

typedef enum { STRIPS, Conjunctive, GeneralPDDL } syntacticclass;

syntacticclass actionclass(int);

syntacticclass goalclass();

int *initialstate;
fma *goal;
int goalisdisjunctive;

void simplifyoperators();
void simplifyoperatorsstatic();

ordintset *effectoccP; /* operators in which var is a positive effect */
ordintset *effectoccN; /* operators in which var is a negative effect */
ordintset *forcedeffectoccP; /* operators in which var is a positive effect */
ordintset *forcedeffectoccN; /* operators in which var is a negative effect */


ordintset *preconoccN; /* operators in which var is negative in precondition */
ordintset *preconoccP; /* operators in which var is positive in precondition */

ordintset *condocc; /* operators in which var occurs in a condition for effect */

ordintset *forcedeffectsP; /* variables the operator always makes true */
ordintset *forcedeffectsN; /* variables the operator always makes false */

ordintset *preconP; /* variable that must be true for operator */
ordintset *preconN; /* variable that must be false for operator */

/* Same as preconP and preconN, but including lits inferred with invariants. */
ordintset *necessarypreconP; /* variable that must be true for operator */
ordintset *necessarypreconN; /* variable that must be false for operator */

ordintset *necessarypreconofP; /* operators in which atom is a nec precon */
ordintset *necessarypreconofN; /* operators in which atom is a nec precon */

int canmaketrue(int,int);
int isaffectedby(int,int);
int opaffects(int,int);
int opaffectsinstate(int *,int,int);
int parallel(int,int);
int Lparallel(int,int);

void findoccurrences();

void sortactions();

void collectliterals(ordintset,int);

void eliminatestaticvariables();

void mergecontras();

int **AeffectoccP; /* operators in which var is a positive effect */
int **AeffectoccN; /* operators in which var is a negative effect */
int **ApreconP; /* variable that must be true for operator */
int **ApreconN; /* variable that must be false for operator */
int **AforcedeffectsP; /* variables the operator always makes true */
int **AforcedeffectsN; /* variables the operator always makes false */
int **AnecessarypreconP; /* variable that must be true for operator */
int **AnecessarypreconN; /* variable that must be false for operator */
int **ApreconoccN; /* operators in which var is negative in precondition */
int **ApreconoccP; /* operators in which var is positive in precondition */
int **Acondocc; /* operators in which var occurs in a condition for effect */

void constructoperatorarrays();
