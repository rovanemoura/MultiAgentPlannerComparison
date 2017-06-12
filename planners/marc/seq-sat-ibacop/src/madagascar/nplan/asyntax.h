

  /*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

/* An atomic fact (or schema) P(t1 t2 t3 ... tn) */

typedef struct _intlist { int hd; struct _intlist *tl; } intlist;
typedef struct _stringlist { char *hd; struct _stringlist *tl; } stringlist;
typedef struct _typedvarlist { int v; int t; struct _typedvarlist *tl; } typedvarlist;


typedef enum {
  STRUE,SFALSE,
  Spatom, Snatom,
  Sconj, Sdisj,
  Sforall, Sforsome,
  Seq, Sneq } Sfmatype;

typedef struct _atom {
  int pred;
  intlist *params;
} atom;


/* Schematic formulae */

typedef struct _Sfma {
  Sfmatype t;
  union {
    atom *a;
    typedvarlist *ss; /* parameters for quantification */
    int p1;
    struct _Sfmalist0 { struct _Sfma *hd; struct _Sfmalist0 *tl; } *juncts;
  };
  int p2;
  struct _Sfma *f;
} Sfma;


typedef enum { SEpatom, SEnatom, SEwhen, SEconj, SEforall } Sefftype;

/* schematic effects */

typedef struct _Seff {
  Sefftype t;
  union {
    atom *a;
    Sfma *cond; /* condition for when */
    struct _Sefflist0 { struct _Seff *hd; struct _Sefflist0 *tl; } *juncts; /* list of effects for conj */
    typedvarlist *ss; /* parameter in forall quantification */
  };
  struct _Seff *effect; /* effect for when, forall */
} Seff;

/* Lists */

typedef struct _Sfmalist { Sfma *hd; struct _Sfmalist* tl; } Sfmalist;
typedef struct _Sefflist { Seff *hd; struct _Sefflist* tl; } Sefflist;
typedef struct _atomlist { atom *hd; struct _atomlist* tl; } atomlist;

Sfmalist *Sfmacons(Sfma *,Sfmalist *);
Sefflist *Seffcons(Seff *,Sefflist *);
intlist *intcons(int,intlist *);
atomlist *atomcons(atom *,atomlist *);

#define EMPTYLIST NULL

/* Domain */

void storedomain(int);
void checkdomain(int);
char *domainname();

void storeconstants(typedvarlist *);
void storetypes(typedvarlist *);
void storepredicates();

void checkrequirements(intlist *);


/* Problem */

void addproblem(int);
char *problemname();

void storeinit(atomlist *);
void storegoal(Sfma *);
atomlist *Sinit;

/* Type variables */

int UNIVTYPE;
typedvarlist *withtype(int,intlist *);
typedvarlist *TVappend(typedvarlist *,typedvarlist *);


/* An operator schema */

typedef struct _Saction {
  int name;
  typedvarlist *params;
  Sfma *precon;
  Seff *effect;
} Saction;

int maxSActions;
Saction *Sactions;
int nOfSActions;
void checkSactionsSize();

atom *newatom(int,intlist *);

typedef struct _obtype {
  int typename;
  intlist *elements;
} obtype;

void storeobjects(typedvarlist *);

void showatom(atom *);

int linenumber;
char *errorstring;

void readfile();

/* Constructors */

/* schematic effects */
Seff* Seffconjunction(Sefflist *);
Seff* Seffwhen(Sfma*, Seff*);
Seff* Seffforall(typedvarlist *,Seff *);
Seff* SPeffatom(atom *);
Seff* SNeffatom(atom *);

/* schematic formulas */
Sfma *Sfalse();
Sfma *Strue();
Sfma *Sconjunction(Sfmalist *);
Sfma *Sdisjunction(Sfmalist *);
Sfma *Satom(atom *);
Sfma *Sneg(Sfma *);
Sfma *SfmaEQ(int,int);
Sfma *Sfmaforall(typedvarlist *,Sfma *);
Sfma *Sfmaforsome(typedvarlist *,Sfma *);

/* create new schematic op */
void addnewaction(int);
void addactionparameters(typedvarlist *);
void addactionprecond(Sfma *);
void addactioneffect(Seff *);

/* Grounding */

void preprocessoperators();
void groundoperators();
void simplifysillyconditionals();
