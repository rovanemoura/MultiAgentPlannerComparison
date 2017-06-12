
/*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

#include <stdio.h>
#include <stdlib.h>

#include "interface.h"
#include "clausesets.h"
#include "clausedb.h"
#include "printplan.h"

#include "../nplan/main.h"
#include "../nplan/asyntax.h"
#include "../nplan/ordintsets.h"
#include "../nplan/operators.h"
#include "../nplan/tables.h"

#include "../nplan/heuristics2.h"

#include <assert.h>
#include <malloc.h>

/* Interface to malloc */

void *ownalloc(int i) {
  void *ptr = malloc(i);
  check_malloc_success(ptr);
  return ptr;
}

#define noASSERTS
#define noDEBUG
#define noHARDDEBUG

#define MAXCCLENGTH 500000

#define min(a,b) (((a)<(b))?(a):(b))

#define max(a,b) (((a)>(b))?(a):(b))

#ifdef GLOBALUS
int GLOBALstacksize;
int *GLOBALunitstack;
int *GLOBALendunitstack,*GLOBALstartunitstack;
#endif

int *currentClause;

int conflicttype2lit;
int conflictl1,conflictl2,*conflictclause;

/* Initialize data structures used for all problem instances. */

#define MAXWASSEEN 10000000
int adjustheap;
int cround;	/* Counter which is incremented for new conflict clauses */
int wasseen[MAXWASSEEN];	/* Counter value when literal was last encountered. */

void init_clausesets() {
  int i;
  cround = 1;
  for(i=0;i<MAXWASSEEN;i++) wasseen[i] = 0;
#ifdef GLOBALUS
  GLOBALstacksize = 0x800000;
  GLOBALunitstack = (int *)ownalloc(sizeof(int) * GLOBALstacksize);
#endif
  adjustheap = 1;
}

#ifdef DEBUG
void printclause(satinstance sati,int *c) {
  while(*c != -1) {
    printf(" %i:",VAR(*c)); printTlit(sati,*c);
    c += 1;
  }
  printf("\n");
}
#endif

#ifdef ASSERTS
int isliteral(satinstance sati,int i) {
  if(VAR(i) < 0) return 0;
  if(VAR(i) >= sati->nOfVars) return 0;
  return 1;
}

int issvar(satinstance sati,int i) {
  int v;
  v = VAR(i) % sati->nOfVarsPerTime;
  if(v < sati->nOfSVars) return 1;
  return 0;
}

int isaction(satinstance sati,int i) {
  int v;
  v = VAR(i) % sati->nOfVarsPerTime;
  if((v >= sati->nOfSVars) && (v < sati->nOfSVars+sati->nOfActions)) return 1;
  return 0;
}

int isaux(satinstance sati,int i) {
  int v;
  v = VAR(i) % sati->nOfVarsPerTime;
  if(v >= sati->nOfSVars+sati->nOfActions) return 1;
  return 0;
}
#endif

#ifdef VSIDS
int isinheap(satinstance sati,int v) {
  return (sati->hindex[v] != -1);
}
#endif

nintlist *nintcons(int i,nintlist *l) {
  nintlist *l2;
  l2 = (nintlist *)malloc(sizeof(nintlist));
  l2->hd = i;
  l2->tl = l;
  return l2;
}

void printliteral(satinstance sati,int l) {
  printTlit(sati,l);
  /*
  int v;
  v = VAR(l)%sati->nOfVarsPerTime;
  if(v < sati->nOfSvars) {
    printatomi(v);
  } else if(v - sati->nOfSVars < sati->nOfActions) {
    printactionname(v-sati->nOfSVars);
  } else {
    printf("AUX%i",v->sati->nOfSVars-sati->nOfActions); 
  }
  printf("@%i",VAR(l)/sati->nOfVarsPerTime);
  */
}

/* Heap data structure */

heap heap_create(int);
void freeheap(heap);
int heap_emptyp(satinstance);
int heap_taketop(satinstance);
void heap_delete(satinstance,int);
void heap_insert(satinstance,int,int);
void heap_increment(satinstance,int);
void heap_increment_by(satinstance,int,int);
void heap_decay(satinstance);
void checkheapconsistency(satinstance);

/* Macros for accessing
   - the value of a propositional variable,
   - the reason for the value of a propositional variable,
   - the decision level of the propositional variable,
   - the status (mainly the phase) of a propositional variable.
*/

#define VALVALUE(v) (sati->vars[(v)].val)
#define VALREASON(v) (sati->vars[(v)].reason)
#define VALDLEVEL(v) (sati->vars[(v)].dlevel)
#define VALSTATUS(v) (sati->vars[(v)].status)

#if defined(__LP64__)
#define REASON_INPUT -2L
#define REASON_DECISION -1L
#else
#define REASON_INPUT -2
#define REASON_DECISION -1
#endif

/* The same, but when referring through the 'sativars' variable. */

#define SATIVARS sativars
#define SVALVALUE(v) (SATIVARS[(v)].val)
#define SVALREASON(v) (SATIVARS[(v)].reason)
#define SVALDLEVEL(v) (SATIVARS[(v)].dlevel)
#define SVALSTATUS(v) (SATIVARS[(v)].status)

/* Macros for accessing
   - the score of a literal,
   - the clauses in which the literal is watched,
   - two literal clauses -l V l' for a literal l.
*/

#define LITSCORE(l) (sati->lits[(l)].score)
#define LITWATCHES(l) (sati->lits[(l)].watches)
#ifdef SPREAD
#define LIT2LITS(l) (sati->al2itsT[(l)])
#endif

/* Macros for accessing the heap. */

#define HINDEX(v) (sati->hindex[(v)])

void report_malloc(char *arrayname,int size) {
#ifdef DEBUG
  printf("%s: %.2f MB\n",arrayname,((float)size)/1024.0/1024.0);
#endif
}

/*************************************************************************/
/** Initialize an instance                                              **/
/*************************************************************************/

satinstance newinstance(int id,int times,int varsPT,int svars,int actions) {
  int i;
  satinstance sati = (satinstance)ownalloc(sizeof(struct _satinstance));

  sati->value = -1;
  sati->id = id;

  sati->nOfSVars = svars;
  sati->nOfActions = actions;
  sati->nOfVars = (times-1)*varsPT+sati->nOfSVars;
  sati->nOfVarsPerTime = varsPT;
  sati->nOfTPoints = times;

#ifdef DEBUG
  printf("\n==================================================\n");
  printf("nOfSVars = %i\n",sati->nOfSVars);
  printf("nOfActions = %i\n",sati->nOfActions);
  printf("nOfVars = %i\n",sati->nOfVars);
  printf("nOfVarsPerTime = %i\n",sati->nOfVarsPerTime);
  printf("nOfTPoints = %i\n",sati->nOfTPoints);
#endif


#ifdef GLOBALUS
  if(GLOBALstacksize < sati->nOfVars) {
    do { GLOBALstacksize = GLOBALstacksize*2; }
    while(GLOBALstacksize < sati->nOfVars);
    GLOBALunitstack = (int *)realloc(GLOBALunitstack,sizeof(int) * GLOBALstacksize);
    check_malloc_success(GLOBALunitstack);
    printf("Increased stack size to %i Kbytes.\n",GLOBALstacksize >> 8);
  }
  GLOBALendunitstack = &(GLOBALunitstack[-1]);
  GLOBALstartunitstack = GLOBALunitstack;
#else
  sati->unitstack = (int *)ownalloc(sizeof(int) * sati->nOfVars);
  report_malloc("unitstack",sizeof(int) * sati->nOfVars);
  sati->endunitstack = -1;
  sati->startunitstack = 0;
#endif


  sati->maxinitialunits = sati->nOfSVars*2+2;
  sati->initialunittable = (int *)ownalloc(sati->maxinitialunits *sizeof(int));
  report_malloc("initialunittable",sati->maxinitialunits*sizeof(int));
  sati->initialunits = 0;

  sati->notcalledbefore = 1;

  sati->complete = 0;

  if(!noT2clauses) {
    sati->l2its = (nintlist **)ownalloc(sizeof(nintlist *) * sati->nOfVarsPerTime * 2);
    report_malloc("l2its",sizeof(nintlist *) * sati->nOfVarsPerTime * 2);
    for(i=0;i<sati->nOfVarsPerTime*2;i++) sati->l2its[i] = NULL;
  }

#ifdef SPREAD
  sati->al2itsT = (int **)ownalloc(sati->nOfVars * 2 * sizeof(int *));
#endif

  sati->lits = (literal *)ownalloc(sizeof(literal) * sati->nOfVars * 2);
  sati->vars = (variable *)ownalloc(sizeof(variable) * sati->nOfVars);
  report_malloc("lits",sizeof(literal) * sati->nOfVars * 2);
  report_malloc("vars",sizeof(variable) * sati->nOfVars);

  sati->decisions = 0;
  sati->conflicts = 0;

  sati->pheuristic = 0;
  sati->heuristic = 0;
  //  sati->nlevel = -1;

#ifdef VSIDS
  /* Heap for maintaining a priority queue of variables. */

  /* This is the location of a variable in the heap array. */
  sati->hindex = (int *)ownalloc(sizeof(int) * (sati->nOfVars+2));
  report_malloc("hindex",sizeof(int) * sati->nOfVars);

  sati->scoreheap = heap_create(sati->nOfVars);

  /* Initially each variable i is in the heap in position i. */
  for(i=0;i<sati->nOfVars;i++) {
    HINDEX(i) = i;
    sati->scoreheap->array[i].k = i;
    sati->scoreheap->array[i].v = 0;
  }
  sati->scoreheap->els = sati->nOfVars;

  adjustheap = 0;	/* Don't adjust the heap before instance complete. */

  sati->VSIDSround = 0;	/* If alternating, always start with the planning heuristic. */
#endif

  for(i=0;i<sati->nOfVars*2;i++) {	/* Initialize literals. */
    LITWATCHES(i) = NULL;
#ifdef VSIDS
    LITSCORE(i) = 0;
#endif
  }

  for(i=0;i<sati->nOfVars;i++) {	/* Initialize variables. */
    VALVALUE(i) = UNASS;
#ifdef VSIDS
    VALSTATUS(i) = 0;
#endif
  }

  /* Gaps for the heuristic omputation */
  //  sati->MAXgaps = sati->nOfSVars;
  //  sati->gaps = (gap *)ownalloc(sizeof(gap) * sati->MAXgaps);
  //  report_malloc("gaps",sizeof(gap) * sati->MAXgaps);
  //  sati->Ngaps = 0;

#ifdef DEBUG
  printf("================================\n");
  printf("Initialized instance:\n");
  printf("total vars: %i\n",sati->nOfVars);
  printf("state vars: %i\n",sati->nOfSVars);
  printf("vars per time: %i\n",sati->nOfVarsPerTime);
  printf("time points: %i\n",sati->nOfTPoints);
  printf("================================\n");
#endif
  return sati;
}

void setheuristic(satinstance sati,int h) {
  sati->heuristic = h;
}

void setpheuristic(satinstance sati,int h) {
  sati->pheuristic = h;
}


/**************************************************************************/
/*******  Keeping track of the heuristic ordering of literals       *******/
/**************************************************************************/

#ifdef VSIDS
int scoreof(satinstance sati,int var) {
  return LITSCORE(PLIT(var)) + LITSCORE(NLIT(var));
}

void decay_score(satinstance sati) {
  int i;
  sati->decaysteps += 1;
  if((sati->decaysteps & 31) == 0) { /* Discount scores. */
    heap_decay(sati);
    for(i=0;i<2*sati->nOfVars;i++) {
      LITSCORE(i) = (LITSCORE(i) >> 1);
    }
  }
}

/* Increase score and update the heap. */

void increase_score(satinstance sati,int lit) {
  LITSCORE(lit) += 1;
  if(adjustheap && HINDEX(VAR(lit)) != -1) {	/* Update heap. */
    heap_increment(sati,HINDEX(VAR(lit)));
  }
}

void increase_score_by(satinstance sati,int lit,int n) {
  LITSCORE(lit) += n;
  if(adjustheap && HINDEX(VAR(lit)) != -1) {	/* Update heap. */
    heap_increment_by(sati,HINDEX(VAR(lit)),n);
  }
}

int getbest(satinstance sati) {
  int var;
  if(!heap_emptyp(sati)) {
    do {
      var = heap_taketop(sati);
    } while((VALVALUE(var) != -1) && (!heap_emptyp(sati)));
    if(VALVALUE(var) != -1) return -1;

    /* Next, decide whether to have the literal POSITIVE or NEGATIVE. */

    /* Use PHASE and SCORE. Break ties randomly. */

    switch(VALSTATUS(var) & 3) {	/* Check phase. */
    case 2: return NLIT(var);
    case 3: return PLIT(var);
    default:
      if(LITSCORE(PLIT(var)) > LITSCORE(NLIT(var))) return PLIT(var);
      if(LITSCORE(PLIT(var)) < LITSCORE(NLIT(var))) return NLIT(var);
	if(2&(random())) return NLIT(var); else return PLIT(var);
    }
  }
  return -1;
}
#endif

/**************************************************************************/
/*******  Construction and extension of clause sets                 *******/
/**************************************************************************/

/*
As explained in Biere's PicoSAT implementation paper.
For each literal, the list of clauses in which it is watched,
is embedded in the clause data structure.
From each literal there is a point to the first clause in which the literal
is watched. The clause data includes a pointer to the next (and consequent)
clauses in which the literal occurs.

IMPLEMENTATION:
table WATCHEDIN[literal]
two additional pointers in each clause

When new clause C is added for a literal, WATCHEDIN[literal] := C,
and C has pointers to the old WATCHEDIN[literal]

When watched literal changes, one literal gets a new clause (added
in front of the list), and the other loses one clause (removed from
the middle of the list by changing the pointer in the preceding clause).

When clauses have been deleted (garbage collection), we must traverse
WATCHEDIN[literal] for all literals, to skip the deleted clauses.

*/

/* Add clause c to literal's watched clause list. */

void setwatch(satinstance sati,int lit,int *c,int index) {
  if(index==0) {
    ASSIGN_WATCHA = LITWATCHES(lit);
    LITWATCHES(lit) = c;
  } else {
    ASSIGN_WATCHB = LITWATCHES(lit);
    LITWATCHES(lit) = c;
  }
}

/* Create a clause */

int from,to,bias;

int auxswatches;

void addnewclause(satinstance sati,int len,clausetype ct,int setwatches) {
#ifdef ASSERTS
  assert(len >= 2);
#endif

  switch(ct) {
  case InitC: from=0; to=0; break;
  case FinalC: from=sati->nOfTPoints-1; to=from; break;
  case TransC: from=0; to=sati->nOfTPoints-2; break;
  }
  
  bias = from*sati->nOfVarsPerTime;

  if(from <= to)
    currentClause = allocpermclause(sati->id,len);
  
  auxswatches = setwatches;
}

void addliteralRAW(satinstance sati,int c,int l) {
  int index = l+bias*2;

  if(from > to) return;

#ifdef ASSERTS
  assert(isliteral(sati,index));
#endif

  currentClause[c] = index;

#ifdef VSIDS
  increase_score(sati,index);
#endif

  if(auxswatches && c < 2) {  /* Set up watches for variables in first two literals. */
    setwatch(sati,index,currentClause,c);
  }
}

inline void addliteral(satinstance sati,int c,int l) {
  addliteralRAW(sati,c,l);
}

/* Finish the clause(s) and return the index of the last clause
   that was generated. */

int *finishclauseRAW(satinstance,int);

int *finishclause(satinstance sati) {
  return finishclauseRAW(sati,1);
}

int *finishclauseRAW(satinstance sati,int score) {
  int i,j,bias2,l,len;
  int *c;

  for(i=to;i>=from+1;i--) {

    len = clauselen(currentClause);

    c = allocpermclause(sati->id,len);

#ifdef ASSERTS
    assert(c != 0);
#endif

    bias2 = i*sati->nOfVarsPerTime;
    for(j=0;j<len;j++) {
      l = currentClause[j]-2*bias+2*bias2;
      c[j] = l;
#ifdef VSIDS
      if(score) increase_score(sati,l);
#endif
      if(auxswatches && j<2) setwatch(sati,l,c,j);
    }
  }

  return currentClause;
}


int add1clauseRAW(satinstance sati,int l,clausetype ct,int score) {
  int from,to,i;
  int index;
  switch(ct) {
  case InitC: from=0; to=0; break;
  case FinalC: from=sati->nOfTPoints-1; to=from; break;
  case TransC: from=0; to=sati->nOfTPoints-2; break;
   default: assert(1==2); break;
  }

  for(i=from;i<=to;i++) {
    index = l+2*i*sati->nOfVarsPerTime;
    if(sati->initialunits+5 > sati->maxinitialunits) {
      sati->maxinitialunits = sati->maxinitialunits*2;
      sati->initialunittable = (int *)realloc(sati->initialunittable,sati->maxinitialunits*sizeof(int));
    }
    sati->initialunittable[sati->initialunits++] = index;
#ifdef VSIDS
    if(score) increase_score(sati,index);
#endif
  }
  return 0;
}

int add1clause(satinstance sati,int l,clausetype ct) {
  return add1clauseRAW(sati,l,ct,1);
}

void add2clauseRAW(satinstance sati,int l1,int l2,clausetype ct) {
  int t,i;
#ifdef VSIDS
  int i1,i2;
#endif
  if(ct == TransC) {

    if(!noT2clauses) {
      if(VAR(l1) < sati->nOfVarsPerTime) {
	sati->l2its[NEG(l1)] = nintcons(l2,sati->l2its[NEG(l1)]);
      } else {
	sati->l2its[NEG(l1-2*sati->nOfVarsPerTime)] = nintcons(l2-2*sati->nOfVarsPerTime,sati->l2its[NEG(l1-2*sati->nOfVarsPerTime)]);
      }
      
      if(VAR(l2) < sati->nOfVarsPerTime) {
	sati->l2its[NEG(l2)] = nintcons(l1,sati->l2its[NEG(l2)]);
      } else {
	sati->l2its[NEG(l2-2*sati->nOfVarsPerTime)] = nintcons(l1-2*sati->nOfVarsPerTime,sati->l2its[NEG(l2-2*sati->nOfVarsPerTime)]);
      }
    }

#ifdef VSIDS
    /* Scores of the 2-literal clauses for all literals */
    for(i=0;i<sati->nOfTPoints;i++) {
      
      i1 = l1+2*i*sati->nOfVarsPerTime;
      i2 = l2+2*i*sati->nOfVarsPerTime;

      if(i1 < 2*sati->nOfVars) increase_score(sati,i1);
      if(i2 < 2*sati->nOfVars) increase_score(sati,i2);
      }
#endif
    
  } else {
    if(ct == InitC) t = 0;
    else t=sati->nOfTPoints-1;

    // These are not needed if the increase_score below is their only use!!!!
    l1 += 2*t*sati->nOfVarsPerTime;
    l2 += 2*t*sati->nOfVarsPerTime;

#ifdef VSIDS
      increase_score(sati,l1);
      increase_score(sati,l2);
#endif

      //    LIT2LITS(NEG(l1)) = nintcons(l2,LIT2LITS(NEG(l1)));
      //    LIT2LITS(NEG(l2)) = nintcons(l1,LIT2LITS(NEG(l2)));
  }
}

void add2clause(satinstance sati,int l1,int l2,clausetype ct) {
  if(ct == TransC) add2clauseRAW(sati,l1,l2,ct);
  else {
    //    printf("@");
    addnewclause(sati,2,ct,1);
    addliteral(sati,0,l1);
    addliteral(sati,1,l2);
    finishclause(sati);
  }
}

int nintlistlen(nintlist *ls) {
  int c;
  c = 0;
  while(ls != NULL) {
    c += 1;
    ls = ls->tl;
  }
  return c;
}

float megab(int i) {
  return ((float)i)/(1024.0*1024.0);
}

#ifdef DEBUG
void showinstancesize(satinstance sati) {
  int cnt;
  int i;
  int longclauses;
  nintlist *ls;

  cnt = 0;
  for(i=0;i<2*sati->nOfVarsPerTime;i++) {
    cnt += nintlistlen(sati->l2its[i]);
  }

  longclauses = 0;
  for(i=0;i<2*sati->nOfVars;i++) {
    ls = LITWATCHES(i);
    while(ls != NULL) {
      longclauses += ((clauselen((intlist*)(ls->hd))+5)*4);
      ls = ls->tl;
    }
  }
  printf("===========================\n");
  printf("l2its is %.1f MB\n",megab((sati->nOfSVars+2*cnt)*4));
  printf("al2its is %.1f MB\n",megab((sati->nOfSVars+cnt)*4));
  printf("lits is %.1f MB\n",megab(3*4*2*sati->nOfVars+longclauses));
  printf("vars is %.1f MB\n",megab(4*4*sati->nOfVars));
  printf("unitstack is %.1f MB\n",megab(4*sati->nOfVars));
#ifdef VSIDS
  printf("scoreheap is %.1f MB\n",megab(sati->nOfVars*8));
#endif
  printf("UCC is %.1f MB\n",megab(10000*4));
  printf("===========================\n");
}
#endif

/* Run this when all binary clauses are known.
 Main function: translated the binary clause linked lists to arrays for
 faster access.
*/

#ifdef SPREAD
int **al2permanent;
#endif

void instancecomplete(satinstance sati) {
  int i,c,j;
  nintlist *ls;

  sati->complete = 1;

  /* Update the heap. */

#ifdef VSIDS
  for(i=0;i<sati->nOfVars;i++) heap_increment_by(sati,HINDEX(i),LITSCORE(PLIT(i))+LITSCORE(NLIT(i)));
#endif

  adjustheap = 1;

#ifdef VSIDS
#ifdef ASSERTS
  checkheapconsistency(sati);
#endif
#endif
  if(!noT2clauses) {

    int arraysize,*a2litarray,*ptr;

    /* Put the binary clauses linked lists into arrays. */

    arraysize = 1;

    for(i=0;i<sati->nOfVarsPerTime*2;i++) {
      arraysize += (1+nintlistlen(sati->l2its[i]));
    }

    a2litarray = (int *)ownalloc(sizeof(int) * arraysize);

    //    printf("Allocated %i elements for a2litarray\n",arraysize);

    a2litarray[0] = 0;
    ptr = a2litarray+1;

    sati->al2its = (int **)ownalloc(sizeof(int *) * sati->nOfVarsPerTime * 2);

    for(i=0;i<sati->nOfVarsPerTime*2;i++) {
      c = nintlistlen(sati->l2its[i]);
      if(c == 0) sati->al2its[i] = a2litarray;
      else {
	sati->al2its[i] = ptr;
	ptr[0] = c;
	ls = sati->l2its[i];
	for(j=0;j<c;j++) {
	  ptr[j+1] = ls->hd;
	  ls = ls->tl;
	}
	ptr += (c+1);
      }
      /* Sort the array to reduce cache misses. */
    }
    /* Completed the move to arrays. */
#ifdef SPREAD
    al2permanent = sati->al2its;
#endif
  }


#ifdef SPREAD
  assert(al2permanent);
  /* Create 2-lit clauses for every time point. */
  //  printf("SPREADING THE 2-LITERAL CLAUSES\n");
  for(i=0;i<sati->nOfVars*2;i++) {
    int origin,bias,maxlen,*v,*v2,j;
    origin = i % (sati->nOfVarsPerTime * 2);
    bias = i-origin;
    //    v = sati->al2its[origin];
    v = al2permanent[origin];
    maxlen = v[0];
    //    printf("%i literals for ",maxlen); printTlit(sati,i); printf(": ");
    v2 = (int *)ownalloc((maxlen+1) * sizeof(int));
    sati->al2itsT[i] = v2;
    v2[0] = maxlen;
    for(j=1;j<=maxlen;j++) {
      if(v[j]+bias >= 0 && v[j]+bias < 2*sati->nOfVars) {
	v2[j] = v[j]+bias;
	//	printTlit(sati,v[j]+bias);
      } else{
	v2[j] = i;
	//	printf(" (i)");
      }
    }
    //    printf("\n");
  }
#endif


#ifdef DEBUG
  //  showinstancesize(sati);
#endif
}


/**************************************************************************/
/*******  Functions for unit propagation                            *******/
/**************************************************************************/

/* Set literals in unitstack true and unit propagate.

The reason parameter denotes the cause of the literal being set.
-1 means that the literal is a decision literal
-2 means that the literal is one of the unit clauses in the input CNF
if the 2 lsbs are 0, the reason is a pointer to a clause where
   the literal was a unit (HERE WE ASSUME THAT ADDRESSES OF CLAUSES
   ALWAYS HAVE THEIR 2 LSBs 0.)
otherwise the reason is (l << 1)|1 for another literal l

If contradiction is reached, return the index of a falsified
clause. */


inline int addtoqueue(satinstance sati,int l,PTRINT reason,int level,variable *sativars) {
#ifdef ASSERTS
  assert(isliteral(sati,l));
#endif

  if(SVALVALUE(VAR(l)) == UNASS) {	/* Literal has no value yet. */

    SVALVALUE(VAR(l)) = VALUE(l);	/* Assign */
#ifdef VSIDS
    SVALSTATUS(VAR(l)) = 2|(VALUE(l)); /* Record phase */
#endif
    SVALREASON(VAR(l)) = reason;
    SVALDLEVEL(VAR(l)) = level;

#ifdef GLOBALUS
    *(++GLOBALendunitstack) = l;    /* Push it into the stack. */
#else
    sati->unitstack[++(sati->endunitstack)] = l;    /* Push it into the stack. */
#endif

    return 0;
  }

  /* Variable had a value already. */

  if(SVALVALUE(VAR(l)) != VALUE(l)) {	/* Conflicting value. */
    return 1;	/* Conflict */
  }
  return 0;
}

/* Same as addtoqueue, except that we KNOW that the variable is unassigned. */

inline int simpleaddtoqueue(satinstance sati,int l,PTRINT reason,int level,variable *sativars) {

  SVALVALUE(VAR(l)) = VALUE(l);	/* Assign */
#ifdef VSIDS
  SVALSTATUS(VAR(l)) = 2|(VALUE(l)); /* Record phase */
#endif
  SVALREASON(VAR(l)) = reason;
  SVALDLEVEL(VAR(l)) = level;

#ifdef GLOBALUS
  *(++GLOBALendunitstack) = l;    /* Push it into the stack. */
#else
  sati->unitstack[++(sati->endunitstack)] = l;    /* Push it into the stack. */
#endif

  return 0;
}

inline int bareaddtoqueue(satinstance sati,int l,variable *sativars) {
#ifdef ASSERTS
  assert(isliteral(sati,l));
#endif

  if(SVALVALUE(VAR(l)) == UNASS) {	/* Literal has no value yet. */

    SVALVALUE(VAR(l)) = VALUE(l);	/* Assign */

#ifdef GLOBALUS
    *(++GLOBALendunitstack) = l;    /* Push it into the stack. */
#else
    sati->unitstack[++(sati->endunitstack)] = l;    /* Push it into the stack. */
#endif

    return 0;
  }

  /* Variable had a value already. */

  if(SVALVALUE(VAR(l)) != VALUE(l)) return 1;	/* Conflict */

  return 0;
}


/* Same as simpleaddtoqueue, but without bookkeeping for learning. */

inline int baresimpleaddtoqueue(satinstance sati,int l,variable *sativars) {
  SVALVALUE(VAR(l)) = VALUE(l);	/* Assign */
#ifdef GLOBALUS
  *(++GLOBALendunitstack) = l;    /* Push it into the stack. */
#else
  sati->unitstack[++(sati->endunitstack)] = l;    /* Push it into the stack. */
#endif
  return 0;
}

/* Propagation with 2-literal clauses. */

inline int propagate2lit(satinstance sati,int lit,int level,variable *sativars) {
  int glit,bias2;
  PTRINT reason;
  int j;
  int nofv2;
  int *AAA;

  reason = (lit << 1)|1;	/* Reason for the literals to be set. */

  /* Propagate the 2-literal clauses for all time points. */

#ifndef SPREAD
  /* Compact binary clause representation */
  glit = lit%(2*sati->nOfVarsPerTime);
  bias2 = lit-glit;

  AAA = sati->al2its[glit];

  nofv2 = 2*sati->nOfVars;

  for(j=1;j<=AAA[0];j++) {
    if(AAA[j]+bias2 < nofv2
       && AAA[j]+bias2 >= 0
       && addtoqueue(sati,AAA[j]+bias2,reason,level,sativars)) {
      conflicttype2lit = 1;
      conflictl1 = NEG(AAA[j]+bias2);
      conflictl2 = lit;
      return 1;
    }
  }
#else
  /* Conventional binary clause representation */
  AAA = LIT2LITS(lit);
  for(j=1;j<=AAA[0];j++) {
    if(addtoqueue(sati,AAA[j],reason,level,sativars)) {
      conflicttype2lit = 1;
      conflictl1 = NEG(AAA[j]);
      conflictl2 = lit;
      return 1;
    }
  }
#endif

  /* Propagate with the 2-literal clauses for the current time point. */

  /*  ls = LIT2LITS(lit);
  while(ls != NULL) {
    if(addtoqueue(sati,ls->hd,reason,level,sativars)) {
      conflicttype2lit = 1;
      conflictl1 = NEG(ls->hd);
      conflictl2 = lit;
      return 1;
    }
    ls = ls->tl;
  }
  */

  return 0;
}

/* Same as propagate2lit, but without book-keeping for learning. */

inline int barepropagate2lit(satinstance sati,int lit,variable *sativars) {
  int glit,bias2;
  int j;
  int nofv2;
  int *AAA;

  /* Propagate the 2-literal clauses for all time points. */

  glit = lit%(2*sati->nOfVarsPerTime);
  bias2 = lit-glit;

  AAA = sati->al2its[glit];

  nofv2 = 2*sati->nOfVars;

  for(j=1;j<=AAA[0];j++) {
    if(AAA[j]+bias2 < nofv2
       && AAA[j]+bias2 >= 0
       && bareaddtoqueue(sati,AAA[j]+bias2,sativars)) {
      return 1;
    }
  }

  /* Propagate with the 2-literal clauses for the current time point. */

  /*
  ls = LIT2LITS(lit);
  while(ls != NULL) {
    if(bareaddtoqueue(sati,ls->hd,sativars)) {
      return 1;
    }
    ls = ls->tl;
  }
  */

  return 0;
}

/* Propagation with long clauses (> 2 literals):
   Go through clauses in which literal is watched.
   If only one unassigned literal left, put into queue.
*/

inline int propagatelong(satinstance sati,int l,int level) {
  int *c,*c2;
  int *nextclause;
  PTRINT *prev;
  int tmp;
  PTRINT *ptmp;
#ifdef ASSERTS
  int *i;
#endif

  variable *sativars;

  sativars = sati->vars;

  prev = &(LITWATCHES(NEG(l)));
  nextclause = LITWATCHES(NEG(l));	/* Clauses where -l is watched */

  while(nextclause != NULL) { /* Visit clauses where -l is watched. */
    c = nextclause;

    /* The watched literals are always the first two in a clause. */
    /* Make the current literal the first in the clause. */
    if(*c != NEG(l)) {
      tmp = c[0]; c[0] = c[1]; c[1] = tmp;
      ptmp = ACCESS_WATCHA; ASSIGN_WATCHA = ACCESS_WATCHB; ASSIGN_WATCHB = ptmp;
    }

    /* If the second watched literal is true, don't do anything. */

    if(SVALVALUE(VAR(*(c+1))) == VALUE(*(c+1))) {
      prev = ADDRESS_WATCHA;
      nextclause = ACCESS_WATCHA;
      continue;
    }

    c2 = c+2;

    /* Find a non-false literal. */
#ifdef ONLY3LITERALCLAUSE
    do {
      if(SVALVALUE(VAR(*c2)) != 1-VALUE(*c2)) goto nonfalse;
      c2 += 1;
    } while(*c2 != -1);
#else
    while(*c2 != -1) {
      if(SVALVALUE(VAR(*c2)) != 1-VALUE(*c2)) goto nonfalse;
      c2 += 1;
    }
#endif

    /* 2nd watched literal is a new unit clause. */

    //    updateactivity(c,sati->conflicts);
    c[PREFIX_ACTIVITY] = sati->conflicts;

    if(addtoqueue(sati,*(c+1),(PTRINT)c,level,SATIVARS)) {
      conflicttype2lit = 0;
      conflictclause = c;
      return 1;
      /* You might exit here before fixing all the clauses with
	 the literal watched. Is this correct? */
    }
    prev = ADDRESS_WATCHA;
    nextclause = ACCESS_WATCHA;
    continue;

  nonfalse:


    /* Swap the old watched literal (at *c) and the new (at *c2). */
    /* Armin Biere (Precosat) says that this swapping is not good:
       you should keep the old watched literal close to the head of the list.
       TEST RESULT: changes runtimes randomly, does not consistently improve.
    */

    tmp = *c2;
#define noBIERE
#ifdef BIERE
    while(c2 > c+2) {
      *c2 = *(c2-1);
      c2 -= 1;
    }
#endif
    *c2 = *c;
    *c = tmp;
	  

    /* Remove the clause from the old literal's watched clause list. */

    *prev = ACCESS_WATCHA;

    nextclause = ACCESS_WATCHA; /* Go to the next clause. */

    /* Add the clause to the new literal's watched clause list. */

    ASSIGN_WATCHA = LITWATCHES(*c);
    LITWATCHES(*c) = c;

  }
  return 0;
}


int barepropagatelong(satinstance sati,int l) {
  int *c,*c2;
  nintlist *nextclause;
  int tmp,*tmp0;
  nintlist *ptmp;
  PTRINT *prev;

  variable *sativars;

  sativars = sati->vars;

  prev = &(LITWATCHES(NEG(l)));
  nextclause = LITWATCHES(NEG(l));	/* Clauses where -l is watched */

  while(nextclause != NULL) { /* Visit clauses where -l is watched. */
    c = nextclause;

    /* The watched literals are always the first two in a clause. */
    /* Make the current literal the first in the clause. */
    if(*c != NEG(l)) {
      tmp = c[0]; c[0] = c[1]; c[1] = tmp;
      tmp0 = ACCESS_WATCHA; ASSIGN_WATCHA = ACCESS_WATCHB; ASSIGN_WATCHB = tmp0;
    }

    /* If the second watched literal is true, don't do anything. */

    if(SVALVALUE(VAR(*(c+1))) == VALUE(*(c+1))) {
      prev = ADDRESS_WATCHA;
      nextclause = ACCESS_WATCHA;
      continue;
    }

    c2 = c+2;

    /* Find a non-false literal. */
    do {
      if(SVALVALUE(VAR(*c2)) != 1-VALUE(*c2)) goto nonfalse;
      c2 += 1;
    } while(*c2 != -1);

    /* 2nd watched literal is a new unit clause. */

    //    updateactivity(c,sati->conflicts);
    c[PREFIX_ACTIVITY] = sati->conflicts;

    if(bareaddtoqueue(sati,*(c+1),SATIVARS)) {
      return 1;
      /* You might exit here before fixing all the clauses with
	 the literal watched. Is this correct? */
    }

    prev = ADDRESS_WATCHA;
    nextclause = ACCESS_WATCHA;
    continue;

  nonfalse:

    tmp = *c2;
    *c2 = *c;
    *c = tmp;
	  
    /* Remove the clause from the old literal's watched clause list. */

    *prev = ACCESS_WATCHA;
    nextclause = ACCESS_WATCHA;
	  
    /* Add the clause to the new literal's watched clause list. */

    ASSIGN_WATCHA = LITWATCHES(*c);
    LITWATCHES(*c) = c;

  }
  return 0;
}

void setUPstacktop(satinstance sati,int top) {
#ifdef GLOBALUS
  GLOBALendunitstack = &(GLOBALunitstack[top]);
  GLOBALstartunitstack = &(GLOBALunitstack[top+1]);
#else
  sati->endunitstack = top;
  sati->startunitstack = top+1;
#endif
}

/* Main function for propagation */

int propagate(satinstance sati,int level) {
  int l;
#ifdef GLOBALUS
  int *startlong;
#else
  int startlong;
#endif
  variable *sativars;

  sativars = sati->vars;

#ifdef GLOBALUS
  startlong = GLOBALstartunitstack;
  while(startlong <= GLOBALendunitstack) {

    while(GLOBALstartunitstack <= GLOBALendunitstack) {
      l = *(GLOBALstartunitstack++);
#else
  startlong = sati->startunitstack;
  while(startlong <= sati->endunitstack) {

    while(sati->startunitstack <= sati->endunitstack) {
      l = sati->unitstack[sati->startunitstack++];
#endif

      /* Propagate with 2-literal clauses */
      if(propagate2lit(sati,l,level,sativars)) return 1;
    }

    /* Propagate with long clauses */

#ifdef GLOBALUS
    l = *(startlong++);
#else
    l = sati->unitstack[startlong++];
#endif
    if(propagatelong(sati,l,level)) return 1;
    
  }

  return 0; /* No contradiction */
}

    /* Like propagate, but without book-keeping for learning. */

int barepropagate(satinstance sati) {
  int l;
#ifdef GLOBALUS
  int *startlong;
#else
  int startlong;
#endif
  variable *sativars;

  sativars = sati->vars;

#ifdef GLOBALUS
  startlong = GLOBALstartunitstack;
  while(startlong <= GLOBALendunitstack) {

    while(GLOBALstartunitstack <= GLOBALendunitstack) {
      l = *(GLOBALstartunitstack++);
#else
  startlong = sati->startunitstack;
  while(startlong <= sati->endunitstack) {

    while(sati->startunitstack <= sati->endunitstack) {
      l = sati->unitstack[sati->startunitstack++];
#endif

      /* Propagate with 2-literal clauses */
      if(barepropagate2lit(sati,l,sativars)) return 1;
    }

    /* Propagate with long clauses */

#ifdef GLOBALUS
    l = *(startlong++);
#else
    l = sati->unitstack[startlong++];
#endif
    if(barepropagatelong(sati,l)) return 1;
    
  }

  return 0; /* No contradiction */
}


/*************************************************************************/
/** Computation of learned clauses                                      **/
/*************************************************************************/

int cc[MAXCCLENGTH];	/* Conflict clause which is being constructed */
int ccp;	/* Index to the last literal in the conflict clause */

inline void addtoconflict(satinstance sati,int l) {
#ifdef ASSERTS
  int i;
  for(i=0;i<=ccp;i++) assert(cc[i] != l);
#endif
  ccp += 1;
  cc[ccp] = l;
#ifdef ASSERTS
  assert(ccp<MAXCCLENGTH);
  assert(isliteral(sati,l));
#endif
}

int ccs[MAXCCLENGTH];	/* The stack used during computing the conflict clause */
int csp;	/* Index to the top of the stack */

/* Push an element to the stack. */

inline void cpush(int i) {
#ifdef ASSERTS
  int j;
#endif
  csp += 1;
#ifdef ASSERTS
  assert(i >= 0);
  assert(csp<MAXCCLENGTH);
  for(j=0;j<csp;j++) assert(ccs[j] != i);
#endif
  ccs[csp] = i;
}

/* Make sure that each literal is considered only once. */

int seenp(int l) {	/* Was literal already seen on the current round? */
  if(wasseen[l] == cround) {
    return 1;
  }
  wasseen[l] = cround;	/* Mark the literal as seen on the current round. */
  return 0;
}

/* Go through unitstack and unset variables. */

void undosetlits(satinstance sati) {
#ifdef GLOBALUS
  int *i;
  for(i=GLOBALendunitstack;i>=GLOBALunitstack;i--) {
    VALVALUE(VAR(*i)) = UNASS;
#else
  int i;
  for(i=sati->endunitstack;i>=0;i--) {
    VALVALUE(VAR(sati->unitstack[i])) = UNASS;
#endif
  }

  setUPstacktop(sati,-1);
}

void WASundosetlits(satinstance sati) {
#ifdef GLOBALUS
  int *i;
  for(i=GLOBALendunitstack;i>=GLOBALunitstack;i--) {
    wasseen[*i] = cround;
    VALVALUE(VAR(*i)) = UNASS;
#else
  int i;
  for(i=sati->endunitstack;i>=0;i--) {
    wasseen[sati->unitstack[i]] = cround;
    VALVALUE(VAR(sati->unitstack[i])) = UNASS;
#endif
  }

  setUPstacktop(sati,-1);
}

/* 
   The literals that have been added as unit clauses.
   They have to be remembered until decision level 0 is visited again,
   because their assignments will be undone for example during
   the standard CDCL main loop run on levels > 0.
 */

void addUCC(satinstance sati,int l) {
  sati->UCC[sati->NofUCClauses++] = l;
#ifdef ASSERTS
  assert(sati->NofUCClauses < MAXUCC);
#endif

#ifdef DEBUG
  printf("New unit clause "); printTlit(sati,l); printf("\n");
#ifndef GLOBALUS
  printf("lvl: %i start: %i end: %i\n",-999,sati->endunitstack,sati->startunitstack);
#endif
#endif
}

int returnUCC(satinstance sati,int level) {
  int i;
  for(i=0;i<sati->NofUCClauses;i++) {
    // The return 1 below should never happen!!!!!
    if(addtoqueue(sati,sati->UCC[i],REASON_INPUT,0,sati->vars) || propagate(sati,0)) return 1;
  }

#ifdef DEBUG
#ifndef GLOBALUS
  printf("lvl: %i start: %i end: %i\n",level,sati->endunitstack,sati->startunitstack);
#endif
#endif

  return 0;
}


/* Compute a 1-UIP or Last-UIP clause.

1. Go through the literals in the current clause.
2. If literal for non-decision level, store it.
3. If non-decision literal for the top level, traverse its reasons.
4. If the reason is -2, ignore the literal.

*/

#define noFIRSTUIP 1

int CCdliteral;	/* The only decision level literal in the learned clause */
int CChighestnond;	/* The highest non-decision level in the clause */
PTRINT CCreason;	/* The reason for the flipped literal after learning */
int CCwatch1;
int CCwatch2;

void learn(satinstance sati,int dlevel) {
  int len,l,rl,i,j;
  PTRINT r;
  int *c;
#ifdef FIRSTUIP
  int dlevells,dleveldone;
#endif

  /* This counter is used for eliminating multiple occurrences of
     a literal from the derivation of the conflict clause.
     If the literal is associated with the current round, it has
     been seen already and will be ignored. */
  cround += 1;

  if(conflicttype2lit) { /* A 2-literal clause was falsified. */
    ccs[0] = conflictl1; /* Put both literals in the stack. */
    ccs[1] = conflictl2;

#ifdef DEBUG
    printf("Violated binary clause ");
    printTlit(sati,conflictl1);
    printf(" ");
    printTlit(sati,conflictl2);
    printf("\n");
#endif

    len = 2;

#ifdef ASSERTS
    assert(isliteral(sati,conflictl1));
    assert(isliteral(sati,conflictl2));
#endif

  } else { /* A clause of >= 3 literals was falsified. */

    //    len = clauselen(conflictclause);
    len = conflictclause[PREFIX_CLAUSELEN];

#ifdef DEBUG
    printf("Violated clause %i (len %i):\n",conflictclause,len);
    for(i=0;i<len;i++) { printf(" %i:",VAR(conflictclause[i])); printTlit(sati,conflictclause[i]); }
    //    for(i=0;i<len;i++) { printf(" %i",conflictclause[i]); }
    printf("\n");
#endif


    /* Put all literals in the clause in the ccs stack. */
    for(i=0;i<len;i++) ccs[i] = NEG(conflictclause[i]);
  }

  CChighestnond = -1; /* Highest non-decision level. */
  /* This will be updated every time a literal is added to
     the conflict clause with addtoconflict. */

  csp = len-1;
  ccp = -1;

  /* Mark all literals initially in the stack as seen. */
  /* Count the number of decision level literals. */
#ifdef FIRSTUIP
  dlevells = 0;
#endif

  for(i=0;i<=csp;i++) {
    wasseen[ccs[i]] = cround;
#ifdef FIRSTUIP
    if(VALDLEVEL(VAR(ccs[i])) == dlevel) dlevells += 1;
#endif
  }

  /* Count decision level literals in the stack. */


#ifdef ASSERTS
  CCwatch1 = -1;
  CCwatch2 = -1;
#endif

#ifdef FIRSTUIP
  dleveldone = 0;
#endif

  while(csp >= 0) {

    l = ccs[csp--];	/* Pop literal from the stack. */

#ifdef ASSERTS
    assert(isliteral(sati,l));
    assert(VALVALUE(VAR(l)) != UNASS);
#endif

    r = VALREASON(VAR(l)); 

#ifdef DEBUG
    printf("Reason of "); printTlit(sati,l); printf(" is "); fflush(stdout);
    if(r == REASON_DECISION) {
      printf("DECISION \n");
    } else if(r == REASON_INPUT) {
      printf("INPUT \n");
    } else if((r&1) == 0) {
      printf("clause %i (of length %i)\n",(int)r,clauselen(r));
      printclause(sati,(int *)r);
    } else {
      printf("Binary resolution with %i:",VAR(r >> 1));
      printTlit(sati,r >> 1);
      printf("\n");
    }
#endif

    /* Infer (learn) a new clause from the falsified clause, one literal
       at a time. */
    
    if(VALDLEVEL(VAR(l)) != dlevel) { /* Non-decision-level literal */
      if(r != REASON_INPUT) {	/* Not an input unit clause. */
	addtoconflict(sati,NEG(l)); /* Have it in the learned clause. */
	CChighestnond = max(VALDLEVEL(VAR(l)),CChighestnond);
      }
    } else {	/* Decision-level literal */
#ifdef FIRSTUIP
      if(r == REASON_DECISION || (dlevells == 1 && dleveldone == 0)) { /* It is the decision literal. */
#else
      if(r == REASON_DECISION) { /* It is the decision literal. */
#endif
	addtoconflict(sati,NEG(l));
	CCwatch1 = NEG(l);
	CCdliteral = NEG(l);
#ifdef FIRSTUIP
	dleveldone = 1;
	dlevells -= 1;
#endif
      } else if(r&1) {	/* Reason is a literal (2 lit clause) */
#ifdef VSIDS
	increase_score(sati,l);	/* Increase score */
#endif

#ifdef FIRSTUIP
	dlevells -= 1;
#endif
	if(!seenp(r >> 1)) {
	  cpush(r >> 1);
#ifdef FIRSTUIP
	  dlevells += 1;
#endif
	}
      } else {	/* Reason is a clause */
#ifdef VSIDS
	increase_score(sati,l);	/* Increase score */
#endif

#ifdef FIRSTUIP
	dlevells -= 1;
#endif
	c = (int *)r;
	while(*c != -1) {	/* Push all literals except l into the stack. */
	  if(VAR(*c) != VAR(l) && !seenp(NEG(*c))) {
#ifdef FIRSTUIP
	    dlevells += 1;
#endif
	    cpush(NEG(*c));
	  }
	  c += 1;
	}
      }
    }

  }

#ifdef DEBUG
  printf("Learned clause %i (%i lits):",clausecount,ccp+1);
  for(i=0;i<=ccp;i++) { printf(" %i:",VAR(cc[i])); printTlit(sati,cc[i]); }
  printf("\n");
#endif

#ifdef ASSERTS
  /* See that the learned clause is false in the current valuation. */
  for(i=0;i<=ccp;i++) {
    assert(VALVALUE(VAR(cc[i])) != VALUE(cc[i]));
    assert(VALVALUE(VAR(cc[i])) != UNASS);
  }
#endif

  /* Minimize the size of the learned clause:
     1. Mark all literals in the clause.
     2. Remove literals whose parent is marked.
  */

#define noMINIMIZE 1

#ifdef MINIMIZE
  cround += 1;
  for(i=0;i<=ccp;i++) wasseen[NEG(cc[i])] = cround;
  i = 0;
  while(i<=ccp) {
    rr = VALREASON(VAR(cc[i]));
    if(rr != REASON_DECISION && (((int)rr)&1) && (wasseen[((int)rr) >> 1] == cround)) { /* Remove. */
      cc[i] = cc[ccp--];
    } else {
      if(VALDLEVEL(VAR(cc[i])) == CChighestnond) CCwatch2 = cc[i];
      i = i + 1;
    }
  }
#else
  for(i=0;i<=ccp;i++) {
    if(VALDLEVEL(VAR(cc[i])) == CChighestnond) CCwatch2 = cc[i];
  }
#endif

  /* Add the new clause to the clause set. */

  if(ccp >= 2) { /* Clause with at least 3 literals */

#ifdef ASSERTS
    assert(isliteral(sati,CCwatch1));
    assert(isliteral(sati,CCwatch2));
    assert(CCwatch1 != CCwatch2);
#endif

    from = 0;
    to = 0;
    bias = 0;

    currentClause = allocclause(sati->id,ccp+1);

    //    updateactivity(currentClause,sati->conflicts);
    currentClause[PREFIX_ACTIVITY] = sati->conflicts;

    auxswatches = 0;

    /* The watched literals are the ones with the highest levels,
       that is, the one at the decision level and one on the
       next highest level. */

    addliteral(sati,0,CCwatch1);
    addliteral(sati,1,CCwatch2);

    j = 2;
    for(i=0;i<=ccp;i++) {
      if(cc[i] != CCwatch1 && cc[i] != CCwatch2) addliteral(sati,j++,cc[i]);
    }

    finishclause(sati);

    CCreason = (PTRINT)currentClause;

    setwatch(sati,CCwatch1,CCreason,0);
    setwatch(sati,CCwatch2,CCreason,1);

  } else if(ccp == 1) { /* Clause with 2 literals */


    //    printf("LEARNED2\n");
#ifdef DEBUG
    printf("LEARNED A 2-LITERAL CLAUSE (horizon %i)\n",sati->nOfTPoints);
#endif

    add2clause(sati,cc[0],cc[1],InitC);

    if(cc[0] == CCdliteral) rl = cc[1];
    else rl = cc[0];
    CCreason = ((NEG(rl))<< 1)|1;

  } else { /* Unit clause */

#ifdef DEBUG
    printf("LEARNED A 1-LITERAL CLAUSE! (horizon %i)\n",sati->nOfTPoints);
#endif
    addUCC(sati,cc[0]);
    CCreason = REASON_INPUT;
    CChighestnond = 0;

  }

}


/**************************************************************************/
/*******       The Conflict-Directed Clause Learning algorithm      *******/
/**************************************************************************/


int chooseliteral(satinstance sati) {
  int l;

#ifdef VSIDS
  if(!flagVSIDSalternate || !sati->VSIDSround) {
#endif
    switch(PLANheuristic) {
    case 0: break;
    default:
      l = do_cpath_heuristic(sati);
      if(l != -1) return l;
      break;
    }
#ifdef VSIDS
  }
#endif

  /* If planning heuristic was not applicable, use the SAT heuristic. */

#ifdef VSIDS
  /* Choose unassigned literal with highest score. */
  return getbest(sati);
#else
  return -1;
#endif
}

#define MAXDECLEV 2000000

#ifdef GLOBALUS
int *declevels[MAXDECLEV];
#else
int declevels[MAXDECLEV];
#endif

#ifdef HARDDEBUG
void showstack(satinstance sati) {
#ifdef GLOBALUS
  int *i,l;
  printf("============================\n");
  for(i=GLOBALendunitstack;i>max(0,GLOBALendunitstack-20);i--) {
    l = *i;
    printf("%2i: %2i ",i-GLOBALendunitstack,VALDLEVEL(VAR(l)));
#else
  int i,l;
  printf("============================\n");
  for(i=sati->endunitstack;i>max(0,sati->endunitstack-20);i--) {
    l = sati->unitstack[i];
    printf("%2i: %2i ",i,VALDLEVEL(VAR(l)));
#endif
    printTlit(sati,l);
    printf("\n");
  }
  printf("----------------------------\n");
#ifndef GLOBALUS
  printf("levels:");
  for(i=max(0,sati->dlevel-8);i<=sati->dlevel;i++) {
    printf(" %i@%i",i,declevels[i-1]);
  }
#endif
  printf("\n");
  printf("============================\n");
}
#endif

void emptystack(satinstance sati) {
#ifdef GLOBALUS
  int *i;
  for(i=GLOBALunitstack;i<=GLOBALendunitstack;i++)
    VALVALUE(VAR(*i)) = UNASS;
#else
  int i;
  for(i=0;i<=sati->endunitstack;i++)
    VALVALUE(VAR(sati->unitstack[i])) = UNASS;
#endif
  
  setUPstacktop(sati,-1);
}

void undo_assignments_until_level(satinstance sati,int btlevel) {
#ifdef GLOBALUS
  int *i,v;
  for(i=declevels[btlevel];i<=GLOBALendunitstack;i++) {
    v = VAR(*i);
#else
  int i,v;
  for(i=declevels[btlevel];i<=sati->endunitstack;i++) {
    v = VAR(sati->unitstack[i]);
#endif
    VALVALUE(v) = UNASS;
#ifdef VSIDS
    if(HINDEX(v) == -1) heap_insert(sati,v,scoreof(sati,v));
#endif
  }
}

/* The standard CDCL Conflict Directed Clause Learning algorithm */

int UPLA(satinstance sati);

int solve0(satinstance sati,int maxconflicts) {
  int l,p,btlevel;
  int q;
  int firsttime;

  firsttime = sati->notcalledbefore;

  if(sati->notcalledbefore) {
    sati->notcalledbefore = 0;
    for(p=0;p<sati->initialunits;p++) {
      if(addtoqueue(sati,sati->initialunittable[p],REASON_INPUT,0,sati->vars)) {
	goto UNSAT;
      }
    }

  }

#ifdef DEBUG
  printf("Propagating initial unit clauses.\n"); fflush(stdout);
#endif

  /* Remove this? */
  if(propagate(sati,0)) goto UNSAT;

  if(firsttime && (flagPreprocessing&1) && UPLA(sati)) goto UNSAT;

  sati->NofUCClauses = 0;

  setUPstacktop(sati,-1);

  sati->dlevel = 0;

  if(sati->pheuristic >= 1) {
    //    gap_init(sati);
    sati->heuristic_mode = 0;
  }

  q = 0;

  //  printplanT(sati); //

  do {

    if(q || propagate(sati,sati->dlevel)) {	/* Got a conflict? */

      sati->heuristic_mode = 0;

      if(sati->dlevel == 0) goto UNSAT;

#ifdef VSIDS
      decay_score(sati);
#endif

      sati->conflicts += 1;

      learn(sati,sati->dlevel);
      maxconflicts -= 1;

      p = CCdliteral; /* Sole literal at the conflict level */
      
      btlevel = CChighestnond; /* Highest level in the conflict clause apart from p. */

#ifdef ASSERTS
      assert(btlevel >= 0);
      assert(btlevel < sati->dlevel);
      assert(VALDLEVEL(VAR(p)) == sati->dlevel);
#endif

#ifdef HARDDEBUG
      printf("Conflict level %i literal ",sati->dlevel); printTlit(sati,p);
      printf(", next level is %i.\n",btlevel);

      showstack(sati);
#endif

#ifdef DEBUG
      printf("Undoing everything down to level %i at stack location %i.\n",btlevel,declevels[btlevel]); fflush(stdout);
#endif

      /* Undo assignments. */

      undo_assignments_until_level(sati,btlevel);

#ifdef GLOBALUS
      GLOBALendunitstack = declevels[btlevel]-1;
      GLOBALstartunitstack = declevels[btlevel];
#else
      setUPstacktop(sati,declevels[btlevel]-1);
#endif

#ifdef ASSERTS
      assert(VALVALUE(VAR(p)) == UNASS);
#endif
      
      sati->dlevel = btlevel;

      if(returnUCC(sati,sati->dlevel)) goto UNSAT;
      if(sati->dlevel == 0) sati->NofUCClauses = 0;

#ifdef DEBUG
      printf("Flipping "); printTlit(sati,p); printf(".\n"); fflush(stdout);
#endif

      /* CCreason refers to the conflict clause. */

      q = addtoqueue(sati,p,CCreason,sati->dlevel,sati->vars);

    } else {	/* Extend the assignment. */

#ifdef DEBUG
      printf("Choosing literal.\n");
#endif

      l = chooseliteral(sati);

#ifdef ASSERTS
      assert(VAR(l) < sati->nOfVars);
#endif

      if(l == -1) goto SAT;	/* All variables assigned already. */

#ifdef DEBUG
      if((debugOutput > 2) && (l != -1)) {
	printf("%i: ",sati->dlevel);
	printliteral(sati,l);
	printf("\n");
      }
#endif

#ifdef DEBUG
#ifndef GLOBALUS
      printf("Assigning "); printTlit(sati,l); printf(" at level %i with stack at %i.\n",sati->dlevel+1,sati->endunitstack+1); fflush(stdout);
#endif
#endif
      sati->decisions += 1;

#ifdef ASSERTS
      /* Here we have a guarantee that l is at this point unassigned!? */
      assert(VALVALUE(VAR(l)) == UNASS);
#endif

#ifdef GLOBALUS
      declevels[sati->dlevel] = GLOBALendunitstack+1;
#else
      declevels[sati->dlevel] = sati->endunitstack+1;
#endif
      sati->dlevel += 1;
      assert(sati->dlevel < MAXDECLEV);

      q = simpleaddtoqueue(sati,l,REASON_DECISION,sati->dlevel,sati->vars);
#ifdef ASSERTS
      assert(q == 0);
#endif
    }

  } while(maxconflicts);

  /* Satisfiability status not determined. Probably do a restart. */

  /* Undo assignments. */

  undo_assignments_until_level(sati,0);

#ifdef GLOBALUS
  GLOBALendunitstack = declevels[0]-1;
  GLOBALstartunitstack = declevels[0];
#else  
  setUPstacktop(sati,declevels[0]-1);
#endif
  
  sati->dlevel = 0;
  if(returnUCC(sati,0)) goto UNSAT;
  sati->NofUCClauses = 0;
  
  return -1;

 UNSAT:
  sati->value = 0;
  return 0;
 SAT:
  printf("SAT (%i decisions %i conflicts)\n",sati->decisions,sati->conflicts);
  sati->value = 1;
  return 1;
}

int solve(satinstance sati) {
  int result;
  int interval;
  interval = 32;
  do {
    result = solve0(sati,interval);
    interval = interval + 5;
  } while(result == -1);
  return result;
}

/* TODO: This function should go through all the clauses that
  are specific to the instance (the time-dependent ones) and
  free them in a way that makes it possible to clausedb.c to
  reuse their space. */

void freeinstance(satinstance sati) {
  if(sati->lits) { free(sati->lits); sati->lits = NULL; }
  if(sati->vars) { free(sati->vars); sati->vars = NULL; }
  if(sati->initialunittable) { free(sati->initialunittable); sati->initialunittable = NULL; }
#ifndef GLOBALUS
  if(sati->unitstack) { free(sati->unitstack); sati->unitstack = NULL; }
#endif

#ifdef SPREAD
  {
    int i;
    for(i=0;i<sati->nOfVars;i++) {
      free(sati->al2itsT[i]);
    }
    free(sati->al2itsT);
  }
#endif

#ifdef VSIDS
  if(sati->hindex) { /* free(sati->hindex); */ sati->hindex = NULL; }
  if(sati->scoreheap) { freeheap(sati->scoreheap); sati->scoreheap = NULL; }
#endif
}


void showvaluation(satinstance sati) {
  int i;
  printf("Valuation:");
  for(i=0;i<sati->nOfVars;i++)
    if(VALVALUE(i) == 1) {
      printf(" ");
      printTlit(sati,PLIT(i));
    }
  printf("\n");
}


/*************************************************************************/
/** Heap for keeping track of highest score variables                   **/
/*************************************************************************/

/* Calculate the index of the parent of an index. */

int parent(int i) {
  return (i-1) >> 1;
}

/* Calculate the index of the 1st child of an index. */

int child1(int i) {
  return i*2+1;
}

/* Calculate the index of the 2nd child of an index. */

int child2(int i) {
  return i*2+2;
}

#ifdef VSIDS
/* Test whether the heap is empty. */

int heap_emptyp(satinstance sati) {
  return (sati->scoreheap->els == 0);
}

/* Return true if index is a leaf. */

int leafp(heap h,int index) {
  return (child1(index) >= h->els);
}

heap heap_create(int elements) {
  heap h;
  h = (heap)ownalloc(sizeof(struct _heap));
  h->els = 0;
  h->maxels = elements;
  h->array = (pair *)ownalloc(sizeof(pair) * elements);
  return h;
}

void freeheap(heap h) {
  free(h->array);
  free(h);
}

/* After moving a non-top element to the top, restore the heap property. */

inline int heap_property_down(satinstance sati,int index) {
  int c1index,c2index;
  int swap;
  int key,val;
  heap h;

  h = sati->scoreheap;

#ifdef ASSERTS
  assert(index < h->els && index >= 0);
#endif

  key = h->array[index].k;
  val = h->array[index].v;

  c1index = child1(index);
  c2index = child2(index);

  /* Should one child be higher than the parent? */

  while(((c1index < h->els && h->array[c1index].v > val) || (c2index < h->els && h->array[c2index].v > val))) {	/* Not a leaf */

    if(c2index < h->els) {
      if(h->array[c1index].v > h->array[c2index].v) swap = c1index;
      else swap = c2index;
    } else swap = c1index;

#ifdef ASSERTS
    assert(index < h->els && index >= 0);
#endif

    /* Move child to the parent's node. */

    h->array[index].k = h->array[swap].k;
    h->array[index].v = h->array[swap].v;

    HINDEX(h->array[index].k) = index;

    /* Continue with the child's node. */

    index = swap;

    c1index = child1(index);
    c2index = child2(index);

  }

  /* Leave the node here when low enough. */

  h->array[index].k = key;
  h->array[index].v = val;

  HINDEX(h->array[index].k) = index;

  return index;
}

void printheap(satinstance sati) {
  heap h = sati->scoreheap;
  int node;
  for(node=0;node<h->els;node++) {
    printf("(%i,%i) ",h->array[node].k,h->array[node].v);
  }
  printf("\n");
}

/* Get the top element of the heap (with the highest value). */

int heap_taketop(satinstance sati) {
  int key;
  heap h = sati->scoreheap;

#ifdef ASSERTS
  assert(h->els);
#endif

  key = h->array[0].k;

  if(h->els > 1) { /* Move last element to top and then push down. */

    h->array[0].k = h->array[h->els-1].k;
    h->array[0].v = h->array[h->els-1].v;

    HINDEX(h->array[0].k) = 0;

  }
  HINDEX(key) = -1;

  h->els -= 1;

  if(h->els) heap_property_down(sati,0);

  return key;
}

/* Move a new element to an appropriate place in the heap. */

void heap_property_up(satinstance sati,int index) {
  int pindex;
  int key,val;
  heap h = sati->scoreheap;

#ifdef ASSERTS
  assert(index >= 0 && index < h->els);
#endif

  key = h->array[index].k;
  val = h->array[index].v;

  pindex = parent(index);
  while(index > 0 && val > h->array[pindex].v) {
    /* Move parent down. */
    h->array[index].k = h->array[pindex].k;
    h->array[index].v = h->array[pindex].v;

    HINDEX(h->array[index].k) = index;

    /* Continue with the parent. */
    index = pindex;
    pindex = parent(index);
  }
  /* When all parents are OK, leave the new element here. */
  h->array[index].k = key;
  h->array[index].v = val;

  HINDEX(h->array[index].k) = index;
}

/* Add new element to the heap. */

void heap_insert(satinstance sati,int key,int val) {
  heap h = sati->scoreheap;

  /* You could speed this a bit up by not writing key and val to memory yet. */
  /* But this would be for heap_insert only. */
  h->array[h->els].k = key;
  h->array[h->els].v = val;
  h->els += 1;
#ifdef ASSERTS
  assert(h->els <= h->maxels);
#endif
  heap_property_up(sati,h->els-1);
}

/* Increment the value of a heap element. */

void heap_increment(satinstance sati,int index) {
  heap h = sati->scoreheap;

  h->array[index].v += 1;
  heap_property_up(sati,index);
}

void heap_increment_by(satinstance sati,int index,int n) {
  heap h = sati->scoreheap;

  h->array[index].v += n;
  heap_property_up(sati,index);
}

/* Perform the decay operation, i.e. halve the value of each element. */

void heap_decay(satinstance sati) {
  int i;
  heap h = sati->scoreheap;
  for(i=0;i<h->els;i++) h->array[i].v = (h->array[i].v) >> 1;
}

/* Delete an arbitrary element from the heap. */

void heap_delete(satinstance sati,int index) {
  int loc;
  heap h = sati->scoreheap;

#ifdef ASSERTS
  assert((h->els > index) && (index >= 0));
#endif

  h->array[index].v = -1000;
  loc = heap_property_down(sati,index);

#ifdef ASSERTS
  assert(leafp(h,loc));
#endif

  h->array[loc].k = h->array[h->els-1].k;
  h->array[loc].v = h->array[h->els-1].v;

  HINDEX(h->array[loc].k) = loc;

  h->els -= 1;

  if(loc < h->els) heap_property_up(sati,loc);
}

void checkheapproperty(heap h) {
  int i;
  for(i=1;i<h->els;i++) {
    assert(h->array[parent(i)].v >= h->array[i].v);
  }
}

void checkheapconsistency(satinstance sati) {
  int i;
  heap h = sati->scoreheap;

  for(i=0;i<h->els;i++) {
    assert(HINDEX(h->array[i].k) == i);
  }

  checkheapproperty(h);
}
#endif




/*************************************************************************/
/**  UPLA                                                               **/
/*************************************************************************/

int UPLA(satinstance sati) {
  int i,k;
  int tmp;
  int cnt,inferred,gotsomething;

  cnt = 0;
  inferred = 0;

#ifdef DEBUG
  printf("DOING UPLA\n");
#endif

  do {

    cround += 1;
    gotsomething = 0;

  for(k=0;k<2*sati->nOfVars;k++) {
#ifdef DEBUG
    printf("/");
#endif

    if(k&1) i = sati->nOfVars-(k >> 1);
    else i = (k >> 1);

    if(VALVALUE(VAR(i)) == UNASS
       && (((VAR(i) >= sati->nOfVarsPerTime) && (VALVALUE(VAR(i)-sati->nOfVarsPerTime) != UNASS))
	   || ((VAR(i) < sati->nOfVars-sati->nOfVarsPerTime) && (VALVALUE(VAR(i)+sati->nOfVarsPerTime) != UNASS)))
       && (wasseen[i] != cround)) {
      
      cnt += 1;

#ifdef DEBUG
      printTlit(sati,i);
#endif

      setUPstacktop(sati,-1);

      baresimpleaddtoqueue(sati,i,sati->vars);
      tmp = barepropagate(sati);
#ifdef DEBUG
#ifndef GLOBALUS
      printf(" %i",sati->endunitstack);
#endif
#endif


      if(tmp) {
	gotsomething = 1;
	undosetlits(sati);
	inferred += 1;
	if(simpleaddtoqueue(sati,NEG(i),REASON_INPUT,0,sati->vars)) return 1;
	if(propagate(sati,0)) return 1;
      } else {
	WASundosetlits(sati);
      }

      setUPstacktop(sati,-1);

    }
  }

  } while(gotsomething);

#ifdef DEBUG
  printf("FINISHED DOING UPLA\n");
#endif
  printf("Did UPLA for %i literals; inferred %i.\n",cnt,inferred);
  return 0;
}

/* UPLA for state variables only. */

int svarUPLA(satinstance sati) {
  int i,t;
  int tmp;
  int cnt;

  cnt = 0;

#ifdef DEBUG
  printf("DOING UPLA\n");
#endif

  cround += 1;

  for(t=1;t<sati->nOfTPoints-1;t++) {  
  for(i=0;i<2*sati->nOfSVars;i++) {
#ifdef DEBUG
    printf("/");
#endif

    if(VALVALUE(VAR(TVAR(i,t))) == UNASS && (wasseen[TVAR(i,t)] != cround)) {
      
      cnt += 1;

#ifdef DEBUG
      printTlit(sati,i);
#endif

      setUPstacktop(sati,-1);

      simpleaddtoqueue(sati,TVAR(i,t),REASON_DECISION,0,sati->vars);
      tmp = propagate(sati,0);
#ifdef DEBUG
#ifndef GLOBALUS
      printf(" %i",sati->endunitstack);
#endif
#endif


      if(tmp) {
	undosetlits(sati);
	if(simpleaddtoqueue(sati,NEG(TVAR(i,t)),REASON_INPUT,0,sati->vars)) return 1;
	if(propagate(sati,0)) return 1;
      } else {
	WASundosetlits(sati);
      }

      setUPstacktop(sati,-1);

    }
  }
  }
#ifdef DEBUG
  printf("FINISHED DOING UPLA\n");
#endif
  printf("Did UPLA for %i literals.\n",cnt);
  return 0;
}
