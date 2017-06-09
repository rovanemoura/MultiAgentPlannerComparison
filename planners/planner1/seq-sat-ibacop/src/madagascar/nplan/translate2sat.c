
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>
#include <assert.h>
#include <malloc.h>

#include "main.h"
#include "asyntax.h"
#include "tables.h"
#include "intsets.h"
#include "ordintsets.h"
#include "operators.h"
#include "dimacs.h"
#include "invariants.h"
#include "NEWNEWscc.h"

#include "../nsat/interface.h"
#include "../nsat/clausedb.h"

#include "translate2sat.h"

#define noDEBUG

#define OPtag	0x10000000
#define VARtag	0x20000000
#define AUXtag	0x30000000
#define NEGtag	0x40000000
#define NEXTtag	0x80000000
#define TYPEtag	0x30000000
#define INDEX	0x0FFFFFFF

/* Tags for op, var, aux encoding */

#define OP(n) ((n)|OPtag)
#define SVAR(n) ((n)|VARtag)
#define AUX(n) ((n)|AUXtag)
#define NEXT(n) ((n)|NEXTtag)

#define fmaOP(n) (Fatom(OP(n)))
#define fmaVAR(n) (Fatom(SVAR(n)))
#define fmaAUX(n) (Fatom(AUX(n)))

#define VARINDEX(v) ((v)&INDEX)
#define VARNEXT(v) ((v)&NEXTtag)
#define VARTYPE(v) ((v)&TYPEtag)

/* Tags for DIMACs clause encoding */

#define INITtag	0x20000000
#define GOALtag	0x40000000
#define TRtag	0x60000000
#define TIMEtags	0x60000000
#define LENBITS	0x0FFFFFFF

void outputDIMACS();
satinstance outputNSAT(int,int);


int nOfAux;
int nOfClauses;
int nOfTClauses;

int allocAUX(int n) {
  int temp;
  temp = nOfAux;
  nOfAux += n;
  return temp;
}

/* Functions for handling formula sets and translating them into CNF. */

typedef enum { inittime, goaltime, transition } formulaclass;

typedef struct {
  formulaclass cl;
  fma *f;
} timedfma;

int nOfFmas;
int maxFmas;

timedfma *fmas;

void initformuladb() {
  nOfAux = 0;
  nOfFmas = 0;
  maxFmas = 50000;
  fmas = (timedfma *)malloc(maxFmas * sizeof(timedfma));
}

void addformula(formulaclass c,fma *f) {
  nOfFmas += 1;

  /* Table size exceeded */
  if(nOfFmas > maxFmas) {
    maxFmas += 50000;
    fmas = (timedfma *)realloc(fmas,maxFmas * sizeof(timedfma));
  }

  fmas[nOfFmas-1].cl = c;
  fmas[nOfFmas-1].f = f;
}

/* Make a copy of a formula with each state variable tagged with
   the StateVar tag (to distinguish it from the action and auxiliary variables.
*/

fma *makeVARfma(fma *f) {
  fmalist *l;
  fma *nf;
  nf = (fma *)malloc(sizeof(fma));
  nf->t = f->t;
  switch(f->t) {
  case patom:
  case natom:
    nf->a = SVAR(f->a); break;
  case disj:
  case conj:
    nf->juncts = NULL;
    l = f->juncts;
    while(l != NULL) {
      nf->juncts = fmacons(makeVARfma(l->hd),nf->juncts);
      l = l->tl;
    }
    break;
  default: 1;
  }
  return nf;
}

/* Make a copy of a formula with the NEXT version of each state variable. */

fma *fmaNEXT(fma *f) {
  fmalist *l;
  switch(f->t) {
  case patom:
  case natom:
    f->a = NEXT(f->a); break;
  case disj:
  case conj:
    l = f->juncts;
    while(l != NULL) {
      fmaNEXT(l->hd);
      l = l->tl;
    }
    break;
  default: 1;
  }
  return f;
}


/* How many bits are needed for expressing numbers from 1 to n? */

int bitsneeded(int n) {
  int cnt;
  cnt = 0;
  while(n > 0) {
    n = n >> 1;
    cnt += 1;
  }
  return cnt;
}

/* Return a conjunction of literals, with the positives in the first list
   and the negatives in the second. */

fma *effectsof(intlist *pos,intlist *neg) {
  fmalist *fs;

  fs = NULL;

  while(pos != NULL) {
    fs = fmacons(fmaVAR(NEXT(pos->hd)),fs);
    pos = pos->tl;
  }

  while(neg != NULL) {
    fs = fmacons(Fneg(fmaVAR(NEXT(neg->hd))),fs);
    neg = neg->tl;
  }
  return Fconj(fs);
}

/* Add a new action/effect to the data structure that enumerates for
   each literal all the possible ways of making it true. */

void storeinCEs(int positive,intlist *effects,int var,fma *precon) {
  CEstruct *s;
  int i;
  for(;effects != NULL;effects = effects->tl) {
    s = (CEstruct *)malloc(sizeof(CEstruct));
    if(positive) i = PLIT(effects->hd); else i = NLIT(effects->hd);
    s->next = CEs[i];
    s->var = var;
    s->condition = precon;
    s->disjunctive = disjunctivep(precon);
    CEs[i] = s;
  }
}

/* Count the number of ways a literal can be made true. */

int nOfCEs(CEstruct *ptr) {
  int n;
  n = 0;
  while(ptr != NULL) {
    n = n + 1;
    ptr = ptr->next;
  }
  return n;
}

/* Create a compact data structure with references to effect variables
   and the associated (pre)conditions. This is used when the flagCEvariables
   is true, and it is used for the computation of the heuristic.
*/

void compactCEs() {
  int i,j;
  int len;
  CEstruct *ptr;
  for(i=0;i<nOfAtoms*2;i++) {
    len = nOfCEs(CEs[i]);
    cCEs[i] = (compactCEstruct *)malloc((len+1)*sizeof(compactCEstruct));
    ptr = CEs[i];
    //    printlit(i); printf(":");
    for(j=0;j<len;j++) {
      cCEs[i][j].var = finalNOTIME(ptr->var);
      cCEs[i][j].disjunctive = ptr->disjunctive;
      cCEs[i][j].condition = ptr->condition;
      //      printf(" ");
      //      printvar(ptr->var); printf(" ");
      //      printfma(ptr->condition); printf(",");
      ptr = ptr->next;
    }
    //    printf("\n");
    cCEs[i][len].var = -1;
  }
}

void translateaction(int i) {
  eff *e;
  fma *ef;
  int aux;

  /* Precondition axiom */

  addformula(transition,Fimpl(fmaOP(i),makeVARfma(actions[i].precon)));

  /* Effect axioms */

  e = actions[i].effects;

  if(flagCEvariables == 0) { /* Do the regular translation. */

    while(e != NULL) {
      ef = effectsof(e->poseffects,e->negeffects);
      addformula(transition,Fimpl(fmaOP(i),Fimpl(makeVARfma(e->condition),ef)));
      e = e->tl;
    }

  } else { /* Do the translation with variables for conditional effects. */

    while(e != NULL) {
      ef = effectsof(e->poseffects,e->negeffects);
      if(e->condition->t == TRUE) { /* The condition is always true. */
	addformula(transition,Fimpl(fmaOP(i),ef));
	/* Store
	     - the action variable, to be used by the heuristic AND
	     - the associated precondition
	*/
	storeinCEs(1,e->poseffects,OP(i),actions[i].precon);
	storeinCEs(0,e->negeffects,OP(i),actions[i].precon);
      } else {
	aux = allocAUX(1);
	addformula(transition,Fimpl(fmaOP(i),Fimpl(makeVARfma(e->condition),fmaAUX(aux))));
	addformula(transition,Fimpl(fmaAUX(aux),ef));
	addformula(transition,Fimpl(fmaAUX(aux),makeVARfma(e->condition)));
	addformula(transition,Fimpl(fmaAUX(aux),fmaOP(i)));
	/* Store
	     - the auxiliary variable, to be used by the heuristic AND
	     - the associated condition and precondition
	*/
	storeinCEs(1,e->poseffects,AUX(aux),Fconj2(e->condition,actions[i].precon));
	storeinCEs(0,e->negeffects,AUX(aux),Fconj2(e->condition,actions[i].precon));
      }
      e = e->tl;
    }

  }
}

/* Print encoding */

void printvar(int v) {
  if(v & NEGtag) printf("-");
  if(VARNEXT(v)) printf("*");
  switch(VARTYPE(v)) {
  case AUXtag: printf("AUX%i",VARINDEX(v)); break;
  case VARtag: printatomi(VARINDEX(v)); break;
  case OPtag: printf("OP%i",VARINDEX(v)); break;
  }
}

void printFfmalist(fmalist *,char *);
void printFfma(fma *f) {
  switch(f->t) {
  case patom: printvar(f->a); break;
  case natom: printf("-"); printvar(f->a); break;
  case conj:
    printf("(");
    printFfmalist(f->juncts,"&");
    printf(")");
    break;
  case disj:
    printf("(");
    printFfmalist(f->juncts,"|");
    printf(")");
    break;
  case TRUE: printf("TRUE"); break;
  case FALSE: printf("FALSE"); break;
  }
}

void printFfmalist(fmalist *l,char *sep) {
  if(l == NULL) return;
  printFfma(l->hd);
  if(l->tl != NULL) printf("%s",sep);
  printFfmalist(l->tl,sep);
}

/* Construct formula expressing conditions when var becomes
   true or false in terms of an applied operator + additional conditions. */

fma *makes(int val,int var) {
  int i;
  intlist *l,*iterate;
  fma *f;
  fmalist *fs0,*fs;
  eff *e;

  fs = NULL;
 
  if(val == 1) OSstart(effectoccP[var],&iterate);
  else OSstart(effectoccN[var],&iterate);

  while(OSnext(&i,&iterate)) {

    assert(i >= 0);
   
    fs0 = NULL; /* Disjuncts of the condition for one operator */
   
    e = actions[i].effects;

    while(e != NULL) {

      if(val == 1) l = e->poseffects;
      else l = e->negeffects;

      if(member(var,l)) {
	if(e->condition->t == TRUE) { /* Becomes true unconditionally */
	  fs = fmacons(fmaOP(i),fs);
	  goto nextop;
	}
	fs0 = fmacons(makeVARfma(e->condition),fs0); /* Add one disjunct */
      }

      e = e->tl;
    }

    fs = fmacons(Fconj(fmacons(fmaOP(i),fmacons(Fdisj(fs0),NULL))),fs);

  nextop: 1;

  }

  return Fdisj(fs);
  
}

/**********************************************************************/
/* */
/**********************************************************************/

/* Computation of clauses that restrict the parallel execution of
operators for three forms of plans.
- sequential plans with (at most) one action per time point
- parallel plans with A-step semantics [Rintanen et al. 2006]
- parallel plans with E-step semantics [Rintanen et al. 2006]
*/

void SEQUmutexes() {
  int i,j,bit;
  int indexbits;
  int firstindexbit;
  /* Each action sets its index in log2 nOfActions auxiliary variables. */

  /* Allocate auxiliary variables for index bits */
  indexbits = bitsneeded(nOfActions);
  firstindexbit = allocAUX(indexbits);
  
  for(i=0;i<nOfActions;i++) {
    bit = 1;
    for(j=0;j<indexbits;j++) {
      if(i&bit) addformula(transition,Fimpl(fmaOP(i),fmaAUX(firstindexbit+j)));
      else addformula(transition,Fimpl(fmaOP(i),Fneg(fmaAUX(firstindexbit+j))));
      bit = bit << 1;
    }
  }
}

/* Linear encoding for E-step constraints:

For each SCC of a disabling graph
   For each literal l

   Find set M operators that make l true.
   Find set R operators that require l to be true.
   (If either set is empty, skip to the next literal.)

   Generate chain of implications from each o in M to
   -o' for all o' in L such that o < o'.

   An auxiliary variable aux_o is true if any of the preceding
   o in M is true. This way the number of 2-literal clauses is
   linear in |M|+|L|.

Small SCCs (size =< 10): NOT IMPLEMENTED YET!!!
   Generate the trivial constraints o -> -o' for every pair
   o and o' such that
     o < o' and o may affect o'
     Effects are (may be) consistent.
     Preconditions are (may be) consistent.
 */

int auxM[1000000];
int auxR[1000000];

int intCmp(int *a,int *b) {
  if(*a > *b) return 1;
  else return 0;
}

/* Optimization to the chain encoding:
   if few operators are included, generate binary mutexes. */

void ESTEPprod(int NM, int NR) {
  int i,j,op1,op2;
  for(i=0;i<NM;i++) {
    for(j=0;j<NR;j++) {
      op1 = auxM[i];
      op2 = auxR[j];
      if(op1 == op2) continue;
      if(!parallel(op1,op2)) continue;
      /* Emit a binary clause for mutual exclusion. */
      addformula(transition,Fimpl(fmaOP(op1),Fneg(fmaOP(op2))));
    }
  }
}

/* Check whether there are any two actions without contradicting effects
   or preconditions. */

int noparallels(int NM, int NR) {
  int i,j,op1,op2;
  for(i=0;i<NM;i++) {
    for(j=0;j<NR;j++) {
      op1 = auxM[i];
      op2 = auxR[j];
      if(op1 != op2 && parallel(op1,op2)) {
	//	printactionname(op1); printactionname(op2); printf(" ARE PARALLEL\n");
	return 0;
      } else {
	//	printactionname(op1); printactionname(op2); printf(" ARE *N*O*T* PARALLEL\n");
      }
    }
  }
  return 1;
}

/* Compute E-step axioms for variable i.
   WHAT parameter:
   0: Do the chain for an SCC of the disabling graph.
   1: Do the chain from 0..nOfActions-1.
   2: Do the chain from nOfActions-1..0.
 */

void ESTEPchain(int i,sccs s,int WHAT) {
  int NM,NR,pM,pR,X,special,tmp,neednewaux,j,op;
  NM = 0;
  NR = 0;

  switch(WHAT) {
  case 1:
    for(j=0;j<nOfActions;j++) {
      if(canmaketrue(j,i)) auxM[NM++] = j;
      if(isaffectedby(j,i) && Lparallel(i,j)) auxR[NR++] = j;
    }
    break;
  case 2:
    for(j=nOfActions-1;j>=0;j--) {
      if(canmaketrue(j,i)) auxM[NM++] = j;
      if(isaffectedby(j,i) && Lparallel(i,j)) auxR[NR++] = j;
    }
    break;
  case 0:
    for(j=0;j<s->NofEls;j++) {
      if(canmaketrue(s->els[j],i)) auxM[NM++] = s->els[j];
      if(isaffectedby(s->els[j],i) && Lparallel(i,s->els[j])) auxR[NR++] = s->els[j];
    }

    /* Both auxR and auxM are sorted to ascending order. */

    qsort(auxR,NR,sizeof(int),intCmp);
    qsort(auxM,NM,sizeof(int),intCmp);
    break;
  }

  /* WARNING: The code that follows assumes that auxR and auxM are sorted
     in an ascending order. ASTEPmutexes tries to use this in descending
     order, and therefore produces non-A-step plans. */

  if(NM == 0 || NR == 0) return; /* Nothing to do */

  if(NM == 1 && NR == 1 && auxR[0] == auxM[0]) return; /* Nothing to do */

  if(NM*NR <= (NM+NR)*3) { ESTEPprod(NM,NR); return; }
 
  if(NM*NR < 5000 && noparallels(NM,NR)) return;

  //	printf("%i modify and %i require ",NM,NR);
  //	if(i&i) { printf("-"); printatomi(i >> 1); }
  //	else printatomi(i >> 1);
  //	printf("\n");

#ifdef DEBUG
  printf("PARALLELISM AXIOMS %i %i for ",NM,NR);
  printlit(i); printf("\n");
  
  for(j=0;j<NM;j++) { printf("%i ",auxM[j]); printactionname(auxM[j]); }
  printf("\n");
  for(j=0;j<NR;j++) { printf("%i ",auxR[j]); printactionname(auxR[j]); }
  printf("\n");

#endif

  X = -1;
  neednewaux = 0;
  
  pM = 0; pR = 0;

  while(pM < NM || pR < NR) {

    if(pM < NM && auxM[pM] < auxR[pR]) op = auxM[pM]; else op = auxR[pR];

    //	  printf("%i@%i!",pM,pR);
    
    if(pR < NR && op == auxR[pR]) { /* Operator may need to be disabled. */
      /* disable the operator */
      if(X != -1) {
	addformula(transition,Fimpl(Fatom(X),Fneg(fmaOP(op))));
#ifdef DEBUG
	printFfma(Fatom(X)); printf(" implies "); printFfma(Fneg(fmaOP(op))); printf("\n");
#endif
      }
      neednewaux = 1;
    }

    if(pM < NM && op == auxM[pM]) { /* Operator may disable */

      /* FIX: the last AUX may be unnecessary. */

      if(neednewaux && X != -1) {
	tmp = AUX(allocAUX(1));
	addformula(transition,Fimpl(Fatom(X),Fatom(tmp)));
#ifdef DEBUG
	printFfma(Fatom(X)); printf(" implies "); printFfma(Fatom(tmp)); printf("\n");
#endif
	X = tmp;
      }

      neednewaux = 0;
      
      if(X == -1) {
	neednewaux = 1;
	X = OP(op);
      } else {
	addformula(transition,Fimpl(fmaOP(op),Fatom(X)));
#ifdef DEBUG
	printFfma(fmaOP(op)); printf(" implies "); printFfma(Fatom(X)); printf("\n");
#endif
      }
    }

    if(auxR[pR] == op) pR += 1;
    if(auxM[pM] == op) pM += 1;
    
  }
}

/* This is the linear-size encoding of A-step mutexes. */

void ASTEPmutexesLINEAR() {
  int i;
  for(i=0;i<2*nOfAtoms;i++) { /* Go through all literals. */
    ESTEPchain(i,NULL,1);
    ESTEPchain(i,NULL,2);
  }
}

/* This is the practically more efficient quadratic encoding of the mutexes. */

int nCands;
int cands[10000];

void ASTEPmutexCANDIDATE(int op1,int op2) {
  int i;
  if(op2 >= op1) return;
  if(!parallel(op1,op2)) return;
  for(i=0;i<nCands;i++) if(cands[i] == op2) return;
  cands[nCands++] = op2;
  assert(nCands < 10000);
}

void ASTEPprecond(int op,fma *f,int polarity) {
  fmalist *fs;
  intlist *tmp;
  int op2;

  switch(f->t) {

  case conj:
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      ASTEPprecond(op,fs->hd,polarity);
      fs = fs->tl;
    }
    break;

  case patom:
  case natom:

    if(polarity == 0 || f->t == patom) {
      OSstart(effectoccN[f->a],&tmp);
      while(OSnext(&op2,&tmp)) {
	ASTEPmutexCANDIDATE(op,op2);
      }
    }

    if(polarity == 0 || f->t == natom) {
      OSstart(effectoccP[f->a],&tmp);
      while(OSnext(&op2,&tmp)) {
	ASTEPmutexCANDIDATE(op,op2);
      }
    }

    break;

  default: break;

  }
}

void ASTEPmutexes() {
  int i,j,op;
  eff *es;
  intlist *ls;
  intlist *tmp;
  for(i=0;i<nOfActions;i++) { /* Go through all actions. */
    /* Locate ones that interfere and can be taken simultaneously. */
    nCands = 0;
    /* Go through effects. */
    es = actions[i].effects;

    while(es != NULL) {
      /* Go through positive effects. */
      ls = es->poseffects;
      while(ls != NULL) {
	OSstart(preconoccN[ls->hd],&tmp);
	while(OSnext(&op,&tmp)) {
	  ASTEPmutexCANDIDATE(i,op);
	}
	OSstart(condocc[ls->hd],&tmp);
	while(OSnext(&op,&tmp)) {
	  ASTEPmutexCANDIDATE(i,op);
	}
	ls = ls->tl;
      }
      /* Go through negative effects. */
      ls = es->negeffects;
      while(ls != NULL) {
	OSstart(preconoccP[ls->hd],&tmp);
	while(OSnext(&op,&tmp)) {
	  ASTEPmutexCANDIDATE(i,op);
	}
	OSstart(condocc[ls->hd],&tmp);
	while(OSnext(&op,&tmp)) {
	  ASTEPmutexCANDIDATE(i,op);
	}
	ls = ls->tl;
      }
      ASTEPprecond(i,es->condition,1);
      es = es->tl;
    }

    /* Go through preconditions. */
    ASTEPprecond(i,actions[i].precon,0);
    /* Emit parallelism constraints. */
    for(j=0;j<nCands;j++) {
      addformula(transition,Fimpl(fmaOP(i),Fneg(fmaOP(cands[j]))));
    }
  }
}


void ESTEPmutexes() {
  int i;
  sccs s;
  ordintset temp;
  intlist *iterate;

  temp = OScreate();

  /* Go through SCCs. */
  s = SCCS;
  while(s != NULL) {

#ifdef ASDFAWER
    if((s->NofEls > 1) && (s->NofEls <= 5)) { /* Small SCCs specially. */
      for(i=0;i<s->NofEls;i++) {
	for(j=i+1;j<s->NofEls;j++) {
	  addformula(transition,Fimpl(fmaOP(s->els[i]),Fneg(fmaOP(s->els[j]))));
	}
      }
    }
#endif

    if(s->NofEls == 1) goto NEXTSCC;

    if(s->NofEls == 2) {
      addformula(transition,Fimpl(fmaOP(s->els[0]),Fneg(fmaOP(s->els[1]))));
      goto NEXTSCC;
    }

    /* Big SCCs are handled through linearization. */

   if(s->NofEls > nOfActions / 3) {
     //   if(s->NofEls > 2) {
#ifdef DEBUG
    printf("SCC of size %i\n",s->NofEls);
#endif

      for(i=0;i<2*nOfAtoms;i++) { /* Go through all literals */
	ESTEPchain(i,s,0);
      }

    } else { /* Or slightly more cleverly. */

      //      printf("Doing SCC number N of size %i.\n",s->NofEls);

      for(i=0;i<s->NofEls;i++) collectliterals(temp,s->els[i]);

      //      printf("OUTPUTTING axioms: "); fflush(stdout);

      OSstart(temp,&iterate);
      while(OSnext(&i,&iterate)) {
	//	printf("%i:",i); fflush(stdout);
	//	printatomi(i); 
	ESTEPchain(i,s,0);
      }

      //      printf("\n");

      OSmakeempty(temp);

    }
  NEXTSCC:

    s = s->next;
  }
}

int varsPerTime;

#define VALUE(l) (((l)&1)^1)
#define NEG(l) ((l)^1)
#define VAR(l) ((l)>>1)
#define PLIT(v) ((v)<< 1)
#define NLIT(v) (NEG(PLIT(v)))
#define LIT(v) (PLIT(v))

/* Test whether l1 implies l2 directly or through invariants. */

int redundant(int l1,int l2) {
  printlit(l1); printf(" -> "); printlit(l2);
  if(l1 == l2 || ISmember(l2,twolits[NEG(l1)])) {
    //    printf(" IS REDUNDANT\n");
    return 1;
  }
  //    printf(" IS NOT\n");
  return 0;
}

void printlit(int l) {
  if(VALUE(l) == 0) printf("-");
  printatomi(VAR(l));
}

fma *fslit(int succ,int l) {
  int v;
  if(succ) v = NEXT(VAR(l)); else v = VAR(l);
  if(VALUE(l)) return fmaVAR(v); else return Fneg(fmaVAR(v));
}

/**********************************************************************/
/* Encoding a planning problem as a set of propositional formulae     */
/**********************************************************************/

void runalgorithmA(int,int);
void runalgorithmB(double,int);

int clauseptr,maxClauseEls;
int *clauseset;

fma *osubstitute(fma *f,int *a) {
  fma *new;
  fmalist *l;

  switch(f->t) {

  case patom: return Fatom(a[f->a]);
  case natom: return Fnatom(a[f->a]);

  case conj:
  case disj:
    new = (fma *)malloc(sizeof(struct _fma));

    new->t = f->t;

    l = f->juncts;
    new->juncts = NULL;

    while(l != NULL) {
      new->juncts = fmacons(osubstitute(l->hd,a),new->juncts);
      l = l->tl;
    }

    return new;

  default:
    return f;
  }
}

void encodingOgata() {
  int i,j;

    int tempVars[nOfAtoms];
    int tempVarsNEW[nOfAtoms];
    int lastAffecting[nOfAtoms];
    int evars[nOfAtoms],evarcnt;
    fma *epos[nOfAtoms];
    fma *eneg[nOfAtoms];

    intlist *ls;

    /* The encoding in Ogata, Tsuchiya & Kikuno, "SAT-based verification
       of safe Petri nets", ATVA 2004, LNCS 3299, 79-92, Springer 2004.
    */

    /* 
       Initialize an array A that shows which variable represents a given
       state variable x. Initialize A[x] to x@t.

       Go through all actions sequentially.

       For the precondition have o@t -> phi[x] for precondition phi where
       each x has been replaced by A[x].

       For effects
         IF phi1 THEN x := 1
       and
         IF phi0 THEN x := 0
       we introduce new auxiliary variables aux (unless the action is
       the last one affecting x, in which case we define aux = x@t+1.)

       The definition of aux is aux <-> (A[x] & -phi0) V phi1, where
       variable y in phi0 and phi1 have been replaced by A[y].

       Assign A[x] := aux.
       
     */

    /* Initialize state variable array. */

    for(i=0;i<nOfAtoms;i++) tempVars[i] = SVAR(i);

    /* Initialize an array with the last action affecting each variable. */

    for(i=0;i<nOfAtoms;i++) {
      int *ptr;

      lastAffecting[i] = -1;

      ptr = AeffectoccP[i];

      while(*ptr != -1) {
	if(*ptr > lastAffecting[i]) {
	  lastAffecting[i] = *ptr;
	}
	ptr++;
      }

      ptr = AeffectoccN[i];

      while(*ptr != -1) {
	if(*ptr > lastAffecting[i]) {
	  lastAffecting[i] = *ptr;
	}
	ptr++;
      }

      //      printf("Last affecting %i is %i.\n",i,lastAffecting[i]);
    }

    /* Go through all actions. */

    for(i=0;i<nOfActions;i++) {

      eff *e;

      int k;

      evarcnt = 0;

      for(j=0;j<nOfAtoms;j++) {
	tempVarsNEW[j] = tempVars[j];
	//printf("tempvar[%i] = %x.\n",j,tempVars[j]);
      }

      /* Enforce precondition. */

      addformula(transition,Fimpl(fmaOP(i),osubstitute(actions[i].precon,tempVars)));

      /* Define effect. Go through all effects a of the action, and for each
	 construct formulas epos[a] and eneg[a] which respectively correspond
	 to the conditions under which the state variable becomes true or
	 false. */

      e = actions[i].effects;

      while(e != NULL) {

	ls = e->poseffects;

	while(ls != NULL) {

	  //	  printf("Positive effect "); printatomi(ls->hd); printf("\n");

	  j = 0;
	  while(j < evarcnt && evars[j] != ls->hd) {
	    j = j+1;
	  }

	  if(j == evarcnt) {
	    epos[j] = e->condition;
	    eneg[j] = Ffalse();
	    evars[j] = ls->hd;
	    evarcnt += 1;
	  } else {
	    epos[j] = Fdisj2(epos[j],e->condition);
	  }

	  ls = ls->tl;
	}

	ls = e->negeffects;

	while(ls != NULL) {

	  //	  printf("Negative effect "); printatomi(ls->hd); printf("\n");

	  j = 0;
	  while(j < evarcnt && evars[j] != ls->hd) {
	    j = j+1;
	  }

	  if(j == evarcnt) {
	    eneg[j] = e->condition;
	    epos[j] = Ffalse();
	    evars[j] = ls->hd;
	    evarcnt += 1;
	  } else {
	    eneg[j] = Fdisj2(eneg[j],e->condition);
	  }

	  ls = ls->tl;
	}

	e = e->tl;
      }

      /* Create an equivalence tempVar[a]NEW <-> (OPi & epos[a]) V (tempvar[a] & -(OPi & eneg[a]))
	 for every effect a of the action. */

      for(j=0;j<evarcnt;j++) {

	int v;

	v = evars[j];

	if(lastAffecting[v] == i) {
	  k = SVAR(NEXT(v));
	} else {
	  k = AUX(allocAUX(1));
	  //	  printf("Created aux %i for var %i.\n",k&0xffff,v);
	}

	tempVarsNEW[v] = k;

	//	printf("Considering effect "); printatomi(v); printf("\n");

	//	printfma(epos[j]); printf("\n");
	//	printfma(eneg[j]); printf("\n");
	//	fflush(stdout);

	addformula(transition,Fimpl(Fatom(k),
				    Fdisj2(Fconj2(fmaOP(i),
						  osubstitute(epos[j],tempVars)),
					   Fconj2(Fatom(tempVars[v]),
						  Fneg(Fconj2(fmaOP(i),
							      osubstitute(eneg[j],tempVars)))))));

	addformula(transition,Fimpl(Fconj2(fmaOP(i),
					   osubstitute(epos[j],tempVars)),
				    Fatom(k)));

	addformula(transition,Fimpl(Fconj2(Fatom(tempVars[v]),
					   Fneg(Fconj2(fmaOP(i),
						       osubstitute(eneg[j],tempVars)))),
				    Fatom(k)));
      }

      for(j=0;j<nOfAtoms;j++) tempVars[j] = tempVarsNEW[j];

    }

}

fma *conjofCEs(int i) {
  intlist *fs;
  CEstruct *s;
  fs = NULL;
  for(s=CEs[i];s!=NULL;s=s->next) {
    fs = intcons(Fatom(s->var),fs);
  }
  if(fs == NULL) return Ftrue();
  else return Fconj(fs);
}

fma *disjofCEs(int i) {
  intlist *fs;
  CEstruct *s;
  fs = NULL;
  for(s=CEs[i];s!=NULL;s=s->next) {
    fs = intcons(Fatom(s->var),fs);
  }
  if(fs == NULL) return Ffalse();
  else return Fdisj(fs);
}

void encoding() {
  int i,j,ptr,len;

  if(firstTimePoint > lastTimePoint) {
    printf("Check -F %i and -T %i: first time point > last\nExiting...",firstTimePoint,lastTimePoint);
    exit(0);
  }

  initformuladb();

  for(i=0;i<nOfAtoms;i++) {
    if(initialstate[i] == 1) addformula(inittime,fmaVAR(i));
    else addformula(inittime,Fneg(fmaVAR(i)));
  }

  addformula(goaltime,makeVARfma(goal));

  if(planSemantics == EStepOgata) {

    encodingOgata();

  } else {

    CEs = (CEstruct **)malloc(nOfAtoms * 2 * sizeof(CEstruct *));
    cCEs = (CEstruct **)malloc(nOfAtoms * 2 * sizeof(CEstruct *));

    for(i=0;i<nOfAtoms*2;i++) CEs[i] = NULL;

    /* Translate actions in the standard way (preconditions, effects). */

    for(i=nOfActions-1;i>=0;i--) translateaction(i);

    /* Frame axioms */

    if(flagCEvariables == 0) { /* The base encoding. */

      for(i=0;i<nOfAtoms;i++) {
	fma *toT,*toF;
	
	/* Condition under which i becomes true */
	toT = makes(1,i);
	/* Condition under which i becomes false */
	toF = makes(0,i);

	addformula(transition,Fimpl(Fneg(fmaVAR(i)),Fimpl(fmaVAR(NEXT(i)),toT)));
	addformula(transition,Fimpl(Fneg(fmaVAR(NEXT(i))),Fimpl(fmaVAR(i),toF)));
	
      }

    } else {

      for(i=0;i<nOfAtoms;i++) {
	addformula(transition,Fimpl(Fneg(fmaVAR(i)),Fimpl(fmaVAR(NEXT(i)),disjofCEs(PLIT(i)))));
	addformula(transition,Fimpl(Fneg(fmaVAR(NEXT(i))),Fimpl(fmaVAR(i),disjofCEs(NLIT(i)))));
      }
      
    }

    /* Mutual exclusion of actions. Dependent on the parallel semantics. */

    printf("Plan type: ");

    switch(planSemantics) {
    case Sequ: /* Sequential semantics one action per time */
      printf("Sequential\n");
      SEQUmutexes();
      break;
    case AStep:	/* parallel A-step semantics */
      printf("A-step\n");
      ASTEPmutexes();
      //    ASTEPmutexesLINEAR();
      break;
    case EStep:	/* parallel E-step semantics */
      printf("E-step\n");
      ESTEPmutexes();
      break;
    case EStepOgata:	/* "sequential" E-step semantics */
      assert(1==0);
      break;
    }

  }

  initclauseset();

  init_clausesets();

  //  printf("Outputting clauses.\n");

  for(i=0;i<nOfFmas;i++) {
    simplifyfma(fmas[i].f);

    if(flagShowInput) {
      switch(fmas[i].cl) {
      case inittime: printf("I:"); break;
      case goaltime: printf("G:"); break;
      case transition: printf("T:"); break;
      }
      printFfma(fmas[i].f); printf("\n");
    }

    produceclauses(fmas[i].f,fmas[i].cl);
  }

  /* Now the number of auxiliary variables is known. */

  varsPerTime = nOfAtoms+nOfActions+nOfAux;

  /* Calculate all variables indices. */

  ptr = 0;
  for(i=0;i<nOfClauses;i++) {
    len = (clauseset[ptr])&LENBITS;
    for(j=1;j<=len;j++) clauseset[ptr+j] = final(clauseset[ptr+j],0);
    ptr = ptr+len+1;
  }

  if(flagCEvariables) compactCEs();

  if(flagOutputDIMACS) {
    outputDIMACS();
  } else {

    if(PLANheuristic == 0) printf("Heuristic: VSIDS\n");

    switch(evalAlgorithm) {
    case 0: runalgorithmA(paramA,outputTimeStep); break;
    case 1: runalgorithmB(paramB,outputTimeStep); break;
    default: 1;
    }
  }
}

void callprintplan(satinstance sati) {
  int usingstd;
  FILE *f;
  if(outputfile == NULL) {
    usingstd = 1;
  } else {
    f = fopen(outputfile,"w");
    if(f == NULL) {
      fprintf(stderr,"WARNING: could not open output file\n");
      usingstd = 1;
    } else {
      usingstd = 0;
    }

  }
  if(usingstd) {
    fprintplan(stdout,sati);
  } else {
    fprintplan(f,sati);
    fclose(f);
  }
}

#define max(A,B) ((A)>(B) ? (A) : (B))
#define min(A,B) ((A)<(B) ? (A) : (B))

void startlength(int i,int len) {
  printf("Horizon %i\n",len);

  seqs[i].sati = outputNSAT(i,len+1);
  seqs[i].calls = 0;
  seqs[i].cost = 0;
  seqs[i].value = -1;
  if(i > 1) seqs[i].sati->al2its = seqs[0].sati->al2its;
}

int gcinterval;
int gccounter;

void init_gc() {
  gcinterval = 1000;
  gccounter = gcinterval;
}

void reset_gccounter(int freed) {
  /* Increase garbage collection interval if collected too little. */
#ifdef DEBUG
  printf("Adjusting garbage collection interval from %i to ",gcinterval);
#endif
  if(freed < 20) gcinterval = gcinterval * 2;
  else if(freed < 100) gcinterval = gcinterval * 3 / 2;
  else if(freed > 800) gcinterval = gcinterval / 8;
  else if(freed > 600) gcinterval = gcinterval / 4;
  else if(freed > 400) gcinterval = gcinterval / 2;
  else if(freed > 200) gcinterval = gcinterval * 2 / 3;
  gccounter = gcinterval;
#ifdef DEBUG
  printf("%i, with %i MB freed.\n",gcinterval,freed);
#endif
}

/* Restart intervals */

int fixed(int i) {
  return flagRestartInterval;
}

/* Luby series:
   t_i = 2^{k-1} if i = 2^k-1
         t_{i-2^{k-1}+1} if 2^{k-1} <= i < 2^k - 1
*/

/*
int luby(int i) {
  int k,p;
  k = 1;
  p = 2;
  while (p < (i+1)) {
    k += 1;
    p *= 2;
  }
  if (p == (i+1)) return (p/2);
  return (luby(i - (p/2) + 1));
}

int funny(int i) {
  switch(i&7) {
  case 0:
  case 1:
  case 2:
  case 4:
  case 5:
  case 6:
    return flagRestartInterval;
  case 3:
    return flagRestartInterval*2/3;
  case 7:
    return flagRestartInterval*2;
  }
}
*/

//#define RESTART luby
#define RESTART fixed
//#define RESTART funny

int instancelength(int i) {
  return i*outputTimeStep+firstTimePoint;
}

void runalgorithmA(int n,int step) {
  int last,active;
  int i,j;
  int interval;

  initclausedb();
  //  init_nondirty();

  active = 0;
  last = -1;
  
  init_gc();

  do {

    /* Start new lengths if there are not enough. */

    while(active < n && instancelength(last+1) <= lastTimePoint) {
      last += 1;
      startlength(last,instancelength(last));
      if(seqs[last].sati->value != 0) active += 1;
    }

    //    printf("solving ..%i with %i active\n",last,active);

    for(i=0;i<=last;i++) {

      if(seqs[i].sati->value == -1) {

	interval = RESTART(seqs[i].calls+1);

	solve0(seqs[i].sati,interval);
#ifdef VSIDS
	seqs[i].sati->VSIDSround = 1-seqs[i].sati->VSIDSround;
#endif
	gccounter -= interval;

	seqs[i].cost += 1;
	seqs[i].calls += 1;

	if(seqs[i].sati->value != -1) {
	  active -= 1;
	  if(seqs[i].sati->value == 1) goto planAfound;
	  printf("%i UNSAT (%i decisions %i conflicts)\n",instancelength(i),seqs[i].sati->decisions,seqs[i].sati->conflicts);
	  for(j=0;j<i;j++) {
	    if(seqs[j].sati->value == -1) {	/* Formulas that must be UNSAT. */
	      active -= 1;
	      seqs[j].sati->value = 0;
	      printf("%i must be UNSAT (%i decisions %i conflicts)\n",instancelength(j),seqs[j].sati->decisions,seqs[j].sati->conflicts);
	    }
	  }
	}
      }

      if(gccounter < 0) {
	int freed;
	freed = garbagecollection();
	reset_gccounter(freed);
      }
    }

#ifdef ASDF
    for(i=0;i<=last;i++) {
      if(seqs[i].sati->value == -1) printf("%i ",instancelength(i));
    }
    printf("\n");

    for(i=0;i<=last;i++) {
      if(seqs[i].sati->value == -1) printf("%i ",seqs[i].calls);
    }
    printf("\n");
#endif

  } while((instancelength(last+1) <= lastTimePoint) || (seqs[last].sati->value == -1));

  printf("PLAN NOT FOUND: steps %i..%i tested\n",firstTimePoint,lastTimePoint);
  return;

 planAfound:
  printf("PLAN FOUND: %i steps\n",instancelength(i));
  callprintplan(seqs[i].sati);
}

double power(double r,int i) {
  int j;
  double value;
  value = 1.0;
  for(j=0;j<i;j++) {
    value = value*r;
  }
  return value;
}

void runalgorithmB(double r,int step) {
  int last,firstactive,actives;
  int threshold;
  int i,j;
  int interval;

  initclausedb();
  //  init_nondirty();

  last = -1;

  init_gc();
  
  actives = 0;

  do {

    firstactive = -1;

    for(i=0;i<=last;i++) {

      if(seqs[i].sati->value == -1) {

	interval = RESTART(seqs[i].calls+1);

	if(firstactive == -1) {
	  firstactive = i;
	  solve0(seqs[i].sati,interval);
#ifdef VSIDS
	  seqs[i].sati->VSIDSround = 1-seqs[i].sati->VSIDSround;
#endif
	  if(seqs[i].sati->value == 0) freeinstance(seqs[i].sati);
	  gccounter -= interval;
	  seqs[i].cost += 1;
	  seqs[i].calls += 1;
	} else {

	  threshold = ((double)(seqs[firstactive].cost))*power(r,i-firstactive)+0.5;
	  if((double)(seqs[i].cost) < threshold) {
	    solve0(seqs[i].sati,interval);
#ifdef VSIDS
	    seqs[i].sati->VSIDSround = 1-seqs[i].sati->VSIDSround;
#endif
	    if(seqs[i].sati->value == 0) freeinstance(seqs[i].sati);
	    gccounter -= interval;
	    seqs[i].cost += 1;
	    seqs[i].calls += 1;
	  }
	}

	if(seqs[i].sati->value != -1) {
	  actives -= 1; // printf("actives = %i\n",actives);
	  if(seqs[i].sati->value == 1) goto planBfound;
	  printf("%i UNSAT (%i decisions %i conflicts)\n",instancelength(i),seqs[i].sati->decisions,seqs[i].sati->conflicts);
	  for(j=0;j<i;j++) {
	    if(seqs[j].sati->value == -1) {	/* Formulas that must be UNSAT. */
	      seqs[j].sati->value = 0;
	      printf("%i must be UNSAT (%i decisions %i conflicts)\n",instancelength(j),seqs[j].sati->decisions,seqs[j].sati->conflicts);
	    }
	  }
	}

	//	if(i==5) printplanT(seqs[i].sati);

      }
    }

    /* Check whether to start new length. */

    if(last > -1) threshold = ((float)(seqs[firstactive].cost))*power(r,last+1-firstactive);

    if((instancelength(last) < lastTimePoint) && (actives < paramM) && (firstactive == -1 || threshold > 0.5) && memoryused() < 1800.0) {
      last += 1;
      startlength(last,instancelength(last));
      actives += 1; // printf("ADD: actives = %i %i\n",actives,paramM);
    }

    if(gccounter < 0) {
      int freed;
      freed = garbagecollection();
      reset_gccounter(freed);
    }
    

  } while((instancelength(last+1) <= lastTimePoint) || (seqs[last].sati->value == -1));

  printf("PLAN NOT FOUND: %i steps tested\n",lastTimePoint);
  return;

 planBfound:
  printf("PLAN FOUND: %i steps\n",instancelength(i));
  callprintplan(seqs[i].sati);
}


/*******************************************************************/
/*    DIMACS output                                                */
/*******************************************************************/

inline int final(int i,int time) {
  int j;
  switch(i&TYPEtag) {
  case AUXtag: j = nOfAtoms+nOfActions; break;
  case VARtag: j = 0; break;
  case OPtag: j = nOfAtoms; break;
  default: assert(1 == 0); // fprintf(stderr,"ERROR: 43346\n"); exit(1); break;
  }
  if(i&NEXTtag) j += varsPerTime;
  j += VARINDEX(i)+time*varsPerTime;
  if(i&NEGtag) return -(j+1); else return j+1;
}

inline int finalNOTIME(int i,int time) {
  int j;
  switch(i&TYPEtag) {
  case AUXtag: j = nOfAtoms+nOfActions; break;
  case VARtag: j = 0; break;
  case OPtag: j = nOfAtoms; break;
  default: assert(1 == 0); // fprintf(stderr,"ERROR: 43346\n"); exit(1); break;
  }
  return j+VARINDEX(i);
}

void initclauseset() {
  nOfClauses = 0;
  nOfTClauses = 0;
  clauseptr = 0;
  maxClauseEls = 65536;
  clauseset = (int *)malloc(maxClauseEls * sizeof(int));
}

void reallocclauseset() {
  maxClauseEls = maxClauseEls * 3 / 2;
  clauseset = (int *)realloc(clauseset,maxClauseEls * sizeof(int));
}

void emitclause(int *vector,int cnt,formulaclass class) {
  int i,tmp;
  nOfClauses += 1;
  if(class == transition) nOfTClauses += 1;
  if(clauseptr + cnt > maxClauseEls) reallocclauseset();
  switch(class) {
  case inittime: tmp = cnt|INITtag; break;
  case goaltime: tmp = cnt|GOALtag; break;
  case transition: tmp = cnt|TRtag; break;
  }
  clauseset[clauseptr++] = tmp;
#ifdef DEBUG
  printf("EMIT: ");
#endif
  for(i=0;i<cnt;i++) {
#ifdef DEBUG
    printvar(vector[i]); printf(" ");
#endif
    clauseset[clauseptr++] = vector[i];
  }
#ifdef DEBUG
  printf("\n");
#endif
}


/*******************************************************************/
/*    CNF transformation                                           */
/*******************************************************************/

#define MAXXCLAUSELEN 20000
int xclauselen;
int xclause[MAXXCLAUSELEN];

#define MAXXSTACK 20000
int xstackptr;
int xstackv[MAXXSTACK];
fma *xstack[MAXXSTACK];

/* Locate all conjuncts, and produce a clause for each. */

void produceclauses(fma *f,formulaclass class) {
  fmalist *fs;

  switch(f->t) {
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      produceclauses(fs->hd,class);
      fs = fs->tl;
    }
    break;
  case disj:
    /* Find all disjuncts, get a literal representing each, and output a clause.
       For non-atomic disjuncts, do the same recursively. */
    if(!biggerthan(f,200)) {
#ifdef DEBUG
      printf("Calling produceclausesENUM with: ");
      printFfma(f);
      printf("\n");
#endif
      produceclausesENUMERATIVE(f,class);
    } else {
      xstackptr = -1;
      xclauselen = 0;
      produceclausesDD(f,class);
      emitclause(xclause,xclauselen,class);
      produceclausesTseitin(class);
    }
    break;
  case patom:
    xclause[0] = f->a;
    emitclause(xclause,1,class);
    break;
  case natom:
    xclause[0] = (f->a)|NEGtag;
    emitclause(xclause,1,class);
  case TRUE: break;
  case FALSE:
    printf("WARNING: Emitting top-level constant FALSE.\n");
    xclause[0] = 0|VARtag;
    emitclause(xclause,1,class);
    xclause[0] = 0|NEGtag|VARtag;
    emitclause(xclause,1,class);
    /* There must be at least one state variable. */
    if(nOfAtoms == 0) nOfAtoms = 1;
    /* Sometimes all vars are eliminated, and we would get errors elsewhere. */
    break;
  }
}

/* Identify the disjuncts of a clause, and put them in the xclause
  array. Disjuncts that are conjunctions are represented by a new
  auxiliary literal, and for each such literal an appropriate equivalence
  is later generated by the produceclausesTseitin procedure.
*/

void produceclausesDD(fma *f,formulaclass class) {
  fmalist *fs;
  int aux;

  switch(f->t) {
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      produceclausesDD(fs->hd,class);
      fs = fs->tl;
    }
    break;
  case conj:
    aux = allocAUX(1);
    xclause[xclauselen++] = AUX(aux);
    xstack[++xstackptr] = f;
    xstackv[xstackptr] = AUX(aux);
    break;
  case patom:
    xclause[xclauselen++] = f->a;
    break;
  case natom:
    xclause[xclauselen++] = (f->a)|NEGtag;
    break;
  case TRUE:
    /* Clause is TRUE: don't generate it!!!!!!!!!!!!!!!!!!!!!!!! */
    assert(1 == 0);
    break;
  case FALSE:
    /* No literal in the clause. */
    break;
  }
}

int produceclausesTseitin(formulaclass class) {
  int type;
  fma *f;
  int aux;

#ifdef DEBUG
  assert(xstackptr >= -1);
  assert(xclauselen >= 0);
  assert(xclauselen < MAXXCLAUSELEN);
  assert(xstackptr < MAXXSTACK);
#endif
  
  while(xstackptr >= 0) {
    /* Pop  x <-> formula from stack and generate clauses. */
    aux = xstackv[xstackptr];
    f = xstack[xstackptr--];

    xclauselen = 0;

    type = f->t;

    /* First disjunct to The Long Clause. */

    switch(type) {
    case disj:
      xclause[xclauselen++] = aux|NEGtag;	/* aux -> l1 V .. V ln */
      break;
    case conj:
      xclause[xclauselen++] = aux;	/* l1 & .. & ln -> aux */
      break;
    default:
      assert(1 == 0);
    }

    /* Generate aux <-> (lit) clauses, and add literals to The Long Clause. */

    pcTseitinRecurse(class,type,aux,f->juncts);

    /* Emit The Long Clause. */

    emitclause(xclause,xclauselen,class);
    
  }
}

void pcTseitinRecurse(int class,int type, int aux, fmalist *fs) {
  fma *f;
  int aux2;
  int c2lause[2];

  while(fs != NULL) {

    f = fs->hd;

    switch(f->t) {

    case conj:

      if(type == conj) pcTseitinRecurse(class,type,aux,f->juncts);
      else {
	aux2 = allocAUX(1);

	xclause[xclauselen++] = AUX(aux2);

	c2lause[0] = aux;
	c2lause[1] = AUX(aux2)|NEGtag;
	emitclause(c2lause,2,class);

	xstack[++xstackptr] = f;
	xstackv[xstackptr] = AUX(aux2);
      }
      break;

    case disj:

      if(type == disj) pcTseitinRecurse(class,type,aux,f->juncts);
      else {
	aux2 = allocAUX(1);

	xclause[xclauselen++] = AUX(aux2)|NEGtag;

	c2lause[0] = aux|NEGtag;
	c2lause[1] = AUX(aux2);
	emitclause(c2lause,2,class);

	xstack[++xstackptr] = f;
	xstackv[xstackptr] = AUX(aux2);
      }
      break;

    case patom:

      if(type == disj) {

	xclause[xclauselen++] = f->a;

	c2lause[0] = aux;
	c2lause[1] = f->a|NEGtag;

      } else {

	xclause[xclauselen++] = (f->a)|NEGtag;

	c2lause[0] = aux|NEGtag;
	c2lause[1] = f->a;

      }

      emitclause(c2lause,2,class);

      break;

    case natom:

      if(type == disj) {

	xclause[xclauselen++] = (f->a)|NEGtag;

	c2lause[0] = aux;
	c2lause[1] = f->a;

      } else {

	xclause[xclauselen++] = f->a;

	c2lause[0] = aux|NEGtag;
	c2lause[1] = f->a|NEGtag;

      }

      emitclause(c2lause,2,class);

      break;

    case TRUE:
      /* Clause is TRUE: don't generate it!!!!!!!!!!!!!!!!!!!!!!!! */
      assert(1 == 0);
      break;
    case FALSE: break;
      assert(1 == 0);
    }

    fs = fs->tl;

  }

}

/* Count the size of the CNF (# of clauses) with the trivial
   transformation based on associativity laws and no auxiliary
   variables.
THIS IS USELESS BECAUSE, WITH 32 BIT INTEGERS, IT OVERFLOWS WITH
MANY NON-STRIPS PROBLEMS, ESPECIALLY ONES THAT CONTAIN QUANTIFICATION.
*/

int trivialsize(fma *f) {
  fmalist *fs;
  int c;
  if(f==NULL) return 1;
  switch(f->t) {
  case TRUE:
  case FALSE:
    return 1;
  case patom:
  case natom:
    return 1;
  case conj:
    fs = f->juncts;
    c = 1;
    while(fs != NULL) {
      c += trivialsize(fs->hd);
      fs = fs->tl;
    }
    return c;
  case disj:
    fs = f->juncts;
    if(fs == NULL) return 1;
    c = 1;
    while(fs != NULL) {
      c = c * trivialsize(fs->hd);
      fs = fs->tl;
    }
    return c;
  }
}

/* Test whether formula is likely to lead clause sets bigger than bound. */

int biggerthan0(fma *f,int bound,int *size) {
  fmalist *fs;
  int c,size0;

  if(f==NULL) {
    *size = 1;
    return 1;
  }

  switch(f->t) {

  case TRUE:
  case FALSE:
  case patom:
  case natom:
    *size = 1;
    return 0;

  case conj:

    fs = f->juncts;
    *size = 0;
    while(fs != NULL) {
      if(biggerthan0(fs->hd,bound,&size0)) return 1;
      *size += size0;
      if(*size > bound) return 1;
      fs = fs->tl;
    }
    return 0;

  case disj:

    fs = f->juncts;
    *size = 1;
    while(fs != NULL) {
      if(biggerthan0(fs->hd,bound,&size0)) return 1;
      *size *= size0;
      if(*size > bound) return 1;
      fs = fs->tl;
    }
    return 0;
  }

  return 0;
}

int biggerthan(fma *f,int bound) {
  int guesstimate;
  if(biggerthan0(f,bound,&guesstimate) ||
     (guesstimate > bound)) return 1;
  else return 0;
}


#define MAXLITERALSCL 2000000
int clauselist[MAXLITERALSCL];
int clausealloc;

/* Trivial CNF transformation by recursively translating subformulas
to clauses, and then recursively combining the clause sets.
For a conjunction, the clauseset is simply the union of the constituent
clause sets.
For a disjunction, the clauseset is the "Cartesian product" of the
clause sets.
*/

void csproduct(int f0,int l0,int f1,int l1,int *f2,int *l2) {
  int i,j,k;
  *f2 = clausealloc;
  *l2 = clausealloc-1;
  i = f0;
  while(i <= l0) {
    j = f1;
    while(j <= l1) {
      /* New clause obtained by concatenation. */
      *l2 = clausealloc;	/* Index of the last clause generated */
      clauselist[clausealloc++] = clauselist[i]+clauselist[j];
      for(k=1;k<=clauselist[i];k++) clauselist[clausealloc++] = clauselist[i+k];
      for(k=1;k<=clauselist[j];k++) clauselist[clausealloc++] = clauselist[j+k];
      j += clauselist[j]+1;
    }
    i += clauselist[i]+1;
  }
}

int copyclauses(int first,int last) {
  int i,tmp;
  tmp = clausealloc;
  i = first;
  while(i <= last+clauselist[last]) {
    clauselist[clausealloc++] = clauselist[i++];
    assert(clausealloc < MAXLITERALSCL);
  }
  return tmp+(last-first);
}

void csconcatenate(int f0,int l0,int f1,int l1,int *f2,int *l2) {
  *f2 = clausealloc;
  copyclauses(f0,l0);
  *l2 = copyclauses(f1,l1);
  return;
}

int onlyliterals(fmalist *fs) {
  while(fs != NULL) {
    switch(fs->hd->t) {
    case patom:
    case natom:
      break;
    default:
      return 0;
    }
    fs = fs->tl;
  }
  return 1;
}

/* Translation from circuits to CNF */

void pc30(fma *f,formulaclass cl,int *first,int *last) {
  fmalist *fs;
  int f0,l0,f1,l1,f2,l2,len;
  switch(f->t) {
  case disj:
    fs = f->juncts;
    if(onlyliterals(fs)) {
      *first = clausealloc;
      *last = clausealloc;
      clausealloc += 1;
      len = 0;
      while(fs != NULL) {
	len += 1;
	switch(fs->hd->t) {
	case patom:
	  clauselist[clausealloc++] = fs->hd->a;
	  break;
	case natom:
	  clauselist[clausealloc++] = fs->hd->a|NEGtag;
	  break;
	default:
	  assert(1 == 0);
	}
	fs = fs->tl;
      }
      clauselist[*first] = len;
    } else {
      pc30(fs->hd,cl,&f0,&l0);
      fs = fs->tl;
      while(fs != NULL) {	/* Repeatedly construct the product. */
	pc30(fs->hd,cl,&f1,&l1);
	csproduct(f0,l0,f1,l1,&f2,&l2);
	f0 = f2;
	l0 = l2;
	fs = fs->tl;
      }
      *first = f0;
      *last = l0;
    }
    return;
  case conj:
    fs = f->juncts;
    if(fs == NULL) {	/* Empty conjunction is the empty clause set. */
      *first = 0;
      *last = -1;
      return;
    }
    pc30(fs->hd,cl,&f0,&l0);
    fs = fs->tl;
    while(fs != NULL) {	/* Repeatedly concatenate. */
      pc30(fs->hd,cl,&f1,&l1);
      csconcatenate(f0,l0,f1,l1,&f2,&l2);
      f0 = f2;
      l0 = l2;
      fs = fs->tl;
    }
    *first = f0;
    *last = l0;
    return;
  case patom:	/* Clause with one positive literal */
    *first = clausealloc;
    *last = clausealloc;
    clauselist[clausealloc] = 1;
    clauselist[clausealloc+1] = f->a;
    clausealloc += 2;
    assert(clausealloc < MAXLITERALSCL);
    break;
  case natom:	/* Clause with one negative literal */
    *first = clausealloc;
    *last = clausealloc;
    clauselist[clausealloc] = 1;
    clauselist[clausealloc+1] = f->a|NEGtag;
    clausealloc += 2;
    assert(clausealloc < MAXLITERALSCL);
    break;
  case TRUE:	/* No clauses. */
    *first = 0;
    *last = -1;
    break;
  case FALSE:	/* One empty clause */
    *first = clausealloc;
    *last = clausealloc;
    clauselist[clausealloc++] = 0;
    assert(clausealloc < MAXLITERALSCL);
    break;
  }
}

void produceclausesENUMERATIVE(fma *f,formulaclass cl) {
  int first,last,c,len;
  clausealloc = 0;
  pc30(f,cl,&first,&last);
  c = first;
  while(c <= last) {
    len = clauselist[c];
    emitclause(clauselist+c+1,len,cl);
    c += len+1;
  }
}

/*******************************************************************/
/*      DIMACS interface                                           */
/*******************************************************************/

void outputDIMACS() {
  int i,j,k,ptr,h,with,len,bias;
  int nOfInvariants;
  char c;
  FILE *F;
  char filename[100];

  printf("DIMACS output\n");

  nOfInvariants = 0;

  for(k=0;k<nOfAtoms;k++) {
    nOfInvariants += IScard(twolits[LIT(k)]);
    nOfInvariants += IScard(twolits[NLIT(k)]);
  }

  for(i=firstTimePoint;i<=lastTimePoint;i+=outputTimeStep) { /* Write files for different lengths. */
    sprintf(filename,"%s-%s.%.3i.cnf",domainname(),problemname(),i);
    printf("Writing %s",filename);
    F = fopen64(filename,"w");
    dimacsheader(F,varsPerTime*(i+1)-nOfActions,nOfClauses+(i-1)*nOfTClauses+i*nOfInvariants/2);
    for(j=0;j<=i;j++) { /* Go through time points for one formula. */
      printf(" %i",j); fflush(stdout);
      ptr = 0;
      bias = j*varsPerTime;
      for(k=0;k<nOfClauses;k++) { /* Output clauses for each time point. */
	with = 0;
	len = clauseset[ptr]&LENBITS;
	switch(clauseset[ptr]&TIMEtags) { /* 1st element = size, timepoints */
	case TRtag: if(j < i) with = 1; break;
	case INITtag: if(j == 0) with = 1; break;
	case GOALtag: if(j == i) with = 1; break;
	default: fprintf(stderr,"ERROR: 56567\n"); exit(1); break;
	}
	ptr += 1;
	if(with) {
	  for(h=ptr;h<ptr+len;h++) {
	    if(clauseset[h] < 0) fprintf(F,"%i ",clauseset[h]-bias);
	    else fprintf(F,"%i ",clauseset[h]+bias);
	  }
	  fprintf(F,"0\n");
	}
	ptr += len;
      }
      /* Output invariants (they are not in clauseset). */
      if(j>0) {
	for(k=0;k<nOfAtoms;k++) {
	  ITstart(twolits[LIT(k)]); /* invariants k V l */
	  while(ITnext(&h)) {
	    if(VAR(h) < k) { /* Only output one of l1 V l2 and l2 V l1. */
	      if(h&1) fprintf(F,"%i -%i 0\n",k+1+bias,VAR(h)+1+bias);
	      else fprintf(F,"%i %i 0\n",k+1+bias,VAR(h)+1+bias);
	    }
	  }
	  ITstart(twolits[NLIT(k)]); /* invariants -k V l */
	  while(ITnext(&h)) {
	    if(VAR(h) < k) { /* Only output one of l1 V l2 and l2 V l1. */
	      if(h&1) fprintf(F,"-%i -%i 0\n",k+1+bias,VAR(h)+1+bias);
	      else fprintf(F,"-%i %i 0\n",k+1+bias,VAR(h)+1+bias);
	    }
	  }
	}
      }
    }
    printf("\n");
    fclose(F);
  }
}


/*******************************************************************/
/*       Call to the Integrated SAT solver                         */
/*******************************************************************/

/* Map literals from 1,-1,2,-2,... to 0,1,2,3,... */

int nsatlit(int l) {
  if(l < 0) return ((-l-1) << 1)|1;
  return ((l-1) << 1);
}

satinstance outputNSAT(int id,int timepoints) {
  int j,k,ptr,h,len;
  clausetype ct;
  satinstance sati;

  sati = newinstance(id,timepoints,varsPerTime,nOfAtoms,nOfActions);
  noT2clauses = (id > 0);
  setheuristic(sati,SATheuristic);
  setpheuristic(sati,PLANheuristic);

  ptr = 0;

  for(k=0;k<nOfClauses;k++) { /* Output clauses. */
    len = clauseset[ptr]&LENBITS;
    switch(clauseset[ptr]&TIMEtags) {
    case TRtag: ct = TransC; break;
    case INITtag: ct = InitC; break;
    case GOALtag: ct = FinalC; break;
    default: fprintf(stderr,"ERROR: 56567\n"); exit(1); break;
    }
    ptr += 1;
    switch(len) {
    case 1: add1clause(sati,nsatlit(clauseset[ptr]),ct); break;
    case 2:
      add2clause(sati,nsatlit(clauseset[ptr]),nsatlit(clauseset[ptr+1]),ct);
      break;
    default:
      addnewclause(sati,len,ct,1);
      for(j=0;j<len;j++) {
	addliteral(sati,j,nsatlit(clauseset[ptr+j]));
      }
      finishclause(sati);
    }
    ptr += len;
  }

  for(k=0;k<nOfAtoms;k++) {
    ITstart(twolits[LIT(k)]); /* invariants k V l */
    while(ITnext(&h)) {
      if(VAR(h) < k) { /* Only output one of l1 V l2 and l2 V l1. */
	add2clause(sati,LIT(k),h,TransC);
      }
    }
    ITstart(twolits[NLIT(k)]); /* invariants -k V l */
    while(ITnext(&h)) {
      if(VAR(h) < k) { /* Only output one of l1 V l2 and l2 V l1. */
	add2clause(sati,NLIT(k),h,TransC);
      }
    }
  }
  //  printf("Total of %i clauses.\n",clausecount);
  instancecomplete(sati);
  if(noT2clauses) sati->al2its = seqs[id-1].sati->al2its;

  return sati;
}
