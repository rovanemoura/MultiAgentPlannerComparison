
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>
#include <malloc.h>

#include "asyntax.h"
#include "intsets.h"
#include "ordintsets.h"
#include "operators.h"
#include "tables.h"
#include "invariants.h"
#include "main.h"

#define noDEBUG

/* Local copies (for inlining) of intsets.c functions */

int *iITptr;
int iITcounter;

void iITstart(intset s) {
  iITcounter = s->nOfEls;
  iITptr = s->elements;
}

int iITnext(int *i) {
  if(iITcounter <= 0) return 0;
  iITcounter -= 1;
  *i = *(iITptr++);
  return 1;
}

//#define iITnext ITnext
//#define iITstart ITstart


int *onelits;
intset *twolits;

/* Preprocessing for operators:
  For each operator and conditional effect compute

  A. list of literals guaranteed to be true before
     the conditional effect is executed

  B. list of literals guaranteed to be true after

Literals derived from the precondition are included in lists A.

If there are unconditional effects (condition TRUE) then
these are included in all lists B.

Literals in list A that are guaranteed not to be falsified
by a simultaneous conditional effect are included in B.

*/

typedef struct _llist {
  intset before;
  intset after;
  struct _llist *next;
}  llist;

/* Compute effect literals consisting of the positive, the negative
   and the ones in a conditional effect with an always-true condition. */

void addeffectlits(intlist *posl,intlist *negl,eff *current,eff *all,intset s) {
  while(posl != NULL) { ISinsert(LIT(posl->hd),s); posl = posl->tl; }
  while(negl != NULL) { ISinsert(NLIT(negl->hd),s); negl = negl->tl; }
  while(all != NULL) {
    if(all->condition->t == TRUE && all != current) {
      posl = all->poseffects;
      while(posl != NULL) {
	ISinsert(LIT(posl->hd),s);
	posl = posl->tl;
      }
      negl = all->negeffects;
      while(negl != NULL) {
	ISinsert(NLIT(negl->hd),s);
	negl = negl->tl;
      }
    }
    all = all->tl;
  }
}

void guaranteedtrue(fma *f,intset ac) {
  fmalist *fs;
  switch(f->t) {
  case FALSE:
  case TRUE:
    break;
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      guaranteedtrue(fs->hd,ac);
      fs = fs->tl;
    }
    break;
  case disj: break;
  case patom: ISinsert(LIT(f->a),ac); break;
  case natom: ISinsert(NLIT(f->a),ac); break;
  }
}

void printlitlist(intset s) {
  int i;
  iITstart(s);
  while(iITnext(&i)) {
    if(i&1) printf(" -");
    else printf(" ");
    printatomi(VAR(i));
  }
}

void showInvariants() {
  int i,cntpersistent;

  cntpersistent = 0;
  printf("PERSISTENT LITERALS:"); fflush(stdout);
  for(i=0;i<nOfAtoms;i++) {
    if(onelits[i] == 1) { printf(" "); printatomi(i); }
    else if(onelits[i] == 0) { printf(" -"); printatomi(i); }
    if(onelits[i] != -1) cntpersistent += 1;
  }
  printf("\nThere are %i persistent literals (out of total of %i literals).\n",cntpersistent,nOfAtoms);

  printf("Invariants:\n");
  
  for(i=0;i<nOfAtoms;i++) {
    printatomi(i); printf(" OR:");
    printlitlist(twolits[LIT(i)]);
    printf("\n");
    
    printf("-"); printatomi(i); printf(" OR:");
    printlitlist(twolits[NLIT(i)]);
    printf("\n");
  }
}

void showInvariantsBrief() {
  int i;
  printf("True literals:");
  for(i=0;i<nOfAtoms;i++) {
    if(onelits[i] == 1) { printf(" "); printatomi(i); }
    else if(onelits[i] == 0) { printf(" -"); printatomi(i); }
  }
  printf("\nTrue 2-literal clauses:\n");
  
  for(i=0;i<nOfAtoms;i++) {
    
    printatomi(i); printf(" OR ");
    printlitlist(twolits[LIT(i)]);
    printf("\n");
    
    printf("-"); printatomi(i); printf(" OR ");
    printlitlist(twolits[NLIT(i)]);
    printf("\n");
  }
}

llist **prepros; /* */

void preprocess() {
  int i;
  intset preconlits;
  llist *l;
  llist **oldnext;
  eff *e;

  prepros = (llist **)malloc(nOfActions * sizeof(llist *));
  preconlits = IScreate(1000);

  for(i=0;i<nOfActions;i++) {

    e = actions[i].effects;

#ifdef DEBUG
    printf("Preprocessing for operator %i:",i); fflush(stdout);
    printaction(i); fflush(stdout);
#endif

    ISmakeempty(preconlits);

    oldnext = &(prepros[i]);
    guaranteedtrue(actions[i].precon,preconlits);

    while(e != NULL) { /* Go through the conditional effects */
      l = (llist *)malloc(sizeof(llist));

      *oldnext = l;

      l->before = IScreateSize(20);
     
      guaranteedtrue(e->condition,l->before);
      ISaddelements(preconlits,l->before);

#ifdef DEBUG
      printf("BEFORE"); fflush(stdout);
      printlitlist(l->before); fflush(stdout);
      printf("\n"); fflush(stdout);
#endif

      l->after = IScreateSize(20);

      addeffectlits(e->poseffects,e->negeffects,e,actions[i].effects,l->after);
#ifdef DEBUG
      printf("AFTER"); fflush(stdout);
      printlitlist(l->after); fflush(stdout);
      printf("\n\n"); fflush(stdout);
#endif

      l->next = NULL;
      oldnext = &(l->next);
      e = e->tl;
    }

  }
}

/*
Test if operator can falsify something that is true in
the initial state.
 */

/*
int falsifysomethinginitial(int op) {
  eff *e;
  intlist *l;
  e = actions[op].effects;
  while(e != NULL) {
    l = e->poseffects;
    while(l != NULL) {
      if(initialstate[l->hd] == 0) return 1;
      l = l->tl;
    }
    l = e->negeffects;
    while(l != NULL) {
      if(initialstate[l->hd] == 1) return 1;
      l = l->tl;
    }
    e = e->tl;
  }
  return 0;
}
*/

int truep(fma *f) {
  fmalist *fs;
  switch(f->t) {
  case natom: return (onelits[f->a] != 1);
  case patom: return (onelits[f->a] != 0);
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      if(truep(fs->hd)) return 1;
      fs = fs->tl;
    }
    return 0;
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      if(!truep(fs->hd)) return 0;
      fs = fs->tl;
    }
    return 1;
  case TRUE: return 1;
  case FALSE: return 0;
  }
  return 0;
}

int notmadefalse(int l,eff *e) {
  intlist *ls;
  while(e != NULL) {
    if(l&1) ls = e->poseffects;
    else ls = e->negeffects;
    while(ls != NULL) {
      if(VAR(l) == ls->hd) return 0;
      ls = ls->tl;
    }
    e = e->tl;
  }
  return 1;
}

void removefalsified(eff *e,intset s) {
  intlist *l;
  while(e != NULL) {
      l = e->poseffects;
      while(l != NULL) { ISremove(NLIT(l->hd),s); l = l->tl; }
      l = e->negeffects;
      while(l != NULL) { ISremove(LIT(l->hd),s); l = l->tl; }
      e = e->tl;
  }
}

int localISmember0(int i,intset s) {
  int j;
  for(j=0;j<s->nOfEls;j++) {
    if(s->elements[j] == i) return 1;
  }
  return 0;
}

int localISmember1(int i,intset s) {
  int j;
  for(j=0;j<s->nOfEls;j++) {
    if(s->elements[j] == i) return 1;
  }
  return 0;
}

int localISmember2(int i,intset s) {
  int j;

  for(j=0;j<s->nOfEls;j++) {
    if(s->elements[j] == i) return 1;
  }
  return 0;
}

int localISmember3(int i,intset s) {
  int j;
  for(j=0;j<s->nOfEls;j++) {
    if(s->elements[j] == i) return 1;
  }
  return 0;
}

int wastruebefore(int l,intset before) {
  int i;
  //  if(localISmember(l,before)) return 1;
  /* Go through the relevant invariants. */
  for(i=0;i<before->nOfEls;i++) {
    if(before->elements[i] == l) return 1;
    if(localISmember0(l,twolits[NEG(before->elements[i])])) return 1;
  }
  return 0;
}

void computeinvariants() {
  int i,iteration,change,j,k;
  eff *e;
  intlist *ip,*in;
  llist *prep;
  intset trueones,ext;
  intset s;
  int trueonesinitialized;

  preprocess();

  ext = IScreateSize(2000);

  trueones = IScreateSize(2000);

  onelits = (int *)malloc(sizeof(int) * nOfAtoms);
  twolits = (intset *)malloc(sizeof(intset) * nOfAtoms * 2);

  for(i=0;i<nOfAtoms;i++) {
    onelits[i] = initialstate[i];
    twolits[LIT(i)] = IScreate();
    twolits[NLIT(i)] = IScreate();
  }

  printf("Invariants:");

  iteration = 0;
  change = 1;

  while(change) {

    printf(" %i",iteration); fflush(stdout);
    change = 0;

    for(i=0;i<nOfActions;i++) {

#ifdef DEBUG
      printf("\nConsidering action %i:",i); fflush(stdout);
      printaction(i);
      printf("\n");
#endif

	  /* Both the precondition and the cond. eff. antecedents are tested without looking at the mutexes: would not usually make a difference! */

      if(!truep(actions[i].precon)) continue; /* Not applicable (yet) */

      /* Go through all first-time falsified literals:
	 weaken to all possible 2-literal clauses

	 Go through all 2-literal clauses with one disjunct falsified:
	 must the other disjunct be true?
      */

      e = actions[i].effects;

      /* prep is a list of known necessary preconditions and effects. */
      prep = prepros[i];

      while(e != NULL) {

	trueonesinitialized = 0;

	if(truep(e->condition)) {

	  /* In the following:
	     We have newly true literals in prep->after and
	     all true literals in trueones.

	     For every l in prep->after and m in trueones, -l V m
             is an invariant.
	     These are all invariants involving -l.

	     We want to store all of these such that both -l and m
             can have both value true and false.
	   */

	  /* Go through effect literals */
	  ip = e->poseffects;
	  in = e->negeffects;

	  while(ip != NULL || in != NULL) {
	    int l;
	    if(ip != NULL) {
	      l = LIT(ip->hd);
	      ip = ip->tl;
	    } else {
	      l = NLIT(in->hd);
	      in = in->tl;
	    }

	    /* See whether making literal l true falsifies something. */

	    if(onelits[VAR(l)] == (l&1)) { /* Falsified a 1-literal */

	      if(trueonesinitialized == 0) {

		trueonesinitialized = 1;
		ISmakeempty(trueones);

		/* literals true because they are in the precondition */
		ISaddelements(prep->before,trueones);
	  
		/* literals true because a 2-literal clause is satisfied */
		iITstart(prep->before);
		while(iITnext(&j)) ISaddelements(twolits[NEG(j)],trueones);

		/* Remove literals that are falsified by this conditional effect
		   or possibly by other conditional effects. */
		removefalsified(actions[i].effects,trueones);

		/* Add literals that are made true. */
		ISaddelements(prep->after,trueones);
	      }

	      change = 1;

	      onelits[VAR(l)] = -1;

	      /* Add 2-literals -a | l */

	      iITstart(trueones);
	      while(iITnext(&k)) {
		if((onelits[VAR(k)] == -1 || (localISmember1(k,prep->after) && onelits[VAR(k)] == (k&1))) && l != k) {
		  ISinsertNODUP(k,twolits[NEG(l)]);
		  ISinsertNODUP(NEG(l),twolits[k]);
		}
	      }
	    } else if(onelits[VAR(l)] == -1) { /* Check preservation of 2-literal clauses */

	      /* Remove candidate 2-literal invariants which were falsified. */

	      IScopyto(twolits[NEG(l)],ext);

	      iITstart(ext);
	      while(iITnext(&k)) {
		if(!(localISmember2(k,prep->after)
		     || (notmadefalse(k,actions[i].effects) && wastruebefore(k,prep->before)))) {
		  change = 1;
		  ISremove(k,twolits[NEG(l)]);
		  ISremove(NEG(l),twolits[k]);
		}
	      }

	      /* If enabled a new state variable value (= invalidated
		 a candidate one-literal invariant), add new candidate
		 two-literal invariants that still remain true. */

	      iITstart(prep->after);
	      while(iITnext(&k)) {
		if(onelits[VAR(k)] == -1 && localISmember3(k,twolits[NEG(l)]) && l != k) {
		  ISinsert(NEG(l),twolits[k]);
		}
	      }

	    }

	  }

	}
	e = e->tl;
	prep = prep->next;
      }

      //      showInvariantsBrief();
    }

    iteration += 1;
  }

      printf(" ");

  if(flagShowInput) {
    showInvariants();
  }
  
}
