
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>
#include <assert.h>

#include "asyntax.h"

#include "main.h"
#include "ordintsets.h"
#include "../nsat/interface.h"
#include "../nsat/clausesets.h"

#include "operators.h"
#include "translate2sat.h"

#define max(A,B) (((A) > (B)) ? (A) : (B))

#define noASSERTS
#define noDEBUG
#define noDEBUGDISJ
#define noHDEBUG
#define noHARDDEBUG

/* Make sure that each literal is considered only once. */
#define MAXSAW 10000000
int hround;	/* Counter which is incremented for new conflict clauses */
int saw[MAXSAW];	/* Counter value when literal was last encountered. */

inline int hseenp(int l) {	/* Was literal already seen on the current round? */
  int tmp;
#ifdef ASSERTS
  assert(l >= 0);
  assert(l < MAXSAW);
#endif
  tmp = (saw[l] == hround);
  saw[l] = hround;	/* Mark the literal as seen on the current round. */
  return tmp;
}


/* Is l@t FALSE? */

inline int falseat(satinstance sati,int l,int t) {
  int v;
  v = VAR(l);
  if(sati->vars[TVAR(v,t)].val == -1) return 0; /* Unassigned */
  if(sati->vars[TVAR(v,t)].val == (l&1)) return 1; /* */
  return 0;
}

/* Is l@t TRUE? */

inline int trueat(satinstance sati,int l,int t) {
  int v;
  v = VAR(l);
  if(sati->vars[TVAR(v,t)].val == -1) return 0; /* Unassigned */
  if(sati->vars[TVAR(v,t)].val != (l&1)) return 1; /* */
  return 0;
}

/* Value of a state variable. */

inline int varvalue(satinstance sati,int v,int t) {
  return sati->vars[TVAR(v,t)].val;
}

/* Value of a literal. */

inline int litvalue(satinstance sati,int l,int t) {
  int val;
  val = sati->vars[TVAR(VAR(l),t)].val;
  if(val == -1) return -1;
  return val^(l&1);
}

typedef struct {
  int l;	/* Will satisfy subgoal l */
  int t;	/* at time point t */
  int var;	/* with variable var (action, conditional effect) */
} planstep;

#define MAXPLANSTEPS 1000000
planstep plansteps[MAXPLANSTEPS];
int Nsteps;

#define STACKSIZE 10000
typedef struct _stackel {
  int lit;
  int t;
  int val;
} stackel;

int stackptr;
stackel stack[STACKSIZE];

/* Order the subgoals according to a measure.
   0. No measure is used (everything is 0).
   1. How early does it have to be true.
*/

inline int goallitvalue(satinstance sati,int l,int t0) {
  int t;
  t = t0;
  if(HEURordmode) {
    while(t >= 0 && litvalue(sati,l,t) == 1) t = t - 1;
    return t+1;
  } else return 0;
}

void push2goalstack(satinstance sati,int l,int t) {
  int v,i;

  i = stackptr++;

  v = goallitvalue(sati,l,t);

  /* Insert the literal in the stack, keeping them in an ascending order. */
  /* The elements i..stackptr-1 must be moved one step forward. */

  while(i > 0 && stack[i-1].val < v) {
    stack[i].lit = stack[i-1].lit;
    stack[i].t = stack[i-1].t;
    stack[i].val = stack[i-1].val;
    i = i-1;
  }

  stack[i].lit = l;
  stack[i].t = t;
  stack[i].val = v;
}

/* Push the constituent literals of a conjunctive goal formula in the stack. */

void push2goalstackCfma(satinstance sati,fma *f,int t) {
  intlist *fs;
  switch(f->t) {
  case patom: push2goalstack(sati,PLIT(f->a),t); break;
  case natom: push2goalstack(sati,NLIT(f->a),t); break;
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      push2goalstackCfma(sati,fs->hd,t);
      fs = fs->tl;
    }
    break;
  case disj:
  case TRUE: break;
  case FALSE:
    assert(1==3);
  default:
    assert(1==2);
  }
}

/* Push a subset of the literals in a general NNF formula which are
   sufficient for making the formula true, in the stack.
   This is as in the ICAPS'11 submission.
   For conjunctions all conjuncts have to be taken.
   For disjunctions one of the disjuncts is taken. If at least one of
   the disjuncts is true or unknown, then the chosen disjunct cannot
   be a false one.
*/

/* The implementation is in with a stack of sets of literals.
   The operations are
   - adding a set into the set (empty or singleton)
   - taking the union of two top stacks
   - removing the top or the second set from the stack
   - comparing the two top sets (cardinality?) for choosing a disjunct

tset_stack: element is the index of the first element of the set in tset_store
*/

typedef struct {
  int l;
  int t;
} storepair;

int stacktop;
int storetop;
int tset_stack[1000];
int tset_card[1000];
storepair tset_store[100000];

#define CARDTOP (storetop-tset_stack[stacktop]+1)
#define CARD2ND (tset_stack[stacktop]-tset_stack[stacktop-1])

void tset_emptyset() {
#ifdef DEBUGDISJ
  printf("tset_emptyset\n");
#endif
  stacktop += 1;
  tset_card[stacktop] = 0;
  tset_stack[stacktop] = storetop+1;
}

void tset_show() {
  int i,j;
#ifdef DEBUGDISJ
  printf("=========================================================\n");
  for(i=0;i<=storetop+1;i++) {
    for(j=0;j<=stacktop;j++) {
      if(tset_stack[j] == i) printf("START %i (card %i)\n",j,tset_card[j]);
    }
    if(i<=storetop) {
      printf("%i: ",i); printlit(tset_store[i].l); printf("@%i\n",tset_store[i].t);
    }
  }
  printf("     =========================================================\n");
#endif
}

/* Add the empty set in the stack. */

void tset_makeempty() {
#ifdef DEBUGDISJ
  printf("tset_makeempty\n");
#endif
  stacktop=-1;
  storetop=-1;
  tset_emptyset(); /* Always have an empty set in the stack. */
}

/* Add a singleton set into the stack. */

void tset_singleton(int l,int t) {
#ifdef DEBUGDISJ
  printf("tset_singleton "); printlit(l); printf("@%i\n",t);
#endif
  storetop = storetop+1;
  tset_store[storetop].l = l;
  tset_store[storetop].t = t;
  stacktop = stacktop+1;
  tset_stack[stacktop] = storetop;
  tset_card[stacktop] = 1;
}

/* Take the union of the two top sets. */

void tset_union() {
#ifdef DEBUGDISJ
  printf("tset_union\n");
#endif
  tset_card[stacktop-1] = tset_card[stacktop-1]+tset_card[stacktop];
  stacktop = stacktop - 1;
}

/* Return TRUE if top element has a smaller cardinality than the second. */

int tset_top_better() {
  if(tset_card[stacktop] < tset_card[stacktop-1]) return 1; else return 0;
}

/* Remove the top set from the stack. */

void tset_top_delete() {
#ifdef DEBUGDISJ
  printf("tset_top_delete\n");
#endif
  storetop = storetop-tset_card[stacktop];
  stacktop = stacktop-1;
}

/* Remove the second set from the stack. */

void tset_second_delete() {
  int i,n;
#ifdef DEBUGDISJ
  printf("tset_second_delete\n");
#endif
  n = tset_card[stacktop];
  /* Move the top set where the second one was. */
  for(i=0;i<n;i++) {
    tset_store[tset_stack[stacktop-1]+i] = tset_store[tset_stack[stacktop]+i];
  }
  storetop = tset_stack[stacktop-1]+n-1;
  stacktop = stacktop-1;
  tset_card[stacktop]=n;
}

int traverseDfma(satinstance sati,fma *f,int t) {
  intlist *fs;
  int have;

#ifdef DEBUGDISJ
  tset_show();
#endif

  switch(f->t) {
    /* Here we have to restrict to literals that are not FALSE ? */
  case patom:
    if(varvalue(sati,f->a,t) == 0) return 0;
    tset_singleton(PLIT(f->a),t);
    return 1;
  case natom:
    if(varvalue(sati,f->a,t) == 1) return 0;
    tset_singleton(NLIT(f->a),t);
    return 1;
  case conj: /* The literal sets for all conjuncts are combined. */
    tset_emptyset();
    fs = f->juncts;
    while(fs != NULL) {
      if(traverseDfma(sati,fs->hd,t) == 0) {
	tset_top_delete();
	return 0;
      }
      tset_union();
      fs = fs->tl;
    }
    return 1;
  case disj: /* The set for one of the disjuncts is chosen, others ignored. */
    //    tset_emptyset(); ???????????????????
    have = 0;
    fs = f->juncts;
    while(fs != NULL) {
      if(traverseDfma(sati,fs->hd,t)) {
	if(have) {
#ifdef DEBUGDISJ
	  tset_show();
#endif
	  if(tset_top_better()) tset_second_delete(); else tset_top_delete();
	}
	have = 1;
      }
      fs = fs->tl;
    }
    return have;
  case TRUE: tset_emptyset(); return 1;
  case FALSE: return 0;
  }
}

void push2goalstackDfma(satinstance sati,fma *f,int t) {
  int i,l,t2;
  tset_makeempty();
  if(traverseDfma(sati,f,t) == 0) return;
#ifdef DEBUGDISJ
  tset_show();
#endif
  //  if(tset_stack[stacktop]+1>storetop) printf("Not pushing anything\n");
  /* Push the literals from the traverseDfma stack into the heuristic stack. */
  for(i=tset_stack[stacktop];i<=storetop;i++) {
    l = tset_store[i].l;
    t2 = tset_store[i].t;
#ifdef DEBUGDISJ
    printf("Pushing %i:",l); printlit(l); printf("@%i\n",t2);
#endif
    push2goalstack(sati,l,t2);
  }
}

#ifdef DEBUG
void showheustack(satinstance sati) {
  int i;
  printf("STACK CONTENTS:\n");
  for(i=0;i<stackptr;i++) {
    printlit(stack[i].lit);
    printf("@%i (score %i)\n",stack[i].t,stack[i].val);
  }
}
#endif

void do_cpath(satinstance);

/* Identify actions that are useful in reaching the goals. */

int do_cpath_heuristic(satinstance sati) {
  int i,j,best,besttime;

  hround += 1;

  Nsteps = 0;
  stackptr = 0;

  switch(sati->heuristic_mode) {

  case 0:	/* Choose an action. */

    if(goalisdisjunctive) {
      push2goalstackDfma(sati,goal,sati->nOfTPoints-1);
    } else {
      push2goalstackCfma(sati,goal,sati->nOfTPoints-1);
    }

#ifdef DEBUG
    showheustack(sati);
#endif

    /* Find paths */
    do_cpath(sati);

    /* Pick action. */

    /* Only one action to choose from: return it directly. */
    if(Nsteps == 1) return PLIT(TVAR(plansteps[0].var,plansteps[0].t));

    if(Nsteps > 0) 
      switch(HEURactionchoice) {
      case 0: /* Choose action randomly. */
	i = random() % Nsteps;
	return PLIT(TVAR(plansteps[i].var,plansteps[i].t));
      case 1: /* Choose the earliest possible action. */
	best = -1;
	besttime = 100000;
	for(i=0;i<Nsteps;i++) {
	  if(plansteps[i].t < besttime) {
	    best = i;
	    besttime = plansteps[i].t;
	  }
	}
	return PLIT(TVAR(plansteps[best].var,plansteps[best].t));
      }

    sati->heuristic_mode = 1;
    sati->heuristic_time = 1;
    sati->heuristic_index = 0;

  case 1:	/* No actions needed. Do inertia. */

#ifdef DEBUG
    printf("Doing INERTIA\n");
#endif

    while(sati->heuristic_time < sati->nOfTPoints) {

	i = sati->heuristic_index;
	j = sati->heuristic_time;

	if(i+1 == sati->nOfSVars) {
	  sati->heuristic_index = 0;
	  sati->heuristic_time += 1;
	} else {
	  sati->heuristic_index += 1;
	}

	if(sati->vars[TVAR(i,j)].val == -1) {
	  if(sati->vars[TVAR(i,j-1)].val == 1) return PLIT(TVAR(i,j));
	  else return NLIT(TVAR(i,j));
	}

    }

    sati->heuristic_mode = 2;
    sati->heuristic_time = 0;
    sati->heuristic_index = 0;

  case 2:	/* All state variables have a value. Set actions to FALSE. */

    while(sati->heuristic_time < sati->nOfTPoints-1) {

	i = sati->heuristic_index;
	j = sati->heuristic_time;

	if(i+1 == sati->nOfActions) {
	  sati->heuristic_index = 0;
	  sati->heuristic_time += 1;
	} else {
	  sati->heuristic_index += 1;
	}

	if(sati->vars[TVAR(ACTVAR(i),j)].val == -1) return NLIT(TVAR(ACTVAR(i),j));
    }

  }

  return -1;

}

/* Go through actions at time point t-1 to find one that could
   make literal l true at t. "Could" means: l occurs as a conditional
   or unconditional effect, and we don't care about the condition.

   This is based on the following parameters (given on command line):
     HEURtime: which time to consider, 0 = earliest, 1 = latest, 2 = all
     HEURops: which operator to consider 0 = first (arbitrary), 1 = all
     HEURchoice: which action@time to choose, 0 = random, 1 = weight
*/

/* Choose an action at t1-1 that can make l@t TRUE. The action is returned
   in var,t.  */

void supports(satinstance sati,int t0,int t1,int l,int *var,int *t,fma **precondition,int *disjunctive) {
  compactCEstruct *ptr;

  for(ptr=cCEs[l];ptr->var != -1;ptr++) { /* All ways of making l true. */
    if(varvalue(sati,ptr->var,t1-1) != 0) {  /* Is it applicable? */
#ifdef HDEBUG
      printf("Add ACTION %i:",ptr->var); printUvar(ptr->var); printf("@%i\n",t1-1);
#endif
      *var = ptr->var;
      *t = t1-1;
      *precondition = ptr->condition;
      *disjunctive = ptr->disjunctive;
      if(HEURops == 0) return; /* All actions or only the 1st (arbitrary) one. */
    }
  }
  assert(1==2);
}


/* Is some action op actually making l true at t-1? (= op assigned TRUE?) */

int litmadetrue(satinstance sati,int l,int t,fma **precondition,int *disjunctive) {
  int i;

  if(!trueat(sati,l,t)) return -1; /* l@t is not even true: no action! */

  for(i=0;cCEs[l][i].var != -1;i++) { /* All ways of making l@t-1 true. */
    if(varvalue(sati,cCEs[l][i].var,t-1) == 1) {
      *precondition = cCEs[l][i].condition;
      *disjunctive = cCEs[l][i].disjunctive;
      return cCEs[l][i].var;
    }
  }
  
  return -1; /* l@t-1 not made true. */
}

/***********************************************************************/
/*** Heuristic:                                                      ***/
/***   Find an unfulfilled (sub)goal and fulfill it at the earliest  ***/
/***   possible time point.                                          ***/
/***                                                                 ***/
/***********************************************************************/

void do_cpath(satinstance sati) {
  int l,t,t1;
  int j,var,isthere;
  int suggestedactionsfound;
  int depthlimitHIGH;
  fma *precondition;
  int disjunctive;

  depthlimitHIGH = -1;
  suggestedactionsfound = 0;

  while(stackptr > 0) {

    /* Pop literal-time pair from the stack. */
    l = stack[stackptr-1].lit;
    t = stack[--stackptr].t;

#ifdef DEBUG
    printf("Find action for goal "); printlit(l); printf("@%i.\n",t);
#endif

    /* Starting from last time point, find last time t such that either
       1) an action op@t-1 makes l@t TRUE, or
       2) l@t-1 is FALSE and l@t is TRUE or UNASSIGNED.
    */
    isthere = 0;
    var = -1;

    for(j=t;j>0;j--) {
      var = litmadetrue(sati,l,j,&precondition,&disjunctive);
      if(var != -1) {
	isthere = 1;
	t1 = j-1;
	break;
      }
      if(falseat(sati,l,j-1)) break;
    }

    if(j == 0) { /* Literal is true at time 0. */
#ifdef DEBUG
      printf("(true in the initial state.)\n");
#endif
      continue; 
    }

    /* Literal is last false at time point j-1. */

    if(var == -1) { /* Choose an action that turns l true between j-1 and j. */
#ifdef DEBUG
      printf("Choosing an action that makes literal true @%i.\n",j);
#endif
      supports(sati,t,j,l,&var,&t1,&precondition,&disjunctive);
#ifdef ASSERTS
    /* The following assertions cannot be false because
       there must be an action that could make l true at t1 (i.e. is not FALSE).
       If there were not, the frame action (-l & l) -> ... would
       contradict Ass1 and Ass2.
    */
      assert(litvalue(sati,l,j-1) == 0);
      assert(litvalue(sati,l,j) != 0);
      assert(var != 0);
#endif
    }

    if(hseenp(TVAR(var,t1))) continue;	/* Operator already processed once. */

    /* Don't go back to top-level goals. */

    if(HEURactiondepthlimit && suggestedactionsfound && t1 >= depthlimitHIGH)
      return;

    if(!isthere) { /* Add the variable to the list of candidate decisions. */
      suggestedactionsfound += 1;
      plansteps[Nsteps].var = var;
      plansteps[Nsteps].t = t1;
      plansteps[Nsteps++].l = l;

      if(suggestedactionsfound == 1) depthlimitHIGH = t1;
    }

#ifdef ASSERTS
    assert(Nsteps < MAXPLANSTEPS);
#endif

    if(suggestedactionsfound >= HEURactions) return;
    
#ifdef DEBUG
    printf(" Push preconditions of %i:",var); printUvar(var); printf("@%i into the stack (%i).\n",t1,isthere);
#endif
    
    if(t1 > 0) {      /* Push preconditions in the stack. */
      if(disjunctive == 0) {
	push2goalstackCfma(sati,precondition,t1);
      } else {
	push2goalstackDfma(sati,precondition,t1);
      }
#ifdef DEBUG
      showheustack(sati);
#endif

    }
  }
}
