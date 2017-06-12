
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>

#include "main.h"
#include "asyntax.h"
#include "intsets.h"
#include "ordintsets.h"
#include "operators.h"
#include "invariants.h"

int member(int i,intlist *l) {
  while(l != NULL) {
    if(l->hd == i) return 1;
    l = l->tl;
  }
  return 0;
}

int intersect(intlist *l1,intlist *l2) {
  while(l1 != NULL) {
    if(member(l1->hd,l2)) return 1;
    l1 = l1->tl;
  }
  return 0;
}

int unconditional(int l,fma *f) {
  fmalist *fs;
  switch(f->t) {
  case disj:
    return 0;
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      if(unconditional(l,fs->hd)) return 1;
      fs = fs->tl;
    }
    return 0;
    break;
  case natom:
    if(NLIT(f->a) == l) return 1;
    return 0;
    break;
  case patom:
    if(PLIT(f->a) == l) return 1;
    return 0;
    break;
  default:
    return 0;
  }
  return 0;
}

int doesnothing(fma *p,eff *e) {
  intlist *l;
  while(e != NULL) {
    l = e->poseffects;
    while(l != NULL) {
      if(!unconditional(PLIT(l->hd),p)) return 0;
      l = l->tl;
    }
    l = e->negeffects;
    while(l != NULL) {
      if(!unconditional(NLIT(l->hd),p)) return 0;
      l = l->tl;
    }
    e = e->tl;
  }
  return 1;
}

/* Remove operators which don't do anything. */

void cleanupoperatorsNOOP() {
  int i;
  int removed;
  removed = 0;
  for(i=0;i<nOfActions;i++) {
  again:
    if(doesnothing(actions[i].precon,actions[i].effects)) {
      removed += 1;
      actions[i] = actions[nOfActions-1];
      nOfActions -= 1;
      if(i <= nOfActions-1) goto again;
    }
  }
  if(debugOutput > 1 && removed) printf("Removed %i actions with inconsistent effects.\n",removed);
}

/* Remove operators with inconsistent effects. */

void cleanupoperators0() {
  int i;
  int removed;
  eff *e;
  removed = 0;
  for(i=0;i<nOfActions;i++) {
  again:
    e = actions[i].effects;
    while(e != NULL) {
      if(intersect(e->poseffects,e->negeffects)) {
	removed += 1;
	actions[i] = actions[nOfActions-1];
	nOfActions -= 1;
	if(i <= nOfActions-1) goto again;
      }
      e = e->tl;
    }
  }
  if(debugOutput > 1 && removed) printf("Removed %i actions with inconsistent effects.\n",removed);
}

/* Conform with funny PDDL semantics on actions that _simultaneously_
   set something both TRUE and FALSE. */

void removefrom(eff *e) {
  intlist *en,**prev;
  prev = &(e->negeffects);
  en = e->negeffects;
  while(en != NULL) {
    if(member(en->hd,e->poseffects)) { /* Remove it. */
      *prev = en->tl;
    } else {
      prev = &(en->tl);
    }
    en = en->tl;
  }
}

/* Standard PDDL behaviour: if both v:=0 and v:=1, then ignore v:=0. */

void cleanupoperators1() {
  int i;
  int removed;
  eff *e;
  removed = 0;
  for(i=0;i<nOfActions;i++) {
  again:
    e = actions[i].effects;
    while(e != NULL) {
      removefrom(e);
      e = e->tl;
    }
  }
  if(debugOutput > 1 && removed) printf("Removed %i actions with inconsistent effects.\n",removed);
}

void cleanupoperators() {
  int i;
  if(flagPDDLadddel) cleanupoperators1();
  else cleanupoperators0();
  cleanupoperatorsNOOP();
  //  printf("After simplifications: %i actions\n",nOfActions);

  if(planSemantics == EStepOgata) {
    /* Randomly shuffle the action array. */

    for(i=0;i<nOfActions;i++) {
      intlist *l;
      fma *f;
      eff *e;
      int j;

      j = random() % nOfActions;

      l = actions[i].name;
      f = actions[i].precon;
      e = actions[i].effects;

      actions[i].name = actions[j].name;
      actions[i].precon = actions[j].precon;
      actions[i].effects = actions[j].effects;

      actions[j].name = l;
      actions[j].precon = f;
      actions[j].effects = e;
    }
  }

}
