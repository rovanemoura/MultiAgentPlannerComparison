
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <assert.h>

#include "main.h"
#include "asyntax.h"
#include "intsets.h"
#include "ordintsets.h"
#include "operators.h"
#include "tables.h"
#include "invariants.h"

#define noDEBUG

/* Local copies (for inlining) of intsets.c functions */

int *jITptr;
int jITcounter;

void jITstart(intset s) {
  jITcounter = s->nOfEls;
  jITptr = s->elements;
}

int jITnext(int *i) {
  if(jITcounter <= 0) return 0;
  jITcounter -= 1;
  *i = *(jITptr++);
  return 1;
}

//#define jITnext ITnext
//#define jITstart ITstart

fmalist *fmacons(fma *h,fmalist *t) {
  fmalist *r = (fmalist *)malloc(sizeof(fmalist));
  r->hd = h;
  r->tl = t;
  return r;
}

void initactions() {
  nOfActions = 0;
  maxActions = 100000;
  actions = (action *)malloc(maxActions * sizeof(action));
}

fma *Fconj(fmalist *fs) {
  fma *f;

  if(fs == NULL) { /* No conjuncts */
    f = (fma*)malloc(sizeof(fma));
    f->t = TRUE;
  } else if(fs->tl == NULL) { /* Only one conjunct */
    return fs->hd;
  } else {
    f = (fma*)malloc(sizeof(fma));
    f->t = conj;
    f->juncts = fs;
  }
  return f;
}

/* Test whether a formula has disjunction in it. */

int disjunctivep(fma *f) {
  fmalist *l;
  switch(f->t) {
  case patom:
  case natom:
    return 0;
  case disj: return 1;
  case conj:
    l = f->juncts;
    while(l != NULL) {
      if(disjunctivep(l->hd)) return 1;
      l = l->tl;
    }
  }
  return 0;
}

fma *Fdisj(fmalist *fs) {
  fma *f;

  if(fs == NULL) { /* No disjuncts */
    f = (fma*)malloc(sizeof(fma));
    f->t = FALSE;
  } else if(fs->tl == NULL) { /* Only one disjunct */
    return fs->hd;
  } else {
    f = (fma*)malloc(sizeof(fma));
    f->t = disj;
    f->juncts = fs;
  }
  return f;
}

fma *Fconj2(fma *f1,fma *f2) {
  fma *f;

  f = (fma*)malloc(sizeof(fma));
  f->t = conj;
  f->juncts = intcons(f1,intcons(f2,NULL));

  return f;
}

fma *Fdisj2(fma *f1,fma *f2) {
  fma *f;

  f = (fma*)malloc(sizeof(fma));
  f->t = disj;
  f->juncts = intcons(f1,intcons(f2,NULL));

  return f;
}

fma *Fatom(int a) {
  fma *f = (fma*)malloc(sizeof(fma));
  f->t = patom;
  f->a = a;
  return f;
}

fma *Fnatom(int a) {
  fma *f = (fma*)malloc(sizeof(fma));
  f->t = natom;
  f->a = a;
  return f;
}

fma *Ffalse() {
  fma *f = (fma*)malloc(sizeof(fma));
  f->t = FALSE;
  return f;
}

fma *Ftrue() {
  fma *f = (fma*)malloc(sizeof(fma));
  f->t = TRUE;
  return f;
}

fma *Fimpl(fma *f1,fma *f2) {
  if(f1->t == TRUE) return f2;
  return Fdisj(fmacons(Fneg(f1),fmacons(f2,NULL)));
}

fma *OLDFneg(fma *f) {
  fmalist *l;
  switch(f->t) {
  case TRUE: f->t = FALSE; break;
  case FALSE: f->t = TRUE; break;
  case patom: f->t = natom; break;
  case natom: f->t = patom; break;
  case conj: f->t = disj;
    l = f->juncts;
    while(l != NULL) {
      Fneg(l->hd);
      l = l->tl;
    }
    break;
  case disj: f->t = conj;
    l = f->juncts;
    while(l != NULL) {
      Fneg(l->hd);
      l = l->tl;
    }
    break;
  }
  return f;
}

fma *Fneg(fma *f) {
  fmalist *l;
  fma *nf;
  nf = (fma *)malloc(sizeof(fma));

  switch(f->t) {

  case TRUE: nf->t = FALSE; break;
  case FALSE: nf->t = TRUE; break;

  case patom: nf->t = natom; nf->a = f->a; break;
  case natom: nf->t = patom; nf->a = f->a; break;

  case conj:
    nf->t = disj;
    l = f->juncts;
    nf->juncts = NULL;
    while(l != NULL) {
      nf->juncts = intcons(Fneg(l->hd),nf->juncts);
      l = l->tl;
    }
    break;

  case disj:
    nf->t = conj;
    l = f->juncts;
    nf->juncts = NULL;
    while(l != NULL) {
      nf->juncts = intcons(Fneg(l->hd),nf->juncts);
      l = l->tl;
    }
    break;
  }

  return nf;
}

/* Test whether a formula is true in a state. */


int ptruep(fma *f,int *state) {
  fmalist *fs;
  switch(f->t) {
  case natom: return (state[f->a] != 1);
  case patom: return (state[f->a] != 0);
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      if(ptruep(fs->hd,state)) return 1;
      fs = fs->tl;
    }
    return 0;
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      if(!ptruep(fs->hd,state)) return 0;
      fs = fs->tl;
    }
    return 1;
  case TRUE: return 1;
  case FALSE: return 0;
  }
  return 0;
}

/* Execute action in a state and modify the successor state.
*/

int execute(int a,int *state,int *state2) {
  eff *e;
  intlist *ls;

  if(!ptruep(actions[a].precon,state)) return 0;

  e = actions[a].effects;
  while(e != NULL) {
    if(ptruep(e->condition,state)) {
      ls = e->poseffects;
      while(ls != NULL) {
#ifdef DEBUG
	printf("Making "); printatomi(ls->hd); printf(" TRUE\n");
#endif
	state2[ls->hd] = 1;
	ls = ls->tl;
      }
      ls = e->negeffects;
      while(ls != NULL) {
#ifdef DEBUG
	printf("Making "); printatomi(ls->hd); printf(" FALSE\n");
#endif
	state2[ls->hd] = 0;
	ls = ls->tl;
      }
    }
    e = e->tl;
  }
  return 1;
}

void executeNOprecon(int a,int *state,int *state2) {
  eff *e;
  intlist *ls;

  e = actions[a].effects;
  while(e != NULL) {
    if(ptruep(e->condition,state)) {
      ls = e->poseffects;
      while(ls != NULL) {
	state2[ls->hd] = 1;
	ls = ls->tl;
      }
      ls = e->negeffects;
      while(ls != NULL) {
	state2[ls->hd] = 0;
	ls = ls->tl;
      }
    }
    e = e->tl;
  }
}

/* Test whether o1 affects o2 in state. This means: is there an _active_
   effect of o1 that disables o2 or changes it's (conditional) effects. */

int opaffectsinstate(int *state,int o1,int o2) {
  eff *es;
  intlist *vs;

  es = actions[o1].effects;

  /* Go through all effects of o1. */
  while(es != NULL) {

    if(ptruep(es->condition,state)) { /* Only look at active effects. */

	vs = es->poseffects;
	while(vs != NULL) {
	  if(isaffectedby(o2,PLIT(vs->hd))) return 1;
	  vs = vs->tl;
	}

	vs = es->negeffects;
	while(vs != NULL) {
	  if(isaffectedby(o2,NLIT(vs->hd))) return 1;
	  vs = vs->tl;
	}

      }

    es = es->tl;
	
  }

  return 0;
}


/* Print various things. */

void printfmalist(fmalist *);
void printfma(fma *f) {
  switch(f->t) {
  case patom: printatomi(f->a); break;
  case natom: printf("(not "); printatomi(f->a); printf(")"); break;
  case conj:
    printf("(and ");
    printfmalist(f->juncts);
    printf(")");
    break;
  case disj:
    printf("(or ");
    printfmalist(f->juncts);
    printf(")");
    break;
  case TRUE:
    printf("TRUE"); break;
  case FALSE:
    printf("FALSE"); break;
  }
}

void printfmalist(fmalist *l) {
  if(l == NULL) return;
  printfma(l->hd);
  if(l->tl != NULL) printf(" ");
  printfmalist(l->tl);
}

void printeff(eff *e) {
  fma *c;
  intlist *l;
  if(e == NULL) return;
  c = e->condition;
  if(c->t != TRUE) {
    printf("(when ");
    printfma(c);
  }
  l = e->poseffects;
  while(l != NULL) {
    printf(" ");
    printatomi(l->hd);
    l = l->tl;
  }
  l = e->negeffects;
  while(l != NULL) {
    printf(" (not "); printatomi(l->hd); printf(") ");
    l = l->tl;
  }
  if(c->t != TRUE) printf(")");
  printeff(e->tl);
}

int fprintactionname(FILE *f,int i) {
  intlist *l;
  int len;
  l = actions[i].name;
  fprintf(f,"%s(",symbol(l->hd));
  len = strlen(symbol(l->hd))+1;
  l = l->tl;
  while(l != NULL) {
    fprintf(f,"%s",symbol(l->hd));
    len += strlen(symbol(l->hd));
    if(l->tl !=NULL) {
      fprintf(f,",");
      len += 1;
    }
    l = l->tl;
  }
  fprintf(f,")");
  return len+1;
}

int printactionname(int i) {
  fprintactionname(stdout,i);
}

int fprintactionnameIPC(FILE *f,int i) {
  intlist *l;
  int len;
  l = actions[i].name;
  fprintf(f,"(%s",symbol(l->hd));
  len = strlen(symbol(l->hd))+1;
  l = l->tl;
  while(l != NULL) {
    fprintf(f," %s",symbol(l->hd));
    len += strlen(symbol(l->hd));
    l = l->tl;
  }
  fprintf(f,")");
  return len+1;
}

int printactionnameIPC(int i) {
  fprintactionnameIPC(stdout,i);
}

void printaction(int i) {
  intlist *l;
  /* Print operator name action(p1,...,pn) */
  l = actions[i].name;
  printf("ACTION %i:%s(",i,symbol(l->hd));
  l = l->tl;
  while(l != NULL) {
    printf("%s",symbol(l->hd));
    if(l->tl !=NULL) printf(",");
    l = l->tl;
  }
  printf(")\n");
  /* Print precondition */
  printfma(actions[i].precon);
  printf("\n");
  /* Print effect */
  printeff(actions[i].effects);
  printf("\n");
}

/* Simplify a formula */

fmalist *allconjuncts(fmalist *fs,fmalist *ac) {
  while(fs != NULL) {
    if(fs->hd->t == conj) ac = allconjuncts(fs->hd->juncts,ac);
    else if(fs->hd->t != TRUE) ac = fmacons(fs->hd,ac);
    fs = fs->tl;
  }
  return ac;
}

fmalist *alldisjuncts(fmalist *fs,fmalist *ac) {
  while(fs != NULL) {
    if(fs->hd->t == disj) ac = alldisjuncts(fs->hd->juncts,ac);
    else if(fs->hd->t != FALSE) ac = fmacons(fs->hd,ac);
    fs = fs->tl;
  }
  return ac;
}

void simplifyfma(fma *f) {
  fmalist *fs;
  int trueone,falseone;
  switch(f->t) {
  case conj:
    falseone = 0;
    fs = f->juncts;
    while(fs != NULL) {
      simplifyfma(fs->hd);
      if(fs->hd->t == FALSE) { falseone = 1; break; }
      fs = fs->tl;
    }
    if(falseone) f->t = FALSE;
    else {
      f->juncts = allconjuncts(f->juncts,NULL);
      if(f->juncts == NULL) f->t = TRUE;
    }
    break;
  case disj:
    trueone = 0;
    fs = f->juncts;
    while(fs != NULL) {
      simplifyfma(fs->hd);
      if(fs->hd->t == TRUE) { trueone = 1; break; }
      fs = fs->tl;
    }
    if(trueone) f->t = TRUE;
    else {
      f->juncts = alldisjuncts(f->juncts,NULL);
      if(f->juncts == NULL) f->t = FALSE;
    }
    break;
  default:
    break;
  }
}

/* Simplify operator set */

void simplifyoperators() {
  int i,removed;
  removed = 0;
  i=0;
  while(i < nOfActions) {
    simplifyfma(actions[i].precon);
    if(actions[i].precon->t == FALSE) {
      actions[i] = actions[nOfActions-1];
      removed += 1;
      nOfActions -= 1;
    }
    i += 1;
  }
  if(debugOutput > 1)
    printf("Removed %i unapplicable actions.\n",removed);
}

/* Test whether a formula can make literal true */

int canmaketrue(int op,int l) {
  eff *effs;
  intlist *ls;
  effs = actions[op].effects;
  while(effs != NULL) {
    if(l&1) ls = effs->negeffects;
    else ls = effs->poseffects;
    while(ls != NULL) {
      if(ls->hd == (l >> 1)) return 1;
      ls = ls->tl;
    }
    effs = effs->tl;
  }
  return 0;
}

int occursin(int v,fma *f) {
  fmalist *fs;
  switch(f->t) {
  case conj:
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      if(occursin(v,fs->hd)) return 1;
      fs = fs->tl;
    }
    return 0;
  case natom:
  case patom:
    if(f->a == v) return 1;
    return 0;
  default:
    return 0;
  }
}

int Loccursin(int l,fma *f) {
  fmalist *fs;
  switch(f->t) {
  case conj:
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      if(Loccursin(l,fs->hd)) return 1;
      fs = fs->tl;
    }
    return 0;
  case natom:
    if(NLIT(f->a) == l) return 1;
    return 0;
  case patom:
    if(PLIT(f->a) == l) return 1;
    return 0;
  default:
    return 0;
  }
}

/* Test whether operator op is affected by literal l. */

int isaffectedby(int op,int l) {
  eff *effs;
  if(Loccursin(NEG(l),actions[op].precon)) return 1;
  effs = actions[op].effects;
  while(effs != NULL) {
    if(occursin(VAR(l),effs->condition)) return 1;
    effs = effs->tl;
  }
  return 0;
}

/* Test whether one operator affects the other.
 This is:
 o1 has an effect that falsifies the precondition of o2 or
 affects the lhs of a conditional effect of o2.
This must be in accordance with the computation of disabling graphs. */

int opaffects(int o1,int o2) {
  eff *es;
  intlist *vs;

  es = actions[o1].effects;

  /* Go through all effects of o1. */
  while(es != NULL) {

    vs = es->poseffects;
    while(vs != NULL) {
      if(isaffectedby(o2,PLIT(vs->hd))) return 1;
      vs = vs->tl;
    }

    vs = es->negeffects;
    while(vs != NULL) {
      if(isaffectedby(o2,NLIT(vs->hd))) return 1;
      vs = vs->tl;
    }

    es = es->tl;
  }

  return 0;
}

/* Test whether two operators can be concurrent, i.e. neither preconditions
   nor effects contradict each other. */

int OLDAmember(int i,int *a) {
  while(*a != -1) {
    if(i == *(a++)) return 1;
  }
  return 0;
}

inline int Amember(int i,int *a) {
  while(*a != -1 && *a != i) a++;
  if(*a != -1) return 1;
  return 0;
}

int parallel(int op1,int op2) {
  int *i,j;

  /* Preconditions contradict? */

  i = AnecessarypreconP[op1];
  while(*i != -1) { /* Go through operators positive precons. */
    if(Amember(*(i++),AnecessarypreconN[op2])) return 0; /* Direct conflict. */
  }
    
  i = AnecessarypreconN[op1];
  while(*i != -1) { /* Go through operators negative precons. */
    if(Amember(*(i++),AnecessarypreconP[op2])) return 0; /* Direct conflict. */
  }

  /* Effects contradict? */

  i = AforcedeffectsP[op1];
  while(*i != -1) { /* Go through operator's positive effects. */

    if(Amember(*i,AforcedeffectsN[op2])) return 0; /* Direct conflict. */

    /* Conflicts through a 2-literal invariant l1 | l2. */

    jITstart(twolits[NLIT(*i)]);
    while(jITnext(&j)) {
      if((j&1) && Amember(VAR(j),AforcedeffectsP[op2])) return 0;
      if((j&1) == 0 && Amember(VAR(j),AforcedeffectsN[op2])) return 0;
    }

    i++;

  }

  i = AforcedeffectsN[op1];
  while(*i != -1) { /* Go through operator's negative effects. */

    if(Amember(*i,AforcedeffectsP[op2])) return 0; /* Direct conflict. */

    /* Conflicts through a 2-literal invariant l1 | l2. */

    jITstart(twolits[PLIT(*i)]);
    while(jITnext(&j)) {
      if((j&1) && Amember(VAR(j),AforcedeffectsP[op2])) return 0;
      if((j&1) == 0 && Amember(VAR(j),AforcedeffectsN[op2])) return 0;
    }

    i++;

  }

  return 1;
}


/* Test whether an effect l can be concurrent with an operator,
   i.e. whether l contradicts the effects of the operator. */

int Lparallel(int l,int op2) {
  int j;

  if((l&1) == 0) { /* Literal is positive. */
    if(OSmember(VAR(l),forcedeffectsN[op2])) { return 0; } /* Direct conflict. */

  } else { /* Literal is negative. */

    if(OSmember(VAR(l),forcedeffectsP[op2])) { return 0; } /* Direct conflict. */

  }

  /* Conflicts through a 2-literal invariant l1 | l2. */

  jITstart(twolits[NEG(l)]);
  while(jITnext(&j)) {
    if((j&1) && OSmember(VAR(j),forcedeffectsP[op2])) { return 0; }
    if((j&1) == 0 && OSmember(VAR(j),forcedeffectsN[op2])) { return 0; }
  }

  return 1;
}



void Fcollectliterals(ordintset s,fma *f) {
  fmalist *fs;
  switch(f->t) {
  case conj:
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      Fcollectliterals(s,fs->hd);
      fs = fs->tl;
    }
    break;
  case natom:
  case patom:
    OSinsert(PLIT(f->a),s);
    OSinsert(NLIT(f->a),s);
    break;
  default: 1;
  }
}

void collectliterals(ordintset s,int op) {
  eff *effs;
  intlist *vs;

  Fcollectliterals(s,actions[op].precon);

  effs = actions[op].effects;

  while(effs != NULL) {

    Fcollectliterals(s,effs->condition);

    vs = effs->poseffects;
    while(vs != NULL) {
      OSinsert(PLIT(vs->hd),s);
      vs = vs->tl;
    }

    vs = effs->negeffects;
    while(vs != NULL) {
      OSinsert(NLIT(vs->hd),s);
      vs = vs->tl;
    }

    effs = effs->tl;
  }

}

/* Replace static variables with truth values */

fma *simplifyfmastatic(fma *f) {
  fmalist *fs;
  fmalist **prev;
  int trueone,falseone;
  switch(f->t) {
  case conj:
    falseone = 0;
    fs = f->juncts;
    prev = &(f->juncts);
    while(fs != NULL) {
      fs->hd = simplifyfmastatic(fs->hd);
      if(fs->hd->t == FALSE) { falseone = 1; break; }
      if(fs->hd->t == TRUE) { *prev = fs->tl; } /* TRUE conjunct: remove */
      else prev = &(fs->tl);
      fs = fs->tl;
    }
    if(falseone) f->t = FALSE;
    if(f->juncts == NULL) f->t = TRUE;
    else if(f->juncts->tl == NULL) return f->juncts->hd;
    break;
  case disj:
    trueone = 0;
    fs = f->juncts;
    prev = &(f->juncts);
    while(fs != NULL) {
      fs->hd = simplifyfmastatic(fs->hd);
      if(fs->hd->t == TRUE) { trueone = 1; break; }
      if(fs->hd->t == FALSE) { *prev = fs->tl; } /* FALSE disjunct: remove */
      else prev = &(fs->tl);
      fs = fs->tl;
    }
    if(trueone) f->t = TRUE;
    if(f->juncts == NULL) f->t = FALSE;
    else if(f->juncts->tl == NULL) return f->juncts->hd;
    break;
  case patom:
    switch(onelits[f->a]) {
    case -1: break;
    case 0: f->t = FALSE; break;
    case 1: f->t = TRUE; break;
    }
    break;
  case natom:
    switch(onelits[f->a]) {
    case -1: break;
    case 1: f->t = FALSE; break;
    case 0: f->t = TRUE; break;
    }
    break;
  default:
    break;
  }
  return f;
}

/* Remove static effects from a list of effect literals. */

intlist *removeirrelevant(intlist *ls) {
  if(ls == NULL) return NULL;
  if(onelits[ls->hd] != -1) return removeirrelevant(ls->tl);
  return intcons(ls->hd,removeirrelevant(ls->tl));

}

eff *simplifyeffstatic(eff *e) {
  if(e == NULL) return NULL;
  e->condition = simplifyfmastatic(e->condition);
  if(e->condition->t == FALSE) return simplifyeffstatic(e->tl);
  else {
    e->poseffects = removeirrelevant(e->poseffects);
    e->negeffects = removeirrelevant(e->negeffects);
    e->tl = simplifyeffstatic(e->tl);
    return e;
  }
}

/* Replace static variables by T of F. */

void simplifyoperatorsstatic() {
  int i,removed;
  removed = 0;
  i=0;
  while(i < nOfActions) {
    actions[i].effects = simplifyeffstatic(actions[i].effects);
    actions[i].precon = simplifyfmastatic(actions[i].precon);
    if(actions[i].precon->t == FALSE || actions[i].effects->condition->t == FALSE) {
      //      printf("REMOVING "); printaction(i);
      actions[i] = actions[nOfActions-1];
      removed += 1;
      nOfActions -= 1;
    } else i += 1;
  }
  if(debugOutput > 1 && removed) printf("Removed %i unapplicable actions.\n",removed);
}

void findfmaoccurrences(int op,fma *f,int polarity) {
  fmalist *fs;
  switch(f->t) {
  case conj:
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      findfmaoccurrences(op,fs->hd,polarity);
      fs = fs->tl;
    }
    break;
  case patom:
    if(polarity == 2) {
#ifdef DEBUG
      printatomi(f->a); printf(" occurs in ");
      printaction(op); printf("\n");
#endif
      OSinsert(op,preconoccP[f->a]);
    } else OSinsert(op,condocc[f->a]);
    break;
  case natom:
    if(polarity == 2) OSinsert(op,preconoccN[f->a]);
    else OSinsert(op,condocc[f->a]);
    break;
  default:
    break;
  }
}

/* Compute literals that are necessarily true when formula f is true.
This also takes into account known 2-literal invariants.
*/

void findnecessaryprecons(int i,fma *f) {
  int j;
  fmalist *fs;
  switch(f->t) {
  case patom:
    //    OSinsert1(f->a,preconP[i]);
    OSinsert(f->a,necessarypreconP[i]);
    OSinsert(i,necessarypreconofP[f->a]);
    jITstart(twolits[NLIT(f->a)]);
    while(jITnext(&j)) {
      if(j&1) {
	OSinsert(VAR(j),necessarypreconN[i]); /* Problem with Blai's example */
	OSinsert(i,necessarypreconofN[VAR(j)]);
      } else {
	OSinsert(VAR(j),necessarypreconP[i]); /* Problem with Blai's example */
	OSinsert(i,necessarypreconofP[VAR(j)]);
      }
    }
    break;
  case natom:
    //    OSinsert1(f->a,preconN[i]);
    OSinsert(f->a,necessarypreconN[i]);
    OSinsert(i,necessarypreconofN[f->a]);
    jITstart(twolits[LIT(f->a)]);
    while(jITnext(&j)) {
      if(j&1) {
	OSinsert(VAR(j),necessarypreconN[i]); /* Problem with Blai's example */
	OSinsert(i,necessarypreconofN[VAR(j)]);
      } else {
	OSinsert(VAR(j),necessarypreconP[i]); /* Problem with Blai's example */
	OSinsert(i,necessarypreconofP[VAR(j)]);
      }
    }
    break;
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      findnecessaryprecons(i,fs->hd);
      fs = fs->tl;
    }
    break;
  case disj:
    break;
  case TRUE:
  case FALSE:
    break;
  }
}

void findoccurrences() {
  int i;
  intlist *l;
  eff *e;
  int always;

  /* Which operators do a variable occur in positively/negatively ? */
  effectoccP = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));
  effectoccN = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));

  preconoccP = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));
  preconoccN = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));

  condocc = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));

  forcedeffectsP = (ordintset *)malloc(nOfActions * sizeof(ordintset));
  forcedeffectsN = (ordintset *)malloc(nOfActions * sizeof(ordintset));

  forcedeffectoccP = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));
  forcedeffectoccN = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));

  //  preconP = (ordintset *)malloc(nOfActions * sizeof(ordintset));
  //  preconN = (ordintset *)malloc(nOfActions * sizeof(ordintset));

  necessarypreconP = (ordintset *)malloc(nOfActions * sizeof(ordintset));
  necessarypreconN = (ordintset *)malloc(nOfActions * sizeof(ordintset));

  necessarypreconofP = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));
  necessarypreconofN = (ordintset *)malloc(nOfAtoms * sizeof(ordintset));

  /* We use the ordintset data structure here because the sets
     involving operators will be ordered without additional
     ordering effort.
     FIX: The lists of literals need effort to order. Should
     these be represented by some other data structure for
     additional efficiency? */


  for(i=0;i<nOfAtoms;i++) {
    effectoccP[i] = OScreateSize(20);
    effectoccN[i] = OScreateSize(20);
    preconoccP[i] = OScreateSize(20);
    preconoccN[i] = OScreateSize(20);
    condocc[i] = OScreateSize(20);
  }

  for(i=0;i<nOfActions;i++) {
    forcedeffectsP[i] = OScreateSize(10);
    forcedeffectsN[i] = OScreateSize(10);

    //    preconP[i] = OScreateSize(30);
    //    preconN[i] = OScreateSize(30);

    necessarypreconP[i] = OScreateSize(30);
    necessarypreconN[i] = OScreateSize(30);
  }

  for(i=0;i<nOfAtoms;i++) {
    forcedeffectoccP[i] = OScreateSize(10);
    forcedeffectoccN[i] = OScreateSize(10);

    necessarypreconofP[i] = OScreateSize(30);
    necessarypreconofN[i] = OScreateSize(30);
  }

  for(i=nOfActions-1;i>=0;i--) {

    /* Go through precondition */

    findnecessaryprecons(i,actions[i].precon);
    findfmaoccurrences(i,actions[i].precon,2);

    /* Go through effects and update effect occurrences */

    e = actions[i].effects;
    
    while(e != NULL) {

      findfmaoccurrences(i,e->condition,1);

      always = (e->condition->t == TRUE);

      l = e->poseffects;
      while(l != NULL) {
	assert(l->hd >= 0);
	OSinsert(i,effectoccP[l->hd]);
	if(always) OSinsert(i,forcedeffectoccP[l->hd]);
	if(always) OSinsert(l->hd,forcedeffectsP[i]);
	l = l->tl;
      }

      l = e->negeffects;
      while(l != NULL) {
	assert(l->hd >= 0);
	OSinsert(i,effectoccN[l->hd]);
	if(always) OSinsert(i,forcedeffectoccN[l->hd]);
	if(always) OSinsert(l->hd,forcedeffectsN[i]);
	l = l->tl;
      }

      e = e->tl;
    }

  }

  constructoperatorarrays();
}

/* Sort actions alphabetically according to their name. */

int actionCmp(action *a1,action *a2) {
  int v;
  intlist *n1,*n2;
  n1 = a1->name;
  n2 = a2->name;
  while(n1 != NULL && n2 != NULL) {
    v = strcmp(symbol(n1->hd),symbol(n2->hd));
    if(v != 0) return v;
    n1 = n1->tl; n2 = n2->tl;
  }
  if(n1 != NULL) return 1;
  return 0;
}

void sortactions() {
  qsort(actions,nOfActions,sizeof(action),actionCmp);
}

/* After detecting static variables, eliminate them so that the
variable numbering without the static variables is contiguous. */

int *mapping;

void renamefma(fma *f,int *mapping) {
  intlist *fs;
  switch(f->t) {
  case patom:
  case natom:
    f->a = mapping[f->a]; break;
  case conj:
  case disj:
    fs = f->juncts;
    while(fs != NULL) {
      renamefma(fs->hd,mapping);
      fs = fs->tl;
    }
    break;
  default: break;
  }
}

void renameeff(eff *e,int *mapping) {
  intlist *es;
  while(e != NULL) {
    renamefma(e->condition,mapping);
    es = e->poseffects;
    while(es != NULL) {
      es->hd = mapping[es->hd];
      es = es->tl;
    }
    es = e->negeffects;
    while(es != NULL) {
      es->hd = mapping[es->hd];
      es = es->tl;
    }
    e = e->tl;
  }
}

void renameaction(action *a,int *mapping) {
  renamefma(a->precon,mapping);
  renameeff(a->effects,mapping);
}

void renametwolits(intset is,int *mapping) {
  int i;
  for(i=0;i<is->nOfEls;i++) {
    if(1&(is->elements[i])) {
      is->elements[i] = NLIT(mapping[VAR(is->elements[i])]);
    } else {
      is->elements[i] = PLIT(mapping[VAR(is->elements[i])]);
    }
  }
}

/* Remove static variables completely, to make the numbering of
   variables contiguous. */

void eliminatestaticvariables() {
  int i;
  int from,to;
  int NEWnOfAtoms;
  mapping = (int *)malloc(nOfAtoms * sizeof(int));

  /* Do the mapping: move a variable one step earlier if the preceding
     variable is a static one. */

  //  for(i=0;i<nOfAtoms;i++) mapping[i] = i;

  to = 0;
  for(from = 0;from<nOfAtoms;from++) {
    if(onelits[from] == -1) { /* Variable is not static. */
      mapping[from] = to;
      to += 1;
    } else {
      mapping[from] = -1;
    }
  }
  NEWnOfAtoms = to;

  /* Elimination requires
     - renaming of the variables in actions
     - restructuring the index->name symbol table
     - twolits
     Elimination happens after the actions and the goal formula have been
     simplified (i.e. static variables have been replaced by T or F.)
  */

  for(i=0;i<nOfActions;i++) renameaction(&(actions[i]),mapping);
  for(i=0;i<nOfAtoms;i++) {
    renametwolits(twolits[PLIT(i)],mapping);
    renametwolits(twolits[NLIT(i)],mapping);
  }

  goal = simplifyfmastatic(goal);

  goalisdisjunctive = disjunctivep(goal);
  if(goalisdisjunctive) printf("Goal: disjunctive\n");
  else printf("Goal: conjunctive\n");

  renamefma(goal,mapping);

  /* Move twolits' contents into place. */

  for(i=0;i<nOfAtoms;i++) {
    if(mapping[i] != -1) {
      assert(mapping[i] >= 0);
      //      printf("mapping[%i] = %i.\n",i,mapping[i]);
      twolits[PLIT(mapping[i])] = twolits[PLIT(i)];
      twolits[NLIT(mapping[i])] = twolits[NLIT(i)];
    }
  }

  /* Fix initial state description. */
  
  for(i=0;i<nOfAtoms;i++) {
    if(mapping[i] != -1) initialstate[mapping[i]] = initialstate[i];
  }

  /* Fix symbol table: state vars' indices have changed! */

  renameatomtable(nOfAtoms,mapping);

  //  printf("WAS %i vars and IS %i vars.\n",nOfAtoms,NEWnOfAtoms);

  nOfAtoms = NEWnOfAtoms;

  printf("Simplified: %i ground actions and %i state variables\n",nOfActions,nOfAtoms);

}

/* Identify pairs of variables that always have the opposite truth value. */

void mergecontras() {
  int l0,l,l2,cnt;
  cnt = 0;
  for(l0=0;l0<nOfAtoms;l0++) {
    l = PLIT(l0);
    jITstart(twolits[l]);
    while(jITnext(&l2)) {
      if(VAR(l) > VAR(l2) && ISmember(NEG(l),twolits[NEG(l2)])) {
	if(l&1) printf("NOT ");
	printatomi(VAR(l));
	if(l2&1) printf("NOT ");
	printatomi(VAR(l2));
	printf(" are converses.\n");
	cnt += 1;
      }
    }
  }
  printf("TOTAL OF %i CONVERSES.\n",cnt);
}

/***************************************************************************/
/* Generate linear array representation of the main data structures here.  */
/***************************************************************************/

/* We will have an array representation of many of the lists in operators.h
  which were first constructed as linked lists. Linked lists have poor
  cache locality and using them has a relatively high performance penalty
  in many cases.
  Related arrays will be placed next to each other to minimize cache misses:
  e.g. preconP and preconN.
*/

void movearraydata(int index,ordintset *sourceset,int **destarray,int **fillptr) {
  int item;
  intlist *iterate;

  destarray[index] = *fillptr;

  OSstart(sourceset[index],&iterate);
  while(OSnext(&item,&iterate)) {
    *((*fillptr)++) = item;
  }
  *((*fillptr)++) = -1;

}

void constructoperatorarrays() {
  int allocsize;
  int i;
  int *fill;
  int *arrayfordata;

  /* Calculate the size of the array that is needed. */

  allocsize = 0;

  for(i=0;i<nOfAtoms;i++) { /* Go through state variable -indexed lists. */
    allocsize += OScard(effectoccP[i]);
    allocsize += OScard(effectoccN[i]);
    allocsize += OScard(condocc[i]);
    allocsize += OScard(preconoccP[i]);
    allocsize += OScard(preconoccN[i]);
  }

  allocsize += 5*nOfAtoms; /* space for end-of-array markers -1 */

  for(i=0;i<nOfActions;i++) { /* Go through action-indexed lists. */
    //    allocsize += OScard(preconP[i]);
    //    allocsize += OScard(preconN[i]);
    allocsize += OScard(necessarypreconP[i]);
    allocsize += OScard(necessarypreconN[i]);
    allocsize += OScard(forcedeffectsP[i]);
    allocsize += OScard(forcedeffectsN[i]);
  }

  allocsize += 6*nOfActions; /* space for end-of-array markers -1 */


  /* Allocate pointer arrays for actions and state variables. */
  
  //  ApreconP = (int **)malloc(nOfActions * sizeof(int *));
  //  ApreconN = (int **)malloc(nOfActions * sizeof(int *));
  AnecessarypreconP = (int **)malloc(nOfActions * sizeof(int *));
  AnecessarypreconN = (int **)malloc(nOfActions * sizeof(int *));
  AforcedeffectsP = (int **)malloc(nOfActions * sizeof(int *));
  AforcedeffectsN = (int **)malloc(nOfActions * sizeof(int *));
  
  AeffectoccP = (int **)malloc(nOfAtoms * sizeof(int *));
  AeffectoccN = (int **)malloc(nOfAtoms * sizeof(int *));
  ApreconoccP = (int **)malloc(nOfAtoms * sizeof(int *));
  ApreconoccN = (int **)malloc(nOfAtoms * sizeof(int *));
  Acondocc = (int **)malloc(nOfAtoms * sizeof(int *));

  arrayfordata = (int *)malloc(allocsize * sizeof(int));

  assert(arrayfordata != NULL);

  fill = arrayfordata;

  /* Fill the massive array, and put pointers to the individual atom's
     and actions's arrays. */

  for(i=0;i<nOfActions;i++) {
    //    movearraydata(i,preconP,ApreconP,&fill);
    //    movearraydata(i,preconN,ApreconN,&fill);
    movearraydata(i,necessarypreconP,AnecessarypreconP,&fill);
    movearraydata(i,necessarypreconN,AnecessarypreconN,&fill);
    movearraydata(i,forcedeffectsP,AforcedeffectsP,&fill);
    movearraydata(i,forcedeffectsN,AforcedeffectsN,&fill);
  }

  for(i=0;i<nOfAtoms;i++) {
    movearraydata(i,effectoccP,AeffectoccP,&fill);
    movearraydata(i,effectoccN,AeffectoccN,&fill);
    movearraydata(i,preconoccP,ApreconoccP,&fill);
    movearraydata(i,preconoccN,ApreconoccN,&fill);
    movearraydata(i,condocc,Acondocc,&fill);
  }

}

/* Check if a formula is a conjunction of 1 or more atomic formulas. */

int conjunctivep(fma *f) {
  fmalist *fs;
  switch(f->t) {
  case disj: return 0;
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      if(!conjunctivep(fs->hd)) return 0;
      fs = fs->tl;
    }
  default: return 1;
  }
}

/* Check if a formula has a fixed truth-value (TRUE or FALSE).
   This is a simple syntactic test, not a full SAT/TAUT test. */

int constantp(fma *f) {
  fmalist *fs;
  switch(f->t) {
  case patom:
  case natom:
    return 0;
  case disj:
  case conj:
    fs = f->juncts;
    while(fs != NULL) {
      if(!constantp(fs->hd)) return 0;
      fs = fs->tl;
    }
    return 1;
  case TRUE:
  case FALSE:
    return 1;
  }
}

int STRIPSaction(int i) {
  eff *e;
  if(!conjunctivep(actions[i].precon)) return 0;
  e = actions[i].effects;
  while(e != NULL) {
    if(!constantp(e->condition)) return 0;
    e = e->tl;
  }
  return 1;
}

int CONJUNCTIVEaction(int i) {
  eff *e;
  if(!conjunctivep(actions[i].precon)) return 0;
  e = actions[i].effects;
  while(e != NULL) {
    if(!conjunctivep(e->condition)) return 0;
    e = e->tl;
  }
  return 1;
}

syntacticclass actionclass(int i) {
  eff *e;
  syntacticclass class;

  if(!conjunctivep(actions[i].precon)) return GeneralPDDL;
  e = actions[i].effects;

  class = STRIPS;
  while(e != NULL) {
    if(!conjunctivep(e->condition)) return GeneralPDDL;
    if(!constantp(e->condition)) class = Conjunctive;
    e = e->tl;
  }
  return class;
}

syntacticclass goalclass() {
  if(!conjunctivep(goal)) return GeneralPDDL;
  else return STRIPS;
}
