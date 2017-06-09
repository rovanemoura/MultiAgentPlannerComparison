
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>

#include "asyntax.h"
#include "intsets.h"
#include "ordintsets.h"
#include "main.h"
#include "tables.h"
#include "operators.h"
#include "instantiation.h"

#include <malloc.h>

#define DEBUGX

void initializeebinding() {
  int i;
  for(i=0;i<20;i++) ebinding[i] = -1;
}

/* Make a copy of an atom and instantiate according to ebinding. */

intlist *instlist(intlist *l) {
  int h;
  if(l == NULL) return NULL;
  if(l->hd < 0 && ebinding[-1-(l->hd)] >= 0) h = ebinding[-1-(l->hd)];
  else h = l->hd;
  return intcons(h,instlist(l->tl));
}

atom *instatom(atom *a) {
  atom *a2 = (atom *)malloc(sizeof(atom));
  a2->pred = a->pred;
  a2->params = instlist(a->params);
  return a2;
}

/* Make of a copy of a paramlist and instantiate according to ebinding. */

typedvarlist *instparamlist(typedvarlist *params,int i) {
  typedvarlist *p;
  if(params == NULL) return NULL;
  p = (typedvarlist *)malloc(sizeof(typedvarlist));
  if(isvar(params->v) && ebinding[i] != -1) {
    p->v = ebinding[i];
  } else {
    p->v = params->v;
  }
  p->t = params->t;
  p->tl = instparamlist(params->tl,i+1);
  return p;
}

/* Make of a copy of a formula and instantiate according to ebinding. */

Sfma *instfma(Sfma *);

Sfmalist *instfmalist(Sfmalist *l) {
  if(l == NULL) return NULL;
  return Sfmacons(instfma(l->hd),instfmalist(l->tl));
}

int evalue(int v) {
  if(v >= 0) return v;
  //  printf("binding variable %i to %s\n",-1-v,symbol(ebinding[-1-v]));
  if(ebinding[-1-v] >= 0) return ebinding[-1-v];
  return v;
}

Sfma *instfma(Sfma *f) {
  Sfma *f2 = (Sfma *)malloc(sizeof(Sfma));
  f2->t = f->t;
  switch(f->t) {
  case Spatom:
  case Snatom:
    f2->a = instatom(f->a);
    break;
  case Sconj:
  case Sdisj:
    f2->juncts = instfmalist(f->juncts);
    break;
  case Sforall:
  case Sforsome:
    f2->ss = f->ss;
    f2->f = instfma(f->f);
    break;
  case Sneq:
  case Seq:
    f2->p1 = evalue(f->p1);
    f2->p2 = evalue(f->p2);
    break;
  case STRUE:
  case SFALSE:
  default:
    break;
  }
  return f2;
}

/* Make of a copy of an effect and instantiate according to ebinding. */

Seff *insteff(Seff *);

Sefflist *instefflist(Sefflist *l) {
  if(l == NULL) return NULL;
  return Seffcons(insteff(l->hd),instefflist(l->tl));
}

Seff *insteff(Seff *e) {
  Seff *e2 = (Seff *)malloc(sizeof(Seff));
  e2->t = e->t;
  switch(e->t) {
  case SEpatom:
  case SEnatom:
    e2->a = instatom(e->a);
    break;
  case SEconj:
    e2->juncts = instefflist(e->juncts);
    break;
  case SEforall:
    e2->effect = insteff(e->effect);
    break;
  case SEwhen:
    e2->cond = instfma(e->cond);
    e2->effect = insteff(e->effect);
    break;
  default:
    break;
  }
  return e2;
}
