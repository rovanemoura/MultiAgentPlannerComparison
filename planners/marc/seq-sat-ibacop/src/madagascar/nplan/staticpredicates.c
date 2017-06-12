
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>

#include "asyntax.h"
#include "main.h"
#include "tables.h"
#include "intsets.h"
#include "ordintsets.h"
#include "operators.h"
#include "instantiation.h"

#include <malloc.h>

intlist *elimparams;

#define noDEBUG

/* Test whether a predicate in a precondition formula is
eliminable, and, eliminate it right away. The variable
elimparams lists the atom's parameters so that we can
produce the instances of the schema corresponding
to different initial literals. */

int eliminablestatic(Sfma *f) {
  Sfmalist *fs;
  switch(f->t) {
  case Spatom:
    if(staticp(f->a->pred)) {
#ifdef DEBUG
      printf("Predicate %s is static and eliminable\n",symbol(f->a->pred));
#endif
      elimparams = f->a->params;
      f->t = STRUE;
      return f->a->pred;
    }
    break;
  case Sconj:
    fs = f->juncts;
    while(fs != NULL) {
      int i = eliminablestatic(fs->hd);
      if(i) return i;
      fs = fs->tl;
    }
    break;
  default:
    break;
  }
  return 0;
}

int match(atom *a,int p,intlist *ps) {
  intlist *ips;
  if(a->pred != p) return 0;
  ips = a->params;
  while(ps != NULL) {
    if(ips == NULL) return 0; /* One atom had fewer parameters */
    if(ps->hd >= 0 && (ps->hd != ips->hd)) return 0; /* Mismatching objects */
    if(ps->hd < 0) { /* It's a variable.. */
      ebinding[-1-(ps->hd)] = ips->hd; /* ..getting value from the init. */
#ifdef DEBUG
      printf("#%i / %s\n",-1-(ps->hd),symbol(ips->hd));
#endif
    }
    ps = ps->tl;
    ips = ips->tl;
  }
  if(ips != NULL) return 0; /* One atom had fewer parameters */
  return 1;
}


/* Main function for eliminating static predicates */

void eliminatestatic() {
  int i,p;
  atomlist *init;
  for(i=0;i<nOfSActions;i++) {
  tryagain:
    p = eliminablestatic(Sactions[i].precon);
    if(p) { /* Eliminate one instance of predicate p */
#ifdef DEBUG
      printf("Eliminating predicate %s\n",symbol(p));
#endif
      /* Go through the initial state description */
      init = Sinit;
      while(init != NULL) {
	/* If atom matches with the static one, instantiate */
	initializeebinding();
	if(match(init->hd,p,elimparams)) {
	  nOfSActions += 1;
	  checkSactionsSize();
	  Sactions[nOfSActions-1].name = Sactions[i].name;
	  Sactions[nOfSActions-1].params = instparamlist(Sactions[i].params,0);
	  Sactions[nOfSActions-1].precon = instfma(Sactions[i].precon);
	  Sactions[nOfSActions-1].effect = insteff(Sactions[i].effect);
#ifdef DEBUG
	  printf("NEW INSTANCE No. %i\n",nOfSActions-1);
	  printSaction(&Sactions[nOfSActions-1]);
#endif
	}
	init = init->tl;
      }
      Sactions[i] = Sactions[nOfSActions-1];
      nOfSActions -= 1;
      goto tryagain; /* There could be another eliminable predicate. */
    }
  }
}
