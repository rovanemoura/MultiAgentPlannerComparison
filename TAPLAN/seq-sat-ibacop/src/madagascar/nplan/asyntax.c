
  /*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <assert.h>

#include "asyntax.h"
#include "tables.h"
#include "intsets.h"
#include "ordintsets.h"
#include "operators.h"
#include "main.h"
#include "instantiation.h"


#include "y.tab.h"

/* DESTRUCTIVE negation of a formula */

Sfma *Sneg(Sfma *f) {
  Sfmalist *l;
  switch(f->t) {
  case STRUE: f->t = SFALSE; break;
  case SFALSE: f->t = STRUE; break;
  case Spatom: f->t = Snatom; break;
  case Snatom: f->t = Spatom; break;
  case Sconj: f->t = Sdisj;
    l = f->juncts;
    while(l != NULL) {
      Sneg(l->hd);
      l = l->tl;
    }
    break;
  case Sdisj: f->t = Sconj;
    l = f->juncts;
    while(l != NULL) {
      Sneg(l->hd);
      l = l->tl;
    }
    break;
  case Sforall: f->t = Sforsome; Sneg(f->f); break;
  case Sforsome: f->t = Sforall; Sneg(f->f); break;
  case Seq: f->t = Sneq; break;
  case Sneq: f->t = Seq; break;
  }
  return f;
}

/* constructors for schematic formulae */

Sfma* Sdisjunction(Sfmalist *fs) {
  Sfma *f = (Sfma*)malloc(sizeof(Sfma));
  f->t = Sdisj;
  f->juncts = fs;
  return f;
}

Sfma* Sconjunction(Sfmalist *fs) {
  Sfma *f = (Sfma*)malloc(sizeof(Sfma));
  f->t = Sconj;
  f->juncts = fs;
  return f;
}

Sfma* Satom(atom *a) {
  Sfma *f = (Sfma*)malloc(sizeof(Sfma));
  f->t = Spatom;
  f->a = a;
  return f;
}

Sfma* Sfalse() {
  Sfma *f = (Sfma*)malloc(sizeof(Sfma));
  f->t = SFALSE;
  return f;
}

Sfma* Strue() {
  Sfma *f = (Sfma*)malloc(sizeof(Sfma));
  f->t = STRUE;
  return f;
}

Sfma* Sfmaforall(typedvarlist *ss, Sfma *f) {
  Sfma *f1 = (Sfma*)malloc(sizeof(Sfma));
  f1->t = Sforall;
  f1->ss = ss;
  f1->f = f;
  return f1;
}

Sfma* SconstantTRUE() {
  Sfma *f = (Sfma*)malloc(sizeof(Sfma));
  f->t = STRUE;
  return f;
}

Sfma* Sfmaforsome(typedvarlist *ss, Sfma *f) {
  Sfma *f1 = (Sfma*)malloc(sizeof(Sfma));
  f1->t = Sforsome;
  f1->ss = ss;
  f1->f = f;
  return f1;
}

Sfma* SfmaEQ(int p1, int p2) {
  Sfma *f1 = (Sfma*)malloc(sizeof(Sfma));
  f1->t = Seq;
  f1->p1 = p1;
  f1->p2 = p2;
  return f1;
}

atom *makeatom(int pr,intlist *pars) {
  atom *a = (atom *)malloc(sizeof(atom));
  a->pred = pr;
  a->params = pars;
  return a;
}

/* Printing */

void printSfmalist(Sfmalist *);
void printSefflist(Sefflist *);

void printstringlist(intlist *sl) {
  while(sl != NULL) {
    if(sl->hd < 0) printf("#%i",-1-(sl->hd));
    else printf("%s",symbol(sl->hd));
    if(sl->tl != NULL) printf(",");
    sl = sl->tl;
  }
}

void printatom(atom *a) {
  printf("%s(",symbol(a->pred));
  printstringlist(a->params);
  printf(")");
}

void printtypedvars(typedvarlist *ss) {
  printf(" (");
  while(ss != NULL) {
    printf("%s:%s",symbol(ss->v),symbol(ss->t));
    if(ss->tl != NULL) printf(" ");
    ss = ss->tl;
  }
  printf(")");
}

void printSfma(Sfma *f) {
  switch(f->t) {
  case STRUE: printf("TRUE"); break;
  case SFALSE: printf("FALSE"); break;
  case Seq: printf(" (= %s %s)",symbol(f->p1),symbol(f->p2)); break;
  case Sneq: printf(" (not (= %s %s))",symbol(f->p1),symbol(f->p2)); break;
  case Spatom: printatom(f->a); break;
  case Snatom: printf("(not "); printatom(f->a); printf(")"); break;
  case Sdisj: printf("(or "); printSfmalist(f->juncts); printf(")"); break;
  case Sconj: printf("(and "); printSfmalist(f->juncts); printf(")"); break;
  case Sforall: printf("(forall "); printtypedvars(f->ss); printSfma(f->f); printf(")"); break;
  case Sforsome: printf("(exists "); printtypedvars(f->ss); printSfma(f->f); printf(")"); break;
    
  }
}

void printSfmalist(Sfmalist *fs) {
  while(fs != NULL) {
    printSfma(fs->hd);
    printf(" ");
    fs = fs->tl;
  }
}

void printSeff(Seff *);
void printSefflist(Sefflist *fs) {
  while(fs != NULL) {
    printSeff(fs->hd);
    printf(" ");
    fs = fs->tl;
  }
}

void printSeff(Seff *e) {
  switch(e->t) {
  case SEpatom: printatom(e->a); break;
  case SEnatom: printf("(not "); printatom(e->a); printf(")"); break;
  case SEconj: printf("(and "); printSefflist(e->juncts); printf(")"); break;
  case SEforall: printf("(forall "); printtypedvars(e->ss); printSeff(e->effect); printf(")"); break;
  case SEwhen:
    printf("(when ");
    printSfma(e->cond);
    printf(" ");
    printSeff(e->effect);
    printf(")");
    break;
  }
}

void printSaction(Saction *a) {
  typedvarlist *l;
  printf(":action %s(",symbol(a->name));
  l = a->params;
  while(l != NULL) {
    printf("%s",symbol(l->v));
    if(l->t) printf(":%s",symbol(l->t));
    else printf(":UNIV");
    if(l->tl != NULL) printf(" ");
    l = l->tl;
  }
  printf(")\n");
  printSfma(a->precon);
  printf("\n");
  printSeff(a->effect);
  printf("\n\n");
}

/* constructors for schematic effects */

Seff* SPeffatom(atom *a) {
  Seff *e = (Seff *)malloc(sizeof(Seff));
  e->t = SEpatom;
  e->a = a;
  return e;
}

Seff* SNeffatom(atom *a) {
  Seff *e = (Seff *)malloc(sizeof(Seff));
  e->t = SEnatom;
  e->a = a;
  return e;
}


Seff* Seffconjunction(Sefflist *efs) {
  Seff *e = (Seff *)malloc(sizeof(Seff));
  e->t = SEconj;
  e->juncts = efs;
  return e;
}

Seff* Seffwhen(Sfma *c,Seff *e) {
  Seff *e1 = (Seff *)malloc(sizeof(Seff));
  e1->t = SEwhen;
  e1->cond = c;
  e1->effect = e;
  return e1;
}

Seff* Seffforall(typedvarlist *p,Seff *e) {
  Seff *e1 = (Seff *)malloc(sizeof(Seff));
  e1->t = SEforall;
  e1->ss = p;
  e1->effect = e;
  return e1;
}

/* Create atom */

atom *newatom(int s,intlist *p) {
  atom *a = (atom *)malloc(sizeof(atom));
  a->pred = s;
  a->params = p;
  return a;
}

         /* PDDL domain definitions */

int nOfTypes;

obtype Stypes[10000];

Sfma *Sgoal;

typedvarlist *Stypes0;

void initPDDLshit() {
  nOfSActions = 0;
  maxSActions = 100000;
  Sactions = (Saction *)malloc(sizeof(Saction) * maxSActions);
  nOfTypes = 0;
  Sgoal = (Sfma *)0;
  Stypes0 = NULL;
}

/* Definitions */


/* Typed var lists */

typedvarlist *TVappend(typedvarlist *l1,typedvarlist *l2) {
  if(l1 == NULL) {
    return l2;
  } else {
    typedvarlist *l3 = TVappend(l1->tl,l2);
    typedvarlist *l4 = (typedvarlist *)malloc(sizeof(typedvarlist));
    l4->v = l1->v;
    l4->t = l1->t;
    l4->tl = l3;
    return l4;
  }
}

/* For a (possibly untyped) list of variables, assign a type */

typedvarlist *withtype(int t,intlist *ss) {
  typedvarlist *l;
  if(ss == NULL) return NULL;
  l = (typedvarlist *)malloc(sizeof(typedvarlist));
  l->v = ss->hd;
  l->t = t;
  l->tl = withtype(t,ss->tl);
  return l;
}

/* Add a new action */

void checkSactionsSize() {
  if(nOfSActions >= maxSActions-1) {
    maxSActions = maxSActions * 3 / 2;
    Sactions = (Saction *)realloc(Sactions,maxSActions * sizeof(Saction));
    assert(Sactions != NULL);
  }
}

void addnewaction(int name) {
  nOfSActions += 1;
  checkSactionsSize();
  Sactions[nOfSActions-1].name = name;
  if(Sactions[nOfSActions-1].effect == NULL) {
    fprintf(stderr,"ERROR: action has not effect.\n");
    exit(1);
  }
  if(Sactions[nOfSActions-1].precon == NULL) {
    Sactions[nOfSActions-1].precon = SconstantTRUE();
  }
  /* Next action */
  Sactions[nOfSActions].precon = NULL;
  Sactions[nOfSActions].effect = NULL;
  Sactions[nOfSActions].params = NULL;
}

/* The following three are called by the parser BEFORE addnewaction */

void addactionparameters(typedvarlist *params) {
  Sactions[nOfSActions].params = params;
}

void addactionprecond(Sfma *p) {
  Sactions[nOfSActions].precon = p;
}

/* Go through the predicates in an action effect and
   mark non-static ones. */

void checkifstatic(Seff *e) {
  atom *a;
  Sefflist *es;
  switch(e->t) {
  case SEpatom:
  case SEnatom:
    a = e->a;
    setnonstatic(a->pred);
    break;
  case SEconj:
    es = e->juncts;
    while(es != NULL) {
      checkifstatic(es->hd);
      es = es->tl;
    }
    break;
  case SEwhen:
  case SEforall:
    checkifstatic(e->effect); break;
  default:
    break;
  }
}

void addactioneffect(Seff *e) {
  Sactions[nOfSActions].effect = e;
  checkifstatic(e);
}

/* Requirements */

void checkrequirements(intlist *l) {
  while(l != NULL) {
    if(strcmp(symbol(l->hd),":strips") == 0) {
    } else if(strcmp(symbol(l->hd),":conditional-effects") == 0) {
    } else if(strcmp(symbol(l->hd),":adl") == 0) {
    } else if(strcmp(symbol(l->hd),":typing") == 0) {
    } else if(strcmp(symbol(l->hd),":equality") == 0) {
    } else if(strcmp(symbol(l->hd),":typing") == 0) {
    } else if(strcmp(symbol(l->hd),":conditional-effects") == 0) {
    } else if(strcmp(symbol(l->hd),":negative-preconditions") == 0) {
    } else if(strcmp(symbol(l->hd),":quantified-preconditions") == 0) {
    } else if(strcmp(symbol(l->hd),":action-costs") == 0) {
    } else {
      fprintf(stderr,"WARNING: unsupported :requirement %s\n",symbol(l->hd));
    }

    if(strcmp(symbol(l->hd),":action-costs") == 0) {
      fprintf(stderr,"WARNING: will ignore action costs\n");
    }

    l = l->tl;
  }
}

    /* Handling types and objects */

/* Destructive addition of a non-duplicate element to a NON-EMPTY list */
void addto(int s,intlist *l) {
  while(l != NULL) {
    if(s == l->hd) return;
    if(l->tl == NULL) {
      l->tl = intcons(s,EMPTYLIST);
      return;
    }
    l = l->tl;
  }
  assert(1==0);
}

void addobject(int v,int t) {
  int i;
  i = 0;
  while(i<nOfTypes) {
    if(t == Stypes[i].typename) { /* Add to type */
      if(Stypes[i].elements == NULL) {
	Stypes[i].elements = intcons(v,EMPTYLIST);
      } else {
	addto(v,Stypes[i].elements);
      }
      return;
    }
    i+=1;
  }
  nOfTypes += 1;
  Stypes[nOfTypes-1].typename = t;
  Stypes[nOfTypes-1].elements = intcons(v,EMPTYLIST);
}

void addsubtyping(int t,int t2) {
  int i;
  intlist *ss;
  i=0;
  while(i<nOfTypes) {
    if(Stypes[i].typename == t) {
      ss = Stypes[i].elements;
      while(ss != NULL) {
	addobject(ss->hd,t2);
	ss = ss->tl;
      }
      return;
    }
    i+=1;
  }
  //  fprintf(stderr,"WARNING: subtype %s is empty\n",symbol(t));
}

/* Predicate definition */

void storepredicates() {
  /* We don't use the predicate definition for anything. */
  /* It could be used for some form of type-checking. */
}

/* Constant definitions */

void storeconstants(typedvarlist *cs) {
  while(cs != NULL) {
    addobject(cs->v,UNIVTYPE);
    addobject(cs->v,cs->t);
    cs = cs->tl;
  }
}

/* Type definitions */

/* This is for handling subtyping */

void storetypes(typedvarlist *ts) {
  Stypes0 = ts;
}

void processtypes() {
  typedvarlist *ts = Stypes0;
  while(ts != NULL) {
    if(ts->t != UNIVTYPE) {
      addsubtyping(ts->v,ts->t);
    }
    ts = ts->tl;
  }
}

        /* PDDL problem definitions */

/* Object definitions */

void storeobjects(typedvarlist *cs) {
  while(cs != NULL) {
    addobject(cs->v,UNIVTYPE);
    addobject(cs->v,cs->t);
    cs = cs->tl;
  }
}

void storeinit(atomlist *a) {
  Sinit = a;
}

void storegoal(Sfma *f) {
  Sgoal = f;
}

/* Domain name */

int domain,problem;

void storedomain(int s) { domain = s; }
void checkdomain(int s) {
  if(s != domain) {
    fprintf(stderr,"WARNING: problem domain '%s' does not match domain name '%s'\n",symbol(s),symbol(domain));
  }
}
char *domainname() { return symbol(domain); }

void addproblem(int s) { problem = s; }
char *problemname() { return symbol(problem); }

/* Lists */

Sfmalist *Sfmacons(Sfma *h,Sfmalist *t) {
  Sfmalist *r = (Sfmalist *)malloc(sizeof(Sfmalist));
  r->hd = h;
  r->tl = t;
  return r;
}

Sefflist *Seffcons(Seff *h,Sefflist *t) {
  Sefflist *r = (Sefflist *)malloc(sizeof(Sefflist));
  r->hd = h;
  r->tl = t;
  return r;
}

intlist *intcons(int h,intlist *t) {
  intlist *r = (intlist *)malloc(sizeof(intlist));
  r->hd = h;
  r->tl = t;
  return r;
}

atomlist *atomcons(atom *h,atomlist *t) {
  atomlist *r = (atomlist *)malloc(sizeof(atomlist));
  r->hd = h;
  r->tl = t;
  return r;
}

/* Reading and processing an input file */

void showstatistics() {
  int i;
  printf("%3i action schemata\n",nOfSActions);
  for(i=0;i<nOfSActions;i++) {
    printSaction(&(Sactions[i]));
  }
  printf("%3i types\n",nOfTypes);
  for(i=0;i<nOfTypes;i++) {
    intlist *ss;
    printf("%s consists of",symbol(Stypes[i].typename));
    ss = Stypes[i].elements;
    while(ss != NULL) {
      printf(" %s",symbol(ss->hd));
      ss = ss->tl;
    }
    printf("\n");
  }
}

void readfile() {
  linenumber = 1;
  if(nOfInputFiles == 0) {
    printf("Reading from standard input\n");
  } else {
    lexeropen(inputfiles[0]);
  }
  errorstring = "";
  initPDDLshit();
  initsymboltable();
  UNIVTYPE = symbolindex("***UNIVTYPE***");
  yyparse();
  processtypes();
  if(flagShowInput) showstatistics();
}

void yyerror() {
  printf("%s on line %i.\n",errorstring,linenumber);
  exit(1);
}

    /* Grounding operators */

/* replace parameter variables by their index in the parameter list */

int paramindex(int i,typedvarlist *params,int strict) {
  int j = 0;
  while(params != NULL) {
    if(params->v == i) return -1-j;
    params = params->tl;
    j += 1;
  }
  if(strict) {
    fprintf(stderr,"ERROR: variable %s not any of the operator parameters\n",symbol(i));
    exit(1);
  } else {
    return i;
  }
}

/* This applies paramindex in the right places */

void preprocessatom(typedvarlist *params,atom *a,int strict) {
  intlist *l;
  l = a->params;
  while(l != NULL) {
    if(isvar(l->hd)) {
      //      printf("variable %s ",symbol(l->hd));
      l->hd = paramindex(l->hd,params,strict);
      //      if(l->hd < 0) printf("got index %i.\n",l->hd);
      //      else printf("is nout bound.\n");
    }
    l = l->tl;
  }
}

/* This applies paramindex in the right places */

void preprocessfma(typedvarlist *params,Sfma *f,int strict) {
  Sfmalist *l;
  switch(f->t) {
  case STRUE:
  case SFALSE:
    break;
  case Spatom: 
  case Snatom: 
    preprocessatom(params,f->a,strict);
    break;
  case Sdisj:
  case Sconj:
    l = f->juncts;
    while(l != NULL) {
      preprocessfma(params,l->hd,strict);
      l = l->tl;
    }
    break;
  case Sforall:
  case Sforsome:
    preprocessfma(params,f->f,strict);
    break;
  case Seq:
  case Sneq:
    if(isvar(f->p1)) f->p1 = paramindex(f->p1,params,strict);
    if(isvar(f->p2)) f->p2 = paramindex(f->p2,params,strict);
    break;
  }
}

/* This applies paramindex in the right places */

void preprocesseff(typedvarlist *params,Seff *e,int strict) {
  Sfmalist *l;
  switch(e->t) {
  case SEpatom:
  case SEnatom:
    preprocessatom(params,e->a,strict);
    break;
  case SEconj:
    l = e->juncts;
    while(l != NULL) {
      preprocesseff(params,l->hd,strict);
      l = l->tl;
    }
    break;
  case SEforall:
    preprocesseff(params,e->effect,strict);
    break;
  case SEwhen:
    preprocessfma(params,e->cond,strict);
    preprocesseff(params,e->effect,strict);
    break;
  }
}

/* elimination of quantification */

intlist *getdomain(int);

/* Go through all valuations of the quantified variables and
   construct a list of instances with each valuation. */

Sfmalist *qgothrough(typedvarlist *ps,Sfma *f,int i,Sfmalist *ac) {
  if(ps == NULL) {
    return Sfmacons(instfma(f),ac);
  } else {
    intlist *vs;

    //    printf("Getting domain of %s:%s\n",symbol(ps->v),symbol(ps->t));
    vs = getdomain(ps->t);

    while(vs != NULL) {
      ebinding[i] = vs->hd;
      ac = qgothrough(ps->tl,f,i+1,ac);
      vs = vs->tl;
    }

    return ac;
  }
}

Sefflist *qEgothrough(typedvarlist *ps,Seff *e,int i,Sefflist *ac) {
  if(ps == NULL) {
    return Seffcons(insteff(e),ac);
  } else {
    intlist *vs;

    vs = getdomain(ps->t);

    while(vs != NULL) {
      ebinding[i] = vs->hd;
      /*      printf("Var %i has value %s\n",i,symbol(vs->hd)); */
      ac = qEgothrough(ps->tl,e,i+1,ac);
      vs = vs->tl;
    }
    return ac;
  }
}

int ELlength(Sefflist *l) {
  if(l == NULL) return 0;
  return 1+ELlength(l->tl);
}

int FLlength(Sfmalist *l) {
  if(l == NULL) return 0;
  return 1+FLlength(l->tl);
}

Sfmalist *eliminateq(typedvarlist *ps,Sfma *f) {
  Sfmalist *l;
  l = NULL;
  preprocessfma(ps,f,0);
  initializeebinding();
  return qgothrough(ps,f,0,NULL);
}

Sefflist *eliminateEq(typedvarlist *ps,Seff *e) {
  Sefflist *l;
  l = NULL;
  preprocesseff(ps,e,0);
  initializeebinding();
  return qEgothrough(ps,e,0,NULL);
}

void eliminatefmaquant(Sfma *f) {
  Sfmalist *fs;
  switch(f->t) {
  case Sconj:
  case Sdisj:
    fs = f->juncts;
    while(fs != NULL) {
      eliminatefmaquant(fs->hd);
      fs = fs->tl;
    }
    break;
  case Sforall:
    eliminatefmaquant(f->f);
    f->t = Sconj;
    f->juncts = eliminateq(f->ss,f->f);
    break;
  case Sforsome:
    eliminatefmaquant(f->f);
    f->t = Sdisj;
    f->juncts = eliminateq(f->ss,f->f);
    break;
  default:
    break;
  }
}

void eliminateeffquant(Seff *e) {
  Sefflist *es;
  switch(e->t) {
  case SEconj:
    es = e->juncts;
    while(es != NULL) {
      eliminateeffquant(es->hd);
      es = es->tl;
    }
    break;
  case SEwhen:
    eliminatefmaquant(e->cond);
    eliminateeffquant(e->effect);
    break;
  case SEforall:
    eliminateeffquant(e->effect);
    e->t = SEconj;
    e->juncts = eliminateEq(e->ss,e->effect);
    break;
  default:
    break;
  }
}

/* Some of the IPC benchmarks (Schedule) contain funny conditionals
   (when A (not A)) which are there to reduce the possible parallelism.
   These conditional can be replaced by (not A) without changing
   their semantics.

   THIS REDUCTION IS WRONG!
*/

int complementaryliteral(Sfma *f,Seff *e) {
  if(f->t == Spatom && e->t == SEnatom) return 1;
  if(f->t == Snatom && e->t == SEpatom) return 1;
  return 0;
}

int equalatom(atom *a1,atom *a2) {
  intlist *l1,*l2;
  if(a1->pred != a2->pred) return 0;
  l1 = a1->params;
  l2 = a2->params;
  while(l1 != NULL && l2 != NULL) {
    /* Test parameter equality. */
    if(l1->hd != l2->hd) return 0;
    l1 = l1->tl;
    l2 = l2->tl;
  }
  return 1;
}

int unconditionaleffp0(Seff *e,atom *a,int negated) {
  Sefflist *es;
  switch(e->t) {
  case SEpatom:
    if(negated == 0 && equalatom(e->a,a)) return 1;
    break;
  case SEnatom:
    if(negated == 1 && equalatom(e->a,a)) return 1;
    break;
  case SEconj:
    es = e->juncts;
    while(es != NULL) {
      if(unconditionaleffp0(es->hd,a,negated)) return 1;
      es = es->tl;
    }
    break;
  case SEwhen:
    break;
  }
  return 0;
}

int unconditionaleffp(int i,atom *a,int negated) {
  return unconditionaleffp0(Sactions[i].effect,a,negated);
}

int simplifysillyconditionals1(int i,Seff *e0,Seff *e) {
  Sefflist *es;
  int flag;
  switch(e->t) {
  case SEconj:
    es = e->juncts;
    flag = 0;
    while(es != NULL) {
      flag = simplifysillyconditionals1(i,e0,es->hd);
      es = es->tl;
    }
    return flag;
    break;
  case SEpatom: /* Test whether positive effect is always contradicted. */
    if(unconditionaleffp(i,e->a,1)) {
      /* Add condition as a precondition. */
      Sactions[i].precon = Sconjunction(Sfmacons(Sactions[i].precon,Sfmacons(Sneg(e0->cond),NULL)));
      /* Remove */
      e0->cond = Sfalse();
      return 1;
    }
    break;
  case SEnatom: /* Test whether negative effect is always contradicted. */
    if(unconditionaleffp(i,e->a,0)) {
      /* Add condition as a precondition. */
      Sactions[i].precon = Sconjunction(Sfmacons(Sactions[i].precon,Sfmacons(Sneg(e0->cond),NULL)));
      /* Remove */
      e0->cond = Sfalse();
      return 1;
    }
    break;
  case SEwhen:
    break;
  }
  return 0;
}

atom *uniqueatom(Seff *e,int *negated) {
  switch(e->t) {
  case SEpatom:
    *negated = 0;
    return e->a;
  case SEnatom:
    *negated = 1;
    return e->a;
  default:
    break;
  }
  return NULL;
}

int whencomplements(Sfma *f,Seff *e) {
  int negative;
  atom *a;
  a = uniqueatom(e,&negative);
  if(a == NULL) return 0;
  switch(f->t) {
  case Spatom:
    if(negative && equalatom(f->a,a)) return 1;
    break;
  case Snatom:
    if(!negative && equalatom(f->a,a)) return 1;
    break;
  default:
    break;
  }    
  return 0;
}

void simplifysillyconditionals0(int i,Seff *e) {
  Sefflist *es;
  switch(e->t) {
  case SEconj:
    es = e->juncts;
    while(es != NULL) {
      simplifysillyconditionals0(i,es->hd);
      es = es->tl;
    }
    break;
  case SEwhen:	/* Go through all effects and see if they are contradicted. */
    if(!simplifysillyconditionals1(i,e,e->effect)) { /* Simplify. */
      /* If did not simplify, then check for elimination. */
      if(whencomplements(e->cond,e->effect)) {
	/* Replace condition with constant TRUE. */
	e->cond = Strue();
      }
    }
    break;
  default:
    break;
  }
}

void simplifysillyconditionals() {
  int i;
  for(i=0;i<nOfSActions;i++) {
    /* Go through all conditional effects with a non-True precondition. */
    simplifysillyconditionals0(i,Sactions[i].effect);
  }
}



/* Replace each occurrence of a schema variable with its index
   in action's the parameter list.
   Replace "forall" with "and" and "exists" with "or".
*/

void preprocessoperators() {
  int i;
  for(i=0;i<nOfSActions;i++) {
    //    printf("processing action schema %i\nprecondition\n",i);
    eliminatefmaquant(Sactions[i].precon);
    //    printf("effect\n");
    eliminateeffquant(Sactions[i].effect);
    //    printf("eliminated quantifiers\n");
    if(flagShowInput) printSaction(&Sactions[i]);
    preprocessfma(Sactions[i].params,Sactions[i].precon,1);
    preprocesseff(Sactions[i].params,Sactions[i].effect,1);
  }
  eliminatefmaquant(Sgoal);
}

/* produce list with parameter values */

intlist *bindingaslist(int *b,int bindings) {
  int i;
  intlist *l;
  l = NULL;
  for(i=bindings-1;i>=0;i--) {
    l = intcons(b[i],l);
  }
  return l;
}

/* Ground a formula */

fmalist *groundfmalist(Sfmalist *,int *);

fma *groundfma(Sfma *sf,int *b) {
  fma *f = (fma *)malloc(sizeof(fma));
  switch(sf->t) {
  case STRUE: f->t = TRUE; break;
  case SFALSE: f->t = FALSE; break;
  case Sconj:
    f->t = conj;
    f->juncts = groundfmalist(sf->juncts,b);
    break;
  case Sdisj:
    f->t = disj;
    f->juncts = groundfmalist(sf->juncts,b);
    break;
  case Seq:
    if(bvalue(sf->p1,b) == bvalue(sf->p2,b)) {
      f->t = TRUE;
    } else {
      f->t = FALSE;
    }
    break;
  case Sneq:
    if(bvalue(sf->p1,b) == bvalue(sf->p2,b)) {
      f->t = FALSE;
    } else {
      f->t = TRUE;
    }
    break;
  case Spatom:
    f->t = patom;
    f->a = atomindex(sf->a,b);
    break;
  case Snatom:
    f->t = natom;
    f->a = atomindex(sf->a,b);
    break;
  case Sforall:
  case Sforsome:
    fprintf(stderr,"INTERNAL ERROR: quantification has not been grounded\n");
    exit(1);
  }
  return f;
}

fmalist *groundfmalist(Sfmalist *l,int *b) {
  if(l == NULL) return NULL;
  return fmacons(groundfma(l->hd,b),groundfmalist(l->tl,b));
}

/* Ground an effect
Assumption: the effect is a conjunction of
conditional effects without nesting (each effect is atomic
or a conjunction of atomic effects.)
 */

intlist *locateposeffectsL(Sefflist *,int *,intlist *);
intlist *locatenegeffectsL(Sefflist *,int *,intlist *);

intlist *locateposeffects(Seff *se,int *b,intlist *ac) {
  switch(se->t) {
  case SEpatom: return intcons(atomindex(se->a,b),ac);
  case SEconj: return locateposeffectsL(se->juncts,b,ac);
  default: return ac;
  }
}

intlist *locateposeffectsL(Sefflist *l,int *b,intlist *ac) {
  if(l == NULL) return ac;
  return locateposeffects(l->hd,b,locateposeffectsL(l->tl,b,ac));
}

intlist *locatenegeffects(Seff *se,int *b,intlist *ac) {
  switch(se->t) {
  case SEnatom: return intcons(atomindex(se->a,b),ac);
  case SEconj: return locatenegeffectsL(se->juncts,b,ac);
  default: return ac;
  }
}

intlist *locatenegeffectsL(Sefflist *l,int *b,intlist *ac) {
  if(l == NULL) return ac;
  return locatenegeffects(l->hd,b,locatenegeffectsL(l->tl,b,ac));
}

eff *locateconditionals(Seff *se,int *b,eff *ac) {
  Sefflist *ses;
  eff *e;
  switch(se->t) {
  case SEconj:
    ses = se->juncts;
    while(ses != NULL) {
      ac = locateconditionals(ses->hd,b,ac);
      ses = ses->tl;
    }
    return ac;
  case SEwhen:
    e = (eff *)malloc(sizeof(eff));
    if(se->cond->t == SFALSE) return ac;
    e->condition = groundfma(se->cond,b);
    e->poseffects = locateposeffects(se->effect,b,NULL);
    e->negeffects = locatenegeffects(se->effect,b,NULL);
    e->tl = ac;
    return e;
  case SEforall:
    fprintf(stderr,"INTERNAL ERROR: forall 4546\n");
    exit(1);
  default:
    return ac;
  }
}


eff *groundeff(Seff *se,int *b) {
  eff *e = (eff *)malloc(sizeof(eff));
  e->condition = (fma *)malloc(sizeof(fma));
  e->condition->t = TRUE;
  e->poseffects = locateposeffects(se,b,NULL);
  e->negeffects = locatenegeffects(se,b,NULL);
  e->tl = locateconditionals(se,b,NULL);
  return e;
}

/* elements associated with a type */

intlist *getdomain(int type) {
  int j;
  for(j=0;j<nOfTypes;j++) {
    if(Stypes[j].typename == type) return Stypes[j].elements;
  }
  if(j == nOfTypes) {
    printf("WARNING: type %s is empty\n",symbol(type));
  }
  return NULL;
}

/* Main functions for grounding */

int binding[100];
int nOfBindings;

void gothrough(int i,int sc,intlist **domains) {
  if(i == nOfBindings) { /* base case, instantiate */
    nOfActions += 1;

    if(nOfActions >= maxActions) {
      maxActions = 2*maxActions;
      actions = (action *)realloc(actions,maxActions * sizeof(action));
      printf("Increased max. actions to %i.\n",maxActions);
    }

    actions[nOfActions-1].name = intcons(Sactions[sc].name,bindingaslist(binding,nOfBindings));
    actions[nOfActions-1].precon = groundfma(Sactions[sc].precon,binding);
    actions[nOfActions-1].effects = groundeff(Sactions[sc].effect,binding);
  } else {
    intlist *l = domains[i];
    while(l != NULL) {
      binding[i] = l->hd;
      gothrough(i+1,sc,domains);
      l = l->tl;
    }
  }
}

void groundoperators() {
  int i,j;
  intlist *domains[100];
  atomlist *al;

  initactions(); /* initialize the ground action data structure */
  initatomtable(); /* initialize the tables for atoms */

  for(i=0;i<nOfSActions;i++) {
    typedvarlist *params = Sactions[i].params;
    typedvarlist *l = params;

    if(flagShowInput) {
      printf("Grounding schema %i:%s\n",i,symbol(Sactions[i].name));
      printSaction(&Sactions[i]);
    }

    /* Fetch domains of the parameters */
    nOfBindings = 0;
    while(l != NULL) {
      if(isvar(l->v)) { /* Parameter is uninstantiated */
	domains[nOfBindings] = getdomain(l->t);
      } else { /* That parameter was already instantiated in the schema */
	domains[nOfBindings] = intcons(l->v,EMPTYLIST);
      }
      nOfBindings += 1;
      l = l->tl;
    }
    assert(nOfBindings < 100);

    /* Go through all parameter assignments and ground */

    gothrough(0,i,domains);

  }

  goal = groundfma(Sgoal,NULL);

  /* Go through the initial state description to assign
     indices to initial state atoms. */

  al = Sinit;
  while(al != NULL) {
    atomindex(al->hd,NULL);
    al = al->tl;
  }

  initialstate = (int *)malloc(sizeof(int) * nOfAtoms);
  for(i=0;i<nOfAtoms;i++) initialstate[i] = 0;

  al = Sinit;
  while(al != NULL) {
    j = atomindex(al->hd,NULL);

    assert(j>=0); assert(j<nOfAtoms);

    initialstate[j] = 1;
    al = al->tl;
  }

}
