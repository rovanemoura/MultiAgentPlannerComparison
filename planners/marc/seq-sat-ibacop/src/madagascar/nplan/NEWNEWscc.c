
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>
#include <malloc.h>
#include <assert.h>

#include "asyntax.h"
#include "ordintsets.h"
#include "operators.h"

#include "main.h"

#include "NEWNEWscc.h"

/**********************************************************************/
/*  Strong components of a graph                                      */
/**********************************************************************/

/* Compute strong components. There is a function children defined
   elsewhere that gives the list of children of a node.
   The starting node is node number 0. The set of all nodes is
   {0,...,nOfNodes-1}. */

#define noDEBUG

int *NNdfnumber;
int *NNstack;
short *NNinstack;
int *NNlowlink;

int NNptr,NNdf;
int NNsingletons;

int nodecounter;

#define min(a,b) (((a)<(b))?(a):(b))

int getnext(int *el,eff **efs,intlist **pe,intlist **ne,int **as1,int **as2) {

 getfromas:

  if(**as1 != -1) {
    *el = **as1;
    (*as1)++;
    return 1;
  }

  if(**as2 != -1) {
    *el = **as2;
    (*as2)++;
    return 1;
  }

 getfrompene:

  if(*pe != NULL) {
    *as1 = ApreconoccN[(*pe)->hd];
    *as2 = Acondocc[(*pe)->hd];
    *pe = (*pe)->tl;
    goto getfromas;
  }

  if(*ne != NULL) {
    *as1 = ApreconoccP[(*ne)->hd];
    *as2 = Acondocc[(*ne)->hd];
    *ne = (*ne)->tl;
    goto getfromas;
  }

  if(*efs != NULL) {
    *pe = (*efs)->poseffects;
    *ne = (*efs)->negeffects;
    *efs = (*efs)->tl;
    goto getfrompene;
  }

  return 0;
}

sccs NNtraverse(int i,sccs ac) {
  int j,current;

  eff *efs;
  intlist *pe,*ne;
  int *as1,*as2;
  int dummy;
  int somethingleft,el;

  nodecounter += 1;
  if((nodecounter&1023) == 1023) { printf("."); fflush(stdout); }

  NNdfnumber[i] = ++NNdf;
  NNlowlink[i] = NNdf;

  NNstack[++NNptr] = i;
  NNinstack[i] = 1;

  current = NNptr;

  efs = actions[i].effects;
  pe = NULL;
  ne = NULL;

  dummy = -1;
  as1 = &dummy;
  as2 = &dummy;

  somethingleft = getnext(&el,&efs,&pe,&ne,&as1,&as2);

  /* Go through all neighbor CANDIDATES. */

  while(somethingleft) {
    
    //     printf("Neighbor %i of %i\n",el,i);
    if(NNdfnumber[el] == -1 && parallel(i,el)) {
      ac = NNtraverse(el,ac);
      NNlowlink[i] = min(NNlowlink[i],NNlowlink[el]);
    } else {
      if(NNinstack[el] == 1 && NNlowlink[i] > NNdfnumber[el] && parallel(i,el)) {
	  NNlowlink[i] = NNdfnumber[el];
	}
    }

    somethingleft = getnext(&el,&efs,&pe,&ne,&as1,&as2);

  }

  if(NNlowlink[i] == NNdfnumber[i]) { /* Found an SCC */
    sccs oldac;
    oldac = ac;
    ac = (sccs)malloc(sizeof(struct _scc));
    ac->NofEls = NNptr-current+1;
    ac->next = oldac;
    ac->els = (int *)malloc(ac->NofEls * sizeof(int));

    if(NNptr == current) NNsingletons += 1;

    for(j=current;j<=NNptr;j++) ac->els[j-current] = NNstack[j];

    if(flagShowInput) {
      printf("new SCC %i:",ac->NofEls);
      for(j=current;j<=NNptr;j++) printactionname(NNstack[j]); // printf(" %i",NNstack[j]);
      printf("\n");
    } else {
      if(debugOutput > 0 && NNptr != current) printf(" %i",NNptr-current+1);
    }
    
    for(j=current;j<=NNptr;j++) NNinstack[NNstack[j]] = 0;
    NNptr = current-1;
  }
  return ac;
}
    
void NEWNEWscc(int nOfNodes) {
  int i;
  sccs ac;

  nodecounter = 0;

  NNdf = -1;
  NNptr = -1;

  if(debugOutput > 0) printf("Finding SCCs for %i nodes.\n",nOfNodes);

  NNdfnumber = (int *)malloc(nOfNodes * sizeof(int));
  NNstack = (int *)malloc(nOfNodes * sizeof(int));
  NNinstack = (short *)malloc(nOfNodes * sizeof(short));
  NNlowlink = (int *)malloc(nOfNodes * sizeof(int));

  NNsingletons = 0;

  for(i=0;i<nOfNodes;i++) {
    NNdfnumber[i] = -1;
    NNinstack[i] = 0;
  }

  ac = NULL;

  for(i=0;i<nOfNodes;i++)
    if(NNdfnumber[i] == -1) {
      ac = NNtraverse(i,ac);
    }

  if(debugOutput > 0) printf(", %i singleton components\n",NNsingletons);

  SCCS = ac;

  free(NNdfnumber);
  free(NNstack);
  free(NNinstack);
  free(NNlowlink);
}
