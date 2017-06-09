
  /*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

#include <stdio.h>
#include <assert.h>

#include "interface.h"
#include "../nplan/main.h"
#include "../nplan/asyntax.h"
#include "../nplan/tables.h"
#include "../nplan/ordintsets.h"
#include "../nplan/operators.h"

#define FIELD 35

/* Print a tabular representation of the values of state variables and
   operators at different time points. */

void printplanT(satinstance sati) {
  int i,j,len;
  for(j=0;j<FIELD;j++) printf("_");
  for(j=0;j<sati->nOfTPoints;j++) printf("%i",j%10);
  printf("\n");
  for(i=0;i<nOfAtoms;i++) {
    len = printatomi(i);
    for(j=0;j<FIELD-len;j++) printf("_");
    for(j=0;j<sati->nOfTPoints;j++) {
      switch(sati->vars[j*sati->nOfVarsPerTime+i].val) {
      case 0: printf("."); break;
      case 1: printf("T"); break;
      default: printf(" "); break;
      }
    }
    printf("\n");
  }

  for(i=0;i<nOfActions;i++) {
    len = printactionname(i);
    for(j=0;j<FIELD-len;j++) printf("_");
    for(j=0;j<sati->nOfTPoints-1;j++) {
      switch(sati->vars[j*sati->nOfVarsPerTime+i+nOfAtoms].val) {
      case 0: printf("."); break;
      case 1: printf("T"); break;
      default: printf(" "); break;
      }
    }
    printf("\n");
  }
}

/* Print a tabular representation of the values of state variables
   at different time points. */

void printplanV(satinstance sati) {
  int i,j,len;
  for(j=0;j<FIELD;j++) printf("_");
  for(j=0;j<sati->nOfTPoints;j++) printf("%i",j%10);
  printf("\n");
  for(i=0;i<nOfAtoms;i++) {
    len = printatomi(i);
    for(j=0;j<FIELD-len;j++) printf("_");
    for(j=0;j<sati->nOfTPoints;j++) {
      switch(sati->vars[j*sati->nOfVarsPerTime+i].val) {
      case 0: printf("."); break;
      case 1: printf("T"); break;
      default: printf(" "); break;
      }
    }
    printf("\n");
  }
}

/* Print the plan as a sequence of operators.

(Test also that the plan actually is a solution to the planning problem.)

*/

void copystate(satinstance sati,int *s1,int *s2) {
  int i;
  for(i=0;i<sati->nOfSVars;i++) s2[i] = s1[i];
}

#define MAXSTEPSIZE 50000

void fprintplan(FILE *f,satinstance sati) {
  int pactions[MAXSTEPSIZE];
  int sactions[MAXSTEPSIZE];
  int substepcnt;
  int pcnt,toprint,round,naf,cnt;
  int t,i,j,actions;
  int state0[sati->nOfSVars],state1[sati->nOfSVars];
  int print_t;
#ifdef IPC
  int print_a;
#endif

  actions = 0;
  print_t = 0;
#ifdef IPC
  print_a = 0;
#endif

  for(i=0;i<sati->nOfSVars;i++) state0[i] = sati->vars[i].val;

  for(t=0;t<sati->nOfTPoints-1;t++) {

    /* Get action indices to pactions. */

    pcnt = 0; /* How many actions in the current time point. */

#ifdef DEBUG
    fprintf(f,"Actions at STEP %i: ",t);
#endif

    for(i=0;i<nOfActions;i++) {
      if(sati->vars[i+sati->nOfSVars+t*sati->nOfVarsPerTime].val == 1) {
	pactions[pcnt++] = i;
	if(pcnt >= MAXSTEPSIZE) {
	  fprintf(stderr,"ERROR: max. step size %i exceeded. Recompile with increased MAXSTEPSIZE.\n",MAXSTEPSIZE);
	}
#ifdef DEBUG
	fprintactionname(f,i);
#endif
	actions += 1;
      }
    }

#ifdef DEBUG
    printf("\n");
#endif

    toprint = pcnt;
    round = 0;

    /* Find actions that don't affect any of the remaining actions. */

    while(toprint) {

      copystate(sati,state0,state1);

      cnt = 0;

      substepcnt = 0; /* Number of actions currently in the substep */

      for(i=0;i<pcnt;i++) {
	if(pactions[i] == -1) continue;
	naf = 0;

	for(j=0;j<pcnt;j++) {
	  if(j==i) continue;
	  if(pactions[j] == -1) continue;

	  if(opaffectsinstate(state1,pactions[i],pactions[j])) {
#ifdef DEBUG
	    printactionname(pactions[i]);
	    printf(" %i affects %i ",pactions[i],pactions[j]);
	    printactionname(pactions[j]);
	    printf("\n");
#endif
	    naf = 1;
	    break;
	  } else {
#ifdef DEBUG
	    printactionname(pactions[i]);
	    printf(" %i does NOT affect %i ",pactions[i],pactions[j]);
	    printactionname(pactions[j]);
	    printf("\n");
#endif
	  }
	  
	}

	if(!naf) {
	  toprint -= 1;
	  cnt += 1;
	  sactions[substepcnt++] = i;
	}
      }

      /* Print the current substep. */

      if(!flagIPCplans) {
	if(toprint == 0 && round == 0) {
	  fprintf(f,"STEP %i:",t);
	} else {
	  fprintf(f,"STEP %i.%i:",t,round);
	}
      }

      for(j=0;j<substepcnt;j++) {

	if(flagIPCplans) {
#ifdef IPC
	  fprintf(f,"%d : ",print_a++);
#else
	  fprintf(f,"%d : ",print_t);
#endif
	  fprintactionnameIPC(f,pactions[sactions[j]]);
	  fprintf(f,"\n");
	} else {
	  fprintf(f," ");
	  fprintactionname(f,pactions[sactions[j]]);
	}

	if(!execute(pactions[sactions[j]],state0,state1)) {
	  fprintf(stderr,"ERROR: plan not executable!\n");
	  exit(1);
	}

	pactions[sactions[j]] = -1;
	
      }

      if(cnt == 0) {
	fprintf(stderr,"ERROR: no actions in current round (%i)!\n",pcnt);
	exit(1);
      }

      copystate(sati,state1,state0);

      print_t += 1;

      if(!flagIPCplans) {
	fprintf(f,"\n");
      }

      round += 1;
    }

  }

  /* Test that the goal is indeed true in the resulting final state. */
  if(!ptruep(goal,state1)) {
    printfma(goal);
    printf("ERROR: goal not reached by the plan.\n");
    exit(1);
  }

  printf("%i actions in the plan.\n",actions);
}

/* Print a literal in the textual form. */

void printUvar(satinstance sati,int i) {
  int t;
  if(i < nOfAtoms) {
    printatomi(i);
  } else if(i < nOfAtoms+nOfActions) {
    printactionname(i-nOfAtoms);
  } else {
    printf("AUX%i",i-nOfAtoms-nOfActions);
  }
}

void printTlit(satinstance sati,int i) {
  int t;
  if(i&1) printf("-");
  i = i >> 1;
  if(planFrontend) {
    t = i/sati->nOfVarsPerTime;
    i = i-t*sati->nOfVarsPerTime;
    printUvar(sati,i);
    printf("[%i]",t);
  } else {
    printf("%i",i+1);
  }
}

