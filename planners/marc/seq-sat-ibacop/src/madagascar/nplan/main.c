
  /*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

#include <stdio.h>
#include <stdlib.h>

#include "main.h"
#include "asyntax.h"
#include "intsets.h"
#include "ordintsets.h"
#include "operators.h"
#include "tables.h"
#include "staticpredicates.h"
#include "cleanup.h"
#include "invariants.h"
#include "disabling.h"
#include "../nsat/interface.h"
#include "../nsat/dimacsinput.h"
#include "../nsat/clausedb.h"
#include "../nsat/rset.h"
#include "translate2sat.h"

#include <assert.h>

#include <sys/types.h>
#include <sys/times.h>
#include <limits.h>
#include <time.h>
#include <unistd.h>
#include <signal.h>

char state_for_random[256];
char dump_for_random[256];

char stri[1000];

long TIMEstartReal,TIMEstart,TIMEpreprocess,TIMEdisabling,TIMEdisaprepro,TIMEinvariants;

long time10ms() {	/* The time spent in user mode in 100ms */
  struct tms TMS;
  times(&TMS);
  return TMS.tms_utime / (sysconf(_SC_CLK_TCK) / 100);
}

int abstimesec() {
  struct timeval aux;
  gettimeofday(&aux,NULL);
  return aux.tv_sec;
}

int abstimeusec() {
  struct timeval aux;
  gettimeofday(&aux,NULL);
  return aux.tv_usec;
}

int flagTimeout;
intset test;

void givememorystatistics() {
  char buf[30];
  snprintf(buf, 30, "/proc/%u/statm", (unsigned)getpid());
  FILE* pf = fopen(buf, "r");
  if (pf) {
    unsigned size; //       total program size
    unsigned resident;//   resident set size
    unsigned share;//      shared pages
    unsigned text;//       text (code)
    unsigned lib;//        library
    unsigned data;//       data/stack
    fscanf(pf, "%u %u %u %u %u %u", &size, &resident, &share, &text, &lib, &data);
    printf("total size %.2f MB\n",
	   /*
	   resident size: %u\n
	   shared pages : %u\n
	   text (code)  : %u\n
	   library      : %u\n
	   data/stack   : %u\n",
	   */
	   ((double)size)/256.0);
  }
  fclose(pf);
}

void timeouthandler(int i) {
  printf("\nTimeout after %i seconds.\n",flagTimeout);
  givememorystatistics();
  exit(0);
}

double time2real(long dif) {
  return ((double)dif)/100.0;
}

int numer(char c) {
  return (c >= '0' && c <= '9');
}

void processheuristic(char *decls) {
  int position,n;

  flagCEvariables = 1;

  HEURordmode = 0;
  HEURordmin = 0;
  HEURordrnd = 0;

  HEURtime = 0;
  HEURops = 0;
  HEURchoice = 0;

  HEURactions = 1;
  HEURactionchoice = 0;
  HEURactiondepthlimit = 0;

  position = 0;

  if(!numer(*decls)) {
    printf("ERROR: Expected numeric option.\n");
    exit(1);
  }

  do {

    n = 0;

    while(numer(*decls)) {
      n = n*10 + (*(decls++) - '0');
    }

    if(*decls != 0 && *decls != ':') {
      printf("ERROR: Number separator is : and not %c.\n",*decls);
      exit(1);
    }
  
    switch(position) {
    case 0: HEURordmode = n; break;
    case 1: HEURordmin = n; break;
    case 2: HEURordrnd = n; break;
    case 3: HEURtime = n; break;
    case 4: HEURops = n; break;
    case 5: HEURchoice = n; break;
    case 6: HEURactions = n; break;
    case 7: HEURactionchoice = n; break;
    case 8: HEURactiondepthlimit = n; break;
    default: printf("ERROR: too many parameters.\n"); exit(1);
    }

    position += 1;

  } while(*(decls++) == ':');

}

int main(int argc,char **argv) {
  int i,j;

  syntacticclass sclass;

  nOfInputFiles = 0;
  
  TIMEstartReal = time(NULL);
  TIMEstart = time10ms();
#include "zPRINTDATE"

  rset_init();

  /* Process command line options. */

  flagShowInput = 0;
  flag2LitClosure = 0;
  flagPDDLadddel = 1;
  flagRestartInterval = 60;
  flagTrick = 0;
  flagPreprocessing = 0;
  flagCEvariables = 1;
  flagRandomSeedTime = 0;

  currentInputFile = 0;
  outputfile = NULL;

  flagOutputDIMACS = 0;
  flagIPCplans = 0;

#ifdef VSIDS
  flagVSIDSalternate = 0;
#endif

  PLANheuristic = 0;	/* By default don't use any planning heuristic. */

#ifdef MPDOWNLOAD
  PLANheuristic = 1;
  HEURordmode = 1;
  HEURordmin = 0;
  HEURordrnd = 0;
  HEURtime = 0;
  HEURops = 0;
  HEURchoice = 0;
  HEURactions = 10;
  HEURactionchoice = 0;
  HEURactiondepthlimit = 1;
  flagCEvariables = 1;
#endif

  lastTimePoint = 500;
  outputTimeStep = 5;

  firstTimePoint = 0;
  debugOutput = 0;

  evalAlgorithm = 1;
  paramB = 0.9;		/* Default: Algorithm B */
  paramM = 18;

  planFrontend = 1;
  planSemantics = EStep;
  
  if(argc == 1) {
    printf("\n");
    printf("The command line parameters are the input file names and options:\n");
    printf("-A n   Run algorithm A with parameter n (range 1 to 20).\n");
    printf("-B r   Run algorithm B with parameter r (range 0.1 to 0.99, default -B %.2f).\n",paramB);
    printf("-M n   With algorithm B, use maximum n processes (default -M %i).\n",paramM);
    printf("-S n   Step for horizon lengths 0,n,2n,3n,... (default -S %i)\n",outputTimeStep);
    //    printf("-H n   Use SAT heuristic n.\n");
    //    printf("-pH n  Use planning heuristic n (0 = none).\n");
    printf("-F n   Starting horizon length (default -F %i)\n",firstTimePoint);
    printf("-T n   Ending horizon length (default -T %i)\n",lastTimePoint);
    printf("-t n   Timeout n seconds of real time\n");
    printf("-P n   Choose plan type n:  (default -P 2)\n");
    printf("           0 = sequential plans\n");
    printf("           1 = A-step plans (Graphplan parallelism)\n");
    printf("           2 = E-step plans (Rintanen et al. 2006)\n");
    printf("           3 = E-step plans (Ogata et al. 2004)\n");
    printf("-O     Write formulas in a file in DIMACS format instead of solving.\n");
    printf("-X     Don't use PDDL semantics for simultaneous v:=0 v:=1.\n");
    printf("-Q     Output plans in the IPC format.\n");
    printf("-o [filename]  Output plan to file.\n");
    //    printf("-W     Use time as the random seed (instead of seed 0).\n");
    //    printf("-Y     Alternate between VSIDS and planning heuristic (must activate -H as well).\n");
    //    printf("-Z n   Select preprocessing.\n");
    //    printf("-R n   Restart interval is n (default = %i).\n",flagRestartInterval);
    //    printf("-I     Show input and auxiliary data in detail.\n");
    //    printf("-2     Use the closure of binary clauses to speed up propagation.\n");
    //    printf("-d b   Level of debug output (default = 0).\n");
    exit(0);
  }

  printf("Options:");

  for(i=1;i<argc;i++) {
    if(argv[i][0] != '-') {
      printf(" file:%s",argv[i]);
      nOfInputFiles += 1;
      inputfiles[nOfInputFiles-1] = argv[i];
    } else {
      switch(argv[i][1]) {
      case 'A': /* evaluation algorithm A */
	if(!sscanf(argv[i+1],"%d",&paramA)) {
	  printf("The -A parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	evalAlgorithm = 0;
	i++;
	printf(" -A %i",paramA);
	break;
      case 'B': /* evaluation algorithm B */
	if(!sscanf(argv[i+1],"%f",&paramB)) {
	  printf("The -B parameter %s is not an real.\n",argv[i+1]);
	  exit(1);
	}
	evalAlgorithm = 1;
	i++;
	printf(" -B %.2f",paramB);
	break;
      case 'M':
	if(!sscanf(argv[i+1],"%d",&paramM)) {
	  printf("The -M parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -M %i",paramM);
	break;
      case 'H': /* Heuristic used by the solver */
	PLANheuristic = 1;
	processheuristic(argv[i+1]);
	printf(" -H %s",argv[i+1]);
	i++;
	break;
      case 'F': /* First time point to consider */
	if(!sscanf(argv[i+1],"%d",&firstTimePoint)) {
	  printf("The -F parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -F %i",firstTimePoint);
	break;
      case 'p': /* Outdated option */
	printf("option not available any more.\n");
	exit(1);
	break;
      case 'R': /* Restart interval */
	if(!sscanf(argv[i+1],"%d",&flagRestartInterval)) {
	  printf("The -R parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -R %i",flagRestartInterval);
	break;
      case 'T': /* Max time points to test */
	if(!sscanf(argv[i+1],"%d",&lastTimePoint)) {
	  printf("The -T parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -T %i",lastTimePoint);
	break;
      case 't':	/* Time out */
	if(!sscanf(argv[i+1],"%d",&flagTimeout)) {
	  printf("The -t parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -t %i",flagTimeout);
	break;
      case 'd':	/* Debug output level */
	if(!sscanf(argv[i+1],"%d",&debugOutput)) {
	  printf("The -d parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -d %i",debugOutput);
	break;
      case 'o':	/* Output file name */
	outputfile=argv[i+1];
	i++;
	printf(" -o %s",outputfile);
	break;
      case 'S': /* Step */
	if(!sscanf(argv[i+1],"%d",&outputTimeStep)) {
	  printf("The -S parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -S %i",outputTimeStep);
	break;
      case 'C': /* Trick (for testing stuff) */
	if(!sscanf(argv[i+1],"%d",&flagTrick)) {
	  printf("The -C parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -C %i",flagTrick);
	break;
      case 'X': /* Don't use PDDL semantics for v:=0 v:=1 assigning 0 first and then 1, without causing contradiction. */
	flagPDDLadddel = 0;
	printf(" -X");
	break;
      case 'W': /* Use time as random seed, instead of seed 0. */
	flagRandomSeedTime = 1;
	printf(" -W");
	break;
      case 'Z': /* Preprocessing options */
	if(!sscanf(argv[i+1],"%d",&flagPreprocessing)) {
	  printf("The -Z parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	i++;
	printf(" -Z %i",flagPreprocessing);
	break;
      case 'O': /* Output to DIMACS instead of running integrated SAT solver */
	flagOutputDIMACS = 1;
	break;
      case 'Q': /* Output to plans in IPC format */
	flagIPCplans = 1;
	break;
      case 'P': /* Parallel semantics */
	if(!sscanf(argv[i+1],"%d",&j)) {
	  printf("The -P parameter %s is not an integer.\n",argv[i+1]);
	  exit(1);
	}
	switch(j) {
	case 0: planSemantics = Sequ; break;
	case 1: planSemantics = AStep; break;
	case 2: planSemantics = EStep; break;
	case 3: planSemantics = EStepOgata; break;
	}
	i++;
	printf(" -P %i",j);
	break;
      case 'I':
	flagShowInput = 1;
	printf(" -I");
	break;
#ifdef VSIDS
      case 'Y':
	flagVSIDSalternate = 1;
	printf(" -Y");
	break;
#endif
      case 'D':	/* Input file is a CNF in DIMACS format. */
	planFrontend = 0;
	printf(" -D");
	break;
      default:
	printf("ignoring %s\n",argv[i]);
	break;
      }
    }
  }

  printf("\n");

  if(flagRandomSeedTime) srandom(abstimeusec());
  else srandom(0);

 /***********************************************************************/
 /***********************************************************************/
 /************************** Show heuristics parameter settings *********/

#define noEXPLAINSETTINGS 1

#ifdef EXPLAINSETTINGS

  switch(HEURops) {
  case 0: printf("Will consider the first (arbitrary) action only.\n"); break;
  case 1: printf("Will consider all actions available.\n"); break;
  case 2: printf("Will consider last computed only.\n"); break;
  }

 switch(HEURtime) {
  case 0: printf("Earliest"); break;
  case 1: printf("Latest"); break;
  case 2: printf("Earliest..Latest"); break;
  }
  switch(HEURchoice) {
  case 0: printf(", chosen randomly.\n"); break;
  case 1: printf(", chosen randomly (biased).\n"); break;
  default: printf(", chosen according to weight.\n");
  }

  printf("Compute %i suggested actions.\n",HEURactions);
  switch(HEURactionchoice) {
  case 0:
    printf("Choose between them randomly.\n"); break;
  case 1:
    printf("Choose suggested action of earliest time point.\n"); break;
  }

  if(HEURactiondepthlimit) printf("For multiple suggested, only go deeper.\n");
  else printf("For multiple suggested, consider all.\n");
#endif

 if(flagTimeout) {
   alarm(flagTimeout);
   signal(SIGALRM,timeouthandler);
 }

/* ******************************************************************** */
/* ******************************************************************** */
/* ******************************************************************** */

#ifndef VSIDS
 if(PLANheuristic == 0) {
   fprintf(stderr,"ERROR: Trying to use VSIDS which was not compiled.\n");
   exit(1);
 }
 printf("Special no-VSIDS version.\n");
#endif


 if(planFrontend != 1) {
   solve(DIMACSinput());
 } else {
   readfile();
   printf("Domain: %s\nProblem: %s\n",domainname(),problemname());
   preprocessoperators();
   eliminatestatic();
   //   simplifysillyconditionals();
   groundoperators();
  
   printf("Parser: %i ground actions and %i state variables\n",nOfActions,nOfAtoms);

   if(flagShowInput) {
     for(i=0;i<nOfActions;i++) printaction(i);
   }

   if(flagShowInput) {
     printf("All atoms: ");
     for(i=0;i<nOfAtoms;i++) {
       printf(" %i:",i);
       printatomi(i); fflush(stdout);
     }
     printf("\n");
   }

  cleanupoperators();

  TIMEinvariants = time10ms();
  computeinvariants();
  printf(" %.2f secs\n", time2real(time10ms() - TIMEinvariants));

  simplifyoperatorsstatic();

  eliminatestaticvariables();

  //  mergecontras();

  sortactions();

  if(flagShowInput) {
    for(i=0;i<nOfActions;i++) printaction(i);
  }

  /* Is the problem instance STRIPS, conjunctive PDDL, or general PDDL? */
  sclass = goalclass();
  for(i=0;i<nOfActions;i++) {
    if(sclass == STRIPS && actionclass(i) == Conjunctive) sclass = Conjunctive;
    if(actionclass(i) == GeneralPDDL) {
      sclass = GeneralPDDL;
      break;
    }
  }

  switch(sclass) {
  case STRIPS: printf("Actions: STRIPS\n"); break;
  case Conjunctive: printf("Actions: conjunctive\n"); break;
  case GeneralPDDL: printf("Actions: general\n"); break;
  }

#ifdef ASDASDF
  if(sclass == GeneralPDDL && PLANheuristic != 0) {
    if(goalclass() == GeneralPDDL) {
      fprintf(stderr,"ERROR: Goal not conjunctive, heuristic not defined.\n");
      exit(1);
    }
    for(i=0;i<nOfActions;i++) {
      //      if(!STRIPSaction(i)) {
      //	fprintf(stderr,"ERROR: Action %i not STRIPS, heuristic not defined.\n",i);
      if(!CONJUNCTIVEaction(i)) {
	fprintf(stderr,"ERROR: Action %i not conjunctive, heuristic not defined.\n",i);
	printaction(i);
	exit(1);
      }
    }
  }
#endif

  TIMEdisaprepro = time10ms();
  findoccurrences();
  if(debugOutput > 0) printf("time %.2f for preprocessing for disabling graph\n", time2real(time10ms() - TIMEdisaprepro));

  if(planSemantics == EStep) {
    printf("Disabling graph: ");
    TIMEdisabling = time10ms();
    NEWNEWscc(nOfActions);
    printf(" %.2f secs\n", time2real(time10ms() - TIMEdisabling));
  }
  

  TIMEpreprocess = time10ms();

  encoding();

  printf("total time %.2f preprocess %.2f \n",
	 time2real(time10ms() - TIMEstart),
	 time2real(TIMEpreprocess - TIMEstart));

  givememorystatistics();

  if(flagOutputDIMACS == 0) {
    printf("t val conflicts decisions\n");
    i = 0;
    do {
      printf("%i %i %i %i\n",seqs[i].sati->nOfTPoints-1,seqs[i].sati->value,seqs[i].sati->conflicts,seqs[i].sati->decisions);
      i += 1;
    } while(i*outputTimeStep+firstTimePoint <= lastTimePoint && seqs[i-1].sati->value != 1);
      //    } while(i*outputTimeStep+firstTimePoint < lastTimePoint);
  }

 }

 return 0;
}
