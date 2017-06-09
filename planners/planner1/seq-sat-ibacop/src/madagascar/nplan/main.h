
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

int SATheuristic;
int PLANheuristic;
int planFrontend;
int flagShowInput;
int flag2LitClosure;
int flagRestartInterval;
int debugOutput;
int flagPDDLadddel;
int flagTrick;
int flagPreprocessing;
int flagIPCplans;
int flagVSIDSalternate;
int flagCEvariables;	/* Create a variable for each conditional effect. */
int flagRandomSeedTime; /* Use the time as a random seed (instead of 0). */

typedef enum { Sequ, EStep, EStepOgata, AStep } semantics;

semantics planSemantics;

int currentInputFile;
int nOfInputFiles;
char *inputfiles[10];
char *outputfile;

int flagOutputDIMACS;

int firstTimePoint;
int lastTimePoint;
int outputTimeStep;


int evalAlgorithm;	/* 0 = A, 1 = B */
int paramA;
float paramB;
int paramM; /* Max. processes for algorithm B. */

/* Heuristics */

int HEURordmode; /* 0 = earliest, 1 = latest, 2 = difference */
int HEURordmin; /* 0 = smaller is better, 1 = bigger is better */
int HEURordrnd; /* 1 = randomly shuffle (to break ties) */
int HEURtime; /* 0 = earliest, 1 = latest, 2 = all */
int HEURops; /* 0 = first, 1 = all */
int HEURchoice; /* 0 = random, 1 = weight */
int HEURactions; /* How many suggested actions found? */
int HEURactionchoice; /* choose action 0 = randomly, 1 = minimal time stamp */
int HEURactiondepthlimit;
