
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

typedef struct _scc {
  int NofEls;
  int *els;
  struct _scc *next;
} *sccs;

void NEWNEWscc(int);

sccs SCCS;
