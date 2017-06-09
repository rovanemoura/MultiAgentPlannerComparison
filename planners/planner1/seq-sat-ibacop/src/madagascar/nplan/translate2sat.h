
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

typedef struct _seq {
  satinstance sati;
  int value;
  int calls;
  int cost;
} seq;

seq seqs[10000];

void encoding();

typedef struct _CEstruct {
  int var;
  int disjunctive;
  fma *condition;
  struct _CEstruct *next;
} CEstruct;

CEstruct **CEs;

typedef struct _compactCEstruct {
  int var;
  int disjunctive;
  fma *condition;
} compactCEstruct;

compactCEstruct **cCEs;
