
/*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

/* Literal and types:

 */

#define VALUE(l) (((l)&1)^1)
#define NEG(l) ((l)^1)
#define VAR(l) ((l)>>1)
#define PLIT(v) ((v)<< 1)
#define NLIT(v) (1+(PLIT(v)))
#define LIT(v) (PLIT(v))

#define UNASS -1

int propagate(satinstance,int);

void init_clausesets();
