
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

/* Representation of literals:
   bit 0 is 1 if literal is negative
   bit 1 and up contain the index of the variable
 */

#define NEG(a) ((a)^1)
#define PLIT(a) ((a) << 1)
#define LIT(a) (PLIT(a))
#define NLIT(a) (NEG(LIT(a)))
#define VAR(l) ((l) >> 1)

int *onelits;
intset *twolits;

void computeinvariants();
