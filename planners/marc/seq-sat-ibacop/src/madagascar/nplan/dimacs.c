
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

#include <stdio.h>

void dimacsheader(FILE *f,int Nvars,int Nclauses) {
  fprintf(f,"p cnf %i %i\n",Nvars,Nclauses);
}
