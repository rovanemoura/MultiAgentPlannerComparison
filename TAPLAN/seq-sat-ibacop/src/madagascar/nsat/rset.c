
  /*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

#include <stdio.h>
#include <stdlib.h>

int *rset;
int MAXrset;
int Nrset;

/* Initialize data structures related to rsets. */

void rset_init() {
  MAXrset = 10000;
  rset = (int *)malloc(sizeof(int) * MAXrset);
  Nrset = 0;
}

/* Empty the rset. */

void rset_empty() {
  Nrset = 0;
}

/* Add one element to the rset. */

void rset_add(int e) {
  if(Nrset >= MAXrset) {
    MAXrset += 10000;
    rset = (int *)realloc(rset,sizeof(int) * MAXrset);
  }
  rset[Nrset] = e;
  Nrset += 1;
}

/* Randomly choose one element from the rset. */

int rset_pick() {
  if(Nrset == 0) return -1;
  return rset[random() % Nrset];
}
