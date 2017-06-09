
  /*   2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au   */

/* Functions for constructing a small set and then randomly
   picking one of the elements.
   If the set is empty, rset_pick will return -1. */

void rset_init();
void rset_empty();
void rset_add(int);
int rset_pick();
