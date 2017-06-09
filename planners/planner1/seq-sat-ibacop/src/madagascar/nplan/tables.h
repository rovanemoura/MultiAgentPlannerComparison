
/*  2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au  */

/* Symboltable for IDs and VARs in lexer */

void initsymboltable();
int symbolindex(char *);
char *symbol(int);

int isvar(int);
int staticp(int);
void setnonstatic(int);

/* Symboltable for p(o1,...,on) atoms. */

int nOfAtoms;

void initatomtable();

int atomindex(atom *,int *);

int bvalue(int,int *);

int printatomi(int i);	/* Print an atom and return its length in chars. */

void renameatomtable(int,int *); /* Rename atoms by using a mapping. */
