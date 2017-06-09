#ifndef lint
static const char yysccsid[] = "@(#)yaccpar	1.9 (Berkeley) 02/21/93";
#endif

#include <stdlib.h>
#include <string.h>

#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYPATCH 20070509

#define YYEMPTY (-1)
#define yyclearin    (yychar = YYEMPTY)
#define yyerrok      (yyerrflag = 0)
#define YYRECOVERING (yyerrflag != 0)

extern int yyparse(void);

static int yygrowstack(void);
#define YYPREFIX "yy"
#line 5 "parser.y"

#include "stdio.h"
#include "main.h"
#include "asyntax.h"

void rparen(char *);

#line 14 "parser.y"
typedef union {
  int i;
  intlist *intlistp;
  atomlist *atomlistp;
  atom *atomp;
  Sfma *Sfmap;
  Sfmalist *Sfmalistp;
  Seff *Seffp;
  Sefflist *Sefflistp;
  typedvarlist *typedvarlistp;
} YYSTYPE;
#line 43 "y.tab.c"
#define RPAREN 257
#define LPAREN 258
#define DASH 259
#define rwDEFINE 260
#define rwACTION 261
#define rwPARAMS 262
#define rwEFFECT 263
#define rwPRECOND 264
#define rwPREDICATES 265
#define rwREQUIREMENTS 266
#define rwTYPES 267
#define rwOBJECTS 268
#define rwINIT 269
#define rwGOAL 270
#define rwDOMAIN 271
#define rwTYPING 272
#define rwAND 273
#define rwOR 274
#define rwWHEN 275
#define rwNOT 276
#define rwIMPLY 277
#define rwFORALL 278
#define rwPROBLEM 279
#define EQUA 280
#define rwEXISTS 281
#define rwLENGTH 282
#define rwCONSTANTS 283
#define rwEITHER 284
#define rwINCREASE 285
#define rwMETRIC 286
#define rwMINIMIZE 287
#define ID 288
#define VAR 289
#define INT 290
#define rwFUNCTIONS 291
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    1,    1,    2,    2,   20,   20,   14,   29,   15,
   23,   23,   23,   24,   24,   22,   22,   30,   31,    3,
    4,    4,   32,    5,   33,    5,   34,    5,   36,    5,
   37,    5,   38,    5,    7,    7,   39,    6,    6,    6,
   25,   25,   25,   21,   21,   27,   27,   26,   40,   41,
    8,   42,    9,   43,    9,   10,   10,   10,   10,   10,
   28,   28,   11,   44,   11,   11,   11,   45,   12,   12,
   46,   35,   35,   47,   16,   48,   16,   49,   16,   50,
   16,   51,   16,   52,   16,   16,   53,   16,   54,   16,
   55,   16,   17,   17,   19,   19,   13,   13,   56,   18,
   57,   18,   58,   18,   59,   18,   18,   60,   18,   61,
   18,
};
short yylen[] = {                                         2,
    1,    2,    0,    1,    1,    2,    0,    5,    0,    5,
    2,    2,    0,    1,    1,    2,    0,    0,    0,   10,
    2,    0,    0,    5,    0,    5,    0,    5,    0,    5,
    0,    5,    0,    6,    2,    1,    0,    5,    2,    2,
    1,    1,    0,    2,    1,    2,    1,    3,    0,    0,
   10,    0,    5,    0,    4,    2,    2,    2,    2,    3,
    4,    1,    4,    0,    9,    2,    0,    0,    6,    0,
    0,    8,    0,    0,    4,    0,    5,    0,    6,    0,
    5,    0,    6,    0,    5,    1,    0,    6,    0,    8,
    0,    8,    2,    1,    2,    1,    1,    1,    0,    4,
    0,    5,    0,    6,    0,    8,    1,    0,    5,    0,
    6,
};
short yydefred[] = {                                      0,
    0,    0,    1,    0,    4,    5,    0,    2,    0,    0,
    0,   18,   49,    0,    0,   22,    0,    0,    0,    0,
    0,   21,    0,    0,   13,    0,    0,    0,   54,    0,
    0,    0,   70,    0,    0,    0,    0,   20,    0,    0,
   57,    0,    0,   86,   59,   56,   13,    0,   52,   51,
    0,    0,   25,   31,   27,    0,   29,    6,    0,    0,
   12,   11,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   55,    0,    0,    0,    0,    0,   33,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   76,
    0,   80,    0,   84,    0,    0,   15,   14,    0,    0,
    9,    0,   53,    0,    0,  107,   40,   39,   35,    0,
    0,   24,   26,   32,   28,    0,   71,   30,   61,    0,
   98,    0,   97,   93,    0,   75,    0,   78,    0,   82,
    0,    0,    0,    0,   42,   87,    0,    0,   16,   37,
    0,    0,    0,    0,    0,   34,   68,    0,   66,    0,
    0,   77,   81,    0,   85,    0,   44,    0,    0,    0,
   46,    0,    0,   10,    0,    0,  101,    0,    0,  108,
    0,    0,    0,    0,    0,    0,    8,   79,   83,   48,
   89,   88,   91,   38,   95,    0,  100,  103,    0,    0,
  110,   69,    0,   63,    0,    0,    0,  102,    0,  109,
    0,    0,    0,    0,   90,   92,  104,  105,  111,   64,
   72,    0,    0,  106,    0,   65,
};
short yydgoto[] = {                                       2,
    3,    4,    5,   18,   22,   78,   79,    6,   20,   29,
  117,   52,  122,   61,   44,   89,   90,  166,  167,   40,
  132,  101,   42,  102,  133,  134,  135,   41,  138,   14,
   23,   81,   82,   84,   57,   86,   83,  110,  165,   15,
   31,   74,   48,  213,  173,  150,   91,  125,  154,  127,
  156,  129,  162,  196,  197,  168,  186,  199,  212,  189,
  202,
};
short yysindex[] = {                                   -235,
 -204,    0,    0, -235,    0,    0, -190,    0, -255, -218,
 -216,    0,    0, -183, -182,    0, -180, -179, -242, -174,
 -246,    0, -181, -202,    0, -171, -200, -198,    0, -242,
 -167, -197,    0, -202, -202, -202, -165,    0, -202, -163,
    0, -164, -215,    0,    0,    0,    0, -159,    0,    0,
 -213, -157,    0,    0,    0, -189,    0,    0, -185, -263,
    0,    0, -171, -171, -171, -171, -171, -153, -234, -152,
 -234, -164,    0, -149, -148, -147, -171, -213,    0, -176,
 -144, -143, -142, -141, -172, -138, -202, -244, -171,    0,
 -137,    0, -171,    0, -171, -168,    0,    0, -234, -168,
    0, -234,    0, -168, -193,    0,    0,    0,    0, -135,
 -172,    0,    0,    0,    0, -250,    0,    0,    0, -162,
    0, -244,    0,    0, -134,    0, -133,    0, -130,    0,
 -168, -150, -127, -168,    0,    0, -125, -124,    0,    0,
 -147, -171, -122, -121, -122,    0,    0, -240,    0, -123,
 -119,    0,    0, -117,    0, -116,    0, -146, -171, -150,
    0, -114, -171,    0, -113, -147,    0, -112, -147,    0,
 -168, -244, -111, -136, -172, -110,    0,    0,    0,    0,
    0,    0,    0,    0,    0, -107,    0,    0, -106, -105,
    0,    0, -132,    0, -131, -104, -103,    0, -102,    0,
 -147,  -99, -202, -165,    0,    0,    0,    0,    0,    0,
    0,  -98,  -96,    0, -172,    0,
};
short yyrindex[] = {                                    147,
    0,    0,    0,  147,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  -95,    0, -167,
    0,    0,    0, -225,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  -93, -225, -225, -159,    0, -225,  -92,
    0,  -91,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  -90,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  -89,    0,    0,    0,    0,    0,    0,    0,
 -182,  -87,    0,    0,    0,    0,    0,  -86,    0,    0,
    0,    0,    0,    0,  -85,    0, -225,    0,  -84,    0,
    0,    0,    0,    0,    0,  -83,    0,    0,    0,  -83,
    0, -182,    0,  -83,    0,    0,    0,    0,    0,    0,
  -85,    0,    0,    0,    0,  -85,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
 -219,  -82,    0,  -81,    0,    0,    0,    0,    0,    0,
  -80,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  -79,    0,    0,    0,    0,
  -83,    0,    0,    0,  -85,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  -93, -159,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  -85,    0,
};
short yygindex[] = {                                      0,
  159,    0,    0,    0,    0,    0,  101,    0,    0,  150,
 -108,    0, -115,    0,  -41,  -24,  -53,  -72,   15,  -34,
 -101,   80,  136,  -47,  -94,    0,   50,  -23,    0,    0,
    0,    0,    0,    0,  -19,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,
};
#define YYTABLESIZE 185
short yytable[] = {                                      53,
   62,   45,  147,  107,   58,  137,  151,  149,  148,  140,
   92,   54,   55,  120,   32,   10,   88,  174,   33,   34,
   35,   99,    1,   11,   71,   24,   25,   26,   27,  157,
   62,    7,  160,    7,  106,  124,   36,   45,  116,   45,
   93,   94,   95,   28,   37,  121,  123,  175,   75,   76,
   77,  136,  108,   97,   98,    7,  191,   63,   64,   65,
   66,   67,   68,  119,   69,   70,  194,    9,  128,   12,
  130,   13,   71,   16,   17,   38,  190,   19,   21,  141,
  123,  142,  143,   30,  144,   39,   43,   46,   47,   50,
   51,  145,   56,   60,   71,   59,  188,   73,   85,  106,
   80,  170,   87,  172,   96,  100,  216,  103,  158,  104,
  105,  111,  112,  113,  114,  115,  116,  169,  118,  126,
  131,  146,  152,  153,  106,   71,  155,  106,  208,  159,
  123,  163,  164,  176,  181,  120,  171,  177,  183,  178,
  179,  180,  182,  184,  187,  192,    3,  193,  195,  198,
  200,  201,  205,  206,  207,  203,  204,  209,  214,  106,
  215,   19,    8,    7,   62,   58,   23,   74,  210,   60,
   36,   67,   94,   43,   41,   47,   99,   96,  109,   49,
  185,  139,   72,  161,  211,
};
short yycheck[] = {                                      34,
   42,   26,  111,   76,   39,  100,  122,  116,  259,  104,
   64,   35,   36,  258,  261,  271,  280,  258,  265,  266,
  267,   69,  258,  279,  288,  268,  269,  270,  271,  131,
   72,  257,  134,  259,   76,   89,  283,  257,  289,  259,
   65,   66,   67,  286,  291,  290,   88,  288,  262,  263,
  264,   99,   77,  288,  289,  260,  172,  273,  274,  275,
  276,  277,  278,   87,  280,  281,  175,  258,   93,  288,
   95,  288,  288,  257,  257,  257,  171,  258,  258,  273,
  122,  275,  276,  258,  278,  288,  258,  288,  287,  257,
  288,  285,  258,  258,  288,  259,  169,  257,  288,  141,
  258,  143,  288,  145,  258,  258,  215,  257,  259,  258,
  258,  288,  257,  257,  257,  257,  289,  142,  257,  257,
  289,  257,  257,  257,  166,  288,  257,  169,  201,  257,
  172,  257,  257,  257,  159,  258,  258,  257,  163,  257,
  257,  288,  257,  257,  257,  257,    0,  284,  259,  257,
  257,  257,  257,  257,  257,  288,  288,  257,  257,  201,
  257,  257,    4,  257,  257,  257,  257,  257,  203,  257,
  257,  257,  257,  257,  257,  257,  257,  257,   78,   30,
  166,  102,   47,  134,  204,
};
#define YYFINAL 2
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 291
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"RPAREN","LPAREN","DASH",
"rwDEFINE","rwACTION","rwPARAMS","rwEFFECT","rwPRECOND","rwPREDICATES",
"rwREQUIREMENTS","rwTYPES","rwOBJECTS","rwINIT","rwGOAL","rwDOMAIN","rwTYPING",
"rwAND","rwOR","rwWHEN","rwNOT","rwIMPLY","rwFORALL","rwPROBLEM","EQUA",
"rwEXISTS","rwLENGTH","rwCONSTANTS","rwEITHER","rwINCREASE","rwMETRIC",
"rwMINIMIZE","ID","VAR","INT","rwFUNCTIONS",
};
char *yyrule[] = {
"$accept : begin",
"begin : entrys",
"entrys : entry entrys",
"entrys :",
"entry : domain",
"entry : problem",
"idlist : ID idlist",
"idlist :",
"costexpr : LPAREN EQUA numexpr numexpr RPAREN",
"$$1 :",
"atom : LPAREN ID varidlist $$1 RPAREN",
"atomlist : atomlist atom",
"atomlist : atomlist costexpr",
"atomlist :",
"varid : VAR",
"varid : ID",
"varidlist : varid varidlist",
"varidlist :",
"$$2 :",
"$$3 :",
"domain : LPAREN rwDEFINE LPAREN rwDOMAIN ID $$2 RPAREN domaindefs $$3 RPAREN",
"domaindefs : domaindefs domaindef",
"domaindefs :",
"$$4 :",
"domaindef : LPAREN rwPREDICATES typedatoms $$4 RPAREN",
"$$5 :",
"domaindef : LPAREN rwREQUIREMENTS idlist $$5 RPAREN",
"$$6 :",
"domaindef : LPAREN rwCONSTANTS objectlist $$6 RPAREN",
"$$7 :",
"domaindef : LPAREN rwFUNCTIONS functiondecls $$7 RPAREN",
"$$8 :",
"domaindef : LPAREN rwTYPES objectlist $$8 RPAREN",
"$$9 :",
"domaindef : LPAREN rwACTION ID actdefs $$9 RPAREN",
"actdefs : actdef actdefs",
"actdefs : actdef",
"$$10 :",
"actdef : rwPARAMS LPAREN opvars $$10 RPAREN",
"actdef : rwPRECOND fma",
"actdef : rwEFFECT effect",
"opvars : varlist",
"opvars : opvarlist",
"opvars :",
"varlist : VAR varlist",
"varlist : VAR",
"opvarlist : opvar opvarlist",
"opvarlist : opvar",
"opvar : varlist DASH ID",
"$$11 :",
"$$12 :",
"problem : LPAREN rwDEFINE LPAREN rwPROBLEM ID $$11 RPAREN defs $$12 RPAREN",
"$$13 :",
"defs : defs LPAREN def $$13 RPAREN",
"$$14 :",
"defs : LPAREN def $$14 RPAREN",
"def : rwDOMAIN ID",
"def : rwOBJECTS objectlist",
"def : rwINIT atomlist",
"def : rwGOAL fma",
"def : rwMETRIC rwMINIMIZE atomlist",
"objectlist : idlist DASH ID objectlist",
"objectlist : idlist",
"typedvarlist : VAR DASH ID typedvarlist",
"$$15 :",
"typedvarlist : VAR DASH LPAREN rwEITHER ID idlist $$15 RPAREN typedvarlist",
"typedvarlist : VAR typedvarlist",
"typedvarlist :",
"$$16 :",
"typedatoms : typedatoms LPAREN ID typedvarlist $$16 RPAREN",
"typedatoms :",
"$$17 :",
"functiondecls : LPAREN ID typedvarlist $$17 RPAREN DASH ID functiondecls",
"functiondecls :",
"$$18 :",
"fma : LPAREN rwAND $$18 RPAREN",
"$$19 :",
"fma : LPAREN rwAND fmas $$19 RPAREN",
"$$20 :",
"fma : LPAREN rwWHEN fma fma $$20 RPAREN",
"$$21 :",
"fma : LPAREN rwOR fmas $$21 RPAREN",
"$$22 :",
"fma : LPAREN rwIMPLY fma fma $$22 RPAREN",
"$$23 :",
"fma : LPAREN rwNOT fma $$23 RPAREN",
"fma : atom",
"$$24 :",
"fma : LPAREN EQUA varid varid $$24 RPAREN",
"$$25 :",
"fma : LPAREN rwFORALL LPAREN opvars RPAREN fma $$25 RPAREN",
"$$26 :",
"fma : LPAREN rwEXISTS LPAREN opvars RPAREN fma $$26 RPAREN",
"fmas : fma fmas",
"fmas : fma",
"effects : effect effects",
"effects : effect",
"numexpr : atom",
"numexpr : INT",
"$$27 :",
"effect : LPAREN rwAND $$27 RPAREN",
"$$28 :",
"effect : LPAREN rwAND effects $$28 RPAREN",
"$$29 :",
"effect : LPAREN rwWHEN fma effect $$29 RPAREN",
"$$30 :",
"effect : LPAREN rwFORALL LPAREN opvars RPAREN effect $$30 RPAREN",
"effect : atom",
"$$31 :",
"effect : LPAREN rwNOT atom $$31 RPAREN",
"$$32 :",
"effect : LPAREN rwINCREASE atom numexpr $$32 RPAREN",
};
#endif
#if YYDEBUG
#include <stdio.h>
#endif

/* define the initial stack-sizes */
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH  YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 10000
#define YYMAXDEPTH  10000
#endif
#endif

#define YYINITSTACKSIZE 500

int      yydebug;
int      yynerrs;
int      yyerrflag;
int      yychar;
short   *yyssp;
YYSTYPE *yyvsp;
YYSTYPE  yyval;
YYSTYPE  yylval;

/* variables for the parser stack */
static short   *yyss;
static short   *yysslim;
static YYSTYPE *yyvs;
static int      yystacksize;
#line 197 "parser.y"

void parseerror(char *s) {
  errorstring = s;
}

void rparen(char *s) {
  errorstring = s;
}
#line 418 "y.tab.c"
/* allocate initial stack or double stack size, up to YYMAXDEPTH */
static int yygrowstack(void)
{
    int newsize, i;
    short *newss;
    YYSTYPE *newvs;

    if ((newsize = yystacksize) == 0)
        newsize = YYINITSTACKSIZE;
    else if (newsize >= YYMAXDEPTH)
        return -1;
    else if ((newsize *= 2) > YYMAXDEPTH)
        newsize = YYMAXDEPTH;

    i = yyssp - yyss;
    newss = (yyss != 0)
          ? (short *)realloc(yyss, newsize * sizeof(*newss))
          : (short *)malloc(newsize * sizeof(*newss));
    if (newss == 0)
        return -1;

    yyss  = newss;
    yyssp = newss + i;
    newvs = (yyvs != 0)
          ? (YYSTYPE *)realloc(yyvs, newsize * sizeof(*newvs))
          : (YYSTYPE *)malloc(newsize * sizeof(*newvs));
    if (newvs == 0)
        return -1;

    yyvs = newvs;
    yyvsp = newvs + i;
    yystacksize = newsize;
    yysslim = yyss + newsize - 1;
    return 0;
}

#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab
int
yyparse(void)
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register const char *yys;

    if ((yys = getenv("YYDEBUG")) != 0)
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = YYEMPTY;

    if (yyss == NULL && yygrowstack()) goto yyoverflow;
    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if ((yyn = yydefred[yystate]) != 0) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yyssp >= yysslim && yygrowstack())
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = YYEMPTY;
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;

    yyerror("syntax error");

#ifdef lint
    goto yyerrlab;
#endif

yyerrlab:
    ++yynerrs;

yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yysslim && yygrowstack())
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = YYEMPTY;
        goto yyloop;
    }

yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    if (yym)
        yyval = yyvsp[1-yym];
    else
        memset(&yyval, 0, sizeof yyval);
    switch (yyn)
    {
case 3:
#line 56 "parser.y"
{ }
break;
case 6:
#line 63 "parser.y"
{ yyval.intlistp = intcons(yyvsp[-1].i,yyvsp[0].intlistp); }
break;
case 7:
#line 64 "parser.y"
{ yyval.intlistp = EMPTYLIST; }
break;
case 8:
#line 67 "parser.y"
{ }
break;
case 9:
#line 70 "parser.y"
{ rparen("term"); }
break;
case 10:
#line 70 "parser.y"
{ yyval.atomp = newatom(yyvsp[-3].i,yyvsp[-2].intlistp); }
break;
case 11:
#line 73 "parser.y"
{ yyval.atomlistp = atomcons(yyvsp[0].atomp,yyvsp[-1].atomlistp); }
break;
case 12:
#line 74 "parser.y"
{ yyval.atomlistp = yyvsp[-1].atomlistp; }
break;
case 13:
#line 75 "parser.y"
{ yyval.atomlistp = EMPTYLIST; }
break;
case 14:
#line 78 "parser.y"
{ yyval.i = yyvsp[0].i; }
break;
case 15:
#line 79 "parser.y"
{ yyval.i = yyvsp[0].i; }
break;
case 16:
#line 82 "parser.y"
{ yyval.intlistp = intcons(yyvsp[-1].i,yyvsp[0].intlistp); }
break;
case 17:
#line 83 "parser.y"
{ yyval.intlistp = EMPTYLIST; }
break;
case 18:
#line 88 "parser.y"
{ rparen("domain"); }
break;
case 19:
#line 88 "parser.y"
{ rparen("domain body"); }
break;
case 20:
#line 88 "parser.y"
{ storedomain(yyvsp[-5].i); }
break;
case 22:
#line 92 "parser.y"
{ }
break;
case 23:
#line 95 "parser.y"
{ rparen(":predicates"); }
break;
case 24:
#line 95 "parser.y"
{ storepredicates(); }
break;
case 25:
#line 96 "parser.y"
{ rparen(":requirements"); }
break;
case 26:
#line 96 "parser.y"
{ checkrequirements(yyvsp[-2].intlistp); }
break;
case 27:
#line 97 "parser.y"
{ rparen(":constants"); }
break;
case 28:
#line 97 "parser.y"
{ storeconstants(yyvsp[-2].typedvarlistp); }
break;
case 29:
#line 98 "parser.y"
{ rparen(":functions"); }
break;
case 30:
#line 98 "parser.y"
{ }
break;
case 31:
#line 99 "parser.y"
{ rparen(":types"); }
break;
case 32:
#line 99 "parser.y"
{ storetypes(yyvsp[-2].typedvarlistp); }
break;
case 33:
#line 100 "parser.y"
{ rparen(":action"); }
break;
case 34:
#line 100 "parser.y"
{ addnewaction(yyvsp[-3].i); }
break;
case 37:
#line 107 "parser.y"
{ rparen(":action definitions"); }
break;
case 38:
#line 107 "parser.y"
{ addactionparameters(yyvsp[-2].typedvarlistp); }
break;
case 39:
#line 108 "parser.y"
{ addactionprecond(yyvsp[0].Sfmap); }
break;
case 40:
#line 109 "parser.y"
{ addactioneffect(yyvsp[0].Seffp); }
break;
case 41:
#line 112 "parser.y"
{ yyval.typedvarlistp = withtype(UNIVTYPE,yyvsp[0].intlistp); }
break;
case 42:
#line 113 "parser.y"
{ yyval.typedvarlistp = yyvsp[0].typedvarlistp; }
break;
case 43:
#line 114 "parser.y"
{ yyval.typedvarlistp = EMPTYLIST; }
break;
case 44:
#line 117 "parser.y"
{ yyval.intlistp = intcons(yyvsp[-1].i,yyvsp[0].intlistp); }
break;
case 45:
#line 118 "parser.y"
{ yyval.intlistp = intcons(yyvsp[0].i, EMPTYLIST); }
break;
case 46:
#line 121 "parser.y"
{ yyval.typedvarlistp = TVappend(yyvsp[-1].typedvarlistp,yyvsp[0].typedvarlistp); }
break;
case 47:
#line 122 "parser.y"
{ yyval.typedvarlistp = yyvsp[0].typedvarlistp; }
break;
case 48:
#line 125 "parser.y"
{ yyval.typedvarlistp = withtype(yyvsp[0].i,yyvsp[-2].intlistp); }
break;
case 49:
#line 129 "parser.y"
{ rparen(":problem"); }
break;
case 50:
#line 129 "parser.y"
{ rparen("problem definition"); }
break;
case 51:
#line 129 "parser.y"
{ addproblem(yyvsp[-5].i); }
break;
case 52:
#line 132 "parser.y"
{ rparen("problem definitions"); }
break;
case 53:
#line 132 "parser.y"
{ }
break;
case 54:
#line 133 "parser.y"
{ rparen("problem definitions"); }
break;
case 55:
#line 133 "parser.y"
{ }
break;
case 56:
#line 136 "parser.y"
{ checkdomain(yyvsp[0].i); }
break;
case 57:
#line 137 "parser.y"
{ storeobjects(yyvsp[0].typedvarlistp); }
break;
case 58:
#line 138 "parser.y"
{ storeinit(yyvsp[0].atomlistp); }
break;
case 59:
#line 139 "parser.y"
{ storegoal(yyvsp[0].Sfmap); }
break;
case 60:
#line 140 "parser.y"
{ }
break;
case 61:
#line 144 "parser.y"
{ yyval.typedvarlistp = TVappend(withtype(yyvsp[-1].i,yyvsp[-3].intlistp),yyvsp[0].typedvarlistp); }
break;
case 62:
#line 145 "parser.y"
{ yyval.typedvarlistp = withtype(UNIVTYPE,yyvsp[0].intlistp); }
break;
case 63:
#line 148 "parser.y"
{ }
break;
case 64:
#line 149 "parser.y"
{ rparen("typed variable list"); }
break;
case 65:
#line 149 "parser.y"
{ }
break;
case 66:
#line 150 "parser.y"
{ }
break;
case 67:
#line 151 "parser.y"
{ }
break;
case 68:
#line 154 "parser.y"
{ rparen("typed atom list"); }
break;
case 69:
#line 154 "parser.y"
{ }
break;
case 70:
#line 155 "parser.y"
{ }
break;
case 71:
#line 158 "parser.y"
{ rparen("function list"); }
break;
case 72:
#line 158 "parser.y"
{ }
break;
case 73:
#line 159 "parser.y"
{ }
break;
case 74:
#line 163 "parser.y"
{ rparen("empty conjunction"); }
break;
case 75:
#line 163 "parser.y"
{ yyval.Sfmap = Strue(); }
break;
case 76:
#line 164 "parser.y"
{ rparen("conjunction"); }
break;
case 77:
#line 164 "parser.y"
{ yyval.Sfmap = Sconjunction(yyvsp[-2].Sfmalistp); }
break;
case 78:
#line 165 "parser.y"
{ rparen("when"); }
break;
case 79:
#line 165 "parser.y"
{ yyval.Sfmap = Sconjunction(Sfmacons(Sneg(yyvsp[-3].Sfmap),Sfmacons(yyvsp[-2].Sfmap,EMPTYLIST))); }
break;
case 80:
#line 166 "parser.y"
{ rparen("disjunction"); }
break;
case 81:
#line 166 "parser.y"
{ yyval.Sfmap = Sdisjunction(yyvsp[-2].Sfmalistp); }
break;
case 82:
#line 167 "parser.y"
{ rparen("imply"); }
break;
case 83:
#line 167 "parser.y"
{ yyval.Sfmap = Sdisjunction(Sfmacons(Sneg(yyvsp[-3].Sfmap),Sfmacons(yyvsp[-2].Sfmap,EMPTYLIST))); }
break;
case 84:
#line 168 "parser.y"
{ rparen("not"); }
break;
case 85:
#line 168 "parser.y"
{ yyval.Sfmap = Sneg(yyvsp[-2].Sfmap); }
break;
case 86:
#line 169 "parser.y"
{ yyval.Sfmap = Satom(yyvsp[0].atomp); }
break;
case 87:
#line 170 "parser.y"
{ rparen("equality"); }
break;
case 88:
#line 170 "parser.y"
{ yyval.Sfmap = SfmaEQ(yyvsp[-3].i,yyvsp[-2].i); }
break;
case 89:
#line 171 "parser.y"
{ rparen("forall"); }
break;
case 90:
#line 171 "parser.y"
{ yyval.Sfmap = Sfmaforall(yyvsp[-4].typedvarlistp,yyvsp[-2].Sfmap); }
break;
case 91:
#line 172 "parser.y"
{ rparen("exists"); }
break;
case 92:
#line 172 "parser.y"
{ yyval.Sfmap = Sfmaforsome(yyvsp[-4].typedvarlistp,yyvsp[-2].Sfmap); }
break;
case 93:
#line 175 "parser.y"
{ yyval.Sfmalistp = Sfmacons(yyvsp[-1].Sfmap,yyvsp[0].Sfmalistp); }
break;
case 94:
#line 176 "parser.y"
{ yyval.Sfmalistp = Sfmacons(yyvsp[0].Sfmap,EMPTYLIST); }
break;
case 95:
#line 179 "parser.y"
{ yyval.Sefflistp = Seffcons(yyvsp[-1].Seffp,yyvsp[0].Sefflistp); }
break;
case 96:
#line 180 "parser.y"
{ yyval.Sefflistp = Seffcons(yyvsp[0].Seffp,EMPTYLIST); }
break;
case 97:
#line 183 "parser.y"
{ }
break;
case 98:
#line 184 "parser.y"
{ }
break;
case 99:
#line 187 "parser.y"
{ rparen("empty conjunction"); }
break;
case 100:
#line 187 "parser.y"
{ yyval.Seffp = Seffconjunction(EMPTYLIST); }
break;
case 101:
#line 188 "parser.y"
{ rparen("compound effect"); }
break;
case 102:
#line 188 "parser.y"
{ yyval.Seffp = Seffconjunction(yyvsp[-2].Sefflistp); }
break;
case 103:
#line 189 "parser.y"
{ rparen("when"); }
break;
case 104:
#line 189 "parser.y"
{ yyval.Seffp = Seffwhen(yyvsp[-3].Sfmap,yyvsp[-2].Seffp); }
break;
case 105:
#line 190 "parser.y"
{ rparen("forall"); }
break;
case 106:
#line 190 "parser.y"
{ yyval.Seffp = Seffforall(yyvsp[-4].typedvarlistp,yyvsp[-2].Seffp); }
break;
case 107:
#line 191 "parser.y"
{ yyval.Seffp = SPeffatom(yyvsp[0].atomp); }
break;
case 108:
#line 192 "parser.y"
{ rparen("not"); }
break;
case 109:
#line 192 "parser.y"
{ yyval.Seffp = SNeffatom(yyvsp[-2].atomp); }
break;
case 110:
#line 193 "parser.y"
{ rparen("increase"); }
break;
case 111:
#line 193 "parser.y"
{ yyval.Seffp = Seffconjunction(NULL); }
break;
#line 1015 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
#endif
    if (yyssp >= yysslim && yygrowstack())
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;

yyoverflow:
    yyerror("yacc stack overflow");

yyabort:
    return (1);

yyaccept:
    return (0);
}
