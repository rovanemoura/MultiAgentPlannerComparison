/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         ops_pddlparse
#define yylex           ops_pddllex
#define yyerror         ops_pddlerror
#define yylval          ops_pddllval
#define yychar          ops_pddlchar
#define yydebug         ops_pddldebug
#define yynerrs         ops_pddlnerrs


/* Copy the first part of user declarations.  */

/* Line 268 of yacc.c  */
#line 19 "scan-ops_pddl.y"


#define YYMAXDEPTH 1000000 
#define YY_NO_UNPUT

#ifndef __SUN__
#define YYSTACK_USE_ALLOCA FALSE
#endif


#include <stdio.h>
#include <string.h> 
#include "ff.h"
#include "memory.h"
#include "parse.h"


#define yyin ops_pddlin
#define yytext ops_pddltext

#ifndef SCAN_ERR
#define SCAN_ERR
#define DOMDEF_EXPECTED            0
#define DOMAIN_EXPECTED            1
#define DOMNAME_EXPECTED           2
#define LBRACKET_EXPECTED          3
#define RBRACKET_EXPECTED          4
#define DOMDEFS_EXPECTED           5
#define REQUIREM_EXPECTED          6
#define TYPEDLIST_EXPECTED         7
#define LITERAL_EXPECTED           8
#define PRECONDDEF_UNCORRECT       9
#define TYPEDEF_EXPECTED          10
#define CONSTLIST_EXPECTED        11
#define PREDDEF_EXPECTED          12 
#define NAME_EXPECTED             13
#define VARIABLE_EXPECTED         14
#define ACTIONFUNCTOR_EXPECTED    15
#define ATOM_FORMULA_EXPECTED     16
#define EFFECT_DEF_EXPECTED       17
#define NEG_FORMULA_EXPECTED      18
#define NOT_SUPPORTED             19
#define ACTION                    20
#define DERIVED_PRED_EXPECTED     21
#endif


#define NAME_STR "name\0"
#define VARIABLE_STR "variable\0"
#define STANDARD_TYPE "OBJECT\0"
 

static char *serrmsg[] = {
  "domain definition expected",
  "'domain' expected",
  "domain name expected",
  "'(' expected",
  "')' expected",
  "additional domain definitions expected",
  "requirements (e.g. ':STRIPS') expected",
  "typed list of <%s> expected",
  "literal expected",
  "uncorrect precondition definition",
  "type definition expected",
  "list of constants expected",
  "predicate definition expected",
  "<name> expected",
  "<variable> expected",
  "action functor expected",
  "atomic formula expected",
  "effect definition expected",
  "negated atomic formula expected",
  "requirement %s not supported by this IPP version",  
  "action definition is not correct",
  "derived predicate definition is not correct",
  NULL
};


//void opserr( int errno, char *par );


static int sact_err;
static char *sact_err_par = NULL;
static PlOperator *scur_op = NULL;
static PlOperator *der_op = NULL;
static Bool sis_negated = FALSE;

 int yylex(void);
 int yyerror(char *msg); 

/* 
 * call	bison -pops -bscan-ops scan-ops.y
 */

void opserr( int errno, char *par )
{

  sact_err = errno;

  if ( sact_err_par ) {
    free(sact_err_par);
  }
  if ( par ) {
    sact_err_par = new_Token(strlen(par)+1);
    strcpy(sact_err_par, par);
  } else {
    sact_err_par = NULL;
  }

}
  
int supported( char *str )

{
  
  int i;
  
/*sositituito per pddl2 (negative-precontions oltre a negation*/
  /*  char * sup[] = { ":STRIPS", ":NEGATION", ":EQUALITY",":TYPING", 
		   ":CONDITIONAL-EFFECTS", ":DISJUNCTIVE-PRECONDITIONS", 
		   ":EXISTENTIAL-PRECONDITIONS", ":UNIVERSAL-PRECONDITIONS", 
		   ":QUANTIFIED-PRECONDITIONS", ":ADL",
		   NULL };     */
  char * sup[] = { ":STRIPS", ":NEGATIVE-PRECONDITIONS",":NEGATION",
		     ":EQUALITY",":TYPING", 
		   ":CONDITIONAL-EFFECTS", ":DISJUNCTIVE-PRECONDITIONS", 
		   ":EXISTENTIAL-PRECONDITIONS", ":UNIVERSAL-PRECONDITIONS", 
		   ":QUANTIFIED-PRECONDITIONS", ":ADL", ":DERIVED",
		   NULL };    

  return 1;
  for (i=0; NULL != sup[i]; i++) {
    if ( SAME == strcmp(sup[i], str) ) {
      return TRUE;
    }
  }
  
  return FALSE;

}


 


/* Line 268 of yacc.c  */
#line 226 "scan-ops_pddl.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     DEFINE_TOK = 258,
     DOMAIN_TOK = 259,
     REQUIREMENTS_TOK = 260,
     TYPES_TOK = 261,
     EITHER_TOK = 262,
     CONSTANTS_TOK = 263,
     PREDICATES_TOK = 264,
     FUNCTIONS_TOK = 265,
     DURATIVE_ACTION_TOK = 266,
     CONDITION_TOK = 267,
     DURATION_TOK = 268,
     DURATION_VAR_TOK = 269,
     AT_START = 270,
     AT_END = 271,
     OVER_ALL = 272,
     INCREASE_TOK = 273,
     DECREASE_TOK = 274,
     TIME_TOK = 275,
     GREATER_OR_EQUAL_TOK = 276,
     LESS_THAN_OR_EQUAL_TOK = 277,
     INTVAL = 278,
     FLOATVAL = 279,
     ASSIGN_TOK = 280,
     SCALE_UP_TOK = 281,
     SCALE_DOWN_TOK = 282,
     PLUS_TOK = 283,
     MINUS_TOK = 284,
     MUL_TOK = 285,
     DIV_TOK = 286,
     EQUAL_TOK = 287,
     GREATER_TOK = 288,
     LESS_THAN_TOK = 289,
     ACTION_TOK = 290,
     VARS_TOK = 291,
     DERIVED_TOK = 292,
     PRECONDITION_TOK = 293,
     PARAMETERS_TOK = 294,
     EFFECT_TOK = 295,
     AND_TOK = 296,
     NOT_TOK = 297,
     WHEN_TOK = 298,
     FORALL_TOK = 299,
     IMPLY_TOK = 300,
     OR_TOK = 301,
     EXISTS_TOK = 302,
     NAME = 303,
     VARIABLE = 304,
     OPEN_PAREN = 305,
     CLOSE_PAREN = 306,
     UMINUS = 307
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 293 of yacc.c  */
#line 169 "scan-ops_pddl.y"


  char string[MAX_LENGTH];
  char *pstring;
  PlNode *pPlNode;
  FactList *pFactList;
  TokenList *pTokenList;
  TypedList *pTypedList;

    int ival;
    float fval;



/* Line 293 of yacc.c  */
#line 329 "scan-ops_pddl.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 343 of yacc.c  */
#line 341 "scan-ops_pddl.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   394

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  53
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  78
/* YYNRULES -- Number of rules.  */
#define YYNRULES  172
/* YYNRULES -- Number of states.  */
#define YYNSTATES  388

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   307

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,     8,    14,    19,    21,    24,
      27,    30,    33,    36,    39,    42,    45,    46,    52,    53,
      54,    61,    62,    68,    69,    70,    77,    78,    79,    87,
      88,    89,    93,    94,   100,   101,   107,   108,   109,   118,
     119,   124,   125,   131,   132,   137,   138,   143,   145,   150,
     155,   160,   166,   174,   182,   184,   190,   192,   197,   203,
     209,   215,   221,   223,   225,   230,   232,   234,   236,   238,
     240,   242,   247,   253,   259,   265,   271,   273,   275,   277,
     279,   281,   283,   285,   287,   289,   290,   293,   295,   300,
     308,   314,   320,   321,   324,   329,   331,   336,   337,   340,
     342,   344,   346,   349,   351,   353,   354,   360,   364,   367,
     368,   374,   378,   381,   382,   383,   392,   393,   399,   400,
     405,   406,   411,   412,   417,   418,   425,   430,   432,   437,
     439,   442,   447,   452,   457,   458,   461,   466,   468,   476,
     482,   488,   493,   498,   503,   508,   514,   520,   526,   532,
     537,   539,   541,   543,   545,   547,   549,   555,   561,   563,
     568,   570,   572,   575,   581,   586,   591,   593,   595,   597,
     599,   601,   604
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      54,     0,    -1,    -1,    55,    56,    -1,    -1,    50,     3,
      58,    57,    59,    -1,    50,     4,    48,    51,    -1,    51,
      -1,    68,    59,    -1,    75,    59,    -1,    73,    59,    -1,
      77,    59,    -1,    60,    59,    -1,   104,    59,    -1,    64,
      59,    -1,   111,    59,    -1,    -1,    50,     9,    62,    61,
      51,    -1,    -1,    -1,    50,    48,   103,    51,    63,    62,
      -1,    -1,    50,    10,    66,    65,    51,    -1,    -1,    -1,
      50,    48,   103,    51,    67,    66,    -1,    -1,    -1,    50,
       5,    69,    48,    70,    71,    51,    -1,    -1,    -1,    48,
      72,    71,    -1,    -1,    50,     6,    74,   102,    51,    -1,
      -1,    50,     8,    76,   102,    51,    -1,    -1,    -1,    50,
      35,    78,    48,    79,    80,    81,    51,    -1,    -1,    39,
      50,   103,    51,    -1,    -1,    36,    50,   103,    51,    81,
      -1,    -1,    38,    84,    82,    81,    -1,    -1,    40,    94,
      83,    81,    -1,    96,    -1,    50,    41,    93,    51,    -1,
      50,    46,    93,    51,    -1,    50,    42,    84,    51,    -1,
      50,    45,    84,    84,    51,    -1,    50,    47,    50,   103,
      51,    84,    51,    -1,    50,    44,    50,   103,    51,    84,
      51,    -1,    85,    -1,    50,    88,    86,    86,    51,    -1,
      89,    -1,    50,    29,    86,    51,    -1,    50,    29,    86,
      86,    51,    -1,    50,    28,    86,    86,    51,    -1,    50,
      30,    86,    86,    51,    -1,    50,    31,    86,    86,    51,
      -1,    87,    -1,    90,    -1,    50,    91,    98,    51,    -1,
      91,    -1,    33,    -1,    34,    -1,    32,    -1,    21,    -1,
      22,    -1,    50,    29,    89,    51,    -1,    50,    29,    89,
      89,    51,    -1,    50,    28,    89,    89,    51,    -1,    50,
      30,    89,    89,    51,    -1,    50,    31,    89,    89,    51,
      -1,    90,    -1,    23,    -1,    24,    -1,    48,    -1,    25,
      -1,    26,    -1,    27,    -1,    18,    -1,    19,    -1,    -1,
      84,    93,    -1,    96,    -1,    50,    41,    95,    51,    -1,
      50,    44,    50,   103,    51,    94,    51,    -1,    50,    43,
      84,    94,    51,    -1,    50,    92,    87,    86,    51,    -1,
      -1,    94,    95,    -1,    50,    42,    97,    51,    -1,    97,
      -1,    50,   101,    98,    51,    -1,    -1,    99,    98,    -1,
      48,    -1,    49,    -1,    48,    -1,    48,   100,    -1,    48,
      -1,    32,    -1,    -1,    48,   130,   100,    51,   102,    -1,
      48,   129,   102,    -1,    48,   102,    -1,    -1,    49,   130,
     100,    51,   103,    -1,    49,   129,   103,    -1,    49,   103,
      -1,    -1,    -1,    50,    11,   105,    48,   106,    80,   107,
      51,    -1,    -1,    36,    50,   103,    51,   107,    -1,    -1,
      13,   124,   108,   107,    -1,    -1,    12,   114,   109,   107,
      -1,    -1,    40,   118,   110,   107,    -1,    -1,    50,    37,
     112,   113,    84,    51,    -1,    50,   101,   103,    51,    -1,
     116,    -1,    50,    41,   115,    51,    -1,   116,    -1,   116,
     115,    -1,    50,    15,    84,    51,    -1,    50,    16,    84,
      51,    -1,    50,    17,    84,    51,    -1,    -1,   118,   117,
      -1,    50,    41,   117,    51,    -1,   119,    -1,    50,    44,
      50,   103,    51,   118,    51,    -1,    50,    43,   114,   118,
      51,    -1,    50,    92,    87,   121,    51,    -1,    50,    15,
      94,    51,    -1,    50,    16,    94,    51,    -1,    50,    15,
     120,    51,    -1,    50,    16,   120,    51,    -1,    50,    18,
      87,   123,    51,    -1,    50,    19,    87,   123,    51,    -1,
      50,    92,    87,   121,    51,    -1,    50,   122,   121,   121,
      51,    -1,    50,    29,   121,    51,    -1,    14,    -1,    86,
      -1,    28,    -1,    29,    -1,    30,    -1,    31,    -1,    50,
      30,    86,    20,    51,    -1,    50,    30,    20,    86,    51,
      -1,    20,    -1,    50,    41,   125,    51,    -1,   126,    -1,
     126,    -1,   126,   125,    -1,    50,   127,    14,   128,    51,
      -1,    50,    15,   126,    51,    -1,    50,    16,   126,    51,
      -1,    22,    -1,    21,    -1,    32,    -1,    89,    -1,    86,
      -1,    29,    48,    -1,    29,    50,     7,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   294,   294,   294,   305,   304,   320,   330,   332,   334,
     336,   338,   340,   343,   345,   349,   357,   356,   366,   369,
     368,   399,   398,   408,   411,   410,   440,   444,   439,   455,
     459,   458,   472,   471,   485,   484,   500,   507,   499,   521,
     525,   538,   541,   560,   559,   566,   565,   580,   593,   599,
     605,   611,   621,   636,   652,   663,   674,   680,   690,   702,
     715,   727,   739,   746,   757,   764,   774,   779,   784,   789,
     794,   801,   808,   815,   822,   829,   836,   844,   854,   866,
     910,   915,   920,   925,   930,   941,   945,   957,   970,   976,
     991,  1006,  1020,  1024,  1036,  1042,  1051,  1063,  1065,  1076,
    1082,  1092,  1099,  1110,  1116,  1127,  1129,  1142,  1153,  1173,
    1175,  1187,  1201,  1220,  1224,  1219,  1234,  1237,  1256,  1255,
    1264,  1263,  1270,  1269,  1280,  1279,  1316,  1344,  1349,  1357,
    1362,  1370,  1376,  1382,  1394,  1398,  1405,  1411,  1417,  1431,
    1446,  1471,  1477,  1483,  1489,  1495,  1505,  1517,  1532,  1539,
    1549,  1558,  1563,  1573,  1583,  1593,  1606,  1618,  1631,  1642,
    1648,  1656,  1661,  1669,  1684,  1689,  1696,  1705,  1714,  1725,
    1727,  1731,  1739
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DEFINE_TOK", "DOMAIN_TOK",
  "REQUIREMENTS_TOK", "TYPES_TOK", "EITHER_TOK", "CONSTANTS_TOK",
  "PREDICATES_TOK", "FUNCTIONS_TOK", "DURATIVE_ACTION_TOK",
  "CONDITION_TOK", "DURATION_TOK", "DURATION_VAR_TOK", "AT_START",
  "AT_END", "OVER_ALL", "INCREASE_TOK", "DECREASE_TOK", "TIME_TOK",
  "GREATER_OR_EQUAL_TOK", "LESS_THAN_OR_EQUAL_TOK", "INTVAL", "FLOATVAL",
  "ASSIGN_TOK", "SCALE_UP_TOK", "SCALE_DOWN_TOK", "PLUS_TOK", "MINUS_TOK",
  "MUL_TOK", "DIV_TOK", "EQUAL_TOK", "GREATER_TOK", "LESS_THAN_TOK",
  "ACTION_TOK", "VARS_TOK", "DERIVED_TOK", "PRECONDITION_TOK",
  "PARAMETERS_TOK", "EFFECT_TOK", "AND_TOK", "NOT_TOK", "WHEN_TOK",
  "FORALL_TOK", "IMPLY_TOK", "OR_TOK", "EXISTS_TOK", "NAME", "VARIABLE",
  "OPEN_PAREN", "CLOSE_PAREN", "UMINUS", "$accept", "file", "$@1",
  "domain_definition", "$@2", "domain_name", "optional_domain_defs",
  "predicates_def", "$@3", "predicates_list", "$@4", "functions_def",
  "$@5", "functions_list", "$@6", "require_def", "$@7", "$@8",
  "require_key_star", "$@9", "types_def", "$@10", "constants_def", "$@11",
  "action_def", "$@12", "$@13", "param_def", "action_def_body", "$@14",
  "$@15", "adl_goal_description", "f_comp", "f_exp", "f_head",
  "binary_comp", "num_exp", "number", "function_symbol", "assign_op",
  "adl_goal_description_star", "adl_effect", "adl_effect_star",
  "literal_term", "atomic_formula_term", "term_star", "term", "name_plus",
  "predicate", "typed_list_name", "typed_list_variable",
  "durative_action_def", "$@16", "$@17", "durative_action_def_body",
  "$@18", "$@19", "$@20", "derived_def", "$@21", "derived_predicate_def",
  "da_adl_goal_description", "timed_adl_goal_description_plus",
  "timed_adl_goal_description", "da_adl_effect_star", "da_adl_effect",
  "timed_adl_effect", "f_assign_da", "f_exp_da", "binary_op", "f_exp_t",
  "duration_constraint", "simple_duration_constraint_plus",
  "simple_duration_constraint", "d_op", "d_value", "type", "either", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    53,    55,    54,    57,    56,    58,    59,    59,    59,
      59,    59,    59,    59,    59,    59,    61,    60,    62,    63,
      62,    65,    64,    66,    67,    66,    69,    70,    68,    71,
      72,    71,    74,    73,    76,    75,    78,    79,    77,    80,
      80,    81,    81,    82,    81,    83,    81,    84,    84,    84,
      84,    84,    84,    84,    84,    85,    86,    86,    86,    86,
      86,    86,    86,    86,    87,    87,    88,    88,    88,    88,
      88,    89,    89,    89,    89,    89,    89,    90,    90,    91,
      92,    92,    92,    92,    92,    93,    93,    94,    94,    94,
      94,    94,    95,    95,    96,    96,    97,    98,    98,    99,
      99,   100,   100,   101,   101,   102,   102,   102,   102,   103,
     103,   103,   103,   105,   106,   104,   107,   107,   108,   107,
     109,   107,   110,   107,   112,   111,   113,   114,   114,   115,
     115,   116,   116,   116,   117,   117,   118,   118,   118,   118,
     118,   119,   119,   119,   119,   119,   119,   120,   121,   121,
     121,   121,   122,   122,   122,   122,   123,   123,   123,   124,
     124,   125,   125,   126,   126,   126,   127,   127,   127,   128,
     128,   129,   130
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     0,     5,     4,     1,     2,     2,
       2,     2,     2,     2,     2,     2,     0,     5,     0,     0,
       6,     0,     5,     0,     0,     6,     0,     0,     7,     0,
       0,     3,     0,     5,     0,     5,     0,     0,     8,     0,
       4,     0,     5,     0,     4,     0,     4,     1,     4,     4,
       4,     5,     7,     7,     1,     5,     1,     4,     5,     5,
       5,     5,     1,     1,     4,     1,     1,     1,     1,     1,
       1,     4,     5,     5,     5,     5,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     2,     1,     4,     7,
       5,     5,     0,     2,     4,     1,     4,     0,     2,     1,
       1,     1,     2,     1,     1,     0,     5,     3,     2,     0,
       5,     3,     2,     0,     0,     8,     0,     5,     0,     4,
       0,     4,     0,     4,     0,     6,     4,     1,     4,     1,
       2,     4,     4,     4,     0,     2,     4,     1,     7,     5,
       5,     4,     4,     4,     4,     5,     5,     5,     5,     4,
       1,     1,     1,     1,     1,     1,     5,     5,     1,     4,
       1,     1,     2,     5,     4,     4,     1,     1,     1,     1,
       1,     2,     3
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     1,     0,     3,     0,     0,     4,     0,
       0,     0,     0,     7,     5,     0,     0,     0,     0,     0,
       0,     0,     0,     6,    26,    32,    34,    18,    23,   113,
      36,   124,    12,    14,     8,    10,     9,    11,    13,    15,
       0,   105,   105,     0,    16,     0,    21,     0,     0,     0,
      27,   105,     0,     0,   109,     0,   109,     0,   114,    37,
       0,     0,    29,     0,   108,   105,     0,    33,    35,   109,
       0,    17,     0,    22,    39,    39,   104,   103,   109,     0,
       0,    54,    47,    95,    30,     0,   171,     0,   107,   101,
       0,   112,   109,     0,    19,    24,     0,   116,    41,     0,
      69,    70,    68,    66,    67,    85,     0,     0,     0,    85,
       0,     0,    97,   125,    29,    28,   172,   102,   105,   111,
       0,    18,    23,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   126,    85,     0,     0,     0,   109,     0,
       0,   109,    77,    78,    79,     0,     0,    62,    56,    63,
      65,    99,   100,     0,    97,    31,   106,   109,    20,    25,
       0,     0,   120,   127,     0,   118,   160,   109,     0,   122,
     137,   115,   109,    43,     0,    45,    87,    38,    86,    48,
      50,    94,     0,     0,    49,     0,     0,     0,     0,     0,
      97,     0,    96,    98,   110,    40,     0,     0,     0,     0,
     116,     0,     0,   167,   166,   168,     0,     0,   116,     0,
       0,     0,     0,     0,    80,    81,    82,   134,     0,     0,
       0,   116,     0,    41,    83,    84,    92,     0,     0,     0,
       0,    41,     0,    51,     0,     0,    56,     0,    56,     0,
      56,     0,    56,     0,    55,     0,     0,     0,     0,     0,
     129,   121,     0,     0,     0,     0,   161,     0,   119,   116,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   134,
       0,   109,     0,   123,    41,    44,    92,     0,     0,     0,
       0,   109,     0,    46,     0,     0,     0,     0,     0,    76,
      57,     0,    71,     0,     0,     0,     0,     0,    64,   131,
     132,   133,   128,   130,   164,   165,   159,   162,   170,    56,
       0,   117,     0,   141,   143,   142,   144,   158,     0,     0,
       0,   136,   135,     0,     0,   150,     0,   151,     0,    42,
      93,    88,     0,     0,     0,    53,    52,    59,     0,     0,
       0,     0,    73,    58,    72,    60,    74,    61,    75,   163,
       0,     0,   145,   146,   139,     0,   152,     0,   154,   155,
       0,   140,    90,     0,    91,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,     0,
       0,   138,   149,     0,    89,   157,   156,   148
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     5,    10,     8,    14,    15,    55,    44,
     121,    16,    57,    46,   122,    17,    40,    62,    85,   114,
      18,    41,    19,    42,    20,    48,    75,    97,   132,   223,
     231,   134,    81,   327,   147,   111,   148,   149,   150,   220,
     135,   276,   277,    82,    83,   153,   154,    90,   112,    52,
      70,    21,    47,    74,   128,   208,   200,   221,    22,    49,
      61,   162,   249,   163,   268,   269,   170,   262,   328,   360,
     319,   165,   255,   256,   207,   310,    65,    66
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -262
static const yytype_int16 yypact[] =
{
    -262,    33,   -12,  -262,    45,  -262,     6,    57,  -262,    18,
     122,    69,    97,  -262,  -262,   122,   122,   122,   122,   122,
     122,   122,   122,  -262,  -262,  -262,  -262,    73,   109,  -262,
    -262,  -262,  -262,  -262,  -262,  -262,  -262,  -262,  -262,  -262,
      90,   116,   116,   126,  -262,   147,  -262,   156,   161,   120,
    -262,     3,   159,   168,   162,   169,   162,   176,  -262,  -262,
       2,   185,   164,    95,  -262,   116,   189,  -262,  -262,    88,
     190,  -262,   198,  -262,   211,   211,  -262,  -262,   162,   252,
     200,  -262,  -262,  -262,  -262,   201,  -262,   249,  -262,   189,
     208,  -262,   162,   189,  -262,  -262,   203,   129,    93,   218,
    -262,  -262,    99,  -262,  -262,   185,   185,   220,   185,   185,
     221,    63,   145,  -262,   164,  -262,  -262,  -262,   116,  -262,
     224,    73,   109,   162,   222,   226,   227,   229,   230,   237,
     185,   238,   240,  -262,   185,   241,   244,   258,   162,   185,
     259,   162,  -262,  -262,  -262,    15,    63,  -262,  -262,  -262,
    -262,  -262,  -262,   260,   145,  -262,  -262,   162,  -262,  -262,
     261,   160,  -262,  -262,   181,  -262,  -262,   162,   239,  -262,
    -262,  -262,   162,  -262,   173,  -262,  -262,  -262,  -262,  -262,
    -262,  -262,   262,   263,  -262,   264,    63,    63,    63,    63,
     145,   265,  -262,  -262,  -262,  -262,   185,   185,   185,   267,
     129,   268,   268,  -262,  -262,  -262,   268,   275,   129,   269,
     271,   271,   104,   104,  -262,  -262,  -262,   229,   222,   272,
     104,   129,   273,    93,  -262,  -262,   238,   276,   185,   277,
     104,    93,   185,  -262,   185,    63,    86,    50,    71,    63,
      86,    63,    86,   274,  -262,   278,   279,   280,   151,   281,
     267,  -262,   246,   282,   283,   284,   268,    63,  -262,   129,
     173,   285,   287,   288,   289,   242,    -1,    -1,   290,   229,
     229,   162,    17,  -262,    93,  -262,   238,   291,     2,   258,
     238,   162,    63,  -262,   292,   293,   294,   202,   295,  -262,
    -262,   296,  -262,   297,   298,   299,   300,   301,  -262,  -262,
    -262,  -262,  -262,  -262,  -262,  -262,  -262,  -262,  -262,  -262,
     302,  -262,   104,  -262,  -262,  -262,  -262,  -262,   307,   303,
     304,  -262,  -262,   305,   306,  -262,   195,  -262,   308,  -262,
    -262,  -262,   309,   310,   311,  -262,  -262,  -262,    86,    86,
      86,    86,  -262,  -262,  -262,  -262,  -262,  -262,  -262,  -262,
      17,     4,  -262,  -262,  -262,   229,    63,    17,    63,    63,
      17,  -262,  -262,   238,  -262,    86,    71,    86,    86,   311,
     312,    63,   338,   313,    50,   314,    17,   315,  -262,   316,
     317,  -262,  -262,   318,  -262,  -262,  -262,  -262
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -262,  -262,  -262,  -262,  -262,  -262,   286,  -262,  -262,   207,
    -262,  -262,  -262,   197,  -262,  -262,  -262,  -262,   209,  -262,
    -262,  -262,  -262,  -262,  -262,  -262,  -262,   319,  -205,  -262,
    -262,    10,  -262,  -111,  -200,  -262,  -178,  -183,  -140,  -167,
     -87,  -127,    94,  -129,  -100,  -118,  -262,    46,   320,   -28,
     -53,  -262,  -262,  -262,  -179,  -262,  -262,  -262,  -262,  -262,
    -262,   153,   123,  -182,   103,  -126,  -262,   163,  -261,  -262,
     108,  -262,   121,  -110,  -262,  -262,   321,   322
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -105
static const yytype_int16 yytable[] =
{
     146,   169,   176,    72,   175,   190,   137,   230,   236,   238,
     240,   242,   266,   267,    53,   166,    91,   250,   275,   317,
     272,   251,   140,    64,   371,    99,   283,   142,   143,   258,
     282,   325,    63,     3,    76,   191,   193,    88,     4,   119,
     142,   143,   273,   186,   187,   188,   189,   178,     6,   318,
      77,    51,   144,   289,   145,   289,     7,   289,   288,   289,
     293,     9,   295,   144,   297,   144,    11,   326,   250,   329,
     160,    80,   243,   142,   143,   235,   237,   239,   241,   309,
     311,   176,   176,   261,   263,   182,   142,   143,   185,   370,
     156,   253,   254,   312,   142,   143,   375,   176,   144,   376,
     145,   290,    24,    25,   194,    26,    27,    28,    29,   142,
     143,   144,   350,   145,   209,   383,   136,    63,   139,   222,
      23,   287,   292,    43,   286,   190,   291,   279,   294,   129,
     296,   130,    30,   131,    31,   117,   287,    69,    50,   120,
     173,   124,   125,    86,   323,    87,   308,   176,  -104,   183,
    -104,   176,   144,   332,   265,   289,   289,   289,   289,    45,
     365,   366,   367,   368,    51,   126,   196,   197,   198,   127,
      60,   334,    12,    13,    54,   196,   197,   198,   236,   238,
     240,   242,   289,   289,   289,   289,   190,   288,   293,   295,
     297,   224,   225,   151,   152,    56,   201,   202,   214,   215,
     216,   199,   203,   204,    58,    76,   245,   246,   247,    59,
      67,    69,    84,   205,   226,   227,   228,   229,   324,    68,
      71,    77,   206,   356,   357,   358,   359,    73,   333,   373,
     338,   339,   340,   341,   176,    79,   377,    89,   280,   369,
     372,    94,   284,   144,   285,   235,   374,   239,   241,    95,
      96,   113,   115,   123,   210,   211,   116,   212,   213,   118,
     379,   201,   202,   291,   214,   215,   216,   203,   204,   133,
     138,   141,   161,   100,   101,   157,   164,   167,   205,   168,
     217,   171,   218,   219,   102,   103,   104,   172,   174,   257,
     144,   177,   179,   105,   106,   180,   107,   108,   109,   110,
      77,    32,    33,    34,    35,    36,    37,    38,    39,   181,
     184,   192,   195,   232,   233,   234,   244,   248,   252,   159,
     259,   260,   271,   155,   274,   298,   278,   281,   158,   299,
     300,   301,   302,   304,   305,   306,   313,   351,   314,   315,
     316,   321,   331,   335,   336,   337,   342,   343,   344,   345,
     346,   347,   348,   349,   352,   353,   354,   355,   380,   361,
     362,   363,   364,   378,   381,   382,   384,   385,   386,   387,
     330,   270,   322,   303,   264,   320,     0,   307,     0,     0,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      92,    93,     0,     0,    98
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-262))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
     111,   127,   131,    56,   131,   145,   106,   174,   186,   187,
     188,   189,   212,   213,    42,   125,    69,   199,   223,    20,
     220,   200,   109,    51,    20,    78,   231,    23,    24,   208,
     230,    14,    29,     0,    32,   146,   154,    65,    50,    92,
      23,    24,   221,    28,    29,    30,    31,   134,     3,    50,
      48,    48,    48,   236,    50,   238,    50,   240,   236,   242,
     238,     4,   240,    48,   242,    48,    48,    50,   250,   274,
     123,    61,   190,    23,    24,   186,   187,   188,   189,   257,
     259,   210,   211,   210,   211,   138,    23,    24,   141,   350,
     118,   201,   202,   260,    23,    24,   357,   226,    48,   360,
      50,    51,     5,     6,   157,     8,     9,    10,    11,    23,
      24,    48,   312,    50,   167,   376,   106,    29,   108,   172,
      51,    50,    51,    50,   235,   265,   237,   227,   239,    36,
     241,    38,    35,    40,    37,    89,    50,    49,    48,    93,
     130,    12,    13,    48,   270,    50,   257,   276,    49,   139,
      51,   280,    48,   280,    50,   338,   339,   340,   341,    50,
     338,   339,   340,   341,    48,    36,    15,    16,    17,    40,
      50,   282,    50,    51,    48,    15,    16,    17,   356,   357,
     358,   359,   365,   366,   367,   368,   326,   365,   366,   367,
     368,    18,    19,    48,    49,    48,    15,    16,    25,    26,
      27,    41,    21,    22,    48,    32,   196,   197,   198,    48,
      51,    49,    48,    32,    41,    42,    43,    44,   271,    51,
      51,    48,    41,    28,    29,    30,    31,    51,   281,   355,
      28,    29,    30,    31,   363,    50,   363,    48,   228,   350,
     351,    51,   232,    48,   234,   356,   357,   358,   359,    51,
      39,    51,    51,    50,    15,    16,     7,    18,    19,    51,
     371,    15,    16,   374,    25,    26,    27,    21,    22,    51,
      50,    50,    50,    21,    22,    51,    50,    50,    32,    50,
      41,    51,    43,    44,    32,    33,    34,    50,    50,    14,
      48,    51,    51,    41,    42,    51,    44,    45,    46,    47,
      48,    15,    16,    17,    18,    19,    20,    21,    22,    51,
      51,    51,    51,    51,    51,    51,    51,    50,    50,   122,
      51,    50,    50,   114,    51,    51,    50,    50,   121,    51,
      51,    51,    51,    51,    51,    51,    51,    30,    51,    51,
      51,    51,    51,    51,    51,    51,    51,    51,    51,    51,
      51,    51,    51,    51,    51,    51,    51,    51,    20,    51,
      51,    51,    51,    51,    51,    51,    51,    51,    51,    51,
     276,   218,   269,   250,   211,   267,    -1,   256,    -1,    -1,
      60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    69,    -1,    -1,    75
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    54,    55,     0,    50,    56,     3,    50,    58,     4,
      57,    48,    50,    51,    59,    60,    64,    68,    73,    75,
      77,   104,   111,    51,     5,     6,     8,     9,    10,    11,
      35,    37,    59,    59,    59,    59,    59,    59,    59,    59,
      69,    74,    76,    50,    62,    50,    66,   105,    78,   112,
      48,    48,   102,   102,    48,    61,    48,    65,    48,    48,
      50,   113,    70,    29,   102,   129,   130,    51,    51,    49,
     103,    51,   103,    51,   106,    79,    32,    48,   101,    50,
      84,    85,    96,    97,    48,    71,    48,    50,   102,    48,
     100,   103,   129,   130,    51,    51,    39,    80,    80,   103,
      21,    22,    32,    33,    34,    41,    42,    44,    45,    46,
      47,    88,   101,    51,    72,    51,     7,   100,    51,   103,
     100,    63,    67,    50,    12,    13,    36,    40,   107,    36,
      38,    40,    81,    51,    84,    93,    84,    97,    50,    84,
      93,    50,    23,    24,    48,    50,    86,    87,    89,    90,
      91,    48,    49,    98,    99,    71,   102,    51,    62,    66,
     103,    50,   114,   116,    50,   124,   126,    50,    50,   118,
     119,    51,    50,    84,    50,    94,    96,    51,    93,    51,
      51,    51,   103,    84,    51,   103,    28,    29,    30,    31,
      91,    86,    51,    98,   103,    51,    15,    16,    17,    41,
     109,    15,    16,    21,    22,    32,    41,   127,   108,   103,
      15,    16,    18,    19,    25,    26,    27,    41,    43,    44,
      92,   110,   103,    82,    18,    19,    41,    42,    43,    44,
      92,    83,    51,    51,    51,    86,    89,    86,    89,    86,
      89,    86,    89,    98,    51,    84,    84,    84,    50,   115,
     116,   107,    50,   126,   126,   125,   126,    14,   107,    51,
      50,    94,   120,    94,   120,    50,    87,    87,   117,   118,
     114,    50,    87,   107,    51,    81,    94,    95,    50,    97,
      84,    50,    87,    81,    84,    84,    86,    50,    89,    90,
      51,    86,    51,    89,    86,    89,    86,    89,    51,    51,
      51,    51,    51,   115,    51,    51,    51,   125,    86,    89,
     128,   107,    92,    51,    51,    51,    51,    20,    50,   123,
     123,    51,   117,   118,   103,    14,    50,    86,   121,    81,
      95,    51,    94,   103,    86,    51,    51,    51,    28,    29,
      30,    31,    51,    51,    51,    51,    51,    51,    51,    51,
      87,    30,    51,    51,    51,    51,    28,    29,    30,    31,
     122,    51,    51,    51,    51,    89,    89,    89,    89,    86,
     121,    20,    86,   118,    86,   121,   121,    94,    51,    86,
      20,    51,    51,   121,    51,    51,    51,    51
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* This macro is provided for backward compatibility. */

#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  YYSIZE_T yysize1;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = 0;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                yysize1 = yysize + yytnamerr (0, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1806 of yacc.c  */
#line 294 "scan-ops_pddl.y"
    { 
  opserr( DOMDEF_EXPECTED, NULL );
}
    break;

  case 4:

/* Line 1806 of yacc.c  */
#line 305 "scan-ops_pddl.y"
    { 
}
    break;

  case 5:

/* Line 1806 of yacc.c  */
#line 308 "scan-ops_pddl.y"
    {
  static int once=0;
  if ( gcmd_line.display_info >= 1 && once==0) {
    printf(" domain '%s' defined", gdomain_name);
    once=1;
  }
}
    break;

  case 6:

/* Line 1806 of yacc.c  */
#line 321 "scan-ops_pddl.y"
    { 
  gdomain_name = new_Token( strlen((yyvsp[(3) - (4)].string))+1 );
  strcpy( gdomain_name, (yyvsp[(3) - (4)].string));
}
    break;

  case 16:

/* Line 1806 of yacc.c  */
#line 357 "scan-ops_pddl.y"
    {
}
    break;

  case 17:

/* Line 1806 of yacc.c  */
#line 360 "scan-ops_pddl.y"
    { 
}
    break;

  case 18:

/* Line 1806 of yacc.c  */
#line 366 "scan-ops_pddl.y"
    {}
    break;

  case 19:

/* Line 1806 of yacc.c  */
#line 369 "scan-ops_pddl.y"
    {

  TypedListList *tll;

  if ( gparse_predicates ) {
    tll = gparse_predicates;
    while ( tll->next ) {
      tll = tll->next;
    }
    tll->next = new_TypedListList();
    tll = tll->next;
  } else {
    tll = new_TypedListList();
    gparse_predicates = tll;
  }

  tll->predicate = new_Token( strlen( (yyvsp[(2) - (4)].string) ) + 1);
  strcpy( tll->predicate, (yyvsp[(2) - (4)].string) );

  tll->args = (yyvsp[(3) - (4)].pTypedList);

}
    break;

  case 21:

/* Line 1806 of yacc.c  */
#line 399 "scan-ops_pddl.y"
    {
}
    break;

  case 22:

/* Line 1806 of yacc.c  */
#line 402 "scan-ops_pddl.y"
    { 
}
    break;

  case 23:

/* Line 1806 of yacc.c  */
#line 408 "scan-ops_pddl.y"
    {}
    break;

  case 24:

/* Line 1806 of yacc.c  */
#line 411 "scan-ops_pddl.y"
    {

  TypedListList *tll;

  if ( gparse_functions ) {
    tll = gparse_functions;
    while ( tll->next ) {
      tll = tll->next;
    }
    tll->next = new_TypedListList();
    tll = tll->next;
  } else {
    tll = new_TypedListList();
    gparse_functions = tll;
  }

  tll->predicate = new_Token( strlen( (yyvsp[(2) - (4)].string) ) + 1);
  strcpy( tll->predicate, (yyvsp[(2) - (4)].string) );

  tll->args = (yyvsp[(3) - (4)].pTypedList);

}
    break;

  case 26:

/* Line 1806 of yacc.c  */
#line 440 "scan-ops_pddl.y"
    {
  opserr( REQUIREM_EXPECTED, NULL );
}
    break;

  case 27:

/* Line 1806 of yacc.c  */
#line 444 "scan-ops_pddl.y"
    { 
  if ( !supported( (yyvsp[(4) - (4)].string) ) ) {
    opserr( NOT_SUPPORTED, (yyvsp[(4) - (4)].string) );
    yyerror(NULL);
  }
}
    break;

  case 30:

/* Line 1806 of yacc.c  */
#line 459 "scan-ops_pddl.y"
    { 
  if ( !supported( (yyvsp[(1) - (1)].string) ) ) {
    opserr( NOT_SUPPORTED, (yyvsp[(1) - (1)].string) );
    yyerror(NULL);
  }
}
    break;

  case 32:

/* Line 1806 of yacc.c  */
#line 472 "scan-ops_pddl.y"
    { 
  opserr( TYPEDEF_EXPECTED, NULL ); 
}
    break;

  case 33:

/* Line 1806 of yacc.c  */
#line 476 "scan-ops_pddl.y"
    {
  gparse_types = (yyvsp[(4) - (5)].pTypedList);
}
    break;

  case 34:

/* Line 1806 of yacc.c  */
#line 485 "scan-ops_pddl.y"
    { 
  opserr( CONSTLIST_EXPECTED, NULL ); 
}
    break;

  case 35:

/* Line 1806 of yacc.c  */
#line 489 "scan-ops_pddl.y"
    {
  gparse_constants = (yyvsp[(4) - (5)].pTypedList);
}
    break;

  case 36:

/* Line 1806 of yacc.c  */
#line 500 "scan-ops_pddl.y"
    { 
#if YYDEBUG != 0
  printf("\n\nin action_def rule\n\n\n"); 
#endif
  opserr( ACTION, NULL );
}
    break;

  case 37:

/* Line 1806 of yacc.c  */
#line 507 "scan-ops_pddl.y"
    { 
  scur_op = new_PlOperator( (yyvsp[(4) - (4)].string) );
}
    break;

  case 38:

/* Line 1806 of yacc.c  */
#line 511 "scan-ops_pddl.y"
    {
  scur_op->next = gloaded_ops;
  gloaded_ops = scur_op; 
}
    break;

  case 39:

/* Line 1806 of yacc.c  */
#line 521 "scan-ops_pddl.y"
    { 
  scur_op->params = NULL; 
}
    break;

  case 40:

/* Line 1806 of yacc.c  */
#line 526 "scan-ops_pddl.y"
    {
  TypedList *tl;
  scur_op->parse_params = (yyvsp[(3) - (4)].pTypedList);
  for (tl = scur_op->parse_params; tl; tl = tl->next) {
    /* to be able to distinguish params from :VARS 
     */
    scur_op->number_of_real_params++;
  }
}
    break;

  case 42:

/* Line 1806 of yacc.c  */
#line 542 "scan-ops_pddl.y"
    {
  TypedList *tl = NULL;

  /* add vars as parameters 
   */
  if ( scur_op->parse_params ) {
    for( tl = scur_op->parse_params; tl->next; tl = tl->next ) {
      /* empty, get to the end of list 
       */
    }
    tl->next = (yyvsp[(3) - (5)].pTypedList);
    tl = tl->next;
  } else {
    scur_op->parse_params = (yyvsp[(3) - (5)].pTypedList);
  }
}
    break;

  case 43:

/* Line 1806 of yacc.c  */
#line 560 "scan-ops_pddl.y"
    { 
  scur_op->preconds = (yyvsp[(2) - (2)].pPlNode); 
}
    break;

  case 45:

/* Line 1806 of yacc.c  */
#line 566 "scan-ops_pddl.y"
    { 
  scur_op->effects = (yyvsp[(2) - (2)].pPlNode); 
}
    break;

  case 47:

/* Line 1806 of yacc.c  */
#line 581 "scan-ops_pddl.y"
    { 
  if ( sis_negated ) {
    (yyval.pPlNode) = new_PlNode(NOT);
    (yyval.pPlNode)->sons = new_PlNode(ATOM);
    (yyval.pPlNode)->sons->atom = (yyvsp[(1) - (1)].pTokenList);
    sis_negated = FALSE;
  } else {
    (yyval.pPlNode) = new_PlNode(ATOM);
    (yyval.pPlNode)->atom = (yyvsp[(1) - (1)].pTokenList);
  }
}
    break;

  case 48:

/* Line 1806 of yacc.c  */
#line 594 "scan-ops_pddl.y"
    { 
  (yyval.pPlNode) = new_PlNode(AND);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 49:

/* Line 1806 of yacc.c  */
#line 600 "scan-ops_pddl.y"
    { 
  (yyval.pPlNode) = new_PlNode(OR);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 50:

/* Line 1806 of yacc.c  */
#line 606 "scan-ops_pddl.y"
    { 
  (yyval.pPlNode) = new_PlNode(NOT);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 51:

/* Line 1806 of yacc.c  */
#line 612 "scan-ops_pddl.y"
    { 
  PlNode *np = new_PlNode(NOT);
  np->sons = (yyvsp[(3) - (5)].pPlNode);
  np->next = (yyvsp[(4) - (5)].pPlNode);

  (yyval.pPlNode) = new_PlNode(OR);
  (yyval.pPlNode)->sons = np;
}
    break;

  case 52:

/* Line 1806 of yacc.c  */
#line 624 "scan-ops_pddl.y"
    { 

  PlNode *pln;

  pln = new_PlNode(EX);
  pln->parse_vars = (yyvsp[(4) - (7)].pTypedList);

  (yyval.pPlNode) = pln;
  pln->sons = (yyvsp[(6) - (7)].pPlNode);

}
    break;

  case 53:

/* Line 1806 of yacc.c  */
#line 639 "scan-ops_pddl.y"
    { 

  PlNode *pln;

  pln = new_PlNode(ALL);
  pln->parse_vars = (yyvsp[(4) - (7)].pTypedList);

  (yyval.pPlNode) = pln;
  pln->sons = (yyvsp[(6) - (7)].pPlNode);

}
    break;

  case 55:

/* Line 1806 of yacc.c  */
#line 664 "scan-ops_pddl.y"
    { 
  (yyval.pPlNode) = new_PlNode(BIN_COMP);
  (yyval.pPlNode)->sons = (yyvsp[(2) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons= (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons->next= (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 56:

/* Line 1806 of yacc.c  */
#line 675 "scan-ops_pddl.y"
    {
       (yyval.pPlNode)=new_PlNode(NUM_EXP);
       (yyval.pPlNode)->sons = (yyvsp[(1) - (1)].pPlNode);
}
    break;

  case 57:

/* Line 1806 of yacc.c  */
#line 681 "scan-ops_pddl.y"
    { 
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(UMINUS_CONN);
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 58:

/* Line 1806 of yacc.c  */
#line 691 "scan-ops_pddl.y"
    { 
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(MINUS_CONN); 
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons->next = (yyvsp[(4) - (5)].pPlNode);

}
    break;

  case 59:

/* Line 1806 of yacc.c  */
#line 703 "scan-ops_pddl.y"
    { 
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(PLUS_CONN); 
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons->next = (yyvsp[(4) - (5)].pPlNode);


}
    break;

  case 60:

/* Line 1806 of yacc.c  */
#line 716 "scan-ops_pddl.y"
    { 
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(MUL_CONN); 
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons->next = (yyvsp[(4) - (5)].pPlNode);

}
    break;

  case 61:

/* Line 1806 of yacc.c  */
#line 728 "scan-ops_pddl.y"
    { 
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(DIV_CONN); 
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons->next = (yyvsp[(4) - (5)].pPlNode);

}
    break;

  case 62:

/* Line 1806 of yacc.c  */
#line 740 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(FN_HEAD);
  (yyval.pPlNode)->atom = (yyvsp[(1) - (1)].pTokenList);
}
    break;

  case 63:

/* Line 1806 of yacc.c  */
#line 747 "scan-ops_pddl.y"
    {
    (yyval.pPlNode)=new_PlNode(ATOM);
    (yyval.pPlNode)->atom = (yyvsp[(1) - (1)].pTokenList);
}
    break;

  case 64:

/* Line 1806 of yacc.c  */
#line 758 "scan-ops_pddl.y"
    { 
  (yyval.pTokenList) = new_TokenList();
  (yyval.pTokenList)->item = (yyvsp[(2) - (4)].pstring);
  (yyval.pTokenList)->next = (yyvsp[(3) - (4)].pTokenList);
}
    break;

  case 65:

/* Line 1806 of yacc.c  */
#line 765 "scan-ops_pddl.y"
    { 
  (yyval.pTokenList) = new_TokenList();
  (yyval.pTokenList)->item = (yyvsp[(1) - (1)].pstring);
}
    break;

  case 66:

/* Line 1806 of yacc.c  */
#line 775 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(GREATER_THAN_CONN);
}
    break;

  case 67:

/* Line 1806 of yacc.c  */
#line 780 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(LESS_THAN_CONN);
}
    break;

  case 68:

/* Line 1806 of yacc.c  */
#line 785 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(EQUAL_CONN);
}
    break;

  case 69:

/* Line 1806 of yacc.c  */
#line 790 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(GREATER_OR_EQUAL_CONN);
}
    break;

  case 70:

/* Line 1806 of yacc.c  */
#line 795 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(LESS_THAN_OR_EQUAL_CONN);
}
    break;

  case 71:

/* Line 1806 of yacc.c  */
#line 802 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(UMINUS_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
   
}
    break;

  case 72:

/* Line 1806 of yacc.c  */
#line 809 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(MINUS_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->next = (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 73:

/* Line 1806 of yacc.c  */
#line 816 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(PLUS_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->next = (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 74:

/* Line 1806 of yacc.c  */
#line 823 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(MUL_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->next = (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 75:

/* Line 1806 of yacc.c  */
#line 830 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(DIV_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->next = (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 76:

/* Line 1806 of yacc.c  */
#line 837 "scan-ops_pddl.y"
    {
    (yyval.pPlNode)=new_PlNode(ATOM);
    (yyval.pPlNode)->atom = (yyvsp[(1) - (1)].pTokenList);
}
    break;

  case 77:

/* Line 1806 of yacc.c  */
#line 845 "scan-ops_pddl.y"
    {
  Token t;
  t = new_Token( strlen((yyvsp[(1) - (1)].string))+1 );
  strcpy (t, (yyvsp[(1) - (1)].string));
  (yyval.pTokenList) = new_TokenList();
  (yyval.pTokenList)->item = t;

}
    break;

  case 78:

/* Line 1806 of yacc.c  */
#line 855 "scan-ops_pddl.y"
    {
  Token t;
  t = new_Token( strlen((yyvsp[(1) - (1)].string))+1 );
  strcpy (t, (yyvsp[(1) - (1)].string));
  (yyval.pTokenList) = new_TokenList();
  (yyval.pTokenList)->item = t;

}
    break;

  case 79:

/* Line 1806 of yacc.c  */
#line 867 "scan-ops_pddl.y"
    { 
  (yyval.pstring) = new_Token( strlen((yyvsp[(1) - (1)].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[(1) - (1)].string) );
}
    break;

  case 80:

/* Line 1806 of yacc.c  */
#line 911 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(ASSIGN_CONN);
}
    break;

  case 81:

/* Line 1806 of yacc.c  */
#line 916 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(SCALE_UP_CONN);
}
    break;

  case 82:

/* Line 1806 of yacc.c  */
#line 921 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(SCALE_DOWN_CONN);
}
    break;

  case 83:

/* Line 1806 of yacc.c  */
#line 926 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(INCREASE_CONN);
}
    break;

  case 84:

/* Line 1806 of yacc.c  */
#line 931 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=new_PlNode(DECREASE_CONN);
}
    break;

  case 85:

/* Line 1806 of yacc.c  */
#line 941 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = NULL;
}
    break;

  case 86:

/* Line 1806 of yacc.c  */
#line 946 "scan-ops_pddl.y"
    {
  (yyvsp[(1) - (2)].pPlNode)->next = (yyvsp[(2) - (2)].pPlNode);
  (yyval.pPlNode) = (yyvsp[(1) - (2)].pPlNode);
}
    break;

  case 87:

/* Line 1806 of yacc.c  */
#line 958 "scan-ops_pddl.y"
    { 
  if ( sis_negated ) {
    (yyval.pPlNode) = new_PlNode(NOT);
    (yyval.pPlNode)->sons = new_PlNode(ATOM);
    (yyval.pPlNode)->sons->atom = (yyvsp[(1) - (1)].pTokenList);
    sis_negated = FALSE;
  } else {
    (yyval.pPlNode) = new_PlNode(ATOM);
    (yyval.pPlNode)->atom = (yyvsp[(1) - (1)].pTokenList);
  }
}
    break;

  case 88:

/* Line 1806 of yacc.c  */
#line 971 "scan-ops_pddl.y"
    { 
  (yyval.pPlNode) = new_PlNode(AND);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 89:

/* Line 1806 of yacc.c  */
#line 979 "scan-ops_pddl.y"
    { 

  PlNode *pln;

  pln = new_PlNode(ALL);
  pln->parse_vars = (yyvsp[(4) - (7)].pTypedList);

  (yyval.pPlNode) = pln;
  pln->sons = (yyvsp[(6) - (7)].pPlNode);

}
    break;

  case 90:

/* Line 1806 of yacc.c  */
#line 992 "scan-ops_pddl.y"
    {
  /* This will be conditional effects in FF representation, but here
   * a formula like (WHEN p q) will be saved as:
   *  [WHEN]
   *  [sons]
   *   /  \
   * [p]  [q]
   * That means, the first son is p, and the second one is q. 
   */
  (yyval.pPlNode) = new_PlNode(WHEN);
  (yyvsp[(3) - (5)].pPlNode)->next = (yyvsp[(4) - (5)].pPlNode);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (5)].pPlNode);
}
    break;

  case 91:

/* Line 1806 of yacc.c  */
#line 1007 "scan-ops_pddl.y"
    {
  PlNode* pln;
  (yyval.pPlNode)=(yyvsp[(2) - (5)].pPlNode);
  pln = new_PlNode(FN_HEAD);
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->atom = (yyvsp[(3) - (5)].pTokenList);
  (yyval.pPlNode)->sons->next = (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 92:

/* Line 1806 of yacc.c  */
#line 1020 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = NULL;
}
    break;

  case 93:

/* Line 1806 of yacc.c  */
#line 1025 "scan-ops_pddl.y"
    {
  (yyvsp[(1) - (2)].pPlNode)->next = (yyvsp[(2) - (2)].pPlNode);
  (yyval.pPlNode) = (yyvsp[(1) - (2)].pPlNode);
}
    break;

  case 94:

/* Line 1806 of yacc.c  */
#line 1037 "scan-ops_pddl.y"
    { 
  (yyval.pTokenList) = (yyvsp[(3) - (4)].pTokenList);
  sis_negated = TRUE;
}
    break;

  case 95:

/* Line 1806 of yacc.c  */
#line 1043 "scan-ops_pddl.y"
    {
  (yyval.pTokenList) = (yyvsp[(1) - (1)].pTokenList);
}
    break;

  case 96:

/* Line 1806 of yacc.c  */
#line 1052 "scan-ops_pddl.y"
    { 
  (yyval.pTokenList) = new_TokenList();
  (yyval.pTokenList)->item = (yyvsp[(2) - (4)].pstring);
  (yyval.pTokenList)->next = (yyvsp[(3) - (4)].pTokenList);
}
    break;

  case 97:

/* Line 1806 of yacc.c  */
#line 1063 "scan-ops_pddl.y"
    { (yyval.pTokenList) = NULL; }
    break;

  case 98:

/* Line 1806 of yacc.c  */
#line 1066 "scan-ops_pddl.y"
    {
  (yyval.pTokenList) = new_TokenList();
  (yyval.pTokenList)->item = (yyvsp[(1) - (2)].pstring);
  (yyval.pTokenList)->next = (yyvsp[(2) - (2)].pTokenList);
}
    break;

  case 99:

/* Line 1806 of yacc.c  */
#line 1077 "scan-ops_pddl.y"
    { 
  (yyval.pstring) = new_Token( strlen((yyvsp[(1) - (1)].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[(1) - (1)].string) );
}
    break;

  case 100:

/* Line 1806 of yacc.c  */
#line 1083 "scan-ops_pddl.y"
    { 
  (yyval.pstring) = new_Token( strlen((yyvsp[(1) - (1)].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[(1) - (1)].string) );
}
    break;

  case 101:

/* Line 1806 of yacc.c  */
#line 1093 "scan-ops_pddl.y"
    {
  (yyval.pTokenList) = new_TokenList();
  (yyval.pTokenList)->item = new_Token( strlen((yyvsp[(1) - (1)].string))+1 );
  strcpy( (yyval.pTokenList)->item, (yyvsp[(1) - (1)].string) );
}
    break;

  case 102:

/* Line 1806 of yacc.c  */
#line 1100 "scan-ops_pddl.y"
    {
  (yyval.pTokenList) = new_TokenList();
  (yyval.pTokenList)->item = new_Token( strlen((yyvsp[(1) - (2)].string))+1 );
  strcpy( (yyval.pTokenList)->item, (yyvsp[(1) - (2)].string) );
  (yyval.pTokenList)->next = (yyvsp[(2) - (2)].pTokenList);
}
    break;

  case 103:

/* Line 1806 of yacc.c  */
#line 1111 "scan-ops_pddl.y"
    { 
  (yyval.pstring) = new_Token( strlen((yyvsp[(1) - (1)].string))+1 );
  strcpy( (yyval.pstring), (yyvsp[(1) - (1)].string) );
}
    break;

  case 104:

/* Line 1806 of yacc.c  */
#line 1117 "scan-ops_pddl.y"
    { 
  (yyval.pstring) = new_Token( strlen(EQ_STR)+1 );
  strcpy( (yyval.pstring), EQ_STR );
}
    break;

  case 105:

/* Line 1806 of yacc.c  */
#line 1127 "scan-ops_pddl.y"
    { (yyval.pTypedList) = NULL; }
    break;

  case 106:

/* Line 1806 of yacc.c  */
#line 1133 "scan-ops_pddl.y"
    { 

  (yyval.pTypedList) = new_TypedList();
  (yyval.pTypedList)->name = new_Token( strlen((yyvsp[(1) - (5)].string))+1 );
  strcpy( (yyval.pTypedList)->name, (yyvsp[(1) - (5)].string) );
  (yyval.pTypedList)->type = (yyvsp[(3) - (5)].pTokenList);
  (yyval.pTypedList)->next = (yyvsp[(5) - (5)].pTypedList);
}
    break;

  case 107:

/* Line 1806 of yacc.c  */
#line 1143 "scan-ops_pddl.y"
    {
  (yyval.pTypedList) = new_TypedList();
  (yyval.pTypedList)->name = new_Token( strlen((yyvsp[(1) - (3)].string))+1 );
  strcpy( (yyval.pTypedList)->name, (yyvsp[(1) - (3)].string) );
  (yyval.pTypedList)->type = new_TokenList();
  (yyval.pTypedList)->type->item = new_Token( strlen((yyvsp[(2) - (3)].pstring))+1 );
  strcpy( (yyval.pTypedList)->type->item, (yyvsp[(2) - (3)].pstring) );
  (yyval.pTypedList)->next = (yyvsp[(3) - (3)].pTypedList);
}
    break;

  case 108:

/* Line 1806 of yacc.c  */
#line 1154 "scan-ops_pddl.y"
    {
  (yyval.pTypedList) = new_TypedList();
  (yyval.pTypedList)->name = new_Token( strlen((yyvsp[(1) - (2)].string))+1 );
  strcpy( (yyval.pTypedList)->name, (yyvsp[(1) - (2)].string) );
  if ( (yyvsp[(2) - (2)].pTypedList) ) {/* another element (already typed) is following */
    (yyval.pTypedList)->type = copy_TokenList( (yyvsp[(2) - (2)].pTypedList)->type );
  } else {/* no further element - it must be an untyped list */
    (yyval.pTypedList)->type = new_TokenList();
    (yyval.pTypedList)->type->item = new_Token( strlen(STANDARD_TYPE)+1 );
    strcpy( (yyval.pTypedList)->type->item, STANDARD_TYPE );
  }
  (yyval.pTypedList)->next = (yyvsp[(2) - (2)].pTypedList);
}
    break;

  case 109:

/* Line 1806 of yacc.c  */
#line 1173 "scan-ops_pddl.y"
    { (yyval.pTypedList) = NULL; }
    break;

  case 110:

/* Line 1806 of yacc.c  */
#line 1179 "scan-ops_pddl.y"
    {
  (yyval.pTypedList) = new_TypedList();
  (yyval.pTypedList)->name = new_Token( strlen((yyvsp[(1) - (5)].string))+1 );
  strcpy( (yyval.pTypedList)->name, (yyvsp[(1) - (5)].string) );
  (yyval.pTypedList)->type = (yyvsp[(3) - (5)].pTokenList);
  (yyval.pTypedList)->next = (yyvsp[(5) - (5)].pTypedList);
}
    break;

  case 111:

/* Line 1806 of yacc.c  */
#line 1191 "scan-ops_pddl.y"
    {
  (yyval.pTypedList) = new_TypedList();
  (yyval.pTypedList)->name = new_Token( strlen((yyvsp[(1) - (3)].string))+1 );
  strcpy( (yyval.pTypedList)->name, (yyvsp[(1) - (3)].string) );
  (yyval.pTypedList)->type = new_TokenList();
  (yyval.pTypedList)->type->item = new_Token( strlen((yyvsp[(2) - (3)].pstring))+1 );
  strcpy( (yyval.pTypedList)->type->item, (yyvsp[(2) - (3)].pstring) );
  (yyval.pTypedList)->next = (yyvsp[(3) - (3)].pTypedList);
}
    break;

  case 112:

/* Line 1806 of yacc.c  */
#line 1202 "scan-ops_pddl.y"
    {
  (yyval.pTypedList) = new_TypedList();
  (yyval.pTypedList)->name = new_Token( strlen((yyvsp[(1) - (2)].string))+1 );
  strcpy( (yyval.pTypedList)->name, (yyvsp[(1) - (2)].string) );
  if ( (yyvsp[(2) - (2)].pTypedList) ) {/* another element (already typed) is following */
    (yyval.pTypedList)->type = copy_TokenList( (yyvsp[(2) - (2)].pTypedList)->type );
  } else {/* no further element - it must be an untyped list */
    (yyval.pTypedList)->type = new_TokenList();
    (yyval.pTypedList)->type->item = new_Token( strlen(STANDARD_TYPE)+1 );
    strcpy( (yyval.pTypedList)->type->item, STANDARD_TYPE );
  }
  (yyval.pTypedList)->next = (yyvsp[(2) - (2)].pTypedList);
}
    break;

  case 113:

/* Line 1806 of yacc.c  */
#line 1220 "scan-ops_pddl.y"
    { 
  opserr( ACTION, NULL ); 
}
    break;

  case 114:

/* Line 1806 of yacc.c  */
#line 1224 "scan-ops_pddl.y"
    {
  scur_op = new_PlOperator( (yyvsp[(4) - (4)].string) );
}
    break;

  case 115:

/* Line 1806 of yacc.c  */
#line 1228 "scan-ops_pddl.y"
    {
  scur_op->next = gloaded_ops;
  gloaded_ops = scur_op;
}
    break;

  case 117:

/* Line 1806 of yacc.c  */
#line 1238 "scan-ops_pddl.y"
    {
  TypedList *tl = NULL;

  /* add vars as parameters
   */
  if ( scur_op->parse_params ) {
    for( tl = scur_op->parse_params; tl->next; tl = tl->next ) {
      /* empty, get to the end of list
       */
    }
    tl->next = (yyvsp[(3) - (5)].pTypedList);
    tl = tl->next;
  } else {
    scur_op->parse_params = (yyvsp[(3) - (5)].pTypedList);
  }
}
    break;

  case 118:

/* Line 1806 of yacc.c  */
#line 1256 "scan-ops_pddl.y"
    {

  scur_op->duration = (yyvsp[(2) - (2)].pPlNode);

}
    break;

  case 120:

/* Line 1806 of yacc.c  */
#line 1264 "scan-ops_pddl.y"
    {
  scur_op->preconds = (yyvsp[(2) - (2)].pPlNode);
}
    break;

  case 122:

/* Line 1806 of yacc.c  */
#line 1270 "scan-ops_pddl.y"
    {
  scur_op->effects = (yyvsp[(2) - (2)].pPlNode);
}
    break;

  case 124:

/* Line 1806 of yacc.c  */
#line 1280 "scan-ops_pddl.y"
    {
  opserr(DERIVED_PRED_EXPECTED, NULL);
}
    break;

  case 125:

/* Line 1806 of yacc.c  */
#line 1284 "scan-ops_pddl.y"
    {
  der_op = new_PlOperator((yyvsp[(4) - (6)].pPlNode) -> atom -> item);
  der_op -> parse_params = (yyvsp[(4) - (6)].pPlNode) -> parse_vars;
  (yyvsp[(4) - (6)].pPlNode) -> parse_vars = NULL;
  der_op -> effects  = (yyvsp[(4) - (6)].pPlNode);
  der_op -> preconds = (yyvsp[(5) - (6)].pPlNode);
  der_op -> next = gderived_predicates;
  gderived_predicates = der_op;
  gnum_derived_predicates++;
}
    break;

  case 126:

/* Line 1806 of yacc.c  */
#line 1317 "scan-ops_pddl.y"
    {
  PlNode *pln;
  TokenList *a;
  TypedList *t;
  pln = new_PlNode(ATOM);
  pln -> atom = new_TokenList();
  pln -> atom -> item = (yyvsp[(2) - (4)].pstring);
  pln -> parse_vars = (yyvsp[(3) - (4)].pTypedList);
  for (a = pln -> atom, t = (yyvsp[(3) - (4)].pTypedList); t; t = t -> next) {
    a -> next = new_TokenList();
    a = a -> next;
    a -> item = (char *) calloc(strlen(t -> name) + 1 ,sizeof(char));
    strcpy(a -> item, t -> name);
  }

  (yyval.pPlNode) = pln;
}
    break;

  case 127:

/* Line 1806 of yacc.c  */
#line 1345 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = (yyvsp[(1) - (1)].pPlNode);
}
    break;

  case 128:

/* Line 1806 of yacc.c  */
#line 1350 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AND);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 129:

/* Line 1806 of yacc.c  */
#line 1358 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=(yyvsp[(1) - (1)].pPlNode);
}
    break;

  case 130:

/* Line 1806 of yacc.c  */
#line 1363 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = (yyvsp[(1) - (2)].pPlNode);
  (yyval.pPlNode)->next = (yyvsp[(2) - (2)].pPlNode);
}
    break;

  case 131:

/* Line 1806 of yacc.c  */
#line 1371 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AT_START_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 132:

/* Line 1806 of yacc.c  */
#line 1377 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AT_END_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 133:

/* Line 1806 of yacc.c  */
#line 1383 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(OVER_ALL_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 134:

/* Line 1806 of yacc.c  */
#line 1394 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = NULL;
}
    break;

  case 135:

/* Line 1806 of yacc.c  */
#line 1399 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = (yyvsp[(1) - (2)].pPlNode);
  (yyval.pPlNode)->next = (yyvsp[(2) - (2)].pPlNode);
}
    break;

  case 136:

/* Line 1806 of yacc.c  */
#line 1406 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AND);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 137:

/* Line 1806 of yacc.c  */
#line 1412 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = (yyvsp[(1) - (1)].pPlNode);
}
    break;

  case 138:

/* Line 1806 of yacc.c  */
#line 1420 "scan-ops_pddl.y"
    {
  PlNode *pln;

  pln = new_PlNode(ALL);
  pln->parse_vars = (yyvsp[(4) - (7)].pTypedList);

  (yyval.pPlNode) = pln;
  pln->sons = (yyvsp[(6) - (7)].pPlNode);
}
    break;

  case 139:

/* Line 1806 of yacc.c  */
#line 1432 "scan-ops_pddl.y"
    {
  /* This will be conditional effects in FF representation, but here
   * a formula like (WHEN p q) will be saved as:
   *  [WHEN]
   *  [sons]
   *   /  \
   * [p]  [q]
   * That means, the first son is p, and the second one is q.
   */
  (yyval.pPlNode) = new_PlNode(WHEN);
  (yyvsp[(3) - (5)].pPlNode)->next = (yyvsp[(4) - (5)].pPlNode);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (5)].pPlNode);
}
    break;

  case 140:

/* Line 1806 of yacc.c  */
#line 1447 "scan-ops_pddl.y"
    {
  PlNode* pln;
  (yyval.pPlNode)=(yyvsp[(2) - (5)].pPlNode);
  pln = new_PlNode(FN_HEAD);
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->atom = (yyvsp[(3) - (5)].pTokenList);
  (yyval.pPlNode)->sons->next = (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 141:

/* Line 1806 of yacc.c  */
#line 1472 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AT_START_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 142:

/* Line 1806 of yacc.c  */
#line 1478 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AT_END_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 143:

/* Line 1806 of yacc.c  */
#line 1484 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AT_START_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 144:

/* Line 1806 of yacc.c  */
#line 1490 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AT_END_CONN);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 145:

/* Line 1806 of yacc.c  */
#line 1496 "scan-ops_pddl.y"
    {
  PlNode *tmp;
  tmp = new_PlNode(INCREASE_CONN);
  tmp->sons = new_PlNode(FN_HEAD);
  tmp->sons->atom = (yyvsp[(3) - (5)].pTokenList);
  tmp->sons->next = (yyvsp[(4) - (5)].pPlNode);
  (yyval.pPlNode) = tmp;
}
    break;

  case 146:

/* Line 1806 of yacc.c  */
#line 1506 "scan-ops_pddl.y"
    {
  PlNode *tmp;
  tmp = new_PlNode(DECREASE_CONN);
  tmp->sons = new_PlNode(FN_HEAD);
  tmp->sons->atom = (yyvsp[(3) - (5)].pTokenList);
  tmp->sons->next = (yyvsp[(4) - (5)].pPlNode);
  (yyval.pPlNode) = tmp;
}
    break;

  case 147:

/* Line 1806 of yacc.c  */
#line 1518 "scan-ops_pddl.y"
    {

  PlNode* pln;
  (yyval.pPlNode)=(yyvsp[(2) - (5)].pPlNode);
  pln = new_PlNode(FN_HEAD);
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->atom = (yyvsp[(3) - (5)].pTokenList);
  (yyval.pPlNode)->sons->next = (yyvsp[(4) - (5)].pPlNode);

}
    break;

  case 148:

/* Line 1806 of yacc.c  */
#line 1533 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = (yyvsp[(2) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons->next = (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 149:

/* Line 1806 of yacc.c  */
#line 1540 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(UMINUS_CONN);
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 150:

/* Line 1806 of yacc.c  */
#line 1550 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(DURATION_VAR_ATOM);
  (yyval.pPlNode)->sons = pln;
}
    break;

  case 152:

/* Line 1806 of yacc.c  */
#line 1564 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(PLUS_CONN);
  (yyval.pPlNode)->sons = pln;

}
    break;

  case 153:

/* Line 1806 of yacc.c  */
#line 1574 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(MINUS_CONN);
  (yyval.pPlNode)->sons = pln;

}
    break;

  case 154:

/* Line 1806 of yacc.c  */
#line 1584 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(MUL_CONN);
  (yyval.pPlNode)->sons = pln;

}
    break;

  case 155:

/* Line 1806 of yacc.c  */
#line 1594 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(F_EXP);
  pln=new_PlNode(DIV_CONN);
  (yyval.pPlNode)->sons = pln;

}
    break;

  case 156:

/* Line 1806 of yacc.c  */
#line 1607 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode) = new_PlNode(F_EXP_T);
  pln = new_PlNode(MUL_CONN);
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->sons = (yyvsp[(3) - (5)].pPlNode);
  (yyval.pPlNode)->sons->sons->next = (yyvsp[(4) - (5)].pPlNode);  

}
    break;

  case 157:

/* Line 1806 of yacc.c  */
#line 1619 "scan-ops_pddl.y"
    {
  PlNode *pln, *pln2;

  (yyval.pPlNode) = new_PlNode(F_EXP_T);
  pln2 = new_PlNode(TIME_VAR);
  pln=new_PlNode(MUL_CONN); 
  (yyval.pPlNode)->sons = pln;
  (yyval.pPlNode)->sons->sons = pln2;
  (yyval.pPlNode)->sons->sons->next = (yyvsp[(4) - (5)].pPlNode);  

}
    break;

  case 158:

/* Line 1806 of yacc.c  */
#line 1632 "scan-ops_pddl.y"
    {
  PlNode *pln;

  pln = new_PlNode(TIME_VAR);
  (yyval.pPlNode) = pln;
}
    break;

  case 159:

/* Line 1806 of yacc.c  */
#line 1643 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AND);
  (yyval.pPlNode)->sons = (yyvsp[(3) - (4)].pPlNode);
}
    break;

  case 160:

/* Line 1806 of yacc.c  */
#line 1649 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AND);
  (yyval.pPlNode)->sons = (yyvsp[(1) - (1)].pPlNode);
}
    break;

  case 161:

/* Line 1806 of yacc.c  */
#line 1657 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = (yyvsp[(1) - (1)].pPlNode);
}
    break;

  case 162:

/* Line 1806 of yacc.c  */
#line 1662 "scan-ops_pddl.y"
    {
  (yyval.pPlNode)=(yyvsp[(1) - (2)].pPlNode);
  (yyval.pPlNode)->next = (yyvsp[(2) - (2)].pPlNode);
}
    break;

  case 163:

/* Line 1806 of yacc.c  */
#line 1674 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode) = (yyvsp[(2) - (5)].pPlNode);
  pln = new_PlNode(DURATION_VAR_ATOM);
  (yyval.pPlNode)->sons->sons = pln;
  (yyval.pPlNode)->sons->sons->next = (yyvsp[(4) - (5)].pPlNode);
}
    break;

  case 164:

/* Line 1806 of yacc.c  */
#line 1685 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AND);
}
    break;

  case 165:

/* Line 1806 of yacc.c  */
#line 1690 "scan-ops_pddl.y"
    {
  (yyval.pPlNode) = new_PlNode(AND);
}
    break;

  case 166:

/* Line 1806 of yacc.c  */
#line 1697 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(DURATION_CONSTRAINT_CONN);
  pln=new_PlNode(LESS_THAN_OR_EQUAL_CONN); 
  (yyval.pPlNode)->sons = pln;
}
    break;

  case 167:

/* Line 1806 of yacc.c  */
#line 1706 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(DURATION_CONSTRAINT_CONN);
  pln=new_PlNode(GREATER_OR_EQUAL_CONN); 
  (yyval.pPlNode)->sons = pln;
}
    break;

  case 168:

/* Line 1806 of yacc.c  */
#line 1715 "scan-ops_pddl.y"
    {
  PlNode *pln;

  (yyval.pPlNode)=new_PlNode(DURATION_CONSTRAINT_CONN);
  pln=new_PlNode(EQUAL_CONN); 
  (yyval.pPlNode)->sons = pln;
}
    break;

  case 171:

/* Line 1806 of yacc.c  */
#line 1732 "scan-ops_pddl.y"
    {
  (yyval.pstring) = new_Token(strlen((yyvsp[(2) - (2)].string)) + 1);
  strcpy((yyval.pstring), (yyvsp[(2) - (2)].string));
}
    break;



/* Line 1806 of yacc.c  */
#line 3616 "scan-ops_pddl.tab.c"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 2067 of yacc.c  */
#line 1742 "scan-ops_pddl.y"

#include "lex.ops_pddl.c"


/**********************************************************************
 * Functions
 **********************************************************************/



int yyerror( char *msg )

{

  if (msg)
    printf("\n%s", msg);

  fprintf(stderr, "\n%s: syntax error in line %d, '%s':\n", 
	  gact_filename, lineno, yytext);

  if ( NULL != sact_err_par ) {
    fprintf(stderr, "%s %s\n", serrmsg[sact_err], sact_err_par);
  } else {
    fprintf(stderr, "%s\n", serrmsg[sact_err]);
  }

  fflush(stdout);
  exit( 1 );

  return 0;

}



void load_ops_file( char *filename )

{

  FILE * fp;/* pointer to input files */
  char tmp[MAX_LENGTH] = "";

  gbracket_count = 0;

   
  /* open operator file 
   */
  if( ( fp = fopen( filename, "r" ) ) == NULL ) {
    sprintf(tmp, "\n Can't find operator file: %s\n\n", filename );
    perror(tmp);
    exit( 1 );
  }

  gact_filename = filename;
  lineno = 1; 
  yyin = fp;

  yyparse();

  fclose( fp );/* and close file again */

}



