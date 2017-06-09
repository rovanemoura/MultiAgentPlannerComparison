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
extern YYSTYPE yylval;
