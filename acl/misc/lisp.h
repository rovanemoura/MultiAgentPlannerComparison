/*
 * copyright (c) 1985 Franz Inc, Alameda, Ca. 
 * copyright (c) 1986-2002 Franz Inc, Berkeley, CA
 * copyright (c) 2002-2015 Franz Inc, Oakland, CA - All rights reserved.
 *
 * The software, data and information contained herein are proprietary
 * to, and comprise valuable trade secrets of, Franz, Inc.  They are
 * given in confidence by Franz, Inc. pursuant to a written license
 * agreement, and may be stored and used only in accordance with the terms
 * of such license.
 *
 * Restricted Rights Legend
 * ------------------------
 * Use, duplication, and disclosure of the software, data and information
 * contained herein by any agency, department or entity of the U.S.
 * Government are subject to restrictions of Restricted Rights for
 * Commercial Software developed at private expense as specified in 
 * DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
 *
 * $Id: lisp.h,v 1.58 2008/07/09 00:52:43 duane Exp $
 *
 * Note that these macros are subject to change in future releases.
 * These macros are tested against our regression test suite, but they
 * apply only to the version of Allegro Common Lisp with which this file
 *  was released.
 *
 * Franz Inc. will endeavor to supply versions of these macros for
 * future releases, but does not guarantee all macros will exist in
 * such future releases.
 *
 */

/*
** All lisp data types are tagged using the lower 3 or 4 bits as a tag value
** Data types fall into two classes:
**  Immediate, where the data is encoded in a 4/8 byte object
**   (these are data types of fixnum and character).
**  Non-immediate, where the lisp value points at the data object
**   (these are all other data types).
**
** The non-immediate data types are also tagged using the lower 3 or 4 bits.
** and the pointer points somewhere to within the first 8 bytes of any
** such Lisp object.
**
** There is a non-immediate data tag of "other"; in this case the first
** byte of the Lisp object will encode the type of the object
**
** Note that not all data types are represented by C macros
**
*********************************************************************
*** All non-immediate Lisp values in C should be declared LispVal **
*********************************************************************
*/

#if !defined(_INCLUDE_LISP_H_)
#define _INCLUDE_LISP_H_

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

#if defined(ACL_MAJ_VER)
#define COMPILING_ACL
#endif

#if defined(COMPILING_ACL)
#define DLL_EXPORT_OR_IMPORT __declspec(dllexport)
#else
#define DLL_EXPORT_OR_IMPORT __declspec(dllimport)
#endif

#if defined(_WIN32)
#define ACL_API DLL_EXPORT_OR_IMPORT
#if defined(_X86_) || defined(_M_IX86)
#define __i386__ 1
#else
#define __alpha
#define Acl32Bit
#endif
#elif defined(WIN64)
#define ACL_API DLL_EXPORT_OR_IMPORT
#else 
#define ACL_API
#endif

#if defined(__x86_64__)
#ifndef Acl64Bit
#define Acl64Bit
#endif
#define Little64Bit 1
#endif

/*
#ifdef __alpha
#ifdef Acl32Bit
# pragma pointer_size (short)
#else
#ifdef Acl64Bit
#define Little64Bit 1
#else
Error: define one of Acl32Bit or Acl64Bit
#endif
#endif
#endif
*/

#if defined(__APPLE__)
#define MacOSX
#if defined(__ppc__)
#define MacOSX32
#endif
#if defined(__ppc64__)
#define MacOSX64
#endif
#if defined(__i386__)
#define MacOSX86
#ifndef Acl32Bit
#define Acl32Bit
#endif
#endif
#endif

#ifdef __IPP3__
#define ACL32Bit
#endif

#if defined(__hpux)||defined(sparc)||defined(_IBMR2)||defined(MacOSX32)||defined(MacOSX64)
#ifndef Acl32Bit
#ifdef Acl64Bit
# define Big64Bit 1
#else
Error: define one of Acl32Bit or Acl64Bit
#endif
#endif
#endif

/* A "natural" is an integer in the native width of the lisp (and
 * not necessarily the machine).  A 64-bit lisp has an 8-byte natural,
 * and a 32-bit lisp has a 4-byte natural.
 */
#ifndef nat
#ifdef Acl64Bit
#if defined(_M_AMD64)
#define nat __int64
#define NAT_SPEC "%I64d"
#define UNAT_SPEC "%I64u"
#else
#define nat long
#define NAT_SPEC "%ld"
#define UNAT_SPEC "%lu"
#endif
#else
#define nat int
#define NAT_SPEC "%d"
#define UNAT_SPEC "%u"
#endif
#endif

#ifdef Acl64Bit
#define Fixnumshift 3
#define Tagshift 4
#define Natwidth 8
#else /* 32 bit */
#define Fixnumshift 2
#define Tagshift 3
#define Natwidth 4
#endif


typedef char * LispVal;
#ifdef Acl64Bit
#define GetTag(v) ((nat)v & 15)
#else
#define GetTag(v) ((nat)v & 7)
#endif


/* the basic tags */
#if defined(__hpux)
/* HP reverses some tags from others, so as to generate quicker typechecks */
#if defined(Acl64Bit)
# define FixnumType 0
# define OtherType  1
# define ListType   2
# define CharType   5
# define OddFixnumType 8
# define NilType    10
# define SymbolType 11
#else
# define HP32Bit 1
# define FixnumType 0
# define OtherType  1
# define ListType   2
# define OddFixnumType 4
# define CharType   5
# define NilType    6
# define SymbolType 7
#endif
#else
#if defined(Acl64Bit)
/* 64 bit lisps other than HP */
# define FixnumType 0
# define ListType   1
# define OtherType  2
# define OddFixnumType 8
# define CharType   6
# define NilType    9
# define SymbolType 11
#else
/* all others */
#if defined(_IBMR2)
#define IBM32Bit 1
#endif
# define FixnumType 0
# define ListType   1
# define OtherType  2
# define OddFixnumType 4
# define NilType    5
# define CharType   6
# define SymbolType 7
#endif
#endif

   
/*
** fixnum data type
**
** convert a fixnum to a machine integer
*/
#define FixnumToNat(v) ((nat)(v) >> Fixnumshift)
/* Obsolete: */
#define FixnumToInt(v) FixnumToNat(v)

/*
** convert a machine integer to a fixnum
*/
#define NatToFixnum(v) ((LispVal)((nat)(v) << Fixnumshift))
/* Obsolete: */
#define IntToFixnum(v) NatToFixnum(v)


#if defined(__i386__) || defined(__x86_64__) || defined(WIN64) || defined(__AM33_2__)
#define TagOffset (-16)
#else
#if defined(m88k)||defined(IBM32Bit)||defined(HP32Bit)||defined(MacOSX32)||defined(__IPP3__)
#define TagOffset 8
#else
#define TagOffset 0
#endif
#endif

#define OtherTypeOfs (OtherType-TagOffset)

/*
** get the type of an "other" object
*/
#define GetOType(v)  (unsigned nat)(*(unsigned char *)((v)-OtherTypeOfs))

#define GetType(v) (( GetTag(v) == OtherType )? GetOType(v) : GetTag(v))


#define SymbolTypeOfs (SymbolType-TagOffset)
/*
** symbol data type; tag is 111 or 1011
** return the value of a symbol
*/

struct Symbol {
    char dtype;              /* nat word 0 */
    char flags;
    short package;
#ifdef Acl64Bit
    int fill1;
#endif
    LispVal value;           /* nat word 1 */
    char pflags;             /* nat word 2 */
#ifdef Acl64Bit
    unsigned int fill2:24;
    unsigned int hash_val;
#else
    unsigned int hash_val:24;
#endif
    LispVal function;        /* nat word 3 */
    LispVal name;            /* nat word 4 */
    LispVal plist;           /* nat word 5 */
    LispVal svvector;        /* nat word 6 */
    LispVal lock_ix;         /* nat word 7 */
};

struct Symbol_Locative {
    char dtype;              /* nat word 0 */
    char flags;
    short package;
#ifdef Acl64Bit
    int fill1;
#endif
    LispVal value;           /* nat word 1 */
    LispVal symbol;          /* nat word 2 */
    LispVal function;        /* nat word 3 */
};

EXTERN ACL_API LispVal *symbol_locative(LispVal);

#define SymbolFlags(s)		(*(char *)((s)-SymbolTypeOfs+1))
#define SymbolPackage(s)	(*(short *)((s)-SymbolTypeOfs+2))
/* Note: this macro only grabs the global value */
#define GlobalSymbolValue(s)		(Vecdata(*(LispVal *)((s)-SymbolTypeOfs+Natwidth))[0])
#define SymbolValue(s)		(*symbol_locative(s))
#ifdef Acl64Bit
#define SymbolHashV(s)		(*(unsigned int *)((s)-SymbolTypeOfs+5*Natwidth/2))
#else
#define SymbolHashV(s)		((struct Symbol *)((s)-SymbolTypeOfs)->hash_val)
#endif
#define SymbolFunct(s)		(*(LispVal *)((s)-SymbolTypeOfs+3*Natwidth))
#define SymbolName(s)		(*(LispVal *)((s)-SymbolTypeOfs+4*Natwidth))
#define SymbolPlist(s)		(*(LispVal *)((s)-SymbolTypeOfs+5*Natwidth))
#define SymbolConstant(s)	(SymbolFlags(s) & 0x80)

#define LocativeSymbol(s)	(*(LispVal *)((s)-SymbolTypeOfs+2*Natwidth))

#if defined(Acl64Bit) || defined(__AM33_2__)
#define HighTypeCodes 1
#endif

/*
 * function data type
 */
#ifdef HighTypeCodes
#define FunctionType 136
#define ClosureType 137
#else
#define FunctionType 8
#define ClosureType 9
#endif

struct Function {
    char dtype;             /* nat word 0 */
    char flags;
#ifdef Big64Bit
    short fill1;
    short fill2;
#endif
    short size;
#ifdef Little64Bit
    int fill1;
#endif
    nat start;              /* nat word 1 */
    char xflags;            /* nat word 2 */
#ifdef Acl64Bit
#ifndef WIN64
    unsigned int fill3:24;
#endif
    unsigned int hash_val;
#else
    unsigned int hash_val:24;
#endif
    LispVal name;           /* nat word 3 */
    LispVal code;           /* nat word 4 */
    LispVal formals;
    LispVal cframe;
    LispVal callcount;
    LispVal locals;
    LispVal const0; /* first constant */
};

#ifdef Acl64Bit
#define FunctionHashV(s)	(*(unsigned int *)((s)-OtherTypeOfs+5*Natwidth/2))
#else
#define FunctionHashV(s)	(((struct Function *)((s)-OtherTypeOfs))->hash_val)
#endif

/*
** character data type
** for most ports:
**    0000 0fff ffff fbbb bbbb bccc cccc cttt
** for ICS:
**    000f ffff bbbb bccc cccc cccc cccc cttt
** where b are the bits, f are the font, and c is the standard character.
** ttt is the tag.
** convert a standard-char to a machine byte
*/
#define ScharToChar(v) ((char)( (v) >> Tagshift))
#define CharToSchar(v) ((LispVal)((((int)(v)) << Tagshift) | CharType))

/*
** nil is special as it is both a symbol and a list
** In particular, the symbol accessor macros won't work right.
** Check on the nilness of a pointer.
*/
#define Null(p) ((p) == nilval)

EXTERN ACL_API LispVal nilval;
EXTERN ACL_API LispVal tval;

/*
** cons data type
** we store it as a car, then cdr get the cdr of a list;
** the result is a pointer to the cdr of the next cons cell
*/

#define ListTypeOfs (ListType-TagOffset)
#define ListCdrOfs Natwidth

/*
** get the cdr of a list; the result is the lisp value
*/
#define Cdr(x) (*(LispVal*)((LispVal)(x) - ListTypeOfs + ListCdrOfs))

/*
** get the car of a list; the result is the lisp value
*/
#define Car(x) (*(LispVal*)((LispVal)(x) - ListTypeOfs))

struct List {
    LispVal car;
    LispVal cdr;
};

/*
** Arrays: these are of type "other"
** All arrays have a certain bit (bit 6 in this Lisp) set in the type.
** Arrays a subtype of both simple-array and vector have bit 5 set also
*/
#define Array_bit 64
#define Vec_bit 32

/*
** There are two structures used for arrays.
**	fixed size vector: for types a subtype of both simple-array and vector
**	array: everything else
**
** vectors of fixed length; have tag type "other"
**  byte 0: type of the vector
**  byte 1,2,3: size of the vector
**  bytes 4 & on: the data, C style.
**
** arrays have tag type "other"
**  byte 0: type of the vector
**  byte 1: unused
**  bytes 2, 3: number of dimensions (rank)
**  bytes 4-7: fill-pointer-value (number of entries as a fixnum).
**  bytes 8-11: data field (a pointer)
**   1. if array is not a displaced array:
**	pointer -> cons cell whose cdr -> fixed size vector
**   2. if array is displaced:
**	pointer may point to either to another array or to a cons cell
**      whose cdr -> fixed size vector
**  bytes 12-15: displacement (fixnum to add before following
**  bytes 16-19: 	       list of fixnum dimensions).
**  bytes 20-23: Array flags - this is a fixnum 
*/

#if (vax || m_sequent || __i386__ || m_isc386 || MIPSEL) || defined(__alpha) || defined(__x86_64__) || defined(__AM33_2__)
#define A_rank(s) (*((short *)((s) - OtherTypeOfs + 2)) & 0xffff)
#else
#if defined(Big64Bit)
#define A_rank(s) ((*((nat *)((s) - OtherTypeOfs)) >> 32) & 0xffff)
#else
#define A_rank(s) (*((nat *)((s) - OtherTypeOfs)) & 0xffff)
#endif /*  */
#endif /* vax or m_sequent or i386 or sun386i or dec3100 */

#define A_flags(s) ((nat)(*(char*)((s) - OtherTypeOfs + 1)))
#define A_fillp(s) (*(nat *)((s) - OtherTypeOfs + Natwidth))
#define A_dataptr(s) (*(LispVal*)((s) - OtherTypeOfs + 2*Natwidth))
#define A_disp(s) (*(nat *)((s) - OtherTypeOfs + 3*Natwidth))
#define A_dims(s)  (*(LispVal*)((s) - OtherTypeOfs + 4*Natwidth))
#define A_hash(s) FixnumToNat(*(nat *)((s) - OtherTypeOfs + 5*Natwidth))

#define A_flag_adj 1	/* bit 0 set if array adjustable */
#define A_flag_fill 2	/* bit 1 set if array has a fill pointer */
#define A_flag_disp 4	/* bit 2 set if displaced to another array */

struct Array {
    unsigned char dtype;
    unsigned int flags:8;
    unsigned int rank:16;
#ifdef Acl64Bit
    int fill1;
#endif
    LispVal fillp;
    LispVal dataptr;
    nat disp;
    LispVal dims;
    nat hash;
};


/*
** and the array types
*/
#define A_arrayType 64		/* (array t) */
#define A_bitType 65		/* (array bit) */
#define A_ubyteType 66		/* (array (mod 2^8)) */
#define A_uwordType 67		/* (array (mod 2^16)) */
#define A_ulongType 68		/* (array (mod 2^32)) */
#define A_charType 69		/* (array string-char) == string */
#define A_floatType 70		/* (array single-float) */
#define A_doubleType 71		/* (array double-float) */
#define A_byteType 72		/* (array (signed-byte 8)) */
#define A_wordType 73		/* (array (signed-byte 16)) */
#define A_nybbleType 74		/* (array (unsigned-byte 4)) */
#define A_longType 75		/* (array (unsigned-byte 32)) */
#define A_fcomplexType 77		/* (array (complex single-float)) */
#define A_dcomplexType 78		/* (array (complex double-float)) */
#define A_uquadType 192		/* (array (mod 2^64)) */
#define A_quadType 193		/* (array (signed-byte 64)) */

/*
 * There are now two kinds of vectors.  The longer kind follow the CL
 * type system, and the "short" vectors are what were previously
 * the CL types, and many are retained for backward compatibility,
 * in case some programs assume the shape of vectors.
 * The names, however, are different; short vectors used to be named
 * with "Vec..." and "V_..." and are now named with "SVec..." and "SV_..."
 *
 */

/*
** return a pointer to the first byte in a fixed size vector
*/
#define sv_data0 (Natwidth - OtherTypeOfs)
#define lv_data0 ((2*Natwidth) - OtherTypeOfs)
#define SVecdata(s) ((LispVal*)((s) + sv_data0))
#define Vecdata(s) ((LispVal*)((s) + lv_data0))

/*
** return the size of a vector
*/

#define sv_size_adj (- OtherTypeOfs)
#define lv_size_adj (Natwidth - OtherTypeOfs)

#if (vax || m_sequent || i386 || m_isc386 || MIPSEL) || defined(__alpha) || defined(__x86_64__) || defined(__AM33_2__)
#define SVecSize(s) ((*(unsigned nat *)((s)+sv_size_adj))>>8)
#else
#define SVecSize(s) ((0xffffff & (*(nat *)((s)+sv_size_adj))))
#endif

#define VecSize(s) FixnumToNat(*(nat*)((s)+lv_size_adj))

struct SVector {
    nat size; 			/* includes type */
    LispVal data;
};


struct Vector {
    char type;
#ifdef Acl64Bit
    unsigned int fill:24;
    unsigned int hash_val;
#else
    unsigned int hash_val:24;
#endif
    LispVal size;		/* A fixnum */
    LispVal data;
};

/*
** a pointer to the first member of a short-simple-vector == (short-simple-array t (*))
*/
#define SV_svecType 96
#define SV_svec(s) SVecdata(s)

/*
** return a pointer to the first member of a vector of bits
** (short-simple-array bit (*))
*/
#define SV_bitType 97
#define SV_bit(s)  SVecdata(s)

/*
** return a pointer to the first member of a vector of unsigned bytes
** (short-simple-array (mod 2^8) (*))
*/
#define SV_ubyteType 98
#define SV_ubyte(s)  SVecdata(s)

/*
** return a pointer to the first member of a vector of unsigned shorts
** (short-simple-array (mod 2^16) (*))
*/
#define SV_uwordType 99
#define SV_uword(s)  SVecdata(s)

/*
** return a pointer to the first member of a vector of unsigned longss
** (short-simple-array (mod 2^32) (*))
*/
#define SV_ulongType 100
#define SV_ulong(s)  SVecdata(s)

/*
** return a pointer to a string
** (short-simple-array standard-char (*)) == (short-simple-string (*))
*/
#define SV_charType 101
#define SV_char(s)  SVecdata(s)

/*
** return a pointer to the first member of a vector of floats
** (short-simple-array single-float (*))
*/
#define SV_floatType 102
#define SV_float(s) ((float *)( SVecdata(s)))

/*
** return a pointer to the first member of a vector of doubles
** (short-simple-array double-float (*))
*/
#define SV_doubleType 103
#ifdef Acl64Bit
#define SV_double(s) ((double *)( SVecdata(s)))
#else
#define SV_double(s) ((double *)( SVecdata(s) + 4))
#endif

/*
** return a pointer to the first member of a vector of signed bytes
** (short-simple-array (signed-byte 8) (*))
*/
#define SV_byteType 104
#define SV_byte(s)  SVecdata(s)

/*
** return a pointer to the first member of a vector of signed shorts
** (short-simple-array (signed-byte 16) (*))
*/
#define SV_wordType 105
#define SV_word(s)  SVecdata(s)

/*
** return a pointer to the first member of a vector of nybbles
** (short-simple-array (unsigned-byte 4) (*)))
*/
#define SV_nybbleType 106
#define SV_nybble(s) ((nat *)( SVecdata(s)))

/*
** return a pointer to the first member of a vector of signed longs
** (short-simple-array (signed-byte 32) (*))
*/
#define SV_longType 107
#define SV_long(s)  SVecdata(s)

/*
** return a pointer to the first member of a vector of float complexes
** (short-simple-array (complex single-float) (*))
*/
#define SV_fcomplexType 109
#define SV_fcomplex(s) ((float *)( SVecdata(s)))

/*
** return a pointer to the first member of a vector of double complexes
** (short-simple-array (complex double-float) (*))
*/
#define SV_dcomplexType 110
#ifdef Acl64Bit
#define SV_dcomplex(s) ((double *)( SVecdata(s) ))
#else
#define SV_dcomplex(s) ((double *)( SVecdata(s) + 4))
#endif

#ifdef Acl64Bit
/*
** return a pointer to the first member of a vector of unsigned quads
** (short-simple-array (mod 2^64) (*))
*/
#define SV_uquadType 192
#define SV_uquad(s)  SVecdata(s)

/*
** return a pointer to the first member of a vector of signed longs
** (short-simple-array (signed-byte 64) (*))
*/
#define SV_quadType 193
#define SV_quad(s)  SVecdata(s)

#endif

/*
 * The new (long) vector types. In general, these have their size
 * stored as a fixnum in what would be the first data slot in a
 * short-vector.  The new array-dimension-limit is most-positive-fixnum.
 * The fixnum array has returned, which had been displaced by the nybble
 * array at around version 5.0.  However, fixnums are now stored in a
 * fixnum array without any shifting, for speed.
 */

/*
** a pointer to the first member of a simple-vector == (simple-array t (*))
*/
#define V_svecType 112
#define V_svec(s) Vecdata(s)

/*
** return a pointer to the first member of a vector of floats
** (simple-array single-float (*))
*/
#define V_floatType 113
#define V_float(s) ((float *)( Vecdata(s)))

/*
** return a pointer to the first member of a vector of doubles
** (simple-array double-float (*))
*/
#define V_doubleType 114
#define V_double(s) ((double *)( Vecdata(s)))

/*
** return a pointer to the first member of a vector of float complexes
** (simple-array (complex single-float) (*))
*/
#define V_fcomplexType 115
#define V_fcomplex(s) ((float *)( Vecdata(s)))

/*
** return a pointer to the first member of a vector of double complexes
** (simple-array (complex double-float) (*))
*/
#define V_dcomplexType 116
#define V_dcomplex(s) ((double *)( Vecdata(s) ))

/*
** return a pointer to a string
** (simple-array standard-char (*)) == (simple-string (*))
*/
#define V_charType 117
#define V_char(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of bits
** (simple-array bit (*))
*/
#define V_bitType 118
#define V_bit(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of fixnums
** (simple-array fixnum (*))
*/
#define V_fixnumType 120
#define V_fixnum(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of signed bytes
** (simple-array (signed-byte 8) (*))
*/
#define V_byteType 121
#define V_byte(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of signed shorts
** (simple-array (signed-byte 16) (*))
*/
#define V_wordType 122
#define V_word(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of signed longs
** (simple-array (signed-byte 32) (*))
*/
#define V_longType 123
#define V_long(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of nybbles
** (simple-array (unsigned-byte 4) (*)))
*/
#define V_nybbleType 124
#define V_nybble(s) ((nat *)( Vecdata(s)))

/*
** return a pointer to the first member of a vector of unsigned bytes
** (simple-array (mod 2^8) (*))
*/
#define V_ubyteType 125
#define V_ubyte(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of unsigned shorts
** (simple-array (mod 2^16) (*))
*/
#define V_uwordType 126
#define V_uword(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of unsigned longs
** (simple-array (mod 2^32) (*))
*/
#define V_ulongType 127
#define V_ulong(s)  Vecdata(s)

#ifdef Acl64Bit
/*
** return a pointer to the first member of a vector of unsigned quads
** (simple-array (mod 2^64) (*))
*/
#define V_uquadType 240
#define V_uquad(s)  Vecdata(s)

/*
** return a pointer to the first member of a vector of signed longs
** (simple-array (signed-byte 64) (*))
*/
#define V_quadType 241
#define V_quad(s)  Vecdata(s)

#endif

/*
** Some other type codes
*/
#define CodeVecType 108		/* a vector of function code	*/
#define InstanceType 128	/* type code for a flavor instance */
#ifdef HighTypeCodes
#define StandardInstanceType 140 /* type code of CLOS standard-instance */
#define StructType 143		/* type code for a structure-object */
#else
#define StandardInstanceType 12	/* type code of CLOS standard-instance */
#define StructType 15		/* type code for a structure-object */
#endif

/* A standard-instance object is representationally similar to a short simple
   vector, although the length is always 4 words and the length field
   is used for other purposes. */

struct StandardInstance {
    nat dtype;
    LispVal wrapper;		/* A simple-vector identifying the class.
				   See below. */
    LispVal instance_slots;	/* A simple-vector holding the instance slots.
				   The ordering of slots is typically the
				   reverse of the order returned by
				   clos:class-slots on the class, but this can
				   be overridden by the MOP. */
    nat unused;			/* Used by foreign function code. */
};

#define WrapperClassSlotsIndex 3 /* The index of the class-slots vector object
				    in the wrapper.  If non null, this is a
				    simple-vector, each element of which is a
				    cons of the symbol naming the slot and the
				    slot value. */
#define WrapperClassIndex 4	/* The index of the class-object in the wrapper. */

/*
 *  Numeric types
 */
#define S_FloatType 16	/* single-float */
#define S_FtoFloat(v)  (* (float *)((v) - OtherTypeOfs + Natwidth))

struct Float {
    nat dtype;
    float data;
};


#define D_FloatType 17	/* double-float */

#ifdef Acl64Bit
# define D_FtoFloat(v)  (* (double *)((v) - OtherTypeOfs + Natwidth))
#else
# define D_FtoFloat(v)  (* (double *)((v) - OtherTypeOfs + Natwidth + Natwidth))
#endif

struct Double {
    nat dtype;
#ifndef Acl64Bit
    int unused;
#endif
    double data;
};

#define BignumType 18	/* bignum */

#define RatioType 19	/* ratio - numerator and denominator Lisp integers */
/* should be C int or bignum pointer */
#define R_num(v)  (*(LispVal*)((v) - OtherTypeOfs + Natwidth))
#define R_den(v)  (*(LispVal*)((v) - OtherTypeOfs + 2*Natwidth))

struct Ratio {
    nat dtype;
    LispVal num;
    LispVal den;
};

#define ComplexType 20	/* complex - real and imaginary parts */
#define C_real(v)  (*(LispVal*)((v) - OtherTypeOfs + Natwidth))
#define C_imag(v)  (*(LispVal*)((v) - OtherTypeOfs + 2*Natwidth))

#ifdef Complex
/* if you get an error on this, then change the name of the structure below */
float readme = "someone defined ``Complex'' which clashes with the following";
#endif /* Complex */

struct Complex {
    nat dtype;
    LispVal num;
    LispVal den;
};

/* definitions for lisp main building ... */

struct shlib_library_item {
    char *	name;
    int		system;
};

#if defined(_WIN32) || defined(WIN64) || defined(__hpux)
#  define LispInit_signature(xxx)			 \
  void xxx(int,char **,char **,				 \
	 void (*)(int),					 \
	 char *, char *,void *,char *,nat(*)(),int,	 \
	 struct shlib_library_item **, void **)
#else
# define LispInit_signature(x)				 \
  void x (int,char **,char **,				 \
	  void (*)(),					 \
	  char *, char *,void *,char *,nat(*)(),int,	 \
	  struct shlib_library_item **, void **)
#endif

typedef LispInit_signature((*LispInit));

typedef nat (*FindSym)(void *, char *);

#endif
