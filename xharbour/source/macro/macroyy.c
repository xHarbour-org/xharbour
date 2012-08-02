/*
 * $Id$
 */
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
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
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         hb_compparse
#define yylex           hb_complex
#define yyerror         hb_comperror
#define yylval          hb_complval
#define yychar          hb_compchar
#define yydebug         hb_compdebug
#define yynerrs         hb_compnerrs


/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
// #line 2 "source\\macro\\macro.y"

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Macro compiler YACC rules and actions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* TODO list
 * 1) jumps longer then 2^15 bytes
 * 2) Change the pcode generated by ::cVar from Self:cVar to QSELF():cVar
 * 3) Support this syntax: nPtr := @Hello()
 */

/* this #define HAVE TO be placed before all #include directives
 */
#define  HB_MACRO_SUPPORT

#include "hbmacro.h"
#include "hbcomp.h"

//JC1: yylex is not threadsafe, we need mutexes
#include "hbstack.h"    // that also includes thread.h

/* AJ: Remove quallification of thread.h inclusion in order to make this file
   useable in both ST and MT modes */
/* #ifdef HB_THREAD_SUPPORT */
  #include "thread.h"
/* #endif */

/* Compile using: bison -d -p hb_comp macro.y */

/* to pacify some warnings in BCC */
#if defined( __BORLANDC__ ) && !defined( __STDC__ )
#  define __STDC__
#endif

#if defined( __POCC__ )
   #pragma warn(push)
   #pragma warn(disable:2154)
#endif

#if defined( __BORLANDC__ )
   #pragma warn -aus
   #pragma warn -ccc
   #pragma warn -rch
#endif

/* AJ: added functions to enter and leave critical section made common to ST
   and MT mode
*/
HB_EXTERN_BEGIN
extern void hb_macro_critical_Lock( void );
extern void hb_macro_critical_UnLock( void );
HB_EXTERN_END

/* NOTE: these symbols are used internally in bison.simple
 */
#ifndef hb_xgrab
   #undef alloca
   #define alloca  hb_xgrab
   #undef malloc
   #define malloc  hb_xgrab
#endif
#ifndef hb_xfree
   #undef free
   #define free hb_xfree
#endif

#define HB_MAX_PENDING_MACRO_EXP 16

static HB_EXPR_PTR s_Pending[ HB_MAX_PENDING_MACRO_EXP ];
static int s_iPending;

/* This is workaround of yyparse() declaration bug in bison.simple
*/

#if !defined(__GNUC__) && !defined(__IBMCPP__)
   #if 0
      /* This makes BCC 551 fail with Bison 1.30, even with the
         supplied harbour.simple file, which makes Bison 1.30 blow.
         [vszakats] */
      void __yy_memcpy ( char*, const char*, unsigned int ); /* to satisfy Borland compiler */
   #endif
#endif

/* yacc/lex related definitions
 */
#undef YYPARSE_PARAM
#define YYPARSE_PARAM HB_MACRO_PARAM    /* parameter passed to yyparse function - it have to be of 'void *' type */
#undef YYLEX_PARAM
#define YYLEX_PARAM   ( (HB_MACRO_PTR)YYPARSE_PARAM ) /* additional parameter passed to yylex */

extern int yyparse( void * );   /* to make happy some purist compiler */

extern void yyerror( char * ); /* parsing error management function */

/* Standard checking for valid expression creation
 */
#define  HB_MACRO_CHECK( pExpr ) \
   if( ! ( HB_MACRO_DATA->status & HB_MACRO_CONT ) ) \
   { \
      hb_compExprDelete( pExpr, HB_MACRO_PARAM ); \
      YYABORT; \
   }

#define HB_MACRO_IFENABLED( pSet, pExpr, flag ) \
   if( HB_MACRO_DATA->supported & (flag) ) \
     { pSet = (pExpr); }\
   else \
   { \
      hb_compExprDelete( (pExpr), HB_MACRO_PARAM ); \
      YYABORT; \
   }

#if defined( __BORLANDC__ ) || defined( __WATCOMC__ )
/* The if() inside this macro is always TRUE but it's used to hide BCC warning */
#define HB_MACRO_ABORT if( !( HB_MACRO_DATA->status & HB_MACRO_CONT ) ) { YYABORT; }
#else
#define HB_MACRO_ABORT { YYABORT; }
#endif


/* Line 189 of yacc.c  */
// #line 250 "obj\\b32\\macroy.c"

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
     IDENTIFIER = 258,
     NIL = 259,
     NUM_DOUBLE = 260,
     INASSIGN = 261,
     NUM_LONG = 262,
     IIF = 263,
     IF = 264,
     LITERAL = 265,
     TRUEVALUE = 266,
     FALSEVALUE = 267,
     AND = 268,
     OR = 269,
     NOT = 270,
     EQ = 271,
     NE1 = 272,
     NE2 = 273,
     INC = 274,
     DEC = 275,
     ALIASOP = 276,
     HASHOP = 277,
     SELF = 278,
     LE = 279,
     GE = 280,
     FIELD = 281,
     MACROVAR = 282,
     MACROTEXT = 283,
     H12AM = 284,
     H12PM = 285,
     PLUSEQ = 286,
     MINUSEQ = 287,
     MULTEQ = 288,
     DIVEQ = 289,
     POWER = 290,
     EXPEQ = 291,
     MODEQ = 292,
     CBMARKER = 293,
     BITAND = 294,
     BITOR = 295,
     BITXOR = 296,
     BITSHIFTR = 297,
     BITSHIFTL = 298,
     POST = 299,
     MATCH = 300,
     LIKE = 301,
     UNARY = 302,
     PRE = 303
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
// #line 171 "source\\macro\\macro.y"

   char *    string;       /* to hold a string returned by lex */
   int       iNumber;      /* to hold a temporary integer number */
   HB_LONG   lNumber;      /* to hold a temporary long number */
   struct
   {
      char *   string;
      int      length;
   } valChar;
   struct
   {
      int    iNumber;      /* to hold a number returned by lex */
      char * szValue;
   } valInteger;
   struct
   {
      HB_LONG   lNumber;   /* to hold a long number returned by lex */
      char *    szValue;
   } valLong;
   struct
   {
      double dNumber;   /* to hold a double number returned by lex */
      /* NOTE: Intentionally using "unsigned char" instead of "BYTE" */
      unsigned char bWidth; /* to hold the width of the value */
      unsigned char bDec; /* to hold the number of decimal points in the value */
      char * szValue;
   } valDouble;
   HB_EXPR_PTR asExpr;
   void * pVoid;        /* to hold any memory structure we may need */



/* Line 214 of yacc.c  */
// #line 367 "obj\\b32\\macroy.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */

/* Line 264 of yacc.c  */
// #line 202 "source\\macro\\macro.y"

/* This must be placed after the above union - the union is
 * typedef-ined to YYSTYPE
 */
int yylex( YYSTYPE *, HB_MACRO_PTR );

/* Line 264 of yacc.c  */
// #line 209 "source\\macro\\macro.y"

#ifdef __WATCOMC__
/* disable warnings for unreachable code */
#pragma warning 13 9
#endif


/* Line 264 of yacc.c  */
// #line 395 "obj\\b32\\macroy.c"

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
# if YYENABLE_NLS
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
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
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
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
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

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  110
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1484

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  70
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  75
/* YYNRULES -- Number of rules.  */
#define YYNRULES  252
/* YYNRULES -- Number of states.  */
#define YYNSTATES  377

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   303

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      61,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    48,    55,    58,     2,
      66,    67,    53,    51,    60,    52,    65,    54,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    64,     2,
      46,    45,    47,     2,    59,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    69,     2,    68,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    62,     2,    63,     2,     2,     2,     2,
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
      49,    50,    56,    57
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,     8,    10,    12,    15,    18,    20,
      22,    24,    26,    29,    31,    33,    35,    37,    39,    48,
      62,    74,    89,   102,   117,   130,   134,   138,   140,   144,
     150,   153,   155,   158,   160,   162,   165,   168,   171,   174,
     178,   181,   184,   187,   190,   193,   195,   197,   200,   203,
     206,   209,   212,   215,   218,   221,   224,   227,   230,   233,
     236,   239,   242,   245,   248,   251,   254,   257,   260,   263,
     266,   269,   272,   275,   278,   281,   284,   287,   290,   293,
     297,   302,   308,   313,   317,   319,   323,   326,   328,   331,
     333,   335,   337,   341,   344,   349,   354,   356,   358,   360,
     362,   364,   366,   368,   370,   372,   374,   376,   378,   380,
     382,   384,   386,   388,   390,   392,   394,   396,   398,   400,
     402,   404,   406,   408,   410,   412,   414,   416,   418,   420,
     421,   426,   428,   432,   433,   435,   437,   439,   441,   443,
     445,   447,   449,   451,   453,   455,   457,   459,   461,   463,
     465,   467,   469,   471,   473,   475,   477,   479,   481,   483,
     485,   487,   490,   493,   496,   499,   502,   505,   509,   513,
     517,   521,   525,   529,   533,   537,   541,   545,   549,   553,
     557,   561,   565,   569,   573,   577,   581,   585,   589,   593,
     597,   601,   605,   609,   613,   617,   621,   625,   627,   629,
     631,   633,   635,   637,   641,   645,   649,   653,   657,   661,
     665,   669,   673,   677,   681,   685,   689,   693,   697,   701,
     705,   709,   713,   717,   721,   725,   729,   733,   736,   739,
     743,   748,   749,   757,   758,   766,   768,   772,   773,   775,
     779,   782,   786,   789,   792,   795,   796,   806,   807,   817,
     818,   826,   827
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      71,     0,    -1,   108,    61,    -1,   108,    -1,   100,    -1,
     111,    -1,     3,     3,    -1,   108,     1,    -1,     1,    -1,
       3,    -1,     5,    -1,     7,    -1,     7,    21,    -1,     4,
      -1,    10,    -1,    11,    -1,    12,    -1,    23,    -1,    62,
      35,    73,    54,    73,    54,    73,    63,    -1,    62,    35,
      73,    54,    73,    54,    73,    73,    64,    73,    64,    73,
      63,    -1,    62,    35,    73,    54,    73,    54,    73,    73,
      64,    73,    63,    -1,    62,    35,    73,    54,    73,    54,
      73,    73,    64,    73,    64,    73,    29,    63,    -1,    62,
      35,    73,    54,    73,    54,    73,    73,    64,    73,    29,
      63,    -1,    62,    35,    73,    54,    73,    54,    73,    73,
      64,    73,    64,    73,    30,    63,    -1,    62,    35,    73,
      54,    73,    54,    73,    73,    64,    73,    30,    63,    -1,
      62,    99,    63,    -1,    62,    83,    63,    -1,    22,    -1,
     108,    22,   112,    -1,    83,    60,   112,    22,   112,    -1,
      81,   129,    -1,    72,    -1,    72,    21,    -1,    27,    -1,
      28,    -1,    87,    21,    -1,    58,   138,    -1,    89,    21,
      -1,    26,    21,    -1,    26,    21,    91,    -1,    91,    86,
      -1,    91,    74,    -1,    91,   139,    -1,    91,    88,    -1,
      91,    90,    -1,    72,    -1,    87,    -1,    74,    93,    -1,
      88,    93,    -1,    90,    93,    -1,   139,    93,    -1,    86,
      93,    -1,    91,    93,    -1,    92,    93,    -1,    74,   138,
      -1,    86,   138,    -1,    88,   138,    -1,    90,   138,    -1,
     139,   138,    -1,    75,   129,    -1,    76,   129,    -1,   131,
     129,    -1,    77,   129,    -1,    79,   129,    -1,    80,   129,
      -1,    78,   129,    -1,    85,   129,    -1,    94,   129,    -1,
      95,   129,    -1,    87,   129,    -1,    89,   129,    -1,   103,
     129,    -1,   105,   129,    -1,   104,   129,    -1,   106,   129,
      -1,    98,   129,    -1,   140,   129,    -1,   138,   129,    -1,
      72,    65,    -1,    97,    72,    65,    -1,    72,    66,    99,
      67,    -1,    97,    72,    66,    99,    67,    -1,    87,    66,
      99,    67,    -1,    72,    66,     1,    -1,   101,    -1,    99,
      60,   101,    -1,    59,    72,    -1,   112,    -1,    59,   108,
      -1,    72,    -1,    87,    -1,    89,    -1,   113,    64,   102,
      -1,    64,   102,    -1,   103,    66,    99,    67,    -1,   104,
      66,    99,    67,    -1,    73,    -1,    75,    -1,    76,    -1,
     131,    -1,    77,    -1,    79,    -1,    80,    -1,    78,    -1,
      81,    -1,    82,    -1,    84,    -1,    94,    -1,    87,    -1,
      89,    -1,    85,    -1,    96,    -1,    98,    -1,   140,    -1,
     103,    -1,   105,    -1,   104,    -1,   106,    -1,    95,    -1,
     118,    -1,   125,    -1,   115,    -1,   116,    -1,   117,    -1,
     126,    -1,   127,    -1,   128,    -1,   107,    -1,   138,    -1,
      -1,   101,    60,   110,   101,    -1,   109,    -1,   111,    60,
     101,    -1,    -1,   108,    -1,    73,    -1,    75,    -1,    79,
      -1,    80,    -1,    76,    -1,   131,    -1,    77,    -1,    78,
      -1,    81,    -1,    84,    -1,    82,    -1,    94,    -1,    95,
      -1,    87,    -1,    89,    -1,    85,    -1,    96,    -1,   138,
      -1,    98,    -1,   140,    -1,   103,    -1,   105,    -1,   104,
      -1,   106,    -1,    19,    -1,    20,    -1,   113,   114,    -1,
      19,   108,    -1,    20,   108,    -1,    15,   108,    -1,    52,
     108,    -1,    51,   108,    -1,    73,     6,   108,    -1,    75,
       6,   108,    -1,    79,     6,   108,    -1,    80,     6,   108,
      -1,    76,     6,   108,    -1,   131,     6,   108,    -1,    77,
       6,   108,    -1,    78,     6,   108,    -1,    81,     6,   108,
      -1,    84,     6,   108,    -1,    82,     6,   108,    -1,    94,
       6,   108,    -1,    95,     6,   108,    -1,    87,     6,   108,
      -1,    89,     6,   108,    -1,    85,     6,   108,    -1,    96,
       6,   108,    -1,   138,     6,   108,    -1,    98,     6,   108,
      -1,   140,     6,   108,    -1,   103,     6,   108,    -1,   105,
       6,   108,    -1,   104,     6,   108,    -1,   106,     6,   108,
      -1,   113,    31,   108,    -1,   113,    32,   108,    -1,   113,
      33,   108,    -1,   113,    34,   108,    -1,   113,    37,   108,
      -1,   113,    36,   108,    -1,   119,    -1,   120,    -1,   121,
      -1,   122,    -1,   123,    -1,   124,    -1,   108,    51,   108,
      -1,   108,    52,   108,    -1,   108,    53,   108,    -1,   108,
      54,   108,    -1,   108,    55,   108,    -1,   108,    35,   108,
      -1,   108,    39,   108,    -1,   108,    40,   108,    -1,   108,
      41,   108,    -1,   108,    42,   108,    -1,   108,    43,   108,
      -1,   108,    13,   108,    -1,   108,    14,   108,    -1,   108,
      16,   108,    -1,   108,    46,   108,    -1,   108,    47,   108,
      -1,   108,    24,   108,    -1,   108,    25,   108,    -1,   108,
      17,   108,    -1,   108,    18,   108,    -1,   108,    48,   108,
      -1,   108,    45,   108,    -1,   108,    50,   108,    -1,   108,
      49,   108,    -1,   130,    68,    -1,    69,   108,    -1,   130,
      60,   108,    -1,   130,    68,    69,   108,    -1,    -1,    62,
      38,   132,   135,    38,   134,    63,    -1,    -1,    62,    38,
     133,   136,    38,   134,    63,    -1,   108,    -1,   134,    60,
     108,    -1,    -1,    72,    -1,   136,    60,    72,    -1,    66,
     112,    -1,   137,    60,   112,    -1,   137,    67,    -1,   137,
       1,    -1,   138,    21,    -1,    -1,     8,    66,   108,    60,
     112,    60,   141,   112,    67,    -1,    -1,     9,    66,   108,
      60,   112,    60,   142,   112,    67,    -1,    -1,     8,    66,
     108,    60,   112,   143,    67,    -1,    -1,     9,    66,   108,
      60,   112,   144,    67,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   302,   302,   312,   322,   339,   346,   371,   391,   413,
     418,   419,   422,   427,   432,   437,   438,   443,   448,   457,
     471,   484,   498,   511,   525,   544,   554,   557,   558,   559,
     564,   569,   572,   577,   580,   605,   616,   619,   626,   627,
     632,   633,   634,   635,   636,   639,   640,   643,   644,   645,
     646,   647,   648,   649,   658,   659,   660,   661,   662,   667,
     668,   669,   670,   671,   672,   673,   674,   675,   676,   677,
     678,   679,   680,   681,   682,   683,   684,   685,   688,   689,
     697,   706,   715,   724,   742,   750,   753,   756,   757,   824,
     825,   826,   829,   832,   839,   849,   855,   856,   857,   858,
     859,   860,   861,   862,   863,   864,   865,   866,   867,   868,
     869,   870,   871,   872,   873,   874,   875,   876,   877,   878,
     879,   880,   881,   882,   883,   884,   885,   888,   889,   892,
     892,   907,   908,   911,   912,   915,   916,   917,   918,   919,
     920,   921,   922,   923,   924,   925,   926,   927,   928,   929,
     930,   931,   932,   933,   934,   935,   936,   937,   938,   944,
     945,   951,   954,   955,   958,   959,   960,   963,   964,   965,
     966,   967,   968,   969,   970,   971,   972,   973,   974,   975,
     976,   977,   978,   979,   980,   981,   982,   983,   984,   985,
     986,   989,   992,   995,   998,  1001,  1004,  1007,  1008,  1009,
    1010,  1011,  1012,  1015,  1016,  1017,  1018,  1019,  1020,  1021,
    1022,  1023,  1024,  1025,  1028,  1029,  1032,  1033,  1034,  1035,
    1036,  1037,  1038,  1039,  1040,  1041,  1042,  1045,  1051,  1052,
    1053,  1057,  1056,  1075,  1074,  1096,  1097,  1103,  1106,  1107,
    1110,  1111,  1114,  1115,  1135,  1139,  1138,  1144,  1143,  1149,
    1148,  1154,  1153
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "NIL", "NUM_DOUBLE",
  "INASSIGN", "NUM_LONG", "IIF", "IF", "LITERAL", "TRUEVALUE",
  "FALSEVALUE", "AND", "OR", "NOT", "EQ", "NE1", "NE2", "INC", "DEC",
  "ALIASOP", "HASHOP", "SELF", "LE", "GE", "FIELD", "MACROVAR",
  "MACROTEXT", "H12AM", "H12PM", "PLUSEQ", "MINUSEQ", "MULTEQ", "DIVEQ",
  "POWER", "EXPEQ", "MODEQ", "CBMARKER", "BITAND", "BITOR", "BITXOR",
  "BITSHIFTR", "BITSHIFTL", "POST", "'='", "'<'", "'>'", "'$'", "MATCH",
  "LIKE", "'+'", "'-'", "'*'", "'/'", "'%'", "UNARY", "PRE", "'&'", "'@'",
  "','", "'\\n'", "'{'", "'}'", "':'", "'.'", "'('", "')'", "']'", "'['",
  "$accept", "Main", "IdentName", "NumValue", "NumAlias", "NilValue",
  "LiteralValue", "Logical", "SelfValue", "Date", "DateTime", "Array",
  "Hash", "HashList", "ArrayAt", "Variable", "VarAlias", "MacroVar",
  "MacroVarAlias", "MacroExpr", "MacroExprAlias", "FieldAlias",
  "FieldVarAlias", "AliasId", "AliasVar", "AliasExpr", "VariableAt",
  "NamespacePath", "FunCall", "ArgList", "ByRefArg", "Argument", "SendId",
  "ObjectData", "WithData", "ObjectMethod", "WithMethod",
  "SimpleExpression", "Expression", "RootParamList", "$@1", "AsParamList",
  "EmptyExpression", "LeftExpression", "PostOp", "ExprPostOp", "ExprPreOp",
  "ExprUnary", "ExprAssign", "ExprPlusEq", "ExprMinusEq", "ExprMultEq",
  "ExprDivEq", "ExprModEq", "ExprExpEq", "ExprOperEq", "ExprMath",
  "ExprBool", "ExprRelation", "ArrayIndex", "IndexList", "CodeBlock", "@2",
  "@3", "BlockExpList", "BlockNoVar", "BlockVarList", "ExpList",
  "PareExpList", "PareExpListAlias", "IfInline", "@4", "@5", "@6", "@7", 0
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
     295,   296,   297,   298,   299,    61,    60,    62,    36,   300,
     301,    43,    45,    42,    47,    37,   302,   303,    38,    64,
      44,    10,   123,   125,    58,    46,    40,    41,    93,    91
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    70,    71,    71,    71,    71,    71,    71,    71,    72,
      73,    73,    74,    75,    76,    77,    77,    78,    79,    80,
      80,    80,    80,    80,    80,    81,    82,    83,    83,    83,
      84,    85,    86,    87,    87,    88,    89,    90,    91,    91,
      92,    92,    92,    92,    92,    93,    93,    94,    94,    94,
      94,    94,    94,    94,    95,    95,    95,    95,    95,    96,
      96,    96,    96,    96,    96,    96,    96,    96,    96,    96,
      96,    96,    96,    96,    96,    96,    96,    96,    97,    97,
      98,    98,    98,    98,    99,    99,   100,   101,   101,   102,
     102,   102,   103,   104,   105,   106,   107,   107,   107,   107,
     107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
     107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
     107,   107,   107,   107,   107,   107,   107,   108,   108,   110,
     109,   111,   111,   112,   112,   113,   113,   113,   113,   113,
     113,   113,   113,   113,   113,   113,   113,   113,   113,   113,
     113,   113,   113,   113,   113,   113,   113,   113,   113,   114,
     114,   115,   116,   116,   117,   117,   117,   118,   118,   118,
     118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
     118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
     118,   119,   120,   121,   122,   123,   124,   125,   125,   125,
     125,   125,   125,   126,   126,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   127,   127,   128,   128,   128,   128,
     128,   128,   128,   128,   128,   128,   128,   129,   130,   130,
     130,   132,   131,   133,   131,   134,   134,   135,   136,   136,
     137,   137,   138,   138,   139,   141,   140,   142,   140,   143,
     140,   144,   140
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     1,     1,     2,     2,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     8,    13,
      11,    14,    12,    14,    12,     3,     3,     1,     3,     5,
       2,     1,     2,     1,     1,     2,     2,     2,     2,     3,
       2,     2,     2,     2,     2,     1,     1,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     3,
       4,     5,     4,     3,     1,     3,     2,     1,     2,     1,
       1,     1,     3,     2,     4,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       4,     1,     3,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     2,     2,     2,     2,     2,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     1,     1,     1,
       1,     1,     1,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     2,     2,     3,
       4,     0,     7,     0,     7,     1,     3,     0,     1,     3,
       2,     3,     2,     2,     2,     0,     9,     0,     9,     0,
       7,     0,     7
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     8,     9,    13,    10,    11,     0,     0,    14,    15,
      16,     0,     0,     0,    17,     0,    33,    34,     0,     0,
       0,     0,   133,     0,   133,     0,    31,    96,     0,    97,
      98,   100,   103,   101,   102,   104,   105,   106,   110,     0,
     108,     0,   109,     0,     0,     0,   107,   118,   111,     0,
     112,     4,     0,   114,   116,   115,   117,   127,     0,   131,
       5,    87,     0,   121,   122,   123,   119,   197,   198,   199,
     200,   201,   202,   120,   124,   125,   126,    99,     0,   128,
       0,   113,     6,    12,     0,     0,     9,   164,   162,   163,
      38,   166,   165,    36,    31,    88,    27,     0,   231,     0,
       0,     0,    84,   134,    89,    90,    91,    93,   134,   240,
       1,    32,    78,     0,     0,    45,    46,    47,    54,     0,
       0,    59,     0,     0,    60,     0,    62,     0,    65,     0,
      63,     0,    64,     0,    30,     0,     0,     0,    66,    51,
      55,     0,    35,   133,    69,    48,    56,     0,    37,    70,
      49,    57,     0,    45,    41,    40,    46,    43,     0,    44,
      52,     0,    42,    53,     0,    67,     0,    68,     0,     0,
       0,    75,   129,     0,   133,    71,     0,   133,    73,     0,
      72,     0,    74,     7,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     2,   133,
     159,   160,     0,     0,     0,     0,     0,     0,     0,   161,
       0,    61,   243,   133,   242,     0,   244,    77,    50,    58,
       0,    76,     0,     0,    39,    11,     0,   237,     0,   133,
      26,   133,    25,   133,    83,     0,   167,   168,   228,     0,
     227,   171,   173,   174,   169,   170,   175,   177,   176,   182,
     180,     0,   181,   178,   179,   183,    79,   133,   185,   133,
     187,     0,   189,     0,   188,   190,   214,   215,   216,   221,
     222,   219,   220,   208,   209,   210,   211,   212,   213,   224,
     217,   218,   223,   226,   225,   203,   204,   205,   206,   207,
     132,   191,   192,   193,   194,   196,   195,    92,   172,   241,
     184,   186,   133,   133,     0,     0,   238,     0,     0,    85,
      28,    80,   229,     0,    82,     0,   130,    94,    95,   249,
     251,     0,     0,     0,     0,   133,   230,    81,   245,     0,
     247,     0,     0,   235,     0,     0,   239,    29,   133,   250,
     133,   252,     0,     0,   232,   234,     0,     0,    18,     0,
     236,   246,   248,     0,     0,     0,     0,    20,     0,    22,
      24,     0,     0,     0,    19,    21,    23
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,   100,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   117,    46,    47,    48,    49,    50,   101,
      51,   102,   107,    53,    54,    55,    56,    57,   108,    59,
     269,    60,    61,    62,   219,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,   121,
     122,    77,   237,   238,   344,   315,   317,    78,    79,    80,
      81,   348,   350,   339,   341
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -111
static const yytype_int16 yypact[] =
{
     347,  -111,    14,  -111,  -111,    15,   -21,   -16,  -111,  -111,
    -111,   502,   502,   502,  -111,    54,  -111,  -111,   502,   502,
      -3,   502,   411,    29,   502,    97,     4,   264,    12,   625,
     647,   666,   686,   695,   734,   741,   974,   993,   763,    12,
      74,    12,   525,    12,    34,    61,   782,   802,  1003,    98,
     811,  -111,    43,   559,   566,   850,   857,  -111,   936,  -111,
      52,  -111,   360,  -111,  -111,  -111,  -111,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,   879,     7,   618,
      12,   898,  -111,  -111,   502,   502,  -111,  1277,  -111,  -111,
      87,  -111,  -111,  -111,     6,  1194,  -111,    -2,   118,   502,
      13,    88,  -111,  1151,  -111,  -111,  -111,  -111,  1194,  -111,
    -111,  -111,  -111,   254,   502,  -111,  -111,  -111,  -111,   502,
     502,  -111,    36,   502,  -111,   502,  -111,   502,  -111,   502,
    -111,   502,  -111,   502,  -111,   502,   502,   502,  -111,  -111,
    -111,   502,  -111,   475,  -111,  -111,  -111,   502,  -111,  -111,
    -111,  -111,    15,   101,  -111,  -111,   107,  -111,   109,  -111,
    -111,   113,  -111,  -111,   502,  -111,   502,  -111,   502,    -6,
     502,  -111,  -111,   502,   475,  -111,   502,   475,  -111,   502,
    -111,   502,  -111,  -111,   502,   502,   502,   502,   502,   502,
     502,   502,   502,   502,   502,   502,   502,   502,   502,   502,
     502,   502,   502,   502,   502,   502,   502,   502,  -111,   475,
    -111,  -111,   502,   502,   502,   502,   502,   502,    29,  -111,
     502,  -111,  -111,   502,  -111,   502,  -111,  -111,  -111,  -111,
     502,  -111,  1061,  1106,  -111,  -111,   100,  -111,    98,   502,
    -111,   475,  -111,   502,  -111,   -44,  1194,  1194,  1194,   502,
      73,  1194,  1194,  1194,  1194,  1194,  1194,  1194,  1194,  1194,
    1194,    31,  1194,  1194,  1194,  1194,  -111,   475,  1194,   475,
    1194,    42,  1194,    57,  1194,  1194,  1237,  1194,  1397,  1397,
    1397,  1429,  1429,   122,  1357,  1277,  1317,   287,   287,  1397,
    1429,  1429,  1429,  1429,  1429,   199,   199,    91,    91,    91,
    -111,  1194,  1194,  1194,  1194,  1194,  1194,  -111,  1194,  -111,
    1194,  1194,   502,   502,    -2,   121,  -111,   -31,   138,  -111,
    -111,  -111,  1194,   502,  -111,    72,  -111,  -111,  -111,   102,
     103,   111,   502,   502,    98,   502,  1194,  -111,  -111,    94,
    -111,   104,    -2,  1194,    92,    93,  -111,  -111,   502,  -111,
     502,  -111,    23,   502,  -111,  -111,   105,   108,  -111,   110,
    1194,  -111,  -111,    -2,    86,   106,   114,  -111,    -2,  -111,
    -111,   -20,   115,   117,  -111,  -111,  -111
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -111,  -111,     3,   -96,   123,  -111,  -111,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,   139,    10,   164,    -9,
     166,   132,  -111,   188,  -111,  -111,  -111,  -111,  -111,  -109,
    -111,     2,   -36,  -111,  -111,  -111,  -111,  -111,     0,  -111,
    -111,  -111,    -4,  -111,  -111,  -111,  -111,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  1016,
    -111,  -111,  -111,  -111,  -110,  -111,  -111,  -111,    38,   180,
    -111,  -111,  -111,  -111,  -111
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -234
static const yytype_int16 yytable[] =
{
      58,   236,    52,     4,   245,   235,   -86,   333,   222,   372,
     373,    87,    88,    89,   106,    86,   241,    82,    91,    92,
     109,    95,   103,   321,    94,   111,   104,   111,     4,   334,
     235,   115,    86,   105,   261,   158,    83,    86,   116,    16,
      17,   152,   115,   374,   115,    84,   115,   153,   115,   116,
      85,   116,   169,   116,   156,   116,    16,    17,    93,   266,
     267,    16,    17,    24,    86,   271,   118,   223,   273,   112,
     113,   112,   113,   239,   224,    90,   240,   140,    24,   146,
     141,   151,   161,   115,   232,   233,   358,    20,    16,    17,
     116,   241,    20,  -148,  -148,   142,   249,   110,   324,    95,
      24,    86,   241,   172,   250,  -148,  -148,  -148,  -148,   327,
    -148,  -148,   209,    15,   246,   365,   366,   241,   229,   247,
     248,  -233,   111,   251,   328,   252,   191,   253,   142,   254,
     148,   255,   241,   256,   226,   257,   258,   259,  -148,   337,
     143,   260,   323,   120,   205,   206,   207,   262,   241,   367,
     368,   242,   353,   353,   314,   354,   355,   191,   325,   332,
     335,   349,   338,   340,   263,   342,   264,   154,   265,   369,
     268,   351,   361,   270,   363,   362,   272,   370,   375,   274,
     376,   275,   307,   155,   276,   277,   278,   279,   280,   281,
     282,   283,   284,   285,   286,   287,   288,   289,   290,   291,
     292,   293,   294,   295,   296,   297,   298,   299,   157,   106,
     159,   300,   301,   302,   303,   304,   305,   306,   331,   309,
     308,   104,   234,   345,   162,   310,     0,   139,   105,   145,
     311,   150,   160,   163,   191,   318,     0,     0,     0,   320,
       0,   316,     0,   319,     0,     0,   352,     0,     0,   322,
     203,   204,   205,   206,   207,   244,   359,    86,     3,     4,
       0,     5,     6,     7,     8,     9,    10,   364,   228,    11,
     114,   326,   371,    12,    13,     0,     0,    14,     0,     0,
      15,    16,    17,  -135,  -135,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -135,  -135,  -135,  -135,     0,
    -135,  -135,     0,     0,     0,    18,    19,     0,   329,   330,
       0,     0,    20,    99,  -133,     0,    22,     0,    23,     0,
      24,  -133,   191,   336,     0,     0,     0,     0,  -135,   195,
     196,   347,   343,   343,     0,     0,     0,   346,   203,   204,
     205,   206,   207,     0,   356,     0,   357,     0,     1,     0,
       2,     3,     4,   360,     5,     6,     7,     8,     9,    10,
       0,     0,    11,     0,     0,     0,    12,    13,     0,     0,
      14,     0,     0,    15,    16,    17,     0,     0,     0,   210,
     211,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   212,   213,   214,   215,     0,   216,   217,    18,    19,
       0,     0,     0,     0,     0,    20,    21,  -133,     0,    22,
       0,    23,     0,    24,    86,     3,     4,     0,     5,     6,
       7,     8,     9,    10,   218,     0,    11,     0,     0,     0,
      12,    13,     0,    96,    14,     0,     0,    15,    16,    17,
       0,     0,     0,     0,     0,     0,    97,     0,     0,    98,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    18,    19,     0,     0,     0,     0,     0,    20,
      99,     0,     0,    22,     0,    23,     0,    24,    86,     3,
       4,     0,     5,     6,     7,     8,     9,    10,     0,     0,
      11,     0,     0,     0,    12,    13,     0,     0,    14,     0,
       0,    15,    16,    17,     0,    86,     3,     4,     0,     5,
       6,     7,     8,     9,    10,     0,     0,    11,     0,     0,
       0,    12,    13,     0,     0,    14,    18,    19,    15,    16,
      17,   147,     0,    20,    99,     0,     0,    22,     0,    23,
       0,    24,     0,     0,  -149,  -149,   148,     0,     0,     0,
       0,     0,     0,    18,    19,     0,  -149,  -149,  -149,  -149,
      20,  -149,  -149,     0,    22,   173,    23,     0,    24,     0,
       0,     0,   176,     0,     0,     0,     0,     0,  -155,  -155,
       0,     0,     0,     0,     0,  -157,  -157,     0,     0,  -149,
    -155,  -155,  -155,  -155,   120,  -155,  -155,  -157,  -157,  -157,
    -157,     0,  -157,  -157,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -155,   225,   174,     0,     0,   120,     0,
    -157,   119,   177,     0,     0,   120,     0,  -152,  -152,   226,
       0,     0,     0,     0,  -136,  -136,     0,     0,     0,  -152,
    -152,  -152,  -152,   123,  -152,  -152,  -136,  -136,  -136,  -136,
       0,  -136,  -136,     0,     0,     0,  -139,  -139,     0,     0,
       0,     0,   125,     0,     0,     0,     0,     0,  -139,  -139,
    -139,  -139,  -152,  -139,  -139,  -141,  -141,   120,     0,  -136,
       0,     0,   127,     0,   120,     0,     0,  -141,  -141,  -141,
    -141,   129,  -141,  -141,     0,  -142,  -142,     0,     0,     0,
       0,  -139,     0,     0,  -137,  -137,   120,  -142,  -142,  -142,
    -142,     0,  -142,  -142,     0,     0,  -137,  -137,  -137,  -137,
    -141,  -137,  -137,     0,     0,   120,     0,     0,     0,     0,
     131,     0,     0,     0,     0,     0,     0,   133,     0,     0,
    -142,     0,     0,  -138,  -138,   120,     0,     0,     0,  -137,
    -143,  -143,     0,     0,   120,  -138,  -138,  -138,  -138,   137,
    -138,  -138,  -143,  -143,  -143,  -143,     0,  -143,  -143,     0,
       0,     0,  -150,  -150,     0,     0,     0,     0,   164,     0,
       0,     0,     0,     0,  -150,  -150,  -150,  -150,  -138,  -150,
    -150,  -146,  -146,   120,     0,  -143,     0,     0,   166,     0,
     120,     0,     0,  -146,  -146,  -146,  -146,   170,  -146,  -146,
       0,  -147,  -147,     0,     0,     0,     0,  -150,     0,     0,
    -153,  -153,   120,  -147,  -147,  -147,  -147,     0,  -147,  -147,
       0,     0,  -153,  -153,  -153,  -153,  -146,  -153,  -153,     0,
       0,   120,     0,     0,     0,     0,   179,     0,     0,     0,
       0,     0,     0,   181,     0,     0,  -147,     0,     0,  -156,
    -156,   120,     0,     0,     0,  -153,  -158,  -158,     0,     0,
     120,  -156,  -156,  -156,  -156,   220,  -156,  -156,  -158,  -158,
    -158,  -158,     0,  -158,  -158,     0,     0,     0,  -140,  -140,
       0,     0,     0,     0,   230,     0,     0,     0,     0,     0,
    -140,  -140,  -140,  -140,  -156,  -140,  -140,  -154,  -154,   120,
       0,  -158,     0,     0,     0,     0,   120,     0,     0,  -154,
    -154,  -154,  -154,     0,  -154,  -154,    -3,   183,     0,     0,
       0,     0,     0,  -140,     0,     0,     0,     0,   120,   184,
     185,     0,   186,   187,   188,     0,     0,     0,     0,     0,
     189,   190,  -154,     0,     0,     0,     0,   120,     0,     0,
       0,   191,     0,     0,     0,   192,   193,   194,   195,   196,
     135,   197,   198,   199,   200,   201,   202,   203,   204,   205,
     206,   207,     0,  -145,  -145,     0,  -134,   208,     0,   136,
       0,     0,     0,     0,     0,  -145,  -145,  -145,  -145,   168,
    -145,  -145,  -144,  -144,     0,     0,     0,     0,     0,     0,
       0,     0,  -151,  -151,  -144,  -144,  -144,  -144,     0,  -144,
    -144,     0,     0,     0,  -151,  -151,  -151,  -151,  -145,  -151,
    -151,     0,     0,     0,     0,     0,   124,   126,   128,   130,
     132,   134,     0,     0,   138,     0,   144,  -144,   149,     0,
       0,     0,   165,   167,     0,     0,   171,  -151,     0,   175,
     178,   180,   182,     0,   184,   185,     0,   186,   187,   188,
       0,     0,     0,     0,     0,   189,   190,     0,     0,     0,
       0,     0,     0,   221,     0,   227,   191,   231,     0,     0,
     192,   193,   194,   195,   196,     0,   197,   198,   199,   200,
     201,   202,   203,   204,   205,   206,   207,     0,     0,   184,
     185,   312,   186,   187,   188,     0,     0,     0,     0,     0,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,     0,     0,     0,   192,   193,   194,   195,   196,
       0,   197,   198,   199,   200,   201,   202,   203,   204,   205,
     206,   207,     0,     0,   184,   185,   313,   186,   187,   188,
       0,     0,     0,   243,     0,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,     0,     0,     0,
     192,   193,   194,   195,   196,     0,   197,   198,   199,   200,
     201,   202,   203,   204,   205,   206,   207,   184,   185,     0,
     186,   187,   188,     0,     0,     0,     0,     0,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
       0,     0,     0,   192,   193,   194,   195,   196,     0,   197,
     198,   199,   200,   201,   202,   203,   204,   205,   206,   207,
     184,     0,     0,   186,   187,   188,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,     0,     0,     0,   192,   193,   194,   195,
     196,     0,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   186,   187,   188,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,     0,     0,     0,   192,   193,   194,   195,
     196,     0,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   186,   187,   188,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,     0,     0,     0,   192,     0,   194,   195,
     196,     0,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   186,   187,   188,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,     0,     0,     0,   192,     0,     0,   195,
     196,     0,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   186,   187,   188,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,     0,     0,     0,     0,     0,     0,   195,
     196,     0,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,     0,     0,     0,     0,     0,
       0,   195,   196,     0,     0,   198,   199,   200,   201,   202,
     203,   204,   205,   206,   207
};

static const yytype_int16 yycheck[] =
{
       0,    97,     0,     5,   113,     7,     0,    38,     1,    29,
      30,    11,    12,    13,    23,     3,    60,     3,    18,    19,
      24,    21,    22,    67,    21,    21,    23,    21,     5,    60,
       7,    28,     3,    23,   143,    44,    21,     3,    28,    27,
      28,     7,    39,    63,    41,    66,    43,    44,    45,    39,
      66,    41,    49,    43,    44,    45,    27,    28,    20,    65,
      66,    27,    28,    66,     3,   174,    28,    60,   177,    65,
      66,    65,    66,    60,    67,    21,    63,    39,    66,    41,
       6,    43,    44,    80,    84,    85,    63,    58,    27,    28,
      80,    60,    58,    19,    20,    21,    60,     0,    67,    99,
      66,     3,    60,    60,    68,    31,    32,    33,    34,    67,
      36,    37,    60,    26,   114,    29,    30,    60,    80,   119,
     120,     3,    21,   123,    67,   125,    35,   127,    21,   129,
      21,   131,    60,   133,    21,   135,   136,   137,    64,    67,
      66,   141,    69,    69,    53,    54,    55,   147,    60,    63,
      64,    63,    60,    60,    54,    63,    63,    35,   267,    38,
      22,    67,    60,    60,   164,    54,   166,    44,   168,    63,
     170,    67,    67,   173,    64,    67,   176,    63,    63,   179,
      63,   181,   218,    44,   184,   185,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,   201,   202,   203,   204,   205,   206,   207,    44,   218,
      44,   209,   212,   213,   214,   215,   216,   217,   314,   223,
     220,   218,    90,   333,    44,   225,    -1,    39,   218,    41,
     230,    43,    44,    45,    35,   239,    -1,    -1,    -1,   243,
      -1,   238,    -1,   241,    -1,    -1,   342,    -1,    -1,   249,
      51,    52,    53,    54,    55,     1,   352,     3,     4,     5,
      -1,     7,     8,     9,    10,    11,    12,   363,    80,    15,
       6,   269,   368,    19,    20,    -1,    -1,    23,    -1,    -1,
      26,    27,    28,    19,    20,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,    -1,
      36,    37,    -1,    -1,    -1,    51,    52,    -1,   312,   313,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    64,    -1,
      66,    67,    35,   323,    -1,    -1,    -1,    -1,    64,    42,
      43,   335,   332,   333,    -1,    -1,    -1,   334,    51,    52,
      53,    54,    55,    -1,   348,    -1,   350,    -1,     1,    -1,
       3,     4,     5,   353,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    -1,    -1,    -1,    19,    20,    -1,    -1,
      23,    -1,    -1,    26,    27,    28,    -1,    -1,    -1,    19,
      20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    32,    33,    34,    -1,    36,    37,    51,    52,
      -1,    -1,    -1,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    64,    -1,    66,     3,     4,     5,    -1,     7,     8,
       9,    10,    11,    12,    64,    -1,    15,    -1,    -1,    -1,
      19,    20,    -1,    22,    23,    -1,    -1,    26,    27,    28,
      -1,    -1,    -1,    -1,    -1,    -1,    35,    -1,    -1,    38,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    -1,    -1,    58,
      59,    -1,    -1,    62,    -1,    64,    -1,    66,     3,     4,
       5,    -1,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    -1,    -1,    -1,    19,    20,    -1,    -1,    23,    -1,
      -1,    26,    27,    28,    -1,     3,     4,     5,    -1,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    -1,    -1,
      -1,    19,    20,    -1,    -1,    23,    51,    52,    26,    27,
      28,     6,    -1,    58,    59,    -1,    -1,    62,    -1,    64,
      -1,    66,    -1,    -1,    19,    20,    21,    -1,    -1,    -1,
      -1,    -1,    -1,    51,    52,    -1,    31,    32,    33,    34,
      58,    36,    37,    -1,    62,     6,    64,    -1,    66,    -1,
      -1,    -1,     6,    -1,    -1,    -1,    -1,    -1,    19,    20,
      -1,    -1,    -1,    -1,    -1,    19,    20,    -1,    -1,    64,
      31,    32,    33,    34,    69,    36,    37,    31,    32,    33,
      34,    -1,    36,    37,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    64,     6,    66,    -1,    -1,    69,    -1,
      64,     6,    66,    -1,    -1,    69,    -1,    19,    20,    21,
      -1,    -1,    -1,    -1,    19,    20,    -1,    -1,    -1,    31,
      32,    33,    34,     6,    36,    37,    31,    32,    33,    34,
      -1,    36,    37,    -1,    -1,    -1,    19,    20,    -1,    -1,
      -1,    -1,     6,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    64,    36,    37,    19,    20,    69,    -1,    64,
      -1,    -1,     6,    -1,    69,    -1,    -1,    31,    32,    33,
      34,     6,    36,    37,    -1,    19,    20,    -1,    -1,    -1,
      -1,    64,    -1,    -1,    19,    20,    69,    31,    32,    33,
      34,    -1,    36,    37,    -1,    -1,    31,    32,    33,    34,
      64,    36,    37,    -1,    -1,    69,    -1,    -1,    -1,    -1,
       6,    -1,    -1,    -1,    -1,    -1,    -1,     6,    -1,    -1,
      64,    -1,    -1,    19,    20,    69,    -1,    -1,    -1,    64,
      19,    20,    -1,    -1,    69,    31,    32,    33,    34,     6,
      36,    37,    31,    32,    33,    34,    -1,    36,    37,    -1,
      -1,    -1,    19,    20,    -1,    -1,    -1,    -1,     6,    -1,
      -1,    -1,    -1,    -1,    31,    32,    33,    34,    64,    36,
      37,    19,    20,    69,    -1,    64,    -1,    -1,     6,    -1,
      69,    -1,    -1,    31,    32,    33,    34,     6,    36,    37,
      -1,    19,    20,    -1,    -1,    -1,    -1,    64,    -1,    -1,
      19,    20,    69,    31,    32,    33,    34,    -1,    36,    37,
      -1,    -1,    31,    32,    33,    34,    64,    36,    37,    -1,
      -1,    69,    -1,    -1,    -1,    -1,     6,    -1,    -1,    -1,
      -1,    -1,    -1,     6,    -1,    -1,    64,    -1,    -1,    19,
      20,    69,    -1,    -1,    -1,    64,    19,    20,    -1,    -1,
      69,    31,    32,    33,    34,     6,    36,    37,    31,    32,
      33,    34,    -1,    36,    37,    -1,    -1,    -1,    19,    20,
      -1,    -1,    -1,    -1,     6,    -1,    -1,    -1,    -1,    -1,
      31,    32,    33,    34,    64,    36,    37,    19,    20,    69,
      -1,    64,    -1,    -1,    -1,    -1,    69,    -1,    -1,    31,
      32,    33,    34,    -1,    36,    37,     0,     1,    -1,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,    69,    13,
      14,    -1,    16,    17,    18,    -1,    -1,    -1,    -1,    -1,
      24,    25,    64,    -1,    -1,    -1,    -1,    69,    -1,    -1,
      -1,    35,    -1,    -1,    -1,    39,    40,    41,    42,    43,
       6,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    -1,    19,    20,    -1,    60,    61,    -1,     6,
      -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,     6,
      36,    37,    19,    20,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    20,    31,    32,    33,    34,    -1,    36,
      37,    -1,    -1,    -1,    31,    32,    33,    34,    64,    36,
      37,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    38,    -1,    40,    64,    42,    -1,
      -1,    -1,    46,    47,    -1,    -1,    50,    64,    -1,    53,
      54,    55,    56,    -1,    13,    14,    -1,    16,    17,    18,
      -1,    -1,    -1,    -1,    -1,    24,    25,    -1,    -1,    -1,
      -1,    -1,    -1,    77,    -1,    79,    35,    81,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    -1,    -1,    13,
      14,    60,    16,    17,    18,    -1,    -1,    -1,    -1,    -1,
      24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    35,    -1,    -1,    -1,    39,    40,    41,    42,    43,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    -1,    -1,    13,    14,    60,    16,    17,    18,
      -1,    -1,    -1,    22,    -1,    24,    25,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    35,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    13,    14,    -1,
      16,    17,    18,    -1,    -1,    -1,    -1,    -1,    24,    25,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    35,
      -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      13,    -1,    -1,    16,    17,    18,    -1,    -1,    -1,    -1,
      -1,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    16,    17,    18,    -1,    -1,    -1,    -1,
      -1,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    16,    17,    18,    -1,    -1,    -1,    -1,
      -1,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,    -1,    -1,    -1,    39,    -1,    41,    42,
      43,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    16,    17,    18,    -1,    -1,    -1,    -1,
      -1,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,    -1,    -1,    -1,    39,    -1,    -1,    42,
      43,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    16,    17,    18,    -1,    -1,    -1,    -1,
      -1,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      43,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    24,    25,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    35,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    43,    -1,    -1,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,     5,     7,     8,     9,    10,    11,
      12,    15,    19,    20,    23,    26,    27,    28,    51,    52,
      58,    59,    62,    64,    66,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    94,    95,    96,    97,
      98,   100,   101,   103,   104,   105,   106,   107,   108,   109,
     111,   112,   113,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   131,   137,   138,
     139,   140,     3,    21,    66,    66,     3,   108,   108,   108,
      21,   108,   108,   138,    72,   108,    22,    35,    38,    59,
      83,    99,   101,   108,    72,    87,    89,   102,   108,   112,
       0,    21,    65,    66,     6,    72,    87,    93,   138,     6,
      69,   129,   130,     6,   129,     6,   129,     6,   129,     6,
     129,     6,   129,     6,   129,     6,     6,     6,   129,    93,
     138,     6,    21,    66,   129,    93,   138,     6,    21,   129,
      93,   138,     7,    72,    74,    86,    87,    88,    89,    90,
      93,   138,   139,    93,     6,   129,     6,   129,     6,    72,
       6,   129,    60,     6,    66,   129,     6,    66,   129,     6,
     129,     6,   129,     1,    13,    14,    16,    17,    18,    24,
      25,    35,    39,    40,    41,    42,    43,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    61,    60,
      19,    20,    31,    32,    33,    34,    36,    37,    64,   114,
       6,   129,     1,    60,    67,     6,    21,   129,    93,   138,
       6,   129,   108,   108,    91,     7,    73,   132,   133,    60,
      63,    60,    63,    22,     1,    99,   108,   108,   108,    60,
      68,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,    99,   108,   108,   108,   108,    65,    66,   108,   110,
     108,    99,   108,    99,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     101,   108,   108,   108,   108,   108,   108,   102,   108,   112,
     108,   108,    60,    60,    54,   135,    72,   136,   112,   101,
     112,    67,   108,    69,    67,    99,   101,    67,    67,   112,
     112,    73,    38,    38,    60,    22,   108,    67,    60,   143,
      60,   144,    54,   108,   134,   134,    72,   112,   141,    67,
     142,    67,    73,    60,    63,    63,   112,   112,    63,    73,
     108,    67,    67,    64,    73,    29,    30,    63,    64,    63,
      63,    73,    29,    30,    63,    63,    63
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
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
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


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval)
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

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
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





/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

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
/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

    /* Number of syntax errors so far.  */
    int yynerrs;

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
  if (yyn == YYPACT_NINF)
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
      if (yyn == 0 || yyn == YYTABLE_NINF)
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

/* Line 1455 of yacc.c  */
// #line 302 "source\\macro\\macro.y"
    {
                           HB_MACRO_DATA->exprType = hb_compExprType( (yyvsp[(1) - (2)].asExpr) );
                           if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_PUSH )
                              hb_compExprDelete( hb_compExprGenPush( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           else if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_STATEMENT )
                              hb_compExprDelete( hb_compExprGenStatement( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           else
                              hb_compExprDelete( hb_compExprGenPop( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           hb_compGenPCode1( HB_P_ENDPROC, HB_MACRO_PARAM );
                        ;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
// #line 312 "source\\macro\\macro.y"
    {
                           HB_MACRO_DATA->exprType = hb_compExprType( (yyvsp[(1) - (1)].asExpr) );
                           if( HB_MACRO_DATA->Flags &  HB_MACRO_GEN_PUSH )
                              hb_compExprDelete( hb_compExprGenPush( (yyvsp[(1) - (1)].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           else if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_STATEMENT )
                              hb_compExprDelete( hb_compExprGenStatement( (yyvsp[(1) - (1)].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           else
                              hb_compExprDelete( hb_compExprGenPop( (yyvsp[(1) - (1)].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           hb_compGenPCode1( HB_P_ENDPROC, HB_MACRO_PARAM );
                        ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
// #line 322 "source\\macro\\macro.y"
    {
                           if( ! ( HB_MACRO_DATA->Flags & HB_MACRO_GEN_LIST ) )
                           {
                              HB_TRACE(HB_TR_DEBUG, ("macro -> invalid expression: %s", HB_MACRO_DATA->string));
                              hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );
                              hb_compExprDelete( (yyvsp[(1) - (1)].asExpr), HB_MACRO_PARAM );
                              YYABORT;
                           }

                           (yyvsp[(1) - (1)].asExpr) = ( HB_MACRO_DATA->Flags & HB_MACRO_GEN_PARE ) ? hb_compExprNewList( (yyvsp[(1) - (1)].asExpr) ) : hb_compExprNewArgList( (yyvsp[(1) - (1)].asExpr) );

                           HB_MACRO_DATA->exprType = hb_compExprType( (yyvsp[(1) - (1)].asExpr) );

                           hb_compExprDelete( hb_compExprGenPush( (yyvsp[(1) - (1)].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );

                           hb_compGenPCode1( HB_P_ENDPROC, HB_MACRO_PARAM );
                        ;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
// #line 339 "source\\macro\\macro.y"
    {
                           HB_MACRO_DATA->exprType = hb_compExprType( (yyvsp[(1) - (1)].asExpr) );

                           hb_compExprDelete( hb_compExprGenPush( (yyvsp[(1) - (1)].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );

                           hb_compGenPCode1( HB_P_ENDPROC, HB_MACRO_PARAM );
                        ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
// #line 346 "source\\macro\\macro.y"
    {
                           HB_TRACE(HB_TR_DEBUG, ("macro -> invalid expression: %s", HB_MACRO_DATA->string));

                           //printf( "Macro: %s\n", HB_MACRO_DATA->string );

                           hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );

                           while ( s_iPending )
                           {
                              hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );
                           }

                           if( (yyvsp[(1) - (2)].string) == (yyvsp[(2) - (2)].string) )
                           {
                              hb_xfree( (yyvsp[(1) - (2)].string) );
                           }
                           else
                           {
                              hb_xfree( (yyvsp[(1) - (2)].string) );
                              hb_xfree( (yyvsp[(2) - (2)].string) );
                           }
                           yylval.string = NULL;

                           HB_MACRO_ABORT;
                        ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
// #line 371 "source\\macro\\macro.y"
    {
                           HB_TRACE(HB_TR_DEBUG, ("macro -> invalid expression: %s", HB_MACRO_DATA->string));

                           //printf( "Macro: %s\n", HB_MACRO_DATA->string );

                           hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );
                           hb_compExprDelete( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM );

                           while ( s_iPending )
                           {
                              hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );
                           }

                           if( yychar == IDENTIFIER && yylval.string )
                           {
                              hb_xfree( yylval.string );
                              yylval.string = NULL;
                           }
                           HB_MACRO_ABORT;
                        ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
// #line 391 "source\\macro\\macro.y"
    {
                           // This case is when error maybe nested in say a CodeBlock.
                           HB_TRACE(HB_TR_DEBUG, ("macro -> invalid syntax: %s", HB_MACRO_DATA->string));

                           //printf( "2-Macro: %s\n", HB_MACRO_DATA->string );

                           hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );

                           while ( s_iPending )
                           {
                              hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );
                           }

                           if( yychar == IDENTIFIER && yylval.string )
                           {
                              hb_xfree( yylval.string );
                              yylval.string = NULL;
                           }
                           HB_MACRO_ABORT;
                        ;}
    break;

  case 9:

/* Line 1455 of yacc.c  */
// #line 413 "source\\macro\\macro.y"
    { (yyval.string) = (yyvsp[(1) - (1)].string); (yyvsp[(1) - (1)].string) = NULL; ;}
    break;

  case 10:

/* Line 1455 of yacc.c  */
// #line 418 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewDouble( (yyvsp[(1) - (1)].valDouble).dNumber, (yyvsp[(1) - (1)].valDouble).bWidth, (yyvsp[(1) - (1)].valDouble).bDec ); ;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
// #line 419 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewLong( (yyvsp[(1) - (1)].valLong).lNumber ); ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
// #line 422 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewLong( (yyvsp[(1) - (2)].valLong).lNumber ); ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
// #line 427 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewNil(); ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
// #line 432 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewString( (yyvsp[(1) - (1)].valChar).string, (yyvsp[(1) - (1)].valChar).length, TRUE ); ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
// #line 437 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewLogical( TRUE ); ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
// #line 438 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewLogical( FALSE ); ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
// #line 443 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewSelf(); ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
// #line 449 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewDate( (yyvsp[(3) - (8)].asExpr), (yyvsp[(5) - (8)].asExpr), (yyvsp[(7) - (8)].asExpr) );
                                     hb_compExprDelete( (yyvsp[(3) - (8)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(5) - (8)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(7) - (8)].asExpr), HB_MACRO_PARAM );
                              ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
// #line 458 "source\\macro\\macro.y"
    { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[(3) - (13)].asExpr), (yyvsp[(5) - (13)].asExpr), (yyvsp[(7) - (13)].asExpr), (yyvsp[(8) - (13)].asExpr), (yyvsp[(10) - (13)].asExpr), (yyvsp[(12) - (13)].asExpr), 0, &iOk );
                                     hb_compExprDelete( (yyvsp[(3) - (13)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(5) - (13)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(7) - (13)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(8) - (13)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(10) - (13)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(12) - (13)].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
// #line 472 "source\\macro\\macro.y"
    { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[(3) - (11)].asExpr), (yyvsp[(5) - (11)].asExpr), (yyvsp[(7) - (11)].asExpr), (yyvsp[(8) - (11)].asExpr), (yyvsp[(10) - (11)].asExpr), NULL, 0, &iOk );
                                     hb_compExprDelete( (yyvsp[(3) - (11)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(5) - (11)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(7) - (11)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(8) - (11)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(10) - (11)].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
// #line 485 "source\\macro\\macro.y"
    { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[(3) - (14)].asExpr), (yyvsp[(5) - (14)].asExpr), (yyvsp[(7) - (14)].asExpr), (yyvsp[(8) - (14)].asExpr), (yyvsp[(10) - (14)].asExpr), (yyvsp[(12) - (14)].asExpr), 1, &iOk );
                                     hb_compExprDelete( (yyvsp[(3) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(5) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(7) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(8) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(10) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(12) - (14)].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
// #line 499 "source\\macro\\macro.y"
    { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[(3) - (12)].asExpr), (yyvsp[(5) - (12)].asExpr), (yyvsp[(7) - (12)].asExpr), (yyvsp[(8) - (12)].asExpr), (yyvsp[(10) - (12)].asExpr), NULL, 1, &iOk );
                                     hb_compExprDelete( (yyvsp[(3) - (12)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(5) - (12)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(7) - (12)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(8) - (12)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(10) - (12)].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
// #line 512 "source\\macro\\macro.y"
    { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[(3) - (14)].asExpr), (yyvsp[(5) - (14)].asExpr), (yyvsp[(7) - (14)].asExpr), (yyvsp[(8) - (14)].asExpr), (yyvsp[(10) - (14)].asExpr), (yyvsp[(12) - (14)].asExpr), 2, &iOk );
                                     hb_compExprDelete( (yyvsp[(3) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(5) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(7) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(8) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(10) - (14)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(12) - (14)].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
// #line 526 "source\\macro\\macro.y"
    { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[(3) - (12)].asExpr), (yyvsp[(5) - (12)].asExpr), (yyvsp[(7) - (12)].asExpr), (yyvsp[(8) - (12)].asExpr), (yyvsp[(10) - (12)].asExpr), NULL, 2, &iOk );
                                     hb_compExprDelete( (yyvsp[(3) - (12)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(5) - (12)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(7) - (12)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(8) - (12)].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[(10) - (12)].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
// #line 544 "source\\macro\\macro.y"
    {
                                   (yyval.asExpr) = hb_compExprNewArray( (yyvsp[(2) - (3)].asExpr) );

                                   if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[(2) - (3)].asExpr) )
                                   {
                                      s_iPending--;
                                   }
                                 ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
// #line 554 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewFunCall( hb_compExprNewFunName( hb_strdup( "HASH" ) ), (yyvsp[(2) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
// #line 557 "source\\macro\\macro.y"
    { (yyval.asExpr) = NULL; ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
// #line 558 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewArgList( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
// #line 559 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprAddListExpr( (yyvsp[(1) - (5)].asExpr), (yyvsp[(3) - (5)].asExpr) ), (yyvsp[(5) - (5)].asExpr) ); ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
// #line 564 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
// #line 569 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewVar( (yyvsp[(1) - (1)].string) ); ;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
// #line 572 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAlias( (yyvsp[(1) - (2)].string) ); ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
// #line 577 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewMacro( NULL, '&', (yyvsp[(1) - (1)].string) );
                                HB_MACRO_CHECK( (yyval.asExpr) );
                              ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
// #line 580 "source\\macro\\macro.y"
    {  ULONG ulLen = strlen( (yyvsp[(1) - (1)].string) );
                                 char * szVarName = hb_macroTextSubst( (yyvsp[(1) - (1)].string), &ulLen );
                                 if( hb_macroIsIdent( szVarName ) )
                                 {
                                    (yyval.asExpr) = hb_compExprNewVar( szVarName );
                                    // Should always be true since hb_compExprNewVar() returned TRUE.
                                    if( (yyvsp[(1) - (1)].string) != szVarName )
                                    {
                                       hb_xfree( (yyvsp[(1) - (1)].string) );
                                       (yyvsp[(1) - (1)].string) = NULL;
                                    }
                                    HB_MACRO_CHECK( (yyval.asExpr) );
                                 }
                                 else
                                 {
                                    /* invalid variable name
                                     */
                                    HB_TRACE(HB_TR_DEBUG, ("macro -> invalid variable name: %s", (yyvsp[(1) - (1)].string)));
                                    hb_xfree( (yyvsp[(1) - (1)].string) );
                                    (yyvsp[(1) - (1)].string) = NULL;
                                    YYABORT;
                                 }
                              ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
// #line 605 "source\\macro\\macro.y"
    {
                                      if( (yyvsp[(1) - (2)].asExpr)->ExprType == HB_ET_VARIABLE )
                                      {
                                         (yyvsp[(1) - (2)].asExpr)->ExprType = HB_ET_ALIAS;
                                      }
                                      (yyval.asExpr) = (yyvsp[(1) - (2)].asExpr);
                                    ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
// #line 616 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewMacro( (yyvsp[(2) - (2)].asExpr), 0, NULL ); ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
// #line 619 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (2)].asExpr); ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
// #line 626 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAlias( hb_strdup( "FIELD") ); ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
// #line 627 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(3) - (3)].asExpr); ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
// #line 632 "source\\macro\\macro.y"
    { hb_compExprDelete( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
// #line 633 "source\\macro\\macro.y"
    { hb_compExprDelete( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
// #line 634 "source\\macro\\macro.y"
    { hb_compExprDelete( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
// #line 635 "source\\macro\\macro.y"
    { hb_compExprDelete( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
// #line 636 "source\\macro\\macro.y"
    { hb_compExprDelete( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
// #line 639 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewVar( (yyvsp[(1) - (1)].string) ); ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
// #line 640 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
// #line 643 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
// #line 644 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
// #line 645 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
// #line 646 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
// #line 647 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
// #line 648 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
// #line 649 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
// #line 658 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
// #line 659 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
// #line 660 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
// #line 661 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
// #line 662 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[(1) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
// #line 667 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
// #line 668 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
// #line 669 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
// #line 670 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
// #line 671 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
// #line 672 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
// #line 673 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
// #line 674 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
// #line 675 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
// #line 676 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
// #line 677 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
// #line 678 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
// #line 679 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
// #line 680 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
// #line 681 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
// #line 682 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
// #line 683 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
// #line 684 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
// #line 685 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
// #line 688 "source\\macro\\macro.y"
    { (yyval.string) = (yyvsp[(1) - (2)].string); ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
// #line 689 "source\\macro\\macro.y"
    {
                                               (yyval.string) = hb_xstrcpy( NULL, (yyvsp[(1) - (3)].string), ".", (yyvsp[(2) - (3)].string), NULL );
                                               hb_xfree( (yyvsp[(1) - (3)].string) );
                                               (yyvsp[(1) - (3)].string) = NULL;
                                             ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
// #line 697 "source\\macro\\macro.y"
    {
                                            (yyval.asExpr) = hb_compExprNewFunCall( hb_compExprNewFunName( (yyvsp[(1) - (4)].string) ), (yyvsp[(3) - (4)].asExpr), HB_MACRO_PARAM );
                                            HB_MACRO_CHECK( (yyval.asExpr) );

                                            if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[(3) - (4)].asExpr) )
                                            {
                                               s_iPending--;
                                            }
                                          ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
// #line 706 "source\\macro\\macro.y"
    {
                                                        (yyval.asExpr) = hb_compExprNewFunCall( hb_compExprNewNamespaceFunName( (yyvsp[(1) - (5)].string), (yyvsp[(2) - (5)].string) ), (yyvsp[(4) - (5)].asExpr), HB_MACRO_PARAM );
                                                        HB_MACRO_CHECK( (yyval.asExpr) );

                                                        if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[(4) - (5)].asExpr) )
                                                        {
                                                           s_iPending--;
                                                        }
                                                      ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
// #line 715 "source\\macro\\macro.y"
    {
                                            (yyval.asExpr) = hb_compExprNewFunCall( (yyvsp[(1) - (4)].asExpr), (yyvsp[(3) - (4)].asExpr), HB_MACRO_PARAM );
                                            HB_MACRO_CHECK( (yyval.asExpr) );

                                            if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[(3) - (4)].asExpr) )
                                            {
                                               s_iPending--;
                                            }
                                          ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
// #line 724 "source\\macro\\macro.y"
    {
                                            hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );

                                            if( yychar == IDENTIFIER && yylval.string )
                                            {
                                               hb_xfree( yylval.string );
                                               yylval.string = NULL;
                                            }

                                            while ( s_iPending )
                                            {
                                               hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );
                                            }

                                            YYABORT;
                                          ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
// #line 742 "source\\macro\\macro.y"
    {
                                            (yyval.asExpr) = hb_compExprNewArgList( (yyvsp[(1) - (1)].asExpr) );

                                            if( s_iPending <= HB_MAX_PENDING_MACRO_EXP )
                                            {
                                               s_Pending[ s_iPending++ ] = (yyval.asExpr);
                                            }
                                          ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
// #line 750 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
// #line 753 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewVarRef( (yyvsp[(2) - (2)].string) ); ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
// #line 756 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
// #line 757 "source\\macro\\macro.y"
    {
                                          switch( (yyvsp[(2) - (2)].asExpr)->ExprType )
                                          {
                                             case HB_ET_VARIABLE:
                                               (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr);
                                               (yyval.asExpr)->ExprType = HB_ET_VARREF;
                                               (yyval.asExpr)->ValType = HB_EV_VARREF;
                                               break;

                                             case HB_ET_ALIASVAR:
                                             {
                                               char *szAlias = (yyvsp[(2) - (2)].asExpr)->value.asAlias.pAlias->value.asSymbol.szName;

                                               if( strcmp( szAlias, "M" ) == 0 || strncmp( szAlias, "MEMVAR", 4 > strlen( szAlias ) ? 4 : strlen( szAlias ) ) == 0 )
                                               {
                                                  (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr)->value.asAlias.pVar;

                                                  (yyvsp[(2) - (2)].asExpr)->value.asAlias.pVar = NULL;
                                                  hb_compExprDelete( (yyvsp[(2) - (2)].asExpr), HB_MACRO_PARAM  );

                                                  if( (yyval.asExpr)->ExprType == HB_ET_MACRO )
                                                  {
                                                     (yyval.asExpr)->value.asMacro.SubType = HB_ET_MACRO_VAR_REF;
                                                  }
                                                  else
                                                  {
                                                     (yyval.asExpr)->ExprType = HB_ET_MEMVARREF;
                                                     (yyval.asExpr)->ValType = HB_EV_VARREF;
                                                  }
                                               }
                                               break;
                                             }

                                             case HB_ET_FUNCALL:
                                                (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr)->value.asFunCall.pFunName;

                                                (yyvsp[(2) - (2)].asExpr)->value.asFunCall.pFunName = NULL;
                                                hb_compExprDelete( (yyvsp[(2) - (2)].asExpr), HB_MACRO_PARAM  );

                                                (yyval.asExpr)->ExprType = HB_ET_FUNREF;
                                                (yyval.asExpr)->ValType = HB_EV_FUNREF;
                                                break;

                                             case HB_ET_SEND:
                                               (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr);
                                               (yyval.asExpr)->value.asMessage.bByRef = TRUE;
                                               break;

                                             case HB_ET_MACRO:
                                               (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr);
                                               (yyval.asExpr)->value.asMacro.SubType = HB_ET_MACRO_VAR_REF;
                                               break;

                                             case HB_ET_ARRAYAT:
                                               (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr);
                                               (yyval.asExpr)->value.asList.bByRef = TRUE;
                                               break;

                                             default:
                                               hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );
                                               (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr);
                                          }
                                       ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
// #line 824 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewFunName( (yyvsp[(1) - (1)].string) ); ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
// #line 825 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); (yyvsp[(1) - (1)].asExpr)->value.asMacro.SubType = HB_ET_MACRO_SYMBOL; ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
// #line 826 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); (yyvsp[(1) - (1)].asExpr)->value.asMacro.SubType = HB_ET_MACRO_SYMBOL; ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
// #line 829 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewSendExp( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
// #line 832 "source\\macro\\macro.y"
    {
                                            (yyval.asExpr) = hb_compExprNewWithSendExp( (yyvsp[(2) - (2)].asExpr) );
                                         ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
// #line 839 "source\\macro\\macro.y"
    {
                                               (yyval.asExpr) = hb_compExprNewMethodCall( (yyvsp[(1) - (4)].asExpr), (yyvsp[(3) - (4)].asExpr) );

                                               if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[(3) - (4)].asExpr) )
                                               {
                                                  s_iPending--;
                                               }
                                             ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
// #line 849 "source\\macro\\macro.y"
    {
                                            (yyval.asExpr) = hb_compExprNewWithMethodCall( (yyvsp[(1) - (4)].asExpr), (yyvsp[(3) - (4)].asExpr) );
                                         ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
// #line 856 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
// #line 857 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
// #line 858 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
// #line 859 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
// #line 860 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
// #line 861 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
// #line 862 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
// #line 863 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
// #line 864 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
// #line 865 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
// #line 866 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
// #line 867 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
// #line 868 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
// #line 869 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
// #line 870 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
// #line 871 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
// #line 872 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
// #line 873 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
// #line 874 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
// #line 875 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
// #line 876 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
// #line 877 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
// #line 878 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
// #line 879 "source\\macro\\macro.y"
    { HB_MACRO_IFENABLED( (yyval.asExpr), (yyvsp[(1) - (1)].asExpr), HB_SM_HARBOUR ); ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
// #line 880 "source\\macro\\macro.y"
    { HB_MACRO_IFENABLED( (yyval.asExpr), (yyvsp[(1) - (1)].asExpr), HB_SM_HARBOUR ); ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
// #line 881 "source\\macro\\macro.y"
    { HB_MACRO_IFENABLED( (yyval.asExpr), (yyvsp[(1) - (1)].asExpr), HB_SM_HARBOUR ); ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
// #line 882 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
// #line 883 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
// #line 884 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
// #line 885 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
// #line 888 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); HB_MACRO_CHECK( (yyval.asExpr) ) ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
// #line 889 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); HB_MACRO_CHECK( (yyval.asExpr) ) ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
// #line 892 "source\\macro\\macro.y"
    {
                                if( !(HB_MACRO_DATA->Flags & HB_MACRO_GEN_LIST) )
                                {
                                   HB_TRACE(HB_TR_DEBUG, ("macro -> invalid expression: %s", HB_MACRO_DATA->string));
                                   hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );
                                   hb_compExprDelete( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM );
                                   YYABORT;
                                }
                             ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
// #line 901 "source\\macro\\macro.y"
    {
                                HB_MACRO_DATA->iListElements = 1;
                                (yyval.asExpr) = hb_compExprAddListExpr( ( HB_MACRO_DATA->Flags & HB_MACRO_GEN_PARE ) ? hb_compExprNewList( (yyvsp[(1) - (4)].asExpr) ) : hb_compExprNewArgList( (yyvsp[(1) - (4)].asExpr) ), (yyvsp[(4) - (4)].asExpr) );
                             ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
// #line 907 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
// #line 908 "source\\macro\\macro.y"
    { HB_MACRO_DATA->iListElements++; (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
// #line 911 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewEmpty(); ;}
    break;

  case 159:

/* Line 1455 of yacc.c  */
// #line 944 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewPostInc( (yyvsp[(0) - (1)].asExpr) ); ;}
    break;

  case 160:

/* Line 1455 of yacc.c  */
// #line 945 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewPostDec( (yyvsp[(0) - (1)].asExpr) ); ;}
    break;

  case 161:

/* Line 1455 of yacc.c  */
// #line 951 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 162:

/* Line 1455 of yacc.c  */
// #line 954 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewPreInc( (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 163:

/* Line 1455 of yacc.c  */
// #line 955 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewPreDec( (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 164:

/* Line 1455 of yacc.c  */
// #line 958 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewNot( (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 165:

/* Line 1455 of yacc.c  */
// #line 959 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewNegate( (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 166:

/* Line 1455 of yacc.c  */
// #line 960 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(2) - (2)].asExpr); ;}
    break;

  case 167:

/* Line 1455 of yacc.c  */
// #line 963 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 168:

/* Line 1455 of yacc.c  */
// #line 964 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 169:

/* Line 1455 of yacc.c  */
// #line 965 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 170:

/* Line 1455 of yacc.c  */
// #line 966 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 171:

/* Line 1455 of yacc.c  */
// #line 967 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 172:

/* Line 1455 of yacc.c  */
// #line 968 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 173:

/* Line 1455 of yacc.c  */
// #line 969 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 174:

/* Line 1455 of yacc.c  */
// #line 970 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 175:

/* Line 1455 of yacc.c  */
// #line 971 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 176:

/* Line 1455 of yacc.c  */
// #line 972 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 177:

/* Line 1455 of yacc.c  */
// #line 973 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 178:

/* Line 1455 of yacc.c  */
// #line 974 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 179:

/* Line 1455 of yacc.c  */
// #line 975 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 180:

/* Line 1455 of yacc.c  */
// #line 976 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 181:

/* Line 1455 of yacc.c  */
// #line 977 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 182:

/* Line 1455 of yacc.c  */
// #line 978 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 183:

/* Line 1455 of yacc.c  */
// #line 979 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 184:

/* Line 1455 of yacc.c  */
// #line 980 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 185:

/* Line 1455 of yacc.c  */
// #line 981 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 186:

/* Line 1455 of yacc.c  */
// #line 982 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 187:

/* Line 1455 of yacc.c  */
// #line 983 "source\\macro\\macro.y"
    { HB_MACRO_IFENABLED( (yyval.asExpr), hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ), HB_SM_HARBOUR ); ;}
    break;

  case 188:

/* Line 1455 of yacc.c  */
// #line 984 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 189:

/* Line 1455 of yacc.c  */
// #line 985 "source\\macro\\macro.y"
    { HB_MACRO_IFENABLED( (yyval.asExpr), hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ), HB_SM_HARBOUR ); ;}
    break;

  case 190:

/* Line 1455 of yacc.c  */
// #line 986 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAssign( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 191:

/* Line 1455 of yacc.c  */
// #line 989 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewPlusEq( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 192:

/* Line 1455 of yacc.c  */
// #line 992 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMinusEq( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 193:

/* Line 1455 of yacc.c  */
// #line 995 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMultEq( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 194:

/* Line 1455 of yacc.c  */
// #line 998 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewDivEq( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 195:

/* Line 1455 of yacc.c  */
// #line 1001 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewModEq( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
// #line 1004 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewExpEq( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
// #line 1007 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 198:

/* Line 1455 of yacc.c  */
// #line 1008 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 199:

/* Line 1455 of yacc.c  */
// #line 1009 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
// #line 1010 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 201:

/* Line 1455 of yacc.c  */
// #line 1011 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
// #line 1012 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (1)].asExpr); ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
// #line 1015 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewPlus( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
// #line 1016 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMinus( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 205:

/* Line 1455 of yacc.c  */
// #line 1017 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMult( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 206:

/* Line 1455 of yacc.c  */
// #line 1018 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewDiv( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 207:

/* Line 1455 of yacc.c  */
// #line 1019 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMod( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 208:

/* Line 1455 of yacc.c  */
// #line 1020 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewPower( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 209:

/* Line 1455 of yacc.c  */
// #line 1021 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitAnd( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 210:

/* Line 1455 of yacc.c  */
// #line 1022 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitOr( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 211:

/* Line 1455 of yacc.c  */
// #line 1023 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitXOr( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 212:

/* Line 1455 of yacc.c  */
// #line 1024 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitShiftR( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 213:

/* Line 1455 of yacc.c  */
// #line 1025 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitShiftL( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 214:

/* Line 1455 of yacc.c  */
// #line 1028 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewAnd( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 215:

/* Line 1455 of yacc.c  */
// #line 1029 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewOr( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 216:

/* Line 1455 of yacc.c  */
// #line 1032 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewEQ( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 217:

/* Line 1455 of yacc.c  */
// #line 1033 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewLT( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 218:

/* Line 1455 of yacc.c  */
// #line 1034 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewGT( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 219:

/* Line 1455 of yacc.c  */
// #line 1035 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewLE( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 220:

/* Line 1455 of yacc.c  */
// #line 1036 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewGE( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 221:

/* Line 1455 of yacc.c  */
// #line 1037 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewNE( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 222:

/* Line 1455 of yacc.c  */
// #line 1038 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewNE( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 223:

/* Line 1455 of yacc.c  */
// #line 1039 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewIN( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 224:

/* Line 1455 of yacc.c  */
// #line 1040 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewEqual( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 225:

/* Line 1455 of yacc.c  */
// #line 1041 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewLike( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 226:

/* Line 1455 of yacc.c  */
// #line 1042 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMatch( (yyvsp[(1) - (3)].asExpr) ), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 227:

/* Line 1455 of yacc.c  */
// #line 1045 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (2)].asExpr); ;}
    break;

  case 228:

/* Line 1455 of yacc.c  */
// #line 1051 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewArrayAt( (yyvsp[(0) - (2)].asExpr), (yyvsp[(2) - (2)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 229:

/* Line 1455 of yacc.c  */
// #line 1052 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewArrayAt( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 230:

/* Line 1455 of yacc.c  */
// #line 1053 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewArrayAt( (yyvsp[(1) - (4)].asExpr), (yyvsp[(4) - (4)].asExpr), HB_MACRO_PARAM ); ;}
    break;

  case 231:

/* Line 1455 of yacc.c  */
// #line 1057 "source\\macro\\macro.y"
    {
                    (yyval.asExpr) = hb_compExprNewCodeBlock();

                    if( s_iPending <= HB_MAX_PENDING_MACRO_EXP )
                    {
                       s_Pending[ s_iPending++ ] = (yyval.asExpr);
                    }
                  ;}
    break;

  case 232:

/* Line 1455 of yacc.c  */
// #line 1066 "source\\macro\\macro.y"
    {
                    (yyval.asExpr) = (yyvsp[(3) - (7)].asExpr);

                    if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyval.asExpr) )
                    {
                       s_iPending--;
                    }
                  ;}
    break;

  case 233:

/* Line 1455 of yacc.c  */
// #line 1075 "source\\macro\\macro.y"
    {
                    (yyval.asExpr) = hb_compExprNewCodeBlock();

                    if( s_iPending <= HB_MAX_PENDING_MACRO_EXP )
                    {
                       s_Pending[ s_iPending++ ] = (yyval.asExpr);
                    }
                  ;}
    break;

  case 234:

/* Line 1455 of yacc.c  */
// #line 1084 "source\\macro\\macro.y"
    {
                    (yyval.asExpr) = (yyvsp[(3) - (7)].asExpr);

                    if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyval.asExpr) )
                    {
                       s_iPending--;
                    }
                  ;}
    break;

  case 235:

/* Line 1455 of yacc.c  */
// #line 1096 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[(-2) - (1)].asExpr), (yyvsp[(1) - (1)].asExpr) ); ;}
    break;

  case 236:

/* Line 1455 of yacc.c  */
// #line 1097 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[(-2) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 237:

/* Line 1455 of yacc.c  */
// #line 1103 "source\\macro\\macro.y"
    { (yyval.asExpr) = NULL; ;}
    break;

  case 238:

/* Line 1455 of yacc.c  */
// #line 1106 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprCBVarAdd( (yyvsp[(0) - (1)].asExpr), (yyvsp[(1) - (1)].string), HB_MACRO_PARAM ); ;}
    break;

  case 239:

/* Line 1455 of yacc.c  */
// #line 1107 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprCBVarAdd( (yyvsp[(0) - (3)].asExpr), (yyvsp[(3) - (3)].string), HB_MACRO_PARAM ); HB_MACRO_CHECK( (yyval.asExpr) ); ;}
    break;

  case 240:

/* Line 1455 of yacc.c  */
// #line 1110 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewList( (yyvsp[(2) - (2)].asExpr) ); ;}
    break;

  case 241:

/* Line 1455 of yacc.c  */
// #line 1111 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[(1) - (3)].asExpr), (yyvsp[(3) - (3)].asExpr) ); ;}
    break;

  case 242:

/* Line 1455 of yacc.c  */
// #line 1114 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (2)].asExpr); ;}
    break;

  case 243:

/* Line 1455 of yacc.c  */
// #line 1115 "source\\macro\\macro.y"
    {
                                            hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );

                                            hb_compExprDelete( (yyvsp[(1) - (2)].asExpr), HB_MACRO_PARAM );

                                            while ( s_iPending )
                                            {
                                               hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );
                                            }

                                            if( yychar == IDENTIFIER && yylval.string )
                                            {
                                               hb_xfree( yylval.string );
                                               yylval.string = NULL;
                                            }

                                            YYABORT;
                                          ;}
    break;

  case 244:

/* Line 1455 of yacc.c  */
// #line 1135 "source\\macro\\macro.y"
    { (yyval.asExpr) = (yyvsp[(1) - (2)].asExpr); ;}
    break;

  case 245:

/* Line 1455 of yacc.c  */
// #line 1139 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewList( (yyvsp[(3) - (6)].asExpr) ), (yyvsp[(5) - (6)].asExpr) ); ;}
    break;

  case 246:

/* Line 1455 of yacc.c  */
// #line 1141 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewIIF( hb_compExprAddListExpr( (yyvsp[(7) - (9)].asExpr), (yyvsp[(8) - (9)].asExpr) ) ); ;}
    break;

  case 247:

/* Line 1455 of yacc.c  */
// #line 1144 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewList( (yyvsp[(3) - (6)].asExpr) ), (yyvsp[(5) - (6)].asExpr) ); ;}
    break;

  case 248:

/* Line 1455 of yacc.c  */
// #line 1146 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewIIF( hb_compExprAddListExpr( (yyvsp[(7) - (9)].asExpr), (yyvsp[(8) - (9)].asExpr) ) ); ;}
    break;

  case 249:

/* Line 1455 of yacc.c  */
// #line 1149 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewList( (yyvsp[(3) - (5)].asExpr) ), (yyvsp[(5) - (5)].asExpr) ); ;}
    break;

  case 250:

/* Line 1455 of yacc.c  */
// #line 1151 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewIIF( hb_compExprAddListExpr( (yyvsp[(6) - (7)].asExpr), hb_compExprNew( HB_ET_NONE ) ) ); ;}
    break;

  case 251:

/* Line 1455 of yacc.c  */
// #line 1154 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewList( (yyvsp[(3) - (5)].asExpr) ), (yyvsp[(5) - (5)].asExpr) ); ;}
    break;

  case 252:

/* Line 1455 of yacc.c  */
// #line 1156 "source\\macro\\macro.y"
    { (yyval.asExpr) = hb_compExprNewIIF( hb_compExprAddListExpr( (yyvsp[(6) - (7)].asExpr), hb_compExprNew( HB_ET_NONE ) ) ); ;}
    break;



/* Line 1455 of yacc.c  */
// #line 4173 "obj\\b32\\macroy.c"
      default: break;
    }
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
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
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
      if (yyn != YYPACT_NINF)
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
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
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



/* Line 1675 of yacc.c  */
// #line 1159 "source\\macro\\macro.y"


#ifdef __WATCOMC__
/* enable warnings for unreachable code */
#pragma warning 13 1
#endif

/*
 ** ------------------------------------------------------------------------ **
 */

int hb_macroYYParse( HB_MACRO_PTR pMacro )
{
   int iResult;
   void * lexBuffer;

   /* AJ: Replace hard coded MT related codes with API to make this file
      common to ST and MT modes
   */
   #if 0
      #ifdef HB_THREAD_SUPPORT
         HB_CRITICAL_LOCK( hb_macroMutex );
      #endif
   #else
      hb_macro_critical_Lock();
   #endif

   // Reset
   s_iPending = 0;

   lexBuffer = hb_compFlexNew( pMacro );

   pMacro->status = HB_MACRO_CONT;
   /* NOTE: bison requires (void *) pointer
    */
   iResult = yyparse( ( void * ) pMacro );

   hb_compFlexDelete( lexBuffer );

   /* AJ: Replace hard coded MT related codes with API to make this file
      common to ST and MT modes
   */
   #if 0
      #ifdef HB_THREAD_SUPPORT
         HB_CRITICAL_UNLOCK( hb_macroMutex );
      #endif
   #else
      hb_macro_critical_UnLock();
   #endif

   return iResult;
}

/* ************************************************************************* */

void yyerror( char * s )
{
   HB_SYMBOL_UNUSED( s );
}

