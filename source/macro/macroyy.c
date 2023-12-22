/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Substitute the type names.  */
#define YYSTYPE         HB_MACRO_YYSTYPE
/* Substitute the variable and function names.  */
#define yyparse         hb_macro_yyparse
#define yylex           hb_macro_yylex
#define yyerror         hb_macro_yyerror
#define yydebug         hb_macro_yydebug
#define yynerrs         hb_macro_yynerrs

/* First part of user prologue.  */
#line 6 "../../macro.y"

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

/* Compile using: bison -d macro.y */

/* to pacify some warnings in BCC */
#if defined( __POCC__ )
   #pragma warn(push)
   #pragma warn(disable:2154)
#elif defined( _MSC_VER )
   #pragma warning (disable:4065)
#elif defined( __BORLANDC__ )
   #if !defined( __STDC__ )
     #define __STDC__
   #endif
   #pragma warn -aus
   #pragma warn -ccc
   #pragma warn -rch
#endif

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

static PHB_EXPR s_Pending[ HB_MAX_PENDING_MACRO_EXP ];
static int      s_iPending;

/* yacc/lex related definitions
 */

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

#line 216 "macroy.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "macroy.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_IDENTIFIER = 3,                 /* IDENTIFIER  */
  YYSYMBOL_NIL = 4,                        /* NIL  */
  YYSYMBOL_NUM_DOUBLE = 5,                 /* NUM_DOUBLE  */
  YYSYMBOL_INASSIGN = 6,                   /* INASSIGN  */
  YYSYMBOL_NUM_LONG = 7,                   /* NUM_LONG  */
  YYSYMBOL_IIF = 8,                        /* IIF  */
  YYSYMBOL_IF = 9,                         /* IF  */
  YYSYMBOL_LITERAL = 10,                   /* LITERAL  */
  YYSYMBOL_TRUEVALUE = 11,                 /* TRUEVALUE  */
  YYSYMBOL_FALSEVALUE = 12,                /* FALSEVALUE  */
  YYSYMBOL_AND = 13,                       /* AND  */
  YYSYMBOL_OR = 14,                        /* OR  */
  YYSYMBOL_NOT = 15,                       /* NOT  */
  YYSYMBOL_EQ = 16,                        /* EQ  */
  YYSYMBOL_NE1 = 17,                       /* NE1  */
  YYSYMBOL_NE2 = 18,                       /* NE2  */
  YYSYMBOL_INC = 19,                       /* INC  */
  YYSYMBOL_DEC = 20,                       /* DEC  */
  YYSYMBOL_ALIASOP = 21,                   /* ALIASOP  */
  YYSYMBOL_HASHOP = 22,                    /* HASHOP  */
  YYSYMBOL_SELF = 23,                      /* SELF  */
  YYSYMBOL_LE = 24,                        /* LE  */
  YYSYMBOL_GE = 25,                        /* GE  */
  YYSYMBOL_FIELD = 26,                     /* FIELD  */
  YYSYMBOL_MACROVAR = 27,                  /* MACROVAR  */
  YYSYMBOL_MACROTEXT = 28,                 /* MACROTEXT  */
  YYSYMBOL_H12AM = 29,                     /* H12AM  */
  YYSYMBOL_H12PM = 30,                     /* H12PM  */
  YYSYMBOL_PLUSEQ = 31,                    /* PLUSEQ  */
  YYSYMBOL_MINUSEQ = 32,                   /* MINUSEQ  */
  YYSYMBOL_MULTEQ = 33,                    /* MULTEQ  */
  YYSYMBOL_DIVEQ = 34,                     /* DIVEQ  */
  YYSYMBOL_POWER = 35,                     /* POWER  */
  YYSYMBOL_EXPEQ = 36,                     /* EXPEQ  */
  YYSYMBOL_MODEQ = 37,                     /* MODEQ  */
  YYSYMBOL_CBMARKER = 38,                  /* CBMARKER  */
  YYSYMBOL_BITAND = 39,                    /* BITAND  */
  YYSYMBOL_BITOR = 40,                     /* BITOR  */
  YYSYMBOL_BITXOR = 41,                    /* BITXOR  */
  YYSYMBOL_BITSHIFTR = 42,                 /* BITSHIFTR  */
  YYSYMBOL_BITSHIFTL = 43,                 /* BITSHIFTL  */
  YYSYMBOL_POST = 44,                      /* POST  */
  YYSYMBOL_45_ = 45,                       /* '='  */
  YYSYMBOL_46_ = 46,                       /* '<'  */
  YYSYMBOL_47_ = 47,                       /* '>'  */
  YYSYMBOL_48_ = 48,                       /* '$'  */
  YYSYMBOL_LIKE = 49,                      /* LIKE  */
  YYSYMBOL_MATCH = 50,                     /* MATCH  */
  YYSYMBOL_51_ = 51,                       /* '+'  */
  YYSYMBOL_52_ = 52,                       /* '-'  */
  YYSYMBOL_53_ = 53,                       /* '*'  */
  YYSYMBOL_54_ = 54,                       /* '/'  */
  YYSYMBOL_55_ = 55,                       /* '%'  */
  YYSYMBOL_UNARY = 56,                     /* UNARY  */
  YYSYMBOL_PRE = 57,                       /* PRE  */
  YYSYMBOL_58_n_ = 58,                     /* '\n'  */
  YYSYMBOL_59_ = 59,                       /* '{'  */
  YYSYMBOL_60_ = 60,                       /* '}'  */
  YYSYMBOL_61_ = 61,                       /* ':'  */
  YYSYMBOL_62_ = 62,                       /* ','  */
  YYSYMBOL_63_ = 63,                       /* '&'  */
  YYSYMBOL_64_ = 64,                       /* '.'  */
  YYSYMBOL_65_ = 65,                       /* '('  */
  YYSYMBOL_66_ = 66,                       /* ')'  */
  YYSYMBOL_67_ = 67,                       /* '@'  */
  YYSYMBOL_68_ = 68,                       /* ']'  */
  YYSYMBOL_69_ = 69,                       /* '['  */
  YYSYMBOL_YYACCEPT = 70,                  /* $accept  */
  YYSYMBOL_Main = 71,                      /* Main  */
  YYSYMBOL_IdentName = 72,                 /* IdentName  */
  YYSYMBOL_NumValue = 73,                  /* NumValue  */
  YYSYMBOL_NumAlias = 74,                  /* NumAlias  */
  YYSYMBOL_NilValue = 75,                  /* NilValue  */
  YYSYMBOL_LiteralValue = 76,              /* LiteralValue  */
  YYSYMBOL_Logical = 77,                   /* Logical  */
  YYSYMBOL_SelfValue = 78,                 /* SelfValue  */
  YYSYMBOL_Date = 79,                      /* Date  */
  YYSYMBOL_DateTime = 80,                  /* DateTime  */
  YYSYMBOL_Array = 81,                     /* Array  */
  YYSYMBOL_Hash = 82,                      /* Hash  */
  YYSYMBOL_HashList = 83,                  /* HashList  */
  YYSYMBOL_ArrayAt = 84,                   /* ArrayAt  */
  YYSYMBOL_Variable = 85,                  /* Variable  */
  YYSYMBOL_VarAlias = 86,                  /* VarAlias  */
  YYSYMBOL_MacroVar = 87,                  /* MacroVar  */
  YYSYMBOL_MacroVarAlias = 88,             /* MacroVarAlias  */
  YYSYMBOL_MacroExpr = 89,                 /* MacroExpr  */
  YYSYMBOL_MacroExprAlias = 90,            /* MacroExprAlias  */
  YYSYMBOL_FieldAlias = 91,                /* FieldAlias  */
  YYSYMBOL_FieldVarAlias = 92,             /* FieldVarAlias  */
  YYSYMBOL_AliasId = 93,                   /* AliasId  */
  YYSYMBOL_AliasVar = 94,                  /* AliasVar  */
  YYSYMBOL_AliasExpr = 95,                 /* AliasExpr  */
  YYSYMBOL_VariableAt = 96,                /* VariableAt  */
  YYSYMBOL_NamespacePath = 97,             /* NamespacePath  */
  YYSYMBOL_FunCall = 98,                   /* FunCall  */
  YYSYMBOL_ArgList = 99,                   /* ArgList  */
  YYSYMBOL_ByRefArg = 100,                 /* ByRefArg  */
  YYSYMBOL_Argument = 101,                 /* Argument  */
  YYSYMBOL_SendId = 102,                   /* SendId  */
  YYSYMBOL_ObjectData = 103,               /* ObjectData  */
  YYSYMBOL_WithData = 104,                 /* WithData  */
  YYSYMBOL_ObjectMethod = 105,             /* ObjectMethod  */
  YYSYMBOL_WithMethod = 106,               /* WithMethod  */
  YYSYMBOL_SimpleExpression = 107,         /* SimpleExpression  */
  YYSYMBOL_Expression = 108,               /* Expression  */
  YYSYMBOL_RootParamList = 109,            /* RootParamList  */
  YYSYMBOL_110_1 = 110,                    /* $@1  */
  YYSYMBOL_AsParamList = 111,              /* AsParamList  */
  YYSYMBOL_EmptyExpression = 112,          /* EmptyExpression  */
  YYSYMBOL_LeftExpression = 113,           /* LeftExpression  */
  YYSYMBOL_PostOp = 114,                   /* PostOp  */
  YYSYMBOL_ExprPostOp = 115,               /* ExprPostOp  */
  YYSYMBOL_ExprPreOp = 116,                /* ExprPreOp  */
  YYSYMBOL_ExprUnary = 117,                /* ExprUnary  */
  YYSYMBOL_ExprAssign = 118,               /* ExprAssign  */
  YYSYMBOL_ExprPlusEq = 119,               /* ExprPlusEq  */
  YYSYMBOL_ExprMinusEq = 120,              /* ExprMinusEq  */
  YYSYMBOL_ExprMultEq = 121,               /* ExprMultEq  */
  YYSYMBOL_ExprDivEq = 122,                /* ExprDivEq  */
  YYSYMBOL_ExprModEq = 123,                /* ExprModEq  */
  YYSYMBOL_ExprExpEq = 124,                /* ExprExpEq  */
  YYSYMBOL_ExprOperEq = 125,               /* ExprOperEq  */
  YYSYMBOL_ExprMath = 126,                 /* ExprMath  */
  YYSYMBOL_ExprBool = 127,                 /* ExprBool  */
  YYSYMBOL_ExprRelation = 128,             /* ExprRelation  */
  YYSYMBOL_ArrayIndex = 129,               /* ArrayIndex  */
  YYSYMBOL_IndexList = 130,                /* IndexList  */
  YYSYMBOL_CodeBlock = 131,                /* CodeBlock  */
  YYSYMBOL_132_2 = 132,                    /* @2  */
  YYSYMBOL_133_3 = 133,                    /* @3  */
  YYSYMBOL_BlockExpList = 134,             /* BlockExpList  */
  YYSYMBOL_BlockNoVar = 135,               /* BlockNoVar  */
  YYSYMBOL_BlockVarList = 136,             /* BlockVarList  */
  YYSYMBOL_ExpList = 137,                  /* ExpList  */
  YYSYMBOL_PareExpList = 138,              /* PareExpList  */
  YYSYMBOL_PareExpListAlias = 139,         /* PareExpListAlias  */
  YYSYMBOL_IfInline = 140,                 /* IfInline  */
  YYSYMBOL_141_4 = 141,                    /* @4  */
  YYSYMBOL_142_5 = 142,                    /* @5  */
  YYSYMBOL_143_6 = 143,                    /* @6  */
  YYSYMBOL_144_7 = 144                     /* @7  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;


/* Second part of user prologue.  */
#line 177 "../../macro.y"


/* This must be placed after the above union - the union is
 * typedef-ined to YYSTYPE
 */
extern int  yylex( YYSTYPE *, PHB_MACRO );    /* main lex token function, called by yyparse() */
extern int  yyparse( PHB_MACRO );             /* main yacc parsing function                   */
extern void yyerror( PHB_MACRO, char * );     /* parsing error management function            */


#line 405 "macroy.c"


#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined HB_MACRO_YYSTYPE_IS_TRIVIAL && HB_MACRO_YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  110
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1422

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  70
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  75
/* YYNRULES -- Number of rules.  */
#define YYNRULES  252
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  377

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   303


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      58,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    48,    55,    63,     2,
      65,    66,    53,    51,    62,    52,    64,    54,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    61,     2,
      46,    45,    47,     2,    67,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    69,     2,    68,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    59,     2,    60,     2,     2,     2,     2,
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

#if HB_MACRO_YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   274,   274,   284,   294,   311,   318,   341,   359,   379,
     384,   385,   388,   393,   398,   403,   404,   409,   414,   423,
     437,   450,   464,   477,   491,   510,   518,   521,   522,   523,
     528,   533,   536,   541,   544,   569,   580,   583,   590,   591,
     596,   597,   598,   599,   600,   603,   604,   607,   608,   609,
     610,   611,   612,   613,   622,   623,   624,   625,   626,   631,
     632,   633,   634,   635,   636,   637,   638,   639,   640,   641,
     642,   643,   644,   645,   646,   647,   648,   649,   652,   653,
     661,   668,   675,   682,   698,   704,   707,   710,   711,   778,
     779,   780,   783,   786,   793,   801,   806,   807,   808,   809,
     810,   811,   812,   813,   814,   815,   816,   817,   818,   819,
     820,   821,   822,   823,   824,   825,   826,   827,   828,   829,
     830,   831,   832,   833,   834,   835,   836,   839,   840,   843,
     843,   858,   859,   862,   863,   866,   867,   868,   869,   870,
     871,   872,   873,   874,   875,   876,   877,   878,   879,   880,
     881,   882,   883,   884,   885,   886,   887,   888,   889,   895,
     896,   902,   905,   906,   909,   910,   911,   914,   915,   916,
     917,   918,   919,   920,   921,   922,   923,   924,   925,   926,
     927,   928,   929,   930,   931,   932,   933,   934,   935,   936,
     937,   940,   943,   946,   949,   952,   955,   958,   959,   960,
     961,   962,   963,   966,   967,   968,   969,   970,   971,   972,
     973,   974,   975,   976,   979,   980,   983,   984,   985,   986,
     987,   988,   989,   990,   991,   992,   993,   996,  1002,  1003,
    1004,  1008,  1007,  1024,  1023,  1043,  1044,  1050,  1053,  1054,
    1057,  1058,  1061,  1062,  1080,  1084,  1083,  1089,  1088,  1094,
    1093,  1099,  1098
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if HB_MACRO_YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "IDENTIFIER", "NIL",
  "NUM_DOUBLE", "INASSIGN", "NUM_LONG", "IIF", "IF", "LITERAL",
  "TRUEVALUE", "FALSEVALUE", "AND", "OR", "NOT", "EQ", "NE1", "NE2", "INC",
  "DEC", "ALIASOP", "HASHOP", "SELF", "LE", "GE", "FIELD", "MACROVAR",
  "MACROTEXT", "H12AM", "H12PM", "PLUSEQ", "MINUSEQ", "MULTEQ", "DIVEQ",
  "POWER", "EXPEQ", "MODEQ", "CBMARKER", "BITAND", "BITOR", "BITXOR",
  "BITSHIFTR", "BITSHIFTL", "POST", "'='", "'<'", "'>'", "'$'", "LIKE",
  "MATCH", "'+'", "'-'", "'*'", "'/'", "'%'", "UNARY", "PRE", "'\\n'",
  "'{'", "'}'", "':'", "','", "'&'", "'.'", "'('", "')'", "'@'", "']'",
  "'['", "$accept", "Main", "IdentName", "NumValue", "NumAlias",
  "NilValue", "LiteralValue", "Logical", "SelfValue", "Date", "DateTime",
  "Array", "Hash", "HashList", "ArrayAt", "Variable", "VarAlias",
  "MacroVar", "MacroVarAlias", "MacroExpr", "MacroExprAlias", "FieldAlias",
  "FieldVarAlias", "AliasId", "AliasVar", "AliasExpr", "VariableAt",
  "NamespacePath", "FunCall", "ArgList", "ByRefArg", "Argument", "SendId",
  "ObjectData", "WithData", "ObjectMethod", "WithMethod",
  "SimpleExpression", "Expression", "RootParamList", "$@1", "AsParamList",
  "EmptyExpression", "LeftExpression", "PostOp", "ExprPostOp", "ExprPreOp",
  "ExprUnary", "ExprAssign", "ExprPlusEq", "ExprMinusEq", "ExprMultEq",
  "ExprDivEq", "ExprModEq", "ExprExpEq", "ExprOperEq", "ExprMath",
  "ExprBool", "ExprRelation", "ArrayIndex", "IndexList", "CodeBlock", "@2",
  "@3", "BlockExpList", "BlockNoVar", "BlockVarList", "ExpList",
  "PareExpList", "PareExpListAlias", "IfInline", "@4", "@5", "@6", "@7", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-111)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-234)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     366,  -111,    32,  -111,  -111,    -6,   -58,   -42,  -111,  -111,
    -111,   895,   895,   895,  -111,    39,  -111,  -111,   895,   895,
     431,     2,    23,   895,   895,    66,    37,   274,    13,   545,
     554,   564,   598,   607,   641,   650,   905,   942,   660,    13,
      73,    13,   221,    13,    24,    53,   694,   703,   949,    67,
     737,  -111,    21,   255,   506,   746,   756,  -111,   839,  -111,
      35,  -111,   281,  -111,  -111,  -111,  -111,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,   790,     9,   441,
      13,   799,  -111,  -111,   895,   895,  -111,  1215,  -111,  -111,
      77,  -111,  -111,  -111,    48,   115,   895,   -54,    70,  -111,
    1089,  -111,  -111,  -111,  -111,  -111,  1132,  -111,     4,  1132,
    -111,  -111,  -111,   336,   895,  -111,  -111,  -111,  -111,   895,
     895,  -111,   -40,   895,  -111,   895,  -111,   895,  -111,   895,
    -111,   895,  -111,   895,  -111,   895,   895,   895,  -111,  -111,
    -111,   895,  -111,   496,  -111,  -111,  -111,   895,  -111,  -111,
    -111,  -111,    -6,   118,  -111,  -111,   119,  -111,   127,  -111,
    -111,   133,  -111,  -111,   895,  -111,   895,  -111,   895,    12,
     895,  -111,  -111,   895,   496,  -111,   895,   496,  -111,   895,
    -111,   895,  -111,  -111,   895,   895,   895,   895,   895,   895,
     895,   895,   895,   895,   895,   895,   895,   895,   895,   895,
     895,   895,   895,   895,   895,   895,   895,   895,  -111,   496,
    -111,  -111,   895,   895,   895,   895,   895,   895,     2,  -111,
     895,  -111,  -111,   895,  -111,   895,  -111,  -111,  -111,  -111,
     895,  -111,  1003,  1046,  -111,  -111,   104,  -111,    67,  -111,
     895,  -111,   496,   895,  -111,   -12,  1132,  1132,  1132,   895,
      96,  1132,  1132,  1132,  1132,  1132,  1132,  1132,  1132,  1132,
    1132,    20,  1132,  1132,  1132,  1132,  -111,   496,  1132,   496,
    1132,    33,  1132,    46,  1132,  1132,  1175,  1132,  1335,  1335,
    1335,  1367,  1367,   139,  1295,  1215,  1255,   369,   369,  1335,
    1367,  1367,  1367,  1367,  1367,   108,   108,    91,    91,    91,
    -111,  1132,  1132,  1132,  1132,  1132,  1132,  -111,  1132,  -111,
    1132,  1132,   895,   895,    48,   137,  -111,    10,   155,  -111,
    -111,  -111,  1132,   895,  -111,    62,  -111,  -111,  -111,   116,
     120,   126,   895,   895,    67,   895,  1132,  -111,  -111,   117,
    -111,   152,    48,  1132,    89,    93,  -111,  -111,   895,  -111,
     895,  -111,    40,  -111,   895,  -111,   153,   158,  -111,   160,
    1132,  -111,  -111,    48,    61,   168,   172,  -111,    48,  -111,
    -111,    14,   173,   174,  -111,  -111,  -111
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     8,     9,    13,    10,    11,     0,     0,    14,    15,
      16,     0,     0,     0,    17,     0,    33,    34,     0,     0,
     133,     0,     0,   133,     0,     0,    31,    96,     0,    97,
      98,   100,   103,   101,   102,   104,   105,   106,   110,     0,
     108,     0,   109,     0,     0,     0,   107,   118,   111,     0,
     112,     4,     0,   114,   116,   115,   117,   127,     0,   131,
       5,    87,     0,   121,   122,   123,   119,   197,   198,   199,
     200,   201,   202,   120,   124,   125,   126,    99,     0,   128,
       0,   113,     6,    12,     0,     0,     9,   164,   162,   163,
      38,   166,   165,    27,     0,   231,     0,     0,     0,    84,
     134,    89,    90,    91,    93,    36,   134,   240,    31,    88,
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
       0,    76,     0,     0,    39,    11,     0,   237,     0,    26,
     133,    25,   133,   133,    83,     0,   167,   168,   228,     0,
     227,   171,   173,   174,   169,   170,   175,   177,   176,   182,
     180,     0,   181,   178,   179,   183,    79,   133,   185,   133,
     187,     0,   189,     0,   188,   190,   214,   215,   216,   221,
     222,   219,   220,   208,   209,   210,   211,   212,   213,   224,
     217,   218,   223,   225,   226,   203,   204,   205,   206,   207,
     132,   191,   192,   193,   194,   196,   195,    92,   172,   241,
     184,   186,   133,   133,     0,     0,   238,     0,     0,    85,
      28,    80,   229,     0,    82,     0,   130,    94,    95,   249,
     251,     0,     0,     0,     0,   133,   230,    81,   245,     0,
     247,     0,     0,   235,     0,     0,   239,    29,   133,   250,
     133,   252,     0,   232,     0,   234,     0,     0,    18,     0,
     236,   246,   248,     0,     0,     0,     0,    20,     0,    22,
      24,     0,     0,     0,    19,    21,    23
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -111,  -111,    -7,   -92,   191,  -111,  -111,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,   193,    18,   194,     5,
     195,   154,  -111,    72,  -111,  -111,  -111,  -111,  -111,  -110,
    -111,     1,    27,  -111,  -111,  -111,  -111,  -111,     0,  -111,
    -111,  -111,   -14,  -111,  -111,  -111,  -111,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,   958,
    -111,  -111,  -111,  -111,   -87,  -111,  -111,  -111,   128,   203,
    -111,  -111,  -111,  -111,  -111
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    97,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   117,    46,    47,    48,    49,    50,    98,
      51,    99,   104,    53,    54,    55,    56,    57,   106,    59,
     269,    60,    61,    62,   219,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,   121,
     122,    77,   237,   238,   344,   315,   317,    78,    79,    80,
      81,   348,   350,   339,   341
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      58,    52,   236,   245,   -86,    86,   239,    84,   240,   107,
     222,    87,    88,    89,   101,    83,    86,   108,    91,    92,
     100,   115,   249,    85,   109,   111,   103,    86,   250,    16,
      17,   152,   115,   261,   115,    82,   115,   153,   115,   102,
      16,    17,   169,   372,   373,     4,   116,   235,   333,   158,
     242,    16,    17,     4,   321,   235,    86,   116,   111,   116,
      90,   116,   156,   116,   271,    22,   110,   273,   112,   113,
      86,   223,   334,   115,   374,   224,   266,   267,    23,   141,
      16,    17,   242,   172,   232,   233,   324,    22,    23,    23,
     365,   366,  -148,  -148,   142,   242,   109,   209,   116,   327,
     358,   112,   113,    15,  -148,  -148,  -148,  -148,   242,  -148,
    -148,   139,   328,   145,   246,   150,   160,   163,  -233,   247,
     248,   367,   368,   251,   242,   252,   191,   253,   337,   254,
     241,   255,   242,   256,  -148,   257,   258,   259,   143,   111,
     142,   260,   120,   191,   205,   206,   207,   262,   148,   353,
     105,   354,   228,   355,   226,   354,   118,   325,   314,   203,
     204,   205,   206,   207,   263,   323,   264,   140,   265,   146,
     268,   151,   161,   270,   191,   332,   272,   335,   338,   274,
     342,   275,   340,   349,   276,   277,   278,   279,   280,   281,
     282,   283,   284,   285,   286,   287,   288,   289,   290,   291,
     292,   293,   294,   295,   296,   297,   298,   299,   229,   309,
     300,   101,   301,   302,   303,   304,   305,   306,   351,   361,
     308,   363,   331,   103,   362,   310,   318,   147,   369,   320,
     311,   316,   370,   375,   376,   154,   102,   155,   157,   159,
    -149,  -149,   148,   319,   234,   307,   345,   162,     0,   322,
     352,     0,  -149,  -149,  -149,  -149,     0,  -149,  -149,     0,
     359,   173,     0,     0,     0,     0,     0,     0,     0,     0,
     326,   364,     0,     0,  -155,  -155,   371,     0,     0,     0,
     114,     0,  -149,     0,     0,     0,  -155,  -155,  -155,  -155,
     120,  -155,  -155,  -135,  -135,     0,     0,     0,   329,   330,
     210,   211,     0,     0,     0,  -135,  -135,  -135,  -135,     0,
    -135,  -135,   212,   213,   214,   215,  -155,   216,   217,     0,
     174,   347,     0,   336,   120,     0,     0,   346,     0,     0,
       0,     0,   343,   343,   356,  -135,   357,   244,     0,    86,
       3,     4,   218,     5,     6,     7,     8,     9,    10,     0,
       0,    11,     0,     0,   360,    12,    13,     0,     0,    14,
       0,     0,    15,    16,    17,     0,     0,     1,     0,     2,
       3,     4,     0,     5,     6,     7,     8,     9,    10,     0,
       0,    11,     0,     0,     0,    12,    13,    18,    19,    14,
       0,     0,    15,    16,    17,    20,     0,    21,  -133,    22,
       0,    23,  -133,    96,   191,     0,     0,     0,     0,     0,
       0,   195,   196,     0,     0,     0,     0,    18,    19,     0,
     203,   204,   205,   206,   207,    20,     0,    21,  -133,    22,
       0,    23,     0,    24,    86,     3,     4,     0,     5,     6,
       7,     8,     9,    10,     0,     0,    11,   225,     0,     0,
      12,    13,     0,    93,    14,     0,     0,    15,    16,    17,
    -152,  -152,   226,     0,     0,     0,    94,     0,     0,    95,
       0,     0,  -152,  -152,  -152,  -152,     0,  -152,  -152,     0,
       0,     0,    18,    19,     0,     0,     0,     0,     0,     0,
      20,     0,    21,     0,    22,     0,    23,     0,    96,    86,
       3,     4,  -152,     5,     6,     7,     8,     9,    10,     0,
     120,    11,   176,     0,     0,    12,    13,     0,     0,    14,
       0,     0,    15,    16,    17,  -157,  -157,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -157,  -157,  -157,
    -157,     0,  -157,  -157,     0,     0,     0,    18,    19,     0,
       0,   119,     0,     0,     0,    20,     0,    21,     0,    22,
     123,    23,     0,    96,  -136,  -136,     0,  -157,     0,     0,
     125,   177,     0,  -139,  -139,   120,  -136,  -136,  -136,  -136,
       0,  -136,  -136,  -141,  -141,  -139,  -139,  -139,  -139,     0,
    -139,  -139,     0,     0,     0,  -141,  -141,  -141,  -141,     0,
    -141,  -141,     0,     0,   127,     0,  -136,     0,     0,     0,
       0,     0,     0,   129,   120,  -139,     0,  -142,  -142,     0,
       0,     0,     0,   120,     0,  -141,  -137,  -137,     0,  -142,
    -142,  -142,  -142,   120,  -142,  -142,     0,     0,  -137,  -137,
    -137,  -137,     0,  -137,  -137,     0,     0,   131,     0,     0,
       0,     0,     0,     0,     0,     0,   133,     0,     0,  -142,
    -138,  -138,     0,     0,     0,     0,   137,   120,  -137,  -143,
    -143,     0,  -138,  -138,  -138,  -138,   120,  -138,  -138,  -150,
    -150,  -143,  -143,  -143,  -143,     0,  -143,  -143,     0,     0,
       0,  -150,  -150,  -150,  -150,     0,  -150,  -150,     0,     0,
     164,     0,  -138,     0,     0,     0,     0,     0,     0,   166,
     120,  -143,     0,  -146,  -146,     0,     0,     0,     0,   120,
       0,  -150,  -147,  -147,     0,  -146,  -146,  -146,  -146,   120,
    -146,  -146,     0,     0,  -147,  -147,  -147,  -147,     0,  -147,
    -147,     0,     0,   170,     0,     0,     0,     0,     0,     0,
       0,     0,   179,     0,     0,  -146,  -153,  -153,     0,     0,
       0,     0,   181,   120,  -147,  -156,  -156,     0,  -153,  -153,
    -153,  -153,   120,  -153,  -153,  -158,  -158,  -156,  -156,  -156,
    -156,     0,  -156,  -156,     0,     0,     0,  -158,  -158,  -158,
    -158,     0,  -158,  -158,     0,     0,   220,     0,  -153,     0,
       0,     0,     0,     0,     0,   230,   120,  -156,     0,  -140,
    -140,     0,     0,     0,     0,   120,     0,  -158,  -154,  -154,
       0,  -140,  -140,  -140,  -140,   120,  -140,  -140,     0,     0,
    -154,  -154,  -154,  -154,     0,  -154,  -154,     0,     0,    -3,
     183,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -140,   184,   185,     0,   186,   187,   188,     0,   120,
    -154,     0,     0,   189,   190,     0,     0,     0,   120,     0,
       0,     0,     0,     0,   191,     0,     0,     0,   192,   193,
     194,   195,   196,     0,   197,   198,   199,   200,   201,   202,
     203,   204,   205,   206,   207,     0,     0,   208,    86,     3,
       4,  -134,     5,     6,     7,     8,     9,    10,     0,     0,
      11,   135,     0,     0,    12,    13,     0,     0,    14,     0,
       0,    15,    16,    17,  -145,  -145,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -145,  -145,  -145,  -145,
       0,  -145,  -145,     0,     0,     0,    18,    19,   136,     0,
       0,     0,     0,     0,    20,   168,    21,     0,    22,     0,
      23,  -144,  -144,     0,     0,     0,  -145,     0,  -151,  -151,
       0,     0,     0,  -144,  -144,  -144,  -144,     0,  -144,  -144,
    -151,  -151,  -151,  -151,     0,  -151,  -151,     0,   124,   126,
     128,   130,   132,   134,     0,     0,   138,     0,   144,     0,
     149,     0,     0,  -144,   165,   167,     0,     0,   171,     0,
    -151,   175,   178,   180,   182,     0,   184,   185,     0,   186,
     187,   188,     0,     0,     0,     0,     0,   189,   190,     0,
       0,     0,     0,     0,     0,   221,     0,   227,   191,   231,
       0,     0,   192,   193,   194,   195,   196,     0,   197,   198,
     199,   200,   201,   202,   203,   204,   205,   206,   207,   184,
     185,     0,   186,   187,   188,   312,     0,     0,     0,     0,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,     0,     0,     0,   192,   193,   194,   195,   196,
       0,   197,   198,   199,   200,   201,   202,   203,   204,   205,
     206,   207,   184,   185,     0,   186,   187,   188,   313,     0,
       0,   243,     0,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,     0,     0,     0,   192,   193,
     194,   195,   196,     0,   197,   198,   199,   200,   201,   202,
     203,   204,   205,   206,   207,   184,   185,     0,   186,   187,
     188,     0,     0,     0,     0,     0,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,     0,     0,
       0,   192,   193,   194,   195,   196,     0,   197,   198,   199,
     200,   201,   202,   203,   204,   205,   206,   207,   184,     0,
       0,   186,   187,   188,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,     0,     0,     0,   192,   193,   194,   195,   196,     0,
     197,   198,   199,   200,   201,   202,   203,   204,   205,   206,
     207,   186,   187,   188,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,     0,     0,     0,   192,   193,   194,   195,   196,     0,
     197,   198,   199,   200,   201,   202,   203,   204,   205,   206,
     207,   186,   187,   188,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,     0,     0,     0,   192,     0,   194,   195,   196,     0,
     197,   198,   199,   200,   201,   202,   203,   204,   205,   206,
     207,   186,   187,   188,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,     0,     0,     0,   192,     0,     0,   195,   196,     0,
     197,   198,   199,   200,   201,   202,   203,   204,   205,   206,
     207,   186,   187,   188,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,     0,     0,     0,     0,     0,     0,   195,   196,     0,
     197,   198,   199,   200,   201,   202,   203,   204,   205,   206,
     207,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,     0,     0,     0,     0,     0,     0,   195,
     196,     0,     0,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207
};

static const yytype_int16 yycheck[] =
{
       0,     0,    94,   113,     0,     3,    60,    65,    62,    23,
       1,    11,    12,    13,    21,    21,     3,    24,    18,    19,
      20,    28,    62,    65,    24,    21,    21,     3,    68,    27,
      28,     7,    39,   143,    41,     3,    43,    44,    45,    21,
      27,    28,    49,    29,    30,     5,    28,     7,    38,    44,
      62,    27,    28,     5,    66,     7,     3,    39,    21,    41,
      21,    43,    44,    45,   174,    63,     0,   177,    64,    65,
       3,    62,    62,    80,    60,    66,    64,    65,    65,     6,
      27,    28,    62,    62,    84,    85,    66,    63,    65,    65,
      29,    30,    19,    20,    21,    62,    96,    62,    80,    66,
      60,    64,    65,    26,    31,    32,    33,    34,    62,    36,
      37,    39,    66,    41,   114,    43,    44,    45,     3,   119,
     120,    60,    61,   123,    62,   125,    35,   127,    66,   129,
      60,   131,    62,   133,    61,   135,   136,   137,    65,    21,
      21,   141,    69,    35,    53,    54,    55,   147,    21,    60,
      22,    62,    80,    60,    21,    62,    28,   267,    54,    51,
      52,    53,    54,    55,   164,    69,   166,    39,   168,    41,
     170,    43,    44,   173,    35,    38,   176,    22,    62,   179,
      54,   181,    62,    66,   184,   185,   186,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,   201,   202,   203,   204,   205,   206,   207,    80,   223,
     209,   218,   212,   213,   214,   215,   216,   217,    66,    66,
     220,    61,   314,   218,    66,   225,   240,     6,    60,   243,
     230,   238,    60,    60,    60,    44,   218,    44,    44,    44,
      19,    20,    21,   242,    90,   218,   333,    44,    -1,   249,
     342,    -1,    31,    32,    33,    34,    -1,    36,    37,    -1,
     352,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     269,   363,    -1,    -1,    19,    20,   368,    -1,    -1,    -1,
       6,    -1,    61,    -1,    -1,    -1,    31,    32,    33,    34,
      69,    36,    37,    19,    20,    -1,    -1,    -1,   312,   313,
      19,    20,    -1,    -1,    -1,    31,    32,    33,    34,    -1,
      36,    37,    31,    32,    33,    34,    61,    36,    37,    -1,
      65,   335,    -1,   323,    69,    -1,    -1,   334,    -1,    -1,
      -1,    -1,   332,   333,   348,    61,   350,     1,    -1,     3,
       4,     5,    61,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    -1,    -1,   354,    19,    20,    -1,    -1,    23,
      -1,    -1,    26,    27,    28,    -1,    -1,     1,    -1,     3,
       4,     5,    -1,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    -1,    -1,    -1,    19,    20,    51,    52,    23,
      -1,    -1,    26,    27,    28,    59,    -1,    61,    62,    63,
      -1,    65,    66,    67,    35,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    43,    -1,    -1,    -1,    -1,    51,    52,    -1,
      51,    52,    53,    54,    55,    59,    -1,    61,    62,    63,
      -1,    65,    -1,    67,     3,     4,     5,    -1,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,     6,    -1,    -1,
      19,    20,    -1,    22,    23,    -1,    -1,    26,    27,    28,
      19,    20,    21,    -1,    -1,    -1,    35,    -1,    -1,    38,
      -1,    -1,    31,    32,    33,    34,    -1,    36,    37,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    -1,    -1,    -1,
      59,    -1,    61,    -1,    63,    -1,    65,    -1,    67,     3,
       4,     5,    61,     7,     8,     9,    10,    11,    12,    -1,
      69,    15,     6,    -1,    -1,    19,    20,    -1,    -1,    23,
      -1,    -1,    26,    27,    28,    19,    20,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,
      34,    -1,    36,    37,    -1,    -1,    -1,    51,    52,    -1,
      -1,     6,    -1,    -1,    -1,    59,    -1,    61,    -1,    63,
       6,    65,    -1,    67,    19,    20,    -1,    61,    -1,    -1,
       6,    65,    -1,    19,    20,    69,    31,    32,    33,    34,
      -1,    36,    37,    19,    20,    31,    32,    33,    34,    -1,
      36,    37,    -1,    -1,    -1,    31,    32,    33,    34,    -1,
      36,    37,    -1,    -1,     6,    -1,    61,    -1,    -1,    -1,
      -1,    -1,    -1,     6,    69,    61,    -1,    19,    20,    -1,
      -1,    -1,    -1,    69,    -1,    61,    19,    20,    -1,    31,
      32,    33,    34,    69,    36,    37,    -1,    -1,    31,    32,
      33,    34,    -1,    36,    37,    -1,    -1,     6,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     6,    -1,    -1,    61,
      19,    20,    -1,    -1,    -1,    -1,     6,    69,    61,    19,
      20,    -1,    31,    32,    33,    34,    69,    36,    37,    19,
      20,    31,    32,    33,    34,    -1,    36,    37,    -1,    -1,
      -1,    31,    32,    33,    34,    -1,    36,    37,    -1,    -1,
       6,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,     6,
      69,    61,    -1,    19,    20,    -1,    -1,    -1,    -1,    69,
      -1,    61,    19,    20,    -1,    31,    32,    33,    34,    69,
      36,    37,    -1,    -1,    31,    32,    33,    34,    -1,    36,
      37,    -1,    -1,     6,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     6,    -1,    -1,    61,    19,    20,    -1,    -1,
      -1,    -1,     6,    69,    61,    19,    20,    -1,    31,    32,
      33,    34,    69,    36,    37,    19,    20,    31,    32,    33,
      34,    -1,    36,    37,    -1,    -1,    -1,    31,    32,    33,
      34,    -1,    36,    37,    -1,    -1,     6,    -1,    61,    -1,
      -1,    -1,    -1,    -1,    -1,     6,    69,    61,    -1,    19,
      20,    -1,    -1,    -1,    -1,    69,    -1,    61,    19,    20,
      -1,    31,    32,    33,    34,    69,    36,    37,    -1,    -1,
      31,    32,    33,    34,    -1,    36,    37,    -1,    -1,     0,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    61,    13,    14,    -1,    16,    17,    18,    -1,    69,
      61,    -1,    -1,    24,    25,    -1,    -1,    -1,    69,    -1,
      -1,    -1,    -1,    -1,    35,    -1,    -1,    -1,    39,    40,
      41,    42,    43,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    -1,    -1,    58,     3,     4,
       5,    62,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,     6,    -1,    -1,    19,    20,    -1,    -1,    23,    -1,
      -1,    26,    27,    28,    19,    20,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,
      -1,    36,    37,    -1,    -1,    -1,    51,    52,     6,    -1,
      -1,    -1,    -1,    -1,    59,     6,    61,    -1,    63,    -1,
      65,    19,    20,    -1,    -1,    -1,    61,    -1,    19,    20,
      -1,    -1,    -1,    31,    32,    33,    34,    -1,    36,    37,
      31,    32,    33,    34,    -1,    36,    37,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    38,    -1,    40,    -1,
      42,    -1,    -1,    61,    46,    47,    -1,    -1,    50,    -1,
      61,    53,    54,    55,    56,    -1,    13,    14,    -1,    16,
      17,    18,    -1,    -1,    -1,    -1,    -1,    24,    25,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    -1,    79,    35,    81,
      -1,    -1,    39,    40,    41,    42,    43,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    13,
      14,    -1,    16,    17,    18,    62,    -1,    -1,    -1,    -1,
      24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    35,    -1,    -1,    -1,    39,    40,    41,    42,    43,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    13,    14,    -1,    16,    17,    18,    62,    -1,
      -1,    22,    -1,    24,    25,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    35,    -1,    -1,    -1,    39,    40,
      41,    42,    43,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    13,    14,    -1,    16,    17,
      18,    -1,    -1,    -1,    -1,    -1,    24,    25,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    35,    -1,    -1,
      -1,    39,    40,    41,    42,    43,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    13,    -1,
      -1,    16,    17,    18,    -1,    -1,    -1,    -1,    -1,    24,
      25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    16,    17,    18,    -1,    -1,    -1,    -1,    -1,    24,
      25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    16,    17,    18,    -1,    -1,    -1,    -1,    -1,    24,
      25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      35,    -1,    -1,    -1,    39,    -1,    41,    42,    43,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    16,    17,    18,    -1,    -1,    -1,    -1,    -1,    24,
      25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      35,    -1,    -1,    -1,    39,    -1,    -1,    42,    43,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    16,    17,    18,    -1,    -1,    -1,    -1,    -1,    24,
      25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      35,    -1,    -1,    -1,    -1,    -1,    -1,    42,    43,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      43,    -1,    -1,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,     5,     7,     8,     9,    10,    11,
      12,    15,    19,    20,    23,    26,    27,    28,    51,    52,
      59,    61,    63,    65,    67,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    94,    95,    96,    97,
      98,   100,   101,   103,   104,   105,   106,   107,   108,   109,
     111,   112,   113,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   131,   137,   138,
     139,   140,     3,    21,    65,    65,     3,   108,   108,   108,
      21,   108,   108,    22,    35,    38,    67,    83,    99,   101,
     108,    72,    87,    89,   102,   138,   108,   112,    72,   108,
       0,    21,    64,    65,     6,    72,    87,    93,   138,     6,
      69,   129,   130,     6,   129,     6,   129,     6,   129,     6,
     129,     6,   129,     6,   129,     6,     6,     6,   129,    93,
     138,     6,    21,    65,   129,    93,   138,     6,    21,   129,
      93,   138,     7,    72,    74,    86,    87,    88,    89,    90,
      93,   138,   139,    93,     6,   129,     6,   129,     6,    72,
       6,   129,    62,     6,    65,   129,     6,    65,   129,     6,
     129,     6,   129,     1,    13,    14,    16,    17,    18,    24,
      25,    35,    39,    40,    41,    42,    43,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    58,    62,
      19,    20,    31,    32,    33,    34,    36,    37,    61,   114,
       6,   129,     1,    62,    66,     6,    21,   129,    93,   138,
       6,   129,   108,   108,    91,     7,    73,   132,   133,    60,
      62,    60,    62,    22,     1,    99,   108,   108,   108,    62,
      68,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,    99,   108,   108,   108,   108,    64,    65,   108,   110,
     108,    99,   108,    99,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     101,   108,   108,   108,   108,   108,   108,   102,   108,   112,
     108,   108,    62,    62,    54,   135,    72,   136,   112,   101,
     112,    66,   108,    69,    66,    99,   101,    66,    66,   112,
     112,    73,    38,    38,    62,    22,   108,    66,    62,   143,
      62,   144,    54,   108,   134,   134,    72,   112,   141,    66,
     142,    66,    73,    60,    62,    60,   112,   112,    60,    73,
     108,    66,    66,    61,    73,    29,    30,    60,    61,    60,
      60,    73,    29,    30,    60,    60,    60
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
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

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
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


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = HB_MACRO_YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == HB_MACRO_YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (pMacro, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use HB_MACRO_YYerror or HB_MACRO_YYUNDEF. */
#define YYERRCODE HB_MACRO_YYUNDEF


/* Enable debugging if requested.  */
#if HB_MACRO_YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, pMacro); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, PHB_MACRO pMacro)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (pMacro);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, PHB_MACRO pMacro)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep, pMacro);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule, PHB_MACRO pMacro)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)], pMacro);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, pMacro); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !HB_MACRO_YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !HB_MACRO_YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
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






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, PHB_MACRO pMacro)
{
  YY_USE (yyvaluep);
  YY_USE (pMacro);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (PHB_MACRO pMacro)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = HB_MACRO_YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == HB_MACRO_YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, pMacro);
    }

  if (yychar <= HB_MACRO_YYEOF)
    {
      yychar = HB_MACRO_YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == HB_MACRO_YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = HB_MACRO_YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = HB_MACRO_YYEMPTY;
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
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* Main: Expression '\n'  */
#line 274 "../../macro.y"
                        {
                           HB_MACRO_DATA->exprType = hb_compExprType( (yyvsp[-1].asExpr) );
                           if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_PUSH )
                              hb_compExprDelete( hb_compExprGenPush( (yyvsp[-1].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           else if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_STATEMENT )
                              hb_compExprDelete( hb_compExprGenStatement( (yyvsp[-1].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           else
                              hb_compExprDelete( hb_compExprGenPop( (yyvsp[-1].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           hb_compGenPCode1( HB_P_ENDPROC, HB_MACRO_PARAM );
                        }
#line 1873 "macroy.c"
    break;

  case 3: /* Main: Expression  */
#line 284 "../../macro.y"
                        {
                           HB_MACRO_DATA->exprType = hb_compExprType( (yyvsp[0].asExpr) );
                           if( HB_MACRO_DATA->Flags &  HB_MACRO_GEN_PUSH )
                              hb_compExprDelete( hb_compExprGenPush( (yyvsp[0].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           else if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_STATEMENT )
                              hb_compExprDelete( hb_compExprGenStatement( (yyvsp[0].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           else
                              hb_compExprDelete( hb_compExprGenPop( (yyvsp[0].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );
                           hb_compGenPCode1( HB_P_ENDPROC, HB_MACRO_PARAM );
                        }
#line 1888 "macroy.c"
    break;

  case 4: /* Main: ByRefArg  */
#line 294 "../../macro.y"
                        {
                           if( ! ( HB_MACRO_DATA->Flags & HB_MACRO_GEN_LIST ) )
                           {
                              HB_TRACE(HB_TR_DEBUG, ("macro -> invalid expression: %s", HB_MACRO_DATA->string));
                              hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );
                              hb_compExprDelete( (yyvsp[0].asExpr), HB_MACRO_PARAM );
                              YYABORT;
                           }

                           (yyvsp[0].asExpr) = ( HB_MACRO_DATA->Flags & HB_MACRO_GEN_PARE ) ? hb_compExprNewList( (yyvsp[0].asExpr) ) : hb_compExprNewArgList( (yyvsp[0].asExpr) );

                           HB_MACRO_DATA->exprType = hb_compExprType( (yyvsp[0].asExpr) );

                           hb_compExprDelete( hb_compExprGenPush( (yyvsp[0].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );

                           hb_compGenPCode1( HB_P_ENDPROC, HB_MACRO_PARAM );
                        }
#line 1910 "macroy.c"
    break;

  case 5: /* Main: AsParamList  */
#line 311 "../../macro.y"
                        {
                           HB_MACRO_DATA->exprType = hb_compExprType( (yyvsp[0].asExpr) );

                           hb_compExprDelete( hb_compExprGenPush( (yyvsp[0].asExpr), HB_MACRO_PARAM ), HB_MACRO_PARAM );

                           hb_compGenPCode1( HB_P_ENDPROC, HB_MACRO_PARAM );
                        }
#line 1922 "macroy.c"
    break;

  case 6: /* Main: IDENTIFIER IDENTIFIER  */
#line 318 "../../macro.y"
                             {
                           HB_TRACE(HB_TR_DEBUG, ("macro -> invalid expression: %s", HB_MACRO_DATA->string));

                           //printf( "Macro: %s\n", HB_MACRO_DATA->string );

                           hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );

                           while( s_iPending )
                              hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );

                           if( (yyvsp[-1].string) == (yyvsp[0].string) )
                           {
                              hb_xfree( (yyvsp[-1].string) );
                           }
                           else
                           {
                              hb_xfree( (yyvsp[-1].string) );
                              hb_xfree( (yyvsp[0].string) );
                           }
                           yylval.string = NULL;

                           HB_MACRO_ABORT;
                        }
#line 1950 "macroy.c"
    break;

  case 7: /* Main: Expression error  */
#line 341 "../../macro.y"
                        {
                           HB_TRACE(HB_TR_DEBUG, ("macro -> invalid expression: %s", HB_MACRO_DATA->string));

                           //printf( "Macro: %s\n", HB_MACRO_DATA->string );

                           hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );
                           hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM );

                           while( s_iPending )
                              hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );

                           if( yychar == IDENTIFIER && yylval.string )
                           {
                              hb_xfree( yylval.string );
                              yylval.string = NULL;
                           }
                           HB_MACRO_ABORT;
                        }
#line 1973 "macroy.c"
    break;

  case 8: /* Main: error  */
#line 359 "../../macro.y"
                        {
                           // This case is when error maybe nested in say a CodeBlock.
                           HB_TRACE(HB_TR_DEBUG, ("macro -> invalid syntax: %s", HB_MACRO_DATA->string));

                           //printf( "2-Macro: %s\n", HB_MACRO_DATA->string );

                           hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );

                           while( s_iPending )
                              hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );

                           if( yychar == IDENTIFIER && yylval.string )
                           {
                              hb_xfree( yylval.string );
                              yylval.string = NULL;
                           }
                           HB_MACRO_ABORT;
                        }
#line 1996 "macroy.c"
    break;

  case 9: /* IdentName: IDENTIFIER  */
#line 379 "../../macro.y"
                              { (yyval.string) = (yyvsp[0].string); (yyvsp[0].string) = NULL; }
#line 2002 "macroy.c"
    break;

  case 10: /* NumValue: NUM_DOUBLE  */
#line 384 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewDouble( (yyvsp[0].valDouble).dNumber, (yyvsp[0].valDouble).bWidth, (yyvsp[0].valDouble).bDec ); }
#line 2008 "macroy.c"
    break;

  case 11: /* NumValue: NUM_LONG  */
#line 385 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewLong( (yyvsp[0].valLong).lNumber ); }
#line 2014 "macroy.c"
    break;

  case 12: /* NumAlias: NUM_LONG ALIASOP  */
#line 388 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewLong( (yyvsp[-1].valLong).lNumber ); }
#line 2020 "macroy.c"
    break;

  case 13: /* NilValue: NIL  */
#line 393 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewNil(); }
#line 2026 "macroy.c"
    break;

  case 14: /* LiteralValue: LITERAL  */
#line 398 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewString( (yyvsp[0].valChar).string, (yyvsp[0].valChar).length, TRUE ); }
#line 2032 "macroy.c"
    break;

  case 15: /* Logical: TRUEVALUE  */
#line 403 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewLogical( TRUE ); }
#line 2038 "macroy.c"
    break;

  case 16: /* Logical: FALSEVALUE  */
#line 404 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewLogical( FALSE ); }
#line 2044 "macroy.c"
    break;

  case 17: /* SelfValue: SELF  */
#line 409 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewSelf(); }
#line 2050 "macroy.c"
    break;

  case 18: /* Date: '{' POWER NumValue '/' NumValue '/' NumValue '}'  */
#line 415 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewDate( (yyvsp[-5].asExpr), (yyvsp[-3].asExpr), (yyvsp[-1].asExpr) );
                                     hb_compExprDelete( (yyvsp[-5].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-3].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM );
                              }
#line 2060 "macroy.c"
    break;

  case 19: /* DateTime: '{' POWER NumValue '/' NumValue '/' NumValue NumValue ':' NumValue ':' NumValue '}'  */
#line 424 "../../macro.y"
                              { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[-10].asExpr), (yyvsp[-8].asExpr), (yyvsp[-6].asExpr), (yyvsp[-5].asExpr), (yyvsp[-3].asExpr), (yyvsp[-1].asExpr), 0, &iOk );
                                     hb_compExprDelete( (yyvsp[-10].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-8].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-6].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-5].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-3].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              }
#line 2078 "macroy.c"
    break;

  case 20: /* DateTime: '{' POWER NumValue '/' NumValue '/' NumValue NumValue ':' NumValue '}'  */
#line 438 "../../macro.y"
                              { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[-8].asExpr), (yyvsp[-6].asExpr), (yyvsp[-4].asExpr), (yyvsp[-3].asExpr), (yyvsp[-1].asExpr), NULL, 0, &iOk );
                                     hb_compExprDelete( (yyvsp[-8].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-6].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-4].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-3].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              }
#line 2095 "macroy.c"
    break;

  case 21: /* DateTime: '{' POWER NumValue '/' NumValue '/' NumValue NumValue ':' NumValue ':' NumValue H12AM '}'  */
#line 451 "../../macro.y"
                              { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[-11].asExpr), (yyvsp[-9].asExpr), (yyvsp[-7].asExpr), (yyvsp[-6].asExpr), (yyvsp[-4].asExpr), (yyvsp[-2].asExpr), 1, &iOk );
                                     hb_compExprDelete( (yyvsp[-11].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-9].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-7].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-6].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-4].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-2].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              }
#line 2113 "macroy.c"
    break;

  case 22: /* DateTime: '{' POWER NumValue '/' NumValue '/' NumValue NumValue ':' NumValue H12AM '}'  */
#line 465 "../../macro.y"
                              { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[-9].asExpr), (yyvsp[-7].asExpr), (yyvsp[-5].asExpr), (yyvsp[-4].asExpr), (yyvsp[-2].asExpr), NULL, 1, &iOk );
                                     hb_compExprDelete( (yyvsp[-9].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-7].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-5].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-4].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-2].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              }
#line 2130 "macroy.c"
    break;

  case 23: /* DateTime: '{' POWER NumValue '/' NumValue '/' NumValue NumValue ':' NumValue ':' NumValue H12PM '}'  */
#line 478 "../../macro.y"
                              { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[-11].asExpr), (yyvsp[-9].asExpr), (yyvsp[-7].asExpr), (yyvsp[-6].asExpr), (yyvsp[-4].asExpr), (yyvsp[-2].asExpr), 2, &iOk );
                                     hb_compExprDelete( (yyvsp[-11].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-9].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-7].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-6].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-4].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-2].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              }
#line 2148 "macroy.c"
    break;

  case 24: /* DateTime: '{' POWER NumValue '/' NumValue '/' NumValue NumValue ':' NumValue H12PM '}'  */
#line 492 "../../macro.y"
                              { int iOk = 0;
                                (yyval.asExpr) = hb_compExprNewDateTime( (yyvsp[-9].asExpr), (yyvsp[-7].asExpr), (yyvsp[-5].asExpr), (yyvsp[-4].asExpr), (yyvsp[-2].asExpr), NULL, 2, &iOk );
                                     hb_compExprDelete( (yyvsp[-9].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-7].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-5].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-4].asExpr), HB_MACRO_PARAM );
                                     hb_compExprDelete( (yyvsp[-2].asExpr), HB_MACRO_PARAM );
                                if( !iOk )
                                {
                                   (yyval.asExpr) = NULL;
                                }
                              }
#line 2165 "macroy.c"
    break;

  case 25: /* Array: '{' ArgList '}'  */
#line 510 "../../macro.y"
                                 {
                                   (yyval.asExpr) = hb_compExprNewArray( (yyvsp[-1].asExpr) );

                                   if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[-1].asExpr) )
                                      s_iPending--;
                                 }
#line 2176 "macroy.c"
    break;

  case 26: /* Hash: '{' HashList '}'  */
#line 518 "../../macro.y"
                                 { (yyval.asExpr) = hb_compExprNewFunCall( hb_compExprNewFunName( hb_strdup( "HASH" ) ), (yyvsp[-1].asExpr), HB_MACRO_PARAM ); }
#line 2182 "macroy.c"
    break;

  case 27: /* HashList: HASHOP  */
#line 521 "../../macro.y"
                                                                 { (yyval.asExpr) = NULL; }
#line 2188 "macroy.c"
    break;

  case 28: /* HashList: Expression HASHOP EmptyExpression  */
#line 522 "../../macro.y"
                                                                 { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewArgList( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr) ); }
#line 2194 "macroy.c"
    break;

  case 29: /* HashList: HashList ',' EmptyExpression HASHOP EmptyExpression  */
#line 523 "../../macro.y"
                                                                 { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprAddListExpr( (yyvsp[-4].asExpr), (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr) ); }
#line 2200 "macroy.c"
    break;

  case 30: /* ArrayAt: Array ArrayIndex  */
#line 528 "../../macro.y"
                                 { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2206 "macroy.c"
    break;

  case 31: /* Variable: IdentName  */
#line 533 "../../macro.y"
                                 { (yyval.asExpr) = hb_compExprNewVar( (yyvsp[0].string) ); }
#line 2212 "macroy.c"
    break;

  case 32: /* VarAlias: IdentName ALIASOP  */
#line 536 "../../macro.y"
                                 { (yyval.asExpr) = hb_compExprNewAlias( (yyvsp[-1].string) ); }
#line 2218 "macroy.c"
    break;

  case 33: /* MacroVar: MACROVAR  */
#line 541 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewMacro( NULL, '&', (yyvsp[0].string) );
                                HB_MACRO_CHECK( (yyval.asExpr) );
                              }
#line 2226 "macroy.c"
    break;

  case 34: /* MacroVar: MACROTEXT  */
#line 544 "../../macro.y"
                              {  HB_SIZE ulLen = strlen( (yyvsp[0].string) );
                                 char * szVarName = hb_macroTextSubst( (yyvsp[0].string), &ulLen );
                                 if( hb_macroIsIdent( szVarName ) )
                                 {
                                    (yyval.asExpr) = hb_compExprNewVar( szVarName );
                                    // Should always be true since hb_compExprNewVar() returned TRUE.
                                    if( (yyvsp[0].string) != szVarName )
                                    {
                                       hb_xfree( (yyvsp[0].string) );
                                       (yyvsp[0].string) = NULL;
                                    }
                                    HB_MACRO_CHECK( (yyval.asExpr) );
                                 }
                                 else
                                 {
                                    /* invalid variable name
                                     */
                                    HB_TRACE(HB_TR_DEBUG, ("macro -> invalid variable name: %s", (yyvsp[0].string)));
                                    hb_xfree( (yyvsp[0].string) );
                                    (yyvsp[0].string) = NULL;
                                    YYABORT;
                                 }
                              }
#line 2254 "macroy.c"
    break;

  case 35: /* MacroVarAlias: MacroVar ALIASOP  */
#line 569 "../../macro.y"
                                    {
                                      if( (yyvsp[-1].asExpr)->ExprType == HB_ET_VARIABLE )
                                      {
                                         (yyvsp[-1].asExpr)->ExprType = HB_ET_ALIAS;
                                      }
                                      (yyval.asExpr) = (yyvsp[-1].asExpr);
                                    }
#line 2266 "macroy.c"
    break;

  case 36: /* MacroExpr: '&' PareExpList  */
#line 580 "../../macro.y"
                                    { (yyval.asExpr) = hb_compExprNewMacro( (yyvsp[0].asExpr), 0, NULL ); }
#line 2272 "macroy.c"
    break;

  case 37: /* MacroExprAlias: MacroExpr ALIASOP  */
#line 583 "../../macro.y"
                                       { (yyval.asExpr) = (yyvsp[-1].asExpr); }
#line 2278 "macroy.c"
    break;

  case 38: /* FieldAlias: FIELD ALIASOP  */
#line 590 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewAlias( hb_strdup( "FIELD") ); }
#line 2284 "macroy.c"
    break;

  case 39: /* FieldAlias: FIELD ALIASOP FieldAlias  */
#line 591 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2290 "macroy.c"
    break;

  case 40: /* FieldVarAlias: FieldAlias VarAlias  */
#line 596 "../../macro.y"
                                                { hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2296 "macroy.c"
    break;

  case 41: /* FieldVarAlias: FieldAlias NumAlias  */
#line 597 "../../macro.y"
                                                { hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2302 "macroy.c"
    break;

  case 42: /* FieldVarAlias: FieldAlias PareExpListAlias  */
#line 598 "../../macro.y"
                                                { hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2308 "macroy.c"
    break;

  case 43: /* FieldVarAlias: FieldAlias MacroVarAlias  */
#line 599 "../../macro.y"
                                                { hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2314 "macroy.c"
    break;

  case 44: /* FieldVarAlias: FieldAlias MacroExprAlias  */
#line 600 "../../macro.y"
                                                { hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM ); (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2320 "macroy.c"
    break;

  case 45: /* AliasId: IdentName  */
#line 603 "../../macro.y"
                              { (yyval.asExpr) = hb_compExprNewVar( (yyvsp[0].string) ); }
#line 2326 "macroy.c"
    break;

  case 46: /* AliasId: MacroVar  */
#line 604 "../../macro.y"
                              { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2332 "macroy.c"
    break;

  case 47: /* AliasVar: NumAlias AliasId  */
#line 607 "../../macro.y"
                                       { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2338 "macroy.c"
    break;

  case 48: /* AliasVar: MacroVarAlias AliasId  */
#line 608 "../../macro.y"
                                       { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2344 "macroy.c"
    break;

  case 49: /* AliasVar: MacroExprAlias AliasId  */
#line 609 "../../macro.y"
                                       { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2350 "macroy.c"
    break;

  case 50: /* AliasVar: PareExpListAlias AliasId  */
#line 610 "../../macro.y"
                                       { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2356 "macroy.c"
    break;

  case 51: /* AliasVar: VarAlias AliasId  */
#line 611 "../../macro.y"
                                       { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2362 "macroy.c"
    break;

  case 52: /* AliasVar: FieldAlias AliasId  */
#line 612 "../../macro.y"
                                       { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2368 "macroy.c"
    break;

  case 53: /* AliasVar: FieldVarAlias AliasId  */
#line 613 "../../macro.y"
                                       { (yyval.asExpr) = hb_compExprNewAliasVar( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2374 "macroy.c"
    break;

  case 54: /* AliasExpr: NumAlias PareExpList  */
#line 622 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2380 "macroy.c"
    break;

  case 55: /* AliasExpr: VarAlias PareExpList  */
#line 623 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2386 "macroy.c"
    break;

  case 56: /* AliasExpr: MacroVarAlias PareExpList  */
#line 624 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2392 "macroy.c"
    break;

  case 57: /* AliasExpr: MacroExprAlias PareExpList  */
#line 625 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2398 "macroy.c"
    break;

  case 58: /* AliasExpr: PareExpListAlias PareExpList  */
#line 626 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewAliasExpr( (yyvsp[-1].asExpr), (yyvsp[0].asExpr) ); }
#line 2404 "macroy.c"
    break;

  case 59: /* VariableAt: NilValue ArrayIndex  */
#line 631 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2410 "macroy.c"
    break;

  case 60: /* VariableAt: LiteralValue ArrayIndex  */
#line 632 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2416 "macroy.c"
    break;

  case 61: /* VariableAt: CodeBlock ArrayIndex  */
#line 633 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2422 "macroy.c"
    break;

  case 62: /* VariableAt: Logical ArrayIndex  */
#line 634 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2428 "macroy.c"
    break;

  case 63: /* VariableAt: Date ArrayIndex  */
#line 635 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2434 "macroy.c"
    break;

  case 64: /* VariableAt: DateTime ArrayIndex  */
#line 636 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2440 "macroy.c"
    break;

  case 65: /* VariableAt: SelfValue ArrayIndex  */
#line 637 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2446 "macroy.c"
    break;

  case 66: /* VariableAt: Variable ArrayIndex  */
#line 638 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2452 "macroy.c"
    break;

  case 67: /* VariableAt: AliasVar ArrayIndex  */
#line 639 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2458 "macroy.c"
    break;

  case 68: /* VariableAt: AliasExpr ArrayIndex  */
#line 640 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2464 "macroy.c"
    break;

  case 69: /* VariableAt: MacroVar ArrayIndex  */
#line 641 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2470 "macroy.c"
    break;

  case 70: /* VariableAt: MacroExpr ArrayIndex  */
#line 642 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2476 "macroy.c"
    break;

  case 71: /* VariableAt: ObjectData ArrayIndex  */
#line 643 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2482 "macroy.c"
    break;

  case 72: /* VariableAt: ObjectMethod ArrayIndex  */
#line 644 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2488 "macroy.c"
    break;

  case 73: /* VariableAt: WithData ArrayIndex  */
#line 645 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2494 "macroy.c"
    break;

  case 74: /* VariableAt: WithMethod ArrayIndex  */
#line 646 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2500 "macroy.c"
    break;

  case 75: /* VariableAt: FunCall ArrayIndex  */
#line 647 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2506 "macroy.c"
    break;

  case 76: /* VariableAt: IfInline ArrayIndex  */
#line 648 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2512 "macroy.c"
    break;

  case 77: /* VariableAt: PareExpList ArrayIndex  */
#line 649 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2518 "macroy.c"
    break;

  case 78: /* NamespacePath: IdentName '.'  */
#line 652 "../../macro.y"
                                        { (yyval.string) = (yyvsp[-1].string); }
#line 2524 "macroy.c"
    break;

  case 79: /* NamespacePath: NamespacePath IdentName '.'  */
#line 653 "../../macro.y"
                                            {
                                               (yyval.string) = hb_xstrcpy( NULL, (yyvsp[-2].string), ".", (yyvsp[-1].string), NULL );
                                               hb_xfree( (yyvsp[-2].string) );
                                               (yyvsp[-2].string) = NULL;
                                             }
#line 2534 "macroy.c"
    break;

  case 80: /* FunCall: IdentName '(' ArgList ')'  */
#line 661 "../../macro.y"
                                          {
                                            (yyval.asExpr) = hb_compExprNewFunCall( hb_compExprNewFunName( (yyvsp[-3].string) ), (yyvsp[-1].asExpr), HB_MACRO_PARAM );
                                            HB_MACRO_CHECK( (yyval.asExpr) );

                                            if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[-1].asExpr) )
                                               s_iPending--;
                                          }
#line 2546 "macroy.c"
    break;

  case 81: /* FunCall: NamespacePath IdentName '(' ArgList ')'  */
#line 668 "../../macro.y"
                                                      {
                                                        (yyval.asExpr) = hb_compExprNewFunCall( hb_compExprNewNamespaceFunName( (yyvsp[-4].string), (yyvsp[-3].string) ), (yyvsp[-1].asExpr), HB_MACRO_PARAM );
                                                        HB_MACRO_CHECK( (yyval.asExpr) );

                                                        if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[-1].asExpr) )
                                                           s_iPending--;
                                                      }
#line 2558 "macroy.c"
    break;

  case 82: /* FunCall: MacroVar '(' ArgList ')'  */
#line 675 "../../macro.y"
                                          {
                                            (yyval.asExpr) = hb_compExprNewFunCall( (yyvsp[-3].asExpr), (yyvsp[-1].asExpr), HB_MACRO_PARAM );
                                            HB_MACRO_CHECK( (yyval.asExpr) );

                                            if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[-1].asExpr) )
                                               s_iPending--;
                                          }
#line 2570 "macroy.c"
    break;

  case 83: /* FunCall: IdentName '(' error  */
#line 682 "../../macro.y"
                                         {
                                            hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );

                                            if( yychar == IDENTIFIER && yylval.string )
                                            {
                                               hb_xfree( yylval.string );
                                               yylval.string = NULL;
                                            }

                                            while( s_iPending )
                                               hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );

                                            YYABORT;
                                          }
#line 2589 "macroy.c"
    break;

  case 84: /* ArgList: Argument  */
#line 698 "../../macro.y"
                                          {
                                            (yyval.asExpr) = hb_compExprNewArgList( (yyvsp[0].asExpr) );

                                            if( s_iPending <= HB_MAX_PENDING_MACRO_EXP )
                                               s_Pending[ s_iPending++ ] = (yyval.asExpr);
                                          }
#line 2600 "macroy.c"
    break;

  case 85: /* ArgList: ArgList ',' Argument  */
#line 704 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 2606 "macroy.c"
    break;

  case 86: /* ByRefArg: '@' IdentName  */
#line 707 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewVarRef( (yyvsp[0].string) ); }
#line 2612 "macroy.c"
    break;

  case 87: /* Argument: EmptyExpression  */
#line 710 "../../macro.y"
                                       { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2618 "macroy.c"
    break;

  case 88: /* Argument: '@' Expression  */
#line 711 "../../macro.y"
                                       {
                                          switch( (yyvsp[0].asExpr)->ExprType )
                                          {
                                             case HB_ET_VARIABLE:
                                               (yyval.asExpr) = (yyvsp[0].asExpr);
                                               (yyval.asExpr)->ExprType = HB_ET_VARREF;
                                               (yyval.asExpr)->ValType  = HB_EV_VARREF;
                                               break;

                                             case HB_ET_ALIASVAR:
                                             {
                                               char *szAlias = (yyvsp[0].asExpr)->value.asAlias.pAlias->value.asSymbol.szName;

                                               if( strcmp( szAlias, "M" ) == 0 || strncmp( szAlias, "MEMVAR", 4 > strlen( szAlias ) ? 4 : strlen( szAlias ) ) == 0 )
                                               {
                                                  (yyval.asExpr) = (yyvsp[0].asExpr)->value.asAlias.pVar;

                                                  (yyvsp[0].asExpr)->value.asAlias.pVar = NULL;
                                                  hb_compExprDelete( (yyvsp[0].asExpr), HB_MACRO_PARAM  );

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
                                                (yyval.asExpr) = (yyvsp[0].asExpr)->value.asFunCall.pFunName;

                                                (yyvsp[0].asExpr)->value.asFunCall.pFunName = NULL;
                                                hb_compExprDelete( (yyvsp[0].asExpr), HB_MACRO_PARAM  );

                                                (yyval.asExpr)->ExprType = HB_ET_FUNREF;
                                                (yyval.asExpr)->ValType  = HB_EV_FUNREF;
                                                break;

                                             case HB_ET_SEND:
                                               (yyval.asExpr) = (yyvsp[0].asExpr);
                                               (yyval.asExpr)->value.asMessage.bByRef = TRUE;
                                               break;

                                             case HB_ET_MACRO:
                                               (yyval.asExpr) = (yyvsp[0].asExpr);
                                               (yyval.asExpr)->value.asMacro.SubType = HB_ET_MACRO_VAR_REF;
                                               break;

                                             case HB_ET_ARRAYAT:
                                               (yyval.asExpr) = (yyvsp[0].asExpr);
                                               (yyval.asExpr)->value.asList.bByRef = TRUE;
                                               break;

                                             default:
                                               hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );
                                               (yyval.asExpr) = (yyvsp[0].asExpr);
                                          }
                                       }
#line 2686 "macroy.c"
    break;

  case 89: /* SendId: IdentName  */
#line 778 "../../macro.y"
                             { (yyval.asExpr) = hb_compExprNewFunName( (yyvsp[0].string) ); }
#line 2692 "macroy.c"
    break;

  case 90: /* SendId: MacroVar  */
#line 779 "../../macro.y"
                             { (yyval.asExpr) = (yyvsp[0].asExpr); (yyvsp[0].asExpr)->value.asMacro.SubType = HB_ET_MACRO_SYMBOL; }
#line 2698 "macroy.c"
    break;

  case 91: /* SendId: MacroExpr  */
#line 780 "../../macro.y"
                             { (yyval.asExpr) = (yyvsp[0].asExpr); (yyvsp[0].asExpr)->value.asMacro.SubType = HB_ET_MACRO_SYMBOL; }
#line 2704 "macroy.c"
    break;

  case 92: /* ObjectData: LeftExpression ':' SendId  */
#line 783 "../../macro.y"
                                               { (yyval.asExpr) = hb_compExprNewSendExp( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 2710 "macroy.c"
    break;

  case 93: /* WithData: ':' SendId  */
#line 786 "../../macro.y"
                                         {
                                            (yyval.asExpr) = hb_compExprNewWithSendExp( (yyvsp[0].asExpr) );
                                         }
#line 2718 "macroy.c"
    break;

  case 94: /* ObjectMethod: ObjectData '(' ArgList ')'  */
#line 793 "../../macro.y"
                                             {
                                               (yyval.asExpr) = hb_compExprNewMethodCall( (yyvsp[-3].asExpr), (yyvsp[-1].asExpr) );

                                               if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyvsp[-1].asExpr) )
                                                  s_iPending--;
                                             }
#line 2729 "macroy.c"
    break;

  case 95: /* WithMethod: WithData '(' ArgList ')'  */
#line 801 "../../macro.y"
                                         {
                                            (yyval.asExpr) = hb_compExprNewWithMethodCall( (yyvsp[-3].asExpr), (yyvsp[-1].asExpr) );
                                         }
#line 2737 "macroy.c"
    break;

  case 97: /* SimpleExpression: NilValue  */
#line 807 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2743 "macroy.c"
    break;

  case 98: /* SimpleExpression: LiteralValue  */
#line 808 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2749 "macroy.c"
    break;

  case 99: /* SimpleExpression: CodeBlock  */
#line 809 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2755 "macroy.c"
    break;

  case 100: /* SimpleExpression: Logical  */
#line 810 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2761 "macroy.c"
    break;

  case 101: /* SimpleExpression: Date  */
#line 811 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2767 "macroy.c"
    break;

  case 102: /* SimpleExpression: DateTime  */
#line 812 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2773 "macroy.c"
    break;

  case 103: /* SimpleExpression: SelfValue  */
#line 813 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2779 "macroy.c"
    break;

  case 104: /* SimpleExpression: Array  */
#line 814 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2785 "macroy.c"
    break;

  case 105: /* SimpleExpression: Hash  */
#line 815 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2791 "macroy.c"
    break;

  case 106: /* SimpleExpression: ArrayAt  */
#line 816 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2797 "macroy.c"
    break;

  case 107: /* SimpleExpression: AliasVar  */
#line 817 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2803 "macroy.c"
    break;

  case 108: /* SimpleExpression: MacroVar  */
#line 818 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2809 "macroy.c"
    break;

  case 109: /* SimpleExpression: MacroExpr  */
#line 819 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2815 "macroy.c"
    break;

  case 110: /* SimpleExpression: Variable  */
#line 820 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2821 "macroy.c"
    break;

  case 111: /* SimpleExpression: VariableAt  */
#line 821 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2827 "macroy.c"
    break;

  case 112: /* SimpleExpression: FunCall  */
#line 822 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2833 "macroy.c"
    break;

  case 113: /* SimpleExpression: IfInline  */
#line 823 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2839 "macroy.c"
    break;

  case 114: /* SimpleExpression: ObjectData  */
#line 824 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2845 "macroy.c"
    break;

  case 115: /* SimpleExpression: ObjectMethod  */
#line 825 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2851 "macroy.c"
    break;

  case 116: /* SimpleExpression: WithData  */
#line 826 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2857 "macroy.c"
    break;

  case 117: /* SimpleExpression: WithMethod  */
#line 827 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2863 "macroy.c"
    break;

  case 118: /* SimpleExpression: AliasExpr  */
#line 828 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2869 "macroy.c"
    break;

  case 119: /* SimpleExpression: ExprAssign  */
#line 829 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2875 "macroy.c"
    break;

  case 120: /* SimpleExpression: ExprOperEq  */
#line 830 "../../macro.y"
                                         { HB_MACRO_IFENABLED( (yyval.asExpr), (yyvsp[0].asExpr), HB_SM_HARBOUR ); }
#line 2881 "macroy.c"
    break;

  case 121: /* SimpleExpression: ExprPostOp  */
#line 831 "../../macro.y"
                                         { HB_MACRO_IFENABLED( (yyval.asExpr), (yyvsp[0].asExpr), HB_SM_HARBOUR ); }
#line 2887 "macroy.c"
    break;

  case 122: /* SimpleExpression: ExprPreOp  */
#line 832 "../../macro.y"
                                         { HB_MACRO_IFENABLED( (yyval.asExpr), (yyvsp[0].asExpr), HB_SM_HARBOUR ); }
#line 2893 "macroy.c"
    break;

  case 123: /* SimpleExpression: ExprUnary  */
#line 833 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2899 "macroy.c"
    break;

  case 124: /* SimpleExpression: ExprMath  */
#line 834 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2905 "macroy.c"
    break;

  case 125: /* SimpleExpression: ExprBool  */
#line 835 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2911 "macroy.c"
    break;

  case 126: /* SimpleExpression: ExprRelation  */
#line 836 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2917 "macroy.c"
    break;

  case 127: /* Expression: SimpleExpression  */
#line 839 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); HB_MACRO_CHECK( (yyval.asExpr) ); }
#line 2923 "macroy.c"
    break;

  case 128: /* Expression: PareExpList  */
#line 840 "../../macro.y"
                                         { (yyval.asExpr) = (yyvsp[0].asExpr); HB_MACRO_CHECK( (yyval.asExpr) ); }
#line 2929 "macroy.c"
    break;

  case 129: /* $@1: %empty  */
#line 843 "../../macro.y"
                              {
                                 if( !(HB_MACRO_DATA->Flags & HB_MACRO_GEN_LIST) )
                                 {
                                    HB_TRACE(HB_TR_DEBUG, ("macro -> invalid expression: %s", HB_MACRO_DATA->string));
                                    hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );
                                    hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM );
                                    YYABORT;
                                 }
                              }
#line 2943 "macroy.c"
    break;

  case 130: /* RootParamList: Argument ',' $@1 Argument  */
#line 852 "../../macro.y"
                              {
                                 HB_MACRO_DATA->iListElements = 1;
                                 (yyval.asExpr) = hb_compExprAddListExpr( ( HB_MACRO_DATA->Flags & HB_MACRO_GEN_PARE ) ? hb_compExprNewList( (yyvsp[-3].asExpr) ) : hb_compExprNewArgList( (yyvsp[-3].asExpr) ), (yyvsp[0].asExpr) );
                              }
#line 2952 "macroy.c"
    break;

  case 131: /* AsParamList: RootParamList  */
#line 858 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2958 "macroy.c"
    break;

  case 132: /* AsParamList: AsParamList ',' Argument  */
#line 859 "../../macro.y"
                                          { HB_MACRO_DATA->iListElements++; (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 2964 "macroy.c"
    break;

  case 133: /* EmptyExpression: %empty  */
#line 862 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewEmpty(); }
#line 2970 "macroy.c"
    break;

  case 159: /* PostOp: INC  */
#line 895 "../../macro.y"
                     { (yyval.asExpr) = hb_compExprNewPostInc( (yyvsp[-1].asExpr) ); }
#line 2976 "macroy.c"
    break;

  case 160: /* PostOp: DEC  */
#line 896 "../../macro.y"
                     { (yyval.asExpr) = hb_compExprNewPostDec( (yyvsp[-1].asExpr) ); }
#line 2982 "macroy.c"
    break;

  case 161: /* ExprPostOp: LeftExpression PostOp  */
#line 902 "../../macro.y"
                                                { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 2988 "macroy.c"
    break;

  case 162: /* ExprPreOp: INC Expression  */
#line 905 "../../macro.y"
                                             { (yyval.asExpr) = hb_compExprNewPreInc( (yyvsp[0].asExpr) ); }
#line 2994 "macroy.c"
    break;

  case 163: /* ExprPreOp: DEC Expression  */
#line 906 "../../macro.y"
                                             { (yyval.asExpr) = hb_compExprNewPreDec( (yyvsp[0].asExpr) ); }
#line 3000 "macroy.c"
    break;

  case 164: /* ExprUnary: NOT Expression  */
#line 909 "../../macro.y"
                                             { (yyval.asExpr) = hb_compExprNewNot( (yyvsp[0].asExpr) ); }
#line 3006 "macroy.c"
    break;

  case 165: /* ExprUnary: '-' Expression  */
#line 910 "../../macro.y"
                                             { (yyval.asExpr) = hb_compExprNewNegate( (yyvsp[0].asExpr) ); }
#line 3012 "macroy.c"
    break;

  case 166: /* ExprUnary: '+' Expression  */
#line 911 "../../macro.y"
                                             { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 3018 "macroy.c"
    break;

  case 167: /* ExprAssign: NumValue INASSIGN Expression  */
#line 914 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3024 "macroy.c"
    break;

  case 168: /* ExprAssign: NilValue INASSIGN Expression  */
#line 915 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3030 "macroy.c"
    break;

  case 169: /* ExprAssign: Date INASSIGN Expression  */
#line 916 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3036 "macroy.c"
    break;

  case 170: /* ExprAssign: DateTime INASSIGN Expression  */
#line 917 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3042 "macroy.c"
    break;

  case 171: /* ExprAssign: LiteralValue INASSIGN Expression  */
#line 918 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3048 "macroy.c"
    break;

  case 172: /* ExprAssign: CodeBlock INASSIGN Expression  */
#line 919 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3054 "macroy.c"
    break;

  case 173: /* ExprAssign: Logical INASSIGN Expression  */
#line 920 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3060 "macroy.c"
    break;

  case 174: /* ExprAssign: SelfValue INASSIGN Expression  */
#line 921 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3066 "macroy.c"
    break;

  case 175: /* ExprAssign: Array INASSIGN Expression  */
#line 922 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3072 "macroy.c"
    break;

  case 176: /* ExprAssign: ArrayAt INASSIGN Expression  */
#line 923 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3078 "macroy.c"
    break;

  case 177: /* ExprAssign: Hash INASSIGN Expression  */
#line 924 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3084 "macroy.c"
    break;

  case 178: /* ExprAssign: AliasVar INASSIGN Expression  */
#line 925 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3090 "macroy.c"
    break;

  case 179: /* ExprAssign: AliasExpr INASSIGN Expression  */
#line 926 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3096 "macroy.c"
    break;

  case 180: /* ExprAssign: MacroVar INASSIGN Expression  */
#line 927 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3102 "macroy.c"
    break;

  case 181: /* ExprAssign: MacroExpr INASSIGN Expression  */
#line 928 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3108 "macroy.c"
    break;

  case 182: /* ExprAssign: Variable INASSIGN Expression  */
#line 929 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3114 "macroy.c"
    break;

  case 183: /* ExprAssign: VariableAt INASSIGN Expression  */
#line 930 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3120 "macroy.c"
    break;

  case 184: /* ExprAssign: PareExpList INASSIGN Expression  */
#line 931 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3126 "macroy.c"
    break;

  case 185: /* ExprAssign: FunCall INASSIGN Expression  */
#line 932 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3132 "macroy.c"
    break;

  case 186: /* ExprAssign: IfInline INASSIGN Expression  */
#line 933 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3138 "macroy.c"
    break;

  case 187: /* ExprAssign: ObjectData INASSIGN Expression  */
#line 934 "../../macro.y"
                                                { HB_MACRO_IFENABLED( (yyval.asExpr), hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ), HB_SM_HARBOUR ); }
#line 3144 "macroy.c"
    break;

  case 188: /* ExprAssign: ObjectMethod INASSIGN Expression  */
#line 935 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3150 "macroy.c"
    break;

  case 189: /* ExprAssign: WithData INASSIGN Expression  */
#line 936 "../../macro.y"
                                                { HB_MACRO_IFENABLED( (yyval.asExpr), hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ), HB_SM_HARBOUR ); }
#line 3156 "macroy.c"
    break;

  case 190: /* ExprAssign: WithMethod INASSIGN Expression  */
#line 937 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprAssign( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3162 "macroy.c"
    break;

  case 191: /* ExprPlusEq: LeftExpression PLUSEQ Expression  */
#line 940 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewPlusEq( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3168 "macroy.c"
    break;

  case 192: /* ExprMinusEq: LeftExpression MINUSEQ Expression  */
#line 943 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMinusEq( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3174 "macroy.c"
    break;

  case 193: /* ExprMultEq: LeftExpression MULTEQ Expression  */
#line 946 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMultEq( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3180 "macroy.c"
    break;

  case 194: /* ExprDivEq: LeftExpression DIVEQ Expression  */
#line 949 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewDivEq( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3186 "macroy.c"
    break;

  case 195: /* ExprModEq: LeftExpression MODEQ Expression  */
#line 952 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewModEq( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3192 "macroy.c"
    break;

  case 196: /* ExprExpEq: LeftExpression EXPEQ Expression  */
#line 955 "../../macro.y"
                                                { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewExpEq( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3198 "macroy.c"
    break;

  case 197: /* ExprOperEq: ExprPlusEq  */
#line 958 "../../macro.y"
                              { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 3204 "macroy.c"
    break;

  case 198: /* ExprOperEq: ExprMinusEq  */
#line 959 "../../macro.y"
                              { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 3210 "macroy.c"
    break;

  case 199: /* ExprOperEq: ExprMultEq  */
#line 960 "../../macro.y"
                              { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 3216 "macroy.c"
    break;

  case 200: /* ExprOperEq: ExprDivEq  */
#line 961 "../../macro.y"
                              { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 3222 "macroy.c"
    break;

  case 201: /* ExprOperEq: ExprModEq  */
#line 962 "../../macro.y"
                              { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 3228 "macroy.c"
    break;

  case 202: /* ExprOperEq: ExprExpEq  */
#line 963 "../../macro.y"
                              { (yyval.asExpr) = (yyvsp[0].asExpr); }
#line 3234 "macroy.c"
    break;

  case 203: /* ExprMath: Expression '+' Expression  */
#line 966 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewPlus( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3240 "macroy.c"
    break;

  case 204: /* ExprMath: Expression '-' Expression  */
#line 967 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMinus( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3246 "macroy.c"
    break;

  case 205: /* ExprMath: Expression '*' Expression  */
#line 968 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMult( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3252 "macroy.c"
    break;

  case 206: /* ExprMath: Expression '/' Expression  */
#line 969 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewDiv( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3258 "macroy.c"
    break;

  case 207: /* ExprMath: Expression '%' Expression  */
#line 970 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMod( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3264 "macroy.c"
    break;

  case 208: /* ExprMath: Expression POWER Expression  */
#line 971 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewPower( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3270 "macroy.c"
    break;

  case 209: /* ExprMath: Expression BITAND Expression  */
#line 972 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitAnd( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3276 "macroy.c"
    break;

  case 210: /* ExprMath: Expression BITOR Expression  */
#line 973 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitOr( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3282 "macroy.c"
    break;

  case 211: /* ExprMath: Expression BITXOR Expression  */
#line 974 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitXOr( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3288 "macroy.c"
    break;

  case 212: /* ExprMath: Expression BITSHIFTR Expression  */
#line 975 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitShiftR( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3294 "macroy.c"
    break;

  case 213: /* ExprMath: Expression BITSHIFTL Expression  */
#line 976 "../../macro.y"
                                              { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewBitShiftL( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3300 "macroy.c"
    break;

  case 214: /* ExprBool: Expression AND Expression  */
#line 979 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewAnd( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3306 "macroy.c"
    break;

  case 215: /* ExprBool: Expression OR Expression  */
#line 980 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewOr( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3312 "macroy.c"
    break;

  case 216: /* ExprRelation: Expression EQ Expression  */
#line 983 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewEQ( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3318 "macroy.c"
    break;

  case 217: /* ExprRelation: Expression '<' Expression  */
#line 984 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewLT( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3324 "macroy.c"
    break;

  case 218: /* ExprRelation: Expression '>' Expression  */
#line 985 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewGT( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3330 "macroy.c"
    break;

  case 219: /* ExprRelation: Expression LE Expression  */
#line 986 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewLE( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3336 "macroy.c"
    break;

  case 220: /* ExprRelation: Expression GE Expression  */
#line 987 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewGE( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3342 "macroy.c"
    break;

  case 221: /* ExprRelation: Expression NE1 Expression  */
#line 988 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewNE( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3348 "macroy.c"
    break;

  case 222: /* ExprRelation: Expression NE2 Expression  */
#line 989 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewNE( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3354 "macroy.c"
    break;

  case 223: /* ExprRelation: Expression '$' Expression  */
#line 990 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewIN( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3360 "macroy.c"
    break;

  case 224: /* ExprRelation: Expression '=' Expression  */
#line 991 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewEqual( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3366 "macroy.c"
    break;

  case 225: /* ExprRelation: Expression LIKE Expression  */
#line 992 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewLike( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3372 "macroy.c"
    break;

  case 226: /* ExprRelation: Expression MATCH Expression  */
#line 993 "../../macro.y"
                                            { (yyval.asExpr) = hb_compExprSetOperand( hb_compExprNewMatch( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3378 "macroy.c"
    break;

  case 227: /* ArrayIndex: IndexList ']'  */
#line 996 "../../macro.y"
                                            { (yyval.asExpr) = (yyvsp[-1].asExpr); }
#line 3384 "macroy.c"
    break;

  case 228: /* IndexList: '[' Expression  */
#line 1002 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewArrayAt( (yyvsp[-2].asExpr), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3390 "macroy.c"
    break;

  case 229: /* IndexList: IndexList ',' Expression  */
#line 1003 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewArrayAt( (yyvsp[-2].asExpr), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3396 "macroy.c"
    break;

  case 230: /* IndexList: IndexList ']' '[' Expression  */
#line 1004 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewArrayAt( (yyvsp[-3].asExpr), (yyvsp[0].asExpr), HB_MACRO_PARAM ); }
#line 3402 "macroy.c"
    break;

  case 231: /* @2: %empty  */
#line 1008 "../../macro.y"
                  {
                    (yyval.asExpr) = hb_compExprNewCodeBlock();

                    if( s_iPending <= HB_MAX_PENDING_MACRO_EXP )
                    {
                       s_Pending[ s_iPending++ ] = (yyval.asExpr);
                    }
                  }
#line 3415 "macroy.c"
    break;

  case 232: /* CodeBlock: '{' CBMARKER @2 BlockNoVar CBMARKER BlockExpList '}'  */
#line 1017 "../../macro.y"
                  {
                    (yyval.asExpr) = (yyvsp[-4].asExpr);

                    if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyval.asExpr) )
                       s_iPending--;
                  }
#line 3426 "macroy.c"
    break;

  case 233: /* @3: %empty  */
#line 1024 "../../macro.y"
                  {
                    (yyval.asExpr) = hb_compExprNewCodeBlock();

                    if( s_iPending <= HB_MAX_PENDING_MACRO_EXP )
                    {
                       s_Pending[ s_iPending++ ] = (yyval.asExpr);
                    }
                  }
#line 3439 "macroy.c"
    break;

  case 234: /* CodeBlock: '{' CBMARKER @3 BlockVarList CBMARKER BlockExpList '}'  */
#line 1033 "../../macro.y"
                  {
                    (yyval.asExpr) = (yyvsp[-4].asExpr);

                    if( s_iPending && s_Pending[ s_iPending - 1 ] == (yyval.asExpr) )
                       s_iPending--;
                  }
#line 3450 "macroy.c"
    break;

  case 235: /* BlockExpList: Expression  */
#line 1043 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[(-2) - (1)].asExpr), (yyvsp[0].asExpr) ); }
#line 3456 "macroy.c"
    break;

  case 236: /* BlockExpList: BlockExpList ',' Expression  */
#line 1044 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[(-2) - (3)].asExpr), (yyvsp[0].asExpr) ); }
#line 3462 "macroy.c"
    break;

  case 237: /* BlockNoVar: %empty  */
#line 1050 "../../macro.y"
                                          { (yyval.asExpr) = NULL; }
#line 3468 "macroy.c"
    break;

  case 238: /* BlockVarList: IdentName  */
#line 1053 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprCBVarAdd( (yyvsp[-1].asExpr), (yyvsp[0].string), HB_MACRO_PARAM ); }
#line 3474 "macroy.c"
    break;

  case 239: /* BlockVarList: BlockVarList ',' IdentName  */
#line 1054 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprCBVarAdd( (yyvsp[-3].asExpr), (yyvsp[0].string), HB_MACRO_PARAM ); HB_MACRO_CHECK( (yyval.asExpr) ); }
#line 3480 "macroy.c"
    break;

  case 240: /* ExpList: '(' EmptyExpression  */
#line 1057 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprNewList( (yyvsp[0].asExpr) ); }
#line 3486 "macroy.c"
    break;

  case 241: /* ExpList: ExpList ',' EmptyExpression  */
#line 1058 "../../macro.y"
                                          { (yyval.asExpr) = hb_compExprAddListExpr( (yyvsp[-2].asExpr), (yyvsp[0].asExpr) ); }
#line 3492 "macroy.c"
    break;

  case 242: /* PareExpList: ExpList ')'  */
#line 1061 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[-1].asExpr); }
#line 3498 "macroy.c"
    break;

  case 243: /* PareExpList: ExpList error  */
#line 1062 "../../macro.y"
                                          {
                                            hb_macroError( EG_SYNTAX, HB_MACRO_PARAM );

                                            hb_compExprDelete( (yyvsp[-1].asExpr), HB_MACRO_PARAM );

                                            while( s_iPending )
                                               hb_compExprDelete( s_Pending[ --s_iPending ], HB_MACRO_PARAM );

                                            if( yychar == IDENTIFIER && yylval.string )
                                            {
                                               hb_xfree( yylval.string );
                                               yylval.string = NULL;
                                            }

                                            YYABORT;
                                          }
#line 3519 "macroy.c"
    break;

  case 244: /* PareExpListAlias: PareExpList ALIASOP  */
#line 1080 "../../macro.y"
                                          { (yyval.asExpr) = (yyvsp[-1].asExpr); }
#line 3525 "macroy.c"
    break;

  case 245: /* @4: %empty  */
#line 1084 "../../macro.y"
             { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewList( (yyvsp[-3].asExpr) ), (yyvsp[-1].asExpr) ); }
#line 3531 "macroy.c"
    break;

  case 246: /* IfInline: IIF '(' Expression ',' EmptyExpression ',' @4 EmptyExpression ')'  */
#line 1086 "../../macro.y"
             { (yyval.asExpr) = hb_compExprNewIIF( hb_compExprAddListExpr( (yyvsp[-2].asExpr), (yyvsp[-1].asExpr) ) ); }
#line 3537 "macroy.c"
    break;

  case 247: /* @5: %empty  */
#line 1089 "../../macro.y"
             { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewList( (yyvsp[-3].asExpr) ), (yyvsp[-1].asExpr) ); }
#line 3543 "macroy.c"
    break;

  case 248: /* IfInline: IF '(' Expression ',' EmptyExpression ',' @5 EmptyExpression ')'  */
#line 1091 "../../macro.y"
             { (yyval.asExpr) = hb_compExprNewIIF( hb_compExprAddListExpr( (yyvsp[-2].asExpr), (yyvsp[-1].asExpr) ) ); }
#line 3549 "macroy.c"
    break;

  case 249: /* @6: %empty  */
#line 1094 "../../macro.y"
             { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewList( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr) ); }
#line 3555 "macroy.c"
    break;

  case 250: /* IfInline: IIF '(' Expression ',' EmptyExpression @6 ')'  */
#line 1096 "../../macro.y"
             { (yyval.asExpr) = hb_compExprNewIIF( hb_compExprAddListExpr( (yyvsp[-1].asExpr), hb_compExprNew( HB_ET_NONE ) ) ); }
#line 3561 "macroy.c"
    break;

  case 251: /* @7: %empty  */
#line 1099 "../../macro.y"
             { (yyval.asExpr) = hb_compExprAddListExpr( hb_compExprNewList( (yyvsp[-2].asExpr) ), (yyvsp[0].asExpr) ); }
#line 3567 "macroy.c"
    break;

  case 252: /* IfInline: IF '(' Expression ',' EmptyExpression @7 ')'  */
#line 1101 "../../macro.y"
             { (yyval.asExpr) = hb_compExprNewIIF( hb_compExprAddListExpr( (yyvsp[-1].asExpr), hb_compExprNew( HB_ET_NONE ) ) ); }
#line 3573 "macroy.c"
    break;


#line 3577 "macroy.c"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == HB_MACRO_YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (pMacro, YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= HB_MACRO_YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == HB_MACRO_YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, pMacro);
          yychar = HB_MACRO_YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
#if defined( _MSC_VER )
   #pragma warning( disable: 4702 )
#endif

yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
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
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, pMacro);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (pMacro, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != HB_MACRO_YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, pMacro);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, pMacro);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 1103 "../../macro.y"


#ifdef __WATCOMC__
/* enable warnings for unreachable code */
#pragma warning 13 1
#endif

/*
 ** ------------------------------------------------------------------------ **
 */

int hb_macroYYParse( PHB_MACRO pMacro )
{
   int    iResult;
   void * lexBuffer;

   /* AJ: Replace hard coded MT related codes with API to make this file
    * common to ST and MT modes
    */
   #if 0
      #ifdef HB_THREAD_SUPPORT
         HB_CRITICAL_LOCK( hb_macroMutex );
      #endif
   #else
      hb_threadLock( HB_MACROMUTEX  );
   #endif

   /* Reset
    */
   s_iPending = 0;

   lexBuffer = hb_compFlexNew( pMacro );

   pMacro->status = HB_MACRO_CONT;

   iResult = yyparse( pMacro );

   hb_compFlexDelete( lexBuffer );

   /* AJ: Replace hard coded MT related codes with API to make this file
    * common to ST and MT modes
    */
   #if 0
      #ifdef HB_THREAD_SUPPORT
         HB_CRITICAL_UNLOCK( hb_macroMutex );
      #endif
   #else
      hb_threadUnLock( HB_MACROMUTEX );
   #endif

   return iResult;
}

/* ************************************************************************* */

void yyerror( PHB_MACRO pMacro, char * s )
{
   HB_SYMBOL_UNUSED( pMacro );
   HB_SYMBOL_UNUSED( s );
}
