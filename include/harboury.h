/*
 * $Id$
 */

/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_HB_COMP_YY_HARBOURY_H_INCLUDED
# define YY_HB_COMP_YY_HARBOURY_H_INCLUDED
/* Debug traces.  */
#ifndef HB_COMP_YYDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define HB_COMP_YYDEBUG 1
#  else
#   define HB_COMP_YYDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define HB_COMP_YYDEBUG 0
# endif /* ! defined YYDEBUG */
#endif  /* ! defined HB_COMP_YYDEBUG */
#if HB_COMP_YYDEBUG
extern int hb_comp_yydebug;
#endif

/* Token kinds.  */
#ifndef HB_COMP_YYTOKENTYPE
# define HB_COMP_YYTOKENTYPE
  enum hb_comp_yytokentype
  {
    HB_COMP_YYEMPTY = -2,
    HB_COMP_YYEOF = 0,             /* "end of file"  */
    HB_COMP_YYerror = 256,         /* error  */
    HB_COMP_YYUNDEF = 257,         /* "invalid token"  */
    FUNCTION = 258,                /* FUNCTION  */
    PROCEDURE = 259,               /* PROCEDURE  */
    IDENTIFIER = 260,              /* IDENTIFIER  */
    RETURN = 261,                  /* RETURN  */
    NIL = 262,                     /* NIL  */
    NUM_DOUBLE = 263,              /* NUM_DOUBLE  */
    INASSIGN = 264,                /* INASSIGN  */
    NUM_INTEGER = 265,             /* NUM_INTEGER  */
    NUM_LONG = 266,                /* NUM_LONG  */
    LOCAL = 267,                   /* LOCAL  */
    STATIC = 268,                  /* STATIC  */
    GLOBAL = 269,                  /* GLOBAL  */
    EXTERNGLOBAL = 270,            /* EXTERNGLOBAL  */
    IIF = 271,                     /* IIF  */
    IF = 272,                      /* IF  */
    ELSE = 273,                    /* ELSE  */
    ELSEIF = 274,                  /* ELSEIF  */
    END = 275,                     /* END  */
    ENDIF = 276,                   /* ENDIF  */
    LITERAL = 277,                 /* LITERAL  */
    TRUEVALUE = 278,               /* TRUEVALUE  */
    FALSEVALUE = 279,              /* FALSEVALUE  */
    NULLVALUE = 280,               /* NULLVALUE  */
    ANNOUNCE = 281,                /* ANNOUNCE  */
    DYNAMIC = 282,                 /* DYNAMIC  */
    EXTERN = 283,                  /* EXTERN  */
    INIT = 284,                    /* INIT  */
    EXIT = 285,                    /* EXIT  */
    AND = 286,                     /* AND  */
    OR = 287,                      /* OR  */
    NOT = 288,                     /* NOT  */
    PUBLIC = 289,                  /* PUBLIC  */
    EQ = 290,                      /* EQ  */
    NE1 = 291,                     /* NE1  */
    NE2 = 292,                     /* NE2  */
    INC = 293,                     /* INC  */
    DEC = 294,                     /* DEC  */
    ALIASOP = 295,                 /* ALIASOP  */
    DOCASE = 296,                  /* DOCASE  */
    CASE = 297,                    /* CASE  */
    OTHERWISE = 298,               /* OTHERWISE  */
    ENDCASE = 299,                 /* ENDCASE  */
    ENDDO = 300,                   /* ENDDO  */
    MEMVAR = 301,                  /* MEMVAR  */
    WHILE = 302,                   /* WHILE  */
    LOOP = 303,                    /* LOOP  */
    FOR = 304,                     /* FOR  */
    NEXT = 305,                    /* NEXT  */
    TO = 306,                      /* TO  */
    STEP = 307,                    /* STEP  */
    LE = 308,                      /* LE  */
    GE = 309,                      /* GE  */
    FIELD = 310,                   /* FIELD  */
    IN = 311,                      /* IN  */
    PARAMETERS = 312,              /* PARAMETERS  */
    H12AM = 313,                   /* H12AM  */
    H12PM = 314,                   /* H12PM  */
    PLUSEQ = 315,                  /* PLUSEQ  */
    MINUSEQ = 316,                 /* MINUSEQ  */
    MULTEQ = 317,                  /* MULTEQ  */
    DIVEQ = 318,                   /* DIVEQ  */
    POWER = 319,                   /* POWER  */
    EXPEQ = 320,                   /* EXPEQ  */
    MODEQ = 321,                   /* MODEQ  */
    PRIVATE = 322,                 /* PRIVATE  */
    BEGINSEQ = 323,                /* BEGINSEQ  */
    BREAK = 324,                   /* BREAK  */
    RECOVER = 325,                 /* RECOVER  */
    RECOVERUSING = 326,            /* RECOVERUSING  */
    DO = 327,                      /* DO  */
    WITH = 328,                    /* WITH  */
    SELF = 329,                    /* SELF  */
    LINE = 330,                    /* LINE  */
    MACROVAR = 331,                /* MACROVAR  */
    MACROTEXT = 332,               /* MACROTEXT  */
    AS_ARRAY = 333,                /* AS_ARRAY  */
    AS_BLOCK = 334,                /* AS_BLOCK  */
    AS_CHARACTER = 335,            /* AS_CHARACTER  */
    AS_CLASS = 336,                /* AS_CLASS  */
    AS_DATE = 337,                 /* AS_DATE  */
    AS_LOGICAL = 338,              /* AS_LOGICAL  */
    AS_NUMERIC = 339,              /* AS_NUMERIC  */
    AS_OBJECT = 340,               /* AS_OBJECT  */
    AS_ENUM = 341,                 /* AS_ENUM  */
    AS_VARIANT = 342,              /* AS_VARIANT  */
    DECLARE = 343,                 /* DECLARE  */
    OPTIONAL = 344,                /* OPTIONAL  */
    DECLARE_CLASS = 345,           /* DECLARE_CLASS  */
    DECLARE_MEMBER = 346,          /* DECLARE_MEMBER  */
    AS_ARRAY_ARRAY = 347,          /* AS_ARRAY_ARRAY  */
    AS_BLOCK_ARRAY = 348,          /* AS_BLOCK_ARRAY  */
    AS_CHARACTER_ARRAY = 349,      /* AS_CHARACTER_ARRAY  */
    AS_CLASS_ARRAY = 350,          /* AS_CLASS_ARRAY  */
    AS_DATE_ARRAY = 351,           /* AS_DATE_ARRAY  */
    AS_LOGICAL_ARRAY = 352,        /* AS_LOGICAL_ARRAY  */
    AS_NUMERIC_ARRAY = 353,        /* AS_NUMERIC_ARRAY  */
    AS_OBJECT_ARRAY = 354,         /* AS_OBJECT_ARRAY  */
    AS_ENUM_ARRAY = 355,           /* AS_ENUM_ARRAY  */
    PROCREQ = 356,                 /* PROCREQ  */
    GET = 357,                     /* GET  */
    WITHOBJ = 358,                 /* WITHOBJ  */
    FOREACH = 359,                 /* FOREACH  */
    EPSILON = 360,                 /* EPSILON  */
    ENUM = 361,                    /* ENUM  */
    TRY = 362,                     /* TRY  */
    CATCH = 363,                   /* CATCH  */
    SWITCH = 364,                  /* SWITCH  */
    DEFAULT = 365,                 /* DEFAULT  */
    ENDSWITCH = 366,               /* ENDSWITCH  */
    CRITICAL = 367,                /* CRITICAL  */
    CRITICAL_STATIC = 368,         /* CRITICAL_STATIC  */
    CBMARKER = 369,                /* CBMARKER  */
    BITAND = 370,                  /* BITAND  */
    BITOR = 371,                   /* BITOR  */
    BITXOR = 372,                  /* BITXOR  */
    BITSHIFTR = 373,               /* BITSHIFTR  */
    BITSHIFTL = 374,               /* BITSHIFTL  */
    FINALLY = 375,                 /* FINALLY  */
    HASHOP = 376,                  /* HASHOP  */
    UTILITY = 377,                 /* UTILITY  */
    DIVERT = 378,                  /* DIVERT  */
    OF = 379,                      /* OF  */
    FLAGS = 380,                   /* FLAGS  */
    NAMESPACE = 381,               /* NAMESPACE  */
    RUNTIME_NAMESPACE = 382,       /* RUNTIME_NAMESPACE  */
    OPTIONAL_NAMESPACE = 383,      /* OPTIONAL_NAMESPACE  */
    EXTERNAL_NAMESPACE = 384,      /* EXTERNAL_NAMESPACE  */
    EXTERNAL_NAMESPACE_MEMBER = 385, /* EXTERNAL_NAMESPACE_MEMBER  */
    IMPLEMENTS_NAMESPACE = 386,    /* IMPLEMENTS_NAMESPACE  */
    DEFINE_NAMESPACE = 387,        /* DEFINE_NAMESPACE  */
    DEFINE_NAMESPACEMEMBER = 388,  /* DEFINE_NAMESPACEMEMBER  */
    USING_NAMESPACE = 389,         /* USING_NAMESPACE  */
    WITH_NAMESPACE = 390,          /* WITH_NAMESPACE  */
    POST = 391,                    /* POST  */
    LIKE = 392,                    /* LIKE  */
    MATCH = 393,                   /* MATCH  */
    UNARY = 394,                   /* UNARY  */
    PRE = 395                      /* PRE  */
  };
  typedef enum hb_comp_yytokentype hb_comp_yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined HB_COMP_YYSTYPE && ! defined HB_COMP_YYSTYPE_IS_DECLARED
union HB_COMP_YYSTYPE
{
#line 213 "../../harbour.sly"

   char *      string;     /* to hold a string returned by lex         */
   int         iNumber;    /* to hold a temporary integer number       */
   HB_LONG     lNumber;    /* to hold a temporary long number          */
   PHB_EXPR    asExpr;
   void *      pVoid;      /* to hold any memory structure we may need */
   struct
   {
      int      iNumber;    /* to hold a number returned by lex         */
      char *   szValue;
   } valInteger;
   struct
   {
      HB_LONG  lNumber;    /* to hold a long number returned by lex    */
      char *   szValue;
   } valLong;
   struct
   {
      double   dNumber;    /* to hold a double number returned by lex                     */
                           /* NOTE: Intentionally using "unsigned char" instead of "BYTE" */
      UCHAR    bWidth;     /* to hold the width of the value                              */
      UCHAR    bDec;       /* to hold the number of decimal points in the value           */
      char *   szValue;
   } valDouble;
   struct
   {
      char *   string;
      int      length;
      BOOL     dealloc;
   } valChar;

#line 244 "harboury.h"

};
typedef union HB_COMP_YYSTYPE HB_COMP_YYSTYPE;
# define HB_COMP_YYSTYPE_IS_TRIVIAL 1
# define HB_COMP_YYSTYPE_IS_DECLARED 1
#endif


extern HB_COMP_YYSTYPE hb_comp_yylval;


int hb_comp_yyparse (void);


#endif /* !YY_HB_COMP_YY_HARBOURY_H_INCLUDED  */
