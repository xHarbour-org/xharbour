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

#ifndef YY_HB_MACRO_YY_MACROY_H_INCLUDED
# define YY_HB_MACRO_YY_MACROY_H_INCLUDED
/* Debug traces.  */
#ifndef HB_MACRO_YYDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define HB_MACRO_YYDEBUG 1
#  else
#   define HB_MACRO_YYDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define HB_MACRO_YYDEBUG 0
# endif /* ! defined YYDEBUG */
#endif  /* ! defined HB_MACRO_YYDEBUG */
#if HB_MACRO_YYDEBUG
extern int hb_macro_yydebug;
#endif

/* Token kinds.  */
#ifndef HB_MACRO_YYTOKENTYPE
# define HB_MACRO_YYTOKENTYPE
  enum hb_macro_yytokentype
  {
    HB_MACRO_YYEMPTY = -2,
    HB_MACRO_YYEOF = 0,            /* "end of file"  */
    HB_MACRO_YYerror = 256,        /* error  */
    HB_MACRO_YYUNDEF = 257,        /* "invalid token"  */
    IDENTIFIER = 258,              /* IDENTIFIER  */
    NIL = 259,                     /* NIL  */
    NUM_DOUBLE = 260,              /* NUM_DOUBLE  */
    INASSIGN = 261,                /* INASSIGN  */
    NUM_LONG = 262,                /* NUM_LONG  */
    IIF = 263,                     /* IIF  */
    IF = 264,                      /* IF  */
    LITERAL = 265,                 /* LITERAL  */
    TRUEVALUE = 266,               /* TRUEVALUE  */
    FALSEVALUE = 267,              /* FALSEVALUE  */
    AND = 268,                     /* AND  */
    OR = 269,                      /* OR  */
    NOT = 270,                     /* NOT  */
    EQ = 271,                      /* EQ  */
    NE1 = 272,                     /* NE1  */
    NE2 = 273,                     /* NE2  */
    INC = 274,                     /* INC  */
    DEC = 275,                     /* DEC  */
    ALIASOP = 276,                 /* ALIASOP  */
    HASHOP = 277,                  /* HASHOP  */
    SELF = 278,                    /* SELF  */
    LE = 279,                      /* LE  */
    GE = 280,                      /* GE  */
    FIELD = 281,                   /* FIELD  */
    MACROVAR = 282,                /* MACROVAR  */
    MACROTEXT = 283,               /* MACROTEXT  */
    H12AM = 284,                   /* H12AM  */
    H12PM = 285,                   /* H12PM  */
    PLUSEQ = 286,                  /* PLUSEQ  */
    MINUSEQ = 287,                 /* MINUSEQ  */
    MULTEQ = 288,                  /* MULTEQ  */
    DIVEQ = 289,                   /* DIVEQ  */
    POWER = 290,                   /* POWER  */
    EXPEQ = 291,                   /* EXPEQ  */
    MODEQ = 292,                   /* MODEQ  */
    CBMARKER = 293,                /* CBMARKER  */
    BITAND = 294,                  /* BITAND  */
    BITOR = 295,                   /* BITOR  */
    BITXOR = 296,                  /* BITXOR  */
    BITSHIFTR = 297,               /* BITSHIFTR  */
    BITSHIFTL = 298,               /* BITSHIFTL  */
    POST = 299,                    /* POST  */
    LIKE = 300,                    /* LIKE  */
    MATCH = 301,                   /* MATCH  */
    UNARY = 302,                   /* UNARY  */
    PRE = 303                      /* PRE  */
  };
  typedef enum hb_macro_yytokentype hb_macro_yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined HB_MACRO_YYSTYPE && ! defined HB_MACRO_YYSTYPE_IS_DECLARED
union HB_MACRO_YYSTYPE
{
#line 146 "../../macro.y"

   char *      string;      /* to hold a string returned by lex */
   int         iNumber;     /* to hold a temporary integer number */
   HB_LONG     lNumber;     /* to hold a temporary long number */
   struct
   {
      char *   string;
      int      length;
   } valChar;
   struct
   {
      int      iNumber;     /* to hold a number returned by lex */
      char *   szValue;
   } valInteger;
   struct
   {
      HB_LONG  lNumber;     /* to hold a long number returned by lex */
      char *   szValue;
   } valLong;
   struct
   {
      double   dNumber;     /* to hold a double number returned by lex */
                            /* NOTE: Intentionally using "unsigned char" instead of "BYTE" */
      unsigned char bWidth; /* to hold the width of the value */
      unsigned char bDec;   /* to hold the number of decimal points in the value */
      char *   szValue;
   } valDouble;
   PHB_EXPR    asExpr;
   void *      pVoid;       /* to hold any memory structure we may need */

#line 151 "macroy.h"

};
typedef union HB_MACRO_YYSTYPE HB_MACRO_YYSTYPE;
# define HB_MACRO_YYSTYPE_IS_TRIVIAL 1
# define HB_MACRO_YYSTYPE_IS_DECLARED 1
#endif




int hb_macro_yyparse (PHB_MACRO pMacro);


#endif /* !YY_HB_MACRO_YY_MACROY_H_INCLUDED  */
