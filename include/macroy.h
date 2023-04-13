/*
 * $Id$
 */

/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

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

#ifndef YY_HB_MACRO_YY_MACROY_H_INCLUDED
# define YY_HB_MACRO_YY_MACROY_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int hb_macro_yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
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
    LIKE = 300,
    MATCH = 301,
    UNARY = 302,
    PRE = 303
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{


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


};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int hb_macro_yyparse (PHB_MACRO pMacro);

#endif /* !YY_HB_MACRO_YY_MACROY_H_INCLUDED  */
