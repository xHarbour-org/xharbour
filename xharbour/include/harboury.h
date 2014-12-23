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

#ifndef YY_HB_COMP_YY_HARBOURY_H_INCLUDED
# define YY_HB_COMP_YY_HARBOURY_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int hb_comp_yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    FUNCTION = 258,
    PROCEDURE = 259,
    IDENTIFIER = 260,
    RETURN = 261,
    NIL = 262,
    NUM_DOUBLE = 263,
    INASSIGN = 264,
    NUM_INTEGER = 265,
    NUM_LONG = 266,
    LOCAL = 267,
    STATIC = 268,
    GLOBAL = 269,
    EXTERNGLOBAL = 270,
    IIF = 271,
    IF = 272,
    ELSE = 273,
    ELSEIF = 274,
    END = 275,
    ENDIF = 276,
    LITERAL = 277,
    TRUEVALUE = 278,
    FALSEVALUE = 279,
    NULLVALUE = 280,
    ANNOUNCE = 281,
    DYNAMIC = 282,
    EXTERN = 283,
    INIT = 284,
    EXIT = 285,
    AND = 286,
    OR = 287,
    NOT = 288,
    PUBLIC = 289,
    EQ = 290,
    NE1 = 291,
    NE2 = 292,
    INC = 293,
    DEC = 294,
    ALIASOP = 295,
    DOCASE = 296,
    CASE = 297,
    OTHERWISE = 298,
    ENDCASE = 299,
    ENDDO = 300,
    MEMVAR = 301,
    WHILE = 302,
    LOOP = 303,
    FOR = 304,
    NEXT = 305,
    TO = 306,
    STEP = 307,
    LE = 308,
    GE = 309,
    FIELD = 310,
    IN = 311,
    PARAMETERS = 312,
    H12AM = 313,
    H12PM = 314,
    PLUSEQ = 315,
    MINUSEQ = 316,
    MULTEQ = 317,
    DIVEQ = 318,
    POWER = 319,
    EXPEQ = 320,
    MODEQ = 321,
    PRIVATE = 322,
    BEGINSEQ = 323,
    BREAK = 324,
    RECOVER = 325,
    RECOVERUSING = 326,
    DO = 327,
    WITH = 328,
    SELF = 329,
    LINE = 330,
    MACROVAR = 331,
    MACROTEXT = 332,
    AS_ARRAY = 333,
    AS_BLOCK = 334,
    AS_CHARACTER = 335,
    AS_CLASS = 336,
    AS_DATE = 337,
    AS_LOGICAL = 338,
    AS_NUMERIC = 339,
    AS_OBJECT = 340,
    AS_ENUM = 341,
    AS_VARIANT = 342,
    DECLARE = 343,
    OPTIONAL = 344,
    DECLARE_CLASS = 345,
    DECLARE_MEMBER = 346,
    AS_ARRAY_ARRAY = 347,
    AS_BLOCK_ARRAY = 348,
    AS_CHARACTER_ARRAY = 349,
    AS_CLASS_ARRAY = 350,
    AS_DATE_ARRAY = 351,
    AS_LOGICAL_ARRAY = 352,
    AS_NUMERIC_ARRAY = 353,
    AS_OBJECT_ARRAY = 354,
    AS_ENUM_ARRAY = 355,
    PROCREQ = 356,
    GET = 357,
    WITHOBJ = 358,
    FOREACH = 359,
    EPSILON = 360,
    ENUM = 361,
    TRY = 362,
    CATCH = 363,
    SWITCH = 364,
    DEFAULT = 365,
    ENDSWITCH = 366,
    CRITICAL = 367,
    CRITICAL_STATIC = 368,
    CBMARKER = 369,
    BITAND = 370,
    BITOR = 371,
    BITXOR = 372,
    BITSHIFTR = 373,
    BITSHIFTL = 374,
    FINALLY = 375,
    HASHOP = 376,
    UTILITY = 377,
    DIVERT = 378,
    OF = 379,
    FLAGS = 380,
    NAMESPACE = 381,
    RUNTIME_NAMESPACE = 382,
    OPTIONAL_NAMESPACE = 383,
    EXTERNAL_NAMESPACE = 384,
    EXTERNAL_NAMESPACE_MEMBER = 385,
    IMPLEMENTS_NAMESPACE = 386,
    DEFINE_NAMESPACE = 387,
    DEFINE_NAMESPACEMEMBER = 388,
    USING_NAMESPACE = 389,
    WITH_NAMESPACE = 390,
    POST = 391,
    LIKE = 392,
    MATCH = 393,
    UNARY = 394,
    PRE = 395
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{


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


};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE hb_comp_yylval;

int hb_comp_yyparse (void);

#endif /* !YY_HB_COMP_YY_HARBOURY_H_INCLUDED  */
