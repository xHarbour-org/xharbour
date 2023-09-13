/* $CATEGORY$SQLRDD/Parser$FILES$sql.lib$
* SQLPARSER
* SQL Lexer
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#define WIN32_LEAN_AND_MEAN

#include "compat.h"

#include "hbsql.h"

#include <ctype.h>

#include "sqly.h"          /* Bison-generated include */
#include "msg.ch"

/* #define DEBUG_YYLEX */

/* Protypes */

int sqlyylex(YYSTYPE* yylvaluep, void* s);

/*
*
*  Lexical Analyzer
*
*/

int sql_yylex(YYSTYPE* yylvaluep, void* s) {

   int token = sqlyylex(yylvaluep, s);

   #ifdef DEBUG_YYLEX
      printf("yylex: token %d\n", token);
   #endif

   return token;
}

int sqlyylex(YYSTYPE* lvalp, void* s) {

   char szDate[9];
   sql_stmt* stmt = (sql_stmt*) s;

   const char* queryPtr = stmt->queryPtr;
   const char* queryEnd = stmt->query + stmt->queryLen;

   /* Remove starting spaces */

   while (queryEnd > queryPtr  && ( isspace(*queryPtr) || *queryPtr == '\n' || *queryPtr == '\r' || *queryPtr == '\t' )) {
      ++queryPtr;
   }

   /* Check for the end of the command */

   stmt->queryPtr = stmt->errPtr = queryPtr;
   if (queryPtr == queryEnd) {
      return EOF;
   }

   /* Check for a number */

   if (( *queryPtr == '-' && ((*(queryPtr+1)) >= '0'  &&  (*(queryPtr+1)) <= '9'))  ||  ( *queryPtr == '.' && ((*(queryPtr+1)) >= '0'  &&  (*(queryPtr+1)) <= '9') )  || (*queryPtr >= '0'  &&  *queryPtr <= '9')) {
      int minus = 0;
      while (*queryPtr == '-') {
         minus = !minus;
         if (++queryPtr == queryEnd) {
            stmt->errMsg = SQL_PARSER_ERROR_NUMBER_NEGATIVE;
            return ERRORVAL;
         }
      }
      while (isspace(*queryPtr) && ( isspace(*queryPtr) || *queryPtr == '\n' || *queryPtr == '\r' || *queryPtr == '\t' )) {
         if (++queryPtr == queryEnd) {
            stmt->errMsg = SQL_PARSER_ERROR_NUMBER_EOF;
            return ERRORVAL;
         }
      }
      if (*queryPtr != '.'  &&  (*queryPtr < '0'  ||  *queryPtr > '9')) {
         stmt->errMsg = SQL_PARSER_ERROR_NUMBER;
         return ERRORVAL;
      }
      while (*queryPtr >= '0'  &&  *queryPtr <= '9') {
         if (++queryPtr == queryEnd) {
            break;
         }
      }

      if (queryPtr == queryEnd  || (*queryPtr != '.'  &&  *queryPtr != 'E'  &&  *queryPtr != 'e')) {
         /* Integer */
         int n;
         if (sscanf(stmt->queryPtr, " %d%n", &lvalp->int_val, &n) != 1) {
            stmt->errMsg = SQL_PARSER_ERROR_NUMBER_INTEGER;
            return ERRORVAL;
         }
         stmt->queryPtr += n;
         if (stmt->queryPtr > queryEnd) {
            /* Jee... */
            /* queryPtr = queryEnd; */
         }
         return INTEGERVAL;
      }
      else {
         /* Real value */
         int n;
         if (sscanf(stmt->queryPtr, " %lf%n", &lvalp->real_val, &n) != 1) {
            stmt->errMsg = SQL_PARSER_ERROR_NUMBER_FLOAT;
            return ERRORVAL;
         }
         stmt->queryPtr += n;
         if (stmt->queryPtr > queryEnd) {
            /* It may never happen */
            /* queryPtr = queryEnd; */
         }
         return REALVAL;
      }
   }

   /* Check for a String */

   if (*queryPtr == '\'') {

      char quoteChar = *queryPtr++;
      char c;
      /* int stringLen = 0; */
      while (queryPtr < queryEnd) {
         c = *queryPtr++;
         if (c == '\\') {
            if (queryPtr == queryEnd) {
               break;
            }
            c = *queryPtr++;
            if (c == '\'') {
               if (queryPtr == queryEnd) {
                  break;
               }
               queryPtr++;
            }
         }
         else if (c == quoteChar) {
            lvalp->item_val = hb_itemNew( NULL );
            hb_itemPutCL( lvalp->item_val, stmt->queryPtr+1, ( queryPtr - stmt->queryPtr - 2 ) );
            stmt->queryPtr = queryPtr;
            return STRINGVAL;
         }
         /* ++stringLen; */
      }
      stmt->errMsg = SQL_PARSER_ERROR_STRING;
      return ERRORVAL;
   }

   /* Check for a QUOTED IDENT */

   if (*queryPtr == '"') {
      /*  This is a string  */
      char quoteChar = *queryPtr++;
      char c;
      while (queryPtr < queryEnd) {
         c = *queryPtr++;
         if (c == quoteChar)
         {
            lvalp->item_val = hb_itemNew( NULL );
            hb_itemPutCL( lvalp->item_val, stmt->queryPtr + 1, ( queryPtr - stmt->queryPtr - 2 ) );
            stmt->queryPtr = queryPtr;
            return QUOTED_IDENT;
         }
      }
      stmt->errMsg = SQL_PARSER_ERROR_STRING_QUOTED;
      return ERRORVAL;
   }

   /* Check for a DATE */

   if (*queryPtr == '[') {
      /*  This is a string  */
      if (queryPtr + 9 < queryEnd && queryPtr[9] == ']' ) {
         memcpy( szDate, stmt->queryPtr + 1, 8 );
         szDate[8] = 0;
         lvalp->item_val = hb_itemNew( NULL );
         hb_itemPutDS( lvalp->item_val, szDate );
         stmt->queryPtr = stmt->queryPtr + 10;
         return DATEVAL;
      }
      stmt->errMsg = SQL_PARSER_ERROR_STRING_DATE;
      return ERRORVAL;
   }

   /* Check for BINDVAR */

   if (*queryPtr == ':') {
      ++queryPtr;
      stmt->queryPtr = queryPtr;
      return BINDVAR;
   }

   /* Operators */

   if (*queryPtr == '+') {
      ++queryPtr;
      lvalp->iOperator = SQL_PCODE_OPERATOR_PLUS;
      stmt->queryPtr = queryPtr;
      return OPERATOR;
   }

   if (*queryPtr == '-') {
      ++queryPtr;
      lvalp->iOperator = SQL_PCODE_OPERATOR_MINUS;
      stmt->queryPtr = queryPtr;
      return OPERATOR;
   }

   if (*queryPtr == '*') {
      ++queryPtr;
      lvalp->iOperator = SQL_PCODE_OPERATOR_MULT;
      stmt->queryPtr = queryPtr;
      return ASTERISK;
   }

   if (*queryPtr == '/') {
      ++queryPtr;
      lvalp->iOperator = SQL_PCODE_OPERATOR_DIV;
      stmt->queryPtr = queryPtr;
      return OPERATOR;
   }

   if (*queryPtr == '=') {
      ++queryPtr;
      if (queryPtr < queryEnd) {
         if (*queryPtr == '=') {
            ++queryPtr;
         }
      }
      lvalp->iOperator = SQL_PCODE_OPERATOR_EQ;
      stmt->queryPtr = queryPtr;
      return EQUALS;
   }

   if (*queryPtr == '>') {
      ++queryPtr;
      if (queryPtr < queryEnd  &&  *queryPtr == '=') {
         ++queryPtr;
         lvalp->iOperator = SQL_PCODE_OPERATOR_GE;
         stmt->queryPtr = queryPtr;
         return COMPARE;
      }
      lvalp->iOperator = SQL_PCODE_OPERATOR_GT;
      stmt->queryPtr = queryPtr;
      return COMPARE;
   }

   if (*queryPtr == '<') {
      ++queryPtr;
      if (queryPtr < queryEnd  &&  *queryPtr == '=') {
         ++queryPtr;
         lvalp->iOperator = SQL_PCODE_OPERATOR_LE;
         stmt->queryPtr = queryPtr;
         return COMPARE;
      }
      if (queryPtr < queryEnd  &&  *queryPtr == '>') {
         ++queryPtr;
         lvalp->iOperator = SQL_PCODE_OPERATOR_NE;
         stmt->queryPtr = queryPtr;
         return COMPARE;
      }
      lvalp->iOperator = SQL_PCODE_OPERATOR_LT;
      stmt->queryPtr = queryPtr;
      return COMPARE;
   }

   if (*queryPtr == '!') {
      ++queryPtr;
      if (queryPtr < queryEnd  &&  *queryPtr == '=') {
         ++queryPtr;
         lvalp->iOperator = SQL_PCODE_OPERATOR_NE;
         stmt->queryPtr = queryPtr;
         return COMPARE;
      }
      return NOT;
   }

   if (*queryPtr == '?') {
      stmt->queryPtr = ++queryPtr;
      lvalp->param = stmt->numParam++;
      if (queryPtr < queryEnd  &&  *queryPtr == '?') {
         ++queryPtr;
         stmt->queryPtr = queryPtr;
         return PARAM_NOT_NULL;
      }
      return PARAM;
   }

   if (isalpha(*queryPtr) || *queryPtr == '_' ) {
      switch (queryPtr[0]) {
      case 'a':
      case 'A':
         if (queryPtr+3 <= queryEnd  &&
         (queryPtr[1] == 'l'  ||  queryPtr[1] == 'L')  &&
         (queryPtr[2] == 'l'  ||  queryPtr[2] == 'L')  &&
         (queryPtr+3 == queryEnd  || !isalnum_(queryPtr[3]))) {
            stmt->queryPtr = queryPtr + 3;
            return ALL;
         }
         if (queryPtr+3 <= queryEnd  &&
         (queryPtr[1] == 'n'  ||  queryPtr[1] == 'N')  &&
         (queryPtr[2] == 'd'  ||  queryPtr[2] == 'D')  &&
         (queryPtr+3 == queryEnd  || !isalnum_(queryPtr[3]))) {
            stmt->queryPtr = queryPtr + 3;
            return AND_OP;
         }
         if (queryPtr+3 <= queryEnd  &&
         (queryPtr[1] == 's'  ||  queryPtr[1] == 'S')  &&
         (queryPtr[2] == 'c'  ||  queryPtr[2] == 'C')  &&
         (queryPtr+3 == queryEnd  || !isalnum_(queryPtr[3]))) {
            stmt->queryPtr = queryPtr + 3;
            return ASC;
         }
         if (queryPtr+2 <= queryEnd  &&
         (queryPtr[1] == 's'  ||  queryPtr[1] == 'S')  &&
         (queryPtr+2 == queryEnd  || !isalnum_(queryPtr[2]))) {
            stmt->queryPtr = queryPtr + 2;
            return AS;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'b'  ||  queryPtr[1] == 'B')  &&
         (queryPtr[2] == 's'  ||  queryPtr[2] == 'S')  &&
         (queryPtr[3] == '(' )  &&
         (queryPtr+4 != queryEnd)) {
            stmt->queryPtr = queryPtr + 4;
            return ABS;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'v'  ||  queryPtr[1] == 'V')  &&
         (queryPtr[2] == 'g'  ||  queryPtr[2] == 'G')  &&
         (queryPtr[3] == '(' )  &&
         (queryPtr+4 != queryEnd)) {
            stmt->queryPtr = queryPtr + 4;
            return AVG;
         }
         break;
      case 'b':
      case 'B':
         if (queryPtr+2 <= queryEnd  &&
         (queryPtr[1] == 'y'  ||  queryPtr[1] == 'Y')  &&
         (queryPtr+2 == queryEnd  || !isalnum_(queryPtr[2]))) {
            stmt->queryPtr = queryPtr + 2;
            return BY;
         }
         break;
      case 'c':
      case 'C':
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'o'  ||  queryPtr[1] == 'O')  &&
         (queryPtr[2] == 'u'  ||  queryPtr[2] == 'U')  &&
         (queryPtr[3] == 'n'  ||  queryPtr[3] == 'N')  &&
         (queryPtr[4] == 't'  ||  queryPtr[4] == 'T')  &&
         (queryPtr[5] == '(' )  &&
         (queryPtr+6 != queryEnd)) {
            stmt->queryPtr = queryPtr + 6;
            return COUNT;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'o'  ||  queryPtr[1] == 'O')  &&
         (queryPtr[2] == 'n'  ||  queryPtr[2] == 'N')  &&
         (queryPtr[3] == 'c'  ||  queryPtr[3] == 'C')  &&
         (queryPtr[4] == 'a'  ||  queryPtr[4] == 'A')  &&
         (queryPtr[5] == 't'  ||  queryPtr[5] == 'T')  &&
         (queryPtr+6 == queryEnd  || !isalnum_(queryPtr[6]))) {
            stmt->queryPtr = queryPtr + 6;
            lvalp->iOperator = SQL_PCODE_OPERATOR_CONCAT;
            return OPERATOR;
         }
         break;
      case 'd':
      case 'D':
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'e'  ||  queryPtr[1] == 'E')  &&
         (queryPtr[2] == 's'  ||  queryPtr[2] == 'S')  &&
         (queryPtr[3] == 'c'  ||  queryPtr[3] == 'C')  &&
         (queryPtr+4 == queryEnd  || !isalnum_(queryPtr[4]))) {
            stmt->queryPtr = queryPtr + 4;
            return DESC;
         }
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'e'  ||  queryPtr[1] == 'E')  &&
         (queryPtr[2] == 'l'  ||  queryPtr[2] == 'L')  &&
         (queryPtr[3] == 'e'  ||  queryPtr[3] == 'E')  &&
         (queryPtr[4] == 't'  ||  queryPtr[4] == 'T')  &&
         (queryPtr[5] == 'e'  ||  queryPtr[5] == 'E')  &&
         (queryPtr+6 == queryEnd  || !isalnum_(queryPtr[6]))) {
            stmt->queryPtr = queryPtr + 6;
            return DELETE_SQL;
         }
         if (queryPtr+8 <= queryEnd  &&
         (queryPtr[1] == 'i'  ||  queryPtr[1] == 'I')  &&
         (queryPtr[2] == 's'  ||  queryPtr[2] == 'S')  &&
         (queryPtr[3] == 't'  ||  queryPtr[3] == 'T')  &&
         (queryPtr[4] == 'i'  ||  queryPtr[4] == 'I')  &&
         (queryPtr[5] == 'n'  ||  queryPtr[5] == 'N')  &&
         (queryPtr[6] == 'c'  ||  queryPtr[6] == 'C')  &&
         (queryPtr[7] == 't'  ||  queryPtr[7] == 'T')  &&
         (queryPtr+8 == queryEnd  || !isalnum_(queryPtr[8]))) {
            stmt->queryPtr = queryPtr + 8;
            return DISTINCT;
         }
         if (queryPtr+5 <= queryEnd  &&
         (queryPtr[1] == 'a'  ||  queryPtr[1] == 'A')  &&
         (queryPtr[2] == 't'  ||  queryPtr[2] == 'T')  &&
         (queryPtr[3] == 'e'  ||  queryPtr[3] == 'E')  &&
         (queryPtr[4] == '(' )  &&
         (queryPtr+5 != queryEnd)) {
            stmt->queryPtr = queryPtr + 5;
            return CURRENT_DATE;
         }
         break;
      case 'f':
      case 'F':
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'r'  ||  queryPtr[1] == 'R')  &&
         (queryPtr[2] == 'o'  ||  queryPtr[2] == 'O')  &&
         (queryPtr[3] == 'm'  ||  queryPtr[3] == 'M')  &&
         (queryPtr+4 == queryEnd  || !isalnum_(queryPtr[4]))) {
            stmt->queryPtr = queryPtr + 4;
            return FROM;
         }
         break;
      case 'g':
      case 'G':
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'r'  ||  queryPtr[1] == 'R')  &&
         (queryPtr[2] == 'o'  ||  queryPtr[2] == 'O')  &&
         (queryPtr[3] == 'u'  ||  queryPtr[3] == 'U')  &&
         (queryPtr[4] == 'p'  ||  queryPtr[4] == 'P')  &&
         (queryPtr+5 == queryEnd  || !isalnum_(queryPtr[5]))) {
            stmt->queryPtr = queryPtr + 5;
            return GROUP;
         }
         break;
      case 'i':
      case 'I':
         if (queryPtr+7 <= queryEnd  &&
         (queryPtr[1] == 's'  ||  queryPtr[1] == 'S')  &&
         (queryPtr[2] == 'n'  ||  queryPtr[2] == 'N')  &&
         (queryPtr[3] == 'u'  ||  queryPtr[3] == 'U')  &&
         (queryPtr[4] == 'l'  ||  queryPtr[4] == 'L')  &&
         (queryPtr[5] == 'l'  ||  queryPtr[5] == 'L')  &&
         (queryPtr[6] == '(' )  &&
         (queryPtr+7 != queryEnd)) {
            stmt->queryPtr = queryPtr + 7;
            return TOKEN_ISNULL;
         }
         if (queryPtr+2 <= queryEnd  &&
         (queryPtr[1] == 's'  ||  queryPtr[1] == 'S')  &&
         (queryPtr+2 == queryEnd  || !isalnum_(queryPtr[2]))) {
            stmt->queryPtr = queryPtr + 2;
            return IS_OP;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'n'  ||  queryPtr[1] == 'N')  &&
         (queryPtr[2] == 't'  ||  queryPtr[2] == 'T')  &&
         (queryPtr[3] == 'o'  ||  queryPtr[3] == 'O')  &&
         (queryPtr+4 == queryEnd  || !isalnum_(queryPtr[4]))) {
            stmt->queryPtr = queryPtr + 4;
            return INTO;
         }
         if (queryPtr+2 <= queryEnd  &&
         (queryPtr[1] == 'n'  ||  queryPtr[1] == 'N')  &&
         (queryPtr+2 == queryEnd  || !isalnum_(queryPtr[2]))) {
            stmt->queryPtr = queryPtr + 2;
            return IN_OP;
         }
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'n'  ||  queryPtr[1] == 'N')  &&
         (queryPtr[2] == 's'  ||  queryPtr[2] == 'S')  &&
         (queryPtr[3] == 'e'  ||  queryPtr[3] == 'E')  &&
         (queryPtr[4] == 'r'  ||  queryPtr[4] == 'R')  &&
         (queryPtr[5] == 't'  ||  queryPtr[5] == 'T')  &&
         (queryPtr+6 == queryEnd  || !isalnum_(queryPtr[6]))) {
            stmt->queryPtr = queryPtr + 6;
            return INSERT;
         }
         break;
      case 'j':
      case 'J':
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'o'  ||  queryPtr[1] == 'O')  &&
         (queryPtr[2] == 'i'  ||  queryPtr[2] == 'I')  &&
         (queryPtr[3] == 'n'  ||  queryPtr[3] == 'N')  &&
         (queryPtr+4 == queryEnd  || !isalnum_(queryPtr[4]))) {
            stmt->queryPtr = queryPtr + 4;
            return JOIN;
         }
         break;
      case 'k':
      case 'K':
         break;
      case 'l':
      case 'L':
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'e'  ||  queryPtr[1] == 'E')  &&
         (queryPtr[2] == 'f'  ||  queryPtr[2] == 'F')  &&
         (queryPtr[3] == 't'  ||  queryPtr[3] == 'T')  &&
         (queryPtr+4 == queryEnd  || !isalnum_(queryPtr[4]))) {
            stmt->queryPtr = queryPtr + 4;
            return LEFT;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'o'  ||  queryPtr[1] == 'O')  &&
         (queryPtr[2] == 'c'  ||  queryPtr[2] == 'C')  &&
         (queryPtr[3] == 'k'  ||  queryPtr[3] == 'K')  &&
         (queryPtr+4 == queryEnd  || !isalnum_(queryPtr[4]))) {
            stmt->queryPtr = queryPtr + 4;
            return LOCK;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'i'  ||  queryPtr[1] == 'I')  &&
         (queryPtr[2] == 'k'  ||  queryPtr[2] == 'K')  &&
         (queryPtr[3] == 'e'  ||  queryPtr[3] == 'E')  &&
         (queryPtr+4 == queryEnd  || !isalnum_(queryPtr[4]))) {
            stmt->queryPtr = queryPtr + 4;
            return LIKE;
         }
         if (queryPtr+5 <= queryEnd  &&
         (queryPtr[1] == 'i'  ||  queryPtr[1] == 'I')  &&
         (queryPtr[2] == 'm'  ||  queryPtr[2] == 'M')  &&
         (queryPtr[3] == 'i'  ||  queryPtr[3] == 'I')  &&
         (queryPtr[4] == 't'  ||  queryPtr[4] == 'T')  &&
         (queryPtr+5 == queryEnd  || !isalnum_(queryPtr[5]))) {
            stmt->queryPtr = queryPtr + 5;
            return LIMIT;
         }
         break;
      case 'm':
      case 'M':
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'a'  ||  queryPtr[1] == 'A')  &&
         (queryPtr[2] == 'x'  ||  queryPtr[2] == 'X')  &&
         (queryPtr[3] == '(' )  &&
         (queryPtr+4 != queryEnd)) {
            stmt->queryPtr = queryPtr + 4;
            return MAX;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'i'  ||  queryPtr[1] == 'I')  &&
         (queryPtr[2] == 'n'  ||  queryPtr[2] == 'N')  &&
         (queryPtr[3] == '(' )  &&
         (queryPtr+4 != queryEnd)) {
            stmt->queryPtr = queryPtr + 4;
            return MIN;
         }
         break;
      case 'n':
      case 'N':
         if (queryPtr+3 <= queryEnd  &&
         (queryPtr[1] == 'o'  ||  queryPtr[1] == 'O')  &&
         (queryPtr[2] == 't'  ||  queryPtr[2] == 'T')  &&
         (queryPtr+3 == queryEnd  || !isalnum_(queryPtr[3]))) {
            stmt->queryPtr = queryPtr + 3;
            return NOT;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'u'  ||  queryPtr[1] == 'U')  &&
         (queryPtr[2] == 'l'  ||  queryPtr[2] == 'L')  &&
         (queryPtr[3] == 'l'  ||  queryPtr[3] == 'L')  &&
         (queryPtr+4 == queryEnd  || !isalnum_(queryPtr[4]))) {
            stmt->queryPtr = queryPtr + 4;
            return NULLVAL;
         }
         break;
      case 'o':
      case 'O':
         if (queryPtr+2 <= queryEnd  &&
         (queryPtr[1] == 'r'  ||  queryPtr[1] == 'R')  &&
         (queryPtr+2 == queryEnd  || !isalnum_(queryPtr[2]))) {
            stmt->queryPtr = queryPtr + 2;
            return OR;
         }
         if (queryPtr+5 <= queryEnd  &&
         (queryPtr[1] == 'r'  ||  queryPtr[1] == 'R')  &&
         (queryPtr[2] == 'd'  ||  queryPtr[2] == 'D')  &&
         (queryPtr[3] == 'e'  ||  queryPtr[3] == 'E')  &&
         (queryPtr[4] == 'r'  ||  queryPtr[4] == 'R')  &&
         (queryPtr+5 == queryEnd  || !isalnum_(queryPtr[5]))) {
            stmt->queryPtr = queryPtr + 5;
            return ORDER;
         }
         if (queryPtr+5 <= queryEnd  &&
         (queryPtr[1] == 'u'  ||  queryPtr[1] == 'U')  &&
         (queryPtr[2] == 't'  ||  queryPtr[2] == 'T')  &&
         (queryPtr[3] == 'e'  ||  queryPtr[3] == 'E')  &&
         (queryPtr[4] == 'r'  ||  queryPtr[4] == 'R')  &&
         (queryPtr+5 == queryEnd  || !isalnum_(queryPtr[5]))) {
            stmt->queryPtr = queryPtr + 5;
            return OUTER;
         }
         break;
      case 'p':
      case 'P':
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'o'  ||  queryPtr[1] == 'O')  &&
         (queryPtr[2] == 'w'  ||  queryPtr[2] == 'W')  &&
         (queryPtr[3] == 'e'  ||  queryPtr[3] == 'E')  &&
         (queryPtr[4] == 'r'  ||  queryPtr[4] == 'R')  &&
         (queryPtr[5] == '(' )  &&
         (queryPtr+6 != queryEnd)) {
            stmt->queryPtr = queryPtr + 6;
            return POWER;
         }
         break;
      case 'r':
      case 'R':
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'o'  ||  queryPtr[1] == 'O')  &&
         (queryPtr[2] == 'u'  ||  queryPtr[2] == 'U')  &&
         (queryPtr[3] == 'n'  ||  queryPtr[3] == 'N')  &&
         (queryPtr[4] == 'd'  ||  queryPtr[4] == 'D')  &&
         (queryPtr[5] == '(' )  &&
         (queryPtr+6 != queryEnd)) {
            stmt->queryPtr = queryPtr + 6;
            return ROUND;
         }
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'i'  ||  queryPtr[1] == 'I')  &&
         (queryPtr[2] == 'g'  ||  queryPtr[2] == 'G')  &&
         (queryPtr[3] == 'h'  ||  queryPtr[3] == 'H')  &&
         (queryPtr[4] == 't'  ||  queryPtr[4] == 'T')  &&
         (queryPtr[5] == '(' )  &&
         (queryPtr+6 == queryEnd  || !isalnum_(queryPtr[6]))) {
            stmt->queryPtr = queryPtr + 6;
            return RIGHT;
         }
         break;
      case 's':
      case 'S':
         if (queryPtr+3 <= queryEnd  &&
         (queryPtr[1] == 'e'  ||  queryPtr[1] == 'E')  &&
         (queryPtr[2] == 't'  ||  queryPtr[2] == 'T')  &&
         (queryPtr+3 == queryEnd  || !isalnum_(queryPtr[3]))) {
            stmt->queryPtr = queryPtr + 3;
            return SET;
         }
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'e'  ||  queryPtr[1] == 'E')  &&
         (queryPtr[2] == 'l'  ||  queryPtr[2] == 'L')  &&
         (queryPtr[3] == 'e'  ||  queryPtr[3] == 'E')  &&
         (queryPtr[4] == 'c'  ||  queryPtr[4] == 'C')  &&
         (queryPtr[5] == 't'  ||  queryPtr[5] == 'T')  &&
         (queryPtr+6 == queryEnd  || !isalnum_(queryPtr[6]))) {
            stmt->queryPtr = queryPtr + 6;
            return SELECT;
         }
         if (queryPtr+7 <= queryEnd  &&
         (queryPtr[1] == 'u'  ||  queryPtr[1] == 'U')  &&
         (queryPtr[2] == 'b'  ||  queryPtr[2] == 'B')  &&
         (queryPtr[3] == 's'  ||  queryPtr[3] == 'S')  &&
         (queryPtr[4] == 't'  ||  queryPtr[4] == 'T')  &&
         (queryPtr[5] == 'r'  ||  queryPtr[5] == 'R')  &&
         (queryPtr[6] == '(' )  &&
         (queryPtr+7 != queryEnd)) {
            stmt->queryPtr = queryPtr + 7;
            return SUBSTR;
         }
         if (queryPtr+4 <= queryEnd  &&
         (queryPtr[1] == 'u'  ||  queryPtr[1] == 'U')  &&
         (queryPtr[2] == 'm'  ||  queryPtr[2] == 'M')  &&
         (queryPtr[3] == '(' )  &&
         (queryPtr+4 != queryEnd)) {
            stmt->queryPtr = queryPtr + 4;
            return SUM;
         }
         break;
      case 't':
      case 'T':
         if (queryPtr+5 <= queryEnd  &&
         (queryPtr[1] == 'r'  ||  queryPtr[1] == 'R')  &&
         (queryPtr[2] == 'i'  ||  queryPtr[2] == 'I')  &&
         (queryPtr[3] == 'm'  ||  queryPtr[3] == 'M')  &&
         (queryPtr[4] == '(' )  &&
         (queryPtr+5 != queryEnd)) {
            stmt->queryPtr = queryPtr + 5;
            return TRIM;
         }
         break;
      case 'u':
      case 'U':
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'p'  ||  queryPtr[1] == 'P')  &&
         (queryPtr[2] == 'd'  ||  queryPtr[2] == 'D')  &&
         (queryPtr[3] == 'a'  ||  queryPtr[3] == 'A')  &&
         (queryPtr[4] == 't'  ||  queryPtr[4] == 'T')  &&
         (queryPtr[5] == 'e'  ||  queryPtr[5] == 'E')  &&
         (queryPtr+6 == queryEnd  || !isalnum_(queryPtr[6]))) {
            stmt->queryPtr = queryPtr + 6;
            return UPDATE;
         }
         if (queryPtr+5 <= queryEnd  &&
         (queryPtr[1] == 'n'  ||  queryPtr[1] == 'N')  &&
         (queryPtr[2] == 'i'  ||  queryPtr[2] == 'I')  &&
         (queryPtr[3] == 'o'  ||  queryPtr[3] == 'O')  &&
         (queryPtr[4] == 'n'  ||  queryPtr[4] == 'N')  &&
         (queryPtr+5 == queryEnd  || !isalnum_(queryPtr[5]))) {
            stmt->queryPtr = queryPtr + 5;
            return UNION;
         }
         break;
      case 'v':
      case 'V':
         if (queryPtr+6 <= queryEnd  &&
         (queryPtr[1] == 'a'  ||  queryPtr[1] == 'A')  &&
         (queryPtr[2] == 'l'  ||  queryPtr[2] == 'L')  &&
         (queryPtr[3] == 'u'  ||  queryPtr[3] == 'U')  &&
         (queryPtr[4] == 'e'  ||  queryPtr[4] == 'E')  &&
         (queryPtr[5] == 's'  ||  queryPtr[5] == 'S')  &&
         (queryPtr+6 == queryEnd  || !isalnum_(queryPtr[6]))) {
            stmt->queryPtr = queryPtr + 6;
            return VALUES;
         }
         break;
      case 'w':
      case 'W':
         if (queryPtr+5 <= queryEnd  &&
         (queryPtr[1] == 'h'  ||  queryPtr[1] == 'H')  &&
         (queryPtr[2] == 'e'  ||  queryPtr[2] == 'E')  &&
         (queryPtr[3] == 'r'  ||  queryPtr[3] == 'R')  &&
         (queryPtr[4] == 'e'  ||  queryPtr[4] == 'E')  &&
         (queryPtr+5 == queryEnd  || !isalnum_(queryPtr[5]))) {
            stmt->queryPtr = queryPtr + 5;
            return WHERE;
         }
         break;
      }

      while (queryPtr < queryEnd  &&  isalnum_(*queryPtr)) {
        ++queryPtr;
      }

      lvalp->item_val = hb_itemNew( NULL );
      hb_itemPutCL( lvalp->item_val, stmt->queryPtr, ( queryPtr - stmt->queryPtr ) );

      stmt->queryPtr = queryPtr;
      return IDENT;
   }
   return *stmt->queryPtr++;
}

int sql_yyerror(void * stmt,const char* msg)
{
   #ifdef YYDEBUG
      printf("Parse Error %p  %s\n",stmt, msg);
   #else
      HB_SYMBOL_UNUSED( msg );
      HB_SYMBOL_UNUSED( stmt );
   #endif

   return 1;
}
