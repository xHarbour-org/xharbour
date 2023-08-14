/*
 * ClipNet Project source code:
 * Compiler Error Services.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#include "common.h"

char *Parser_asErrors[] =
{
   "Statement not allowed outside of procedure or function",
   "Redefinition of procedure or function: \'%s\'",
   "Duplicate variable declaration: \'%s\'",
   "%s declaration follows executable statement",
   "Outer codeblock variable is out of reach: \'%s\'",
   "Invalid numeric format '.'",
   "Unterminated string: \'%s\'",
   "Redefinition of predefined function %s: \'%s\'",
   "Illegal variable \'%s\' initializer: \'%s\'",
   "ENDIF does not match IF",
   "ENDDO does not match WHILE",
   "ENDCASE does not match DO CASE",
   "NEXT does not match FOR",
   "ELSE does not match IF",
   "ELSEIF does not match IF",
   "Syntax error: \'%s\' %s",
   "Unclosed control structures",
   "%s statement with no loop in sight",
   "Syntax error: \'%s\' in: \'%s\'",
   "Incomplete statement: %s",
   "Incorrect number of arguments: %s %s",
   "Invalid lvalue: \'%s\' %s",
   "Invalid use of \'@\' (pass by reference): \'%s\'",
   "Formal parameters already declared",
   "Invalid %s from within of SEQUENCE code",
   "Unterminated array index",
   "Could not allocate %s byte(s)",
   "Could not reallocate %s byte(s)",
   "Freeing a NULL memory pointer",
   "Can't create output file: \'%s\'",
   "Can't create preprocessed output file: \'%s\'",
   "Bad command line option: \'%s\'",
   "Bad command line parameter: \'%s\'",
   "Invalid filename: \'%s\'",
   "Mayhem in CASE handler",
   "Operation not supported for this data type: \'%s\'",
   "Invalid alias expression: \'%s\'",
   "Invalid array index expression: \'%s\'",
   "Bound error: \'%s\'",
   "Macro of declared symbol: \'%s\'",
   "Invalid selector in send: \'%s\'",
   "ANNOUNCEd procedure \'%s\' must be a public symbol",
   "Jump PCode not found",
   "CASE or OTHERWISE does not match DO CASE",
   "Code block contains both macro and declared symbol references",
   "GET contains complex macro",
   "Unterminated inline block in function: \'%s\'",
   "Too many inline blocks %s",
   "Inline C requires C output generation, use -gc[n]"
};

char *Parser_asWarnings[] =
{
   "1Ambiguous reference: \'%s\'",
   "1Ambiguous reference, assuming memvar: \'%s\'",
   "2Variable: \'%s\' declared but not used in function: \'%s\'",
   "2Codeblock parameter: \'%s\' declared but not used in function: \'%s\'",
   "1RETURN statement with no return value in function",
   "1Procedure returns value",
   "1Function \'%s\' does not end with RETURN statement",
   "3Incompatible type in assignment to: \'%s\' expected: \'%s\'",
   "3Incompatible operand type: \'%s\' expected: \'%s\'",
   "4Suspicious operand type: \'unknown\' expected: \'%s\'",
   "3Incompatible operand types: \'%s\' and: \'%s\'",
   "4Suspicious type in assignment to: \'%s\' expected: \'%s\'",
   "3Can\'t use array index with non-array",
   "3Incompatible return type: \'%s\' expected: \'%s\'",
   "4Suspicious return type: \'%s\' expected: \'%s\'",
   "3Invalid number of parameters: %s expected: %s",
   "3Incompatible parameter: %s expected: \'%s\'",
   "4Suspicious parameter: %s expected: \'%s\'",
   "3Duplicate declaration of %s \'%s\'",
   "3Function \'%s\' conflicting with its declaration",
   "3Variable \'%s\' used but never initialized",
   "3Value of Variable \'%s\' never used",
   "3Incompatible type in assignment to declared array element expected: \'%s\'",
   "4Suspicious type in assignment to declared array element expected: \'%s\'",
   "3Class \'%s\' not known in declaration of \'%s\'",
   "3Message \'%s\' not known in class \'%s\'",
   "0Meaningless use of expression: \'%s\'",
   "2Unreachable code",
   "1Redundant \'ANNOUNCE %s\' statement ignored"
};

void Parser_GenError( char *asErrors[], char cPrefix, PARSER_ERROR iError, const char * sError1, const char *sError2, PARSER_CONTEXT *Parser_pContext )
{
   int iLine = Parser_pContext->iLine;

   //if( cPrefix != 'F' && Parser_bError )
   //   return;

   printf( "\r" );

   if( Parser_pContext->Files.pLast && Parser_pContext->Files.pLast->sName )
   {
      printf( "%s(%i) ", Parser_pContext->Files.pLast->sName, iLine );
   }

   if( sError1 && sError1[0] ==  '\n' ) 
   {
      sError1 = "EOL"; 
   } 

   printf( "Error %c%04i  ", cPrefix, iError );
   printf( asErrors[ iError ], sError1, sError2 );
   printf( "\n" );

   Parser_pContext->iErrors++;

   if( cPrefix == 'F' )
   { 
      exit( PARSER_FAILURE );
   } 
}

void Parser_GenWarning( char *asWarnings[], char cPrefix, int iWarning, const char *sWarning1, const char *sWarning2, PARSER_CONTEXT *Parser_pContext )
{
   char *sWarning = asWarnings[ iWarning - 1 ];
   int iLine = Parser_pContext->iLine;

   if( ( sWarning[ 0 ] - '0' ) <= Parser_pContext->iWarnings )
   {
      if( Parser_pContext->Files.pLast && Parser_pContext->Files.pLast->sName )
      { 
         printf( "\r%s(%i) ", Parser_pContext->Files.pLast->sName, iLine );
      } 

      printf( "Warning %c%04i  ", cPrefix, iWarning );
      printf( sWarning + 1, sWarning1, sWarning2 );
      printf( "\n" );

      Parser_pContext->bAnyWarning = TRUE;    /* report warnings at exit */
   }
}

void RaiseParseError( PARSER_ERROR iError, PARSER_CONTEXT *Parser_pContext )
{
   longjmp( Parser_pContext->JumpBuffer, iError );
}