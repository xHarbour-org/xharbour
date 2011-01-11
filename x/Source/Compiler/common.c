/*
 * ClipNet Project source code:
 * Common support code.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#include "common.h"

char * ClipNet_strdup( const char *pString )
{
   char *pCopy;
   int  iLen = strlen( pString ) + 1;

   pCopy = ( char * ) ClipNet_alloc( iLen );
   memcpy( pCopy, pString, iLen );

   return pCopy;
}

char * ClipNet_LineKind( LINE_KIND Kind )
{
   switch( Kind ) 
   {
      CASE_STRING( LINE_KIND_ASSIGNMENT ); 
         break; 

      CASE_STRING( LINE_KIND_UNARY ); 
         break;

      CASE_STRING( LINE_KIND_FUNC_CALL ); 
         break;

      CASE_STRING( LINE_KIND_IIF ); 
         break;

      CASE_STRING( LINE_KIND_METHOD_CALL ); 
         break;

      CASE_STRING( LINE_KIND_PRIVATES ); 
         break;

      CASE_STRING( LINE_KIND_PUBLICS ); 
         break;

      CASE_STRING( LINE_KIND_PARAMETERS ); 
         break;

      CASE_STRING( LINE_KIND_IF ); 
         break;

      CASE_STRING( LINE_KIND_ELSEIF ); 
         break;

      CASE_STRING( LINE_KIND_ELSE ); 
         break;

      CASE_STRING( LINE_KIND_FOR ); 
         break;

      CASE_STRING( LINE_KIND_WHILE ); 
         break;

      CASE_STRING( LINE_KIND_CASE ); 
         break;

      CASE_STRING( LINE_KIND_OTHERWISE ); 
         break;

      CASE_STRING( LINE_KIND_SEQUENCE ); 
         break;

      CASE_STRING( LINE_KIND_RECOVER ); 
         break;

      CASE_STRING( LINE_KIND_FLOW ); 
         break;

      CASE_STRING( LINE_KIND_RETURN ); 
         break;

       default:
        PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in: ClipNet_LineKind()" );
   } 

   return "internal error!!!";
}