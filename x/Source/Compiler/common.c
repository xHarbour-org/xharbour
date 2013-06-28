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
   unsigned long  ulLen = strlen( pString ) + 1;

   pCopy = ( char * ) ClipNet_alloc( ulLen );
   memcpy( pCopy, pString, ulLen );

   return pCopy;
}

const char * ClipNet_DeclaredKind( const DECLARED *pDeclared, PARSER_CONTEXT *Parser_pContext )
{
   switch( pDeclared->Kind )
   {
      CASE_STRING( DECLARED_KIND_NONE )
         break;
      CASE_STRING( DECLARED_KIND_FIELD )
         break;
      CASE_STRING( DECLARED_KIND_LOCAL )
         break;
      CASE_STRING( DECLARED_KIND_LOCAL_PARAMETER )
         break;
      CASE_STRING( DECLARED_KIND_MEMVAR )
         break;
      CASE_STRING( DECLARED_KIND_PARAMETER )
         break;
      CASE_STRING( DECLARED_KIND_PRIVATE )
         break;
      CASE_STRING( DECLARED_KIND_PUBLIC )
         break;
      CASE_STRING( DECLARED_KIND_STATIC )
         break;
         
      default:
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in: ClipNet_LValueKind()" );
   }
   
   return __SOURCE__;
}

const char * ClipNet_LValueKind( const LVALUE *pLValue, PARSER_CONTEXT *Parser_pContext )
{
   switch( pLValue->Kind )
   {
      CASE_STRING( LVALUE_KIND_VARIABLE )
         break;
      CASE_STRING( LVALUE_KIND_MACRO )
         break;
      CASE_STRING( LVALUE_KIND_ARRAY_ELEMENT )
         break;
      CASE_STRING( LVALUE_KIND_OBJ_PROPERTY )
         break;
      CASE_STRING( LVALUE_KIND_ALIASED_FIELD )
         break;
         
      default:
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in: ClipNet_LValueKind()" );
   }
   
   return __SOURCE__;
}

const char * ClipNet_ValueKind( const VALUE *pValue, PARSER_CONTEXT *Parser_pContext )
{
   switch( pValue->Kind )
   {
      CASE_STRING( VALUE_KIND_NIL )
         break;
         
      CASE_STRING( VALUE_KIND_CONSTANT )
         break;
         
      CASE_STRING( VALUE_KIND_LVALUE )
         break;
         
      CASE_STRING( VALUE_KIND_ARRAY )
         break;
         
      CASE_STRING( VALUE_KIND_BLOCK )
         break;
         
      CASE_STRING( VALUE_KIND_UNARY )
         break;
         
      CASE_STRING( VALUE_KIND_BINARY )
         break;
         
      CASE_STRING( VALUE_KIND_ALIASED )
         break;
         
      CASE_STRING( VALUE_KIND_ASSIGNMENT )
         break;
         
      CASE_STRING( VALUE_KIND_FUNC_CALL )
         break;
         
      CASE_STRING( VALUE_KIND_IIF )
         break;
         
      CASE_STRING( VALUE_KIND_METHOD_CALL )
         break;
         
      CASE_STRING( VALUE_KIND_LIST )
         break;
         
      CASE_STRING( VALUE_KIND_BYREF )
         break;
         
      default:
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in: ClipNet_ValueKind()" );
   }
   
   return __SOURCE__;
}

const char * ClipNet_LineKind( const LINE *pLine, PARSER_CONTEXT *Parser_pContext )
{
   switch( pLine->Kind )
   {
      CASE_STRING( LINE_KIND_ASSIGNMENT );
         break;

         
      CASE_STRING( LINE_KIND_CASE );
         break;
         
      CASE_STRING( LINE_KIND_OTHERWISE );
         break;

         
      CASE_STRING( LINE_KIND_FOR );
         break;
         
      CASE_STRING( LINE_KIND_WHILE );
         break;
         
      CASE_STRING( LINE_KIND_FLOW );
         break;

         
      CASE_STRING( LINE_KIND_FUNC_CALL );
         break;
         
      CASE_STRING( LINE_KIND_IIF );
         break;
         
      CASE_STRING( LINE_KIND_METHOD_CALL );
         break;
         
         
      CASE_STRING( LINE_KIND_IF );
         break;
         
      CASE_STRING( LINE_KIND_ELSEIF );
         break;
         
      CASE_STRING( LINE_KIND_ELSE );
         break;
 
         
      CASE_STRING( LINE_KIND_PARAMETERS );
         break;
         
      CASE_STRING( LINE_KIND_PRIVATES );
         break;
         
      CASE_STRING( LINE_KIND_PUBLICS );
         break;

         
      CASE_STRING( LINE_KIND_RETURN );
         break;
         
         
      CASE_STRING( LINE_KIND_SEQUENCE );
         break;
         
      CASE_STRING( LINE_KIND_RECOVER );
         break;
         
         
      CASE_STRING( LINE_KIND_SWITCH );
         break;
         
      CASE_STRING( LINE_KIND_SWITCHCASE );
         break;
         
      CASE_STRING( LINE_KIND_SWITCHDEAFULT );
         break;
  
         
      CASE_STRING( LINE_KIND_TRY );
         break;
         
      CASE_STRING( LINE_KIND_CATCH );
         break;
         
      CASE_STRING( LINE_KIND_FINALLY );
         break;
         
         
      CASE_STRING( LINE_KIND_UNARY ); 
         break;

      CASE_STRING( LINE_KIND_LIST );
         break;

       default:
        PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in: ClipNet_LineKind()" );
   } 

   return __SOURCE__;
}