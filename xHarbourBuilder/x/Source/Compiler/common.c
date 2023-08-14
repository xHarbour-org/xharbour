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

const char * ClipNet_MacroKind( const MACRO *pMacro )
{
   switch( pMacro->Kind )
   {
      CASE_STRING( MACRO_KIND_COMPLEX )
         break;
      CASE_STRING( MACRO_KIND_SIMPLE )
         break;
         
      default:
         printf( "internal error - unexpected case in: " __SOURCE__ "\n" );
   }
   
   return __SOURCE__;
}

const char * ClipNet_DeclaredKind( const DECLARED *pDeclared )
{
   switch( pDeclared->Kind )
   {
      CASE_STRING( DECLARED_KIND_NONE )
         break;
      CASE_STRING( DECLARED_KIND_FIELD )
         break;
      CASE_STRING( DECLARED_KIND_GLOBAL )
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
         printf( "internal error - unexpected case in: " __SOURCE__ "\n" );
   }
   
   return __SOURCE__;
}

const char * ClipNet_LValueKind( const VALUE *pLValue)
{
   switch( pLValue->Kind & ~( VALUE_KIND_ERROR_MASK | VALUE_KIND_ASSIGNED_MASK ) )
   {
      CASE_STRING( VALUE_KIND_VARIABLE )
         break;
      CASE_STRING( VALUE_KIND_MACRO )
         break;
      CASE_STRING( VALUE_KIND_ARRAY_ELEMENT )
         break;
      CASE_STRING( VALUE_KIND_OBJECT_PROPERTY )
         break;
      CASE_STRING( VALUE_KIND_ALIASED_FIELD )
         break;
         
      default:
         printf( "internal error - unexpected case in: " __SOURCE__ "\n" );
   }
   
   return __SOURCE__;
}

const char * ClipNet_ValueKind( const VALUE *pValue )
{
   switch( pValue->Kind & ~( VALUE_KIND_ERROR_MASK | VALUE_KIND_ASSIGNED_MASK ) )
   {
      CASE_STRING( VALUE_KIND_CONSTANT )
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
         
      CASE_STRING( VALUE_KIND_FUNCTION_CALL )
         break;
         
      CASE_STRING( VALUE_KIND_IIF )
         break;
         
      CASE_STRING( VALUE_KIND_METHOD_CALL )
         break;
         
      CASE_STRING( VALUE_KIND_LIST )
         break;
         
      CASE_STRING( VALUE_KIND_BYREF )
         break;
         
      CASE_STRING(VALUE_KIND_VARIABLE)
         break;
         
      CASE_STRING(VALUE_KIND_MACRO )
         break;
         
      CASE_STRING(VALUE_KIND_ARRAY_ELEMENT )
         break;
         
      CASE_STRING(VALUE_KIND_ALIASED_FIELD )
         break;
         
      CASE_STRING(VALUE_KIND_OBJECT_PROPERTY )
         break;
         
      CASE_STRING(VALUE_KIND_FIELD )
         break;
           
      CASE_STRING(VALUE_KIND_MEMVAR )
         break;
           
      default:
         printf( "internal error - unexpected case in: " __SOURCE__ "\n" );
   }
   
   return __SOURCE__;
}

const char * ClipNet_LineKind( const LINE *pLine )
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

         
      CASE_STRING( LINE_KIND_FUNCTION_CALL );
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
         printf( "internal error - unexpected case in: " __SOURCE__ "\n" );
   }

   return __SOURCE__;
}

const char * ClipNet_BinaryKind( const BINARY *pBinary )
{
   switch( pBinary->Kind )
   {
      CASE_STRING( BINARY_KIND_AND );
         break;

      CASE_STRING( BINARY_KIND_BITAND );
         break;
         
      CASE_STRING( BINARY_KIND_BITLEFT );
         break;
         
      CASE_STRING( BINARY_KIND_BITOR );
         break;
         
      CASE_STRING( BINARY_KIND_BITRIGHT );
         break;
         
      CASE_STRING( BINARY_KIND_BITXOR );
         break;
         
      CASE_STRING( BINARY_KIND_DIVIDE );
         break;
         
      CASE_STRING( BINARY_KIND_EQUAL );
         break;
         
      CASE_STRING( BINARY_KIND_EXACTEQUAL );
         break;
         
      CASE_STRING( BINARY_KIND_GREATER );
         break;
         
      CASE_STRING( BINARY_KIND_GREATEREQUAL );
         break;
         
      CASE_STRING( BINARY_KIND_HAS );
         break;
         
      CASE_STRING( BINARY_KIND_IN );
         break;
         
      CASE_STRING( BINARY_KIND_LESSER );
         break;
         
      CASE_STRING( BINARY_KIND_LESSEREQUAL );
         break;
         
      CASE_STRING( BINARY_KIND_LIKE );
         break;
         
      CASE_STRING( BINARY_KIND_MINUS );
         break;
         
      CASE_STRING( BINARY_KIND_MODULUS );
         break;
         
      CASE_STRING( BINARY_KIND_MULTIPLY );
         break;
         
      CASE_STRING( BINARY_KIND_NOTEQUAL);
         break;
         
      CASE_STRING( BINARY_KIND_OR );
         break;
         
      CASE_STRING( BINARY_KIND_PLUS );
         break;
         
      CASE_STRING( BINARY_KIND_POWER );
         break;
         
      default:
         printf( "internal error - unexpected case in: " __SOURCE__ "\n" );
   }
   
   return __SOURCE__;
}

const char * ClipNet_UnaryKind( const UNARY *pUnary )
{
   switch( pUnary->Kind )
   {
         CASE_STRING( UNARY_KIND_DEC );
         break;
         
         CASE_STRING( UNARY_KIND_INC );
         break;
         
      default:
         printf( "internal error - unexpected case in: " __SOURCE__ "\n" );
   }
   
   return __SOURCE__;
}
