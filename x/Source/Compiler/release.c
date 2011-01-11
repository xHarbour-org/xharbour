#include "common.h"

void Release_LValue( LVALUE *pLValue )
{
   ClipNet_free( (void *) pLValue );
}

void Release_List( LIST *pList )
{
   // TODO!!!
   ClipNet_free( (void *) pList );
}

void Release_Block( BLOCK *pBlock )
{
   ClipNet_free( (void *) pBlock );
}

void Release_Unary( UNARY *pUnary )
{
   ClipNet_free( (void *) pUnary );
}

void Release_Binnary( BINARY *pBinary )
{
   ClipNet_free( (void *) pBinary );
}

void Release_Alias( ALIASED *pAliased )
{
   ClipNet_free( (void *) pAliased );
}

void Release_Assignment( ASSIGNMENT *pAssignment )
{
   ClipNet_free( (void *) pAssignment );
}

void Release_FuncCall( FUNC_CALL *pFuncCall )
{
   ClipNet_free( (void *) pFuncCall );
}

void Release_IIF( IIF *pIIF )
{
   ClipNet_free( (void *) pIIF );
}

void Release_MethodCall( METHOD_CALL *pMethodCall )
{
   ClipNet_free( (void *) pMethodCall );
}

void Release_Value( VALUE *pValue )
{
   switch( pValue->Kind )
   {
      case VALUE_KIND_NIL : 
         break;

      case VALUE_KIND_CONSTANT : 
         ClipNet_free( (void *) pValue->Value.pConstant );
         break;
 
      case VALUE_KIND_LVALUE : 
         Release_LValue( pValue->Value.pLValue );
         break;

      case VALUE_KIND_ARRAY : 
         Release_List( pValue->Value.pArray );
         break;

      case VALUE_KIND_BLOCK : 
         Release_Block( pValue->Value.pBlock );
         break;
 
      case VALUE_KIND_UNARY : 
         Release_Unary( pValue->Value.pUnary );
         break;
 
      case VALUE_KIND_BINARY : 
         Release_Binnary( pValue->Value.pBinary );
         break;
 
      case VALUE_KIND_ALIASED : 
         Release_Alias( pValue->Value.pAliased );
         break;
 
      case VALUE_KIND_ASSIGNMENT : 
         Release_Assignment( pValue->Value.pAssignment );
         break;
 
      case VALUE_KIND_FUNC_CALL : 
         Release_FuncCall( pValue->Value.pFuncCall );
         break;
 
      case VALUE_KIND_IIF : 
         Release_IIF( pValue->Value.pIIF );
         break;
  
      case VALUE_KIND_METHOD_CALL : 
         Release_MethodCall( pValue->Value.pMethodCall );
         break;

      case VALUE_KIND_LIST : 
         Release_List( pValue->Value.pList );
         break;
   }

   ClipNet_free( (void *) pValue );
}