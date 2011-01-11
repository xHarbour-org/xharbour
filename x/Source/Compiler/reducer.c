#include "common.h"

VALUE * ( *ReduceValue[] )( VALUE * ) =
   {
      Reduce_NIL,           //VALUE_KIND_NIL,
      Reduce_Constant,      //VALUE_KIND_CONSTANT,
      Reduce_LValue,        //VALUE_KIND_LVALUE,
      Reduce_Array,         //VALUE_KIND_ARRAY,
      Reduce_Block,         //VALUE_KIND_BLOCK,
      Reduce_Unary,         //VALUE_KIND_UNARY,
      Reduce_Binary,        //VALUE_KIND_BINARY,
      Reduce_Aliased,       //VALUE_KIND_ALIASED,
      Reduce_Assignment,    //VALUE_KIND_ASSIGNMENT,
      Reduce_FuncCall,      //VALUE_KIND_FUNC_CALL,
      Reduce_IIF,           //VALUE_KIND_IIF,
      Reduce_MethodCall,    //VALUE_KIND_METHOD_CALL,
      Reduce_List           //VALUE_KIND_LIST
   };

int BinaryPrecedence[ TOKEN_MAX_TOKENS ];

void Reducer_Init( void )
{

   BinaryPrecedence[ BINARY_KIND_POWER ]        = 7;

   BinaryPrecedence[ BINARY_KIND_MODULUS ]      = 6;
   BinaryPrecedence[ BINARY_KIND_MULTIPLY ]     = 6;
   BinaryPrecedence[ BINARY_KIND_DIVIDE ]       = 6;

   BinaryPrecedence[ BINARY_KIND_PLUS ]         = 5;
   BinaryPrecedence[ BINARY_KIND_MINUS ]        = 5;

   BinaryPrecedence[ BINARY_KIND_BITLEFT ]      = 4;
   BinaryPrecedence[ BINARY_KIND_BITRIGHT ]     = 4;

   BinaryPrecedence[ BINARY_KIND_IN ]           = 4;
   BinaryPrecedence[ BINARY_KIND_LIKE ]         = 4;
   BinaryPrecedence[ BINARY_KIND_HAS ]          = 4;
   BinaryPrecedence[ BINARY_KIND_GREATEREQUAL ] = 4;
   BinaryPrecedence[ BINARY_KIND_LESSEREQUAL ]  = 4;
   BinaryPrecedence[ BINARY_KIND_GREATER ]      = 4;
   BinaryPrecedence[ BINARY_KIND_LESSER ]       = 4;

   BinaryPrecedence[ BINARY_KIND_EXACTEQUAL ]   = 3;
   BinaryPrecedence[ BINARY_KIND_EQUAL ]        = 3;
   BinaryPrecedence[ BINARY_KIND_NOTEQUAL ]     = 3;

   BinaryPrecedence[ BINARY_KIND_BITOR ]        = 2;
   BinaryPrecedence[ BINARY_KIND_BITXOR ]       = 2;
   BinaryPrecedence[ BINARY_KIND_BITAND ]       = 2;

   BinaryPrecedence[ BINARY_KIND_AND ]          = 1;
   BinaryPrecedence[ BINARY_KIND_OR ]           = 1;
}

VALUE * Reduce_NIL( VALUE * pValue )
{
   return pValue;
}

VALUE * Reduce_Constant( VALUE * pValue )
{
   return pValue;
}

VALUE * Reduce_LValue( VALUE * pValue )
{
   switch( pValue->Value.pLValue->Kind )
   {
      case LVALUE_KIND_ID :
      case LVALUE_KIND_MEMVAR :
          return pValue;

      case LVALUE_KIND_MACRO :
         switch( pValue->Value.pLValue->Value.pMacro->Kind )
         {
            case MACRO_KIND_SIMPLE :
               return pValue;

            case MACRO_KIND_COMPLEX :
               return ReduceValue[ pValue->Value.pLValue->Value.pMacro->Value.pComplex->Kind ]( pValue->Value.pLValue->Value.pMacro->Value.pComplex );

            default :
               PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in Reduce_LValue( LVALUE_KIND_MACRO )." );
         }
         break;

      case LVALUE_KIND_ARRAY_ELEMENT :
        break;

      case LVALUE_KIND_OBJ_PROPERTY :
        break;

      case LVALUE_KIND_ALIASED_FIELD :
        break;

      default :
         printf( "Kind: %i\n", pValue->Value.pLValue->Kind );
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in Reduce_LValue( LVALUE_KIND )." );
   }

   return pValue;
}

VALUE * Reduce_Array( VALUE * pValue )
{
   LIST_NODE *pNode = pValue->Value.pArray->pFirst;
   VALUE * pElement;

   while( pNode )
   {
      pElement = pNode->pValue;

      if( pElement )
      {
         pNode->pValue = ReduceValue[ pElement->Kind ]( pElement );
      }

      pNode = pNode->pNext;
   }

   return pValue;
}

VALUE * Reduce_Block( VALUE * pValue )
{
   LIST_NODE *pNode = pValue->Value.pBlock->pList->Value.pList->pFirst;
   VALUE * pElement;

   while( pNode )
   {
      pElement = pNode->pValue;

      if( pElement )
      {
         pNode->pValue = ReduceValue[ pElement->Kind ]( pElement );
      }

      pNode = pNode->pNext;
   }

   return pValue;
}

VALUE * Reduce_Unary( VALUE * pValue )
{
   if( pValue->Value.pUnary->pLValue )
   {
      pValue->Value.pUnary->pLValue = ReduceValue[ pValue->Value.pUnary->pLValue->Kind ]( pValue->Value.pUnary->pLValue );
   }

   return pValue;
}

VALUE * Reduce_Binary( VALUE * pValue )
{
   if( pValue->Value.pBinary->pLeft && pValue->Value.pBinary->pRight )
   {
      pValue->Value.pBinary->pLeft = ReduceValue[ pValue->Value.pBinary->pLeft->Kind ]( pValue->Value.pBinary->pLeft );
      pValue->Value.pBinary->pRight = ReduceValue[ pValue->Value.pBinary->pRight->Kind ]( pValue->Value.pBinary->pRight );
   }
   else
   {
      return pValue;
   }

   if( pValue->Value.pBinary->pLeft->Kind == VALUE_KIND_CONSTANT && pValue->Value.pBinary->pRight->Kind == VALUE_KIND_CONSTANT )
   {
      printf( "TODO: REDUCE Binary constants\n" );
   }

   return pValue;
}

VALUE * Reduce_Aliased( VALUE * pValue )
{
   if( pValue->Value.pAliased->pArea && pValue->Value.pAliased->pValue )
   {
      pValue->Value.pAliased->pArea = ReduceValue[ pValue->Value.pAliased->pArea->Kind ]( pValue->Value.pAliased->pArea );
      pValue->Value.pAliased->pValue = ReduceValue[ pValue->Value.pAliased->pValue->Kind ]( pValue->Value.pAliased->pValue );
   }

   return pValue;
}

VALUE * Reduce_Assignment( VALUE * pValue )
{
   if( pValue->Value.pAssignment->pLValue && pValue->Value.pAssignment->pValue )
   {
      // LValue may be macro
      pValue->Value.pAssignment->pLValue = ReduceValue[ pValue->Value.pAssignment->pLValue->Kind ]( pValue->Value.pAssignment->pLValue );
      pValue->Value.pAssignment->pValue = ReduceValue[ pValue->Value.pAssignment->pValue->Kind ]( pValue->Value.pAssignment->pValue );
   }

   return pValue;
}

VALUE * Reduce_FuncCall( VALUE * pValue )
{
   if( pValue->Value.pFuncCall->pSymbol )
   {
      pValue->Value.pFuncCall->pSymbol = ReduceValue[ pValue->Value.pFuncCall->pSymbol->Kind ]( pValue->Value.pFuncCall->pSymbol );

      if( pValue->Value.pFuncCall->pArguments )
      {
         pValue->Value.pFuncCall->pArguments = ReduceValue[ pValue->Value.pFuncCall->pArguments->Kind ]( pValue->Value.pFuncCall->pArguments );
      }
   }

   return pValue;
}

VALUE * Reduce_IIF( VALUE * pValue )
{
   if( pValue->Value.pIIF->pCond && pValue->Value.pIIF->pTrue && pValue->Value.pIIF->pFalse )
   {
      pValue->Value.pIIF->pCond = ReduceValue[ pValue->Value.pIIF->pCond->Kind ]( pValue->Value.pIIF->pCond );
      pValue->Value.pIIF->pTrue = ReduceValue[ pValue->Value.pIIF->pTrue->Kind ]( pValue->Value.pIIF->pTrue );
      pValue->Value.pIIF->pFalse = ReduceValue[ pValue->Value.pIIF->pFalse->Kind ]( pValue->Value.pIIF->pFalse );
   
      if( pValue->Value.pIIF->pCond->Kind == VALUE_KIND_CONSTANT )
      {
         printf( "TODO: REDUCE IIF()\n" );
      }
   }

   return pValue;
}

VALUE * Reduce_MethodCall( VALUE * pValue )
{
   if( pValue->Value.pMethodCall->pObject && pValue->Value.pMethodCall->pMethod )
   {
      pValue->Value.pMethodCall->pObject = ReduceValue[ pValue->Value.pMethodCall->pObject->Kind ]( pValue->Value.pMethodCall->pObject );
      pValue->Value.pMethodCall->pMethod = ReduceValue[ pValue->Value.pMethodCall->pMethod->Kind ]( pValue->Value.pMethodCall->pMethod );
   }

   return pValue;
}

VALUE * Reduce_List( VALUE * pValue )
{
   LIST_NODE *pNode = pValue->Value.pArray->pFirst;
   VALUE * pElement;

   while( pNode )
   {
      pElement = pNode->pValue;

      if( pElement )
      {
         pNode->pValue = ReduceValue[ pElement->Kind ]( pElement );
      }

      pNode = pNode->pNext;
   }

   return pValue;
}

VALUE * LeftLeaf( VALUE * pValue )
{
   switch( pValue->Kind )
   {
      case VALUE_KIND_CONSTANT : 
      case VALUE_KIND_LVALUE :
      case VALUE_KIND_ARRAY : 
      case VALUE_KIND_BLOCK : 
      case VALUE_KIND_UNARY : 
         return pValue;

      case VALUE_KIND_BINARY : 
         return LeftLeaf( pValue->Value.pBinary->pLeft );
 
      case VALUE_KIND_ALIASED : 
         return LeftLeaf( pValue->Value.pAliased->pArea );
         break;
 
      case VALUE_KIND_ASSIGNMENT : 
         return pValue->Value.pAssignment->pLValue;
         break;
 
      case VALUE_KIND_FUNC_CALL : 
      case VALUE_KIND_IIF : 
      case VALUE_KIND_METHOD_CALL : 
         return pValue;

      case VALUE_KIND_LIST : 
         return LeftLeaf( pValue->Value.pList->pLast->pValue );

      default :
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in LeftLeaf(2)." );
   }

   return NULL;
}

VALUE * Get_LValue( VALUE * pValue )
{
  Get_LValue_Top :

   switch( pValue->Kind )
   {
      case VALUE_KIND_CONSTANT :
         return NULL;

      case VALUE_KIND_LVALUE :
         return pValue;

      case VALUE_KIND_ARRAY : 
      case VALUE_KIND_BLOCK : 
         return NULL;

      case VALUE_KIND_UNARY : 
         pValue = LeftLeaf( pValue->Value.pUnary->pLValue );
         goto Get_LValue_Top;

      case VALUE_KIND_BINARY : 
         pValue = LeftLeaf( pValue->Value.pBinary->pLeft );
         goto Get_LValue_Top;
 
      case VALUE_KIND_ALIASED : 
         return NULL;
         break;
 
      case VALUE_KIND_ASSIGNMENT : 
         pValue = pValue->Value.pAssignment->pLValue;
         goto Get_LValue_Top;
         break;
 
      case VALUE_KIND_FUNC_CALL : 
      case VALUE_KIND_IIF : 
      case VALUE_KIND_METHOD_CALL : 
         return NULL;

      case VALUE_KIND_LIST : 
         return NULL;

      default :
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in LeftLeaf(2)." );
   }

   return NULL;
}