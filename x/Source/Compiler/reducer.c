#include "common.h"

static VALUE * ( *ReduceValue[] )( VALUE *, PARSER_CONTEXT * ) =
   {
      Reduce_NILValue,           //VALUE_KIND_NIL,
      Reduce_ConstantValue,      //VALUE_KIND_CONSTANT,
      Reduce_LValueValue,        //VALUE_KIND_LVALUE,
      Reduce_ArrayValue,         //VALUE_KIND_ARRAY,
      Reduce_BlockValue,         //VALUE_KIND_BLOCK,
      Reduce_UnaryValue,         //VALUE_KIND_UNARY,
      Reduce_BinaryValue,        //VALUE_KIND_BINARY,
      Reduce_AliasedValue,       //VALUE_KIND_ALIASED,
      Reduce_AssignmentValue,    //VALUE_KIND_ASSIGNMENT,
      Reduce_FuncCallValue,      //VALUE_KIND_FUNC_CALL,
      Reduce_IIFValue,           //VALUE_KIND_IIF,
      Reduce_MethodCallValue,    //VALUE_KIND_METHOD_CALL,
      Reduce_ListValue           //VALUE_KIND_LIST
   };

int BinaryPrecedence[ TOKEN_MAX_TOKENS ] = {1};

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

VALUE * Reduce_NILValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   return pValue;
}

VALUE * Reduce_ConstantValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   return pValue;
}

VALUE * Reduce_LValueValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   switch( pValue->Value.pLValue->Kind )
   {
      case LVALUE_KIND_VARIABLE :
          return pValue;

      case LVALUE_KIND_MACRO :
         switch( pValue->Value.pLValue->Value.pMacro->Kind )
         {
            case MACRO_KIND_SIMPLE :
               return pValue;

            case MACRO_KIND_COMPLEX :
               return ReduceValue[ pValue->Value.pLValue->Value.pMacro->Value.pComplex->Kind ]( pValue->Value.pLValue->Value.pMacro->Value.pComplex, Parser_pContext );

            default :
               PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in Reduce_LValue( LVALUE_KIND_MACRO )." );
         }
         break;

      case LVALUE_KIND_ARRAY_ELEMENT :
         pValue->Value.pLValue->Value.pArrayElement->pArray = ReduceValue[ pValue->Value.pLValue->Value.pArrayElement->pArray->Kind ](pValue->Value.pLValue->Value.pArrayElement->pArray, Parser_pContext );
         pValue->Value.pLValue->Value.pArrayElement->pIndexList = Reduce_List( pValue->Value.pLValue->Value.pArrayElement->pIndexList , Parser_pContext );
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

VALUE * Reduce_ArrayValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   pValue->Value.pArray = Reduce_List( pValue->Value.pArray, Parser_pContext );

   return pValue;
}

VALUE * Reduce_BlockValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   pValue->Value.pBlock->pList = Reduce_List( pValue->Value.pArray, Parser_pContext );
   return pValue;
}

VALUE * Reduce_UnaryValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pUnary->pLValue )
   {
      pValue->Value.pUnary->pLValue = ReduceValue[ pValue->Value.pUnary->pLValue->Kind ]( pValue->Value.pUnary->pLValue, Parser_pContext );
   }

   return pValue;
}

VALUE * Reduce_BinaryValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pBinary->pLeft && pValue->Value.pBinary->pRight )
   {
      pValue->Value.pBinary->pLeft = ReduceValue[ pValue->Value.pBinary->pLeft->Kind ]( pValue->Value.pBinary->pLeft, Parser_pContext );
      pValue->Value.pBinary->pRight = ReduceValue[ pValue->Value.pBinary->pRight->Kind ]( pValue->Value.pBinary->pRight, Parser_pContext );
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

VALUE * Reduce_AliasedValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pAliased->pArea && pValue->Value.pAliased->pValue )
   {
      pValue->Value.pAliased->pArea = ReduceValue[ pValue->Value.pAliased->pArea->Kind ]( pValue->Value.pAliased->pArea, Parser_pContext );
      pValue->Value.pAliased->pValue = ReduceValue[ pValue->Value.pAliased->pValue->Kind ]( pValue->Value.pAliased->pValue, Parser_pContext );
   }

   return pValue;
}

VALUE * Reduce_AssignmentValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pAssignment->pLValue && pValue->Value.pAssignment->pValue )
   {
      // LValue may be macro
      pValue->Value.pAssignment->pLValue = ReduceValue[ pValue->Value.pAssignment->pLValue->Kind ]( pValue->Value.pAssignment->pLValue, Parser_pContext );
      pValue->Value.pAssignment->pValue = ReduceValue[ pValue->Value.pAssignment->pValue->Kind ]( pValue->Value.pAssignment->pValue , Parser_pContext);
   }

   return pValue;
}

VALUE * Reduce_FuncCallValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pFunctionCall->pSymbol )
   {
      pValue->Value.pFunctionCall->pSymbol = ReduceValue[ pValue->Value.pFunctionCall->pSymbol->Kind ]( pValue->Value.pFunctionCall->pSymbol, Parser_pContext );

      if( pValue->Value.pFunctionCall->pArguments )
      {
         pValue->Value.pFunctionCall->pArguments = ReduceValue[ pValue->Value.pFunctionCall->pArguments->Kind ]( pValue->Value.pFunctionCall->pArguments, Parser_pContext );
      }
   }

   return pValue;
}

VALUE * Reduce_IIFValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pIIF->pCond && pValue->Value.pIIF->pTrue && pValue->Value.pIIF->pFalse )
   {
      pValue->Value.pIIF->pCond = ReduceValue[ pValue->Value.pIIF->pCond->Kind ]( pValue->Value.pIIF->pCond, Parser_pContext );
      pValue->Value.pIIF->pTrue = ReduceValue[ pValue->Value.pIIF->pTrue->Kind ]( pValue->Value.pIIF->pTrue, Parser_pContext );
      pValue->Value.pIIF->pFalse = ReduceValue[ pValue->Value.pIIF->pFalse->Kind ]( pValue->Value.pIIF->pFalse, Parser_pContext );
   
      if( pValue->Value.pIIF->pCond->Kind == VALUE_KIND_CONSTANT )
      {
         printf( "TODO: REDUCE IIF()\n" );
      }
   }

   return pValue;
}

VALUE * Reduce_MethodCallValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pMethodCall->pObject && pValue->Value.pMethodCall->pMethod )
   {
      pValue->Value.pMethodCall->pObject = ReduceValue[ pValue->Value.pMethodCall->pObject->Kind ]( pValue->Value.pMethodCall->pObject, Parser_pContext );
      pValue->Value.pMethodCall->pMethod = ReduceValue[ pValue->Value.pMethodCall->pMethod->Kind ]( pValue->Value.pMethodCall->pMethod, Parser_pContext );
   }

   return pValue;
}

// Worker of Reduce_ListValue()
LIST * Reduce_List( LIST * pList, PARSER_CONTEXT *Parser_pContext )
{
   LIST_NODE *pNode = pList->pFirst;
   VALUE * pElement;
   
   while( pNode )
   {
      pElement = pNode->pValue;
      
      if( pElement )
      {
         pNode->pValue = ReduceValue[ pElement->Kind ]( pElement, Parser_pContext );
      }
      
      pNode = pNode->pNext;
   }
   
   return pList;
}

VALUE * Reduce_ListValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   pValue->Value.pList = Reduce_List( pValue->Value.pArray, Parser_pContext );
   
   return pValue;
}

#if 0
VALUE * LeftLeaf( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
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
         return LeftLeaf( pValue->Value.pBinary->pLeft, Parser_pContext );
 
      case VALUE_KIND_ALIASED : 
         return LeftLeaf( pValue->Value.pAliased->pArea, Parser_pContext );
         break;
 
      case VALUE_KIND_ASSIGNMENT : 
         return LeftLeaf( )pValue->Value.pAssignment->pLValue );
         break;
 
      case VALUE_KIND_FUNC_CALL : 
      case VALUE_KIND_IIF : 
      case VALUE_KIND_METHOD_CALL : 
         return pValue;

      case VALUE_KIND_LIST : 
         return LeftLeaf( pValue->Value.pList->pLast->pValue, Parser_pContext );

      default :
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in LeftLeaf(2)." );
   }

   return NULL;
}

VALUE * Get_LValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
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
         return pValue->Value.pUnary->pLValue;

      case VALUE_KIND_BINARY : 
         pValue = pValue->Value.pBinary->pLeft;
         goto Get_LValue_Top;
 
      case VALUE_KIND_ALIASED : 
         return pValue;
         
      case VALUE_KIND_ASSIGNMENT :
         return pValue->Value.pAssignment->pLValue;
 
      case VALUE_KIND_FUNC_CALL : 
      case VALUE_KIND_IIF : 
      case VALUE_KIND_METHOD_CALL : 
         return NULL;

      case VALUE_KIND_LIST : 
         return NULL;

      default :
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in Get_LValue()." );
   }

   return NULL;
}
#endif