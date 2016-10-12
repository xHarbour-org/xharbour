#include "common.h"

static VALUE * ( *ReduceValue[] )( VALUE *, PARSER_CONTEXT * ) =
   {
      [ VALUE_KIND_CONSTANT ]                = Reduce_ConstantValue,
      [ VALUE_KIND_ARRAY ]                   = Reduce_ArrayValue,
      [ VALUE_KIND_BLOCK ]                   = Reduce_BlockValue,
      [ VALUE_KIND_BYREF ]                   = Reduce_ByRefValue,
      [ VALUE_KIND_LIST ]                    = Reduce_ListValue,
      
      [ VALUE_KIND_IIF ]                     = Reduce_IIFValue,

      [ VALUE_KIND_UNARY ]                   = Reduce_UnaryValue,
 
      [ VALUE_KIND_ALIASED ]                 = Reduce_AliasedValue,
      [ VALUE_KIND_BINARY ]                  = Reduce_BinaryValue,
      [ VALUE_KIND_ASSIGNMENT ]              = Reduce_AssignmentValue,

      [ VALUE_KIND_FUNCTION_CALL ]           = Reduce_FuncCallValue,
      [ VALUE_KIND_METHOD_CALL ]             = Reduce_MethodCallValue,
      
      [ VALUE_KIND_VARIABLE        & 0x0FF ] = Reduce_VariableValue,
      [ VALUE_KIND_MACRO           & 0x0FF ] = Reduce_MacroValue,
      [ VALUE_KIND_ARRAY_ELEMENT   & 0x0FF ] = Reduce_ArrayElementValue,
      [ VALUE_KIND_OBJECT_PROPERTY & 0x0FF ] = Reduce_ObjectPropertyValue,
      [ VALUE_KIND_ALIASED_FIELD   & 0x0FF ] = Reduce_AliasedFieldValue,
   };

int BinaryPrecedence[ TOKEN_MAX_TOKENS ] = 
{

   [ BINARY_KIND_POWER ]        = 7,

   [ BINARY_KIND_MODULUS ]      = 6,
   [ BINARY_KIND_MULTIPLY ]     = 6,
   [ BINARY_KIND_DIVIDE ]       = 6,

   [ BINARY_KIND_PLUS ]         = 5,
   [ BINARY_KIND_MINUS ]        = 5,

   [ BINARY_KIND_BITLEFT ]      = 4,
   [ BINARY_KIND_BITRIGHT ]     = 4,

   [ BINARY_KIND_IN ]           = 4,
   [ BINARY_KIND_LIKE ]         = 4,
   [ BINARY_KIND_HAS ]          = 4,
   [ BINARY_KIND_GREATEREQUAL ] = 4,
   [ BINARY_KIND_LESSEREQUAL ]  = 4,
   [ BINARY_KIND_GREATER ]      = 4,
   [ BINARY_KIND_LESSER ]       = 4,

   [ BINARY_KIND_EXACTEQUAL ]   = 3,
   [ BINARY_KIND_EQUAL ]        = 3,
   [ BINARY_KIND_NOTEQUAL ]     = 3,

   [ BINARY_KIND_BITOR ]        = 2,
   [ BINARY_KIND_BITXOR ]       = 2,
   [ BINARY_KIND_BITAND ]       = 2,

   [ BINARY_KIND_AND ]          = 1,
   [ BINARY_KIND_OR ]           = 1,
};

VALUE * Reduce_ConstantValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   return pValue;
}

VALUE * Reduce_ArrayValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   pValue->Value.pArray = Reduce_List( pValue->Value.pArray, Parser_pContext );

   return pValue;
}

VALUE * Reduce_BlockValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   return pValue;
}

VALUE * Reduce_ByRefValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   pValue->Value.pBlock->pList = Reduce_List( pValue->Value.pArray, Parser_pContext );
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

VALUE * Reduce_UnaryValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pUnary->pLValue )
   {
      pValue->Value.pUnary->pLValue = ReduceValue[ pValue->Value.pUnary->pLValue->Kind ]( pValue->Value.pUnary->pLValue, Parser_pContext );
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

VALUE * Reduce_MethodCallValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Value.pMethodCall->pObject && pValue->Value.pMethodCall->pMethod )
   {
      pValue->Value.pMethodCall->pObject = ReduceValue[ pValue->Value.pMethodCall->pObject->Kind ]( pValue->Value.pMethodCall->pObject, Parser_pContext );
      pValue->Value.pMethodCall->pMethod = ReduceValue[ pValue->Value.pMethodCall->pMethod->Kind ]( pValue->Value.pMethodCall->pMethod, Parser_pContext );
   }

   return pValue;
}

VALUE * Reduce_VariableValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   return pValue;
}

VALUE * Reduce_MacroValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   switch( pValue->Value.pMacro->Kind )
   {
      case MACRO_KIND_SIMPLE :
         return pValue;
         
      case MACRO_KIND_COMPLEX :
         pValue->Value.pMacro->Value.pComplex =  ReduceValue[ pValue->Value.pMacro->Value.pComplex->Kind ]( pValue->Value.pMacro->Value.pComplex, Parser_pContext );
         
      default :
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in: " __SOURCE__, Parser_pContext );
   }
   
   return pValue;
}

VALUE * Reduce_ArrayElementValue( VALUE *pValue, PARSER_CONTEXT *Parser_pContext )
{
   pValue->Value.pArrayElement->pArray = ReduceValue[ pValue->Value.pArrayElement->pArray->Kind ]( pValue->Value.pArrayElement->pArray, Parser_pContext );
   pValue->Value.pArrayElement->pIndexList = Reduce_List( pValue->Value.pArrayElement->pIndexList, Parser_pContext );
   
   return pValue;
}

VALUE * Reduce_ObjectPropertyValue( VALUE *pValue, PARSER_CONTEXT *Parser_pContext )
{
   pValue->Value.pObjectProperty->pObject = ReduceValue[ pValue->Value.pObjectProperty->pObject->Kind ]( pValue->Value.pObjectProperty->pObject, Parser_pContext );
   pValue->Value.pObjectProperty->pSymbol = ReduceValue[ pValue->Value.pObjectProperty->pSymbol->Kind ]( pValue->Value.pObjectProperty->pSymbol, Parser_pContext );
   
   return pValue;
}

VALUE * Reduce_AliasedFieldValue( VALUE *pValue, PARSER_CONTEXT *Parser_pContext )
{
   pValue->Value.pAliasedField->pSymbol = ReduceValue[ pValue->Value.pAliasedField->pSymbol->Kind ]( pValue->Value.pAliasedField->pSymbol, Parser_pContext );
   pValue->Value.pAliasedField->pAlias = ReduceValue[ pValue->Value.pAliasedField->pAlias->Kind ]( pValue->Value.pAliasedField->pAlias, Parser_pContext );
   
   return pValue;
}
