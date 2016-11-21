#include "common.h"

void Release_ID( ID *pID )
{
   // TODO:
}

void Release_Declared( DECLARED *pDeclared )
{
   Release_ID( pDeclared->pID );
   
   if( pDeclared->Kind == DECLARED_KIND_FIELD )
   {
      #if 0
      if( pDeclared->Attribute.pAlias )
      {
         Release_ID( pDeclared->Attribute.pAlias );
      }
      #endif
   }
   else if( pDeclared->Attribute.pInit )
   {
      Release_Value( pDeclared->Attribute.pInit );
   }
}

void Release_Macro( MACRO *pMacro )
{
   switch( pMacro->Kind )
   {
      case MACRO_KIND_SIMPLE:
         Release_ID( pMacro->Value.pID );
         break;

      case MACRO_KIND_COMPLEX:
         Release_Value( pMacro->Value.pComplex );
         break;
   }
}

void Release_ListNode( LIST_NODE *pListNode )
{
   Release_Value( pListNode->pValue );
   ClipNet_free( (void *) pListNode );
}

void Release_List( LIST *pList )
{
   LIST_NODE *pFree, *pListNode = pList->pFirst;

   while( pListNode )
   {
      pFree = pListNode;
      pListNode = pListNode->pNext;
      Release_ListNode( pFree );
   }

   ClipNet_free( (void *) pList );
}

void Release_Block( BLOCK *pBlock )
{
   if( pBlock->pBlockLocals )
   {
      ClipNet_free( pBlock->pBlockLocals );
   }
   
   if( pBlock->pList )
   {
      ClipNet_free( pBlock->pList );
   }
   
   ClipNet_free( pBlock );
}

void Release_Unary( UNARY *pUnary )
{
   Release_Value( pUnary->pLValue );
   ClipNet_free( (void *) pUnary );
}

void Release_Binnary( BINARY *pBinary )
{
   Release_Value( pBinary->pLeft );
   Release_Value( pBinary->pRight );
   
   ClipNet_free( (void *) pBinary );
}

void Release_Alias( ALIASED *pAliased )
{
   Release_Value( pAliased->pArea );
   Release_Value( pAliased->pValue );
   
   ClipNet_free( (void *) pAliased );
}

void Release_Assignment( ASSIGNMENT *pAssignment )
{
   Release_Value( pAssignment->pLValue );
   Release_Value( pAssignment->pValue );
   
   ClipNet_free( (void *) pAssignment );
}

void Release_FunctionCall( FUNCTION_CALL *pFunctionCall )
{
   Release_Value( pFunctionCall->pSymbol );
   
   if( pFunctionCall->pArguments )
   {
      Release_Value( pFunctionCall->pArguments );
   }
   
   ClipNet_free( (void *) pFunctionCall );
}

void Release_IIF( IIF *pIIF )
{
   Release_Value( pIIF->pCond );
   Release_Value( pIIF->pTrue );
   Release_Value( pIIF->pFalse );
   
   ClipNet_free( (void *) pIIF );
}

void Release_MethodCall( METHOD_CALL *pMethodCall )
{
   Release_Value( pMethodCall->pObject );
   // REVIEW!
   Release_Value( pMethodCall->pMethod );
   
   ClipNet_free( (void *) pMethodCall );
}

void Release_ByRef( VALUE *pByRef )
{
   ClipNet_free( (void *) pByRef );
}

void Release_Value( VALUE *pValue )
{
   switch( pValue->Kind & ~( VALUE_KIND_ERROR_MASK | VALUE_KIND_ASSIGNED_MASK ) )
   {

      case VALUE_KIND_NONE:
         break;
         
      case VALUE_KIND_VARIABLE :
         Release_Declared( pValue->Value.pVariable );
         break;
         
      case VALUE_KIND_MACRO :
         Release_Macro( pValue->Value.pMacro );
         break;
         
      case VALUE_KIND_ARRAY_ELEMENT :
      case VALUE_KIND_OBJECT_PROPERTY :
      case VALUE_KIND_ALIASED_FIELD :
         
      case VALUE_KIND_CONSTANT :
         ClipNet_free( (void *) pValue->Value.pConstant );
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
 
      case VALUE_KIND_FUNCTION_CALL :
         Release_FunctionCall( pValue->Value.pFunctionCall );
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
         
      case VALUE_KIND_BYREF :
         //Release_ByRef( pValue->Value.pByRef );
         break;
         
      default:
         printf( "Internal error - unexpected case in: " __SOURCE__ );
   }

   ClipNet_free( (void *) pValue );
}

void Relese_Line( LINE *pLine, PARSER_CONTEXT *Parser_pContext )
{
   switch( pLine->Kind )
   {
      case LINE_KIND_CASE:
         break;
      case LINE_KIND_OTHERWISE:
         break;
   
      case LINE_KIND_FOR:
         break;
      case LINE_KIND_WHILE:
         break;
      case LINE_KIND_FLOW:
         break;
   
      case LINE_KIND_IF:
         break;
      case LINE_KIND_ELSEIF:
         break;
      case LINE_KIND_ELSE:
   
      case LINE_KIND_PARAMETERS:
         break;
      case LINE_KIND_PRIVATES:
         break;
      case LINE_KIND_PUBLICS:
         break;
   
      case LINE_KIND_RETURN:
         break;
   
      case LINE_KIND_SEQUENCE:
         break;
      case LINE_KIND_RECOVER:
         break;
   
      case LINE_KIND_SWITCH:
         break;
      case LINE_KIND_SWITCHCASE:
         break;
      case LINE_KIND_SWITCHDEAFULT:
         break;
   
      case LINE_KIND_TRY:
         break;
      case LINE_KIND_CATCH:
         break;
      case LINE_KIND_FINALLY:
         break;
   
      case LINE_KIND_ASSIGNMENT:
         Release_Assignment( pLine->Value.pAssignment );
         break;         
      case LINE_KIND_UNARY:
         Release_Unary( pLine->Value.pUnary );
         break;
      case LINE_KIND_FUNCTION_CALL:
         Release_FunctionCall( pLine->Value.pFuncttionCall );
         break;
      case LINE_KIND_IIF:
         Release_IIF( pLine->Value.pIIF );
         break;
      case LINE_KIND_METHOD_CALL:
         Release_MethodCall( pLine->Value.pMethodCall );
         break;
      case LINE_KIND_LIST:
         Release_List( pLine->Value.pList );
         break;

      default:
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", internal error - unexpected case in: LineKind()", Parser_pContext );                  
   }
}
