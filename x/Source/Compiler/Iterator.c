//
//  Iterator.c
//  x
//
//  Created by Ron Pinkas on 6/26/13.
//  Copyright (c) 2013 http://www.xHarbour.com. All rights reserved.
//

#include "common.h"

static char sPad[2048] = { 0 } ;

int IterateLValue( LVALUE *pLValue, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;

   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';

   printf( "%s %i Iterating LValue Kind: %s\n", sPad, iNestingLevel, ClipNet_LValueKind( pLValue, Parser_pContext ) );
   
   switch( pLValue->Kind )
   {
      case LVALUE_KIND_VARIABLE:
         printf( "%s %i [%s] Variable: %s\n", sPad, iNestingLevel, ClipNet_DeclaredKind( pLValue->Value.pVariable, Parser_pContext ) , pLValue->Value.pVariable->pID->Name );
         break;
         
      case LVALUE_KIND_ALIASED_FIELD:
         printf( "%s %i Variable: %s\n", sPad, iNestingLevel, pLValue->Value.pVariable->pID->Name );
         break;
         
       default:
         iResult = 1;
         break;
   }

   sPad[iNestingLevel--] = '\0';
   sPad[iNestingLevel] = ' ';
   
   printf( "%s %i DONE Iterating LValue Kind: %s\n", sPad, iNestingLevel + 1, ClipNet_LValueKind( pLValue, Parser_pContext ) );
   
   return iResult;
}

int IterateValue( VALUE *pValue, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
   
   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Value Kind: %s\n", sPad, iNestingLevel, ClipNet_ValueKind( pValue, Parser_pContext ) );

   switch( pValue->Kind )
   {
      case VALUE_KIND_NIL:
         break;
         
      case VALUE_KIND_CONSTANT:
         break;

      case VALUE_KIND_LVALUE:
         iResult = IterateLValue( pValue->Value.pLValue, Parser_pContext, iNestingLevel );
         break;
         
      case VALUE_KIND_ARRAY:
         break;
      case VALUE_KIND_BLOCK:
         break;
      case VALUE_KIND_UNARY:
         break;
         
      case VALUE_KIND_BINARY:
         iResult = IterateValue( pValue->Value.pBinary->pLeft, Parser_pContext, iNestingLevel );
         iResult = iResult || IterateValue( pValue->Value.pBinary->pRight, Parser_pContext, iNestingLevel );
         break;
         
      case VALUE_KIND_ALIASED:
         break;
      case VALUE_KIND_ASSIGNMENT:
         iResult = IterateValue( pValue->Value.pAssignment->pLValue, Parser_pContext, iNestingLevel );
         iResult = iResult || IterateValue( pValue->Value.pAssignment->pValue, Parser_pContext, iNestingLevel );
         break;
         
      case VALUE_KIND_FUNC_CALL:
         break;
      case VALUE_KIND_IIF:
         break;
      case VALUE_KIND_METHOD_CALL:
         break;
      case VALUE_KIND_LIST:
         break;
      case VALUE_KIND_BYREF:
         break;
         
      default:
         iResult = 1;
   }
   
   sPad[iNestingLevel--] = '\0';
   sPad[iNestingLevel] = ' ';
   
   printf( "%s %i DONE Iterating Value Kind: %s\n", sPad, iNestingLevel + 1, ClipNet_ValueKind( pValue, Parser_pContext ) );

   return iResult;
}

int IterateLine( LINE *pLine, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
   
   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Line: %i Kind: %s\n", sPad, iNestingLevel, pLine->iNo, ClipNet_LineKind(pLine, Parser_pContext ) );
      
   switch( pLine->Kind )
   {
      case LINE_KIND_ASSIGNMENT:
         iResult = IterateValue( pLine->Value.pAssignment, Parser_pContext, iNestingLevel );
         break;
         
      case LINE_KIND_CASE:
         break;
      case LINE_KIND_OTHERWISE:
         break;
   
      case LINE_KIND_FOR:
         break;
      case LINE_KIND_WHILE:
         break;
      case LINE_KIND_FLOW:
   
      case LINE_KIND_FUNC_CALL:
         break;
      case LINE_KIND_IIF:
         break;
         
      case LINE_KIND_METHOD_CALL:
         break;
   
      case LINE_KIND_IF:
         iResult = IterateValue( pLine->Value.pIf->pCondExp, Parser_pContext, iNestingLevel );
         iResult = IterateBody( pLine->Value.pIf->pBody, Parser_pContext, iNestingLevel );
         {
            LINE *pElseIf = pLine->Value.pIf->pElseIf;
            
            while( pElseIf)
            {
               iResult = IterateLine( pElseIf, Parser_pContext, iNestingLevel );
               iResult = IterateBody( pElseIf->Value.pElseIf->pBody, Parser_pContext, iNestingLevel );
               pElseIf = pElseIf->Value.pElseIf->pNext;
            }
         }
         break;
         
      case LINE_KIND_ELSEIF:
         break;
      case LINE_KIND_ELSE:
         break;
   
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
   
      case LINE_KIND_UNARY:
         break;
   
      case LINE_KIND_LIST:
         iResult = 0;
         break;
         
      default:
         iResult = 1;
   }
   
   sPad[iNestingLevel--] = '\0';
   sPad[iNestingLevel] = ' ';
   
   printf( "%s %i DONE Iterating Line: %i Kind: %s\n", sPad, iNestingLevel + 1, pLine->iNo, ClipNet_LineKind(pLine, Parser_pContext ) );
   
   return iResult;
}

int IterateBody( BODY *pBody, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{   
   LINE *pLine = pBody->pLines;
   
   int iResult = 0;

   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Body\n", sPad, iNestingLevel );
   
   while( pLine && iResult == 0 )
   {
      iResult = IterateLine( pLine, Parser_pContext, iNestingLevel );
      
      pLine = pLine->pNext;
   }
   
   sPad[iNestingLevel--] = '\0';
   sPad[iNestingLevel] = ' ';
   
   printf( "%s %i DONE Iterating Body\n", sPad, iNestingLevel + 1 );

   return iResult;
}

int IterateAST( PARSER_CONTEXT *Parser_pContext )
{
   int iNestingLevel = 0;
   
   FUNCTION *pFunc = Parser_pContext->Parser_Functions.pFirst;
   int iResult = 0;
   
   while( pFunc && iResult == 0 )
   {
      sPad[iNestingLevel++] = ' ';
      sPad[iNestingLevel] = '\0';
      
      printf( "%s %i Iterating Function: %s\n", sPad, iNestingLevel, pFunc->pName->Name );
      
      iResult = IterateBody( pFunc->pBody, Parser_pContext, iNestingLevel );

      sPad[iNestingLevel--] = '\0';
      sPad[iNestingLevel] = ' ';

      printf( "%s %i DONE Iterating Function: %s\n", sPad, iNestingLevel, pFunc->pName->Name );

      pFunc = pFunc->pNext;
   }
   
   return iResult;
}
