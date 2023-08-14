//
//  Iterator.c
//  x
//
//  Created by Ron Pinkas on 6/26/13.
//  Copyright (c) 2013 http://www.xHarbour.com. All rights reserved.
//

#include "common.h"

static char sPad[2048] = { 0 } ;

int IterateMacro( MACRO * pMacro, PARSER_CONTEXT *Parser_pContext, int iNestingLevel, BOOL bAssign )
{
   int iResult = 0;

   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Macro Kind: %s\n", sPad, iNestingLevel, ClipNet_MacroKind( pMacro ) );
   
   if( bAssign )
   {
      printf( "*** ASSIGN ***\n" );
   }
      
   switch( pMacro->Kind )
   {
      case  MACRO_KIND_SIMPLE:
         printf( "%s %i Simple Macro: %s\n", sPad, iNestingLevel, pMacro->Value.pID->Name );
         break;
        
      case  MACRO_KIND_COMPLEX:
         iResult = IterateValue( pMacro->Value.pComplex, Parser_pContext, iNestingLevel );
         break;
        
      default:
        iResult = 1;
   }

   printf( "%s %i DONE Iterating Macro Kind: %s\n", sPad, iNestingLevel, ClipNet_MacroKind( pMacro ) );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateVariable( DECLARED *pVariable, PARSER_CONTEXT *Parser_pContext, int iNestingLevel, VALUE_KIND Kind )
{
   int iResult = 0;
   
   if( Kind & VALUE_KIND_MEMVAR )
   {
       printf( "%s %i [%s] Variable: %s\n", sPad, iNestingLevel, "MEMVAR", pVariable->pID->Name );
   }
   else if( Kind & VALUE_KIND_FIELD )
   {
       printf( "%s %i [%s] Variable: %s\n", sPad, iNestingLevel, "FIELD", pVariable->pID->Name );
   }
   else
   {
      printf( "%s %i [%s] Variable: %s\n", sPad, iNestingLevel, ClipNet_DeclaredKind( pVariable ), pVariable->pID->Name );
   }
    
   return iResult;
}

int IterateAliasedField( ALIASED_FIELD *pAliasedField, PARSER_CONTEXT *Parser_pContext, int iNestingLevel, BOOL bAssign )
{
   int iResult = 0;

   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Aliased Field\n", sPad, iNestingLevel );
   
   if( bAssign )
   {
      printf( "*** ASSIGN ***\n" );
   }
   
   IterateValue( pAliasedField->pAlias, Parser_pContext, iNestingLevel );

   IterateValue( pAliasedField->pSymbol, Parser_pContext, iNestingLevel );
  
   printf( "%s %i DONE Iterating Aliased Field\n", sPad, iNestingLevel );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateBinary( BINARY *pBinary, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
   
   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Binary Kind: %s\n", sPad, iNestingLevel, ClipNet_BinaryKind( pBinary ) );

   iResult = IterateValue( pBinary->pRight, Parser_pContext, iNestingLevel );
   iResult = iResult || IterateValue( pBinary->pLeft, Parser_pContext, iNestingLevel );
   
   printf( "%s %i DONE Iterating Binary\n", sPad, iNestingLevel );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateAssignment( ASSIGNMENT * pAssignment, BOOL bUsed, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
   
   iResult = IterateValue( pAssignment->pValue, Parser_pContext, iNestingLevel );
   iResult = iResult || IterateValue( pAssignment->pLValue, Parser_pContext, iNestingLevel );
   
   if( bUsed )
   {
      printf( "PUSH\n" );
   }
   
   return iResult;
}

int IterateLValue( VALUE *pValue, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
   
   switch( pValue->Kind & ~( VALUE_KIND_ERROR_MASK | VALUE_KIND_ASSIGNED_MASK ) )
   {
      case VALUE_KIND_VARIABLE:
         iResult = IterateVariable( pValue->Value.pVariable, Parser_pContext ,iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
         break;
         
      case VALUE_KIND_MACRO:
         iResult = IterateMacro( pValue->Value.pMacro, Parser_pContext, iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
         break;
         
      case VALUE_KIND_ALIASED_FIELD:
         iResult = IterateAliasedField( pValue->Value.pAliasedField, Parser_pContext, iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
         break;
         
      default:
         iResult = 1;
   }
   
   return iResult;
}

int IterateUnary( UNARY *pUnary, BOOL bUsed, PARSER_CONTEXT *Parser_pContext, int iNestedLevel )
{
   int iResult = 0;
   
   if( pUnary->When == UNARY_WHEN_PRE )
   {
      printf( "%s\n", ClipNet_UnaryKind( pUnary ) );
      IterateLValue( pUnary->pLValue, Parser_pContext, iNestedLevel );
   }
   
   if( bUsed )
   {
      printf( "PUSH\n" );
   }

   if( pUnary->When == UNARY_WHEN_POST )
   {
      printf( "%s\n", ClipNet_UnaryKind( pUnary ) );
      IterateLValue( pUnary->pLValue, Parser_pContext, iNestedLevel );
   }
   
   return iResult;
}

int IterateArrayElement( ARRAY_ELEMENT *pArrayElement, PARSER_CONTEXT *Parser_pContext, int iNestingLevel, BOOL bAssign )
{
   int iResult = 0;
   
   //TODO:
   IterateValue( pArrayElement->pArray, Parser_pContext, iNestingLevel );
   //IterateList( pArrayElement->pIndexList, Parser_pContext, iNestingLevel );
   
   return iResult;
}

int IterateValue( VALUE *pValue, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
   VALUE_KIND UnMaskedKind = pValue->Kind & ~( VALUE_KIND_ERROR_MASK | VALUE_KIND_ASSIGNED_MASK );
                                              
   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Value Kind: %s\n", sPad, iNestingLevel, ClipNet_ValueKind( pValue ) );

   switch( UnMaskedKind )
   {
      case VALUE_KIND_VARIABLE:
         iResult = IterateVariable( pValue->Value.pVariable, Parser_pContext ,iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
         break;

       case VALUE_KIND_MEMVAR:
           iResult = IterateVariable( pValue->Value.pVariable, Parser_pContext ,iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
           break;
           
       case VALUE_KIND_FIELD:
           iResult = IterateVariable( pValue->Value.pVariable, Parser_pContext ,iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
           break;
           
      case VALUE_KIND_MACRO:
         iResult = IterateMacro( pValue->Value.pMacro, Parser_pContext, iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
         break;
 
      case VALUE_KIND_ARRAY_ELEMENT:
         iResult = IterateArrayElement( pValue->Value.pArrayElement, Parser_pContext, iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
         break;
         
      case VALUE_KIND_OBJECT_PROPERTY:
         //iResult = IterateObjectProperty( pValue->Value.pObjectProperty, Parser_pContext, iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
         break;
         
      case VALUE_KIND_ALIASED_FIELD:
         iResult = IterateAliasedField( pValue->Value.pAliasedField, Parser_pContext, iNestingLevel, ( pValue->Kind & VALUE_KIND_ASSIGNED_MASK ) == VALUE_KIND_ASSIGNED_MASK );
         break;
         
      case VALUE_KIND_CONSTANT:
         break;

      case VALUE_KIND_ARRAY:
         break;
         
      case VALUE_KIND_BLOCK:
         break;
         
      case VALUE_KIND_BYREF:
         break;
         
      case VALUE_KIND_BINARY:
         IterateBinary( pValue->Value.pBinary, Parser_pContext, iNestingLevel );
         break;
         
      case VALUE_KIND_ALIASED:
         break;
      
         
      case VALUE_KIND_UNARY:
         IterateUnary(pValue->Value.pUnary, TRUE, Parser_pContext, iNestingLevel );
         break;
         
      case VALUE_KIND_ASSIGNMENT:
         IterateAssignment( pValue->Value.pAssignment, TRUE, Parser_pContext, iNestingLevel );
         break;
         
      case VALUE_KIND_IIF:
         break;
         
      case VALUE_KIND_FUNCTION_CALL:
         break;
         
      case VALUE_KIND_METHOD_CALL:
         break;
         
      case VALUE_KIND_LIST:
         break;
   
         
      default:
         iResult = 1;
   }
   
   printf( "%s %i DONE Iterating Value Kind: %s\n", sPad, iNestingLevel, ClipNet_ValueKind( pValue ) );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateLine( LINE *pLine, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
   
   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Line: %i Kind: %s\n", sPad, iNestingLevel, pLine->iNo, ClipNet_LineKind( pLine ) );
      
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
      
      case LINE_KIND_IF:
         iResult = IterateValue( pLine->Value.pIf->pCondExp, Parser_pContext, iNestingLevel );
         iResult = iResult || IterateBody( pLine->Value.pIf->pBody, Parser_pContext, iNestingLevel );
         {
            LINE *pElseIf = pLine->Value.pIf->pElseIf;
            LINE *pElse   = pLine->Value.pIf->pElse;
            
            while( pElseIf)
            {
               iResult = IterateLine( pElseIf, Parser_pContext, iNestingLevel );
               iResult = iResult || IterateBody( pElseIf->Value.pElseIf->pBody, Parser_pContext, iNestingLevel );
               pElseIf = pElseIf->Value.pElseIf->pNext;
            }
            
            
            if( pElse)
            {
               iResult = IterateLine( pElse, Parser_pContext, iNestingLevel );
               iResult = iResult || IterateBody( pElse->Value.pElse->pBody, Parser_pContext, iNestingLevel );
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
         IterateUnary( pLine->Value.pUnary, FALSE, Parser_pContext, iNestingLevel );
         break;
   
      case LINE_KIND_ASSIGNMENT:
         IterateAssignment( pLine->Value.pAssignment, FALSE, Parser_pContext, iNestingLevel );
         break;

      case LINE_KIND_IIF:
         break;
                  
      case LINE_KIND_FUNCTION_CALL:
         break;
         
      case LINE_KIND_METHOD_CALL:
         break;
        
      case LINE_KIND_LIST:
         iResult = 0;
         break;
         
      default:
         iResult = 1;
   }
   
   printf( "%s %i DONE Iterating Line: %i Kind: %s\n", sPad, iNestingLevel, pLine->iNo, ClipNet_LineKind( pLine ) );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateBody( BODY *pBody, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{   
   LINE *pLine = pBody->pLines;
   
   int iResult = 0;

   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating Body\n", sPad, iNestingLevel );
   
   while( pLine )
   {
      iResult = IterateLine( pLine, Parser_pContext, iNestingLevel );
      
      pLine = pLine->pNext;
   }
   
   printf( "%s %i DONE Iterating Body\n", sPad, iNestingLevel );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateLocalParams( DECLARED *pLocalParam, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;

   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';

   printf( "%s %i Iterating  Local Params: \n", sPad, iNestingLevel );
   
   while( pLocalParam )
   {
      printf( "%s\n", pLocalParam->pID->Name );
      
      if( pLocalParam->Attribute.pInit )
      {
         IterateValue( pLocalParam->Attribute.pInit, Parser_pContext, iNestingLevel );
      }
      
      pLocalParam = pLocalParam->pNext;
   }
 
   printf( "%s %i DONE Iterating Local Params\n", sPad, iNestingLevel );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateLocals( DECLARED *pLocal, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;

   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
   
   printf( "%s %i Iterating  Locals:\n", sPad, iNestingLevel );
   
   while( pLocal )
   {
      printf( "%s\n", pLocal->pID->Name );

      if( pLocal->Attribute.pInit )
      {
         IterateValue( pLocal->Attribute.pInit, Parser_pContext, iNestingLevel );
      }
      
      pLocal = pLocal->pNext;
   }
 
   printf( "%s %i DONE Iterating Locals\n", sPad, iNestingLevel );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateStatics( DECLARED *pStatic, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
 
   sPad[iNestingLevel++] = ' ';
   sPad[iNestingLevel] = '\0';
  
   printf( "%s %i Iterating  Statics: \n", sPad, iNestingLevel );

   while( pStatic )
   {
      printf( "%s\n", pStatic->pID->Name );
      
      if( pStatic->Attribute.pInit )
      {
         IterateValue( pStatic->Attribute.pInit, Parser_pContext, iNestingLevel );
      }
      
      pStatic = pStatic->pNext;
   }

   printf( "%s %i DONE Iterating Statics\n", sPad, iNestingLevel );

   sPad[--iNestingLevel] = '\0';
   
   return iResult;
}

int IterateDeclared( DECLARED *pDeclared, PARSER_CONTEXT *Parser_pContext, int iNestingLevel )
{
   int iResult = 0;
   
   printf( "%s %i Iterating %s:(%s)", sPad, iNestingLevel, ClipNet_DeclaredKind( pDeclared ), ( pDeclared->Attribute.pAlias ? pDeclared->Attribute.pAlias->Name : "" ) );
   
   while( pDeclared )
   {
      printf( "%s%s", pDeclared->pID->Name, ( pDeclared->pNext ? ", " : "" ) );
      
      pDeclared = pDeclared->pNext;
   }
   
   printf( "\n" );
   
   return iResult;
}

int IterateAST( PARSER_CONTEXT *Parser_pContext )
{
   int iNestingLevel = 0;
   
   FUNCTION *pFunc = Parser_pContext->Functions.pFirst;
   int iResult = 0;
   
   //TODO: Declared
   //IterateGlobals()
   
   while( pFunc && iResult == 0 )
   {
      sPad[iNestingLevel++] = ' ';
      sPad[iNestingLevel] = '\0';
      
      printf( "%s %i Iterating Function: %s\n", sPad, iNestingLevel, pFunc->pName->Name );
      
      IterateLocalParams( pFunc->pLocalParameters, Parser_pContext, iNestingLevel );
      IterateStatics( pFunc->pStatics, Parser_pContext, iNestingLevel );
      IterateLocals( pFunc->pLocals, Parser_pContext, iNestingLevel );

      iResult = IterateBody( pFunc->pBody, Parser_pContext, iNestingLevel );

      printf( "%s %i DONE Iterating Function: %s\n", sPad, iNestingLevel, pFunc->pName->Name );
 
      sPad[--iNestingLevel] = '\0';

      pFunc = pFunc->pNext;
   }
   
   return iResult;
}
