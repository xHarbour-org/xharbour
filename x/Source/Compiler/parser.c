/*  
 * ClipNet Project source code:
 * Parser main file.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

//#define DEBUG_VALUE
//#define DEBUG_BINARY

#include "common.h"

//extern char *Parser_asErrors[];
void       ParseSource( PARSER_CONTEXT *Parser_pContext );
void       ParseFunction( PARSER_CONTEXT *Parser_pContext );

DECLARED * ParseDeclaredList( DECLARED_KIND Kind, int *piParams, PARSER_CONTEXT *Parser_pContext );

DECLARED * ParseGlobalDeclarationLine( PARSER_CONTEXT *Parser_pContext );
void       ParseGlobalDeclarations( PARSER_CONTEXT *Parser_pContext );
DECLARED * ParseDeclarationLine( PARSER_CONTEXT *Parser_pContext );
void       ParseDeclarations( PARSER_CONTEXT *Parser_pContext );

BODY *     ParseBody( PARSER_CONTEXT *Parser_pContext );
LINE *     ParseIf( PARSER_CONTEXT *Parser_pContext );
VALUE *    ParseValue( PARSER_CONTEXT *Parser_pContext );
LIST *     ParseList( PRG_TYPE Type, PARSER_CONTEXT *Parser_pContext );
VALUE *    ParseListAsValue( PRG_TYPE Type, PARSER_CONTEXT *Parser_pContext );
void *     ParseLine( PARSING_STATE State, PARSER_CONTEXT *Parser_pContext );

YYSTYPE   yylval;

int main( int argc, char *argv[] )
{
   PARSER_CONTEXT *Parser_pContext = New_Context();
   FILE *hSource;
   
   if( argc >= 2 )
   { 
      hSource = fopen( (const char *) argv[1], "r" );
   }
   else
   {
      printf( "Syntax: Parser <source>\n" ); 
      return 1; 
   } 
 
   if( hSource )
   { 
      PARSED_FILE *pSource = NEW( PARSED_FILE ); 

      pSource->sName = argv[1];
      pSource->hFile = hSource; 
      pSource->pNext = NULL;
      pSource->pPrev = NULL;
      pSource->iLine = 0;

      Parser_pContext->Files.iFiles = 1;
      Parser_pContext->Files.pLast = pSource;
 
      Parser_pContext->pIDs = NewMap( 2039 );
      
      ParseSource( Parser_pContext );
      
      IterateAST( Parser_pContext );
      
      ListMapEntries( Parser_pContext->pIDs );
      DeleteMap(Parser_pContext->pIDs );
      
      pSource = Parser_pContext->Files.pLast;
 
      while( pSource ) 
      {
         PARSED_FILE *pFree = pSource;
         pSource = pSource->pPrev; 
         ClipNet_free( pFree ); 
      }

      return 0;
   }
   else
   { 
      printf( "Could not open: '%s'\n", argv[1] ); 
      return 1;
   } 
}

#ifdef PARSE_ERROR_FUNCTION
void PARSE_ERROR( int iError, const char *sError1, const char *sError2, PARSER_CONTEXT *Parser_pContext )
{
   Parser_pContext->sErrorSource = __SOURCE__;
   Parser_pContext->sError1 = sError1;
   Parser_pContext->sError2 = sError2;
   
   assert( Parser_pContext->bCanJump );
   
   if( Parser_pContext->bCanJump )
   {
      longjmp( Parser_pContext->JumpBuffer, iError );
   }
   
   return;
}
#endif

void ParseSource( PARSER_CONTEXT *Parser_pContext )
{
   ParseGlobalDeclarations( Parser_pContext );
   
   while( LOOK_AHEAD_TOKEN() > 0 )
   {
      ParseFunction( Parser_pContext );
   }
   
   fclose( Parser_pContext->Files.pLast->hFile );
}

DECLARED * ParseGlobalDeclarationLine( PARSER_CONTEXT *Parser_pContext )
{
   DECLARED **ppDeclared;
   int      *piDeclared;
   
   switch( LOOK_AHEAD_TOKEN() )
   {
      case TOKEN_GLOBAL :
         ppDeclared = &( Parser_pContext->pGlobals );
         piDeclared = &( Parser_pContext->iGlobals );
         break;
         
      case TOKEN_EXTERNGLOBAL :
         ppDeclared = &( Parser_pContext->pExternGlobals );
         piDeclared = &( Parser_pContext->iExternGlobals );
         break;
         
      case TOKEN_STATIC :
         ppDeclared = &( Parser_pContext->pStatics );
         piDeclared = &( Parser_pContext->iStatics );
         break;
         
      case TOKEN_MEMVAR :
         ppDeclared = &( Parser_pContext->pMemvars );
         piDeclared = &( Parser_pContext->iMemvars );
         break;
         
      case TOKEN_FIELD :
         ppDeclared = &( Parser_pContext->pFields );
         piDeclared = &( Parser_pContext->iFields );
         break;
         
      case TOKEN_DECLARE :
         //TODO:
         return NULL;
         
      default:
         return NULL;
   }
   
   USE_AHEAD_TOKEN();
   
   while( *ppDeclared )
   {
      ppDeclared = &( ( *ppDeclared )->pNext );
   }
   
   *ppDeclared = ParseDeclaredList( (DECLARED_KIND) LAST_TOKEN(), piDeclared, Parser_pContext );
   
   ACCEPT_EOL()
   
   return *ppDeclared;
}

void ParseGlobalDeclarations( PARSER_CONTEXT *Parser_pContext )
{
   do
   {
#ifdef GETTING_EMPTY_LINES
      while( LOOK_AHEAD_TOKEN() == '\n' )
      {
         DROP_AHEAD_TOKEN();
      }
#endif
      
      if( NEW_DEFINITION( LOOK_AHEAD_TOKEN() ) )
      {
         return;
      }

      ParseLine( PARSING_STATE_GLOBAL_DECLARATIONS, Parser_pContext );      
   }
   while( LAST_TOKEN() == '\n' );
   
   return;
}

FUNCTION * ParseFunctionDefinitionLine( PARSER_CONTEXT *Parser_pContext )
{
   FUNCTION * pFunc;
   FUNC_KIND FuncKind = (FUNC_KIND) LOOK_AHEAD_TOKEN();
   
   if( FuncKind >= FUNC_KIND_MIN && FuncKind <= FUNC_KIND_MAX )
   {
      DROP_AHEAD_TOKEN();
      
      if( LOOK_AHEAD_TOKEN() == TOKEN_IDENTIFIER )
      {
         DROP_AHEAD_TOKEN();
         
         pFunc = New_Function( yylval.sText, FuncKind, Parser_pContext );
         
         if( LOOK_AHEAD_TOKEN() == '(' )
         {
            DROP_AHEAD_TOKEN();
            
            pFunc->pLocalParameters = ParseDeclaredList( DECLARED_KIND_LOCAL_PARAMETER, &( pFunc->iLocalParameters ), Parser_pContext );
            
            if( LOOK_AHEAD_TOKEN() == ')' )
            {
               DROP_AHEAD_TOKEN();
            }
            else
            {
               PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected ')'.", Parser_pContext );
            }
         }
         
         ACCEPT_EOL();
         
         return pFunc;
      }
   }
   
   PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected entity definition.", Parser_pContext );
   
   return NULL;
}

void ParseFunction( PARSER_CONTEXT *Parser_pContext )
{

   FUNCTION * pFunc = ParseLine( PARSING_STATE_DEFINITION, Parser_pContext );
   
   if( pFunc)
   {
       ParseDeclarations( Parser_pContext );

       pFunc->pBody = ParseBody( Parser_pContext );
   }

   return;
}
  
DECLARED * ParseDeclaredList( DECLARED_KIND Kind, int *piDeclares, PARSER_CONTEXT *Parser_pContext )
{
   DECLARED *pFirst = NULL, *pLast = NULL;

   while( LOOK_AHEAD_TOKEN() == TOKEN_IDENTIFIER )
   {
      DECLARED * pDeclared = New_DeclaredID( yylval.sText, Kind, Parser_pContext );

      DROP_AHEAD_TOKEN();

      if( pLast )
      {
         pLast->pNext = pDeclared;
         pLast = pDeclared;
      }
      else
      {
         pFirst = pDeclared;
         pLast  = pDeclared;
      }
      
      (*piDeclares)++;

      if( Kind == DECLARED_KIND_GLOBAL  ||
          Kind == DECLARED_KIND_LOCAL   || Kind == DECLARED_KIND_STATIC ||
          Kind == DECLARED_KIND_PRIVATE || Kind == DECLARED_KIND_PUBLIC ||
          Kind == DECLARED_KIND_LOCAL_PARAMETER )
      {
         if( LOOK_AHEAD_TOKEN() == TOKEN_INASSIGN )
         {
            DROP_AHEAD_TOKEN();
            pDeclared->Attribute.pInit = ParseValue( Parser_pContext );
         }
      }
      
      if( LOOK_AHEAD_TOKEN() == ',' )
      {
         DROP_AHEAD_TOKEN();
      }
      else if( LOOK_AHEAD_TOKEN() == TOKEN_IN && Kind == DECLARED_KIND_FIELD )
      {
         ID *pID;
         
         DROP_AHEAD_TOKEN();
         
         pID = New_ID( yylval.sText, Parser_pContext );
         
         if( LOOK_AHEAD_TOKEN() == TOKEN_IDENTIFIER )
         {
            pDeclared = pFirst;
            
            while( pDeclared )
            {
               pDeclared->Attribute.pAlias = pID;
               pDeclared = pDeclared->pNext;
            }
         }
      }
      else
      {
         break;
      }
   }

   return pFirst;
}

DECLARED * ParseDeclarationLine( PARSER_CONTEXT *Parser_pContext )
{
   FUNCTION *pFunction = Parser_pContext->Functions.pLast;
   DECLARED **ppDeclared;
   int      *piDeclared;
 
   switch( LOOK_AHEAD_TOKEN() )
   {
      case TOKEN_LOCAL :
         ppDeclared = &( pFunction->pLocals );
         piDeclared = &( pFunction->iLocals );
         break;
         
      case TOKEN_STATIC :
         ppDeclared = &( pFunction->pStatics );
         piDeclared = &( pFunction->iStatics );
         break;
         
      case TOKEN_MEMVAR :
         ppDeclared = &( pFunction->pMemvars );
         piDeclared = &( pFunction->iMemvars );
         break;
         
      case TOKEN_FIELD :
         ppDeclared = &( pFunction->pFields );
         piDeclared = &( pFunction->iFields );
         break;
         
      default:
         return NULL;
   }
   
   USE_AHEAD_TOKEN();
   
   while( *ppDeclared )
   {
      ppDeclared = &( ( *ppDeclared )->pNext );
   }

   *ppDeclared = ParseDeclaredList( (DECLARED_KIND) LAST_TOKEN(), piDeclared, Parser_pContext );
   
   ACCEPT_EOL()
   
   return *ppDeclared;
}

void ParseDeclarations( PARSER_CONTEXT *Parser_pContext )
{
   do
   {
       #ifdef GETTING_EMPTY_LINES
         while( LOOK_AHEAD_TOKEN() == '\n' )
         {
            DROP_AHEAD_TOKEN();
         }
       #endif
   
      if( ParseLine( PARSING_STATE_DECLARAIONS, Parser_pContext ) == NULL )
      {
         break;
      }
   }
   while( LAST_TOKEN() == '\n' );
}

LINE * ParseExecutableLine( PARSER_CONTEXT *Parser_pContext )
{
   LINE * pLine = NULL;
   VALUE * pValue = NULL;
   
#define GETTING_EMPTY_LINES
#ifdef GETTING_EMPTY_LINES
   while( LOOK_AHEAD_TOKEN() == '\n' )
   {
      DROP_AHEAD_TOKEN();
   }
#endif
   
   switch( LOOK_AHEAD_TOKEN() )
   {
      case TOKEN_IF :
         DROP_AHEAD_TOKEN();
         pLine = ParseIf( Parser_pContext );
         break;
         
      case TOKEN_RETURN :
         DROP_AHEAD_TOKEN();
         pLine = New_Line( ParseValue( Parser_pContext ), LINE_KIND_RETURN, Parser_pContext );
         break;
         
      case TOKEN_PRIVATE :
      case TOKEN_PUBLIC :
      case TOKEN_PARAMETERS :
         DROP_AHEAD_TOKEN();
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", TODO: Parse MEM Declares!" , Parser_pContext);
         break;
         
      default :
         pValue = ParseValue( Parser_pContext );
         
         if( pValue )
         {
            switch( pValue->Kind )
            {
               case VALUE_KIND_BINARY:
                  if( pValue->Value.pBinary->Kind == BINARY_KIND_EQUAL &&
                      ( pValue->Value.pBinary->pLeft->Kind & VALUE_KIND_ASSIGNABLE_MASK ) == VALUE_KIND_ASSIGNABLE_MASK )
                  {
                     ASSIGNMENT *pAssignment = New_Assignment( pValue->Value.pBinary->pLeft, pValue->Value.pBinary->pRight , ASSIGNMENT_KIND_NORMAL, Parser_pContext );
                     
                     pValue->Value.pBinary->pLeft = NULL;
                     pValue->Value.pBinary->pRight = NULL;
                     ClipNet_free( pValue->Value.pAssignment );
                     pValue->Value.pAssignment = NULL;
                     pValue->Kind = VALUE_KIND_NONE;
                     
                     //printf( "Binary EQUAL used as assignment\n" );
                     pLine = New_Line( (void *) pAssignment, LINE_KIND_ASSIGNMENT, Parser_pContext );
                  }
                  else
                  {
                     PARSE_ERROR( PARSER_ERR_SYNTAX, ClipNet_BinaryKind( pValue->Value.pBinary ), "Binary Kind can't be used as a statement.", Parser_pContext );
                  }
                  break;
                  
               case VALUE_KIND_UNARY:
                  pLine = New_Line( (void *) pValue->Value.pUnary, LINE_KIND_UNARY, Parser_pContext );
                  
                  pValue->Value.pUnary = NULL;
                  pValue->Kind = VALUE_KIND_NONE;
                  break;

               case VALUE_KIND_ASSIGNMENT:
                  pLine = New_Line( (void *) pValue->Value.pAssignment, LINE_KIND_ASSIGNMENT, Parser_pContext );

                  pValue->Value.pAssignment = NULL;
                  pValue->Kind = VALUE_KIND_NONE;
                  break;
                  
               case VALUE_KIND_IIF:
                  pLine = New_Line( (void *) pValue->Value.pIIF, LINE_KIND_IIF, Parser_pContext );

                  pValue->Value.pIIF = NULL;
                  pValue->Kind = VALUE_KIND_NONE;
                  break;
                                    
               case VALUE_KIND_FUNCTION_CALL:
                  pLine = New_Line( (void *) pValue->Value.pFunctionCall, LINE_KIND_FUNCTION_CALL, Parser_pContext );

                  pValue->Value.pFunctionCall = NULL;
                  pValue->Kind = VALUE_KIND_NONE;
                  break;
                  
               case VALUE_KIND_METHOD_CALL:
                  pLine = New_Line( (void *) pValue->Value.pMethodCall, LINE_KIND_METHOD_CALL, Parser_pContext );
                  
                  pValue->Value.pMethodCall = NULL;
                  pValue->Kind = VALUE_KIND_NONE;
                  break;
  
               default :
                  PARSE_ERROR( PARSER_ERR_SYNTAX, "Not a valid statement", ClipNet_ValueKind( pValue ), Parser_pContext );
            }
            
            Release_Value( pValue );
            pValue = NULL;
         }
   }
   
   ACCEPT_EOL();
   
   return pLine;
}

void * ParseLine( PARSING_STATE State, PARSER_CONTEXT *Parser_pContext )
{
   void *pResult;   
   int iError;
   
   Parser_pContext->sErrorSource = NULL;
   Parser_pContext->sError1 = NULL;
   Parser_pContext->sError2 = NULL;
  
   iError = setjmp( Parser_pContext->JumpBuffer );
   Parser_pContext->bCanJump = TRUE;
   
   if( iError == 0 )
   {
      switch( State )
      {
         case PARSING_STATE_EXECUTABLE:
            pResult = (void *) ParseExecutableLine( Parser_pContext );
            break;
            
         case PARSING_STATE_DEFINITION:
            pResult = ParseFunctionDefinitionLine( Parser_pContext );
            break;
            
         case PARSING_STATE_DECLARAIONS:
            pResult = (void *) ParseDeclarationLine( Parser_pContext );
            break;
            
         case PARSING_STATE_GLOBAL_DECLARATIONS:
            pResult = (void *) ParseGlobalDeclarationLine( Parser_pContext );
            break;
      }
   }
   else
   {
      // Must have arrived from longjump() in PARSE_ERROR()!!!
      Line_ParseError:
   
      Parser_GenError( Parser_asErrors, 'E', iError, Parser_pContext->sError1, Parser_pContext->sError2 , Parser_pContext );   
      printf( "%s\n", Parser_pContext->sErrorSource );
   
      while( LOOK_AHEAD_TOKEN() != -1 && LOOK_AHEAD_TOKEN() != '\n' )
      {
         DROP_AHEAD_TOKEN();
      }
      while( LOOK_AHEAD_TOKEN() == '\n' )
      {
         USE_AHEAD_TOKEN();
      }
      
      pResult = NULL;
   }
   
   Parser_pContext->bCanJump = FALSE;
   
   return pResult;
}

BODY * ParseBody( PARSER_CONTEXT *Parser_pContext )
{
   LINE *pLine = NULL;
   BODY *pBody = New_Body( Parser_pContext );
   
   //pBody->pLines = NULL;

   do
   {      
      if( EOB( LOOK_AHEAD_TOKEN() ) )
      {
         break;
      }
      
      if( pLine )
      {
         pLine->pNext = ParseLine( PARSING_STATE_EXECUTABLE, Parser_pContext );
         
         if( pLine->pNext )
         {
            pLine = pLine->pNext;
         }
         else
         {
            // We silently skip over the offensive line
         }
      }
      else
      {
         pLine = ParseLine( PARSING_STATE_EXECUTABLE, Parser_pContext );
         
         if( pLine )
         {
            pBody->pLines = pLine;
         }
         else
         {
            // We silently skip over the offensive line
         }
      }
   }
   while( LAST_TOKEN() == '\n' );

   #ifdef DEBUG_BODY
      printf( "\nBody ended with: %i\n", LOOL_AHEAD_TOKEN() );
   #endif

   return pBody;
}

LINE * ParseIf( PARSER_CONTEXT *Parser_pContext )
{
   BODY * pBody = NULL;
   LINE * pLine = NULL, *pLine2 = NULL, *pLine3 = NULL;
   VALUE * pValue = NULL;

   pValue = ParseValue( Parser_pContext );
   ACCEPT_EOL();

   pBody = ParseBody( Parser_pContext );

   pLine = New_If( pValue, pBody, Parser_pContext );

   if( LOOK_AHEAD_TOKEN() == TOKEN_ELSEIF )
   {
      DROP_AHEAD_TOKEN();
      
      pValue = ParseValue( Parser_pContext );
      ACCEPT_EOL();

      pBody = ParseBody( Parser_pContext );

      pLine2 = New_ElseIf( pValue, pBody, Parser_pContext );

      pLine->Value.pIf->pElseIf = pLine2;

      while( LOOK_AHEAD_TOKEN() == TOKEN_ELSEIF )
      {
         DROP_AHEAD_TOKEN();
         
         pValue = ParseValue( Parser_pContext );
         ACCEPT_EOL();

         pBody = ParseBody( Parser_pContext );

         pLine3 = New_ElseIf( pValue, pBody, Parser_pContext );

         pLine2->pNext = pLine3;
         pLine2 = pLine3;
      }                
   }

   if( LOOK_AHEAD_TOKEN() == TOKEN_ELSE )
   {
      DROP_AHEAD_TOKEN();
      
      ACCEPT_EOL();

      pBody = ParseBody( Parser_pContext );
      pLine->Value.pIf->pElse = New_Else( pBody, Parser_pContext );
   }

   ACCEPT_END( TOKEN_ENDIF );
   
   return pLine;
}

VALUE * ParseValue( PARSER_CONTEXT *Parser_pContext )
{
   VALUE *pValue = NULL, *pValue2 = NULL;
   BOOL bNegate = FALSE, bNot = FALSE;

  NextValue:

   switch( LOOK_AHEAD_TOKEN() )
   {
      case '+':
         DROP_AHEAD_TOKEN();

         goto NextValue;

      case '-':
         DROP_AHEAD_TOKEN();
         
         bNegate = ! bNegate;
         goto NextValue;

      case TOKEN_NOT:
         DROP_AHEAD_TOKEN();
         
         bNot = ! bNot;
         goto NextValue;
         
      case TOKEN_INC:         
      case TOKEN_DEC:
         USE_AHEAD_TOKEN();
         pValue = New_UnaryValue( ParseValue( Parser_pContext ), (UNARY_KIND) LAST_TOKEN(), UNARY_WHEN_PRE, Parser_pContext );
         
         break;

      case TOKEN_NIL:
         DROP_AHEAD_TOKEN();
         
         pValue = New_NILValue( Parser_pContext );
         
         break;

      case TOKEN_CONSTANT:
         DROP_AHEAD_TOKEN();
         
         pValue = New_ConstantValue( &( yylval.Constant ), Parser_pContext );
         
         break;

      case TOKEN_IDENTIFIER:
         DROP_AHEAD_TOKEN();
         
         pValue = New_IDValue( yylval.sText, VALUE_KIND_VARIABLE, Parser_pContext );
         
         break;

      case TOKEN_MACROVAR:
      case TOKEN_MACROTEXT:
         DROP_AHEAD_TOKEN();
         
         pValue = New_MacroValue( (void *)( yylval.sText ), MACRO_KIND_SIMPLE, Parser_pContext );
         break;

      case '&':
         DROP_AHEAD_TOKEN();
         
         pValue = New_MacroValue( (void *) ParseValue( Parser_pContext ), MACRO_KIND_COMPLEX, Parser_pContext );
         
         break;

      case TOKEN_IIF:
         DROP_AHEAD_TOKEN();
         
         if( LOOK_AHEAD_TOKEN() == '(' )
         {
            VALUE *pCond, *pTrue, *pFalse;

            DROP_AHEAD_TOKEN();
            
            pCond = ParseValue( Parser_pContext );

            if( pCond && LOOK_AHEAD_TOKEN() == ',' )
            {
               DROP_AHEAD_TOKEN();
               
               pTrue = ParseValue( Parser_pContext );

               if( pTrue && LOOK_AHEAD_TOKEN() == ',' )
               {
                  DROP_AHEAD_TOKEN();
                  
                  pFalse = ParseValue( Parser_pContext );

                  if( pFalse && LOOK_AHEAD_TOKEN() == ')' )
                  {
                     DROP_AHEAD_TOKEN();
                     
                     pValue = New_IIFValue( pCond, pTrue, pFalse, Parser_pContext );
                     
                     ClipNet_free( pFalse );
                     
                     break;
                  }
                  
                  ClipNet_free( pTrue );
               }
               
               ClipNet_free( pCond );
            }
         }

         PARSE_ERROR( PARSER_ERR_SYNTAX, "Invalid IIF()syntax", yytext, Parser_pContext );
         
         break;
         
      case '(' :
         DROP_AHEAD_TOKEN();
         
         //printf( "LIST\n" );
         if( ( pValue = ParseListAsValue( PRG_TYPE_ANY, Parser_pContext ) ) )
         {
            ACCEPT_TOKEN_AND_BREAK( ')' );
         }
         
         PARSE_ERROR( PARSER_ERR_SYNTAX, "Invalid List syntax", yytext, Parser_pContext );

         if( pValue )
         {
            Release_Value( pValue );
            pValue = NULL;
         }
         
         break;

      case '{' :         
         DROP_AHEAD_TOKEN();
         
         if( LOOK_AHEAD_TOKEN() == TOKEN_CBMARKER )
         {
            BLOCK *pBlock;

            DROP_AHEAD_TOKEN();
            
            pValue = New_BlockValue( Parser_pContext );
            pBlock = pValue->Value.pBlock;

            if( LOOK_AHEAD_TOKEN() != TOKEN_CBMARKER )
            {
               //printf( "Parse Block params...\n" );
               pBlock->pBlockLocals = ParseDeclaredList( DECLARED_KIND_LOCAL_PARAMETER, &pBlock->iBlockLocals, Parser_pContext );
               
               if( pBlock->pBlockLocals == NULL )
               {
                  
                  Release_Value( pValue );
                  pValue = NULL;
                  
                  PARSE_ERROR( PARSER_ERR_BLOCK, "Invalid Codeblock Parametrs", yytext, Parser_pContext );
                  break;
               }
            }
                                                        
            ACCEPT_TOKEN( TOKEN_CBMARKER );
            
            pBlock->pList = ParseList( PRG_TYPE_ANY, Parser_pContext );
            
            if( pBlock->pList )
            {
               ACCEPT_TOKEN_AND_BREAK( '}' );
            }
            else
            {
               PARSE_ERROR( PARSER_ERR_BLOCK, "Invalid Codeblock expression", yytext, Parser_pContext );
            }
         }
         else
         {
            if( ( pValue = ParseListAsValue( PRG_TYPE_ANY, Parser_pContext ) ) != NULL )
            {
               ACCEPT_TOKEN_AND_BREAK( '}' );
            }
            
            PARSE_ERROR( PARSER_ERR_SYNTAX, "Invalid Array syntax", yytext, Parser_pContext );
         }
         
         Release_Value( pValue );
         pValue = NULL;
         
         break;
         
      default:
         #ifdef DEBUG_VALUE
            printf( "Not supported: %i\n", LOOK_AHEAD_TOKEN() );
         #endif

         goto ParseValue_Done;
   }

   if( pValue )
   {
      pValue->bNegate = bNegate;
      pValue->bNot    = bNot;
   }
   else
   {
      goto ParseValue_Done;
   }
   
  TryExtendedValue :

   #ifdef DEBUG_EXTEND
      printf( "Extend Kind: %i before Token: %i %s\n", pValue->Kind, LOOK_AHEAD_TOKEN(), yytext );
   #endif

   switch( LOOK_AHEAD_TOKEN() )
   {
      case BINARY_KIND_POWER :
         
      case BINARY_KIND_MODULUS :
      case BINARY_KIND_MULTIPLY :
      case BINARY_KIND_DIVIDE :
         
      case BINARY_KIND_PLUS :
      case BINARY_KIND_MINUS :
         
      case BINARY_KIND_BITLEFT :
      case BINARY_KIND_BITRIGHT :
         
      case BINARY_KIND_IN :
      case BINARY_KIND_LIKE :
      case BINARY_KIND_HAS :
      case BINARY_KIND_GREATEREQUAL :
      case BINARY_KIND_LESSEREQUAL :
      case BINARY_KIND_GREATER :
      case BINARY_KIND_LESSER :
         
      case BINARY_KIND_EXACTEQUAL :
      case BINARY_KIND_EQUAL :
      case BINARY_KIND_NOTEQUAL :
         
      case BINARY_KIND_BITOR :
      case BINARY_KIND_BITXOR :
      case BINARY_KIND_BITAND :
         
      case BINARY_KIND_AND :
      case BINARY_KIND_OR :
      {
         BINARY_KIND Kind;
         
         USE_AHEAD_TOKEN();
         Kind = (BINARY_KIND) LAST_TOKEN();

         pValue2 = ParseValue( Parser_pContext );

         if( pValue2 )
         {
            pValue = New_BinaryValue( pValue, pValue2, Kind, Parser_pContext );
            //goto TryExtendedValue; //Right side will consume all!
         }
         else
         {
            PARSE_ERROR( PARSER_ERR_SYNTAX, "Invalid Right Side Expression", yytext, Parser_pContext );
         }
         
         break;
      }
         
      case TOKEN_ALIAS :
         DROP_AHEAD_TOKEN();
         
         if( 
             pValue->Kind == VALUE_KIND_VARIABLE &&
             (
                ( pValue->Value.pVariable->pID->Name[0] == 'M' && pValue->Value.pVariable->pID->Name[1] == '\0' )
                ||
                (
                   strncmp( pValue->Value.pVariable->pID->Name, "MEMVAR", 4 ) == 0 &&
                   strncmp( pValue->Value.pVariable->pID->Name, "MEMVAR", strlen( pValue->Value.pVariable->pID->Name ) ) == 0
                )
                ||
                (
                   strncmp( pValue->Value.pVariable->pID->Name, "FIELD", 4 ) == 0 &&
                   strncmp( pValue->Value.pVariable->pID->Name, "FIELD", strlen( pValue->Value.pVariable->pID->Name ) ) == 0
                )
             )
           )
         {
            VALUE_KIND Kind;
            
            if( pValue->Value.pVariable->pID->Name[0] == 'M' )
            {
               Kind = VALUE_KIND_MEMVAR;
            }
            else
            {
               Kind = VALUE_KIND_FIELD;
            }
            
            switch (LOOK_AHEAD_TOKEN() )
            {                  
               case TOKEN_IDENTIFIER:
                  DROP_AHEAD_TOKEN();
                  
                  Release_Value( pValue );
                  pValue = New_IDValue( yylval.sText, Kind, Parser_pContext ) ;
                  break;

               case TOKEN_MACROVAR:
               case TOKEN_MACROTEXT:
                  DROP_AHEAD_TOKEN();
                  
                  Release_Value( pValue );
            
                  pValue = New_MacroValue( (void *)( yylval.sText ), MACRO_KIND_SIMPLE, Parser_pContext );
                  pValue->Value.pMacro->Resolution = (MACRO_RESOLUTION) Kind;
                  break;
                  
               case '&':
                  DROP_AHEAD_TOKEN();
                  
                  Release_Value( pValue );
                  pValue = NULL;
                  
                  if( LOOK_AHEAD_TOKEN() == '(' )
                  {
                     pValue = New_MacroValue( (void *) ParseValue( Parser_pContext ), MACRO_KIND_COMPLEX, Parser_pContext );
                     pValue->Value.pMacro->Resolution = (MACRO_RESOLUTION) Kind;
                  }
                  else
                  {
                     PARSE_ERROR( PARSER_ERR_BAD_MACRO, "& Followed by invalid expression", yytext, Parser_pContext );
                  }
                  
               default:
                  break;
            }
         }
         else
         {
            pValue2 = ParseValue( Parser_pContext );
            
            if( pValue2 )
            {
               pValue = New_AliasedValue( pValue, pValue2, Parser_pContext );
            }
            else
            {
               PARSE_ERROR( PARSER_ERR_INVALID_ALIAS, "Invalid Aliased expression", yytext, Parser_pContext );
            }
         }
         
         goto TryExtendedValue;

      //printf( "FUNCTION CALL\n" );
      case '(' :
         DROP_AHEAD_TOKEN();
         
         if( LOOK_AHEAD_TOKEN() == ')' )
         {
            ACCEPT_TOKEN( ')' );
            pValue = New_FunctionCallValue( pValue, NULL, Parser_pContext );
         }
         else
         {
            if( ( pValue2 = ParseListAsValue( PRG_TYPE_ANY, Parser_pContext ) ) != NULL )
            {
               ACCEPT_TOKEN( ')' );
               pValue = New_FunctionCallValue( pValue, pValue2, Parser_pContext );
            }
         }
               
         goto TryExtendedValue;

      case '[' :
      {
         LIST *pList;
         
         DROP_AHEAD_TOKEN();
         
         //printf( "ARRAY INDEX\n" );
         if( ( pList = ParseList( (PRG_TYPE) ( PRG_TYPE_NUMERIC | PRG_TYPE_STRING ), Parser_pContext ) ) != NULL )
         {
            ACCEPT_TOKEN( ']' );
            pValue = New_ArrayElementValue( pValue, pList, Parser_pContext );
         }

         goto TryExtendedValue;
      }
         
      case TOKEN_INASSIGN :
      case TOKEN_PLUSEQ : 
      case TOKEN_MINUSEQ :
      case TOKEN_MULTEQ :
      case TOKEN_DIVEQ :
      case TOKEN_POWER :
      case TOKEN_EXPEQ :
      case TOKEN_MODEQ :
      {
         ASSIGNMENT_KIND Kind;
      
         USE_AHEAD_TOKEN();
         Kind = (ASSIGNMENT_KIND) LAST_TOKEN();
         
         pValue2 = ParseValue( Parser_pContext );

         if( pValue2 )
         {
            pValue = New_AssignmentValue( pValue, pValue2, Kind, Parser_pContext );

            #ifdef DEBUG_INASSIGN
               printf( "Assignee Kind: %i ASSIGNED %p\n", pValue->Value.pAssignment->pLValue->Kind, pValue->Value.pAssignment->pValue );
            #endif
         }
         else
         {
            PARSE_ERROR( PARSER_ERR_SYNTAX, "Invalid assignment value", yytext, Parser_pContext );
         }
         
         //goto TryExtendedValue; //Right side will consume all!
         break;
      }
         
      case TOKEN_INC :
      case TOKEN_DEC :
         USE_AHEAD_TOKEN();
         pValue = New_UnaryValue( pValue, (UNARY_KIND) LAST_TOKEN(), UNARY_WHEN_POST, Parser_pContext );
         
         goto TryExtendedValue;

     #ifdef DEBUG_VALUE
      default:
            printf( "Not an Extender Tokens: %i\n", LOOK_AHEAD_TOKEN() );
     #endif
   }

  ParseValue_Done :

   #ifdef DEBUG_VALUE
      if( pValue )
      {
         printf( "Returning Value Kind: %i\n", pValue->Kind );
      }
      else
      {
         printf( "Returning INVALID Value\n" );      
      }
   #endif

   return pValue;
}

VALUE * ParseListAsValue( PRG_TYPE Type, PARSER_CONTEXT *Parser_pContext )
{
   LIST *pList = ParseList( Type, Parser_pContext );

   if( pList )
   {
      return New_Value( (void *) pList, VALUE_KIND_LIST, Parser_pContext );
   }
   else
   {
      // TODO: Review!
      //PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid list." );
   }
   
   return NULL;
}

LIST * ParseList( PRG_TYPE Type, PARSER_CONTEXT *Parser_pContext )
{
   LIST *pList = NULL;
   VALUE *pValue;

  NextNode :

   pValue = ParseValue( Parser_pContext );

   if( pValue )
   {
      if( ( Type | pValue->Type ) != Type )
      {
         if( pList )
         { 
            Release_List( pList );
            pList = NULL;
         }

         printf( "Wanted Type: %i Got Kind: %i Type: %i\n", Type, pValue->Kind, pValue->Type );
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid type of list member.", Parser_pContext );
         return NULL;
         //goto ParseList_Next;
      }
   }
   else
   {
      if( Type == PRG_TYPE_UNDEF )
      {
         pValue = New_NILValue( Parser_pContext );
      }
      else
      {
         if( pList )
         {
            Release_List( pList );
            pList = NULL;
         }

        return NULL;
      }
   }

   if( pList == NULL )
   {
      pList = New_List( pValue, Parser_pContext );
   }
   else
   {
      New_ListNode( pList, pValue, Parser_pContext );
   }

  ParseList_Next :

   if( LOOK_AHEAD_TOKEN() == ',' )
   {
      DROP_AHEAD_TOKEN();
      goto NextNode;
   }

   return pList;
}
 
INLINE * Parser_InlineAdd( char *sName, PARSER_CONTEXT *Parser_pContext )
{
   INLINE *pInline;

   New_Function( sName, FUNC_KIND_FUNC_STATIC, Parser_pContext );

   pInline = New_Inline( sName, Parser_pContext );

   if( Parser_pContext->Inlines.iCount == 0 )
   {
      Parser_pContext->Inlines.pFirst = pInline;
      Parser_pContext->Inlines.pLast  = pInline;
   }
   else
   {
      Parser_pContext->Inlines.pLast->pNext = pInline;
      Parser_pContext->Inlines.pLast = pInline;
   }

   Parser_pContext->Inlines.iCount++;

   return pInline;
}
