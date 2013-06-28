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

void       ParseFunction( PARSER_CONTEXT *Parser_pContext );
DECLARED * ParseDeclaredList( DECLARED_KIND Kind, int *piParams, TOKEN_ID Terminator, PARSER_CONTEXT *Parser_pContext );
void       ParseDeclarations( PARSER_CONTEXT *Parser_pContext );
BODY *     ParseBody( LINE * pPrevLine, PARSER_CONTEXT *Parser_pContext );
LINE *     ParseIf( PARSER_CONTEXT *Parser_pContext );
VALUE *    ParseValue( PARSER_CONTEXT *Parser_pContext );
VALUE *    ParseList( PRG_TYPE Type, PARSER_CONTEXT *Parser_pContext );

YYSTYPE   yylval;
extern char *yytext;

#if 1
#else
void DumpBody( BODY * pBody, int *piSpaces, PARSER_CONTEXT *Parser_pContext );
void DumpLine( LINE * pLine, int *piSpaces, PARSER_CONTEXT *Parser_pContext );
#endif

int main( int argc, char *argv[] )
{
   PARSER_CONTEXT *Parser_pContext = New_Context();
   FILE *hSource;

#if 1
#else
   FUNCTION *pFunc;
   int iSpaces = 0;
#endif
   
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

      Parser_pContext->Parser_Files.iFiles = 1;
      Parser_pContext->Parser_Files.pLast = pSource;
 
      Reducer_Init();

      while( NEXT_TOKEN() > 0 )
      {
         ParseFunction( Parser_pContext );
      }
    
#if 1
      IterateAST( Parser_pContext );
#else
      pFunc = Parser_pContext->Parser_Functions.pFirst;
    
      while( pFunc )
      {         
         printf( "\nFunction: %s LocalParams: %i Locals %i, Statics %i, Memvars %i, Fields %i, Params: %i Privates: %i Publics: %i\n", pFunc->pName->Name, pFunc->iLocalParams, pFunc->iLocals, pFunc->iStatics, pFunc->iMemvars, pFunc->iFields, pFunc->iParams, pFunc->iPrivates, pFunc->iPublics );

         DumpBody( pFunc->pBody, &iSpaces, Parser_pContext );

         pFunc = pFunc->pNext;
      }
#endif
      
      fclose( hSource );
 
      pSource = Parser_pContext->Parser_Files.pLast;
 
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

#if 1
#else
void DumpBody( BODY * pBody, int *piSpaces, PARSER_CONTEXT *Parser_pContext )
{
   if( pBody && pBody->pLines )
   {
      LINE * pLine = pBody->pLines;

      do
      {
         DumpLine( pLine, piSpaces, Parser_pContext );

         pLine = pLine->pNext;
      }
      while( pLine );
   }
}

void DumpLine( LINE * pLine, int *piSpaces, PARSER_CONTEXT *Parser_pContext )
{
   char *sSpacer;

   sSpacer = (char *) malloc( *piSpaces + 1 );
   memset( (void *) sSpacer, ' ', *piSpaces );
   sSpacer[ *piSpaces ] = '\0';

   printf( "%sLine kind: %s\n", sSpacer, ClipNet_LineKind( pLine, Parser_pContext ) );

   if( pLine->Kind == LINE_KIND_IF )
   {
      LINE *pIF = pLine;

      *piSpaces += 3;

      DumpBody( pLine->Value.pIf->pBody, piSpaces, Parser_pContext );

      pLine = pLine->Value.pIf->pElseIf;
      while( pLine )
      {
         printf( "%sLine kind: %s\n", sSpacer, ClipNet_LineKind( pLine, Parser_pContext ) );
         DumpBody( pLine->Value.pElseIf->pBody, piSpaces, Parser_pContext );
         pLine = pLine->pNext;
      }

      pLine = pIF->Value.pIf->pElse;
      if( pLine )
      {
         printf( "%sLine kind: %s\n", sSpacer, ClipNet_LineKind( pLine, Parser_pContext ) );
         DumpBody( pLine->Value.pElse->pBody, piSpaces, Parser_pContext );
      }

      printf( "%sLine kind: %s\n", sSpacer, "ENDIF" );

      *piSpaces -= 3;
   }

   free( (void *) sSpacer );
}
#endif

VALUE * Flag_AsMemvar( VALUE * pValue, PARSER_CONTEXT *Parser_pContext )
{
   if( pValue->Kind == VALUE_KIND_LVALUE )
   {
      LVALUE *pLValue = pValue->Value.pLValue;
      
      switch( pLValue->Kind )
      {
         case LVALUE_KIND_VARIABLE :
            pLValue->Value.pVariable->Kind = DECLARED_KIND_MEMVAR;
            break;

         case LVALUE_KIND_MACRO :
            // Macro Identifier may also refer to FIELD.
            pLValue->Value.pVariable->Kind = DECLARED_KIND_MEMVAR;
            break;

         default :
            PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid MEMVAR expression. (how come???)" );      
      }
   }
   else
   {
      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid MEMVAR expression." );
   }

   return pValue;
}

void ParseFunction( PARSER_CONTEXT *Parser_pContext )
{
   FUNCTION * pFunc;
   FUNC_KIND FuncKind = (FUNC_KIND) LAST_TOKEN();

   if( FuncKind >= FUNC_KIND_MIN && FuncKind <= FUNC_KIND_MAX )
   {
      if( NEXT_TOKEN() == TOKEN_IDENTIFIER )
      {
         pFunc = New_Function( yylval.sText, FuncKind, Parser_pContext );

         if( NEXT_TOKEN() == '(' )
         {
            pFunc->pLocalParameters = ParseDeclaredList( DECLARED_KIND_LOCAL_PARAMETER, &( pFunc->iLocalParameters ), (TOKEN_ID) ')', Parser_pContext );
            (void) NEXT_TOKEN();
         }

         EXPECTED_EOL();

         ParseDeclarations( Parser_pContext );

         pFunc->pBody = ParseBody( NULL, Parser_pContext );

         return;
      }
   }

   Error:

      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected entity definition." );

   return;
}
  
DECLARED * ParseDeclaredList( DECLARED_KIND Kind, int *piParams, TOKEN_ID Terminator, PARSER_CONTEXT *Parser_pContext )
{
   DECLARED *pFirst = NULL, *pLast = NULL;

   while( LOOK_AHEAD_TOKEN() == TOKEN_IDENTIFIER )
   {
      DECLARED * pParameter = New_DeclaredID( yylval.sText, Kind, Parser_pContext );

      DROP_AHEAD_TOKEN();

      if( pLast )
      {
         pLast->pNext = pParameter;
         pLast = pParameter;
      }
      else
      {
         pFirst = pParameter;
         pLast  = pParameter;
      }
      (*piParams)++;


      if( LOOK_AHEAD_TOKEN() == TOKEN_INASSIGN )
      {
         DROP_AHEAD_TOKEN();
         pParameter->pInit = ParseValue( Parser_pContext );
      }
      
      if( LOOK_AHEAD_TOKEN() == ',' )
      {
         DROP_AHEAD_TOKEN();
      }
      else
      {
         break;
      }
   }

   if( LOOK_AHEAD_TOKEN() == Terminator )
   {
      DROP_AHEAD_TOKEN();
   }
   else
   {
      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected terminator." );
      //TODO Release Memory!
      return NULL;
   }

   return pFirst;
}

void ParseDeclarations( PARSER_CONTEXT *Parser_pContext )
{
   FUNCTION *pFunction = Parser_pContext->Parser_Functions.pLast;
   DECLARED **ppDeclared;
   int      *piDeclared;

   Again:
   
      switch( NEXT_TOKEN() )
      {
         case TOKEN_LOCAL :
            ppDeclared = &( pFunction->pLocals );
            piDeclared = &( pFunction->iLocals );
         case TOKEN_STATIC :
            ppDeclared = &( pFunction->pStatics );
            piDeclared = &( pFunction->iStatics );
         case TOKEN_MEMVAR :
            ppDeclared = &( pFunction->pMemvars );
            piDeclared = &( pFunction->iMemvars );
         case TOKEN_FIELD :
            DROP_AHEAD_TOKEN();
         
            ppDeclared = &( pFunction->pFields );
            piDeclared = &( pFunction->iFields );
         
            *ppDeclared = ParseDeclaredList( LAST_TOKEN(), piDeclared, (TOKEN_ID) '\n', Parser_pContext );
            break;

         default:
            #ifdef GETTING_EMPTY_LINES
               while( LOOK_AHEAD_TOKEN() == '\n' )
               {
                  DROP_AHEAD_TOKEN();
               }
            #endif
            
            if( LAST_TOKEN() == '\n' )
            {
               goto Again;
            }
      }
}

LINE * ParseLine( PARSER_CONTEXT *Parser_pContext )
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
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", TODO: Parse MEM Declares!" );
         break;
         
      default :
         pValue = ParseValue( Parser_pContext );
         
         if( pValue )
         {
            switch( pValue->Kind )
            {
               case VALUE_KIND_ASSIGNMENT:
                  pLine = New_Line( (void *) pValue, LINE_KIND_ASSIGNMENT, Parser_pContext );
                  break;
                  
               case VALUE_KIND_BINARY:
                  if( pValue->Value.pBinary->Kind == BINARY_KIND_EQUAL && pValue->Value.pBinary->pLeft->Kind == VALUE_KIND_LVALUE )
                  {
                     //printf( "Binary EQUAL used as assignment\n" );
                     pLine = New_Line( (void *) pValue, LINE_KIND_ASSIGNMENT, Parser_pContext );
                  }
                  else
                  {
                     PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", Value can't be used as a statement." );
                     Release_Value( pValue );
                     pValue = NULL;
                  }
                  break;
                  
               case VALUE_KIND_UNARY:
                  pLine = New_Line( (void *) pValue, LINE_KIND_UNARY, Parser_pContext );
                  break;
                  
               case VALUE_KIND_FUNC_CALL:
                  pLine = New_Line( (void *) pValue, LINE_KIND_FUNC_CALL, Parser_pContext );
                  break;
                  
               case VALUE_KIND_IIF:
                  pLine = New_Line( (void *) pValue, LINE_KIND_IIF, Parser_pContext );
                  break;
                  
               case VALUE_KIND_METHOD_CALL:
                  pLine = New_Line( (void *) pValue, LINE_KIND_METHOD_CALL, Parser_pContext );
                  break;
  
               default :
                  printf( "Kind: %i\n", pValue->Kind );
                  PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", Value can't be used as a statement." );
                  Release_Value( pValue );
                  pValue = NULL;
            }
         }
   }
   
   ACCEPT_EOL();
   
   return pLine;
}

BODY * ParseBody( LINE * pPrevLine, PARSER_CONTEXT *Parser_pContext )
{
   BODY *pBody = New_Body();
   LINE *pLine = NULL;
   BOOL bEOB = FALSE;
  
   pBody->pLines = NULL;

   do
   {      
      if( EOB( LOOK_AHEAD_TOKEN() ) )
      {
         if( Parser_pContext->Parser_iNextToken == TOKEN_END ||
             Parser_pContext->Parser_iNextToken == TOKEN_ENDCASE ||
             Parser_pContext->Parser_iNextToken == TOKEN_ENDDO ||
             Parser_pContext->Parser_iNextToken == TOKEN_ENDIF ||
             Parser_pContext->Parser_iNextToken == TOKEN_NEXT )
         {
            // Eat it :-)
            DROP_AHEAD_TOKEN();
         }
         
         bEOB = TRUE;
         break;
      }
      
      pLine = ParseLine( Parser_pContext );
      
      if( pBody->pLines == NULL )
      {
         pBody->pLines = pLine;
      }

      if( pPrevLine )
      {
         pPrevLine->pNext = pLine;
      }

      pPrevLine = pLine;
   } 
   while( TRUE );

   #ifdef DEBUG_BODY
      printf( "\nBody ended with: %i\n", LAST_TOKEN() );
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

   pBody = ParseBody( NULL, Parser_pContext );

   pLine = New_If( pValue, pBody, Parser_pContext );

   if( LOOK_AHEAD_TOKEN() == TOKEN_ELSEIF )
   {
      DROP_AHEAD_TOKEN();
      
      pValue = ParseValue( Parser_pContext );
      ACCEPT_EOL();

      pBody = ParseBody( NULL, Parser_pContext );

      pLine2 = New_ElseIf( pValue, pBody, Parser_pContext );

      pLine->Value.pIf->pElseIf = pLine2;

      while( LOOK_AHEAD_TOKEN() == TOKEN_ELSEIF )
      {
         DROP_AHEAD_TOKEN();
         
         pValue = ParseValue( Parser_pContext );
         ACCEPT_EOL();

         pBody = ParseBody( NULL, Parser_pContext );

         pLine3 = New_ElseIf( pValue, pBody, Parser_pContext );

         pLine2->pNext = pLine3;
         pLine2 = pLine3;
      }                
   }

   if( LOOK_AHEAD_TOKEN() == TOKEN_ELSE )
   {
      DROP_AHEAD_TOKEN();
      
      ACCEPT_EOL();

      pBody = ParseBody( NULL, Parser_pContext );
      pLine->Value.pIf->pElse = New_Else( pBody, Parser_pContext );
   }

   ACCEPT_EOS( TOKEN_ENDIF );
   
   return pLine;
}

VALUE * ParseValue( PARSER_CONTEXT *Parser_pContext )
{
   VALUE *pValue = NULL, *pValue2 = NULL;
   BOOL bNegate = FALSE;

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

      case TOKEN_INC:         
      case TOKEN_DEC:
         DROP_AHEAD_TOKEN();
         
         pValue = New_Unary( ParseValue( Parser_pContext ), (UNARY_KIND) LAST_TOKEN(), UNARY_WHEN_PRE, Parser_pContext );
         break;

      case TOKEN_NIL:
         DROP_AHEAD_TOKEN();
         
         pValue = New_NIL( Parser_pContext );
         break;

      case TOKEN_CONSTANT:
         DROP_AHEAD_TOKEN();
         
         pValue = New_Constant( &( yylval.Constant ), Parser_pContext );
         break;

      case TOKEN_IDENTIFIER:
         DROP_AHEAD_TOKEN();
         
         pValue = New_LValueID( yylval.sText, DECLARED_KIND_NONE, Parser_pContext );
         break;

      case TOKEN_MACROVAR:
      case TOKEN_MACROTEXT:
         DROP_AHEAD_TOKEN();
         
         pValue = New_Macro( yylval.sText, MACRO_KIND_SIMPLE, Parser_pContext );
         break;

      case '&':
         DROP_AHEAD_TOKEN();
         
         pValue = New_Macro( ParseValue( Parser_pContext ), MACRO_KIND_COMPLEX, Parser_pContext );
         break;

      case TOKEN_IIF:
         DROP_AHEAD_TOKEN();
         
         if( LOOK_AHEAD_TOKEN() == '(' )
         {
            VALUE *pCond, *pTrue, *pFalse;

            DROP_AHEAD_TOKEN();
            
            pCond = ParseValue( Parser_pContext );

            if( pCond && NEXT_TOKEN() == ',' )
            {
               pTrue = ParseValue( Parser_pContext );

               if( pTrue && NEXT_TOKEN() == ',' )
               {
                  pFalse = ParseValue( Parser_pContext );

                  if( pFalse && NEXT_TOKEN() == ')' )
                  {
                     pValue = New_IIF( pCond, pTrue, pFalse, Parser_pContext );
                     break;
                  }

                  if( pFalse )
                  {
                     ClipNet_free( pFalse );
                  }
               }

               if( pTrue )
               {
                  ClipNet_free( pTrue );
               }
            }

            if( pCond )
            {
               ClipNet_free( pCond );
            }

            PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid IIF syntax." );
         }
   
      case '(' :
         DROP_AHEAD_TOKEN();
         
         //printf( "LIST\n" );
         if( ( pValue = ParseList( (PRG_TYPE) 0xFF, Parser_pContext ) ) != NULL )
         {
            ACCEPT_TOKEN( ')' );
         }
         break;

      case '{' :         
         DROP_AHEAD_TOKEN();
         
         if( LOOK_AHEAD_TOKEN() == TOKEN_CBMARKER )
         {
            BLOCK *pBlock;

            DROP_AHEAD_TOKEN();
            
            pValue = New_Block( Parser_pContext );
            pBlock = pValue->Value.pBlock;

            //printf( "Parse Block params...\n" );

            if( ( pBlock->pBlockLocals = ParseDeclaredList( DECLARED_KIND_LOCAL_PARAMETER, &pBlock->iBlockLocals, TOKEN_CBMARKER, Parser_pContext ) ) != NULL )
            {
               printf( "Parse Block list...\n" );

               if( ( pBlock->pList = ParseList( (PRG_TYPE) 0xFF, Parser_pContext ) ) != NULL )
               {
                  printf( "Block list: %p Nodes: %i\n", pBlock->pList, pBlock->pList->Value.pList->iNodes );
                  ACCEPT_TOKEN( '}' );
               }
            }      
         }
         else
         {
            //printf( "LITERAL ARRAY token: %i\n", LAST_TOKEN() );

            if( ( pValue = ParseList( (PRG_TYPE) 0xFF, Parser_pContext ) ) != NULL )
            {
               ACCEPT_TOKEN( '}' );
            }
         }
         break;

      default:
         #ifdef DEBUG_VALUE
            printf( "Not supported: %i\n", LOOK_AHEAD_TOKEN() );
         #endif

         goto ParseValue_Done;
   }

  TryExtendedValue :

   #ifdef DEBUG_EXTEND
      printf( "Extend Kind: %i after Last Token: %i %s\n", pValue->Kind, LAST_TOKEN(), yytext );
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
         DROP_AHEAD_TOKEN();
         
         {
            BINARY_KIND Kind = (BINARY_KIND) LAST_TOKEN();

            pValue2 = ParseValue( Parser_pContext );

            pValue = New_Binary( pValue, pValue2, Kind, Parser_pContext );
            goto TryExtendedValue;
         }
   
      case TOKEN_ALIAS :
         DROP_AHEAD_TOKEN();
         
         pValue2 = ParseValue( Parser_pContext );

         if(
             pValue->Kind == VALUE_KIND_LVALUE && pValue->Value.pLValue->Kind == LVALUE_KIND_VARIABLE &&
                             (
                               (
                                  pValue->Value.pLValue->Value.pVariable->pID->Name[0] == 'M' &&
                                  pValue->Value.pLValue->Value.pVariable->pID->Name[1] == '\0'
                               )
                               ||
                               (
                                  strncmp( pValue->Value.pLValue->Value.pVariable->pID->Name, "MEMVAR", 4 ) == 0 &&
                                  strncmp( pValue->Value.pLValue->Value.pVariable->pID->Name, "MEMVAR", strlen( pValue->Value.pLValue->Value.pVariable->pID->Name ) ) == 0
                               )
                            )
           )
         {
            //printf( "MEMVAR\n" );
            Release_Value( pValue );
            pValue = Flag_AsMemvar( pValue2, Parser_pContext );
         }
         else
         {
            //printf( "ALIAS\n" );
            pValue = New_Aliased( pValue, pValue2, Parser_pContext );
         }
         goto TryExtendedValue;

      case '(' :
         DROP_AHEAD_TOKEN();
         
         //printf( "FUNCTION CALL\n" );
         if( ( pValue2 = ParseList( PRG_TYPE_UNDEF, Parser_pContext ) ) != NULL )
         {
            ACCEPT_TOKEN( ')' );
            pValue = New_FuncCall( pValue, pValue2, Parser_pContext );
         }

         goto TryExtendedValue;

      case '[' :
         DROP_AHEAD_TOKEN();
         
         //printf( "ARRAY INDEX\n" );
         if( ( pValue2 = ParseList( (PRG_TYPE) ( PRG_TYPE_NUMERIC | PRG_TYPE_STRING ), Parser_pContext ) ) != NULL )
         {
            ACCEPT_TOKEN( ']' );
            pValue = New_ArrayElement( pValue, pValue2, Parser_pContext );
         }

         goto TryExtendedValue;

      case TOKEN_INASSIGN :
      case TOKEN_PLUSEQ : 
      case TOKEN_MINUSEQ :
      case TOKEN_MULTEQ :
      case TOKEN_DIVEQ :
      case TOKEN_POWER :
      case TOKEN_EXPEQ :
      case TOKEN_MODEQ :
         DROP_AHEAD_TOKEN();
         
         pValue2 = ParseValue(Parser_pContext );

         pValue = New_Assignment( pValue, pValue2, (ASSIGNMENT_KIND) LAST_TOKEN(), Parser_pContext );

         #ifdef DEBUG_INASSIGN
            printf( "LValue Kind: %i ASSIGNED %p\n", pValue->Value.pAssignment->pLValue->Value.pLValue->Kind, pValue->Value.pAssignment->pValue );
         #endif

         //goto TryExtendedValue; //Right side will consume all!
         break;

      case TOKEN_INC :
      case TOKEN_DEC :
         DROP_AHEAD_TOKEN();
         
         pValue = New_Unary( pValue, (UNARY_KIND) LAST_TOKEN(), UNARY_WHEN_POST, Parser_pContext );
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

VALUE * ParseList( PRG_TYPE Type, PARSER_CONTEXT *Parser_pContext )
{
   LIST *pList = NULL;
   VALUE *pValue;

  NextNode :

   pValue = ParseValue( Parser_pContext );

   if( pValue )
   {
      if( Type && ! Type & pValue->Type )
      {
         if( pList )
         { 
            Release_List( pList );
            pList = NULL;
         }

         printf( "Wanted Type: %i Got Kind: %i Type: %i\n", Type, pValue->Kind, pValue->Type );
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid type of list member." );
         return NULL;
         //goto ParseList_Next;
      }
   }
   else
   {
      if( Type == PRG_TYPE_UNDEF )
      {
         pValue = New_NIL( Parser_pContext );
      }
      else
      {
         if( pList )
         {
            Release_List( pList );
            pList = NULL;
         }

        PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid list member." );

        return NULL;
        //goto ParseList_Next;
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

   if( pList )
   {
      return New_Value( (void *) pList, VALUE_KIND_LIST, Parser_pContext );
   }
   else
   {
      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid list." );
   }

   return NULL;
}
 
INLINE * Parser_InlineAdd( char *sName, PARSER_CONTEXT *Parser_pContext )
{
   INLINE *pInline;

   New_Function( sName, FUNC_KIND_FUNC_STATIC, Parser_pContext );

   pInline = New_Inline( sName, Parser_pContext );

   if( Parser_pContext->Parser_Inlines.iCount == 0 )
   {
      Parser_pContext->Parser_Inlines.pFirst = pInline;
      Parser_pContext->Parser_Inlines.pLast  = pInline;
   }
   else
   {
      Parser_pContext->Parser_Inlines.pLast->pNext = pInline;
      Parser_pContext->Parser_Inlines.pLast = pInline;
   }

   Parser_pContext->Parser_Inlines.iCount++;

   return pInline;
}