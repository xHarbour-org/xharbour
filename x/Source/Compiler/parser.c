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

int Parser_iBackend = LANG_C;
int Parser_iInlineID = 0;

int Parser_iLine = 0, Parser_iErrors = 0, Parser_iWarnings = 0;
int Parser_iLinePRG = 0, Parser_iLineINLINE = 0;
int Parser_iNextToken = 0;
int Parser_iToken = 0;

BOOL Parser_bError = FALSE, Parser_bAnyWarning = FALSE, Parser_bMute = FALSE, Parser_bPPO = FALSE;

FUNCTIONS    Parser_Functions;
PARSED_FILES Parser_Files;
INLINES      Parser_Inlines;
FILE         *pParser_PPO;

YYSTYPE   yylval;

char Parser_cInlineID = '0';

extern char *Parser_asErrors[];
extern char *yytext;

void       ParseFunction( void );
DECLARED * ParseParams( int *piParams, TOKEN_ID Terminator );
void       ParseDeclarations( void );
BODY *     ParseBody( LINE * pPrevLine );
LINE *     ParseIf( void );
VALUE *    ParseValue( void );
VALUE *    ParseList( PRG_TYPE Type );

void DumpBody( BODY * pBody, int *piSpaces );
void DumpLine( LINE * pLine, int *piSpaces );

int main( int argc, char *argv[] )
{
   FILE *hSource; 
 
   FUNCTION * pFunc;

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
      int iSpaces = 0;

      pSource->sName = argv[1];
      pSource->hFile = hSource; 
      pSource->pNext = NULL;
      pSource->pPrev = NULL;
      pSource->iLine = 0;

      Parser_Files.iFiles = 1;
      Parser_Files.pLast = pSource;  
 
      Reducer_Init();

      while( NEXT_TOKEN() > 0 )
      {
         ParseFunction();
      }
    
      pFunc = Parser_Functions.pFirst;
    
      while( pFunc )
      {         
         printf( "\nFunction: %s LocalParams: %i Locals %i, Statics %i, Memvars %i, Fields %i, Params: %i Privates: %i Publics: %i\n", pFunc->pName->Name, pFunc->iLocalParams, pFunc->iLocals, pFunc->iStatics, pFunc->iMemvars, pFunc->iFields, pFunc->iParams, pFunc->iPrivates, pFunc->iPublics );

         DumpBody( pFunc->pBody, &iSpaces );

         pFunc = pFunc->pNext;
      }

      fclose( hSource );
 
      pSource = Parser_Files.pLast;
 
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

void DumpBody( BODY * pBody, int *piSpaces )
{
   if( pBody && pBody->pLines )
   {
      LINE * pLine = pBody->pLines;

      do
      {
         DumpLine( pLine, piSpaces );

         pLine = pLine->pNext;
      }
      while( pLine );
   }
}

void DumpLine( LINE * pLine, int *piSpaces )
{
   char *sSpacer;

   sSpacer = (char *) malloc( *piSpaces + 1 );
   memset( (void *) sSpacer, ' ', *piSpaces );
   sSpacer[ *piSpaces ] = '\0';

   printf( "%sLine kind: %s\n", sSpacer, ClipNet_LineKind( pLine->Kind ) );

   if( pLine->Kind == LINE_KIND_IF )
   {
      LINE *pIF = pLine;

      *piSpaces += 3;

      DumpBody( pLine->Value.pIf->pBody, piSpaces );

      pLine = pLine->Value.pIf->pElseIf;
      while( pLine )
      {
         printf( "%sLine kind: %s\n", sSpacer, ClipNet_LineKind( pLine->Kind ) );
         DumpBody( pLine->Value.pElseIf->pBody, piSpaces );
         pLine = pLine->pNext;
      }

      pLine = pIF->Value.pIf->pElse;
      if( pLine )
      {
         printf( "%sLine kind: %s\n", sSpacer, ClipNet_LineKind( pLine->Kind ) );
         DumpBody( pLine->Value.pElse->pBody, piSpaces );
      }

      printf( "%sLine kind: %s\n", sSpacer, "ENDIF" );

      *piSpaces -= 3;
   }

   free( (void *) sSpacer );
}

VALUE * Flag_AsMemvar( VALUE * pValue )
{
   VALUE * pLValue = Get_LValue( pValue );

   if( pLValue && pLValue->Kind == VALUE_KIND_LVALUE )
   {
      switch( pLValue->Value.pLValue->Kind )
      {
         case LVALUE_KIND_ID :
            pLValue->Value.pLValue->Kind = LVALUE_KIND_MEMVAR;
            pLValue->Value.pLValue->Value.pMemvar = pLValue->Value.pLValue->Value.pID;
            break;

         case LVALUE_KIND_MACRO :
            // Macro always refer to MEVAR anyway.
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

void ParseFunction( void )
{
   FUNCTION * pFunc;
   FUNC_KIND FuncKind = (FUNC_KIND) LAST_TOKEN();

   if( FuncKind >= FUNC_KIND_FUNC && FuncKind <= FUNC_KIND_PROC_STATICCRITICAL )
   {
      if( NEXT_TOKEN() == TOKEN_IDENTIFIER )
      {
         pFunc = New_Function( yylval.sText, FuncKind );

         if( NEXT_TOKEN() == '(' )
         {
            pFunc->pLocalParams = ParseParams( &(pFunc->iLocalParams), (TOKEN_ID) ')' );
            NEXT_TOKEN();
         }

         EXPECTED_EOL();

         ParseDeclarations();

         pFunc->pBody = ParseBody( NULL );

         return;
      }
   }

   Error:

      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected entity definition." );

   return;
}
  
DECLARED * ParseParams( int *piParams, TOKEN_ID Terminator )
{
   DECLARED *pFirst = NULL, *pLast = NULL;

   while( NEXT_TOKEN() == TOKEN_IDENTIFIER )
   {
      DECLARED * pParam = New_DeclaredID( yylval.sText, DECLARED_KIND_LOCAL_PARAM );

      if( pLast )
      {
         pLast->pNext = pParam;
         pLast = pParam;
      }
      else
      {
         pFirst = pParam;
         pLast  = pParam;
      }
      (*piParams)++;

      if( NEXT_TOKEN() != ',' )
      {
         break;
      }
   }

   if( LAST_TOKEN() != Terminator )
   {
      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected terminator." );
      return NULL;
   }
   else
   {
      if( pFirst == NULL )
      {
         return New_Declared();
      }
   }

   return pFirst;
}

void ParseDeclarations( void )
{
   DECLARED * pLocal;
   DECLARED * pStatic;
   DECLARED * pMemvar;
   DECLARED * pField;

   Again:

     if( NEXT_TOKEN() == TOKEN_LOCAL )
     {
        while ( NEXT_TOKEN() == TOKEN_IDENTIFIER )
        {
           pLocal = New_DeclaredID( yylval.sText, DECLARED_KIND_LOCAL );
 
           if( Parser_Functions.pLast->pLocals )
           {
              Parser_Functions.pLast->pLocals->pNext = pLocal;
              Parser_Functions.pLast->iLocals++;
           }
           else
           {
              Parser_Functions.pLast->pLocals = pLocal;
              Parser_Functions.pLast->iLocals = 1;
           }

           if( NEXT_TOKEN() == TOKEN_INASSIGN )
           {
              pLocal->pInit = ParseValue();
           }
           else if( LAST_TOKEN() != ',' )
           {
              break;
           }
        }  

        EXPECTED_EOL();

        goto Again;
     }
     else if( LAST_TOKEN() == TOKEN_STATIC )
     {
        while ( NEXT_TOKEN() == TOKEN_IDENTIFIER )
        {
           pStatic = New_DeclaredID( yylval.sText, DECLARED_KIND_STATIC ); 

           if( Parser_Functions.pLast->pStatics )
           {
              Parser_Functions.pLast->pStatics->pNext = pStatic;
              Parser_Functions.pLast->iStatics++;
           }
           else
           {
              Parser_Functions.pLast->pStatics = pStatic;
              Parser_Functions.pLast->iStatics = 1;
           }

           if( NEXT_TOKEN() == TOKEN_INASSIGN )
           {
              ParseValue();
           }
           else if( LAST_TOKEN() != ',' )
           {
              PUSH_BACK();
              break;
           }
        }  

        EXPECTED_EOL();

        goto Again;
     }
     else if( LAST_TOKEN() == TOKEN_MEMVAR )
     {
        while( NEXT_TOKEN() == TOKEN_IDENTIFIER )
        {
           pMemvar = New_DeclaredID( yylval.sText, DECLARED_KIND_MEMVAR ); 

           if( Parser_Functions.pLast->pMemvars )
           {
              Parser_Functions.pLast->pMemvars->pNext = pMemvar;
              Parser_Functions.pLast->iMemvars++;
           }
           else
           {
              Parser_Functions.pLast->pMemvars = pMemvar;
              Parser_Functions.pLast->iMemvars = 1;
           }

           if( NEXT_TOKEN() != ',' )
           {
              PUSH_BACK();
              break;
           }
        }

        EXPECTED_EOL();

        goto Again;
     }
     else if( LAST_TOKEN() == TOKEN_FIELD )
     {
        while ( NEXT_TOKEN() == TOKEN_IDENTIFIER )
        {
           pField = New_DeclaredID( yylval.sText, DECLARED_KIND_FIELD ); 

           if( Parser_Functions.pLast->pFields )
           {
              Parser_Functions.pLast->pFields->pNext = pField;
              Parser_Functions.pLast->iFields++;
           }
           else
           {
              Parser_Functions.pLast->pFields = pField;
              Parser_Functions.pLast->iFields = 1;
           }

           if( NEXT_TOKEN() != ',' )
           {
              PUSH_BACK();
              break;
           }
        }

        EXPECTED_EOL();

        goto Again;
     }
   #if GETTING_EMPTY_LINES
     else if( LAST_TOKEN() == '\n' )
     {
        goto Again;
     }
   #endif
     else
     {
        PUSH_BACK();
     }
}

BODY * ParseBody( LINE * pPrevLine )
{
   BODY * pBody = New_Body();
   LINE * pLine = NULL;
   VALUE * pValue = NULL;
   BOOL bEOB = FALSE;
   pBody->pLines = NULL;

   EXECUTABLE_MEMVAR *pMemvar, *pLastMemvar;
   EXECUTABLE_MEMVAR_KIND MemvarKind;

   do
   { 
      switch ( NEXT_TOKEN() )
      {
         
         #define GETTING_EMPTY_LINES
         #ifdef GETTING_EMPTY_LINES
         case '\n' :
            //printf( "EMPTY Line!!!\n" );
            continue;
         #endif

         case TOKEN_IF : 
            pLine = ParseIf();
            break;

         case TOKEN_RETURN :
           pLine = New_Line( ParseValue(), LINE_KIND_RETURN );
           break;

         case TOKEN_PRIVATE :
         case TOKEN_PUBLIC :
           
           pLastMemvar = NULL;
           MemvarKind  = ( LAST_TOKEN() == TOKEN_PRIVATE ? EXECUTABLE_MEMVAR_KIND_PRIVATE : EXECUTABLE_MEMVAR_KIND_PUBLIC );

           do
           {
              pMemvar = New_ExecutableMemvar( ParseValue(), MemvarKind );

              if( pLastMemvar == NULL )
              {                  
                 pLine = New_Line( (void *) pMemvar, MemvarKind == EXECUTABLE_MEMVAR_KIND_PRIVATE ? LINE_KIND_PRIVATES : LINE_KIND_PUBLICS );
              }
              else
              {
                 pLastMemvar->pNext = pMemvar;
              }

              pLastMemvar = pMemvar;
             
              if( NEXT_TOKEN() == TOKEN_INASSIGN )
              {
                 pMemvar->pInit = ParseValue();
              }
              else
              {
                 PUSH_BACK();
              }

              if( NEXT_TOKEN() != ',' )
              {
                 PUSH_BACK();
                 break;
              }
           }
           while( pMemvar );
           break;

         case TOKEN_PARAMETERS :
           while ( NEXT_TOKEN() == TOKEN_IDENTIFIER )
           {
              EXECUTABLE_MEMVAR *pMemvar;

              PUSH_BACK(); 
              pMemvar = New_ExecutableMemvar( ParseValue(), EXECUTABLE_MEMVAR_KIND_PARAMETER );
              pLine   = New_Line( (void *) pMemvar, LINE_KIND_PARAMETERS );
             
              if( NEXT_TOKEN() != ',' )
              {
                 PUSH_BACK();
                 break;
              }

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

           PUSH_BACK();
           break;

         default :
            //printf( "Pushback: %i\n", LAST_TOKEN() );
            PUSH_BACK();
            pValue = ParseValue();

            if( pValue )
            {
                switch( pValue->Kind )
                {
                   case VALUE_KIND_ASSIGNMENT:
                      pLine = New_Line( (void *) pValue, LINE_KIND_ASSIGNMENT );
                      break;

                   case VALUE_KIND_BINARY:
                      if( pValue->Value.pBinary->Kind == BINARY_KIND_EQUAL && pValue->Value.pBinary->pLeft->Kind == VALUE_KIND_LVALUE )
                      {
                         //printf( "Binary EQUAL used as assignment\n" );
                         pLine = New_Line( (void *) pValue, LINE_KIND_ASSIGNMENT );
                      }
                      else
                      {
                         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", Value can't be used as a statement." );
                         Release_Value( pValue );
                         pValue = NULL;
                         continue;
                      }
                      break;
   
                   case VALUE_KIND_UNARY:
                      pLine = New_Line( (void *) pValue, LINE_KIND_UNARY );
                      break;

                   case VALUE_KIND_FUNC_CALL:
                      pLine = New_Line( (void *) pValue, LINE_KIND_FUNC_CALL );
                      break;

                   case VALUE_KIND_IIF:
                      pLine = New_Line( (void *) pValue, LINE_KIND_IIF );
                      break;
   
                   case VALUE_KIND_METHOD_CALL:
                      pLine = New_Line( (void *) pValue, LINE_KIND_METHOD_CALL );
                      break;

                   default :
                      printf( "Kind: %i\n", pValue->Kind );
                      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", Value can't be used as a statement." );
                      Release_Value( pValue );
                      pValue = NULL;
                      continue;
               }
            }
            else
            {  
               if( EOB( LAST_TOKEN() ) )
               {
                  bEOB = TRUE;
                  break;
               }

               PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, "Internal error - No Value!!!" );
               ACCEPT_EOL()
               continue;
            }
      }

      if( bEOB )
      {
         PUSH_BACK();
         break;
      }
      else
      {
         ACCEPT_EOL();
      }

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

LINE * ParseIf( void )
{
   BODY * pBody = NULL;
   LINE * pLine = NULL, *pLine2 = NULL, *pLine3 = NULL;
   VALUE * pValue = NULL;

   pValue = ParseValue();
   ACCEPT_EOL();

   pBody = ParseBody( NULL );

   pLine = New_If( pValue, pBody );

   if( NEXT_TOKEN() == TOKEN_ELSEIF )
   {
      pValue = ParseValue();
      ACCEPT_EOL();

      pBody = ParseBody( NULL );

      pLine2 = New_ElseIf( pValue, pBody );

      pLine->Value.pIf->pElseIf = pLine2;

      while( NEXT_TOKEN() == TOKEN_ELSEIF )
      {
         pValue = ParseValue();
         ACCEPT_EOL();

         pBody = ParseBody( NULL );

         pLine3 = New_ElseIf( pValue, pBody );

         pLine2->pNext = pLine3;
         pLine2 = pLine3;
      }                
   }

   // NEXT_TOKEN() above always extracted!
   if( LAST_TOKEN() == TOKEN_ELSE )
   {
      ACCEPT_EOL();

      pBody = ParseBody( NULL );
      pLine->Value.pIf->pElse = New_Else( pBody );
   }
   else
   {
      PUSH_BACK();
   }

   ACCEPT_EOS( TOKEN_ENDIF );
   
   return pLine;
}

VALUE * ParseValue( void )
{
   VALUE *pValue = NULL, *pValue2 = NULL;
   BOOL bNegate = FALSE;

  NextValue:

   switch( NEXT_TOKEN() ) 
   {
      case '+':
         goto NextValue;

      case '-':
         bNegate = ! bNegate;
         goto NextValue;

      case TOKEN_INC:         
      case TOKEN_DEC:
         pValue = New_Unary( ParseValue(), (UNARY_KIND) LAST_TOKEN(), UNARY_WHEN_PRE );
         break;

      case TOKEN_NIL:
         pValue = New_NIL();
         break;

      case TOKEN_CONSTANT:
         pValue = New_Constant( &( yylval.Constant ) );
         break;

      case TOKEN_IDENTIFIER:
         pValue = New_LValueID( yylval.sText );
         break;

      case TOKEN_MACROVAR:
      case TOKEN_MACROTEXT:
         pValue = New_Macro( yylval.sText, MACRO_KIND_SIMPLE );
         break;

      case '&':
         pValue = New_Macro( ParseValue(), MACRO_KIND_COMPLEX );
         break;

      case TOKEN_IIF:
         if( NEXT_TOKEN() == '(' )
         {
            VALUE *pCond, *pTrue, *pFalse;

            pCond = ParseValue();

            if( pCond && NEXT_TOKEN() == ',' )
            {
               pTrue = ParseValue();

               if( pTrue && NEXT_TOKEN() == ',' )
               {
                  pFalse = ParseValue();

                  if( pFalse && NEXT_TOKEN() == ')' )
                  {
                     pValue = New_IIF( pCond, pTrue, pFalse );
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
         else
         {
            PUSH_BACK();
         }
         break;
   
      case '(' :
         //printf( "LIST\n" );
         if( ( pValue = ParseList( (PRG_TYPE) 0xFF ) ) != NULL )
         {
            ACCEPT_TOKEN( ')' );
         }
         break;

      case '{' :         
         if( NEXT_TOKEN() == TOKEN_CBMARKER )
         {
            BLOCK *pBlock;

            pValue = New_Block();
            pBlock = pValue->Value.pBlock;

            //printf( "Parse Block params...\n" );

            if( ( pBlock->pBlockLocals = ParseParams( &pBlock->iBlockLocals, TOKEN_CBMARKER ) ) != NULL )
            {
               printf( "Parse Block list...\n" );

               if( ( pBlock->pList = ParseList( (PRG_TYPE) 0xFF ) ) != NULL )
               {
                  printf( "Block list: %p Nodes: %i\n", pBlock->pList, pBlock->pList->Value.pList->iNodes );
                  ACCEPT_TOKEN( '}' );
               }
            }      
         }
         else
         {
            //printf( "LITERAL ARRAY token: %i\n", LAST_TOKEN() );

            PUSH_BACK();

            if( ( pValue = ParseList( (PRG_TYPE) 0xFF ) ) != NULL )
            {
               ACCEPT_TOKEN( '}' );
            }
         }
         break;

      default:
         #ifdef DEBUG_VALUE
            printf( "Not supported: %i\n", LAST_TOKEN() );
         #endif

         PUSH_BACK();
         goto ParseValue_Done;
   }

  TryExtendedValue :

   #ifdef DEBUG_EXTEND
      printf( "Extend Kind: %i after Last Token: %i %s\n", pValue->Kind, LAST_TOKEN(), yytext );
   #endif

   switch( NEXT_TOKEN() ) 
   {
      case '^' :               
                         
      case '%' :               
      case '*' :               
      case '/' :               
                         
      case '+' :               
      case '-' :               
                         
      case TOKEN_BITSHIFTL :   
      case TOKEN_BITSHIFTR :   
                         
      case '$' :               
      case '~' :               
      case '?' :               
      case 'ò' :               
      case 'ó' :               
      case '>' :               
      case '<' :               
                         
      case TOKEN_EQ :          
      case '=' :               
      case '#' :               
                         
      case TOKEN_BITOR :       
      case TOKEN_BITXOR :      
      case TOKEN_BITAND :      
                         
      case TOKEN_AND :         
      case TOKEN_OR :
      {
         BINARY_KIND Kind = (BINARY_KIND) LAST_TOKEN();

         pValue2 = ParseValue();

         pValue = New_Binary( pValue, pValue2, Kind );
         goto TryExtendedValue;
      }
   
      case TOKEN_ALIAS :
        pValue2 = ParseValue();

        if( pValue->Kind == VALUE_KIND_LVALUE && pValue->Value.pLValue->Kind == LVALUE_KIND_ID && 
            ( 
              ( 
                pValue->Value.pLValue->Value.pID->Name[0] == 'M' && 
                pValue->Value.pLValue->Value.pID->Name[1] == '\0' 
              )
              ||
              ( 
                strncmp( pValue->Value.pLValue->Value.pID->Name, "MEMVAR", 4 ) == 0 &&
                strncmp( pValue->Value.pLValue->Value.pID->Name, "MEMVAR", strlen( pValue->Value.pLValue->Value.pID->Name ) ) == 0
              )
            )
         )
         {
            //printf( "MEMVAR\n" );
            Release_Value( pValue );
            pValue = Flag_AsMemvar( pValue2 );
            //printf( "MEMVAR Kind: %i\n", pValue->Kind );
         }
         else
         {
            //printf( "ALIAS\n" );
            pValue = New_Aliased( pValue, pValue2 );
         }
         goto TryExtendedValue;

      case '(' :
         //printf( "FUNCTION CALL\n" );
         if( ( pValue2 = ParseList( PRG_TYPE_UNDEF ) ) != NULL )
         {
            ACCEPT_TOKEN( ')' );
            pValue = New_FuncCall( pValue, pValue2 );
         }

         goto TryExtendedValue;

      case '[' :
         //printf( "ARRAY INDEX\n" );
         if( ( pValue2 = ParseList( (PRG_TYPE) ( PRG_TYPE_NUMERIC | PRG_TYPE_STRING ) ) ) != NULL )
         {
            ACCEPT_TOKEN( ']' );
            pValue = New_ArrayElement( pValue, pValue2 );
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

         pValue2 = ParseValue();

         pValue = New_Assignment( pValue, pValue2, (ASSIGNMENT_KIND) LAST_TOKEN() );

         #ifdef DEBUG_INASSIGN
            printf( "LValue Kind: %i ASSIGNED %p\n", pValue->Value.pAssignment->pLValue->Value.pLValue->Kind, pValue->Value.pAssignment->pValue );
         #endif

         //goto TryExtendedValue; //Right side will consume all!
         break;

      case TOKEN_INC :
      case TOKEN_DEC :
         pValue = New_Unary( pValue, (UNARY_KIND) LAST_TOKEN(), UNARY_WHEN_POST );
         goto TryExtendedValue;

      default:
         #ifdef DEBUG_VALUE
            printf( "2 - Not supported: %i\n", LAST_TOKEN() );
         #endif
         PUSH_BACK();     
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

VALUE * ParseList( PRG_TYPE Type )
{
   LIST *pList = NULL;
   VALUE *pValue;

  NextNode :

   pValue = ParseValue();

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
         PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid list type." );
         return NULL;
         //goto ParseList_Next;
      }
   }
   else
   {
      if( Type == PRG_TYPE_UNDEF )
      {
         pValue = New_NIL();
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
      pList = New_List( pValue ); 
   }
   else
   {
      New_ListNode( pList, pValue ); 
   }

  ParseList_Next :

   if( NEXT_TOKEN() == ',' )
   {
      goto NextNode;
   }
   else
   {
      PUSH_BACK();
   }

   if( pList )
   {
      return New_Value( (void *) pList, VALUE_KIND_LIST );
   }
   else
   {
      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", invalid list." );
   }

   return NULL;
}
 
INLINE * Parser_InlineAdd( char *sName )
{
   INLINE *pInline;

   New_Function( sName, FUNC_KIND_FUNC_STATIC );

   pInline = New_Inline( sName );

   if( Parser_Inlines.iCount == 0 )
   {
      Parser_Inlines.pFirst = pInline;
      Parser_Inlines.pLast  = pInline;
   }
   else
   {
      Parser_Inlines.pLast->pNext = pInline;
      Parser_Inlines.pLast = pInline;
   }

   Parser_Inlines.iCount++;

   return pInline;
}