#include "common.h"

BOOL PP_bInline = FALSE;

char PP_LineBuffer[ LINE_MAX_LEN];  

int PP_iLineLen;
 
int PP_NextLine( FILE *pPPO, char *sLine, PARSER_CONTEXT *Parser_pContext )
{
   char cChar, cLastChar = 0;
   char *pSemiColon = NULL;
   char cTerminator = 0;

   PP_iLineLen = 0 ;
 
   while( ( cChar = getc( Parser_pContext->Files.pLast->hFile ) ) != EOF )
   {  
      if( Parser_pContext->iLine && ( Parser_pContext->iLine % 100 ) == 0 && Parser_pContext->bMute == FALSE )
      {
         printf( "\r%i", Parser_pContext->iLine );
         fflush( stdout );
      }

      //printf( "Pos: %i, Char: %c pSemiColon: %p\n", PP_iLineLen, cChar, pSemiColon );

      if( cChar == '\n' )
      {  
         Parser_pContext->iLine = ++Parser_pContext->Files.pLast->iLine;

         while( PP_iLineLen && isspace( PP_LineBuffer[ --PP_iLineLen ] ) )
         {
         }
         
         if( PP_iLineLen == 0 )
         {
            continue; 
         }

         if( PP_LineBuffer[ PP_iLineLen ] == ';' )
         {
            // Override the semi colon and continue with next line.  
            //printf( "Continue after ';' at: %i\n", PP_iLineLen );
            pSemiColon = NULL;
            continue; 
         }

         ++PP_iLineLen;

         PP_LineBuffer[ PP_iLineLen++ ] = '\n'; 
         break; 
      }
      else if( cTerminator == 0 && pSemiColon == NULL && cChar == ';' )
      {
         pSemiColon = PP_LineBuffer + PP_iLineLen;
         cLastChar = cChar;
      }
      else if( cTerminator == 0 && pSemiColon && ! isspace( cChar ) )
      {     
         ungetc( cChar, Parser_pContext->Files.pLast->hFile );
    
         while( PP_LineBuffer[ --PP_iLineLen ] != ';' )
         {
            ungetc( PP_LineBuffer[ PP_iLineLen ], Parser_pContext->Files.pLast->hFile );
         }    

         PP_LineBuffer[ PP_iLineLen++ ] = '\n'; 
         break;
      }
      else
      {
         if( cTerminator )
         {
            if( cChar == cTerminator )
            {
               cTerminator = 0;
               cLastChar = cChar;
            }
         }
         else
         {
            if( cChar == '"' )
            {
               cTerminator = '"';
            }
            else if( cChar == '\'' )
            {
               cTerminator = '\'';
            }   
            else if( cChar == '[' &&  ! isalnum( cLastChar ) && strchr( ")]}.\"'", cLastChar ) == NULL )
            {
               cTerminator = ']';
            }
            else if( ! isspace( cChar ) )
            {
               cLastChar = cChar;
            }   
         }
      }

      PP_LineBuffer[ PP_iLineLen++ ] = cChar;
   } 
   
   //printf( "Len: %i\n", PP_iLineLen );

   if( PP_iLineLen )  
   { 
      strncpy( sLine, PP_LineBuffer, PP_iLineLen );
      sLine[ PP_iLineLen ] = '\0'; 
      //printf( "Source: >%s<\n", sLine );
   }
   else
   {
      //printf( "EOF!\n" );
   }
 
   return PP_iLineLen;
}