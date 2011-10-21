/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Ron Pinkas Ron@xHarbour.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

#include "hbclass.ch"

STATIC s_hEnvVars := { => }

STATIC s_sProgramsFolder

// No Actions.
#define TYPE_SOURCE_LIB -5
#define TYPE_SOURCE_OBJ -4
#define TYPE_SOURCE_RES -3
#define TYPE_RC         -2
#define TYPE_INCLUDE    -1
#define TYPE_NO_ACTION   0

// Actions generating
#define TYPE_FROM_PRG    1
#define TYPE_FROM_C      2
#define TYPE_FROM_SLY    3
#define TYPE_FROM_RC     4

// Targets
#define TYPE_EXE        10
#define TYPE_LIB        11
#define TYPE_DLL        12
#define TYPE_HRB        13

#ifdef __PLATFORM__Windows
  #define DRIVE_SEPARATOR ':'
  #define DIR_SEPARATOR '\'
  #define EOL Chr(13) + Chr(10)
  #define EXACT_CASE .F.
#else
  #define DRIVE_SEPARATOR ''
  #define DIR_SEPARATOR '/'
  #define EOL Chr(10)
  #define EXACT_CASE .T.
#endif


#pragma BEGINDUMP

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"

BOOL FindIncludes( const char *szFile, PHB_ITEM pArray )
{
   FILE *hFile;
   char cDelimiter;
   char szInclude[ FILENAME_MAX + 1];
   int iLen;
   HB_ITEM Include;

   hFile = fopen( szFile, "r" );

   //TraceLog( "debug.log", "Handle: %i, File: '%s'\n", hFile, szFile );

   if( hFile )
   {
      //TraceLog( "Scaning: '%s'\n", szFile );
   }
   else
   {
      TraceLog( "trace.log", "Couldn't scan: '%s'\n", szFile );
      return FALSE;
   }

   goto NewLine;

   while( feof( hFile ) == 0 )
   {
      if( cDelimiter == '/' )
      {
         cDelimiter = getc( hFile );
         if( feof( hFile ) )
         {
            fclose( hFile );
            return FALSE;
         }

         if( cDelimiter == '/' )
         {
            do
            {
               cDelimiter = getc( hFile );

               if( feof( hFile ) )
               {
                  fclose( hFile );
                  return FALSE;
               }
            }
            while( cDelimiter != '\n' && cDelimiter != '\r' );

            goto NewLine;
         }
         else if( cDelimiter == '*' )
         {
            Comment :

            while( getc( hFile ) != '*' )
            {
               if( feof( hFile ) )
               {
                  fclose( hFile );
                  return FALSE;
               }
            }

            if( ( cDelimiter = getc( hFile ) ) != '/' )
            {
               ungetc( cDelimiter, hFile );
               goto Comment;
            }

            goto Next;
         }
      }
      else if( cDelimiter == '\n' )
      {
         NewLine:

         do
         {
            cDelimiter = getc( hFile );

            if( feof( hFile ) )
            {
               fclose( hFile );
               return FALSE;
            }
         }
         while( cDelimiter == '\n' || cDelimiter == '\r' || cDelimiter == ' ' || cDelimiter == '\t' );

         //TraceLog( "debug.log", "Char: %i '%c'\n", cDelimiter, cDelimiter );

         if( cDelimiter == '#' )
         {
            do
            {
               cDelimiter = getc( hFile );

               if( feof( hFile ) )
               {
                  fclose( hFile );
                  return FALSE;
               }
            }
            while( cDelimiter == ' ' || cDelimiter == '\t' || cDelimiter == '\n' || cDelimiter == '\r' );

            ungetc( cDelimiter, hFile );

            if( tolower( getc( hFile ) ) == 'i'  && tolower( getc( hFile ) ) == 'n'  &&
                tolower( getc( hFile ) ) == 'c' && tolower( getc( hFile ) ) == 'l' && tolower( getc( hFile ) ) == 'u'  &&
                tolower( getc( hFile ) ) == 'd'  && tolower( getc( hFile ) ) == 'e' )
            {
               do
               {
                  cDelimiter = getc( hFile );

                  if( feof( hFile ) )
                  {
                     fclose( hFile );
                     return FALSE;
                  }
               }
               while( cDelimiter == ' ' || cDelimiter == '\t' || cDelimiter == '\n' || cDelimiter == '\r' );

               if( cDelimiter == '<' )
               {
                  cDelimiter = '>';
               }
               else if( cDelimiter != '"' && cDelimiter != '\'' )
               {
                  goto Next;
               }

               iLen = 0;
               do
               {
                  szInclude[ iLen ] = getc( hFile );

                  if( feof( hFile ) )
                  {
                     fclose( hFile );
                     return FALSE;
                  }
               }
               while( szInclude[ iLen ] != cDelimiter && ++iLen < FILENAME_MAX ) ;

               if( iLen )
               {
                  szInclude[ iLen ] = '\0';

                  Include.type = HB_IT_NIL;
                  hb_itemPutC( &Include, szInclude );

                  hb_arrayAddForward( pArray, &Include );
               }
            }
         }
         else
         {
            continue;
         }
      }

      Next:

      cDelimiter = getc( hFile );
      //printf( "Char: %i '%c'\n", cDelimiter, cDelimiter );
   }

   fclose( hFile );
   return TRUE;
}

HB_FUNC( FINDINCLUDES )
{
   HB_ITEM Array;

   Array.type = HB_IT_NIL;
   hb_arrayNew( &Array, 0 );

   FindIncludes( hb_parcx( 1 ), &Array );

   hb_itemForwardValue( hb_param( -1, HB_IT_ANY ), &Array );
}

#ifdef __GNUC__
  HB_FUNC( GETMODULEFILENAME )
  {
     hb_retc( " /usr" );
  }

#endif

#pragma ENDDUMP

#ifdef GUI
  #pragma BEGINDUMP
     #ifndef GUI
        #define GUI
     #endif
  #pragma ENDDUMP
#endif

#ifdef __PLATFORM__Windows
  #pragma BEGINDUMP

  #include <stdlib.h>
  #include <windows.h>

  //#undef  OutputDebugString
  //#define OutputDebugString( s )

  BOOL s_bWaitContinue;

  //#define Alert( sMessage ) MessageBox( 0, sMessage, "xBuild Wizard", MB_ICONINFORMATION )

  void Process_SetContinueFlag( BOOL bWaitContinue )
  {
     s_bWaitContinue = bWaitContinue;
  }

  HB_FUNC( CREATEPROCESSWAIT )
  {
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    HANDLE hOutput = NULL;
    BOOL bSuccess;
    PHB_ITEM pEnv;
    char *sEnv = NULL;

    if( hb_parclen(2) >= 32768 )
    {
       OutputDebugString( "CreateProcess() - command line to long!\n" );
       TraceLog( hb_parc(3) ? hb_parc(3) : "error.log", "CreateProcess() - command line too long:\n%s\n", hb_parc(2) );
       hb_retl( 0 );
       return;
    }

   #if 0
    OutputDebugString( "CreateProcess( " );
    OutputDebugString( hb_parcx(1) );
    OutputDebugString( ", " );
    OutputDebugString( hb_parcx(2) );
    OutputDebugString( ", " );
    OutputDebugString( hb_parcx(3) );
    OutputDebugString( ", " );
    OutputDebugString( hb_parcx(4) );
    OutputDebugString( " )\n" );
   #endif

    if( hb_parc(3) )
    {
       SECURITY_ATTRIBUTES sa;

       ZeroMemory( &sa, sizeof(sa) );
       sa.nLength = sizeof(SECURITY_ATTRIBUTES);
       sa.bInheritHandle = TRUE;
       sa.lpSecurityDescriptor = NULL;

       hOutput = CreateFile(
                                     hb_parc(3),                                        //
                                     GENERIC_READ | GENERIC_WRITE,          //
                                     FILE_SHARE_READ | FILE_SHARE_WRITE, // share for reading & writing.
                                     &sa,                                                 //
                                     OPEN_ALWAYS,                                  //
                                     FILE_ATTRIBUTE_NORMAL,                   // normal file
                                     NULL                                                // no attr. template
                                   );

       if( hOutput == INVALID_HANDLE_VALUE)
       {
          TraceLog( hb_parc(3), "CreateFile() FAILED (%i)\n", GetLastError() );
          OutputDebugString( "CreateFile() FAILED!\n" );
          hb_retl( 0 );
          return;
       }

       SetFilePointer( hOutput, 0, NULL, FILE_END );
    }

    // si
    ZeroMemory( &si, sizeof(si) );
    si.cb          = sizeof(si);
    si.dwFlags     |= STARTF_USESHOWWINDOW;
    si.wShowWindow |= SW_HIDE;
    if( hOutput != NULL )
    {
       si.dwFlags    |= STARTF_USESTDHANDLES;
       si.hStdInput  = GetStdHandle( STD_INPUT_HANDLE );
       si.hStdOutput = hOutput;
       si.hStdError  = hOutput;
    }

    // pi
    ZeroMemory( &pi, sizeof(pi) );

    pEnv = hb_param( 4, HB_IT_STRING );
    if( pEnv )
    {
       sEnv = pEnv->item.asString.value;
    }

    // Start the child process.
    if( CreateProcess(
                             hb_parc(1),             //App name.
                             (char *) hb_parc(2),             // Command line.
                             NULL,                     // Process handle not inheritable.
                             NULL,                     // Thread handle not inheritable.
                             ( hOutput != NULL ), // Set handle inheritance .
                             0,                          // No creation flags.
                             sEnv,                     // Use parent's environment block.
                             NULL,                     // Use parent's starting directory.
                             &si,                       // Pointer to STARTUPINFO structure.
                             &pi                        // Pointer to PROCESS_INFORMATION structure.
                          )
      )
    {
       s_bWaitContinue = TRUE;
       bSuccess = TRUE;

      #if 0
       OutputDebugString( "Waiting...\n" );
      #endif

       // Wait until child process exits.
       while( WaitForSingleObject( pi.hProcess, 100 ) == WAIT_TIMEOUT )
       {
          #ifdef GUI
            MSG msg;
          #endif

          if( s_bWaitContinue == FALSE )
          {
             bSuccess = FALSE;
             break;
          }

          #ifdef GUI
            if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
            {
               if( ! IsDialogMessage( GetActiveWindow(), &msg ) )
               {
                  TranslateMessage( &msg );
                  DispatchMessage( &msg );
               }
            }
         #endif
       }

      #if 0
       OutputDebugString( "Finished.\n" );
      #endif

       // Close process and thread handles.
       CloseHandle( pi.hProcess );
       CloseHandle( pi.hThread );
    }
    else
    {
       char *lpMsgBuf;
       DWORD dw = GetLastError();

       FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, dw, MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ), (LPTSTR) &lpMsgBuf, 0, NULL );
       TraceLog( "error.log", "CreateProcess FAILED with %d, >%s<!\n", dw, lpMsgBuf );
       OutputDebugString( "CreateProcess() FAILED with:" );
       OutputDebugString( lpMsgBuf );
       OutputDebugString( "\n" );
       LocalFree( (LPVOID) lpMsgBuf );

       bSuccess = FALSE;
    }

    if( hOutput )
    {
       CloseHandle( hOutput );
    }

    hb_retl( bSuccess );
  }

#if 1
  HB_FUNC( GETMODULEFILENAME )
  {
     char sFile[ MAX_PATH ];

     GetModuleFileName( NULL, sFile, MAX_PATH );
     hb_retc( sFile );
  }
#endif

  #pragma ENDDUMP

#endif

#if 1
  #pragma BEGINDUMP
  //----------------------- Dont link all the R/T -----------------------------//
  void hb_vmSymbolInit_RT( void )
  {
  }

  void hb_i18nInit( void )
  {
  }

  void hb_i18nExit( void )
  {
  }

  //void TraceLog( const char * sFile, const char * sTraceMsg, ... )
  //{
  //}
  #pragma ENDDUMP

#ifdef NO_MACRO_RT
  #pragma BEGINDUMP

  void hb_macroGetValue( HB_ITEM_PTR pItem, BYTE iContext, BYTE flags )
  {
     HB_SYMBOL_UNUSED( pItem );
     HB_SYMBOL_UNUSED( iContext );
     HB_SYMBOL_UNUSED( flags );

     printf( "Macro compiler not linked!\n" );
     exit(1);
  }

  void hb_macroSetValue( HB_ITEM_PTR pItem, BYTE flags )
  {
     HB_SYMBOL_UNUSED( pItem );
     HB_SYMBOL_UNUSED( flags );

     printf( "Macro compiler not linked!\n" );
     exit(1);
  }

  HB_MACRO_PTR hb_macroCompile( const char * szString )
  {
     HB_SYMBOL_UNUSED( szString );

     printf( "Macro compiler not linked!\n" );
     exit(1);
     return NULL;
  }

  char * hb_macroGetType( HB_ITEM_PTR pItem, BYTE flags )
  {
     HB_SYMBOL_UNUSED( pItem );
     HB_SYMBOL_UNUSED( flags );

     return "U";
  }

  void hb_macroDelete( HB_MACRO_PTR pMacro )
  {
     HB_SYMBOL_UNUSED( pMacro );

     printf( "Macro compiler not linked!\n" );
     exit(1);
  }

  void hb_macroRun( HB_MACRO_PTR pMacro )
  {
     HB_SYMBOL_UNUSED( pMacro );

     printf( "Macro compiler not linked!\n" );
     exit(1);
  }

  BOOL hb_macroIsIdent( char * szString )
  {
     HB_SYMBOL_UNUSED( szString );

     return FALSE;
  }

  char * hb_macroTextSubst( char * szString, ULONG *pulStringLen )
  {
     HB_SYMBOL_UNUSED( szString );
     HB_SYMBOL_UNUSED( *pulStringLen );

     return szString;
  }

  void hb_macroPopAliasedValue( HB_ITEM_PTR pAlias, HB_ITEM_PTR pVar, BYTE flags )
  {
     HB_SYMBOL_UNUSED( pAlias );
     HB_SYMBOL_UNUSED( pVar );
     HB_SYMBOL_UNUSED( flags );

     printf( "Macro compiler not linked!\n" );
     exit(1);
  }

  void hb_macroPushAliasedValue( HB_ITEM_PTR pAlias, HB_ITEM_PTR pVar, BYTE flags )
  {
     HB_SYMBOL_UNUSED( pAlias );
     HB_SYMBOL_UNUSED( pVar );
     HB_SYMBOL_UNUSED( flags );

     printf( "Macro compiler not linked!\n" );
     exit(1);
  }

  char * hb_macroExpandString( char *szString, ULONG ulLength, BOOL *pbNewString )
  {
     HB_SYMBOL_UNUSED( szString );
     HB_SYMBOL_UNUSED( ulLength );

     *pbNewString = FALSE;
     return szString;
  }

  void hb_macroPushSymbol( HB_ITEM_PTR pItem )
  {
     HB_SYMBOL_UNUSED( pItem );

     printf( "Macro compiler not linked!\n" );
     exit(1);
  }

  void hb_macroTextValue( HB_ITEM_PTR pItem )
  {
     HB_SYMBOL_UNUSED( pItem );
  }

  ULONG hb_macroSetMacro( BOOL bSet, ULONG flag )
  {
     HB_SYMBOL_UNUSED( bSet );
     HB_SYMBOL_UNUSED( flag );

     return 0;
  }

  HB_FUNC( HB_SETMACRO )
  {
  }

  #pragma ENDDUMP
#endif

#endif
