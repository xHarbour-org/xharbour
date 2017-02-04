/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * BCC Librarian Helpers for the purpose of building xHarbour
 *
 * Copyright 2012 Andi Jahja <andi.jahja@yahoo.co.id>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
   Simple Librarian for non standard syntax
   Syntax : HBLIB <libexe> <flags> <compiler> <out.lib> <a.obj ....>
   Example: HBLIB __BORLANDC__ TLIB.EXE "/0 /C /P256" <out.lib> <a.obj .....>
   Note   : For BCC and DMC to simplify makefiles
 */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

typedef enum
{
   UNKNOWN,
   BORLANDC,
   DMC
} COMPILERS;

static ULONG runlib( char * szCmd, char * szRsp, char * szFlags )
{
   ULONG                rc             = 0;
   char *               pEnvCMD        = getenv( "COMSPEC" );
   char *               szCommandLine  = ( char * ) malloc( 512 );

   STARTUPINFO          StartupInfo;
   PROCESS_INFORMATION  ProcessInfo;

   memset( szCommandLine, 0, 512 );
   memset( &StartupInfo, 0, sizeof( StartupInfo ) );

   StartupInfo.cb          = sizeof( STARTUPINFO );
   StartupInfo.dwFlags     = STARTF_USESHOWWINDOW;
   StartupInfo.wShowWindow = SW_NORMAL;

   sprintf( szCommandLine, "%s /c %s %s @%s", pEnvCMD, szCmd, szFlags, szRsp );

   if( ! CreateProcess( NULL, szCommandLine, NULL, NULL, FALSE, 0x0000, NULL, NULL, &StartupInfo, &ProcessInfo ) )
   {
      free( szCommandLine );
      return GetLastError();
   }

   WaitForSingleObject( ProcessInfo.hProcess, INFINITE );

   if( ! GetExitCodeProcess( ProcessInfo.hProcess, &rc ) )
      rc = 0;

   CloseHandle( ProcessInfo.hThread );
   CloseHandle( ProcessInfo.hProcess );

   free( szCommandLine );

   return rc;
}

static const char * file_ext( const char * filename )
{
   const char * dot = strrchr( filename, '.' );

   if( ! dot || dot == filename )
      return "";

   return dot + 1;
}

static char * lower( char * pszText )
{
   char * pszPos;

   for( pszPos = pszText; *pszPos; pszPos++ )
      *pszPos = ( char ) tolower( ( char ) *pszPos );

   return pszText;
}

static void createfromlst( FILE * fList, FILE * h, char * szObjDir, int iComp, BOOL bDll )
{
   int      c, i = 0;
   char *   string   = ( char * ) malloc( 256 );
   char     szList[ 256 * 1000 ]; /* should be enough for 1000 modules */
   char *   szObjExt = getenv( "OBJEXT" );

   *szList = 0;
   memset( string, 0, 256 );

   do
   {
      c = fgetc( fList );

      if( c == '\n' )
      {
         string[ i ] = 0;

         while( *string == ' ' )
            string++;

         if( *string && ( string[ 0 ] == '!' ) )
         {
            char *   inc = ( char * ) malloc( 256 );
            char *   s;

            memset( inc, 0, 256 );
            memcpy( inc, lower( string ), strlen( string ) );

            s = strstr( inc, "include" );

            if( s && *s )
            {
               char * sz = strchr( s, ' ' );

               if( ! sz )
               {
                  printf( "Error: Invalid include directive\n" );
                  free( string );
                  free( inc );
                  exit( EXIT_FAILURE );
               }
               else
               {
                  FILE * hInc;

                  while( *sz == ' ' )
                     sz++;

                  hInc = fopen( sz, "r" );

                  if( hInc )
                  {
                     /* recursive, i love it :-) */
                     createfromlst( hInc, h, szObjDir, iComp, bDll );
                     fclose( hInc );
                  }
                  else
                  {
                     printf( "Error: %s not found\n", sz );
                     free( string );
                     free( inc );
                     exit( EXIT_FAILURE );
                  }
               }
            }

            free( inc );
         }
         else if( *string && ! ( string[ 0 ] == ';' ) )
         {
            if( bDll )
            {
               char s[ 256 ];

               sprintf( s, "%s\\%s%s ", szObjDir, string, szObjExt );
               strcat( szList, s );
            }
            else
               fprintf( h, ( iComp == BORLANDC ) ? "+ %s\\%s%s &\n" : "%s\\%s%s\n", szObjDir, string, szObjExt );
         }

         *string  = 0;
         i        = 0;
      }
      else
      if( ! ( c == '\r' ) )
         string[ i++ ] = ( char ) c;

   }
   while( c != EOF );

   free( string );

   if( bDll )
      fprintf( h, "%s", szList );
}

/* Syntax:  HBLIB <libexe> <flags> <compiler> <out.lib> <a.obj ....>
   Example: HBLIB __BORLANDC__ TLIB.EXE "/0 /C /P256" <out.lib> <a.obj .....>
 */

int main( int argc, char * argv[] )
{
   int   i;
   ULONG rc;
   int   iResult = EXIT_FAILURE;

   if( argc >= 6 )
   {
      char           szRsp[ 256 ];
      FILE *         h;
      int            iComp       = 0;
      BOOL           bDll        = FALSE;
      BOOL           bIsDef      = FALSE;
      const char *   szFileExt   = lower( ( char * ) file_ext( ( const char * ) argv[ 5 ] ) );
      char *         szLibs;

      if( argc >= 7 && argv[ 7 ] )
      {
         const char * szDLL = lower( argv[ 7 ] );
         bDll = ( strcmp( szDLL, "dll" ) == 0 );
      }

      if( bDll && argc >= 8 && argv[ 8 ] )
      {
         const char * szDef = lower( ( char * ) file_ext( ( const char * ) argv[ 8 ] ) );
         bIsDef = ( strcmp( szDef, "def" ) == 0 );
      }

      *szRsp = 0;

      sprintf( szRsp, "%s.@@@", _tempnam( "", "xx" ) );

      if( strcmp( argv[ 1 ], "__BORLANDC__" ) && strcmp( argv[ 1 ], "__DMC__" ) )
      {
         printf( "Compiler not defined ...\n" );
         exit( EXIT_FAILURE );
      }

      if( strcmp( argv[ 1 ], "__BORLANDC__" ) == 0 )
      {
#if defined( __BORLANDC__ ) && defined( HB_OS_WIN_64 )
         szLibs   = "cw64mt.a import64.a ws2_32.a iphlpapi.a";
#else
         szLibs   = "cw32mt.lib import32.lib ws2_32.lib iphlpapi.lib";
#endif
         iComp    = BORLANDC;
      }
      else if( strcmp( argv[ 1 ], "__DMC__" ) == 0 )
      {
         szLibs   = "";
         iComp    = DMC;
      }

      h = fopen( szRsp, "wb" );

      if( ! bDll )
      {
         if( iComp == BORLANDC )
         {
            fprintf( h, "%s &\n", argv[ 4 ] );  // library name
         }
         else if( iComp == DMC )
         {
            fprintf( h, "%s\n", argv[ 4 ] ); // library name
         }
      }
      else
      {
         if( iComp == BORLANDC )
#if defined( __BORLANDC__ ) && defined( HB_OS_WIN_64 )
            fprintf( h, "%s", "c0d64w.o " );
#else
            fprintf( h, "%s", "c0d32.obj " );
#endif
      }

      if( strcmp( szFileExt, "lst" ) == 0 )
      {
         FILE * fList = fopen( argv[ 5 ], "r" );

         if( ! fList )
         {
            printf( "Error: Unable to open %s\n", argv[ 5 ] );
            DeleteFile( szRsp );
            exit( EXIT_FAILURE );
         }

         createfromlst( fList, h, argv[ 6 ], iComp, bDll );
         fclose( fList );
      }
      else
      {
         for( i = 5; i < argc; i++ )
            fprintf( h, ( iComp == BORLANDC ) ? "+ %s &\n" : "%s\n", argv[ i ] );
      }

      if( ! bDll && ( iComp == BORLANDC ) )
         fprintf( h, "+\n" );

      if( bDll )
      {
         if( argc >= 10 && argv[ 10 ] )
         {
            int u;

            for( u = 10; u < argc; u++ )
               fprintf( h, "%s ", argv[ u ] );
         }

         fprintf( h, ",%s,%s.map,%s,%s,%s\n", argv[ 4 ], argv[ 4 ], szLibs, bIsDef ? argv[ 8 ] : "", argv[ 9 ] );
      }

      fclose( h );

      DeleteFile( argv[ 4 ] );

      rc = runlib( argv[ 2 ], szRsp, argv[ 3 ] );

      if( rc == 0 )
         iResult = EXIT_SUCCESS;

      DeleteFile( szRsp );
   }
   else
   {
      printf( "Syntax:  HBLIB <compiler> <libexe> <flags> <out.lib> <a.obj ....>\n" );
      printf( "Example: HBLIB __BORLANDC__ TLIB.EXE \"/0 /C /P256\" out.lib a.obj b.obj c.obj\n" );
   }

   return iResult;
}

