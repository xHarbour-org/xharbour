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
#include <stdio.h>
#include <stdlib.h>

static ULONG runlib( char* szCmd, char* szRsp )
{
   ULONG rc            = 0;
   char *pEnvCMD       = getenv( "COMSPEC" );
   char *szCommandLine = ( char * ) malloc( 512 );

   STARTUPINFO           StartupInfo;
   PROCESS_INFORMATION   ProcessInfo;

   memset( szCommandLine, 0, 512 );
   memset( &StartupInfo,  0, sizeof( StartupInfo ) );

   StartupInfo.cb          = sizeof( STARTUPINFO );
   StartupInfo.dwFlags     = STARTF_USESHOWWINDOW;
   StartupInfo.wShowWindow = SW_NORMAL;

   sprintf( szCommandLine, "%s /c %s @%s", pEnvCMD, szCmd, szRsp );

   if ( !CreateProcess( NULL, szCommandLine, NULL, NULL, FALSE, 0x0000, NULL, NULL, &StartupInfo, &ProcessInfo ) )
   {
      free( szCommandLine );
      return GetLastError();
   }

   WaitForSingleObject( ProcessInfo.hProcess, INFINITE );

   if ( !GetExitCodeProcess( ProcessInfo.hProcess, &rc ) )
      rc = 0;

   CloseHandle( ProcessInfo.hThread );
   CloseHandle( ProcessInfo.hProcess );

   free( szCommandLine );

   return rc;
}

/* Syntax:  HBLIB <libexe> <flags> <compiler> <out.lib> <a.obj ....>
   Example: HBLIB __BORLANDC__ TLIB.EXE "/0 /C /P256" <out.lib> <a.obj .....>
*/

int main( int argc, char *argv[] )
{
   int i;
   ULONG rc;
   int iResult = EXIT_FAILURE;

   if ( argc >= 6 )
   {
      char szRsp[ 256 ];
      FILE *h;
      int iComp = 0;

      *szRsp = 0;

      sprintf( szRsp, "%s.@@@", _tempnam( "", "xx" ) );

      if ( strcmp( argv[ 1 ], "__BORLANDC__" ) && strcmp( argv[ 1 ], "__DMC__" ) )
      {
         printf( "Compiler not defined ...\n" );
         exit( EXIT_FAILURE );
      }

      if ( strcmp( argv[ 1 ], "__BORLANDC__" ) == NULL )
         iComp = 1;
      else if ( strcmp( argv[ 1 ], "__DMC__" ) == NULL )
         iComp = 2;

      h = fopen( szRsp, "wb" );

      if ( iComp == 1 )
      {
         fprintf( h, "%s &\n", argv[ 3 ] ); // flags
         fprintf( h, "%s &\n", argv[ 4 ] ); // library name
      }
      else if ( iComp == 2 )
      {
         fprintf( h, "%s\n", argv[ 3 ] ); // flags
         fprintf( h, "%s\n", argv[ 4 ] ); // library name
      }

      DeleteFile( argv[ 4 ] );

      for( i = 5; i < argc; i ++ )
      {
         if ( iComp == 1 )
         {
            if ( i == ( argc - 1 ) )
               fprintf( h, "+ %s\n",  argv[ i ] );
            else
               fprintf( h, "+ %s &\n", argv[ i ] );
         }
         else if ( iComp == 2 )
            fprintf( h, "%s\n",  argv[ i ] );
      }

      fclose( h );

      rc = runlib( argv[ 2 ], szRsp );

      if ( rc == 0 )
         iResult = EXIT_SUCCESS;

      DeleteFile( szRsp );
   }
   else
   {
      printf("Syntax:  HBLIB <compiler> <libexe> <flags> <out.lib> <a.obj ....>\n");
      printf("Example: HBLIB __BORLANDC__ TLIB.EXE \"/0 /C /P256\" out.lib a.obj b.obj c.obj\n");
   }

   return iResult;
}

