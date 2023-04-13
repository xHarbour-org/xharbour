/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Resource Writer for xHarbour
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
 * NOTE: This utility is intended ONLY for building xHarbour from source
 */

#if defined( _MSC_VER )
#   if !defined( _CRT_SECURE_NO_WARNINGS )
#      define _CRT_SECURE_NO_WARNINGS
#   endif
#   if ( defined( _MSC_FULL_VER ) && ( _MSC_FULL_VER == 13104035 ) )
#      pragma comment( lib, "bufferoverflowU" )
#   endif
#endif

#include <windows.h>
#include <time.h>
#include <stdio.h>
#include "hbver.h"
#include "hbverbld.h"

#define COMPILER_BUF_SIZE 80

static char * _hb_verCompiler( void )
{
   const char *   pszName;
   char           szSub[ 32 ] = { '\0' };
   int            iVerMajor;
   int            iVerMinor;
   int            iVerPatch;
   char *         pszCompiler = ( char * ) malloc( COMPILER_BUF_SIZE );

#if defined( __IBMC__ ) || defined( __IBMCPP__ )

#  if defined( __IBMC__ )
      iVerMajor   = __IBMC__;
#  else
      iVerMajor   = __IBMCPP__;
#  endif

   if( iVerMajor >= 300 )
      pszName = "IBM Visual Age C++";
   else
      pszName = "IBM C++";

   iVerMajor   /= 100;
   iVerMinor   = iVerMajor % 100;
   iVerPatch   = 0;

#elif defined( __XCC__ )

   pszName     = "Pelles ISO C Compiler (XCC)";
   iVerMajor   = __POCC__ / 100;
   iVerMinor   = __POCC__ % 100;
   iVerPatch   = 0;

#elif defined( __POCC__ )

   pszName     = "Pelles ISO C Compiler";
   iVerMajor   = __POCC__ / 100;
   iVerMinor   = __POCC__ % 100;
   if( ( iVerMajor == 2 ) && ( iVerMinor == 70 ) )
      pszName = "XCC ISO C Compiler";
   iVerPatch   = 0;

#elif defined( __LCC__ )

   pszName     = "Logiciels/Informatique lcc-win32";
   iVerMajor   = 0 /* __LCC__ / 100 */;
   iVerMinor   = 0 /* __LCC__ % 100 */;
   iVerPatch   = 0;

#elif defined( __DMC__ )

   pszName     = __DMC_VERSION_STRING__;
   iVerMajor   = 0;
   iVerMinor   = 0;
   iVerPatch   = 0;

#elif defined( __ICL )

   pszName = "Intel(R) C";

#  if defined( __cplusplus )
      strncpy( szSub, "++", sizeof( szSub ) - 1 );
#  endif

   iVerMajor   = __ICL / 100;
   iVerMinor   = __ICL % 100;
   iVerPatch   = 0;

#elif defined( __ICC )

   pszName = "Intel(R) (ICC) C";

#  if defined( __cplusplus )
      strncpy( szSub, "++", sizeof( szSub ) - 1 );
#  endif

   iVerMajor   = __ICC / 100;
   iVerMinor   = __ICC % 100;
   iVerPatch   = 0;

#elif defined( _MSC_VER )

#  if ( _MSC_VER >= 800 )
      pszName  = "Microsoft Visual C";
#  else
      pszName  = "Microsoft C";
#  endif

#  if defined( __cplusplus )
      strncpy( szSub, "++", sizeof( szSub ) - 1 );
#  endif

   iVerMajor   = _MSC_VER / 100;
   iVerMinor   = _MSC_VER % 100;

#  if defined( _MSC_FULL_VER )
#     if ( _MSC_VER >= 1400 )
         iVerPatch = _MSC_FULL_VER - ( _MSC_VER * 100000 );
#     else
         iVerPatch = _MSC_FULL_VER - ( _MSC_VER * 10000 );
#     endif
#  else
      iVerPatch    = 0;
#  endif

#elif defined( __BORLANDC__ )

#  if ( __BORLANDC__ >= 0x590 )
#     if ( __BORLANDC__ >= 0x620 )
         pszName  = "Borland/Embarcadero C++";
#     else
         pszName  = "Borland/CodeGear C++";
#     endif
#  else
      pszName     = "Borland C++";
#  endif
#  if ( __BORLANDC__ == 0x410 ) /* Version 3.1 */
      iVerMajor   = 3;
      iVerMinor   = 1;
      iVerPatch   = 0;
#  elif ( __BORLANDC__ >= 0x500 ) /* Version 5.x */
      iVerMajor   = __BORLANDC__ >> 8;
      iVerMinor   = ( __BORLANDC__ & 0xFF ) >> 4;
      iVerPatch   = __BORLANDC__ & 0xF;
#  else /* Version 4.x */
      iVerMajor   = __BORLANDC__ >> 8;
      iVerMinor   = ( __BORLANDC__ - 1 & 0xFF ) >> 4;
      iVerPatch   = 0;
#  endif

#elif defined( __TURBOC__ )

   pszName     = "Borland Turbo C";
   iVerMajor   = __TURBOC__ >> 8;
   iVerMinor   = __TURBOC__ & 0xFF;
   iVerPatch   = 0;

#elif defined( __MPW__ )

   pszName     = "MPW C";
   iVerMajor   = __MPW__ / 100;
   iVerMinor   = __MPW__ % 100;
   iVerPatch   = 0;

#elif defined( __WATCOMC__ )

#  if __WATCOMC__ < 1200
      pszName  = "Watcom C";
#  else
      pszName  = "Open Watcom C";
#  endif

#  if defined( __cplusplus )
      strncpy( szSub, "++", sizeof( szSub ) - 1 );
#  endif

   iVerMajor    = __WATCOMC__ / 100;
   iVerMinor    = __WATCOMC__ % 100;

#  if defined( __WATCOM_REVISION__ )
      iVerPatch = __WATCOM_REVISION__;
#  else
      iVerPatch = 0;
#  endif

#elif defined( __GNUC__ )

#  if defined( __DJGPP__ )
      pszName  = "Delorie GNU C";
#  elif defined( __CYGWIN__ )
      pszName  = "Cygwin GNU C";
#  elif defined( __MINGW32__ )
      pszName  = "MinGW GNU C";
#  elif defined( __RSX32__ )
      pszName  = "EMX/RSXNT/DOS GNU C";
#  elif defined( __RSXNT__ )
      pszName  = "EMX/RSXNT/Win32 GNU C";
#  elif defined( __EMX__ )
      pszName  = "EMX GNU C";
#  else
      pszName  = "GNU C";
#  endif

#  if defined( __cplusplus )
      strncpy( szSub, "++", sizeof( szSub ) - 1 );
#  endif

   iVerMajor    = __GNUC__;
   iVerMinor    = __GNUC_MINOR__;
#  if defined( __GNUC_PATCHLEVEL__ )
      iVerPatch = __GNUC_PATCHLEVEL__;
#  else
      iVerPatch = 0;
#  endif
#else

   pszName     = ( char * ) NULL;
   iVerMajor   = iVerMinor = iVerPatch = 0;

#endif

   if( pszName )
   {
#if defined( __ICL )
      sprintf( pszCompiler, "%s%s %hd.%01d Build %u", pszName, szSub, iVerMajor, iVerMinor, __INTEL_COMPILER_BUILD_DATE );
#else
      if( iVerPatch != 0 )
#        if defined( _MSC_VER )
#           if defined( _MSC_BUILD )
               sprintf( pszCompiler, "%s%s %hd.%02d.%hu.%02d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch, _MSC_BUILD );
#        else
#           if ( _MSC_VER == 1400 )
               sprintf( pszCompiler, "%s%s %hd.%02d.%hu", pszName, szSub, iVerMajor, iVerMinor, iVerPatch );
#     else
         sprintf( pszCompiler, "%s%s %hd.%02d.%hu", pszName, szSub, iVerMajor, iVerMinor, iVerPatch );
#     endif
#   endif
#else
         sprintf( pszCompiler, "%s%s %hd.%hd.%hd", pszName, szSub, iVerMajor, iVerMinor, iVerPatch );
#endif
      else if( iVerMajor != 0 || iVerMinor != 0 )
         sprintf( pszCompiler, "%s%s %hd.%hd", pszName, szSub, iVerMajor, iVerMinor );
      else
         sprintf( pszCompiler, "%s%s", pszName, szSub );
#endif
   }
   else
      strcpy( pszCompiler, "(unknown)" );

#if defined( __DJGPP__ )
   sprintf( szSub, sizeof( szSub ), " (DJGPP %i.%02i)", ( int ) __DJGPP__, ( int ) __DJGPP_MINOR__ );
   strncat( pszCompiler, szSub );
#endif

   strcat( pszCompiler, " (32-bit)" );

   return pszCompiler;
}

static char * cParExp( char * szParam, char * szStr )
{
   char szTemp[ 265 ];
   char * pAction;
   int  iPos = 0;

   strncpy( szParam, szStr, 264 );

   while( ( pAction = strchr( szParam + iPos, '$' ) ) != NULL && *( pAction + 1 ) )
   {
      switch( *( pAction + 1 ) )
      {
         case 'y':
         case 'Y':              /* put _HB_CURR_YEAR value */
            strncpy( szTemp, pAction + 2, 264 );
            *pAction = '\0';
            strcat( szParam, _HB_CURR_YEAR );
            strcat( szParam, szTemp );
            break;

         default:
            iPos = pAction - szParam + 1;
            break;
      }
   }

   return szParam;
}

int main( int argc, char * argv[] )
{
   if( argc >= 7 )
   {
      FILE * h = fopen( argv[ 1 ], "wb" );

      if( h )
      {
         SYSTEMTIME  t;
         char *      cCompiler = _hb_verCompiler();
         char *      aMo[]     = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };
         char        szParam[ 265 ];

         GetLocalTime( &t );

         fprintf( h, "1 VERSIONINFO\n" );
         fprintf( h, "FILEVERSION %d,%d,%d,%d\n", HB_VER_MAJOR,HB_VER_MINOR,HB_VER_REVISION,HB_VER_CVSID );
         fprintf( h, "PRODUCTVERSION %d,%d,%d,%d\n", HB_VER_MAJOR,HB_VER_MINOR,HB_VER_REVISION,HB_VER_CVSID );
#if defined( __DMC__ )
         fprintf( h, "BEGIN\n" );
#else
         fprintf( h, "{\n" );
#endif
         fprintf( h, " BLOCK \"StringFileInfo\"\n" );
#if defined( __DMC__ )
         fprintf( h, " BEGIN\n" );
#else
         fprintf( h, " {\n" );
#endif
         fprintf( h, "  BLOCK \"040904E4\"\n" );
#if defined( __DMC__ )
         fprintf( h, "  BEGIN\n" );
#else
         fprintf( h, "  {\n" );
#endif
         fprintf( h, "   VALUE \"CompanyName\", \"%s\\000\"\n", cParExp( szParam, argv[6] ) );
         fprintf( h, "   VALUE \"FileDescription\", \"%s\\000\"\n", cParExp( szParam, argv[2] ) );
         fprintf( h, "   VALUE \"FileVersion\", \"%d.%d.%d.%d\\000\"\n", HB_VER_MAJOR,HB_VER_MINOR,HB_VER_REVISION,HB_VER_CVSID );
         fprintf( h, "   VALUE \"InternalName\", \"%s\\000\"\n", cParExp( szParam, argv[3] ) );
         fprintf( h, "   VALUE \"LegalCopyright\", \"\\251 %s\\000\"\n", cParExp( szParam, argv[4] ) );
         fprintf( h, "   VALUE \"ProductName\", \"%s\\000\"\n", cParExp( szParam, argv[2] ) );
         fprintf( h, "   VALUE \"ProductVersion\", \"%d.%d.%d.%d\\000\"\n", HB_VER_MAJOR,HB_VER_MINOR,HB_VER_REVISION,HB_VER_CVSID );
         fprintf( h, "   VALUE \"Programmer\", \"%s\\000\"\n", cParExp( szParam, argv[5] ) );
         fprintf( h, "   VALUE \"CompileDate\", \"%02d%s%s%s%04d\\000\"\n", t.wDay, " ", aMo[ t.wMonth - 1 ], " ", t.wYear );
         fprintf( h, "   VALUE \"CompileTime\", \"%02d:%02d:%02d\\000\"\n", t.wHour, t.wMinute, t.wSecond );
         fprintf( h, "   VALUE \"OriginalFileName\", \"%s\\000\"\n", cParExp( szParam, argv[3] ) );
         fprintf( h, "   VALUE \"Compiler\", \"%s\\000\"\n", cCompiler );
         fprintf( h, "   VALUE \"PrivateBuild\", \"\\000\"\n" );
         fprintf( h, "   VALUE \"SpecialBuild\", \"\\000\"\n" );
#if defined( __DMC__ )
         fprintf( h, "  END\n" );
         fprintf( h, " END\n" );
#else
         fprintf( h, "  }\n" );
         fprintf( h, " }\n" );
#endif
         fprintf( h, " BLOCK \"VarFileInfo\"\n" );
#if defined( __DMC__ )
         fprintf( h, " BEGIN\n" );
#else
         fprintf( h, " {\n" );
#endif
         fprintf( h, "  VALUE \"Translation\", 1033, 1252\n" );
#if defined( __DMC__ )
         fprintf( h, " END\n" );
         fprintf( h, "END\n" );
#else
         fprintf( h, " }\n" );
         fprintf( h, "}\n" );
#endif
         if( argc >= 8 )
            fprintf( h, "\nXHBICON    ICON   \"%s\" \n", argv[ 7 ] );

         free( cCompiler );
         fclose( h );

         return EXIT_SUCCESS;
      }
   }
   else
   {
      char sCommand[ 256 ];
      sprintf( sCommand, "%s: Incorrect parameters\nSyntax: %s <cRCFile> <cDescription> <cDllName> <cCopyright> <cProgrammer> <cCompanyName>", argv[ 0 ], argv[ 0 ] );
      printf( "%s\n", sCommand );
   }

   return EXIT_FAILURE;
}
