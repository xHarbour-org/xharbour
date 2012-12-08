/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Native compiler object module generation from Harbour C output.
 *
 * Copyright 2001 Jos‚ Lal¡n <dezac@corevia.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbcomp.h"
#include "hb_io.h"



/* QUESTION: Allocate buffer dynamically ? */
#define HB_CFG_LINE_LEN ( HB_PATH_MAX << 1 )

#include "hbexemem.h"

/*--------------------------------------------------------------------------*/

static char * hb_searchpath( const char * pszFile, char * pszEnv, char * pszCfg )
{
   char *   pszPath;
   BOOL     bFound = FALSE;

   /* Check current dir first  */
   if( hb_fsFileExists( ( const char * ) pszFile ) )
   {
      hb_snprintf( pszCfg, HB_PATH_MAX, "%s", pszFile );
      return ( char * ) pszFile;
   }
   else
   {
      /* Check if pszFile exists somewhere in the path */
      while( *pszEnv )
      {
         pszPath = pszEnv;
         while( *pszEnv )
         {
            if( *pszEnv == HB_OS_PATH_LIST_SEP_CHR )
            {
               *pszEnv++ = '\0';
               break;
            }
            pszEnv++;
         }
         if( *pszPath )
         {
            hb_snprintf( pszCfg, HB_PATH_MAX, "%s%c%s", pszPath, HB_OS_PATH_DELIM_CHR, pszFile );
            if( hb_fsFileExists( ( const char * ) pszCfg ) )
            {
               bFound = TRUE;
               break;
            }
         }
      }
   }

   /* If not found, make sure to return a NULL string */
   if( ! bFound )
      *pszCfg = '\0';

   return ( char * ) pszCfg;
}

/* Builds platform dependant object module from Harbour C output */
void hb_compGenCObj( PHB_FNAME pFileName, const char * szSourceExtension )
{
   char     szFileName[ HB_PATH_MAX ];
   char     szLine[ HB_CFG_LINE_LEN + 1 ];
   char     szCompiler[ HB_CFG_LINE_LEN + 1 ]   = "";
   char     szOptions[ HB_CFG_LINE_LEN + 1 ]    = "";
   char     szCommandLine[ HB_CFG_LINE_LEN * 2 + 1 ];
   char     szOutPath[ HB_PATH_MAX ]            = "\0";

#if defined( HOST_OS_UNIX_COMPATIBLE )
   char     szDefaultPath[ HB_PATH_MAX ]        = "/etc:/usr/local/etc";
   char *   pszEnv                              = szDefaultPath;
   #define HB_CFG_FILENAME "harbour.cfg"
   #define HB_NULL_STR     " > /dev/null"
   #define HB_ACCESS_FLAG  F_OK
#elif defined( HB_OS_DOS_COMPATIBLE )
   char     szDefaultPath[ HB_PATH_MAX ]  = "PATH";
   char *   pszEnv                        = hb_getenv( "PATH" );
   #define HB_CFG_FILENAME "harbour.cfg"
   #define HB_NULL_STR     " >nul"
   #define HB_ACCESS_FLAG  0
#else
   char     szDefaultPath[ HB_PATH_MAX ] = NULL;
#endif

   FILE *   yyc;
   char *   pszCfg;
   BOOL     bVerbose = FALSE; /* Don't show C compiler messages (default). */
   BOOL     bDelTmp  = TRUE;  /* Delete intermediate C file (default). */
   int      iSuccess;

   /* First pass: build the C output */

   /* Force file extension to avoid collisions when called from a make utility
   */
   pFileName->szExtension = ".c";
   hb_fsFNameMerge( szFileName, pFileName );
   hb_compGenCCode( hb_comp_pFileName, szSourceExtension );

   /* Begin second pass */

   pszCfg = hb_getenv( "HB_CFG_FILE" );

   if( ! pszCfg )
   {
      /* Grab space */
      pszCfg = ( char * ) hb_xgrab( /*strlen( pszEnv )*/ HB_PATH_MAX );

      if( pszEnv && pszEnv[ 0 ] != '\0' )
      {
         if( ! *hb_searchpath( HB_CFG_FILENAME, pszEnv, pszCfg ) )
            pszCfg = NULL;
      }
   }

   if( pszCfg )
   {
      yyc = hb_fopen( pszCfg, "rt" );
      if( ! yyc )
      {
         hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "\nError: Can't open %s file.\n", pszCfg );
         hb_compOutErr( hb_comp_szMsgBuf );
         return;
      }

      while( fgets( szLine, HB_CFG_LINE_LEN, yyc ) != NULL )
      {
         HB_SIZE  ulLen;
         char *   szStr = szLine;
         char *   szToken;

         /* Trim left */
         while( HB_ISSPACE( *szStr ) )
            szStr++;

         /* Trim right */
         ulLen = strlen( szStr );
         while( ulLen && HB_ISSPACE( szStr[ ulLen - 1 ] ) )
            ulLen--;

         szStr[ ulLen ] = '\0';
         /* TODO: Check for comments within macros, i.e: CC=bcc32 #comment */

         if( *szStr )
         {
            szToken = strchr( szStr, '=' );

            if( szToken )
            {
               *szToken++ = '\0';
               if( *szToken )
               {
                  /* Checks compiler name */
                  if( ! hb_stricmp( szStr, "CC" ) )
                     hb_snprintf( szCompiler, sizeof( szCompiler ), "%s", szToken );
                  /* Checks optional switches */
                  else if( ! hb_stricmp( szStr, "CFLAGS" ) )
                     hb_snprintf( szOptions, sizeof( szCompiler ), "%s", szToken );
                  /* Wanna see C compiler output ? */
                  else if( ! hb_stricmp( szStr, "VERBOSE" ) )
                  {
                     if( ! hb_stricmp( szToken, "YES" ) )
                        bVerbose = TRUE;
                  }
                  /* Delete intermediate C file ? */
                  else if( ! hb_stricmp( szStr, "DELTMP" ) )
                  {
                     if( ! hb_stricmp( szToken, "NO" ) )
                        bDelTmp = FALSE;
                  }
               }
            }
         }
      }

      fclose( yyc );

   }
   else
   {
      printf(
         "\nError: Can't find %s file in %s.\n"
         "harbour.cfg is a text file that contains:\n"
         "CC=C compiler binary name eg. CC=gcc\n"
         "CFLAGS=C compiler options eg. -c -I<includes>\n"
         "       ( 'compile only' and harbour include dir are mandatory )\n"
         "VERBOSE=NO|YES to show steps messages default is NO\n"
         "DELTMP=NO|YES to delete generated C source default is YES\n"
         "remember also to properly set the C compiler env.\n", HB_CFG_FILENAME, szDefaultPath );
      return;
   }

#if defined( HB_OS_DOS_COMPATIBLE )
   {
      if( pszEnv )
         hb_xfree( ( void * ) pszEnv );
   }
#endif

   hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "\nBuilding object module for \'%s\'\nusing C compiler \'%s\' as defined in \'%s\'...\n", szFileName, szCompiler, pszCfg );
   hb_compOutStd( hb_comp_szMsgBuf );

   /* Check if -o<path> was used */
   if( hb_comp_pOutPath )
   {
      PHB_FNAME   pOut                    = hb_fsFNameSplit( ( char * ) szFileName );
      char        pszTemp[ HB_PATH_MAX ]  = "";

      if( hb_comp_pOutPath->szPath )
         pOut->szPath = hb_comp_pOutPath->szPath;

#if defined( __BORLANDC__ ) || defined( _MSC_VER ) || defined( __WATCOMC__ )
      pOut->szExtension = ".obj";
#else
      pOut->szExtension = ".o";  /* Don't know if we can hardcode it for Un*x */
#endif
      hb_fsFNameMerge( pszTemp, pOut );

#if defined( _MSC_VER )
      hb_strncat( szOutPath, "-Fo", sizeof( szOutPath ) - 1 );
#elif defined( __WATCOMC__ )
      hb_strncat( szOutPath, "-fo=", sizeof( szOutPath ) - 1 );
#else
      hb_strncat( szOutPath, "-o", sizeof( szOutPath ) - 1 );
#endif

      hb_strncat( szOutPath, pszTemp, sizeof( szOutPath ) - 1 );

      hb_xfree( pOut );
   }

   if( *szCompiler )
   {
      hb_snprintf( szCommandLine, sizeof( szCommandLine ), "%s %s %s %s", szCompiler, szOptions, szOutPath, szFileName );

      if( bVerbose )
      {
         hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Exec: %s\n", szCommandLine );
         hb_compOutStd( hb_comp_szMsgBuf );
      }
      else
         hb_xstrcat( szCommandLine, HB_NULL_STR, 0 );

      /* Compile it! */
      iSuccess = ( system( szCommandLine ) != -1 );

      if( iSuccess )
         hb_compOutStd( "Done.\n" );
      else
      {
         hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Failed to execute: \"%s\"\n", szCommandLine );
         hb_compOutErr( hb_comp_szMsgBuf );
      }

      /* Delete intermediate .c file */
      /* QUESTION: Leave this file if C compiler fails ? */
      if( bDelTmp ) /* && iSuccess ) */
      {
         if( bVerbose )
         {
            hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Deleting: \"%s\"\n", szFileName );
            hb_compOutStd( hb_comp_szMsgBuf );
         }

         remove( ( char * ) szFileName );

         if( bVerbose )
            hb_compOutStd( "Done.\n" );
      }
   }
   else
   {
      hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Error: No compiler defined in %s\n", HB_CFG_FILENAME );
      hb_compOutErr( hb_comp_szMsgBuf );
   }

   if( pszCfg )
      hb_xfree( pszCfg );
}
