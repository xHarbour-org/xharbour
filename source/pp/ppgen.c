/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    preprocessor static rules generator.
 *    It creates .c file with tables for defines/[x]translates/[x]commands
 *    found in given .ch or .prg file
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#define  __PPGEN__
#include "ppcore.c"

#include "pragma.h"
#include "hbset.h"
#include <ctype.h>

typedef struct _RESERVEDNAME
{
   char * szName;
   struct _RESERVEDNAME * pNext;
} RESERVEDNAME, *PRESERVEDNAME;

static const char * s_otherReservedName[] = {
   "HIDDEN",
   "EXPORT",
   "METHOD",
   "ASSIGN",
   "ACCESS",
   "DESTRUCTOR",
   "CLASSMETHOD",
   "CONSTRUCTOR",
   "CLASSDATA",
   "DATA",
   "CLASS",
   "CLASSVAR",
   "VAR",
   "DECLSUPERN"
};

static PRESERVEDNAME s_PPReservedName = NULL;

static BOOL hb_pp_NameFound( char * szName )
{
   BOOL        bFound    = FALSE;
   PRESERVEDNAME pTemp = s_PPReservedName;

   while( pTemp )
   {
      if( strncmp( pTemp->szName, szName, strlen( szName ) ) == 0  )
      {
         bFound = TRUE;
         break;
      }

      pTemp = pTemp->pNext;
   }

   return bFound;
}

static void hb_pp_CollectReservedName( char * szName )
{
   PRESERVEDNAME pTemp, pLast;

   szName = hb_strupr( szName );

   if( hb_pp_NameFound( szName ) )
      return;

   pTemp          = ( PRESERVEDNAME ) hb_xgrab( sizeof( RESERVEDNAME ) );
   pTemp->szName  = ( char * ) hb_xgrab( strlen( szName ) + 1 );
   hb_strncpy( pTemp->szName, szName, strlen( szName ) );

   pTemp->pNext = NULL;

   if( s_PPReservedName )
   {
      pLast = s_PPReservedName;

      while( pLast->pNext )
         pLast = pLast->pNext;

      pLast->pNext = pTemp;
   }
   else
   {
      s_PPReservedName = pTemp;
   }
}

/*
 * library functions used by PP core code
 * necessary to create standalone binaries
 */

#ifndef hb_xgrab
void * hb_xgrab( HB_SIZE ulSize )
{
   return malloc( ( size_t ) ulSize );
}
#endif

#ifndef hb_xgrabEx
void * hb_xgrabEx( HB_SIZE ulSize, const char *szSource, int iLine, const char *szFunc )
{
   HB_SYMBOL_UNUSED( szSource );
   HB_SYMBOL_UNUSED( iLine );
   HB_SYMBOL_UNUSED( szFunc );

   return malloc( ( size_t ) ulSize );
}
#endif

#ifndef hb_xrealloc
void * hb_xrealloc( void * pMem, HB_SIZE ulSize )
{
   return realloc( pMem, ( size_t ) ulSize );
}
#endif

#ifndef hb_xreallocEx
void * hb_xreallocEx( void * pMem, HB_SIZE ulSize, const char *szSource, int iLine, const char *szFunc )
{
   HB_SYMBOL_UNUSED( szSource );
   HB_SYMBOL_UNUSED( iLine );
   HB_SYMBOL_UNUSED( szFunc );

   return realloc( pMem, ( size_t ) ulSize );
}
#endif

#ifndef hb_xfree
void hb_xfree( void * pMem )
{
   free( pMem );
}
#endif

#ifndef hb_xfreebEx
void hb_xfreeEx( void *pMem, const char *szSource, int iLine, const char *szFunc )
{
   HB_SYMBOL_UNUSED( szSource );
   HB_SYMBOL_UNUSED( iLine );
   HB_SYMBOL_UNUSED( szFunc );

   free( pMem );
}
#endif

char * hb_conNewLine( void )
{
   return "\n";
}

void hb_conOutErr( const char * pStr, HB_SIZE ulLen )
{
   fprintf( stderr, "%.*s", ( int ) ( ulLen ? ulLen : strlen( pStr ) ), pStr );
   fflush( stderr );
}

void hb_conOutStd( const char * pStr, HB_SIZE ulLen )
{
   fprintf( stdout, "%.*s", ( int ) ( ulLen ? ulLen : strlen( pStr ) ), pStr );
   fflush( stdout );
}

HB_SIZE hb_xquery( USHORT uiMode )
{
   HB_SYMBOL_UNUSED( uiMode ); return 0;
}

const char * hb_fsNameConv( const char * szFileName, char ** pszFree )
{
   if( pszFree )
      *pszFree = NULL;
   return szFileName;
}

int hb_setGetDirSeparator( void )
{
   return HB_OS_PATH_DELIM_CHR;
}

ULONGLONG hb_verCvsID( void )
{
   return 0;
}

int hb_verSVNDateID( void )
{
   return 0;
}

const char * hb_verCvsChangeLogID( void )
{
   return NULL;
}

const char * hb_verCvsLastEntry( void )
{
   return NULL;
}

const char * hb_verFlagsC( void )
{
   return NULL;
}

const char * hb_verFlagsL( void )
{
   return NULL;
}

const char * hb_verFlagsPRG( void )
{
   return NULL;
}

void hb_compSetDeferredFlagOn( void )
{
   ;
}

void hb_compSetCOutput( int iOutput )
{
   HB_SYMBOL_UNUSED( iOutput );
}

/*
 * functions to create .c files with rules defined in given PP context
 */
static int hb_pp_writeTokenCount( PHB_PP_TOKEN pToken )
{
   int iToken = 0;

   while( pToken )
   {
      iToken += hb_pp_writeTokenCount( pToken->pMTokens ) + 1;
      pToken  = pToken->pNext;
   }
   return iToken;
}

static void hb_pp_writeToken( FILE * fout, PHB_PP_TOKEN pToken,
                              char * szName, int iToken, BOOL fLast )
{
   while( pToken )
   {
      int iOptional = hb_pp_writeTokenCount( pToken->pMTokens ), i;

      i = ( int ) strlen( szName );
      if( pToken->pNext )
         fprintf( fout, "   { %s +%2d", szName, iToken + iOptional + 1 );
      else
         fprintf( fout, "   { NULL%*s", i, "" );

      if( iOptional )
         fprintf( fout, ", %s +%2d", szName, iToken + 1 );
      else
         fprintf( fout, ", NULL%*s", i, "" );

      i = 16 - ( int ) strlen( pToken->value );
      fprintf( fout, ", \"%s%s\", %*s %2d,%2d, 0x%04x, %d }%s\n",
               pToken->value[ 0 ] == '\\' && pToken->value[ 1 ] == '\0' ? "\\" : "",
               pToken->value,
               i < 0 ? 0 : i, "",
               pToken->len, pToken->spaces,
               pToken->type | HB_PP_TOKEN_STATIC | HB_PP_TOKEN_PREDEFINED,
               pToken->index,
               fLast && ! pToken->pNext && iOptional == 0 ? "" : "," );

      if( iOptional )
         hb_pp_writeToken( fout, pToken->pMTokens, szName, iToken + 1,
                           pToken->pNext == NULL && fLast );

      iToken += iOptional + 1;
      pToken  = pToken->pNext;
   }
}

static void hb_pp_writeTokenList( FILE * fout, PHB_PP_TOKEN pTokenLst, char * szName )
{
   int iTokens;

   iTokens = hb_pp_writeTokenCount( pTokenLst );
   if( iTokens )
   {
      fprintf( fout, "static HB_PP_TOKEN %s[ %d ] = {\n",
               szName, iTokens );
      hb_pp_writeToken( fout, pTokenLst, szName, 0, TRUE );
      fprintf( fout, "};\n" );
   }
}

static int hb_pp_writeRules( FILE * fout, PHB_PP_RULE pFirst, char * szName )
{
   char        szMatch[ 16 ], szResult[ 16 ];
   ULONG       ulRepeatBits, ulBit;
   PHB_PP_RULE pRule;
   int         iRule;
   USHORT      u;

   iRule = 0;
   pRule = pFirst;
   while( pRule )
   {
      ++iRule;

      if( pRule->pMatch )
      {
         if( strlen( pRule->pMatch->value ) > 1 )
            hb_pp_CollectReservedName( pRule->pMatch->value );

         hb_snprintf( szMatch, sizeof( szMatch ), "s_%cm%03d", szName[ 0 ], iRule );
         hb_pp_writeTokenList( fout, pRule->pMatch, szMatch );
      }

      if( pRule->pResult )
      {
         hb_snprintf( szResult, sizeof( szResult ), "s_%cr%03d", szName[ 0 ], iRule );
         hb_pp_writeTokenList( fout, pRule->pResult, szResult );
      }
      pRule = pRule->pPrev;
   }

   fprintf( fout, "static const HB_PP_DEFRULE s_%s[ %d ] = {\n",
            szName, iRule );

   iRule = 0;
   pRule = pFirst;
   while( pRule )
   {
      ++iRule;

      if( pRule->pMatch )
         hb_snprintf( szMatch, sizeof( szMatch ), "s_%cm%03d", szName[ 0 ], iRule );
      else
         hb_strncpy( szMatch, "NULL   ", sizeof( szResult ) - 1 );
      if( pRule->pResult )
         hb_snprintf( szResult, sizeof( szResult ), "s_%cr%03d", szName[ 0 ], iRule );
      else
         hb_strncpy( szResult, "NULL   ", sizeof( szResult ) - 1 );

      ulRepeatBits = 0;
      for( u = 0, ulBit = 1; u < pRule->markers; ++u, ulBit <<= 1 )
      {
         if( pRule->pMarkers[ u ].canrepeat )
            ulRepeatBits |= ulBit;
      }
      fprintf( fout, "   { %s, %s, %d,%2d, 0x%04lx }%s\n",
               szMatch, szResult, HB_PP_CMP_MODE( pRule->mode ),
               pRule->markers, ulRepeatBits, pRule->pPrev ? "," : "" );
      pRule = pRule->pPrev;
   }
   fprintf( fout, "};\n\n" );
   return iRule;
}

static void hb_pp_generateInitFunc( FILE * fout, int iRules,
                                    char * szVar, char * szRule )
{
   fprintf( fout, "   hb_pp_initRules( &pState->p%s, &pState->i%s, ",
            szVar, szVar );
   if( iRules )
      fprintf( fout, "s_%s, %d );\n", szRule, iRules );
   else
      fprintf( fout, "NULL, 0 );\n" );
}

static void hb_pp_generateRules( FILE * fout, FILE * fword, PHB_PP_STATE pState )
{
   PRESERVEDNAME pTemp;
   int  iDefs = 0, iTrans = 0, iCmds = 0;
   UINT wNum = 0;

   fprintf( fout, "/*\n * $Id$\n */\n\n/*\n"
            " * Harbour Project source code:\n"
            " *    Build in preprocessor rules.\n"
            " *\n"
            " * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>\n"
            " * www - http://www.harbour-project.org\n"
            " *\n"
            " * This file is generated automatically by Harbour preprocessor\n"
            " * and is covered by the same license as Harbour PP\n"
            " */\n\n#define _HB_PP_INTERNAL\n#include \"hbpp.h\"\n\n" );

   fprintf( fword, "/*\n * $Id$\n */\n\n/*\n"
            " * Harbour Project source code:\n"
            " *    Word list which should not be used as variable names.\n"
            " *\n"
            " * Copyright 2012 Andi Jahja <andi.jahja@yahoo.co.id>\n"
            " * www - http://www.harbour-project.org\n"
            " *\n"
            " * This file is generated automatically by Harbour preprocessor\n"
            " * and is covered by the same license as Harbour PP\n"
            "*/\n\n#include \"hbapi.h\"\n"
            "#include \"hbcomp.h\"\n"
            "\nstatic const char * s_szReservedName[] = {\n" );

   if( pState->pDefinitions )
      iDefs = hb_pp_writeRules( fout, pState->pDefinitions, "def" );
   if( pState->pTranslations )
      iTrans = hb_pp_writeRules( fout, pState->pTranslations, "trs" );
   if( pState->pCommands )
      iCmds = hb_pp_writeRules( fout, pState->pCommands, "cmd" );

   fprintf( fout, "\nvoid hb_pp_setStdRules( PHB_PP_STATE pState )\n{\n" );
   hb_pp_generateInitFunc( fout, iDefs, "Definitions", "def" );
   hb_pp_generateInitFunc( fout, iTrans, "Translations", "trs" );
   hb_pp_generateInitFunc( fout, iCmds, "Commands", "cmd" );
   fprintf( fout, "}\n" );

   do
   {
      if ( ! hb_pp_NameFound( ( char * ) s_otherReservedName[ wNum ] ) )
         fprintf( fword, "   \"%s\",\n", ( char* ) s_otherReservedName[ wNum ] );

      ++ wNum;
   } while( wNum < sizeof( s_otherReservedName ) / sizeof( char * ) );

   pTemp = s_PPReservedName;

   do
   {
      fprintf( fword, "   \"%s\"", pTemp->szName );
      hb_xfree( pTemp->szName );
      pTemp = pTemp->pNext;
      hb_xfree( ( void * ) s_PPReservedName );
      s_PPReservedName = pTemp;
      fprintf( fword, s_PPReservedName ? ",\n" : "\n" );
   } while( pTemp );

   fprintf( fword, "};\n\n"
            "#define RESERVED_NAMES sizeof( s_szReservedName ) / sizeof( char * )\n\n"
            "BOOL hb_compReservedPPName( char * szName )\n"
            "{\n"
            "   UINT  wNum   = 0;\n"
            "   int   iFound = 1;\n"
            "   int   iLen   = ( int ) strlen( szName );\n\n"
            "   while( wNum < RESERVED_NAMES && iFound )\n"
            "   {\n"
            "      int u = ( int ) strlen( s_szReservedName[ wNum ] ) ;\n\n"
            "      if ( iLen == u )\n"
            "      {\n"
            "         int i, j = 0;\n\n"
            "         for ( i = 0; i < u; i ++ )\n"
            "         {\n"
            "            if ( szName[ i ] == s_szReservedName[ wNum ] [ i ] )\n"
            "               j ++;\n"
            "            else\n"
            "               break;\n"
            "         }\n\n"
            "         if ( j == u )\n"
            "            iFound = 0;\n"
            "      }\n\n"
            "      ++wNum;\n"
            "   }\n\n"
            "   return ( iFound == 0 );\n"
            "}\n" );
}

static void hb_pp_undefCompilerRules( PHB_PP_STATE pState )
{
   int            i;
   PHB_PP_RULE *  pRulePtr, pRule;
   char *         szRules[] = { "__HARBOUR__",
                                "__DATE__",
                                "__TIME__",
                                "__HB_MAIN__",
                                "__ARCH16BIT__",
                                "__ARCH32BIT__",
                                "__ARCH64BIT__",
                                "__LITTLE_ENDIAN__",
                                "__BIG_ENDIAN__",
                                "__PDP_ENDIAN__",
                                NULL };

   for( i = 0; szRules[ i ]; ++i )
      hb_pp_delDefine( pState, szRules[ i ] );

   pRulePtr = &pState->pDefinitions;
   while( *pRulePtr )
   {
      pRule = *pRulePtr;
      if( ! pRule->pMatch->pNext &&
          strncmp( pRule->pMatch->value, "__PLATFORM__", 12 ) == 0 )
      {
         *pRulePtr = pRule->pPrev;
         hb_pp_ruleFree( pRule );
         pState->iDefinitions--;
      }
      else
         pRulePtr = &pRule->pPrev;
   }
}

static int hb_pp_preprocesfile( PHB_PP_STATE pState, char * szRuleFile, char* szWordFile )
{
   int    iResult = 0;
   ULONG  ulLen;
   FILE * fWord = NULL;

   while( hb_pp_nextLine( pState, &ulLen ) != NULL && ulLen )
      ;

   if( szWordFile )
   {
      fWord = hb_fopen( szWordFile, "w" );

      if( ! fWord )
      {
         perror( szWordFile );
         iResult = 1;
      }
   }

   if( iResult == 0 && szRuleFile )
   {
      FILE * foutr;

      foutr = hb_fopen( szRuleFile, "w" );
      if( ! foutr )
      {
         perror( szRuleFile );
         iResult = 1;
      }
      else
      {
         hb_pp_undefCompilerRules( pState );
         hb_pp_generateRules( foutr, fWord, pState );
         fclose( foutr );
         fclose( fWord );
      }
   }

   return iResult;
}

static int hb_pp_generateVerInfo( char * szVerFile, char * szCVSID, char * szCVSDateID, char * szChangeLogID, char * szLastEntry )
{
   int     iResult = 0;
   char *  pszEnv;
   FILE *  fout;

   fout = hb_fopen( szVerFile, "w" );
   if( ! fout )
   {
#if ! defined( __MINGW32CE__ ) && ! defined( HB_OS_WIN_CE )
      perror( szVerFile );
#endif
      iResult = 1;
   }
   else
   {
      fprintf( fout, "/*\n * $Id$\n */\n\n/*\n"
               " * Harbour Project source code:\n"
               " *    Version information and build time switches.\n"
               " *\n"
               " * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>\n"
               " * www - http://www.harbour-project.org\n"
               " *\n"
               " * This file is generated automatically by Harbour preprocessor\n"
               " * and is covered by the same license as Harbour PP\n"
               " */\n\n" );

      fprintf( fout, "\n#ifndef __HBVERBLD_INCLUDED" );
      fprintf( fout, "\n#define __HBVERBLD_INCLUDED\n" );

      if( szCVSID )
      {
         fprintf( fout, "\n#define HB_VER_GITFAKEID\t%s\n", szCVSID );
         fprintf( fout, "#define HB_VER_CVSID\t\t%s\n", szCVSID );
      }

      if( szCVSDateID )
      {
         fprintf( fout, "\n#if defined(HB_VER_BUILDDATE)" );
         fprintf( fout, "\n#undef HB_VER_BUILDDATE" );
         fprintf( fout, "\n#endif\n" );
         fprintf( fout, "#define HB_VER_BUILDDATE\t%s\n", szCVSDateID );
      }

      if( szChangeLogID )
      {
         fprintf( fout, "\n#if defined(HB_VER_CHLCVS)" );
         fprintf( fout, "\n#undef HB_VER_CHLCVS" );
         fprintf( fout, "\n#endif\n" );
         fprintf( fout, "\n#define HB_VER_CHLCVS\t\"%s\"\n", szChangeLogID );
      }

      if( szLastEntry )
      {
         fprintf( fout, "\n#if defined(HB_VER_LENTRY)" );
         fprintf( fout, "\n#undef HB_VER_LENTRY" );
         fprintf( fout, "\n#endif\n" );
         fprintf( fout, "\n#define HB_VER_LENTRY\t\"%s\"\n", szLastEntry );
      }

      pszEnv = hb_getenv( "C_USR" );
      if( pszEnv )
      {
         fprintf( fout, "\n#undef  HB_VER_C_USR" );
         fprintf( fout, "\n#define HB_VER_C_USR\t\"%s\"\n", pszEnv );
         hb_xfree( pszEnv );
      }

      pszEnv = hb_getenv( "L_USR" );
      if( pszEnv )
      {
         fprintf( fout, "\n#undef  HB_VER_L_USR" );
         fprintf( fout, "\n#define HB_VER_L_USR\t\"%s\"\n", pszEnv );
         hb_xfree( pszEnv );
      }

      pszEnv = hb_getenv( "PRG_USR" );
      if( pszEnv )
      {
         fprintf( fout, "\n#undef  HB_VER_PRG_USR" );
         fprintf( fout, "\n#define HB_VER_PRG_USR\t\"%s\"\n", pszEnv );
         hb_xfree( pszEnv );
      }

      fprintf( fout, "\n#endif /* __HBVERBLD_INCLUDED */\n" );

      fclose( fout );
   }

   return iResult;
}

static int hb_pp_parseChangelog( PHB_PP_STATE pState, const char * pszFileName,
                                 BOOL fQuiet, char **pszGitFakeID, char **pszChgLogDateID,
                                 char ** pszChangeLogGitHash, char **pszLastEntry )
{
   int           iResult = 0;
   const char *  pszFile;
   FILE *        file_in;

   pszFile = pszFileName ? pszFileName : "../../../../ChangeLog";

   do
   {
      if( hb_fsFileExists( pszFile ) )
         break;
      pszFile += 3;
   }
   while( ! pszFileName && ( *pszFile == '.' || *pszFile == 'C' ) );

   file_in = hb_fopen( pszFile, "r" );
   if( ! file_in )
   {
      if( ! fQuiet )
      {
#if ! defined( __MINGW32CE__ ) && ! defined( HB_OS_WIN_CE )
         perror( pszFile );
#else
         fprintf( stderr, "Cannot open the %s file.\n", pszFile );
#endif
      }
      iResult = 1;
   }
   else
   {
      char szLine[ 256 ]             = { 0 };
      char szChangeLogGitHash[ 256 ] = { 0 };
      char szLastEntry[256]          = { 0 };
      char szChgLogDateID[9]         = { 0 };
      char szChgLogTimeID[5]         = { 0 };
      char szGitFakeID[32]           = { 0 };
   
      do
      {
         char *szFrom, *szUTC;

         if( ! fgets( szLine, sizeof( szLine ), file_in ) )
            break;

         if( szChangeLogGitHash[0] == 0 && ( szFrom = strstr( szLine, " $Id: " ) ) != NULL )
         {
            char *szTo = strstr( szFrom + 1, " $" );
            hb_strncpy( szChangeLogGitHash, szFrom + 6, szTo - szFrom - 6 );

            //fprintf( stderr, "ChangeLog Git Hash: '%s'\n", szChangeLogGitHash );  
         }
         else if( szChgLogDateID[0] == 0 && ( ( ( szUTC = strstr( szLine, "UTC-" ) ) != NULL ) || ( ( szUTC = strstr( szLine, "UTC+" ) ) != NULL ) ) && szUTC - szLine == 17 )
         {
            if( szLine[ strlen( szLine ) - 2 ] == '\r' )
            {
               hb_strncpy( szLastEntry, szLine, strlen( szLine ) - 2 );
            }
            else
            {
               hb_strncpy( szLastEntry, szLine, strlen( szLine ) - 1 );
            }

            szChgLogDateID[0] = szLine[0];
            szChgLogDateID[1] = szLine[1];
            szChgLogDateID[2] = szLine[2];
            szChgLogDateID[3] = szLine[3];
            szChgLogDateID[4] = szLine[5];
            szChgLogDateID[5] = szLine[6];
            szChgLogDateID[6] = szLine[8];
            szChgLogDateID[7] = szLine[9];
            szChgLogDateID[8] = '\0';

            //fprintf( stderr, "ChangeLog Date ID: '%s'\n", szChgLogDateID );

            szChgLogTimeID[0] = szLine[11];
            szChgLogTimeID[1] = szLine[12];
            szChgLogTimeID[2] = szLine[14];
            szChgLogTimeID[3] = szLine[15];
            szChgLogTimeID[4] = '\0';

            //fprintf( stderr, "ChangeLog Time ID: '%s'\n", szChgLogTimeID );
            // HB_VER_GITFAKEID and HB_VER_CVSID with ULL suffix
            snprintf( szGitFakeID, sizeof( szGitFakeID ), "HB_ULL( %s%s )", szChgLogDateID, szChgLogTimeID );
            //fprintf( stderr, "Git Fake ID: '%s'\n", szGitFakeID );
         }
      }
      while( ! feof( file_in ) );

      fclose( file_in );

      if( szChangeLogGitHash[0] == 0 && szChgLogDateID[0] == 0 && szChgLogTimeID[0] == 0 && szLastEntry[0] == 0 )
      {
         if( ! fQuiet )
            fprintf( stderr, "Invalid $Id or Change Log\'s format in file: '%s'.\n", pszFile );

         iResult = 1;
      }
      else
      {
         hb_pp_addDefine( pState, "HB_VER_LENTRY", szLastEntry );
         *pszLastEntry = hb_strdup( szLastEntry );

         hb_pp_addDefine( pState, "HB_VER_CHLHASH", szChangeLogGitHash );
         *pszChangeLogGitHash = hb_strdup( szChangeLogGitHash );

         hb_pp_addDefine( pState, "HB_VER_GITFAKEID", szGitFakeID );
         hb_pp_addDefine( pState, "HB_VER_CVSID", szGitFakeID ); 
         *pszGitFakeID = hb_strdup( szGitFakeID );

         hb_pp_addDefine( pState, "HB_VER_BUILDDATE", szChgLogDateID );
         *pszChgLogDateID = hb_strdup( szChgLogDateID );
      }
   }

   return iResult;
}

/*
 * ppgen only functions
 */
static void hb_pp_usage( char * szName )
{
   printf( "Syntax:  %s <file>[.prg] [options]\n\n", szName );
   printf( "Options: -i<path>  \tadd #include file search path\n"
           "         -c[<file>]\tlook for ChangeLog file\n"
           "         -o<file>  \tcreates .c file with PP rules\n"
           "         -v<file>  \tcreates .h file with version information\n"
           "         -w        \twrite preprocessed (.ppo) input file\n"
           "         -q        \tdisable information messages\n" );
}

int main( int argc, char * argv[] )
{
   char *        szFile        = NULL, * szRuleFile = NULL, * szWordFile = NULL, * szVerFile = NULL;
   char *        szLogFile     = NULL;
   BOOL          fQuiet        = FALSE, fWrite = FALSE, fChgLog = FALSE;
   char *        szChangeLogID = NULL, * szLastEntry = NULL;
   int           iResult       = 0, i;
   PHB_PP_STATE  pState;

   pState = hb_pp_new();

   if( argc >= 2 )
   {
      szFile = argv[ 1 ];
      for( i = 2; szFile && i < argc; i++ )
      {
         if( ! HB_ISOPTSEP( argv[ i ][ 0 ] ) )
            szFile = NULL;
         else
         {
            switch( argv[ i ][ 1 ] )
            {
               case 'q':
               case 'Q':
                  if( argv[ i ][ 2 ] )
                     szFile = NULL;
                  else
                     fQuiet = TRUE;
                  break;

               case 'w':
               case 'W':
                  if( argv[ i ][ 2 ] )
                     szFile = NULL;
                  else
                     fWrite = TRUE;
                  break;

               case 'c':
               case 'C':
                  fChgLog = TRUE;
                  if( argv[ i ][ 2 ] )
                     szLogFile = argv[ i ] + 2;
                  break;

               case 'i':
               case 'I':
                  if( argv[ i ][ 2 ] )
                     hb_pp_addSearchPath( pState, argv[ i ] + 2, FALSE );
                  else
                     szFile = NULL;
                  break;

               case 'o':
               case 'O':
                  if( argv[ i ][ 2 ] )
                     szRuleFile = argv[ i ] + 2;
                  else
                     szFile = NULL;
                  break;

               case 'x':
               case 'X':
                  if( argv[ i ][ 2 ] )
                     szWordFile = argv[ i ] + 2;
                  else
                     szWordFile = NULL;
                  break;

               case 'v':
               case 'V':
                  if( argv[ i ][ 2 ] )
                     szVerFile = argv[ i ] + 2;
                  else
                     szFile = NULL;
                  break;

               default:
                  szFile = NULL;
                  break;
            }
         }
      }
   }

   if( szFile )
   {
      hb_pp_init( pState, fQuiet, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL );

      if( hb_pp_inFile( pState, szFile, TRUE, NULL, TRUE ) )
      {
         char * szSVNID = NULL;
         char * szSVNDateID = NULL;

         if( fWrite )
         {
            char        szFileName[ HB_PATH_MAX ];
            PHB_FNAME   pFileName;

            pFileName               = hb_fsFNameSplit( szFile );
            pFileName->szExtension  = ".ppo";
            hb_fsFNameMerge( szFileName, pFileName );
            hb_xfree( pFileName );

            hb_pp_outFile( pState, szFileName, NULL );
         }

         if( fChgLog )
            iResult = hb_pp_parseChangelog( pState, szLogFile, fQuiet,
                                            &szSVNID, &szSVNDateID, &szChangeLogID, &szLastEntry );

         if( iResult == 0 )
            iResult = hb_pp_preprocesfile( pState, szRuleFile, szWordFile );

         if( iResult == 0 && szVerFile )
            iResult = hb_pp_generateVerInfo( szVerFile, szSVNID, szSVNDateID,
                                             szChangeLogID, szLastEntry );

         if( szSVNID )
            hb_xfree( szSVNID );

         if( szSVNDateID )   
            hb_xfree( szSVNDateID );
      }
      else
         iResult = 1;
   }
   else
   {
      hb_pp_usage( argv[ 0 ] );
      iResult = 1;
   }

   if( szChangeLogID )
      hb_xfree( szChangeLogID );
   if( szLastEntry )
      hb_xfree( szLastEntry );

   hb_pp_free( pState );

   return iResult;
}

#if defined( HB_OS_WIN_CE ) && ! defined( __CEGCC__ )
#  include "hbwmain.c"
#endif
