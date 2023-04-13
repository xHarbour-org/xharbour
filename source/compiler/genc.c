/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source generation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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


#include <assert.h>
#include <time.h>

#include "hbcomp.h"
#include "hbdate.h"
#include "hbexemem.h"
#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

extern void hb_compGenCRealCode( PFUNCTION pFunc, FILE * yyc );
extern void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc );
extern void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc );

static void hb_compGenCInLine( FILE * );
static void hb_writeEndInit( FILE * yyc, const char * szModuleName );
static void hb_compGenCAddProtos( FILE * yyc );

// AJ: 2004-02-05
// Routines to check C-In-Line static function declared in a PRG file
// with #PRAGMA BEGINDUMP - ENDDUMP
static void hb_compGenCInLineSymbol( void );
static void hb_compGenCCheckInLineStatic( const char * str );
static BOOL hb_compCStaticSymbolFound( const char * szSymbol, int iOption );
static BOOL hb_compSymbFound( const char * szSymbol );
static void hb_compWriteGlobalFunc( FILE * yyc, short * iLocalGlobals, short * iGlobals, BOOL );
static void hb_compWriteDeclareGlobal( FILE * yyc, short iLocalGlobals );
static BOOL hb_compWriteExternEntries( FILE * yyc, BOOL bSymFIRST, BOOL, BOOL, const char * szModuleName );
static void hb_compWritePragma( FILE * yyc, const char * szPrefix, const char * szModuleName );
extern void hb_gencc_string_put( FILE * yyc, BYTE * pText, HB_SIZE usLen );
/* struct to hold symbol names of c-in-line static functions */
typedef struct _SSYMLIST
{
   char * szName;
   int Type;
   USHORT uEntry;
   struct _SSYMLIST * pNext;
} SSYMLIST, * PSSYMLIST;

static PSSYMLIST pStatSymb = NULL;

#define HB_PROTO_FUNC_STATIC  1
#define HB_PROTO_FUNC_PUBLIC  2
#define HB_PROTO_FUNC_INIT    3
#define HB_PROTO_FUNC_EXIT    4

PNAMESPACE hb_compGenerateXNS( PNAMESPACE pNamespace, void ** pCargo )
{
   FILE *      yyc      = *( ( FILE ** ) pCargo );
   FILE *      yycOuter = yyc;
   PNAMESPACE  pOuter   = NULL;
   PNAMESPACE  pMember;
   int         iLevel;
   char *      szFileName;

   if( ( pNamespace->type & NSTYPE_STEALTH ) == NSTYPE_STEALTH )
      return pNamespace;
   else if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
   {
      szFileName  = hb_xstrcpy( NULL, pNamespace->szName, ".hxns", NULL );
      yyc         = hb_fopen( szFileName, "ab" );
   }
   else
   {
      szFileName  = hb_xstrcpy( NULL, pNamespace->szName, ".xns", NULL );
      yyc         = hb_fopen( szFileName, "wb" );
   }

   if( ! yyc )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );

   hb_xfree( szFileName );

   if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
   {
      fprintf( yyc, "#if defined( NAMESPACE_TYPEDEF ) && ! defined( %s_TYPEDEF )\n", pNamespace->szName );
      fprintf( yyc, "#define %s_TYPEDEF\n", pNamespace->szName );

      fprintf( yyc, "#if defined( __PRG__ )\n" );
      fprintf( yyc, "DEFINE NAMESPACE %s\n", pNamespace->szName );
      fprintf( yyc, "#else\n" );
      fprintf( yyc, "struct\n" );
   }
   else
   {
      fprintf( yyc, "#ifndef NAMESPACE_%s\n", pNamespace->szName );
      fprintf( yyc, "#define NAMESPACE_%s\n", pNamespace->szName );

      fprintf( yyc, "#if defined( __PRG__ )\n" );
      fprintf( yyc, "DEFINE NAMESPACE %s\n", pNamespace->szName );
      fprintf( yyc, "#else\n" );
      fprintf( yyc, "#define NSID_%s %d\n", pNamespace->szName, pNamespace->iID );
      fprintf( yyc, "typedef struct\n" );
   }
   fprintf( yyc, "{\n" );
   fprintf( yyc, "#endif\n" );

   iLevel   = 1;
   pMember  = pNamespace->pNext;

   while( pMember )
   {
      if( pMember->type & NSTYPE_SPACE )
      {
         if( ( pMember->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS && pMember->pOuter == NULL )
         {
            hb_compGenerateXNS( pMember, pCargo );
            pMember  = hb_compNamespaceEnumSkipMembers( pMember->pNext );
            pMember  = pMember->pNext;
            continue;
         }
         else if( ( pMember->type & NSTYPE_EXTERNAL ) == NSTYPE_EXTERNAL )
         {
            fprintf( yyc, "   #define NAMESPACE_TYPEDEF\n" );
            fprintf( yyc, "   #include \"%s.hxns\"\n", pMember->szName );
            fprintf( yyc, "   #undef NAMESPACE_TYPEDEF\n" );

            pMember = pMember->pNext;
            continue;
         }

         ++iLevel;
         pOuter = pMember;

         fprintf( yyc, "   #if defined( __PRG__ )\n" );
         fprintf( yyc, "   DEFINE NAMESPACE %s\n", pMember->szName );
         fprintf( yyc, "   #else\n" );
         fprintf( yyc, "   struct /* %s */\n", pMember->szName );
         fprintf( yyc, "   {\n" );
         fprintf( yyc, "   #endif\n" );
      }
      else if( pMember->type & NSTYPE_MEMBER )
      {
         if( ( pMember->type & NSTYPE_STATIC ) == NSTYPE_STATIC )
         {
            /* Don't publish */
         }
         else if( pMember->pFunc && ( pMember->pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) & ~HB_FS_STATIC ) )
         {
            /* Don't publish */
         }
         else
         {
            fprintf( yyc, "   #if defined( __PRG__ )\n" );
            fprintf( yyc, "   DEFINE NAMESPACE MEMBER %s\n", pMember->szName );
            fprintf( yyc, "   #else\n" );
            fprintf( yyc, "   PHB_FUNC %s;\n\n", pMember->szName );  // Don't remove DOUBLE \n!!!
            fprintf( yyc, "   #endif\n" );
         }
      }

      if( pMember->type & NSTYPE_END )
      {
         if( --iLevel == 0 )
            break;

         fprintf( yyc, "   #if defined( __PRG__ )\n" );
         fprintf( yyc, "   END\n" );
         fprintf( yyc, "   #else\n" );
         fprintf( yyc, "   } %s;\n\n", pOuter->szName );  /* Don't remove DOUBLE \n!!! */
         fprintf( yyc, "   #endif\n" );

         pOuter = pOuter->pOuter;
      }

      pMember = pMember->pNext;
   }

   if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
   {
      fprintf( yyc, "#if defined( __PRG__ )\n" );
      fprintf( yyc, "END\n" );
      fprintf( yyc, "#else\n" );
      fprintf( yyc, "} %s;\n\n", pNamespace->szName );  /* Don't remove DOUBLE \n!!! */
      fprintf( yyc, "#endif\n" );
      fprintf( yyc, "#endif\n" );
      fprintf( yyc, "\n" );
      fprintf( yyc, "#if defined( NAMESPACE_INIT ) && ! defined( __PRG__ ) \n" );
   }
   else
   {
      fprintf( yyc, "#if defined( __PRG__ )\n" );
      fprintf( yyc, "END\n" );
      fprintf( yyc, "#else\n" );
      fprintf( yyc, "} HB_NS_%s;\n\n", pNamespace->szName );  /* Don't remove DOUBLE \n!!! */
      fprintf( yyc, "#endif\n" );
      fprintf( yyc, "#endif\n" );

      fclose( yyc );

      yyc = yycOuter;

      fprintf( yyc, "\n#include \"%s.xns\"\n", pNamespace->szName );

      fprintf( yyc, "\nHB_NS_%s %s =\n", pNamespace->szName, pNamespace->szName );
   }

   fprintf( yyc, "   {\n" );

   iLevel   = 1;
   pOuter   = pNamespace;
   pMember  = pNamespace->pNext;

   while( pMember )
   {
      if( pMember->type & NSTYPE_SPACE )
      {
         if( ( pMember->type & NSTYPE_EXTERNAL ) == NSTYPE_EXTERNAL )
         {
            fprintf( yyc, "   #define NAMESPACE_INIT\n" );
            fprintf( yyc, "   #include \"%s.hxns\"\n", pMember->szName );
            fprintf( yyc, "   #undef NAMESPACE_INIT\n" );

            pMember = pMember->pNext;
            continue;
         }

         ++iLevel;
         pOuter = pMember;

         fprintf( yyc, "   { /* %s */\n", pMember->szName );
      }
      else if( pMember->type & NSTYPE_MEMBER )
      {
         if( ( pMember->type & NSTYPE_STATIC ) == NSTYPE_STATIC )
         {
            /* Don't publish */
         }
         else if( pMember->pFunc && ( pMember->pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) & ~HB_FS_STATIC ) )
         {
            /* Don't publish */
         }
         else if( ( pMember->pOuter->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
            fprintf( yyc, "     /* %s = */ HB_OPTIONAL_NAMESPACE_FUNCNAME( %d, %s ),\n", pMember->szName, pMember->iID, pMember->szName );
         else if( ( pMember->type & NSTYPE_EXTERNAL ) == NSTYPE_EXTERNAL )
            fprintf( yyc, "     /* %s = */ HB_EXTERNAL_NAMESPACE_FUNCNAME( %d, %s ),\n", pMember->szName, pNamespace->iID, pMember->szName );
         else
         {
            if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
               fprintf( yyc, "     /* %s = */ HB_NAMESPACE_FUNCNAME( %d, %s ),\n", pMember->szName, pMember->iID, pMember->szName );
            else
               fprintf( yyc, "     /* %s = */ HB_NAMESPACE_FUNCNAME( %d, %s ),\n", pMember->szName, pOuter->iID, pMember->szName );
         }
      }

      if( pMember->type & NSTYPE_END )
      {
         if( --iLevel == 0 )
            break;

         pOuter = pOuter->pOuter;

         fprintf( yyc, "   },\n" );
      }

      pMember = pMember->pNext;
   }

   if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
   {
      fprintf( yyc, "   },\n" );
      fprintf( yyc, "#endif\n" );

      fclose( yyc );
   }
   else
      fprintf( yyc, "   };\n" );

   return pNamespace;
}

void hb_compGenCCode( PHB_FNAME pFileName, const char * szSourceExtension )      /* generates the C language output */
{
   char        szExternName[ HB_PATH_MAX ];
   char        szExtName[ HB_PATH_MAX ];
   char        szFileName[ HB_PATH_MAX ];
   char        szModuleName[ HB_PATH_MAX ];
   char        szSourceName[ HB_PATH_MAX ], * pTmp;
   PFUNCTION   pFunc;
   PFUNCALL    pFunCall;
   PCOMSYMBOL  pSym           = hb_comp_symbols.pFirst;
   FILE *      yyc;                    /* file handle for C output */
   FILE *      fExtern        = NULL;  /* file handle all public function */
   FILE *      fCodeExt       = NULL;  /* file handle for external function required by pCode.DLL */
   PINLINE     pInline        = hb_comp_inlines.pFirst;
   short       iLocalGlobals  = 0, iGlobals = 0;

   BOOL        bIsStaticFunction;
   BOOL        bIsInitFunction;
   BOOL        bIsExitFunction;
   BOOL        bIsStaticVariable;
   BOOL        bIsGlobalVariable;
   BOOL        bIsLineNumberInfo;

   BOOL        bCritical = FALSE;

   int         iSymOffset, iStartupOffset;

   PSSYMLIST   pTemp;
   BOOL        bSymFIRST         = FALSE;

   BOOL        bBeginExt         = FALSE;

   BOOL        bIsUsedNamespaces = FALSE;

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".c";

   if ( hb_comp_OutputIsCpp )
      pFileName->szExtension = ".cpp";

   hb_fsFNameMerge( szFileName, pFileName );

   /*
      Automatically generate filename.xbx - list of public functions useable
      for generating extern.ch
   */
   pFileName->szExtension  = ".xbx";
   hb_fsFNameMerge( szExternName, pFileName );

   pFileName->szExtension  = szSourceExtension;
   pFileName->szPath       = NULL;
   hb_fsFNameMerge( szSourceName, pFileName );


   while( ( pTmp = strchr( szSourceName, '\\' ) ) != NULL )
      *pTmp = '/';

   yyc = hb_fopen( szFileName, "wb" );

   if( ! yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( hb_comp_createExternList )
   {
      fExtern = hb_fopen( szExternName, "wb" );;
      if( ! fExtern )
      {
         hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szExternName, NULL );
         return;
      }
      fprintf( fExtern,
         "; List of Public Functions\n"
         "; Module: %s\n\n", szSourceName );
   }

   /*
    * Create *.dyn when /vd is used
    */
   if( hb_comp_autoDeferred )
   {
      pFileName->szExtension  = ".dyn";
      hb_fsFNameMerge( szExtName, pFileName );
      fCodeExt                = hb_fopen( szExtName, "wb" );
   }

   /*
    * Automatically generate filename.xbx - list of public functions useable
    * for generating extern.ch
   */

   /*
    * Create *.p when /gc4 is used
    */
   hb_compPCodeStat( pFileName );

   hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Generating C source output to \'%s\'...\n", szFileName );
   hb_compOutStd( hb_comp_szMsgBuf );

   hb_strncpyUpper( szModuleName, pFileName->szName, sizeof( szModuleName ) - 1 );
   /* replace non ID characters in name of local symbol table by '_' */
   {
      HB_SIZE iLen = strlen( szModuleName ), i;

      for( i = 0; i < iLen; i++ )
      {
         char c = szModuleName[ i ];

         if( ! ( c >= 'A' && c <= 'Z' ) &&
             ! ( c >= 'a' && c <= 'z' ) &&
             ! ( c >= '0' && c <= '9' ) &&
             ! ( c == '_' ) )
         {
            szModuleName[ i ] = '_';
         }
      }
   }

   {
      char *      szComp   = hb_verCompiler();
      char *      szHrb    = hb_verHarbour();

#if defined( HB_OS_WIN )
      SYSTEMTIME  t;
      GetLocalTime( &t );
#else
      time_t      t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );
#endif
      fprintf( yyc, "/*\n * %s\n", szHrb );
      /* AJ: Some compilers performs [f]printf("<%s>",string) incorrecltly */
      fprintf( yyc, " * Generated C source code from %s%s%s\n", "<", hb_comp_PrgFileName, ">" );
      if( hb_Command_Line && *hb_Command_Line )
         fprintf( yyc, " * Command: %s\n", hb_Command_Line );
#if defined( HB_OS_WIN )
      fprintf( yyc, " * Created: %04d.%02d.%02d %02d:%02d:%02d (%s)\n */\n\n", t.wYear, t.wMonth, t.wDay, t.wHour, t.wMinute, t.wSecond, szComp );
#else
      fprintf( yyc, " * Created: %04d.%02d.%02d %02d:%02d:%02d (%s)\n */\n\n", oTime->tm_year + 1900, oTime->tm_mon + 1, oTime->tm_mday, oTime->tm_hour, oTime->tm_min, oTime->tm_sec, szComp );
#endif
      hb_xfree( szComp );
      hb_xfree( szHrb );
   }

   if( hb_comp_autoDeferred )
   {
      fprintf( yyc,
         "#if defined( __cplusplus ) && !defined( HB_STATIC_STARTUP )\n"
         "   #define HB_STATIC_STARTUP\n"
         "#endif\n\n" );
   }

   if( hb_comp_iFunctionCnt || hb_comp_Namespaces.pFirst )
   {
      fprintf( yyc, "#include \"hbvmpub.h\"\n" );

      if( hb_comp_iGenCOutput == HB_COMPGENC_REALCODE )
      {
         fprintf( yyc,
            "#include \"hbxvm.h\"\n"
            "#include \"hbapierr.h\"\n" );
      }
      else if( hb_comp_iGenCOutput != HB_COMPGENC_COMPACT )
         fprintf( yyc, "#include \"hbpcode.h\"\n" );

      fprintf( yyc,
         "#include \"hbinit.h\"\n\n"
         "#define __PRG_SOURCE__ \"%s\"\n\n", hb_comp_PrgFileName );

      if( hb_comp_Namespaces.pFirst )
      {
         PNAMESPACE pNamespace = hb_comp_Namespaces.pFirst;

         do
         {
            if( pNamespace->type & NSTYPE_SPACE )
            {
               if( ( pNamespace->type & NSTYPE_STEALTH ) == NSTYPE_STEALTH )
                  fprintf( yyc, "#include \"%s.xns\"\n", pNamespace->szName );
            }

            pNamespace = pNamespace->pNext;
         }
         while( pNamespace );

         fprintf( yyc, "\n" );
      }

      pFunc = hb_comp_functions.pFirst;

      if( ! hb_comp_bStartProc )
         pFunc = pFunc->pNext; /* No implicit starting procedure */

#ifdef BROKEN_MODULE_SPACE_LOGIC
      fprintf( yyc, "HB_FUNC_STATIC( __begin__ );\n" );
#endif

      fprintf( yyc, "/* Forward declarations of all PRG defined Functions. */\n" );

      /* write functions prototypes for PRG defined functions */
      while( pFunc )
      {
         bIsInitFunction   = ( pFunc->cScope & HB_FS_INIT & ~HB_FS_STATIC );
         bIsExitFunction   = ( pFunc->cScope & HB_FS_EXIT & ~HB_FS_STATIC );
         bIsStaticFunction = bIsInitFunction == FALSE && bIsExitFunction == FALSE && ( pFunc->cScope & HB_FS_STATIC );
         bIsStaticVariable = ( pFunc == hb_comp_pInitFunc );
         bIsGlobalVariable = ( pFunc == hb_comp_pGlobalsFunc );
         bIsLineNumberInfo = ( pFunc == hb_comp_pLineNumberFunc );

         if( pFunc->cScope & HB_FS_CRITICAL )
            bCritical = TRUE;

         if( pFunc->pNamespace && ( HB_ISALPHA( pFunc->szName[ 0 ] ) || pFunc->szName[ 0 ] == '_' ) )
         {
            if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
            {
               if( bIsStaticFunction )
                  fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pFunc->szName );
               else
                  fprintf( yyc, "HB_FUNC_OPTIONAL_NAMESPACE( /* %s */ %d, %s );\n", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
            }
            else if( ( pFunc->pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
               fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ %d, %s );\n", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
            else if( ( pFunc->pNamespace->type & NSTYPE_STEALTH ) == NSTYPE_STEALTH )
               fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ NSID_%s, %s );\n", pFunc->pNamespace->szFullPath, pFunc->pNamespace->szName, pFunc->szName );
            else
               fprintf( yyc, "HB_FUNC_NAMESPACE( /* %s */ %d, %s );\n", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
         }
         /* Is it a PUBLIC FUNCTION/PROCEDURE */
         else if( bIsStaticFunction )
            fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pFunc->szName );
         /* Is it a STATIC$ */
         else if( bIsStaticVariable )
            fprintf( yyc, "HB_FUNC_INITSTATICS();\n" ); /* NOTE: hb_ intentionally in lower case */
         /* Is it a GLOBAL$ */
         else if( bIsGlobalVariable )
            fprintf( yyc, "HB_FUNC_INITGLOBALS();\n" ); /* NOTE: hb_ intentionally in lower case */
         /* Is it an (_INITLINES) function */
         else if( bIsLineNumberInfo )
            fprintf( yyc, "HB_FUNC_INITLINES();\n" );
         /* Is it an INIT FUNCTION/PROCEDURE */
         else if( bIsInitFunction )
            fprintf( yyc, "HB_FUNC_INIT( %.*s );\n", ( int ) strlen( pFunc->szName ) - 1, pFunc->szName );
         /* Is it an EXIT FUNCTION/PROCEDURE */
         else if( bIsExitFunction )
            fprintf( yyc, "HB_FUNC_EXIT( %.*s );\n", ( int ) strlen( pFunc->szName ) - 1, pFunc->szName );
         /* Then it must be a STATIC FUNCTION/PROCEDURE */
         else
            fprintf( yyc, "HB_FUNC( %s );\n", pFunc->szName );

         pFunc = pFunc->pNext;
      }

      {
         PNAMESPACE pNamespace = hb_comp_Namespaces.pFirst;

         while( pNamespace )
         {
            if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
            {
               char *      szFileName  = hb_xstrcpy( NULL, pNamespace->szName, ".hxns", NULL );
               FILE *      yyc;
               PNAMESPACE  pMember;
               int         iLevel      = 1;

               yyc = hb_fopen( szFileName, "wb" );

               if( ! yyc )
                  hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );

               hb_xfree( szFileName );

               fprintf( yyc, "#if defined( NAMESPACE_DECFUNCS ) && ! defined( __PRG__ ) \n" );

               pMember = pNamespace->pNext;

               while( pMember )
               {
                  if( pMember->type & NSTYPE_MEMBER )
                  {
                     if( ( pMember->pFunc->cScope & HB_FS_STATIC ) == 0 )
                        fprintf( yyc, "   HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ %d, %s );\n", pNamespace->szFullPath, pNamespace->iID, pMember->szName );
                  }

                  if( pMember->type & NSTYPE_SPACE )
                  {
                     pNamespace = pMember;
                     ++iLevel;
                  }

                  if( pMember->type & NSTYPE_END )
                  {
                     if( --iLevel == 0 )
                        break;
                  }

                  pMember = pMember->pNext;
               }

               fprintf( yyc, "\n#endif\n\n" );  /* Dont remove prefix \n! */
               fclose( yyc );

               if( pMember )
                  pNamespace = pMember;
               else
                  break;
            }
            else if( ( pNamespace->type & ( NSTYPE_EXTERNAL | NSTYPE_MEMBER ) ) == ( NSTYPE_EXTERNAL | NSTYPE_MEMBER ) )
            {
               if( ( pNamespace->pOuter->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
                  fprintf( yyc, "HB_FUNC_OPTIONAL_NAMESPACE( /* %s */ %d, %s );\n", pNamespace->pOuter->szFullPath, pNamespace->pOuter->iID, pNamespace->szName );
               else
                  fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ %d, %s );\n", pNamespace->pOuter->szFullPath, pNamespace->pOuter->iID, pNamespace->szName );
            }

            pNamespace = pNamespace->pNext;
         }
      }

      if( hb_comp_Namespaces.pFirst )
      {
         PNAMESPACE pNamespace = hb_comp_Namespaces.pFirst;

         do
         {
            if( ( pNamespace->type & ( NSTYPE_EXTERNAL | NSTYPE_SPACE ) ) == ( NSTYPE_EXTERNAL | NSTYPE_SPACE ) )
            {
               fprintf( yyc, "#define NAMESPACE_DECFUNCS\n" );
               fprintf( yyc, "#include \"%s.hxns\"\n", pNamespace->szName );
               fprintf( yyc, "#undef NAMESPACE_DECFUNCS\n" );
            }

            pNamespace = pNamespace->pNext;
         }
         while( pNamespace );
      }

      /* Function prototype for Global Initialization */
      hb_compWriteGlobalFunc( yyc, &iLocalGlobals, &iGlobals, TRUE );

      /* write functions prototypes for inline blocks */
      while( pInline )
      {
         if( pInline->szName )
         {
            if( ! hb_compCStaticSymbolFound( pInline->szName, HB_PROTO_FUNC_STATIC ) )
               fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pInline->szName );
         }
         pInline = pInline->pNext;
      }

      /* check c-in-line static functions */
      pInline = hb_comp_inlines.pFirst;

      if( pInline && pInline->pCode )
         hb_compGenCInLineSymbol();

      fprintf( yyc, "\n" );

      if( hb_comp_UsedNamespaces.pFirst )
      {
         PNAMESPACE pUsedNamespace = hb_comp_UsedNamespaces.pFirst;

         do
         {
            if( pUsedNamespace->szName[ 0 ] != '*' && ( pUsedNamespace->type & NSTYPE_SPACE ) && pUsedNamespace->pOuter == NULL )
            {
               fprintf( yyc, "\n#include \"%s.xns\"\n", pUsedNamespace->szName );
               fprintf( yyc, "extern HB_NS_%s %s;\n", pUsedNamespace->szName, pUsedNamespace->szName );
            }

            pUsedNamespace = pUsedNamespace->pNext;
         }
         while( pUsedNamespace );
      }

      /* write functions prototypes for called functions outside this PRG */
      pFunCall = hb_comp_funcalls.pFirst;

      if( pFunCall )
         fprintf( yyc, "/* Forward declarations of all externally defined Functions. */\n" );

      while( pFunCall )
      {
         PFUNCTION pFunc = NULL;

         if( pFunCall->Namespace )
         {
            if( ( pFunCall->iFlags & NSF_RESOLVE ) == NSF_RESOLVE )
            {
               pFunc = hb_compFunctionResolve( pFunCall->szName, ( PNAMESPACE ) pFunCall->Namespace, NULL );
#if 0
               fprintf( yyc, "/* %s from %s resolved to %s */\n", pFunCall->szName, ( (PNAMESPACE) pFunCall->Namespace )->szName , pFunc && pFunc->pNamespace ? pFunc->pNamespace->szFullPath  : "global" );
#endif
            }
         }
#if defined( __XCC__ )
         if( pFunc == ( PFUNCTION ) 1 )
#else
         if( pFunc == ( PFUNCTION ) (HB_LONG) 1 )
#endif
            fprintf( yyc, "/* Skipped: call to '%s' resolved to external */\n", pFunCall->szName );
         else if( pFunCall->Namespace && ( ( ( pFunCall->iFlags & NSF_RESOLVE ) != NSF_RESOLVE ) || ( pFunc && pFunc->pNamespace ) ) )
         {
            /* No prototype for Namespace function calls! */
            if( pFunc && pFunc->pNamespace )
               fprintf( yyc, "/* Skipped: call to '%s' resolved to: '%s' */\n", pFunCall->szName, pFunc->pNamespace->szFullPath );
            else
               fprintf( yyc, "/* Skipped: call to: '%s' of: '%s' */\n", pFunCall->szName, ( char * ) pFunCall->Namespace );
         }
         else if( hb_compFunctionFind( pFunCall->szName, NULL, NSF_NONE ) == NULL && hb_compInlineFind( pFunCall->szName ) == NULL )
         {
            PCOMSYMBOL pSym = hb_compSymbolFind( pFunCall->szName, NULL, NULL, SYMF_FUNCALL );

            /* Skip! */
            if( pSym && ( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED ) )
            {
               if( pSym->Namespace )
               {
                  if( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR )
                     fprintf( yyc, "/* Skipped DEFERRED call to: '%s' of: '%s' */\n", pSym->szName, ( ( PNAMESPACE ) pSym->Namespace )->szFullPath );
                  else if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
                     fprintf( yyc, "/* Skipped DEFERRED call to: '%s' of: '%s' */\n", pSym->szName, ( char * ) pSym->Namespace );
                  else
                     assert( 0 );
               }
               else
                  fprintf( yyc, "/* Skipped DEFERRED call to: '%s' */\n", pSym->szName );
            }
            else if( hb_compCStaticSymbolFound( pFunCall->szName, HB_PROTO_FUNC_PUBLIC ) )
            {
               /* Will be processed by hb_compGenCAddProtos()
                * fprintf( yyc, "HB_FUNC_PUBLIC( %s );\n", pFunCall->szName );
                */
            }
            else if( hb_compCStaticSymbolFound( pFunCall->szName, HB_PROTO_FUNC_STATIC ) )
            {
               /* Will be processed by hb_compGenCAddProtos()
                * fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pFunCall->szName );
                */
            }
            else
            {
               if( hb_comp_autoDeferred )
               {
                  pSym->cScope |= HB_FS_DEFERRED;

                  if( fCodeExt )
                  {
                     if( ! bBeginExt )
                     {
                        fprintf( fCodeExt,
                           "/*\n"
                           "   Dynamic functions required by module: \"%s\"\n"
                           "   This file should be included on top of file when not\n"
                           "   compiled with -vd option\n"
                           "*/\n\n", szSourceName );
                        bBeginExt = TRUE;
                     }

                     fprintf( fCodeExt, "DYNAMIC %s\n", pFunCall->szName );
                  }
               }
               else
                  fprintf( yyc, "HB_FUNC_EXTERN( %s );\n", pFunCall->szName );
            }
         }

         pFunCall = pFunCall->pNext;
      }

      /* Write C DUMP fucntion prototypes */
      hb_compGenCAddProtos( yyc );

      if( bCritical )
      {
         fprintf( yyc, "\n#define HB_THREAD_SUPPORT\n" );
         fprintf( yyc, "#include \"thread.h\"\n\n" );

         pFunc = hb_comp_functions.pFirst;
         while( pFunc )
         {
            if( pFunc->cScope & HB_FS_CRITICAL )
               fprintf( yyc, "static HB_CRITICAL_T s_Critical%s;\n", pFunc->szName );

            pFunc = pFunc->pNext;
         }

         /* Write init function for CRITICAL Functions Mutex initialization. */
         fprintf( yyc, "\nHB_CALL_ON_STARTUP_BEGIN( hb_InitCritical%s )\n", szModuleName );

         pFunc = hb_comp_functions.pFirst;
         while( pFunc )
         {
            if( pFunc->cScope & HB_FS_CRITICAL )
               fprintf( yyc, "   HB_CRITICAL_INIT( s_Critical%s );\n", pFunc->szName );

            pFunc = pFunc->pNext;
         }

         fprintf( yyc, "HB_CALL_ON_STARTUP_END( hb_InitCritical%s )\n", szModuleName );
      }

#ifdef BROKEN_MODULE_SPACE_LOGIC
      fprintf( yyc, "HB_FUNC_STATIC( __end__ );\n" );
#endif

      if( hb_comp_Namespaces.pFirst )
      {
         void * pTemp = ( void * ) yyc;

         hb_compNamespaceEnumSpaces( hb_comp_Namespaces.pFirst, hb_compGenerateXNS, &pTemp );

         yyc = ( FILE * ) pTemp;
      }

      fprintf( yyc,
         "\n#undef HB_PRG_PCODE_VER\n"
         "#define HB_PRG_PCODE_VER %i\n", ( int ) HB_PCODE_VER );

      hb_compWriteDeclareGlobal( yyc, iLocalGlobals );

      if( hb_comp_UsedNamespaces.pFirst )
      {
         PNAMESPACE pNamespace = hb_comp_UsedNamespaces.pFirst;

         do
         {
            if( pNamespace->type & NSTYPE_SPACE )
            {
               if( ( pNamespace->type & NSTYPE_USED ) == NSTYPE_USED )
               {
                  bIsUsedNamespaces = TRUE;
                  break;
               }
            }

            pNamespace = pNamespace->pNext;
         }
         while( pNamespace );
      }

      if( bIsUsedNamespaces )
      {
         PNAMESPACE pNamespace = hb_comp_UsedNamespaces.pFirst;

         fprintf( yyc, "\nstatic char *pNamespaces = \"" );

         do
         {
            if( ( pNamespace->type & NSTYPE_SPACE ) )
            {
               if( ( pNamespace->type & NSTYPE_USED ) == NSTYPE_USED )
                  fprintf( yyc, "%s\\0", pNamespace->szFullPath );
            }

            pNamespace = pNamespace->pNext;
         }
         while( pNamespace );

         fprintf( yyc, "\";\n"
                  "#undef HB_MODULE_NAMESPACES\n"
                  "#define HB_MODULE_NAMESPACES pNamespaces\n" );
      }

      /* writes the symbol table */
      /* Generate the wrapper that will initialize local symbol table
       */
      fprintf( yyc, "\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_%s%s )\n", hb_comp_szPrefix, szModuleName );

      iSymOffset     = 0;
      iStartupOffset = -1;

      while( pSym )
      {
         if( pSym->szName[ 0 ] == '(' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
             * we are using these two bits to mark the special function used to
             * initialize static variables
             */
            fprintf( yyc, "{ \"%s\", {HB_FS_INITEXIT}, {hb_INITSTATICS}, &ModuleFakeDyn }", pSym->szName ); /* NOTE: hb_ intentionally in lower case */
         }
         else if( pSym->szName[ 0 ] == '[' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
             * we are using these two bits to mark the special function used to
             * initialize global variables
             */
            fprintf( yyc, "{ \"(_INITGLOBALS)\", {HB_FS_INITEXIT}, {hb_INITGLOBALS}, &ModuleFakeDyn }" ); /* NOTE: hb_ intentionally in lower case */
         }
         else if( pSym->szName[ 0 ] == '<' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
             * we are using these two bits to mark the special function used to
             * initialize debugging info about valid stop lines
             */
            fprintf( yyc, "{ \"(_INITLINES)\", {HB_FS_INITEXIT}, {hb_INITLINES}, &ModuleFakeDyn }" ); /* NOTE: hb_ intentionally in lower case */
         }
         else if( pSym->szName[ 0 ] == '{' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
             * we are using these two bits to mark the special function used to
             * initialize global variables
             */
            fprintf( yyc, "{ \"hb_REGISTERGLOBALS\", {HB_FS_INITEXIT}, {hb_REGISTERGLOBALS}, &ModuleFakeDyn }" ); /* NOTE: hb_ intentionally in lower case */
         }
         else
         {
            if( ( pSym->iFlags & SYMF_FUNCALL ) == SYMF_FUNCALL )
            {
               if( ( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH ) || ( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR ) )
               {
                  if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
                     pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPATH );
                  else
                     pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPTR );

                  if( pFunc )
                  {
                     if( ( pSym->cScope & HB_FS_LOCAL ) )
                     {
                        assert( ( ( pFunc->pNamespace->type & NSTYPE_RUNTIME ) == NSTYPE_RUNTIME ) ||
                                ( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL ) );
                        assert( ( pFunc->cScope & HB_FS_PUBLIC ) );
                     }
                     else
                     {
                        assert( ( ( pFunc->pNamespace->type & NSTYPE_RUNTIME ) != NSTYPE_RUNTIME ) ||
                                ( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) != NSTYPE_OPTIONAL ) );

                        assert( ( pSym->cScope & ( HB_FS_PUBLIC | HB_FS_STATIC ) ) == 0 );
                        pSym->cScope |= ( HB_FS_LOCAL | HB_FS_STATIC );
                     }
                  }
                  else
                  {
                     if( ! ( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED ) )
                        pSym->cScope |= ( HB_FS_INDIRECT | HB_FS_PUBLIC );
                  }
               }
               else if( ( pSym->iFlags & SYMF_NS_RESOLVE ) == SYMF_NS_RESOLVE )
               {
                  assert( ( pSym->cScope & HB_FS_PUBLIC ) == 0 );

                  pFunc = hb_compFunctionResolve( pSym->szName, ( PNAMESPACE ) pSym->Namespace, pSym );

#if defined( __XCC__ )
                  if( pFunc == ( PFUNCTION ) 1 )
#else
                  if( pFunc == ( PFUNCTION ) ( HB_LONG ) 1 )
#endif
                     /* Resolved to external member. */
                     pFunc = NULL;
               }
               else if( ( pSym->cScope & HB_FS_LOCAL ) != HB_FS_LOCAL )
               {
                  /* It's a function defined in this module */
                  if( ( pFunc = hb_compFunctionFind( pSym->szName, NULL, SYMF_FUNCALL ) ) != NULL )
                     assert( 0 );
                  else if( hb_compCStaticSymbolFound( pSym->szName, HB_PROTO_FUNC_PUBLIC ) )
                  {
                     if( ( pSym->cScope & HB_FS_STATIC ) == 0 )
                        pSym->cScope |= ( HB_FS_LOCAL | HB_FS_PUBLIC );
                  }
                  else if( hb_compCStaticSymbolFound( pSym->szName, HB_PROTO_FUNC_STATIC ) )
                  {
                     if( ( pSym->cScope & HB_FS_PUBLIC ) == 0 )
                        pSym->cScope |= ( HB_FS_LOCAL | HB_FS_STATIC );
                  }
                  else
                  {
                     if( ( pSym->cScope & HB_FS_STATIC ) == 0 )
                        pSym->cScope |= HB_FS_PUBLIC;
                  }
               }

               if( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
               {
                  assert( ( pSym->cScope & HB_FS_LOCAL ) == 0 );
                  assert( ( pSym->cScope & HB_FS_PUBLIC ) == HB_FS_PUBLIC );
               }
               else
               {
                  if( ( pSym->cScope & HB_FS_PUBLIC ) == HB_FS_PUBLIC )
                     assert( ( pSym->cScope & HB_FS_STATIC ) == 0 );
                  else
                  {
                     assert( ( pSym->cScope & HB_FS_STATIC ) == HB_FS_STATIC );
                     assert( ( pSym->cScope & HB_FS_LOCAL ) == HB_FS_LOCAL );
                  }
               }
            }

            if( pSym->Namespace )
            {
               if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
               {
                  if( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
                     fprintf( yyc, "{ \"%s.%s\", {", ( char * ) pSym->Namespace, pSym->szName );
                  else
                     fprintf( yyc, "{ \"%s\", {", pSym->szName );
               }
               else if( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR )
               {
                  /* if( ( ( (PNAMESPACE) pSym->Namespace )->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
                   */

                  if( ( ( ( PNAMESPACE ) pSym->Namespace )->type & NSTYPE_RUNTIME ) == NSTYPE_RUNTIME )
                     fprintf( yyc, "{ \"%s.%s\", {", ( ( PNAMESPACE ) pSym->Namespace )->szFullPath, pSym->szName );
                  else if( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
                     fprintf( yyc, "{ \"%s.%s\", {", ( ( PNAMESPACE ) pSym->Namespace )->szFullPath, pSym->szName );
                  else
                     fprintf( yyc, "{ \"%s\", {", pSym->szName );
               }
               else
                  assert( 0 );
            }
            else
               fprintf( yyc, "{ \"%s\", {", pSym->szName );

            if( pSym->cScope & HB_FS_INIT & ~HB_FS_STATIC )
               fprintf( yyc, "HB_FS_INIT" );
            else if( pSym->cScope & HB_FS_EXIT & ~HB_FS_STATIC )
               fprintf( yyc, "HB_FS_EXIT" );
            else if( pSym->cScope & HB_FS_STATIC )
               fprintf( yyc, "HB_FS_STATIC" );
            else if( ( pSym->cScope & HB_FS_INDIRECT ) == HB_FS_INDIRECT )
               fprintf( yyc, "HB_FS_INDIRECT" );
            else
            {
               fprintf( yyc, "HB_FS_PUBLIC" );
               if( ( pSym->cScope & HB_FS_FIRST ) != HB_FS_FIRST && ( pSym->cScope & HB_FS_LOCAL ) == HB_FS_LOCAL && hb_comp_createExternList )
                  fprintf( fExtern, "EXTERNAL %s\n", pSym->szName );
            }

            if( ( pSym->cScope & HB_FS_LOCAL ) == HB_FS_LOCAL )
            {
               fprintf( yyc, " | HB_FS_LOCAL" );

               if( ( ! ( pSym->cScope & HB_FS_INIT & ~HB_FS_STATIC ) &&
                     ! ( pSym->cScope & HB_FS_EXIT & ~HB_FS_STATIC ) &&
                     ( pSym->cScope & HB_FS_STATIC ) == HB_FS_STATIC ) &&
                   ! hb_compFunCallFind( pSym->szName, pSym->Namespace, pSym->iFlags ) )
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_STATIC_UNUSED, pSym->szName, NULL );
            }
            else if( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED ) // MUTUALLY EXCLUSIVE
               fprintf( yyc, " | HB_FS_DEFERRED" );

            if( ( pSym->cScope & HB_FS_FIRST ) && ( ! hb_comp_bNoStartUp ) )
            {
               fprintf( yyc, " | HB_FS_FIRST" );
               iStartupOffset = iSymOffset;
               bSymFIRST      = TRUE;
            }

            /* specify the function address if it is a defined function or an
               external called function */

            if( ( pSym->cScope & HB_FS_INDIRECT ) == HB_FS_INDIRECT )
            {
               if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
                  fprintf( yyc, "}, {(PHB_FUNC) &%s.%s}, &ModuleFakeDyn }", ( char * ) pSym->Namespace, pSym->szName );
               else
                  fprintf( yyc, "}, {(PHB_FUNC) &%s.%s}, &ModuleFakeDyn }", ( ( PNAMESPACE ) pSym->Namespace )->szFullPath, pSym->szName );
            }
            else if( ( pSym->iFlags & SYMF_FUNCALL ) && ( ( pSym->cScope & HB_FS_LOCAL ) == HB_FS_LOCAL ) ) /* is it a function defined in this module */
            {
               if( ( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH ) || ( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR ) )
               {
                  assert( pFunc );

                  /* pFunc is NOT a typo - resolved above! */
                  if( ( pFunc->pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
                     /* pFunc is NOT a typo - resolved above! */
                     fprintf( yyc, "}, {HB_EXTERNAL_NAMESPACE_FUNCNAME( %d, %s )}, &ModuleFakeDyn }", pFunc->pNamespace->iID, pSym->szName );
                  else if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
                     /* pFunc is NOT a typo - resolved above! */
                     fprintf( yyc, "}, {HB_OPTIONAL_NAMESPACE_FUNCNAME( %d, %s )}, &ModuleFakeDyn }", pFunc->pNamespace->iID, pSym->szName );
                  else
                     /* pFunc is NOT a typo - resolved above! */
                     fprintf( yyc, "}, {HB_NAMESPACE_FUNCNAME( %d, %s )}, &ModuleFakeDyn }", pFunc->pNamespace->iID, pSym->szName );
               }
               else if( pSym->cScope & HB_FS_INIT & ~HB_FS_STATIC )
                  fprintf( yyc, "}, {HB_INIT_FUNCNAME( %.*s )}, &ModuleFakeDyn }", ( int ) strlen( pSym->szName ) - 1, pSym->szName );
               else if( pSym->cScope & HB_FS_EXIT & ~HB_FS_STATIC )
                  fprintf( yyc, "}, {HB_EXIT_FUNCNAME( %.*s )}, &ModuleFakeDyn }", ( int ) strlen( pSym->szName ) - 1, pSym->szName );
               else
                  fprintf( yyc, "}, {HB_FUNCNAME( %s )}, &ModuleFakeDyn }", pSym->szName );
            }
            else if( pSym->iFlags & SYMF_FUNCALL ) /* && hb_compFunCallFind( pSym->szName, pSym->Namespace, pSym->iFlags ) ) // is it a function called from this module */
            {
               if( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
                  fprintf( yyc, "}, {NULL}, NULL }" );
               else
                  fprintf( yyc, "}, {HB_FUNCNAME( %s )}, NULL }", pSym->szName );
            }
            else
               fprintf( yyc, "}, {NULL}, NULL }" );   /* memvar */
         }

         if( pSym != hb_comp_symbols.pLast )
            fprintf( yyc, ",\n" );

         pSym = pSym->pNext;
         iSymOffset++;
      }

      /*
         Write entries for external functions
       */
      if( pStatSymb )
         hb_compWriteExternEntries( yyc, bSymFIRST, TRUE, TRUE, szModuleName );

      /*
         End of initializaton codes
       */
      hb_writeEndInit( yyc, szModuleName);

#ifdef BROKEN_MODULE_SPACE_LOGIC
      fprintf( yyc, "\nHB_FUNC_STATIC( __begin__ ){}\n\n" );
#endif

      if( hb_comp_bExplicitStartProc && iStartupOffset >= 0 )
      {
         fprintf( yyc, "extern HB_EXPORT void hb_vmExplicitStartup( PHB_SYMB pSymbol );\n" );
         fprintf( yyc, "void hb_InitExplicitStartup( void )\n" );
         fprintf( yyc, "{\n" );
         fprintf( yyc, "   hb_vmExplicitStartup( symbols_table + %i );\n", iStartupOffset );
         fprintf( yyc, "}\n" );

         hb_compWritePragma( yyc, "", "" );
      }

      if( bCritical )
         hb_compWritePragma( yyc, "", szModuleName );

      /* Generate functions data
       */
      pFunc = hb_comp_functions.pFirst;

      if( ! hb_comp_bStartProc )
         pFunc = pFunc->pNext; /* No implicit starting procedure */

      while( pFunc )
      {
         bIsInitFunction   = ( pFunc->cScope & HB_FS_INIT & ~HB_FS_STATIC );
         bIsExitFunction   = ( pFunc->cScope & HB_FS_EXIT & ~HB_FS_STATIC );
         bIsStaticFunction = bIsInitFunction == FALSE && bIsExitFunction == FALSE && ( pFunc->cScope & HB_FS_STATIC );
         bIsStaticVariable = ( pFunc == hb_comp_pInitFunc );
         bIsGlobalVariable = ( pFunc == hb_comp_pGlobalsFunc );
         bIsLineNumberInfo = ( pFunc == hb_comp_pLineNumberFunc );

         if( pFunc->pNamespace && ( HB_ISALPHA( pFunc->szName[ 0 ] ) || pFunc->szName[ 0 ] == '_' ) )
         {
            if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
            {
               if( bIsStaticFunction )
                  fprintf( yyc, "HB_FUNC_STATIC( %s )", pFunc->szName );
               else
                  fprintf( yyc, "HB_FUNC_OPTIONAL_NAMESPACE( /* %s */ %d, %s )", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
            }
            else if( ( pFunc->pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
               fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ %d, %s )", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
            else if( ( pFunc->pNamespace->type & NSTYPE_STEALTH ) == NSTYPE_STEALTH )
               fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ NSID_%s, %s )", pFunc->pNamespace->szFullPath, pFunc->pNamespace->szName, pFunc->szName );
            else
               fprintf( yyc, "HB_FUNC_NAMESPACE( /* %s */ %d, %s )", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
         }
         /* Is it a PUBLIC FUNCTION/PROCEDURE */
         else if( bIsStaticFunction )
            fprintf( yyc, "HB_FUNC_STATIC( %s )", pFunc->szName );
         /* Is it STATICS$ */
         else if( bIsStaticVariable )
            fprintf( yyc, "HB_FUNC_INITSTATICS()" ); /* NOTE: hb_ intentionally in lower case */
         /* Is it GLOBALS$ */
         else if( bIsGlobalVariable )
            fprintf( yyc, "HB_FUNC_INITGLOBALS()" ); /* NOTE: hb_ intentionally in lower case */
         /* Is it (_INITLINES) */
         else if( bIsLineNumberInfo )
            fprintf( yyc, "HB_FUNC_INITLINES()" ); /* NOTE: hb_ intentionally in lower case */
         /* Is it an INIT FUNCTION/PROCEDURE */
         else if( bIsInitFunction )
            fprintf( yyc, "HB_FUNC_INIT( %.*s )", ( int ) strlen( pFunc->szName ) - 1, pFunc->szName );
         /* Is it an EXIT FUNCTION/PROCEDURE */
         else if( bIsExitFunction )
            fprintf( yyc, "HB_FUNC_EXIT( %.*s )", ( int ) strlen( pFunc->szName ) - 1, pFunc->szName );
         /* Then it must be a STATIC FUNCTION/PROCEDURE */
         else
            fprintf( yyc, "HB_FUNC( %s )", pFunc->szName );

         fprintf( yyc, "\n" );
         if( hb_comp_iGenCOutput == HB_COMPGENC_REALCODE )
            hb_compGenCRealCode( pFunc, yyc );
         else
         {
            if( hb_comp_iGenCOutput == HB_COMPGENC_COMPACT )
               hb_compGenCCompact( pFunc, yyc );
            else
               hb_compGenCReadable( pFunc, yyc );
         }
         fprintf( yyc, "\n" );

         pFunc = pFunc->pNext;
      }

      if( iLocalGlobals )
      {
         fprintf( yyc,
            "HB_FUNC_REGISTERGLOBAL()\n"
            "{\n"
            "   hb_vmRegisterGlobals( &ppGlobals, %i );\n"
            "}\n\n", iGlobals  );
      }

      /* Generate codeblocks data
       */
      if( hb_comp_cInlineID )
      {
         fprintf( yyc,
            "#include \"hbapi.h\"\n"
            "#include \"hbstack.h\"\n"
            "#include \"hbapierr.h\"\n"
            "#include \"hbapiitm.h\"\n"
            "#include \"hbvm.h\"\n"
            "#include \"hboo.ch\"\n" );
      }

      pInline = hb_comp_inlines.pFirst;

      if( pInline && pInline->pCode )
         hb_compGenCInLine( yyc );

#ifdef BROKEN_MODULE_SPACE_LOGIC
      fprintf( yyc, "\nHB_FUNC_STATIC( __end__ ){}\n" );
#endif
   }
   else
   {
      /*
       * We do not have an ordinary PRG code in file
       */

      BOOL bInline = ( pInline && pInline->pCode );

      if( bInline )
         hb_compGenCInLineSymbol();

      /*
       * We have functions in dump areas
       */

      if( pStatSymb )
      {
         fprintf( yyc, "#include \"hbvmpub.h\"\n" );

         if( hb_comp_iGenCOutput != HB_COMPGENC_COMPACT )
            fprintf( yyc, "#include \"hbpcode.h\"\n" );

         fprintf( yyc, "#include \"hbinit.h\"\n\n" );

         hb_compGenCAddProtos( yyc );

         if( hb_compWriteExternEntries( yyc, bSymFIRST, FALSE, FALSE, szModuleName ) )
            hb_writeEndInit( yyc, szModuleName );
      }

      if( bInline )
         hb_compGenCInLine( yyc );
      else
      {
         if( ! pStatSymb )
            fprintf( yyc, "/* Empty source file */\n\n" );
      }
   }

   fclose( yyc );

   if( hb_comp_createExternList )
      fclose( fExtern );

   if( fCodeExt )
      fclose( fCodeExt );

#ifdef HB_BACK_END
   if( HB_BACK_END == 0 )
#endif
   {
      hb_compOutStd( "Done.\n" );
   }

   pTemp = pStatSymb;

   while( pTemp )
   {
#if defined( HB_COMP_DEBUG )
      printf( "RELEASING : >>%s<<\n", pTemp->szName );
#endif
      hb_xfree( pTemp->szName );
      pTemp       = pTemp->pNext;
      hb_xfree( ( void * ) pStatSymb );
      pStatSymb   = pTemp;
   }
}

/*
   Write symbol entries to intialized on startup
 */
static BOOL hb_compWriteExternEntries( FILE * yyc, BOOL bSymFIRST, BOOL bNewLine, BOOL bPrg, const char * szModuleName )
{
   BOOL        bStartFunc  = FALSE;
   BOOL        bBegin      = FALSE;
   PSSYMLIST   pTemp       = pStatSymb;
   USHORT      ulLen       = pStatSymb->uEntry;
   char *      szEntries   = ( char * ) hb_xgrab( ( ulLen + 1 ) * 256  );
   HB_SIZE     ul;

   hb_xmemset( szEntries, '\0', ( ulLen + 1 ) * 256 );

   while( pTemp )
   {
      if( /* ( ! hb_compFunctionFind( pTemp->szName, NULL, NSF_NONE ) ) && */
         ( ! hb_compSymbFound( pTemp->szName ) ) )
      {
         if( bNewLine && ! bBegin )
         {
            bBegin = TRUE;
            hb_xstrcat( szEntries, ",\n", 0 );
         }

         if( pTemp->Type == HB_PROTO_FUNC_STATIC )
         {
            hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_STATIC_UNUSED, pTemp->szName, NULL );
            pTemp = pTemp->pNext;
            continue;
         }
         else if( pTemp->Type == HB_PROTO_FUNC_EXIT )
            hb_xstrcat( szEntries, "{ \"", pTemp->szName, "$\", {HB_FS_EXIT | HB_FS_LOCAL}, {HB_EXIT_FUNCNAME( ", pTemp->szName, " )}, &ModuleFakeDyn },\n", NULL );
         else if( pTemp->Type == HB_PROTO_FUNC_INIT )
            hb_xstrcat( szEntries, "{ \"", pTemp->szName, "$\", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( ", pTemp->szName, " )}, &ModuleFakeDyn },\n", NULL );
         else
         {
            if( ( ! bNewLine ) && ( ! bBegin ) )
            {
               char szVer[ 5 ];

               bBegin = TRUE;

               hb_snprintf( szVer, sizeof( szVer ), "%i", HB_PCODE_VER );

               hb_xstrcat( szEntries,
                           "\n#define __PRG_SOURCE__ \"", hb_comp_PrgFileName, "\"\n\n",
                           "#undef HB_PRG_PCODE_VER\n",
                           "#define HB_PRG_PCODE_VER ", szVer, "\n",
                           "\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_", hb_comp_szPrefix, szModuleName, " )\n", NULL );
            }

            if( bPrg )
            {
               /* if( hb_compFunCallFind( pTemp->szName, NULL, NSF_NONE ) )
                */
               {
                  hb_xstrcat( szEntries, "{ \"", pTemp->szName, "\", {HB_FS_PUBLIC | HB_FS_LOCAL", NULL );

                  if( ! bSymFIRST && ! hb_comp_bNoStartUp && ! bStartFunc )
                  {
                     bStartFunc = TRUE;
                     hb_xstrcat( szEntries, " | HB_FS_FIRST", 0 );
                  }

                  hb_xstrcat( szEntries, "}, {HB_FUNCNAME( ", pTemp->szName, " )}, NULL },\n", NULL );
               }
            }
            else
            {
               if( ! bSymFIRST && ! hb_comp_bNoStartUp && ! bStartFunc )
               {
                  bStartFunc = TRUE;
                  hb_xstrcat( szEntries, "{ \"", pTemp->szName, "\", {HB_FS_PUBLIC | HB_FS_LOCAL",
                              " | HB_FS_FIRST", "}, {HB_FUNCNAME( ", pTemp->szName, " )}, NULL },\n", NULL );
               }
            }
         }
      }

      pTemp = pTemp->pNext;
   }

   ul = strlen( szEntries );

   if( ul && szEntries[ ul - 2 ] == ',' )
      szEntries[ ul - 2 ] = '\0';

   if( ul && ( bPrg || bStartFunc ) )
      fprintf( yyc, "%s", szEntries );

   hb_xfree( szEntries );

   return bStartFunc;
}

static void hb_compWriteDeclareGlobal( FILE * yyc, short iLocalGlobals )
{
   BOOL bWriteExtern = FALSE;

   fprintf( yyc, "\n#include \"hbapi.h\"\n" );

   if( hb_comp_pGlobals )
   {
      PVAR  pGlobal  = hb_comp_pGlobals;
      int   iGlobals = 0;

      while( pGlobal )
      {
         if( ! bWriteExtern )
         {
            bWriteExtern = TRUE;
            fprintf( yyc, "\nHB_EXTERN_BEGIN\n" );
         }

         iGlobals++;

         if( pGlobal->szAlias == NULL )
            fprintf( yyc, "HB_EXPORT HB_ITEM_NEW( %s );\n", pGlobal->szName );
         else
            fprintf( yyc, "extern HB_IMPORT HB_ITEM %s;\n", pGlobal->szName );

         pGlobal = pGlobal->pNext;
      }

      if( bWriteExtern )
         fprintf( yyc, "HB_EXTERN_END\n" );

      fprintf( yyc, "\nstatic PHB_ITEM pGlobals[%i];\n", iGlobals );

      if( iLocalGlobals || ( hb_comp_iGenCOutput == HB_COMPGENC_REALCODE ) )
         fprintf( yyc, "static PHB_ITEM *ppGlobals = pGlobals;\n" );

      pGlobal = hb_comp_pGlobals;

      fprintf( yyc, "\nstatic void module_InitGlobalsArray( void )\n{\n" );

      iGlobals = 0;
      while( pGlobal )
      {
         fprintf( yyc, "   pGlobals[%i] = &%s;\n", iGlobals, pGlobal->szName );
         pGlobal = pGlobal->pNext;
         iGlobals++;
      }

      fprintf( yyc,
         "}\n\n"
         "#undef HB_MODULE_GLOBALS\n"
         "#define HB_MODULE_GLOBALS ( module_InitGlobalsArray(), (PHB_ITEM *) pGlobals )\n" );
   }
}

/*
   Write function prototype for Global Variables
 */
static void hb_compWriteGlobalFunc( FILE * yyc, short * iLocalGlobals, short * iGlobals, BOOL bWriteProto )
{
   /* Any NON EXTERN Globals to Register? */
   PVAR  pGlobal  = hb_comp_pGlobals;
   short uGlobals = 0, uLocalGlobals = 0;

   while( pGlobal )
   {
      uGlobals++;

      if( pGlobal->szAlias == NULL )
         uLocalGlobals++;

      pGlobal = pGlobal->pNext;
   }

   if( uLocalGlobals && bWriteProto )
      fprintf( yyc, "HB_FUNC_REGISTERGLOBAL();\n" ); /* NOTE: hb_ intentionally in lower case */

   *iGlobals      = uGlobals;
   *iLocalGlobals = uLocalGlobals;
}

/*
   Functions prototypes from dump areas
 */
static void hb_compGenCAddProtos( FILE * yyc )
{
   PSSYMLIST pTemp;

   if( pStatSymb == NULL )
      return;

   pTemp = pStatSymb;

   fprintf( yyc, "\n/* Forward declarations of all DUMP defined Functions. */\n" );

   while( pTemp )
   {
      if( ! hb_compFunctionFind( pTemp->szName, NULL, NSF_NONE ) )
      {
         if( pTemp->Type == HB_PROTO_FUNC_EXIT )
            fprintf( yyc, "HB_FUNC_EXIT( %s );\n", pTemp->szName );
         else if( pTemp->Type == HB_PROTO_FUNC_INIT )
            fprintf( yyc, "HB_FUNC_INIT( %s );\n", pTemp->szName );
         else if( pTemp->Type == HB_PROTO_FUNC_STATIC )
            fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pTemp->szName );
         else if( pTemp->Type == HB_PROTO_FUNC_PUBLIC )
            fprintf( yyc, "HB_FUNC( %s );\n", pTemp->szName );
      }
      pTemp = pTemp->pNext;
   }
}

static void hb_writeEndInit( FILE * yyc, const char * szModuleName )
{
/*	
   fprintf( yyc,
#ifdef BROKEN_MODULE_SPACE_LOGIC
            ",\n"
            "{ \"!\", {0}, {HB_FUNCNAME( __begin__ )}, NULL },\n"
            "{ \"!\", {0}, {HB_FUNCNAME( __end__ )}, NULL }"
#endif
            "\nHB_INIT_SYMBOLS_END( hb_vm_SymbolInit_%s%s )\n\n",
            hb_comp_szPrefix, szModuleName );
   hb_compWritePragma( yyc, hb_comp_szPrefix, szModuleName );
*/
   fprintf( yyc, "\nHB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_%s%s, ",
                 hb_comp_szPrefix, szModuleName );
   //hb_gencc_string_put( yyc, ( BYTE * ) szSourceFile, strlen( szSourceFile ) );
   fprintf( yyc, "__PRG_SOURCE__,  0x%04x )\n\n",  HB_PCODE_VER );

   fprintf( yyc, "#if defined( HB_PRAGMA_STARTUP )\n"
                 "   #pragma startup hb_vm_SymbolInit_%s%s\n"
                 "#elif defined( HB_DATASEG_STARTUP )\n"
                 "   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_%s%s )\n"
                 "   #include \"hbiniseg.h\"\n"
                 "#endif\n\n",
                 hb_comp_szPrefix, szModuleName,
                 hb_comp_szPrefix, szModuleName );
   
}

/*
   Searching for function names in glocal symbol list
 */
static BOOL hb_compSymbFound( const char * szSymbol )
{
   BOOL        bStatSymFound  = FALSE;
   PCOMSYMBOL  pSym_          = hb_comp_symbols.pFirst;

   while( pSym_ )
   {
      if( strcmp( pSym_->szName, szSymbol ) == 0 )
      {
         bStatSymFound = TRUE;
         break;
      }
      pSym_ = pSym_->pNext;
   }

   return bStatSymFound;
}

/*
   Searching for function names in in-line-c for writing prototypes
 */
static BOOL hb_compCStaticSymbolFound( const char * szSymbol, int iOption )
{
   BOOL        bFound   = FALSE;
   PSSYMLIST   pTemp    = pStatSymb;

   while( pTemp )
   {
      if( strcmp( pTemp->szName, szSymbol ) == 0 && pTemp->Type == iOption )
      {
         bFound = TRUE;
         break;
      }

      pTemp = pTemp->pNext;
   }

   return bFound;
}

/*
   Collecting function names from in-line-c.
   HB_FUNC([]), HB_FUNC_STATIC([]), HB_FUNC_INIT([]), HB_FUNC_EXIT([])
 */
static void hb_compCStatSymList( const char * statSymName, int iOption )
{
   PSSYMLIST pTemp, pLast;

   if( hb_compCStaticSymbolFound( statSymName, iOption ) )
      return;

   pTemp          = ( PSSYMLIST ) hb_xgrab( sizeof( SSYMLIST ) );
   pTemp->szName  = ( char * ) hb_xgrab( strlen( statSymName ) + 1 );
   hb_xstrcpy( pTemp->szName, statSymName, 0 );
   pTemp->Type    = iOption;
   pTemp->pNext   = NULL;

#if defined( HB_COMP_DEBUG )
   printf( "C Func: '%s' Type: %i\n", pTemp->szName, pTemp->Type );
#endif

   if( pStatSymb )
   {
      pLast = pStatSymb;

      while( pLast->pNext )
         pLast = pLast->pNext;

      pLast->pNext = pTemp;
      pStatSymb->uEntry++;
   }
   else
   {
      pStatSymb         = pTemp;
      pStatSymb->uEntry = 1;
   }
}

/*
   Parsing in-line-c codes to extract function names
   HB_FUNC([]), HB_FUNC_STATIC([]), HB_FUNC_INIT([]), HB_FUNC_EXIT([])
 */
static void hb_compGenCCheckInLineStatic( const char * sInline )
{
   char *   szTmp, * szContinue;
   BOOL     bStatic, bInit, bExit;

   while( ( sInline = strstr( sInline, "HB_FUNC" ) ) != NULL )
   {
#if defined( HB_COMP_DEBUG )
      printf( "Found: %s\n", sInline );
#endif
      sInline  += 7;

      bStatic  = FALSE;
      bInit    = FALSE;
      bExit    = FALSE;

      if( strncmp( sInline, "_STATIC", 7 ) == 0 )
      {
         sInline  += 7;
         bStatic  = TRUE;
      }
      else if( strncmp( sInline, "_INIT", 5 ) == 0 )
      {
         sInline  += 5;
         bInit    = TRUE;
      }
      else if( strncmp( sInline, "_EXIT", 5 ) == 0 )
      {
         sInline  += 5;
         bExit    = TRUE;
      }

      while( HB_ISSPACE( *sInline ) )
         sInline++;

      if( *sInline != '(' )
         continue;

      sInline++;

      while( HB_ISSPACE( *sInline ) )
         sInline++;

      szTmp = ( char * ) strchr( sInline, ')' );

      if( szTmp == NULL )
      {
#if defined( HB_COMP_DEBUG )
         printf( "Invalid Syntax: %s\n", sInline );
#endif
         continue;
      }

      szContinue = szTmp + 1;

      szTmp--;
      while( HB_ISSPACE( *szTmp ) )
         szTmp--;

      szTmp[ 1 ] = '\0';

      hb_compCStatSymList( sInline, bStatic ? HB_PROTO_FUNC_STATIC :
                           bInit   ? HB_PROTO_FUNC_INIT   :
                           bExit   ? HB_PROTO_FUNC_EXIT   :
                           HB_PROTO_FUNC_PUBLIC );

      if( szContinue[ -1 ] == '\0' )
         *szTmp = ')';
      else
         *szTmp = ' ';

      sInline = szContinue;
#if defined( HB_COMP_DEBUG )
      printf( "Continue at: %s\n", sInline );
#endif
   }
}

/*
   Grab the content of in-line-c codes to be parse for function names
 */
static void hb_compGenCInLineSymbol( void )
{
   PINLINE pInline = hb_comp_inlines.pFirst;

   while( pInline )
   {
      HB_SIZE  uiLen    = strlen( ( char * ) pInline->pCode ) + 1;
      char *   sInline  = ( char * ) hb_xgrab( uiLen );

      hb_snprintf( sInline, ( size_t ) uiLen, "%s", pInline->pCode );

      if( sInline )
      {
         char * szStripped = hb_stripOutComments( sInline, TRUE );
         hb_compGenCCheckInLineStatic( szStripped );
         hb_xfree( sInline );
         hb_xfree( szStripped );
         pInline = pInline->pNext;
      }
   }
}

/*
   "Copy & Paste" the contents of in-line-c to C file output
 */
static void hb_compGenCInLine( FILE * yyc )
{
   PINLINE  pInline = hb_comp_inlines.pFirst;
   char *   pszFileName;

   while( pInline )
   {
      fprintf( yyc, "#line %i \"", ( pInline->iLine + 1 ) );

      pszFileName = pInline->szFileName;

      while( *pszFileName )
      {
         if( *pszFileName == '\\' )
            fprintf( yyc, "\\" );

         fprintf( yyc, "%c", *pszFileName++ );
      }

      fprintf( yyc, "\"\n" );

      if( pInline->szName )
         fprintf( yyc, "HB_FUNC_STATIC( %s )\n", pInline->szName );

      fprintf( yyc, "%s", pInline->pCode );
      pInline = pInline->pNext;
   }
}

static void hb_compWritePragma( FILE * yyc, const char * szPrefix, const char * szModuleName )
{
   fprintf( yyc, "#if defined( HB_PRAGMA_STARTUP )\n"
            "   #pragma startup hb_vm_SymbolInit_%s%s\n"
            "#elif defined( HB_DATASEG_STARTUP )\n"
            "   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_%s%s )\n"
            "   #include \"hbiniseg.h\"\n"
            "#endif\n\n",
            szPrefix, szModuleName,
            szPrefix, szModuleName );
}
