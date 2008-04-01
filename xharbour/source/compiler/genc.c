/*
 * $Id: genc.c,v 1.167 2008/03/10 17:19:03 ronpinkas Exp $
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

static int hb_comp_iBaseLine;

extern void hb_compGenCRealCode( PFUNCTION pFunc, FILE * yyc );

static void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc );
static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc );
static void hb_compGenCInLine( FILE* );
static void hb_writeEndInit( FILE* yyc );
static void hb_compGenCAddProtos( FILE *yyc );

// AJ: 2004-02-05
// Routines to check C-In-Line static function declared in a PRG file
// with #PRAGMA BEGINDUMP - ENDDUMP
static void hb_compGenCInLineSymbol( void );
static void hb_compGenCCheckInLineStatic( char * str );
static BOOL hb_compCStaticSymbolFound( char* szSymbol, int iOption );
static BOOL hb_compSymbFound( char* szSymbol );
static void hb_compWriteGlobalFunc( FILE *yyc, short *iLocalGlobals, short * iGlobals, BOOL );
static void hb_compWriteDeclareGlobal( FILE *yyc );
static BOOL hb_compWriteExternEntries( FILE *yyc, BOOL bSymFIRST, BOOL, BOOL );
/* struct to hold symbol names of c-in-line static functions */
typedef struct _SSYMLIST
{
   char *    szName;
   int       Type;
   USHORT    uEntry;
   struct _SSYMLIST * pNext;
} SSYMLIST, * PSSYMLIST;

static PSSYMLIST pStatSymb = NULL;

/* helper structure to pass information */
typedef struct HB_stru_genc_info
{
   FILE * yyc;
   BOOL bVerbose;
   USHORT iNestedCodeblock;
} HB_GENC_INFO, * HB_GENC_INFO_PTR;

#define HB_GENC_FUNC( func ) HB_PCODE_FUNC( func, HB_GENC_INFO_PTR )
typedef HB_GENC_FUNC( HB_GENC_FUNC_ );
typedef HB_GENC_FUNC_ * HB_GENC_FUNC_PTR;

#define HB_PROTO_FUNC_STATIC  1
#define HB_PROTO_FUNC_PUBLIC  2
#define HB_PROTO_FUNC_INIT    3
#define HB_PROTO_FUNC_EXIT    4


PNAMESPACE hb_compGenerateXNS( PNAMESPACE pNamespace, void **pCargo )
{
    FILE *yyc = *( (FILE **) pCargo );
    FILE *yycOuter = yyc;
    PNAMESPACE pOuter = NULL;
    PNAMESPACE pMember;
    int iLevel;
    char *szFileName;

    if( ( pNamespace->type & NSTYPE_STEALTH ) == NSTYPE_STEALTH )
    {
       return pNamespace;
    }
    else if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
    {
       szFileName = hb_xstrcpy( NULL, pNamespace->szName, ".hxns", NULL );
       yyc = hb_fopen( szFileName, "ab" );
    }
    else
    {
       szFileName = hb_xstrcpy( NULL, pNamespace->szName, ".xns", NULL );
       yyc = hb_fopen( szFileName, "wb" );
    }

    if( ! yyc )
    {
       hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
    }

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

    iLevel = 1;
    pMember = pNamespace->pNext;

    while( pMember )
    {
       if( pMember->type & NSTYPE_SPACE )
       {
          if( ( pMember->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS && pMember->pOuter == NULL )
          {
             hb_compGenerateXNS( pMember, pCargo );
             pMember = hb_compNamespaceEnumSkipMembers( pMember->pNext );
             pMember = pMember->pNext;
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
             // Don't publish
          }
          else if( pMember->pFunc && ( pMember->pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) & ~HB_FS_STATIC ) )
          {
             // Don't publish
          }
          else
          {
             fprintf( yyc, "   #if defined( __PRG__ )\n" );
             fprintf( yyc, "   DEFINE NAMESPACE MEMBER %s\n", pMember->szName );
             fprintf( yyc, "   #else\n" );
             fprintf( yyc, "   PHB_FUNC %s;\n\n", pMember->szName ); // Don't remove DOUBLE \n!!!
             fprintf( yyc, "   #endif\n" );
          }
       }

       if( pMember->type & NSTYPE_END )
       {
          if( --iLevel == 0 )
          {
             break;
          }

          fprintf( yyc, "   #if defined( __PRG__ )\n" );
          fprintf( yyc, "   END\n" );
          fprintf( yyc, "   #else\n" );
          fprintf( yyc, "   } %s;\n\n", pOuter->szName ); // Don't remove DOUBLE \n!!!
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
       fprintf( yyc, "} %s;\n\n", pNamespace->szName ); // Don't remove DOUBLE \n!!!
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
       fprintf( yyc, "} HB_NS_%s;\n\n", pNamespace->szName ); // Don't remove DOUBLE \n!!!
       fprintf( yyc, "#endif\n" );
       fprintf( yyc, "#endif\n" );

       fclose( yyc );

       yyc = yycOuter;

       fprintf( yyc, "\n#include \"%s.xns\"\n", pNamespace->szName );

       fprintf( yyc, "\nHB_NS_%s %s =\n", pNamespace->szName, pNamespace->szName );
    }

    fprintf( yyc, "   {\n" );

    iLevel  = 1;
    pOuter  = pNamespace;
    pMember = pNamespace->pNext;

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
             // Don't publish
          }
          else if( pMember->pFunc && ( pMember->pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) & ~HB_FS_STATIC ) )
          {
             // Don't publish
          }
          else if( ( pMember->pOuter->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
          {
             fprintf( yyc, "     /* %s = */ HB_OPTIONAL_NAMESPACE_FUNCNAME( %d, %s ),\n", pMember->szName, pMember->iID, pMember->szName );
          }
          else if( ( pMember->type & NSTYPE_EXTERNAL ) == NSTYPE_EXTERNAL )
          {
             fprintf( yyc, "     /* %s = */ HB_EXTERNAL_NAMESPACE_FUNCNAME( %d, %s ),\n", pMember->szName, pNamespace->iID, pMember->szName );
          }
          else
          {
             if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
             {
                fprintf( yyc, "     /* %s = */ HB_NAMESPACE_FUNCNAME( %d, %s ),\n", pMember->szName, pMember->iID, pMember->szName );
             }
             else
             {
                fprintf( yyc, "     /* %s = */ HB_NAMESPACE_FUNCNAME( %d, %s ),\n", pMember->szName, pOuter->iID, pMember->szName );
             }
          }
       }

       if( pMember->type & NSTYPE_END )
       {
          if( --iLevel == 0 )
          {
             break;
          }

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
    {
       fprintf( yyc, "   };\n" );
    }

    return pNamespace;
}

void hb_compGenCCode( PHB_FNAME pFileName, char *szSourceExtension )      /* generates the C language output */
{
   char szExtName[ _POSIX_PATH_MAX ];
   char szFileName[ _POSIX_PATH_MAX ];
   char szSourceName[ _POSIX_PATH_MAX ], *pTmp;
   PFUNCTION pFunc;
   PFUNCALL pFunCall;
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   FILE * yyc; /* file handle for C output */
   FILE * fCodeExt = NULL; /* file handle for external function required by pCode.DLL */
   PINLINE pInline = hb_comp_inlines.pFirst;
   short iLocalGlobals = 0, iGlobals = 0;

   BOOL bIsStaticFunction ;
   BOOL bIsInitFunction   ;
   BOOL bIsExitFunction   ;
   BOOL bIsStaticVariable ;
   BOOL bIsGlobalVariable ;
   BOOL bIsLineNumberInfo;

   BOOL bCritical = FALSE;

   int  iSymOffset, iStartupOffset;

   PSSYMLIST pTemp;
   BOOL bSymFIRST = FALSE;

   BOOL bBeginExt = FALSE;

   BOOL bIsUsedNamespaces = FALSE;

   if( ! pFileName->szExtension )
   {
      pFileName->szExtension = ".c";
   }

   hb_fsFNameMerge( szFileName, pFileName );

   pFileName->szExtension = szSourceExtension;
   pFileName->szPath = NULL;
   hb_fsFNameMerge( szSourceName, pFileName );


   while( ( pTmp = strchr( szSourceName, '\\' ) ) != NULL )
   {
      *pTmp = '/';
   }

   yyc = hb_fopen( szFileName, "wb" );

   if( ! yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   /*
    Create *.ext when /vd is used
   */
   if ( hb_comp_autoDeferred )
   {
      pFileName->szExtension = ".ext";
      hb_fsFNameMerge( szExtName, pFileName );
      fCodeExt = hb_fopen( szExtName, "wb" );
   }

   /*
    Create *.p when /gc4 is used
   */
   if( hb_comp_iGenVarList )
   {
      hb_compPCodeStat( pFileName );
   }

   if( ! hb_comp_bQuiet )
   {
      printf( "Generating C source output to \'%s\'...\n", szFileName );
      fflush( stdout );
   }

   hb_strupr( hb_comp_FileAsSymbol );

   {
       int iTmp = strlen( hb_comp_FileAsSymbol );
       int iCh;

       for ( iCh = 0; iCh < iTmp; iCh ++ )
       {
          if ( ( hb_comp_FileAsSymbol[ iCh ] == '!' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '~' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '$' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '%' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '\'') ||
               ( hb_comp_FileAsSymbol[ iCh ] == ')' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '(' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == ' ' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '-' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '@' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '{' ) ||
               ( hb_comp_FileAsSymbol[ iCh ] == '}' ) )
          {
               hb_comp_FileAsSymbol[ iCh ] = '_';
          }
       }
   }

   {
      char *szComp = hb_verCompiler();
      char *szHrb  = hb_verHarbour();
      time_t t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );

      fprintf( yyc, "/*\n * %s\n", szHrb );
      /* AJ: Some compilers performs [f]printf("<%s>",string) incorrecltly */
      fprintf( yyc, " * Generated C source code from %s%s%s\n", "<", hb_comp_PrgFileName, ">" );
      if( hb_Command_Line && *hb_Command_Line )
      {
         fprintf( yyc, " * Command: %s\n", hb_Command_Line );
      }
      fprintf( yyc, " * Created: %04d.%02d.%02d %02d:%02d:%02d (%s)\n */\n\n", oTime->tm_year + 1900, oTime->tm_mon + 1, oTime->tm_mday, oTime->tm_hour, oTime->tm_min, oTime->tm_sec, szComp );

      hb_xfree( szComp );
      hb_xfree( szHrb );
   }

   if( hb_comp_iFunctionCnt || hb_comp_Namespaces.pFirst )
   {
      fprintf( yyc, "#include \"hbvmpub.h\"\n" );

      if( hb_comp_iGenCOutput == HB_COMPGENC_REALCODE )
      {
         fprintf( yyc, "#include \"hbxvm.h\"\n" );
         fprintf( yyc, "#include \"hbapierr.h\"\n" );
      }
      else if( hb_comp_iGenCOutput != HB_COMPGENC_COMPACT )
      {
         fprintf( yyc, "#include \"hbpcode.h\"\n" );
      }

      fprintf( yyc, "#include \"hbinit.h\"\n\n" );

      fprintf( yyc, "#define __PRG_SOURCE__ \"%s\"\n\n", hb_comp_PrgFileName );


      if( hb_comp_Namespaces.pFirst )
      {
         PNAMESPACE pNamespace = hb_comp_Namespaces.pFirst;

         do
         {
            if( pNamespace->type & NSTYPE_SPACE )
            {
               if( ( pNamespace->type & NSTYPE_STEALTH ) == NSTYPE_STEALTH )
               {
                  fprintf( yyc, "#include \"%s.xns\"\n", pNamespace->szName );
               }
            }

            pNamespace = pNamespace->pNext;
         }
         while ( pNamespace );

         fprintf( yyc, "\n" );
      }

      pFunc = hb_comp_functions.pFirst;

      if( ! hb_comp_bStartProc )
      {
         pFunc = pFunc->pNext; /* No implicit starting procedure */
      }

#ifdef BROKEN_MODULE_SPACE_LOGIC
      fprintf( yyc, "HB_FUNC_STATIC( __begin__ );\n" );
#endif

      /* write functions prototypes for PRG defined functions */
      while( pFunc )
      {
         bIsInitFunction   = ( pFunc->cScope & HB_FS_INIT & ~HB_FS_STATIC ) ;
         bIsExitFunction   = ( pFunc->cScope & HB_FS_EXIT & ~HB_FS_STATIC ) ;
         bIsStaticFunction = bIsInitFunction == FALSE && bIsExitFunction == FALSE && ( pFunc->cScope & HB_FS_STATIC ) ;
         bIsStaticVariable = ( pFunc == hb_comp_pInitFunc ) ;
         bIsGlobalVariable = ( pFunc == hb_comp_pGlobalsFunc ) ;
         bIsLineNumberInfo = ( pFunc == hb_comp_pLineNumberFunc );

         if( pFunc->cScope & HB_FS_CRITICAL )
         {
            bCritical = TRUE;
         }

         if( pFunc->pNamespace && ( isalpha( pFunc->szName[0] ) || pFunc->szName[0] == '_' ) )
         {
            if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
            {
               if ( bIsStaticFunction )
               {
                  fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pFunc->szName );
               }
               else
               {
                  fprintf( yyc, "HB_FUNC_OPTIONAL_NAMESPACE( /* %s */ %d, %s );\n", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
               }
            }
            else if( ( pFunc->pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
            {
               fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ %d, %s );\n", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
            }
            else if( ( pFunc->pNamespace->type & NSTYPE_STEALTH ) == NSTYPE_STEALTH )
            {
               fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ NSID_%s, %s );\n", pFunc->pNamespace->szFullPath, pFunc->pNamespace->szName, pFunc->szName );
            }
            else
            {
               fprintf( yyc, "HB_FUNC_NAMESPACE( /* %s */ %d, %s );\n", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
            }
         }
         /* Is it a PUBLIC FUNCTION/PROCEDURE */
         else if ( bIsStaticFunction )
         {
            fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pFunc->szName );
         }
         /* Is it a STATIC$ */
         else if ( bIsStaticVariable )
         {
            fprintf( yyc, "HB_FUNC_INITSTATICS();\n" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it a GLOBAL$ */
         else if ( bIsGlobalVariable )
         {
            fprintf( yyc, "HB_FUNC_INITGLOBALS();\n" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it an (_INITLINES) function */
         else if ( bIsLineNumberInfo )
         {
            fprintf( yyc, "HB_FUNC_INITLINES();\n" );
         }
         /* Is it an INIT FUNCTION/PROCEDURE */
         else if ( bIsInitFunction )
         {
            fprintf( yyc, "HB_FUNC_INIT( %.*s );\n", (int) strlen( pFunc->szName ) - 1, pFunc->szName );
         }
         /* Is it an EXIT FUNCTION/PROCEDURE */
         else if ( bIsExitFunction )
         {
            fprintf( yyc, "HB_FUNC_EXIT( %.*s );\n", (int) strlen( pFunc->szName ) - 1, pFunc->szName );
         }
         /* Then it must be a STATIC FUNCTION/PROCEDURE */
         else
         {
            fprintf( yyc, "HB_FUNC( %s );\n", pFunc->szName );
         }

         pFunc = pFunc->pNext;
      }

      {
         PNAMESPACE pNamespace = hb_comp_Namespaces.pFirst;

         while( pNamespace )
         {
            if( ( pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
            {
                char *szFileName = hb_xstrcpy( NULL, pNamespace->szName, ".hxns", NULL );
                FILE *yyc;
                PNAMESPACE pMember;
                int iLevel = 1;

                yyc = hb_fopen( szFileName, "wb" );

                if( ! yyc )
                {
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
                }

                hb_xfree( szFileName );

                fprintf( yyc, "#if defined( NAMESPACE_DECFUNCS ) && ! defined( __PRG__ ) \n" );

                pMember = pNamespace->pNext;

                while( pMember )
                {
                   if( pMember->type & NSTYPE_MEMBER )
                   {
                      if( ( pMember->pFunc->cScope & HB_FS_STATIC ) == 0 )
                      {
                         fprintf( yyc, "   HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ %d, %s );\n", pNamespace->szFullPath, pNamespace->iID, pMember->szName );
                      }
                   }

                   if( pMember->type & NSTYPE_SPACE )
                   {
                      pNamespace = pMember;
                      ++iLevel;
                   }

                   if( pMember->type & NSTYPE_END )
                   {
                      if( --iLevel == 0 )
                      {
                         break;
                      }
                   }

                   pMember = pMember->pNext;
                }

                fprintf( yyc, "\n#endif\n" ); // Dont remove prefix \n!
                fprintf( yyc, "\n" );
                fclose( yyc );

                if( pMember )
                {
                   pNamespace = pMember;
                }
                else
                {
                   break;
                }
            }
            else if( ( pNamespace->type & ( NSTYPE_EXTERNAL | NSTYPE_MEMBER ) ) == ( NSTYPE_EXTERNAL | NSTYPE_MEMBER ) )
            {
               if( ( pNamespace->pOuter->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL  )
               {
                  fprintf( yyc, "HB_FUNC_OPTIONAL_NAMESPACE( /* %s */ %d, %s );\n", pNamespace->pOuter->szFullPath, pNamespace->pOuter->iID, pNamespace->szName );
               }
               else
               {
                  fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ %d, %s );\n", pNamespace->pOuter->szFullPath, pNamespace->pOuter->iID, pNamespace->szName );
               }
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
            {
               fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pInline->szName );
            }
         }
         pInline = pInline->pNext;
      }

      /* check c-in-line static functions */
      pInline = hb_comp_inlines.pFirst;

      if ( pInline && pInline->pCode )
      {
         hb_compGenCInLineSymbol();
      }

      fprintf( yyc, "\n" );

      if( hb_comp_UsedNamespaces.pFirst )
      {
         PNAMESPACE pUsedNamespace = hb_comp_UsedNamespaces.pFirst;

         do
         {
            if( pUsedNamespace->szName[0] != '*' && ( pUsedNamespace->type & NSTYPE_SPACE ) && pUsedNamespace->pOuter == NULL )
            {
               fprintf( yyc, "\n#include \"%s.xns\"\n", pUsedNamespace->szName );
               fprintf( yyc, "extern HB_NS_%s %s;\n", pUsedNamespace->szName, pUsedNamespace->szName );
            }

            pUsedNamespace = pUsedNamespace->pNext;
         }
         while( pUsedNamespace );
      }

      fprintf( yyc, "\n" );

      /* write functions prototypes for called functions outside this PRG */
      pFunCall = hb_comp_funcalls.pFirst;
      while( pFunCall )
      {
         PFUNCTION pFunc = NULL;

         if( pFunCall->Namespace )
         {
            if( ( pFunCall->iFlags & NSF_RESOLVE ) == NSF_RESOLVE )
            {
               pFunc = hb_compFunctionResolve( pFunCall->szName, (PNAMESPACE) pFunCall->Namespace, NULL );
               //fprintf( yyc, "/* %s from %s resolved to %s */\n", pFunCall->szName, ( (PNAMESPACE) pFunCall->Namespace )->szName , pFunc && pFunc->pNamespace ? pFunc->pNamespace->szFullPath  : "global" );
            }
         }

         if( pFunc == (PFUNCTION) 1 )
         {
            fprintf( yyc, "/* Skipped: call to '%s' resolved to external */\n", pFunCall->szName );
         }
         else if( pFunCall->Namespace && ( ( ( pFunCall->iFlags & NSF_RESOLVE ) != NSF_RESOLVE ) || ( pFunc && pFunc->pNamespace ) ) )
         {
            // No prototype for Namespace function calls!
            if( pFunc && pFunc->pNamespace )
            {
               fprintf( yyc, "/* Skipped: call to '%s' resolved to: '%s' */\n", pFunCall->szName, pFunc->pNamespace->szFullPath );
            }
            else
            {
               fprintf( yyc, "/* Skipped: call to: '%s' of: '%s' */\n", pFunCall->szName, (char *) pFunCall->Namespace );
            }
         }
         else if( hb_compFunctionFind( pFunCall->szName, NULL, NSF_NONE ) == NULL && hb_compInlineFind( pFunCall->szName ) == NULL )
         {
            PCOMSYMBOL pSym = hb_compSymbolFind( pFunCall->szName, NULL, NULL, SYMF_FUNCALL );

            // Skip!
            if( pSym && ( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED ) )
            {
               if( pSym->Namespace )
               {
                  if( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR )
                  {
                     fprintf( yyc, "/* Skipped DEFERRED call to: '%s' of: '%s' */\n", pSym->szName, ( (PNAMESPACE) pSym->Namespace )->szFullPath );
                  }
                  else if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
                  {
                     fprintf( yyc, "/* Skipped DEFERRED call to: '%s' of: '%s' */\n", pSym->szName, (char *) pSym->Namespace );
                  }
                  else
                  {
                     assert(0);
                  }
               }
               else
               {
                  fprintf( yyc, "/* Skipped DEFERRED call to: '%s' */\n", pSym->szName );
               }
            }
            else if( hb_compCStaticSymbolFound( pFunCall->szName, HB_PROTO_FUNC_STATIC ) )
            {
               fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pFunCall->szName );
            }
            else
            {
               if( ! hb_compCStaticSymbolFound( pFunCall->szName, HB_PROTO_FUNC_PUBLIC ) )
               {
                  if( hb_comp_autoDeferred )
                  {
                     pSym->cScope |= HB_FS_DEFERRED;

                     if( fCodeExt )
                     {
                        if( ! bBeginExt )
                        {
                           fprintf( fCodeExt, "// External Functions Required by Module: \"%s\"\n\n", szSourceName );
                           fprintf( fCodeExt, "#ifndef __%s_EXTERNALS__\n", pFileName->szName );
                           fprintf( fCodeExt, "#define __%s_EXTERNALS__\n\n", pFileName->szName );
                           bBeginExt = TRUE;
                        }

                        fprintf( fCodeExt, "EXTERNAL %s\n", pFunCall->szName );
                     }
                  }
                  else
                  {
                     fprintf( yyc, "HB_FUNC_EXTERN( %s );\n", pFunCall->szName );
                  }
               }
            }
         }

         pFunCall = pFunCall->pNext;
      }

      /* Write fucntion prototypes */
      hb_compGenCAddProtos( yyc );

      if( bCritical )
      {
         fprintf( yyc, "\n#define HB_THREAD_SUPPORT\n" );
         fprintf( yyc, "#include \"thread.h\"\n\n" );

         pFunc = hb_comp_functions.pFirst;
         while( pFunc )
         {
            if( pFunc->cScope & HB_FS_CRITICAL )
            {
               fprintf( yyc, "static HB_CRITICAL_T s_Critical%s;\n", pFunc->szName );
            }

            pFunc = pFunc->pNext;
         }

         // Write init function for CRITICAL Functions Mutex initialization.
         fprintf( yyc, "\nHB_CALL_ON_STARTUP_BEGIN( hb_InitCritical%s )\n", hb_comp_FileAsSymbol );

         pFunc = hb_comp_functions.pFirst;
         while( pFunc )
         {
            if( pFunc->cScope & HB_FS_CRITICAL )
            {
               fprintf( yyc, "   HB_CRITICAL_INIT( s_Critical%s );\n", pFunc->szName );
            }

            pFunc = pFunc->pNext;
         }

         fprintf( yyc, "HB_CALL_ON_STARTUP_END( hb_InitCritical%s )\n", hb_comp_FileAsSymbol );
      }

#ifdef BROKEN_MODULE_SPACE_LOGIC
      fprintf( yyc, "HB_FUNC_STATIC( __end__ );\n" );
#endif

      if( hb_comp_Namespaces.pFirst )
      {
        void *pTemp = (void *) yyc;

        hb_compNamespaceEnumSpaces( hb_comp_Namespaces.pFirst, hb_compGenerateXNS, &pTemp );

        yyc = (FILE *)pTemp;
      }

      fprintf( yyc, "\n#undef HB_PRG_PCODE_VER\n" );
      fprintf( yyc, "#define HB_PRG_PCODE_VER %i\n", (int) HB_PCODE_VER );

      hb_compWriteDeclareGlobal( yyc );

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
         while ( pNamespace );
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
               {
                  fprintf( yyc, "%s\\0", pNamespace->szFullPath );
               }
            }

            pNamespace = pNamespace->pNext;
         }
         while ( pNamespace );

         fprintf( yyc, "\";\n"
                       "#undef HB_MODULE_NAMESPACES\n"
                       "#define HB_MODULE_NAMESPACES pNamespaces\n" );
      }

      /* writes the symbol table */
      /* Generate the wrapper that will initialize local symbol table
       */
      fprintf( yyc, "\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_%s%s )\n", hb_comp_szPrefix, hb_comp_FileAsSymbol );

      iSymOffset = 0;
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
                  {
                     pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPATH );
                  }
                  else
                  {
                     pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPTR );
                  }

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
                     {
                        pSym->cScope |= ( HB_FS_INDIRECT | HB_FS_PUBLIC );
                     }
                  }
               }
               else if( ( pSym->iFlags & SYMF_NS_RESOLVE ) == SYMF_NS_RESOLVE )
               {
                  assert( ( pSym->cScope & HB_FS_PUBLIC ) == 0 );

                  pFunc = hb_compFunctionResolve( pSym->szName, (PNAMESPACE) pSym->Namespace, pSym );

                  if( pFunc == (PFUNCTION) 1 )
                  {
                     // Resolved to external member.
                     pFunc = NULL;
                  }
               }
               else if( ( pSym->cScope & HB_FS_LOCAL ) != HB_FS_LOCAL )
               {
                  /* It's a function defined in this module */
                  if( ( pFunc = hb_compFunctionFind( pSym->szName, NULL, SYMF_FUNCALL ) ) != NULL )
                  {
                     assert( 0 );
                  }
                  else if( hb_compCStaticSymbolFound( pSym->szName, HB_PROTO_FUNC_PUBLIC ) )
                  {
                     if( ( pSym->cScope & HB_FS_STATIC ) == 0 )
                     {
                        pSym->cScope |= ( HB_FS_LOCAL | HB_FS_PUBLIC );
                     }
                  }
                  else if( hb_compCStaticSymbolFound( pSym->szName, HB_PROTO_FUNC_STATIC ) )
                  {
                     if( ( pSym->cScope & HB_FS_PUBLIC ) == 0 )
                     {
                        pSym->cScope |= ( HB_FS_LOCAL | HB_FS_STATIC );
                     }
                  }
                  else
                  {
                     if( ( pSym->cScope & HB_FS_STATIC ) == 0 )
                     {
                        pSym->cScope |= HB_FS_PUBLIC;
                     }
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
                  {
                     assert( ( pSym->cScope & HB_FS_STATIC ) == 0 );
                  }
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
                  {
                    fprintf( yyc, "{ \"%s.%s\", {", (char *) pSym->Namespace, pSym->szName );
                  }
                  else
                  {
                     fprintf( yyc, "{ \"%s\", {", pSym->szName );
                  }
               }
               else if( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR )
               {
                  //if( ( ( (PNAMESPACE) pSym->Namespace )->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )

                  if( ( ( (PNAMESPACE) pSym->Namespace )->type & NSTYPE_RUNTIME ) == NSTYPE_RUNTIME )
                  {
                     fprintf( yyc, "{ \"%s.%s\", {", ( (PNAMESPACE) pSym->Namespace )->szFullPath, pSym->szName );
                  }
                  else if( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
                  {
                     fprintf( yyc, "{ \"%s.%s\", {", ( (PNAMESPACE) pSym->Namespace )->szFullPath, pSym->szName );
                  }
                  else
                  {
                     fprintf( yyc, "{ \"%s\", {", pSym->szName );
                  }
               }
               else
               {
                  assert(0);
               }
            }
            else
            {
               fprintf( yyc, "{ \"%s\", {", pSym->szName );
            }

            if( pSym->cScope & HB_FS_INIT & ~HB_FS_STATIC )
            {
               fprintf( yyc, "HB_FS_INIT" );
            }
            else if( pSym->cScope & HB_FS_EXIT & ~HB_FS_STATIC )
            {
               fprintf( yyc, "HB_FS_EXIT" );
            }
            else if( pSym->cScope & HB_FS_STATIC )
            {
               fprintf( yyc, "HB_FS_STATIC" );
            }
            else if( ( pSym->cScope & HB_FS_INDIRECT ) == HB_FS_INDIRECT )
            {
               fprintf( yyc, "HB_FS_INDIRECT" );
            }
            else
            {
               fprintf( yyc, "HB_FS_PUBLIC" );
            }

            if( ( pSym->cScope & HB_FS_LOCAL ) == HB_FS_LOCAL )
            {
               fprintf( yyc, " | HB_FS_LOCAL" );
            }
            else if( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED ) // MUTUALLY EXCLUSIVE
            {
               fprintf( yyc, " | HB_FS_DEFERRED" );
            }

            if ( ( pSym->cScope & HB_FS_FIRST ) &&  ( ! hb_comp_bNoStartUp ) )
            {
               fprintf( yyc, " | HB_FS_FIRST" );
               iStartupOffset = iSymOffset;
               bSymFIRST = TRUE;
            }

            /* specify the function address if it is a defined function or an
               external called function */

            if( ( pSym->cScope & HB_FS_INDIRECT ) == HB_FS_INDIRECT )
            {
               if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
               {
                  fprintf( yyc, "}, {(PHB_FUNC) &%s.%s}, &ModuleFakeDyn }", (char *) pSym->Namespace, pSym->szName );
               }
               else
               {
                  fprintf( yyc, "}, {(PHB_FUNC) &%s.%s}, &ModuleFakeDyn }", ( (PNAMESPACE) pSym->Namespace )->szFullPath, pSym->szName );
               }
            }
            else if( ( pSym->iFlags & SYMF_FUNCALL ) && ( ( pSym->cScope & HB_FS_LOCAL ) == HB_FS_LOCAL ) ) /* is it a function defined in this module */
            {
               if( ( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH ) || ( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR ) )
               {
                  assert( pFunc );

                  // pFunc is NOT a typo - resolved above!
                  if( ( pFunc->pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
                  {
                     // pFunc is NOT a typo - resolved above!
                     fprintf( yyc, "}, {HB_EXTERNAL_NAMESPACE_FUNCNAME( %d, %s )}, &ModuleFakeDyn }", pFunc->pNamespace->iID, pSym->szName );
                  }
                  else if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
                  {
                     // pFunc is NOT a typo - resolved above!
                     fprintf( yyc, "}, {HB_OPTIONAL_NAMESPACE_FUNCNAME( %d, %s )}, &ModuleFakeDyn }", pFunc->pNamespace->iID, pSym->szName );
                  }
                  else
                  {
                     // pFunc is NOT a typo - resolved above!
                     fprintf( yyc, "}, {HB_NAMESPACE_FUNCNAME( %d, %s )}, &ModuleFakeDyn }", pFunc->pNamespace->iID, pSym->szName );
                  }
               }
               else if( pSym->cScope & HB_FS_INIT & ~HB_FS_STATIC )
               {
                  fprintf( yyc, "}, {HB_INIT_FUNCNAME( %.*s )}, &ModuleFakeDyn }", (int) strlen( pSym->szName ) - 1, pSym->szName );
               }
               else if( pSym->cScope & HB_FS_EXIT & ~HB_FS_STATIC )
               {
                  fprintf( yyc, "}, {HB_EXIT_FUNCNAME( %.*s )}, &ModuleFakeDyn }", (int) strlen( pSym->szName ) - 1, pSym->szName );
               }
               else
               {
                  fprintf( yyc, "}, {HB_FUNCNAME( %s )}, &ModuleFakeDyn }", pSym->szName );
               }
            }
            else if( pSym->iFlags & SYMF_FUNCALL ) //&& hb_compFunCallFind( pSym->szName, pSym->Namespace, pSym->iFlags ) ) /* is it a function called from this module */
            {
               if( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
               {
                  fprintf( yyc, "}, {NULL}, NULL }" );
               }
               else
               {
                  fprintf( yyc, "}, {HB_FUNCNAME( %s )}, NULL }", pSym->szName );
               }
            }
            else
            {
               fprintf( yyc, "}, {NULL}, NULL }" );   /* memvar */
            }
         }

         if( pSym != hb_comp_symbols.pLast )
         {
            fprintf( yyc, ",\n" );
         }

         pSym = pSym->pNext;

         iSymOffset++;
      }

      /*
         Write entries for external functions
      */
      if( pStatSymb )
      {
         hb_compWriteExternEntries( yyc, bSymFIRST, TRUE, TRUE );
      }

      /*
         End of initializaton codes
      */
      hb_writeEndInit( yyc );

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

         fprintf( yyc, "#if defined(HB_PRAGMA_STARTUP)\n" );
         fprintf( yyc, "   #pragma startup hb_InitExplicitStartup\n" );
         fprintf( yyc, "#elif defined(HB_MSC_STARTUP)\n"
                       "   #if _MSC_VER >= 1010\n"
                       "      #pragma data_seg( \".CRT$XIY\" )\n"
                       "      #pragma comment( linker, \"/Merge:.CRT=.data\" )\n"
                       "   #else\n"
                       "      #pragma data_seg( \"XIY\" )\n"
                       "   #endif\n"
                       "   static HB_$INITSYM hb_auto_InitExplicitStartup = hb_InitExplicitStartup;\n"
                       "   #pragma data_seg()\n"
                       "#endif\n\n" );
      }

      if( bCritical )
      {
         fprintf( yyc, "#if defined(HB_PRAGMA_STARTUP)\n" );
         fprintf( yyc, "   #pragma startup hb_InitCritical%s\n", hb_comp_FileAsSymbol );
         fprintf( yyc, "#elif defined(HB_MSC_STARTUP)\n"
                       "   #if _MSC_VER >= 1010\n"
                       "      #pragma data_seg( \".CRT$XIY\" )\n"
                       "      #pragma comment( linker, \"/Merge:.CRT=.data\" )\n"
                       "   #else\n"
                       "      #pragma data_seg( \"XIY\" )\n"
                       "   #endif\n"
                       "   static HB_$INITSYM hb_auto_InitCritical = hb_InitCritical%s;\n"
                       "   #pragma data_seg()\n"
                       "#endif\n\n", hb_comp_FileAsSymbol );
      }

      /* Generate functions data
       */
      pFunc = hb_comp_functions.pFirst;

      if( ! hb_comp_bStartProc )
      {
         pFunc = pFunc->pNext; /* No implicit starting procedure */
      }

      while( pFunc )
      {
         bIsInitFunction   = ( pFunc->cScope & HB_FS_INIT & ~HB_FS_STATIC ) ;
         bIsExitFunction   = ( pFunc->cScope & HB_FS_EXIT & ~HB_FS_STATIC ) ;
         bIsStaticFunction = bIsInitFunction == FALSE && bIsExitFunction == FALSE && ( pFunc->cScope & HB_FS_STATIC ) ;
         bIsStaticVariable = ( pFunc == hb_comp_pInitFunc ) ;
         bIsGlobalVariable = ( pFunc == hb_comp_pGlobalsFunc ) ;
         bIsLineNumberInfo = ( pFunc == hb_comp_pLineNumberFunc );

         if( pFunc->pNamespace && ( isalpha( pFunc->szName[0] ) || pFunc->szName[0] == '_' ) )
         {
            if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
            {
               if ( bIsStaticFunction )
               {
                  fprintf( yyc, "HB_FUNC_STATIC( %s )", pFunc->szName );
               }
               else
               {
                  fprintf( yyc, "HB_FUNC_OPTIONAL_NAMESPACE( /* %s */ %d, %s )", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
               }
            }
            else if( ( pFunc->pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
            {
               fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ %d, %s )", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
            }
            else if( ( pFunc->pNamespace->type & NSTYPE_STEALTH ) == NSTYPE_STEALTH )
            {
               fprintf( yyc, "HB_FUNC_EXTERNAL_NAMESPACE( /* %s */ NSID_%s, %s )", pFunc->pNamespace->szFullPath, pFunc->pNamespace->szName, pFunc->szName );
            }
            else
            {
               fprintf( yyc, "HB_FUNC_NAMESPACE( /* %s */ %d, %s )", pFunc->pNamespace->szFullPath, pFunc->pNamespace->iID, pFunc->szName );
            }
         }
         /* Is it a PUBLIC FUNCTION/PROCEDURE */
         else if ( bIsStaticFunction )
         {
            fprintf( yyc, "HB_FUNC_STATIC( %s )", pFunc->szName );
         }
         /* Is it STATICS$ */
         else if( bIsStaticVariable )
         {
            fprintf( yyc, "HB_FUNC_INITSTATICS()" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it GLOBALS$ */
         else if( bIsGlobalVariable )
         {
            fprintf( yyc, "HB_FUNC_INITGLOBALS()" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it (_INITLINES) */
         else if( bIsLineNumberInfo )
         {
            fprintf( yyc, "HB_FUNC_INITLINES()" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it an INIT FUNCTION/PROCEDURE */
         else if ( bIsInitFunction )
         {
            fprintf( yyc, "HB_FUNC_INIT( %.*s )", (int) strlen( pFunc->szName ) - 1, pFunc->szName );
         }
         /* Is it an EXIT FUNCTION/PROCEDURE */
         else if ( bIsExitFunction )
         {
            fprintf( yyc, "HB_FUNC_EXIT( %.*s )", (int) strlen( pFunc->szName ) - 1, pFunc->szName );
         }
         /* Then it must be a STATIC FUNCTION/PROCEDURE */
         else
         {
            fprintf( yyc, "HB_FUNC( %s )", pFunc->szName );
         }

         fprintf( yyc, "\n" );
         if( hb_comp_iGenCOutput == HB_COMPGENC_REALCODE )
         {
            hb_compGenCRealCode( pFunc, yyc );
         }
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
         fprintf( yyc, "HB_FUNC_REGISTERGLOBAL()\n"
                       "{\n"
                       "   hb_vmRegisterGlobals( &pGlobals, %i );\n", iGlobals );
         fprintf( yyc, "}\n\n" );
      }

      /*
      Generate pCode Listing
      */

      /* Generate codeblocks data
       */
      if( hb_comp_cInlineID )
      {
         fprintf( yyc, "#include \"hbapi.h\"\n" );
         fprintf( yyc, "#include \"hbstack.h\"\n" );
         fprintf( yyc, "#include \"hbapierr.h\"\n" );
         fprintf( yyc, "#include \"hbapiitm.h\"\n" );
         fprintf( yyc, "#include \"hbvm.h\"\n" );
         fprintf( yyc, "#include \"hboo.ch\"\n" );
      }

      pInline = hb_comp_inlines.pFirst;

      if ( pInline && pInline->pCode )
      {
         hb_compGenCInLine( yyc );
      }

#ifdef BROKEN_MODULE_SPACE_LOGIC
      fprintf( yyc, "\nHB_FUNC_STATIC( __end__ ){}\n" );
#endif
   }
   else
   {
      /*
         We do not have an ordinary PRG code in file
      */

      BOOL bInline = (pInline && pInline->pCode);

      if ( bInline )
      {
         hb_compGenCInLineSymbol();
      }

      /*
        We have functions in dump areas
      */

      if ( pStatSymb )
      {
         fprintf( yyc, "#include \"hbvmpub.h\"\n" );

         if( hb_comp_iGenCOutput != HB_COMPGENC_COMPACT )
         {
            fprintf( yyc, "#include \"hbpcode.h\"\n" );
         }

         fprintf( yyc, "#include \"hbinit.h\"\n\n" );

         hb_compGenCAddProtos( yyc );

         if ( hb_compWriteExternEntries( yyc, bSymFIRST, FALSE, FALSE ) )
         {
            hb_writeEndInit( yyc );
         }
      }

      if ( bInline )
      {
         hb_compGenCInLine( yyc );
      }
      else
      {
         if( !pStatSymb )
         {
            fprintf( yyc, "/* Empty source file */\n\n" );
         }
      }
   }

   fclose( yyc );

   if ( fCodeExt )
   {
      fprintf( fCodeExt, "\n#endif\n" );
      fclose( fCodeExt );
   }

#ifdef HB_BACK_END
   if( HB_BACK_END == 0 )
#endif
   {
      if( ! hb_comp_bQuiet )
      {
         printf( "Done.\n" );
      }
   }

   pTemp = pStatSymb;

   while( pTemp )
   {
      // printf( "RELEASING : >>%s<<\n", pTemp->szName );
      hb_xfree( pTemp->szName );
      pTemp = pTemp->pNext;
      hb_xfree( ( void * ) pStatSymb );
      pStatSymb = pTemp;
   }
}

/*
   Write symbol entries to intialized on startup
*/
static BOOL hb_compWriteExternEntries( FILE *yyc, BOOL bSymFIRST, BOOL bNewLine, BOOL bPrg )
{
   BOOL bStartFunc = FALSE;
   BOOL bBegin = FALSE;
   PSSYMLIST pTemp = pStatSymb;
   USHORT ulLen = pStatSymb->uEntry;
   char *szEntries = (char*) hb_xgrab( (ulLen + 1) * 256  );
   USHORT ul;

   hb_xmemset( szEntries, '\0', (ulLen + 1) * 256 );

   while( pTemp )
   {
      if( ! hb_compFunctionFind( pTemp->szName, NULL, NSF_NONE ) && ! hb_compSymbFound( pTemp->szName ) && ( pTemp->Type & ( HB_PROTO_FUNC_PUBLIC | HB_PROTO_FUNC_EXIT | HB_PROTO_FUNC_INIT ) ) )
      {
         if( bNewLine && !bBegin )
         {
            bBegin = TRUE;
            strcat( szEntries, ",\n" );
         }

         if( ( ! bNewLine ) && ( ! bBegin ) )
         {
            char szVer[5];
            bBegin = TRUE;
            sprintf( szVer,"%i", HB_PCODE_VER );
            hb_xstrcat( szEntries,
               "\n#define __PRG_SOURCE__ \"",hb_comp_PrgFileName,"\"\n\n",
               "#undef HB_PRG_PCODE_VER\n",
               "#define HB_PRG_PCODE_VER ", szVer,"\n",
               "\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_",hb_comp_szPrefix,hb_comp_FileAsSymbol," )\n", NULL );
         }

         if( bPrg )
         {
            if( hb_compFunCallFind( pTemp->szName, NULL, NSF_NONE ) )
            {
               hb_xstrcat( szEntries, "{ \"", pTemp->szName, "\", {HB_FS_PUBLIC | HB_FS_LOCAL", NULL );

               if( !bSymFIRST && !hb_comp_bNoStartUp && !bStartFunc )
               {
                  bStartFunc = TRUE;
                  strcat( szEntries, " | HB_FS_FIRST" );
               }

               hb_xstrcat( szEntries, "}, {HB_FUNCNAME( ", pTemp->szName," )}, NULL },\n", NULL );
            }
         }
         else
         {
            if( ! bSymFIRST && ! hb_comp_bNoStartUp && !bStartFunc  )
            {
               bStartFunc = TRUE;
               hb_xstrcat( szEntries, "{ \"",pTemp->szName,"\", {HB_FS_PUBLIC | HB_FS_LOCAL", NULL );
               strcat( szEntries, " | HB_FS_FIRST" );
               hb_xstrcat( szEntries, "}, {HB_FUNCNAME( ",pTemp->szName," )}, NULL },\n", NULL );
            }
         }

         if( pTemp->Type == HB_PROTO_FUNC_EXIT )
         {
            hb_xstrcat( szEntries, "{ \"",pTemp->szName,"$\", {HB_FS_EXIT | HB_FS_LOCAL}, {HB_EXIT_FUNCNAME( ",pTemp->szName," )}, &ModuleFakeDyn },\n", NULL );
         }
         else if( pTemp->Type == HB_PROTO_FUNC_INIT )
         {
            hb_xstrcat( szEntries, "{ \"",pTemp->szName,"$\", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( ",pTemp->szName," )}, &ModuleFakeDyn },\n", NULL );
         }
      }

      pTemp = pTemp->pNext;

   }

   ul = strlen( szEntries );

   if( ul && szEntries[ul - 2 ] == ',' )
   {
      szEntries[ul - 2 ] = '\0';
   }

   if( bPrg || bStartFunc )
   {
      fprintf( yyc, szEntries );
   }

   hb_xfree( szEntries );

   return ( bStartFunc );
}

static void hb_compWriteDeclareGlobal( FILE *yyc )
{
   fprintf( yyc, "\n#include \"hbapi.h\"\n\n" );

   if( hb_comp_pGlobals )
   {
      PVAR pGlobal = hb_comp_pGlobals;

      while( pGlobal )
      {
         if( pGlobal->szAlias == NULL )
         {
            fprintf( yyc, "HB_ITEM_NEW( %s );\n", pGlobal->szName );
         }
         else
         {
            fprintf( yyc, "extern HB_ITEM %s;\n", pGlobal->szName );
         }

         pGlobal = pGlobal->pNext;
      }

      fprintf( yyc, "\nstatic const PHB_ITEM pConstantGlobals[] = {\n" );

      pGlobal = hb_comp_pGlobals;

      while( pGlobal )
      {
         fprintf( yyc, "                                             &%s%c\n", pGlobal->szName, pGlobal->pNext ? ',' : ' ' );
         pGlobal = pGlobal->pNext;
      }

      fprintf( yyc, "                                           };\n"
                    "static PHB_ITEM *pGlobals = (PHB_ITEM *) pConstantGlobals;\n"
                    "#undef HB_MODULE_GLOBALS\n"
                    "#define HB_MODULE_GLOBALS pGlobals\n" );
   }
}

/*
   Write function prototype for Global Variables
*/
static void hb_compWriteGlobalFunc( FILE *yyc, short *iLocalGlobals, short * iGlobals, BOOL bWriteProto )
{
   // Any NON EXTERN Globals to Register?
   PVAR pGlobal = hb_comp_pGlobals;
   short uGlobals = 0, uLocalGlobals = 0;

   while( pGlobal )
   {
      uGlobals++;

      if( pGlobal->szAlias == NULL )
      {
         uLocalGlobals++;
      }

      pGlobal = pGlobal->pNext;
   }

   if( uLocalGlobals && bWriteProto )
   {
      fprintf( yyc, "HB_FUNC_REGISTERGLOBAL();\n" ); /* NOTE: hb_ intentionally in lower case */
   }

   *iGlobals      = uGlobals;
   *iLocalGlobals = uLocalGlobals;
}

/*
   Functions prototypes from dump areas
*/
static void hb_compGenCAddProtos( FILE *yyc )
{
   PSSYMLIST pTemp = pStatSymb;

   while( pTemp )
   {
      if( ! hb_compFunctionFind( pTemp->szName, NULL, NSF_NONE ) )
      {
         if ( pTemp->Type == HB_PROTO_FUNC_EXIT )
         {
            fprintf( yyc, "HB_FUNC_EXIT( %s );\n",pTemp->szName);
         }
         else if ( pTemp->Type == HB_PROTO_FUNC_INIT )
         {
            fprintf( yyc, "HB_FUNC_INIT( %s );\n",pTemp->szName);
         }
         else if ( pTemp->Type == HB_PROTO_FUNC_STATIC && !hb_compSymbFound( pTemp->szName ) )
         {
            fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pTemp->szName );
         }
         else if ( pTemp->Type == HB_PROTO_FUNC_PUBLIC )
         {
            fprintf( yyc, "HB_FUNC( %s );\n", pTemp->szName );
         }
      }
      pTemp = pTemp->pNext;
   }
}

static void hb_writeEndInit( FILE* yyc )
{
   fprintf( yyc,
#ifdef BROKEN_MODULE_SPACE_LOGIC
                 ",\n"
                 "{ \"!\", {0}, {HB_FUNCNAME( __begin__ )}, NULL },\n"
                 "{ \"!\", {0}, {HB_FUNCNAME( __end__ )}, NULL }"
#endif
                 "\nHB_INIT_SYMBOLS_END( hb_vm_SymbolInit_%s%s )\n\n"
                 "#if defined(HB_PRAGMA_STARTUP)\n"
                 "   #pragma startup hb_vm_SymbolInit_%s%s\n"
                 "#elif defined(HB_MSC_STARTUP)\n"
                 "   #if _MSC_VER >= 1010\n"
                 /* [pt] First version of MSC I have that supports this */
                 /* is msvc4.1 (which is msc 10.10) */
                 "      #pragma data_seg( \".CRT$XIY\" )\n"
                 "      #pragma comment( linker, \"/Merge:.CRT=.data\" )\n"
                 "   #else\n"
                 "      #pragma data_seg( \"XIY\" )\n"
                 "   #endif\n"
                 "   static HB_$INITSYM hb_vm_auto_SymbolInit_%s%s = hb_vm_SymbolInit_%s%s;\n"
                 "   #pragma data_seg()\n"
                 "#endif\n\n",
                 hb_comp_szPrefix, hb_comp_FileAsSymbol,
                 hb_comp_szPrefix, hb_comp_FileAsSymbol,
                 hb_comp_szPrefix, hb_comp_FileAsSymbol,
                 hb_comp_szPrefix, hb_comp_FileAsSymbol );
}

/*
  Searching for function names in glocal symbol list
*/
static BOOL hb_compSymbFound( char* szSymbol )
{
   BOOL bStatSymFound = FALSE;
   PCOMSYMBOL pSym_ = hb_comp_symbols.pFirst;

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
static BOOL hb_compCStaticSymbolFound( char* szSymbol, int iOption )
{
   BOOL bFound = FALSE;
   PSSYMLIST pTemp = pStatSymb;

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
  Collecting function names from in-line-c. There are three categories, ie
  HB_FUNC_STATIC, HB_FUNC and HB_FUNC_EXEC
*/
static void hb_compCStatSymList( char* statSymName, int iOption )
{
   PSSYMLIST pTemp, pLast;
   int ulLen = strlen( statSymName );

   while( ulLen && HB_ISSPACE( statSymName[ ulLen - 1 ] ) )
   {
      ulLen--;
   }

   statSymName[ ulLen ] = '\0';

   if (hb_compCStaticSymbolFound( statSymName, iOption ))
   {
      return;
   }

   pTemp = (PSSYMLIST) hb_xgrab( sizeof( SSYMLIST ) );
   pTemp->szName = (char*) hb_xgrab( strlen( statSymName ) + 1 );
   strcpy( pTemp->szName, statSymName );
   pTemp->Type = iOption;
   pTemp->pNext = NULL;

   if( pStatSymb )
   {
      pLast = pStatSymb;

      while( pLast->pNext )
      {
         pLast = pLast->pNext;
      }

      pLast->pNext = pTemp;
      pStatSymb->uEntry ++;
   }
   else
   {
      pStatSymb = pTemp;
      pStatSymb->uEntry = 1;
   }
}

/*
  Parsing in-line-c codes to extract function names
*/
static void hb_compGenCCheckInLineStatic( char *sInline )
{
   char *szTmp, *szTmp2;
   int iOption;
   char *sBase = sInline;

   //printf( "%s\n", sInline );

   while( ( sInline = strstr( sInline, "HB_FUNC" ) ) != NULL )
   {
      sInline += 7;
      iOption = HB_PROTO_FUNC_PUBLIC;

      /* If it is a PHB_FUNC then skip it */
      if ( sInline - sBase >= 8 && *(sInline - 8 ) == 'P' )
      {
         continue;
      }
      /* If it is a HB_FUNCNAME, we want it for externs */
      else if ( sInline[0] == 'N' &&
                sInline[1] == 'A' &&
                sInline[2] == 'M' &&
                sInline[3] == 'E' )
      {
         sInline += 4;
         continue;
      }
      /* If it is a HB_FUNC_PTR then skip it */
      else if ( sInline[0] == '_' &&
                sInline[1] == 'P' &&
                sInline[2] == 'T' &&
                sInline[3] == 'R' )
      {
         sInline += 4;
         continue;
      }
      /* If it is a HB_FUNC_EXTERN then skip it */
      else if ( sInline[0] == '_' &&
                sInline[1] == 'E' &&
                sInline[2] == 'X' &&
                sInline[3] == 'T' &&
                sInline[4] == 'E' &&
                sInline[5] == 'R' &&
                sInline[6] == 'N' )
      {
         sInline += 7;
         continue;
      }
      /* If it is a HB_FUNC_EXEC then skip it */
      else if ( sInline[0] == '_' &&
                sInline[1] == 'E' &&
                sInline[2] == 'X' &&
                sInline[3] == 'E' &&
                sInline[4] == 'C' )
      {
         sInline += 5;
         continue;
      }
      /* If it is a HB_FUNC_EXIT */
      else if ( sInline[0] == '_' &&
                sInline[1] == 'E' &&
                sInline[2] == 'X' &&
                sInline[3] == 'I' &&
                sInline[4] == 'T' )
      {
         iOption = HB_PROTO_FUNC_EXIT;
         sInline += 5;
      }
      /* If it is a HB_FUNC_INIT */
      else if ( sInline[0] == '_' &&
                sInline[1] == 'I' &&
                sInline[2] == 'N' &&
                sInline[3] == 'I' &&
                sInline[4] == 'T' )
      {
         iOption = HB_PROTO_FUNC_INIT;
         sInline += 5;
      }
      /* If it is a HB_FUNC_STATIC we want it */
      else if ( sInline[0] == '_' &&
                sInline[1] == 'S' &&
                sInline[2] == 'T' &&
                sInline[3] == 'A' &&
                sInline[4] == 'T' &&
                sInline[5] == 'I' &&
                sInline[6] == 'C' )
      {
         iOption = HB_PROTO_FUNC_STATIC;
         sInline += 7;
      }

      szTmp = strchr( sInline, '(' );
      if( szTmp == NULL )
      {
         continue;
      }
      szTmp++;

      while( HB_ISSPACE( *szTmp ) )
      {
         szTmp++;
      }

      szTmp2 = strchr( szTmp, ')' );
      if( szTmp2 == NULL )
      {
         continue;
      }

      *szTmp2 = '\0';

      hb_compCStatSymList( szTmp, iOption );
      *szTmp2 = ')';

      sInline = szTmp2 + 1;
   }
}

/*
  Grab the content of in-line-c codes to be parse for function names
*/
static void hb_compGenCInLineSymbol()
{
   PINLINE pInline = hb_comp_inlines.pFirst;

   while( pInline )
   {
      char *sInline = (char*) hb_xgrab( strlen( (char*) pInline->pCode) + 1 );
      sprintf( sInline, "%s", pInline->pCode );

      if( sInline )
      {
         hb_compGenCCheckInLineStatic( sInline );
         hb_xfree( sInline );
         pInline = pInline->pNext;
      }
   }
}

/*
  "Copy & Paste" the contents of in-line-c to C file output
*/
static void hb_compGenCInLine( FILE *yyc )
{
   PINLINE pInline = hb_comp_inlines.pFirst;
   char *pszFileName;

   while( pInline )
   {
      fprintf( yyc, "#line %i \"", ( pInline->iLine + 1 ) );

      pszFileName = pInline->szFileName;

      while( *pszFileName )
      {
         if( *pszFileName == '\\' )
         {
            fprintf( yyc, "\\" );
         }
         fprintf( yyc, "%c", *pszFileName++ );
      }

      fprintf( yyc, "\"\n" );

      if( pInline->szName )
      {
         fprintf( yyc, "HB_FUNC_STATIC( %s )\n", pInline->szName );
      }

      fprintf( yyc, "%s", pInline->pCode );
      pInline = pInline->pNext;
   }
}

static HB_GENC_FUNC( hb_p_and )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_AND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPUSH,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dec )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DEC,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraydim )
{
   fprintf( cargo->yyc, "\tHB_P_ARRAYDIM, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_divide )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DIVIDE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_do )
{
   fprintf( cargo->yyc, "\tHB_P_DO, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_doshort )
{
   fprintf( cargo->yyc, "\tHB_P_DOSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_duplicate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DUPLICATE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dupltwo )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DUPLTWO,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_equal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_exactlyequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EXACTLYEQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endblock )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   --cargo->iNestedCodeblock;
   fprintf( cargo->yyc, "\tHB_P_ENDBLOCK,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endproc )
{
   if( (lPCodePos+1) == pFunc->lPCodePos )
   {
      fprintf( cargo->yyc, "\tHB_P_ENDPROC\n" );
   }
   else
   {
      fprintf( cargo->yyc, "\tHB_P_ENDPROC,\n" );
   }
   return 1;
}

static HB_GENC_FUNC( hb_p_false )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FALSE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_fortest )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FORTEST,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_frame )
{
   fprintf( cargo->yyc, "\tHB_P_FRAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* locals, params */" );
   }

   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_funcptr )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FUNCPTR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_function )
{
   fprintf( cargo->yyc, "\tHB_P_FUNCTION, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_functionshort )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FUNCTIONSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_arraygen )
{
   fprintf( cargo->yyc, "\tHB_P_ARRAYGEN, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_hashgen )
{
   fprintf( cargo->yyc, "\tHB_P_HASHGEN, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_greater )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_GREATER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_greaterequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_GREATEREQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inc )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INC,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_instring )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INSTRING,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_jumpnear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_jump )
{
   fprintf( cargo->yyc, "\tHB_P_JUMP, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumpfalsenear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSENEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumpfalse )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfalsefar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSEFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumptruenear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUENEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumptrue )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumptruefar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUEFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_less )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LESS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_lessequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LESSEQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_line )
{
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "/* %05li */ ", lPCodePos );
   }
   else
   {
      fprintf( cargo->yyc, "\t" );
   }

   fprintf( cargo->yyc, "HB_P_LINE, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_localname )
{
   ULONG ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_LOCALNAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 3 );
   }

   fprintf( cargo->yyc, "\n" );
   lPCodePos += 3;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];
      if( chr == '\'' || chr == '\\')
      {
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      }
      else
      {
         fprintf( cargo->yyc, " \'%c\',", chr );
      }
   }
   fprintf( cargo->yyc, " 0,\n" );

   return lPCodePos - ulStart + 1;
}

static HB_GENC_FUNC( hb_p_macropop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOP, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropopaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOPALIASED, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSH, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropusharg )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHARG, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushlist )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHLIST, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushindex )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHINDEX, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushpare )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHPARE, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHALIASED, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macrosymbol )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROSYMBOL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrotext )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROTEXT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_message )
{
   fprintf( cargo->yyc, "\tHB_P_MESSAGE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_minus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MINUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modulename )
{
   ULONG ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_MODULENAME," );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 1 );
   }
   fprintf( cargo->yyc, "\n" );
   lPCodePos++;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];
      if( chr == '\'' || chr == '\\')
      {
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      }
      else
      {
         fprintf( cargo->yyc, " \'%c\',", chr );
      }
   }
   fprintf( cargo->yyc, " 0,\n" );

   return lPCodePos - ulStart + 1;
}

static HB_GENC_FUNC( hb_p_modulus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MODULUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_mult )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MULT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_negate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NEGATE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_not )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_notequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOTEQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_or )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_OR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_parameter )
{
   fprintf( cargo->yyc, "\tHB_P_PARAMETER, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_plus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PLUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POPALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popaliasedfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELDNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_popaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocal )
{
   fprintf( cargo->yyc, "\tHB_P_POPLOCAL, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */
      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_poplocalnear )
{
   fprintf( cargo->yyc, "\tHB_P_POPLOCALNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int) (signed char) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
         * referenced in a codeblock -handle it with care
         */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_popmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPMEMVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_popstatic )
{
   fprintf( cargo->yyc, "\tHB_P_POPSTATIC, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      PVAR pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;
      USHORT wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
      {
         pTmp = pTmp->pNext;
      }

      pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );

      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popvariable )
{
   fprintf( cargo->yyc, "\tHB_P_POPVARIABLE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName  );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_power )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POWER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushaliasedfield )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELDNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushblock )
{
   USHORT wVar, w;
   ULONG ulStart = lPCodePos;

   ++cargo->iNestedCodeblock;

   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCK, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */",
               HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }
   fprintf( cargo->yyc, "\n" );

   w = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 3 ] ) );
   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* number of local parameters (%i) */", w );
   }
   fprintf( cargo->yyc, "\n" );

   wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 5 ] ) );

   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ lPCodePos + 5 ],
            pFunc->pCode[ lPCodePos + 6 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* number of local variables (%i) */", wVar );
   }

   fprintf( cargo->yyc, "\n" );

   lPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */

   /* create the table of referenced local variables */
   while( wVar-- )
   {
      w = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos ] ) );
      fprintf( cargo->yyc, "\t%i, %i,", pFunc->pCode[ lPCodePos ], pFunc->pCode[ lPCodePos + 1 ] );

      /* NOTE:
         * When a codeblock is used to initialize a static variable
         * the names of local variables cannot be determined
         * because at the time of C code generation we don't know
         * in which function was defined this local variable
         */
      if( ( pFunc->cScope & HB_FS_INITEXIT ) != HB_FS_INITEXIT )
      {
         if( cargo->bVerbose )
         {
            fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, w )->szName );
         }
      }

      fprintf( cargo->yyc, "\n" );
      lPCodePos += 2;
   }

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_pushblockshort )
{
   ++cargo->iNestedCodeblock;

   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCKSHORT, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */",
               pFunc->pCode[ lPCodePos + 1 ] );
   }
   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushdouble )
{
   int i;

   fprintf( cargo->yyc, "\tHB_P_PUSHDOUBLE," );
   ++lPCodePos;
   for( i = 0; i < ( int ) ( sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) ); ++i )
   {
      fprintf( cargo->yyc, " %i,", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );
   }
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %.*f, %d, %d */",
      *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) + sizeof( BYTE ) ] ) ),
      HB_PCODE_MKDOUBLE( &( pFunc->pCode[ lPCodePos ] ) ),
      *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
      *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) + sizeof( BYTE ) ] ) ) );
   }
   fprintf( cargo->yyc, "\n" );

   return sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) + 1;
}

static HB_GENC_FUNC( hb_p_pushfield )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushbyte )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHBYTE, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */",
               ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushint )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHINT, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocal )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */
      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocalnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEAR, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int )(signed char) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_localnearinc )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARINC, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int )(signed char) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_localneardec )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARDEC, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int )(signed char) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalnearinc )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEARINC, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int )(signed char) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalneardec )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEARDEC, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int )(signed char) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      SHORT iVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */
      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushlong )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLONG, %i, %i, %i, %i, ",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %li */", HB_PCODE_MKLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_pushlonglong )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLONGLONG, %i, %i, %i, %i, %i, %i, %i, %i, ",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ],
            pFunc->pCode[ lPCodePos + 5 ],
            pFunc->pCode[ lPCodePos + 6 ],
            pFunc->pCode[ lPCodePos + 7 ],
            pFunc->pCode[ lPCodePos + 8 ] );
   if( cargo->bVerbose )
   {
#ifdef HB_LONG_LONG_OFF
      fprintf( cargo->yyc, "\t/* %f */", HB_PCODE_MKLONGLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
#else
      fprintf( cargo->yyc, "\t/* %" PFLL "i */", HB_PCODE_MKLONGLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
#endif
   }
   fprintf( cargo->yyc, "\n" );

   return 9;
}

static HB_GENC_FUNC( hb_p_pushdatetime )
{
   int i;

   fprintf( cargo->yyc, "\tHB_P_PUSHDATETIME, " );

   for( i = 0; i < ( int ) ( sizeof( UINT32 ) + sizeof( UINT32 ) ); ++i )
   {
      fprintf( cargo->yyc, " %i,", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + 1 + i ] );
   }

   if( cargo->bVerbose )
   {
      char szDateTime[24];
      fprintf( cargo->yyc, "\t/* %s */",
          hb_datetimeDecStr( szDateTime, HB_PCODE_MKLONG( ( pFunc->pCode + lPCodePos + 1 ) ), HB_PCODE_MKLONG( ( pFunc->pCode + lPCodePos + 5 ) ) ) );
//      printf("szDateTime=%s lDate=%d lTime=%d\n", szDateTime, HB_PCODE_MKLONG( ( pFunc->pCode + lPCodePos + 1 ) ), HB_PCODE_MKLONG( ( pFunc->pCode + lPCodePos + 5 ) ) );
   }

   fprintf( cargo->yyc, "\t/* HB_ET_DDATETIME */\n" );

   return sizeof(double) + 1;
}

static HB_GENC_FUNC( hb_p_pushdate )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHDATE, " );

   fprintf( cargo->yyc, "%i, %i, %i, %i, ",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   fprintf( cargo->yyc, "\t/* HB_ET_DDATE */\n" );
   return 5;
}

static HB_GENC_FUNC( hb_p_pushmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushmemvarref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVARREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushnil )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHNIL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushself )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHSELF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushstatic )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATIC, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      PVAR pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;
      USHORT wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
      {
         pTmp = pTmp->pNext;
      }

      pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushstaticref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATICREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      PVAR pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;
      USHORT wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
      {
         pTmp = pTmp->pNext;
      }

      pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushstr )
{
   ULONG ulStart = lPCodePos;
   USHORT wLen = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

   fprintf( cargo->yyc, "\tHB_P_PUSHSTR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", wLen );
   }

   lPCodePos += 3;
   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      while( wLen-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else if( strchr( "\'\\\"", uchr ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else
         {
            fprintf( cargo->yyc, "\'%c\', ", uchr );
         }
      }
   }
   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_pushstrshort )
{
   ULONG ulStart = lPCodePos;
   USHORT wLen = pFunc->pCode[ lPCodePos + 1 ];

   fprintf( cargo->yyc, "\tHB_P_PUSHSTRSHORT, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", wLen );
   }

   lPCodePos += 2;
   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      while( wLen-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
            * NOTE: After optimization some CHR(n) can be converted
            *    into a string containing nonprintable characters.
            *
            * TODO: add switch to use hexadecimal format "%#04x"
            */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else if( strchr( "\'\\\"", uchr ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else
         {
            fprintf( cargo->yyc, "\'%c\', ", uchr );
         }
      }
   }
   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_pushsym )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYM, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushsymnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYMNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushvariable )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHVARIABLE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_retvalue )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_RETVALUE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_send )
{
   fprintf( cargo->yyc, "\tHB_P_SEND, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_sendshort )
{
   fprintf( cargo->yyc, "\tHB_P_SENDSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_seqbegin )
{
   fprintf( cargo->yyc, "\tHB_P_SEQBEGIN, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, lPCodePos + lOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_seqend )
{
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "/* %05li */ ", lPCodePos );
   }
   else
   {
      fprintf( cargo->yyc, "\t" );
   }
   fprintf( cargo->yyc, "HB_P_SEQEND, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, lPCodePos + lOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_seqrecover )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SEQRECOVER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_sframe )
{
   fprintf( cargo->yyc, "\tHB_P_SFRAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS) */" );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_statics )
{
   LONG lByteCount = 5;

   fprintf( cargo->yyc, "\tHB_P_STATICS, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS), %i statics */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 3 ] ) ) );
   }
   fprintf( cargo->yyc, "\n" );
   return lByteCount;
}

static HB_GENC_FUNC( hb_p_staticname )
{
   ULONG ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_STATICNAME, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 4 );
   }
   fprintf( cargo->yyc, "\n" );
   lPCodePos += 4;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];
      if( chr == '\'' || chr == '\\')
      {
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      }
      else
      {
         fprintf( cargo->yyc, " \'%c\',", chr );
      }
   }
   fprintf( cargo->yyc, " 0,\n" );

   return lPCodePos - ulStart + 1;
}

static HB_GENC_FUNC( hb_p_swapalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SWAPALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_true )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_TRUE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_one )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ONE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_zero )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ZERO,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_noop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dummy )
{
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrolist )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROLIST,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrolistend )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROLISTEND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_localnearaddint )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARADDINT, %i, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int) (signed char) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, iVar )->szName, HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) ) );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_localnearsetint )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARSETINT, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int) (signed char) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, iVar )->szName, HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) ) );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_addint )
{
   fprintf( cargo->yyc, "\tHB_P_ADDINT, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i*/", HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_localnearsetstr )
{

   ULONG ulStart = lPCodePos;
   USHORT uLen   = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) );

   fprintf( cargo->yyc, "\tHB_P_LOCALNEARSETSTR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int) (signed char) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, iVar )->szName, uLen );
      }
   }

   lPCodePos += 4;

   if( uLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );

      while( uLen-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else if( strchr( "\'\\\"", uchr ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else
         {
            fprintf( cargo->yyc, "\'%c\', ", uchr );
         }
      }
   }

   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_left )
{
   fprintf( cargo->yyc, "\tHB_P_LEFT, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_right )
{
   fprintf( cargo->yyc, "\tHB_P_RIGHT, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_substr )
{
   fprintf( cargo->yyc, "\tHB_P_SUBSTR, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ], pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i %i*/",
               HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ),
               HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 3 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_lineoffset )
{
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "/* %05li */ ", lPCodePos );
   }
   else
   {
      fprintf( cargo->yyc, "\t" );
   }

   fprintf( cargo->yyc, "HB_P_LINEOFFSET, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i */", hb_comp_iBaseLine + pFunc->pCode[ lPCodePos + 1 ] );
   }
   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_baseline )
{
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "/* %05li */ ", lPCodePos );
   }
   else
   {
      fprintf( cargo->yyc, "\t" );
   }

   fprintf( cargo->yyc, "HB_P_BASELINE, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      hb_comp_iBaseLine = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %i */", hb_comp_iBaseLine );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_withobject )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_WITHOBJECT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_sendwith )
{
   fprintf( cargo->yyc, "\tHB_P_SENDWITH, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_sendwithshort )
{
   fprintf( cargo->yyc, "\tHB_P_SENDWITHSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_endwithobject )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENDWITHOBJECT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_foreach )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FOREACH,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumerate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENUMERATE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endenumerate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENDENUMERATE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumindex )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENUMINDEX,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushglobal )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHGLOBAL, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compVariableFind( hb_comp_pGlobals, (USHORT) pFunc->pCode[ lPCodePos + 1 ] + 1 )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_popglobal )
{
   fprintf( cargo->yyc, "\tHB_P_POPGLOBAL, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compVariableFind( hb_comp_pGlobals, (USHORT) pFunc->pCode[ lPCodePos + 1 ] + 1 )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushglobalref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHGLOBALREF, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s */", hb_compVariableFind( hb_comp_pGlobals, (USHORT) pFunc->pCode[ lPCodePos + 1 ] + 1 )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_switchcase )
{
   fprintf( cargo->yyc, "\tHB_P_SWITCHCASE, %i, %i, %i, %i, ",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %li */", HB_PCODE_MKLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_like )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LIKE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_match )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MATCH,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushmacroref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHMACROREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_ivarref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_IVARREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_classsetmodule )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_CLASSSETMODULE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitand )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITAND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitor )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITOR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitxor )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITXOR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitshiftr )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITSHIFTR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitshiftl )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITSHIFTL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_largeframe )
{
   fprintf( cargo->yyc, "\tHB_P_LARGEFRAME, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* (lo)locals, (hi)locals, params */" );
   }

   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_pushwith )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHWITH,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushstrhidden )
{
   ULONG ulStart = lPCodePos;
   USHORT wLen = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
   BYTE bType = pFunc->pCode[ lPCodePos + 3 ];
   USHORT wLenBuffer = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 4 ] ) );

   fprintf( cargo->yyc, "\tHB_P_PUSHSTRHIDDEN, %i, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],     // LO: String length
            pFunc->pCode[ lPCodePos + 2 ],     // HI: String length
            pFunc->pCode[ lPCodePos + 3 ],     // Hide type
            pFunc->pCode[ lPCodePos + 4 ],     // LO: Buffer length
            pFunc->pCode[ lPCodePos + 5 ] );   // HI: Buffer length

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i, %i, %i */", wLen, bType, wLenBuffer );
   }

   lPCodePos += 6;
   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      while( wLenBuffer-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else if( strchr( "\'\\\"", uchr ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else
         {
            fprintf( cargo->yyc, "\'%c\', ", uchr );
         }
      }
   }
   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_localnearsetstrhidden )
{
   ULONG ulStart = lPCodePos;
   USHORT uLen   = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) );
   BYTE bType = pFunc->pCode[ lPCodePos + 4 ];
   USHORT wLenBuffer = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 5 ] ) );

   fprintf( cargo->yyc, "\tHB_P_LOCALNEARSETSTRHIDDEN, %i, %i, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],     // LO: String length
            pFunc->pCode[ lPCodePos + 3 ],     // HI: String length
            pFunc->pCode[ lPCodePos + 4 ],     // Hide type
            pFunc->pCode[ lPCodePos + 5 ],     // LO: Buffer length
            pFunc->pCode[ lPCodePos + 6 ] );   // HI: Buffer length

   if( cargo->bVerbose )
   {
      int iVar = (int) (signed char) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i, %i, %i */", -iVar, bType, wLenBuffer );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i, %i, %i */", iVar, bType, wLenBuffer );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s %i, %i, %i*/", hb_compLocalVariableFind( pFunc, iVar )->szName, uLen, bType, wLenBuffer );
      }
   }

   lPCodePos += 7;

   if( uLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );

      while( wLenBuffer-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else if( strchr( "\'\\\"", uchr ) )
         {
            fprintf( cargo->yyc, "%i, ", uchr );
         }
         else
         {
            fprintf( cargo->yyc, "\'%c\', ", uchr );
         }
      }
   }

   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_trybegin )
{
   fprintf( cargo->yyc, "\tHB_P_TRYBEGIN, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, lPCodePos + lOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_tryend )
{
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "/* %05li */ ", lPCodePos );
   }
   else
   {
      fprintf( cargo->yyc, "\t" );
   }
   fprintf( cargo->yyc, "HB_P_TRYEND, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, lPCodePos + lOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_tryrecover )
{
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "/* %05li */ ", lPCodePos );
   }
   else
   {
      fprintf( cargo->yyc, "\t" );
   }
   fprintf( cargo->yyc, "HB_P_TRYRECOVER, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, lPCodePos + lOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_finally )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FINALLY,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endfinally )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENDFINALLY,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_localnearadd )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARADD, %i,",
                        pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = (int) (signed char) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_arraypushref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPUSHREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypopplus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPOPPLUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_divert )
{
   if( (lPCodePos+1) == pFunc->lPCodePos )
   {
      fprintf( cargo->yyc, "\tHB_P_DIVERT\n" );
   }
   else
   {
      fprintf( cargo->yyc, "\tHB_P_DIVERT,\n" );
   }
   return 1;
}

static HB_GENC_FUNC( hb_p_divertof )
{
   if( (lPCodePos+1) == pFunc->lPCodePos )
   {
      fprintf( cargo->yyc, "\tHB_P_DIVERTOF\n" );
   }
   else
   {
      fprintf( cargo->yyc, "\tHB_P_DIVERTOF,\n" );
   }
   return 1;
}

/* NOTE: The order of functions has to match the order of opcodes
 *       mnemonics
 */
static HB_GENC_FUNC_PTR s_verbose_table[] = {
   hb_p_and,                                         /* HB_P_AND,                  */
   hb_p_arraypush,                                   /* HB_P_ARRAYPUSH,            */
   hb_p_arraypop,                                    /* HB_P_ARRAYPOP,             */
   hb_p_arraydim,                                    /* HB_P_ARRAYDIM,             */
   hb_p_arraygen,                                    /* HB_P_ARRAYGEN,             */
   hb_p_equal,                                       /* HB_P_EQUAL,                */
   hb_p_endblock,                                    /* HB_P_ENDBLOCK,             */
   hb_p_endproc,                                     /* HB_P_ENDPROC,              */
   hb_p_exactlyequal,                                /* HB_P_EXACTLYEQUAL,         */
   hb_p_false,                                       /* HB_P_FALSE,                */
   hb_p_fortest,                                     /* HB_P_FORTEST,              */
   hb_p_function,                                    /* HB_P_FUNCTION,             */
   hb_p_functionshort,                               /* HB_P_FUNCTIONSHORT,        */
   hb_p_frame,                                       /* HB_P_FRAME,                */
   hb_p_funcptr,                                     /* HB_P_FUNCPTR,              */
   hb_p_greater,                                     /* HB_P_GREATER,              */
   hb_p_greaterequal,                                /* HB_P_GREATEREQUAL,         */
   hb_p_dec,                                         /* HB_P_DEC,                  */
   hb_p_divide,                                      /* HB_P_DIVIDE,               */
   hb_p_do,                                          /* HB_P_DO,                   */
   hb_p_doshort,                                     /* HB_P_DOSHORT,              */
   hb_p_duplicate,                                   /* HB_P_DUPLICATE,            */
   hb_p_dupltwo,                                     /* HB_P_DUPLTWO,              */
   hb_p_inc,                                         /* HB_P_INC,                  */
   hb_p_instring,                                    /* HB_P_INSTRING,             */
   hb_p_jumpnear,                                    /* HB_P_JUMPNEAR,             */
   hb_p_jump,                                        /* HB_P_JUMP,                 */
   hb_p_jumpfar,                                     /* HB_P_JUMPFAR,              */
   hb_p_jumpfalsenear,                               /* HB_P_JUMPFALSENEAR,        */
   hb_p_jumpfalse,                                   /* HB_P_JUMPFALSE,            */
   hb_p_jumpfalsefar,                                /* HB_P_JUMPFALSEFAR,         */
   hb_p_jumptruenear,                                /* HB_P_JUMPTRUENEAR,         */
   hb_p_jumptrue,                                    /* HB_P_JUMPTRUE,             */
   hb_p_jumptruefar,                                 /* HB_P_JUMPTRUEFAR,          */
   hb_p_lessequal,                                   /* HB_P_LESSEQUAL,            */
   hb_p_less,                                        /* HB_P_LESS,                 */
   hb_p_line,                                        /* HB_P_LINE,                 */
   hb_p_localname,                                   /* HB_P_LOCALNAME,            */
   hb_p_macropop,                                    /* HB_P_MACROPOP,             */
   hb_p_macropopaliased,                             /* HB_P_MACROPOPALIASED,      */
   hb_p_macropush,                                   /* HB_P_MACROPUSH,            */
   hb_p_macropusharg,                                /* HB_P_MACROPUSHARG,         */
   hb_p_macropushlist,                               /* HB_P_MACROPUSHLIST,        */
   hb_p_macropushindex,                              /* HB_P_MACROPUSHINDEX,       */
   hb_p_macropushpare,                               /* HB_P_MACROPUSHPARE,        */
   hb_p_macropushaliased,                            /* HB_P_MACROPUSHALIASED,     */
   hb_p_macrosymbol,                                 /* HB_P_MACROSYMBOL,          */
   hb_p_macrotext,                                   /* HB_P_MACROTEXT,            */
   hb_p_message,                                     /* HB_P_MESSAGE,              */
   hb_p_minus,                                       /* HB_P_MINUS,                */
   hb_p_modulus,                                     /* HB_P_MODULUS,              */
   hb_p_modulename,                                  /* HB_P_MODULENAME,           */
   /* start: pcodes generated by macro compiler */
   hb_p_dummy,                                       /* HB_P_MMESSAGE,             */
   hb_p_dummy,                                       /* HB_P_MPOPALIASEDFIELD,     */
   hb_p_dummy,                                       /* HB_P_MPOPALIASEDVAR,       */
   hb_p_dummy,                                       /* HB_P_MPOPFIELD,            */
   hb_p_dummy,                                       /* HB_P_MPOPMEMVAR,           */
   hb_p_dummy,                                       /* HB_P_MPUSHALIASEDFIELD,    */
   hb_p_dummy,                                       /* HB_P_MPUSHALIASEDVAR,      */
   hb_p_dummy,                                       /* HB_P_MPUSHBLOCK,           */
   hb_p_dummy,                                       /* HB_P_MPUSHFIELD,           */
   hb_p_dummy,                                       /* HB_P_MPUSHMEMVAR,          */
   hb_p_dummy,                                       /* HB_P_MPUSHMEMVARREF,       */
   hb_p_dummy,                                       /* HB_P_MPUSHSYM,             */
   hb_p_dummy,                                       /* HB_P_MPUSHVARIABLE,        */
   /* end: */
   hb_p_mult,                                        /* HB_P_MULT,                 */
   hb_p_negate,                                      /* HB_P_NEGATE,               */
   hb_p_noop,                                        /* HB_P_NOOP,                 */
   hb_p_not,                                         /* HB_P_NOT,                  */
   hb_p_notequal,                                    /* HB_P_NOTEQUAL,             */
   hb_p_or,                                          /* HB_P_OR,                   */
   hb_p_parameter,                                   /* HB_P_PARAMETER,            */
   hb_p_plus,                                        /* HB_P_PLUS,                 */
   hb_p_pop,                                         /* HB_P_POP,                  */
   hb_p_popalias,                                    /* HB_P_POPALIAS,             */
   hb_p_popaliasedfield,                             /* HB_P_POPALIASEDFIELD,      */
   hb_p_popaliasedfieldnear,                         /* HB_P_POPALIASEDFIELDNEAR,  */
   hb_p_popaliasedvar,                               /* HB_P_POPALIASEDVAR,        */
   hb_p_popfield,                                    /* HB_P_POPFIELD,             */
   hb_p_poplocal,                                    /* HB_P_POPLOCAL,             */
   hb_p_poplocalnear,                                /* HB_P_POPLOCALNEAR,         */
   hb_p_popmemvar,                                   /* HB_P_POPMEMVAR,            */
   hb_p_popstatic,                                   /* HB_P_POPSTATIC,            */
   hb_p_popvariable,                                 /* HB_P_POPVARIABLE,          */
   hb_p_power,                                       /* HB_P_POWER,                */
   hb_p_pushalias,                                   /* HB_P_PUSHALIAS,            */
   hb_p_pushaliasedfield,                            /* HB_P_PUSHALIASEDFIELD,     */
   hb_p_pushaliasedfieldnear,                        /* HB_P_PUSHALIASEDFIELDNEAR, */
   hb_p_pushaliasedvar,                              /* HB_P_PUSHALIASEDVAR,       */
   hb_p_pushblock,                                   /* HB_P_PUSHBLOCK,            */
   hb_p_pushblockshort,                              /* HB_P_PUSHBLOCKSHORT,       */
   hb_p_pushfield,                                   /* HB_P_PUSHFIELD,            */
   hb_p_pushbyte,                                    /* HB_P_PUSHBYTE,             */
   hb_p_pushint,                                     /* HB_P_PUSHINT,              */
   hb_p_pushlocal,                                   /* HB_P_PUSHLOCAL,            */
   hb_p_pushlocalnear,                               /* HB_P_PUSHLOCALNEAR,        */
   hb_p_pushlocalref,                                /* HB_P_PUSHLOCALREF,         */
   hb_p_pushlong,                                    /* HB_P_PUSHLONG,             */
   hb_p_pushmemvar,                                  /* HB_P_PUSHMEMVAR,           */
   hb_p_pushmemvarref,                               /* HB_P_PUSHMEMVARREF,        */
   hb_p_pushnil,                                     /* HB_P_PUSHNIL,              */
   hb_p_pushdouble,                                  /* HB_P_PUSHDOUBLE,           */
   hb_p_pushself,                                    /* HB_P_PUSHSELF,             */
   hb_p_pushstatic,                                  /* HB_P_PUSHSTATIC,           */
   hb_p_pushstaticref,                               /* HB_P_PUSHSTATICREF,        */
   hb_p_pushstr,                                     /* HB_P_PUSHSTR,              */
   hb_p_pushstrshort,                                /* HB_P_PUSHSTRSHORT,         */
   hb_p_pushsym,                                     /* HB_P_PUSHSYM,              */
   hb_p_pushsymnear,                                 /* HB_P_PUSHSYMNEAR,          */
   hb_p_pushvariable,                                /* HB_P_PUSHVARIABLE,         */
   hb_p_retvalue,                                    /* HB_P_RETVALUE,             */
   hb_p_send,                                        /* HB_P_SEND,                 */
   hb_p_sendshort,                                   /* HB_P_SENDSHORT,            */
   hb_p_seqbegin,                                    /* HB_P_SEQBEGIN,             */
   hb_p_seqend,                                      /* HB_P_SEQEND,               */
   hb_p_seqrecover,                                  /* HB_P_SEQRECOVER,           */
   hb_p_sframe,                                      /* HB_P_SFRAME,               */
   hb_p_statics,                                     /* HB_P_STATICS,              */
   hb_p_staticname,                                  /* HB_P_STATICNAME,           */
   hb_p_swapalias,                                   /* HB_P_SWAPALIAS,            */
   hb_p_true,                                        /* HB_P_TRUE,                 */
   hb_p_zero,                                        /* HB_P_ZERO,                 */
   hb_p_one,                                         /* HB_P_ONE,                  */
   hb_p_macrolist,                                   /* HB_P_MACROLIST,            */
   hb_p_macrolistend,                                /* HB_P_MACROLISTEND,         */
   hb_p_localnearaddint,                             /* HB_P_LOCALNEARADDINT,      */
   hb_p_localnearsetint,                             /* HB_P_LOCALNEARSETINT,      */
   hb_p_localnearsetstr,                             /* HB_P_LOCALNEARSETSTR,      */
   hb_p_addint,                                      /* HB_P_ADDINT,               */
   hb_p_left,                                        /* HB_P_LEFT,                 */
   hb_p_right,                                       /* HB_P_RIGHT,                */
   hb_p_substr,                                      /* HB_P_SUBSTR,               */
   hb_p_dummy,                                       /* HB_P_MPUSHSTR,             */
   hb_p_baseline,                                    /* HB_P_BASELINE,             */
   hb_p_lineoffset,                                  /* HB_P_LINEOFFSET,           */
   hb_p_withobject,                                  /* HB_P_WITHOBJECT,           */
   hb_p_sendwith,                                    /* HB_P_SENDWITH,             */
   hb_p_sendwithshort,                               /* HB_P_SENDWITHSHORT,        */
   hb_p_endwithobject,                               /* HB_P_ENDWITHOBJECT,        */
   hb_p_foreach,                                     /* HB_P_FOREACH,              */
   hb_p_enumerate,                                   /* HB_P_ENUMERATE,            */
   hb_p_endenumerate,                                /* HB_P_ENDENUMERATE,         */
   hb_p_pushglobal,                                  /* HB_P_PUSHGLOBAL,           */
   hb_p_popglobal,                                   /* HB_P_POPGLOBAL,            */
   hb_p_pushglobalref,                               /* HB_P_PUSHGLOBALREF,        */
   hb_p_enumindex,                                   /* HB_P_ENUMINDEX,            */
   hb_p_switchcase,                                  /* HB_P_SWITCHCASE,           */
   hb_p_like,                                        /* HB_P_LIKE,                 */
   hb_p_match,                                       /* HB_P_MATCH,                */
   hb_p_pushmacroref,                                /* HB_P_PUSHMACROREF,         */
   hb_p_ivarref,                                     /* HB_P_IVARREF,              */
   hb_p_classsetmodule,                              /* HB_P_CLASSSETMODULE,       */
   hb_p_bitand,                                      /* HB_P_BITAND,               */
   hb_p_bitor,                                       /* HB_P_BITOR,                */
   hb_p_bitxor,                                      /* HB_P_BITXOR,               */
   hb_p_bitshiftr,                                   /* HB_P_SHIFTR,               */
   hb_p_bitshiftl,                                   /* HB_P_SHIFTL,               */
   hb_p_largeframe,                                  /* HB_P_LARGEFRAME,           */
   hb_p_pushwith,                                    /* HB_P_PUSHWITH,             */
   hb_p_pushlonglong,                                /* HB_P_PUSHLONGLONG,         */
   hb_p_pushstrhidden,                               /* HB_P_PUSHSTRHIDDEN,        */
   hb_p_localnearsetstrhidden,                       /* HB_P_LOCALNEARSETSTRHIDDEN,*/
   hb_p_trybegin,                                    /* HB_P_TRYBEGIN,             */
   hb_p_tryend,                                      /* HB_P_TRYEND,               */
   hb_p_tryrecover,                                  /* HB_P_TRYRECOVER,           */
   hb_p_finally,                                     /* HB_P_FINALLY,              */
   hb_p_endfinally,                                  /* HB_P_ENDFINALLY,           */
   hb_p_localnearadd,                                /* HB_P_LOCALNEARADD          */
   hb_p_arraypushref,                                /* HB_P_ARRAYPUSHREF          */
   hb_p_arraypopplus,                                /* HB_P_ARRAYPOPPLUS          */
   hb_p_pushdatetime,                                /* HB_P_PUSHDATETIME          */
   hb_p_pushdate,                                    /* HB_P_PUSHDATE              */
   hb_p_hashgen,                                     /* HB_P_HASHGEN               */
   hb_p_localnearinc,                                /* HB_P_LOCALNEARINC,         */
   hb_p_localneardec,                                /* HB_P_LOCALNEARDEC,         */
   hb_p_pushlocalnearinc,                            /* HB_P_PUSHLOCALNEARINC,     */
   hb_p_pushlocalneardec,                            /* HB_P_PUSHLOCALNEARDEC,     */
   hb_p_divert,                                      /* HB_P_DIVERT                */
   hb_p_divertof                                     /* HB_P_DIVERTOF              */
};

static void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc )
{
   HB_GENC_INFO genc_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( HB_GENC_FUNC_PTR ) );

   genc_info.iNestedCodeblock = 0;
   genc_info.bVerbose = ( hb_comp_iGenCOutput == HB_COMPGENC_VERBOSE );
   genc_info.yyc = yyc;

   fprintf( yyc, "{\n   static const BYTE pcode[] =\n   {\n" );

   hb_compPCodeEval( pFunc, ( HB_PCODE_FUNC_PTR * ) s_verbose_table, ( void * ) &genc_info );
   if( genc_info.bVerbose )
   {
      fprintf( yyc, "/* %05li */\n", pFunc->lPCodePos );
   }

   fprintf( yyc, "   };\n\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
   {
      fprintf( yyc, "   HB_CRITICAL_LOCK( s_Critical%s );\n", pFunc->szName );
   }

   fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
   {
      fprintf( yyc, "   HB_CRITICAL_UNLOCK( s_Critical%s );\n", pFunc->szName );
   }

   fprintf( yyc,  "}\n" );
}

static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc )
{
   ULONG lPCodePos = 0;
   int nChar;

   fprintf( yyc, "\n{\n   static const BYTE pcode[] =\n   {\n" );

   nChar = 0;

   while( lPCodePos < pFunc->lPCodePos )
   {
      ++nChar;

      if( nChar > 1 )
      {
         fprintf( yyc, ", " );
      }

      if( nChar == 15 )
      {
         fprintf( yyc, "\n\t" );
         nChar = 1;
      }

      /* Displaying as decimal is more compact than hex */
      fprintf( yyc, "%d", ( int ) pFunc->pCode[ lPCodePos++ ] );

   }

   if( nChar != 0)
   {
      fprintf( yyc, "\n" );
   }

   fprintf( yyc, "   };\n\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
   {
      fprintf( yyc, "   HB_CRITICAL_LOCK( s_Critical%s );\n", pFunc->szName );
   }

   fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
   {
      fprintf( yyc, "   HB_CRITICAL_UNLOCK( s_Critical%s );\n", pFunc->szName );
   }

   fprintf( yyc,  "}\n" );
}
