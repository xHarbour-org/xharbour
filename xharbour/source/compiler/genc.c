/*
 * $Id: genc.c,v 1.73 2004/04/21 01:29:29 andijahja Exp $
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

#include "hbcomp.h"

static int hb_comp_iBaseLine;

static void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc );
static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc );
static void hb_compGenCInLine( FILE* ) ;

// AJ: 2004-02-05
// Routines to check C-In-Line static function declared in a PRG file
// with #PRAGMA BEGINDUMP - ENDDUMP
static void hb_compGenCInLineSymbol( void );
static void hb_compGenCCheckInLineStatic( char * str );
static BOOL hb_compCStaticSymbolFound( char* szSymbol, BOOL bSearchStatic );
/* struct to hold symbol names of c-in-line static functions */
typedef struct _SSYMLIST
{
   char *    szName;
   struct _SSYMLIST * pNext;
} SSYMLIST, * PSSYMLIST;

static PSSYMLIST pStatSymFirst = NULL;
static PSSYMLIST pPubSymFirst = NULL;

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

/*
 AJ: 2003-06-25
 Extended to generate pCode listing
 hb_comp_iGenVarList is first initialized in harbour.c as FALSE
 The value is TRUE when /gc3 is used
*/
extern BOOL hb_comp_iGenVarList;
extern char *hb_comp_FileAsSymbol;
extern char *hb_comp_PrgFileName;

/*
 hb_comp_pCodeList is the file handle on which pCode Listing will be written
*/
FILE *hb_comp_pCodeList = NULL;

void hb_compGenCCode( PHB_FNAME pFileName, char *szSourceExtension )      /* generates the C language output */
{
   char szFileName[ _POSIX_PATH_MAX ];
   char szpCodeFileName[ _POSIX_PATH_MAX ] ;
   char szSourceName[ _POSIX_PATH_MAX ], *pTmp;
   PFUNCTION pFunc = hb_comp_functions.pFirst;
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   PCOMDECLARED pDeclared;
   PCOMCLASS    pClass;
   FILE * yyc; /* file handle for C output */
   PINLINE pInline = hb_comp_inlines.pFirst;
   PVAR pGlobal, pDelete;
   short iLocalGlobals = 0, iGlobals = 0;

   BOOL bIsPublicFunction ;
   BOOL bIsInitFunction   ;
   BOOL bIsExitFunction   ;
   BOOL bIsStaticVariable ;
   BOOL bIsGlobalVariable ;

   BOOL bCritical = FALSE;

   int  iSymOffset, iStartupOffset;

   PSSYMLIST pStatSymTemp;

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

   hb_strupr( pFileName->szName );

   yyc = fopen( szFileName, "wb" );

   if( ! yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   /*
    Create *.p when /gc3 is used
   */
   if ( hb_comp_iGenVarList )
   {
      pFileName->szExtension = ".p";
      hb_fsFNameMerge( szpCodeFileName, pFileName );
      hb_comp_pCodeList = fopen( szpCodeFileName,"wb" );

      if( !hb_comp_pCodeList )
      {
         hb_comp_iGenVarList = FALSE;
      }
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

   fprintf( yyc, "/*\n * xHarbour Compiler, build %d.%d (%s)\n", HB_VER_MINOR, HB_VER_REVISION, HB_VER_LEX );
   /* AJ: Some compilers performs [f]printf("<%s>",string) incorrecltly */
   fprintf( yyc, " * Generated C source code from %s%s%s\n */\n\n", "<", hb_comp_PrgFileName, ">" );

   if( hb_comp_iFunctionCnt )
   {
      fprintf( yyc, "#include \"hbvmpub.h\"\n" );

      if( hb_comp_iGenCOutput != HB_COMPGENC_COMPACT )
      {
         fprintf( yyc, "#include \"hbpcode.h\"\n" );
      }

      fprintf( yyc, "#include \"hbinit.h\"\n\n" );

      fprintf( yyc, "#define __PRG_SOURCE__ \"%s\"\n\n", hb_comp_PrgFileName );

      if( ! hb_comp_bStartProc )
      {
         pFunc = pFunc->pNext; /* No implicit starting procedure */
      }

      /* write functions prototypes for PRG defined functions */
      while( pFunc )
      {
         bIsInitFunction   = ( pFunc->cScope & HB_FS_INIT ) ;
         bIsExitFunction   = ( pFunc->cScope & HB_FS_EXIT ) ;
         bIsStaticVariable = ( pFunc == hb_comp_pInitFunc ) ;
         bIsGlobalVariable = ( pFunc == hb_comp_pGlobalsFunc ) ;
         bIsPublicFunction = ( pFunc->cScope == HB_FS_PUBLIC ) ;

         if( pFunc->cScope & HB_FS_CRITICAL )
         {
            bCritical = TRUE;
         }

         /* Is it a PUBLIC FUNCTION/PROCEDURE */
         if ( bIsPublicFunction )
         {
            fprintf( yyc, "HB_FUNC( %s );\n", pFunc->szName );
         }
         /* Is it a STATIC$ */
         else if ( bIsStaticVariable )
         {
            fprintf( yyc, "\nstatic HARBOUR hb_INITSTATICS( void );\n\n" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it a GLOBAL$ */
         else if ( bIsGlobalVariable )
         {
            fprintf( yyc, "static HARBOUR hb_INITGLOBALS( void );\n" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it an INIT FUNCTION/PROCEDURE */
         else if ( bIsInitFunction )
         {
            pFunc->szName[ strlen( pFunc->szName ) - 1 ] = '_';
            fprintf( yyc, "HB_FUNC_INIT( _%s );\n", pFunc->szName );
         }
         /* Is it an EXIT FUNCTION/PROCEDURE */
         else if ( bIsExitFunction )
         {
            pFunc->szName[ strlen( pFunc->szName ) - 1 ] = '_';
            fprintf( yyc, "HB_FUNC_EXIT( _%s );\n", pFunc->szName );
         }
         /* Then it must be a STATIC FUNCTION/PROCEDURE */
         else
         {
            fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pFunc->szName );
         }

         pFunc = pFunc->pNext;
      }

      // Any NON EXTERN Globals to Register?
      pGlobal = hb_comp_pGlobals;
      while( pGlobal )
      {
         iGlobals++;

         if( pGlobal->szAlias == NULL )
         {
            iLocalGlobals++;
         }

         pGlobal = pGlobal->pNext;
      }

      if( iLocalGlobals )
      {
         fprintf( yyc, "static HARBOUR hb_REGISTERGLOBALS( void );\n\n" ); /* NOTE: hb_ intentionally in lower case */
      }

      /* write functions prototypes for inline blocks */
      while( pInline )
      {
         if( pInline->szName )
         {
            fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pInline->szName );
         }
         pInline = pInline->pNext;
      }

      /* check c-in-line static functions */
      pInline = hb_comp_inlines.pFirst;
      if ( pInline )
      {
         hb_compGenCInLineSymbol();
      }

      /* write functions prototypes for called functions outside this PRG */
      pFunc = hb_comp_funcalls.pFirst;
      while( pFunc )
      {
         if( hb_compFunctionFind( pFunc->szName ) == NULL && hb_compInlineFind( pFunc->szName ) == NULL )
         {
            if( hb_compCStaticSymbolFound( pFunc->szName, TRUE ) )
            {
               fprintf( yyc, "HB_FUNC_STATIC( %s );\n", pFunc->szName );
            }
            else
            {
               if( hb_compCStaticSymbolFound( pFunc->szName, FALSE ) )
               {
                  fprintf( yyc, "HB_FUNC( %s );\n", pFunc->szName );
               }
               else
               {
                  fprintf( yyc, "HB_FUNC_EXTERN( %s );\n", pFunc->szName );
               }
            }
         }

         pFunc = pFunc->pNext;
      }

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
         fprintf( yyc, "\nHB_CALL_ON_STARTUP_BEGIN( hb_InitCritical%s )\n", pFileName->szName );

         pFunc = hb_comp_functions.pFirst;
         while( pFunc )
         {
            if( pFunc->cScope & HB_FS_CRITICAL )
            {
               fprintf( yyc, "   HB_CRITICAL_INIT( s_Critical%s );\n", pFunc->szName );
            }

            pFunc = pFunc->pNext;
         }

         fprintf( yyc, "HB_CALL_ON_STARTUP_END( hb_InitCritical%s )\n", pFileName->szName );
      }

      /* writes the symbol table */
      /* Generate the wrapper that will initialize local symbol table
       */

      fprintf( yyc, "\n#undef HB_PRG_PCODE_VER\n" );
      fprintf( yyc, "#define HB_PRG_PCODE_VER %i\n", (int) HB_PCODE_VER );

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
            fprintf( yyc, "{ \"(_INITSTATICS)\", HB_FS_INIT | HB_FS_EXIT, {hb_INITSTATICS}, NULL }" ); /* NOTE: hb_ intentionally in lower case */
         }
         else if( pSym->szName[ 0 ] == '[' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
            * we are using these two bits to mark the special function used to
            * initialize global variables
            */
            fprintf( yyc, "{ \"(_INITGLOBALS)\", HB_FS_INIT | HB_FS_EXIT, {hb_INITGLOBALS}, NULL }" ); /* NOTE: hb_ intentionally in lower case */
         }
         else if( pSym->szName[ 0 ] == '{' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
            * we are using these two bits to mark the special function used to
            * initialize global variables
            */
            fprintf( yyc, "{ \"hb_REGISTERGLOBALS\", HB_FS_INIT | HB_FS_EXIT, {hb_REGISTERGLOBALS}, NULL }" ); /* NOTE: hb_ intentionally in lower case */
         }
         else
         {
            if( pSym->cScope & HB_FS_INIT || pSym->cScope & HB_FS_EXIT )
            {
               pSym->szName[ strlen( pSym->szName ) - 1 ] = '$';
               fprintf( yyc, "{ \"%s\", ", pSym->szName );
               pSym->szName[ strlen( pSym->szName ) - 1 ] = '_';
            }
            else
            {
               fprintf( yyc, "{ \"%s\", ", pSym->szName );
            }

            if( pSym->cScope & HB_FS_STATIC )
            {
               fprintf( yyc, "HB_FS_STATIC" );

               if( pSym->cScope & HB_FS_PUBLIC )
               {
                  fprintf( yyc, " | HB_FS_PUBLIC" );
               }
            }
            else if( pSym->cScope & HB_FS_INIT )
            {
               fprintf( yyc, "HB_FS_INIT" );

               if( pSym->cScope & HB_FS_PUBLIC )
               {
                  fprintf( yyc, " | HB_FS_PUBLIC" );
               }
            }
            else if( pSym->cScope & HB_FS_EXIT )
            {
               fprintf( yyc, "HB_FS_EXIT" );

               if( pSym->cScope & HB_FS_PUBLIC )
               {
                  fprintf( yyc, " | HB_FS_PUBLIC" );
               }
            }
            else
            {
               if ( hb_compCStaticSymbolFound( pSym->szName, TRUE ) )
               {
                  fprintf( yyc, "HB_FS_STATIC" );

                  if( pSym->cScope & HB_FS_PUBLIC )
                  {
                     fprintf( yyc, " | HB_FS_PUBLIC" );
                  }
               }
               else
               {
                  fprintf( yyc, "HB_FS_PUBLIC" );
               }
            }

            if( pSym->cScope & VS_MEMVAR )
            {
               fprintf( yyc, " | HB_FS_MEMVAR" );
            }

            if( ( pSym->cScope != HB_FS_MESSAGE ) && ( pSym->cScope & HB_FS_MESSAGE ) ) /* only for non public symbols */
            {
               fprintf( yyc, " | HB_FS_MESSAGE" );
            }

            if ( ( pSym->cScope & HB_FS_FIRST ) &&  ( ! hb_comp_bNoStartUp ) )
            {
               fprintf( yyc, " | HB_FS_FIRST" );

               iStartupOffset = iSymOffset;
            }

            /* specify the function address if it is a defined function or an
               external called function */
            if( hb_compFunctionFind( pSym->szName ) ) /* is it a function defined in this module */
            {
               if( pSym->cScope & HB_FS_INIT || pSym->cScope & HB_FS_EXIT )
               {
                  fprintf( yyc, ", {HB_FUNCNAME( _%s )}, (PHB_DYNS) 1 }", pSym->szName );
               }
               else
               {
                  fprintf( yyc, ", {HB_FUNCNAME( %s )}, (PHB_DYNS) 1 }", pSym->szName );
               }
            }
            else if( hb_compFunCallFind( pSym->szName ) ) /* is it a function called from this module */
            {
               fprintf( yyc, ", {HB_FUNCNAME( %s )}, NULL }", pSym->szName );
            }
            else
            {
               fprintf( yyc, ", {NULL}, NULL }" );   /* memvar */
            }
         }

         if( pSym != hb_comp_symbols.pLast )
         {
            fprintf( yyc, ",\n" );
         }

         pSym = pSym->pNext;
         iSymOffset++;
      }

      fprintf( yyc, "\nHB_INIT_SYMBOLS_END( hb_vm_SymbolInit_%s%s )\n\n"
                    "#if defined(HB_PRAGMA_STARTUP)\n"
                    "   #pragma startup hb_vm_SymbolInit_%s%s\n"
                    "#elif defined(_MSC_VER)\n"
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

      if( hb_comp_bExplicitStartProc && iStartupOffset >= 0 )
      {
         fprintf( yyc, "extern HB_EXPORT void hb_vmExplicitStartup( PHB_SYMB pSymbol );\n" );
         fprintf( yyc, "void hb_InitExplicitStartup( void )\n" );
         fprintf( yyc, "{\n" );
         fprintf( yyc, "   hb_vmExplicitStartup( symbols + %i );\n", iStartupOffset );
         fprintf( yyc, "}\n" );

         fprintf( yyc, "#if defined(HB_PRAGMA_STARTUP)\n" );
         fprintf( yyc, "   #pragma startup hb_InitExplicitStartup\n" );
         fprintf( yyc, "#elif defined(_MSC_VER)\n" );
         fprintf( yyc, "   static HB_$INITSYM hb_auto_InitExplicitStartup = hb_InitExplicitStartup;\n" );
         fprintf( yyc, "#endif\n\n" );
      }

      if( bCritical )
      {
         fprintf( yyc, "#if defined(HB_PRAGMA_STARTUP)\n" );
         fprintf( yyc, "   #pragma startup hb_InitCritical%s\n", hb_comp_FileAsSymbol );
         fprintf( yyc, "#elif defined(_MSC_VER)\n" );
         fprintf( yyc, "   static HB_$INITSYM hb_auto_InitCritical = hb_InitCritical%s;\n", hb_comp_FileAsSymbol );
         fprintf( yyc, "#endif\n\n" );
      }

      if( hb_comp_pGlobals )
      {
         fprintf( yyc, "\n#include \"hbapi.h\"\n\n" );

         pGlobal = hb_comp_pGlobals;
         while( pGlobal )
         {
            if( pGlobal->szAlias == NULL )
            {
               fprintf( yyc, "HB_ITEM %s = { 0, { { 0 } } };\n", pGlobal->szName );
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
                       "static PHB_ITEM *pGlobals = (PHB_ITEM *) pConstantGlobals;\n\n" );
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
         // The pCode Table is Written Here
        if ( hb_comp_iGenVarList )
        {
           fprintf( hb_comp_pCodeList, "[%s]\n", pFunc->szName );
        }

         bIsInitFunction   = ( pFunc->cScope & HB_FS_INIT ) ;
         bIsExitFunction   = ( pFunc->cScope & HB_FS_EXIT ) ;
         bIsStaticVariable = ( pFunc == hb_comp_pInitFunc ) ;
         bIsGlobalVariable = ( pFunc == hb_comp_pGlobalsFunc ) ;
         bIsPublicFunction = ( pFunc->cScope == HB_FS_PUBLIC ) ;

         /* Is it a PUBLIC FUNCTION/PROCEDURE */
         if ( bIsPublicFunction )
         {
            fprintf( yyc, "HB_FUNC( %s )", pFunc->szName );
         }
         /* Is it STATICS$ */
         else if( bIsStaticVariable )
         {
            fprintf( yyc, "static HARBOUR hb_INITSTATICS( void )" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it GLOBALS$ */
         else if( bIsGlobalVariable )
         {
            fprintf( yyc, "static HARBOUR hb_INITGLOBALS( void )" ); /* NOTE: hb_ intentionally in lower case */
         }
         /* Is it an INIT FUNCTION/PROCEDURE */
         else if ( bIsInitFunction )
         {
            fprintf( yyc, "HB_FUNC_INIT( _%s )", pFunc->szName );
         }
         /* Is it an EXIT FUNCTION/PROCEDURE */
         else if ( bIsExitFunction )
         {
            fprintf( yyc, "HB_FUNC_EXIT( _%s )", pFunc->szName );
         }
         /* Then it must be a STATIC FUNCTION/PROCEDURE */
         else
         {
            fprintf( yyc, "HB_FUNC_STATIC( %s )", pFunc->szName );
         }

         fprintf( yyc, "\n{\n   static const BYTE pcode[] =\n   {\n" );

         if( hb_comp_iGenCOutput == HB_COMPGENC_COMPACT )
         {
            hb_compGenCCompact( pFunc, yyc );
         }
         else
         {
            hb_compGenCReadable( pFunc, yyc );
         }

         fprintf( yyc, "   };\n\n" );

         // Finished Writting The pCode Table
         // printf( "\n" );

         if( pFunc->cScope & HB_FS_CRITICAL )
         {
            fprintf( yyc, "   HB_CRITICAL_LOCK( s_Critical%s );\n", pFunc->szName );
         }

         fprintf( yyc, "   hb_vmExecute( pcode, symbols, %s );\n", hb_comp_pGlobals ? "&pGlobals" : "NULL" );

         if( pFunc->cScope & HB_FS_CRITICAL )
         {
            fprintf( yyc, "   HB_CRITICAL_UNLOCK( s_Critical%s );\n", pFunc->szName );
         }

         fprintf( yyc,  "}\n\n" );

         pFunc = pFunc->pNext;
      }

      if( iLocalGlobals )
      {
         fprintf( yyc, "static HARBOUR hb_REGISTERGLOBALS( void )\n"
                       "{\n"
                       "   hb_vmRegisterGlobals( &pGlobals, %i );\n", iGlobals );
         fprintf( yyc, "}\n\n" );
      }

      /*
      Generate pCode Listing
      */

      /* Generate codeblocks data
       */
      if( hb_comp_cInlineID > '0' )
      {
         fprintf( yyc, "#include \"hbapi.h\"\n" );
         fprintf( yyc, "#include \"hbstack.h\"\n" );
         fprintf( yyc, "#include \"hbapierr.h\"\n" );
         fprintf( yyc, "#include \"hbapiitm.h\"\n" );
         fprintf( yyc, "#include \"hbvm.h\"\n" );
         fprintf( yyc, "#include \"hboo.ch\"\n" );
      }

      pInline = hb_comp_inlines.pFirst;

      if ( pInline )
      {
         hb_compGenCInLine( yyc );
      }
   }
   else
   {
      pInline = hb_comp_inlines.pFirst;

      if ( pInline )
      {
         hb_compGenCInLine( yyc );
      }
      else
      {
         fprintf( yyc, "/* Empty source file */\n\n" );
      }
   }

   fclose( yyc );

   pFunc = hb_comp_functions.pFirst;
   while( pFunc )
   {
      pFunc = hb_compFunctionKill( pFunc );
   }

   pFunc = hb_comp_funcalls.pFirst;
   while( pFunc )
   {
      hb_comp_funcalls.pFirst = pFunc->pNext;
      hb_xfree( ( void * ) pFunc );  /* NOTE: szName will be released by hb_compSymbolKill() */
      pFunc = hb_comp_funcalls.pFirst;
   }

   pInline = hb_comp_inlines.pFirst;
   while( pInline )
   {
      hb_comp_inlines.pFirst = pInline->pNext;
      if( pInline->pCode )
      {
         hb_xfree( ( void * ) pInline->pCode );
      }
      hb_xfree( ( void * ) pInline->szFileName );
      hb_xfree( ( void * ) pInline );  /* NOTE: szName will be released by hb_compSymbolKill() */
      pInline = hb_comp_inlines.pFirst;
   }

   if ( hb_comp_iWarnings >= 3 )
   {
      pDeclared = hb_comp_pReleaseDeclared->pNext;
      while( pDeclared )
      {
         hb_comp_pFirstDeclared = pDeclared->pNext;
         hb_xfree( ( void * ) pDeclared );
         pDeclared = hb_comp_pFirstDeclared;
      }

      pClass = hb_comp_pReleaseClass->pNext;
      while( pClass )
      {
         hb_comp_pFirstClass = pClass->pNext;

         pDeclared = pClass->pMethod;
         while ( pDeclared )
         {
            hb_comp_pFirstDeclared = pDeclared->pNext;
            hb_xfree( ( void * ) pDeclared );
            pDeclared = hb_comp_pFirstDeclared;
         }

         hb_xfree( ( void * ) pClass );
         pClass = hb_comp_pFirstClass;
      }
   }

   pSym = hb_comp_symbols.pFirst;
   while( pSym )
   {
      pSym = hb_compSymbolKill( pSym );
   }

   pGlobal = hb_comp_pGlobals;
   while( pGlobal )
   {
      pDelete = pGlobal;
      pGlobal = pGlobal->pNext;
      hb_xfree( pDelete );
   }

   #ifndef HB_BACK_END
      #define HB_BACK_END 0
   #endif

   if( HB_BACK_END == 0 )
   {
      if( ! hb_comp_bQuiet )
      {
         printf( "Done.\n" );
      }
   }

   /*
    Close .p file
   */
   if ( hb_comp_iGenVarList )
   {
      fclose( hb_comp_pCodeList );
   }

   hb_xfree( hb_comp_PrgFileName );

   pStatSymTemp = pStatSymFirst;
   while( pStatSymTemp )
   {
      // printf( "RELEASING STATIC: >>%s<<\n", pStatSymTemp->szName );
      hb_xfree( pStatSymTemp->szName );
      pStatSymTemp = pStatSymTemp->pNext;
      hb_xfree( ( void * ) pStatSymFirst );
      pStatSymFirst = pStatSymTemp;
   }

   pStatSymTemp = pPubSymFirst;
   while( pStatSymTemp )
   {
      // printf( "RELEASING PUBLIC: >>%s<<\n", pStatSymTemp->szName );
      hb_xfree( pStatSymTemp->szName );
      pStatSymTemp = pStatSymTemp->pNext;
      hb_xfree( ( void * ) pPubSymFirst );
      pPubSymFirst = pStatSymTemp;
   }

}

/*
  Searching for function names in in-line-c for writing prototypes
*/
static BOOL hb_compCStaticSymbolFound( char* szSymbol, BOOL bSearchStatic )
{
   BOOL bStatSymFound = FALSE;
   PSSYMLIST pStatSymTemp = bSearchStatic ? pStatSymFirst : pPubSymFirst;

   while( pStatSymTemp )
   {
      if( strcmp( pStatSymTemp->szName, szSymbol ) == 0 )
      {
         bStatSymFound = TRUE;
         break;
      }
      pStatSymTemp = pStatSymTemp->pNext;
   }

   return bStatSymFound;
}

/*
  Collecting function names from in-line-c. There are two categories, ie
  statics (HB_FUNC_STATIC) and publics (HB_FUNC)
*/
static void hb_compCStatSymList( char* statSymName, BOOL bPublic )
{
   PSSYMLIST pStatSymLast = (PSSYMLIST) hb_xgrab( sizeof( SSYMLIST ) );
   int ulLen = strlen( statSymName );

   while( ulLen && HB_ISSPACE( statSymName[ ulLen - 1 ] ) )
   {
      ulLen--;
   }

   statSymName[ ulLen ] = '\0';
   pStatSymLast->szName = (char*) hb_xgrab( strlen( statSymName ) + 1 );
   strcpy( pStatSymLast->szName, statSymName );

   if( bPublic )
   {
      pStatSymLast->pNext = pPubSymFirst ? pPubSymFirst : NULL;
      pPubSymFirst = pStatSymLast;
   }
   else
   {
      pStatSymLast->pNext = pStatSymFirst ? pStatSymFirst : NULL ;
      pStatSymFirst = pStatSymLast;
   }
}

/*
  Parsing in-line-c codes to extract function names
*/
static void hb_compGenCCheckInLineStatic( char *str )
{
   LONG nAt;
   char *pTmp, *pCode;
   LONG ulLen = strlen( str );
   LONG i ;
   BOOL bPublic;

   while( ( nAt = hb_strAt( "HB_FUNC", 7, str, ulLen ) ) != 0 )
   {
      bPublic = ( str[ nAt + 6 ] != '_' );

      str += nAt;
      i = 0;

      while( ( pTmp = strchr( str, '(' ) ) == NULL && ++i < ulLen ) {}

      pTmp++ ;

      while( HB_ISSPACE( *pTmp ) )
      {
         pTmp++ ;
      }

      i = 1;
      ulLen = strlen( str );

      while( ( pCode = strchr( str, ')' ) ) == NULL && ++i < ulLen ) {}

      *pCode = '\0';

      hb_compCStatSymList( pTmp, bPublic );

   }
}

/*
  Grab the content of in-line-c codes to be parse for function names
*/
static void hb_compGenCInLineSymbol()
{
   PINLINE pInline = hb_comp_inlines.pFirst;
   char *sInline;

   while( pInline )
   {
      sInline = (char*) hb_xgrab( strlen( (char*) pInline->pCode ) + 1 );
      strcpy( sInline, (char*) pInline->pCode );
      hb_compGenCCheckInLineStatic( sInline );
      pInline = pInline->pNext;
      hb_xfree( sInline );
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
   return 1;
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
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );

      if( lOffset > 127 )
      {
         lOffset -= 256;
      }

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
      LONG lOffset = ( LONG ) HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      if( lOffset > SHRT_MAX )
      {
         lOffset -= 65536;
      }

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
      LONG lOffset = ( LONG ) HB_PCODE_MK24BIT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      if( lOffset > 8388607L )
      {
         lOffset -= 16777216;
      }

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
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
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );

      if( lOffset > 127 )
      {
         lOffset -= 256;
      }

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
      LONG lOffset = ( LONG ) HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      if( lOffset > SHRT_MAX )
      {
         lOffset -= 65536;
      }

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
      LONG lOffset = ( LONG ) HB_PCODE_MK24BIT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      if( lOffset > 8388607L )
      {
         lOffset -= 16777216;
      }

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
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
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );

      if( lOffset > 127 )
      {
         lOffset -= 256;
      }

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
      LONG lOffset = ( LONG ) HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      if( lOffset > SHRT_MAX )
      {
         lOffset -= 65536;
      }

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
      LONG lOffset = ( LONG ) HB_PCODE_MK24BIT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      if( lOffset > 8388607L )
      {
         lOffset -= 16777216;
      }

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
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

   return (USHORT) ( lPCodePos - ulStart + 1 );
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

   return (USHORT) ( lPCodePos - ulStart + 1 );
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
      SHORT wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( wVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -wVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", wVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, wVar )->szName );
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
      char wVar = ( char ) pFunc->pCode[ lPCodePos + 1 ];
      /* Variable with negative order are local variables
         * referenced in a codeblock -handle it with care
         */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( wVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -wVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", wVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, wVar )->szName );
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
      if( ( pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) ) != ( HB_FS_INIT | HB_FS_EXIT ) )
      {
         if( cargo->bVerbose )
         {
            fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, w )->szName );
         }
      }

      fprintf( cargo->yyc, "\n" );
      lPCodePos += 2;
   }

   return (USHORT) (lPCodePos - ulStart);
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
      *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
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
            (int)(char) pFunc->pCode[ lPCodePos + 1 ] );
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
      fprintf( cargo->yyc, "\t/* %i */",
            (int)(short) HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
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
      SHORT wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( wVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -wVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", wVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, wVar )->szName );
      }
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocalnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      signed char wVar = ( signed char ) pFunc->pCode[ lPCodePos + 1 ];
      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( wVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -wVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", wVar );
         }
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, wVar )->szName );
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
      SHORT wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( wVar < 0 )
         {
            fprintf( cargo->yyc, "\t/* localvar%i */", -wVar );
         }
         else
         {
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", wVar );
         }
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, wVar )->szName );
      }
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlong )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLONG, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %li */", HB_PCODE_MKLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }
   fprintf( cargo->yyc, "\n" );

   return 1 + sizeof( LONG );
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

   return (USHORT) (lPCodePos - ulStart);
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

   return (USHORT) ( lPCodePos - ulStart );
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
      LONG lOffset = HB_PCODE_MK24BIT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, lPCodePos + lOffset );
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
      LONG lOffset = HB_PCODE_MK24BIT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, lPCodePos + lOffset );
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
   return (USHORT) lByteCount;
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

   return (USHORT) ( lPCodePos - ulStart + 1 );
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
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARADDINT, %i, %i, %i,", pFunc->pCode[ lPCodePos + 1 ],
                                                               pFunc->pCode[ lPCodePos + 2 ],
                                                               pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, ( signed char ) pFunc->pCode[ lPCodePos + 1 ] )->szName,
               HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_localnearsetint )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARSETINT, %i, %i, %i,", pFunc->pCode[ lPCodePos + 1 ],
                                                               pFunc->pCode[ lPCodePos + 2 ],
                                                               pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, ( signed char ) pFunc->pCode[ lPCodePos + 1 ] )->szName,
               HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) ) );
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
      // fprintf( cargo->yyc, "\t/* %i */", uLen );
      // To Be More Readable
      fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, ( signed char ) pFunc->pCode[ lPCodePos + 1 ] )->szName,
               uLen );

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

   return ( USHORT ) ( lPCodePos - ulStart );
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
   fprintf( cargo->yyc, "\tHB_P_SWITCHCASE, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %li */", HB_PCODE_MKLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 1 + sizeof( LONG );
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

/* NOTE: The order of functions has to match the order of opcodes
 *       mnemonics
 */
static HB_GENC_FUNC_PTR s_verbose_table[] = {
   hb_p_and,
   hb_p_arraypush,
   hb_p_arraypop,
   hb_p_arraydim,
   hb_p_arraygen,
   hb_p_equal,
   hb_p_endblock,
   hb_p_endproc,
   hb_p_exactlyequal,
   hb_p_false,
   hb_p_fortest,
   hb_p_function,
   hb_p_functionshort,
   hb_p_frame,
   hb_p_funcptr,
   hb_p_greater,
   hb_p_greaterequal,
   hb_p_dec,
   hb_p_divide,
   hb_p_do,
   hb_p_doshort,
   hb_p_duplicate,
   hb_p_dupltwo,
   hb_p_inc,
   hb_p_instring,
   hb_p_jumpnear,
   hb_p_jump,
   hb_p_jumpfar,
   hb_p_jumpfalsenear,
   hb_p_jumpfalse,
   hb_p_jumpfalsefar,
   hb_p_jumptruenear,
   hb_p_jumptrue,
   hb_p_jumptruefar,
   hb_p_lessequal,
   hb_p_less,
   hb_p_line,
   hb_p_localname,
   hb_p_macropop,
   hb_p_macropopaliased,
   hb_p_macropush,
   hb_p_macropusharg,
   hb_p_macropushlist,
   hb_p_macropushindex,
   hb_p_macropushpare,
   hb_p_macropushaliased,
   hb_p_macrosymbol,
   hb_p_macrotext,
   hb_p_message,
   hb_p_minus,
   hb_p_modulus,
   hb_p_modulename,
   /* start: pcodes generated by macro compiler */
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   /* end: */
   hb_p_mult,
   hb_p_negate,
   hb_p_noop,
   hb_p_not,
   hb_p_notequal,
   hb_p_or,
   hb_p_parameter,
   hb_p_plus,
   hb_p_pop,
   hb_p_popalias,
   hb_p_popaliasedfield,
   hb_p_popaliasedfieldnear,
   hb_p_popaliasedvar,
   hb_p_popfield,
   hb_p_poplocal,
   hb_p_poplocalnear,
   hb_p_popmemvar,
   hb_p_popstatic,
   hb_p_popvariable,
   hb_p_power,
   hb_p_pushalias,
   hb_p_pushaliasedfield,
   hb_p_pushaliasedfieldnear,
   hb_p_pushaliasedvar,
   hb_p_pushblock,
   hb_p_pushblockshort,
   hb_p_pushfield,
   hb_p_pushbyte,
   hb_p_pushint,
   hb_p_pushlocal,
   hb_p_pushlocalnear,
   hb_p_pushlocalref,
   hb_p_pushlong,
   hb_p_pushmemvar,
   hb_p_pushmemvarref,
   hb_p_pushnil,
   hb_p_pushdouble,
   hb_p_pushself,
   hb_p_pushstatic,
   hb_p_pushstaticref,
   hb_p_pushstr,
   hb_p_pushstrshort,
   hb_p_pushsym,
   hb_p_pushsymnear,
   hb_p_pushvariable,
   hb_p_retvalue,
   hb_p_send,
   hb_p_sendshort,
   hb_p_seqbegin,
   hb_p_seqend,
   hb_p_seqrecover,
   hb_p_sframe,
   hb_p_statics,
   hb_p_staticname,
   hb_p_swapalias,
   hb_p_true,
   hb_p_zero,
   hb_p_one,
   hb_p_macrolist,
   hb_p_macrolistend,
   hb_p_localnearaddint,
   hb_p_localnearsetint,
   hb_p_localnearsetstr,
   hb_p_addint,
   hb_p_left,
   hb_p_right,
   hb_p_substr,
   hb_p_dummy,
   hb_p_baseline,
   hb_p_lineoffset,
   hb_p_withobject,
   hb_p_sendwith,
   hb_p_sendwithshort,
   hb_p_endwithobject,
   hb_p_foreach,
   hb_p_enumerate,
   hb_p_endenumerate,
   hb_p_pushglobal,
   hb_p_popglobal,
   hb_p_pushglobalref,
   hb_p_enumindex,
   hb_p_switchcase,
   hb_p_like,
   hb_p_match,
   hb_p_pushmacroref,
   hb_p_ivarref,
   hb_p_classsetmodule
};

static void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc )
{
   HB_GENC_INFO genc_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( HB_GENC_FUNC_PTR ) );

   genc_info.iNestedCodeblock = 0;
   genc_info.bVerbose = ( hb_comp_iGenCOutput == HB_COMPGENC_VERBOSE );
   genc_info.yyc = yyc;

   if( ! hb_comp_bQuiet && hb_comp_iGenVarList )
   {
      printf( "Generating pcode list for '%s'...\n", pFunc->szName );
   }

   hb_compPCodeEval( pFunc, ( HB_PCODE_FUNC_PTR * ) s_verbose_table, ( void * ) &genc_info, hb_comp_iGenVarList );

   if( genc_info.bVerbose )
   {
      fprintf( yyc, "/* %05li */\n", pFunc->lPCodePos );
   }
}

static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc )
{
   ULONG lPCodePos = 0;
   int nChar;

   fprintf( yyc, "\t" );

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
}
