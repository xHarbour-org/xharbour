/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Compiler main file
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.xharbour.org
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.xharbour.org
 *
 * Copyright 2000 RonPinkas <Ron@Profit-Master.com>
 *    hb_compPrepareOptimize()
 *    hb_compOptimizeJumps()
 *    hb_compOptimizeFrames()
 *    hb_compAutoOpenAdd()
 *    hb_compAutoOpenFind()
 *    hb_compAutoOpen()
 *    hb_compDeclaredParameterAdd()
 *    hb_compClassAdd()
 *    hb_compClassFind()
 *    hb_compMethodAdd()
 *    hb_compMethodFind()
 *    hb_compDeclaredAdd()
 *    hb_compDeclaredInit()
 *    hb_compEnumAdd()
 *    hb_compEnumMemberAdd()
 *
 * Copyright 2005 Vicente Guerra <vicente@guerra.com.mx>
 *    hb_compHideString()
 *
 * See doc/license.txt for licensing terms.
 *
 */



#include "hbcomp.h"
#include "hbhash.h"
#include "hbi18n.h"
#include "hbexemem.h"
#include "hbmemory.ch"
#include "hbset.h"
#include "hbdate.h"
#include "hbverbld.h"
#include "hbapierr.h"
#include "pragma.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

#if defined( HB_OS_DOS ) && defined( __BORLANDC__ )
   #include <limits.h>
extern unsigned _stklen = UINT_MAX;
#endif

static void hb_compInitVars( void );
static void hb_compGenOutput( int, const char * szSourceExtension );
static void hb_compOutputFile( void );

int hb_compLocalGetPos( char * szVarName );                          /* returns the order + 1 of a local variable */
int hb_compStaticGetPos( char *, PFUNCTION );                        /* return if passed name is a static variable */
int hb_compFieldGetPos( char *, PFUNCTION );                         /* return if passed name is a field variable */
int hb_compMemvarGetPos( char *, PFUNCTION );                        /* return if passed name is a memvar variable */
USHORT hb_compVariableGetPos( PVAR pVars, char * szVarName );        /* returns the order + 1 of a variable if defined or zero */

static void hb_compGenFieldPCode( BYTE, int, char *, PFUNCTION );    /* generates the pcode for database field */
static void hb_compGenVariablePCode( BYTE, char * );                 /* generates the pcode for undeclared variable */
static void hb_compGenVarPCode( BYTE, char * );                      /* generates the pcode for undeclared variable */
static void hb_compGenLocalName( USHORT wLocal, char * szVarName );  /* generates the pcode for local variable name */

static PFUNCTION hb_compFunctionNew( char *, HB_SYMBOLSCOPE );       /* creates and initialises the _FUNC structure */
static PINLINE hb_compInlineNew( char * );                           /* creates and initialises the __INLINE structure */
static void hb_compCheckDuplVars( PVAR pVars, char * szVarName );    /*checks for duplicate variables definitions */
static int hb_compProcessRSPFile( char * );                          /* process response file */
static int hb_compCompile( char * szPrg );

static void hb_compOptimizeJumps( void );
static void hb_compOptimizeFrames( PFUNCTION pFunc );

static void hb_compDeclaredInit( void );

static void hb_compLineNumberDefStart( void );
static void hb_compLineNumberDefEnd( void );

/* global variables */
PHB_PP_STATE   hb_comp_PP = NULL;

char *         hb_pp_STD_CH;
int            hb_pp_STD_CH_ADDITIVE;

FUNCTIONS      hb_comp_functions;
FUNCALLS       hb_comp_funcalls;
COMPSYMBOLS    hb_comp_symbols;
PCOMDECLARED   hb_comp_pFirstDeclared;
PCOMDECLARED   hb_comp_pLastDeclared;
PCOMDECLARED   hb_comp_pReleaseDeclared;

PCOMCLASS      hb_comp_pFirstClass;
PCOMCLASS      hb_comp_pLastClass;
PCOMCLASS      hb_comp_pReleaseClass;
char *         hb_comp_szFromClass;
PCOMDECLARED   hb_comp_pLastMethod;

PENUMDEF       hb_comp_pEnum;

char *         hb_comp_szFromEnum;

int            hb_comp_iLine;                               /* currently processed line number (globaly) */
char *         hb_comp_szFile;                              /* File Name of last compiled line */
char *         hb_comp_PrgFileName;                         /* Original PRG File Name */
PFUNCTION      hb_comp_pInitFunc;
PFUNCTION      hb_comp_pGlobalsFunc;
PFUNCTION      hb_comp_pLineNumberFunc;
PHB_FNAME      hb_comp_pFileName          = NULL;

BOOL           hb_comp_bPPO               = FALSE;          /* flag indicating, is ppo output needed */
FILE *         hb_comp_yyppo              = NULL;           /* output .ppo file */
BOOL           hb_comp_bStartProc         = TRUE;           /* holds if we need to create the starting procedure */
BOOL           hb_comp_bExplicitStartProc = FALSE;          /* holds if we need to support EXPLICIT App starting procedure */
BOOL           hb_comp_bLineNumbers       = TRUE;           /* holds if we need pcodes with line numbers */
BOOL           hb_comp_bQuiet             = FALSE;          /* quiet mode */
BOOL           hb_comp_bShortCuts         = TRUE;           /* .and. & .or. expressions shortcuts */
int            hb_comp_iWarnings          = 0;              /* enable parse warnings */
BOOL           hb_comp_bAnyWarning        = FALSE;          /* holds if there was any warning during the compilation process */
BOOL           hb_comp_bAutoMemvarAssume  = FALSE;          /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
BOOL           hb_comp_bForceMemvars      = FALSE;          /* holds if memvars are assumed when accesing undeclared variable (-v)*/
BOOL           hb_comp_bDebugInfo         = FALSE;          /* holds if generate debugger required info */
char           hb_comp_szPrefix[ 21 ]     = { '\0' };       /* holds the prefix added to the generated symbol init function name (in C output currently) */
int            hb_comp_iGenCOutput  = HB_COMPGENC_VERBOSE;  /* C code generation should be verbose (use comments) or not */
BOOL           hb_comp_bNoStartUp   = FALSE;                /* C code generation embed HB_FS_FIRST or not */
int            hb_comp_iExitLevel   = HB_EXITLEVEL_DEFAULT; /* holds if there was any warning during the compilation process */
int            hb_comp_iFunctionCnt;
int            hb_comp_iErrorCount;
char           hb_comp_cVarType;                            /* current declared variable type */
char           hb_comp_cDataListType;                       /* current declared variable list type */
char           hb_comp_cCastType;                           /* current casting type */
BOOL           hb_comp_bDontGenLineNum = FALSE;             /* suppress line number generation */
HB_SIZE        hb_comp_ulLastLinePos;                       /* position of last opcode with line number */
HB_SIZE        hb_comp_ulLastModuleNamePos;                 /* position of last opcode with module name */
HB_SIZE        hb_comp_ulSavedModuleNamePos;                /* position of last opcode with module name saved inside a STATIC/GLOBAL definition */
int            hb_comp_iStaticCnt;                          /* number of defined statics variables on the PRG */
int            hb_comp_iVarScope;                           /* holds the scope for next variables to be defined */
PHB_FNAME      hb_comp_pOutPath           = NULL;
PHB_FNAME      hb_comp_ppo_pOutPath       = NULL;
BOOL           hb_comp_bCredits           = FALSE;          /* print credits */
BOOL           hb_comp_bBuildInfo         = FALSE;          /* print build info */
BOOL           hb_comp_bI18n              = FALSE;          /* Output i18n file */
char *         hb_comp_szHILout           = NULL;           /* Output file name */
FILE *         hb_comp_HILfile            = NULL;           /* output .hil file */
BOOL           hb_comp_bLogo              = TRUE;           /* print logo */
BOOL           hb_comp_bSyntaxCheckOnly   = FALSE;          /* syntax check only */
int            hb_comp_iLanguage          = LANG_C;         /* default Harbour generated output language */
int            hb_comp_iJumpOptimize      = 1;
char *         hb_comp_szDeclaredFun      = NULL;
char *         hb_Command_Line;                             /* Switches to be documented in generated C file */
PHB_EXPR_LIST  hb_comp_exprs              = NULL;           /* expressions list */

/* FILE *      hb_comp_errFile            = NULL;
 */

BOOL           hb_comp_bAutoOpen          = TRUE;
BOOL           hb_comp_bError             = FALSE;
USHORT         hb_comp_cInlineID          = 0;

int            hb_comp_iLineINLINE        = 0;
int            hb_comp_iLinePRG;
INLINES        hb_comp_inlines;

/* various compatibility flags (-k switch) */
HB_SIZE        hb_comp_Supported;

int            hb_comp_iBaseLine;
HB_SIZE        hb_comp_ulLastOffsetPos;

PVAR           hb_comp_pGlobals;
short          hb_comp_iGlobals;

/* Encode strings method (0 means "no encode") */
int            hb_comp_iHidden = 0;

/* EXTERNAL statement can be placed into any place in a function - this flag is
 * used to suppress error report generation
 */
/*
   I don't see where this is needed  - pt
   see 3 other occurrances of this variable

   static BOOL hb_comp_bExternal   = FALSE;
 */

/* Ron Pinkas 2009/02/12 WHY not respect es2 for any warning - as per Clipper docs?
 */
#if 0
/* Limit the warning that stop compilation into ambiguous reference only so
   that warnings on uninitialized locals would not stop compilation when
   exit severity is set to 2
 */
BOOL        hb_comp_AmbiguousVar = FALSE;
#endif

NAMESPACES  hb_comp_Namespaces, hb_comp_UsedNamespaces;

/* linked list with EXTERNAL symbols declarations
 */
static PEXTERN    hb_comp_pExterns  = NULL;
static PAUTOOPEN  hb_comp_pAutoOpen = NULL;
static int hb_compAutoOpen( char * szPrg, BOOL * bSkipGen );

/* -m Support */
static BOOL hb_compAutoOpenFind( char * szName );

extern int hb_comp_yyparse( void );    /* main yacc parsing function */

extern void hb_compReleaseRTVars( void );
extern void hb_compReleaseLoops( void );
extern void hb_compReleaseElseIfs( void );
/*
   The following two variables are for the purpose of
   creating Local Variable List (.var) file
 */
BOOL     hb_comp_iGenVarList              = FALSE;
FILE *   hb_comp_VariableList             = NULL;

/* PreProcessor Tracing support. */
BOOL     hb_comp_bTracePP                 = FALSE;

/* Auto declare external function as dynamic */
BOOL     hb_comp_autoDeferred             = FALSE;

/* Force cpp output (default is c )*/
BOOL     hb_comp_OutputIsCpp              = FALSE;

/* procude list of public function in a module */
BOOL     hb_comp_createExternList         = FALSE;

/* force use of reserved PP words as variable name */
BOOL     hb_comp_bUsePPReservedWord       = FALSE;

BOOL     hb_comp_bWarnUnUsedLocals        = FALSE;
BOOL     hb_comp_bWarnUnUsedStatics       = FALSE;
BOOL     hb_comp_bWarnUnUsedGlobals       = FALSE;
BOOL     hb_comp_bWarnUnUsedMemvars       = FALSE;
BOOL     hb_comp_bWarnUnUsedFields        = FALSE;
BOOL     hb_comp_bWarnUnUsedBlockParams   = FALSE;

/* how many pcode used in a module */
ULONG    hb_comp_upCodeTotal              = 0;

/* AJ: 2012-12-08 Harbour compiler output handlers
 * These replace the use of hb_comp_errFile
 * Errors are now directed to stderr
 */
char           hb_comp_szMsgBuf[ HB_PATH_MAX * 2 + 80 ] = { '\0' };
void*          hb_compHandle                            = NULL;
HB_OUTSTDFUNC  hb_outStdFunc                            = NULL;
HB_OUTERRFUNC  hb_outErrFunc                            = NULL;

#define MAX_MEM_COMMAND_LINE 10240

/* Used in hb_compAutoOpen() */
int                  ArgC = 0;
char **              ArgV;

extern HB_COMP_IDS   hb_compExpr_IDs;

static BOOL hb_bcompDeclaredInit = FALSE;

int hb_compMain( int argc, char * argv[] )
{
   int      iStatus = EXIT_SUCCESS;
   int      i;
   BOOL     bAnyFiles;
   char *   szBuildInfo;

   hb_bcompDeclaredInit = FALSE;

   /* Stored for later use in hb_compAutoOpen(). */
   ArgC                 = argc;
   ArgV                 = ( char ** ) argv;

   hb_comp_PP           = hb_pp_new();

   hb_comp_pOutPath     = NULL;
   hb_comp_ppo_pOutPath = NULL;

/* AJ: 2012-12-08, Revised, by using new output handlers */
#if 0
#if defined( HOST_OS_UNIX_COMPATIBLE )
   hb_comp_errFile      = stderr;
#else
   hb_comp_errFile      = stdout;
#endif
#endif

   /* Activate Harbour extensions by default. */
   hb_comp_Supported = HB_COMPFLAG_HARBOUR;
   hb_comp_Supported |= HB_COMPFLAG_XBASE;
   hb_comp_Supported |= HB_COMPFLAG_HB_INLINE;
   hb_comp_Supported |= HB_COMPFLAG_OPTJUMP;

   /* First check the environment variables */
   hb_Command_Line   = ( char * ) hb_xgrab( MAX_MEM_COMMAND_LINE );
   hb_xmemset( hb_Command_Line, 0, MAX_MEM_COMMAND_LINE );

   hb_compChkCompilerSwitch( 0, NULL );

   /* Then check command line arguments
      This will override duplicated environment settings */
   hb_compChkCompilerSwitch( argc, argv );

   if( hb_comp_bLogo )
      hb_compPrintLogo();

   if( hb_comp_bBuildInfo )
   {
      hb_compOutStd( "\n" );
      szBuildInfo = hb_verBuildInfo( TRUE );
      hb_xfree( szBuildInfo );
      hb_compCleanUp();
      return iStatus;
   }

   if( hb_comp_bCredits )
   {
      hb_compPrintCredits();
      hb_compCleanUp();
      return iStatus;
   }

   /* Set Search Path */
   hb_compChkPaths();

   /* Set standard rules */
   hb_compInitPP( argc, argv );

   /* Prepare the table of identifiers */
   hb_compIdentifierOpen();

   /* Load standard Declarations. */
   if( hb_comp_iWarnings >= 3 && ! hb_bcompDeclaredInit )
      hb_compDeclaredInit();

   /* Process all files passed via the command line. */

   bAnyFiles = FALSE;

   for( i = 1; i < argc; i++ )
   {
      hb_comp_upCodeTotal = 0;

      if( ! HB_ISOPTSEP( argv[ i ][ 0 ] ) )
      {
         if( argv[ i ][ 0 ] == '@' )
            iStatus = hb_compProcessRSPFile( argv[ i ] );
         else
            iStatus = hb_compCompile( argv[ i ] );

         if( ! bAnyFiles )
            bAnyFiles = TRUE;

         if( iStatus != EXIT_SUCCESS )
            break;
      }
   }

   hb_compCleanUp();

   if( ! bAnyFiles && ! hb_comp_bQuiet )
   {
      hb_compPrintUsage( argv[ 0 ] );
      iStatus = EXIT_FAILURE;
   }

   if( hb_comp_iErrorCount > 0 )
   {
      if( hb_comp_exprs )
      {
         PHB_EXPR_LIST pList, pExp;

         pList = pExp = hb_comp_exprs;
         hb_comp_exprs = NULL;
         while( pExp )
         {
            hb_compExprDelete( &pExp->Expr );
            pExp = pExp->pNext;
         }
         while( pList )
         {
            pExp  = pList;
            pList = pList->pNext;
            hb_xfree( pExp );
         }
      }

      iStatus = EXIT_FAILURE;
   }

   if( hb_comp_VariableList != NULL )
      fclose( hb_comp_VariableList );

   /* hb_xexit(); */

   return iStatus;
}

/* ------------------------------------------------------------------------- */
/**                          ACTIONS                                        **/
/* ------------------------------------------------------------------------- */

/*
 * This function adds the name of called function into the list
 * as they have to be placed on the symbol table later than the first
 * public symbol
 */
PFUNCALL hb_compFunCallAdd( char * szName, void * Namespace, int iFlags )
{
   PFUNCALL pFunCall;

   pFunCall = hb_compFunCallFind( szName, Namespace, iFlags );

   if( pFunCall == NULL )
   {
      pFunCall             = ( PFUNCALL ) hb_xgrab( sizeof( _FUNCALL ) );

      pFunCall->szName     = szName;
      pFunCall->Namespace  = Namespace;
      pFunCall->iFlags     = iFlags;
      pFunCall->pNext      = NULL;

      if( ! hb_comp_funcalls.iCount )
      {
         hb_comp_funcalls.pFirst = pFunCall;
         hb_comp_funcalls.pLast  = pFunCall;
      }
      else
      {
         hb_comp_funcalls.pLast->pNext = pFunCall;
         hb_comp_funcalls.pLast        = pFunCall;
      }

      hb_comp_funcalls.iCount++;

      if( ( pFunCall->iFlags & NSF_EXPLICITPATH ) == NSF_EXPLICITPATH && ( pFunCall->iFlags & NSF_DEFER ) != NSF_DEFER )
      {
         char *   szNamespace = ( char * ) Namespace;
         char *   pTmp        = strchr( szNamespace, '.' );

         if( pTmp )
            pTmp[ 0 ] = '\0';

         szNamespace = hb_compIdentifierNew( szNamespace, TRUE );

         if( hb_compNamespaceFind( hb_comp_Namespaces.pFirst, szNamespace, NSTYPE_SPACE ) == NULL && hb_compNamespaceFind( hb_comp_UsedNamespaces.pFirst, szNamespace, NSTYPE_SPACE ) == NULL )
         {
            hb_compUsedNamespaceNew( szNamespace, NSTYPE_SPACE );
            hb_compUsedNamespaceEnd();
         }

         if( pTmp )
            pTmp[ 0 ] = '.';
      }
   }

   return pFunCall;
}

/*
 * This function adds the name of external symbol into the list of externals
 * as they have to be placed on the symbol table later than the first
 * public symbol
 */
void hb_compExternAdd( char * szExternName, char * szNamespace, HB_SYMBOLSCOPE cScope ) /* defines a new extern name */
{
   PEXTERN     pExtern, pLast;
   PCOMSYMBOL  pSym;

   if( strcmp( "_GET_", szExternName ) == 0 )
   {
      /* special function to implement @ GET statement */
      hb_compExternAdd( "__GETA", szNamespace, 0 );
      szExternName = "__GET";
   }

   pExtern              = ( PEXTERN ) hb_xgrab( sizeof( _EXTERN ) );

   pExtern->szName      = szExternName;
   pExtern->szNamespace = szNamespace;
   pExtern->cScope      = cScope;
   pExtern->pNext       = NULL;

   if( hb_comp_pExterns == NULL )
      hb_comp_pExterns = pExtern;
   else
   {
      pLast = hb_comp_pExterns;

      while( pLast->pNext )
         pLast = pLast->pNext;

      pLast->pNext = pExtern;
   }

   if( szNamespace )
   {
      PNAMESPACE  pNamespace;
      char *      szConcat = hb_xstrcpy( NULL, szNamespace, ".", szExternName, NULL );

      szNamespace = hb_compIdentifierNew( szConcat, FALSE );

      pNamespace  = hb_compNamespaceFind( hb_comp_UsedNamespaces.pFirst, szNamespace, 0 );

      if( pNamespace )
      {
         /* TODO: Duplicate error! */
      }
      else
         hb_compUsedNamespaceNew( szNamespace, NSF_DEFER );

      pSym = hb_compSymbolAdd( pExtern->szName, NULL, ( void * ) pExtern->szNamespace, SYMF_NS_EXPLICITPATH | SYMF_PUBLIC );

      if( pExtern->cScope & HB_FS_DEFERRED )
         hb_compFunCallAdd( pExtern->szName, pExtern->szNamespace, NSF_DEFER );
      else
         hb_compFunCallAdd( pExtern->szName, pExtern->szNamespace, NSF_EXPLICITPATH );

   }
   else
   {
      pSym = hb_compSymbolAdd( pExtern->szName, NULL, NULL, SYMF_FUNCALL | SYMF_PUBLIC );

      hb_compFunCallAdd( pExtern->szName, NULL, NSF_NONE );
   }

   if( ( cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
      pSym->cScope |= HB_FS_DEFERRED;

/*
   hb_comp_bExternal = TRUE;
 */
}

void hb_compDeclaredParameterAdd( char * szVarName, BYTE cValueType )
{
   /* Nothing to do since no warnings requested.*/
   if( hb_comp_iWarnings < 3 )
   {
      HB_SYMBOL_UNUSED( szVarName );
      return;
   }

   /* Either a Declared Function Parameter or a Declared Method Parameter. */
   if( hb_comp_szDeclaredFun )
   {
      /* Find the Declared Function owner of this parameter. */
      PCOMDECLARED pDeclared = hb_compDeclaredFind( hb_comp_szDeclaredFun );

      if( pDeclared )
      {
         pDeclared->iParamCount++;

         if( pDeclared->cParamTypes )
         {
            pDeclared->cParamTypes     = ( BYTE * ) hb_xrealloc( pDeclared->cParamTypes, pDeclared->iParamCount );
            pDeclared->pParamClasses   = ( PCOMCLASS * ) hb_xrealloc( pDeclared->pParamClasses, pDeclared->iParamCount * sizeof( PCOMCLASS ) );
         }
         else
         {
            pDeclared->cParamTypes     = ( BYTE * ) hb_xgrab( 1 );
            pDeclared->pParamClasses   = ( PCOMCLASS * ) hb_xgrab( sizeof( PCOMCLASS ) );
         }

         pDeclared->cParamTypes[ pDeclared->iParamCount - 1 ] = cValueType;

         if( HB_TOUPPER( cValueType ) == 'S' )
         {
            pDeclared->pParamClasses[ pDeclared->iParamCount - 1 ]   = hb_compClassFind( hb_comp_szFromClass );

            /* Resetting */
            hb_comp_szFromClass                                      = NULL;
         }
      }
   }
   else /* Declared Method Parameter */
   {
#if defined( HB_COMP_DEBUG )
      printf( "\n[hb_compDeclaredParameterAdd] Adding parameter: %s Type: %c In Method: %s Class: %s FROM CLASS: %s\n", szVarName, cValueType, hb_comp_pLastMethod->szName, hb_comp_pLastClass->szName, hb_comp_szFromClass );
#endif
      hb_comp_pLastMethod->iParamCount++;

      if( hb_comp_pLastMethod->cParamTypes )
      {
         hb_comp_pLastMethod->cParamTypes    = ( BYTE * ) hb_xrealloc( hb_comp_pLastMethod->cParamTypes, hb_comp_pLastMethod->iParamCount );
         hb_comp_pLastMethod->pParamClasses  = ( PCOMCLASS * ) hb_xrealloc( hb_comp_pLastMethod->pParamClasses, hb_comp_pLastMethod->iParamCount * sizeof( COMCLASS ) );
      }
      else
      {
         hb_comp_pLastMethod->cParamTypes    = ( BYTE * ) hb_xgrab( 1 );
         hb_comp_pLastMethod->pParamClasses  = ( PCOMCLASS * ) hb_xgrab( sizeof( COMCLASS ) );
      }

      hb_comp_pLastMethod->cParamTypes[ hb_comp_pLastMethod->iParamCount - 1 ] = cValueType;

      if( HB_TOUPPER( cValueType ) == 'S' )
      {
         hb_comp_pLastMethod->pParamClasses[ hb_comp_pLastMethod->iParamCount - 1 ] = hb_compClassFind( hb_comp_szFromClass );

#if defined( HB_COMP_DEBUG )
         printf( "\n[hb_compDeclaredParameterAdd] Parameter: %s FROM CLASS: %s\n", szVarName, hb_comp_pLastMethod->pParamClasses[ hb_comp_pLastMethod->iParamCount - 1 ]->szName );
#endif
         /* Resetting */
         hb_comp_szFromClass = NULL;
      }
   }
}

void hb_compVariableAdd( char * szVarName, BYTE cValueType )
{
   PVAR        pVar, pLastVar;
   PFUNCTION   pFunc = hb_comp_functions.pLast;
   BOOL        bUsed = FALSE;

#if defined( HB_AVOID_RESERVED_WORDS )
   if( ! hb_comp_bUsePPReservedWord && szVarName && hb_compReservedPPName( szVarName ) )
   {
       hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_USE_RESERVED_NAME, szVarName, NULL );
       return;
   }
#endif

   if( hb_comp_iVarScope == VS_GLOBAL || hb_comp_iVarScope == VS_EXTERNGLOBAL )
   {
      if( ( ! hb_comp_bStartProc ) && hb_comp_functions.iCount <= 1 )
      {
         if( ++hb_comp_iGlobals > 255 )
         {
            hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_TOOMANY_GLOBALS, szVarName, NULL );
            return;
         }
      }
      else
      {
         hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_GLOBAL_MISPLACED, szVarName, NULL );
         return;
      }
   }

   if( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 && hb_comp_iVarScope == VS_LOCAL )
   {
      /* Variable declaration is outside of function/procedure body.
         In this case only STATIC and PARAMETERS variables are allowed. */
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_OUTSIDE, NULL, NULL );
      return;
   }

   /* check if we are declaring local/static variable after some
    * executable statements
    * Note: FIELD and MEMVAR are executable statements
    */
   if( ( hb_comp_functions.pLast->bFlags & FUN_STATEMENTS ) && ! ( hb_comp_iVarScope == VS_FIELD || ( hb_comp_iVarScope & VS_MEMVAR ) ) )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_FOLLOWS_EXEC, ( hb_comp_iVarScope == VS_LOCAL ? "LOCAL" : "STATIC" ), NULL );

   /* STATIC vars can not be declared in an Extebded Codeblock <|...| ...>.
    */
   if( hb_comp_functions.iCount > 1 && hb_comp_functions.pLast->szName == NULL && hb_comp_iVarScope == VS_STATIC )
   {
      hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "%i Proc: %s\n", hb_comp_functions.iCount, hb_comp_functions.pLast->szName );
      hb_compOutErr( hb_comp_szMsgBuf );
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_FOLLOWS_EXEC, "STATIC", NULL );
   }

   /* Check if a declaration of duplicated variable name is requested */
   if( pFunc->szName )
   {
      /* variable defined in a function/procedure */
      hb_compCheckDuplVars( pFunc->pFields, szVarName );
      hb_compCheckDuplVars( pFunc->pStatics, szVarName );

      /*NOTE: Clipper warns if PARAMETER variable duplicates the MEMVAR
       * declaration
       */
      if( hb_comp_iVarScope == VS_PRIVATE || hb_comp_iVarScope == VS_PUBLIC )
      {
         if( hb_comp_bAutoMemvarAssume )
            hb_compCheckDuplVars( pFunc->pMemvars, szVarName );
      }
      else
         hb_compCheckDuplVars( pFunc->pMemvars, szVarName );
   }

   hb_compCheckDuplVars( hb_comp_pGlobals, szVarName );
   hb_compCheckDuplVars( pFunc->pLocals, szVarName );

   pVar              = ( PVAR ) hb_xgrab( sizeof( VAR ) );
   pVar->szName      = szVarName;
   pVar->szAlias     = NULL;
   pVar->cType       = cValueType;
   pVar->iUsed       = VU_NOT_USED;
   pVar->pNext       = NULL;
   pVar->iDeclLine   = hb_comp_iLine - 1;

   if( HB_TOUPPER( cValueType ) == 'S' )
   {
#if defined( HB_COMP_DEBUG )
      printf( "\n[hb_compVariableAdd] Variable %s is of Class: %s\n", szVarName, hb_comp_szFromClass );
#endif

      pVar->Extended.pClass = hb_compClassFind( hb_comp_szFromClass );

      if( ! pVar->Extended.pClass )
      {
         hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_CLASS_NOT_FOUND, hb_comp_szFromClass, szVarName );
         pVar->cType = 'O';
      }

      /* Resetting */
      hb_comp_szFromClass = NULL;
   }

   if( hb_comp_iVarScope & VS_PARAMETER )
      pVar->iUsed = VU_INITIALIZED;

   if( hb_comp_iVarScope & VS_MEMVAR )
   {
      USHORT wPos = 0;

#if defined( HB_COMP_DEBUG )
      printf( "\n[hb_compVariableAdd] Adding: %s in Function: %s\n", pVar->szName, pFunc->szName );
#endif

      if( hb_comp_bAutoMemvarAssume || hb_comp_iVarScope == VS_MEMVAR )
      {
         /* add this variable to the list of MEMVAR variables
          */
         if( ! pFunc->pMemvars )
            pFunc->pMemvars = pVar;
         else
         {
            pLastVar = pFunc->pMemvars;

            if( pLastVar->szName == szVarName )
               /* The duplicate var will never be marked as used - avoid redundant warning!
                */
               pVar->iUsed |= VU_USED;

            while( pLastVar->pNext )
            {
               if( pLastVar->szName == szVarName )
                  /* The duplicate var will never be marked as used - avoid redundant warning!
                   */
                  pVar->iUsed |= VU_USED;

               pLastVar = pLastVar->pNext;
            }

            pLastVar->pNext = pVar;
         }

         bUsed = TRUE;
      }

      switch( hb_comp_iVarScope )
      {
         case VS_MEMVAR:
            /* variable declared in MEMVAR statement */
            break;

         case ( VS_PARAMETER | VS_PRIVATE ):
         {
            if( hb_comp_functions.pLast->wParamCount >= 254 )
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_TOOMANY_PARAMS, szVarName, NULL );

            if( ++hb_comp_functions.pLast->wParamNum > hb_comp_functions.pLast->wParamCount )
               hb_comp_functions.pLast->wParamCount = hb_comp_functions.pLast->wParamNum;

            hb_compSymbolAdd( szVarName, &wPos, NULL, SYMF_PUBLIC );

            hb_compGenPCode4( HB_P_PARAMETER, HB_LOBYTE( wPos ), HB_HIBYTE( wPos ), HB_LOBYTE( hb_comp_functions.pLast->wParamNum ), ( BOOL ) 0 );
         }

            if( hb_comp_iWarnings >= 3 )
            {
               PVAR pMemVar = pFunc->pMemvars;

               while( pMemVar )
               {
                  if( pMemVar->szName == pVar->szName )
                     break;
                  else
                     pMemVar = pMemVar->pNext;
               }

               /* Not declared as memvar. */
               if( pMemVar == NULL )
               {
                  /* add this variable to the list of PRIVATE variables. */
                  if( ! pFunc->pPrivates )
                     pFunc->pPrivates = pVar;
                  else
                  {
                     pLastVar = pFunc->pPrivates;

                     while( pLastVar->pNext )
                        pLastVar = pLastVar->pNext;

                     pLastVar->pNext = pVar;
                  }

                  bUsed = TRUE;
#if defined( HB_COMP_DEBUG )
                  printf( "\n[hb_compVariableAdd] Added Private: %s Type %c\n", pVar->szName, pVar->cType );
#endif
               }
            }

            break;

         case VS_PRIVATE:
            hb_compSymbolAdd( szVarName, &wPos, NULL, SYMF_PUBLIC );

            if( hb_comp_iWarnings >= 3 )
            {
               PVAR pMemVar = pFunc->pMemvars;

               while( pMemVar )
               {
                  if( pMemVar->szName == pVar->szName )
                     break;
                  else
                     pMemVar = pMemVar->pNext;
               }

               /* Not declared as memvar. */
               if( pMemVar == NULL )
               {
                  /* add this variable to the list of PRIVATE variables. */
                  if( ! pFunc->pPrivates )
                     pFunc->pPrivates = pVar;
                  else
                  {
                     pLastVar = pFunc->pPrivates;

                     while( pLastVar->pNext )
                        pLastVar = pLastVar->pNext;

                     pLastVar->pNext = pVar;
                  }

                  bUsed = TRUE;
#if defined( HB_COMP_DEBUG )
                  printf( "\n[hb_compVariableAdd]Added Private: %s Type %c\n", pVar->szName, pVar->cType );
#endif
               }
            }

            break;

         case VS_PUBLIC:
            hb_compSymbolAdd( szVarName, &wPos, NULL, SYMF_PUBLIC );

            break;
      }

      if( ! bUsed )
         hb_xfree( pVar );
   }
   else
   {
      switch( hb_comp_iVarScope )
      {
         case VS_LOCAL:
         case VS_PARAMETER:
         {
            USHORT wLocal = 1;

            if( ! pFunc->pLocals )
               pFunc->pLocals = pVar;
            else
            {
               pLastVar = pFunc->pLocals;

               while( pLastVar->pNext )
               {
                  pLastVar = pLastVar->pNext;
                  wLocal++;
               }

               pLastVar->pNext = pVar;
               wLocal++;
            }

            if( hb_comp_iVarScope == VS_PARAMETER )
            {
               ++hb_comp_functions.pLast->wParamCount;
               hb_comp_functions.pLast->bFlags |= FUN_USES_LOCAL_PARAMS;
            }

            if( hb_comp_bDebugInfo && szVarName )
               hb_compGenLocalName( wLocal, szVarName );
         }
         break;

         case VS_STATIC:
            if( ! pFunc->pStatics )
               pFunc->pStatics = pVar;
            else
            {
               pLastVar = pFunc->pStatics;

               while( pLastVar->pNext )
                  pLastVar = pLastVar->pNext;

               pLastVar->pNext = pVar;
            }
            break;

         case VS_FIELD:
            if( ! pFunc->pFields )
               pFunc->pFields = pVar;
            else
            {
               pLastVar = pFunc->pFields;

               while( pLastVar->pNext )
                  pLastVar = pLastVar->pNext;

               pLastVar->pNext = pVar;
            }

            break;

         case VS_GLOBAL:
         case VS_EXTERNGLOBAL:
            if( ! hb_comp_pGlobals )
               hb_comp_pGlobals = pVar;
            else
            {
               pLastVar = hb_comp_pGlobals;

               while( pLastVar->pNext )
                  pLastVar = pLastVar->pNext;

               pLastVar->pNext = pVar;
            }

            if( hb_comp_iVarScope == VS_EXTERNGLOBAL )
               pVar->szAlias = "";

            break;
      }
   }
}

void hb_compGenGlobalName( char * szVarName )
{
   if( hb_comp_bDebugInfo )
   {
      BYTE *   pBuffer;
      int      iVar;
      HB_SIZE  iVarLen = strlen( szVarName );
      int      i;
      PVAR     pVar;

      hb_compGlobalsDefStart();

      iVar     = hb_compVariableGetPos( hb_comp_pGlobals, szVarName );
      pVar     = hb_compVariableFind( hb_comp_pGlobals, iVar );

      pBuffer  = ( BYTE * ) hb_xgrab( iVarLen + 5 );
      i        = 0;

      if( ! pVar->szAlias )
         /* GLOBAL defined in the current module */
         pBuffer[ i++ ] = HB_P_LOCALNAME;
      else
      {
         /* GLOBAL EXTERNAL */
         /* We don't set FUN_USES_STATICS here because HB_P_STATICNAME is
          * used only as a convenience way to not create a new PCODE
          */
         pBuffer[ i++ ] = HB_P_STATICNAME;
         pBuffer[ i++ ] = FALSE; /* currently unused in HVM */
      }
      pBuffer[ i++ ] = HB_LOBYTE( iVar );
      pBuffer[ i++ ] = HB_HIBYTE( iVar );
      HB_MEMCPY( ( BYTE * ) ( &( pBuffer[ i ] ) ), szVarName, ( size_t ) ( iVarLen + 1 ) );
      hb_compGenPCodeN( pBuffer, i + iVarLen + 1, 0 );
      hb_xfree( pBuffer );

      hb_compGlobalsDefEnd();
   }
}

static void hb_compGenLocalName( USHORT wLocal, char * szVarName )
{
   BYTE * pBuffer;

   pBuffer        = ( BYTE * ) hb_xgrab( strlen( szVarName ) + 4 );

   pBuffer[ 0 ]   = HB_P_LOCALNAME;
   pBuffer[ 1 ]   = HB_LOBYTE( wLocal );
   pBuffer[ 2 ]   = HB_HIBYTE( wLocal );

   HB_MEMCPY( ( BYTE * ) ( &( pBuffer[ 3 ] ) ), szVarName, strlen( szVarName ) + 1 );

   hb_compGenPCodeN( pBuffer, strlen( szVarName ) + 4, 0 );

   hb_xfree( pBuffer );
}

void hb_compGenStaticName( char * szVarName )
{
   if( hb_comp_bDebugInfo )
   {
      BYTE        bGlobal = 0;
      PFUNCTION   pFunc;
      BYTE *      pBuffer;
      int         iVar;

      if( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 )
      {
         /* Variable declaration is outside of function/procedure body.
          *             File-wide static variable
          *          */
         hb_compStaticDefStart();
         bGlobal = 1;
      }

      pFunc          = hb_comp_functions.pLast;
      pBuffer        = ( BYTE * ) hb_xgrab( strlen( szVarName ) + 5 );
      iVar           = hb_compStaticGetPos( szVarName, pFunc );

      pBuffer[ 0 ]   = HB_P_STATICNAME;
      pBuffer[ 1 ]   = bGlobal; /* currently unused in HVM */
      pBuffer[ 2 ]   = HB_LOBYTE( iVar );
      pBuffer[ 3 ]   = HB_HIBYTE( iVar );

      HB_MEMCPY( ( BYTE * ) ( &( pBuffer[ 4 ] ) ), szVarName, strlen( szVarName ) + 1 );

      hb_compGenPCodeN( pBuffer, strlen( szVarName ) + 5, 0 );

      hb_xfree( pBuffer );

      if( bGlobal )
         hb_compStaticDefEnd();
   }
}

/* Generate an error if passed variable name cannot be used in macro
 * expression.
 * Only MEMVAR or undeclared (memvar will be assumed) variables can be used.
 */
BOOL hb_compVariableMacroCheck( char * szVarName )
{
   BOOL bValid = FALSE;

   if( hb_compLocalGetPos( szVarName ) > 0 )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
   else if( hb_compStaticGetPos( szVarName, hb_comp_functions.pLast ) > 0 )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
   else if( hb_compFieldGetPos( szVarName, hb_comp_functions.pLast ) > 0 )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
   else if( ! hb_comp_bStartProc )
   {
      if( hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast ) == 0 )
      {
         /* This is not a local MEMVAR
          */
         if( hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst ) > 0 )
            hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
         else if( hb_compStaticGetPos( szVarName, hb_comp_functions.pFirst ) > 0 )
            hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
         else
            bValid = TRUE;    /* undeclared variable */
      }
      else
         bValid = TRUE;
   }
   else
      bValid = TRUE;    /* undeclared variable */

   return bValid;
}

PCOMCLASS hb_compClassAdd( char * szClassName )
{
   PCOMCLASS      pClass;
   PCOMDECLARED   pDeclared;

#if defined( HB_COMP_DEBUG )
   printf( "[hb_compClassAdd] Declaring Class: %s\n", szClassName );
#endif

   if( hb_comp_iWarnings < 3 )
      return NULL;

   if( ( pClass = hb_compClassFind( szClassName ) ) != NULL )
   {
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_DUP_DECLARATION, "Class", szClassName );
      return pClass;
   }

   pClass            = ( PCOMCLASS ) hb_xgrab( sizeof( COMCLASS ) );

   pClass->szName    = szClassName;
   pClass->pMethod   = NULL;
   pClass->pNext     = NULL;

   if( hb_comp_pReleaseClass == NULL )
      hb_comp_pReleaseClass = pClass;

   hb_comp_pLastClass->pNext = pClass;
   hb_comp_pLastClass        = pClass;

   /* Auto declaration for the Class Function. */
   pDeclared                  = hb_compDeclaredAdd( szClassName );
   pDeclared->cType           = 'S';
   pDeclared->Extended.pClass = pClass;

   return pClass;
}

PCOMCLASS hb_compClassFind( char * szClassName )
{
   PCOMCLASS pClass = hb_comp_pFirstClass;

   if( hb_comp_iWarnings < 3 )
      return NULL;

   while( pClass )
   {
      if( pClass->szName == szClassName )
         return pClass;
      else
      {
         if( pClass->pNext )
            pClass = pClass->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

PCOMDECLARED hb_compMethodAdd( PCOMCLASS pClass, char * szMethodName, BOOL bFree )
{
   PCOMDECLARED pMethod;

#if defined( HB_COMP_DEBUG )
   printf( "\n[hb_compMethodAdd] Declaring Method: %s of Class: %s Pointer: %li\n", szMethodName, pClass->szName, pClass );
#endif

   if( hb_comp_iWarnings < 3 )
      return NULL;

   if( ( pMethod = hb_compMethodFind( pClass, szMethodName ) ) != NULL )
   {
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_DUP_DECLARATION, "Method", szMethodName );

      /* Last Declaration override previous declarations */
      if( bFree )
         hb_xfree( szMethodName );
      if( pMethod->cParamTypes )
         hb_xfree( pMethod->cParamTypes );
      pMethod->cParamTypes = NULL;
      pMethod->iParamCount = 0;
      if( pMethod->pParamClasses )
         hb_xfree( pMethod->pParamClasses );
      pMethod->pParamClasses = NULL;

      return pMethod;
   }

   pMethod                 = ( PCOMDECLARED ) hb_xgrab( sizeof( COMDECLARED ) );

   pMethod->szName         = szMethodName;
   pMethod->cType          = ' '; /* Not known yet */
   pMethod->cParamTypes    = NULL;
   pMethod->iParamCount    = 0;
   pMethod->pParamClasses  = NULL;
   pMethod->bFree          = bFree;
   pMethod->pNext          = NULL;

   if( pClass->pMethod == NULL )
      pClass->pMethod = pMethod;
   else
      pClass->pLastMethod->pNext = pMethod;

   pClass->pLastMethod  = pMethod;

   hb_comp_pLastMethod  = pMethod;

   return pMethod;
}

PCOMDECLARED hb_compMethodFind( PCOMCLASS pClass, char * szMethodName )
{
   if( pClass )
   {
      PCOMDECLARED pMethod = pClass->pMethod;

      while( pMethod )
      {
         if( ! strcmp( pMethod->szName, szMethodName ) )
            return pMethod;
         pMethod = pMethod->pNext;
      }
   }

   return NULL;
}

static void hb_compDeclaredInit( void )
{
  #define _DECL static COMDECLARED
   /*
      \x5c -> ByRef    (+60)             '-'  -> NIL
      \x7a -> Optional (+90)             'U'  -> Undefined

      ' '  -> AnyType                    'A'  -> Array                     'B'  -> Array
      'A'  -> Array of AnyType           'a'  -> Array of Arrays           'b'  -> Array of Blocks
      \x7a -> Optional AnyType           \x9b -> Optional Array            \x9c -> Optional Block
      \x94 -> Optional Array of AnyType  \xb5 -> Optional Array of Arrays  \xb6 -> Optional Array of Blocks

      'C'  -> Character/String           'D'  -> Date                      'L'  -> Logical
      'c'  -> Array of Strings           'd'  -> Array of Dates            'l'  -> Array of Logicals
      \x9d -> Optional Character         \x9e -> Optional Date             \xa6 -> Optional Logical
      \xb7 -> Optional Array of Strings  \xb8 -> Optional Array of Dates   \xc0 -> Optional Array of Logicals

      'N'  -> Numeric                    'O'  -> Object                    'S'  -> Class
      'n'  -> Array of Numerics          'o'  -> Array of Objects          's'  -> Array of Classes
      \xa8 -> Optional Numeric           \xa9 -> Optional Object           \xad -> Optional Class
      \xc2 -> Optional Array of Numerics \xc3 -> Optional Array of Objects \xc7 -> Optional Array of Classes
    */

   /* ------------------------------------------------- Standard Functions -------------------------------------------------- */

   /*              Name        Ret  # of Prams  Param Types                                                   Ret Class  Param Classes  Next
                   ----------  ---  ----------  ------------------------------                                ---------  -------------  ------ */
   _DECL s_001 = { "AADD", ' ', 2, ( BYTE * ) "A ", { NULL }, NULL, NULL, FALSE };
   _DECL s_002 = { "ABS", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_001, FALSE };
   _DECL s_003 = { "ACHOICE", 'N', 9, ( BYTE * ) "NNNNA\x7a\x9d\xa8\xa8", { NULL }, NULL, &s_002, FALSE };
   _DECL s_004 = { "ACLONE", 'A', 1, ( BYTE * ) "A", { NULL }, NULL, &s_003, FALSE };
   _DECL s_005 = { "ACOPY", 'A', 5, ( BYTE * ) "AA\xa8\xa8\xa8", { NULL }, NULL, &s_004, FALSE };
   _DECL s_006 = { "ADEL", 'A', 3, ( BYTE * ) "AN\xa6", { NULL }, NULL, &s_005, FALSE };
   _DECL s_007 = { "ADIR", 'N', 6, ( BYTE * ) "\x9d\x9b\x9b\x9b\x9b\x9b", { NULL }, NULL, &s_006, FALSE };
   _DECL s_008 = { "AEVAL", 'A', 4, ( BYTE * ) "AB\xa8\xa8", { NULL }, NULL, &s_007, FALSE };
   _DECL s_009 = { "AFIELDS", 'N', 4, ( BYTE * ) "A\x9b\x9b\x9b", { NULL }, NULL, &s_008, FALSE };
   _DECL s_010 = { "AFILL", 'A', 4, ( BYTE * ) "A \xa8\xa8", { NULL }, NULL, &s_009, FALSE };
   _DECL s_011 = { "AINS", 'A', 4, ( BYTE * ) "AN\xa6\x7a", { NULL }, NULL, &s_010, FALSE };
   _DECL s_012 = { "ALERT", 'N', 4, ( BYTE * ) "C\x9b\x9d\xa8", { NULL }, NULL, &s_011, FALSE };
   _DECL s_013 = { "ALIAS", 'C', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_012, FALSE };
   _DECL s_014 = { "ALLTRIM", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_013, FALSE };
   _DECL s_015 = { "AMPM", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_014, FALSE };
   _DECL s_016 = { "ARRAY", 'A', 3, ( BYTE * ) "N\xa8\xa8", { NULL }, NULL, &s_015, FALSE };
   _DECL s_017 = { "ASC", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_016, FALSE };
   _DECL s_018 = { "ASCAN", 'N', 4, ( BYTE * ) "A\xa7\xa8\xa8", { NULL }, NULL, &s_017, FALSE };
   _DECL s_019 = { "ASIZE", 'A', 2, ( BYTE * ) "AN", { NULL }, NULL, &s_018, FALSE };
   _DECL s_020 = { "ASORT", 'A', 4, ( BYTE * ) "A\xa8\xa8\x9c", { NULL }, NULL, &s_019, FALSE };
   _DECL s_021 = { "AT", 'N', 4, ( BYTE * ) "CC\xa8\xa8", { NULL }, NULL, &s_020, FALSE };
   _DECL s_022 = { "ATAIL", ' ', 1, ( BYTE * ) "A", { NULL }, NULL, &s_021, FALSE };
   _DECL s_023 = { "BIN2I", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_022, FALSE };
   _DECL s_024 = { "BIN2L", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_023, FALSE };
   _DECL s_025 = { "BIN2U", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_024, FALSE };
   _DECL s_026 = { "BIN2W", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_025, FALSE };
   _DECL s_027 = { "BOF", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_026, FALSE };
   _DECL s_028 = { "BROWSE", '-', 4, ( BYTE * ) "\xa8\xa8\xa8\xa8", { NULL }, NULL, &s_027, FALSE };
   _DECL s_029 = { "CDOW", 'C', 1, ( BYTE * ) "D", { NULL }, NULL, &s_028, FALSE };
   _DECL s_030 = { "CHR", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_029, FALSE };
   _DECL s_031 = { "CMONTH", 'C', 1, ( BYTE * ) "D", { NULL }, NULL, &s_030, FALSE };
   _DECL s_032 = { "COL", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_031, FALSE };
   _DECL s_033 = { "CTOD", 'D', 1, ( BYTE * ) "C", { NULL }, NULL, &s_032, FALSE };
   _DECL s_034 = { "CURDIR", 'C', 1, ( BYTE * ) "\x9d", { NULL }, NULL, &s_033, FALSE };
   _DECL s_035 = { "DATE", 'D', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_034, FALSE };
   _DECL s_036 = { "DAY", 'N', 1, ( BYTE * ) "D", { NULL }, NULL, &s_035, FALSE };
   _DECL s_037 = { "DAYS", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_036, FALSE };
   _DECL s_038 = { "DBAPPEND", '-', 1, ( BYTE * ) "\xa6", { NULL }, NULL, &s_037, FALSE };
   _DECL s_039 = { "DBCLEARFILTER", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_038, FALSE };
   _DECL s_040 = { "DBCLEARINDEX", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_039, FALSE };
   _DECL s_041 = { "DBCLEARRELATION", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_040, FALSE };
   _DECL s_042 = { "DBCLOSEALL", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_041, FALSE };
   _DECL s_043 = { "DBCLOSEAREA", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_042, FALSE };
   _DECL s_044 = { "DBCOMMIT", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_043, FALSE };
   _DECL s_045 = { "DBCOMMITALL", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_044, FALSE };
   _DECL s_046 = { "DBCREATE", '-', 5, ( BYTE * ) "CA\x9d\xa6\x9d", { NULL }, NULL, &s_045, FALSE };
   _DECL s_047 = { "DBCREATEINDEX", '-', 5, ( BYTE * ) "C B\xa6\xa6", { NULL }, NULL, &s_046, FALSE };
   _DECL s_048 = { "DBDELETE", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_047, FALSE };
   _DECL s_049 = { "DBEDIT", '-', 12, ( BYTE * ) "\xa8\xa8\xa8\xa8\x7a\x7a\x7a\x7a\x7a\x7a\x7a\x7a", { NULL }, NULL, &s_048, FALSE };
   _DECL s_050 = { "DBEVAL", '-', 6, ( BYTE * ) "B\x9c\x9cNNL", { NULL }, NULL, &s_049, FALSE };
   _DECL s_051 = { "DBF", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_050, FALSE };
   _DECL s_052 = { "DBFILTER", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_051, FALSE };
   _DECL s_053 = { "DBGOBOTTOM", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_052, FALSE };
   _DECL s_054 = { "DBGOTO", '-', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_053, FALSE };
   _DECL s_055 = { "DBGOTOP", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_054, FALSE };
   _DECL s_056 = { "DBRECALL", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_055, FALSE };
   _DECL s_057 = { "DBREINDEX", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_056, FALSE };
   _DECL s_058 = { "DBRELATION", ' ', 1, ( BYTE * ) "N", { NULL }, NULL, &s_057, FALSE };
   _DECL s_059 = { "DBRLOCK", 'L', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_058, FALSE };
   _DECL s_060 = { "DBRLOCKLIST", 'A', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_059, FALSE };
   _DECL s_061 = { "DBRSELECT", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_060, FALSE };
   _DECL s_062 = { "DBRUNLOCK", '-', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_061, FALSE };
   _DECL s_063 = { "DBSEEK", 'L', 3, ( BYTE * ) " \xa6\xa6", { NULL }, NULL, &s_062, FALSE };
   _DECL s_064 = { "DBSELECTAREA", '-', 1, ( BYTE * ) " ", { NULL }, NULL, &s_063, FALSE };
   _DECL s_065 = { "DBSETDRIVER", 'C', 1, ( BYTE * ) "\x9d", { NULL }, NULL, &s_064, FALSE };
   _DECL s_066 = { "DBSETFILTER", '-', 2, ( BYTE * ) "B\x9d", { NULL }, NULL, &s_065, FALSE };
   _DECL s_067 = { "DBSETINDEX", '-', 1, ( BYTE * ) "C", { NULL }, NULL, &s_066, FALSE };
   _DECL s_068 = { "DBSETORDER", '-', 1, ( BYTE * ) "N", { NULL }, NULL, &s_067, FALSE };
   _DECL s_069 = { "DBSETRELATION", '-', 3, ( BYTE * ) " BC", { NULL }, NULL, &s_068, FALSE };
   _DECL s_070 = { "DBSKIP", '-', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_069, FALSE };
   _DECL s_071 = { "DBSTRUCT", 'A', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_070, FALSE };
   _DECL s_072 = { "DBUNLOCK", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_071, FALSE };
   _DECL s_073 = { "DBUNLOCKALL", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_072, FALSE };
   _DECL s_074 = { "DBUSEAREA", '-', 7, ( BYTE * ) "\xa6\x9d" "C\x9d\xa6\xa6\x9d", { NULL }, NULL, &s_073, FALSE };
   _DECL s_075 = { "DELETED", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_074, FALSE };
   _DECL s_076 = { "DESCEND", ' ', 1, ( BYTE * ) " ", { NULL }, NULL, &s_075, FALSE };
   _DECL s_077 = { "DEVOUT", '-', 2, ( BYTE * ) " \x9d", { NULL }, NULL, &s_076, FALSE };
   _DECL s_078 = { "DEVOUTPICT", '-', 3, ( BYTE * ) " C\x9d", { NULL }, NULL, &s_077, FALSE };
   _DECL s_079 = { "DEVPOS", '-', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_078, FALSE };
   _DECL s_080 = { "DIRECTORY", 'A', 3, ( BYTE * ) "\x9d\x9d\xa6", { NULL }, NULL, &s_079, FALSE };
   _DECL s_081 = { "DIRCHANGE", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_080, FALSE };
   _DECL s_082 = { "DIRREMOVE", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_081, FALSE };
   _DECL s_083 = { "DISKSPACE", 'N', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_082, FALSE };
   _DECL s_084 = { "DISPBEGIN", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_083, FALSE };
   _DECL s_085 = { "DISPCOUNT", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_084, FALSE };
   _DECL s_086 = { "DISPEND", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_085, FALSE };
   _DECL s_087 = { "DISPOUT", '-', 2, ( BYTE * ) " \x9d", { NULL }, NULL, &s_086, FALSE };
   _DECL s_088 = { "DOW", 'N', 1, ( BYTE * ) "D", { NULL }, NULL, &s_087, FALSE };
   _DECL s_089 = { "DTOC", 'C', 1, ( BYTE * ) "D", { NULL }, NULL, &s_088, FALSE };
   _DECL s_090 = { "DTOS", 'C', 1, ( BYTE * ) "D", { NULL }, NULL, &s_089, FALSE };
   _DECL s_091 = { "ELAPTIME", 'C', 2, ( BYTE * ) "CC", { NULL }, NULL, &s_090, FALSE };
   _DECL s_092 = { "EMPTY", 'L', 1, ( BYTE * ) " ", { NULL }, NULL, &s_091, FALSE };
   _DECL s_093 = { "EOF", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_092, FALSE };
   _DECL s_094 = { "ERRORNEW", 'S', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_093, FALSE };
   _DECL s_095 = { "EVAL", ' ', 3, ( BYTE * ) "B\x7a\x7a", { NULL }, NULL, &s_094, FALSE };
   _DECL s_096 = { "EXP", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_095, FALSE };
   _DECL s_097 = { "FCLOSE", 'L', 1, ( BYTE * ) "N", { NULL }, NULL, &s_096, FALSE };
   _DECL s_098 = { "FCOUNT", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_097, FALSE };
   _DECL s_099 = { "FCREATE", 'N', 2, ( BYTE * ) "C\xa8", { NULL }, NULL, &s_098, FALSE };
   _DECL s_100 = { "FERASE", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_099, FALSE };
   _DECL s_101 = { "FERROR", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_100, FALSE };
   _DECL s_102 = { "FIELD", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_101, FALSE };
   _DECL s_103 = { "FIELDBLOCK", 'B', 1, ( BYTE * ) "C", { NULL }, NULL, &s_102, FALSE };
   _DECL s_104 = { "FIELDGET", ' ', 1, ( BYTE * ) "N", { NULL }, NULL, &s_103, FALSE };
   _DECL s_105 = { "FIELDNAME", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_104, FALSE };
   _DECL s_106 = { "FIELDPOS", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_105, FALSE };
   _DECL s_107 = { "FIELDPUT", ' ', 2, ( BYTE * ) "N ", { NULL }, NULL, &s_106, FALSE };
   _DECL s_108 = { "FIELDWBLOCK", 'B', 2, ( BYTE * ) "CN", { NULL }, NULL, &s_107, FALSE };
   _DECL s_109 = { "FILE", 'L', 1, ( BYTE * ) "C", { NULL }, NULL, &s_108, FALSE };
   _DECL s_110 = { "FLOCK", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_109, FALSE };
   _DECL s_111 = { "FOPEN", 'N', 2, ( BYTE * ) "C\xa8", { NULL }, NULL, &s_110, FALSE };
   _DECL s_112 = { "FOUND", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_111, FALSE };
   _DECL s_113 = { "FREAD", 'N', 3, ( BYTE * ) "N\x5cN", { NULL }, NULL, &s_112, FALSE };
   _DECL s_114 = { "FREADSTR", 'C', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_113, FALSE };
   _DECL s_115 = { "FRENAME", 'N', 2, ( BYTE * ) "CC", { NULL }, NULL, &s_114, FALSE };
   _DECL s_116 = { "FSEEK", 'N', 3, ( BYTE * ) "NN\xa8", { NULL }, NULL, &s_115, FALSE };
   _DECL s_117 = { "FWRITE", 'N', 3, ( BYTE * ) "NC\xa8", { NULL }, NULL, &s_116, FALSE };
   _DECL s_118 = { "GETACTIVE", '-', 1, ( BYTE * ) "O", { NULL }, NULL, &s_117, FALSE };
   _DECL s_119 = { "GETAPPLYKEY", '-', 2, ( BYTE * ) "ON", { NULL }, NULL, &s_118, FALSE };
   _DECL s_120 = { "GETDOSETKEY", '-', 2, ( BYTE * ) "BO", { NULL }, NULL, &s_119, FALSE };
   _DECL s_121 = { "GETENV", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_120, FALSE };
   _DECL s_122 = { "GETNEW", 'O', 6, ( BYTE * ) "\xa8\xa8\x9c\x9d\x9d\x9d", { NULL }, NULL, &s_121, FALSE };
   _DECL s_123 = { "GETPREVALIDATE", 'L', 1, ( BYTE * ) "O", { NULL }, NULL, &s_122, FALSE };
   _DECL s_124 = { "GETPOSTVALIDATE", 'L', 1, ( BYTE * ) "O", { NULL }, NULL, &s_123, FALSE };
   _DECL s_125 = { "GETREADER", '-', 1, ( BYTE * ) "O", { NULL }, NULL, &s_124, FALSE };
   _DECL s_126 = { "HARDCR", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_125, FALSE };
   _DECL s_127 = { "HB_ANSITOOEM", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_126, FALSE };
   _DECL s_128 = { "HB_DISKSPACE", 'N', 2, ( BYTE * ) "\x9d\xa8", { NULL }, NULL, &s_127, FALSE };
   _DECL s_129 = { "HB_FEOF", 'L', 1, ( BYTE * ) "N", { NULL }, NULL, &s_128, FALSE };
   _DECL s_130 = { "HB_OEMTOANSI", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_129, FALSE };
   _DECL s_131 = { "HEADER", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_130, FALSE };
   _DECL s_132 = { "I2BIN", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_131, FALSE };
   _DECL s_133 = { "IF", ' ', 3, ( BYTE * ) "L  ", { NULL }, NULL, &s_132, FALSE };
   _DECL s_134 = { "INDEXEXT", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_133, FALSE };
   _DECL s_135 = { "INDEXKEY", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_134, FALSE };
   _DECL s_136 = { "INDEXORD", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_135, FALSE };
   _DECL s_137 = { "INKEY", 'N', 2, ( BYTE * ) "\xa8\xa8", { NULL }, NULL, &s_136, FALSE };
   _DECL s_138 = { "INT", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_137, FALSE };
   _DECL s_139 = { "ISAFFIRM", 'L', 1, ( BYTE * ) "C", { NULL }, NULL, &s_138, FALSE };
   _DECL s_140 = { "ISALPHA", 'L', 1, ( BYTE * ) "C", { NULL }, NULL, &s_139, FALSE };
   _DECL s_141 = { "ISDIGIT", 'L', 1, ( BYTE * ) "C", { NULL }, NULL, &s_140, FALSE };
   _DECL s_142 = { "ISDISK", 'L', 1, ( BYTE * ) "C", { NULL }, NULL, &s_141, FALSE };
   _DECL s_143 = { "ISLOWER", 'L', 1, ( BYTE * ) "C", { NULL }, NULL, &s_142, FALSE };
   _DECL s_144 = { "ISNEGATIVE", 'L', 1, ( BYTE * ) "C", { NULL }, NULL, &s_143, FALSE };
   _DECL s_145 = { "ISUPPER", 'L', 1, ( BYTE * ) "C", { NULL }, NULL, &s_144, FALSE };
   _DECL s_146 = { "L2BIN", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_145, FALSE };
   _DECL s_147 = { "LASTKEY", 'N', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_146, FALSE };
   _DECL s_148 = { "LASTREC", 'N', 1, ( BYTE * ) " ", { NULL }, NULL, &s_147, FALSE };
   _DECL s_149 = { "LEFT", 'C', 2, ( BYTE * ) "CN", { NULL }, NULL, &s_148, FALSE };
   _DECL s_150 = { "LEN", 'N', 1, ( BYTE * ) " ", { NULL }, NULL, &s_149, FALSE };
   _DECL s_151 = { "LOG", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_150, FALSE };
   _DECL s_152 = { "LOWER", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_151, FALSE };
   _DECL s_153 = { "LTRIM", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_152, FALSE };
   _DECL s_154 = { "LUPDATE", 'D', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_153, FALSE };
   _DECL s_155 = { "MAKEDIR", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_154, FALSE };
   _DECL s_156 = { "MAX", ' ', 2, ( BYTE * ) "  ", { NULL }, NULL, &s_155, FALSE };
   _DECL s_157 = { "MAXCOL", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_156, FALSE };
   _DECL s_158 = { "MAXROW", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_157, FALSE };
   _DECL s_159 = { "MCOL", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_158, FALSE };
   _DECL s_160 = { "MEMOEDIT", 'C', 13, ( BYTE * ) "\x9d\xa8\xa8\xa8\xa8\xa6\x9d\xa8\xa8\xa8\xa8\xa8\xa8", { NULL }, NULL, &s_159, FALSE };
   _DECL s_161 = { "MEMOTRAN", 'C', 3, ( BYTE * ) "C\x9d\x9d", { NULL }, NULL, &s_160, FALSE };
   _DECL s_162 = { "MEMOLINE", 'C', 5, ( BYTE * ) "C\xa8\xa8\xa8\xa6", { NULL }, NULL, &s_161, FALSE };
   _DECL s_163 = { "MEMORY", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_162, FALSE };
   _DECL s_164 = { "MEMOREAD", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_163, FALSE };
   _DECL s_165 = { "MEMOTRAN", 'C', 3, ( BYTE * ) "C\x9d\x9d", { NULL }, NULL, &s_164, FALSE };
   _DECL s_166 = { "MEMOWRIT", 'L', 2, ( BYTE * ) "CC", { NULL }, NULL, &s_165, FALSE };
   _DECL s_167 = { "MEMVARBLOCK", 'B', 1, ( BYTE * ) "C", { NULL }, NULL, &s_166, FALSE };
   _DECL s_168 = { "MIN", ' ', 2, ( BYTE * ) "  ", { NULL }, NULL, &s_167, FALSE };
   _DECL s_169 = { "MLCOUNT", 'N', 4, ( BYTE * ) "C\xa8\xa8\xa6", { NULL }, NULL, &s_168, FALSE };
   _DECL s_170 = { "MLCTOPOS", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_169, FALSE };
   _DECL s_171 = { "MLPOS", 'N', 5, ( BYTE * ) "CNN\xa8\xa6", { NULL }, NULL, &s_170, FALSE };
   _DECL s_172 = { "MOD", 'N', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_171, FALSE };
   _DECL s_173 = { "MONTH", 'N', 1, ( BYTE * ) "D", { NULL }, NULL, &s_172, FALSE };
   _DECL s_174 = { "MPOSTOLC", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_173, FALSE };
   _DECL s_175 = { "MROW", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_174, FALSE };
   _DECL s_176 = { "NATIONMSG", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_175, FALSE };
   _DECL s_177 = { "NETERR", 'L', 1, ( BYTE * ) "\xa6", { NULL }, NULL, &s_176, FALSE };
   _DECL s_178 = { "NETNAME", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_177, FALSE };
   _DECL s_179 = { "NEXTKEY", 'N', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_178, FALSE };
   _DECL s_180 = { "ORDBAGEXT", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_179, FALSE };
   _DECL s_181 = { "ORDBAGNAME", 'C', 1, ( BYTE * ) " ", { NULL }, NULL, &s_180, FALSE };
   _DECL s_182 = { "ORDCREATE", '-', 5, ( BYTE * ) "C\x9d \x9c\xa6", { NULL }, NULL, &s_181, FALSE };
   _DECL s_183 = { "ORDDESTROY", '-', 2, ( BYTE * ) "C\x9d", { NULL }, NULL, &s_182, FALSE };
   _DECL s_184 = { "ORDFOR", 'C', 2, ( BYTE * ) " \x9d", { NULL }, NULL, &s_183, FALSE };
   _DECL s_185 = { "ORDKEY", 'C', 2, ( BYTE * ) " \x9d", { NULL }, NULL, &s_184, FALSE };
   _DECL s_186 = { "ORDLISTADD", '-', 2, ( BYTE * ) "C\x9d", { NULL }, NULL, &s_185, FALSE };
   _DECL s_187 = { "ORDLISTCLEAR", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_186, FALSE };
   _DECL s_188 = { "ORDLISTREBUILD", '-', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_187, FALSE };
   _DECL s_189 = { "ORDNAME", 'C', 2, ( BYTE * ) "N\x9d", { NULL }, NULL, &s_188, FALSE };
   _DECL s_190 = { "ORDNUMBER", 'N', 2, ( BYTE * ) "C\x9d", { NULL }, NULL, &s_189, FALSE };
   _DECL s_191 = { "ORDSETFOCUS", 'C', 2, ( BYTE * ) "\x7a\x9d", { NULL }, NULL, &s_190, FALSE };
   _DECL s_192 = { "OS", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_191, FALSE };
   _DECL s_193 = { "OUTERR", '-', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_192, FALSE };
   _DECL s_194 = { "OUTSTD", '-', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_193, FALSE };
   _DECL s_195 = { "PADC", 'C', 3, ( BYTE * ) " N\x9d", { NULL }, NULL, &s_194, FALSE };
   _DECL s_196 = { "PADL", 'C', 3, ( BYTE * ) " N\x9d", { NULL }, NULL, &s_195, FALSE };
   _DECL s_197 = { "PADR", 'C', 3, ( BYTE * ) " N\x9d", { NULL }, NULL, &s_196, FALSE };
   _DECL s_198 = { "PCOL", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_197, FALSE };
   _DECL s_199 = { "PCOUNT", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_198, FALSE };
   _DECL s_200 = { "PROCFILE", 'C', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_199, FALSE };
   _DECL s_201 = { "PROCLINE", 'N', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_200, FALSE };
   _DECL s_202 = { "PROCNAME", 'N', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_201, FALSE };
   _DECL s_203 = { "PROW", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_202, FALSE };
   _DECL s_204 = { "RAT", 'N', 2, ( BYTE * ) "CC", { NULL }, NULL, &s_203, FALSE };
   _DECL s_205 = { "RDDLIST", 'A', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_204, FALSE };
   _DECL s_206 = { "RDDNAME", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_205, FALSE };
   _DECL s_207 = { "RDDSETDEFAULT", 'C', 1, ( BYTE * ) "\x9d", { NULL }, NULL, &s_206, FALSE };
   _DECL s_208 = { "READEXIT", 'L', 1, ( BYTE * ) "\xa6", { NULL }, NULL, &s_207, FALSE };
   _DECL s_209 = { "READUPDATED", 'L', 1, ( BYTE * ) "\xa6", { NULL }, NULL, &s_208, FALSE };
   _DECL s_210 = { "READINSERT", 'L', 1, ( BYTE * ) "\xa6", { NULL }, NULL, &s_209, FALSE };
   _DECL s_211 = { "READKEY", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_210, FALSE };
   _DECL s_212 = { "READFORMAT", 'B', 1, ( BYTE * ) "B", { NULL }, NULL, &s_211, FALSE };
   _DECL s_213 = { "READKILL", 'L', 1, ( BYTE * ) "\xa6", { NULL }, NULL, &s_212, FALSE };
   _DECL s_214 = { "READMODAL", 'L', 2, ( BYTE * ) "A\xa8", { NULL }, NULL, &s_213, FALSE };
   _DECL s_215 = { "READUPDATED", 'L', 1, ( BYTE * ) "\xa6", { NULL }, NULL, &s_214, FALSE };
   _DECL s_216 = { "READVAR", 'C', 1, ( BYTE * ) "\x9d", { NULL }, NULL, &s_215, FALSE };
   _DECL s_217 = { "RECCOUNT", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_216, FALSE };
   _DECL s_218 = { "RECNO", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_217, FALSE };
   _DECL s_219 = { "RECSIZE", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_218, FALSE };
   _DECL s_220 = { "REPLICATE", 'C', 2, ( BYTE * ) "CN", { NULL }, NULL, &s_219, FALSE };
   _DECL s_221 = { "RESTSCREEN", '-', 5, ( BYTE * ) "\xa8\xa8\xa8\xa8\x9d", { NULL }, NULL, &s_220, FALSE };
   _DECL s_222 = { "RIGHT", 'C', 2, ( BYTE * ) "CN", { NULL }, NULL, &s_221, FALSE };
   _DECL s_223 = { "RLOCK", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_222, FALSE };
   _DECL s_224 = { "ROUND", 'N', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_223, FALSE };
   _DECL s_225 = { "ROW", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_224, FALSE };
   _DECL s_226 = { "RTRIM", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_225, FALSE };
   _DECL s_227 = { "SAVESCREEN", '-', 4, ( BYTE * ) "\xa8\xa8\xa8\xa8", { NULL }, NULL, &s_226, FALSE };
   _DECL s_228 = { "SCROLL", '-', 6, ( BYTE * ) "\xa8\xa8\xa8\xa8\xa8\xa8", { NULL }, NULL, &s_227, FALSE };
   _DECL s_229 = { "SECONDS", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_228, FALSE };
   _DECL s_230 = { "SECS", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_229, FALSE };
   _DECL s_231 = { "SELECT", 'N', 1, ( BYTE * ) "\x9d", { NULL }, NULL, &s_230, FALSE };
   _DECL s_232 = { "SET", ' ', 3, ( BYTE * ) "N\x7a\xa6", { NULL }, NULL, &s_231, FALSE };
   _DECL s_233 = { "SETCOLOR", 'C', 1, ( BYTE * ) "\x9d", { NULL }, NULL, &s_232, FALSE };
   _DECL s_234 = { "SETCURSOR", 'N', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_233, FALSE };
   _DECL s_235 = { "SETKEY", ' ', 3, ( BYTE * ) "N\x9c\x9c", { NULL }, NULL, &s_234, FALSE };
   _DECL s_236 = { "SETMODE", 'L', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_235, FALSE };
   _DECL s_237 = { "SETPOS", '-', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_236, FALSE };
   _DECL s_238 = { "SETPRC", '-', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_237, FALSE };
   _DECL s_239 = { "SETTYPEAHEAD", '-', 1, ( BYTE * ) "N", { NULL }, NULL, &s_238, FALSE };
   _DECL s_240 = { "SPACE", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_239, FALSE };
   _DECL s_241 = { "SQRT", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_240, FALSE };
   _DECL s_242 = { "STR", 'C', 3, ( BYTE * ) "N\xa8\xa8", { NULL }, NULL, &s_241, FALSE };
   _DECL s_243 = { "STRTRAN", 'C', 5, ( BYTE * ) "CC\x9d\xa8\xa8", { NULL }, NULL, &s_242, FALSE };
   _DECL s_244 = { "STRZERO", 'C', 3, ( BYTE * ) "N\xa8\xa8", { NULL }, NULL, &s_243, FALSE };
   _DECL s_245 = { "STUFF", 'C', 4, ( BYTE * ) "CNNC", { NULL }, NULL, &s_244, FALSE };
   _DECL s_246 = { "SUBSTR", 'C', 3, ( BYTE * ) "CN\xa8", { NULL }, NULL, &s_245, FALSE };
   _DECL s_247 = { "TBROWSENEW", 'O', 4, ( BYTE * ) "NNNN", { NULL }, NULL, &s_246, FALSE };
   _DECL s_248 = { "TBROWSEDB", 'O', 4, ( BYTE * ) "NNNN", { NULL }, NULL, &s_247, FALSE };
   _DECL s_249 = { "TBCOLUMNNEW", 'O', 2, ( BYTE * ) "CB", { NULL }, NULL, &s_248, FALSE };
   _DECL s_250 = { "TIME", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_249, FALSE };
   _DECL s_251 = { "TONE", '-', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_250, FALSE };
   _DECL s_252 = { "TRANSFORM", 'C', 2, ( BYTE * ) " C", { NULL }, NULL, &s_251, FALSE };
   _DECL s_253 = { "TRIM", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_252, FALSE };
   _DECL s_254 = { "TYPE", 'C', 2, ( BYTE * ) " N", { NULL }, NULL, &s_253, FALSE };
   _DECL s_255 = { "U2BIN", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_254, FALSE };
   _DECL s_256 = { "UPDATED", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_255, FALSE };
   _DECL s_257 = { "UPPER", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_256, FALSE };
   _DECL s_258 = { "USED", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_257, FALSE };
   _DECL s_259 = { "VAL", 'N', 1, ( BYTE * ) "C", { NULL }, NULL, &s_258, FALSE };
   _DECL s_260 = { "VALTYPE", ' ', 1, ( BYTE * ) " ", { NULL }, NULL, &s_259, FALSE };
   _DECL s_261 = { "VERSION", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_260, FALSE };
   _DECL s_262 = { "W2BIN", 'C', 1, ( BYTE * ) "N", { NULL }, NULL, &s_261, FALSE };
   _DECL s_263 = { "WORD", 'N', 1, ( BYTE * ) "N", { NULL }, NULL, &s_262, FALSE };
   _DECL s_264 = { "HB_FNAMESPLIT", '-', 5, ( BYTE * ) "C\x5c\x5c\x5c\x5c", { NULL }, NULL, &s_263, FALSE };
   _DECL s_265 = { "HB_FNAMEMERGE", 'C', 4, ( BYTE * ) "CCCC", { NULL }, NULL, &s_264, FALSE };

   /* TODO: Rest of Standard Functions. */

   /* -------------------------------------------------- Standard Classes --------------------------------------------------- */

   static COMCLASS   s_ERROR     = { "ERROR", NULL, NULL, NULL };
   static COMCLASS   s_GET       = { "GET", NULL, NULL, &s_ERROR };
   static COMCLASS   s_TBCOLUMN  = { "TBCOLUMN", NULL, NULL, &s_GET };
   static COMCLASS   s_TBROWSE   = { "TBROWSE", NULL, NULL, &s_TBCOLUMN };

   /*       Name     Ret  # of Prams  Param Types   Ret Class  Param Classes  Next
      ---------------  ---  ----------  --------------------  ---------  -------------  --------------- */
   _DECL s_ERROR_01              = { "ARGS", 'A', 0, ( BYTE * ) NULL, { NULL }, NULL, NULL, FALSE };
   _DECL s_ERROR_02              = { "CANDEFAULT", 'B', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_01, FALSE };
   _DECL s_ERROR_03              = { "CANRETRY", 'B', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_02, FALSE };
   _DECL s_ERROR_04              = { "CANSUBSTITUTE", 'B', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_03, FALSE };
   _DECL s_ERROR_05              = { "CARGO", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_04, FALSE };
   _DECL s_ERROR_06              = { "DESCRIPTION", 'S', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_05, FALSE };
   _DECL s_ERROR_07              = { "FILENAME", 'S', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_06, FALSE };
   _DECL s_ERROR_08              = { "GENCODE", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_07, FALSE };
   _DECL s_ERROR_09              = { "OPERATION", 'S', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_08, FALSE };
   _DECL s_ERROR_10              = { "OSCODE", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_09, FALSE };
   _DECL s_ERROR_11              = { "SEVERITY", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_10, FALSE };
   _DECL s_ERROR_12              = { "SUBCODE", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_11, FALSE };
   _DECL s_ERROR_13              = { "SUBSYSTEM", 'S', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_12, FALSE };
   _DECL s_ERROR_14              = { "TRIES", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_ERROR_13, FALSE };

   /*       Name                             Ret  # of Prams  Param Types   Ret Class  Param Classes  Next
      ---------------                          ---  ----------  --------------------  ---------  -------------  --------------- */
   _DECL s_GET_01                = { "ASSIGN", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, NULL, FALSE };
   _DECL s_GET_02                = { "COLORDISP", 'S', 1, ( BYTE * ) "\x9d", { &s_GET }, NULL, &s_GET_01, FALSE };
   _DECL s_GET_03                = { "DISPLAY", 'S', 1, ( BYTE * ) "\xa6", { &s_GET }, NULL, &s_GET_02, FALSE };
   _DECL s_GET_04                = { "KILLFOCUS", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_03, FALSE };
   _DECL s_GET_05                = { "PARSEPICT", 'C', 1, ( BYTE * ) "C", { NULL }, NULL, &s_GET_04, FALSE };
   _DECL s_GET_06                = { "RESET", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_05, FALSE };
   _DECL s_GET_07                = { "SETFOCUS", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_06, FALSE };
   _DECL s_GET_08                = { "UNDO", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_07, FALSE };
   _DECL s_GET_09                = { "UNTRANSFORM", 'S', 1, ( BYTE * ) "\x9d", { &s_GET }, NULL, &s_GET_08, FALSE };
   _DECL s_GET_10                = { "UPDATEBUFFER", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_09, FALSE };
   _DECL s_GET_11                = { "VARGET", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_GET_10, FALSE };
   _DECL s_GET_12                = { "VARPUT", ' ', 2, ( BYTE * ) " \xa6", { NULL }, NULL, &s_GET_11, FALSE };

   _DECL s_GET_13                = { "END", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_12, FALSE };
   _DECL s_GET_14                = { "HOME", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_13, FALSE };
   _DECL s_GET_15                = { "LEFT", 'S', 1, ( BYTE * ) "\xa6", { &s_GET }, NULL, &s_GET_14, FALSE };
   _DECL s_GET_16                = { "RIGHT", 'S', 1, ( BYTE * ) "\xa6", { &s_GET }, NULL, &s_GET_15, FALSE };
   _DECL s_GET_17                = { "TODECPOS", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_16, FALSE };
   _DECL s_GET_18                = { "WORDLEFT", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_17, FALSE };
   _DECL s_GET_19                = { "WORDRIGHT", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_18, FALSE };

   _DECL s_GET_20                = { "BACKSPACE", 'S', 1, ( BYTE * ) "\xa6", { &s_GET }, NULL, &s_GET_19, FALSE };
   _DECL s_GET_21                = { "DELETE", 'S', 1, ( BYTE * ) "\xa6", { &s_GET }, NULL, &s_GET_20, FALSE };
   _DECL s_GET_22                = { "DELEND", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_21, FALSE };
   _DECL s_GET_23                = { "DELLEFT", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_22, FALSE };
   _DECL s_GET_24                = { "DELRIGHT", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_23, FALSE };
   _DECL s_GET_25                = { "DELWORDLEFT", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_24, FALSE };
   _DECL s_GET_26                = { "DELWORDRIGHT", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_25, FALSE };

   _DECL s_GET_27                = { "INSERT", 'S', 1, ( BYTE * ) "C", { &s_GET }, NULL, &s_GET_26, FALSE };
   _DECL s_GET_28                = { "OVERSTRIKE", 'S', 1, ( BYTE * ) "C", { &s_GET }, NULL, &s_GET_27, FALSE };

   _DECL s_GET_29                = { "DELETEALL", 'S', 0, ( BYTE * ) NULL, { &s_GET }, NULL, &s_GET_28, FALSE };
   _DECL s_GET_30                = { "ISEDITABLE", 'L', 1, ( BYTE * ) "N", { NULL }, NULL, &s_GET_29, FALSE };
   _DECL s_GET_31                = { "INPUT", 'C', 0, ( BYTE * ) "C", { NULL }, NULL, &s_GET_30, FALSE };
   _DECL s_GET_32                = { "PUTMASK", 'C', 2, ( BYTE * ) "CL", { NULL }, NULL, &s_GET_31, FALSE };
   _DECL s_GET_33                = { "HASSCROLL", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_GET_32, FALSE };
   _DECL s_GET_34                = { "FIRSTEDITABLE", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_GET_33, FALSE };
   _DECL s_GET_35                = { "LASTEDITABLE", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_GET_34, FALSE };

   /*       Name     Ret  # of Prams  Param Types   Ret Class  Param Classes  Next
      ---------------  ---  ----------  --------------------  ---------  -------------  --------------- */
   _DECL s_TBCOLUMN_01           = { "BLOCK", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, NULL, FALSE };
   _DECL s_TBCOLUMN_02           = { "CARGO", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_01, FALSE };
   _DECL s_TBCOLUMN_03           = { "COLORBLOCK", 'A', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_02, FALSE };
   _DECL s_TBCOLUMN_04           = { "COLSEP", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_03, FALSE };
   _DECL s_TBCOLUMN_05           = { "DEFCOLOR", 'A', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_04, FALSE };
   _DECL s_TBCOLUMN_06           = { "FOOTING", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_05, FALSE };
   _DECL s_TBCOLUMN_07           = { "FOOTSEP", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_06, FALSE };
   _DECL s_TBCOLUMN_08           = { "HEADING", 'C', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_07, FALSE };
   _DECL s_TBCOLUMN_09           = { "PICTURE", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_08, FALSE };
   _DECL s_TBCOLUMN_10           = { "WIDTH", 'N', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_TBCOLUMN_09, FALSE };
   _DECL s_TBCOLUMN_11           = { "COLPOS", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_10, FALSE };
   _DECL s_TBCOLUMN_12           = { "HEADSEP", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBCOLUMN_11, FALSE };


   /*       Name     Ret  # of Prams  Param Types   Ret Class  Param Classes  Next
      ---------------  ---  ----------  --------------------  ---------  -------------  --------------- */
   _DECL s_TBROWSE_01   = { "BORDER", ' ', 1, ( BYTE * ) "\x7a", { NULL }, NULL, NULL, FALSE };
   _DECL s_TBROWSE_02   = { "COLORSPEC", ' ', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_TBROWSE_01, FALSE };
   _DECL s_TBROWSE_03   = { "NBOTTOM", ' ', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_TBROWSE_02, FALSE };
   _DECL s_TBROWSE_04   = { "NLEFT", ' ', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_TBROWSE_03, FALSE };
   _DECL s_TBROWSE_05   = { "NRIGHT", ' ', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_TBROWSE_04, FALSE };
   _DECL s_TBROWSE_06   = { "NTOP", ' ', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_TBROWSE_05, FALSE };
   _DECL s_TBROWSE_07   = { "COLSEP", ' ', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_TBROWSE_06, FALSE };
   _DECL s_TBROWSE_08   = { "FOOTSEP", ' ', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_TBROWSE_07, FALSE };
   _DECL s_TBROWSE_09   = { "HEADSEP", ' ', 1, ( BYTE * ) "\x7a", { NULL }, NULL, &s_TBROWSE_08, FALSE };
   _DECL s_TBROWSE_10   = { "FREEZE", ' ', 1, ( BYTE * ) "\xa8", { NULL }, NULL, &s_TBROWSE_09, FALSE };
   _DECL s_TBROWSE_11   = { "DOWN", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_10, FALSE };
   _DECL s_TBROWSE_12   = { "END", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_11, FALSE };
   _DECL s_TBROWSE_13   = { "GOBOTTOM", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_12, FALSE };
   _DECL s_TBROWSE_14   = { "GOTOP", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_13, FALSE };
   _DECL s_TBROWSE_15   = { "HOME", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_14, FALSE };
   _DECL s_TBROWSE_16   = { "LEFT", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_15, FALSE };
   _DECL s_TBROWSE_17   = { "PAGEDOWN", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_16, FALSE };
   _DECL s_TBROWSE_18   = { "PAGEUP", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_17, FALSE };
   _DECL s_TBROWSE_19   = { "PANEND", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_18, FALSE };
   _DECL s_TBROWSE_20   = { "PANHOME", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_19, FALSE };
   _DECL s_TBROWSE_21   = { "PANLEFT", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_20, FALSE };
   _DECL s_TBROWSE_22   = { "PANRIGHT", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_21, FALSE };
   _DECL s_TBROWSE_23   = { "RIGHT", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_22, FALSE };
   _DECL s_TBROWSE_24   = { "UP", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_23, FALSE };
   _DECL s_TBROWSE_25   = { "ADDCOLUMN", 'O', 1, ( BYTE * ) "O", { NULL }, NULL, &s_TBROWSE_24, FALSE };
   _DECL s_TBROWSE_26   = { "DELCOLUMN", 'O', 1, ( BYTE * ) "N", { NULL }, NULL, &s_TBROWSE_25, FALSE };
   _DECL s_TBROWSE_27   = { "INSCOLUMN", 'O', 2, ( BYTE * ) "NO", { NULL }, NULL, &s_TBROWSE_26, FALSE };
   _DECL s_TBROWSE_28   = { "GETCOLUMN", 'O', 1, ( BYTE * ) "N", { NULL }, NULL, &s_TBROWSE_27, FALSE };
   _DECL s_TBROWSE_29   = { "SETCOLUMN", 'O', 2, ( BYTE * ) "NO", { NULL }, NULL, &s_TBROWSE_28, FALSE };
   _DECL s_TBROWSE_30   = { "COLWIDTH", ' ', 1, ( BYTE * ) "N", { NULL }, NULL, &s_TBROWSE_29, FALSE };
   _DECL s_TBROWSE_31   = { "COLCOUNT", 'N', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_30, FALSE };
   _DECL s_TBROWSE_32   = { "COLORRECT", 'O', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_31, FALSE };
   _DECL s_TBROWSE_33   = { "CONFIGURE", 'O', 1, ( BYTE * ) "N", { NULL }, NULL, &s_TBROWSE_32, FALSE };
   _DECL s_TBROWSE_34   = { "DEHILITE", 'O', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_33, FALSE };
   _DECL s_TBROWSE_35   = { "FORCESTABLE", 'O', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_34, FALSE };
   _DECL s_TBROWSE_36   = { "FORCESTABILIZE", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_35, FALSE };
   _DECL s_TBROWSE_37   = { "HILITE", 'O', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_36, FALSE };
   _DECL s_TBROWSE_38   = { "INVALIDATE", 'O', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_37, FALSE };
   _DECL s_TBROWSE_39   = { "REFRESHALL", 'O', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_38, FALSE };
   _DECL s_TBROWSE_40   = { "REFRESHCURRENT", 'O', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_39, FALSE };
   _DECL s_TBROWSE_41   = { "STABILIZE", 'L', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_40, FALSE };
   _DECL s_TBROWSE_42   = { "SETKEY", ' ', 2, ( BYTE * ) "N\xa9", { NULL }, NULL, &s_TBROWSE_41, FALSE };
   _DECL s_TBROWSE_43   = { "APPLYKEY", ' ', 1, ( BYTE * ) "N", { NULL }, NULL, &s_TBROWSE_42, FALSE };
   _DECL s_TBROWSE_44   = { "INITKEYS", ' ', 1, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_43, FALSE };
   _DECL s_TBROWSE_45   = { "TAPPLYKEY", ' ', 2, ( BYTE * ) "N\xa9", { NULL }, NULL, &s_TBROWSE_44, FALSE };
   _DECL s_TBROWSE_46   = { "HITTEST", ' ', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_TBROWSE_45, FALSE };
   _DECL s_TBROWSE_47   = { "SETSTYLE", ' ', 2, ( BYTE * ) "N\xa6", { NULL }, NULL, &s_TBROWSE_46, FALSE };
   _DECL s_TBROWSE_48   = { "MGOTOYX", ' ', 2, ( BYTE * ) "NN", { NULL }, NULL, &s_TBROWSE_47, FALSE };
   _DECL s_TBROWSE_49   = { "POSCURSOR", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_48, FALSE };
   _DECL s_TBROWSE_50   = { "LEFTDETERMINE", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_49, FALSE };
   _DECL s_TBROWSE_51   = { "DISPCELL", ' ', 3, ( BYTE * ) "NN\xc2", { NULL }, NULL, &s_TBROWSE_50, FALSE };
   _DECL s_TBROWSE_52   = { "HOWMANYCOL", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_51, FALSE };
   _DECL s_TBROWSE_53   = { "REDRAWHEADERS", ' ', 1, ( BYTE * ) "N", { NULL }, NULL, &s_TBROWSE_52, FALSE };
   _DECL s_TBROWSE_54   = { "MOVED", ' ', 0, ( BYTE * ) NULL, { NULL }, NULL, &s_TBROWSE_53, FALSE };
   _DECL s_TBROWSE_55   = { "WRITEMLINETEXT", ' ', 4, ( BYTE * ) "CNLC", { NULL }, NULL, &s_TBROWSE_54, FALSE };
   _DECL s_TBROWSE_56   = { "SETFROZENCOLS", ' ', 1, ( BYTE * ) "N", { NULL }, NULL, &s_TBROWSE_55, FALSE };
   _DECL s_TBROWSE_57   = { "SETCOLUMNWIDTH", ' ', 1, ( BYTE * ) "O", { NULL }, NULL, &s_TBROWSE_56, FALSE };
   _DECL s_TBROWSE_58   = { "SETBORDER", ' ', 1, ( BYTE * ) "C", { NULL }, NULL, &s_TBROWSE_57, FALSE };


   /* TODO: Finish definition of GET, and add definitions for TBROWSE. */

   #undef _DECL

   /* ------- */

   /* First (bottom) Method */
   s_ERROR.pMethod      = &s_ERROR_14;
   /* Last (top) Method. */
   s_ERROR.pLastMethod  = &s_ERROR_01;

   /* ------- */

   /* First (bottom) Method */
   s_GET.pMethod     = &s_GET_35; /* Change to BOTTOM Method. */
   /* Last (top) Method. */
   s_GET.pLastMethod = &s_GET_01;

   /* ------- */

   /* First (bottom) Method */
   s_TBCOLUMN.pMethod      = &s_TBCOLUMN_12; /* Change to BOTTOM Method. */
   /* Last (top) Method. */
   s_TBCOLUMN.pLastMethod  = &s_TBCOLUMN_01;

   /* ------- */

   /* First (bottom) Method */
   s_TBROWSE.pMethod       = &s_TBROWSE_58; /* Change to BOTTOM Method. */
   /* Last (top) Method. */
   s_TBROWSE.pLastMethod   = &s_TBROWSE_01;

   /* ------- */

   hb_comp_pFirstDeclared     = &s_265; /* Change to BOTTOM Function. */
   hb_comp_pLastDeclared      = &s_001;
   hb_comp_pReleaseDeclared   = s_001.pNext;

   hb_comp_pFirstClass        = &s_TBROWSE;
   hb_comp_pLastClass         = &s_ERROR;
   hb_comp_pReleaseClass      = s_ERROR.pNext;
}

void _hb_compDeclaredInit( void )
{
   if( ! hb_bcompDeclaredInit )
   {
      hb_bcompDeclaredInit = TRUE;
      hb_compDeclaredInit();
   }
}

PCOMDECLARED hb_compDeclaredAdd( char * szDeclaredName )
{
   PCOMDECLARED pDeclared;

   if( hb_comp_iWarnings < 3 )
      return NULL;

#if defined( HB_COMP_DEBUG )
   printf( "\n[hb_compDeclaredAdd] Declaring Function: %s\n", szDeclaredName );
#endif

   if( ( pDeclared = hb_compDeclaredFind( szDeclaredName ) ) != NULL )
   {
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_DUP_DECLARATION, "Function", szDeclaredName );

      /* Last declaration will take effect. */
      pDeclared->cType           = ' '; /* Not known yet */
      pDeclared->cParamTypes     = NULL;
      pDeclared->iParamCount     = 0;
      pDeclared->pParamClasses   = NULL;

      return pDeclared;
   }

   pDeclared                     = ( PCOMDECLARED ) hb_xgrab( sizeof( COMDECLARED ) );

   pDeclared->szName             = szDeclaredName;
   pDeclared->cType              = ' '; /* Not known yet */
   pDeclared->cParamTypes        = NULL;
   pDeclared->iParamCount        = 0;
   pDeclared->pParamClasses      = NULL;
   pDeclared->pNext              = NULL;

   if( hb_comp_pReleaseDeclared == NULL )
      hb_comp_pReleaseDeclared = pDeclared;

   hb_comp_pLastDeclared->pNext  = pDeclared;
   hb_comp_pLastDeclared         = pDeclared;

   return pDeclared;
}

PCOMSYMBOL hb_compSymbolAdd( char * szSymbolName, USHORT * pwPos, void * Namespace, int iFlags )
{
   PCOMSYMBOL pSym;

   if( szSymbolName[ 0 ] )
   {
      /* Create a symbol for non-empty names only.
       * NOTE: an empty name is passed for a fake starting function when
       * '-n' switch is used
       */

      pSym = hb_compSymbolFind( szSymbolName, pwPos, Namespace, iFlags );

      if( pSym )
      {
         pSym->iFlags   |= iFlags;
         pSym->cScope   |= ( iFlags & ( HB_FS_PUBLIC | HB_FS_STATIC ) );

         /* Can never be both!
          */
         assert( ( pSym->cScope & ( HB_FS_PUBLIC | HB_FS_STATIC ) ) != ( HB_FS_PUBLIC | HB_FS_STATIC ) );

         return pSym;
      }

      pSym           = ( PCOMSYMBOL ) hb_xgrab( sizeof( COMSYMBOL ) );

      pSym->szName   = szSymbolName;
      pSym->cType    = hb_comp_cVarType;
      pSym->iFlags   = iFlags;

      if( iFlags & SYMF_FUNCALL )
      {
         if( iFlags & SYMF_PUBLIC )
            pSym->cScope = HB_FS_PUBLIC;
         else if( iFlags & SYMF_STATIC )
            pSym->cScope = HB_FS_STATIC;
         else
            /* This symbol will have to be resolved to HB_FS_PUBLIC or HB_FS_STATIC
             */
            pSym->cScope = ( HB_SYMBOLSCOPE ) 0;
      }
      else if( iFlags & SYMF_PUBLIC )
         pSym->cScope = HB_FS_PUBLIC;
      else
         assert( 0 );

      pSym->Namespace   = Namespace;
      pSym->pNext       = NULL;

      if( ! hb_comp_symbols.iCount )
      {
         hb_comp_symbols.pFirst  = pSym;
         hb_comp_symbols.pLast   = pSym;
      }
      else
      {
         ( ( PCOMSYMBOL ) hb_comp_symbols.pLast )->pNext = pSym;
         hb_comp_symbols.pLast                           = pSym;
      }

      hb_comp_symbols.iCount++;

      if( pwPos )
         *pwPos = ( USHORT ) ( hb_comp_symbols.iCount - 1 ); /* position number starts form 0 */
   }
   else
      pSym = NULL;

   return pSym;
}

/*
 * This function creates and initialises the _FUNC structure
 */
static PFUNCTION hb_compFunctionNew( char * szName, HB_SYMBOLSCOPE cScope )
{
   PFUNCTION pFunc;

#if defined( HB_COMP_DEBUG )
   printf( "[hb_compFunctionNew] Function: %s\n", szName );
#endif

   pFunc                   = ( PFUNCTION ) hb_xgrab( sizeof( _FUNC ) );
   pFunc->szName           = szName;
   pFunc->pNamespace       = hb_comp_Namespaces.pCurrent;
   pFunc->cScope           = cScope;
   pFunc->pLocals          = NULL;
   pFunc->pStatics         = NULL;
   pFunc->pFields          = NULL;
   pFunc->pMemvars         = NULL;
   pFunc->pPrivates        = NULL;
   pFunc->pEnums           = NULL;
   pFunc->pCode            = NULL;
   pFunc->lPCodeSize       = 0;
   pFunc->lPCodePos        = 0;
   pFunc->pNext            = NULL;
   pFunc->wParamCount      = 0;
   pFunc->wParamNum        = 0;
   pFunc->iStaticsBase     = hb_comp_iStaticCnt;
   pFunc->pOwner           = NULL;
   pFunc->bFlags           = 0;
   pFunc->iNOOPs           = 0;
   pFunc->iJumps           = 0;
   pFunc->pNOOPs           = NULL;
   pFunc->pJumps           = NULL;
   pFunc->pStack           = NULL;
   pFunc->iStackSize       = 0;
   pFunc->iStackIndex      = 0;
   pFunc->iStackFunctions  = 0;
   pFunc->iStackClasses    = 0;

   return pFunc;
}

static PINLINE hb_compInlineNew( char * szName )
{
   PINLINE pInline;

   pInline              = ( PINLINE ) hb_xgrab( sizeof( __INLINE ) );

   pInline->szName      = szName;
   pInline->pCode       = NULL;
   pInline->lPCodeSize  = 0;
   pInline->pNext       = NULL;
   pInline->szFileName  = hb_strdup( hb_pp_fileName( hb_comp_PP ) );
   pInline->iLine       = hb_comp_iLine - 1;

   return pInline;
}

/*
 * Stores a Clipper defined function/procedure
 * szFunName - name of a function
 * cScope    - scope of a function
 * iType     - FUN_PROCEDURE if a procedure or 0
 */
void hb_compFunctionAdd( char * szFunName, HB_SYMBOLSCOPE cScope, int iType )
{
   PCOMSYMBOL  pSym;
   PFUNCTION   pFunc;
   char *      szFunction;
   PNAMESPACE  pNamespace = NULL;

   hb_compFinalizeFunction();    /* fix all previous function returns offsets */

   if( cScope & HB_FS_INITEXIT & ~HB_FS_STATIC )
   {
      HB_SIZE  iLen        = strlen( szFunName );
      char *   sDecorated  = ( char * ) hb_xgrab( iLen + 2 );

      hb_xstrcpy( sDecorated, szFunName, 0 );
      szFunName               = sDecorated;

      szFunName[ iLen ]       = '$';
      szFunName[ iLen + 1 ]   = '\0';

      szFunName               = hb_compIdentifierNew( szFunName, FALSE );
   }

   pFunc = hb_compFunctionFind( szFunName, ( void * ) hb_comp_Namespaces.pCurrent, NSF_MEMBER );

   if( pFunc )
   {
      /* The name of a function/procedure is already defined */
      if( ( pFunc != hb_comp_functions.pFirst ) || hb_comp_bStartProc )
         /* it is not a starting procedure that was automatically created */
         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_FUNC_DUPL, szFunName, NULL );
   }

   if( hb_comp_Namespaces.pCurrent == NULL )
   {
#if ( ! defined( HB_RESERVED_OFF ) )
      szFunction = hb_compReservedName( szFunName );

      if( szFunction && ! ( hb_comp_functions.iCount == 0 && ! hb_comp_bStartProc ) )
         /* We are ignoring it when it is the name of PRG file and we are
          * not creating implicit starting procedure
          */
         hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_FUNC_RESERVED, szFunction, szFunName );
#endif
   }
   else
      pNamespace = hb_compNamespaceNew( szFunName, NSTYPE_MEMBER | ( ( cScope & HB_FS_STATIC ) ?  NSTYPE_STATIC : 0 ) );

   hb_comp_iFunctionCnt++;

   if( szFunName[ 0 ] )
   {
      if( hb_comp_Namespaces.pCurrent == NULL ||
          ( cScope & ( HB_FS_INIT | HB_FS_EXIT ) & ~HB_FS_STATIC ) != 0 ||
          ( ( hb_comp_Namespaces.pCurrent->type & ( ( NSTYPE_RUNTIME | NSTYPE_OPTIONAL ) & ~NSTYPE_SPACE ) ) != 0 &&
            ( cScope & HB_FS_STATIC ) == 0 ) )
      {
         if( hb_comp_Namespaces.pCurrent )
            pSym = hb_compSymbolAdd( szFunName, NULL, ( void * ) hb_comp_Namespaces.pCurrent, SYMF_NS_EXPLICITPTR | ( cScope & ( HB_FS_PUBLIC | HB_FS_STATIC ) ) );
         else
            pSym = hb_compSymbolAdd( szFunName, NULL, ( void * ) hb_comp_Namespaces.pCurrent, SYMF_FUNCALL | ( cScope & ( HB_FS_PUBLIC | HB_FS_STATIC ) ) );

         pSym->cScope |= ( HB_FS_LOCAL | cScope );
      }
   }

   pFunc          = hb_compFunctionNew( szFunName, cScope );
   pFunc->bFlags  |= ( BYTE ) iType;

   if( pNamespace )
      pNamespace->pFunc = pFunc;

   if( hb_comp_functions.iCount == 0 )
   {
      hb_comp_functions.pFirst   = pFunc;
      hb_comp_functions.pLast    = pFunc;
   }
   else
   {
      hb_comp_functions.pLast->pNext   = pFunc;
      hb_comp_functions.pLast          = pFunc;
   }

   hb_comp_functions.iCount++;

   hb_comp_ulLastLinePos   = 0;                       /* optimization of line numbers opcode generation */
   hb_comp_ulLastOffsetPos = 0;                       /* optimization of line numbers opcode generation */

   hb_compGenPCode3( HB_P_FRAME, 0, 0, ( BOOL ) 0 );  /* frame for locals and parameters */
   hb_compGenPCode3( HB_P_SFRAME, 0, 0, ( BOOL ) 0 ); /* frame for statics variables */

   hb_comp_iBaseLine = 0;

   if( hb_comp_bDebugInfo )
   {
      char        szFileName[ HB_PATH_MAX ];
      PHB_FNAME   hb_FileName;

      hb_FileName          = hb_fsFNameSplit( hb_pp_fileName( hb_comp_PP ) );
      hb_FileName->szPath  = NULL;
      hb_fsFNameMerge( szFileName, hb_FileName );
      hb_xfree( ( void * ) hb_FileName );
      hb_compGenModuleName( szFileName, szFunName );
   }

   hb_comp_bDontGenLineNum = FALSE; /* reset the flag */
}

PINLINE hb_compInlineAdd( char * szFunName )
{
   PINLINE     pInline;
   PCOMSYMBOL  pSym;

   if( szFunName )
   {
      pSym           = hb_compSymbolAdd( szFunName, NULL, NULL, SYMF_FUNCALL | SYMF_STATIC );
      pSym->cScope   |= HB_FS_LOCAL;
   }

   pInline = hb_compInlineNew( szFunName );

   if( hb_comp_inlines.iCount == 0 )
   {
      hb_comp_inlines.pFirst  = pInline;
      hb_comp_inlines.pLast   = pInline;
   }
   else
   {
      hb_comp_inlines.pLast->pNext  = pInline;
      hb_comp_inlines.pLast         = pInline;
   }

   hb_comp_inlines.iCount++;

   return pInline;
}

/* create an ANNOUNCEd procedure
 */
void hb_compAnnounce( char * szFunName )
{
   PFUNCTION pFunc;

   pFunc = hb_compFunctionFind( szFunName, ( void * ) hb_comp_Namespaces.pCurrent, NSF_MEMBER );

   if( pFunc )
   {
      /* there is a function/procedure defined already - ANNOUNCEd procedure
       * have to be a public symbol - check if existing symbol is public
       */
      if( pFunc->cScope & HB_FS_STATIC )
         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_FUNC_ANNOUNCE, szFunName, NULL );
   }
   else
   {
      /* create a new procedure
       */
      if( hb_comp_Namespaces.pCurrent == NULL )
      {
         PCOMSYMBOL pSym = hb_compSymbolAdd( szFunName, NULL, hb_comp_Namespaces.pCurrent, SYMF_FUNCALL | SYMF_PUBLIC );

         pSym->cScope |= HB_FS_LOCAL;
      }

      pFunc          = hb_compFunctionNew( szFunName, HB_FS_PUBLIC | HB_FS_LOCAL );
      pFunc->bFlags  |= FUN_PROCEDURE;

      if( hb_comp_functions.iCount == 0 )
      {
         hb_comp_functions.pFirst   = pFunc;
         hb_comp_functions.pLast    = pFunc;
      }
      else
      {
         hb_comp_functions.pLast->pNext   = pFunc;
         hb_comp_functions.pLast          = pFunc;
      }

      hb_comp_functions.iCount++;
      hb_comp_iFunctionCnt++;

      /* this function have a very limited functionality
       */
      hb_compGenPCode1( HB_P_ENDPROC );
   }
}

/* NOTE: Names of variables and functions are released in hbident.c on exit */
PFUNCTION hb_compFunctionKill( PFUNCTION pFunc )
{
   PFUNCTION   pNext = pFunc->pNext;
   PVAR        pVar;

   while( pFunc->pLocals )
   {
      pVar           = pFunc->pLocals;
      pFunc->pLocals = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pStatics )
   {
      pVar              = pFunc->pStatics;
      pFunc->pStatics   = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pFields )
   {
      pVar           = pFunc->pFields;
      pFunc->pFields = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pMemvars )
   {
      pVar              = pFunc->pMemvars;
      pFunc->pMemvars   = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pPrivates )
   {
      pVar              = pFunc->pPrivates;
      pFunc->pPrivates  = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pEnums )
   {
      PENUMDEF pEnum = pFunc->pEnums;
      pFunc->pEnums = pEnum->pNext;

      hb_xfree( ( void * ) pEnum->pMembers );
      hb_xfree( ( void * ) pEnum );
   }

   /* Release the NOOP array. */
   if( pFunc->pNOOPs )
      hb_xfree( ( void * ) pFunc->pNOOPs );

   /* Release the Jumps array. */
   if( pFunc->pJumps )
      hb_xfree( ( void * ) pFunc->pJumps );

   if( pFunc->pCode )
      hb_xfree( ( void * ) pFunc->pCode );

/* hb_xfree( ( void * ) pFunc->szName ); The name will be released in hb_compSymbolKill() */
   hb_xfree( ( void * ) pFunc );

   return pNext;
}

/* NOTE: Name of symbols are released in hbident.c  on exit */
PCOMSYMBOL hb_compSymbolKill( PCOMSYMBOL pSym )
{
   PCOMSYMBOL pNext = pSym->pNext;

   hb_xfree( ( void * ) pSym );

   return pNext;
}

void hb_compGenBreak( void )
{
   hb_compGenPushFunCall( hb_compExpr_IDs.BREAK_, NULL );
   hb_compGenPushNil();
}

void hb_compGenWithObject( PHB_EXPR pObject )
{
   hb_compExprDelete( hb_compExprGenPush( pObject ) );
   hb_compGenPCode1( HB_P_WITHOBJECT );
   hb_comp_wWithObjCounter++;
}

void hb_compGenEndWithObject( void )
{
   hb_compGenPCode1( HB_P_ENDWITHOBJECT );
   if( hb_comp_wWithObjCounter )
      hb_comp_wWithObjCounter--;
}

PFUNCALL hb_compFunCallFind( char * szFuncName, void * Namespace, int iFlags ) /* returns a previously called defined function */
{
   PFUNCALL pFunCall = hb_comp_funcalls.pFirst;

   HB_SYMBOL_UNUSED( iFlags );

   while( pFunCall )
   {
      if( pFunCall->szName == szFuncName && pFunCall->Namespace == Namespace )
         return pFunCall;

      pFunCall = pFunCall->pNext;
   }

   return NULL;
}

PFUNCTION hb_compFunctionFind( char * szFunctionName, void * Namespace, int iFlags ) /* returns a previously defined function */
{
   PFUNCTION pFunc = hb_comp_functions.pFirst;

   while( pFunc )
   {
      if( pFunc->szName == szFunctionName )
      {
         if( ( iFlags & NSF_EXPLICITPATH ) == NSF_EXPLICITPATH )
         {
            if( Namespace == NULL && pFunc->pNamespace == NULL )
               return pFunc;
            else if( pFunc->pNamespace && pFunc->pNamespace->szFullPath == ( char * ) Namespace )
               return pFunc;
         }
         else if( ( iFlags & NSF_EXPLICITPTR ) == NSF_EXPLICITPTR )
         {
            if( pFunc->pNamespace == ( PNAMESPACE ) Namespace )
               return pFunc;
         }
         else if( ( iFlags & NSF_MEMBER ) == NSF_MEMBER )
         {
            if( pFunc->pNamespace == ( PNAMESPACE ) Namespace )
               return pFunc;
         }
         else
         {
            if( pFunc->pNamespace == NULL )
               return pFunc;
         }
      }

      pFunc = pFunc->pNext;
   }

   return NULL;
}

PFUNCTION hb_compFunctionResolve( char * szFunctionName, PNAMESPACE pCallerNamespace, PCOMSYMBOL pSymbol )
{
   PNAMESPACE pNamespace = pCallerNamespace, pMember;

   assert( pSymbol == NULL || ( pSymbol->iFlags & SYMF_NS_RESOLVE ) == SYMF_NS_RESOLVE );

   do
   {
      pMember = hb_compNamespaceFindMember( pNamespace->pNext, szFunctionName, NSTYPE_MEMBER );

      if( pMember )
      {
         if( ( pMember->type & NSTYPE_EXTERNAL ) == NSTYPE_EXTERNAL )
         {
            if( pSymbol )
            {
               pSymbol->Namespace   = ( void * ) pNamespace;
               pSymbol->iFlags      &= ~SYMF_NS_RESOLVE;
               pSymbol->iFlags      |= SYMF_NS_EXPLICITPTR;
               pSymbol->cScope      |= ( HB_FS_INDIRECT | HB_FS_PUBLIC );

               //REVIEW: pSymbol->cScope |= HB_FS_LOCAL;
            }
#if defined( __XCC__ )
            return ( PFUNCTION ) 1;
#else
            return ( PFUNCTION ) ( HB_LONG ) 1;
#endif
         }
         else
         {
            if( pSymbol )
            {
               pSymbol->Namespace   = ( void * ) pNamespace;
               pSymbol->iFlags      &= ~SYMF_NS_RESOLVE;
               pSymbol->iFlags      |= SYMF_NS_EXPLICITPTR;
               pSymbol->cScope      |= HB_FS_LOCAL;

               if( ( pSymbol->cScope & ( HB_FS_PUBLIC | HB_FS_STATIC ) ) == 0 )
               {
                  if( ( pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_RUNTIME || ( pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
                     pSymbol->cScope |= HB_FS_PUBLIC;
                  else
                     pSymbol->cScope |= HB_FS_STATIC;
               }
            }

            return pMember->pFunc;
         }
      }
      else
      {
         pNamespace = pNamespace->pOuter;
      }
   }
   while( pNamespace );

   if( pSymbol )
   {
      PCOMSYMBOL pGlobalSym = hb_compSymbolFind( szFunctionName, NULL, NULL, SYMF_FUNCALL );

      if( pGlobalSym )
         pSymbol->cScope |= ( pGlobalSym->cScope & ( HB_FS_DEFERRED | HB_FS_LOCAL | HB_FS_STATIC ) );

      if( ( pSymbol->cScope & HB_FS_STATIC ) == 0 )
         pSymbol->cScope |= HB_FS_PUBLIC;

      pSymbol->Namespace   = NULL;
      pSymbol->iFlags      &= ~SYMF_NS_RESOLVE;
      pSymbol->iFlags      |= SYMF_FUNCALL;
   }

   return NULL;
}

PINLINE hb_compInlineFind( char * szFunctionName )
{
   PINLINE pInline = hb_comp_inlines.pFirst;

   while( pInline )
   {
      if( pInline->szName && strcmp( pInline->szName, szFunctionName ) == 0 )
         return pInline;
      else
      {
         if( pInline->pNext )
            pInline = pInline->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

/* return variable using its order after final fixing */
PVAR hb_compLocalVariableFind( PFUNCTION pFunc, int wVar )
{
   if( pFunc->wParamCount && ! ( pFunc->bFlags & FUN_USES_LOCAL_PARAMS ) )
      wVar -= pFunc->wParamCount;

   return hb_compVariableFind( pFunc->pLocals, wVar );
}

PVAR hb_compVariableFind( PVAR pVars, int wOrder ) /* returns variable if defined or zero */
{
   int w = 1;

   if( pVars )
   {
      while( pVars->pNext && w++ < wOrder )
         pVars = pVars->pNext;
   }

   return pVars;
}

USHORT hb_compVariableGetPos( PVAR pVars, char * szVarName ) /* returns the order + 1 of a variable if defined or zero */
{
   USHORT wVar = 1;

   while( pVars )
   {
      if( pVars->szName == szVarName )
      {
         /* if( hb_comp_iWarnings < 3 )
          */
         {
            pVars->iUsed |= VU_USED;
         }
         /*
          *  else
          *  Handled by hb_compStrongType()
          */

         return wVar;
      }
      else
      {
         if( pVars->pNext )
         {
            pVars = pVars->pNext;
            wVar++;
         }
         else
            return 0;
      }
   }
   return 0;
}

int hb_compLocalGetPos( char * szVarName ) /* returns the order + 1 of a variable if defined or zero */
{
   int         iVar;
   PFUNCTION   pFunc = hb_comp_functions.pLast;

   if( pFunc->szName )
   {
      /* we are in a function/procedure -we don't need any tricks */
      if( pFunc->pOwner )
         pFunc = pFunc->pOwner;

      iVar = hb_compVariableGetPos( pFunc->pLocals, szVarName );
   }
   else
   {
      /* we are in a codeblock */
      iVar = hb_compVariableGetPos( pFunc->pLocals, szVarName );

      if( iVar == 0 )
      {
         /* this is not a current codeblock parameter
          * we have to check the list of nested codeblocks up to a function
          * where the codeblock is defined
          */
         PFUNCTION   pOutBlock = pFunc; /* the outermost codeblock */
         BOOL        bStatic;

         pFunc = pFunc->pOwner;

         while( pFunc )
         {
            bStatic = FALSE;

            if( ( pFunc->cScope & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
            {
               /* we are in a codeblock used to initialize a static variable -
                * skip to a function where this static variable was declared
                */
               pFunc    = pFunc->pOwner;
               bStatic  = TRUE;
            }

            iVar = hb_compVariableGetPos( pFunc->pLocals, szVarName );

            if( iVar )
            {
               if( pFunc->pOwner )
               {
                  /* this variable is defined in a parent codeblock
                   * It is not possible to access a parameter of a codeblock in which
                   * the current codeblock is defined
                   */
                  hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_OUTER_VAR, szVarName, NULL );
                  return iVar;
               }
               else if( bStatic )
               {
                  /* local variable was referenced in a codeblock during
                   * initialization of static variable. This cannot be supported
                   * because static variables are initialized at program
                   * startup when there is no local variables yet - hence we
                   * cannot detach this local variable
                   * For example:
                   * LOCAL locvar
                   * STATIC stavar:={ | x | locvar}
                   *
                   * NOTE: Clipper creates such a codeblock however at the
                   * time of codeblock evaluation it generates a runtime error:
                   * 'bound error: array acccess'
                   * Called from: (b)STATICS$(0)
                   */
                  hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ILLEGAL_INIT, "(b)", szVarName );
                  return iVar;
               }
               else
               {
                  /* We want to access a local variable defined in a function
                   * that owns this codeblock. We cannot access this variable in
                   * a normal way because at runtime the stack base will point
                   * to local variables of EVAL function.
                   *  The codeblock cannot have static variables then we can
                   * use this structure to store temporarily all referenced
                   * local variables
                   */
                  /* NOTE: The list of local variables defined in a function
                   * and referenced in a codeblock will be stored in a outer
                   * codeblock only. This makes sure that all variables will be
                   * detached properly - the inner codeblock can be created
                   * outside of a function where it was defined when the local
                   * variables are not accessible.
                   */
                  iVar = -hb_compVariableGetPos( pOutBlock->pStatics, szVarName );

                  if( iVar == 0 )
                  {
                     /* this variable was not referenced yet - add it to the list */
                     PVAR pVar;

                     pVar              = ( PVAR ) hb_xgrab( sizeof( VAR ) );
                     pVar->szName      = szVarName;
                     pVar->cType       = ' ';
                     pVar->iUsed       = VU_NOT_USED;
                     pVar->pNext       = NULL;
                     pVar->iDeclLine   = hb_comp_iLine - 1;

                     /* Use negative order to signal that we are accessing a local
                      * variable from a codeblock
                      */
                     iVar              = -1; /* first variable */

                     if( ! pOutBlock->pStatics )
                        pOutBlock->pStatics = pVar;
                     else
                     {
                        PVAR pLastVar = pOutBlock->pStatics;

                        --iVar;   /* this will be at least second variable */

                        while( pLastVar->pNext )
                        {
                           pLastVar = pLastVar->pNext;
                           --iVar;
                        }

                        pLastVar->pNext = pVar;
                     }
                  }

                  return iVar;
               }
            }

            pOutBlock   = pFunc;
            pFunc       = pFunc->pOwner;
         }
      }
   }

   return iVar;
}

/* Checks if passed variable name is declared as STATIC
 * Returns 0 if not found in STATIC list or its position in this list if found
 *
 * All static variables are hold in a single array at runtime then positions
 * are numbered for whole PRG module.
 */
int hb_compStaticGetPos( char * szVarName, PFUNCTION pFunc )
{
   int iVar;

   while( pFunc->pOwner )     /* pOwner is not NULL if STATIC var := value is used */
      pFunc = pFunc->pOwner;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar = hb_compVariableGetPos( pFunc->pStatics, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;

      iVar = hb_compVariableGetPos( pFunc->pStatics, szVarName );
   }

   if( iVar )
      iVar += pFunc->iStaticsBase;

   return iVar;
}

/* Checks if passed variable name is declared as FIELD
 * Returns 0 if not found in FIELD list or its position in this list if found
 */
int hb_compFieldGetPos( char * szVarName, PFUNCTION pFunc )
{
   int iVar;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar = hb_compVariableGetPos( pFunc->pFields, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;

      iVar = hb_compVariableGetPos( pFunc->pFields, szVarName );
   }
   return iVar;
}

/* Checks if passed variable name is declared as MEMVAR
 * Returns 0 if not found in MEMVAR list or its position in this list if found
 */
int hb_compMemvarGetPos( char * szVarName, PFUNCTION pFunc )
{
   int iVar;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar = hb_compVariableGetPos( pFunc->pMemvars, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;

      iVar = hb_compVariableGetPos( pFunc->pMemvars, szVarName );
   }
   return iVar;
}

/* returns a symbol pointer from the symbol table
 * and sets its position in the symbol table.
 * NOTE: symbol's position number starts from 0
 */
PCOMDECLARED hb_compDeclaredFind( char * szDeclaredName )
{
   PCOMDECLARED pSym = hb_comp_pFirstDeclared;

   while( pSym )
   {
      if( pSym->szName == szDeclaredName )
         return pSym;
      else
      {
         if( pSym->pNext )
            pSym = pSym->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

PCOMSYMBOL hb_compSymbolFind( char * szSymbolName, USHORT * pwPos, void * Namespace, int iFlags )
{
   PCOMSYMBOL  pSym;
   USHORT      wCnt = 0;

   if( pwPos )
      *pwPos = 0;

   /* First we want a function [call] symbol if exists.
    */
   if( ( iFlags & SYMF_FUNCALL ) )
   {
      pSym = hb_comp_symbols.pFirst;

      while( pSym )
      {
         if( pSym->szName == szSymbolName && pSym->Namespace == Namespace )
         {
            /* Function symbol is always valid match for function call
             */
            if( ( pSym->iFlags & SYMF_FUNCALL ) )
               break;
         }

         pSym = pSym->pNext;
         ++wCnt;
      }
   }
   else
      pSym = NULL;

   /*
      0 or SYMF_STATIC can only be a function [call] so already scanned above.
      Non function public, or 2nd scan for public function.
    */
   if( pSym == NULL && ( iFlags & SYMF_PUBLIC ) )
   {
      pSym = hb_comp_symbols.pFirst;

      while( pSym )
      {
         if( pSym->szName == szSymbolName && pSym->Namespace == Namespace )
         {
            if( ( pSym->iFlags & SYMF_PUBLIC ) )
               /*
                * Public Function must be a *definition* thus it can adopt any
                * Public symbol. Once a public symbol is used by a function
                * definition all subsequent public usages are also acceptable.
                */
               break;
         }

         pSym = pSym->pNext;
         ++wCnt;
      }
   }

   if( pSym )
   {
      if( pwPos )
         *pwPos = wCnt;
   }

   return pSym;
}

/* returns a symbol based on its index on the symbol table
 * index starts from 0
 */
PCOMSYMBOL hb_compSymbolGetPos( USHORT wSymbol )
{
   PCOMSYMBOL  pSym  = hb_comp_symbols.pFirst;
   USHORT      w     = 0;

   while( w++ < wSymbol && pSym->pNext )
      pSym = pSym->pNext;

   return pSym;
}

USHORT hb_compFunctionGetPos( char * szFunctionName ) /* return 0 if not found or order + 1 */
{
   PFUNCTION   pFunc       = hb_comp_functions.pFirst;
   USHORT      wFunction   = ( USHORT ) hb_comp_bStartProc;

   while( pFunc )
   {
      if( pFunc->szName == szFunctionName && pFunc != hb_comp_functions.pFirst )
         return wFunction;
      else
      {
         if( pFunc->pNext )
         {
            pFunc = pFunc->pNext;
            wFunction++;
         }
         else
            return 0;
      }
   }

   return 0;
}

void hb_compNOOPadd( PFUNCTION pFunc, HB_SIZE ulPos )
{
   pFunc->iNOOPs++;

   if( pFunc->pNOOPs )
   {
      pFunc->pNOOPs                       = ( HB_SIZE * ) hb_xrealloc( pFunc->pNOOPs, sizeof( HB_SIZE ) * pFunc->iNOOPs );
      pFunc->pNOOPs[ pFunc->iNOOPs - 1 ]  = ulPos;
   }
   else
   {
      pFunc->pNOOPs                       = ( HB_SIZE * ) hb_xgrab( sizeof( HB_SIZE ) );
      pFunc->pNOOPs[ pFunc->iNOOPs - 1 ]  = ulPos;
   }
}

void hb_compPrepareOptimize( void )
{
   if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_OPTJUMP ) )
   {
      hb_comp_functions.pLast->iJumps++;

      if( hb_comp_functions.pLast->pJumps )
      {
         hb_comp_functions.pLast->pJumps                                         = ( HB_SIZE * ) hb_xrealloc( hb_comp_functions.pLast->pJumps, sizeof( HB_SIZE ) * hb_comp_functions.pLast->iJumps );
         hb_comp_functions.pLast->pJumps[ hb_comp_functions.pLast->iJumps - 1 ]  = ( HB_SIZE ) ( hb_comp_functions.pLast->lPCodePos - 4 );
      }
      else
      {
         hb_comp_functions.pLast->pJumps                                         = ( HB_SIZE * ) hb_xgrab( sizeof( HB_SIZE ) );
         hb_comp_functions.pLast->pJumps[ hb_comp_functions.pLast->iJumps - 1 ]  = ( LONG ) ( hb_comp_functions.pLast->lPCodePos - 4 );
      }
   }
}

HB_SIZE hb_compGenJump( HB_LONG lOffset )
{
   if( HB_LIM_INT24( lOffset ) )
      hb_compGenPCode4( HB_P_JUMPFAR, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_LOBYTE( HB_HIWORD( lOffset ) ), ( BOOL ) 1 );
   else
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

HB_SIZE hb_compGenJumpFalse( HB_LONG lOffset )
{
   if( HB_LIM_INT24( lOffset ) )
      hb_compGenPCode4( HB_P_JUMPFALSEFAR, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_LOBYTE( HB_HIWORD( lOffset ) ), ( BOOL ) 1 );
   else
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

HB_SIZE hb_compGenJumpTrue( HB_LONG lOffset )
{
   if( HB_LIM_INT24( lOffset ) )
      hb_compGenPCode4( HB_P_JUMPTRUEFAR, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_LOBYTE( HB_HIWORD( lOffset ) ), ( BOOL ) 1 );
   else
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

void hb_compGenJumpThere( HB_ULONG ulFrom, HB_ULONG ulTo )
{
   BYTE *   pCode    = hb_comp_functions.pLast->pCode;
   LONG     lOffset  = ( LONG ) ( ulTo - ulFrom + 1 );

   if( HB_LIM_INT24( lOffset ) )
      HB_PUT_LE_UINT24( &pCode[ ( ULONG ) ulFrom ], lOffset );
   else
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );
}

void hb_compGenJumpHere( HB_ULONG ulOffset )
{
   hb_compGenJumpThere( ulOffset, hb_comp_functions.pLast->lPCodePos );
}

void hb_compGenModuleName( const char * szFile, const char * szFunc )
{
   HB_SIZE  iBufLen;
   HB_SIZE  iFileLen = strlen( szFile );
   HB_SIZE  iFuncLen;
   BYTE *   pBuffer;

   /* Use a special form "filename:" when the file changes within function */
   if( szFunc != NULL && *szFunc == '\0' && hb_comp_functions.pLast->pCode )
   {
      char * szLast = ( char * ) hb_comp_functions.pLast->pCode + hb_comp_ulLastModuleNamePos + 1;

      if( ( ! strncmp( szFile, szLast, ( size_t ) iFileLen ) ) && ( szLast[ iFileLen ] == '\0' || szLast[ iFileLen ] == ':' ) )
         return;
   }

   iFuncLen       = szFunc ? strlen( szFunc ) + 1 : 0;
   iBufLen        = 1 + iFileLen + ( szFunc ? iFuncLen : 0 ) + 1;
   pBuffer        = ( BYTE * ) hb_xgrab( iBufLen );
   pBuffer[ 0 ]   = HB_P_MODULENAME;
   HB_MEMCPY( ( BYTE * ) ( &( pBuffer[ 1 ] ) ), ( BYTE * ) szFile, ( size_t ) iFileLen );

   if( szFunc )
   {
      pBuffer[ 1 + iFileLen ] = ':';
      HB_MEMCPY( ( BYTE * ) ( &( pBuffer[ 1 + iFileLen + 1 ] ) ), ( BYTE * ) szFunc, ( size_t ) iFuncLen );
   }
   else
      pBuffer[ 1 + iFileLen ] = '\0';

   hb_comp_ulLastModuleNamePos = hb_comp_functions.pLast->lPCodePos;

   hb_compGenPCodeN( pBuffer, iBufLen, 0 );
   hb_xfree( pBuffer );
}

void hb_compLinePush( void ) /* generates the pcode with the currently compiled source code line */
{
   if( hb_comp_functions.pLast && ( hb_comp_functions.pLast->bFlags & FUN_SEALED ) )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_OUTSIDE, NULL, NULL );

   if( hb_comp_bLineNumbers && ! hb_comp_bDontGenLineNum )
   {
      int   iLine       = hb_comp_iLine - 1;
      int   iOffset     = hb_comp_iLine - hb_comp_iBaseLine;
      BOOL  bCodeblock  = ( hb_comp_functions.pLast->pOwner != NULL );

#if defined( HB_COMP_DEBUG )
      printf( "[hb_compLinePush] Line: %i, Offset %i LastPos %i, Pos %i\n", hb_comp_iLine, iOffset, hb_comp_ulLastOffsetPos, hb_comp_functions.pLast->lPCodePos );
#endif

      if( ! bCodeblock && hb_comp_bDebugInfo )
         hb_compGenModuleName( hb_pp_fileName( hb_comp_PP ), "" );

      if( ! bCodeblock && hb_comp_iBaseLine == 0 )
      {
         hb_comp_iBaseLine       = hb_comp_iLine;
         hb_comp_ulLastLinePos   = hb_comp_functions.pLast->lPCodePos;
         hb_compGenPCode3( HB_P_BASELINE, HB_LOBYTE( iLine ), HB_HIBYTE( iLine ), ( BOOL ) 0 );
      }
      else if( ! bCodeblock && iOffset >= 0 && iOffset < 256 )
      {
         if( ( hb_comp_functions.pLast->lPCodePos - hb_comp_ulLastLinePos ) == 3 )
         {
            if( hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos ] == HB_P_BASELINE )
               hb_comp_iBaseLine = hb_comp_iLine;

            hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos + 1 ] = HB_LOBYTE( iLine );
            hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos + 2 ] = HB_HIBYTE( iLine );
         }
         else if( ( ( hb_comp_functions.pLast->lPCodePos - hb_comp_ulLastOffsetPos ) > 2 ) || hb_comp_bDebugInfo )
         {
#if defined( HB_COMP_DEBUG )
            printf( "[hb_compLinePush] Offset: %i Line: %i\n", iOffset, iLine );
#endif
            hb_comp_ulLastOffsetPos = hb_comp_functions.pLast->lPCodePos;
            hb_compGenPCode2( HB_P_LINEOFFSET, ( BYTE ) iOffset, ( BOOL ) 0 );
         }
         else
         {
#if defined( HB_COMP_DEBUG )
            printf( "[hb_compLinePush] Overwrite Offset: %i Line: %i\n", iOffset, iLine );
#endif
            hb_comp_functions.pLast->pCode[ hb_comp_ulLastOffsetPos + 1 ] = ( BYTE ) iOffset;
         }
      }
      else if( bCodeblock
               || ( ( hb_comp_functions.pLast->lPCodePos - hb_comp_ulLastLinePos ) > 3 )
               || hb_comp_bDebugInfo )
      {
         if( ! bCodeblock )
            hb_comp_ulLastLinePos = hb_comp_functions.pLast->lPCodePos;

         hb_compGenPCode3( HB_P_LINE, HB_LOBYTE( iLine ), HB_HIBYTE( iLine ), ( BOOL ) 0 );
      }
      else
      {
         hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos + 1 ] = HB_LOBYTE( iLine );
         hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos + 2 ] = HB_HIBYTE( iLine );
      }
   }

   if( hb_comp_functions.pLast->bFlags & FUN_BREAK_CODE )
      /* previous line contained RETURN/BREAK/LOOP/EXIT statement */
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_UNREACHABLE, NULL, NULL );

   hb_comp_bDontGenLineNum                   = FALSE;

   /* clear RETURN/BREAK flag */
   hb_comp_functions.pLast->bFlags           &= ~( FUN_WITH_RETURN | FUN_BREAK_CODE );

   /* Resting Compile Time Stack */
   hb_comp_functions.pLast->iStackIndex      = 0;
   hb_comp_functions.pLast->iStackFunctions  = 0;
   hb_comp_functions.pLast->iStackClasses    = 0;
}

/* Generates the pcode with the currently compiled source code line
 * if debug code was requested only
 */
void hb_compLinePushIfDebugger( void )
{
   if( hb_comp_bDebugInfo )
      hb_compLinePush();
   else
   {
      if( hb_comp_functions.pLast->bFlags & FUN_BREAK_CODE )
         /* previous line contained RETURN/BREAK/LOOP/EXIT statement */
         hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_UNREACHABLE, NULL, NULL );

      hb_comp_functions.pLast->bFlags &= ~( FUN_WITH_RETURN | FUN_BREAK_CODE );   /* clear RETURN flag */
   }
}

void hb_compLinePushIfInside( void ) /* generates the pcode with the currently compiled source code line */
{
   /* This line can be placed inside a procedure or function only
    * except EXTERNAL
    */
/*
   if( ! hb_comp_bExternal )
   {
 */
   if( ( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 ) || ( hb_comp_functions.pLast->bFlags & FUN_SEALED ) )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_OUTSIDE, NULL, NULL );
/*
   }
 */

   hb_comp_functions.pLast->bFlags |= FUN_STATEMENTS;
   hb_compLinePush();
}

/*
 * Function generates pcode for undeclared variable
 */
static void hb_compGenVariablePCode( BYTE bPCode, char * szVarName )
{
   BOOL bGenCode;

   /*
    * NOTE:
    * Clipper always assumes a memvar variable if undeclared variable
    * is popped (a value is asssigned to a variable).
    */

   if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_HARBOUR ) )
      bGenCode = hb_comp_bForceMemvars;    /* harbour compatibility */
   else
      bGenCode = ( hb_comp_bForceMemvars || bPCode == HB_P_POPVARIABLE );

   if( bGenCode )
   {
      /* -v switch was used -> assume it is a memvar variable
       */
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_MEMVAR_ASSUMED, szVarName, NULL );

      /* Ron Pinkas 2009/02/12 - if es2 is used the compilation will be stopped without the need for this flag! */
#if 0
      /* Clipper compatibility
         Should not compile with /es2 eventhough /v is used
       */
      if( hb_comp_iExitLevel == HB_EXITLEVEL_DELTARGET )
         hb_comp_AmbiguousVar = TRUE;
#endif

      if( bPCode == HB_P_POPVARIABLE )
         bPCode = HB_P_POPMEMVAR;
      else if( bPCode == HB_P_PUSHVARIABLE )
         bPCode = HB_P_PUSHMEMVAR;
      else
         bPCode = HB_P_PUSHMEMVARREF;
   }
   else
   {
      /* Ron Pinkas 2009/02/12 - redundant flag */
#if 0
      hb_comp_AmbiguousVar = TRUE;
#endif

      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_AMBIGUOUS_VAR, szVarName, NULL );
   }

   hb_compGenVarPCode( bPCode, szVarName );
}

/* Generate a pcode for a field variable
 */
static void hb_compGenFieldPCode( BYTE bPCode, int wVar, char * szVarName, PFUNCTION pFunc )
{
   PVAR pField;

   if( ! pFunc->szName )
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;
   }

   pField = hb_compVariableFind( pFunc->pFields, wVar );

   if( pField->szAlias ) /* the alias was specified in FIELD declaration
                          * Push alias symbol before the field symbol
                          */
   {
      if( bPCode == HB_P_POPFIELD )
         bPCode = HB_P_POPALIASEDFIELD;
      else if( bPCode == HB_P_PUSHFIELD )
         bPCode = HB_P_PUSHALIASEDFIELD;

      hb_compGenPushSymbol( pField->szAlias, NULL, SYMF_PUBLIC );
   }

   hb_compGenVarPCode( bPCode, szVarName );
}

/*
 * Function generates passed pcode for passed runtime variable
 * (field or memvar)
 */
static void hb_compGenVarPCode( BYTE bPCode, char * szVarName )
{
   USHORT wVar = 0;

   hb_compSymbolAdd( szVarName, &wVar, NULL, SYMF_PUBLIC );

   if( bPCode == HB_P_PUSHALIASEDFIELD && wVar <= 255 )
   {
      /* Set VU_USED flag if declared MEMVAR
       */
      if( hb_compFieldGetPos( szVarName, hb_comp_functions.pLast ) == 0 )
      {
         if( ! hb_comp_bStartProc )
            hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst );
      }

      hb_compGenPCode2( HB_P_PUSHALIASEDFIELDNEAR, ( BYTE ) wVar, ( BOOL ) 1 );
   }
   else if( bPCode == HB_P_POPALIASEDFIELD && wVar <= 255 )
   {
      /* Set VU_USED flag if declared MEMVAR
       */
      if( hb_compFieldGetPos( szVarName, hb_comp_functions.pLast ) == 0 )
      {
         if( ! hb_comp_bStartProc )
            hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst );
      }

      hb_compGenPCode2( HB_P_POPALIASEDFIELDNEAR, ( BYTE ) wVar, ( BOOL ) 1 );
   }
   else
   {
      /* Set VU_USED flag if declared MEMVAR
       */
      if( hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast ) == 0 )
      {
         if( ! hb_comp_bStartProc )
            hb_compMemvarGetPos( szVarName, hb_comp_functions.pFirst );
      }

      hb_compGenPCode3( bPCode, HB_LOBYTE( wVar ), HB_HIBYTE( wVar ), ( BOOL ) 1 );
   }
}

void hb_compGenMessage( char * szMsgName )       /* sends a message to an object */
{
   USHORT wSym = 0;

   hb_compSymbolAdd( szMsgName, &wSym, NULL, SYMF_PUBLIC );

   hb_compGenPCode3( HB_P_MESSAGE, HB_LOBYTE( wSym ), HB_HIBYTE( wSym ), ( BOOL ) 1 );
}

void hb_compGenMessageData( char * szMsg ) /* generates an underscore-symbol name for a data assignment */
{
   char * szResult = ( char * ) hb_xgrab( strlen( szMsg ) + 2 );

   hb_xstrcpy( szResult, "_", 0 );
   hb_xstrcat( szResult, szMsg, 0 );

   hb_compGenMessage( hb_compIdentifierNew( szResult, FALSE ) );
}

/* Check variable in the following order:
 * LOCAL variable
 *    local STATIC variable
 *       local FIELD variable
 *  local MEMVAR variable
 * global STATIC variable
 *    global FIELD variable
 *       global MEMVAR variable
 * (if not found - it is an undeclared variable)
 */
void hb_compGenPopVar( char * szVarName ) /* generates the pcode to pop a value from the virtual machine stack onto a variable */
{
   PFUNCTION   pFunc;
   int         iVar;

   iVar = hb_compLocalGetPos( szVarName );

   if( iVar )
   {
      /* local variable
       */
      /* local variables used in a coddeblock will not be adjusted
       * if PARAMETERS statement will be used then it is safe to
       * use 2 bytes for LOCALNEAR
       */
      if( HB_LIM_INT8( iVar ) && ! hb_comp_functions.pLast->szName )
         hb_compGenPCode2( HB_P_POPLOCALNEAR, ( BYTE ) iVar, ( BOOL ) 1 );
      else
         hb_compGenPCode3( HB_P_POPLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );

      return;
   }

   /* Check if we are generating a pop code for static variable
    * initialization function - if YES then we have to switch to a function
    * where the static variable was declared
    */
   if( ( hb_comp_functions.pLast->cScope & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
      pFunc = hb_comp_functions.pLast->pOwner;
   else
      pFunc = hb_comp_functions.pLast;

   iVar = hb_compStaticGetPos( szVarName, pFunc );

   if( iVar )
   {
      /* Static variable declared in current function
       */
      hb_compGenPCode3( HB_P_POPSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
      pFunc->bFlags |= FUN_USES_STATICS;

      return;
   }

   iVar = hb_compVariableGetPos( hb_comp_pGlobals, szVarName );

   if( iVar )
   {
      hb_compGenPCode2( HB_P_POPGLOBAL, ( BYTE ) ( iVar - 1 ), ( BOOL ) 1 );

      return;
   }

   iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pLast );

   if( iVar )
   {
      /* field declared in current function
       */
      hb_compGenFieldPCode( HB_P_POPFIELD, iVar, szVarName, hb_comp_functions.pLast );

      return;
   }

   iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast );

   if( iVar )
   {
      /* Memvar variable declared in current functions
       */
      hb_compGenVarPCode( HB_P_POPMEMVAR, szVarName );

      return;
   }

   if( ! hb_comp_bStartProc )
   {
      iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pFirst );

      if( iVar )
      {
         /* Global static variable
          */
         hb_compGenPCode3( HB_P_POPSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
         hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;

         return;
      }

      iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst );

      if( iVar )
      {
         /* Global field declaration
          */
         hb_compGenFieldPCode( HB_P_POPFIELD, iVar, szVarName, hb_comp_functions.pFirst );

         return;
      }

      iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pFirst );

      if( iVar )
      {
         /* Global Memvar variable declaration
          */
         hb_compGenVarPCode( HB_P_POPMEMVAR, szVarName );

         return;
      }
   }

   /* undeclared variable
    */
   hb_compGenVariablePCode( HB_P_POPVARIABLE, szVarName );
}

/* generates the pcode to pop a value from the virtual machine stack onto
 * an aliased variable
 */
void hb_compGenPopAliasedVar( char * szVarName,
                              BOOL bPushAliasValue,
                              char * szAlias,
                              long lWorkarea )
{

   if( bPushAliasValue )
   {
      if( szAlias )
      {
         if( szAlias[ 0 ] == 'M' && szAlias[ 1 ] == '\0' ) /* M->variable */
            hb_compGenVarPCode( HB_P_POPMEMVAR, szVarName );
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );

            if( iCmp == 0 )
               iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );

            if( iCmp == 0 ) /* MEMVAR-> or MEMVA-> or MEMV-> */
               hb_compGenVarPCode( HB_P_POPMEMVAR, szVarName );
            else /* field variable */
            {
               iCmp = strncmp( szAlias, "FIELD", 4 );

               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );

               if( iCmp == 0 ) /* FIELD-> */
                  hb_compGenVarPCode( HB_P_POPFIELD, szVarName );
               else /* database alias */
               {
                  hb_compGenPushSymbol( szAlias, NULL, SYMF_PUBLIC );
                  hb_compGenVarPCode( HB_P_POPALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea );
         hb_compGenVarPCode( HB_P_POPALIASEDFIELD, szVarName );
      }
   }
   else
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      hb_compGenVarPCode( HB_P_POPALIASEDVAR, szVarName );
}

/* generates the pcode to push a nonaliased variable value to the virtual
 * machine stack
 */
void hb_compGenPushVar( char * szVarName )
{
   int iVar;

   iVar = hb_compLocalGetPos( szVarName );

   if( iVar )
   {
      /* local variable
       */
      /* local variables used in a coddeblock will not be adjusted
       * if PARAMETERS statement will be used then it is safe to
       * use 2 bytes for LOCALNEAR
       */
      if( HB_LIM_INT8( iVar ) && ! hb_comp_functions.pLast->szName )
         hb_compGenPCode2( HB_P_PUSHLOCALNEAR, ( BYTE ) iVar, ( BOOL ) 1 );
      else
         hb_compGenPCode3( HB_P_PUSHLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );

      return;
   }

   iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pLast );

   if( iVar )
   {
      /* Static variable declared in current function
       */
      hb_compGenPCode3( HB_P_PUSHSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
      hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;

      return;
   }

   iVar = hb_compVariableGetPos( hb_comp_pGlobals, szVarName );

   if( iVar )
   {
      hb_compGenPCode2( HB_P_PUSHGLOBAL, ( BYTE ) ( iVar - 1 ), ( BOOL ) 1 );

      return;
   }

   iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pLast );

   if( iVar )
   {
      /* field declared in current function
       */
      hb_compGenFieldPCode( HB_P_PUSHFIELD, iVar, szVarName, hb_comp_functions.pLast );

      return;
   }

   iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast );

   if( iVar )
   {
      /* Memvar variable declared in current functions
       */
      hb_compGenVarPCode( HB_P_PUSHMEMVAR, szVarName );

      return;
   }

   if( ! hb_comp_bStartProc )
      iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pFirst );

   if( iVar )
   {
      /* Global static variable
       */
      hb_compGenPCode3( HB_P_PUSHSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
      hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;

      return;
   }

   if( ! hb_comp_bStartProc )
      iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst );

   if( iVar )
   {
      /* Global field declaration
       */
      hb_compGenFieldPCode( HB_P_PUSHFIELD, iVar, szVarName, hb_comp_functions.pFirst );

      return;
   }

   if( ! hb_comp_bStartProc )
      iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pFirst );

   if( iVar )
      /* Global Memvar variable declaration */
      hb_compGenVarPCode( HB_P_PUSHMEMVAR, szVarName );
   else
      /* undeclared variable
       */
      hb_compGenVariablePCode( HB_P_PUSHVARIABLE, szVarName );
}

void hb_compGenPushVarRef( char * szVarName ) /* generates the pcode to push a variable by reference to the virtual machine stack */
{
   int iVar;

   iVar = hb_compLocalGetPos( szVarName );

   if( iVar )
   {
      /* local variable
       */
      hb_compGenPCode3( HB_P_PUSHLOCALREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );

      return;
   }

   iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pLast );

   if( iVar )
   {
      /* Static variable declared in current function
       */
      hb_compGenPCode3( HB_P_PUSHSTATICREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
      hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;

      return;
   }

   iVar = hb_compVariableGetPos( hb_comp_pGlobals, szVarName );

   if( iVar )
   {
      hb_compGenPCode2( HB_P_PUSHGLOBALREF, ( BYTE ) ( iVar - 1 ), ( BOOL ) 1 );

      return;
   }

   iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pLast );

   if( iVar )
   {
      /* pushing fields by reference is not allowed */
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_REFER, szVarName, NULL );

      return;
   }

   iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast );

   if( iVar )
   {
      /* Memvar variable declared in current functions
       */
      hb_compGenVarPCode( HB_P_PUSHMEMVARREF, szVarName );

      return;
   }

   if( ! hb_comp_bStartProc )
      iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pFirst );

   if( iVar )
   {
      /* Global static variable
       */
      hb_compGenPCode3( HB_P_PUSHSTATICREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
      hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;

      return;
   }

   if( ! hb_comp_bStartProc )
      iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst );

   if( iVar )
   {
      /* pushing fields by reference is not allowed */
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_REFER, szVarName, NULL );

      return;
   }

   if( ! hb_comp_bStartProc )
      iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pFirst );

   if( iVar )
      /* Global Memvar variable declaration
       */
      hb_compGenVarPCode( HB_P_PUSHMEMVARREF, szVarName );
   else
      /* undeclared variable - field cannot be passed by the
       * reference - assume the memvar
       */
      hb_compGenVariablePCode( HB_P_PUSHMEMVARREF, szVarName );
}

void hb_compGenPushMemVarRef( char * szVarName ) /* generates the pcode to push a variable by reference to the virtual machine stack */
{
   hb_compGenVarPCode( HB_P_PUSHMEMVARREF, szVarName );
}

/* commented out because it's not used currently
 *
 * static BOOL hb_compVarFind( char * szVarName )
 * {
 * PFUNCTION pFunc = hb_comp_functions.pFirst;
 *
 * while( pFunc )
 * {
 *    if( hb_compMemvarGetPos( szVarName, pFunc ) > 0 )
 *       return TRUE;
 *
 *    pFunc = pFunc->pNext;
 * }
 *
 * return FALSE;
 * }
 */

/* generates the pcode to push an aliased variable value to the virtual
 * machine stack
 */
void hb_compGenPushAliasedVar( char * szVarName,
                               BOOL bPushAliasValue,
                               char * szAlias,
                               long lWorkarea )
{
   if( bPushAliasValue )
   {
      if( szAlias )
      {
         /* myalias->var
          * FIELD->var
          * MEMVAR->var
          */
         if( szAlias[ 0 ] == 'M' && szAlias[ 1 ] == '\0' ) /* M->variable */
         {
            if( lWorkarea == -1 )
               hb_compGenVarPCode( HB_P_PUSHMEMVARREF, szVarName );
            else
               hb_compGenVarPCode( HB_P_PUSHMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );

            if( iCmp == 0 )
               iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );

            if( iCmp == 0 ) /* MEMVAR-> or MEMVA-> or MEMV-> */
            {
               if( lWorkarea == -1 )
                  hb_compGenVarPCode( HB_P_PUSHMEMVARREF, szVarName );
               else
                  hb_compGenVarPCode( HB_P_PUSHMEMVAR, szVarName );
            }
            else /* field variable */
            {
               iCmp = strncmp( szAlias, "FIELD", 4 );

               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );

               if( iCmp == 0 ) /* FIELD-> */
                  hb_compGenVarPCode( HB_P_PUSHFIELD, szVarName );
               else /* database alias */
               {
                  hb_compGenPushSymbol( szAlias, NULL, SYMF_PUBLIC );
                  hb_compGenVarPCode( HB_P_PUSHALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea );
         hb_compGenVarPCode( HB_P_PUSHALIASEDFIELD, szVarName );
      }
   }
   else
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      hb_compGenVarPCode( HB_P_PUSHALIASEDVAR, szVarName );
}

void hb_compGenPushLogical( int iTrueFalse ) /* pushes a logical value on the virtual machine stack */
{
   hb_compGenPCode1( iTrueFalse ? ( BYTE ) HB_P_TRUE : ( BYTE ) HB_P_FALSE );
}

void hb_compGenPushNil( void )
{
   hb_compGenPCode1( HB_P_PUSHNIL );
}

/* generates the pcode to push a double number on the virtual machine stack */
void hb_compGenPushDouble( double dNumber, BYTE bWidth, BYTE bDec )
{
   BYTE pBuffer[ sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) + 1 ];

   pBuffer[ 0 ]                                       = HB_P_PUSHDOUBLE;
   HB_PUT_LE_DOUBLE( &( pBuffer[ 1 ] ), dNumber );

   pBuffer[ 1 + sizeof( double ) ]                    = bWidth;
   pBuffer[ 1 + sizeof( double ) + sizeof( BYTE ) ]   = bDec;

   hb_compGenPCodeN( pBuffer, 1 + sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ), 1 );
}

void hb_compGenPushFunCall( char * szFunName, char * szNamespace )
{
   char * szFunction;

   if( ( szNamespace && szNamespace[ 0 ] == '*' ) ||
       ( hb_comp_UsedNamespaces.pCurrent && hb_comp_UsedNamespaces.pCurrent->szName[ 0 ] == '*' ) ||
       ( szNamespace == NULL && hb_comp_Namespaces.pCurrent == NULL && hb_comp_UsedNamespaces.pCurrent == NULL ) )
   {
      /* Abbreviated function might have been used - change it for whole name
       */
#if ( ! defined( HB_RESERVED_OFF ) )
      szFunction = hb_compReservedName( szFunName );
#endif
   }
   else
      szFunction = NULL;

   if( szFunction )
   {
      szFunction = hb_compIdentifierNew( szFunction, TRUE );

      hb_compFunCallAdd( szFunction, NULL, NSF_NONE );
      hb_compGenPushSymbol( szFunction, NULL, SYMF_FUNCALL );
   }
   else
   {
      if( ( szNamespace == NULL ) && hb_comp_UsedNamespaces.pFirst )
      {
         if( hb_comp_UsedNamespaces.pCurrent )
         {
            if( hb_comp_UsedNamespaces.pCurrent->szName[ 0 ] == '*' )
               szNamespace = "*";
            else
            {
               PNAMESPACE pResolved = hb_compNamespaceFindMember( hb_comp_UsedNamespaces.pCurrent->pNext, szFunName, NSTYPE_MEMBER );

               if( pResolved )
                  szNamespace = hb_comp_UsedNamespaces.pCurrent->szFullPath;
            }
         }
         else
            szNamespace = hb_compFunctionResolveUsed( szFunName );
      }

      if( szNamespace )
      {
         if( szNamespace[ 0 ] == '*' )
         {
            hb_compFunCallAdd( szFunName, NULL, NSF_NONE );
            hb_compGenPushSymbol( szFunName, NULL, SYMF_FUNCALL );
         }
         else
         {
            hb_compFunCallAdd( szFunName, ( void * ) szNamespace, NSF_EXPLICITPATH );
            hb_compGenPushSymbol( szFunName, ( void * ) szNamespace, NSF_EXPLICITPATH );
         }
      }
      else
      {
         if( hb_comp_Namespaces.pCurrent == NULL )
         {
            hb_compFunCallAdd( szFunName, NULL, NSF_NONE );
            hb_compGenPushSymbol( szFunName, NULL, SYMF_FUNCALL );
         }
         else
         {
            hb_compFunCallAdd( szFunName, ( void * ) hb_comp_Namespaces.pCurrent, NSF_RESOLVE );
            hb_compGenPushSymbol( szFunName, ( void * ) hb_comp_Namespaces.pCurrent, NSF_RESOLVE );
         }
      }
   }
}

char * hb_compFunctionResolveUsed( char * szFunName )
{
   PNAMESPACE  pNamespace = hb_comp_UsedNamespaces.pFirst;
   PNAMESPACE  pResolved;

   while( pNamespace )
   {
      if( ( pNamespace->type & NSTYPE_SPACE ) )
      {
         if( ( pNamespace->type & NSTYPE_USED ) == NSTYPE_USED )
         {
            pResolved = hb_compNamespaceFindMember( pNamespace->pNext, szFunName, NSTYPE_MEMBER );

            if( pResolved )
               return pNamespace->szFullPath;
         }
      }

      pNamespace = pNamespace->pNext;
   }

   return NULL;
}

/* generates the pcode to push switchcase value on the virtual machine stack */
void hb_compGenSwitchCase( long lValue )
{
   BYTE  pBuffer[ 5 ];
   int   i = 0;

   pBuffer[ i ] = HB_P_SWITCHCASE;
   while( ++i < ( int ) sizeof( pBuffer ) )
   {
      pBuffer[ i ]   = HB_LOBYTE( lValue );
      lValue         >>= 8;
   }

   hb_compGenPCodeN( pBuffer, sizeof( pBuffer ), FALSE );
}

/* generates the pcode to push a long number on the virtual machine stack */
void hb_compGenPushLong( HB_LONG lNumber )
{
   if( lNumber == 0 )
      hb_compGenPCode1( HB_P_ZERO );
   else if( lNumber == 1 )
      hb_compGenPCode1( HB_P_ONE );
   else if( HB_LIM_INT8( lNumber ) )
      hb_compGenPCode2( HB_P_PUSHBYTE, ( BYTE ) lNumber, TRUE );
   else if( HB_LIM_INT16( lNumber ) )
      hb_compGenPCode3( HB_P_PUSHINT, HB_LOBYTE( lNumber ), HB_HIBYTE( lNumber ), TRUE );
   else if( HB_LIM_INT32( lNumber ) )
   {
      BYTE  pBuffer[ 5 ];
      int   i = 0;

      pBuffer[ i ] = HB_P_PUSHLONG;
      while( ++i < ( int ) sizeof( pBuffer ) )
      {
         pBuffer[ i ]   = HB_LOBYTE( lNumber );
         lNumber        >>= 8;
      }

      hb_compGenPCodeN( pBuffer, sizeof( pBuffer ), TRUE );
   }
   else
   {
      BYTE  pBuffer[ 9 ];
      int   i = 0;

      pBuffer[ i ] = HB_P_PUSHLONGLONG;
      while( ++i < ( int ) sizeof( pBuffer ) )
      {
         pBuffer[ i ]   = HB_LOBYTE( lNumber );
         lNumber        >>= 8;
      }

      hb_compGenPCodeN( pBuffer, sizeof( pBuffer ), TRUE );
   }
}

BYTE * hb_compHideString( int iType, char * szText, HB_SIZE ulStrLen, HB_SIZE * ulBufferLen )
{
   BYTE *   pBuffer;
   ULONG    ulCount;

   switch( iType )
   {
      case 1:              /* Simple XOR 0xf3 mask */
         pBuffer = ( BYTE * ) hb_xgrab( ulStrLen + 1 );

         for( ulCount = 0; ulCount < ulStrLen; ulCount++ )
            pBuffer[ ulCount ] = ( BYTE ) ( szText[ ulCount ] ^ 0xf3 );

         *ulBufferLen = ulStrLen;
         break;

      default:             /* No encode */
         pBuffer        = ( BYTE * ) hb_xgrab( ulStrLen + 1 );
         HB_MEMCPY( pBuffer, ( BYTE * ) szText, ( size_t ) ulStrLen );
         *ulBufferLen   = ulStrLen;
         break;

   }

   return pBuffer;
}

/* generates the pcode to push a date on the virtual machine stack */
void hb_compGenPushDate( long lDate, long lTime, USHORT uType )
{
   BYTE     pBuffer[ sizeof( UINT32 ) + sizeof( UINT32 ) + 1 ];
   USHORT   uLen;

   switch( uType )
   {
      case HB_ET_DDATE:
         pBuffer[ 0 ]   = HB_P_PUSHDATE;
         HB_PUT_LE_UINT32( ( pBuffer + 1 ), lDate );
         uLen           = 1 + sizeof( UINT32 );
         break;
      case HB_ET_DDATETIME:
         pBuffer[ 0 ]   = HB_P_PUSHDATETIME;
         HB_PUT_LE_UINT32( ( pBuffer + 1 ), lDate );
         HB_PUT_LE_UINT32( ( pBuffer + 5 ), lTime );
         uLen           = sizeof( UINT32 ) + sizeof( UINT32 ) + 1;
         break;
      default:
         pBuffer[ 0 ]   = 0;
         uLen           = 2;
   }

   hb_compGenPCodeN( pBuffer, uLen, 1 );
}

/* generates the pcode to push a string on the virtual machine stack */
void hb_compGenPushString( char * szText, HB_SIZE ulStrLen )
{
   BYTE * pBuffer;

   if( hb_comp_iHidden )
   {
      HB_SIZE ulBufferLen;

      pBuffer = hb_compHideString( hb_comp_iHidden, szText, ulStrLen, &ulBufferLen );

      hb_compGenPCode3( HB_P_PUSHSTRHIDDEN, HB_LOBYTE( ulStrLen ), HB_HIBYTE( ulStrLen ), TRUE );
      hb_compGenPCode3( ( BYTE ) hb_comp_iHidden, HB_LOBYTE( ulBufferLen ), HB_HIBYTE( ulBufferLen ), TRUE );
      hb_compGenPCodeN( pBuffer, ulBufferLen, 1 );
   }
   else if( ulStrLen > 255 )
   {
      pBuffer        = ( BYTE * ) hb_xgrab( ulStrLen + 3 );

      pBuffer[ 0 ]   = HB_P_PUSHSTR;
      pBuffer[ 1 ]   = HB_LOBYTE( ulStrLen );
      pBuffer[ 2 ]   = HB_HIBYTE( ulStrLen );

      HB_MEMCPY( ( BYTE * ) ( &( pBuffer[ 3 ] ) ), ( BYTE * ) szText, ( size_t ) ulStrLen );

      hb_compGenPCodeN( pBuffer, ulStrLen + 3, 1 );
   }
   else
   {
      pBuffer        = ( BYTE * ) hb_xgrab( ulStrLen + 2 );

      pBuffer[ 0 ]   = HB_P_PUSHSTRSHORT;
      pBuffer[ 1 ]   = ( BYTE ) ulStrLen;

      HB_MEMCPY( ( BYTE * ) ( &( pBuffer[ 2 ] ) ), ( BYTE * ) szText, ( size_t ) ulStrLen );

      hb_compGenPCodeN( pBuffer, ulStrLen + 2, 1 );
   }

   hb_xfree( pBuffer );
}

/* generates the pcode to push a symbol on the virtual machine stack */
void hb_compGenPushSymbol( char * szSymbolName, void * Namespace, int iFlags )
{
   USHORT wSym = 0;

   hb_compSymbolAdd( szSymbolName, &wSym, Namespace, iFlags );

   if( wSym > 255 )
      hb_compGenPCode3( HB_P_PUSHSYM, HB_LOBYTE( wSym ), HB_HIBYTE( wSym ), ( BOOL ) 1 );
   else
      hb_compGenPCode2( HB_P_PUSHSYMNEAR, ( BYTE ) wSym, ( BOOL ) 1 );
}

static void hb_compCheckDuplVars( PVAR pVar, char * szVarName )
{
   while( pVar )
   {
      if( pVar->szName && pVar->szName == szVarName )
      {
         hb_compErrorDuplVar( szVarName );
         break;
      }
      else
         pVar = pVar->pNext;
   }
}


#define OPT_LOCAL_FLAG_BLOCK    0x01
#define OPT_LOCAL_FLAG_PUSH     0x02
#define OPT_LOCAL_FLAG_POP      0x04
#define OPT_LOCAL_FLAG_PUSHREF  0x08
#define OPT_LOCAL_FLAG_POPSELF  0x10
#define OPT_LOCAL_FLAG_CHANGE   0x20

typedef struct
{
   SHORT isNumber;
   HB_BYTE  bFlags;
} HB_OPT_LOCAL, * PHB_OPT_LOCAL;

static HB_BOOL hb_compIsIsJump( HB_BYTE bPCode )
{
   return ( bPCode >= HB_P_JUMPNEAR && bPCode <= HB_P_JUMPTRUEFAR ) ||
            bPCode == HB_P_SEQBEGIN || bPCode == HB_P_SEQEND ||
            bPCode == HB_P_TRYBEGIN || bPCode == HB_P_TRYEND || bPCode == HB_P_TRYRECOVER;
}

static SHORT hb_compLocalGetNumber( HB_BYTE * pCode )
{
   switch( *pCode )
   {
      case HB_P_POPLOCALNEAR:
      case HB_P_PUSHLOCALNEAR:
      case HB_P_LOCALNEARADDINT:
      case HB_P_LOCALNEARSETINT:
      case HB_P_LOCALNEARSETSTR:
      case HB_P_LOCALNEARSETSTRHIDDEN:
      case HB_P_LOCALNEARADD:
      case HB_P_LOCALNEARINC:
      case HB_P_LOCALNEARDEC:
      case HB_P_PUSHLOCALNEARINC:
      case HB_P_PUSHLOCALNEARDEC:
         return ( SHORT ) ( ( signed char ) pCode[ 1 ] );

      case HB_P_LOCALNAME:
      case HB_P_POPLOCAL:
      case HB_P_PUSHLOCAL:
      case HB_P_PUSHLOCALREF:
         return HB_PCODE_MKSHORT( pCode + 1 );
   }
   return 0;
}

static HB_ISIZ hb_compJumpGetOffset( HB_BYTE * pCode )
{
   switch( *pCode )
   {
      case HB_P_JUMPNEAR:
      case HB_P_JUMPFALSENEAR:
      case HB_P_JUMPTRUENEAR:
         return *( ( signed char * ) pCode + 1 );

      case HB_P_JUMP:
      case HB_P_JUMPFALSE:
      case HB_P_JUMPTRUE:
         return HB_PCODE_MKSHORT( pCode + 1 );

      case HB_P_JUMPFAR:
      case HB_P_JUMPFALSEFAR:
      case HB_P_JUMPTRUEFAR:
      case HB_P_SEQBEGIN:
      case HB_P_SEQEND:
      case HB_P_TRYBEGIN:
      case HB_P_TRYEND:
      case HB_P_TRYRECOVER:
         return HB_PCODE_MKINT24( pCode + 1 );
   }
   return 0;
}

static HB_BOOL hb_compHasJump( PFUNCTION pFunc, HB_SIZE nPos )
{
   HB_SIZE nJump;

   for( nJump = 0; nJump < pFunc->iJumps; nJump++ )
   {
      HB_SIZE nJumpAddr = pFunc->pJumps[ nJump ];
      switch( pFunc->pCode[ nJumpAddr ] )
      {
         case HB_P_JUMPNEAR:
         case HB_P_JUMPFALSENEAR:
         case HB_P_JUMPTRUENEAR:
            nJumpAddr += ( signed char ) pFunc->pCode[ nJumpAddr + 1 ];
            break;

         case HB_P_JUMP:
         case HB_P_JUMPFALSE:
         case HB_P_JUMPTRUE:
            nJumpAddr += HB_PCODE_MKSHORT( &pFunc->pCode[ nJumpAddr + 1 ] );
            break;

         case HB_P_NOOP:
         case HB_P_POP:
            nJumpAddr = nPos + 1;
            break;

         default:
            nJumpAddr += HB_PCODE_MKINT24( &pFunc->pCode[ nJumpAddr + 1 ] );
            break;
      }
      if( nJumpAddr == nPos )
         return HB_TRUE;
   }

   return HB_FALSE;
}

static void hb_compPCodeEnumScanLocals( PFUNCTION pFunc, PHB_OPT_LOCAL pLocals )
{
   HB_SIZE nPos = 0, nPredPos = 0;
   SHORT   isVar;
   HB_BOOL fWasJump = 0;

   while( nPos < pFunc->lPCodePos )
   {
      if( hb_compIsIsJump( pFunc->pCode[ nPos ] ) )
         fWasJump = HB_TRUE;

      isVar = hb_compLocalGetNumber( &pFunc->pCode[ nPos ] );

      if( pFunc->pCode[ nPos ] == HB_P_PUSHBLOCK )
      {
         HB_BYTE * pCode = &pFunc->pCode[ nPos + 5 ];
         HB_USHORT usVarCount, usVar;

         usVarCount = HB_PCODE_MKUSHORT( pCode );
         while( usVarCount-- )
         {
            pCode += 2;
            usVar = HB_PCODE_MKUSHORT( pCode );
            if( usVar > 0 )
               pLocals[ usVar - 1 ].bFlags |= OPT_LOCAL_FLAG_BLOCK;
         }
      }
      else if( isVar > 0 )
         switch( pFunc->pCode[ nPos ] )
         {
            case HB_P_POPLOCALNEAR:
            case HB_P_POPLOCAL:
               if( nPos > 0 && pFunc->pCode[ nPredPos ] == HB_P_PUSHSELF &&
                  ! hb_compHasJump( pFunc, nPos ) && ! fWasJump )
                  pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_POPSELF;
               else
                  pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_POP;
               break;

            case HB_P_PUSHLOCALNEARINC:
            case HB_P_PUSHLOCALNEARDEC:
               pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_CHANGE;
			   /* fallthrough */
               /*break;*/

            case HB_P_PUSHLOCALNEAR:
            case HB_P_PUSHLOCAL:
               pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_PUSH;
               break;

            case HB_P_PUSHLOCALREF:
               pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_PUSHREF;
               break;

            case HB_P_LOCALNEARADDINT:
            case HB_P_LOCALNEARINC:
            case HB_P_LOCALNEARDEC:
            case HB_P_LOCALNEARADD:
            case HB_P_LOCALNEARSETINT:
            case HB_P_LOCALNEARSETSTR:
            case HB_P_LOCALNEARSETSTRHIDDEN:
               pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_CHANGE;
               break;
         }

      nPredPos = nPos;
      nPos += hb_compPCodeSize( pFunc, nPos );
   }
}

static HB_BOOL hb_compPCodeTraceAssignedUnused( PFUNCTION pFunc, HB_SIZE nPos, HB_BYTE * pMap, SHORT isLocal )
{
   for( ;; )
   {
      if( pMap[ nPos ] )
         return HB_FALSE;

      pMap[ nPos ] = 1;

      if( pFunc->pCode[ nPos ] == HB_P_POPLOCAL ||
          pFunc->pCode[ nPos ] == HB_P_POPLOCALNEAR ||
          pFunc->pCode[ nPos ] == HB_P_PUSHLOCAL ||
          pFunc->pCode[ nPos ] == HB_P_PUSHLOCALNEAR ||
          pFunc->pCode[ nPos ] == HB_P_PUSHLOCALREF ||
          pFunc->pCode[ nPos ] == HB_P_LOCALNEARADDINT ||
          pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETINT ||
          pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETSTR ||
          pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETSTRHIDDEN ||
          pFunc->pCode[ nPos ] == HB_P_LOCALNEARADD ||
          pFunc->pCode[ nPos ] == HB_P_LOCALNEARINC ||
          pFunc->pCode[ nPos ] == HB_P_LOCALNEARDEC ||
          pFunc->pCode[ nPos ] == HB_P_PUSHLOCALNEARINC ||
          pFunc->pCode[ nPos ] == HB_P_PUSHLOCALNEARDEC )
      {
         if( hb_compLocalGetNumber( pFunc->pCode + nPos ) == isLocal )
         {
            if( pFunc->pCode[ nPos ] == HB_P_POPLOCAL ||
                pFunc->pCode[ nPos ] == HB_P_POPLOCALNEAR ||
                pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETINT ||
                pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETSTR ||
                pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETSTRHIDDEN )
               return HB_FALSE;
            else
               return HB_TRUE;
         }
      }

      if( hb_compIsIsJump( pFunc->pCode[ nPos ] ) )
      {
         HB_SIZE nPos2 = nPos + hb_compJumpGetOffset( &pFunc->pCode[ nPos ] );

         if( pFunc->pCode[ nPos ] == HB_P_JUMP ||
             pFunc->pCode[ nPos ] == HB_P_JUMPNEAR ||
             pFunc->pCode[ nPos ] == HB_P_JUMPFAR )
         {
            nPos = nPos2;
            continue;
         }

         if( hb_compPCodeTraceAssignedUnused( pFunc, nPos2, pMap, isLocal ) )
            return HB_TRUE;
      }
      else if( pFunc->pCode[ nPos ] == HB_P_SWITCHCASE )
      {
         nPos += 5;

         if( hb_compPCodeTraceAssignedUnused( pFunc, nPos, pMap, isLocal ) )
            return HB_TRUE;

         nPos += hb_compPCodeSize( pFunc, nPos );
         continue;
      }

      if( pFunc->pCode[ nPos ] == HB_P_ENDPROC || pFunc->pCode[ nPos ] == HB_P_ENDBLOCK )
         break;

      nPos += hb_compPCodeSize( pFunc, nPos );
   }
   return HB_FALSE;
}

static void hb_compPCodeEnumAssignedUnused( PFUNCTION pFunc, PHB_OPT_LOCAL pLocals )
{
   HB_BYTE * pMap;
   HB_SIZE   nPos = 0, nPredPos = 0;
   SHORT     nLocal, nBase = 0, nLine = 0;
   HB_BOOL   fCheck;

   pMap = ( HB_BYTE * ) hb_xgrab( pFunc->lPCodePos );

   while( nPos < pFunc->lPCodePos )
   {
      fCheck = ( pFunc->pCode[ nPos ] == HB_P_POPLOCAL ||
                 pFunc->pCode[ nPos ] == HB_P_POPLOCALNEAR ) &&
                 ! ( nPos > 0 && pFunc->pCode[ nPredPos ] == HB_P_PUSHNIL );

      if( ! fCheck && ( pFunc->pCode[ nPos ] == HB_P_LOCALNEARADDINT ||
                        pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETINT ||
                        pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETSTR ||
                        pFunc->pCode[ nPos ] == HB_P_LOCALNEARSETSTRHIDDEN ||
                        pFunc->pCode[ nPos ] == HB_P_LOCALNEARADD ||
                        pFunc->pCode[ nPos ] == HB_P_LOCALNEARINC ||
                        pFunc->pCode[ nPos ] == HB_P_LOCALNEARDEC ) )
         fCheck = HB_TRUE;

      if( fCheck && ( nLocal = hb_compLocalGetNumber( &pFunc->pCode[ nPos ] ) ) > ( SHORT ) pFunc->wParamCount )
      {
         if( ( pLocals[ nLocal - 1 ].bFlags & ( OPT_LOCAL_FLAG_BLOCK | OPT_LOCAL_FLAG_PUSHREF ) ) == 0 &&
             pLocals[ nLocal - 1 ].bFlags != OPT_LOCAL_FLAG_POPSELF &&
             pLocals[ nLocal - 1 ].bFlags != ( OPT_LOCAL_FLAG_PUSH | OPT_LOCAL_FLAG_POPSELF ) )
         {
            memset( pMap, 0, pFunc->lPCodePos );
            pMap[ nPos ] = 1;

            if( ! hb_compPCodeTraceAssignedUnused( pFunc, nPos + hb_compPCodeSize( pFunc, nPos ), pMap, nLocal ) )
            {
               SHORT is;
               PVAR  pVar = pFunc->pLocals;
               char  szFun[ 256 ];

               for( is = 1; is < nLocal; is++ )
                  pVar = pVar->pNext;

               hb_snprintf( szFun, sizeof( szFun ), "%s(%i)", pFunc->szName, nLine );
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGNED_UNUSED, pVar->szName, szFun );
            }
         }
      }
      else if( pFunc->pCode[ nPos ] == HB_P_BASELINE )
         nLine = nBase = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPos + 1 ] );
      else if( pFunc->pCode[ nPos ] == HB_P_LINEOFFSET )
         nLine = nBase + ( SHORT ) pFunc->pCode[ nPos + 1 ];

      nPredPos = nPos;
      nPos    += hb_compPCodeSize( pFunc, nPos );
   }
   hb_xfree( pMap );
}

static void hb_compPCodeTraceOptimizer( PFUNCTION pFunc )
{
   PHB_OPT_LOCAL pLocals;
   PVAR          pVar;
   HB_USHORT     usLocalCount, usIndex;
   HB_SIZE       nPos;

   /* Many (perhaps ALL) functions of pcode trace optimization dependes on pcodes.
      Please, check these functions if new pcode is added, or existing changed.
      Special attention should be paid, if new pcode introduces branching, codeblocks,
      or are related to parameters, local variables. [Mindaugas] */

   usLocalCount = 0;
   pVar = pFunc->pLocals;
   while( pVar )
   {
      pVar = pVar->pNext;
      ++usLocalCount;
   }

   if( ! usLocalCount )
      return;

   nPos = 0;
   while( nPos < pFunc->lPCodePos )
   {
      if( pFunc->pCode[ nPos ] == HB_P_PARAMETER )
         return;
      nPos += hb_compPCodeSize( pFunc, nPos );
   }

   pLocals = ( PHB_OPT_LOCAL ) hb_xgrab( sizeof( HB_OPT_LOCAL ) * usLocalCount );
   memset( pLocals, 0, sizeof( HB_OPT_LOCAL ) * usLocalCount );
   hb_compPCodeEnumScanLocals( pFunc, pLocals );

   usIndex = 0;
   pVar = pFunc->pLocals;
   while( pVar )
   {
      if( usIndex >= pFunc->wParamCount && pLocals[ usIndex ].bFlags == OPT_LOCAL_FLAG_PUSH )
      {
         char szFun[ 256 ];

         hb_snprintf( szFun, sizeof( szFun ), "%s(%i)", pFunc->szName, pVar->iDeclLine );
         hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NEVER_ASSIGNED, pVar->szName, szFun );
      }

      pVar = pVar->pNext;
      ++usIndex;
   }

   hb_compPCodeEnumAssignedUnused( pFunc, pLocals );

   hb_xfree( pLocals );
}

void hb_compFinalizeFunction( void ) /* fixes all last defined function returns jumps offsets */
{
   PFUNCTION pFunc = hb_comp_functions.pLast;

   if( pFunc )
   {
      if( ( pFunc->bFlags & FUN_WITH_RETURN ) == 0 )
      {
         /* The last statement in a function/procedure was not a RETURN
          * Generate end-of-procedure pcode
          */
         hb_compGenPCode1( HB_P_ENDPROC );
      }

      if( pFunc->bFlags & FUN_USES_LOCAL_PARAMS )
      {
         USHORT PCount = pFunc->wParamCount;

         /* do not adjust if local parameters are used -remove NOOPs only */
         pFunc->wParamCount = 0;
         /* There was a PARAMETERS statement used.
          * NOTE: This fixes local variables references in a case when
          * there is PARAMETERS statement after a LOCAL variable declarations.
          * All local variables are numbered from 1 - which means use first
          * item from the eval stack. However if PARAMETERS statement is used
          * then there are additional items on the eval stack - the
          * function arguments. Then first local variable is at the position
          * (1 + <number of arguments>). We cannot fix this numbering
          * because the PARAMETERS statement can be used even at the end
          * of function body when all local variables are already created.
          */

         hb_compFixFuncPCode( pFunc );
         pFunc->wParamCount = PCount;
      }
      else
         hb_compFixFuncPCode( pFunc );

      hb_compOptimizeJumps();

      if( hb_comp_iWarnings )
      {
         /*
            REVIEW: based on VU_INITIALIZED - but NIL is a default VALID value!!!
            hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NOT_INITIALIZED, pVar->szName, NULL );
          */

         if( pFunc->szName && pFunc->szName[ 0 ] )
         {
            PVAR pVar;

            if( hb_comp_bWarnUnUsedLocals )
            {
               pVar = pFunc->pLocals;

               while( pVar )
               {
                  if( pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
                  {
                     char szFun[ 256 ];

                     hb_snprintf( szFun, sizeof( szFun ), "%s(%i)", pFunc->szName, pVar->iDeclLine );
                     hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
                  }

                  pVar = pVar->pNext;
               }

               if( hb_comp_iWarnings >= 3 )
                  hb_compPCodeTraceOptimizer( pFunc );
            }

            if( hb_comp_bWarnUnUsedStatics )
            {
               pVar = pFunc->pStatics;

               while( pVar )
               {
                  if( pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
                  {
                     char szFun[ 256 ];

                     hb_snprintf( szFun, sizeof( szFun ), "%s(%i)", pFunc->szName, pVar->iDeclLine );
                     hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
                  }

                  pVar = pVar->pNext;
               }
            }

            if( hb_comp_bWarnUnUsedMemvars )
            {
               pVar = pFunc->pMemvars;

               while( pVar )
               {
                  if( pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
                  {
                     char szFun[ 256 ];

                     hb_snprintf( szFun, sizeof( szFun ), "%s(%i)", pFunc->szName, pVar->iDeclLine );
                     hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
                  }

                  pVar = pVar->pNext;
               }
            }

            if( hb_comp_bWarnUnUsedFields )
            {
               pVar = pFunc->pFields;

               while( pVar )
               {
                  if( pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
                  {
                     char szFun[ 256 ];

                     hb_snprintf( szFun, sizeof( szFun ), "%s(%i)", pFunc->szName, pVar->iDeclLine );
                     hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
                  }

                  pVar = pVar->pNext;
               }
            }
         }

         /* Check if the function returned some value
          */
         if( ( pFunc->bFlags & FUN_WITH_RETURN ) == 0 && ( pFunc->bFlags & FUN_PROCEDURE ) == 0 && ( hb_comp_functions.pLast->bFlags & FUN_BREAK_CODE ) == 0 )
            hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_FUN_WITH_NO_RETURN, pFunc->szName, NULL );

         /* Compile Time Strong Type Checking is not needed any more. */
         if( pFunc->pStack )
            hb_xfree( ( void * ) pFunc->pStack );

         pFunc->iStackSize       = 0;
         pFunc->iStackIndex      = 0;
         pFunc->iStackFunctions  = 0;
         pFunc->iStackClasses    = 0;
      }
   }
}

static void hb_compOptimizeFrames( PFUNCTION pFunc )
{
   USHORT   w;
   int      iOffset = 0;

   if( pFunc == NULL )
   {
      return;
   }

   if( pFunc == hb_comp_pInitFunc )
   {
      if( pFunc->pCode[ 0 ] == HB_P_STATICS && pFunc->pCode[ 5 ] == HB_P_SFRAME )
      {
         hb_compSymbolFind( hb_comp_pInitFunc->szName, &w, NULL, SYMF_FUNCALL );

         pFunc->pCode[ 1 ] = HB_LOBYTE( w );
         pFunc->pCode[ 2 ] = HB_HIBYTE( w );
         pFunc->pCode[ 6 ] = HB_LOBYTE( w );
         pFunc->pCode[ 7 ] = HB_HIBYTE( w );

         /* Remove the SFRAME pcode if there's no global static
            initialization: */

         /* NOTE: For some reason this will not work for the static init
            function, so I'm using an ugly hack instead. [vszakats] */
/*       if( !( pFunc->bFlags & FUN_USES_STATICS ) ) */
         if( pFunc->pCode[ 8 ] == HB_P_ENDPROC )
         {
            pFunc->lPCodePos -= 3;
            memmove( pFunc->pCode + 5, pFunc->pCode + 8, ( size_t ) ( pFunc->lPCodePos - 5 ) );
         }
      }
   }
   else if( pFunc->pCode[ 0 ] == HB_P_FRAME &&
            pFunc->pCode[ 3 ] == HB_P_SFRAME )
   {
      PVAR  pLocal;
      int   iLocals = 0;
      BOOL  bSkipFRAME;
      BOOL  bSkipSFRAME;

      pLocal = pFunc->pLocals;

      while( pLocal )
      {
         if( hb_comp_VariableList != NULL )
            fprintf( hb_comp_VariableList, "%s=%i\n", pLocal->szName, pLocal->iUsed );

         pLocal = pLocal->pNext;
         iLocals++;
      }

      if( iLocals || pFunc->wParamCount )
      {
         if( pFunc->bFlags & FUN_USES_LOCAL_PARAMS )
            iLocals -= pFunc->wParamCount;

         if( iLocals < 256 )
            pFunc->pCode[ 1 ] = ( BYTE ) ( iLocals );
         else
         {
            iOffset = 1;

            if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 1 )
               pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += 1 );

            memmove( pFunc->pCode + 3, pFunc->pCode + 2, ( size_t ) ( pFunc->lPCodePos - 2 ) );
            pFunc->lPCodePos++;

            pFunc->pCode[ 0 ] = HB_P_LARGEFRAME;
            pFunc->pCode[ 1 ] = HB_LOBYTE( iLocals );
            pFunc->pCode[ 2 ] = HB_HIBYTE( iLocals );
         }

         /* SomeFunc( ... ) - Variable number paramaters. */
         if( pFunc->pCode[ 2 + iOffset ] < HB_VAR_PARAM_FLAG )
            pFunc->pCode[ 2 + iOffset ] = ( BYTE ) ( pFunc->wParamCount );

         bSkipFRAME = FALSE;
      }
      else
         bSkipFRAME = TRUE;

      if( pFunc->bFlags & FUN_USES_STATICS )
      {
         hb_compSymbolFind( hb_comp_pInitFunc->szName, &w, NULL, SYMF_FUNCALL );

         pFunc->pCode[ 4 + iOffset ]   = HB_LOBYTE( w );
         pFunc->pCode[ 5 + iOffset ]   = HB_HIBYTE( w );

         bSkipSFRAME                   = FALSE;
      }
      else
         bSkipSFRAME = TRUE;

      /* Remove the frame pcodes if they are not needed */

      if( bSkipFRAME && bSkipSFRAME )
      {
         pFunc->lPCodePos -= 6;
         memmove( pFunc->pCode, pFunc->pCode + 6, ( size_t ) pFunc->lPCodePos );
      }
      else if( bSkipFRAME )
      {
         pFunc->lPCodePos -= 3;
         memmove( pFunc->pCode, pFunc->pCode + 3, ( size_t ) pFunc->lPCodePos );
      }
      else if( bSkipSFRAME )
      {
         pFunc->lPCodePos -= 3;
         memmove( pFunc->pCode + 3 + iOffset, pFunc->pCode + 6 + iOffset, ( size_t ) ( pFunc->lPCodePos - 3 ) );
      }

      /* hb_xfree( szFunctionName ); */
   }
}

static int hb_compSort_ULONG( const void * pLeft, const void * pRight )
{
   ULONG ulLeft   = *( ( ULONG * ) ( pLeft ) );
   ULONG ulRight  = *( ( ULONG * ) ( pRight ) );

   if( ulLeft == ulRight )
      return 0;
   else if( ulLeft < ulRight )
      return -1;
   else
      return 1;
}

void hb_compNOOPfill( PFUNCTION pFunc, HB_SIZE ulFrom, int iCount, BOOL fPop, BOOL fCheck )
{
   ULONG ul;

   while( iCount-- )
   {
      if( fPop )
      {
         pFunc->pCode[ ulFrom ]  = HB_P_POP;
         fPop                    = FALSE;
      }
      else if( fCheck && pFunc->pCode[ ulFrom ] == HB_P_NOOP && pFunc->iNOOPs )
      {
         for( ul = 0; ul < pFunc->iNOOPs; ++ul )
         {
            if( pFunc->pNOOPs[ ul ] == ulFrom )
               break;
         }
         if( ul == pFunc->iNOOPs )
            hb_compNOOPadd( pFunc, ulFrom );
      }
      else
      {
         pFunc->pCode[ ulFrom ] = HB_P_NOOP;
         hb_compNOOPadd( pFunc, ulFrom );
      }
      ++ulFrom;
   }
}

BOOL hb_compIsJump( PFUNCTION pFunc, HB_SIZE ulPos )
{
   ULONG iJump;

   /*
    * Do not allow any optimization (code striping) when Jump Optimization
    * is disabled and we do not have any information about jump addreses
    */
   if( ! HB_COMP_ISSUPPORTED( HB_COMPFLAG_OPTJUMP ) )
      return TRUE;

   for( iJump = 0; iJump < pFunc->iJumps; iJump++ )
   {
      HB_SIZE ulJumpAddr = pFunc->pJumps[ iJump ];
      switch( pFunc->pCode[ ulJumpAddr ] )
      {
         case HB_P_JUMPNEAR:
         case HB_P_JUMPFALSENEAR:
         case HB_P_JUMPTRUENEAR:
            ulJumpAddr += ( signed char ) pFunc->pCode[ ulJumpAddr + 1 ];
            break;

         case HB_P_JUMP:
         case HB_P_JUMPFALSE:
         case HB_P_JUMPTRUE:
            ulJumpAddr += HB_PCODE_MKSHORT( &pFunc->pCode[ ulJumpAddr + 1 ] );
            break;

         default:
            ulJumpAddr += HB_PCODE_MKINT24( &pFunc->pCode[ ulJumpAddr + 1 ] );
            break;
      }
      if( ulJumpAddr == ulPos )
         return TRUE;
   }

   return FALSE;
}

/* Jump Optimizer and dummy code eliminator */
static void hb_compOptimizeJumps( void )
{
   BYTE *      pCode = hb_comp_functions.pLast->pCode;
   HB_SIZE *   pNOOPs, * pJumps;
   HB_SIZE     ulOptimized, ulNextByte, ulBytes2Copy, ulJumpAddr, iNOOP, iJump;
   int         iPass;

   if( ! HB_COMP_ISSUPPORTED( HB_COMPFLAG_OPTJUMP ) )
   {
      return;
   }

   hb_compCodeTraceMarkDead( hb_comp_functions.pLast );

   for( iPass = 0; iPass < 3; ++iPass )
   {
      LONG lOffset;

      if( iPass == 2 && ! hb_comp_bDebugInfo && hb_comp_bLineNumbers )
         hb_compStripFuncLines( hb_comp_functions.pLast );

      if( hb_comp_functions.pLast->iJumps > 0 )
      {
         pJumps   = hb_comp_functions.pLast->pJumps;
         iJump    = hb_comp_functions.pLast->iJumps - 1;

         do
         {
            ulJumpAddr = pJumps[ iJump ];

            /*
             * optimize existing jumps, it will be good to also join
             * unconditional jump chain calculating total jump offset but
             * it will be necessary to add some code to protect against
             * infinite loop which will appear when we add optimization
             * for the PCODE sequences like:
             *
             *    HB_P_{FALSE|TRUE},
             * [ no jump targets or stack modification here ]
             *    HB_P_JUMP{FALSE|TRUE}*,
             *
             * I'll think about sth like that later, [druzus]
             */
            switch( pCode[ ulJumpAddr ] )
            {
               case HB_P_JUMPNEAR:
                  if( ( signed char ) pCode[ ulJumpAddr + 1 ] == 2 )
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr, 2, FALSE, FALSE );
                  break;

               case HB_P_JUMPFALSENEAR:
               case HB_P_JUMPTRUENEAR:
                  if( ( signed char ) pCode[ ulJumpAddr + 1 ] == 2 )
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr, 2, TRUE, FALSE );
                  break;

               case HB_P_JUMP:
                  lOffset = HB_PCODE_MKSHORT( &pCode[ ulJumpAddr + 1 ] );

                  if( lOffset == 3 )
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr, 3, FALSE, FALSE );
                  else if( HB_LIM_INT8( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMPNEAR;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 2, 1, FALSE, FALSE );
                  }
                  break;

               case HB_P_JUMPFALSE:
                  lOffset = HB_PCODE_MKSHORT( &pCode[ ulJumpAddr + 1 ] );

                  if( lOffset == 3 )
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr, 3, TRUE, FALSE );
                  else if( HB_LIM_INT8( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMPFALSENEAR;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 2, 1, FALSE, FALSE );
                  }
                  break;

               case HB_P_JUMPTRUE:
                  lOffset = HB_PCODE_MKSHORT( &pCode[ ulJumpAddr + 1 ] );

                  if( lOffset == 3 )
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr, 3, TRUE, FALSE );
                  else if( HB_LIM_INT8( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMPTRUENEAR;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 2, 1, FALSE, FALSE );
                  }
                  break;

               case HB_P_JUMPFAR:
                  lOffset = HB_PCODE_MKINT24( &pCode[ ulJumpAddr + 1 ] );

                  if( lOffset == 4 )
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr, 4, FALSE, FALSE );
                  else if( HB_LIM_INT8( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMPNEAR;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 2, 2, FALSE, FALSE );
                  }
                  else if( HB_LIM_INT16( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMP;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 3, 1, FALSE, FALSE );
                  }
                  break;

               case HB_P_JUMPFALSEFAR:
                  lOffset = HB_PCODE_MKINT24( &pCode[ ulJumpAddr + 1 ] );

                  if( lOffset == 4 )
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr, 4, TRUE, FALSE );
                  else if( HB_LIM_INT8( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMPFALSENEAR;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 2, 2, FALSE, FALSE );
                  }
                  else if( HB_LIM_INT16( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMPFALSE;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 3, 1, FALSE, FALSE );
                  }
                  break;

               case HB_P_JUMPTRUEFAR:
                  lOffset = HB_PCODE_MKINT24( &pCode[ ulJumpAddr + 1 ] );

                  if( lOffset == 4 )
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr, 4, TRUE, FALSE );
                  else if( HB_LIM_INT8( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMPTRUENEAR;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 2, 2, FALSE, FALSE );
                  }
                  else if( HB_LIM_INT16( lOffset ) )
                  {
                     pCode[ ulJumpAddr ] = HB_P_JUMPTRUE;
                     hb_compNOOPfill( hb_comp_functions.pLast, ulJumpAddr + 3, 1, FALSE, FALSE );
                  }
                  break;
            }

            /* remove dummy jumps (over dead code) */
            if( pCode[ ulJumpAddr ] == HB_P_NOOP || pCode[ ulJumpAddr ] == HB_P_POP )
            {
               if( hb_comp_functions.pLast->iJumps > iJump + 1 )
                  memmove( &pJumps[ iJump ], &pJumps[ iJump + 1 ], ( size_t ) ( hb_comp_functions.pLast->iJumps - iJump - 1 ) * sizeof( HB_SIZE ) );

               hb_comp_functions.pLast->iJumps--;
            }
         }
         while( iJump-- );

         if( hb_comp_functions.pLast->iJumps == 0 )
         {
            hb_xfree( hb_comp_functions.pLast->pJumps );
            hb_comp_functions.pLast->pJumps = NULL;
         }
      }

      if( hb_comp_functions.pLast->iNOOPs == 0 )
         return;

      pNOOPs = hb_comp_functions.pLast->pNOOPs;

      /* Needed so the pasting of PCODE pieces below will work correctly */
      qsort( ( void * ) pNOOPs, ( size_t ) hb_comp_functions.pLast->iNOOPs, sizeof( HB_SIZE ), hb_compSort_ULONG );

      if( hb_comp_functions.pLast->iJumps )
      {
         LONG *   plSizes, * plShifts;
         HB_SIZE  ulSize;

         pJumps   = hb_comp_functions.pLast->pJumps;
         ulSize   = sizeof( HB_SIZE ) * hb_comp_functions.pLast->iJumps;
         plSizes  = ( LONG * ) hb_xgrab( ulSize );
         plShifts = ( LONG * ) hb_xgrab( ulSize );

         for( iJump = 0; iJump < hb_comp_functions.pLast->iJumps; iJump++ )
            plSizes[ iJump ] = plShifts[ iJump ] = 0;

         /* First Scan NOOPS - Adjust Jump addresses. */
         for( iNOOP = 0; iNOOP < hb_comp_functions.pLast->iNOOPs; iNOOP++ )
         {
            /* Adjusting preceding jumps that pooint to code beyond the current NOOP
               or trailing backward jumps pointing to lower address. */
            for( iJump = 0; iJump < hb_comp_functions.pLast->iJumps; iJump++ )
            {
               ulJumpAddr = pJumps[ iJump ];

               switch( pCode[ ulJumpAddr ] )
               {
                  case HB_P_JUMPNEAR:
                  case HB_P_JUMPFALSENEAR:
                  case HB_P_JUMPTRUENEAR:
                     lOffset = ( signed char ) pCode[ ulJumpAddr + 1 ];
                     break;

                  case HB_P_JUMP:
                  case HB_P_JUMPFALSE:
                  case HB_P_JUMPTRUE:
                     lOffset = HB_PCODE_MKSHORT( &pCode[ ulJumpAddr + 1 ] );
                     break;

                  case HB_P_JUMPFAR:
                  case HB_P_JUMPTRUEFAR:
                  case HB_P_JUMPFALSEFAR:
                  case HB_P_SEQBEGIN:
                  case HB_P_SEQEND:
                  case HB_P_TRYBEGIN:
                  case HB_P_TRYEND:
                  case HB_P_TRYRECOVER:
                     lOffset = HB_PCODE_MKINT24( &pCode[ ulJumpAddr + 1 ] );
                     break;

                  default:
                     hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_NOT_FOUND, NULL, NULL );
                     continue;
               }

               /* update jump size */
               if( lOffset > 0 ) /* forward (positive) jump */
               {
                  /* Only if points to code beyond the current fix. */
                  if( pNOOPs[ iNOOP ] > ulJumpAddr &&
                      pNOOPs[ iNOOP ] < ( ULONG ) ( ulJumpAddr + lOffset ) )
                     plSizes[ iJump ]--;
               }
               else /* if( lOffset < 0 ) - backword (negative) jump */
               {
                  /* Only if points to code prior the current fix. */
                  if( pNOOPs[ iNOOP ] < ulJumpAddr &&
                      pNOOPs[ iNOOP ] >= ( ULONG ) ( ulJumpAddr + lOffset ) )
                     plSizes[ iJump ]++;
               }

               /* update jump address */
               if( pNOOPs[ iNOOP ] < ulJumpAddr )
                  plShifts[ iJump ]++;
            }
         }

         for( iJump = 0; iJump < hb_comp_functions.pLast->iJumps; iJump++ )
         {
            lOffset = plSizes[ iJump ];

            if( lOffset != 0 )
            {
               ulJumpAddr = pJumps[ iJump ];
               switch( pCode[ ulJumpAddr ] )
               {
                  case HB_P_JUMPNEAR:
                  case HB_P_JUMPFALSENEAR:
                  case HB_P_JUMPTRUENEAR:
                     lOffset                 += ( signed char ) pCode[ ulJumpAddr + 1 ];
                     pCode[ ulJumpAddr + 1 ] = HB_LOBYTE( lOffset );
                     break;

                  case HB_P_JUMP:
                  case HB_P_JUMPFALSE:
                  case HB_P_JUMPTRUE:
                     lOffset += HB_PCODE_MKSHORT( &pCode[ ulJumpAddr + 1 ] );
                     HB_PUT_LE_UINT16( &pCode[ ulJumpAddr + 1 ], lOffset );
                     break;

                  default:
                     lOffset += HB_PCODE_MKINT24( &pCode[ ulJumpAddr + 1 ] );
                     HB_PUT_LE_UINT24( &pCode[ ulJumpAddr + 1 ], lOffset );
                     break;
               }
            }

            pJumps[ iJump ] -= plShifts[ iJump ];
         }

         hb_xfree( plSizes );
         hb_xfree( plShifts );
      }

      ulOptimized = ulNextByte = 0;
      /* Second Scan, after all adjustements been made, we can copy the optimized code. */
      for( iNOOP = 0; iNOOP < hb_comp_functions.pLast->iNOOPs; iNOOP++ )
      {
         ulBytes2Copy = ( pNOOPs[ iNOOP ] - ulNextByte );

         memmove( pCode + ulOptimized, pCode + ulNextByte, ( size_t ) ulBytes2Copy );

         ulOptimized += ulBytes2Copy;
         ulNextByte  += ulBytes2Copy;

         /* Skip the NOOP and point to next valid byte */
         ulNextByte++;
      }

      ulBytes2Copy                        = ( hb_comp_functions.pLast->lPCodePos - ulNextByte );
      memmove( pCode + ulOptimized, pCode + ulNextByte, ( size_t ) ulBytes2Copy );
      ulOptimized                         += ulBytes2Copy;

      hb_comp_functions.pLast->lPCodePos  = ulOptimized;
      hb_comp_functions.pLast->lPCodeSize = ulOptimized;

      hb_xfree( hb_comp_functions.pLast->pNOOPs );
      hb_comp_functions.pLast->pNOOPs     = NULL;
      hb_comp_functions.pLast->iNOOPs     = 0;
   }
}

/* Generate the opcode to open BEGIN/END sequence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * - either the address of HB_P_SEQEND opcode if there is no RECOVER clause
 * - or the address of RECOVER code
 */
HB_SIZE hb_compSequenceBegin( void )
{
   hb_compGenPCode4( HB_P_SEQBEGIN, 0, 0, 0, ( BOOL ) 0 );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

/* Generate the opcode to close BEGIN/END sequence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * the address of first line after END SEQUENCE
 * This opcode will be executed if recover code was not requested (as the
 * last statement in code beetwen BEGIN ... RECOVER) or if BREAK was requested
 * and there was no matching RECOVER clause.
 */
HB_SIZE hb_compSequenceEnd( void )
{
   hb_compGenPCode4( HB_P_SEQEND, 0, 0, 0, ( BOOL ) 0 );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

/* Generate the opcode to open TRY/END tryuence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * - either the address of HB_P_TRYEND opcode if there is no RECOVER clause
 * - or the address of RECOVER code
 */
HB_SIZE hb_compTryBegin( void )
{
   hb_compGenPCode4( HB_P_TRYBEGIN, 0, 0, 0, ( BOOL ) 0 );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

/* Generate the opcode to close TRY/END tryuence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * the address of first line after END TRYUENCE
 * This opcode will be executed if recover code was not requested (as the
 * last statement in code beetwen BEGIN ... RECOVER) or if BREAK was requested
 * and there was no matching RECOVER clause.
 */
HB_SIZE hb_compTryEnd( void )
{
   hb_compGenPCode4( HB_P_TRYEND, 0, 0, 0, ( BOOL ) 0 );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

/* Remove unnecessary opcodes in case there were no executable statements
 * beetwen BEGIN and RECOVER sequence
 */

void hb_compSequenceFinish( HB_SIZE ulStartPos, int bUsualStmts )
{
   if( ! hb_comp_bDebugInfo ) /* only if no debugger info is required */
   {
      if( ! bUsualStmts )
      {
         if( ! HB_COMP_ISSUPPORTED( HB_COMPFLAG_OPTJUMP ) )
         {
            hb_comp_functions.pLast->lPCodePos  = ulStartPos - 1; /* remove also HB_P_SEQBEGIN */
            hb_comp_ulLastLinePos               = ulStartPos - 5;
         }
         else
         {
            /*
             * We can safely remove the dead code when Jump Optimization
             * is enabled by replacing it with HB_P_NOOP PCODEs - which
             * will be later eliminated and jump data updated.
             */
            while( ulStartPos <= hb_comp_functions.pLast->lPCodePos )
            {
               hb_comp_functions.pLast->pCode[ ulStartPos - 1 ] = HB_P_NOOP;
               hb_compNOOPadd( hb_comp_functions.pLast, ulStartPos - 1 );
               ++ulStartPos;
            }

            hb_comp_ulLastLinePos = ulStartPos - 5;
         }
      }
   }
}

/* Set the name of an alias for the list of previously declared FIELDs
 *
 * szAlias -> name of the alias
 * iField  -> position of the first FIELD name to change
 */
void hb_compFieldSetAlias( char * szAlias, int iField )
{
   PVAR pVar;

   pVar = hb_comp_functions.pLast->pFields;

   while( iField-- && pVar )
      pVar = pVar->pNext;

   while( pVar )
   {
      pVar->szAlias  = szAlias;
      pVar           = pVar->pNext;
   }
}

/* This functions counts the number of FIELD declaration in a function
 * We will required this information in hb_compFieldSetAlias function
 */
int hb_compFieldsCount( void )
{
   int   iFields  = 0;
   PVAR  pVar     = hb_comp_functions.pLast->pFields;

   while( pVar )
   {
      ++iFields;
      pVar = pVar->pNext;
   }

   return iFields;
}

/*
 * Start of definition of static variable
 * We are using here the special function hb_comp_pInitFunc which will store
 * pcode needed to initialize all static variables declared in PRG module.
 * pOwner member will point to a function where the static variable is
 * declared:
 */
void hb_compStaticDefStart( void )
{
   hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;

   if( ! hb_comp_pInitFunc )
   {
      BYTE pBuffer[ 5 ];

      hb_comp_pInitFunc          = hb_compFunctionNew( hb_compExpr_IDs.__INITSTATICS__, HB_FS_INIT );
      hb_comp_pInitFunc->pOwner  = hb_comp_functions.pLast;
      hb_comp_pInitFunc->bFlags  = FUN_USES_STATICS | FUN_PROCEDURE;
      hb_comp_pInitFunc->cScope  = HB_FS_INITEXIT;
      hb_comp_functions.pLast    = hb_comp_pInitFunc;

      pBuffer[ 0 ]               = HB_P_STATICS;
      pBuffer[ 1 ]               = 0;
      pBuffer[ 2 ]               = 0;
      pBuffer[ 3 ]               = 1; /* the number of static variables is unknown now */
      pBuffer[ 4 ]               = 0;

      hb_compGenPCodeN( pBuffer, 5, 0 );

      hb_compGenPCode3( HB_P_SFRAME, 0, 0, ( BOOL ) 0 );     /* frame for statics variables */

      if( hb_comp_bDebugInfo )
      {
         hb_comp_ulSavedModuleNamePos = hb_comp_ulLastModuleNamePos;
         hb_compGenModuleName( hb_pp_fileName( hb_comp_PP ), NULL );
      }
   }
   else
   {
      hb_comp_pInitFunc->pOwner  = hb_comp_functions.pLast;
      hb_comp_functions.pLast    = hb_comp_pInitFunc;

      if( hb_comp_bDebugInfo )
      {
         hb_comp_ulSavedModuleNamePos = hb_comp_ulLastModuleNamePos;
      }
   }
}

/*
 * End of definition of static variable
 * Return to previously pcoded function.
 */
void hb_compStaticDefEnd( void )
{
   hb_comp_functions.pLast    = hb_comp_pInitFunc->pOwner;
   hb_comp_pInitFunc->pOwner  = NULL;
   ++hb_comp_iStaticCnt;

   if( hb_comp_bDebugInfo )
      hb_comp_ulLastModuleNamePos = hb_comp_ulSavedModuleNamePos;
}

void hb_compGlobalsDefStart( void )
{
   if( ! hb_comp_pGlobalsFunc )
   {
      hb_comp_pGlobalsFunc          = hb_compFunctionNew( hb_compIdentifierNew( "[_INITGLOBALS]", TRUE ), HB_FS_INIT );
      hb_comp_pGlobalsFunc->pOwner  = hb_comp_functions.pLast;
      hb_comp_pGlobalsFunc->bFlags  = FUN_PROCEDURE;
      hb_comp_pGlobalsFunc->cScope  = HB_FS_INITEXIT;
      hb_comp_functions.pLast       = hb_comp_pGlobalsFunc;

      if( hb_comp_bDebugInfo )
      {
         hb_comp_ulSavedModuleNamePos = hb_comp_ulLastModuleNamePos;
         hb_compGenModuleName( hb_pp_fileName( hb_comp_PP ), NULL );
      }
   }
   else
   {
      hb_comp_pGlobalsFunc->pOwner  = hb_comp_functions.pLast;
      hb_comp_functions.pLast       = hb_comp_pGlobalsFunc;

      if( hb_comp_bDebugInfo )
         hb_comp_ulSavedModuleNamePos = hb_comp_ulLastModuleNamePos;
   }
}

/*
 * End of definition of global variable
 * Return to previously pcoded function.
 */
void hb_compGlobalsDefEnd( void )
{
   hb_comp_functions.pLast       = hb_comp_pGlobalsFunc->pOwner;
   hb_comp_pGlobalsFunc->pOwner  = NULL;

   if( hb_comp_bDebugInfo )
      hb_comp_ulLastModuleNamePos = hb_comp_ulSavedModuleNamePos;
}

/*
 * Start of stop line number info generation
 */
static void hb_compLineNumberDefStart( void )
{
   if( ! hb_comp_pLineNumberFunc )
   {
      hb_comp_pLineNumberFunc          = hb_compFunctionNew( hb_compExpr_IDs.__INITLINES__, HB_FS_INIT );
      hb_comp_pLineNumberFunc->pOwner  = hb_comp_functions.pLast;
      hb_comp_pLineNumberFunc->bFlags  = 0;
      hb_comp_pLineNumberFunc->cScope  = HB_FS_INITEXIT;
      hb_comp_functions.pLast          = hb_comp_pLineNumberFunc;

      if( hb_comp_bDebugInfo )
         hb_compGenModuleName( hb_comp_pFileName->szName, NULL );
   }
   else
   {
      hb_comp_pLineNumberFunc->pOwner  = hb_comp_functions.pLast;
      hb_comp_functions.pLast          = hb_comp_pLineNumberFunc;
   }
}

/*
 * End of stop line number info generation
 * Return to previously pcoded function.
 */
static void hb_compLineNumberDefEnd( void )
{
   hb_comp_functions.pLast          = hb_comp_pLineNumberFunc->pOwner;
   hb_comp_pLineNumberFunc->pOwner  = NULL;
}

/*
 * Start a new fake-function that will hold pcodes for a codeblock
 */
void hb_compCodeBlockStart( void )
{
   PFUNCTION pBlock;

#if defined( HB_COMP_DEBUG )
   printf( "[hb_compCodeBlockStart] Block start\n" );
#endif

   pBlock                  = hb_compFunctionNew( NULL, HB_FS_STATIC );

   pBlock->pOwner          = hb_comp_functions.pLast;
   pBlock->iStaticsBase    = hb_comp_functions.pLast->iStaticsBase;

   hb_comp_functions.pLast = pBlock;
}

PHB_EXPR hb_compCodeBlockEnd( BOOL bExt )
{
   PFUNCTION pCodeblock;    /* pointer to the current codeblock */
   PFUNCTION pFunc;         /* pointer to a function that owns a codeblock */
   char *    pFuncName;     /* name (if one exists) of the function that owns a codeblock */
   USHORT    wSize;
   USHORT    wLocals = 0;   /* number of referenced local variables */
   USHORT    wLocalsCnt, wLocalsLen;
   USHORT    wPos;
   int       iLocalPos;
   PVAR      pVar, pFree;

   PHB_EXPR  pExtBlock;
   PFUNCTION pFunExtBlock = NULL;

   hb_compGenPCode1( HB_P_ENDBLOCK ); /* finish the codeblock */
   //hb_compFixFuncPCode( hb_comp_functions.pLast );
   hb_compOptimizeJumps();

   pCodeblock = hb_comp_functions.pLast;

   if( bExt )
   {
      /*
       * Container for the complete HB_P_PUSHBLOCK and the block itslef.
       * The complete PCODE will later be used in hb_compExprUseExtBlock.
       */
      pFunExtBlock            = hb_compFunctionNew( NULL, HB_FS_STATIC );
      hb_comp_functions.pLast = pFunExtBlock;
   }
   else
   {
      /*
       * Return to pcode buffer of function/codeblock in which the current
       * codeblock was defined
       */
      hb_comp_functions.pLast = pCodeblock->pOwner;
   }

   /* find the function that owns the codeblock */
   pFunc       = pCodeblock->pOwner;
   pFuncName   = pFunc->szName;

   while( pFunc->pOwner )
   {
      pFunc = pFunc->pOwner;

      if( pFunc->szName && *pFunc->szName )
         pFuncName = pFunc->szName;
   }

   pFunc->bFlags |= ( BYTE ) ( pCodeblock->bFlags & FUN_USES_STATICS );

   /* generate a proper codeblock frame with a codeblock size and with
    * a number of expected parameters
    */
   /* QUESTION: would be 64kB enough for a codeblock size?
    * we are assuming now a USHORT for a size of codeblock
    */

   /* Count the number of referenced local variables */
   wLocalsLen  = 0;
   pVar        = pCodeblock->pStatics;

   while( pVar )
   {
      if( hb_comp_bDebugInfo )
         wLocalsLen += ( USHORT ) ( 4 + strlen( pVar->szName ) );

      pVar = pVar->pNext;
      ++wLocals;
   }

   wLocalsCnt  = wLocals;

   /* NOTE: 2 = HB_P_PUSHBLOCK | HB_P_PUSHBLOCKSHORT + BYTE( size ) + _ENDBLOCK */
   wSize       = ( USHORT ) ( pCodeblock->lPCodePos + 2 );

   if( hb_comp_bDebugInfo )
   {
      wSize += ( USHORT ) ( 3 + strlen( hb_pp_fileName( hb_comp_PP ) ) + strlen( pFuncName ) );
      wSize += wLocalsLen;
   }

   if( bExt )
      wSize += ( USHORT ) 3; /* HB_P_FRAME l, p */

   if( wSize <= 255 && pCodeblock->wParamCount == 0 && wLocals == 0 )
      hb_compGenPCode2( HB_P_PUSHBLOCKSHORT, ( BYTE ) wSize, ( BOOL ) 0 );
   else
   {
      /* NOTE: 5 = BYTE( size ) + USHORT( wParams ) + USHORT( wLocals ) */
      wSize += ( USHORT ) ( 5 + wLocals * 2 );

      hb_compGenPCode3( HB_P_PUSHBLOCK, HB_LOBYTE( wSize ), HB_HIBYTE( wSize ), ( BOOL ) 0 );
      hb_compGenPCode2( HB_LOBYTE( pCodeblock->wParamCount ), HB_HIBYTE( pCodeblock->wParamCount ), ( BOOL ) 0 );
      hb_compGenPCode2( HB_LOBYTE( wLocals ), HB_HIBYTE( wLocals ), ( BOOL ) 0 );
   }

   /* generate the table of referenced local variables */
   pVar = pCodeblock->pStatics;

   while( wLocals-- )
   {
      wPos  = hb_compVariableGetPos( pFunc->pLocals, pVar->szName );
      hb_compGenPCode2( HB_LOBYTE( wPos ), HB_HIBYTE( wPos ), ( BOOL ) 0 );

      pVar  = pVar->pNext;
   }

   if( bExt )
   {
      wLocals  = 0;
      pVar     = pCodeblock->pLocals;
      while( pVar )
      {
         wLocals++;
         pVar = pVar->pNext;
      }

      if( wLocals )
         wLocals -= pCodeblock->wParamCount;

      hb_compGenPCode3( HB_P_FRAME, ( BYTE ) wLocals, ( BYTE ) pCodeblock->wParamCount, ( BOOL ) 0 );   /* frame for locals and parameters */
   }

   if( hb_comp_bDebugInfo )
   {
      hb_compGenModuleName( hb_pp_fileName( hb_comp_PP ), pFuncName );

      /* generate the names of referenced local variables */
      pVar        = pCodeblock->pStatics;
      iLocalPos   = -1;

      while( wLocalsCnt-- )
      {
         hb_compGenLocalName( ( USHORT ) iLocalPos, pVar->szName );
         iLocalPos--;

         pVar = pVar->pNext;
      }
   }

   hb_compGenPCodeN( pCodeblock->pCode, pCodeblock->lPCodePos, ( BOOL ) 0 );
   hb_xfree( ( void * ) pCodeblock->pCode );

   if( bExt )
   {
      pExtBlock = hb_compExprNewExtBlock( pFunExtBlock->pCode, pFunExtBlock->lPCodePos );

      /* pFunExtBlock->pCode = NULL; */
      hb_xfree( ( void * ) pFunExtBlock );

      /* return to pcode buffer of function in which the extened
       * codeblock was defined in.
       */
      hb_comp_functions.pLast = pCodeblock->pOwner;
   }
   else
      pExtBlock = NULL;

   /* Cleanup the codeblock wrapper function. */

   /* Release Locals. */
   pVar = pCodeblock->pLocals;
   while( pVar )
   {
      if( hb_comp_bWarnUnUsedBlockParams && pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
         hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_BLOCKVAR_NOT_USED, pVar->szName, pFunc->szName );

      /* free used variables */
      pFree = pVar;

      pVar  = pVar->pNext;
      hb_xfree( ( void * ) pFree );
   }

   pVar = pCodeblock->pStatics;
   while( pVar )
   {
      /* free used variables */
      pFree = pVar;

      pVar  = pVar->pNext;
      hb_xfree( ( void * ) pFree );
   }

   /* Release the NOOP array. */
   if( pCodeblock->pNOOPs )
      hb_xfree( ( void * ) pCodeblock->pNOOPs );

   /* Release the Jumps array. */
   if( pCodeblock->pJumps )
      hb_xfree( ( void * ) pCodeblock->pJumps );

   /* Compile Time Strong Type Checking Stack is not needed any more. */
   if( pCodeblock->pStack )
      hb_xfree( ( void * ) pCodeblock->pStack );

   /*
      pCodeblock->iStackSize      = 0;
      pCodeblock->iStackIndex     = 0;
      pCodeblock->iStackFunctions = 0;
      pCodeblock->iStackClasses   = 0;
    */

   hb_xfree( ( void * ) pCodeblock );

   return pExtBlock;
}

/* ************************************************************************* */

/* initialize support variables */
static void hb_compInitVars( void )
{
   hb_comp_functions.iCount         = 0;
   hb_comp_functions.pFirst         = NULL;
   hb_comp_functions.pLast          = NULL;
   hb_comp_funcalls.iCount          = 0;
   hb_comp_funcalls.pFirst          = NULL;
   hb_comp_funcalls.pLast           = NULL;
   hb_comp_symbols.iCount           = 0;
   hb_comp_symbols.pFirst           = NULL;
   hb_comp_symbols.pLast            = NULL;
   hb_comp_szAnnounce               = NULL;
   hb_comp_pInitFunc                = NULL;
   hb_comp_bAnyWarning              = FALSE;

   hb_comp_iLine                    = 1;
   hb_comp_iBaseLine                = 0;
   hb_comp_iFunctionCnt             = 0;
   hb_comp_iErrorCount              = 0;
   hb_comp_cVarType                 = ' ';
   hb_comp_ulLastLinePos            = 0;
   hb_comp_ulLastOffsetPos          = 0;
   hb_comp_iStaticCnt               = 0;
   hb_comp_iVarScope                = VS_LOCAL;
   hb_comp_exprs                    = NULL;

   hb_comp_inlines.iCount           = 0;
   hb_comp_inlines.pFirst           = NULL;
   hb_comp_inlines.pLast            = NULL;

   hb_comp_pGlobalsFunc             = NULL;
   hb_comp_pGlobals                 = NULL;
   hb_comp_iGlobals                 = 0;

   hb_comp_pEnum                    = NULL;

   hb_comp_pLineNumberFunc          = NULL;

   hb_comp_Namespaces.pFirst        = NULL;
   hb_comp_Namespaces.pLast         = NULL;
   hb_comp_Namespaces.pCurrent      = NULL;

   hb_comp_UsedNamespaces.pFirst    = NULL;
   hb_comp_UsedNamespaces.pLast     = NULL;
   hb_comp_UsedNamespaces.pCurrent  = NULL;
}

PNAMESPACE hb_compNamespaceEnumDeep( PNAMESPACE pStart, PHB_COMP_NS_ENUMFUN pEnumFunc, void ** pCargo )
{
   PNAMESPACE pNamespace = pStart->pNext;

   pStart->type   |= NSENUM_SPACEADVISE;
   pEnumFunc( pStart, pCargo );
   pStart->type   &= ~NSENUM_SPACEADVISE;

   while( pNamespace )
   {
      if( pNamespace->type & NSTYPE_SPACE )
         pNamespace = hb_compNamespaceEnumDeep( pNamespace, pEnumFunc, pCargo );
      else
      {
         if( pNamespace && ( pNamespace->type & NSTYPE_END ) )
            break;
      }

      pNamespace = pNamespace->pNext;
   }

   pStart->type   |= NSENUM_SPACEBEGIN;
   pEnumFunc( pStart, pCargo );
   pStart->type   &= ~NSENUM_SPACEBEGIN;

   pNamespace     = pStart->pNext;
   while( pNamespace )
   {
      pEnumFunc( pNamespace, pCargo );

      if( pNamespace->type & NSTYPE_END )
      {
         pStart->type   |= NSENUM_SPACEEND;
         pEnumFunc( pStart, pCargo );
         pStart->type   &= ~NSENUM_SPACEEND;

         while( ( pNamespace->type & NSTYPE_END ) && pNamespace->pNext && ( pNamespace->pNext->type & NSTYPE_SPACE ) )
            pNamespace = hb_compNamespaceEnumDeep( pNamespace->pNext, pEnumFunc, pCargo );

         return pNamespace;
      }

      if( ( pNamespace->type & NSTYPE_SPACE ) )
      {
         if( ( pNamespace->type & NSTYPE_END ) != NSTYPE_END )
            pNamespace = hb_compNamespaceEnumSkipMembers( pNamespace->pNext );
      }

      pNamespace = pNamespace->pNext;
   }

   assert( 0 );
   return NULL;
}

PNAMESPACE hb_compNamespaceEnumSkipMembers( PNAMESPACE pNamespace )
{
   int iLevel = 1;

   while( pNamespace )
   {
      if( pNamespace->type & NSTYPE_SPACE )
         iLevel++;

      /* Don't else, maybe both if empty! */
      if( pNamespace->type & NSTYPE_END )
      {
         if( --iLevel == 0 )
            return pNamespace;
      }

      pNamespace = pNamespace->pNext;
   }

   assert( 0 );
   return NULL;
}


void hb_compNamespaceEnumSpaces( PNAMESPACE pStart, PHB_COMP_NS_ENUMFUN pEnumFunc, void ** pCargo )
{
   PNAMESPACE pNamespace;

   pNamespace = pStart;

   while( pNamespace )
   {
      if( pNamespace->type & NSTYPE_SPACE )
      {
         pEnumFunc( pNamespace, pCargo );
         pNamespace = hb_compNamespaceEnumSkipMembers( pNamespace->pNext );
      }

      pNamespace = pNamespace->pNext;
   }
}

void hb_compNamespaceEnumerate( PHB_COMP_NS_ENUMFUN pEnumFunc, void ** pCargo )
{
   PNAMESPACE pNamespace = hb_comp_Namespaces.pFirst;

   while( pNamespace )
   {
      pEnumFunc( pNamespace, pCargo );

      pNamespace = pNamespace->pNext;
   }
}

PNAMESPACE hb_compNamespaceNew( char * szName, int iType )
{
   PNAMESPACE pNew;

   pNew           = ( PNAMESPACE ) hb_xgrab( sizeof( _NAMESPACE ) );

   pNew->type     = iType;
   pNew->szName   = szName;
   pNew->pNext    = NULL;

   if( hb_comp_Namespaces.pCurrent )
   {
      char * szFullPath = ( char * ) hb_xgrab( strlen( hb_comp_Namespaces.pCurrent->szFullPath ) + 1 + strlen( szName ) + 1 );

      hb_xstrcpy( szFullPath, hb_comp_Namespaces.pCurrent->szFullPath, ".", szName, NULL );
      pNew->szFullPath = hb_compIdentifierNew( szFullPath, FALSE );
   }
   else
      pNew->szFullPath = szName;

   if( hb_comp_Namespaces.pFirst )
   {
      if( iType & NSTYPE_SPACE )
         pNew->iID = hb_comp_Namespaces.pLast->iID + 1;
      else
         pNew->iID = hb_comp_Namespaces.pLast->iID;

      hb_comp_Namespaces.pLast->pNext = pNew;
   }
   else
   {
      pNew->iID                  = 1;

      hb_comp_Namespaces.pFirst  = pNew;
   }

   pNew->pOuter   = hb_comp_Namespaces.pCurrent;
   pNew->pFunc    = NULL;

   if( iType & NSTYPE_SPACE )
   {
      if( pNew->pOuter )
         pNew->type |= pNew->pOuter->type;

      hb_comp_Namespaces.pCurrent = pNew;
   }

   hb_comp_Namespaces.pLast = pNew;

   return pNew;
}

void hb_compNamespaceEnd( void )
{
   if( ( hb_comp_Namespaces.pLast->type & NSTYPE_END ) )
      hb_compNamespaceNew( "", NSTYPE_TERMINATOR );
   else
      hb_comp_Namespaces.pLast->type |= NSTYPE_END;

   hb_comp_Namespaces.pCurrent = hb_comp_Namespaces.pCurrent->pOuter;
}

PNAMESPACE hb_compNamespaceFind( PNAMESPACE pNamespace, char * szName, int type )
{
   while( pNamespace )
   {
      if( pNamespace->szName == szName && ( ( pNamespace->type & type ) == type ) )
         return pNamespace;

      pNamespace = pNamespace->pNext;
   }

   return NULL;
}

PNAMESPACE hb_compNamespaceFindMember( PNAMESPACE pNamespace, char * szName, int type )
{
   while( pNamespace )
   {
      if( pNamespace->szName == szName && ( ( pNamespace->type & type ) == type ) )
         return pNamespace;

      if( ( pNamespace->type & NSTYPE_END ) )
      {
         if( ( pNamespace->type & NSTYPE_SPACE ) == 0 )
            return NULL;
      }
      else if( ( pNamespace->type & NSTYPE_SPACE ) )
         pNamespace = hb_compNamespaceEnumSkipMembers( pNamespace->pNext );

      pNamespace = pNamespace->pNext;
   }

   return NULL;
}

PNAMESPACE hb_compUsedNamespaceNew( char * szName, int iType )
{
   PNAMESPACE pNew;

   pNew           = ( PNAMESPACE ) hb_xgrab( sizeof( _NAMESPACE ) );

   pNew->type     = iType;
   pNew->szName   = szName;
   pNew->iID      = 0;
   pNew->pNext    = NULL;

   if( hb_comp_UsedNamespaces.pCurrent && ( ( iType & NSF_EXPLICITPATH ) != NSF_EXPLICITPATH ) )
   {
      char * szFullPath = ( char * ) hb_xgrab( strlen( hb_comp_UsedNamespaces.pCurrent->szFullPath ) + 1 + strlen( szName ) + 1 );

      hb_xstrcpy( szFullPath, hb_comp_UsedNamespaces.pCurrent->szFullPath, ".", szName, NULL );
      pNew->szFullPath = hb_compIdentifierNew( szFullPath, FALSE );
   }
   else
      pNew->szFullPath = szName;

   if( hb_comp_UsedNamespaces.pFirst )
      hb_comp_UsedNamespaces.pLast->pNext = pNew;
   else
      hb_comp_UsedNamespaces.pFirst = pNew;

   pNew->pOuter   = hb_comp_UsedNamespaces.pCurrent;
   pNew->pFunc    = NULL;

   if( iType & NSTYPE_SPACE )
   {
      if( pNew->pOuter )
         pNew->type |= pNew->pOuter->type;

      hb_comp_UsedNamespaces.pCurrent = pNew;
   }

   hb_comp_UsedNamespaces.pLast = pNew;

   return pNew;
}

void hb_compUsedNamespaceEnd( void )
{
   if( ( hb_comp_UsedNamespaces.pLast->type & NSTYPE_END ) )
      hb_compUsedNamespaceNew( hb_comp_UsedNamespaces.pCurrent->szName, NSTYPE_TERMINATOR );
   else
      hb_comp_UsedNamespaces.pLast->type |= NSTYPE_END;

   hb_comp_UsedNamespaces.pCurrent = hb_comp_UsedNamespaces.pCurrent->pOuter;
}

static void hb_compGenOutput( int iLanguage, const char * szSourceExtension )
{

   switch( iLanguage )
   {
      case LANG_C:
         hb_compGenCCode( hb_comp_pFileName, szSourceExtension );
         break;

      case LANG_PORT_OBJ:
         hb_compGenPortObj( hb_comp_pFileName );
         break;

      case LANG_OBJ_MODULE:
         hb_compGenCObj( hb_comp_pFileName, szSourceExtension );
         break;
   }
}

static void hb_compOutputFile( void )
{
   hb_comp_pFileName->szPath        = NULL;
   hb_comp_pFileName->szExtension   = NULL;

   /* we create the output file name */
   if( hb_comp_pOutPath )
   {
      if( hb_comp_pOutPath->szPath )
         hb_comp_pFileName->szPath = hb_comp_pOutPath->szPath;

      if( hb_comp_pOutPath->szName )
      {
         hb_comp_pFileName->szName = hb_comp_pOutPath->szName;

         if( hb_comp_pOutPath->szExtension )
            hb_comp_pFileName->szExtension = hb_comp_pOutPath->szExtension;
      }
   }
}

static void hb_compAddInitFunc( PFUNCTION pFunc )
{
   PCOMSYMBOL pSym = hb_compSymbolAdd( pFunc->szName, NULL, NULL, SYMF_FUNCALL | SYMF_STATIC );

   pSym->cScope                     |= pFunc->cScope;

   hb_comp_functions.pLast->pNext   = pFunc;
   hb_comp_functions.pLast          = pFunc;

   hb_compGenPCode1( HB_P_ENDPROC );

   ++hb_comp_functions.iCount;
}

static int hb_compCompile( char * szPrg )
{
   int         iStatus = EXIT_SUCCESS;
   PHB_FNAME   pFileName;
   BOOL        bFunc, bKillGlobalsFunc = FALSE;
   char *      szTempName;
   ULONG       i, u = 0;


   hb_comp_pFileName = hb_fsFNameSplit( szPrg );

   if( hb_comp_pFileName->szName )
   {
      char           szFileName[ HB_PATH_MAX ];
      char           sz__File__[ HB_PATH_MAX + 2 ];
      char           szPpoName[ HB_PATH_MAX ];
      char           szPptName[ HB_PATH_MAX ];
      char           szHILName[ HB_PATH_MAX ];
      char           szVarListName[ HB_PATH_MAX ];
      const char *   szSourceExtension;

      /* Clear and reinitialize preprocessor state */
      hb_pp_reset( hb_comp_PP );

      if( hb_comp_iLanguage == LANG_PORT_OBJ )
         hb_pp_addDefine( hb_comp_PP, "__HRB__", "1" );

      if( ! hb_comp_pFileName->szExtension )
         hb_comp_pFileName->szExtension = ".prg";

      szTempName           = ( char * ) hb_xgrab( HB_PATH_MAX );
      hb_comp_PrgFileName  = ( char * ) hb_xgrab( HB_PATH_MAX );
      hb_snprintf( szTempName, HB_PATH_MAX, "%s%s%s", hb_comp_pFileName->szPath ? hb_comp_pFileName->szPath : "", hb_comp_pFileName->szName, hb_comp_pFileName->szExtension );


      for( i = 0; i < strlen( szTempName ); i++ )
      {
         if( szTempName[ i ] == '\\' )
         {
            hb_comp_PrgFileName[ u++ ] = '\\';
            hb_comp_PrgFileName[ u++ ] = '\\';
         }
         else
            hb_comp_PrgFileName[ u++ ] = szTempName[ i ];
      }

      hb_comp_PrgFileName[ u ]   = 0;
      hb_xfree( szTempName );

      szSourceExtension          = hb_comp_pFileName->szExtension;

      hb_fsFNameMerge( szFileName, hb_comp_pFileName );

      /* defined __FILE__ */
      hb_snprintf( sz__File__, sizeof( sz__File__ ), "\"%s\"", szFileName );
      hb_pp_addDefine( hb_comp_PP, "__FILE__", sz__File__ );

      /* Local Variable List (.var) File
         hb_comp_iGenVarList is TRUE if /gcS is used
       */
      if( hb_comp_iGenVarList )
      {
         hb_comp_pFileName->szExtension   = ".var";
         hb_fsFNameMerge( szVarListName, hb_comp_pFileName );
         hb_comp_VariableList             = hb_fopen( szVarListName, "w" );
      }

      if( hb_comp_bPPO )
      {
         hb_comp_pFileName->szExtension = ".ppo";

         if( hb_comp_ppo_pOutPath )
            hb_comp_pFileName->szPath = hb_comp_ppo_pOutPath->szPath;

         hb_fsFNameMerge( szPpoName, hb_comp_pFileName );

         if( ! hb_pp_outFile( hb_comp_PP, szPpoName, NULL ) )
            iStatus = EXIT_FAILURE;

         if( hb_comp_bTracePP && iStatus == EXIT_SUCCESS )
         {
            hb_comp_pFileName->szExtension = ".ppt";

            if( hb_comp_ppo_pOutPath )
               hb_comp_pFileName->szPath = hb_comp_ppo_pOutPath->szPath;

            hb_fsFNameMerge( szPptName, hb_comp_pFileName );

            if( ! hb_pp_traceFile( hb_comp_PP, szPptName, NULL ) )
               iStatus = EXIT_FAILURE;
         }
      }

      /* Giancarlo Niccolai: opening/creating i18n file */
      if( hb_comp_bI18n )
      {
         const char * szExt;

         /* Get correct filename */
         if( hb_comp_szHILout == NULL )
         {
            szExt                            = hb_comp_pFileName->szExtension;
            hb_comp_pFileName->szExtension   = ".hil";
            hb_fsFNameMerge( szHILName, hb_comp_pFileName );
            hb_comp_pFileName->szExtension   = szExt;
         }
         else
            hb_xstrcpy( szHILName, hb_comp_szHILout, 0 );

         /* if the file already exists... */
         hb_comp_HILfile = hb_fopen( szHILName, "r+b" );

         if( hb_comp_HILfile != NULL )
            /* ... just go to the end */
            fseek( hb_comp_HILfile, 0, SEEK_END );
         else
         {
            HB_I18N_TAB_HEADER head;

            /* try to create it... */
            hb_comp_HILfile = hb_fopen( szHILName, "wb" );
            if( hb_comp_HILfile == NULL )
            {
               hb_compGenError( hb_comp_szErrors,
                                'E', HB_COMP_ERR_CREATE_HIL,
                                szHILName, NULL );
               iStatus = EXIT_FAILURE;
            }
            else
            {
               /* and write header */
               hb_snprintf( head.signature, sizeof( head.signature ), "\3HIL" );
               hb_snprintf( head.author, sizeof( head.author ), "The xharbour group" );
               hb_snprintf( head.language, sizeof( head.language ), HB_INTERNATIONAL_NAME );
               hb_snprintf( head.language_int, sizeof( head.language_int ), HB_INTERNATIONAL_NAME );
               hb_snprintf( head.language_code, sizeof( head.language_code ), HB_INTERNATIONAL_CODE );
               head.entries = -1; /* unknown */
               fwrite( &head, sizeof( head ), 1, hb_comp_HILfile );
            }
         }
      }

      if( iStatus == EXIT_SUCCESS )
      {
         /* Initialize support variables */
         hb_compInitVars();

         if( hb_pp_inFile( hb_comp_PP, szFileName, FALSE, NULL, FALSE ) )
         {
            BOOL bSkipGen = FALSE;

            hb_comp_szFile = szFileName;

            if( hb_comp_bPPO )
               hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Compiling '%s' and generating preprocessed output to '%s'...\n", szFileName, szPpoName );
            else
               hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Compiling '%s'...\n", szFileName );

            hb_compOutStd( hb_comp_szMsgBuf );

            if( hb_comp_bI18n )
            {
               hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Generating international list to '%s'...\n", szHILName );
               hb_compOutStd( hb_comp_szMsgBuf );
            }

            /* Generate the starting procedure frame */
            if( hb_comp_bStartProc )
               hb_compFunctionAdd( hb_compIdentifierNew( hb_strupr( hb_strdup( hb_comp_pFileName->szName ) ), FALSE ), HB_FS_PUBLIC, FUN_PROCEDURE );
            else
               /* Don't pass the name of module if the code for starting procedure
                * will be not generated. The name cannot be placed as first symbol
                * because this symbol can be used as function call or memvar's name.
                */
               hb_compFunctionAdd( hb_compIdentifierNew( "", TRUE ), HB_FS_PUBLIC, FUN_PROCEDURE );

            hb_compExprINIT();

            hb_comp_yyparse();

            if( hb_comp_bPPO && hb_comp_yyppo )
            {
               fclose( hb_comp_yyppo );
               hb_comp_yyppo = NULL;
            }

#if defined( HB_FORCE_CLOSE_DUMP_AREA )
            if( iBeginDump != iEndDump )
            {
               hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_UNBALANCE_PRAGMAS, szPrg, NULL );
               iStatus = EXIT_FAILURE;
               return iStatus;
            }
#endif

            /* Saving main file. */
            pFileName = hb_comp_pFileName;

            /* Open refernced modules. */
            while( hb_comp_pAutoOpen )
            {
               PAUTOOPEN pAutoOpen = hb_comp_pAutoOpen;

               hb_comp_pAutoOpen = hb_comp_pAutoOpen->pNext;

               if( ! hb_compFunctionFind( pAutoOpen->szName, NULL, NSF_NONE ) )
                  hb_compAutoOpen( pAutoOpen->szName, &bSkipGen );

               hb_xfree( pAutoOpen );
            }

            /* Restoring main file. */
            hb_comp_pFileName = pFileName;

            /* Begin of finalization phase. */

            /* fix all previous function returns offsets */
            hb_compFinalizeFunction();

            if( hb_comp_bDebugInfo )
               hb_compExternAdd( hb_compIdentifierNew( "__DBGENTRY", TRUE ), NULL, ( HB_SYMBOLSCOPE ) 0 );
               /* hb_compFunCallAdd( "__DBGENTRY", NULL, NSF_NONE );
                * pSyn
                */

            if( hb_comp_pInitFunc )
            {
               char szNewName[ 32 ];

               /* Fix the number of static variables */
               hb_comp_pInitFunc->pCode[ 3 ]    = HB_LOBYTE( hb_comp_iStaticCnt );
               hb_comp_pInitFunc->pCode[ 4 ]    = HB_HIBYTE( hb_comp_iStaticCnt );
               hb_comp_pInitFunc->iStaticsBase  = hb_comp_iStaticCnt;

               /* Update pseudo function name */
               hb_snprintf( szNewName, sizeof( szNewName ), "(_INITSTATICS%05d)", hb_comp_iStaticCnt );
               hb_comp_pInitFunc->szName = hb_compIdentifierNew( szNewName, TRUE );

               hb_compAddInitFunc( hb_comp_pInitFunc );
            }

            if( hb_comp_bLineNumbers && hb_comp_bDebugInfo )
            {
               PHB_DEBUGINFO pInfo = hb_compGetDebugInfo(), pNext;

               if( pInfo )
               {
                  int iModules = 0;

                  hb_compLineNumberDefStart();

                  do
                  {
                     HB_SIZE  ulSkip   = pInfo->ulFirstLine >> 3;
                     HB_SIZE  ulLen    = ( ( pInfo->ulLastLine + 7 ) >> 3 ) - ulSkip;

                     hb_compGenPushString( pInfo->pszModuleName, strlen( pInfo->pszModuleName ) + 1 );
                     hb_compGenPushLong( ulSkip << 3 );
                     hb_compGenPushString( ( char * ) pInfo->pLineMap + ulSkip, ulLen + 1 );
                     hb_compGenPCode3( HB_P_ARRAYGEN, 3, 0, ( BOOL ) 0 );
                     iModules++;

                     pNext = pInfo->pNext;
                     hb_xfree( pInfo->pszModuleName );
                     hb_xfree( pInfo->pLineMap );
                     hb_xfree( pInfo );
                     pInfo = pNext;
                  }
                  while( pInfo );

                  hb_compGenPCode3( HB_P_ARRAYGEN, HB_LOBYTE( iModules ), HB_HIBYTE( iModules ), ( BOOL ) 0 );
                  hb_compGenPCode1( HB_P_RETVALUE );

                  hb_compLineNumberDefEnd();

                  hb_compAddInitFunc( hb_comp_pLineNumberFunc );
               }
            }

            if( hb_comp_pGlobalsFunc )
            {
               PVAR pGlobal = hb_comp_pGlobals;

               if( hb_comp_pGlobalsFunc->pCode )
                  hb_compAddInitFunc( hb_comp_pGlobalsFunc );
               else
                  bKillGlobalsFunc = TRUE;

               /* Any NON EXTERN Globals? */
               while( pGlobal )
               {
                  if( pGlobal->szAlias == NULL )
                     break;

                  pGlobal = pGlobal->pNext;
               }

               /* Yes. */
               if( pGlobal )
               {
                  PCOMSYMBOL pSym;

                  pSym           = hb_compSymbolAdd( hb_compIdentifierNew( "{_REGISTERGLOBALS}", TRUE ), NULL, NULL, SYMF_FUNCALL | SYMF_STATIC );
                  pSym->cScope   = HB_FS_INITEXIT;
               }
            }

            if( hb_comp_szAnnounce )
               hb_compAnnounce( hb_comp_szAnnounce );

            /* End of finalization phase. */

            /* if( hb_comp_iErrorCount || ( hb_comp_AmbiguousVar && hb_comp_bAnyWarning ) )
             */
            if( hb_comp_iErrorCount || hb_comp_bAnyWarning )
            {
               if( hb_comp_iErrorCount )
               {
                  iStatus  = EXIT_FAILURE;
                  bSkipGen = TRUE;
                  hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "\r%i error%c\n", hb_comp_iErrorCount,
                               hb_comp_iErrorCount > 1 ? 's' : ' ' );
                  hb_compOutStd( hb_comp_szMsgBuf );
               }
               else if( hb_comp_iExitLevel == HB_EXITLEVEL_SETEXIT )
                  iStatus = EXIT_FAILURE;
               else if( hb_comp_iExitLevel == HB_EXITLEVEL_DELTARGET )
               {
                  iStatus  = EXIT_FAILURE;
                  bSkipGen = TRUE;
               }
               if( bSkipGen )
                  hb_compOutStd( "\nNo code generated\n" );
            }

            if( ! hb_comp_bSyntaxCheckOnly && ! bSkipGen && ( hb_comp_iErrorCount == 0 ) )
            {
               PFUNCTION   pFunc;
               BOOL        bDoFirst = TRUE;

               /* we create the output file name */
               hb_compOutputFile();

               if( ! hb_comp_bStartProc )
                  --hb_comp_iFunctionCnt;

               pFunc = hb_comp_functions.pFirst;

               if( hb_comp_iWarnings >= 2 && hb_comp_bStartProc == FALSE )
               {
                  PVAR pVar;

                  if( hb_comp_bWarnUnUsedStatics )
                  {
                     pVar = pFunc->pStatics;

                     while( pVar )
                     {
                        if( pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
                        {
                           char szFun[ 256 ];

                           hb_snprintf( szFun, sizeof( szFun ), "*(%i)", pVar->iDeclLine );
                           hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
                        }

                        pVar = pVar->pNext;
                     }
                  }

                  if( hb_comp_bWarnUnUsedGlobals )
                  {
                     pVar = hb_comp_pGlobals;

                     while( pVar )
                     {
                        if( pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
                        {
                           char szFun[ 256 ];

                           hb_snprintf( szFun, sizeof( szFun ), "*(%i)", pVar->iDeclLine );
                           hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
                        }

                        pVar = pVar->pNext;
                     }
                  }

                  if( hb_comp_bWarnUnUsedMemvars )
                  {
                     pVar = pFunc->pMemvars;

                     while( pVar )
                     {
                        if( pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
                        {
                           char szFun[ 256 ];

                           hb_snprintf( szFun, sizeof( szFun ), "*(%i)", pVar->iDeclLine );
                           hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
                        }

                        pVar = pVar->pNext;
                     }
                  }

                  if( hb_comp_bWarnUnUsedFields )
                  {
                     pVar = pFunc->pFields;

                     while( pVar )
                     {
                        if( pVar->szName && ( ( pVar->iUsed & VU_USED ) == 0 ) )
                        {
                           char szFun[ 256 ];

                           hb_snprintf( szFun, sizeof( szFun ), "*(%i)", pVar->iDeclLine );
                           hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
                        }

                        pVar = pVar->pNext;
                     }
                  }
               }

               while( pFunc )
               {

                  bFunc = ( ( strlen( pFunc->szName ) != 0 ) && ( strcmp( pFunc->szName, "(_INITSTATICS)" ) > 0 ) );

                  /* Using function name as section name */
                  if( bFunc && hb_comp_iGenVarList )
                     fprintf( hb_comp_VariableList, "[%s]\n", pFunc->szName );

                  hb_compOptimizeFrames( pFunc );

                  /* Just a new line for readability */
                  if( bFunc && hb_comp_iGenVarList )
                     fprintf( hb_comp_VariableList, "\n" );

                  if( bDoFirst && pFunc->szName[ 0 ] && ( pFunc->cScope & HB_FS_INITEXIT & ~HB_FS_STATIC ) == 0 && ( pFunc->cScope & HB_FS_UTILITY ) != HB_FS_UTILITY )
                  {
                     PCOMSYMBOL pSym;

                     if( pFunc->pNamespace )
                        pSym = hb_compSymbolAdd( pFunc->szName, NULL, pFunc->pNamespace, SYMF_NS_EXPLICITPTR );
                     else
                        pSym = hb_compSymbolFind( pFunc->szName, NULL, NULL, SYMF_FUNCALL );

                     assert( pSym );

                     pSym->cScope   |= HB_FS_FIRST;
                     bDoFirst       = FALSE;
                  }

                  pFunc = pFunc->pNext;
               }

               {
                  char *szFirst    = NULL;
                  char *szMain     = NULL;
                  PCOMSYMBOL pSymb = hb_comp_symbols.pFirst;

                  while( pSymb )
                  {
                     if( strlen( pSymb->szName ) > 0 && !szFirst )
                        szFirst = pSymb->szName;

                     if( strcmp( pSymb->szName, "MAIN" ) == 0 && strlen( pSymb->szName ) == 4 && ! szMain )
                        szMain = pSymb->szName;

                     pSymb = pSymb->pNext;
                  }

                  if( szMain )
                  {
                     BOOL bFirst   = TRUE;
                     int iLenFirst = ( int ) strlen( szFirst );

                     pSymb = hb_comp_symbols.pFirst;

                     while( pSymb )
                     {
                        if( strcmp( pSymb->szName, szFirst ) == 0 && ( int ) strlen( pSymb->szName ) == iLenFirst && bFirst )
                        {
                           pSymb->cScope  &= ~HB_FS_FIRST;
                           bFirst = FALSE;
                        }

                        if( strcmp( pSymb->szName, "MAIN" ) == 0 && strlen( pSymb->szName ) == 4  )
                           pSymb->cScope  |= HB_FS_FIRST;

                        pSymb = pSymb->pNext;
                     }
                  }
               }

               hb_compGenOutput( hb_comp_iLanguage, szSourceExtension );

               hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "\rLines %i, Functions/Procedures %i, pCodes %u\n", hb_comp_iLine, hb_comp_iFunctionCnt, ( unsigned int ) hb_comp_upCodeTotal );
               hb_compOutStd( hb_comp_szMsgBuf );
            }
         }
         else
         {
            hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Cannot open input file: %s\n", szFileName );
            hb_compOutErr( hb_comp_szMsgBuf );

#if defined( HB_COMP_DEBUG )
            printf( "[hb_compCompile] No code generated\n" );
#endif
            iStatus = EXIT_FAILURE;
         }
      }
   }
   else
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADFILENAME, szPrg, NULL );
      iStatus = EXIT_FAILURE;
   }

   hb_xfree( hb_comp_PrgFileName );
   hb_comp_PrgFileName = NULL;

   hb_compReleaseRTVars();
   hb_compReleaseLoops();
   hb_compReleaseElseIfs();

   if( hb_comp_functions.pFirst )
   {
      PFUNCTION pFunc = hb_comp_functions.pFirst;

      do
      {
         if( pFunc == hb_comp_functions.pLast )
            hb_comp_functions.pLast = NULL;

         pFunc = hb_compFunctionKill( pFunc );
      }
      while( pFunc );
   }

   if( hb_comp_functions.pLast )
      hb_compFunctionKill( hb_comp_functions.pLast );

   if( bKillGlobalsFunc )
      hb_compFunctionKill( hb_comp_pGlobalsFunc );

   if( hb_comp_funcalls.pFirst )
   {
      PFUNCALL pFunCall = hb_comp_funcalls.pFirst;

      do
      {
         hb_comp_funcalls.pFirst = pFunCall->pNext;
         hb_xfree( ( void * ) pFunCall );  /* NOTE: szName will be released by hb_compSymbolKill() */
         pFunCall                = hb_comp_funcalls.pFirst;
      }
      while( pFunCall );
   }

   if( hb_comp_pExterns )
   {
      PEXTERN pDelete;

      do
      {
         pDelete           = hb_comp_pExterns;
         hb_comp_pExterns  = hb_comp_pExterns->pNext;
         hb_xfree( ( void * ) pDelete );
      }
      while( hb_comp_pExterns );
   }


   if( hb_comp_inlines.pFirst )
   {
      PINLINE pInline = hb_comp_inlines.pFirst;

      do
      {
         hb_comp_inlines.pFirst = pInline->pNext;

         if( pInline->pCode )
            hb_xfree( ( void * ) pInline->pCode );

         hb_xfree( ( void * ) pInline->szFileName );
         hb_xfree( ( void * ) pInline );  /* NOTE: szName will be released by hb_compSymbolKill() */

         pInline = hb_comp_inlines.pFirst;
      }
      while( pInline );
   }

   if( hb_comp_iWarnings >= 3 && hb_comp_pReleaseDeclared )
   {

      PCOMDECLARED   pDeclared   = hb_comp_pReleaseDeclared;
      PCOMCLASS      pClass      = hb_comp_pReleaseClass;

      do
      {
         hb_comp_pReleaseDeclared   = pDeclared->pNext;
         if( pDeclared->cParamTypes )
            hb_xfree( pDeclared->cParamTypes );
         if( pDeclared->pParamClasses )
            hb_xfree( pDeclared->pParamClasses );
         hb_xfree( ( void * ) pDeclared );
         pDeclared                  = hb_comp_pReleaseDeclared;
      }
      while( pDeclared );

      while( pClass )
      {
         hb_comp_pReleaseClass   = pClass->pNext;

         pDeclared               = pClass->pMethod;

         while( pDeclared )
         {
            hb_comp_pReleaseDeclared   = pDeclared->pNext;
            if( pDeclared->cParamTypes )
               hb_xfree( pDeclared->cParamTypes );
            if( pDeclared->pParamClasses )
               hb_xfree( pDeclared->pParamClasses );
            if( pDeclared->bFree )
                  hb_xfree( pDeclared->szName );
            hb_xfree( ( void * ) pDeclared );
            pDeclared                  = hb_comp_pReleaseDeclared;
         }

         hb_xfree( ( void * ) pClass );
         pClass = hb_comp_pReleaseClass;
      }
   }

   if( hb_comp_symbols.pFirst )
   {
      PCOMSYMBOL pSym = hb_comp_symbols.pFirst;

      do
      {
         pSym = hb_compSymbolKill( pSym );
      }
      while( pSym );
   }

   if( hb_comp_pGlobals )
   {
      PVAR pGlobal = hb_comp_pGlobals, pDelete;

      do
      {
         pDelete  = pGlobal;
         pGlobal  = pGlobal->pNext;
         hb_xfree( pDelete );
      }
      while( pGlobal );
   }

   if( hb_comp_Namespaces.pFirst )
   {
      PNAMESPACE pNamespace = hb_comp_Namespaces.pFirst, pDelete;

      do
      {
         pDelete     = pNamespace;
         pNamespace  = pNamespace->pNext;
         hb_xfree( pDelete );
      }
      while( pNamespace );
   }

   if( hb_comp_UsedNamespaces.pFirst )
   {
      PNAMESPACE pNamespace = hb_comp_UsedNamespaces.pFirst, pDelete;

      do
      {
         pDelete     = pNamespace;
         if( pDelete == hb_comp_UsedNamespaces.pCurrent )
            hb_comp_UsedNamespaces.pCurrent = NULL;
         pNamespace  = pNamespace->pNext;
         hb_xfree( pDelete );
      }
      while( pNamespace );

      /* on syntax error */
      if( hb_comp_UsedNamespaces.pCurrent )
      {
         hb_xfree( hb_comp_UsedNamespaces.pCurrent );
         hb_comp_UsedNamespaces.pCurrent = NULL;
      }
   }

   /* have we got i18n file ? */
   if( hb_comp_HILfile != NULL )
   {
      fclose( hb_comp_HILfile );
      hb_comp_HILfile = NULL;
   }

   if( hb_comp_szHILout != NULL )
   {
      hb_xfree( hb_comp_szHILout );
      hb_comp_szHILout = NULL;
   }

   hb_xfree( hb_comp_pFileName );
   hb_comp_pFileName = NULL;

   return iStatus;
}

void hb_compAutoOpenAdd( char * szName )
{
   if( hb_comp_bAutoOpen && ! hb_compAutoOpenFind( szName ) )
   {
      PAUTOOPEN pAutoOpen = ( PAUTOOPEN ) hb_xgrab( sizeof( AUTOOPEN ) ), pLast;

      pAutoOpen->szName = szName;
      pAutoOpen->pNext  = NULL;

      if( hb_comp_pAutoOpen == NULL )
         hb_comp_pAutoOpen = pAutoOpen;
      else
      {
         pLast = hb_comp_pAutoOpen;

         while( pLast->pNext )
            pLast = pLast->pNext;

         pLast->pNext = pAutoOpen;
      }
   }
}

static BOOL hb_compAutoOpenFind( char * szName )
{
   PAUTOOPEN pLast = hb_comp_pAutoOpen;

   if( pLast == NULL )
      return FALSE;

   if( pLast->szName == szName )
      return TRUE;
   else
   {
      while( pLast->pNext )
      {
         pLast = pLast->pNext;

         if( pLast->szName == szName )
            return TRUE;
      }
   }
   return FALSE;
}

static int hb_compAutoOpen( char * szPrg, BOOL * pbSkipGen )
{
   int iStatus = EXIT_SUCCESS;

   hb_comp_pFileName = hb_fsFNameSplit( szPrg );

   if( hb_comp_pFileName->szName )
   {
      char  szFileName[ HB_PATH_MAX ];   /* filename to parse */
      char  szPpoName[ HB_PATH_MAX ];

      /* Clear and reinitialize preprocessor state */
      hb_pp_reset( hb_comp_PP );

      if( ! hb_comp_pFileName->szExtension )
         hb_comp_pFileName->szExtension = ".prg";

      hb_fsFNameMerge( szFileName, hb_comp_pFileName );

      if( hb_comp_bPPO )
      {
         hb_comp_pFileName->szExtension = ".ppo";
         hb_fsFNameMerge( szPpoName, hb_comp_pFileName );

         if( ! hb_pp_outFile( hb_comp_PP, szPpoName, NULL ) )
            iStatus = EXIT_FAILURE;
      }

      if( iStatus == EXIT_SUCCESS )
      {
         /* Minimal Init. */
         hb_comp_iLine = 1;

         if( hb_pp_inFile( hb_comp_PP, szFileName, FALSE, NULL, FALSE ) )
         {
            if( hb_comp_bPPO )
               hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Compiling module '%s' and generating preprocessed output to '%s'...\n", szFileName, szPpoName );
            else
               hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Compiling module '%s'...\n", szFileName );

            hb_compOutStd( hb_comp_szMsgBuf );

            /* Generate the starting procedure frame */
            if( hb_comp_bStartProc )
               hb_compFunctionAdd( hb_compIdentifierNew( hb_strupr( hb_strdup( hb_comp_pFileName->szName ) ), FALSE ), HB_FS_PUBLIC, FUN_PROCEDURE );

            {
               int   i  = hb_comp_iExitLevel;
               BOOL  b  = hb_comp_bAnyWarning;

               hb_comp_yyparse();

               hb_comp_iExitLevel   = ( i > hb_comp_iExitLevel ? i : hb_comp_iExitLevel );
               hb_comp_bAnyWarning  = b || hb_comp_bAnyWarning;
            }

            if( hb_comp_bAnyWarning )
            {
               if( hb_comp_iExitLevel == HB_EXITLEVEL_SETEXIT )
                  iStatus = EXIT_FAILURE;
               else if( hb_comp_iExitLevel == HB_EXITLEVEL_DELTARGET )
               {
                  iStatus     = EXIT_FAILURE;
                  *pbSkipGen  = TRUE;
                  hb_compOutStd( "\nNo code generated.\n" );
               }
            }
         }
         else
         {
            hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Cannot open %s, assumed external\n", szFileName );
            hb_compOutErr( hb_comp_szMsgBuf );
         }
      }

      hb_xfree( hb_comp_pFileName );
   }
   else
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADFILENAME, szPrg, NULL );
      iStatus = EXIT_FAILURE;
   }

   return iStatus;
}

void hb_compEnumAdd( char * szName )
{
   PENUMDEF pEnum = ( PENUMDEF ) hb_xgrab( sizeof( ENUMDEF ) );

#if defined( HB_COMP_DEBUG )
   printf( "[hb_compEnumAdd] New set: '%s'\n", szName );
#endif

   pEnum->szName     = szName;
   pEnum->lMembers   = 0;
   pEnum->pMembers   = NULL;
   pEnum->pNext      = NULL;

   if( hb_comp_functions.pLast->pEnums )
      hb_comp_functions.pLast->pEnums->pNext = pEnum;
   else
      hb_comp_functions.pLast->pEnums = pEnum;

   hb_comp_pEnum = pEnum;
}

void hb_compEnumMemberAdd( char * szName )
{
#if defined( HB_COMP_DEBUG )
   printf( "[hb_compEnumMemberAdd] Member: '%s'\n", szName );
#endif

   if( hb_comp_pEnum->pMembers )
      hb_comp_pEnum->pMembers = ( char ** ) hb_xrealloc( hb_comp_pEnum->pMembers, ++hb_comp_pEnum->lMembers * sizeof( char * ) );
   else
      hb_comp_pEnum->pMembers = ( char ** ) hb_xgrab( sizeof( char * ) );

   hb_comp_pEnum->pMembers[ hb_comp_pEnum->lMembers - 1 ] = szName;
}

/* Giancarlo Niccolai: internatonalization functions */
void hb_compAddI18nString( char * szString )
{
   HB_SIZE nLen;

   nLen = strlen( szString );

   /* include trailing 0 */
   /* writing 8 characters in ASCII numeric format,
      so it can be read from xharbour without conversions, and is
      portable across architectures */
   fprintf( hb_comp_HILfile, "%8d", ( int ) nLen + 1 );
   fputs( szString, hb_comp_HILfile );
   /* I want the trailing 0 to be in the file as a validity marker */
   fputc( 0, hb_comp_HILfile );
   /* hil file is an hit file without translations */
   fprintf( hb_comp_HILfile, "       0" );

   if( ferror( hb_comp_HILfile ) )
   {
      /* TODO:Signal error */
      hb_comp_bI18n = FALSE;
   }
}

static int hb_compProcessRSPFile( char * szRspName )
{
   FILE *   inFile;
   int      ch;
   int      i;
/* int      iProcess = 1; */
   int      iStatus     = EXIT_SUCCESS;
   BOOL     bFirstChar  = FALSE;

   szRspName++;

   inFile = hb_fopen( szRspName, "r" );
   i      = 0;

   if( ! inFile )
   {
      hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Cannot open input file: %s\n", szRspName );
      hb_compOutErr( hb_comp_szMsgBuf );
      iStatus = EXIT_FAILURE;
   }
   else
   {
      char * szFile = ( char * ) hb_xgrab( HB_PATH_MAX );

      hb_xmemset( szFile, '\0', HB_PATH_MAX );

      while( ( ch = fgetc( inFile ) ) != EOF )
      {
         if( ch == '\n' || ch == ' ' )
         {
            if( ! ( *szFile ) )
               continue;

            iStatus = hb_compCompile( szFile );

            if( iStatus != EXIT_SUCCESS )
               break;

/*          iProcess ++; */

            i = 0;
            hb_xmemset( szFile, '\0', HB_PATH_MAX );
         }
         else if( ch == '#' )
         {
            while( ( ch = fgetc( inFile ) ) != '\n' && ch != EOF )
            {
               ;
            }
         }
         else if( ch == ',' )
            continue;
         else if( ch == '"' )
         {
            while( ( ch = fgetc( inFile ) ) != '"' && ch != '\n' && ch != EOF )
            {
               ;
               szFile[ i++ ] = ( char ) ch;
            }
         }
         else
         {
            /* left trim() */
            if( HB_ISSPACE( ch ) )
            {
               if( bFirstChar )
                  /* allow space in file name */
                  szFile[ i++ ] = ( char ) ch;
            }
            else
            {
               szFile[ i++ ] = ( char ) ch;

               if( ! bFirstChar )
                  bFirstChar = TRUE;
            }
         }
      }

      fclose( inFile );
      hb_xfree( szFile );
   }

   return iStatus;
}
