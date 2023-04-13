/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Virtual Machine
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_vmPushLongConst()
 *    hb_vmPushDoubleConst()
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    __VMVARSGET()
 *    __VMVARSLIST()
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *      Threadsafing and MT stack operations. MT Optimization.
 *
 * Copyright 2005 Vicente Guerra <vicente@guerra.com.mx>
 *    hb_vmUnhideString()
 *
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *    hb_vmSumDate()
 *    hb_vmSubDate()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#include <math.h>
#include <time.h>
#include <ctype.h>

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapidbg.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapirdd.h"
#include "hbapigt.h"
#include "hbvm.h"
#include "hbxvm.h"
#include "hbpcode.h"
#include "hbset.h"
#include "inkey.ch"
#include "classes.h"
#include "hboo.ch"
#include "hbdebug.ch"
#include "hbdate.h"

#include "hbi18n.h"
#include "hbserv.h"
#include "hashapi.h"

#include "hbmemory.ch"
#include "divert.ch"

#ifdef HB_MACRO_STATEMENTS
   #include "hbpp.h"
#endif

#ifndef HB_CDP_SUPPORT_OFF
   #include "hbapicdp.h"
#endif

#if ! defined( HB_OS_DOS ) && ! defined( HB_OS_DARWIN_5 )
   #include "hbserv.h"
#endif

#if 0
   /* AJ: do not know this construct */
   #ifdef HB_VM_REQUESTS
      HB_VM_REQUESTS
   #endif
#endif

#if defined( HB_OS_WIN )
   /* Mouse Disabling */
   extern BOOL b_MouseEnable;
   #include <windows.h>
   #include <ole2.h>
   static BOOL s_bUnInitOle   = FALSE;
#endif

static void hb_vmClassError( UINT uiParams, const char * szClassName, const char * szMsg, PHB_ITEM pSelf );

HB_FUNC_EXTERN( SYSINIT );

/* PCode functions */

/* Operators (mathematical / character / misc) */
static void    hb_vmNegate( void );                                              /* negates (-) the latest value on the stack */
static void    hb_vmAddInt( PHB_ITEM pResult, LONG lAdd );                    /* add integer to given item */
static void    hb_vmPlus( PHB_ITEM pLeft, PHB_ITEM pRight, PHB_ITEM pResult );   /* sums the latest two values on the stack, removes them and leaves the result */
static void    hb_vmMinus( void );                                               /* substracts the latest two values on the stack, removes them and leaves the result */
static void    hb_vmMult( void );                                                /* multiplies the latest two values on the stack, removes them and leaves the result */
static void    hb_vmDivide( void );                                              /* divides the latest two values on the stack, removes them and leaves the result */
static void    hb_vmModulus( void );                                             /* calculates the modulus of latest two values on the stack, removes them and leaves the result */
static void    hb_vmPower( void );                                               /* power the latest two values on the stack, removes them and leaves the result */
static void    hb_vmInc( void );                                                 /* increment the latest numeric value on the stack */
static void    hb_vmDec( void );                                                 /* decrements the latest numeric value on the stack */
static void    hb_vmFuncPtr( void );                                             /* pushes a function address pointer. Removes the symbol from the satck */
static void    hb_vmTimeStampPut( PHB_ITEM pItem, long lJulian, long lMilliSec );
static void    hb_vmTimeStampAdd( PHB_ITEM pResult, PHB_ITEM pItem, double dValue );

/* Operators (relational) */
static void    hb_vmEqual( BOOL bExact );    /* checks if the two latest values on the stack are equal, removes both and leaves result */
static void    hb_vmNotEqual( void );        /* checks if the two latest values on the stack are not equal, removes both and leaves result */
static void    hb_vmLess( void );            /* checks if the latest - 1 value is less than the latest, removes both and leaves result */
static void    hb_vmLessEqual( void );       /* checks if the latest - 1 value is less than or equal the latest, removes both and leaves result */
static void    hb_vmGreater( void );         /* checks if the latest - 1 value is greater than the latest, removes both and leaves result */
static void    hb_vmGreaterEqual( void );    /* checks if the latest - 1 value is greater than or equal the latest, removes both and leaves result */
static void    hb_vmInstringOrArray( void ); /* check whether string 1 is contained in string 2 */
static void    hb_vmForTest( void );         /* test for end condition of for */

/* Operators (logical) */
static void    hb_vmNot( void );             /* changes the latest logical value on the stack */
static void    hb_vmAnd( void );             /* performs the logical AND on the latest two values, removes them and leaves result on the stack */
static void    hb_vmOr( void );              /* performs the logical OR on the latest two values, removes them and leaves result on the stack */

/* Operators (bit) */
static void    hb_vmBitAnd( void );          /* performs the bit AND on the latest two values, removes them and leaves result on the stack */
static void    hb_vmBitOr( void );           /* performs the bit OR on the latest two values, removes them and leaves result on the stack */
static void    hb_vmBitXor( void );          /* performs the bit XOR on the latest two values, removes them and leaves result on the stack */
static void    hb_vmBitShiftLeft( void );    /* performs the bit left shift on the latest two values, removes them and leaves result on the stack */
static void    hb_vmBitShiftRight( void );   /* performs the bit right shift on the latest two values, removes them and leaves result on the stack */

/* Array */
static void    hb_vmArrayPush( void );                /* pushes an array element to the stack, removing the array and the index from the stack */
static void    hb_vmArrayPushRef( void );             /* pushes a reference to an array element to the stack, removing the array and the index from the stack */
static void    hb_vmArrayPop( HB_PCODE pcode );       /* pops a value from the stack */
static void    hb_vmArrayDim( USHORT uiDimensions );  /* generates an uiDimensions Array and initialize those dimensions from the stack values */
static void    hb_vmArrayGen( ULONG ulElements );     /* generates an ulElements Array and fills it from the stack values */
static void    hb_vmArrayNew( PHB_ITEM, USHORT );  /* creates array */

/* Database */
static HB_ERRCODE hb_vmSelectWorkarea( PHB_ITEM, PHB_SYMB );   /* select the workarea using a given item or a substituted value */
static void    hb_vmSwapAlias( void );                         /* swaps items on the eval stack and pops the workarea number */

/* Hash */
static void    hb_vmHashGen( ULONG ulPairs ); /* generates an ulElements Hash and fills it from the stack values */

#if !defined( HB_NO_DEBUG )
static void    hb_vmDebugEntry( int nMode, int nLine, char * szName, int nIndex, int nFrame );
static void    hb_vmDebuggerExit( void );                               /* shuts down the debugger */
static void    hb_vmLocalName( USHORT uiLocal, char * szLocalName );    /* locals and parameters index and name information for the debugger */
static void    hb_vmStaticName( USHORT uiStatic, char * szStaticName ); /* statics vars information for the debugger */
static void    hb_vmModuleName( char * szModuleName );  /* PRG and function name information for the debugger */
static void    hb_vmDebuggerShowLine( USHORT uiLine );  /* makes the debugger shows a specific source code line */
static void    hb_vmDebuggerEndProc( void );            /* notifies the debugger for an endproc */
static BOOL             s_bDebugging;
static BOOL             s_bDebugRequest = FALSE; /* debugger invoked via the VM */
static PHB_DYNS         s_pDynsDbgEntry = NULL;  /* Cached __DBGENTRY symbol */
static HB_DBGENTRY_FUNC s_pFunDbgEntry;          /* C level debugger entry */
#endif

/* Execution */
static HARBOUR hb_vmDoBlock( void );                                       /* executes a codeblock */
static void    hb_vmFrame( unsigned short iLocals, BYTE bParams );         /* increases the stack pointer for the amount of locals and params suplied */
static void    hb_vmDivertFrame( unsigned short iLocals, BYTE bParams );   /* increases the stack pointer for the amount of locals and params suplied */
static void    hb_vmSetDivert( BOOL bDivertOf );

static void    hb_vmSFrame( PHB_SYMB pSym );                      /* sets the statics frame for a function */
static void    hb_vmStatics( PHB_SYMB pSym, USHORT uiStatics );   /* increases the global statics array to hold a PRG statics */
static void    hb_vmEndBlock( void );                             /* copies the last codeblock pushed value into the return value */
static void    hb_vmRetValue( void );                             /* pops the latest stack value into stack.Return */

/* Push */
static void    hb_vmPushAlias( void );                                                    /* pushes the current workarea number */
static void    hb_vmPushAliasedField( PHB_SYMB );                                         /* pushes an aliased field on the eval stack */
static void    hb_vmPushAliasedVar( PHB_SYMB );                                           /* pushes an aliased variable on the eval stack */
static void    hb_vmPushBlock( const BYTE * pCode, USHORT usSize, BOOL bDynCode );        /* creates a codeblock */
static void    hb_vmPushBlockShort( const BYTE * pCode, USHORT usSize, BOOL bDynCode );   /* creates a codeblock */
static void    hb_vmPushDoubleConst( double dNumber, int iWidth, int iDec );              /* Pushes a double constant (pcode) */
static void    hb_vmPushMacroBlock( BYTE * pCode );                                       /* creates a macro-compiled codeblock */
static void    hb_vmPushLocal( SHORT iLocal );                                            /* pushes the content of a local onto the stack */
static void    hb_vmPushLocalByRef( SHORT iLocal );                                       /* pushes a local by refrence onto the stack */
static void    hb_vmPushHBLong( HB_LONG lNumber );                                        /* pushes a HB_LONG number onto the stack */

#if ! defined( HB_LONG_LONG_OFF )
static void hb_vmPushLongLongConst( LONGLONG lNumber );                                   /* Pushes a LONGLONG constant (pcode) */
#endif

#if HB_INT_MAX >= INT32_MAX
static void    hb_vmPushIntegerConst( int iNumber );     /* Pushes a int constant (pcode) */
#else
static void    hb_vmPushLongConst( LONG lNumber );       /* Pushes a LONG constant (pcode) */
#endif

static void    hb_vmPushStatic( USHORT uiStatic );       /* pushes the containts of a static onto the stack */
static void    hb_vmPushStaticByRef( USHORT uiStatic );  /* pushes a static by refrence onto the stack */
static void    hb_vmPushVariable( PHB_SYMB pVarSymb );   /* pushes undeclared variable */
static void    hb_vmDuplicate( void );                   /* duplicates the latest value on the stack */
static void    hb_vmDuplTwo( void );                     /* duplicates the latest two value on the stack */
static void    hb_vmSumDate( PHB_ITEM pItem1, PHB_ITEM pItem2, PHB_ITEM pResult );
static void    hb_vmSubDate( PHB_ITEM pItem1, PHB_ITEM pItem2 );

/* Pop */
static BOOL    hb_vmPopLogical( void );            /* pops the stack latest value and returns its logical value */
static HB_LONG hb_vmPopHBLong( void );             /* pops the stack latest value and returns its HB_LONG value */
static double  hb_vmPopNumber( void );             /* pops the stack latest value and returns its numeric value */
static double  hb_vmPopDouble( int * );            /* pops the stack latest value and returns its double numeric format value */
static void    hb_vmPopAlias( void );              /* pops the workarea number form the eval stack */
static void    hb_vmPopAliasedField( PHB_SYMB );   /* pops an aliased field from the eval stack*/
static void    hb_vmPopAliasedVar( PHB_SYMB );     /* pops an aliased variable from the eval stack*/
static void    hb_vmPopLocal( SHORT iLocal );      /* pops the stack latest value onto a local */
static void    hb_vmPopStatic( USHORT uiStatic );  /* pops the stack latest value onto a static */

/* misc */
static void    hb_vmDoInitStatics( void );         /* executes all _INITSTATICS functions */
static void    hb_vmDoInitFunctions( void );       /* executes all defined PRGs INIT functions */
static BOOL    hb_vmDoInitFunc( char * pFuncSym ); /* executes CLIPINIT, HB_OLEINIT and HASHENTRY */

#if 0
static void    hb_vmStaticName( BYTE bIsGlobal, USHORT uiStatic, char * szStaticName );   /* statics vars information for the debugger */
static void    hb_vmPushNumInt( HB_LONG lNumber );                                        /* pushes a number on to the stack and decides if it is integer or HB_LONG */
void           hb_vmDoExitFunctions( void );                                              /* executes all defined PRGs EXIT functions */
extern BOOL    hb_regex( char cRequest, PHB_ITEM pRegEx, PHB_ITEM pString );
#endif

#if !defined( HB_NO_PROFILER )
BOOL  hb_bProfiler = FALSE;                  /* profiler status is off */
ULONG hb_ulOpcodesCalls[ HB_P_LAST_PCODE ];  /* array to profile opcodes calls */
ULONG hb_ulOpcodesTime[ HB_P_LAST_PCODE ];   /* array to profile opcodes consumed time */
extern void hb_mthAddTime( PMETHOD, ULONG ); /* profiler from classes.c */
#endif

#if defined( HARBOUR_START_PROCEDURE )
char * s_pszLinkedMain = NULL;   /* name of starup function set by linker */
#endif

#if !defined( HB_NO_TRACE )
BOOL hb_bTracePrgCalls = FALSE;  /* prg tracing is off */
#endif

/* virtual machine state */

HB_SYMB                 hb_symEval = { "__EVAL", { HB_FS_PUBLIC }, { hb_vmDoBlock }, NULL }; /* symbol to evaluate codeblocks */

static HB_ITEM          s_aStatics;                                                          /* Harbour array to hold all application statics variables */
static PHB_SYMB         s_pSymStart          = NULL;                                         /* start symbol of the application. MAIN() is not required */
static PSYMBOLS         s_pSymbols           = NULL;                                         /* to hold a linked list of all different modules symbol tables */
static ULONG            s_ulFreeSymbols      = 0;                                            /* number of free module symbols */
static void *           s_hDynLibID          = NULL;                                         /* unique identifer to mark symbol tables loaded from dynamic libraries */
static BOOL             s_fCloneSym          = FALSE;                                        /* clone registered symbol tables */

static int              s_iErrorLevel        = 0;                                            /* application exit errorlevel */

#if !defined( HB_NO_DEBUG )
/* Stores level of procedures call stack */
static ULONG            s_ulProcLevel        = 0;
#endif

#if 0
/* init GUI Error Message */
BOOL                    b_GUIErrorMessage    = FALSE;
#endif

/* static, for now */
HB_EXTERN_BEGIN
PHB_FUNC                pHVMFuncService      = NULL;
BOOL                    hb_vm_bQuitRequest   = FALSE;
char *                  hb_vm_sNull          = "";
int                     hb_vm_iTry           = 0;
HB_ITEM                 hb_vm_BreakBlock     = HB_ITEM_NIL;
extern void             hb_filebufInit( void );
HB_EXTERN_END

static int              s_iBaseLine = 0;

static HB_ITEM          s_aGlobals; /* Harbour array to hold all application global variables */

static PHB_FUNC_LIST    s_InitFunctions   = NULL;
static PHB_FUNC_LIST    s_ExitFunctions   = NULL;

static BOOL             s_bDynamicSymbols = FALSE;

/* 21/10/00 - maurilio.longo@libero.it
   This Exception Handler gets called in case of an abnormal termination of an harbour program and
   displays a full stack trace at the harbour language level */
#if defined( HB_OS_OS2 )
ULONG _System OS2TermHandler( PEXCEPTIONREPORTRECORD p1,
                              PEXCEPTIONREGISTRATIONRECORD p2,
                              PCONTEXTRECORD p3,
                              PVOID pv );
#endif

#if !defined( HB_THREAD_SUPPORT ) && !defined( HB_NO_BACKGROUND )
/* abackground function counter */
static int     s_iBackground  = 0;
#endif

static HB_DYNS ModuleFakeDyn  = HB_DYNS_INIT;

#if ( !defined( __BORLANDC__ ) || defined( __EXPORT__ ) )
static BOOL    s_Do_xinit = TRUE;
#endif

void hb_vmAtInit( HB_INIT_FUNC pFunc, void * cargo )
{
   PHB_FUNC_LIST pLst = ( PHB_FUNC_LIST ) hb_xgrab( sizeof( HB_FUNC_LIST ) );

#if ( !defined( __BORLANDC__ ) || defined( __EXPORT__ ) )
   if( s_Do_xinit )
   {
      s_Do_xinit = FALSE;
      hb_xinit();
   }
#endif

   pLst->pFunc       = pFunc;
   pLst->cargo       = cargo;
   pLst->pNext       = s_InitFunctions;
   s_InitFunctions   = pLst;
}

void hb_vmAtExit( HB_INIT_FUNC pFunc, void * cargo )
{
   PHB_FUNC_LIST pLst = ( PHB_FUNC_LIST ) hb_xgrab( sizeof( HB_FUNC_LIST ) );

   pLst->pFunc       = pFunc;
   pLst->cargo       = cargo;
   pLst->pNext       = s_ExitFunctions;
   s_ExitFunctions   = pLst;
}

static void hb_vmCleanModuleFunctions( void )
{
   PHB_FUNC_LIST pLst;

   while( s_InitFunctions )
   {
      pLst              = s_InitFunctions;
      s_InitFunctions   = pLst->pNext;
      hb_xfree( pLst );
   }

   while( s_ExitFunctions )
   {
      pLst              = s_ExitFunctions;
      s_ExitFunctions   = pLst->pNext;
      hb_xfree( pLst );
   }
}

static void hb_vmDoModuleFunctions( PHB_FUNC_LIST pFunctions )
{
   PHB_FUNC_LIST pLst = pFunctions;

   while( pLst )
   {
      pLst->pFunc( pLst->cargo );
      pLst = pLst->pNext;
   }
}

#if !defined( HB_THREAD_SUPPORT )

BOOL hb_vmIsMt( void ) { return FALSE; }
void hb_vmLock( void ) {;}
void hb_vmUnlock( void ) {;}

#else

BOOL hb_vmIsMt( void ) { return TRUE; }

/* unlock VM, allow GC and other exclusive single task code execution */
void hb_vmUnlock( void )
{
   HB_THREAD_STUB
   HB_STACK_UNLOCK
#if defined( HB_OS_WIN )
   HB_TEST_CANCEL_ENABLE_ASYN
#endif
}

/* lock VM blocking GC and other exclusive single task code execution */
void hb_vmLock( void )
{
   HB_THREAD_STUB
#if defined( HB_OS_WIN )
   HB_DISABLE_ASYN_CANC
#endif
   HB_STACK_LOCK
}

#endif /* HB_THREAD_SUPPORT */

static BYTE * hb_vmUnhideString( BYTE uiType, ULONG ulSize, const BYTE * pSource, ULONG ulBufferSize )
{
   BYTE * pBuffer = ( BYTE * ) hb_xgrab( HB_MAX( ulSize, 1 ) );

   HB_SYMBOL_UNUSED( ulBufferSize );

   switch( uiType )
   {
      case 1:      /* Simple XOR 0xf3 */
      {
         BYTE * pTarget = pBuffer;

         while( ulSize-- )
            *pTarget++ = ( BYTE ) ( ( *pSource++ ) ^ 0xf3 );

         break;
      }

      default:     /* No decode */
         HB_MEMCPY( pBuffer, pSource, ulSize );
         break;
   }

   return pBuffer;
}

/* Initialize Error system */
static void hb_vmDoInitError( void )
{
   /*
    * TODO: replace it by our own minimum error system: f.e __ERRORBLOCK
    * for which we will be sure that it does not use any static variable
    * references. ErrorSys can be replaced by user and it is not safe to
    * set it here. This is only temporary solution to the moment we create
    * very small and basic error system which will be set when static
    * variable are initialized to report any error which can appear in
    * this process (very seldom situation because code for static
    * initialization is limited). The real erro block will be set
    * by CLIPInit function [druzus].
    */
   HB_THREAD_STUB_STACK

   if( ! hb_vmDoInitFunc( "__ERRORBLOCK" ) )
      hb_errInternal( HB_EI_ERRUNRECOV, "Error! missing __ErrorBlock()", NULL, NULL );

   if( hb_vmDoInitFunc( "__BREAKBLOCK" ) )
      hb_itemForwardValue( &hb_vm_BreakBlock, hb_stackReturnItem() );
   else
      hb_errInternal( HB_EI_ERRUNRECOV, "Error! missing __BreakBlock()", NULL, NULL );
}

/* Initialize Func
 * CLIPINIT Initialize ErrorBlock() and __SetHelpK()
 * HB_OLEINIT Initialize Ole System IF linked in.
 * HASHENTRY  Initialize
 */
static BOOL hb_vmDoInitFunc( char * pFuncSym )
{
   PHB_DYNS pDynSym;

   assert( pFuncSym != NULL );

   pDynSym = hb_dynsymFind( pFuncSym );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo( 0 );

      return TRUE;
   }

   return FALSE;
}

void hb_vmSymbolResolveDeferred( void )
{
   PSYMBOLS pModuleSymbols = s_pSymbols;

   while( pModuleSymbols )
   {
      if( ( pModuleSymbols->hScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
      {
         PHB_DYNS       pDynSym;
         PHB_SYMB       pModuleSymbol;
         register UINT  ui;

/* #define DEBUG_RESOLVE_SYMBOLS */
#ifdef DEBUG_RESOLVE_SYMBOLS
         TraceLog( NULL, "Resolve Module: '%s' has Deferred Symbols\n", pModuleSymbols->szModuleName );
#endif
         for( ui = 0; ui < pModuleSymbols->uiModuleSymbols; ui++ )
         {
            pModuleSymbol = pModuleSymbols->pSymbolTable + ui;

            if( ( pModuleSymbol->scope.value & HB_FS_DEFERRED ) == HB_FS_DEFERRED /* && pModuleSymbol->value.pFunPtr == NULL */ )
            {
               pDynSym = hb_dynsymFind( pModuleSymbol->szName );

               if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
               {
                  pModuleSymbol->scope.value    |= ( pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC );
                  pModuleSymbol->value.pFunPtr  = pDynSym->pSymbol->value.pFunPtr;

#ifdef DEBUG_RESOLVE_SYMBOLS
                  TraceLog( NULL, "Resolved Deferred: '%s'\n", pModuleSymbol->szName );
#endif
               }
            }
         }
      }
#ifdef DEBUG_RESOLVE_SYMBOLS
      else
         TraceLog( NULL, "Module: '%s' does NOT have any Deferred Symbols\n", pModuleSymbols->szModuleName );
#endif

      pModuleSymbols = pModuleSymbols->pNext;
   }
}

static void hb_vmSymbolOverloadDefinition( PHB_DYNS pDynSym )
{
   PSYMBOLS pModuleSymbols = s_pSymbols;

   while( pModuleSymbols )
   {
      PHB_SYMB       pModuleSymbol, pOverloadedSym = pDynSym->pSymbol;
      register UINT  ui;

      for( ui = 0; ui < pModuleSymbols->uiModuleSymbols; ui++ )
      {
         pModuleSymbol = pModuleSymbols->pSymbolTable + ui;

         if( pModuleSymbol->pDynSym == pDynSym && pModuleSymbol->value.pFunPtr != pOverloadedSym->value.pFunPtr )
         {
            pModuleSymbol->value.pFunPtr  = pOverloadedSym->value.pFunPtr;
            pModuleSymbol->scope.value    &= ~( HB_FS_LOCAL | HB_FS_PCODEFUNC );
            pModuleSymbol->scope.value    |= ( pOverloadedSym->scope.value & HB_FS_PCODEFUNC );
         }
      }

      pModuleSymbols = pModuleSymbols->pNext;
   }
}

/* application entry point */
void hb_vmInit( BOOL bStartMainProc )
{
   HB_SYMB                       FakeInitSymbol = { "", { HB_FS_STATIC }, { NULL }, &ModuleFakeDyn };

#ifndef HB_THREAD_SUPPORT
   register UINT                 uiCounter;
#endif

#if defined( HB_OS_OS2 )
   EXCEPTIONREGISTRATIONRECORD   RegRec   = { 0 };    /* Exception Registration Record */
   APIRET                        rc       = NO_ERROR; /* Return code                   */
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInit()" ) );

   /* initialize internal data structures */
   s_aStatics.type   = HB_IT_NIL;
   s_iErrorLevel     = 0;

#ifndef HB_NO_DEBUG
   s_bDebugging      = FALSE;
   s_pDynsDbgEntry   = hb_dynsymFind( "__DBGENTRY" );
#endif

#if 0
   if( hb_dynsymFind( "HB_GUIERRORMESSAGE" ) )
      b_GUIErrorMessage = TRUE;
#endif
   /*
      Moved to hb_vmProcessSymbols() to avoid GPF trap.
      hb_xinit();
    */

   hb_gcInit();

#ifdef HB_THREAD_SUPPORT
   HB_TRACE( HB_TR_INFO, ( "threadInit" ) );
   hb_threadInit();
#endif

   HB_TRACE( HB_TR_INFO, ( "stackInit" ) );
   hb_stackInit();

#ifndef HB_THREAD_SUPPORT
   /* In MT mode this is a part of stack initialization code */
   HB_VM_STACK.pSequence   = NULL;

   HB_VM_STACK.uiVMFlags   = 0;

   for( uiCounter = 0; uiCounter < HB_MAX_WITH_OBJECTS; uiCounter++ )
      HB_VM_STACK.aWithObject[ uiCounter ].type = HB_IT_NIL;

   HB_VM_STACK.wWithObjectCounter = 0;

   for( uiCounter = 0; uiCounter < HB_MAX_ENUMERATIONS; uiCounter++ )
   {
      HB_VM_STACK.aEnumCollection[ uiCounter ].type   = HB_IT_NIL;
      HB_VM_STACK.awEnumIndex[ uiCounter ]            = 0;
   }

   HB_VM_STACK.wEnumCollectionCounter  = 0;

   /* initialization of macro & codeblock parameter passing */
   HB_VM_STACK.iExtraParamsIndex       = 0;
   HB_VM_STACK.iExtraElementsIndex     = 0;
   HB_VM_STACK.iExtraElements          = 0;
   HB_VM_STACK.iExtraIndex             = 0;
#endif /* HB_THREAD_SUPPORT */

   s_aGlobals.type                     = HB_IT_NIL;
   hb_arrayNew( &s_aGlobals, 0 );
   /* printf( "Allocated s_aGlobals: %p Owner: %p\n", &s_aGlobals, s_aGlobals.item.asArray.value->pOwners ); */

   hb_clsInit();              /* initialize Classy/OO system */

   HB_TRACE( HB_TR_INFO, ( "errInit" ) );
   hb_errInit();

   HB_TRACE( HB_TR_INFO, ( "dynsymNew" ) );
   hb_dynsymNew( &hb_symEval, NULL );  /* initialize dynamic symbol for evaluating codeblocks */

   {
      HB_THREAD_STUB
      /* _SET_* initialization */
      HB_TRACE( HB_TR_INFO, ( "setInitialize" ) );
      hb_setInitialize( hb_stackSetStruct() );
   }

   HB_TRACE( HB_TR_INFO, ( "conInit" ) );
   hb_conInit();    /* initialize Console */

#ifndef HB_THREAD_SUPPORT
   /* in threads, it is called from thread stack constructor */
   HB_TRACE( HB_TR_INFO, ( "memvarsInit" ) );
   hb_memvarsInit();
#endif

   HB_TRACE( HB_TR_INFO, ( "il8nInit" ) );
   hb_i18nInit( NULL, NULL );  /* try to open default language. */

   HB_TRACE( HB_TR_INFO, ( "filebufInit" ) );
   hb_filebufInit();

   HB_TRACE( HB_TR_INFO, ( "SymbolInit_RT" ) );
   hb_vmSymbolInit_RT(); /* initialize symbol table with runtime support functions */
   hb_vmSymbolResolveDeferred();

   s_bDynamicSymbols = TRUE;

   /* Set the language to the default */
   hb_langSelectID( HB_MACRO2STRING( HB_LANG_DEFAULT ) );

   /* Check for some internal switches */
   HB_TRACE( HB_TR_INFO, ( "cmdarg" ) );
   hb_cmdargProcessVM();

#ifndef HB_NO_PROFILER
   HB_TRACE( HB_TR_INFO, ( "profiler" ) );
   /* Initialize opcodes profiler support arrays */
   {
      ULONG ul;

      for( ul = 0; ul < HB_P_LAST_PCODE; ul++ )
      {
         hb_ulOpcodesCalls[ ul ] = 0;
         hb_ulOpcodesTime[ ul ]  = 0;
      }
   }
#endif /* HB_NO_PROFILER  */

   /* Intitialize basic error system to report errors which can appear
    * in InitStatics, the real (full feature) error block will be installed
    * in InitClip()
    */
   HB_TRACE( HB_TR_INFO, ( "InitError" ) );
   hb_vmDoInitError();

#ifndef HB_NO_DEBUG
   if( s_pDynsDbgEntry )
   {
      /* Try to get C dbgEntry() function pointer */
      if( ! s_pFunDbgEntry )
         hb_vmDebugEntry( HB_DBG_GETENTRY, 0, NULL, 0, 0 );

      if( ! s_pFunDbgEntry )
         s_pFunDbgEntry = hb_vmDebugEntry;
   }
#endif /* HB_NO_DEBUG */

   /* Call functions that initializes static variables
    * Static variables have to be initialized before any INIT functions
    * because INIT function can use static variables.
    */
   HB_TRACE( HB_TR_INFO, ( "InitStatics" ) );
   hb_vmDoInitStatics();

   HB_TRACE( HB_TR_INFO, ( "InitClip" ) );
   hb_vmDoInitFunc( "CLIPINIT" ); /* Initialize ErrorBlock() and __SetHelpK() */

#if defined( HB_OS_WIN )
   if( hb_dynsymFind( "TOLEAUTO" ) )
   {
      if( OleInitialize( NULL ) == S_OK )  /* Do NOT use SUCCEEDED() due to S_FALSE! */
         s_bUnInitOle = TRUE;

      HB_TRACE( HB_TR_INFO, ( "HB_OLEINIT" ) );
      hb_vmDoInitFunc( "HB_OLEINIT" );
   }
#endif /* HB_OS_WIN */

   HB_TRACE( HB_TR_INFO, ( "HASHENTRY" ) );
   hb_vmDoInitFunc( "HASHENTRY" );

   hb_vmPushSymbol( &FakeInitSymbol );

   HB_TRACE( HB_TR_INFO, ( "ModuleFunctions(s_InitFunctions)" ) );
   hb_vmDoModuleFunctions( s_InitFunctions );

   hb_stackRemove( 1 );

   /* printf( "Before InitFunctions\n" ); */
   HB_TRACE( HB_TR_INFO, ( "InitFunctions" ) );
   hb_vmDoInitFunctions(); /* process defined INIT functions */

   /* This is undocumented CA-Clipper, if there's a function called _APPMAIN
      it will be executed first. [vszakats] */
   {
      PHB_DYNS pDynSym = hb_dynsymFind( "_APPMAIN" );

      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
         s_pSymStart = pDynSym->pSymbol;
#ifdef HARBOUR_START_PROCEDURE
      else
      {
         /* if first char is '@' then start procedure were set by
          * programmer explicitly and should have the highest priority
          * in other case it's the name of first public function in
          * first linked moudule which is used if there is no
          * HARBOUR_START_PROCEDURE in code
          */
         if( s_pszLinkedMain && *s_pszLinkedMain == '@' )
            pDynSym = hb_dynsymFind( s_pszLinkedMain + 1 );
         else
         {
            pDynSym = hb_dynsymFind( HARBOUR_START_PROCEDURE );

            if( ! ( pDynSym && pDynSym->pSymbol->value.pFunPtr ) && s_pszLinkedMain )
               pDynSym = hb_dynsymFind( s_pszLinkedMain );
         }

         if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
            s_pSymStart = pDynSym->pSymbol;
         else
            hb_errInternal( HB_EI_VMBADSTARTUP, NULL, HARBOUR_START_PROCEDURE, NULL );
      }
#else
#ifndef HB_C52_STRICT
      else if( bStartMainProc && ! s_pSymStart )
      {
         hb_errInternal( HB_EI_VMNOSTARTUP, NULL, NULL, NULL );
      }
#endif /* HB_C52_STRICT */
#endif /* HARBOUR_START_PROCEDURE */
   }

#if defined( HB_OS_OS2 ) /* Add OS2TermHandler to this thread's chain of exception handlers */
   RegRec.ExceptionHandler = ( ERR ) OS2TermHandler;
   rc                      = DosSetExceptionHandler( &RegRec );

   if( rc != NO_ERROR )
      hb_errInternal( HB_EI_ERRUNRECOV, "Unable to setup exception handler (DosSetExceptionHandler())", NULL, NULL );
#endif /* HB_OS_OS2 */

   if( bStartMainProc && s_pSymStart )
   {
      register int   i;
      register int   iArgCount;

      hb_vmPushSymbol( s_pSymStart );  /* pushes first HB_FS_PUBLIC defined symbol to the stack */
      hb_vmPushNil();                  /* places NIL at self */

      iArgCount = 0;
      for( i = 1; i < hb_cmdargARGC(); i++ ) /* places application parameters on the stack */
      {
         char ** argv = hb_cmdargARGV();

         /* Filter out any parameters beginning with //, like //INFO */
         if( ! hb_cmdargIsInternal( argv[ i ] ) )
         {
            hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );
            iArgCount++;
         }
      }

      /* printf( "Before Startup\n" ); */
      hb_vmDo( ( USHORT ) iArgCount ); /* invoke it with number of supplied parameters */
   }

#if defined( HB_OS_OS2 )
   /* I don't do any check on return code since harbour is exiting in any case */
   rc = DosUnsetExceptionHandler( &RegRec );
#endif
}

void hb_vmReleaseLocalSymbols( void )
{
   PSYMBOLS pDestroy;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmReleaseLocalSymbols()" ) );

   while( s_pSymbols )
   {
      pDestroy    = s_pSymbols;
      s_pSymbols  = s_pSymbols->pNext;

      if( pDestroy->szModuleName )
         hb_xfree( pDestroy->szModuleName );

      if( pDestroy->fAllocated )
      {
         register UINT  ui;
         PHB_SYMB       pSymbol;
         PHB_DYNS       pDynSym;

         for( ui = 0; ui < pDestroy->uiModuleSymbols; ui++ )
         {
            pSymbol = pDestroy->pSymbolTable + ui;

            if( ! HB_ISINITEXIT( pSymbol->scope.value ) && ! ( pSymbol->scope.value & HB_FS_STATIC ) )
            {
               pDynSym = HB_SYM_GETDYNSYM( pSymbol );

               if( pDynSym && pDynSym->pSymbol == pSymbol )
                  pDynSym->pSymbol = NULL;
            }

            hb_xfree( ( void * ) pSymbol->szName );
         }

         hb_xfree( pDestroy->pSymbolTable );
      }

      hb_xfree( pDestroy );
   }

   HB_TRACE( HB_TR_DEBUG, ( "Done hb_vmReleaseLocalSymbols()" ) );
}

int hb_vmQuit( void )
{
   HB_SYMB        FakeQuitSymbol = { "", { HB_FS_STATIC }, { NULL }, &ModuleFakeDyn };

   static BOOL    bQuitting      = FALSE;
   register UINT  i;
   register UINT  uiCounter;

   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmQuit(%i)" ) );

/* #define TRACE_QUIT */
#ifdef TRACE_QUIT
   TraceLog( NULL, "vmQuit()\n" );
#endif

#ifdef HB_THREAD_SUPPORT
   /* Quit sequences for non-main thread */
   /* We are going to quit now, so we don't want to have mutexes
      blocking our output */
   hb_stackSetStruct()->HB_SET_OUTPUTSAFETY = FALSE;

   if( ! HB_SAME_THREAD( hb_main_thread_id, HB_CURRENT_THREAD() ) )
   {
      hb_vm_bQuitRequest = TRUE;

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
      HB_DISABLE_ASYN_CANC
      HB_STACK_LOCK
      hb_threadCancelInternal();    /* never returns */
#else
      pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, NULL );
      HB_STACK_LOCK
      pthread_exit( 0 );
#endif /* defined( HB_OS_WIN ) || defined( HB_OS_OS2 ) */
   }
#endif /* HB_THREAD_SUPPORT */

   if( bQuitting == ( BOOL ) TRUE )
      return s_iErrorLevel;

   bQuitting = TRUE;

#ifdef HB_THREAD_SUPPORT
   hb_threadKillAll();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After KillAll\n" );
#endif /* TRACE_QUIT */

   hb_threadWaitAll();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After WaitAll\n" );
#endif /* TRACE_QUIT */
#endif /* HB_THREAD_SUPPORT */

#ifdef HB_MACRO_STATEMENTS
   hb_pp_Free();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After PP\n" );
#endif /* TRACE_QUIT */
#endif /* HB_MACRO_STATEMENTS */

   hb_idleShutDown();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Idle\n" );
#endif /* TRACE_QUIT */

#if !defined( HB_NO_BACKGROUND )
   hb_backgroundShutDown();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Background\n" );
#endif /* TRACE_QUIT */
#endif /* HB_NO_BACKGROUND */

   if( HB_VM_STACK.pPos == HB_VM_STACK.pItems )
      hb_vmPushSymbol( &FakeQuitSymbol );

   hb_stackSetActionRequest( 0 );   /* EXIT procedures should be processed */
   hb_vmDoExitFunctions();          /* process defined EXIT functions */
#ifdef TRACE_QUIT
   TraceLog( NULL, "After ExitFunctions\n" );
#endif /* TRACE_QUIT */

   /* process AtExit registered functions */
   hb_vmDoModuleFunctions( s_ExitFunctions );
#ifdef TRACE_QUIT
   TraceLog( NULL, "After ModuleFunctions\n" );
#endif /* TRACE_QUIT */

   hb_setkeyExit();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After setkey\n" );
#endif /* TRACE_QUIT */

#ifndef HB_NO_DEBUG
   /* deactivate debugger */
   hb_vmDebuggerExit();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Debugger\n" );
#endif /* TRACE_QUIT */
#endif /* HB_NO_DEBUG */

   /* release all known items stored in subsystems */
   hb_stackSetActionRequest( 0 );
   hb_rddShutDown();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After RDD\n" );
#endif /* TRACE_QUIT */

   hb_i18nExit();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After i18n\n" );
#endif /* TRACE_QUIT */

   hb_serviceExit();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Service\n" );
#endif /* TRACE_QUIT */

#if ! defined( HB_OS_DOS ) && ! defined( HB_OS_DARWIN_5 )
   if( pHVMFuncService )
   {
      pHVMFuncService();
#ifdef TRACE_QUIT
      TraceLog( NULL, "After FunService\n" );
#endif /* TRACE_QUIT */
   }
#endif /* !defined( HB_OS_DOS ) && ! defined( HB_OS_DARWIN_5 ) */

   /* HB_VM_STACK.pBase will be cleared, but we need a base symbol! */
   HB_VM_STACK.pBase = HB_VM_STACK.pItems;
   hb_vmPushSymbol( &FakeQuitSymbol );
   ( *HB_VM_STACK.pBase )->item.asSymbol.pCargo->stackbase  = 0;

   /*
      NO Need to perform individual hb_itemClear() since hb_gcCollectAll() will perform needed cleanup!
      hb_stackRemove( 0 );
    */
   while( HB_VM_STACK.pPos > HB_VM_STACK.pItems )
   {
      PHB_ITEM pItem = *( HB_VM_STACK.pPos - 1 );

      if( HB_IS_STRING( pItem ) )
         hb_itemReleaseString( pItem );
      else if( HB_IS_SYMBOL( pItem ) )
      {
         assert( pItem->item.asSymbol.pCargo );
         hb_xfree( ( void * ) pItem->item.asSymbol.pCargo );
         pItem->item.asSymbol.pCargo = NULL;
      }

      pItem->type = HB_IT_NIL;

      --HB_VM_STACK.pPos;
   }

   hb_vmPushSymbol( &FakeQuitSymbol );

#ifdef TRACE_QUIT
   TraceLog( NULL, "After Stack\n" );
#endif

   /* FOR EACH Enumerations. */
   uiCounter = HB_VM_STACK.wEnumCollectionCounter;
   for( i = 0; i < uiCounter; i++ )
   {
      if( HB_IS_COMPLEX( &( HB_VM_STACK.aEnumCollection[ i ] ) ) )
         hb_itemClear( &( HB_VM_STACK.aEnumCollection[ i ] ) );
   }
#ifdef TRACE_QUIT
   TraceLog( NULL, "After FOREACH\n" );
#endif

   /* WITH OBJECT */
   uiCounter = HB_VM_STACK.wWithObjectCounter;
   for( i = 0; i < uiCounter; i++ )
   {
      if( HB_IS_COMPLEX( &( HB_VM_STACK.aWithObject[ i ] ) ) )
         hb_itemClear( &( HB_VM_STACK.aWithObject[ i ] ) );
   }
#ifdef TRACE_QUIT
   TraceLog( NULL, "After WITHOBJECT\n" );
#endif

   if( s_aGlobals.type == HB_IT_ARRAY )
   {
      /* Because GLOBALS items are of type HB_IT_REF (see hb_vmRegisterGlobals())! */
      hb_arrayFill( &s_aGlobals, ( *HB_VM_STACK.pPos ), 1, s_aGlobals.item.asArray.value->ulLen );
      /* TraceLog( NULL, "Releasing s_aGlobals: %p\n", &s_aGlobals ); */
      hb_arrayRelease( &s_aGlobals );
      s_aGlobals.type = HB_IT_NIL;
      /* TraceLog( NULL, "   Released s_aGlobals: %p\n", &s_aGlobals ); */
   }
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Globals\n" );
#endif

   hb_memvarsClear();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After memvarsClear\n" );
#endif

   hb_itemSetNil( &HB_VM_STACK.Return );
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Return\n" );
#endif

   if (s_aStatics.type == HB_IT_ARRAY)
   {
	   /* Beyond this point all STATICs will be empty, Destructors will have access to the NIL values, we avoid GPF by not releasing the array yet. */
	   hb_arrayFill(&s_aStatics, (*HB_VM_STACK.pPos), 1, s_aStatics.item.asArray.value->ulLen);
   }
#ifdef TRACE_QUIT
   TraceLog( NULL, "After reset Statics\n" );
#endif

   hb_clsClearAllClassDatas();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After reset Class Datas\n" );
#endif

   hb_gcCollectAll( TRUE );
#ifdef TRACE_QUIT
   TraceLog( NULL, "After CollectAll\n" );
#endif

   /* Absolutley NO PRG code beyond this point! */
   hb_clsDisableDestructors();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After DisableDestructors\n" );
#endif

   /* #define DEBUG_DESTRUCTORS */

   hb_stackRemove( 1 ); /* Base Symbol! */
#ifdef TRACE_QUIT
   TraceLog( NULL, "After stackRemove\n" );
#endif

   hb_clsClearAll();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After clsClear\n" );
#endif
#ifdef DEBUG_DESTRUCTORS
   hb_gcCollectAll( TRUE );
   TraceLog( NULL, "CollectAll after clsClear\n" );
#endif

   if( s_aStatics.type == HB_IT_ARRAY )
   {
      HB_TRACE( HB_TR_DEBUG, ( "Releasing s_aStatics: %p\n", &s_aStatics ) );

/* #define DEBUG_STATICS */
#ifdef DEBUG_STATICS
      {
         ULONG ulLen = s_aStatics.item.asArray.value->ulLen;
         ULONG ulIndex;
         char  sBuffer[ 128 ];

         for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
         {
            hb_snprintf( sBuffer, sizeof( sBuffer ), "# %i type: %i\n", ulIndex, ( s_aStatics.item.asArray.value->pItems + ulIndex )->type );

#if defined( HB_OS_WIN )
            OutputDebugString( sBuffer );
#else
            printf( sBuffer );
#endif /* HB_OS_WIN */
         }
      }
#endif /* DEBUG_STATICS */

      hb_arrayRelease( &s_aStatics );
      s_aStatics.type = HB_IT_NIL;

      HB_TRACE( HB_TR_DEBUG, ( "   Released s_aStatics: %p\n", &s_aStatics ) );
   }
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Statics\n" );
#endif /* TRACE_QUIT */
#ifdef DEBUG_DESTRUCTORS
   hb_gcCollectAll( TRUE );
   TraceLog( NULL, "CollectAll after Statics\n" );
#endif /* DEBUG_DESTRUCTORS */

   hb_itemClear( &hb_vm_BreakBlock );
#ifdef TRACE_QUIT
   TraceLog( NULL, "After BreakBlock\n" );
#endif
#ifdef DEBUG_DESTRUCTORS
   hb_gcCollectAll( TRUE );
   TraceLog( NULL, "CollectAll after BreakBlock\n" );
#endif

   hb_errExit();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Err\n" );
#endif
#ifdef DEBUG_DESTRUCTORS
   hb_gcCollectAll( TRUE );
   TraceLog( NULL, "CollectAll after Err\n" );
#endif

   /* release all known garbage */
   if( hb_xquery( HB_MEM_USEDMAX ) ) /* check if fmstat is ON */
   {
      hb_gcCollectAll( TRUE );
#ifdef TRACE_QUIT
      TraceLog( NULL, "After CollectAll\n" );
#endif
   }
   else
   {
      hb_gcReleaseAll();

#ifdef HB_THREAD_SUPPORT
      /* Released by hb_gcReleaseAll()! */
      hb_stackMT.errorBlock            = NULL;
      hb_stackMT.aTryCatchHandlerStack = NULL;
#endif

#ifdef TRACE_QUIT
      TraceLog( NULL, "After ReleaseAll\n" );
#endif
   }

#ifndef HB_THREAD_SUPPORT
   /* MUST be *after* release of all known item, as they may indirectly refer to memvars */
   hb_memvarsRelease();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Memvar\n" );
#endif
#endif /* HB_THREAD_SUPPORT */

   hb_vmCleanModuleFunctions();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After CleanModuleFunctions\n" );
#endif

   hb_stackFree();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After hbStackFree\n" );
#endif

   hb_clsReleaseAll();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Class\n" );
#endif

   hb_vmReleaseLocalSymbols();  /* releases the local modules linked list */
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Symbols\n" );
#endif

   /* hb_dynsymLog(); */
   hb_dynsymRelease();          /* releases the dynamic symbol table */
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Dyn\n" );
#endif

   hb_conRelease();             /* releases Console */
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Con\n" );
#endif

   hb_setRelease( hb_stackSetStruct() );  /* releases Sets */
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Set\n" );
#endif

#ifndef HB_CDP_SUPPORT_OFF
   hb_cdpReleaseAll();          /* releases codepages */
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Cdp\n" );
#endif /* TRACE_QUIT */
#endif /* HB_CDP_SUPPORT_OFF */

#ifdef TRACE_QUIT
   TraceLog( NULL, "After Sequence\n" );
#endif

#if defined( HB_OS_WIN )
   if( s_bUnInitOle )
      OleUninitialize();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After Ole\n" );
#endif /* TRACE_QUIT */
#endif /* HB_OS_WIN */

   hb_xexit();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After xexit\n" );
#endif /* TRACE_QUIT */

#ifdef HB_THREAD_SUPPORT
   hb_threadExit();
#ifdef TRACE_QUIT
   TraceLog( NULL, "After thread exit\n" );
#endif /* TRACE_QUIT */
#endif /* HB_THREAD_SUPPORT */

   hb_traceExit();

   return s_iErrorLevel;
}

void hb_vmExecute( register const BYTE * pCode, register PHB_SYMB pSymbols )
{
   HB_THREAD_STUB

   ULONG          w                       = 0;
   ULONG          lNextSection /*= 0*/;
   ULONG          lCatchSection           = 0;
   ULONG          lFinallySection         = 0;
   ULONG          wEnumCollectionCounter  = HB_VM_STACK.wEnumCollectionCounter;
   ULONG          wWithObjectCounter      = HB_VM_STACK.wWithObjectCounter;
   BOOL           bCanRecover             = FALSE;
   BOOL           bCanFinalize            = FALSE;
   BOOL           bDynCode                = pSymbols == NULL || ( pSymbols->scope.value & HB_FS_DYNCODE ) != 0;
#if ! defined( HB_GUI ) || ! defined( HB_NO_BACKGROUND )
   PHB_SET_STRUCT pSet                    = hb_stackSetStruct();
#endif

#if !defined( HB_GUI )
   static USHORT  uiPolls                 = 1;
#endif

#if !defined( HB_NO_PROFILER )
   ULONG ulLastOpcode   = 0;  /* opcodes profiler support */
   ULONG ulPastClock    = 0;  /* opcodes profiler support */
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmExecute(%p, %p, %p)", pCode, pSymbols ) );

   assert( hb_stack_ready );

   /* TraceLog( NULL, "%s->hb_vmExecute(%p, %p, %p)\n", hb_stackBaseItem()->item.asSymbol.value->szName, pCode, pSymbols ); */

   /* NOTE: if pSymbols == NULL then hb_vmExecute is called from macro
    * evaluation. In this case all PRIVATE variables created during
    * macro evaluation belong to a function/procedure where macro
    * compiler was called.
    */

#ifndef HB_NO_PROFILER
   if( hb_bProfiler )
      ulPastClock = ( ULONG ) clock();
#endif /* HB_NO_PROFILER */

   for(;; )
   {
#ifndef HB_NO_PROFILER
      if( hb_bProfiler )
      {
         ULONG ulActualClock = ( ULONG ) clock();

         hb_ulOpcodesTime[ ulLastOpcode ] += ( ulActualClock - ulPastClock );
         ulPastClock                      = ulActualClock;
         ulLastOpcode                     = pCode[ w ];
         hb_ulOpcodesCalls[ ulLastOpcode ]++;
      }
#endif /* HB_NO_PROFILER */

#ifndef HB_GUI
      if( ! --uiPolls )
      {
         if( pSet->HB_SET_CANCEL || pSet->HB_SET_DEBUG )
            hb_inkeyPoll();
      }
#endif /* HB_GUI */

#if ! defined( HB_THREAD_SUPPORT ) && ! defined( HB_NO_BACKGROUND )
      if( pSet->HB_SET_BACKGROUNDTASKS && ( ++s_iBackground > pSet->HB_SET_BACKGROUNDTICK ) )
      {
         hb_backgroundRun();
         s_iBackground = 0;
      }
#endif /* ! defined( HB_THREAD_SUPPORT ) && ! defined( HB_NO_BACKGROUND ) */

      switch( pCode[ w ] )
      {
         /* Operators ( mathematical / character / misc ) */

         case HB_P_NEGATE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_NEGATE" ) );
            hb_vmNegate();
            w++;
            break;

         case HB_P_PLUS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PLUS" ) );

            hb_vmPlus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );
            hb_stackPop();
            w++;
            break;

         case HB_P_MINUS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MINUS" ) );
            hb_vmMinus();
            w++;
            break;

         case HB_P_MULT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MULT" ) );
            hb_vmMult();
            w++;
            break;

         case HB_P_DIVIDE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_DIVIDE" ) );
            hb_vmDivide();
            w++;
            break;

         case HB_P_MODULUS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MODULUS" ) );
            hb_vmModulus();
            w++;
            break;

         case HB_P_POWER:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POWER" ) );
            hb_vmPower();
            w++;
            break;

         case HB_P_BITAND:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_BITAND" ) );
            hb_vmBitAnd();
            w++;
            break;

         case HB_P_BITOR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_BITOR" ) );
            hb_vmBitOr();
            w++;
            break;

         case HB_P_BITXOR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_BITXOR" ) );
            hb_vmBitXor();
            w++;
            break;

         case HB_P_BITSHIFTR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_BITSHIFTR" ) );
            hb_vmBitShiftRight();
            w++;
            break;

         case HB_P_BITSHIFTL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_BITSHIFTL" ) );
            hb_vmBitShiftLeft();
            w++;
            break;

         case HB_P_INC:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_INC" ) );
            hb_vmInc();
            w++;
            break;

         case HB_P_DEC:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_DEC" ) );
            hb_vmDec();
            w++;
            break;

         case HB_P_FUNCPTR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FUNCPTR" ) );
            hb_vmFuncPtr();
            w++;
            break;

         /* Operators (relational) */

         case HB_P_EQUAL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_EQUAL" ) );
            hb_vmEqual( FALSE );
            w++;
            break;

         case HB_P_EXACTLYEQUAL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_EXACTLTYEQUAL" ) );
            hb_vmEqual( TRUE );
            w++;
            break;

         case HB_P_NOTEQUAL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_NOTEQUAL" ) );
            hb_vmNotEqual();
            w++;
            break;

         case HB_P_LESS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LESS" ) );
            hb_vmLess();
            w++;
            break;

         case HB_P_LESSEQUAL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LESSEQUAL" ) );
            hb_vmLessEqual();
            w++;
            break;

         case HB_P_GREATER:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_GREATER" ) );
            hb_vmGreater();
            w++;
            break;

         case HB_P_GREATEREQUAL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_GREATEREQUAL" ) );
            hb_vmGreaterEqual();
            w++;
            break;

         /* Also used for Arrays!!! */
         case HB_P_INSTRING:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_INSTRING" ) );
            hb_vmInstringOrArray();
            w++;
            break;

         case HB_P_LIKE:
         {
            PHB_ITEM pResult = hb_stackItemFromTop( -2 );
            BOOL     bLike;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LIKE" ) );

            bLike = hb_regex( 1, hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );

            hb_stackPop();

            hb_itemClear( pResult );
            pResult->type                 = HB_IT_LOGICAL;
            pResult->item.asLogical.value = bLike;
            w++;
            break;
         }

         case HB_P_MATCH:
         {
            PHB_ITEM pResult = hb_stackItemFromTop( -2 );
            BOOL     bMatch;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MATCH" ) );

            bMatch = hb_regex( 2, hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );

            hb_stackPop();

            hb_itemClear( pResult );
            pResult->type                 = HB_IT_LOGICAL;
            pResult->item.asLogical.value = bMatch;
            w++;
            break;
         }

         case HB_P_FORTEST:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FORTEST" ) );
            hb_vmForTest();
            w++;
            break;

         /* Operators (logical) */

         case HB_P_NOT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_NOT" ) );
            hb_vmNot();
            w++;
            break;

         case HB_P_AND:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_AND" ) );
            hb_vmAnd();
            w++;
            break;

         case HB_P_OR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_OR" ) );
            hb_vmOr();
            w++;
            break;

         /* Array */

         case HB_P_ARRAYPUSH:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ARRAYPUSH" ) );
            hb_vmArrayPush();
            w++;
            break;

         case HB_P_ARRAYPUSHREF:
            hb_vmArrayPushRef();
            w++;
            break;

         case HB_P_ARRAYPOP:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ARRAYPOP" ) );
            hb_vmArrayPop( HB_P_NOOP );
            w++;
            break;

         case HB_P_ARRAYPOPPLUS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ARRAYPOPPLUS" ) );
            hb_vmArrayPop( HB_P_PLUS );
            w++;
            break;

         case HB_P_ARRAYDIM:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ARRAYDIM" ) );
            hb_vmArrayDim( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_ARRAYGEN:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ARRAYGEN %i + %i", HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), HB_VM_STACK.iExtraElements ) );
            hb_vmArrayGen( ( ULONG ) HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) + HB_VM_STACK.iExtraElements );
            HB_VM_STACK.iExtraElements = 0;
            w                          += 3;
            break;

         case HB_P_HASHGEN:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_HASHGEN %i + %i", HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), HB_VM_STACK.iExtraElements ) );
            hb_vmHashGen( ( ULONG ) HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) + HB_VM_STACK.iExtraElements );
            HB_VM_STACK.iExtraElements = 0;
            w                          += 3;
            break;

         /* Object */

         case HB_P_MESSAGE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MESSAGE" ) );
            /* TraceLog( NULL, "%s->HB_P_MESSAGE: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) ); */
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         /* Database */

         case HB_P_SWAPALIAS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SWAPALIAS" ) );
            hb_vmSwapAlias();
            w++;
            break;

         /* Execution */

         case HB_P_DO:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_DO" ) );
            hb_vmDo( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_DOSHORT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_DOSHORT" ) );
            hb_vmDo( pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_FUNCTION:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FUNCTION" ) );

            hb_itemSetNil( hb_stackReturnItem() );
            hb_vmDo( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_stackPushReturn();
            w += 3;
            break;

         case HB_P_FUNCTIONSHORT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FUNCTIONSHORT" ) );

            hb_itemSetNil( hb_stackReturnItem() );
            hb_vmDo( pCode[ w + 1 ] );
            hb_stackPushReturn();
            w += 2;
            break;

         case HB_P_WITHOBJECT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_WITHOBJECT" ) );

            hb_itemForwardValue( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter++ ] ), hb_stackItemFromTop( -1 ) );

            if( HB_VM_STACK.wWithObjectCounter == HB_MAX_WITH_OBJECTS )
               hb_errInternal( HB_EI_ERRUNRECOV, "WITH OBJECT excessive nesting!", NULL, NULL );

            hb_stackPop();
            w++;
            break;

         case HB_P_ENDWITHOBJECT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ENDWITHOBJECT" ) );

            if( HB_VM_STACK.wWithObjectCounter )
               hb_itemClear( &( HB_VM_STACK.aWithObject[ --HB_VM_STACK.wWithObjectCounter ] ) );

            w++;
            break;

         case HB_P_FOREACH:
         {
            PHB_ITEM pEnumeration = &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] );

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FOREACH" ) );

            hb_itemForwardValue( pEnumeration, hb_stackItemFromTop( -1 ) );
            hb_stackPop();

            if( hb_objGetOpOver( pEnumeration ) & HB_CLASS_OP_FOREACH )
            {
               HB_ITEM_NEW( ForEachOp );

               hb_itemPutNI( &ForEachOp, FOREACH_BEGIN );

               hb_vmOperatorCall( pEnumeration, &ForEachOp, "__OPFOREACH", NULL, 0, pEnumeration );
            }
            else if( HB_IS_ARRAY( pEnumeration ) || HB_IS_STRING( pEnumeration ) )
            {
               /* No prep needed. */
            }
            else if( HB_IS_HASH( pEnumeration ) /* && hb_hashGetCompatibility( pEnumeration ) */ )
            {
               /* No prep needed. */
            }
            else
               hb_errRT_BASE( EG_ARG, 1602, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pEnumeration, hb_itemPutNI( *HB_VM_STACK.pPos, 1 ) );

            HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] = hb_itemUnRefOnce( hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            HB_VM_STACK.wEnumCollectionCounter++;

            if( HB_VM_STACK.wEnumCollectionCounter == HB_MAX_ENUMERATIONS )
               hb_errInternal( HB_EI_ERRUNRECOV, "FOR EACH excessive nesting!", NULL, NULL );

            w++;
            break;
         }

         case HB_P_ENUMERATE:
         {
            BOOL bPushLogical = FALSE;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ENUMERATE" ) );

            if( HB_VM_STACK.wEnumCollectionCounter )
            {
               PHB_ITEM pEnumeration   = &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter - 1 ] );
               PHB_ITEM pEnumerator    = HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter - 1 ];
               HB_SIZE  ulEnumIndex    = ++HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter - 1 ];

               if( hb_objGetOpOver( pEnumeration ) & HB_CLASS_OP_FOREACH )
               {
                  HB_ITEM_NEW( ForEachOp );
                  HB_ITEM_NEW( ForEachIndex );

                  hb_itemPutNI( &ForEachOp, FOREACH_ENUMERATE );
                  hb_itemPutNL( &ForEachIndex, ( LONG ) ulEnumIndex );
                  hb_vmOperatorCall( pEnumeration, &ForEachOp, "__OPFOREACH", &ForEachIndex, 0, pEnumerator );

                  bPushLogical = TRUE;

                  if( hb_stackGetActionRequest() == HB_BREAK_REQUESTED )
                  {
                     hb_stackSetActionRequest( 0 );
                     bPushLogical = FALSE;
                  }
               }
               else if( HB_IS_ARRAY( pEnumeration ) || HB_IS_STRING( pEnumeration ) )
                  bPushLogical = hb_arrayGetByRef( pEnumeration, ulEnumIndex, pEnumerator );
               else if( HB_IS_HASH( pEnumeration ) )
               {
                  if( hb_hashGetCompatibility( pEnumeration ) )
                  {
                     ulEnumIndex = hb_hashAAGetRealPos( pEnumeration, ulEnumIndex );

                     if( ulEnumIndex )
                     {
                        hb_hashGet( pEnumeration, ulEnumIndex, pEnumerator );
                        bPushLogical = TRUE;
                     }
                  }
                  else
                  {
                     PHB_ITEM pKey = hb_hashGetKeyAt( pEnumeration, ulEnumIndex );

                     if( pKey )
                     {
                        PHB_ITEM pValue = hb_hashGetValueAt( pEnumeration, ulEnumIndex );

                        hb_clsInst( ( USHORT ) hb_clsGetHandleFromName( "HASHENTRY" ), pEnumerator );
                        hb_arraySet( pEnumerator, pEnumerator->item.asArray.value->ulLen - 2, pEnumeration );
                        hb_arraySet( pEnumerator, pEnumerator->item.asArray.value->ulLen - 1, pKey );
                        hb_arraySet( pEnumerator, pEnumerator->item.asArray.value->ulLen, pValue );

                        bPushLogical = TRUE;
                     }
                  }
               }
#ifdef DEBUG
               else
               {
                  /* Should never get FALSE! */
                  assert( bPushLogical = TRUE );
               }
#endif /* DEBUG */
            }
#ifdef DEBUG
            else
            {
               /* Should never get FALSE! */
               assert( bPushLogical = TRUE );
            }
#endif /* DEBUG */
            hb_vmPushLogical( bPushLogical );
            w++;
            break;
         }

         case HB_P_ENDENUMERATE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ENDENUMERATE" ) );

            if( HB_VM_STACK.wEnumCollectionCounter )
            {
               --HB_VM_STACK.wEnumCollectionCounter;

               if( hb_objGetOpOver( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ) ) & HB_CLASS_OP_FOREACH )
               {
                  HB_ITEM_NEW( ForEachOp );

                  hb_itemPutNI( &ForEachOp, FOREACH_END );

                  hb_vmOperatorCall( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ), &ForEachOp, "__OPFOREACH", NULL, 0, NULL );
                  hb_itemClear( &HB_VM_STACK.Return );
               }
            }

            hb_itemClear( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ) );
            HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;

            /* Incase EXIT was used. */
            hb_itemClear( HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] );
            w++;
            break;

         case HB_P_ENUMINDEX:
         {
            PHB_ITEM pItem;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ENUMINDEX" ) );

            pItem                      = hb_stackAllocItem();
            pItem->type                = HB_IT_LONG;
            pItem->item.asLong.length  = 10;
            pItem->item.asLong.value   = 0;

            if( HB_VM_STACK.wEnumCollectionCounter )
               pItem->item.asLong.value = HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter - 1 ];

            w++;
            break;
         }

         case HB_P_SENDWITH:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SENDWITH" ) );
         /* Intentionally NOT breaking - fall through! */

         case HB_P_SEND:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SEND" ) );

            hb_vmSend( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;

            if( pCode[ w ] == HB_P_POP )
            {
               HB_TRACE( HB_TR_DEBUG, ( "skipped HB_P_POP" ) );
               w++;
            }
            else
               hb_stackPushReturn();

            break;

         case HB_P_SENDWITHSHORT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SENDWITHSHORT" ) );
         /* Intentionally NOT breaking - fall through! */

         case HB_P_SENDSHORT:
         {
            USHORT   usParams = pCode[ w + 1 ];
            PHB_FUNC pFunc;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SENDSHORT" ) );

            if( HB_VM_STACK.iExtraParamsIndex == 0 )
            {
               if( usParams == 0 )
               {
                  PHB_ITEM pSelf = hb_stackItemFromTop( -1 );

                  if( HB_IS_OBJECT( pSelf ) && pSelf->item.asArray.value->uiPrevCls == 0 )
                  {
                     BOOL  bConstructor;
                     BOOL  bSymbol;

                     pFunc = hb_objGetMthd( pSelf, hb_stackItemFromTop( -2 )->item.asSymbol.value, FALSE, &bConstructor, 1, &bSymbol );

                     if( pFunc == hb___msgGetData )
                     {
                        if( ( HB_VM_STACK.pMethod )->uiData > ( USHORT ) pSelf->item.asArray.value->ulLen ) /* Resize needed ? */
                           hb_arraySize( pSelf, ( HB_VM_STACK.pMethod )->uiData );                          /* Make large enough */

                        hb_arrayGet( pSelf, ( HB_VM_STACK.pMethod )->uiData, &( HB_VM_STACK.Return ) );

                        hb_stackPop(); /* pSelf.  */
                        hb_stackPop(); /* Symbol. */

                        goto SEND_Finalization;
                     }
                     else if( pFunc == hb___msgGetClsData )
                     {
                        /* Recycle the Symbol Item. */
                        hb_arrayGet( hb_clsClassesArray()[ pSelf->item.asArray.value->uiClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiData, &( HB_VM_STACK.Return ) );

                        hb_stackPop(); /* pSelf.  */
                        hb_stackPop(); /* Symbol. */

                        goto SEND_Finalization;
                     }
                     else if( pFunc == hb___msgGetShrData )
                     {
                        if( ( HB_VM_STACK.pMethod )->uiSprClass )
                           /* Recycle the Symbol Item. */
                           hb_arrayGet( hb_clsClassesArray()[ ( HB_VM_STACK.pMethod )->uiSprClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiDataShared, &( HB_VM_STACK.Return ) );

                        hb_stackPop(); /* pSelf.  */
                        hb_stackPop(); /* Symbol. */

                        goto SEND_Finalization;
                     }
                  }
               }
               else if( usParams == 1 )
               {
                  PHB_ITEM pSelf = hb_stackItemFromTop( -2 );

                  if( HB_IS_OBJECT( pSelf ) && pSelf->item.asArray.value->uiPrevCls == 0 )
                  {
                     BOOL  bConstructor;
                     BOOL  bSymbol;

                     pFunc = hb_objGetMthd( pSelf, hb_stackItemFromTop( -3 )->item.asSymbol.value, FALSE, &bConstructor, 1, &bSymbol );

                     if( pFunc == hb___msgSetData )
                     {
                        if( ( HB_VM_STACK.pMethod )->uiData > ( USHORT ) pSelf->item.asArray.value->ulLen ) /* Resize needed ? */
                           hb_arraySize( pSelf, ( HB_VM_STACK.pMethod )->uiData );                          /* Make large enough */

                        hb_arraySet( pSelf, ( HB_VM_STACK.pMethod )->uiData, hb_stackItemFromTop( -1 ) );

                        hb_itemForwardValue( &( HB_VM_STACK.Return ), hb_stackItemFromTop( -1 ) );

                        hb_stackDec(); /* pNewValue. */
                        hb_stackPop(); /* pSelf.     */
                        hb_stackPop(); /* Symbol.    */

                        goto SEND_Finalization;
                     }
                     else if( pFunc == hb___msgSetClsData )
                     {
                        /* Recycle the Symbol Item. */
                        hb_arraySet( hb_clsClassesArray()[ pSelf->item.asArray.value->uiClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiData, hb_stackItemFromTop( -1 ) );

                        hb_itemForwardValue( &( HB_VM_STACK.Return ), hb_stackItemFromTop( -1 ) );

                        hb_stackDec(); /* pNewValue. */
                        hb_stackPop(); /* pSelf.     */
                        hb_stackPop(); /* Symbol.    */

                        goto SEND_Finalization;
                     }
                     else if( pFunc == hb___msgSetShrData )
                     {
                        if( ( HB_VM_STACK.pMethod )->uiSprClass )
                           /* Recycle the Symbol Item */
                           hb_arraySet( hb_clsClassesArray()[ ( HB_VM_STACK.pMethod )->uiSprClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiDataShared, hb_stackItemFromTop( -1 ) );

                        hb_itemForwardValue( &( HB_VM_STACK.Return ), hb_stackItemFromTop( -1 ) );

                        hb_stackDec(); /* pNewValue. */
                        hb_stackPop(); /* pSelf.     */
                        hb_stackPop(); /* Symbol.    */

                        goto SEND_Finalization;
                     }
                  }
               }
            }

            if( hb_stackGetActionRequest() != HB_BREAK_REQUESTED )
            {
               /* TraceLog( NULL, "Func: %p Trying hb_vmSend()\n", pFunc ); */
               hb_vmSend( usParams );
            }

            SEND_Finalization:

            w += 2;
            if( pCode[ w ] == HB_P_POP )
            {
               HB_TRACE( HB_TR_DEBUG, ( "skipped HB_P_POP" ) );
               w++;
            }
            else
               hb_stackPushReturn();

            break;
         }

         case HB_P_CLASSSETMODULE:
         {
            PHB_ITEM pClassHandle = hb_stackItemFromTop( -1 );

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_CLASSETMODULE" ) );

            if( HB_IS_INTEGER( pClassHandle ) )
               hb_clsSetModule( ( USHORT ) ( pClassHandle->item.asInteger.value ) );
            else
               hb_errRT_BASE( EG_ARG, 1603, NULL, "__ClsSetModule()", 1, pClassHandle );

            hb_stackPop(); /* pClassHandle. */
            w++;
            break;
         }

         case HB_P_IVARREF:
         {
            PHB_ITEM pSelf = hb_stackItemFromTop( -1 ), pMsg = hb_stackItemFromTop( -2 );

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_IVARREF" ) );

            if( HB_IS_OBJECT( pSelf ) ||
                ( HB_IS_ARRAY( pSelf ) && hb_cls_uiArrayClass ) ||
                ( HB_IS_BLOCK( pSelf ) && hb_cls_uiBlockClass ) ||
                ( HB_IS_STRING( pSelf ) && hb_cls_uiCharacterClass ) ||
                ( HB_IS_DATE( pSelf ) && hb_cls_uiDateClass ) ||
                ( HB_IS_LOGICAL( pSelf ) && hb_cls_uiLogicalClass ) ||
                ( HB_IS_NIL( pSelf ) && hb_cls_uiNilClass ) ||
                ( HB_IS_NUMERIC( pSelf ) && hb_cls_uiNumericClass ) ||
                ( HB_IS_POINTER( pSelf ) && hb_cls_uiPointerClass ) ||
                ( HB_IS_HASH( pSelf ) && hb_cls_uiHashClass ) )
            {
               PHB_FUNC pFunc = NULL;
               BOOL     bConstructor;
               BOOL     bSymbol;

               pFunc = hb_objGetMthd( pSelf, pMsg->item.asSymbol.value, FALSE, &bConstructor, 2, &bSymbol );
               if( pFunc == hb___msgGetData )
               {
                  if( ( HB_VM_STACK.pMethod )->uiData > ( USHORT ) pSelf->item.asArray.value->ulLen ) /* Resize needed ? */
                     hb_arraySize( pSelf, ( HB_VM_STACK.pMethod )->uiData );                          /* Make large enough */

                  /* Recycle the Symbol Item. */
                  hb_arrayGetByRef( pSelf, ( HB_VM_STACK.pMethod )->uiData, pMsg );
               }
               else if( pFunc == hb___msgGetClsData )
                  /* Recycle the Symbol Item. */
                  hb_arrayGetByRef( hb_clsClassesArray()[ pSelf->item.asArray.value->uiClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiData, pMsg );
               else if( pFunc == hb___msgGetShrData )
               {
                  if( ( HB_VM_STACK.pMethod )->uiSprClass )
                     /* Recycle the Symbol Item. */
                     hb_arrayGetByRef( hb_clsClassesArray()[ ( HB_VM_STACK.pMethod )->uiSprClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiDataShared, pMsg );
               }
               else if( hb_stackGetActionRequest() != HB_BREAK_REQUESTED )
               {
                  /*
                   * hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, "No instance variable", hb_stackItemFromTop( -2 )->item.asSymbol.value->szName, 1, pSelf );
                   * pMsg->type = HB_IT_NIL;
                   */

                  hb_vmSend( 0 );
                  hb_itemPushForward( &( HB_VM_STACK.Return ) );
                  w++;
                  break;
               }
               else if( HB_IS_HASH( pSelf ) )
               {
                  HB_SIZE        ulPos    = 0;
                  const char *   szIndex  = pMsg->item.asSymbol.value->szName;

                  /* Following are NOT assignable, in an array form, and or byref - commented by Ron 2006-09-14
                   * if( strcmp( szIndex, "CLASSNAME" ) == 0 )
                   * {
                   * hb_itemPutC( pMsg, "HASH" );
                   * }
                   * else if( strcmp( szIndex, "CLASSH" ) == 0 )
                   * {
                   * hb_itemPutNI( pMsg, 0 );
                   * }
                   * else if( strcmp( szIndex, "KEYS" ) == 0 )
                   * {
                   * hb_hashGetKeys( pMsg, pSelf );
                   * }
                   * else if( strcmp( szIndex, "VALUES" ) == 0 )
                   * {
                   * hb_hashGetValues( pMsg, pSelf );
                   * }
                   * else
                   */
                  {
                     HB_ITEM_NEW( hbIndex );

                     hb_itemPutCRawStatic( &hbIndex, szIndex, strlen( szIndex ) );

                     if( hb_hashScan( pSelf, &hbIndex, &ulPos ) )
                        hb_hashGet( pSelf, ulPos, pMsg );
                     else
                        hb_vmClassError( 0, "HASH", szIndex, pSelf );
                  }
               }
            }
            else if( HB_IS_HASH( pSelf ) )
            {
               HB_SIZE        ulPos    = 0;
               const char *   szIndex  = pMsg->item.asSymbol.value->szName;

               /* Following are NOT assignable, in an array form, and or byref - commented by Ron 2006-09-14
                * if( strcmp( szIndex, "CLASSNAME" ) == 0 )
                * {
                * hb_itemPutC( pMsg, "HASH" );
                * }
                * else if( strcmp( szIndex, "CLASSH" ) == 0 )
                * {
                * hb_itemPutNI( pMsg, 0 );
                * }
                * else if( strcmp( szIndex, "KEYS" ) == 0 )
                * {
                * hb_hashGetKeys( pMsg, pSelf );
                * }
                * else if( strcmp( szIndex, "VALUES" ) == 0 )
                * {
                * hb_hashGetValues( pMsg, pSelf );
                * }
                * else
                */
               {
                  HB_ITEM_NEW( hbIndex );

                  hb_itemPutCRawStatic( &hbIndex, szIndex, strlen( szIndex ) );

                  if( hb_hashScan( pSelf, &hbIndex, &ulPos ) )
                     hb_hashGet( pSelf, ulPos, pMsg );
                  else
                     hb_vmClassError( 0, "HASH", szIndex, pSelf );
               }
            }
            else
            {
               hb_errRT_BASE_SubstR( EG_NOOBJECT, 1004, "Not object", pMsg->item.asSymbol.value->szName, 1, pSelf );
               hb_itemForwardValue( pMsg, &( HB_VM_STACK.Return ) );
            }

            /* Symbol was recycled.  */
            hb_stackPop(); /* pSelf. */
            w++;
            break;
         }

         case HB_P_BASELINE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_BASELINE" ) );

            s_iBaseLine                                        = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            hb_stackBaseItem()->item.asSymbol.pCargo->lineno   = ( USHORT ) s_iBaseLine;

            /* printf( "BASE Proc: %s Line: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.pCargo->lineno ); */

#ifndef HB_NO_DEBUG
            if( s_bDebugging )
               hb_vmDebuggerShowLine( ( USHORT ) s_iBaseLine );
#endif /* HB_NO_DEBUG */

            HB_TRACE( HB_TR_INFO, ( "Opcode: HB_P_LINE: %s (%i)", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.pCargo->lineno ) );
            w += 3;
            break;

         case HB_P_LINE:
            HB_TRACE( HB_TR_INFO, ( "Opcode: HB_P_LINE: %s (%i)",
                                    hb_stackBaseItem()->item.asSymbol.value->szName,
                                    hb_stackBaseItem()->item.asSymbol.pCargo->lineno ) );

            hb_stackBaseItem()->item.asSymbol.pCargo->lineno = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

#ifndef HB_NO_DEBUG
            if( s_bDebugging )
               hb_vmDebuggerShowLine( hb_stackBaseItem()->item.asSymbol.pCargo->lineno );
#endif /* HB_NO_DEBUG */

            w += 3;
            break;

         case HB_P_LINEOFFSET:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LINEOFFSET" ) );

            hb_stackBaseItem()->item.asSymbol.pCargo->lineno = ( USHORT ) ( s_iBaseLine + ( BYTE ) pCode[ w + 1 ] );

#ifndef HB_NO_DEBUG
            if( s_bDebugging )
               hb_vmDebuggerShowLine( hb_stackBaseItem()->item.asSymbol.pCargo->lineno );
#endif /* HB_NO_DEBUG */

            HB_TRACE( HB_TR_INFO, ( "Opcode: HB_P_LINEOFFSET: %s (%i)", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.pCargo->lineno ) );
            w += 2;
            break;

         case HB_P_PARAMETER:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PARAMETER" ) );
            hb_memvarNewParameter( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), hb_stackItemFromBase( pCode[ w + 3 ] ) );
            w += 4;
            break;

         case HB_P_FRAME:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FRAME" ) );
            hb_vmFrame( ( unsigned short ) pCode[ w + 1 ], pCode[ w + 2 ] );
            w += 3;
            break;

         case HB_P_LARGEFRAME:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FRAME" ) );
            hb_vmFrame( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), pCode[ w + 3 ] );
            w += 4;
            break;

         case HB_P_SFRAME:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SFRAME" ) );
            hb_vmSFrame( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_STATICS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_STATICS" ) );
            hb_vmStatics( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), HB_PCODE_MKUSHORT( &( pCode[ w + 3 ] ) ) );
            w += 5;
            break;

         case HB_P_RETVALUE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_RETVALUE" ) );
            hb_vmRetValue();
            w++;
            break;

         case HB_P_LOCALNAME:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNAME" ) );

#ifndef HB_NO_DEBUG
            hb_vmLocalName( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ),
                            ( char * ) pCode + w + 3 );
#endif /* HB_NO_DEBUG */

            w += 3;
            while( pCode[ w++ ] )
            {
            }
            break;

         case HB_P_STATICNAME:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_STATICNAME" ) );
#ifndef HB_NO_DEBUG
            hb_vmStaticName( HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) ),
                             ( char * ) pCode + w + 4 );
/*
 *          hb_vmStaticName( pCode[ w + 1 ], HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) ),
 *                          ( char * ) pCode + w + 4 );
 */
#endif /* HB_NO_DEBUG  */
            w += 4;
            while( pCode[ w++ ] )
            {
            }
            break;

         case HB_P_MODULENAME:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MODULENAME" ) );
#ifndef HB_NO_DEBUG
            hb_vmModuleName( ( char * ) pCode + w + 1 );
#endif /* HB_NO_DEBUG  */
            while( pCode[ w++ ] )
            {
            }
            break;

         /* BEGIN SEQUENCE/RECOVER/END SEQUENCE */

         case HB_P_TRYBEGIN:
            /* Incase recent FINALLY hit a new TRY segment before the ENDFINALLY. */
            if( bCanFinalize && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_FINALIZED ) )
            {
/* #define DEBUG_FINALLY */
#ifdef DEBUG_FINALLY
               printf( "%s-> (TRYBEGIN)- Pending: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->uiActionRequest );
#endif /* DEBUG_FINALLY */
            }

            hb_vm_iTry++;

            lFinallySection   = 0;
            lCatchSection     = 0;
            lNextSection      = w + HB_PCODE_MKINT24( &pCode[ w + 1 ] );

#ifdef DEBUG_FINALLY
            printf( "Next: %i at %li\n", pCode[ lNextSection ], lNextSection );
#endif /* DEBUG_FINALLY */

            if( pCode[ lNextSection ] == HB_P_FINALLY )
            {
               lFinallySection = lNextSection;

#ifdef DEBUG_FINALLY
               printf( "Has finally at: %li and no catcher!\n", lFinallySection );
#endif /* DEBUG_FINALLY */
            }
            else
            {
               lCatchSection  = lNextSection;
               lNextSection   += HB_PCODE_MKINT24( &pCode[ lCatchSection + 1 ] );

               if( pCode[ lNextSection ] == HB_P_FINALLY )
               {
                  lFinallySection = lNextSection;

#ifdef DEBUG_FINALLY
                  printf( "Has finally at: %li after catch at: %li\n", lFinallySection, lCatchSection );
#endif /* DEBUG_FINALLY */
               }
               else
               {
#ifdef DEBUG_FINALLY
                  printf( "Has catch at: %li and no finally!\n", lCatchSection );
#endif /* DEBUG_FINALLY */
               }
            }
         /* Intentionally FALL through. */
         /* fallthrough */
         case HB_P_SEQBEGIN:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SEQBEGIN" ) );
            {
               PHB_SEQUENCE pSequence = ( PHB_SEQUENCE ) hb_xgrab( sizeof( HB_SEQUENCE ) );

               /*
                * Create the SEQUENCE envelope
                * [ break return value      ]  -4
                * [ address of recover code ]  -3
                * [ previous recover base   ]  -2
                * [ current recovery state  ]  -1
                * [                         ] <- new recover base
                */

               /*
                * 1) clear the storage for value returned by BREAK statement
                */
               hb_stackAllocItem()->type     = HB_IT_NIL;

               /*
                * 2) store the address of RECOVER or END opcode
                */
               ( *HB_VM_STACK.pPos )->type   = HB_IT_LONG;

               pSequence->lRecover           = ( ULONG ) w + HB_PCODE_MKINT24( &pCode[ w + 1 ] );

               /*
                * 3) store current RECOVER base
                */
               pSequence->lBase              = ( ULONG ) hb_stackTopOffset();

               /* Make sure it's NIL */
               hb_itemClear( hb_stackItem( pSequence->lBase - 1 ) );

               pSequence->uiStatus = 0;

               /*
                * 4) store current bCanRecover flag - in a case of nested sequences
                * in the same procedure/function
                */
               if( bCanRecover )
                  pSequence->uiStatus |= HB_SEQ_PRESET_CANRECOVER;

               if( bCanFinalize )
                  pSequence->uiStatus |= HB_SEQ_PRESET_CANFINALIZE;

               /*
                * we are now inside a valid SEQUENCE envelope
                */
               bCanRecover                         = TRUE;

               pSequence->wEnumCollectionCounter   = HB_VM_STACK.wEnumCollectionCounter;
               pSequence->wWithObjectCounter       = HB_VM_STACK.wWithObjectCounter;

               pSequence->lFinally                 = 0;
               pSequence->uiActionRequest          = 0;

               pSequence->pPrev                    = HB_VM_STACK.pSequence;
               HB_VM_STACK.pSequence               = pSequence;

               if( pCode[ w ] == HB_P_TRYBEGIN )
               {
                  HB_VM_STACK.pSequence->pPrevErrBlock   = hb_errorBlock( &hb_vm_BreakBlock );

                  bCanFinalize                           = ( BOOL ) lFinallySection;

                  if( bCanFinalize )
                  {
                     HB_VM_STACK.pSequence->lFinally = ( ULONG ) lFinallySection;

                     if( lCatchSection == 0 )
                        HB_VM_STACK.pSequence->uiStatus |= ( HB_SEQ_RECOVERED | HB_SEQ_RETHROW );

#ifdef DEBUG_FINALLY
                     printf( "%s->Finally %p at: %li bCanFinalize: %i Prev: %li\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence->lFinally, HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE, HB_VM_STACK.pSequence->pPrev );

                     if( HB_VM_STACK.pSequence->pPrev )
                        printf( "%s->PREV Finally at: %li\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->pPrev->lFinally );
#endif /* DEBUG_FINALLY */
                  }
               }
               else
                  bCanFinalize = FALSE;

               w += 4;
               break;
            }

         case HB_P_TRYEND:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_TRYEND" ) );

            hb_vm_iTry--;
            hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
            hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );

#ifdef DEBUG_FINALLY
            printf( "%s->TRYEND: %li Finally: %li\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence->lFinally );
#endif /* DEBUG_FINALLY */

         /* Intentionally FALL through. */
         /* fallthrough */
         case HB_P_SEQEND:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SEQEND" ) );

            if( HB_VM_STACK.pSequence->lFinally == 0 )
            {
               PHB_SEQUENCE pFree;

               /* Discard the value returned by BREAK statement - there
                * was no RECOVER clause or there was no BREAK statement
                */
               hb_stackPop();

               /*
                * Remove the SEQUENCE envelope
                * This is executed either at the end of sequence or as the
                * response to the break statement if there is no RECOVER clause
                * but ONLY if we don't also have a FINALLY section.
                */
               pFree                   = HB_VM_STACK.pSequence;

               /*
                * Restore previous recovery state
                */
               bCanRecover             = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANRECOVER;
               bCanFinalize            = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE;

               HB_VM_STACK.pSequence   = HB_VM_STACK.pSequence->pPrev;
               hb_xfree( ( void * ) pFree );
            }
            else
               HB_VM_STACK.pSequence->uiStatus |= HB_SEQ_RECOVERED;

            /*
             * skip over RECOVER section, if any.
             */
            w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            break;

         case HB_P_TRYRECOVER:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_TRYRECOVER" ) );

            hb_vm_iTry--;
            hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
            hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );

#ifdef DEBUG_FINALLY
            printf( "%s->TRYRECOVER: %p Finally: %i bCanFinalize %i got Type: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence->lFinally, bCanFinalize, hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 )->type );
#endif /* DEBUG_FINALLY */

            /* The HB_VM_STACK.pSequence->lBase will now be poped as stack top, but it must be reserved for potential forwarding to outer! */
            hb_stackPush();
            hb_itemForwardValue( hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );

            w += 3;
         /* Intentionally FALL through. */
         /* fallthrough */
         case HB_P_SEQRECOVER:
         {
            PHB_SEQUENCE pFree = HB_VM_STACK.pSequence;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SEQRECOVER" ) );

            /*
             * Execute the RECOVER code
             */

            /*
             * Leave the value returned from BREAK  - it will be popped
             * in next executed opcode
             */

            if( HB_VM_STACK.pSequence->lFinally == 0 )
            {
               /*
                * Restore previous recovery state
                */
               bCanRecover             = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANRECOVER;
               bCanFinalize            = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE;

               HB_VM_STACK.pSequence   = HB_VM_STACK.pSequence->pPrev;
               hb_xfree( pFree );
            }
            else
               HB_VM_STACK.pSequence->uiStatus |= HB_SEQ_RECOVERED;

            w++;
            break;
         }

         case HB_P_FINALLY:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FINALLY" ) );

#ifdef DEBUG_FINALLY
            printf( "%s->Finally status: %p request: %i Top: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->uiStatus, HB_VM_STACK.pSequence->uiActionRequest, hb_stackItemFromTop( -1 )->type );
#endif /* DEBUG_FINALLY */

            HB_VM_STACK.pSequence->uiStatus |= HB_SEQ_FINALIZED;
            w++;
            break;

         case HB_P_ENDFINALLY:
         {
            PHB_SEQUENCE pFree = HB_VM_STACK.pSequence;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ENDFINALLY" ) );

            if( HB_VM_STACK.pSequence->uiActionRequest )
            {
               hb_stackSetActionRequest( HB_VM_STACK.pSequence->uiActionRequest );

               if( hb_stackGetActionRequest() & HB_BREAK_REQUESTED )
               {
                  if( HB_VM_STACK.pSequence->pPrev )
                  {
                     hb_itemForwardValue( hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 ), hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 ) );

#ifdef DEBUG_FINALLY
                     if( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RETHROW )
                        printf( "Rethrow type: %i after completed execution\n", hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 )->type );
                     else
                        printf( "Forwarded to outer type: %i after completed execution\n", hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 )->type );
#endif /* DEBUG_FINALLY */
                  }
               }

#ifdef DEBUG_FINALLY
               printf( "%s->Restored Deferred Action %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackGetActionRequest() );
#endif /* DEBUG_FINALLY */
            }

            hb_stackPop();

            bCanRecover             = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANRECOVER;
            bCanFinalize            = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE;

            HB_VM_STACK.pSequence   = HB_VM_STACK.pSequence->pPrev;
            hb_xfree( ( void * ) pFree );

#ifdef DEBUG_FINALLY
            printf( "%s->Completed execution: %p Restored: %p Finally: %li Pending: %i bCanFinalize: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, pFree, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence ? HB_VM_STACK.pSequence->lFinally : 0, hb_stackGetActionRequest(), bCanFinalize );
#endif /* DEBUG_FINALLY */

            w++;
            break;
         }

         /* Jumps */

         case HB_P_JUMPNEAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMPNEAR" ) );
            w += ( signed char ) pCode[ w + 1 ];
            break;

         case HB_P_JUMP:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMP" ) );
            w += HB_PCODE_MKSHORT( &pCode[ w + 1 ] );
            break;

         case HB_P_JUMPFAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMPFAR" ) );
            w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            break;

         case HB_P_JUMPFALSENEAR:
         {
            int iJump = 2;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMPFALSENEAR" ) );

            if( ! hb_vmPopLogical() )
               iJump = ( signed char ) pCode[ w + 1 ];

            w += iJump;
            break;
         }

         case HB_P_JUMPFALSE:
         {
            short iJump = 3;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMPFALSE" ) );

            if( ! hb_vmPopLogical() )
               iJump = HB_PCODE_MKSHORT( &pCode[ w + 1 ] );

            w += iJump;
            break;
         }

         case HB_P_JUMPFALSEFAR:
         {
            LONG iJump = 4;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMPFALSEFAR" ) );

            if( ! hb_vmPopLogical() )
               iJump = HB_PCODE_MKINT24( &pCode[ w + 1 ] );

            w += iJump;
            break;
         }

         case HB_P_JUMPTRUENEAR:
         {
            int iJump = 2;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMPTRUENEAR" ) );

            if( hb_vmPopLogical() )
               iJump = ( signed char ) pCode[ w + 1 ];

            w += iJump;
            break;
         }

         case HB_P_JUMPTRUE:
         {
            short iJump = 3;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMPTRUE" ) );

            if( hb_vmPopLogical() )
               iJump = HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) );

            w += iJump;
            break;
         }

         case HB_P_JUMPTRUEFAR:
         {
            LONG iJump = 4;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_JUMPTRUEFAR" ) );

            if( hb_vmPopLogical() )
               iJump = HB_PCODE_MKINT24( &pCode[ w + 1 ] );

            w += iJump;
            break;
         }

         /* Push */

         case HB_P_TRUE:
         {
            PHB_ITEM pItem;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_TRUE" ) );

            pItem                         = hb_stackAllocItem();
            pItem->type                   = HB_IT_LOGICAL;
            pItem->item.asLogical.value   = TRUE;
            w++;
            break;
         }

         case HB_P_FALSE:
         {
            PHB_ITEM pItem;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_FALSE" ) );

            pItem                         = hb_stackAllocItem();
            pItem->type                   = HB_IT_LOGICAL;
            pItem->item.asLogical.value   = FALSE;
            w++;
            break;
         }

         case HB_P_ONE:
         {
            PHB_ITEM pItem;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ONE" ) );

            pItem                         = hb_stackAllocItem();
            pItem->type                   = HB_IT_INTEGER;
            pItem->item.asInteger.value   = 1;
            pItem->item.asInteger.length  = 10;
            w++;
            break;
         }

         case HB_P_ZERO:
         {
            PHB_ITEM pItem;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ZERO" ) );

            pItem                         = hb_stackAllocItem();
            pItem->type                   = HB_IT_INTEGER;
            pItem->item.asInteger.value   = 0;
            pItem->item.asInteger.length  = 10;
            w++;
            break;
         }

         case HB_P_PUSHNIL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHNIL" ) );

            hb_stackAllocItem()->type = HB_IT_NIL;
            w++;
            break;

         case HB_P_PUSHBYTE:
         {
            PHB_ITEM pItem;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHBYTE" ) );

            pItem                         = hb_stackAllocItem();
            pItem->type                   = HB_IT_INTEGER;
            pItem->item.asInteger.value   = ( signed char ) pCode[ w + 1 ];
            pItem->item.asInteger.length  = 10;
            w                             += 2;
            break;
         }

         case HB_P_PUSHINT:
         {
            PHB_ITEM pItem;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHINT" ) );

            pItem                         = hb_stackAllocItem();
            pItem->type                   = HB_IT_INTEGER;
            pItem->item.asInteger.value   = HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) );
            pItem->item.asInteger.length  = 10;
            w                             += 3;
            break;
         }

         case HB_P_PUSHLONG:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHLONG" ) );

#if HB_INT_MAX >= INT32_MAX
            hb_vmPushIntegerConst( ( int ) HB_PCODE_MKLONG( &pCode[ w + 1 ] ) );
#else
            hb_vmPushLongConst( ( LONG ) HB_PCODE_MKLONG( &pCode[ w + 1 ] ) );
#endif /* HB_INT_MAX >= INT32_MAX */
            w += 5;
            break;

         case HB_P_PUSHLONGLONG:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHLONGLONG" ) );
#if ! defined( HB_LONG_LONG_OFF )
            hb_vmPushLongLongConst( HB_PCODE_MKLONGLONG( &pCode[ w + 1 ] ) );
#else
            hb_vmPushDoubleConst( HB_PCODE_MKLONGLONG( &pCode[ w + 1 ] ),
                                  HB_DEFAULT_WIDTH, HB_DEFAULT_DECIMALS );
#endif /* HB_LONG_LONG_OFF */
            w += 9;
            break;

         case HB_P_PUSHDOUBLE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHDOUBLE" ) );

            hb_vmPushDoubleConst( HB_PCODE_MKDOUBLE( &pCode[ w + 1 ] ),
                                  ( int ) *( BYTE * ) &pCode[ w + 1 + sizeof( double ) ],
                                  ( int ) *( BYTE * ) &pCode[ w + 1 + sizeof( double ) + sizeof( BYTE ) ] );

            w += 1 + sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE );
            break;

         case HB_P_PUSHDATETIME:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHDATETIME" ) );

            hb_vmPushDateTime( HB_PCODE_MKLONG( &pCode[ w + 1 ] ), HB_PCODE_MKLONG( &pCode[ w + 5 ] ) );
            w += sizeof( UINT32 ) + sizeof( UINT32 ) + 1;

            break;

         case HB_P_PUSHDATE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHDATE" ) );

            hb_vmPushDate( HB_PCODE_MKLONG( &pCode[ w + 1 ] ) );
            w += 5;
            break;

         case HB_P_PUSHSTR:
         {
            USHORT uiSize = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHSTR" ) );

            if( bDynCode )
               hb_vmPushString( ( char * ) pCode + w + 3, ( ULONG ) ( uiSize - 1 ) );
            else
               hb_itemPushStaticString( ( char * ) pCode + w + 3, ( ULONG ) ( uiSize - 1 ) );

            w += ( 3 + uiSize );
            break;
         }

         case HB_P_PUSHSTRSHORT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHSTRSHORT" ) );
            {
               BYTE uiSize = pCode[ w + 1 ];

               if( bDynCode )
                  hb_vmPushString( ( char * ) pCode + w + 2, ( ULONG ) ( uiSize - 1 ) );
               else
                  hb_itemPushStaticString( ( char * ) pCode + w + 2, ( ULONG ) ( uiSize - 1 ) );

               w += ( 2 + uiSize );
               break;
            }

         case HB_P_PUSHSTRHIDDEN:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHSTRHIDDEN" ) );
            {
               ULONG    ulSize      = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
               ULONG    ulBufferLen = HB_PCODE_MKUSHORT( &( pCode[ w + 4 ] ) );
               BYTE *   pBuffer;

               pBuffer = hb_vmUnhideString( pCode[ w + 3 ], ulSize, pCode + w + 6, ulBufferLen );

               hb_itemPutCPtr( hb_stackAllocItem(), ( char * ) pBuffer, ulSize - 1 );

               w += ( 6 + ulBufferLen );
               break;
            }

         case HB_P_PUSHBLOCK:
         {
            USHORT usSize = HB_PCODE_MKUSHORT( &pCode[ w + 1 ] );

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHBLOCK" ) );

            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5 +6 -> number of referenced local variables
             * +7    -> start of table with referenced local variables
             */
            hb_vmPushBlock( pCode + w + 3, usSize - 3, bDynCode );
            w += usSize;
            break;
         }

         case HB_P_PUSHBLOCKSHORT:
         {
            USHORT usSize = pCode[ w + 1 ];

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHBLOCKSHORT" ) );
            /* +0    -> _pushblock
             * +1    -> size of codeblock
             */
            hb_vmPushBlockShort( pCode + w + 2, usSize - 2, bDynCode );
            w += usSize;
            break;
         }

         case HB_P_PUSHSELF:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHSELF" ) );
            hb_vmPush( hb_stackSelfItem() );
            w++;
            break;

         case HB_P_PUSHWITH:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHWITH" ) );
            if( HB_VM_STACK.wWithObjectCounter )
               hb_vmPush( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ) );
            else
               hb_vmPushNil();

            w++;
            break;

         case HB_P_PUSHSYM:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHSYM" ) );
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSYMNEAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHSYMNEAR" ) );
            hb_vmPushSymbol( pSymbols + ( USHORT ) pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_PUSHALIAS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHALIAS" ) );
            hb_vmPushAlias();
            w++;
            break;

         case HB_P_PUSHALIASEDFIELD:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ALIASFIELD" ) );
            hb_vmPushAliasedField( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHALIASEDFIELDNEAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ALIASFIELDNEAR" ) );
            hb_vmPushAliasedField( pSymbols + pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_PUSHALIASEDVAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ALIASEDVAR" ) );
            hb_vmPushAliasedVar( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHFIELD:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHFIELD" ) );
            /* It pushes the current value of the given field onto the eval stack
             */
            hb_rddGetFieldValue( hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHGLOBAL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHGLOBAL" ) );
            /* JC1: Should be threadsafe: pCodes are done at compile time, and
             * a clash with another thread here should just bring to a race,
             * not a crash; a race that must be resolved at PRG level.
             */
            hb_vmPush( HB_GETGLOBALS()[ pCode[ w + 1 ] ] );
            w += 2;
            break;

         case HB_P_PUSHGLOBALREF:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHGLOBALREF" ) );
            {
               PHB_ITEM pItem    = hb_stackAllocItem();
               short    iGlobal  = pCode[ w + 1 ];

               pItem->type                               = HB_IT_BYREF;
               pItem->item.asRefer.value                 = iGlobal + 1; /* To offset the -1 below. */
               pItem->item.asRefer.offset                = -1;          /* Because 0 will be translated as a STATIC in hb_itemUnref(); */
               pItem->item.asRefer.BasePtr.itemsbasePtr  = HB_GETGLOBALSPTR();

#ifdef HB_UNSHARE_REFERENCES
               hb_itemUnShare( HB_GETGLOBALS()[ iGlobal ] );
#endif /* HB_UNSHARE_REFERENCES */
            }
            w += 2;
            break;

         case HB_P_PUSHLOCAL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHLOCAL" ) );
            hb_vmPushLocal( HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHLOCALNEAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHLOCALNEAR" ) );
            hb_vmPushLocal( ( SHORT ) ( signed char ) ( pCode[ w + 1 ] ) );
            w += 2;  /* only first two bytes are used */
            break;

         case HB_P_PUSHLOCALNEARINC:
         {
            PHB_ITEM pLocal;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARINC" ) );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) ( pCode[ w + 1 ] ) );
            hb_vmAddInt( pLocal, 1 );
            hb_vmPush( pLocal );
            w += 2;
            break;
         }

         case HB_P_PUSHLOCALNEARDEC:
         {
            PHB_ITEM pLocal;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARINC" ) );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) ( pCode[ w + 1 ] ) );
            hb_vmAddInt( pLocal, -1 );
            hb_vmPush( pLocal );
            w += 2;
            break;
         }

         case HB_P_LOCALNEARADD:
         {
            PHB_ITEM pLocal;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARADD" ) );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) pCode[ w + 1 ] );
            hb_vmPlus( pLocal, hb_stackItemFromTop( -1 ), pLocal );
            hb_stackPop();
            w += 2;
            break;
         }

         case HB_P_LOCALNEARADDINT:
         {
            PHB_ITEM pLocal;
            int      iAdd = HB_PCODE_MKSHORT( &pCode[ w + 2 ] );

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARADDINT" ) );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) ( pCode[ w + 1 ] ) );
            hb_vmAddInt( pLocal, iAdd );
            w += 4;
            break;
         }

         case HB_P_LOCALNEARINC:
         {
            PHB_ITEM pLocal;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARINC" ) );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) ( pCode[ w + 1 ] ) );
            hb_vmAddInt( pLocal, 1 );
            w += 2;
            break;
         }

         case HB_P_LOCALNEARDEC:
         {
            PHB_ITEM pLocal;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARDEC" ) );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) ( pCode[ w + 1 ] ) );
            hb_vmAddInt( pLocal, -1 );
            w += 2;
            break;
         }

         case HB_P_LOCALNEARSETINT:
         {
            int      iNewVal = HB_PCODE_MKSHORT( &( pCode[ w + 2 ] ) );
            PHB_ITEM pLocal;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARSETINT" ) );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) ( pCode[ w + 1 ] ) );
            if( ( ! HB_IS_NUMBER( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
            {
               hb_vmPushInteger( iNewVal );
               hb_vmOperatorCall( pLocal, hb_stackItemFromTop( -1 ), "__OPASSIGN", NULL, 1, pLocal );
            }
            else
               hb_itemPutNI( pLocal, iNewVal );

            w += 4;
            break;
         }

         case HB_P_LOCALNEARSETSTR:
         {
            USHORT   uiSize = HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) );
            PHB_ITEM pLocal;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARSETSTR" ) );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) ( pCode[ w + 1 ] ) );
            if( ( ! HB_IS_STRING( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
            {
               if( bDynCode )
                  hb_vmPushString( ( char * ) pCode + w + 4, ( ULONG ) ( uiSize - 1 ) );
               else
                  hb_itemPushStaticString( ( char * ) pCode + w + 4, ( ULONG ) ( uiSize - 1 ) );

               hb_vmOperatorCall( pLocal, hb_stackItemFromTop( -1 ), "__OPASSIGN", NULL, 1, pLocal );
            }
            else
            {
               if( bDynCode )
                  hb_itemPutCL( pLocal, ( char * ) ( pCode ) + w + 4, ( ULONG ) uiSize - 1 );
               else
                  hb_itemPutCRawStatic( pLocal, ( char * ) ( pCode ) + w + 4, ( ULONG ) uiSize - 1 );
            }
            w += ( 4 + uiSize );
            break;
         }

         case HB_P_LOCALNEARSETSTRHIDDEN:
         {
            ULONG    ulSize      = HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) );
            ULONG    ulBufferLen = HB_PCODE_MKUSHORT( &( pCode[ w + 5 ] ) );
            PHB_ITEM pLocal;
            BYTE *   pBuffer;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARSETSTRHIDDEN" ) );

            pBuffer = hb_vmUnhideString( pCode[ w + 4 ], ulSize, pCode + w + 7, ulBufferLen );

            HB_STACK_OR_BLOCK_LOCAL( pLocal, ( int ) ( signed char ) ( pCode[ w + 1 ] ) );
            if( ( ! HB_IS_STRING( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               hb_itemPutCPtr( pItem, ( char * ) pBuffer, ulSize - 1 );
               hb_vmOperatorCall( pLocal, pItem, "__OPASSIGN", NULL, 1, pLocal );
            }
            else
               hb_itemPutCPtr( pLocal, ( char * ) pBuffer, ulSize - 1 );

            w += ( 7 + ulBufferLen );
            break;
         }

         case HB_P_ADDINT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ADDINT" ) );
            {
               PHB_ITEM pItem = hb_stackItemFromTop( -1 );
               int      iAdd  = HB_PCODE_MKSHORT( &pCode[ w + 1 ] );

               hb_vmAddInt( pItem, iAdd );
               w += 3;
               break;
            }

         case HB_P_SWITCHCASE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_SWITCHCASE" ) );
            {
               PHB_ITEM pTop  = hb_stackItemFromTop( -1 );
               LONG     lCase = HB_PCODE_MKLONG( &( pCode[ w + 1 ] ) );

               if( pTop->type & HB_IT_INTEGER )
                  hb_vmPushLogical( ( LONG ) ( pTop->item.asInteger.value ) == lCase );
               else if( pTop->type & HB_IT_LONG )
                  hb_vmPushLogical( ( LONG ) pTop->item.asLong.value == lCase );
               else if( pTop->type & HB_IT_STRING && pTop->item.asString.length == 1 )
                  hb_vmPushLogical( ( LONG ) ( ( BYTE ) pTop->item.asString.value[ 0 ] ) == lCase );
               else
               {
                  PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1604, NULL, "SWITCH", 1, pTop );
                  if( pResult )
                  {
                     hb_vmPush( pResult );
                     hb_itemRelease( pResult );
                  }
               }
               w += 5;
               break;
            }

         case HB_P_LEFT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LEFT" ) );
            {
               USHORT   iNewLen  = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
               PHB_ITEM pString  = hb_stackItemFromTop( -1 );

               if( HB_IS_STRING( pString ) )
               {
                  if( ( ULONG ) iNewLen < pString->item.asString.length )
                     hb_itemPutCL( pString, pString->item.asString.value, iNewLen );
               }
               else
               {
                  PHB_ITEM pResult;

                  hb_vmPushInteger( iNewLen );
                  pResult = hb_errRT_BASE_Subst( EG_ARG, 1124, NULL, "LEFT", 2, pString, hb_stackItemFromTop( -1 ) );
                  if( pResult )
                  {
                     hb_stackPop();
                     hb_itemForwardValue( pString, pResult );
                     hb_itemRelease( pResult );
                  }
               }
               w += 3;
               break;
            }

         case HB_P_RIGHT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_RIGHT" ) );
            {
               PHB_ITEM pString  = hb_stackItemFromTop( -1 );
               USHORT   iNewLen  = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

               if( HB_IS_STRING( pString ) )
               {
                  if( ( ULONG ) iNewLen < pString->item.asString.length )
                     hb_itemPutCL( pString, pString->item.asString.value + pString->item.asString.length - iNewLen, iNewLen );
               }
               else
               {
                  /* Clipper doesn't error */
                  hb_itemPutC( pString, NULL );
               }
               w += 3;
               break;
            }

         case HB_P_PUSHLOCALREF:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHLOCALREF" ) );
            hb_vmPushLocalByRef( HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSTATIC:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHSTATIC" ) );
            hb_vmPushStatic( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSTATICREF:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHSTATICREF" ) );
            hb_vmPushStaticByRef( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHMEMVAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHMEMVAR" ) );
            hb_memvarGetValue( hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHMEMVARREF:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHMEMVARREF" ) );
            hb_memvarGetRefer( hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHMACROREF:
         {
            PHB_ITEM pTop  = hb_stackItemFromTop( -1 );
            PHB_SYMB pSym  = pTop->item.asSymbol.value;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHMEMVARREF" ) );

            hb_memvarGetRefer( pTop, pSym );
            w++;
            break;
         }

         case HB_P_PUSHVARIABLE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_PUSHVARIABLE" ) );
            /* Push a value of variable of unknown type onto the eval stack
             */
            hb_vmPushVariable( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_DUPLICATE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_DUPLICATE" ) );
            hb_vmDuplicate();
            w++;
            break;

         case HB_P_DUPLTWO:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_DUPLTWO" ) );
            hb_vmDuplTwo();
            w++;
            break;

         /* Pop */

         case HB_P_POP:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POP" ) );
            hb_stackPop();
            w++;
            break;

         case HB_P_POPALIAS:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPALIAS" ) );
            hb_vmPopAlias();
            w++;
            break;

         case HB_P_POPALIASEDFIELD:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPALIASEDFIELD" ) );
            hb_vmPopAliasedField( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_POPALIASEDFIELDNEAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPALIASEDFIELDNEAR" ) );
            hb_vmPopAliasedField( pSymbols + pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_POPALIASEDVAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPALIASEDVAR" ) );
            hb_vmPopAliasedVar( pSymbols + HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ) );
            w += 3;
            break;

         case HB_P_POPFIELD:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPFIELD" ) );
            /* Pops a value from the eval stack and uses it to set
             * a new value of the given field
             */
            hb_rddPutFieldValue( hb_stackItemFromTop( -1 ), pSymbols + HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ) );
            hb_stackPop();
            w += 3;
            break;

         case HB_P_POPGLOBAL:
         {
            PHB_ITEM pTop     = hb_stackItemFromTop( -1 );
            PHB_ITEM pGlobal  = HB_GETGLOBALS()[ pCode[ w + 1 ] ];

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPGLOBAL" ) );

            if( ( HB_IS_NUMBER( pGlobal ) && HB_IS_NUMBER( *( HB_VM_STACK.pPos - 1 ) ) ) || pGlobal->type == pTop->type )
               hb_itemForwardValue( pGlobal, pTop );
            else if( hb_objGetOpOver( pGlobal ) & HB_CLASS_OP_ASSIGN )
            {
               hb_vmOperatorCall( pGlobal, pTop, "__OPASSIGN", NULL, 0, pGlobal );
               hb_itemClear( pTop );
            }
            else
               hb_itemForwardValue( pGlobal, pTop );

            hb_stackDec();
            w += 2;
            break;
         }

         case HB_P_POPLOCAL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPLOCAL" ) );
            hb_vmPopLocal( HB_PCODE_MKSHORT( &pCode[ w + 1 ] ) );
            w += 3;
            break;

         case HB_P_POPLOCALNEAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPLOCALNEAR" ) );
            hb_vmPopLocal( ( SHORT ) ( signed char ) ( pCode[ w + 1 ] ) );
            w += 2;  /* only first two bytes are used */
            break;

         case HB_P_POPSTATIC:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPSTATIC" ) );
            hb_vmPopStatic( HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ) );
            w += 3;
            break;

         case HB_P_POPMEMVAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPMEMVAR" ) );
            hb_memvarSetValue( pSymbols + HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ),
                               hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            w += 3;
            break;

         case HB_P_POPVARIABLE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_POPVARIABLE" ) );
            /*
             * 2004-03-19 Ron Pinkas
             * Test with Clipper shows that for assignment, MEMVAR context is always used even if MEMVAR
             * does NOT exists, and a FIELD with this name exists!!!
             *
             * Here is the Test Ueed - Clipper produced NO R/T Error - indicating MEMVAR was created.
             *
             *   PROCEDURE Main()
             *
             *      USE Test
             *      First := First
             *      CLOSE
             *      ? First
             *
             *   RETURN
             */
            hb_memvarSetValue( pSymbols + HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ),
                               hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            w += 3;
            break;

         /* macro creation */

         case HB_P_MACROPOP:
            /* compile and run - pop a value from the stack */
            hb_macroSetValue( hb_stackItemFromTop( -1 ), pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPOPALIASED:
            /* compile and run - pop an aliased variable from the stack */
            hb_macroPopAliasedValue( hb_stackItemFromTop( -2  ), hb_stackItemFromTop( -1 ), pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSH:
            /* compile and run - leave the result on the stack
             * the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSHARG:
         {
            UINT     uiJump = 3;
            PHB_SYMB pSymTemp;

            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROPUSHARG" ) );

            /* compile and run - leave the result on the stack
             * the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHARG, pCode[ ++w ] );
            w++;

            if( HB_VM_STACK.iExtraParamsIndex && HB_VM_STACK.apExtraParamsSymbol[ HB_VM_STACK.iExtraParamsIndex - 1 ] == NULL )
            {
               switch( pCode[ w ] )
               {
                  case HB_P_PUSHSYMNEAR:
                     pSymTemp = pSymbols + ( USHORT ) ( pCode[ w + 1 ] );
                     uiJump   = 2;
					 /* fallthrough */
                  case HB_P_MPUSHSYM:
                  {
                     PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );

                     pSymTemp = pDynSym->pSymbol;
                     uiJump   = sizeof( PHB_DYNS ) + 1;
                  }
				  /* fallthrough */
                  default:
                     pSymTemp = pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
                     uiJump   = 3;
               }
               HB_VM_STACK.apExtraParamsSymbol[ HB_VM_STACK.iExtraParamsIndex - 1 ] = pSymTemp;
            }
            else
            {
               switch( pCode[ w ] )
               {
                  case HB_P_PUSHSYMNEAR:
                     uiJump   = 2;
                     break;
                  case HB_P_MPUSHSYM:
                     uiJump   = sizeof( PHB_DYNS ) + 1;
                     break;
                  default:
                     uiJump   = 3;
                     break;
               }
            }
            w += uiJump;
            break;
         }

         case HB_P_MACROLIST:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROLIST" ) );

            HB_VM_STACK.aiExtraElements[ HB_VM_STACK.iExtraElementsIndex++ ] = 0;
            w++;
            break;

         case HB_P_MACROPUSHLIST:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROPUSHLIST" ) );
            /* compile and run - leave the result on the stack
             * the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHLIST, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROLISTEND:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROLISTEND" ) );

            HB_VM_STACK.iExtraElements = HB_VM_STACK.aiExtraElements[ --HB_VM_STACK.iExtraElementsIndex ];
            w++;
            break;

         case HB_P_MACROPUSHINDEX:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROPUSHINDEX" ) );

            /* compile and run - leave the result on the stack *
             * the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHINDEX, pCode[ ++w ] );

            if( HB_VM_STACK.iExtraIndex )
            {
               HB_ITEM *      aExtraItems = ( HB_ITEM * ) hb_xgrab( sizeof( HB_ITEM ) * HB_VM_STACK.iExtraIndex );
               register int   i;

               /* Storing and removing the extra indexes. */
               for( i = HB_VM_STACK.iExtraIndex - 1; i >= 0; i-- )
               {
                  ( aExtraItems + i )->type = HB_IT_NIL;
                  hb_itemRawMove( aExtraItems + i, hb_stackItemFromTop( -1 ) );
                  hb_stackPop();
               }

               /* First index is still on stack.*/
               hb_vmArrayPush();

               /* Now process each of the additional index including the last one (we will skip the HB_P_ARRAYPUSH which is know to follow . */
               for( i = 0; i < HB_VM_STACK.iExtraIndex; i++ )
               {
                  hb_vmPush( aExtraItems + i );

                  if( HB_IS_COMPLEX( aExtraItems + i ) )
                     hb_itemClear( aExtraItems + i );

                  hb_vmArrayPush();
               }

               hb_xfree( aExtraItems );
               w++; /* To force skip the HB_P_ARRAYPUSH (was already processed above). */
            }
            w++;
            break;

         case HB_P_MACROPUSHPARE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROPUSHPARE" ) );

            /* compile and run - leave the result on the stack
             * the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHPARE, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSHALIASED:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROPUSHALIASED" ) );

            /* compile and run - leave an aliased variable on the stack */
            hb_macroPushAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROSYMBOL:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROSYMBOL" ) );

            /* compile into a symbol name (used in function calls) */
            hb_macroPushSymbol( hb_stackItemFromTop( -1 ) );
            w++;
            break;

         case HB_P_MACROTEXT:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MACROTEXT" ) );

            /* macro text substitution
             * "text &macro.other text"
             */
            hb_macroTextValue( hb_stackItemFromTop( -1 ) );
            w++;
            break;

         /* macro compiled opcodes - we are using symbol address here */

         case HB_P_MMESSAGE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MMESSAGE" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_vmPushSymbol( pDynSym->pSymbol );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPOPALIASEDFIELD:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPOPALIASEDFIELD" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_vmPopAliasedField( pDynSym->pSymbol );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPOPALIASEDVAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPOPALIASEDVAR" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_vmPopAliasedVar( pDynSym->pSymbol );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPOPFIELD:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPOPFIELD" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );

               /* Pops a value from the eval stack and uses it to set
                * a new value of the given field
                */
               hb_rddPutFieldValue( hb_stackItemFromTop( -1 ), pDynSym->pSymbol );
               hb_stackPop();
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPOPMEMVAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPOPMEMVAR" ) );
            {
               PHB_DYNS pDynSym  = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               PHB_ITEM    pTop     = *( HB_VM_STACK.pPos - 1 );

               hb_memvarSetValue( pDynSym->pSymbol, pTop );
               hb_stackPop();
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPUSHALIASEDFIELD:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHALIASEDFIELD" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_vmPushAliasedField( pDynSym->pSymbol );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPUSHALIASEDVAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHALIASEDVAR" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_vmPushAliasedVar( pDynSym->pSymbol );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPUSHBLOCK:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHBLOCK" ) );
            {
               /* NOTE: the pcode is stored in dynamically allocated memory
                * We need to handle it with more care than compile-time
                * codeblocks
                */
               /* +0    -> _pushblock
                * +1 +2 -> size of codeblock
                * +3 +4 -> number of expected parameters
                * +5    -> pcode bytes
                */

               hb_vmPushMacroBlock( ( BYTE * ) ( pCode + w ) );
               w += HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
               break;
            }

         case HB_P_MPUSHFIELD:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHBLOCK" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               /* It pushes the current value of the given field onto the eval stack
                */
               hb_rddGetFieldValue( hb_stackAllocItem(), pDynSym->pSymbol );
               HB_TRACE( HB_TR_INFO, ( "(hb_vmMPushField)" ) );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPUSHMEMVAR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHMEMVAR" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_memvarGetValue( hb_stackAllocItem(), pDynSym->pSymbol );
               HB_TRACE( HB_TR_INFO, ( "(hb_vmMPushMemvar)" ) );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPUSHMEMVARREF:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHMEMVAR" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_memvarGetRefer( hb_stackAllocItem(), pDynSym->pSymbol );
               HB_TRACE( HB_TR_INFO, ( "(hb_vmMPushMemvarRef)" ) );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPUSHSYM:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHSYM" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_vmPushSymbol( pDynSym->pSymbol );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPUSHVARIABLE:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHVARIABLE" ) );
            {
               PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + w + 1 );
               hb_vmPushVariable( pDynSym->pSymbol );
               w += sizeof( PHB_DYNS ) + 1;
               break;
            }

         case HB_P_MPUSHSTR:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_MPUSHSTR" ) );
            {
               USHORT uiSize = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

               hb_vmPushString( ( char * ) ( pCode + w + 3 ), ( ULONG ) ( uiSize - 1 ) );
               w += ( 3 + uiSize );
               break;
            }

         /* misc */

         case HB_P_NOOP:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_NOOP" ) );
            /* Intentionally do nothing */
            w++;
            break;

         case HB_P_ENDBLOCK:
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_ENDBLOCK" ) );
            hb_vmEndBlock();

            /* IMHO It will be cleaner to make exactly the same action here
             * as for HB_P_ENDPROC, instead of this direct return, Druzus
             */

            /*
             * *** NOTE!!! Return!!!
             */
            return;

         case HB_P_ENDPROC:
            HB_TRACE( HB_TR_INFO, ( "HB_P_ENDPROC" ) );
            /* manually inlined hb_vmRequestEndProc() for some C compilers
             * which does not make such optimisation
             */
            hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
            break;

         case HB_P_DIVERTOF:
            HB_TRACE( HB_TR_INFO, ( "HB_P_DIVERTOF" ) );
            hb_vmSetDivert( TRUE );
            w++;
            break;

         case HB_P_DIVERT:
            HB_TRACE( HB_TR_INFO, ( "HB_P_DIVERT" ) );
            hb_vmSetDivert( FALSE );
            w++;
            break;

         default:
            /* TODO: Include to failing pcode in the error message */
            hb_errInternal( HB_EI_VMBADOPCODE, NULL, NULL, NULL );
            break;
      }

      /* Accepts generalized quit request */
#ifdef HB_THREAD_SUPPORT
      if( hb_vm_bQuitRequest )
         hb_stackSetActionRequest( HB_QUIT_REQUESTED );
#endif /* HB_THREAD_SUPPORT */

      if( hb_stackGetActionRequest() )
      {
         /* Incase recent FINALLY hit a RETURN or BREAK before the ENDFINALLY. */
         if( bCanFinalize && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_FINALIZED ) )
         {
            PHB_SEQUENCE pFree = HB_VM_STACK.pSequence;

            /* Restore the deferred action overriding the new request. */
            if( hb_stackGetActionRequest() != HB_QUIT_REQUESTED && HB_VM_STACK.pSequence->uiActionRequest )
               hb_stackSetActionRequest( HB_VM_STACK.pSequence->uiActionRequest );

            if( hb_stackGetActionRequest() & HB_BREAK_REQUESTED )
            {
               if( HB_VM_STACK.pSequence->pPrev )
               {
                  hb_itemForwardValue( hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 ), hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 ) );

#ifdef DEBUG_FINALLY
                  if( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RETHROW )
                     printf( "Rethrow type: %i after partial execution\n", hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 )->type );
                  else
                     printf( "Forwarded to outer type: %i after partial execution\n", hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 )->type );
#endif /* DEBUG_FINALLY */
               }
            }

            bCanRecover             = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANRECOVER;
            bCanFinalize            = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE;

            HB_VM_STACK.pSequence   = HB_VM_STACK.pSequence->pPrev;
            hb_xfree( ( void * ) pFree );

#ifdef DEBUG_FINALLY
            printf( "%s->Removed partially executed: %p Restored: %p Finally: %i Pending: %i bCanFinalize: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, pFree, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence ? HB_VM_STACK.pSequence->lFinally : 0, hb_stackGetActionRequest(), bCanFinalize );
#endif /* DEBUG_FINALLY */
         }

         if( hb_stackGetActionRequest() & HB_ENDPROC_REQUESTED )
         {
            /* request to stop current procedure was issued
             * (from macro evaluation or diverted call with no DIVERT_RESUME)
             */
            if( bCanFinalize && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_FINALIZED ) == 0 )
            {
#ifdef DEBUG_FINALLY
               printf( "%s->DEFER RETURN - go to %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->lFinally );
#endif /* DEBUG_FINALLY */

               HB_VM_STACK.pSequence->uiStatus        |= HB_SEQ_FINALIZED;
               HB_VM_STACK.pSequence->uiActionRequest = HB_ENDPROC_REQUESTED;

               w                                      = HB_VM_STACK.pSequence->lFinally;
               hb_stackSetActionRequest( 0 );
            }
            else
            {
               hb_stackSetActionRequest( 0 );
               break;
            }
         }
         else if( hb_stackGetActionRequest() & HB_BREAK_REQUESTED )
         {
            if( bCanFinalize && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RECOVERED ) && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_FINALIZED ) == 0 )
            {
#ifdef DEBUG_FINALLY
               printf( "%s->DEFER BREAK - go to %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->lFinally );
#endif /* DEBUG_FINALLY */

               HB_VM_STACK.pSequence->uiStatus        |= HB_SEQ_FINALIZED;
               HB_VM_STACK.pSequence->uiActionRequest = HB_BREAK_REQUESTED;

               /* We didn't realy have a recover section, so do the bCanRecover too! */
               if( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RETHROW )
               {
                  assert( HB_VM_STACK.pSequence->lFinally == HB_VM_STACK.pSequence->lRecover );

#ifdef DEBUG_FINALLY
                  printf( "%s->Doing bCanRecover too\n", hb_stackBaseItem()->item.asSymbol.value->szName );
#endif /* DEBUG_FINALLY */

                  goto hb_vmExecute_CanRecover;
               }
               else
               {
                  w = HB_VM_STACK.pSequence->lFinally;
                  hb_stackSetActionRequest( 0 );
               }
            }
            else if( bCanRecover && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RECOVERED ) == 0 )
            {
               hb_vmExecute_CanRecover:

               /* Reset FOR EACH. */
               while( HB_VM_STACK.wEnumCollectionCounter > HB_VM_STACK.pSequence->wEnumCollectionCounter )
               {
                  HB_VM_STACK.wEnumCollectionCounter--;
                  hb_itemClear( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ) );
                  hb_itemClear( HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] );
                  HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;
               }

               /* Reset WITH OBJECT. */
               while( HB_VM_STACK.wWithObjectCounter > HB_VM_STACK.pSequence->wWithObjectCounter )
               {
                  --HB_VM_STACK.wWithObjectCounter;
                  hb_itemClear( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter ] ) );
               }

               /*
                * There is the BEGIN/END sequence deifined in current
                * procedure/function - use it to continue opcodes execution
                */

               /*
                * remove all items placed on the stack after BEGIN code
                */
               hb_stackRemove( ( LONG ) HB_VM_STACK.pSequence->lBase );

               w = HB_VM_STACK.pSequence->lRecover;

               /*
                * leave the SEQUENCE envelope on the stack - it will
                * be popped either in RECOVER or END opcode
                */
               hb_stackSetActionRequest( 0 );
            }
            else
               break;
         }
         else if( hb_stackGetActionRequest() & HB_QUIT_REQUESTED )
         {
#ifdef HB_THREAD_SUPPORT
            /* Generalize quit request so that the whole VM is affected */
            hb_vm_bQuitRequest = TRUE;
#endif /* HB_THREAD_SUPPORT */

            if( ! ( HB_VM_STACK.uiVMFlags & HB_SUSPEND_QUIT ) )
               exit( hb_vmQuit() );

            break;
         }
      }

      /* JC1: now we can safely test for cancellation & tell garbage we are ready*/
#if defined( HB_THREAD_SUPPORT )
      if( ! --HB_VM_STACK.iPcodeCount )
      {
         HB_VM_STACK.iPcodeCount = HB_VM_UNLOCK_PERIOD;
         HB_STACK_UNLOCK;
         HB_TEST_CANCEL;
         HB_STACK_LOCK;

         /* Run background functions every unlock period */
#if ! defined( HB_NO_BACKGROUND )
         if( pSet->HB_SET_BACKGROUNDTASKS )
            hb_backgroundRun();
#endif /* HB_NO_BACKGROUND */
      }
#endif /* HB_THREAD_SUPPORT */
   }

   /* No cancellation here */
   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

   HB_TRACE( HB_TR_DEBUG, ( "DONE hb_vmExecute(%p, %p)", pCode, pSymbols ) );

   HB_TRACE( HB_TR_DEBUG, ( "RESET PrivateBase hb_vmExecute(%p, %p)", pCode, pSymbols ) );

   /* Reset FOR EACH. */
   while( HB_VM_STACK.wEnumCollectionCounter > wEnumCollectionCounter )
   {
      HB_VM_STACK.wEnumCollectionCounter--;
      hb_itemClear( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ) );
      /* hb_itemClear( HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] ); */
      HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;
   }

   /* Reset WITH OBJECT. */
   while( HB_VM_STACK.wWithObjectCounter > wWithObjectCounter )
   {
      --HB_VM_STACK.wWithObjectCounter;
      hb_itemClear( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter ] ) );
   }

   /* JC1: do not allow cancellation or idle MT func: thread cleanup procedure
    * is under way, or another VM might return in control
    */

   /* TraceLog( NULL, "DONE! %s->hb_vmExecute(%p, %p, %p)\n", hb_stackBaseItem()->item.asSymbol.value->szName, pCode, pSymbols, HB_GETGLOBALS() ); */
}

HB_FUNC( HB_VMEXECUTE )
{
   HB_THREAD_STUB_STACK

   const BYTE * szString = ( const BYTE * ) hb_parc( 1 );

   if( szString )
   {
      HB_ISIZ lOffset = hb_stackTopOffset();

      hb_vmExecute( szString, ( PHB_SYMB ) hb_parptr( 2 ) );

      if( hb_stackTopOffset() > lOffset )
      {
#if 1
         if( hb_stackGetActionRequest() == 0 )
         {
            hb_itemForwardValue( &( HB_VM_STACK.Return ), hb_stackItemFromTop( -1 ) );

            do
            {
               hb_stackPop();
            }
            while( hb_stackTopOffset() > lOffset );
         }
#else
         hb_itemCopy( &( HB_VM_STACK.Return ), hb_stackItemFromTop( -1 ) );
#endif
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 9102, NULL, "HB_vmExecute", 3, hb_paramError( 1 ), hb_paramError( 2 ) );
}

/* ------------------------------- */
/* Operators ( mathematical        */
/*             character / misc )  */
/* ------------------------------- */

static void hb_vmAddInt( PHB_ITEM pResult, LONG lAdd )
{
   HB_THREAD_STUB_STACK

   double dNewVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmAddInt(%p,%ld)", pResult, lAdd ) );

   if( HB_IS_BYREF( pResult ) )
      pResult = hb_itemUnRef( pResult );

   if( HB_IS_NUMINT( pResult ) )
   {
      HB_LONG lVal = HB_ITEM_GET_NUMINTRAW( pResult ), lResult;

      lResult = lVal + lAdd;

      if( lAdd >= 0 ? lResult >= lVal : lResult < lVal )
      {
         HB_ITEM_PUT_NUMINTRAW( pResult, lResult );
         return;
      }
      else
         dNewVal = ( double ) lResult;
   }
   else if( HB_IS_DATETIME( pResult ) )
   {
      pResult->item.asDate.value += lAdd;
      return;
   }
   else if( HB_IS_DOUBLE( pResult ) )
      dNewVal = pResult->item.asDouble.value + lAdd;
   else if( HB_IS_STRING( pResult ) && pResult->item.asString.length == 1 )
   {
      hb_itemPutCLStatic( pResult, hb_szAscii[ ( UCHAR ) ( pResult->item.asString.value[ 0 ] + lAdd ) ], 1 );
      return;
   }
   else if( lAdd == 1 && ( hb_objGetOpOver( pResult ) & HB_CLASS_OP_INC ) )
   {
      hb_vmOperatorCallUnary( pResult, "__OPINC", pResult );
      return;
   }
   else if( lAdd == -1 && ( hb_objGetOpOver( pResult ) & HB_CLASS_OP_DEC ) )
   {
      hb_vmOperatorCallUnary( pResult, "__OPDEC", pResult );
      return;
   }
   else if( ( hb_objGetOpOver( pResult ) & HB_CLASS_OP_PLUS ) )
   {
      PHB_ITEM pAdd = hb_stackTopItem();
      hb_vmPushInteger( lAdd );
      hb_vmOperatorCall( pResult, pAdd, "__OPPLUS", NULL, 1, pResult );
      return;
   }
   else
   {
      PHB_ITEM pAdd = hb_stackTopItem();
      PHB_ITEM pSubst;

      if( lAdd > 0 )
      {
         hb_vmPushInteger( lAdd );
         pSubst = hb_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pResult, pAdd );
      }
      else
      {
         hb_vmPushInteger( -lAdd );
         pSubst = hb_errRT_BASE_Subst( EG_ARG, 1082, NULL, "-", 2, pResult, pAdd );
      }

      if( pSubst )
      {
         hb_stackPop();
         hb_itemForwardValue( pResult, pSubst );
         hb_itemRelease( pSubst );
      }
      return;
   }

   if( ! HB_IS_DOUBLE( pResult ) )
   {
      pResult->type                    = HB_IT_DOUBLE;
      pResult->item.asDouble.decimal   = 0;
   }

   pResult->item.asDouble.value  = dNewVal;
   pResult->item.asDouble.length = ( UINT ) HB_DBL_LENGTH( dNewVal );
}

/* NOTE: Clipper is resetting the number width on a negate. */

static void hb_vmNegate( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmNegate()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_INTEGER( pItem ) )
   {
#if -HB_INT_MAX > HB_INT_MIN
      if( pItem->item.asInteger.value < -HB_INT_MAX )
      {
#if HB_LONG_MAX > HB_INT_MAX
         HB_LONG lValue = ( HB_LONG ) pItem->item.asInteger.value;
         pItem->type                   = HB_IT_LONG;
         pItem->item.asLong.value      = -lValue;
         pItem->item.asLong.length     = HB_LONG_EXPLENGTH( -lValue );
#else
         double dValue = ( double ) pItem->item.asInteger.value;
         pItem->type                   = HB_IT_DOUBLE;
         pItem->item.asDouble.value    = -dValue;
         pItem->item.asDouble.length   = HB_DBL_LENGTH( -dValue );
#endif /* HB_LONG_MAX > HB_INT_MAX */
      }
      else
#endif /* -HB_INT_MAX > HB_INT_MIN */
      {
         pItem->type                   = HB_IT_INTEGER;
         pItem->item.asInteger.value   = -pItem->item.asInteger.value;
         pItem->item.asInteger.length  = HB_INT_EXPLENGTH( pItem->item.asInteger.value );
      }
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if -HB_LONG_MAX > HB_LONG_MIN
      if( pItem->item.asLong.value < -HB_LONG_MAX )
      {
         double dValue = ( double ) pItem->item.asLong.value;
         pItem->type                   = HB_IT_DOUBLE;
         pItem->item.asDouble.value    = -dValue;
         pItem->item.asDouble.length   = HB_DBL_LENGTH( -dValue );
      }
      else
#endif /* -HB_LONG_MAX > HB_LONG_MIN */
      {
         pItem->type                = HB_IT_LONG;
         pItem->item.asLong.value   = -pItem->item.asLong.value;
         pItem->item.asLong.length  = HB_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->type                   = HB_IT_DOUBLE;
      pItem->item.asDouble.value    = -pItem->item.asDouble.value;
      pItem->item.asDouble.length   = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1080, NULL, "-", 1, pItem );

      if( pResult )
      {
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmPlus( PHB_ITEM pLeft, PHB_ITEM pRight, PHB_ITEM pResult )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPlus()" ) );

   /* printf( "Left: %i, Right: %i\n", pLeft->type, pRight->type ); */

   /* Must be first because STRING (length 1) qualifies as NUMERIC! */
   if( HB_IS_STRING( pLeft ) && HB_IS_STRING( pRight ) )
   {
      HB_SIZE  ulLen1   = pLeft->item.asString.length;
      HB_SIZE  ulLen2   = pRight->item.asString.length;

      /* printf( "Adding '%s' + '%s'\n", pItem1->item.asString.value, pItem2->item.asString.value ); */

      if( ulLen2 )
      {
         if( ulLen1 )
         {
            /* if( ( double ) ( ( double ) ulLen1 + ( double ) ulLen2 ) < ( double ) ULONG_MAX ) */
            if( ulLen1 < ULONG_MAX - ulLen2 )
            {
               HB_SIZE ulNewLen = ulLen1 + ulLen2;

               if( pResult->item.asString.allocated && ( *( pResult->item.asString.pulHolders ) == 1 ) )
               {
                  __HB_STRING_REALLOC( pResult, ulNewLen );

                  if( pLeft != pResult )
                     hb_xmemcpy( pResult, pLeft->item.asString.value, ( size_t ) ulLen1 );

                  hb_xmemcpy( ( void * ) ( pResult->item.asString.value + ulLen1 ), ( void * ) pRight->item.asString.value, ( size_t ) ulLen2 );

               }
               else
               {
                  char * sResult = ( char * ) hb_xgrab( ulNewLen + 1 );

                  hb_xmemcpy( sResult, pLeft->item.asString.value, ( size_t ) ulLen1 );
                  hb_xmemcpy( sResult + ulLen1, pRight->item.asString.value, ( size_t ) ulLen2 );
                  hb_itemPutCPtr( pResult, sResult, ulNewLen );
               }
            }
            else
            {
               HB_TRACE( HB_TR_DEBUG, ( "Error! String overflow in hb_vmPlus()" ) );
               hb_errRT_BASE( EG_STROVERFLOW, 1209, NULL, "+", 2, pLeft, pRight );
            }
         }
         else
            hb_itemCopy( pResult, pRight );
      }
      else
      {
         if( pResult != pLeft )
            hb_itemCopy( pResult, pLeft );
      }
   }
   else if( ( HB_IS_DATETIME( pLeft ) && ( HB_IS_DATETIME( pRight ) || HB_IS_NUMERIC( pRight ) ) ) ||
            ( HB_IS_NUMERIC( pLeft ) && HB_IS_DATETIME( pRight ) ) )
      hb_vmSumDate( pLeft, pRight, pResult );
   else if( HB_IS_STRING( pLeft ) && ( HB_IS_NUMERIC( pLeft ) && HB_IS_NUMERIC( pRight ) ) )
      hb_itemPutCLStatic( pResult, hb_szAscii[ ( UCHAR ) ( pLeft->item.asString.value[ 0 ] + ( LONG ) hb_itemGetND( pRight ) ) ], 1 );
#if 0
   /*
    * Shoud: 1 + "a" produce "b" like 1 + Date() produces Date() + 1?
    *
    * RP: No, because single Char are considered numeric when used in numeric context.
    * The left side determines the context. Clipper's handling of 1 + Date() is an odd exception.
    */
   else if( HB_IS_STRING( pRight ) && ( HB_IS_NUMERIC( pLeft ) && HB_IS_NUMERIC( pRight ) ) )
      hb_itemPutCLStatic( pResult, hb_szAscii[ ( UCHAR ) ( pRight->item.asString.value[ 0 ] + ( LONG ) hb_itemGetND( pLeft ) ) ], 1 );
   }
#endif /* 0 */
   else if( HB_IS_NUMINT( pLeft ) && HB_IS_NUMINT( pRight ) )
   {
      HB_LONG  lNumber1 = hb_itemGetNInt( pLeft );
      HB_LONG  lNumber2 = hb_itemGetNInt( pRight );
      HB_LONG  lResult  = lNumber1 + lNumber2;

      if( lNumber2 >= 0 ? lResult >= lNumber1 : lResult < lNumber1 )
         hb_itemPutNInt( pResult, lResult );
      else
         hb_itemPutNDDec( pResult, ( double ) lNumber1 + ( double ) lNumber2, 0 );
   }
   else if( HB_IS_NUMERIC( pLeft ) && HB_IS_NUMERIC( pRight ) )
   {
      double   dNumber1;
      double   dNumber2;
      int      iDec1    = 0;
      int      iDec2    = 0;
      int      iType1   = ( int ) pLeft->type;
      int      iType2   = ( int ) pRight->type;

      dNumber1 = hb_itemGetNDDec( pLeft, &iDec1 );
      dNumber2 = hb_itemGetNDDec( pRight, &iDec2 );

      hb_itemPutNumType( pResult, dNumber1 + dNumber2, ( ( iDec1 > iDec2 ) ? iDec1 : iDec2 ), iType1, iType2 );
   }
   else if( hb_objGetOpOver( pLeft ) & HB_CLASS_OP_PLUS )
      hb_vmOperatorCall( pLeft, pRight, "__OPPLUS", NULL, 0, pResult );
   else if( HB_IS_HASH( pLeft ) && HB_IS_HASH( pRight ) )
   {
      HB_SIZE ulLen = pRight->item.asHash.value->ulTotalLen;
      HB_ITEM_NEW( hbNum );
      HB_ITEM_NEW( HashResult );

      hb_hashClone( pLeft, &HashResult );
      hb_itemPutNI( &hbNum, 0 ); /* normal merge mode */

      hb_hashMerge( &HashResult, pRight, 1, ulLen, &hbNum );
      hb_itemForwardValue( pResult, &HashResult );
   }
   else
   {
      if( ( HB_IS_TIMEFLAG( pLeft ) || HB_IS_TIMEFLAG( pRight ) ) && ( HB_IS_NUMERIC( pLeft ) && HB_IS_NUMERIC( pRight ) ) )
      {
         LONG lDate = pLeft->item.asDate.value;
         lDate                      += hb_itemGetNL( pRight );
         pLeft->item.asDate.value   = lDate;
         hb_itemPutDL( pResult, lDate );
      }
      else
      {
         PHB_ITEM pErrResult = hb_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pLeft, pRight );

         if( pErrResult )
         {
            hb_itemForwardValue( pResult, pErrResult );
            hb_itemRelease( pErrResult );
         }
      }
   }
}

static void hb_vmMinus( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMinus()" ) );

   pItem1   = hb_stackItemFromTop( -2 );
   pItem2   = hb_stackItemFromTop( -1 );

   /* Must be first because STRING (length 1) qualifies as NUMERIC! */
   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_SIZE  ulLen1   = pItem1->item.asString.length;
      HB_SIZE  ulLen2   = pItem2->item.asString.length;

      if( ulLen1 < ULONG_MAX - ulLen2 )
      {
         char *   szNewString = ( char * ) hb_xgrab( ulLen1 + ulLen2 + 1 );
         HB_SIZE  ulNewLen    = ulLen1 + ulLen2;

         while( ulLen1 && pItem1->item.asString.value[ ulLen1 - 1 ] == ' ' )
            ulLen1--;

         hb_xmemcpy( szNewString, pItem1->item.asString.value, ( size_t ) ulLen1 );
         hb_xmemcpy( szNewString + ulLen1, pItem2->item.asString.value, ( size_t ) ulLen2 );
         hb_xmemset( szNewString + ulLen1 + ulLen2, ' ', ( size_t ) ( pItem1->item.asString.length - ulLen1 ) );
         szNewString[ ulNewLen ] = '\0';

         HB_TRACE( HB_TR_DEBUG, ( "Released hb_vmMinus() Created \"%s\"", szNewString ) );

         hb_itemPutCPtr( pItem1, szNewString, ulNewLen );

         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 1210, NULL, "-", 2, pItem1, pItem2 );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_NUMBER( pItem2 ) )
      hb_vmSubDate( pItem1, pItem2 );
   else if( HB_IS_STRING( pItem1 ) && ( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) ) )
   {
      hb_itemPutCLStatic( pItem1, hb_szAscii[ ( UCHAR ) ( pItem1->item.asString.value[ 0 ] - ( LONG ) hb_itemGetND( pItem2 ) ) ], 1 );
      hb_stackPop();
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG  lNumber2 = hb_vmPopHBLong();
      HB_LONG  lNumber1 = hb_vmPopHBLong();
      HB_LONG  lResult  = lNumber1 - lNumber2;

      if( lNumber2 <= 0 ? lResult >= lNumber1 : lResult < lNumber1 )
         hb_vmPushNumInt( lResult );
      else
         hb_vmPushDouble( ( double ) lNumber1 - ( double ) lNumber2, 0 );
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double   dNumber2;
      double   dNumber1;
      int      iDec2;
      int      iDec1;
      int      iType2   = ( int ) pItem2->type;
      int      iType1   = ( int ) pItem1->type;

      dNumber2 = hb_vmPopDouble( &iDec2 );
      dNumber1 = hb_vmPopDouble( &iDec1 );

      hb_vmPushNumType( dNumber1 - dNumber2, ( ( iDec1 > iDec2 ) ? iDec1 : iDec2 ), iType1, iType2 );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_MINUS )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPMINUS", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else if( HB_IS_HASH( pItem1 ) && HB_IS_HASH( pItem2 ) )
   {
      HB_SIZE ulLen = pItem2->item.asHash.value->ulTotalLen;
      HB_ITEM_NEW( hbNum );
      HB_ITEM_NEW( HashResult );

      hb_hashClone( pItem1, &HashResult );
      hb_itemPutNI( &hbNum, 3 ); /* NOT mode */

      hb_hashMerge( &HashResult, pItem2, 1, ulLen, &hbNum );
      hb_stackPop();
      hb_stackPop();
      hb_itemPushForward( &HashResult );
   }
   else if( HB_IS_HASH( pItem1 ) && HB_IS_ARRAY( pItem2 ) )
   {
      HB_SIZE  ulLen = pItem2->item.asArray.value->ulLen;
      HB_SIZE  ulPos = 0;
      HB_ITEM_NEW( HashResult );
      PHB_ITEM pRef  = pItem2->item.asArray.value->pItems;

      hb_hashClone( pItem1, &HashResult );
      while( ulLen > 0 )
      {
         if( hb_hashScan( &HashResult, pRef, &ulPos ) )
            hb_hashRemove( &HashResult, ulPos );

         pRef++;
         ulLen--;
      }
      hb_stackPop();
      hb_stackPop();
      hb_itemPushForward( &HashResult );
   }
   else if( HB_IS_HASH( pItem1 ) && (
               pItem2->type & ( HB_IT_NUMERIC | HB_IT_STRING | HB_IT_DATE ) ) )
   {
      HB_SIZE ulPos = 0;
      HB_ITEM_NEW( HashResult );

      if( hb_hashScan( pItem1, pItem2, &ulPos ) )
      {
         hb_hashClone( pItem1, &HashResult );
         hb_hashRemove( &HashResult, ulPos );
      }

      hb_stackPop();
      hb_stackPop();
      hb_itemPushForward( &HashResult );
   }
   else
   {
      if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
      {
         HB_ITEM_NEW( Result );
         long lTime     = pItem1->item.asDate.time -
                          pItem2->item.asDate.time,
              lJulian   = pItem1->item.asDate.value -
                          pItem2->item.asDate.value;

         if( lTime != 0 )
            hb_itemPutNDDec( &Result, ( ( double ) lJulian + ( double ) lTime / HB_MILLISECS_PER_DAY ), HB_TIMEDIFF_DEC );
         else
            hb_itemPutNL( &Result, lJulian );

         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( &Result );
      }
      else if( HB_IS_DATETIME( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
      {
         HB_ITEM_NEW( Result );

         if( HB_IS_TIMEFLAG( pItem1 ) )
         {
            if( HB_IS_NUMINT( pItem2 ) )
               hb_vmTimeStampPut( &Result, pItem1->item.asDate.value -
                                  ( long ) hb_itemGetNL( pItem2 ),
                                  pItem1->item.asDate.time );
            else
               hb_vmTimeStampAdd( &Result, pItem1, -pItem2->item.asDouble.value );
         }
         else
            hb_itemPutDL( &Result, hb_itemGetDL( pItem1 ) - hb_itemGetNL( pItem2 ) );

         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( &Result );
      }
      else if( HB_IS_TIMEFLAG( pItem1 ) || HB_IS_TIMEFLAG( pItem2 ) )
      {
         if( ! ( HB_IS_TIMEFLAG( pItem1 ) ) )
            goto Datetime_Error;
         else
         {
            HB_ITEM_NEW( Result );
            double dResult = hb_itemGetDTD( pItem1 ) - hb_itemGetDTD( pItem2 );

            hb_itemPutND( &Result, dResult );
            hb_stackPop();
            hb_stackPop();
            hb_itemPushForward( &Result );
         }
      }
      else
         Datetime_Error:
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1082, NULL, "-", 2, pItem1, pItem2 );

         if( pResult )
         {
            hb_stackPop();
            hb_stackPop();
            hb_itemPushForward( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
}

static void hb_vmMult( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMult()" ) );

   pItem1   = hb_stackItemFromTop( -2 );
   pItem2   = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double   d2;
      double   d1;
      int      iDec2    = 0;
      int      iDec1    = 0;
      int      iType2   = ( int ) pItem2->type;
      int      iType1   = ( int ) pItem1->type;

      d2 = hb_vmPopDouble( &iDec2 );
      d1 = hb_vmPopDouble( &iDec1 );
      hb_vmPushNumType( d1 * d2, iDec1 + iDec2, iType1, iType2 );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_MULT )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPMULT", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmDivide( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDivide()" ) );

   pItem1   = hb_stackItemFromTop( -2 );
   pItem2   = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dDivisor = hb_itemGetND( pItem2 );

      if( dDivisor == 0.0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pItem1, pItem2 );

         if( pResult )
         {
            hb_stackPop();
            hb_stackPop();
            hb_itemPushForward( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
      {
         hb_stackPop(); /* pop divisor from the stack */

         /* If all both operand was integer and the result is an integer, too,
          * push the number without decimals. Clipper compatible. Actually,
          * this is not Clipper compatible. The only time Clipper returns 0
          * decimal places is for compiler optimized integer division with an
          * integer result. Therefore this code is not needed and has been
          * removed - David G. Holm <dholm@jsd-llc.com>
          * if( bIntegerOperands && fmod( hb_itemGetND( pItem1 ), dDivisor ) == 0.0 )
          * hb_vmPushNumber( hb_vmPopNumber() / dDivisor, 0 );
          * else
          */
         hb_vmPushDouble( hb_vmPopNumber() / dDivisor, hb_stackSetStruct()->HB_SET_DECIMALS );
      }
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_DIVIDE )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPDIVIDE", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmModulus( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmModulus()" ) );

   pItem1   = hb_stackItemFromTop( -2 );
   pItem2   = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lDivisor = HB_ITEM_GET_NUMINTRAW( pItem2 );

      if( lDivisor == 0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pItem1, pItem2 );

         if( pResult )
         {
            hb_stackPop();
            hb_stackPop();
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
      {
         PHB_SET_STRUCT pSet = hb_stackSetStruct();

         pItem2->type = HB_IT_NIL;
         hb_stackDec(); /* pop divisor from the stack */
         hb_stackDec();
         /* NOTE: Clipper always returns the result of modulus
                  with the SET number of decimal places. */
         if( pSet->HB_SET_DECIMALS == 0 )
            hb_vmPushNumInt( HB_ITEM_GET_NUMINTRAW( pItem1 ) % lDivisor );
         else
            hb_vmPushDouble( ( double ) ( HB_ITEM_GET_NUMINTRAW( pItem1 ) % lDivisor ), pSet->HB_SET_DECIMALS );
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dDivisor = hb_itemGetND( pItem2 );

      if( dDivisor == 0.0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pItem1, pItem2 );

         if( pResult )
         {
            hb_stackPop();
            hb_stackPop();
            hb_itemPushForward( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
      {
         hb_stackPop(); /* pop divisor from the stack */

         /* NOTE: Clipper always returns the result of modulus
                  with the SET number of decimal places. */
         hb_vmPushDouble( fmod( hb_vmPopNumber(), dDivisor ), hb_stackSetStruct()->HB_SET_DECIMALS );
      }
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_MOD )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPMOD", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmPower( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPower()" ) );

   pItem1   = hb_stackItemFromTop( -2 );
   pItem2   = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double   d2 = hb_vmPopNumber();
      double   d1 = hb_vmPopNumber();

      /* NOTE: Clipper always returns the result of power
               with the SET number of decimal places. */
      hb_vmPushDouble( pow( d1, d2 ), hb_stackSetStruct()->HB_SET_DECIMALS );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_POWER )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPPOWER", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1088, NULL, "^", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmInc( void )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInc()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
      hb_itemPutCLStatic( pItem, hb_szAscii[ ( UCHAR ) ( pItem->item.asString.value[ 0 ] + 1 ) ], 1 );
   else if( HB_IS_DATE( pItem ) )
      pItem->item.asDate.value++;
   else if( HB_IS_NUMINT( pItem  ) )
   {
      if( HB_IS_INTEGER( pItem ) && pItem->item.asInteger.value < HB_INT_MAX )
      {
         pItem->item.asInteger.value++;
         pItem->item.asInteger.length = HB_INT_EXPLENGTH( pItem->item.asInteger.value );
      }
      else if( HB_IS_LONG( pItem ) && pItem->item.asLong.value < HB_LONG_MAX )
      {
         pItem->item.asLong.value++;
         pItem->item.asLong.length = HB_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
      else
      {
         double   dNumber;
         int      iDec  = 0;
         int      iType = ( int ) pItem->type;

         dNumber = hb_vmPopDouble( &iDec );
         hb_vmPushNumType( ++dNumber, iDec, iType, iType );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asDouble.value++;
      pItem->item.asDouble.length = ( UINT ) HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( hb_objGetOpOver( pItem ) & HB_CLASS_OP_INC )
      hb_vmOperatorCallUnary( pItem, "__OPINC", pItem );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1086, NULL, "++", 1, pItem );

      if( pResult )
      {
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmDec( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDec()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
      hb_itemPutCLStatic( pItem, hb_szAscii[ ( UCHAR ) ( pItem->item.asString.value[ 0 ] - 1 ) ], 1 );
   else if( HB_IS_DATE( pItem ) )
      pItem->item.asDate.value--;
   else if( HB_IS_NUMINT( pItem  ) )
   {
      if( HB_IS_INTEGER( pItem ) && pItem->item.asInteger.value > HB_INT_MIN )
      {
         pItem->item.asInteger.value--;
         pItem->item.asInteger.length = HB_INT_EXPLENGTH( pItem->item.asInteger.value );
      }
      else if( HB_IS_LONG( pItem ) && pItem->item.asLong.value > HB_LONG_MIN )
      {
         pItem->item.asLong.value--;
         pItem->item.asLong.length = HB_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
      else
      {
         double   dNumber;
         int      iDec  = 0;
         int      iType = ( int ) pItem->type;

         dNumber = hb_vmPopDouble( &iDec );
         hb_vmPushNumType( --dNumber, iDec, iType, iType );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asDouble.value--;
      pItem->item.asDouble.length = ( UINT ) HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( hb_objGetOpOver( pItem ) & HB_CLASS_OP_DEC )
      hb_vmOperatorCallUnary( pItem, "__OPDEC", pItem );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1087, NULL, "--", 1, pItem );

      if( pResult )
      {
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmFuncPtr( void ) /* pushes a function address pointer. Removes the symbol from the satck */
{
   HB_THREAD_STUB

   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFuncPtr()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_SYMBOL( pItem ) )
   {
      assert( pItem->item.asSymbol.pCargo );
      hb_xfree( ( void * ) pItem->item.asSymbol.pCargo );

      pItem->item.asPointer.value   = ( void * ) pItem->item.asSymbol.value;
      pItem->item.asPointer.collect = FALSE;

      pItem->type                   = HB_IT_POINTER;
   }
   else
      hb_errInternal( HB_EI_VMNOTSYMBOL, NULL, "hb_vmFuncPtr()", NULL );
}

/* ------------------------------- */
/* Operators (relational)          */
/* ------------------------------- */

static void hb_vmEqual( BOOL bExact )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEqual(%d)", ( int ) bExact ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_NIL( pItem1 ) )
   {
      pItem1->type                  = HB_IT_LOGICAL;
      pItem1->item.asLogical.value  = HB_IS_NIL( pItem2 );
      hb_stackPop(); /* clear the pItem2 */
   }
   else if( HB_IS_NIL( pItem2 ) )
   {
      hb_stackDec(); /* pItem2 is already NIL */
      hb_stackPop(); /* clear the pItem1 */
      hb_vmPushLogical( FALSE );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
	  BOOL i; 
	  if ( bExact ) 
	  {
         i = pItem1->item.asString.length == pItem2->item.asString.length &&
                        ( pItem1->item.asString.value == pItem2->item.asString.value ||
                          memcmp( pItem1->item.asString.value,
                                  pItem2->item.asString.value,
                                  pItem1->item.asString.length ) == 0 );
		  
	  }
	  else
	  {
	  
         i = hb_itemStrCmp( pItem1, pItem2, bExact ) == 0;
      }
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) ==
                                        HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type                  = HB_IT_LOGICAL;
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
      hb_vmPushLogical( hb_vmPopNumber() == hb_vmPopNumber() );
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem1 ) && HB_IS_TIMEFLAG( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value ==
                                          pItem2->item.asDate.value ) &&
                                        ( pItem1->item.asDate.time ==
                                          pItem2->item.asDate.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value ==
                                          pItem2->item.asDate.value );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( pItem1->item.asLogical.value ==
                                        pItem2->item.asLogical.value );
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_POINTER( pItem1 ) && HB_IS_POINTER( pItem2 ) )
   {
      BOOL bResult = ( pItem1->item.asPointer.value == pItem2->item.asPointer.value );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_EQUAL && bExact == FALSE )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPEQUAL", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_EXACTEQUAL && bExact )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPEXACTEQUAL", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else if( bExact && HB_IS_ARRAY( pItem1 ) && HB_IS_ARRAY( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asArray.value == pItem2->item.asArray.value;
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( bExact && HB_IS_HASH( pItem1 ) && HB_IS_HASH( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asHash.value == pItem2->item.asHash.value;
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( bExact && HB_IS_BLOCK( pItem1 ) && HB_IS_BLOCK( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asBlock.value == pItem2->item.asBlock.value;
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( pItem1->type != pItem2->type ||
            ( HB_IS_BLOCK( pItem1 ) && HB_IS_BLOCK( pItem2 ) ) ||
            ( HB_IS_ARRAY( pItem1 ) && HB_IS_ARRAY( pItem2 ) ) )
   {
      PHB_ITEM pResult;

      if( bExact )
         pResult = hb_errRT_BASE_Subst( EG_ARG, 1070, NULL, "==", 2, pItem1, pItem2 );
      else
         pResult = hb_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
   else
   {
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( FALSE );
   }
}

static void hb_vmNotEqual( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmNotEqual()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_NIL( pItem1 ) )
   {
      pItem1->type                  = HB_IT_LOGICAL;
      pItem1->item.asLogical.value  = ! HB_IS_NIL( pItem2 );
      hb_stackPop(); /* clear the pItem2 */
   }
   else if( HB_IS_NIL( pItem2 ) )
   {
      hb_stackDec(); /* pItem2 is already NIL */
      hb_stackPop(); /* clear the pItem1 */
      hb_vmPushLogical( TRUE );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );

      hb_stackPop();      
      hb_itemClear( pItem1 );
      
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = i != 0;  
      
       }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) !=
                                        HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type                  = HB_IT_LOGICAL;
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
      hb_vmPushLogical( hb_vmPopNumber() != hb_vmPopNumber() );
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( pItem1->item.asLogical.value !=
                                        pItem2->item.asLogical.value );
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem1 ) && HB_IS_TIMEFLAG( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value !=
                                          pItem2->item.asDate.value ) ||
                                        ( pItem1->item.asDate.time !=
                                          pItem2->item.asDate.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value !=
                                          pItem2->item.asDate.value );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_POINTER( pItem1 ) && HB_IS_POINTER( pItem2 ) )
   {
      BOOL bResult = ( pItem1->item.asPointer.value != pItem2->item.asPointer.value );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_NOTEQUAL )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPNOTEQUAL", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else if( pItem1->type != pItem2->type ||
            ( HB_IS_BLOCK( pItem1 ) && HB_IS_BLOCK( pItem2 ) ) ||
            ( HB_IS_ARRAY( pItem1 ) && HB_IS_ARRAY( pItem2 ) ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
   else if( HB_IS_HASH( pItem1 ) && HB_IS_HASH( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asHash.value != pItem2->item.asHash.value;
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else
   {
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( TRUE );
   }
}

static void hb_vmLess( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmLess()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i < 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) <
                                        HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type                  = HB_IT_LOGICAL;
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double   dNumber2 = hb_vmPopNumber();
      double   dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 < dNumber2 );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem1 ) && HB_IS_TIMEFLAG( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value <
                                          pItem2->item.asDate.value ) ||
                                        ( pItem1->item.asDate.value ==
                                          pItem2->item.asDate.value &&
                                          pItem1->item.asDate.time <
                                          pItem2->item.asDate.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value <
                                          pItem2->item.asDate.value );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( pItem1->item.asLogical.value <
                                        pItem2->item.asLogical.value );
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_LESS )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPLESS", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmLessEqual( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmLessEqual()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i <= 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) <=
                                        HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type                  = HB_IT_LOGICAL;
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double   dNumber2 = hb_vmPopNumber();
      double   dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 <= dNumber2 );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem1 ) && HB_IS_TIMEFLAG( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value <
                                          pItem2->item.asDate.value ) ||
                                        ( pItem1->item.asDate.value ==
                                          pItem2->item.asDate.value &&
                                          pItem1->item.asDate.time <=
                                          pItem2->item.asDate.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value <=
                                          pItem2->item.asDate.value );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( pItem1->item.asLogical.value <=
                                        pItem2->item.asLogical.value );
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_LESSEQUAL )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPLESSEQUAL", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmGreater( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmGreater()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i > 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) >
                                        HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type                  = HB_IT_LOGICAL;
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double   dNumber2 = hb_vmPopNumber();
      double   dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 > dNumber2 );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem1 ) && HB_IS_TIMEFLAG( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value >
                                          pItem2->item.asDate.value ) ||
                                        ( pItem1->item.asDate.value ==
                                          pItem2->item.asDate.value &&
                                          pItem1->item.asDate.time >
                                          pItem2->item.asDate.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value >
                                          pItem2->item.asDate.value );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( pItem1->item.asLogical.value >
                                        pItem2->item.asLogical.value );
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_GREATER )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPGREATER", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmGreaterEqual( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmGreaterEqual()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i >= 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) >=
                                        HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type                  = HB_IT_LOGICAL;
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double   dNumber2 = hb_vmPopNumber();
      double   dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 >= dNumber2 );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem1 ) && HB_IS_TIMEFLAG( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value >
                                          pItem2->item.asDate.value ) ||
                                        ( pItem1->item.asDate.value ==
                                          pItem2->item.asDate.value &&
                                          pItem1->item.asDate.time >=
                                          pItem2->item.asDate.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDate.value >=
                                          pItem2->item.asDate.value );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( pItem1->item.asLogical.value >=
                                        pItem2->item.asLogical.value );
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_GREATEREQUAL )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPGREATEREQUAL", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1076, NULL, ">=", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmInstringOrArray( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInstring()" ) );

   pItem1   = hb_stackItemFromTop( -2 );
   pItem2   = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      BOOL bResult = ( hb_strAt( pItem1->item.asString.value, pItem1->item.asString.length,
                                 pItem2->item.asString.value, pItem2->item.asString.length ) != 0 );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_INSTRING )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPINSTRING", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else if( hb_objGetOpOver( pItem2 ) & HB_CLASS_OP_CONTAINS )
   {
      hb_vmOperatorCall( pItem2, pItem1, "__OPCONTAINS", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else if( HB_IS_ARRAY( pItem2 ) )
   {
      BOOL bResult = ( BOOL ) hb_arrayScan( pItem2, pItem1, NULL, NULL, TRUE, FALSE );

      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( HB_IS_HASH( pItem2 ) && ( HB_IS_ORDERABLE( pItem1 ) || ( HB_IS_HASH( pItem1 ) && hb_hashLen( pItem1 ) == 1 ) ) )
   {
      HB_SIZE ulPos = 0;

      if( HB_IS_HASH( pItem1 ) ) /* length 1 by hypotesis */
      {
         if( hb_hashScan( pItem2, pItem1->item.asHash.value->pKeys, &ulPos ) )
         {
            HB_ITEM_NEW( hbV1 );
            PHB_ITEM pV2 = hb_hashGetValueAt( pItem2, ulPos );

            hb_itemCopy( &hbV1, hb_hashGetValueAt( pItem1, 1 ) );
            hb_stackPop();
            hb_stackPop();

            hb_itemCopy( hb_stackAllocItem(), &hbV1 );
            hb_itemCopy( hb_stackAllocItem(), pV2 );

            /* now in the stack we have our values. */
            hb_vmEqual( TRUE ); /* this will pop params and push result */
         }
         else
         {
            hb_stackPop();
            hb_stackPop();
            hb_vmPushLogical( FALSE );
         }
      }
      else
      {
         BOOL bResult = hb_hashScan( pItem2, pItem1, &ulPos );

         hb_stackPop();
         hb_stackPop();
         hb_vmPushLogical( bResult );
      }
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1109, NULL, "$", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* At this moment the eval stack should store:
 * -3 -> <current counter value>
 * -2 -> <end value>
 * -1 -> <step value>
 */
static void hb_vmForTest( void ) /* Test to check the end point of the FOR */
{
   HB_THREAD_STUB

   double dStep;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmForTest()" ) );

   while( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) )
   {
      PHB_ITEM pItem1   = hb_stackItemFromTop( -1 );
      PHB_ITEM pResult  = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
      else
         /* NOTE: Return from the inside. */
         return;
   }

   dStep = hb_vmPopNumber();

   while( ( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) ) && ( ! HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) ) )
   {
      PHB_ITEM pItem1   = hb_stackItemFromTop( -1 );
      PHB_ITEM pResult  = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

      if( ! pResult )
         /* NOTE: Return from the inside. */
         return;

      hb_stackPop();
      hb_itemPushForward( pResult );
      hb_itemRelease( pResult );
   }

   if( hb_stackItemFromTop( -1 )->type == HB_IT_LOGICAL )
   {
      BOOL  lEnd;
      BOOL  lCurrent;

      lEnd = hb_vmPopLogical();
      while( ! HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) )
      {
         PHB_ITEM pItem1   = hb_stackItemFromTop( -1 );
         PHB_ITEM pResult  = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

         if( ! pResult )
            /* NOTE: Return from the inside. */
            return;

         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }

      lCurrent = hb_vmPopLogical();

      if( dStep >= 0 )  /* Positive loop. Use LESS */
         hb_vmPushLogical( lCurrent <= lEnd );
      else if( dStep < 0 ) /* Negative loop. Use GREATER */
         hb_vmPushLogical( lCurrent >= lEnd );
   }
   else
   {
      double   dEnd;
      double   dCurrent;

      dEnd = hb_vmPopNumber();
      while( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) )
      {
         PHB_ITEM pItem1   = hb_stackItemFromTop( -1 );
         PHB_ITEM pResult  = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

         if( ! pResult )
            /* NOTE: Return from the inside. */
            return;

         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }

      dCurrent = hb_vmPopNumber();

      if( dStep >= 0 ) /* Positive loop. Use LESS */
         hb_vmPushLogical( dCurrent <= dEnd );
      else if( dStep < 0 ) /* Negative loop. Use GREATER */
         hb_vmPushLogical( dCurrent >= dEnd );
   }
}

/* ------------------------------- */
/* Operators (logical)             */
/* ------------------------------- */

static void hb_vmNot( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmNot()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_LOGICAL( pItem ) )
      pItem->item.asLogical.value = ! pItem->item.asLogical.value;

#ifdef HB_USE_NUMERIC_IF
   else if( HB_IS_NUMERIC( pItem ) )
   {
      if( HB_IS_INTEGER( hb_stackItemFromTop( -1 ) ) )
      {
         if( pItem->item.asInteger.value == 0 )
            pItem->item.asInteger.value = 1;
         else
            pItem->item.asInteger.value = 0;
      }
      else if( HB_IS_LONG( hb_stackItemFromTop( -1 ) ) )
      {
         if( pItem->item.asLong.value == 0 )
            pItem->item.asLong.value = 1;
         else
            pItem->item.asLong.value = 0;
      }
      else
      {
         if( pItem->item.asDouble.value == 0 )
            pItem->item.asDouble.value = 1.0;
         else
            pItem->item.asDouble.value = 0.0;
      }
   }
#endif /* HB_USE_NUMERIC_IF */

   else if( hb_objGetOpOver( pItem ) & HB_CLASS_OP_NOT )
      hb_vmOperatorCallUnary( pItem, "__OPNOT", pItem );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1077, NULL, ".NOT.", 1, pItem );

      if( pResult )
      {
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmAnd( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmAnd()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( pItem1->item.asLogical.value &&
                                        pItem2->item.asLogical.value );
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_AND )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPAND", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1078, NULL, ".AND.", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmOr( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmOr()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value  = ( pItem1->item.asLogical.value ||
                                        pItem2->item.asLogical.value );
      pItem2->type                  = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_OR )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPOR", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1079, NULL, ".OR.", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* ------------------------------- */
/* Operators (bit)                 */
/* ------------------------------- */
static void hb_vmBitAnd( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmBitAnd()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_NUMBER( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value  = hb_itemGetNInt( pItem1 ) & hb_itemGetNInt( pItem2 );
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( pItem1->item.asLong.value );
      /* Don't move up! */
      pItem1->type               = HB_IT_LONG;

      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_SIZE  ulLen1   = pItem1->item.asString.length;
      HB_SIZE  ulLen2   = pItem2->item.asString.length;

      if( ulLen1 && ulLen2 )
      {
         char *            pString1 = pItem1->item.asString.value;
         char *            pString2 = pItem2->item.asString.value;
         register HB_SIZE  ulPos1;
         register HB_SIZE  ulPos2;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString1 = ( char * ) hb_xgrab( ulLen1 + 1 );
            hb_xmemcpy( ( void * ) pString1, ( void * ) pItem1->item.asString.value, ( size_t ) ulLen1 );
            hb_itemPutCPtr( pItem1, pString1, ulLen1 );
         }

         for( ulPos1 = ulPos2 = 0; ulPos1 < ulLen1; ulPos1++ )
         {
            pString1[ ulPos1 ] &= pString2[ ulPos2 ];

            if( ++ulPos2 == ulLen2 )
               ulPos2 = 0;
         }
      }

      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      HB_SIZE ulLen = pItem1->item.asString.length;

      if( ulLen )
      {
         char     cVal     = ( char ) hb_itemGetNL( pItem2 );
         char *   pString  = pItem1->item.asString.value;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString = ( char * ) hb_xgrab( ulLen + 1 );
            hb_xmemcpy( ( void * ) pString, ( void * ) pItem1->item.asString.value, ( size_t ) ulLen + 1 );
            hb_itemPutCPtr( pItem1, pString, ulLen );
         }

         while( ulLen-- )
            pString[ ulLen ] &= cVal;
      }

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_LONG  lVal     = hb_itemGetNInt( pItem1 );
      HB_SIZE  ulLen    = pItem2->item.asString.length;
      char *   pString  = pItem2->item.asString.value;

      while( ulLen )
         lVal &= pString[ --ulLen ];

      pItem1->type               = HB_IT_LONG;
      pItem1->item.asLong.value  = lVal;
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( lVal );

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value  = hb_itemGetNInt( pItem1 ) & hb_itemGetNInt( pItem2 );
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( pItem1->item.asLong.value );
      /* Don't move up! */
      pItem1->type               = HB_IT_LONG;

      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1088, NULL, "&", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmBitOr( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmBitOr()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_NUMBER( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value  = hb_itemGetNInt( pItem1 ) | hb_itemGetNInt( pItem2 );
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( pItem1->item.asLong.value );
      /* Don't move up! */
      pItem1->type               = HB_IT_LONG;

      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_SIZE  ulLen1   = pItem1->item.asString.length;
      HB_SIZE  ulLen2   = pItem2->item.asString.length;

      if( ulLen1 && ulLen2 )
      {
         char *            pString1 = pItem1->item.asString.value;
         char *            pString2 = pItem2->item.asString.value;
         register HB_SIZE  ulPos1;
         register HB_SIZE  ulPos2;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString1 = ( char * ) hb_xgrab( ulLen1 + 1 );
            hb_xmemcpy( ( void * ) pString1, ( void * ) pItem1->item.asString.value, ( size_t ) ulLen1 );
            hb_itemPutCPtr( pItem1, pString1, ulLen1 );
         }

         for( ulPos1 = ulPos2 = 0; ulPos1 < ulLen1; ulPos1++ )
         {
            pString1[ ulPos1 ] |= pString2[ ulPos2 ];

            if( ++ulPos2 == ulLen2 )
               ulPos2 = 0;
         }
      }
      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      HB_SIZE ulLen = pItem1->item.asString.length;

      if( ulLen )
      {
         char     cVal     = ( char ) hb_itemGetNL( pItem2 );
         char *   pString  = pItem1->item.asString.value;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString = ( char * ) hb_xgrab( ulLen + 1 );
            hb_xmemcpy( ( void * ) pString, ( void * ) pItem1->item.asString.value, ( size_t ) ulLen + 1 );
            hb_itemPutCPtr( pItem1, pString, ulLen );
         }

         while( ulLen-- )
            pString[ ulLen ] |= cVal;
      }
      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_LONG  lVal     = hb_itemGetNInt( pItem1 );
      HB_SIZE  ulLen    = pItem2->item.asString.length;
      char *   pString  = pItem2->item.asString.value;

      while( ulLen )
         lVal |= pString[ --ulLen ];

      pItem1->type               = HB_IT_LONG;
      pItem1->item.asLong.value  = lVal;
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( lVal );

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value  = hb_itemGetNInt( pItem1 ) | hb_itemGetNInt( pItem2 );
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( pItem1->item.asLong.value );
      /* Don't move up! */
      pItem1->type               = HB_IT_LONG;

      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1088, NULL, "|", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmBitXor( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmBitXor()" ) );

   pItem2   = hb_stackItemFromTop( -1 );
   pItem1   = hb_stackItemFromTop( -2 );

   if( HB_IS_NUMBER( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value  = hb_itemGetNInt( pItem1 ) ^ hb_itemGetNInt( pItem2 );
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( pItem1->item.asLong.value );
      /* Don't move up! */
      pItem1->type               = HB_IT_LONG;

      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_SIZE  ulLen1   = pItem1->item.asString.length;
      HB_SIZE  ulLen2   = pItem2->item.asString.length;

      if( ulLen1 && ulLen2 )
      {
         char *            pString1 = pItem1->item.asString.value;
         char *            pString2 = pItem2->item.asString.value;
         register HB_SIZE  ulPos1;
         register HB_SIZE  ulPos2;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString1 = ( char * ) hb_xgrab( ulLen1 + 1 );
            hb_xmemcpy( ( void * ) pString1, ( void * ) pItem1->item.asString.value, ( size_t ) ulLen1 );
            hb_itemPutCPtr( pItem1, pString1, ulLen1 );
         }

         for( ulPos1 = ulPos2 = 0; ulPos1 < ulLen1; ulPos1++ )
         {
            pString1[ ulPos1 ] ^= pString2[ ulPos2 ];

            if( ++ulPos2 == ulLen2 )
               ulPos2 = 0;
         }
      }
      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      HB_SIZE ulLen = pItem1->item.asString.length;

      if( ulLen )
      {
         char     cVal     = ( char ) hb_itemGetNL( pItem2 );
         char *   pString  = pItem1->item.asString.value;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString = ( char * ) hb_xgrab( ulLen + 1 );
            hb_xmemcpy( ( void * ) pString, ( void * ) pItem1->item.asString.value, ( size_t ) ulLen + 1 );
            hb_itemPutCPtr( pItem1, pString, ulLen );
         }

         while( ulLen-- )
            pString[ ulLen ] ^= cVal;
      }
      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_LONG  lVal     = hb_itemGetNInt( pItem1 );
      HB_SIZE  ulLen    = pItem2->item.asString.length;
      char *   pString  = pItem2->item.asString.value;

      while( ulLen )
         lVal ^= pString[ --ulLen ];

      pItem1->type               = HB_IT_LONG;
      pItem1->item.asLong.value  = lVal;
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( lVal );

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value  = hb_itemGetNInt( pItem1 ) ^ hb_itemGetNInt( pItem2 );
      pItem1->item.asLong.length = ( UINT ) HB_LONG_LENGTH( pItem1->item.asLong.value );
      /* Don't move up! */
      pItem1->type               = HB_IT_LONG;

      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1088, NULL, "^^", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmBitShiftLeft( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmBitShiftLeft()" ) );

   pItem1   = hb_stackItemFromTop( -2 );
   pItem2   = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      hb_itemPutNInt( pItem1, hb_itemGetNInt( pItem1 ) << hb_itemGetNI( pItem2 ) );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1601, NULL, "<<", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmBitShiftRight( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmBitShiftRight()" ) );

   pItem1   = hb_stackItemFromTop( -2 );
   pItem2   = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      hb_itemPutNInt( pItem1, hb_itemGetNInt( pItem1 ) >> hb_itemGetNI( pItem2 ) );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1601, NULL, ">>", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* ------------------------------- */
/* Array                           */
/* ------------------------------- */

static void hb_vmArrayPush( void )
{
   HB_THREAD_STUB

   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   LONG     lIndex;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayPush()" ) );

   pIndex   = hb_stackItemFromTop( -1 );
   pArray   = hb_stackItemFromTop( -2 );

   if( HB_IS_BYREF( pArray ) )
      pArray = hb_itemUnRef( pArray );

   if( hb_objGetOpOver( pArray ) & HB_CLASS_OP_ARRAYINDEX )
   {
      hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
      return;
   }

   if( HB_IS_HASH( pArray ) && HB_IS_ORDERABLE( pIndex ) )
   {
      HB_SIZE  ulPos = 0;
      PHB_ITEM pTemp;

      if( HB_IS_NUMBER( pIndex ) && hb_hashGetCompatibility( pArray ) )
      {
         /* Associative Array compatibility */
         LONG lPos = hb_itemGetNL( pIndex );

         if( lPos < 0 )
            lPos += ( LONG ) ( 1 + hb_hashLen( pArray ) );

         ulPos = hb_hashAAGetRealPos( pArray, ( ULONG ) lPos );
         if( ulPos == 0 )
         {
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
            return;
         }
      }
      else
      {
         /* Hash compatibility */
         if( ! hb_hashScan( pArray, pIndex, &ulPos ) )
         {
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
            return;
         }
      }

      pTemp = hb_itemNew( NULL );
      hb_hashGet( pArray, ulPos, pTemp );

      hb_stackPop();
      hb_itemForwardValue( hb_stackItemFromTop( -1 ), pTemp );
      hb_itemRelease( pTemp );
      return;
   }

   if( HB_IS_INTEGER( pIndex ) )
      lIndex = ( LONG ) pIndex->item.asInteger.value;
   else if( HB_IS_LONG( pIndex ) )
      lIndex = ( LONG ) pIndex->item.asLong.value;
   else if( HB_IS_DOUBLE( pIndex ) )
      lIndex = ( LONG ) pIndex->item.asDouble.value;
#ifndef HB_C52_STRICT
   else if( HB_IS_STRING( pIndex ) && pIndex->item.asString.length == 1 )
      lIndex = ( LONG ) ( BYTE ) pIndex->item.asString.value[ 0 ];
   else if( HB_IS_STRING( pIndex ) && HB_IS_OBJECT( pArray ) &&
            strcmp( "TASSOCIATIVEARRAY", hb_objGetClsName( pArray ) ) == 0 )
   {
      hb_vmPushSymbol( hb_dynsymGetCase( pIndex->item.asString.value )->pSymbol );
      hb_itemPushForward( pArray );

      hb_vmSend( 0 );

      /* Pop pIndex. */
      hb_stackPop();

      /* Recycle pArray. */
      hb_itemForwardValue( pArray, &( HB_VM_STACK.Return ) );

      return;
   }
#endif /* HB_C52_STRICT */
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
#ifndef HB_C52_STRICT
      if( lIndex < 0 )
      {
         if( pArray->item.asArray.value->ulLen )
            lIndex += ( LONG ) ( pArray->item.asArray.value->ulLen + 1 );
         /* Empty array and -1 Index -> return NIL. */
         else if( lIndex == -1 )
         {
            hb_stackPop();
            hb_itemClear( hb_stackItemFromTop( -1 ) );
            return;
         }
      }
#endif /* HB_C52_STRICT */

      if( lIndex > 0 && ( ULONG ) lIndex <= pArray->item.asArray.value->ulLen )
      {
#ifdef HB_ARRAY_USE_COUNTER
          if( pArray->item.asArray.value->ulHolders > 1 )
#else
          if( pArray->item.asArray.value->pOwners->pNext )
#endif /* HB_ARRAY_USE_COUNTER */
          {
              /* this is a temporary copy of an array - we can overwrite
               * it with no problem
               */
              hb_arrayGet( pArray, ( ULONG ) lIndex, pArray );
              hb_stackPop();
          }
          else
          {
              /* this is a constant array { 1, 2, 3 } - we cannot use
              * the optimization here
              */
              HB_ITEM_NEW( item );

              hb_arrayGet( pArray, ( ULONG ) lIndex, &item );
              hb_stackPop();
              hb_itemForwardValue( hb_stackItemFromTop( -1 ), &item );
          }
      }
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
#ifndef HB_C52_STRICT
   else if( HB_IS_STRING( pArray ) )
   {
      if( lIndex > 0 )
         lIndex--;
      else if( lIndex < 0 )
      {
         if( pArray->item.asString.length )
            lIndex += ( LONG ) pArray->item.asString.length;
         else if( lIndex == -1 )
         {
            hb_stackPop();

            /*
             * Empty String and Index is -1, this may be result of optimizations:
             *
             *    SubStr( "", -1, 1 ) or aTail( "" ).
             *
             * The SubStr() should return "", while the aTail("") should return NIL (Invalid Argument).
             *
             * We choose the SubStr() context as the more appropriate, because the Invalid Argument context
             * is of questionable compatibilty value.
             *
               hb_itemClear( hb_stackItemFromTop( -1 ) );
             */

            return;
         }

         if( lIndex < 0 )
            lIndex = 0;
      }

      hb_stackPop();

      if( ( ULONG ) lIndex < pArray->item.asString.length )
         hb_itemPutCLStatic( pArray, hb_szAscii[ ( UCHAR ) ( pArray->item.asString.value[ lIndex ] ) ], 1 );
      else
         hb_itemPutCL( pArray, NULL, 0 );
   }
#endif /* HB_C52_STRICT */
   else
   {
      /* aTail( NIL ) in Clipper -> NIL // No Error. */
      if( HB_IS_NIL( pArray ) && lIndex == -1 )
      {
         hb_stackPop();
         hb_itemClear( hb_stackItemFromTop( -1 ) );
      }
      else
         hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
}

static void hb_vmArrayPushRef( void )
{
   HB_THREAD_STUB

   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   LONG     lIndex;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayPushRef()" ) );

   pIndex   = hb_stackItemFromTop( -1 );
   pArray   = hb_stackItemFromTop( -2 );

   if( hb_objGetOpOver( pArray ) & HB_CLASS_OP_ARRAYINDEX )
   {
      hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", NULL, 2, NULL );
      hb_itemPushForward( &( HB_VM_STACK.Return ) );
      return;
   }

   if( HB_IS_HASH( pArray ) && HB_IS_ORDERABLE( pIndex ) )
   {
      hb_vmArrayPush();
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
      if( HB_IS_INTEGER( pIndex ) )
         lIndex = ( LONG ) pIndex->item.asInteger.value;
      else if( HB_IS_LONG( pIndex ) )
         lIndex = ( LONG ) pIndex->item.asLong.value;
      else if( HB_IS_DOUBLE( pIndex ) )
         lIndex = ( LONG ) pIndex->item.asDouble.value;
      else if( HB_IS_STRING( pIndex ) && pIndex->item.asString.length == 1 )
         lIndex = ( LONG ) ( BYTE ) pIndex->item.asString.value[ 0 ];
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );

         if( pResult )
         {
            hb_stackPop();
            hb_stackPop();
            hb_itemPushForward( pResult );
            hb_itemRelease( pResult );
         }
         return;
      }

      if( lIndex < 0 )
      {
         lIndex += ( LONG ) ( pArray->item.asArray.value->ulLen + 1 );
      }

      if( lIndex > 0 && ( ULONG ) lIndex <= pArray->item.asArray.value->ulLen )
      {
#ifdef HB_ARRAY_USE_COUNTER
         if( pArray->item.asArray.value->ulHolders > 1 )
#else
         if( pArray->item.asArray.value->pOwners->pNext )
#endif /* HB_ARRAY_USE_COUNTER */
         {
            /* move the array on stack to index position to free the
               place for new reference */
            hb_itemForwardValue( pIndex, pArray );
            hb_arrayGetByRef( pIndex, ( ULONG ) lIndex, pArray );
            /* pop the source array (at index position) from stack */
            hb_stackPop();
         }
         else
            /* Literal array - can not push by ref! */
            hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
      }
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
   else
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
}

static void hb_vmArrayPop( HB_PCODE pcode )
{
   HB_THREAD_STUB

   PHB_ITEM pValue;
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   LONG     lIndex;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayPop()" ) );

   pValue   = hb_stackItemFromTop( -3 );
   pArray   = hb_stackItemFromTop( -2 );
   pIndex   = hb_stackItemFromTop( -1 );

   if( HB_IS_BYREF( pArray ) )
      pArray = hb_itemUnRef( pArray );

   if( hb_objGetOpOver( pArray ) & HB_CLASS_OP_ARRAYINDEX )
   {
      if( pcode == HB_P_PLUS )
      {
         PHB_ITEM pElement = hb_itemNew( NULL );

         hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", NULL, 0, pElement );
         hb_vmPlus( pElement, pValue, pElement );

         hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", pElement, 2, NULL );
         hb_itemPushForward( &( HB_VM_STACK.Return ) );
         hb_itemRelease( pElement );
      }
      else
      {
         hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", pValue, 2, NULL );
         hb_itemPushForward( &( HB_VM_STACK.Return ) );
      }

      return;
   } /* hb_objGetOpOver( pArray ) & HB_CLASS_OP_ARRAYINDEX */

   if( HB_IS_HASH( pArray ) && HB_IS_ORDERABLE( pIndex ) )
   {
      HB_SIZE ulPos = 0;

      if( HB_IS_NUMBER( pIndex ) && hb_hashGetCompatibility( pArray ) )
      {
         /* Compatibilidad con Associative Array */
         LONG lPos = hb_itemGetNL( pIndex );

         if( lPos < 0 )
            lPos += ( LONG ) ( 1 + hb_hashLen( pArray ) );

         ulPos = hb_hashAAGetRealPos( pArray, ( ULONG ) lPos );

         if( ulPos == 0 )
         {
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
            return;
         }
      }
      else
      {
         if( pcode == HB_P_PLUS )
         {
            if( ! hb_hashScan( pArray, pIndex, &ulPos ) )
            {
               hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
               return;
            }
         }
         else
         {
            hb_hashAdd( pArray, ULONG_MAX, pIndex, pValue );
            goto ArrayPop_Finalization;
         }
      } /* HB_IS_NUMBER( pIndex ) && hb_hashGetCompatibility( pArray ) */

      if( pcode == HB_P_PLUS )
      {
         PHB_ITEM pElem = hb_itemNew( NULL );

         hb_hashGetForward( pArray, ulPos, pElem );

         hb_vmPlus( pElem, pValue, pElem );

         hb_hashSetForward( pArray, ulPos, pElem );

         hb_itemRelease( pElem );
      }
      else
      {
         hb_hashSet( pArray, ulPos, pValue );
      }

      ArrayPop_Finalization:

      hb_stackPop();
      hb_stackPop();
      hb_stackPop();

      return;
   }  /* HB_IS_HASH( pArray ) && HB_IS_ORDERABLE( pIndex ) */

   if( HB_IS_INTEGER( pIndex ) )
      lIndex = ( LONG ) pIndex->item.asInteger.value;
   else if( HB_IS_LONG( pIndex ) )
      lIndex = ( LONG ) pIndex->item.asLong.value;
   else if( HB_IS_DOUBLE( pIndex ) )
      lIndex = ( LONG ) pIndex->item.asDouble.value;
#ifndef HB_C52_STRICT
   else if( HB_IS_STRING( pIndex ) && pIndex->item.asString.length == 1 )
      lIndex = ( LONG ) ( BYTE ) pIndex->item.asString.value[ 0 ];
   else if( HB_IS_STRING( pIndex ) && HB_IS_OBJECT( pArray ) && strcmp( "TASSOCIATIVEARRAY", hb_objGetClsName( pArray ) ) == 0 )
   {
      char szMessage[ HB_SYMBOL_NAME_LEN ];

      szMessage[ 0 ] = '_';
      szMessage[ 1 ] = '\0';
      hb_xstrcat( szMessage, pIndex->item.asString.value, 0 );

      /* Optimized - recycling the parameters. */
#if 1
      /* Swap - pIndex no longer needed. */
      hb_itemForwardValue( pIndex, pValue );

      /* Recycle pValue as Message. */
      hb_itemPutSymbol( pValue, hb_dynsymGetCase( szMessage )->pSymbol );

      if( HB_IS_BYREF( hb_stackItemFromTop( -2 ) ) )
         hb_itemCopy( hb_stackItemFromTop( -2 ), pArray );

      hb_vmSend( 1 );
#else
      hb_vmPushSymbol( hb_dynsymGetCase( szMessage )->pSymbol );
      hb_vmPush( pArray );
      hb_vmPush( pValue );

      hb_vmSend( 1 );

      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
#endif /* 1 */

      if( HB_IS_COMPLEX( &( HB_VM_STACK.Return ) ) )
         hb_itemClear( &( HB_VM_STACK.Return ) );

      return;
   }
#endif /* HB_C52_STRICT */
   else
   {
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, pIndex, pValue );
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
#ifndef HB_C52_STRICT
      if( lIndex < 0 )
         lIndex += ( LONG ) ( pArray->item.asArray.value->ulLen + 1 );
#endif /* HB_C52_STRICT */
      if( lIndex > 0 && ( ULONG ) lIndex <= pArray->item.asArray.value->ulLen )
      {
         /* Remove MEMOFLAG if exists (assignment from field). */
         pValue->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );

         if( pcode == HB_P_PLUS )
         {
            PHB_ITEM pElement = hb_itemNew( NULL );

            hb_arrayGetForward( pArray, ( ULONG ) lIndex, pElement );

            hb_vmPlus( pElement, pValue, pElement );

            hb_arraySetForward( pArray, ( ULONG ) lIndex, pElement );

            hb_itemRelease( pElement );
         }
         else
            hb_arraySetForward( pArray, ( ULONG ) lIndex, pValue );

         hb_stackPop();
         hb_stackPop();
         hb_stackPop(); /* remove the value from the stack just like other POP operations */
      }
      else
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
   }
#ifndef HB_C52_STRICT
   /* Only allowing assignment of strings (char) and numerics
    * into String as Array.
    */
   else if( HB_IS_STRING( pArray ) && ( HB_IS_STRING( pValue ) || HB_IS_NUMERIC( pValue ) ) )
   {
      if( lIndex > 0 )
         lIndex--;
      else if( lIndex < 0 )
         lIndex += ( LONG ) pArray->item.asString.length;

      if( lIndex >= 0 && ( ULONG ) lIndex < pArray->item.asString.length )
      {
         BYTE bNewChar;

         /* pArray = pArray->item.asString.pOrigin; */

         if( pValue->type & HB_IT_STRING )
            bNewChar = ( BYTE ) pValue->item.asString.value[ 0 ];
         else if( pValue->type == HB_IT_INTEGER )
            bNewChar = ( BYTE ) pValue->item.asInteger.value;
         else if( pValue->type == HB_IT_LONG )
            bNewChar = ( BYTE ) pValue->item.asLong.value;
         else
            bNewChar = ( BYTE ) pValue->item.asDouble.value;

         if( pArray->item.asString.allocated == 0 || *( pArray->item.asString.pulHolders ) > 1 )
         {
            char * sNew = ( char * ) hb_xgrab( pArray->item.asString.length + 1 );

            HB_MEMCPY( sNew, pArray->item.asString.value, ( size_t ) pArray->item.asString.length );
            sNew[ pArray->item.asString.length ] = '\0';
            hb_itemPutCPtr( pArray, sNew, pArray->item.asString.length );
         }

         if( pcode == HB_P_PLUS )
            pArray->item.asString.value[ lIndex ] += ( char ) bNewChar;
         else
            pArray->item.asString.value[ lIndex ] = ( char ) bNewChar;

         hb_stackPop();
         hb_stackPop();
         hb_stackPop(); /* remove the value from the stack just like other POP operations */
      }
      else
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, pIndex, pValue );
   }
#endif /* HB_C52_STRICT */
   else
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, pIndex, pValue );
}

static void hb_vmArrayDim( USHORT uiDimensions ) /* generates an uiDimensions Array and initialize those dimensions from the stack values */
{
   HB_THREAD_STUB

   HB_ITEM_NEW( itArray );

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayDim(%hu)", uiDimensions ) );

   hb_vmArrayNew( &itArray, uiDimensions );

   while( uiDimensions-- )
      hb_stackPop();

   hb_itemForwardValue( hb_stackAllocItem(), &itArray );
}

static void hb_vmArrayGen( ULONG ulElements ) /* generates an ulElements Array and fills it from the stack values */
{
   HB_THREAD_STUB
   PHB_ITEM       pItem;

   HB_ITEM_NEW( itArray );
   register ULONG ulPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayGen(%lu)", ulElements ) );

   hb_arrayNew( &itArray, ulElements );

   for( ulPos = 0; ulPos < ulElements; ulPos++ )
   {
      pItem       = hb_stackItemFromTop( ulPos - ulElements );

      pItem->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
      hb_itemForwardValue( itArray.item.asArray.value->pItems + ulPos, pItem );
   }

   /* Poping 1 less than element,so we can override the 1st
    * element with the new array
    */
   for( ulPos = 1; ulPos < ulElements; ulPos++ )
      hb_stackPop();

   /* Override 1st element if there was one, or push... */
   if( ulElements )
      hb_itemForwardValue( ( hb_stackItemFromTop( -1 ) ), &itArray );
   else
      hb_itemForwardValue( hb_stackAllocItem(), &itArray );
}

/* generates an ulPairs Hash and fills it from the stack values */
static void hb_vmHashGen( ULONG ulPairs )
{
   HB_THREAD_STUB

   PHB_ITEM       pHash;
   register ULONG ulPos;
   register ULONG ulItems = ulPairs * 2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayGen(%lu)", ulPairs ) );

   pHash = hb_hashNew( NULL );

   if( ulPairs )
      hb_hashPreallocate( pHash, ulPairs );

   for( ulPos = 0; ulPos < ulItems; ulPos += 2 )
   {
      if( ! hb_hashAdd( pHash, ULONG_MAX, hb_stackItemFromTop( ulPos - ulItems ), hb_stackItemFromTop( ulPos - ulItems + 1 ) ) )
      {
         hb_hashRelease( pHash );
         hb_itemRelease( pHash );
         hb_errRT_BASE( EG_BOUND, 1131, "Hash value insertion failed", hb_langDGetErrorDesc( EG_ARRDIMENSION ), 0 );
         return;
      }
   }

   /* Poping 1 less than element,so we can override the 1st element with
    * the new array
    */
   for( ulPos = 1; ulPos < ulItems; ulPos++ )
      hb_stackPop();

   /* Override 1st element if there was one, or push... */
   if( ulPairs )
      hb_itemForwardValue( ( hb_stackItemFromTop( -1 ) ), pHash );
   else
      hb_itemForwardValue( hb_stackAllocItem(), pHash );

   hb_itemRelease( pHash );
}

/* This function creates an array item using 'uiDimension' as an index
 * to retrieve the number of elements from the stack
 */
static void hb_vmArrayNew( PHB_ITEM pArray, USHORT uiDimension )
{
   HB_THREAD_STUB

   ULONG       ulElements;
   PHB_ITEM pDim;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayNew(%p, %hu)", pArray, uiDimension ) );

   pDim = hb_stackItemFromTop( -uiDimension );

   /* use the proper type of number of elements */
   switch( pDim->type )
   {
      case HB_IT_INTEGER:
         ulElements = ( ULONG ) pDim->item.asInteger.value;
         break;

      case HB_IT_LONG:
         ulElements = ( ULONG ) pDim->item.asLong.value;
         break;

      case HB_IT_DOUBLE:
         ulElements = ( ULONG ) pDim->item.asDouble.value;
         break;

      default:
         /* NOTE: Clipper creates empty array if non-numeric value is
          * specified as dimension and stops further processing.
          * There is no runtime error generated.
          */
         ulElements = 0;
         break;
   }

   /* create an array */
   hb_arrayNew( pArray, ulElements );

   if( --uiDimension )
   {
      /* call self recursively to create next dimensions
       */
      while( ulElements )
         hb_vmArrayNew( hb_arrayGetItemPtr( pArray, ulElements-- ), uiDimension );
   }
}

/* ------------------------------- */
/* Object                          */
/* ------------------------------- */

void hb_vmOperatorCall( PHB_ITEM pObjItem, PHB_ITEM pMsgItem, char * szSymbol, PHB_ITEM pArg, int iPop, PHB_ITEM pResult )
{
   /* NOTE: There is no need to test if specified symbol exists. It is checked
    * by the caller (if HB_IS_OBJECT() && HAS_METHOD() )
    */
   HB_THREAD_STUB
   ULONG ulSend = 1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmOperatorCall(%p, %p, %s)", pObjItem, pMsgItem, szSymbol ) );

   /* printf( "BEFORE %s Top: %i Type: %i\n", szSymbol, hb_stackTopOffset(), hb_stackItemFromTop( -1 )->type );
    */

   hb_vmPushSymbol( hb_dynsymFind( szSymbol )->pSymbol );

   hb_vmPush( pObjItem );   /* Push object   */
   hb_vmPush( pMsgItem );   /* Push argument */

   if( pArg )
   {
      hb_vmPush( pArg );    /* Push argument */
      ulSend = 2;
   }

   hb_vmSend( ( USHORT ) ulSend );

   /* printf( "AFTER %s Top: %i Type: %i\n", szSymbol, hb_stackTopOffset(), hb_stackItemFromTop( -1 )->type );
    */

   while( iPop-- )
      hb_stackPop();

   /* Push return value on the stack
    * NOTE: for performance reason we could have avoided pop of the second argument.
    * and recycle it with the return value, but that would be WRONG in case of Argument BYREF.
    */
   if( pResult )
      hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );
}

void hb_vmOperatorCallUnary( PHB_ITEM pObjItem, char * szSymbol, PHB_ITEM pResult )
{
   /* NOTE: There is no need to test if specified symbol exists. It is checked
    * by the caller (if HB_IS_OBJECT() && HAS_METHOD() )
    */
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmOperatorCallUnary(%p, %s)", pObjItem, szSymbol ) );

   /* printf( "%s Top: %i Type: %i\n", szSymbol, hb_stackTopOffset(), hb_stackItemFromTop( -1 )->type );
    */

   hb_vmPushSymbol( hb_dynsymFind( szSymbol )->pSymbol );
   hb_vmPush( pObjItem );                    /* Push object */
   hb_vmSend( 0 );

   /* printf( "AFTER %s Top: %i Type: %i\n", szSymbol, hb_stackTopOffset(), hb_stackItemFromTop( -1 )->type );
    */

   if( pResult )
      hb_itemForwardValue( pResult, &( HB_VM_STACK.Return ) );
}

/* ------------------------------- */
/* Database                        */
/* ------------------------------- */

static HB_ERRCODE hb_vmSelectWorkarea( PHB_ITEM pAlias, PHB_SYMB pField )
{
   HB_THREAD_STUB
   HB_ERRCODE  errCode;
   BOOL        fRepeat;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSelectWorkArea(%p,%p)", pAlias, pField ) );

   /* NOTE: Clipper doesn't generate an error if an workarea specified
    * as numeric value cannot be selected
    */
   do
   {
      fRepeat  = FALSE;
      errCode  = SUCCESS;

      switch( pAlias->type )
      {
         case HB_IT_INTEGER:
            /* Alias was used as integer value, for example: 4->field
             * or it was saved on the stack using hb_vmPushAlias()
             * or was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( pAlias->item.asInteger.value );
            break;

         case HB_IT_LONG:
            /* Alias was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( ( int ) pAlias->item.asLong.value );
            break;

         case HB_IT_DOUBLE:
            /* Alias was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( ( int ) pAlias->item.asDouble.value );
            break;

         case HB_IT_SYMBOL:
            /* Alias was specified using alias identifier, for example: al->field
             */
            errCode = hb_rddSelectWorkAreaSymbol( pAlias->item.asSymbol.value );
            break;

         case HB_IT_STRING:
         {
            /* Alias was evaluated from an expression, for example: (cVar)->field
             */
            /* expand '&' operator if exists */
            char *   szAlias;
            BOOL     bNewString = FALSE;

            szAlias = hb_macroExpandString( pAlias->item.asString.value, pAlias->item.asString.length, &bNewString );

            if( pField )
               errCode = hb_rddSelectWorkAreaAlias( szAlias );
            else
            {
               int iArea = 0;
               hb_rddGetAliasNumber( szAlias, &iArea );
               errCode = hb_rddSelectWorkAreaNumber( iArea );
            }

            if( bNewString )
               hb_xfree( szAlias );

            break;
         }

         default:
            if( pField )
            {
               PHB_ITEM pSubstVal;

               hb_vmPushString( pField->szName, strlen( pField->szName ) );

               pSubstVal = hb_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&", 2, pAlias, hb_stackItemFromTop( -1 ) );
               hb_stackPop();

               if( pSubstVal )
               {
                  hb_itemRawMove( pAlias, pSubstVal );
                  hb_itemRelease( pSubstVal );
                  fRepeat = TRUE;
               }
               else
                  errCode = HB_FAILURE;
            }
            else
               hb_rddSelectWorkAreaNumber( -1 );

            break;
      }
   }
   while( fRepeat );

   hb_itemSetNil( pAlias );

   return errCode;
}

/* Swaps two last items on the eval stack - the last item after swaping
 * is popped as current workarea number
 */
static void hb_vmSwapAlias( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;
   PHB_ITEM pWorkArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSwapAlias()" ) );

   pItem       = hb_stackItemFromTop( -1 );
   pWorkArea   = hb_stackItemFromTop( -2 );

   hb_vmSelectWorkarea( pWorkArea, NULL );

   hb_itemRawMove( pWorkArea, pItem );
   hb_stackDec();
}

/* ------------------------------- */
/* Execution                       */
/* ------------------------------- */

void hb_vmDo( USHORT uiParams )
{
   HB_THREAD_STUB

   PHB_ITEM       pItem;
   PHB_ITEM       pSelf;
   PHB_SYMB       pSym;
   PHB_FUNC       pFunc;
   HB_STACK_STATE sStackState;
   int            iPresetBase = s_iBaseLine;

#ifndef HB_NO_DEBUG
   BOOL           bDebugPrevState;
#endif /* HB_NO_DEBUG */

#ifndef HB_NO_PROFILER
   ULONG          ulClock     = 0;
   BOOL           bProfiler   = hb_bProfiler; /* because profiler state may change */
#endif /* HB_NO_PROFILER */

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDo(%hu)", uiParams ) );

   /* printf( "\VmDo nItems: %i Params: %i \n", HB_VM_STACK.pPos - HB_VM_STACK.pBase, uiParams );
    */

#ifndef HB_NO_DEBUG
   s_ulProcLevel++;
#endif /* HB_NO_DEBUG */

   if( HB_VM_STACK.iExtraParamsIndex && HB_IS_SYMBOL( pItem = hb_stackItemFromTop( -( uiParams + HB_VM_STACK.aiExtraParams[ HB_VM_STACK.iExtraParamsIndex - 1 ] + 2 ) ) ) && pItem->item.asSymbol.value == HB_VM_STACK.apExtraParamsSymbol[ HB_VM_STACK.iExtraParamsIndex - 1 ] )
      uiParams += ( USHORT ) HB_VM_STACK.aiExtraParams[ --HB_VM_STACK.iExtraParamsIndex ];

   if( hb_stackItemFromTop( -( uiParams + 1 ) )->type )
   {
      /* TraceLog( NULL, "DIVERTED hb_vmDo() to hb_vmSend()\n" );
       */
      hb_vmSend( uiParams );
      return;
   }

#ifndef HB_NO_PROFILER
   if( bProfiler )
      ulClock = ( ULONG ) clock();
#endif /* HB_NO_PROFILER */

   /* TraceLog( NULL, "StackNewFrame %hu\n", uiParams );
    */

   pItem             = hb_stackNewFrame( &sStackState, uiParams );
   pSym              = pItem->item.asSymbol.value;
   pSelf             = hb_stackSelfItem(); /* NIL, OBJECT or BLOCK */
#ifndef HB_NO_DEBUG
   bDebugPrevState   = s_bDebugging;
   s_bDebugging      = FALSE;
#endif /* HB_NO_DEBUG */

   /* TraceLog( NULL, "Symbol: '%s'\n", pSym->szName );
    */

   assert( HB_SYM_GETDYNSYM( pSym ) );

   if( HB_IS_NIL( pSelf ) ) /* are we sending a message ? */
   {
      if( ( pSym->scope.value & HB_FS_INDIRECT ) != HB_FS_INDIRECT )
         pFunc = pSym->value.pFunPtr;
      else
         pFunc = *pSym->value.pIndirectFunPtr;

      if( pFunc )
      {
#ifndef HB_NO_PROFILER
         if( bProfiler /*&& HB_SYM_GETDYNSYM(pSym)*/ )
            HB_SYM_GETDYNSYM( pSym )->ulRecurse++;
#endif /* HB_NO_PROFILER */

#ifndef HB_NO_TRACE
         if( hb_bTracePrgCalls )
            HB_TRACE( HB_TR_ALWAYS, ( "Calling: %s", pSym->szName ) );
#endif /* HB_NO_TRACE */

         HB_TRACE( HB_TR_DEBUG, ( "Calling: %s", pSym->szName ) );

         /* printf( "Doing: '%s'\n", pSym->szName ); */
         if( pSym->scope.value & HB_FS_PCODEFUNC )
            /* Running pCode dynamic function from .HRB */
            hb_vmExecute( ( ( PHB_PCODEFUNC ) pFunc )->pCode, ( ( PHB_PCODEFUNC ) pFunc )->pSymbols );
         else
            pFunc();

         /* printf( "Done: '%s'\n", pSym->szName ); */

         HB_TRACE( HB_TR_DEBUG, ( "Done: %s", pSym->szName ) );

#ifndef HB_NO_PROFILER
         if( bProfiler /*&& HB_SYM_GETDYNSYM(pSym)*/ )
         {
            HB_SYM_GETDYNSYM( pSym )->ulCalls++;           /* profiler support */

            /* Time spent has to be added only inside topmost call of a recursive function */
            if( HB_SYM_GETDYNSYM( pSym )->ulRecurse == 1 )
               HB_SYM_GETDYNSYM( pSym )->ulTime += clock() - ulClock; /* profiler support */

            HB_SYM_GETDYNSYM( pSym )->ulRecurse--;
         }
#endif /* HB_NO_PROFILER */
      } /* if( pFunc ) */
      else
         hb_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, pSym->szName, HB_ERR_ARGS_BASEPARAMS );
   }
   else
   {
      HB_TRACE( HB_TR_ERROR, ( "hb_vmDo() internal logic error, Symbol: '%s' Fun: %p, Self Type: %i", pSym->szName, pSym->value.pFunPtr, pSelf->type ) );
      hb_errInternal( HB_EI_ERRUNRECOV, "Error! hb_vmDo() internal logic failure, Symbol: '%s'.", pSym->szName, NULL );
   } /* HB_IS_NIL( pSelf ) */

   HB_TRACE( HB_TR_DEBUG, ( "DONE hb_vmDo(%hu)", uiParams ) );

#ifndef HB_NO_DEBUG
   if( s_bDebugging )
      hb_vmDebuggerEndProc();
#endif /* HB_NO_DEBUG */

   if( ( HB_VM_STACK.uiVMFlags & HB_SUSPEND_QUIT ) == 0 || ( hb_stackGetActionRequest() & HB_QUIT_REQUESTED ) == 0 )
      hb_stackOldFrame( &sStackState );

   HB_TRACE( HB_TR_DEBUG, ( "Restored OldFrame hb_vmDo(%hu)", uiParams ) );

#ifndef HB_NO_DEBUG
   s_bDebugging   = bDebugPrevState;
   s_ulProcLevel--;
#endif /* HB_NO_DEBUG */

   s_iBaseLine    = iPresetBase;
}

/* JC1: I need this error display routine to be used also by hash pseudo class
   operators, so I put it here
 */
static void hb_vmClassError( UINT uiParams, const char * szClassName, const char * szMsg, PHB_ITEM pSelf )
{
   char     sDesc[ 128 ] = { '\0' };
   PHB_ITEM pArgsArray;

   /* Should be optimized by rewriting hb_arrayFrom*()
    * to accept Pointer to use.
    */
   pArgsArray = hb_arrayFromStack( ( USHORT ) uiParams );
   hb_vmPush( pArgsArray );
   hb_itemRelease( pArgsArray );

   if( *szMsg == '_' )
   {
      /* TraceLog( NULL, "Class: '%s' has no property: '%s'\n", sClass, pSym->szName );
       */
      hb_snprintf( ( char * ) sDesc, sizeof( sDesc ), "Class: '%s' has no property", szClassName );
      if( pSelf )
         hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, ( char * ) sDesc, szMsg + 1, 1, pSelf );
      else
         hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, ( char * ) sDesc, szMsg + 1, HB_ERR_ARGS_SELFPARAMS );
   }
   else
   {
      /* TraceLog( NULL, "Class: '%s' has no method: '%s'\n", sClass, pSym->szName );
       */
      hb_snprintf( ( char * ) sDesc, sizeof( sDesc ), "Class: '%s' has no exported method", szClassName );
      if( pSelf )
         hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, ( char * ) sDesc, szMsg, 1, pSelf );
      else
         hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, ( char * ) sDesc, szMsg, HB_ERR_ARGS_SELFPARAMS );
   }
}

void hb_vmSend( USHORT uiParams )
{
   HB_THREAD_STUB

   PHB_ITEM       pItem;
   PHB_SYMB       pSym;
   PHB_ITEM       pSelf;
   PHB_FUNC       pFunc          = NULL;
   HB_STACK_STATE sStackState;
   int            iPresetBase    = s_iBaseLine;
   BOOL           bConstructor   = FALSE;
   BOOL           bSymbol        = FALSE;
#ifndef HB_NO_DEBUG
   BOOL           bDebugPrevState;
#endif /* HB_NO_DEBUG */

#ifdef HB_THREAD_SUPPORT
   USHORT         uiClass = 0;
#endif /* HB_THREAD_SUPPORT */

#ifndef HB_NO_PROFILER
   ULONG    ulClock     = 0;
   PMETHOD  pMethod     = NULL;
   BOOL     bProfiler   = hb_bProfiler; /* because profiler state may change */
#endif /* HB_NO_PROFILER */

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_vmSend(%hu)", uiParams ) );

   /* TraceLog( NULL, "From: '%s'\n", hb_stackBaseItem()->item.asSymbol.value->szName );
    */

   hb_itemSetNil( hb_stackReturnItem() );

   /* printf( "\n VmSend nItems: %i Params: %i Extra %i\n", HB_VM_STACK.pPos - HB_VM_STACK.pBase, uiParams, hb_vm_aiExtraParams[hb_vm_iExtraParamsIndex - 1] );
    */

#ifndef HB_NO_DEBUG
   s_ulProcLevel++;
#endif /* HB_NO_DEBUG */

   if( HB_VM_STACK.iExtraParamsIndex && HB_IS_SYMBOL( pItem = hb_stackItemFromTop( -( uiParams + HB_VM_STACK.aiExtraParams[ HB_VM_STACK.iExtraParamsIndex - 1 ] + 2 ) ) ) && pItem->item.asSymbol.value == HB_VM_STACK.apExtraParamsSymbol[ HB_VM_STACK.iExtraParamsIndex - 1 ] )
      uiParams += ( USHORT ) HB_VM_STACK.aiExtraParams[ --HB_VM_STACK.iExtraParamsIndex ];

#ifndef HB_NO_PROFILER
   if( bProfiler )
      ulClock = ( ULONG ) clock();
#endif /* HB_NO_PROFILER */

   pItem             = hb_stackNewFrame( &sStackState, uiParams ); /* procedure name */
   pSym              = pItem->item.asSymbol.value;
   pSelf             = ( *( HB_VM_STACK.pBase + 1 ) ); /* NIL, OBJECT or BLOCK */

#ifndef HB_NO_DEBUG
   bDebugPrevState   = s_bDebugging;
   s_bDebugging      = FALSE;
#endif /* HB_NO_DEBUG */

   if( HB_IS_BYREF( pSelf ) )
      pSelf = hb_itemUnRef( pSelf );

   if( HB_IS_BLOCK( pSelf ) && pSym == &( hb_symEval ) )
      pFunc = pSym->value.pFunPtr;        /* __EVAL method = function */
   else if( HB_IS_BLOCK( pSelf ) && strcmp( pSym->szName, "EVAL" ) == 0 )
   {
      pSym  = &hb_symEval;
      pFunc = pSym->value.pFunPtr;        /* __EVAL method = function */
   }
   else if( HB_IS_BLOCK( pSelf ) && strcmp( pSym->szName, "EXEC" ) == 0 )
   {
      pSym  = &hb_symEval;
      pFunc = pSym->value.pFunPtr;        /* __EVAL method = function */
   }
   else if( HB_IS_POINTER( pSelf ) && strcmp( pSym->szName, "EXEC" ) == 0 )
   {
      pFunc                      = ( PHB_FUNC ) pSelf->item.asPointer.value; /* Symbol of POINTER */
      bSymbol                    = TRUE;

      pItem->item.asSymbol.value = ( PHB_SYMB ) pFunc;   /* PROCNAME() */
      hb_itemSetNil( pSelf );                            /* HB_QSELF() */
   }
   else if( HB_IS_OBJECT( pSelf ) ||
            ( HB_IS_ARRAY( pSelf ) && hb_cls_uiArrayClass ) ||
            ( HB_IS_BLOCK( pSelf ) && hb_cls_uiBlockClass ) ||
            ( HB_IS_STRING( pSelf ) && hb_cls_uiCharacterClass ) ||
            ( HB_IS_DATE( pSelf ) && hb_cls_uiDateClass ) ||
            ( HB_IS_LOGICAL( pSelf ) && hb_cls_uiLogicalClass ) ||
            ( HB_IS_NIL( pSelf ) && hb_cls_uiNilClass ) ||
            ( HB_IS_NUMERIC( pSelf ) && hb_cls_uiNumericClass ) ||
            ( HB_IS_POINTER( pSelf ) && hb_cls_uiPointerClass )
            )    /* Object passed */
   {
      /* TraceLog( NULL, "Object: '%s' Message: '%s'\n", hb_objGetClsName( pSelf ), pSym->szName );
       */

      pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol );

#ifdef HB_THREAD_SUPPORT
      if( HB_VM_STACK.pMethod && ( HB_VM_STACK.pMethod->uiScope & HB_OO_CLSTP_SYNC ) != 0 )
      {
         uiClass = hb_objClassH( pSelf );
         /* Put uiClass in the SYNC methods list. */
         hb_clsPutSyncID( uiClass );
      }
#endif /* HB_THREAD_SUPPORT */

      if( uiParams == 1 && pFunc == hb___msgGetData )
         pFunc = hb___msgSetData;

      if( HB_IS_OBJECT( pSelf ) )
      {
         PHB_BASEARRAY pSelfBase = pSelf->item.asArray.value;

         if( pSelfBase->uiPrevCls ) /* Is is a Super cast ? */
         {
            HB_ITEM_NEW( RealSelf );
            USHORT uiClass;

            /* printf( "\n VmSend Method: %s \n", pSym->szName ); */
            uiClass                                   = pSelfBase->uiClass;
            pItem->item.asSymbol.pCargo->uiSuperClass = uiClass;

            /* TraceLog( NULL, "pRealSelf %p pItems %p\n", pRealSelf, pSelfBase->pItems );
             */

            /* Replace the current stacked value with real self */
            hb_itemCopy( &RealSelf, pSelfBase->pItems );
            hb_itemForwardValue( pSelf, &RealSelf );
         }
      }
   }
   else if( HB_IS_HASH( pSelf ) )
   {
      /*  Using FALSE for lAllowErrFunc because we must prefer (at this point) default messages below over OnError handler if any
       */
      if( hb_cls_uiHashClass && ( pFunc = hb_objGetMthd( pSelf, pSym, FALSE, &bConstructor, FALSE, &bSymbol ) ) != NULL )
      {
         /* goto DoFunc; */
      }
      else if( uiParams == 1 )
      {
         if( pSym->szName[ 0 ] == '_' )
         {
            hb_hashAddChar( pSelf, pSym->szName + 1, hb_stackItemFromTop( -1 ) );
            hb_itemCopy( &( HB_VM_STACK.Return ), hb_stackItemFromTop( -1 ) );
         }
         else if( hb_cls_uiHashClass && ( pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol ) ) != NULL )
         {
            /* goto DoFunc; */
         }
         else
            hb_vmClassError( uiParams, "HASH", pSym->szName, NULL );
      }
      else if( uiParams == 0 )
      {
         if( strcmp( pSym->szName, "CLASSNAME" ) == 0 )
            hb_itemPutC( &( HB_VM_STACK.Return ), "HASH" );
         else if( strcmp( pSym->szName, "CLASSH" ) == 0 )
            hb_itemPutNI( &( HB_VM_STACK.Return ), 0 );
         else if( strcmp( pSym->szName, "KEYS" ) == 0 )
            hb_hashGetKeys( &( HB_VM_STACK.Return ), pSelf );
         else if( strcmp( pSym->szName, "VALUES" ) == 0 )
            hb_hashGetValues( &( HB_VM_STACK.Return ), pSelf );
         else if( hb_cls_uiHashClass && ( pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol ) ) != NULL )
         {
            /* goto DoFunc; */
         }
         else
         {
            HB_SIZE ulPos = 0;
            HB_ITEM_NEW( hbIndex );

            hb_itemPutCRawStatic( &hbIndex, pSym->szName, strlen( pSym->szName ) );

            if( hb_hashScan( pSelf, &hbIndex, &ulPos ) )
               hb_hashGet( pSelf, ulPos, &HB_VM_STACK.Return );
            else
               hb_vmClassError( uiParams, "HASH", pSym->szName, NULL );
         }
      }
      else if( hb_cls_uiHashClass && ( pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol ) ) != NULL )
      {
         /* goto DoFunc; */
      }
      else
         hb_vmClassError( uiParams, "HASH", pSym->szName, NULL );
   }

   if( pFunc )
   {
#ifndef HB_NO_PROFILER
      if( bProfiler )
         pMethod = HB_VM_STACK.pMethod;
#endif /* HB_NO_PROFILER */

#ifndef HB_NO_TRACE
      if( hb_bTracePrgCalls )
         HB_TRACE( HB_TR_ALWAYS, ( "Calling: %s", pSym->szName ) );
#endif /* HB_NO_TRACE */

      HB_TRACE( HB_TR_DEBUG, ( "Calling: %s", pSym->szName ) );

      /* TraceLog( NULL, "Doing %s\n", pSym->szName );
       */

      if( bSymbol )
      {
         PHB_SYMB pFuncSym = ( PHB_SYMB ) pFunc;
         pFunc = pFuncSym->value.pFunPtr;

         if( pFunc )
         {
            /* Force correct context for the executing function */
            pItem->item.asSymbol.value = pFuncSym;

            if( pFuncSym->scope.value & HB_FS_CLSERROR )
               /* Mark the memory as AutoRelease because the function can
                * call to QUIT and not return, not free the memory and report
                * memory leak.
                */
               hb_xautorelease( pFuncSym );

            if( pFuncSym->scope.value & HB_FS_PCODEFUNC )
               /* Running pCode dynamic function from .HRB */
               hb_vmExecute( ( ( PHB_PCODEFUNC ) pFunc )->pCode, ( ( PHB_PCODEFUNC ) pFunc )->pSymbols );
            else
               pFunc();

            if( pFuncSym->scope.value & HB_FS_CLSERROR )
            {
               hb_xfree( pFuncSym );

               /* NEEDed because Destructor will inspect the symbol value, which was just FREEd */
               pItem->item.asSymbol.value = pSym;
            }
         }
         else
         {
            const char * sClass = hb_objGetClsName( pSelf );
            /* TraceLog( NULL, "METHOD NOT FOUND!\n" );
             */
            hb_vmClassError( uiParams, sClass, pSym->szName, NULL );
         }
      }
      else
         pFunc();

      /* TraceLog( NULL, "Done\n" ); */

      HB_TRACE( HB_TR_DEBUG, ( "Done: %s", pSym->szName ) );

#ifndef HB_NO_PROFILER
      if( bProfiler )
         hb_mthAddTime( pMethod, clock() - ulClock );
#endif /* HB_NO_PROFILER */

#ifdef HB_THREAD_SUPPORT
      if( uiClass )
         /* Delete uiClass from SYNC methods list. */
         hb_clsDelSyncID( uiClass );
#endif /* HB_THREAD_SUPPORT */

      /* Constructor must ALWAYS return Self. */
      if( bConstructor )
      {
         /* printf( "OOPS! Constructor!\n" ); */
         /* hb_itemForwardValue( &(HB_VM_STACK.Return ), pSelf ); */

         if( hb_stackGetActionRequest() == 0 )
         {
            if( ( ! HB_IS_OBJECT( &( HB_VM_STACK.Return ) ) ) || pSelf->item.asArray.value != HB_VM_STACK.Return.item.asArray.value )
            {
               PHB_ITEM pResult;

               hb_vmPush( &( HB_VM_STACK.Return ) );
               pResult = hb_errRT_BASE_Subst( EG_BADSELF, 1605, NULL, pSym->szName, 2, pSelf, hb_stackItemFromTop( -1 ) );
               hb_stackPop();

               if( pResult )
                  hb_itemRelease( hb_itemReturnForward( pResult ) );
               else
                  hb_itemForwardValue( &( HB_VM_STACK.Return ), pSelf );
            }
         }
      }
   }
   else if( ! HB_IS_HASH( pSelf ) )
   {
      if( strncmp( pSym->szName, "CLASSNAME", strlen( pSym->szName ) < 4 ? 4 : strlen( pSym->szName ) ) == 0 )
      {
         const char * sClass = hb_objGetClsName( pSelf );
         hb_itemPutC( &( HB_VM_STACK.Return ), sClass );
      }
      else if( strncmp( pSym->szName, "CLASSH", 6 ) == 0 )
         hb_itemPutNI( &( HB_VM_STACK.Return ), 0 );
      else
      {
         const char * sClass = hb_objGetClsName( pSelf );
         /* TraceLog( NULL, "METHOD NOT FOUND!\n" );
          */
         hb_vmClassError( uiParams, sClass, pSym->szName, NULL );
      }
   }

   HB_TRACE( HB_TR_DEBUG, ( "Done hb_vmSend()" ) );

#ifndef HB_NO_DEBUG
   if( s_bDebugging )
      hb_vmDebuggerEndProc();
   s_bDebugging = bDebugPrevState;
   s_ulProcLevel--;
#endif /* HB_NO_DEBUG */

   if( ( HB_VM_STACK.uiVMFlags & HB_SUSPEND_QUIT ) == 0 || ( hb_stackGetActionRequest() & HB_QUIT_REQUESTED ) == 0 )
      hb_stackOldFrame( &sStackState );

   HB_TRACE( HB_TR_DEBUG, ( "Restored Stack hb_vmSend()" ) );

   s_iBaseLine = iPresetBase;
}

static HARBOUR hb_vmDoBlock( void )
{
   HB_THREAD_STUB

   PHB_ITEM pBaseSym = ( *HB_VM_STACK.pBase );
   PHB_ITEM pBlock;
   PSYMBOLS pModuleSymbols; /* = NULL; */
   int      iParam;
   long     lStatics;
   USHORT   uiLine;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDoBlock()" ) );

   pBlock = hb_stackSelfItem();

   if( HB_IS_BYREF( pBlock ) )
      pBlock = hb_itemUnRef( pBlock );

   if( ! HB_IS_BLOCK( pBlock ) )
      hb_errInternal( HB_EI_VMNOTCBLOCK, NULL, "hb_vmDoBlock()", NULL );

   /* Check for valid count of parameters */
   iParam = pBlock->item.asBlock.paramcnt - hb_pcount();

   /* add missing parameters */
   while( iParam-- > 0 )
      hb_vmPushNil();

   /* Save the current line number. */
   uiLine = pBaseSym->item.asSymbol.pCargo->lineno;

   /* Change the line number to line where block was defined. */
   pBaseSym->item.asSymbol.pCargo->lineno = pBlock->item.asBlock.value->lineno;

   /* Set symbol context of codeblock execution to where it was defined */
   pBaseSym->item.asSymbol.value          = pBlock->item.asBlock.value->symbol;

   /* Save current Statics Context. */
   lStatics                               = HB_VM_STACK.lStatics;

   /* Change Statics context to that of the module where the Block was defined. */
   HB_VM_STACK.lStatics                   = pBlock->item.asBlock.statics;

   /* if( ! ( pBlock->item.asBlock.value->uiFlags & CBF_PRIVATE_VARS ) ) */
   {
      pModuleSymbols = HB_SYM_GETMODULESYM( pBaseSym->item.asSymbol.value );
   }
   /* TraceLog( NULL, "Set Module: %s for Block: %s(%i)\n", pModuleSymbols ? pModuleSymbols->szModuleName : "", pBlock->item.asBlock.value->symbol->szName, pBlock->item.asBlock.value->lineno );
    */

   hb_vmExecute( pBlock->item.asBlock.value->pCode, pModuleSymbols ? pModuleSymbols->pSymbolTable : NULL );

   /* Restore Statics context. */
   HB_VM_STACK.lStatics = lStatics;

   HB_TRACE( HB_TR_DEBUG, ( "Done hb_vmDoBlock()" ) );

   if( ( pBlock->item.asBlock.value->uiFlags & CBF_PRIVATE_VARS ) )
      /* Set last PrivateBase to avoid delete memvars created in codeblock */
      pBaseSym->item.asSymbol.pCargo->privatesbase = hb_memvarGetPrivatesBase();

   /* Restore line numer. */
   pBaseSym->item.asSymbol.pCargo->lineno = uiLine;

   HB_TRACE( HB_TR_DEBUG, ( "Restored Satck hb_vmDoBlock()" ) );
}

/* Evaluates a passed codeblock item with no arguments passed to a codeblock
 */
PHB_ITEM hb_vmEvalBlock( PHB_ITEM pBlock )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEvalBlock(%p)", pBlock ) );

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pBlock );
   hb_vmSend( 0 );

   return &( HB_VM_STACK.Return );
}

/* Evaluates a codeblock item using passed additional arguments
 * pBlock = an item of codeblock type to evaluate
 * ulArgCount = number of arguments passed to a codeblock
 * ... = the list of arguments of type PHB_ITEM
 *
 **for example:
 * retVal = hb_vmEvalBlockV( pBlock, 2, pParam1, pParam2 );
 */
PHB_ITEM hb_vmEvalBlockV( PHB_ITEM pBlock, HB_SIZE ulArgCount, ... )
{
   HB_THREAD_STUB
   va_list        va;
   register ULONG i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEvalBlockV(%p, %hu, ...)", pBlock, ulArgCount ) );

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pBlock );

   va_start( va, ulArgCount );
   for( i = 1; i <= ulArgCount; i++ )
      hb_vmPush( va_arg( va, PHB_ITEM ) );
   va_end( va );

   /* take care here, possible loss of data LONG to SHORT ... */
   /* added an explicit casting here for VC++ JFL */
   hb_vmSend( ( USHORT ) ulArgCount );

   return &( HB_VM_STACK.Return );
}

/* Evaluates a passed codeblock item or macro pointer item
 */
PHB_ITEM hb_vmEvalBlockOrMacro( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEvalBlockOrMacro(%p)", pItem ) );

   if( pItem->type == HB_IT_BLOCK )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pItem );
      hb_vmSend( 0 );
   }
   else
   {
      PHB_MACRO pMacro = ( PHB_MACRO ) hb_itemGetPtr( pItem );
      if( pMacro )
      {
         hb_macroRun( pMacro );
         hb_itemForwardValue( hb_stackReturnItem(), hb_stackItemFromTop( -1 ) );
         hb_stackPop();
      }
      else
         hb_itemClear( hb_stackReturnItem() );
   }
   return hb_stackReturnItem();
}

/*
 * destroy codeblock or macro in given item
 */
void hb_vmDestroyBlockOrMacro( PHB_ITEM pItem )
{
   if( pItem->type == HB_IT_POINTER )
   {
      PHB_MACRO pMacro = ( PHB_MACRO ) hb_itemGetPtr( pItem );
      if( pMacro )
         hb_macroDelete( pMacro );
   }
   hb_itemRelease( pItem );
}

void hb_vmFunction( USHORT uiParams )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFunction(%hu)", uiParams ) );

   if( hb_stackItemFromTop( -( uiParams + 1 ) )->type )
      hb_vmSend( uiParams );
   else
   {
      hb_itemSetNil( hb_stackReturnItem() );
      hb_vmDo( uiParams );
   }

   /*
    ***
    * TODO: This should be changed to hb_itemForwardValue()
    * This is here to protect against ill behaved FWH code, which uses HB_VM_STACK.Return
    * after calling vmFunction().
    ***
    */
   hb_itemCopy( hb_stackAllocItem(), &( HB_VM_STACK.Return ) );
}

#ifndef HB_NO_DEBUG
static void hb_vmDebugEntry( int nMode, int nLine, char * szName, int nIndex, int nFrame )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDebugEntry" ) );

   HB_SYMBOL_UNUSED( nFrame );

   switch( nMode )
   {
      case HB_DBG_MODULENAME:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_MODULENAME );
         hb_vmPushString( szName, strlen( szName ) );
         hb_vmDo( 2 );
         break;

      case HB_DBG_LOCALNAME:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_LOCALNAME );
         hb_vmPushInteger( nIndex );
         hb_vmPushString( szName, strlen( szName ) );
         hb_vmDo( 3 );
         break;

      case HB_DBG_STATICNAME:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_STATICNAME );
         hb_vmPushSize( HB_VM_STACK.lStatics ); /* current static frame */
         hb_vmPushInteger( nIndex );            /* variable index */
         hb_vmPushString( szName, strlen( szName ) );
         hb_vmDo( 4 );
         break;

      case HB_DBG_ENDPROC:
         hb_stackPushReturn(); /* saves the previous returned value */
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_ENDPROC );
         hb_vmDo( 1 );

         hb_stackPopReturn(); /* restores the previous returned value */
         break;

      case HB_DBG_SHOWLINE:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_SHOWLINE );
         hb_vmPushInteger( nLine );
         hb_vmDo( 2 );
         break;

      case HB_DBG_GETENTRY:
         /* Try to get C dbgEntry() function pointer */
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_GETENTRY );
         hb_vmDo( 1 );
         break;

      case HB_DBG_VMQUIT:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_VMQUIT );
         hb_vmDo( 1 );
         break;
   }
}

static void hb_vmDummyDebugEntry( int nMode, int nLine, char * szName, int nIndex, int nFrame )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDummyDebugEntry" ) );

   HB_SYMBOL_UNUSED( nMode );
   HB_SYMBOL_UNUSED( nLine );
   HB_SYMBOL_UNUSED( szName );
   HB_SYMBOL_UNUSED( nIndex );
   HB_SYMBOL_UNUSED( nFrame );
}

static void hb_vmDebuggerExit( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDebuggerExit" ) );

   /* is debugger linked ? */
   if( s_pFunDbgEntry )
   {
      s_bDebugging = FALSE;
      /* inform debugger that we are quitting now */
      s_pFunDbgEntry( HB_DBG_VMQUIT, 0, NULL, 0, 0 );
      /* set dummy debugger function to avoid debugger activation in .prg
       *       destructors if any */
      s_pFunDbgEntry = hb_vmDummyDebugEntry;
   }
}

static void hb_vmLocalName( USHORT uiLocal, char * szLocalName ) /* locals and parameters index and name information for the debugger */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmLocalName(%hu, %s)", uiLocal, szLocalName ) );

   if( s_bDebugging )
   {
      s_bDebugging   = FALSE;
      s_pFunDbgEntry( HB_DBG_LOCALNAME, 0, szLocalName, uiLocal, 0 );
      s_bDebugging   = TRUE;
   }
}

static void hb_vmStaticName( USHORT uiStatic, char * szStaticName ) /* statics vars information for the debugger */
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmStaticName(%hu, %s)", uiStatic, szStaticName ) );

   if( s_bDebugging )
   {
      s_bDebugging   = FALSE;
      s_pFunDbgEntry( HB_DBG_STATICNAME, 0, szStaticName, uiStatic, HB_VM_STACK.lStatics );
      s_bDebugging   = TRUE;
   }
}

static void hb_vmModuleName( char * szModuleName ) /* PRG and function name information for the debugger */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmModuleName(%s)", szModuleName ) );

   if( s_pFunDbgEntry )
   {
      s_bDebugging   = FALSE;
      s_pFunDbgEntry( HB_DBG_MODULENAME, 0, szModuleName, 0, 0 );
      s_bDebugging   = TRUE;
   }
}

static void hb_vmDebuggerEndProc( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDebuggerEndProc()" ) );

   s_bDebugging = FALSE;
   s_pFunDbgEntry( HB_DBG_ENDPROC, 0, NULL, 0, 0 );
}

static void hb_vmDebuggerShowLine( USHORT uiLine ) /* makes the debugger shows a specific source code line */
{
   BOOL bDebugging = s_bDebugging;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDebuggerShowLine(%hu)", uiLine ) );

   s_bDebugging   = FALSE;
   s_pFunDbgEntry( HB_DBG_SHOWLINE, uiLine, NULL, 0, 0 );
   s_bDebugging   = bDebugging;
}
#endif /* HB_NO_DEBUG */

static void hb_vmFrame( unsigned short iLocals, BYTE bParams )
{
   HB_THREAD_STUB

   PHB_ITEM pBase       = *HB_VM_STACK.pBase;
   int      iArguments  = pBase->item.asSymbol.pCargo->arguments;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFrame(%d, %d)", iLocals, ( int ) bParams ) );

   /* #define DEBUG_FRAME */
   /* #define DEBUG_DIVERT_FRAME */

   /* NORMAL CALL */
   if( pBase->item.asSymbol.pCargo->params == 0 )
   {
      /* Explicit Parameters */
      if( bParams != HB_VAR_PARAM_FLAG )
      {
         int   iExtraArguments   = ( iArguments > bParams ) ? iArguments - bParams : 0;
         int   iAllocateLocals   = ( iLocals > iExtraArguments ) ? iLocals - iExtraArguments : 0;

#ifdef DEBUG_FRAME
         TraceLog( NULL, "SET FRAME sym: %s, Arguments: %i, Params: %i, Locals %i, Stack: %i ", pBase->item.asSymbol.value->szName, iArguments, bParams, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
#endif /* DEBUG_FRAME */

         if( iExtraArguments )
         {
            int   iPopArguments  = ( iExtraArguments > iLocals ) ? iExtraArguments - iLocals : 0;
            int   iClearLocals   = ( iLocals - iAllocateLocals );

#ifdef DEBUG_FRAME
            TraceLog( NULL, "Pop: %i, Clear Locals %i", iPopArguments, iClearLocals );
#endif /* DEBUG_FRAME */

            while( iPopArguments > 0 )
            {
               /* TraceLog( NULL, "FRAME POP ARG sym: %s, Extra: %i, Arguments: %i, Locals %i, Stack: %i\n", pBase->item.asSymbol.value->szName, iOverShort, iArguments, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
                */
               hb_stackPop();
               iPopArguments--;
            }

            while( iClearLocals > 0 )
            {
               PHB_ITEM pLocal = hb_stackItemFromBase( bParams + iClearLocals );

               /* TraceLog( NULL, "FRAME CLEAR LOCAL sym: %s, Local: %i, Arguments: %i, Locals %i, Stack: %i\n", pBase->item.asSymbol.value->szName, iClear, iArguments, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
                */
               hb_itemClear( pLocal );
               iClearLocals--;
            }
         }
         else
         {
            int iAllocateParams = ( bParams > iArguments ) ? bParams - iArguments : 0;

#ifdef DEBUG_FRAME
            TraceLog( NULL, "Alloc Params: %i ", iAllocateParams );
#endif /* DEBUG_FRAME */

            while( iAllocateParams > 0 )
            {
               /* TraceLog( NULL, "FRAME ALLOCATe PARAM sym: %s, Local: %i, Arguments: %i, Locals %i, Stack: %i\n", pBase->item.asSymbol.value->szName, -iOverShort, iArguments, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
                */
               hb_vmPushNil();
               iAllocateParams--;
            }
         }

#ifdef DEBUG_FRAME
         TraceLog( NULL, "Alloc Locals: %i\n", iAllocateLocals );
#endif /* DEBUG_FRAME */

         while( iAllocateLocals > 0 )
         {
            /* TraceLog( NULL, "FRAME ALLOCATE LOCAL sym: %s, Local: %i, Arguments: %i, Locals %i, Stack: %i\n", pBase->item.asSymbol.value->szName, -iOverShort, iArguments, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
             */
            hb_vmPushNil();
            iAllocateLocals--;
         }
      }
      /* VARPARAMS (...) */
      else
      {
         if( iLocals )
         {
            int iAllocateLocals = iLocals;

#ifdef DEBUG_FRAME
            TraceLog( NULL, "ALLOCATE LOCALS VARPARAMS: %s %i of: %i\n", pBase->item.asSymbol.value->szName, iAllocateLocals, iLocals );
#endif /* DEBUG_FRAME */

            /* Make space for the Locals. */
            while( iAllocateLocals > 0 )
            {
               /* TraceLog( NULL, "PUSH FRAME sym: %s, Local: %i, Arguments: %i, Locals %i, Stack: %i\n", pBase->item.asSymbol.value->szName, -iOverShort, iArguments, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
                */
               hb_vmPushNil();
               iAllocateLocals--;
            }

            if( iArguments )
            {
               PHB_ITEM * pStackArguments = ( PHB_ITEM * ) hb_xgrab( iArguments * sizeof( PHB_ITEM ) );

#ifdef DEBUG_FRAME
               TraceLog( NULL, "SWAP VARPARAMS: %s Arguments: %i  Locals: %i\n", pBase->item.asSymbol.value->szName, iArguments, iLocals );
#endif /* DEBUG_FRAME */

               /* Store copy of the arguments pointers. */
               HB_MEMCPY( ( void * ) pStackArguments, ( void * ) ( HB_VM_STACK.pBase + 2 ), iArguments * sizeof( PHB_ITEM ) );

               /* Move the Locals Pointers into bottom of the frame. */
               memmove( ( void * ) ( HB_VM_STACK.pBase + 2 ), ( void * ) ( HB_VM_STACK.pBase + 2 + iArguments ), iLocals * sizeof( PHB_ITEM ) );

               /* Restore the Arguments Pointers on top of the Locals. */
               HB_MEMCPY( ( void * ) ( HB_VM_STACK.pBase + 2 + iLocals ), ( void * ) pStackArguments, iArguments * sizeof( PHB_ITEM ) );

               /* Free the copy */
               hb_xfree( ( void * ) pStackArguments );
            }
         }
         else
         {
            /* VARPARAMS with NO locals, nothing to do! */
         }
      }
   }
   /* DIVERT frame! */
   else
      hb_vmDivertFrame( iLocals, bParams );

   pBase->item.asSymbol.pCargo->locals = iLocals;
   pBase->item.asSymbol.pCargo->params = bParams;
}

static void hb_vmDivertFrame( unsigned short iLocals, BYTE bParams )
{
   HB_THREAD_STUB

   PHB_ITEM pBase       = *HB_VM_STACK.pBase;
   int      iArguments  = pBase->item.asSymbol.pCargo->arguments;

   /* Explicit Parameters */
   if( bParams != HB_VAR_PARAM_FLAG )
   {
      if( pBase->item.asSymbol.pCargo->params == HB_VAR_PARAM_NOERR )
      {
         /* Developer's responsible for this time bomb! */
         pBase->item.asSymbol.pCargo->locals = iLocals;
         pBase->item.asSymbol.pCargo->params = bParams;

         return;
      }

      if( pBase->item.asSymbol.pCargo->params == HB_VAR_PARAM_NONE )
         pBase->item.asSymbol.pCargo->params = 0;

      /*
       * DIVERT of parent has declared parameters. DIVERT worker
       * must have same signature or access to parent locals will fail!
       */
      if( iLocals && pBase->item.asSymbol.pCargo->locals && ( pBase->item.asSymbol.pCargo->params != HB_VAR_PARAM_FLAG ) && ( bParams != pBase->item.asSymbol.pCargo->params ) )
      {
#ifdef DEBUG_DIVERT_FRAME
         TraceLog( NULL, "DIVERT FRAME SIGNATURE MISMATCH sym: %s, %i, %i\n", pBase->item.asSymbol.value->szName, bParams, pBase->item.asSymbol.pCargo->params );
#endif /* DEBUG_DIVERT_FRAME */

         hb_errRT_BASE( EG_ARG, 9103, "DIVERT parameters declaration mismatch", pBase->item.asSymbol.value->szName, 0 );
      }
      else
      {
         int   iParentFrame   = ( ( pBase->item.asSymbol.pCargo->params == HB_VAR_PARAM_FLAG ) ? iArguments : pBase->item.asSymbol.pCargo->params ) + pBase->item.asSymbol.pCargo->locals;
         int   iAllocate      = ( bParams + iLocals - iParentFrame );
         int   iClearParams   = ( bParams - iArguments );

#ifdef DEBUG_DIVERT_FRAME
         TraceLog( NULL, "DIVERT SET FRAME sym: %s, Frame: %i, Arguments: %i, Params: %i, Locals %i, Stack: %i", pBase->item.asSymbol.value->szName, iParentFrame, iArguments, bParams, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
         TraceLog( NULL, " Allocate: %i, Clear Params: %i\n", iAllocate, iClearParams );
#endif /* DEBUG_DIVERT_FRAME */

         while( iAllocate > 0 )
         {
            /* TraceLog( NULL, "DIVERT FRAME ALLOCATE LOCAL sym: %s, Local: %i, Arguments: %i, Locals %i, Stack: %i\n", pBase->item.asSymbol.value->szName, -iOverShort, iArguments, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
             */
            hb_vmPushNil();
            iAllocate--;
         }

         while( iClearParams > 0 )
         {
            PHB_ITEM pParam = hb_stackItemFromBase( iArguments + iClearParams );

            hb_itemClear( pParam );
            /* TraceLog( NULL, "DIVERT FRAME CLEAR PARAM sym: %s, Local: %i, Arguments: %i, Locals %i, Stack: %i\n", pBase->item.asSymbol.value->szName, iClear, iArguments, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
             */
            iClearParams--;
         }
      }
   }
   /* VARPARAMS (...) */
   else
   {
      if( iLocals )
      {
         int   iParentFrame      = ( ( pBase->item.asSymbol.pCargo->params == HB_VAR_PARAM_FLAG ) ? iArguments : pBase->item.asSymbol.pCargo->params ) + pBase->item.asSymbol.pCargo->locals;
         int   iAllocateLocals   = iArguments + iLocals - iParentFrame;

#ifdef DEBUG_DIVERT_FRAME
         TraceLog( NULL, "DIVERT FRAME VARPARAMS: '%s' Allocate: %i of: %i \n", pBase->item.asSymbol.value->szName, iAllocateLocals, iLocals );
#endif /* DEBUG_DIVERT_FRAME */

         /* Make space for the Locals. */
         while( iAllocateLocals > 0 )
         {
            /* TraceLog( NULL, "DIVERT FRAME PUSH sym: %s, Local: %i, Arguments: %i, Locals %i, Stack: %i\n", pBase->item.asSymbol.value->szName, -iOverShort, iArguments, iLocals, hb_stackTopOffset() - hb_stackBaseOffset() );
             */
            hb_vmPushNil();
            iAllocateLocals--;
         }

         /*
          * Variable arguments function is the exception, so it's more efficent
          * to rearrange the stack [once] for this exception, rather then force
          * every local and param read/write access to be calculating possible
          * offset!
          */
         if( iArguments )
         {
            PHB_ITEM * pStackArguments = ( PHB_ITEM * ) hb_xgrab( iArguments * sizeof( PHB_ITEM ) );

#ifdef DEBUG_DIVERT_FRAME
            TraceLog( NULL, "DIVERT FRAME VARPARAMS: '%s' Swap Arguments: %i  Locals: %i\n", pBase->item.asSymbol.value->szName, iArguments, iLocals );
#endif /* DEBUG_DIVERT_FRAME */

            /* Store copy of the arguments pointers. */
            HB_MEMCPY( ( void * ) pStackArguments, ( void * ) ( HB_VM_STACK.pBase + 2 ), iArguments * sizeof( PHB_ITEM ) );

            /* Move the Locals Pointers into bottom of the frame. */
            memmove( ( void * ) ( HB_VM_STACK.pBase + 2 ), ( void * ) ( HB_VM_STACK.pBase + 2 + iArguments ), iLocals * sizeof( PHB_ITEM ) );

            /* Restore the Arguments Pointers on top of the Locals. */
            HB_MEMCPY( ( void * ) ( HB_VM_STACK.pBase + 2 + iLocals ), ( void * ) pStackArguments, iArguments * sizeof( PHB_ITEM ) );

            /* Free the copy */
            hb_xfree( ( void * ) pStackArguments );
         }
      }
      else
      {
         /* VARPARAMS with NO locals, nothing to do! */
      }
   }
}

static void hb_vmSetDivert( BOOL bDivertOf )
{
   HB_THREAD_STUB

   int      iFlags   = ( int ) hb_itemGetNI( hb_stackItemFromTop( -1 ) );
   PHB_SYMB pSym     = ( PHB_SYMB ) hb_itemGetPtr( hb_stackItemFromTop( -2 ) );

   /* Flags */
   hb_stackPop();
   /* Symbol */
   hb_stackPop();

   if( pSym )
   {
      PHB_ITEM pBase       = *HB_VM_STACK.pBase;
      PHB_SYMB preset_pSym = pBase->item.asSymbol.value;
      int      iBaseLine   = s_iBaseLine;

      USHORT   params      = pBase->item.asSymbol.pCargo->params;
      USHORT   arguments   = pBase->item.asSymbol.pCargo->arguments;
      USHORT   locals      = pBase->item.asSymbol.pCargo->locals;
      USHORT   lineno      = pBase->item.asSymbol.pCargo->lineno;

      PHB_FUNC pFunc       = pSym->value.pFunPtr;
      PHB_ITEM pSelf       = hb_stackSelfItem();
      PHB_ITEM pDivertSelf;

      if( bDivertOf )
      {
         pDivertSelf = hb_itemNew( hb_stackItemFromTop( -1 ) );

         /* OF Object */
         hb_stackPop();

         hb_itemSwap( pSelf, pDivertSelf );
      }
      else
         pDivertSelf = NULL;

/* #define DEBUG_DIVERT */
#ifdef DEBUG_DIVERT
         TraceLog( NULL, "DIVERT: '%s' TO: '%s' With: %p Params: %i Arguments: %i  Locals Offseted: %i\n", preset_pSym->szName, pSym->szName, pSelf->type, params, arguments, locals );
#endif /* DEBUG_DIVERT */

      /* VARPARAMS(...) - UNSWAP to restore original stack. */
      if( params == HB_VAR_PARAM_FLAG && locals && arguments )
      {
         int         iLocals           = locals, iArguments = arguments;
         PHB_ITEM *  pStackArguments   = ( PHB_ITEM * ) hb_xgrab( iArguments * sizeof( PHB_ITEM ) );

#ifdef DEBUG_DIVERT
         TraceLog( NULL, "DIVERT Parent: '%s' UnSwap Arguments: %i  Locals: %i\n", preset_pSym->szName, iArguments, iLocals );
#endif /* DEBUG_DIVERT */

         /* Store copy of the Arguments pointers. */
         HB_MEMCPY( ( void * ) pStackArguments, ( void * ) ( HB_VM_STACK.pBase + 2 + iLocals ), iArguments * sizeof( PHB_ITEM ) );

         /* Move the Locals Pointers up beyond the Arguments space. */
         memmove( ( void * ) ( HB_VM_STACK.pBase + 2 + iArguments ), ( void * ) ( HB_VM_STACK.pBase + 2 ), iLocals * sizeof( PHB_ITEM ) );

         /* Restore the Arguments Pointers to BASE of the Frame. */
         HB_MEMCPY( ( void * ) ( HB_VM_STACK.pBase + 2 ), ( void * ) pStackArguments, iArguments * sizeof( PHB_ITEM ) );

         /* Free the copy */
         hb_xfree( ( void * ) pStackArguments );
      }

      if( ( iFlags & DIVERT_RESET_LOCALS ) == DIVERT_RESET_LOCALS )
      {
         int   iClearLocals   = locals;
         int   iOffset        = ( ( params == HB_VAR_PARAM_FLAG ) ? arguments : params );

#ifdef DEBUG_DIVERT
         TraceLog( NULL, "DIVERT Parent: '%s' Reset: %i, Arguments: %i, Params: %i Locals: %i Offset: %i\n", preset_pSym->szName, iClearLocals, arguments, params, locals, iOffset );
#endif /* DEBUG_DIVERT */
         while( iClearLocals > 0 )
         {
            PHB_ITEM pLocal = hb_stackItemFromBase( iOffset + iClearLocals );

            /* TraceLog( NULL, "DIVERT Parent: '%s' Clear Local %i Pos: %i, Type: %p\n", preset_pSym->szName, iClearLocals, iOffset + iClearLocals, pLocal->type );
            */

            hb_itemClear( pLocal );
            iClearLocals--;
         }
      } /* iFlags & DIVERT_RESET_LOCALS ) == DIVERT_RESET_LOCALS */

      /* Divert! */
      pBase->item.asSymbol.value          = pSym;
      pBase->item.asSymbol.pCargo->lineno = 0;

      /* Signify the DIVERT for hb_vmFrame()! */
      if( iFlags & DIVERT_IGNORE_SIGNATURE )
      {
         /* hb_vmFrame() will not enforce signature match but will detect a DIVERT FRAME! */
         pBase->item.asSymbol.pCargo->params = HB_VAR_PARAM_NOERR;
      }
      else
      {
         if( params == 0 )
            pBase->item.asSymbol.pCargo->params = HB_VAR_PARAM_NONE;
      } /* iFlags & DIVERT_IGNORE_SIGNATURE */

      if( pSelf->type == HB_IT_NIL )
      {
         if( pSym->scope.value & HB_FS_PCODEFUNC )
            hb_vmExecute( ( ( PHB_PCODEFUNC ) pFunc )->pCode, ( ( PHB_PCODEFUNC ) pFunc )->pSymbols );
         else
            pFunc();
      }
      else
      {
         BOOL           bConstructor   = FALSE, bSymbol = FALSE;
         PHB_BASEARRAY  pSelfBase      = pSelf->item.asArray.value;

         pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol );

         if( pSelfBase->uiPrevCls ) /* Is is a Super cast ? */
         {
            HB_ITEM_NEW( RealSelf );
            USHORT uiClass = pSelfBase->uiClass;

            pBase->item.asSymbol.pCargo->uiSuperClass = uiClass;

            /* Replace the current stacked value with real self */
            hb_itemCopy( &RealSelf, pSelfBase->pItems );
            hb_itemForwardValue( pSelf, &RealSelf );
         }

         if( bSymbol )
         {
            PHB_SYMB pFuncSym = ( PHB_SYMB ) pFunc;
            pFunc = pFuncSym->value.pFunPtr;

            if( pFunc )
            {
               /* Force correct context for the executing function */
               pBase->item.asSymbol.value = pFuncSym;

               if( pFuncSym->scope.value & HB_FS_CLSERROR )
               {
                  /* Mark the memory as AutoRelease because the function can
                   * call to QUIT and not return, not free the memory and report
                   * memory leak.
                   */
                  hb_xautorelease( pFuncSym );
               }

               if( pFuncSym->scope.value & HB_FS_PCODEFUNC )
               {
                  /* Running pCode dynamic function from .HRB */
                  hb_vmExecute( ( ( PHB_PCODEFUNC ) pFunc )->pCode, ( ( PHB_PCODEFUNC ) pFunc )->pSymbols );
               }
               else
                  pFunc();

               if( pFuncSym->scope.value & HB_FS_CLSERROR )
               {
                  hb_xfree( pFuncSym );

                  /* NEEDed because Destructor will inspect the symbol value, which was just FREEd */
                  pBase->item.asSymbol.value = pSym;
               }
            }
            else
            {
               const char * sClass = hb_objGetClsName( pSelf );
               hb_vmClassError( hb_pcount(), sClass, pSym->szName, NULL );
            }
         }
         else
            pFunc();
      } /* pSelf->type == HB_IT_NIL */

      if( ( iFlags & DIVERT_RESUME ) == 0 )
      {
         if( hb_stackGetActionRequest() == 0 )
         {
#ifdef DEBUG_DIVERT
            TraceLog( NULL, "DIVERT Ended: '%s' After: %s With: %p Return: %p Params: %i Arguments: %i  Locals Offseted: %i\n", pSym->szName, preset_pSym->szName, pSelf->type, HB_VM_STACK.Return.type, params, arguments, locals );
#endif /* DEBUG_DIVERT */

            hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
         }
      }
      else
      {
#ifdef DEBUG_DIVERT
         TraceLog( NULL, "DIVERT Resume: '%s' After: '%s' With: %p Params: %i Arguments: %i  Locals Offseted: %i\n", preset_pSym->szName, pSym->szName, pSelf->type, params, arguments, locals );
#endif /* DEBUG_DIVERT */
      }

      /* RESTORE FRAME! */

      /* Parent was VARPARAMS(...) - RE-SWAP if callee was Normal call. */
      if( params == HB_VAR_PARAM_FLAG )
      {
         /* DIVERT was ALSO a VARPARAMS(...) */
         if( pBase->item.asSymbol.pCargo->params == HB_VAR_PARAM_FLAG )
         {
            /* No need to RESWAP already done HB_P_FRAME of the DIVERT! */
#ifdef DEBUG_DIVERT
            TraceLog( NULL, "DIVERT: '%s' was VARPARAM too. Parent: '%s' Params: %i Arguments: %i  Locals Offseted: %i\n", pSym->szName, preset_pSym->szName, params, arguments, locals );
#endif /* DEBUG_DIVERT */
         }
         else
         {
            if( locals && arguments )
            {
               int         iLocals           = locals, iArguments = arguments;
               PHB_ITEM *  pStackArguments   = ( PHB_ITEM * ) hb_xgrab( iArguments * sizeof( PHB_ITEM ) );

#ifdef DEBUG_DIVERT
               TraceLog( NULL, "DIVERT '%s' had Normal Frame. ReSwap Parent: '%s' Params: %i Arguments: %i  Locals Offseted: %i\n", pSym->szName, preset_pSym->szName, params, arguments, locals );
#endif /* DEBUG_DIVERT */
               /* Store copy of the arguments pointers. */
               HB_MEMCPY( ( void * ) pStackArguments, ( void * ) ( HB_VM_STACK.pBase + 2 ), iArguments * sizeof( PHB_ITEM ) );

               /* Move the Locals Pointers into bottom of the frame. */
               memmove( ( void * ) ( HB_VM_STACK.pBase + 2 ), ( void * ) ( HB_VM_STACK.pBase + 2 + iArguments ), iLocals * sizeof( PHB_ITEM ) );

               /* Restore the Arguments Pointers on top of the Locals. */
               HB_MEMCPY( ( void * ) ( HB_VM_STACK.pBase + 2 + iLocals ), ( void * ) pStackArguments, iArguments * sizeof( PHB_ITEM ) );

               /* Free the copy */
               hb_xfree( ( void * ) pStackArguments );
            }
         }
      }
      /* Parent was normal call UnSwap if callee was VARPARAM. */
      else
      {
         if( pBase->item.asSymbol.pCargo->params != HB_VAR_PARAM_FLAG )
         {
            /* No need to RESWAP already done HB_P_FRAME of the DIVERT! */
#ifdef DEBUG_DIVERT
            TraceLog( NULL, "DIVERT: '%s' had Normal Frame too. Parent: %s Params: %i Arguments: %i  Locals Offseted: %i\n", pSym->szName, preset_pSym->szName, params, arguments, locals );
#endif /* DEBUG_DIVERT */
         }
         else
         {
            if( pBase->item.asSymbol.pCargo->locals && pBase->item.asSymbol.pCargo->arguments )
            {
               int         iLocals           = pBase->item.asSymbol.pCargo->locals, iArguments = pBase->item.asSymbol.pCargo->arguments;
               PHB_ITEM *  pStackArguments   = ( PHB_ITEM * ) hb_xgrab( iArguments * sizeof( PHB_ITEM ) );

#ifdef DEBUG_DIVERT
               TraceLog( NULL, "DIVERT: '%s' was VARPARAM UN-SWAP Parent: %s, Params: %i Arguments: %i  Locals Offseted: %i\n", pSym->szName, preset_pSym->szName, params, arguments, locals );
#endif /* DEBUG_DIVERT */
               /* Store copy of the arguments pointers. */
               HB_MEMCPY( ( void * ) pStackArguments, ( void * ) ( HB_VM_STACK.pBase + 2 + iLocals ), iArguments * sizeof( PHB_ITEM ) );

               /* Move the Locals Pointers up beyond the Arguments space. */
               memmove( ( void * ) ( HB_VM_STACK.pBase + 2 + iArguments ), ( void * ) ( HB_VM_STACK.pBase + 2 ), iLocals * sizeof( PHB_ITEM ) );

               /* Restore the Arguments Pointers to the BASE of the Frame. */
               HB_MEMCPY( ( void * ) ( HB_VM_STACK.pBase + 2 ), ( void * ) pStackArguments, iArguments * sizeof( PHB_ITEM ) );

               /* Free the copy */
               hb_xfree( ( void * ) pStackArguments );
            }
         }
      } /* params == HB_VAR_PARAM_FLAG */

      /* Restore! */
      if( pDivertSelf )
      {
         /* Restore! */
         hb_itemForwardValue( pSelf, pDivertSelf );
         /* Free! */
         hb_itemRelease( pDivertSelf );
      }

      s_iBaseLine                         = iBaseLine;
      pBase->item.asSymbol.value          = preset_pSym;
      pBase->item.asSymbol.pCargo->lineno = lineno;
      /* pBase->item.asSymbol.pCargo->arguments = arguments;
       * // not manipulated!
       */
      pBase->item.asSymbol.pCargo->params = params;
      pBase->item.asSymbol.pCargo->locals = locals;
   }
   else
      hb_errRT_BASE( EG_ARG, 9101, NULL, "DIVERT", 1, hb_stackItemFromTop( -1 ) );
}

/* sets the statics frame for a function */
static void hb_vmSFrame( PHB_SYMB pSym )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSFrame(%p)", pSym ) );

   /* _INITSTATICS is now the statics frame. Statics() changed it! */
   hb_stackSetStaticsBase( pSym->value.iStaticsBase ); /* pSym is { "$_INITSTATICS", HB_FS_INITEXIT, _INITSTATICS } for each PRG */
}

/* initializes the global aStatics array or redimensionates it */
static void hb_vmStatics( PHB_SYMB pSym, USHORT uiStatics )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmStatics(%p, %hu)", pSym, uiStatics ) );

   if( HB_IS_NIL( &s_aStatics ) )
   {
      pSym->value.iStaticsBase = 0; /* statics frame for this PRG */
      hb_arrayNew( &s_aStatics, uiStatics );
      /* printf( "Allocated s_aStatics: %p %p\n", &s_aStatics, s_aStatics.item.asArray.value->pOwners ); */
   }
   else
   {
      pSym->value.iStaticsBase = ( int ) ( &s_aStatics )->item.asArray.value->ulLen;
      hb_arraySize( &s_aStatics, ( &s_aStatics )->item.asArray.value->ulLen + uiStatics );
      /* TraceLog( NULL, "Symbol: %s Statics: %i Size: %i\n", pSym->szName, uiStatics, hb_arrayLen( &s_aStatics ) );
       */
   }
}

static void hb_vmEndBlock( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEndBlock()" ) );

   hb_stackPopReturn();
}

static void hb_vmRetValue( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRetValue()" ) );

   hb_stackPopReturn();
   /* for clipper compatibility */
   hb_stackReturnItem()->type &= ~HB_IT_MEMOFLAG;
}

static void hb_vmTimeStampPut( PHB_ITEM pItem, long lJulian, long lMilliSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmTimeStampPut(%p,%ld,%ld)", pItem, lJulian, lMilliSec ) );

   /* timestamp normalization */
   if( lJulian < 0 )
   {
      if( lMilliSec <= -HB_MILLISECS_PER_DAY )
      {
         lMilliSec += HB_MILLISECS_PER_DAY;
         --lJulian;
      }
      else if( lMilliSec > 0 )
      {
         lMilliSec -= HB_MILLISECS_PER_DAY;
         ++lJulian;
         if( lMilliSec > 0 )
         {
            lMilliSec -= HB_MILLISECS_PER_DAY;
            ++lJulian;
         }
      }
   }
   else
   {
      if( lMilliSec >= HB_MILLISECS_PER_DAY )
      {
         lMilliSec -= HB_MILLISECS_PER_DAY;
         ++lJulian;
      }
      else if( lMilliSec < 0 )
      {
         lMilliSec += HB_MILLISECS_PER_DAY;
         --lJulian;
         if( lMilliSec < 0 )
         {
            lMilliSec += HB_MILLISECS_PER_DAY;
            --lJulian;
         }
      }
   }

   hb_itemPutTDT( pItem, lJulian, lMilliSec );
}

static void hb_vmTimeStampAdd( PHB_ITEM pResult, PHB_ITEM pItem, double dValue )
{
   long lJulian, lMilliSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmTimeStampAdd(%p,%p,%lf)", pResult, pItem, dValue ) );

   hb_timeStampUnpackDT( dValue, &lJulian, &lMilliSec );

   lJulian     += pItem->item.asDate.value;
   lMilliSec   += pItem->item.asDate.time;

   hb_vmTimeStampPut( pResult, lJulian, lMilliSec );
}

static void hb_vmSumDate( PHB_ITEM pItem1, PHB_ITEM pItem2, PHB_ITEM pResult )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSumDate(%p,%p,%p)", pItem1, pItem2, pResult ) );

   if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem1 ) || HB_IS_TIMEFLAG( pItem2 ) )
         hb_vmTimeStampPut( pResult, pItem1->item.asDate.value +
                            pItem2->item.asDate.value,
                            pItem1->item.asDate.time +
                            pItem2->item.asDate.time );
      else
         /* NOTE: This is not a bug. CA-Cl*pper does exactly that for DATEs. */
         hb_itemPutDL( pResult, pItem1->item.asDate.value +
                       pItem2->item.asDate.value );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem1 ) )
      {
         if( HB_IS_NUMINT( pItem2 ) )
            hb_vmTimeStampPut( pResult, pItem1->item.asDate.value +
                               ( long ) HB_ITEM_GET_NUMINTRAW( pItem2 ),
                               pItem1->item.asDate.time );
         else
            hb_vmTimeStampAdd( pResult, pItem1, pItem2->item.asDouble.value );
      }
      else
         hb_itemPutDL( pResult, hb_itemGetDL( pItem1 ) + hb_itemGetNL( pItem2 ) );
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMEFLAG( pItem2 ) )
      {
         if( HB_IS_NUMINT( pItem1 ) )
            hb_vmTimeStampPut( pResult, ( long ) HB_ITEM_GET_NUMINTRAW( pItem1 ) +
                               pItem2->item.asDate.value,
                               pItem2->item.asDate.time );
         else
            hb_vmTimeStampAdd( pResult, pItem2, pItem1->item.asDouble.value );
      }
      else
         hb_itemPutDL( pResult, hb_itemGetNL( pItem1 ) + hb_itemGetDL( pItem2 ) );
   }

}

static void hb_vmSubDate( PHB_ITEM pDate, PHB_ITEM pOther )
{
   LONG  lDate = pDate->item.asDate.value;
   LONG  lTime = pDate->item.asDate.time;

   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSubDate(%p,%p)", pDate, pOther ) );

   if( HB_IS_NUMINT( pOther ) )
   {
      lDate -= hb_itemGetNL( pOther );
   }
   else
   {
      double   dDate = 0.0;
      div_t    result;

      lTime -= ( LONG ) ( modf( hb_itemGetND( pOther ), &dDate ) * ( double ) ( 86400 * HB_DATETIMEINSEC ) );

      if( lTime < 0 )
      {
         lTime += ( 86400 * HB_DATETIMEINSEC );
         dDate += 1;
      }
      result   = div( ( int ) lTime, ( int ) ( 86400 * HB_DATETIMEINSEC ) );
      lTime    = result.rem;
      lDate    -= ( ( LONG ) dDate - result.quot );
   }
   hb_stackPop();

   if( lDate < 0 )
   {
      lDate = 0;
   }

   pDate->type                = lTime ? HB_IT_TIMEFLAG : HB_IT_DATE;
   pDate->item.asDate.value   = lDate;
   pDate->item.asDate.time    = lTime;
}

/* ------------------------------- */
/* Push                            */
/* ------------------------------- */

void hb_vmPush( PHB_ITEM pItem )
{
   HB_THREAD_STUB
   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_vmPush(%p) type: %i", pItem, pItem->type ) );

   hb_itemCopy( hb_stackAllocItem(), pItem );
}

void hb_vmPushState( void )
{
   HB_THREAD_STUB
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushState()" ) );

   /* Save top item which can be processed at this moment */
   hb_stackPush();

   hb_stackPushReturn();
}

void hb_vmPushNil( void )
{
   HB_THREAD_STUB
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushNil()" ) );

   hb_stackAllocItem()->type = HB_IT_NIL;
}

void hb_vmPushLogical( BOOL bValue )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLogical(%d)", ( int ) bValue ) );

   pItem                         = hb_stackAllocItem();
   pItem->type                   = HB_IT_LOGICAL;
   pItem->item.asLogical.value   = bValue ? TRUE : FALSE;
}

void hb_vmPushNumber( double dNumber, int iDec )
{
   hb_vmPushNumType( dNumber, iDec, 0, 0 );
}

void hb_vmPushNumType( double dNumber, int iDec, int iType1, int iType2 )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushNumType(%lf, %d, %i, %i)", dNumber, iDec, iType1, iType2 ) );

   if( iDec || iType1 & HB_IT_DOUBLE || iType2 & HB_IT_DOUBLE )
      hb_vmPushDouble( dNumber, iDec );
   else if( HB_DBL_LIM_INT( dNumber ) )
      hb_vmPushInteger( ( int ) dNumber );
   else if( HB_DBL_LIM_LONG( dNumber ) )
      hb_vmPushHBLong( ( HB_LONG ) dNumber );
   else
      hb_vmPushDouble( dNumber, hb_stackSetStruct()->HB_SET_DECIMALS );
}

void hb_vmPushNumInt( HB_LONG lNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushNumInt(%Ld, %i, %i)", lNumber ) );

   if( HB_LIM_INT( lNumber ) )
      hb_vmPushInteger( ( int ) lNumber );
   else
      hb_vmPushHBLong( lNumber );
}

static int hb_vmCalcIntWidth( HB_LONG lNumber )
{
   int iWidth = 20;

   if( lNumber > -1000000000L )
   {
      iWidth = 10;
      while( lNumber >= 1000000000L )
      {
         iWidth++;
         lNumber /= 10;
      }
   }

   return iWidth;
}

void hb_vmPushInteger( int iNumber )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushInteger(%d)", iNumber ) );

   pItem                         = hb_stackAllocItem();
   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = iNumber;
   pItem->item.asInteger.length  = HB_INT_EXPLENGTH( iNumber );
}

#if HB_INT_MAX >= INT32_MAX
static void hb_vmPushIntegerConst( int iNumber )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushIntegerConst(%d)", iNumber ) );

   pItem                         = hb_stackAllocItem();
   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = iNumber;
   pItem->item.asInteger.length  = hb_vmCalcIntWidth( iNumber );
}

#else

static void hb_vmPushLongConst( LONG lNumber )
{
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLongConst(%ld)", lNumber ) );

   pItem                         = hb_stackAllocItem();
#if HB_INT_MAX >= LONG_MAX
   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = ( int ) lNumber;
   pItem->item.asInteger.length  = ( UINT ) hb_vmCalcIntWidth( lNumber );
#else
   pItem->type                   = HB_IT_LONG;
   pItem->item.asLong.value      = ( HB_LONG ) lNumber;
   pItem->item.asLong.length     = hb_vmCalcIntWidth( lNumber );
#endif /* HB_INT_MAX >= LONG_MAX */
}
#endif /* HB_INT_MAX >= INT32_MAX */

void hb_vmPushLong( LONG lNumber )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLong(%ld)", lNumber ) );

   pItem                         = hb_stackAllocItem();
#if HB_INT_MAX >= LONG_MAX
   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = ( int ) lNumber;
   pItem->item.asInteger.length  = HB_INT_EXPLENGTH( lNumber );
#else
   pItem->type                   = HB_IT_LONG;
   pItem->item.asLong.value      = ( HB_LONG ) lNumber;
   pItem->item.asLong.length     = HB_LONG_EXPLENGTH( lNumber );
#endif
}

static void hb_vmPushHBLong( HB_LONG lNumber )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushHBLong(%" PFHL "d)", lNumber ) );

   pItem                      = hb_stackAllocItem();
   pItem->type                = HB_IT_LONG;
   pItem->item.asLong.value   = lNumber;
   pItem->item.asLong.length  = HB_LONG_LENGTH( lNumber );
}

#if ! defined( HB_LONG_LONG_OFF )
static void hb_vmPushLongLongConst( LONGLONG llNumber )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLongLongConst(%" PFLL "d)", llNumber ) );

   pItem                      = hb_stackAllocItem();
   pItem->type                = HB_IT_LONG;
   pItem->item.asLong.value   = ( HB_LONG ) llNumber;
   pItem->item.asLong.length  = hb_vmCalcIntWidth( llNumber );
}
#endif /* HB_LONG_LONG_OFF */

void hb_vmPushDouble( double dNumber, int iDec )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushDouble(%lf, %d)", dNumber, iDec ) );

   pItem                         = hb_stackAllocItem();
   pItem->type                   = HB_IT_DOUBLE;
   pItem->item.asDouble.value    = dNumber;
   pItem->item.asDouble.length   = HB_DBL_LENGTH( dNumber );

   if( iDec == HB_DEFAULT_DECIMALS )
      pItem->item.asDouble.decimal = hb_stackSetStruct()->HB_SET_DECIMALS;
   else
      pItem->item.asDouble.decimal = iDec;
}

#if 0
static int hb_vmCalcDoubleWidth( double dNumber, int iDec )
{
   int iSize;

   if( dNumber < 0 )
      iSize = dNumber > -10 ? 2 : ( int ) log10( -dNumber ) + 2;
   else
      iSize = dNumber < 10 ? 1 : ( int ) log10( dNumber ) + 1;

   return iDec == 0 ? iSize + 1 : iSize;
}
#endif

static void hb_vmPushDoubleConst( double dNumber, int iWidth, int iDec )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushDoubleConst(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   pItem                      = hb_stackAllocItem();
   pItem->type                = HB_IT_DOUBLE;
   pItem->item.asDouble.value = dNumber;

   if( iDec == HB_DEFAULT_DECIMALS )
      pItem->item.asDouble.decimal = hb_stackSetStruct()->HB_SET_DECIMALS;
   else
      pItem->item.asDouble.decimal = ( UINT ) iDec;

   if( iWidth == HB_DEFAULT_WIDTH )
      pItem->item.asDouble.length = ( UINT ) HB_DBL_LENGTH( dNumber );
   else
      pItem->item.asDouble.length = ( UINT ) iWidth;
}

void hb_vmPushDate( LONG lDate )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushDate(%ld)", lDate ) );

   pItem                      = hb_stackAllocItem();
   pItem->type                = HB_IT_DATE;
   pItem->item.asDate.value   = lDate;
   pItem->item.asDate.time    = 0;
}

void hb_vmPushDateTime( LONG lDate, LONG lTime )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushDateTime(%ld,%ld)", lDate, lTime ) );

   pItem                      = hb_stackAllocItem();
   pItem->type                = HB_IT_TIMEFLAG;
   pItem->item.asDate.value   = lDate;
   pItem->item.asDate.time    = lTime;
}

void hb_vmPushPointer( void * pPointer )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushPointer(%ld)", pPointer ) );

   pItem                         = hb_stackAllocItem();
   pItem->type                   = HB_IT_POINTER;
   pItem->item.asPointer.value   = pPointer;
   pItem->item.asPointer.collect = FALSE;
}

void hb_vmPushString( const char * szText, HB_SIZE length )
{
   HB_THREAD_STUB
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushString( \"%s\", %lu ) %p", szText, length, ( *HB_VM_STACK.pPos ) ) );

   hb_itemPutCL( hb_stackAllocItem(), szText, length );
}

void hb_vmPushSymbol( PHB_SYMB pSym )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_vmPushSymbol(%p) \"%s\"", pSym, pSym->szName ) );

   pItem                                  = hb_stackAllocItem();
   hb_itemPutSymbol( pItem, pSym );
   pItem->item.asSymbol.pCargo->stackbase = ( long ) ( HB_VM_STACK.pBase - HB_VM_STACK.pItems );
}

void hb_vmPushDynSym( PHB_DYNS pDynSym )
{
   hb_vmPushSymbol( pDynSym->pSymbol );
}

void hb_vmPushEvalSym( void )
{
   hb_vmPushSymbol( &hb_symEval );
}

/* -3    -> HB_P_PUSHBLOCK
 * -2 -1 -> size of codeblock
 *  0 +1 -> number of expected parameters
 * +2 +3 -> number of referenced local variables
 * +4    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlock( const BYTE * pCode, USHORT usSize, BOOL bDynCode )
{
   HB_THREAD_STUB
   BYTE *   pBlockCode;
   BYTE *   pBlockTemp;
   USHORT   uiLocals;
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushBlock(%p, %p)", pCode ) );

   pItem       = hb_stackAllocItem();
   uiLocals    = HB_PCODE_MKUSHORT( &pCode[ 2 ] );
   usSize      -= 4 + ( uiLocals << 1 );
   pBlockCode  = ( BYTE * ) pCode + 4 + ( uiLocals << 1 );

   if( bDynCode )
   {
      pBlockTemp  = pBlockCode;
      pBlockCode  = ( BYTE * ) hb_xgrab( usSize );
      HB_MEMCPY( pBlockCode, pBlockTemp, usSize );
   }

   pItem->item.asBlock.value =
      hb_codeblockNew( pBlockCode,           /* pcode buffer */
                       uiLocals,             /* number of referenced local variables */
                       pCode + 4,            /* table with referenced local variables */
                       ( *HB_VM_STACK.pBase )->item.asSymbol.value );

   pItem->type                         = HB_IT_BLOCK;

   /* store the statics base of function where the codeblock was defined
    */
   pItem->item.asBlock.statics         = hb_stackGetStaticsBase();
   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt        = HB_PCODE_MKUSHORT( pCode );

   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.value->lineno   = ( *HB_VM_STACK.pBase )->item.asSymbol.pCargo->lineno;

   pItem->item.asBlock.value->uiClass  = 0;
   /* TraceLog( NULL, "PROC Block: '%s' Line: %i Module: %s\n", pTop->item.asBlock.value->symbol->szName, pTop->item.asBlock.value->lineno, HB_SYM_GETMODULESYM( pTop->item.asBlock.value->symbol ) ? HB_SYM_GETMODULESYM( pTop->item.asBlock.value->symbol )->szModuleName : "" );
    */
   if( HB_IS_ARRAY( *( HB_VM_STACK.pBase + 1 ) ) ) /* it is a method name */
   {
      pItem->item.asBlock.value->uiClass = ( *( HB_VM_STACK.pBase + 1 ) )->item.asArray.value->uiClass;
      /* TraceLog( NULL, "OBJECT Block: '%s' Line: %i Module: %s\n", pTop->item.asBlock.value->symbol->szName, pTop->item.asBlock.value->lineno, HB_SYM_GETMODULESYM( pTop->item.asBlock.value->symbol ) ? HB_SYM_GETMODULESYM( pTop->item.asBlock.value->symbol )->szModuleName : "" );
       */
   }

   pItem->item.asBlock.value->uLen = usSize;

   if( bDynCode )
      pItem->item.asBlock.value->uiFlags |= CBF_DYNAMIC_BUFFER;
}

/* -2    -> HB_P_PUSHBLOCKSHORT
 * -1    -> size of codeblock
 *  0    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlockShort( const BYTE * pCode, USHORT usSize, BOOL bDynCode )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushBlockShort(%p, %p)", pCode ) );

   if( bDynCode )
   {
      BYTE * pBuffer = ( BYTE * ) hb_xgrab( usSize );
      HB_MEMCPY( pBuffer, pCode, usSize );
      pCode = pBuffer;
   }

   pItem                      = hb_stackAllocItem();
   pItem->item.asBlock.value  =
      hb_codeblockNew( pCode,  /* pcode buffer */
                       0,      /* number of referenced local variables */
                       NULL,   /* table with referenced local variables */
                       ( *HB_VM_STACK.pBase )->item.asSymbol.value );

   pItem->type                         = HB_IT_BLOCK;

   /* store the statics base of function where the codeblock was defined
    */
   pItem->item.asBlock.statics         = hb_stackGetStaticsBase();
   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt        = 0;

   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.value->lineno   = ( *HB_VM_STACK.pBase )->item.asSymbol.pCargo->lineno;

   pItem->item.asBlock.value->uiClass  = 0;
   /* TraceLog( NULL, "PROC Block: '%s' Line: %i Module: %s\n", pTop->item.asBlock.value->symbol->szName, pTop->item.asBlock.value->lineno, HB_SYM_GETMODULESYM( pTop->item.asBlock.value->symbol ) ? HB_SYM_GETMODULESYM( pTop->item.asBlock.value->symbol )->szModuleName : "" ); */
   if( HB_IS_ARRAY( *( HB_VM_STACK.pBase + 1 ) ) ) /* it is a method name */
   {
      pItem->item.asBlock.value->uiClass = ( *( HB_VM_STACK.pBase + 1 ) )->item.asArray.value->uiClass;
      /* TraceLog( NULL, "OBJECT Block: '%s' Line: %i Module: %s\n", pTop->item.asBlock.value->symbol->szName, pTop->item.asBlock.value->lineno, HB_SYM_GETMODULESYM( pTop->item.asBlock.value->symbol ) ? HB_SYM_GETMODULESYM( pTop->item.asBlock.value->symbol )->szModuleName : "" ); */
   }

   pItem->item.asBlock.value->uLen = usSize;

   if( bDynCode )
      pItem->item.asBlock.value->uiFlags |= CBF_DYNAMIC_BUFFER; /* dynBuffer = bDynCode; */
}

/* +0    -> HB_P_MPUSHBLOCK
 * +1 +2 -> size of codeblock
 * +3 +4 -> number of expected parameters
 * +5    -> start of pcode
 *
 * NOTE: pCode points to dynamically allocated memory
 */
static void hb_vmPushMacroBlock( BYTE * pCode )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushMacroBlock(%p)", pCode ) );

   pItem                               = hb_stackAllocItem();

   pItem->item.asBlock.value           = hb_codeblockMacroNew( pCode + 5, HB_PCODE_MKUSHORT( &( pCode[ 1 ] ) ) - 5 );
   pItem->type                         = HB_IT_BLOCK;

   /* store the statics base of function where the codeblock was defined
    */
   pItem->item.asBlock.statics         = hb_stackGetStaticsBase();

   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt        = HB_PCODE_MKUSHORT( &( pCode[ 3 ] ) );

   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.value->symbol   = ( *HB_VM_STACK.pBase )->item.asSymbol.value;
   pItem->item.asBlock.value->lineno   = ( *HB_VM_STACK.pBase )->item.asSymbol.pCargo->lineno;

   if( hb_stackSetStruct()->HB_SET_MACROBLOCKVARS )
      pItem->item.asBlock.value->uiFlags |= CBF_PRIVATE_VARS;

   pItem->item.asBlock.value->uiClass = 0;
   /* TraceLog( NULL, "PROC Block: '%s' Line: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, ( *HB_VM_STACK.pBase )->item.asSymbol.pCargo->lineno ); */
   if( HB_IS_ARRAY( *( HB_VM_STACK.pBase + 1 ) ) ) /* it is a method name */
   {
      pItem->item.asBlock.value->uiClass = ( *( HB_VM_STACK.pBase + 1 ) )->item.asArray.value->uiClass;
      /* TraceLog( NULL, "OBJECT Block: '%s' Line: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, ( *HB_VM_STACK.pBase )->item.asSymbol.pCargo->lineno ); */
   }

   pItem->item.asBlock.value->uLen = ( USHORT ) ( HB_PCODE_MKUSHORT( &( pCode[ 1 ] ) ) - 5 );
}

/* pushes current workarea number on the eval stack
 */
static void hb_vmPushAlias( void )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushAlias()" ) );

   pItem                         = hb_stackAllocItem();
   pItem->type                   = HB_IT_INTEGER;
   pItem->item.asInteger.value   = hb_rddGetCurrentWorkAreaNumber();
   pItem->item.asInteger.length  = 10;
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of a given field
 * (for performance reason it replaces alias value with field value)
 */
static void hb_vmPushAliasedField( PHB_SYMB pSym )
{
   HB_THREAD_STUB
   PHB_ITEM pAlias;
   int      iCurrArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushAliasedField(%p)", pSym ) );

   iCurrArea   = hb_rddGetCurrentWorkAreaNumber();
   pAlias      = hb_stackItemFromTop( -1 );

   /*
    * NOTE: hb_vmSelectWorkarea clears passed item
    */
   if( hb_vmSelectWorkarea( pAlias, pSym ) == SUCCESS )
      hb_rddGetFieldValue( pAlias, pSym );

   hb_rddSelectWorkAreaNumber( iCurrArea );
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of either a field or a memvar based on alias value
 * (for performance reason it replaces alias value with field value)
 * This is used in the following context:
 * ( any_alias )->variable
 */
static void hb_vmPushAliasedVar( PHB_SYMB pSym )
{
   HB_THREAD_STUB
   PHB_ITEM pAlias = hb_stackItemFromTop( -1 );

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushAliasedVar(%p)", pSym ) );

   if( HB_IS_STRING( pAlias ) )
   {
      char * szAlias = pAlias->item.asString.value;

      if( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' )
      {
         if( szAlias[ 1 ] == '\0' ||            /* M->variable */
             ( pAlias->item.asString.length >= 4 &&
               hb_strnicmp( szAlias, "MEMVAR",  /* MEMVAR-> or MEMVA-> or MEMV-> */
                            pAlias->item.asString.length ) == 0 ) )
         {
            hb_memvarGetValue( pAlias, pSym );
            return;
         }
      }
      else if( pAlias->item.asString.length >= 4 &&
               ( hb_strnicmp( szAlias, "FIELD",    /* FIELD-> or FIEL-> */
                              pAlias->item.asString.length ) == 0 ||
                 hb_strnicmp( szAlias, "_FIELD",   /* _FIELD-> or _FIE-> */
                              pAlias->item.asString.length ) == 0 ) )
      {
         hb_rddGetFieldValue( pAlias, pSym );
         return;
      }
   }
   hb_vmPushAliasedField( pSym );
}

static void hb_vmPushLocal( SHORT iLocal )
{
   HB_THREAD_STUB

   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLocal(%hd)", iLocal ) );

   HB_STACK_OR_BLOCK_LOCAL( pLocal, iLocal );

   hb_itemCopy( hb_stackAllocItem(), pLocal );
}

static void hb_vmPushLocalByRef( SHORT iLocal )
{
   HB_THREAD_STUB

   PHB_ITEM pTop = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLocalByRef(%hd)", iLocal ) );

   /* we store its stack offset instead of a pointer to support a dynamic stack */
   if( iLocal >= 0 )
   {
#if 0
      PHB_ITEM pLocal = hb_stackLocalVariable( &iLocal );

      /*
       * R.P. assuming this was only an optimization, thus it was disabled
       * to allow detection of:
       *
       *    SomeLocal := ( @SomeLocal )
       *
       * so that such syntax can be used to express an explicit request
       * to reset "sticky" BYREF, f.e.:
       *
       *    SomeLocal := ( @SomeArray[1] )
       *
       * every subsequent assignment to SomeLocal will be forwarded to
       * SomeArray[1] which is indeed the intent, but then HOW do you
       * request to BREAK the BYREF?
       *
       * See condition in hb_vmPopLocal()
       */

      if( HB_IS_BYREF( pLocal ) && ( ! HB_IS_ENUM( pLocal ) ) && ( ! HB_IS_EXTREF( pLocal ) ) )
      {
         hb_itemCopy( pTop, pLocal );
         return;
      }
#endif /* 0 */

      /*
       * #ifdef HB_UNSHARE_REFERENCES
       *    hb_itemUnShare( *( HB_VM_STACK.pBase + iLocal + 1 ) );
       * #endif
       */
      pTop->item.asRefer.BasePtr.itemsbasePtr = hb_stackItemBasePtr();
   }
   else
   {
      /* store direct codeblock address because an item where a codeblock
       * is stored can be no longer placed on the eval stack at the time
       * of a codeblock evaluation or variable access
       */
      pTop->item.asRefer.BasePtr.block = ( hb_stackSelfItem() )->item.asBlock.value;
   }

   pTop->type                 = HB_IT_BYREF;
   pTop->item.asRefer.value   = iLocal;
   pTop->item.asRefer.offset  = ( long ) hb_stackBaseOffset();
}

static void hb_vmPushStatic( USHORT uiStatic )
{
   HB_THREAD_STUB

   PHB_ITEM pStatic;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushStatic(%hu)", uiStatic ) );

   pStatic = s_aStatics.item.asArray.value->pItems + hb_stackGetStaticsBase() + uiStatic - 1;
   hb_itemCopy( hb_stackAllocItem(),
                HB_IS_BYREF( pStatic ) ? hb_itemUnRef( pStatic ) : pStatic );
}

static void hb_vmPushStaticByRef( USHORT uiStatic )
{
   HB_THREAD_STUB

   PHB_ITEM pTop = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushStaticByRef(%hu)", uiStatic ) );

#ifdef HB_UNSHARE_REFERENCES
   hb_itemUnShare( s_aStatics.item.asArray.value->pItems + hb_stackGetStaticsBase() + uiStatic - 1 );
#endif

   pTop->type                             = HB_IT_BYREF;
   /* we store the offset instead of a pointer to support a dynamic stack */
   pTop->item.asRefer.value               = hb_stackGetStaticsBase() + uiStatic - 1;
   pTop->item.asRefer.offset              = 0; /* 0 for static variables */
   pTop->item.asRefer.BasePtr.pBaseArray  = s_aStatics.item.asArray.value;

#ifdef HB_ARRAY_USE_COUNTER
   HB_ATOMIC_INC( s_aStatics.item.asArray.value->ulHolders );
#else
   hb_arrayRegisterHolder( s_aStatics.item.asArray.value, ( void * ) pTop );
#endif
}

static void hb_vmPushVariable( PHB_SYMB pVarSymb )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;
   USHORT   uiAction;

   HB_TRACE( HB_TR_INFO, ( "(hb_vmPushVariable)" ) );

   pItem = hb_stackAllocItem();

   do
   {
      /* First try if passed symbol is a name of field
       * in a current workarea - if it is not a field (FAILURE)
       * then try the memvar variable
       */
      if( ( uiAction = hb_rddFieldGet( pItem, pVarSymb ) ) != HB_SUCCESS )
      {
         if( ( uiAction = hb_memvarGet( pItem, pVarSymb ) ) != HB_SUCCESS )
         {
            PHB_ITEM pError;

            pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                   NULL, pVarSymb->szName,
                                   0, EF_CANRETRY );

            uiAction = hb_errLaunch( pError );
            hb_errRelease( pError );
         }
      }
   }
   while( uiAction == E_RETRY );
}

static void hb_vmDuplicate( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDuplicate()" ) );

   pItem = hb_stackItemFromTop( -1 );
   hb_itemCopy( hb_stackAllocItem(), pItem );
}

static void hb_vmDuplTwo( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDuplTwo()" ) );

   pItem = hb_stackItemFromTop( -2 );
   hb_itemCopy( hb_stackAllocItem(), pItem );
   pItem = hb_stackItemFromTop( -2 );
   hb_itemCopy( hb_stackAllocItem(), pItem );
}

/* ------------------------------- */
/* Pop                             */
/* ------------------------------- */

void hb_vmPopState( void )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_vmPopState()" ) );

   hb_stackPopReturn();

   /* Restore top item */
   hb_stackDec();
}

static BOOL hb_vmPopLogical( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopLogical()" ) );

   if( HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) )
   {
      hb_stackDec();

      ( *HB_VM_STACK.pPos )->type = HB_IT_NIL;
      return ( *HB_VM_STACK.pPos )->item.asLogical.value;
   }
#ifdef HB_USE_NUMERIC_IF
   else if( HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) )
   {
      hb_stackDec();

      ( *HB_VM_STACK.pPos )->type = HB_IT_NIL;

      if( HB_IS_INTEGER( hb_stackItemFromTop( -1 ) ) )
         return ( *HB_VM_STACK.pPos )->item.asInteger.value != 0;
      else if( HB_IS_LONG( hb_stackItemFromTop( -1 ) ) )
         return ( *HB_VM_STACK.pPos )->item.asLong.value != 0;
      else
         return ( *HB_VM_STACK.pPos )->item.asDouble.value != 0.0;
   }
#endif /* HB_USE_NUMERIC_IF */
   else
   {
      hb_errRT_BASE( EG_ARG, 1066, NULL, hb_langDGetErrorDesc( EG_CONDITION ), 1, hb_stackItemFromTop( -1 ) );
      return FALSE;
   }
}

/* NOTE: Type checking should be done by the caller. */

static double hb_vmPopNumber( void )
{
   HB_THREAD_STUB

   double   dNumber;
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopNumber()" ) );

   pItem = hb_stackItemFromTop( -1 );
   hb_stackDec();

   switch( pItem->type )
   {
      case HB_IT_INTEGER:
         dNumber = ( double ) pItem->item.asInteger.value;
         break;

      case HB_IT_LONG:
         dNumber = ( double ) pItem->item.asLong.value;
         break;

      case HB_IT_DOUBLE:
         dNumber = pItem->item.asDouble.value;
         break;

      case HB_IT_DATE:
         dNumber = ( double ) pItem->item.asDate.value;
         break;

      case HB_IT_TIMEFLAG:
         dNumber = hb_datetimePack( pItem->item.asDate.value, pItem->item.asDate.time );
         break;

      case HB_IT_STRING:
         dNumber = ( double ) ( BYTE ) ( pItem->item.asString.value[ 0 ] );
         hb_itemReleaseString( pItem );
         break;

      default:
         dNumber = 0.0; /* To avoid GCC -O2 warning */
         hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_vmPopNumber()", NULL );
         break;
   }

   ( *HB_VM_STACK.pPos )->type = HB_IT_NIL;

   return dNumber;
}

/* NOTE: Type checking should be done by the caller. */

static HB_LONG hb_vmPopHBLong( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;
   HB_LONG  lNumber;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopHBLong()" ) );

   pItem = hb_stackItemFromTop( -1 );
   hb_stackDec();

   switch( pItem->type )
   {
      case HB_IT_INTEGER:
         lNumber = ( HB_LONG ) pItem->item.asInteger.value;
         break;

      case HB_IT_LONG:
         lNumber = ( HB_LONG ) pItem->item.asLong.value;
         break;

      case HB_IT_DOUBLE:
         lNumber = ( HB_LONG ) pItem->item.asDouble.value;
         break;

      default:
         lNumber = 0; /* To avoid GCC -O2 warning */
         hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_vmPopNumber()", NULL );
         break;
   }

   ( hb_stackTopItem() )->type = HB_IT_NIL;

   return lNumber;
}

/* NOTE: Type checking should be done by the caller. */

static double hb_vmPopDouble( int * piDec )
{
   HB_THREAD_STUB

   double   dNumber;
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopDouble(%p)", piDec ) );

   pItem                         = hb_stackItemFromTop( -1 );
   hb_stackDec();
   dNumber                       = hb_itemGetNDDec( pItem, piDec );
   ( *HB_VM_STACK.pPos )->type   = HB_IT_NIL;

   return dNumber;
}

/* Pops the item from the eval stack and uses it to select the current
 * workarea
 */
static void hb_vmPopAlias( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopAlias()" ) );

   hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ), NULL ); /* it clears the passed item */
   hb_stackDec();
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into a given field
 */
static void hb_vmPopAliasedField( PHB_SYMB pSym )
{
   HB_THREAD_STUB

   int iCurrArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopAliasedField(%p)", pSym ) );

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();

   if( hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ), pSym ) == SUCCESS )
      hb_rddPutFieldValue( hb_stackItemFromTop( -2 ), pSym );

   hb_rddSelectWorkAreaNumber( iCurrArea );
   hb_stackDec(); /* alias - it was cleared in hb_vmSelectWorkarea */
   hb_stackPop(); /* field value */
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into either a field or a memvar based on the alias value
 * This is used in the following context:
 * ( any_alias )->variable
 */
static void hb_vmPopAliasedVar( PHB_SYMB pSym )
{
   HB_THREAD_STUB

   PHB_ITEM pAlias = hb_stackItemFromTop( -1 );

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopAliasedVar(%p)", pSym ) );

   /* "M", "MEMV" - "MEMVAR" and "FIEL" - "FIELD" are reserved aliases
    */
   if( HB_IS_STRING( pAlias ) )
   {
      char * szAlias = pAlias->item.asString.value;

      if( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' )
      {
         if( szAlias[ 1 ] == '\0' ||            /* M->variable */
             ( pAlias->item.asString.length >= 4 &&
               hb_strnicmp( szAlias, "MEMVAR",  /* MEMVAR-> or MEMVA-> or MEMV-> */
                            pAlias->item.asString.length ) == 0 ) )
         {
            hb_memvarSetValue( pSym, hb_stackItemFromTop( -2 ) );
            hb_stackPop(); /* alias */
            hb_stackPop(); /* value */
            return;
         }
      }
      else if( pAlias->item.asString.length >= 4 &&
               ( hb_strnicmp( szAlias, "FIELD",    /* FIELD-> or FIEL-> */
                              pAlias->item.asString.length ) == 0 ||
                 hb_strnicmp( szAlias, "_FIELD",   /* _FIELD-> or _FIE-> */
                              pAlias->item.asString.length ) == 0 ) )
      {
         hb_rddPutFieldValue( hb_stackItemFromTop( -2 ), pSym );
         hb_stackPop(); /* alias */
         hb_stackPop(); /* value */
         return;
      }
   }

   hb_vmPopAliasedField( pSym );
}

static void hb_vmPopLocal( SHORT iLocal )
{
   HB_THREAD_STUB

   PHB_ITEM pLocal, pVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopLocal(%hd)", iLocal ) );

   pVal        = hb_stackItemFromTop( -1 );

   /* Remove MEMOFLAG if exists (assignment from field). */
   pVal->type  &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );

   HB_STACK_OR_BLOCK_LOCAL( pLocal, iLocal );

   if( HB_IS_BYREF( pVal ) )
   {
      /*
       *  R.P. explicit request to unreference:
       *
       *     SomeLocal := @SomeLocal
       */
      if( hb_itemUnRefOnce( pVal ) == hb_stackItemFromBase( iLocal ) )
      {
         if( hb_stackItemFromBase( iLocal ) != pLocal )
            /* Keep the ultimate value! */
            hb_itemCopy( hb_stackItemFromBase( iLocal ), pLocal );

         hb_stackDec();

         return;
      }
      else if( hb_itemUnRef( pVal ) == pLocal )
      {
         hb_errRT_BASE( EG_ARG, 9104, NULL, "Cyclic-Reference assignment!", 0 );
         hb_stackDec();

         return;
      }
   }

   if( ( HB_IS_NUMBER( pLocal ) && HB_IS_NUMBER( pVal ) ) || pLocal->type == pVal->type )
      hb_itemForwardValue( pLocal, pVal );
   else if( hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmOperatorCall( pLocal, pVal, "__OPASSIGN", NULL, 0, pLocal );
      hb_itemClear( pVal );
   }
   else
      hb_itemForwardValue( pLocal, pVal );

   hb_stackDec();
}

static void hb_vmPopStatic( USHORT uiStatic )
{
   HB_THREAD_STUB

   PHB_ITEM pStatic;
   PHB_ITEM pVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopStatic(%hu)", uiStatic ) );

   pVal        = hb_stackItemFromTop( -1 );

   /* Remove MEMOFLAG if exists (assignment from field). */
   pVal->type  &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
   pStatic     = s_aStatics.item.asArray.value->pItems + hb_stackGetStaticsBase() + uiStatic - 1;

/* #define DEBUG_STATIC_DESTRUCTORS */
#ifdef DEBUG_STATIC_DESTRUCTORS
   if( HB_IS_ARRAY( pVal ) )
   {
      if( pVal->item.asArray.value->uiClass )
      {
         PCLASS pClass = hb_clsClassesArray() + pVal->item.asArray.value->uiClass - 1;

         if( pClass->uiScope & HB_OO_CLS_DESTRUC_SYMB )
         {
            hb_errRT_BASE( EG_ARG, 3009, "Static value can not contain an object with a destructor", "hb_vmPopStatic", 1, pVal );
            return;
         }
      }
   }
#endif /* DEBUG_STATIC_DESTRUCTORS */

   /* TraceLog( NULL, "Assign Static: %i, Class: %s\n", uiStatic, hb_objGetClsName( pVal ) ); */

   if( HB_IS_BYREF( pVal ) )
   {
      if( hb_itemUnRef( pVal ) == pStatic )
      {
         hb_errRT_BASE( EG_ARG, 9105, NULL, "Cyclic-Reference assignment!", 0 );
         hb_stackDec();
         return;
      }
   }

   if( ( HB_IS_NUMBER( pStatic ) && HB_IS_NUMBER( pVal ) ) || pStatic->type == pVal->type )
      hb_itemForwardValue( pStatic, pVal );
   else if( hb_objGetOpOver( pStatic ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmOperatorCall( pStatic, pVal, "__OPASSIGN", NULL, 0, pStatic );
      hb_itemClear( pVal );
   }
   else
      hb_itemForwardValue( pStatic, pVal );

   hb_stackDec();
}

/* ----------------------------------------------- */
/*
 * Functions to manage module symbols
 */

PHB_SYMB hb_vmGetRealFuncSym( PHB_SYMB pSym )
{
   if( pSym && ! ( pSym->scope.value & HB_FS_LOCAL ) )
   {
      pSym = pSym->pDynSym &&
             ( pSym->pDynSym->pSymbol->scope.value & HB_FS_LOCAL ) ?
             pSym->pDynSym->pSymbol : NULL;
   }

   return pSym;
}

BOOL hb_vmFindModuleSymbols( PHB_SYMB pSym, PHB_SYMB * pSymbols,
                             USHORT * puiSymbols )
{
   if( pSym )
   {
      PSYMBOLS pLastSymbols = s_pSymbols;

/*
 *    if( pSym->scope.value & HB_FS_PCODEFUNC )
 *       pSymbols = pSym->value.pCodeFunc->pSymbols;
 */

      while( pLastSymbols )
      {
         if( pLastSymbols->fActive &&
             pSym >= pLastSymbols->pSymbolTable &&
             pSym < pLastSymbols->pSymbolTable + pLastSymbols->uiModuleSymbols )
         {
            *pSymbols   = pLastSymbols->pSymbolTable;
            *puiSymbols = ( USHORT ) pLastSymbols->uiModuleSymbols;
            return TRUE;
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }

   *pSymbols   = NULL;
   *puiSymbols = 0;
   return FALSE;
}

static PSYMBOLS hb_vmFindFreeModule( PHB_SYMB pSymbols, UINT uiSymbols, const char * szModuleName )
{
   PSYMBOLS       pLastSymbols = s_pSymbols;
   PHB_SYMB       pModuleSymbols;
   register UINT  ui;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFindFreeModule(%p,%hu,%s)", pSymbols, uiSymbols, szModuleName ) );

   if( s_ulFreeSymbols )
   {
      while( pLastSymbols )
      {
         if( ! pLastSymbols->fActive &&
             pLastSymbols->uiModuleSymbols == uiSymbols &&
             pLastSymbols->szModuleName != NULL &&
             strcmp( pLastSymbols->szModuleName, szModuleName ) == 0 )
         {
            pModuleSymbols = pLastSymbols->pSymbolTable;

            for( ui = 0; ui < uiSymbols; ++ui )
            {
               if( ( pSymbols[ ui ].scope.value & ~( HB_FS_PCODEFUNC | HB_FS_DYNCODE ) ) !=
                   pModuleSymbols[ ui ].scope.value ||
                   strcmp( pSymbols[ ui ].szName, pModuleSymbols[ ui ].szName ) != 0 )
                  break;
            }

            if( ui == uiSymbols )
            {
               --s_ulFreeSymbols;
               return pLastSymbols;
            }
         }

         pLastSymbols = pLastSymbols->pNext;
      }
   }

   return NULL;
}

void hb_vmFreeSymbols( PSYMBOLS pSymbols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFreeSymbols(%p)", pSymbols ) );

   if( pSymbols->fActive )
   {
      register UINT ui;

      for( ui = 0; ui < pSymbols->uiModuleSymbols; ++ui )
      {
         /* do not overwrite already initialized statics' frame */
         if( ! HB_ISINITEXIT( pSymbols->pSymbolTable[ ui ].scope.value ) )
         {
            pSymbols->pSymbolTable[ ui ].value.pFunPtr   = NULL;
            pSymbols->pSymbolTable[ ui ].scope.value     &= ~( HB_FS_PCODEFUNC | HB_FS_DYNCODE | HB_FS_LOCAL );
         }
      }

      pSymbols->hDynLib = NULL;
      pSymbols->fActive = FALSE;
      ++s_ulFreeSymbols;
   }
}

void hb_vmBeginSymbolGroup( void * hDynLib, BOOL fClone )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmBeginSymbolGroup(%p,%d)", hDynLib, ( int ) fClone ) );

   s_hDynLibID = hDynLib;
   s_fCloneSym = fClone;
}

void hb_vmInitSymbolGroup( void * hNewDynLib, int argc, char * argv[] )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInitSymbolGroup(%p,%d,%p)", hNewDynLib, argc, argv ) );

   s_fCloneSym = FALSE;

   if( s_hDynLibID )
   {
      PSYMBOLS       pLastSymbols   = s_pSymbols;
      void *         hDynLib        = s_hDynLibID;
      register UINT  ui;
      BOOL           fFound         = FALSE;

      s_hDynLibID = NULL;

      while( pLastSymbols )
      {
         if( pLastSymbols->hDynLib == hDynLib )
         {
            fFound = TRUE;

            if( pLastSymbols->fInitStatics && pLastSymbols->fActive )
            {
               for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
               {
                  if( HB_ISINITEXIT( ( pLastSymbols->pSymbolTable + ui )->scope.value ) )
                  {
                     hb_vmPushSymbol( pLastSymbols->pSymbolTable + ui );
                     hb_vmPushNil();
                     hb_vmDo( 0 );
                  }
               }

               pLastSymbols->fInitStatics = FALSE;
            }

            pLastSymbols->hDynLib = hNewDynLib;
         }

         pLastSymbols = pLastSymbols->pNext;
      }

      if( fFound )
      {
         pLastSymbols = s_pSymbols;

         while( pLastSymbols )
         {
            if( pLastSymbols->hDynLib == hNewDynLib )
            {
               if( pLastSymbols->fActive && ( pLastSymbols->hScope & HB_FS_INIT ) != 0 )
               {
                  for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
                  {
                     if( HB_ISINIT( ( pLastSymbols->pSymbolTable + ui )->scope.value ) )
                     {
                        register int i;

                        hb_vmPushSymbol( pLastSymbols->pSymbolTable + ui );
                        hb_vmPushNil();

                        for( i = 0; i < argc; ++i )
                           hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );

                        hb_vmDo( ( USHORT ) argc );
                     }
                  }
               }
            }

            pLastSymbols = pLastSymbols->pNext;
         }
      }
   }
}

void hb_vmExitSymbolGroup( void * hDynLib )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmExitSymbolGroup(%p)", hDynLib ) );

   if( hDynLib )
   {
      PSYMBOLS pLastSymbols   = s_pSymbols;
      BOOL     fFound         = FALSE;

      while( pLastSymbols )
      {
         if( pLastSymbols->hDynLib == hDynLib )
         {
            fFound = TRUE;

            if( pLastSymbols->fActive && ( pLastSymbols->hScope & HB_FS_EXIT ) != 0 )
            {
               register UINT ui;

               for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
               {
                  if( HB_ISEXIT( ( pLastSymbols->pSymbolTable + ui )->scope.value ) )
                  {
                     hb_vmPushSymbol( pLastSymbols->pSymbolTable + ui );
                     hb_vmPushNil();
                     hb_vmDo( 0 );
                  }
               }
            }
         }

         pLastSymbols = pLastSymbols->pNext;
      }

      if( fFound )
      {
         pLastSymbols = s_pSymbols;

         while( pLastSymbols )
         {
            if( pLastSymbols->hDynLib == hDynLib )
            {
               hb_clsDeactiveClass( pLastSymbols );
               hb_vmFreeSymbols( pLastSymbols );
            }

            pLastSymbols = pLastSymbols->pNext;
         }
      }
   }
}

PSYMBOLS    hb_vmRegisterSymbols( PHB_SYMB pSymbolTable   , UINT uiSymbols, const char * szModuleName,  BOOL fDynLib, BOOL fClone, PHB_ITEM * pGlobals )
{
   PSYMBOLS       pNewSymbols = NULL;
   PHB_SYMB       pSymbol;
   HB_SYMBOLSCOPE hSymScope;
   register UINT  ui;
   BOOL           fInitStatics = FALSE;
   BOOL           fRecycled;
   BOOL           fPublic;
   BOOL           fStatics;

#ifdef BROKEN_MODULE_SPACE_LOGIC
   PHB_FUNC       pModuleFirstFunction, pModuleLastFunction;
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRegisterSymbols(%p,%hu,%s,%d,%d,%p)", pSymbolTable, uiSymbols, szModuleName, ( int ) fDynLib, ( int ) fClone, pGlobals ) );

#if 0
   TraceLog( NULL, "Register: %s\n", szModuleName );
#endif

   if( s_ulFreeSymbols )
      pNewSymbols = hb_vmFindFreeModule( pSymbolTable, uiSymbols, szModuleName );

   if( pNewSymbols )
   {
      pNewSymbols->fActive = fRecycled = TRUE;
      pNewSymbols->hDynLib = s_hDynLibID;
      pNewSymbols->hScope  = 0;
   }
   else
   {
      fRecycled = FALSE;

      if( fClone )
      {
         PHB_SYMB pSymbols = ( PHB_SYMB ) hb_xgrab( uiSymbols * sizeof( HB_SYMB ) );

         HB_MEMCPY( pSymbols, pSymbolTable, uiSymbols * sizeof( HB_SYMB ) );

         for( ui = 0; ui < uiSymbols; ui++ )
            pSymbols[ ui ].szName = hb_strdup( pSymbols[ ui ].szName );

         pSymbolTable = pSymbols;
      }

      pNewSymbols                   = ( PSYMBOLS ) hb_xgrab( sizeof( SYMBOLS ) );
      pNewSymbols->pSymbolTable     = pSymbolTable;
      pNewSymbols->uiModuleSymbols  = uiSymbols;
      pNewSymbols->szModuleName     = hb_strdup( szModuleName );
      
      pNewSymbols->fAllocated       = fClone;
      pNewSymbols->fActive          = TRUE;
      pNewSymbols->fInitStatics     = FALSE;
      pNewSymbols->hDynLib          = s_hDynLibID;
      pNewSymbols->hScope           = 0;
      pNewSymbols->pNext            = NULL;

      if( s_pSymbols == NULL )
         s_pSymbols = pNewSymbols;
      else
      {
         PSYMBOLS pLastSymbols;

         pLastSymbols = s_pSymbols;

         while( pLastSymbols->pNext ) /* locates the latest processed group of symbols */
            pLastSymbols = pLastSymbols->pNext;

         pLastSymbols->pNext = pNewSymbols;
      }
   }

   pNewSymbols->pGlobals = pGlobals;

#ifdef BROKEN_MODULE_SPACE_LOGIC
   if( uiSymbols > 2 && pNewSymbols->pSymbolTable[ uiSymbols - 1 ].szName[ 0 ] == '!' && pNewSymbols->pSymbolTable[ uiSymbols - 2 ].szName[ 0 ] == '!' )
   {
      pModuleFirstFunction = pNewSymbols->pSymbolTable[ uiSymbols - 2 ].value.pFunPtr;
      pModuleLastFunction  = pNewSymbols->pSymbolTable[ uiSymbols - 1 ].value.pFunPtr;
      uiSymbols            -= 2;
   }
   else
   {
      pModuleFirstFunction = ( PHB_FUNC ) 0x00000000;
      pModuleLastFunction  = ( PHB_FUNC ) 0xFFFFFFFF;
   }
#endif /* BROKEN_MODULE_SPACE_LOGIC */

/* #define DEBUG_SYMBOLS */

#ifdef DEBUG_SYMBOLS
   if( fDynLib )
      TraceLog( NULL, "Module: '%s' is DYNAMIC\n", szModuleName );
   else
      TraceLog( NULL, "Module: '%s' is NOT DYNAMIC\n", szModuleName );
#endif /* DEBUG_SYMBOLS */

   for( ui = 0; ui < uiSymbols; ui++ ) /* register each public symbol on the dynamic symbol table */
   {
      pSymbol  = pNewSymbols->pSymbolTable + ui;

      fStatics = HB_ISINITEXIT( pSymbol->scope.value );

      if( fRecycled && ! fStatics )
      {
         pSymbol->value.pFunPtr  = ( pSymbolTable + ui )->value.pFunPtr;
         pSymbol->scope.value    = ( pSymbolTable + ui )->scope.value;
      }

      if( fDynLib )
         pSymbol->scope.value |= HB_FS_DYNCODE;

      hSymScope            = pSymbol->scope.value;
      pNewSymbols->hScope  |= hSymScope;
      fPublic              = ( hSymScope & HB_FS_PUBLIC ) != 0;

      if( fStatics )
         fInitStatics = TRUE;

      if( ( hSymScope & HB_FS_PCODEFUNC ) != 0 && ( fRecycled || fClone ) )
         pSymbol->value.pCodeFunc->pSymbols = pNewSymbols->pSymbolTable;

      if( ! s_pSymStart && fDynLib == FALSE && ( hSymScope & HB_FS_FIRST ) != 0 && ( hSymScope & HB_FS_STATIC ) == 0 )
         /* first public defined symbol to start execution */
         s_pSymStart = pSymbol;

      /* Enable this code to see static functions which are registered in global dynsym table */
#if 0
      if( fPublic && ( hSymScope & HB_FS_STATIC ) != 0 )
         TraceLog( NULL, "Registring STATIC!!!: %s:%s scope %04x\r\n", szModuleName, pSymbol->szName, hSymScope );
#endif /* 0 */

      if( fPublic )
      {
         PHB_DYNS pDynSym = hb_dynsymFind( pSymbol->szName );

#ifdef DEBUG_SYMBOLS
         TraceLog( NULL, "Public: '%s' of Module: '%s' is: %s\n", pSymbol->szName, szModuleName, ( hSymScope & HB_FS_LOCAL ) == HB_FS_LOCAL ? "LOCAL" : "IMPORTED" );
#endif /* DEBUG_SYMBOLS */

#ifdef BROKEN_MODULE_SPACE_LOGIC
         if( ( hSymScope & HB_FS_LOCAL ) == HB_FS_LOCAL )
         {
            /* TraceLog( NULL, "Local Function: '%s' of Module: '%s'\n", pSymbol->szName, szModuleName ); */

            if( ( void * ) pSymbol->value.pFunPtr < ( void * ) pModuleFirstFunction || ( void * ) pSymbol->value.pFunPtr > ( void * ) pModuleLastFunction )
            {
               hSymScope            &= ~HB_FS_LOCAL;
               pSymbol->scope.value &= ~HB_FS_LOCAL;

               TraceLog( NULL, "Local Function: '%s' of Module: '%s' is not linked in.\n", pSymbol->szName, szModuleName );
            }
            /* else
             * {
             *   TraceLog( NULL, "LINKED Local Function: '%s' of Module: '%s' %p > %p < %p\n", pSymbol->szName, szModuleName, pModuleFirstFunction, pSymbol->value.pFunPtr, pModuleLastFunction );
             * }
             */
         }
#endif /* BROKEN_MODULE_SPACE_LOGIC */
         /* #define TRACE_DUPLICATE_FUNCTIONS */

         if( fDynLib )
         {
            if( pSymbol->value.pFunPtr || ( hSymScope & HB_FS_DEFERRED ) )
            {
#ifdef DEBUG_SYMBOLS
               TraceLog( NULL, "Symbol: %s has pointer OR is DEFERRED.\n", pSymbol->szName );
#endif /* DEBUG_SYMBOLS */
               if( pDynSym )
               {
                  if( ( hSymScope & HB_FS_LOCAL ) == HB_FS_LOCAL )
                  {
                     PSYMBOLS pModuleSymbols;

                     assert( pSymbol->value.pFunPtr );

                     if( ( pDynSym->pSymbol->scope.value & HB_FS_LOCAL ) != HB_FS_LOCAL )
                     {
                        /* This is the first candidate of true local symbol. */
                        pDynSym->pSymbol        = pSymbol;
                        pDynSym->pModuleSymbols = pNewSymbols;
                     }
                     else if( pDynSym->pSymbol->value.pFunPtr == pSymbol->value.pFunPtr )
                     {
#ifdef HB_STARTUP_REVERSED_LINK_ORDER
#ifdef TRACE_DUPLICATE_FUNCTIONS
                        /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                        TraceLog( NULL, "*** WARNING! Function: %s in Module: %s shadows previously registered Module: %s\n",
                                  pSymbol->szName, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>" );
#endif /* TRACE_DUPLICATE_FUNCTIONS */
                        pDynSym->pSymbol        = pSymbol;
                        pDynSym->pModuleSymbols = pNewSymbols;
#else

                        if( pDynSym->pSymbol->scope.value & HB_FS_DYNCODE )
                        {
#ifdef TRACE_DUPLICATE_FUNCTIONS
                           /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                           TraceLog( NULL, "*** WARNING! Function: %s in Module: %s shadows previously registered DYNAMIC Module: %s\n",
                                     pSymbol->szName, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>" );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                           /* The prior function was loaded from a DLL, so later registration must always take effect! */
                           pDynSym->pSymbol        = pSymbol;
                           pDynSym->pModuleSymbols = pNewSymbols;
                        }
                        else
                        {
#ifdef TRACE_DUPLICATE_FUNCTIONS
                          /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                          TraceLog( NULL, "*** WARNING! Function: %s in Module: %s is hidden by previously linked Module: %s\n",
                                    pSymbol->szName, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>" );
#endif /* TRACE_DUPLICATE_FUNCTIONS */
                       }
#endif /* HB_STARTUP_REVERSED_LINK_ORDER */
                    }
                    else
                    {
                       assert( pDynSym->pSymbol->value.pFunPtr );

#ifdef HB_STARTUP_REVERSED_LINK_ORDER
#ifdef TRACE_DUPLICATE_FUNCTIONS
                       /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                       TraceLog( NULL, "*** WARNING! Function: %s duplicate definition %p in Module: %s shadows previously registered Module: %s Definition %p\n",
                                 pSymbol->szName, pSymbol->value.pFunPtr, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>", pDynSym->pSymbol->value.pFunPtr );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                       pDynSym->pSymbol->value.pFunPtr  = pSymbol->value.pFunPtr;

                       pDynSym->pSymbol->scope.value    &= ~( HB_FS_LOCAL | HB_FS_PCODEFUNC );
                       pDynSym->pSymbol->scope.value    |= ( pSymbol->scope.value & HB_FS_PCODEFUNC );

                       pDynSym->pSymbol                 = pSymbol;
                       pDynSym->pModuleSymbols          = pNewSymbols;

                       hb_vmSymbolOverloadDefinition( pDynSym );
#else

                       if( pDynSym->pSymbol->scope.value & HB_FS_DYNCODE )
                       {
#ifdef TRACE_DUPLICATE_FUNCTIONS
                          /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                          TraceLog( NULL, "*** WARNING! Function: %s duplicate definition %p in Module: %s shadows previously registered DYNAMIC Module: %s Definition %p\n",
                                    pSymbol->szName, pSymbol->value.pFunPtr, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>", pDynSym->pSymbol->value.pFunPtr );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                          /* The prior function was loaded from a DLL, so later registration must always take effect! */
                          pDynSym->pSymbol->value.pFunPtr  = pSymbol->value.pFunPtr;

                          pDynSym->pSymbol->scope.value    &= ~( HB_FS_LOCAL | HB_FS_PCODEFUNC );
                          pDynSym->pSymbol->scope.value    |= ( pSymbol->scope.value & HB_FS_PCODEFUNC );

                          pDynSym->pSymbol                 = pSymbol;
                          pDynSym->pModuleSymbols          = pNewSymbols;

                          hb_vmSymbolOverloadDefinition( pDynSym );
                       }
                       else
                       {
#ifdef TRACE_DUPLICATE_FUNCTIONS
                          /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                          TraceLog( NULL, "*** WARNING! Function: %s Duplicate Definition: %p in Module: %s is hidden by previously registered Module: %s Definition: %p\n",
                                    pSymbol->szName, pSymbol->value.pFunPtr, szModuleName,
                                    pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>", pDynSym->pSymbol->value.pFunPtr );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                          /*
                           * Force all local symbols instances of public function to use the first (or last) registered definition,
                           * This will force a single function implementation at prg level, even if linker allowed multiple definitions.
                           */
                          pSymbol->value.pFunPtr  = pDynSym->pSymbol->value.pFunPtr;

                          pSymbol->scope.value    &= ~( HB_FS_LOCAL | HB_FS_PCODEFUNC );
                          pSymbol->scope.value    |= ( pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC );

                          hb_vmSymbolOverloadDefinition( pDynSym );
                       }
#endif /* HB_STARTUP_REVERSED_LINK_ORDER */
                       hSymScope = pSymbol->scope.value;
                    }

                    pModuleSymbols = s_pSymbols;

                    while( pModuleSymbols )
                    {
                       if( ( pModuleSymbols->hScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED )
                       {
                          PHB_SYMB       pModuleSymbol;
                          register UINT  ui;
#ifdef DEBUG_SYMBOLS
                          TraceLog( NULL, "Module: '%s' has Deferred Symbols, resolving: '%s' \n", pModuleSymbols->szModuleName, pSymbol->szName );
#endif /* DEBUG_SYMBOLS */
                          for( ui = 0; ui < pModuleSymbols->uiModuleSymbols; ui++ )
                          {
                             pModuleSymbol = pModuleSymbols->pSymbolTable + ui;

                             if( pModuleSymbol->pDynSym == pDynSym && ( ( pModuleSymbol->scope.value & HB_FS_DEFERRED ) == HB_FS_DEFERRED ) /* && pModuleSymbol->value.pFunPtr == NULL */ )
                             {

                                pModuleSymbol->scope.value   = ( HB_SYMBOLSCOPE ) ( pModuleSymbol->scope.value & ~HB_FS_PCODEFUNC ) | ( hSymScope & HB_FS_PCODEFUNC );
                                pModuleSymbol->value.pFunPtr = pSymbol->value.pFunPtr;
#ifdef DEBUG_SYMBOLS
                                TraceLog( NULL, "Resolved Deferred: '%s'\n", pSymbol->szName );
#endif /* DEBUG_SYMBOLS */
                             }
                          }
                       }
#ifdef DEBUG_SYMBOLS
                       else
                          TraceLog( NULL, "Module: '%s' does NOT have any Deferred Symbols\n", pModuleSymbols->szModuleName );
#endif /* DEBUG_SYMBOLS */
                       pModuleSymbols = pModuleSymbols->pNext;
                    }
                 }
                 /* Should we support dynamic overloading of already resolved HB_FS_DEFERRED as per below? */
                 else if( pDynSym->pSymbol->value.pFunPtr )
                 {
                    if( ( pSymbol->scope.value & HB_FS_DEFERRED ) == HB_FS_DEFERRED /* && pSymbol->value.pFunPtr == NULL */ )
                    {
                       pSymbol->scope.value   = ( HB_SYMBOLSCOPE ) ( hSymScope & ~HB_FS_PCODEFUNC ) | ( pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC );
                       pSymbol->value.pFunPtr = pDynSym->pSymbol->value.pFunPtr;
                    }
                 }

                 pSymbol->pDynSym = pDynSym;
                 continue;
              }
           }
           else /* if( pSymbol->value.pFunPtr || ( hSymScope & HB_FS_DEFERRED ) ) */
           {
              if( pDynSym )
              {
                 pSymbol->pDynSym = pDynSym;
                 continue;
              }
           }
        }
        else
        {
           if( pDynSym )
           {
              if( ( hSymScope & HB_FS_LOCAL ) == HB_FS_LOCAL )
              {
                 assert( pSymbol->value.pFunPtr );

                 if( ( pDynSym->pSymbol->scope.value & HB_FS_LOCAL ) != HB_FS_LOCAL )
                 {
                    /* This is the first candidate of true local symbol. */
                    pDynSym->pSymbol        = pSymbol;
                    pDynSym->pModuleSymbols = pNewSymbols;
                 }
                 else if( pDynSym->pSymbol->value.pFunPtr == pSymbol->value.pFunPtr )
                 {
#ifdef HB_STARTUP_REVERSED_LINK_ORDER
#ifdef TRACE_DUPLICATE_FUNCTIONS
                    /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                    TraceLog( NULL, "*** WARNING! Function: %s in Module: %s shadows previously registered Module: %s\n",
                              pSymbol->szName, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>" );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                    pDynSym->pSymbol        = pSymbol;
                    pDynSym->pModuleSymbols = pNewSymbols;

#else

                    if( pDynSym->pSymbol->scope.value & HB_FS_DYNCODE )
                    {
#ifdef TRACE_DUPLICATE_FUNCTIONS
                       /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                       TraceLog( NULL, "*** WARNING! Function: %s in Module: %s shadows previously registered DYNAMIC Module: %s\n",
                                 pSymbol->szName, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>" );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                       /* The prior function was loaded from a DLL, so later registration must always take effect! */
                       pDynSym->pSymbol        = pSymbol;
                       pDynSym->pModuleSymbols = pNewSymbols;
                    }
                    else
                    {
#ifdef TRACE_DUPLICATE_FUNCTIONS
                       /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                       TraceLog( NULL, "*** WARNING! Function: %s in Module: %s is hidden by previously linked Module: %s\n",
                                 pSymbol->szName, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>" );
#endif /* TRACE_DUPLICATE_FUNCTIONS */
                    }
#endif /* HB_STARTUP_REVERSED_LINK_ORDER */
                 }
                 else
                 {
                    assert( pDynSym->pSymbol->value.pFunPtr );

#ifdef HB_STARTUP_REVERSED_LINK_ORDER
#ifdef TRACE_DUPLICATE_FUNCTIONS
                    /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                    TraceLog( NULL, "*** WARNING! Function: %s duplicate definition %p in Module: %s shadows previously registered Module: %s Definition %p\n",
                              pSymbol->szName, pSymbol->value.pFunPtr, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>", pDynSym->pSymbol->value.pFunPtr );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                    pDynSym->pSymbol->value.pFunPtr  = pSymbol->value.pFunPtr;

                    pDynSym->pSymbol->scope.value    &= ~( HB_FS_LOCAL | HB_FS_PCODEFUNC );
                    pDynSym->pSymbol->scope.value    |= ( pSymbol->scope.value & HB_FS_PCODEFUNC );

                    pDynSym->pSymbol                 = pSymbol;
                    pDynSym->pModuleSymbols          = pNewSymbols;

                    hb_vmSymbolOverloadDefinition( pDynSym );
#else

                    if( pDynSym->pSymbol->scope.value & HB_FS_DYNCODE )
                    {
#ifdef TRACE_DUPLICATE_FUNCTIONS
                       /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                       TraceLog( NULL, "*** WARNING! Function: %s duplicate definition %p in Module: %s shadows previously registered DYNAMIC Module: %s Definition %p\n",
                                 pSymbol->szName, pSymbol->value.pFunPtr, szModuleName, pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>", pDynSym->pSymbol->value.pFunPtr );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                       /* The prior function was loaded from a DLL, so later registration must always take effect! */
                       pDynSym->pSymbol->value.pFunPtr  = pSymbol->value.pFunPtr;

                       pDynSym->pSymbol->scope.value    &= ~( HB_FS_LOCAL | HB_FS_PCODEFUNC );
                       pDynSym->pSymbol->scope.value    |= ( pSymbol->scope.value & HB_FS_PCODEFUNC );

                       pDynSym->pSymbol                 = pSymbol;
                       pDynSym->pModuleSymbols          = pNewSymbols;

                       hb_vmSymbolOverloadDefinition( pDynSym );
                    }
                    else
                    {
#ifdef TRACE_DUPLICATE_FUNCTIONS
                       /* NOTE: hb_traceInit() is not yet executed, but it uses s_bEmpty to not override output preceding hb_vmInit() */
                       TraceLog( NULL, "*** WARNING! Function: %s Duplicate Definition: %p in Module: %s is hidden by previously registered Module: %s Definition: %p\n",
                                 pSymbol->szName, pSymbol->value.pFunPtr, szModuleName,
                                 pDynSym->pModuleSymbols ? pDynSym->pModuleSymbols->szModuleName : "<unspecified>", pDynSym->pSymbol->value.pFunPtr );
#endif /* TRACE_DUPLICATE_FUNCTIONS */

                       /*
                          Force all local symbols instances of public function to use the first (or last) registered definition,
                          This will force a single function implementation at prg level, even if linker allowed multiple definitions.
                        */
                       pSymbol->value.pFunPtr  = pDynSym->pSymbol->value.pFunPtr;

                       pSymbol->scope.value    &= ~( HB_FS_LOCAL | HB_FS_PCODEFUNC );
                       pSymbol->scope.value    |= ( pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC );

                       hb_vmSymbolOverloadDefinition( pDynSym );
                    }
#endif /* HB_STARTUP_REVERSED_LINK_ORDER */
                    /* hSymScope = pSymbol->scope.value; // End of loop - no longer used! */
                 }
              }
              else
              {
                 if( pDynSym->pSymbol->value.pFunPtr == NULL && pSymbol->value.pFunPtr )
                    pDynSym->pSymbol = pSymbol;
              }

              pSymbol->pDynSym = pDynSym;
              continue;
           }
        }

        /* pDynSym =*/ hb_dynsymNew( pSymbol, pNewSymbols );
        /* TraceLog( NULL, "Module: %s Dyn: %p %s pModuleSymbols: %p\n", szModuleName, pDynSym, pSymbol->szName, pDynSym->pModuleSymbols ); */
      }
   }

   if( ! fRecycled )
      pNewSymbols->fInitStatics = fInitStatics;

   return pNewSymbols;
}

#if ! defined( HB_NO_DUPLICATE_HVMPROCESSSYMBOL )
PSYMBOLS hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char * szModule,  int iPCodeVer, PHB_ITEM * pGlobals ) /* module symbols initialization */
{
   //HB_TRACE( HB_TR_DEBUG, ( "hb_vmProcessSymbols(%p, %dl )", pSymbols ) );
     HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessSymbols(%p,%hu,%s,%hu)", pSymbols, uiModuleSymbols, szModule,  iPCodeVer));

#ifdef HB_THREAD_SUPPORT
   /* initialize internal mutex for MT mode */
   hb_threadInit();
#endif

   if( iPCodeVer != HB_PCODE_VER )
   {
      char szPCode[ 12 ];

      hb_snprintf( szPCode, sizeof( szPCode ), "%i", iPCodeVer );

      hb_errInternal( HB_EI_ERRUNRECOV,
                      "Module: '%s' was compiled into PCODE version: %s,"
                      "this version of xHarbour expects version: " __STR( HB_PCODE_VER ), szModule, szPCode );
   }

#if ( ! defined( __BORLANDC__ ) || defined( __EXPORT__ ) )
   if( s_Do_xinit )
   {
      s_Do_xinit = FALSE;

      hb_xinit();
   }
#endif

   return hb_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModule,  s_bDynamicSymbols, s_fCloneSym, pGlobals );
}
#endif

/* HVM & RTL in harbour.dll */
PSYMBOLS hb_vmProcessSysDllSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char * szModule,  int iPCodeVer, PHB_ITEM * pGlobals )
{
   
   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessSysDllSymbols(%p,%hu,%s,%hu)", pSymbols, uiModuleSymbols, szModule,  iPCodeVer));   

#ifdef HB_THREAD_SUPPORT
   /* initialize internal mutex for MT mode */
   hb_threadInit();
#endif

   if( iPCodeVer != HB_PCODE_VER )
   {
      char szPCode[ 12 ];

      hb_snprintf( szPCode, sizeof( szPCode ), "%i", iPCodeVer );

      hb_errInternal( HB_EI_ERRUNRECOV,
                      "Module: '%s' was compiled into PCODE version: %s,"
                      "this version of xHarbour expects version: " __STR( HB_PCODE_VER ), szModule, szPCode );
   }

#if ( ! defined( __BORLANDC__ ) || defined( __EXPORT__ ) )
   if( s_Do_xinit )
   {
      s_Do_xinit = FALSE;

      hb_xinit();
   }
#endif

   return hb_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModule,   TRUE, s_fCloneSym, pGlobals );
}

/*
   This is the worker function for the hb_vmProcessSymbols() variation from maindlle.c
   It is important that prg dlls which are staticly linked to harbour.dll should be
   linked to dllmain.lib, ahead of harbour.dll, so that hb_vmProcessSymbols() from maindlle.c
   will be used to process the prg dll's symbols, instead of the vmProcessSymbols() from
   maindllh.c which is a wrapper of vmProcessSysDllSymbols() and is included in harbour.dll
 */
PSYMBOLS hb_vmProcessPrgDllSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char * szModule,  int iPCodeVer, PHB_ITEM * pGlobals )
{
   PSYMBOLS pNewSymbols;
   PHB_SYMB pSymStart = s_pSymStart;

  
   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessSysDllSymbols(%p,%hu,%s,%lu,%hu)", pSymbols, uiModuleSymbols, szModule,  iPCodeVer));      

#ifdef HB_THREAD_SUPPORT
   /* initialize internal mutex for MT mode */
   hb_threadInit();
#endif

   if( iPCodeVer != HB_PCODE_VER )
   {
      char szPCode[ 12 ];

      hb_snprintf( szPCode, sizeof( szPCode ), "%i", iPCodeVer );

      hb_errInternal( HB_EI_ERRUNRECOV,
                      "Module: '%s' was compiled into PCODE version: %s,"
                      "this version of xHarbour expects version: " __STR( HB_PCODE_VER ), szModule, szPCode );
   }

#if ( ! defined( __BORLANDC__ ) || defined( __EXPORT__ ) )
   if( s_Do_xinit )
   {
      s_Do_xinit = FALSE;

      hb_xinit();
   }
#endif

   /* s_bDynamicSymbols used instead of TRUE, because we still want to support that functionality. */
   pNewSymbols = hb_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModule,   FALSE, s_bDynamicSymbols, pGlobals );
   /* Incase any of the prg dll's sources was *not* compiled with -n0! */
   s_pSymStart = pSymStart;

   return pNewSymbols;
}

/*
   This is the worker function for the hb_vmProcessSymbols() variation from usedll.c
   It is important that prg exes which are staticly linked to harbour.dll should be
   linked to usedll.lib, ahead of harbour.dll, so that hb_vmProcessSymbols() from usedll.c
   will be used to process the client exe symbols, instead of the vmProcessSymbols() from
   maindllh.c which is a wrapper of vmProcessSysDllSymbols() and is included in harbour.dll
 */
PSYMBOLS hb_vmProcessExeUsesDllSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char * szModule,  int iPCodeVer, PHB_ITEM * pGlobals )
{
   
   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessDllSymbols(%p,%hu,%s,%hu)", pSymbols, uiModuleSymbols, szModule, iPCodeVer));         

#ifdef HB_THREAD_SUPPORT
   /* initialize internal mutex for MT mode */
   hb_threadInit();
#endif

   if( iPCodeVer != HB_PCODE_VER )
   {
      char szPCode[ 12 ];

      hb_snprintf( szPCode, sizeof( szPCode ), "%i", iPCodeVer );

      hb_errInternal( HB_EI_ERRUNRECOV,
                      "Module: '%s' was compiled into PCODE version: %s,"
                      "this version of xHarbour expects version: " __STR( HB_PCODE_VER ), szModule, szPCode );
   }

#if ( ! defined( __BORLANDC__ ) || defined( __EXPORT__ ) )
   if( s_Do_xinit )
   {
      s_Do_xinit = FALSE;

      hb_xinit();
   }
#endif

   /* s_bDynamicSymbols used instead of TRUE, because we still want to support that functionality. */
   return hb_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModule,  FALSE, s_bDynamicSymbols, pGlobals );
}

PSYMBOLS * hb_vmSymbols( void )
{
   return &s_pSymbols;
}

PSYMBOLS hb_vmLastModule( void )
{
   PSYMBOLS pLastModule = s_pSymbols;

   if( pLastModule )
   {
      while( pLastModule->pNext )
         pLastModule = pLastModule->pNext;
   }

   return pLastModule;
}

void hb_vmExplicitStartup( PHB_SYMB pSymbol )
{
   s_pSymStart = pSymbol; /* first public defined symbol to start execution */
   /* printf( "Startup: '%s' Func: %p\n", pSymbol->szName, pSymbol->value.pFunPtr ); */
}

/* This calls all _INITSTATICS functions defined in the application.
 * We are using a special symbol's scope HB_FS_INITEXIT to mark
 * this function. These two bits cannot be marked at the same
 * time for normal user defined functions.
 */
static void hb_vmDoInitStatics( void )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDoInitStatics()" ) );

   do
   {
      if( pLastSymbols->fInitStatics )
      {
         register UINT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            if( HB_ISINITEXIT( ( pLastSymbols->pSymbolTable + ui )->scope.value ) )
            {
               hb_vmPushSymbol( pLastSymbols->pSymbolTable + ui );
               hb_vmPushNil();
               hb_vmDo( 0 );
            }
         }

         pLastSymbols->fInitStatics = FALSE;
      }

      pLastSymbols = pLastSymbols->pNext;

   }
   while( pLastSymbols );
}

void hb_vmDoExitFunctions( void )
{
   HB_THREAD_STUB

   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDoExitFunctions()" ) );

   while( pLastSymbols )
   {
      /* only if module contains some EXIT functions */
      if( pLastSymbols->fActive && pLastSymbols->hScope & HB_FS_EXIT )
      {
         register UINT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            if( HB_ISEXIT( ( pLastSymbols->pSymbolTable + ui )->scope.value ) )
            {
               hb_vmPushSymbol( pLastSymbols->pSymbolTable + ui );
               hb_vmPushNil();
               hb_vmDo( 0 );

               if( hb_stackGetActionRequest() )
               {
                  /* QUIT or BREAK was issued - stop processing
                   */
                  return;
               }
            }
         }
      }

      pLastSymbols = pLastSymbols->pNext;
   }
}

static void hb_vmDoInitFunctions( void )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDoInitFunctions()" ) );

   do
   {
      /* only if module contains some INIT functions */
      if( pLastSymbols->fActive && pLastSymbols->hScope & HB_FS_INIT )
      {
         register UINT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            if( HB_ISINIT( ( pLastSymbols->pSymbolTable + ui )->scope.value ) )
            {
               int            argc        = hb_cmdargARGC();
               register int   i;
               register int   iArgCount   = 0;
               char **        argv        = hb_cmdargARGV();

               hb_vmPushSymbol( pLastSymbols->pSymbolTable + ui );
               hb_vmPushNil();

               for( i = 1; i < argc; i++ ) /* places application parameters on the stack */
               {
                  /* Filter out any parameters beginning with //, like //INFO */
                  if( ! hb_cmdargIsInternal( argv[ i ] ) )
                  {
                     hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );
                     iArgCount++;
                  }
               }

               hb_vmDo( ( USHORT ) iArgCount );
            }
         }
      }

      pLastSymbols = pLastSymbols->pNext;

   }
   while( pLastSymbols );
}

PSYMBOLS hb_vmFindModule( PHB_SYMB pSymbols )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFindModule(%p)", pSymbols ) );

   do
   {
      if( pLastSymbols->pSymbolTable == pSymbols )
      {
         return pLastSymbols;
      }
      pLastSymbols = pLastSymbols->pNext;
   }
   while( pLastSymbols );

   return NULL;
}

PSYMBOLS hb_vmFindModuleByName( char * szModuleName )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFindModuleByName(%s)", szModuleName ) );

   do
   {
      if( strcmp( pLastSymbols->szModuleName, szModuleName ) == 0 )
         return pLastSymbols;

      pLastSymbols = pLastSymbols->pNext;
   }
   while( pLastSymbols );

   return NULL;
}

static PHB_SYMB hb_vmFindSymInModule( PSYMBOLS pModuleSymbols, const char * szName )
{
   if( pModuleSymbols )
   {
      PHB_SYMB       pSymbolTable = pModuleSymbols->pSymbolTable;
      PHB_SYMB       pSymbol;
      register UINT  ui;

      for( ui = 0; ui < pModuleSymbols->uiModuleSymbols; ++ui )
      {
         pSymbol = pSymbolTable + ui;

         if( strcmp( pSymbol->szName, szName ) == 0 )
            return pSymbol;
      }
   }

   return NULL;
}

/* NOTE: We should make sure that these get linked.
         Don't make this function static, because it's not called from
         this file. [vszakats] */

void hb_vmForceLink( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmForceLink()" ) );

   HB_FUNCNAME( SYSINIT ) ();
}

/* ----------------------------- */

HB_FUNC( ERRORLEVEL )
{
   HB_THREAD_STUB_API

   hb_retni( s_iErrorLevel );

   /* NOTE: This should be ISNUM( 1 ), but it's sort of a Clipper bug that it
            accepts other types also and considers them zero. [vszakats] */

   if( hb_pcount() >= 1 )
   {
      /* Only replace the error level if a parameter was passed */
      s_iErrorLevel = hb_parni( 1 );
   }
}

void hb_vmRequestQuit( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestQuit()" ) );

   hb_stackSetActionRequest( HB_QUIT_REQUESTED );
}

void hb_vmRequestEndProc( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestEndProc()" ) );

   hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
}

void hb_vmRequestBreak( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestBreak(%p)", pItem ) );

   if( pItem && HB_VM_STACK.pSequence )
   {
      PHB_SEQUENCE pSequence = HB_VM_STACK.pSequence;

      /*
       * while( pSequence && ( pSequence->uiStatus & HB_SEQ_RECOVERED ) && ( pSequence->uiStatus & HB_SEQ_RETHROW == 0 ) )
       * {
       *    pSequence = pSequence->pPrev;
       * }
       */

      if( pSequence )
      {
         assert( pSequence->lBase );

         if( hb_stackItem( pSequence->lBase - 1 )->type == HB_IT_NIL )
         {
#ifdef DEBUG_FINALLY
            printf( "Break Type %i for: %p at: %li\n", pItem->type, pSequence, pSequence->lBase - 1 );
#endif

            hb_itemForwardValue( hb_stackItem( pSequence->lBase - 1 ), pItem );
         }
         else
         {
#ifdef DEBUG_FINALLY
            printf( "Disregard Break Type %i for: %p at: %li\n", pItem->type, pSequence, pSequence->lBase - 1 );
#endif
         }
      }
   }

   hb_stackSetActionRequest( HB_BREAK_REQUESTED );
}

void hb_vmRequestCancel( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestCancel()" ) );

   if( hb_stackSetStruct()->HB_SET_CANCEL )
   {
      char           buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 + 64 ]; /* 64 for the Canceled at: (%i) overhead. */
      register UINT  i     = 1;
      register UINT  i2;
      USHORT         uLine = 0;

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( buffer, sizeof( buffer ), "Cancelled at: %s (%i)", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.pCargo->lineno );
      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );

      while( buffer[ 0 ] )
      {
         hb_procinfo( ( int ) i++, buffer, &uLine, NULL );

         if( buffer[ 0 ] == 0 )
            break;

         i2 = ( UINT ) strlen( ( char * ) buffer );
         hb_snprintf( buffer + i2, sizeof( buffer ) - i2, " (%u)", uLine );

         hb_conOutErr( buffer, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }

      hb_stackSetActionRequest( HB_QUIT_REQUESTED );
   }
}

USHORT hb_vmRequestQuery( void )
{
   HB_THREAD_STUB

   return hb_stackGetActionRequest();
}

BOOL hb_vmRequestReenter( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestReenter()" ) );

   hb_stackPushReturn();

   hb_vmPushInteger( hb_stackGetActionRequest() );
   hb_stackSetActionRequest( 0 );

   return TRUE;
}

void hb_vmRequestRestore( void )
{
   USHORT uiAction;

   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestRestore()" ) );

   uiAction = ( USHORT ) hb_stackItemFromTop( -1 )->item.asInteger.value |
              hb_stackGetActionRequest();
   if( uiAction & HB_QUIT_REQUESTED )
      hb_stackSetActionRequest( HB_QUIT_REQUESTED );
   else if( uiAction & HB_BREAK_REQUESTED )
      hb_stackSetActionRequest( HB_BREAK_REQUESTED );
   else if( uiAction & HB_ENDPROC_REQUESTED )
      hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
   else
      hb_stackSetActionRequest( 0 );

   hb_stackDec();
   hb_stackPopReturn();
}

void hb_vmRequestReset( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestReset()" ) );

   hb_stackSetActionRequest( 0 );
}

void hb_vmRequest( USHORT uiRequest )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequest()" ) );

   hb_stackSetActionRequest( uiRequest );
}

/* ------------------------------------------------------------------------ */
/* The garbage collector interface */
/* ------------------------------------------------------------------------ */

/* Mark all locals as used so they will not be released by the
 * garbage collector
 */

#ifndef HB_THREAD_SUPPORT
void hb_vmIsLocalRef( void )
{
   UINT  i;
   UINT  iCounter;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmIsLocalRef()" ) );

   if( HB_IS_GCITEM( &hb_stackST.Return ) )
   {
      hb_gcItemRef( &( hb_stackST.Return ) );
   }
   /* printf( "After ReturnRef\n" ); */

   if( hb_stackST.pPos > hb_stackST.pItems )
   {
      /* the eval stack is not cleared yet */
      PHB_ITEM * pItem = hb_stackST.pPos - 1;

      while( pItem != hb_stackST.pItems )
      {
         if( HB_IS_GCITEM( *pItem ) )
            hb_gcItemRef( *pItem );

         --pItem;
      }
   }

   /* FOR EACH Enumerations. */
   iCounter = HB_VM_STACK.wEnumCollectionCounter;
   for( i = 0; i < iCounter; i++ )
   {
      if( HB_IS_GCITEM( &( HB_VM_STACK.aEnumCollection[ i ] ) ) )
         hb_gcItemRef( &( HB_VM_STACK.aEnumCollection[ i ] ) );
   }

   /* WITH OBJECT */
   iCounter = HB_VM_STACK.wWithObjectCounter;
   for( i = 0; i < iCounter; i++ )
   {
      if( HB_IS_GCITEM( &( HB_VM_STACK.aWithObject[ i ] ) ) )
         hb_gcItemRef( &( HB_VM_STACK.aWithObject[ i ] ) );
   }
}
#endif /* HB_THREAD_SUPPORT */

/* Mark all statics as used so they will not be released by the
 * garbage collector
 */
void hb_vmIsStaticRef( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmIsStaticRef()" ) );

   /* statics are stored as an item of array type */
   hb_gcItemRef( &s_aStatics );
}

void hb_vmRegisterGlobals( PHB_ITEM ** pGlobals, short iGlobals )
{
   HB_THREAD_STUB

   PHB_ITEM    pTop        = ( *HB_VM_STACK.pPos );
   UINT           uiPrevLen   = ( USHORT ) ( &s_aGlobals )->item.asArray.value->ulLen;
   UINT           uiAdd       = 0;
   UINT           ulLen;
   register UINT  iGlobal;

   ulLen       = iGlobals + uiPrevLen;
   hb_arraySize( &s_aGlobals, ulLen );

   pTop->type  = HB_IT_BYREF;

   for( iGlobal = uiPrevLen + 1; iGlobal <= ulLen; iGlobal++ )
   {
      pTop->item.asRefer.value  = ( LONG )++ uiAdd;  /* To offset the -1 below. */
      pTop->item.asRefer.offset = -1;                /* Because 0 will be translated as a STATIC in hb_itemUnref(); */
      pTop->item.asRefer.BasePtr.itemsbasePtr = pGlobals;

      hb_arraySet( &s_aGlobals, iGlobal, pTop );
      /* printf( "*** Added %i ***\n", iGlobal ); */
   }

   pTop->type = HB_IT_NIL;
}

void hb_vmIsGlobalRef( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmIsGlobalRef()" ) );

   /* Globals are stored as an item of array type */
   hb_gcItemRef( &s_aGlobals );
}

/* $Doc$
 * $FuncName$     __SETPROFILER( <lOnOff> ) --> <lOldValue>
 * $Description$  Turns on | off the profiler activity
 * $End$ */
HB_FUNC( __SETPROFILER )
{
#ifndef HB_NO_PROFILER
   HB_THREAD_STUB_API

   BOOL bOldValue = hb_bProfiler;

   hb_bProfiler = hb_parl( 1 );

   hb_retl( bOldValue );
#endif
}

/* $Doc$
 * $FuncName$     __TRACEPRGCALLS( <lOnOff> ) --> <lOldValue>
 * $Description$  Turns on | off tracing of PRG-level function and method calls
 * $End$ */
HB_FUNC( __TRACEPRGCALLS )
{
   HB_THREAD_STUB_API
#ifndef HB_NO_TRACE
   BOOL bOldValue    = hb_bTracePrgCalls;

   hb_bTracePrgCalls = hb_parl( 1 );

   hb_retl( bOldValue );
#else

   hb_retl( 0 );
#endif
}

void hb_vmPushBaseArray( PHB_BASEARRAY pBaseArray )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushBaseArray(%p)", pBaseArray ) );
   pItem                      = hb_stackAllocItem();
   pItem->type                = HB_IT_ARRAY;
   pItem->item.asArray.value  = pBaseArray;

#ifdef HB_ARRAY_USE_COUNTER
   HB_ATOMIC_INC( pBaseArray->ulHolders );
#else
   hb_arrayRegisterHolder( pBaseArray, ( void * ) pItem );
#endif
}

/* ------------------------------- */
/* Extended references             */
/* ------------------------------- */

/*
 * extended item reference functions
 */
static PHB_ITEM hb_vmItemRefRead( PHB_ITEM pRefer )
{
   return ( PHB_ITEM ) pRefer->item.asExtRef.value;
}

static PHB_ITEM hb_vmItemRefWrite( PHB_ITEM pRefer, PHB_ITEM pSource )
{
   HB_SYMBOL_UNUSED( pSource );
   return ( PHB_ITEM ) pRefer->item.asExtRef.value;
}

static void hb_vmItemRefCopy( PHB_ITEM pDest )
{
   pDest->type = HB_IT_NIL;
   hb_itemCopy( pDest, ( PHB_ITEM ) pDest->item.asExtRef.value );
}

static void hb_vmItemRefDummy( void * value )
{
   HB_SYMBOL_UNUSED( value );
}

/*
 * push extended item reference
 */
void hb_vmPushItemRef( PHB_ITEM pItem )
{
   static const HB_EXTREF  s_ItmExtRef = {
      hb_vmItemRefRead,
      hb_vmItemRefWrite,
      hb_vmItemRefCopy,
      hb_vmItemRefDummy,
      hb_vmItemRefDummy
   };

   PHB_ITEM                pRefer;

   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushItemRef(%p)", pItem ) );

   pRefer                        = hb_stackAllocItem();
   pRefer->type                  = HB_IT_BYREF | HB_IT_EXTREF;
   pRefer->item.asExtRef.value   = ( void * ) pItem;
   pRefer->item.asExtRef.func    = &s_ItmExtRef;
}

HB_FUNC( HB_FUNCPTR )
{
   HB_THREAD_STUB

   PHB_ITEM pParam = hb_stackItemFromBase( 1 );
   PHB_DYNS pDynSym;

   if( HB_IS_STRING( pParam ) )
   {
      char * sSym = hb_strUpperCopy( pParam->item.asString.value, pParam->item.asString.length );

      pDynSym = hb_dynsymFind( sSym );

      if( pDynSym )
      {
         ( &( HB_VM_STACK.Return ) )->type                     = HB_IT_POINTER;
         ( &( HB_VM_STACK.Return ) )->item.asPointer.value     = ( void * ) pDynSym->pSymbol;
         ( &( HB_VM_STACK.Return ) )->item.asPointer.collect   = FALSE;
      }
      else
      {
         hb_vmPushLong( 0 );
      }

      /* avoid cross locking */
      hb_xfree( sSym );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1606, NULL, "HB_FuncPtr", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( HB_QSELF )
{
   HB_THREAD_STUB

   PHB_ITEM *  pBase    = HB_VM_STACK.pBase;
   LONG        lLevel   = hb_parnl( 1 );

   /* Outer function level. */
   pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;

   while( ( HB_IS_BLOCK( *( pBase + 1 ) ) || lLevel-- > 0 ) && pBase != HB_VM_STACK.pItems )
   {
      /* TraceLog( NULL, "Skipped: %s\n", ( *pBase )->item.asSymbol.value->szName );
       */

      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;
   }

   /* TraceLog( NULL, "Found: %s\n", ( *pBase )->item.asSymbol.value->szName );
    */

   hb_itemCopy( &( HB_VM_STACK.Return ), *( pBase + 1 ) );
}

HB_FUNC( __OPCOUNT ) /* it returns the total amount of opcodes */
{
   HB_THREAD_STUB_API

   hb_retnl( HB_P_LAST_PCODE - 1 );
}

/* profiler: It returns an array with an opcode called and
   consumed times { nTimes, nTime }, given the opcode index */
HB_FUNC( __OPGETPRF )
{
   HB_THREAD_STUB_API
#ifndef HB_NO_PROFILER
   ULONG ulOpcode = ( ULONG ) hb_parnl( 1 );

   hb_reta( 2 );

   if( ulOpcode < HB_P_LAST_PCODE )
   {
      hb_stornl( ( LONG ) hb_ulOpcodesCalls[ ulOpcode ], -1, 1 );
      hb_stornl( ( LONG ) hb_ulOpcodesTime[ ulOpcode ], -1, 2 );
   }
   else
#else
      hb_reta( 2 );
#endif
   {
      hb_stornl( 0, -1, 1 );
      hb_stornl( 0, -1, 2 );
   }
}

HB_FUNC( HB_SAVEBLOCK )
{
   HB_THREAD_STUB

   PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

   if( pBlock && pBlock->item.asBlock.value->uiLocals == 0 )
   {
      PSYMBOLS pModuleSymbols = HB_SYM_GETMODULESYM( pBlock->item.asBlock.value->symbol );

      hb_arrayNew( &( HB_VM_STACK.Return ), 7 );

      hb_arraySetC(  &( HB_VM_STACK.Return ), 1, pModuleSymbols ? pModuleSymbols->szModuleName : "" );
      hb_arraySetCL( &( HB_VM_STACK.Return ), 2, ( char * ) pBlock->item.asBlock.value->pCode, pBlock->item.asBlock.value->uLen );
      hb_arraySetNI( &( HB_VM_STACK.Return ), 3, pBlock->item.asBlock.paramcnt );
      hb_arraySetNI( &( HB_VM_STACK.Return ), 4, pBlock->item.asBlock.value->uiClass );
      hb_arraySetC(  &( HB_VM_STACK.Return ), 5, pBlock->item.asBlock.value->symbol->szName );
      hb_arraySetNI( &( HB_VM_STACK.Return ), 6, pBlock->item.asBlock.value->lineno );
      hb_arraySetNI( &( HB_VM_STACK.Return ), 7, pBlock->item.asBlock.statics );
   }
   else
      hb_errRT_BASE( EG_ARG, 9106, NULL, "Not a Codeblock, or Codeblock exports detached locals!", 1, hb_paramError( 1 ) );
}

HB_FUNC( HB_RESTOREBLOCK )
{
   HB_THREAD_STUB

   PHB_ITEM pBlockAsArray = hb_param( 1, HB_IT_ARRAY );

   if( pBlockAsArray && ( hb_arrayLen( pBlockAsArray ) == 4 || hb_arrayLen( pBlockAsArray ) == 7 ) )
   {
      HB_ITEM_NEW( ModuleName );
      HB_ITEM_NEW( PCode );
      HB_ITEM_NEW( ParamCount );
      HB_ITEM_NEW( ClassH );
      HB_ITEM_NEW( ProcName );
      HB_ITEM_NEW( ProcLine );
      HB_ITEM_NEW( lStatics );

      hb_arrayGet( pBlockAsArray, 1, &ModuleName );
      hb_arrayGet( pBlockAsArray, 2, &PCode );
      hb_arrayGet( pBlockAsArray, 3, &ParamCount );
      hb_arrayGet( pBlockAsArray, 4, &ClassH );
      hb_arrayGet( pBlockAsArray, 5, &ProcName );
      hb_arrayGet( pBlockAsArray, 6, &ProcLine );
      hb_arrayGet( pBlockAsArray, 7, &lStatics );

      if( HB_IS_STRING( &ModuleName ) && HB_IS_STRING( &PCode ) && HB_IS_INTEGER( &ParamCount ) && HB_IS_INTEGER( &ClassH ) )
      {
         PHB_ITEM *  pBase = hb_stackGetBase( 1 );
         HB_ITEM     Block;

         Block.type                          = HB_IT_BLOCK;
         Block.item.asBlock.value            = hb_codeblockMacroNew( ( BYTE * ) ( PCode.item.asString.value ), ( USHORT ) PCode.item.asString.length );
         Block.item.asBlock.value->uLen      = ( USHORT ) PCode.item.asString.length;
         Block.item.asBlock.value->uiClass   = ( USHORT ) ClassH.item.asInteger.value;
         Block.item.asBlock.paramcnt         = ( USHORT ) ParamCount.item.asInteger.value;

         if( HB_IS_STRING( &ProcName ) && HB_IS_INTEGER( &ProcLine ) && HB_IS_INTEGER( &lStatics ) )
         {
            Block.item.asBlock.value->symbol = hb_vmFindSymInModule( hb_vmFindModuleByName( ModuleName.item.asString.value ), ProcName.item.asString.value );
            Block.item.asBlock.value->lineno = ( USHORT ) ProcLine.item.asInteger.value;
            Block.item.asBlock.statics       = lStatics.item.asInteger.value;
         }
         else if( ( *( pBase + 1 ) )->type == HB_IT_BLOCK )
         {
            Block.item.asBlock.value->symbol = ( *( pBase + 1 ) )->item.asBlock.value->symbol;
            Block.item.asBlock.value->lineno = ( *( pBase + 1 ) )->item.asBlock.value->lineno;

            Block.item.asBlock.statics       = hb_stackGetStaticsBase();
         }
         else
         {
            Block.item.asBlock.value->symbol = ( *pBase )->item.asSymbol.value;
            Block.item.asBlock.value->lineno = ( *pBase )->item.asSymbol.pCargo->lineno;

            Block.item.asBlock.statics       = hb_stackGetStaticsBase();
         }

         /* TraceLog( NULL, "Proc: %s Line %i Self: %p\n", Block.item.asBlock.value->procname, Block.item.asBlock.value->lineno, Block.item.asBlock.value->pSelfBase );
          */

         hb_itemForwardValue( &( HB_VM_STACK.Return ), &Block );
      }
      else
         hb_errRT_BASE( EG_ARG, 9107, NULL, "Not a persisted Codeblock!", 1, hb_paramError( 1 ) );

      hb_itemClear( &ModuleName );
      hb_itemClear( &PCode );
      hb_itemClear( &ParamCount );
      hb_itemClear( &ClassH );
   }
   else
      hb_errRT_BASE( EG_ARG, 9108, NULL, "Not a persisted Codeblock!", 1, hb_paramError( 1 ) );
}

#if 0
HB_FUNC( HB_GUIERRORMESSAGE ) {;}
#endif

HB_FUNC( HB_NOMOUSE ) {;}

HB_FUNC( HB_NOSTARTUPWINDOW ) {;}

HB_FUNC( HB_RESETWITH )
{
   HB_THREAD_STUB

   if( hb_pcount() >= 1 )
      hb_itemForwardValue( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ), hb_stackItemFromBase( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 1607, NULL, "HB_RESETWITH", 0, NULL );
}

HB_FUNC( HB_WITHOBJECTCOUNTER )
{
   HB_THREAD_STUB

   hb_retnl( ( LONG ) HB_VM_STACK.wWithObjectCounter );
}

HB_FUNC( __TEXTINTO )
{
   HB_THREAD_STUB

   hb_itemCopy( hb_stackSelfItem(), & ( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ) );
   hb_symEval.value.pFunPtr();
}

HB_FUNC( HB_MULTITHREAD )
{
   HB_THREAD_STUB_API
#if defined( HB_THREAD_SUPPORT )
   hb_retl( TRUE );
#else
   hb_retl( FALSE );
#endif
}

HB_FUNC( HB_VMMODE )
{
   HB_THREAD_STUB_API
#if defined( HB_NO_PROFILER ) && defined( HB_NO_TRACE ) && ! defined( HB_GUI )
   /* optimized for console applications */
   hb_retni( 2 );
#elif defined( HB_NO_PROFILER ) && defined( HB_NO_TRACE ) && defined( HB_GUI )
   /* optimized for gui applications */
   hb_retni( 1 );
#else
   /* no optimization */
   hb_retni( 0 );
#endif
}

#undef HB_FORCE_LINK_MAIN

#if defined( HB_OS_WIN ) && ! defined( __EXPORT__ )

#  if 0
#     define HB_FORCE_LINK_MAIN  hb_forceLinkMainWin
#  endif

#elif defined( HB_OS_LINUX ) && defined( __WATCOMC__ )

#  define HB_FORCE_LINK_MAIN  hb_forceLinkMainStd

#endif

#ifdef HB_FORCE_LINK_MAIN
   HB_EXTERN_BEGIN
   extern void HB_FORCE_LINK_MAIN( void );
   HB_EXTERN_END
   void _hb_forceLinkMain() { HB_FORCE_LINK_MAIN(); }
#endif

#define HB_XVM_RETURN return ( hb_stackGetActionRequest() ? hb_xvmActionRequest() : FALSE );

static BOOL hb_xvmActionRequest( void )
{
   HB_THREAD_STUB_STACK

   if( hb_stackGetActionRequest() & ( HB_ENDPROC_REQUESTED | HB_BREAK_REQUESTED ) )
      return TRUE;
   else if( hb_stackGetActionRequest() & HB_QUIT_REQUESTED )
      exit( hb_vmQuit() );

   return FALSE;
}

void hb_xvmExitProc( void )
{
   HB_THREAD_STUB_STACK

   if( hb_stackGetActionRequest() & HB_ENDPROC_REQUESTED )
      hb_stackSetActionRequest( 0 );
}

void hb_xvmSeqBegin( void )
{
   HB_THREAD_STUB_STACK
   PHB_SEQUENCE pSequence;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSeqBegin()" ) );

   pSequence                           = ( PHB_SEQUENCE ) hb_xgrab( sizeof( HB_SEQUENCE ) );

   /* 1) clear the storage for value returned by BREAK statement */
   hb_stackAllocItem()->type           = HB_IT_NIL;

   /* 2) the address of RECOVER or END opcode - not used in C code */
   pSequence->lRecover                 = 0;

   /* 3) store current RECOVER base */
   pSequence->lBase                    = ( ULONG ) hb_stackTopOffset();

   /* 4) current bCanRecover flag - not used in C code */
   pSequence->uiStatus                 = 0;

   /* finally block address - not used in C code */
   pSequence->lFinally                 = 0;

   pSequence->wEnumCollectionCounter   = HB_VM_STACK.wEnumCollectionCounter;
   pSequence->wWithObjectCounter       = HB_VM_STACK.wWithObjectCounter;

   pSequence->pPrevErrBlock            = NULL;
   pSequence->pPrev                    = HB_VM_STACK.pSequence;
   HB_VM_STACK.pSequence               = pSequence;
}

BOOL hb_xvmSeqEnd( void )
{
   HB_THREAD_STUB_STACK
   PHB_SEQUENCE pFree;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSeqEnd()" ) );

   pFree = HB_VM_STACK.pSequence;

   /* remove all items placed on the stack after BEGIN code */
   hb_stackRemove( ( LONG ) HB_VM_STACK.pSequence->lBase );

   /* Discard the value returned by BREAK statement */
   hb_stackPop();

   HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;
   hb_xfree( ( void * ) pFree );

   if( hb_stackGetActionRequest() & HB_ENDPROC_REQUESTED )
      return TRUE;
   else if( hb_stackGetActionRequest() & HB_BREAK_REQUESTED )
      hb_stackSetActionRequest( 0 );
   else if( hb_stackGetActionRequest() & HB_QUIT_REQUESTED )
      exit( hb_vmQuit() );
   return FALSE;
}

BOOL hb_xvmSeqRecover( void )
{
   HB_THREAD_STUB_STACK
   PHB_SEQUENCE pFree;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSeqRecover()" ) );

   pFree = HB_VM_STACK.pSequence;

   /* remove all items placed on the stack after BEGIN code */
   hb_stackRemove( ( LONG ) HB_VM_STACK.pSequence->lBase );

   /*
    * Leave the value returned from BREAK
    * it will be popped in next executed opcode
    */

   HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;
   hb_xfree( pFree );

   if( hb_stackGetActionRequest() & HB_ENDPROC_REQUESTED )
      return TRUE;
   else if( hb_stackGetActionRequest() & HB_BREAK_REQUESTED )
      hb_stackSetActionRequest( 0 );
   else if( hb_stackGetActionRequest() & HB_QUIT_REQUESTED )
      exit( hb_vmQuit() );
   return FALSE;
}

void hb_xvmTryBegin( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmTryBegin()" ) );

   hb_vm_iTry++;
   hb_xvmSeqBegin();
   HB_VM_STACK.pSequence->pPrevErrBlock = hb_errorBlock( &hb_vm_BreakBlock );
}

BOOL hb_xvmTryEnd( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmTryEnd()" ) );

   hb_vm_iTry--;
   hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
   hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );
   return hb_xvmSeqEnd();
}

void hb_xvmTryEndFin( void )
{
   HB_THREAD_STUB_STACK
   USHORT uiActionRequest;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmTryEndFin()" ) );

   /* store requested action */
   uiActionRequest = hb_stackGetActionRequest();

   hb_vm_iTry--;
   hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
   hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );

   hb_xvmSeqRecover();

   /* Do we have outer sequence block? */
   if( HB_VM_STACK.pSequence )
   {
      /* move the value return by BREAK statement to outer sequence block */
      hb_itemForwardValue( hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 ),
                           hb_stackItemFromTop( -1 ) );
   }

   /* Discard the value returned by BREAK statement */
   hb_stackPop();

   /* restore requested action */
   hb_stackSetActionRequest( uiActionRequest );
}

BOOL hb_xvmTryRecover( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmTryRecover()" ) );

   hb_vm_iTry--;
   hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
   hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );

   return hb_xvmSeqRecover();
}

USHORT hb_xvmBeginFinally( void )
{
   HB_THREAD_STUB_STACK

   USHORT uiActionRequest = hb_stackGetActionRequest();

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmBeginFinally()" ) );

   hb_stackSetActionRequest( 0 );

   return uiActionRequest;
}

BOOL hb_xvmEndFinally( USHORT uiActionRequest )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEndFinally(%hu)", uiActionRequest ) );

   if( uiActionRequest )
   {
      /* TODO: we should decide here about priority of different exceptions */
      hb_stackSetActionRequest( uiActionRequest );
   }

   HB_XVM_RETURN
}

void hb_xvmSetLine( USHORT uiLine )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSetLine(%hu)", uiLine ) );

   hb_stackBaseItem()->item.asSymbol.pCargo->lineno = uiLine;
#ifndef HB_NO_DEBUG
   if( s_bDebugging )
      hb_vmDebuggerShowLine( uiLine );
#endif
}

void hb_xvmBaseLine( USHORT uiLine )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmBaseLine(%hu)", uiLine ) );

   s_iBaseLine = hb_stackBaseItem()->item.asSymbol.pCargo->lineno = uiLine;
#ifndef HB_NO_DEBUG
   if( s_bDebugging )
      hb_vmDebuggerShowLine( uiLine );
#endif
}

void hb_xvmLineOffset( BYTE bLine )
{
   HB_THREAD_STUB_STACK

   USHORT uiLineno = ( USHORT ) ( s_iBaseLine + bLine );

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLineOffset(%d)", bLine ) );

   hb_stackBaseItem()->item.asSymbol.pCargo->lineno = uiLineno;

#ifndef HB_NO_DEBUG
   if( s_bDebugging )
   {
      hb_vmDebuggerShowLine( uiLineno );
   }
#endif
}

BOOL hb_xvmClassSetModule( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pClassHandle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmClassSetModule()" ) );

   pClassHandle = hb_stackItemFromTop( -1 );

   if( HB_IS_INTEGER( pClassHandle ) )
      hb_clsSetModule( ( USHORT ) ( pClassHandle->item.asInteger.value ) );
   else
      hb_errRT_BASE( EG_ARG, 1603, NULL, "__ClsSetModule()", 1, pClassHandle );

   hb_stackPop(); /* pClassHandle */

   HB_XVM_RETURN
}

BOOL hb_xvmIVarRef( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pSelf, pMsg;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmIVarRef()" ) );

   pSelf = hb_stackItemFromTop( -1 );
   pMsg  = hb_stackItemFromTop( -2 );

   if( HB_IS_OBJECT( pSelf ) )
   {
      BOOL     bConstructor;
      BOOL     bSymbol;
      PHB_FUNC pFunc;

      pFunc = hb_objGetMthd( pSelf, pMsg->item.asSymbol.value, FALSE, &bConstructor, 2, &bSymbol );

      if( pFunc == hb___msgGetData )
      {
         if( ( HB_VM_STACK.pMethod )->uiData > ( USHORT ) pSelf->item.asArray.value->ulLen ) /* Resize needed ? */
            hb_arraySize( pSelf, ( HB_VM_STACK.pMethod )->uiData );                          /* Make large enough */

         hb_arrayGetByRef( pSelf, ( HB_VM_STACK.pMethod )->uiData, pMsg );
      }
      else if( pFunc == hb___msgGetClsData )
         hb_arrayGetByRef( hb_clsClassesArray()[ pSelf->item.asArray.value->uiClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiData, pMsg );
      else if( pFunc == hb___msgGetShrData )
      {
         if( ( HB_VM_STACK.pMethod )->uiSprClass )
            hb_arrayGetByRef( hb_clsClassesArray()[ ( HB_VM_STACK.pMethod )->uiSprClass - 1 ].pClassDatas, ( HB_VM_STACK.pMethod )->uiDataShared, pMsg );
      }
      else if( hb_stackGetActionRequest() != HB_BREAK_REQUESTED )
      {
         hb_vmSend( 0 );
         hb_itemPushForward( &( HB_VM_STACK.Return ) );

         HB_XVM_RETURN
      }
   }
   else if( HB_IS_HASH( pSelf ) )
   {
      const char *   szIndex = pMsg->item.asSymbol.value->szName;
      HB_SIZE        ulPos;

      if( strcmp( szIndex, "CLASSNAME" ) == 0 )
         hb_itemPutC( pMsg, "HASH" );
      else if( strcmp( szIndex, "CLASSH" ) == 0 )
         hb_itemPutNI( pMsg, 0 );
      else if( strcmp( szIndex, "KEYS" ) == 0 )
         hb_hashGetKeys( pMsg, pSelf );
      else if( strcmp( szIndex, "VALUES" ) == 0 )
         hb_hashGetValues( pMsg, pSelf );
      else
      {
         HB_ITEM_NEW( hbIndex );
         hb_itemPutCRawStatic( &hbIndex, szIndex, strlen( szIndex ) );

         if( hb_hashScan( pSelf, &hbIndex, &ulPos ) )
            hb_hashGet( pSelf, ulPos, pMsg );
         else
            hb_vmClassError( 0, "HASH", szIndex, pSelf );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_NOOBJECT, 1004, "Not object", pMsg->item.asSymbol.value->szName, 1, pSelf );
      hb_itemForwardValue( pMsg, &( HB_VM_STACK.Return ) );
   }

   hb_stackPop();

   HB_XVM_RETURN
}

void hb_xvmFrame( int iLocals, int iParams )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmFrame(%d, %d)", iLocals, iParams ) );

   hb_vmFrame( ( unsigned short ) iLocals, ( BYTE ) iParams );
}

void hb_xvmSFrame( PHB_SYMB pSymbol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSFrame(%p)", pSymbol ) );

   hb_vmSFrame( pSymbol );
}

BOOL hb_xvmDo( USHORT uiParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDo(%hu)", uiParams ) );

   hb_vmDo( uiParams );

   HB_XVM_RETURN
}

BOOL hb_xvmFunction( USHORT uiParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmFunction(%hu)", uiParams ) );

   hb_itemSetNil( hb_stackReturnItem() );
   hb_vmDo( uiParams );
   hb_stackPushReturn();

   HB_XVM_RETURN
}

BOOL hb_xvmSend( USHORT uiParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSend(%hu)", uiParams ) );

   hb_itemSetNil( hb_stackReturnItem() );
   hb_vmSend( uiParams );
   hb_stackPushReturn();

   HB_XVM_RETURN
}

BOOL hb_xvmSendWith( USHORT uiParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSendWith(%hu)", uiParams ) );

   hb_itemSetNil( hb_stackReturnItem() );
   hb_vmSend( uiParams );
   hb_stackPushReturn();

   HB_XVM_RETURN
}

void hb_xvmRetValue( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmRetValue()" ) );

   hb_stackPopReturn();
   hb_stackReturnItem()->type &= ~HB_IT_MEMOFLAG;
}

void hb_xvmStatics( PHB_SYMB pSymbol, USHORT uiStatics )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmStatics(%p,%hu)", pSymbol, uiStatics ) );

   hb_vmStatics( pSymbol, uiStatics );
}

void hb_xvmParameter( PHB_SYMB pSymbol, int iParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmParameter(%p,%d)", pSymbol, iParams ) );

   hb_memvarNewParameter( pSymbol, hb_stackItemFromBase( iParams ) );
}

void hb_xvmPushLocal( SHORT iLocal )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushLocal(%hd)", iLocal ) );

   hb_vmPushLocal( iLocal );
}

void hb_xvmPushLocalByRef( SHORT iLocal )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushLocalByRef(%hd)", iLocal ) );

   hb_vmPushLocalByRef( iLocal );
}

void hb_xvmPopLocal( SHORT iLocal )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopLocal(%hd)", iLocal ) );

   hb_vmPopLocal( iLocal );
}

void hb_xvmPushStatic( USHORT uiStatic )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushStatic(%hu)", uiStatic ) );

   hb_vmPushStatic( uiStatic );
}

void hb_xvmPushStaticByRef( USHORT uiStatic )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushStaticByRef(%hu)", uiStatic ) );

   hb_vmPushStaticByRef( uiStatic );
}

void hb_xvmPopStatic( USHORT uiStatic )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopStatic(%hu)", uiStatic ) );

   hb_vmPopStatic( uiStatic );
}

void hb_xvmPushGlobal( USHORT uiGlobal, PHB_ITEM ** pGlobals )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushGlobal(%hu)", uiGlobal ) );

   hb_vmPush( ( *pGlobals )[ uiGlobal ] );
}

void hb_xvmPushGlobalByRef( USHORT uiGlobal, PHB_ITEM ** pGlobals )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushGlobalByRef(%hu)", uiGlobal ) );

   pItem                                     = hb_stackAllocItem();
   pItem->type                               = HB_IT_BYREF;

   pItem->item.asRefer.value                 = uiGlobal + 1;   /* To offset the -1 below. */
   pItem->item.asRefer.offset                = -1;             /* Because 0 will be translated as a STATIC in hb_itemUnref(); */
   pItem->item.asRefer.BasePtr.itemsbasePtr  = pGlobals;

#ifdef HB_UNSHARE_REFERENCES
   hb_itemUnShare( ( *pGlobals )[ uiGlobal ] );
#endif
}

void hb_xvmPopGlobal( USHORT uiGlobal, PHB_ITEM ** pGlobals )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pTop = hb_stackItemFromTop( -1 );

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopGlobal(%hu)", uiGlobal ) );

   if( ( HB_IS_NUMBER( ( *pGlobals )[ uiGlobal ] ) && HB_IS_NUMBER( *( HB_VM_STACK.pPos - 1 ) ) ) ||
       ( ( *pGlobals )[ uiGlobal ] )->type == pTop->type )
      hb_itemForwardValue( ( *pGlobals )[ uiGlobal ], pTop );
   else if( hb_objGetOpOver( ( *pGlobals )[ uiGlobal ] ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmOperatorCall( ( *pGlobals )[ uiGlobal ], pTop, "__OPASSIGN", NULL, 0, ( *pGlobals )[ uiGlobal ] );
      hb_itemClear( pTop );
   }
   else
      hb_itemForwardValue( ( *pGlobals )[ uiGlobal ], pTop );

   hb_stackDec();
}

BOOL hb_xvmPushVariable( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushVariable(%p)", pSymbol ) );

   hb_vmPushVariable( pSymbol );

   HB_XVM_RETURN
}

BOOL hb_xvmPopVariable( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopVariable(%p)", pSymbol ) );

   hb_memvarSetValue( pSymbol, hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

void hb_xvmPushBlockShort( const BYTE * pCode, USHORT usSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushBlockShort(%p, %p)", pCode ) );

   hb_vmPushBlockShort( pCode, usSize, FALSE );
}

void hb_xvmPushBlock( const BYTE * pCode, USHORT usSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushBlock(%p, %p)", pCode ) );

   hb_vmPushBlock( pCode, usSize, FALSE );
}

void hb_xvmPushSelf( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushSelf()" ) );

   hb_vmPush( hb_stackSelfItem() );
}

BOOL hb_xvmPopLogical( BOOL * pfValue )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopLogical(%p)", pfValue ) );

   *pfValue = hb_vmPopLogical();

   HB_XVM_RETURN
}

BOOL hb_xvmPopAlias( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopAlias()" ) );

   hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ), NULL ); /* it clears the passed item */
   hb_stackDec();

   HB_XVM_RETURN
}

BOOL hb_xvmSwapAlias( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSwapAlias()" ) );

   hb_vmSwapAlias();

   HB_XVM_RETURN
}

BOOL hb_xvmPushField( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushField(%p)", pSymbol ) );

   hb_rddGetFieldValue( hb_stackAllocItem(), pSymbol );

   HB_XVM_RETURN
}

BOOL hb_xvmPushAlias( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushAlias()" ) );

   hb_vmPushAlias();

   HB_XVM_RETURN
}

BOOL hb_xvmPushAliasedField( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushAliasedField(%p)", pSymbol ) );

   hb_vmPushAliasedField( pSymbol );

   HB_XVM_RETURN
}

BOOL hb_xvmPushAliasedVar( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushAliasedVar(%p)", pSymbol ) );

   hb_vmPushAliasedVar( pSymbol );

   HB_XVM_RETURN
}

BOOL hb_xvmPopField( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopField(%p)", pSymbol ) );

   hb_rddPutFieldValue( hb_stackItemFromTop( -1 ), pSymbol );
   hb_stackPop();

   HB_XVM_RETURN
}

BOOL hb_xvmPushMemvar( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushMemvar(%p)", pSymbol ) );

   hb_memvarGetValue( hb_stackAllocItem(), pSymbol );

   HB_XVM_RETURN
}

BOOL hb_xvmPushMemvarByRef( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushMemvarByRef(%p)", pSymbol ) );

   hb_memvarGetRefer( hb_stackAllocItem(), pSymbol );

   HB_XVM_RETURN
}

BOOL hb_xvmPopMemvar( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopMemvar(%p)", pSymbol ) );

   hb_memvarSetValue( pSymbol, hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

BOOL hb_xvmPopAliasedField( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopAliasedField(%p)", pSymbol ) );

   hb_vmPopAliasedField( pSymbol );

   HB_XVM_RETURN
}

BOOL hb_xvmPopAliasedVar( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopAliasedVar(%p)", pSymbol ) );

   hb_vmPopAliasedVar( pSymbol );

   HB_XVM_RETURN
}

BOOL hb_xvmLocalAddInt( int iLocal, LONG lAdd )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalAddInt(%d,%ld)", iLocal, lAdd ) );

   hb_vmAddInt( hb_stackItemFromBase( iLocal ), lAdd );

   HB_XVM_RETURN
}

BOOL hb_xvmLocalAdd( int iLocal )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalAdd(%d)", iLocal ) );

   HB_STACK_OR_BLOCK_LOCAL( pLocal, iLocal );

   hb_vmPlus( pLocal, hb_stackItemFromTop( -1 ), pLocal );
   hb_stackPop();

   HB_XVM_RETURN
}

BOOL hb_xvmEqual( BOOL fExact )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEqual(%d)", ( int ) fExact ) );

   hb_vmEqual( fExact );

   HB_XVM_RETURN
}

BOOL hb_xvmAnd( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmAnd()" ) );

   hb_vmAnd();

   HB_XVM_RETURN
}

BOOL hb_xvmOr( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmOr()" ) );

   hb_vmOr();

   HB_XVM_RETURN
}

BOOL hb_xvmNot( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmNot()" ) );

   hb_vmNot();

   HB_XVM_RETURN
}

BOOL hb_xvmNegate( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmNegate()" ) );

   hb_vmNegate();

   HB_XVM_RETURN
}

BOOL hb_xvmPower( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPower()" ) );

   hb_vmPower();

   HB_XVM_RETURN
}

void hb_xvmDuplicate( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDuplicate()" ) );

   hb_vmDuplicate();
}

void hb_xvmDuplTwo( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDuplTwo()" ) );

   hb_vmDuplTwo();
}

BOOL hb_xvmForTest( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmForTest()" ) );

   hb_vmForTest();

   HB_XVM_RETURN
}

void hb_xvmFuncPtr( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmFuncPtr()" ) );

   hb_vmFuncPtr();
}

BOOL hb_xvmNotEqual( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEqual()" ) );

   hb_vmNotEqual();

   HB_XVM_RETURN
}

BOOL hb_xvmLess( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLess()" ) );

   hb_vmLess();

   HB_XVM_RETURN
}

BOOL hb_xvmLessEqual( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLessEqual()" ) );

   hb_vmLessEqual();

   HB_XVM_RETURN
}

BOOL hb_xvmGreater( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmGreater()" ) );

   hb_vmGreater();

   HB_XVM_RETURN
}

BOOL hb_xvmGreaterEqual( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmGreaterEqual()" ) );

   hb_vmGreaterEqual();

   HB_XVM_RETURN
}

BOOL hb_xvmInstring( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmInstring()" ) );

   hb_vmInstringOrArray();

   HB_XVM_RETURN
}

BOOL hb_xvmAddInt( LONG lAdd )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalAddInt(%ld)", lAdd ) );

   hb_vmAddInt( hb_stackItemFromTop( -1 ), lAdd );

   HB_XVM_RETURN
}

BOOL hb_xvmPlus( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPlus()" ) );

   hb_vmPlus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ),
              hb_stackItemFromTop( -2 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

BOOL hb_xvmMinus( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMinus()" ) );

   hb_vmMinus();

   HB_XVM_RETURN
}

BOOL hb_xvmMultByInt( LONG lValue )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMultByInt(%ld)", lValue ) );

   pValue = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pValue ) )
   {
      double   dValue;
      int      iDec;
      int      iType = ( int ) pValue->type;

      dValue = hb_itemGetNDDec( pValue, &iDec );

      hb_itemPutNumType( pValue, dValue * lValue, iDec, iType, HB_IT_INTEGER );
   }
   else if( hb_objGetOpOver( pValue ) & HB_CLASS_OP_MULT )
   {
      hb_vmPushLong( lValue );
      hb_vmOperatorCall( pValue, hb_stackItemFromTop( -1 ), "__OPMULT", NULL, 2, NULL );
      hb_itemPushForward( hb_stackReturnItem() );
   }
   else
   {
      PHB_ITEM pSubst;

      hb_vmPushLong( lValue );
      pSubst = hb_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*", 2, pValue, hb_stackItemFromTop( -1 ) );

      if( pSubst )
      {
         hb_stackPop();
         hb_itemForwardValue( pValue, pSubst );
         hb_itemRelease( pSubst );
      }
   }

   HB_XVM_RETURN
}

BOOL hb_xvmMult( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMult()" ) );

   hb_vmMult();

   HB_XVM_RETURN
}

BOOL hb_xvmDivideByInt( LONG lDivisor )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDivideByInt(%ld)", lDivisor ) );

   pValue = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pValue ) )
   {
      if( lDivisor == 0 )
      {
         PHB_ITEM pSubst;

         hb_vmPushLong( lDivisor );
         pSubst = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pValue, hb_stackItemFromTop( -1 ) );

         if( pSubst )
         {
            hb_stackPop();
            hb_itemForwardValue( pValue, pSubst );
            hb_itemRelease( pSubst );
         }
      }
      else
      {
         hb_itemPutNDDec( pValue, hb_itemGetND( pValue ) / lDivisor, hb_stackSetStruct()->HB_SET_DECIMALS );
      }
   }
   else if( hb_objGetOpOver( pValue ) & HB_CLASS_OP_DIVIDE )
   {
      hb_vmPushLong( lDivisor );
      hb_vmOperatorCall( pValue, hb_stackItemFromTop( -1 ), "__OPDIVIDE", NULL, 2, NULL );
      hb_itemPushForward( hb_stackReturnItem() );
   }
   else
   {
      PHB_ITEM pSubst;

      hb_vmPushLong( lDivisor );
      pSubst = hb_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/", 2, pValue, hb_stackItemFromTop( -1 ) );

      if( pSubst )
      {
         hb_stackPop();
         hb_itemForwardValue( pValue, pSubst );
         hb_itemRelease( pSubst );
      }
   }

   HB_XVM_RETURN
}

BOOL hb_xvmDivide( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDivide()" ) );

   hb_vmDivide();

   HB_XVM_RETURN
}

BOOL hb_xvmModulus( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmModulus()" ) );

   hb_vmModulus();

   HB_XVM_RETURN
}

BOOL hb_xvmInc( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmInc()" ) );

   hb_vmInc();

   HB_XVM_RETURN
}

BOOL hb_xvmDec( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDec()" ) );

   hb_vmDec();

   HB_XVM_RETURN
}

BOOL hb_xvmBitAnd( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmBitAnd()" ) );

   hb_vmBitAnd();

   HB_XVM_RETURN
}

BOOL hb_xvmBitOr( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmBitOr()" ) );

   hb_vmBitOr();

   HB_XVM_RETURN
}

BOOL hb_xvmBitXor( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmBitXor()" ) );

   hb_vmBitXor();

   HB_XVM_RETURN
}

BOOL hb_xvmBitShiftL( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmBitShiftL()" ) );

   hb_vmBitShiftLeft();

   HB_XVM_RETURN
}

BOOL hb_xvmBitShiftR( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmBitShiftR()" ) );

   hb_vmBitShiftRight();

   HB_XVM_RETURN
}

BOOL hb_xvmLeft( USHORT usLeft )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pString;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLeft(%hu)", usLeft ) );

   pString = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pString ) )
   {
      if( ( ULONG ) usLeft < pString->item.asString.length )
      {
         hb_itemPutCL( pString, pString->item.asString.value, usLeft );
      }
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushInteger( usLeft );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1124, NULL, "LEFT", 2, pString, hb_stackItemFromTop( -1 ) );
      if( pResult )
      {
         hb_stackPop();
         hb_itemForwardValue( pString, pResult );
         hb_itemRelease( pResult );
      }
   }

   HB_XVM_RETURN
}

BOOL hb_xvmRight( USHORT usRight )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pString;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLeft(%hu)", usRight ) );

   pString = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pString ) )
   {
      if( ( ULONG ) usRight < pString->item.asString.length )
      {
         hb_itemPutCL( pString, pString->item.asString.value + pString->item.asString.length - usRight, usRight );
      }
   }
   else
   {
      /* Clipper doesn't error */
      hb_itemPutC( pString, NULL );
   }

   HB_XVM_RETURN
}

BOOL hb_xvmLike( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pResult;
   BOOL     bLike;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLike()" ) );

   pResult                       = hb_stackItemFromTop( -2 );
   bLike                         = hb_regex( 1, hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );
   hb_stackPop();
   hb_itemClear( pResult );
   pResult->type                 = HB_IT_LOGICAL;
   pResult->item.asLogical.value = bLike;

   HB_XVM_RETURN
}

BOOL hb_xvmMatch( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pResult;
   BOOL     bMatch;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMatch()" ) );

   pResult                       = hb_stackItemFromTop( -2 );
   bMatch                        = hb_regex( 2, hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );
   hb_stackPop();
   hb_itemClear( pResult );
   pResult->type                 = HB_IT_LOGICAL;
   pResult->item.asLogical.value = bMatch;

   HB_XVM_RETURN
}

void hb_xvmArrayDim( USHORT uiDimensions )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayDim(%hu)", uiDimensions ) );

   hb_vmArrayDim( uiDimensions );
}

void hb_xvmArrayGen( ULONG ulElements )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayGen(%lu)", ulElements ) );

   hb_vmArrayGen( ulElements );
   HB_VM_STACK.iExtraElements = 0;
}

void hb_xvmHashGen( ULONG ulPairs )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmHAshGen(%lu)", ulPairs ) );

   hb_vmHashGen( ulPairs );
   HB_VM_STACK.iExtraElements = 0;
}

static void hb_vmArrayItemPush( HB_SIZE ulIndex )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pArray;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayItemPush(%lu)", ulIndex ) );

   pArray = hb_stackItemFromTop( -1 );

   if( hb_objGetOpOver( pArray ) & HB_CLASS_OP_ARRAYINDEX )
   {
      hb_vmPushNumInt( ulIndex );
      hb_vmOperatorCall( pArray, hb_stackItemFromTop( -1 ), "__OPARRAYINDEX", NULL, 2, NULL );
      hb_itemPushForward( hb_stackReturnItem() );
   }
   else if( HB_IS_ARRAY( pArray ) )
   {
      if( ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      {
         if( pArray->item.asArray.value->ulHolders > 1 )
         {
            /* this is a temporary copy of an array - we can overwrite
             * it with no problem
             */
            hb_itemCopy( pArray, pArray->item.asArray.value->pItems + ulIndex - 1 );
         }
         else
         {
            /* this is a constant array { 1, 2, 3 } - we cannot use
             * the optimization here
             */
            PHB_ITEM pItem = hb_stackAllocItem();

            hb_itemForwardValue( pItem, pArray->item.asArray.value->pItems + ulIndex - 1 );
            hb_itemForwardValue( pArray, pItem );
            hb_stackDec();
         }
      }
      else
      {
         hb_vmPushNumInt( ulIndex );
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, hb_stackItemFromTop( -1 ) );
      }
   }
#ifndef HB_C52_STRICT
   else if( HB_IS_STRING( pArray ) )
   {
      if( pArray->item.asString.length > 0 )
      {
         if( ulIndex > 0 )
         {
            ulIndex--;
         }
         if( ulIndex < pArray->item.asString.length )
         {
            hb_itemPutCLStatic( pArray, hb_szAscii[ ( UCHAR ) ( pArray->item.asString.value[ ulIndex ] ) ], 1 );
         }
         else
         {
            hb_itemPutCL( pArray, NULL, 0 );
         }
      }
   }
#endif
   else if( HB_IS_HASH( pArray ) )
   {
      hb_vmPushNumInt( ulIndex );
      /* Associative Array compatibility */
      if( hb_hashGetCompatibility( pArray ) )
      {
         ulIndex = hb_hashAAGetRealPos( pArray, ulIndex );
         if( ulIndex == 0 )
         {
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, hb_stackItemFromTop( -1 ) );
            return;
         }
      }
      else
      {
         /* Hash compatibility */
         if( ! hb_hashScan( pArray, hb_stackItemFromTop( -1 ), &ulIndex ) )
         {
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, hb_stackItemFromTop( -1 ) );
            return;
         }
      }
      hb_hashGet( pArray, ulIndex, hb_stackItemFromTop( -1 ) );
      hb_itemForwardValue( pArray, hb_stackItemFromTop( -1 ) );
      hb_stackDec();
   }
   else
   {
      hb_vmPushNumInt( ulIndex );
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, hb_stackItemFromTop( -1 ) );
   }
}

static void hb_vmArrayItemPop( ULONG ulIndex )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pValue;
   PHB_ITEM pArray;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayItemPop(%lu", ulIndex ) );

   pValue   = hb_stackItemFromTop( -2 );
   pArray   = hb_stackItemFromTop( -1 );

   if( HB_IS_BYREF( pArray ) )
      pArray = hb_itemUnRef( pArray );

   if( hb_objGetOpOver( pArray ) & HB_CLASS_OP_ARRAYINDEX )
   {
      hb_vmPushNumInt( ulIndex );
      hb_vmOperatorCall( pArray, hb_stackItemFromTop( -1 ), "__OPARRAYINDEX", pValue, 2, NULL );
      hb_itemPushForward( hb_stackReturnItem() );
   }
   else if( HB_IS_ARRAY( pArray ) )
   {
      if( ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      {
         pValue->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
         hb_itemForwardValue( pArray->item.asArray.value->pItems + ulIndex - 1, pValue );
         hb_stackPop();
         hb_stackDec(); /* value was moved above hb_stackDec() is enough */
      }
      else
      {
         hb_vmPushNumInt( ulIndex );
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, hb_stackItemFromTop( -1 ) );
      }
   }
#ifndef HB_C52_STRICT
   /* Only allowing assignment of strings (char) and numerics into String as Array. */
   else if( HB_IS_STRING( pArray ) && ( HB_IS_STRING( pValue ) || HB_IS_NUMERIC( pValue ) ) )
   {
      if( ulIndex > 0 )
         ulIndex--;
      if( ulIndex < pArray->item.asString.length )
      {
         BYTE bNewChar;

         /* pArray = pArray->item.asString.pOrigin; */
         if( pValue->type & HB_IT_STRING )
            bNewChar = ( BYTE ) pValue->item.asString.value[ 0 ];
         else if( pValue->type == HB_IT_INTEGER )
            bNewChar = ( BYTE ) pValue->item.asInteger.value;
         else if( pValue->type == HB_IT_LONG )
            bNewChar = ( BYTE ) pValue->item.asLong.value;
         else
            bNewChar = ( BYTE ) pValue->item.asDouble.value;

         if( pArray->item.asString.length == 1 )
            hb_itemPutCLStatic( pArray, hb_szAscii[ ( UCHAR ) bNewChar ], 1 );
         else if( pArray->item.asString.allocated == 0 || *( pArray->item.asString.pulHolders ) > 1 )
            hb_itemUnShare( pArray );

         pArray->item.asString.value[ ulIndex ] = ( char ) bNewChar;
         hb_stackPop();
         hb_stackPop();
      }
      else
      {
         hb_vmPushNumInt( ulIndex );
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, hb_stackItemFromTop( -1 ), pValue );
      }
   }
#endif /* HB_C52_STRICT */
   else
   {
      hb_vmPushNumInt( ulIndex );
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, hb_stackItemFromTop( -1 ), pValue );
   }
}

BOOL hb_xvmArrayPush( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayPush()" ) );

   hb_vmArrayPush();

   HB_XVM_RETURN
}

BOOL hb_xvmArrayPushRef( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayPushRef()" ) );

   hb_vmArrayPushRef();

   HB_XVM_RETURN
}

BOOL hb_xvmArrayItemPush( ULONG ulIndex )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayItemPush(%lu)", ulIndex ) );

   hb_vmArrayItemPush( ulIndex );

   HB_XVM_RETURN
}

BOOL hb_xvmArrayPop( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayPop()" ) );

   hb_vmArrayPop( HB_P_NOOP );

   HB_XVM_RETURN
}

BOOL hb_xvmArrayPopPlus( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayPopPlus()" ) );

   hb_vmArrayPop( HB_P_PLUS );

   HB_XVM_RETURN
}

BOOL hb_xvmArrayItemPop( ULONG ulIndex )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayItemPop(%lu)", ulIndex ) );

   hb_vmArrayItemPop( ulIndex );

   HB_XVM_RETURN
}

void hb_xvmPushDouble( double dNumber, int iWidth, int iDec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushDouble(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   hb_vmPushDoubleConst( dNumber, iWidth, iDec );
}

#ifdef HB_LONG_LONG_OFF
void hb_xvmPushLongLong( double dNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushLongLong(%l.0f)", dNumber ) );

   hb_vmPushDoubleConst( dNumber, HB_DEFAULT_WIDTH, 0 );
}
#else
void hb_xvmPushLongLong( LONGLONG llNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushLongLong(%" PFLL "i)", llNumber ) );

   hb_vmPushLongLongConst( llNumber );
}
#endif /* HB_LONG_LONG_OFF */

void hb_xvmLocalName( USHORT uiLocal, char * szLocalName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalName(%hu, %s)", uiLocal, szLocalName ) );

#ifndef HB_NO_DEBUG
   hb_vmLocalName( uiLocal, szLocalName );
#else
   HB_SYMBOL_UNUSED( uiLocal );
   HB_SYMBOL_UNUSED( szLocalName );
#endif /* HB_NO_DEBUG */
}

void hb_xvmStaticName( BYTE bIsGlobal, USHORT uiStatic, char * szStaticName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmStaticName(%d, %hu, %s)", ( int ) bIsGlobal, uiStatic, szStaticName ) );

   HB_SYMBOL_UNUSED( bIsGlobal );
#ifndef HB_NO_DEBUG
   hb_vmStaticName( uiStatic, szStaticName );
#else
   HB_SYMBOL_UNUSED( uiStatic );
   HB_SYMBOL_UNUSED( szStaticName );
#endif /* HB_NO_DEBUG */
}

void hb_xvmModuleName( char * szModuleName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmModuleName(%s)", szModuleName ) );
#ifndef HB_NO_DEBUG
   hb_vmModuleName( szModuleName );
#else
   HB_SYMBOL_UNUSED( szModuleName );
#endif /* HB_NO_DEBUG */
}

void hb_xvmMacroList( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroList()" ) );

   HB_VM_STACK.aiExtraElements[ HB_VM_STACK.iExtraElementsIndex++ ] = 0;
}

void hb_xvmMacroListEnd( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroListEnd()" ) );

   HB_VM_STACK.iExtraElements = HB_VM_STACK.aiExtraElements[ --HB_VM_STACK.iExtraElementsIndex ];
}

BOOL hb_xvmMacroPush( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPush(%d)", bFlags ) );

   hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, bFlags );

   HB_XVM_RETURN
}

BOOL hb_xvmPushMacroRef( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pTop;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushMacroRef()" ) );

   pTop = hb_stackItemFromTop( -1 );
   hb_memvarGetRefer( pTop, pTop->item.asSymbol.value );

   HB_XVM_RETURN
}

BOOL hb_xvmMacroPushIndex( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushIndex(%d)", bFlags ) );

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHINDEX, bFlags );

   if( HB_VM_STACK.iExtraIndex )
   {
      HB_ITEM *   aExtraItems = ( HB_ITEM * ) hb_xgrab( sizeof( HB_ITEM ) * HB_VM_STACK.iExtraIndex );
      int         i;

      /* Storing and removing the extra indexes. */
      for( i = HB_VM_STACK.iExtraIndex - 1; i >= 0; i-- )
      {
         ( aExtraItems + i )->type = HB_IT_NIL;
         hb_itemRawMove( aExtraItems + i, hb_stackItemFromTop( -1 ) );
         hb_stackDec();
      }

      /* First index is still on stack.*/
      hb_vmArrayPush();

      /* Now process each of the additional index.
       * Do not process the last one which will be processes by the
       * HB_P_ARRAYPUSH which is know to follow
       */
      for( i = 0; i < HB_VM_STACK.iExtraIndex; i++ )
      {
         hb_vmPush( aExtraItems + i );

         if( HB_IS_COMPLEX( aExtraItems + i ) )
            hb_itemClear( aExtraItems + i );

         if( i < HB_VM_STACK.iExtraIndex - 1 )
            hb_vmArrayPush();
      }

      hb_xfree( aExtraItems );
   }

   HB_XVM_RETURN
}

BOOL hb_xvmMacroPushArg( PHB_SYMB pSymbol, BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushArg(%p, %d)", pSymbol, bFlags ) );

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHARG, bFlags );

   if( HB_VM_STACK.iExtraParamsIndex && HB_VM_STACK.apExtraParamsSymbol[ HB_VM_STACK.iExtraParamsIndex - 1 ] == NULL )
      HB_VM_STACK.apExtraParamsSymbol[ HB_VM_STACK.iExtraParamsIndex - 1 ] = pSymbol;

   HB_XVM_RETURN
}

BOOL hb_xvmMacroPushList( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushList(%d)", bFlags ) );

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHLIST, bFlags );

   HB_XVM_RETURN
}

BOOL hb_xvmMacroPushPare( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushPare(%d)", bFlags ) );

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHPARE, bFlags );

   HB_XVM_RETURN
}

BOOL hb_xvmMacroPushAliased( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushAliased(%d)", bFlags ) );

   hb_macroPushAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), bFlags );

   HB_XVM_RETURN
}

BOOL hb_xvmMacroPop( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPop(%d)", bFlags ) );

   hb_macroSetValue( hb_stackItemFromTop( -1 ), bFlags );

   HB_XVM_RETURN
}

BOOL hb_xvmMacroPopAliased( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPopAliased(%d)", bFlags ) );

   hb_macroPopAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), bFlags );

   HB_XVM_RETURN
}

BOOL hb_xvmMacroSymbol( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroSymbol()" ) );

   hb_macroPushSymbol( hb_stackItemFromTop( -1 ) );

   HB_XVM_RETURN
}

BOOL hb_xvmMacroText( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroText()" ) );

   hb_macroTextValue( hb_stackItemFromTop( -1 ) );

   HB_XVM_RETURN
}

void hb_xvmLocalSetInt( int iLocal, int iVal )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalSetInt(%d, %d)", iLocal, iVal ) );

   HB_STACK_OR_BLOCK_LOCAL( pLocal, iLocal );

   if( ( ! HB_IS_NUMBER( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmPushInteger( iVal );
      hb_vmOperatorCall( pLocal, hb_stackItemFromTop( -1 ), "__OPASSIGN", NULL, 1, pLocal );
   }
   else
      hb_itemPutNI( pLocal, iVal );
}

void hb_xvmLocalSetStr( int iLocal, const char * pVal, ULONG ulLen )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalSetInt(%d,%p,%lu)", iLocal, pVal, ulLen ) );

   HB_STACK_OR_BLOCK_LOCAL( pLocal, iLocal );

   if( ( ! HB_IS_STRING( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
   {
      hb_itemPushStaticString( ( char * ) pVal, ulLen );
      hb_vmOperatorCall( pLocal, hb_stackItemFromTop( -1 ), "__OPASSIGN", NULL, 1, pLocal );
   }
   else
      hb_itemPutCRawStatic( pLocal, ( char * ) pVal, ulLen );
}

BOOL hb_xvmSwitchCase( LONG lCase )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pTop;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSwitchCase(%ld)", lCase ) );

   pTop = hb_stackItemFromTop( -1 );

   if( pTop->type & HB_IT_INTEGER )
      hb_vmPushLogical( ( LONG ) ( pTop->item.asInteger.value ) == lCase );
   else if( pTop->type & HB_IT_LONG )
      hb_vmPushLogical( ( LONG ) pTop->item.asLong.value == lCase );
   else if( pTop->type & HB_IT_STRING && pTop->item.asString.length == 1 )
      hb_vmPushLogical( ( LONG ) ( ( BYTE ) pTop->item.asString.value[ 0 ] ) == lCase );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1604, NULL, "SWITCH", 1, pTop );
      if( pResult )
      {
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }

   HB_XVM_RETURN
}

void hb_xvmPushWith( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushWith()" ) );

   if( HB_VM_STACK.wWithObjectCounter )
      hb_vmPush( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ) );
   else
      hb_vmPushNil();
}

BOOL hb_xvmWithObject( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmWithObject()" ) );

   hb_itemForwardValue( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter++ ] ), hb_stackItemFromTop( -1 ) );

   if( HB_VM_STACK.wWithObjectCounter == HB_MAX_WITH_OBJECTS )
   {
      hb_errInternal( HB_EI_ERRUNRECOV, "WITH OBJECT excessive nesting!", NULL, NULL );

#if 0
      while( HB_VM_STACK.wWithObjectCounter )
      {
         --HB_VM_STACK.wWithObjectCounter;
         hb_itemClear( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter ] ) );
      }
#endif
   }
   hb_stackPop();

   HB_XVM_RETURN
}

BOOL hb_xvmEndWithObject( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEndWithObject()" ) );

   if( HB_VM_STACK.wWithObjectCounter )
      hb_itemClear( &( HB_VM_STACK.aWithObject[ --HB_VM_STACK.wWithObjectCounter ] ) );

   HB_XVM_RETURN
}

void hb_xvmEnumIndex( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pTop;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEnumIndex()" ) );

   pTop        = hb_stackAllocItem();
   pTop->type  = HB_IT_LONG;

   if( HB_VM_STACK.wEnumCollectionCounter )
      pTop->item.asLong.value = HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter - 1 ];
   else
      pTop->item.asLong.value = 0;

   pTop->item.asLong.length = 10;
}

BOOL hb_xvmForEach( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pEnumeration;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmForEach()" ) );

   pEnumeration = &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] );
   hb_itemForwardValue( pEnumeration, hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   if( hb_objGetOpOver( pEnumeration ) & HB_CLASS_OP_FOREACH )
   {
      HB_ITEM_NEW( ForEachOp );

      hb_itemPutNI( &ForEachOp, FOREACH_BEGIN );

      hb_vmOperatorCall( pEnumeration, &ForEachOp, "__OPFOREACH", NULL, 0, pEnumeration );
   }
   else if( HB_IS_ARRAY( pEnumeration ) || HB_IS_STRING( pEnumeration ) )
   {
      /* No prep needed. */
   }
   else if( HB_IS_HASH( pEnumeration ) && hb_hashGetCompatibility( pEnumeration ) )
   {
      /* No prep needed. */
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1602, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pEnumeration, hb_itemPutNI( *HB_VM_STACK.pPos, 1 ) );
   }

   HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] = hb_itemUnRef( hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   HB_VM_STACK.wEnumCollectionCounter++;

   if( HB_VM_STACK.wEnumCollectionCounter == HB_MAX_ENUMERATIONS )
   {
      hb_errInternal( HB_EI_ERRUNRECOV, "FOR EACH excessive nesting!", NULL, NULL );

#if 0
      /* Release ALL FOR EACH. */
      while( HB_VM_STACK.wEnumCollectionCounter )
      {
         HB_VM_STACK.wEnumCollectionCounter--;
         hb_itemClear( pEnumeration );
         HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;
      }
#endif
   }

   HB_XVM_RETURN
}

BOOL hb_xvmEnumerate( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEnumerate()" ) );

   if( HB_VM_STACK.wEnumCollectionCounter )
   {
      PHB_ITEM pEnumeration   = &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter - 1 ] );
      PHB_ITEM pEnumerator    = HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter - 1 ];
      HB_SIZE  ulEnumIndex    = ++HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter - 1 ];

      if( hb_objGetOpOver( pEnumeration ) & HB_CLASS_OP_FOREACH )
      {
         HB_ITEM_NEW( ForEachOp );
         HB_ITEM_NEW( ForEachIndex );

         hb_itemPutNI( &ForEachOp, FOREACH_ENUMERATE );
         hb_itemPutNL( &ForEachIndex, ( LONG ) ulEnumIndex );

         hb_vmOperatorCall( pEnumeration, &ForEachOp, "__OPFOREACH", &ForEachIndex, 0, pEnumerator );

         if( hb_stackGetActionRequest() == HB_BREAK_REQUESTED )
         {
            hb_stackSetActionRequest( 0 );
            hb_vmPushLogical( FALSE );
         }
         else
            hb_vmPushLogical( TRUE );
      }
      else if( HB_IS_ARRAY( pEnumeration ) || HB_IS_STRING( pEnumeration ) )
         hb_vmPushLogical( hb_arrayGetByRef( pEnumeration, ulEnumIndex, pEnumerator ) );
      else if( HB_IS_HASH( pEnumeration ) && hb_hashGetCompatibility( pEnumeration ) )
      {
         ulEnumIndex = hb_hashAAGetRealPos( pEnumeration, ulEnumIndex );

         if( ulEnumIndex )
         {
            hb_hashGet( pEnumeration, ulEnumIndex, pEnumerator );
            hb_vmPushLogical( TRUE );
         }
         else
            hb_vmPushLogical( FALSE );
      }
      else
         hb_vmPushLogical( FALSE );
   }
   else
      hb_vmPushLogical( FALSE );

   HB_XVM_RETURN
}

BOOL hb_xvmEndEnumerate( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEndEnumerate()" ) );

   if( HB_VM_STACK.wEnumCollectionCounter )
   {
      --HB_VM_STACK.wEnumCollectionCounter;

      if( hb_objGetOpOver( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ) ) & HB_CLASS_OP_FOREACH )
      {
         HB_ITEM_NEW( ForEachOp );

         hb_itemPutNI( &ForEachOp, FOREACH_END );

         hb_vmOperatorCall( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ), &ForEachOp, "__OPFOREACH", NULL, 0, NULL );
         hb_itemClear( &HB_VM_STACK.Return );
      }
   }

   hb_itemClear( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ) );
   HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;

   /* Incase EXIT was used. */
   hb_itemClear( HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] );

   HB_XVM_RETURN
}

void hb_xvmLocalSetStringHidden( int iLocal, BYTE bType, ULONG ulSize, const char * pVal, ULONG ulBufferSize )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pLocal;
   BYTE *   pBuffer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalSetStringHidden(%d,%d,%lu,%p,%lu)", iLocal, bType, ulSize, pVal, ulBufferSize ) );

   HB_STACK_OR_BLOCK_LOCAL( pLocal, iLocal );

   pBuffer = hb_vmUnhideString( bType, ulSize, ( const BYTE * ) pVal, ulBufferSize );

   if( ( ! HB_IS_STRING( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmPushString( ( char * ) pBuffer, ulSize - 1 );
      hb_vmOperatorCall( pLocal, *( HB_VM_STACK.pPos - 1 ), "__OPASSIGN", NULL, 1, pLocal );
   }
   else
      hb_itemPutCPtr( pLocal, ( char * ) pBuffer, ulSize - 1 );
}

void hb_xvmPushStringHidden( BYTE bType, ULONG ulSize, const char * pVal, ULONG ulBufferSize )
{
   BYTE * pBuffer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalSetStringHidden(%d,%lu,%p,%lu)", bType, ulSize, pVal, ulBufferSize ) );

   pBuffer = hb_vmUnhideString( bType, ulSize, ( const BYTE * ) pVal, ulBufferSize );

   hb_vmPushString( ( char * ) pBuffer, ulSize - 1 );
}

void hb_xvmDivert( BOOL bDivertOf )
{
   hb_vmSetDivert( bDivertOf );
}

/* ------------------------------ */
/* The debugger support functions */
/* ------------------------------ */
void hb_vmRequestDebug( void )
{
#ifndef HB_NO_DEBUG
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestDebug()" ) );
   s_bDebugRequest = TRUE;
#endif /* HB_NO_DEBUG */
}

BOOL hb_dbg_InvokeDebug( BOOL bInvoke )
{
#ifndef HB_NO_DEBUG
   BOOL bRequest = s_bDebugRequest;

   s_bDebugRequest = bInvoke;
   return bRequest;
#else
   HB_SYMBOL_UNUSED( bInvoke );
   return FALSE;
#endif /* HB_NO_DEBUG */
}

HB_DBGENTRY_FUNC hb_dbg_SetEntry( HB_DBGENTRY_FUNC pFunDbgEntry )
{
#ifndef HB_NO_DEBUG
   HB_DBGENTRY_FUNC pPrevFunc = s_pFunDbgEntry;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbg_SetEntry(%p)", pFunDbgEntry ) );

   s_pFunDbgEntry = pFunDbgEntry;
   return pPrevFunc;
#else
   HB_SYMBOL_UNUSED( pFunDbgEntry );
   return NULL;
#endif /* HB_NO_DEBUG */
}

PHB_ITEM hb_dbg_vmVarSGet( int nStatic, int nOffset )
{
   return hb_arrayGetItemPtr( &s_aStatics, nStatic + nOffset );
}

ULONG hb_dbg_ProcLevel( void )
{
#ifndef HB_NO_DEBUG
   return s_ulProcLevel;
#else
   return 0;
#endif /* HB_NO_DEBUG */
}

/*
 * check if the debugger activation was requested or request the debugger
 * activation if .T. is passed
 */
HB_FUNC( HB_DBG_INVOKEDEBUG )
{
   HB_THREAD_STUB_API

#ifndef HB_NO_DEBUG
   BOOL bRequest = s_bDebugRequest;

   if( hb_pcount() > 0 )
      s_bDebugRequest = hb_parl( 1 );
   else
      s_bDebugRequest = FALSE;

   hb_retl( bRequest );
#else
   hb_retl( FALSE );
#endif /* HB_NO_DEBUG */
}

/* $Doc$
 * $FuncName$     <aStat> hb_dbg_vmVarSList()
 * $Description$  Return a clone of the statics array.
 * $End$ */
HB_FUNC( HB_DBG_VMVARSLIST )
{
   PHB_ITEM pStatics = hb_arrayClone( &s_aStatics, NULL );

   hb_itemRelease( hb_itemReturnForward( pStatics ) );
}

/* $Doc$
 * $FuncName$     <nStatics> hb_dbg_vmVarSLen()
 * $Description$  Return the statics array length.
 * $End$ */
HB_FUNC( HB_DBG_VMVARSLEN )
{
   HB_THREAD_STUB

   hb_retns( hb_arrayLen( &s_aStatics ) );
}

/* $Doc$
 * $FuncName$     <xStat> hb_dbg_vmVarSGet(<nStatic>,<nOffset>)
 * $Description$  Return a specified statics
 * $End$ */
HB_FUNC( HB_DBG_VMVARSGET )
{
   hb_itemReturn( hb_dbg_vmVarSGet( hb_parni( 1 ), hb_parni( 2 ) ) );
}

/* $Doc$
 * $FuncName$     hb_dbg_vmVarSSet(<nStatic>,<nOffset>,<uValue>)
 * $Description$  Sets the value of a specified statics
 * $End$ */
HB_FUNC( HB_DBG_VMVARSSET )
{
   PHB_ITEM pItem = hb_param( 3, HB_IT_ANY );

   if( pItem )
      hb_arraySet( &s_aStatics, hb_parni( 1 ) + hb_parni( 2 ), pItem );
}

HB_FUNC( HB_DBG_PROCLEVEL )
{
   HB_THREAD_STUB_API

#ifndef HB_NO_DEBUG
   hb_retnl( ( LONG ) s_ulProcLevel - 1 ); /* Don't count self */
#else
   hb_retnl( 0 );
#endif /* HB_NO_DEBUG */
}

HB_SIZE hb_dbg_vmVarGCount( void )
{
   return hb_arrayLen( &s_aGlobals );
}

PHB_ITEM hb_dbg_vmVarGGet( int nGlobal, int nOffset )
{
   return hb_arrayGetItemPtr( &s_aGlobals, nGlobal + nOffset );
}

/* $Doc$
 * $FuncName$     <aStat> __vmVarGList()
 * $Description$  Return a clone of the globals array.
 * $End$ */
HB_FUNC( HB_DBG_VMVARGLIST )
{
   PHB_ITEM pGlobals = hb_arrayClone( &s_aGlobals, NULL );

   hb_itemRelease( hb_itemReturnForward( pGlobals ) );
}

HB_FUNC( HB_DBG_VMVARGGET )
{
   hb_itemReturn( hb_dbg_vmVarGGet( hb_parni( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( HB_DBG_VMVARGSET )
{
   PHB_ITEM pItem = hb_param( 3, HB_IT_ANY );

   if( pItem )
      hb_arraySet( &s_aGlobals, hb_parni( 1 ) + hb_parni( 2 ), pItem );
}

HB_FUNC( __VMVARGLIST )
{
   HB_FUNC_EXEC( HB_DBG_VMVARGLIST );
}

HB_FUNC( __VMVARSLIST )
{
   HB_FUNC_EXEC( HB_DBG_VMVARSLIST );
}

HB_FUNC( __VMVARSLEN )
{
   HB_FUNC_EXEC( HB_DBG_VMVARSLEN );
}

HB_FUNC( __VMVARSGET )
{
   HB_FUNC_EXEC( HB_DBG_VMVARSGET );
}

HB_FUNC( __VMVARSSET )
{
   HB_FUNC_EXEC( HB_DBG_VMVARSSET );
}

void hb_vmPushSize( HB_ISIZ nNumber )
{
#if HB_SIZE_MAX <= HB_UINT_MAX
   hb_vmPushInteger( ( int ) nNumber );
#else
   if( HB_LIM_INT( nNumber ) )
      hb_vmPushInteger( ( int ) nNumber );
   else
      hb_vmPushHBLong( nNumber );
#endif /* HB_SIZE_MAX <= HB_UINT_MAX */
}
