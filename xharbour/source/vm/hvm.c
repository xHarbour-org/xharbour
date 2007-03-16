/*
 * $Id: hvm.c,v 1.602 2007/03/04 13:37:52 druzus Exp $
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
 * See doc/license.txt for licensing terms.
 *
 */

/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#include <math.h>
#include <time.h>
#include <ctype.h>
#include <assert.h>

#include "hbvmopt.h"
#include "hbxvm.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapirdd.h"
#include "hbapigt.h"
#include "hbvm.h"
#include "hbpcode.h"
#include "hbset.h"
#include "inkey.ch"
#include "classes.h"
#include "hboo.ch"
#include "hbdebug.ch"

#include "hbi18n.h"
#include "hbserv.h"
#include "hashapi.h"

#include "hbmemory.ch"

#ifdef HB_MACRO_STATEMENTS
   #include "hbpp.h"
#endif

#ifndef HB_CDP_SUPPORT_OFF
   #include "hbapicdp.h"
#endif

#if !defined( HB_OS_DOS ) && !defined( HB_OS_DARWIN_5 )
   #include "hbserv.h"
#endif

#ifdef HB_VM_REQUESTS
   HB_VM_REQUESTS
#endif

#if defined(HB_OS_WIN_32)
   /* Mouse Disabling */
   extern BOOL b_MouseEnable;

   #include <windows.h>
   #include <ole2.h>

   static BOOL s_bUnInitOle = FALSE;
#endif

PHB_FUNC pHVMFuncService = NULL;

static void hb_vmClassError( int uiParams, char *szClassName, char *szMsg, PHB_ITEM pSelf );

HB_FUNC_EXTERN( SYSINIT );

/* PCode functions */

/* Operators (mathematical / character / misc) */
static void     hb_vmNegate( void );          /* negates (-) the latest value on the stack */
static void     hb_vmAddInt( HB_ITEM_PTR pResult, LONG lAdd );      /* add integer to given item */
static void     hb_vmPlus( PHB_ITEM pLeft, PHB_ITEM pRight, PHB_ITEM pResult ); /* sums the latest two values on the stack, removes them and leaves the result */
static void     hb_vmMinus( void );           /* substracts the latest two values on the stack, removes them and leaves the result */
static void     hb_vmMult( void );            /* multiplies the latest two values on the stack, removes them and leaves the result */
static void     hb_vmDivide( void );          /* divides the latest two values on the stack, removes them and leaves the result */
static void     hb_vmModulus( void );         /* calculates the modulus of latest two values on the stack, removes them and leaves the result */
static void     hb_vmPower( void );           /* power the latest two values on the stack, removes them and leaves the result */
static void     hb_vmInc( void );             /* increment the latest numeric value on the stack */
static void     hb_vmDec( void );             /* decrements the latest numeric value on the stack */
static void     hb_vmFuncPtr( void );         /* pushes a function address pointer. Removes the symbol from the satck */

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
static void    hb_vmBitShiftLeft( void );     /* performs the bit left shift on the latest two values, removes them and leaves result on the stack */
static void    hb_vmBitShiftRight( void );    /* performs the bit right shift on the latest two values, removes them and leaves result on the stack */

/* Array */
static void    hb_vmArrayPush( void );       /* pushes an array element to the stack, removing the array and the index from the stack */
static void    hb_vmArrayPushRef( void );
static void    hb_vmArrayPop( HB_PCODE pcode );      /* pops a value from the stack */
static void    hb_vmArrayDim( USHORT uiDimensions ); /* generates an uiDimensions Array and initialize those dimensions from the stack values */
static void    hb_vmArrayGen( ULONG ulElements ); /* generates an ulElements Array and fills it from the stack values */
static void    hb_vmArrayNew( HB_ITEM_PTR, USHORT ); /* creates array */

/* Object */
void    hb_vmOperatorCall( PHB_ITEM, PHB_ITEM, char *, PHB_ITEM, int, PHB_ITEM ); /* call an overloaded operator */
void    hb_vmOperatorCallUnary( PHB_ITEM, char *, PHB_ITEM ); /* call an overloaded unary operator */

/* Database */
static ERRCODE hb_vmSelectWorkarea( PHB_ITEM, PHB_SYMB );  /* select the workarea using a given item or a substituted value */
static void    hb_vmSwapAlias( void );           /* swaps items on the eval stack and pops the workarea number */

/* Execution */
static HARBOUR hb_vmDoBlock( void );             /* executes a codeblock */
static void    hb_vmDebuggerExit( void );        /* shuts down the debugger */
static void    hb_vmLocalName( USHORT uiLocal, char * szLocalName ); /* locals and parameters index and name information for the debugger */
// static void    hb_vmStaticName( BYTE bIsGlobal, USHORT uiStatic, char * szStaticName ); /* statics vars information for the debugger */
static void    hb_vmStaticName( USHORT uiStatic, char * szStaticName ); /* statics vars information for the debugger */
static void    hb_vmModuleName( char * szModuleName ); /* PRG and function name information for the debugger */
static void    hb_vmFrame( unsigned short iLocals, BYTE bParams ); /* increases the stack pointer for the amount of locals and params suplied */
static void    hb_vmSFrame( PHB_SYMB pSym );     /* sets the statics frame for a function */
static void    hb_vmStatics( PHB_SYMB pSym, USHORT uiStatics ); /* increases the global statics array to hold a PRG statics */
static void    hb_vmEndBlock( void );            /* copies the last codeblock pushed value into the return value */
static void    hb_vmRetValue( void );            /* pops the latest stack value into stack.Return */
static void    hb_vmDebuggerShowLine( USHORT uiLine ); /* makes the debugger shows a specific source code line */
static void    hb_vmDebuggerEndProc( void );     /* notifies the debugger for an endproc */

/* Push */
static void    hb_vmPushAlias( void );            /* pushes the current workarea number */
static void    hb_vmPushAliasedField( PHB_SYMB ); /* pushes an aliased field on the eval stack */
static void    hb_vmPushAliasedVar( PHB_SYMB );   /* pushes an aliased variable on the eval stack */
static void    hb_vmPushBlock( const BYTE * pCode, USHORT usSize, BOOL bDynCode, PHB_SYMB pSymbols, PHB_ITEM** pGlobals ); /* creates a codeblock */
static void    hb_vmPushBlockShort( const BYTE * pCode, USHORT usSize, BOOL bDynCode, PHB_SYMB pSymbols, PHB_ITEM** pGlobals ); /* creates a codeblock */
static void    hb_vmPushDoubleConst( double dNumber, int iWidth, int iDec ); /* Pushes a double constant (pcode) */
static void    hb_vmPushMacroBlock( BYTE * pCode, PHB_SYMB pSymbols ); /* creates a macro-compiled codeblock */
static void    hb_vmPushLocal( SHORT iLocal );    /* pushes the containts of a local onto the stack */
static void    hb_vmPushLocalByRef( SHORT iLocal );    /* pushes a local by refrence onto the stack */
static void    hb_vmPushLongConst( LONG lNumber );  /* Pushes a LONG constant (pcode) */
static void    hb_vmPushHBLong( HB_LONG lNumber ); /* pushes a HB_LONG number onto the stack */
#if !defined( HB_LONG_LONG_OFF )
   static void hb_vmPushLongLongConst( LONGLONG lNumber );  /* Pushes a LONGLONG constant (pcode) */
#endif
#if HB_INT_MAX >= INT32_MAX
static void    hb_vmPushIntegerConst( int iNumber );  /* Pushes a int constant (pcode) */
#endif
static void    hb_vmPushNumInt( HB_LONG lNumber );     /* pushes a number on to the stack and decides if it is integer or HB_LONG */
static void    hb_vmPushStatic( USHORT uiStatic );     /* pushes the containts of a static onto the stack */
static void    hb_vmPushStaticByRef( USHORT uiStatic ); /* pushes a static by refrence onto the stack */
static void    hb_vmPushVariable( PHB_SYMB pVarSymb ); /* pushes undeclared variable */
static void    hb_vmDuplicate( void );            /* duplicates the latest value on the stack */
static void    hb_vmDuplTwo( void );              /* duplicates the latest two value on the stack */

/* Pop */
static BOOL    hb_vmPopLogical( void );           /* pops the stack latest value and returns its logical value */
static HB_LONG hb_vmPopHBLong( void );            /* pops the stack latest value and returns its HB_LONG value */
static double  hb_vmPopNumber( void );            /* pops the stack latest value and returns its numeric value */
static double  hb_vmPopDouble( int * );           /* pops the stack latest value and returns its double numeric format value */
static void    hb_vmPopAlias( void );             /* pops the workarea number form the eval stack */
static void    hb_vmPopAliasedField( PHB_SYMB );  /* pops an aliased field from the eval stack*/
static void    hb_vmPopAliasedVar( PHB_SYMB );    /* pops an aliased variable from the eval stack*/
static void    hb_vmPopLocal( SHORT iLocal );     /* pops the stack latest value onto a local */
static void    hb_vmPopStatic( USHORT uiStatic ); /* pops the stack latest value onto a static */

/* misc */
static void    hb_vmDoInitStatics( void );        /* executes all _INITSTATICS functions */
static void    hb_vmDoInitFunctions( void );      /* executes all defined PRGs INIT functions */
// HB_EXPORT void hb_vmDoExitFunctions( void );      /* executes all defined PRGs EXIT functions */


// extern BOOL   hb_regex( char cRequest, PHB_ITEM pRegEx, PHB_ITEM pString );

extern HARBOUR  hb___msgGetClsData( void );
extern HARBOUR  hb___msgSetClsData( void );
extern HARBOUR  hb___msgGetShrData( void );
extern HARBOUR  hb___msgSetShrData( void );
extern HARBOUR  hb___msgGetData( void );
extern HARBOUR  hb___msgSetData( void );
extern PCLASS   hb_clsClassesArray( void );
extern void hb_clsSetModule( USHORT uiClass );

#ifndef HB_NO_PROFILER
   BOOL hb_bProfiler = FALSE; /* profiler status is off */
   ULONG hb_ulOpcodesCalls[ HB_P_LAST_PCODE ]; /* array to profile opcodes calls */
   ULONG hb_ulOpcodesTime[ HB_P_LAST_PCODE ]; /* array to profile opcodes consumed time */

   extern void hb_mthAddTime( PMETHOD, ULONG ); /* profiler from classes.c */
#endif

#ifdef HARBOUR_START_PROCEDURE
   char *s_pszLinkedMain = NULL; /* name of starup function set by linker */
#endif

#ifndef HB_NO_TRACE
   BOOL hb_bTracePrgCalls = FALSE; /* prg tracing is off */
#endif

/* virtual machine state */

HB_SYMB  hb_symEval = { "__EVAL", {HB_FS_PUBLIC}, {hb_vmDoBlock}, NULL }; /* symbol to evaluate codeblocks */

static HB_ITEM  s_aStatics;         /* Harbour array to hold all application statics variables */
static USHORT   s_uiStatics;        /* Number of statics added after processing hb_vmStatics() */
static PHB_SYMB s_pSymStart = NULL; /* start symbol of the application. MAIN() is not required */
static PSYMBOLS s_pSymbols = NULL;  /* to hold a linked list of all different modules symbol tables */
static ULONG    s_ulFreeSymbols = 0;/* number of free module symbols */
static void *   s_hDynLibID = NULL; /* unique identifer to mark symbol tables loaded from dynamic libraries */
static BOOL     s_fCloneSym = FALSE;/* clone registered symbol tables */

static int      s_iErrorLevel;      /* application exit errorlevel */

static BOOL     s_bDebugging;
static BOOL     s_bDebugRequest;    /* debugger invoked via the VM */
static BOOL     s_bDebugShowLines;  /* update source code line on the debugger display */
static BOOL     s_bDebuggerIsWorking; /* to know when __DBGENTRY is beeing invoked */
static PHB_SYMB s_pSymDbgEntry = NULL; /* Cached __DBGENTRY symbol */
HB_EXPORT HB_DBGENTRY_FUNC hb_vm_pFunDbgEntry = NULL; /* C level debugger entry */
/* Stores level of procedures call stack */
static ULONG    s_ulProcLevel = 0;

char *hb_vm_sNull = "";

/* static, for now */
BOOL hb_vm_bQuitRequest = FALSE;

int hb_vm_iTry = 0;

PHB_ITEM hb_vm_BreakBlock = NULL;

static int s_iBaseLine;

static   HB_ITEM  s_aGlobals;         /* Harbour array to hold all application global variables */

static PHB_FUNC_LIST s_InitFunctions = NULL;
static PHB_FUNC_LIST s_ExitFunctions = NULL;

/* 21/10/00 - maurilio.longo@libero.it
   This Exception Handler gets called in case of an abnormal termination of an harbour program and
   displays a full stack trace at the harbour language level */
#if defined(HB_OS_OS2)
ULONG _System OS2TermHandler(PEXCEPTIONREPORTRECORD       p1,
                             PEXCEPTIONREGISTRATIONRECORD p2,
                             PCONTEXTRECORD               p3,
                             PVOID                        pv);
#endif

#ifndef HB_THREAD_SUPPORT
  /* background function counter */
  static int s_iBackground = 0;
#endif

void HB_EXPORT hb_vmAtInit( HB_INIT_FUNC pFunc, void * cargo )
{
   PHB_FUNC_LIST pLst = ( PHB_FUNC_LIST ) hb_xgrab( sizeof( HB_FUNC_LIST ) );

   pLst->pFunc = pFunc;
   pLst->cargo = cargo;
   pLst->pNext = s_InitFunctions;
   s_InitFunctions = pLst;
}

void HB_EXPORT hb_vmAtExit( HB_INIT_FUNC pFunc, void * cargo )
{
   PHB_FUNC_LIST pLst = ( PHB_FUNC_LIST ) hb_xgrab( sizeof( HB_FUNC_LIST ) );

   pLst->pFunc = pFunc;
   pLst->cargo = cargo;
   pLst->pNext = s_ExitFunctions;
   s_ExitFunctions = pLst;
}

static void hb_vmCleanModuleFunctions( void )
{
   PHB_FUNC_LIST pLst;

   while( s_InitFunctions )
   {
      pLst = s_InitFunctions;
      s_InitFunctions = pLst->pNext;
      hb_xfree( pLst );
   }
   while( s_ExitFunctions )
   {
      pLst = s_ExitFunctions;
      s_ExitFunctions = pLst->pNext;
      hb_xfree( pLst );
   }
}

static void hb_vmDoModuleInitFunctions( void )
{
   PHB_FUNC_LIST pLst = s_InitFunctions;

   while( pLst )
   {
      pLst->pFunc( pLst->cargo );
      pLst = pLst->pNext;
   }
}

static void hb_vmDoModuleExitFunctions( void )
{
   PHB_FUNC_LIST pLst = s_ExitFunctions;

   while( pLst )
   {
      pLst->pFunc( pLst->cargo );
      pLst = pLst->pNext;
   }
}

static BYTE * hb_vmUnhideString( BYTE uiType, ULONG ulSize, const BYTE * pSource, ULONG ulBufferSize )
{
   BYTE * pBuffer = ( BYTE * ) hb_xgrab( HB_MAX( ulSize, 1 ) ), * pTarget;

   HB_SYMBOL_UNUSED( ulBufferSize );

   switch( uiType )
   {
      case 1:      /* Simple XOR 0xf3 */
         pTarget = pBuffer;
         while( ulSize-- )
         {
            *pTarget++ = ( *pSource++ ) ^ 0xf3;
         }
         break;

      default:     /* No decode */
         memcpy( pBuffer, pSource, ulSize );
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
   PHB_DYNS pDynSym = hb_dynsymFind( "__ERRORBLOCK" );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo(0);
   }
   else
   {
      hb_errInternal( HB_EI_ERRUNRECOV, "Error! missing __ErrorBlock()", NULL, NULL );
   }

   pDynSym = hb_dynsymFind( "__BREAKBLOCK" );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo(0);
      hb_vm_BreakBlock = hb_itemNew( NULL );
      hb_itemForwardValue( hb_vm_BreakBlock, hb_stackReturnItem() );
   }
   else
   {
      hb_errInternal( HB_EI_ERRUNRECOV, "Error! missing __BreakBlock()", NULL, NULL );
   }
}

/* Initialize ErrorBlock() and __SetHelpK() */
static void hb_vmDoInitClip( void )
{
   PHB_DYNS pDynSym = hb_dynsymFind( "CLIPINIT" );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo(0);
   }
}

#if defined(HB_OS_WIN_32)
/* Initialize Ole System IF linked in. */
static void hb_vmDoInitOle( void )
{
   PHB_DYNS pDynSym = hb_dynsymFind( "HB_OLEINIT" );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo(0);
   }
}
#endif

static void hb_vmDoInitHashEntry( void )
{
   PHB_DYNS pDynSym = hb_dynsymFind( "HASHENTRY" );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo(0);
   }
}

/* application entry point */
void HB_EXPORT hb_vmInit( BOOL bStartMainProc )
{
#if defined(HB_OS_OS2)
   EXCEPTIONREGISTRATIONRECORD RegRec = {0};       /* Exception Registration Record */
   APIRET rc = NO_ERROR;                           /* Return code                   */
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_vmInit()"));

   /* initialize internal data structures */
   s_aStatics.type = HB_IT_NIL;
   s_iErrorLevel = 0;
   s_bDebugging = FALSE;
   s_bDebugShowLines = FALSE;
   s_bDebuggerIsWorking = FALSE;

   hb_xinit();
   hb_gcInit();

#ifdef HB_THREAD_SUPPORT
   HB_TRACE( HB_TR_INFO, ("threadInit") );
   hb_threadInit();
#endif

   HB_TRACE( HB_TR_INFO, ("stackInit") );
   hb_stackInit();

#ifndef HB_THREAD_SUPPORT
   /* In MT mode this is a part of stack initialization code */
   HB_VM_STACK.pSequence = NULL;
   HB_VM_STACK.uiActionRequest = 0;

   for( HB_VM_STACK.wWithObjectCounter = 0; HB_VM_STACK.wWithObjectCounter < HB_MAX_WITH_OBJECTS; HB_VM_STACK.wWithObjectCounter++  )
   {
      HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter ].type = HB_IT_NIL;
   }

   HB_VM_STACK.wWithObjectCounter = 0;
   HB_VM_STACK.bWithObject = FALSE;

   for( HB_VM_STACK.wEnumCollectionCounter = 0; HB_VM_STACK.wEnumCollectionCounter < HB_MAX_ENUMERATIONS; HB_VM_STACK.wEnumCollectionCounter++ )
   {
      HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ].type = HB_IT_NIL;
      HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;
   }

   HB_VM_STACK.wEnumCollectionCounter = 0;

   /* initialization of macro & codeblock parameter passing */
   HB_VM_STACK.iExtraParamsIndex = 0;
   HB_VM_STACK.iExtraElementsIndex = 0;
   HB_VM_STACK.iExtraElements = 0;
   HB_VM_STACK.iExtraIndex = 0;
#endif

#if defined(HB_OS_WIN_32)
   if( hb_dynsymFind( "HB_NOMOUSE" ) )
   {
      b_MouseEnable = FALSE;
   }
#endif

   s_aGlobals.type = HB_IT_NIL;
   hb_arrayNew( &s_aGlobals, 0 );
   //printf( "Allocated s_aGlobals: %p Owner: %p\n", &s_aGlobals, s_aGlobals.item.asArray.value->pOwners );

   HB_TRACE( HB_TR_INFO, ("errInit") );
   hb_errInit();

   HB_TRACE( HB_TR_INFO, ("dynsymNew") );
   hb_dynsymNew( &hb_symEval, NULL );  /* initialize dynamic symbol for evaluating codeblocks */

   HB_TRACE( HB_TR_INFO, ("setInitialize") );
   hb_setInitialize();        /* initialize Sets */

   HB_TRACE( HB_TR_INFO, ("conInit") );
   hb_conInit();    /* initialize Console */

#ifndef HB_THREAD_SUPPORT
   /* in threads, it is called from thread stack constructor */
   HB_TRACE( HB_TR_INFO, ("memvarsInit") );
   hb_memvarsInit();
#endif

   HB_TRACE( HB_TR_INFO, ("il18Init") );
   hb_i18nInit( NULL, NULL );  // try to open default language.

   HB_TRACE( HB_TR_INFO, ("SymbolInit_RT") );
   hb_vmSymbolInit_RT();      /* initialize symbol table with runtime support functions */

   /* Set the language to the default */
   hb_langSelectID( HB_MACRO2STRING( HB_LANG_DEFAULT ) );

   /* Check for some internal switches */
   HB_TRACE( HB_TR_INFO, ("cmdarg") );
   hb_cmdargProcessVM();

   #ifndef HB_NO_PROFILER
      HB_TRACE( HB_TR_INFO, ("profiler" ) );
      /* Initialize opcodes profiler support arrays */
      {
         ULONG ul;

         for( ul = 0; ul < HB_P_LAST_PCODE; ul++ )
         {
            hb_ulOpcodesCalls[ ul ] = 0;
            hb_ulOpcodesTime[ ul ] = 0;
         }
      }
   #endif

   /* Intitialize basic error system to report errors which can appear
    * in InitStatics, the real (full feature) error block will be installed
    * in InitClip()
    */
   HB_TRACE( HB_TR_INFO, ("InitError") );
   hb_vmDoInitError();

   /* Call functions that initializes static variables
    * Static variables have to be initialized before any INIT functions
    * because INIT function can use static variables.
    */
   HB_TRACE( HB_TR_INFO, ("InitStatics") );
   hb_vmDoInitStatics();

   HB_TRACE( HB_TR_INFO, ("InitClip") );
   hb_vmDoInitClip(); // Initialize ErrorBlock() and __SetHelpK()

   #if defined(HB_OS_WIN_32)
      if( hb_dynsymFind( "TOLEAUTO" ) )
      {
         if( OleInitialize( NULL ) == S_OK ) // Do NOT use SUCCEEDED() due to S_FALSE!
         {
            s_bUnInitOle = TRUE;
         }

         hb_vmDoInitOle();
      }
   #endif

   hb_vmDoInitHashEntry();

   HB_TRACE( HB_TR_INFO, ("InitModuleFunctions") );
   hb_vmDoModuleInitFunctions();

   //printf( "Before InitFunctions\n" );
   HB_TRACE( HB_TR_INFO, ("InitFunctions") );
   hb_vmDoInitFunctions(); /* process defined INIT functions */

   /* This is undocumented CA-Clipper, if there's a function called _APPMAIN
      it will be executed first. [vszakats] */
   {
      PHB_DYNS pDynSym = hb_dynsymFind( "_APPMAIN" );

      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
      {
         s_pSymStart = pDynSym->pSymbol;
      }
#ifdef HARBOUR_START_PROCEDURE
      else
      {
         /* if first char is '@' then start procedure were set by
            programmer explicitly and should have the highest priority
            in other case it's the name of first public function in
            first linked moudule which is used if there is no
            HARBOUR_START_PROCEDURE in code */
         if( s_pszLinkedMain && *s_pszLinkedMain == '@' )
         {
            pDynSym = hb_dynsymFind( s_pszLinkedMain + 1 );
         }
         else
         {
            pDynSym = hb_dynsymFind( HARBOUR_START_PROCEDURE );

            if( ! ( pDynSym && pDynSym->pSymbol->value.pFunPtr ) && s_pszLinkedMain )
            {
               pDynSym = hb_dynsymFind( s_pszLinkedMain );
            }
         }

         if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
         {
            s_pSymStart = pDynSym->pSymbol;
         }
         else
         {
            hb_errInternal( HB_EI_VMBADSTARTUP, NULL, HARBOUR_START_PROCEDURE, NULL );
         }
      }
#else
#ifndef HB_C52_STRICT
      else if( bStartMainProc && ! s_pSymStart )
      {
         hb_errInternal( HB_EI_VMNOSTARTUP, NULL, NULL, NULL );
      }
#endif
#endif
   }

   #if defined(HB_OS_OS2) /* Add OS2TermHandler to this thread's chain of exception handlers */
      RegRec.ExceptionHandler = (ERR)OS2TermHandler;
      rc = DosSetExceptionHandler( &RegRec );

      if (rc != NO_ERROR)
      {
         hb_errInternal( HB_EI_ERRUNRECOV, "Unable to setup exception handler (DosSetExceptionHandler())", NULL, NULL );
      }
   #endif

   if( bStartMainProc && s_pSymStart )
   {
      int i;
      int iArgCount;

      hb_vmPushSymbol( s_pSymStart ); /* pushes first HB_FS_PUBLIC defined symbol to the stack */
      hb_vmPushNil();                 /* places NIL at self */

      iArgCount = 0;
      for( i = 1; i < hb_cmdargARGC(); i++ )     /* places application parameters on the stack */
      {
         char ** argv = hb_cmdargARGV();

         /* Filter out any parameters beginning with //, like //INFO */
         if( ! hb_cmdargIsInternal( argv[ i ] ) )
         {
            hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );
            iArgCount++;
         }
      }

      //printf( "Before Startup\n" );
      hb_vmDo( iArgCount ); /* invoke it with number of supplied parameters */
   }

   #if defined(HB_OS_OS2)
      /* I don't do any check on return code since harbour is exiting in any case */
      rc = DosUnsetExceptionHandler( &RegRec );
   #endif
}

void hb_vmReleaseLocalSymbols( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmReleaseLocalSymbols()"));

   while( s_pSymbols )
   {
      PSYMBOLS pDestroy;

      pDestroy = s_pSymbols;
      s_pSymbols = s_pSymbols->pNext;

      if( pDestroy->szModuleName )
      {
         hb_xfree( pDestroy->szModuleName );
      }
      if( pDestroy->fAllocated )
      {
         USHORT ui;
         for( ui = 0; ui < pDestroy->uiModuleSymbols; ++ui )
         {
            PHB_SYMB pSymbol = pDestroy->pModuleSymbols + ui;
            if( pSymbol->pDynSym && pSymbol->pDynSym->pSymbol == pSymbol )
            {
               pSymbol->pDynSym->pSymbol = NULL;
            }
            hb_xfree( pSymbol->szName );
         }
         hb_xfree( pDestroy->pModuleSymbols );
      }
      hb_xfree( pDestroy );
   }

   HB_TRACE(HB_TR_DEBUG, ("Done hb_vmReleaseLocalSymbols()"));
}

int HB_EXPORT hb_vmQuit( void )
{
   static BOOL bQuitting = FALSE;
   unsigned int i;

   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmQuit(%i)"));

#ifdef HB_THREAD_SUPPORT
   /* Quit sequences for non-main thread */
   /* We are going to quit now, so we don't want to have mutexes
      blocking our output */
   hb_set.HB_SET_OUTPUTSAFETY = FALSE;

   if (! HB_SAME_THREAD( hb_main_thread_id, HB_CURRENT_THREAD()) )
   {
      hb_vm_bQuitRequest = TRUE;
      #if defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
         HB_DISABLE_ASYN_CANC
         HB_STACK_LOCK
         hb_threadCancelInternal(); // never returns
      #else
         pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, NULL );
         HB_STACK_LOCK
         pthread_exit(0);
      #endif
   }
#endif

   if( bQuitting == FALSE )
   {
      bQuitting = TRUE;
   }
   else
   {
      return s_iErrorLevel;
   }

   //printf("\nvmQuit()\n" );
#ifdef HB_THREAD_SUPPORT
   hb_threadKillAll();
   hb_threadWaitAll();
#endif

#ifdef HB_MACRO_STATEMENTS
   hb_pp_Free();
   //printf("After PP\n" );
#endif

   HB_VM_STACK.uiActionRequest = 0;         /* EXIT procedures should be processed */

   hb_vmDoExitFunctions();       /* process defined EXIT functions */
   //printf("After ExitFunctions\n" );

   /* process AtExit registered functions */
   hb_vmDoModuleExitFunctions();
   hb_vmCleanModuleFunctions();

   /* release all known items stored in subsystems */
   hb_rddShutDown();
   //printf("After RDD\n" );

   hb_idleShutDown();
   //printf("After Idle\n" );

   hb_backgroundShutDown();

   hb_inkeyExit();
   //printf("\nAfter inkey\n" );

   hb_i18nExit();

   /* deactivate debugger */
   hb_vmDebuggerExit();

#if !defined( HB_OS_DOS ) && !defined( HB_OS_DARWIN_5 )
   if( pHVMFuncService )
      pHVMFuncService();
#endif

   if( hb_set.HB_SET_EOL )
   {
      hb_itemRelease( hb_set.HB_SET_EOL );
   }

   // FOR EACH Enumerations.
   for( i = 0; i < HB_VM_STACK.wEnumCollectionCounter; i++ )
   {
      if( HB_IS_COMPLEX(  &( HB_VM_STACK.aEnumCollection[ i ] ) ) )
      {
         hb_itemClear( &( HB_VM_STACK.aEnumCollection[ i ] ) );
      }
   }

   // WITH OBJECT
   for( i = 0; i < HB_VM_STACK.wWithObjectCounter; i++ )
   {
      if( HB_IS_COMPLEX( &( HB_VM_STACK.aWithObject[ i ] ) ) )
      {
         hb_itemClear( &( HB_VM_STACK.aWithObject[ i ] ) );
      }
   }

   /* release all remaining items */
   /*
    * NO Need to perform individual hb_itemClear() since hb_gcCollectAll() will perform needed cleanup!
    * This also avoids GPF trap related to Destructor using ProcName() etc.
   hb_stackRemove( 0 );
   */
   while( HB_VM_STACK.pPos > HB_VM_STACK.pItems )
   {
      if( HB_IS_STRING( *( HB_VM_STACK.pPos - 1 ) ) )
      {
         hb_itemReleaseString( *( HB_VM_STACK.pPos - 1 ) );
      }

      ( *( HB_VM_STACK.pPos - 1 ) )->type = HB_IT_NIL;

      --HB_VM_STACK.pPos;
   }
   //printf("After Stack\n" );

   if( HB_IS_COMPLEX( &(HB_VM_STACK.Return) ) )
   {
      hb_itemClear( &(HB_VM_STACK.Return) );
   }
   //printf("After Return\n" );

   if( s_aGlobals.type == HB_IT_ARRAY )
   {
      // Because GLOBALS items are of type HB_IT_REF (see hb_vmRegisterGlobals())!
      hb_arrayFill( &s_aGlobals, ( *HB_VM_STACK.pPos ), 1, s_aGlobals.item.asArray.value->ulLen );
      //TraceLog( NULL, "Releasing s_aGlobals: %p\n", &s_aGlobals );
      hb_arrayRelease( &s_aGlobals );
      s_aGlobals.type = HB_IT_NIL;
      //TraceLog( NULL, "   Released s_aGlobals: %p\n", &s_aGlobals );
   }
   //printf("\nAfter Globals\n" );

   // To allow Class Destructors after all other cleanup, but before it's TOO late. - Don't MOVE!!!
   hb_gcCollectAll( TRUE );

   // Static can NOT be released prior to hb_gcCollectAll() above because Destrcutor might trigger DBG code that uses them.
   if( s_aStatics.type == HB_IT_ARRAY )
   {
      HB_TRACE(HB_TR_DEBUG, ("Releasing s_aStatics: %p\n", &s_aStatics) );
     //#define DEBUG_STATICS
     #ifdef DEBUG_STATICS
     {
       ULONG ulLen = s_aStatics.item.asArray.value->ulLen;
       ULONG ulIndex;
       char sBuffer[128];

       for( ulIndex = 0; ulIndex < ulLen; ulIndex++ )
       {
         sprintf( sBuffer, "# %i type: %i\n", ulIndex, (s_aStatics.item.asArray.value->pItems + ulIndex)->type );
         OutputDebugString( sBuffer );
       }
     }
     #endif

      hb_arrayRelease( &s_aStatics );
      s_aStatics.type = HB_IT_NIL;
      HB_TRACE(HB_TR_DEBUG, ("   Released s_aStatics: %p\n", &s_aStatics) );
   }
   //printf("\nAfter Statics\n" );

   #if defined(HB_OS_WIN_32)
      if( s_bUnInitOle )
      {
         OleUninitialize();
      }
   #endif

   hb_errExit();
   //printf("After Err\n" );

   hb_clsReleaseAll();
   //printf("After Class\n" );

   hb_vmReleaseLocalSymbols();  /* releases the local modules linked list */
   //printf("After Symbols\n" );

   hb_dynsymRelease();          /* releases the dynamic symbol table */
   //printf("After Dyn\n" );

   hb_conRelease();             /* releases Console */
   //printf("After Con\n" );

   hb_setRelease();             /* releases Sets */
   //printf("After Set\n" );

#ifndef HB_CDP_SUPPORT_OFF
   hb_cdpReleaseAll();          /* releases codepages */
   //printf("After Cdp\n" );
#endif

   hb_gcCollectAll( TRUE );
   //printf("After GC before Memvar\n" );

#ifndef HB_THREAD_SUPPORT
   hb_memvarsRelease();         /* clear all PUBLIC variables */
   //printf("After Memvar\n" );
#endif

   hb_stackFree();
   //printf("After hbStackFree\n" );

   hb_itemRelease( hb_vm_BreakBlock );

   /* hb_dynsymLog(); */

   /* release all known garbage */
   if ( hb_xquery( HB_MEM_USEDMAX ) ) /* check if fmstat is ON */
   {
      hb_gcCollectAll( TRUE );
   }
   else
   {
      hb_gcReleaseAll();
   }
   //printf("After GC\n" );

   while( HB_VM_STACK.pSequence )
   {
      PHB_SEQUENCE pFree = HB_VM_STACK.pSequence;

      HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;

      hb_xfree( (void *) pFree );
   }

   hb_xexit();
   //printf("After xexit\n" );

   hb_traceExit();
   //printf("After traceExit\n" );

#ifdef HB_THREAD_SUPPORT
   hb_threadExit();
   //printf("After thread exit\n" );
#endif

   return s_iErrorLevel;
}

void HB_EXPORT hb_vmExecute( register const BYTE * pCode, register PHB_SYMB pSymbols, PHB_ITEM** pGlobals )
{
   HB_THREAD_STUB

   LONG w = 0;
   BOOL bCanRecover = FALSE;
   BOOL bCanFinalize = FALSE;
   BOOL bDynCode = pSymbols == NULL || ( pSymbols->scope.value & HB_FS_DYNCODE ) != 0;
   LONG lNextSection /*= 0*/, lCatchSection = 0, lFinallySection = 0;
   ULONG ulPrivateBase;
   ULONG wEnumCollectionCounter = HB_VM_STACK.wEnumCollectionCounter;
   ULONG wWithObjectCounter = HB_VM_STACK.wWithObjectCounter;

#ifndef HB_GUI
   static unsigned short uiPolls = 1;
#endif

#ifndef HB_NO_PROFILER
   ULONG ulLastOpcode = 0; /* opcodes profiler support */
   ULONG ulPastClock = 0;  /* opcodes profiler support */
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_vmExecute(%p, %p, %p)", pCode, pSymbols, pGlobals));

   //TraceLog( NULL, "%s->hb_vmExecute(%p, %p, %p)\n", hb_stackBaseItem()->item.asSymbol.value->szName, pCode, pSymbols, pGlobals );

   /* NOTE: if pSymbols == NULL then hb_vmExecute is called from macro
    * evaluation. In this case all PRIVATE variables created during
    * macro evaluation belong to a function/procedure where macro
    * compiler was called.
    */

   /* NOTE: Initialization with 0 is needed to avoid GCC -O2 warning */
   ulPrivateBase = pSymbols ? hb_memvarGetPrivatesBase() : 0;

#ifndef HB_NO_PROFILER
   if( hb_bProfiler )
   {
      ulPastClock = ( ULONG ) clock();
   }
#endif

   while( TRUE )
   {
#ifndef HB_NO_PROFILER
      if( hb_bProfiler )
      {
         ULONG ulActualClock = ( ULONG ) clock();

         hb_ulOpcodesTime[ ulLastOpcode ] += ( ulActualClock - ulPastClock );
         ulPastClock = ulActualClock;
         ulLastOpcode = pCode[ w ];
         hb_ulOpcodesCalls[ ulLastOpcode ]++;
      }
#endif

#ifndef HB_GUI
      if( ! --uiPolls )
      {
         if( hb_set.HB_SET_CANCEL || hb_set.HB_SET_DEBUG )
         {
            hb_inkeyPoll();
         }
      }
#endif

#if ! defined( HB_THREAD_SUPPORT )
      if( hb_set.HB_SET_BACKGROUNDTASKS )
      {
         if( ++s_iBackground > hb_set.HB_SET_BACKGROUNDTICK )
         {
            hb_backgroundRun();
            s_iBackground = 0;
         }
      }
#endif

      switch( pCode[ w ] )
      {
         /* Operators ( mathematical / character / misc ) */

         case HB_P_NEGATE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_NEGATE") );
            hb_vmNegate();
            w++;
            break;

         case HB_P_PLUS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PLUS") );

            hb_vmPlus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );
            hb_stackPop();

            w++;
            break;

         case HB_P_MINUS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_MINUS") );
            hb_vmMinus();
            w++;
            break;

         case HB_P_MULT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_MULT") );
            hb_vmMult();
            w++;
            break;

         case HB_P_DIVIDE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_DIVIDE") );
            hb_vmDivide();
            w++;
            break;

         case HB_P_MODULUS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_MODULUS") );
            hb_vmModulus();
            w++;
            break;

         case HB_P_POWER:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POWER") );
            hb_vmPower();
            w++;
            break;

         case HB_P_BITAND :
            HB_TRACE(HB_TR_DEBUG, ("HB_P_BITAND"));
            hb_vmBitAnd();
            w++;
            break;

         case HB_P_BITOR :
            HB_TRACE(HB_TR_DEBUG, ("HB_P_BITOR"));
            hb_vmBitOr();
            w++;
            break;

         case HB_P_BITXOR :
            HB_TRACE(HB_TR_DEBUG, ("HB_P_BITXOR"));
            hb_vmBitXor();
            w++;
            break;

         case HB_P_BITSHIFTR :
            HB_TRACE(HB_TR_DEBUG, ("HB_P_BITSHIFTR"));
            hb_vmBitShiftRight();
            w++;
            break;

         case HB_P_BITSHIFTL :
            HB_TRACE(HB_TR_DEBUG, ("HB_P_BITSHIFTL"));
            hb_vmBitShiftLeft();
            w++;
            break;

         case HB_P_INC:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_INC") );
            hb_vmInc();
            w++;
            break;

         case HB_P_DEC:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_DEC") );
            hb_vmDec();
            w++;
            break;

         case HB_P_FUNCPTR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_FUNCPTR") );
            hb_vmFuncPtr();
            w++;
            break;

         /* Operators (relational) */

         case HB_P_EQUAL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_EQUAL") );
            hb_vmEqual( FALSE );
            w++;
            break;

         case HB_P_EXACTLYEQUAL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_EXACTLTYEQUAL") );
            hb_vmEqual( TRUE );
            w++;
            break;

         case HB_P_NOTEQUAL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_NOTEQUAL") );
            hb_vmNotEqual();
            w++;
            break;

         case HB_P_LESS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_LESS") );
            hb_vmLess();
            w++;
            break;

         case HB_P_LESSEQUAL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_LESSEQUAL") );
            hb_vmLessEqual();
            w++;
            break;

         case HB_P_GREATER:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_GREATER") );
            hb_vmGreater();
            w++;
            break;

         case HB_P_GREATEREQUAL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_GREATEREQUAL") );
            hb_vmGreaterEqual();
            w++;
            break;

            // Also used for Arrays!!!
         case HB_P_INSTRING:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_INSTRING") );
            hb_vmInstringOrArray();
            w++;
            break;

         case HB_P_LIKE:
         {
            PHB_ITEM pResult = hb_stackItemFromTop( -2 );
            BOOL bLike;

            HB_TRACE( HB_TR_DEBUG, ("HB_P_LIKE") );

            bLike = hb_regex( 1, hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );

            hb_stackPop();

            hb_itemClear( pResult );
            pResult->type = HB_IT_LOGICAL;
            pResult->item.asLogical.value = bLike;

            w++;
            break;
         }

         case HB_P_MATCH:
         {
            PHB_ITEM pResult = hb_stackItemFromTop( -2 );
            BOOL bMatch;

            HB_TRACE( HB_TR_DEBUG, ("HB_P_MATCH") );

            bMatch = hb_regex( 2, hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );

            hb_stackPop();

            hb_itemClear( pResult );
            pResult->type = HB_IT_LOGICAL;
            pResult->item.asLogical.value = bMatch;

            w++;
            break;
         }

         case HB_P_FORTEST:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_FORTEST") );
            hb_vmForTest();
            w++;
            break;

         /* Operators (logical) */

         case HB_P_NOT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_NOT") );
            hb_vmNot();
            w++;
            break;

         case HB_P_AND:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_AND") );
            hb_vmAnd();
            w++;
            break;

         case HB_P_OR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_OR") );
            hb_vmOr();
            w++;
            break;

         /* Array */

         case HB_P_ARRAYPUSH:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ARRAYPUSH") );
            hb_vmArrayPush();
            w++;
            break;


         case HB_P_ARRAYPUSHREF:
            hb_vmArrayPushRef();
            w++;
            break;

         case HB_P_ARRAYPOP:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ARRAYPOP") );
            hb_vmArrayPop( HB_P_NOOP );
            w++;
            break;

         case HB_P_ARRAYPOPPLUS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ARRAYPOPPLUS") );
            hb_vmArrayPop( HB_P_PLUS );
            w++;
            break;

         case HB_P_ARRAYDIM:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ARRAYDIM") );
            hb_vmArrayDim( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_ARRAYGEN:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ARRAYGEN %i + %i", HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), HB_VM_STACK.iExtraElements ) );
            hb_vmArrayGen( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) + HB_VM_STACK.iExtraElements );
            HB_VM_STACK.iExtraElements = 0;

            w += 3;
            break;

         /* Object */

         case HB_P_MESSAGE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_MESSAGE") );
            //TraceLog( NULL, "%s->HB_P_MESSAGE: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         /* Database */

         case HB_P_SWAPALIAS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_SWAPALIAS") );
            hb_vmSwapAlias();
            w++;
            break;

         /* Execution */

         case HB_P_DO:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_DO") );
            hb_vmDo( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_DOSHORT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_DOSHORT") );
            hb_vmDo( pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_FUNCTION:
         {
            USHORT uiParams = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

            HB_TRACE( HB_TR_DEBUG, ("HB_P_FUNCTION") );

            //hb_vmFunction( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );

            if( HB_IS_COMPLEX( &(HB_VM_STACK.Return) ) )
            {
               hb_itemClear( &(HB_VM_STACK.Return) );
            }
            else
            {
               ( &(HB_VM_STACK.Return) )->type = HB_IT_NIL;
            }
            hb_vmDo( uiParams );


            // Thread Safety.
            hb_stackPush();
            hb_itemForwardValue( *( HB_VM_STACK.pPos - 1 ), &(HB_VM_STACK.Return) );

            w += 3;
            break;
         }

         case HB_P_FUNCTIONSHORT:
         {
            USHORT uiParams = pCode[ w + 1 ];

            HB_TRACE( HB_TR_DEBUG, ("HB_P_FUNCTIONSHORT") );

            //hb_vmFunction( pCode[ w + 1 ] );

            if( HB_IS_COMPLEX( &(HB_VM_STACK.Return) ) )
            {
               hb_itemClear( &(HB_VM_STACK.Return) );
            }
            else
            {
               ( &(HB_VM_STACK.Return) )->type = HB_IT_NIL;
            }
            hb_vmDo( uiParams );

            hb_stackPush();
            hb_itemForwardValue( *( HB_VM_STACK.pPos - 1 ), &(HB_VM_STACK.Return) );

            w += 2;
            break;
         }

         case HB_P_WITHOBJECT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_WITHOBJECT") );

            hb_itemForwardValue( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter++ ] ), hb_stackItemFromTop( -1 ) );

            if( HB_VM_STACK.wWithObjectCounter == HB_MAX_WITH_OBJECTS )
            {
               hb_errRT_BASE( EG_ARG, 9002, NULL, "WITH OBJECT excessive nesting!", 0 );

               // Release ALL WITH OBJECT.
               while( HB_VM_STACK.wWithObjectCounter )
               {
                  --HB_VM_STACK.wWithObjectCounter;
                  hb_itemClear( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter ] ) );
               }
            }

            hb_stackPop();
            w++;
            break;

         case HB_P_ENDWITHOBJECT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ENDWITHOBJECT") );

            if( HB_VM_STACK.wWithObjectCounter )
            {
               hb_itemClear( &( HB_VM_STACK.aWithObject[ --HB_VM_STACK.wWithObjectCounter ] ) );
            }

            w++;
            break;

         case HB_P_FOREACH:
         {
            PHB_ITEM pEnumeration = &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] );

            HB_TRACE( HB_TR_DEBUG, ("HB_P_FOREACH") );

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
                // No prep needed.
            }
            else if( HB_IS_HASH( pEnumeration ) /* && hb_hashGetCompatibility( pEnumeration ) */ )
            {
                // No prep needed.
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 1602, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pEnumeration, hb_itemPutNI( * HB_VM_STACK.pPos, 1 ) );
            }

            HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] = hb_itemUnRef( hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            HB_VM_STACK.wEnumCollectionCounter++;

            if( HB_VM_STACK.wEnumCollectionCounter == HB_MAX_ENUMERATIONS )
            {
               hb_errRT_BASE( EG_ARG, 9002, NULL, "FOR EACH excessive nesting!", 0 );

               // Release ALL FOR EACH.
               while( HB_VM_STACK.wEnumCollectionCounter )
               {
                  HB_VM_STACK.wEnumCollectionCounter--;
                  hb_itemClear( pEnumeration );
                  HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;
               }
            }

            w++;
            break;
         }

         case HB_P_ENUMERATE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ENUMERATE") );

            if( HB_VM_STACK.wEnumCollectionCounter )
            {
               PHB_ITEM pEnumeration = &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter - 1 ] );
               PHB_ITEM pEnumerator  = HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter - 1 ];
               ULONG ulEnumIndex     = ++HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter - 1 ];

               if( hb_objGetOpOver( pEnumeration ) & HB_CLASS_OP_FOREACH )
               {
                  HB_ITEM_NEW( ForEachOp );
                  HB_ITEM_NEW( ForEachIndex );

                  hb_itemPutNI( &ForEachOp, FOREACH_ENUMERATE );
                  hb_itemPutNL( &ForEachIndex, ulEnumIndex );

                  hb_vmOperatorCall( pEnumeration, &ForEachOp, "__OPFOREACH", &ForEachIndex, 0, pEnumerator );

                  if( HB_VM_STACK.uiActionRequest == HB_BREAK_REQUESTED )
                  {
                     HB_VM_STACK.uiActionRequest = 0;
                     hb_vmPushLogical( FALSE );
                  }
                  else
                  {
                     hb_vmPushLogical( TRUE );
                  }
               }
               else if( HB_IS_ARRAY( pEnumeration ) || HB_IS_STRING( pEnumeration ) )
               {
                  hb_vmPushLogical( hb_arrayGetByRef( pEnumeration, ulEnumIndex, pEnumerator ) );
               }
               else if( HB_IS_HASH( pEnumeration ) )
               {
                  if( hb_hashGetCompatibility( pEnumeration ) )
                  {
                     ulEnumIndex = hb_hashAAGetRealPos( pEnumeration, ulEnumIndex );

                     if( ulEnumIndex )
                     {
                        hb_hashGet( pEnumeration, ulEnumIndex, pEnumerator );
                        hb_vmPushLogical( TRUE );
                     }
                     else
                     {
                        hb_vmPushLogical( FALSE );
                     }
                  }
                  else
                  {
                     PHB_ITEM pKey = hb_hashGetKeyAt( pEnumeration, ulEnumIndex );

                     if( pKey )
                     {
                        PHB_ITEM pValue = hb_hashGetValueAt( pEnumeration, ulEnumIndex );

                        hb_clsInst( hb_clsGetHandleFromName( "HASHENTRY" ), pEnumerator );

                        hb_arraySet( pEnumerator, pEnumerator->item.asArray.value->ulLen - 2 , pEnumeration );
                        hb_arraySet( pEnumerator, pEnumerator->item.asArray.value->ulLen - 1 , pKey );
                        hb_arraySet( pEnumerator, pEnumerator->item.asArray.value->ulLen     , pValue );

                        hb_vmPushLogical( TRUE );
                     }
                     else
                     {
                        hb_vmPushLogical( FALSE );
                     }
                  }
               }
               else
               {
                  // Should never get here!
                  hb_vmPushLogical( FALSE );
               }
            }
            else
            {
               // Should never get here!
               hb_vmPushLogical( FALSE );
            }

            w++;
            break;

         case HB_P_ENDENUMERATE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ENDENUMERATE") );

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

            // Incase EXIT was used.
            hb_itemClear( HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] );

            w++;
            break;

         case HB_P_ENUMINDEX:
            ( *HB_VM_STACK.pPos )->type = HB_IT_LONG;

            if( HB_VM_STACK.wEnumCollectionCounter )
            {
               ( *HB_VM_STACK.pPos )->item.asLong.value = HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter - 1 ];
            }
            else
            {
               ( *HB_VM_STACK.pPos )->item.asLong.value = 0;
            }

            ( * HB_VM_STACK.pPos )->item.asLong.length = 10;

            hb_stackPush();
            w++;
            break;

         case HB_P_SENDWITH:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_SENDWITH") );

            HB_VM_STACK.bWithObject = TRUE;
            // Intentionally NOT breaking - fall through!

         case HB_P_SEND:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_SEND") );
            hb_vmSend( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );

            w += 3;

            if( pCode[w] == HB_P_POP )
            {
               HB_TRACE( HB_TR_DEBUG, ("skipped HB_P_POP") );
               w++;
            }
            else
            {
               // Thread Safety.
               hb_stackPush();
               hb_itemForwardValue( *( HB_VM_STACK.pPos - 1 ), &(HB_VM_STACK.Return) );
            }
            break;

         case HB_P_SENDWITHSHORT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_SENDWITHSHORT") );

            HB_VM_STACK.bWithObject = TRUE;
            // Intentionally NOT breaking - fall through!

         case HB_P_SENDSHORT:
         {
            USHORT usParams =  pCode[ w + 1 ];
            PHB_FUNC pFunc;

            HB_TRACE( HB_TR_DEBUG, ("HB_P_SENDSHORT") );

            if( HB_VM_STACK.iExtraParamsIndex == 0 )
            {
               if( usParams == 0 )
               {
                  PHB_ITEM pSelf = hb_stackItemFromTop( -1 );

                  if( HB_IS_OBJECT( pSelf ) && pSelf->item.asArray.value->uiPrevCls == 0 )
                  {
                     BOOL bConstructor;
                     BOOL bSymbol;

                     pFunc = hb_objGetMthd( pSelf, hb_stackItemFromTop( -2 )->item.asSymbol.value, FALSE, &bConstructor, 1, &bSymbol );

                     if( pFunc == hb___msgGetData )
                     {
                        if( (HB_VM_STACK.pMethod)->uiData > ( USHORT ) pSelf->item.asArray.value->ulLen ) /* Resize needed ? */
                        {
                           hb_arraySize( pSelf, (HB_VM_STACK.pMethod)->uiData ); /* Make large enough */
                        }

                        hb_arrayGet( pSelf, (HB_VM_STACK.pMethod)->uiData, &(HB_VM_STACK.Return) );

                        hb_stackPop(); //pSelf.
                        hb_stackPop(); //Symbol.

                        goto SEND_Finalization;
                     }
                     else if( pFunc == hb___msgGetClsData )
                     {
                        // Recycle the Symbol Item.
                        hb_arrayGet( hb_clsClassesArray()[ pSelf->item.asArray.value->uiClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiData, &(HB_VM_STACK.Return) );

                        hb_stackPop(); //pSelf.
                        hb_stackPop(); //Symbol.

                        goto SEND_Finalization;
                     }
                     else if( pFunc == hb___msgGetShrData )
                     {
                        if( (HB_VM_STACK.pMethod)->uiSprClass )
                        {
                           // Recycle the Symbol Item.
                           hb_arrayGet( hb_clsClassesArray()[ (HB_VM_STACK.pMethod)->uiSprClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiDataShared, &(HB_VM_STACK.Return) );
                        }

                        hb_stackPop(); //pSelf.
                        hb_stackPop(); //Symbol.

                        goto SEND_Finalization;
                     }
                  }
               }
               else if( usParams == 1 )
               {
                  PHB_ITEM pSelf = hb_stackItemFromTop( -2 );

                  if( HB_IS_OBJECT( pSelf ) && pSelf->item.asArray.value->uiPrevCls == 0 )
                  {
                     BOOL bConstructor;
                     BOOL bSymbol;

                     pFunc = hb_objGetMthd( pSelf, hb_stackItemFromTop( -3 )->item.asSymbol.value, FALSE, &bConstructor, 1, &bSymbol );

                     if( pFunc == hb___msgSetData )
                     {
                        if( (HB_VM_STACK.pMethod)->uiData > ( USHORT ) pSelf->item.asArray.value->ulLen ) /* Resize needed ? */
                        {
                           hb_arraySize( pSelf, (HB_VM_STACK.pMethod)->uiData ); /* Make large enough */
                        }

                        hb_arraySet( pSelf, (HB_VM_STACK.pMethod)->uiData, hb_stackItemFromTop( - 1 ) );

                        hb_itemForwardValue( &(HB_VM_STACK.Return), hb_stackItemFromTop( - 1 ) );

                        hb_stackPop(); //pNewValue.
                        hb_stackPop(); //pSelf.
                        hb_stackPop(); //Symbol.

                        goto SEND_Finalization;
                     }
                     else if( pFunc == hb___msgSetClsData )
                     {
                        // Recycle the Symbol Item.
                        hb_arraySet( hb_clsClassesArray()[ pSelf->item.asArray.value->uiClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiData, hb_stackItemFromTop( - 1 ) );

                        hb_itemForwardValue( &(HB_VM_STACK.Return), hb_stackItemFromTop( - 1 ) );

                        hb_stackPop(); //pNewValue.
                        hb_stackPop(); //pSelf.
                        hb_stackPop(); //Symbol.

                        goto SEND_Finalization;
                     }
                     else if( pFunc == hb___msgSetShrData )
                     {
                        if( (HB_VM_STACK.pMethod)->uiSprClass )
                        {
                           // Recycle the Symbol Item.
                           hb_arraySet( hb_clsClassesArray()[ (HB_VM_STACK.pMethod)->uiSprClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiDataShared, hb_stackItemFromTop( - 1 ) );
                        }

                        hb_itemForwardValue( &(HB_VM_STACK.Return), hb_stackItemFromTop( - 1 ) );

                        hb_stackPop(); //pNewValue.
                        hb_stackPop(); //pSelf.
                        hb_stackPop(); //Symbol.

                        goto SEND_Finalization;
                     }
                  }
               }
            }

            if( HB_VM_STACK.uiActionRequest != HB_BREAK_REQUESTED )
            {
               //TraceLog( NULL, "Func: %p Trying hb_vmSend()\n", pFunc );
               hb_vmSend( usParams );
            }

         SEND_Finalization:

            w += 2;

            if( pCode[w] == HB_P_POP )
            {
               HB_TRACE( HB_TR_DEBUG, ("skipped HB_P_POP") );
               w++;
            }
            else
            {
               // Thread Safety.
               hb_stackPush();
               hb_itemForwardValue( *( HB_VM_STACK.pPos - 1 ), &(HB_VM_STACK.Return) );
            }

            break;
         }

         case HB_P_CLASSSETMODULE:
         {
            PHB_ITEM pClassHandle = hb_stackItemFromTop( -1 );

            if( HB_IS_INTEGER( pClassHandle ) )
            {
               hb_clsSetModule( (USHORT) ( pClassHandle->item.asInteger.value ) );
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 1603, NULL, "__ClsSetModule()", 1, pClassHandle );
            }

            hb_stackPop(); //pClassHandle.
            w++;
            break;
         }

         case HB_P_IVARREF:
         {
            PHB_ITEM pSelf = hb_stackItemFromTop( -1 ), pMsg = hb_stackItemFromTop( -2 );

            if( HB_IS_OBJECT( pSelf ) ||
              ( HB_IS_ARRAY( pSelf )   && hb_cls_uiArrayClass )     ||
              ( HB_IS_BLOCK( pSelf )   && hb_cls_uiBlockClass )     ||
              ( HB_IS_STRING( pSelf )  && hb_cls_uiCharacterClass ) ||
              ( HB_IS_DATE( pSelf )    && hb_cls_uiDateClass )      ||
              ( HB_IS_LOGICAL( pSelf ) && hb_cls_uiLogicalClass )   ||
              ( HB_IS_NIL( pSelf )     && hb_cls_uiNilClass )       ||
              ( HB_IS_NUMERIC( pSelf ) && hb_cls_uiNumericClass )   ||
              ( HB_IS_POINTER( pSelf ) && hb_cls_uiPointerClass )   ||
              ( HB_IS_HASH( pSelf )    && hb_cls_uiHashClass ) )
            {
               BOOL bConstructor;
               BOOL bSymbol;
               PHB_FUNC pFunc;

               pFunc = hb_objGetMthd( pSelf, pMsg->item.asSymbol.value, FALSE, &bConstructor, 2, &bSymbol );

               if( pFunc == hb___msgGetData )
               {
                  if( (HB_VM_STACK.pMethod)->uiData > ( USHORT ) pSelf->item.asArray.value->ulLen ) /* Resize needed ? */
                  {
                     hb_arraySize( pSelf, (HB_VM_STACK.pMethod)->uiData ); /* Make large enough */
                  }

                  // Recycle the Symbol Item.
                  hb_arrayGetByRef( pSelf, (HB_VM_STACK.pMethod)->uiData, pMsg );
               }
               else if( pFunc == hb___msgGetClsData )
               {
                  // Recycle the Symbol Item.
                  hb_arrayGetByRef( hb_clsClassesArray()[ pSelf->item.asArray.value->uiClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiData, pMsg );
               }
               else if( pFunc == hb___msgGetShrData )
               {
                  if( (HB_VM_STACK.pMethod)->uiSprClass )
                  {
                     // Recycle the Symbol Item.
                     hb_arrayGetByRef( hb_clsClassesArray()[ (HB_VM_STACK.pMethod)->uiSprClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiDataShared, pMsg );
                  }
               }
               else if( HB_VM_STACK.uiActionRequest != HB_BREAK_REQUESTED )
               {
                  /*
                   hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, "No instance variable", hb_stackItemFromTop( -2 )->item.asSymbol.value->szName, 1, pSelf );
                   *
                   pMsg->type = HB_IT_NIL;
                   */

                  hb_vmSend( 0 );

                  hb_itemPushForward( &(HB_VM_STACK.Return) );

                  w++;
                  break;
               }
               else if ( HB_IS_HASH( pSelf ) )
               {
                  char * szIndex = pMsg->item.asSymbol.value->szName;
                  ULONG ulPos;

                  /* Following are NOT assignable, in an array form, and or byref - commented by Ron 2006-09-14
                  if( strcmp( szIndex, "CLASSNAME" ) == 0 )
                  {
                     hb_itemPutC( pMsg, "HASH" );
                  }
                  else if( strcmp( szIndex, "CLASSH" ) == 0 )
                  {
                     hb_itemPutNI( pMsg, 0 );
                  }
                  else if( strcmp( szIndex, "KEYS" ) == 0 )
                  {
                     hb_hashGetKeys( pMsg, pSelf );
                  }
                  else if( strcmp( szIndex, "VALUES" ) == 0 )
                  {
                     hb_hashGetValues( pMsg, pSelf );
                  }
                  else
                  */
                  {
                     HB_ITEM_NEW( hbIndex );

                     hb_itemPutCRawStatic( &hbIndex, szIndex, strlen( szIndex ) );

                     if( hb_hashScan( pSelf, &hbIndex , &ulPos ) )
                     {
                        hb_hashGet( pSelf, ulPos, pMsg );
                     }
                     else
                     {
                        hb_vmClassError( 0, "HASH", szIndex, pSelf );
                     }
                  }
               }
            }
            else if ( HB_IS_HASH( pSelf ) )
            {
               char * szIndex = pMsg->item.asSymbol.value->szName;
               ULONG ulPos;

               /* Following are NOT assignable, in an array form, and or byref - commented by Ron 2006-09-14
               if( strcmp( szIndex, "CLASSNAME" ) == 0 )
               {
                  hb_itemPutC( pMsg, "HASH" );
               }
               else if( strcmp( szIndex, "CLASSH" ) == 0 )
               {
                  hb_itemPutNI( pMsg, 0 );
               }
               else if( strcmp( szIndex, "KEYS" ) == 0 )
               {
                  hb_hashGetKeys( pMsg, pSelf );
               }
               else if( strcmp( szIndex, "VALUES" ) == 0 )
               {
                  hb_hashGetValues( pMsg, pSelf );
               }
               else
               */
               {
                  HB_ITEM_NEW( hbIndex );

                  hb_itemPutCRawStatic( &hbIndex, szIndex, strlen( szIndex ) );

                  if( hb_hashScan( pSelf, &hbIndex , &ulPos ) )
                  {
                     hb_hashGet( pSelf, ulPos, pMsg );
                  }
                  else
                  {
                     hb_vmClassError( 0, "HASH", szIndex, pSelf );
                  }
               }
            }
            else
            {
               hb_errRT_BASE_SubstR( EG_NOOBJECT, 1004, "Not object", pMsg->item.asSymbol.value->szName, 1, pSelf );
               hb_itemForwardValue( pMsg, &(HB_VM_STACK.Return ) );
            }

            // Symbol was recycled.
            hb_stackPop(); //pSelf.

            w++;
            break;
         }

         case HB_P_BASELINE:
            s_iBaseLine = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            hb_stackBaseItem()->item.asSymbol.lineno = s_iBaseLine;

            //printf( "BASE Proc: %s Line: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.lineno );

            if( s_bDebugging && s_bDebugShowLines )
            {
               hb_vmDebuggerShowLine( s_iBaseLine );
            }

            w += 3;

            HB_TRACE(HB_TR_INFO, ("Opcode: HB_P_LINE: %s (%i)", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.lineno));

            break;

         case HB_P_LINE:
            hb_stackBaseItem()->item.asSymbol.lineno = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

            //printf( "Proc: %s Line: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.lineno );

            if( s_bDebugging && s_bDebugShowLines )
            {
               hb_vmDebuggerShowLine( hb_stackBaseItem()->item.asSymbol.lineno );
            }

            w += 3;

            HB_TRACE(HB_TR_INFO, ("Opcode: HB_P_LINE: %s (%i)", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.lineno));

            break;

         case HB_P_LINEOFFSET:
            hb_stackBaseItem()->item.asSymbol.lineno = s_iBaseLine + (BYTE) pCode[ w + 1 ];

            if( s_bDebugging && s_bDebugShowLines )
            {
               hb_vmDebuggerShowLine( hb_stackBaseItem()->item.asSymbol.lineno );
            }
            w += 2;

            HB_TRACE(HB_TR_INFO, ("Opcode: HB_P_LINEOFFSET: %s (%i)", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.lineno));

            break;

         case HB_P_PARAMETER:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PARAMETER") );
            hb_memvarNewParameter( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), hb_stackItemFromBase( pCode[ w + 3 ] ) );
            w += 4;
            break;

         case HB_P_FRAME:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_FRAME") );
            hb_vmFrame( (unsigned short) pCode[ w + 1 ], pCode[ w + 2 ] );
            w += 3;
            break;

         case HB_P_LARGEFRAME:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_FRAME") );
            hb_vmFrame( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), pCode[ w + 3 ] );
            w += 4;
            break;

         case HB_P_SFRAME:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_SFRAME") );
            hb_vmSFrame( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_STATICS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_STATICS") );
            hb_vmStatics( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), HB_PCODE_MKUSHORT( &( pCode[ w + 3 ] ) ) );
            w += 5;
            break;

         case HB_P_RETVALUE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_RETVALUE") );
            hb_vmRetValue();
            w++;
            break;

         case HB_P_LOCALNAME:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_LOCALNAME") );
            hb_vmLocalName( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ),
                            ( char * ) pCode + w + 3 );
            w += 3;
            while( pCode[ w++ ] ) {}
            break;

         case HB_P_STATICNAME:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_STATICNAME") );
            hb_vmStaticName( HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) ),
                             ( char * ) pCode + w + 4 );
/*
            hb_vmStaticName( pCode[ w + 1 ], HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) ),
                            ( char * ) pCode + w + 4 );
*/
            w += 4;
            while( pCode[ w++ ] ) {}
            break;

         case HB_P_MODULENAME:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_MODULENAME") );
            hb_vmModuleName( ( char * ) pCode + w + 1 );
            while( pCode[ w++ ] ) {}
            break;

         /* BEGIN SEQUENCE/RECOVER/END SEQUENCE */

         case HB_P_TRYBEGIN:
            // Incase recent FINALLY hit a new TRY segment before the ENDFINALLY.
            if( bCanFinalize && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_FINALIZED ) )
            {
               //#define DEBUG_FINALLY

               #ifdef DEBUG_FINALLY
                  printf( "%s-> (TRYBEGIN)- Pending: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->uiActionRequest );
               #endif
            }

            hb_vm_iTry++;

            lFinallySection = 0;
            lCatchSection   = 0;
            lNextSection    = w + HB_PCODE_MKINT24( &pCode[ w + 1 ] );

            #ifdef DEBUG_FINALLY
               printf( "Next: %i at %li\n", pCode[ lNextSection ], lNextSection );
            #endif

            if( pCode[ lNextSection ] == HB_P_FINALLY )
            {
               lFinallySection = lNextSection;

               #ifdef DEBUG_FINALLY
                  printf( "Has finally at: %li and no catcher!\n", lFinallySection );
               #endif
            }
            else
            {
               lCatchSection = lNextSection;
               lNextSection += HB_PCODE_MKINT24( &pCode[ lCatchSection + 1 ] );

               if( pCode[ lNextSection ] == HB_P_FINALLY )
               {
                  lFinallySection = lNextSection;

                  #ifdef DEBUG_FINALLY
                     printf( "Has finally at: %li after catch at: %li\n", lFinallySection, lCatchSection );
                  #endif
               }
               else
               {
                  #ifdef DEBUG_FINALLY
                     printf( "Has catch at: %li and no finally!\n", lCatchSection );
                  #endif
               }
            }
            // Intentionally FALL through.

         case HB_P_SEQBEGIN:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_SEQBEGIN") );

         {
            PHB_SEQUENCE pSequence = (PHB_SEQUENCE) hb_xgrab( sizeof( HB_SEQUENCE ) );

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
            ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;
            hb_stackPush();

            /*
             * 2) store the address of RECOVER or END opcode
             */
            ( * HB_VM_STACK.pPos )->type = HB_IT_LONG;

            pSequence->lRecover = w + HB_PCODE_MKINT24( &pCode[ w + 1 ] );

            /*
             * 3) store current RECOVER base
             */
            pSequence->lBase = hb_stackTopOffset();

            // Make sure it's NIL
            hb_itemClear( hb_stackItem( pSequence->lBase - 1 ) );

            pSequence->uiStatus = 0;

            /*
             * 4) store current bCanRecover flag - in a case of nested sequences
             * in the same procedure/function
             */
            if( bCanRecover )
            {
               pSequence->uiStatus |= HB_SEQ_PRESET_CANRECOVER;
            }

            if( bCanFinalize )
            {
               pSequence->uiStatus |= HB_SEQ_PRESET_CANFINALIZE;
            }

            /*
             * we are now inside a valid SEQUENCE envelope
             */
            bCanRecover = TRUE;

            pSequence->wEnumCollectionCounter = HB_VM_STACK.wEnumCollectionCounter;
            pSequence->wWithObjectCounter     = HB_VM_STACK.wWithObjectCounter;

            pSequence->lFinally        = 0;
            pSequence->uiActionRequest = 0;

            pSequence->pPrev = HB_VM_STACK.pSequence;
            HB_VM_STACK.pSequence = pSequence;

            if( pCode[ w ] == HB_P_TRYBEGIN )
            {
               HB_VM_STACK.pSequence->pPrevErrBlock = hb_errorBlock( hb_vm_BreakBlock );

               bCanFinalize = lFinallySection;

               if( bCanFinalize )
               {
                  HB_VM_STACK.pSequence->lFinally = lFinallySection;

                  if( lCatchSection == 0 )
                  {
                     HB_VM_STACK.pSequence->uiStatus |= ( HB_SEQ_RECOVERED | HB_SEQ_RETHROW );
                  }

                  #ifdef DEBUG_FINALLY
                     printf( "%s->Finally %li at: %li bCanFinalize: %i Prev: %li\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence->lFinally, HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE, HB_VM_STACK.pSequence->pPrev );

                     if( HB_VM_STACK.pSequence->pPrev )
                     {
                        printf( "%s->PREV Finally at: %li\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->pPrev->lFinally );
                     }
                  #endif
               }
            }
            else
            {
               bCanFinalize = FALSE;
            }

            w += 4;
            break;
         }

         case HB_P_TRYEND:
            hb_vm_iTry--;

            hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
            hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );

            #ifdef DEBUG_FINALLY
               printf( "%s->TRYEND: %li Finally: %li\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence->lFinally );
            #endif

            // Intentionally FALL through.

         case HB_P_SEQEND:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_SEQEND") );

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
               pFree = HB_VM_STACK.pSequence;

               /*
                * Restore previous recovery state
                */
               bCanRecover  = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANRECOVER;
               bCanFinalize = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE;

               HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;
               hb_xfree( (void *) pFree );
            }
            else
            {
               HB_VM_STACK.pSequence->uiStatus |= HB_SEQ_RECOVERED;
            }

            /*
             * skip over RECOVER section, if any.
             */
            w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            break;

         case HB_P_TRYRECOVER:
            hb_vm_iTry--;

            hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
            hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );

            #ifdef DEBUG_FINALLY
               printf( "%s->TRYRECOVER: %li Finally: %i bCanFinalize %i got Type: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence->lFinally, bCanFinalize, hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 )->type );
            #endif

            // The HB_VM_STACK.pSequence->lBase will now be poped as stack top, but it must be reserved for potential forwarding to outer!
            hb_stackPush();
            hb_itemForwardValue( hb_stackItemFromTop( -1 ), hb_stackItemFromTop(-2) );

            w += 3;
            // Intentionally FALL through.

         case HB_P_SEQRECOVER:
         {
            PHB_SEQUENCE pFree = HB_VM_STACK.pSequence;

            HB_TRACE( HB_TR_DEBUG, ("HB_P_SEQRECOVER") );

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
               bCanRecover  = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANRECOVER;
               bCanFinalize = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE;

               HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;
               hb_xfree( pFree );
            }
            else
            {
               HB_VM_STACK.pSequence->uiStatus |= HB_SEQ_RECOVERED;
            }

            w++;
            break;
         }

         case HB_P_FINALLY:
           #ifdef DEBUG_FINALLY
             printf( "%s->Finally status %i: Top: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->uiStatus, hb_stackItemFromTop(-1)->type );
           #endif

           HB_VM_STACK.pSequence->uiStatus |= HB_SEQ_FINALIZED;

           w++;
           break;

         case HB_P_ENDFINALLY:
         {
           PHB_SEQUENCE pFree = HB_VM_STACK.pSequence;

           if( HB_VM_STACK.pSequence->uiActionRequest )
           {
              HB_VM_STACK.uiActionRequest = HB_VM_STACK.pSequence->uiActionRequest;

              if( HB_VM_STACK.uiActionRequest & HB_BREAK_REQUESTED )
              {
                 if( HB_VM_STACK.pSequence->pPrev )
                 {
                    hb_itemForwardValue( hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 ), hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 ) );

                    #ifdef DEBUG_FINALLY
                       if( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RETHROW )
                       {
                          printf( "Rethrow type: %i after completed execution\n", hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 )->type );
                       }
                       else
                       {
                          printf( "Forwarded to outer type: %i after completed execution\n", hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 )->type );
                       }
                    #endif
                 }
              }

              #ifdef DEBUG_FINALLY
                 printf( "%s->Restored Deferred Action %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.uiActionRequest );
              #endif
           }

           hb_stackPop();

           bCanRecover  = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANRECOVER;
           bCanFinalize = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE;

           HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;
           hb_xfree( (void *) pFree );

           #ifdef DEBUG_FINALLY
              printf( "%s->Completed execution: %p Restored: %p Finally: %li Pending: %i bCanFinalize: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, pFree, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence ? HB_VM_STACK.pSequence->lFinally : 0, HB_VM_STACK.uiActionRequest, bCanFinalize );
           #endif

           w++;
           break;
         }

         /* Jumps */

         case HB_P_JUMPNEAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMPNEAR") );
            w += (signed char) pCode[ w + 1 ];
            break;

         case HB_P_JUMP:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMP") );
            w += HB_PCODE_MKSHORT( &pCode[ w + 1 ] );
            break;

         case HB_P_JUMPFAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMPFAR") );
            w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            break;

         case HB_P_JUMPFALSENEAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMPFALSENEAR") );

            if( ! hb_vmPopLogical() )
            {
               w += (signed char) pCode[ w + 1 ];
            }
            else
            {
               w += 2;
            }

            break;

         case HB_P_JUMPFALSE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMPFALSE") );

            if( ! hb_vmPopLogical() )
            {
               w += HB_PCODE_MKSHORT( &pCode[ w + 1 ] );
            }
            else
            {
               w += 3;
            }

            break;

         case HB_P_JUMPFALSEFAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMPFALSEFAR") );

            if( ! hb_vmPopLogical() )
            {
               w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            }
            else
            {
               w += 4;
            }

            break;

         case HB_P_JUMPTRUENEAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMPTRUENEAR") );

            if( hb_vmPopLogical() )
            {
               w += (signed char) pCode[ w + 1 ];
            }
            else
            {
               w += 2;
            }

            break;

         case HB_P_JUMPTRUE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMPTRUE") );

            if( hb_vmPopLogical() )
            {
               w += HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) );
            }
            else
            {
               w += 3;
            }

            break;

         case HB_P_JUMPTRUEFAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_JUMPTRUEFAR") );

            if( hb_vmPopLogical() )
            {
               w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            }
            else
            {
               w += 4;
            }

            break;

         /* Push */

         case HB_P_TRUE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_TRUE") );

            ( * HB_VM_STACK.pPos )->type = HB_IT_LOGICAL;
            ( * HB_VM_STACK.pPos )->item.asLogical.value = TRUE;
            hb_stackPush();

            w++;
            break;

         case HB_P_FALSE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_FALSE") );

            ( * HB_VM_STACK.pPos )->type = HB_IT_LOGICAL;
            ( * HB_VM_STACK.pPos )->item.asLogical.value = FALSE;
            hb_stackPush();

            w++;
            break;

         case HB_P_ONE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ONE") );

            ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
            ( * HB_VM_STACK.pPos )->item.asInteger.value = 1;
            ( * HB_VM_STACK.pPos )->item.asInteger.length = 10;
            hb_stackPush();

            w++;
            break;

         case HB_P_ZERO:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ZERO") );

            ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
            ( * HB_VM_STACK.pPos )->item.asInteger.value = 0;
            ( * HB_VM_STACK.pPos )->item.asInteger.length = 10;
            hb_stackPush();

            w++;
            break;

         case HB_P_PUSHNIL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHNIL") );

            ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;
            hb_stackPush();

            w++;
            break;

         case HB_P_PUSHBYTE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHBYTE") );

            ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
            ( * HB_VM_STACK.pPos )->item.asInteger.value = ( signed char ) pCode[ w + 1 ];
            ( * HB_VM_STACK.pPos )->item.asInteger.length = 10;
            hb_stackPush();

            w += 2;
            break;

         case HB_P_PUSHINT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHINT") );

            ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
            ( * HB_VM_STACK.pPos )->item.asInteger.value = HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) );
            ( * HB_VM_STACK.pPos )->item.asInteger.length = 10;
            hb_stackPush();

            w += 3;
            break;

         case HB_P_PUSHLONG:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHLONG") );

#if HB_INT_MAX >= INT32_MAX
            hb_vmPushIntegerConst( ( int ) HB_PCODE_MKLONG( &pCode[ w + 1 ] ) );
#else
            hb_vmPushLongConst( ( LONG ) HB_PCODE_MKLONG( &pCode[ w + 1 ] ) );
#endif
            w += 5;
            break;

         case HB_P_PUSHLONGLONG:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHLONGLONG") );
#if !defined( HB_LONG_LONG_OFF )
            hb_vmPushLongLongConst( HB_PCODE_MKLONGLONG( &pCode[ w + 1 ] ) );
#else
            hb_vmPushDoubleConst( HB_PCODE_MKLONGLONG( &pCode[ w + 1 ] ),
                                  HB_DEFAULT_WIDTH, HB_DEFAULT_DECIMALS );
#endif
            w += 9;

            break;

         case HB_P_PUSHDOUBLE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHDOUBLE") );

            hb_vmPushDoubleConst( HB_PCODE_MKDOUBLE( &pCode[ w + 1 ] ),
                                  ( int ) * ( BYTE * ) &pCode[ w + 1 + sizeof( double ) ],
                                  ( int ) * ( BYTE * ) &pCode[ w + 1 + sizeof( double ) + sizeof( BYTE ) ] );

            w += 1 + sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE );
            break;

         case HB_P_PUSHSTR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHSTR") );
         {
            USHORT uiSize = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

            if( bDynCode )
            {
               hb_vmPushString( ( char * ) pCode + w + 3, ( ULONG ) ( uiSize - 1 ) );
            }
            else
            {
               hb_itemPushStaticString( ( char * ) pCode + w + 3, ( ULONG ) ( uiSize - 1 ) );
            }

            w += ( 3 + uiSize );
            break;
         }

         case HB_P_PUSHSTRSHORT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHSTRSHORT") );
         {
            BYTE uiSize = pCode[ w + 1 ];

            if( bDynCode )
            {
               hb_vmPushString( ( char * ) pCode + w + 2, ( ULONG ) ( uiSize - 1 ) );
            }
            else
            {
               hb_itemPushStaticString( ( char * ) pCode + w + 2, ( ULONG ) ( uiSize - 1 ) );
            }

            w += ( 2 + uiSize );
            break;
         }

         case HB_P_PUSHSTRHIDDEN:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHSTRHIDDEN") );
         {
            ULONG ulSize = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            ULONG ulBufferLen = HB_PCODE_MKUSHORT( &( pCode[ w + 4 ] ) );
            BYTE *pBuffer;

            pBuffer = hb_vmUnhideString( pCode[ w + 3 ], ulSize, pCode + w + 6, ulBufferLen );

            hb_stackPush();
            hb_itemPutCPtr( *( HB_VM_STACK.pPos - 1 ), ( char * ) pBuffer, ulSize - 1 );

            w += ( 6 + ulBufferLen );
            break;
         }

         case HB_P_PUSHBLOCK:
         {
            USHORT usSize = HB_PCODE_MKUSHORT( &pCode[ w + 1 ] );

            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHBLOCK") );
            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5 +6 -> number of referenced local variables
             * +7    -> start of table with referenced local variables
             */
            hb_vmPushBlock( pCode + w + 3, usSize - 3, bDynCode, pSymbols, pGlobals );
            w += usSize;
            break;
         }

         case HB_P_PUSHBLOCKSHORT:
         {
            USHORT usSize = pCode[ w + 1 ];

            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHBLOCKSHORT") );
            /* +0    -> _pushblock
             * +1    -> size of codeblock
             */
            hb_vmPushBlockShort( pCode + w + 2, usSize - 2, bDynCode, pSymbols, pGlobals );
            w += usSize;
            break;
         }

         case HB_P_PUSHSELF:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHSELF") );
            hb_vmPush( hb_stackSelfItem() );
            w++;
            break;

         case HB_P_PUSHWITH:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHWITH") );
            if( HB_VM_STACK.wWithObjectCounter )
            {
               hb_vmPush( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ) );
            }
            else
            {
                hb_vmPushNil();
            }
            w++;
            break;

         case HB_P_PUSHSYM:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHSYM") );
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSYMNEAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHSYMNEAR") );
            hb_vmPushSymbol( pSymbols + ( USHORT ) pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_PUSHALIAS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHALIAS") );
            hb_vmPushAlias();
            w++;
            break;

         case HB_P_PUSHALIASEDFIELD:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ALIASFIELD") );
            hb_vmPushAliasedField( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHALIASEDFIELDNEAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ALIASFIELDNEAR") );
            hb_vmPushAliasedField( pSymbols + pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_PUSHALIASEDVAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ALIASEDVAR") );
            hb_vmPushAliasedVar( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHFIELD:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHFIELD") );
            /* It pushes the current value of the given field onto the eval stack
             */
            hb_stackPush();
            hb_rddGetFieldValue( hb_stackItemFromTop( -1 ), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHGLOBAL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHGLOBAL") );
            //JC1: Should be threadsafe: pCodes are done at compile time, and
            // a clash with another thread here should just bring to a race,
            // not a crash; a race that must be resolved at PRG level.
            hb_vmPush( (*pGlobals)[ pCode[ w + 1 ] ] );
            w += 2;
            break;

         case HB_P_PUSHGLOBALREF:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHGLOBALREF") );
            {
               short iGlobal    = pCode[ w + 1 ];
               PHB_ITEM pTop = ( * HB_VM_STACK.pPos );

               pTop->type = HB_IT_BYREF;

               pTop->item.asRefer.value = iGlobal + 1; // To offset the -1 below.
               pTop->item.asRefer.offset = -1; // Because 0 will be translated as a STATIC in hb_itemUnref();
               pTop->item.asRefer.BasePtr.itemsbasePtr = pGlobals;

#ifdef HB_UNSHARE_REFERENCES
               hb_itemUnShare( (*pGlobals)[ iGlobal ] );
#endif
               hb_stackPush();
            }

            w += 2;
            break;

         case HB_P_PUSHLOCAL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHLOCAL") );
            hb_vmPushLocal( HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHLOCALNEAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHLOCALNEAR") );
            hb_vmPushLocal( ( signed char ) pCode[ w + 1 ] );
            w += 2;  /* only first two bytes are used */
            break;

         case HB_P_LOCALNEARADD:
         {
            PHB_ITEM pLocal = hb_itemUnRef( hb_stackItemFromBase( ( unsigned char ) pCode[ w + 1 ] ) );

            hb_vmPlus( pLocal, hb_stackItemFromTop( -1 ), pLocal );

            hb_stackPop();

            w += 2;
            break;
         }

         case HB_P_LOCALNEARADDINT:
         {
            PHB_ITEM pLocal = hb_stackItemFromBase( pCode[ w + 1 ] );
            int iAdd = HB_PCODE_MKSHORT( &pCode[ w + 2 ] );

            HB_TRACE( HB_TR_DEBUG, ("HB_P_LOCALNEARADDINT") );

            hb_vmAddInt( pLocal, iAdd );
            w += 4;
            break;
         }

         case HB_P_LOCALNEARSETINT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_LOCALNEARSETINT") );
            {
               PHB_ITEM pLocal = hb_stackItemFromBase( (unsigned char) pCode[ w + 1 ] );
               int iNewVal = HB_PCODE_MKSHORT( &( pCode[ w + 2 ] ) );

               if( HB_IS_BYREF( pLocal ) )
               {
                  pLocal = hb_itemUnRef( pLocal );
               }

               if( ( ! HB_IS_NUMBER( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
               {
                  hb_vmPushInteger( iNewVal );
                  hb_vmOperatorCall( pLocal, hb_stackItemFromTop( -1 ), "__OPASSIGN", NULL, 1, pLocal );
               }
               else
               {
                  hb_itemPutNI( pLocal, iNewVal );
               }
               w += 4;
               break;
            }

         case HB_P_LOCALNEARSETSTR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_LOCALNEARSETSTR") );
            {
               PHB_ITEM pLocal = hb_stackItemFromBase( ( unsigned char ) pCode[ w + 1 ] );
               USHORT uiSize = HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) );

               if( HB_IS_BYREF( pLocal ) )
               {
                  pLocal = hb_itemUnRef( pLocal );
               }

               if( ( ! HB_IS_STRING( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
               {
                  if( bDynCode )
                  {
                     hb_vmPushString( ( char * ) pCode + w + 4, ( ULONG ) ( uiSize - 1 ) );
                  }
                  else
                  {
                     hb_itemPushStaticString( ( char * ) pCode + w + 4, ( ULONG ) ( uiSize - 1 ) );
                  }
                  hb_vmOperatorCall( pLocal, hb_stackItemFromTop( -1 ), "__OPASSIGN", NULL, 1, pLocal );
               }
               else
               {
                  if( bDynCode )
                  {
                     hb_itemPutCL( pLocal, ( char * ) ( pCode ) + w + 4, uiSize - 1 );
                  }
                  else
                  {
                     hb_itemPutCRawStatic( pLocal, ( char * ) ( pCode ) + w + 4, uiSize - 1 );
                  }
               }
               w += ( 4 + uiSize );
               break;
            }

         case HB_P_LOCALNEARSETSTRHIDDEN:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_LOCALNEARSETSTRHIDDEN") );
            {
               PHB_ITEM pLocal = hb_stackItemFromBase( ( unsigned char ) pCode[ w + 1 ] );
               ULONG ulSize = HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) );
               ULONG ulBufferLen = HB_PCODE_MKUSHORT( &( pCode[ w + 5 ] ) );
               BYTE *pBuffer;

               pBuffer = hb_vmUnhideString( pCode[ w + 4 ], ulSize, pCode + w + 7, ulBufferLen );

               if( HB_IS_BYREF( pLocal ) )
               {
                  pLocal = hb_itemUnRef( pLocal );
               }

               if( ( ! HB_IS_STRING( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
               {
                  hb_stackPush();
                  hb_itemPutCPtr( hb_stackItemFromTop( -1 ), (char *) pBuffer, ulSize - 1 );
                  hb_vmOperatorCall( pLocal, *( HB_VM_STACK.pPos - 1 ), "__OPASSIGN", NULL, 1, pLocal );
               }
               else
               {
                  hb_itemPutCPtr( pLocal, (char *) pBuffer, ulSize - 1 );
               }
               w += ( 7 + ulBufferLen );
               break;
            }

         case HB_P_ADDINT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ADDINT") );
            {
               PHB_ITEM pItem = hb_stackItemFromTop( -1 );
               int iAdd = HB_PCODE_MKSHORT( &pCode[ w + 1 ] );

               hb_vmAddInt( pItem, iAdd );
               w += 3;
               break;
            }

         case HB_P_SWITCHCASE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_SWITCHCASE") );
            {
               PHB_ITEM pTop = hb_stackItemFromTop( -1 );
               LONG lCase = HB_PCODE_MKLONG( &( pCode[ w + 1 ] ) );

               if( pTop->type & HB_IT_INTEGER )
               {
                  hb_vmPushLogical( (LONG) ( pTop->item.asInteger.value ) == lCase );
               }
               else if( pTop->type & HB_IT_LONG )
               {
                  hb_vmPushLogical( (LONG) pTop->item.asLong.value == lCase );
               }
               else if( pTop->type & HB_IT_STRING && pTop->item.asString.length == 1 )
               {
                  hb_vmPushLogical( (LONG) ( ( BYTE ) pTop->item.asString.value[0] ) == lCase );
               }
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
            HB_TRACE( HB_TR_DEBUG, ("HB_P_LEFT") );
            {
               USHORT iNewLen = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
               PHB_ITEM pString = hb_stackItemFromTop( -1 );

               if( HB_IS_STRING( pString ) )
               {
                  if( ( ULONG ) iNewLen < pString->item.asString.length )
                  {
                     hb_itemPutCL( pString, pString->item.asString.value, iNewLen );
                  }
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
            HB_TRACE( HB_TR_DEBUG, ("HB_P_RIGHT") );
            {
               USHORT iNewLen = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
               PHB_ITEM pString = hb_stackItemFromTop( -1 );

               if( HB_IS_STRING( pString ) )
               {
                  if( ( ULONG ) iNewLen < pString->item.asString.length )
                  {
                     hb_itemPutCL( pString, pString->item.asString.value + pString->item.asString.length - iNewLen, iNewLen );
                  }
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
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHLOCALREF") );
            hb_vmPushLocalByRef( HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSTATIC:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHSTATIC") );
            hb_vmPushStatic( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSTATICREF:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHSTATICREF") );
            hb_vmPushStaticByRef( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHMEMVAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHMEMVAR") );
            hb_memvarGetValue( ( * HB_VM_STACK.pPos ), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_stackPush();
            w += 3;
            break;

         case HB_P_PUSHMEMVARREF:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHMEMVARREF") );
            hb_memvarGetRefer( ( * HB_VM_STACK.pPos ), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_stackPush();
            w += 3;
            break;

         case HB_P_PUSHMACROREF:
         {
            PHB_ITEM pTop = hb_stackItemFromTop( -1 );
            PHB_SYMB pSym = pTop->item.asSymbol.value;

            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHMEMVARREF") );

            hb_memvarGetRefer( pTop, pSym );
            w++;
            break;
         }

         case HB_P_PUSHVARIABLE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_PUSHVARIABLE") );
            /* Push a value of variable of unknown type onto the eval stack
             */
            hb_vmPushVariable( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_DUPLICATE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_DUPLICATE") );
            hb_vmDuplicate();
            w++;
            break;

         case HB_P_DUPLTWO:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_DUPLTWO") );
            hb_vmDuplTwo();
            w++;
            break;

         /* Pop */

         case HB_P_POP:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POP") );
            hb_stackPop();
            w++;
            break;

         case HB_P_POPALIAS:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPALIAS") );
            hb_vmPopAlias();
            w++;
            break;

         case HB_P_POPALIASEDFIELD:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPALIASEDFIELD") );
            hb_vmPopAliasedField( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_POPALIASEDFIELDNEAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPALIASEDFIELDNEAR") );
            hb_vmPopAliasedField( pSymbols + pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_POPALIASEDVAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPALIASEDVAR") );
            hb_vmPopAliasedVar( pSymbols + HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ) );
            w += 3;
            break;

         case HB_P_POPFIELD:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPFIELD") );
            /* Pops a value from the eval stack and uses it to set
             * a new value of the given field
             */
            hb_rddPutFieldValue( hb_stackItemFromTop(-1), pSymbols + HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ) );
            hb_stackPop();

            w += 3;
            break;

         case HB_P_POPGLOBAL:
         {
            PHB_ITEM pTop = hb_stackItemFromTop( -1 );
            PHB_ITEM pGlobal = (*pGlobals)[ pCode[ w + 1 ] ]

            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPGLOBAL") );

            if( ( HB_IS_NUMBER( pGlobal ) && HB_IS_NUMBER( *( HB_VM_STACK.pPos - 1 ) ) ) || pGlobal->type == pTop->type )
            {
               hb_itemForwardValue( pGlobal, pTop );
            }
            else if( hb_objGetOpOver( pGlobal ) & HB_CLASS_OP_ASSIGN )
            {
               hb_vmOperatorCall( pGlobal, pTop, "__OPASSIGN", NULL, 0, pGlobal );
               hb_itemClear( pTop );
            }
            else
            {
              hb_itemForwardValue( pGlobal, pTop );
            }

            hb_stackDec();

            w += 2;
            break;
         }

         case HB_P_POPLOCAL:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPLOCAL") );
            hb_vmPopLocal( HB_PCODE_MKSHORT( &pCode[ w + 1 ] ) );
            w += 3;
            break;

         case HB_P_POPLOCALNEAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPLOCALNEAR") );
            hb_vmPopLocal( ( signed char ) pCode[ w + 1 ] );
            w += 2;  /* only first two bytes are used */
            break;

         case HB_P_POPSTATIC:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPSTATIC") );
            hb_vmPopStatic( HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ) );
            w += 3;
            break;

         case HB_P_POPMEMVAR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPMEMVAR") );
            hb_memvarSetValue( pSymbols + HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ),
                               hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            w += 3;
            break;

         case HB_P_POPVARIABLE:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_POPVARIABLE") );
            /*
               2004-03-19 Ron Pinkas
               Test with Clipper shows that for assignment, MEMVAR context is always used even if MEMVAR
               does NOT exists, and a FIELD with this name exists!!!

               Here is the Test Ueed - Clipper produced NO R/T Error - indicating MEMVAR was created.

                 PROCEDURE Main()

                    USE Test
                    First := First
                    CLOSE
                    ? First

                 RETURN
            */
            hb_memvarSetValue( pSymbols + HB_PCODE_MKUSHORT( &pCode[ w + 1 ] ),
                               hb_stackItemFromTop(-1) );
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
            hb_macroPopAliasedValue( hb_stackItemFromTop( - 2  ), hb_stackItemFromTop( -1 ), pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSH:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSHARG:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHARG, pCode[ ++w ] );
            w++;

            if( HB_VM_STACK.iExtraParamsIndex && HB_VM_STACK.apExtraParamsSymbol[HB_VM_STACK.iExtraParamsIndex - 1] == NULL )
            {
               if( pCode[w] == HB_P_PUSHSYMNEAR )
               {
                  HB_VM_STACK.apExtraParamsSymbol[HB_VM_STACK.iExtraParamsIndex - 1] = pSymbols + ( USHORT ) ( pCode[w + 1] );
                  w += 2;
               }
               else if( pCode[w] == HB_P_MPUSHSYM )
               {
                  HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );

                  HB_VM_STACK.apExtraParamsSymbol[HB_VM_STACK.iExtraParamsIndex - 1] = pDynSym->pSymbol;
                  w += sizeof( HB_DYNS_PTR ) + 1;
               }
               else
               {
                  HB_VM_STACK.apExtraParamsSymbol[HB_VM_STACK.iExtraParamsIndex - 1] = pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
                  w += 3;
               }
            }
            else
            {
               if( pCode[w] == HB_P_PUSHSYMNEAR )
               {
                  w += 2;
               }
               else if( pCode[w] == HB_P_MPUSHSYM )
               {
                  w += sizeof( HB_DYNS_PTR ) + 1;
               }
               else
               {
                  w += 3;
               }
            }
            break;

         case HB_P_MACROLIST:
            HB_VM_STACK.aiExtraElements[HB_VM_STACK.iExtraElementsIndex++] = 0;

            w++;
            break;

         case HB_P_MACROPUSHLIST:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHLIST, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROLISTEND:
            HB_VM_STACK.iExtraElements = HB_VM_STACK.aiExtraElements[--HB_VM_STACK.iExtraElementsIndex];

            w++;
            break;

         case HB_P_MACROPUSHINDEX:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHINDEX, pCode[ ++w ] );

            if( HB_VM_STACK.iExtraIndex )
            {
               HB_ITEM *aExtraItems = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) * HB_VM_STACK.iExtraIndex );
               int i;

               /* Storing and removing the extra indexes. */
               for ( i = HB_VM_STACK.iExtraIndex - 1; i >= 0; i-- )
               {
                  ( aExtraItems + i )->type = HB_IT_NIL;
                  hb_itemMove( aExtraItems + i, hb_stackItemFromTop(-1) );
                  hb_stackPop();
               }

               /* First index is still on stack.*/
               hb_vmArrayPush();

               /* Now process each of the additional index including the last one (we will skip the HB_P_ARRAYPUSH which is know to follow . */
               for ( i = 0; i < HB_VM_STACK.iExtraIndex; i++ )
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
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHPARE, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSHALIASED:
            /* compile and run - leave an aliased variable on the stack */
            hb_macroPushAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROSYMBOL:
            /* compile into a symbol name (used in function calls) */
            hb_macroPushSymbol( hb_stackItemFromTop( -1 ) );
            w++;
            break;

         case HB_P_MACROTEXT:
            /* macro text substitution
             * "text &macro.other text"
             */
            hb_macroTextValue( hb_stackItemFromTop( -1 ) );
            w++;
            break;

         /* macro compiled opcodes - we are using symbol address here */

         case HB_P_MMESSAGE:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );

            hb_vmPushSymbol( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPOPALIASEDFIELD:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );

            hb_vmPopAliasedField( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPOPALIASEDVAR:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPopAliasedVar( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPOPFIELD:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );

            /* Pops a value from the eval stack and uses it to set
            * a new value of the given field
            */
            hb_rddPutFieldValue( hb_stackItemFromTop(-1), pDynSym->pSymbol );
            hb_stackPop();

            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPOPMEMVAR:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            PHB_ITEM pTop;

            pTop = *( HB_VM_STACK.pPos - 1 );

            hb_memvarSetValue( pDynSym->pSymbol, pTop );

            hb_stackPop();

            HB_TRACE(HB_TR_INFO, ("(hb_vmMPopMemvar)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHALIASEDFIELD:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushAliasedField( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHALIASEDVAR:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushAliasedVar( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHBLOCK:
         {
            /*NOTE: the pcode is stored in dynamically allocated memory
            * We need to handle it with more care than compile-time
            * codeblocks
            */
            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5    -> pcode bytes
             */
            hb_vmPushMacroBlock( ( BYTE * ) ( pCode + w ), pSymbols );
            w += HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            break;
         }

         case HB_P_MPUSHFIELD:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            /* It pushes the current value of the given field onto the eval stack
            */
            hb_stackPush();
            hb_rddGetFieldValue( hb_stackItemFromTop( -1 ), pDynSym->pSymbol );
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushField)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHMEMVAR:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_memvarGetValue( ( * HB_VM_STACK.pPos ), pDynSym->pSymbol );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushMemvar)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHMEMVARREF:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_memvarGetRefer( ( * HB_VM_STACK.pPos ), pDynSym->pSymbol );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushMemvarRef)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHSYM:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushSymbol( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHVARIABLE:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushVariable( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHSTR:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_MPUSHSTR") );
         {
            USHORT uiSize = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

            hb_vmPushString( ( char * ) ( pCode + w + 3 ), ( ULONG )( uiSize - 1 ) );

            w += ( 3 + uiSize );
            break;
         }

         /* misc */

         case HB_P_NOOP:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_NOOP") );
            /* Intentionally do nothing */
            w++;
            break;

         case HB_P_ENDBLOCK:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_ENDBLOCK") );
            hb_vmEndBlock();

            /* IMHO It will be cleaner to make exactly the same action here
               as for HB_P_ENDPROC, instead of this direct return, Druzus */

            /* end of a codeblock - stop evaluation */
            if( pSymbols )
            {
               hb_memvarSetPrivatesBase( ulPrivateBase );
            }

            /*
             * *** NOTE!!! Return!!!
             */
            return;

         case HB_P_ENDPROC:
            HB_TRACE(HB_TR_INFO, ("HB_P_ENDPROC"));
            /* manually inlined hb_vmRequestEndProc() for some C compilers
             * which does not make such optimisation
             */
            HB_VM_STACK.uiActionRequest = HB_ENDPROC_REQUESTED;
            break;

         default:
            /* TODO: Include to failing pcode in the error message */
            hb_errInternal( HB_EI_VMBADOPCODE, NULL, NULL, NULL );
            break;
      }

      /* Accepts generalized quit request */
      #ifdef HB_THREAD_SUPPORT
      if ( hb_vm_bQuitRequest )
      {
         HB_VM_STACK.uiActionRequest = HB_QUIT_REQUESTED;
      }
      #endif

      if( HB_VM_STACK.uiActionRequest )
      {
         // Incase recent FINALLY hit a RETURN or BREAK before the ENDFINALLY.
         if( bCanFinalize && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_FINALIZED ) )
         {
            PHB_SEQUENCE pFree = HB_VM_STACK.pSequence;

            // Restore the deferred action overriding the new request.
            if( HB_VM_STACK.uiActionRequest != HB_QUIT_REQUESTED && HB_VM_STACK.pSequence->uiActionRequest )
            {
               HB_VM_STACK.uiActionRequest = HB_VM_STACK.pSequence->uiActionRequest;
            }

            if( HB_VM_STACK.uiActionRequest & HB_BREAK_REQUESTED )
            {
               if( HB_VM_STACK.pSequence->pPrev )
               {
                  hb_itemForwardValue( hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 ), hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 ) );

                  #ifdef DEBUG_FINALLY
                     if( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RETHROW )
                     {
                        printf( "Rethrow type: %i after partial execution\n", hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 )->type );
                     }
                     else
                     {
                        printf( "Forwarded to outer type: %i after partial execution\n", hb_stackItem( HB_VM_STACK.pSequence->pPrev->lBase - 1 )->type );
                     }
                  #endif
               }
            }

            bCanRecover  = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANRECOVER;
            bCanFinalize = HB_VM_STACK.pSequence->uiStatus & HB_SEQ_PRESET_CANFINALIZE;

            HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;
            hb_xfree( (void *) pFree );

            #ifdef DEBUG_FINALLY
               printf( "%s->Removed partially executed: %p Restored: %p Finally: %i Pending: %i bCanFinalize: %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, pFree, HB_VM_STACK.pSequence, HB_VM_STACK.pSequence ? HB_VM_STACK.pSequence->lFinally : 0, HB_VM_STACK.uiActionRequest, bCanFinalize );
            #endif
         }

         if( HB_VM_STACK.uiActionRequest & HB_ENDPROC_REQUESTED )
         {
            /* request to stop current procedure was issued
             * (from macro evaluation)
             */
            if( bCanFinalize && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_FINALIZED ) == 0 )
            {
               #ifdef DEBUG_FINALLY
                  printf( "%s->DEFER RETURN - go to %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->lFinally );
               #endif

               HB_VM_STACK.pSequence->uiStatus |= HB_SEQ_FINALIZED;
               HB_VM_STACK.pSequence->uiActionRequest = HB_ENDPROC_REQUESTED;

               w = HB_VM_STACK.pSequence->lFinally;
               HB_VM_STACK.uiActionRequest = 0;
            }
            else
            {
               HB_VM_STACK.uiActionRequest = 0;
               break;
            }
         }
         else if( HB_VM_STACK.uiActionRequest & HB_BREAK_REQUESTED )
         {
            if( bCanFinalize && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RECOVERED ) && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_FINALIZED ) == 0 )
            {
               #ifdef DEBUG_FINALLY
                  printf( "%s->DEFER BREAK - go to %i\n", hb_stackBaseItem()->item.asSymbol.value->szName, HB_VM_STACK.pSequence->lFinally );
               #endif

               HB_VM_STACK.pSequence->uiStatus |= HB_SEQ_FINALIZED;
               HB_VM_STACK.pSequence->uiActionRequest = HB_BREAK_REQUESTED;

               // We didn't realy have a recover section, so do the bCanRecover too!
               if( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RETHROW )
               {
                  assert( HB_VM_STACK.pSequence->lFinally == HB_VM_STACK.pSequence->lRecover );

                  #ifdef DEBUG_FINALLY
                     printf( "%s->Doing bCanRecover too\n", hb_stackBaseItem()->item.asSymbol.value->szName );
                  #endif

                  goto hb_vmExecute_CanRecover;
               }
               else
               {
                  w = HB_VM_STACK.pSequence->lFinally;
                  HB_VM_STACK.uiActionRequest = 0;
               }
            }
            else if( bCanRecover && ( HB_VM_STACK.pSequence->uiStatus & HB_SEQ_RECOVERED ) == 0 )
            {
              hb_vmExecute_CanRecover:

               // Reset FOR EACH.
               while( HB_VM_STACK.wEnumCollectionCounter > HB_VM_STACK.pSequence->wEnumCollectionCounter )
               {
                  HB_VM_STACK.wEnumCollectionCounter--;
                  hb_itemClear( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ) );
                  hb_itemClear( HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] );
                  HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;
               }

               // Reset WITH OBJECT.
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
               hb_stackRemove( HB_VM_STACK.pSequence->lBase );

               w = HB_VM_STACK.pSequence->lRecover;

               /*
                * leave the SEQUENCE envelope on the stack - it will
                * be popped either in RECOVER or END opcode
                */
               HB_VM_STACK.uiActionRequest = 0;
            }
            else
            {
               break;
            }
         }
         else if( HB_VM_STACK.uiActionRequest & HB_QUIT_REQUESTED )
         {
            #ifdef HB_THREAD_SUPPORT
               /* Generalize quit request so that the whole VM is affected */
               hb_vm_bQuitRequest = TRUE;
            #endif

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
         if( hb_set.HB_SET_BACKGROUNDTASKS )
         {
            hb_backgroundRun();
         }
      }
#endif

   }

   /* No cancellation here */
   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

   HB_TRACE(HB_TR_DEBUG, ("DONE hb_vmExecute(%p, %p)", pCode, pSymbols));

   if( pSymbols )
   {
      hb_memvarSetPrivatesBase( ulPrivateBase );
   }

   HB_TRACE(HB_TR_DEBUG, ("RESET PrivateBase hb_vmExecute(%p, %p)", pCode, pSymbols));

   // Reset FOR EACH.
   while( HB_VM_STACK.wEnumCollectionCounter > wEnumCollectionCounter )
   {
      HB_VM_STACK.wEnumCollectionCounter--;
      hb_itemClear( &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter ] ) );
      //hb_itemClear( HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] );
      HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;
   }

   // Reset WITH OBJECT.
   while( HB_VM_STACK.wWithObjectCounter > wWithObjectCounter )
   {
      --HB_VM_STACK.wWithObjectCounter;
      hb_itemClear( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter ] ) );
   }

   //JC1: do not allow cancellation or idle MT func: thread cleanup procedure
   // is under way, or another VM might return in control

   //TraceLog( NULL, "DONE! %s->hb_vmExecute(%p, %p, %p)\n", hb_stackBaseItem()->item.asSymbol.value->szName, pCode, pSymbols, pGlobals );
}

HB_FUNC( HB_VMEXECUTE )
{
   HB_THREAD_STUB_STACK

   const BYTE *szString = (const BYTE *) hb_parc(1);

   if( szString )
   {
      LONG lOffset = hb_stackTopOffset();

      hb_vmExecute( szString, (PHB_SYMB) hb_parptr(2), (PHB_ITEM **) hb_parptr(3) );

      if( hb_stackTopOffset() > lOffset )
      {
         #if 1
            if( HB_VM_STACK.uiActionRequest == 0 )
            {
               hb_itemForwardValue( &(HB_VM_STACK.Return ), hb_stackItemFromTop( -1 ) );

               do
               {
                  hb_stackPop();
               }
               while( hb_stackTopOffset() > lOffset );
            }
         #else
            hb_itemCopy( &(HB_VM_STACK.Return ), hb_stackItemFromTop( -1 ) );
         #endif
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "HB_vmExecute", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError(3) );
   }
}

/* ------------------------------- */
/* Operators ( mathematical        */
/*             character / misc )  */
/* ------------------------------- */

static void hb_vmAddInt( HB_ITEM_PTR pResult, LONG lAdd )
{
   HB_THREAD_STUB_STACK

   double dNewVal;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmAddInt(%p,%ld)", pResult, lAdd));

   if( HB_IS_BYREF( pResult ) )
   {
      pResult = hb_itemUnRef( pResult );
   }

   if( HB_IS_NUMINT( pResult ) )
   {
      HB_LONG lVal = HB_ITEM_GET_NUMINTRAW( pResult ), lNewVal;

      lNewVal = lVal + lAdd;

      if( lAdd >= 0 ? lNewVal >= lVal : lNewVal <  lVal )
      {
         HB_ITEM_PUT_NUMINTRAW( pResult, lNewVal );
         return;
      }
      else
      {
         dNewVal = ( double ) lVal + ( double ) lAdd;
      }
   }
   else if( HB_IS_DATE( pResult ) )
   {
      pResult->item.asDate.value += lAdd;
      return;
   }
   else if( HB_IS_STRING( pResult ) && pResult->item.asString.length == 1 )
   {
      hb_itemPutCLStatic( pResult, hb_szAscii[ ( UCHAR ) ( pResult->item.asString.value[ 0 ] + lAdd ) ], 1 );
      return;
   }
   else if( pResult->type & HB_IT_DOUBLE )
   {
      dNewVal = pResult->item.asDouble.value + lAdd;
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
      PHB_ITEM pSubst, pAdd = hb_stackTopItem();

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

   if( !HB_IS_DOUBLE( pResult ) )
   {
      pResult->type = HB_IT_DOUBLE;
      pResult->item.asDouble.decimal = 0;
   }

   pResult->item.asDouble.value = dNewVal;
   pResult->item.asDouble.length = HB_DBL_LENGTH( dNewVal );
}

/* NOTE: Clipper is resetting the number width on a negate. */

static void hb_vmNegate( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmNegate()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_INTEGER( pItem ) )
   {
#if -HB_INT_MAX > HB_INT_MIN
      if ( pItem->item.asInteger.value < -HB_INT_MAX )
      {
#if HB_LONG_MAX > HB_INT_MAX
         HB_LONG lValue = ( HB_LONG ) pItem->item.asInteger.value;
         pItem->type = HB_IT_LONG;
         pItem->item.asLong.value = -lValue;
         pItem->item.asLong.length = HB_LONG_LENGTH( -lValue );
#else
         double dValue = ( double ) pItem->item.asInteger.value;
         pItem->type = HB_IT_DOUBLE;
         pItem->item.asDouble.value = -dValue;
         pItem->item.asDouble.length = HB_DBL_LENGTH( -dValue );
#endif
      }
      else
#endif
      {
         pItem->item.asInteger.value = -pItem->item.asInteger.value;
         pItem->item.asInteger.length = HB_INT_LENGTH( pItem->item.asInteger.value );
      }
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if -HB_LONG_MAX > HB_LONG_MIN
      if ( pItem->item.asLong.value < -HB_LONG_MAX )
      {
         double dValue = ( double ) pItem->item.asLong.value;
         pItem->type = HB_IT_DOUBLE;
         pItem->item.asDouble.value = -dValue;
         pItem->item.asDouble.length = HB_DBL_LENGTH( -dValue );
      }
      else
#endif
      {
         pItem->item.asLong.value = -pItem->item.asLong.value;
         pItem->item.asLong.length = HB_LONG_LENGTH( pItem->item.asLong.value );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asDouble.value = -pItem->item.asDouble.value;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
   {
      pItem->item.asInteger.value = - ( int ) ( BYTE ) pItem->item.asString.value[0];
      pItem->type = HB_IT_INTEGER;
      pItem->item.asInteger.length = 10;
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

   //printf( "Left: %i, Right: %i\n", pLeft->type, pRight->type );

   // Must be first because STRING (length 1) qualifies as NUMERIC!
   if( HB_IS_STRING( pLeft ) && HB_IS_STRING( pRight ) )
   {
      ULONG ulLen1 = pLeft->item.asString.length;
      ULONG ulLen2 = pRight->item.asString.length;

      //printf( "Adding '%s' + '%s'\n", pItem1->item.asString.value, pItem2->item.asString.value );

      if( ulLen2 )
      {
         if( ulLen1 )
         {
            //if( ( double ) ( ( double ) ulLen1 + ( double ) ulLen2 ) < ( double ) ULONG_MAX )
            if( ulLen1 < ULONG_MAX - ulLen2 )
            {
               ULONG ulNewLen = ulLen1 + ulLen2;

               if( pResult->item.asString.allocated && ( *( pResult->item.asString.pulHolders ) == 1 ) )
               {
                  __HB_STRING_REALLOC( pResult, ulNewLen );

                  if ( pLeft != pResult )
                  {
                     hb_xmemcpy( pResult, pLeft->item.asString.value, ulLen1 );
                  }
                  hb_xmemcpy( (void *) ( pResult->item.asString.value + ulLen1 ), (void *) pRight->item.asString.value, ulLen2 );

               } else {
                  char *sResult = (char *) hb_xgrab( ulNewLen + 1 );

                  hb_xmemcpy( sResult, pLeft->item.asString.value, ulLen1 );
                  hb_xmemcpy( sResult + ulLen1, pRight->item.asString.value, ulLen2 );
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
         {
            hb_itemCopy( pResult, pRight );
         }
      }
      else
      {
         if( pResult != pLeft )
         {
            hb_itemCopy( pResult, pLeft );
         }
      }
   }
   else if( ( HB_IS_DATE( pLeft ) || HB_IS_DATE( pRight ) ) && ( HB_IS_NUMERIC( pLeft ) && HB_IS_NUMERIC( pRight ) ) )
   {
      hb_itemPutDL( pResult, (LONG) hb_itemGetND( pLeft ) + (LONG) hb_itemGetND( pRight ) );
   }
   else if( HB_IS_STRING( pLeft ) && ( HB_IS_NUMERIC( pLeft ) && HB_IS_NUMERIC( pRight ) ) )
   {
      hb_itemPutCLStatic( pResult, hb_szAscii[ ( UCHAR ) ( pLeft->item.asString.value[ 0 ] + (LONG) hb_itemGetND( pRight ) ) ], 1 );
   }
   #if 1 // Shoud: 1 + "a" produce "b" like 1 + Date() produces Date() + 1?
   else if( HB_IS_STRING( pRight ) && ( HB_IS_NUMERIC( pLeft ) && HB_IS_NUMERIC( pRight ) ) )
   {
      hb_itemPutCLStatic( pResult, hb_szAscii[ ( UCHAR ) ( pRight->item.asString.value[ 0 ] + (LONG) hb_itemGetND( pLeft ) ) ], 1 );
   }
   #endif
   else if( HB_IS_NUMINT( pLeft ) && HB_IS_NUMINT( pRight ) )
   {
      HB_LONG lNumber1 = hb_itemGetNInt( pLeft );
      HB_LONG lNumber2 = hb_itemGetNInt( pRight );
      HB_LONG lResult = lNumber1 + lNumber2;

      if( lNumber2 >= 0 ? lResult >= lNumber1 : lResult < lNumber1 )
      {
         hb_itemPutNInt( pResult, lResult );
      }
      else
      {
         hb_itemPutNDDec( pResult, ( double ) lNumber1 + ( double ) lNumber2, 0 );
      }
   }
   else if( HB_IS_NUMERIC( pLeft ) && HB_IS_NUMERIC( pRight ) )
   {
      int iDec1, iDec2, iType1 = pLeft->type, iType2 = pRight->type;
      double dNumber1 = hb_itemGetNDDec( pLeft, &iDec1 );
      double dNumber2 = hb_itemGetNDDec( pRight, &iDec2 );

      hb_itemPutNumType( pResult, dNumber1 + dNumber2, ( ( iDec1 > iDec2 ) ? iDec1 : iDec2 ), iType1, iType2 );
   }
   else if( hb_objGetOpOver( pLeft ) & HB_CLASS_OP_PLUS )
   {
      hb_vmOperatorCall( pLeft, pRight, "__OPPLUS", NULL, 0, pResult );
   }
   else if( HB_IS_HASH( pLeft ) && HB_IS_HASH( pRight ) )
   {
      ULONG ulLen = pRight->item.asHash.value->ulTotalLen;
      HB_ITEM_NEW( hbNum );
      HB_ITEM_NEW( HashResult );

      hb_hashClone( pLeft, &HashResult );
      hb_itemPutNI( &hbNum, 0 ); // normal merge mode

      hb_hashMerge( &HashResult, pRight, 1, ulLen, &hbNum );
      hb_itemForwardValue( pResult, &HashResult );
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

static void hb_vmMinus( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmMinus()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   // Must be first because STRING (length 1) qualifies as NUMERIC!
   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      ULONG ulLen1 = pItem1->item.asString.length;
      ULONG ulLen2 = pItem2->item.asString.length;

      if( ulLen1 < ULONG_MAX - ulLen2 )
      {
         char * szNewString = ( char * ) hb_xgrab( ulLen1 + ulLen2 + 1 );
         ULONG ulNewLen = ulLen1 + ulLen2;

         while( ulLen1 && pItem1->item.asString.value[ ulLen1 - 1 ] == ' ' )
         {
            ulLen1--;
         }

         hb_xmemcpy( szNewString, pItem1->item.asString.value, ulLen1 );
         hb_xmemcpy( szNewString + ulLen1, pItem2->item.asString.value, ulLen2 );
         hb_xmemset( szNewString + ulLen1 + ulLen2, ' ', pItem1->item.asString.length - ulLen1 );
         szNewString[ ulNewLen ] = '\0';

         HB_TRACE( HB_TR_DEBUG, ( "Released hb_vmMinus() Created \"%s\"", szNewString ) );

         hb_itemPutCPtr( pItem1, szNewString, ulNewLen );

         hb_stackPop();
      }
      else
      {
         hb_errRT_BASE( EG_STROVERFLOW, 1210, NULL, "-", 2, pItem1, pItem2 );
      }
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_NUMBER( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();

      hb_vmPushDate( (LONG) dNumber1 - (LONG) dNumber2 );
   }
   else if( HB_IS_STRING( pItem1 ) && ( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) ) )
   {
      hb_itemPutCLStatic( pItem1, hb_szAscii[ ( UCHAR ) ( pItem1->item.asString.value[ 0 ] - (LONG) hb_itemGetND( pItem2 ) ) ], 1 );
      hb_stackPop();
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lNumber2 = hb_vmPopHBLong();
      HB_LONG lNumber1 = hb_vmPopHBLong();
      HB_LONG lResult = lNumber1 - lNumber2;

      if ( lNumber2 <= 0 ? lResult >= lNumber1 : lResult < lNumber1 )
      {
         hb_vmPushNumInt( lResult );
      }
      else
      {
         hb_vmPushDouble( ( double ) lNumber1 - ( double ) lNumber2, 0 );
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      int iDec2, iDec1, iType2 = pItem2->type, iType1 = pItem1->type;
      double dNumber2 = hb_vmPopDouble( &iDec2 );
      double dNumber1 = hb_vmPopDouble( &iDec1 );

      hb_vmPushNumType( dNumber1 - dNumber2, ( ( iDec1 > iDec2 ) ? iDec1 : iDec2 ), iType1, iType2 );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_MINUS )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPMINUS", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
   }
   else if( HB_IS_HASH( pItem1 ) && HB_IS_HASH( pItem2 ) )
   {
      ULONG ulLen = pItem2->item.asHash.value->ulTotalLen;
      HB_ITEM_NEW( hbNum );
      HB_ITEM_NEW( HashResult );

      hb_hashClone( pItem1, &HashResult );
      hb_itemPutNI( &hbNum, 3 ); // NOT mode

      hb_hashMerge( &HashResult, pItem2, 1, ulLen, &hbNum );
      hb_stackPop();
      hb_stackPop();
      hb_itemPushForward( &HashResult );
   }
   else if( HB_IS_HASH( pItem1 ) && HB_IS_ARRAY( pItem2 ) )
   {
      ULONG ulLen = pItem2->item.asArray.value->ulLen;
      ULONG ulPos;
      HB_ITEM_NEW( HashResult );
      PHB_ITEM pRef = pItem2->item.asArray.value->pItems;

      hb_hashClone( pItem1, &HashResult );
      while( ulLen > 0 )
      {
         if( hb_hashScan( &HashResult, pRef, &ulPos ) )
         {
            hb_hashRemove( &HashResult, ulPos );
         }
         pRef++;
         ulLen --;
      }
      hb_stackPop();
      hb_stackPop();
      hb_itemPushForward( &HashResult );
   }
   else if( HB_IS_HASH( pItem1 ) && (
      pItem2->type & ( HB_IT_NUMERIC | HB_IT_STRING | HB_IT_DATE ) ) )
   {
      ULONG ulPos;
      HB_ITEM_NEW( HashResult );

      if ( hb_hashScan( pItem1, pItem2, &ulPos ) )
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

static void hb_vmMult( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmMult()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      int iDec2, iDec1, iType2 = pItem2->type, iType1 = pItem1->type;
      double d2 = hb_vmPopDouble( &iDec2 );
      double d1 = hb_vmPopDouble( &iDec1 );
      hb_vmPushNumType( d1 * d2, iDec1 + iDec2, iType1, iType2 );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_MULT )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPMULT", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDivide()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

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
            push the number without decimals. Clipper compatible. Actually,
            this is not Clipper compatible. The only time Clipper returns 0
            decimal places is for compiler optimized integer division with an
            integer result. Therefore this code is not needed and has been
            removed - David G. Holm <dholm@jsd-llc.com>
         if( bIntegerOperands && fmod( hb_itemGetND( pItem1 ), dDivisor ) == 0.0 )
            hb_vmPushNumber( hb_vmPopNumber() / dDivisor, 0 );
         else
         */
         hb_vmPushDouble( hb_vmPopNumber() / dDivisor, hb_set.HB_SET_DECIMALS );
      }
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_DIVIDE )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPDIVIDE", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmModulus()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lDivisor = HB_ITEM_GET_NUMINTRAW( pItem2 );

      if ( lDivisor == 0 )
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
         pItem2->type = HB_IT_NIL;
         hb_stackDec(); /* pop divisor from the stack */
         hb_stackDec();
         /* NOTE: Clipper always returns the result of modulus
                  with the SET number of decimal places. */
         if ( hb_set.HB_SET_DECIMALS == 0 )
            hb_vmPushNumInt( HB_ITEM_GET_NUMINTRAW( pItem1 ) % lDivisor );
         else
            hb_vmPushDouble( ( double ) ( HB_ITEM_GET_NUMINTRAW( pItem1 ) % lDivisor ), hb_set.HB_SET_DECIMALS );
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
         hb_vmPushDouble( fmod( hb_vmPopNumber(), dDivisor ), hb_set.HB_SET_DECIMALS );
      }
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_MOD )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPMOD", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPower()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double d2 = hb_vmPopNumber();
      double d1 = hb_vmPopNumber();

      /* NOTE: Clipper always returns the result of power
               with the SET number of decimal places. */
      hb_vmPushDouble( pow( d1, d2 ), hb_set.HB_SET_DECIMALS );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_POWER )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPPOWER", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmInc()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
   {
      hb_itemPutCLStatic( pItem, hb_szAscii[ ( UCHAR ) ( pItem->item.asString.value[ 0 ] + 1 ) ], 1 );
   }
   else if( HB_IS_DATE( pItem ) )
   {
      pItem->item.asDate.value++;
   }
   else if( HB_IS_NUMINT( pItem  ) )
   {
      if( HB_IS_INTEGER( pItem ) && pItem->item.asInteger.value < HB_INT_MAX )
      {
         pItem->item.asInteger.value++;
         pItem->item.asInteger.length = HB_INT_LENGTH( pItem->item.asInteger.value );
      }
      else if( HB_IS_LONG( pItem ) && pItem->item.asLong.value < HB_LONG_MAX )
      {
         pItem->item.asLong.value++;
         pItem->item.asLong.length = HB_LONG_LENGTH( pItem->item.asLong.value );
      }
      else
      {
         int iDec, iType = pItem->type;
         double dNumber = hb_vmPopDouble( &iDec );
         hb_vmPushNumType( ++dNumber, iDec, iType, iType );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asDouble.value++;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( hb_objGetOpOver( pItem ) & HB_CLASS_OP_INC )
   {
      hb_vmOperatorCallUnary( pItem, "__OPINC", pItem );
   }
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDec()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pItem ) && pItem->item.asString.length == 1 )
   {
      hb_itemPutCLStatic( pItem, hb_szAscii[ ( UCHAR ) ( pItem->item.asString.value[ 0 ] - 1 ) ], 1 );
   }
   else if( HB_IS_DATE( pItem ) )
   {
      pItem->item.asDate.value--;
   }
   else if( HB_IS_NUMINT( pItem  ) )
   {
      if( HB_IS_INTEGER( pItem ) && pItem->item.asInteger.value > HB_INT_MIN )
      {
         pItem->item.asInteger.value--;
         pItem->item.asInteger.length = HB_INT_LENGTH( pItem->item.asInteger.value );
      }
      else if( HB_IS_LONG( pItem ) && pItem->item.asLong.value > HB_LONG_MIN )
      {
         pItem->item.asLong.value--;
         pItem->item.asLong.length = HB_LONG_LENGTH( pItem->item.asLong.value );
      }
      else
      {
         int iDec, iType = pItem->type;
         double dNumber = hb_vmPopDouble( &iDec );
         hb_vmPushNumType( --dNumber, iDec, iType, iType );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asDouble.value--;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( hb_objGetOpOver( pItem ) & HB_CLASS_OP_DEC )
   {
      hb_vmOperatorCallUnary( pItem, "__OPDEC", pItem );
   }
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

static void hb_vmFuncPtr( void )  /* pushes a function address pointer. Removes the symbol from the satck */
{
   HB_THREAD_STUB

   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmFuncPtr()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_SYMBOL( pItem ) )
   {
      pItem->item.asPointer.value = (void *) pItem->item.asSymbol.value;
      pItem->item.asPointer.collect = FALSE;
      pItem->type = HB_IT_POINTER;
   }
   else
   {
      hb_errInternal( HB_EI_VMNOTSYMBOL, NULL, "hb_vmFuncPtr()", NULL );
   }
}

/* ------------------------------- */
/* Operators (relational)          */
/* ------------------------------- */

static void hb_vmEqual( BOOL bExact )
{
   HB_THREAD_STUB

   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmEqual(%d)", (int) bExact));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_NIL( pItem1 ) )
   {
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = HB_IS_NIL( pItem2 );
      hb_stackPop();    /* clear the pItem2 */
   }
   else if( HB_IS_NIL( pItem2 ) )
   {
      hb_stackDec();    /* pItem2 is already NIL */
      hb_stackPop();    /* clear the pItem1 */
      hb_vmPushLogical( FALSE );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, bExact );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i == 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) ==
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      hb_vmPushLogical( hb_vmPopNumber() == hb_vmPopNumber() );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asLogical.value ==
                                       pItem2->item.asLogical.value);
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_POINTER( pItem1 ) && HB_IS_POINTER( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asPointer.value ==
                                       pItem2->item.asPointer.value );
      pItem1->type = HB_IT_LOGICAL;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_EQUAL && bExact == FALSE)
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPEQUAL", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_EXACTEQUAL && bExact )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPEXACTEQUAL", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmNotEqual()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_NIL( pItem1 ) )
   {
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = ! HB_IS_NIL( pItem2 );
      hb_stackPop();    /* clear the pItem2 */
   }
   else if( HB_IS_NIL( pItem2 ) )
   {
      hb_stackDec();    /* pItem2 is already NIL */
      hb_stackPop();    /* clear the pItem1 */
      hb_vmPushLogical( TRUE );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i != 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) !=
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      hb_vmPushLogical( hb_vmPopNumber() != hb_vmPopNumber() );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asLogical.value !=
                                       pItem2->item.asLogical.value);
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_POINTER( pItem1 ) && HB_IS_POINTER( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asPointer.value !=
                                       pItem2->item.asPointer.value );
      pItem1->type = HB_IT_LOGICAL;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }

   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_NOTEQUAL )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPNOTEQUAL", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmLess()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i < 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) <
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 < dNumber2 );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asLogical.value <
                                       pItem2->item.asLogical.value);
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_LESS )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPLESS", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmLessEqual()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i <= 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) <=
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 <= dNumber2 );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asLogical.value <=
                                       pItem2->item.asLogical.value);
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_LESSEQUAL )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPLESSEQUAL", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmGreater()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i > 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) >
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 > dNumber2 );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asLogical.value >
                                       pItem2->item.asLogical.value);
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_GREATER )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPGREATER", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmGreaterEqual()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i >= 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) >=
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 >= dNumber2 );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asLogical.value >=
                                       pItem2->item.asLogical.value);
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_GREATEREQUAL )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPGREATEREQUAL", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmInstring()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

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
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
   }
   else if( HB_IS_ARRAY( pItem2 ) )
   {
      BOOL bResult = hb_arrayScan( pItem2, pItem1, NULL, NULL, TRUE, FALSE );

      hb_stackPop();
      hb_stackPop();

      hb_vmPushLogical( bResult );
   }
   else if( HB_IS_HASH( pItem2 ) && ( HB_IS_ORDERABLE( pItem1 ) || ( HB_IS_HASH( pItem1 ) && hb_hashLen( pItem1 ) == 1) ) )
   {
      ULONG ulPos;

      if( HB_IS_HASH( pItem1 ) ) // length 1 by hypotesis
      {
         if( hb_hashScan( pItem2, pItem1->item.asHash.value->pKeys, &ulPos ) )
         {
            HB_ITEM_NEW( hbV1 );
            PHB_ITEM pV2 = hb_hashGetValueAt( pItem2, ulPos );

            hb_itemCopy( &hbV1, hb_hashGetValueAt( pItem1, 1) );
            hb_stackPop();
            hb_stackPop();

            hb_stackPush();
            hb_itemCopy( *( HB_VM_STACK.pPos - 1 ), &hbV1 );
            hb_stackPush();
            hb_itemCopy( *( HB_VM_STACK.pPos - 1 ), pV2 );

            // now in the stack we have our values.
            hb_vmEqual( TRUE );  //this will pop params and push result
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
         BOOL bRes = hb_hashScan( pItem2, pItem1, &ulPos );
         hb_stackPop();
         hb_stackPop();

         hb_vmPushLogical( bRes );
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
static void hb_vmForTest( void )        /* Test to check the end point of the FOR */
{
   HB_THREAD_STUB

   double dStep;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmForTest()"));

   while( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) )
   {
      PHB_ITEM pItem1 = hb_stackItemFromTop( -1 );
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
      else
      {
         /* NOTE: Return from the inside. */
         return;
      }
   }

   dStep = hb_vmPopNumber();

   while( ( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) ) && ( ! HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) ) )
   {
      PHB_ITEM pItem1 = hb_stackItemFromTop( -1 );
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemPushForward( pResult );
         hb_itemRelease( pResult );
      }
      else
      {
         /* NOTE: Return from the inside. */
         return;
      }
   }

   if ( hb_stackItemFromTop( -1 )->type == HB_IT_LOGICAL )
   {
      BOOL lEnd;
      BOOL lCurrent;

      lEnd = hb_vmPopLogical();
      while( ! HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) )
      {
         PHB_ITEM pItem1 = hb_stackItemFromTop( -1 );
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

         if( pResult )
         {
            hb_stackPop();
            hb_itemPushForward( pResult );
            hb_itemRelease( pResult );
         }
         else
         {
            /* NOTE: Return from the inside. */
            return;
         }
      }

      lCurrent = hb_vmPopLogical();

      if( dStep >= 0 )           /* Positive loop. Use LESS */
      {
         hb_vmPushLogical( lCurrent <= lEnd );
      }
      else if( dStep < 0 )      /* Negative loop. Use GREATER */
      {
         hb_vmPushLogical( lCurrent >= lEnd );
      }
   }
   else
   {
      double dEnd;
      double dCurrent;

      dEnd = hb_vmPopNumber();

      while( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) )
      {
         PHB_ITEM pItem1 = hb_stackItemFromTop( -1 );
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

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

      dCurrent = hb_vmPopNumber();

      if( dStep >= 0 )          /* Positive loop. Use LESS */
      {
         hb_vmPushLogical( dCurrent <= dEnd );
      }
      else if( dStep < 0 )      /* Negative loop. Use GREATER */
      {
         hb_vmPushLogical( dCurrent >= dEnd );
      }
   }
}

/* ------------------------------- */
/* Operators (logical)             */
/* ------------------------------- */

static void hb_vmNot( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmNot()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_LOGICAL( pItem ) )
   {
      pItem->item.asLogical.value = ! pItem->item.asLogical.value;
   }

#ifdef HB_USE_NUMERIC_IF
   else if( HB_IS_NUMERIC( pItem ) )
   {
      if( HB_IS_INTEGER( hb_stackItemFromTop( -1 ) ) )
      {
         if( pItem->item.asInteger.value == 0 )
         {
            pItem->item.asInteger.value = 1;
         }
         else
         {
            pItem->item.asInteger.value = 0;
         }
      }
      else if( HB_IS_LONG( hb_stackItemFromTop( -1 ) ) )
      {
         if( pItem->item.asLong.value == 0 )
         {
            pItem->item.asLong.value = 1;
         }
         else
         {
            pItem->item.asLong.value = 0;
         }
      }
      else
      {
         if( pItem->item.asDouble.value == 0 )
         {
            pItem->item.asDouble.value = 1.0;
         }
         else
         {
            pItem->item.asDouble.value = 0.0;
         }
      }
   }
#endif

   else if( hb_objGetOpOver( pItem ) & HB_CLASS_OP_NOT )
   {
      hb_vmOperatorCallUnary( pItem, "__OPNOT", pItem );
   }
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmAnd()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asLogical.value &&
                                       pItem2->item.asLogical.value );
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_AND )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPAND", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmOr()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asLogical.value ||
                                       pItem2->item.asLogical.value );
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( hb_objGetOpOver( pItem1 ) & HB_CLASS_OP_OR )
   {
      hb_vmOperatorCall( pItem1, pItem2, "__OPOR", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmBitAnd()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_NUMBER( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value = hb_itemGetNInt( pItem1 ) & hb_itemGetNInt( pItem2 );
      pItem1->type = HB_IT_LONG;
      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) &&  HB_IS_STRING( pItem2 ) )
   {
      ULONG  ulLen1 = pItem1->item.asString.length;
      ULONG  ulLen2 = pItem2->item.asString.length;

      if( ulLen1 && ulLen2 )
      {
         char*  pString1 = pItem1->item.asString.value;
         char*  pString2 = pItem2->item.asString.value;
         ULONG  ulPos1, ulPos2;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString1 = (char*) hb_xgrab( ulLen1 + 1 );
            hb_xmemcpy( (void*) pString1, (void*) pItem1->item.asString.value, ulLen1 );
            hb_itemPutCPtr( pItem1, pString1, ulLen1 );
         }

         for( ulPos1 = ulPos2 = 0;  ulPos1 < ulLen1;  ulPos1++ )
         {
            pString1[ulPos1] &= pString2[ulPos2];

            if( ++ulPos2 == ulLen2 )
            {
               ulPos2 = 0;
            }
         }
      }

      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) &&  HB_IS_NUMERIC( pItem2 ) )
   {
      ULONG  ulLen = pItem1->item.asString.length;

      if( ulLen )
      {
         char   cVal = (char) hb_itemGetNL( pItem2 );
         char*  pString = pItem1->item.asString.value;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString = (char*) hb_xgrab( ulLen + 1 );
            hb_xmemcpy( (void*) pString, (void*) pItem1->item.asString.value, ulLen + 1 );
            hb_itemPutCPtr( pItem1, pString, ulLen );
         }

         while( ulLen-- )
         {
            pString[ulLen] &= cVal;
         }
      }

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) &&  HB_IS_STRING( pItem2 ) )
   {
      HB_LONG  lVal = hb_itemGetNInt( pItem1 );
      char*    pString = pItem2->item.asString.value;
      ULONG    ulLen = pItem2->item.asString.length;

      while ( ulLen )
         lVal &= pString[--ulLen];

      pItem1->type = HB_IT_LONG;
      pItem1->item.asLong.value = lVal;

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value = hb_itemGetNInt( pItem1 ) & hb_itemGetNInt( pItem2 );
      pItem1->type = HB_IT_LONG;
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmBitOr()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_NUMBER( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value = hb_itemGetNInt( pItem1 ) | hb_itemGetNInt( pItem2 );
      pItem1->type = HB_IT_LONG;
      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) &&  HB_IS_STRING( pItem2 ) )
   {
      ULONG  ulLen1 = pItem1->item.asString.length;
      ULONG  ulLen2 = pItem2->item.asString.length;

      if( ulLen1 && ulLen2 )
      {
         char*  pString1 = pItem1->item.asString.value;
         char*  pString2 = pItem2->item.asString.value;
         ULONG  ulPos1, ulPos2;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString1 = (char*) hb_xgrab( ulLen1 + 1 );
            hb_xmemcpy( (void*) pString1, (void*) pItem1->item.asString.value, ulLen1 );
            hb_itemPutCPtr( pItem1, pString1, ulLen1 );
         }

         for( ulPos1 = ulPos2 = 0;  ulPos1 < ulLen1;  ulPos1++ )
         {
            pString1[ulPos1] |= pString2[ulPos2];

            if( ++ulPos2 == ulLen2 )
            {
               ulPos2 = 0;
            }
         }
      }

      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) &&  HB_IS_NUMERIC( pItem2 ) )
   {
      ULONG  ulLen = pItem1->item.asString.length;

      if( ulLen )
      {
         char   cVal = (char) hb_itemGetNL( pItem2 );
         char*  pString = pItem1->item.asString.value;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString = (char*) hb_xgrab( ulLen + 1 );
            hb_xmemcpy( (void*) pString, (void*) pItem1->item.asString.value, ulLen + 1 );
            hb_itemPutCPtr( pItem1, pString, ulLen );
         }

         while( ulLen-- )
         {
            pString[ulLen] |= cVal;
         }
      }

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) &&  HB_IS_STRING( pItem2 ) )
   {
      HB_LONG   lVal = hb_itemGetNInt( pItem1 );
      char*     pString = pItem2->item.asString.value;
      ULONG     ulLen = pItem2->item.asString.length;

      while ( ulLen )
      {
         lVal |= pString[--ulLen];
      }

      pItem1->type = HB_IT_LONG;
      pItem1->item.asLong.value = lVal;

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value = hb_itemGetNInt( pItem1 ) | hb_itemGetNInt( pItem2 );
      pItem1->type = HB_IT_LONG;
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmBitXor()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_NUMBER( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value = hb_itemGetNInt( pItem1 ) ^ hb_itemGetNInt( pItem2 );
      pItem1->type = HB_IT_LONG;
      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) &&  HB_IS_STRING( pItem2 ) )
   {
      ULONG  ulLen1 = pItem1->item.asString.length;
      ULONG  ulLen2 = pItem2->item.asString.length;

      if( ulLen1 && ulLen2 )
      {
         char*  pString1 = pItem1->item.asString.value;
         char*  pString2 = pItem2->item.asString.value;
         ULONG  ulPos1, ulPos2;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString1 = (char*) hb_xgrab( ulLen1 + 1 );
            hb_xmemcpy( (void*) pString1, (void*) pItem1->item.asString.value, ulLen1 );
            hb_itemPutCPtr( pItem1, pString1, ulLen1 );
         }

         for( ulPos1 = ulPos2 = 0;  ulPos1 < ulLen1;  ulPos1++ )
         {
            pString1[ulPos1] ^= pString2[ulPos2];

            if( ++ulPos2 == ulLen2 )
            {
               ulPos2 = 0;
            }
         }
      }

      hb_stackPop();
   }
   else if( HB_IS_STRING( pItem1 ) &&  HB_IS_NUMERIC( pItem2 ) )
   {
      ULONG  ulLen = pItem1->item.asString.length;

      if( ulLen )
      {
         char   cVal = (char) hb_itemGetNL( pItem2 );
         char*  pString = pItem1->item.asString.value;

         if( ! HB_IS_STRINGWR( pItem1 ) )
         {
            pString = (char*) hb_xgrab( ulLen + 1 );
            hb_xmemcpy( (void*) pString, (void*) pItem1->item.asString.value, ulLen + 1 );
            hb_itemPutCPtr( pItem1, pString, ulLen );
         }

         while( ulLen-- )
         {
            pString[ulLen] ^= cVal;
         }
      }

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) &&  HB_IS_STRING( pItem2 ) )
   {
      HB_LONG   lVal = hb_itemGetNInt( pItem1 );
      char*     pString = pItem2->item.asString.value;
      ULONG     ulLen = pItem2->item.asString.length;

      while ( ulLen )
      {
         lVal ^= pString[--ulLen];
      }

      pItem1->type = HB_IT_LONG;
      pItem1->item.asLong.value = lVal;

      hb_stackPop();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLong.value = hb_itemGetNInt( pItem1 ) ^ hb_itemGetNInt( pItem2 );
      pItem1->type = HB_IT_LONG;
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmBitShiftLeft()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmBitShiftRight()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPush()"));

   pIndex = hb_stackItemFromTop( -1 );
   pArray = hb_stackItemFromTop( -2 );

   if( HB_IS_BYREF( pArray ) )
   {
      pArray = hb_itemUnRef( pArray );
   }

   if( hb_objGetOpOver( pArray ) & HB_CLASS_OP_ARRAYINDEX )
   {
      hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", NULL, 2, NULL );
      hb_itemPushForward( &(HB_VM_STACK.Return ) );
      return;
   }

   if( HB_IS_HASH( pArray ) && HB_IS_ORDERABLE( pIndex ) )
   {
      ULONG ulPos;
      PHB_ITEM pTemp;

      if( HB_IS_NUMBER( pIndex ) && hb_hashGetCompatibility( pArray ) )
      {
         // Associative Array compatibility
         LONG lPos = hb_itemGetNL( pIndex );

         if( lPos < 0 )
         {
            lPos += 1 + hb_hashLen( pArray );
         }

         ulPos = hb_hashAAGetRealPos( pArray, (ULONG) lPos );

         if( ulPos == 0 )
         {
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
            return;
         }
      }
      else
      {
         // Hash compatibility
         if( ! hb_hashScan(pArray, pIndex, &ulPos ) )
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
   {
      lIndex = ( LONG ) pIndex->item.asInteger.value;
   }
   else if( HB_IS_LONG( pIndex ) )
   {
      lIndex = ( LONG ) pIndex->item.asLong.value;
   }
   else if( HB_IS_DOUBLE( pIndex ) )
   {
      lIndex = ( LONG ) pIndex->item.asDouble.value;
   }
#ifndef HB_C52_STRICT
   else if( HB_IS_STRING( pIndex ) && pIndex->item.asString.length == 1 )
   {
      lIndex = ( LONG ) ( BYTE ) pIndex->item.asString.value[0];
   }
   else if( HB_IS_STRING( pIndex ) && HB_IS_OBJECT( pArray ) &&
            strcmp( "TASSOCIATIVEARRAY", hb_objGetClsName( pArray ) ) == 0 )
   {
      hb_vmPushSymbol( hb_dynsymGetCase( pIndex->item.asString.value )->pSymbol );
      hb_itemPushForward( pArray );

      hb_vmSend( 0 );

      // Pop pIndex.
      hb_stackPop();

      // Recycle pArray.
      hb_itemForwardValue( pArray, &(HB_VM_STACK.Return ) );

      return;
   }
#endif
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
            {
               lIndex += ( pArray->item.asArray.value->ulLen + 1 );
            }
            // Empty array and -1 Index -> return NIL.
            else if( lIndex == -1 )
            {
               hb_stackPop();
               hb_itemClear( hb_stackItemFromTop( -1 ) );
               return;
            }
         }
      #endif

      if( lIndex > 0 && (ULONG) lIndex <= pArray->item.asArray.value->ulLen )
      {
        #ifdef HB_ARRAY_USE_COUNTER
         if( pArray->item.asArray.value->ulHolders > 1 )
        #else
         if( pArray->item.asArray.value->pOwners->pNext )
        #endif
         {
            /* this is a temporary copy of an array - we can overwrite
             * it with no problem
            */
            hb_arrayGet( pArray, (ULONG) lIndex, pArray );
            hb_stackPop();
         }
         else
         {
            /* this is a constant array { 1, 2, 3 } - we cannot use
             * the optimization here
            */
            HB_ITEM_NEW( item );

            hb_arrayGet( pArray, lIndex, &item );

            hb_stackPop();

            hb_itemForwardValue( hb_stackItemFromTop( -1 ), &item );
         }
      }
      else
      {
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
      }
   }
 #ifndef HB_C52_STRICT
   else if( HB_IS_STRING( pArray ) )
   {
      if( lIndex > 0 )
      {
         lIndex--;
      }
      else if( lIndex < 0 )
      {
         if( pArray->item.asString.length )
         {
            lIndex += pArray->item.asString.length;
         }
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
         {
            lIndex = 0;
         }
      }

      hb_stackPop();

      if( (ULONG) lIndex < pArray->item.asString.length )
      {
         hb_itemPutCLStatic( pArray, hb_szAscii[ ( UCHAR ) ( pArray->item.asString.value[ lIndex ] ) ], 1 );
      }
      else
      {
         hb_itemPutCL( pArray, NULL, 0 );
      }
   }
 #endif
   else
   {
      //aTail( NIL ) in Clipper -> NIL // No Error.
      if( HB_IS_NIL( pArray ) && lIndex == -1 )
      {
         hb_stackPop();
         hb_itemClear( hb_stackItemFromTop( -1 ) );
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
      }
   }
}

static void hb_vmArrayPushRef( void )
{
   HB_THREAD_STUB

   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   LONG     lIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPushRef()"));

   pIndex = hb_stackItemFromTop( -1 );
   pArray = hb_stackItemFromTop( -2 );

   if( HB_IS_HASH( pArray ) && HB_IS_ORDERABLE( pIndex ) )
   {
      hb_vmArrayPush();
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
      if( HB_IS_INTEGER( pIndex ) )
      {
         lIndex = ( LONG ) pIndex->item.asInteger.value;
      }
      else if( HB_IS_LONG( pIndex ) )
      {
         lIndex = ( LONG ) pIndex->item.asLong.value;
      }
      else if( HB_IS_DOUBLE( pIndex ) )
      {
         lIndex = ( LONG ) pIndex->item.asDouble.value;
      }
      else if( HB_IS_STRING( pIndex ) && pIndex->item.asString.length == 1 )
      {
         lIndex = ( LONG ) ( BYTE ) pIndex->item.asString.value[0];
      }
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
         lIndex += ( pArray->item.asArray.value->ulLen + 1 );
      }

      if( lIndex > 0 && (ULONG) lIndex <= pArray->item.asArray.value->ulLen )
      {
        #ifdef HB_ARRAY_USE_COUNTER
         if( pArray->item.asArray.value->ulHolders > 1 )
        #else
         if( pArray->item.asArray.value->pOwners->pNext )
        #endif
         {
            /* move the array on stack to index position to free the
               place for new reference */
            hb_itemForwardValue( pIndex, pArray );
            hb_arrayGetByRef( pIndex, (ULONG) lIndex, pArray );
            /* pop the source array (at index position) from stack */
            hb_stackPop();
         }
         else
         {
            // Literal array - can not push by ref!
            hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
         }
      }
      else
      {
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
}

static void hb_vmArrayPop( HB_PCODE pcode )
{
   HB_THREAD_STUB

   PHB_ITEM pValue;
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   LONG     lIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPop()"));

   pValue = hb_stackItemFromTop( -3 );
   pArray = hb_stackItemFromTop( -2 );
   pIndex = hb_stackItemFromTop( -1 );

   if( HB_IS_BYREF( pArray ) )
   {
      pArray = hb_itemUnRef( pArray );
   }

   if( hb_objGetOpOver( pArray ) & HB_CLASS_OP_ARRAYINDEX )
   {
      if( pcode == HB_P_PLUS )
      {
         PHB_ITEM pElement = hb_itemNew( NULL );

         hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", NULL, 0, pElement );
         hb_vmPlus( pElement, pValue, pElement );

         hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", pElement, 2, NULL );
         hb_itemPushForward( &(HB_VM_STACK.Return ) );
         hb_itemRelease( pElement );
      }
      else
      {
         hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX", pValue, 2, NULL );
         hb_itemPushForward( &(HB_VM_STACK.Return ) );
      }

      return;
   }

   if( HB_IS_HASH( pArray ) && HB_IS_ORDERABLE( pIndex ) )
   {
      ULONG ulPos;

      if( HB_IS_NUMBER( pIndex ) && hb_hashGetCompatibility( pArray ) )
      {
         // Compatibilidad con Associative Array
         LONG lPos = hb_itemGetNL( pIndex );

         if( lPos < 0 )
         {
            lPos += 1 + hb_hashLen( pArray );
         }

         ulPos = hb_hashAAGetRealPos( pArray, (ULONG) lPos );

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
            if( ! hb_hashScan(pArray, pIndex, &ulPos ) )
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
      }

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
   }

   if( HB_IS_INTEGER( pIndex ) )
   {
      lIndex = ( LONG ) pIndex->item.asInteger.value;
   }
   else if( HB_IS_LONG( pIndex ) )
   {
      lIndex = ( LONG ) pIndex->item.asLong.value;
   }
   else if( HB_IS_DOUBLE( pIndex ) )
   {
      lIndex = ( LONG ) pIndex->item.asDouble.value;
   }
#ifndef HB_C52_STRICT
   else if( HB_IS_STRING( pIndex ) && pIndex->item.asString.length == 1 )
   {
      lIndex = ( LONG ) ( BYTE ) pIndex->item.asString.value[0];
   }
   else if( HB_IS_STRING( pIndex ) && HB_IS_OBJECT( pArray ) && strcmp( "TASSOCIATIVEARRAY", hb_objGetClsName( pArray ) ) == 0 )
   {
      char szMessage[ HB_SYMBOL_NAME_LEN ];

      szMessage[0] = '_';
      szMessage[1] = '\0';
      strcat( szMessage, pIndex->item.asString.value );

      // Optimized - recycling the parameters.
      #if 1
         // Swap - pIndex no longer needed.
         hb_itemForwardValue( pIndex, pValue );

         // Recycle pValue as Message.
         pValue->type = HB_IT_SYMBOL;
         pValue->item.asSymbol.value = hb_dynsymGetCase( szMessage )->pSymbol;
         pValue->item.asSymbol.stackbase = HB_VM_STACK.pPos - 3 - HB_VM_STACK.pItems;
         pValue->item.asSymbol.uiSuperClass = 0;

         if( HB_IS_BYREF( hb_stackItemFromTop( -2 ) ) )
         {
            hb_itemCopy( hb_stackItemFromTop( -2 ), pArray );
         }

         hb_vmSend( 1 );
      #else
         hb_vmPushSymbol( hb_dynsymGetCase( szMessage )->pSymbol );
         hb_vmPush( pArray );
         hb_vmPush( pValue );

         hb_vmSend( 1 );

         hb_stackPop();
         hb_stackPop();
         hb_stackPop();
      #endif

      if( HB_IS_COMPLEX( &(HB_VM_STACK.Return) ) )
      {
         hb_itemClear( &(HB_VM_STACK.Return) );
      }

      return;
   }
#endif
   else
   {
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, pIndex, pValue );
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
      #ifndef HB_C52_STRICT
         if( lIndex < 0 )
         {
            lIndex += ( pArray->item.asArray.value->ulLen + 1 );
         }
      #endif

      if( lIndex > 0 && (ULONG) lIndex <= pArray->item.asArray.value->ulLen )
      {
         /* Remove MEMOFLAG if exists (assignment from field). */
         pValue->type &= ~HB_IT_MEMOFLAG;

         if( pcode == HB_P_PLUS )
         {
            PHB_ITEM pElement = hb_itemNew( NULL );

            hb_arrayGetForward( pArray, (ULONG) lIndex, pElement );

            hb_vmPlus( pElement, pValue, pElement );

            hb_arraySetForward( pArray, (ULONG) lIndex, pElement );

            hb_itemRelease( pElement );
         }
         else
         {
            hb_arraySetForward( pArray, (ULONG) lIndex, pValue );
         }

         hb_stackPop();
         hb_stackPop();
         hb_stackPop();    /* remove the value from the stack just like other POP operations */
      }
      else
      {
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
      }
   }
 #ifndef HB_C52_STRICT
   // Only allowing assignment of strings (char) and numerics into String as Array.
   else if( HB_IS_STRING( pArray ) && ( HB_IS_STRING( pValue ) || HB_IS_NUMERIC( pValue ) ) )
   {
      if( lIndex > 0 )
      {
         lIndex--;
      }
      else if( lIndex < 0 )
      {
         lIndex += pArray->item.asString.length;
      }

      if( lIndex >= 0 && (ULONG) lIndex < pArray->item.asString.length )
      {
         BYTE bNewChar;

         //pArray = pArray->item.asString.pOrigin;

         if( pValue->type & HB_IT_STRING )
         {
            bNewChar = pValue->item.asString.value[0];
         }
         else if( pValue->type == HB_IT_INTEGER )
         {
            bNewChar = (BYTE) pValue->item.asInteger.value;
         }
         else if( pValue->type == HB_IT_LONG )
         {
            bNewChar = (BYTE) pValue->item.asLong.value;
         }
         else
         {
            bNewChar = (BYTE) pValue->item.asDouble.value;
         }

         if( pArray->item.asString.allocated == 0 || *( pArray->item.asString.pulHolders ) > 1 )
         {
            char *sNew = (char *) hb_xgrab( pArray->item.asString.length + 1 );

            memcpy( sNew, pArray->item.asString.value, pArray->item.asString.length );
            sNew[ pArray->item.asString.length ] = '\0';
            hb_itemPutCPtr( pArray, sNew, pArray->item.asString.length );
         }

         if( pcode == HB_P_PLUS )
         {
            pArray->item.asString.value[ lIndex ] += bNewChar;
         }
         else
         {
            pArray->item.asString.value[ lIndex ] = bNewChar;
         }

         hb_stackPop();
         hb_stackPop();
         hb_stackPop();    /* remove the value from the stack just like other POP operations */
      }
      else
      {
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, pIndex, pValue );
      }
   }
 #endif
   else
   {
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, pIndex, pValue );
   }
}

static void hb_vmArrayDim( USHORT uiDimensions ) /* generates an uiDimensions Array and initialize those dimensions from the stack values */
{
   HB_THREAD_STUB

   HB_ITEM_NEW( itArray );

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayDim(%hu)", uiDimensions));

   hb_vmArrayNew( &itArray, uiDimensions );

   while( uiDimensions-- )
   {
      hb_stackPop();
   }

   hb_itemForwardValue( ( * HB_VM_STACK.pPos ), &itArray );
   hb_stackPush();

}

static void hb_vmArrayGen( ULONG ulElements ) /* generates an ulElements Array and fills it from the stack values */
{
   HB_THREAD_STUB

   HB_ITEM_NEW( itArray );
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayGen(%lu)", ulElements));

   hb_arrayNew( &itArray, ulElements );

   for( ulPos = 0; ulPos < ulElements; ulPos++ )
   {
      hb_itemForwardValue( itArray.item.asArray.value->pItems + ulPos, hb_stackItemFromTop( ulPos - ulElements ) );
   }

   /* Poping 1 less than element,so we can override the 1st element with the new array */
   for( ulPos = 1; ulPos < ulElements; ulPos++ )
   {
      hb_stackPop();
   }

   /* Override 1st element if there was one, or push... */
   if( ulElements )
   {
      hb_itemForwardValue( ( hb_stackItemFromTop( - 1 ) ), &itArray );
   }
   else
   {
      hb_itemForwardValue( ( * HB_VM_STACK.pPos ), &itArray );
      hb_stackPush();
   }
}

/* This function creates an array item using 'uiDimension' as an index
 * to retrieve the number of elements from the stack
 */
static void hb_vmArrayNew( PHB_ITEM pArray, USHORT uiDimension )
{
   HB_THREAD_STUB

   ULONG ulElements;
   HB_ITEM_PTR pDim;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayNew(%p, %hu)", pArray, uiDimension));

   pDim = hb_stackItemFromTop( - uiDimension );

   /* use the proper type of number of elements */
   switch( pDim->type )
   {
      case HB_IT_INTEGER:
         ulElements = ( ULONG ) pDim->item.asInteger.value;
         break;

      case HB_IT_LONG:
         ulElements = (ULONG) pDim->item.asLong.value;
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
      {
         hb_vmArrayNew( hb_arrayGetItemPtr( pArray, ulElements-- ), uiDimension );
      }
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmOperatorCall(%p, %p, %s)", pObjItem, pMsgItem, szSymbol));

   //printf( "BEFORE %s Top: %i Type: %i\n", szSymbol, hb_stackTopOffset(), hb_stackItemFromTop( -1 )->type );

   hb_vmPushSymbol( hb_dynsymFind( szSymbol )->pSymbol );

   hb_vmPush( pObjItem );                             /* Push object              */
   hb_vmPush( pMsgItem );                             /* Push argument            */

   if( pArg )
   {
      hb_vmPush( pArg );                             /* Push argument            */
      hb_vmSend( 2 );
   }
   else
   {
      hb_vmSend( 1 );
   }

   //printf( "AFTER %s Top: %i Type: %i\n", szSymbol, hb_stackTopOffset(), hb_stackItemFromTop( -1 )->type );

   while( iPop-- )
   {
      hb_stackPop();
   }

   /* Push return value on the stack
    * NOTE: for performance reason we could have avoided pop of the second argument.
    * and recycle it with the return value, but that would be WRONG in case of Argument BYREF.
    */
   if( pResult )
   {
      hb_itemForwardValue( pResult, &(HB_VM_STACK.Return) );
   }
}

void hb_vmOperatorCallUnary( PHB_ITEM pObjItem, char * szSymbol, PHB_ITEM pResult )
{
   /* NOTE: There is no need to test if specified symbol exists. It is checked
    * by the caller (if HB_IS_OBJECT() && HAS_METHOD() )
    */
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmOperatorCallUnary(%p, %s)", pObjItem, szSymbol));

   //printf( "%s Top: %i Type: %i\n", szSymbol, hb_stackTopOffset(), hb_stackItemFromTop( -1 )->type );

   hb_vmPushSymbol( hb_dynsymFind( szSymbol )->pSymbol );
   hb_vmPush( pObjItem );                             /* Push object */

   hb_vmSend( 0 );

   //printf( "AFTER %s Top: %i Type: %i\n", szSymbol, hb_stackTopOffset(), hb_stackItemFromTop( -1 )->type );

   if( pResult )
   {
      hb_itemForwardValue( pResult, &(HB_VM_STACK.Return) );
   }
}

/* ------------------------------- */
/* Database                        */
/* ------------------------------- */

static ERRCODE hb_vmSelectWorkarea( PHB_ITEM pAlias, PHB_SYMB pField )
{
   HB_THREAD_STUB
   ERRCODE errCode;
   BOOL fRepeat;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmSelectWorkArea(%p,%p)", pAlias, pField));

   /* NOTE: Clipper doesn't generate an error if an workarea specified
    * as numeric value cannot be selected
    */
   do
   {
      fRepeat = FALSE;
      errCode = SUCCESS;

      switch( pAlias->type )
      {
         case HB_IT_INTEGER:
            /* Alias was used as integer value, for example: 4->field
             * or it was saved on the stack using hb_vmPushAlias()
             * or was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( pAlias->item.asInteger.value );
            pAlias->type = HB_IT_NIL;
            break;

         case HB_IT_LONG:
            /* Alias was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( ( int ) pAlias->item.asLong.value );
            pAlias->type = HB_IT_NIL;
            break;

         case HB_IT_DOUBLE:
            /* Alias was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( ( int ) pAlias->item.asDouble.value );
            pAlias->type = HB_IT_NIL;
            break;

         case HB_IT_SYMBOL:
            /* Alias was specified using alias identifier, for example: al->field
             */
            errCode = hb_rddSelectWorkAreaSymbol( pAlias->item.asSymbol.value );
            pAlias->type = HB_IT_NIL;
            break;

         case HB_IT_STRING:
         {
            /* Alias was evaluated from an expression, for example: (cVar)->field
             */
            /* expand '&' operator if exists */
            char * szAlias;
            BOOL bNewString;

            szAlias = hb_macroExpandString( pAlias->item.asString.value, pAlias->item.asString.length, &bNewString );
            if( pField )
            {
               errCode = hb_rddSelectWorkAreaAlias( szAlias );
            }
            else
            {
               int iArea;
               hb_rddGetAliasNumber( szAlias, &iArea );
               hb_rddSelectWorkAreaNumber( iArea );
            }

            if( bNewString )
               hb_xfree( szAlias );
            hb_itemClear( pAlias );
            break;
         }

         default:
            if( pField )
            {
               PHB_ITEM pSubstVal;

               hb_vmPushString( pField->szName, strlen( pField->szName ) );
               pSubstVal = hb_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&",
                                       2, pAlias, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
               if( pSubstVal )
               {
                  hb_itemMove( pAlias, pSubstVal );
                  hb_itemRelease( pSubstVal );
                  fRepeat = TRUE;
               }
               else
               {
                  if( HB_IS_COMPLEX( pAlias ) )
                     hb_itemClear( pAlias );
                  else
                     pAlias->type = HB_IT_NIL;
                  errCode = FAILURE;
               }
            }
            else
            {
               hb_rddSelectWorkAreaNumber( -1 );
               if( HB_IS_COMPLEX( pAlias ) )
                  hb_itemClear( pAlias );
               else
                  pAlias->type = HB_IT_NIL;
            }
            break;
      }
   }
   while( fRepeat );

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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmSwapAlias()"));

   pItem = hb_stackItemFromTop( -1 );
   pWorkArea = hb_stackItemFromTop( -2 );

   hb_vmSelectWorkarea( pWorkArea, NULL );

   hb_itemMove( pWorkArea, pItem );
   hb_stackDec();
}

/* ------------------------------- */
/* Execution                       */
/* ------------------------------- */

HB_EXPORT void hb_vmDo( USHORT uiParams )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;
   PHB_SYMB pSym;
   HB_STACK_STATE sStackState;
   PHB_ITEM pSelf;
   PHB_FUNC pFunc;
   BOOL     bDebugPrevState;
   int      iPresetBase = s_iBaseLine;

   #ifndef HB_NO_PROFILER
      ULONG    ulClock = 0;
      BOOL     bProfiler = hb_bProfiler; /* because profiler state may change */
   #endif

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDo(%hu)", uiParams));

   //printf( "\VmDo nItems: %i Params: %i \n", HB_VM_STACK.pPos - HB_VM_STACK.pBase, uiParams );

   s_ulProcLevel++;

   if( HB_VM_STACK.iExtraParamsIndex && HB_IS_SYMBOL( pItem = hb_stackItemFromTop( -( uiParams + HB_VM_STACK.aiExtraParams[HB_VM_STACK.iExtraParamsIndex - 1] + 2 ) ) ) && pItem->item.asSymbol.value == HB_VM_STACK.apExtraParamsSymbol[HB_VM_STACK.iExtraParamsIndex - 1] )
   {
      uiParams += HB_VM_STACK.aiExtraParams[--HB_VM_STACK.iExtraParamsIndex];
   }

   if( hb_stackItemFromTop( - ( uiParams + 1 ) )->type )
   {
      //TraceLog( NULL, "DIVERTED hb_vmDo() to hb_vmSend()\n" );
      hb_vmSend( uiParams );
      return;
   }

   #ifndef HB_NO_PROFILER
      if( bProfiler )
      {
         ulClock = ( ULONG ) clock();
      }
   #endif

   //TraceLog( NULL, "StackNewFrame %hu\n", uiParams );

   pItem = hb_stackNewFrame( &sStackState, uiParams );
   pSym = pItem->item.asSymbol.value;
   pSelf = hb_stackSelfItem();   /* NIL, OBJECT or BLOCK */
   bDebugPrevState = s_bDebugging;
   s_bDebugging = FALSE;

   //TraceLog( NULL, "Symbol: '%s'\n", pSym->szName );

   if( HB_IS_NIL( pSelf ) ) /* are we sending a message ? */
   {
      pFunc = pSym->value.pFunPtr;

      if( pFunc )
      {
         #ifndef HB_NO_PROFILER
            if( bProfiler && pSym->pDynSym )
            {
               pSym->pDynSym->ulRecurse++;
            }
         #endif

         #ifndef HB_NO_TRACE
            if ( hb_bTracePrgCalls )
            {
               HB_TRACE(HB_TR_ALWAYS, ("Calling: %s", pSym->szName));
            }
         #endif

         HB_TRACE( HB_TR_DEBUG, ("Calling: %s", pSym->szName));

         //printf( "Doing: '%s'\n", pSym->szName );
         if ( pSym->scope.value & HB_FS_PCODEFUNC )
         {
            /* Running pCode dynamic function from .HRB */
            hb_vmExecute( ( (PHB_PCODEFUNC) pFunc )->pCode, ( (PHB_PCODEFUNC) pFunc )->pSymbols, ( (PHB_PCODEFUNC) pFunc )->pGlobals );
         }
         else
         {
            pFunc();
         }
         //printf( "Done: '%s'\n", pSym->szName );

         HB_TRACE( HB_TR_DEBUG, ("Done: %s", pSym->szName));

         #ifndef HB_NO_PROFILER
            if( bProfiler && pSym->pDynSym )
            {
               pSym->pDynSym->ulCalls++;                   /* profiler support */

               /* Time spent has to be added only inside topmost call of a recursive function */
               if( pSym->pDynSym->ulRecurse == 1 )
               {
                  pSym->pDynSym->ulTime += clock() - ulClock; /* profiler support */
               }

               pSym->pDynSym->ulRecurse--;
            }
         #endif
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, pSym->szName, HB_ERR_ARGS_BASEPARAMS );
      }
   }
   else
   {
      HB_TRACE( HB_TR_ERROR, ( "hb_vmDo() internal logic error, Symbol: '%s' Fun: %p, Self Type: %i", pSym->szName, pSym->value.pFunPtr, pSelf->type ) );
      hb_errInternal( HB_EI_ERRUNRECOV, "Error! hb_vmDo() internal logic failure, Symbol: '%s'.", pSym->szName, NULL );
   }

   HB_TRACE(HB_TR_DEBUG, ("DONE hb_vmDo(%hu)", uiParams));

   if( s_bDebugging )
   {
      hb_vmDebuggerEndProc();
   }

   hb_stackOldFrame( &sStackState );

   HB_TRACE(HB_TR_DEBUG, ("Restored OldFrame hb_vmDo(%hu)", uiParams));

   s_bDebugging = bDebugPrevState;
   s_ulProcLevel--;

   s_iBaseLine = iPresetBase;
}

/* JC1: I need this error display routine to be used also by hash pseudo class
   operators, so I put it here
*/
static void hb_vmClassError( int uiParams, char *szClassName, char *szMsg, PHB_ITEM pSelf )
{
   char sDesc[128];
   PHB_ITEM pArgsArray;

   // Should be optimized by rewriting hb_arrayFrom*() to accept Pointer to use.
   pArgsArray = hb_arrayFromStack( uiParams );
   hb_vmPush( pArgsArray );
   hb_itemRelease( pArgsArray );

   if( *szMsg == '_' )
   {
      //TraceLog( NULL, "Class: '%s' has no property: '%s'\n", sClass, pSym->szName );
      sprintf( (char *) sDesc, "Class: '%s' has no property", szClassName );
      if( pSelf )
         hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, (char *) sDesc, szMsg + 1, 1, pSelf );
      else
         hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, (char *) sDesc, szMsg + 1, HB_ERR_ARGS_SELFPARAMS );
   }
   else
   {
      //TraceLog( NULL, "Class: '%s' has no method: '%s'\n", sClass, pSym->szName );
      sprintf( (char *) sDesc, "Class: '%s' has no exported method", szClassName );
      if( pSelf )
         hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, (char *) sDesc, szMsg, 1, pSelf );
      else
         hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, (char *) sDesc, szMsg, HB_ERR_ARGS_SELFPARAMS );
   }
}

HB_EXPORT void hb_vmSend( USHORT uiParams )
{
   HB_THREAD_STUB

   PHB_ITEM       pItem;
   PHB_SYMB       pSym;
   HB_STACK_STATE sStackState;
   PHB_ITEM       pSelf;
   PHB_FUNC       pFunc = NULL;
   BOOL           bDebugPrevState;
   PHB_BASEARRAY  pSelfBase = NULL;
   BOOL           lPopSuper = FALSE;
   int            iPresetBase = s_iBaseLine;
   BOOL           bConstructor = FALSE;
   BOOL           bSymbol = FALSE;
#ifdef HB_THREAD_SUPPORT
   USHORT         uiClass = 0;
#endif

   #ifndef HB_NO_PROFILER
      ULONG       ulClock = 0;
      PMETHOD     pMethod = NULL;
      BOOL        bProfiler = hb_bProfiler; /* because profiler state may change */
   #endif

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_vmSend(%hu)", uiParams ) );

   //TraceLog( NULL, "From: '%s'\n", hb_stackBaseItem()->item.asSymbol.value->szName );

   if( HB_IS_COMPLEX( &(HB_VM_STACK.Return) ) )
   {
      hb_itemClear( &(HB_VM_STACK.Return) );
   }
   else
   {
      ( &(HB_VM_STACK.Return) )->type = HB_IT_NIL;
   }

   //printf( "\n VmSend nItems: %i Params: %i Extra %i\n", HB_VM_STACK.pPos - HB_VM_STACK.pBase, uiParams, hb_vm_aiExtraParams[hb_vm_iExtraParamsIndex - 1] );

   s_ulProcLevel++;

   if( HB_VM_STACK.iExtraParamsIndex && HB_IS_SYMBOL( pItem = hb_stackItemFromTop( -( uiParams + HB_VM_STACK.aiExtraParams[HB_VM_STACK.iExtraParamsIndex - 1] + 2 ) ) ) && pItem->item.asSymbol.value == HB_VM_STACK.apExtraParamsSymbol[HB_VM_STACK.iExtraParamsIndex - 1] )
   {
      uiParams += HB_VM_STACK.aiExtraParams[--HB_VM_STACK.iExtraParamsIndex];
   }

   #ifndef HB_NO_PROFILER
      if( bProfiler )
      {
         ulClock = ( ULONG ) clock();
      }
   #endif

   pItem = hb_stackNewFrame( &sStackState, uiParams );   /* procedure name */
   pSym = pItem->item.asSymbol.value;

   if( HB_VM_STACK.bWithObject )
   {
      HB_VM_STACK.bWithObject = FALSE;

      if( HB_VM_STACK.wWithObjectCounter )
      {
         hb_itemCopy( * ( HB_VM_STACK.pBase + 1 ), &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ) ) ;
      }
      else
      {
         hb_itemClear( * ( HB_VM_STACK.pBase + 1 ) ) ;
      }
   }

   pSelf = ( * ( HB_VM_STACK.pBase + 1 ) );   /* NIL, OBJECT or BLOCK */

   bDebugPrevState = s_bDebugging;
   s_bDebugging = FALSE;

   if( HB_IS_BYREF( pSelf ) )
   {
      pSelf = hb_itemUnRef( pSelf );
   }

   if( HB_IS_BLOCK( pSelf ) && pSym == &( hb_symEval ) )
   {
      pFunc = pSym->value.pFunPtr;                 /* __EVAL method = function */
   }
   else if( HB_IS_BLOCK( pSelf ) && strcmp( pSym->szName, "EVAL" ) == 0 )
   {
      pSym = &hb_symEval;
      pFunc = pSym->value.pFunPtr;                 /* __EVAL method = function */
   }
   else if( HB_IS_OBJECT( pSelf ) ||
            ( HB_IS_ARRAY( pSelf )   && hb_cls_uiArrayClass )     ||
            ( HB_IS_BLOCK( pSelf )   && hb_cls_uiBlockClass )     ||
            ( HB_IS_STRING( pSelf )  && hb_cls_uiCharacterClass ) ||
            ( HB_IS_DATE( pSelf )    && hb_cls_uiDateClass )      ||
            ( HB_IS_LOGICAL( pSelf ) && hb_cls_uiLogicalClass )   ||
            ( HB_IS_NIL( pSelf )     && hb_cls_uiNilClass )       ||
            ( HB_IS_NUMERIC( pSelf ) && hb_cls_uiNumericClass )   ||
            ( HB_IS_POINTER( pSelf ) && hb_cls_uiPointerClass )
          )               /* Object passed            */
   {
      //TraceLog( NULL, "Object: '%s' Message: '%s'\n", hb_objGetClsName( pSelf ), pSym->szName );

      if( pSym == &hb_symDestructor )
      {
         pFunc = (PHB_FUNC) pSym;
         bSymbol = TRUE;
      }
      else
      {
         pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol );

#ifdef HB_THREAD_SUPPORT
         if( HB_VM_STACK.pMethod && (HB_VM_STACK.pMethod->uiScope & HB_OO_CLSTP_SYNC) )
         {
            uiClass = hb_objClassH( pSelf );
            // Put uiClass in the SYNC methods list.
            hb_clsPutSyncID( uiClass );
         }
#endif

         if( uiParams == 1 && pFunc == hb___msgGetData )
         {
            pFunc = hb___msgSetData;
         }

         if( HB_IS_OBJECT( pSelf ) )
         {
            pSelfBase = pSelf->item.asArray.value;

            if( pSelfBase->uiPrevCls ) /* Is is a Super cast ? */
            {
               HB_ITEM_NEW( RealSelf );
               USHORT nPos;
               USHORT uiClass;

               //printf( "\n VmSend Method: %s \n", pSym->szName );
               uiClass = pSelfBase->uiClass;
               pItem->item.asSymbol.uiSuperClass = uiClass;

               //TraceLog( NULL, "pRealSelf %p pItems %p\n", pRealSelf, pSelfBase->pItems );

               /* Replace the current stacked value with real self */
               hb_itemCopy( &RealSelf, pSelfBase->pItems );
               hb_itemForwardValue( pSelf, &RealSelf );

               if( HB_IS_OBJECT( pSelf ) )
               {
                  /* Take back the good pSelfBase */
                  pSelfBase = pSelf->item.asArray.value;

                  /* Push current SuperClass handle */
                  lPopSuper = TRUE ;

                  if( ! pSelf->item.asArray.value->puiClsTree )
                  {
                     pSelf->item.asArray.value->puiClsTree   = ( USHORT * ) hb_xgrab( sizeof( USHORT ) );
                     pSelf->item.asArray.value->puiClsTree[0] = 0;
                  }

                  nPos = pSelfBase->puiClsTree[0] + 1;
                  pSelfBase->puiClsTree = ( USHORT * ) hb_xrealloc( pSelfBase->puiClsTree, sizeof( USHORT ) * (nPos+1) ) ;
                  pSelfBase->puiClsTree[0] = nPos ;
                  pSelfBase->puiClsTree[ nPos ] = uiClass;
               }
            }
         }
      }
   }
   else if ( HB_IS_HASH( pSelf ) )
   {
	  // Using FALSE for lAllowErrFunc because we must prefer (at this point) default messages below over OnError handler if any
      if( hb_cls_uiHashClass && ( pFunc = hb_objGetMthd( pSelf, pSym, FALSE, &bConstructor, FALSE, &bSymbol ) ) != NULL )
	  {
		 //goto DoFunc;
	  }
      else if( uiParams == 1 )
      {
         if ( pSym->szName[0] == '_' )
         {
            hb_hashAddChar( pSelf, pSym->szName + 1, hb_stackItemFromTop( -1 ) );
            hb_itemCopy( &(HB_VM_STACK.Return), hb_stackItemFromTop( -1 ) );
         }
         else if( hb_cls_uiHashClass && ( pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol ) ) != NULL )
	     {
		    //goto DoFunc;
		 }
         else
         {
            hb_vmClassError( uiParams, "HASH", pSym->szName, NULL );
         }
      }
      else if ( uiParams == 0)
      {
         ULONG ulPos;

         if( strcmp( pSym->szName, "CLASSNAME" ) == 0 )
         {
            hb_itemPutC( &(HB_VM_STACK.Return), "HASH" );
         }
         else if( strcmp( pSym->szName, "CLASSH" ) == 0 )
         {
            hb_itemPutNI( &(HB_VM_STACK.Return), 0 );
         }
         else if( strcmp( pSym->szName, "KEYS" ) == 0 )
         {
            hb_hashGetKeys( &(HB_VM_STACK.Return), pSelf );
         }
         else if( strcmp( pSym->szName, "VALUES" ) == 0 )
         {
            hb_hashGetValues( &(HB_VM_STACK.Return), pSelf );
         }
         else if( hb_cls_uiHashClass && ( pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol ) ) != NULL )
	     {
		    //goto DoFunc;
		 }
         else
         {
            HB_ITEM_NEW( hbIndex );

            hb_itemPutCRawStatic( &hbIndex, pSym->szName, strlen( pSym->szName ) );

            if ( hb_hashScan( pSelf, &hbIndex , &ulPos ) )
            {
               hb_hashGet( pSelf, ulPos, &HB_VM_STACK.Return );
            }
            else
            {
               hb_vmClassError( uiParams, "HASH", pSym->szName, NULL );
            }
         }
      }
      else if( hb_cls_uiHashClass && ( pFunc = hb_objGetMthd( pSelf, pSym, TRUE, &bConstructor, FALSE, &bSymbol ) ) != NULL )
      {
         //goto DoFunc;
      }
      else
      {
         hb_vmClassError( uiParams, "HASH", pSym->szName, NULL );
      }
   }

   if( pFunc )
   {
      #ifndef HB_NO_PROFILER
         if( bProfiler )
         {
            pMethod = HB_VM_STACK.pMethod;
         }
      #endif

      #ifndef HB_NO_TRACE
         if ( hb_bTracePrgCalls )
         {
            HB_TRACE(HB_TR_ALWAYS, ("Calling: %s", pSym->szName));
         }
      #endif

      HB_TRACE( HB_TR_DEBUG, ("Calling: %s", pSym->szName));

      //TraceLog( NULL, "Doing %s\n", pSym->szName );

      if( bSymbol )
      {
         PHB_SYMB pSymbol = (PHB_SYMB) pFunc;
         pFunc = ((PHB_SYMB) pFunc)->value.pFunPtr;

         if( pFunc )
         {
            if( pSymbol->scope.value & HB_FS_PCODEFUNC )
            {
               /* Running pCode dynamic function from .HRB */
               hb_vmExecute( ( (PHB_PCODEFUNC) pFunc )->pCode, ( (PHB_PCODEFUNC) pFunc )->pSymbols, ( (PHB_PCODEFUNC) pFunc )->pGlobals );
            }
            else
            {
               pFunc();
            }
         }
         else
         {
            char *sClass = hb_objGetClsName( pSelf );
            //TraceLog( NULL, "METHOD NOT FOUND!\n" );
            hb_vmClassError( uiParams, sClass, pSym->szName, NULL );
         }
      }
      else
      {
         pFunc();
      }

      //TraceLog( NULL, "Done\n" );

      HB_TRACE( HB_TR_DEBUG, ("Done: %s", pSym->szName));

      if( ( pSym != &hb_symEval ) && lPopSuper && pSelfBase->puiClsTree )
      {
         USHORT nPos = pSelfBase->puiClsTree[0] - 1;

         /* POP SuperClass handle */
         if( nPos )
         {
            pSelfBase->puiClsTree = ( USHORT * ) hb_xrealloc( pSelfBase->puiClsTree, sizeof( USHORT ) * (nPos + 1) );
            pSelfBase->puiClsTree[0] = nPos;
         }
         else
         {
            hb_xfree(pSelfBase->puiClsTree);
            pSelfBase->puiClsTree = NULL ;
         }
      }

      #ifndef HB_NO_PROFILER
         if( bProfiler )
         {
            hb_mthAddTime( pMethod, clock() - ulClock );
         }
      #endif

#ifdef HB_THREAD_SUPPORT
      if( uiClass )
      {
         // Delete uiClass from SYNC methods list.
         hb_clsDelSyncID( uiClass );
      }
#endif

      // Constructor must ALWAYS return Self.
      if( bConstructor )
      {
         //printf( "OOPS! Constructor!\n" );
         //hb_itemForwardValue( &(HB_VM_STACK.Return ), pSelf );

         if( HB_VM_STACK.uiActionRequest == 0 )
         {
            if( ( ! HB_IS_OBJECT( &( HB_VM_STACK.Return ) ) ) || pSelf->item.asArray.value != HB_VM_STACK.Return.item.asArray.value )
            {
               PHB_ITEM pResult;

               hb_vmPush( &( HB_VM_STACK.Return ) );
               pResult  = hb_errRT_BASE_Subst( EG_BADSELF, 1605, NULL, pSym->szName, 2, pSelf, hb_stackItemFromTop( -1 ) );
               hb_stackPop();

               if( pResult )
               {
                  hb_itemRelease( hb_itemReturnForward( pResult ) );
               }
               else
               {
                  hb_itemForwardValue( &(HB_VM_STACK.Return ), pSelf );
               }
            }
         }
      }
   }
   else if ( ! HB_IS_HASH( pSelf ) )
   {
      if( strncmp( pSym->szName, "CLASSNAME", strlen( pSym->szName ) < 4 ? 4 : strlen( pSym->szName ) ) == 0 )
      {
         char *sClass = hb_objGetClsName( pSelf );
         hb_itemPutC( &(HB_VM_STACK.Return), sClass );
      }
      else if( strncmp( pSym->szName, "CLASSH", 6 ) == 0 )
      {
         hb_itemPutNI( &(HB_VM_STACK.Return), 0 );
      }
      else
      {
         char *sClass = hb_objGetClsName( pSelf );
         //TraceLog( NULL, "METHOD NOT FOUND!\n" );
         hb_vmClassError( uiParams, sClass, pSym->szName, NULL );
      }
   }

   HB_TRACE(HB_TR_DEBUG, ("Done hb_vmSend()"));

   if( s_bDebugging )
   {
      hb_vmDebuggerEndProc();
   }

   hb_stackOldFrame( &sStackState );

   HB_TRACE(HB_TR_DEBUG, ("Restored Stack hb_vmSend()"));

   s_bDebugging = bDebugPrevState;
   s_ulProcLevel--;

   s_iBaseLine = iPresetBase;
}

static HARBOUR hb_vmDoBlock( void )
{
   HB_THREAD_STUB

   PHB_ITEM pBlock;
   USHORT uiLine;
   int iParam;
   int iStatics;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoBlock()"));

   pBlock = hb_stackSelfItem();

   if( HB_IS_BYREF( pBlock ) )
   {
      pBlock = hb_itemUnRef( pBlock );
   }

   if( ! HB_IS_BLOCK( pBlock ) )
   {
      hb_errInternal( HB_EI_VMNOTCBLOCK, NULL, "hb_vmDoBlock()", NULL );
   }

   /* Check for valid count of parameters */
   iParam = pBlock->item.asBlock.paramcnt - hb_pcount();

   /* add missing parameters */
   while( iParam-- > 0 )
   {
      hb_vmPushNil();
   }

   // Save the current line number.
   uiLine = hb_stackBaseItem()->item.asSymbol.lineno;

   // Change the line number to line where block was defined.
   hb_stackBaseItem()->item.asSymbol.lineno = pBlock->item.asBlock.value->lineno;

   // Save current Statics Context.
   iStatics = HB_VM_STACK.iStatics;

   // Change Statics context to that of the module where the Block was defined.
   HB_VM_STACK.iStatics = pBlock->item.asBlock.statics;

   hb_vmExecute( pBlock->item.asBlock.value->pCode, pBlock->item.asBlock.value->pSymbols, pBlock->item.asBlock.value->pGlobals );

   // Restore Statics context.
   HB_VM_STACK.iStatics = iStatics;

   HB_TRACE(HB_TR_DEBUG, ("Done hb_vmDoBlock()"));

   // Restore line numer.
   hb_stackBaseItem()->item.asSymbol.lineno = uiLine;

   HB_TRACE(HB_TR_DEBUG, ("Restored Satck hb_vmDoBlock()"));
}

/* Evaluates a passed codeblock item with no arguments passed to a codeblock
*/
HB_EXPORT HB_ITEM_PTR hb_vmEvalBlock( HB_ITEM_PTR pBlock )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmEvalBlock(%p)", pBlock));

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pBlock );
   hb_vmSend( 0 );
   return &(HB_VM_STACK.Return);
}

/* Evaluates a codeblock item using passed additional arguments
 * pBlock = an item of codeblock type to evaluate
 * ulArgCount = number of arguments passed to a codeblock
 * ... = the list of arguments of type PHB_ITEM
 *
 *for example:
 * retVal = hb_vmEvalBlockV( pBlock, 2, pParam1, pParam2 );
*/
HB_EXPORT HB_ITEM_PTR hb_vmEvalBlockV( HB_ITEM_PTR pBlock, ULONG ulArgCount, ... )
{
   HB_THREAD_STUB
   va_list va;
   ULONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmEvalBlockV(%p, %hu, ...)", pBlock, ulArgCount));

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pBlock );

   va_start( va, ulArgCount );
   for( i = 1; i <= ulArgCount; i++ )
   {
      hb_vmPush( va_arg( va, PHB_ITEM ) );
   }
   va_end( va );

   /* take care here, possible loss of data LONG to SHORT ... */
   /* added an explicit casting here for VC++ JFL */
   hb_vmSend( (USHORT) ulArgCount );

   return &(HB_VM_STACK.Return);
}

/* Evaluates a passed codeblock item or macro pointer item
 */
HB_EXPORT PHB_ITEM hb_vmEvalBlockOrMacro( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmEvalBlockOrMacro(%p)", pItem));

   if( pItem->type == HB_IT_BLOCK )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pItem );
      hb_vmSend( 0 );
   }
   else
   {
      HB_MACRO_PTR pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pItem );
      if( pMacro )
      {
         hb_macroRun( pMacro );
         hb_itemForwardValue( hb_stackReturnItem(), hb_stackItemFromTop( - 1 ) );
         hb_stackPop();
      }
      else
      {
         hb_itemClear( hb_stackReturnItem() );
      }
   }
   return hb_stackReturnItem();
}

/*
 * destroy codeblock or macro in given item
 */
HB_EXPORT void hb_vmDestroyBlockOrMacro( PHB_ITEM pItem )
{
   if( pItem->type == HB_IT_POINTER )
   {
      HB_MACRO_PTR pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pItem );
      if( pMacro )
      {
         hb_macroDelete( pMacro );
      }
   }
   hb_itemRelease( pItem );
}


HB_EXPORT void hb_vmFunction( USHORT uiParams )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ("hb_vmFunction(%hu)", uiParams ) );

   if( hb_stackItemFromTop( - ( uiParams + 1 ) )->type )
   {
      hb_vmSend( uiParams );
   }
   else
   {
      if( HB_IS_COMPLEX( &(HB_VM_STACK.Return) ) )
      {
         hb_itemClear( &(HB_VM_STACK.Return) );
      }
      else
      {
         ( &(HB_VM_STACK.Return) )->type = HB_IT_NIL;
      }

      hb_vmDo( uiParams );
   }

   /*
    ***
    * TODO: This should be changed to hb_itemForwardValue()
    * This is here to protect against ill behaved FWH code, which uses HB_VM_STACK.Return
    * after calling vmFunction().
    ***
    */
   hb_itemCopy( ( * HB_VM_STACK.pPos ), &(HB_VM_STACK.Return) );
   hb_stackPush();
}


static void hb_vmDummyDebugEntry( int nMode, int nLine, char *szName, int nIndex, int nFrame )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDummyDebugEntry"));

   HB_SYMBOL_UNUSED( nMode );
   HB_SYMBOL_UNUSED( nLine );
   HB_SYMBOL_UNUSED( szName );
   HB_SYMBOL_UNUSED( nIndex );
   HB_SYMBOL_UNUSED( nFrame );
}

static void hb_vmDebuggerExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerExit"));

   /* is debugger linked ? */
   if ( hb_vm_pFunDbgEntry || s_pSymDbgEntry )
   {
      /* inform debugger that we are quitting now */
      if ( hb_vm_pFunDbgEntry )
      {
         hb_vm_pFunDbgEntry( HB_DBG_VMQUIT, 0, NULL, 0, 0 );
      }
      else
      {
         hb_vmPushSymbol( s_pSymDbgEntry );
         hb_vmPushNil();
         hb_vmPushLongConst( HB_DBG_VMQUIT );
         hb_vmDo( 1 );
      }
   }
   /* set dummy debugger function to avoid debugger activation in .prg
    *       destructors if any */
   hb_vm_pFunDbgEntry = hb_vmDummyDebugEntry;
}

static void hb_vmCacheDbgEntry( void )
{
   /* Cache __DBGENTRY symbol to speed everything up */
   if ( !hb_vm_pFunDbgEntry && !s_pSymDbgEntry )
   {
      s_pSymDbgEntry = hb_dynsymFind( "__DBGENTRY" )->pSymbol;

      /* Try to get C dbgEntry() function pointer */
      hb_vmPushSymbol( s_pSymDbgEntry );
      hb_vmPushNil();
      hb_vmPushLongConst( HB_DBG_GETENTRY );
      hb_vmDo( 1 );
   }
}

static void hb_vmLocalName( USHORT uiLocal, char * szLocalName ) /* locals and parameters index and name information for the debugger */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmLocalName(%hu, %s)", uiLocal, szLocalName));

   s_bDebugging = TRUE;
   s_bDebugShowLines = FALSE;

   s_bDebuggerIsWorking = TRUE;

   hb_vmCacheDbgEntry();

   if ( hb_vm_pFunDbgEntry )
   {
      hb_vm_pFunDbgEntry( HB_DBG_LOCALNAME, 0, szLocalName, uiLocal, 0 );
   }
   else
   {
      hb_vmPushSymbol( s_pSymDbgEntry );
      hb_vmPushNil();
      hb_vmPushLongConst( HB_DBG_LOCALNAME );
      hb_vmPushLongConst( uiLocal );
      hb_vmPushString( szLocalName, strlen( szLocalName ) );
      hb_vmDo( 3 );
   }
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;
}

static void hb_vmStaticName( USHORT uiStatic, char * szStaticName ) /* statics vars information for the debugger */
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmStaticName(%hu, %s)", uiStatic, szStaticName));

   s_bDebugging = TRUE;
   s_bDebugShowLines = FALSE;

   s_bDebuggerIsWorking = TRUE;
   if ( hb_vm_pFunDbgEntry )
   {
      hb_vm_pFunDbgEntry( HB_DBG_STATICNAME, 0, szStaticName, uiStatic, HB_VM_STACK.iStatics );
   }
   else
   {
      hb_vmPushSymbol( s_pSymDbgEntry );
      hb_vmPushNil();
      hb_vmPushLongConst( HB_DBG_STATICNAME );
      hb_vmPushLongConst( HB_VM_STACK.iStatics );  /* current static frame */
      hb_vmPushLongConst( uiStatic );  /* variable index */
      hb_vmPushString( szStaticName, strlen( szStaticName ) );
      hb_vmDo( 4 );
   }
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;
}

static void hb_vmModuleName( char * szModuleName ) /* PRG and function name information for the debugger */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmModuleName(%s)", szModuleName));

   s_bDebugging = TRUE;
   s_bDebugShowLines = FALSE;

   s_bDebuggerIsWorking = TRUE;

   hb_vmCacheDbgEntry();

   if ( hb_vm_pFunDbgEntry )
   {
      hb_vm_pFunDbgEntry( HB_DBG_MODULENAME, 0, szModuleName, 0, 0 );
   }
   else
   {
      hb_vmPushSymbol( s_pSymDbgEntry );
      hb_vmPushNil();
      hb_vmPushLongConst( HB_DBG_MODULENAME );
      hb_vmPushString( szModuleName, strlen( szModuleName ) );

      hb_vmDo( 2 );
   }
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;
}

static void hb_vmFrame( unsigned short iLocals, BYTE bParams )
{
   HB_THREAD_STUB

   int iTotal, iExtra;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmFrame(%d, %d)", iLocals, (int) bParams));

   if( bParams == 255 )
   {
      hb_stackBaseItem()->item.asSymbol.paramcnt += 256;

      while( iLocals-- > 0 )
      {
         hb_vmPushNil();
      }
      return;
   }

   iExtra = hb_pcount() - bParams;

   while( iExtra > 0 )
   {
      PHB_ITEM pExtra = hb_stackItemFromTop( -iExtra );

      if( HB_IS_COMPLEX( pExtra ) )
      {
         hb_itemClear( pExtra );
      }
      else
      {
         pExtra->type = HB_IT_NIL;
      }

      iExtra--;
   }

   iTotal = iLocals + bParams;
   if( iTotal )
   {
      int i = iTotal - hb_pcount();

      while( i-- > 0 )
      {
         hb_vmPushNil();
      }
   }
}

static void hb_vmSFrame( PHB_SYMB pSym )      /* sets the statics frame for a function */
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmSFrame(%p)", pSym));

   /* _INITSTATICS is now the statics frame. Statics() changed it! */
   HB_VM_STACK.iStatics = pSym->value.iStaticsBase; /* pSym is { "$_INITSTATICS", HB_FS_INITEXIT, _INITSTATICS } for each PRG */
}

static void hb_vmStatics( PHB_SYMB pSym, USHORT uiStatics ) /* initializes the global aStatics array or redimensionates it */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStatics(%p, %hu)", pSym, uiStatics));

   if( HB_IS_NIL( &s_aStatics ) )
   {
      pSym->value.iStaticsBase = 0;         /* statics frame for this PRG */
      hb_arrayNew( &s_aStatics, uiStatics );
      //printf( "Allocated s_aStatics: %p %p\n", &s_aStatics, s_aStatics.item.asArray.value->pOwners );
   }
   else
   {
      pSym->value.iStaticsBase = (&s_aStatics)->item.asArray.value->ulLen;
      hb_arraySize( &s_aStatics, (&s_aStatics)->item.asArray.value->ulLen + uiStatics );
      //TraceLog( NULL, "Symbol: %s Statics: %i Size: %i\n", pSym->szName, uiStatics, hb_arrayLen( &s_aStatics ) );
   }

   s_uiStatics = uiStatics; /* We need s_uiStatics for processing hb_vmStaticName() */
}

static void hb_vmEndBlock( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmEndBlock()"));

   hb_itemForwardValue( &(HB_VM_STACK.Return),  *( HB_VM_STACK.pPos - 1 ) );

   hb_stackDec();
}

static void hb_vmRetValue( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = *( HB_VM_STACK.pPos - 1 );

   HB_TRACE(HB_TR_DEBUG, ("hb_vmRetValue()"));

   /* for clipper compatibility */
   pItem->type &= ~HB_IT_MEMOFLAG;
   hb_itemForwardValue( &(HB_VM_STACK.Return), pItem ) ;

   hb_stackDec();
}

static void hb_vmDebuggerEndProc( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerEndProc()"));

   s_bDebugShowLines = FALSE;
   s_bDebuggerIsWorking = TRUE;
   if( hb_vm_pFunDbgEntry )
   {
      hb_vm_pFunDbgEntry( HB_DBG_ENDPROC, 0, NULL, 0, 0 );
   }
   else
   {
      PHB_ITEM pReturn = hb_stackTopItem();
      hb_stackPush();
      hb_itemForwardValue( pReturn, &(HB_VM_STACK.Return) ); /* saves the previous returned value */

      hb_vmPushSymbol( s_pSymDbgEntry );
      hb_vmPushNil();
      hb_vmPushLongConst( HB_DBG_ENDPROC );
      hb_vmDo( 1 );

      hb_itemForwardValue( &(HB_VM_STACK.Return), pReturn ); /* restores the previous returned value */
      hb_stackDec();
   }
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;

}

static void hb_vmDebuggerShowLine( USHORT uiLine ) /* makes the debugger shows a specific source code line */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerShowLine(%hu)", uiLine));

   s_bDebugShowLines = FALSE;
   s_bDebuggerIsWorking = TRUE;
   if ( hb_vm_pFunDbgEntry )
   {
      hb_vm_pFunDbgEntry( HB_DBG_SHOWLINE, uiLine, NULL, 0, 0 );
   }
   else
   {
      hb_vmPushSymbol( s_pSymDbgEntry );
      hb_vmPushNil();
      hb_vmPushLongConst( HB_DBG_SHOWLINE );
      hb_vmPushInteger( uiLine );
      hb_vmDo( 2 );
   }
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;
}

/* ------------------------------- */
/* Push                            */
/* ------------------------------- */

HB_EXPORT void hb_vmPush( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_vmPush(%p) type: %i", pItem, pItem->type ) );

   hb_itemCopy( ( * HB_VM_STACK.pPos ), pItem );
   hb_stackPush();
}

HB_EXPORT void hb_vmPushState( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushState()"));

   /* Save top item which can be processed at this moment */
   hb_stackPush();

   hb_itemForwardValue( * HB_VM_STACK.pPos, &(HB_VM_STACK.Return) );
   hb_stackPush();
}

HB_EXPORT void hb_vmPushNil( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNil()"));

   ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;
   hb_stackPush();
}

HB_EXPORT void hb_vmPushLogical( BOOL bValue )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLogical(%d)", (int) bValue));

   ( * HB_VM_STACK.pPos )->type = HB_IT_LOGICAL;
   ( * HB_VM_STACK.pPos )->item.asLogical.value = bValue;
   hb_stackPush();
}

HB_EXPORT void hb_vmPushNumber( double dNumber, int iDec )
{
   hb_vmPushNumType( dNumber, iDec, 0, 0 );
}

HB_EXPORT void hb_vmPushNumType( double dNumber, int iDec, int iType1, int iType2 )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNumType(%lf, %d, %i, %i)", dNumber, iDec, iType1, iType2));

   if( iDec || iType1 & HB_IT_DOUBLE || iType2 & HB_IT_DOUBLE )
   {
      hb_vmPushDouble( dNumber, iDec );
   }
   else if ( HB_DBL_LIM_INT( dNumber ) )
   {
      hb_vmPushInteger( ( int ) dNumber );
   }
   else if ( HB_DBL_LIM_LONG( dNumber ) )
   {
      hb_vmPushHBLong( ( HB_LONG ) dNumber );
   }
   else
   {
      hb_vmPushDouble( dNumber, hb_set.HB_SET_DECIMALS );
   }
}

static void hb_vmPushNumInt( HB_LONG lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNumInt(%Ld, %i, %i)", lNumber ));

   if ( HB_LIM_INT( lNumber ) )
   {
      hb_vmPushInteger( ( int ) lNumber );
   }
   else
   {
      hb_vmPushHBLong( lNumber );
   }
}

static int hb_vmCalcIntWidth( HB_LONG lNumber )
{
   int iWidth;

   if( lNumber <= -1000000000L )
   {
      iWidth = 20;
   }
   else
   {
      iWidth = 10;
      while ( lNumber >= 1000000000L )
      {
         iWidth++;
         lNumber /= 10;
      }
   }
   return iWidth;
}

HB_EXPORT void hb_vmPushInteger( int iNumber )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushInteger(%d)", iNumber));

   ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
   ( * HB_VM_STACK.pPos )->item.asInteger.value = iNumber;
   ( * HB_VM_STACK.pPos )->item.asInteger.length = HB_INT_LENGTH( iNumber );
   hb_stackPush();
}

#if HB_INT_MAX >= INT32_MAX
static void hb_vmPushIntegerConst( int iNumber )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushIntegerConst(%d)", iNumber));

   ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
   ( * HB_VM_STACK.pPos )->item.asInteger.value = iNumber;
   ( * HB_VM_STACK.pPos )->item.asInteger.length = hb_vmCalcIntWidth( iNumber );
   hb_stackPush();
}
#endif

HB_EXPORT void hb_vmPushLong( LONG lNumber )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLong(%ld)", lNumber));

#if HB_INT_MAX >= LONG_MAX
   ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
   ( * HB_VM_STACK.pPos )->item.asInteger.value = ( int ) lNumber;
   ( * HB_VM_STACK.pPos )->item.asInteger.length = HB_INT_LENGTH( lNumber );
#else
   ( * HB_VM_STACK.pPos )->type = HB_IT_LONG;
   ( * HB_VM_STACK.pPos )->item.asLong.value = lNumber;
   ( * HB_VM_STACK.pPos )->item.asLong.length = HB_LONG_LENGTH( lNumber );
#endif
   hb_stackPush();
}

static void hb_vmPushLongConst( LONG lNumber )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLongConst(%ld)", lNumber));

#if HB_INT_MAX >= LONG_MAX
   ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
   ( * HB_VM_STACK.pPos )->item.asInteger.value = ( int ) lNumber;
   ( * HB_VM_STACK.pPos )->item.asInteger.length = hb_vmCalcIntWidth( lNumber );
#else
   ( * HB_VM_STACK.pPos )->type = HB_IT_LONG;
   ( * HB_VM_STACK.pPos )->item.asLong.value = ( HB_LONG ) lNumber;
   ( * HB_VM_STACK.pPos )->item.asLong.length = hb_vmCalcIntWidth( lNumber );
#endif
   hb_stackPush();
}

static void hb_vmPushHBLong( HB_LONG lNumber )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushHBLong(%" PFHL "d)", lNumber));

   ( * HB_VM_STACK.pPos )->type = HB_IT_LONG;
   ( * HB_VM_STACK.pPos )->item.asLong.value = lNumber;
   ( * HB_VM_STACK.pPos )->item.asLong.length = HB_LONG_LENGTH( lNumber );

   hb_stackPush();
}

#if !defined( HB_LONG_LONG_OFF )
static void hb_vmPushLongLongConst( LONGLONG llNumber )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLongLongConst(%" PFLL "d)", llNumber));

   ( * HB_VM_STACK.pPos )->type = HB_IT_LONG;
   ( * HB_VM_STACK.pPos )->item.asLong.value = llNumber;
   ( * HB_VM_STACK.pPos )->item.asLong.length = hb_vmCalcIntWidth( llNumber );

   hb_stackPush();
}
#endif

HB_EXPORT void hb_vmPushDouble( double dNumber, int iDec )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDouble(%lf, %d)", dNumber, iDec));

   ( * HB_VM_STACK.pPos )->type = HB_IT_DOUBLE;
   ( * HB_VM_STACK.pPos )->item.asDouble.value = dNumber;
   ( * HB_VM_STACK.pPos )->item.asDouble.length = HB_DBL_LENGTH( dNumber );
   if( iDec == HB_DEFAULT_DECIMALS )
   {
      ( * HB_VM_STACK.pPos )->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   }
   else
   {
      ( * HB_VM_STACK.pPos )->item.asDouble.decimal = iDec;
   }

   hb_stackPush();
}

#if 0
static int hb_vmCalcDoubleWidth( double dNumber, int iDec )
{
   int iSize;

   if ( dNumber < 0 )
   {
      iSize = dNumber > -10 ? 2 : ( int ) log10( -dNumber ) + 2;
   }
   else
   {
      iSize = dNumber < 10 ? 1 : ( int ) log10( dNumber ) + 1;
   }

   return iDec == 0 ? iSize + 1 : iSize;
}
#endif

static void hb_vmPushDoubleConst( double dNumber, int iWidth, int iDec )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDoubleConst(%lf, %d, %d)", dNumber, iWidth, iDec));

   ( * HB_VM_STACK.pPos )->type = HB_IT_DOUBLE;
   ( * HB_VM_STACK.pPos )->item.asDouble.value = dNumber;

   if( iDec == HB_DEFAULT_DECIMALS )
      ( * HB_VM_STACK.pPos )->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   else
      ( * HB_VM_STACK.pPos )->item.asDouble.decimal = iDec;

   if( iWidth == HB_DEFAULT_WIDTH )
      ( * HB_VM_STACK.pPos )->item.asDouble.length = HB_DBL_LENGTH( dNumber );
   else
      ( * HB_VM_STACK.pPos )->item.asDouble.length = iWidth;

   hb_stackPush();
}

HB_EXPORT void hb_vmPushDate( LONG lDate )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDate(%ld)", lDate));

   ( * HB_VM_STACK.pPos )->type = HB_IT_DATE;
   ( * HB_VM_STACK.pPos )->item.asDate.value = lDate;
   hb_stackPush();
}

HB_EXPORT void hb_vmPushPointer( void * pPointer )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushPointer(%ld)", pPointer));

   ( * HB_VM_STACK.pPos )->type = HB_IT_POINTER;
   ( * HB_VM_STACK.pPos )->item.asPointer.value = pPointer;
   ( * HB_VM_STACK.pPos )->item.asPointer.collect = FALSE;

   hb_stackPush();
}

HB_EXPORT void hb_vmPushString( const char * szText, ULONG length )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushString( \"%s\", %lu ) %p", szText, length, ( * HB_VM_STACK.pPos ) ) );

   hb_itemPutCL( ( * HB_VM_STACK.pPos ), szText, length );

   hb_stackPush();
}

HB_EXPORT void hb_vmPushSymbol( PHB_SYMB pSym )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_DEBUG, ("hb_vmPushSymbol(%p) \"%s\"", pSym, pSym->szName ) );

   #if 0
      printf( "Symbol: %s\n", pSym->szName );

      if( pSym->pDynSym )
      {
         printf( "Module: %p\n", pSym->pDynSym->pModuleSymbols );

         if( pSym->pDynSym->pModuleSymbols )
         {
            printf( "ModuleName: %s\n", pSym->pDynSym->pModuleSymbols->szModuleName );
         }
      }
   #endif

   ( * HB_VM_STACK.pPos )->type = HB_IT_SYMBOL;

   if( pSym == &( hb_symEval ) && HB_VM_STACK.pBase && (* HB_VM_STACK.pBase)->type == HB_IT_SYMBOL && ( * HB_VM_STACK.pBase )->item.asSymbol.value->pDynSym )
   {
      pSym->pDynSym->pModuleSymbols = (* HB_VM_STACK.pBase)->item.asSymbol.value->pDynSym->pModuleSymbols;
   }

   ( * HB_VM_STACK.pPos )->item.asSymbol.value = pSym;
   ( * HB_VM_STACK.pPos )->item.asSymbol.stackbase = hb_stackTopOffset();
   ( * HB_VM_STACK.pPos )->item.asSymbol.uiSuperClass = 0;


   hb_stackPush();
}

HB_EXPORT void hb_vmPushDynSym( PHB_DYNS pDynSym )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDynSym(%p)", pDynSym));

   hb_stackPush();
   pItem = hb_stackItemFromTop( -1 );
   pItem->type = HB_IT_SYMBOL;
   pItem->item.asSymbol.value = pDynSym->pSymbol;
   pItem->item.asSymbol.stackbase = hb_stackTopOffset();
   pItem->item.asSymbol.uiSuperClass = 0;
}

HB_EXPORT void hb_vmPushEvalSym( void )
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
static void hb_vmPushBlock( const BYTE * pCode, USHORT usSize, BOOL bDynCode, PHB_SYMB pSymbols, PHB_ITEM** pGlobals )
{
   HB_THREAD_STUB
   PHB_ITEM pTop;
   BYTE * pBlockCode;

   USHORT uiLocals;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushBlock(%p, %p)", pCode, pSymbols));

   pTop = hb_stackTopItem();
   hb_stackPush();

   uiLocals = HB_PCODE_MKUSHORT( &pCode[ 2 ] );

   usSize -= 4 + ( uiLocals << 1 );
   if( bDynCode )
   {
      pBlockCode = ( BYTE * ) hb_xgrab( usSize );
      memcpy( pBlockCode, pCode + 4 + ( uiLocals << 1 ), usSize );
   }
   else
   {
      pBlockCode = ( BYTE * ) pCode + 4 + ( uiLocals << 1 );
   }

   pTop->item.asBlock.value =
         hb_codeblockNew( pBlockCode,               /* pcode buffer */
         uiLocals,                                  /* number of referenced local variables */
         pCode + 4,                                 /* table with referenced local variables */
         pSymbols, pGlobals );

   pTop->type = HB_IT_BLOCK;

   /* store the statics base of function where the codeblock was defined
    */
   pTop->item.asBlock.statics = HB_VM_STACK.iStatics;
   /* store the number of expected parameters
    */
   pTop->item.asBlock.paramcnt = HB_PCODE_MKUSHORT( pCode );

   /* store the line number where the codeblock was defined
    */
   pTop->item.asBlock.value->procname = ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName;
   pTop->item.asBlock.value->lineno = ( *HB_VM_STACK.pBase )->item.asSymbol.lineno;

   if( HB_IS_ARRAY( *( HB_VM_STACK.pBase + 1 ) ) )  /* it is a method name */
   {
      pTop->item.asBlock.value->uiClass = ( *( HB_VM_STACK.pBase + 1 ) )->item.asArray.value->uiClass;

      //TraceLog( NULL, "OBJECT Block: '%s' Line: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, ( *HB_VM_STACK.pBase )->item.asSymbol.lineno );
   }
   else
   {
      //TraceLog( NULL, "PROC Block: '%s' Line: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, ( *HB_VM_STACK.pBase )->item.asSymbol.lineno );

      pTop->item.asBlock.value->uiClass = 0;
   }

   pTop->item.asBlock.value->uLen = usSize;
   pTop->item.asBlock.value->dynBuffer = bDynCode;
}

/* -2    -> HB_P_PUSHBLOCKSHORT
 * -1    -> size of codeblock
 *  0    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlockShort( const BYTE * pCode, USHORT usSize, BOOL bDynCode, PHB_SYMB pSymbols, PHB_ITEM** pGlobals )
{
   HB_THREAD_STUB
   PHB_ITEM pTop;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushBlockShort(%p, %p)", pCode, pSymbols));

   pTop = hb_stackTopItem();
   hb_stackPush();

   if( bDynCode )
   {
      BYTE * pBuffer = ( BYTE * ) hb_xgrab( usSize );
      memcpy( pBuffer, pCode, usSize );
      pCode = pBuffer;
   }

   pTop->item.asBlock.value =
         hb_codeblockNew( pCode,                    /* pcode buffer         */
         0,                                         /* number of referenced local variables */
         NULL,                                      /* table with referenced local variables */
         pSymbols, pGlobals );

   pTop->type = HB_IT_BLOCK;

   /* store the statics base of function where the codeblock was defined
    */
   pTop->item.asBlock.statics = HB_VM_STACK.iStatics;
   /* store the number of expected parameters
    */
   pTop->item.asBlock.paramcnt = 0;

   /* store the line number where the codeblock was defined
    */
   pTop->item.asBlock.value->procname = ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName;
   pTop->item.asBlock.value->lineno = ( *HB_VM_STACK.pBase )->item.asSymbol.lineno;

   if( HB_IS_ARRAY( *( HB_VM_STACK.pBase + 1 ) ) )  /* it is a method name */
   {
      pTop->item.asBlock.value->uiClass = ( *( HB_VM_STACK.pBase + 1 ) )->item.asArray.value->uiClass;

      //TraceLog( NULL, "OBJECT Block: '%s' Line: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, ( *HB_VM_STACK.pBase )->item.asSymbol.lineno
   }
   else
   {
      //TraceLog( NULL, "PROC Block: '%s' Line: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, ( *HB_VM_STACK.pBase )->item.asSymbol.lineno

      pTop->item.asBlock.value->uiClass = 0;
   }

   pTop->item.asBlock.value->uLen = usSize;
   pTop->item.asBlock.value->dynBuffer = bDynCode;
}

/* +0    -> HB_P_MPUSHBLOCK
 * +1 +2 -> size of codeblock
 * +3 +4 -> number of expected parameters
 * +5    -> start of pcode
 *
 * NOTE: pCode points to dynamically allocated memory
 */
static void hb_vmPushMacroBlock( BYTE * pCode, PHB_SYMB pSymbols )
{
   HB_THREAD_STUB
   PHB_ITEM pTop;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushMacroBlock(%p, %p)", pCode, pSymbols));

   HB_SYMBOL_UNUSED( pSymbols ); /* TODO: remove pSymbols */

   pTop = hb_stackTopItem();
   hb_stackPush();

   pTop->item.asBlock.value = hb_codeblockMacroNew( pCode + 5, HB_PCODE_MKUSHORT( &( pCode[ 1 ] ) ) - 5 );
   pTop->type = HB_IT_BLOCK;

   /* store the statics base of function where the codeblock was defined
    */
   pTop->item.asBlock.statics = HB_VM_STACK.iStatics;

   /* store the number of expected parameters
    */
   pTop->item.asBlock.paramcnt = HB_PCODE_MKUSHORT( &( pCode[ 3 ] ) );

   /* store the line number where the codeblock was defined
    */
   pTop->item.asBlock.value->procname = ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName;
   pTop->item.asBlock.value->lineno = ( *HB_VM_STACK.pBase )->item.asSymbol.lineno;

   if( HB_IS_ARRAY( *( HB_VM_STACK.pBase + 1 ) ) )  /* it is a method name */
   {
      pTop->item.asBlock.value->uiClass = ( *( HB_VM_STACK.pBase + 1 ) )->item.asArray.value->uiClass;

      //TraceLog( NULL, "OBJECT Block: '%s' Line: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, ( *HB_VM_STACK.pBase )->item.asSymbol.lineno );
   }
   else
   {
      //TraceLog( NULL, "PROC Block: '%s' Line: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, ( *HB_VM_STACK.pBase )->item.asSymbol.lineno );

      pTop->item.asBlock.value->uiClass = 0;
   }

   pTop->item.asBlock.value->uLen = HB_PCODE_MKUSHORT( &( pCode[ 1 ] ) ) - 5;
}

/* pushes current workarea number on the eval stack
 */
static void hb_vmPushAlias( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAlias()"));

   ( * HB_VM_STACK.pPos )->type = HB_IT_INTEGER;
   ( * HB_VM_STACK.pPos )->item.asInteger.value = hb_rddGetCurrentWorkAreaNumber();
   ( * HB_VM_STACK.pPos )->item.asInteger.length = 10;
   hb_stackPush();
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of a given field
 * (for performance reason it replaces alias value with field value)
 */
static void hb_vmPushAliasedField( PHB_SYMB pSym )
{
   HB_THREAD_STUB

   PHB_ITEM pAlias;
   int iCurrArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAliasedField(%p)", pSym));

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   pAlias = hb_stackItemFromTop( -1 );

   /* NOTE: hb_vmSelecWorkarea clears passed item
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAliasedVar(%p)", pSym));

   if( HB_IS_STRING( pAlias ) )
   {
      char * szAlias = pAlias->item.asString.value;

      if( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' )
      {
         if( szAlias[ 1 ] == '\0' || /* M->variable */
             ( pAlias->item.asString.length >= 4 &&
               hb_strnicmp( szAlias, "MEMVAR", /* MEMVAR-> or MEMVA-> or MEMV-> */
                                     pAlias->item.asString.length ) == 0 ) )
         {
            hb_memvarGetValue( pAlias, pSym );
            return;
         }
      }
      else if( pAlias->item.asString.length >= 4 &&
               hb_strnicmp( szAlias, "FIELD", /* FIELD-> or FIEL-> */
                                     pAlias->item.asString.length ) == 0 )
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLocal(%hd)", iLocal));

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = hb_stackItemFromBase( iLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * hb_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = hb_codeblockGetVar( hb_stackSelfItem(), ( LONG ) iLocal );
   }

   if( HB_IS_BYREF( pLocal ) )
   {
      hb_itemCopy( ( * HB_VM_STACK.pPos ), hb_itemUnRef( pLocal ) );
   }
   else
   {
      hb_itemCopy( ( * HB_VM_STACK.pPos ), pLocal );
   }

   hb_stackPush();
}

static void hb_vmPushLocalByRef( SHORT iLocal )
{
   HB_THREAD_STUB

   PHB_ITEM pTop = ( * HB_VM_STACK.pPos );

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLocalByRef(%hd)", iLocal));

   pTop->type = HB_IT_BYREF;
   /* we store its stack offset instead of a pointer to support a dynamic stack */
   pTop->item.asRefer.offset = hb_stackBaseOffset();

   if( iLocal >= 0 )
   {
      // SomeFunc( ... ) - Variable paramaters.
      if( hb_stackBaseItem()->item.asSymbol.paramcnt > 255 )
      {
         iLocal += hb_stackBaseItem()->item.asSymbol.paramcnt - 256;
      }
#ifdef HB_UNSHARE_REFERENCES
      hb_itemUnShare( *( HB_VM_STACK.pBase + iLocal + 1 ) );
#endif
      pTop->item.asRefer.BasePtr.itemsbasePtr = &HB_VM_STACK.pItems;
   }
   else
   {
      /* store direct codeblock address because an item where a codeblock
       * is stored can be no longer placed on the eval stack at the time
       * of a codeblock evaluation or variable access
      */
      pTop->item.asRefer.BasePtr.block = (hb_stackSelfItem())->item.asBlock.value;
   }

   pTop->item.asRefer.value = iLocal;

   hb_stackPush();
}

static void hb_vmPushStatic( USHORT uiStatic )
{
   HB_THREAD_STUB

   PHB_ITEM pStatic;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStatic(%hu)", uiStatic));

   pStatic = s_aStatics.item.asArray.value->pItems + HB_VM_STACK.iStatics + uiStatic - 1;

   if( HB_IS_BYREF( pStatic ) )
   {
      hb_itemCopy( ( * HB_VM_STACK.pPos ), hb_itemUnRef( pStatic ) );
   }
   else
   {
      hb_itemCopy( ( * HB_VM_STACK.pPos ), pStatic );
   }

   hb_stackPush();
}

static void hb_vmPushStaticByRef( USHORT uiStatic )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pTop = ( * HB_VM_STACK.pPos );

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStaticByRef(%hu)", uiStatic));

#ifdef HB_UNSHARE_REFERENCES
   hb_itemUnShare( s_aStatics.item.asArray.value->pItems + HB_VM_STACK.iStatics + uiStatic - 1 );
#endif

   pTop->type = HB_IT_BYREF;
   /* we store the offset instead of a pointer to support a dynamic stack */
   pTop->item.asRefer.value = HB_VM_STACK.iStatics + uiStatic - 1;
   pTop->item.asRefer.offset = 0;    /* 0 for static variables */
   pTop->item.asRefer.BasePtr.pBaseArray = s_aStatics.item.asArray.value;

   #ifdef HB_ARRAY_USE_COUNTER
      s_aStatics.item.asArray.value->ulHolders++;
   #else
      hb_arrayRegisterHolder( s_aStatics.item.asArray.value, (void *) pTop );
   #endif

   hb_stackPush();
}

static void hb_vmPushVariable( PHB_SYMB pVarSymb )
{
   HB_THREAD_STUB
   USHORT uiAction = E_DEFAULT;
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_INFO, ("(hb_vmPushVariable)"));

   pItem = hb_stackTopItem();
   hb_stackPush();

   do
   {
      /* First try if passed symbol is a name of field
         * in a current workarea - if it is not a field (FAILURE)
         * then try the memvar variable
         */

      if( hb_rddFieldGet( pItem, pVarSymb ) != SUCCESS )
      {
         if( hb_memvarGet( pItem, pVarSymb ) != SUCCESS )
         {
            HB_ITEM_PTR pError;

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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDuplicate()"));

   hb_itemCopy( ( * HB_VM_STACK.pPos ), hb_stackItemFromTop( -1 ) );
   hb_stackPush();
}

static void hb_vmDuplTwo( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDuplTwo()"));

   hb_itemCopy( ( * HB_VM_STACK.pPos ), hb_stackItemFromTop( -2 ) );
   hb_stackPush();
   hb_itemCopy( ( * HB_VM_STACK.pPos ), hb_stackItemFromTop( -2 ) );
   hb_stackPush();
}

/* ------------------------------- */
/* Pop                             */
/* ------------------------------- */

HB_EXPORT void hb_vmPopState( void )
{
   HB_THREAD_STUB

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_vmPopState()" ) );

   hb_itemForwardValue( &(HB_VM_STACK.Return), hb_stackItemFromTop( -1 ) );
   hb_stackDec();

   /* Restore top item */
   hb_stackDec();
}

static BOOL hb_vmPopLogical( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopLogical()"));

   if( HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) )
   {
      hb_stackDec();

      ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;
      return ( * HB_VM_STACK.pPos )->item.asLogical.value;
   }
#ifdef HB_USE_NUMERIC_IF
   else if( HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) )
   {
      hb_stackDec();

      ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;

      if( HB_IS_INTEGER( hb_stackItemFromTop( -1 ) ) )
      {
         return ( ( * HB_VM_STACK.pPos )->item.asInteger.value != 0 );
      }
      else if( HB_IS_LONG( hb_stackItemFromTop( -1 ) ) )
      {
         return ( ( * HB_VM_STACK.pPos )->item.asLong.value != 0 );
      }
      else
      {
         return ( ( * HB_VM_STACK.pPos )->item.asDouble.value != 0.0 );
      }
   }
#endif
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

   PHB_ITEM pItem;
   double dNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopNumber()"));

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
         dNumber = (double) pItem->item.asDate.value;
         break;

      case HB_IT_STRING:
         dNumber = (double) ( BYTE ) ( pItem->item.asString.value[0] );
         hb_itemReleaseString( pItem );
         break;

      default:
         dNumber = 0;  /* To avoid GCC -O2 warning */
         hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_vmPopNumber()", NULL );
         break;
   }

   ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;

   return dNumber;
}

/* NOTE: Type checking should be done by the caller. */

static HB_LONG hb_vmPopHBLong( void )
{
   HB_THREAD_STUB

   PHB_ITEM pItem;
   HB_LONG lNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopHBLong()"));

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
         lNumber = 0;  /* To avoid GCC -O2 warning */
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

   PHB_ITEM pItem;
   double dNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopDouble(%p)", piDec));

   pItem = hb_stackItemFromTop( -1 );

   hb_stackDec();

   dNumber = hb_itemGetNDDec( pItem, piDec );

   ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;

   return dNumber;
}

/* Pops the item from the eval stack and uses it to select the current
 * workarea
 */
static void hb_vmPopAlias( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAlias()"));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAliasedField(%p)", pSym));

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();

   if( hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ), pSym ) == SUCCESS )
   {
      hb_rddPutFieldValue( hb_stackItemFromTop( -2 ), pSym );
   }

   hb_rddSelectWorkAreaNumber( iCurrArea );
   hb_stackDec();    /* alias - it was cleared in hb_vmSelectWorkarea */
   hb_stackPop();    /* field value */
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into either a field or a memvar based on the alias value
 * This is used in the following context:
 * ( any_alias )->variable
 */
static void hb_vmPopAliasedVar( PHB_SYMB pSym )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pAlias = hb_stackItemFromTop( -1 );

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAliasedVar(%p)", pSym));

   /* "M", "MEMV" - "MEMVAR" and "FIEL" - "FIELD" are reserved aliases
    */
   if( HB_IS_STRING( pAlias ) )
   {
      char * szAlias = pAlias->item.asString.value;

      if( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' )
      {
         if( szAlias[ 1 ] == '\0' || /* M->variable */
             ( pAlias->item.asString.length >= 4 &&
               hb_strnicmp( szAlias, "MEMVAR", /* MEMVAR-> or MEMVA-> or MEMV-> */
                                     pAlias->item.asString.length ) == 0 ) )
         {
            hb_memvarSetValue( pSym, hb_stackItemFromTop( -2 ) );
            hb_stackPop();    /* alias */
            hb_stackPop();    /* value */
            return;
         }
      }
      else if( pAlias->item.asString.length >= 4 &&
               hb_strnicmp( szAlias, "FIELD", /* FIELD-> or FIEL-> */
                                     pAlias->item.asString.length ) == 0 )
      {
         hb_rddPutFieldValue( hb_stackItemFromTop( -2 ), pSym );
         hb_stackPop();    /* alias */
         hb_stackPop();    /* value */
         return;
      }
   }
   hb_vmPopAliasedField( pSym );
}

static void hb_vmPopLocal( SHORT iLocal )
{
   HB_THREAD_STUB

   PHB_ITEM pLocal, pVal;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopLocal(%hd)", iLocal));

   pVal = *( HB_VM_STACK.pPos - 1 );

   /* Remove MEMOFLAG if exists (assignment from field). */
   pVal->type &= ~HB_IT_MEMOFLAG;

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = hb_stackItemFromBase( iLocal );

      /* Assigned to a parameter by refrence. */
      if( HB_IS_BYREF( pLocal ) )
      {
         pLocal = hb_itemUnRef( pLocal );
      }
   }
   else
   {
      /* local variable referenced in a codeblock
       * hb_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = hb_codeblockGetVar( hb_stackSelfItem(), iLocal ) ;
   }

   if( ( HB_IS_NUMBER( pLocal ) && HB_IS_NUMBER( pVal ) ) || pLocal->type == pVal->type )
   {
      hb_itemForwardValue( pLocal, pVal );
   }
   else if( hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmOperatorCall( pLocal, pVal, "__OPASSIGN", NULL, 0, pLocal );
      hb_itemClear( pVal );
   }
   else
   {
      hb_itemForwardValue( pLocal, pVal );
   }

   hb_stackDec();
}

static void hb_vmPopStatic( USHORT uiStatic )
{
   HB_THREAD_STUB

   PHB_ITEM pStatic, pVal;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopStatic(%hu)", uiStatic));

   pVal = *( HB_VM_STACK.pPos - 1 );

   /* Remove MEMOFLAG if exists (assignment from field). */
   pVal->type &= ~HB_IT_MEMOFLAG;

   pStatic = s_aStatics.item.asArray.value->pItems + HB_VM_STACK.iStatics + uiStatic - 1;

   //TraceLog( NULL, "Assign Static: %i, Class: %s\n", uiStatic, hb_objGetClsName( pVal ) );

   if( ( HB_IS_NUMBER( pStatic ) && HB_IS_NUMBER( pVal ) ) || pStatic->type == pVal->type )
   {
      hb_itemForwardValue( pStatic, pVal );
   }
   else if( hb_objGetOpOver( pStatic ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmOperatorCall( pStatic, pVal, "__OPASSIGN", NULL, 0, pStatic );
      hb_itemClear( pVal );
   }
   else
   {
      hb_itemForwardValue( pStatic, pVal );
   }

   hb_stackDec();
}

/* ----------------------------------------------- */

static PSYMBOLS hb_vmFindFreeModule( PHB_SYMB pSymbols, USHORT uiSymbols, char * szModuleName )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmFindFreeModule(%p,%hu,%s)", pSymbols, uiSymbols, szModuleName));

   if( s_ulFreeSymbols )
   {
      while( pLastSymbols )
      {
         if( !pLastSymbols->fActive &&
             pLastSymbols->uiModuleSymbols == uiSymbols &&
             pLastSymbols->szModuleName != NULL &&
             strcmp( pLastSymbols->szModuleName, szModuleName ) == 0 )
         {
            PHB_SYMB pModuleSymbols = pLastSymbols->pModuleSymbols;
            USHORT ui;

            for( ui = 0; ui < uiSymbols; ++ui )
            {
               if( ( pSymbols[ ui ].scope.value & ~( HB_FS_PCODEFUNC | HB_FS_DYNCODE ) ) !=
                     pModuleSymbols[ ui ].scope.value ||
                   strcmp( pSymbols[ ui ].szName, pModuleSymbols[ ui ].szName ) != 0 )
               {
                  break;
               }
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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmFreeSymbols(%p)", pSymbols));

   if( pSymbols->fActive )
   {
      USHORT ui;

      for( ui = 0; ui < pSymbols->uiModuleSymbols; ++ui )
      {
         HB_SYMBOLSCOPE scope = pSymbols->pModuleSymbols[ ui ].scope.value & HB_FS_INITEXIT;

         /* do not overwrite already initialized statics' frame */
         if( scope != HB_FS_INITEXIT )
         {
            pSymbols->pModuleSymbols[ ui ].value.pFunPtr = NULL;
         }

         pSymbols->pModuleSymbols[ ui ].scope.value &= ~( HB_FS_PCODEFUNC | HB_FS_DYNCODE );
      }

      pSymbols->hDynLib = NULL;
      pSymbols->fActive = FALSE;
      ++s_ulFreeSymbols;
   }
}

void hb_vmBeginSymbolGroup( void * hDynLib, BOOL fClone )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmBeginSymbolGroup(%p,%d)", hDynLib, (int)fClone));

   s_hDynLibID = hDynLib;
   s_fCloneSym = fClone;
}

void hb_vmInitSymbolGroup( void * hNewDynLib, int argc, char * argv[] )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInitSymbolGroup(%p,%d,%p)", hNewDynLib, argc, argv));

   s_fCloneSym = FALSE;

   if( s_hDynLibID )
   {
      PSYMBOLS pLastSymbols = s_pSymbols;
      void * hDynLib = s_hDynLibID;
      BOOL fFound = FALSE;
      USHORT ui;

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
                  HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

                  if( scope == HB_FS_INITEXIT )
                  {
                     hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
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
                     HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

                     if( scope == HB_FS_INIT )
                     {
                        int i;
                        hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                        hb_vmPushNil();
                        for( i = 0; i < argc; ++i )
                        {
                           hb_vmPushString( argv[i], strlen( argv[i] ) );
                        }
                        hb_vmDo( argc );
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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmExitSymbolGroup(%p)", hDynLib));

   if( hDynLib )
   {
      PSYMBOLS pLastSymbols = s_pSymbols;
      BOOL fFound = FALSE;

      while( pLastSymbols )
      {
         if( pLastSymbols->hDynLib == hDynLib )
         {
            fFound = TRUE;
            if( pLastSymbols->fActive && ( pLastSymbols->hScope & HB_FS_EXIT ) != 0 )
            {
               USHORT ui;
               for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
               {
                  HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

                  if( scope == HB_FS_EXIT )
                  {
                     hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
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
               hb_vmFreeSymbols( pLastSymbols );
            }
            pLastSymbols = pLastSymbols->pNext;
         }
      }
   }
}

PSYMBOLS hb_vmRegisterSymbols( PHB_SYMB pModuleSymbols, USHORT uiSymbols, char * szModuleName, BOOL fDynLib, BOOL fClone )
{
   PSYMBOLS pNewSymbols;
   BOOL fRecycled, fInitStatics = FALSE;
   USHORT ui;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmRegisterSymbols(%p,%hu,%s,%d,%d)", pModuleSymbols, uiSymbols, szModuleName, (int)fDynLib, (int)fClone));

   pNewSymbols = s_ulFreeSymbols == 0 ? NULL : hb_vmFindFreeModule( pModuleSymbols, uiSymbols, szModuleName );

   if( pNewSymbols )
   {
      pNewSymbols->fActive = fRecycled = TRUE;
      pNewSymbols->hDynLib = s_hDynLibID;
      pNewSymbols->hScope = 0;
   }
   else
   {
      fRecycled = FALSE;

      if( fClone )
      {
         PHB_SYMB pSymbols = ( PHB_SYMB ) hb_xgrab( uiSymbols * sizeof( HB_SYMB ) );

         memcpy( pSymbols, pModuleSymbols, uiSymbols * sizeof( HB_SYMB ) );

         for( ui = 0; ui < uiSymbols; ui++ )
         {
            pSymbols[ ui ].szName = hb_strdup( pSymbols[ ui ].szName );
         }

         pModuleSymbols = pSymbols;
      }

      pNewSymbols = ( PSYMBOLS ) hb_xgrab( sizeof( SYMBOLS ) );
      pNewSymbols->pModuleSymbols = pModuleSymbols;
      pNewSymbols->uiModuleSymbols = uiSymbols;
      pNewSymbols->szModuleName = hb_strdup( szModuleName );
      pNewSymbols->fAllocated = fClone;
      pNewSymbols->fActive = TRUE;
      pNewSymbols->fInitStatics = FALSE;
      pNewSymbols->hDynLib = s_hDynLibID;
      pNewSymbols->hScope = 0;
      pNewSymbols->pNext = NULL;

      if( s_pSymbols == NULL )
      {
         s_pSymbols = pNewSymbols;
      }
      else
      {
         PSYMBOLS pLastSymbols;

         pLastSymbols = s_pSymbols;

         while( pLastSymbols->pNext ) /* locates the latest processed group of symbols */
         {
            pLastSymbols = pLastSymbols->pNext;
         }

         pLastSymbols->pNext = pNewSymbols;
      }
   }

   for( ui = 0; ui < uiSymbols; ui++ ) /* register each public symbol on the dynamic symbol table */
   {
      PHB_SYMB pSymbol = pNewSymbols->pModuleSymbols + ui;
      HB_SYMBOLSCOPE hSymScope;
      BOOL fPublic, fStatics;

      fStatics = ( pSymbol->scope.value & HB_FS_INITEXIT ) == HB_FS_INITEXIT;

      if( fRecycled && !fStatics )
      {
         pSymbol->value.pFunPtr = ( pModuleSymbols + ui )->value.pFunPtr;
         pSymbol->scope.value = ( pModuleSymbols + ui )->scope.value;
      }

      if( fDynLib )
      {
         pSymbol->scope.value |= HB_FS_DYNCODE;
      }

      hSymScope = pSymbol->scope.value;
      pNewSymbols->hScope |= hSymScope;
      fPublic = ( hSymScope & ( HB_FS_PUBLIC | HB_FS_MESSAGE | HB_FS_MEMVAR ) ) != 0;

      if( fStatics )
      {
         fInitStatics = TRUE;
      }

      if( ( hSymScope & HB_FS_PCODEFUNC ) != 0 && ( fRecycled || fClone ) )
      {
         pSymbol->value.pCodeFunc->pSymbols = pNewSymbols->pModuleSymbols;
      }

      if( !s_pSymStart && !fDynLib && ( hSymScope & HB_FS_FIRST ) != 0 && ( hSymScope & HB_FS_INITEXIT ) == 0 )
      {
         /* first public defined symbol to start execution */
         s_pSymStart = pSymbol;
      }

      /* Enable this code to see static functions which are registered in global dynsym table */
   #if 0
      if( fPublic && ( hSymScope & ( HB_FS_INITEXIT | HB_FS_STATIC ) ) != 0 )
      {
         printf( "Registring: %s:%s scope %04x\r\n", szModuleName, pSymbol->szName, hSymScope ); fflush(stdout);
      }
   #endif

      if( fPublic )
      {
         if( fDynLib && ( pSymbol->value.pFunPtr || ( pSymbol->scope.value & HB_FS_DEFERRED ) ) )
         {
            PHB_DYNS pDynSym;

            pDynSym = hb_dynsymFind( pSymbol->szName );

            if( pDynSym )
            {
               pSymbol->pDynSym = pDynSym;

               // Should we support dynamic overloading of already resolved HB_FS_DEFERRED as per below?
               if( pDynSym->pSymbol->value.pFunPtr )
               {
                  if( ( pSymbol->scope.value & HB_FS_DEFERRED ) /* && pSymbol->value.pFunPtr == NULL */ )
                  {
                     pSymbol->scope.value = ( pSymbol->scope.value & ~HB_FS_PCODEFUNC ) | ( pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC );
                     pSymbol->value.pFunPtr = pDynSym->pSymbol->value.pFunPtr;
                  }
               }
               else if( pSymbol->value.pFunPtr )
               {
                  PSYMBOLS pModuleSymbols;

                  pDynSym->pSymbol = pSymbol;

                  pModuleSymbols = s_pSymbols;

                  while( pModuleSymbols )
                  {
                     if( pModuleSymbols->hScope & HB_FS_DEFERRED )
                     {
                        USHORT ui;

                        //TraceLog( NULL, "Module '%s' has Deferred Symbols\n", pModuleSymbols->szModuleName );

                        for( ui = 0; ui < pModuleSymbols->uiModuleSymbols; ui++ )
                        {
                           PHB_SYMB pModuleSymbol = pModuleSymbols->pModuleSymbols + ui;

                           if( pModuleSymbol->pDynSym == pDynSym && pModuleSymbol->scope.value & HB_FS_DEFERRED /* && pModuleSymbol->value.pFunPtr == NULL */ )
                           {
                               //TraceLog( NULL, "Deferred: '%s'\n", pSymbol->szName );

                               pModuleSymbol->scope.value = ( pModuleSymbol->scope.value & ~HB_FS_PCODEFUNC ) | ( pSymbol->scope.value & HB_FS_PCODEFUNC );
                               pModuleSymbol->value.pFunPtr = pSymbol->value.pFunPtr;
                           }
                        }
                     }

                     pModuleSymbols = pModuleSymbols->pNext;
                  }
               }

               continue;
            }
         }

         hb_dynsymNew( pSymbol, pNewSymbols );
      }
   }

   if( ! fRecycled )
   {
      pNewSymbols->fInitStatics = fInitStatics;
   }

   return pNewSymbols;
}

HB_EXPORT PHB_SYMB hb_vmProcessSymbols( PHB_SYMB pSymbols, ... ) /* module symbols initialization */
{
   va_list ap;
   USHORT uiModuleSymbols;
   int iPCodeVer = 0;
   char *szModule;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessSymbols(%p, %dl )", pSymbols));

#ifdef HB_THREAD_SUPPORT
   /* initialize internal mutex for MT mode */
   hb_threadInit();
#endif

   va_start( ap, pSymbols );

      uiModuleSymbols = (USHORT) va_arg( ap, int );
      szModule = va_arg( ap, char * );

      if( szModule )
      {
         int iLen = strlen( szModule );

         if( ( iLen > 2 && szModule[ iLen - 2 ] == '.' ) ||
             ( iLen > 3 && szModule[ iLen - 3 ] == '.' ) ||
             ( iLen > 4 && szModule[ iLen - 4 ] == '.' ) )
         {
            iPCodeVer = va_arg( ap, int );
         }
         else
         {
            szModule = NULL;
         }
      }

   va_end( ap );

   if( iPCodeVer != HB_PCODE_VER )
   {
      char szBuff[ 512 ];
      char szPCode[ 12 ];

      if( !szModule )
      {
         USHORT ui;
         for( ui = 0; ui < uiModuleSymbols; ui++ )
         {
            if( pSymbols[ui].value.pFunPtr )
            {
               break;
            }
         }
         if( ui == uiModuleSymbols )
         {
            sprintf( szBuff, "Program with 1st sym: %s", pSymbols[0].szName );
         }
         else
         {
            sprintf( szBuff, "Program with 1st fun: %s", pSymbols[ui].szName );
         }
         szModule = szBuff;
      }
      sprintf( szPCode, "%i", iPCodeVer );

      hb_errInternal( HB_EI_ERRUNRECOV,
                      "'%s' was compiled by older version, "
                      "PCODE version %s is no longer supported - "
                      "Please recompile.\n", szModule, szPCode );
   }

   return hb_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModule,
                                s_fCloneSym, s_fCloneSym )->pModuleSymbols;
}

/* hvm support for pcode DLLs */

HB_EXPORT PHB_SYMB hb_vmProcessDllSymbols( PHB_SYMB pSymbols, ... )
{
   va_list ap;
   USHORT uiModuleSymbols;
   char *szModule;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessDllSymbols(%p, %hu)", pSymbols, uiModuleSymbols));

#ifdef HB_THREAD_SUPPORT
   /* initialize internal mutex for MT mode */
   hb_threadInit();
#endif
   va_start( ap, pSymbols );
      uiModuleSymbols = (USHORT) va_arg( ap, int );
      szModule = va_arg( ap, char * );
   va_end( ap );

   return hb_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModule,
                                TRUE, s_fCloneSym )->pModuleSymbols;
}

HB_EXPORT PSYMBOLS * hb_vmSymbols( void )
{
   return &s_pSymbols;
}

HB_EXPORT PSYMBOLS hb_vmLastModule( void )
{
   PSYMBOLS pLastModule = s_pSymbols;

   if( pLastModule )
   {
      while( pLastModule->pNext )
      {
         pLastModule = pLastModule->pNext;
      }
   }

   return pLastModule;
}

HB_EXPORT void hb_vmExplicitStartup( PHB_SYMB pSymbol )
{
   s_pSymStart = pSymbol;  /* first public defined symbol to start execution */
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoInitStatics()"));

   do
   {
      if( pLastSymbols->fInitStatics )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

            if( scope == HB_FS_INITEXIT )
            {
               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               hb_vmPushNil();
               hb_vmDo( 0 );
            }
         }
         pLastSymbols->fInitStatics = FALSE;
      }
      pLastSymbols = pLastSymbols->pNext;

   } while( pLastSymbols );
}

HB_EXPORT void hb_vmDoExitFunctions( void )
{
   HB_THREAD_STUB

   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoExitFunctions()"));

   while( pLastSymbols )
   {
      /* only if module contains some EXIT functions */
      if( pLastSymbols->fActive && pLastSymbols->hScope & HB_FS_EXIT )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

            if( scope == HB_FS_EXIT )
            {
               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               hb_vmPushNil();
               hb_vmDo( 0 );

               if( HB_VM_STACK.uiActionRequest )
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoInitFunctions()"));

   do
   {
      /* only if module contains some INIT functions */
      if( pLastSymbols->fActive && pLastSymbols->hScope & HB_FS_INIT )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

            if( scope == HB_FS_INIT )
            {
               int argc = hb_cmdargARGC();
               char ** argv = hb_cmdargARGV();

               int i;
               int iArgCount;

               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               hb_vmPushNil();

               iArgCount = 0;
               for( i = 1; i < argc; i++ ) /* places application parameters on the stack */
               {
                  /* Filter out any parameters beginning with //, like //INFO */
                  if( ! hb_cmdargIsInternal( argv[ i ] ) )
                  {
                     hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );
                     iArgCount++;
                  }
               }

               hb_vmDo( iArgCount );
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;

   } while( pLastSymbols );
}

PSYMBOLS hb_vmFindModule( PHB_SYMB pSymbols )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmFindModule(%p)", pSymbols));

   do
   {
      if( pLastSymbols->pModuleSymbols == pSymbols )
      {
         return pLastSymbols;
      }

      pLastSymbols = pLastSymbols->pNext;

   } while( pLastSymbols );

   return NULL;
}

PSYMBOLS hb_vmFindModuleByName( char *szModuleName )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmFindModuleByName(%s)", szModuleName));

   do
   {
      if( strcmp( pLastSymbols->szModuleName, szModuleName ) == 0 )
      {
         return pLastSymbols;
      }

      pLastSymbols = pLastSymbols->pNext;

   } while( pLastSymbols );

   return NULL;
}

/* NOTE: We should make sure that these get linked.
         Don't make this function static, because it's not called from
         this file. [vszakats] */

void hb_vmForceLink( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmForceLink()"));

   HB_FUNCNAME( SYSINIT )();
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

void HB_EXPORT hb_vmRequestQuit( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestQuit()"));

   HB_VM_STACK.uiActionRequest = HB_QUIT_REQUESTED;
}

void HB_EXPORT hb_vmRequestEndProc( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestEndProc()"));

   HB_VM_STACK.uiActionRequest = HB_ENDPROC_REQUESTED;
}

void HB_EXPORT hb_vmRequestBreak( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestBreak(%p)", pItem));

   if( pItem && HB_VM_STACK.pSequence )
   {
      PHB_SEQUENCE pSequence = HB_VM_STACK.pSequence;

      /*
      while( pSequence && ( pSequence->uiStatus & HB_SEQ_RECOVERED ) && ( pSequence->uiStatus & HB_SEQ_RETHROW == 0 ) )
      {
         pSequence = pSequence->pPrev;
      }
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

   HB_VM_STACK.uiActionRequest = HB_BREAK_REQUESTED;
}

USHORT HB_EXPORT hb_vmRequestQuery( void )
{
   HB_THREAD_STUB

   return HB_VM_STACK.uiActionRequest;
}

void HB_EXPORT hb_vmRequestCancel( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestCancel()"));

   if( hb_set.HB_SET_CANCEL )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 +  64 ]; // 64 for the Canceled at: (%i) overhead.
      int i = 1, i2;
      USHORT uLine;

      hb_conOutErr( hb_conNewLine(), 0 );
      sprintf( buffer, "Cancelled at: %s (%i)", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.lineno );
      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );

      while ( buffer[0] )
      {
         hb_procinfo( i++, buffer, &uLine, NULL );

         if( buffer[0] == 0 )
         {
            break;
         }

         i2 = strlen( (char *) buffer );
         sprintf( buffer + i2, " (%u)", uLine );

         hb_conOutErr( buffer, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }

      HB_VM_STACK.uiActionRequest = HB_QUIT_REQUESTED;
   }
}

void HB_EXPORT hb_vmRequestReset( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestReset()"));

   HB_VM_STACK.uiActionRequest = 0;
}

void HB_EXPORT hb_vmRequest( USHORT uiRequest )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequest()"));

   HB_VM_STACK.uiActionRequest = uiRequest;
}

/* ------------------------------ */
/* The debugger support functions */
/* ------------------------------ */

void HB_EXPORT hb_vmRequestDebug( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestDebug()"));

   s_bDebugRequest = TRUE;
}

/* Check if debugger activation was requested or request debugger activation
 * if .T. is passed */
HB_FUNC( HB_DBG_INVOKEDEBUG )
{
   HB_THREAD_STUB_API

   BOOL bRequest = s_bDebugRequest;

   if ( hb_pcount() > 0 )
   {
      s_bDebugRequest = hb_parl( 1 );
   }
   else
   {
      s_bDebugRequest = FALSE;
   }
   hb_retl( bRequest );
}


BOOL HB_EXPORT
hb_dbg_InvokeDebug( BOOL bInvoke )
{
   BOOL bRequest = s_bDebugRequest;

   s_bDebugRequest = bInvoke;
   return bRequest;
}


/* $Doc$
 * $FuncName$     <aStat> hb_dbg_vmVarSList()
 * $Description$  Return a clone of the statics array.
 * $End$ */
HB_FUNC( HB_DBG_VMVARSLIST )
{
   HB_THREAD_STUB

   PHB_ITEM pStatics = hb_arrayClone( &s_aStatics, NULL );

   hb_itemForwardValue( &(HB_VM_STACK.Return), pStatics );
   hb_itemRelease( pStatics );
}

/* $Doc$
 * $FuncName$     <nStatics> hb_dbg_vmVarSLen()
 * $Description$  Return the statics array length.
 * $End$ */
HB_FUNC( HB_DBG_VMVARSLEN )
{
   HB_THREAD_STUB_API

   hb_retnl( s_aStatics.item.asArray.value->ulLen );
}


HB_EXPORT PHB_ITEM
hb_dbg_vmVarSGet( int nStatic, int nOffset )
{
   return s_aStatics.item.asArray.value->pItems + nStatic + nOffset - 1;
}


/* $Doc$
 * $FuncName$     <xStat> hb_dbg_vmVarSGet(<nStatic>,<nOffset>)
 * $Description$  Return a specified statics
 * $End$ */
HB_FUNC( HB_DBG_VMVARSGET )
{
   HB_THREAD_STUB

   hb_itemCopy( &(HB_VM_STACK.Return),
                hb_dbg_vmVarSGet( hb_parni( 1 ), hb_parni( 2 ) ) );
}

/* $Doc$
 * $FuncName$     hb_dbg_vmVarSSet(<nStatic>,<nOffset>,<uValue>)
 * $Description$  Sets the value of a specified statics
 * $End$ */
HB_FUNC( HB_DBG_VMVARSSET )
{
   hb_itemCopy( s_aStatics.item.asArray.value->pItems + hb_parni(1) + hb_parni(2) - 1, hb_itemParamPtr( 3, HB_IT_ANY ) );
}


ULONG HB_EXPORT
hb_dbg_ProcLevel( void )
{
   return s_ulProcLevel;
}


HB_FUNC( HB_DBG_PROCLEVEL )
{
   HB_THREAD_STUB_API

   hb_retnl( s_ulProcLevel - 1 );   /* Don't count self */
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
       UINT i;

       HB_TRACE(HB_TR_DEBUG, ("hb_vmIsLocalRef()"));

       if( HB_IS_GCITEM( &hb_stackST.Return ) )
       {
          hb_gcItemRef( &(hb_stackST.Return) );
       }
       //printf( "After ReturnRef\n" );

       if( hb_stackST.pPos > hb_stackST.pItems )
       {
          /* the eval stack is not cleared yet */
          HB_ITEM_PTR * pItem = hb_stackST.pPos - 1;

          while( pItem != hb_stackST.pItems )
          {
             if( HB_IS_GCITEM( *pItem ) )
             {
                hb_gcItemRef( *pItem );
             }

             --pItem;
          }
       }

       // FOR EACH Enumerations.
       for( i = 0; i < HB_VM_STACK.wEnumCollectionCounter; i++ )
       {
          if( HB_IS_GCITEM( &( HB_VM_STACK.aEnumCollection[ i ] ) ) )
          {
             hb_gcItemRef( &( HB_VM_STACK.aEnumCollection[ i ] ) );
          }
       }

       // WITH OBJECT
       for( i = 0; i < HB_VM_STACK.wWithObjectCounter; i++ )
       {
          if( HB_IS_GCITEM( &( HB_VM_STACK.aWithObject[ i ] ) ) )
          {
             hb_gcItemRef( &( HB_VM_STACK.aWithObject[ i ] ) );
          }
       }
    }
#endif

/* Mark all statics as used so they will not be released by the
 * garbage collector
 */
void hb_vmIsStaticRef( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsStaticRef()"));

   /* statics are stored as an item of array type */
   hb_gcItemRef( &s_aStatics );
}

void HB_EXPORT hb_vmRegisterGlobals( PHB_ITEM **pGlobals, short iGlobals )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pTop = ( * HB_VM_STACK.pPos );
   short iGlobal;
   USHORT uiPrevLen = ( USHORT ) (&s_aGlobals)->item.asArray.value->ulLen;
   USHORT uiAdd = 0;
   USHORT ulLen;

   ulLen = iGlobals + uiPrevLen;
   hb_arraySize( &s_aGlobals, ulLen );

   pTop->type = HB_IT_BYREF;

   for ( iGlobal = uiPrevLen + 1; iGlobal <= ulLen; iGlobal++ )
   {
      pTop->item.asRefer.value = ++ uiAdd; // To offset the -1 below.
      pTop->item.asRefer.offset = -1; // Because 0 will be translated as a STATIC in hb_itemUnref();
      pTop->item.asRefer.BasePtr.itemsbasePtr = pGlobals;

      hb_arraySet( &s_aGlobals, iGlobal, pTop );
      //printf( "*** Added %i ***\n", iGlobal );
   }

   pTop->type = HB_IT_NIL;
}

void hb_vmIsGlobalRef( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsGlobalRef()"));

   /* Globals are stored as an item of array type */
   hb_gcItemRef( &s_aGlobals );
}

/* $Doc$
 * $FuncName$     <aStat> __vmVarGList()
 * $Description$  Return a clone of the globals array.
 * $End$ */
HB_FUNC( __VMVARGLIST )
{
   HB_THREAD_STUB

   PHB_ITEM pGlobals = hb_arrayClone( &s_aGlobals, NULL );

   hb_itemForwardValue( &(HB_VM_STACK.Return), pGlobals );
   hb_itemRelease( pGlobals );
}

HB_EXPORT ULONG hb_dbg_vmVarGCount( void )
{
   return s_aGlobals.item.asArray.value->ulLen;
}

HB_EXPORT PHB_ITEM
hb_dbg_vmVarGGet( int nGlobal, int nOffset )
{
   return s_aGlobals.item.asArray.value->pItems + nGlobal + nOffset - 1;
}

HB_FUNC( HB_DBG_VMVARGGET )
{
   HB_THREAD_STUB

   hb_itemCopy( &(HB_VM_STACK.Return),
                hb_dbg_vmVarGGet( hb_parni( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( HB_DBG_VMVARGSET )
{
   PHB_ITEM pItem = s_aGlobals.item.asArray.value->pItems + hb_parni( 1 ) + hb_parni( 2 ) - 1;

   hb_itemCopy( hb_itemUnRef( pItem ), hb_itemParamPtr( 3, HB_IT_ANY ) );
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
      BOOL bOldValue = hb_bTracePrgCalls;

      hb_bTracePrgCalls = hb_parl( 1 );

      hb_retl( bOldValue );
   #else

      hb_retl( 0 );
   #endif
}

HB_EXPORT void hb_vmPushBaseArray( PHB_BASEARRAY pBaseArray )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushBaseArray(%p)", pBaseArray));

   ( * HB_VM_STACK.pPos )->type = HB_IT_ARRAY;
   ( * HB_VM_STACK.pPos )->item.asArray.value = pBaseArray;

   #ifdef HB_ARRAY_USE_COUNTER
      pBaseArray->ulHolders++;
   #else
       hb_arrayRegisterHolder( pBaseArray, (void *) ( * HB_VM_STACK.pPos ) );
   #endif

   hb_stackPush();
}

HB_EXPORT void hb_vmPushItemRef( PHB_ITEM pItem, PHB_ITEM * pItemRef[], int iPos )
{
   HB_THREAD_STUB
   PHB_ITEM pTop;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushItemRef(%p, %p, %d)", pItem, pItemRef, iPos));

#ifdef HB_UNSHARE_REFERENCES
   pItem = hb_itemUnShare( pItem );
#endif
   hb_stackPush();
   pTop = hb_stackItemFromTop( -1 );
   ( * pItemRef )[ iPos ] = pItem;
   pTop->type = HB_IT_BYREF;
   pTop->item.asRefer.offset = -1;
   pTop->item.asRefer.BasePtr.itemsbasePtr = pItemRef;
   pTop->item.asRefer.value = iPos + 1;
}

HB_FUNC( HB_FUNCPTR )
{
   HB_THREAD_STUB

   PHB_ITEM pParam = hb_stackItemFromBase( 1 );
   PHB_DYNS pDynSym;

   if( HB_IS_STRING( pParam ) )
   {
      char *sSym = hb_strUpperCopy( pParam->item.asString.value, pParam->item.asString.length );

      pDynSym = hb_dynsymFind( sSym );

      if( pDynSym )
      {
         ( &(HB_VM_STACK.Return) )->type = HB_IT_POINTER;
         ( &(HB_VM_STACK.Return) )->item.asPointer.value = (void *) pDynSym->pSymbol;
         ( &(HB_VM_STACK.Return) )->item.asPointer.collect = FALSE;
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

   PHB_ITEM * pBase = HB_VM_STACK.pBase;
   LONG lLevel = hb_parnl( 1 );

   // Outer function level.
   pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;

   while( ( HB_IS_BLOCK( *( pBase + 1 ) ) || lLevel-- > 0 ) && pBase != HB_VM_STACK.pItems )
   {
      //TraceLog( NULL, "Skipped: %s\n", ( *pBase )->item.asSymbol.value->szName );

      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
   }

   //TraceLog( NULL, "Found: %s\n", ( *pBase )->item.asSymbol.value->szName );

   hb_itemCopy( &(HB_VM_STACK.Return), *( pBase + 1 ) );
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
   ULONG ulOpcode = hb_parnl( 1 );

   hb_reta( 2 );

   if( ulOpcode < HB_P_LAST_PCODE )
   {
      hb_stornl( hb_ulOpcodesCalls[ ulOpcode ], -1, 1 );
      hb_stornl( hb_ulOpcodesTime[ ulOpcode ],  -1, 2 );
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
      PSYMBOLS pModuleSymbols = pBlock->item.asBlock.value->pSymbols ? hb_vmFindModule( pBlock->item.asBlock.value->pSymbols ) : NULL;

      hb_arrayNew( &( HB_VM_STACK.Return ), 4 );

      hb_itemPutC(  hb_arrayGetItemPtr( &( HB_VM_STACK.Return ), 1 ), pModuleSymbols ? pModuleSymbols->szModuleName : "" );
      hb_itemPutCL( hb_arrayGetItemPtr( &( HB_VM_STACK.Return ), 2 ), (char *) pBlock->item.asBlock.value->pCode, pBlock->item.asBlock.value->uLen );
      hb_itemPutNI( hb_arrayGetItemPtr( &( HB_VM_STACK.Return ), 3 ), pBlock->item.asBlock.paramcnt );
      hb_itemPutNI( hb_arrayGetItemPtr( &( HB_VM_STACK.Return ), 4 ), pBlock->item.asBlock.value->uiClass );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9003, NULL, "Not a Codeblock, or Codeblock exports detached locals!", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( HB_RESTOREBLOCK )
{
   HB_THREAD_STUB

   PHB_ITEM pBlockAsArray = hb_param( 1, HB_IT_ARRAY );

   if ( pBlockAsArray && hb_arrayLen( pBlockAsArray ) == 4 )
   {
      PSYMBOLS pModuleSymbols;
      HB_ITEM_NEW( ModuleName );
      HB_ITEM_NEW( PCode );
      HB_ITEM_NEW( ParamCount );
      HB_ITEM_NEW( ClassH );

      hb_arrayGet( pBlockAsArray, 1, &ModuleName );
      hb_arrayGet( pBlockAsArray, 2, &PCode );
      hb_arrayGet( pBlockAsArray, 3, &ParamCount );
      hb_arrayGet( pBlockAsArray, 4, &ClassH );

      if( ModuleName.type & HB_IT_STRING && PCode.type & HB_IT_STRING && ParamCount.type == HB_IT_INTEGER && ClassH.type == HB_IT_INTEGER )
      {
         PHB_ITEM * pBase = HB_VM_STACK.pBase;
         HB_ITEM Block;

         pModuleSymbols = hb_vmFindModuleByName( ModuleName.item.asString.value );

         Block.type = HB_IT_BLOCK;
         Block.item.asBlock.value = hb_codeblockMacroNew( (BYTE *) ( PCode.item.asString.value ), ( USHORT )PCode.item.asString.length );

         Block.item.asBlock.value->uLen     = (USHORT) PCode.item.asString.length;
         Block.item.asBlock.value->pSymbols = pModuleSymbols ? pModuleSymbols->pModuleSymbols : NULL;
         Block.item.asBlock.paramcnt        = ParamCount.item.asInteger.value;
         Block.item.asBlock.value->uiClass  = ClassH.item.asInteger.value;

         Block.item.asBlock.statics = HB_VM_STACK.iStatics;

         pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;

         if( ( *( pBase + 1 ) )->type == HB_IT_BLOCK )
         {
            Block.item.asBlock.value->procname = ( *( pBase + 1 ) )->item.asBlock.value->procname;
            Block.item.asBlock.value->lineno = ( *( pBase + 1 ) )->item.asBlock.value->lineno;
         }
         else
         {
            Block.item.asBlock.value->procname = ( *pBase )->item.asSymbol.value->szName;
            Block.item.asBlock.value->lineno = ( *pBase )->item.asSymbol.lineno;
         }

         //TraceLog( NULL, "Proc: %s Line %i Self: %p\n", Block.item.asBlock.value->procname, Block.item.asBlock.value->lineno, Block.item.asBlock.value->pSelfBase );

         hb_itemForwardValue( &( HB_VM_STACK.Return ), &Block );
      }
	  else
	  {
         hb_errRT_BASE( EG_ARG, 9003, NULL, "Not a persisted Codeblock!", 1, hb_paramError( 1 ) );
	  }

      hb_itemClear( &ModuleName );
      hb_itemClear( &PCode );
      hb_itemClear( &ParamCount );
      hb_itemClear( &ClassH );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9003, NULL, "Not a persisted Codeblock!", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( HB_NOMOUSE )
{
}

HB_FUNC( HB_NOSTARTUPWINDOW )
{
}

HB_FUNC( HB_RESETWITH )
{
   HB_THREAD_STUB

   if( hb_pcount() >= 1 )
   {
      hb_itemForwardValue( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ), hb_stackItemFromBase( 1 ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1607, NULL, "HB_RESETWITH", 0, NULL );
   }
}

HB_FUNC( HB_WITHOBJECTCOUNTER )
{
   HB_THREAD_STUB

   hb_retnl( HB_VM_STACK.wWithObjectCounter ) ;
}

HB_FUNC( __TEXTINTO )
{
   HB_THREAD_STUB

   hb_itemCopy( hb_stackSelfItem(), &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ) );
   hb_symEval.value.pFunPtr();
}

HB_FUNC( __VMVARSLIST )
{
   HB_FUNCNAME(HB_DBG_VMVARSLIST)();
}

HB_FUNC( __VMVARSLEN )
{
   HB_FUNCNAME(HB_DBG_VMVARSLEN)();
}

HB_FUNC( __VMVARSGET )
{
   HB_FUNCNAME(HB_DBG_VMVARSGET)();
}

HB_FUNC( __VMVARSSET )
{
   HB_FUNCNAME(HB_DBG_VMVARSSET)();
}

HB_FUNC( HB_MULTITHREAD )
{
   HB_THREAD_STUB_API
#if defined(HB_THREAD_SUPPORT)
   hb_retl( TRUE );
#else
   hb_retl( FALSE );
#endif
}

HB_FUNC( HB_VMMODE )
{
   HB_THREAD_STUB_API
#if defined(HB_NO_PROFILER) && defined(HB_NO_TRACE) && !defined(HB_GUI)
   // optimized for console applications
   hb_retni( 2 );
#elif defined(HB_NO_PROFILER) && defined(HB_NO_TRACE) && defined(HB_GUI)
   // optimized for gui applications
   hb_retni( 1 );
#else
   // no optimization
   hb_retni( 0 );
#endif
}

#undef HB_FORCE_LINK_MAIN

#if defined(HB_OS_WIN_32) && !defined(__EXPORT__) && \
    ( defined(__DMC__) || defined(__WATCOMC__) || defined(__MINGW32__) )

#  define HB_FORCE_LINK_MAIN  hb_forceLinkMainWin

#elif defined(HB_OS_LINUX) && defined(__WATCOMC__)

#  define HB_FORCE_LINK_MAIN  hb_forceLinkMainStd

#endif

#ifdef HB_FORCE_LINK_MAIN
HB_EXTERN_BEGIN
extern void HB_EXPORT HB_FORCE_LINK_MAIN( void );
HB_EXTERN_END
void _hb_forceLinkMain()
{
   HB_FORCE_LINK_MAIN();
}
#endif


#define HB_XVM_RETURN   return ( HB_VM_STACK.uiActionRequest ? hb_xvmActionRequest() : FALSE );

static BOOL hb_xvmActionRequest( void )
{
   HB_THREAD_STUB_STACK

   if( HB_VM_STACK.uiActionRequest & ( HB_ENDPROC_REQUESTED | HB_BREAK_REQUESTED ) )
   {
      return TRUE;
   }
   else if( HB_VM_STACK.uiActionRequest & HB_QUIT_REQUESTED )
   {
      exit( hb_vmQuit() );
   }

   return FALSE;
}

HB_EXPORT void hb_xvmExitProc( ULONG ulPrivateBase )
{
   HB_THREAD_STUB_STACK

   if( HB_VM_STACK.uiActionRequest & HB_ENDPROC_REQUESTED )
      HB_VM_STACK.uiActionRequest = 0;

   hb_memvarSetPrivatesBase( ulPrivateBase );
}

HB_EXPORT void hb_xvmSeqBegin( void )
{
   HB_THREAD_STUB_STACK
   PHB_SEQUENCE pSequence;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSeqBegin()"));

   pSequence = (PHB_SEQUENCE) hb_xgrab( sizeof( HB_SEQUENCE ) );

   /* 1) clear the storage for value returned by BREAK statement */
   hb_stackTopItem()->type = HB_IT_NIL;
   hb_stackPush();

   /* 2) the address of RECOVER or END opcode - not used in C code */
   pSequence->lRecover = 0;

   /* 3) store current RECOVER base */
   pSequence->lBase = hb_stackTopOffset();

   /* 4) current bCanRecover flag - not used in C code */
   pSequence->uiStatus = 0;

   /* finally block address - not used in C code */
   pSequence->lFinally = 0;

   pSequence->wEnumCollectionCounter = HB_VM_STACK.wEnumCollectionCounter;
   pSequence->wWithObjectCounter     = HB_VM_STACK.wWithObjectCounter;

   pSequence->pPrevErrBlock = NULL;
   pSequence->pPrev = HB_VM_STACK.pSequence;
   HB_VM_STACK.pSequence = pSequence;
}

HB_EXPORT BOOL hb_xvmSeqEnd( void )
{
   HB_THREAD_STUB_STACK
   PHB_SEQUENCE pFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSeqEnd()"));

   pFree = HB_VM_STACK.pSequence;

   /* remove all items placed on the stack after BEGIN code */
   hb_stackRemove( HB_VM_STACK.pSequence->lBase );

   /* Discard the value returned by BREAK statement */
   hb_stackPop();

   HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;
   hb_xfree( (void *) pFree );

   if( HB_VM_STACK.uiActionRequest & HB_ENDPROC_REQUESTED )
      return TRUE;
   else if( HB_VM_STACK.uiActionRequest & HB_BREAK_REQUESTED )
      HB_VM_STACK.uiActionRequest = 0;
   else if( HB_VM_STACK.uiActionRequest & HB_QUIT_REQUESTED )
      exit( hb_vmQuit() );
   return FALSE;
}

HB_EXPORT BOOL hb_xvmSeqRecover( void )
{
   HB_THREAD_STUB_STACK
   PHB_SEQUENCE pFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSeqRecover()"));

   pFree = HB_VM_STACK.pSequence;

   /* remove all items placed on the stack after BEGIN code */
   hb_stackRemove( HB_VM_STACK.pSequence->lBase );

   /*
    * Leave the value returned from BREAK
    * it will be popped in next executed opcode
    */

   HB_VM_STACK.pSequence = HB_VM_STACK.pSequence->pPrev;
   hb_xfree( pFree );

   if( HB_VM_STACK.uiActionRequest & HB_ENDPROC_REQUESTED )
      return TRUE;
   else if( HB_VM_STACK.uiActionRequest & HB_BREAK_REQUESTED )
      HB_VM_STACK.uiActionRequest = 0;
   else if( HB_VM_STACK.uiActionRequest & HB_QUIT_REQUESTED )
      exit( hb_vmQuit() );
   return FALSE;
}

HB_EXPORT void hb_xvmTryBegin( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmTryBegin()"));

   hb_vm_iTry++;
   hb_xvmSeqBegin();
   HB_VM_STACK.pSequence->pPrevErrBlock = hb_errorBlock( hb_vm_BreakBlock );
}

HB_EXPORT BOOL hb_xvmTryEnd( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmTryEnd()"));

   hb_vm_iTry--;
   hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
   hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );
   return hb_xvmSeqEnd();
}

HB_EXPORT void hb_xvmTryEndFin( void )
{
   HB_THREAD_STUB_STACK
   USHORT uiActionRequest;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmTryEndFin()"));

   /* store requested action */
   uiActionRequest = HB_VM_STACK.uiActionRequest;

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
   HB_VM_STACK.uiActionRequest = uiActionRequest;
}

HB_EXPORT BOOL hb_xvmTryRecover( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmTryRecover()"));

   hb_vm_iTry--;
   hb_itemRelease( hb_errorBlock( HB_VM_STACK.pSequence->pPrevErrBlock ) );
   hb_itemRelease( HB_VM_STACK.pSequence->pPrevErrBlock );

   return hb_xvmSeqRecover();
}

HB_EXPORT USHORT hb_xvmBeginFinally( void )
{
   HB_THREAD_STUB_STACK

   USHORT uiActionRequest = HB_VM_STACK.uiActionRequest;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmBeginFinally()"));

   HB_VM_STACK.uiActionRequest = 0;

   return uiActionRequest;
}

HB_EXPORT BOOL hb_xvmEndFinally( USHORT uiActionRequest )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEndFinally(%hu)", uiActionRequest));

   if( uiActionRequest )
   {
      /* TODO: we should decide here about priority of different exceptions */
      HB_VM_STACK.uiActionRequest = uiActionRequest;
   }

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmSetLine( USHORT uiLine )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSetLine(%hu)", uiLine));

   hb_stackBaseItem()->item.asSymbol.lineno = uiLine;
   if( s_bDebugging && s_bDebugShowLines )
      hb_vmDebuggerShowLine( uiLine );
}

HB_EXPORT void hb_xvmBaseLine( USHORT uiLine )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmBaseLine(%hu)", uiLine));

   s_iBaseLine = hb_stackBaseItem()->item.asSymbol.lineno = uiLine;
   if( s_bDebugging && s_bDebugShowLines )
      hb_vmDebuggerShowLine( uiLine );
}

HB_EXPORT void hb_xvmLineOffset( BYTE bLine )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLineOffset(%d)", bLine));

   hb_stackBaseItem()->item.asSymbol.lineno = s_iBaseLine + bLine;

   if( s_bDebugging && s_bDebugShowLines )
      hb_vmDebuggerShowLine( s_iBaseLine + bLine );
}

HB_EXPORT BOOL hb_xvmClassSetModule( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pClassHandle;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmClassSetModule()"));

   pClassHandle = hb_stackItemFromTop( -1 );

   if( HB_IS_INTEGER( pClassHandle ) )
   {
      hb_clsSetModule( (USHORT) ( pClassHandle->item.asInteger.value ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1603, NULL, "__ClsSetModule()", 1, pClassHandle );
   }

   hb_stackPop(); /* pClassHandle */

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmIVarRef( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pSelf, pMsg;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmIVarRef()"));

   pSelf = hb_stackItemFromTop( -1 );
   pMsg = hb_stackItemFromTop( -2 );

   if( HB_IS_OBJECT( pSelf ) )
   {
      BOOL bConstructor;
      BOOL bSymbol;
      PHB_FUNC pFunc;

      pFunc = hb_objGetMthd( pSelf, pMsg->item.asSymbol.value, FALSE, &bConstructor, 2, &bSymbol );

      if( pFunc == hb___msgGetData )
      {
         if( (HB_VM_STACK.pMethod)->uiData > ( USHORT ) pSelf->item.asArray.value->ulLen ) /* Resize needed ? */
         {
            hb_arraySize( pSelf, (HB_VM_STACK.pMethod)->uiData ); /* Make large enough */
         }
         hb_arrayGetByRef( pSelf, (HB_VM_STACK.pMethod)->uiData, pMsg );
      }
      else if( pFunc == hb___msgGetClsData )
      {
         hb_arrayGetByRef( hb_clsClassesArray()[ pSelf->item.asArray.value->uiClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiData, pMsg );
      }
      else if( pFunc == hb___msgGetShrData )
      {
         if( (HB_VM_STACK.pMethod)->uiSprClass )
         {
            hb_arrayGetByRef( hb_clsClassesArray()[ (HB_VM_STACK.pMethod)->uiSprClass - 1 ].pClassDatas, (HB_VM_STACK.pMethod)->uiDataShared, pMsg );
         }
      }
      else if( HB_VM_STACK.uiActionRequest != HB_BREAK_REQUESTED )
      {
         hb_vmSend( 0 );
         hb_itemPushForward( &(HB_VM_STACK.Return) );

         HB_XVM_RETURN
      }
   }
   else if ( HB_IS_HASH( pSelf ) )
   {
      char * szIndex = pMsg->item.asSymbol.value->szName;
      ULONG ulPos;

      if( strcmp( szIndex, "CLASSNAME" ) == 0 )
      {
         hb_itemPutC( pMsg, "HASH" );
      }
      else if( strcmp( szIndex, "CLASSH" ) == 0 )
      {
         hb_itemPutNI( pMsg, 0 );
      }
      else if( strcmp( szIndex, "KEYS" ) == 0 )
      {
         hb_hashGetKeys( pMsg, pSelf );
      }
      else if( strcmp( szIndex, "VALUES" ) == 0 )
      {
         hb_hashGetValues( pMsg, pSelf );
      }
      else
      {
         HB_ITEM_NEW( hbIndex );
         hb_itemPutCRawStatic( &hbIndex, szIndex, strlen( szIndex ) );

         if( hb_hashScan( pSelf, &hbIndex , &ulPos ) )
         {
            hb_hashGet( pSelf, ulPos, pMsg );
         }
         else
         {
            hb_vmClassError( 0, "HASH", szIndex, pSelf );
         }
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_NOOBJECT, 1004, "Not object", pMsg->item.asSymbol.value->szName, 1, pSelf );
      hb_itemForwardValue( pMsg, &(HB_VM_STACK.Return) );
   }

   hb_stackPop();

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmFrame( int iLocals, int iParams )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmFrame(%d, %d)", iLocals, iParams));

   hb_vmFrame( ( BYTE ) iLocals, ( BYTE ) iParams );
}

HB_EXPORT void hb_xvmSFrame( PHB_SYMB pSymbol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSFrame(%p)", pSymbol));

   hb_vmSFrame( pSymbol );
}

HB_EXPORT BOOL hb_xvmDo( USHORT uiParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDo(%hu)", uiParams));

   hb_vmDo( uiParams );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmFunction( USHORT uiParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmFunction(%hu)", uiParams));

   if( HB_IS_COMPLEX( hb_stackReturnItem() ) )
      hb_itemClear( hb_stackReturnItem() );
   else
      hb_stackReturnItem()->type = HB_IT_NIL;

   hb_vmDo( uiParams );

   hb_itemForwardValue( hb_stackTopItem(), hb_stackReturnItem() );
   hb_stackPush();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmSend( USHORT uiParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSend(%hu)", uiParams));

   if( HB_IS_COMPLEX( hb_stackReturnItem() ) )
      hb_itemClear( hb_stackReturnItem() );
   else
      hb_stackReturnItem()->type = HB_IT_NIL;

   hb_vmSend( uiParams );

   hb_itemForwardValue( hb_stackTopItem(), hb_stackReturnItem() );
   hb_stackPush();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmSendWith( USHORT uiParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSendWith(%hu)", uiParams));

   HB_VM_STACK.bWithObject = TRUE;

   if( HB_IS_COMPLEX( hb_stackReturnItem() ) )
   {
      hb_itemClear( hb_stackReturnItem() );
   }
   else
   {
      hb_stackReturnItem()->type = HB_IT_NIL;
   }

   hb_vmSend( uiParams );
   hb_itemForwardValue( hb_stackTopItem(), hb_stackReturnItem() );
   hb_stackPush();

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmRetValue( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmRetValue()"));

   hb_stackDec();                                      /* make the last item visible */
   hb_itemForwardValue( hb_stackReturnItem(), hb_stackTopItem() ); /* copy it */
}

HB_EXPORT void hb_xvmStatics( PHB_SYMB pSymbol, USHORT uiStatics )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmStatics(%p,%hu)", pSymbol, uiStatics));

   hb_vmStatics( pSymbol, uiStatics );
}

HB_EXPORT void hb_xvmParameter( PHB_SYMB pSymbol, int iParams )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmParameter(%p,%d)", pSymbol, iParams));

   hb_memvarNewParameter( pSymbol, hb_stackItemFromBase( iParams ) );
}

HB_EXPORT void hb_xvmPushLocal( SHORT iLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushLocal(%hd)", iLocal));

   hb_vmPushLocal( iLocal );
}

HB_EXPORT void hb_xvmPushLocalByRef( SHORT iLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushLocalByRef(%hd)", iLocal));

   hb_vmPushLocalByRef( iLocal );
}

HB_EXPORT void hb_xvmPopLocal( SHORT iLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopLocal(%hd)", iLocal));

   hb_vmPopLocal( iLocal );
}

HB_EXPORT void hb_xvmPushStatic( USHORT uiStatic )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushStatic(%hu)", uiStatic));

   hb_vmPushStatic( uiStatic );
}

HB_EXPORT void hb_xvmPushStaticByRef( USHORT uiStatic )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushStaticByRef(%hu)", uiStatic));

   hb_vmPushStaticByRef( uiStatic );
}

HB_EXPORT void hb_xvmPopStatic( USHORT uiStatic )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopStatic(%hu)", uiStatic));

   hb_vmPopStatic( uiStatic );
}

HB_EXPORT void hb_xvmPushGlobal( USHORT uiGlobal, PHB_ITEM** pGlobals )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushGlobal(%hu)", uiGlobal));

   hb_vmPush( (*pGlobals)[ uiGlobal ] );
}

HB_EXPORT void hb_xvmPushGlobalByRef( USHORT uiGlobal, PHB_ITEM** pGlobals )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pTop = ( * HB_VM_STACK.pPos );

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushGlobalByRef(%hu)", uiGlobal));

   pTop->type = HB_IT_BYREF;

   pTop->item.asRefer.value = uiGlobal + 1; // To offset the -1 below.
   pTop->item.asRefer.offset = -1; // Because 0 will be translated as a STATIC in hb_itemUnref();
   pTop->item.asRefer.BasePtr.itemsbasePtr = pGlobals;

   #ifdef HB_UNSHARE_REFERENCES
     hb_itemUnShare( (*pGlobals)[ uiGlobal ] );
   #endif

   hb_stackPush();
}

HB_EXPORT void hb_xvmPopGlobal( USHORT uiGlobal, PHB_ITEM** pGlobals )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pTop = hb_stackItemFromTop( -1 );

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopGlobal(%hu)", uiGlobal));

   if( ( HB_IS_NUMBER( (*pGlobals)[ uiGlobal ] ) && HB_IS_NUMBER( *( HB_VM_STACK.pPos - 1 ) ) ) ||
       ( (*pGlobals)[ uiGlobal ] )->type == pTop->type )
   {
      hb_itemForwardValue( (*pGlobals)[ uiGlobal ], pTop );
   }
   else if( hb_objGetOpOver( (*pGlobals)[ uiGlobal ] ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmOperatorCall( (*pGlobals)[ uiGlobal ], pTop, "__OPASSIGN", NULL, 0, (*pGlobals)[ uiGlobal ] );
      hb_itemClear( pTop );
   }
   else
   {
     hb_itemForwardValue( (*pGlobals)[ uiGlobal ], pTop );
   }

   hb_stackDec();
}

HB_EXPORT BOOL hb_xvmPushVariable( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPushVariable(%p)", pSymbol));

   hb_vmPushVariable( pSymbol );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPopVariable( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPopVariable(%p)", pSymbol));

   hb_memvarSetValue( pSymbol, hb_stackItemFromTop(-1) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmPushBlockShort( const BYTE * pCode, USHORT usSize, PHB_SYMB pSymbols, PHB_ITEM** pGlobals )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushBlockShort(%p, %p)", pCode, pSymbols));

   hb_vmPushBlockShort( pCode, usSize, FALSE, pSymbols, pGlobals );
}

HB_EXPORT void hb_xvmPushBlock( const BYTE * pCode, USHORT usSize, PHB_SYMB pSymbols, PHB_ITEM** pGlobals )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushBlock(%p, %p)", pCode, pSymbols));

   hb_vmPushBlock( pCode, usSize, FALSE, pSymbols, pGlobals );
}

HB_EXPORT void hb_xvmPushSelf( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushSelf()"));

   hb_vmPush( hb_stackSelfItem() );
}

HB_EXPORT BOOL hb_xvmPopLogical( BOOL * pfValue )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopLogical(%p)", pfValue));

   *pfValue = hb_vmPopLogical();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPopAlias( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopAlias()"));

   hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ), NULL ); /* it clears the passed item */
   hb_stackDec();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmSwapAlias( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSwapAlias()"));

   hb_vmSwapAlias();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPushField( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPushField(%p)", pSymbol));

   hb_stackPush();
   hb_rddGetFieldValue( hb_stackItemFromTop( -1 ), pSymbol );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPushAlias( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushAlias()"));

   hb_vmPushAlias();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPushAliasedField( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPushAliasedField(%p)", pSymbol));

   hb_vmPushAliasedField( pSymbol );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPushAliasedVar( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPushAliasedVar(%p)", pSymbol));

   hb_vmPushAliasedVar( pSymbol );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPopField( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPopField(%p)", pSymbol));

   hb_rddPutFieldValue( hb_stackItemFromTop(-1), pSymbol );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPushMemvar( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPushMemvar(%p)", pSymbol));

   hb_memvarGetValue( hb_stackTopItem(), pSymbol );
   hb_stackPush();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPushMemvarByRef( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPushMemvarByRef(%p)", pSymbol));

   hb_memvarGetRefer( hb_stackTopItem(), pSymbol );
   hb_stackPush();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPopMemvar( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPopMemvar(%p)", pSymbol));

   hb_memvarSetValue( pSymbol, hb_stackItemFromTop(-1) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPopAliasedField( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPopAliasedField(%p)", pSymbol));

   hb_vmPopAliasedField( pSymbol );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPopAliasedVar( PHB_SYMB pSymbol )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_INFO, ("hb_xvmPopAliasedVar(%p)", pSymbol));

   hb_vmPopAliasedVar( pSymbol );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmLocalAddInt( int iLocal, LONG lAdd )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalAddInt(%d,%ld)", iLocal, lAdd));

   hb_vmAddInt( hb_stackItemFromBase( iLocal ), lAdd );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmLocalAdd( int iLocal )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pLocal;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalAdd(%d)", iLocal));

   pLocal = hb_stackItemFromBase( iLocal );

   if( HB_IS_BYREF( pLocal ) )
   {
      pLocal = hb_itemUnRef( pLocal );
   }
   hb_vmPlus( pLocal, hb_stackItemFromTop( -1 ), pLocal );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmEqual( BOOL fExact )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEqual(%d)", (int) fExact));

   hb_vmEqual( fExact );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmAnd( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmAnd()"));

   hb_vmAnd();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmOr( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmOr()"));

   hb_vmOr();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmNot( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmNot()"));

   hb_vmNot();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmNegate( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmNegate()"));

   hb_vmNegate();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPower( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPower()"));

   hb_vmPower();

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmDuplicate( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDuplicate()"));

   hb_vmDuplicate();
}

HB_EXPORT void hb_xvmDuplTwo( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDuplTwo()"));

   hb_vmDuplTwo();
}

HB_EXPORT BOOL hb_xvmForTest( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmForTest()"));

   hb_vmForTest();

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmFuncPtr( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmFuncPtr()"));

   hb_vmFuncPtr();
}

HB_EXPORT BOOL hb_xvmNotEqual( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEqual()"));

   hb_vmNotEqual();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmLess( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLess()"));

   hb_vmLess();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmLessEqual( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLessEqual()"));

   hb_vmLessEqual();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmGreater( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmGreater()"));

   hb_vmGreater();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmGreaterEqual( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmGreaterEqual()"));

   hb_vmGreaterEqual();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmInstring( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmInstring()"));

   hb_vmInstringOrArray();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmAddInt( LONG lAdd )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalAddInt(%ld)", lAdd));

   hb_vmAddInt( hb_stackItemFromTop( -1 ), lAdd );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPlus( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPlus()"));

   hb_vmPlus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ),
              hb_stackItemFromTop( -2 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMinus( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMinus()"));

   hb_vmMinus();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMultByInt( LONG lValue )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMultByInt(%ld)", lValue));

   pValue = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pValue ) )
   {
      int iDec, iType = pValue->type;
      double dValue = hb_itemGetNDDec( pValue, &iDec );

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

HB_EXPORT BOOL hb_xvmMult( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMult()"));

   hb_vmMult();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmDivideByInt( LONG lDivisor )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDivideByInt(%ld)", lDivisor));

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
         hb_itemPutNDDec( pValue, hb_itemGetND( pValue ) / lDivisor, hb_set.HB_SET_DECIMALS );
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

HB_EXPORT BOOL hb_xvmDivide( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDivide()"));

   hb_vmDivide();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmModulus( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmModulus()"));

   hb_vmModulus();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmInc( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmInc()"));

   hb_vmInc();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmDec( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDec()"));

   hb_vmDec();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmBitAnd( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmBitAnd()"));

   hb_vmBitAnd();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmBitOr( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmBitOr()"));

   hb_vmBitOr();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmBitXor( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmBitXor()"));

   hb_vmBitXor();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmBitShiftL( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmBitShiftL()"));

   hb_vmBitShiftLeft();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmBitShiftR( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmBitShiftR()"));

   hb_vmBitShiftRight();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmLeft( USHORT usLeft )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pString;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLeft(%hu)", usLeft));

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

HB_EXPORT BOOL hb_xvmRight( USHORT usRight )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pString;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLeft(%hu)", usRight));

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

HB_EXPORT BOOL hb_xvmLike( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pResult;
   BOOL bLike;

   HB_TRACE( HB_TR_DEBUG, ("hb_xvmLike()") );

   pResult = hb_stackItemFromTop( -2 );
   bLike = hb_regex( 1, hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );
   hb_stackPop();
   hb_itemClear( pResult );
   pResult->type = HB_IT_LOGICAL;
   pResult->item.asLogical.value = bLike;

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMatch( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pResult;
   BOOL bMatch;

   HB_TRACE( HB_TR_DEBUG, ("hb_xvmMatch()") );

   pResult = hb_stackItemFromTop( -2 );
   bMatch = hb_regex( 2, hb_stackItemFromTop( -1 ), hb_stackItemFromTop( -2 ) );
   hb_stackPop();
   hb_itemClear( pResult );
   pResult->type = HB_IT_LOGICAL;
   pResult->item.asLogical.value = bMatch;

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmArrayDim( USHORT uiDimensions )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayDim(%hu)", uiDimensions));

   hb_vmArrayDim( uiDimensions );
}

HB_EXPORT void hb_xvmArrayGen( ULONG ulElements )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayGen(%lu)", ulElements));

   hb_vmArrayGen( ulElements );
   HB_VM_STACK.iExtraElements = 0;
}

static void hb_vmArrayItemPush( ULONG ulIndex )
{
   HB_THREAD_STUB_STACK

   PHB_ITEM pArray;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayItemPush(%lu)", ulIndex));

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
            PHB_ITEM pItem = hb_stackTopItem();

            hb_stackPush();
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
      // Associative Array compatibility
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
         // Hash compatibility
         if( ! hb_hashScan(pArray, hb_stackItemFromTop( -1 ), &ulIndex ) )
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

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayItemPop(%lu", ulIndex));

   pValue = hb_stackItemFromTop( -2 );
   pArray = hb_stackItemFromTop( -1 );

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
         pValue->type &= ~HB_IT_MEMOFLAG;
         hb_itemForwardValue( pArray->item.asArray.value->pItems + ulIndex - 1, pValue );
         hb_stackPop();
         hb_stackDec();    /* value was moved above hb_stackDec() is enough */
      }
      else
      {
         hb_vmPushNumInt( ulIndex );
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, hb_stackItemFromTop( -1 ) );
      }
   }
#ifndef HB_C52_STRICT
   // Only allowing assignment of strings (char) and numerics into String as Array.
   else if( HB_IS_STRING( pArray ) && ( HB_IS_STRING( pValue ) || HB_IS_NUMERIC( pValue ) ) )
   {
      if( ulIndex > 0 )
      {
         ulIndex--;
      }
      if( ulIndex < pArray->item.asString.length )
      {
         BYTE bNewChar;

         //pArray = pArray->item.asString.pOrigin;
         if( pValue->type & HB_IT_STRING )
         {
            bNewChar = pValue->item.asString.value[0];
         }
         else if( pValue->type == HB_IT_INTEGER )
         {
            bNewChar = (BYTE) pValue->item.asInteger.value;
         }
         else if( pValue->type == HB_IT_LONG )
         {
            bNewChar = (BYTE) pValue->item.asLong.value;
         }
         else
         {
            bNewChar = (BYTE) pValue->item.asDouble.value;
         }

         if( pArray->item.asString.length == 1 )
         {
            hb_itemPutCLStatic( pArray, hb_szAscii[ ( UCHAR ) bNewChar ], 1 );
         }
         else if( pArray->item.asString.allocated == 0 || *( pArray->item.asString.pulHolders ) > 1 )
         {
            hb_itemUnShare( pArray );
         }

         pArray->item.asString.value[ ulIndex ] = bNewChar;
         hb_stackPop();
         hb_stackPop();
      }
      else
      {
         hb_vmPushNumInt( ulIndex );
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, hb_stackItemFromTop( -1 ), pValue );
      }
   }
#endif
   else
   {
      hb_vmPushNumInt( ulIndex );
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, hb_stackItemFromTop( -1 ), pValue );
   }
}


HB_EXPORT BOOL hb_xvmArrayPush( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayPush()"));

   hb_vmArrayPush();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmArrayPushRef( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayPushRef()"));

   hb_vmArrayPushRef();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmArrayItemPush( ULONG ulIndex )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayItemPush(%lu)", ulIndex));

   hb_vmArrayItemPush( ulIndex );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmArrayPop( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayPop()"));

   hb_vmArrayPop( HB_P_NOOP );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmArrayPopPlus( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayPopPlus()"));

   hb_vmArrayPop( HB_P_PLUS );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmArrayItemPop( ULONG ulIndex )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayItemPop(%lu)", ulIndex));

   hb_vmArrayItemPop( ulIndex );

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmPushDouble( double dNumber, int iWidth, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushDouble(%lf, %d, %d)", dNumber, iWidth, iDec));

   hb_vmPushDoubleConst( dNumber, iWidth, iDec );
}

#ifdef HB_LONG_LONG_OFF
HB_EXPORT void hb_xvmPushLongLong( double dNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushLongLong(%l.0f)", dNumber));

   hb_vmPushDoubleConst( dNumber, HB_DEFAULT_WIDTH, 0 );
}
#else
HB_EXPORT void hb_xvmPushLongLong( LONGLONG llNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushLongLong(%" PFLL "i)", llNumber));

   hb_vmPushLongLongConst( llNumber );
}
#endif

HB_EXPORT void hb_xvmLocalName( USHORT uiLocal, char * szLocalName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalName(%hu, %s)", uiLocal, szLocalName));

   hb_vmLocalName( uiLocal, szLocalName );
}

HB_EXPORT void hb_xvmStaticName( BYTE bIsGlobal, USHORT uiStatic, char * szStaticName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmStaticName(%d, %hu, %s)", (int)bIsGlobal, uiStatic, szStaticName));

   HB_SYMBOL_UNUSED( bIsGlobal );
   hb_vmStaticName( uiStatic, szStaticName );
}

HB_EXPORT void hb_xvmModuleName( char * szModuleName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmModuleName(%s)", szModuleName));

   hb_vmModuleName( szModuleName );
}

HB_EXPORT void hb_xvmMacroList( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroList()"));

   HB_VM_STACK.aiExtraElements[ HB_VM_STACK.iExtraElementsIndex++ ] = 0;
}

HB_EXPORT void hb_xvmMacroListEnd( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroListEnd()"));

   HB_VM_STACK.iExtraElements = HB_VM_STACK.aiExtraElements[ --HB_VM_STACK.iExtraElementsIndex ];
}

HB_EXPORT BOOL hb_xvmMacroPush( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPush(%d)", bFlags));

   hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, bFlags );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmPushMacroRef( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pTop;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushMacroRef()"));

   pTop = hb_stackItemFromTop( -1 );
   hb_memvarGetRefer( pTop, pTop->item.asSymbol.value );

   HB_XVM_RETURN
}


HB_EXPORT BOOL hb_xvmMacroPushIndex( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushIndex(%d)", bFlags));

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHINDEX, bFlags );

   if( HB_VM_STACK.iExtraIndex )
   {
      HB_ITEM *aExtraItems = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) * HB_VM_STACK.iExtraIndex );
      int i;

      /* Storing and removing the extra indexes. */
      for ( i = HB_VM_STACK.iExtraIndex - 1; i >= 0; i-- )
      {
         ( aExtraItems + i )->type = HB_IT_NIL;
         hb_itemMove( aExtraItems + i, hb_stackItemFromTop(-1) );
         hb_stackDec();
      }

      /* First index is still on stack.*/
      hb_vmArrayPush();

      /* Now process each of the additional index.
       * Do not process the last one which will be processes by the
       * HB_P_ARRAYPUSH which is know to follow
       */
      for ( i = 0; i < HB_VM_STACK.iExtraIndex; i++ )
      {
         hb_vmPush( aExtraItems + i );

         if( HB_IS_COMPLEX( aExtraItems + i ) )
         {
            hb_itemClear( aExtraItems + i );
         }

         if( i < HB_VM_STACK.iExtraIndex - 1 )
         {
            hb_vmArrayPush();
         }
      }

      hb_xfree( aExtraItems );
   }

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMacroPushArg( PHB_SYMB pSymbol, BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushArg(%p, %d)", pSymbol, bFlags));

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHARG, bFlags );

   if( HB_VM_STACK.iExtraParamsIndex && HB_VM_STACK.apExtraParamsSymbol[ HB_VM_STACK.iExtraParamsIndex - 1 ] == NULL )
   {
      HB_VM_STACK.apExtraParamsSymbol[ HB_VM_STACK.iExtraParamsIndex - 1 ] = pSymbol;
   }

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMacroPushList( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushList(%d)", bFlags));

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHLIST, bFlags );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMacroPushPare( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushPare(%d)", bFlags));

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHPARE, bFlags );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMacroPushAliased( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushAliased(%d)", bFlags));

   hb_macroPushAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), bFlags );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMacroPop( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPop(%d)", bFlags));

   hb_macroSetValue( hb_stackItemFromTop( -1 ), bFlags );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMacroPopAliased( BYTE bFlags )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPopAliased(%d)", bFlags));

   hb_macroPopAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), bFlags );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMacroSymbol( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroSymbol()"));

   hb_macroPushSymbol( hb_stackItemFromTop( -1 ) );

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmMacroText( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroText()"));

   hb_macroTextValue( hb_stackItemFromTop( -1 ) );

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmLocalSetInt( USHORT usLocal, int iVal )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pLocal;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalSetInt(%hu, %d)", usLocal, iVal));

   pLocal = hb_stackItemFromBase( usLocal );

   if( HB_IS_BYREF( pLocal ) )
   {
      pLocal = hb_itemUnRef( pLocal );
   }
   if( ( ! HB_IS_NUMBER( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmPushInteger( iVal );
      hb_vmOperatorCall( pLocal, hb_stackItemFromTop( -1 ), "__OPASSIGN", NULL, 1, pLocal );
   }
   else
   {
      hb_itemPutNI( pLocal, iVal );
   }
}

HB_EXPORT void hb_xvmLocalSetStr( USHORT usLocal, const char * pVal, ULONG ulLen )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pLocal;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalSetInt(%hu,%p,%lu)", usLocal, pVal, ulLen));

   pLocal = hb_stackItemFromBase( usLocal );

   if( HB_IS_BYREF( pLocal ) )
   {
      pLocal = hb_itemUnRef( pLocal );
   }

   if( ( ! HB_IS_STRING( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
   {
      hb_itemPushStaticString( ( char * ) pVal, ulLen );
      hb_vmOperatorCall( pLocal, hb_stackItemFromTop( -1 ), "__OPASSIGN", NULL, 1, pLocal );
   }
   else
   {
      hb_itemPutCRawStatic( pLocal, ( char * ) pVal, ulLen );
   }
}

HB_EXPORT BOOL hb_xvmSwitchCase( LONG lCase )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pTop;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSwitchCase(%ld)", lCase));

   pTop = hb_stackItemFromTop( -1 );

   if( pTop->type & HB_IT_INTEGER )
   {
      hb_vmPushLogical( (LONG) ( pTop->item.asInteger.value ) == lCase );
   }
   else if( pTop->type & HB_IT_LONG )
   {
      hb_vmPushLogical( (LONG) pTop->item.asLong.value == lCase );
   }
   else if( pTop->type & HB_IT_STRING && pTop->item.asString.length == 1 )
   {
      hb_vmPushLogical( (LONG) ( ( BYTE ) pTop->item.asString.value[0] ) == lCase );
   }
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

HB_EXPORT void hb_xvmPushWith( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushWith()"));

   if( HB_VM_STACK.wWithObjectCounter )
   {
      hb_vmPush( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter - 1 ] ) );
   }
   else
   {
       hb_vmPushNil();
   }
}

HB_EXPORT BOOL hb_xvmWithObject( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmWithObject()"));

   hb_itemForwardValue( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter++ ] ), hb_stackItemFromTop( -1 ) );

   if( HB_VM_STACK.wWithObjectCounter == HB_MAX_WITH_OBJECTS )
   {
      hb_errRT_BASE( EG_ARG, 9002, NULL, "WITH OBJECT excessive nesting!", 0 );

      while( HB_VM_STACK.wWithObjectCounter )
      {
         --HB_VM_STACK.wWithObjectCounter;
         hb_itemClear( &( HB_VM_STACK.aWithObject[ HB_VM_STACK.wWithObjectCounter ] ) );
      }
   }
   hb_stackPop();

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmEndWithObject( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEndWithObject()"));

   if( HB_VM_STACK.wWithObjectCounter )
   {
      hb_itemClear( &( HB_VM_STACK.aWithObject[ --HB_VM_STACK.wWithObjectCounter ] ) );
   }

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmEnumIndex( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pTop;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEnumIndex()"));

   pTop = hb_stackTopItem();
   pTop->type = HB_IT_LONG;

   if( HB_VM_STACK.wEnumCollectionCounter )
   {
      pTop->item.asLong.value = HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter - 1 ];
   }
   else
   {
      pTop->item.asLong.value = 0;
   }

   pTop->item.asLong.length = 10;
   hb_stackPush();
}

HB_EXPORT BOOL hb_xvmForEach( void )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pEnumeration;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmForEach()"));

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
       // No prep needed.
   }
   else if( HB_IS_HASH( pEnumeration ) && hb_hashGetCompatibility( pEnumeration ) )
   {
       // No prep needed.
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1602, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pEnumeration, hb_itemPutNI( * HB_VM_STACK.pPos, 1 ) );
   }

   HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] = hb_itemUnRef( hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   HB_VM_STACK.wEnumCollectionCounter++;

   if( HB_VM_STACK.wEnumCollectionCounter == HB_MAX_ENUMERATIONS )
   {
      hb_errRT_BASE( EG_ARG, 9002, NULL, "FOR EACH excessive nesting!", 0 );

      // Release ALL FOR EACH.
      while( HB_VM_STACK.wEnumCollectionCounter )
      {
         HB_VM_STACK.wEnumCollectionCounter--;
         hb_itemClear( pEnumeration );
         HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter ] = 0;
      }
   }

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmEnumerate( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEnumerate()"));

   if( HB_VM_STACK.wEnumCollectionCounter )
   {
      PHB_ITEM pEnumeration = &( HB_VM_STACK.aEnumCollection[ HB_VM_STACK.wEnumCollectionCounter - 1 ] );
      PHB_ITEM pEnumerator  = HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter - 1 ];
      ULONG ulEnumIndex     = ++HB_VM_STACK.awEnumIndex[ HB_VM_STACK.wEnumCollectionCounter - 1 ];

      if( hb_objGetOpOver( pEnumeration ) & HB_CLASS_OP_FOREACH )
      {
         HB_ITEM_NEW( ForEachOp );
         HB_ITEM_NEW( ForEachIndex );

         hb_itemPutNI( &ForEachOp, FOREACH_ENUMERATE );
         hb_itemPutNL( &ForEachIndex, ulEnumIndex );

         hb_vmOperatorCall( pEnumeration, &ForEachOp, "__OPFOREACH", &ForEachIndex, 0, pEnumerator );

         if( HB_VM_STACK.uiActionRequest == HB_BREAK_REQUESTED )
         {
            HB_VM_STACK.uiActionRequest = 0;
            hb_vmPushLogical( FALSE );
         }
         else
         {
            hb_vmPushLogical( TRUE );
         }
      }
      else if( HB_IS_ARRAY( pEnumeration ) || HB_IS_STRING( pEnumeration ) )
      {
         hb_vmPushLogical( hb_arrayGetByRef( pEnumeration, ulEnumIndex, pEnumerator ) );
      }
      else if( HB_IS_HASH( pEnumeration ) && hb_hashGetCompatibility( pEnumeration ) )
      {
         ulEnumIndex = hb_hashAAGetRealPos( pEnumeration, ulEnumIndex );

         if( ulEnumIndex )
         {
            hb_hashGet( pEnumeration, ulEnumIndex, pEnumerator );
            hb_vmPushLogical( TRUE );
         }
         else
         {
            hb_vmPushLogical( FALSE );
         }
      }
      else
      {
         hb_vmPushLogical( FALSE );
      }
   }
   else
   {
      hb_vmPushLogical( FALSE );
   }

   HB_XVM_RETURN
}

HB_EXPORT BOOL hb_xvmEndEnumerate( void )
{
   HB_THREAD_STUB_STACK

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEndEnumerate()"));

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

   // Incase EXIT was used.
   hb_itemClear( HB_VM_STACK.apEnumVar[ HB_VM_STACK.wEnumCollectionCounter ] );

   HB_XVM_RETURN
}

HB_EXPORT void hb_xvmLocalSetStringHidden( int iLocal, BYTE bType, ULONG ulSize, const char * pVal, ULONG ulBufferSize )
{
   HB_THREAD_STUB_STACK
   PHB_ITEM pLocal;
   BYTE *pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalSetStringHidden(%d,%d,%lu,%p,%lu)", iLocal, bType, ulSize, pVal, ulBufferSize));

   pLocal = hb_stackItemFromBase( iLocal );
   if( HB_IS_BYREF( pLocal ) )
   {
      pLocal = hb_itemUnRef( pLocal );
   }

   pBuffer = hb_vmUnhideString( bType, ulSize, ( const BYTE * ) pVal, ulBufferSize );

   if( ( ! HB_IS_STRING( pLocal ) ) && hb_objGetOpOver( pLocal ) & HB_CLASS_OP_ASSIGN )
   {
      hb_vmPushString( ( char * ) pBuffer, ulSize - 1 );
      hb_vmOperatorCall( pLocal, *( HB_VM_STACK.pPos - 1 ), "__OPASSIGN", NULL, 1, pLocal );
   }
   else
   {
      hb_itemPutCPtr( pLocal, ( char * ) pBuffer, ulSize - 1 );
   }
}

HB_EXPORT void hb_xvmPushStringHidden( BYTE bType, ULONG ulSize, const char * pVal, ULONG ulBufferSize )
{
   BYTE *pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalSetStringHidden(%d,%lu,%p,%lu)", bType, ulSize, pVal, ulBufferSize));

   pBuffer = hb_vmUnhideString( bType, ulSize, ( const BYTE * ) pVal, ulBufferSize );

   hb_vmPushString( ( char * ) pBuffer, ulSize - 1 );
}
