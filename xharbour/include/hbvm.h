/*
 * $Id: hbvm.h,v 1.51 2007/04/22 22:50:27 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Header file for the Virtual Machine API
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

#ifndef HB_VM_H_
#define HB_VM_H_

#include "hbapi.h"

#define HB_SEQ_PRESET_CANRECOVER  0x0001
#define HB_SEQ_PRESET_CANFINALIZE 0x0002

#define HB_SEQ_RECOVERED          0x0004
#define HB_SEQ_FINALIZED          0x0008

#define HB_SEQ_RETHROW            0x8000

typedef struct _HB_SEQUENCE
{
   USHORT   uiStatus;

   USHORT   uiActionRequest;

   ULONG    lBase;
   ULONG    lRecover;
   ULONG    lFinally;

   UINT     wEnumCollectionCounter;
   UINT     wWithObjectCounter;

   PHB_ITEM pPrevErrBlock;

   struct _HB_SEQUENCE *pPrev;
} HB_SEQUENCE, *PHB_SEQUENCE;


HB_EXTERN_BEGIN

/* Harbour virtual machine init/exit functions */
extern void HB_EXPORT hb_vmInit( BOOL bStartMainProc );
extern int  HB_EXPORT hb_vmQuit( void );            /* Immediately quits the virtual machine */

/* registration AtInit and AtExit functions - they are executed
 * just before (after) .prg INIT (EXIT) procedures.
 */
extern void HB_EXPORT hb_vmAtInit( HB_INIT_FUNC pFunc, void * cargo );
extern void HB_EXPORT hb_vmAtExit( HB_INIT_FUNC pFunc, void * cargo );

/* Harbour virtual machine functions */
extern HB_EXPORT void     hb_vmExplicitStartup( PHB_SYMB pSymbol );

extern void hb_vmSymbolInit_RT( void );   /* initialization of runtime support symbols */

/* Harbour virtual machine escaping API */
extern void    HB_EXPORT hb_vmRequestBreak( PHB_ITEM pItem );
extern void    HB_EXPORT hb_vmRequestCancel( void );
extern void    HB_EXPORT hb_vmRequestDebug( void );
extern void    HB_EXPORT hb_vmRequestEndProc( void );
extern USHORT  HB_EXPORT hb_vmRequestQuery( void );
extern void    HB_EXPORT hb_vmRequestQuit( void );
extern void    HB_EXPORT hb_vmRequestReset( void );
extern void    HB_EXPORT hb_vmRequest( USHORT );

/* Return values of hb_vmRequestQuery() */
#define HB_QUIT_REQUESTED       1   /* immediately quit the application */
#define HB_BREAK_REQUESTED      2   /* break to nearest RECOVER/END sequence */
#define HB_ENDPROC_REQUESTED    4   /* immediately return from procedure (error handler in macro evaluation) */

/* Public PCode functions */

/* Execution */
HB_EXPORT extern void     hb_vmDo( USHORT uiParams );      /* invoke the virtual machine */
HB_EXPORT extern void     hb_vmFunction( USHORT uiParams ); /* executes a function saving its result */
HB_EXPORT extern void     hb_vmSend( USHORT uiParams ); /* sends a message to an object */
HB_EXPORT extern PHB_ITEM hb_vmEvalBlock( PHB_ITEM pBlockItem ); /* executes passed codeblock with no arguments */
/* executes passed codeblock with variable number of arguments */
HB_EXPORT extern PHB_ITEM hb_vmEvalBlockV( PHB_ITEM pBlockItem, ULONG ulArgCount, ... );
HB_EXPORT extern PHB_ITEM hb_vmEvalBlockOrMacro( PHB_ITEM pItem ); /* executes codeblock or macro pointed by given item */
HB_EXPORT extern void     hb_vmDestroyBlockOrMacro( PHB_ITEM pItem ); /* destroy codeblock or macro in given item */

/* Push */
HB_EXPORT extern void    hb_vmPush( PHB_ITEM pItem );     /* pushes a generic item onto the stack */
HB_EXPORT extern void    hb_vmPushNil( void );            /* in this case it places nil at self */
HB_EXPORT extern void    hb_vmPushNumber( double dNumber, int iDec ); /* pushes a number on to the stack and decides if it is SHORT, LONG or double */
HB_EXPORT extern void    hb_vmPushInteger( int iNumber ); /* pushes a integer number onto the stack */
HB_EXPORT extern void    hb_vmPushLong( LONG lNumber ); /* pushes a LONG number onto the stack */
HB_EXPORT extern void    hb_vmPushLongLong( HB_LONG lNumber ); /* pushes a long long number onto the stack */
HB_EXPORT extern void    hb_vmPushDouble( double lNumber, int iDec ); /* pushes a double number onto the stack */
HB_EXPORT extern void    hb_vmPushLogical( BOOL bValue );    /* pushes a logical value onto the stack */
HB_EXPORT extern void    hb_vmPushString( const char * szText, ULONG length );  /* pushes a string on to the stack */
HB_EXPORT extern void    hb_vmPushDate( LONG lDate );   /* pushes a LONG date onto the stack */
HB_EXPORT extern void    hb_vmPushDateTime( LONG lDate, LONG lTime );   /* pushes a datetime onto the stack */
HB_EXPORT extern void    hb_vmPushSymbol( PHB_SYMB pSym ); /* pushes a function pointer onto the stack */
HB_EXPORT extern void    hb_vmPushDynSym( PHB_DYNS pDynSym ); /* pushes a function/method pointer onto the stack */
HB_EXPORT extern void    hb_vmPushEvalSym( void ); /* pushes a codeblock eval symbol onto the stack */
HB_EXPORT extern void    hb_vmPushPointer( void * ); /* push an item of HB_IT_POINTER type */
HB_EXPORT extern void    hb_vmPushBaseArray( PHB_BASEARRAY pBaseArray );
HB_EXPORT extern void    hb_vmPushItemRef( PHB_ITEM pItem, PHB_ITEM * pItemRef[], int iPos );
HB_EXPORT extern void    hb_vmPushState( void ); /* push current VM state on stack */
HB_EXPORT extern void    hb_vmPopState( void ); /* pop current VM state from stack */

HB_EXPORT extern PSYMBOLS hb_vmLastModule( void );
HB_EXPORT extern PSYMBOLS hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, char *szModule, int iPCodeVer, PHB_ITEM *pGlobals ); /* statics symbols initialization */
HB_EXPORT extern PSYMBOLS hb_vmProcessDllSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, char *szModule, int iPCodeVer );
HB_EXPORT extern PSYMBOLS * hb_vmSymbols( void );
HB_EXPORT extern void     hb_vmDoExitFunctions( void );
HB_EXPORT extern void     hb_vmPushNumType( double dNumber, int iDec, int iType1, int iType2 );

/* Debugger API */
typedef void (*HB_DBGENTRY_FUNC) ( int , int , char *, int , int );
HB_EXPORT extern HB_DBGENTRY_FUNC hb_vm_pFunDbgEntry;
HB_EXPORT extern BOOL hb_dbg_InvokeDebug( BOOL bInvoke );
HB_EXPORT extern ULONG hb_dbg_ProcLevel( void );
HB_EXPORT extern PHB_ITEM hb_dbg_vmVarSGet( int nStatic, int nOffset );
HB_EXPORT extern PHB_ITEM hb_dbg_vmVarLGet( int iLevel, int iLocal );
HB_EXPORT extern ULONG hb_dbg_vmVarGCount( void );
HB_EXPORT extern PHB_ITEM hb_dbg_vmVarGGet( int nGlobal, int nOffset );
HB_EXPORT extern void hb_dbgEntry( int nMode, int nLine, char *szName, int nIndex, int nFrame );
HB_EXPORT extern void hb_dbgAddBreak( void *handle, char *cModule, int nLine, char *szFunction );
HB_EXPORT extern void hb_dbgAddWatch( void *handle, char *szExpr, BOOL bTrace );
HB_EXPORT extern void hb_dbgDelBreak( void *handle, int nBreak );
HB_EXPORT extern void hb_dbgDelWatch( void *handle, int nWatch );
HB_EXPORT extern PHB_ITEM hb_dbgGetExpressionValue( void *handle, char *expression );
HB_EXPORT extern PHB_ITEM hb_dbgGetSourceFiles( void *handle );
HB_EXPORT extern PHB_ITEM hb_dbgGetWatchValue( void *handle, int nWatch );
HB_EXPORT extern BOOL hb_dbgIsValidStopLine( void *handle, char *szModule, int nLine );
HB_EXPORT extern void hb_dbgSetCBTrace( void *handle, BOOL bCBTrace );
HB_EXPORT extern void hb_dbgSetGo( void *handle );
HB_EXPORT extern void hb_dbgSetInvoke( void *handle, BOOL ( *pFunInvoke )( void ) );
HB_EXPORT extern void hb_dbgSetNextRoutine( void *handle );
HB_EXPORT extern void hb_dbgSetQuit( void *handle );
HB_EXPORT extern void hb_dbgSetToCursor( void *handle, char *szModule, int nLine );
HB_EXPORT extern void hb_dbgSetTrace( void *handle );
HB_EXPORT extern void hb_dbgSetWatch( void *handle, int nWatch, char *szExpr, BOOL bTrace );

#if ( defined(__DMC__) || defined(__POCC__) )
   #if !defined(HB_DLL_REQUIRED_DLLMAIN)
      #define HB_DLL_REQUIRED_DLLMAIN
   #endif
#endif

HB_EXTERN_END

#endif /* HB_VM_H_ */
