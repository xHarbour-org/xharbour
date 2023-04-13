/*
 * $Id$
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
extern HB_EXPORT void     hb_vmInit( BOOL bStartMainProc );
extern HB_EXPORT int      hb_vmQuit( void ); /* Immediately quits the virtual machine, return ERRORLEVEL code */

/* registration AtInit and AtExit functions - they are executed
 * just before (after) .prg INIT (EXIT) procedures.
 */
extern HB_EXPORT void     hb_vmAtInit( HB_INIT_FUNC pFunc, void * cargo );
extern HB_EXPORT void     hb_vmAtExit( HB_INIT_FUNC pFunc, void * cargo );

/* Harbour virtual machine functions */
extern HB_EXPORT void     hb_vmExplicitStartup( PHB_SYMB pSymbol );
extern HB_EXPORT PSYMBOLS hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char *szModule,  int iPCodeVer, PHB_ITEM *pGlobals ); /* statics symbols initialization */
extern HB_EXPORT PSYMBOLS hb_vmProcessSysDllSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char *szModule,  int iPCodeVer, PHB_ITEM *pGlobals );
extern HB_EXPORT PSYMBOLS hb_vmProcessPrgDllSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char *szModule,  int iPCodeVer, PHB_ITEM *pGlobals );
extern HB_EXPORT PSYMBOLS hb_vmProcessExeUsesDllSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char *szModule,  int iPCodeVer, PHB_ITEM *pGlobals );

#ifdef _HB_API_INTERNAL_
   extern BOOL        hb_vmFindModuleSymbols( PHB_SYMB pSym, PHB_SYMB * pSymbols, USHORT * puiSymbols );
   extern PHB_SYMB    hb_vmGetRealFuncSym( PHB_SYMB pSym );
#endif


extern HB_EXPORT void     hb_vmSymbolInit_RT( void );   /* initialization of runtime support symbols */

/* Harbour virtual machine escaping API */
extern HB_EXPORT void    hb_vmRequestDebug( void );
extern HB_EXPORT void    hb_vmRequestBreak( PHB_ITEM pItem );
extern HB_EXPORT void    hb_vmRequestCancel( void );
extern HB_EXPORT void    hb_vmRequestQuit( void );
extern HB_EXPORT void    hb_vmRequestEndProc( void );
extern HB_EXPORT USHORT  hb_vmRequestQuery( void );
extern HB_EXPORT BOOL    hb_vmRequestReenter( void );
extern HB_EXPORT void    hb_vmRequestRestore( void );
extern HB_EXPORT void    hb_vmRequestReset( void );
extern HB_EXPORT void    hb_vmRequest( USHORT );

/* Return values of hb_vmRequestQuery() */
#define HB_QUIT_REQUESTED       0x0001   /* immediately quit the application */
#define HB_BREAK_REQUESTED      0x0002   /* break to nearest RECOVER/END sequence */
#define HB_ENDPROC_REQUESTED    0x0004   /* immediately return from procedure (error handler in macro evaluation) */
#define HB_REQUEST_MASK ( HB_QUIT_REQUESTED | HB_BREAK_REQUESTED | HB_ENDPROC_REQUESTED )
#define HB_SUSPEND_QUIT         0x0100

/* Public PCode functions */

/* Execution */
extern HB_EXPORT void     hb_vmDo( USHORT uiParams );      /* invoke the virtual machine */
extern HB_EXPORT void     hb_vmFunction( USHORT uiParams ); /* executes a function saving its result */
extern HB_EXPORT void     hb_vmSend( USHORT uiParams ); /* sends a message to an object */
extern HB_EXPORT PHB_ITEM hb_vmEvalBlock( PHB_ITEM pBlockItem ); /* executes passed codeblock with no arguments */
/* executes passed codeblock with variable number of arguments */
extern HB_EXPORT PHB_ITEM hb_vmEvalBlockV( PHB_ITEM pBlockItem, HB_SIZE ulArgCount, ... );
extern HB_EXPORT PHB_ITEM hb_vmEvalBlockOrMacro( PHB_ITEM pItem ); /* executes codeblock or macro pointed by given item */
extern HB_EXPORT void     hb_vmDestroyBlockOrMacro( PHB_ITEM pItem ); /* destroy codeblock or macro in given item */

/* Push */
extern HB_EXPORT void     hb_vmPush( PHB_ITEM pItem );     /* pushes a generic item onto the stack */
extern HB_EXPORT void     hb_vmPushNil( void );            /* in this case it places nil at self */
extern HB_EXPORT void     hb_vmPushNumber( double dNumber, int iDec ); /* pushes a number on to the stack and decides if it is SHORT, LONG or double */
extern HB_EXPORT void     hb_vmPushInteger( int iNumber ); /* pushes a integer number onto the stack */
extern HB_EXPORT void     hb_vmPushLong( LONG lNumber ); /* pushes a LONG number onto the stack */
extern HB_EXPORT void     hb_vmPushLongLong( HB_LONG lNumber ); /* pushes a long long number onto the stack */
extern HB_EXPORT void     hb_vmPushDouble( double lNumber, int iDec ); /* pushes a double number onto the stack */
extern HB_EXPORT void     hb_vmPushNumInt( HB_LONG lNumber );  /* pushes a number on to the stack and decides if it is integer or HB_LONG */
extern HB_EXPORT void     hb_vmPushLogical( BOOL bValue );    /* pushes a logical value onto the stack */
extern HB_EXPORT void     hb_vmPushString( const char * szText, HB_SIZE length );  /* pushes a string on to the stack */
extern HB_EXPORT void     hb_vmPushDate( LONG lDate );  /* pushes a LONG date onto the stack */
extern HB_EXPORT void     hb_vmPushDateTime( LONG lDate, LONG lTime );   /* pushes a datetime onto the stack */
extern HB_EXPORT void     hb_vmPushSymbol( PHB_SYMB pSym ); /* pushes a function pointer onto the stack */
extern HB_EXPORT void     hb_vmPushDynSym( PHB_DYNS pDynSym ); /* pushes a function/method pointer onto the stack */
extern HB_EXPORT void     hb_vmPushEvalSym( void ); /* pushes a codeblock eval symbol onto the stack */
extern HB_EXPORT void     hb_vmPushPointer( void * ); /* push an item of HB_IT_POINTER type */
extern HB_EXPORT void     hb_vmPushBaseArray( PHB_BASEARRAY pBaseArray );
extern HB_EXPORT void     hb_vmPushState( void ); /* push current VM state on stack */
extern HB_EXPORT void     hb_vmPopState( void ); /* pop current VM state from stack */
extern HB_EXPORT void     hb_vmPushItemRef( PHB_ITEM pItem ); /* push item reference */

extern HB_EXPORT void     hb_vmPushSize( HB_ISIZ nNumber );

extern HB_EXPORT BOOL     hb_vmIsMt( void ); /* return TRUE if HVM is compiled with thread support */
extern HB_EXPORT void     hb_vmLock( void ); /* lock VM blocking GC execution by other threads */
extern HB_EXPORT void     hb_vmUnlock( void ); /* unlock VM, allow GC execution */
extern HB_EXPORT PSYMBOLS hb_vmLastModule( void );
extern HB_EXPORT PSYMBOLS * hb_vmSymbols( void );
extern HB_EXPORT void     hb_vmDoExitFunctions( void );
extern HB_EXPORT void     hb_vmPushNumType( double dNumber, int iDec, int iType1, int iType2 );

/* Object */
extern void hb_vmOperatorCall( PHB_ITEM, PHB_ITEM, char *, PHB_ITEM, int, PHB_ITEM ); /* call an overloaded operator */
extern void hb_vmOperatorCallUnary( PHB_ITEM, char *, PHB_ITEM ); /* call an overloaded unary operator */

#if defined(__DMC__) || defined(_MSC_VER) || defined(__POCC__)
   #if !defined(HB_DLL_REQUIRED_DLLMAIN)
      #define HB_DLL_REQUIRED_DLLMAIN
   #endif
#endif

HB_EXTERN_END

#endif /* HB_VM_H_ */
