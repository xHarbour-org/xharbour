/*
 * $Id: hbvm.h,v 1.19 2004/04/13 09:44:37 jacekp Exp $
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

HB_EXTERN_BEGIN

/* Harbour virtual machine init/exit functions */
extern void HB_EXPORT hb_vmInit( BOOL bStartMainProc );
extern int  HB_EXPORT hb_vmQuit( void );            /* Immediately quits the virtual machine */

/* Harbour virtual machine functions */
extern void HB_EXPORT hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols, PHB_ITEM** pGlobals );  /* invokes the virtual machine */
extern void HB_EXPORT hb_vmProcessSymbols( PHB_SYMB pSymbols, ... ); /* statics symbols initialization */
extern HB_EXPORT void hb_vmExplicitStartup( PHB_SYMB pSymbol );
extern void    hb_vmSymbolInit_RT( void );   /* initialization of runtime support symbols */

/* Harbour virtual machine escaping API */
extern void    HB_EXPORT hb_vmRequestBreak( PHB_ITEM pItem );
extern void    HB_EXPORT hb_vmRequestCancel( void );
extern void    hb_vmRequestDebug( void );
extern void    HB_EXPORT hb_vmRequestEndProc( void );
extern USHORT  HB_EXPORT hb_vmRequestQuery( void );
extern void    HB_EXPORT hb_vmRequestQuit( void );
extern void    HB_EXPORT hb_vmRequestReset( void );

/* Return values of hb_vmRequestQuery() */
#define HB_QUIT_REQUESTED       1   /* immediately quit the application */
#define HB_BREAK_REQUESTED      2   /* break to nearest RECOVER/END sequence */
#define HB_ENDPROC_REQUESTED    4   /* immediately return from procedure (error handler in macro evaluation) */

/* Public PCode functions */

/* Execution */
HB_EXPORT extern void    hb_vmDo( USHORT uiParams );      /* invoke the virtual machine */
HB_EXPORT extern void    hb_vmFunction( USHORT uiParams ); /* executes a function saving its result */
HB_EXPORT extern void    hb_vmSend( USHORT uiParams ); /* sends a message to an object */
HB_EXPORT extern PHB_ITEM hb_vmEvalBlock( PHB_ITEM pBlockItem ); /* executes passed codeblock with no arguments */
/* executes passed codeblock with variable number of arguments */
HB_EXPORT extern PHB_ITEM hb_vmEvalBlockV( PHB_ITEM pBlockItem, ULONG ulArgCount, ... );

/* Push */
HB_EXPORT extern void    hb_vmPush( PHB_ITEM pItem );     /* pushes a generic item onto the stack */
HB_EXPORT extern void    hb_vmPushNil( void );            /* in this case it places nil at self */
HB_EXPORT extern void    hb_vmPushNumber( double dNumber, int iDec ); /* pushes a number on to the stack and decides if it is SHORT, LONG or double */
HB_EXPORT extern void    hb_vmPushInteger( int iNumber ); /* pushes a integer number onto the stack */
HB_EXPORT extern void    hb_vmPushLong( LONG lNumber ); /* pushes a LONG number onto the stack */
HB_EXPORT extern void    hb_vmPushDouble( double lNumber, int iDec ); /* pushes a double number onto the stack */
HB_EXPORT extern void    hb_vmPushLogical( BOOL bValue );    /* pushes a logical value onto the stack */
HB_EXPORT extern void    hb_vmPushString( char * szText, ULONG length );  /* pushes a string on to the stack */
HB_EXPORT extern void    hb_vmPushDate( LONG lDate );   /* pushes a LONG date onto the stack */
HB_EXPORT extern void    hb_vmPushSymbol( PHB_SYMB pSym ); /* pushes a function pointer onto the stack */
HB_EXPORT extern void    hb_vmPushPointer( void * ); /* push an item of HB_IT_POINTER type */
HB_EXPORT extern void    hb_vmPushBaseArray( PHB_BASEARRAY pBaseArray );

#ifndef HB_LONG_LONG_OFF
   HB_EXPORT extern void hb_vmPushLongLong( LONGLONG lNumber );
   HB_EXPORT extern void hb_vmPushNumInt( LONGLONG lNumber );
#else
   HB_EXPORT extern void hb_vmPushNumInt( LONG lNumber );
#endif

HB_EXPORT extern void hb_vmDoExitFunctions( void );
HB_EXPORT extern PSYMBOLS hb_vmLastModule( void );
HB_EXPORT extern void hb_vmProcessSymbols( PHB_SYMB pSymbols, ... );
HB_EXPORT extern void hb_vmPushNumType( double dNumber, int iDec, int iType1, int iType2 );
HB_EXPORT extern PSYMBOLS * hb_vmSymbols( void );
HB_EXPORT extern void hb_vmProcessDllSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols );

HB_EXTERN_END

#endif /* HB_VM_H_ */
