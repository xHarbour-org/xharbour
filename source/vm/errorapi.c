/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Error API
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
 *    DOSERROR()
 *    __ERRINHANDLER()
 *    __ERRRT_BASE()
 *    __ERRRT_SBASE()
 *    hb_errLaunch()
 *    hb_errLaunchSubst()
 *    hb_errGetFlags()
 *    hb_errPutFlags()
 *    hb_errRT_New()
 *    hb_errRT_New_Subst()
 *    hb_errRT_BASE()
 *    hb_errRT_BASE_Ext1()
 *    hb_errRT_BASE_Subst()
 *    hb_errInternal()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    rewritten in C ERROR class and all hb_errGet*() and hb_errPut*()
 *    functions
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "classes.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbset.h"
#include "hbvm.h"

#ifdef HB_OS_WIN
   #define HB_OS_WIN_USED
   #include "windows.h"
#endif

/* This is added to be able to detect a recursive error, and not let Harbour
   go into an infinite loop, this is an emulated version of the Clipper
   "Unrecoverable error 650: Processor stack fault" internal error, but
   better shows what is really the problem. [vszakats] */
#define HB_ERROR_LAUNCH_MAX      hb_stackSetStruct()->HB_SET_ERRORLOOP

/* Error class instance variables offsets */
#define HB_TERROR_CARGO          1
#define HB_TERROR_ARGS           2
#define HB_TERROR_FLAGS          3
#define HB_TERROR_DESCRIPTION    4
#define HB_TERROR_FILENAME       5
#define HB_TERROR_GENCODE        6
#define HB_TERROR_OPERATION      7
#define HB_TERROR_OSCODE         8
#define HB_TERROR_SEVERITY       9
#define HB_TERROR_SUBCODE        10
#define HB_TERROR_SUBSYSTEM      11
#define HB_TERROR_TRIES          12

/* xHarbour additions: */
#define HB_TERROR_PROCNAME       13
#define HB_TERROR_PROCLINE       14
#define HB_TERROR_RUNNINGTHREADS 15
#define HB_TERROR_OSTHREADID     16
#define HB_TERROR_VMTHREADID     17
#define HB_TERROR_MODULENAME     18
#define HB_TERROR_CALLSTACK      19
#define HB_TERROR_CANDEFAULT     20
#define HB_TERROR_CANRETRY       21
#define HB_TERROR_CANSUBST       22

#define HB_TERROR_IVARCOUNT      22


HB_FUNC( ERRORNEW );
/* pseudo function name in operation description
   (deprecated, kept for compatibility, use HB_ERR_FUNCNAME instead) */
const char        hb_errFuncName = 1;

static PHB_ITEM   s_pError       = NULL;

/* static HB_SYMB  s_symErrorNew = { "ERRORNEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ERRORNEW )}, NULL }; */

/* In MT, this data is held in the stack */
#ifndef HB_THREAD_SUPPORT
   static PHB_ERROR_INFO  s_errorHandler = NULL;
   static PHB_ITEM        s_errorBlock;
   static int             s_iLaunchCount = 0;
   static USHORT          s_uiErrorDOS   = 0; /* The value of DOSERROR() */
#else
#  define s_errorHandler  ( HB_VM_STACK.errorHandler )
#  define s_errorBlock    ( HB_VM_STACK.errorBlock )
#  define s_iLaunchCount  ( HB_VM_STACK.iLaunchCount )
#  define s_uiErrorDOS    ( HB_VM_STACK.uiErrorDOS )

#  ifdef HB_OS_OS2
#     include "thread.h"
#  endif

#endif

#if ! defined( HB_OS_DOS ) && ! defined( HB_OS_DARWIN_5 )
#   include "hbserv.h"
/* extern BOOL hb_isService(void); */
#endif

HB_EXTERN_BEGIN
extern int     hb_vm_iTry;
extern HB_ITEM hb_vm_BreakBlock;
HB_EXTERN_END

static BOOL hb_errGetNumCode( int * piValue, const char * szOperation )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_NUMERIC );

   if( pItem )
      *piValue = hb_itemGetNI( pItem );
   else
   {
      pItem = hb_errRT_BASE_Subst( EG_ARG, 0, NULL, szOperation,
                                   HB_ERR_ARGS_BASEPARAMS );
      if( ! pItem )
      {
         *piValue = 0;
         return FALSE;
      }

      if( ! HB_IS_NUMERIC( pItem ) )
         hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );

      *piValue = hb_itemGetNI( pItem );
      hb_itemRelease( pItem );
   }

   return TRUE;
}

HB_FUNC_STATIC( CARGO )
{
   HB_THREAD_STUB

   hb_itemReturn( hb_errGetCargo( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _CARGO )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem )
      hb_errPutCargo( hb_stackSelfItem(), pItem );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( ARGS )
{
   HB_THREAD_STUB

   hb_itemReturn( hb_errGetArgs( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _ARGS )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ARRAY );

   if( pItem )
      hb_errPutArgsArray( hb_stackSelfItem(), pItem );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( CANDEFAULT )
{
   HB_THREAD_STUB

   hb_retl( ( hb_errGetFlags( hb_stackSelfItem() ) & EF_CANDEFAULT ) != 0 );
}

HB_FUNC_STATIC( _CANDEFAULT )
{
   HB_THREAD_STUB

   if( ISLOG( 1 ) )
   {
      PHB_ITEM pError   = hb_stackSelfItem();
      BOOL     fCan     = hb_parl( 1 );

      if( fCan )
         hb_errPutFlags( pError, ( USHORT ) ( hb_errGetFlags( pError ) | EF_CANDEFAULT ) );
      else
         hb_errPutFlags( pError, ( USHORT ) ( hb_errGetFlags( pError ) & ~EF_CANDEFAULT ) );

      hb_retl( fCan );
   }
}

HB_FUNC_STATIC( CANRETRY )
{
   HB_THREAD_STUB

   hb_retl( ( hb_errGetFlags( hb_stackSelfItem() ) & EF_CANRETRY ) != 0 );
}

HB_FUNC_STATIC( _CANRETRY )
{
   HB_THREAD_STUB

   if( ISLOG( 1 ) )
   {
      PHB_ITEM pError   = hb_stackSelfItem();
      BOOL     fCan     = hb_parl( 1 );

      if( fCan )
         hb_errPutFlags( pError, ( USHORT ) ( hb_errGetFlags( pError ) | EF_CANRETRY ) );
      else
         hb_errPutFlags( pError, ( USHORT ) ( hb_errGetFlags( pError ) & ~EF_CANRETRY ) );

      hb_retl( fCan );
   }
}

HB_FUNC_STATIC( CANSUBST )
{
   HB_THREAD_STUB

   hb_retl( ( hb_errGetFlags( hb_stackSelfItem() ) & EF_CANSUBSTITUTE ) != 0 );
}

HB_FUNC_STATIC( _CANSUBST )
{
   HB_THREAD_STUB

   if( ISLOG( 1 ) )
   {
      PHB_ITEM pError   = hb_stackSelfItem();
      BOOL     fCan     = hb_parl( 1 );

      if( fCan )
         hb_errPutFlags( pError, ( USHORT ) ( hb_errGetFlags( pError ) | EF_CANSUBSTITUTE ) );
      else
         hb_errPutFlags( pError, ( USHORT ) ( hb_errGetFlags( pError ) & ~EF_CANSUBSTITUTE ) );

      hb_retl( fCan );
   }
}

HB_FUNC_STATIC( DESCRIPTION )
{
   HB_THREAD_STUB

   hb_retc( hb_errGetDescription( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _DESCRIPTION )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_STRING( pItem ) )
      hb_errPutDescription( hb_stackSelfItem(), hb_itemGetCPtr( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( FILENAME )
{
   HB_THREAD_STUB

   hb_retc( hb_errGetFileName( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _FILENAME )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_STRING( pItem ) )
      hb_errPutFileName( hb_stackSelfItem(), hb_itemGetCPtr( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( OPERATION )
{
   HB_THREAD_STUB

   hb_retc( hb_errGetOperation( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _OPERATION )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_STRING( pItem ) )
      hb_errPutOperation( hb_stackSelfItem(), hb_itemGetCPtr( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( SUBSYSTEM )
{
   HB_THREAD_STUB

   hb_retc( hb_errGetSubSystem( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _SUBSYSTEM )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_STRING( pItem ) )
      hb_errPutSubSystem( hb_stackSelfItem(), hb_itemGetCPtr( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( GENCODE )
{
   HB_THREAD_STUB

   hb_retni( hb_errGetGenCode( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _GENCODE )
{
   HB_THREAD_STUB

   int iValue;

   if( hb_errGetNumCode( &iValue, "GENCODE" ) )
   {
      hb_errPutGenCode( hb_stackSelfItem(), ( HB_ERRCODE ) iValue );
      hb_errPutDescription( hb_stackSelfItem(),
                            hb_langDGetErrorDesc( iValue ) );
   }

   hb_retni( iValue );
}

HB_FUNC_STATIC( OSCODE )
{
   HB_THREAD_STUB

   hb_retni( hb_errGetOsCode( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _OSCODE )
{
   HB_THREAD_STUB

   int iValue;

   if( hb_errGetNumCode( &iValue, "OSCODE" ) )
      hb_errPutOsCode( hb_stackSelfItem(), ( HB_ERRCODE ) iValue );

   hb_retni( iValue );
}

HB_FUNC_STATIC( SUBCODE )
{
   HB_THREAD_STUB

   hb_retni( hb_errGetSubCode( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _SUBCODE )
{
   HB_THREAD_STUB

   int iValue;

   if( hb_errGetNumCode( &iValue, "SUBCODE" ) )
      hb_errPutSubCode( hb_stackSelfItem(), ( HB_ERRCODE ) iValue );

   hb_retni( iValue );
}

HB_FUNC_STATIC( SEVERITY )
{
   HB_THREAD_STUB

   hb_retni( hb_errGetSeverity( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _SEVERITY )
{
   HB_THREAD_STUB

   int iValue;

   if( hb_errGetNumCode( &iValue, "SEVERITY" ) )
      hb_errPutSeverity( hb_stackSelfItem(), ( USHORT ) iValue );

   hb_retni( iValue );
}

HB_FUNC_STATIC( TRIES )
{
   HB_THREAD_STUB

   hb_retni( hb_errGetTries( hb_stackSelfItem() ) );
}

HB_FUNC_STATIC( _TRIES )
{
   HB_THREAD_STUB

   int iValue;

   if( hb_errGetNumCode( &iValue, "TRIES" ) )
      hb_errPutTries( hb_stackSelfItem(), ( USHORT ) iValue );

   hb_retni( iValue );
}

HB_FUNC_STATIC( _PROCNAME )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_STRING( pItem ) )
      hb_errPutProcName( hb_stackSelfItem(), hb_itemGetCPtr( pItem ) );

   hb_itemReturn( pItem );
}


HB_FUNC_STATIC( _PROCLINE )
{
   HB_THREAD_STUB

   int iValue;

   if( hb_errGetNumCode( &iValue, "PROCLINE" ) )
      hb_errPutProcLine( hb_stackSelfItem(), ( USHORT ) iValue );

   hb_retni( iValue );
}

HB_FUNC_STATIC( _RUNNINGTHREADS )
{
   HB_THREAD_STUB

   int iValue;

   if( hb_errGetNumCode( &iValue, "RUNNINGTHREADS" ) )
      hb_errPutRunningThreads( hb_stackSelfItem(), ( USHORT ) iValue );

   hb_retni( iValue );
}

HB_FUNC_STATIC( _OSTHREADID )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_INTEGER( pItem ) )
      hb_errPutThreadId( hb_stackSelfItem(), ( HB_THREAD_T ) ( HB_PTRDIFF ) hb_itemGetNInt( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( _VMTHREADID )
{
   HB_THREAD_STUB

   int iValue;

   if( hb_errGetNumCode( &iValue, "VMTHREADID" ) )
      hb_errPutVmThreadId( hb_stackSelfItem(), ( UINT ) iValue );

   hb_retni( iValue );
}

HB_FUNC_STATIC( _MODULENAME )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem && HB_IS_STRING( pItem ) )
      hb_errPutModuleName( hb_stackSelfItem(), hb_itemGetCPtr( pItem ) );

   hb_itemReturn( pItem );
}

HB_FUNC_STATIC( _AASTACK )
{
   HB_THREAD_STUB

   PHB_ITEM pItem = hb_param( 1, HB_IT_ARRAY );

   if( pItem )
      hb_errPutCallStack( hb_stackSelfItem(), pItem );

   hb_itemReturn( pItem );
}

static USHORT hb_errClassCreate( void )
{
   USHORT   usClassH = hb_clsCreate( HB_TERROR_IVARCOUNT, "ERROR" );
   PHB_ITEM pString, pNumber, pLogical, pArray;

   pString  = hb_itemPutC( NULL, "" );
   pNumber  = hb_itemPutNI( NULL, 0 );
   pLogical = hb_itemPutL( NULL, FALSE );
   pArray   = hb_itemArrayNew( 0 );

   hb_clsAdd( usClassH, "ARGS", HB_FUNCNAME( ARGS ) );
   hb_clsAdd( usClassH, "_ARGS", HB_FUNCNAME( _ARGS ) );
   hb_clsAdd( usClassH, "CANDEFAULT", HB_FUNCNAME( CANDEFAULT ) );
   hb_clsAdd( usClassH, "_CANDEFAULT", HB_FUNCNAME( _CANDEFAULT ) );
   hb_clsAdd( usClassH, "CANRETRY", HB_FUNCNAME( CANRETRY ) );
   hb_clsAdd( usClassH, "_CANRETRY", HB_FUNCNAME( _CANRETRY ) );
   hb_clsAdd( usClassH, "CANSUBSTITUTE", HB_FUNCNAME( CANSUBST ) );
   hb_clsAdd( usClassH, "_CANSUBSTITUTE", HB_FUNCNAME( _CANSUBST ) );
   hb_clsAdd( usClassH, "CARGO", HB_FUNCNAME( CARGO ) );
   hb_clsAdd( usClassH, "_CARGO", HB_FUNCNAME( _CARGO ) );
   hb_clsAdd( usClassH, "DESCRIPTION", HB_FUNCNAME( DESCRIPTION ) );
   hb_clsAdd( usClassH, "_DESCRIPTION", HB_FUNCNAME( _DESCRIPTION ) );
   hb_clsAdd( usClassH, "FILENAME", HB_FUNCNAME( FILENAME ) );
   hb_clsAdd( usClassH, "_FILENAME", HB_FUNCNAME( _FILENAME ) );
   hb_clsAdd( usClassH, "GENCODE", HB_FUNCNAME( GENCODE ) );
   hb_clsAdd( usClassH, "_GENCODE", HB_FUNCNAME( _GENCODE ) );
   hb_clsAdd( usClassH, "OPERATION", HB_FUNCNAME( OPERATION ) );
   hb_clsAdd( usClassH, "_OPERATION", HB_FUNCNAME( _OPERATION ) );
   hb_clsAdd( usClassH, "OSCODE", HB_FUNCNAME( OSCODE ) );
   hb_clsAdd( usClassH, "_OSCODE", HB_FUNCNAME( _OSCODE ) );
   hb_clsAdd( usClassH, "SEVERITY", HB_FUNCNAME( SEVERITY ) );
   hb_clsAdd( usClassH, "_SEVERITY", HB_FUNCNAME( _SEVERITY ) );
   hb_clsAdd( usClassH, "SUBCODE", HB_FUNCNAME( SUBCODE ) );
   hb_clsAdd( usClassH, "_SUBCODE", HB_FUNCNAME( _SUBCODE ) );
   hb_clsAdd( usClassH, "SUBSYSTEM", HB_FUNCNAME( SUBSYSTEM ) );
   hb_clsAdd( usClassH, "_SUBSYSTEM", HB_FUNCNAME( _SUBSYSTEM ) );
   hb_clsAdd( usClassH, "TRIES", HB_FUNCNAME( TRIES ) );
   hb_clsAdd( usClassH, "_TRIES", HB_FUNCNAME( _TRIES ) );

   /* xHarbour additions: */
   hb_clsAddDataInit( usClassH, "PROCNAME", HB_TERROR_PROCNAME, pString );
   hb_clsAdd(         usClassH, "_PROCNAME", HB_FUNCNAME( _PROCNAME ) );
   hb_clsAddDataInit( usClassH, "PROCLINE", HB_TERROR_PROCLINE, pNumber );
   hb_clsAdd(         usClassH, "_PROCLINE", HB_FUNCNAME( _PROCLINE ) );
   hb_clsAddDataInit( usClassH, "RUNNINGTHREADS", HB_TERROR_RUNNINGTHREADS, pNumber );
   hb_clsAdd(         usClassH, "_RUNNINGTHREADS", HB_FUNCNAME( _RUNNINGTHREADS ) );
   hb_clsAddDataInit( usClassH, "OSTHREADID", HB_TERROR_OSTHREADID, pNumber );
   hb_clsAdd(         usClassH, "_OSTHREADID", HB_FUNCNAME( _OSTHREADID ) );
   hb_clsAddDataInit( usClassH, "VMTHREADID", HB_TERROR_VMTHREADID, pNumber );
   hb_clsAdd(         usClassH, "_VMTHREADID", HB_FUNCNAME( _VMTHREADID ) );
   hb_clsAddDataInit( usClassH, "MODULENAME", HB_TERROR_MODULENAME, pString );
   hb_clsAdd(         usClassH, "_MODULENAME", HB_FUNCNAME( _MODULENAME ) );
   hb_clsAddDataInit( usClassH, "AASTACK", HB_TERROR_CALLSTACK, pArray );
   hb_clsAdd(         usClassH, "_AASTACK", HB_FUNCNAME( _AASTACK ) );

   hb_itemRelease( pString );
   hb_itemRelease( pNumber );
   hb_itemRelease( pLogical );
   hb_itemRelease( pArray );
   return usClassH;
}

HB_FUNC( ERRORNEW )
{
   HB_THREAD_STUB

   PHB_ITEM pError = hb_errNew(), pCallStack;
   char     szModuleName[ HB_PATH_MAX ];
   char     szProcName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
   USHORT   uLine;

   // hb_errNew() defaults to stack level 0, remove the entry for ERRORNEW()!
   pCallStack = hb_errGetCallStack( pError );
   hb_arrayDel( pCallStack, 1 );
   hb_arraySize( pCallStack, hb_arrayLen( pCallStack ) - 1 );

   if( ISCHAR( 1 ) )
      hb_errPutSubSystem( pError, hb_parc( 1 ) );

   if( ISNUM( 2 ) )
      hb_errPutGenCode( pError, ( HB_ERRCODE ) hb_parni( 2 ) );

   if( ISNUM( 3 ) )
      hb_errPutSubCode( pError, ( HB_ERRCODE ) hb_parni( 3 ) );

   if( ISCHAR( 4 ) )
      hb_errPutOperation( pError, hb_parc( 4 ) );

   if( ISCHAR( 5 ) )
      hb_errPutDescription( pError, hb_parc( 5 ) );

   if( ISARRAY( 6 ) )
      hb_errPutArgsArray( pError, hb_param( 6, HB_IT_ARRAY ) );

   if( ISCHAR( 7 ) )
      hb_errPutModuleName( pError, hb_parc( 7 ) );
   else
   {
      /* Must override default stack level 0, in hb_errNew()!
       */
      hb_procinfo( 1, NULL, NULL, szModuleName );
      hb_errPutModuleName( pError, szModuleName );
   }

   if( ISCHAR( 8 ) )
      hb_errPutProcName( pError, hb_parc( 8 ) );
   else
      /* Must override default stack level 0, in hb_errNew()!
       */
      hb_errPutProcName( pError, hb_procinfo( 1, szProcName, NULL, NULL ) );

   if( ISNUM( 9 ) )
      hb_errPutProcLine( pError, hb_parni( 9 ) );
   else
   {
      /* Must override default stack level 0, in hb_errNew()!
       */
      hb_procinfo( 1, NULL, &uLine, NULL );
      hb_errPutProcLine( pError, uLine );
   }

#ifdef HB_THREAD_SUPPORT
   hb_errPutRunningThreads( pError, hb_threadCountStacks() );
   hb_errPutThreadId( pError, HB_CURRENT_THREAD() );
   hb_errPutVmThreadId( pError, HB_VM_STACK.th_vm_id );
#endif

   hb_itemReturnRelease( pError );
}

/* There's a similar undocumented, internal function in CA-Cl*pper named
   ErrorInHandler(). [vszakats] */

HB_FUNC( __ERRINHANDLER )
{
   hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );
}

HB_FUNC( ERRORBLOCK )
{
   HB_THREAD_STUB

   PHB_ITEM pNewErrorBlock = hb_param( 1, HB_IT_BLOCK );
   PHB_ITEM pErrorBlock    = s_errorBlock;

   hb_itemReturn( pErrorBlock );

   if( pNewErrorBlock )
      hb_itemCopy( pErrorBlock, pNewErrorBlock );
}

PHB_ITEM hb_errorBlock( PHB_ITEM pNewErrorBlock )
{
   HB_THREAD_STUB

   PHB_ITEM pReturn = hb_itemNew( s_errorBlock );

   if( pNewErrorBlock )
      hb_itemCopy( s_errorBlock, pNewErrorBlock );

   return pReturn;
}

/* set new low-level error launcher (C function) and return
 * handler currently active
 */
PHB_ERROR_INFO hb_errorHandler( PHB_ERROR_INFO pNewHandler )
{
   HB_THREAD_STUB

   PHB_ERROR_INFO pOld = s_errorHandler;

   if( pNewHandler )
      pNewHandler->Previous = s_errorHandler;

   s_errorHandler = pNewHandler;

   return pOld;
}

/* TOFIX: Make it Clipper compatible. [vszakats] */

HB_FUNC( DOSERROR )
{
   HB_THREAD_STUB

   hb_retni( s_uiErrorDOS );

   if( ISNUM( 1 ) )
      s_uiErrorDOS = ( USHORT ) hb_parni( 1 );
}

void hb_errInit( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_errInit()" ) );

   s_errorHandler = NULL;
   s_iLaunchCount = 0;
   s_uiErrorDOS   = 0;

   /* error function */
   hb_dynsymGetCase( "ERRORNEW" );

   /* Create error class and base object */
   s_pError = hb_itemNew( NULL );
   hb_clsAssociate( hb_errClassCreate() );
   hb_itemMove( s_pError, hb_stackReturnItem() );

#ifndef HB_THREAD_SUPPORT
   s_errorBlock = hb_itemNew( NULL );
#endif
}

void hb_errExit( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errExit()" ) );

   hb_itemRelease( s_pError );
   s_pError = NULL;

#ifndef HB_THREAD_SUPPORT
   if( s_errorBlock )
      hb_itemRelease( s_errorBlock );
#endif
}

PHB_ITEM hb_errNew( void )
{
   PHB_ITEM pError, pCallStack;
   int      iLevel;
   char     szModuleName[ HB_PATH_MAX ];
   char     szProcName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
   USHORT   uLine;

   HB_TRACE( HB_TR_DEBUG, ( "hb_errNew()" ) );

   if( ( ! s_pError ) || ( ! HB_IS_OBJECT( s_pError ) ) )
      hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );

   pError      = hb_arrayClone( s_pError, NULL );

   /* Build callstack array */
   pCallStack  = hb_itemArrayNew( 0 );
   for( iLevel = 0; hb_procinfo( iLevel, szProcName, &uLine, szModuleName ) && *szProcName; iLevel++ )
   {
      PHB_ITEM pItem = hb_itemArrayNew( 3 );

      if( iLevel == 0 )
      {
         hb_errPutModuleName( pError, szModuleName );
         hb_errPutProcName( pError, szProcName );
         hb_errPutProcLine( pError, uLine );
      }

      hb_arraySetC( pItem, 1, szModuleName );
      hb_arraySetC( pItem, 2, szProcName );
      hb_arraySetNI( pItem, 3, ( int ) uLine );

      hb_arrayAddForward( pCallStack, pItem );
      hb_itemRelease( pItem );
   }
   hb_errPutCallStack( pError, pCallStack );
   hb_itemRelease( pCallStack );

   return pError;
}

USHORT hb_errLaunch( PHB_ITEM pError )
{
   HB_THREAD_STUB

#ifdef HB_THREAD_SUPPORT
   BOOL old_bIdleFence;
#endif

   USHORT uiAction = hb_vmRequestQuery();
   USHORT usRequest;

   HB_TRACE( HB_TR_DEBUG, ( "hb_errLaunch(%p)", pError ) );

   if( uiAction )
      return E_BREAK;
   else
      uiAction = E_DEFAULT;

   /* Act as an idle inspector */
#ifdef HB_THREAD_SUPPORT
   /* Don't run on quit request */
   if( hb_vm_bQuitRequest )
      return 0;    /* Meaningless here */

   /* Force idle fencing (should be locked, but race conditions
      are not a problem in this case) */
   old_bIdleFence = hb_bIdleFence;
   hb_bIdleFence  = TRUE;

   hb_threadWaitForIdle();

   hb_bIdleFence  = old_bIdleFence;
   usRequest      = 0;
#endif

   if( pError )
   {
      PHB_ITEM pResult;

      /* Check if we have a valid error handler */

      if( hb_itemType( s_errorBlock ) != HB_IT_BLOCK )
      {
         hb_itemRelease( pError );
         hb_errInternal( HB_EI_ERRNOBLOCK, NULL, NULL, NULL );
      }

      /* Check if the error launcher was called too many times recursively */
      if( s_iLaunchCount == HB_ERROR_LAUNCH_MAX )
      {
         hb_itemRelease( pError );
         hb_errInternal( HB_EI_ERRTOOMANY, NULL, NULL, NULL );
      }

      /* Launch the error handler: "lResult := EVAL( ErrorBlock(), oError )" */
      s_iLaunchCount++;

      /* set DOSERROR() to last OS error code */
      s_uiErrorDOS = hb_errGetOsCode( pError );

      if( s_errorHandler )
      {
         /* there is a low-level error handler defined - use it instead
          * of normal Harbour-level one
          */
         s_errorHandler->Error      = pError;
         s_errorHandler->ErrorBlock = s_errorBlock;
         pResult                    = ( s_errorHandler->Func )( s_errorHandler );
         s_errorHandler->Error      = NULL;
      }
      else if( hb_vm_iTry && s_errorBlock->item.asBlock.value == hb_vm_BreakBlock.item.asBlock.value )
      {
         hb_vmRequestBreak( pError );
         s_iLaunchCount--;

         /* Allow other threads to go */
#if defined( HB_THREAD_SUPPORT )
         hb_threadIdleEnd();
#endif
         return E_BREAK;
      }
      else
      {
         HB_VM_STACK.uiVMFlags   |= HB_SUSPEND_QUIT;
         pResult                 = hb_itemDo( s_errorBlock, 1, pError );
         HB_VM_STACK.uiVMFlags   &= ~HB_SUSPEND_QUIT;
      }

      s_iLaunchCount--;

      /* Check results */

      usRequest = hb_vmRequestQuery();

      if( usRequest == HB_QUIT_REQUESTED )
      {
         if( pResult )
            hb_itemRelease( pResult );

         uiAction = E_BREAK;

         /* Allow other threads to go */
#if defined( HB_THREAD_SUPPORT )
         hb_itemRelease( pError );
         /* We are going to quit now, so we don't want to have mutexes
            blocking our output */
         hb_stackSetStruct()->HB_SET_OUTPUTSAFETY = FALSE;
         hb_threadIdleEnd();
         exit( hb_vmQuit() );
#endif
      }
      else if( usRequest == HB_BREAK_REQUESTED || usRequest == HB_ENDPROC_REQUESTED )
      {
         if( pResult )
            hb_itemRelease( pResult );

         uiAction = E_BREAK;
      }
      else if( pResult )
      {
         BOOL     bFailure = FALSE;
         USHORT   uiFlags  = hb_errGetFlags( pError );

         /* If the error block didn't return a logical value, */
         /* or the canSubstitute flag has been set, consider it as a failure */
         if( hb_itemType( pResult ) != HB_IT_LOGICAL || ( uiFlags & EF_CANSUBSTITUTE ) )
            bFailure = TRUE;
         else
         {
            uiAction = hb_itemGetL( pResult ) ? E_RETRY : E_DEFAULT;

            if( ( uiAction == E_DEFAULT && ! ( uiFlags & EF_CANDEFAULT ) ) ||
                ( uiAction == E_RETRY && ! ( uiFlags & EF_CANRETRY ) ) )
               bFailure = TRUE;
         }

         hb_itemRelease( pResult );

         if( bFailure )
         {
            hb_itemRelease( pError );
            hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );
         }

         /* Add one try to the counter. */

         if( uiAction == E_RETRY )
            hb_errPutTries( pError, hb_errGetTries( pError ) + 1 );
      }
      else
      {
         hb_itemRelease( pError );
         hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );
      }
   }
   else
      uiAction = E_RETRY; /* Clipper does this, undocumented */

   /* Allow other threads to go */
#if defined( HB_THREAD_SUPPORT )
   if( usRequest != HB_QUIT_REQUESTED )
      hb_threadIdleEnd();
#endif

   return uiAction;
}

/* This error launcher should be used in those situations, where the error
   handler is expected to return a value to be substituted as the result of
   a failed operation. [vszakats] */

/* NOTE: This should only be called when the EF_CANSUBSTITUE flag was set
         Since in this case the error handler will return the value
         to be substituted. [vszakats] */

/* NOTE: The item pointer returned should be hb_itemRelease()-d by the
         caller if it was not NULL. [vszakats] */

PHB_ITEM hb_errLaunchSubst( PHB_ITEM pError )
{
   HB_THREAD_STUB

   PHB_ITEM pResult;
   USHORT   usRequest;

#ifdef HB_THREAD_SUPPORT
   BOOL     old_bIdleFence;
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_errLaunchSubst(%p)", pError ) );

   if( hb_vmRequestQuery() )
      return NULL;

   /* Act as an idle inspector */
#ifdef HB_THREAD_SUPPORT
   /* Don't run on quit request */
   if( hb_vm_bQuitRequest )
      return NULL;    /* Meaningless here */

   /* Force idle fencing */
   old_bIdleFence = hb_bIdleFence;
   hb_bIdleFence  = TRUE;
   hb_threadWaitForIdle();
   hb_bIdleFence  = old_bIdleFence;
   usRequest      = 0;
#endif

   if( pError )
   {
      /* Check if we have a valid error handler */
      if( hb_itemType( s_errorBlock ) != HB_IT_BLOCK )
      {
         hb_itemRelease( pError );
         hb_errInternal( HB_EI_ERRNOBLOCK, NULL, NULL, NULL );
      }

      /* Check if the error launcher was called too many times recursively */
      if( s_iLaunchCount == HB_ERROR_LAUNCH_MAX )
      {
         hb_itemRelease( pError );
         hb_errInternal( HB_EI_ERRTOOMANY, NULL, NULL, NULL );
      }

      /* Launch the error handler: "xResult := EVAL( ErrorBlock(), oError )" */
      s_iLaunchCount++;

      /* set DOSERROR() to last OS error code */
      s_uiErrorDOS = hb_errGetOsCode( pError );

      if( s_errorHandler )
      {
         /* there is a low-level error handler defined - use it instead
          * of normal Harbour-level one
          */
         s_errorHandler->Error      = pError;
         s_errorHandler->ErrorBlock = s_errorBlock;
         pResult                    = ( s_errorHandler->Func )( s_errorHandler );
         s_errorHandler->Error      = NULL;
      }
      else if( hb_vm_iTry && s_errorBlock->item.asBlock.value == hb_vm_BreakBlock.item.asBlock.value )
      {
         hb_vmRequestBreak( pError );
         s_iLaunchCount--;

         /* Allow other threads to go */
#if defined( HB_THREAD_SUPPORT )
         hb_threadIdleEnd();
#endif
         return NULL;
      }
      else
      {
         HB_VM_STACK.uiVMFlags   |= HB_SUSPEND_QUIT;
         pResult                 = hb_itemDo( s_errorBlock, 1, pError );
         HB_VM_STACK.uiVMFlags   &= ~HB_SUSPEND_QUIT;
      }

      s_iLaunchCount--;

      /* Check results */
      usRequest = hb_vmRequestQuery();

      if( usRequest == HB_QUIT_REQUESTED )
      {
         if( pResult )
         {
            hb_itemRelease( pResult );
            pResult = NULL;
         }

         hb_itemRelease( pError );

         /*
          *  If the error happened from an EXIT procedure (already called from hb_vmQuit() then
          *   hb_vmQuit() might immdeiately return here!
          */
#if defined( HB_THREAD_SUPPORT )
         /* We are going to quit now, so we don't want to have mutexes
            blocking our output */
         hb_stackSetStruct()->HB_SET_OUTPUTSAFETY = FALSE;
         hb_threadIdleEnd();
#endif

         exit( hb_vmQuit() );
      }
      else if( usRequest == HB_BREAK_REQUESTED || usRequest == HB_ENDPROC_REQUESTED )
      {
         if( pResult )
            hb_itemRelease( pResult );

         pResult = NULL;
      }
      else
      {
         /* If the canSubstitute flag has not been set,
            consider it as a failure. */
         if( ! ( hb_errGetFlags( pError ) & EF_CANSUBSTITUTE ) )
         {
            hb_itemRelease( pError );
            hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );
         }
      }
   }
   else
      pResult = hb_itemNew( NULL );

   /* Allow other threads to go */
#if defined( HB_THREAD_SUPPORT )
   if( usRequest != HB_QUIT_REQUESTED )
      hb_threadIdleEnd();
#endif

   return pResult;
}

void hb_errRelease( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errRelease(%p)", pError ) );

   /* NOTE: NULL pointer is checked by hb_itemRelease() [vszakats] */
   hb_itemRelease( pError );
}

PHB_ITEM hb_errGetCargo( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetCargo(%p)", pError ) );

   return hb_arrayGetItemPtr( pError, HB_TERROR_CARGO );
}

PHB_ITEM hb_errPutCargo( PHB_ITEM pError, PHB_ITEM pCargo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutCargo(%p, %p)", pError, pCargo ) );

   hb_arraySet( pError, HB_TERROR_CARGO, pCargo );

   return pError;
}

PHB_ITEM hb_errGetArgs( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetArgs(%p)", pError ) );

   return hb_arrayGetItemPtr( pError, HB_TERROR_ARGS );
}

PHB_ITEM hb_errPutArgsArray( PHB_ITEM pError, PHB_ITEM pArgs )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutArgsArray(%p, %p)", pError, pArgs ) );

   hb_arraySet( pError, HB_TERROR_ARGS, pArgs );

   return pError;
}

const char * hb_errGetDescription( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetDescription(%p)", pError ) );

   return hb_arrayGetCPtr( pError, HB_TERROR_DESCRIPTION );
}

PHB_ITEM hb_errPutDescription( PHB_ITEM pError, const char * szDescription )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutDescription(%p, %s)", pError, szDescription ) );

   hb_arraySetC( pError, HB_TERROR_DESCRIPTION, szDescription );

   return pError;
}

const char * hb_errGetFileName( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetFileName(%p)", pError ) );

   return hb_arrayGetCPtr( pError, HB_TERROR_FILENAME );
}

PHB_ITEM hb_errPutFileName( PHB_ITEM pError, const char * szFileName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutFileName(%p, %s)", pError, szFileName ) );

   hb_arraySetC( pError, HB_TERROR_FILENAME, szFileName );

   return pError;
}

HB_ERRCODE hb_errGetGenCode( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetGenCode(%p)", pError ) );

   return ( HB_ERRCODE ) hb_arrayGetNI( pError, HB_TERROR_GENCODE );
}

PHB_ITEM hb_errPutGenCode( PHB_ITEM pError, HB_ERRCODE errGenCode )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutGenCode(%p, %d)", pError, errGenCode ) );

   hb_arraySetNI( pError, HB_TERROR_GENCODE, errGenCode );

   return pError;
}

const char * hb_errGetOperation( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetOperation(%p)", pError ) );

   return hb_arrayGetCPtr( pError, HB_TERROR_OPERATION );
}

PHB_ITEM hb_errPutOperation( PHB_ITEM pError, const char * szOperation )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutOperation(%p, %s)", pError, szOperation == HB_ERR_FUNCNAME ? "HB_ERR_FUNCNAME" : szOperation ) );

   if( szOperation == HB_ERR_FUNCNAME )
   {
      PHB_SYMB pSym = hb_itemGetSymbol( hb_stackBaseItem() );

      if( pSym )
         szOperation = pSym->szName;
   }

   hb_arraySetC( pError, HB_TERROR_OPERATION, szOperation );

   return pError;
}

HB_ERRCODE hb_errGetOsCode( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetOsCode(%p)", pError ) );

   return ( HB_ERRCODE ) hb_arrayGetNI( pError, HB_TERROR_OSCODE );
}

PHB_ITEM hb_errPutOsCode( PHB_ITEM pError, HB_ERRCODE errOsCode )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutOsCode(%p, %d)", pError, errOsCode ) );

   hb_arraySetNI( pError, HB_TERROR_OSCODE, errOsCode );

   return pError;
}

USHORT hb_errGetSeverity( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetSeverity(%p)", pError ) );

   return ( USHORT ) hb_arrayGetNI( pError, HB_TERROR_SEVERITY );
}

PHB_ITEM hb_errPutSeverity( PHB_ITEM pError, USHORT uiSeverity )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutSeverity(%p, %hu)", pError, uiSeverity ) );

   hb_arraySetNI( pError, HB_TERROR_SEVERITY, uiSeverity );

   return pError;
}

HB_ERRCODE hb_errGetSubCode( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetSubCode(%p)", pError ) );

   return ( HB_ERRCODE ) hb_arrayGetNI( pError, HB_TERROR_SUBCODE );
}

PHB_ITEM hb_errPutSubCode( PHB_ITEM pError, HB_ERRCODE errSubCode )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutSubCode(%p, %d)", pError, errSubCode ) );

   hb_arraySetNI( pError, HB_TERROR_SUBCODE, errSubCode );

   return pError;
}

const char * hb_errGetSubSystem( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetSubSytem(%p)", pError ) );

   return hb_arrayGetCPtr( pError, HB_TERROR_SUBSYSTEM );
}

PHB_ITEM hb_errPutSubSystem( PHB_ITEM pError, const char * szSubSystem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutSubSytem(%p, %s)", pError, szSubSystem ) );

   hb_arraySetC( pError, HB_TERROR_SUBSYSTEM, szSubSystem );

   return pError;
}

char * hb_errGetProcName( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetProcName(%p)", pError ) );

   return hb_arrayGetCPtr( pError, HB_TERROR_PROCNAME );
}

PHB_ITEM hb_errPutProcName( PHB_ITEM pError, const char * szProcName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutProcName(%p, %s)", pError, szProcName ) );

   hb_arraySetC( pError, HB_TERROR_PROCNAME, szProcName );

   return pError;
}

UINT hb_errGetProcLine( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetProcLine(%p)", pError ) );

   return hb_arrayGetNI( pError, HB_TERROR_PROCLINE );
}

PHB_ITEM hb_errPutProcLine( PHB_ITEM pError, UINT uiLine )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutProcLine(%p, %d)", pError, uiLine ) );

   hb_arraySetNI( pError, HB_TERROR_PROCLINE, uiLine );

   return pError;
}

HB_THREAD_T hb_errGetThreadId( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetThreadId(%p)", pError ) );

   return ( HB_THREAD_T ) ( HB_PTRDIFF ) hb_arrayGetNInt( pError, HB_TERROR_OSTHREADID );
}

PHB_ITEM hb_errPutThreadId( PHB_ITEM pError, HB_THREAD_T thId )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutThreadId(%p, %X)", pError, ( int ) thId ) );

   hb_arraySetNInt( pError, HB_TERROR_OSTHREADID, ( HB_LONG ) ( HB_PTRDIFF ) thId );

   return pError;
}

UINT hb_errGetRunningThreads( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetRunningThreads(%p)", pError ) );

   return ( UINT ) hb_arrayGetNI( pError, HB_TERROR_RUNNINGTHREADS );
}

PHB_ITEM hb_errPutRunningThreads( PHB_ITEM pError, UINT uiCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutRunningThreads(%p, %d)", pError, uiCount ) );

   hb_arraySetNI( pError, HB_TERROR_RUNNINGTHREADS, uiCount );

   return pError;
}

UINT hb_errGetVmThreadId( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetVmThreadId(%p)", pError ) );

   return ( UINT ) hb_arrayGetNI( pError, HB_TERROR_VMTHREADID );
}

PHB_ITEM hb_errPutVmThreadId( PHB_ITEM pError, UINT uiThId )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutVmThreadId(%p, %d)", pError, uiThId ) );

   hb_arraySetNI( pError, HB_TERROR_VMTHREADID, ( int ) uiThId );

   return pError;
}

char * hb_errGetModuleName( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetModuleName(%p)", pError ) );

   return hb_arrayGetCPtr( pError, HB_TERROR_MODULENAME );
}

PHB_ITEM hb_errPutModuleName( PHB_ITEM pError, const char * szModuleName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutModuleName(%p, %s)", pError, szModuleName ) );

   hb_arraySetC( pError, HB_TERROR_MODULENAME, szModuleName );

   return pError;
}

PHB_ITEM hb_errGetCallStack( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetCallStack(%p)", pError ) );

   return hb_arrayGetItemPtr( pError, HB_TERROR_CALLSTACK );
}

PHB_ITEM hb_errPutCallStack( PHB_ITEM pError, PHB_ITEM pCallStack )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutCallStack(%p, %p)", pError, pCallStack ) );

   hb_arraySet( pError, HB_TERROR_CALLSTACK, pCallStack );

   return pError;
}

USHORT hb_errGetTries( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetTries(%p)", pError ) );

   return ( USHORT ) hb_arrayGetNI( pError, HB_TERROR_TRIES );
}

PHB_ITEM hb_errPutTries( PHB_ITEM pError, USHORT uiTries )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutTries(%p, %hu)", pError, uiTries ) );

   hb_arraySetNI( pError, HB_TERROR_TRIES, uiTries );

   return pError;
}

USHORT hb_errGetFlags( PHB_ITEM pError )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errGetFlags(%p)", pError ) );

   return ( USHORT ) hb_arrayGetNI( pError, HB_TERROR_FLAGS );
}

PHB_ITEM hb_errPutFlags( PHB_ITEM pError, USHORT uiFlags )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutFlags(%p, %hu)", pError, uiFlags ) );

   uiFlags &= EF_CANRETRY | EF_CANSUBSTITUTE | EF_CANDEFAULT;
   hb_arraySetNI( pError, HB_TERROR_FLAGS, uiFlags );
   hb_arraySetL( pError, HB_TERROR_CANRETRY, ( uiFlags & EF_CANRETRY ) != 0 );
   hb_arraySetL( pError, HB_TERROR_CANDEFAULT, ( uiFlags & EF_CANDEFAULT ) != 0 );
   hb_arraySetL( pError, HB_TERROR_CANSUBST, ( uiFlags & EF_CANSUBSTITUTE ) != 0 );

   return pError;
}

PHB_ITEM hb_errPutArgs( PHB_ITEM pError, ULONG ulArgCount, ... )
{
   PHB_ITEM pArray;
   ULONG    ulArgPos;
   va_list  va;

   HB_TRACE( HB_TR_DEBUG, ( "hb_errPutArgs(%p, %lu, ...)", pError, ulArgCount ) );

   pArray = hb_itemArrayNew( ulArgCount );

   /* Build the array from the passed arguments. */

   va_start( va, ulArgCount );
   for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      hb_itemArrayPut( pArray, ulArgPos, va_arg( va, PHB_ITEM ) );
   va_end( va );

   /* Assign the new array to the object data item. */
   hb_errPutArgsArray( pError, pArray );

   /* Release the Array. */
   hb_itemRelease( pArray );

   return pError;
}

/* Wrappers for hb_errLaunch() */

PHB_ITEM hb_errRT_New(
   USHORT uiSeverity,
   const char * szSubSystem,
   HB_ERRCODE errGenCode,
   HB_ERRCODE errSubCode,
   const char * szDescription,
   const char * szOperation,
   HB_ERRCODE errOsCode,
   USHORT uiFlags )
{
   HB_THREAD_STUB

   PHB_ITEM pError = hb_errNew();

   hb_errPutSeverity( pError, uiSeverity );
   hb_errPutSubSystem( pError, szSubSystem ? szSubSystem : HB_ERR_SS_BASE );
   hb_errPutGenCode( pError, errGenCode );
   hb_errPutSubCode( pError, errSubCode );
   hb_errPutDescription( pError, szDescription ? szDescription : ( const char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRDESC + errGenCode ) );
   hb_errPutOperation( pError, szOperation ? szOperation : "" );
   hb_errPutOsCode( pError, errOsCode );
   hb_errPutFlags( pError, uiFlags );

#ifdef HB_THREAD_SUPPORT
   hb_errPutThreadId( pError, HB_CURRENT_THREAD() );
   hb_errPutVmThreadId( pError, HB_VM_STACK.th_vm_id );
   hb_errPutRunningThreads( pError, hb_threadCountStacks() );
#endif

   return pError;
}

PHB_ITEM hb_errRT_New_Subst(
   USHORT uiSeverity,
   const char * szSubSystem,
   HB_ERRCODE errGenCode,
   HB_ERRCODE errSubCode,
   const char * szDescription,
   const char * szOperation,
   HB_ERRCODE errOsCode,
   USHORT uiFlags )
{
   PHB_ITEM pError = hb_errRT_New( uiSeverity, szSubSystem, errGenCode,
                                   errSubCode, szDescription, szOperation,
                                   errOsCode, uiFlags | EF_CANSUBSTITUTE );

   /* One level deeper for these errors */
   if( strcmp( szSubSystem, HB_ERR_SS_BASE ) == 0 && ( ( errGenCode == EG_NOVARMETHOD && errSubCode == 1005 ) || ( errGenCode == EG_NOMETHOD && errSubCode == 1004 ) ) )
   {
      char     szModuleName[ HB_PATH_MAX ];
      char     szProcName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
      USHORT   uLine;

      hb_procinfo( 1, szProcName, &uLine, szModuleName );

      /* Must override default stack level 0, in hb_errNew()!
       */
      hb_errPutModuleName( pError, szModuleName );
      hb_errPutProcName( pError, szProcName );
      hb_errPutProcLine( pError, uLine );
   }

   return pError;
}

PHB_ITEM hb_errRT_SubstParams( const char * szSubSystem, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation )
{
   PHB_ITEM pRetVal;
   PHB_ITEM pError;
   PHB_ITEM pArray;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_errRT_SubstParams()" ) );

   pError   = hb_errRT_New_Subst( ES_ERROR, szSubSystem ? szSubSystem : HB_ERR_SS_BASE,
                                  errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );

   pArray   = hb_arrayBaseParams();

   /* Assign the new array to the object data item. */
   hb_errPutArgsArray( pError, pArray );

   /* Release the Array. */
   hb_itemRelease( pArray );

   /* Ok, launch... */
   pRetVal = hb_errLaunchSubst( pError );

   hb_itemRelease( pError );

   return pRetVal;
}

PHB_ITEM hb_errRT_FileError( PHB_ITEM pError, const char * szSubSystem,
                             HB_ERRCODE errGenCode, HB_ERRCODE errSubCode,
                             const char * szFileName )
{
   if( ! pError )
   {
      pError = hb_errNew();
      hb_errPutSeverity( pError, ES_ERROR );
      hb_errPutSubSystem( pError, szSubSystem ? szSubSystem : HB_ERR_SS_BASE );
      hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
      hb_errPutFileName( pError, szFileName );
   }
   hb_errPutGenCode( pError, errGenCode );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( errGenCode ) );
   hb_errPutSubCode( pError, errSubCode );
   hb_errPutOsCode( pError, hb_fsError() );

   return pError;
}

HB_FUNC( __ERRRT_BASE )
{
   hb_errRT_BASE( ( HB_ERRCODE ) hb_parni( 1 ),
                  ( HB_ERRCODE ) hb_parni( 2 ),
                  hb_parcx( 3 ),
                  hb_parcx( 4 ),
                  ( USHORT ) hb_parni( 5 ),
                  hb_param( 6, HB_IT_ANY ) );
}

HB_FUNC( __ERRRT_SBASE )
{
   hb_errRT_BASE_SubstR( ( HB_ERRCODE ) hb_parni( 1 ),
                         ( HB_ERRCODE ) hb_parni( 2 ),
                         hb_parcx( 3 ),
                         hb_parcx( 4 ),
                         ( USHORT ) hb_parni( 5 ),
                         hb_param( 6, HB_IT_ANY ) );
}

USHORT hb_errRT_BASE( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ULONG ulArgCount, ... )
{
   HB_THREAD_STUB

   USHORT   uiAction;
   PHB_ITEM pArray;
   va_list  va;
   ULONG    ulArgPos;
   PHB_ITEM pError = hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );

   /* Build the array from the passed arguments. */
   if( ulArgCount == 0 )
      pArray = NULL;
   else if( ulArgCount == HB_ERR_ARGS_BASEPARAMS )
      pArray = ( hb_pcount() == 0 ) ? NULL : hb_arrayBaseParams();
   else if( ulArgCount == HB_ERR_ARGS_SELFPARAMS )
      pArray = hb_arraySelfParams();
   else
   {
      pArray = hb_itemArrayNew( ulArgCount );

      va_start( va, ulArgCount );
      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         PHB_ITEM pArg = va_arg( va, PHB_ITEM );
         if( pArg )
            hb_itemArrayPut( pArray, ulArgPos, pArg );
      }
      va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      hb_errPutArgsArray( pError, pArray );

      /* Release the Array. */
      hb_itemRelease( pArray );
   }

   /* Ok, launch... */
   uiAction = hb_errLaunch( pError );

   /* Release. */
   hb_errRelease( pError );

   return uiAction;
}

USHORT hb_errRT_BASE_Ext1( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode, USHORT uiFlags, ULONG ulArgCount, ... )
{
   HB_THREAD_STUB

   USHORT   uiAction;
   PHB_ITEM pArray;
   va_list  va;
   ULONG    ulArgPos;
   PHB_ITEM pError = hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags );

   /* Build the array from the passed arguments. */
   if( ulArgCount == 0 )
      pArray = NULL;
   else if( ulArgCount == HB_ERR_ARGS_BASEPARAMS )
      pArray = ( hb_pcount() == 0 ) ? NULL : hb_arrayBaseParams();
   else if( ulArgCount == HB_ERR_ARGS_SELFPARAMS )
      pArray = hb_arraySelfParams();
   else
   {
      pArray = hb_itemArrayNew( ulArgCount );

      va_start( va, ulArgCount );
      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         PHB_ITEM pArg = va_arg( va, PHB_ITEM );
         if( pArg )
            hb_itemArrayPut( pArray, ulArgPos, pArg );
      }
      va_end( va );
   }
   if( pArray )
   {
      /* Assign the new array to the object data item. */
      hb_errPutArgsArray( pError, pArray );

      /* Release the Array. */
      hb_itemRelease( pArray );
   }

   /* Ok, launch... */
   uiAction = hb_errLaunch( pError );

   hb_errRelease( pError );

   return uiAction;
}

PHB_ITEM hb_errRT_BASE_Subst( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ULONG ulArgCount, ... )
{
   HB_THREAD_STUB

   PHB_ITEM pRetVal;
   PHB_ITEM pError;
   PHB_ITEM pArray;
   va_list  va;
   ULONG    ulArgPos;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_errRT_BASE_Subst()" ) );

   pError = hb_errRT_New_Subst( ES_ERROR, HB_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );

   /* Build the array from the passed arguments. */
   if( ulArgCount == 0 )
      pArray = NULL;
   else if( ulArgCount == HB_ERR_ARGS_BASEPARAMS )
      pArray = ( hb_pcount() == 0 ) ? NULL : hb_arrayBaseParams();
   else if( ulArgCount == HB_ERR_ARGS_SELFPARAMS )
      pArray = hb_arraySelfParams();
   else
   {
      pArray = hb_itemArrayNew( ulArgCount );

      va_start( va, ulArgCount );
      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         PHB_ITEM pArg = va_arg( va, PHB_ITEM );
         if( pArg )
            hb_itemArrayPut( pArray, ulArgPos, pArg );
      }
      va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      hb_errPutArgsArray( pError, pArray );

      /* Release the Array. */
      hb_itemRelease( pArray );
   }

   /* Ok, launch... */
   pRetVal = hb_errLaunchSubst( pError );

   hb_errRelease( pError );

   return pRetVal;
}

void hb_errRT_BASE_SubstR( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ULONG ulArgCount, ... )
{
   HB_THREAD_STUB

   PHB_ITEM pError = hb_errRT_New_Subst( ES_ERROR, HB_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );
   PHB_ITEM pArray;
   va_list  va;
   ULONG    ulArgPos;

   /* Build the array from the passed arguments. */
   if( ulArgCount == 0 )
      pArray = NULL;
   else if( ulArgCount == HB_ERR_ARGS_BASEPARAMS )
      pArray = ( hb_pcount() == 0 ) ? NULL : hb_arrayBaseParams();
   else if( ulArgCount == HB_ERR_ARGS_SELFPARAMS )
      pArray = hb_arraySelfParams();
   else
   {
      pArray = hb_itemArrayNew( ulArgCount );

      va_start( va, ulArgCount );
      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         PHB_ITEM pArg = va_arg( va, PHB_ITEM );
         if( pArg )
            hb_itemArrayPut( pArray, ulArgPos, pArg );
      }
      va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      hb_errPutArgsArray( pError, pArray );

      /* Release the Array. */
      hb_itemRelease( pArray );
   }

   /* Ok, launch... */
   hb_itemReturnRelease( hb_errLaunchSubst( pError ) );
   hb_errRelease( pError );
}

USHORT hb_errRT_TERM( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode, USHORT uiFlags )
{
   PHB_ITEM pError   = hb_errRT_New( ES_ERROR, HB_ERR_SS_TERMINAL, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags );
   USHORT   uiAction = hb_errLaunch( pError );

   hb_errRelease( pError );

   return uiAction;
}

USHORT hb_errRT_DBCMD( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation )
{
   USHORT   uiFlags  = ( errGenCode == EG_NOTABLE ) ? EF_CANDEFAULT: EF_NONE;
   PHB_ITEM pError   = hb_errRT_New( ES_ERROR, HB_ERR_SS_DBCMD, errGenCode, errSubCode, szDescription, szOperation, 0, uiFlags );
   USHORT   uiAction = hb_errLaunch( pError );

   hb_errRelease( pError );

   return uiAction;
}

USHORT hb_errRT_DBCMD_Ext( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, USHORT uiFlags )
{
   PHB_ITEM pError   = hb_errRT_New( ES_ERROR, HB_ERR_SS_DBCMD, errGenCode, errSubCode, szDescription, szOperation, 0, uiFlags );
   USHORT   uiAction = hb_errLaunch( pError );

   hb_itemRelease( pError );

   return uiAction;
}

USHORT hb_errRT_TOOLS( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation )
{
   PHB_ITEM pError   = hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );
   USHORT   uiAction = hb_errLaunch( pError );

   hb_itemRelease( pError );

   return uiAction;
}

/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. [vszakats] */

void hb_errInternal( ULONG ulIntCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   char     title[ 64 ], buffer[ 1024 ];
   FILE *   fpError;
   BOOL     bLang;

   HB_TRACE( HB_TR_DEBUG, ( "hb_errInternal(%lu, %s, %s, %s)", ulIntCode, szText, szPar1, szPar2 ) );

   bLang = ( hb_langID() != NULL );

   if( szText )
   {
      fpError = hb_fopen( hb_setGetErrorLog(), "a" );

      if( fpError )
      {
         fclose( fpError );
         TraceLog( hb_setGetErrorLog(), szText, szPar1, szPar2 );
      }
   }

   hb_conOutErr( hb_conNewLine(), 0 );

   hb_snprintf( title, sizeof( title ), bLang ?
                ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRINTR ) :
                "Unrecoverable error %lu: ", ulIntCode );

   hb_conOutErr( title, 0 );

   if( szText != NULL )
      hb_snprintf( buffer, sizeof( buffer ), szText, szPar1, szPar2 );
   else if( bLang )
      hb_snprintf( buffer, sizeof( buffer ), ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRINTR + ulIntCode - 9000 ), szPar1, szPar2 );

   hb_conOutErr( buffer, 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
   hb_stackDispCall();

#ifdef HB_OS_WIN
   MessageBox( NULL, buffer, title, MB_ICONSTOP );
#endif

   /* release console settings */
   hb_conRelease();

   if( hb_cmdargCheck( "ERRGPF" ) )
   {
      int * pGPF = NULL;
      *pGPF       = 0;
      *( --pGPF ) = 0;
   }

#if defined( HB_THREAD_SUPPORT ) && defined( HB_OS_OS2 )
   /* Post all threads waiting on an indefinite wait */
   DosPostEventSem( hb_hevWakeUpAll );
   /* Let's give them some time to wake up */
   DosSleep( 5000 );
   /* Stop VM, I cannot call exit() here or I end up with a zombie process */
   hb_vmQuit();
#endif

   exit( EXIT_FAILURE );
}
