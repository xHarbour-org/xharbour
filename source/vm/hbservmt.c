/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * The MT support
 *
 * Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
 *                Ron Pinkas [Ron@RonPinkas.com]
 * www - http://www.xharbour.org
 *
 * this program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * this program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
 * GNU General public License for more details.
 *
 * You should have received a copy of the GNU General public License
 * along with this software; see the file COPYING.  if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * this exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General public License.
 *
 * this exception applies only to the code released with this xHarbour
 * explicit exception.  if you add/copy code from other sources,
 * as the General public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * if you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * if you do not wish that, delete this exception notice.
 *
 *
 */

/* source/rtl/hbserv.c */

void hb_service_hbstartservice( PHB_ITEM p_hooks )
{
#ifdef HB_THREAD_SUPPORT
   int iCount = hb_threadCountStacks();

   if( iCount > 2 || ( p_hooks == NULL && iCount > 1 ) )
   {
      /* TODO: Right error code here
       */
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Service must be started before starting threads", NULL, 0 );
      return;
   }
#else
   HB_SYMBOL_UNUSED( p_hooks );
#endif
}

void hb_service_ConsoleHandlerRoutineDestroyStack( PHB_STACK pStack )
{
#ifdef HB_THREAD_SUPPORT
   if( pStack )
      hb_threadDestroyStack( pStack );
#else
   HB_SYMBOL_UNUSED( pStack );
#endif
}

PHB_STACK hb_service_ConsoleHandlerRoutineInit( void )
{
#ifdef HB_THREAD_SUPPORT
   PHB_STACK pStack = NULL;

   /* we need a new stack: this is NOT an hb thread. */
#ifdef HB_OS_WIN
   if( TlsGetValue( hb_dwCurrentStack ) == 0 )
   {
      pStack         = hb_threadCreateStack( GetCurrentThreadId() );
      pStack->th_h   = GetCurrentThread();
      TlsSetValue( hb_dwCurrentStack, ( void * ) pStack );
   }
   return pStack;
#else
   return NULL;
#endif
#else
   return NULL;
#endif
}

void hb_service_threadCancel( void )
{
#ifndef HB_THREAD_SUPPORT
   hb_vmQuit();
   exit( 0 );
#else
   hb_threadCancelInternal();
#endif
}

void hb_service_signalHandlerQuit( PHB_FUNC pFunc )
{
#ifndef HB_THREAD_SUPPORT
   HB_SYMBOL_UNUSED( pFunc );
   hb_vmQuit();
   exit( 0 );
#else
   /* Allow signals to go through pthreads */
   /*  s_serviceSetDflSig(); */
   pFunc();
   /* NOTICE: should be pthread_exit(0), but a bug in linuxthread prevents it:
      calling pthread exit from a signal handler will cause infinite wait for
      restart signal.
      This solution is rude, while the other would allow clean VM termination...
      but it works.
    */
   exit( 0 );
#endif
}
