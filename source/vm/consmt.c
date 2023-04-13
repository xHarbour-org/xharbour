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

#if 0
#ifdef HB_THREAD_SUPPORT
static void s_doNothing( void * nothing )
{
   HB_SYMBOL_UNUSED( nothing );
}
#endif
#endif

int hb_idle_msec_default( void )
{
#if defined( HB_THREAD_SUPPORT ) || defined( HB_OS_UNIX )
   return 10;
#else
#   if defined( HB_OS_WIN ) || defined( __CYGWIN__ )
       return 20;
#   else
       return 1;
#   endif
#endif
}

void hb_idle_releaseCPU( USHORT uiIdleSleepMsec, BOOL bIdleWaitNoCpu )
{
#if defined( HB_THREAD_SUPPORT )

   hb_threadSleep( uiIdleSleepMsec, bIdleWaitNoCpu );

#else

#if defined( HB_OS_OS2 ) || defined( HB_OS_DOS ) || defined( HB_OS_DARWIN ) || defined( HB_OS_UNIX )
   HB_SYMBOL_UNUSED( bIdleWaitNoCpu );
#endif

#if defined( HB_OS_WIN ) || defined( __CYGWIN__ )
   /* Forfeit the remainder of the current time slice. */
   if( bIdleWaitNoCpu )
      WaitMessage();
   else
      Sleep( uiIdleSleepMsec );
#elif defined( HB_OS_OS2 )
   /* 23/nov/2000 - maurilio.longo@libero.it
      Minimum time slice under OS/2 is 32 milliseconds, passed 1 will be rounded to 32 and
      will give a chance to threads of lower priority to get executed.
      Passing 0 causes current thread to give up its time slice only if there are threads of
      equal priority waiting to be dispatched. Note: certain versions of OS/2 kernel have a
      bug which causes DosSleep(0) not to work as expected.  */
   DosSleep( uiIdleSleepMsec );    /* Duration is in milliseconds */

#elif defined( HB_OS_DOS )

   /* NOTE: there is a bug under NT 4 and 2000 -  if the app is running
      in protected mode, time slices will _not_ be released - you must switch
      to real mode first, execute the following, and switch back.

      It just occurred to me that this is actually by design.  Since MS doesn't
      want you to do this from a console app, their solution was to not allow
      the call to work in protected mode - screw the rest of the planet <g>.

      returns zero on failure. (means not supported)
    */

   {
      union REGS regs;

      regs.h.ah         = 2;
      regs.HB_XREGS.ax  = 0x1680;

      HB_DOS_INT86( 0x2F, &regs, &regs );
   }

#elif defined( HB_OS_DARWIN )
   usleep( uiIdleSleepMsec );
#elif defined( HB_OS_UNIX )
   {
      HB_SYMBOL_UNUSED( uiIdleSleepMsec );
      /* struct timeval tv;
       * tv.tv_sec = 0;
       * tv.tv_usec = 1000;
       * select( 0, NULL, NULL, NULL, &tv );
       */
      struct timeval tv;
      tv.tv_sec   = 0;
      tv.tv_usec  = 20000;
      select( 0, NULL, NULL, NULL, &tv );
   }
#else
   /* Do nothing */
#endif
#endif
}

void hb_stack_lock( void )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB
   if( ! HB_VM_STACK.bInUse && HB_VM_STACK.uiIdleInspect == 0 )
   {
      HB_SHARED_LOCK( hb_runningStacks );
      while( hb_runningStacks.aux )
         HB_SHARED_WAIT( hb_runningStacks );
      hb_runningStacks.content.asLong++;
      HB_VM_STACK.bInUse = TRUE;
      HB_SHARED_UNLOCK( hb_runningStacks );
   }
#endif
}

void hb_stack_unlock( void )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB
   if( HB_VM_STACK.bInUse && HB_VM_STACK.uiIdleInspect == 0 )
   {
      HB_SHARED_LOCK( hb_runningStacks );
      hb_runningStacks.content.asLong--;
      HB_VM_STACK.bInUse = FALSE;
      HB_SHARED_SIGNAL( hb_runningStacks );
      HB_SHARED_UNLOCK( hb_runningStacks );
   }
#endif
}

void hb_console_safe_lock( void )
{
#if defined( HB_THREAD_SUPPORT )
/*   HB_THREAD_STUB
 *   HB_CLEANUP_PUSH( hb_setGetOutputSafety() ? s_doNothing : hb_rawMutexForceUnlock, hb_outputMutex );
 */
   if( hb_setGetOutputSafety() )
      HB_CRITICAL_LOCK( hb_outputMutex );
#endif
}

void hb_console_safe_unlock( void )
{
#if defined( HB_THREAD_SUPPORT )
/* HB_THREAD_STUB
 */
   if( hb_setGetOutputSafety() )
      HB_CRITICAL_UNLOCK( hb_outputMutex );
/* HB_CLEANUP_POP;
 */
#endif
}
