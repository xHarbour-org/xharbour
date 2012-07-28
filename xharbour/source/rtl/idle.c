/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The idle state collector
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_releaseCPU()
 *
 * Copyright 2003 Giancarlo Niccolai <antispam at niccolai dot ws>
 *    hb_idleAddFunc()
 *    hb_idleDelFunc()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_DOSPROCESS
#define INCL_NOPMAPI
#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbdate.h"
#include "error.ch"
#include "thread.h"
#if defined( HB_OS_UNIX )
   #include <sys/times.h>
   #include <unistd.h>
#endif
#include <time.h>

/* list of background tasks
 * A pointer into an array of pointers to items with a codeblock
*/
static HB_ITEM_PTR * s_pIdleTasks = NULL;

/* flag to prevent recursive calls of hb_idleState() */
static BOOL s_bIamIdle = FALSE;

/* current task to be executed */
static USHORT s_uiIdleTask = 0;

/* number of tasks in the list */
static USHORT s_uiIdleMaxTask = 0;

#if defined(HB_THREAD_SUPPORT) || defined(HB_OS_UNIX)
   #define HB_IDLE_MSEC_DEFAULT 10
#else
   #if defined(HB_OS_WIN) || defined(__CYGWIN__)
      #define HB_IDLE_MSEC_DEFAULT 20
   #else
      #define HB_IDLE_MSEC_DEFAULT 1
   #endif
#endif
/* sleep idle time in milli-seconds */
static USHORT s_uiIdleSleepMsec = HB_IDLE_MSEC_DEFAULT;
/* indicates if OS wait state is to be used instead of sleep() type function */
static int s_iIdleWaitNoCpu = 0;

/* flag to indicate GarbageCollection should be done in idle state. */
BOOL hb_vm_bCollectGarbage = TRUE;

void hb_releaseCPU( BOOL bIndefinite )
{
#if defined(HB_THREAD_SUPPORT) || defined(HB_OS_WIN) || defined(__CYGWIN__)
   BOOL bIdleWaitNoCpu = ( s_iIdleWaitNoCpu && bIndefinite && !s_uiIdleMaxTask ) ;   /* Only if No idle tasks */
#else
   HB_SYMBOL_UNUSED( bIndefinite );
#endif
   HB_TRACE(HB_TR_DEBUG, ("releaseCPU()"));

   /* TODO: Add code to release time slices on all platforms */
#ifdef HB_THREAD_SUPPORT

   hb_threadSleep( s_uiIdleSleepMsec, bIdleWaitNoCpu );

#else

   #if defined(HB_OS_WIN) || defined(__CYGWIN__)
      /* Forfeit the remainder of the current time slice. */
      if ( bIdleWaitNoCpu )
      {
         WaitMessage() ;
      }
      else
      {
         Sleep( s_uiIdleSleepMsec );
      }
   #elif defined(HB_OS_OS2)
      /* 23/nov/2000 - maurilio.longo@libero.it
         Minimum time slice under OS/2 is 32 milliseconds, passed 1 will be rounded to 32 and
         will give a chance to threads of lower priority to get executed.
         Passing 0 causes current thread to give up its time slice only if there are threads of
         equal priority waiting to be dispatched. Note: certain versions of OS/2 kernel have a
         bug which causes DosSleep(0) not to work as expected.  */
      DosSleep( s_uiIdleSleepMsec ); /* Duration is in milliseconds */

   #elif defined(HB_OS_DOS)

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

         regs.h.ah = 2;
         regs.HB_XREGS.ax = 0x1680;

         HB_DOS_INT86( 0x2F, &regs, &regs );
      }

   #elif defined(HB_OS_DARWIN)
      usleep( s_uiIdleSleepMsec );
   #elif defined(HB_OS_UNIX)
   {

      //struct timeval tv;
      //tv.tv_sec = 0;
      //tv.tv_usec = 1000;
//      select( 0, NULL, NULL, NULL, &tv );
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 20000;
      select( 0, NULL, NULL, NULL, &tv );


   }
   #else

   /* Do nothing */

   #endif
#endif
}

/* performs all tasks defined for idle state */
void hb_idleState( BOOL bIndefinite )
{
   if( ! s_bIamIdle )
   {
      s_bIamIdle = TRUE;

      if( hb_vm_bCollectGarbage )
      {
         hb_vm_bCollectGarbage = FALSE;
         hb_gcCollectAll( FALSE );
      }
      else if ( s_uiIdleTask < s_uiIdleMaxTask )
      {
         hb_itemRelease( hb_itemDo( s_pIdleTasks[ s_uiIdleTask ], 0 ) );
         ++s_uiIdleTask;
      }
      else
      {
         if( s_uiIdleMaxTask && hb_setGetIdleRepeat() &&
             s_uiIdleTask == s_uiIdleMaxTask )
         {
            s_uiIdleTask = 0;
            hb_vm_bCollectGarbage = TRUE;
         }
         hb_releaseCPU( bIndefinite );
      }
      s_bIamIdle = FALSE;
   }
}

void hb_idleReset( void )
{
   if( s_uiIdleTask == s_uiIdleMaxTask )
   {
      s_uiIdleTask = 0;
      hb_vm_bCollectGarbage = TRUE;
   }
}

/* close all active background task on program exit */
void hb_idleShutDown( void )
{
   if( s_pIdleTasks )
   {
      do
      {
         hb_itemRelease( s_pIdleTasks[ --s_uiIdleMaxTask ] );
      }
      while( s_uiIdleMaxTask );
      hb_xfree( s_pIdleTasks );
      s_pIdleTasks = NULL;
   }
}

void hb_idleSleep( double dSeconds )
{
   if( dSeconds >= 0 )
   {
      HB_ULONG end_timer = hb_dateMilliSeconds() + ( HB_ULONG ) ( dSeconds * 1000 );

      while( hb_dateMilliSeconds() < end_timer )
      {
         hb_idleState( FALSE );
      }
      hb_idleReset();
   }
}

/* signal that the user code is in idle state */
HB_FUNC( HB_IDLESTATE )
{
   hb_idleState( ( int ) hb_parl( 1 ) );
}

/* call from user code to reset idle state */
HB_FUNC( HB_IDLERESET )
{
   hb_idleReset();
}

/* call from user code to stay in idle state for given period */
HB_FUNC( HB_IDLESLEEP )
{
   hb_idleSleep( hb_parnd( 1 ) );
}


void * hb_idleAddFunc( PHB_ITEM pBlock )
{
   if( !HB_IS_BLOCK( pBlock ) && !HB_IS_ARRAY( pBlock ) )
   {
      return NULL;
   }

   ++s_uiIdleMaxTask;
   if( !s_pIdleTasks )
   {
      s_pIdleTasks = ( HB_ITEM_PTR * ) hb_xgrab( sizeof( HB_ITEM_PTR ) );
   }
   else
   {
      s_pIdleTasks = ( HB_ITEM_PTR * ) hb_xrealloc( s_pIdleTasks, sizeof( HB_ITEM_PTR ) * s_uiIdleMaxTask );
   }
   /* store a copy of passed codeblock
   */
   s_pIdleTasks[ s_uiIdleMaxTask - 1 ] = hb_itemNew( pBlock );

   /* return a pointer as a handle to this idle task
   */
   if ( HB_IS_ARRAY( pBlock ) )
   {
      return ( void * ) pBlock->item.asArray.value;    /* TODO: access to pointers from harbour code */
   }
   else
   {
      return ( void * ) pBlock->item.asBlock.value;    /* TODO: access to pointers from harbour code */
   }
}

/* add a new background task and return its handle */
HB_FUNC( HB_IDLEADD )
{
   HB_ITEM_PTR pBlock = hb_param( 1, HB_IT_ANY );

   if( HB_IS_BLOCK( pBlock ) || HB_IS_ARRAY( pBlock ) )
   {
      hb_retptr( hb_idleAddFunc( pBlock ) );
   }
   else
      hb_retptr( NULL );    /* error - a codeblock is required */
}

PHB_ITEM hb_idleDelFunc( void * pID )
{
   SHORT iTask;
   PHB_ITEM pItem = NULL;

   iTask = 0;
   while( iTask < s_uiIdleMaxTask )
   {
      pItem = s_pIdleTasks[ iTask ];

      if( ( pItem->type == HB_IT_BLOCK &&
            pID == ( void * ) pItem->item.asBlock.value ) ||
          ( pItem->type == HB_IT_ARRAY &&
            pID == ( void * ) pItem->item.asArray.value ) )

      {
         --s_uiIdleMaxTask;
         hb_itemRelease( pItem ) ;  // 23/02/2004 1:58p.m. Peter Rees: added line to fix memory leak
         if( s_uiIdleMaxTask )
         {
            if( iTask != s_uiIdleMaxTask )
            {
               HB_MEMCPY( &s_pIdleTasks[ iTask ], &s_pIdleTasks[ iTask + 1 ],
                        sizeof( HB_ITEM_PTR ) * ( s_uiIdleMaxTask - iTask ) );
            }
            s_pIdleTasks = ( HB_ITEM_PTR * ) hb_xrealloc( s_pIdleTasks, sizeof( HB_ITEM_PTR ) * s_uiIdleMaxTask );
         }
         else
         {
            hb_xfree( s_pIdleTasks );
            s_pIdleTasks = NULL;
         }
         /* Pitem has now a valid value */
         break;
      }
      ++iTask;
      /* Pitem is now NULL */
      pItem = NULL;
   }

   return pItem;
}

/* Delete a task with given handle and return a codeblock with this task */
HB_FUNC( HB_IDLEDEL )
{
   PHB_ITEM pItem = NULL;

   if( s_pIdleTasks && ( hb_parinfo( 1 ) & HB_IT_POINTER ) )
   {
      void * pID = hb_parptr( 1 );   /* TODO: access to pointers from harbour code */

      if ( pID )
      {
         pItem = hb_idleDelFunc( pID );
      }
   }

   if( pItem == NULL )
   {
      hb_ret();    /* return NIL */
   }
   else
   {
      hb_itemReturnForward( pItem ); /* return a codeblock */
   }
}

HB_FUNC( HB_IDLESLEEPMSEC )
{
   hb_retnl( s_uiIdleSleepMsec );
   if ( hb_pcount() > 0 )
   {
      s_uiIdleSleepMsec = ( USHORT ) hb_parnl( 1 );
   }
}

HB_FUNC( HB_IDLEWAITNOCPU )
{
   hb_retnl( ( LONG ) s_iIdleWaitNoCpu );
   if ( hb_pcount() > 0 )
   {
      s_iIdleWaitNoCpu = ( int ) hb_parnl( 1 );
   }
}
