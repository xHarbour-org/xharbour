/*
 * $Id: bkgtsks.c,v 1.1 2003/12/19 01:41:11 fsgiudice Exp $
 */

/*
 * Harbour Project source code:
 * The background tasks - an extension of idle state
 *
 * Copyright 2003 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org - http://www.harbour-project.org
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
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_DOSPROCESS
#define INCL_NOPMAPI
#define HB_OS_WIN_32_USED

#define HB_THRAED_OPTIMIZE_STACK
#include "hbstack.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbvm.h"
#include "error.ch"

#if defined(HB_OS_UNIX)
#if defined(HB_OS_DARWIN)
   #include <unistd.h>    /* We need usleep() in Darwin */
#else
   #include <time.h>
#endif
#endif

#ifndef HB_THREAD_SUPPORT
/* list of background tasks
 * A pointer into an array of pointers to items with a codeblock
*/
static HB_ITEM_PTR * s_pBackgroundTasks = NULL;

/* flag to prevent recursive calls of hb_backgroundState() */
static BOOL s_bIamBackground = FALSE;

/* current task to be executed */
static USHORT s_uiBackgroundTask = 0;

/* number of tasks in the list */
static USHORT s_uiBackgroundMaxTask = 0;
#else

#define s_pBackgroundTasks    HB_VM_STACK.pBackgroundTasks
#define s_bIamBackground      HB_VM_STACK.bIamBackground
#define s_uiBackgroundTask    HB_VM_STACK.uiBackgroundTask
#define s_uiBackgroundMaxTask HB_VM_STACK.uiBackgroundMaxTask

#endif


/* RUN all tasks defined in background state */
void hb_backgroundRun( void )
{
   HB_THREAD_STUB

   if( ! s_bIamBackground )
   {
      s_bIamBackground = TRUE;

      if ( s_uiBackgroundTask < s_uiBackgroundMaxTask )
      {
         PHB_ITEM pItem = s_pBackgroundTasks[ s_uiBackgroundTask ];
         if ( HB_IS_BLOCK( pItem ) )
         {
            hb_vmEvalBlock( pItem );
         }
         else
         {
            hb_execFromArray( pItem );
         }
         ++s_uiBackgroundTask;
      }
      else
      {
         if( s_uiBackgroundMaxTask &&
             s_uiBackgroundTask == s_uiBackgroundMaxTask )
         {
            s_uiBackgroundTask = 0;
         }
      }
      s_bIamBackground = FALSE;
   }
}

void hb_backgroundReset( void )
{
   HB_THREAD_STUB

   if( s_uiBackgroundTask == s_uiBackgroundMaxTask )
   {
      s_uiBackgroundTask = 0;
   }
}

/* close all active background task on program exit */
void hb_backgroundShutDown( void )
{
   HB_THREAD_STUB

   if( s_pBackgroundTasks )
   {
      do
      {
         hb_itemRelease( s_pBackgroundTasks[ --s_uiBackgroundMaxTask ] );
      }
      while( s_uiBackgroundMaxTask );
      hb_xfree( s_pBackgroundTasks );
      s_pBackgroundTasks = NULL;
   }
}

/* forces to run Background functions */
HB_FUNC( HB_BACKGROUNDRUN )
{
   hb_backgroundRun();
}

/* call from user code to reset Background state */
HB_FUNC( HB_BACKGROUNDRESET )
{
   hb_backgroundReset();
}


ULONG hb_backgroundAddFunc( PHB_ITEM pBlock )
{
   HB_THREAD_STUB

   ++s_uiBackgroundMaxTask;
   if( !s_pBackgroundTasks )
   {
      s_pBackgroundTasks = ( HB_ITEM_PTR * ) hb_xgrab( sizeof( HB_ITEM_PTR ) );
   }
   else
   {
      s_pBackgroundTasks = ( HB_ITEM_PTR * ) hb_xrealloc( s_pBackgroundTasks, sizeof( HB_ITEM_PTR ) * s_uiBackgroundMaxTask );
   }
   /* store a copy of passed codeblock
   */
   s_pBackgroundTasks[ s_uiBackgroundMaxTask - 1 ] = hb_itemNew( pBlock );

   /* return a pointer as a handle to this Background task
   */
   if ( HB_IS_ARRAY( pBlock ) )
   {
      return ( ULONG ) pBlock->item.asArray.value;    /* TODO: access to pointers from harbour code */
   }
   else
   {
      return ( ULONG ) pBlock->item.asArray.value;    /* TODO: access to pointers from harbour code */
   }
}

/* add a new background task and return its handle */
HB_FUNC( HB_BACKGROUNDADD )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pBlock = hb_param( 1, HB_IT_ANY );

   if( HB_IS_BLOCK( pBlock ) || HB_IS_ARRAY( pBlock ) )
   {
      hb_retnl( hb_backgroundAddFunc( pBlock ) );
   }
   else
      hb_retnl( -1 );    /* error - a codeblock is required */
}

PHB_ITEM hb_backgroundDelFunc( ULONG ulID )
{
   HB_THREAD_STUB
   SHORT iTask;
   PHB_ITEM pItem = NULL;

   iTask = 0;
   while( iTask < s_uiBackgroundMaxTask )
   {
      pItem = s_pBackgroundTasks[ iTask ];

      if( ( pItem->type == HB_IT_BLOCK &&
            ulID == ( ULONG ) pItem->item.asBlock.value ) ||
          ( pItem->type == HB_IT_ARRAY &&
            ulID == ( ULONG ) pItem->item.asArray.value ) )

      {
         --s_uiBackgroundMaxTask;
         if( s_uiBackgroundMaxTask )
         {
            if( iTask != s_uiBackgroundMaxTask )
            {
               memcpy( &s_pBackgroundTasks[ iTask ], &s_pBackgroundTasks[ iTask + 1 ],
                        sizeof( HB_ITEM_PTR ) * ( s_uiBackgroundMaxTask - iTask ) );
            }
            s_pBackgroundTasks = ( HB_ITEM_PTR * ) hb_xrealloc( s_pBackgroundTasks, sizeof( HB_ITEM_PTR ) * s_uiBackgroundMaxTask );
         }
         else
         {
            hb_xfree( s_pBackgroundTasks );
            s_pBackgroundTasks = NULL;
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
HB_FUNC( HB_BACKGROUNDDEL )
{
   HB_THREAD_STUB
   PHB_ITEM pItem = NULL;

   if( s_pBackgroundTasks && ( hb_parinfo( 1 ) & HB_IT_NUMERIC ) )
   {
      ULONG ulID = hb_parnl( 1 );   /* TODO: access to pointers from harbour code */

      pItem = hb_backgroundDelFunc( ulID );
   }

   if( pItem == NULL )
   {
      hb_ret();    /* return NIL */
   }
   else
   {
      hb_itemReturn( pItem ); /* return a codeblock */
   }
}
