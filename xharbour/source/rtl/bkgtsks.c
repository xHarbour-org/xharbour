/*
 * $Id: bkgtsks.c,v 1.8 2004/03/18 23:07:32 fsgiudice Exp $
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
static HB_BACKGROUNDTASK_PTR * s_pBackgroundTasks = NULL;

/* flag to prevent recursive calls of hb_backgroundState() */
static BOOL s_bIamBackground = FALSE;

/* current task to be executed */
static USHORT s_uiBackgroundTask = 0;

/* number of tasks in the list */
static USHORT s_uiBackgroundMaxTask = 0;
#else

#define s_pBackgroundTasks    (HB_VM_STACK.pBackgroundTasks)
#define s_bIamBackground      HB_VM_STACK.bIamBackground
#define s_uiBackgroundTask    HB_VM_STACK.uiBackgroundTask
#define s_uiBackgroundMaxTask HB_VM_STACK.uiBackgroundMaxTask

#endif

/* ------------------------  C  LEVEL ------------------------------ */

ULONG hb_backgroundAddFunc( PHB_ITEM pBlock, int nMillisec, BOOL bActive )
{
   HB_THREAD_STUB
   PHB_BACKGROUNDTASK pBkgTask;

   /* store a copy of passed codeblock
   */

   pBkgTask = ( PHB_BACKGROUNDTASK ) hb_xgrab( sizeof( HB_BACKGROUNDTASK ) );

   pBkgTask->pTask    = hb_itemNew( pBlock );
   pBkgTask->dSeconds = hb_secondsCPU( 3 );
   pBkgTask->millisec = nMillisec;
   pBkgTask->bActive  = bActive;

   ++s_uiBackgroundMaxTask;
   if( !s_pBackgroundTasks )
   {
      s_pBackgroundTasks = ( PHB_BACKGROUNDTASK * ) hb_xgrab( sizeof( HB_BACKGROUNDTASK ) );
   }
   else
   {
      s_pBackgroundTasks = ( PHB_BACKGROUNDTASK * ) hb_xrealloc( s_pBackgroundTasks, sizeof( HB_BACKGROUNDTASK ) * s_uiBackgroundMaxTask );
   }
   s_pBackgroundTasks[ s_uiBackgroundMaxTask - 1 ] = pBkgTask;

   /* return a pointer as a handle to this Background task
   */

   if ( HB_IS_ARRAY( pBlock ) )
   {
      return ( ULONG ) pBlock->item.asArray.value;    /* TODO: access to pointers from harbour code */
   }
   else
   {
      return ( ULONG ) pBlock->item.asBlock.value;    /* TODO: access to pointers from harbour code */
   }

}

/* RUN all tasks defined in background state */
void hb_backgroundRun( void )
{
   HB_THREAD_STUB
   PHB_BACKGROUNDTASK pBkgTask;

   if( ! s_bIamBackground )
   {
      s_bIamBackground = TRUE;

      if ( s_uiBackgroundTask < s_uiBackgroundMaxTask )
      {
         pBkgTask = (PHB_BACKGROUNDTASK) s_pBackgroundTasks[ s_uiBackgroundTask ];

         /* Check if a task can run */
         if ( pBkgTask->bActive &&
              ( pBkgTask->millisec == 0 ||
                !( pBkgTask->dSeconds ) ||
                ( ( ( hb_secondsCPU( 3 ) - pBkgTask->dSeconds ) * 1000 ) >= pBkgTask->millisec )
              )
            )
         {
            PHB_ITEM pItem = pBkgTask->pTask;
            if ( HB_IS_BLOCK( pItem ) )
            {
               hb_vmEvalBlock( pItem );
            }
            else
            {
               hb_execFromArray( pItem );
            }
            pBkgTask->dSeconds = hb_secondsCPU( 3 );
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

/* RUN only one tasks, no control done */
void hb_backgroundRunSingle( ULONG ulID )
{
   HB_THREAD_STUB
   PHB_BACKGROUNDTASK pBkgTask;

   if( ! s_bIamBackground )
   {
      s_bIamBackground = TRUE;

      pBkgTask = hb_backgroundFind( ulID );
      if ( pBkgTask )
      {
         PHB_ITEM pItem;
         pItem = pBkgTask->pTask;

         if ( HB_IS_BLOCK( pItem ) )
         {
            hb_vmEvalBlock( pItem );
         }
         else
         {
            hb_execFromArray( pItem );
         }
      }

      s_bIamBackground = FALSE;
   }
}

/* reset background counter to 0 */
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
         PHB_BACKGROUNDTASK pBkgTask;
         pBkgTask = s_pBackgroundTasks[ --s_uiBackgroundMaxTask ];
         hb_itemRelease( pBkgTask->pTask );
         pBkgTask->pTask = NULL;
         hb_xfree( pBkgTask );
      }
      while( s_uiBackgroundMaxTask );
      hb_xfree( s_pBackgroundTasks );

      s_pBackgroundTasks = NULL;
   }
}

PHB_ITEM hb_backgroundDelFunc( ULONG ulID )
{
   HB_THREAD_STUB
   SHORT iTask;
   PHB_BACKGROUNDTASK pBkgTask;
   PHB_ITEM pItem = NULL;
   BOOL bOldSet   = hb_set.HB_SET_BACKGROUNDTASKS;

   hb_set.HB_SET_BACKGROUNDTASKS = FALSE;

   iTask = 0;
   while( iTask < s_uiBackgroundMaxTask )
   {
      pBkgTask = s_pBackgroundTasks[ iTask ];
      pItem = pBkgTask->pTask;

      if( ( pItem->type == HB_IT_BLOCK &&
            ulID == ( ULONG ) pItem->item.asBlock.value ) ||
          ( pItem->type == HB_IT_ARRAY &&
            ulID == ( ULONG ) pItem->item.asArray.value ) )

      {
         --s_uiBackgroundMaxTask;
         hb_itemRelease( pItem );
         hb_xfree( pBkgTask );

         if( s_uiBackgroundMaxTask )
         {
            if( iTask != s_uiBackgroundMaxTask )
            {
               memcpy( &s_pBackgroundTasks[ iTask ], &s_pBackgroundTasks[ iTask + 1 ],
                        sizeof( HB_BACKGROUNDTASK ) * ( s_uiBackgroundMaxTask - iTask ) );
            }
            s_pBackgroundTasks = ( PHB_BACKGROUNDTASK * ) hb_xrealloc( s_pBackgroundTasks, sizeof( HB_BACKGROUNDTASK ) * s_uiBackgroundMaxTask );
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
   }

   hb_set.HB_SET_BACKGROUNDTASKS = bOldSet;

   return pItem;
}

/* Find a task */
PHB_BACKGROUNDTASK hb_backgroundFind( ULONG ulID )
{
   HB_THREAD_STUB
   SHORT iTask;
   PHB_BACKGROUNDTASK pBkgTask;

      iTask = 0;
      while( iTask < s_uiBackgroundMaxTask )
      {
         PHB_ITEM pItem;
         pBkgTask = s_pBackgroundTasks[ iTask ];
         pItem = pBkgTask->pTask;

         if( ( pItem->type == HB_IT_BLOCK &&
               ulID == ( ULONG ) pItem->item.asBlock.value ) ||
             ( pItem->type == HB_IT_ARRAY &&
               ulID == ( ULONG ) pItem->item.asArray.value ) )
         {
             return pBkgTask;
         }
         //else
         //{
         //    return NULL;
         //}

         ++iTask;
      }
      return NULL;

}

/* Set task as active */
BOOL hb_backgroundActive( ULONG ulID, BOOL bActive )
{
   HB_THREAD_STUB
   PHB_BACKGROUNDTASK pBkgTask;
   BOOL bOldState = FALSE;

   pBkgTask = hb_backgroundFind( ulID );

   if ( pBkgTask )
   {
      bOldState = pBkgTask->bActive;
      pBkgTask->bActive = bActive;
   }
   return bOldState;

}

/* Set task time */
int hb_backgroundTime( ULONG ulID, int nMillisec )
{
   HB_THREAD_STUB
   PHB_BACKGROUNDTASK pBkgTask;
   int nOldState = 0;

   pBkgTask = hb_backgroundFind( ulID );

   if ( pBkgTask )
   {
      nOldState = pBkgTask->millisec;
      pBkgTask->millisec = nMillisec;
   }
   return nOldState;

}


/* ------------------------ PRG LEVEL ------------------------------ */

/* forces to run Background functions */
HB_FUNC( HB_BACKGROUNDRUN )
{
   if ( s_pBackgroundTasks )
   {
      if ( hb_parinfo( 1 ) & HB_IT_NUMERIC )
      {
         ULONG ulID = hb_parnl( 1 );   /* TODO: access to pointers from harbour code */
         hb_backgroundRunSingle( ulID );
      }
      else
      {
         hb_backgroundRun();
      }
   }
}

/* call from user code to reset Background state */
HB_FUNC( HB_BACKGROUNDRESET )
{
   hb_backgroundReset();
}


/* add a new background task and return its handle */
HB_FUNC( HB_BACKGROUNDADD )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pBlock    = hb_param( 1, HB_IT_ANY );
   HB_ITEM_PTR pMillisec = hb_param( 2, HB_IT_NUMERIC );
   HB_ITEM_PTR pActive   = hb_param( 3, HB_IT_LOGICAL );

   if( HB_IS_BLOCK( pBlock ) || HB_IS_ARRAY( pBlock ) )
   {
      hb_retnl( hb_backgroundAddFunc( pBlock,
                                      ( pMillisec == NULL ? 0 : hb_itemGetNI( pMillisec ) ),
                                      ( pActive   == NULL ? TRUE : hb_itemGetL( pActive ) )
                                    ) );
   }
   else
      hb_retnl( -1 );    /* error - a codeblock is required */
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

/* Delete a task with given handle and return a codeblock with this task */
HB_FUNC( HB_BACKGROUNDACTIVE )
{
   HB_THREAD_STUB
   BOOL  bOldActive = FALSE;

   if ( s_pBackgroundTasks && ( hb_parinfo( 1 ) & HB_IT_NUMERIC ) )
   {
      ULONG ulID    = hb_parnl( 1 );   /* TODO: access to pointers from harbour code */
      BOOL  bActive = TRUE;
      if ( hb_parinfo( 2 ) & HB_IT_LOGICAL )
      {
         bActive = hb_parl( 2 );
      }

      bOldActive = hb_backgroundActive( ulID, bActive );
   }

   hb_retl( bOldActive ); /* return a codeblock */

}

/* Delete a task with given handle and return a codeblock with this task */
HB_FUNC( HB_BACKGROUNDTIME )
{
   HB_THREAD_STUB
   int  nOldMillisec = 0;

   if ( s_pBackgroundTasks && ( hb_parinfo( 1 ) & HB_IT_NUMERIC ) )
   {
      ULONG ulID    = hb_parnl( 1 );   /* TODO: access to pointers from harbour code */
      int  nMillisec = 1000;
      if ( hb_parinfo( 2 ) & HB_IT_NUMERIC )
      {
         nMillisec = hb_parni( 2 );
      }

      nOldMillisec = hb_backgroundTime( ulID, nMillisec );
   }

   hb_retni( nOldMillisec ); /* return a codeblock */
}
