/*
 * $Id: thread.c,v 1.1 2002/12/18 13:43:57 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 * The MT support
 *
 * Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
 *                Ron Pinkas [Ron@RonPinkas.com]
 * www - http://www.xharbour.org
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 * hb_itemClear() and hb_itemCopy() are derivative work of original code
 * in the Harbour Project http://harbour-project.org (source/vm/itemapi.c)
 * Copyright of Antonio Linares <alinares@fivetech.com>
 *
 */

#include "hbstack.h"

#ifdef HB_THREAD_SUPPORT

#include <malloc.h>

#include "hbapi.h"
#include "thread.h"

HB_THREAD_CONTEXT *hb_ht_context;
HB_MUTEX_T context_monitor;

void hb_createContext( void )
{
   HB_THREAD_CONTEXT *p;
   HB_THREAD_CONTEXT *tc;
   int i;

   tc = (HB_THREAD_CONTEXT *) malloc( sizeof( HB_THREAD_CONTEXT));
#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
   tc->th_id = pthread_self();
#else
   tc->th_id = GetCurrentThreadId();
#endif

   tc->stack = ( HB_STACK *) malloc( sizeof( HB_STACK ) );
   tc->next = NULL;

   tc->stack->pItems = ( HB_ITEM_PTR * ) malloc( sizeof( HB_ITEM_PTR ) * STACK_INITHB_ITEMS );
   tc->stack->pBase  = tc->stack->pItems;
   tc->stack->pPos   = tc->stack->pItems;     /* points to the first stack item */
   tc->stack->wItems = STACK_INITHB_ITEMS;

   for( i=0; i < tc->stack->wItems; ++i )
   {
      tc->stack->pItems[ i ] = (HB_ITEM *) malloc( sizeof( HB_ITEM ) );
   }

   ( * (tc->stack->pPos) )->type = HB_IT_NIL;

   HB_MUTEX_LOCK( &context_monitor );

   if( hb_ht_context == NULL )
   {
      hb_ht_context = tc;
   }
   else
   {
      p = hb_ht_context;
      while( p->next )
      {
         p = p->next;
      }
      p->next = tc;
   }

   HB_MUTEX_UNLOCK( &context_monitor );
}

void hb_destroyContext( void )
{
   HB_THREAD_CONTEXT *p, *prev;
   HB_THREAD_T id;
   int i;

   if( hb_ht_context == NULL )
   {
      return;
   }

#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
   id = pthread_self();
#else
   id = GetCurrentThreadId();
#endif

   HB_MUTEX_LOCK( &context_monitor );

   p = hb_ht_context;
   prev = NULL;
   while( p && p->th_id != id )
   {
      prev = p;
      p = p->next;
   }

   if( p )
   {
      /*unlink the stack*/
      if( prev )
      {
         prev->next = p->next;
      }
      else
      {
         hb_ht_context = p->next;
      }

      HB_MUTEX_UNLOCK( &context_monitor );

      /* Free each element of the stack */
      for( i = 0; i < p->stack->wItems; ++i )
      {
         free( p->stack->pItems[ i ] );
      }

      free( p->stack->pItems );

      /* Free the stack */
      free( p->stack );

      /* Free the context */
      free( p );
   }
   else
   {
      HB_MUTEX_UNLOCK( &context_monitor );
   }
}

HB_THREAD_CONTEXT *hb_getCurrentContext( void )
{
   //static HB_THREAD_CONTEXT *last_context = NULL;
   HB_THREAD_CONTEXT *p;
   HB_THREAD_T id;

#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
   id = pthread_self();
#else
   id = GetCurrentThreadId();
#endif

/*
   if ( last_context != NULL && last_context->th_id == id )
   {
      return last_context->stack;
   }
*/

   HB_MUTEX_LOCK( &context_monitor );

   p = hb_ht_context;
   while( p && p->th_id != id )
   {
      p = p->next;
   }

   HB_MUTEX_UNLOCK( &context_monitor );

   return p;
}

void hb_contextInit( void )
{
   hb_ht_context = NULL;
   HB_MUTEX_INIT( &context_monitor );
   //last_context = NULL;
}

#ifdef HB_OS_WIN_32
   DWORD WINAPI
#else
   void *
#endif

hb_create_a_thread(

#ifdef HB_OS_WIN_32
   LPVOID sym
#else
   void *sym
#endif
                  )
{
   USHORT uiParam;
   HB_THREAD_PARAM *pt = (HB_THREAD_PARAM *) sym;
   PHB_ITEM pPointer = hb_arrayGetItemPtr( pt->args, 1 );

   hb_createContext();

   if( HB_IS_SYMBOL( pPointer ) )
   {
      hb_vmPushSymbol( pPointer->item.asSymbol.value );
   }
   else if( HB_IS_STRING( pPointer ) )
   {
      PHB_DYNS pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pPointer ) );

      hb_vmPushSymbol( pExecSym->pSymbol );
      hb_vmPushNil();
   }
   else if( HB_IS_BLOCK( pPointer ) )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pPointer );
   }

   for( uiParam = 2; uiParam <= pt->count; uiParam++ )
   {
      hb_vmPush( hb_arrayGetItemPtr( pt->args, uiParam ) );
   }

   if( HB_IS_OBJECT( hb_arrayGetItemPtr( pt->args, 2 ) ) )
   {
      hb_vmSend( pt->count - 2 );
   }
   else
   {
      if( HB_IS_SYMBOL( pPointer ) )
      {
         hb_vmDo( pt->count - 2 );
      }
      else
      {
         hb_vmDo( pt->count - 1 );
      }
   }

   hb_itemRelease( pt->args );
   hb_xfree( pt );

   hb_destroyContext();

   return NULL;
}

HB_FUNC( STARTTHREAD )
{
   PHB_ITEM pPointer;
   PHB_ITEM pargs = hb_arrayFromParams( hb_stack.pBase );
   HB_THREAD_T thid;
   HB_THREAD_PARAM *pt;

   pPointer  = hb_arrayGetItemPtr( pargs, 1 );

   /* Error Checking */
   if ( pPointer->type == HB_IT_LONG )
   {
      PHB_FUNC pFunc = (PHB_FUNC) hb_itemGetNL( pPointer );
      PHB_ITEM pSelf = NULL;
      PHB_DYNS pExecSym;
      int iParams;

      if( hb_pcount() >= 2 )
      {
         if( HB_IS_OBJECT( *( hb_stack.pBase + 1 + 2 ) ) )
         {
            pSelf = *( hb_stack.pBase + 1 + 2 );
            pExecSym = hb_clsSymbolFromFunction( pSelf, pFunc );
         }
      }

      if( pSelf == NULL )
      {
         pExecSym = hb_dynsymFindFromFunction( pFunc );
      }

      if( pExecSym == NULL )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "StartThread", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         return;
      }

      // Converting it to its Symbol.
      pPointer->type = HB_IT_SYMBOL;
      pPointer->item.asSymbol.value = pExecSym->pSymbol;
   }
   else if( HB_IS_STRING( pPointer ) )
   {
      PHB_DYNS pDynSym = hb_dynsymFindName( hb_itemGetCPtr( pPointer ) );

      if( ! pDynSym )
      {
         hb_errRT_BASE( EG_NOFUNC, 1001, NULL, hb_itemGetCPtr( pPointer ), 1, pargs );
         hb_itemRelease( pargs );
         return;
      }
   }
   else if( ( ! HB_IS_BLOCK( pPointer ) ) && ( ! HB_IS_SYMBOL( pPointer ) )  && ( ! HB_IS_LONG( pPointer ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STARTTHREAD", 1, pargs );
      hb_itemRelease( pargs );
      return;
   }

   pt = (HB_THREAD_PARAM *) hb_xgrab( sizeof( HB_THREAD_PARAM ) );
   pt->args = pargs;
   pt->count = hb_pcount();

   /*phase 3: launch thread */
#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
   if( pthread_create( &thid, NULL, hb_create_a_thread, (void * ) pt ) == 0 );
#else
   if( CreateThread( NULL, 0, hb_create_a_thread, (LPVOID) pt, 0, &thid ) )
#endif
   {
      // Wait for the actual creation.
      while( hb_ht_context == NULL )
      {
         #if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
           {
              static struct timespec nanosecs = { 0, 1000 };
              nanosleep( &nanosecs, NULL );
           }
         #elif defined(HB_OS_WIN_32)
            Sleep( 0 );
         #endif
      }
   }

   hb_retnl( (long) thid );
}

HB_FUNC( CREATEMUTEX )
{
   HB_MUTEX_T *mutex = (HB_MUTEX_T *) hb_gcAlloc( sizeof( HB_MUTEX_T ), NULL );

   HB_MUTEX_INIT( mutex );

   hb_retptr( mutex );
}

HB_FUNC( DESTROYMUTEX )
{
   PHB_ITEM pMutex = hb_param( 1, HB_IT_POINTER );
   HB_MUTEX_T *mutex;

   if( pMutex )
   {
      mutex = pMutex->item.asPointer.value;
      HB_MUTEX_DESTROY( mutex );
      hb_gcFree( mutex );
   }
}

HB_FUNC( MUTEXLOCK )
{
   PHB_ITEM pMutex = hb_param( 1, HB_IT_POINTER );
   HB_MUTEX_T *mutex;

   if( pMutex )
   {
      mutex = pMutex->item.asPointer.value;
      HB_MUTEX_LOCK( mutex );
   }
}

HB_FUNC( MUTEXUNLOCK )
{
   PHB_ITEM pMutex = hb_param( 1, HB_IT_POINTER );
   HB_MUTEX_T *mutex;

   if( pMutex )
   {
      mutex = pMutex->item.asPointer.value;

      HB_MUTEX_UNLOCK( mutex );
   }
}

HB_FUNC( WAITFORTHREADS )
{
   while( hb_ht_context )
   {
      #if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
        {
           static struct timespec nanosecs = { 0, 1000 };
           nanosleep( &nanosecs, NULL );
        }
      #elif defined(HB_OS_WIN_32)
         Sleep( 0 );
      #endif
   }
}
#endif

