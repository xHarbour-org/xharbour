/*
* $Id: thread.c,v 1.41 2003/02/01 12:45:36 jonnymind Exp $
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


#if defined( HB_OS_DARWIN )
   #include <stdlib.h>
   #include <unistd.h>    /* We need usleep() in Darwin */
#else
   #include <malloc.h>
#endif

#if defined(__GNUC__) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))
      #include <sys/time.h>
      #include <time.h>
#endif

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "thread.h"

#ifdef HB_THREAD_SUPPORT

HB_THREAD_CONTEXT *hb_ht_context;
HB_CRITICAL_T hb_threadContextMutex;
HB_THREAD_CONTEXT *last_context;
HB_THREAD_T hb_main_thread_id;

HB_THREAD_CONTEXT *hb_threadCreateContext( HB_THREAD_T th )
{
   HB_THREAD_CONTEXT *p;
   HB_THREAD_CONTEXT *tc;
   int i;

   tc = (HB_THREAD_CONTEXT *) malloc( sizeof( HB_THREAD_CONTEXT));
   tc->th_id = th;

   tc->stack       = ( HB_STACK *) malloc( sizeof( HB_STACK ) );
   tc->Cargo       = NULL;
   tc->pDestructor = NULL;
   //tc->GCList      = NULL;
   tc->next        = NULL;

   tc->stack->pItems = ( HB_ITEM_PTR * ) malloc( sizeof( HB_ITEM_PTR ) * STACK_THREADHB_ITEMS );
   tc->stack->pBase  = tc->stack->pItems;
   tc->stack->pPos   = tc->stack->pItems;     /* points to the first stack item */
   tc->stack->wItems = STACK_THREADHB_ITEMS;

   //printf( "New Context: %p Stack: %p\n", tc, tc->stack );

   for( i = 0; i < tc->stack->wItems; ++i )
   {
      tc->stack->pItems[ i ] = (HB_ITEM *) malloc( sizeof( HB_ITEM ) );
   }

   ( * (tc->stack->pPos) )->type = HB_IT_NIL;

   HB_CRITICAL_LOCK( hb_threadContextMutex );

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

   HB_CRITICAL_UNLOCK( hb_threadContextMutex );

   return tc;
}


void hb_threadDestroyContext( HB_THREAD_T th_id )
{
   HB_THREAD_CONTEXT *p, *prev;
   int i;

   if ( hb_ht_context == NULL )
   {
      return;
   }

   HB_CRITICAL_LOCK( hb_threadContextMutex );

   p = hb_ht_context;
   prev = NULL;

   while ( p && p->th_id != th_id )
   {
      prev = p;
      p = p->next;
   }

   if( p )
   {
      /*unlink the stack*/
      if ( prev )
      {
         prev->next = p->next;
      }
      else
      {
         hb_ht_context = p->next;
      }

      // Only for secondary Stacks.
      HB_CRITICAL_UNLOCK( hb_threadContextMutex );

      /* Free each element of the stack */
      for( i = 0; i < p->stack->wItems; ++i )
      {
         if( HB_IS_COMPLEX( p->stack->pItems[ i ] ) )
         {
            hb_itemClear( p->stack->pItems[ i ] );
         }

         free( p->stack->pItems[ i ] );
      }

      /* Free the stack */
      free( p->stack->pItems );
      free( p->stack );

      // Call destructor
      if( p->pDestructor )
      {
         p->pDestructor( (void *) p );
      }

      /* Free the context */
      free( p );
   }
   else
   {
      char errdat[70];
      HB_CRITICAL_UNLOCK( hb_threadContextMutex );
      sprintf( errdat, "Context not found for Thread %ld",  (long) th_id );
      hb_errRT_BASE_SubstR( EG_CORRUPTION, 10001, errdat, "hb_threadDestroyContext", 0 );
   }
}

HB_THREAD_CONTEXT *hb_threadGetContext( HB_THREAD_T id )
{
   HB_THREAD_CONTEXT *p;

   //printf( "Requested context for %ld\r\n", id);

   HB_CRITICAL_LOCK( hb_threadContextMutex );

   if( last_context && last_context->th_id == id )
   {
      p = last_context;
   }
   else {

      p = hb_ht_context;

      while( p && p->th_id != id )
      {
         p = p->next;
      }

      if( p )
      {
         last_context = p;
      }
      else
      {
         char errdat[64];

         sprintf( errdat, "Context not found for Thread %ld",  (long) id );
         hb_errRT_BASE_SubstR( EG_CORRUPTION, 10001, errdat, "hb_threadGetCurrentContext", 0 );
      }

   }

   HB_CRITICAL_UNLOCK( hb_threadContextMutex );

   return p;

}

void hb_threadInit( void )
{
   hb_ht_context = NULL;
   HB_CRITICAL_INIT( hb_threadContextMutex );
   last_context = NULL;
   hb_main_thread_id = HB_CURRENT_THREAD();
}

void hb_threadExit( void )
{

   HB_CRITICAL_LOCK( hb_threadContextMutex );

   while( hb_ht_context )
   {
      #if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
         pthread_cancel( hb_ht_context->th_id );
         pthread_join( hb_ht_context->th_id, 0 );
      #else
         TerminateThread( hb_ht_context->th_h, 0);
         WaitForSingleObject( hb_ht_context->th_h, INFINITE );
         CloseHandle( hb_ht_context->th_h );
      #endif
      hb_threadDestroyContext( hb_ht_context->th_id );
   }

   HB_CRITICAL_UNLOCK( hb_threadContextMutex );
   HB_CRITICAL_DESTROY( hb_threadContextMutex );
}

#ifdef HB_OS_WIN_32
   DWORD WINAPI
#else
   void *
#endif

hb_create_a_thread(

#ifdef HB_OS_WIN_32
   LPVOID Cargo
#else
   void *Cargo
#endif
               )
{
   USHORT uiParam;
   HB_THREAD_PARAM *pt = (HB_THREAD_PARAM *) Cargo;
   HB_THREAD_T tCurrent = HB_CURRENT_THREAD();
   PHB_ITEM pPointer = hb_arrayGetItemPtr( pt->pArgs, 1 );

   #ifndef HB_OS_WIN_32
      /* Under windows, the context is created by the caller */
      hb_threadCreateContext( tCurrent );
      //printf( "Created context %p: %ld (%ld)\r\n", test, test->th_id, HB_CURRENT_THREAD() );

      /* now that the context has been created,
         it is safe to set async cancellation mode */
      pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS , NULL );
   #endif

   if( HB_IS_SYMBOL( pPointer ) )
   {
      hb_vmPushSymbol( pPointer->item.asSymbol.value );

      if( pt->bIsMethod )
      {
         hb_vmPush( hb_arrayGetItemPtr( pt->pArgs, 2 ) );
         uiParam = 3;
      }
      else
      {
         hb_vmPushNil();
         uiParam = 2;
      }

   }
   else if( HB_IS_BLOCK( pPointer ) )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pPointer );
      uiParam = 2;
   }

   for( ; uiParam <= pt->uiCount; uiParam++ )
   {
      hb_vmPush( hb_arrayGetItemPtr( pt->pArgs, uiParam ) );
   }

   hb_gcUnlock( pt->pArgs->item.asArray.value );
   hb_itemRelease( pt->pArgs );

   if( pt->bIsMethod )
   {
      hb_vmSend( pt->uiCount - 2 );
   }
   else
   {
      hb_vmDo( pt->uiCount - 1 );
   }

   free( pt );
   hb_threadDestroyContext( tCurrent );


   #ifdef HB_OS_WIN_32
      return 0;
   #else
      /* now we can detach this thread */
      pthread_detach( HB_CURRENT_THREAD() );
      return NULL;
   #endif
}

void hb_threadIsLocalRef( void )
{
   HB_THREAD_CONTEXT *pContext = hb_ht_context;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsLocalRef()"));

   HB_CRITICAL_LOCK( hb_threadContextMutex );

   hb_vmIsLocalRef();

   while( pContext )
   {
      //printf( "   Context: %p Stack: %p Items: %i\n", pContext, pContext->stack, pContext->stack->pPos - pContext->stack->pItems );

      if( pContext->stack->Return.type & (HB_IT_BYREF | HB_IT_POINTER | HB_IT_ARRAY | HB_IT_BLOCK) )
      {
         hb_gcItemRef( &(pContext->stack->Return) );
      }
      //printf( "After ThreadReturnRef\n" );

      if( pContext->stack->pPos > pContext->stack->pItems )
      {
         HB_ITEM_PTR *pItem = pContext->stack->pPos - 1;

         while( pItem != pContext->stack->pItems )
         {
            if( ( *pItem )->type & (HB_IT_BYREF | HB_IT_POINTER | HB_IT_ARRAY | HB_IT_BLOCK) )
            {
               //printf( "      Item: %p\n", pItem );
               hb_gcItemRef( *pItem );
            }

            --pItem;
         }
      }

      pContext = pContext->next;
   }

   HB_CRITICAL_UNLOCK( hb_threadContextMutex );
}

HB_FUNC( STARTTHREAD )
{
   PHB_ITEM pPointer;
   PHB_ITEM pArgs;
   HB_THREAD_T th_id;
   HB_THREAD_PARAM *pt;
   PHB_DYNS pExecSym;
   PHB_FUNC pFunc;
   BOOL bIsMethod = FALSE;
#ifdef HB_OS_WIN_32
   HANDLE th_h;
#endif

   pArgs = hb_arrayFromParamsLocked( HB_VM_STACK.pBase );
   pPointer  = hb_arrayGetItemPtr( pArgs, 1 );

   /* Error Checking */
   if( pPointer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STARTTHREAD", 1, pArgs );
      hb_gcUnlock( pt->pArgs->item.asArray.value );
      hb_itemRelease( pArgs );
      return;
   }

   /* Is it a function pointer? */
   if ( pPointer->type == HB_IT_LONG )
   {
      pFunc =  (PHB_FUNC) hb_itemGetNL( pPointer );
      pExecSym = hb_dynsymFindFromFunction( pFunc );

      if( pExecSym == NULL )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "StartThread", 1, hb_paramError( 1 ) );
         hb_gcUnlock( pt->pArgs->item.asArray.value );
         hb_itemRelease( pArgs );
         return;
      }

      // Converting it to its Symbol.
      pPointer->type = HB_IT_SYMBOL;
      pPointer->item.asSymbol.value = pExecSym->pSymbol;
   }
   /* Is it an object? */
   else if( hb_pcount() >= 2 && pPointer->type == HB_IT_OBJECT )
   {
      PHB_ITEM pString = hb_arrayGetItemPtr( pArgs, 2 );

      if( pString->type == HB_IT_STRING )
      {
         pFunc = (PHB_FUNC) hb_objHasMsg( pPointer, pString->item.asString.value );
      }
      else if( pString->type == HB_IT_LONG )
      {
         pFunc = (PHB_FUNC) hb_itemGetNL( pString );
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "HB_ObjMsgPtr", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         return;
      }

      pExecSym = hb_clsSymbolFromFunction( pPointer , pFunc );

      if( pExecSym == NULL )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "StartThread", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         hb_gcUnlock( pt->pArgs->item.asArray.value );
         hb_itemRelease( pArgs );
         return;
      }

      bIsMethod = TRUE;

      /* Now we must move the object in the second place */
      hb_itemSwap( pPointer, hb_arrayGetItemPtr( pArgs, 2 ) );

      pPointer->type = HB_IT_SYMBOL;
      pPointer->item.asSymbol.value = pExecSym->pSymbol;
   }
   /* Is it a function name? */
   else if( pPointer->type == HB_IT_STRING )
   {
      pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pPointer ) );

      if( ! pExecSym )
      {
         hb_errRT_BASE( EG_NOFUNC, 1001, NULL, hb_itemGetCPtr( pPointer ), 1, pArgs );
         hb_gcUnlock( pt->pArgs->item.asArray.value );
         hb_itemRelease( pArgs );
         return;
      }

      pPointer->type = HB_IT_SYMBOL;
      pPointer->item.asSymbol.value = pExecSym->pSymbol;
   }
   /* Is it a code block? */
   else if( pPointer->type != HB_IT_BLOCK )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STARTTHREAD", 1, pArgs );
      hb_gcUnlock( pt->pArgs->item.asArray.value );
      hb_itemRelease( pArgs );
      return;
   }

   pt = (HB_THREAD_PARAM *) malloc( sizeof( HB_THREAD_PARAM ) );
   pt->pArgs = pArgs;
   pt->uiCount = hb_pcount();
   pt->bIsMethod = bIsMethod;

#if defined(HB_OS_WIN_32)
   /* creates a thread, but don't start it */
   if( ( th_h = CreateThread( NULL, 0, hb_create_a_thread, (LPVOID) pt, CREATE_SUSPENDED, &th_id ) ) != NULL )
   {
      HB_THREAD_CONTEXT *context = hb_threadCreateContext( th_id );
      context->th_h = th_h;
      /* now the thread can start */
      ResumeThread( th_h );
      hb_retnl( (long) th_id );
   }
#else
   if( pthread_create( &th_id, NULL, hb_create_a_thread, (void * ) pt ) == 0 )
   {
      hb_retnl( (long) th_id );
   }
#endif
   else
   {
      hb_retnl( -1 );
   }
}

HB_FUNC( STOPTHREAD )
{
   HB_MUTEX_STRUCT *Mutex;
   HB_THREAD_T th;
   PHB_ITEM pMutex;
#ifdef HB_OS_WIN_32
   HB_THREAD_CONTEXT *context;
#endif

   th = (HB_THREAD_T) hb_parnl( 1 );
   pMutex = hb_param( 2, HB_IT_STRING );


   if( ! ISNUM( 1 ) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STOPTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   #if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
      pthread_cancel( th );
      /* Notify mutex before to join */
      if( pMutex != NULL )
      {
         Mutex = (HB_MUTEX_STRUCT *)  pMutex->item.asString.value;

         while( Mutex->waiting  > 0)
         {
               HB_COND_SIGNAL( Mutex->cond );
               Mutex->waiting--;
         }
      }
      pthread_join( th, 0 );
   #else

      context = hb_threadGetContext( th );
      /* TODO: error checking here */

      TerminateThread( context->th_h, 0);

      /* Notify mutex before to join */
      if( pMutex != NULL )
      {
         Mutex = (HB_MUTEX_STRUCT *)  pMutex->item.asString.value;

         while( Mutex->waiting  > 0)
         {
               HB_COND_SIGNAL( Mutex->cond );
               Mutex->waiting--;
         }
      }
      WaitForSingleObject( context->th_h, INFINITE );
      CloseHandle( context->th_h );
   #endif

   hb_threadDestroyContext( th );
}

HB_FUNC( KILLTHREAD )
{
   HB_THREAD_T th = (HB_THREAD_T) hb_parnl( 1 );
#ifdef HB_OS_WIN_32
   HB_THREAD_CONTEXT *context;
#endif

   if( ! ISNUM( 1 ) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "KILLTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   #if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
      pthread_cancel( th );
   #else
      context = hb_threadGetContext( th );
      TerminateThread( context->th_h, 0);
      CloseHandle( context->th_h );
   #endif
}

HB_FUNC( CLEARTHREAD )
{
   HB_THREAD_T th;

   if( ! ISNUM( 1 ) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "CLEARTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   th = (HB_THREAD_T) hb_parnl( 1 );

   hb_threadDestroyContext( th );
}

HB_FUNC( JOINTHREAD )
{
   HB_THREAD_T  th;
   int result;
#ifdef HB_OS_WIN_32
   HB_THREAD_CONTEXT *context;
#endif

   if( ! ISNUM( 1 ) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "KILLTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   th = (HB_THREAD_T) hb_parnl( 1 );

   #if ! defined( HB_OS_WIN_32 )
      result = pthread_join( th, 0 );
   #else
      context = hb_threadGetContext( th );
      result = (int) WaitForSingleObject( context->th_h, INFINITE );
   #endif
}

HB_FUNC( CREATEMUTEX )
{
   HB_MUTEX_STRUCT *mt = (HB_MUTEX_STRUCT *) hb_xgrab( sizeof( HB_MUTEX_STRUCT ) );

   HB_MUTEX_INIT( mt->mutex );
   HB_COND_INIT( mt->cond );

   mt->lock_count = 0;
   mt->waiting = 0;
   mt->locker = 0;
   mt->aEventObjects = hb_itemArrayNew( 0 );

   hb_retclenAdoptRaw( (char *) mt, sizeof( HB_MUTEX_STRUCT ) );
}

HB_FUNC( DESTROYMUTEX )
{
   HB_MUTEX_STRUCT *Mutex;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );

   if( pMutex == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "DESTROYMUTEX", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   Mutex = (HB_MUTEX_STRUCT *)  pMutex->item.asString.value;

   while( Mutex->lock_count )
   {
      HB_MUTEX_UNLOCK( Mutex->mutex );
      Mutex->lock_count--;
   }

   HB_MUTEX_DESTROY( Mutex->mutex );
   HB_COND_DESTROY( Mutex->cond );
   hb_itemRelease( Mutex->aEventObjects );
}

HB_FUNC( MUTEXLOCK )
{
   HB_MUTEX_STRUCT *Mutex;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );

   if( pMutex == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXLOCK", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   Mutex = (HB_MUTEX_STRUCT *) pMutex->item.asString.value;

   if( Mutex->locker == HB_CURRENT_THREAD() )
   {
      Mutex->lock_count ++;
   }
   else
   {
      HB_MUTEX_LOCK( Mutex->mutex );

      Mutex->locker = HB_CURRENT_THREAD();
      Mutex->lock_count = 1;
   }
}

HB_FUNC( MUTEXUNLOCK )
{
   HB_MUTEX_STRUCT *Mutex;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );

   if( pMutex == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXUNLOCK", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   Mutex = (HB_MUTEX_STRUCT *) pMutex->item.asString.value;

   if( Mutex->locker == HB_CURRENT_THREAD() )
   {
      Mutex->lock_count --;

      if( Mutex->lock_count == 0 )
      {
         Mutex->locker = 0;
         HB_MUTEX_UNLOCK( Mutex->mutex );
      }
   }
}

HB_FUNC( SUBSCRIBE )
{
   int lc, iWaitRes;
   int islocked;
   PHB_ITEM pNotifyVal;


   HB_MUTEX_STRUCT *Mutex;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pStatus = hb_param( 3, HB_IT_BYREF );

   /* Parameter error checking */
   if( pMutex == NULL || ( hb_pcount() == 2 && ! ISNUM(2)) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "SUBSCRIBE", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   Mutex = (HB_MUTEX_STRUCT *) pMutex->item.asString.value;

   if( Mutex->locker != HB_CURRENT_THREAD() )
   {
      islocked = 0;
      HB_MUTEX_LOCK( Mutex->mutex );
   }
   else
   {
      islocked = 1;
   }

   Mutex->locker = 0;
   lc = Mutex->lock_count;
   Mutex->lock_count = 0;

   Mutex->waiting ++;

   iWaitRes = 0; /* presuming success */
   if( Mutex->waiting > 0 )
   {
      if ( hb_pcount() == 1 )
      {
         HB_COND_WAIT( Mutex->cond, Mutex->mutex );
      }
      else
      {
         iWaitRes = HB_COND_WAITTIME( Mutex->cond, Mutex->mutex, hb_parnl( 2 ));
         /* On success, the notify will decrease the counter */
         if ( iWaitRes != 0 )
         {
            Mutex->waiting --;
         }
      }
   }

   // Prepare return value
   Mutex->lock_count = lc;

   if( ! islocked )
   {
      HB_MUTEX_UNLOCK( Mutex->mutex );
   }
   else
   {
      Mutex->locker = HB_CURRENT_THREAD();
      Mutex->locker = 0;
   }

   if ( iWaitRes == 0 )
   {
      if ( pStatus )
      {
         pStatus->type = HB_IT_LOGICAL;
         pStatus->item.asLogical.value = 1;
      }
      pNotifyVal = hb_arrayGetItemPtr( Mutex->aEventObjects, 1 );
      hb_itemReturn( pNotifyVal );
      hb_arrayDel( Mutex->aEventObjects, 1 );
      hb_arraySize( Mutex->aEventObjects, hb_arrayLen( Mutex->aEventObjects) - 1);
   }
   else
   {
      if ( pStatus )
      {
         pStatus->type = HB_IT_LOGICAL;
         pStatus->item.asLogical.value = 0;
      }
      hb_ret();
   }
}

HB_FUNC( SUBSCRIBENOW )
{
   int lc, iWaitRes;
   int islocked;
   HB_MUTEX_STRUCT *Mutex;
   PHB_ITEM pNotifyVal;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pStatus = hb_param( 3, HB_IT_BYREF );

   /* Parameter error checking */
   if( pMutex == NULL || ( hb_pcount() == 2 && ! ISNUM(2)) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "SUBSCRIBENOW", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Mutex = (HB_MUTEX_STRUCT *) pMutex->item.asString.value;

   if( Mutex->locker != HB_CURRENT_THREAD() )
   {
      islocked = 0;
      HB_MUTEX_LOCK( Mutex->mutex );
   }
   else
   {
      islocked = 1;
   }

   Mutex->locker = 0;
   lc = Mutex->lock_count;
   Mutex->lock_count = 0;

   /* Destroying previous notify objects */
   if( Mutex->waiting <= 0 )
   {
      hb_arraySize( Mutex->aEventObjects, 0 );
      Mutex->waiting = 1;
   }
   else
   {
      Mutex->waiting++;
   }

   iWaitRes = 0;
   if( hb_pcount() == 1 )
   {
      HB_COND_WAIT( Mutex->cond, Mutex->mutex );
   }
   else
   {
      iWaitRes = HB_COND_WAITTIME( Mutex->cond, Mutex->mutex, hb_parnl( 2 ) );
      /* On success, the notify will decrease the counter */
      if ( iWaitRes != 0 )
      {
         Mutex->waiting --;
      }
   }

   /* Prepare return value */
   Mutex->lock_count = lc;

   if( ! islocked )
   {
      HB_MUTEX_UNLOCK( Mutex->mutex );
      Mutex->locker = 0;
   }
   else
   {
      Mutex->locker = HB_CURRENT_THREAD();
   }

   if ( iWaitRes == 0 )
   {
      if ( pStatus )
      {
         pStatus->type = HB_IT_LOGICAL;
         pStatus->item.asLogical.value = 1;
      }
      pNotifyVal = hb_arrayGetItemPtr( Mutex->aEventObjects, 1 );
      hb_itemReturn( pNotifyVal );
      hb_arrayDel( Mutex->aEventObjects, 1 );
      hb_arraySize( Mutex->aEventObjects, hb_arrayLen( Mutex->aEventObjects) - 1);
   }
   else
   {
      if ( pStatus )
      {
         pStatus->type = HB_IT_LOGICAL;
         pStatus->item.asLogical.value = 0;
      }
      hb_ret();
   }
}

HB_FUNC( NOTIFY )
{
   HB_MUTEX_STRUCT *Mutex;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pVal = hb_param( 2, HB_IT_ANY );

   /* Parameter error checking */
   if( pMutex == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "NOTIFY", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Mutex = (HB_MUTEX_STRUCT *) pMutex->item.asString.value;


   if ( pVal == NULL )
   {
      pVal = hb_itemNew( NULL );
   }

   Mutex->waiting--;
   hb_arrayAdd( Mutex->aEventObjects, pVal );
   HB_COND_SIGNAL( Mutex->cond );
}

HB_FUNC( NOTIFYALL )
{
   HB_MUTEX_STRUCT *Mutex;
   int iWt;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pVal = hb_param( 2, HB_IT_ANY );

   /* Parameter error checking */
   if( pMutex == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "NOTIFYALL", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Mutex = (HB_MUTEX_STRUCT *) pMutex->item.asString.value;

   if ( pVal == NULL )
   {
      pVal = hb_itemNew( NULL );
   }

   for( iWt = 0; iWt < Mutex->waiting; iWt++ )
   {
      hb_arrayAdd( Mutex->aEventObjects, pVal );
   }

   while( Mutex->waiting > 0 )
   {
      Mutex->waiting --;
      HB_COND_SIGNAL( Mutex->cond );
   }
}

HB_FUNC( THREADSLEEP )
{
   if( ! ISNUM( 1 ) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "THREADSLEEP", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   #if defined( HB_OS_DARWIN )
      usleep( hb_parni( 1 ) * 1000 );
   #elif defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
      {
         struct timespec ts;
         ts.tv_sec = hb_parni( 1 ) / 1000;
         ts.tv_nsec = (hb_parni( 1 ) % 1000) * 1000000;
         nanosleep( &ts, 0 );
      }
   #else
      Sleep( hb_parni( 1 ) );
   #endif
}

HB_FUNC( THREADGETCURRENT )
{
   hb_retnl( (long) HB_CURRENT_THREAD() );
}

HB_FUNC( WAITFORTHREADS )
{
   while( hb_ht_context )
   {
      #if defined(HB_OS_WIN_32)
         Sleep( 1 );
      #elif defined(HB_OS_DARWIN)
         usleep( 1 );
      #else
         static struct timespec nanosecs = { 0, 1000 };
         nanosleep( &nanosecs, NULL );
      #endif
   }
}

/*
JC: I am leaving this in the source code for now; you can never know, this could
be useful in the future.
*/
#if defined( HB_OS_WIN_32 )
DWORD hb_SignalObjectAndWait( HB_COND_T hToSignal, HB_MUTEX_T hToWaitFor, DWORD dwMillisec, BOOL bUnused )
{
   HB_SYMBOL_UNUSED( bUnused );

   ReleaseMutex( hToSignal );
   /* return 0 on success like unix functions */
   return ( WaitForSingleObject( hToWaitFor, dwMillisec ) != WAIT_OBJECT_0);
}
#else
int hb_condTimeWait( pthread_cond_t *cond, pthread_mutex_t *mutex, int iMillisec )
{
   struct timeval now;
   struct timespec timeout;
   gettimeofday( &now, NULL );
   timeout.tv_nsec = (now.tv_usec + ( (iMillisec % 1000l) * 1000l ) )* 1000l;
   timeout.tv_sec = now.tv_sec + (iMillisec / 1000l) + timeout.tv_nsec / 1000000000l;
   timeout.tv_nsec %= 1000000000l;
   return pthread_cond_timedwait( cond, mutex, &timeout );
}
#endif
/*
JC1: This should be reactivated if we want flat mutex

void hb_threadLock( HB_LWR_MUTEX *lpMutex )
{
   if ( lpMutex->Locker == HB_CURRENT_THREAD() )
   {
      lpMutex->nCount++;
   }
   else
   {
      HB_CRITICAL_LOCK( lpMutex->Critical );
      lpMutex->nCount = 1;
      lpMutex->Locker = HB_CURRENT_THREAD();
   }
}

void hb_threadUnlock( HB_LWR_MUTEX *lpMutex )
{
   if ( lpMutex->Locker == HB_CURRENT_THREAD() )
   {
      lpMutex->nCount--;

      if ( lpMutex->nCount == 0 )
      {
         lpMutex->Locker = 0;
         HB_CRITICAL_UNLOCK( lpMutex->Critical );
      }
   }
}
*/
/*****************************************************/
/* Forbid mutex management                           */

void hb_threadForbidenInit( HB_FORBID_MUTEX *Forbid )
{
   HB_CRITICAL_INIT( Forbid->Control );
   Forbid->lCount = 0;
}

void hb_threadForbidenDestroy( HB_FORBID_MUTEX *Forbid )
{
   HB_CRITICAL_DESTROY( Forbid->Control );
}

void hb_threadForbid( HB_FORBID_MUTEX *Forbid )
{
   /* Request for control section */
   HB_CRITICAL_LOCK( Forbid->Control );

   Forbid->lCount++;

   /* Now we can release the control section */
   HB_CRITICAL_UNLOCK( Forbid->Control );
}


void hb_threadAllow( HB_FORBID_MUTEX *Forbid )
{
   /* Request for control section */
   HB_CRITICAL_LOCK( Forbid->Control );

   Forbid->lCount --;

   /* Now we can release the control section */
   HB_CRITICAL_UNLOCK( Forbid->Control );
}
#endif
