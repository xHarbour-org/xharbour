/*
* $Id: thread.c,v 1.76 2003/05/16 19:52:12 druzus Exp $
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
* hb_itemClear() and hb_itemCopy() are derivative work of original code
* in the Harbour Project http://harbour-project.org (source/vm/itemapi.c)
* Copyright of Antonio Linares <alinares@fivetech.com>
*
*/

/* JC1: pretty useless to even include files if thread is off */
#ifdef HB_THREAD_SUPPORT

#define HB_THREAD_OPTIMIZE_STACK

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
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"
#ifdef HB_OS_WIN_32
#define extern
#endif
#include "thread.h"
#ifdef HB_OS_WIN_32
#undef extern
#endif

/* Creating a trylock for systems that have to use LWR */
#if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )

/* To be honest, this implementation is far from being thread safe;
   TODO: better implementation, but I argue it will ALWAYS be better
   to implement system specific solution, where available.
*/

static HB_CRITICAL_T s_mtxTryLock;

BOOL hb_critical_trylock( HB_CRITICAL_T *lpMutex )
{
   HB_CRITICAL_LOCK( s_mtxTryLock );
   if ( lpMutex->Locker == HB_CURRENT_THREAD() )
   {
      lpMutex->nCount++;
      HB_CRITICAL_UNLOCK( s_mtxTryLock );
      return TRUE;
   }
   else
   {
      if ( lpMutex->nCount > 0 )
      {
         HB_CRITICAL_UNLOCK( s_mtxTryLock );
         return FALSE;
      }
      HB_CRITICAL_LOCK( lpMutex->Critical );
      lpMutex->nCount = 1;
      lpMutex->Locker = HB_CURRENT_THREAD();
      HB_CRITICAL_UNLOCK( s_mtxTryLock );
   }
}

#endif  //LWR mutexes for OS without them.

HB_STACK *hb_ht_stack;
HB_STACK *last_stack;
HB_MUTEX_STRUCT *hb_ht_mutex;
HB_THREAD_T hb_main_thread_id;

static HB_CRITICAL_T s_thread_unique_id_mutex;
static UINT s_thread_unique_id;

#ifdef HB_OS_WIN_32
   DWORD hb_dwCurrentStack;
#else
   pthread_key_t hb_pkCurrentStack;
#endif

/* Declarations of shell mutexes */
HB_CRITICAL_T hb_threadStackMutex;
HB_CRITICAL_T hb_globalsMutex;
HB_CRITICAL_T hb_staticsMutex;
HB_CRITICAL_T hb_memvarsMutex;
HB_CRITICAL_T hb_macroMutex;
HB_CRITICAL_T hb_allocMutex;
HB_CRITICAL_T hb_garbageAllocMutex;
HB_CRITICAL_T hb_outputMutex;
HB_CRITICAL_T hb_mutexMutex;
HB_CRITICAL_T hb_cancelMutex;
HB_CRITICAL_T hb_fenceMutex;

HB_SHARED_RESOURCE hb_runningStacks;
#ifdef HB_OS_WIN_32
   HB_SHARED_RESOURCE hb_idleQueueRes;
#endif

BOOL hb_bIdleFence;

static UINT hb_threadUniqueId()
{
   volatile UINT uiRet;
   HB_CRITICAL_LOCK( s_thread_unique_id_mutex );
   if ( s_thread_unique_id == HB_THREAD_MAX_UNIQUE_ID )
   {
      s_thread_unique_id = 1;
   }
   s_thread_unique_id++;
   uiRet = s_thread_unique_id;
   HB_CRITICAL_UNLOCK( s_thread_unique_id_mutex );

   return uiRet;
}

HB_STACK *hb_threadCreateStack( HB_THREAD_T th )
{
   HB_STACK *tc;
   int i;

#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_LOCK( hb_allocMutex );
#endif
   tc = (HB_STACK *) malloc( sizeof( HB_STACK));

   tc->th_id = th;
   tc->th_vm_id = hb_threadUniqueId();

   tc->next = NULL;

   tc->pItems = ( HB_ITEM_PTR * ) malloc( sizeof( HB_ITEM_PTR ) * STACK_THREADHB_ITEMS );
   tc->pBase  = tc->pItems;
   tc->pPos   = tc->pItems;     /* points to the first stack item */
   tc->wItems = STACK_THREADHB_ITEMS;
   tc->pMethod = NULL;
   tc->Return.type = HB_IT_NIL;
   tc->bInUse = FALSE;

   tc->errorHandler = NULL;
   tc->errorBlock = hb_itemNew( NULL );
   tc->aTryCatchHandlerStack = hb_itemNew( NULL );
   hb_arrayNew( tc->aTryCatchHandlerStack, 0 );
   hb_gcLock(  tc->aTryCatchHandlerStack );
   tc->iLaunchCount = 0;
   tc->uiErrorDOS = 0;

   #ifdef HB_OS_WIN_32
      tc->th_h = NULL;
      tc->bCanceled = FALSE;
      tc->bCanCancel = FALSE;
      /*
      tc->iCleanCount = 0;
      tc->pCleanUp = (HB_CLEANUP_FUNC *) malloc( sizeof( HB_CLEANUP_FUNC ) * HB_MAX_CLEANUPS );
      tc->pCleanUpParam = (void **) malloc( sizeof( void *) * HB_MAX_CLEANUPS );
      */
   #endif

   for( i = 0; i < tc->wItems; i++ )
   {
      tc->pItems[ i ] = (HB_ITEM *) malloc( sizeof( HB_ITEM ) );
   }
   ( * (tc->pPos) )->type = HB_IT_NIL;

#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_UNLOCK( hb_allocMutex );
#endif

   return tc;
}

/*JC1: WARNING
* this function is not locked because is meant to be called ONLY within
* a thread stack locked area.
*/
HB_STACK *hb_threadLinkStack( HB_STACK *tc )
{
   HB_STACK *p;

   p = hb_ht_stack;

   while( p->next )
   {
      p = p->next;
   }

   p->next = tc;

   //last_stack = p;

   return tc;
}


/*JC1: WARNING
* this function is not locked because is meant to be called ONLY within
* a thread stack locked area.
*/
HB_STACK *hb_threadUnlinkStack( HB_STACK* pStack )
{
   HB_STACK *p, *prev;

   if ( hb_ht_stack == NULL )
   {
      return NULL;
   }

   /* never unlinks the main stack */
   prev = hb_ht_stack;
   p = hb_ht_stack->next;

   while ( p && p != pStack )
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
         hb_ht_stack = p->next;
      }
   }
   else
   {
      char errdat[64];

      sprintf( errdat, "Stack not found for Thread %ld",  (long) pStack->th_id );
      hb_errRT_BASE_SubstR( EG_CORRUPTION, 10001, errdat, "hb_threadUnlinkStack", 0 );
   }

   return p;
}

/* DEBUG FUNCTION */

int hb_threadCountStacks()
{
   HB_STACK *p;
   int count = 0;

   /* never unlinks the main stack */
   HB_CRITICAL_LOCK( hb_threadStackMutex );

   p = hb_ht_stack;
   while ( p )
   {
      count ++;
      p = p->next;
   }
   HB_CRITICAL_UNLOCK( hb_threadStackMutex );

   return count;
}

HB_FUNC( HB_THREADGETTRYERRORARRAY )
{
   HB_THREAD_STUB
   hb_itemCopy( &(HB_VM_STACK.Return), HB_VM_STACK.aTryCatchHandlerStack );
}

HB_FUNC( HB_THREADCOUNTSTACKS )
{
   HB_THREAD_STUB
   hb_retni( hb_threadCountStacks() );
}

void hb_threadDestroyStack( HB_STACK *pStack )
{
   int i;
   PHB_ITEM *pPos;

   /* Refuse to destroy the main thread stack: it will be
      deleted by the VM at program termination. */
   if ( pStack == &hb_stack )
   {
      return;
   }

   /* Free each element of the stack */
   for( pPos = pStack->pItems; pPos < pStack->pPos; pPos++)
   {
      if( HB_IS_COMPLEX( *pPos ) )
      {
         hb_itemClear( *pPos );
      }
   }

   /* Error handler is never allocated; it resides in the stack, or
      is owned by callers. */
   if( pStack->errorBlock && pStack->errorBlock->type )
   {
      hb_itemClear( pStack->errorBlock );
   }

   hb_itemClear( pStack->aTryCatchHandlerStack );

   /* Free each element of the stack */
#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_LOCK( hb_allocMutex );
#endif
   for( i = 0; i < pStack->wItems; i++ )
   {
      free( pStack->pItems[ i ] );
   }
   /* Free the stack */

   free( pStack->pItems );
   #ifdef HB_OS_WIN_32
   //free( pStack->pCleanUp );
   //free( pStack->pCleanUpParam );
   #endif
   free( pStack );
#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_UNLOCK( hb_allocMutex );
#endif

   // Call destructor
   /*
   if( pStack->pDestructor )
   {
      pStack->pDestructor( (void *) p );
   }*/

}

HB_STACK *hb_threadGetStack( HB_THREAD_T id )
{
   HB_STACK *p;

   HB_CRITICAL_LOCK( hb_threadStackMutex );

   if( last_stack && last_stack->th_id == id )
   {
      p = last_stack;
   }
   else {

      p = hb_ht_stack;

      while( p && p->th_id != id )
      {
         p = p->next;
      }

      if( p )
      {
         last_stack = p;
      }

   }

   HB_CRITICAL_UNLOCK( hb_threadStackMutex );
   return p;
}

/* to be internally used byt functions willing to know if there is a stack */
HB_STACK *hb_threadGetStackNoError( HB_THREAD_T id )
{
   HB_STACK *p;

   HB_CRITICAL_LOCK( hb_threadStackMutex );

   if( last_stack && last_stack->th_id == id )
   {
      p = last_stack;
   }
   else {

      p = hb_ht_stack;

      while( p && p->th_id != id )
      {
         p = p->next;
      }

      if( p )
      {
         last_stack = p;
      }
   }

   HB_CRITICAL_UNLOCK( hb_threadStackMutex );
   return p;
}


/* This function is meant to be called in a protected environment */
void hb_threadFillStack( HB_STACK *pStack, PHB_ITEM pArgs )
{
   PHB_ITEM pPointer;
   PHB_ITEM pItem, *pPos;
   USHORT uiParam = 1;

   pPos = pStack->pPos;

   pPointer = hb_arrayGetItemPtr( pArgs, 1 );

   if( HB_IS_SYMBOL( pPointer ) )
   {
      (*pPos)->type = HB_IT_SYMBOL;
      (*pPos)->item.asSymbol.value = pPointer->item.asSymbol.value;
      (*pPos)->item.asSymbol.stackbase = pPos - pStack->pItems;
      pPos++;
      (*pPos)->type = HB_IT_NIL;

      if( pStack->bIsMethod )
      {
         pItem = hb_arrayGetItemPtr( pArgs, 2 );
         hb_itemCopy( (*pPos), pItem );
         uiParam = 3;
      }
      else
      {
         (*pPos)->type = HB_IT_NIL;
         uiParam = 2;
      }
      pPos++;
      (*pPos)->type = HB_IT_NIL;
   }
   else if( HB_IS_BLOCK( pPointer ) )
   {
      (*pPos)->type = HB_IT_SYMBOL;
      (*pPos)->item.asSymbol.value = &hb_symEval;
      (*pPos)->item.asSymbol.stackbase = pPos - pStack->pItems;
      pPos++;
      (*pPos)->type = HB_IT_NIL;
      hb_itemCopy( (*pPos), pPointer );
      pPos++;
      (*pPos)->type = HB_IT_NIL;
      uiParam = 2;
   }

   for( ; uiParam <=(USHORT) pStack->uiParams; uiParam++ )
   {
      hb_itemCopy( (*pPos), hb_arrayGetItemPtr( pArgs, uiParam ) );
      pPos++;
      (*pPos)->type = HB_IT_NIL;
   }
   pStack->pPos = pPos;

   hb_itemRelease( pArgs );
}


HB_MUTEX_STRUCT *hb_threadLinkMutex( HB_MUTEX_STRUCT *mx )
{
   HB_MUTEX_STRUCT *p;

   HB_CRITICAL_LOCK( hb_mutexMutex );
   if ( hb_ht_mutex == NULL )
   {
      hb_ht_mutex = mx;
      HB_CRITICAL_UNLOCK( hb_mutexMutex );
      return mx;
   }
      
   p = hb_ht_mutex;

   while( p->next )
   {
      p = p->next;
   }

   p->next = mx;
   
   HB_CRITICAL_UNLOCK( hb_mutexMutex );

   return mx;
}


HB_MUTEX_STRUCT *hb_threadUnlinkMutex( HB_MUTEX_STRUCT *pMtx )
{
   HB_MUTEX_STRUCT *p, *prev;

   HB_CRITICAL_LOCK( hb_mutexMutex );
   
   if ( hb_ht_mutex == NULL )
   {
      HB_CRITICAL_UNLOCK( hb_mutexMutex );
      return NULL;
   }

   p = hb_ht_mutex;
   prev = NULL;

   while ( p && p != pMtx )
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
         hb_ht_mutex = p->next;
      }
   }

   if( p->lock_count )
   {
      HB_MUTEX_UNLOCK( p->mutex );
   }

   HB_CRITICAL_UNLOCK( hb_mutexMutex );

   return p;
}

#ifdef HB_OS_WIN_32
/* should be called by the only thread running */

void hb_threadSuspendAll()
{
   HB_THREAD_T th_id = HB_CURRENT_THREAD();
   HB_STACK *pStack = hb_ht_stack;

   while ( pStack != NULL )
   {
      if ( pStack->th_id != th_id )
      {
         SuspendThread( pStack->th_h );
      }
      pStack = pStack->next;
   }
}

void hb_threadResumeAll()
{
   HB_THREAD_T th_id = HB_CURRENT_THREAD();
   HB_STACK *pStack = hb_ht_stack;

   while ( pStack != NULL )
   {
      if ( pStack->th_id != th_id )
      {
         ResumeThread( pStack->th_h );
      }
      pStack = pStack->next;
   }
}

void hb_threadCallIdle()
{
   HB_IDLE_FUNC_LIST *pIdle;
   HB_CRITICAL_LOCK( hb_idleQueueRes.Mutex );   

   pIdle = (HB_IDLE_FUNC_LIST *)hb_idleQueueRes.content.asPointer;
   while( pIdle != NULL )
   {
      pIdle->func();
      pIdle = pIdle->next;
   }
   
   /* Begin to signal waiting threads */
   pIdle = (HB_IDLE_FUNC_LIST*)hb_idleQueueRes.content.asPointer;
   if ( pIdle != NULL )
   {
      HB_COND_SIGNAL( hb_idleQueueRes.Cond );
      hb_idleQueueRes.content.asPointer = (void *) pIdle->next;
      hb_xfree( pIdle );
   }

   HB_CRITICAL_UNLOCK( hb_idleQueueRes.Mutex );
}

void hb_threadSubscribeIdle( HB_IDLE_FUNC pFunc )
{
   HB_IDLE_FUNC_LIST *pIdle, *pBeg;
   HB_THREAD_STUB

   /* Executes immediately if no other thread is running */
   HB_CRITICAL_LOCK( hb_runningStacks.Mutex );
   if ( hb_runningStacks.content.asLong == 1 )
   {
      pFunc();
      HB_CRITICAL_UNLOCK( hb_runningStacks.Mutex );
      return;
   }
   HB_CRITICAL_UNLOCK( hb_runningStacks.Mutex );

   pIdle = (HB_IDLE_FUNC_LIST *) hb_xgrab( sizeof( HB_IDLE_FUNC_LIST ) );
   pIdle->func = pFunc;
   pIdle->next = NULL;
   
   HB_CRITICAL_LOCK( hb_idleQueueRes.Mutex );

   if ( hb_idleQueueRes.content.asPointer == NULL )
   {
      hb_idleQueueRes.content.asPointer = (void *) pIdle;
   }
   else
   {
      pBeg = (HB_IDLE_FUNC_LIST*)hb_idleQueueRes.content.asPointer;
      while( pBeg->next != NULL )
      {
         pBeg = pBeg->next;
      }
      pBeg->next = pIdle;
   }
   HB_CRITICAL_UNLOCK( hb_idleQueueRes.Mutex );
   HB_STACK_UNLOCK;

   // here we can allow cancellation
   HB_TEST_CANCEL_ENABLE_ASYN;
   WaitForSingleObject( hb_idleQueueRes.Cond, INFINITE );
   HB_DISABLE_ASYN_CANC;
   
   HB_STACK_LOCK;
   HB_CRITICAL_LOCK( hb_idleQueueRes.Mutex );
      
   /* Continue to signal waiting threaeds */
   pIdle = (HB_IDLE_FUNC_LIST *)hb_idleQueueRes.content.asPointer;
   if ( pIdle != NULL )
   {
      HB_COND_SIGNAL( hb_idleQueueRes.Cond );
      hb_idleQueueRes.content.asPointer = (void *) pIdle->next;
      hb_xfree( pIdle );
   }

   HB_CRITICAL_UNLOCK( hb_idleQueueRes.Mutex );
}

void hb_threadCancelInternal( )
{
   HB_THREAD_STUB

   /* Make sure we are not going to be canceled */
   HB_DISABLE_ASYN_CANC;

/*
   iCount = HB_VM_STACK.iCleanCount;
   while ( iCount > 0 )
   {
      iCount--;
      HB_VM_STACK.pCleanUp[ iCount ]( HB_VM_STACK.pCleanUpParam[ iCount ]);
   }
   // the stack must have been destroyed by the last cleanup function
*/
   hb_threadTerminator( &HB_VM_STACK );
   ExitThread( 0 );
}

/***
* Warning: cancel mutex must be held before calling this one
* NEVER use this function to cancel the calling thread
*/
void hb_threadCancel( HB_STACK *pStack )
{

   /*
   previous section using kind cancellation
   CONTEXT context;
   // stack resource mutex is being locked now
   pStack->bInUse = TRUE;  // mark the stack as used
   SuspendThread( pStack->th_h ); // stop thread before he can do something with stack

   context.ContextFlags = CONTEXT_CONTROL;
   GetThreadContext(  pStack->th_h , &context);
   // _x86 only!!!
   context.Eip = (DWORD)hb_threadCancelInternal;
   SetThreadContext(  pStack->th_h , &context);
   ResumeThread(  pStack->th_h );
   HB_CRITICAL_UNLOCK( hb_cancelMutex );
   */

   TerminateThread( pStack->th_h, 0 );
   hb_threadTerminator( (void *)pStack );
   HB_CRITICAL_UNLOCK( hb_cancelMutex );

}

#endif

void hb_threadTerminator( void *pData )
{
#ifdef HB_OS_WIN_32
   HB_STACK *_pStack_ = (HB_STACK *) pData;
#else
   HB_STACK *_pStack_ = pthread_getspecific( hb_pkCurrentStack );
#endif

   HB_MUTEX_STRUCT *pMtx;

#ifndef HB_OS_WIN_32
   pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, NULL );
#endif

   HB_STACK_LOCK;

   HB_CRITICAL_LOCK( hb_threadStackMutex );
   /* now we can detach this thread */
   hb_threadUnlinkStack( _pStack_ );
   HB_CRITICAL_UNLOCK( hb_threadStackMutex );

   #ifdef HB_OS_WIN_32
      CloseHandle( _pStack_->th_h );
   #else
      pthread_detach( HB_CURRENT_THREAD() );
   #endif

   /* eventually unlocks held mutexes */
   HB_CRITICAL_LOCK( hb_mutexMutex );
   pMtx = hb_ht_mutex;
   while( pMtx != NULL )
   {
      if ( pMtx->locker == _pStack_->th_id )
      {
         hb_mutexForceUnlock( pMtx );
      }
      pMtx = pMtx->next;
   }
   HB_CRITICAL_UNLOCK( hb_mutexMutex );

   hb_threadDestroyStack( _pStack_ );
   
   /* we are out of business */
   HB_CRITICAL_LOCK( hb_runningStacks.Mutex );
   hb_runningStacks.content.asLong--;
   HB_COND_SIGNAL( hb_runningStacks.Cond );
   HB_CRITICAL_UNLOCK( hb_runningStacks.Mutex );
}

void hb_mutexForceUnlock( void *mtx )
{
   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) mtx;
   Mutex->locker = 0;
   Mutex->lock_count = 0;
   HB_MUTEX_UNLOCK( Mutex->mutex );
}

void hb_rawMutexForceUnlock( void * mtx )
{
   HB_CRITICAL_T *Mutex = (HB_CRITICAL_T *) mtx;
   HB_CRITICAL_UNLOCK( *Mutex );
}


#ifdef HB_OS_WIN_32
   DWORD WINAPI hb_create_a_thread( LPVOID Cargo )
#else
   void *hb_create_a_thread( void *Cargo )
#endif
{
   volatile HB_STACK *_pStack_ = (HB_STACK *) Cargo;
   PHB_DYNS pExecSym;

   /* Sets the cancellation handler so small delays in
   cancellation do not cause segfault or memory leaks */
#ifdef HB_OS_WIN_32
   TlsSetValue( hb_dwCurrentStack, ( void * ) _pStack_ );
#else
   _pStack_->th_id = HB_CURRENT_THREAD();
   pthread_setspecific( hb_pkCurrentStack, Cargo );
   pthread_cleanup_push( hb_threadTerminator, NULL );
   /* wait for the father to be done */
   HB_CRITICAL_LOCK( hb_threadStackMutex );
   /* It is not elegant, but the father is holding the mutex
      as long as necessary */
   HB_CRITICAL_UNLOCK( hb_threadStackMutex );
#endif

   // call errorsys() to initialize errorblock
   pExecSym = hb_dynsymFind( "ERRORSYS" );

   if( pExecSym )
   {
      hb_vmPushSymbol( pExecSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo(0);
   }

   if( _pStack_->bIsMethod )
   {
      hb_vmSend( HB_VM_STACK.uiParams - 2 );
   }
   else
   {
      hb_vmDo( HB_VM_STACK.uiParams - 1 );
   }

   #ifdef HB_OS_WIN_32
      hb_threadCancelInternal(); // never returns
      return 0;
   #else
      /* After this points prevents cancellation, so we can have a clean
      quit. else, a cancellation request could be issued inside the
      cleanup pop cycle, causing re-entering in the cleanup functions */
      pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, NULL );
      /* pop cleanup; also calls the cleanup function*/
      pthread_cleanup_pop( 1 );
      return NULL;
   #endif
}

/**
* JC1: **IsLocalRef functions are thread unsafe functions by default,
* because the GC process is a linearized process by hypotesis. NEVER
* call this function from outside GC process, since this function must
* be locked both in GC (outer) and in thread (inner) mutexes
*/
void hb_threadIsLocalRef( void )
{
   HB_STACK *pStack;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsLocalRef()"));

   pStack = hb_ht_stack;

   while( pStack )
   {

      if( pStack->Return.type & (HB_IT_BYREF | HB_IT_POINTER | HB_IT_ARRAY | HB_IT_BLOCK) )
      {
         hb_gcItemRef( &(pStack->Return) );
      }

      if( pStack->pPos > pStack->pItems )
      {
         HB_ITEM_PTR *pItem = pStack->pPos - 1;

         while( pItem != pStack->pItems )
         {
            if( ( *pItem )->type & (HB_IT_BYREF | HB_IT_POINTER | HB_IT_ARRAY | HB_IT_BLOCK) )
            {
               hb_gcItemRef( *pItem );
            }

            --pItem;
         }
      }

      pStack = pStack->next;
   }
}

HB_FUNC( STARTTHREAD )
{
   HB_THREAD_STUB
   
   PHB_ITEM pPointer;
   PHB_ITEM pArgs;
   HB_THREAD_T th_id;
   PHB_DYNS pExecSym;
   PHB_FUNC pFunc;
   BOOL bIsMethod = FALSE;
   HB_STACK *pStack;

#ifdef HB_OS_WIN_32
   HANDLE th_h;
#endif

   pArgs = hb_arrayFromParamsLocked( HB_VM_STACK.pBase );
   pPointer  = hb_arrayGetItemPtr( pArgs, 1 );

   /* Error Checking */
   if( pPointer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STARTTHREAD", 1, pArgs );
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
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "StartThread", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         return;
      }

      pExecSym = hb_clsSymbolFromFunction( pPointer , pFunc );

      if( pExecSym == NULL )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "StartThread", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
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
      hb_itemRelease( pArgs );
      return;
   }

   // Create the stack here to avoid cross locking of alloc mutex
   pStack = hb_threadCreateStack( 0 );
   pStack->uiParams = hb_pcount();
   pStack->bIsMethod = bIsMethod;

   HB_CRITICAL_LOCK( hb_threadStackMutex );
   hb_threadFillStack( pStack, pArgs );
   hb_threadLinkStack( pStack );

   /* Forbid usage of stack before that VM takes care of it */
#if defined(HB_OS_WIN_32)
   if( ( th_h = CreateThread( NULL, 0, hb_create_a_thread, (void *) pStack , CREATE_SUSPENDED, &th_id ) ) != NULL )
#else
   if( pthread_create( &th_id, NULL, hb_create_a_thread, (void *) pStack ) == 0 )
#endif
   {
      // Id is now provided by the called thread, but also here: it will be
      // set by the first that is able to run
      pStack->th_id = th_id;
      /* Under windws, we put the handle after creation */
#if defined(HB_OS_WIN_32)
      pStack->th_h = th_h;
      ResumeThread( th_h );
#endif
      hb_retnl( (long) th_id );
   }
   else
   {
      hb_threadUnlinkStack( pStack );
      hb_threadDestroyStack( pStack );
      hb_retnl( -1 );
   }
   //notice that the child thread won't be able to proceed until we
   // release this mutex.
   HB_CRITICAL_UNLOCK( hb_threadStackMutex );
}

HB_FUNC( STOPTHREAD )
{
   HB_THREAD_STUB

   HB_MUTEX_STRUCT *Mutex;
   HB_THREAD_T th;
   PHB_ITEM pMutex;
#ifdef HB_OS_WIN_32
   HB_STACK *stack;
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
      pthread_join( th, NULL );

   #else
      stack = hb_threadGetStack( th );

      HB_CRITICAL_LOCK( hb_cancelMutex );
      stack->bCanceled = TRUE;
      HB_CRITICAL_UNLOCK( hb_cancelMutex );
      
      HB_TEST_CANCEL_ENABLE_ASYN;
      WaitForSingleObject( stack->th_h, INFINITE );
      HB_DISABLE_ASYN_CANC;   
   #endif

   /* Notify mutex before to leave */
   if( pMutex != NULL )
   {
      Mutex = (HB_MUTEX_STRUCT *)  pMutex->item.asString.value;

      while( Mutex->waiting  > 0)
      {
            HB_COND_SIGNAL( Mutex->cond );
            Mutex->waiting--;
      }
   }

}


HB_FUNC( KILLTHREAD )
{
   HB_THREAD_STUB

   HB_THREAD_T th = (HB_THREAD_T) hb_parnl( 1 );
#ifdef HB_OS_WIN_32
   HB_STACK *stack;
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
      stack = hb_threadGetStack( th );
      /* Shell locking the thread */
      HB_CRITICAL_LOCK( hb_cancelMutex );
      if ( ! stack->bCanCancel )
      {
         stack->bCanceled = TRUE;
         HB_CRITICAL_UNLOCK( hb_cancelMutex );
      }
      else
      {
         hb_threadCancel( stack ); //also unlocks the mutex
      }
   #endif

}


HB_FUNC( JOINTHREAD )
{   
   HB_THREAD_STUB
   
   HB_THREAD_T  th;
#ifdef HB_OS_WIN_32
   HB_STACK *stack;
#endif

   if( ! ISNUM( 1 ) )
   {
      PHB_ITEM pArgs;
      
      pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "JOINTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   th = (HB_THREAD_T) hb_parnl( 1 );

   #if ! defined( HB_OS_WIN_32 )
      HB_STACK_UNLOCK;
      if( pthread_join( th, NULL ) != 0 )
      {
         HB_STACK_LOCK;
         hb_retl( FALSE );
         return;
      }
   #else
      stack = hb_threadGetStackNoError( th );
      if( stack == NULL )
      {
         hb_retl( FALSE );
         return;
      }
      HB_STACK_UNLOCK;
      WaitForSingleObject( stack->th_h, INFINITE );
   #endif
   HB_STACK_LOCK;
   
   hb_retl( TRUE );

}

/*JC1: this will always be called when cancellation is delayed */
HB_FUNC( CREATEMUTEX )
{
   HB_THREAD_STUB
   HB_MUTEX_STRUCT *mt;

   mt = (HB_MUTEX_STRUCT *) hb_xgrab( sizeof( HB_MUTEX_STRUCT ) );

   HB_MUTEX_INIT( mt->mutex );
   HB_COND_INIT( mt->cond );

   mt->lock_count = 0;
   mt->waiting = 0;
   mt->locker = 0;
   mt->aEventObjects = hb_itemArrayNew( 0 );
   mt->next = 0;

   hb_threadLinkMutex( mt );
   
   hb_retclenAdoptRaw( (char *) mt, sizeof( HB_MUTEX_STRUCT ) );
}

/*JC1: this will always be called when cancellation is delayed */
HB_FUNC( DESTROYMUTEX )
{
   HB_THREAD_STUB

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

   hb_threadUnlinkMutex( Mutex );
   
   HB_MUTEX_DESTROY( Mutex->mutex );
   HB_COND_DESTROY( Mutex->cond );
   hb_itemRelease( Mutex->aEventObjects );
}

/*JC1: this will always be called when cancellation is delayed */
HB_FUNC( MUTEXLOCK )
{
   HB_THREAD_STUB
   HB_MUTEX_STRUCT *Mutex;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );

   if( pMutex == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXLOCK", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   /* Cannot be interrupted now */
   Mutex = (HB_MUTEX_STRUCT *) pMutex->item.asString.value;
   if( Mutex->locker == HB_CURRENT_THREAD() )
   {
      Mutex->lock_count ++;
   }
   else
   {  
      HB_STACK_UNLOCK;
      HB_MUTEX_LOCK( Mutex->mutex );
      Mutex->locker = HB_CURRENT_THREAD();
      Mutex->lock_count = 1;
      HB_STACK_LOCK;
   }
}

/*JC1: this will always be called when cancellation is delayed */
HB_FUNC( MUTEXUNLOCK )
{
   HB_THREAD_STUB

   HB_MUTEX_STRUCT *Mutex;
   PHB_ITEM pMutex = hb_param( 1, HB_IT_STRING );

   if( pMutex == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXUNLOCK", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   /* Cannot be interrupted now */

   //HB_CRITICAL_LOCK( hb_mutexMutex );
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
   //HB_CRITICAL_UNLOCK( hb_mutexMutex );
}

/*JC1: this will always be called when cancellation is delayed */
/* TODO: cleanup actions for cancellations while in COND_WAIT */
HB_FUNC( SUBSCRIBE )
{
   HB_THREAD_STUB
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
         HB_STACK_UNLOCK;
         HB_COND_WAIT( Mutex->cond, Mutex->mutex );
         HB_STACK_LOCK;
      }
      else
      {
         HB_STACK_UNLOCK;
         iWaitRes = HB_COND_WAITTIME( Mutex->cond, Mutex->mutex, hb_parnl( 2 ));
         HB_STACK_LOCK;
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

/*JC1: this will always be called when cancellation is delayed */
/* TODO: cleanup actions for cancellations while in COND_WAIT */
HB_FUNC( SUBSCRIBENOW )
{
   HB_THREAD_STUB
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
      HB_STACK_UNLOCK;
      HB_COND_WAIT( Mutex->cond, Mutex->mutex );
      HB_STACK_LOCK;
   }
   else
   {
      HB_STACK_UNLOCK;
      iWaitRes = HB_COND_WAITTIME( Mutex->cond, Mutex->mutex, hb_parnl( 2 ) );
      HB_STACK_LOCK;
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
   HB_THREAD_STUB
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
   HB_THREAD_STUB
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
   HB_THREAD_STUB
   if( ! ISNUM( 1 ) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "THREADSLEEP", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   hb_threadSleep( hb_parni( 1 ) );
}

void hb_threadSleep( int millisec )
{
   HB_THREAD_STUB

   HB_STACK_UNLOCK;

   #if defined( HB_OS_DARWIN )
      usleep( millisec * 1000 );
   #elif defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
      {
         struct timespec ts;
         ts.tv_sec = millisec / 1000;
         ts.tv_nsec = (millisec % 1000) * 1000000;
         nanosleep( &ts, 0 );
      }
   #else
      Sleep( millisec );
   #endif

   HB_STACK_LOCK;
}

HB_FUNC( THREADGETCURRENT )
{
   HB_THREAD_STUB
   hb_retnl( (long) HB_CURRENT_THREAD() );
}

HB_FUNC( THREADGETCURRENTINTERNAL )
{
   HB_THREAD_STUB
   hb_retnl( (long) HB_VM_STACK.th_vm_id );
}

void hb_threadWaitAll()
{
   HB_THREAD_STUB

   // refuse to run if we are not the main thread
   if ( hb_main_thread_id != HB_CURRENT_THREAD() )
   {
      return;
   }

   // check for the main stack to be the only one left
   HB_CRITICAL_LOCK( hb_threadStackMutex );
   while( hb_ht_stack->next != NULL )
   {
      HB_CRITICAL_UNLOCK( hb_threadStackMutex );
      HB_STACK_UNLOCK;
      #if defined(HB_OS_WIN_32)
         Sleep( 1 );
      #elif defined(HB_OS_DARWIN)
         usleep( 1 );
      #else
      {
         static struct timespec nanosecs = { 0, 1000 };
         nanosleep( &nanosecs, NULL );
      }
      #endif
      HB_STACK_LOCK;
      HB_CRITICAL_LOCK( hb_threadStackMutex );
   }
   HB_CRITICAL_UNLOCK( hb_threadStackMutex );
}

void hb_threadKillAll()
{
   HB_STACK *pStack;
   #ifdef HB_OS_WIN_32
      HANDLE th_h;
   #endif

   /* DO NOT destroy main thread stack */
   pStack = hb_ht_stack;
   while( pStack )
   {
      if ( pStack->th_id == hb_main_thread_id || pStack->th_id == HB_CURRENT_THREAD() )
      {
         pStack = pStack->next;
         continue;
      }
      #ifndef HB_OS_WIN_32
         // Allows the target thread to cleanup if and when needed.
         pthread_cancel( pStack->th_id );
      #else
         /* Shell locking the thread */
         HB_CRITICAL_LOCK( hb_cancelMutex );
         if ( ! pStack->bCanCancel )
         {
            pStack->bCanceled = TRUE;
            HB_CRITICAL_UNLOCK( hb_cancelMutex );
         }
         else
         {
            hb_threadCancel( pStack ); // also unlocks the mutex
         }
      #endif
      pStack = pStack->next;
   }
}

HB_FUNC( WAITFORTHREADS )
{
   hb_threadWaitAll();
}


HB_FUNC( KILLALLTHREADS )
{
   hb_threadKillAll();
}


HB_FUNC( THREADIDLEFENCE )
{
   HB_THREAD_STUB
   BOOL bOld;

   HB_CRITICAL_LOCK( hb_runningStacks.Mutex );

   bOld = hb_bIdleFence;

   if ( hb_pcount() == 1 )
   {
      hb_bIdleFence = hb_parl( 1 );
   }
   
   hb_retl( bOld );
   
   HB_CRITICAL_UNLOCK( hb_runningStacks.Mutex );
}

#ifndef HB_OS_WIN_32

void hb_threadResetAux( void *ptr )
{
   ((HB_SHARED_RESOURCE *) ptr)->aux = 0;
   HB_COND_SIGNAL( hb_runningStacks.Cond );
}

/* hb_runningStacks mutex must be held before calling this function */
void hb_threadWaitForIdle( void )
{
   /* Do we have to set an idle fence? */
   if ( hb_bIdleFence )
   {
      /* blocks all the threads */
      hb_runningStacks.aux = 1;
   }

   HB_CLEANUP_PUSH( hb_threadResetAux, hb_runningStacks );
   /* wait until the road is clear */
   while ( hb_runningStacks.content.asLong != 0 )
   {
      HB_COND_WAIT( hb_runningStacks.Cond, hb_runningStacks.Mutex );
   }
   /* blocks all threads here if not blocked before */
   hb_runningStacks.aux = 1;
   // no need to signal, no one must be awaken
   HB_CLEANUP_POP;
}

#endif

/*
JC: I am leaving this in the source code for now; you can never know, this could
be useful in the future.
*/
#if defined( HB_OS_WIN_32 ) //&& ( WINVER < 0x0400)
DWORD hb_SignalObjectAndWait( HB_MUTEX_T hToSignal, HB_COND_T hToWaitFor, DWORD dwMillisec, BOOL bUnused )
{
   HB_THREAD_STUB
   int iStatus;
   HB_SYMBOL_UNUSED( bUnused );

   HB_MUTEX_UNLOCK( hToSignal );
   // here we can allow cancellation
   HB_TEST_CANCEL_ENABLE_ASYN;
   /* return 0 on success like unix functions */
   iStatus = WaitForSingleObject( hToWaitFor, dwMillisec ) != WAIT_OBJECT_0;
   HB_DISABLE_ASYN_CANC;
   HB_MUTEX_LOCK( hToSignal );

   return iStatus;
}
#endif

#ifndef HB_OS_WIN_32
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

void hb_threadInit( void )
{
   HB_CRITICAL_INIT( hb_threadStackMutex );
   HB_CRITICAL_INIT( hb_allocMutex );
   HB_CRITICAL_INIT( hb_garbageAllocMutex );
   HB_CRITICAL_INIT( hb_macroMutex );
   HB_CRITICAL_INIT( hb_outputMutex );
   HB_CRITICAL_INIT( hb_mutexMutex );

   HB_SHARED_INIT( hb_runningStacks, 0 );

   last_stack = NULL;
   hb_main_thread_id = HB_CURRENT_THREAD();
   hb_ht_stack = &hb_stack;
   hb_ht_mutex = NULL;

   /* Idle fence is true by default */
   hb_bIdleFence = TRUE;

   #ifdef HB_OS_WIN_32
      HB_CRITICAL_INIT( hb_fenceMutex );
      HB_CRITICAL_INIT( hb_cancelMutex );
      hb_dwCurrentStack = TlsAlloc();
      TlsSetValue( hb_dwCurrentStack, (void *)hb_ht_stack );
      HB_SHARED_INIT( hb_idleQueueRes, 0 );
   #else
      pthread_key_create( &hb_pkCurrentStack, NULL );
      pthread_setspecific( hb_pkCurrentStack, (void *)hb_ht_stack );
   #endif

   #if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )
      HB_CRITICAL_INIT( s_mtxTryLock );
   #endif

   s_thread_unique_id = 1;
   HB_CRITICAL_INIT( s_thread_unique_id_mutex );
}

void hb_threadExit( void )
{
   hb_threadKillAll();
   hb_threadWaitAll();

   // the main stack is now destroyed by hb_stack_exit
   //hb_threadDestroyStack( hb_ht_stack );
   hb_ht_stack = NULL;

   /* Destroyng all shell locks mutexes */
   HB_SHARED_DESTROY( hb_runningStacks );
   HB_CRITICAL_DESTROY( hb_mutexMutex );
   HB_CRITICAL_DESTROY( hb_outputMutex );
   HB_CRITICAL_DESTROY( hb_macroMutex );
   HB_CRITICAL_DESTROY( hb_garbageAllocMutex );
   HB_CRITICAL_DESTROY( hb_allocMutex );
   HB_CRITICAL_DESTROY( hb_threadStackMutex );

   #ifdef HB_OS_WIN_32
      TlsFree( hb_dwCurrentStack );
      HB_CRITICAL_DESTROY( hb_cancelMutex );
      HB_SHARED_DESTROY( hb_idleQueueRes );
      HB_CRITICAL_DESTROY( hb_fenceMutex );
   #else
      pthread_key_delete( hb_pkCurrentStack );
   #endif

   #if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )
      HB_CRITICAL_DESTROY( s_mtxTryLock );
   #endif

   HB_CRITICAL_DESTROY( s_thread_unique_id_mutex );
}

#endif

