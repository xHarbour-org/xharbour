/*
* $Id: thread.c,v 1.59 2003/03/08 02:25:34 jonnymind Exp $
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
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"
#include "thread.h"

/* Creating a trylock for systems that have to use LWR */
#if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )

/* To be honest, this implementation is far from being thread safe;
   TODO: better implementation, but I argue it will ALWAYS be better
   to implement syste specific solution, where available.
*/

static HB_CRITICAL_T s_mtxTryLock;

BOOL hb_critical_mutex_trylock( HB_CRITICAL_T *lpMutex )
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
      HB_MUTEX_LOCK( lpMutex->Critical );
      lpMutex->nCount = 1;
      lpMutex->Locker = HB_CURRENT_THREAD();
      HB_CRITICAL_UNLOCK( s_mtxTryLock );
   }
}

#endif  //LWR mutexes for OS without them.

#if defined( HB_OS_LINUX )
   pthread_key_t hb_thread_stack_key;
#endif

HB_STACK *hb_ht_stack;
HB_STACK *last_stack;
HB_THREAD_T hb_main_thread_id;
volatile static int s_threadStarted = 0;

/* Declarations of shell mutexes */
HB_CRITICAL_T hb_garbageMutex;
HB_CRITICAL_T hb_threadStackMutex;
HB_CRITICAL_T hb_globalsMutex;
HB_CRITICAL_T hb_staticsMutex;
HB_CRITICAL_T hb_memvarsMutex;
HB_CRITICAL_T hb_allocMutex;
HB_CRITICAL_T hb_garbageAllocMutex;
HB_CRITICAL_T hb_outputMutex;

HB_SHARED_RESOURCE hb_runningStacks;

HB_STACK *hb_threadCreateStack( HB_THREAD_T th )
{
   HB_STACK *tc;
   int i;

   //HB_CRITICAL_LOCK( hb_allocMutex );
   tc = (HB_STACK *) malloc( sizeof( HB_STACK));

   tc->th_id = th;
   tc->next = NULL;

   tc->pItems = ( HB_ITEM_PTR * ) malloc( sizeof( HB_ITEM_PTR ) * STACK_THREADHB_ITEMS );
   tc->pBase  = tc->pItems;
   tc->pPos   = tc->pItems;     /* points to the first stack item */
   tc->wItems = STACK_THREADHB_ITEMS;
   tc->pMethod = NULL;
   tc->Return.type = HB_IT_NIL;
   tc->bInUse = FALSE;
   #ifdef HB_OS_WIN_32
      tc->th_h = NULL;
      tc->bCanceled = FALSE;
      tc->iCleanCount = 0;
      tc->pCleanUp = (HB_CLEANUP_FUNC *) malloc( sizeof( HB_CLEANUP_FUNC ) * HB_MAX_CLEANUPS );
      tc->pCleanUpParam = (void **) malloc( sizeof( void *) * HB_MAX_CLEANUPS );
      
   #endif

   for( i = 0; i < tc->wItems; i++ )
   {
      tc->pItems[ i ] = (HB_ITEM *) malloc( sizeof( HB_ITEM ) );
   }
   ( * (tc->pPos) )->type = HB_IT_NIL;

   //HB_CRITICAL_UNLOCK( hb_allocMutex );

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
   s_threadStarted ++;

   return tc;
}


/*JC1: WARNING
* this function is not locked because is meant to be called ONLY within
* a thread stack locked area.
*/
HB_STACK *hb_threadUnlinkStack( HB_THREAD_T th_id )
{
   HB_STACK *p, *prev;

   if ( hb_ht_stack == NULL )
   {
      return NULL;
   }

   p = hb_ht_stack;
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
         hb_ht_stack = p->next;
      }
      s_threadStarted --;
   }

   return p;
}


void hb_threadDestroyStack( HB_STACK *pStack )
{
   int i;
   PHB_ITEM *pPos;

   /* Free each element of the stack */

   for( pPos = pStack->pItems; pPos < pStack->pPos; pPos++)
   {
      if( HB_IS_COMPLEX( *pPos ) )
      {
         hb_itemClear( *pPos );
      }
   }

   /* Free each element of the stack */

   for( i = 0; i < pStack->wItems; i++ )
   {
      free( pStack->pItems[ i ] );
   }
   /* Free the stack */

   free( pStack->pItems );
   #ifdef HB_OS_WIN_32
   free( pStack->pCleanUp );
   free( pStack->pCleanUpParam );
   #endif
   free( pStack );

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
      else
      {
         char errdat[64];

         sprintf( errdat, "Stack not found for Thread %ld",  (long) id );
         hb_errRT_BASE_SubstR( EG_CORRUPTION, 10001, errdat, "hb_threadGetStack", 0 );
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
   USHORT uiParam;

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


#ifdef HB_OS_WIN_32
void hb_threadTerminator2()
{
   HB_STACK *pStack = hb_threadGetStack( HB_CURRENT_THREAD() );
   hb_threadTerminator( pStack );
}

void hb_threadTerminator( void *pData )
{
   HB_STACK *_pStack_ = (HB_STACK *) pData;
   HB_STACK_LOCK;  // prevent being searched here

     
   HB_CRITICAL_LOCK( hb_threadStackMutex );
   /* now we can detach this thread */
   hb_threadUnlinkStack( _pStack_->th_id );
   HB_CRITICAL_UNLOCK( hb_threadStackMutex );
   CloseHandle( _pStack_->th_h );
   hb_threadDestroyStack( _pStack_ );
   
   HB_SET_SHARED( hb_runningStacks, HB_COND_INC, -1 );
}

void hb_threadTestCancel()
{
   int iCount;
   HB_THREAD_STUB

   if ( HB_VM_STACK.bCanceled )
   {
      /* Calling the cleanup stack */
      iCount = HB_VM_STACK.iCleanCount;
      while (iCount > 0 )
      {
         iCount--;
         HB_VM_STACK.pCleanUp[ iCount ]( HB_VM_STACK.pCleanUpParam[ iCount ]);
      }
      
      ExitThread( 0 );
   }
}

void hb_threadCancelInternal( )
{
   HB_THREAD_STUB
   int iCount;
   
   /* Calling the cleanup stack */
   iCount = HB_VM_STACK.iCleanCount;
   while ( iCount > 0 )
   {
      iCount--;
      HB_VM_STACK.pCleanUp[ iCount ]( HB_VM_STACK.pCleanUpParam[ iCount ]);
   }
   // the stack must have been destroyed by the last cleanup function
   ExitThread( 0 );
}

/***
* Warning: running stack mutex must be held before calling this one
* NEVER use this function to cancel the calling thread
*/
void hb_threadCancel( HB_STACK *pStack )
{
   CONTEXT context;

   // stack resource mutex is being locked now
   pStack->bInUse = TRUE;  // mark the stack as used   
   SuspendThread( pStack->th_h ); // stop thread before he can do something with stack
   HB_MUTEX_UNLOCK( hb_runningStacks.Mutex );

   context.ContextFlags = CONTEXT_CONTROL;
   GetThreadContext(  pStack->th_h , &context);
   // _x86 only!!!
   context.Eip = (DWORD)hb_threadCancelInternal;
   SetThreadContext(  pStack->th_h , &context);
   ResumeThread(  pStack->th_h );
}

         
#else

void hb_threadTerminator( void *pData )
{
   HB_STACK *_pStack_ = (HB_STACK *) pData;
   HB_STACK_LOCK;
   
   HB_CRITICAL_LOCK( hb_threadStackMutex );
   /* now we can detach this thread */
   hb_threadUnlinkStack( _pStack_->th_id );
   HB_CRITICAL_UNLOCK( hb_threadStackMutex );
   hb_threadDestroyStack( _pStack_ );
   
   pthread_detach( HB_CURRENT_THREAD() );
   HB_SET_SHARED( hb_runningStacks, HB_COND_INC, -1 );
}

#endif


void hb_mutexForceUnlock( void *mtx)
{
   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) mtx;
   Mutex->locker = 0;
   Mutex->lock_count = 0;
   HB_MUTEX_UNLOCK( Mutex->mutex);	
}

void hb_rawMutexForceUnlock( void * mtx)
{
   HB_MUTEX_T *Mutex = (HB_MUTEX_T *) mtx;
   HB_MUTEX_UNLOCK( *Mutex );	
}


#ifdef HB_OS_WIN_32
   DWORD WINAPI hb_create_a_thread( LPVOID Cargo )
#else
   void *hb_create_a_thread( void *Cargo )
#endif
{
   HB_STACK *_pStack_ = hb_threadGetStack( HB_CURRENT_THREAD() );

   #if defined( HB_OS_LINUX )
      /* link stack to local thread data */
      pthread_setspecific(hb_thread_stack_key, _pStack_);
   #endif

   /* avoid warning */
   if ( Cargo != 0 ) return 0;
   /* Sets the cancellation handler so small delays in
   cancellation do not cause segfault or memory leaks */
#ifdef HB_OS_WIN_32
   HB_CLEANUP_PUSH( hb_threadTerminator, &(HB_VM_STACK) );
#else
   HB_CLEANUP_PUSH( hb_threadTerminator, HB_VM_STACK );
#endif

   // Do and Send reduce the count of function by one.
   if( _pStack_->bIsMethod )
   {
      hb_vmSend( HB_VM_STACK.uiParams - 2 );
   }
   else
   {
      hb_vmDo( HB_VM_STACK.uiParams - 1 );
   }

   #ifdef HB_OS_WIN_32
   {  
      HB_VM_STACK.bCanceled = TRUE;
      hb_threadTestCancel(); // never returns 
      return 0;
   }
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

   // We signal that we are going to start a thread
   HB_CRITICAL_LOCK( hb_threadStackMutex );

#if defined(HB_OS_WIN_32)
   if( ( th_h = CreateThread( NULL, 0, hb_create_a_thread, NULL , CREATE_SUSPENDED, &th_id ) ) != NULL )
#else
   if( pthread_create( &th_id, NULL, hb_create_a_thread, NULL ) == 0 )
#endif
   {
      /* Under windws, we put the handle after creation */
      HB_STACK *pStack = hb_threadCreateStack( th_id );
      pStack->uiParams = hb_pcount();
      pStack->bIsMethod = bIsMethod;
      hb_threadFillStack( pStack, pArgs );
      hb_threadLinkStack( pStack );

#if defined(HB_OS_WIN_32)
      pStack->th_h = th_h;
      ResumeThread( th_h );
#endif

      hb_retnl( (long) th_id );
   }
   else
   {
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
      /* Shell locking the thread */
      HB_MUTEX_LOCK( hb_runningStacks.Mutex );
      if ( stack->bInUse )
      {
         stack->bCanceled = TRUE;
         HB_MUTEX_UNLOCK( hb_runningStacks.Mutex );
         WaitForSingleObject( stack->th_h, INFINITE );
      }
      else
      {
         hb_threadCancel( stack ); // also unlocks the mutex
      }
   
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
      HB_MUTEX_LOCK( hb_runningStacks.Mutex );
      if ( stack->bInUse )
      {
         stack->bCanceled = TRUE;
         HB_MUTEX_UNLOCK( hb_runningStacks.Mutex );
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

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "KILLTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );

      return;
   }

   th = (HB_THREAD_T) hb_parnl( 1 );

   #if ! defined( HB_OS_WIN_32 )
      HB_STACK_UNLOCK;
      pthread_join( th, 0 );
   #else
      stack = hb_threadGetStack( th );
      HB_STACK_UNLOCK;
      WaitForSingleObject( stack->th_h, INFINITE );
   #endif
   HB_STACK_LOCK;
   
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

   while( Mutex->lock_count )
   {
      HB_MUTEX_UNLOCK( Mutex->mutex );
      Mutex->lock_count--;
   }

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
      /* prevent the thread from being canceled while waiting for the mutex */
      HB_CLEANUP_PUSH( hb_mutexForceUnlock, Mutex )
      HB_MUTEX_LOCK( Mutex->mutex );
      HB_TEST_CANCEL(); // is this really thread safe? What if canc arrives AFTER
      HB_CLEANUP_POP;
      Mutex->locker = HB_CURRENT_THREAD();
      Mutex->lock_count = 1;
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

void hb_threadWaitAll()
{
   HB_THREAD_STUB
   
   HB_STACK_UNLOCK;
   while( s_threadStarted > 1 )
   {
      #if defined(HB_OS_WIN_32)
         Sleep( 1 );
      #elif defined(HB_OS_DARWIN)
         usleep( 1 );
      #else
         static struct timespec nanosecs = { 0, 1000 };
         nanosleep( &nanosecs, NULL );
      #endif
      HB_TEST_CANCEL();
   }
   HB_STACK_LOCK;
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
         HB_MUTEX_LOCK( hb_runningStacks.Mutex );
         if ( pStack->bInUse )
         {
            pStack->bCanceled = TRUE;
            HB_MUTEX_UNLOCK( hb_runningStacks.Mutex );
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

void hb_threadInit( void )
{
   HB_CRITICAL_INIT( hb_threadStackMutex );
   HB_CRITICAL_INIT( hb_allocMutex );
   HB_CRITICAL_INIT( hb_garbageAllocMutex );
   HB_CRITICAL_INIT( hb_outputMutex );
   HB_SHARED_INIT( hb_runningStacks, 0 );

   last_stack = NULL;
   hb_main_thread_id = HB_CURRENT_THREAD();
   hb_ht_stack = hb_threadCreateStack( hb_main_thread_id );
   s_threadStarted = 1;

   #if defined( HB_OS_LINUX )
      /* create key for stack in thread local data */
      pthread_key_create(&hb_thread_stack_key, NULL);
      /* link stack to local thread data */
      pthread_setspecific(hb_thread_stack_key, hb_threadGetStack( hb_main_thread_id ));
   #endif
   
   #if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )
      HB_CRITICAL_INIT( s_mtxTryLock );
   #endif
}

void hb_threadExit( void )
{
   hb_threadKillAll();
   hb_threadWaitAll();
   
   hb_threadDestroyStack( hb_ht_stack );
   hb_ht_stack = NULL;
   s_threadStarted = 0;
   
   /* Destroyng all shell locks mutexes */
   HB_SHARED_DESTROY( hb_runningStacks );
   HB_CRITICAL_DESTROY( hb_outputMutex );
   HB_CRITICAL_DESTROY( hb_garbageAllocMutex );
   HB_CRITICAL_DESTROY( hb_allocMutex );
   HB_CRITICAL_DESTROY( hb_threadStackMutex );

   #if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )
      HB_CRITICAL_DESTROY( s_mtxTryLock );
   #endif
}


/*
JC1: this should be reactivated if we want flat mutex

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
#endif
