/*
* $Id: thread.c,v 1.141 2003/12/09 02:53:49 ronpinkas Exp $
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

/* JC1: Now including all this files to make threadsleep available in ST */
#define HB_THREAD_OPTIMIZE_STACK

#if defined( HB_OS_DARWIN ) || defined(__DJGPP__)
   #include <stdlib.h>
   #include <unistd.h>    /* We need usleep() in Darwin */
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
#include "classes.h"

#ifdef HB_OS_WIN_32
   #include <windows.h>
   #include <process.h>
#endif

#ifdef HB_OS_WIN_32
    #define extern
#endif

#include "thread.h"

#ifdef HB_OS_WIN_32
   #undef extern
#endif

#ifdef HB_THREAD_SUPPORT

// FROM MEMVARS.C:
//TODO:Move this in an appropriate file
#define TABLE_INITHB_VALUE   100
#define TABLE_EXPANDHB_VALUE  50

/**************************************************************/
/* Part 1: Global objects declaration                         */
/**************************************************************/

HB_STACK *hb_ht_stack = 0;
HB_STACK *last_stack;
HB_MUTEX_STRUCT *hb_ht_mutex;
HB_THREAD_T hb_main_thread_id;

static HB_CRITICAL_T s_thread_unique_id_mutex;
static UINT s_thread_unique_id;

#ifdef HB_OS_WIN_32
   HB_EXPORT DWORD hb_dwCurrentStack;
#else
   pthread_key_t hb_pkCurrentStack;
#endif

/* Declarations of shell mutexes */
HB_EXPORT HB_COND_T hb_threadStackCond;

HB_EXPORT HB_CRITICAL_T hb_globalsMutex;
HB_EXPORT HB_CRITICAL_T hb_staticsMutex;
HB_EXPORT HB_CRITICAL_T hb_memvarsMutex;
HB_EXPORT HB_CRITICAL_T hb_macroMutex;
HB_EXPORT HB_CRITICAL_T hb_allocMutex;
HB_EXPORT HB_CRITICAL_T hb_garbageAllocMutex;
HB_EXPORT HB_CRITICAL_T hb_outputMutex;
HB_EXPORT HB_CRITICAL_T hb_mutexMutex;
HB_EXPORT HB_CRITICAL_T hb_cancelMutex;
HB_EXPORT HB_CRITICAL_T hb_dynsymMutex;

HB_EXPORT HB_SHARED_RESOURCE hb_runningStacks;

BOOL hb_bIdleFence;


/**************************************************************/
/* Part 2: Initialization and termination of thread subsystem */
/**************************************************************/

void hb_threadInit( void )
{
   HB_CRITICAL_INIT( hb_allocMutex );
   HB_CRITICAL_INIT( hb_garbageAllocMutex );
   HB_CRITICAL_INIT( hb_macroMutex );
   HB_CRITICAL_INIT( hb_outputMutex );
   HB_CRITICAL_INIT( hb_mutexMutex );
   HB_CRITICAL_INIT( hb_dynsymMutex );
   HB_COND_INIT( hb_threadStackCond );
   s_thread_unique_id = 1;
   HB_CRITICAL_INIT( s_thread_unique_id_mutex );
   HB_SHARED_INIT( hb_runningStacks, 0 );

   #if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )
      HB_CRITICAL_INIT( s_mtxTryLock );
   #endif

   last_stack = NULL;
   hb_main_thread_id = HB_CURRENT_THREAD();

   hb_ht_mutex = NULL;

   /* Idle fence is true by default */
   hb_bIdleFence = TRUE;

   #ifdef HB_OS_WIN_32
      HB_CRITICAL_INIT( hb_cancelMutex );

      hb_dwCurrentStack = TlsAlloc();
      TlsSetValue( hb_dwCurrentStack, (void *)&hb_stack );
   #else
      pthread_key_create( &hb_pkCurrentStack, NULL );
      pthread_setspecific( hb_pkCurrentStack, (void *)&hb_stack );
   #endif
}

void hb_threadExit( void )
{
   hb_threadKillAll();
   hb_threadWaitAll();

   hb_ht_stack = NULL; //signal we are not ready anymore to collect vm stats
   hb_threadDestroyStack( &hb_stack );

   // the main stack still exists, but we must signal it has not items anymore
   hb_stack.pItems = NULL;
   hb_stack.wItems = 0;
}

void hb_threadCloseHandles( void )
{
   // Now we destroy the things that makes MT stack to exist,
   // so, after this one the VM must really quit

   /* Destroyng all shell locks mutexes */
   HB_SHARED_DESTROY( hb_runningStacks );
   HB_CRITICAL_DESTROY( hb_mutexMutex );
   HB_CRITICAL_DESTROY( hb_outputMutex );
   HB_CRITICAL_DESTROY( hb_macroMutex );
   HB_CRITICAL_DESTROY( hb_garbageAllocMutex );
   HB_CRITICAL_DESTROY( hb_allocMutex );
   HB_CRITICAL_DESTROY( hb_dynsymMutex );
   HB_COND_DESTROY(hb_threadStackCond);

   #ifdef HB_OS_WIN_32
      TlsFree( hb_dwCurrentStack );
      HB_CRITICAL_DESTROY( hb_cancelMutex );
   #else
      pthread_key_delete( hb_pkCurrentStack );
   #endif

   #if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )
      HB_CRITICAL_DESTROY( s_mtxTryLock );
   #endif

   HB_CRITICAL_DESTROY( s_thread_unique_id_mutex );
}


/**************************************************************/
/* Part 2: Thread stack internal management                   */
/**************************************************************/

static UINT hb_threadUniqueId( void )
{
   volatile UINT uiRet;
   HB_CRITICAL_LOCK( s_thread_unique_id_mutex );
   if ( s_thread_unique_id == HB_THREAD_MAX_UNIQUE_ID )
   {
      s_thread_unique_id = 1;
   }
   uiRet = s_thread_unique_id;
   s_thread_unique_id++;
   HB_CRITICAL_UNLOCK( s_thread_unique_id_mutex );

   return uiRet;
}


/*
  Creating a new stack
*/
HB_STACK *hb_threadCreateStack( HB_THREAD_T th )
{
   HB_STACK *tc;

   tc = (HB_STACK *) hb_xgrab( sizeof( HB_STACK));
   hb_threadSetupStack( tc, th );

   return tc;
}

/*
  Filling a new stack with default values
*/
void hb_threadSetupStack( HB_STACK *tc, HB_THREAD_T th )
{
   int i;
   ULONG uCount;

   tc->th_id = th;
   tc->uiIdleInspect = 0;
   /* In unix, is the thread that sets up its data. */
   #ifdef HB_OS_WIN_32
   tc->th_vm_id = hb_threadUniqueId();
   #endif

   tc->next = NULL;

   tc->pItems = ( HB_ITEM_PTR * ) hb_xgrab( sizeof( HB_ITEM_PTR ) * STACK_THREADHB_ITEMS );
   tc->pBase  = tc->pItems;
   tc->pPos   = tc->pItems;     /* points to the first stack item */
   tc->wItems = STACK_THREADHB_ITEMS;
   tc->pMethod = NULL;
   tc->Return.type = HB_IT_NIL;
   tc->bInUse = FALSE;
   tc->iPcodeCount = 0;

   tc->errorHandler = NULL;
   tc->errorBlock = hb_itemNew( NULL );
   tc->aTryCatchHandlerStack = hb_itemNew( NULL );

   /* VM requests and recover sequence */
   tc->uiActionRequest = 0;
   tc->lRecoverBase = 0;

   hb_arrayNew( tc->aTryCatchHandlerStack, 0 );
   hb_gcLock(  tc->aTryCatchHandlerStack );
   tc->iLaunchCount = 0;
   tc->uiErrorDOS = 0;

   #ifdef HB_OS_WIN_32
      tc->th_h = NULL;
      tc->bCanceled = FALSE;
      tc->bCanCancel = FALSE;

      tc->iCleanCount = 0;
      tc->pCleanUp = (HB_CLEANUP_FUNC *) hb_xgrab( sizeof( HB_CLEANUP_FUNC ) * HB_MAX_CLEANUPS );
      tc->pCleanUpParam = (void **) hb_xgrab( sizeof( void *) * HB_MAX_CLEANUPS );

   #endif

   for( i = 0; i < tc->wItems; i++ )
   {
      tc->pItems[ i ] = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) );
   }
   ( * (tc->pPos) )->type = HB_IT_NIL;

   /* Initialization of "foreach" and "with object" */
   for ( uCount = 0; uCount < HB_MAX_WITH_OBJECTS; uCount++  )
   {
      tc->aWithObject[ uCount ].type = HB_IT_NIL;
   }
   tc->wWithObjectCounter = 0;
   tc->bWithObject = FALSE;

   for ( uCount = 0; uCount < HB_MAX_ENUMERATIONS; uCount++ )
   {
      tc->aEnumCollection[ uCount ].type = HB_IT_NIL;
      tc->awEnumIndex[ uCount ] = 0;
   }
   tc->wEnumCollectionCounter = 0;

   /* Dynsym Thread Specific table. */
   tc->uiClosestDynSym = 0;
   tc->pDynItems = NULL;
   tc->uiDynSymbols = 0;

   /* initialization of macro & codeblock parameter passing */
   tc->iExtraParamsIndex = 0;
   tc->iExtraElementsIndex = 0;
   tc->iExtraElements = 0;
   tc->iExtraIndex = 0;

   /* Initialization of dbcmd related variables */
   tc->uiCurrArea = 1;
   tc->pCurrArea = 0;

   /* Initialization of private and public memvars */
   hb_memvarsInit( tc );

   /* Initialization of dynsym pointers to memvars */
   /*tc->hMemvars = (HB_HANDLE *) hb_xgrab( sizeof( HB_HANDLE ) * TABLE_INITHB_VALUE );
   tc->hMemvarsAllocated = TABLE_INITHB_VALUE;
   tc->hMemvarsLastFree = 1;
   tc->hMemvars[0]= 0;*/
}

/*
This functions fills the stack just before thread creations, with the
arguments for the new thread main routine.
 */
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
   tc->next = NULL;

   //last_stack = p;

   return tc;
}

/*
   Destroys a stack; on exit, even pStack is destroyed.
*/
void hb_threadDestroyStack( HB_STACK *pStack )
{
   long i;
   PHB_ITEM *pPos;
   /* Free each element of the stack */
   if ( pStack != &hb_stack ) {
      for( pPos = pStack->pItems; pPos < pStack->pPos; pPos++)
      {
         if( HB_IS_COMPLEX( *pPos ) )
         {
            hb_itemClear( *pPos );
         }
      }
   }

   /* Eventually free the return value of the stack */
   if( HB_IS_COMPLEX( &(pStack->Return) ) )
   {
      hb_itemClear( &(pStack->Return) );
   }

   /* Error handler is never allocated; it resides in the stack, or
      is owned by callers. */
   if( pStack->errorBlock && pStack->errorBlock->type != HB_IT_NIL )
   {
      // Harbour should remove the error handler of the main stack
      // from PRG level or around that.
      if( pStack != &hb_stack ) {
         hb_itemClear( pStack->errorBlock );
      }
   }
   /* Free each element of the stack */
   for( i = 0; i < pStack->wItems; i++ )
   {
      hb_xfree( pStack->pItems[ i ] );
   }
   /* Free the stack */

   hb_xfree( pStack->pItems );

   // releases this thread's memvars

   if( pStack != &hb_stack )
   {
      if ( pStack->aTryCatchHandlerStack &&  pStack->aTryCatchHandlerStack->type != HB_IT_NIL )
      {
         hb_itemClear( pStack->aTryCatchHandlerStack );
      }
      // Main thread should have them removed before arriving here.
      hb_memvarsRelease( pStack );
   }

/*
   if( pStack->hMemvars )
      hb_xfree( pStack->hMemvars );
*/


   #ifdef HB_OS_WIN_32
   hb_xfree( pStack->pCleanUp );
   hb_xfree( pStack->pCleanUpParam );
   #endif


   // Free only if we are not destroing the main stack
   if ( pStack != &hb_stack )
   {
      hb_xfree( pStack );
   }
}


/*
  Links a stack in the stack list; to be used ONLY while holding the
  hb_threadStack mutex.
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

   HB_COND_SIGNAL( hb_threadStackCond );

   return p;
}


/*
  Find a stack given the threadID of the owner; rises an error if
  not found.
*/

HB_STACK *hb_threadGetStack( HB_THREAD_T id )
{
   HB_STACK *p;

   HB_SHARED_LOCK( hb_runningStacks );

   if( last_stack && HB_SAME_THREAD( last_stack->th_id, id ) )
   {
      p = last_stack;
   }
   else {

      p = hb_ht_stack;

      while( p && ! HB_SAME_THREAD( p->th_id, id) )
      {
         p = p->next;
      }

      if( p )
      {
         last_stack = p;
      }

   }

   hb_errRT_BASE_SubstR( EG_ARG, 1099, "Can't find thread ID", "INTERNAL THREADING", 1, hb_paramError( 1 ) );

   HB_SHARED_UNLOCK( hb_runningStacks );
   return p;
}

/*
   to be internally used by functions willing to know if there is a stack
*/
HB_STACK *hb_threadGetStackNoError( HB_THREAD_T id )
{
   HB_STACK *p;

   HB_SHARED_LOCK( hb_runningStacks );

   if( last_stack && HB_SAME_THREAD( last_stack->th_id, id) )
   {
      p = last_stack;
   }
   else {

      p = hb_ht_stack;

      while( p && ! HB_SAME_THREAD( p->th_id, id) )
      {
         p = p->next;
      }

      if( p )
      {
         last_stack = p;
      }
   }

   HB_SHARED_UNLOCK( hb_runningStacks );
   return p;
}


/**
* Thread related STACK gc reference function. It is needed in the
* MARK phase of the mark-sweep GC to mark the objects in all the
* local thread stacks.
*/

void hb_threadIsLocalRef( void )
{
   HB_STACK *pStack;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsLocalRef()"));

   pStack = hb_ht_stack;

   while( pStack )
   {

      if( pStack->Return.type & (HB_IT_BYREF | HB_IT_POINTER | HB_IT_ARRAY | HB_IT_HASH | HB_IT_BLOCK) )
      {
         hb_gcItemRef( &(pStack->Return) );
      }

      if( pStack->pPos > pStack->pItems )
      {
         HB_ITEM_PTR *pItem = pStack->pPos - 1;

         while( pItem != pStack->pItems )
         {
            if( ( *pItem )->type & (HB_IT_BYREF | HB_IT_POINTER | HB_IT_ARRAY | HB_IT_HASH | HB_IT_BLOCK) )
            {
               hb_gcItemRef( *pItem );
            }

            --pItem;
         }
      }

      pStack = pStack->next;
   }
}

/* DEBUG FUNCTION:
   Stack count can NEVER be accurate; but it is useful to know about
   prorgram status or progress.
*/
int hb_threadCountStacks()
{
   HB_STACK *p;
   int count = 0;

   /* never unlinks the main stack */
   HB_SHARED_LOCK( hb_runningStacks );

   p = hb_ht_stack;
   while ( p )
   {
      count ++;
      p = p->next;
   }
   HB_SHARED_UNLOCK( hb_runningStacks );

   return count;
}

#if 0
/* TODO: collecting of unused old memvars. */
void hb_threadSetHMemvar( PHB_DYNS pDyn, HB_HANDLE hv )
{
   HB_THREAD_STUB

   if ( _pStack_->hMemvarsAllocated <= _pStack_->hMemvarsLastFree )
   {
      _pStack_->hMemvars = ( HB_HANDLE *) hb_xrealloc( _pStack_->hMemvars,
             sizeof( HB_HANDLE ) * (_pStack_->hMemvarsAllocated + TABLE_EXPANDHB_VALUE) );
      _pStack_->hMemvarsAllocated += TABLE_EXPANDHB_VALUE;
   }

   pDyn->hMemvar = _pStack_->hMemvarsLastFree;
   _pStack_->hMemvars[ _pStack_->hMemvarsLastFree++ ] = hv;
}
#endif



/**************************************************************/
/* Part 3: Thread internal support api                        */
/**************************************************************/

/*
   This is the main function of every thread; it prepares the
   stack with the data for the XHARBOUR function that has to
   be calleed, and calls it.
*/
#ifdef HB_OS_WIN_32
//   DWORD WINAPI hb_create_a_thread( LPVOID Cargo )
   unsigned __stdcall hb_create_a_thread( void * Cargo )
#else
   void *hb_create_a_thread( void *Cargo )
#endif
{
   HB_STACK *_pStack_ = (HB_STACK *) Cargo;
   PHB_DYNS pExecSym;

   /* Sets the cancellation handler so small delays in
   cancellation do not cause segfault or memory leaks */
#ifdef HB_OS_WIN_32
   TlsSetValue( hb_dwCurrentStack, ( void * ) _pStack_ );
#else
   /* The fist that arrives among father and child will set up
      the stack id. */
   _pStack_->th_id = HB_CURRENT_THREAD();
   pthread_setspecific( hb_pkCurrentStack, Cargo );
   pthread_cleanup_push( hb_threadTerminator, NULL );
#endif


   // call errorsys() to initialize errorblock
   hb_dynsymLock();
   pExecSym = hb_dynsymFind( "ERRORSYS" );

   if( pExecSym )
   {
      hb_vmPushSymbol( pExecSym->pSymbol );
      hb_dynsymUnlock();
      hb_vmPushNil();
      hb_vmDo(0);
   }
   else
   {
      hb_dynsymLock();
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


/*
   Cancels a thread; called internally by the windows-specific thread
   killer; It must be used by the target thread to cancel itself.
*/

#ifdef HB_OS_WIN_32
void hb_threadCancelInternal( )
{
   HB_THREAD_STUB
   int iCount;

   /* Make sure we are not going to be canceled */
   HB_DISABLE_ASYN_CANC;


   iCount = HB_VM_STACK.iCleanCount;
   while ( iCount > 0 )
   {
      iCount--;
      HB_VM_STACK.pCleanUp[ iCount ]( HB_VM_STACK.pCleanUpParam[ iCount ]);
   }
   // the stack must have been destroyed by the last cleanup function

   hb_threadTerminator( &HB_VM_STACK );
/*   #ifndef __BORLANDC__
   ExitThread( 0 );
   #else*/
   _endthreadex( 0 );
//   #endif
}

/***
* Warning: cancel mutex must be held before calling this one
* NEVER use this function to cancel the calling thread.
*/
void hb_threadCancel( HB_STACK *pStack )
{
   HB_THREAD_STUB
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
   HB_DISABLE_ASYN_CANC;
   HB_CRITICAL_UNLOCK( hb_cancelMutex );

   hb_threadTerminator( (void *)pStack );

}

#endif

/*
   Standard thread termination routine; this is called by a thread
   to cleanup the things; at the exit of this function, the thread
   is terminated.
*/
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
      if ( HB_SAME_THREAD( pMtx->locker, _pStack_->th_id) )
      {
         hb_mutexForceUnlock( pMtx );
      }
      pMtx = pMtx->next;
   }
   HB_CRITICAL_UNLOCK( hb_mutexMutex );

   /* we are out of business */
   HB_SHARED_LOCK( hb_runningStacks );

   /* now we can detach this thread */
   hb_threadUnlinkStack( _pStack_ );
   hb_threadDestroyStack( _pStack_ );

   if ( --hb_runningStacks.content.asLong < 1 )
   {
      HB_SHARED_SIGNAL( hb_runningStacks );
   }
   HB_SHARED_UNLOCK( hb_runningStacks );
}

/*
   Waits for the running stack counter to reach 0 (and removes
   it from the running threads pool). The calling thread is
   then NOT an idle inspector: it just RESTARTS after all the
   others are done.

   IT is meant to be called from the main thread.
*/
HB_EXPORT void hb_threadWaitAll()
{
   HB_THREAD_STUB

   // refuse to run if we are not the main thread
   if (! HB_SAME_THREAD( hb_main_thread_id, HB_CURRENT_THREAD()) )
   {
      return;
   }


   HB_SHARED_LOCK( hb_runningStacks );
   hb_runningStacks.content.asLong --;
   HB_VM_STACK.bInUse = 0;

   while ( hb_runningStacks.content.asLong > 0 || hb_ht_stack->next != NULL )
   {
      HB_SHARED_WAIT( hb_runningStacks );
   }

   /* no more threads now */
   hb_runningStacks.content.asLong ++;
   HB_VM_STACK.bInUse = 1;
   HB_SHARED_UNLOCK( hb_runningStacks );
}


/*
   Kill all the threads except the main one. Must be called
   from the main thread.
*/
HB_EXPORT void hb_threadKillAll()
{
   HB_STACK *pStack;
   HB_THREAD_STUB;

   hb_threadWaitForIdle();

   pStack = hb_ht_stack;
   while( pStack )
   {
      /* DO NOT destroy main thread stack */
      if ( HB_SAME_THREAD( pStack->th_id, hb_main_thread_id )
          || HB_SAME_THREAD( pStack->th_id, HB_CURRENT_THREAD()) )
      {
         pStack = pStack->next;
         continue;
      }
      HB_STACK_UNLOCK
      #ifndef HB_OS_WIN_32
         // Allows the target thread to cleanup if and when needed.
         pthread_cancel( pStack->th_id );
         pthread_join( pStack->th_id, NULL );
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
      HB_STACK_LOCK
      #endif
      pStack = pStack->next;
   }
}

/*
   Used as a cleanup routines for possible cancelations in
   idle inspectors.
*/
void hb_threadResetAux( void *ptr )
{
   ((HB_SHARED_RESOURCE *) ptr)->aux = 0;
   HB_SHARED_SIGNAL( hb_runningStacks );
   HB_SHARED_UNLOCK( hb_runningStacks );
}

/*
   Transforms the calling thread in an idle inspector.
   Waits for all the threads to be idle, or eventually forces
   them to be idle if hb_bIdleFence is true.
   While waiting, the waiting thread is removed from the running
   stacks pool, but it readded as soon as idle inspecting rigth is
   gained.

   hb_runningStacks mutex must be held before calling this function,
   and is still held on exit.
*/
void hb_threadWaitForIdle( void )
{
   HB_THREAD_STUB

   HB_SHARED_LOCK( hb_runningStacks );

   /* Do we have to set an idle fence? */
   if ( hb_bIdleFence )
   {
      /* blocks all the threads */
      hb_runningStacks.aux = 1;
   }

   hb_runningStacks.content.asLong --;

   HB_CLEANUP_PUSH( hb_threadResetAux, hb_runningStacks );
   HB_VM_STACK.bInUse = FALSE;

   /* wait until the road is clear (only WE are running) */
   while ( hb_runningStacks.content.asLong != 0 )
   {
      HB_SHARED_WAIT( hb_runningStacks );
   }
   /* blocks all threads here if not blocked before */
   hb_runningStacks.aux = 1;

   /* And also prevents other idle inspectors to go */
   hb_runningStacks.content.asLong ++;

   /* And this allows ourself to ignore our stack lock requests,
      being then able to run PRG level code */
   HB_VM_STACK.uiIdleInspect++;

   // no need to signal, no one must be awaken
   HB_CLEANUP_POP;

   HB_SHARED_UNLOCK( hb_runningStacks );
}

/* Thread idle end must be called to drop an idle inspecting status;
   NEVER call this when the thread is not an idle inspector. */
void hb_threadIdleEnd( void )
{
   HB_THREAD_STUB

   hb_runningStacks.aux = 0;

   HB_VM_STACK.uiIdleInspect--;

   HB_VM_STACK.bInUse = TRUE;
   // this will also signal the changed situation.
   HB_SHARED_SIGNAL( hb_runningStacks );
}

/*
   Condition variables needs a special handling to be omomorphic on
   variuous systems.
*/
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

#else

/***************************************************
 Posix like condition variable for WIN32
 Based on the Terekhov - Thomas algorithm version 9
*/

/*
   Init the data needed for the condition variable.
*/
BOOL hb_threadCondInit( HB_WINCOND_T *cond )
{
   cond->nWaitersGone = 0;
   cond->nWaitersBlocked = 0;
   cond->nWaitersToUnblock = 0;

   InitializeCriticalSection( &(cond->mtxUnblockLock) );
   cond->semBlockLock = NULL;
   cond->semBlockQueue = NULL;

   cond->semBlockLock = CreateSemaphore( NULL, 1, 20000000, NULL);
   if ( cond->semBlockLock != NULL ) {
      cond->semBlockQueue = CreateSemaphore( NULL, 0, 20000000, NULL );
      if ( cond->semBlockQueue == NULL )
      {
          return FALSE;
      }
   }
   else {
      return FALSE;
   }
   return TRUE;
}

/*
   Destroys the condition variable.
*/
void hb_threadCondDestroy( HB_WINCOND_T *cond )
{
   DeleteCriticalSection( &(cond->mtxUnblockLock) );
   if ( cond->semBlockLock != NULL ) {
      CloseHandle( cond->semBlockLock );
   }
   if ( cond->semBlockQueue != NULL )
   {
      CloseHandle( cond->semBlockQueue );
   }
}

/*
   Issues a signal, that is, wake ALL the threads who are waiting NOW
   and ONLY them.
*/
void hb_threadCondSignal( HB_WINCOND_T *cond )
{
   register int nSignalsToIssue;

   EnterCriticalSection( &(cond->mtxUnblockLock) );

   if ( cond->nWaitersToUnblock ) {
      if ( ! cond->nWaitersBlocked ) {        // NO-OP
         LeaveCriticalSection( &cond->mtxUnblockLock );
         return;
      }
      nSignalsToIssue = cond->nWaitersBlocked;
      cond->nWaitersToUnblock += nSignalsToIssue;
      cond->nWaitersBlocked = 0;
   }
   else if ( cond->nWaitersBlocked > cond->nWaitersGone ) { // HARMLESS RACE CONDITION!
      WaitForSingleObject( cond->semBlockLock, INFINITE );
      if ( cond->nWaitersGone ) {
         cond->nWaitersBlocked -= cond->nWaitersGone;
         cond->nWaitersGone = 0;
      }
      nSignalsToIssue = cond->nWaitersToUnblock = cond->nWaitersBlocked;
      cond->nWaitersBlocked = 0;
   }
   else { // NO-OP
      LeaveCriticalSection( &(cond->mtxUnblockLock) );
      return;
   }
   LeaveCriticalSection( &(cond->mtxUnblockLock) );
   ReleaseSemaphore( cond->semBlockQueue,nSignalsToIssue, NULL );
}


/*
   Wait for a signal to be issued (at maximum for a given time or INFINITE)
*/
BOOL hb_threadCondWait( HB_WINCOND_T *cond, CRITICAL_SECTION *mutex ,
      DWORD dwTimeout )
{
   HB_THREAD_STUB

   register int nSignalsWasLeft;
   register int bTimeout;

   WaitForSingleObject( cond->semBlockLock, INFINITE );
   cond->nWaitersBlocked++;
   ReleaseSemaphore( cond->semBlockLock, 1, NULL );

   LeaveCriticalSection( mutex );

   HB_TEST_CANCEL_ENABLE_ASYN
   if ( WaitForSingleObject( cond->semBlockQueue, dwTimeout ) != WAIT_OBJECT_0 )
   {
      bTimeout = 1;
   }
   else
   {
      bTimeout = 0;
   }
   HB_DISABLE_ASYN_CANC

   EnterCriticalSection( &cond->mtxUnblockLock );

   if ( (nSignalsWasLeft = cond->nWaitersToUnblock) != 0)
   {
      cond->nWaitersToUnblock--;
   }
   else if ( ++cond->nWaitersGone == 2000000000L )
   {
      WaitForSingleObject( cond->semBlockLock, INFINITE );
      cond->nWaitersBlocked -= cond->nWaitersGone;
      ReleaseSemaphore( cond->semBlockLock, 1, NULL );
      cond->nWaitersGone = 0;
   }
   LeaveCriticalSection( &(cond->mtxUnblockLock) );

   if ( nSignalsWasLeft == 1 )
   {
      ReleaseSemaphore( cond->semBlockLock,1, NULL );
   }

   EnterCriticalSection( mutex );
   return !bTimeout;
}
#endif // win32




/**************************************************************/
/* Part 4: XHARBOUR threading API                             */
/**************************************************************/

/*
   Starts a new thread;
*/
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
   PHB_THREAD_ID pThread;

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
      hb_dynsymLock();
      pExecSym = hb_dynsymFindFromFunction( pFunc );

      if( pExecSym == NULL )
      {
         hb_dynsymUnlock();
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "STARTTHREAD", 1, hb_paramError( 1 ) );
         hb_itemRelease( pArgs );
         return;
      }

      // Converting it to its Symbol.
      pPointer->type = HB_IT_SYMBOL;
      pPointer->item.asSymbol.value = pExecSym->pSymbol;
      hb_dynsymUnlock();
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
      hb_dynsymLock();
      pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pPointer ) );

      if( ! pExecSym )
      {
         hb_dynsymUnlock();
         hb_errRT_BASE( EG_NOFUNC, 1001, NULL, hb_itemGetCPtr( pPointer ), 1, pArgs );
         hb_itemRelease( pArgs );
         return;
      }

      pPointer->type = HB_IT_SYMBOL;
      pPointer->item.asSymbol.value = pExecSym->pSymbol;
      hb_dynsymUnlock();
   }
   /* Is it a code block? */
   else if( pPointer->type != HB_IT_BLOCK )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STARTTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   // Create the thread ID object; for now it is a flat pointer
   pThread =(PHB_THREAD_ID)   hb_gcAlloc( sizeof( HB_THREAD_ID ), NULL );
   pThread->sign = HB_THREAD_ID_SIGN;

   // Create the stack here to avoid cross locking of alloc mutex
   pStack = hb_threadCreateStack( 0 );

   pStack->uiParams = hb_pcount();
   pStack->bIsMethod = bIsMethod;

   /* Forbid usage of stack before that new thread's VM takes care of it */
   hb_threadFillStack( pStack, pArgs );

   /* we can be inspected now, but we are sure that our child thread
      stack cannot */
   HB_SHARED_LOCK( hb_runningStacks );

   hb_runningStacks.content.asLong++;
   pStack->bInUse = TRUE;
   pStack->th_vm_id = hb_threadUniqueId();
   hb_threadLinkStack( pStack );

#if defined(HB_OS_WIN_32)
/*   #ifndef __BORLANDC__
      if( ( th_h = CreateThread( NULL, 0, hb_create_a_thread, (void *) pStack , CREATE_SUSPENDED, &th_id ) ) != NULL )
   #else*/
      if( ( th_h = (HANDLE)_beginthreadex( NULL, 0, hb_create_a_thread, (void *) pStack, CREATE_SUSPENDED, (unsigned int *) &th_id) ) != 0L )
//   #endif
#else
   if( pthread_create( &th_id, NULL, hb_create_a_thread, (void *) pStack ) == 0 )
#endif
   {
      /* under linux, this will be set by the first thread, father or
         child, that is able to reach this line */
      pStack->th_id = th_id;

      /* Under windws, we put the handle after creation */
#if defined(HB_OS_WIN_32)
      pStack->th_h = th_h;
      ResumeThread( th_h );
#endif

      pThread->threadId = th_id;
      pThread->bReady = TRUE;
      pThread->pStack = pStack;
      hb_retptr( pThread );
   }
   else
   {
      hb_threadDestroyStack( pStack );
      pThread->bReady = FALSE;
      hb_retptr( pThread );
   }
   //notice that the child thread won't be able to proceed until we
   // release this mutex.
   HB_SHARED_UNLOCK( hb_runningStacks );
}

/*
   Try to gently stop a thread, and waits for its termination.
*/
HB_FUNC( STOPTHREAD )
{
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = (PHB_THREAD_ID) hb_parpointer( 1 );

   if( pThread == NULL || pThread->sign != HB_THREAD_ID_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STOPTHREAD", 1,
         hb_paramError(1) );
      return;
   }

   if ( ! pThread->bReady )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
         "STOPTHREAD", 1, hb_paramError(1) );
      return;
   }

   HB_STACK_UNLOCK;

   #if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )

      pthread_cancel( pThread->threadId );
      pthread_join( pThread->threadId, NULL );

   #else
      HB_CRITICAL_LOCK( hb_cancelMutex );
      pThread->pStack->bCanceled = TRUE;
      HB_CRITICAL_UNLOCK( hb_cancelMutex );

      HB_TEST_CANCEL_ENABLE_ASYN;
      WaitForSingleObject( pThread->pStack->th_h, INFINITE );
      HB_DISABLE_ASYN_CANC;
   #endif

   HB_STACK_LOCK;
}

/*
   Try to gently stop a thread, and if this is not possible,
   use the maximum severity allowed. It does not wait for
   target thread to be terminated.
*/
HB_FUNC( KILLTHREAD )
{
#ifdef HB_OS_WIN_32
   HB_THREAD_STUB
#endif

   PHB_THREAD_ID pThread = (PHB_THREAD_ID) hb_parpointer( 1 );

   if( pThread == NULL || pThread->sign != HB_THREAD_ID_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "KILLTHREAD", 1,
         hb_paramError(1) );
      return;
   }

   if ( ! pThread->bReady )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
         "KILLTHREAD", 1, hb_paramError(1) );
      return;
   }

   #if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
      pthread_cancel( pThread->threadId );
   #else
      /* Shell locking the thread */
      HB_STACK_UNLOCK;

      HB_CRITICAL_LOCK( hb_cancelMutex );
      if ( ! pThread->pStack->bCanCancel )
      {
         pThread->pStack->bCanceled = TRUE;
         HB_CRITICAL_UNLOCK( hb_cancelMutex );
      }
      else
      {
         hb_threadCancel( pThread->pStack ); //also unlocks the mutex
      }

      HB_STACK_LOCK;
   #endif
}

/*
   Waits until a given thread terminates.
*/
HB_FUNC( JOINTHREAD )
{
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = (PHB_THREAD_ID) hb_parpointer( 1 );

   if( pThread == NULL || pThread->sign != HB_THREAD_ID_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "JOINTHREAD", 1,
         hb_paramError(1) );
      return;
   }

   if ( ! pThread->bReady )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
         "JOINTHREAD", 1, hb_paramError(1) );
      return;
   }

   HB_STACK_UNLOCK;

   #if ! defined( HB_OS_WIN_32 )
      if( pthread_join( pThread->threadId, NULL ) != 0 )
      {
         HB_STACK_LOCK;
         hb_retl( FALSE );
         return;
      }
   #else
      WaitForSingleObject( pThread->pStack->th_h, INFINITE );
   #endif

   HB_STACK_LOCK;

   hb_retl( TRUE );
}

/*
   Get current thread ID (based on SYSTEM ID)
   (deprecated)
*/
HB_FUNC( THREADGETCURRENT )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif
   hb_retnl( (long) HB_CURRENT_THREAD() );
}

/*
   Get current thread VM order
   - Deprecated -
*/
HB_FUNC( THREADGETCURRENTINTERNAL )
{
   HB_THREAD_STUB
   hb_retnl( (long) HB_VM_STACK.th_vm_id );
}

/*
   Return an isnstance of current thread object
*/
HB_FUNC( GETCURRENTTHREAD )
{
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = (PHB_THREAD_ID)
         hb_gcAlloc( sizeof( HB_THREAD_ID ), NULL );

   pThread->sign = HB_THREAD_ID_SIGN;
   pThread->threadId = HB_CURRENT_THREAD();
   pThread->pStack = &HB_VM_STACK;
   pThread->bReady = TRUE;

   hb_retptr( pThread );
}

/*
   Returns VM thread id
*/
HB_FUNC( GETTHREADID )
{
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = (PHB_THREAD_ID) hb_parpointer( 1 );

   if( pThread != NULL )
   {
      if ( pThread->sign != HB_THREAD_ID_SIGN )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GETTHREADID", 1,
            hb_paramError(1) );
         return;
      }
      else if ( ! pThread->bReady )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
            "GETTHREADID", 1, hb_paramError(1) );
         return;
      }
      hb_retnl( (long) pThread->pStack->th_vm_id );
   }
   else
   {
      hb_retnl( HB_VM_STACK.th_vm_id );
   }
}

/*
   Returns a numeric representation of SYSTEM thread id, where available.
   BE CAREFUL - this is mainly a debugging function!
   Don't use it for important code (or be sure to bind to a given platform).
*/
HB_FUNC( GETSYSTEMTHREADID )
{
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = (PHB_THREAD_ID) hb_parpointer( 1 );

   if( pThread != NULL )
   {
      if ( pThread->sign != HB_THREAD_ID_SIGN )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GETSYSTEMTHREADID", 1,
            hb_paramError(1) );
         return;
      }
      else if ( ! pThread->bReady )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
            "GETSYSTEMTHREADID", 1, hb_paramError(1) );
         return;
      }
      #if 1
         hb_retnl( (long) pThread->threadId );
      #endif
      /* Place here a warning or a special value for system without
        numeric or enumerable thread ids
      */
   }
   else
   {
      hb_retnl( HB_VM_STACK.th_id );
   }
}

/*
   Returns true if two thread objects are the same.
*/
HB_FUNC( ISSAMETHREAD )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif
   PHB_THREAD_ID pThread1 = (PHB_THREAD_ID) hb_parpointer( 1 );
   PHB_THREAD_ID pThread2 = (PHB_THREAD_ID) hb_parpointer( 2 );

   if( pThread1 == NULL || pThread1->sign != HB_THREAD_ID_SIGN ||
       ( pThread2 != NULL && pThread2->sign != HB_THREAD_ID_SIGN ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "THREADISSAME", 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   if ( ! pThread1->bReady )
   {
      hb_retl( FALSE );
   }
   else {
      if ( pThread2 == NULL )
      {
         hb_retl( HB_SAME_THREAD( pThread1->threadId, HB_CURRENT_THREAD() ) );
      }
      else if ( ! pThread2->bReady )
      {
         hb_retl( FALSE );
      }
      else
      {
         hb_retl( HB_SAME_THREAD( pThread1->threadId, pThread2->threadId ) );
      }
   }
}


/*
   Returns true if the thread object refers to a valid system thread.
*/
HB_FUNC( ISVALIDTHREAD )
{
#if defined( HB_API_MACROS)
   HB_THREAD_STUB
#endif
   PHB_THREAD_ID pThread = (PHB_THREAD_ID) hb_parpointer( 1 );

   if( pThread == NULL || pThread->sign != HB_THREAD_ID_SIGN )
   {
      hb_retl( FALSE );
   }
   else
   {
      hb_retl( pThread->bReady );
   }
}

/*
   Waits for all threads, except this, to be terminated before proceed.
   Use in the main thread.
*/
HB_FUNC( WAITFORTHREADS )
{
   hb_threadWaitAll();
}


/*
   Kills all the threads, except this.
   Use in the main thread.
*/
HB_FUNC( KILLALLTHREADS )
{
   hb_threadKillAll();
}


/*
   Rises or lower atomically the idle fence for idle inspector.
   Notice that some idle inspectors will force this anyhow (i.e.
   the error handler).
*/
HB_FUNC( THREADIDLEFENCE )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif
   BOOL bOld;

   HB_SHARED_LOCK( hb_runningStacks );

   bOld = hb_bIdleFence;

   if ( hb_pcount() == 1 )
   {
      hb_bIdleFence = hb_parl( 1 );
   }

   hb_retl( bOld );

   HB_SHARED_UNLOCK( hb_runningStacks );
}


/*
   Function used by the error recovery routine to get the
   error handlers from the current stack.
*/
HB_FUNC( HB_THREADGETTRYERRORARRAY )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif
   hb_itemReturnCopy( HB_VM_STACK.aTryCatchHandlerStack );
}

/*
   Mainly a debug function: use cautiously, the count is NOT
   threadsafe.
*/
HB_FUNC( HB_THREADCOUNTSTACKS )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif
   hb_retni( hb_threadCountStacks() );
}



/**************************************************************/
/* Part 5: internal management of XHARBOUR mutex objects      */
/**************************************************************/

/*
  Links the mutex into the mutex list. A mutex list is used to
  force-unlocking of mutexes that have been locked by failing
  or dying threads (that forgot to unlock them.
*/
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

/*
  Unlinks the mutex into the mutex list.
*/
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
      if( p->lock_count )
      {
         hb_mutexForceUnlock( p );
      }
   }


   HB_CRITICAL_UNLOCK( hb_mutexMutex );

   return p;
}

/*
  Force the unlocking of XHARBOUR mutex objects.
*/
void hb_mutexForceUnlock( void *mtx )
{
   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) mtx;

   if( Mutex->locker != 0 )
   {
      Mutex->lock_count = 0;
      Mutex->locker = 0;
      /* warn other therads that this mutex has become available */
      HB_COND_SIGNAL( Mutex->cond );
   }
}

/*
  Unlocks a raw CRITICAL_SECTION type mutex; used internally as
  cleanup function for condition waits and other may-fail
  operations.
*/
void hb_rawMutexForceUnlock( void * mtx )
{
   HB_CRITICAL_T *Mutex = (HB_CRITICAL_T *) mtx;
   HB_CRITICAL_UNLOCK( *Mutex );
}

/*
   Garbage finalization function for XHARBOUR mutex objects.
   When the gc detects a mutex to be destroyed, it calls this
   function to clean it.
*/
HB_GARBAGE_FUNC( hb_threadMutexFinalize )
{
   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) Cargo;

   if ( Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errInternal( HB_EI_MEMCORRUPT,
         "hb_threadMutexFinalize: Corrupted mutex item at 0x%p",
         (char *) Mutex, NULL );
      return;
   }


   hb_threadUnlinkMutex( Mutex );

   HB_CRITICAL_DESTROY( Mutex->mutex );
   HB_COND_DESTROY( Mutex->cond );
   hb_arrayRelease( Mutex->aEventObjects );
   hb_gcFree( Mutex );
}


/******************************************************************/
/* Part 6: Xharbour MUTEX object API                              */
/******************************************************************/

/*
   Create a new mutex (marking it disposeable by the GC)
*/
HB_FUNC( HB_MUTEXCREATE )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif
   HB_MUTEX_STRUCT *mt;

   mt = (HB_MUTEX_STRUCT *)
      hb_gcAlloc( sizeof( HB_MUTEX_STRUCT ), hb_threadMutexFinalize );

   HB_CRITICAL_INIT( mt->mutex );
   HB_COND_INIT( mt->cond );

   mt->sign = HB_MUTEX_SIGNATURE;
   mt->lock_count = 0;
   mt->waiting = 0;
   mt->locker = 0;
   mt->aEventObjects = hb_itemArrayNew( 0 );
   hb_gcLock( mt->aEventObjects );
   mt->next = 0;

   hb_threadLinkMutex( mt );

   hb_retptr( mt );
}

/*
   JC1: Compatibility; DestroyMutex does not exists anymore
   TODO: rise a deprecation warning
*/
HB_FUNC( DESTROYMUTEX )
{

}

/*
   Locks a mutex; locking is done by waiting for the mutex resource
   to become available. This wait is cancelable.
*/

HB_FUNC( HB_MUTEXLOCK )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif

   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) hb_parpointer(1);

   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXLOCK", 1, hb_paramError(1) );
      return;
   }

   if( HB_SAME_THREAD( Mutex->locker, HB_CURRENT_THREAD() ) )
   {
      Mutex->lock_count ++;
   }
   else
   {
      HB_STACK_UNLOCK;

      HB_CRITICAL_LOCK( Mutex->mutex );
      HB_CLEANUP_PUSH( hb_rawMutexForceUnlock, Mutex->mutex );

      while ( Mutex->locker != 0 )
      {
         HB_COND_WAIT( Mutex->cond, Mutex->mutex );
      }
      Mutex->locker = HB_CURRENT_THREAD();
      Mutex->lock_count = 1;

      HB_CLEANUP_POP;
      HB_CRITICAL_UNLOCK( Mutex->mutex );

      HB_STACK_LOCK;
   }
}


/*
   Try to lock a mutex; return immediately on failure.
*/
HB_FUNC( HB_MUTEXTRYLOCK )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif

   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) hb_parpointer(1);

   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXTRYLOCK", 1, hb_paramError(1) );
      return;
   }

   if( HB_SAME_THREAD( Mutex->locker, HB_CURRENT_THREAD() ) )
   {
      Mutex->lock_count ++;
      hb_retl( TRUE );
   }
   else
   {
      HB_CRITICAL_LOCK( Mutex->mutex );
      if ( Mutex->locker != 0 )
      {
         hb_retl( FALSE );
      }
      else
      {
         Mutex->locker = HB_CURRENT_THREAD();
         Mutex->lock_count = 1;
         hb_retl( FALSE );
      }
      HB_CRITICAL_UNLOCK( Mutex->mutex );
   }
}

/*
   Unlocks a mutex; this succeeds only if the calling thread is
   the owner of the mutex, else the call is ignored.
*/
HB_FUNC( HB_MUTEXUNLOCK )
{
   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) hb_parpointer(1);

   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXUNLOCK", 1, hb_paramError(1) );
      return;
   }

   HB_CRITICAL_LOCK( Mutex->mutex );
   if( HB_SAME_THREAD( Mutex->locker, HB_CURRENT_THREAD()) )
   {
      Mutex->lock_count --;

      if( Mutex->lock_count == 0 )
      {
         Mutex->locker = 0;
         HB_COND_SIGNAL( Mutex->cond );
      }
   }
   HB_CRITICAL_UNLOCK( Mutex->mutex );
}


/*
   Short for subscribe/susbscribeNow, that are very similar.
   TODO: race condition notify/notifyAll()
*/
static void s_subscribeInternal( int mode )
{
#if defined(HB_API_MACROS)
   HB_THREAD_STUB
#endif
   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) hb_parpointer(1);
   PHB_ITEM pStatus = hb_param( 3, HB_IT_BYREF );
   int iWaitRes;
   ULONG ulWaitTime;

   /* Parameter error checking */
   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE || hb_pcount() > 3 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "SUBSCRIBE", 3,
         hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      return;
   }

   HB_STACK_UNLOCK;
   HB_CRITICAL_LOCK( Mutex->mutex );
   /* If we are subscribing now, we must flatten pre-notified data */
   if ( mode == 1 && hb_arrayLen( Mutex->aEventObjects ) > 0 )
   {
      hb_arraySize(  Mutex->aEventObjects, 0 );
   }

   /* warining; does not checks if the current thread is holding the mutex */
   Mutex->waiting ++;

   if ( hb_pcount() == 1 )
   {
      while( hb_arrayLen( Mutex->aEventObjects ) == 0 )
      {
         HB_COND_WAIT( Mutex->cond, Mutex->mutex );
      }
      iWaitRes = 0;  /* means success of wait */
   }
   else
   {
      ulWaitTime = hb_parnl(2);
      if ( ulWaitTime > 0 )
      {
         iWaitRes = HB_COND_WAITTIME( Mutex->cond, Mutex->mutex, ulWaitTime);
      }
      else
      {
         /* Remember that 0 means success */
         iWaitRes = hb_arrayLen( Mutex->aEventObjects ) == 0;
      }
   }

   /* No more waiting */
   Mutex->waiting --;

   if ( iWaitRes == 0 )
   {
      if ( pStatus )
      {
         pStatus->type = HB_IT_LOGICAL;
         pStatus->item.asLogical.value = 1;
      }
      hb_itemReturn( hb_arrayGetItemPtr( Mutex->aEventObjects, 1 ) );
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

   HB_CRITICAL_UNLOCK( Mutex->mutex );

   HB_STACK_LOCK;
}

/*
   Wait until the mutex is notified; accepts already notified objects
*/
HB_FUNC( SUBSCRIBE )
{
   s_subscribeInternal( 0 );
}

/*
   Wait until the mutex is notified; discard previous notifications
*/
HB_FUNC( SUBSCRIBENOW )
{
   s_subscribeInternal(1);
}

/*
   Signal that something meaningful has happened, and wake ONE subscriber.
*/
HB_FUNC( NOTIFY )
{
   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) hb_parpointer(1);
   PHB_ITEM pVal = hb_param( 2, HB_IT_ANY );

   /* Parameter error checking */
   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "NOTIFY", 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   HB_CRITICAL_LOCK( Mutex->mutex );
   if ( pVal == NULL )
   {
      /* add a NIL at bottom */
      hb_arraySize( Mutex->aEventObjects, hb_arrayLen( Mutex->aEventObjects ) + 1 );
   }
   else
   {
      hb_arrayAdd( Mutex->aEventObjects, pVal );
   }

   HB_COND_SIGNAL( Mutex->cond );
   HB_CRITICAL_UNLOCK( Mutex->mutex );
}


/*
   Signal that something meaningful has happened, and wake ALL subscribers
   that are CURRENTLY waiting. Prepares a copy of the notification object
   (if given) for each thread.
*/
HB_FUNC( NOTIFYALL )
{
   HB_MUTEX_STRUCT *Mutex = (HB_MUTEX_STRUCT *) hb_parpointer(1);
   PHB_ITEM pVal = hb_param( 2, HB_IT_ANY );
   BOOL bClear;
   int iWt;

   /* Parameter error checking */
   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "NOTIFYALL", 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   if ( pVal == NULL )
   {
      bClear = TRUE;
      pVal = hb_itemNew( NULL );
   }

   HB_CRITICAL_LOCK( Mutex->mutex );
   for( iWt = 0; iWt < Mutex->waiting; iWt++ )
   {
      hb_arrayAdd( Mutex->aEventObjects, pVal );
   }
   HB_COND_SIGNAL( Mutex->cond );
   HB_CRITICAL_UNLOCK( Mutex->mutex );

   if ( bClear )
   {
      hb_itemClear( pVal );
   }
}


#endif //thread support


/******************************************************************/
/* Part 7: Xharbour thread functions available in ST mode         */
/******************************************************************/

void hb_threadSleep( int millisec )
{
   HB_THREAD_STUB

   HB_STACK_UNLOCK;

   #if defined( HB_OS_DARWIN ) || defined(__DJGPP__)
      usleep( millisec * 1000 );
   #elif defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
      {
         struct timespec ts;
         ts.tv_sec = millisec / 1000;
         ts.tv_nsec = (millisec % 1000) * 1000000;
         nanosleep( &ts, 0 );
      }
   #elif defined(HB_OS_OS2)
      DosSleep( millisec );
   #else
      HB_TEST_CANCEL_ENABLE_ASYN;
      Sleep( millisec );
      HB_DISABLE_ASYN_CANC;
   #endif

   HB_STACK_LOCK;
}

HB_FUNC( THREADSLEEP )
{
   if( ! ISNUM( 1 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "THREADSLEEP", 1,
         hb_param(1, HB_IT_ANY) );
      return;
   }

   hb_threadSleep( hb_parni( 1 ) );
}

HB_FUNC( SECONDSSLEEP )
{

   int sleep;

   if( ! ISNUM( 1 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "SECONDSSLEEP", 1,
         hb_param(1, HB_IT_ANY) );
      return;
   }

   sleep = (int) (hb_parnd( 1 ) * 1000.0);
   hb_threadSleep( sleep );
}

/* Become thread inspector */
HB_FUNC( THREADINSPECT )
{
   #ifdef HB_THREAD_SUPPORT
   hb_threadWaitForIdle();
   #endif
}

/* Drop status of thread inspecting, and restart other threads */
HB_FUNC( THREADINSPECTEND )
{
   #ifdef HB_THREAD_SUPPORT
   hb_threadIdleEnd();
   #endif
}

HB_FUNC( THREADISINSPECTOR )
{
   HB_THREAD_STUB

   #ifdef HB_THREAD_SUPPORT
   /* This is atomically changed by idle inspectors; there is no need
      to lock it as non-idle-inspectors can only read it */
   hb_retl( HB_VM_STACK.uiIdleInspect > 0 );
   #else
   hb_retl( TRUE ); // always inspecting
   #endif
}
