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

/* JC1: Now including all this files to make threadsleep available in ST */
#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"
#include "classes.h"
#include "hbapirdd.h"

#if defined( HB_OS_WIN )
HB_EXTERN_BEGIN
extern HB_STACK * _TlsGetValue( void );
extern void _TlsSetValue( void * );
HB_EXTERN_END
#endif
#if defined( HB_OS_DARWIN ) || defined( __DJGPP__ )
   #include <stdlib.h>
   #include <unistd.h>    /* We need usleep() in Darwin */
#endif

#if defined( HB_OS_UNIX ) || \
   ( defined( __GNUC__ ) && ( ! defined( __RSXNT__ ) ) && ( ! defined( __CYGWIN__ ) ) )
      #include <sys/time.h>
      #include <time.h>
      #include <errno.h>
      #include <unistd.h>
#endif

#if defined( HB_OS_OS2 )
   #include <stdlib.h>
#endif

#ifdef HB_OS_WIN
   #include <windows.h>
   #include <process.h>
#endif

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
    #define extern
#endif

#include "thread.h"

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   #undef extern
#endif

#ifdef HB_THREAD_SUPPORT

/* FROM MEMVARS.C:
 * TODO:Move this in an appropriate file
 */
#define TABLE_INITHB_VALUE    100
#define TABLE_EXPANDHB_VALUE  50

/**************************************************************/
/* Part 1: Global objects declaration                         */
/**************************************************************/

#if defined( HB_THREAD_TLS_KEYWORD )
#if __GNUC__ >= 3 || defined( __BORLANDC__ )
   __thread HB_STACK * hb_thread_stack;
#elif defined( _MSC_VER )
   HB_STACK __declspec( thread ) * hb_thread_stack;
#else
   #error "This platform does not support __thread keyword; undefine HB_THREAD_TLS_KEYWORD & recompile"
#endif
#endif

HB_STACK *           hb_ht_stack = NULL;
HB_STACK *           last_stack  = NULL;
HB_MUTEX_STRUCT *    hb_ht_mutex;
HB_THREAD_T          hb_main_thread_id;

static BOOL          s_hb_threadIsInit = FALSE;
static HB_CRITICAL_T s_thread_unique_id_mutex;
static UINT          s_thread_unique_id;

#if defined( HB_OS_WIN )
#if ! defined( HB_VM_ALL )
   DWORD          hb_dwCurrentStack;
#endif
#elif defined( HB_OS_OS2 )
   PPVOID         hb_dwCurrentStack;
   HEV            hb_hevWakeUpAll; /* posted to wake up all threads waiting somewhere on an INDEFINITE wait */
#elif ! defined( HB_THREAD_TLS_KEYWORD )
   pthread_key_t  hb_pkCurrentStack;
#endif

HB_EXTERN_BEGIN

/* Declarations of shell mutexes */
HB_COND_T            hb_threadStackCond;

HB_CRITICAL_T        hb_globalsMutex;
HB_CRITICAL_T        hb_staticsMutex;
HB_CRITICAL_T        hb_memvarsMutex;
HB_CRITICAL_T        hb_macroMutex;
HB_CRITICAL_T        hb_allocMutex;
HB_CRITICAL_T        hb_garbageAllocMutex;
HB_CRITICAL_T        hb_outputMutex;
HB_CRITICAL_T        hb_mutexMutex;
HB_CRITICAL_T        hb_cancelMutex;
HB_CRITICAL_T        hb_dynsymMutex;

HB_SHARED_RESOURCE   hb_runningStacks;

BOOL                 hb_bIdleFence;


/**************************************************************/
/* Part 2: Initialization and termination of thread subsystem */
/**************************************************************/

void hb_threadInit( void )
{
   if( ! s_hb_threadIsInit )
   {
#ifdef HB_OS_OS2
      /* It is ahead of all initializations since every condition requires it */
      DosCreateEventSem( NULL, ( PHEV ) &hb_hevWakeUpAll, 0L, FALSE );
#endif

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

#if defined( HB_OS_UNIX ) && ! defined( HB_OS_LINUX ) && ! defined( HB_OS_BSD )
      /* If your OS doesn't support trylock, you can implement it here;
       * if it does, add it to #if. */
/*    HB_CRITICAL_INIT( s_mtxTryLock ); */
#endif

      last_stack        = NULL;
      hb_ht_stack       = &hb_stackMT;
      hb_main_thread_id = HB_CURRENT_THREAD();

      hb_ht_mutex       = NULL;

      /* Idle fence is true by default */
      hb_bIdleFence     = TRUE;

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
      HB_CRITICAL_INIT( hb_cancelMutex );
#endif

#if ! defined( HB_THREAD_TLS_KEYWORD )
      /* hb_thread_stack does not need initialization */
#ifdef HB_OS_WIN
      hb_dwCurrentStack = TlsAlloc();
      TlsSetValue( hb_dwCurrentStack, ( void * ) hb_ht_stack );
#elif defined( HB_OS_OS2 )
      DosAllocThreadLocalMemory( 1, ( PULONG * ) &hb_dwCurrentStack );
      *hb_dwCurrentStack = ( PVOID ) hb_ht_stack;
#else
      pthread_key_create( &hb_pkCurrentStack, NULL );
      pthread_setspecific( hb_pkCurrentStack, ( void * ) hb_ht_stack );
#endif
#else
#if ! defined( HB_THREAD_TLS_BUG )
      hb_thread_stack   = hb_ht_stack;
#endif
#endif
      s_hb_threadIsInit = TRUE;
   }
}

#if defined( HB_OS_WIN )
HB_STACK * _TlsGetValue( void )
{
   return ( HB_STACK * ) TlsGetValue( hb_dwCurrentStack );
}

void _TlsSetValue( void * p )
{
   TlsSetValue( hb_dwCurrentStack, p );
}
#endif

void hb_threadExit( void )
{
   if( s_hb_threadIsInit )
   {
      /* Destroyng all shell locks mutexes */
      HB_SHARED_DESTROY( hb_runningStacks );
      HB_CRITICAL_DESTROY( hb_mutexMutex );
      HB_CRITICAL_DESTROY( hb_outputMutex );
      HB_CRITICAL_DESTROY( hb_macroMutex );
      HB_CRITICAL_DESTROY( hb_garbageAllocMutex );
      HB_CRITICAL_DESTROY( hb_allocMutex );
      HB_CRITICAL_DESTROY( hb_dynsymMutex );
      HB_COND_DESTROY( hb_threadStackCond );

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
      HB_CRITICAL_DESTROY( hb_cancelMutex );
#endif

#if ! defined( HB_THREAD_TLS_KEYWORD )
#ifdef HB_OS_WIN
      TlsFree( hb_dwCurrentStack );
#elif defined( HB_OS_OS2 )
      DosFreeThreadLocalMemory( ( PULONG ) &hb_dwCurrentStack );
      DosCloseEventSem( hb_hevWakeUpAll );
#else
      pthread_key_delete( hb_pkCurrentStack );
#endif
#endif

#if defined( HB_OS_UNIX ) && ! defined( HB_OS_LINUX ) && ! defined( HB_OS_BSD )
      /* If your OS doesn't support trylock, you can implement it here;
       * if it does, add it to #if. */
/*    HB_CRITICAL_DESTROY( s_mtxTryLock ); */
#endif
      HB_CRITICAL_DESTROY( s_thread_unique_id_mutex );
      hb_ht_stack       = NULL;
      last_stack        = NULL;
      s_hb_threadIsInit = FALSE;
   }
}

/**************************************************************/
/* Part 2: Thread stack internal management                   */
/**************************************************************/

static UINT hb_threadUniqueId( void )
{
   volatile UINT uiRet;

   HB_CRITICAL_LOCK( s_thread_unique_id_mutex );

   if( s_thread_unique_id == HB_THREAD_MAX_UNIQUE_ID )
      s_thread_unique_id = 1;

   uiRet = s_thread_unique_id;
   s_thread_unique_id++;

   HB_CRITICAL_UNLOCK( s_thread_unique_id_mutex );

   return uiRet;
}

/*
   Creating a new stack
 */
HB_STACK * hb_threadCreateStack( HB_THREAD_T th )
{
   HB_STACK * tc;

   HB_TRACE( HB_TR_DEBUG, ( "hb_threadCreateStack(...)" ) );

   tc = ( HB_STACK * ) hb_xgrab( sizeof( HB_STACK ) );
   hb_threadSetupStack( tc, th );

   return tc;
}

/*
   Filling a new stack with default values
 */
void hb_threadSetupStack( HB_STACK * tc, HB_THREAD_T th )
{
   ULONG uCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_threadSetupStack(%p, ...)", tc ) );

   hb_stack_init( tc );

   tc->th_id                  = th;
   tc->uiIdleInspect          = 0;
   /* In unix, is the thread that sets up its data. */
   #if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   tc->th_vm_id               = hb_threadUniqueId();
   #endif

   tc->next                   = NULL;

   tc->pMethod                = NULL;
   tc->pSyncId                = NULL;
   tc->bInUse                 = FALSE;
   tc->bActive                = FALSE;
   tc->iPcodeCount            = HB_VM_UNLOCK_PERIOD;

   tc->errorHandler           = NULL;
   tc->errorBlock             = hb_itemNew( NULL );
   tc->aTryCatchHandlerStack  = hb_itemArrayNew( 0 );

   tc->uiVMFlags              = 0;
   tc->lRecoverBase           = 0;

   tc->iLaunchCount           = 0;
   tc->uiErrorDOS             = 0;

   tc->pBackgroundTasks       = NULL;
   tc->bIamBackground         = 0;
   tc->uiBackgroundTask       = 0;
   tc->uiBackgroundMaxTask    = 0;
   tc->ulBackgroundID         = 0;

#if defined( HB_OS_WIN )
   tc->th_h                   = NULL;
#endif
#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   tc->bCanceled              = FALSE;
   tc->bCanCancel             = FALSE;

   tc->iCleanCount            = 0;
   tc->pCleanUp               = ( HB_CLEANUP_FUNC * ) hb_xgrab( sizeof( HB_CLEANUP_FUNC ) * HB_MAX_CLEANUPS );
   tc->pCleanUpParam          = ( void ** ) hb_xgrab( sizeof( void * ) * HB_MAX_CLEANUPS );
#endif

   /* Initialization of "foreach" and "with object" */
   for( uCount = 0; uCount < HB_MAX_WITH_OBJECTS; uCount++ )
   {
      tc->aWithObject[ uCount ].type = HB_IT_NIL;
   }
   tc->wWithObjectCounter = 0;

   for( uCount = 0; uCount < HB_MAX_ENUMERATIONS; uCount++ )
   {
      tc->aEnumCollection[ uCount ].type  = HB_IT_NIL;
      tc->awEnumIndex[ uCount ]           = 0;
   }
   tc->wEnumCollectionCounter = 0;

   tc->pSequence              = NULL;

   tc->pFinally               = NULL;

   /* Dynsym Thread Specific table. */
   tc->uiClosestDynSym        = 0;
   tc->pDynItems              = NULL;
   tc->uiDynSymbols           = 0;

   /* initialization of macro & codeblock parameter passing */
   tc->iExtraParamsIndex      = 0;
   tc->iExtraElementsIndex    = 0;
   tc->iExtraElements         = 0;
   tc->iExtraIndex            = 0;

   /* Initialization of private and public memvars */
   hb_memvarsInit( tc );

   /* Initialization of dynsym pointers to memvars */
   /*tc->hMemvars = (HB_HANDLE *) hb_xgrab( sizeof( HB_HANDLE ) * TABLE_INITHB_VALUE );
      tc->hMemvarsAllocated = TABLE_INITHB_VALUE;
      tc->hMemvarsLastFree = 1;
      tc->hMemvars[0]= 0;*/

   tc->pThreadID     = NULL;
   tc->pThreadReady  = NULL;
}

/*
   This functions fills the stack just before thread creations, with the
   arguments for the new thread main routine.
 */
void hb_threadFillStack( HB_STACK * pStack, PHB_ITEM pArgs )
{
   PHB_ITEM pPointer;
   PHB_ITEM pItem, * pPos;
   USHORT   uiParam = 1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_threadFillStack(%p, %p)", pStack, pArgs ) );

   pPos     = pStack->pPos;
   pPointer = hb_arrayGetItemPtr( pArgs, 1 );

   if( HB_IS_SYMBOL( pPointer ) )
   {
      hb_itemPutSymbol( *pPos, pPointer->item.asSymbol.value );

      pPos++;
      ( *pPos )->type = HB_IT_NIL;

      if( pStack->bIsMethod )
      {
         pItem    = hb_arrayGetItemPtr( pArgs, 2 );
         hb_itemCopy( ( *pPos ), pItem );
         uiParam  = 3;
      }
      else
      {
         ( *pPos )->type   = HB_IT_NIL;
         uiParam           = 2;
      }

      pPos++;
      ( *pPos )->type = HB_IT_NIL;
   }
   else if( HB_IS_BLOCK( pPointer ) )
   {
      hb_itemPutSymbol( *pPos, &hb_symEval );
      ( *pPos )->item.asSymbol.pCargo->stackbase   = (long) (pPos - pStack->pItems);

      pPos++;
      ( *pPos )->type                              = HB_IT_NIL;
      hb_itemCopy( ( *pPos ), pPointer );

      pPos++;
      ( *pPos )->type   = HB_IT_NIL;
      uiParam           = 2;
   }

   for(; uiParam <= ( USHORT ) pStack->uiParams; uiParam++ )
   {
      hb_itemCopy( ( *pPos ), hb_arrayGetItemPtr( pArgs, uiParam ) );
      pPos++;
      ( *pPos )->type = HB_IT_NIL;
   }

   pStack->pPos = pPos;

   hb_itemRelease( pArgs );
}


/*JC1: WARNING
 * this function is not locked because is meant to be called ONLY within
 * a thread stack locked area.
 */
HB_STACK * hb_threadLinkStack( HB_STACK * tc )
{
   HB_STACK ** p;

   HB_TRACE( HB_TR_DEBUG, ( "hb_threadLinkStack(%p)", tc ) );

   p = &hb_ht_stack;

   while( *p )
   {
      p = &( *p )->next;
   }

   *p       = tc;
   tc->next = NULL;

   /* last_stack = *p; */

   return tc;
}

/*
   Destroys a stack; on exit, even pStack is destroyed.
 */
void hb_threadDestroyStack( HB_STACK * pStack )
{
   LONG        i;
   PHB_ITEM *  pPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_threadDestroyStack(%p)", pStack ) );

   pStack->bActive = FALSE;

   /* Free each element of the stack, but not for main stack; main stack
      is freed by hb_stackFree as in ST */
/* if( pStack != &hb_stackMT ) */
   {
      for( pPos = pStack->pItems; pPos < pStack->pPos; pPos++ )
      {
         if( HB_IS_COMPLEX( *pPos ) )
            hb_itemClear( *pPos );
      }

      /* Free each element of the stack */
      for( i = 0; i < pStack->wItems; i++ )
      {
         hb_xfree( pStack->pItems[ i ] );
      }
      /* Free the stack */

      hb_xfree( pStack->pItems );
   }

   /* Eventually free the return value of the stack */
   if( HB_IS_COMPLEX( &( pStack->Return ) ) )
      hb_itemClear( &( pStack->Return ) );

   /* Error handler is never allocated; it resides in the stack, or
      is owned by callers. */
   if( pStack->errorBlock )
      hb_itemRelease( pStack->errorBlock );

   if( pStack->aTryCatchHandlerStack )
      hb_itemRelease( pStack->aTryCatchHandlerStack );

   /* releases this thread's memvars
    * if( pStack != &hb_stackMT )
    */
   {
      /* Main thread should have them removed before arriving here.
       */
      hb_memvarsRelease( pStack );
   }

/*
   if( pStack->hMemvars )
      hb_xfree( pStack->hMemvars );
 */

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   hb_xfree( pStack->pCleanUp );
   hb_xfree( pStack->pCleanUpParam );
   pStack->iCleanCount = -1;
#endif

   /* Free Sync method table
    */
   if( pStack->pSyncId )
      hb_xfree( pStack->pSyncId );

   /* Decrement counter reference and free ThreadReady structure
    */
   if( pStack->pThreadReady )
   {
      pStack->pThreadReady->bActive = FALSE;

      if( HB_ATOMIC_DEC( pStack->pThreadReady->ulCounter ) == 0 )
         hb_xfree( pStack->pThreadReady );
   }

   hb_rddWaShutDown( pStack->rdd );

   /* Free only if we are not destroying the main stack
    */
   if( pStack != &hb_stackMT )
   {
      hb_setRelease( &pStack->set );

      while( pStack->pSequence )
      {
         PHB_SEQUENCE pFree = pStack->pSequence;

         pStack->pSequence = pStack->pSequence->pPrev;

         hb_xfree( ( void * ) pFree );
      }

      hb_xfree( pStack );
   }
}

/*
   Links a stack in the stack list; to be used ONLY while holding the
   hb_threadStack mutex.
 */

HB_STACK * hb_threadUnlinkStack( HB_STACK * pStack )
{
   HB_STACK ** p;

   p = &hb_ht_stack;

   while( *p && *p != pStack )
   {
      p = &( *p )->next;
   }

   if( *p )
   {
      *p = ( *p )->next;

      if( *p == last_stack )
         last_stack = NULL;
   }
   else
   {
      char errdat[ 64 ];

      hb_snprintf( errdat, sizeof( errdat ), "Stack not found for Thread %ld", ( LONG ) pStack->th_id );
      hb_errRT_BASE_SubstR( EG_CORRUPTION, 10001, errdat, "hb_threadUnlinkStack", 0 );
   }

   HB_COND_SIGNAL( hb_threadStackCond );

   return *p;
}

/*
   Find a stack given the threadID of the owner; rises an error if
   not found.
 */

HB_STACK * hb_threadGetStack( HB_THREAD_T id )
{
   HB_STACK * p;

   HB_SHARED_LOCK( hb_runningStacks );

   if( last_stack && HB_SAME_THREAD( last_stack->th_id, id ) )
      p = last_stack;
   else
   {
      p = hb_ht_stack;

      while( p && ! HB_SAME_THREAD( p->th_id, id ) )
      {
         p = p->next;
      }

      if( p )
         last_stack = p;

   }

   if( ! p )
      hb_errRT_BASE_SubstR( EG_ARG, 1099, "Can't find thread ID", "INTERNAL THREADING", 1, hb_paramError( 1 ) );

   HB_SHARED_UNLOCK( hb_runningStacks );
   return p;
}

/*
   to be internally used by functions willing to know if there is a stack
 */
HB_STACK * hb_threadGetStackNoError( HB_THREAD_T id )
{
   HB_STACK * p;

   HB_SHARED_LOCK( hb_runningStacks );

   if( last_stack && HB_SAME_THREAD( last_stack->th_id, id ) )
      p = last_stack;
   else
   {
      p = hb_ht_stack;

      while( p && ! HB_SAME_THREAD( p->th_id, id ) )
      {
         p = p->next;
      }

      if( p )
         last_stack = p;
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
   HB_STACK *  pStack;
   UINT        i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmIsLocalRef()" ) );

   pStack = hb_ht_stack;

   while( pStack )
   {
      if( HB_IS_GCITEM( &( pStack->Return ) ) )
         hb_gcItemRef( &( pStack->Return ) );

      if( pStack->pPos > pStack->pItems )
      {
         PHB_ITEM * pItem = pStack->pPos - 1;

         while( pItem != pStack->pItems )
         {
            if( HB_IS_GCITEM( *pItem ) )
               hb_gcItemRef( *pItem );

            --pItem;
         }
      }

      /* FOR EACH Enumerations.
       */
      for( i = 0; i < pStack->wEnumCollectionCounter; i++ )
      {
         if( HB_IS_GCITEM( &( pStack->aEnumCollection[ i ] ) ) )
            hb_gcItemRef( &( pStack->aEnumCollection[ i ] ) );
      }

      /* WITH OBJECT
       */
      for( i = 0; i < pStack->wWithObjectCounter; i++ )
      {
         if( HB_IS_GCITEM( &( pStack->aWithObject[ i ] ) ) )
            hb_gcItemRef( &( pStack->aWithObject[ i ] ) );
      }

      /* stack memvars:
       */
      hb_memvarsIsMemvarRef( pStack );

      pStack = pStack->next;
   }

}

/* DEBUG FUNCTION:
   Stack count can NEVER be accurate; but it is useful to know about
   prorgram status or progress.
 */
int hb_threadCountStacks( void )
{
   HB_STACK *  p;
   int         count = 0;

   /* never unlinks the main stack */
   HB_SHARED_LOCK( hb_runningStacks );

   p = hb_ht_stack;
   while( p )
   {
      count++;
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

   if( _pStack_->hMemvarsAllocated <= _pStack_->hMemvarsLastFree )
   {
      _pStack_->hMemvars            = ( HB_HANDLE * ) hb_xrealloc( _pStack_->hMemvars,
                                                                   sizeof( HB_HANDLE ) * ( _pStack_->hMemvarsAllocated + TABLE_EXPANDHB_VALUE ) );
      _pStack_->hMemvarsAllocated   += TABLE_EXPANDHB_VALUE;
   }

   pDyn->hMemvar                                      = _pStack_->hMemvarsLastFree;
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
#ifdef HB_OS_WIN
/*   DWORD WINAPI hb_create_a_thread( LPVOID Cargo ) */
unsigned __stdcall hb_create_a_thread( void * Cargo )
#else
void * hb_create_a_thread( void * Cargo )
#endif
{
   HB_STACK *  _pStack_ = ( HB_STACK * ) Cargo;
   PHB_DYNS    pExecSym;

   /* Sets the cancellation handler so small delays in
      cancellation do not cause segfault or memory leaks */
#if defined( HB_OS_WIN )
#ifdef HB_THREAD_TLS_KEYWORD
   hb_thread_stack = _pStack_;
#else
   TlsSetValue( hb_dwCurrentStack, ( void * ) _pStack_ );
#endif

#elif defined( HB_OS_OS2 )
   *hb_dwCurrentStack = ( void * ) _pStack_;
#else
   /* The first that arrives among father and child will set up
      the stack id. */
   _pStack_->th_id   = HB_CURRENT_THREAD();
#ifdef HB_THREAD_TLS_KEYWORD
   hb_thread_stack   = _pStack_;
#else
   pthread_setspecific( hb_pkCurrentStack, Cargo );
#endif
   pthread_cleanup_push( hb_threadTerminator, NULL );
#endif

   /* call errorsys() to initialize errorblock
    */
   pExecSym = hb_dynsymFind( "ERRORSYS" );

   if( pExecSym )
   {
      hb_vmPushSymbol( pExecSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo( 0 );
   }

   if( _pStack_->bIsMethod )
      hb_vmSend( ( USHORT ) ( HB_VM_STACK.uiParams - 2 ) );
   else
      hb_vmDo( ( USHORT ) ( HB_VM_STACK.uiParams - 1 ) );

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   hb_threadCancelInternal();    /* never returns */
   return 0;
#else
   /* After this point, prevent cancellation, so we can have a clean
      quit. Otherwise, a cancellation request could be issued inside the
      cleanup pop cycle, causing re-entering in the cleanup functions */
   {
      int oldstate;

      /* The second parameter is not optional in Darwin! */
      pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, &oldstate );
   }
   /* pop cleanup; also calls the cleanup function */
   pthread_cleanup_pop( 1 );
   return NULL;
#endif
}

/*
   Cancels a thread; called internally by the windows-specific thread
   killer; It must be used by the target thread to cancel itself.
 */

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
void hb_threadCancelInternal( void )
{
   HB_THREAD_STUB
   int iCount;

   /* Make sure we are not going to be canceled */
   HB_DISABLE_ASYN_CANC;


   iCount = HB_VM_STACK.iCleanCount;
   while( iCount > 0 )
   {
      iCount--;
      HB_VM_STACK.pCleanUp[ iCount ]( HB_VM_STACK.pCleanUpParam[ iCount ] );
   }
   /* the stack must have been destroyed by the last cleanup function
    */

   hb_threadTerminator( &HB_VM_STACK );
/* #ifndef __BORLANDC__
   ExitThread( 0 );
   #else*/
#ifdef HB_OS_OS2
   _endthread();
#else
   _endthreadex( 0 );
#endif
/*  #endif */
}

/***
 * Warning: cancel mutex must be held before calling this one
 * NEVER use this function to cancel the calling thread.
 */
void hb_threadCancel( HB_STACK * pStack )
{
   HB_THREAD_STUB
   /*
    * previous section using kind cancellation
    * CONTEXT context;
    * // stack resource mutex is being locked now
    * pStack->bInUse = TRUE;  // mark the stack as used
    * SuspendThread( pStack->th_h ); // stop thread before he can do something with stack

    * context.ContextFlags = CONTEXT_CONTROL;
    * GetThreadContext(  pStack->th_h , &context);
    * // _x86 only!!!
    * context.Eip = (DWORD)hb_threadCancelInternal;
    * SetThreadContext(  pStack->th_h , &context);
    * ResumeThread(  pStack->th_h );
    * HB_CRITICAL_UNLOCK( hb_cancelMutex );
    */

#ifdef HB_OS_OS2
   DosKillThread( pStack->th_id );
#else
   TerminateThread( pStack->th_h, 0 );
#endif
   HB_DISABLE_ASYN_CANC;
   HB_CRITICAL_UNLOCK( hb_cancelMutex );

   hb_threadTerminator( ( void * ) pStack );

}
#else
void hb_threadCancelInternal()
{
   HB_THREAD_STUB
   hb_threadTerminator(&HB_VM_STACK );
   pthread_exit( 0 );
}
#endif

/*
   Standard thread termination routine; this is called by a thread
   to cleanup the things; at the exit of this function, the thread
   is terminated.
 */
void hb_threadTerminator( void * pData )
{
   HB_MUTEX_STRUCT * pMtx;
   PHB_THREAD_ID     pThreadId;

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   HB_STACK *        _pStack_ = ( HB_STACK * ) pData;
#else
   #ifdef HB_THREAD_TLS_KEYWORD
   HB_STACK *        _pStack_ = hb_thread_stack;
   #else
   HB_STACK *        _pStack_ = ( HB_STACK * ) pthread_getspecific( hb_pkCurrentStack );
   #endif
   HB_SYMBOL_UNUSED( pData );
#endif

#if ! defined( HB_OS_WIN ) && ! defined( HB_OS_OS2 )
   {
      int oldstate;

      /* The second parameter is not optional in Darwin! */
      pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, &oldstate );
   }
#endif

   HB_STACK_LOCK;

#if defined( HB_OS_WIN )
   CloseHandle( _pStack_->th_h );

#elif defined( HB_OS_OS2 )
   /* nothing to do */

#else
   pthread_detach( HB_CURRENT_THREAD() );
#endif

   /* eventually unlocks held mutexes */
   HB_CRITICAL_LOCK( hb_mutexMutex );
   pMtx = hb_ht_mutex;
   while( pMtx != NULL )
   {
      if( HB_SAME_THREAD( pMtx->locker, _pStack_->th_id ) )
         hb_mutexForceUnlock( pMtx );

      pMtx = pMtx->next;
   }
   HB_CRITICAL_UNLOCK( hb_mutexMutex );

   /* we are out of business */
   HB_SHARED_LOCK( hb_runningStacks );

   pThreadId = _pStack_->pThreadID;
   while( pThreadId )
   {
      pThreadId->pStack = NULL;
      pThreadId         = pThreadId->next;
   }

   /* now we can detach this thread */
   hb_threadUnlinkStack( _pStack_ );
   hb_threadDestroyStack( _pStack_ );

   if( --hb_runningStacks.content.asLong < 1 )
      HB_SHARED_SIGNAL( hb_runningStacks );

   HB_SHARED_UNLOCK( hb_runningStacks );
}

/*
   Waits for the running stack counter to reach 0 (and removes
   it from the running threads pool). The calling thread is
   then NOT an idle inspector: it just RESTARTS after all the
   others are done.

   IT is meant to be called from the main thread.
 */
void hb_threadWaitAll( void )
{
   HB_THREAD_STUB

   /* refuse to run if we are not the main thread
    */
   if( ! HB_SAME_THREAD( hb_main_thread_id, HB_CURRENT_THREAD() ) )
      return;

   HB_SHARED_LOCK( hb_runningStacks );
   hb_runningStacks.content.asLong--;
   HB_VM_STACK.bInUse = 0;
   HB_SHARED_SIGNAL( hb_runningStacks );

   while( hb_runningStacks.content.asLong > 0 || hb_ht_stack->next != NULL )
   {
      HB_SHARED_WAIT( hb_runningStacks );
   }

   /* no more threads now */
   hb_runningStacks.content.asLong++;
   HB_VM_STACK.bInUse = 1;
   HB_SHARED_UNLOCK( hb_runningStacks );
}

/*
   Kill all the threads except the main one. Must be called
   from the main thread.
 */
void hb_threadKillAll( void )
{
   HB_STACK * pStack;

   hb_threadWaitForIdle();

   pStack = hb_ht_stack;
   while( pStack )
   {
      /* DO NOT destroy main thread stack */
      if( HB_SAME_THREAD( pStack->th_id, hb_main_thread_id )
          || HB_SAME_THREAD( pStack->th_id, HB_CURRENT_THREAD() ) )
      {
         pStack = pStack->next;
         continue;
      }
#if ! defined( HB_OS_WIN ) && ! defined( HB_OS_OS2 )
      /* Allows the target thread to cleanup if and when needed.
       */
      pthread_cancel( pStack->th_id );
      pStack = pStack->next;
#else
      if( ! pStack->bCanCancel )
         pStack->bCanceled = TRUE;
      else
      {
         HB_MUTEX_STRUCT * pMtx;
         HB_STACK *        pNext;

         /* This is a subset of terminateThread: as this routine is
            an idle inspector, many of the cares in terminateThread may
            NOT be applied. */
#ifdef HB_OS_OS2
         DosKillThread( pStack->th_id );
#else
         TerminateThread( pStack->th_h, 0 );
         CloseHandle( pStack->th_h );
#endif

         pMtx = hb_ht_mutex;
         while( pMtx != NULL )
         {
            if( HB_SAME_THREAD( pMtx->locker, pStack->th_id ) )
               hb_mutexForceUnlock( pMtx );

            pMtx = pMtx->next;
         }

         pNext = pStack->next;
         /* now we can detach this thread */
         hb_threadUnlinkStack( pStack );
         hb_threadDestroyStack( pStack );
         pStack = pNext;
      }
#endif
   }
   hb_threadIdleEnd();
}

/*
   Used as a cleanup routines for possible cancelations in
   idle inspectors.
 */
void hb_threadResetAux( void * ptr )
{
   ( ( HB_SHARED_RESOURCE * ) ptr )->aux = 0;
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
   if( hb_bIdleFence )
      /* blocks all the threads */
      hb_runningStacks.aux = 1;

   hb_runningStacks.content.asLong--;

   HB_CLEANUP_PUSH( hb_threadResetAux, hb_runningStacks );
   HB_VM_STACK.bInUse = FALSE;

   /* wait until the road is clear (only WE are running) */
   while( hb_runningStacks.content.asLong != 0 )
   {
      HB_SHARED_WAIT( hb_runningStacks );
   }
   /* blocks all threads here if not blocked before */
   hb_runningStacks.aux = 1;

   /* And also prevents other idle inspectors to go */
   hb_runningStacks.content.asLong++;

   /* And this allows ourself to ignore our stack lock requests,
      being then able to run PRG level code */
   HB_VM_STACK.uiIdleInspect++;

   /* no need to signal, no one must be awaken
    */
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
   /* this will also signal the changed situation.
    */
   HB_SHARED_SIGNAL( hb_runningStacks );
}

/*
   Condition variables needs a special handling to be omomorphic on
   various systems.
 */
#if ! defined( HB_OS_WIN ) && ! defined( HB_OS_OS2 )

int hb_condTimeWait( pthread_cond_t * cond, pthread_mutex_t * mutex, int iMillisec )
{
   struct timeval    now;
   struct timespec   timeout;

   gettimeofday( &now, NULL );
   timeout.tv_nsec   = ( now.tv_usec + ( ( iMillisec % 1000l ) * 1000l ) ) * 1000l;
   timeout.tv_sec    = now.tv_sec + ( iMillisec / 1000l ) + timeout.tv_nsec / 1000000000l;
   timeout.tv_nsec   %= 1000000000l;
   return pthread_cond_timedwait( cond, mutex, &timeout );
}

#endif

#ifdef HB_OS_WIN
/***************************************************
   Posix like condition variable for WIN32
   Based on the Terekhov - Thomas algorithm version 9
 */

/*
   Init the data needed for the condition variable.
 */
BOOL hb_threadCondInit( HB_WINCOND_T * cond )
{
   cond->nWaitersGone      = 0;
   cond->nWaitersBlocked   = 0;
   cond->nWaitersToUnblock = 0;

   InitializeCriticalSection( &( cond->mtxUnblockLock ) );
   cond->semBlockLock      = NULL;
   cond->semBlockQueue     = NULL;
   cond->semBlockLock      = CreateSemaphore( NULL, 1, 20000000, NULL );

   if( cond->semBlockLock != NULL )
   {
      cond->semBlockQueue = CreateSemaphore( NULL, 0, 20000000, NULL );

      if( cond->semBlockQueue == NULL )
         return FALSE;
   }
   else
      return FALSE;

   return TRUE;
}

/*
   Destroys the condition variable.
 */
void hb_threadCondDestroy( HB_WINCOND_T * cond )
{
   DeleteCriticalSection( &( cond->mtxUnblockLock ) );

   if( cond->semBlockLock != NULL )
      CloseHandle( cond->semBlockLock );

   if( cond->semBlockQueue != NULL )
      CloseHandle( cond->semBlockQueue );
}

/*
   Issues a signal, that is, wake ALL the threads who are waiting NOW
   and ONLY them.
 */
void hb_threadCondSignal( HB_WINCOND_T * cond )
{
   register int nSignalsToIssue;

   EnterCriticalSection( &( cond->mtxUnblockLock ) );

   if( cond->nWaitersToUnblock )
   {
      if( ! cond->nWaitersBlocked )           /* NO-OP */
      {
         LeaveCriticalSection( &cond->mtxUnblockLock );
         return;
      }
      nSignalsToIssue         = cond->nWaitersBlocked;
      cond->nWaitersToUnblock += nSignalsToIssue;
      cond->nWaitersBlocked   = 0;
   }
   else if( cond->nWaitersBlocked > cond->nWaitersGone )    /* HARMLESS RACE CONDITION! */
   {
      WaitForSingleObject( cond->semBlockLock, INFINITE );
      if( cond->nWaitersGone )
      {
         cond->nWaitersBlocked   -= cond->nWaitersGone;
         cond->nWaitersGone      = 0;
      }
      nSignalsToIssue         = cond->nWaitersToUnblock = cond->nWaitersBlocked;
      cond->nWaitersBlocked   = 0;
   }
   else   /* NO-OP */
   {
      LeaveCriticalSection( &( cond->mtxUnblockLock ) );
      return;
   }

   LeaveCriticalSection( &( cond->mtxUnblockLock ) );
   ReleaseSemaphore( cond->semBlockQueue, nSignalsToIssue, NULL );
}

/*
   Wait for a signal to be issued (at maximum for a given time or INFINITE)
 */
BOOL hb_threadCondWait( HB_WINCOND_T * cond, CRITICAL_SECTION * mutex,
                        DWORD dwTimeout )
{
   HB_THREAD_STUB

   register int   nSignalsWasLeft;
   register int   bTimeout;

   WaitForSingleObject( cond->semBlockLock, INFINITE );
   cond->nWaitersBlocked++;
   ReleaseSemaphore( cond->semBlockLock, 1, NULL );
   LeaveCriticalSection( mutex );

   HB_TEST_CANCEL_ENABLE_ASYN
   bTimeout = ( WaitForSingleObject( cond->semBlockQueue, dwTimeout ) == WAIT_OBJECT_0 ) ? 1 : 0 ;
   HB_DISABLE_ASYN_CANC

   EnterCriticalSection(&cond->mtxUnblockLock );

   if( ( nSignalsWasLeft = cond->nWaitersToUnblock ) != 0 )
      cond->nWaitersToUnblock--;
   else if( ++cond->nWaitersGone == 2000000000L )
   {
      WaitForSingleObject( cond->semBlockLock, INFINITE );
      cond->nWaitersBlocked   -= cond->nWaitersGone;
      ReleaseSemaphore( cond->semBlockLock, 1, NULL );
      cond->nWaitersGone      = 0;
   }

   LeaveCriticalSection( &( cond->mtxUnblockLock ) );

   if( nSignalsWasLeft == 1 )
      ReleaseSemaphore( cond->semBlockLock, 1, NULL );

   EnterCriticalSection( mutex );
   return ! bTimeout;
}
#endif /* win32 */


#ifdef HB_OS_OS2

/* Condition management for OS/2 - <maurilio.longo@libero.it>

   Here I use two different systems to prevent zombie processes.
   - Conditions use a MuxWait semaphore, that is a semaphore which is made up of other semaphores
     and "returns" when any of the semaphores which make it up gets posted. This way I can
     wake up all threads waiting on a semaphore posting a single, global, wake-up-all semaphore
   - Mutexes do a "polling" wait, that is, instead of waiting forever on a mutex request there is a "short"
     wait followed by a test on the same semaphore used by conditions to be awaken when needed; if this
     semaphore has been posted, mutexes are not requested (or are released as soon as they are owned) any more
     I do know that this wastes a few cpu cycles, but I do prefer this to an unkillable process.
 */

BOOL hb_threadCondInit( HB_COND_T * cond )
{
   HEV         CondSem;
   SEMRECORD   apsr[ 2 ];
   APIRET      rc;

   if( ( rc = DosCreateEventSem( NULL, ( PHEV ) &CondSem, 0L, FALSE ) ) == NO_ERROR )
   {
      apsr[ 0 ].hsemCur = ( HSEM ) CondSem;
      apsr[ 0 ].ulUser  = 1;
      apsr[ 1 ].hsemCur = ( HSEM ) hb_hevWakeUpAll;
      apsr[ 1 ].ulUser  = 2;

      if( ( rc = DosCreateMuxWaitSem( NULL, ( PHMUX ) cond, 2L, ( PSEMRECORD ) apsr, DCMW_WAIT_ANY ) ) != NO_ERROR )
         /* printf("DosCreateMuxWaitSem rc = %u\r\n", rc);
          * raise runtime error
          */
         return FALSE;
   }
   else
      /* printf("DosCreateEventSem rc = %u\r\n", rc);
       * raise runtime error
       */
      return FALSE;

   return TRUE;
}

void hb_threadCondDestroy( HB_COND_T * cond )
{
   SEMRECORD   apsr[ 2 ];
   ULONG       ulSemCount = 2, pflAttr;

   DosQueryMuxWaitSem( *cond, &ulSemCount, ( PSEMRECORD ) apsr, &pflAttr );

   if( DosCloseEventSem( ( HEV ) apsr[ 0 ].hsemCur ) == ERROR_SEM_BUSY )
   {
      DosPostEventSem( ( HEV ) apsr[ 0 ].hsemCur );
      DosCloseEventSem( ( HEV ) apsr[ 0 ].hsemCur );
   }

   DosCloseMuxWaitSem( *cond );
}

BOOL hb_threadCondWait( HB_COND_T * cond, CRITICAL_SECTION * mutex, DWORD dwTimeout )
{
   HB_THREAD_STUB
   register int   bTimeout;
   ULONG          ulPostedSem;

   HB_TEST_CANCEL_ENABLE_ASYN

   DosReleaseMutexSem( * mutex );

   bTimeout = ( DosWaitMuxWaitSem( *cond, dwTimeout, &ulPostedSem ) != NO_ERROR ) ? 1 : 0;

   HB_DISABLE_ASYN_CANC

   if( ulPostedSem == 1 )
      hb_threadMtxPoll( *mutex );

   return ! bTimeout;
}

void hb_threadCondSignal( HB_COND_T * cond )
{
   ULONG       ulPostCount;
   SEMRECORD   apsr[ 2 ];
   ULONG       ulSemCount = 2, pflAttr;
   APIRET      rc;

   rc = DosQueryMuxWaitSem( *cond, &ulSemCount, ( PSEMRECORD ) apsr, &pflAttr );

   if( rc != NO_ERROR )
   {
      /* printf("DosQueryMuxWaitSem() rc %u count %u\r\n", rc, ulSemCount);
         raise runtime error
       */
   }

   DosPostEventSem( ( HEV ) apsr[ 0 ].hsemCur );
   DosResetEventSem( ( HEV ) apsr[ 0 ].hsemCur, &ulPostCount );

}

/*
   This is an hack to prevent that a thread, blocked on a mutex wait, makes
   a program unkillable. This is a limit of OS/2, not all system calls can
   be interrupted. So, I wait for a certain amount of time on a mutex request
   then, If I didn't get ownership, I test to see if program is dieing, if so I release
   mutex (if I hold it) and then stop acquiring it.

 */
void hb_threadMtxPoll( HB_CRITICAL_T mtx )
{
   APIRET   rc;
   ULONG    ulPostCount;

   do
   {
      rc = DosRequestMutexSem( mtx, 2000 );
      DosQueryEventSem( hb_hevWakeUpAll, &ulPostCount );

      if( ( ulPostCount != 0 ) && ( rc == NO_ERROR ) )
         DosReleaseMutexSem( mtx );
   }
   while( ulPostCount == 0 && rc != NO_ERROR );
}

#endif /* OS/2 */

/**************************************************************/
/* Part 4: XHARBOUR threading API                             */
/**************************************************************/

/*
   Garbage finalization function for XHARBOUR thread objects.
   When the gc detects a thread object is not anymore referenced,
   the object is cleared; the thread is left alive though.
 */
HB_GARBAGE_FUNC( hb_threadThreadIdFinalize )
{
   PHB_THREAD_ID ThreadID = ( PHB_THREAD_ID ) Cargo;

   if( ThreadID->sign != HB_THREAD_ID_SIGN )
   {
      hb_errInternal( HB_EI_MEMCORRUPT,
                      "hb_threadThreadIdFinalize: Corrupted thread object at 0x%p",
                      ( char * ) ThreadID, NULL );
      return;
   }

   HB_SHARED_LOCK( hb_runningStacks );
   if( ThreadID->pThreadReady->bActive )
   {
      if( ThreadID->pStack )
      {
         if( ThreadID == ThreadID->pStack->pThreadID )
            ThreadID->pStack->pThreadID = ThreadID->next;
         else
         {
            PHB_THREAD_ID pThread = ThreadID->pStack->pThreadID;

            while( pThread && pThread->next != ThreadID )
            {
               pThread = pThread->next;
            }

            if( pThread )
               pThread->next = ThreadID->next;
         }
      }
   }
   HB_SHARED_UNLOCK( hb_runningStacks );

   if( HB_ATOMIC_DEC( ThreadID->pThreadReady->ulCounter ) == 0 )
      hb_xfree( ThreadID->pThreadReady );

   /* hb_gcFree( ThreadID ); */
}

/*
   Get current thread ID (based on SYSTEM ID)
   (deprecated)
 */
HB_FUNC( THREADGETCURRENT )
{
   HB_THREAD_STUB_API
   hb_retnl( ( LONG ) HB_CURRENT_THREAD() );
}

/*
   Get current thread VM order
   - Deprecated -
 */
HB_FUNC( THREADGETCURRENTINTERNAL )
{
   HB_THREAD_STUB
   hb_retnl( ( LONG ) HB_VM_STACK.th_vm_id );
}

/*
   Return an instance of current thread object
 */
HB_FUNC( GETCURRENTTHREAD )
{
   HB_THREAD_STUB
   PHB_THREAD_ID  pThreadID;
   PHB_THREAD_ID  pThread = ( PHB_THREAD_ID )
                            hb_gcAlloc( sizeof( HB_THREAD_ID ), hb_threadThreadIdFinalize );

   pThread->sign           = HB_THREAD_ID_SIGN;
   pThread->threadId       = HB_CURRENT_THREAD();
   pThread->pStack         = &HB_VM_STACK;
   pThread->bReady         = TRUE;
   pThread->next           = NULL;
   pThread->pThreadReady   = pThread->pStack->pThreadReady;
   HB_ATOMIC_INC( pThread->pThreadReady->ulCounter );

   hb_retptrGC( pThread );

   HB_SHARED_LOCK( hb_runningStacks );

   if( ( &HB_VM_STACK )->pThreadID )
   {
      pThreadID = ( &HB_VM_STACK )->pThreadID;
      while( pThreadID->next )
      {
         pThreadID = pThreadID->next;
      }
      pThreadID->next = pThread;
   }
   else
      ( &HB_VM_STACK )->pThreadID = pThread;

   HB_SHARED_UNLOCK( hb_runningStacks );

}

/*
   Returns VM thread id
 */
HB_FUNC( GETTHREADID )
{
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = ( PHB_THREAD_ID ) hb_parptr( 1 );

   if( pThread != NULL )
   {
      if( pThread->sign != HB_THREAD_ID_SIGN )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GETTHREADID", 1,
                               hb_paramError( 1 ) );
         return;
      }
      else if( ! pThread->pThreadReady->bActive || ! pThread->bReady )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
                               "GETTHREADID", 1, hb_paramError( 1 ) );
         return;
      }

      if( pThread->pStack )
         hb_retnl( ( LONG ) pThread->pStack->th_vm_id );
      else
         hb_retnl( 0 );
   }
   else
      hb_retnl( HB_VM_STACK.th_vm_id );
}

/*
   Returns a numeric representation of SYSTEM thread id, where available.
   BE CAREFUL - this is mainly a debugging function!
   Don't use it for important code (or be sure to bind to a given platform).
 */
HB_FUNC( GETSYSTEMTHREADID )
{
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = ( PHB_THREAD_ID ) hb_parptr( 1 );

   if( pThread != NULL )
   {
      if( pThread->sign != HB_THREAD_ID_SIGN )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GETSYSTEMTHREADID", 1,
                               hb_paramError( 1 ) );
         return;
      }
      else if( ! pThread->pThreadReady->bActive || ! pThread->bReady )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
                               "GETSYSTEMTHREADID", 1, hb_paramError( 1 ) );
         return;
      }
#if 1
      hb_retnl( ( LONG ) pThread->threadId );
#endif
      /* Place here a warning or a special value for system without
         numeric or enumerable thread ids
       */
   }
   else
      hb_retnint( ( HB_PTRDIFF ) HB_VM_STACK.th_id );
}

/*
   Returns true if two thread objects are the same.
 */
HB_FUNC( ISSAMETHREAD )
{
   HB_THREAD_STUB_API
   PHB_THREAD_ID  pThread1 = ( PHB_THREAD_ID ) hb_parptr( 1 );
   PHB_THREAD_ID  pThread2 = ( PHB_THREAD_ID ) hb_parptr( 2 );

   if( pThread1 == NULL || pThread1->sign != HB_THREAD_ID_SIGN ||
       ( pThread2 != NULL && pThread2->sign != HB_THREAD_ID_SIGN ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "ISSAMETHREAD", 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   if( ! pThread1->pThreadReady->bActive || ! pThread1->bReady )
      hb_retl( FALSE );
   else
   {
      if( pThread2 == NULL )
         hb_retl( HB_SAME_THREAD( pThread1->threadId, HB_CURRENT_THREAD() ) );
      else if( ! pThread2->pThreadReady->bActive || ! pThread2->bReady )
         hb_retl( FALSE );
      else
         hb_retl( HB_SAME_THREAD( pThread1->threadId, pThread2->threadId ) );
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
   HB_THREAD_STUB_API
   BOOL bOld;

   HB_SHARED_LOCK( hb_runningStacks );

   bOld = hb_bIdleFence;

   if( hb_pcount() == 1 )
      hb_bIdleFence = hb_parl( 1 );

   hb_retl( bOld );

   HB_SHARED_UNLOCK( hb_runningStacks );
}

/*
   Function used by the error recovery routine to get the
   error handlers from the current stack.
 */
HB_FUNC( HB_THREADGETTRYERRORARRAY )
{
   HB_THREAD_STUB
   hb_itemReturn( HB_VM_STACK.aTryCatchHandlerStack );
}

/*
   Mainly a debug function: use cautiously, the count is NOT
   threadsafe.
 */
HB_FUNC( HB_THREADCOUNTSTACKS )
{
   HB_THREAD_STUB_API
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
HB_MUTEX_STRUCT * hb_threadLinkMutex( HB_MUTEX_STRUCT * mx )
{
   HB_MUTEX_STRUCT * p;

   HB_CRITICAL_LOCK( hb_mutexMutex );

   if( hb_ht_mutex == NULL )
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
HB_MUTEX_STRUCT * hb_threadUnlinkMutex( HB_MUTEX_STRUCT * pMtx )
{
   HB_MUTEX_STRUCT * p, * prev;

   HB_CRITICAL_LOCK( hb_mutexMutex );

   if( hb_ht_mutex == NULL )
   {
      HB_CRITICAL_UNLOCK( hb_mutexMutex );
      return NULL;
   }

   p     = hb_ht_mutex;
   prev  = NULL;

   while( p && p != pMtx )
   {
      prev  = p;
      p     = p->next;
   }

   if( p )
   {
      /*unlink the stack*/
      if( prev )
         prev->next = p->next;
      else
         hb_ht_mutex = p->next;

      if( p->lock_count )
         hb_mutexForceUnlock( p );
   }

   HB_CRITICAL_UNLOCK( hb_mutexMutex );

   return p;
}

/*
   Force the unlocking of XHARBOUR mutex objects.
 */
void hb_mutexForceUnlock( void * mtx )
{
   HB_MUTEX_STRUCT * Mutex = ( HB_MUTEX_STRUCT * ) mtx;

   if( Mutex->locker != 0 )
   {
      Mutex->lock_count = 0;
      Mutex->locker     = 0;
      /* warn other therads that this mutex has become available
       */
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
   HB_CRITICAL_T * Mutex = ( HB_CRITICAL_T * ) mtx;

   HB_CRITICAL_UNLOCK( *Mutex );
}

/*
   Garbage finalization function for XHARBOUR mutex objects.
   When the gc detects a mutex to be destroyed, it calls this
   function to clean it.
 */
HB_GARBAGE_FUNC( hb_threadMutexFinalize )
{
   HB_MUTEX_STRUCT * Mutex = ( HB_MUTEX_STRUCT * ) Cargo;

   if( Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errInternal( HB_EI_MEMCORRUPT,
                      "hb_threadMutexFinalize: Corrupted mutex item at 0x%p",
                      ( char * ) Mutex, NULL );
      return;
   }

   hb_threadUnlinkMutex( Mutex );

   HB_CRITICAL_DESTROY( Mutex->mutex );
   HB_COND_DESTROY( Mutex->cond );
   hb_itemRelease( Mutex->aEventObjects );
   /* hb_gcFree( Mutex ); */
}

/******************************************************************/
/* Part 6: Xharbour MUTEX object API                              */
/******************************************************************/

/*
   Create a new mutex (marking it disposeable by the GC)
 */
PHB_ITEM hb_threadMutexCreate( PHB_ITEM pItem )
{
   HB_MUTEX_STRUCT * mt;

   mt = ( HB_MUTEX_STRUCT * ) hb_gcAlloc( sizeof( HB_MUTEX_STRUCT ), hb_threadMutexFinalize );

   HB_CRITICAL_INIT( mt->mutex );
   HB_COND_INIT( mt->cond );

   mt->sign          = HB_MUTEX_SIGNATURE;
   mt->lock_count    = 0;
   mt->waiting       = 0;
   mt->locker        = 0;
   mt->aEventObjects = hb_itemArrayNew( 0 );
   mt->next          = 0;

   hb_threadLinkMutex( mt );

   return hb_itemPutPtrGC( pItem, ( void * ) mt );
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
BOOL hb_threadMutexLock( PHB_ITEM pItem, BOOL bError )
{
   HB_THREAD_STUB
   HB_MUTEX_STRUCT * Mutex = ( HB_MUTEX_STRUCT * ) hb_itemGetPtr( pItem );

   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      if( bError )
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXLOCK", 1, hb_paramError( 1 ) );

      return FALSE;
   }

   if( HB_SAME_THREAD( Mutex->locker, HB_CURRENT_THREAD() ) )
      Mutex->lock_count++;
   else
   {
      HB_STACK_UNLOCK;

      HB_CRITICAL_LOCK( Mutex->mutex );
      HB_CLEANUP_PUSH( hb_rawMutexForceUnlock, Mutex->mutex );

      while( Mutex->locker != 0 )
      {
         HB_COND_WAIT( Mutex->cond, Mutex->mutex );
      }
      Mutex->locker     = HB_CURRENT_THREAD();
      Mutex->lock_count = 1;

      HB_CLEANUP_POP;
      HB_CRITICAL_UNLOCK( Mutex->mutex );

      HB_STACK_LOCK;
   }
   return TRUE;
}

/*
   Locks a mutex; locking is done by waiting for the mutex resource
   to become available with timeout. This wait is cancelable.
 */
BOOL hb_threadMutexTimeOutLock( PHB_ITEM pItem, int iTimeOut, BOOL bError )
{
   HB_THREAD_STUB
   HB_MUTEX_STRUCT * Mutex       = ( HB_MUTEX_STRUCT * ) hb_itemGetPtr( pItem );

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   DWORD             dwTimeOut   = ( DWORD ) iTimeOut;
#else
   int               dwTimeOut   = iTimeOut;
#endif

   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE || iTimeOut < 0 )
   {
      if( bError )
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXTIMEOUTLOCK", 2, hb_paramError( 1 ), hb_paramError( 2 ) );

      return FALSE;
   }

   if( HB_SAME_THREAD( Mutex->locker, HB_CURRENT_THREAD() ) )
      Mutex->lock_count++;
   else
   {
      BOOL bLock;
      HB_STACK_UNLOCK;

      HB_CRITICAL_LOCK( Mutex->mutex );
      HB_CLEANUP_PUSH( hb_rawMutexForceUnlock, Mutex->mutex );

      HB_COND_WAITTIME( Mutex->cond, Mutex->mutex, dwTimeOut );

      if( Mutex->locker != 0 )
         bLock = FALSE;
      else
      {
         Mutex->locker     = HB_CURRENT_THREAD();
         Mutex->lock_count = 1;
         bLock             = TRUE;
      }

      HB_CLEANUP_POP;
      HB_CRITICAL_UNLOCK( Mutex->mutex );

      HB_STACK_LOCK;
      return bLock;
   }
   return TRUE;
}

/*
   Try to lock a mutex; return immediately on failure.
 */
BOOL hb_threadMutexTryLock( PHB_ITEM pItem, BOOL bError )
{
   HB_THREAD_STUB
   HB_MUTEX_STRUCT * Mutex = ( HB_MUTEX_STRUCT * ) hb_itemGetPtr( pItem );
   BOOL              bLock;

   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      if( bError )
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXTRYLOCK", 1, hb_paramError( 1 ) );

      return FALSE;
   }

   if( HB_SAME_THREAD( Mutex->locker, HB_CURRENT_THREAD() ) )
   {
      Mutex->lock_count++;
      bLock = TRUE;
   }
   else
   {
      HB_STACK_UNLOCK;

      HB_CRITICAL_LOCK( Mutex->mutex );
      HB_CLEANUP_PUSH( hb_rawMutexForceUnlock, Mutex->mutex );

      if( Mutex->locker != 0 )
         bLock = FALSE;
      else
      {
         Mutex->locker     = HB_CURRENT_THREAD();
         Mutex->lock_count = 1;
         bLock             = TRUE;
      }

      HB_CLEANUP_POP;
      HB_CRITICAL_UNLOCK( Mutex->mutex );

      HB_STACK_LOCK;
   }
   return bLock;
}

HB_EXTERN_END

/*
   Unlocks a mutex; this succeeds only if the calling thread is
   the owner of the mutex, else the call is ignored.
 */
void hb_threadMutexUnlock( PHB_ITEM pItem, BOOL bError )
{
   HB_MUTEX_STRUCT * Mutex = ( HB_MUTEX_STRUCT * ) hb_itemGetPtr( pItem );

   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      if( bError )
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "MUTEXUNLOCK", 1, hb_paramError( 1 ) );

      return;
   }

   HB_CRITICAL_LOCK( Mutex->mutex );
   if( HB_SAME_THREAD( Mutex->locker, HB_CURRENT_THREAD() ) )
   {
      Mutex->lock_count--;

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
   HB_THREAD_STUB
   HB_MUTEX_STRUCT * Mutex    = ( HB_MUTEX_STRUCT * ) hb_parptr( 1 );
   PHB_ITEM          pStatus  = hb_param( 3, HB_IT_BYREF );
   int               iWaitRes;
   ULONG             ulWaitTime;

   /* Parameter error checking */
   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE || hb_pcount() > 3 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "SUBSCRIBE", 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      return;
   }

   /* Unlock SYNC methods locked */
   hb_clsUnmutexSync();

   HB_STACK_UNLOCK;
   HB_CRITICAL_LOCK( Mutex->mutex );

   /* If we are subscribing now, we must flatten pre-notified data */
   if( mode == 1 && hb_arrayLen( Mutex->aEventObjects ) > 0 )
      hb_arraySize(  Mutex->aEventObjects, 0 );

   /* warning; does not checks if the current thread is holding the mutex */
   Mutex->waiting++;

   if( hb_pcount() == 1 )
   {
      while( hb_arrayLen( Mutex->aEventObjects ) == 0 )
      {
         HB_COND_WAIT( Mutex->cond, Mutex->mutex );
      }
      iWaitRes = 0;  /* means success of wait */
   }
   else
   {
      ulWaitTime = hb_parnl( 2 );
      if( ulWaitTime > 0 )
         iWaitRes = HB_COND_WAITTIME( Mutex->cond, Mutex->mutex, ulWaitTime );
      else
         /* Remember that 0 means success */
         iWaitRes = hb_arrayLen( Mutex->aEventObjects ) == 0;
   }

   /* No more waiting */
   Mutex->waiting--;

   if( iWaitRes == 0 )
   {
      if( pStatus )
         hb_itemPutL( pStatus, TRUE );
         /* pStatus->type = HB_IT_LOGICAL;
          * pStatus->item.asLogical.value = 1;
          */

      hb_itemReturnForward( hb_arrayGetItemPtr( Mutex->aEventObjects, 1 ) );
      hb_arrayDel( Mutex->aEventObjects, 1 );
      hb_arraySize( Mutex->aEventObjects, hb_arrayLen( Mutex->aEventObjects ) - 1 );
   }
   else
   {
      if( pStatus )
         hb_itemPutL( pStatus, FALSE );
         /* pStatus->type = HB_IT_LOGICAL;
          * pStatus->item.asLogical.value = 0;
          */

      hb_ret();
   }

   HB_CRITICAL_UNLOCK( Mutex->mutex );
   HB_STACK_LOCK;

   /* Lock SYNC methods */
   hb_clsRemutexSync();
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
   s_subscribeInternal( 1 );
}

/*
   Signal that something meaningful has happened, and wake ONE subscriber.
 */
HB_FUNC( NOTIFY )
{
   HB_MUTEX_STRUCT * Mutex = ( HB_MUTEX_STRUCT * ) hb_parptr( 1 );
   PHB_ITEM          pVal  = hb_param( 2, HB_IT_ANY );

   /* Parameter error checking */
   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "NOTIFY", 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   HB_CRITICAL_LOCK( Mutex->mutex );

   if( pVal == NULL )
      /* add a NIL at bottom */
      hb_arraySize( Mutex->aEventObjects, hb_arrayLen( Mutex->aEventObjects ) + 1 );
   else
      hb_arrayAdd( Mutex->aEventObjects, pVal );

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
   HB_MUTEX_STRUCT * Mutex    = ( HB_MUTEX_STRUCT * ) hb_parptr( 1 );
   PHB_ITEM          pVal     = hb_param( 2, HB_IT_ANY );
   BOOL              bClear   = FALSE;
   int               iWt;

   /* Parameter error checking */
   if( Mutex == NULL || Mutex->sign != HB_MUTEX_SIGNATURE )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "NOTIFYALL", 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   if( pVal == NULL )
   {
      bClear   = TRUE;
      pVal     = hb_itemNew( NULL );
   }

   HB_CRITICAL_LOCK( Mutex->mutex );

   for( iWt = 0; iWt < Mutex->waiting; iWt++ )
   {
      hb_arrayAdd( Mutex->aEventObjects, pVal );
   }

   HB_COND_SIGNAL( Mutex->cond );
   HB_CRITICAL_UNLOCK( Mutex->mutex );

   if( bClear )
      hb_itemRelease( pVal );
}

#endif /* thread support */

/******************************************************************/
/* Part 7: Xharbour thread functions available in ST mode         */
/******************************************************************/

HB_EXTERN_BEGIN

void hb_threadSleep( int millisec, BOOL bIdleWaitNoCpu )
{
   HB_THREAD_STUB

   HB_STACK_UNLOCK;

#if ! defined( HB_OS_WIN )
   HB_SYMBOL_UNUSED( bIdleWaitNoCpu );
#endif

#if defined( HB_OS_DARWIN ) || defined( __DJGPP__ )
   usleep( millisec * 1000 );

#elif defined( HB_OS_OS2 )
   HB_TEST_CANCEL_ENABLE_ASYN;
   DosSleep( millisec );
   HB_DISABLE_ASYN_CANC;

#elif defined( HB_OS_UNIX ) || defined( HB_OS_UNIX_COMPATIBLE )
   {
      struct timespec ts, trem;
      ts.tv_sec   = millisec / 1000;
      ts.tv_nsec  = ( millisec % 1000 ) * 1000000;
      while( nanosleep( &ts, &trem ) != 0 && errno == EINTR )
      {
         ts = trem;
      }
   }

#elif defined( HB_OS_WIN )
   HB_TEST_CANCEL_ENABLE_ASYN;
   if( bIdleWaitNoCpu )
      WaitMessage();
   else
      Sleep( millisec );
   HB_DISABLE_ASYN_CANC;
#else
   /* Note: delay() in <dos.h> for DJGPP does not work and
            delay() in <dos.h> for BORLANDC is not multi-
            tasking friendly. */
   delay( millisec );
#endif

   HB_STACK_LOCK;
}

HB_EXTERN_END

HB_FUNC( THREADSLEEP )
{
   if( ! ISNUM( 1 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "THREADSLEEP", 1,
                            hb_param( 1, HB_IT_ANY ) );
      return;
   }

   hb_threadSleep( hb_parni( 1 ), hb_parl( 2 ) );
}

HB_FUNC( SECONDSSLEEP )
{
   int sleep;

   if ( ! ISNUM( 1 ) )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "SECONDSSLEEP", 1,
                            hb_param( 1, HB_IT_ANY ) );

   sleep = ( int ) ( hb_parnd( 1 ) * 1000.0 );
   hb_threadSleep( sleep, FALSE );
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
   hb_retl( TRUE ); /* always inspecting */
#endif
}

HB_FUNC( HB_MUTEXCREATE )
{
#if defined( HB_THREAD_SUPPORT )
   hb_itemRelease( hb_itemReturnForward( hb_threadMutexCreate( NULL ) ) );
#endif
}

HB_FUNC( HB_MUTEXLOCK )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB_API
   hb_retl( hb_threadMutexLock( hb_param( 1, HB_IT_POINTER ), TRUE ) );
#endif
}

HB_FUNC( HB_MUTEXTIMEOUTLOCK )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB_API
   hb_retl( hb_threadMutexTimeOutLock( hb_param( 1, HB_IT_POINTER ), hb_parni( 2 ), TRUE ) );
#endif
}

HB_FUNC( HB_MUTEXTRYLOCK )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB_API
   hb_retl( hb_threadMutexTryLock( hb_param( 1, HB_IT_POINTER ), TRUE ) );
#endif
}

HB_FUNC( HB_MUTEXUNLOCK )
{
#if defined( HB_THREAD_SUPPORT )
   hb_threadMutexUnlock( hb_param( 1, HB_IT_POINTER ), TRUE );
#endif
}

/*
   Starts a new thread;
 */
HB_FUNC( STARTTHREAD )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB

   PHB_ITEM          pPointer;
   PHB_ITEM          pArgs;
   HB_THREAD_T       th_id;
   PHB_DYNS          pExecSym;
   PHB_SYMB          pSymbol     = NULL;
   BOOL              bIsMethod   = FALSE;
   HB_STACK *        pStack;
   PHB_THREAD_ID     pThread;
   PHB_THREAD_READY  pThreadReady;

#ifdef HB_OS_WIN
   HANDLE            th_h;
#endif

   pArgs    = hb_arrayFromParams( HB_VM_STACK.pBase );
   pPointer = hb_arrayGetItemPtr( pArgs, 1 );

   /* Error Checking */
   if( pPointer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STARTTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   /* Is it a function pointer? */
   if( pPointer->type == HB_IT_POINTER )
   {
      pSymbol = ( PHB_SYMB ) hb_itemGetPtr( pPointer );

      if( pSymbol == NULL )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "STARTTHREAD", 1, hb_paramError( 1 ) );
         hb_itemRelease( pArgs );
         return;
      }

      /* Converting it to its Symbol.
       */
      hb_itemPutSymbol( pPointer, pSymbol );
      pPointer->item.asSymbol.pCargo->stackbase = ( long ) ( HB_VM_STACK.pBase - HB_VM_STACK.pItems );
   }
   /* Is it an object? */
   else if( hb_pcount() >= 2 && pPointer->type == HB_IT_OBJECT )
   {
      PHB_ITEM pString = hb_arrayGetItemPtr( pArgs, 2 );

      if( pString->type == HB_IT_STRING )
      {
         pExecSym = hb_dynsymFindName( pString->item.asString.value );

         if( pExecSym )
            pSymbol = pExecSym->pSymbol;
      }
      else if( pString->type == HB_IT_POINTER )
         pSymbol = ( PHB_SYMB ) hb_itemGetPtr( pString );

      if( pSymbol == NULL )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, "StartThread", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         hb_itemRelease( pArgs );
         return;
      }

      bIsMethod = TRUE;

      /* Now we must move the object in the second place */
      hb_itemSwap( pPointer, hb_arrayGetItemPtr( pArgs, 2 ) );

      hb_itemPutSymbol( pPointer, pSymbol );
      pPointer->item.asSymbol.pCargo->stackbase = ( long ) ( HB_VM_STACK.pBase - HB_VM_STACK.pItems );
   }
   /* Is it a function name? */
   else if( pPointer->type == HB_IT_STRING )
   {
      pExecSym = hb_dynsymFindName( pPointer->item.asString.value );

      if( pExecSym == NULL )
      {
         hb_errRT_BASE( EG_NOFUNC, 1001, NULL, pPointer->item.asString.value, HB_ERR_ARGS_BASEPARAMS );
         hb_itemRelease( pArgs );
         return;
      }

      hb_itemPutSymbol( pPointer, pExecSym->pSymbol );
      pPointer->item.asSymbol.pCargo->stackbase = ( long ) ( HB_VM_STACK.pBase - HB_VM_STACK.pItems );
   }
   /* Is it a code block? */
   else if( pPointer->type != HB_IT_BLOCK )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STARTTHREAD", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   pThreadReady            = ( PHB_THREAD_READY ) hb_xgrab( sizeof( HB_THREAD_READY ) );
   pThreadReady->bActive   = FALSE;
   pThreadReady->ulCounter = 2;  /* pStack and pThread */

   /* Create the thread ID object; for now it is a flat pointer
    */
   pThread                 = ( PHB_THREAD_ID ) hb_gcAlloc( sizeof( HB_THREAD_ID ), hb_threadThreadIdFinalize );
   pThread->sign           = HB_THREAD_ID_SIGN;
   pThread->pThreadReady   = pThreadReady;

   /* Create the stack here to avoid cross locking of alloc mutex
    */
   pStack                  = hb_threadCreateStack( 0 );

   pStack->uiParams        = hb_pcount();
   pStack->bIsMethod       = bIsMethod;
   pStack->pThreadReady    = pThreadReady;

   {
      PHB_SET_STRUCT pSet = hb_setClone( hb_stackSetStruct() );

      HB_MEMCPY( &pStack->set, pSet, sizeof( HB_SET_STRUCT ) );
      hb_xfree( pSet );
   }

   /* Forbid usage of stack before that new thread's VM takes care of it */
   hb_threadFillStack( pStack, pArgs );

   /* we can be inspected now, but we are sure that our child thread
      stack cannot */
   HB_SHARED_LOCK( hb_runningStacks );

   hb_runningStacks.content.asLong++;
   pStack->bInUse    = TRUE;
   pStack->bActive   = TRUE;
   pStack->th_vm_id  = hb_threadUniqueId();
   hb_threadLinkStack( pStack );

#if defined( HB_OS_WIN )
/* #ifndef __BORLANDC__
      if( ( th_h = CreateThread( NULL, 0, hb_create_a_thread, (void *) pStack , CREATE_SUSPENDED, &th_id ) ) != NULL )
   #else */
   if( ( th_h = ( HANDLE ) _beginthreadex( NULL, 0, hb_create_a_thread, ( void * ) pStack, CREATE_SUSPENDED, &th_id ) ) != 0L )
/* #endif */
#elif defined( HB_OS_OS2 )
   if( ( th_id = _beginthread( ( void * ) hb_create_a_thread, NULL, 128 * 1024, ( void * ) pStack ) ) >= 0 )
#else
   if( pthread_create( &th_id, NULL, hb_create_a_thread, ( void * ) pStack ) == 0 )
#endif
   {
      /* under linux, this will be set by the first thread, father or
         child, that is able to reach this line */
      pStack->th_id = th_id;

      /* Under windows, we put the handle after creation */
#if defined( HB_OS_WIN )
      pStack->th_h            = th_h;
      ResumeThread( th_h );
#endif
      pThread->threadId       = th_id;
      pThread->bReady         = TRUE;
      pThread->pStack         = pStack;
      pThread->next           = NULL;
      pStack->pThreadID       = pThread;
      pThreadReady->bActive   = TRUE;
      hb_retptrGC( pThread );
   }
   else
   {
      hb_threadUnlinkStack( pStack );
      hb_threadDestroyStack( pStack );
      pThread->bReady = FALSE;
      hb_retptrGC( pThread );
   }
   /* notice that the child thread won't be able to proceed until we
    * release this mutex.
    */
   HB_SHARED_UNLOCK( hb_runningStacks );
#endif
}

/*
   Waits until a given thread terminates.
 */
HB_FUNC( JOINTHREAD )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = ( PHB_THREAD_ID ) hb_parptr( 1 );

   if( pThread == NULL || pThread->sign != HB_THREAD_ID_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "JOINTHREAD", 1,
                            hb_paramError( 1 ) );
      return;
   }

   if( pThread == NULL || ! pThread->pThreadReady->bActive || ! pThread->bReady )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
                            "JOINTHREAD", 1, hb_paramError( 1 ) );
      return;
   }

   if( pThread->pStack )
   {
      HB_STACK_UNLOCK;

#if ! defined( HB_OS_WIN ) && ! defined( HB_OS_OS2 )
      if( pthread_join( pThread->threadId, NULL ) != 0 )
      {
         HB_STACK_LOCK;
         hb_retl( FALSE );
         return;
      }
#else
#ifdef HB_OS_WIN
      WaitForSingleObject( pThread->pStack->th_h, INFINITE );
#else
      DosWaitThread( &pThread->pStack->th_id, DCWW_WAIT );
#endif
#endif

      HB_STACK_LOCK;
   }

   hb_retl( TRUE );
#endif
}

/*
   Try to gently stop a thread, and if this is not possible,
   use the maximum severity allowed. It does not wait for
   target thread to be terminated.
 */
HB_FUNC( KILLTHREAD )
{
#if defined( HB_THREAD_SUPPORT )

#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   HB_THREAD_STUB
#endif

   PHB_THREAD_ID pThread = ( PHB_THREAD_ID ) hb_parptr( 1 );

   if( pThread == NULL || pThread->sign != HB_THREAD_ID_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "KILLTHREAD", 1,
                            hb_paramError( 1 ) );
      return;
   }

   if( ! pThread->pThreadReady->bActive || ! pThread->bReady )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
                            "KILLTHREAD", 1, hb_paramError( 1 ) );
      return;
   }

   if( pThread->pStack )
   {
#if defined( HB_OS_UNIX ) || defined( HB_OS_UNIX_COMPATIBLE )
      pthread_cancel( pThread->threadId );
#else
      /* Shell locking the thread */
      HB_STACK_UNLOCK;

      HB_CRITICAL_LOCK( hb_cancelMutex );
      if( ! pThread->pStack->bCanCancel )
      {
         pThread->pStack->bCanceled = TRUE;
         HB_CRITICAL_UNLOCK( hb_cancelMutex );
      }
      else
         hb_threadCancel( ( HB_STACK * ) pThread->pStack );  /* also unlocks the mutex */

      HB_STACK_LOCK;
#endif
   }
#endif
}

/*
   Try to gently stop a thread, and waits for its termination.
 */
HB_FUNC( STOPTHREAD )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB
   PHB_THREAD_ID pThread = ( PHB_THREAD_ID ) hb_parptr( 1 );

   if( pThread == NULL || pThread->sign != HB_THREAD_ID_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STOPTHREAD", 1,
                            hb_paramError( 1 ) );
      return;
   }

   if( ! pThread->bReady )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Given thread is not valid",
                            "STOPTHREAD", 1, hb_paramError( 1 ) );
      return;
   }

   if( pThread->pThreadReady->bActive && pThread->pStack )
   {
      HB_STACK_UNLOCK;

#if defined( HB_OS_UNIX ) || defined( HB_OS_UNIX_COMPATIBLE )
      pthread_cancel( pThread->threadId );
      pthread_join( pThread->threadId, NULL );
#else
      HB_CRITICAL_LOCK( hb_cancelMutex );
      pThread->pStack->bCanceled = TRUE;
      HB_CRITICAL_UNLOCK( hb_cancelMutex );

      HB_TEST_CANCEL_ENABLE_ASYN;
#ifdef HB_OS_WIN
      WaitForSingleObject( pThread->pStack->th_h, INFINITE );
#else
      DosWaitThread( &pThread->pStack->th_id, DCWW_WAIT );
#endif
      HB_DISABLE_ASYN_CANC;
#endif

      HB_STACK_LOCK;
   }
#endif
}

/*
   Returns true if the thread object refers to a valid system thread.
 */
HB_FUNC( ISVALIDTHREAD )
{
#if defined( HB_THREAD_SUPPORT )
   HB_THREAD_STUB_API
   PHB_THREAD_ID pThread = ( PHB_THREAD_ID ) hb_parptr( 1 );

   if( pThread == NULL || pThread->sign != HB_THREAD_ID_SIGN || pThread->pStack == NULL || ! pThread->pThreadReady->bActive )
      hb_retl( FALSE );
   else
      hb_retl( pThread->bReady );
#else
   hb_retl( FALSE );
#endif
}

/*
   The following functions were added in order to remove hard-wired
   HB_THREAD_SUPPORT in many runtime functions, thus making them common
   to both ST and MT
 */

HB_EXTERN_BEGIN

#include "hbservmt.c"
#include "rddmt.c"
#include "consmt.c"

#ifdef HB_THREAD_SUPPORT
static HB_CRITICAL_T s_arc4Mtx;
static HB_CRITICAL_T s_sockMtx;
static HB_CRITICAL_T s_hsxMtx;
static HB_CRITICAL_T s_fileMtx;
static HB_CRITICAL_T s_fileNetMtx;
static HB_CRITICAL_T s_traceMtx;
static HB_CRITICAL_T s_ServiceMutex;
#endif

/* generalisation */
void hb_threadLockInit( int iMtx )
{
#if defined( HB_THREAD_SUPPORT )
   switch( iMtx )
   {
      case S_ARC4MTX:
         HB_CRITICAL_INIT( s_arc4Mtx );
         break;
      case S_SOCKMTX:
         HB_CRITICAL_INIT( s_sockMtx );
         break;
      case S_HSXMTX:
         HB_CRITICAL_INIT( s_hsxMtx );
         break;
      case S_FILEMTX:
         HB_CRITICAL_INIT( s_fileMtx );
         break;
      case S_FILENETMTX:
         HB_CRITICAL_INIT( s_fileNetMtx );
         break;
      case S_TRACEMTX:
         HB_CRITICAL_INIT( s_traceMtx );
         break;
      case HB_MACROMUTEX:
         HB_CRITICAL_INIT( hb_macroMutex );
         break;
      case HB_OUTPUTMUTEX:
         HB_CRITICAL_INIT( hb_outputMutex );
         break;
      case S_SERVICEMUTEX:
         HB_CRITICAL_INIT( s_ServiceMutex );
         break;
   }
#else
   HB_SYMBOL_UNUSED( iMtx );
#endif
}

void hb_threadLock( int iMtx )
{
#if defined( HB_THREAD_SUPPORT )
   switch( iMtx )
   {
      case S_ARC4MTX:
         HB_CRITICAL_LOCK( s_arc4Mtx );
         break;
      case S_SOCKMTX:
         HB_CRITICAL_LOCK( s_sockMtx );
         break;
      case S_HSXMTX:
         HB_CRITICAL_LOCK( s_hsxMtx );
         break;
      case S_FILEMTX:
         HB_CRITICAL_LOCK( s_fileMtx );
         break;
      case S_FILENETMTX:
         HB_CRITICAL_LOCK( s_fileNetMtx );
         break;
      case S_TRACEMTX:
         HB_CRITICAL_LOCK( s_traceMtx );
         break;
      case HB_MACROMUTEX:
         HB_CRITICAL_LOCK( hb_macroMutex );
         break;
      case HB_OUTPUTMUTEX:
         HB_CRITICAL_LOCK( hb_outputMutex );
         break;
      case S_SERVICEMUTEX:
         HB_CRITICAL_LOCK( s_ServiceMutex );
         break;
   }
#else
   HB_SYMBOL_UNUSED( iMtx );
#endif
}

void hb_threadUnLock( int iMtx )
{
#if defined( HB_THREAD_SUPPORT )
   switch( iMtx )
   {
      case S_ARC4MTX:
         HB_CRITICAL_UNLOCK( s_arc4Mtx );
         break;
      case S_SOCKMTX:
         HB_CRITICAL_UNLOCK( s_sockMtx );
         break;
      case S_HSXMTX:
         HB_CRITICAL_UNLOCK( s_hsxMtx );
         break;
      case S_FILEMTX:
         HB_CRITICAL_UNLOCK( s_fileMtx );
         break;
      case S_FILENETMTX:
         HB_CRITICAL_UNLOCK( s_fileNetMtx );
         break;
      case S_TRACEMTX:
         HB_CRITICAL_UNLOCK( s_traceMtx );
         break;
      case HB_MACROMUTEX:
         HB_CRITICAL_UNLOCK( hb_macroMutex );
         break;
      case HB_OUTPUTMUTEX:
         HB_CRITICAL_UNLOCK( hb_outputMutex );
         break;
      case S_SERVICEMUTEX:
         HB_CRITICAL_UNLOCK( s_ServiceMutex );
         break;
   }
#else
   HB_SYMBOL_UNUSED( iMtx );
#endif
}

void hb_threadLockDestroy( int iMtx )
{
#if defined( HB_THREAD_SUPPORT )
   switch( iMtx )
   {
      case S_ARC4MTX:
         HB_CRITICAL_DESTROY( s_arc4Mtx );
         break;
      case S_SOCKMTX:
         HB_CRITICAL_DESTROY( s_sockMtx );
         break;
      case S_HSXMTX:
         HB_CRITICAL_DESTROY( s_hsxMtx );
         break;
      case S_FILEMTX:
         HB_CRITICAL_DESTROY( s_fileMtx );
         break;
      case S_FILENETMTX:
         HB_CRITICAL_DESTROY( s_fileNetMtx );
         break;
      case S_TRACEMTX:
         HB_CRITICAL_DESTROY( s_traceMtx );
         break;
      case HB_MACROMUTEX:
         HB_CRITICAL_DESTROY( hb_macroMutex );
         break;
      case HB_OUTPUTMUTEX:
         HB_CRITICAL_DESTROY( hb_outputMutex );
         break;
      case S_SERVICEMUTEX:
         HB_CRITICAL_DESTROY( s_ServiceMutex );
         break;
   }
#else
   HB_SYMBOL_UNUSED( iMtx );
#endif
}

HB_EXTERN_END

HB_FUNC( HB_GETCURRENTPROCESSID )
{
   HB_THREAD_STUB
#if defined( HB_OS_WIN )
   hb_retnl( (ULONG) GetCurrentProcessId() );
#else
   hb_retnl( (ULONG) getpid() );
#endif
}
