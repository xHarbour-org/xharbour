/*
* $Id: thread.h,v 1.27 2003/03/02 15:22:30 jonnymind Exp $
*/

/*
* xHarbour Project source code:
* The MT support
*
* Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
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

#ifndef HB_THREAD_H_
#define HB_THREAD_H_

#include "hbdefs.h"
#include "hbstack.h"
#include "hbapierr.h"

#ifdef HB_THREAD_SUPPORT

#if defined( HB_OS_WIN_32 )
   #ifndef _WIN32_WINNT
      #define _WIN32_WINNT 0x0400
   #endif
   #include <windows.h>
#endif

#if defined(HB_OS_WIN_32)
   #define HB_THREAD_T                 DWORD

   #define HB_CRITICAL_T               CRITICAL_SECTION
   #define HB_CRITICAL_INIT( x )       InitializeCriticalSection( &(x) )
   #define HB_CRITICAL_DESTROY( x )    DeleteCriticalSection( &(x) )
   #define HB_CRITICAL_LOCK( x )       EnterCriticalSection( &(x) )
   #define HB_CRITICAL_UNLOCK( x )     LeaveCriticalSection( &(x) )
   #define HB_CRITICAL_TRYLOCK( x )    TryEnterCriticalSection( &(x) )

   #define HB_MUTEX_T                  HANDLE
   #define HB_MUTEX_INIT( x )          x = CreateMutex( NULL, FALSE, NULL )
   #define HB_MUTEX_DESTROY( x )       CloseHandle( x )
   #define HB_MUTEX_LOCK( x )          WaitForSingleObject( x, INFINITE )
   #define HB_MUTEX_UNLOCK( x )        ReleaseMutex( x )

   #define HB_COND_T                   HANDLE
   #define HB_COND_INIT( x )           x = CreateEvent( NULL,FALSE, FALSE, NULL )

   DWORD hb_SignalObjectAndWait( HB_COND_T hToSignal, HB_MUTEX_T hToWaitFor, DWORD dwMillisec, BOOL bUnused );

   #define HB_COND_WAIT( x, y )        hb_SignalObjectAndWait( y, x, INFINITE, FALSE )
   #define HB_COND_WAITTIME( x, y, t ) hb_SignalObjectAndWait( y, x, t, FALSE )
   #define HB_COND_SIGNAL( x )         PulseEvent( x )
   #define HB_COND_DESTROY( x )        CloseHandle( x )

   #define HB_CURRENT_THREAD           GetCurrentThreadId
   
   // No need for cleanup pushing under windows (for now)
   #define HB_CLEANUP_PUSH(X,Y)
   #define HB_CLEANUP_POP( X )
#else

    #include <pthread.h>
    #define HB_THREAD_T                 pthread_t

    #define HB_MUTEX_T                  pthread_mutex_t
    #define HB_MUTEX_INIT( x )          pthread_mutex_init( &(x), NULL )
    #define HB_MUTEX_DESTROY( x )       pthread_mutex_destroy( &(x) )
    #define HB_MUTEX_LOCK( x )          pthread_mutex_lock( &(x) )
    #define HB_MUTEX_UNLOCK( x )        pthread_mutex_unlock( &(x) )

/** JC1:
    To reactivate flat unix mutex, uncomment this section and comment HB_CRITICAL_T delarations below

    #define HB_CRITICAL_T                  pthread_mutex_t
    #define HB_CRITICAL_INIT( x )          pthread_mutex_init( &(x), NULL )
    #define HB_CRITICAL_DESTROY( x )       pthread_mutex_destroy( &(x) )
    #define HB_CRITICAL_LOCK( x )          pthread_mutex_lock( &(x) )
    #define HB_CRITICAL_UNLOCK( x )        pthread_mutex_unlock( &(x) )
**/

    extern int hb_condTimeWait( pthread_cond_t *cond, pthread_mutex_t *mutex, int iMillisec );
    extern void hb_mutexForceUnlock( void *);
    extern void hb_rawMutexForceUnlock( void *);

    
    #define HB_COND_T                   pthread_cond_t
    #define HB_COND_INIT( x )           pthread_cond_init( &(x), NULL )
    #define HB_COND_WAIT( x, y )        pthread_cond_wait( &(x), &(y) )
    #define HB_COND_WAITTIME( x, y, t )  hb_condTimeWait( &(x) , &(y), t )
    #define HB_COND_SIGNAL( x )         pthread_cond_signal( &(x) )
    #define HB_COND_DESTROY( x )        pthread_cond_destroy( &(x) )

    #define HB_CURRENT_THREAD           pthread_self
    #define HB_TEST_CANCEL              pthread_testcancel
    #define HB_CLEANUP_PUSH(x, y )      pthread_cleanup_push( x, (void *)&(y) )
    #define HB_CLEANUP_POP              pthread_cleanup_pop    
#endif

/* Complex Mutex Structure*/
typedef struct {
   HB_MUTEX_T mutex;
   HB_COND_T cond;
   HB_THREAD_T locker;
   USHORT lock_count;
   int waiting;
   PHB_ITEM aEventObjects;
} HB_MUTEX_STRUCT;

/* Context */
typedef struct tag_HB_THREAD_CONTEXT
{
   HB_THREAD_T th_id;
   HB_STACK *stack;
   /* Is this thread going to run a method? */
   BOOL bIsMethod;
   /* data to initialize the stack */
   UINT uiParams;   
   /* Flag to signal that the context is in use */
   BOOL bInUse; // this must be used with the guard of a global resource

#ifdef HB_OS_WIN_32
   HANDLE th_h;
   BOOL bCanceled; // set when there is a cancel request and bInUse is true
#endif
   struct tag_HB_THREAD_CONTEXT *next;
} HB_THREAD_CONTEXT;

extern HB_THREAD_CONTEXT *last_context;

/* Ligthweight system indepented reentrant mutex, used internally by harbour */
typedef struct tag_HB_LWR_MUTEX
{
    HB_THREAD_T Locker;
#if defined(HB_OS_WIN32)
    HB_CRITICAL_T Critical;
#else
    HB_MUTEX_T Critical;
#endif
    int nCount;
} HB_LWR_MUTEX;

/*********************************************************************/
/* Shared resource is a set of a resource, a mutex and a condition. */

typedef struct tag_HB_SHARED_RESOURCE
{
   HB_MUTEX_T Mutex;  // mutex is used to read or write safely
   union {              // data that can be read or written
      long asLong;
      void *asPointer;
   } content;
   unsigned int aux;
   HB_COND_T Cond; //condition that may change
} HB_SHARED_RESOURCE;

#define HB_SHARED_INIT( pshr, data ) \
{ \
   HB_MUTEX_INIT( pshr.Mutex );\
   HB_COND_INIT( pshr.Cond ); \
   pshr.aux = 0;\
   pshr.content.asLong = data;\
}

#define HB_SHARED_DESTROY( pshr ) \
{ \
   HB_MUTEX_DESTROY( pshr.Mutex );\
   HB_COND_DESTROY( pshr.Cond ); \
}

#define HB_COND_EQUAL         0
#define HB_COND_GREATER       1
#define HB_COND_GREATER_EQUAL 2
#define HB_COND_LESS          3
#define HB_COND_INC           1
#define HB_COND_SET           0

#include <hbtrace.h>
/* Lightweight macro for condition check */
//TraceLog( "mtgc.log", "FILE %s(%d): SET PSHR: %d   THID: %d\r\n", __FILE__, __LINE__, pshr.content.asLong, HB_CURRENT_THREAD() );
#define HB_SET_SHARED( pshr, pMode, pValue ) \
{\
   HB_MUTEX_LOCK( pshr.Mutex );\
   pshr.content.asLong = pMode == HB_COND_INC ? pshr.content.asLong + pValue : pValue;\
   HB_COND_SIGNAL( pshr.Cond );\
   HB_MUTEX_UNLOCK( pshr.Mutex );\
}

/* Lightweight macro for condition check */
#define HB_WAIT_SHARED( pshr, cond, pData, pMode, pValue ) \
{\
   HB_CLEANUP_PUSH( hb_rawMutexForceUnlock, pshr.Mutex );\
   HB_MUTEX_LOCK( pshr.Mutex );\
   while ( 1 ) \
   {\
      if ( (cond == HB_COND_EQUAL && (long) pData == pshr.content.asLong) ||\
         (cond == HB_COND_GREATER && pshr.content.asLong > (long) pData) ||\
         (cond == HB_COND_GREATER_EQUAL && pshr.content.asLong >= (long) pData) || \
         (cond == HB_COND_LESS && pshr.content.asLong < (long) pData) )\
         break;\
      HB_COND_WAIT( pshr.Cond, pshr.Mutex );\
   }\
   pshr.content.asLong = pMode == HB_COND_INC ? pshr.content.asLong + pValue : pValue;\
   HB_COND_SIGNAL( pshr.Cond );\
   HB_MUTEX_UNLOCK( pshr.Mutex );\
   HB_CLEANUP_POP( 0 );\
}
    
    
/***********************************************************************/
/* Function and globals definitions */
extern HB_THREAD_CONTEXT *hb_ht_context;
extern HB_THREAD_T hb_main_thread_id;

extern HB_THREAD_CONTEXT *hb_threadCreateContext( HB_THREAD_T th_id );
extern HB_THREAD_CONTEXT *hb_threadLinkContext( HB_THREAD_CONTEXT *tc );
extern HB_THREAD_CONTEXT *hb_threadUnlinkContext( HB_THREAD_T th_id );
extern void hb_threadDestroyContext( HB_THREAD_CONTEXT *pContext );
extern HB_THREAD_CONTEXT *hb_threadGetContext( HB_THREAD_T th_id );
extern void hb_threadInit( void );
extern void hb_threadExit( void );
extern void hb_threadCreateStack( HB_THREAD_CONTEXT *pContext, PHB_ITEM pArgs );
void hb_threadWaitAll();
void hb_threadKillAll();
void hb_threadSleep( int millisec );

#ifdef HB_OS_WIN_32
extern void hb_threadTerminator();
extern void hb_threadTerminator2( HB_THREAD_CONTEXT * );
#else
extern void hb_threadTerminator( void *pData );
#endif

/** JC1:
   In MT libs, every function accessing stack will record the HB_STACK 
   (provided by hb_threadGetCurrentStack()) into a local Stack variable, and
   this variable will be accessed instead of HB_VM_STACK.
*/
#define hb_threadGetCurrentStack() ( hb_threadGetCurrentContext()->stack )
#define hb_threadGetCurrentContext() ( hb_threadGetContext( HB_CURRENT_THREAD() ) )

#ifdef HB_THREAD_OPTIMIZE_STACK
   #define HB_VM_STACK (*_pStack_)
   #define HB_VM_CONTEXT (*_pContext_)
   #define HB_THREAD_STUB\
      HB_THREAD_CONTEXT *_pContext_ = hb_threadGetCurrentContext(); \
      HB_STACK *_pStack_ = _pContext_->stack; 
#else
   #define HB_VM_CONTEXT (* hb_threadGetCurrentContext() )
   #define HB_VM_STACK (* hb_threadGetCurrentStack() )
   #define HB_THREAD_STUB
#endif


/* LWRM management */

/* JC1: If we want flat mutex, this section should be uncommented
extern void hb_threadLock( HB_LWR_MUTEX *m );
extern void hb_threadUnlock( HB_LWR_MUTEX *m );
*/

/** AUTO reentrant mutex if using UNIX */
/** JC1: we'll be using it in POSIX implementation without reentrant mutexes */
#if ( defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE ) ) && \
   ! defined( HB_OS_LINUX )

    #define HB_CRITICAL_T               HB_LWR_MUTEX
    #define HB_CRITICAL_INIT( x )       \
            { \
               HB_MUTEX_INIT( x.Critical );    \
               x.Locker = 0; \
               x.nCount = 0; \
            }

    #define HB_CRITICAL_DESTROY( x )    HB_MUTEX_DESTROY( x.Critical )

    #define HB_CRITICAL_LOCK( lpMutex )  \
         { \
            if ( lpMutex.Locker == HB_CURRENT_THREAD() )\
            {\
               lpMutex.nCount++;\
            }\
            else\
            {\
               HB_MUTEX_LOCK( lpMutex.Critical );\
               lpMutex.nCount = 1;\
               lpMutex.Locker = HB_CURRENT_THREAD();\
            }\
         }
         
    BOOL hb_critical_mutex_trylock( HB_CRITICAL_T *lpMutex );
    #define HB_CRITICAL_TRYLOCK( Mutex )   hb_critical_mutex_trylock( &(Mutex) )
    
    #define HB_CRITICAL_UNLOCK( lpMutex ) \
         {\
            if ( lpMutex.Locker == HB_CURRENT_THREAD() )\
            {\
               lpMutex.nCount--;\
               if ( lpMutex.nCount == 0 )\
               {\
                  lpMutex.Locker = 0;\
                  HB_MUTEX_UNLOCK( lpMutex.Critical );\
               }\
            }\
         }
#else
   #ifdef HB_OS_LINUX
      #include <errno.h>
   /* ODD: this definition is missing on some linux headers;
      we should remove it when this bug is fixed */
   int pthread_mutexattr_setkind_np( pthread_mutexattr_t * attr, int kind );
      #define HB_CRITICAL_T               HB_MUTEX_T

      #define HB_CRITICAL_INIT( x )       \
      {\
         pthread_mutexattr_t attr;\
         pthread_mutexattr_init( &attr );\
         pthread_mutexattr_setkind_np( &attr, PTHREAD_MUTEX_RECURSIVE_NP);\
         pthread_mutex_init( &(x), &attr );\
         pthread_mutexattr_destroy( &attr );\
      }

      /*#define HB_CRITICAL_LOCK( x ) \
      {\
         pthread_setcanceltype( PTHREAD_CANCEL_DEFERRED, NULL );\
         pthread_cleanup_push(hb_mutexForceUnlock, (void *) &(x));\
         HB_MUTEX_LOCK( x );\
         pthread_testcancel();\
         pthread_cleanup_pop(0);\
         pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, NULL );\
      }*/

      #define HB_CRITICAL_LOCK( x )       HB_MUTEX_LOCK( x );
      #define HB_CRITICAL_UNLOCK( x )     HB_MUTEX_UNLOCK( x );
      #define HB_CRITICAL_DESTROY( x )    HB_MUTEX_DESTROY( x )
      #define HB_CRITICAL_TRYLOCK( x )    ( pthread_mutex_trylock( &(x) ) != EBUSY )

   #endif
#endif

/* More elegant guard of a small section of code */
#define HB_THREAD_GUARD( mutex, code )\
   {\
      HB_CRITICAL_LOCK( mutex );\
      { code; }\
      HB_CRITICAL_UNLOCK( mutex );\
   }

/************************************************************
* List of mutexes that can be used to regulate concurency
*************************************************************/
/* Monitor for sync access to the garbage collecting process */
extern HB_CRITICAL_T hb_garbageMutex;
/* Monitor for sync access to the local contexts */
extern HB_CRITICAL_T hb_threadContextMutex;
/* Monitor for sync access to the global context */
extern HB_CRITICAL_T hb_globalsMutex;
/* Monitor for sync access to the global context */
extern HB_CRITICAL_T hb_staticsMutex;
/* Monitor for sync access to the global context */
extern HB_CRITICAL_T hb_memvarsMutex;
/* Guard for threadunsafe malloc and free */
extern HB_CRITICAL_T hb_allocMutex;
/* Guard for console and output and free */
extern HB_CRITICAL_T hb_outputMutex;
/* Guard for memory allocated by the garbage collector */
extern HB_CRITICAL_T hb_garbageAllocMutex;

/* count of running contexts; set to -1 to block contexts from running */
extern HB_SHARED_RESOURCE hb_runningContexts;

/* Context using management */
#define HB_CONTEXT_LOCK \
{\
   HB_CLEANUP_PUSH( hb_rawMutexForceUnlock, hb_runningContexts.Mutex );\
   HB_MUTEX_LOCK( hb_runningContexts.Mutex );\
   if( ! HB_VM_CONTEXT.bInUse ) \
   {\
      while ( hb_runningContexts.content.asLong < 0 ) \
      {\
         HB_COND_WAIT( hb_runningContexts.Cond, hb_runningContexts.Mutex );\
      }\
      hb_runningContexts.content.asLong++;\
      HB_VM_CONTEXT.bInUse = TRUE;\
      HB_COND_SIGNAL( hb_runningContexts.Cond );\
   }\
   HB_MUTEX_UNLOCK( hb_runningContexts.Mutex );\
   HB_CLEANUP_POP( 0 );\
} 

#ifdef HB_OS_WIN32

   #define HB_CONTEXT_UNLOCK \
   {\
      HB_MUTEX_LOCK( hb_runningContexts.Mutex );\
      if( HB_VM_CONTEXT.bInUse ) \
      {\
         hb_runningContexts.content.asLong--;\
         HB_VM_CONTEXT.bInUse = FALSE;\
         HB_COND_SIGNAL( hb_runningContexts.Cond );\
      }\
      HB_MUTEX_UNLOCK( hb_runningContexts.Mutex );\
      if( HB_VM_CONTEXT.bCanceled ) \
      {\
         hb_threadTerminator();\
         ExitThread( 0 );\
      }\
   } 

#else

   #define HB_CONTEXT_UNLOCK \
   {\
      HB_MUTEX_LOCK( hb_runningContexts.Mutex );\
      if( HB_VM_CONTEXT.bInUse ) \
      {\
         hb_runningContexts.content.asLong--;\
         HB_VM_CONTEXT.bInUse = FALSE;\
         HB_COND_SIGNAL( hb_runningContexts.Cond );\
      }\
      HB_MUTEX_UNLOCK( hb_runningContexts.Mutex );\
   } 
#endif

#define HB_CONTEXT_STOP_TELLING( var, value )\
{\
   HB_CLEANUP_PUSH( hb_rawMutexForceUnlock, hb_runningContexts.Mutex );\
   HB_MUTEX_LOCK( hb_runningContexts.Mutex );\
   var = value;\
   while ( hb_runningContexts.content.asLong != 0 ) \
   {\
      HB_COND_WAIT( hb_runningContexts.Cond, hb_runningContexts.Mutex );\
   }\
   hb_runningContexts.content.asLong = -1;\
   HB_COND_SIGNAL( hb_runningContexts.Cond );\
   HB_MUTEX_UNLOCK( hb_runningContexts.Mutex );\
   HB_CLEANUP_POP( 0 );\
}

#define HB_CONTEXT_STOP    ( HB_WAIT_SHARED( hb_runningContexts, HB_COND_EQUAL, 0, HB_COND_SET, -1 ) )
#define HB_CONTEXT_START   HB_SET_SHARED( hb_runningContexts, HB_COND_SET, 0 )

/* LEVEL 3 shell lock - locks all inner level objects */
#if defined(HB_OS_UNIX) && ! defined(HB_OS_LINUX )

   #define HB_SHELL_LOCK_LEVEL_3 \
      HB_CRITICAL_UNLOCK( s_mtxTryLock ); \
      HB_CRITICAL_LOCK( hb_allocMutex ); \
      HB_CRITICAL_LOCK( hb_garbageAllocMutex ); \
      HB_CRITICAL_LOCK( hb_outputMutex )
   #define HB_SHELL_UNLOCK_LEVEL_3 \
      HB_CRITICAL_UNLOCK( hb_outputMutex ); \
      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex ); \
      HB_CRITICAL_UNLOCK( hb_allocMutex );\
      HB_CRITICAL_UNLOCK( s_mtxTryLock )

#else

   #define HB_SHELL_LOCK_LEVEL_3 \
      HB_CRITICAL_LOCK( hb_allocMutex ); \
      HB_CRITICAL_LOCK( hb_garbageAllocMutex ); \
      HB_CRITICAL_LOCK( hb_outputMutex )
   #define HB_SHELL_UNLOCK_LEVEL_3 \
      HB_CRITICAL_UNLOCK( hb_outputMutex ); \
      HB_CRITICAL_UNLOCK( hb_garbageAllocMutex ); \
      HB_CRITICAL_UNLOCK( hb_allocMutex )
#endif

/* Level 2 shell lock - locks all the top-level objects */
#define HB_SHELL_LOCK_LEVEL_2 \
      HB_CRITICAL_LOCK( hb_threadContextMutex ); \
      HB_CRITICAL_LOCK( hb_globalsMutex ); \
      HB_CRITICAL_LOCK( hb_staticsMutex ); \
      HB_CRITICAL_LOCK( hb_memvarsMutex )

#define HB_SHELL_UNLOCK_LEVEL_2 \
      HB_CRITICAL_UNLOCK( hb_memvarsMutex ); \
      HB_CRITICAL_UNLOCK( hb_staticsMutex ); \
      HB_CRITICAL_UNLOCK( hb_globalsMutex ); \
      HB_CRITICAL_UNLOCK( hb_threadContextMutex )

/* Level 1 shell lock - locks the single thread object */
#define HB_SHELL_LOCK_LEVEL_1 \
   HB_CRITICAL_LOCK( hb_garbageMutex )

#define HB_SHELL_UNLOCK_LEVEL_1 \
   HB_CRITICAL_UNLOCK( hb_garbageMutex )

   
   
   
/******************************************************/
/* Definitions when threading is turned off */

#else 

   #define HB_CRITICAL_LOCK( x )
   #define HB_CRITICAL_TRYLOCK( x )
   #define HB_CRITICAL_UNLOCK( x )
   #define HB_THREAD_GUARD( mutex, code ) { code; }

   #define HB_TEST_CANCEL
   #define HB_SET_SHARED( x, y, z )
   #define HB_WAIT_SHARED( x, y, z, k, m )
   #define HB_CONTEXT_STOP   
   #define HB_CONTEXT_STOP_TELLING(x, y)
   #define HB_CONTEXT_START  
   #define HB_CONTEXT_LOCK  
   #define HB_CONTEXT_UNLOCK   
   #define HB_CLEANUP_PUSH( x, y )
   #define HB_CLEANUP_POP( x )
   
   #define HB_THREAD_STUB
   #define HB_VM_CONTEXT
   #define HB_VM_STACK hb_stack

#endif

#endif
