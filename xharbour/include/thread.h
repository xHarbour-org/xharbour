/*
* $Id: thread.h,v 1.24 2003/02/22 05:54:57 jonnymind Exp $
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

    int hb_condTimeWait( pthread_cond_t *cond, pthread_mutex_t *mutex, int iMillisec );
    
    #define HB_COND_T                   pthread_cond_t
    #define HB_COND_INIT( x )           pthread_cond_init( &(x), NULL )
    #define HB_COND_WAIT( x, y )        pthread_cond_wait( &(x), &(y) )
    #define HB_COND_WAITTIME( x, y, t )  hb_condTimeWait( &(x) , &(y), t )
    #define HB_COND_SIGNAL( x )         pthread_cond_signal( &(x) )
    #define HB_COND_DESTROY( x )        pthread_cond_destroy( &(x) )

    #define HB_CURRENT_THREAD           pthread_self

/* Thread support is only for linux and windows now */
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
   /* monitor must be owned to operate on the context */
#ifdef HB_OS_WIN_32
    HANDLE th_h;
#endif
   struct tag_HB_THREAD_CONTEXT *next;
} HB_THREAD_CONTEXT;

extern HB_THREAD_CONTEXT *last_context;
/* Parameters passed for thread creation */
typedef struct tag_HB_THREAD_PARAM
{
    HB_THREAD_T th_id;
    PHB_ITEM pArgs;
    USHORT uiCount;
    BOOL bIsMethod;
#ifdef HB_OS_WIN_32
   HB_THREAD_CONTEXT *context;
#endif
} HB_THREAD_PARAM;

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


extern HB_THREAD_CONTEXT *hb_ht_context;
extern HB_THREAD_T hb_main_thread_id;

extern HB_THREAD_CONTEXT *hb_threadCreateContext( HB_THREAD_T th_id );
extern HB_THREAD_CONTEXT *hb_threadLinkContext( HB_THREAD_CONTEXT *tc );
extern HB_THREAD_CONTEXT *hb_threadUnlinkContext( HB_THREAD_T th_id );
extern void hb_threadDestroyContext( HB_THREAD_CONTEXT *pContext );
extern HB_THREAD_CONTEXT *hb_threadGetContext( HB_THREAD_T th_id );
extern void hb_threadInit( void );
extern void hb_threadExit( void );

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
    #define HB_CRITICAL_TRYLOCK( lpMutex )   hb_critical_mutex_trylock( lpMutex )
    
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

      #define HB_CRITICAL_LOCK( x )       HB_MUTEX_LOCK( x )
      #define HB_CRITICAL_UNLOCK( x )     HB_MUTEX_UNLOCK( x )
      #define HB_CRITICAL_DESTROY( x )    HB_MUTEX_DESTROY( x )
      #define HB_CRITICAL_TRYLOCK( x )    pthread_mutex_trylock( x )
   #endif
#endif

/* Forbidder mutex for xharbour */
typedef struct tag_HB_FORBID_MUTEX
{
    HB_CRITICAL_T Control;
    long lCount;
} HB_FORBID_MUTEX;

/* Forbidden mutex management */
void hb_threadForbidenInit( HB_FORBID_MUTEX *Forbid );
void hb_threadForbidenDestroy( HB_FORBID_MUTEX *Forbid );
extern void hb_threadForbid( HB_FORBID_MUTEX * );
extern void hb_threadAllow( HB_FORBID_MUTEX * );

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

#else

   #define HB_CRITICAL_LOCK( x )
   #define HB_CRITICAL_TRYLOCK( x )
   #define HB_CRITICAL_UNLOCK( x )
   #define HB_THREAD_GUARD( mutex, code ) { code; }

#endif


#endif
