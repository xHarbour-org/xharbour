/*
* $Id: thread.h,v 1.8 2002/12/20 19:00:49 ronpinkas Exp $
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
#include "hbvm.h"
#include "hbapierr.h"

#if defined( HB_OS_WIN_32 )
   #ifndef _WIN32_WINNT
      #define _WIN32_WINNT 0x0400
   #endif
   #include <windows.h>
#endif

#if defined(HB_OS_WIN_32)
   #define HB_THREAD_T                 DWORD
   #define HB_THREAD_HANDLE            HANDLE

   #define HB_CRITICAL_T               CRITICAL_SECTION
   #define HB_CRITICAL_INIT( x )       InitializeCriticalSection( &(x) )
   #define HB_CRITICAL_DESTROY( x )    DeleteCriticalSection( &(x) )
   #define HB_CRITICAL_LOCK( x )       EnterCriticalSection( &(x) )
   #define HB_CRITICAL_UNLOCK( x )     LeaveCriticalSection( &(x) )

   #define HB_MUTEX_T                  HANDLE
   #define HB_MUTEX_INIT( x )          x = CreateMutex( NULL, FALSE, NULL )
   #define HB_MUTEX_DESTROY( x )       CloseHandle( x )
   #define HB_MUTEX_LOCK( x )          WaitForSingleObject( x, INFINITE )
   #define HB_MUTEX_UNLOCK( x )        ReleaseMutex( x )

   #define HB_COND_T                   HANDLE
   #define HB_COND_INIT( x )           x = CreateEvent( NULL, TRUE, FALSE, NULL )

   #define HB_COND_WAIT( x, y )        SignalObjectAndWait( y, x, INFINITE, FALSE )
   #define HB_COND_WAITTIME( x, y, t ) SignalObjectAndWait( y, x, t *1000, FALSE )
   #define HB_COND_SIGNAL( x )         SetEvent( x )
   #define HB_COND_DESTROY( x )        CloseHandle( x )

   #define HB_CURRENT_THREAD           GetCurrentThreadId
   #define HB_CURRENT_THREAD_HANDLE    GetCurrentThread
#else
    #include <pthread.h>
    #define HB_THREAD_T                 pthread_t
    #define HB_THREAD_HANDLE            pthread_t

    #define HB_CRITICAL_T               pthread_mutex_t
    #define HB_CRITICAL_INIT( x )       pthread_mutex_init( &(x), NULL )
    #define HB_CRITICAL_DESTROY( x )    pthread_mutex_destroy( &(x) )
    #define HB_CRITICAL_LOCK( x )       pthread_mutex_lock( &(x) )
    #define HB_CRITICAL_UNLOCK( x )     pthread_mutex_unlock( &(x) )

    #define HB_MUTEX_T                  pthread_mutex_t
    #define HB_MUTEX_INIT( x )          pthread_mutex_init( &(x), NULL )
    #define HB_MUTEX_DESTROY( x )       pthread_mutex_destroy( &(x) )
    #define HB_MUTEX_LOCK( x )          pthread_mutex_lock( &(x) )
    #define HB_MUTEX_UNLOCK( x )        pthread_mutex_unlock( &(x) )

    #define HB_COND_T                   pthread_cond_t
    #define HB_COND_INIT( x )           pthread_cond_init( &(x), NULL )
    #define HB_COND_WAIT( x, y )        pthread_cond_wait( &(x), &(y) )
    #define HB_COND_WAITTIME( x, y, t )  \
        {\
            struct timeval now;\
            struct timespec timeout;\
            gettimeofday(&now, 0);\
            timeout.tv_sec = now.tv_sec + t /1000;\
            timeout.tv_nsec = now.tv_usec * 1000 + (t % 1000) * 1000000 ;\
            pthread_cond_timedwait( &x, &y, &timeout );\
        }
    #define HB_COND_SIGNAL( x )         pthread_cond_signal( &x )
    #define HB_COND_DESTROY( x )        pthread_cond_destroy( &x )

    #define HB_CURRENT_THREAD           pthread_self
    #define HB_CURRENT_THREAD_HANDLE    pthread_self

/* Thread support is only for linux and windows now */
#endif

/* Complex Mutex Structure*/
typedef struct {
    HB_MUTEX_T mutex;
    HB_COND_T cond;
    HB_THREAD_HANDLE locker;
	USHORT lock_count;
	int waiting;
	PHB_ITEM event_object;
} HB_MUTEX_STRUCT;

/* Context */
typedef struct tag_HB_THREAD_CONTEXT {
    HB_THREAD_T th_id;
#if defined(HB_OS_WIN_32)
    HB_THREAD_HANDLE th_h;
#endif
    HB_STACK *stack;
    struct tag_HB_THREAD_CONTEXT *next;
} HB_THREAD_CONTEXT;

/* Parameters passed for thread creation */
typedef struct tag_HB_THREAD_PARAM {
    HB_THREAD_T th_id;
    PHB_ITEM args;
    USHORT count;
} HB_THREAD_PARAM;

extern HB_STACK hb_stack_general;
extern HB_THREAD_CONTEXT *hb_ht_context;
extern HB_CRITICAL_T context_monitor;

extern void hb_createContext( void );
extern void hb_destroyContext( void );
extern void hb_destroyContextFromHandle( HB_THREAD_HANDLE th_h );
extern void hb_threadInit( void );
extern void hb_threadExit( void );
extern HB_THREAD_CONTEXT *hb_getCurrentContext( void );

#endif
