/*
* $Id: thread.h,v 1.60 2003/10/18 01:15:18 jonnymind Exp $
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
*/

#ifndef HB_THREAD_H_
#define HB_THREAD_H_

#include "hbapi.h"

#ifdef HB_THREAD_SUPPORT

/* Check if malloc/free is thread safe */
/*
JC1: tests demonstrates that this does not work
#ifndef HB_SAFE_ALLOC
#  if defined( HB_OS_LINUX ) && (defined(_THREAD_SAFE) || defined(_REENTRANT))
#    define HB_SAFE_ALLOC
#  endif
#endif
*/

/* We should assert that cleanup functions must be in limited number */
typedef void (*HB_CLEANUP_FUNC)(void *);
#define HB_MAX_CLEANUPS  12
#define HB_THREAD_MAX_UNIQUE_ID  32000

/* Maximun number of cycles that can be completed by VM without stack unlock */
#define HB_VM_UNLOCK_PERIOD 20

#if defined(HB_OS_WIN_32)
   #ifndef _WIN32_WINNT
      #define _WIN32_WINNT 0x0400
   #endif
   #define _WINSOCKAPI_  /* Prevents inclusion of Winsock.h in Windows.h */
   #include <windows.h>

   typedef struct tag_HB_WINCOND_T
   {
      HANDLE semBlockLock;
      HANDLE semBlockQueue;
      CRITICAL_SECTION mtxUnblockLock;
      int nWaitersGone;
      int nWaitersBlocked;
      int nWaitersToUnblock;
   } HB_WINCOND_T, *PHB_WINCOND_T;

   #define HB_THREAD_T                 DWORD

   #define HB_CRITICAL_T               CRITICAL_SECTION
   #define HB_CRITICAL_INIT( x )       InitializeCriticalSection( &(x) )
   #define HB_CRITICAL_DESTROY( x )    DeleteCriticalSection( &(x) )
   #define HB_CRITICAL_LOCK( x )       EnterCriticalSection( &(x) )
   #define HB_CRITICAL_UNLOCK( x )     LeaveCriticalSection( &(x) )
   #define HB_CRITICAL_TRYLOCK( x )    TryEnterCriticalSection( &(x) )

   #define HB_MUTEX_T                  HANDLE
   #define HB_MUTEX_INIT( x )          ( x = CreateSemaphore( NULL, 1, 1, NULL) )
   #define HB_MUTEX_DESTROY( x )       CloseHandle( x )
   #define HB_MUTEX_LOCK( x )          WaitForSingleObject( x, INFINITE )
   #define HB_MUTEX_UNLOCK( x )        ReleaseSemaphore( x, 1, NULL )

   #define HB_COND_T                   HB_WINCOND_T
   #define HB_COND_INIT( x )           hb_threadCondInit( &(x) )
   #define HB_COND_WAIT( x, y )        hb_threadCondWait( &(x), y, INFINITE )
   #define HB_COND_WAITTIME( x, y, t ) hb_threadCondWait( &(x), y, t )
   #define HB_COND_SIGNAL( x )         hb_threadCondSignal( &(x) )
   #define HB_COND_DESTROY( x )        hb_threadCondDestroy( &(x) )

   typedef void ( * HB_IDLE_FUNC )( void );

   #define HB_CURRENT_THREAD           GetCurrentThreadId
   #define HB_SAME_THREAD(x, y)        (x == y)

   /* Guard for cancellation requets */
   extern HB_CRITICAL_T hb_cancelMutex;

   #define HB_ENABLE_ASYN_CANC       HB_THREAD_GUARD( hb_cancelMutex, HB_VM_STACK.bCanCancel = TRUE )
   #define HB_DISABLE_ASYN_CANC      HB_THREAD_GUARD( hb_cancelMutex, HB_VM_STACK.bCanCancel = FALSE )
   #define HB_TEST_CANCEL_ENABLE_ASYN\
   {\
      HB_CRITICAL_LOCK( hb_cancelMutex );\
      if ( HB_VM_STACK.bCanceled )\
      {\
         HB_CRITICAL_UNLOCK( hb_cancelMutex );\
         hb_threadCancelInternal();\
      }\
      HB_VM_STACK.bCanCancel = TRUE;\
      HB_CRITICAL_UNLOCK( hb_cancelMutex );\
   }

   #define HB_TEST_CANCEL\
   {\
      HB_CRITICAL_LOCK( hb_cancelMutex );\
      if ( HB_VM_STACK.bCanceled )\
      {\
         HB_CRITICAL_UNLOCK( hb_cancelMutex );\
         hb_threadCancelInternal();\
      }\
      HB_CRITICAL_UNLOCK( hb_cancelMutex );\
   }


   #define HB_CLEANUP_PUSH(X,Y)
   #define HB_CLEANUP_POP
   #define HB_CLEANUP_POP_EXEC

#ifdef __cplusplus
extern "C" {
#endif
   extern DWORD hb_dwCurrentStack;
#ifdef __cplusplus
}
#endif
   #define hb_threadGetCurrentStack() ( (HB_STACK *) TlsGetValue( hb_dwCurrentStack ) )
/*
   #define HB_STACK_LOCK \
   {\
      HB_MUTEX_LOCK( hb_runningStacks.Mutex );\
      if( ! HB_VM_STACK.bInUse ) \
      {\
         hb_runningStacks.content.asLong++;\
         HB_VM_STACK.bInUse = TRUE;\
      }\
      HB_MUTEX_UNLOCK( hb_runningStacks.Mutex );\
   }

   #define HB_STACK_UNLOCK \
   {\
      HB_MUTEX_LOCK( hb_runningStacks.Mutex );\
      if( HB_VM_STACK.bInUse ) \
      {\
         HB_VM_STACK.bInUse = FALSE;\
         if ( --hb_runningStacks.content.asLong == 0)\
         {\
            hb_threadCallIdle();\
         }\
      }\
      HB_MUTEX_UNLOCK( hb_runningStacks.Mutex );\
   }

   typedef struct tag_HB_IDLE_FUNC_LIST
   {
      HB_IDLE_FUNC func;
      struct tag_HB_IDLE_FUNC_LIST *next;
   } HB_IDLE_FUNC_LIST;
*/
#else

   #include <pthread.h>
   #include <errno.h>
   #define HB_THREAD_T                 pthread_t
   #define HB_CRITICAL_T               pthread_mutex_t

   /* ODD: this definition is missing on some linux headers;
      we should remove it when this bug is fixed */
   int pthread_mutexattr_setkind_np( pthread_mutexattr_t * attr, int kind );
   #define HB_CRITICAL_INIT( x )       \
      {\
         pthread_mutexattr_t attr;\
         pthread_mutexattr_init( &attr );\
         pthread_mutexattr_setkind_np( &attr, PTHREAD_MUTEX_RECURSIVE_NP);\
         pthread_mutex_init( &(x), &attr );\
         pthread_mutexattr_destroy( &attr );\
      }

   #define HB_MUTEX_T                  pthread_mutex_t
   #define HB_CRITICAL_DESTROY( x )    pthread_mutex_destroy( &(x) )
   #define HB_CRITICAL_LOCK( x )       pthread_mutex_lock( &(x) )
   #define HB_CRITICAL_UNLOCK( x )     pthread_mutex_unlock( &(x) )
   #define HB_CRITICAL_TRYLOCK( x )    ( pthread_mutex_trylock( &(x) ) != EBUSY )

   #define HB_MUTEX_INIT( x )          HB_CRITICAL_INIT( x )
   #define HB_MUTEX_DESTROY( x )       HB_CRITICAL_DESTROY( x )
   #define HB_MUTEX_LOCK( x )          HB_CRITICAL_LOCK( x )
   #define HB_MUTEX_UNLOCK( x )        HB_CRITICAL_UNLOCK( x )

   extern int hb_condTimeWait( pthread_cond_t *cond, pthread_mutex_t *mutex, int iMillisec );

   #define HB_COND_T                   pthread_cond_t
   #define HB_COND_INIT( x )           pthread_cond_init( &(x), NULL )
   #define HB_COND_WAIT( x, y )        pthread_cond_wait( &(x), &(y) )
   #define HB_COND_WAITTIME( x, y, t )  hb_condTimeWait( &(x) , &(y), t )
   #define HB_COND_SIGNAL( x )         pthread_cond_broadcast( &(x) )
   #define HB_COND_DESTROY( x )        pthread_cond_destroy( &(x) )

   #define HB_CURRENT_THREAD           pthread_self
   #define HB_CLEANUP_PUSH(x, y )      pthread_cleanup_push( x, (void *)&(y) )
   #define HB_CLEANUP_POP              pthread_cleanup_pop(0)
   #define HB_CLEANUP_POP_EXEC         pthread_cleanup_pop(1)
   #define HB_SAME_THREAD(x,y)         pthread_equal( x, y )

   #define HB_ENABLE_ASYN_CANC
   #define HB_DISABLE_ASYN_CANC
   #define HB_TEST_CANCEL_ENABLE_ASYN  pthread_testcancel()
   #define HB_TEST_CANCEL              pthread_testcancel()

   extern pthread_key_t hb_pkCurrentStack;
   #define hb_threadGetCurrentStack() ( (HB_STACK *) pthread_getspecific( hb_pkCurrentStack ) )

#endif

/* Context using management */
#define HB_STACK_LOCK \
{\
   HB_MUTEX_LOCK( hb_runningStacks.Mutex );\
   if( ! HB_VM_STACK.bInUse ) \
   {\
      while ( hb_runningStacks.aux ) \
      {\
         HB_COND_WAIT( hb_runningStacks.Cond, hb_runningStacks.Mutex );\
      }\
      hb_runningStacks.content.asLong++;\
      HB_VM_STACK.bInUse = TRUE;\
      HB_COND_SIGNAL( hb_runningStacks.Cond );\
   }\
   HB_MUTEX_UNLOCK( hb_runningStacks.Mutex );\
}

#define HB_STACK_UNLOCK \
{\
   HB_MUTEX_LOCK( hb_runningStacks.Mutex );\
   if( HB_VM_STACK.bInUse ) \
   {\
      hb_runningStacks.content.asLong--;\
      HB_VM_STACK.bInUse = FALSE;\
      HB_COND_SIGNAL( hb_runningStacks.Cond );\
   }\
   HB_MUTEX_UNLOCK( hb_runningStacks.Mutex );\
}

extern void hb_threadWaitForIdle( void );

/**********************************************************/
/*
* Enanched stack for multithreading
*/

typedef struct
{
   PHB_DYNS pDynSym;             /* Pointer to dynamic symbol */
} DYNHB_ITEM, * PDYNHB_ITEM, * DYNHB_ITEM_PTR;

/* Forward declarations for stack */
struct HB_ERROR_INFO_;

/* Declaration from dbfcmd.c in rdd */
typedef struct _AREANODE
{
   void * pArea;               /* WorkAreas with different sizes */
   struct _AREANODE * pPrev;   /* Prev WorkArea in the list */
   struct _AREANODE * pNext;   /* Next WorkArea in the list */
} AREANODE;

typedef AREANODE * LPAREANODE;

typedef struct tag_HB_STACK
{
   PHB_ITEM * pItems;       /* pointer to the stack items */
   PHB_ITEM * pPos;         /* pointer to the latest used item */
   LONG     wItems;       /* total items that may be holded on the stack */
   HB_ITEM  Return;       /* latest returned value */
   PHB_ITEM * pBase;        /* stack frame position for the current function call */
   PHB_ITEM * pEvalBase;    /* stack frame position for the evaluated codeblock */
   int      iStatics;     /* statics base for the current function call */
   char     szDate[ 9 ];  /* last returned date from _pards() yyyymmdd format */

   /* JC1: thread safe classes messaging */
   struct hb_class_method * pMethod;        /* Selcted method to send message to */

   UINT th_vm_id;

   HB_THREAD_T th_id;
   /* Is this thread going to run a method? */
   BOOL bIsMethod;
   /* data to initialize the stack */
   UINT uiParams;
   /* Flag to signal that the context is in use */
   BOOL bInUse; /* this must be used with the guard of a global resource */

   /* MT error handler, one for thread! */
   struct HB_ERROR_INFO_ *errorHandler;
   /* Codeblock for error handling */
   PHB_ITEM errorBlock;
   int     iLaunchCount;
   USHORT  uiErrorDOS; /* The value of DOSERROR() */

   /* List of error handlers for TRY/CATCH blocks */
   PHB_ITEM aTryCatchHandlerStack;
   /* VM requests and recover sequence */
   USHORT uiActionRequest;
   ULONG lRecoverBase;

   /* Mt With Object index */
   HB_ITEM aWithObject[ HB_MAX_WITH_OBJECTS ];
   USHORT  wWithObjectCounter;
   BOOL    bWithObject;

   /* Mt for each enumeration index */
   HB_ITEM  aEnumCollection[ HB_MAX_ENUMERATIONS ];
   PHB_ITEM apEnumVar[ HB_MAX_ENUMERATIONS ];
   ULONG    awEnumIndex[ HB_MAX_ENUMERATIONS ];
   USHORT   wEnumCollectionCounter;

   /* management of codeblock and macro params */
   int aiExtraParams[HB_MAX_MACRO_ARGS];
   int iExtraParamsIndex;
   PHB_SYMB apExtraParamsSymbol[HB_MAX_MACRO_ARGS];
   int aiExtraElements[HB_MAX_MACRO_ARGS];
   int iExtraElementsIndex;
   int iExtraElements;
   int iExtraIndex;


   /* FS api error system */
   USHORT uiErrorLast;
   USHORT uiOsErrorLast;

   /* Dynsym thread-specific table */
   UINT uiClosestDynSym;
   PDYNHB_ITEM pDynItems;
   USHORT uiDynSymbols;

   /* Management of PRIVATE variables (and macro memvars) */
   PHB_DYNS * privateStack;
   ULONG privateStackSize;
   ULONG privateStackCnt;
   ULONG privateStackBase;

   /* Management of globals memvars */
   ULONG globalTableSize;
   ULONG globalFirstFree;
   ULONG globalLastFree;
   ULONG globalFreeCnt;
   HB_VALUE_PTR globalTable;

   /* Pointers to hMemvar for thread aware dynsyms */
   HB_HANDLE *hMemvars;
   ULONG hMemvarsAllocated;
   ULONG hMemvarsLastFree;

   /* Data useful for dbcmd & friends */
   USHORT uiCurrArea;        /* Selectd area */
   LPAREANODE pCurrArea;  /* Pointer to a selected and valid area */

   struct tag_HB_STACK *next;

#ifdef HB_OS_WIN_32
   HANDLE th_h;
   BOOL bCanceled; /* set when there is a cancel request and bInUse is true */
   BOOL bCanCancel;
   /* Windows cleanup functions are working, but currently uneeded;
   So the are left here in [LEFTOVER] status
   HB_CLEANUP_FUNC *pCleanUp;
   void **pCleanUpParam;
   int iCleanCount;*/
#endif

} HB_STACK;

/* Complex Mutex Structure*/
typedef struct tag_HB_MUTEX_STRUCT {
   HB_MUTEX_T mutex;
   HB_COND_T cond;
   HB_THREAD_T locker;
   USHORT lock_count;
   int waiting;
   PHB_ITEM aEventObjects;
   struct tag_HB_MUTEX_STRUCT *next;
} HB_MUTEX_STRUCT;

/*********************************************************************/
/* Shared resource is a set of a resource, a mutex and a condition. */

typedef struct tag_HB_SHARED_RESOURCE
{
   HB_MUTEX_T Mutex;  /* mutex is used to read or write safely */
   union {              /* data that can be read or written */
      volatile long asLong;
      volatile void *asPointer;
   } content;
   volatile unsigned int aux;
   HB_COND_T Cond; /* condition that may change */
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
/*   TraceLog( "mtgc.log", "FILE %s(%d): BEGINWAIT: %d   THID: %d\r\n", __FILE__, __LINE__, pshr.content.asLong, HB_CURRENT_THREAD() ); */
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
   HB_CLEANUP_POP;\
}

/** JC1:
   In MT libs, every function accessing stack will record the HB_STACK
   (provided by hb_threadGetCurrentContext()) into a local Stack variable, and
   this variable will be accessed instead of HB_VM_STACK.
*/

#if defined( HB_THREAD_OPTIMIZE_STACK ) && ! defined( HB_NO_DEFAULT_STACK_MACROS )
   #define HB_VM_STACK (*_pStack_)
   #define HB_THREAD_STUB\
      HB_STACK *_pStack_ = hb_threadGetCurrentStack();
#else
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
#if 0
/* Ligthweight system indepented reentrant mutex, used internally by harbour */
typedef struct tag_HB_LWR_MUTEX
{
    HB_THREAD_T Locker;
    HB_CRITICAL_T Critical;
    int nCount;
} HB_LWR_MUTEX;

   ! defined( HB_OS_LINUX )

    #define HB_CRITICAL_T               HB_LWR_MUTEX
    #define HB_CRITICAL_INIT( x )       \
            { \
               HB_CRITICAL_INIT( x.Critical );    \
               x.Locker = 0; \
               x.nCount = 0; \
            }

    #define HB_CRITICAL_DESTROY( x )    HB_CRITICAL_DESTROY( x.Critical )

    #define HB_CRITICAL_LOCK( lpMutex )  \
         { \
            if ( lpMutex.Locker == HB_CURRENT_THREAD() )\
            {\
               lpMutex.nCount++;\
            }\
            else\
            {\
               HB_CRITICAL_LOCK( lpMutex.Critical );\
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
                  HB_CRITICAL_UNLOCK( lpMutex.Critical );\
               }\
            }\
         }
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
/* Monitor for sync access to the local contexts */
extern HB_CRITICAL_T hb_threadStackMutex;
/* Monitor for sync access to the global stack */
extern HB_CRITICAL_T hb_globalsMutex;
/* Monitor for sync access to the global stack */
extern HB_CRITICAL_T hb_staticsMutex;
/* Monitor for sync access to the global stack */
extern HB_CRITICAL_T hb_memvarsMutex;
/* Guard for threadunsafe malloc and free */
extern HB_CRITICAL_T hb_allocMutex;
/* Guard for console and output and free */
extern HB_CRITICAL_T hb_outputMutex;
/* Guard for memory allocated by the garbage collector */
extern HB_CRITICAL_T hb_garbageAllocMutex;
/* Guard for thread unsafe macro compilation */
extern HB_CRITICAL_T hb_macroMutex;
/* Guard for PRG level mutex asyncrhonous operations */
extern HB_CRITICAL_T hb_mutexMutex;

/* count of running stacks; set to -1 to block stacks from running */
extern HB_SHARED_RESOURCE hb_runningStacks;
#ifdef HB_OS_WIN_32
   /* Fence guard for windows */
   extern HB_CRITICAL_T hb_fenceMutex;
   extern HB_SHARED_RESOURCE hb_idleQueueRes;
#endif

/* regulates idle aware threads to be fenced or free */
extern BOOL hb_bIdleFence;

/***********************************************************************/
/* Function and globals definitions */
extern HB_STACK *last_stack;
extern HB_STACK *hb_ht_stack;
extern HB_MUTEX_STRUCT *hb_ht_mutex;
extern HB_THREAD_T hb_main_thread_id;

extern HB_STACK *hb_threadCreateStack( HB_THREAD_T th_id );
extern void hb_threadSetupStack( HB_STACK *tc, HB_THREAD_T th );
extern HB_STACK *hb_threadLinkStack( HB_STACK *tc );
extern HB_STACK *hb_threadUnlinkStack( HB_STACK *pStack );
extern void hb_threadDestroyStack( HB_STACK *pStack );
extern HB_STACK *hb_threadGetStack( HB_THREAD_T th_id );
extern void hb_threadInit( void );
extern void hb_threadExit( void );
extern void hb_threadCloseHandles( void );
extern int hb_threadCountStacks( void );
extern void hb_threadFillStack( HB_STACK *pStack, PHB_ITEM pArgs );
extern void hb_threadWaitAll( void );
extern void hb_threadKillAll( void );
extern void hb_threadSleep( int millisec );
extern void hb_mutexForceUnlock( void *);
extern void hb_rawMutexForceUnlock( void *);
extern HB_MUTEX_STRUCT *hb_threadLinkMutex( HB_MUTEX_STRUCT *mx );
extern HB_MUTEX_STRUCT *hb_threadUnlinkMutex( HB_MUTEX_STRUCT *mx );
extern void hb_threadTerminator( void *pData );

/* External functions used by thread as helper */
extern void hb_memvarsInit( HB_STACK * );
extern void hb_memvarsRelease( HB_STACK * );
extern void hb_memvarValueDecRefMT( HB_HANDLE hValue, HB_STACK *pStack );
extern void HB_EXPORT hb_itemClearMT( PHB_ITEM pItem, HB_STACK *pStack );

/* Used by dynsym thread specific system */
void hb_threadSetHMemvar( PHB_DYNS pDyn, HB_HANDLE hv );

/* Win 32 specific functions */
#ifdef HB_OS_WIN_32
   DWORD hb_SignalObjectAndWait( HB_MUTEX_T hToSignal, HB_COND_T hToWaitFor, DWORD dwMillisec, BOOL bUnused );
   void hb_threadSuspendAll( void );
   void hb_threadResumeAll( void );
   void hb_threadSubscribeIdle( HB_IDLE_FUNC );
   void hb_threadCallIdle( void );
   void hb_threadCancelInternal( void );

   BOOL hb_threadCondInit( HB_WINCOND_T *cond );
   void hb_threadCondDestroy( HB_WINCOND_T *cond );
   void hb_threadCondSignal( HB_WINCOND_T *cond );
   BOOL hb_threadCondWait( HB_WINCOND_T *cond, HANDLE mutex , DWORD dwTimeout );

#endif



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
   #define HB_STACK_LOCK
   #define HB_STACK_UNLOCK
   #define HB_CLEANUP_PUSH( x, y )
   #define HB_CLEANUP_POP
   #define HB_CLEANUP_POP_EXEC

   #define HB_THREAD_STUB
   #define HB_VM_STACK hb_stack
   #define HB_ENABLE_ASYN_CANC
   #define HB_DISABLE_ASYN_CANC
   #define HB_TEST_CANCEL_ENABLE_ASYN

#endif

#endif
