/*
 * $Id: thread.h,v 1.0 2002/10/25 03:06:09 ronpinkas Exp $
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
   #include <windows.h>
#endif

#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
    #include <pthread.h>
    #define HB_THREAD_T pthread_t
    #define HB_MUTEX_T pthread_mutex_t
    #define HB_MUTEX_INIT( x )      pthread_mutex_init( x, NULL )
    #define HB_MUTEX_DESTROY( x )   pthread_mutex_destroy( x )
    #define HB_MUTEX_LOCK( x )          pthread_mutex_lock( x )
    #define HB_MUTEX_UNLOCK( x )        pthread_mutex_unlock( x )
/* Thread support is only for linux and windows now */
#elif defined(HB_OS_WIN_32)
    #define HB_THREAD_T           DWORD
    #define HB_MUTEX_T            CRITICAL_SECTION
    #define HB_MUTEX_INIT( x )    InitializeCriticalSection( x )
    #define HB_MUTEX_DESTROY( x ) DeleteCriticalSection( x )
    #define HB_MUTEX_LOCK( x )    EnterCriticalSection( x )
    #define HB_MUTEX_UNLOCK( x )  LeaveCriticalSection( x )
#endif

typedef struct tag_HB_THREAD_CONTEXT {
    HB_THREAD_T th_id;
    HB_STACK *stack;
    struct tag_HB_THREAD_CONTEXT *next;
} HB_THREAD_CONTEXT;

/* Context */
typedef struct tag_HB_THREAD_PARAM {
    PHB_ITEM args;
    USHORT count;
} HB_THREAD_PARAM;

extern HB_STACK hb_stack_general;
extern HB_THREAD_CONTEXT *hb_ht_context;
extern HB_MUTEX_T context_monitor;

extern void hb_createContext( void );
extern void hb_destroyContext( void );
extern void hb_contextInit( void );
extern HB_THREAD_CONTEXT *hb_getCurrentContext( void );
#endif
