/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Fixed Memory API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_xmemcpy()
 *    hb_xmemset()
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_xquery()
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 *    Threadsafing of MT startup and closing sequences
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: This definitions must be ahead of any and all #include statements */

/* For MS-Win builds */
#define HB_OS_WIN_USED

#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbdate.h"
#include "hbver.h"
#include "hbapifs.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbmemory.ch"
#include "hbinit.h"

#if ( defined( __BORLANDC__ ) && ( __BORLANDC__ == 0x620 ) )
/* Disabled: Borland C 6.2 internal error when compiling dlmalloc.c */
   #if defined( HB_FM_DL_ALLOC )
      #undef HB_FM_DL_ALLOC
   #endif
#endif

#if defined( HB_FM_STATISTICS )
/* Disable external memory manager when debugging */
   #undef HB_FM_DL_ALLOC
#endif

#if defined( HB_FM_DL_ALLOC )
   #undef HB_FM_STD_ALLOC
   #undef HB_FM_WIN32_ALLOC
#elif defined( __EXPORT__ ) && ! defined( HB_FM_WIN32_ALLOC )
   #define HB_FM_WIN32_ALLOC
   #undef HB_FM_DL_ALLOC
   #undef HB_FM_STD_ALLOC
#elif defined( HB_FM_WIN32_ALLOC )
   #undef HB_FM_DL_ALLOC
   #undef HB_FM_STD_ALLOC
#elif defined( HB_FM_STD_ALLOC )
   #undef HB_FM_DL_ALLOC
   #undef HB_FM_WIN32_ALLOC
#else
   #define HB_FM_STD_ALLOC
   #undef HB_FM_DL_ALLOC
   #undef HB_FM_WIN32_ALLOC
#endif

#if defined( HB_FM_DL_ALLOC )
/* #  define NO_MALLINFO 1 */
/* #  define INSECURE */
/* #  define USE_DL_PREFIX */
#  if defined( HB_DEBUG )
#     if ! defined( DEBUG )
#        define DEBUG     1
#     endif
#  endif
#  undef FORCEINLINE
#  if ! defined( FORCEINLINE )
#     define FORCEINLINE   HB_FORCEINLINE
#  endif
#  define REALLOC_ZERO_BYTES_FREES
#  if defined( HB_THREAD_SUPPORT )
#     define USE_LOCKS     1
#  else
#     define USE_LOCKS     0
#  endif
#  if defined( __BORLANDC__ )
#     pragma warn -8019
#     pragma warn -8027
#     pragma warn -8084
#     pragma warn -8041
#     pragma warn -8008
#     pragma warn -8004
#     pragma warn -8066
#  elif defined( _MSC_VER ) || defined( __DMC__ ) || defined( __WATCOMC__ )
#     if (( defined( __DMC__ ) || defined( _MSC_VER ) ) && ! defined( __POCC__ ) )
         HB_EXTERN_BEGIN
         WINBASEAPI
         BOOL
         WINAPI
         InitializeCriticalSectionAndSpinCount(
            LPCRITICAL_SECTION lpCriticalSection,
            DWORD dwSpinCount
            );
         HB_EXTERN_END
#     endif
#     if defined( __WATCOMC__ )
#        pragma disable_message ( 201 )
#        pragma disable_message ( 302 )
#     endif
#     define USE_DL_PREFIX
#  endif
#  include "errno.h"
#  if defined( __POCC__ )
#     pragma warn(push)
#     pragma warn(disable:2154)
#     pragma warn(disable:2243)
#  endif
#  include "dlmalloc.c"
#  if defined( __WATCOMC__ )
#     pragma enable_message ( 201 )
#  endif
#  if defined( __POCC__ )
#     pragma warn(pop)
#  endif
#  if defined( __BORLANDC__ )
#     pragma warn +8019
#     pragma warn +8027
#     pragma warn +8084
#     pragma warn +8041
#     pragma warn +8008
#     pragma warn +8004
#     pragma warn +8066
#  endif
#  if defined( USE_DL_PREFIX )
#     define malloc( n )      dlmalloc( ( n ) )
#     define realloc( p, n )  dlrealloc( ( p ), ( n ) )
#     define free( p )        dlfree( ( p ) )
#  endif
#elif defined( HB_FM_WIN32_ALLOC ) && defined( HB_OS_WIN )
#  if defined( HB_FM_LOCALALLOC )
#     define malloc( n )      ( void * ) LocalAlloc( LMEM_FIXED, ( n ) )
#     define realloc( p, n )  ( void * ) LocalReAlloc( ( HLOCAL ) ( p ), ( n ), LMEM_MOVEABLE )
#     define free( p )        LocalFree( ( HLOCAL ) ( p ) )
#  else
static HANDLE hProcessHeap = 0;
#     define malloc( n )      ( assert( hProcessHeap ), ( void * ) HeapAlloc( hProcessHeap, 0, ( n ) ) )
#     define realloc( p, n )  ( void * ) HeapReAlloc( hProcessHeap, 0, ( void * ) ( p ), ( n ) )
#     define free( p )        HeapFree( hProcessHeap, 0, ( void * ) ( p ) )
#  endif
#endif

#ifndef HB_FM_STATISTICS
#  undef HB_PARANOID_MEM_CHECK
#endif

#if defined( HB_FM_STATISTICS ) && ! defined( HB_TR_LEVEL )
#  define HB_TR_LEVEL         HB_TR_ERROR
#endif

#ifdef HB_FM_STATISTICS

#ifndef HB_MEMFILER
#  define HB_MEMFILER         0xff
#endif
#define HB_MEMINFO_SIGNATURE  0x19730403

typedef struct _HB_MEMINFO
{
   UINT32 ulSignature;
   HB_SIZE ulSize;
   USHORT uiProcLine;
   USHORT uiAutoRelease;
   char szProcName[ HB_SYMBOL_NAME_LEN + 1 ];
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
} HB_MEMINFO, * PHB_MEMINFO;

#ifdef HB_ALLOC_ALIGNMENT
#  define HB_MEMINFO_SIZE  ( ( ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) - \
                               ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) % HB_ALLOC_ALIGNMENT ) + \
                             HB_COUNTER_OFFSET )
#else
#  define HB_MEMINFO_SIZE  ( sizeof( HB_MEMINFO ) + HB_COUNTER_OFFSET )
#endif

#define HB_FM_GETSIG( p, n )  HB_GET_UINT32( ( BYTE * ) ( p ) + ( n ) )
#define HB_FM_SETSIG( p, n )  HB_PUT_UINT32( ( BYTE * ) ( p ) + ( n ), HB_MEMINFO_SIGNATURE )
#define HB_FM_CLRSIG( p, n )  HB_PUT_UINT32( ( BYTE * ) ( p ) + ( n ), 0 )

#define HB_ALLOC_SIZE( n )    ( size_t ) ( ( n ) + HB_MEMINFO_SIZE + sizeof( UINT32 ) )

#define HB_FM_PTR( p )        ( ( PHB_MEMINFO ) ( ( BYTE * ) ( p ) - HB_MEMINFO_SIZE ) )


/* NOTE: we cannot use here HB_TRACE because it will overwrite the
 * function name/line number of code which called hb_xalloc/hb_xgrab
 */
#define HB_TRACE_FM HB_TRACE_STEALTH

static HB_ISIZ       s_lMemoryBlocks      = 0;  /* memory blocks used */
static HB_ISIZ       s_lMemoryMaxBlocks   = 0;  /* maximum number of used memory blocks */
static HB_ISIZ       s_lMemoryMaxConsumed = 0;  /* memory max size consumed */
static HB_ISIZ       s_lMemoryConsumed    = 0;  /* memory size consumed */
static HB_ISIZ       s_lAllocations       = 0;
static HB_ISIZ       s_lReAllocations     = 0;
static HB_ISIZ       s_lFreed             = 0;

static PHB_MEMINFO   s_pFirstBlock        = NULL;
static PHB_MEMINFO   s_pLastBlock         = NULL;

#else /* ! HB_FM_STATISTICS */

typedef void * PHB_MEMINFO;
#define HB_MEMINFO_SIZE HB_COUNTER_OFFSET
#define HB_ALLOC_SIZE( n )       ( size_t ) ( ( n ) + HB_MEMINFO_SIZE )
#define HB_FM_PTR( p )           HB_COUNTER_PTR( p )
#define HB_TRACE_FM     HB_TRACE

#endif /* HB_FM_STATISTICS */

#define HB_MEM_PTR( p )          ( ( void * ) ( ( BYTE * ) ( p ) + HB_MEMINFO_SIZE ) )


#define HB_ATOMIC_INCP( p )      HB_ATOMIC_INC( *( p ) )
#define HB_ATOMIC_DECP( p )      HB_ATOMIC_DEC( *( p ) )

#ifndef HB_ATOMIC_GET
#  define HB_ATOMIC_GET( p )     ( *( p ) )
#endif
#ifndef HB_ATOMIC_SET
#  define HB_ATOMIC_SET( p, n )  ( ( *( p ) ) = ( n ) )
#endif

#if defined( HB_THREAD_SUPPORT ) && \
   ( ! defined( HB_SAFE_ALLOC ) || defined( HB_FM_STATISTICS ) )

#  define HB_MEM_THLOCK()        do { \
      if( hb_stack_ready ) \
      { \
         HB_CRITICAL_LOCK( hb_allocMutex ); \
      } \
} while( 0 )

#  define HB_MEM_THUNLOCK()      do { \
      if( hb_stack_ready ) \
      { \
         HB_CRITICAL_UNLOCK( hb_allocMutex ); \
      } \
} while( 0 )

#else
#  define HB_MEM_THLOCK()
#  define HB_MEM_THUNLOCK()
#endif

/* allocates fixed memory, do *not* exits on failure */
#ifdef hb_xalloc
   #undef hb_xalloc
void * hb_xalloc( HB_SIZE ulSize )
{
   return malloc( ulSize );
}
#else
void * hb_xalloc( HB_SIZE ulSize )         /* allocates fixed memory, returns NULL on failure */
{
   PHB_MEMINFO pMem;

   HB_TRACE_FM( HB_TR_DEBUG, ( "hb_xalloc(%" HB_PFS "u)", ulSize ) );

   if( ulSize == 0 )
      hb_errInternal( HB_EI_XALLOCNULLSIZE, NULL, NULL, NULL );

   pMem = ( PHB_MEMINFO ) malloc( HB_ALLOC_SIZE( ulSize ) );

   if( ! pMem )
      return pMem;

#ifdef HB_FM_STATISTICS

   HB_MEM_THLOCK();

   s_lAllocations++;

   if( ! s_pFirstBlock )
   {
      pMem->pPrevBlock  = NULL;
      s_pFirstBlock     = pMem;
   }
   else
   {
      pMem->pPrevBlock           = s_pLastBlock;
      s_pLastBlock->pNextBlock   = pMem;
   }
   s_pLastBlock         = pMem;

   pMem->pNextBlock     = NULL;
   pMem->ulSignature    = HB_MEMINFO_SIGNATURE;
   HB_FM_SETSIG( HB_MEM_PTR( pMem ), ulSize );
   pMem->ulSize         = ulSize; /* size of the memory block */
   pMem->uiAutoRelease  = 0;

   if( hb_tr_level() >= HB_TR_DEBUG )
   {
      /* NOTE: PRG line number/procname is not very useful during hunting
       * for memory leaks - this is why we are using the previously stored
       * function/line info - this is a location of code that called
       * hb_xalloc/hb_xgrab
       */
      pMem->uiProcLine = ( USHORT ) hb_tr_line_; /* C line number */
      hb_xstrcpy( pMem->szProcName, hb_tr_file_, 0 );
   }
   else
   {
      HB_THREAD_STUB

      if( hb_stack_ready && HB_VM_STACK.pBase != HB_VM_STACK.pItems )
      {
         /* PRG line number */
         pMem->uiProcLine = ( *( HB_VM_STACK.pBase ) )->item.asSymbol.pCargo->lineno;
         /* PRG ProcName */
         hb_xstrcpy( pMem->szProcName, ( *( HB_VM_STACK.pBase ) )->item.asSymbol.value->szName, 0 );
      }
      else
      {
         /* PRG line number */
         pMem->uiProcLine        = 0;
         /* PRG ProcName */
         pMem->szProcName[ 0 ]   = '\0';
      }
   }

   s_lMemoryConsumed += ulSize + sizeof( HB_COUNTER );
   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   s_lMemoryBlocks++;
   if( s_lMemoryMaxBlocks < s_lMemoryBlocks )
      s_lMemoryMaxBlocks = s_lMemoryBlocks;

   HB_MEM_THUNLOCK();

#ifdef HB_PARANOID_MEM_CHECK
   memset( HB_MEM_PTR( pMem ), HB_MEMFILER, ulSize );
#endif

#endif

   HB_ATOMIC_SET( HB_COUNTER_PTR( HB_MEM_PTR( pMem ) ), 1 );

   return HB_MEM_PTR( pMem );
}
#endif

#ifdef hb_xgrab
#undef hb_xgrab
void * hb_xgrab( HB_SIZE ulSize )
{
   return malloc( ulSize );
}
#else
void * hb_xgrab( HB_SIZE ulSize )         /* allocates fixed memory, exits on failure */
{
   PHB_MEMINFO pMem;

   HB_TRACE_FM( HB_TR_DEBUG, ( "hb_xgrab(%" HB_PFS "u)", ulSize ) );

   if( ulSize == 0 )
      hb_errInternal( HB_EI_XGRABNULLSIZE, NULL, NULL, NULL );

   pMem = ( PHB_MEMINFO ) malloc( HB_ALLOC_SIZE( ulSize ) );

   if( ! pMem )
      hb_errInternal( HB_EI_XGRABALLOC, NULL, NULL, NULL );

#ifdef HB_FM_STATISTICS

   HB_MEM_THLOCK();

   /* allocation should be counted AFTER we know that malloc has suceed */
   s_lAllocations++;

   if( ! s_pFirstBlock )
   {
      pMem->pPrevBlock  = NULL;
      s_pFirstBlock     = pMem;
   }
   else
   {
      pMem->pPrevBlock           = s_pLastBlock;
      s_pLastBlock->pNextBlock   = pMem;
   }
   s_pLastBlock         = pMem;

   pMem->pNextBlock     = NULL;
   pMem->ulSignature    = HB_MEMINFO_SIGNATURE;
   HB_FM_SETSIG( HB_MEM_PTR( pMem ), ulSize );
   pMem->ulSize         = ulSize; /* size of the memory block */
   pMem->uiAutoRelease  = 0;

   if( hb_tr_level() >= HB_TR_DEBUG )
   {
      /* NOTE: PRG line number/procname is not very useful during hunting
       * for memory leaks - this is why we are using the previously stored
       * function/line info - this is a location of code that called
       * hb_xalloc/hb_xgrab
       */
      pMem->uiProcLine = ( USHORT ) hb_tr_line_; /* C line number */
      hb_xstrcpy( pMem->szProcName, hb_tr_file_, 0 );
   }
   else
   {
      HB_THREAD_STUB

      if( hb_stack_ready && HB_VM_STACK.pBase != HB_VM_STACK.pItems )
      {
         /* PRG line number */
         pMem->uiProcLine = ( *( HB_VM_STACK.pBase ) )->item.asSymbol.pCargo->lineno;
         /* PRG ProcName */
         hb_xstrcpy( pMem->szProcName, ( *( HB_VM_STACK.pBase ) )->item.asSymbol.value->szName, 0 );
      }
      else
      {
         /* PRG line number */
         pMem->uiProcLine        = 0;
         /* PRG ProcName */
         pMem->szProcName[ 0 ]   = '\0';
      }
   }

   s_lMemoryConsumed += ulSize + sizeof( HB_COUNTER );
   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   s_lMemoryBlocks++;
   if( s_lMemoryMaxBlocks < s_lMemoryBlocks )
      s_lMemoryMaxBlocks = s_lMemoryBlocks;

   HB_MEM_THUNLOCK();

#ifdef HB_PARANOID_MEM_CHECK
   memset( HB_MEM_PTR( pMem ), HB_MEMFILER, ulSize );
#endif

#endif

   HB_ATOMIC_SET( HB_COUNTER_PTR( HB_MEM_PTR( pMem ) ), 1 );

   return HB_MEM_PTR( pMem );
}
#endif

#ifdef hb_xrealloc
#undef hb_xrealloc
void * hb_xrealloc( void * pMem, HB_SIZE ulSize )
{
   return realloc( pMem, ulSize );
}
#else
void * hb_xrealloc( void * pMem, HB_SIZE ulSize )       /* reallocates memory */
{
   HB_TRACE_FM( HB_TR_DEBUG, ( "hb_xrealloc(%p, %" HB_PFS "u)", pMem, ulSize ) );

#if 0
   /* disabled to make hb_xrealloc() ANSI-C realloc() compatible */
   if( ! pMem )
      hb_errInternal( HB_EI_XREALLOCNULL, NULL, NULL, NULL );

   if( ulSize == 0 )
      hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
#endif

#ifdef HB_FM_STATISTICS
   if( pMem == NULL )
   {
      if( ulSize == 0 )
         hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
      return hb_xgrab( ulSize );
   }
   else if( ulSize == 0 )
   {
      hb_xfree( pMem );
      return NULL;
   }
   else
   {
      PHB_MEMINFO pMemBlock;
      HB_SIZE     ulMemSize;

      pMemBlock = HB_FM_PTR( pMem );

      if( pMemBlock->ulSignature != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XREALLOCINV, NULL, NULL, NULL );

      ulMemSize = pMemBlock->ulSize;

      if( HB_FM_GETSIG( pMem, ulMemSize ) != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XMEMOVERFLOW, NULL, NULL, NULL );

      s_lReAllocations++;

      HB_FM_CLRSIG( pMem, ulMemSize );

      HB_MEM_THLOCK();

#ifdef HB_PARANOID_MEM_CHECK
      pMem = malloc( HB_ALLOC_SIZE( ulSize ) );
      if( pMem )
      {
         if( ulSize > ulMemSize )
         {
            HB_MEMCPY( pMem, pMemBlock, HB_ALLOC_SIZE( ulMemSize ) );
            memset( ( BYTE * ) pMem + HB_ALLOC_SIZE( ulMemSize ), HB_MEMFILER, ulSize - ulMemSize );
         }
         else
            HB_MEMCPY( pMem, pMemBlock, HB_ALLOC_SIZE( ulSize ) );
      }
      memset( pMemBlock, HB_MEMFILER, HB_ALLOC_SIZE( ulMemSize ) );
      free( pMemBlock );
#else
      pMem              = realloc( pMemBlock, HB_ALLOC_SIZE( ulSize ) );
#endif

      s_lMemoryConsumed += ( ulSize - ulMemSize );
      if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
         s_lMemoryMaxConsumed = s_lMemoryConsumed;

      if( pMem )
      {
         ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;  /* size of the memory block */
         HB_FM_SETSIG( HB_MEM_PTR( pMem ), ulSize );
         if( ( ( PHB_MEMINFO ) pMem )->pPrevBlock )
            ( ( PHB_MEMINFO ) pMem )->pPrevBlock->pNextBlock = ( PHB_MEMINFO ) pMem;
         if( ( ( PHB_MEMINFO ) pMem )->pNextBlock )
            ( ( PHB_MEMINFO ) pMem )->pNextBlock->pPrevBlock = ( PHB_MEMINFO ) pMem;

         if( s_pFirstBlock == pMemBlock )
            s_pFirstBlock = ( PHB_MEMINFO ) pMem;
         if( s_pLastBlock == pMemBlock )
            s_pLastBlock = ( PHB_MEMINFO ) pMem;
      }

      HB_MEM_THUNLOCK();

      if( ! pMem )
         hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );
   }
#else

   if( pMem == NULL )
   {
      if( ulSize == 0 )
         hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
      pMem = malloc( HB_ALLOC_SIZE( ulSize ) );
   }
   else if( ulSize == 0 )
   {
      free( ( void * ) HB_FM_PTR( pMem ) );
      return NULL;
   }
   else
   {
      HB_MEM_THLOCK();
      pMem = realloc( ( void * ) HB_FM_PTR( pMem ), HB_ALLOC_SIZE( ulSize ) );
      HB_MEM_THUNLOCK();
   }

   if( ! pMem )
      hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );

#endif

   return HB_MEM_PTR( pMem );
}
#endif

#ifdef hb_xfree
#undef hb_xfree
void hb_xfree( void * pMem )
{
   free( pMem );
}
#else
void hb_xfree( void * pMem )            /* frees fixed memory */
{
   HB_TRACE_FM( HB_TR_DEBUG, ( "hb_xfree(%p)", pMem ) );

   if( pMem )
   {
#ifdef HB_FM_STATISTICS

      PHB_MEMINFO pMemBlock = HB_FM_PTR( pMem );

      s_lFreed++;

      if( pMemBlock->ulSignature != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XFREEINV, "hb_xfree() Invalid Pointer %p %s", ( char * ) pMem, ( char * ) pMem );

      if( HB_FM_GETSIG( pMem, pMemBlock->ulSize ) != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XMEMOVERFLOW, "hb_xfree(%p) Pointer Overflow '%s'", ( char * ) pMem, ( char * ) pMem );

      HB_MEM_THLOCK();

      s_lMemoryConsumed -= pMemBlock->ulSize + sizeof( HB_COUNTER );
      s_lMemoryBlocks--;

      if( pMemBlock->pPrevBlock )
         pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;
      else
         s_pFirstBlock = pMemBlock->pNextBlock;

      if( pMemBlock->pNextBlock )
         pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;
      else
         s_pLastBlock = pMemBlock->pPrevBlock;

      HB_MEM_THUNLOCK();

      pMemBlock->ulSignature = 0;
      HB_FM_CLRSIG( pMem, pMemBlock->ulSize );

#ifdef HB_PARANOID_MEM_CHECK
      memset( pMemBlock, HB_MEMFILER, HB_ALLOC_SIZE( pMemBlock->ulSize ) );
#endif

      free( ( void * ) pMemBlock );

#else

      HB_MEM_THLOCK();

      free(  HB_FM_PTR( pMem ) );

      HB_MEM_THUNLOCK();

#endif
   }
   else
      hb_errInternal( HB_EI_XFREENULL, NULL, NULL, NULL );
}
#endif

/* increment reference counter */
#undef hb_xRefInc
void hb_xRefInc( void * pMem )
{
   HB_ATOMIC_INCP( HB_COUNTER_PTR( pMem ) );
}

/* decrement reference counter, return TRUE when 0 reached */
#undef hb_xRefDec
BOOL hb_xRefDec( void * pMem )
{
   return HB_ATOMIC_DECP( HB_COUNTER_PTR( pMem ) ) == 0;
}

/* decrement reference counter and free the block when 0 reached */
#undef hb_xRefFree
void hb_xRefFree( void * pMem )
{
#ifdef HB_FM_STATISTICS

   if( HB_FM_PTR( pMem )->ulSignature != HB_MEMINFO_SIGNATURE )
      hb_errInternal( HB_EI_XFREEINV, NULL, NULL, NULL );

   if( HB_ATOMIC_DECP( HB_COUNTER_PTR( pMem ) ) == 0 )
      hb_xfree( pMem );

#else

   if( HB_ATOMIC_DECP( HB_COUNTER_PTR( pMem ) ) == 0 )
      free( ( void * ) HB_FM_PTR( pMem ) );

#endif
}

/* return number of references */
#undef hb_xRefCount
HB_COUNTER hb_xRefCount( void * pMem )
{
   return HB_ATOMIC_GET( HB_COUNTER_PTR( pMem ) );
}

/* reallocates memory, create copy if reference counter greater then 1 */
#undef hb_xRefResize
void * hb_xRefResize( void * pMem, HB_SIZE ulSave, HB_SIZE ulSize )
{

#ifdef HB_FM_STATISTICS
   if( HB_ATOMIC_GET( HB_COUNTER_PTR( pMem ) ) > 1 )
   {
      void * pMemNew = HB_MEMCPY( hb_xgrab( ulSize ), pMem, ( size_t ) HB_MIN( ulSave, ulSize ) );

      if( HB_ATOMIC_DECP( HB_COUNTER_PTR( pMem ) ) == 0 )
         hb_xfree( pMem );

      return pMemNew;
   }

   return hb_xrealloc( pMem, ulSize );

#else

   if( HB_ATOMIC_GET( HB_COUNTER_PTR( pMem ) ) > 1 )
   {
      void * pMemNew = malloc( HB_ALLOC_SIZE( ulSize ) );

      if( pMemNew )
      {
         HB_ATOMIC_SET( HB_COUNTER_PTR( HB_MEM_PTR( pMemNew ) ), 1 );
         HB_MEMCPY( HB_MEM_PTR( pMemNew ), pMem, ( size_t ) HB_MIN( ulSave, ulSize ) );
         if( HB_ATOMIC_DECP( HB_COUNTER_PTR( pMem ) ) == 0 )
            free( ( void * ) HB_FM_PTR( pMem ) );
         return HB_MEM_PTR( pMemNew );
      }
   }
   else
   {
      pMem = realloc( ( void * ) HB_FM_PTR( pMem ), HB_ALLOC_SIZE( ulSize ) );
      if( pMem )
         return HB_MEM_PTR( pMem );
      else
         hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );
   }
   #if 0
   hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );
   return NULL;
   #else
   return NULL;
   #endif
#endif
}

#undef hb_xautorelease
void hb_xautorelease( void * pMem )            /* set memory to autorelease */
{
#ifdef HB_FM_STATISTICS

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_xautorelease(%p)", pMem ) );

   if( pMem )
   {
      PHB_MEMINFO pMemBlock = HB_FM_PTR( pMem ) ;;//( PHB_MEMINFO ) ( ( char * ) pMem - HB_MEMINFO_SIZE );

      if( pMemBlock->ulSignature != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XFREEINV, "hb_xautorelease() Invalid Pointer %p %s", ( char * ) pMem, ( char * ) pMem );

      if( HB_FM_GETSIG( pMem ,  pMemBlock->ulSize ) != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XMEMOVERFLOW, "hb_xautorelease(%p) Pointer Overflow '%s'", ( char * ) pMem, ( char * ) pMem );

      pMemBlock->uiAutoRelease = 1;
   }
#else
   HB_SYMBOL_UNUSED( pMem );
#endif
}

/* NOTE: Debug function, it will always return 0 when HB_FM_STATISTICS is
         not defined, don't use it for final code */

HB_SIZE hb_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xsize(%p)", pMem ) );

#ifdef HB_FM_STATISTICS
   return HB_FM_PTR( pMem )->ulSize;
#else
   HB_SYMBOL_UNUSED( pMem );

   return 0;
#endif
}

void hb_xinit( void ) /* Initialize fixed memory subsystem */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xinit()" ) );

#if defined( HB_FM_WIN32_ALLOC ) && defined( HB_OS_WIN ) && ! defined( HB_FM_LOCALALLOC )
   hProcessHeap = GetProcessHeap();
#endif

   return;
}

/* Returns pointer to string containing printable version
   of pMem memory block */

#ifdef HB_FM_STATISTICS
static char * hb_mem2str( char * membuffer, void * pMem, HB_SIZE uiSize )
{
   BYTE *   cMem = ( BYTE * ) pMem;
   HB_SIZE     uiIndex, uiPrintable;

   uiPrintable = 0;
   for( uiIndex = 0; uiIndex < uiSize; uiIndex++ )
      if( ( cMem[ uiIndex ] & 0x60 ) != 0 )
         uiPrintable++;

   if( uiPrintable * 100 / uiSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( uiIndex = 0; uiIndex < uiSize; uiIndex++ )
         if( cMem[ uiIndex ] >= ' ' )
            membuffer[ uiIndex ] = cMem[ uiIndex ];
         else
            membuffer[ uiIndex ] = '.';
      membuffer[ uiIndex ] = '\0';
   }
   else
   {
      /* format as hex */
      for( uiIndex = 0; uiIndex < uiSize; uiIndex++ )
      {
         int lownibble, hinibble;
         hinibble                      = cMem[ uiIndex ] >> 4;
         lownibble                     = cMem[ uiIndex ] & 0x0F;
         membuffer[ uiIndex * 2 ]      = hinibble <= 9 ?
                               ( '0' + hinibble ) : ( 'A' + hinibble - 10 );
         membuffer[ uiIndex * 2 + 1 ]  = lownibble <= 9 ?
                               ( '0' + lownibble ) : ( 'A' + lownibble - 10 );
      }
      membuffer[ uiIndex * 2 ] = '\0';
   }

   return membuffer;
}

#define HB_MAX_MEM2STR_BLOCK 256
void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xexit()" ) );

/* JC1: The problem with threads here is that the stack has already been
 * destroyed, but hb_conOut and other functions may allocate memory with xgrab, using the
 * stack... Notice that this can possibly leads to problem also in ST
 * apps in the future. We must de-tangle the initialization and closing
 * sequence
 */
   {
      register PHB_MEMINFO pMemBlock = s_pFirstBlock;
      register PHB_MEMINFO pMemTemp;

      while( pMemBlock )
      {
         pMemTemp    = pMemBlock;
         pMemBlock   = pMemBlock->pNextBlock;

         if( pMemTemp->uiAutoRelease )
            hb_xfree( ( void * ) ( ( char * ) pMemTemp + HB_MEMINFO_SIZE ) );
      }
   }

   if( s_lMemoryBlocks || hb_cmdargCheck( "INFO" ) )
   {
      char        membuffer[ HB_MAX_MEM2STR_BLOCK * 2 + 1 ]; /* multiplied by 2 to allow hex format */
      PHB_MEMINFO pMemBlock;
      USHORT      ui;
      char        buffer[ 100 ];
      FILE *      hLog = NULL;

      if( s_lMemoryBlocks )
         hLog = hb_fopen( "fm.log", "a+" );

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( buffer, sizeof( buffer ), "Total %" HB_PFS "i allocations (%" HB_PFS "i reallocation), of which %" HB_PFS "i freed.", s_lAllocations, s_lReAllocations, s_lFreed );

      if( hLog )
      {
         char *   szPlatform  = hb_verPlatform();
         char *   szCompiler  = hb_verCompiler();
         char *   szHarbour   = hb_verHarbour();
         char *   szTime      = ( char * ) hb_xgrab( 9 );
         int      iYear, iMonth, iDay;

         hb_dateToday( &iYear, &iMonth, &iDay );
         hb_dateTimeStr( szTime );

         fprintf( hLog, "Memory Allocation Report\n" );
         fprintf( hLog, "Application: %s\n", hb_cmdargARGV()[ 0 ] );
         fprintf( hLog, "xHarbour Version: %s\n", szHarbour );
         fprintf( hLog, "Compiler: %s\n", szCompiler );
         fprintf( hLog, "Platform: %s\n", szPlatform );
         fprintf( hLog, "Time Occured: %04d.%02d.%02d %s\n", iYear, iMonth, iDay, szTime );
         fprintf( hLog, "%s\n", buffer );

         hb_xfree( szPlatform );
         hb_xfree( szCompiler );
         hb_xfree( szHarbour  );
         hb_xfree( szTime );
      }

      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( buffer, sizeof( buffer ), "Highest total allocated %" HB_PFS "i bytes in %" HB_PFS "i blocks.", s_lMemoryMaxConsumed, s_lMemoryMaxBlocks );

      if( hLog )
         fprintf( hLog, "%s\n", buffer );

      hb_conOutErr( buffer, 0 );

      if( s_lMemoryBlocks )
      {
         hb_conOutErr( hb_conNewLine(), 0 );
         hb_snprintf( buffer, sizeof( buffer ), "WARNING! Memory allocated but not released: %" HB_PFS "i bytes (%" HB_PFS "i blocks)", s_lMemoryConsumed, s_lMemoryBlocks );

         if( hLog )
            fprintf( hLog, "%s\n", buffer );

         hb_conOutErr( buffer, 0 );
      }

      hb_conOutErr( hb_conNewLine(), 0 );

      for( ui = 1, pMemBlock = s_pFirstBlock; pMemBlock; pMemBlock = pMemBlock->pNextBlock, ++ui )
      {
         HB_TRACE( HB_TR_ERROR, ( "Block %i %p (size %" HB_PFS "u) %s(%i), \"%s\"", ui,
                                  ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                                  pMemBlock->ulSize, pMemBlock->szProcName, pMemBlock->uiProcLine,
                                  hb_mem2str( membuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                                              HB_MIN(  pMemBlock->ulSize, HB_MAX_MEM2STR_BLOCK ) ) ) );

         if( hLog )
            fprintf( hLog, "Block %i %p (size %" HB_PFS "u) %s(%i), \"%s\"\n", ui - 1,
                     ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                     pMemBlock->ulSize, pMemBlock->szProcName, pMemBlock->uiProcLine,
                     hb_mem2str( membuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                                 HB_MIN(  pMemBlock->ulSize, HB_MAX_MEM2STR_BLOCK ) ) );
      }

      if( hLog )
      {
         fprintf( hLog, "--------------------------------------------------------------------------------\n" );
         fclose( hLog );
      }
   }
#ifdef __WIN32__
   else
   {
      OutputDebugString( "HB_XEXIT(): No Memory Leak Detected." );
#if defined( HB_FM_STD_ALLOC ) || ( defined( HB_FM_WIN32_ALLOC ) && ! defined( HB_OS_WIN ) )
      OutputDebugString( "using HB_FM_STD_ALLOC." );
#elif defined( HB_FM_WIN32_ALLOC ) && defined( HB_OS_WIN ) && defined( HB_FM_LOCALALLOC )
      OutputDebugString( "using HB_FM_WIN32_LOCALALLOC." );
#elif defined( HB_FM_WIN32_ALLOC ) && defined( HB_OS_WIN )
      OutputDebugString( "using HB_FM_WIN32_HEAPALLOC." );
#elif defined( HB_FM_DL_ALLOC )
      OutputDebugString( "using HB_FM_DL_ALLOC." );
#endif
#ifdef HB_FM_STATISTICS
      OutputDebugString( "with HB_FM_STATISTICS activated." );
#endif
   }
#endif
}

#else

void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xexit()" ) );
}

#endif

/* hb_xmemcpy and hb_xmemset are only needed when
   unsigned int and unsigned long differ in length */

/* unfortunately it's not true - on 64bit platforms int is 32 bit
   and long is 64.
   we need these functions only when max(size_t) < max(long)
   what could be detected and set in header files. Here check
   only for hb_xmem* macro definition

   #if UINT_MAX != ULONG_MAX
 */
#ifndef hb_xmemcpy
void * hb_xmemcpy( void * pDestArg, void * pSourceArg, HB_SIZE ulLen )
{
   BYTE *   pDest;
   BYTE *   pSource;
   HB_SIZE    ulRemaining;
   int      iCopySize;

   assert( pDestArg != NULL && pSourceArg != NULL );
   assert( ulLen > 0 );
   /* blocks overlapped? use memmove. */
   assert( pDestArg >= pSourceArg + ulLen || pSourceArg >= pDestArg + ulLen );

   HB_TRACE( HB_TR_DEBUG, ( "hb_xmemcpy(%p, %p, %" HB_PFS "u)", pDestArg, pSourceArg, ulLen ) );

   pDest       = ( BYTE * ) pDestArg;
   pSource     = ( BYTE * ) pSourceArg;
   ulRemaining = ulLen;

   while( ulRemaining )
   {
      /* Overcome the HB_MEMCPY() size_t limitation */
      if( ulRemaining > UINT_MAX )
      {
         iCopySize   = UINT_MAX;
         ulRemaining -= ( HB_SIZE ) iCopySize;
      }
      else
      {
         iCopySize   = ( int ) ulRemaining;
         ulRemaining = 0;
      }
      HB_MEMCPY( pDest, pSource, iCopySize );
      pDest    += iCopySize;
      pSource  += iCopySize;
   }

   return pDestArg;
}
#endif

#ifndef hb_xmemset
void * hb_xmemset( void * pDestArg, int iFill, HB_SIZE ulLen )
{
   BYTE *   pDest;
   HB_SIZE    ulRemaining;
   int      iSetSize;

   assert( pDestArg != NULL );
   assert( ulLen > 0 );

   HB_TRACE( HB_TR_DEBUG, ( "hb_xmemset(%p, %d, %" HB_PFS "u)", pDestArg, iFill, ulLen ) );

   pDest       = ( BYTE * ) pDestArg;
   ulRemaining = ulLen;

   while( ulRemaining )
   {
      /* Overcome the memset() size_t limitation */
      if( ulRemaining > UINT_MAX )
      {
         iSetSize    = UINT_MAX;
         ulRemaining -= ( HB_SIZE ) iSetSize;
      }
      else
      {
         iSetSize    = ( int ) ulRemaining;
         ulRemaining = 0;
      }
      memset( pDest, iFill, iSetSize );
      pDest += iSetSize;
   }

   return pDestArg;
}
#endif

HB_SIZE hb_xquery( USHORT uiMode )
{
   HB_SIZE ulResult;

   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_xquery(%hu)", uiMode ) );

   /* TODO: Return the correct values instead of 9999 */

   switch( uiMode )
   {
      case HB_MEM_CHAR:    /*               (Free Variable Space [KB])          */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            ulResult = memorystatus.dwAvailPhys / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               ulResult = 0;
            else
               ulResult = ulSysInfo / 1024;
         }
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_BLOCK:   /*           (Largest String [KB])               */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            ulResult = HB_MIN( memorystatus.dwAvailPhys, ULONG_MAX ) / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               ulResult = 0;
            else
               ulResult = HB_MIN( ulSysInfo, ULONG_MAX ) / 1024;
         }
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_RUN:     /*              (RUN Memory [KB])                  */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            ulResult = memorystatus.dwAvailPhys / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               ulResult = 0;
            else
               ulResult = ulSysInfo / 1024;
         }
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_VM:      /* UNDOCUMENTED! (Virtual Memory [KB])            */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            ulResult = memorystatus.dwAvailVirtual / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               ulResult = 0;
            else
               ulResult = ulSysInfo / 1024;
         }
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_EMS:     /* UNDOCUMENTED! (Free Expanded Memory [KB]) (?)  */
#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
         ulResult = 0;
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_FM:      /* UNDOCUMENTED! (Fixed Memory/Heap [KB]) (?)     */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            ulResult = memorystatus.dwTotalPhys / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_MAXPRMEM, QSV_MAXPRMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               ulResult = 0;
            else
               ulResult = ulSysInfo / 1024;
         }
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_FMSEGS:  /* UNDOCUMENTED! (Segments in Fixed Memory/Heap) (?) */
#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
         ulResult = 1;
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_SWAP:    /* UNDOCUMENTED! (Free Swap Memory [KB])             */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            ulResult = memorystatus.dwAvailPageFile / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            /* NOTE: There is no way to know how much a swap file can grow on an
                     OS/2 system. I think we should return free space on DASD
                     media which contains swap file [maurilio.longo] */
            ulResult = 9999;
         }
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_CONV:    /* UNDOCUMENTED! (Free Conventional [KB])            */
#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
         ulResult = 0;
#else
         ulResult = 9999;
#endif
         break;

      case HB_MEM_EMSUSED: /* UNDOCUMENTED! (Used Expanded Memory [KB]) (?)     */
         ulResult = 0;
         break;

      case HB_MEM_USED:    /* Harbour extension (Memory used [bytes])           */
#ifdef HB_FM_STATISTICS
         ulResult = s_lMemoryConsumed;
#else
         ulResult = 0;
#endif
         break;

      case HB_MEM_BLOCKS:  /* Harbour extension (Memory blocks used)            */
#ifdef HB_FM_STATISTICS
         ulResult = s_lMemoryBlocks;
#else
         ulResult = 0;
#endif
         break;

      case HB_MEM_USEDMAX: /* Harbour extension (Maximum memory used [bytes])   */
#ifdef HB_FM_STATISTICS
         ulResult = s_lMemoryMaxConsumed;
#else
         ulResult = 0;
#endif
         break;

      case HB_MEM_STACKITEMS: /* Harbour extension (Total items allocated for the stack)      */
         ulResult = hb_stack_ready ?  hb_stackTotalItems() : 0;
         break;

      case HB_MEM_STACK:   /* Harbour extension (Total memory size used by the stack [bytes]) */
         ulResult = hb_stack_ready ?  hb_stackTotalItems() * sizeof( HB_ITEM ) : 0;
         break;

      case HB_MEM_STACK_TOP: /* Harbour extension (Total items currently on the stack)      */
         ulResult = hb_stack_ready ? hb_stackTopOffset() : 0;
         break;

      case HB_MEM_LIST_BLOCKS: /* Harbour extension (List all allocated blocks)      */
#ifdef HB_FM_STATISTICS
         {
            char        membuffer[ HB_MAX_MEM2STR_BLOCK * 2 + 1 ]; /* multiplied by 2 to allow hex format */
            USHORT      ui;
            PHB_MEMINFO pMemBlock;

            TraceLog( NULL, "Total %" HB_PFS "i allocations (%" HB_PFS "i reallocation), of which %" HB_PFS "i freed.\n", s_lAllocations, s_lReAllocations, s_lFreed );
            TraceLog( NULL, "--------------------------------------------------------------------------------------\n" );

            for( ui = 1, pMemBlock = s_pFirstBlock; pMemBlock; pMemBlock = pMemBlock->pNextBlock, ++ui )
            {
               TraceLog( NULL, "Block %i %p (size %" HB_PFS "u) %s(%i), \"%s\"\n", ui,
                         ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                         pMemBlock->ulSize, pMemBlock->szProcName, pMemBlock->uiProcLine,
                         hb_mem2str( membuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,  pMemBlock->ulSize ) );
            }

            ulResult = s_lMemoryConsumed;
         }
#else
         ulResult = 0;
#endif
         break;

      default:
         ulResult = 0;
   }

   return ulResult;
}

HB_FUNC( MEMORY )
{
   HB_THREAD_STUB_API
   hb_retns( hb_xquery( ( USHORT ) hb_parni( 1 ) ) );
}

#ifdef HB_FM_STATISTICS
HB_FUNC( HB_FM_STAT )
{
};
#else
HB_FUNC( HB_FM_NOSTAT )
{
};
#endif

/* This pragma with maximum priority [64] under c function, all other xharbour startup has priority [100] */
#if ( defined( __BORLANDC__ ) && ! defined( __EXPORT__ ) )
   #pragma startup hb_xinit 64
#endif
