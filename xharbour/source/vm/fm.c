/*
 * $Id: fm.c,v 1.58 2004/02/25 15:10:58 lculik Exp $
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
 *    MEMORY()
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 *    Threadsafing of MT startup and closing sequences
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
* JC1:
* In MT system, the stack of the virtual machine may not be ready
* to accept hb_x* request of accessing the stack to store data about
* memory consumption.
* For this reason, an IFDEF marked with a  *** comment has been added
* to make sure that the stack is currently able to accept the requests
* from FM.
*/

#define HB_OS_WIN_32_USED
#define HB_THREAD_OPTIMIZE_STACK

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_BASE
#define INCL_DOSMISC
#define INCL_DOSERRORS
#define INCL_DOSPROCESS

#ifndef __MPW__
   #ifdef HB_OS_BSD
      #include <stdlib.h>     /* There is no malloc.h in Darwin, and BSD complaints, too */
   #else
      #include <malloc.h>
   #endif
#endif

#include "hbapi.h"
#include "hbver.h"
#include "hbapifs.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbmemory.ch"

#ifndef HB_FM_STATISTICS
#  undef HB_PARANOID_MEM_CHECK
#endif
#ifndef HB_OS_WIN_32
#  undef HB_FM_WIN32_ALLOC
#endif

#ifdef HB_PARANOID_MEM_CHECK
   HB_ITEM itmMemStat;
   char *pszMemStat = "This is static text for paranoid mem test.";
   BOOL bParanoidMemInit = FALSE;
#endif

#if defined(HB_FM_STATISTICS) && !defined(HB_TR_LEVEL)
   #define HB_TR_LEVEL HB_TR_ERROR
#endif

#ifdef HB_FM_STATISTICS
#include <time.h>
#define HB_MEMINFO_SIGNATURE 0x19730403

typedef struct _HB_MEMINFO
{
   ULONG  ulSignature;
   ULONG  ulSize;
   USHORT uiProcLine;
   char   szProcName[ HB_SYMBOL_NAME_LEN + 1 ];
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
} HB_MEMINFO, * PHB_MEMINFO;

static LONG s_lMemoryBlocks = 0;      /* memory blocks used */
static LONG s_lMemoryMaxBlocks = 0;   /* maximum number of used memory blocks */
static LONG s_lMemoryMaxConsumed = 0; /* memory size consumed */
static LONG s_lMemoryConsumed = 0;    /* memory max size consumed */
static LONG s_lAllocations = 0;
static LONG s_lReAllocations = 0;
static LONG s_lFreed = 0;

static PHB_MEMINFO s_pFirstBlock = NULL;
static PHB_MEMINFO s_pLastBlock = NULL;

#endif

#ifdef HB_PARANOID_MEM_CHECK
void hb_paraniodMemInit( void *pMem, ULONG ulSize )
{
   void *pTmp;

   if( !bParanoidMemInit )
   {
      itmMemStat.type = HB_IT_STRING;
      itmMemStat.item.asString.puiHolders = (ULONG*) malloc( sizeof( ULONG ) );
      *( itmMemStat.item.asString.puiHolders ) = 1;
      itmMemStat.item.asString.bStatic = FALSE;
      itmMemStat.item.asString.length  = strlen(pszMemStat);
      itmMemStat.item.asString.value   = pszMemStat;

      bParanoidMemInit = TRUE;
   }
   ( char * ) pTmp = ( char * ) pMem + ulSize - sizeof(HB_ITEM);
   while( pTmp >= pMem )
   {
      memcpy(pTmp, &itmMemStat, sizeof(HB_ITEM));
      ( char * ) pTmp -= sizeof(HB_ITEM);
   }
   //memset(pMem, 0, ulSize);
}

void hb_paraniodMemCheck( void *pMem )
{
   static BOOL bLoop = FALSE;

   if( ! bLoop &&
       ( pMem == pszMemStat ||
         pMem == itmMemStat.item.asString.puiHolders ||
         itmMemStat.item.asString.puiHolders == NULL ||
         *( itmMemStat.item.asString.puiHolders ) != 1 ) )
   {
      bLoop = TRUE;
      hb_errInternal( HB_EI_XMEMOVERFLOW, "hb_xfree(%p) [Paranoid Test] Pointer Overflow '%s'", (char *) pMem, (char *) pMem );
   }
}
#endif

/* allocates fixed memory, do *not* exits on failure */
void HB_EXPORT * hb_xalloc( ULONG ulSize )
{
   void * pMem;
#ifdef HB_FM_STATISTICS
    HB_THREAD_STUB
#endif

   /* NOTE: we cannot use here HB_TRACE because it will overwrite the
    * function name/line number of code which called hb_xalloc/hb_xgrab
    */
   HB_TRACE_STEALTH(HB_TR_INFO, ("hb_xalloc(%lu)", ulSize));

   if( ulSize == 0 )
   {
      hb_errInternal( HB_EI_XGRABNULLSIZE, NULL, NULL, NULL );
   }

#ifdef HB_FM_STATISTICS

   HB_CRITICAL_LOCK( hb_allocMutex );

#ifdef HB_FM_WIN32_ALLOC
   pMem = (void *) LocalAlloc( LMEM_FIXED, ulSize + sizeof( HB_MEMINFO ) + sizeof( ULONG ) );
#else
   pMem = malloc( ulSize + sizeof( HB_MEMINFO ) + sizeof( ULONG ) );
#endif

   if( ! pMem )
   {
      HB_CRITICAL_UNLOCK( hb_allocMutex );
      return NULL;
   }

   s_lAllocations++;

   if( ! s_pFirstBlock )
   {
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = NULL;
      s_pFirstBlock = ( PHB_MEMINFO ) pMem;
   }
   else
   {
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = s_pLastBlock;
      s_pLastBlock->pNextBlock = ( PHB_MEMINFO ) pMem;
   }

   s_pLastBlock = ( PHB_MEMINFO ) pMem;

   ( ( PHB_MEMINFO ) pMem )->pNextBlock = NULL;
   ( ( PHB_MEMINFO ) pMem )->ulSignature = HB_MEMINFO_SIGNATURE;
   ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;  /* size of the memory block */

   {
      ULONG *pSig = (ULONG *)( ( ( BYTE * ) pMem ) + ulSize + sizeof(HB_MEMINFO) );
      *pSig = HB_MEMINFO_SIGNATURE;
   }

   if( hb_tr_level() >= HB_TR_DEBUG )
   {
      /* NOTE: PRG line number/procname is not very useful during hunting
      * for memory leaks - this is why we are using the previously stored
      * function/line info - this is a location of code that called
      * hb_xalloc/hb_xgrab
      */
      /* C line number */
      ( ( PHB_MEMINFO ) pMem )->uiProcLine = hb_tr_line_;
      strcpy( ( ( PHB_MEMINFO ) pMem )->szProcName, hb_tr_file_ );
   }
   else
   {

      if(
         #ifdef HB_THREAD_SUPPORT  /*** Se JC1: notes at begin */
            hb_ht_stack == &hb_stack &&
         #endif
         HB_VM_STACK.pItems && ( HB_VM_STACK.pBase != HB_VM_STACK.pItems ) )
      {
          /* PRG line number */
         ( ( PHB_MEMINFO ) pMem )->uiProcLine = (*(HB_VM_STACK.pBase))->item.asSymbol.lineno;
          /* PRG ProcName */
         strcpy( ( ( PHB_MEMINFO ) pMem )->szProcName, (*(HB_VM_STACK.pBase))->item.asSymbol.value->szName );
      }
      else
      {
         /* PRG line number */
         ( ( PHB_MEMINFO ) pMem )->uiProcLine = 0;
         /* PRG ProcName */
         ( ( PHB_MEMINFO ) pMem )->szProcName[ 0 ] = '\0';
      }
   }

   s_lMemoryConsumed += ulSize;

   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
   {
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   }

   s_lMemoryBlocks++;

   if( s_lMemoryMaxBlocks < s_lMemoryBlocks )
   {
      s_lMemoryMaxBlocks = s_lMemoryBlocks;
   }

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_xalloc(%lu) returning: %p", ulSize, (char *) pMem + sizeof( HB_MEMINFO ) ) );

   HB_CRITICAL_UNLOCK( hb_allocMutex );

#ifdef HB_PARANOID_MEM_CHECK
   hb_paraniodMemInit( ( char * ) pMem + sizeof( HB_MEMINFO ), ulSize );
#endif
   return ( char * ) pMem + sizeof( HB_MEMINFO );

#else

#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_LOCK( hb_allocMutex );
#endif

#ifdef HB_FM_WIN32_ALLOC
   pMem = (void *) LocalAlloc( LMEM_FIXED, ulSize );
#else
   pMem = malloc( ulSize );
#endif

#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_UNLOCK( hb_allocMutex );
#endif
   return pMem;
 #endif
}

/* allocates fixed memory, exits on failure */
void HB_EXPORT * hb_xgrab( ULONG ulSize )
{
   void * pMem;

   /* NOTE: we cannot use here HB_TRACE because it will overwrite the
    * function name/line number of code which called hb_xalloc/hb_xgrab
    */
   HB_TRACE_STEALTH(HB_TR_INFO, ("hb_xgrab(%lu)", ulSize));

   if( ulSize == 0 )
   {
      hb_errInternal( HB_EI_XGRABNULLSIZE, NULL, NULL, NULL );
   }

#ifdef HB_FM_STATISTICS

   HB_CRITICAL_LOCK( hb_allocMutex );

#ifdef HB_FM_WIN32_ALLOC
   pMem = (void *) LocalAlloc( LMEM_FIXED, ulSize + sizeof( HB_MEMINFO ) + sizeof( ULONG ) );
#else
   pMem = malloc( ulSize + sizeof( HB_MEMINFO ) + sizeof( ULONG ) );
#endif

   if( ! pMem )
   {
      HB_CRITICAL_UNLOCK( hb_allocMutex );

      hb_errInternal( HB_EI_XGRABALLOC, NULL, NULL, NULL );
   }
   /* allocation should be counted AFTER we know that malloc has suceed */
   s_lAllocations++;

   if( ! s_pFirstBlock )
   {
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = NULL;
      s_pFirstBlock = ( PHB_MEMINFO ) pMem;
   }
   else
   {
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = s_pLastBlock;
      s_pLastBlock->pNextBlock = ( PHB_MEMINFO ) pMem;
   }

   s_pLastBlock = ( PHB_MEMINFO ) pMem;

   ( ( PHB_MEMINFO ) pMem )->pNextBlock = NULL;
   ( ( PHB_MEMINFO ) pMem )->ulSignature = HB_MEMINFO_SIGNATURE;
   ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;  /* size of the memory block */

   {
      ULONG *pSig = (ULONG *)( ( ( BYTE * ) pMem ) + ulSize + sizeof(HB_MEMINFO) );
      *pSig = HB_MEMINFO_SIGNATURE;
   }

   if( hb_tr_level() >= HB_TR_DEBUG )
   {
      /* NOTE: PRG line number/procname is not very useful during hunting
      * for memory leaks - this is why we are using the previously stored
      * function/line info - this is a location of code that called
      * hb_xalloc/hb_xgrab
      */
      /* C line number */
      ( ( PHB_MEMINFO ) pMem )->uiProcLine = hb_tr_line_;
      strcpy( ( ( PHB_MEMINFO ) pMem )->szProcName, hb_tr_file_ );
   }
   else
   {
      HB_THREAD_STUB

      if(
         #if defined( HB_THREAD_SUPPORT )  /*** Se JC1: notes at begin */
            hb_ht_stack != 0 &&
         #endif
         HB_VM_STACK.pItems && ( HB_VM_STACK.pBase != HB_VM_STACK.pItems ) )
      {
         /* PRG line number */
         ( ( PHB_MEMINFO ) pMem )->uiProcLine = (*(HB_VM_STACK.pBase))->item.asSymbol.lineno;
         /* PRG ProcName */
         strcpy( ( ( PHB_MEMINFO ) pMem )->szProcName, (*(HB_VM_STACK.pBase))->item.asSymbol.value->szName );
      }
      else
      {
         /* PRG line number */
         ( ( PHB_MEMINFO ) pMem )->uiProcLine = 0;
         /* PRG ProcName */
         ( ( PHB_MEMINFO ) pMem )->szProcName[ 0 ] = '\0';
      }
   }

   s_lMemoryConsumed += ulSize;

   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
   {
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   }

   s_lMemoryBlocks++;

   if( s_lMemoryMaxBlocks < s_lMemoryBlocks )
   {
      s_lMemoryMaxBlocks = s_lMemoryBlocks;
   }

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_xgrab(%lu) returning: %p", ulSize, (char *) pMem + sizeof( HB_MEMINFO ) ) );

   HB_CRITICAL_UNLOCK( hb_allocMutex );

#ifdef HB_PARANOID_MEM_CHECK
   hb_paraniodMemInit( ( char * ) pMem + sizeof( HB_MEMINFO ), ulSize );
#endif
   return ( char * ) pMem + sizeof( HB_MEMINFO );

#else

#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_LOCK( hb_allocMutex );
#endif

#ifdef HB_FM_WIN32_ALLOC
   pMem = (void *) LocalAlloc( LMEM_FIXED, ulSize );
#else
   pMem = malloc( ulSize );
#endif

#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_UNLOCK( hb_allocMutex );
#endif

   if( ! pMem )
   {
      hb_errInternal( HB_EI_XGRABALLOC, NULL, NULL, NULL );
   }

   return pMem;

#endif
}

void HB_EXPORT * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
#ifdef HB_FM_STATISTICS
   PHB_MEMINFO pMemBlock;
   ULONG ulMemSize;
   ULONG *pSig;

   HB_TRACE_STEALTH(HB_TR_INFO, ("hb_xrealloc(%p, %lu)", pMem, ulSize));

   HB_CRITICAL_LOCK( hb_allocMutex );
   s_lReAllocations++;

   if( ! pMem )
   {
      HB_CRITICAL_UNLOCK( hb_allocMutex );
      hb_errInternal( HB_EI_XREALLOCNULL, NULL, NULL, NULL );
   }

   if( ulSize == 0 )
   {
      HB_CRITICAL_UNLOCK( hb_allocMutex );
      hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
   }

   pMemBlock = ( PHB_MEMINFO ) ( ( char * ) pMem - sizeof( HB_MEMINFO ) );

   if( pMemBlock->ulSignature != HB_MEMINFO_SIGNATURE )
   {
      HB_CRITICAL_UNLOCK( hb_allocMutex );
      hb_errInternal( HB_EI_XREALLOCINV, NULL, NULL, NULL );
   }

   ulMemSize = pMemBlock->ulSize;

   pSig = (ULONG *)( ( ( BYTE * ) pMem ) + ulMemSize );

   if( *pSig != HB_MEMINFO_SIGNATURE )
   {
      HB_CRITICAL_UNLOCK( hb_allocMutex );
      hb_errInternal( HB_EI_XMEMOVERFLOW, "hb_xrealloc()", NULL, NULL );
   }

#ifdef HB_FM_WIN32_ALLOC
   pMem = (void *) LocalReAlloc( (HLOCAL) pMemBlock,
          ulSize + sizeof( HB_MEMINFO ) + sizeof( ULONG ), LMEM_MOVEABLE );
#else
   pMem = realloc( pMemBlock, ulSize + sizeof( HB_MEMINFO ) + sizeof( ULONG ) );
#endif

   s_lMemoryConsumed += ( ulSize - ulMemSize );

   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
   {
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   }

   if( ! pMem )
   {
      HB_CRITICAL_UNLOCK( hb_allocMutex );
      hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );
   }

   ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;  /* size of the memory block */
   {
      ULONG *pSig = (ULONG *)( ( ( BYTE * ) pMem ) + ulSize + sizeof(HB_MEMINFO) );
      *pSig = HB_MEMINFO_SIGNATURE;
   }

   if( ( ( PHB_MEMINFO ) pMem )->pPrevBlock )
   {
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock->pNextBlock = ( PHB_MEMINFO ) pMem;
   }

   if( ( ( PHB_MEMINFO ) pMem )->pNextBlock )
   {
      ( ( PHB_MEMINFO ) pMem )->pNextBlock->pPrevBlock = ( PHB_MEMINFO ) pMem;
   }

   if( s_pFirstBlock == pMemBlock )
   {
      s_pFirstBlock = ( PHB_MEMINFO ) pMem;
   }

   if( s_pLastBlock == pMemBlock )
   {
      s_pLastBlock = ( PHB_MEMINFO ) pMem;
   }

   HB_CRITICAL_UNLOCK( hb_allocMutex );

#ifdef HB_PARANOID_MEM_CHECK
   if( ulSize > ulMemSize )
      hb_paraniodMemInit( ( char * ) pMem + sizeof( HB_MEMINFO ) + ulMemSize, ulSize - ulMemSize );
#endif
   return ( char * ) pMem + sizeof( HB_MEMINFO );

#else

   HB_TRACE(HB_TR_DEBUG, ("hb_xrealloc(%p, %lu)", pMem, ulSize));

   if( ! pMem )
   {
      hb_errInternal( HB_EI_XREALLOCNULL, NULL, NULL, NULL );
   }

   if( ulSize == 0 )
   {
      hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
   }


#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_LOCK( hb_allocMutex );
#endif

#ifdef HB_FM_WIN32_ALLOC
   pMem = (void *) LocalReAlloc( (HLOCAL) pMem, ulSize, LMEM_MOVEABLE );
#else
   pMem = realloc( pMem, ulSize );
#endif

#ifndef HB_SAFE_ALLOC
   HB_CRITICAL_UNLOCK( hb_allocMutex );
#endif

   if( ! pMem )
   {
      hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );
   }

   return pMem;

#endif
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{

#ifdef HB_FM_STATISTICS

   HB_TRACE_STEALTH( HB_TR_INFO, ( "hb_xfree(%p)", pMem ) );

   HB_CRITICAL_LOCK( hb_allocMutex );
   s_lFreed++;

   if( pMem )
   {
      PHB_MEMINFO pMemBlock = ( PHB_MEMINFO ) ( ( char * ) pMem - sizeof( HB_MEMINFO ) );
      ULONG *pSig;

#ifdef HB_PARANOID_MEM_CHECK
      hb_paraniodMemCheck( pMem );
#endif
      if( pMemBlock->ulSignature != HB_MEMINFO_SIGNATURE )
      {
         //printf( "hb_xfree() Invalid Pointer %p %s", (char *) pMem, (char *) pMem );

         HB_CRITICAL_UNLOCK( hb_allocMutex );
         hb_errInternal( HB_EI_XFREEINV, "hb_xfree() Invalid Pointer %p %s", (char *) pMem, (char *) pMem );
      }

      pSig  = (ULONG *)( ( ( BYTE * ) pMem ) + pMemBlock->ulSize );

      if( *pSig != HB_MEMINFO_SIGNATURE )
      {
         HB_CRITICAL_UNLOCK( hb_allocMutex );
         hb_errInternal( HB_EI_XMEMOVERFLOW, "hb_xfree(%p) Pointer Overflow '%s'", (char *) pMem, (char *) pMem );
      }

      s_lMemoryConsumed -= pMemBlock->ulSize;
      s_lMemoryBlocks--;

      if( pMemBlock->pPrevBlock )
      {
         pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;
      }
      else
      {
         s_pFirstBlock = pMemBlock->pNextBlock;
      }

      if( pMemBlock->pNextBlock )
      {
         pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;
      }
      else
      {
         s_pLastBlock = pMemBlock->pPrevBlock;
      }

      pMemBlock->ulSignature = *pSig = 0;

#ifdef HB_FM_WIN32_ALLOC
      LocalFree( (HLOCAL) pMemBlock );
#else
      free( ( void * ) pMemBlock );
#endif
      HB_CRITICAL_UNLOCK( hb_allocMutex );
   }
   else
   {
      HB_TRACE_STEALTH(HB_TR_INFO, ("hb_xfree(NULL)!"));

      HB_CRITICAL_UNLOCK( hb_allocMutex );

      hb_errInternal( HB_EI_XFREENULL, "hb_xfree(NULL)", NULL, NULL );
   }

#else

   HB_TRACE(HB_TR_DEBUG, ("hb_xfree(%p)", pMem));

   if( pMem )
   {
#ifndef HB_SAFE_ALLOC
      HB_CRITICAL_LOCK( hb_allocMutex );
#endif

#ifdef HB_FM_WIN32_ALLOC
      LocalFree( (HLOCAL) pMem );
#else
      free( pMem );
#endif

#ifndef HB_SAFE_ALLOC
      HB_CRITICAL_UNLOCK( hb_allocMutex );
#endif
   }
   else
   {
      hb_errInternal( HB_EI_XFREENULL, NULL, NULL, NULL );
   }

#endif
}

/* NOTE: Debug function, it will always return 0 when HB_FM_STATISTICS is
         not defined, don't use it for final code [vszakats] */

ULONG HB_EXPORT hb_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xsize(%p)", pMem));

   #ifdef HB_FM_STATISTICS
      return ( ( PHB_MEMINFO ) ( ( char * ) pMem - sizeof( HB_MEMINFO ) ) )->ulSize;
   #else
      HB_SYMBOL_UNUSED( pMem );

      return 0;
   #endif
}

void HB_EXPORT hb_xinit( void ) /* Initialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xinit()"));

   #ifdef HB_THREAD_SUPPORT
      hb_threadInit();
      hb_stackInit();
   #endif
}


/* Returns pointer to string containing printable version
   of pMem memory block */

char * hb_mem2str( void * pMem, UINT uiSize )
{
#define HB_MAX_MEM2STR_BLOCK 256

   static BYTE cBuffer[2*HB_MAX_MEM2STR_BLOCK+1]; /* multiplied by 2 to allow hex format */
   BYTE *cMem = (BYTE*) pMem;
   UINT uiIndex, uiPrintable;

   if( uiSize > HB_MAX_MEM2STR_BLOCK )
      uiSize = HB_MAX_MEM2STR_BLOCK;

   uiPrintable = 0;
   for( uiIndex=0; uiIndex < uiSize; uiIndex++ )
      if( cMem[uiIndex] >= ' ' )
         uiPrintable++;

   if( (uiPrintable*100)/uiSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( uiIndex=0; uiIndex < uiSize; uiIndex++ )
         if( cMem[uiIndex] >= ' ' )
            cBuffer[uiIndex] = cMem[uiIndex];
         else
            cBuffer[uiIndex] = '.';
      cBuffer[uiIndex] = '\0';
   }
   else
   {
     /* format as hex */
      for( uiIndex=0; uiIndex < uiSize; uiIndex++ )
      {
         int lownibble, hinibble;
         lownibble = (cMem[uiIndex])>>4;
         hinibble = (cMem[uiIndex]) & 0x0F;
         cBuffer[uiIndex*2] = (hinibble <= 9) ?  ('0'+hinibble) : ('A'+hinibble-10);
         cBuffer[uiIndex*2+1] = (lownibble <= 9) ? ('0'+lownibble) : ('A'+lownibble-10);
      }
      cBuffer[uiIndex*2] = '\0';
   }

   return (char *)cBuffer;
}



void HB_EXPORT hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xexit()"));

#ifdef HB_FM_STATISTICS

//JC1: The problem with threads here is that the stack has already been
// destroyed, but hb_conOut and other functions may allocate memory with xgrab, using the
// stack... Notice that this can possibly leads to problem also in ST
// apps in the future. We must de-tangle the initialization and closing
// sequence

   if( s_lMemoryBlocks || hb_cmdargCheck( "INFO" ) )
   {
      PHB_MEMINFO pMemBlock;
      USHORT ui;
      char buffer[ 100 ];
      FILE *hLog = NULL;
      char szResult_Date[ 11 ];
      char szResult_Time[ 11 ];
      time_t t;
      struct tm * oTime;

      if( s_lMemoryBlocks )
      {
         time( &t );
         oTime = localtime( &t );
         sprintf( szResult_Date, "%04d.%02d.%02d", oTime->tm_year + 1900, oTime->tm_mon + 1, oTime->tm_mday );
         sprintf( szResult_Time, "%02d:%02d:%02d", oTime->tm_hour, oTime->tm_min, oTime->tm_sec );
         hLog = fopen( "fm.log", "a+" );
      }

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      sprintf( buffer, "Total %li allocations (%li reallocation), of which %li freed.", s_lAllocations, s_lReAllocations, s_lFreed );
      if ( hLog )
      {
         char *szPlatform = hb_verPlatform();
         char *szCompiler = hb_verCompiler();
         char *szHarbour  = hb_verHarbour();
         fprintf( hLog, "Memory Allocation Report\n");
         fprintf( hLog, "Application: %s\n", hb_cmdargARGV()[0] );
         fprintf( hLog, "xHarbour Version: %s\n", szHarbour );
         fprintf( hLog, "Compiler: %s\n", szCompiler );
         fprintf( hLog, "Platform: %s\n", szPlatform );
         fprintf( hLog, "Time Occured: %s %s\n", szResult_Date, szResult_Time );
         fprintf( hLog, "%s\n", buffer );
         hb_xfree( szPlatform );
         hb_xfree( szCompiler );
         hb_xfree( szHarbour  );
      }
      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      sprintf( buffer, "Highest total allocated %li bytes in %li blocks.", s_lMemoryMaxConsumed, s_lMemoryMaxBlocks );
      if ( hLog )
      {
         fprintf( hLog, "%s\n", buffer );
      }
      hb_conOutErr( buffer, 0 );

      if( s_lMemoryBlocks )
      {
         hb_conOutErr( hb_conNewLine(), 0 );
         sprintf( buffer, "WARNING! Memory allocated but not released: %li bytes (%li blocks)", s_lMemoryConsumed, s_lMemoryBlocks );
         if ( hLog )
         {
            fprintf( hLog, "%s\n", buffer );
         }
         hb_conOutErr( buffer, 0 );
      }

      hb_conOutErr( hb_conNewLine(), 0 );

      for( ui = 1, pMemBlock = s_pFirstBlock; pMemBlock; pMemBlock = pMemBlock->pNextBlock )
      {
         HB_TRACE( HB_TR_ERROR, ( "Block %i %p (size %lu) %s(%i), \"%s\"",
            ui++,
            (char *) ( pMemBlock + 1 ),
            pMemBlock->ulSize,
            pMemBlock->szProcName,
            pMemBlock->uiProcLine,
            hb_mem2str( pMemBlock + 1, pMemBlock->ulSize ) ) );

         if ( hLog )
         {
            fprintf( hLog, "Block %i %p (size %lu) %s(%i), \"%s\"\n",
               ui-1,
               (char *) ( pMemBlock + 1 ),
               pMemBlock->ulSize,
               pMemBlock->szProcName,
               pMemBlock->uiProcLine,
               hb_mem2str( pMemBlock + 1, pMemBlock->ulSize ) );
         }
      }

      if( hLog )
      {
         fprintf( hLog, "--------------------------------------------------------------------------------\n");
         fclose( hLog );
      }
   }

#endif
}

#if UINT_MAX != ULONG_MAX

/* hb_xmemcpy and hb_xmemset are only needed when
   unsigned int and unsigned long differ in length */

void * hb_xmemcpy( void * pDestArg, void * pSourceArg, ULONG ulLen )
{
   BYTE * pDest;
   BYTE * pSource;
   ULONG  ulRemaining;
   int    iCopySize;

   HB_TRACE(HB_TR_DEBUG, ("hb_xmemcpy(%p, %p, %lu)", pDestArg, pSourceArg, ulLen));

   pDest = ( BYTE * ) pDestArg;
   pSource = ( BYTE * ) pSourceArg;
   ulRemaining = ulLen;

   while( ulRemaining )
   {
      /* Overcome the memcpy() size_t limitation */
      if( ulRemaining > UINT_MAX )
      {
         iCopySize = UINT_MAX;
         ulRemaining -= ( ULONG ) iCopySize;
      }
      else
      {
         iCopySize = ( int ) ulRemaining;
         ulRemaining = 0;
      }
      memcpy( pDest, pSource, iCopySize );
      pDest += iCopySize;
      pSource += iCopySize;
   }

   return pDestArg;
}

void * hb_xmemset( void * pDestArg, int iFill, ULONG ulLen )
{
   BYTE * pDest;
   ULONG  ulRemaining;
   int    iSetSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_xmemset(%p, %d, %lu)", pDestArg, iFill, ulLen));

   pDest = ( BYTE * ) pDestArg;
   ulRemaining = ulLen;

   while( ulRemaining )
   {
      /* Overcome the memset() size_t limitation */
      if( ulRemaining > UINT_MAX )
      {
         iSetSize = UINT_MAX;
         ulRemaining -= ( ULONG ) iSetSize;
      }
      else
      {
         iSetSize = ( int ) ulRemaining;
         ulRemaining = 0;
      }
      memset( pDest, iFill, iSetSize );
      pDest += iSetSize;
   }

   return pDestArg;
}

#endif

ULONG hb_xquery( USHORT uiMode )
{
   ULONG ulResult;
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_xquery(%hu)", uiMode));

   /* TODO: Return the correct values instead of 9999 [vszakats] */

   switch( uiMode )
   {
   case HB_MEM_CHAR:       /*               (Free Variable Space [KB])          */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPhys / 1024;
      }
      #elif defined(HB_OS_OS2)
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

   case HB_MEM_BLOCK:      /*               (Largest String [KB])               */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = HB_MIN( memorystatus.dwAvailPhys, ULONG_MAX ) / 1024;
      }
      #elif defined(HB_OS_OS2)
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

   case HB_MEM_RUN:        /*               (RUN Memory [KB])                   */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPhys / 1024;
      }
      #elif defined(HB_OS_OS2)
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

   case HB_MEM_VM:         /* UNDOCUMENTED! (Virtual Memory [KB])               */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailVirtual / 1024;
      }
      #elif defined(HB_OS_OS2)
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

   case HB_MEM_EMS:        /* UNDOCUMENTED! (Free Expanded Memory [KB]) (?)     */
      #if defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
         ulResult = 0;
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_FM:         /* UNDOCUMENTED! (Fixed Memory/Heap [KB]) (?)        */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwTotalPhys / 1024;
      }
      #elif defined(HB_OS_OS2)
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

   case HB_MEM_FMSEGS:     /* UNDOCUMENTED! (Segments in Fixed Memory/Heap) (?) */
      #if defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
         ulResult = 1;
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_SWAP:       /* UNDOCUMENTED! (Free Swap Memory [KB])             */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPageFile / 1024;
      }
      #elif defined(HB_OS_OS2)
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

   case HB_MEM_CONV:       /* UNDOCUMENTED! (Free Conventional [KB])            */
      #if defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
         ulResult = 0;
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_EMSUSED:    /* UNDOCUMENTED! (Used Expanded Memory [KB]) (?)     */
      ulResult = 0;
      break;

   case HB_MEM_USED:       /* Harbour extension (Memory used [bytes])           */
#ifdef HB_FM_STATISTICS
      ulResult = s_lMemoryConsumed;
#else
      ulResult = 0;
#endif
      break;

   case HB_MEM_USEDMAX:    /* Harbour extension (Maximum memory used [bytes])   */
#ifdef HB_FM_STATISTICS
      ulResult = s_lMemoryMaxConsumed;
#else
      ulResult = 0;
#endif
      break;

   case HB_MEM_STACKITEMS: /* Harbour extension (Total items allocated for the stack)      */
      #ifdef HB_THREAD_SUPPORT  /*** Se JC1: notes at begin */
         if ( hb_ht_stack == &hb_stack )
         {
      #endif

         ulResult = HB_VM_STACK.wItems;

      #ifdef HB_THREAD_SUPPORT
         }
         else
         {
            ulResult = 0;
         }
      #endif
      break;

   case HB_MEM_STACK:      /* Harbour extension (Total memory size used by the stack [bytes]) */
      #ifdef HB_THREAD_SUPPORT  /*** Se JC1: notes at begin */
         if ( hb_ht_stack == &hb_stack )
         {
      #endif

         ulResult = HB_VM_STACK.wItems * sizeof( HB_ITEM );

      #ifdef HB_THREAD_SUPPORT
         }
         else
         {
            ulResult = 0;
         }
      #endif
      break;

   case HB_MEM_STACK_TOP : /* Harbour extension (Total items currently on the stack)      */
      ulResult = hb_stackTopOffset( );
      break;

   case HB_MEM_LIST_BLOCKS : /* Harbour extension (List all allocated blocks)      */
     #ifdef HB_FM_STATISTICS
     {
        USHORT ui;
        PHB_MEMINFO pMemBlock;

        TraceLog( NULL, "Total %li allocations (%li reallocation), of which %li freed.\n", s_lAllocations, s_lReAllocations, s_lFreed );
        TraceLog( NULL, "--------------------------------------------------------------------------------------\n" );

        for( ui = 1, pMemBlock = s_pFirstBlock; pMemBlock; pMemBlock = pMemBlock->pNextBlock )
        {
           TraceLog( NULL, "Block %i %p (size %lu) %s(%i), \"%s\"\n",
              ui++,
              (char *) ( pMemBlock + 1 ),
              pMemBlock->ulSize,
              pMemBlock->szProcName,
              pMemBlock->uiProcLine,
              hb_mem2str( pMemBlock + 1, pMemBlock->ulSize ) );
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
   HB_THREAD_STUB
   hb_retnl( hb_xquery( hb_parni( 1 ) ) );
}

#ifdef HB_FM_STATISTICS
HB_FUNC( HB_FM_STAT ) {};
#else
HB_FUNC( HB_FM_NOSTAT ) {};
#endif
