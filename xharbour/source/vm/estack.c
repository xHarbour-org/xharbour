/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The eval stack management functions
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
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *
 */

/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#define HB_OS_WIN_USED

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbdefs.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbapirdd.h"
#include "hbmath.h"

HB_EXTERN_BEGIN

/* ------------------------------- */

#if ! defined( STACK_INITHB_ITEMS )
   #define STACK_INITHB_ITEMS    200
#endif
#if ! defined( STACK_EXPANDHB_ITEMS )
   #define STACK_EXPANDHB_ITEMS  20
#endif

/* ------------------------------- */

#if ! defined( _HB_STACK_LOCAL_MACROS_ )
   #if defined( HB_THREAD_SUPPORT )
      HB_STACK hb_stackMT;
   #else
      HB_STACK hb_stackST;
      HB_STACK *  hb_stack_ptr   = &hb_stackST;
   #endif /* HB_THREAD_SUPPORT */
   #if ! defined( HB_VM_ALL )
      BOOL        hb_stack_ready = FALSE;
   #endif /* ! HB_VM_ALL */
#endif /* _HB_STACK_LOCAL_MACROS_ */

static HB_IOERRORS s_IOErrors;

/* ------------------------------- */

static HB_SYMB s_initSymbol = { "hb_stackInit", { HB_FS_STATIC }, { NULL }, NULL };

/* ------------------------------- */

/* Stack initialization part common for ST and MT */
void hb_stack_init( PHB_STACK pStack )
{
   LONG i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_stack_init(%p)", pStack ) );

   memset( pStack, 0, sizeof( HB_STACK ) );

   pStack->pItems = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * STACK_INITHB_ITEMS );
   pStack->pBase  = pStack->pItems;
   pStack->pPos   = pStack->pItems;       /* points to the first stack item */
   pStack->wItems = STACK_INITHB_ITEMS;
   pStack->pEnd   = pStack->pItems + pStack->wItems;

   for( i = 0; i < pStack->wItems; ++i )
   {
      pStack->pItems[ i ]        = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
      pStack->pItems[ i ]->type  = HB_IT_NIL;
   }

   pStack->pPos++;
   hb_itemPutSymbol( *pStack->pItems, &s_initSymbol );

   pStack->Return.type        = HB_IT_NIL;

   pStack->rdd                = hb_rddWaInit();
   pStack->rddTls.uiCurrArea  = 1;
}

void hb_stackInit( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stackInit()" ) );

   hb_vmProcessSymbols( &s_initSymbol, 1, "estack.c",  ( int ) HB_PCODE_VER, NULL );

#ifndef HB_THREAD_SUPPORT
   hb_stack_init( &hb_stackST );
#else
   hb_threadSetupStack( &hb_stackMT, HB_CURRENT_THREAD() );
#endif

   hb_stack_ready = TRUE;
}

void hb_stackFree( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stackFree()" ) );

   hb_stack_ready = FALSE;

#ifndef HB_THREAD_SUPPORT
   {
      long i = hb_stackST.wItems - 1;

      while( i >= 0 )
      {
         if( HB_IS_SYMBOL( hb_stackST.pItems[ i ] ) )
            hb_xfree( hb_stackST.pItems[ i ]->item.asSymbol.pCargo );

         hb_xfree( hb_stackST.pItems[ i-- ] );
      }
   }

   hb_xfree( hb_stackST.pItems );

   hb_stackST.pItems = NULL;
   hb_stackST.pBase  = NULL;
   hb_stackST.pPos   = NULL;
   hb_stackST.pEnd   = NULL;
   hb_stackST.wItems = 0;

   hb_rddWaShutDown( hb_stackST.rdd );

   while( hb_stackST.pSequence )
   {
      PHB_SEQUENCE pFree = hb_stackST.pSequence;

      hb_stackST.pSequence = hb_stackST.pSequence->pPrev;

      hb_xfree( ( void * ) pFree );
   }

#else

   hb_threadDestroyStack( &hb_stackMT );
#endif
}

HB_EXTERN_BEGIN

#if defined( HB_VM_ALL )
BOOL _hb_stack_ready( void )
{
   return hb_stack_ready;
}
#endif

#undef hb_stackId
void * hb_stackId( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stackId()" ) );

#ifdef HB_THREAD_SUPPORT
   return ( void * ) &hb_stackMT;
#else
   return ( void * ) &hb_stackST;
#endif
}

void hb_s_hb_exc( HB_MATH_EXCEPTION hb_exc )
{
#ifdef HB_THREAD_SUPPORT
   HB_THREAD_STUB
   ( HB_VM_STACK.math_exc ).type          = hb_exc.type;
   ( HB_VM_STACK.math_exc ).funcname      = hb_exc.funcname;
   ( HB_VM_STACK.math_exc ).error         = hb_exc.error;
   ( HB_VM_STACK.math_exc ).arg1          = hb_exc.arg1;
   ( HB_VM_STACK.math_exc ).arg2          = hb_exc.arg2;
   ( HB_VM_STACK.math_exc ).retval        = hb_exc.retval;
   ( HB_VM_STACK.math_exc ).retvalwidth   = hb_exc.retvalwidth;
   ( HB_VM_STACK.math_exc ).retvaldec     = hb_exc.retvaldec;
   ( HB_VM_STACK.math_exc ).handled       = hb_exc.handled;
#else
   HB_SYMBOL_UNUSED( hb_exc );
#endif
}

#if ( defined( HB_THREAD_SUPPORT ) && defined( HB_VM_ALL ) )
/*
 * Wrapper to avoid direct access to stack. Needed if VM is amalgamated.
 */

BOOL hb_stackcheckrddpstack( const char * szName, HB_STACK * pstack )
{
   return pstack == &hb_stackMT || strncmp( szName, ":TH:", 4 ) == 0;
}

#endif
HB_EXTERN_END

#undef hb_stackSetStruct
PHB_SET_STRUCT hb_stackSetStruct( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackSetStruct()" ) );

   return &HB_VM_STACK.set;
}

#undef hb_stackPop
void hb_stackPop( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackPop()" ) );

   if( --HB_VM_STACK.pPos <= HB_VM_STACK.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   if( HB_IS_COMPLEX( *( HB_VM_STACK.pPos ) ) )
      hb_itemClear( *( HB_VM_STACK.pPos ) );
   else
      ( *( HB_VM_STACK.pPos ) )->type = HB_IT_NIL;
}

#undef hb_stackPopReturn
void hb_stackPopReturn( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackPopReturn()" ) );

   if( HB_IS_COMPLEX( &HB_VM_STACK.Return ) )
      hb_itemClear( &HB_VM_STACK.Return );

   if( --HB_VM_STACK.pPos <= HB_VM_STACK.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   hb_itemRawMove( &HB_VM_STACK.Return, *HB_VM_STACK.pPos );
}

#undef hb_stackDec
void hb_stackDec( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackDec()" ) );

   if( --HB_VM_STACK.pPos <= HB_VM_STACK.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
}

#undef hb_stackDecrease
void hb_stackDecrease( ULONG ulItems )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackDecrease()" ) );

   if( ( HB_VM_STACK.pPos -= ulItems ) <= HB_VM_STACK.pBase )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
}

#undef hb_stackPush
void hb_stackPush( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackPush()" ) );

   /* enough room for another item ? */
   if( ++HB_VM_STACK.pPos == HB_VM_STACK.pEnd )
      hb_stackIncrease();
}

#undef hb_stackAllocItem
PHB_ITEM hb_stackAllocItem( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackAllocItem()" ) );

   if( ++HB_VM_STACK.pPos == HB_VM_STACK.pEnd )
      hb_stackIncrease();

   return *( HB_VM_STACK.pPos - 1 );
}

#undef hb_stackPushReturn
void hb_stackPushReturn( void )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackPushReturn()" ) );

   hb_itemRawMove( *HB_VM_STACK.pPos, &HB_VM_STACK.Return );

   /* enough room for another item ? */
   if( ++HB_VM_STACK.pPos == HB_VM_STACK.pEnd )
      hb_stackIncrease();
}

void hb_stackIncrease( void )
{
   HB_THREAD_STUB
   long  BaseIndex;  /* index of stack base */
   long  CurrIndex;  /* index of current top item */
   long  EndIndex;   /* index of current top item */

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackIncrease()" ) );

#ifndef HB_ARRAY_USE_COUNTER
   PHB_ITEM * pOldItems = HB_VM_STACK.pItems;
#endif

   BaseIndex   = ( long ) ( HB_VM_STACK.pBase - HB_VM_STACK.pItems );
   CurrIndex   = ( long ) ( HB_VM_STACK.pPos - HB_VM_STACK.pItems );
   EndIndex    = ( long ) ( HB_VM_STACK.pEnd - HB_VM_STACK.pItems );

   /* no, make more headroom: */
   /* hb_stackDispLocal(); */
   HB_VM_STACK.pItems = ( PHB_ITEM * ) hb_xrealloc( ( void * ) HB_VM_STACK.pItems,
                                                    sizeof( PHB_ITEM ) * ( HB_VM_STACK.wItems + STACK_EXPANDHB_ITEMS ) );

   /* fix possibly modified by realloc pointers: */
   HB_VM_STACK.pPos     = HB_VM_STACK.pItems + CurrIndex;
   HB_VM_STACK.pBase    = HB_VM_STACK.pItems + BaseIndex;
   HB_VM_STACK.wItems   += STACK_EXPANDHB_ITEMS;
   HB_VM_STACK.pEnd     = HB_VM_STACK.pItems + HB_VM_STACK.wItems;

#ifndef HB_ARRAY_USE_COUNTER
   if( HB_VM_STACK.pItems != pOldItems )
   {
      LONG i;

      /* TraceLog( NULL, "Expanding Stack: %p\n", pOldItems );
       */

      for( i = 0; i < CurrIndex; i++ )
      {
         if( HB_VM_STACK.pItems[ i ]->type == HB_IT_ARRAY && HB_VM_STACK.pItems[ i ]->item.asArray.value )
            hb_arrayResetHolder( HB_VM_STACK.pItems[ i ]->item.asArray.value, ( void * ) ( pOldItems[ i ] ), ( void * ) ( HB_VM_STACK.pItems[ i ] ) );
         else if( HB_VM_STACK.pItems[ i ]->type == HB_IT_BYREF && HB_VM_STACK.pItems[ i ]->item.asRefer.offset == 0 )
            hb_arrayResetHolder( HB_VM_STACK.pItems[ i ]->item.asRefer.BasePtr.pBaseArray, ( void * ) ( pOldItems[ i ] ), ( void * ) ( HB_VM_STACK.pItems[ i ] ) );
      }

      /* TraceLog( NULL, "New Stack: %p\n", HB_VM_STACK.pItems );
       */
   }
#endif

   do
   {
      HB_VM_STACK.pItems[ EndIndex ]         = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
      HB_VM_STACK.pItems[ EndIndex ]->type   = HB_IT_NIL;
   }
   while( ++EndIndex < HB_VM_STACK.wItems );

}

void hb_stackRemove( long lUntilPos )
{
   HB_THREAD_STUB
   PHB_ITEM * pEnd = HB_VM_STACK.pItems + lUntilPos;

   while( HB_VM_STACK.pPos > pEnd )
      hb_stackPop();
}

PHB_ITEM hb_stackNewFrame( HB_STACK_STATE * pStack, USHORT uiParams )
{
   HB_THREAD_STUB

   PHB_ITEM *  pBase = HB_VM_STACK.pPos - uiParams - 2;
   PHB_ITEM    pItem = *pBase; /* procedure name */

   if( ! HB_IS_SYMBOL( pItem ) )
   {
      /* QUESTION: Is this call needed ? [vszakats] */
      hb_stackDispLocal();
      hb_errInternal( HB_EI_VMNOTSYMBOL, NULL, "hb_vmDo()", NULL );
   }

   pStack->lStatics                          = HB_VM_STACK.lStatics;

   pItem->item.asSymbol.pCargo->privatesbase = hb_memvarGetPrivatesBase();
   pItem->item.asSymbol.pCargo->arguments    = uiParams;

   HB_VM_STACK.pBase                         = pBase;

   return pItem;
}

void hb_stackOldFrame( HB_STACK_STATE * pStack )
{
   HB_THREAD_STUB

   long           iLocal;
   PHB_ITEM       pDetached;
   USHORT         uiRequest;
   PHB_SYMBCARGO  pCargo         = ( *HB_VM_STACK.pBase )->item.asSymbol.pCargo;
   long           stackbase      = pCargo->stackbase;
   int            iArgs          = pCargo->arguments;
   HB_SIZE        ulPrivateBase  = pCargo->privatesbase;

   uiRequest = hb_vmRequestQuery();
   hb_vmRequestReset();

   while( HB_VM_STACK.pPos > HB_VM_STACK.pBase )
   {
      PHB_ITEM pItem = *( HB_VM_STACK.pPos - 1 );

      iLocal = ( long ) ( HB_VM_STACK.pPos - HB_VM_STACK.pBase - 2 );

      if( iLocal >= 0 && iLocal <= iArgs && HB_IS_MEMVAR( pItem ) )
      {
         /* printf( "Func: %s Params: %i Local %i Type: %i\n", (*HB_VM_STACK.pBase)->item.asSymbol.value->szName, (*HB_VM_STACK.pBase)->item.asSymbol.pCargo->arguments, iLocal, pItem->type );
          */

         pDetached = hb_itemUnRefOnce( pItem );

         /* printf( "   Func: %s Params: %i Local %i UnRef Type: %i\n", (*HB_VM_STACK.pBase)->item.asSymbol.value->szName, (*HB_VM_STACK.pBase)->item.asSymbol.pCargo->arguments, iLocal, pDetached->type );
          */

         if( HB_IS_BYREF( pDetached ) )
            hb_itemCopy( pDetached, hb_itemUnRef( pDetached ) );
            /* printf( "Severed Detached Local: %i Type: %i\n", iLocal, pDetached->type );
             */

         hb_itemClear( pItem );
      }
      else if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
      else
         pItem->type = HB_IT_NIL;

      --HB_VM_STACK.pPos;
   }

   hb_memvarSetPrivatesBase( ulPrivateBase );

   HB_VM_STACK.pBase    = HB_VM_STACK.pItems + stackbase;
   HB_VM_STACK.lStatics = pStack->lStatics;

   hb_vmRequest( uiRequest );
}

void hb_stackClearPrivateBases( void )
{
   HB_THREAD_STUB

   PHB_ITEM pBase = *HB_VM_STACK.pBase;

   while( pBase->item.asSymbol.pCargo->privatesbase != 0 )
   {
      pBase->item.asSymbol.pCargo->privatesbase = 0;
      pBase                                     = *( HB_VM_STACK.pItems + pBase->item.asSymbol.pCargo->stackbase );
   }
}

#undef hb_stackTopOffset
long hb_stackTopOffset( void )
{
   HB_THREAD_STUB
   return ( long ) ( HB_VM_STACK.pPos - HB_VM_STACK.pItems );
}

#undef hb_stackBaseOffset
long hb_stackBaseOffset( void )
{
   HB_THREAD_STUB
   return ( long ) ( HB_VM_STACK.pBase - HB_VM_STACK.pItems + 1 );
}

long hb_stackBaseProcOffset( int iLevel )
{
   HB_THREAD_STUB
   long lOffset = ( long ) ( HB_VM_STACK.pBase - HB_VM_STACK.pItems );

   while( iLevel-- > 0 && lOffset > 0 )
      lOffset = ( *( HB_VM_STACK.pItems + lOffset ) )->item.asSymbol.pCargo->stackbase;

   if( iLevel < 0 && ( lOffset > 0 || HB_IS_SYMBOL( *HB_VM_STACK.pItems ) ) )
      return lOffset;
   else
      return -1;
}

/**
   JC1: from that point on, stack optimization is no longer needed:
   We have all small functions that reference stack only once, and
   to do a simple operation, so putting them in the local function
   stack instead of calling the getCurrentStack() function directly
   is just a waste of time. For this reason, we reset the standard
   HB_VM_STACK.
 **/
#if defined( HB_THREAD_SUPPORT )
   #undef HB_VM_STACK
   #define HB_VM_STACK ( *hb_threadGetCurrentStack() )
#endif

#undef hb_stackTotalItems
long hb_stackTotalItems( void )
{
   return HB_VM_STACK.wItems;
}

PHB_IOERRORS hb_stackIOErrors( void )
{
#if defined( HB_THREAD_SUPPORT )
   if( hb_stack_ready )
      return &HB_VM_STACK.IOErrors;
#endif
   return &s_IOErrors;
}

#undef hb_stackRDD
PHB_STACKRDD hb_stackRDD( void )
{
   return HB_VM_STACK.rdd;
}

#undef hb_stackRDDTLS
PHB_STACKRDD_TLS hb_stackRDDTLS( void )
{
   return &HB_VM_STACK.rddTls;
}

#undef hb_stackGetStaticsBase
long hb_stackGetStaticsBase( void )
{
   return HB_VM_STACK.lStatics;
}

#undef hb_stackSetStaticsBase
void hb_stackSetStaticsBase( long lBase )
{
   HB_VM_STACK.lStatics = lBase;
}

#undef hb_stackItemBasePtr
PHB_ITEM ** hb_stackItemBasePtr( void )
{
   return &HB_VM_STACK.pItems;
}

#undef hb_stackItem
PHB_ITEM hb_stackItem( long iItemPos )
{
   if( iItemPos < 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return *( HB_VM_STACK.pItems + iItemPos );
}

#undef hb_stackItemFromTop
PHB_ITEM hb_stackItemFromTop( int nFromTop )
{
   if( nFromTop > 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return *( HB_VM_STACK.pPos + nFromTop );
}

#undef hb_stackItemFromBase
PHB_ITEM hb_stackItemFromBase( int nFromBase )
{
   if( nFromBase <= 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   /* printf( "Local %i Params: %i\n", nFromBase, hb_stackBaseItem()->item.asSymbol.pCargo->arguments );
    */

   return *( HB_VM_STACK.pBase + nFromBase + 1 );
}

#undef hb_stackLocalVariable
PHB_ITEM hb_stackLocalVariable( int * piFromBase )
{
   /* PHB_ITEM pBase = *HB_VM_STACK.pBase;
    */

   if( *piFromBase <= 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return *( HB_VM_STACK.pBase + *piFromBase + 1 );
}

#undef hb_stackTopItem
PHB_ITEM hb_stackTopItem( void )
{
   return *HB_VM_STACK.pPos;
}

#undef hb_stackBaseItem
PHB_ITEM hb_stackBaseItem( void )
{
   return *HB_VM_STACK.pBase;
}

/* Returns SELF object, an evaluated codeblock or NIL for normal func/proc
 */
#undef hb_stackSelfItem
PHB_ITEM hb_stackSelfItem( void )
{
   return *( HB_VM_STACK.pBase + 1 );
}

#undef hb_stackReturnItem
PHB_ITEM hb_stackReturnItem( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stackReturnItem()" ) );

   return &( HB_VM_STACK.Return );
}

#undef hb_stackDateBuffer
char * hb_stackDateBuffer( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_stackDateBuffer()" ) );

   return HB_VM_STACK.szDate;
}

PHB_ITEM * hb_stackGetBase( int iLevel )
{
   if( iLevel > 0 )
   {
      long lBase = ( *HB_VM_STACK.pBase )->item.asSymbol.pCargo->stackbase;

      while( ( --iLevel > 0 ) && ( lBase > 0 ) && HB_IS_SYMBOL( *( HB_VM_STACK.pItems + lBase ) ) )
         lBase = ( *( HB_VM_STACK.pItems + lBase ) )->item.asSymbol.pCargo->stackbase;

      if( iLevel == 0 && lBase >= 0 && HB_IS_SYMBOL( *( HB_VM_STACK.pItems + lBase ) ) )
         return HB_VM_STACK.pItems + lBase;
   }
   else if( iLevel == 0 )
      return HB_VM_STACK.pBase;

   /* hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
    */

   return NULL;
}

#undef hb_stackGetActionRequest
USHORT hb_stackGetActionRequest( void )
{
   return ( USHORT ) ( HB_VM_STACK.uiVMFlags & HB_REQUEST_MASK );
}

#undef hb_stackSetActionRequest
void hb_stackSetActionRequest( USHORT uiAction )
{
   HB_VM_STACK.uiVMFlags   &= ~HB_REQUEST_MASK;
   HB_VM_STACK.uiVMFlags   |= uiAction;
}

/* NOTE: DEBUG function */
void hb_stackDispLocal( void )
{
   PHB_ITEM * pBase;

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackDispLocal()" ) );

   printf( "%s", hb_conNewLine() );
   printf( HB_I_( "Virtual Machine Stack Dump at %s(%i):" ),
           ( *( HB_VM_STACK.pBase ) )->item.asSymbol.value->szName,
           ( *( HB_VM_STACK.pBase ) )->item.asSymbol.pCargo->lineno );
   printf( "%s", hb_conNewLine() );
   printf( "--------------------------" );

   for( pBase = HB_VM_STACK.pBase; pBase <= HB_VM_STACK.pPos; pBase++ )
   {
      printf( "%s", hb_conNewLine() );

      switch( hb_itemType( *pBase ) )
      {
         case HB_IT_NIL:
            printf( HB_I_( "NIL " ) );
            break;

         case HB_IT_ARRAY:
            if( hb_arrayIsObject( *pBase ) )
               printf( HB_I_( "OBJECT = %s " ), hb_objGetClsName( *pBase ) );
            else
               printf( HB_I_( "ARRAY " ) );
            break;

         case HB_IT_HASH:
            printf( HB_I_( "HASH " ) );
            break;

         case HB_IT_BLOCK:
            printf( HB_I_( "BLOCK " ) );
            break;

         case HB_IT_DATE:
         {
            char szDate[ 9 ];
            printf( HB_I_( "DATE = \"%s\" " ), hb_itemGetDS( *pBase, szDate ) );
            break;
         }

         case HB_IT_TIMEFLAG:
         {
            char szDate[ 19 ];
            printf( HB_I_( "DATETIME = \"%s\" " ), hb_itemGetDTS( *pBase, szDate ) );
            break;
         }

         case HB_IT_DOUBLE:
            printf( HB_I_( "DOUBLE = %f " ), hb_itemGetND( *pBase ) );
            break;

         case HB_IT_LOGICAL:
            printf( HB_I_( "LOGICAL = %s " ), hb_itemGetL( *pBase ) ? ".T." : ".F." );
            break;

         case HB_IT_LONG:
            printf( HB_I_( "LONG = %" PFHL "i " ), hb_itemGetNInt( *pBase ) );
            break;

         case HB_IT_INTEGER:
            printf( HB_I_( "INTEGER = %i " ), hb_itemGetNI( *pBase ) );
            break;

         case HB_IT_STRING:
            printf( HB_I_( "STRING = \"%s\" " ), hb_itemGetCPtr( *pBase ) );
            break;

         case HB_IT_SYMBOL:
            printf( HB_I_( "SYMBOL = %s " ), ( *pBase )->item.asSymbol.value->szName );
            break;

         case HB_IT_POINTER:
            printf( HB_I_( "POINTER = %p " ), ( *pBase )->item.asPointer.value );
            break;

         default:
            printf( HB_I_( "UNKNOWN = TYPE %i " ), hb_itemType( *pBase ) );
            break;
      }
   }
}

void hb_stackDispCall( void )
{
   PHB_ITEM *  pBase;
   char        buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_stackDispCall()" ) );

   if( HB_VM_STACK.pPos == HB_VM_STACK.pItems )
   {
      hb_snprintf( buffer, sizeof( buffer ), "Called from hb_vmQuit()" );
      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      return;
   }

   pBase = HB_VM_STACK.pBase;

   do
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      if( HB_IS_ARRAY( *( pBase + 1 ) ) )
         hb_snprintf( buffer, sizeof( buffer ), HB_I_( "Called from %s:%s(%i)" ), hb_objGetClsName( *( pBase + 1 ) ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.pCargo->lineno );
      else
         hb_snprintf( buffer, sizeof( buffer ), HB_I_( "Called from %s(%i)" ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.pCargo->lineno );

      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );

      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;
   }
   while( pBase > HB_VM_STACK.pItems );
}

#if defined( HB_INCLUDE_WINEXCHANDLER ) && defined( HB_OS_WIN )
LONG WINAPI hb_UnhandledExceptionFilter( struct _EXCEPTION_POINTERS * ExceptionInfo )
{
   PHB_ITEM *  pBase = HB_VM_STACK.pBase;
   char        msg[ ( HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ) * 32 ];

   HB_SYMBOL_UNUSED( ExceptionInfo );

   msg[ 0 ] = '\0';

   do
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      if( HB_IS_ARRAY( *( pBase + 1 ) ) )
         hb_snprintf( buffer, sizeof( buffer ), HB_I_( "Called from %s:%s(%i)\n" ), hb_objGetClsName( *( pBase + 1 ) ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.pCargo->lineno );
      else
         hb_snprintf( buffer, sizeof( buffer ), HB_I_( "Called from %s(%i)\n" ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.pCargo->lineno );

      strcat( msg, buffer );

      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;
   }
   while( pBase != HB_VM_STACK.pItems );

   MessageBox( NULL, msg, HB_I_( "Harbour Exception" ), MB_ICONSTOP );

   return EXCEPTION_EXECUTE_HANDLER; /* EXCEPTION_CONTINUE_SEARCH; */
}
#endif

#if defined( HB_OS_OS2 )
ULONG _System OS2TermHandler( PEXCEPTIONREPORTRECORD p1,
                              PEXCEPTIONREGISTRATIONRECORD p2,
                              PCONTEXTRECORD p3,
                              PVOID pv )
{

   PHB_ITEM * pBase = HB_VM_STACK.pBase;

   HB_SYMBOL_UNUSED( p1 );
   HB_SYMBOL_UNUSED( p2 );
   HB_SYMBOL_UNUSED( p3 );
   HB_SYMBOL_UNUSED( pv );

   /* Don't print stack trace if inside unwind, normal process termination or process killed or
      during debugging */
   if( p1->ExceptionNum != XCPT_UNWIND && p1->ExceptionNum < XCPT_BREAKPOINT )
   {
      fprintf( stderr, HB_I_( "\nException %lx at address %lx \n" ), p1->ExceptionNum, ( ULONG ) p1->ExceptionAddress );

      do
      {
         if( HB_IS_ARRAY( *( pBase + 1 ) ) )
            fprintf( stderr, HB_I_( "Called from %s:%s(%i)\n" ), hb_objGetClsName( *( pBase + 1 ) ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.pCargo->lineno );
         else
            fprintf( stderr, HB_I_( "Called from %s(%i)\n" ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.pCargo->lineno );

         pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.pCargo->stackbase;
      }
      while( pBase != HB_VM_STACK.pItems );
   }

   return XCPT_CONTINUE_SEARCH;          /* Exception not resolved... */
}
#endif

HB_EXTERN_END
