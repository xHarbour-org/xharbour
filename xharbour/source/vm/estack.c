/*
 * $Id: estack.c,v 1.87 2007/04/08 07:20:56 ronpinkas Exp $
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

#define HB_OS_WIN_32_USED

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbdefs.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbapierr.h"
#include "hbvm.h"

HB_EXTERN_BEGIN

/* ------------------------------- */

#ifdef HB_THREAD_SUPPORT
HB_STACK hb_stackMT;
#else
HB_STACK hb_stackST;
HB_STACK * hb_stack_ptr = &hb_stackST;
#endif

HB_EXPORT BOOL hb_stack_ready = FALSE;

/* ------------------------------- */

#undef hb_stackPop
HB_EXPORT void hb_stackPop( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_stackPop()"));

   if( HB_IS_COMPLEX( *( HB_VM_STACK.pPos - 1 ) ) )
   {
      hb_itemClear( *( HB_VM_STACK.pPos - 1 ) );
   }
   else
   {
      ( *( HB_VM_STACK.pPos - 1 ) )->type = HB_IT_NIL;
   }

   if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems )
   {
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
   }
}

#undef hb_stackDec
HB_EXPORT void hb_stackDec( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_stackDec()"));

   if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems )
   {
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
   }
}

#undef hb_stackPush
HB_EXPORT void hb_stackPush( void )
{
   LONG CurrIndex;   /* index of current top item */
   LONG TopIndex;    /* index of the topmost possible item */

   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_stackPush()"));

   CurrIndex = HB_VM_STACK.pPos - HB_VM_STACK.pItems;
   TopIndex  = HB_VM_STACK.wItems - 1;

   /* enough room for another item ? */
   if( !( TopIndex > CurrIndex ) )
   {
      #ifndef HB_ARRAY_USE_COUNTER
         PHB_ITEM *pOldItems = HB_VM_STACK.pItems;
      #endif

      LONG BaseIndex;   /* index of stack base */
      LONG i;

      BaseIndex = HB_VM_STACK.pBase - HB_VM_STACK.pItems;

      /* no, make more headroom: */
      /* hb_stackDispLocal(); */
      HB_VM_STACK.pItems = ( HB_ITEM_PTR * ) hb_xrealloc( (void *) HB_VM_STACK.pItems, sizeof( HB_ITEM_PTR ) *
                                ( HB_VM_STACK.wItems + STACK_EXPANDHB_ITEMS ) );

      /* fix possibly invalid pointers: */
      HB_VM_STACK.pPos = HB_VM_STACK.pItems + CurrIndex;
      HB_VM_STACK.pBase = HB_VM_STACK.pItems + BaseIndex;
      HB_VM_STACK.wItems += STACK_EXPANDHB_ITEMS;

      #ifndef HB_ARRAY_USE_COUNTER
         if( HB_VM_STACK.pItems != pOldItems )
         {
            //TraceLog( NULL, "Expanding Stack: %p\n", pOldItems );

            for( i = 0; i < CurrIndex; i++ )
            {
               if( HB_VM_STACK.pItems[ i ]->type == HB_IT_ARRAY && HB_VM_STACK.pItems[ i ]->item.asArray.value )
               {
                  hb_arrayResetHolder( HB_VM_STACK.pItems[ i ]->item.asArray.value, (void *) ( pOldItems[i] ), (void *) ( HB_VM_STACK.pItems[i] ) );
               }
               else if( HB_VM_STACK.pItems[ i ]->type == HB_IT_BYREF && HB_VM_STACK.pItems[ i ]->item.asRefer.offset == 0 )
               {
                  hb_arrayResetHolder( HB_VM_STACK.pItems[ i ]->item.asRefer.BasePtr.pBaseArray, (void *) ( pOldItems[i] ), (void *) ( HB_VM_STACK.pItems[i] ) );
               }
            }

            //TraceLog( NULL, "New Stack: %p\n", HB_VM_STACK.pItems );
         }
      #endif

      for( i = CurrIndex + 1; i < HB_VM_STACK.wItems; i++ )
      {
         HB_VM_STACK.pItems[ i ] = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) );
      }
      /* hb_stackDispLocal(); */
   }

   /* now, push it: */
   HB_VM_STACK.pPos++;

   ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;
}

void hb_stackIncrease( void )
{
   LONG BaseIndex;   /* index of stack base */
   LONG CurrIndex;   /* index of current top item */
   LONG i;

   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_stackIncrease()"));

   BaseIndex = HB_VM_STACK.pBase - HB_VM_STACK.pItems;
   CurrIndex = HB_VM_STACK.pPos - HB_VM_STACK.pItems;

   /* no, make more headroom: */
   /* hb_stackDispLocal(); */
   HB_VM_STACK.pItems = ( HB_ITEM_PTR * ) hb_xrealloc( ( void *) HB_VM_STACK.pItems, sizeof( HB_ITEM_PTR ) *
                             ( HB_VM_STACK.wItems + STACK_EXPANDHB_ITEMS ) );

   /* fix possibly modified by realloc pointers: */
   HB_VM_STACK.pPos = HB_VM_STACK.pItems + CurrIndex;
   HB_VM_STACK.pBase = HB_VM_STACK.pItems + BaseIndex;
   HB_VM_STACK.wItems += STACK_EXPANDHB_ITEMS;
   for( i = CurrIndex + 1; i < HB_VM_STACK.wItems; ++i )
      HB_VM_STACK.pItems[ i ] = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) );
}

void hb_stackInit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackInit()"));

#ifndef HB_THREAD_SUPPORT

   hb_stackST.pItems = ( HB_ITEM_PTR * ) hb_xgrab( sizeof( HB_ITEM_PTR ) * STACK_INITHB_ITEMS );
   hb_stackST.pBase  = hb_stackST.pItems;
   hb_stackST.pPos   = hb_stackST.pItems;     /* points to the first stack item */
   hb_stackST.wItems = STACK_INITHB_ITEMS;

   {
      LONG i;
      for( i = 0; i < hb_stackST.wItems; ++i )
      {
         hb_stackST.pItems[ i ] = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) );
      }
   }
   ( * hb_stackST.pPos )->type = HB_IT_NIL;
   hb_stackST.Return.type = HB_IT_NIL;

#else

   hb_threadSetupStack( &hb_stackMT, HB_CURRENT_THREAD() );

#endif

   hb_stack_ready = TRUE;
}

void hb_stackFree( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackFree()"));

   hb_stack_ready = FALSE;

#ifndef HB_THREAD_SUPPORT

   {
      LONG i = hb_stackST.wItems - 1;
      while( i >= 0 )
      {
         hb_xfree( hb_stackST.pItems[ i-- ] );
      }
   }
   hb_xfree( hb_stackST.pItems );

   hb_stackST.pItems = NULL;
   hb_stackST.pBase  = NULL;
   hb_stackST.pPos   = NULL;
   hb_stackST.wItems = 0;

#else

   hb_threadDestroyStack( &hb_stackMT );

#endif
}

void hb_stackRemove( LONG lUntilPos )
{
   HB_THREAD_STUB
   HB_ITEM_PTR * pEnd = HB_VM_STACK.pItems + lUntilPos;

   while( HB_VM_STACK.pPos > pEnd )
   {
      hb_stackPop();
   }
}

HB_EXPORT HB_ITEM_PTR hb_stackNewFrame( HB_STACK_STATE * pStack, USHORT uiParams )
{
   HB_THREAD_STUB

   PHB_ITEM *pBase = HB_VM_STACK.pPos - uiParams - 2;
   PHB_ITEM pItem = *pBase;   /* procedure name */

   if( ! HB_IS_SYMBOL( pItem ) )
   {
      /* QUESTION: Is this call needed ? [vszakats] */
      hb_stackDispLocal();
      hb_errInternal( HB_EI_VMNOTSYMBOL, NULL, "hb_vmDo()", NULL );
   }

   pStack->iStatics = HB_VM_STACK.iStatics;

   pItem->item.asSymbol.stackbase = HB_VM_STACK.pBase - HB_VM_STACK.pItems;
   pItem->item.asSymbol.lineno = 0;
   pItem->item.asSymbol.paramcnt = uiParams;
   pItem->item.asSymbol.paramsoffset = 0;

   HB_VM_STACK.pBase = pBase;

   return pItem;
}

HB_EXPORT void hb_stackOldFrame( HB_STACK_STATE * pStack )
{
   HB_THREAD_STUB
   int iLocal;
   PHB_ITEM pDetached;
   USHORT uiRequest;
   LONG stackbase = (*HB_VM_STACK.pBase)->item.asSymbol.stackbase;

   uiRequest = hb_vmRequestQuery();
   hb_vmRequestReset();

   while( HB_VM_STACK.pPos > HB_VM_STACK.pBase )
   {
      PHB_ITEM pItem = *( HB_VM_STACK.pPos - 1 );

      iLocal = HB_VM_STACK.pPos - HB_VM_STACK.pBase - 2;

      if( iLocal >= 0 && iLocal <= (*HB_VM_STACK.pBase)->item.asSymbol.paramcnt && HB_IS_MEMVAR( pItem ) )
      {
         //printf( "Func: %s Params: %i Local %i Type: %i\n", (*HB_VM_STACK.pBase)->item.asSymbol.value->szName, (*HB_VM_STACK.pBase)->item.asSymbol.paramcnt, iLocal, pItem->type );

         pDetached = hb_itemUnRefOnce( pItem );

         //printf( "   Func: %s Params: %i Local %i UnRef Type: %i\n", (*HB_VM_STACK.pBase)->item.asSymbol.value->szName, (*HB_VM_STACK.pBase)->item.asSymbol.paramcnt, iLocal, pDetached->type );

         if( HB_IS_BYREF( pDetached ) )
         {
            hb_itemCopy( pDetached, hb_itemUnRef( pDetached ) );
            //printf( "Severed Detached Local: %i Type: %i\n", iLocal, pDetached->type );
         }

         hb_itemClear( pItem );
      }
      else if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
      else
      {
         pItem->type = HB_IT_NIL;
      }

      --HB_VM_STACK.pPos;
   }

   HB_VM_STACK.pBase = HB_VM_STACK.pItems + stackbase;
   HB_VM_STACK.iStatics = pStack->iStatics;

   hb_vmRequest( uiRequest );
}

#undef hb_stackTopOffset
HB_EXPORT LONG hb_stackTopOffset( void )
{
   HB_THREAD_STUB
   return HB_VM_STACK.pPos - HB_VM_STACK.pItems;
}

#undef hb_stackBaseOffset
HB_EXPORT LONG hb_stackBaseOffset( void )
{
   HB_THREAD_STUB
   return HB_VM_STACK.pBase - HB_VM_STACK.pItems + 1;
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
   #define HB_VM_STACK (* hb_threadGetCurrentStack() )
#endif

#undef hb_stackItem
HB_EXPORT HB_ITEM_PTR hb_stackItem( LONG iItemPos )
{
   if( iItemPos < 0 )
   {
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
   }

   return ( * ( HB_VM_STACK.pItems + iItemPos ) );
}

#undef hb_stackItemFromTop
HB_EXPORT HB_ITEM_PTR hb_stackItemFromTop( int nFromTop )
{
   if( nFromTop > 0 )
   {
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
   }

   return ( *( HB_VM_STACK.pPos + nFromTop ) );
}

#undef hb_stackItemFromBase
HB_EXPORT HB_ITEM_PTR hb_stackItemFromBase( int nFromBase )
{
   if( nFromBase <= 0 )
   {
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
   }

   //printf( "Local %i Params: %i\n", nFromBase, hb_stackBaseItem()->item.asSymbol.paramcnt );

   return ( * ( HB_VM_STACK.pBase + nFromBase + 1 ) );
}

#undef hb_stackTopItem
HB_EXPORT HB_ITEM_PTR hb_stackTopItem( void )
{
    return * HB_VM_STACK.pPos;
}

#undef hb_stackBaseItem
HB_EXPORT HB_ITEM_PTR hb_stackBaseItem( void )
{
   return * HB_VM_STACK.pBase;
}

/* Returns SELF object, an evaluated codeblock or NIL for normal func/proc
*/
#undef hb_stackSelfItem
HB_EXPORT HB_ITEM_PTR hb_stackSelfItem( void )
{
   return * ( HB_VM_STACK.pBase + 1 );
}

#undef hb_stackReturnItem
HB_EXPORT HB_ITEM_PTR hb_stackReturnItem( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackReturnItem()"));

   return &( HB_VM_STACK.Return );
}

#undef hb_stackDateBuffer
HB_EXPORT char * hb_stackDateBuffer( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDateBuffer()"));

   return HB_VM_STACK.szDate;
}

HB_EXPORT PHB_ITEM * hb_stackGetBase( int iLevel )
{
   if( iLevel > 0 )
   {
      LONG lBase = (* HB_VM_STACK.pBase )->item.asSymbol.stackbase;

      while( ( --iLevel > 0 ) && ( lBase > 0 ) && HB_IS_SYMBOL( *( HB_VM_STACK.pItems + lBase ) ) )
      {
         lBase = ( *( HB_VM_STACK.pItems + lBase ) )->item.asSymbol.stackbase;
      }

      if( iLevel == 0 && lBase >= 0 && HB_IS_SYMBOL( *( HB_VM_STACK.pItems + lBase ) ) )
      {
         return HB_VM_STACK.pItems + lBase;
      }
   }
   else if( iLevel == 0 )
   {
      return HB_VM_STACK.pBase;
   }

   //hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return NULL;
}


/* NOTE: DEBUG function */
void hb_stackDispLocal( void )
{
   PHB_ITEM * pBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispLocal()"));

   printf( hb_conNewLine() );
   printf( HB_I_("Virtual Machine Stack Dump at %s(%i):"), ( *(HB_VM_STACK.pBase) )->item.asSymbol.value->szName, ( *(HB_VM_STACK.pBase) )->item.asSymbol.lineno );
   printf( hb_conNewLine() );
   printf( "--------------------------" );

   for( pBase = HB_VM_STACK.pBase; pBase <= HB_VM_STACK.pPos; pBase++ )
   {
      printf( hb_conNewLine() );

      switch( hb_itemType( *pBase ) )
      {
         case HB_IT_NIL:
            printf( HB_I_("NIL ") );
            break;

         case HB_IT_ARRAY:
            if( hb_arrayIsObject( *pBase ) )
               printf( HB_I_("OBJECT = %s "), hb_objGetClsName( *pBase ) );
            else
               printf( HB_I_("ARRAY ") );
            break;

         case HB_IT_HASH:
               printf( HB_I_("HASH ") );
            break;

         case HB_IT_BLOCK:
            printf( HB_I_("BLOCK ") );
            break;

         case HB_IT_DATE:
            if( (*pBase)->item.asDate.time == 0 )
            {
               char szDate[ 9 ];
               printf( HB_I_("DATE = \"%s\" "), hb_itemGetDS( *pBase, szDate ) );
            }
            else
            {
               char szDate[ 19 ];
               printf( HB_I_("DATETIME = \"%s\" "), hb_itemGetDTS( *pBase, szDate ) );
            }
            break;

         case HB_IT_DOUBLE:
            printf( HB_I_("DOUBLE = %f "), hb_itemGetND( *pBase ) );
            break;

         case HB_IT_LOGICAL:
            printf( HB_I_("LOGICAL = %s "), hb_itemGetL( *pBase ) ? ".T." : ".F." );
            break;

         case HB_IT_INTEGER:
            printf( HB_I_("INTEGER = %i "), hb_itemGetNI( *pBase ) );
            break;

         case HB_IT_LONG:
            printf( HB_I_("LONG = %" PFHL "i "), hb_itemGetNInt( *pBase ) );
            break;

         case HB_IT_STRING:
            printf( HB_I_("STRING = \"%s\" "), ( *pBase )->item.asString.value );
            break;

         case HB_IT_SYMBOL:
            printf( HB_I_("SYMBOL = %s "), ( *pBase )->item.asSymbol.value->szName );
            break;

         case HB_IT_POINTER:
            printf( HB_I_("POINTER = %p "), ( *pBase )->item.asPointer.value );
            break;

         default:
            printf( HB_I_("UNKNOWN = TYPE %i "), hb_itemType( *pBase ) );
            break;
      }
   }
}

void hb_stackDispCall( void )
{
   PHB_ITEM *pBase;
   char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispCall()"));

   if( HB_VM_STACK.pPos == HB_VM_STACK.pItems )
   {
      sprintf( buffer, "Called from hb_vmQuit()" );
      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
	  return;
   }

   pBase = HB_VM_STACK.pBase;

   do
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      if( HB_IS_ARRAY( *( pBase + 1 ) ) )
      {
         sprintf( buffer, HB_I_("Called from %s:%s(%i)"), hb_objGetClsName( *(pBase + 1) ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.lineno );
      }
      else
      {
         sprintf( buffer, HB_I_("Called from %s(%i)"), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.lineno );
      }

      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );

      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
   }
   while( pBase > HB_VM_STACK.pItems );
}

#ifdef HB_INCLUDE_WINEXCHANDLER

#if defined(HB_OS_WIN_32)

LONG WINAPI hb_UnhandledExceptionFilter( struct _EXCEPTION_POINTERS * ExceptionInfo )
{
   PHB_ITEM *pBase = HB_VM_STACK.pBase;

   char msg[ ( HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ) * 32 ];

   HB_SYMBOL_UNUSED( ExceptionInfo );

   msg[ 0 ] = '\0';

   do
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      if( HB_IS_ARRAY( *( pBase + 1 ) ) )
      {
         sprintf( buffer, HB_I_("Called from %s:%s(%i)\n"), hb_objGetClsName( *(pBase + 1) ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.lineno );
      }
      else
      {
         sprintf( buffer, HB_I_("Called from %s(%i)\n"), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.lineno );
      }

      strcat( msg, buffer );

      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
   }
   while( pBase != HB_VM_STACK.pItems );

   MessageBox( NULL, msg, HB_I_("Harbour Exception"), MB_ICONSTOP );

   return EXCEPTION_EXECUTE_HANDLER; /* EXCEPTION_CONTINUE_SEARCH; */
}

#endif

#endif

#if defined(HB_OS_OS2)

ULONG _System OS2TermHandler(PEXCEPTIONREPORTRECORD       p1,
                             PEXCEPTIONREGISTRATIONRECORD p2,
                             PCONTEXTRECORD               p3,
                             PVOID                        pv) {

   PHB_ITEM *pBase = HB_VM_STACK.pBase;

   HB_SYMBOL_UNUSED(p1);
   HB_SYMBOL_UNUSED(p2);
   HB_SYMBOL_UNUSED(p3);
   HB_SYMBOL_UNUSED(pv);

   /* Don't print stack trace if inside unwind, normal process termination or process killed or
      during debugging */
   if (p1->ExceptionNum != XCPT_UNWIND && p1->ExceptionNum < XCPT_BREAKPOINT) {

      fprintf(stderr, HB_I_("\nException %lx at address %lx \n"), p1->ExceptionNum, (ULONG)p1->ExceptionAddress);

      do
      {
         if( HB_IS_ARRAY( *( pBase + 1 ) ) )
         {
            fprintf( stderr, HB_I_("Called from %s:%s(%i)\n"), hb_objGetClsName( *(pBase + 1) ), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.lineno );
         }
         else
         {
            fprintf( stderr, HB_I_("Called from %s(%i)\n"), ( *pBase )->item.asSymbol.value->szName, ( *pBase )->item.asSymbol.lineno );
         }

         pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
      }
      while( pBase != HB_VM_STACK.pItems );
   }

   return XCPT_CONTINUE_SEARCH;          /* Exception not resolved... */
}
#endif

HB_EXTERN_END
