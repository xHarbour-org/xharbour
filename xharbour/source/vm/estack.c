/*
 * $Id: estack.c,v 1.17 2002/12/19 18:15:35 ronpinkas Exp $
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
 */

#if defined(HB_INCLUDE_WINEXCHANDLER)
   #define HB_OS_WIN_32_USED
#endif

#include "hbapi.h"
#include "hbdefs.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbapierr.h"

/* ------------------------------- */

HB_STACK hb_stack;

#ifdef HB_THREAD_SUPPORT
   #include "thread.h"

   HB_STACK *hb_getCurrentStack( void )
   {
      // Most common first.
      if( hb_ht_context == NULL )
      {
         return &hb_stack;
      }
      else
      {
         //static HB_THREAD_CONTEXT *last_context = NULL;
         HB_THREAD_CONTEXT *p;
         HB_THREAD_T id;

         id = HB_CURRENT_THREAD();

         /*
         if ( last_context != NULL && last_context->th_id == id )
         {
            return last_context->stack;
         }
         */

         HB_MUTEX_LOCK( &context_monitor );

         p = hb_ht_context;
         while( p && p->th_id != id )
         {
            p = p->next;
         }

         if( p )
         {
            //last_context = p;
            HB_MUTEX_UNLOCK( &context_monitor );
            return p->stack;
         }
         else
         {
            // TODO: Add Error Message.
            HB_MUTEX_UNLOCK( &context_monitor );
            return &hb_stack;
         }
      }
   }
#endif

/* ------------------------------- */

void hb_stackPop( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPop()"));

   if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems )
   {
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
   }

   if( HB_IS_COMPLEX( *HB_VM_STACK.pPos ) )
   {
      hb_itemClear( *HB_VM_STACK.pPos );
   }
   else
   {
      ( *HB_VM_STACK.pPos )->type = HB_IT_NIL;
   }
}

void hb_stackDec( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDec()"));

   if( --HB_VM_STACK.pPos < HB_VM_STACK.pItems )
   {
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
   }
}

void hb_stackFree( void )
{
   LONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackFree()"));

   i = HB_VM_STACK.wItems - 1;
   while( i >= 0 )
   {
      hb_xfree( HB_VM_STACK.pItems[ i-- ] );
   }
   hb_xfree( HB_VM_STACK.pItems );
}

void hb_stackPush( void )
{
   LONG CurrIndex;   /* index of current top item */
   LONG TopIndex;    /* index of the topmost possible item */
   LONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackPush()"));

   CurrIndex = HB_VM_STACK.pPos - HB_VM_STACK.pItems;
   TopIndex  = HB_VM_STACK.wItems - 1;

   /* enough room for another item ? */
   if( !( TopIndex > CurrIndex ) )
   {
      LONG BaseIndex;   /* index of stack base */

      BaseIndex = HB_VM_STACK.pBase - HB_VM_STACK.pItems;

      /* no, make more headroom: */
      /* hb_stackDispLocal(); */
      HB_VM_STACK.pItems = ( HB_ITEM_PTR * ) hb_xrealloc( ( void *)HB_VM_STACK.pItems, sizeof( HB_ITEM_PTR ) *
                                ( HB_VM_STACK.wItems + STACK_EXPANDHB_ITEMS ) );

      /* fix possibly invalid pointers: */
      HB_VM_STACK.pPos = HB_VM_STACK.pItems + CurrIndex;
      HB_VM_STACK.pBase = HB_VM_STACK.pItems + BaseIndex;
      HB_VM_STACK.wItems += STACK_EXPANDHB_ITEMS;

      for( i=CurrIndex + 1; i < HB_VM_STACK.wItems; ++i )
      {
         HB_VM_STACK.pItems[ i ] = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) );
      }
      /* hb_stackDispLocal(); */
   }

   /* now, push it: */
   HB_VM_STACK.pPos++;

   ( * HB_VM_STACK.pPos )->type = HB_IT_NIL;
}

void hb_stackInit( void )
{
   LONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackInit()"));

   // Optimized to directly accessing hb_stack instead of HB_VM_STACK
   hb_stack.pItems = ( HB_ITEM_PTR * ) hb_xgrab( sizeof( HB_ITEM_PTR ) * STACK_INITHB_ITEMS );
   hb_stack.pBase  = hb_stack.pItems;
   hb_stack.pPos   = hb_stack.pItems;     /* points to the first stack item */
   hb_stack.wItems = STACK_INITHB_ITEMS;

   for( i=0; i < hb_stack.wItems; ++i )
   {
      hb_stack.pItems[ i ] = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) );
   }

   ( * hb_stack.pPos )->type = HB_IT_NIL;
}

void hb_stackRemove( LONG lUntilPos )
{
   HB_ITEM_PTR * pEnd = HB_VM_STACK.pItems + lUntilPos;

   // Optimized to directly accessing hb_stack instead of HB_VM_STACK
   while( hb_stack.pPos > pEnd )
   {
      hb_stackPop();
   }
}

HB_ITEM_PTR hb_stackNewFrame( HB_STACK_STATE * pStack, USHORT uiParams )
{
   HB_ITEM_PTR pItem = hb_stackItemFromTop( - uiParams - 2 );   /* procedure name */

   if( ! HB_IS_SYMBOL( pItem ) )
   {
      /* QUESTION: Is this call needed ? [vszakats] */
      hb_stackDispLocal();
      hb_errInternal( HB_EI_VMNOTSYMBOL, NULL, "hb_vmDo()", NULL );
   }

   pStack->lBaseItem = HB_VM_STACK.pBase - HB_VM_STACK.pItems;
   pStack->iStatics = HB_VM_STACK.iStatics;

   pItem->item.asSymbol.lineno = 0;
   pItem->item.asSymbol.paramcnt = uiParams;
   HB_VM_STACK.pBase = HB_VM_STACK.pItems + pItem->item.asSymbol.stackbase;
   pItem->item.asSymbol.stackbase = pStack->lBaseItem;

   return pItem;
}

void hb_stackOldFrame( HB_STACK_STATE * pStack )
{
   while( HB_VM_STACK.pPos > HB_VM_STACK.pBase )
   {
      hb_stackPop();
   }

   HB_VM_STACK.pBase = HB_VM_STACK.pItems + pStack->lBaseItem;
   HB_VM_STACK.iStatics = pStack->iStatics;
}

#undef hb_stackItem
HB_ITEM_PTR HB_EXPORT hb_stackItem( LONG iItemPos )
{
   if( iItemPos < 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return ( * ( HB_VM_STACK.pItems + iItemPos ) );
}

#undef hb_stackItemFromTop
HB_ITEM_PTR HB_EXPORT hb_stackItemFromTop( int nFromTop )
{
   if( nFromTop > 0 )
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );

   return ( * ( HB_VM_STACK.pPos + nFromTop ) );
}

#undef hb_stackItemFromBase
HB_ITEM_PTR HB_EXPORT hb_stackItemFromBase( int nFromBase )
{
   if( nFromBase <= 0 )
   {
      hb_errInternal( HB_EI_STACKUFLOW, NULL, NULL, NULL );
   }

   //printf( "Local %i Params: %i\n", nFromBase, hb_stackBaseItem()->item.asSymbol.paramcnt );

   if( hb_stackBaseItem()->item.asSymbol.paramcnt < 255 )
   {
      return ( * ( HB_VM_STACK.pBase + nFromBase + 1 ) );
   }
   else
   {
      return ( * ( HB_VM_STACK.pBase + nFromBase + 1 + hb_stackBaseItem()->item.asSymbol.paramcnt - 256 ) );
   }
}

#undef hb_stackTopItem
HB_ITEM_PTR HB_EXPORT hb_stackTopItem( void )
{
    return * HB_VM_STACK.pPos;
}

#undef hb_stackBaseItem
HB_ITEM_PTR HB_EXPORT hb_stackBaseItem( void )
{
   return * HB_VM_STACK.pBase;
}

/* Returns SELF object, an evaluated codeblock or NIL for normal func/proc
*/
#undef hb_stackSelfItem
HB_ITEM_PTR HB_EXPORT hb_stackSelfItem( void )
{
   return * ( HB_VM_STACK.pBase + 1 );
}

#undef hb_stackTopOffset
LONG HB_EXPORT hb_stackTopOffset( void )
{
   return HB_VM_STACK.pPos - HB_VM_STACK.pItems;
}

#undef hb_stackBaseOffset
LONG HB_EXPORT hb_stackBaseOffset( void )
{
   return HB_VM_STACK.pBase - HB_VM_STACK.pItems + 1;
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

         case HB_IT_BLOCK:
            printf( HB_I_("BLOCK ") );
            break;

         case HB_IT_DATE:
            {
               char szDate[ 9 ];
               printf( HB_I_("DATE = \"%s\" "), hb_itemGetDS( *pBase, szDate ) );
            }
            break;

         case HB_IT_DOUBLE:
            printf( HB_I_("DOUBLE = %f "), hb_itemGetND( *pBase ) );
            break;

         case HB_IT_LOGICAL:
            printf( HB_I_("LOGICAL = %s "), hb_itemGetL( *pBase ) ? ".T." : ".F." );
            break;

         case HB_IT_LONG:
            printf( HB_I_("LONG = %lu "), hb_itemGetNL( *pBase ) );
            break;

         case HB_IT_INTEGER:
            printf( HB_I_("INTEGER = %i "), hb_itemGetNI( *pBase ) );
            break;

         case HB_IT_STRING:
            printf( HB_I_("STRING = \"%s\" "), hb_itemGetCPtr( *pBase ) );
            break;

         case HB_IT_SYMBOL:
            printf( HB_I_("SYMBOL = %s "), ( *pBase )->item.asSymbol.value->szName );
            break;

         default:
            printf( HB_I_("UNKNOWN = TYPE %i "), hb_itemType( *pBase ) );
            break;
      }
   }
}

void hb_stackDispCall( void )
{
   PHB_ITEM * pBase = HB_VM_STACK.pBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispCall()"));

   while( pBase != HB_VM_STACK.pItems )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;

      if( ( *( pBase + 1 ) )->type == HB_IT_ARRAY )
         sprintf( buffer, HB_I_("Called from %s:%s(%i)"), hb_objGetClsName( *(pBase + 1) ),
            ( *pBase )->item.asSymbol.value->szName,
            ( *pBase )->item.asSymbol.lineno );
      else
         sprintf( buffer, HB_I_("Called from %s(%i)"),
            ( *pBase )->item.asSymbol.value->szName,
            ( *pBase )->item.asSymbol.lineno );

      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }
}

/* ------------------------------------------------------------------------ */
/* The garbage collector interface */
/* ------------------------------------------------------------------------ */

/* Mark all locals as used so they will not be released by the
 * garbage collector
 */
void hb_vmIsLocalRef( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsLocalRef()"));

   if( HB_VM_STACK.pPos > HB_VM_STACK.pItems )
   {
      /* the eval stack is not cleared yet */
      HB_ITEM_PTR * pItem = HB_VM_STACK.pPos - 1;

      while( pItem != HB_VM_STACK.pItems )
      {
         if( ( *pItem )->type & (HB_IT_BYREF | HB_IT_POINTER | HB_IT_ARRAY | HB_IT_BLOCK) )
            hb_gcItemRef( *pItem );
         --pItem;
      }
   }
}

#ifdef HB_INCLUDE_WINEXCHANDLER

#if defined(HB_OS_WIN_32)

WINBASEAPI LONG WINAPI UnhandledExceptionFilter( struct _EXCEPTION_POINTERS * ExceptionInfo )
{
   PHB_ITEM *pBase = HB_VM_STACK.pBase;

   char msg[ ( HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ) * 32 ];

   HB_SYMBOL_UNUSED( ExceptionInfo );

   msg[ 0 ] = '\0';

   do
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      if( ( *( pBase + 1 ) )->type == HB_IT_ARRAY )
         sprintf( buffer, HB_I_("Called from %s:%s(%i)\n"), hb_objGetClsName( *(pBase + 1) ),
            ( *pBase )->item.asSymbol.value->szName,
            ( *pBase )->item.asSymbol.lineno );
      else
         sprintf( buffer, HB_I_("Called from %s(%i)\n"),
            ( *pBase )->item.asSymbol.value->szName,
            ( *pBase )->item.asSymbol.lineno );

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
         if( ( *( pBase + 1 ) )->type == HB_IT_ARRAY )
            fprintf( stderr, HB_I_("Called from %s:%s(%i)\n"), hb_objGetClsName( *(pBase + 1) ),
                     ( *pBase )->item.asSymbol.value->szName,
                     ( *pBase )->item.asSymbol.lineno );
         else
            fprintf( stderr, HB_I_("Called from %s(%i)\n"),
                     ( *pBase )->item.asSymbol.value->szName,
                     ( *pBase )->item.asSymbol.lineno );

         pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
      }
      while( pBase != HB_VM_STACK.pItems );
   }

   return XCPT_CONTINUE_SEARCH;          /* Exception not resolved... */
}
#endif
