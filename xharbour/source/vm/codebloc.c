/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Codeblock runtime support
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

/* The Harbour implementation of codeblocks */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapierr.h"

/* Creates the codeblock structure
 *
 * pBuffer -> the buffer with pcodes (without HB_P_PUSHBLOCK)
 * uiLocals -> number of local variables referenced in a codeblock
 * pLocalPosTable -> a table with positions on eval stack for referenced variables
 * pSymbol -> a pointer to the Symbol where block was defined
 *
 * Note: pLocalPosTable cannot be used if uiLocals is ZERO
 *
 * Another note:
 *       pLocalPosTable actually resides in the pcode before pBuffer, and we
 *       must use HB_PCODE_MKUSHORT( pLocalPosTable++ ) to handle
 *       little-endian platform-indepent pcode on big-endian machines.
 *
 * Yet another note ;-):
 *       HB_PCODE_MK*(), HB_{GET,PUT}_{LE,BE}_*() and HB_*_{FROM,TO}_LE()
 *       as many others are macros which may use more then once given
 *       parameters - you cannot use and {pre/post}{inc/dec}rementation
 *       in parameters. Changing them to function (or pseudo function) will
 *       reduce performance in many cases (but in some when poor compiler
 *       is used can give faster codes), Druzus
 *
 */
PHB_CODEBLOCK hb_codeblockNew( const BYTE * pBuffer,
                                  USHORT uiLocals, const BYTE * pLocalPosTable,
                                  PHB_SYMB pSymbol )
{
   PHB_CODEBLOCK pCBlock;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockNew(%p, %hu, %p, %p)", pBuffer, uiLocals, pLocalPosTable, pSymbol ) );

   pCBlock           = ( PHB_CODEBLOCK ) hb_gcAlloc( sizeof( HB_CODEBLOCK ), hb_codeblockDeleteGarbage );

   /* Store the number of referenced local variables
    */
   pCBlock->uiLocals = uiLocals;

   if( uiLocals )
   {
      /* NOTE: if a codeblock will be created by macro compiler then
       * uiLocal have to be ZERO
       * uiLocal will be also ZERO if it is a nested codeblock
       */
      USHORT   ui = 1;
      PHB_ITEM pLocal;

      /* Create a table that will store the values of local variables
       * accessed in a codeblock
       * The element 0 is used as the counter of references to this table
       * NOTE: This table can be shared by codeblocks created during
       * evaluation of this codeblock
       */
      pCBlock->pLocals                          = ( PHB_ITEM ) hb_xgrab( ( uiLocals + 1 ) * sizeof( HB_ITEM ) );
      pCBlock->pLocals[ 0 ].type                = HB_IT_LONG;
      pCBlock->pLocals[ 0 ].item.asLong.value   = 1;

      while( uiLocals-- )
      {
         /*
          * Swap the current value of local variable with the reference to this value.
          */
         pLocal         = hb_stackItemFromBase( HB_PCODE_MKUSHORT( pLocalPosTable ) );
         pLocalPosTable += 2;

         pLocal         = hb_memvarDetachLocal( pLocal );
         hb_itemRawCpy( pCBlock->pLocals + ui, pLocal );
         /* Increment the reference counter so this value will not be
          * released if other codeblock will be deleted
          */
         hb_memvarValueIncRef( ( HB_HANDLE ) pLocal->item.asMemvar.value );
         ++ui;
      }
   }
   else
   {
      /* Check if this codeblock is created during evaluation of another
       * codeblock - all inner codeblocks use the local variables table
       * created during creation of the outermost codeblock
       */
      PHB_ITEM pLocal;

      pLocal = hb_stackSelfItem();
      if( HB_IS_BLOCK( pLocal ) )
      {
         PHB_CODEBLOCK pOwner = pLocal->item.asBlock.value;

         pCBlock->pLocals  = pOwner->pLocals;
         pCBlock->uiLocals = uiLocals = pOwner->uiLocals;

         if( pOwner->pLocals ) /* the outer codeblock have the table with local references - reuse it */
         {
            while( uiLocals )
            {
               hb_memvarValueIncRef( ( HB_HANDLE ) pCBlock->pLocals[ uiLocals ].item.asMemvar.value );
               --uiLocals;
            }
            /* increment a reference counter for the table of local references
             */
            pCBlock->pLocals[ 0 ].item.asLong.value++;
         }
      }
      else
         pCBlock->pLocals = NULL;
   }

   /*
    * The codeblock pcode is stored in static segment.
    * The only allowed operation on a codeblock is evaluating it then
    * there is no need to duplicate its pcode - just store the pointer to it
    */
   pCBlock->pCode        = ( BYTE * ) pBuffer;
   pCBlock->symbol       = pSymbol;
   pCBlock->ulCounter    = 1;
   /*
    * pCBlock->dynBuffer = FALSE;
    * pCBlock->bPrivVars = FALSE;
    * pCBlock->bDynami c = FALSE;
    */
   pCBlock->uiFlags      = 0;

   HB_TRACE( HB_TR_INFO, ( "codeblock created (%i) %lx", pCBlock->ulCounter, pCBlock ) );

   return pCBlock;
}

PHB_CODEBLOCK hb_codeblockMacroNew( BYTE * pBuffer, USHORT usLen )
{
   PHB_CODEBLOCK pCBlock;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockMacroNew(%p, %i)", pBuffer, usLen ) );

   pCBlock           = ( PHB_CODEBLOCK ) hb_gcAlloc( sizeof( HB_CODEBLOCK ), hb_codeblockDeleteGarbage );

   /* Store the number of referenced local variables
    */
   pCBlock->uiLocals = 0;
   pCBlock->pLocals  = NULL;
   /*
    * The codeblock pcode is stored in dynamically allocated memory that
    * can be deallocated after creation of a codeblock. We have to duplicate
    * the passed buffer
    */
   pCBlock->pCode    = ( BYTE * ) hb_xgrab( usLen );
   HB_MEMCPY( pCBlock->pCode, pBuffer, usLen );

   /*
    * pCBlock->dynBuffer = TRUE;
    * pCBlock->bPrivVars = FALSE;
    * pCBlock->bDynamic  = TRUE;
    */
   pCBlock->uiFlags      = ( CBF_DYNAMIC_BUFFER | CBF_DYNAMIC );
   pCBlock->symbol       = NULL; /* macro-compiled codeblock cannot acces a local symbol table */
   pCBlock->ulCounter    = 1;

   HB_TRACE( HB_TR_INFO, ( "codeblock created (%li) %lx", pCBlock->ulCounter, pCBlock ) );

   return pCBlock;
}

/* Delete a codeblock
 */
void  hb_codeblockDelete( PHB_ITEM pItem )
{
   PHB_CODEBLOCK pCBlock = pItem->item.asBlock.value;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockDelete(%p)", pItem ) );

   if( pCBlock && HB_ATOMIC_DEC( pCBlock->ulCounter ) == 0 )
   {
      if( pCBlock->pLocals )
      {
         USHORT ui = pCBlock->uiLocals;

         while( ui )
         {
            /* TraceLog( NULL, "Release Detached %i\n", ui );
             */
            hb_memvarValueDecRef( ( HB_HANDLE ) pCBlock->pLocals[ ui ].item.asMemvar.value );
            /* TraceLog( NULL, "DONE Release Detached %i\n", ui );
             */

            ui--;
         }

         /* decrement the table reference counter and release memory if
          * it was the last reference
          */
         if( --pCBlock->pLocals[ 0 ].item.asLong.value == 0 )
         {
            /* TraceLog( NULL, "Free Locals\n" ); */
            HB_TRACE( HB_TR_DEBUG, ( "Free,Locals %p", pCBlock->pLocals ) );
            hb_xfree( pCBlock->pLocals );
         }
      }

      /* free space allocated for pcodes - if it was a macro-compiled codeblock
       */
      if( pCBlock->pCode && ( pCBlock->uiFlags & CBF_DYNAMIC_BUFFER ) )
      {
         HB_TRACE( HB_TR_DEBUG, ( "Free pCode, %p", pCBlock->pCode ) );
         hb_xfree( pCBlock->pCode );
         pCBlock->pCode = NULL;
      }

      /* free space allocated for a CODEBLOCK structure
       */
      hb_gcFree( pCBlock );
   }
}

/* Release all allocated memory when called from the garbage collector
 */
HB_GARBAGE_FUNC( hb_codeblockDeleteGarbage )
{
   PHB_CODEBLOCK pCBlock = ( PHB_CODEBLOCK ) Cargo;

   HB_TRACE( HB_TR_INFO, ( "hb_codeblockDeleteGarbage(%p)", Cargo ) );

   /* free space allocated for local variables
    */
   if( pCBlock->pLocals )
   {
      USHORT ui = 1;

      while( ui <= pCBlock->uiLocals )
      {
         //TraceLog( NULL, "GC Release Detached %i %i\n", ui, pCBlock->pLocals[ ui ].item.asMemvar.value );
         hb_memvarValueDecGarbageRef( ( HB_HANDLE ) pCBlock->pLocals[ ui ].item.asMemvar.value );
         ++ui;
      }

      /* decrement the table reference counter and release memory if
       * it was the last reference
       */
      if( --pCBlock->pLocals[ 0 ].item.asLong.value == 0 )
      {
         hb_xfree( pCBlock->pLocals );
         pCBlock->pLocals = NULL;
      }
   }

   /* free space allocated for pcodes - if it was a macro-compiled codeblock
    */
   if( pCBlock->pCode && ( pCBlock->uiFlags & CBF_DYNAMIC_BUFFER ) )
   {
      hb_xfree( pCBlock->pCode );
      pCBlock->pCode = NULL;
   }
}

/* Get local variable referenced in a codeblock
 */
PHB_ITEM hb_codeblockGetVar( PHB_ITEM pItem, long iItemPos )
{
   PHB_CODEBLOCK pCBlock = pItem->item.asBlock.value;

   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockGetVar(%p, %ld)", pItem, iItemPos ) );

   if( pCBlock->uiLocals < -iItemPos )
   {
      hb_errInternal( HB_EI_ERRUNRECOV,
         "Codeblock %p does not export %li detached locals", ( const char * ) pCBlock,
#if defined( HB_OS_WIN_64 )
         ( const char * ) ( HB_LONG ) -iItemPos );
#else
         ( const char * ) -iItemPos );
#endif
   }

   /* local variables accessed in a codeblock are always stored as reference */
   return hb_itemUnRef( pCBlock->pLocals - iItemPos );
}

/* Get local variable passed by reference
 */
PHB_ITEM  hb_codeblockGetRef( PHB_CODEBLOCK pCBlock, PHB_ITEM pRefer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_codeblockGetRef(%p, %p)", pCBlock, pRefer ) );

   return pCBlock->pLocals - pRefer->item.asRefer.value;
}
