/*
 * $Id: codebloc.c,v 1.44 2004/04/27 22:04:11 ronpinkas Exp $
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

extern void hb_memvarReleaseDetached( HB_HANDLE hValue );

/*
JC1: they are not needed anymore, it seems: all their reference are
commented out. I commet this out here, to state that access must be
thread-guarded, if someone ever wants to have them back

extern PHB_ITEM **hb_vm_pGlobals;
extern short hb_vm_iGlobals;
*/

/* Creates the codeblock structure
 *
 * pBuffer -> the buffer with pcodes (without HB_P_PUSHBLOCK)
 * uiLocals -> number of local variables referenced in a codeblock
 * pLocalPosTable -> a table with positions on eval stack for referenced variables
 * pSymbols    -> a pointer to the module symbol table
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
HB_CODEBLOCK_PTR hb_codeblockNew( BYTE * pBuffer,
            USHORT uiLocals,
            USHORT * pLocalPosTable,
            PHB_SYMB pSymbols, PHB_ITEM** pGlobals )
{
   HB_CODEBLOCK_PTR pCBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockNew(%p, %hu, %p, %p, %p)", pBuffer, uiLocals, pLocalPosTable, pSymbols, pGlobals));

   pCBlock = ( HB_CODEBLOCK_PTR ) hb_gcAlloc( sizeof( HB_CODEBLOCK ), hb_codeblockDeleteGarbage );

   pCBlock->procname = NULL;

   /* Store the number of referenced local variables
    */
   pCBlock->uiLocals = uiLocals;

   if( uiLocals )
   {
      /* NOTE: if a codeblock will be created by macro compiler then
       * uiLocal have to be ZERO
       * uiLocal will be also ZERO if it is a nested codeblock
       */
      USHORT ui = 1;
      PHB_ITEM pLocal;
      HB_HANDLE hMemvar;

      /* Create a table that will store the values of local variables
       * accessed in a codeblock
       * The element 0 is used as the counter of references to this table
       * NOTE: This table can be shared by codeblocks created during
       * evaluation of this codeblock
       */
      pCBlock->pLocals = ( PHB_ITEM ) hb_xgrab( ( uiLocals + 1 ) * sizeof( HB_ITEM ) );
      pCBlock->pLocals[ 0 ].type = HB_IT_LONG;
      pCBlock->pLocals[ 0 ].item.asLong.value = 1;

      while( uiLocals-- )
      {
         /*
          * Swap the current value of local variable with the reference to this value.
          */
         pLocal = hb_stackItemFromBase( HB_PCODE_MKUSHORT( pLocalPosTable ) );
         pLocalPosTable++;

         if( ! HB_IS_MEMVAR( pLocal ) )
         {
            /* Change the value only if this variable is not referenced
             * by another codeblock yet.
             * In this case we have to copy the current value to a global memory
             * pool so it can be shared by codeblocks
             */

            hMemvar = hb_memvarValueNew( pLocal, FALSE );

            pLocal->type = HB_IT_BYREF | HB_IT_MEMVAR;
            pLocal->item.asMemvar.itemsbase = hb_memvarValueBaseAddress();
            pLocal->item.asMemvar.offset    = 0;
            pLocal->item.asMemvar.value     = hMemvar;

            memcpy( pCBlock->pLocals + ui, pLocal, sizeof( HB_ITEM ) );
            hb_memvarValueIncRef( pLocal->item.asMemvar.value );
         }
         else
         {
            /* This variable is already detached (by another codeblock)
             * - copy the reference to a value
             */
            memcpy( pCBlock->pLocals + ui, pLocal, sizeof( HB_ITEM ) );
            //TraceLog( NULL, "Already detached: %i\n", pLocal->type );

            /* Increment the reference counter so this value will not be
             * released if other codeblock will be deleted
             */
            hb_memvarValueIncRef( pLocal->item.asMemvar.value );
            //TraceLog( NULL, "Detach: %p to %p\n", pLocal, pCBlock->pLocals + ui );
         }

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
         HB_CODEBLOCK_PTR pOwner = pLocal->item.asBlock.value;

         pCBlock->pLocals = pOwner->pLocals;
         pCBlock->uiLocals = uiLocals = pOwner->uiLocals;

         if( pOwner->pLocals )
         {  /* the outer codeblock have the table with local references - reuse it */
            while( uiLocals )
            {
               hb_memvarValueIncRef( pCBlock->pLocals[ uiLocals ].item.asMemvar.value );
               --uiLocals;
            }
            /* increment a reference counter for the table of local references
             */
            pCBlock->pLocals[ 0 ].item.asLong.value++;
         }
      }
      else
      {
         pCBlock->pLocals = NULL;
      }
   }

   /*
    * The codeblock pcode is stored in static segment.
    * The only allowed operation on a codeblock is evaluating it then
    * there is no need to duplicate its pcode - just store the pointer to it
    */
   pCBlock->pCode     = pBuffer;
   pCBlock->dynBuffer = FALSE;

   pCBlock->pSymbols  = pSymbols;
   pCBlock->ulCounter = 1;

   pCBlock->pGlobals  = pGlobals;

   HB_TRACE(HB_TR_INFO, ("codeblock created (%li) %lx", pCBlock->ulCounter, pCBlock));

   return pCBlock;
}

HB_CODEBLOCK_PTR hb_codeblockMacroNew( BYTE * pBuffer, USHORT usLen )
{
   HB_CODEBLOCK_PTR pCBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockMacroNew(%p, %i)", pBuffer, usLen));

   pCBlock = ( HB_CODEBLOCK_PTR ) hb_gcAlloc( sizeof( HB_CODEBLOCK ), hb_codeblockDeleteGarbage );

   pCBlock->procname = NULL;

   /* Store the number of referenced local variables
    */
   pCBlock->uiLocals = 0;
   pCBlock->pLocals  = NULL;
   /*
    * The codeblock pcode is stored in dynamically allocated memory that
    * can be deallocated after creation of a codeblock. We have to duplicate
    * the passed buffer
    */
   pCBlock->pCode = ( BYTE * ) hb_xgrab( usLen );
   memcpy( pCBlock->pCode, pBuffer, usLen );
   pCBlock->dynBuffer = TRUE;

   pCBlock->pSymbols  = NULL; /* macro-compiled codeblock cannot acces a local symbol table */
   pCBlock->ulCounter = 1;

   pCBlock->pGlobals  = NULL;

   HB_TRACE(HB_TR_INFO, ("codeblock created (%li) %lx", pCBlock->ulCounter, pCBlock));

   return pCBlock;
}

/* Delete a codeblock
 */
void  hb_codeblockDelete( HB_ITEM_PTR pItem )
{
   HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockDelete(%p)", pItem));

   #ifndef HB_ARRAY_USE_COUNTER
   // Called recursively from hb_codeblockDeleteGarbage()!
   if( pCBlock && pCBlock->pSelfBase == (PHB_BASEARRAY) 1 )
   {
      return;
   }
   #endif

   if( pCBlock && (--pCBlock->ulCounter == 0) )
   {
      if( pCBlock->pSelfBase )
      {
         #ifdef HB_ARRAY_USE_COUNTER
            pCBlock->pSelfBase->ulHolders--;
         #else
            PHB_BASEARRAY pSelfBase = pCBlock->pSelfBase;

            // HACK! Avoid possible recursion problem when one of the array items of the attached object in turn points to this block.
            pCBlock->pSelfBase = (PHB_BASEARRAY) 1;

            hb_arrayReleaseHolder( pSelfBase, (void *) pCBlock );
         #endif

         pCBlock->pSelfBase = NULL;
      }

      if( pCBlock->pLocals )
      {
         USHORT ui = pCBlock->uiLocals;

         while( ui )
         {
            //TraceLog( NULL, "Release Detached %i\n", ui );
            //hb_memvarValueDecRef( pCBlock->pLocals[ ui ].item.asMemvar.value );
            hb_memvarReleaseDetached( pCBlock->pLocals[ ui ].item.asMemvar.value );
            //TraceLog( NULL, "DONE Release Detached %i\n", ui );

			ui--;
         }

         /* decrement the table reference counter and release memory if
          * it was the last reference
          */
         if( --pCBlock->pLocals[ 0 ].item.asLong.value == 0 )
         {
            //TraceLog( NULL, "Free Locals\n" );
            HB_TRACE( HB_TR_DEBUG, ( "Free,Locals %p", pCBlock->pLocals ) );
            hb_xfree( pCBlock->pLocals );
         }
      }

      /* free space allocated for pcodes - if it was a macro-compiled codeblock
       */
      if( pCBlock->pCode && pCBlock->dynBuffer )
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
   HB_CODEBLOCK_PTR pCBlock = ( HB_CODEBLOCK_PTR ) Cargo;

   HB_TRACE(HB_TR_INFO, ("hb_codeblockDeleteGarbage(%p)", Cargo));

   if( pCBlock->pSelfBase )
   {
      pCBlock->pSelfBase = NULL;
   }

   /* free space allocated for local variables
    */
   if( pCBlock->pLocals )
   {
      USHORT ui = 1;

      while( ui <= pCBlock->uiLocals )
      {
         //TraceLog( NULL, "GC Release Detached %i %i\n", ui, pCBlock->pLocals[ ui ].item.asMemvar.value );
         hb_memvarValueDecGarbageRef( pCBlock->pLocals[ ui ].item.asMemvar.value );
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
   if( pCBlock->pCode && pCBlock->dynBuffer )
   {
      hb_xfree( pCBlock->pCode );
      pCBlock->pCode = NULL;
   }
}

/* Evaluate passed codeblock
 * Before evaluation we have to switch to a static variable base that
 * was defined when the codeblock was created.
 * (The codeblock can only see the static variables defined in a module
 * where the codeblock was created)
 */
void hb_codeblockEvaluate( HB_ITEM_PTR pItem )
{
   int iStatics = HB_VM_STACK.iStatics;
   /*
   PHB_ITEM **Saved_pGlobals = hb_vm_pGlobals;
   short      Saved_iGlobals = hb_vm_iGlobals;
   short iGlobal;
   */

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockEvaluate(%p)", pItem));

 /*
   // Lock Module Globals, so that GC from callee will not release higher level Globals.
   for ( iGlobal = 0; iGlobal < hb_vm_iGlobals; iGlobal++ )
   {
      if( (*hb_vm_pGlobals)[ iGlobal ]->type == HB_IT_ARRAY )
      {
         hb_gcLock( (*hb_vm_pGlobals)[ iGlobal ]->item.asArray.value );
      }
      else if( (*hb_vm_pGlobals)[ iGlobal ]->type == HB_IT_BLOCK )
      {
         hb_gcLock( (*hb_vm_pGlobals)[ iGlobal ]->item.asBlock.value );
      }
   }
 */

   /*
   hb_vm_pGlobals = pItem->item.asBlock.value->pGlobals;
   hb_vm_iGlobals = pItem->item.asBlock.value->iGlobals;
   */

   HB_VM_STACK.iStatics = pItem->item.asBlock.statics;
   hb_vmExecute( pItem->item.asBlock.value->pCode, pItem->item.asBlock.value->pSymbols, pItem->item.asBlock.value->pGlobals );
   /* hb_vmExecute() unlocks the stack on exit */
   HB_VM_STACK.iStatics = iStatics;

   /*
   hb_vm_pGlobals = Saved_pGlobals;
   hb_vm_iGlobals = Saved_iGlobals;
   */
 /*
   // Un-Lock Module Globals, so that GC can release current level Globals.
   for ( iGlobal = 0; iGlobal < hb_vm_iGlobals; iGlobal++ )
   {
      if( (*hb_vm_pGlobals)[ iGlobal ]->type == HB_IT_ARRAY )
      {
         hb_gcUnlock( (*hb_vm_pGlobals)[ iGlobal ]->item.asArray.value );
      }
      else if( (*hb_vm_pGlobals)[ iGlobal ]->type == HB_IT_BLOCK )
      {
         hb_gcUnlock( (*hb_vm_pGlobals)[ iGlobal ]->item.asBlock.value );
      }
   }
 */
}

/* Get local variable referenced in a codeblock
 */
PHB_ITEM  hb_codeblockGetVar( PHB_ITEM pItem, LONG iItemPos )
{
   HB_CODEBLOCK_PTR pCBlock = pItem->item.asBlock.value;

   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockGetVar(%p, %ld)", pItem, iItemPos));

   /* local variables accessed in a codeblock are always stored as reference */
   return hb_itemUnRef( pCBlock->pLocals - iItemPos );
}

/* Get local variable passed by reference
 */
PHB_ITEM  hb_codeblockGetRef( HB_CODEBLOCK_PTR pCBlock, PHB_ITEM pRefer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_codeblockGetRef(%p, %p)", pCBlock, pRefer));

   return pCBlock->pLocals - pRefer->item.asRefer.value;
}
