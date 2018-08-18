/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour simple hash table implementation
 *
 * Copyright 1999-2002 Ryszard Glab <rglab@imid.med.pl>
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

#include "hbhash.h"
#include "hbexemem.h"

#if defined( __HB_COMPILER__ )
   #include "hbcomp.h"
#endif

static PHB_HASH_ITEM hb_hashItemNew( HB_SIZE ulKey, const void * pValue )
{
   PHB_HASH_ITEM pItem = ( PHB_HASH_ITEM ) hb_xgrab( sizeof( HB_HASH_ITEM ) );

   pItem->key   = ulKey;
   pItem->cargo = pValue;
   pItem->next  = NULL;

   return pItem;
}

static void hb_hashItemDelete( PHB_HASH_ITEM pItem )
{
   hb_xfree( ( void * ) pItem );
}

/* create a new  hash table
 * ulSize = initial numer of items in the table
 * pHashTable = a function that calculates a hash key value
 *       (first parameter is a value to add)
 * pDelete = a function that clears item's value before item's releasing
 *       (first parameter is a value to clear)
 * pComp = a function for comparing a values
 *       (first and second are values to compare, function have to return
 *        zero if values match or nonzero if they don't match)
 */
PHB_HASH_TABLE hb_hashTableCreate( HB_SIZE       ulSize,
                                   PHB_HASH_FUNC pHashFunc,
                                   PHB_HASH_FUNC pDelete,
                                   PHB_HASH_FUNC pComp )
{
   PHB_HASH_TABLE pTable = ( PHB_HASH_TABLE ) hb_xgrab( sizeof( HB_HASH_TABLE ) );

   pTable->ulTableSize     = ulSize;
   pTable->pKeyFunc        = pHashFunc;
   pTable->pDeleteItemFunc = pDelete;
   pTable->pCompFunc       = pComp;
   pTable->ulCount         = pTable->ulUsed = 0;

   pTable->pItems          = ( PHB_HASH_ITEM * ) hb_xgrabz( sizeof( PHB_HASH_ITEM ) * ulSize );
//   memset( pTable->pItems, 0, ( size_t ) ( sizeof( PHB_HASH_ITEM ) * ulSize ) );

   return pTable;
}

/* Delete all items in the hash table and next delete the table
 */
void hb_hashTableKill( PHB_HASH_TABLE pTable )
{
   HB_SIZE ulSize = 0;

   while( ulSize < pTable->ulTableSize )
   {
      if( pTable->pItems[ ulSize ] )
      {
         PHB_HASH_ITEM pItem, pNext;

         pItem = pTable->pItems[ ulSize ];
         while( pItem )
         {
            if( pTable->pDeleteItemFunc )
               ( pTable->pDeleteItemFunc )( pItem->cargo, NULL );
            pNext = pItem->next;
            hb_xfree( ( void * ) pItem );
            pItem = pNext;
         }
      }
      ++ulSize;
   }
   hb_xfree( pTable->pItems );
   hb_xfree( pTable );
}

/* resize table */
PHB_HASH_TABLE hb_hashTableResize( PHB_HASH_TABLE pTable, HB_SIZE ulNewSize )
{
   PHB_HASH_TABLE pNew;
   HB_SIZE          ulSize = 0;

   if( ulNewSize == 0 )
      ulNewSize = 2 * pTable->ulTableSize + 1;

   pNew = hb_hashTableCreate( ulNewSize,
                              pTable->pKeyFunc,
                              pTable->pDeleteItemFunc,
                              pTable->pCompFunc );

   while( ulSize < pTable->ulTableSize )
   {
      if( pTable->pItems[ ulSize ] )
      {
         PHB_HASH_ITEM pItem;

         pItem = pTable->pItems[ ulSize ];
         while( pItem )
         {
            HB_SIZE       ulKey;
            PHB_HASH_ITEM pNewItem, pNext;

            pNext    = pItem->next;
            ulKey    = ( pTable->pKeyFunc )( pItem->cargo, NULL );
            pNewItem = pNew->pItems[ ulKey ];
            if( pNewItem )
            {
               while( pNewItem->next )
                  pNewItem = pNewItem->next;
               pNewItem->next = pItem;
            }
            else
            {
               pNew->pItems[ ulKey ] = pItem;
               ++pNew->ulUsed;
            }
            pItem->key  = ulKey;
            pItem->next = NULL;
            ++pNew->ulCount;
            pItem       = pNext;
         }
      }
      ++ulSize;
   }
   hb_xfree( pTable->pItems );
   hb_xfree( pTable );

   return pNew;
}

/* add a new value into th ehash table */
BOOL hb_hashTableAdd( PHB_HASH_TABLE pTable, const void * pValue )
{
   HB_SIZE       ulKey;
   PHB_HASH_ITEM pItem;

   ulKey = ( pTable->pKeyFunc )( pValue, NULL );
   pItem = pTable->pItems[ ulKey ];
   if( pItem )
   {
      while( pItem->next )
         pItem = pItem->next;
      pItem->next = hb_hashItemNew( ulKey, pValue );
   }
   else
   {
      pTable->pItems[ ulKey ] = hb_hashItemNew( ulKey, pValue );
      ++pTable->ulUsed;
   }
   ++pTable->ulCount;

   return TRUE;
}

/* return the pointer to item's value or NULL if not found
 */
const void * hb_hashTableFind( PHB_HASH_TABLE pTable, const void * pValue )
{
   HB_SIZE       ulKey;
   PHB_HASH_ITEM pItem;
   const void *        pFound = NULL;

   ulKey = ( pTable->pKeyFunc )( pValue, NULL );
   pItem = pTable->pItems[ ulKey ];
   if( pItem )
   {
      while( pItem && ( ( pTable->pCompFunc )( pItem->cargo, pValue ) != 0 ) )
         pItem = pItem->next;

      if( pItem )
         pFound = pItem->cargo;
   }

   return pFound;
}

/* Delete an item from the table
 * Returns TRUE if item was found and returns FALSE when passed item
 * is not stored in the table
 */
BOOL hb_hashTableDel( PHB_HASH_TABLE pTable, void * pValue )
{
   HB_SIZE       ulKey;
   PHB_HASH_ITEM pItem;
   PHB_HASH_ITEM pPrev  = NULL;
   BOOL          bFound = FALSE;

   ulKey = ( pTable->pKeyFunc )( pValue, NULL );
   pItem = pTable->pItems[ ulKey ];
   while( pItem && ! bFound )
   {
      if( ( pTable->pCompFunc )( pItem->cargo, pValue ) == 0 )
      {
         if( pPrev )
            pPrev->next = pItem->next;
         else
         {
            pTable->pItems[ ulKey ] = pItem->next;
            if( ! pItem->next )
               --pTable->ulUsed;
         }

         hb_hashItemDelete( pItem );
         bFound = TRUE;
         --pTable->ulCount;
      }
      else
      {
         pPrev = pItem;
         pItem = pItem->next;
      }
   }

   return bFound;
}

/* return the hash table size */
HB_SIZE hb_hashTableSize( PHB_HASH_TABLE pTable )
{
   return pTable->ulTableSize;
}
