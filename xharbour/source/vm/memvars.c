/*
 * $Id: memvars.c,v 1.53 2004/02/14 21:01:19 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Memvar (PRIVATE/PUBLIC) runtime support
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 *           2003 Giancarlo Niccolai <gian@niccolai.ws>
 *                +Threadsafing and MT stack private variables
 *
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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    __MVSAVE()
 *    __MVRESTORE() (Thanks to Dave Pearson and Jo French for the original
 *                   Clipper function (FReadMem()) to read .mem files)
 *
 * Copyright 2002 Ron Pinnkas <ron@ronpinkas.com>
 *   hb_memvarReleasePublic()
 *   hb_memvarReleasePublicWorker()
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz>
 *    HB_FUNC( __MVSYMBOLINFO )
 *    static HB_DYNS_FUNC( hb_GetSymbolInfo )
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_THREAD_OPTIMIZE_STACK

#include <ctype.h> /* for toupper() function */

#include "hbapi.h"
#include "hbfast.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h" /* for __MVSAVE()/__MVRESTORE() */
#include "hbdate.h" /* for __MVSAVE()/__MVRESTORE() */
#include "hbcomp.h" /* for VS_* macros */
#include "hbstack.h"

#include "error.ch"
#include "hbmemvar.ch"
#include "hbset.h"

//JC1: under threads, we need this to be in thread stack
#ifndef HB_THREAD_SUPPORT
static PHB_DYNS * s_privateStack  = NULL;
static ULONG s_privateStackSize = 0;
static ULONG s_privateStackCnt  = 0;
static ULONG s_privateStackBase = 0;

static ULONG s_globalTableSize = 0;
static ULONG s_globalFirstFree = 0;
static ULONG s_globalLastFree  = 0;
static ULONG s_globalFreeCnt   = 0;
static HB_VALUE_PTR s_globalTable = NULL;

#else

#define  s_privateStack       (HB_VM_STACK.privateStack)
#define  s_privateStackSize   (HB_VM_STACK.privateStackSize)
#define  s_privateStackCnt    (HB_VM_STACK.privateStackCnt)
#define  s_privateStackBase   (HB_VM_STACK.privateStackBase)

#define  s_globalTableSize   (HB_VM_STACK.globalTableSize)
#define  s_globalFirstFree   (HB_VM_STACK.globalFirstFree)
#define  s_globalLastFree    (HB_VM_STACK.globalLastFree)
#define  s_globalFreeCnt     (HB_VM_STACK.globalFreeCnt)
#define  s_globalTable       (HB_VM_STACK.globalTable)

PHB_DYNS s_memvarThGetName( char * szName, HB_STACK *pstack )
{
   char szNewName[270];
   sprintf( szNewName, ":TH:%d:%s", pstack->th_vm_id, szName );
   return hb_dynsymGet( szNewName );
}

PHB_DYNS s_memvarThFindName( char * szName, HB_STACK *pstack )
{
   char szNewName[270];
   sprintf( szNewName, ":TH:%d:%s", pstack->th_vm_id, szName );
   return hb_dynsymFindName( szNewName );
}

#endif

#define TABLE_INITHB_VALUE   100
#define TABLE_EXPANDHB_VALUE  50

struct mv_PUBLIC_var_info
{
   int iPos;
   BOOL bFound;
   HB_DYNS_PTR pDynSym;
};

static void hb_memvarCreateFromDynSymbol( PHB_DYNS, BYTE, PHB_ITEM );
static void hb_memvarAddPrivate( PHB_DYNS );
static HB_DYNS_PTR hb_memvarFindSymbol( HB_ITEM_PTR );
void hb_memvarReleasePublic( PHB_ITEM pMemVar );

#ifndef HB_THREAD_SUPPORT
void hb_memvarsInit( void )
{
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsInit()"));

   s_globalTable = ( HB_VALUE_PTR ) hb_xgrab( sizeof( HB_VALUE ) * TABLE_INITHB_VALUE );
   s_globalTableSize = TABLE_INITHB_VALUE;
   s_globalFreeCnt   = 0;
   s_globalFirstFree = s_globalLastFree = 1;

   for( ulPos = s_globalFirstFree; ulPos < s_globalTableSize; ulPos++ )
   {
      s_globalTable[ ulPos ].item.type = HB_IT_NIL;
      s_globalTable[ ulPos ].counter = 0;
   }

   s_privateStack = ( PHB_DYNS * ) hb_xgrab( sizeof( PHB_DYNS ) * TABLE_INITHB_VALUE );
   s_privateStackSize = TABLE_INITHB_VALUE;
   s_privateStackCnt  = s_privateStackBase = 0;
}

/* clear all variables except the detached ones
 * Should be called at application exit only
*/
void hb_memvarsRelease( void )
{
   HB_THREAD_STUB
   ULONG ulCnt;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsClear()"));

   ulCnt = s_globalLastFree;

   if( s_globalTable )
   {
      while( --ulCnt )
      {
         /*
          * 2003-Sep-10 Ron Pinkas - Ok to process all, including detached variables.
         if( s_globalTable[ ulCnt ].counter && s_globalTable[ ulCnt ].hPrevMemvar != ( HB_HANDLE )-1 )
          */
         {
            if( HB_IS_STRING( &s_globalTable[ ulCnt ].item ) )
            {
               hb_itemReleaseString( &s_globalTable[ ulCnt ].item );
            }
            else if( HB_IS_COMPLEX( &s_globalTable[ ulCnt ].item ) )
            {
               /*
                * We do NOT need to worry about Arrays & Blcoks as these values have been released
                * by hb_gcReleaseAll() unconditionally!
                *
               */
               #ifdef DEBUG_MEMORY_LEAKS
                  // hb_gcAll() is used instead of hb_gcReleaseAll() - so we need to clean afterall.
                  hb_itemClear( &s_globalTable[ ulCnt ].item );
               #endif
            }

            s_globalTable[ ulCnt ].counter = 0;
         }
      }

      hb_xfree( s_globalTable );
      s_globalTable = NULL;
   }

   if( s_privateStack )
   {
      hb_xfree( s_privateStack );
      s_privateStack = NULL;
   }
}

#else

void hb_memvarsInit( HB_STACK *pStack )
{
   pStack->globalTable = ( HB_VALUE_PTR ) hb_xgrab( sizeof( HB_VALUE ) * TABLE_INITHB_VALUE );
   pStack->privateStack = ( PHB_DYNS * ) hb_xgrab( sizeof( PHB_DYNS ) * TABLE_INITHB_VALUE );
   pStack->globalTableSize = TABLE_INITHB_VALUE;
   pStack->globalFreeCnt   = 0;
   pStack->globalFirstFree = pStack->globalLastFree = 1;

   pStack->privateStackSize = TABLE_INITHB_VALUE;
   pStack->privateStackCnt  = pStack->privateStackBase = 0;
}

/* clear all variables except the detached ones
 * Should be called at application exit only
*/
void hb_memvarsRelease( HB_STACK *pStack )
{
   ULONG ulCnt;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsClear()"));

   ulCnt = pStack->globalLastFree;

   if( pStack->globalTable )
   {
      while( --ulCnt )
      {
         /*
          * 2003-Sep-10 Ron Pinkas - Ok to process all, including detached variables.
         if( s_globalTable[ ulCnt ].counter && s_globalTable[ ulCnt ].hPrevMemvar != ( HB_HANDLE )-1 )
          */
         {
            if( HB_IS_STRING( &pStack->globalTable[ ulCnt ].item ) )
            {
               hb_itemReleaseString( &pStack->globalTable[ ulCnt ].item );
            }
            else if( HB_IS_COMPLEX( &pStack->globalTable[ ulCnt ].item ) )
            {
               /*
                * We do NOT need to worry about Arrays & Blcoks as these values have been released
                * by hb_gcReleaseAll() unconditionally!
                *
                hb_itemClearMT( &pStack->globalTable[ ulCnt ].item, pStack );
                */
            }
         }

          pStack->globalTable[ ulCnt ].counter = 0;
      }

      hb_xfree( pStack->globalTable );
      pStack->globalTable = NULL;
   }

   if( pStack->privateStack )
   {
      hb_xfree( pStack->privateStack );
      pStack->privateStack = NULL;
   }
}

#endif

/*
 * This function base address of values table
 * JC1: I don't like this. Really.
 */
HB_VALUE_PTR *hb_memvarValueBaseAddress( void )
{
   HB_THREAD_STUB

   return &s_globalTable;
}

/*
 * This function creates new global value.
 *
 * pSource = item value that have to be stored or NULL
 * bTrueMemvar = TRUE | FALSE
 *    FALSE if function is called to create memvar variable for a codeblock
 *       (to store detached local variable) - in this case we have to do
 *       exact copy of passed item (without duplicating its value and
 *       without reference decrementing)
 *    TRUE if we are creating regular memvar variable (PUBLI or PRIVATE)
 *       In this case we have to do normal item coping.
 *
 * Returns:
 *  handle to variable memory or fails
 *
*/
HB_HANDLE hb_memvarValueNew( HB_ITEM_PTR pSource, BOOL bTrueMemvar )
{
   HB_THREAD_STUB

   HB_VALUE_PTR pValue;
   HB_HANDLE hValue;   /* handle 0 is reserved */
                       /* = 1 removed, since it's initialized in all branches. Caused a warning with Borland C++ */

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueNew(%p, %d)", pSource, (int) bTrueMemvar));

   if( s_globalFreeCnt )
   {
      /* There are holes in the table
         * Get a first available hole
         */
      hValue = s_globalFirstFree;
      --s_globalFreeCnt;

      /* Now find the next hole
         */
      if( s_globalFreeCnt )
      {
         ++s_globalFirstFree;

         while( s_globalTable[ s_globalFirstFree ].counter )
         {
            ++s_globalFirstFree;
         }
      }
      else
      {
         /* No more holes
            */
         s_globalFirstFree = s_globalLastFree;
      }
   }
   else
   {
      /* Allocate the value from the end of table
         */
      if( s_globalFirstFree < s_globalTableSize )
      {
         hValue = s_globalFirstFree;
         s_globalFirstFree = ++s_globalLastFree;
      }
      else
      {
        ULONG ulPos;

        #ifndef HB_ARRAY_USE_COUNTER
           HB_VALUE_PTR pOldValues = s_globalTable;
           ULONG ulValues = s_globalTableSize;
        #endif

         /* No more free values in the table - expand the table
            */
         hValue = s_globalTableSize;
         s_globalFirstFree = s_globalLastFree = s_globalTableSize + 1;
         s_globalTableSize += TABLE_EXPANDHB_VALUE;

         s_globalTable = ( HB_VALUE_PTR ) hb_xrealloc( s_globalTable, sizeof( HB_VALUE ) * s_globalTableSize );

         #ifndef HB_ARRAY_USE_COUNTER
            if( s_globalTable != pOldValues )
            {
               for( ulPos = 0; ulPos < ulValues; ulPos++ )
               {
                  if( HB_IS_ARRAY( &( ( s_globalTable + ulPos )->item ) ) && ( s_globalTable + ulPos )->item.item.asArray.value )
                  {
                     hb_arrayResetHolder( ( s_globalTable + ulPos )->item.item.asArray.value, (void *) &( ( pOldValues + ulPos )->item ), (void * ) &( ( s_globalTable + ulPos )->item ) );
                  }
               }
            }
         #endif

         for( ulPos = s_globalFirstFree; ulPos < s_globalTableSize; ulPos++ )
         {
            s_globalTable[ ulPos ].item.type = HB_IT_NIL;
            s_globalTable[ ulPos ].counter = 0;
         }
      }
   }

   pValue = s_globalTable + hValue;

   pValue->counter = 1;
   pValue->item.type = HB_IT_NIL;

   if( pSource )
   {
      if( bTrueMemvar )
      {
         hb_itemCopy( &pValue->item, pSource );
      }
      else
      {
         memcpy( &pValue->item, pSource, sizeof(HB_ITEM) );

         if( HB_IS_ARRAY( pSource ) && pSource->item.asArray.value )
         {
            //TraceLog( NULL, "Detached %p array: %p to %p\n", pSource, pSource->item.asArray.value, &pValue->item );

            #ifndef HB_ARRAY_USE_COUNTER
               hb_arrayResetHolder( pSource->item.asArray.value, pSource, &pValue->item );
            #endif
         }
      }
   }

   if( bTrueMemvar )
   {
      pValue->hPrevMemvar = 0;
   }
   else
   {
      pValue->hPrevMemvar = ( HB_HANDLE )-1;    /* detached variable */
   }

   HB_TRACE(HB_TR_INFO, ("hb_memvarValueNew: memvar item created with handle %i", hValue));

   return hValue;
}

/*
 * This function pushes passed dynamic symbol that belongs to PRIVATE variable
 * into the stack. The value will be popped from it if the variable falls
 * outside the scope (either by using RELEASE, CLEAR ALL, CLEAR MEMORY or by
 * an exit from the function/procedure)
 *
 */
static void hb_memvarAddPrivate( PHB_DYNS pDynSym )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarAddPrivate(%p)", pDynSym));

   /* Allocate the value from the end of table
    */

   if( s_privateStackCnt == s_privateStackSize )
   {
      /* No more free values in the table - expand the table
       */
      s_privateStackSize += TABLE_EXPANDHB_VALUE;
      s_privateStack = ( PHB_DYNS * ) hb_xrealloc( s_privateStack, sizeof( PHB_DYNS ) * s_privateStackSize );
   }

   s_privateStack[ s_privateStackCnt++ ] = pDynSym;
}

/*
 * This function returns current PRIVATE variables stack base
 */
ULONG hb_memvarGetPrivatesBase( void )
{
   HB_THREAD_STUB

   ULONG ulBase = s_privateStackBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetPrivatesBase()"));

   s_privateStackBase = s_privateStackCnt;

   return ulBase;
}

/*
 * This function releases PRIVATE variables created after passed base
 *
 */
void hb_memvarSetPrivatesBase( ULONG ulBase )
{
   HB_THREAD_STUB

   HB_HANDLE hVar;
   HB_HANDLE hOldValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarSetPrivatesBase(%lu)", ulBase));

   while( s_privateStackCnt > s_privateStackBase )
   {
      --s_privateStackCnt;
      hVar = s_privateStack[ s_privateStackCnt ]->hMemvar;
      if( hVar )
      {
          hOldValue = s_globalTable[ hVar ].hPrevMemvar;
          hb_memvarValueDecRef( hVar );
          /*
          * Restore previous value for variables that were overridden
          */
          s_privateStack[ s_privateStackCnt ]->hMemvar = hOldValue;
      }
   }

   s_privateStackBase = ulBase;
}

/*
 * This function increases the number of references to passed global value
 *
 */
void hb_memvarValueIncRef( HB_HANDLE hValue )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueIncRef(%lu)", hValue));

   s_globalTable[ hValue ].counter++;

   HB_TRACE(HB_TR_INFO, ("Memvar item (%i) increment refCounter=%li", hValue, s_globalTable[ hValue ].counter));
}

#ifndef HB_THREAD_SUPPORT
static void hb_memvarRecycle( HB_HANDLE hValue )
{
   if( s_globalFirstFree > hValue )
   {
      if( ( s_globalLastFree - hValue ) == 1 )
         s_globalFirstFree = s_globalLastFree = hValue;     /* last item */
      else
      {
         s_globalFirstFree = hValue;
         ++s_globalFreeCnt;             /* middle item */
      }
   }
   else if( ( s_globalLastFree - hValue ) == 1 )
   {
      s_globalLastFree = hValue;         /* last item */

      if( s_globalLastFree == s_globalFirstFree )
      {
         s_globalFreeCnt = 0;
      }
   }
   else
   {
      ++s_globalFreeCnt;
   }
}

#else
static void hb_memvarRecycleMT( HB_HANDLE hValue, HB_STACK *pStack )
{
   if( pStack->globalFirstFree > hValue )
   {
      if( ( pStack->globalLastFree - hValue ) == 1 )
      {
         pStack->globalFirstFree = pStack->globalLastFree = hValue;     /* last item */
      }
      else
      {
         pStack->globalFirstFree = hValue;
         ++pStack->globalFreeCnt;             /* middle item */
      }
   }
   else if( ( pStack->globalLastFree - hValue ) == 1 )
   {
      pStack->globalLastFree = hValue;         /* last item */

      if( pStack->globalLastFree == pStack->globalFirstFree )
      {
         pStack->globalFreeCnt = 0;
      }
   }
   else
   {
      ++pStack->globalFreeCnt;
   }
}
#endif

/*
 * This function decreases the number of references to passed global value.
 * If it is the last reference then this value is deleted.
 *
 */
void hb_memvarValueDecRef( HB_HANDLE hValue )
{
   HB_THREAD_STUB

   HB_VALUE_PTR pValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueDecRef(%lu)", hValue));

   pValue = s_globalTable + hValue;

   //TraceLog( NULL, "Memvar item (%i) Counter: %li\n", hValue, pValue->counter );

   if( pValue->counter > 0 )
   {
      /* Notice that Counter can be equal to 0.
      * This can happen if for example PUBLIC variable holds a codeblock
      * with detached variable. When hb_memvarsRelease() is called then
      * detached variable can be released before the codeblock. So if
      * the codeblock will be released later then it will try to release
      * again this detached variable.
      */
      if( --( pValue->counter ) == 0 )
      {
         if( HB_IS_COMPLEX( &( pValue->item ) ) )
         {
            hb_itemClear( &( pValue->item ) );
         }
         else
         {
            ( &pValue->item )->type = HB_IT_NIL;
         }

         #ifndef HB_THREAD_SUPPORT
         hb_memvarRecycle( hValue );
         #else
         hb_memvarRecycleMT( hValue, &HB_VM_STACK );
         #endif

         HB_TRACE(HB_TR_INFO, ("Memvar item (%i) deleted", hValue));
      }
   }
}

#ifdef HB_THREAD_SUPPORT
void hb_memvarValueDecRefMT( HB_HANDLE hValue, HB_STACK *pStack )
{
   HB_VALUE_PTR pValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueDecRef(%lu)", hValue));

   pValue = pStack->globalTable + hValue;

   HB_TRACE(HB_TR_INFO, ("Memvar item (%i) decrement refCounter=%li", hValue, pValue->counter-1));

   if( pValue->counter > 0 )
   {
      if( --pValue->counter == 0 )
      {
         if( HB_IS_COMPLEX( &pValue->item ) )
         {
            hb_itemClearMT( &pValue->item, pStack );
         }
         else
         {
            ( &pValue->item )->type = HB_IT_NIL;
         }

         hb_memvarRecycleMT( hValue, pStack );

         HB_TRACE(HB_TR_INFO, ("Memvar item (%i) deleted", hValue));
      }
   }
}

#endif

/* This function is called from releasing of detached local variables
 * referenced in a codeblock that is wiped out by the Garbage Collector.
 * Decrement the reference counter and clear a value stored in the memvar.
 * Don't clear arrays or codeblocks to avoid loops - these values will be
 * released by the garbage collector.
 */
void hb_memvarValueDecGarbageRef( HB_HANDLE hValue )
{
   HB_THREAD_STUB
   HB_VALUE_PTR pValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueDecRef(%lu)", hValue));

   pValue = s_globalTable + hValue;

   HB_TRACE(HB_TR_INFO, ("Memvar item (%i) decrement refCounter=%li", hValue, pValue->counter-1));

   /* Notice that Counter can be equal to 0.
   * This can happen if for example PUBLIC variable holds a codeblock
   * with detached variable. When hb_memvarsRelease() is called then
   * detached variable can be released before the codeblock. So if
   * the codeblock will be released later then it will try to release
   * again this detached variable.
   */
   if( --pValue->counter <= 0 )
   {
      if( HB_IS_STRING( &pValue->item ) )
      {
         hb_itemReleaseString( &pValue->item );
      }
      else if( HB_IS_ARRAY( &pValue->item ) )
      {
         #ifndef HB_ARRAY_USE_COUNTER
            if( pValue->item.item.asArray.value )
            {
               hb_arrayReleaseHolderGarbage( pValue->item.item.asArray.value, &pValue->item );
            }
         #endif
      }
      else if( HB_IS_COMPLEX( &pValue->item ) )
      {
         /*
          * We do NOT need to worry about Arrays & Blcoks as these values have been released
          * by hb_gcReleaseAll() unconditionally!
          *
         hb_itemClear( &pValue->item );
         */
      }

      pValue->item.type = HB_IT_NIL;

      #ifndef HB_THREAD_SUPPORT
         hb_memvarRecycle( hValue );
      #else
         hb_memvarRecycleMT( hValue, &HB_VM_STACK );
      #endif

      HB_TRACE(HB_TR_INFO, ("Memvar item (%i) deleted", hValue));
   }
}

/*
 * This functions copies passed item value into the memvar pointed
 * by symbol
 *
 * pMemvar - symbol associated with a variable
 * pItem   - value to store in memvar
 *
 */
void hb_memvarSetValue( PHB_SYMB pMemvarSymb, HB_ITEM_PTR pItem )
{
   HB_THREAD_STUB
   PHB_DYNS pDyn;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarSetValue(%p, %p)", pMemvarSymb, pItem));

   #ifdef HB_THREAD_SUPPORT
   // we must find the thread specific name
   pDyn = s_memvarThGetName( pMemvarSymb->szName, &HB_VM_STACK );
   #else
   pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;
   #endif
   if( pDyn )
   {
      HB_TRACE(HB_TR_INFO, ("Memvar item (%i)(%s) assigned", pDyn->hMemvar , pMemvarSymb->szName));

      if( pDyn->hMemvar )
      {
         /* value is already created */
         HB_ITEM_PTR pSetItem;

         pSetItem = &s_globalTable[ pDyn->hMemvar ].item;

         // JC1: the variable we have now can't be destroyed in the meanwhile.
         // It could be changed, but this is a race condition that must be
         // prevented at prg level.
         if( HB_IS_BYREF( pSetItem ) )
         {
            pSetItem = hb_itemUnRef( pSetItem );
         }

         hb_itemCopy( pSetItem, pItem );
      }
      else
      {
         /* assignment to undeclared memvar - PRIVATE is assumed
          */
         hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pItem );
      }

      /* Remove MEMOFLAG if exists (assignment from field). */
      s_globalTable[ pDyn->hMemvar ].item.type &= ~HB_IT_MEMOFLAG;
   }
   else
   {
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
   }
}

ERRCODE hb_memvarGet( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   HB_THREAD_STUB
   PHB_DYNS pDyn;
   ERRCODE bSuccess = FAILURE;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGet(%p, %p)", pItem, pMemvarSymb));

   #ifdef HB_THREAD_SUPPORT
   // we must find the thread specific name
   pDyn = s_memvarThGetName( pMemvarSymb->szName, &HB_VM_STACK );
   #else
   pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;
   #endif

   if( pDyn )
   {
      HB_TRACE(HB_TR_INFO, ("Memvar item (%i)(%s) queried", pDyn->hMemvar, pMemvarSymb->szName));

      if( pDyn->hMemvar )
      {
         /* value is already created
          */
         HB_ITEM_PTR pGetItem = &s_globalTable[ pDyn->hMemvar ].item;

         if( HB_IS_BYREF( pGetItem ) )
         {
            hb_itemCopy( pItem, hb_itemUnRef( pGetItem ) );
         }
         else
         {
            hb_itemCopy( pItem, pGetItem );
         }

         bSuccess = SUCCESS;
      }
   }
   else
   {
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
   }

   return bSuccess;
}

void hb_memvarGetValue( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetValue(%p, %p)", pItem, pMemvarSymb));

   if( hb_memvarGet( pItem, pMemvarSymb ) == FAILURE )
   {
      /* Generate an error with retry possibility
       * (user created error handler can create this variable)
       */
      USHORT uiAction = E_RETRY;
      HB_ITEM_PTR pError;

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003, NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );

         if( uiAction == E_RETRY )
         {
            if( hb_memvarGet( pItem, pMemvarSymb ) == SUCCESS )
            {
               uiAction = E_DEFAULT;
            }
         }
      }

      hb_errRelease( pError );
   }
}

void hb_memvarGetRefer( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   HB_THREAD_STUB
   PHB_DYNS pDyn;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetRefer(%p, %p)", pItem, pMemvarSymb));
   #ifdef HB_THREAD_SUPPORT
   // we must find the thread specific name
   pDyn = s_memvarThGetName( pMemvarSymb->szName, &HB_VM_STACK);
   #else
   pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;
   #endif

   if( pDyn )
   {
      HB_TRACE(HB_TR_INFO, ("Memvar item (%i)(%s) referenced", pDyn->hMemvar, pMemvarSymb->szName));

      if( pDyn->hMemvar )
      {
         PHB_ITEM pReference;

         pReference = &s_globalTable[ pDyn->hMemvar ].item;

         if( pReference->type == HB_IT_STRING && pReference->item.asString.bStatic )
         {
            char *sString = (char*) hb_xgrab( pReference->item.asString.length + 1 );

            memcpy( sString, pReference->item.asString.value, pReference->item.asString.length + 1 );

            pReference->item.asString.value = sString;
            pReference->item.asString.bStatic = FALSE;
            pReference->item.asString.puiHolders = (USHORT*) hb_xgrab( sizeof( USHORT ) );
            *( pReference->item.asString.puiHolders ) = 1;
         }

         /* value is already created */
         pItem->type = HB_IT_BYREF | HB_IT_MEMVAR;
         pItem->item.asMemvar.offset = 0;
         pItem->item.asMemvar.value = pDyn->hMemvar;
         pItem->item.asMemvar.itemsbase = &s_globalTable;
         ++s_globalTable[ pDyn->hMemvar ].counter;
      }
      else
      {
         /* Generate an error with retry possibility
          * (user created error handler can make this variable accessible)
          */
         USHORT uiAction = E_RETRY;
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003, NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );

            if( uiAction == E_RETRY )
            {
               if( pDyn->hMemvar )
               {
                  /* value is already created */
                  pItem->type = HB_IT_BYREF | HB_IT_MEMVAR;
                  pItem->item.asMemvar.offset = 0;
                  pItem->item.asMemvar.value = pDyn->hMemvar;

                  pItem->item.asMemvar.itemsbase = &s_globalTable;
                  ++s_globalTable[ pDyn->hMemvar ].counter;

                  uiAction = E_DEFAULT;
               }
            }
         }

         hb_errRelease( pError );
      }
   }
   else
   {
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
   }
}

/*
 */
void hb_memvarNewParameter( PHB_SYMB pSymbol, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarNewParameter(%p, %p)", pSymbol, pValue));

   hb_memvarCreateFromDynSymbol( pSymbol->pDynSym, HB_MV_PRIVATE, pValue );
}

char * hb_memvarGetStrValuePtr( char * szVarName, ULONG *pulLen )
{
   HB_THREAD_STUB

   HB_ITEM itName;
   HB_DYNS_PTR pDynVar;
   char * szValue = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetStrValuePtr(%s, %p)", szVarName, pulLen));

   itName.type = HB_IT_STRING;
   itName.item.asString.value  = szVarName;
   itName.item.asString.length = *pulLen;
   itName.item.asString.bStatic = TRUE;
   itName.item.asString.puiHolders = NULL;

   hb_dynsymLock();
   pDynVar = hb_memvarFindSymbol( &itName );

   if( pDynVar )
   {
      /* there is dynamic symbol with the requested name - check if it is
       * a memvar variable
       */
      if( pDynVar->hMemvar )
      {
         /* variable contains some data
          */
         HB_ITEM_PTR pItem = &s_globalTable[ pDynVar->hMemvar ].item;
         hb_dynsymUnlock();

         if( HB_IS_BYREF( pItem ) )
         {
            pItem = hb_itemUnRef( pItem );   /* it is a PARAMETER variable */
         }

         if( HB_IS_STRING( pItem ) )
         {
            szValue = pItem->item.asString.value;
            *pulLen = pItem->item.asString.length;
         }
      }
      else
      {
         hb_dynsymUnlock();
      }
   }
   else
   {
      hb_dynsymUnlock();
   }

   return szValue;
}

/*
 * This function creates a value for memvar variable
 *
 * pMemvar - an item that stores the name of variable - it can be either
 *          the HB_IT_SYMBOL (if created by PUBLIC statement) or HB_IT_STRING
 *          (if created by direct call to __PUBLIC function)
 * bScope - the scope of created variable - if a variable with the same name
 *          exists already then it's value is hidden by new variable with
 *          passed scope
 * pValue - optional item used to initialize the value of created variable
 *          or NULL
 *
 */
void hb_memvarCreateFromItem( PHB_ITEM pMemvar, BYTE bScope, PHB_ITEM pValue )
{
   HB_THREAD_STUB
   PHB_DYNS pDynVar = NULL;


   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCreateFromItem(%p, %d, %p)", pMemvar, bScope, pValue));

   /* find dynamic symbol or creeate one */
   if( HB_IS_SYMBOL( pMemvar ) )
   {
      #ifdef HB_THREAD_SUPPORT
         pDynVar = s_memvarThGetName( pMemvar->item.asSymbol.value->szName, &HB_VM_STACK );
      #else
         pDynVar = hb_dynsymGet( pMemvar->item.asSymbol.value->szName );
      #endif
   }
   else if( HB_IS_STRING( pMemvar ) )
   {
      #ifdef HB_THREAD_SUPPORT
         pDynVar = s_memvarThGetName( pMemvar->item.asString.value, &HB_VM_STACK );
      #else
         pDynVar = hb_dynsymGet( pMemvar->item.asString.value );
      #endif
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3008, NULL, "&", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }

   if( pDynVar )
   {
      hb_memvarCreateFromDynSymbol( pDynVar, bScope, pValue );
   }
}

static void hb_memvarCreateFromDynSymbol( PHB_DYNS pDynVar, BYTE bScope, PHB_ITEM pValue )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCreateFromDynSymbol(%p, %d, %p)", pDynVar, bScope, pValue));

   if( bScope & VS_PUBLIC )
   {
      /* If the variable with the same name exists already
       * then the current value have to be unchanged
       */
      if( ! pDynVar->hMemvar )
      {
         pDynVar->hMemvar = hb_memvarValueNew( pValue, TRUE );

         if( ! pValue )
         {
            /* new PUBLIC variable - initialize it to .F.
             */
            s_globalTable[ pDynVar->hMemvar ].item.type = HB_IT_LOGICAL;

            /* NOTE: PUBLIC variables named CLIPPER and HARBOUR are initialized */
            /*       to .T., this is normal Clipper behaviour. [vszakats] */

            if( strcmp( pDynVar->pSymbol->szName, "HARBOUR" ) == 0 || strcmp( pDynVar->pSymbol->szName, "CLIPPER" ) == 0 )
            {
               s_globalTable[ pDynVar->hMemvar ].item.item.asLogical.value = TRUE;
            }
            else
            {
               s_globalTable[ pDynVar->hMemvar ].item.item.asLogical.value = FALSE;
            }
         }
      }
   }
   else
   {
      /* We need to store the handle to the value of variable that is
       * visible at this moment so later we can restore this value when
       * the new variable will be released
       */
      HB_HANDLE hCurrentValue = pDynVar->hMemvar;

      pDynVar->hMemvar = hb_memvarValueNew( pValue, TRUE );
      s_globalTable[ pDynVar->hMemvar ].hPrevMemvar = hCurrentValue;
      /* Add this variable to the PRIVATE variables stack
       */
      hb_memvarAddPrivate( pDynVar );
   }
}

/* This function releases all memory occupied by a memvar variable
 * It also restores the value that was hidden if there is another
 * PRIVATE variable with the same name.
 */
static void hb_memvarRelease( HB_ITEM_PTR pMemvar )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarRelease(%p)", pMemvar));

   if( HB_IS_STRING( pMemvar ) )
   {
      ULONG ulBase = s_privateStackCnt;

      /* Find the variable with a requested name that is currently visible
       * Start from the top of the stack.
       */
      while( ulBase > 0 )
      {
         PHB_DYNS pDynVar;

         --ulBase;
         pDynVar = s_privateStack[ ulBase ];

         /* reset current value to NIL - the overriden variables will be
         * visible after exit from current procedure
         */
         if( pDynVar->hMemvar )
         {
            if( hb_stricmp( pDynVar->pSymbol->szName, pMemvar->item.asString.value ) == 0 )
            {
               PHB_ITEM pRef;

               pRef = &s_globalTable[ pDynVar->hMemvar ].item;

               if( HB_IS_COMPLEX( pRef ) )
               {
                  hb_itemClear( pRef );
               }
               else
               {
                  pRef->type = HB_IT_NIL;
               }

               return;
            }
         }
      }

      // No match found for PRIVATEs - try PUBLICs.
      hb_memvarReleasePublic( pMemvar );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 3008, NULL, "RELEASE", 1, hb_paramError( 1 ) );
   }
}


/* This function releases all memory occupied by a memvar variable and
 * assigns NIL value - it releases variables created in current
 * procedure only.
 * The scope of released variables are specified using passed name's mask
 */
static void hb_memvarReleaseWithMask( char *szMask, BOOL bInclude )
{
   HB_THREAD_STUB

   ULONG ulBase = s_privateStackCnt;
   PHB_DYNS pDynVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarReleaseWithMask(%s, %d)", szMask, (int) bInclude));

   while( ulBase > s_privateStackBase )
   {
      --ulBase;
      pDynVar = s_privateStack[ ulBase ];

      /* reset current value to NIL - the overriden variables will be
       * visible after exit from current procedure
       */
      if( pDynVar->hMemvar )
      {
         PHB_ITEM pRef;

         pRef = &s_globalTable[ pDynVar->hMemvar ].item;

         if( bInclude )
         {
            if( ( szMask[ 0 ] == '*') || hb_strMatchRegExp( pDynVar->pSymbol->szName, szMask ) )
            {
               if( HB_IS_COMPLEX( pRef ) )
               {
                  hb_itemClear( pRef );
               }
               else
               {
                  pRef->type = HB_IT_NIL;
               }
            }
         }
         else if( ! hb_strMatchRegExp( pDynVar->pSymbol->szName, szMask ) )
         {
            if( HB_IS_COMPLEX( pRef ) )
            {
               hb_itemClear( pRef );
            }
            else
            {
               pRef->type = HB_IT_NIL;
            }
         }
      }
   }
}

/* Checks if passed dynamic symbol is a variable and returns its scope
 */
static int hb_memvarScopeGet( PHB_DYNS pDynVar )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarScopeGet(%p)", pDynVar));

   if( pDynVar->hMemvar == 0 )
   {
      return HB_MV_UNKNOWN;
   }
   else
   {
      ULONG ulBase = s_privateStackCnt;    /* start from the top of the stack */
      int iMemvar = HB_MV_PUBLIC;

      while( ulBase )
      {
         --ulBase;

         if( pDynVar == s_privateStack[ ulBase ] )
         {
            if( ulBase >= s_privateStackBase )
            {
               iMemvar = HB_MV_PRIVATE_LOCAL;
            }
            else
            {
               iMemvar = HB_MV_PRIVATE_GLOBAL;
            }

            ulBase = 0;
         }
      }

      return iMemvar;
   }
}

/* This function checks the scope of passed variable name
 */
int hb_memvarScope( char * szVarName )
{
   HB_THREAD_STUB
   int iMemvar;
   PHB_DYNS pDynVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarScope(%s)", szVarName));

   #ifdef HB_THREAD_SUPPORT
      pDynVar = s_memvarThFindName( szVarName, &HB_VM_STACK );
   #else
      pDynVar = hb_dynsymFindName( szVarName );
   #endif

   if( pDynVar )
   {
      iMemvar = hb_memvarScopeGet( pDynVar );
   }
   else
   {
      iMemvar = HB_MV_NOT_FOUND;
   }

   return iMemvar;
}

/* Releases memory occupied by a variable
 */
static HB_DYNS_FUNC( hb_memvarClear )
{
   HB_THREAD_STUB

   HB_SYMBOL_UNUSED( Cargo );

   if( pDynSymbol->hMemvar )
   {
      s_globalTable[ pDynSymbol->hMemvar ].counter = 1;
      hb_memvarValueDecRef( pDynSymbol->hMemvar );
      pDynSymbol->hMemvar = 0;
   }

   return TRUE;
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and
 * increments the counter eventually
 */
static HB_DYNS_FUNC( hb_memvarCountPublics )
{
   if( hb_memvarScopeGet( pDynSymbol ) == HB_MV_PUBLIC )
   {
      ( * ( ( int * )Cargo ) )++;
   }

   return TRUE;
}

static HB_DYNS_FUNC( hb_memvarReleasePublicWorker )
{
   HB_THREAD_STUB

   if( hb_memvarScopeGet( pDynSymbol ) == HB_MV_PUBLIC )
   {
      if( hb_stricmp( pDynSymbol->pSymbol->szName, (char *) Cargo ) == 0 )
      {
         s_globalTable[ pDynSymbol->hMemvar ].counter = 1;
         hb_memvarValueDecRef( pDynSymbol->hMemvar );
         pDynSymbol->hMemvar = 0;
         return FALSE;
      }
   }

   return TRUE;
}

void hb_memvarReleasePublic( PHB_ITEM pMemVar )
{
   char *sPublic = pMemVar->item.asString.value;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarReleasePublic(%p)", pMemVar ));

   hb_dynsymEval( hb_memvarReleasePublicWorker, ( void * ) sPublic );
}

/* Count the number of variables with given scope
 */
static int hb_memvarCount( int iScope )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCount(%d)", iScope));

   if( iScope == HB_MV_PUBLIC )
   {
      int iPublicCnt = 0;

      hb_dynsymEval( hb_memvarCountPublics, ( void * ) &iPublicCnt );

      return iPublicCnt;
   }
   else
   {
      return s_privateStackCnt;  /* number of PRIVATE variables */
   }
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and returns
 * a pointer to its dynamic symbol
 */
static HB_DYNS_FUNC( hb_memvarFindPublicByPos )
{
   BOOL bCont = TRUE;

   if( hb_memvarScopeGet( pDynSymbol ) == HB_MV_PUBLIC )
   {
      struct mv_PUBLIC_var_info *pStruPub = (struct mv_PUBLIC_var_info *) Cargo;

      if( pStruPub->iPos-- == 0 )
      {
         pStruPub->bFound  = TRUE;
         pStruPub->pDynSym = pDynSymbol;
         bCont =FALSE;
      }
   }

   return bCont;
}

/* Returns the pointer to item that holds a value of variable (or NULL if
 * not found). It fills also the pointer to the variable name
 * Both pointers points to existing and used data - they shouldn't be
 * deallocated.
 */
static HB_ITEM_PTR hb_memvarDebugVariable( int iScope, int iPos, char * *pszName )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pValue = NULL;
   *pszName = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDebugVariable(%d, %d, %p)", iScope, iPos, pszName));

   if( iPos > 0 )
   {
      --iPos;

      if( iScope == HB_MV_PUBLIC )
      {
         struct mv_PUBLIC_var_info struPub;

         struPub.iPos   = iPos;
         struPub.bFound = FALSE;
         /* enumerate existing dynamic symbols and fill this structure
          * with info for requested PUBLIC variable
          */
         hb_dynsymEval( hb_memvarFindPublicByPos, ( void * ) &struPub );

         if( struPub.bFound )
         {
            pValue =&s_globalTable[ struPub.pDynSym->hMemvar ].item;
            *pszName =struPub.pDynSym->pSymbol->szName;
         }
      }
      else
      {
         if( ( ULONG ) iPos < s_privateStackCnt )
         {
            HB_DYNS_PTR pDynSym = s_privateStack[ iPos ];
            pValue =&s_globalTable[ pDynSym->hMemvar ].item;
            *pszName = pDynSym->pSymbol->szName;
         }
      }
   }

   return pValue;
}

/* JC1 NOTE: Not locking it because it is necessary to lock the caller. */
static HB_DYNS_PTR hb_memvarFindSymbol( HB_ITEM_PTR pName )
{
   HB_DYNS_PTR pDynSym = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarFindSymbol(%p)", pName));


   if( pName )
   {
      ULONG ulLen = pName->item.asString.length;

      if( ulLen )
      {
         #ifdef HB_THREAD_SUPPORT
            char szNewName[270];
            HB_THREAD_STUB;
            sprintf( szNewName, ":TH:%d:%s", HB_VM_STACK.th_vm_id, pName->item.asString.value );
            pDynSym = hb_dynsymFindName( szNewName );
         #else
            pDynSym = hb_dynsymFindName( pName->item.asString.value );
         #endif
      }
   }

   return pDynSym;
}

/* ************************************************************************** */

HB_FUNC( __MVPUBLIC )
{
   HB_THREAD_STUB

   int iCount = hb_pcount();

   if( iCount )
   {
      int i;

      for( i = 1; i <= iCount; i++ )
      {
         PHB_ITEM pMemvar = hb_param( i, HB_IT_ANY );

         if( pMemvar )
         {
            if( HB_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               ( &VarItem )->type = HB_IT_NIL;

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_memvarCreateFromItem( &VarItem, VS_PUBLIC, NULL );

                  if( HB_IS_COMPLEX( &VarItem ) )
                  {
                     hb_itemClear( &VarItem );
                  }
               }
            }
            else
            {
               hb_memvarCreateFromItem( pMemvar, VS_PUBLIC, NULL );
            }
         }
      }
   }
}

HB_FUNC( __MVPRIVATE )
{
   HB_THREAD_STUB

   int iCount = hb_pcount();

   if( iCount )
   {
      int i;

      for( i = 1; i <= iCount; i++ )
      {
         PHB_ITEM pMemvar = hb_param( i, HB_IT_ANY );

         if( pMemvar )
         {
            if( HB_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               ( &VarItem )->type = HB_IT_NIL;

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_memvarCreateFromItem( &VarItem, VS_PRIVATE, NULL );

                  if( HB_IS_COMPLEX( &VarItem ) )
                  {
                     hb_itemClear( &VarItem );
                  }
               }
            }
            else
            {
               hb_memvarCreateFromItem( pMemvar, VS_PRIVATE, NULL );
            }
         }
      }
   }
}

HB_FUNC( __MVXRELEASE )
{
   HB_THREAD_STUB

   int iCount = hb_pcount();

   if( iCount )
   {
      int i;

      for( i = 1; i <= iCount; i++ )
      {
         PHB_ITEM pMemvar = hb_param( i, HB_IT_ANY );

         if( pMemvar )
         {
            if( HB_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               ( &VarItem )->type = HB_IT_NIL;

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_memvarRelease( &VarItem );

                  if( HB_IS_COMPLEX( &VarItem ) )
                  {
                     hb_itemClear( &VarItem );
                  }
               }
            }
            else
            {
               hb_memvarRelease( pMemvar );
            }
         }
      }
   }
}

HB_FUNC( __MVRELEASE )
{
   HB_THREAD_STUB

   int iCount = hb_pcount();

   if( iCount )
   {
      PHB_ITEM pMask = hb_param( 1, HB_IT_STRING );

      if( pMask )
      {
         BOOL bIncludeVar;

         if( iCount > 1 )
         {
            bIncludeVar = hb_parl( 2 );
         }
         else
         {
            bIncludeVar = TRUE;
         }

         if( pMask->item.asString.value[ 0 ] == '*' )
         {
            bIncludeVar = TRUE;   /* delete all memvar variables */
         }

         hb_memvarReleaseWithMask( pMask->item.asString.value, bIncludeVar );
      }
   }
}

HB_FUNC( __MVSCOPE )
{
   HB_THREAD_STUB

   int iMemvar = HB_MV_ERROR;

   if( hb_pcount() )
   {
      PHB_ITEM pVarName = hb_param( 1, HB_IT_STRING );

      if( pVarName )
      {
         iMemvar = hb_memvarScope( pVarName->item.asString.value );
      }
   }

   hb_retni( iMemvar );
}

HB_FUNC( __MVCLEAR )
{
   hb_dynsymEval( hb_memvarClear, NULL );
}

HB_FUNC( __MVDBGINFO )
{
   HB_THREAD_STUB

   int iCount = hb_pcount();

   if( iCount == 1 )          /* request for a number of variables */
   {
      hb_retni( hb_memvarCount( hb_parni( 1 ) ) );
   }

   else if( iCount >= 2 )     /* request for a value of variable */
   {
      HB_ITEM_PTR pValue;
      char * szName;

      pValue = hb_memvarDebugVariable( hb_parni( 1 ), hb_parni( 2 ), &szName );

      if( pValue )
      {
         /*the requested variable was found
          */
         if( iCount >= 3 && ISBYREF( 3 ) )
         {
            /* we have to use this variable regardless of its current value
             */
            HB_ITEM_PTR pName = hb_param( 3, HB_IT_ANY );

            hb_itemPutC( pName, szName ); /* clear an old value and copy a new one */
            /* szName points directly to a symbol name - it cannot be released
             */
         }

         hb_itemCopy( &(HB_VM_STACK.Return), pValue );
         /* pValue points directly to the item structure used by this variable
          * this item cannot be released
          */
      }
      else
      {
         hb_ret(); /* return NIL value */

         if( iCount >= 3 && ISBYREF( 3 ) )
         {
            /* we have to use this variable regardless of its current value
             */
            HB_ITEM_PTR pName = hb_param( 3, HB_IT_ANY );

            hb_itemPutC( pName, "?" ); /* clear an old value and copy a new one */
         }
      }
   }
}

HB_FUNC( __MVEXIST )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pName = hb_param( 1, HB_IT_STRING );
   PHB_DYNS pDyn = NULL;

   hb_dynsymLock();
   hb_retl( pName && ( ( pDyn = hb_memvarFindSymbol( pName ) ) != NULL ) && pDyn->hMemvar );
   hb_dynsymUnlock();
}

HB_FUNC( __MVGET )
{
   HB_ITEM_PTR pName = hb_param( 1, HB_IT_STRING );

   if( pName )
   {
      HB_DYNS_PTR pDynVar;

      hb_dynsymLock();
      pDynVar = hb_memvarFindSymbol( pName );

      if( pDynVar )
      {
         HB_ITEM retValue;
         PHB_SYMB pSymbol;

         pSymbol = pDynVar->pSymbol;
         hb_dynsymUnlock();

         ( &retValue )->type = HB_IT_NIL;
         hb_memvarGetValue( &retValue, pSymbol );

         hb_itemReturn( &retValue );
      }
      else
      {
         USHORT uiAction = E_RETRY;
         HB_ITEM_PTR pError;

         /* Generate an error with retry possibility
          * (user created error handler can create this variable)
          */
         hb_dynsymUnlock();


         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                 NULL, pName->item.asString.value, 0, EF_CANRETRY );

         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );

            if( uiAction == E_RETRY )
            {
               hb_dynsymLock();
               pDynVar = hb_memvarFindSymbol( pName );

               if( pDynVar )
               {
                  HB_ITEM retValue;
                  PHB_SYMB pSymbol = pDynVar->pSymbol;

                  hb_dynsymUnlock();
                  ( &retValue )->type = HB_IT_NIL;

                  hb_memvarGetValue( &retValue, pSymbol );

                  hb_itemReturn( &retValue );

                  uiAction = E_DEFAULT;
               }
               else
               {
                  hb_dynsymUnlock();
               }
            }
         }

         hb_errRelease( pError );
      }
   }
   else
   {
      /* either the first parameter is not specified or it has a wrong type
       * (it must be a string)
       * This is not a critical error - we can continue normal processing
       */
      /* TODO: This should be expanded a little to report a passed incorrect
       * value to the error handler
       */
      hb_errRT_BASE_SubstR( EG_ARG, 3009, NULL, NULL, 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( __MVPUT )
{
   HB_THREAD_STUB

   HB_ITEM_PTR pName = hb_param( 1, HB_IT_STRING );
   HB_ITEM nil;
   HB_ITEM_PTR pValue = &nil;

   nil.type = HB_IT_NIL;

   if( hb_pcount() >= 2 )
   {
      pValue = hb_param( 2, HB_IT_ANY );
   }

   if( pName )
   {
      /* the first parameter is a string with not empty variable name
       */
      HB_DYNS_PTR pDynVar;

      hb_dynsymLock();
      pDynVar = hb_memvarFindSymbol( pName );

      if( pDynVar )
      {
         /* variable was declared somwhere - assign a new value
          */
         PHB_SYMB pSymbol = pDynVar->pSymbol;
         hb_dynsymUnlock();
         hb_memvarSetValue( pSymbol, pValue );
      }
      else
      {
         PHB_DYNS pDyn;
         hb_dynsymUnlock();

         /* attempt to assign a value to undeclared variable
          * create the PRIVATE one
          */
         #ifdef HB_THREAD_SUPPORT
            pDyn = s_memvarThGetName( pName->item.asString.value, &HB_VM_STACK );
         #else
            pDyn = hb_dynsymGet( pName->item.asString.value );
         #endif

         hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pValue );
      }

      hb_itemForwardValue( &(HB_VM_STACK.Return), pValue );
   }
   else
   {
      /* either the first parameter is not specified or it has a wrong type
       * (it must be a string)
       * This is not a critical error - we can continue normal processing
       */
      /* TODO: This should be expanded a little to report a passed incorrect
       * value to the error handler
       */
      HB_ITEM_PTR pRetValue = hb_errRT_BASE_Subst( EG_ARG, 3010, NULL, NULL, 1, hb_paramError( 1 ) );

      if( pRetValue )
      {
         hb_itemRelease( pRetValue );
      }

      hb_itemForwardValue( &(HB_VM_STACK.Return), pValue );
   }
}

#define HB_MEM_REC_LEN          32
#define HB_MEM_NUM_LEN          8

typedef struct
{
   char * pszMask;
   BOOL bIncludeMask;
   BYTE * buffer;
   FHANDLE fhnd;
} MEMVARSAVE_CARGO;

/* saves a variable to a mem file already open */

static HB_DYNS_FUNC( hb_memvarSave )
{
   HB_THREAD_STUB
   char * pszMask    = ( ( MEMVARSAVE_CARGO * ) Cargo )->pszMask;
   BOOL bIncludeMask = ( ( MEMVARSAVE_CARGO * ) Cargo )->bIncludeMask;
   BYTE * buffer     = ( ( MEMVARSAVE_CARGO * ) Cargo )->buffer;
   FHANDLE fhnd      = ( ( MEMVARSAVE_CARGO * ) Cargo )->fhnd;

   /* NOTE: Harbour name lengths are not limited, but the .MEM file
            structure is not flexible enough to allow for it.
            [vszakats] */

   if( pDynSymbol->hMemvar )
   {
      BOOL bMatch = ( pszMask[ 0 ] == '*' || hb_strMatchRegExp( pDynSymbol->pSymbol->szName, pszMask ) );

      PHB_ITEM pItem;

      pItem = &s_globalTable[ pDynSymbol->hMemvar ].item;

      /* Process it if it matches the passed mask */
      if( bIncludeMask ? bMatch : ! bMatch )
      {
         /* NOTE: Clipper will not initialize the record buffer with
                  zeros, so they will look trashed. [vszakats] */
         memset( buffer, 0, HB_MEM_REC_LEN );

         /* NOTE: Save only the first 10 characters of the name */
         strncpy( ( char * ) buffer, pDynSymbol->pSymbol->szName, 10 );
         buffer[ 10 ] = '\0';

         if( HB_IS_STRING( pItem ) && ( hb_itemGetCLen( pItem ) + 1 ) <= SHRT_MAX )
         {
            /* Store the closing zero byte, too */
            USHORT uiLength = ( USHORT ) ( hb_itemGetCLen( pItem ) + 1 );

            buffer[ 11 ] = 'C' + 128;
            buffer[ 16 ] = HB_LOBYTE( uiLength );
            buffer[ 17 ] = HB_HIBYTE( uiLength );

            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWrite( fhnd, ( BYTE * ) hb_itemGetCPtr( pItem ), uiLength );
         }
         else if( HB_IS_DATE( pItem ) )
         {
            double dNumber = ( double ) hb_itemGetDL( pItem );

            buffer[ 11 ] = 'D' + 128;
            buffer[ 16 ] = 1;
            buffer[ 17 ] = 0;

            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWrite( fhnd, ( BYTE * ) &dNumber, sizeof( dNumber ) );
         }
         else if( HB_IS_NUMERIC( pItem ) )
         {
            double dNumber = hb_itemGetND( pItem );
            int iWidth;
            int iDec;

            hb_itemGetNLen( pItem, &iWidth, &iDec );

            buffer[ 11 ] = 'N' + 128;
#ifdef HB_C52_STRICT
/* NOTE: This is the buggy, but fully CA-Cl*pper compatible method. [vszakats] */
            buffer[ 16 ] = ( BYTE ) iWidth + ( HB_IS_DOUBLE( pItem ) ? iDec + 1 : 0 );
#else
/* NOTE: This would be the correct method, but Clipper is buggy here. [vszakats] */
            buffer[ 16 ] = ( BYTE ) iWidth + ( iDec == 0 ? 0 : iDec + 1 );
#endif
            buffer[ 17 ] = ( BYTE ) iDec;

            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWrite( fhnd, ( BYTE * ) &dNumber, sizeof( dNumber ) );
         }
         else if( HB_IS_LOGICAL( pItem ) )
         {
            BYTE byLogical[ 1 ];

            buffer[ 11 ] = 'L' + 128;
            buffer[ 16 ] = sizeof( BYTE );
            buffer[ 17 ] = 0;

            byLogical[ 0 ] = hb_itemGetL( pItem ) ? 1 : 0;

            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWrite( fhnd, byLogical, sizeof( BYTE ) );
         }
      }
   }
   return TRUE;
}

HB_FUNC( __MVSAVE )
{
   HB_THREAD_STUB

   /* Clipper also checks for the number of arguments here */
   if( hb_pcount() == 3 && ISCHAR( 1 ) && ISCHAR( 2 ) && ISLOG( 3 ) )
   {
      PHB_FNAME pFileName;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      FHANDLE fhnd;

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( pFileName->szExtension == NULL )
         pFileName->szExtension = ".mem";

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Create .MEM file */

      while( ( fhnd = hb_fsCreate( ( BYTE * ) szFileName, FC_NORMAL ) ) == FS_ERROR )
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_CREATE, 2006, NULL, szFileName, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }

      if( fhnd != FS_ERROR )
      {
         BYTE buffer[ HB_MEM_REC_LEN ];
         MEMVARSAVE_CARGO msc;

         msc.pszMask      = hb_parc( 2 );
         msc.bIncludeMask = hb_parl( 3 );
         msc.buffer       = buffer;
         msc.fhnd         = fhnd;

         /* Walk through all visible memory variables and save each one */

         hb_dynsymEval( hb_memvarSave, ( void * ) &msc );

         buffer[ 0 ] = '\x1A';
         hb_fsWrite( fhnd, buffer, 1 );

         hb_fsClose( fhnd );
      }
   }
   else
   {
      /* NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3x. [ckedem] */
      hb_errRT_BASE( EG_ARG, 2008, NULL, "__MSAVE", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* NOTE: There's an extension in Harbour, which makes it possible to only
         load (or not load) variable names with a specific name mask.
         [vszakats] */

HB_FUNC( __MVRESTORE )
{
   HB_THREAD_STUB

   /* Clipper checks for the number of arguments here here, but we cannot
      in Harbour since we have two optional parameters as an extension. */
   if( ISCHAR( 1 ) && ISLOG( 2 ) )
   {
      PHB_FNAME pFileName;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      FHANDLE fhnd;

      BOOL bAdditive = hb_parl( 2 );

      /* Clear all memory variables if not ADDITIVE */

      if( ! bAdditive )
      {
         hb_dynsymEval( hb_memvarClear, NULL );
      }

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( pFileName->szExtension == NULL )
      {
         pFileName->szExtension = ".mem";
      }

      if( pFileName->szPath == NULL )
      {
         pFileName->szPath = hb_set.HB_SET_DEFAULT;
      }

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Open .MEM file */

      while( ( fhnd = hb_fsOpen( ( BYTE * ) szFileName, FO_READ | FO_DENYWRITE | FO_PRIVATE ) ) == FS_ERROR )
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2005, NULL, szFileName, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 2, hb_paramError( 1 ), hb_paramError( 2 ) );

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }

      if( fhnd != FS_ERROR )
      {
         char * pszMask = ISCHAR( 3 ) ? hb_parc( 3 ) : "*";
         BOOL bIncludeMask = ISCHAR( 4 ) ? hb_parl( 4 ) : TRUE;
         BYTE buffer[ HB_MEM_REC_LEN ];

         while( hb_fsRead( fhnd, buffer, HB_MEM_REC_LEN ) == HB_MEM_REC_LEN )
         {
            PHB_ITEM pName = hb_itemPutC( NULL, ( char * ) buffer );
            USHORT uiType = ( USHORT ) ( buffer[ 11 ] - 128 );
            USHORT uiWidth = ( USHORT ) buffer[ 16 ];
            USHORT uiDec = ( USHORT ) buffer[ 17 ];
            PHB_ITEM pItem = NULL;

            switch( uiType )
            {
               case 'C':
               {
                  BYTE * pbyString;

                  uiWidth += uiDec * 256;
                  pbyString = ( BYTE * ) hb_xgrab( uiWidth );

                  if( hb_fsRead( fhnd, pbyString, uiWidth ) == uiWidth )
                     pItem = hb_itemPutCL( NULL, ( char * ) pbyString, uiWidth - 1 );

                  hb_xfree( pbyString );

                  break;
               }

               case 'N':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     pItem = hb_itemPutNLen( NULL, * ( double * ) &pbyNumber, uiWidth - ( uiDec ? ( uiDec + 1 ) : 0 ), uiDec );

                  break;
               }

               case 'D':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     pItem = hb_itemPutDL( NULL, ( LONG ) ( * ( double * ) &pbyNumber ) );

                  break;
               }

               case 'L':
               {
                  BYTE pbyLogical[ 1 ];

                  if( hb_fsRead( fhnd, pbyLogical, 1 ) == 1 )
                  {
                     pItem = hb_itemPutL( NULL, pbyLogical[ 0 ] != 0 );
                  }

                  break;
               }
            }

            if( pItem )
            {
               BOOL bMatch = ( pszMask[ 0 ] == '*' || hb_strMatchRegExp( hb_itemGetCPtr( pName ), pszMask ) );

               /* Process it if it matches the passed mask */
               if( bIncludeMask ? bMatch : ! bMatch )
               {
                  /* the first parameter is a string with not empty variable name */
                  HB_DYNS_PTR pDynVar;

                  hb_dynsymLock();
                  pDynVar = hb_memvarFindSymbol( pName );

                  if( pDynVar )
                  {
                     /* variable was declared somwhere - assign a new value */
                     PHB_SYMB pSymbol = pDynVar->pSymbol;
                     hb_dynsymUnlock();

                     hb_memvarSetValue( pSymbol, pItem );
                  }
                  else
                  {
                     PHB_DYNS pDyn;

                     hb_dynsymUnlock();
                     /* attempt to assign a value to undeclared variable create the PRIVATE one */
                     #ifdef HB_THREAD_SUPPORT
                        pDyn = s_memvarThGetName( pName->item.asString.value, &HB_VM_STACK );
                     #else
                        pDyn = hb_dynsymGet( pName->item.asString.value );
                     #endif
                     hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pItem );
                  }

                  hb_itemReturn( pItem );
               }

               hb_itemRelease( pItem );
            }

            hb_itemRelease( pName );
         }

         hb_fsClose( fhnd );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2007, NULL, "__MRESTORE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ----------------------------------------------------------------------- */
/* The garbage collector interface */
/* ----------------------------------------------------------------------- */


/* Mark all memvars as used so they will not be released by the
 * garbage collector
 */
//JC1: This is done inside a garbage collecting, that is done one
// thread at a time. No need to lock mutex.
void hb_memvarsIsMemvarRef( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsIsMemvarRef()"));

   if( s_globalTable )
   {
      ULONG ulCnt = s_globalLastFree;

      while( --ulCnt )
      {
         /* do not check detached variables - for these variables only
          * references from the eval stack are meaningfull for the GC
         */
         if( s_globalTable[ ulCnt ].counter && s_globalTable[ ulCnt ].hPrevMemvar != ( HB_HANDLE )-1 )
         {
            hb_gcItemRef( &s_globalTable[ ulCnt ].item );
         }
      }
   }
}

HB_HANDLE hb_memvarGetVarHandle( char *szName )
{
   HB_THREAD_STUB
   PHB_DYNS pDyn;

   #ifdef HB_THREAD_SUPPORT
      char szNewName[270];
      sprintf( szNewName, ":TH:%d:%s", HB_VM_STACK.th_vm_id, szName );
   #endif

   hb_dynsymLock();
   pDyn = hb_dynsymFindName( szName );

   if( pDyn != NULL )
   {
      HB_HANDLE hHand = pDyn->hMemvar;
      hb_dynsymUnlock();
      return hHand;
   }
   else
   {
      hb_dynsymUnlock();
      return 0; /* invalid handle */
   }
}

HB_EXPORT PHB_ITEM hb_memvarGetValueByHandle( HB_HANDLE hMemvar )
{
   HB_THREAD_STUB

   if( hMemvar && hMemvar < s_globalTableSize )
   {
      return &s_globalTable[ hMemvar ].item;
   }
   else
   {
      return NULL;
   }
}

static HB_DYNS_FUNC( hb_GetSymbolInfo )
{
  HB_THREAD_STUB
  if( pDynSymbol->hMemvar )
  {
    PHB_ITEM pArray   = ( ( PHB_ITEM ) Cargo );
    PHB_ITEM pItem = &s_globalTable[ pDynSymbol->hMemvar ].item;
    PHB_ITEM pSubItems = hb_itemArrayNew( 2 );
    PHB_ITEM pName = hb_itemPutC( NULL,pDynSymbol->pSymbol->szName);
    PHB_ITEM pValue = hb_itemNew(NULL);
    hb_itemCopy(pValue,pItem );
    hb_arraySet( pSubItems , 1 , pName ) ;
    hb_arraySet( pSubItems , 2 , pValue) ;
    hb_arrayAdd( pArray , pSubItems);
    hb_itemRelease( pName ) ;
    hb_itemRelease( pValue ) ;
    hb_itemRelease( pSubItems );
  }
  return TRUE;
}

HB_FUNC( __MVSYMBOLINFO )
{
  HB_THREAD_STUB
  PHB_ITEM pArray = hb_itemArrayNew( 0 );
  hb_dynsymEval( hb_GetSymbolInfo, ( void * ) pArray );
  hb_itemReturn( pArray);
//  hb_itemRelease( pArray ) ;
}
