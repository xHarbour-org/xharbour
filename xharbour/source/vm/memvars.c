/*
 * $Id$
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
 *
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_THREAD_OPTIMIZE_STACK

#include <ctype.h> /* for HB_TOUPPER() function */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h" /* for __MVSAVE()/__MVRESTORE() */
#include "hbdate.h"  /* for __MVSAVE()/__MVRESTORE() */
#include "hbcomp.h"  /* for VS_* macros */
#include "error.ch"
#include "hbmemvar.ch"
#include "hbset.h"
#include "hbstack.h"
#include "hbvm.h"
#include "classes.h"
#include "hbregex.h"

//JC1: under threads, we need this to be in thread stack
#ifndef HB_THREAD_SUPPORT
static PHB_DYNS *    s_privateStack       = NULL;
static HB_SIZE       s_privateStackSize   = 0;
static HB_SIZE       s_privateStackCnt    = 0;
static HB_SIZE       s_privateStackBase   = 0;

static HB_SIZE       s_globalTableSize    = 0;
static HB_SIZE       s_globalFirstFree    = 0;
static HB_SIZE       s_globalLastFree     = 0;
static PHB_VALUE  s_globalTable        = NULL;

#else

#define  s_privateStack       ( HB_VM_STACK.privateStack )
#define  s_privateStackSize   ( HB_VM_STACK.privateStackSize )
#define  s_privateStackCnt    ( HB_VM_STACK.privateStackCnt )
#define  s_privateStackBase   ( HB_VM_STACK.privateStackBase )

#define  s_globalTableSize    ( HB_VM_STACK.globalTableSize )
#define  s_globalFirstFree    ( HB_VM_STACK.globalFirstFree )
#define  s_globalLastFree     ( HB_VM_STACK.globalLastFree )
#define  s_globalTable        ( HB_VM_STACK.globalTable )

PHB_DYNS s_memvarThGetName( const char * szName, HB_STACK * pstack )
{
   /* Can NOT use HB_VM_STACK here!!! */
   if( pstack == &hb_stackMT || strncmp( szName, ":TH:", 4 ) == 0 )
      return hb_dynsymGet( szName );
   else
   {
      char szNewName[ 270 ];
      hb_snprintf( szNewName, sizeof( szNewName ), ":TH:%d:%s", pstack->th_vm_id, szName );
      return hb_dynsymGet( szNewName );
   }
}

PHB_DYNS s_memvarThFindName( char * szName, HB_STACK * pstack )
{
   /* Can NOT use HB_VM_STACK here!!! */
   if( pstack == &hb_stackMT || strncmp( szName, ":TH:", 4 ) == 0 )
      return hb_dynsymFindName( szName );
   else
   {
      char szNewName[ 270 ];
      hb_snprintf( szNewName, sizeof( szNewName ), ":TH:%d:%s", pstack->th_vm_id, szName );
      return hb_dynsymFindName( szNewName );
   }
}

#endif

#define TABLE_INITHB_VALUE    100
#define TABLE_EXPANDHB_VALUE  50

struct mv_PUBLIC_var_info
{
   int iPos;
   BOOL bFound;
   PHB_DYNS pDynSym;
};

static void hb_memvarCreateFromDynSymbol( PHB_DYNS, BYTE, PHB_ITEM );
static void hb_memvarAddPrivate( PHB_DYNS );
static PHB_DYNS hb_memvarFindSymbol( PHB_ITEM );
void hb_memvarReleasePublic( PHB_ITEM pMemVar );

/* Fake Clear all variable - the value will be cleared by subsequent GC scan
 * Should be called at application exit only
 */
void hb_memvarsClear( void )
{
   HB_THREAD_STUB
   HB_SIZE ulCnt = s_globalLastFree;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarsClear()" ) );

   if( s_globalTable )
   {
      while( --ulCnt )
      {
         if( s_globalTable[ ulCnt ].counter > 0 )
         {
            if( HB_IS_STRING( s_globalTable[ ulCnt ].pVarItem ) )
               hb_itemReleaseString( s_globalTable[ ulCnt ].pVarItem );

            s_globalTable[ ulCnt ].pVarItem->type = HB_IT_NIL;
         }
      }
   }
}

#ifndef HB_THREAD_SUPPORT
void hb_memvarsInit( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarsInit()" ) );

   s_globalTable        = ( PHB_VALUE ) hb_xgrab( sizeof( HB_VALUE ) * TABLE_INITHB_VALUE );
   s_globalTableSize    = TABLE_INITHB_VALUE;
   s_globalFirstFree    = 0;
   s_globalLastFree     = 1;

   s_privateStack       = ( PHB_DYNS * ) hb_xgrab( sizeof( PHB_DYNS ) * TABLE_INITHB_VALUE );
   s_privateStackSize   = TABLE_INITHB_VALUE;
   s_privateStackCnt    = s_privateStackBase = 0;
}

/* Releases all variable containers - the value will be cleared by subsequent GC scan
 * Should be called at application exit only
 */
void hb_memvarsRelease( void )
{
   HB_THREAD_STUB
   HB_SIZE ulCnt = s_globalLastFree;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarsRelease()" ) );

   if( s_globalTable )
   {
      while( --ulCnt )
      {
         if( s_globalTable[ ulCnt ].counter > 0 )
         {
            hb_xfree( s_globalTable[ ulCnt ].pVarItem );
            s_globalTable[ ulCnt ].pVarItem  = NULL;
            s_globalTable[ ulCnt ].counter   = 0;
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

void hb_memvarsInit( HB_STACK * pStack )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarsInit(%p)", pStack ) );

   pStack->globalTable        = ( PHB_VALUE ) hb_xgrab( sizeof( HB_VALUE ) * TABLE_INITHB_VALUE );
   pStack->privateStack       = ( PHB_DYNS * ) hb_xgrab( sizeof( PHB_DYNS ) * TABLE_INITHB_VALUE );
   pStack->globalTableSize    = TABLE_INITHB_VALUE;
   pStack->globalFirstFree    = 0;
   pStack->globalLastFree     = 1;

   pStack->privateStackSize   = TABLE_INITHB_VALUE;
   pStack->privateStackCnt    = pStack->privateStackBase = 0;
}

/* Releases all variable containers - the value will be cleared by subsequent GC scan
 * Should be called at application exit only
 */
void hb_memvarsRelease( HB_STACK * pStack )
{
   HB_SIZE ulCnt;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarsRelease(%p)", pStack ) );

   ulCnt = pStack->globalLastFree;

   if( pStack->globalTable )
   {
      while( --ulCnt )
      {
         if( pStack->globalTable[ ulCnt ].counter > 0 )
         {
            hb_xfree( pStack->globalTable[ ulCnt ].pVarItem );
            pStack->globalTable[ ulCnt ].pVarItem  = NULL;
            pStack->globalTable[ ulCnt ].counter   = 0;
         }
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
PHB_VALUE * hb_memvarValueBaseAddress( void )
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
HB_HANDLE hb_memvarValueNew( PHB_ITEM pSource, BOOL bTrueMemvar )
{
   HB_THREAD_STUB

   PHB_VALUE   pValue;
   HB_HANDLE      hValue; /* handle 0 is reserved */

   /* = 1 removed, since it's initialized in all branches. Caused a warning with Borland C++ */

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarValueNew(%p, %d)", pSource, ( int ) bTrueMemvar ) );

   if( s_globalFirstFree )
   {
      /* There are holes in the table - get a first available hole
       */
      hValue            = ( HB_HANDLE ) s_globalFirstFree;
      s_globalFirstFree = s_globalTable[ hValue ].hPrevMemvar;
   }
   else
   {
      /* Allocate the value from the end of table
       */
      if( s_globalLastFree < s_globalTableSize )
         hValue = ( HB_HANDLE ) s_globalLastFree++;
      else
      {
         /* No more free values in the table - expand the table
          */
         hValue            = ( HB_HANDLE ) s_globalTableSize;
         s_globalLastFree  = s_globalTableSize + 1;
         s_globalTableSize += TABLE_EXPANDHB_VALUE;
         s_globalTable     = ( PHB_VALUE ) hb_xrealloc( s_globalTable, sizeof( HB_VALUE ) * s_globalTableSize );
      }
   }

   pValue                  = s_globalTable + hValue;
   pValue->pVarItem        = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
   pValue->pVarItem->type  = HB_IT_NIL;
   pValue->counter         = 1;

   if( bTrueMemvar )
   {
      pValue->hPrevMemvar = 0;

      if( pSource )
      {
         hb_itemCopy( pValue->pVarItem, pSource );
         /* Remove MEMOFLAG if exists (assignment from field). */
         pValue->pVarItem->type &= ~HB_IT_MEMOFLAG;
      }
   }
   else
   {
      pValue->hPrevMemvar     = ( HB_HANDLE ) -1; /* detached variable */

      hb_itemRawCpy( pValue->pVarItem, pSource );
      pValue->pVarItem->type  &= ~HB_IT_DEFAULT;

#ifndef HB_ARRAY_USE_COUNTER
      if( pSource->type == HB_IT_ARRAY && pSource->item.asArray.value )
      {
         /* TraceLog( NULL, "Detached %p array: %p to %p\n", pSource, pSource->item.asArray.value, pValue->pVarItem );
          */
         hb_arrayResetHolder( pSource->item.asArray.value, pSource, pValue->pVarItem );
      }
      else if( pSource->type == HB_IT_BYREF && pSource->item.asRefer.offset == 0 )
         hb_arrayResetHolder( pSource->item.asRefer.BasePtr.pBaseArray, pSource, pValue->pVarItem );
#endif
   }

   HB_TRACE( HB_TR_INFO, ( "hb_memvarValueNew: memvar item created with handle %i", hValue ) );

   return hValue;
}

/* Detach local variable (swap current value with a memvar handle)
 */
PHB_ITEM hb_memvarDetachLocal( PHB_ITEM pLocal )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarDetachLocal(%p, %d)", pLocal, pLocal->type ) );

   /* This code is only for Harbour memvar management */
#if 0
   if( HB_IS_BYREF( pLocal ) && ! HB_IS_MEMVAR( pLocal ) )
   {
      PHB_ITEM pItem = pLocal;

      do
      {
         pLocal = hb_itemUnRefOnce( pLocal );
      }
      while( HB_IS_BYREF( pLocal ) && ! HB_IS_MEMVAR( pLocal ) && ( pLocal != pItem ) );
   }
#endif

   /* Change the value only if this variable is not referenced
    * by another codeblock yet.
    * In this case we have to copy the current value to a global memory
    * pool so it can be shared by codeblocks
    */
   if( ! HB_IS_MEMVAR( pLocal ) )
   {
      HB_HANDLE hMemvar = hb_memvarValueNew( pLocal, FALSE );

      pLocal->type                     = HB_IT_BYREF | HB_IT_MEMVAR;
      pLocal->item.asMemvar.itemsbase  = &s_globalTable;
      pLocal->item.asMemvar.offset     = 0;
      pLocal->item.asMemvar.value      = hMemvar;
   }
   return pLocal;
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarAddPrivate(%p)", pDynSym ) );

   /* Allocate the value from the end of table
    */
   if( s_privateStackCnt == s_privateStackSize )
   {
      /* No more free values in the table - expand the table
       */
      s_privateStackSize   += TABLE_EXPANDHB_VALUE;
      s_privateStack       = ( PHB_DYNS * ) hb_xrealloc( s_privateStack, sizeof( PHB_DYNS ) * s_privateStackSize );
   }

   s_privateStack[ s_privateStackCnt++ ] = pDynSym;
}

/*
 * This function returns current PRIVATE variables stack base
 */
HB_SIZE hb_memvarGetPrivatesBase( void )
{
   HB_THREAD_STUB

   HB_SIZE ulBase = s_privateStackBase;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarGetPrivatesBase()" ) );

   s_privateStackBase = s_privateStackCnt;
   return ulBase;
}

/*
 * This function releases PRIVATE variables created after passed base
 */
void hb_memvarSetPrivatesBase( HB_SIZE ulBase )
{
   HB_THREAD_STUB

   HB_HANDLE hVar, hOldValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarSetPrivatesBase(%lu)", ulBase ) );

   while( s_privateStackCnt > s_privateStackBase )
   {
      --s_privateStackCnt;
      hVar = s_privateStack[ s_privateStackCnt ]->hMemvar;

      if( hVar )
      {
         hOldValue = s_globalTable[ hVar ].hPrevMemvar;
         hb_memvarValueDecRef( hVar );

         if( s_globalTable[ hVar ].counter )
            s_globalTable[ hVar ].hPrevMemvar = ( HB_HANDLE ) -1;

         /*
          * Restore previous value for variables that were overridden
          */
         s_privateStack[ s_privateStackCnt ]->hMemvar = hOldValue;
      }
   }

   assert( ulBase <= s_privateStackBase );
   s_privateStackBase = ulBase;
}

/*
 * This function increases the number of references to passed global value
 *
 */
void hb_memvarValueIncRef( HB_HANDLE hValue )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarValueIncRef(%lu)", hValue ) );

   s_globalTable[ hValue ].counter++;

   HB_TRACE( HB_TR_INFO, ( "Memvar item (%i) increment refCounter=%li", hValue, s_globalTable[ hValue ].counter ) );
}

#ifndef HB_THREAD_SUPPORT
static void hb_memvarRecycle( HB_HANDLE hValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarRecycle(%lu)", hValue ) );

   if( s_globalFirstFree )
   {
      s_globalTable[ hValue ].hPrevMemvar = ( HB_HANDLE ) s_globalFirstFree;
      s_globalFirstFree                   = hValue;
   }
   else
   {
      /* first free value in the list */
      s_globalFirstFree                   = hValue;
      s_globalTable[ hValue ].hPrevMemvar = 0;
   }
}

#else
static void hb_memvarRecycleMT( HB_HANDLE hValue, HB_STACK * pStack )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarRecycleMT(%lu)", hValue ) );

   if( pStack->globalFirstFree )
   {
      pStack->globalTable[ hValue ].hPrevMemvar = ( HB_HANDLE ) pStack->globalFirstFree;
      pStack->globalFirstFree                   = hValue;
   }
   else
   {
      /* first free value in the list */
      pStack->globalFirstFree                   = hValue;
      pStack->globalTable[ hValue ].hPrevMemvar = 0;
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

   PHB_VALUE pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarValueDecRef(%lu)", hValue ) );

   pValue = s_globalTable + hValue;

   /* TraceLog( NULL, "Memvar item (%i) Counter: %li\n", hValue, pValue->counter );
    */

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
         if( HB_IS_COMPLEX( pValue->pVarItem ) )
            hb_itemClear( pValue->pVarItem );

         hb_xfree( pValue->pVarItem );
         pValue->pVarItem = NULL;

#ifndef HB_THREAD_SUPPORT
         hb_memvarRecycle( hValue );
#else
         hb_memvarRecycleMT( hValue, &HB_VM_STACK );
#endif

         HB_TRACE( HB_TR_INFO, ( "Memvar item (%i) deleted", hValue ) );
      }
   }
}

#ifdef HB_THREAD_SUPPORT
void hb_memvarValueDecRefMT( HB_HANDLE hValue, HB_STACK * pStack )
{
   PHB_VALUE pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarValueDecRef(%lu)", hValue ) );

   pValue = pStack->globalTable + hValue;

   HB_TRACE( HB_TR_INFO, ( "Memvar item (%i) decrement refCounter=%li", hValue, pValue->counter - 1 ) );

   if( pValue->counter > 0 )
   {
      if( --pValue->counter == 0 )
      {
         if( HB_IS_COMPLEX( pValue->pVarItem ) )
            hb_itemClearMT( pValue->pVarItem, pStack );

         hb_xfree( pValue->pVarItem );
         pValue->pVarItem = NULL;

         hb_memvarRecycleMT( hValue, pStack );

         HB_TRACE( HB_TR_INFO, ( "Memvar item (%i) deleted", hValue ) );
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
   PHB_VALUE pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarValueDecRef(%lu)", hValue ) );

   /* Might be called from hb_gcAll() after hb_memvarsRelease()
    * if HB_FM_STATISTICS defined.
    */
   if( s_globalTable == NULL )
      return;

   pValue = s_globalTable + hValue;

   HB_TRACE( HB_TR_INFO, ( "Memvar item (%i) decrement refCounter=%li", hValue, pValue->counter - 1 ) );

   /* Notice that Counter can be equal to 0.
    * This can happen if for example PUBLIC variable holds a codeblock
    * with detached variable. When hb_memvarsRelease() is called then
    * detached variable can be released before the codeblock. So if
    * the codeblock will be released later then it will try to release
    * again this detached variable.
    */

   if( pValue->counter > 0 )
   {
      if( --pValue->counter == 0 )
      {
         if( HB_IS_STRING( pValue->pVarItem ) )
         {
            hb_itemReleaseString( pValue->pVarItem );
            pValue->pVarItem->type = HB_IT_NIL;
         }

         hb_xfree( pValue->pVarItem );
         pValue->pVarItem = NULL;

#ifndef HB_THREAD_SUPPORT
         hb_memvarRecycle( hValue );
#else
         hb_memvarRecycleMT( hValue, &HB_VM_STACK );
#endif

         HB_TRACE( HB_TR_INFO, ( "Memvar item (%i) deleted", hValue ) );
      }
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
void hb_memvarSetValue( PHB_SYMB pMemvarSymb, PHB_ITEM pItem )
{
   HB_THREAD_STUB
   PHB_DYNS pDyn;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarSetValue(%p, %p)", pMemvarSymb, pItem ) );

#ifdef HB_THREAD_SUPPORT
   /* we must find the thread specific name */
   pDyn        = s_memvarThGetName( pMemvarSymb->szName, &HB_VM_STACK );
   #else
   pDyn        = ( PHB_DYNS ) pMemvarSymb->pDynSym;
#endif

   pItem->type &= ~HB_IT_MEMOFLAG;

   if( pDyn )
   {
      HB_TRACE( HB_TR_INFO, ( "Memvar item (%i)(%s) assigned", pDyn->hMemvar, pMemvarSymb->szName ) );

      if( pDyn->hMemvar )
      {
         /* value is already created */
         PHB_ITEM pSetItem = s_globalTable[ pDyn->hMemvar ].pVarItem;

         /* JC1: the variable we have now can't be destroyed in the meanwhile.
          * It could be changed, but this is a race condition that must be
          * prevented at prg level.
          */
         if( HB_IS_BYREF( pSetItem ) )
            pSetItem = hb_itemUnRef( pSetItem );

         if( HB_IS_OBJECT( pSetItem ) && ( hb_objGetOpOver( pSetItem ) & HB_CLASS_OP_ASSIGN ) )
            hb_vmOperatorCall( pSetItem, pItem, "__OPASSIGN", NULL, 0, pSetItem );
         else
            hb_itemCopy( pSetItem, pItem );

         /* Count this new value.
          * s_globalTable[ pDyn->hMemvar ].counter = 1;
          */
      }
      else
         /* assignment to undeclared memvar - PRIVATE is assumed */
         hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pItem );

      /* Remove MEMOFLAG if exists (assignment from field). */
      s_globalTable[ pDyn->hMemvar ].pVarItem->type &= ~HB_IT_MEMOFLAG;
   }
   else
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
}

HB_ERRCODE hb_memvarGet( PHB_ITEM pItem, PHB_SYMB pMemvarSymb )
{
   HB_THREAD_STUB
   PHB_DYNS    pDyn;
   HB_ERRCODE  bSuccess = FAILURE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarGet(%p, %p)", pItem, pMemvarSymb ) );

#ifdef HB_THREAD_SUPPORT
   /* we must find the thread specific name */
   pDyn  = s_memvarThGetName( pMemvarSymb->szName, &HB_VM_STACK );
#else
   pDyn  = ( PHB_DYNS ) pMemvarSymb->pDynSym;
#endif

   if( pDyn )
   {
      HB_TRACE( HB_TR_INFO, ( "Memvar item (%i)(%s) queried", pDyn->hMemvar, pMemvarSymb->szName ) );

      if( pDyn->hMemvar )
      {
         /* value is already created
          */
         PHB_ITEM pGetItem = s_globalTable[ pDyn->hMemvar ].pVarItem;

         if( HB_IS_BYREF( pGetItem ) )
            hb_itemCopy( pItem, hb_itemUnRef( pGetItem ) );
         else
            hb_itemCopy( pItem, pGetItem );

         bSuccess = SUCCESS;
      }
   }
   else
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );

   return bSuccess;
}

void hb_memvarGetValue( PHB_ITEM pItem, PHB_SYMB pMemvarSymb )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarGetValue(%p, %p)", pItem, pMemvarSymb ) );

   if( hb_memvarGet( pItem, pMemvarSymb ) == FAILURE )
   {
      /* Generate an error with retry possibility
       * (user created error handler can create this variable)
       */
      USHORT      uiAction = E_RETRY;
      PHB_ITEM pError;

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003, NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );

         if( uiAction == E_RETRY )
         {
            if( hb_memvarGet( pItem, pMemvarSymb ) == SUCCESS )
               uiAction = E_DEFAULT;
         }
      }

      hb_itemRelease( pError );
   }
}

void hb_memvarGetRefer( PHB_ITEM pItem, PHB_SYMB pMemvarSymb )
{
   HB_THREAD_STUB
   PHB_DYNS pDyn;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarGetRefer(%p, %p)", pItem, pMemvarSymb ) );
#ifdef HB_THREAD_SUPPORT
   /* we must find the thread specific name */
   pDyn  = s_memvarThGetName( pMemvarSymb->szName, &HB_VM_STACK );
#else
   pDyn  = ( PHB_DYNS ) pMemvarSymb->pDynSym;
#endif

   if( pDyn )
   {
      HB_TRACE( HB_TR_INFO, ( "Memvar item (%i)(%s) referenced", pDyn->hMemvar, pMemvarSymb->szName ) );

      if( pDyn->hMemvar )
      {
         PHB_ITEM pReference;

         pReference = s_globalTable[ pDyn->hMemvar ].pVarItem;

         if( HB_IS_BYREF( pReference ) )
         {
            hb_itemCopy( pItem, pReference );
            return;
         }

#ifdef HB_UNSHARE_REFERENCES
         hb_itemUnShare( pReference );
#endif
         /* TraceLog( NULL, "Ref to %s (%i) type: %i counter: %i\n", pMemvarSymb->szName, pDyn->hMemvar, pReference->type, s_globalTable[ pDyn->hMemvar ].counter );
          */

         hb_itemClear( pItem );

         /* value is already created */
         pItem->type                      = HB_IT_BYREF | HB_IT_MEMVAR;
         pItem->item.asMemvar.offset      = 0;
         pItem->item.asMemvar.value       = pDyn->hMemvar;
         pItem->item.asMemvar.itemsbase   = &s_globalTable;

         s_globalTable[ pDyn->hMemvar ].counter++;
      }
      else
      {
         /* Generate an error with retry possibility
          * (user created error handler can make this variable accessible)
          */
         USHORT      uiAction = E_RETRY;
         PHB_ITEM pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003, NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );

            if( uiAction == E_RETRY )
            {
               if( pDyn->hMemvar )
               {
                  hb_itemClear( pItem );

                  /* value is already created */
                  pItem->type                      = HB_IT_BYREF | HB_IT_MEMVAR;
                  pItem->item.asMemvar.offset      = 0;
                  pItem->item.asMemvar.value       = pDyn->hMemvar;

                  pItem->item.asMemvar.itemsbase   = &s_globalTable;
                  ++s_globalTable[ pDyn->hMemvar ].counter;

                  uiAction                         = E_DEFAULT;
               }
            }
         }

         hb_itemRelease( pError );
      }
   }
   else
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
}

void hb_memvarNewParameter( PHB_SYMB pSymbol, PHB_ITEM pValue )
{
   HB_THREAD_STUB

   PHB_DYNS pDyn;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarNewParameter(%p, %p)", pSymbol, pValue ) );
#ifdef HB_THREAD_SUPPORT
   /* we must find the thread specific name */
   pDyn  = s_memvarThGetName( pSymbol->szName, &HB_VM_STACK );
#else
   pDyn  = ( PHB_DYNS ) pSymbol->pDynSym;
#endif

   hb_memvarCreateFromDynSymbol( pDyn, HB_MV_PRIVATE, pValue );
}

char * hb_memvarGetStrValuePtr( char * szVarName, HB_SIZE * pulLen )
{
   HB_THREAD_STUB

   HB_ITEM_NEW( itName );
   PHB_DYNS pDynVar;
   char *      szValue = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarGetStrValuePtr(%s, %p)", szVarName, pulLen ) );

   hb_itemPutCLStatic( &itName, szVarName, *pulLen );

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
         PHB_ITEM pItem = s_globalTable[ pDynVar->hMemvar ].pVarItem;

         if( HB_IS_BYREF( pItem ) )
            pItem = hb_itemUnRef( pItem );   /* it is a PARAMETER variable */

         if( HB_IS_STRING( pItem ) )
         {
            szValue  = pItem->item.asString.value;
            *pulLen  = pItem->item.asString.length;
         }
      }
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarCreateFromItem(%p, %d, %p)", pMemvar, bScope, pValue ) );

   /* find dynamic symbol or creeate one */
   if( HB_IS_SYMBOL( pMemvar ) )
   {
#ifdef HB_THREAD_SUPPORT
      pDynVar  = s_memvarThGetName( pMemvar->item.asSymbol.value->szName, &HB_VM_STACK );
#else
      pDynVar  = hb_dynsymGet( pMemvar->item.asSymbol.value->szName );
#endif
   }
   else if( HB_IS_STRING( pMemvar ) )
   {
#ifdef HB_THREAD_SUPPORT
      pDynVar  = s_memvarThGetName( pMemvar->item.asString.value, &HB_VM_STACK );
#else
      pDynVar  = hb_dynsymGet( pMemvar->item.asString.value );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 3008, NULL, "&", 2, hb_paramError( 1 ), hb_paramError( 2 ) );

   if( pDynVar )
      hb_memvarCreateFromDynSymbol( pDynVar, bScope, pValue );
}

static void hb_memvarCreateFromDynSymbol( PHB_DYNS pDynVar, BYTE bScope, PHB_ITEM pValue )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarCreateFromDynSymbol(%p, %d, %p)", pDynVar, bScope, pValue ) );

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
            s_globalTable[ pDynVar->hMemvar ].pVarItem->type = HB_IT_LOGICAL;

            /* NOTE: PUBLIC variables named CLIPPER and HARBOUR are initialized */
            /*       to .T., this is normal Clipper behaviour. [vszakats] */

            if( strcmp( pDynVar->pSymbol->szName, "HARBOUR" ) == 0 || strcmp( pDynVar->pSymbol->szName, "CLIPPER" ) == 0 )
               s_globalTable[ pDynVar->hMemvar ].pVarItem->item.asLogical.value = TRUE;
            else
               s_globalTable[ pDynVar->hMemvar ].pVarItem->item.asLogical.value = FALSE;
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

      pDynVar->hMemvar                                = hb_memvarValueNew( pValue, TRUE );
      s_globalTable[ pDynVar->hMemvar ].hPrevMemvar   = hCurrentValue;
      /* Add this variable to the PRIVATE variables stack
       */
      hb_memvarAddPrivate( pDynVar );
   }
}

/* This function releases all memory occupied by a memvar variable
 * It also restores the value that was hidden if there is another
 * PRIVATE variable with the same name.
 */
static void hb_memvarRelease( PHB_ITEM pMemvar )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarRelease(%p)", pMemvar ) );

   if( HB_IS_STRING( pMemvar ) )
   {
      HB_SIZE ulBase = s_privateStackCnt;

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
            if( hb_symcmp( pDynVar->pSymbol->szName, pMemvar->item.asString.value ) == 0 )
            {
               PHB_ITEM pRef;

               /* Wrong to reset - private may be used again in this procedure and counter maybecome negative!
                  s_globalTable[ pDynVar->hMemvar ].counter = 0;
                */

               pRef = s_globalTable[ pDynVar->hMemvar ].pVarItem;

               if( HB_IS_COMPLEX( pRef ) )
                  hb_itemClear( pRef );
               else
                  pRef->type = HB_IT_NIL;

               return;
            }
         }
      }

      /* No match found for PRIVATEs - try PUBLICs. */
      hb_memvarReleasePublic( pMemvar );
   }
   else
      hb_errRT_BASE( EG_ARG, 3008, NULL, "RELEASE", 1, hb_paramError( 1 ) );
}

/* This function releases all memory occupied by a memvar variable and
 * assigns NIL value - it releases variables created in current
 * procedure only.
 * The scope of released variables are specified using passed name's mask
 */
static void hb_memvarReleaseWithMask( char * szRegEx, BOOL bInclude )
{
   HB_THREAD_STUB

   HB_SIZE  ulBase = s_privateStackCnt;
   PHB_DYNS pDynVar;

   HB_REGEX RegEx;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarReleaseWithMask(%s, %d)", szRegEx, ( int ) bInclude ) );

   if( ! hb_regexCompile( &RegEx, szRegEx, 0, 0 ) )
   {
      hb_errInternal( 9100, "Invalid mask passed as MEMVAR filter '%s'\n", szRegEx, NULL );
   }

   while( ulBase > hb_stackBaseItem()->item.asSymbol.pCargo->privatesbase )
   {
      --ulBase;
      pDynVar = s_privateStack[ ulBase ];

      /* reset current value to NIL - the overriden variables will be
       * visible after exit from current procedure
       */
      if( pDynVar->hMemvar )
      {
         PHB_ITEM pRef     = s_globalTable[ pDynVar->hMemvar ].pVarItem;
         BOOL     bMatch   = hb_regexMatch( &RegEx, pDynVar->pSymbol->szName, TRUE );

         if( bInclude ? bMatch : ! bMatch )
         {
            /* Wrong to reset - private may be used again in this procedure and counter maybecome negative!
               s_globalTable[ pDynVar->hMemvar ].counter = 0;
             */

            if( HB_IS_COMPLEX( pRef ) )
               hb_itemClear( pRef );
            else
               pRef->type = HB_IT_NIL;
         }
      }
   }

   hb_regexFree( &RegEx );
}

/* Checks if passed dynamic symbol is a variable and returns its scope
 */
static int hb_memvarScopeGet( PHB_DYNS pDynVar )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarScopeGet(%p)", pDynVar ) );

   if( pDynVar->hMemvar == 0 )
      return HB_MV_UNKNOWN;
   else
   {
      HB_SIZE  ulBase   = s_privateStackCnt; /* start from the top of the stack */
      int      iMemvar  = HB_MV_PUBLIC;

      while( ulBase )
      {
         --ulBase;

         if( pDynVar == s_privateStack[ ulBase ] )
         {
            iMemvar = ( ulBase >= s_privateStackBase ) ? HB_MV_PRIVATE_LOCAL: HB_MV_PRIVATE_GLOBAL;
            ulBase  = 0;
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
   PHB_DYNS pDynVar;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarScope(%s)", szVarName ) );

#ifdef HB_THREAD_SUPPORT
   pDynVar  = s_memvarThFindName( szVarName, &HB_VM_STACK );
#else
   pDynVar  = hb_dynsymFindName( szVarName );
#endif

   return pDynVar ? hb_memvarScopeGet( pDynVar ): HB_MV_NOT_FOUND;
}

/* Releases memory occupied by a variable
 */
static HB_DYNS_FUNC( hb_memvarClear )
{
   HB_THREAD_STUB

   if( pDynSymbol->hMemvar )
   {
#if defined( HB_OS_WIN_64 )
      if( pDynSymbol->hMemvar != ( HB_HANDLE ) ( HB_ULONG ) Cargo )
#else
      if( pDynSymbol->hMemvar != ( HB_HANDLE ) Cargo )
#endif
      {
         s_globalTable[ pDynSymbol->hMemvar ].counter = 1;
         hb_memvarValueDecRef( pDynSymbol->hMemvar );
         pDynSymbol->hMemvar                          = 0;
      }
   }

   return TRUE;
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and
 * increments the counter eventually
 */
static HB_DYNS_FUNC( hb_memvarCountPublics )
{
   if( hb_memvarScopeGet( pDynSymbol ) == HB_MV_PUBLIC )
      ( *( ( int * ) Cargo ) )++;

   return TRUE;
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and
 * released it eventually stoping the searching
 */
static HB_DYNS_FUNC( hb_memvarReleasePublicWorker )
{
   HB_THREAD_STUB

   if( hb_memvarScopeGet( pDynSymbol ) == HB_MV_PUBLIC )
   {
      if( hb_symcmp( pDynSymbol->pSymbol->szName, ( char * ) Cargo ) == 0 )
      {
         s_globalTable[ pDynSymbol->hMemvar ].counter = 1;
         hb_memvarValueDecRef( pDynSymbol->hMemvar );
         pDynSymbol->hMemvar                          = 0;
         return FALSE;
      }
   }

   return TRUE;
}

/* find and release if exists given public variable
 */
void hb_memvarReleasePublic( PHB_ITEM pMemVar )
{
   char * sPublic = pMemVar->item.asString.value;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarReleasePublic(%p)", pMemVar ) );

   hb_dynsymEval( hb_memvarReleasePublicWorker, ( void * ) sPublic );
}

/* Count the number of variables with given scope
 */
static HB_ISIZ hb_memvarCount( int iScope )
{
   HB_THREAD_STUB

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarCount(%d)", iScope ) );

   if( iScope == HB_MV_PUBLIC )
   {
      int iPublicCnt = 0;

      hb_dynsymEval( hb_memvarCountPublics, ( void * ) &iPublicCnt );

      return iPublicCnt;
   }

   return s_privateStackCnt;  /* number of PRIVATE variables */
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and returns
 * a pointer to its dynamic symbol
 */
static HB_DYNS_FUNC( hb_memvarFindPublicByPos )
{
   BOOL bCont = TRUE;

   if( hb_memvarScopeGet( pDynSymbol ) == HB_MV_PUBLIC )
   {
      struct mv_PUBLIC_var_info * pStruPub = ( struct mv_PUBLIC_var_info * ) Cargo;

      if( pStruPub->iPos-- == 0 )
      {
         pStruPub->bFound  = TRUE;
         pStruPub->pDynSym = pDynSymbol;
         bCont             = FALSE;
      }
   }

   return bCont;
}

/* Returns the pointer to item that holds a value of variable (or NULL if
 * not found). It fills also the pointer to the variable name
 * Both pointers points to existing and used data - they shouldn't be
 * deallocated.
 */
static PHB_ITEM hb_memvarDebugVariable( int iScope, int iPos, const char ** pszName )
{
   HB_THREAD_STUB

   PHB_ITEM pValue = NULL;

   *pszName = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarDebugVariable(%d, %d, %p)", iScope, iPos, pszName ) );

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
            pValue   = s_globalTable[ struPub.pDynSym->hMemvar ].pVarItem;
            *pszName = struPub.pDynSym->pSymbol->szName;
         }
      }
      else
      {
         if( ( HB_SIZE ) iPos < s_privateStackCnt )
         {
            PHB_DYNS pDynSym = s_privateStack[ iPos ];
            pValue   = s_globalTable[ pDynSym->hMemvar ].pVarItem;
            *pszName = pDynSym->pSymbol->szName;
         }
      }
   }

   return pValue;
}

/* JC1 NOTE: Not locking it because it is necessary to lock the caller. */
static PHB_DYNS hb_memvarFindSymbol( PHB_ITEM pName )
{
   PHB_DYNS pDynSym = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarFindSymbol(%p)", pName ) );

   if( pName )
   {
      HB_SIZE ulLen = pName->item.asString.length;

      if( ulLen )
      {
#ifdef HB_THREAD_SUPPORT
         HB_THREAD_STUB;
         pDynSym  = s_memvarThFindName( pName->item.asString.value, &HB_VM_STACK );
#else
         pDynSym  = hb_dynsymFindName( pName->item.asString.value );
#endif
      }
   }

   return pDynSym;
}

/* ************************************************************************** */

HB_FUNC( __MVPUBLIC )
{
   HB_THREAD_STUB_API

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
               HB_SIZE j, ulLen = hb_arrayLen( pMemvar );

               for( j = 1; j <= ulLen; j++ )
                  hb_memvarCreateFromItem( hb_arrayGetItemPtr( pMemvar, j ), VS_PUBLIC, NULL );
            }
            else
               hb_memvarCreateFromItem( pMemvar, VS_PUBLIC, NULL );
         }
      }
   }
}

HB_FUNC( __MVPRIVATE )
{
   HB_THREAD_STUB_API

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
               HB_SIZE j, ulLen = hb_arrayLen( pMemvar );

               for( j = 1; j <= ulLen; j++ )
                  hb_memvarCreateFromItem( hb_arrayGetItemPtr( pMemvar, j ), VS_PRIVATE, NULL );
            }
            else
               hb_memvarCreateFromItem( pMemvar, VS_PRIVATE, NULL );
         }
      }

      /* Created vars should be owned by our caller!!! */
      s_privateStackBase = s_privateStackCnt;
   }
}

HB_FUNC( __MVXRELEASE )
{
   HB_THREAD_STUB_API

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
               HB_SIZE j, ulLen = hb_arrayLen( pMemvar );

               for( j = 1; j <= ulLen; j++ )
                  hb_memvarRelease( hb_arrayGetItemPtr( pMemvar, j ) );
            }
            else
               hb_memvarRelease( pMemvar );
         }
      }
   }
}

HB_FUNC( __MVRELEASE )
{
   HB_THREAD_STUB_API

   /* Arbitary value which should be big enough. */
   char  szRegEx[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN ];

   int   iCount = hb_pcount();

   if( iCount )
   {
      PHB_ITEM pMask = hb_param( 1, HB_IT_STRING );

      if( pMask )
      {
         BOOL bIncludeVar= ( iCount > 1 ) ? hb_parl( 2 ) : TRUE;

         if( pMask->item.asString.value[ 0 ] == '*' )
            bIncludeVar = TRUE;   /* delete all memvar variables */

         Mask2RegEx( ( char * ) ( pMask->item.asString.value ), ( char * ) szRegEx, FALSE );
         hb_memvarReleaseWithMask( szRegEx, bIncludeVar );
      }
   }
}

HB_FUNC( __MVSCOPE )
{
   HB_THREAD_STUB_API

   int iMemvar = HB_MV_ERROR;

   if( hb_pcount() )
   {
      PHB_ITEM pVarName = hb_param( 1, HB_IT_STRING );

      if( pVarName )
         iMemvar = hb_memvarScope( pVarName->item.asString.value );
   }

   hb_retni( iMemvar );
}

static void hb_memvarClearAll( void )
{
   HB_THREAD_STUB
   PHB_DYNS pGetList = hb_dynsymFind( "GETLIST" );

   /* Let all existing stack frames know they don't have any privates, any longer!
    */
   hb_stackClearPrivateBases();

   s_privateStackBase = 0;
   hb_memvarSetPrivatesBase( 0 );

   if( pGetList && pGetList->hMemvar )
   {
#if defined( HB_OS_WIN_64 )
      hb_dynsymEval( hb_memvarClear, ( void * ) ( HB_ULONG ) pGetList->hMemvar );
#else
      hb_dynsymEval( hb_memvarClear, ( void * ) pGetList->hMemvar );
#endif
   }
   else
      hb_dynsymEval( hb_memvarClear, NULL );
}

HB_FUNC( __MVCLEAR )
{
   hb_memvarClearAll();
}

HB_FUNC( __MVDBGINFO )
{
   HB_THREAD_STUB_ANY

   int iCount = hb_pcount();

   if( iCount == 1 )          /* request for a number of variables */
      hb_retns( hb_memvarCount( hb_parni( 1 ) ) );
   else if( iCount >= 2 )     /* request for a value of variable */
   {
      PHB_ITEM    pValue;
      const char *   szName;

      pValue = hb_memvarDebugVariable( hb_parni( 1 ), hb_parni( 2 ), &szName );

      if( pValue )
      {
         /*the requested variable was found
          */
         if( iCount >= 3 && ISBYREF( 3 ) )
         {
            /* we have to use this variable regardless of its current value
             */
            PHB_ITEM pName = hb_param( 3, HB_IT_ANY );

            hb_itemPutC( pName, szName ); /* clear an old value and copy a new one */
            /* szName points directly to a symbol name - it cannot be released
             */
         }
         hb_itemReturn( pValue );
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
            PHB_ITEM pName = hb_param( 3, HB_IT_ANY );

            hb_itemPutC( pName, "?" ); /* clear an old value and copy a new one */
         }
      }
   }
}

HB_FUNC( __MVEXIST )
{
   HB_THREAD_STUB_API

   PHB_ITEM pName = hb_param( 1, HB_IT_STRING );
   PHB_DYNS    pDyn  = NULL;

   hb_retl( pName && ( ( pDyn = hb_memvarFindSymbol( pName ) ) != NULL ) && pDyn->hMemvar );
}

HB_FUNC( __MVGET )
{
   HB_THREAD_STUB_ANY

   PHB_ITEM pName = hb_param( 1, HB_IT_STRING );

   if( pName )
   {
      PHB_DYNS pDynVar;

      pDynVar = hb_memvarFindSymbol( pName );

      if( pDynVar )
      {
         HB_ITEM  retValue;
         PHB_SYMB pSymbol;

         pSymbol              = pDynVar->pSymbol;

         ( &retValue )->type  = HB_IT_NIL;
         hb_memvarGetValue( &retValue, pSymbol );

         hb_itemReturnForward( &retValue );
      }
      else
      {
         USHORT      uiAction = E_RETRY;
         PHB_ITEM pError;

         /* Generate an error with retry possibility
          * (user created error handler can create this variable)
          */

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                NULL, pName->item.asString.value, 0, EF_CANRETRY );

         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );

            if( uiAction == E_RETRY )
            {
               pDynVar = hb_memvarFindSymbol( pName );

               if( pDynVar )
               {
                  PHB_SYMB pSymbol = pDynVar->pSymbol;

                  hb_memvarGetValue( hb_stackReturnItem(), pSymbol );

                  uiAction = E_DEFAULT;
               }
            }
         }

         hb_itemRelease( pError );
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

   PHB_ITEM pName    = hb_param( 1, HB_IT_STRING );
   HB_ITEM     nil;
   PHB_ITEM pValue   = &nil;

   nil.type = HB_IT_NIL;

   if( hb_pcount() >= 2 )
      pValue = hb_param( 2, HB_IT_ANY );

   if( pName )
   {
      /* the first parameter is a string with not empty variable name
       */
      PHB_DYNS pDynVar;

      pDynVar = hb_memvarFindSymbol( pName );

      if( pDynVar )
      {
         /* variable was declared somwhere - assign a new value
          */
         PHB_SYMB pSymbol = pDynVar->pSymbol;
         hb_memvarSetValue( pSymbol, pValue );
      }
      else
      {
         PHB_DYNS pDyn;

         /* attempt to assign a value to undeclared variable
          * create the PRIVATE one
          */
#ifdef HB_THREAD_SUPPORT
         pDyn  = s_memvarThGetName( pName->item.asString.value, &HB_VM_STACK );
#else
         pDyn  = hb_dynsymGet( pName->item.asString.value );
#endif

         hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pValue );

         /* Created var should be owned by our caller!!! */
         s_privateStackBase = s_privateStackCnt;
      }

      hb_itemForwardValue( hb_stackReturnItem(), pValue );
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
      PHB_ITEM pRetValue = hb_errRT_BASE_Subst( EG_ARG, 3010, NULL, NULL, 2, hb_paramError( 1 ), hb_paramError( 2 ) );

      if( pRetValue )
         hb_itemRelease( pRetValue );

      hb_itemForwardValue( hb_stackReturnItem(), pValue );
   }
}

#define HB_MEM_REC_LEN  32
#define HB_MEM_NUM_LEN  8

typedef struct
{
   const char * pszMask;
   BOOL bIncludeMask;
   BOOL bLongName;
   BYTE * buffer;
   HB_FHANDLE fhnd;
   HB_REGEX regEx;
} MEMVARSAVE_CARGO;

/* saves a variable to a mem file already open */

static HB_DYNS_FUNC( hb_memvarSave )
{
   HB_THREAD_STUB
   const char *   pszMask        = ( ( MEMVARSAVE_CARGO * ) Cargo )->pszMask;
   BOOL           bIncludeMask   = ( ( MEMVARSAVE_CARGO * ) Cargo )->bIncludeMask;
   BYTE *         buffer         = ( ( MEMVARSAVE_CARGO * ) Cargo )->buffer;
   HB_FHANDLE     fhnd           = ( ( MEMVARSAVE_CARGO * ) Cargo )->fhnd;
   BOOL           bLongName      = ( ( MEMVARSAVE_CARGO * ) Cargo )->bLongName;
   UINT           uMemLen        = 10;
   UINT           uMLen          = HB_MEM_REC_LEN;

   /* xHarbour extended feature, restore variables with 64 chars long */
   if( bLongName )
   {
      uMemLen  = HB_SYMBOL_NAME_LEN;
      uMLen    = HB_SYMBOL_NAME_LEN + 22;
      /*
       * Why 22 ? I don't know :-( It just came from simple math ie.
       * For 10-char-length, HB_MEM_REC_LEN is 32. So, 22 bytes are considered
       * spared/reserved. Current implementation only utilizes 3 bytes.
       */
   }

   /* NOTE: Harbour name lengths are not limited, but the .mem file
            structure is not flexible enough to allow for it.
            [vszakats] */

   if( pDynSymbol->hMemvar )
   {
      BOOL     bMatch = ( pszMask[ 0 ] == '*' || hb_regexMatch( &( ( ( MEMVARSAVE_CARGO * ) Cargo )->regEx ), pDynSymbol->pSymbol->szName, TRUE ) );
      PHB_ITEM pItem  = s_globalTable[ pDynSymbol->hMemvar ].pVarItem;

      /* Process it if it matches the passed mask */
      if( bIncludeMask ? bMatch : ! bMatch )
      {
         /* NOTE: Clipper will not initialize the record buffer with
                  zeros, so they will look trashed. [vszakats] */

         memset( buffer, 0, uMLen );
         hb_strncpy( ( char * ) buffer, pDynSymbol->pSymbol->szName, uMemLen );
         buffer[ uMemLen ] = '\0';

         if( HB_IS_STRING( pItem ) && ( pItem->item.asString.length + 1 ) <= SHRT_MAX )
         {
            /* Store the closing zero byte, too */
            USHORT uiLength = ( USHORT ) ( pItem->item.asString.length + 1 );

            buffer[ uMemLen + 1 ]   = 'C' + 128;
            buffer[ uMemLen + 6 ]   = HB_LOBYTE( uiLength );
            buffer[ uMemLen + 7 ]   = HB_HIBYTE( uiLength );

            hb_fsWrite( fhnd, buffer, ( USHORT ) uMLen );
            hb_fsWrite( fhnd, ( BYTE * ) pItem->item.asString.value, uiLength );
         }
         else if( HB_IS_TIMEFLAG( pItem ) )
         {
            BYTE byNum[ sizeof( double ) ];

            buffer[ uMemLen + 1 ]   = 'T' + 128;
            buffer[ uMemLen + 6 ]   = 1;
            buffer[ uMemLen + 7 ]   = 0;

            HB_PUT_LE_DOUBLE( byNum, ( double ) hb_datetimePack( pItem->item.asDate.value, pItem->item.asDate.time ) );

            hb_fsWrite( fhnd, buffer, ( USHORT ) uMLen );
            hb_fsWrite( fhnd, byNum, sizeof( byNum ) );
         }
         else if( HB_IS_DATE( pItem ) )
         {
            BYTE byNum[ sizeof( double ) ];

            buffer[ uMemLen + 1 ]   = 'D' + 128;
            buffer[ uMemLen + 6 ]   = 1;
            buffer[ uMemLen + 7 ]   = 0;

            HB_PUT_LE_DOUBLE( byNum, ( double ) pItem->item.asDate.value );

            hb_fsWrite( fhnd, buffer, ( USHORT ) uMLen );
            hb_fsWrite( fhnd, byNum, sizeof( byNum ) );
         }
         else if( HB_IS_NUMERIC( pItem ) )
         {
            BYTE  byNum[ sizeof( double ) ];
            int   iWidth;
            int   iDec;

            hb_itemGetNLen( pItem, &iWidth, &iDec );

            buffer[ uMemLen + 1 ]   = 'N' + 128;
#ifdef HB_C52_STRICT
/* NOTE: This is the buggy, but fully CA-Cl*pper compatible method. [vszakats] */
            buffer[ uMemLen + 6 ]   = ( BYTE ) iWidth + ( HB_IS_DOUBLE( pItem ) ? iDec + 1 : 0 );
#else
/* NOTE: This would be the correct method, but Clipper is buggy here. [vszakats] */
            buffer[ uMemLen + 6 ]   = ( BYTE ) ( iWidth + ( iDec == 0 ? 0 : iDec + 1 ) );
#endif
            buffer[ uMemLen + 7 ]   = ( BYTE ) iDec;

            HB_PUT_LE_DOUBLE( byNum, hb_itemGetND( pItem ) );

            hb_fsWrite( fhnd, buffer, ( USHORT ) uMLen );
            hb_fsWrite( fhnd, byNum, sizeof( byNum ) );
         }
         else if( HB_IS_LOGICAL( pItem ) )
         {
            BYTE byLogical[ 1 ];

            buffer[ uMemLen + 1 ]   = 'L' + 128;
            buffer[ uMemLen + 6 ]   = sizeof( BYTE );
            buffer[ uMemLen + 7 ]   = 0;

            byLogical[ 0 ]          = hb_itemGetL( pItem ) ? 1 : 0;

            hb_fsWrite( fhnd, buffer, ( USHORT ) uMLen );
            hb_fsWrite( fhnd, byLogical, sizeof( BYTE ) );
         }
      }
   }

   return TRUE;
}

HB_FUNC( __MVSAVE )
{
   HB_THREAD_STUB_API

   /* Clipper also checks for the number of arguments here */
   if( hb_pcount() >= 3 && ISCHAR( 1 ) && ISCHAR( 2 ) && ISLOG( 3 ) )
   {
      PHB_FNAME   pFileName;
      char        szFileName[ HB_PATH_MAX ];
      HB_FHANDLE  fhnd;
      UINT        uLen = HB_MEM_REC_LEN;

      if( ISLOG( 4 ) && hb_parl( 4 ) )
         uLen = HB_SYMBOL_NAME_LEN + 22;

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( pFileName->szExtension == NULL && hb_stackSetStruct()->HB_SET_DEFEXTENSIONS )
         pFileName->szExtension = ".mem";

      if( ! pFileName->szPath )
         pFileName->szPath = hb_stackSetStruct()->HB_SET_DEFAULT;

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Create .mem file */

      while( ( fhnd = hb_fsCreate( szFileName, FC_NORMAL ) ) == FS_ERROR )
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_CREATE, 2006, NULL, szFileName, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }

      if( fhnd != FS_ERROR )
      {
         MEMVARSAVE_CARGO  msc;
         /* Arbitary value, *SHOULD* be long enough. */
         char              szRegEx[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN ];

         msc.pszMask       = hb_parcx( 2 );
         msc.bIncludeMask  = hb_parl( 3 );
         msc.bLongName     = hb_parl( 4 );
         msc.buffer        = ( BYTE * ) hb_xgrab( uLen );
         msc.fhnd          = fhnd;

         Mask2RegEx( ( char * ) ( msc.pszMask ), szRegEx, FALSE );

         if( ! hb_regexCompile( &msc.regEx, szRegEx, 0, 0 ) )
            hb_errInternal( 9100, "Invalid mask passed as MEMVAR filter '%s' -> '%s'\n", ( char * ) ( msc.pszMask ), szRegEx );

         /* Walk through all visible memory variables and save each one */
         hb_dynsymEval( hb_memvarSave, ( void * ) &msc );

         hb_regexFree( &msc.regEx );

         msc.buffer[ 0 ] = '\x1A';
         hb_fsWrite( fhnd, msc.buffer, 1 );
         hb_fsCommit( fhnd );
         hb_fsClose( fhnd );

         hb_xfree( msc.buffer );
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
      in Harbour since we have two optional parameters as an extension.
      We make it three for restoring long-name variables (AJ:2005/04/25)
    */
   if( ISCHAR( 1 ) && ISLOG( 2 ) )
   {
      PHB_FNAME   pFileName;
      char        szFileName[ HB_PATH_MAX ];
      HB_FHANDLE  fhnd;

      BOOL        bAdditive   = hb_parl( 2 );
      BOOL        bLongName   = ISLOG( 3 ) ? hb_parl( 3 ) : FALSE;
      UINT        uLen        = HB_MEM_REC_LEN;
      UINT        uMemLen     = 10;

      /* Clear all memory variables if not ADDITIVE */

      if( ! bAdditive )
         hb_memvarClearAll();

      /* xHarbour extended feature, save variables with 64 chars long */
      if( bLongName )
      {
         uLen     = HB_SYMBOL_NAME_LEN + 22;
         uMemLen  = HB_SYMBOL_NAME_LEN;
         /*
          * Why 22 ? I don't know :-( It just came from simple math ie.
          * For 10-char-length, HB_MEM_REC_LEN is 32. So, 22 bytes are considered
          * spared/reserved. Current implementation only utilizes 3 bytes.
          */
      }

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( pFileName->szExtension == NULL && hb_stackSetStruct()->HB_SET_DEFEXTENSIONS )
         pFileName->szExtension = ".mem";

      if( ! pFileName->szPath )
         pFileName->szPath = hb_stackSetStruct()->HB_SET_DEFAULT;

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Open .mem file */

      while( ( fhnd = hb_fsExtOpen( szFileName, NULL, FO_READ | FO_DENYWRITE | FO_PRIVATE | FXO_DEFAULTS, NULL, NULL ) ) == FS_ERROR )
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2005, NULL, szFileName, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 2, hb_paramError( 1 ), hb_paramError( 2 ) );

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }

      if( fhnd != FS_ERROR )
      {
         const char *   pszMask        = ISCHAR( 4 ) ? hb_parc( 4 ) : "*";
         BOOL           bIncludeMask   = ISLOG( 5 ) ? hb_parl( 5 ) : TRUE;
         BYTE *         buffer         = ( BYTE * ) hb_xgrab( uLen );

         /* Arbitary value which should be big enough. */
         char           szRegEx[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN ];

         HB_ITEM        Name, Item;

         HB_REGEX       RegEx;

         Item.type   = HB_IT_NIL;
         Name.type   = HB_IT_NIL;

         Mask2RegEx( ( char * ) pszMask, ( char * ) szRegEx, FALSE );

         if( ! hb_regexCompile( &RegEx, szRegEx, 0, 0 ) )
            hb_errInternal( 9100, "Invalid mask passed as MEMVAR filter '%s' -> '%s'\n", ( char * ) pszMask, szRegEx );

         while( hb_fsRead( fhnd, buffer, ( USHORT ) uLen ) == ( USHORT ) uLen )
         {
            USHORT   uiType   = ( USHORT ) ( buffer[ 1 + uMemLen ] - 128 );
            USHORT   uiWidth  = ( USHORT ) buffer[ 6 + uMemLen ];
            USHORT   uiDec    = ( USHORT ) buffer[ 7 + uMemLen ];
            BOOL     bMatch;

            hb_itemPutC( &Name, ( char * ) buffer );

            switch( uiType )
            {
               case 'C':
               {
                  BYTE * pbyString;

                  uiWidth     += uiDec * 256;
                  pbyString   = ( BYTE * ) hb_xgrab( uiWidth );

                  if( hb_fsRead( fhnd, pbyString, uiWidth ) == uiWidth )
                     hb_itemPutCPtr( &Item, ( char * ) pbyString, uiWidth - 1 );
                  else
                     hb_errInternal( 9100, "Restore failed for: '%s'\n", hb_itemGetCPtr( &Name ), NULL );

                  break;
               }

               case 'N':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     hb_itemPutNLen( &Item, HB_GET_LE_DOUBLE( pbyNumber ), uiWidth - ( uiDec ? ( uiDec + 1 ) : 0 ), uiDec );
                  else
                     hb_errInternal( 9100, "Restore failed for: '%s'\n", hb_itemGetCPtr( &Name ), NULL );

                  break;
               }

               case 'D':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     hb_itemPutDL( &Item, ( LONG ) HB_GET_LE_DOUBLE( pbyNumber ) );
                  else
                     hb_errInternal( 9100, "Restore failed for: '%s'\n", hb_itemGetCPtr( &Name ), NULL );

                  break;
               }

               case 'T':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     hb_itemPutDTD( &Item, HB_GET_LE_DOUBLE( pbyNumber ) );
                  else
                     hb_errInternal( 9100, "Restore failed for: '%s'\n", hb_itemGetCPtr( &Name ), NULL );

                  break;
               }

               case 'L':
               {
                  BYTE pbyLogical[ 1 ];

                  if( hb_fsRead( fhnd, pbyLogical, 1 ) == 1 )
                     hb_itemPutL( &Item, pbyLogical[ 0 ] != 0 );
                  else
                     hb_errInternal( 9100, "Restore failed for: '%s'\n", hb_itemGetCPtr( &Name ), NULL );

                  break;
               }

               default:
               {
                  char szType[ 6 ];

                  hb_snprintf( szType, sizeof( szType ), "%i", uiType );
                  hb_errInternal( 9100, "Restore failed, unsupported type: %s for: '%s'\n", szType, hb_itemGetCPtr( &Name ) );
#if 0
                  hb_itemClear( &Item );
#endif
               }
            }

            bMatch = ( pszMask[ 0 ] == '*' || hb_regexMatch( &RegEx, ( &Name )->item.asString.value, TRUE ) );

            /* Process it if it matches the passed mask */
            if( bIncludeMask ? bMatch : ! bMatch )
            {
               /* the first parameter is a string with not empty variable name */
               PHB_DYNS pDynVar = hb_memvarFindSymbol( &Name );

               if( pDynVar )
               {
                  /* variable was declared somwhere - assign a new value */
                  PHB_SYMB pSymbol = pDynVar->pSymbol;

                  hb_memvarSetValue( pSymbol, &Item );
               }
               else
               {
                  PHB_DYNS pDyn;

                  /* attempt to assign a value to undeclared variable create the PRIVATE one */
#ifdef HB_THREAD_SUPPORT
                  pDyn  = s_memvarThGetName( ( &Name )->item.asString.value, &HB_VM_STACK );
#else
                  pDyn  = hb_dynsymGet( ( &Name )->item.asString.value );
#endif

                  hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, &Item );
               }
            }
         }

         hb_fsClose( fhnd );

         /* Created vars should be owned by our caller!!! */
         s_privateStackBase = s_privateStackCnt;

         hb_itemClear( &Name );

         hb_regexFree( &RegEx );

         if( buffer )
            hb_xfree( buffer );

         hb_itemReturnForward( &Item );
      }
      else
         hb_retl( FALSE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2007, NULL, "__MRESTORE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
}

/* ----------------------------------------------------------------------- */
/* The garbage collector interface */
/* ----------------------------------------------------------------------- */

/* Mark all memvars as used so they will not be released by the
 * garbage collector
 */
#ifdef HB_THREAD_SUPPORT
void hb_memvarsIsMemvarRef( void * pData )
{
   HB_STACK * pStack = ( HB_STACK * ) pData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarsIsMemvarRef()" ) );

   if( pStack->globalTable )
   {
      HB_SIZE ulCnt = pStack->globalLastFree;

      while( --ulCnt )
      {
         /* do not check detached variables - for these variables only
          * references from the eval stack are meaningfull for the GC
          */
         if( pStack->globalTable[ ulCnt ].counter && pStack->globalTable[ ulCnt ].hPrevMemvar != ( HB_HANDLE ) -1 )
            hb_gcItemRef( pStack->globalTable[ ulCnt ].pVarItem );
      }
   }
}

#else
void hb_memvarsIsMemvarRef( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_memvarsIsMemvarRef()" ) );

   if( s_globalTable )
   {
      HB_SIZE ulCnt = s_globalLastFree;

      while( --ulCnt )
      {
         /* do not check detached variables - for these variables only
          * references from the eval stack are meaningfull for the GC
          */
         if( s_globalTable[ ulCnt ].counter && s_globalTable[ ulCnt ].hPrevMemvar != ( HB_HANDLE ) -1 )
            hb_gcItemRef( s_globalTable[ ulCnt ].pVarItem );
      }
   }
}
#endif

HB_HANDLE hb_memvarGetVarHandle( char * szName )
{
   PHB_DYNS pDyn;

#ifdef HB_THREAD_SUPPORT
   HB_THREAD_STUB

   if( &( HB_VM_STACK ) == &hb_stackMT || strncmp( szName, ":TH:", 4 ) == 0 )
      pDyn = hb_dynsymFindName( szName );
   else
   {
      char szNewName[ 270 ];

      hb_snprintf( szNewName, sizeof( szNewName ), ":TH:%d:%s", HB_VM_STACK.th_vm_id, szName );

      pDyn = hb_dynsymFindName( szNewName );
   }
#else
   pDyn = hb_dynsymFindName( szName );
#endif

   if( pDyn != NULL )
   {
      HB_HANDLE hHand = pDyn->hMemvar;
      return hHand;
   }

   return 0; /* invalid handle */
}

PHB_ITEM hb_memvarGetValueByHandle( HB_HANDLE hMemvar )
{
   HB_THREAD_STUB

   if( hMemvar && hMemvar < s_globalTableSize )
      return s_globalTable[ hMemvar ].pVarItem;

   return NULL;
}

static HB_DYNS_FUNC( hb_GetSymbolInfo )
{
   HB_THREAD_STUB

   if( pDynSymbol->hMemvar )
   {
      PHB_ITEM pArray   = ( ( PHB_ITEM ) Cargo );
      PHB_ITEM pItem    = s_globalTable[ pDynSymbol->hMemvar ].pVarItem;
      HB_ITEM  SubItems;

      SubItems.type = HB_IT_NIL;

      hb_arrayNew( &SubItems, 2 );
      hb_arraySetC( &SubItems, 1, pDynSymbol->pSymbol->szName );
      hb_itemCopy( hb_arrayGetItemPtr( &SubItems, 2 ), pItem );
      hb_arrayAddForward( pArray, &SubItems );
   }
   return TRUE;
}

HB_FUNC( __MVSYMBOLINFO )
{
   HB_ITEM Array;

   Array.type = HB_IT_NIL;
   hb_arrayNew( &Array, 0 );
   hb_dynsymEval( hb_GetSymbolInfo, ( void * ) &Array );
   hb_itemReturnForward( &Array );
}

/* debugger function */
PHB_ITEM hb_memvarGetValueBySym( PHB_DYNS pDynSym )
{
   return pDynSym->hMemvar ? hb_memvarGetValueByHandle( pDynSym->hMemvar ) : NULL;
}
