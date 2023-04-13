/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Dynamic symbol table management
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

/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"

/* JC1: the search of an intem could depend on the current thread stack */
#ifndef HB_THREAD_SUPPORT

static PDYNHB_ITEM   s_pDynItems    = NULL;  /* Pointer to dynamic items */
static UINT          s_uiDynSymbols = 0;     /* Number of symbols present */

/* Closest symbol for match. hb_dynsymFind() will search for the name. */
/* If it cannot find the name, it positions itself to the */
/* closest symbol.  */
static UINT s_uiClosestDynSym = 0;

#else

#define s_uiClosestDynSym HB_VM_STACK.uiClosestDynSym

/* JC1: temporarily turned off, relaying on the old system now
   #define s_pDynItems        HB_VM_STACK.pDynItems
   #define s_uiDynSymbols     HB_VM_STACK.uiDynSymbols
 */

static PDYNHB_ITEM   s_pDynItems    = NULL;  /* Pointer to dynamic items */
static UINT          s_uiDynSymbols = 0;     /* Number of symbols present */

#endif

typedef struct _HB_SYM_HOLDER
{
   HB_SYMB symbol;
   struct _HB_SYM_HOLDER * pNext;
   char szName[ 1 ];
}
HB_SYM_HOLDER, * PHB_SYM_HOLDER;

static PHB_SYM_HOLDER s_pAllocSyms = NULL;

void hb_dynsymLog( void )
{
   /* HB_THREAD_STUB */
   register UINT uiPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymLog()" ) );

   hb_dynsymLock();

   for( uiPos = 0; uiPos < s_uiDynSymbols; uiPos++ )   /* For all dynamic symbols */
      printf( "%i %s\n", uiPos + 1, s_pDynItems[ uiPos ].pDynSym->pSymbol->szName );

   hb_dynsymUnlock();
}

PHB_SYMB hb_symbolNew( const char * szName )      /* Create a new symbol */
{
   PHB_SYM_HOLDER pHolder;
   UINT           iLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_symbolNew(%s)", szName ) );

   iLen                          = ( UINT ) strlen( szName );
   pHolder                       = ( PHB_SYM_HOLDER ) hb_xgrab( sizeof( HB_SYM_HOLDER ) + iLen );
   HB_MEMCPY( pHolder->szName, szName, iLen + 1 );
   pHolder->pNext                = s_pAllocSyms;
   s_pAllocSyms                  = pHolder;

   pHolder->symbol.szName        = pHolder->szName;
   pHolder->symbol.scope.value   = 0;
   pHolder->symbol.value.pFunPtr = NULL;
   pHolder->symbol.pDynSym       = NULL;

   return &pHolder->symbol;
}

PHB_DYNS hb_dynsymNew( PHB_SYMB pSymbol, PSYMBOLS pModuleSymbols )    /* creates a new dynamic symbol */
{
   HB_THREAD_STUB
   PHB_DYNS pDynSym;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymNew(%p, %p)", pSymbol, pModuleSymbols ) );

   hb_dynsymLock();

   pDynSym = hb_dynsymFind( pSymbol->szName );  /* Find position */

   if( pDynSym )                                /* If name exists */
   {
      assert( 0 );

      if( ( pSymbol->scope.value & HB_FS_LOCAL ) == HB_FS_LOCAL )
      {
#if 0
         assert( pModuleSymbols );

         if( pDynSym->pModuleSymbols )
            TraceLog( NULL, "Symbol: '%s' was previously defined at Module: '%s'\n", pSymbol->szName, pDynSym->pModuleSymbols->szModuleName );
#endif

         /* if( pSymbol->value.pFunPtr && pDynSym->pSymbol->value.pFunPtr == NULL )
          */
         {
#if 0
            /* reenabled - it's still wrong, Druzus */
            /* see note below */
            /* register only non static functions */
            if( ( pSymbol->scope.value & ( HB_FS_STATIC | HB_FS_INITEXIT ) ) == HB_FS_STATIC )
            {
               assert( 0 );
               TraceLog( NULL, "Rejecting: %s in %s\n", pSymbol->szName, pModuleSymbols ? pModuleSymbols->szModuleName : "" );
            }
            else
#endif
            {
               /* This is the symbol of the function definition module.
                */
               assert( pSymbol->value.pFunPtr );
               pDynSym->pSymbol = pSymbol;
            }
         }

         pDynSym->pModuleSymbols = pModuleSymbols;
         /* TraceLog( NULL, "Symbol: '%s' DEFINED in Module: '%s'\n", pSymbol->szName, pModuleSymbols ? pModuleSymbols->szModuleName : "" );
          */
      }

      pSymbol->pDynSym = pDynSym;    /* place a pointer to DynSym */

      hb_dynsymUnlock();
      return pDynSym;                /* Return pointer to DynSym */
   }

   if( s_uiDynSymbols == 0 )              /* Do we have any symbols ? */
   {
      pDynSym = s_pDynItems[ 0 ].pDynSym; /* Point to first symbol */
      /* *<1>* Remember we already got this one */
   }
   else                     /* We want more symbols ! */
   {
      s_pDynItems = ( PDYNHB_ITEM ) hb_xrealloc( s_pDynItems, ( s_uiDynSymbols + 1 ) * sizeof( DYNHB_ITEM ) );

      if( s_uiClosestDynSym <= s_uiDynSymbols )   /* Closest < current !! */
      {                                     /* Here it goes :-) */
         register UINT uiPos;

         for( uiPos = 0; uiPos < ( UINT ) ( s_uiDynSymbols - s_uiClosestDynSym ); uiPos++ )
         {
            /* Insert element in array */
            HB_MEMCPY( &s_pDynItems[ s_uiDynSymbols - uiPos ], &s_pDynItems[ s_uiDynSymbols - uiPos - 1 ], sizeof( DYNHB_ITEM ) );
         }
      }

      pDynSym                                   = ( PHB_DYNS ) hb_xgrab( sizeof( HB_DYNS ) );
      s_pDynItems[ s_uiClosestDynSym ].pDynSym  = pDynSym;   /* Enter DynSym */
   }

   s_uiDynSymbols++;             /* Got one more symbol */

   pDynSym->hMemvar     = 0;
   pDynSym->hArea       = 0;
#ifndef HB_NO_PROFILER
   pDynSym->ulCalls     = 0;     /* profiler support */
   pDynSym->ulTime      = 0;     /* profiler support */
   pDynSym->ulRecurse   = 0;
#endif

#if 0
   /* now the compiler creates separate symbols for functions and
    * fields/memvars/aliases so we should not have any problem here
    * and this code is not necessary but I decide to left it here
    * disabled at least for debugging.
    */
   if( pSymbol->value.pFunPtr && ( pSymbol->scope.value & ( HB_FS_STATIC | HB_FS_INITEXIT ) ) == HB_FS_STATIC )
   {
      /*
       * This symbol points to static function - we cannot register
       * such symbols in global dynsyms because we may have more
       * static functions with the same name and non static one
       * registered later - the static function should be directly
       * accessible _ONLY_ from their modules.
       * So we will clone this symbol.
       */
      assert( 0 );
      TraceLog( NULL, "Cloned: %s in %s\n", pSymbol->szName, pModuleSymbols ? pModuleSymbols->szModuleName : "" );

      pSymbol->pDynSym  = pDynSym;                          /* place a pointer to DynSym in original symbol */
      pSymbol           = hb_symbolNew( pSymbol->szName );  /* clone the symbol */
   }
#endif

   /* TraceLog( NULL, "Symbol: '%s' IMPORTED in Module: '%s'\n", pSymbol->szName, pModuleSymbols ? pModuleSymbols->szModuleName : "" );
    */

   if( ( pSymbol->scope.value & HB_FS_LOCAL ) == HB_FS_LOCAL )
   {
      /* This is the true local symbol
       */
      assert( pSymbol->value.pFunPtr );
      assert( pModuleSymbols );
      pDynSym->pModuleSymbols = pModuleSymbols;
      /* TraceLog( NULL, "Symbol: '%s' DEFINED in Module: '%s'\n", pSymbol->szName, pModuleSymbols ? pModuleSymbols->szModuleName : "" );
       */
   }
   else
   {
      pDynSym->pModuleSymbols = NULL;
   }

   pDynSym->pSymbol  = pSymbol;
   pSymbol->pDynSym  = pDynSym;               /* place a pointer to DynSym */

   hb_dynsymUnlock();

   return pDynSym;
}

PHB_DYNS hb_dynsymGet( const char * szName )  /* finds and creates a symbol if not found */
{
   HB_THREAD_STUB_STACK

   /* make a copy as we may get a const string, then turn it to uppercase */
   char     szUprName[ HB_SYMBOL_NAME_LEN + 1 ];
   PHB_DYNS pDynSym;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymGet(%s)", szName ) );

   {
      register int   iLen  = HB_SYMBOL_NAME_LEN;
      char *         pDest = szUprName;

      do
      {
         register char cChar = *szName++;
         if( cChar == 0 || cChar == ' ' || cChar == '\t' )
            break;
         else if( cChar >= 'a' && cChar <= 'z' )
            *pDest++ = cChar - ( 'a' - 'A' );
         else
            *pDest++ = cChar;
      }
      while( --iLen );
      *pDest = '\0';
   }

   /* JC1: Notice, locking this function MAY seem useless but it is not.
      Suppose two threads calling this functon with the same szUprName: both
      of them may find ! pDynSym, and both of them may proceed to hb_dynsymNew().
      Although this operation would suceed, one of the threas would get an
      invalid reference, and we would have a memory leak, as one of the
      two dynsymNew() would be overriden
    */
   hb_dynsymLock();

   pDynSym = hb_dynsymFind( ( char * ) szUprName );

   if( ! pDynSym )       /* Does it exists ? */
   {
      /* TraceLog( NULL, "*** Did NOT find >%s< - CREATED New!\n", szUprName );
       */
      pDynSym                       = hb_dynsymNew( hb_symbolNew( ( char * ) szUprName ), HB_GETMODULESYM() ); /* Make new symbol */
      pDynSym->pSymbol->scope.value = HB_FS_PUBLIC;
   }

   hb_dynsymUnlock();

   return pDynSym;
}

PHB_DYNS hb_dynsymGetCase( const char * szName )  /* finds and creates a symbol if not found CASE SENSITIVE! */
{
   HB_THREAD_STUB_STACK
   PHB_DYNS pDynSym;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymGetCase(%s)", szName ) );

   /* TraceLog( NULL, "Searching: %s\n", szName );
    */

   /* JC1: read the notice for hb_dynsymGet() */
   hb_dynsymLock();

   pDynSym = hb_dynsymFind( szName );

   if( ! pDynSym )       /* Does it exists ? */
   {
      /* TraceLog( NULL, "Creating: %s\n", szName );
       */
      pDynSym                       = hb_dynsymNew( hb_symbolNew( szName ), HB_GETMODULESYM() ); /* Make new symbol */
      pDynSym->pSymbol->scope.value = HB_FS_PUBLIC;
   }

   hb_dynsymUnlock();

   /* TraceLog( NULL, "Returning: %p\n", pDynSym );
    */

   return pDynSym;
}

PHB_DYNS hb_dynsymGetCaseWithNamespaces( const char * szName, const char * pNamespaces )  /* finds and creates a symbol if not found CASE SENSITIVE! */
{
   HB_THREAD_STUB_STACK
   PHB_DYNS pDynSym;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymGetCase(%s)", szName ) );

   /* TraceLog( NULL, "Searching: %s\n", szName );
    */

   /* JC1: read the notice for hb_dynsymGet() */
   hb_dynsymLock();

   pDynSym = hb_dynsymFindWithNamespaces( szName, pNamespaces );

   if( ! pDynSym )       /* Does it exists ? */
   {
      /* TraceLog( NULL, "Creating: %s\n", szName );
       */
      pDynSym                       = hb_dynsymNew( hb_symbolNew( szName ), HB_GETMODULESYM() ); /* Make new symbol */
      pDynSym->pSymbol->scope.value = HB_FS_PUBLIC;
   }

   hb_dynsymUnlock();

   /* TraceLog( NULL, "Returning: %p\n", pDynSym );
    */

   return pDynSym;
}

PHB_DYNS hb_dynsymGetWithNamespaces( const char * szName, const char * pNamespaces )  /* finds and creates a symbol if not found CASE INSENSITIVE! */
{
   HB_THREAD_STUB_STACK
   PHB_DYNS pDynSym;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymGetCase(%s)", szName ) );

   /* TraceLog( NULL, "Searching: %s\n", szName );
    */

   /* JC1: read the notice for hb_dynsymGet() */
   hb_dynsymLock();

   pDynSym = hb_dynsymFindNameWithNamespaces( szName, pNamespaces );

   if( ! pDynSym )       /* Does it exists ? */
   {
      /* TraceLog( NULL, "Creating: %s\n", szName );
       */
      pDynSym                       = hb_dynsymNew( hb_symbolNew( szName ), HB_GETMODULESYM() ); /* Make new symbol */
      pDynSym->pSymbol->scope.value = HB_FS_PUBLIC;
   }

   hb_dynsymUnlock();

   /* TraceLog( NULL, "Returning: %p\n", pDynSym );
    */

   return pDynSym;
}

PHB_DYNS hb_dynsymFindName( const char * szName )  /* finds a symbol */
{
   char szUprName[ HB_SYMBOL_NAME_LEN + 1 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymFindName(%s)", szName ) );

   {
      int      iLen  = HB_SYMBOL_NAME_LEN;
      char *   pDest = szUprName;

      do
      {
         char cChar = *szName++;
         if( cChar == 0 || cChar == ' ' || cChar == '\t' )
            break;
         else if( cChar >= 'a' && cChar <= 'z' )
            *pDest++ = cChar - ( 'a' - 'A' );
         else
            *pDest++ = cChar;
      }
      while( --iLen );
      *pDest = '\0';
   }

   return hb_dynsymFind( ( char * ) szUprName );
}


PHB_DYNS hb_dynsymFind( const char * szName )
{
   HB_THREAD_STUB
   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymFind(%s)", szName ) );

   hb_dynsymLock();

   if( s_pDynItems == NULL )
   {
      s_pDynItems          = ( PDYNHB_ITEM ) hb_xgrab( sizeof( DYNHB_ITEM ) ); /* Grab array */
      s_pDynItems->pDynSym = ( PHB_DYNS ) hb_xgrab( sizeof( HB_DYNS ) );
      /* Always grab a first symbol. Never an empty bucket. *<1>* */
      memset( s_pDynItems->pDynSym, 0, sizeof( HB_DYNS ) );

      hb_dynsymUnlock();
      return NULL;
   }
   else
   {
      /* Classic Tree Insert Sort Mechanism
       *
       * Insert Sort means the new item is entered alphabetically into
       * the array. In this case s_pDynItems !
       *
       * 1) We start in the middle of the array.
       * 2a) If the symbols are equal -> we have found the symbol !!
       *     Champagne ! We're done.
       *  b) If the symbol we are looking for ('ge') is greater than the
       *     middle ('po'), we start looking left.
       *     Only the first part of the array is going to be searched.
       *     Go to (1)
       *  c) If the symbol we are looking for ('ge') is smaller than the
       *     middle ('ko'), we start looking right
       *     Only the last part of the array is going to be searched.
       *     Go to (1)
       */

      UINT  uiFirst  = 0;
      UINT  uiLast   = s_uiDynSymbols;
      UINT  uiMiddle = uiLast / 2;
      int   iCmp;

      s_uiClosestDynSym = uiMiddle;               /* Start in the middle      */
      while( uiFirst < uiLast )
      {
         iCmp = strcmp( s_pDynItems[ uiMiddle ].pDynSym->pSymbol->szName, szName );

         if( iCmp == 0 )
         {
            PHB_DYNS pDynSym = s_pDynItems[ uiMiddle ].pDynSym;

            s_uiClosestDynSym = uiMiddle;
            hb_dynsymUnlock();
            return pDynSym;
         }
         else if( iCmp < 0 )
         {
            uiLast            = uiMiddle;
            s_uiClosestDynSym = uiMiddle;
         }
         else /* if( iCmp > 0 ) */
         {
            uiFirst           = uiMiddle + 1;
            s_uiClosestDynSym = uiFirst;
         }

         uiMiddle = uiFirst + ( ( uiLast - uiFirst ) / 2 );
      }
   }

#ifdef HB_SYMLIMIT_10_WORKAROUND

   /*
    *  (c) 2002, Marcelo Lombardo <lombardo@uol.com.br>
    *  This is an emulation workaround for the symbol table limited to 10 chars,
    *  since the build flag -DHB_SYMBOL_NAME_LEN=10 is not an option anymore.
    */

   if( s_uiClosestDynSym < s_uiDynSymbols )
   {
      USHORT   iLen1 = strlen( szName );
      USHORT   iLen2 = strlen( s_pDynItems[ s_uiClosestDynSym ].pDynSym->pSymbol->szName );
      BOOL     bOk   = 1;
      USHORT   uiPos;

      /*
       *  Let's check the closer symbol found. This code compares each char in the smallest symbol
       *  name to the largest symbol name, if both are larger than 10.
       */

      if( iLen1 >= 10 && iLen2 >= 10 && ( ! ( iLen1 == iLen2 && iLen1 == 10 ) ) )
      {
         if( iLen1 > iLen2 )
         {
            for( uiPos = 0; uiPos < iLen2; uiPos++ )
            {
               if( szName[ uiPos ] != s_pDynItems[ s_uiClosestDynSym ].pDynSym->pSymbol->szName[ uiPos ] )
               {
                  bOk = 0;
                  break;
               }
            }
         }
         else if( iLen2 > iLen1 )
         {
            for( uiPos = 0; uiPos < iLen1; uiPos++ )
            {
               if( szName[ uiPos ] != s_pDynItems[ s_uiClosestDynSym ].pDynSym->pSymbol->szName[ uiPos ] )
               {
                  bOk = 0;
                  break;
               }
            }
         }
         else if( iLen1 == iLen2 )
            bOk = 0;

         if( bOk )
         {
            PHB_DYNS pDynSym = s_pDynItems[ s_uiClosestDynSym ].pDynSym;

            hb_dynsymUnlock();
            return pDynSym;
         }
      }

      /*
       *  We did not find the symbol, but "nCount" looks closer to the tree search
       *  than "nCountDial", when searching for "nCountDialog". So our best chance
       *  is to cut off szName up to 10 chars and redo the search.
       */

      if( iLen1 > 10 && iLen2 < 10 )
      {
         USHORT   uiFirst  = 0;
         USHORT   uiLast   = s_uiDynSymbols;
         USHORT   uiMiddle = uiLast / 2;
         char     szNameLimited[ 10 + 1 ];
         char *   pDest    = szNameLimited;

         iLen1          = 10;

         pDest[ iLen1 ] = '\0';

         while( iLen1-- )
            *pDest++ = *szName++;

         s_uiClosestDynSym = uiMiddle;                  /* Start in the middle      */

         while( uiFirst < uiLast )
         {
            int iCmp = strcmp( s_pDynItems[ uiMiddle ].pDynSym->pSymbol->szName, szNameLimited );

            if( iCmp == 0 )
            {
               PHB_DYNS pDynSym = s_pDynItems[ uiMiddle ].pDynSym;

               s_uiClosestDynSym = uiMiddle;
               hb_dynsymUnlock();
               return pDynSym;
            }
            else if( iCmp < 0 )
            {
               uiLast            = uiMiddle;
               s_uiClosestDynSym = uiMiddle;
            }
            else /* if( iCmp > 0 ) */
            {
               uiFirst           = uiMiddle + 1;
               s_uiClosestDynSym = uiFirst;
            }
            uiMiddle = uiFirst + ( ( uiLast - uiFirst ) / 2 );
         }
      }

      /*
       *  In other hand, if szName has 10 chars and the Symbol table contains a similar
       *  entry, s_uiClosestDynSym may be wrong.
       *  For instance, if we search for "cAliasRela", but in the symbol table we have
       *  "cAliasRelac" and "cAliasNiv", the tree schema returns the wrong entry as the
       *  closest ("cAliasNiv"). The best solution in this case is to complete szName
       *  with some trailing chars ( "_" ), and redo the process.
       */

      if( iLen1 == 10 && iLen2 < 10 )
      {
         USHORT   uiFirst  = 0;
         USHORT   uiLast   = s_uiDynSymbols;
         USHORT   uiMiddle = uiLast / 2;
         USHORT   iuCount;
         char     szNameExtended[ 10 + 8 ];
         char *   pDest    = szNameExtended;

         pDest[ 17 ] = '\0';

         for( iuCount = 0; iuCount < 17; iuCount++ )
         {
            if( iuCount < 10 )
               pDest[ iuCount ] = szName[ iuCount ];
            else
               pDest[ iuCount ] = '_';
         }

         s_uiClosestDynSym = uiMiddle;                  /* Start in the middle      */

         while( uiFirst < uiLast )
         {
            int iCmp = strcmp( s_pDynItems[ uiMiddle ].pDynSym->pSymbol->szName, szNameExtended );

            if( iCmp == 0 )
            {
               PHB_DYNS pDynSym = s_pDynItems[ uiMiddle ].pDynSym;

               s_uiClosestDynSym = uiMiddle;
               hb_dynsymUnlock();
               return pDynSym;
            }
            else if( iCmp < 0 )
            {
               uiLast            = uiMiddle;
               s_uiClosestDynSym = uiMiddle;
            }
            else /* if( iCmp > 0 ) */
            {
               uiFirst           = uiMiddle + 1;
               s_uiClosestDynSym = uiFirst;
            }
            uiMiddle = uiFirst + ( ( uiLast - uiFirst ) / 2 );
         }

         iLen1 = strlen( szName );
         iLen2 = strlen( s_pDynItems[ s_uiClosestDynSym ].pDynSym->pSymbol->szName );
         bOk   = 1;

         if( iLen2 > 10 )
         {
            for( uiPos = 0; uiPos < iLen1; uiPos++ )
            {
               if( szName[ uiPos ] != s_pDynItems[ s_uiClosestDynSym ].pDynSym->pSymbol->szName[ uiPos ] )
               {
                  bOk = 0;
                  break;
               }
            }

            if( bOk )
            {
               PHB_DYNS pDynSym = s_pDynItems[ s_uiClosestDynSym ].pDynSym;

               hb_dynsymUnlock();
               return pDynSym;
            }
         }
      }
   }

#endif

   hb_dynsymUnlock();

   return NULL;
}

PHB_DYNS hb_dynsymFindWithNamespaces( const char * szName, const char * pNamespaces )
{
   if( pNamespaces )
   {
      char *   szNamespace = ( char * ) pNamespaces;
      PHB_DYNS pDynSym;

      while( *szNamespace )
      {
         char * szQualified = hb_xstrcpy( NULL, szNamespace, ".", szName, NULL );

         pDynSym = hb_dynsymFind( szQualified );

         hb_xfree( szQualified );

         if( pDynSym )
            return pDynSym;

         szNamespace += strlen( szNamespace ) + 1;
      }

      return NULL;
   }
   else
      return hb_dynsymFind( szName );
}

PHB_DYNS hb_dynsymFindNameWithNamespaces( const char * szName, const char * pNamespaces )
{
   if( pNamespaces )
   {
      char *   szNamespace = ( char * ) pNamespaces;
      PHB_DYNS pDynSym;
      char     szUprName[ HB_SYMBOL_NAME_LEN + 1 ];

      {
         int      iLen  = HB_SYMBOL_NAME_LEN;
         char *   pDest = szUprName;

         do
         {
            char cChar = *szName++;
            if( cChar == 0 || cChar == ' ' || cChar == '\t' )
               break;
            else if( cChar >= 'a' && cChar <= 'z' )
               *pDest++ = cChar - ( 'a' - 'A' );
            else
               *pDest++ = cChar;
         }
         while( --iLen );
         *pDest = '\0';
      }

      while( *szNamespace )
      {
         char * szQualified = hb_xstrcpy( NULL, szNamespace, ".", szUprName, NULL );

         pDynSym = hb_dynsymFindName( szQualified );

         hb_xfree( szQualified );

         if( pDynSym )
            return pDynSym;

         szNamespace += strlen( szNamespace ) + 1;
      }

      return NULL;
   }
   else
      return hb_dynsymFindName( szName );
}

PHB_SYMB hb_dynsymGetSymbol( const char * szName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymGetSymbol(%s)", szName ) );

   return hb_dynsymGet( szName )->pSymbol;
}

PHB_SYMB hb_dynsymFindSymbol( const char * szName )
{
   PHB_DYNS pDynSym;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymFindSymbol(%s)", szName ) );

   pDynSym = hb_dynsymFind( szName );

   if( pDynSym->pSymbol )
      return pDynSym->pSymbol;

   return NULL;
}

PHB_SYMB hb_dynsymSymbol( PHB_DYNS pDynSym )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymSymbol(%p)", pDynSym ) );

   return pDynSym->pSymbol;
}

const char * hb_dynsymName( PHB_DYNS pDynSym )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymName(%p)", pDynSym ) );

   return pDynSym->pSymbol->szName;
}

BOOL hb_dynsymIsFunction( PHB_DYNS pDynSym )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymIsFunction(%p)", pDynSym ) );

   return pDynSym->pSymbol->value.pFunPtr != NULL;
}

HB_HANDLE hb_dynsymMemvarHandle( PHB_DYNS pDynSym )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymMemvarHandle(%p)", pDynSym ) );

   return pDynSym->hMemvar;
}

HB_HANDLE hb_dynsymAreaHandle( PHB_DYNS pDynSym )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymAreaHandle(%p)", pDynSym ) );

   return pDynSym->hArea;
}

void hb_dynsymSetAreaHandle( PHB_DYNS pDynSym, const int iArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymSetAreaHandle(%p,%d)", pDynSym, iArea ) );

   pDynSym->hArea = ( HB_HANDLE ) iArea;
}

UINT hb_dynsymEval( PHB_DYNS_FUNC pFunction, void * Cargo )
{
   register UINT  uiPos;
   BOOL           bCont = TRUE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymEval(%p, %p)", pFunction, Cargo ) );

   hb_dynsymLock();

   for( uiPos = 0; uiPos < s_uiDynSymbols && bCont; uiPos++ )
      bCont = ( pFunction ) ( s_pDynItems[ uiPos ].pDynSym, Cargo );

   hb_dynsymUnlock();

   return uiPos;
}

/* JC1: this is called at VM termination, no need to lock */
void hb_dynsymRelease( void )
{
   register UINT  uiPos;
   PHB_SYM_HOLDER pHolder;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dynsymRelease()" ) );

   for( uiPos = 0; uiPos < s_uiDynSymbols; uiPos++ )
      hb_xfree( ( s_pDynItems + uiPos )->pDynSym );

   hb_xfree( s_pDynItems );

   s_pDynItems    = NULL;
   s_uiDynSymbols = 0;

   while( s_pAllocSyms )
   {
      pHolder        = s_pAllocSyms;
      s_pAllocSyms   = s_pAllocSyms->pNext;
      hb_xfree( pHolder );
   }
}

/* NOT TESTED YET!!!
 */
PHB_DYNS hb_dynsymPos( USHORT uiPos )
{
   PHB_DYNS ret = NULL;

   hb_dynsymLock();

   if( ( UINT ) uiPos < s_uiDynSymbols )
      ret = s_pDynItems[ uiPos ].pDynSym;

   hb_dynsymUnlock();

   return ret;
}

PDYNHB_ITEM hb_dynsymItems( void )
{
   return s_pDynItems;
}

UINT * hb_dynsymCount( void )
{
   return &s_uiDynSymbols;
}

#ifdef HB_EXTENSION

HB_FUNC( __DYNSCOUNT ) /* How much symbols do we have: dsCount = __dynsymCount() */
{
   HB_THREAD_STUB_API
   hb_retnl( ( LONG ) s_uiDynSymbols );
}

HB_FUNC( __DYNSGETNAME ) /* Get name of symbol: cSymbol = __dynsymGetName( dsIndex ) */
{
   HB_THREAD_STUB_API
   LONG           lIndex   = hb_parnl( 1 ); /* NOTE: This will return zero if the parameter is not numeric */
   const char *   szRet    = "";

   hb_dynsymLock();

   if( lIndex >= 1 && lIndex <= ( LONG ) s_uiDynSymbols )
   {
      szRet = s_pDynItems[ lIndex - 1 ].pDynSym->pSymbol->szName;
   }
   hb_retc( szRet );

   hb_dynsymUnlock();
}

HB_FUNC( __DYNSGETINDEX ) /* Gimme index number of symbol: dsIndex = __dynsymGetIndex( cSymbol ) */
{
   HB_THREAD_STUB
   LONG     lRet = 0L;
   PHB_DYNS pDynSym;

   /* JC1: Does not needs lock... */
   hb_dynsymLock();

   pDynSym = hb_dynsymFindName( hb_parcx( 1 ) );

   if( pDynSym )
   {
      /* ... because this is from HB_VM_STACK (see macros at top of file) */
      lRet = ( LONG ) ( s_uiClosestDynSym + 1 );
   }
   hb_retnl( lRet );

   hb_dynsymUnlock();
}

HB_FUNC( __DYNSISFUN ) /* returns .t. if a symbol has a function/procedure pointer,
                          given its symbol index */
{
   HB_THREAD_STUB_API
   LONG lIndex = hb_parnl( 1 ); /* NOTE: This will return zero if the parameter is not numeric */

   hb_dynsymLock();

   if( lIndex >= 1 && lIndex <= ( LONG ) s_uiDynSymbols )
      hb_retl( hb_dynsymIsFunction( s_pDynItems[ lIndex - 1 ].pDynSym ) );
   else
      hb_retl( FALSE );

   hb_dynsymUnlock();
}

HB_FUNC( __DYNSGETPRF ) /* profiler: It returns an array with a function or procedure
                                     called and consumed times { nTimes, nTime }
                                     , given the dynamic symbol index */
{
   HB_THREAD_STUB_API

#ifndef HB_NO_PROFILER
   /* NOTE: This will return zero if the parameter is not numeric */
   LONG lIndex = hb_parnl( 1 );
#endif

   hb_reta( 2 );
   hb_stornl( 0, -1, 1 );
   hb_stornl( 0, -1, 2 );

#ifndef HB_NO_PROFILER
   hb_dynsymLock();

   if( lIndex >= 1 && lIndex <= ( LONG ) s_uiDynSymbols )
   {
      if( hb_dynsymIsFunction( s_pDynItems[ lIndex - 1 ].pDynSym ) ) /* it is a function or procedure */
      {
         hb_stornl( ( LONG ) s_pDynItems[ lIndex - 1 ].pDynSym->ulCalls, -1, 1 );
         hb_stornl( ( LONG ) s_pDynItems[ lIndex - 1 ].pDynSym->ulTime, -1, 2 );
      }
   }

   hb_dynsymUnlock();
#endif
}

#endif

/* JC1: Reentrant functions */
#ifdef HB_THREAD_SUPPORT

PHB_DYNS hb_dynsymNew_r( PHB_SYMB pSymbol, PSYMBOLS pModuleSymbols, PHB_DYNS pDest )
{
   PHB_DYNS pRet;

   hb_dynsymLock();

   pRet = hb_dynsymNew( pSymbol, pModuleSymbols );

   if( pRet )
   {
      HB_MEMCPY( pDest, pRet, sizeof( HB_DYNS ) );
      hb_dynsymUnlock();
      return pDest;
   }

   hb_dynsymUnlock();

   return NULL;
}

PHB_DYNS hb_dynsymGet_r( const char * szName, PHB_DYNS pDest )
{
   PHB_DYNS pRet;

   hb_dynsymLock();

   pRet = hb_dynsymGet( szName );

   if( pRet )
   {
      HB_MEMCPY( pDest, pRet, sizeof( HB_DYNS ) );
      hb_dynsymUnlock();
      return pDest;
   }

   hb_dynsymUnlock();

   return NULL;
}

PHB_DYNS hb_dynsymGetCase_r( const char * szName, PHB_DYNS pDest )
{
   PHB_DYNS pRet;

   hb_dynsymLock();

   pRet = hb_dynsymGetCase( szName );

   if( pRet )
   {
      HB_MEMCPY( pDest, pRet, sizeof( HB_DYNS ) );
      hb_dynsymUnlock();
      return pDest;
   }

   hb_dynsymUnlock();

   return NULL;
}

PHB_DYNS hb_dynsymFind_r( const char * szName, PHB_DYNS pDest )
{
   PHB_DYNS pRet;

   hb_dynsymLock();

   pRet = hb_dynsymFind( szName );

   if( pRet )
   {
      HB_MEMCPY( pDest, pRet, sizeof( HB_DYNS ) );
      hb_dynsymUnlock();
      return pDest;
   }

   hb_dynsymUnlock();

   return NULL;
}

PHB_DYNS hb_dynsymFindName_r( const char * szName, PHB_DYNS pDest )
{
   PHB_DYNS pRet;

   hb_dynsymLock();

   pRet = hb_dynsymFindName( szName );

   if( pRet )
   {
      HB_MEMCPY( pDest, pRet, sizeof( HB_DYNS ) );
      hb_dynsymUnlock();
      return pDest;
   }

   hb_dynsymUnlock();

   return NULL;
}

#endif /* HB_THREAD_SUPPORT */
