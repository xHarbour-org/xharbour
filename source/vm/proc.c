/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PROCNAME(), PROCLINE() and PROCFILE() functions
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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    PROCFILE()
 *
 * Copyright 2001 JFL (Mafact) <jfl@mafact.com>
 *    Adding the MethodName() just calling Procname()
 *    call to hb_objGetRealClsName in case of object
 *    Special treatment in case of Object and __Eval (only for methodname)
 *    skipping block and adding (b) before the method name
 *
 * Copyright 2002 Ron Pinkas <ron@ronpinkas.com>
 *    hb_procinfo()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbstack.h"
#include "classes.h"
#include "hbapierr.h"

HB_FUNC( METHODNAME )
{
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];

   hb_retc( hb_procinfo( hb_parni( 1 ) + 1, szName, NULL, NULL ) );
}

HB_FUNC( PROCNAME )
{
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];

   hb_retc( hb_procinfo( hb_parni( 1 ) + 1, szName, NULL, NULL ) );
}

HB_FUNC( PROCLINE )
{
   USHORT uLine = 0;

   hb_procinfo( hb_parni( 1 ) + 1, NULL, &uLine, NULL );

   hb_retni( uLine );
}

HB_FUNC( PROCFILE )
{
   char szModuleName[ HB_PATH_MAX ];

   hb_procinfo( hb_parni( 1 ) + 1, NULL, NULL, szModuleName );

   hb_retc( szModuleName );
}

/* NOTE: szName size must be an at least: HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 */

char * hb_procinfo( int iLevel, char * szName, USHORT * uLine, char * szModuleName  )
{
   PHB_ITEM *  pBase, pSelf;
   USHORT      uiSuperClass;
   long        lOffset;

   /* Default and safety to empty string. */
   if( szName )
      szName[ 0 ] = '\0';

   if( szModuleName )
      szModuleName[ 0 ] = '\0';

   if( uLine )
      *uLine = 0;

   /* Called from hb_vmQuit(). */
   if( HB_VM_STACK.pPos == HB_VM_STACK.pItems )
   {
      if( szName )
         hb_xstrcpy( szName, "hb_vmQuit", 0 );

      if( szModuleName )
         hb_xstrcpy( szModuleName, "hvm.c", 0 );

      return szName;
   }

   lOffset = ( long ) hb_stackBaseProcOffset( iLevel );

   if( lOffset > 0 && ( pBase = hb_stackGetBase( iLevel ) ) != NULL )
   {
      pSelf = *( pBase + 1 );

      if( szName )
      {
         if( HB_IS_OBJECT( pSelf ) )  /* it is a method name */
         {
            uiSuperClass = ( USHORT ) ( *pBase )->item.asSymbol.pCargo->uiSuperClass;

            if( uiSuperClass && uiSuperClass <= hb_clsMaxClasses() )
               hb_xstrcpy( szName, ( hb_clsClassesArray() + ( *pBase )->item.asSymbol.pCargo->uiSuperClass - 1 )->szName, 0 );
            else
               hb_xstrcpy( szName, hb_objGetClsName( pSelf ), 0 );

            hb_xstrcat( szName, ":", ( *pBase )->item.asSymbol.value->szName, 0 );
         }
         else if( HB_IS_BLOCK( pSelf ) )  /* it is a Block Evaluation. */
         {
            hb_xstrcpy( szName, "(b)", 0 );

            if( pSelf->item.asBlock.value->uiClass )
            {
               if( pSelf->item.asBlock.value->uiClass <= hb_clsMaxClasses() )
                  hb_xstrcat( szName, hb_clsName( pSelf->item.asBlock.value->uiClass ), ":", 0 );
               else
                  hb_errInternal( HB_EI_ERRUNRECOV, "Corrupted codeblock, points to invalid class id!", NULL, NULL );
            }

            hb_xstrcat( szName, ( *pBase )->item.asSymbol.value->szName, 0 );
         }
         else
         {
            const char * pPureName = strrchr( ( *pBase )->item.asSymbol.value->szName, '.' );

            if( pPureName )
               hb_strncpy( szName, pPureName + 1, HB_SYMBOL_NAME_LEN );
            else
               hb_strncpy( szName, ( *pBase )->item.asSymbol.value->szName, HB_SYMBOL_NAME_LEN );
         }
      }

      if( uLine )
      {
         if( HB_IS_OBJECT( pSelf ) && strcmp( "TASSOCIATIVEARRAY", hb_objGetClsName( pSelf ) ) == 0 )
            *uLine = 0;
         else if( HB_IS_BLOCK( pSelf ) )  /* it is a Block Evaluation. */
            *uLine = pSelf->item.asBlock.value->lineno;
         else
            *uLine = ( *pBase )->item.asSymbol.pCargo->lineno;
      }

      if( szModuleName )
      {
#if 0
         if( HB_IS_OBJECT( pSelf ) )    /* it is a method name */
         {
            /* Find the real module where the Method is defined. */
            if( ( *pBase )->item.asSymbol.pCargo->uiSuperClass )
            {
               uiSuperClass = ( *pBase )->item.asSymbol.uiSuperClass;
            }
            else
            {
               if( pSelf->item.asArray.value->puiClsTree && pSelf->item.asArray.value->puiClsTree[ 0 ] )
               {
                  /* Save. */
                  UINT uiPos = pSelf->item.asArray.value->puiClsTree[ 0 ];

                  /* Hide. */
                  pSelf->item.asArray.value->puiClsTree[ 0 ]   = 0;

                  uiSuperClass                                 = hb_objGetRealCls( pSelf, ( *pBase )->item.asSymbol.value->szName );

                  /* Restore. */
                  pSelf->item.asArray.value->puiClsTree[ 0 ]   = uiPos;
               }
               else
                  uiSuperClass = hb_objGetRealCls( pSelf, ( *pBase )->item.asSymbol.value->szName );

               if( uiSuperClass == 0 )
                  uiSuperClass = pSelf->item.asArray.value->uiClass;
            }

            if( uiSuperClass <= hb_clsMaxClasses() )
            {
               PCLASS pClass = hb_clsClassesArray() + uiSuperClass - 1;

               if( pClass->pModuleSymbols && pClass->pModuleSymbols->szModuleName )
                  hb_xstrcpy( szModuleName, pClass->pModuleSymbols->szModuleName, 0 );
            }
            else
            {
               TraceLog( "Error.log", "Corrupted object, points to invalid class id: %i of %i", pSelf->item.asArray.value->uiClass, hb_clsMaxClasses() );
               hb_errInternal( HB_EI_ERRUNRECOV, "Corrupted object, points to invalid class id!", NULL, NULL );
            }
         }
         else if( HB_IS_BLOCK( pSelf ) )     /* it is a Block Evaluation. */
         {
            PSYMBOLS pBlockModuleSymbols = pSelf->item.asBlock.value->pModuleSymbols;

            if( pBlockModuleSymbols && pBlockModuleSymbols->szModuleName )
               hb_xstrcpy( szModuleName, pBlockModuleSymbols->szModuleName, 0 );
         }
         else
#else
         {
            if( pBase )
            {
               PSYMBOLS pModuleSymbols = HB_BASE_GETMODULESYM( pBase );

               /* TraceLog( NULL, "Sym: %s Dyn: %p Module: %p\n", hb_itemGetSymbol( *pBase )->szName, hb_itemGetSymbol( *pBase )->pDynSym, pModuleSymbols );
                */

               if( pModuleSymbols && pModuleSymbols->szModuleName )
                  hb_xstrcpy( szModuleName, pModuleSymbols->szModuleName, 0 );
            }
         }
#endif
      }
   }
   return szName;
}
