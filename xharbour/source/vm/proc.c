/*
 * $Id: proc.c,v 1.16 2003/09/12 15:28:47 ronpinkas Exp $
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
   char szModuleName[ _POSIX_PATH_MAX + 1 ];

   hb_procinfo( hb_parni( 1 ) + 1, NULL, NULL, szModuleName );

   hb_retc( szModuleName );
}

/* NOTE: szName size must be an at least: HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 */

char * hb_procinfo( int iLevel, char *szName, USHORT *uLine, char *szModuleName  )
{
   PHB_ITEM * pBase = HB_VM_STACK.pBase, pSelf;

   // Default and safety to empty string.
   if( szName )
   {
      szName[0] = '\0';
   }

   if( szModuleName )
   {
      szModuleName[0] = '\0';
   }

   if( uLine )
   {
      *uLine = 0;
   }

   // Called from hb_vmQuit().
   if( HB_VM_STACK.pPos == HB_VM_STACK.pItems )
   {
      if( szName )
      {
         strcpy( szName, "hb_vmQuit" );
      }

      if( szModuleName )
      {
         strcpy( szModuleName, "hvm.c" );
      }

      return szName;
   }

   while( iLevel-- > 0 && pBase != HB_VM_STACK.pItems )
   {
      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
   }

   pSelf = *( pBase + 1 );

   if( iLevel < 0 )
   {
      if( szName )
      {
         if( HB_IS_OBJECT( pSelf ) )  /* it is a method name */
         {
            strcpy( szName, hb_objGetRealClsName( pSelf, ( *pBase )->item.asSymbol.value->szName ) );
            strcat( szName, ":" );
            strcat( szName, ( *pBase )->item.asSymbol.value->szName );
         }
         else if( HB_IS_BLOCK( pSelf ) )  /* it is a Block Evaluation. */
         {
            strcpy( szName, "(b)" );

            if( pSelf->item.asBlock.value->pSelfBase )
            {
               if( pSelf->item.asBlock.value->pSelfBase->uiClass <= hb_clsMaxClasses() )
               {
                  PCLASS pClass = hb_clsClassesArray() + ( pSelf->item.asBlock.value->pSelfBase->uiClass - 1 );

                  strcat( szName, pClass->szName );
                  strcat( szName, ":" );
               }
               else
               {
                  hb_errInternal( HB_EI_ERRUNRECOV, "Corrupted codeblock, points to invalid class id!", NULL, NULL );
               }
            }

            strcat( szName, pSelf->item.asBlock.value->procname );
         }
         else
         {
            strncpy( szName, ( *pBase )->item.asSymbol.value->szName, HB_SYMBOL_NAME_LEN + 1 );
         }
      }

      if( uLine )
      {
         if( HB_IS_OBJECT( pSelf ) && strcmp( "TASSOCIATIVEARRAY", hb_objGetClsName( pSelf ) ) == 0 )
         {
            *uLine = 0;
         }
         else if( HB_IS_BLOCK( pSelf ) )  /* it is a Block Evaluation. */
         {
            *uLine = pSelf->item.asBlock.value->lineno;
         }
         else
         {
            *uLine = ( *pBase )->item.asSymbol.lineno;
         }
      }

      if( szModuleName )
      {
         if( HB_IS_OBJECT( pSelf ) ) /* it is a method name */
         {
            if( pSelf->item.asArray.value->uiClass <= hb_clsMaxClasses() )
            {
               PCLASS pClass = hb_clsClassesArray() + ( pSelf->item.asArray.value->uiClass - 1 );

               if( pClass->pModuleSymbols && pClass->pModuleSymbols->szModuleName )
               {
                  strcpy( szModuleName, pClass->pModuleSymbols->szModuleName );
               }
            }
            else
            {
               TraceLog( "Error.log", "Corrupted object, points to invalid class id: %i of %i", pSelf->item.asArray.value->uiClass, hb_clsMaxClasses() );
               hb_errInternal( HB_EI_ERRUNRECOV, "Corrupted object, points to invalid class id!", NULL, NULL );
            }
         }
         else if( HB_IS_BLOCK( pSelf ) )  /* it is a Block Evaluation. */
         {
            PSYMBOLS pBlockModuleSymbols = hb_vmFindModule( pSelf->item.asBlock.value->pSymbols );

            if( pBlockModuleSymbols && pBlockModuleSymbols->szModuleName )
            {
               strcpy( szModuleName, pBlockModuleSymbols->szModuleName );
            }
         }
         else
         {
            if( pBase &&
                ( *pBase )->item.asSymbol.value->pDynSym &&
                ( *pBase )->item.asSymbol.value->pDynSym->pModuleSymbols &&
                ( *pBase )->item.asSymbol.value->pDynSym->pModuleSymbols->szModuleName )
            {
               strcpy( szModuleName, ( *pBase )->item.asSymbol.value->pDynSym->pModuleSymbols->szModuleName );
            }
         }
      }
   }

   return szName;
}
