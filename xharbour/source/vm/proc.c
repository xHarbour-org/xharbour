/*
 * $Id: proc.c,v 1.3 2002/12/19 18:15:36 ronpinkas Exp $
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

HB_FUNC( METHODNAME )
{
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];

   hb_retc( hb_procinfo( hb_parni( 1 ) + 1, szName, NULL ) );
}

HB_FUNC( PROCNAME )
{
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];

   hb_retc( hb_procinfo( hb_parni( 1 ) + 1, szName, NULL ) );
}

HB_FUNC( PROCLINE )
{
   USHORT uLine = 0;

   hb_procinfo( hb_parni( 1 ) + 1, NULL, &uLine );
   hb_retni( uLine );
}

#ifdef HB_C52_UNDOC

/* NOTE: Clipper undocumented function, which always returns an empty
         string. [vszakats] */

HB_FUNC( PROCFILE )
{
   hb_retc( "" );
}

#endif

/* NOTE: szName size must be an at least: HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 */

char * hb_procinfo( int iLevel, char *szName, USHORT *uLine  )
{
   PHB_ITEM * pBase = HB_VM_STACK.pBase;
   //BOOL bBlock = FALSE ;

   // Default and safety to empty string.
   if( szName )
   {
      szName[0] = '\0';
   }

   while( iLevel-- > 0 && pBase != HB_VM_STACK.pItems )
   {
      pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
   }

   if( iLevel < 0 )
   {
      #if 0
      /* Is it a block evaluation or inline method? if so back one more ... */
      if( ( strcmp( ( *pBase )->item.asSymbol.value->szName, "EVAL" ) == 0 ||
            strcmp( ( *pBase )->item.asSymbol.value->szName, "__EVAL" ) == 0 ) &&  pBase != HB_VM_STACK.pItems )
      {
         pBase = HB_VM_STACK.pItems + ( *pBase )->item.asSymbol.stackbase;
         bBlock = TRUE ;
      }
      #endif

      if( szName )
      {
         if( ( *( pBase + 1 ) )->type == HB_IT_ARRAY )  /* it is a method name */
         {
            strcpy( szName, hb_objGetRealClsName( *( pBase + 1 ), ( *pBase )->item.asSymbol.value->szName ) );
            strcat( szName, ":" );
         }

         if( ( *( pBase + 1 ) )->type == HB_IT_BLOCK )  /* it is a Block Evaluation. */
         {
            strcat( szName, "(b)" );
            strcat( szName, ( *( pBase + 1 ) )->item.asBlock.value->procname );
         }
         else
         {
            strcat( szName, ( *pBase )->item.asSymbol.value->szName );
         }
      }

      if( uLine )
      {
         if( ( *( pBase + 1 ) )->type == HB_IT_BLOCK )  /* it is a Block Evaluation. */
         {
            *uLine = ( *( pBase + 1) )->item.asBlock.value->lineno;
         }
         else
         {
            *uLine = ( *pBase )->item.asSymbol.lineno;
         }
      }
   }

   return szName;
}
