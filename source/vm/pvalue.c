/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PVALUE() function
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"

HB_FUNC( PVALUE )
{
   USHORT      uiParam  = ( USHORT ) hb_parni( 1 ), uiParams;
   PHB_ITEM *  pBase    = HB_VM_STACK.pItems + ( hb_stackBaseItem() )->item.asSymbol.pCargo->stackbase; /* Skip function + self */

   uiParams = ( *pBase )->item.asSymbol.pCargo->arguments;

   if( uiParam && uiParam <= uiParams ) /* Valid number */
   {
      USHORT   uiOffset;
      PHB_ITEM pItem;

      if( ( *pBase )->item.asSymbol.pCargo->params == HB_VAR_PARAM_FLAG )
         uiOffset = ( *pBase )->item.asSymbol.pCargo->locals;
      else
         uiOffset = 0;

      pItem = *( pBase + 1 + uiParam + uiOffset );

      hb_itemReturn( pItem );

      if( hb_pcount() > 1 && HB_IS_BYREF( pItem ) )
         hb_itemCopy( pItem, hb_param( 2, HB_IT_ANY ) );
   }
   else
      hb_ret();    /* return NIL */
}
