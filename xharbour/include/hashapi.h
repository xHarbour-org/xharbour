/*
 * $Id: hbhashapi.h,v 1.5 2003/11/23 03:13:53 jonnymind Exp $
 */

/*
 * Harbour Project source code:
 * The HASH API (C level)
 *
 * Copyright 2003 Giancarlo Niccolai
 * www - http://www.xharbour.org
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


#ifndef HB_HASHAPI_H_
#define HB_HASHAPI_H_

#include "hbsetup.h"

#define  HB_HASH_ALLOC_BLOCK  16

PHB_ITEM HB_EXPORT hb_hashNew( PHB_ITEM pItem );
BOOL HB_EXPORT hb_hashAdd( PHB_ITEM pHash, ULONG ulPos, PHB_ITEM pKey, PHB_ITEM pValue );
BOOL HB_EXPORT hb_hashAddForward( PHB_ITEM pHash, ULONG ulPos, PHB_ITEM pKey, PHB_ITEM pValue );
BOOL HB_EXPORT hb_hashRemove( PHB_ITEM pHash, ULONG ulPos );
BOOL HB_EXPORT hb_hashScan( PHB_ITEM pHash, PHB_ITEM pKey, ULONG *ulIndex );
ULONG HB_EXPORT hb_hashLen( PHB_ITEM pHash );
BOOL HB_EXPORT hb_hashSet( PHB_ITEM pHash, ULONG ulIndex, PHB_ITEM pItem );
BOOL HB_EXPORT hb_hashSetForward( PHB_ITEM pHash, ULONG ulIndex, PHB_ITEM pItem );
BOOL HB_EXPORT hb_hashGet( PHB_ITEM pHash, ULONG ulIndex, PHB_ITEM pItem );

void HB_EXPORT hb_hashPreallocate( PHB_ITEM pHash, ULONG ulLength );
PHB_ITEM HB_EXPORT hb_hashClone( PHB_ITEM pSrcHash );
void HB_EXPORT hb_hashMerge( PHB_ITEM pDest, PHB_ITEM pSource,
      ULONG ulStart, ULONG ulEnd, PHB_ITEM pBlock );

PHB_ITEM HB_EXPORT hb_hashGetKeys( PHB_ITEM pHash );
PHB_ITEM HB_EXPORT hb_hashGetValues( PHB_ITEM pHash );
PHB_ITEM HB_EXPORT hb_hashGetKeyAt( PHB_ITEM pHash, ULONG ulPos );
PHB_ITEM HB_EXPORT hb_hashGetValueAt( PHB_ITEM pHash, ULONG ulPos );


PHB_ITEM HB_EXPORT hb_hashGetKeyAt( PHB_ITEM pHash, ULONG ulPos );
PHB_ITEM HB_EXPORT hb_hashGetValueAt( PHB_ITEM pHash, ULONG ulPos );


void hb_hashReleaseBase( PHB_BASEHASH pBaseHash );
BOOL HB_EXPORT hb_hashRelease( PHB_ITEM pHash );
HB_GARBAGE_FUNC( hb_hashReleaseGarbage );

/* Some utility macro */
#define hb_hashAddChar( pHash, czKey, pValue ) \
   {\
      HB_ITEM ___it_temp_;\
      ___it_temp_.type= HB_IT_NIL;\
      hb_hashAdd( pHash, ULONG_MAX, hb_itemPutCRawStatic( &___it_temp_, czKey, strlen(czKey) ), pValue );\
   }

#define hb_hashRemoveChar( pHash, czKey ) \
   {\
      HB_ITEM ___it_temp_;\
      ___it_temp_.type= HB_IT_NIL;\
      hb_hashRemove( pHash, hb_itemPutC( &___it_temp_, czKey ) );\
   }

#endif

