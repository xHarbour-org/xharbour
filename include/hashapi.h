/*
 * $Id$
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
HB_EXTERN_BEGIN
#define  HB_HASH_ALLOC_BLOCK  16

HB_EXPORT PHB_ITEM hb_hashNew( PHB_ITEM pItem );
HB_EXPORT BOOL hb_hashAdd( PHB_ITEM pHash, HB_SIZE ulPos, PHB_ITEM pKey, PHB_ITEM pValue );
HB_EXPORT BOOL hb_hashAddForward( PHB_ITEM pHash, HB_SIZE ulPos, PHB_ITEM pKey, PHB_ITEM pValue );
HB_EXPORT BOOL hb_hashRemove( PHB_ITEM pHash, HB_SIZE ulPos );
HB_EXPORT BOOL hb_hashScan( PHB_ITEM pHash, PHB_ITEM pKey, HB_SIZE *ulIndex );
HB_EXPORT BOOL hb_hashSet( PHB_ITEM pHash, HB_SIZE ulIndex, PHB_ITEM pItem );
HB_EXPORT BOOL hb_hashSetForward( PHB_ITEM pHash, HB_SIZE ulIndex, PHB_ITEM pItem );
HB_EXPORT BOOL hb_hashGet( PHB_ITEM pHash, HB_SIZE ulIndex, PHB_ITEM pItem );
HB_EXPORT BOOL hb_hashGetForward( PHB_ITEM pHash, HB_SIZE ulIndex, PHB_ITEM pItem );
HB_EXPORT void hb_hashSetCaseMatch( PHB_ITEM pHash, BOOL bCase );

HB_EXPORT void hb_hashPreallocate( PHB_ITEM pHash, HB_SIZE ulLength );
HB_EXPORT PHB_ITEM hb_hashClone( PHB_ITEM pSrcHash, PHB_ITEM pDestHash );
HB_EXPORT void hb_hashMerge( PHB_ITEM pDest, PHB_ITEM pSource,
      HB_SIZE ulStart, HB_SIZE ulEnd, PHB_ITEM pBlock );

HB_EXPORT  BOOL hb_hashSetAACompatibility( PHB_ITEM pHash, BOOL bCompatAA, BOOL bSilent );

#ifdef HB_API_MACROS
//   #define hb_hashLen( pHash )                  ( pHash )->item.asHash.value->ulTotalLen
   #define hb_hashGetCompatibility( pHash )     (( pHash )->item.asHash.value->pAccessAA == NULL?FALSE:TRUE)
   #define hb_hashAAGetRealPos( pHash, ulPos )  ( ( ( ulPos ) > 0 && ( ulPos) <= hb_hashLen( ( pHash ) ) )? *(( pHash )->item.asHash.value->pAccessAA + ( ulPos ) - 1 ) : 0 )
#else
   
   HB_EXPORT  BOOL hb_hashGetCompatibility( PHB_ITEM pHash );
   HB_EXPORT HB_SIZE hb_hashAAGetRealPos( PHB_ITEM pHash, HB_SIZE ulPos );
#endif
HB_EXPORT HB_SIZE hb_hashLen( PHB_ITEM pHash );
HB_EXPORT PHB_ITEM hb_hashGetKeys( PHB_ITEM pKeys, PHB_ITEM pHash );
HB_EXPORT PHB_ITEM hb_hashGetValues( PHB_ITEM pValues, PHB_ITEM pHash );
HB_EXPORT PHB_ITEM hb_hashGetKeyAt( PHB_ITEM pHash, HB_SIZE ulPos );
HB_EXPORT PHB_ITEM hb_hashGetValueAt( PHB_ITEM pHash, HB_SIZE ulPos );


HB_EXPORT PHB_ITEM hb_hashGetKeyAt( PHB_ITEM pHash, HB_SIZE ulPos );
HB_EXPORT PHB_ITEM hb_hashGetValueAt( PHB_ITEM pHash, HB_SIZE ulPos );

void hb_hashReleaseBase( PHB_BASEHASH pBaseHash );
HB_EXPORT BOOL hb_hashRelease( PHB_ITEM pHash );
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
HB_EXTERN_END
#endif

