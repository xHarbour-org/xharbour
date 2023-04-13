/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * The FastItem Optimization API
 *
 * Copyright 2001 Ron Pinkas <ron@@ronpinkas.com>
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */

#ifndef HB_FASTITEM_H_
   #define HB_FASTITEM_H_

   #include "hbapi.h"

   HB_EXTERN_BEGIN

    typedef struct _HB_SCANNED_ARRAYS
    {
       PHB_BASEARRAY             pScannedBaseArray;
       struct _HB_SCANNED_ARRAYS * pNext;
    } HB_SCANNED_ARRAYS, * PHB_SCANNED_ARRAYS;

   extern HB_EXPORT PHB_ITEM hb_itemPutCStatic( PHB_ITEM pItem, const char * szText );
   extern HB_EXPORT PHB_ITEM hb_itemPutCLStatic( PHB_ITEM pItem, const char * szText, HB_SIZE ulLen );
   extern HB_EXPORT void     hb_itemPushForward( PHB_ITEM pItem );
   extern HB_EXPORT void     hb_itemForwardValue( PHB_ITEM pDest, PHB_ITEM pSource );
   extern HB_EXPORT void     hb_itemReleaseString( PHB_ITEM pItem );
   extern HB_EXPORT void     hb_itemPushStaticString( const char * szText, HB_SIZE length );

   extern HB_EXPORT USHORT hb_itemArrayCyclicCount( PHB_ITEM pArray );
   extern HB_EXPORT USHORT hb_itemArrayCyclicCountWorker( PHB_BASEARRAY pArray, PHB_SCANNED_ARRAYS pScannedList, PHB_BASEARRAY pTopBaseArray );
   extern HB_EXPORT BYTE   hb_itemParamId( PHB_ITEM pItem );

   #define hb_itemMove( dst, src )     hb_itemForwardValue( (dst), (src) )

   HB_EXTERN_END

#endif
