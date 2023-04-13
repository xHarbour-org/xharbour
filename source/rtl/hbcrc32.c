
/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Crc32 checksum function
 *
 * Copyright 2003 Luiz Rafael Culik Guimaraes <culikr@uol.com.br>
 * www - http://www.xharbour.org
 * SEE ALSO COPYRIGHT NOTICE FOR CRC32 BELOW.
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

/* This file includes code slices from crc32.c for advanced CRC
 * Holder of copyright for this code is:
 *
 * Copyright (C) 1995-2002 Mark Adler
 *
 * ZLIB (containing crc32 code) can be found at:
 * http://www.gzip.org/zlib/
 */

#include "hbzlib.h"

HB_FUNC( HB_CRC32 )
{
   PHB_ITEM pString  = hb_param( 1, HB_IT_STRING );
   ULONG    ulSum    = 0;

   if( pString == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "Must be a string", 1, hb_param( 1, HB_IT_ANY ) );
      return;
   }

   if( ISNUM( 2 ) )
   {
      ulSum = ( ULONG ) hb_parnl( 2 );
   }

   hb_retnint( crc32( ulSum, ( const BYTE * ) hb_itemGetCPtr( pString ),
                      ( uInt ) hb_itemGetCLen( pString ) ) );
}

