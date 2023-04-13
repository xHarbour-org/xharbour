/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    string API functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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

   Used parts from Harbour for use with SQLite3 lib
   November 18, 2011 by R.Visscher <richard@irvis.com>
*/

#ifndef HB_APISTR_H_
#define HB_APISTR_H_

#include "hbapi.h"
#include "hbapicdp.h"

HB_EXTERN_BEGIN

extern HB_EXPORT const char * hb_parstr_utf8( int iParam, void ** phString, HB_SIZE * pnLen );
extern HB_EXPORT void hb_retstrlen_utf8( const char * szText, HB_SIZE nLen );
extern HB_EXPORT void hb_retstr_utf8( const char * szText );
extern HB_EXPORT void hb_strfree( void * hString );
extern HB_EXPORT const char * hb_strnull( const char * str );
extern HB_EXPORT const char * hb_itemGetStrUTF8( PHB_ITEM pItem, void ** phString, HB_SIZE * pnLen );
extern HB_EXPORT PHB_ITEM hb_itemPutStrLenUTF8( PHB_ITEM pItem, const char * pStr, HB_SIZE nLen );
extern HB_EXPORT PHB_ITEM hb_itemPutStrUTF8( PHB_ITEM pItem, const char * pStr );
extern HB_EXPORT BOOL hb_arraySetStrUTF8( PHB_ITEM pArray, HB_SIZE nIndex, const char * pStr);
extern HB_EXPORT const char * hb_strget( PHB_ITEM pItem, void ** phStr, HB_SIZE * pnLen );
extern HB_EXPORT HB_SIZE hb_strcopy( PHB_ITEM pItem, char * pStr, HB_SIZE nLen );

HB_EXTERN_END

#endif /* HB_APISTR_H_ */

