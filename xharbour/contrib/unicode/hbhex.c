/*
 * $Id: hbhex.c,v 1.1 2004/01/14 13:59:52 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_NLTOHX()
 *    HB_HXTONL()
 *
 * Copyright 2004 Dmitry V. Korzhov <dk@april26.spb.ru>
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
HB_NLTOHX(n) -> c
      Converts integer variables to string
   Parameters:
      n - numeric value (0-4294967295)
   Returns:
      hexadecimal string ("0"-"FFFFFFFF") representation

HB_HXTONL(c) -> n
      Gets integer numeric variable from string
   Parameters:
      c - hexadecimal string ("0"-"FFFFFFFF") representation
   Returns:
      numeric value (0-4294967295)
*/

#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC(HB_NLTOHX)
{
	BYTE *dststr;
	if (hb_pcount()) {
		dststr=(BYTE *) hb_xgrab(9);
		sprintf((char *) dststr,"%lX",hb_parnl(1));
		hb_retc((char *) dststr);
		hb_xfree(dststr);
	}
	else hb_retc("");
}

HB_FUNC(HB_NLTOH8)
{
	int i;
	BYTE *dststr;
	if (hb_pcount()) {
		dststr=(BYTE *) hb_xgrab(9);
		sprintf((char *) dststr,"%08lX",hb_parnl(1));
		for (i=0;i<8;i++) if (dststr[i]==' ') dststr[i]='0';
		hb_retclen((char *) dststr,8);
		hb_xfree(dststr);
	}
	else hb_retc("");
}

HB_FUNC(HB_HXTONL)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG val=0l;
	BYTE *srcstr;
	if (phbstr) {
		srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
		sscanf((char *) srcstr,"%lX",&val);
		hb_retnl(val);
	}
	else hb_retnl(0);
}
