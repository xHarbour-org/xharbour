/*
 * $Id: hbcy.c,v 1.1 2004/01/14 13:59:52 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_YYENCODE()
 *    HB_YYDECODE()
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
HB_YYENCODE(string) -> yy_string
      Encodes string to YYencode (some YEnc aware mail/news software used)
      NB: remember of getting CRC32 checksum for source string
   Parameters:
      string  - source character string
   Returns:
      YYencoded string

HB_YYDECODE(xx_string) -> string
      Decodes string from YYencode (1 section)
   Parameters:
      yy_string  - YYencode encoded string
   Returns:
      decoded string
*/

#include "hbapi.h"
#include "hbapiitm.h"

static BYTE *yy_tomask=(BYTE*) "\t\n\r.=\0";
static ULONG yy_len=128;

static ULONG str2yye(BYTE *,ULONG,BYTE *);
static ULONG yye2str(BYTE *,ULONG,BYTE *);

HB_FUNC(HB_YYENCODE)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG srclen,dstlen;
	BYTE *srcstr,*dststr;
	if ((hb_pcount()<2) || ((yy_len=hb_parnl(2))==0)) yy_len=128;
	if (phbstr) {
		srcstr=(BYTE *)hb_itemGetCPtr(phbstr);
		srclen=hb_itemGetCLen(phbstr);
		dstlen=str2yye(srcstr,srclen,NULL);
		dststr=(BYTE *) hb_xgrab(dstlen);
		str2yye(srcstr,srclen,dststr);
		hb_retclen((char *) dststr,dstlen);
		hb_xfree(dststr);
	}
	else hb_retc("");
}

HB_FUNC(HB_YYDECODE)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG srclen,dstlen;
	BYTE *srcstr,*dststr;
	if (phbstr) {
		srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
		srclen=hb_itemGetCLen(phbstr);
		dstlen=yye2str(srcstr,srclen,NULL);
		dststr=(BYTE *) hb_xgrab(dstlen);
		yye2str(srcstr,srclen,dststr);
		hb_retclen((char*) dststr,dstlen);
		hb_xfree(dststr);
	}
	else hb_retc("");
}

static ULONG str2yye(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
	ULONG i,x,dstlen=0,l=0;
	for (i=0;i<srclen;i++) {
		l++;
		x=0xFF & (0x2A + (ULONG) srcstr[i]);
		if (strchr((char *) yy_tomask,x)) {
			if (dststr) {
				dststr[dstlen++]='=';
				if ((l++)%yy_len==0) {
					l=1;
					if (OS_EOL_LEN==2) dststr[dstlen++]='\r';
					dststr[dstlen++]='\n';
				}
				dststr[dstlen++]=(BYTE) (x+0x40);
			}
			else {
				if ((l++)%yy_len==0) {
					l=1;
					dstlen+=OS_EOL_LEN;
				}
				dstlen+=2;
			}
		}
		else {
			if (dststr) dststr[dstlen++]=(BYTE) x;
			else dstlen++;
		}
		if (l%yy_len==0) {
			l=0;
			if (dststr) {
				if (OS_EOL_LEN==2) dststr[dstlen++]='\r';
				dststr[dstlen++]='\n';
			}
			else dstlen+=OS_EOL_LEN;
		}
	}
	return dstlen;
}

static ULONG yye2str(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
	ULONG i,dstlen=0;
	int s=0;
	for (i=0;i<srclen;i++) {
		if (strchr((char *) yy_tomask,srcstr[i])) {
			if (srcstr[i]=='=') s=1;
			continue;
		}
		if ((s==1) && strchr((char *) yy_tomask,srcstr[i]-0x40)==NULL) break;
		if (dststr) dststr[dstlen++]=(BYTE) (0xFF&((int) srcstr[i]+0xD6-s*0x40));
		else dstlen++;
		s=0;
	}
	return dstlen;
}
