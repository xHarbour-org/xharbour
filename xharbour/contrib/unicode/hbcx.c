/*
 * $Id: hbcx.c,v 1.1 2004/01/14 13:59:52 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_XXENCODE()
 *    HB_XXDECODE()
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

/*XXencode support*/

/*
HB_XXENCODE(string) -> xx_string
      Encodes string to XXencode (some mail/news software used)
   Parameters:
      string  - source character string
   Returns:
      XXencoded string

HB_XXDECODE(xx_string) -> string
      Decodes string from XXencode
   Parameters:
      xx_string  - XXencode encoded string
   Returns:
      decoded string
*/

#include "hbapi.h"
#include "hbapiitm.h"
#define UU_STR_LEN 60
#define UE_STR_LEN 45

static BYTE *eolchars=(BYTE*) "\r\n";
static BYTE *xxechars=(BYTE*) "+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

static ULONG int_xxenc(BYTE *,ULONG,BYTE *);
static ULONG int_xxdec(BYTE *,ULONG,BYTE *);
static BYTE int_xxbyte(BYTE);
static BYTE int_xxbval(BYTE);

HB_FUNC(HB_XXENCODE)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG srclen,dstlen;
	BYTE *srcstr,*dststr;
	if (phbstr) {
		srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
		srclen=hb_itemGetCLen(phbstr);
		dstlen=int_xxenc(srcstr,srclen,NULL);
		dststr=(BYTE *) hb_xgrab(dstlen);
		int_xxenc(srcstr,srclen,dststr);
		hb_retclen((char *) dststr,dstlen);
		hb_xfree(dststr);
	}
	else hb_retc("");
}

HB_FUNC(HB_XXDECODE)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG srclen,dstlen;
	BYTE *srcstr,*dststr;
	if (phbstr) {
		srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
		srclen=hb_itemGetCLen(phbstr);
		dstlen=int_xxdec(srcstr,srclen,NULL);
		dststr=(BYTE *) hb_xgrab(dstlen);
		int_xxdec(srcstr,srclen,dststr);
		hb_retclen((char*) dststr,dstlen);
		hb_xfree(dststr);
	}
	else hb_retc("");
}

static BYTE int_xxbyte(BYTE c)
{
	if (c<'\100') return xxechars[c];
	else return '\177';
}

static BYTE int_xxbval(BYTE c)
{
	BYTE *x;
	x=(BYTE *) strchr((char *) xxechars,c);
	if ((c=='\0') || (x==NULL)) return '\177';
	else return (BYTE) (x-xxechars);
}

static ULONG int_xxenc(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
	ULONG dstlen=0,i=0;
	while (i<srclen) {
		if (i%UE_STR_LEN==0) {
			if (dststr) {
				if (i) {
					if (OS_EOL_LEN-1) dststr[dstlen++]='\r';
					dststr[dstlen++]='\n';
				}
				if ((srclen-i)>UE_STR_LEN) dststr[dstlen++]=int_xxbyte(UE_STR_LEN);
				else dststr[dstlen++]=int_xxbyte((BYTE)(srclen-i));
			}
			else dstlen+=1+(i?OS_EOL_LEN:0);
		}
		if (dststr) dststr[dstlen++]=int_xxbyte((srcstr[i]&0xFC)>>2);
		else dstlen++;
		if (++i==srclen) {
			if (dststr) {
				dststr[dstlen++]=int_xxbyte((srcstr[i-1]&0x03)<<4);
				dststr[dstlen++]=int_xxbyte(0);
				dststr[dstlen++]=int_xxbyte(0);
			}
			else dstlen+=3;
			break;
		}
		if (dststr) dststr[dstlen++]=int_xxbyte(((srcstr[i-1]&0x03)<<4)|((srcstr[i]&0xF0)>>4));
		else dstlen++;
		if (++i==srclen) {
			if (dststr) {
				dststr[dstlen++]=int_xxbyte((srcstr[i-1]&0x0F)<<2);
				dststr[dstlen++]=int_xxbyte(0);
			}
			else dstlen+=2;
			break;
		}
		if (dststr) {
			dststr[dstlen++]=int_xxbyte(((srcstr[i-1]&0x0F)<<2)|((srcstr[i]&0xC0)>>6));
			dststr[dstlen++]=int_xxbyte(srcstr[i]&0x3F);
		}
		else dstlen+=2;
		if (++i==srclen) break;
	}
	if (dststr) {
		if (OS_EOL_LEN-1) dststr[dstlen++]='\r';
		dststr[dstlen++]='\n';
		dststr[dstlen++]=int_xxbyte('\0');
		if (OS_EOL_LEN-1) dststr[dstlen++]='\r';
		dststr[dstlen++]='\n';
	}
	else dstlen+=2*OS_EOL_LEN+1;
	return dstlen;
}

static ULONG int_xxdec(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
	ULONG dstlen=0,i=0;
	BYTE j,l,tmp[4];
	while (i<srclen) {
		l=int_xxbval(srcstr[i++]);
		if (l=='\0') break;
		j=0;
		while (1) {
			tmp[0]=int_xxbval(srcstr[i++]);
			if (tmp[0]>'\077') break;
			tmp[1]=int_xxbval(srcstr[i++]);
			if (tmp[1]>'\077') break;
			if (dststr) dststr[dstlen++]=(tmp[0]<<2)|((tmp[1]&'\060')>>4);
			else dstlen++;
			if (++j==l) break;
			tmp[2]=int_xxbval(srcstr[i++]);
			if (tmp[2]>'\077') break;
			if (dststr) dststr[dstlen++]=((tmp[1]&'\017')<<4)|((tmp[2]&'\074')>>2);
			else dstlen++;
			if (++j==l) break;
			tmp[3]=int_xxbval(srcstr[i++]);
			if (tmp[3]>'\077') break;
			if (dststr) dststr[dstlen++]=((tmp[2]&'\003')<<6)|(tmp[3]);
			else dstlen++;
			if (++j==l) break;
		}
		if (j==l) {
			while ((i<srclen)&&(strchr((char*) eolchars,srcstr[i])==NULL)) i++;
			while ((i<srclen)&&(strchr((char*) eolchars,srcstr[i])!=NULL)) i++;
			if (i==srclen) break;
		}
		else break;
	}
	return dstlen;
}
