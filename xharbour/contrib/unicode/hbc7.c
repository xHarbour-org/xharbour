/*
 * $Id: hbc7.c,v 1.1 2004/01/14 06:14:03 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_URLENCODE()
 *    HB_QPENCODE()
 *    HB_URLDECODE()
 *    HB_QPDECODE()
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
HB_URLENCODE(string) -> ue_string
      Encodes string to URLencode (%XX)
      "HTTP unsafe" characters are encoded to "%XX",
      where XX-hexadecimal character code.
   Parameters:
      string  - source character string
   Returns:
      encoded string

HB_QPENCODE(string) -> qp_string
      Encodes string to Quoted-Printable (=XX)
      "Mail unsafe" characters are encoded to "=XX",
      where XX-hexadecimal character code.
   Parameters:
      string  - source character string
   Returns:
      encoded string

HB_URLDECODE(ue_string) -> string
      Decodes string from URLencode
   Parameters:
      ue_string  - URLencode encoded string
   Returns:
      decoded string

HB_QPDECODE(qp_string) -> string
      Decodes string from Quoted-Printable
   Parameters:
      qp_string  - Quoted-Printable encoded string
   Returns:
      decoded string
*/

#include "hbapi.h"
#include "hbapiitm.h"

#define CHR_ENC_UE  '%'
#define CHR_ENC_QP  '='
#define CHR_ENC_DEC '#'
#define CHR_ENC_OCT '\\'
#define MAX_QPLINE 72
#define hexdigits (uechars+51)
#define decdigits (uechars+57)
#define octdigits (uechars+59)

static BYTE *uechars=(BYTE *) "*-.@_ZYXWVUTSRQPONMLKJIHGFEDCBAzyxwvutsrqponmlkjihgfedcba9876543210";
//static BYTE *decdigits=hexdigits+6;
//static BYTE *octdigits=hexdigits+8;

static ULONG str2ue7(BYTE *,ULONG,BYTE *);
static ULONG str2qp7(BYTE *,ULONG,BYTE *);
static ULONG ue72str(BYTE *,ULONG,BYTE *);
static ULONG qp72str(BYTE *,ULONG,BYTE *);
static BOOL isdigit(BYTE,ULONG);

HB_FUNC(HB_URLENCODE)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG srclen,dstlen;
	BYTE *srcstr,*dststr;
	if (phbstr) {
		srcstr=(BYTE*) hb_itemGetCPtr(phbstr);
		srclen=hb_itemGetCLen(phbstr);
		dstlen=str2ue7(srcstr,srclen,NULL);
		dststr=(BYTE *) hb_xgrab(dstlen+1);
		str2ue7(srcstr,srclen,dststr);
		hb_retclen((char *) dststr,dstlen);
		hb_xfree(dststr);
	}
	else hb_retc("");
	return;
}

HB_FUNC(HB_QPENCODE)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG srclen,dstlen;
	BYTE *srcstr,*dststr;
	if (phbstr) {
		srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
		srclen=hb_itemGetCLen(phbstr);
		dstlen=str2qp7(srcstr,srclen,NULL);
		dststr=(BYTE *) hb_xgrab(dstlen+1);
		str2qp7(srcstr,srclen,dststr);
		hb_retclen((char *) dststr,dstlen);
		hb_xfree(dststr);
	}
	else hb_retc("");
	return;
}

HB_FUNC(HB_URLDECODE)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG srclen,dstlen;
	BYTE *srcstr,*dststr;
	if (phbstr) {
		srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
		srclen=hb_itemGetCLen(phbstr);
		dstlen=ue72str(srcstr,srclen,NULL);
		dststr=(BYTE *) hb_xgrab(dstlen);
		ue72str(srcstr,srclen,dststr);
		hb_retclen((char*) dststr,dstlen);
		hb_xfree(dststr);
	}
	else hb_retc("");
	return;
}

HB_FUNC(HB_QPDECODE)
{
	PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
	ULONG srclen,dstlen;
	BYTE *srcstr,*dststr;
	if (phbstr) {
		srcstr=(BYTE*) hb_itemGetCPtr(phbstr);
		srclen=hb_itemGetCLen(phbstr);
		dstlen=qp72str(srcstr,srclen,NULL);
		dststr=(BYTE *) hb_xgrab(dstlen);
		qp72str(srcstr,srclen,dststr);
		hb_retclen((char* )dststr,dstlen);
		hb_xfree(dststr);
	}
	else hb_retc("");
	return;
}

//internal
static BOOL isdigit(BYTE c,ULONG r)
{
	switch (r) {
		case 8:
			return (strchr((char *) octdigits,(c|'\040'))!=NULL);
		case 10:
			return (strchr((char *) decdigits,(c|'\040'))!=NULL);
		case 16:
			return (strchr((char *) hexdigits,(c|'\040'))!=NULL);
		default:
			return FALSE;
	}
}

ULONG str2ue7(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
	ULONG i,dstlen=0;
	for (i=0;i<srclen;i++) {
		if (srcstr[i]=='\0') {
			if (dststr) dststr[dstlen++]='&';
			else dstlen++;
		}
		else if (srcstr[i]==' ') {
			if (dststr) dststr[dstlen++]='+';
			else dstlen++;
		}
		else if (strchr((char *) uechars,srcstr[i])!=NULL) {
			if (dststr) dststr[dstlen++]=srcstr[i];
			else dstlen++;
		}
		else {
			if (dststr) {
				dststr[dstlen]=CHR_ENC_UE;
				sprintf((char*) dststr+dstlen+1,"%02X",srcstr[i]);
			}
			dstlen+=3;
		}
	}
	return dstlen;
}

static ULONG str2qp7(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
	ULONG i,n=0,dstlen=0;
	for (i=0;i<srclen;i++) {
		if (n>=MAX_QPLINE) {
			if (dststr) {
				dststr[dstlen++]=CHR_ENC_QP;
				dststr[dstlen++]='\r';
				dststr[dstlen++]='\n';
			}
			else dstlen+=3;
			n=0;
		}
		if ((srcstr[i]=='\r')&&(srcstr[i+1]=='\n')) {
			n=0;
			if (dststr) {
				dststr[dstlen++]='\r';
				dststr[dstlen++]='\n';
			}
			else dstlen+=2;
			i++;
		}
		else if ((srcstr[i]=='\t')||((srcstr[i]>='\040')&&(srcstr[i]!=CHR_ENC_QP)&&(srcstr[i]<=(BYTE) '\176'))) {
			n++;
			if (dststr) dststr[dstlen++]=srcstr[i];
			else dstlen++;
		}
		else {
			n+=3;
			if (dststr) {
				dststr[dstlen]=CHR_ENC_QP;
				sprintf((char *) dststr+dstlen+1,"%02X",srcstr[i]);
			}
			dstlen+=3;
		}
	}
	return dstlen;
}

static ULONG ue72str(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
	ULONG i,j,dstlen=0;
	for (i=0;i<srclen;i++) {
		if (srcstr[i]==CHR_ENC_UE) {
			if (isdigit(srcstr[i+1],16)&&isdigit(srcstr[i+2],16)) {
				if (dststr) {
					sscanf((char *) srcstr+i+1,"%02X",&j);
					dststr[dstlen++]=(BYTE) j;
				}
				else dstlen++;
				i+=2;
			}
			else break;
		}
		else if (srcstr[i]=='+') {
			if (dststr) dststr[dstlen++]=' ';
			else dstlen++;
		}
		else if (srcstr[i]=='&') {
			if (dststr) dststr[dstlen++]='\0';
			else dstlen++;
		}
		else if (strchr((char*) uechars,srcstr[i])!=NULL) {
			if (dststr) dststr[dstlen++]=srcstr[i];
			else dstlen++;
		}
		else break;
	}
	return dstlen;
}

static ULONG qp72str(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
	ULONG i,j,dstlen=0;
	for (i=0;i<srclen;i++) {
		if (srcstr[i]==CHR_ENC_QP) {
			if ((srcstr[i+1]=='\r')&&(srcstr[i+2]=='\n')) {
				i+=2;
				continue;
			}
			else if (isdigit(srcstr[i+1],16)&&isdigit(srcstr[i+2],16)) {
				if (dststr) {
					sscanf((char*)srcstr+i+1,"%02X",&j);
					dststr[dstlen++]=(BYTE) j;
				}
				else dstlen++;
				i+=2;
			}
			else {
				if (dststr) dststr[dstlen++]=srcstr[i];
				else dstlen++;
			}
		}
		else {
			if (dststr) dststr[dstlen++]=srcstr[i];
			else dstlen++;
		}
	}
	return dstlen;
}


