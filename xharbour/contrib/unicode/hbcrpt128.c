/*
 * $Id: hbcrpt128.c,v 1.4 2004/02/24 14:15:39 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    MKCRYPTKEY()
 *    ENCRYPT128()
 *    DECRYPT128()
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
MKCRYPTKEY(cPassPhrase)
     Prepares internal encryption key (128 bit)
   Parameters:
     cPassPhrase - encryption key phrase
   Returns
     NIL

ENCRYPT128(string) -> enc_string
     Encrypts a string (128 bit)
   User must execute a MKCRYPTKEY() call prior
   to prepare a desired key
   Parameters:
     string - string to encrypt
   Returns
     Encrypted byte sequence (size changes)

DECRYPT128(enc_string) -> string
     Decrypts a string (128 bit)
   User must execute a MKCRYPTKEY() call prior
   to prepare a desired key
   Parameters:
     enc_string - encrypted byte sequence
   Returns
     decrypted string
*/

#include "hbapi.h"
#include "hbapiitm.h"

static void int_encrypt(BYTE *,ULONG);
static void int_decrypt(BYTE *,ULONG);
ULONG hbcc_crc32(BYTE *,ULONG,ULONG);

static ULONG tbl[512];
// static ULONG tmpKey[4];

HB_FUNC(MKCRYPTKEY) //(cPass) -> ckey(32)
{
    PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
    int i,j,k,l,m,n,srcLen;
    ULONG tCRC[4]={0xA965569Al,0x569AA965l,0x5A96A569l,0x69A55A96l};
    BYTE *srcStr;
//  BYTE *dstStr;

    if (phbstr)
    {
       srcStr=(BYTE *)hb_itemGetCPtr(phbstr);
       srcLen=hb_itemGetCLen(phbstr);
    }
    else
    {
       srcStr=(BYTE*)"";
       srcLen=0;
    }

    for(i=0;i<srcLen;i++)
       tCRC[i%4]=hbcc_crc32(srcStr+i,1,tCRC[i%4]);

//  dstStr=(BYTE *)hb_xgrab(288);
//  for (i=0;i<4;i++) {
//       for (j=0;j<8;j++) {
//               k=(i<<3)+j;
//               dstStr[k]=(BYTE) (tCRC[3-i]>>((~(j<<2))&0x1E))&0xFF;
//       }
//  }

    for(i=0;i<4;i++)
    {
       for (j=0;j<4;j++)
       {
          for (k=0;k<4;k++)
          {
             l=(tCRC[i]>>((j<<3)|(k<<1)))&3;
             for (n=0;n<4;n++)
             {
                m=(n+l)&3;
                tbl[(n<<6)+(i<<4)+(j<<2)+k]=(BYTE) ((m&2)? 264-((i<<5)+((j&2)<<3)+(m<<3)): (i<<5)+((j&2)<<3)+(m<<3))+((m%3)? 7-(((j&1)<<2)+k): ((j&1)<<2)+k);
             }
          }
       }
    }

//  for (i=0;i<256;i++) dstStr[32+i]=tbl[i];
//  hb_retclen(dstStr,288);
//  hb_xfree(dstStr);

}

HB_FUNC(ENCRYPT128) //(cStr)->cEnc
{
    PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
    ULONG srclen,dstlen,crc,i;
    BYTE *srcstr,*dststr;

    if (phbstr)
    {
       srcstr=(BYTE*) hb_itemGetCPtr(phbstr);
       srclen=hb_itemGetCLen(phbstr);
       dstlen=32*((srclen+39)/32);
       crc=hbcc_crc32(srcstr,srclen,0l);
       dststr=(BYTE *) hb_xgrab(dstlen);
       dststr[0]=(BYTE) ((srclen>>24)&0xFF);
       dststr[1]=(BYTE) ((srclen>>16)&0xFF);
       dststr[2]=(BYTE) ((srclen>>8)&0xFF);
       dststr[3]=(BYTE) (srclen&0xFF);
       dststr[4]=(BYTE) ((crc>>24)&0xFF);
       dststr[5]=(BYTE) ((crc>>16)&0xFF);
       dststr[6]=(BYTE) ((crc>>8)&0xFF);
       dststr[7]=(BYTE) (crc&0xFF);
       memcpy(dststr+8,srcstr,srclen);

       for (i=0;i<dstlen-8-srclen;i++)
          dststr[srclen+8+i]=dststr[i+8];

       int_encrypt(dststr,dstlen);
       hb_retclenAdoptRaw((char *) dststr,dstlen);
    }
    else
    {
       hb_retc("");
    }
}

HB_FUNC(DECRYPT128) //(cEnc)->cStr
{
    PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
    ULONG srclen,dstlen,crc,i;
    BYTE *srcstr,*dststr;

    if (phbstr)
    {
       srcstr=(BYTE *)hb_itemGetCPtr(phbstr);
       srclen=hb_itemGetCLen(phbstr);
       dststr=(BYTE *) hb_xgrab(srclen);
       memcpy(dststr,srcstr,srclen);
       int_decrypt(dststr,srclen);
       dstlen=(((ULONG) dststr[0])<<24) | (((ULONG) dststr[1])<<16) | (((ULONG) dststr[2])<<8) | ((ULONG) dststr[3]);

       if (srclen<(8*((dstlen+15)/8)))
       {
          hb_xfree(dststr);
          hb_retc("");
          return;
       }

       crc=(((ULONG) dststr[4])<<24) | (((ULONG) dststr[5])<<16) | (((ULONG) dststr[6])<<8) | ((ULONG) dststr[7]);

       for (i=0;i<dstlen;i++)
          dststr[i]=dststr[i+8];

       dststr=(BYTE *) hb_xrealloc(dststr,dstlen);

       if (crc!=hbcc_crc32(dststr,dstlen,0l))
       {
          hb_xfree(dststr);
          hb_retc("");
          return;
       }
       hb_retclenAdoptRaw((char *) dststr,dstlen);
    }
    else
    {
       hb_retc("");
    }
}

static void int_encrypt(BYTE *src,ULONG len)
{
    ULONG n=0;
    BYTE tmp[32],i,j,k;

    while (n<len)
    {
       for (i=0;i<32;i++)
          tmp[i]='\0';

       for (i=0;i<32;i++)
       {
          for (j=0;j<8;j++)
          {
             k=(BYTE)tbl[(i<<3)|j];
             tmp[k>>3] |= (src[n+i]&('\001'<<j)) ? ('\001'<<(k&'\007')) : '\0';
          }
       }

       for (i=0;i<32;i++)
          src[n++]=tmp[i];
    }
}

static void int_decrypt(BYTE *src,ULONG len)
{
    ULONG n=0;
    BYTE tmp[32],i,j,k;

    while (n<len)
    {
       for (i=0;i<32;i++)
          tmp[i]='\0';

       for (i=0;i<32;i++)
       {
          for (j=0;j<8;j++)
          {
             k=(BYTE)tbl[(i<<3)|j];
             tmp[i] |= (('\001'<<(k&7))&src[n+(k>>3)]) ? ('\001'<<j) : '\0';
          }
       }

       for (i=0;i<32;i++)
          src[n++]=tmp[i];
    }

    return;
}
