/*
 * $Id: hbcrpt32.c,v 1.1 2004/01/14 06:14:03 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_ENCRYPT32()
 *    HB_DECRYPT32()
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
HB_ENCRYPT32(string,pass) -> enc_string
      Encrypts a string
   Parameters:
      string - string variable to encrypt
      pass   - string variable to calculate encryption key
   Returns:
      Encrypted byte sequence (size changes)

HB_DECRYPT32(enc_string,pass) -> string
      Decrypts a string
   Parameters:
      enc_string - encrypted byte sequence
      pass       - string variable to calculate encryption key
   Returns:
      Decrypted string
*/

#include "hbapi.h"
#include "hbapiitm.h"

static BYTE tbl[64];

static void int_encrypt(BYTE *,ULONG);
static void int_decrypt(BYTE *,ULONG);
static void int_cryptbl(BYTE *,ULONG);
ULONG hbcc_crc32(BYTE *,ULONG,ULONG);

HB_FUNC(HB_ENCRYPT32) //(cStr)->cEnc
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   PHB_ITEM phbpsw=hb_param(2,HB_IT_STRING);
   ULONG srclen,dstlen,crc;
   BYTE *srcstr,*dststr;

   if (phbpsw)
   {
      int_cryptbl((BYTE*) hb_itemGetCPtr(phbpsw),hb_itemGetCLen(phbpsw));
   }
   else
   {
      int_cryptbl((BYTE*)"",0);
   }

   if (phbstr)
   {
      srcstr=(BYTE*) hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      dstlen=8*((srclen+15)/8);
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
      int_encrypt(dststr,dstlen);
      hb_retclen((char *) dststr,dstlen);
      hb_xfree(dststr);
   }
   else
   {
      hb_retc("");
   }
}

HB_FUNC(HB_DECRYPT32) //(cEnc)->cStr
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   PHB_ITEM phbpsw=hb_param(2,HB_IT_STRING);
   ULONG srclen,dstlen,crc,i;
   BYTE *srcstr,*dststr;

   if (phbpsw)
   {
      int_cryptbl((BYTE*)hb_itemGetCPtr(phbpsw),hb_itemGetCLen(phbpsw));
   }
   else
   {
      int_cryptbl((BYTE*)"",0);
   }

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
      {
         dststr[i]=dststr[i+8];
      }

      dststr=(BYTE *) hb_xrealloc(dststr,dstlen);

      if (crc!=hbcc_crc32(dststr,dstlen,0l))
      {
         hb_xfree(dststr);
         hb_retc("");
         return;
      }

      hb_retclen((char *) dststr,dstlen);
      hb_xfree(dststr);
   }
   else
   {
      hb_retc("");
   }
}

static void int_encrypt(BYTE *src,ULONG len)
{
   ULONG n=0;
   BYTE tmp[8],i,j,k;

   while (n<len)
   {
      for (i=0;i<8;i++)
      {
         tmp[i]='\0';
      }

      for (i=0;i<8;i++)
      {
         for (j=0;j<8;j++)
         {
            k=tbl[(i<<3)|j];
            tmp[k>>3] |= (src[n+i]&('\001'<<j)) ? ('\001'<<(k&'\007')) : '\0';
         }
      }

      for (i=0;i<8;i++)
      {
         src[n++]=tmp[i];
      }
   }
}

static void int_decrypt(BYTE *src,ULONG len)
{
   ULONG n=0;
   BYTE tmp[8],i,j,k;

   while (n<len)
   {
      for (i=0;i<8;i++)
      {
         tmp[i]='\0';
      }

      for (i=0;i<8;i++)
      {
         for (j=0;j<8;j++)
         {
            k=tbl[(i<<3)|j];
            tmp[i] |= (('\001'<<(k&7))&src[n+(k>>3)]) ? ('\001'<<j) : '\0';
         }
      }

      for (i=0;i<8;i++)
      {
         src[n++]=tmp[i];
      }
   }

   return;
}

static void int_cryptbl(BYTE *psw,ULONG len)
{
   ULONG key;
   BYTE i,j,k,l,n=0;
   BYTE tmp[8];

   key=hbcc_crc32(psw,len,0x3C4B5A69l);

   for (i=0;i<4;i++)
   {
      for (l=0;l<8;l++)
      {
         tmp[l]='\0';
      }

      for (j=0;j<4;j++)
      {
         for (k=0;k<4;k++)
         {
            l='\003'&(i+((unsigned char) (key>>(((j<<2)|k)<<1))));

            switch(l)
            {
            case 0:
               tmp[j]|='\001'<<k;
               break;

            case 1:
               tmp[k]|='\001'<<(7-j);
               break;

            case 2:
               tmp[7-j]|='\001'<<(7-k);
               break;

            case 3:
               tmp[7-k]|='\001'<<(j);
               break;
            }
         }
      }

      for (j=0;j<8;j++)
      {
         for (k=0;k<8;k++)
         {
            if (tmp[j]&('\001'<<k))
            {
               tbl[n++]=(j<<3)|k;
            }
         }
      }
   }
}
