/*
 * $Id: hbcrc32.c,v 1.1 2004/01/14 06:14:03 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_CRC32()
 *    HB_NCRC32()
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
HB_CRC32(string) -> Crc32
      Calculates CCITT Crc32 (32-bit Cyclic redundancy checksum)
   Parameters:
      string  - string variable to calculate CRC32
   Returns:
      Crc32 as a 4-byte string variable (high byte first)

HB_NCRC32(string,intermed32) -> nCrc32
      Calculates CCITT Crc32 (32-bit Cyclic redundancy checksum)
   Parameters:
      string     - string variable to calculate CRC32
      intermed32 - unnecessary parameter to start from default 0
                   as an integer (0-4294967295) if current string
                   is a fragment to calculate CRC32 for entire file
                   [HB_NCRC32(c1+c2,0)==HB_NCRC32(c2,HB_NCRC32(c1,0))]
   Returns:
      nCrc32 as an integer (0-4294967295)
*/

#include "hbapi.h"
#include "hbapiitm.h"

#define CRC32INIT      (0xFFFFFFFFL)

static ULONG crc32tbl[256]={
0x00000000l,0x77073096l,0xEE0E612Cl,0x990951BAl,0x076DC419l,0x706AF48Fl,0xE963A535l,0x9E6495A3l,
0x0EDB8832l,0x79DCB8A4l,0xE0D5E91El,0x97D2D988l,0x09B64C2Bl,0x7EB17CBDl,0xE7B82D07l,0x90BF1D91l,
0x1DB71064l,0x6AB020F2l,0xF3B97148l,0x84BE41DEl,0x1ADAD47Dl,0x6DDDE4EBl,0xF4D4B551l,0x83D385C7l,
0x136C9856l,0x646BA8C0l,0xFD62F97Al,0x8A65C9ECl,0x14015C4Fl,0x63066CD9l,0xFA0F3D63l,0x8D080DF5l,
0x3B6E20C8l,0x4C69105El,0xD56041E4l,0xA2677172l,0x3C03E4D1l,0x4B04D447l,0xD20D85FDl,0xA50AB56Bl,
0x35B5A8FAl,0x42B2986Cl,0xDBBBC9D6l,0xACBCF940l,0x32D86CE3l,0x45DF5C75l,0xDCD60DCFl,0xABD13D59l,
0x26D930ACl,0x51DE003Al,0xC8D75180l,0xBFD06116l,0x21B4F4B5l,0x56B3C423l,0xCFBA9599l,0xB8BDA50Fl,
0x2802B89El,0x5F058808l,0xC60CD9B2l,0xB10BE924l,0x2F6F7C87l,0x58684C11l,0xC1611DABl,0xB6662D3Dl,
0x76DC4190l,0x01DB7106l,0x98D220BCl,0xEFD5102Al,0x71B18589l,0x06B6B51Fl,0x9FBFE4A5l,0xE8B8D433l,
0x7807C9A2l,0x0F00F934l,0x9609A88El,0xE10E9818l,0x7F6A0DBBl,0x086D3D2Dl,0x91646C97l,0xE6635C01l,
0x6B6B51F4l,0x1C6C6162l,0x856530D8l,0xF262004El,0x6C0695EDl,0x1B01A57Bl,0x8208F4C1l,0xF50FC457l,
0x65B0D9C6l,0x12B7E950l,0x8BBEB8EAl,0xFCB9887Cl,0x62DD1DDFl,0x15DA2D49l,0x8CD37CF3l,0xFBD44C65l,
0x4DB26158l,0x3AB551CEl,0xA3BC0074l,0xD4BB30E2l,0x4ADFA541l,0x3DD895D7l,0xA4D1C46Dl,0xD3D6F4FBl,
0x4369E96Al,0x346ED9FCl,0xAD678846l,0xDA60B8D0l,0x44042D73l,0x33031DE5l,0xAA0A4C5Fl,0xDD0D7CC9l,
0x5005713Cl,0x270241AAl,0xBE0B1010l,0xC90C2086l,0x5768B525l,0x206F85B3l,0xB966D409l,0xCE61E49Fl,
0x5EDEF90El,0x29D9C998l,0xB0D09822l,0xC7D7A8B4l,0x59B33D17l,0x2EB40D81l,0xB7BD5C3Bl,0xC0BA6CADl,
0xEDB88320l,0x9ABFB3B6l,0x03B6E20Cl,0x74B1D29Al,0xEAD54739l,0x9DD277AFl,0x04DB2615l,0x73DC1683l,
0xE3630B12l,0x94643B84l,0x0D6D6A3El,0x7A6A5AA8l,0xE40ECF0Bl,0x9309FF9Dl,0x0A00AE27l,0x7D079EB1l,
0xF00F9344l,0x8708A3D2l,0x1E01F268l,0x6906C2FEl,0xF762575Dl,0x806567CBl,0x196C3671l,0x6E6B06E7l,
0xFED41B76l,0x89D32BE0l,0x10DA7A5Al,0x67DD4ACCl,0xF9B9DF6Fl,0x8EBEEFF9l,0x17B7BE43l,0x60B08ED5l,
0xD6D6A3E8l,0xA1D1937El,0x38D8C2C4l,0x4FDFF252l,0xD1BB67F1l,0xA6BC5767l,0x3FB506DDl,0x48B2364Bl,
0xD80D2BDAl,0xAF0A1B4Cl,0x36034AF6l,0x41047A60l,0xDF60EFC3l,0xA867DF55l,0x316E8EEFl,0x4669BE79l,
0xCB61B38Cl,0xBC66831Al,0x256FD2A0l,0x5268E236l,0xCC0C7795l,0xBB0B4703l,0x220216B9l,0x5505262Fl,
0xC5BA3BBEl,0xB2BD0B28l,0x2BB45A92l,0x5CB36A04l,0xC2D7FFA7l,0xB5D0CF31l,0x2CD99E8Bl,0x5BDEAE1Dl,
0x9B64C2B0l,0xEC63F226l,0x756AA39Cl,0x026D930Al,0x9C0906A9l,0xEB0E363Fl,0x72076785l,0x05005713l,
0x95BF4A82l,0xE2B87A14l,0x7BB12BAEl,0x0CB61B38l,0x92D28E9Bl,0xE5D5BE0Dl,0x7CDCEFB7l,0x0BDBDF21l,
0x86D3D2D4l,0xF1D4E242l,0x68DDB3F8l,0x1FDA836El,0x81BE16CDl,0xF6B9265Bl,0x6FB077E1l,0x18B74777l,
0x88085AE6l,0xFF0F6A70l,0x66063BCAl,0x11010B5Cl,0x8F659EFFl,0xF862AE69l,0x616BFFD3l,0x166CCF45l,
0xA00AE278l,0xD70DD2EEl,0x4E048354l,0x3903B3C2l,0xA7672661l,0xD06016F7l,0x4969474Dl,0x3E6E77DBl,
0xAED16A4Al,0xD9D65ADCl,0x40DF0B66l,0x37D83BF0l,0xA9BCAE53l,0xDEBB9EC5l,0x47B2CF7Fl,0x30B5FFE9l,
0xBDBDF21Cl,0xCABAC28Al,0x53B39330l,0x24B4A3A6l,0xBAD03605l,0xCDD70693l,0x54DE5729l,0x23D967BFl,
0xB3667A2El,0xC4614AB8l,0x5D681B02l,0x2A6F2B94l,0xB40BBE37l,0xC30C8EA1l,0x5A05DF1Bl,0x2D02EF8Dl};

ULONG hbcc_crc32(BYTE *buf,ULONG len,ULONG crc)
{
   ULONG i=0;

   crc^=CRC32INIT;

   while( i<len )
   {
      crc=crc32tbl[(crc^buf[i++])&0xff]^(crc>>8);
   }

   return CRC32INIT^crc;
}

HB_FUNC(HB_CRC32)
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   ULONG srclen,crc;
   BYTE *srcstr;
   BYTE *dststr=(BYTE*) "\0\0\0\0";

   if (phbstr)
   {
      srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      crc=hbcc_crc32(srcstr,srclen,0l);
      dststr[0]=(BYTE )(0xFF&(crc>>24));
      dststr[1]=(BYTE )(0xFF&(crc>>16));
      dststr[2]=(BYTE )(0xFF&(crc>>8));
      dststr[3]=(BYTE )(0xFF&crc);
   }

   hb_retclen((char *) dststr,4);
}

HB_FUNC(HB_NCRC32)
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   ULONG srclen,crc=0;
   BYTE *srcstr;

   if (phbstr)
   {
      if (hb_pcount()>1)
      {
         crc=hb_parnl(2);
      }

      srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      crc=hbcc_crc32(srcstr,srclen,crc);
   }

   hb_retnl(crc);
}
