/*
 * $Id: hbcu.c,v 1.6 2004/02/24 14:15:39 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_UUENCODE()
 *    HB_UUDECODE()
 *    HB_UUDECODE_FILE()
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

/*UUencode support*/

/*
HB_UUENCODE(string) -> uu_string
      Encodes string to UUencode (mail/news used)
   Parameters:
      string  - source character string
   Returns:
      UUencoded string

HB_UUDECODE(uu_string) -> string
      Decodes string from UUencode
   Parameters:
      uu_string  - UUencode encoded string
   Returns:
      decoded string

UUDECODE_FILE( <cFileInput>, [<cFileOutput>] ) -> int
   Description:
      UUDecode a given file
   Parameters:
      cFileInput = string, source filename to be decoded
                   OR
                   array, an array of file chunks arranged in proper order
      cFileOutput = output filename
   Returns:
      Upon succesful decoding the function returns numnber of bytes written
*/

#include "hbapi.h"
#include "hbapiitm.h"
#define UU_STR_LEN 60
#define UE_STR_LEN 45

static BYTE *eolchars=(BYTE *) "\r\n";

static ULONG int_uuenc(BYTE *,ULONG,BYTE *);
static ULONG int_uudec(BYTE *,ULONG,BYTE *);
static BYTE int_uubyte(BYTE);
static BYTE int_uubval(BYTE);
extern BOOL hbcc_file_read ( FILE *, char * );

HB_FUNC( UUDECODE_FILE )
{
   PHB_ITEM pinFile = hb_param( 1, HB_IT_ANY );
   PHB_ITEM poutFile = hb_param( 2, HB_IT_STRING );
   FILE *inFile, *outFile;
   char *string, *szFileName;
   ULONG srclen, dstlen, nBytesWritten = 0;
   BYTE *dststr;
   HB_ITEM Struct, Item;
   USHORT uiLen = 1, uiCount;
   BOOL bOutFile = FALSE;

   if( pinFile )
   {
      if ( ISCHAR( 1 ) )
      {
         if ( strlen( pinFile->item.asString.value ) == 0 )
         {
            hb_retni( 0 );
            return;
         }
         else
         {
            Struct.type = HB_IT_NIL;
            Item.type = HB_IT_NIL;
            hb_arrayNew( &Struct, 1 );
            hb_arraySet( &Struct, 1, hb_itemPutC( &Item, pinFile->item.asString.value ) );
            hb_itemClear( &Item );
         }
      }
      else if ( ISARRAY( 1 ) )
      {
         Struct.type = HB_IT_NIL;
         hb_itemCopy( &Struct, hb_param( 1, HB_IT_ARRAY ));
         uiLen = (USHORT) Struct.item.asArray.value->ulLen;

         if ( uiLen <= 0 )
         {
            hb_itemClear( &Struct );
            hb_retni( 0 );
            return;
         }
      }
      else
      {
         hb_retni( 0 );
         return;
      }
   }
   else
   {
      hb_retni( 0 );
      return;
   }

   if ( poutFile )
   {
      if ( strlen(poutFile->item.asString.value) == 0 )
      {
         hb_itemClear( &Struct );
         hb_retni(0);
         return;
      }
   }

   string = (char*) hb_xgrab( SHRT_MAX );

   for ( uiCount = 0; uiCount < uiLen; uiCount++ )
   {
      szFileName = hb_arrayGetC( &Struct, uiCount + 1 );

      if ( !szFileName )
      {
         hb_itemClear( &Struct );
         hb_xfree( string );
         hb_retni( 0 );
         return;
      }

      if ( strlen( szFileName ) == 0 )
      {
         hb_itemClear( &Struct );
         hb_xfree( szFileName );
         hb_xfree( string );
         hb_retni( 0 );
         return;
      }

      inFile = fopen( szFileName, "rb" );

      if ( !inFile )
      {
         hb_itemClear( &Struct );
         hb_xfree( szFileName );
         hb_xfree( string );
         hb_retni( 0 );
         return;
      }

      while ( hbcc_file_read ( inFile, string ) )
      {
         if ( string )
         {
            if ( !bOutFile )
            {
               if ( poutFile )
               {
                  if ( strstr ( string ,"begin 6" ) != NULL )
                  {
                     outFile = fopen( poutFile->item.asString.value, "wb" );

                     if ( !outFile )
                     {
                        break;
                     }

                     bOutFile = TRUE;
                     continue;
                  }
               }
               else
               {

                  if ( strstr ( string ,"begin 6" ) != NULL )
                  {
                     char *szFile ;
                     szFile = string + 10;

                     if( szFile )
                     {
                        outFile = fopen( szFile, "wb" );

                        if ( outFile )
                        {
                           bOutFile = TRUE;
                           continue;
                        }
                     }
                  }
               }
            } // end if ( !bOutFile )

            srclen = strlen( string );
            dstlen = int_uudec((BYTE*) string,srclen,NULL);
            if ( dstlen )
            {
               dststr = (BYTE *) hb_xgrab(dstlen);
               int_uudec((BYTE*) string,srclen,dststr);

               if ( bOutFile )
               {
                  nBytesWritten += fwrite( dststr, sizeof(BYTE), dstlen, outFile );
               }

               hb_xfree(dststr);
            }
         }
      }

      fclose( inFile );

      if ( szFileName )
      {
         hb_xfree( szFileName );
      }
   }

   hb_retnl( nBytesWritten );

   hb_xfree( string );

   fclose( outFile );

   hb_itemClear( &Struct );
}

HB_FUNC(HB_UUENCODE)
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   ULONG srclen,dstlen;
   BYTE *srcstr,*dststr;

   if (phbstr)
   {
      srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      dstlen=int_uuenc(srcstr,srclen,NULL);
      dststr=(BYTE *) hb_xgrab(dstlen);
      int_uuenc(srcstr,srclen,dststr);
      hb_retclenAdoptRaw((char *) dststr,dstlen);
   }
   else
   {
      hb_retc("");
   }
}

HB_FUNC(HB_UUDECODE)
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   ULONG srclen,dstlen;
   BYTE *srcstr,*dststr;

   if (phbstr)
   {
      srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      dstlen=int_uudec(srcstr,srclen,NULL);
      dststr=(BYTE *) hb_xgrab(dstlen);
      int_uudec(srcstr,srclen,dststr);
      hb_retclenAdoptRaw((char*) dststr,dstlen);
   }
   else
   {
      hb_retc("");
   }
}

static BYTE int_uubyte(BYTE c)
{
   BYTE x;

   x=c+' ';

   if (x==' ')
   {
      return '\140';
   }
   else if (x>'\140')
   {
      return '\177';
   }
   else
   {
      return x;
   }
}

static BYTE int_uubval(BYTE c)
{
   BYTE x;

   x=c-' ';

   if (x=='\100')
   {
      return '\0';
   }
   else
   {
      return x;
   }
}

static ULONG int_uuenc(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
   ULONG dstlen=0,i=0;

   while (i<srclen)
   {
      if (i%UE_STR_LEN==0)
      {
         if (dststr)
         {
            if (i)
            {
               //if (OS_EOL_LEN-1)
               //{
               //   dststr[dstlen++]='\r';
               //}

               dststr[dstlen++]='\n';
            }

            if ((srclen-i)>UE_STR_LEN)
            {
               dststr[dstlen++]=int_uubyte(UE_STR_LEN);
            }
            else
            {
               dststr[dstlen++]=int_uubyte((BYTE)(srclen-i));
            }
         }
         else
         {
            dstlen+=1+(i?OS_EOL_LEN:0);
         }
      }

      if (dststr)
      {
         dststr[dstlen++]=int_uubyte((srcstr[i]&0xFC)>>2);
      }
      else
      {
         dstlen++;
      }

      if (++i==srclen)
      {
         if (dststr)
         {
            dststr[dstlen++]=int_uubyte((srcstr[i-1]&0x03)<<4);
            dststr[dstlen++]=int_uubyte(0);
            dststr[dstlen++]=int_uubyte(0);
         }
         else
         {
            dstlen+=3;
         }

         break;
      }

      if (dststr)
      {
         dststr[dstlen++]=int_uubyte(((srcstr[i-1]&0x03)<<4)|((srcstr[i]&0xF0)>>4));
      }
      else
      {
         dstlen++;
      }

      if (++i==srclen)
      {
         if (dststr)
         {
            dststr[dstlen++]=int_uubyte((srcstr[i-1]&0x0F)<<2);
            dststr[dstlen++]=int_uubyte(0);
         }
         else
         {
            dstlen+=2;
         }

         break;
      }

      if (dststr)
      {
         dststr[dstlen++]=int_uubyte(((srcstr[i-1]&0x0F)<<2)|((srcstr[i]&0xC0)>>6));
         dststr[dstlen++]=int_uubyte(srcstr[i]&0x3F);
      }
      else
      {
         dstlen+=2;
      }

      if (++i==srclen)
      {
         break;
      }
   }

   if (dststr)
   {
      //if (OS_EOL_LEN-1)
      //{
      //   dststr[dstlen++]='\r';
      //}

      dststr[dstlen++]='\n';
      dststr[dstlen++]=int_uubyte('\0');

      //if (OS_EOL_LEN-1)
      //{
      //   dststr[dstlen++]='\r';
      //}

      dststr[dstlen++]='\n';
   }
   else
   {
      dstlen+=2*OS_EOL_LEN+1;
   }

   return dstlen;
}

static ULONG int_uudec(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
   ULONG dstlen=0,i=0;
   BYTE j,l,tmp[4];

   while (i<srclen)
   {
      l=int_uubval(srcstr[i++]);

      if (l=='\0')
      {
         break;
      }

      j=0;

      while (1)
      {
         tmp[0]=int_uubval(srcstr[i++]);

         if (tmp[0]>'\077')
         {
            break;
         }

         tmp[1]=int_uubval(srcstr[i++]);

         if (tmp[1]>'\077')
         {
            break;
         }

         if (dststr)
         {
            dststr[dstlen++]=(tmp[0]<<2)|((tmp[1]&'\060')>>4);
         }
         else
         {
            dstlen++;
         }

         if (++j==l)
         {
            break;
         }

         tmp[2]=int_uubval(srcstr[i++]);

         if (tmp[2]>'\077')
         {
            break;
         }

         if (dststr)
         {
            dststr[dstlen++]=((tmp[1]&'\017')<<4)|((tmp[2]&'\074')>>2);
         }
         else
         {
            dstlen++;
         }

         if (++j==l)
         {
            break;
         }

         tmp[3]=int_uubval(srcstr[i++]);

         if (tmp[3]>'\077')
         {
            break;
         }

         if (dststr)
         {
            dststr[dstlen++]=((tmp[2]&'\003')<<6)|(tmp[3]);
         }
         else
         {
            dstlen++;
         }

         if (++j==l)
         {
            break;
         }
      }

      if (j==l)
      {
         while ((i<srclen)&&(strchr((char*)eolchars,srcstr[i])==NULL))
         {
            i++;
         }

         while ((i<srclen)&&(strchr((char*)eolchars,srcstr[i])!=NULL))
         {
            i++;
         }

         if (i==srclen)
         {
            break;
         }
      }
      else
      {
         break;
      }
   }

   return dstlen;
}
