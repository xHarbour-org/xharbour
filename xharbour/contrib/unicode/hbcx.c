/*
 * $Id: hbcx.c,v 1.5 2004/02/24 14:15:39 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_XXENCODE()
 *    HB_XXDECODE()
 *    HB_XXDECODE_FILE()
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
HB_XXENCODE(string) -> string
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

XXDECODE_FILE( <cFileInput>, [<cFileOutput>] ) -> int
   Description:
      XXDecode a given
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
#include "hbapifs.h"
#define UU_STR_LEN 60
#define UE_STR_LEN 45

static BYTE *eolchars=(BYTE*) "\r\n";
static BYTE *xxechars=(BYTE*) "+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static ULONG int_xxenc(BYTE *,ULONG,BYTE *);
static ULONG int_xxdec(BYTE *,ULONG,BYTE *);
static BYTE int_xxbyte(BYTE);
static BYTE int_xxbval(BYTE);
extern BYTE *hbcc_getfilename ( BYTE *strFullPath );
extern BOOL hbcc_file_read ( FILE *, char * );

HB_FUNC( XXDECODE_FILE )
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
            if ( strstr ( string ,"end" ) != NULL )
            {
               continue;
            }

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
                  else
                  {
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
                  else
                  {
                     continue;
                  }
               }
            } // end if ( !bOutFile )

            srclen = strlen( string );
            dstlen = int_xxdec((BYTE*) string,srclen,NULL);
            if ( dstlen )
            {
               dststr = (BYTE *) hb_xgrab(dstlen);
               int_xxdec((BYTE*) string,srclen,dststr);

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

HB_FUNC ( XXENCODE_FILE_BY_CHUNK )
{
   PHB_ITEM pInFile  = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pLine = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pOutFile = hb_param( 3, HB_IT_STRING );
   PHB_FNAME pFileName = NULL;
   ULONG nSize,i=0, ulLine, ulDecoded = 0;
   FILE *infile, *OutFile;
   int c1, c2, c3;
   int iPart = 1;
   char *szFileName, *cMask;
   char szDestFile[ _POSIX_PATH_MAX ] ;

   if ( !pInFile )
   {
      hb_retni( -1 );
      return;
   }

   szFileName = (char*) hbcc_getfilename( (BYTE*) pInFile->item.asString.value );

   if ( strlen( pInFile->item.asString.value ) == 0 || strlen ( szFileName ) == 0 )
   {
      hb_retni( -1 );
      return;
   }

   infile = fopen( pInFile->item.asString.value, "rb" );

   if ( !infile )
   {
      hb_retni( -1 );
      return;
   }

   if( ! pLine )
   {
      fclose( infile );
      hb_retni( -4 );
      return;
   }
   else
   {
      ulLine = pLine->item.asLong.value;
      if ( ulLine <= 0 )
      {
         fclose( infile );
         hb_retni( -4 );
         return;
      }
   }

   fseek( infile, 0L, SEEK_END );
   nSize = ftell( infile );
   fseek( infile, 0L, SEEK_SET );

   if ( pOutFile )
   {
      if ( strlen ( pOutFile->item.asString.value ) == 0 )
      {
         fclose( infile );
         hb_retni( -2 );
         return;
      }
      cMask = pOutFile->item.asString.value;
   }
   else
   {
      pFileName = hb_fsFNameSplit(pInFile->item.asString.value);
      cMask = pFileName->szName;
   }

   sprintf( szDestFile, "%s%02d%s", cMask, iPart, ".xxe" );

   OutFile = fopen( szDestFile, "wb" );

   if ( !OutFile )
   {
      if ( pFileName != NULL )
      {
         hb_xfree( pFileName );
      }
      hb_retni( -3 );
      fclose( infile );
      return ;
   }

   fprintf( OutFile, "begin 644 %s\n", szFileName );

   while ((c1 = getc(infile)) != EOF)
   {
      if ( i % UE_STR_LEN == 0 )
      {
         if ( i )
         {
            putc( '\n', OutFile );
         }

         if ( ++ ulDecoded == ulLine )
         {
            iPart ++;
            ulDecoded = 0;
            fclose( OutFile );
            sprintf( szDestFile, "%s%02d%s", cMask, iPart, ".xxe" );
            OutFile = fopen( szDestFile, "wb" );
            if ( (nSize-i) > UE_STR_LEN )
            {
               putc(int_xxbyte(UE_STR_LEN),OutFile);
            }
            else
            {
               putc(int_xxbyte((BYTE)(nSize-i)),OutFile);
            }
         }
         else
         {
            if ( (nSize-i) > UE_STR_LEN )
            {
               putc(int_xxbyte(UE_STR_LEN),OutFile);
            }
            else
            {
               putc(int_xxbyte((BYTE)(nSize-i)),OutFile);
            }
         }
      }

      putc( int_xxbyte((c1&0xFC)>>2), OutFile );

      ++ i;
      c2 = getc( infile );

      if ( c2 == EOF )
      {
         putc( int_xxbyte((c1&0x03)<<4),OutFile);
         putc( int_xxbyte(0),OutFile);
         putc( int_xxbyte(0),OutFile);
         break;
      }

      putc( int_xxbyte(((c1&0x03)<<4)|((c2&0xF0)>>4)),OutFile);

      ++ i;
      c3 = getc( infile );

      if ( c3 == EOF )
      {
         putc( int_xxbyte((c2&0x0F)<<2),OutFile);
         putc( int_xxbyte(0),OutFile);
         break;
      }

      putc(int_xxbyte(((c2&0x0F)<<2)|((c3&0xC0)>>6)),OutFile);
      putc(int_xxbyte(c3&0x3F),OutFile);

      ++ i;
   }

   putc( '\n', OutFile );
   putc(int_xxbyte('\0'),OutFile);
   putc( '\n', OutFile );

   fprintf( OutFile, "end\n" );

   fclose( infile );
   fclose( OutFile );

   hb_retni(0);

   if ( pFileName != NULL )
   {
      hb_xfree( pFileName );
   }
}

HB_FUNC ( XXENCODE_FILE )
{
   PHB_ITEM pInFile  = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pOutFile = hb_param( 2, HB_IT_STRING );
   PHB_FNAME pFileName ;
   ULONG nSize,i=0;
   FILE *infile, *OutFile;
   int c1, c2, c3;
   char *szFileName;
   char szDestFile[ _POSIX_PATH_MAX ] ;

   if ( !pInFile )
   {
      hb_retni( -1 );
      return;
   }

   szFileName = (char*) hbcc_getfilename( (BYTE*) pInFile->item.asString.value );

   if ( strlen ( szFileName ) == 0 )
   {
      hb_retni( -1 );
      return;
   }

   infile = fopen( pInFile->item.asString.value, "rb" );

   if ( !infile )
   {
      hb_retni( -3 );
      return;
   }

   fseek( infile, 0L, SEEK_END );
   nSize = ftell( infile );
   fseek( infile, 0L, SEEK_SET );

   if ( pOutFile )
   {
      if ( strlen ( pOutFile->item.asString.value ) == 0 )
      {
         hb_retni( -2 );
         return;
      }
      OutFile = fopen( pOutFile->item.asString.value, "wb" );
   }
   else
   {
      pFileName = hb_fsFNameSplit(pInFile->item.asString.value);
      pFileName->szExtension = ".yye";
      hb_fsFNameMerge( szDestFile, pFileName );
      OutFile = fopen( szDestFile, "wb" );
   }

   if ( !OutFile )
   {
      hb_retni( -4 );
      fclose( infile );
      return ;
   }

   fprintf( OutFile, "begin 644 %s\n", szFileName );

   while ((c1 = getc(infile)) != EOF)
   {
      if ( i % UE_STR_LEN == 0 )
      {
         if ( i )
         {
            putc( '\n', OutFile );
         }

         if ( (nSize-i) > UE_STR_LEN )
         {
            putc(int_xxbyte(UE_STR_LEN),OutFile);
         }
         else
         {
            putc(int_xxbyte((BYTE)(nSize-i)),OutFile);
         }
      }

      putc( int_xxbyte((c1&0xFC)>>2), OutFile );

      ++ i;
      c2 = getc( infile );

      if ( c2 == EOF )
      {
         putc( int_xxbyte((c1&0x03)<<4),OutFile);
         putc( int_xxbyte(0),OutFile);
         putc( int_xxbyte(0),OutFile);
         break;
      }

      putc( int_xxbyte(((c1&0x03)<<4)|((c2&0xF0)>>4)),OutFile);

      ++ i;
      c3 = getc( infile );

      if ( c3 == EOF )
      {
         putc( int_xxbyte((c2&0x0F)<<2),OutFile);
         putc( int_xxbyte(0),OutFile);
         break;
      }

      putc(int_xxbyte(((c2&0x0F)<<2)|((c3&0xC0)>>6)),OutFile);
      putc(int_xxbyte(c3&0x3F),OutFile);

      ++ i;
   }

   putc( '\n', OutFile );
   putc(int_xxbyte('\0'),OutFile);
   putc( '\n', OutFile );

   fprintf( OutFile, "end\n" );

   fclose( infile );
   fclose( OutFile );

   hb_retni(0);
}

HB_FUNC(HB_XXENCODE)
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   ULONG srclen,dstlen;
   BYTE *srcstr,*dststr;

   if (phbstr)
   {
      srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      dstlen=int_xxenc(srcstr,srclen,NULL);
      dststr=(BYTE *) hb_xgrab(dstlen);
      int_xxenc(srcstr,srclen,dststr);
      hb_retclenAdoptRaw((char *) dststr,dstlen);
   }
   else
   {
      hb_retc("");
   }
}

HB_FUNC(HB_XXDECODE)
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   ULONG srclen,dstlen;
   BYTE *srcstr,*dststr;

   if (phbstr)
   {
      srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      dstlen=int_xxdec(srcstr,srclen,NULL);
      dststr=(BYTE *) hb_xgrab(dstlen);
      int_xxdec(srcstr,srclen,dststr);
      hb_retclenAdoptRaw((char*) dststr,dstlen);
   }
   else
   {
      hb_retc("");
   }
}

static BYTE int_xxbyte(BYTE c)
{
   if (c<'\100')
   {
      return xxechars[c];
   }
   else
   {
      return '\177';
   }
}

static BYTE int_xxbval(BYTE c)
{
   BYTE *x;

   x=(BYTE *) strchr((char *) xxechars,c);

   if ((c=='\0') || (x==NULL))
   {
      return '\177';
   }
   else
   {
      return (BYTE) (x-xxechars);
   }
}

static ULONG int_xxenc(BYTE *srcstr,ULONG srclen,BYTE *dststr)
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
               dststr[dstlen++]=int_xxbyte(UE_STR_LEN);
            }
            else
            {
               dststr[dstlen++]=int_xxbyte((BYTE)(srclen-i));
            }
         }
         else
         {
            dstlen+=1+(i?OS_EOL_LEN:0);
         }
      }

      if (dststr)
      {
         dststr[dstlen++]=int_xxbyte((srcstr[i]&0xFC)>>2);
      }
      else
      {
         dstlen++;
      }

      if (++i==srclen)
      {
         if (dststr)
         {
            dststr[dstlen++]=int_xxbyte((srcstr[i-1]&0x03)<<4);
            dststr[dstlen++]=int_xxbyte(0);
            dststr[dstlen++]=int_xxbyte(0);
         }
         else
         {
            dstlen+=3;
         }

         break;
      }

      if (dststr)
      {
         dststr[dstlen++]=int_xxbyte(((srcstr[i-1]&0x03)<<4)|((srcstr[i]&0xF0)>>4));
      }
      else
      {
         dstlen++;
      }

      if (++i==srclen)
      {
         if (dststr)
         {
            dststr[dstlen++]=int_xxbyte((srcstr[i-1]&0x0F)<<2);
            dststr[dstlen++]=int_xxbyte(0);
         }
         else
         {
            dstlen+=2;
         }

         break;
      }

      if (dststr)
      {
         dststr[dstlen++]=int_xxbyte(((srcstr[i-1]&0x0F)<<2)|((srcstr[i]&0xC0)>>6));
         dststr[dstlen++]=int_xxbyte(srcstr[i]&0x3F);
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
      dststr[dstlen++]=int_xxbyte('\0');

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

static ULONG int_xxdec(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
   ULONG dstlen=0,i=0;
   BYTE j,l,tmp[4];

   while (i<srclen)
   {
      l=int_xxbval(srcstr[i++]);

      if (l=='\0')
      {
         break;
      }

      j=0;

      while (1)
      {
         tmp[0]=int_xxbval(srcstr[i++]);

         if (tmp[0]>'\077')
         {
            break;
         }

         tmp[1]=int_xxbval(srcstr[i++]);

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

         tmp[2]=int_xxbval(srcstr[i++]);

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

         tmp[3]=int_xxbval(srcstr[i++]);

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
         while ((i<srclen)&&(strchr((char*) eolchars,srcstr[i])==NULL))
         {
            i++;
         }

         while ((i<srclen)&&(strchr((char*) eolchars,srcstr[i])!=NULL))
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

