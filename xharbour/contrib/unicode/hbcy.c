/*
 * $Id: hbcy.c,v 1.6 2004/02/21 14:39:01 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_YYENCODE()
 *    HB_YYDECODE()
 *    HB_YYDECODE_FILE()
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
extern BOOL hbcc_file_read ( FILE *, char * );

HB_FUNC( YYDECODE_FILE )
{
   PHB_ITEM pinFile = hb_param( 1, HB_IT_ANY );
   PHB_ITEM poutFile = hb_param( 2, HB_IT_STRING );
   FILE *inFile, *outFile;
   char *string, *szFileName;
   ULONG srclen, dstlen, nBytesWritten = 0;
   BYTE *dststr;
   HB_ITEM pStruct, pItem;
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
            pStruct.type = HB_IT_NIL;
            pItem.type = HB_IT_NIL;
            hb_arrayNew( &pStruct, 1 );
            hb_arraySet( &pStruct, 1, hb_itemPutC( &pItem, pinFile->item.asString.value ) );
         }
      }
      else if ( ISARRAY( 1 ) )
      {
         pStruct = (*hb_param( 1, HB_IT_ARRAY ));
         uiLen = (USHORT) pStruct.item.asArray.value->ulLen;

         if ( uiLen <= 0 )
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
         hb_retni(0);
         return;
      }
   }

   string = (char*) hb_xgrab( SHRT_MAX );

   for ( uiCount = 0; uiCount < uiLen; uiCount++ )
   {
      szFileName = hb_arrayGetC( &pStruct, uiCount + 1 );

      if ( !szFileName )
      {
         hb_xfree( string );
         hb_retni( 0 );
         return;
      }

      if ( strlen( szFileName ) == 0 )
      {
         hb_xfree( szFileName );
         hb_xfree( string );
         hb_retni( 0 );
         return;
      }

      inFile = fopen( szFileName, "rb" );

      if ( !inFile )
      {
         hb_xfree( szFileName );
         hb_xfree( string );
         hb_retni( 0 );
         return;
      }

      while ( hbcc_file_read ( inFile, string ) )
      {
         if ( string )
         {
            srclen = strlen( string );
            dstlen = yye2str((BYTE*) string,srclen,NULL);
            if ( dstlen )
            {
               dststr = (BYTE *) hb_xgrab(dstlen);
               yye2str((BYTE*) string,srclen,dststr);

               if ( bOutFile )
               {
                  nBytesWritten += fwrite( dststr, sizeof(BYTE), dstlen, outFile );
               }

               hb_xfree(dststr);
            }
            else
            {
              /* file name always at the first line */
              /* substring 10 */
              if ( !bOutFile )
              {
                 if ( poutFile )
                 {
                    if ( strstr ( string ,"=ybegin" ) != NULL )
                    {
                       outFile = fopen( poutFile->item.asString.value, "wb" );

                       if ( !outFile )
                       {
                          break;
                       }

                       bOutFile = TRUE;
                    }
                 }
                 else
                 {
                    char *szFile ;
                    int ulHeader;
                    int n_At;

                    if ( strstr ( string ,"=ybegin" ) != NULL )
                    {
                       ulHeader = strlen( string );

                       n_At = hb_strAt( "name=", 5, string, ulHeader );

                       szFile = string + n_At + 4 ;

                       // printf( "szFile=%s\n",szFile);

                       if( szFile )
                       {
                          outFile = fopen( szFile, "wb" );

                          if ( outFile )
                          {
                             bOutFile = TRUE;
                          }
                       }
                    }
                 }
              }
            }
         }
      }

      fclose( inFile );

      if ( szFileName )
         hb_xfree( szFileName );
   }

   hb_retnl( nBytesWritten );

   hb_xfree( string );

   fclose( outFile );
}

HB_FUNC(HB_YYENCODE)
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   ULONG srclen,dstlen;
   BYTE *srcstr,*dststr;

   if ((hb_pcount()<2) || ((yy_len=hb_parnl(2))==0))
   {
      yy_len=128;
   }

   if (phbstr)
   {
      srcstr=(BYTE *)hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      dstlen=str2yye(srcstr,srclen,NULL);
      dststr=(BYTE *) hb_xgrab(dstlen);
      str2yye(srcstr,srclen,dststr);
      hb_retclenAdoptRaw((char *) dststr,dstlen);
   }
   else
   {
      hb_retc("");
   }
}

HB_FUNC(HB_YYDECODE)
{
   PHB_ITEM phbstr=hb_param(1,HB_IT_STRING);
   ULONG srclen,dstlen;
   BYTE *srcstr,*dststr;

   if (phbstr)
   {
      srcstr=(BYTE *) hb_itemGetCPtr(phbstr);
      srclen=hb_itemGetCLen(phbstr);
      dstlen=yye2str(srcstr,srclen,NULL);
      dststr=(BYTE *) hb_xgrab(dstlen);
      yye2str(srcstr,srclen,dststr);
      hb_retclenAdoptRaw((char*) dststr,dstlen);
   }
   else
      hb_retc("");
}

static ULONG str2yye(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
   ULONG i,x,dstlen=0,l=0;

   for (i=0;i<srclen;i++)
   {
      l++;
      x=0xFF & (0x2A + (ULONG) srcstr[i]);

      if (strchr((char *) yy_tomask,x))
      {
         if (dststr)
         {
            dststr[dstlen++]='=';

            if ((l++)%yy_len==0)
            {
               l=1;

               // if (OS_EOL_LEN==2)
                  // dststr[dstlen++]='\r';

               dststr[dstlen++]='\n';
            }

            dststr[dstlen++]=(BYTE) (x+0x40);
         }
         else
         {
            if ((l++)%yy_len==0)
            {
               l=1;
               dstlen+=OS_EOL_LEN;
            }
            dstlen+=2;
         }
      }
      else
      {
         if (dststr)
            dststr[dstlen++]=(BYTE) x;
         else
            dstlen++;
      }

      if (l%yy_len==0)
      {
         l=0;

         if (dststr)
         {
            // if (OS_EOL_LEN==2)
            //   dststr[dstlen++]='\r';

            dststr[dstlen++]='\n';
         }
         else
            dstlen+=OS_EOL_LEN;
      }
   }
   return dstlen;
}

static ULONG yye2str(BYTE *srcstr,ULONG srclen,BYTE *dststr)
{
   ULONG i,dstlen=0;
   int s=0;

   for (i=0;i<srclen;i++)
   {
      if (strchr((char *) yy_tomask,srcstr[i]))
      {
         if (srcstr[i]=='=')
            s=1;

         continue;
      }

      if ((s==1) && strchr((char *) yy_tomask,srcstr[i]-0x40)==NULL)
         break;

      if (dststr)
         dststr[dstlen++]=(BYTE) (0xFF&((int) srcstr[i]+0xD6-s*0x40));
      else
         dstlen++;

      s=0;
   }

   return dstlen;
}
