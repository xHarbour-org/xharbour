/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
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

#include "hbclass.ch"

#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapierr.h"
#include "hbstack.h"
#pragma ENDDUMP

CLASS TIPEncoderQP FROM TIPEncoder

   METHOD New()      Constructor
   METHOD Encode( cData )
   METHOD Decode( cData )

ENDCLASS

METHOD New() CLASS TIPEncoderQP

   ::cName := "Quoted-Printable"

   RETURN Self



#pragma BEGINDUMP

HB_FUNC( TIPENCODERQP_ENCODE )
{
   const char *cData = hb_parc(1);
   int nLen = (int) hb_parclen(1);
   char *cRet;
   unsigned char cElem;
   int nVal, iLineLen = 0;
   int nPosRet = 0, nPos = 0;

   if ( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         "TIPENCODEQP_ENCODE", 1, hb_paramError(1) );
      return;
   }

   if ( ! nLen )
   {
      hb_retc( "" );
      return;
   }

   // Preallocating maximum possible length
   cRet = (char *) hb_xgrab( nLen * 3 + ( nLen/72 ) *3 + 3 );
   // last +3 is trailing \r\n\0
   while ( nPos < nLen )
   {
      cElem = (unsigned char) cData[ nPos ];

      // We chose not to encode spaces and tab here.
      // cElem is signed and ranges from -126 to +127.
      // negative values are automatically encoded
      if ( (cElem >=33 && cElem <= 60) || cElem >= 62 ||
         cElem == 9 || cElem == 32 )
      {
         cRet[nPosRet++] = (char) cElem;
         iLineLen++;
      }
      else
      {
         cRet[nPosRet++] = '=';
         nVal = cElem >> 4;
         cRet[nPosRet++] = (char) (nVal < 10 ? '0' + nVal : 'A' + nVal - 10);
         nVal = cElem & 0x0f;
         cRet[nPosRet++] = (char) (nVal < 10 ? '0' + nVal : 'A' + nVal - 10);
         iLineLen+=3;
      }

      nPos++;

      if ( iLineLen >= 72 )
      {
         cRet[nPosRet++] = '=';
         cRet[nPosRet++] = '\r';
         cRet[nPosRet++] = '\n';
         iLineLen = 0;
      }
   }

   /* Securing last line trailing space, if needed */
   cElem = (unsigned char) cRet[nPosRet - 1];
   if ( cElem == 9 || cElem == 32 )
   {
      cRet[nPosRet++] = '=';
      cRet[nPosRet++] = '\r';
      cRet[nPosRet++] = '\n';
   }
   /* Adding canonical new line for RFC2045 blocks */

   /* this function also adds a zero */
   cRet = (char *) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclenAdopt( cRet, nPosRet );
}

#pragma ENDDUMP


#pragma BEGINDUMP
HB_FUNC( TIPENCODERQP_DECODE )
{
   const char *cData = hb_parc(1);
   int nLen = (int) hb_parclen(1);
   char *cRet;
   int nPos = 0, nPosRet = 0, nVal;
   unsigned char cElem, cCipher;

   if ( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         "TIPENCODERQP_DECODE", 1, hb_paramError(1) );
      return;
   }

   if ( ! nLen )
   {
      hb_retc( "" );
      return;
   }


   // allocate maximum possible lenght.
   cRet = (char *) hb_xgrab( nLen + 1 );

   while ( nPos < nLen )
   {
      cElem = (unsigned char) cData[ nPos ];

      if ( cElem == '=' )
      {
         if ( nPos < nLen - 2 )
         {
            cCipher = (unsigned char) cData[ ++nPos ];
            //soft line break
            if ( cCipher == '\r' )
            {
               nPos += 2;
               continue;
            }
            else {

               nVal = cCipher >= 'A' && cCipher <= 'F' ? cCipher - 'A' + 10 :
                     cCipher - '0';
               nVal *= 16;

               cCipher = (unsigned char) cData[ ++nPos ];
               nVal += cCipher >= 'A' && cCipher <= 'F' ? cCipher - 'A' + 10 :
                     cCipher - '0';

               cRet[ nPosRet++ ] = (char) nVal;
            }
         }
         // else the encoding is malformed
         else
         {
            if (nPosRet > 0 )
            {
               break;
            }
         }
      }
      else
      {
         cRet[ nPosRet++ ] = (char) cElem;
      }

      nPos ++;
   }

   /* this function also adds a zero */
   /* hopefully reduce the size of cRet */
   cRet = (char *) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclenAdopt( cRet, nPosRet );
}

#pragma ENDDUMP

