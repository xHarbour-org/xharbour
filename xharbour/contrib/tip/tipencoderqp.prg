/**********************************************
* tIPEncoderBase64.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipencoderbase64.prg,v 1.1 2003/11/30 14:41:50 jonnymind Exp $
************************************************/
#include "hbclass.ch"

#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapierr.h"
#include "hbstack.h"
#enddump

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
   char *cData = hb_parc(1);
   int nLen = hb_parclen(1);
   char *cRet;
   char cElem;
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
   }

   // Preallocating maximum possible length
   cRet = (char *) hb_xgrab( nLen * 3 + ( nLen/72 ) *3 + 3 );
   // last +3 is trailing \r\n\0
   while ( nPos < nLen )
   {
      cElem = cData[ nPos ];

      // We chose not to encode spaces and tab here.
      // cElem is signed and ranges from -126 to +127.
      // negative values are automatically encoded
      if ( (cElem >=33 && cElem <= 60) || cElem >= 62 ||
         cElem == 9 || cElem == 32 )
      {
         cRet[nPosRet++] = cElem;
         iLineLen++;
      }
      else
      {
         cRet[nPosRet++] = '=';
         nVal = ((unsigned char)cElem) >> 4;
         cRet[nPosRet++] = (char) (nVal < 10 ? '0' + nVal : 'A' + nVal - 10);
         nVal = ((unsigned char)cElem) & 0x0f;
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
   cElem = cRet[nPosRet - 1];
   if ( cElem == 9 || cElem == 32 )
   {
      cRet[nPosRet++] = '=';
   }
   /* Adding canonical new line for RFC2045 blocks */
   cRet[nPosRet++] = '\r';
   cRet[nPosRet++] = '\n';

   /* this function also adds a zero */
   cRet = (char *) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclenAdopt( cRet, nPosRet );
}
#pragma ENDDUMP


#pragma BEGINDUMP
HB_FUNC( TIPENCODERQP_DECODE )
{
   char *cData = hb_parc(1);
   int nLen = hb_parclen(1);
   char *cRet;
   int nPos = 0, nPosRet = 0, nVal;
   char cElem, cCipher;

   if ( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         "TIPENCODERQP_DECODE", 1, hb_paramError(1) );
      return;
   }

   if ( ! nLen )
   {
      hb_retc( "" );
   }


   // allocate maximum possible lenght.
   cRet = (char *) hb_xgrab( nLen + 1 );

   while ( nPos < nLen )
   {
      cElem = cData[ nPos ];

      if ( cElem == '=' )
      {
         if ( nPos < nLen - 2 )
         {
            cCipher = cData[ ++nPos ];
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

               cCipher = cData[ ++nPos ];
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
         cRet[ nPosRet++ ] = cElem;
      }

      nPos ++;
   }

   /* this function also adds a zero */
   /* hopefully reduce the size of cRet */
   cRet = (char *) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclenAdopt( cRet, nPosRet );
}
#pragma ENDDUMP

