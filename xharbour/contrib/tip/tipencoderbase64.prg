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

CLASS TIPEncoderBase64 FROM TIPEncoder
   // Set this to .T. to enable RFC 2068 (HTTP/1.1) exception to
   // RFC 2045 (MIME) base64 format. This exception consists in
   // not applying CRLF after each 76 output bytes.
   DATA bHttpExcept

   METHOD New()      Constructor
   METHOD Encode( cData )
   METHOD Decode( cData )
ENDCLASS

METHOD New() CLASS TIPEncoderBase64
   ::cName := "Base64"
   ::bHttpExcept := .F.
RETURN Self



#pragma BEGINDUMP

HB_FUNC( TIPENCODERBASE64_ENCODE )
{
   char *cData = hb_parc(1);
   char *cRet;
   int nLen = hb_parclen(1);
   int nPos = 0, nPosRet = 0;
   int nPosBlock = 0, nLineCount = 0;
   ULONG nFinalLen;
   char cElem, cElem1;
   BOOL bExcept;

   if ( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         "TIPENCODERBASE64_ENCODE", 1, hb_paramError(1) );
      return;
   }

   if ( ! nLen )
   {
      hb_retc( "" );
   }

   // read the status of bHttpExcept
   if ( hb_pcount() > 1 )
   {
      // this makes this function static!!!!
      bExcept = hb_parl(2);
   }
   else
   {
      bExcept = hb_itemGetL( hb_objSendMsg( hb_stackSelfItem(), "BHTTPEXCEPT",0));
   }
   // we know exactly the renturned length.
   nFinalLen = (ULONG) ((nLen / 3 + 2) * 4);
   // add line termination padding, CRLF each 76 output bytes
   nFinalLen += (nFinalLen / 72 +1) * 2;
   cRet = (char *) hb_xgrab( nFinalLen );

   while ( nPos < nLen )
   {
      cElem = cData[ nPos ];
      // NOT using trailing 0 here as some buggy 3dparty func
      // will create strings without trailing 0.

      nPosBlock++;

      switch( nPosBlock )
      {
         case 1:
            cElem = cElem >> 2;
            break;
         case 2:
            cElem1 = nPos < nLen -1 ? cData[ nPos + 1] : 0;
            cElem = ((cElem & 0x3) << 4) | cElem1 >> 4;
            nPos++;
            break;
         case 3:
            cElem1 = nPos < nLen -1 ? cData[ nPos + 1] : 0;
            cElem = ((cElem & 0xF) << 2) | cElem1 >> 6;
            nPos++;
            break;
         case 4:
            cElem = cElem & 0x3f;
            nPos++;
            nPosBlock = 0;
            break;
      }

      if ( cElem < 26 )
      {
         cRet[nPosRet++] = cElem + 'A';
      }
      else if ( cElem < 52 )
      {
         cRet[nPosRet++] = ( cElem - 26 ) + 'a';
      }
      else if ( cElem < 62 )
      {
         cRet[nPosRet++] = ( cElem - 52 ) + '0';
      }
      else if ( cElem == 62 )
      {
         cRet[nPosRet++] = '+';
      }
      else
      {
         cRet[nPosRet++] = '/';
      }

      if ( ! bExcept )
      {
         nLineCount ++ ;
         /* RFC says to add a CRLF each 76 chars, but is pretty unclear about
            the fact of this 76 chars counting CRLF or not. Common
            practice is to limit line size to 72 chars */
         if ( nLineCount == 72 )
         {
            cRet[nPosRet++] = '\r';
            cRet[nPosRet++] = '\n';
            nLineCount = 0;
         }
      }
   }

   switch( nPos % 3 )
   {
      case 1:
         cRet[ nPosRet++ ] = '=';
         /* fallthrough */
      case 2:
         cRet[ nPosRet++ ] = '=';
         /* fallthrough */
   }

   /* RFC is not explicit, but CRLF SHOULD be added at bottom
      during encoding phase */
   if ( ! bExcept )
   {
      cRet[nPosRet++] = '\r';
      cRet[nPosRet++] = '\n';
   }

   /* this function also adds a zero */
   hb_retclenAdopt( cRet, nPosRet );
}
#pragma ENDDUMP


#pragma BEGINDUMP
HB_FUNC( TIPENCODERBASE64_DECODE )
{
   char *cData = hb_parc(1);
   char *cRet;
   int nLen = hb_parclen(1);
   int nPos = 0, nPosRet = 0, nPosBlock = 0;
   char cElem;

   if ( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         "TIPENCODERBASE64_DECODE", 1, hb_paramError(1) );
      return;
   }

   if ( ! nLen )
   {
      hb_retc( "" );
   }


   // we know exactly the renturned length.
   cRet = (char *) hb_xgrab( (nLen / 4 + 1) * 3 );

   while ( nPos < nLen )
   {
      cElem = cData[ nPos ];

      if ( cElem >= 'A' && cElem <= 'Z' )
      {
         cElem -= 'A';
      }
      else if ( cElem >= 'a' && cElem <= 'z' )
      {
         cElem = cElem - 'a' + 26;
      }
      else if ( cElem >= '0' && cElem <= '9' )
      {
         cElem = cElem - '0' + 52;
      }
      else if ( cElem == '+' )
      {
         cElem = 62;
      }
      else if ( cElem == '/' )
      {
         cElem = 63;
      }
      // end of stream?
      else if ( cElem == '=' )
      {
         break;
      }
      // RFC 2045 specifies characters not in base64 must be ignored
      else
      {
         nPos++;
         continue;
      }

      switch( nPosBlock )
      {
         case 0:
            cRet[nPosRet]  = cElem << 2;
            nPosBlock++;
            break;
         case 1:
            // higer bits are zeros
            cRet[nPosRet] |= cElem >> 4;
            nPosRet++;
            cRet[nPosRet]  = cElem << 4;
            nPosBlock++;
            break;
         case 2:
            // higer bits are zeros
            cRet[nPosRet] |= cElem >> 2;
            nPosRet++;
            cRet[nPosRet]  = cElem << 6;
            nPosBlock++;
            break;
         case 3:
            cRet[nPosRet] |= cElem;
            nPosRet++;
            nPosBlock = 0;
            break;
      }

      nPos++;
   }

   /* this function also adds a zero */
   /* hopefully reduce the size of cRet */
   cRet = (char *) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclenAdopt( cRet, nPosRet );
}
#pragma ENDDUMP

