/**********************************************
* tIPEncoderURL.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipencoderurl.prg,v 1.3 2004/04/23 16:30:21 jonnymind Exp $
************************************************/
#include "hbclass.ch"

#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapierr.h"
#include "hbstack.h"
#enddump

CLASS TIPEncoderUrl FROM TIPEncoder
   METHOD New()   Constructor
   METHOD Encode()
   METHOD Decode()
ENDCLASS

METHOD New() CLASS TIPEncoderURL
   ::cName := "urlencoded"
RETURN Self


#pragma BEGINDUMP

HB_FUNC( TIPENCODERURL_ENCODE )
{
   char *cData = hb_parc(1);
   int nLen = hb_parclen(1);
   BOOL bComplete = hb_parl(2);
   char *cRet;
   int nPos = 0, nPosRet = 0, nVal;
   char cElem;
   
   if ( hb_pcount() < 2 )
   {
      bComplete = TRUE;
   }

   if ( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         "TIPENCODERBASE64_ENCODE", 1, hb_paramError(1) );
      return;
   }

   if ( ! nLen )
   {
      hb_retc( "" );
      return;
   }

   // Giving maximum final length possible
   cRet = (char *) hb_xgrab( nLen * 3 +1);

   while ( nPos < nLen )
   {
      cElem = cData[ nPos ];

      if ( cElem == ' ' )
      {
         cRet[ nPosRet ] = '+';
      }
      else if (
         (cElem >= 'A' && cElem <= 'Z') ||
         (cElem >= 'a' && cElem <= 'z') ||
         (cElem >= '0' && cElem <= '9') ||
         cElem == '.' || cElem == ',' || cElem == '&' ||
         cElem == '/' || cElem == ';' || cElem =='_' )
      {
         cRet[ nPosRet ] = cElem;
      }
      else if ( ! bComplete && ( cElem == ':' || cElem == '?' || cElem == '=' ) )
      {
         cRet[ nPosRet ] = cElem;
      }
      else // encode!
      {
         cRet[ nPosRet++] = '%';
         nVal = ((unsigned char) cElem) >> 4;
         cRet[ nPosRet++] = nVal < 10 ? '0' + nVal : 'A' + nVal - 10;
         nVal = ((unsigned char) cElem) & 0x0f;
         cRet[ nPosRet ] = nVal < 10 ? '0' + nVal : 'A' + nVal - 10;
      }

      nPosRet++;
      nPos++;
   }

   cRet[ nPosRet ] = 0;
   /* this function also adds a zero */
   hb_retclenAdoptRaw( cRet, nPosRet );
}
#pragma ENDDUMP


#pragma BEGINDUMP
HB_FUNC( TIPENCODERURL_DECODE )
{
   char *cData = hb_parc(1);
   int nLen = hb_parclen(1);
   char *cRet;
   int nPos = 0, nPosRet = 0;
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
      return;
   }


   // maximum possible lenght
   cRet = (char *) hb_xgrab( nLen );

   while ( nPos < nLen )
   {
      cElem = cData[ nPos ];

      if ( cElem == '+' )
      {
         cRet[ nPosRet ] = ' ';
      }
      else if ( cElem == '%' )
      {
         if ( nPos < nLen - 2 )
         {
            cElem = cData[ ++nPos ];
            cRet[ nPosRet ] = cElem < 'A' ? cElem - '0' : cElem - 'A' + 10;
            cRet[ nPosRet ] *= 16;

            cElem = cData[ ++nPos ];
            cRet[ nPosRet ] |= cElem < 'A' ? cElem - '0' : cElem - 'A' + 10;
         }
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
         cRet[ nPosRet ] = cElem;
      }

      nPos++;
      nPosRet++;
   }

   /* this function also adds a zero */
   /* hopefully reduce the size of cRet */
   cRet = (char *) hb_xrealloc( cRet, nPosRet + 1 );
   hb_retclenAdopt( cRet, nPosRet );
}
#pragma ENDDUMP

