/*
 * $Id: hbhex2n.c,v 1.4 2004/04/03 19:41:19 mlombardo Exp $
 */

/*
 * xHarbour Project source code:
 * Symbolic hexadecimal signature for transfer and visualization
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbdefs.h"
#include "hbvm.h"
#include "hbapierr.h"


HB_FUNC( HB_NUMTOHEX )
{
   ULONG ulNum;
   int iCipher;
   char ret[32];
   char tmp[32];
   int len = 0, len1 = 0;

   if( ISNUM(1) )
   {
      ulNum = (ULONG) hb_parnl( 1 );
   }
   else if ( ISPOINTER( 1 ) )
   {
      ulNum = (ULONG) hb_parptr( 1 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "HB_NUMTOHEX", 1, hb_param(1,HB_IT_ANY) );
      return;
   }


   while ( ulNum > 0 )
   {
      iCipher = ulNum % 16;
      if ( iCipher < 10 )
      {
         tmp[ len++ ] = '0' + iCipher;
      }
      else
      {
         tmp[ len++ ] = 'A' + (iCipher - 10 );
      }
      ulNum >>=4;

   }

   while ( len > 0 )
   {
      ret[len1++] = tmp[ --len ];
   }
   ret[len1] = '\0';

   hb_retc( ret );
}


HB_FUNC( HB_HEXTONUM )
{
   ULONG ulNum = 0;
   char *cHex, c;
   ULONG ulCipher;

   if( ! ISCHAR(1) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "HB_HEXTONUM", 1, hb_param(1,HB_IT_ANY) );
      return;
   }
   cHex = hb_parc( 1 );

   while ( *cHex )
   {
      c = *cHex;
      ulNum <<= 4;
      if ( c >= '0' && c <= '9' )
      {
         ulCipher = (ULONG) ( c - '0' );
      }
      else if ( c >= 'A' && c <= 'F' )
      {
         ulCipher = (ULONG) ( c - 'A' )+10;
      }
      else if ( c >= 'a' && c <= 'f' )
      {
         ulCipher = (ULONG) ( c - 'a' )+10;
      }
      else
      {
         ulNum = 0;
         break;
      }
      ulNum += ulCipher;
      cHex++;
   }

   hb_retnl( (LONG) ulNum  );
}

HB_FUNC( HB_STRTOHEX )
{
   char *outbuff;
   char *cStr;
   char *c;
   USHORT i, len;
   int iCipher, iNum;

   if( ! ISCHAR(1) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "HB_STRTOHEX", 1, hb_param(1,HB_IT_ANY) );
      return;
   }

   cStr = hb_parc( 1 );
   len = hb_parclen( 1 );
   outbuff = hb_xgrab( (len * 2) + 1 );
   c = outbuff;

   for( i = 0; i < len; i++ )
   {

      iNum = cStr[i];
      c[0] = '0';
      c[1] = '0';

      iCipher = iNum % 16;
      if ( iCipher < 10 )
      {
         c[1] = '0' + iCipher;
      }
      else
      {
         c[1] = 'A' + (iCipher - 10 );
      }
      iNum >>=4;

      iCipher = iNum % 16;
      if ( iCipher < 10 )
      {
         c[0] = '0' + iCipher;
      }
      else
      {
         c[0] = 'A' + (iCipher - 10 );
      }

      c+=2;
   }

   outbuff[len*2] = '\0';
   hb_retc( outbuff );
}

HB_FUNC( HB_HEXTOSTR )
{
   char *outbuff;
   char *cStr;
   char c;
   USHORT i, len, nalloc;
   int iCipher, iNum;

   if( ! ISCHAR(1) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "HB_STRTOHEX", 1, hb_param(1,HB_IT_ANY) );
      return;
   }

   cStr = hb_parc( 1 );
   len = hb_parclen( 1 );
   nalloc = (USHORT) (len/2);
   outbuff = hb_xgrab( nalloc + 1 );

   for( i = 0; i < nalloc; i++ )
   {
      // First byte

      c = *cStr;
      iNum = 0;
      iNum <<= 4;
      iCipher = 0;

      if ( c >= '0' && c <= '9' )
      {
         iCipher = (ULONG) ( c - '0' );
      }
      else if ( c >= 'A' && c <= 'F' )
      {
         iCipher = (ULONG) ( c - 'A' )+10;
      }
      else if ( c >= 'a' && c <= 'f' )
      {
         iCipher = (ULONG) ( c - 'a' )+10;
      }

      iNum += iCipher;
      cStr++;

      // Second byte

      c = *cStr;
      iNum <<= 4;
      iCipher = 0;

      if ( c >= '0' && c <= '9' )
      {
         iCipher = (ULONG) ( c - '0' );
      }
      else if ( c >= 'A' && c <= 'F' )
      {
         iCipher = (ULONG) ( c - 'A' )+10;
      }
      else if ( c >= 'a' && c <= 'f' )
      {
         iCipher = (ULONG) ( c - 'a' )+10;
      }

      iNum += iCipher;
      cStr++;
      outbuff[i] = iNum;
   }

   outbuff[nalloc] = '\0';
   hb_retc( outbuff );
}
