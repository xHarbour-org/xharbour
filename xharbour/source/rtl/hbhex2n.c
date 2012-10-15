/*
 * $Id$
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

HB_ULONG hb_hextonum( const char * cHex )
{
   HB_ULONG ulNum = 0;
   char     c;
   int      iDigit;

   while( *cHex && ( *cHex == ' ' ) )
      cHex++;

   while( *cHex )
   {
      ulNum <<= 4;

      c     = *cHex;
      if( c >= '0' && c <= '9' )
      {
         iDigit = c - '0';
      }
      else if( c >= 'A' && c <= 'F' )
      {
         iDigit = c - 'A' + 10;
      }
      else if( c >= 'a' && c <= 'f' )
      {
         iDigit = c - 'a' + 10;
      }
      else
      {
         ulNum = 0;
         break;
      }
      ulNum += iDigit;
      cHex++;
   }

   return ulNum;
}

HB_FUNC( NUMTOHEX )
{
   int      iDigit;
   char     ret[ 33 ];
   int      iLen, iDefaultLen;
   HB_ULONG ulNum;

   if( ISNUM( 2 ) )
   {
      iLen        = hb_parni( 2 );
      iLen        = ( iLen < 1 ) ? 1 : ( ( iLen > 32 ) ? 32 : iLen );
      iDefaultLen = 0;
   }
   else
   {
      iLen        = 32;
      iDefaultLen = 1;
   }

   if( ISNUM( 1 ) )
   {
      ulNum = ( HB_ULONG ) hb_parnint( 1 );
   }
   else if( ISPOINTER( 1 ) )
   {
      ulNum = ( HB_PTRDIFF ) hb_parptr( 1 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "NUMTOHEX", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   ret[ iLen ] = '\0';
   do
   {
      iDigit         = ( int ) ( ulNum & 0x0F );
      ret[ --iLen ]  = ( char ) ( iDigit + ( iDigit < 10 ? '0' : 'A' - 10 ) );
      ulNum          >>= 4;
   }
   while( iDefaultLen ? ulNum > 0 : iLen > 0 );

   hb_retc( &ret[ iLen ] );
}


HB_FUNC( HEXTONUM )
{
   if( ! ISCHAR( 1 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "HEXTONUM", 1, hb_paramError( 1 ) );
      return;
   }

   hb_retnint( hb_hextonum( hb_parc( 1 ) ) );
}


HB_FUNC( STRTOHEX )
{
   char *         cOutBuf;
   const char *   cStr;
   char *         c;
   const char *   cSep = "";
   unsigned char  ucChar;
   HB_SIZE        ul, ulLen, ulLenSep = 0;
   int            iDigit;

   if( ! ISCHAR( 1 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "STRTOHEX", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   if( ISCHAR( 2 ) )
   {
      cSep     = hb_parc( 2 );
      ulLenSep = hb_parclen( 2 );
   }

   cStr  = hb_parc( 1 );
   ulLen = hb_parclen( 1 );

   if( ! ulLen )
   {
      hb_retc( "" );
      return;
   }

   c = cOutBuf = ( char * ) hb_xgrab( ulLen * 2 + ( ulLen - 1 ) * ulLenSep + 1 );

   for( ul = 0; ul < ulLen; ul++ )
   {
      if( ulLenSep && ul )
      {
         HB_MEMCPY( c, cSep, ( size_t ) ulLenSep );
         c += ulLenSep;
      }

      ucChar   = ( unsigned char ) cStr[ ul ];

      iDigit   = ( int ) ( ucChar & 0x0F );
      c[ 1 ]   = ( char ) ( iDigit + ( iDigit < 10 ? '0' : 'A' - 10 ) );

      ucChar   >>= 4;

      iDigit   = ( int ) ucChar;
      c[ 0 ]   = ( char ) ( iDigit + ( iDigit < 10 ? '0' : 'A' - 10 ) );

      c        += 2;
   }
   hb_retclen( cOutBuf, c - cOutBuf );
   hb_xfree( cOutBuf );
}

HB_FUNC( HEXTOSTR )
{
   char *   cOutBuf, * cStr;
   char     c;
   int      iByte, iFirst;
   HB_SIZE  ul, ulLen, ulPos, ulAlloc;

   if( ! ISCHAR( 1 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "HEXTOSTR", 1, hb_paramError( 1 ) );
      return;
   }

   cStr     = ( char * ) hb_parc( 1 );
   ulLen    = hb_parclen( 1 );
   ulAlloc  = ( int ) ( ulLen / 2 );
   cOutBuf  = ( char * ) hb_xgrab( ulAlloc + 1 );

   ulPos    = 0;
   iByte    = 0;
   iFirst   = 1;

   for( ul = 0; ul < ulLen; ul++ )
   {
      iByte <<= 4;

      c     = *cStr++;
      if( c >= '0' && c <= '9' )
      {
         iByte += c - '0';
      }
      else if( c >= 'A' && c <= 'F' )
      {
         iByte += c - 'A' + 10;
      }
      else if( c >= 'a' && c <= 'f' )
      {
         iByte += c - 'a' + 10;
      }
      else
      {
         continue;
      }

      iFirst ^= 1;
      if( iFirst )
      {
         cOutBuf[ ulPos++ ]   = ( char ) iByte;
         iByte                = 0;
      }
   }
   hb_retclen( cOutBuf, ulPos );
   hb_xfree( cOutBuf );
}
