/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Remote Procedure Call code
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
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
 *    Support DateTime
 *
 */

#if defined(__WATCOMC__)
   #pragma disable_message ( 124 )
#elif defined(__POCC__)
   #pragma warn (disable:2130) // Result of comparison is constant.
   #pragma warn (disable:2154) // Unreachable code.
#endif

#include "hbapi.h"
#include "hbapiitm.h"
#include "inet.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbfast.h"

/* Returns a string containing 8 characters in network byte order
 * HB_CreateLen8( nLen ) --> returns the bytes containing the code
 */

static void hb_createlen8( BYTE * ret, HB_LONG uRet )
{
   int i;

   for( i = 7; i >= 0; i-- )
   {
      ret[ i ] = ( BYTE ) ( uRet & 0xff );
      uRet   >>= 8;
   }
}

HB_FUNC( HB_CREATELEN8 )
{
   BYTE ret[ 8 ];

   if( ISNUM( 1 ) )
   {
      hb_createlen8( ret, hb_parnint( 1 ) );
      hb_retclen( ( char * ) ret, 8 );
   }
   else if( ISBYREF( 1 ) && ISCHAR( 1 ) && ISNUM( 2 ) )
   {
      char *  buffer;
      HB_SIZE ulLen;

      if( hb_itemGetWriteCL( hb_param( 1, HB_IT_STRING ), &buffer, &ulLen ) && ulLen >= 8 )
         hb_createlen8( ( BYTE * ) buffer, hb_parnint( 2 ) );
   }
}

/* Returns a numeric length using the first 4 bytes of the given string
 * HB_GetLen8( cStr ) --> nLength
 */
static HB_LONG hb_getlen8( BYTE * cStr )
{
   int     i, iShift;
   HB_LONG ulRet = 0;

   for( i = 7, iShift = 0; i >= 0; i--, iShift += 8 )
      ulRet += ( ( HB_LONG ) cStr[ i ] ) << iShift;

   return ulRet;
}

HB_FUNC( HB_GETLEN8 )
{
   if( hb_parclen( 1 ) < 8 )
      hb_retni( -1 );
   else
      hb_retnint( hb_getlen8( ( BYTE * ) hb_parc( 1 ) ) );
}

/* Serializes a variable into a serialization stream, socket or string
 * HB_SERIALIZE( oVariuous )--> cData
 */
HB_FUNC( HB_SERIALIZESIMPLE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   BYTE * cRet;
   ULONG  ulRet;

   if( pItem == NULL )
   {
      /* TODO: error code
       */
      hb_ret();
      return;
   }

   if( HB_IS_BYREF( pItem ) )
      hb_itemUnRef( pItem );

   if( HB_IS_MEMVAR( pItem ) )
   {
      PHB_VALUE pValue;

      pValue = *( pItem->item.asMemvar.itemsbase ) + pItem->item.asMemvar.offset +
               pItem->item.asMemvar.value;
      pItem  = pValue->pVarItem;
   }

   switch( pItem->type )
   {
      case HB_IT_STRING:
      case HB_IT_MEMOFLAG | HB_IT_STRING:
         ulRet     = ( ULONG ) ( pItem->item.asString.length + 9 );
         cRet      = ( BYTE * ) hb_xgrab( ulRet + 1 );
         cRet[ 0 ] = ( BYTE ) 'C';
         hb_createlen8( cRet + 1, pItem->item.asString.length );
         HB_MEMCPY( cRet + 9, pItem->item.asString.value, ( size_t ) pItem->item.asString.length );
         break;

      case HB_IT_LOGICAL:
         ulRet     = 2;
         cRet      = ( BYTE * ) hb_xgrab( ulRet + 1 );
         cRet[ 0 ] = ( BYTE ) 'L';
         cRet[ 1 ] = ( BYTE ) ( pItem->item.asLogical.value ? 'T' : 'F' );
         break;

      case HB_IT_INTEGER:
         ulRet     = 10;
         cRet      = ( BYTE * ) hb_xgrab( ulRet + 1 );
         cRet[ 0 ] = ( BYTE ) 'N';
         cRet[ 1 ] = ( BYTE ) 'I';
         hb_createlen8( cRet + 2, pItem->item.asInteger.value );
         break;

      case HB_IT_LONG:
         ulRet     = HB_LONG_LENGTH( pItem->item.asLong.value );
         cRet      = ( BYTE * ) hb_xgrab( ulRet + 1 );
         cRet[ 0 ] = ( BYTE ) 'N';
         cRet[ 1 ] = ( ulRet == 20 ) ? ( BYTE ) 'X' : ( BYTE ) 'L';

         hb_createlen8( cRet + 2, pItem->item.asLong.value );
         break;

      case HB_IT_DOUBLE:
         ulRet     = 2 + sizeof( double );
         cRet      = ( BYTE * ) hb_xgrab( ulRet + 1 );
         cRet[ 0 ] = ( BYTE ) 'N';
         cRet[ 1 ] = ( BYTE ) 'D';
         HB_MEMCPY( cRet + 2, &( pItem->item.asDouble.value ), sizeof( double ) );
         break;

      case HB_IT_DATE:
      case HB_IT_TIMEFLAG:
         if( pItem->item.asDate.time == 0 )
         {
            ulRet     = 9;
            cRet      = ( BYTE * ) hb_xgrab( ulRet + 1 );
            cRet[ 0 ] = ( BYTE ) 'D';
            hb_createlen8( cRet + 1, pItem->item.asDate.value );
         }
         else
         {
            double dDateTime = hb_datetimePack( pItem->item.asDate.value, pItem->item.asDate.time );
            ulRet     = 1 + sizeof( double );
            cRet      = ( BYTE * ) hb_xgrab( ulRet + 1 );
            cRet[ 0 ] = ( BYTE ) 'T';
            HB_MEMCPY( cRet + 1, &( dDateTime ), sizeof( double ) );
         }
         break;

      case HB_IT_NIL:
         ulRet     = 1;
         cRet      = ( BYTE * ) hb_xgrab( ulRet + 1 );
         cRet[ 0 ] = ( BYTE ) 'Z';
         break;

      /* not implemented ? */
      default:
         hb_ret();
         return;
   }

   hb_retclenAdopt( ( char * ) cRet, ulRet );
}

/* Deserializes a variable and get the value back
 */
HB_FUNC( HB_DESERIALIZESIMPLE )
{
   PHB_ITEM pItem    = hb_param( 1, HB_IT_STRING );
   LONG     ulMaxlen = ISNUM( 2 )? hb_parnl( 2 ) : -1;
   ULONG    ulData;
   char *   cBuf;

   if( pItem == NULL )
   {
      /* TODO: error code
       */
      hb_ret();
      return;
   }

   cBuf = pItem->item.asString.value;

   switch( cBuf[ 0 ] )
   {
      case 'C':
         ulData = ( ULONG ) hb_getlen8( ( BYTE * ) cBuf + 1 );
         if( ulMaxlen > 0 && ulData > ( ULONG ) ulMaxlen )
            hb_ret();
         else
            hb_retclen( cBuf + 9, ulData );
         break;

      case 'L':
         hb_retl( cBuf[ 1 ] == 'T' );
         break;

      case 'N':
         if( cBuf[ 1 ] == 'I' )
         {
            ulData = ( ULONG ) hb_getlen8( ( BYTE * ) cBuf + 2 );
            hb_retni( ( int ) ulData );
         }
         else if( cBuf[ 1 ] == 'L' )
         {
            ulData = ( ULONG ) hb_getlen8( ( BYTE * ) cBuf + 2 );
            hb_retnl( ( LONG ) ulData );
         }
         else if( cBuf[ 1 ] == 'X' )
            hb_retnint( ( HB_LONG ) hb_getlen8( ( BYTE * ) cBuf + 2 ) );
         else
            hb_retnd( *( ( double * ) ( cBuf + 2 ) ) );

         break;

      case 'D':
         ulData = ( ULONG ) hb_getlen8( ( BYTE * ) ( cBuf + 1 ) );
         hb_retdl( ulData );
         break;

      case 'T':
         hb_retdtd( *( ( double * ) ( cBuf + 1 ) ) );
         break;

      case 'Z':
         /* ulData = 1;
          */
         hb_ret();

         break;
   }
}

ULONG hb_serialNextRaw( const char * cBuf )
{
   ULONG ulData, ulNext;
   ULONG ulCount;

   switch( cBuf[ 0 ] )
   {
      case 'C':
         ulData = ( ULONG ) hb_getlen8( ( BYTE * ) cBuf + 1 );
         return ulData + 9;

      case 'L':
         return 2;

      case 'N':
         if( cBuf[ 1 ] == 'X' ) /* workaround for bug in serialization code */
            return 20;
         if( cBuf[ 1 ] == 'I' || cBuf[ 1 ] == 'X' || cBuf[ 1 ] == 'L' )
            return 10;

         return 2 + sizeof( double );

      case 'D':
         return 9;

      case 'T':
         return 1 + sizeof( double );

      case 'A':
         ulData  = ulNext = 9;
         ulCount = ( ULONG ) hb_getlen8( ( BYTE * ) ( cBuf + 1 ) );

         while( ulCount > 0 )
         {
            cBuf   += ulNext;
            ulNext  = hb_serialNextRaw( cBuf );
            ulData += ulNext;
            ulCount--;
         }
         return ulData;

      case 'H':
         ulData  = ulNext = 9;
         ulCount = ( ULONG ) hb_getlen8( ( BYTE * ) ( cBuf + 1 ) );

         while( ulCount > 0 )
         {
            cBuf   += ulNext;
            ulNext  = hb_serialNextRaw( cBuf );
            cBuf   += ulNext;
            ulData += ulNext;
            ulNext  = hb_serialNextRaw( cBuf );
            ulData += ulNext;
            ulCount--;
         }
         return ulData;

      case 'O':
         ulNext  = 9;
         ulCount = ( ULONG ) hb_getlen8( ( BYTE * ) ( cBuf + 1 ) );
         /* remove class name
          */
         ulNext += hb_serialNextRaw( ( char * ) ( cBuf + 9 ) );
         ulData  = ulNext;

         while( ulCount > 0 )
         {
            /* remove property name
             */
            cBuf   += ulNext;
            ulNext  = hb_serialNextRaw( cBuf );
            ulData += ulNext;
            /* remove property value
             */
            cBuf   += ulNext;
            ulNext  = hb_serialNextRaw( cBuf );
            ulData += ulNext;
            ulCount--;
         }
         return ulData;

      case 'B':
         ulCount = ( ULONG ) hb_getlen8( ( BYTE * ) ( cBuf + 2 ) );
         return ulCount + 10;

      case 'R': return 10;

      case 'Q':
         /* ulNext = 9; */
         ulCount = ( ULONG ) hb_getlen8( ( BYTE * ) ( cBuf + 1 ) );
         return ulCount + 9;

      case 'Z': return 1;
   }
   return 0;
}

HB_FUNC( HB_SERIALNEXT )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   char *   cBuf;

   if( pItem == NULL )
   {
      /* TODO: error code
       */
      hb_ret();
      return;
   }

   cBuf = pItem->item.asString.value;

   hb_retnl( hb_serialNextRaw( cBuf ) );
}


HB_FUNC( HB_DESERIALBEGIN )
{
   BYTE *   cBuf;
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );

   if( pItem == NULL )
   {
      /* TODO: error code
       */
      hb_ret();
      return;
   }

   cBuf = ( BYTE * ) hb_xgrab( pItem->item.asString.length + 9 );
   hb_createlen8( cBuf, 9 );
   HB_MEMCPY( cBuf + 8, pItem->item.asString.value, ( size_t ) pItem->item.asString.length );
   hb_retclenAdopt( ( char * ) cBuf, 8 + pItem->item.asString.length );
}

HB_FUNC( HB_DESERIALIZEARRAY )
{
   HB_SIZE      i, lArrayLen, lNext;
   const char * cBuf    = hb_parc( 2 );
   HB_SIZE      lLen    = (HB_SIZE) hb_parclen( 2 );
   PHB_ITEM     pArray  = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM     pMaxLen = hb_param( 3, HB_IT_ANY );
   PHB_ITEM     pRObj   = hb_param( 4, HB_IT_ANY );
   PHB_ITEM     pRHash  = hb_param( 5, HB_IT_ANY );
   PHB_ITEM     pRArray = hb_param( 6, HB_IT_ANY );
   PHB_ITEM     pRBlock = hb_param( 7, HB_IT_ANY );
   PHB_DYNS     pHB_Deserialize = hb_dynsymFind( "HB_DESERIALIZE" );

   if( pArray && cBuf && pHB_Deserialize )
   {
      lArrayLen = hb_arrayLen( pArray );

      for( i = 1; i <= lArrayLen; i++ )
      {
         hb_vmPushDynSym( pHB_Deserialize );    /* HB_Deserialize( */
         hb_vmPushNil();
         hb_itemPushStaticString( cBuf, lLen ); /*    cSerial, */
         hb_vmPush( pMaxLen );                  /*    nMaxLen, */
         hb_vmPushLogical( TRUE );              /*    .T., */

         if( pRObj )
            hb_vmPush( pRObj );                 /*    aObj, */
         else
            hb_vmPushNil();

         if( pRHash )
            hb_vmPush( pRHash );                /*    aHash, */
         else
            hb_vmPushNil();

         if( pRArray )
            hb_vmPush( pRArray );               /*    aArray, */
         else
            hb_vmPushNil();

         if( pRBlock )
            hb_vmPush( pRBlock );               /*    aBlock */
         else
            hb_vmPushNil();

         hb_vmDo( 7 );                          /*    ) */

         hb_arraySetForward( pArray, i, hb_stackReturnItem() );

         lNext = hb_serialNextRaw( cBuf );
         cBuf += lNext;
         lLen -= lNext;

         if( lLen < 0 )
            lLen = 0;
      }
   }
   hb_retnl( ( LONG ) ( hb_parclen( 2 ) - lLen ) );
}
