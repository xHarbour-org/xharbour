/*
 * $Id$
 *
 * xHarbour Project source code:
 * CT3 Date & Time supplementary functions:
 *
 *  SETNEWDATE() is supplementary of SetDate(), see dattime3.prg
 *  SETNEWTIME() is supplementary of SetTime(), see dattime3.prg
 *
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
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
 *
 * See doc/license.txt for licensing terms.
 *
 */
#ifndef _SVID_SOURCE
   #define _SVID_SOURCE
#endif

#include "hbapi.h"
#include "hbdate.h"

#if defined( HB_OS_WIN )
#include <windows.h>
#include <winbase.h>
#endif

#include <time.h>

HB_FUNC( SETNEWDATE )
{
#if defined( HB_OS_WIN )
   {
      WORD        wNewYear, wNewMonth, wNewDay, wNewDayOfWeek;
      // BOOL lMode;
      SYSTEMTIME  st;

      wNewYear       = ( WORD ) hb_parni( 1 );
      wNewMonth      = ( WORD ) hb_parni( 2 );
      wNewDay        = ( WORD ) hb_parni( 3 );
      wNewDayOfWeek  = ( WORD ) hb_parni( 4 );
      // lMode = hb_parl(5);

      GetLocalTime( &st );

      st.wYear       = wNewYear;
      st.wMonth      = wNewMonth;
      st.wDayOfWeek  = wNewDayOfWeek;
      st.wDay        = wNewDay;

      hb_retl( SetLocalTime( &st ) );
   }
#elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ )
/* stime exists only in SVr4, SVID, X/OPEN and Linux */
   {
      /* LONG lNewYear,lNewMonth,lNewDay,lNewDayOfWeek; */

      ULONG    lNewDate;
      int      iY, iM, iD;
      time_t   tm;

      iY       = hb_parni( 1 );
      iM       = hb_parni( 2 );
      iD       = hb_parni( 3 );

      lNewDate = hb_dateEncode( iY, iM, iD ) - hb_dateEncode( 1970, 1, 1 );

      tm       = time( NULL );
      tm       = lNewDate * 86400 + ( tm % 86400 );

      hb_retl( stime( &tm ) == 0 );
   }
#else
   hb_retl( FALSE );
#endif
}

HB_FUNC( SETNEWTIME )
{
#if defined( HB_OS_WIN )
   {
      WORD        wNewHour, wNewMin, wNewSec;
      // BOOL lMode;
      SYSTEMTIME  st;

      wNewHour = ( WORD ) hb_parni( 1 );
      wNewMin  = ( WORD ) hb_parni( 2 );
      wNewSec  = ( WORD ) hb_parni( 3 );
      // lMode = (BOOL) hb_parl(4);

      GetLocalTime( &st );

      st.wHour    = wNewHour;
      st.wMinute  = wNewMin;
      st.wSecond  = wNewSec;

      hb_retl( SetLocalTime( &st ) );
   }
#elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ )
/* stime exists only in SVr4, SVID, X/OPEN and Linux */
   {
      ULONG    lNewTime;
      int      iH, iM, iS;
      time_t   tm;

      iH       = hb_parni( 1 );
      iM       = hb_parni( 2 );
      iS       = hb_parni( 3 );
      lNewTime = iH * 3600 + iM * 60 + iS;

      tm       = time( NULL );
      tm       += lNewTime - ( tm % 86400 );

      hb_retl( stime( &tm ) == 0 );
   }
#else
   hb_retl( FALSE );
#endif

}
