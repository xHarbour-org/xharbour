/*
 * $Id: settime.c,v 1.0 2004/01/07 16:53:31 modalsist Exp $
 *
 * xHarbour Project source code:
 * CT3 Date & Time supplementary functions:
 *
 *  SETNEWDATE() is supplementary of SetDate(), see dattime3.prg
 *  SETNEWTIME() is supplementary of SetTime(), see dattime3.prg
 *  WAITPERIOD() used directly by user.
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

#include "hbapi.h"

#if defined(HB_OS_WIN_32)

#include <windows.h>
#include <winbase.h>

#define HB_OS_WIN_32_USED

HB_FUNC ( SETNEWDATE )
{
   WORD wNewYear,wNewMonth,wNewDay,wNewDayOfWeek;
   BOOL lMode;
   SYSTEMTIME st;

   wNewYear = hb_parni(1);
   wNewMonth = hb_parni(2);
   wNewDay = hb_parni(3);
   wNewDayOfWeek = hb_parni(4);
   lMode = hb_parl(5);

   GetLocalTime(&st) ;

   st.wYear = wNewYear ;
   st.wMonth = wNewMonth ;
   st.wDayOfWeek = wNewDayOfWeek ;
   st.wDay = wNewDay ;

   hb_retl ( SetLocalTime(&st) );
}


HB_FUNC ( SETNEWTIME )
{
   WORD wNewHour,wNewMin,wNewSec;
   BOOL lMode ;
   SYSTEMTIME st ;

   wNewHour = (WORD) hb_parni(1);
   wNewMin = (WORD) hb_parni(2);
   wNewSec = (WORD) hb_parni(3);
   lMode = (BOOL) hb_parl(4);

   GetLocalTime(&st) ;

   st.wHour = wNewHour ;
   st.wMinute = wNewMin ;
   st.wSecond = wNewSec ;

   hb_retl ( SetLocalTime(&st) );

}

/*  $DOC$
 *  $FUNCNAME$
 *      WAITPERIOD()
 *  $CATEGORY$
 *      LIBCT date and time functions
 *  $ONELINER$
 *      Pauses a specified time in increments of 1/100 seconds
 *  $SYNTAX$
 *      WAITPERIOD([<nDelay>]) --> lNotElapsed
 *  $ARGUMENTS$
 *      <nDelay>  Designates the waiting period at initialization in
 *      1/100ths of seconds.  Values from 1 to 8, 640, 000 (one day) are
 *      possible.
 *  $RETURNS$
 *      WAITPERIOD() returns .T. , if the time span designated at initialization
 *      has not elapsed.
 *  $DESCRIPTION$
 *      This function sets a time span for a xHarbour DO WHILE loop to run.
 *      The function must initialize prior to the loop, since you must specify
 *      the <nDelay> parameter in 1/100th seconds.  Subsequently, the function
 *      can be implemented without a parameter for additional loop conditions.
 *      It returns .T., as long as the designated time span has not yet run out.
 *
 *      Note
 *
 *      The function notes the status of the internal timer at
 *      initialization.  From that point on, the initialization should always
 *      precede the respective DO WHILE; otherwise, the time delay is
 *      incorrect.  The passing of midnight (the time resets to the 0 value)
 *      is taken into account.
 *  $EXAMPLES$
 *      Run a loop for 5 seconds:
 *
 *      WAITPERIOD(500)               // Initialization, 5 seconds
 *      DO WHILE <cond1> .AND. <cond2> .AND. WAITPERIOD()
 *        *...
 *      ENDDO
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      WAITPERIOD() is Clipper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is settime.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC ( WAITPERIOD )
{
   DWORD dwWait = hb_parnd( 1 ) ;

   Sleep( ( dwWait * 10 ) );

   hb_retl( FALSE );
}


#endif

