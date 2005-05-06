/*
 * $Id: dattime3.prg,v 1.1 2004/08/25 17:03:00 lf_sfnet Exp $
 */

/*
 * xHarbour project source code:
 * CT3 date and time functions
 *
 * SETDATE, SETTIME, SHOWTIME, TIMEVALID
 *
 * Copyright 2003 Carlos Eduardo Brock <brock_carlos@yahoo.com.br>
 * Author of any supplementary functions to ShowTime().
 *
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 * Author of SetDate, SetTime, TimeValid and WaitPeriod.
 * Author, in conjointly with Carlos Eduardo Brock, of ShowTime.
 *
 * http://www.xhabour.org
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
#include "common.ch"
STATIC aH_Timers := {}, nHandle, lHandle

***************************************************************************
FUNCTION ShowTime( nRow , nCol , lHideSeconds , cColor , lTwelve , lAmPm  )
***************************************************************************
   Local nColMax

   IF valtype(nRow)==NIL .and.;
      valtype(nCol)==NIL .and.;
      valtype(lHideSeconds)==NIL .and.;
      valtype(cColor)==NIL .and.;
      valtype(lTwelve)==NIL .and.;
      valtype(lAmPm)==NIL

      hb_ShowTimeOff()

   ELSE

      default nrow         TO row()
      default nCol         TO  Col()    
      default lHideSeconds TO .F.
      default cColor       TO setcolor()
      default lTwelve      TO .F.
      default lAmPm        TO .F.
 


      IF nRow < 0
         nRow := 0
      ENDIF

      IF nCol < 0
         nCol := 0
      ENDIF

      nColMax := maxcol() - iif(lHideSeconds,4,7) - iif(lAmPm,1,0)

      nCol := Min( ncol, nColMax )
      nRow := Min( nRow, maxrow() )
      
      hb_ShowTimeEvent( "ShowTime", .T., { || hb_ShowTimeClock( nRow, nCol , cColor , lHideSeconds , lTwelve , lAmPm ) }, 100 )

   ENDIF

RETURN ""


STATIC FUNCTION hb_ShowTimeOff()
// supplementary showtime FUNCTION
   hb_ShowTimeEvent( "*", .F. )

RETURN NIL

STATIC FUNCTION hb_ShowTimeClock( nRow, nCol , cColor , lHideSeconds , lTwelve , lAmPm )
// supplementary showtime FUNCTION
   Local cTime := ""
   Local cShowTime := ""
   Local cHour := ""
   Local cAmPm

   IF cTime <> Time()

      cShowTime := IIF(!lHideSeconds,time(),substr(time(),1,5))

      IF lTwelve

         cHour := Substr(cShowTime,1,2)
         cAmPm := IIF(val(cHour)>11,"p","a")

         IF Val( cHour ) > 12
            cHour := Str( Val( cHour )-12 ,2,0)
         ENDIF

         cShowTime := cHour + Substr(cShowTime,3)+IIF(lAmPm,cAmPm,"")

      ENDIF

      @ nRow, nCol say + cShowTime color cColor
      cTime := Time()

   ENDIF

RETURN NIL

STATIC FUNCTION hb_ShowTimeEvent( cIDName, lActiv, bCode, nTime )
// supplementary showtime FUNCTION
   Local nHPos

   IF ValType( cIDName ) == "U" .AND. ValType( lActiv ) == "U"
      RETURN lHandle
   ENDIF

   IF valtype(cIdName)=="U"
      cIdName := "*"
   ENDIF

   IF valtype(lActiv)=="U"
      lActiv := .T.
   ENDIF


   IF cIDName <> "*"
      nHPos  := ASCan( aH_Timers, {|nI| nI[ 1 ] == cIDName } )
   ELSE

      nHPos := 0

      IF ! lActiv
         HB_IdleDel( nHandle )
         lHandle := .F.
         nHandle := NIL
      ELSE
         nHandle := HB_IdleAdd( {|| hb_ShowTime_Eval_Event( ) } )
         lHandle := .T.
      ENDIF

      RETURN lHandle
   ENDIF

   IF ( nHPos == 0 .AND. ValType( lActiv ) == "L" )
      AAdd( aH_Timers, { cIDName, .T., bCode, nTime, lActiv, hb_ShowTimeProxExc(1)} )
   ELSE

      IF lActiv == .F.
         aH_Timers[ nHPos ][ 5 ] := .F.
       ELSE
         aH_Timers[ nHPos ][ 5 ] := .T.
      ENDIF

   ENDIF

   IF nHandle == NIL
      nHandle := HB_IdleAdd( {|| hb_ShowTime_Eval_Event( ) } )
      lHandle := .T.
   ENDIF

RETURN lHandle

STATIC FUNCTION hb_ShowTime_Eval_Event( )
   Local nI, nC := Col(), nR := Row()
   STATIC nCont := 0

   nCont ++
   For nI := 1 To Len( aH_Timers )
      aH_Timers[ nI ][ 2 ] := .F.   // will not re-excuted during the execution.

      IF aH_Timers[ nI ][ 5 ] .AND. aH_Timers[ nI ][ 6 ] <= hb_ShowTimeProxExc() .AND. aH_Timers[ nI ][ 5 ]
         Eval( aH_Timers[ nI ][ 3 ] )
         aH_Timers[ nI ][ 6 ] := hb_ShowtimeProxExc( aH_Timers[ nI ][ 4 ] )
      ENDIF

      aH_Timers[ nI ][ 2 ] := .T.
   Next

   DevPos( nR, nC )

RETURN NIL

STATIC FUNCTION hb_ShowTimeProxExc( nTime )
// Showtime FUNCTION sypplementary
   Local cNext, nSec, cDays, dData := Date()

   IF valtype(nTime)=="U"
      nTime := 0
   ENDIF

   nSec := Seconds() + ( nTime / 100 )

   IF nSec > 86399    // seconds of one day
      nSec := 86399 - nSec
      dData ++
   ENDIF

   cDays := LTrim( Str( Int( dData - CToD("01/01/2000") ) ) )
   cNext := cDays + "," + LTrim( Str( Int( nSec ) ) )

RETURN cNext


*********************************
FUNCTION SetDate( dDate , lMode )
*********************************
   Local nYear,nMonth,nDay,nDoW,lRet := .F.

   IF valtype( lMode ) != "L"
      lMode := .F.
   ENDIF

   IF valtype ( dDate ) == "D"
      nYear   := Year(dDate)
      nMonth  := Month(dDate)
      nDay    := Day(dDate)
      nDoW    := DOW(dDate)
      lRet    := SetNewDate( nYear , nMonth , nDay , nDoW , lMode )
   ENDIF

RETURN lRet

*********************************
FUNCTION SetTime( cTime, lMode )
*********************************
   Local nNewHour,nNewMin,nNewSec,lRet := .F.

   IF valtype( lMode ) != "L"
      lMode := .F.
   ENDIF

   IF TimeValid( cTime )
      nNewHour := Val( Substr(cTime,1,2))
      nNewMin  := Val( Substr(cTime,4,2))
      nNewSec  := Val( Substr(cTime,7,2))
      lRet  := SetNewTime( nNewHour , nNewMin , nNewSec , lMode )
   ENDIF

RETURN lRet


***************************
FUNCTION TimeValid( cTime )
***************************
   Local nHour,nMin,nSec
   Local cHour,cMin,cSec

   IF valtype( cTime ) != "C"
      RETURN ( .F. )
   ENDIF

   cHour := Substr(cTime,1,2)
   cMin  := Substr(cTime,4,2)
   cSec  := IIF(Len(cTime)=8,Substr(cTime,7,2),"")

   IF empty( cHour ) .or. empty( cMin ) .or. Len( cMin ) < 2
      RETURN .F.
   ENDIF

   nHour := Val( cHour )
   nMin  := Val( cMin )
   nSec  := Val( cSec )

   IF nHour >= 0 .and. nHour <= 23
      IF nMin >= 0 .and. nMin <= 59
         IF nSec >= 0 .and. nSec <= 59
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

RETURN .F.
