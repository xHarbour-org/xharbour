/*
 * $Id: dattime3.prg,v 1.6 2006/10/31 02:41:42 modalsist Exp $
 */

/*
 * xHarbour project source code:
 * CT3 date and time functions
 *
 * SETDATE, SETTIME, SHOWTIME, TIMEVALID
 *
 * Copyright 2003 Carlos Eduardo Brock <brock_carlos@yahoo.com.br>
 * Author of ShowTime supplementary functions.
 *
 * Copyright 2005 Eduardo Fernandes <modalsist@yahoo.com.br>
 * Author of SetDate, SetTime and TimeValid.
 * Author, in conjoint with Carlos Eduardo Brock, of ShowTime.
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

#pragma BEGINDUMP

#include "hbapigt.h"

#pragma ENDDUMP

#include "common.ch"

/* Showtime static vars */
STATIC s_aSHT_Timer := {}   // array of timers created for each ShowTime call.
STATIC s_nSHT_Handle        // handle returned by HB_IdleAdd() function.
STATIC s_nSHT_Id    := 0    // Id for each showtime call, if any.

*--------------------------------------------------------------------------
FUNCTION ShowTime( nRow , nCol , lHideSeconds , cColor , lTwelve , lAmPm  )
*--------------------------------------------------------------------------
Local nColMax, nWindow
Local cId

   IF PCount()==0 //.AND. s_nSHT_Handle != NIL
      IF s_nSHT_Handle != NIL
         hb_ShowTimeOff()
      ENDIF
      RETURN ""
   ENDIF

   s_nSHT_Id ++
   cId := "ShowTime_"+Ltrim(Str(s_nSHT_Id))

   if s_nSHT_Id > 1 .AND. s_nSHT_Handle != NIL
      hb_ShowTimeOff()
   endif

   nWindow := SetMainWindow()

   DEFAULT nRow         TO Row()
   DEFAULT nCol         TO Col()    
   DEFAULT lHideSeconds TO .F.
   DEFAULT cColor       TO setcolor()
   DEFAULT lTwelve      TO .F.
   DEFAULT lAmPm        TO .F.
 
   nRow := Max(0,nRow)
   nCol := Max(0,nCol)

   nColMax := MaxCol() - iif(lHideSeconds,4,7) - iif(lAmPm,1,0)

   nCol := Min( nCol, nColMax )
   nRow := Min( nRow, MaxRow() )

   RestoreWindow( nWindow )

   hb_ShowTimeEvent( cId, .T., { || hb_ShowTimeClock( nRow, nCol , cColor , lHideSeconds , lTwelve , lAmPm ) }, 100 )

RETURN ""

*-------------------------------
STATIC FUNCTION hb_ShowTimeOff()
*-------------------------------

   hb_ShowTimeEvent( "*" )

RETURN NIL

*----------------------------------------------------------------
STATIC FUNCTION hb_ShowTimeEvent( cIDName, lActiv, bCode, nTime )
*----------------------------------------------------------------
Local nHPos, cIdDel, nHDel

   IF ValType( cIDName ) == "U" .AND. ValType( lActiv ) == "U"
      RETURN NIL
   ENDIF

   IF valtype(cIdName)=="U"
      cIdName := "*"
   ENDIF

   IF valtype(lActiv)=="U"
      lActiv := .T.
   ENDIF


   IF cIDName == "*"

      HB_IdleDel( s_nSHT_Handle )
      s_nSHT_Handle := NIL

      IF !Empty( s_aSHT_Timer )

         IF s_nSHT_Id > 1
            ADel( s_aSHT_Timer, s_nSHT_Id - 1, .T.)
         ELSEIF s_nSHT_Id == 1
            ADel( s_aSHT_Timer, 1, .T.)
         ENDIF

      ENDIF

      RETURN NIL

   ELSE

      nHPos  := ASCan( s_aSHT_Timer, {|nI| nI[ 1 ] == cIDName } )

      IF nHPos == 0
         AAdd( s_aSHT_Timer, { cIDName, .T., bCode, nTime, lActiv, hb_ShowTimeProxExc(1)} )
      ELSE
         s_aSHT_Timer[ nHPos ][ 5 ] := lActiv
      ENDIF

      IF s_nSHT_Handle == NIL
         s_nSHT_Handle := HB_IdleAdd( {|| hb_ShowTime_Eval_Event() } )
      ENDIF

   ENDIF

RETURN NIL

*---------------------------------------
STATIC FUNCTION hb_ShowTime_Eval_Event()
*---------------------------------------
Local aTimer, nC := Col(), nR := Row()

   For Each aTimer IN s_aSHT_Timer 

       aTimer[ 2 ] := .F.   // will not re-excuted during the execution.

       IF aTimer[ 5 ] .AND. aTimer[ 6 ] <= hb_ShowTimeProxExc()
          Eval( aTimer[ 3 ] )
          aTimer[ 6 ] := hb_ShowtimeProxExc( aTimer[ 4 ] )
       ENDIF

       aTimer[ 2 ] := .T.

   Next

   // 2006/OCT/30 - E.F. - Changed to setpos() to avoid move printer head to
   //                      screen coordinates if "set device to printer" is seted
   //                      under gtwin while showtime() is active.
   //DevPos( nR, nC )
   SetPos( nR, nC )

RETURN NIL

*------------------------------------------
STATIC FUNCTION hb_ShowTimeProxExc( nTime )
*------------------------------------------
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

*---------------------------------------------------------------------------------------
STATIC FUNCTION hb_ShowTimeClock( nRow, nCol , cColor , lHideSeconds , lTwelve , lAmPm )
*---------------------------------------------------------------------------------------
Local cTime := ""
Local cShowTime := ""
Local cHour := ""
Local cAmPm
Local nWindow

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

      nWindow := SetMainWindow()

      // 2006/OCT/30 - E.F. - Changed to Dispoutat() to force show time value
      //                      at display screen only. This avoid the printing
      //                      show time value if "set device to printer" is seted
      //                      under gtwin while showtime() is active.  
      //@ nRow, nCol say + cShowTime color cColor
      DispOutAt( nRow, nCol, cShowTime, cColor )

      RestoreWindow( nWindow )

      cTime := Time()

   ENDIF

RETURN NIL

*--------------------------------
FUNCTION SetDate( dDate , lMode )
*--------------------------------
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

*-------------------------------
FUNCTION SetTime( cTime, lMode )
*-------------------------------
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


*--------------------------
FUNCTION TimeValid( cTime )
*--------------------------
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


#pragma BEGINDUMP

HB_FUNC_STATIC( SETMAINWINDOW )
{
   hb_retni( hb_ctWSelect( -1 ) );
   hb_ctWSelect( 0 );
}

HB_FUNC_STATIC( RESTOREWINDOW )
{
   int iWindow = hb_parni( 1 );

   if( iWindow != 0 )
      hb_ctWSelect( iWindow );
}

#pragma ENDDUMP
