/*
 * $Id: dattime3.prg,v 1.3 2004/06/19 10:30:00 modalsist Exp $
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

/*  $DOC$
 *  $FUNCNAME$
 *      SHOWTIME()
 *  $CATEGORY$
 *      LibCT Date and Time Functions
 *  $ONELINER$
 *      Continuously displays the time at desired screen position
 *  $SYNTAX$
 *      SHOWTIME([<nRow>], [<nColumn>], [<lMode>],
 *               [<cAttr|nAttr>], [<l12/24>], [<lAM/PM>]) --> cNull
 *  $ARGUMENTS$
 *      <nRow>  Designates the row where the time is displayed.  The default
 *      is the cursor row.
 *
 *      <nColumn>  Designates the column where the time is displayed.  The
 *      default is the cursor column. The maximum column is calculated by
 *      FUNCTION if it´s exceed showtime string length.
 *
 *      <lMode>  Designates whether seconds are to appear in the time
 *      display (.F.), or not (.T.).  The default is display seconds (.F.).
 *
 *      <cAttr|nAttr>  Designates to the screen attribute to use for the
 *      time display.  Strings in the form "NN/NN" or "CC/CC" (e.g., "0/7" or
 *      "B/GR") are possible.  The default is the setting for SetColor().
 *
 *      <l12/24>  By designating this parameter as .T., a 12-hour display is
 *      selected.  The default is a 24-hour display (.F.).
 *
 *      <lAM/PM>  If you have a 12-hour display, you may want to switch on
 *      an am/pm display.  When you specify this parameter as .T., an "a" or "p"
 *      (as in DOS), appears to reflect the time.  The default is no display
 *      (.F.).
 *
 *      A call in ShowTime() without parameters uninstalls the time display.
 *
 *  $RETURNS$
 *      SHOWTIME() always RETURNs a null string.
 *  $DESCRIPTION$
 *      This FUNCTION permits you to constantly display the time in any screen
 *      position desired. SHOWTIME() is interrupt controlled and has the ability
 *      to display the time in either "hh:mm:ss" or "hh:mm" format.  A 12- or 24-
 *      hour display can be selected, with or without an am/pm display.
 *
 *      Warning!  SHOWTIME() works with the interrupt system.  Before
 *      leaving your xHarbour application, you must uninstall SHOWTIME() , so
 *      that the interrupt vectors can be restored.  Otherwise, the system will
 *      be unstable and will eventually crash.  Simultaneous use of the
 *      accompanying Extended Drivers Modules automatically restores the
 *      interrupt vectors in use.
 *
 *      Specify the screen attribute as either a numeric or a string
 *      in "NN/NN" form.  If no attribute is specified, the value for CLEARA
 *      applies (see Introduction Video FUNCTIONs).
 *  $EXAMPLES$
 *      Display a clock in row 0, column 70, with no seconds display
 *      and the standard attribute.  When a display is shown in row 0, switch
 *      SCOREBOARD off!
 *
 *      SET SCOREBOARD OFF
 *      SHOWTIME(0, 70, .T.)      //  Turn on clock
 *         *...
 *         *...
 *      SHOWTIME()               //  Uninstall clock
 *
 *      Show a 12-hour display. Hide seconds and show am/pm on the lowest row.
 *      Show white numbers on a blue background:
 *
 *      SHOWTIME(24, 70, .T., "W/B" , .T., .T. )
 *
 *  $TESTS$
 *      SHOWTIME(24, 70, .F., "W/B" , .F. , .F. )  -> 13:45:30
 *      SHOWTIME(24, 70, .F., "W/B" , .F. , .T. )  -> 13:45:30
 *      SHOWTIME(24, 70, .F., "W/B" , .T. , .F. )  ->  1:45:30
 *      SHOWTIME(24, 70, .F., "W/B" , .T. , .T. )  ->  1:45:30p
 *      SHOWTIME(24, 70, .T., "W/B" , .F. , .F. )  -> 13:45
 *      SHOWTIME(24, 70, .T., "W/B" , .F. , .T. )  -> 13:45
 *      SHOWTIME(24, 70, .T., "W/B" , .T. , .F. )  ->  1:45
 *      SHOWTIME(24, 70, .T., "W/B" , .T. , .T. )  ->  1:45p
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is dattime3.prg, library is libct.
 *  $SEEALSO$
 *      TIME()
 *  $END$
 */
***************************************************************************
FUNCTION ShowTime( nRow , nCol , lHideSeconds , cColor , lTwelve , lAmPm  )
***************************************************************************
   Local nColMax

   IF valtype(nRow)=="U" .and.;
      valtype(nCol)=="U" .and.;
      valtype(lHideSeconds)=="U" .and.;
      valtype(cColor)=="U" .and.;
      valtype(lTwelve)=="U" .and.;
      valtype(lAmPm)=="U"

      hb_ShowTimeOff()

   ELSE

      IF valtype(nRow)=="U"
         nRow := Row()
      ENDIF

      IF valtype(nCol)=="U"
         nCol := Col()
      ENDIF

      IF valtype(lHideSeconds)=="U"
         lHideSeconds := .F.
      ENDIF

      IF valtype(cColor)=="U"
         cColor := setcolor()
      ENDIF

      IF valtype(lTwelve)=="U"
         lTwelve := .F.
      ENDIF

      IF valtype(lAmPm)=="U"
         lAmPm := .F.
      ENDIF

      IF !lTwelve
         lAmPm := .F.
      ENDIF

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
   Local nHD,nHPos

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

      IF ! lActiv
         HB_IdleDel( nHandle )
         lHandle := .F.
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


/*  $DOC$
 *  $FUNCNAME$
 *      SETDATE()
 *  $CATEGORY$
 *      LibCT Date and Time Functions
 *  $ONELINER$
 *      Sets the system date
 *  $SYNTAX$
 *      SETDATE(<dDate>, [<lMode>]) --> lSet
 *  $ARGUMENTS$
 *      <dDate>  Designates which date to use to set the system date.
 *
 *      <lMode>  Designates whether the date should also be set in the CMOS-
 *      RAM of an AT.  The default is do not write (.F.). Note that in Windows
 *      plataform this adjust is automatic, therefore this parameter is without
 *      efect.
 *  $RETURNS$
 *      SETDATE() RETURNs .T. when the date is successfully set.
 *  $DESCRIPTION$
 *      When you use this FUNCTION to set the system date from within your
 *      xHarbour application, all files acquire this date with each write
 *      procedure.
 *  $EXAMPLES$
 *      Set the system date in each case; but the hardware clock only
 *      on an AT:
 *
 *      dNewDate  :=  CTOD("07/30/91")
 *      IF ISAT()
 *         SETDATE(dNewDate, .T.)
 *      ELSE
 *         SETDATE(dNewDate)
 *      ENDIF
 *
 *      Or, more compactly:
 *
 *      SETDATE(dNewDate, ISAT())
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is dattime3.prg, library is libct.
 *  $SEEALSO$
 *      SETTIME()
 *  $END$
 */
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

/*  $DOC$
 *  $FUNCNAME$
 *      SETTIME()
 *  $CATEGORY$
 *      LibCT Date and Time Functions
 *  $ONELINER$
 *      Sets the system clock
 *  $SYNTAX$
 *      SETTIME(<cTime>, [<lMode>]) --> lSet
 *  $ARGUMENTS$
 *      <cTime>  Designates a character string that contains the time that
 *      is to become the system time.
 *
 *      <lMode>  Designates whether the time should also be set in the
 *      CMOS-RAM of an AT.  The default is do not write to CMOS-RAM. Note that in
 *      Windows plataform this adjust is automatic, therefore this parameter is
 *      without efect.
 *  $RETURNS$
 *      The FUNCTION RETURNs .T. when the time is set successfully.
 *  $DESCRIPTION$
 *      When you use this FUNCTION to convert the time into the system time from
 *      within your xHarbour application, all files acquire this time with
 *      each write procedure.
 *  $EXAMPLES$
 *      Set the system time in each case; but the hardware clock only
 *      on an AT:
 *
 *      cNewTime  :=  "10:20:00"
 *      IF ISAT()
 *        SETTIME(cNewTime, .T.)
 *      ELSE
 *        SETTIME(cNewTime)
 *      ENDIF
 *
 *      Or, more compactly:
 *
 *      SETTIME(cNewTime, ISAT())
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is dattime3.prg, library is libct.
 *  $SEEALSO$
 *      SETDATE(),TIMEVALID()
 *  $END$
 */
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

/*  $DOC$
 *  $FUNCNAME$
 *      TIMEVALID()
 *  $CATEGORY$
 *      LibCT Date and Time Functions
 *  $ONELINER$
 *      Determines whether a specIFied time is valid
 *  $SYNTAX$
 *      TIMEVALID(<cTime>) --> lValid
 *  $ARGUMENTS$
 *      <cTime>  Designates a character string that contains the time to
 *      test.
 *  $RETURNS$
 *      TIMEVALID() RETURNs .T. when <cTime> is a valid time; or .F. when
 *      <cTime> is an invalid time.
 *  $DESCRIPTION$
 *      With input that requires time manipulation, writing your own UDF to
 *      check time inputs was unavoidable up to now.  TIMEVALID() permits
 *      Complete checking of a time designation.  You can use this FUNCTION
 *      effectively with a VALID clause within a READ mask.
 *
 *      Note
 *
 *      Note the format for time designations.  There must always be
 *      two digits for hours, minutes, seconds, and hundredths; otherwise,
 *      the time it is regarded as invalid.  Valid examples are "12",
 *      "12:59", "12:59:59", and "12:59:59:99".  By contrast, invalid
 *      examples are "24", "12:60", or "12:1", and/or "12:".  IF you work
 *      with time strings that are not completely filled and that you need to
 *      check with TIMEVALID(), then they must be TRIMmed prior to the use of
 *      TIMEVALID() (see following Examples).
 *  $EXAMPLES$
 *      Using the VALID clause with TRIM, all valid times are
 *      accepted, even IF no seconds or minutes are specIFied:
 *
 *      cBegin  :=  SPACE(11)
 *      @ 5, 10 SAY "Please input time for beginning work:";
 *      GET cBegin VALID TIMEVALID(TRIM(cBegin))
 *      READ
 *
 *      Using a VALID clause without TRIM, hours and minutes must be
 *      specified, so that TIMEVALID() can confirm a valid time:
 *
 *      cBegin  :=  SPACE(5)
 *      @ 5, 10 SAY "Please input time for beginning work:";
 *      GET cBegin VALID TIMEVALID(cBegin)
 *      READ
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is dattime3.prg, library is libct.
 *  $SEEALSO$
 *      SETTIME()
 *  $END$
 */

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
