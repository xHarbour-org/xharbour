/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Date & Time functions, part II: - ADDMONTH()
 *                                       - CTODOW()
 *                                       - CTOMONTH()
 *                                       - DAYSINMONTH()
 *                                       - DAYSTOMONTH()
 *                                       - DMY()
 *                                       - DOY()
 *                                       - ISLEAP()
 *                                       - LASTDAYOM()
 *                                       - MDY()
 *                                       - NTOCDOW()
 *                                       - NTOCMONTH()
 *                                       - QUARTER()
 *                                       - WEEK()
 *
 * Copyright 2002 Alan Secker <alansecker@globalnet.co.uk>
 * Copyright 2003 Martin Vogel <vogel@inttec.de>: Enhancements, internationalization, documentation headers
 * www - http://www.harbour-project.org
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


#include "set.ch"
#include "hblang.ch"



FUNCTION addmonth ( ddate, nmth )
local nDay
// local ndays
// local sev
// local dnew
// local dEnd
// local nOldday
local nMonth
local nYear
local nLDOM

//   if nmth > 70
//      return stod ()
//   endif
//
//   nOldday := day (ddate)
//   ndays   := nmth * 30
//
//   dnew    := ddate + ndays
//
//   nMonth  := month ( dnew )
//   nNyear  := year  ( dnew )
//
//   dEnd    := ctod (str (nOldday) + "/" + str(nMonth) + "/" + str (nNyear))
//
//   return dEnd

   if !(valtype (ddate) $ "DN")
      return stod()
   endif

   if HB_ISNUMERIC( ddate )
      nmth := ddate
      ddate := date()
   endif

   nmth := int (nmth)

   nDay   := day (ddate)
   nMonth := month (ddate)
   nYear  := year (ddate)

   nMonth += nmth

   if nMonth <= 0
      do while nMonth <= 0
	 nMonth += 12
	 nYear--
      enddo
   endif

   if nMonth > 12
      do while nMonth > 12
	 nMonth -= 12
	 nYear++
      enddo
   endif

   // correction for different end of months
   if nDay > (nLDOM := lastdayom (nMonth))
     nDay := nLDOM
   endif

   ddate := stod (strzero (nYear, 4) + strzero (nMonth, 2) + strzero (nDay, 2))
   return (ddate)



FUNCTION ctodow ( cDow )
//local cWeek  := "SUNMONTUEWEDTHUFRISAT "
//local nWk    := len (cWeek)
//local cMatch := left (upper ( Alltrim (cDow)), 3)
//local n
//local nDay   := 0
//
//   for n := 1 to nWk step 3
//        if RTRIM (substr (cWeek, n, 3)) == cMatch
//           nDay := INT (((n-1) / 3) + 1)
//           exit
//        endif
//   next
//
//   return nDay

local nOrdinal := 0
local bExact

  if !HB_ISSTRING( cDow )
     return (0)
  endif

  bExact := set (_SET_EXACT, .F.)
  cDow   := upper (alltrim (cDow))

  do while nOrdinal < 7
     if upper (alltrim (hb_langmessage (HB_LANG_ITEM_BASE_DAY + nOrdinal))) == cDow
	set (_SET_EXACT, bExact)
	return (nOrdinal+1)
     endif
     nOrdinal++
  enddo

  set (_SET_EXACT, bExact)
  return (0)



FUNCTION ctomonth ( cDom )
//local cMonth := "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC "
//local nMnth  := len (cMonth)
//local cMatch := left (upper ( Alltrim (cDom)), 3)
//local n
//local nDay   := 0

local nOrdinal := 0
local bExact

//   for n := 1 to nMnth step 3
//        if RTRIM (substr (cMonth, n, 3)) == cMatch
//           nDay := INT (((n-1) / 3) + 1)
//           exit
//        endif
//   next
//
//   return nDay

  if !HB_ISSTRING( cDom )
     return (0)
  endif

  bExact := set (_SET_EXACT, .F.)
  cDom   := upper (alltrim (cDom))

  do while nOrdinal < 12
     if upper (alltrim (hb_langmessage (HB_LANG_ITEM_BASE_MONTH + nOrdinal))) == cDom
	set (_SET_EXACT, bExact)
	return (nOrdinal+1)
     endif
     nOrdinal++
  enddo

  set (_SET_EXACT, bExact)
  return (0)



FUNCTION daysInmonth ( nMonth, lLeap )

local nday

  do case
     case nMonth == 2
          if lLeap == .T.
             nday := 29
          else
             nday := 28
          endif

     case nMonth == 4 .or. nMonth == 6 .or. ;
          nMonth == 9 .or. nMonth == 11
          nday := 30
     otherwise
          nday := 31
  endcase

  return nday


FUNCTION daystomonth ( nMonth, lLeap )

local ndays

   switch nMonth
      case 2
           ndays :=  31     // + Jan 31
	   exit
      case 3
           ndays :=  59     // + Feb 28
	   exit
      case 4
           ndays :=  90     // + Mar 31
	   exit
      case 5
           ndays := 120     // + Apr 30
	   exit
      case 6
           ndays := 151     // + May 31
	   exit
      case 7
           ndays := 181     // + Jun 30
	   exit
      case 8
           ndays := 212     // + Jul 31
	   exit
      case 9
           ndays := 243     // + Aug 31
	   exit
      case 10
           ndays := 273     // + Sep 30
	   exit
      case 11
           ndays := 304     // + Oct 31
	   exit
      case 12
           ndays := 334     // + Nov 30
	   exit
      default
	   ndays := 0
   endswitch

   if HB_ISLOGICAL( lLeap ) .and. lLeap
      ndays++
   endif

   return ndays



FUNCTION dmy ( ddate, lmode )

//local nMonth  := month (dDate)
//local nDay    :=   day (dDate)
//local nYear   :=  year (dDate)
local nMonth, nDay, nYear

local cPeriod
local cDate
local cMonth

local cYear
local lSetCentury := __SETCENTURY()

   if !HB_ISDATE( ddate )
      ddate := date ()
   endif

   nMonth  := month (ddate)
   nDay    :=   day (ddate)
   nYear   :=  year (ddate)

   cMonth := ntocmonth ( nMonth )
   cYear := str (nYear, iif (lSetCentury, 4, 2))

   cPeriod := if (lmode == .T., ".", "")


   cDate := ltrim ( str ( nDay )) + cPeriod + " " + cMonth + " " + ;
            ltrim ( cYear )
//            ltrim ( str ( nYear ))
   return cDate



FUNCTION doy ( dDate )

local lleap
local nMonth  := month (dDate)
local nDay    := day (dDate)
local numdays

   if !HB_ISDATE( dDate )
      dDate := date()
   endif

   if empty (dDate)
      return 0
   endif

   lLeap := isleap (dDate)
   numdays := daystomonth ( nMonth, lleap ) + nDay

   return numdays



FUNCTION isleap ( ddate )

local nYear
local nMmyr
local nCyYr
local nQdYr
local lRetval

   if empty ( ddate )
     ddate := date ()
   endif

   nYear  := year (ddate)
   nCyYr  := nYear / 400
//   nMmyr  := nyear /1000
   nMmyr  := nYear /100
   nQdYr  := nYear / 4

   do case
      case int (nCyYr) == nCyYr
           lRetVal := .T.

      case int (nMmyr) == nMmyr
           lRetVal := .F.

      case int (nQdYr) == nQdYr
           lRetVal := .T.

      otherwise
           lRetVal := .F.
   endcase

   return lRetVal


FUNCTION lastdayom ( xDate )

local nMonth := 0
local nDays  := 0
local lleap  := .F.

     do case
        case empty ( xDate)
             nMonth := month ( date() )

        case HB_ISDATE( xDate )
             nMonth  := month (xdate)
             lleap := isleap ( xdate)

        case HB_ISNUMERIC( xDate )
             if xdate > 12
                nmonth := 0
             else
                nMonth := xDate
             endif
     endcase

     if nmonth != 0
        ndays := daysInmonth ( nMonth, lleap )
     endif

     return ndays



FUNCTION mdy ( dDate )

local nMonth
local nDay
local nYear
local cDate
local cMonth

local lSetCentury := __SETCENTURY()
local cYear

//   default dDate to date()
   if !HB_ISDATE( ddate )
      ddate := date ()
   endif

   nMonth  := month (dDate)
   nDay    :=   day (dDate)
   nYear   :=  year (dDate)
   cMonth  := ntocmonth ( nMonth )

   cYear := str (nYear, iif (lSetCentury, 4, 2))

   cDate := cMonth + " " + ;
            ltrim ( str ( nDay )) + " " + ;
            ltrim ( cYear )
   //            ltrim ( str ( nYear ))

   return cDate



FUNCTION ntocdow ( nDay )

local cDay := ""

   if nDay >= 1 .AND. nDay <= 7
     cDay := hb_langmessage (HB_LANG_ITEM_BASE_DAY + (nDay-1))
   endif

   return cDay



FUNCTION ntocmonth ( nMonthNum )

local cMonth := ""

   if nMonthNum >= 1 .AND. nMonthNum <= 12
     cMonth := hb_langmessage (HB_LANG_ITEM_BASE_MONTH + (nMonthNum - 1))
   endif

   return cMonth



FUNCTION quarter ( ddate )

local nmonth
local nretmonth

   if empty (ddate)
      ddate := date()
   endif

   nmonth    := month (ddate)
//   nretmonth := int (( nmonth / 3 ) + 0.67 )
   nretmonth := int ((nmonth + 2) / 3 )

   return nretmonth




FUNCTION week ( dDate, lSWN )

local nMonth
local nDays
local nDay
local nWeek
local nPart
local dDate2
// local nleap

//       do case
//	  case HB_ISDATE( dDate ) .and. empty ( dDate )
//             return nDays
//
//	  case empty (dDate)
//             dDate := date()
//       endcase

     if HB_ISDATE( dDate ) .and. empty (dDate)
	return 0
     endif

     if empty (dDate)
	dDate := date()
     endif

     nMonth  := month (dDate)
     nDay    :=   day (dDate)

     if !HB_ISLOGICAL( lSWN )
	lSWN := .F.
     endif

     if lSWN
       // simple week number

       //     nleap := if (isleap (dDate), 1, nleap)
       //     ndays := daystomonth ( nMonth, nleap ) + nday
       nDays := daystomonth ( nMonth, isleap (dDate)) + nDay

       nPart := nDays % 7
       nWeek := INT (nDays / 7)

       nWeek := INT (if ( nPart > 0, ++ nWeek, nWeek))

     else
	// ISO8601 week number
	dDate2 := dDate + 3 - ((dow(dDate)+5) % 7)
	nWeek := 1 + int ((dDate2 - boy (dDate2)) / 7)

     endif

     return nWeek
