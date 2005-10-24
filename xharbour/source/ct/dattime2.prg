/*
 * $Id: dattime2.prg,v 1.4 2005/01/15 15:38:02 ptsarenko Exp $
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
 * Copyright 2005 Pavel Tsarenko <tpe2@mail.ru>: some functons rewritten in C
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


FUNCTION ctodow ( cDow )

local nOrdinal := 0
local bExact

  if valtype (cDow) != "C"
     return (0)
  endif
  
  bExact = set (_SET_EXACT, .F.)
  cDow = upper (alltrim (cDow))

  do while nOrdinal < 7
     if upper (alltrim (hb_langmessage (HB_LANG_ITEM_BASE_DAY + nOrdinal))) = cDow
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

//   for n = 1 to nMnth step 3
//        if RTRIM (substr (cMonth, n, 3)) == cMatch
//           nDay := INT (((n-1) / 3) + 1)
//           exit
//        endif
//   next
//
//   return nDay

  if valtype (cDom) != "C"
     return (0)
  endif
  
  bExact = set (_SET_EXACT, .F.)
  cDom = upper (alltrim (cDom))

  do while nOrdinal < 12
     if upper (alltrim (hb_langmessage (HB_LANG_ITEM_BASE_MONTH + nOrdinal))) = cDom
        set (_SET_EXACT, bExact)
        return (nOrdinal+1)
     endif
     nOrdinal++
  enddo

  set (_SET_EXACT, bExact)
  return (0)


FUNCTION dmy ( ddate, lmode )

local nMonth, nDay, nYear 

local cPeriod := ""
local cDate
local cMonth 

local cYear
local lSetCentury := __SETCENTURY()

   if valtype (ddate) != "D"
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
   return cDate


   
FUNCTION mdy ( dDate )

local nMonth
local nDay
local nYear
local cDate
local cMonth 

local lSetCentury := __SETCENTURY()
local cYear

//   default dDate to date()
   if valtype (ddate) != "D"
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
   
   return cDate

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbdate.h"

BOOL ct_isleap(int iYear)
{
   return ( ( iYear % 4 == 0 && iYear % 100 != 0 ) || iYear % 400 == 0 );
}

int ct_daysinmonth(int iMonth, BOOL bLeap)
{
   if( iMonth == 2)
   {
      return( bLeap ? 29 : 28 );
   }
   else if( iMonth == 4 || iMonth == 6 || iMonth == 9 || iMonth == 11 )
   {
      return( 30 );
   }
   else
   {
      return( 31 );
   }
}

int ct_daystomonth(int iMonth, BOOL bLeap)
{
   int iMonthes[] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

   return( (iMonth < 1 && iMonth > 12) ? 0 : iMonthes[iMonth - 1] +
           ((bLeap && iMonth > 2) ? 1 : 0) );
}

LONG ct_doy( LONG lDate )
{
   int iYear, iMonth, iDay;
   LONG lFirst;

   hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
   lFirst = hb_dateEncode(iYear, 1, 1);
   return( lDate - lFirst + 1 );
}

HB_FUNC( ADDMONTH )
{
   int iYear, iMonth, iDay, iNum, iDays;

   if( ISDATE( 1 ) )
   {
      PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );
      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      iNum = hb_parni( 2 );
   }
   else if( ISNUM(1) )
   {
      iNum = hb_parni( 1 );
      hb_dateToday( &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_retdl( 0 );
      return;
   }

   iMonth += iNum;
   while( iMonth <= 0)
   {
      iMonth += 12;
      iYear --;
   }
   while( iMonth > 12)
   {
      iMonth -= 12;
      iYear ++;
   }

   iDays = ct_daysinmonth( iMonth, ct_isleap( iYear ) );
   if( iDay > iDays )
   {
      iDay = iDays;
   }

   hb_retd( iYear, iMonth, iDay);
}

HB_FUNC( DOY )
{
   LONG lDate;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      lDate = hb_itemGetDL( pDate );
   }
   else
   {
      int iYear, iMonth, iDay;
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode(iYear, iMonth, iDay);
   }

   hb_retnl( ct_doy(lDate) );
}

HB_FUNC( ISLEAP )
{
   int iYear, iMonth, iDay;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate && hb_itemGetDL( pDate ) )
   {
      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
   }

   hb_retl( ct_isleap( iYear ) );
}


HB_FUNC( DAYSTOMONTH )
{
   int iMonth = (ISNUM( 1 ) ? hb_parni(1) : 0 );
   BOOL bLeap = (ISLOG( 2 ) ? hb_parl(2) : 0 );

   hb_retni( ct_daystomonth(iMonth, bLeap) );
}

HB_FUNC( DAYSINMONTH )
{
   int iMonth = (ISNUM( 1 ) ? hb_parni(1) : 0 );
   BOOL bLeap = (ISLOG( 2 ) ? hb_parl(2) : 0 );

   hb_retni( ct_daysinmonth( iMonth, bLeap) );

}

HB_FUNC( QUARTER )
{
   int iYear, iMonth, iDay;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      if( hb_itemGetDL( pDate ) )
      {
         hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      }
      else
      {
         hb_retni(0);
         return;
      }
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
   }

   hb_retni( (iMonth+2) / 3);
}

HB_FUNC( LASTDAYOM )
{
   BOOL bLeap = 0;
   int iYear, iMonth, iDay;

   if ( ISDATE(1) )
   {
      PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );
      LONG lDate = hb_itemGetDL( pDate );
      if( lDate )
      {
         hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      }
      else
      {
         hb_dateToday( &iYear, &iMonth, &iDay );
      }
      bLeap = ct_isleap( iYear );
   }
   else if( ISNUM(1) )
   {
      iMonth = hb_parni(1);
   }
   else
   {
      iMonth = 0;
   }

   hb_retni( (iMonth && (iMonth <= 12) ? ct_daysinmonth(iMonth, bLeap) : 0) );

}

HB_FUNC( NTOCDOW )
{
   hb_retc( hb_dateCDOW( hb_parni(1) ) );
}

HB_FUNC( NTOCMONTH )
{
   hb_retc( hb_dateCMonth( hb_parni(1) ) );
}

HB_FUNC( WEEK )
{
   int iYear, iMonth, iDay, iWeek;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );
   LONG lDate = 0;
   BOOL bSWN  = ( ISLOG(2) ? hb_parl(2) : 0);

   if( ISDATE(1) )
   {
      lDate = hb_itemGetDL( pDate );
      if( ! lDate )
      {
         hb_retni( 0 );
         return;
      }
   }

   if( lDate )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode(iYear, iMonth, iDay);
   }

   if( bSWN )
   {
      int iDays = ct_daystomonth( iMonth, ct_isleap( iYear )) + iDay;
      int iPart = (iDays % 7);

      iWeek = iDays / 7;
      if( iPart > 0 ) iWeek ++;
   }
   else
   {
      LONG lDate2 = lDate + 3 - ((hb_dateDOW( iYear, iMonth, iDay ) + 5) % 7);

      iWeek = (ct_doy(lDate2) - 1) / 7 + 1;
   }

   hb_retni( iWeek );
}

#pragma ENDDUMP
