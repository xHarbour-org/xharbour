/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HMonthCalendar class
 *
 * Copyright 2004 Marcos Antonio Gambeta <marcos_gambeta@hotmail.com>
 * www - http://geocities.yahoo.com.br/marcosgambeta/
*/

//--------------------------------------------------------------------------//

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

//--------------------------------------------------------------------------//

CLASS HMonthCalendar INHERIT HControl

   CLASS VAR winclass   INIT MONTHCAL_CLASS

   DATA value
   DATA bChange

   METHOD New( oWndParent, nId, vari, nStyle, nLeft, nTop, nWidth, nHeight, ;
               oFont, bInit, bChange, cTooltip, lNoToday, lNoTodayCircle, ;
               lWeekNumbers )
   METHOD Activate()
   METHOD Init()
   METHOD SetValue( dValue )
   METHOD GetValue()

ENDCLASS

//--------------------------------------------------------------------------//

METHOD New( oWndParent, nId, vari, nStyle, nLeft, nTop, nWidth, nHeight, ;
            oFont, bInit, bChange, cTooltip, lNoToday, lNoTodayCircle, ;
            lWeekNumbers ) CLASS HMonthCalendar

   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil, ::NewId(), nId )
   ::value   := Iif( Valtype(vari)=="D" .And. !Empty(vari), vari, Date() )
   ::style   := Hwg_BitOr( Iif( nStyle==Nil, 0, nStyle ), WS_CHILD+WS_VISIBLE+WS_TABSTOP )
   ::style   := ::style + Iif( lNoToday      , MCS_NOTODAY      , 0 )
   ::style   := ::style + Iif( lNoTodayCircle, MCS_NOTODAYCIRCLE, 0 )
   ::style   := ::style + Iif( lWeekNumbers  , MCS_WEEKNUMBERS  , 0 )
   ::oFont   := Iif( oFont==Nil, ::oParent:oFont, oFont )
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bChange := bChange
   ::tooltip := cTooltip

   HWG_InitCommonControlsEx()
   ::Activate()
   ::oParent:AddControl( Self )

   If bChange != Nil
      ::oParent:AddEvent( MCN_SELECT, ::id, bChange, .T. )
   EndIf

Return Self

//--------------------------------------------------------------------------//

METHOD Activate CLASS HMonthCalendar

   If ::oParent:handle != 0
      ::handle := InitMonthCalendar ( ::oParent:handle, ::id, ::style, ;
                  ::nLeft, ::nTop, ::nWidth, ::nHeight, ::oFont:handle )
      ::Init()
   EndIf

Return Nil

//--------------------------------------------------------------------------//

METHOD Init() CLASS HMonthCalendar

   If !::lInit
      Super:Init()
      If !Empty( ::value )
         SetMonthCalendarDate( ::handle , ::value )
      EndIf
   EndIf

Return Nil

//--------------------------------------------------------------------------//

METHOD SetValue( dValue ) CLASS HMonthCalendar

   If Valtype(dValue)=="D" .And. !Empty(dValue)
      SetMonthCalendarDate( ::handle, dValue )
      ::value := dValue
   EndIf

Return Nil

//--------------------------------------------------------------------------//

METHOD GetValue() CLASS HMonthCalendar

   ::value := GetMonthCalendarDate( ::handle )

Return (::value)

//--------------------------------------------------------------------------//

#pragma BEGINDUMP

#define _WIN32_IE      0x0500
#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbdate.h"

HB_FUNC ( INITMONTHCALENDAR )
{
   HWND hMC;
   RECT rc;

   hMC = CreateWindowEx( 0,
                         MONTHCAL_CLASS,
                         "",
                         (LONG) hb_parnl(3),
                         0,0,0,0,
                         (HWND) hb_parnl(1),
                         (HMENU) hb_parni(2),
                         GetModuleHandle(NULL),
                         NULL );

	SendMessage( hMC, (UINT) WM_SETFONT, (WPARAM) (HFONT) hb_parnl(8), 1 );

   MonthCal_GetMinReqRect( hMC, &rc );

   SetWindowPos( hMC, NULL, hb_parni(4), hb_parni(5), rc.right, rc.bottom, SWP_NOZORDER );

	hb_retnl( (LONG) hMC );
}

HB_FUNC ( SETMONTHCALENDARDATE ) // adaptation of function SetDatePicker of file Control.c
{
   PHB_ITEM pDate = hb_param( 2, HB_IT_DATE );

   if( pDate )
   {
      SYSTEMTIME sysTime;
      long lYear, lMonth, lDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &lYear, &lMonth, &lDay );

      sysTime.wYear = (unsigned short) lYear;
      sysTime.wMonth = (unsigned short) lMonth;
      sysTime.wDay = (unsigned short) lDay;
      sysTime.wDayOfWeek = 0;
      sysTime.wHour = 0;
      sysTime.wMinute = 0;
      sysTime.wSecond = 0;
      sysTime.wMilliseconds = 0;

      MonthCal_SetCurSel( (HWND) hb_parnl (1), &sysTime);

   }
}

HB_FUNC ( GETMONTHCALENDARDATE ) // adaptation of function GetDatePicker of file Control.c
{
   SYSTEMTIME st;
   char szDate[9];

   SendMessage( (HWND) hb_parnl (1), MCM_GETCURSEL, 0, (LPARAM) &st);

   hb_dateStrPut( szDate, st.wYear, st.wMonth, st.wDay );
   szDate[8] = 0;
   hb_retds( szDate );
}

#pragma ENDDUMP

