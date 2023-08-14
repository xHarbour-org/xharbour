/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// MonthCalendar.prg                                                                                             *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#ifdef VXH_PROFESSIONAL

#include "debug.ch"
#Include 'inkey.ch'
#Include 'vxh.ch'

//-----------------------------------------------------------------------------------------------

CLASS MonthCalendar INHERIT TitleControl

   PROPERTY DayState          SET ::SetStyle( MCS_DAYSTATE, v )      DEFAULT .F.
   PROPERTY MultiSelect       SET ::SetStyle( MCS_MULTISELECT, v )   DEFAULT .F.
   PROPERTY NoToday           SET ::SetStyle( MCS_NOTODAY, v )       DEFAULT .F.
   PROPERTY NoTodayCircle     SET ::SetStyle( MCS_NOTODAYCIRCLE, v ) DEFAULT .F.
   PROPERTY WeekNumbers       SET ::SetStyle( MCS_WEEKNUMBERS, v )   DEFAULT .F.
   PROPERTY Date              SET ::SetCurSel(v)                     DEFAULT DATE()
   PROPERTY Today             SET ::SetToday(v)                      DEFAULT DATE()
   PROPERTY BackColor         ROOT "Colors" SET ::SetCalendarColor( MCSC_MONTHBK, v )
   PROPERTY ForeColor         ROOT "Colors" SET ::SetCalendarColor( MCSC_TEXT, v )
   PROPERTY TitleBackColor    SET ::SetCalendarColor( MCSC_TITLEBK, v )
   PROPERTY TitleForeColor    SET ::SetCalendarColor( MCSC_TITLETEXT, v )
   PROPERTY TrailingTextColor SET ::SetCalendarColor( MCSC_TRAILINGTEXT, v )

   DATA NmDayState   EXPORTED
   DATA NmSelChange  EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnParentNotify()
   METHOD SetCurSel()
   METHOD SetRange()
   METHOD SetToday()
   METHOD SetCalendarColor( nPos, nColor ) INLINE ::SendMessage( MCM_SETCOLOR, nPos, nColor )
   METHOD GetCalendarColor( nPos ) INLINE ::SendMessage( MCM_GETCOLOR, nPos, 0 )
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS MonthCalendar
   InitCommonControlsEx( ICC_DATE_CLASSES )
   ::xHeight := 0
   ::xWidth  := 0
   DEFAULT ::__xCtrlName TO "MonthCalendar"
   ::ClsName    := MONTHCAL_CLASS
   ::Super:Init( oParent )
   ::Style     := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Events := {}
   IF oParent:DesignMode
      ::Events := { ;
                  {"General",     {;
                                  { "OnInit"            , "", "" },;
                                  { "OnCreate"          , "", "" },;
                                  { "OnViewChange"      , "", "" },;
                                  { "OnSelect"          , "", "" },;
                                  { "OnSelChange"       , "", "" },;
                                  { "OnGetDayState"     , "", "" } } } }
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS MonthCalendar
   LOCAL rc := (struct RECT)
   ExecuteEvent( "OnInit", Self )
   ::Super:Create()
   IF ::xHeight == 0 .AND. ::xWidth == 0
      SendMessage( ::hWnd, MCM_GETMINREQRECT, 0, @rc )
      ::xWidth := rc:right
      ::xHeight := rc:bottom
      ::MoveWindow()
   ENDIF

   DEFAULT ::xBackColor         TO ::GetCalendarColor( MCSC_MONTHBK      )
   DEFAULT ::xForeColor         TO ::GetCalendarColor( MCSC_TEXT         )
   DEFAULT ::xTitleBackColor    TO ::GetCalendarColor( MCSC_TITLEBK      )
   DEFAULT ::xTitleForeColor    TO ::GetCalendarColor( MCSC_TITLETEXT    )
   DEFAULT ::xTrailingTextColor TO ::GetCalendarColor( MCSC_TRAILINGTEXT )

   ::SetCurSel( ::Date )
   ::SetToday( ::Today )

   ::BackColor         := ::xBackColor
   ::ForeColor         := ::xForeColor
   ::TitleBackColor    := ::xTitleBackColor
   ::TitleForeColor    := ::xTitleForeColor
   ::TrailingTextColor := ::xTrailingTextColor
   IF !EMPTY( ::Caption )
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER)
   ENDIF
RETURN Self

METHOD OnParentNotify( nwParam, nlParam ) CLASS MonthCalendar
   LOCAL nRet := 0
   (nwParam)
   DO CASE
      CASE ::Parent:hdr:code == MCN_GETDAYSTATE
           ::NmDayState := (struct NMDAYSTATE*) nlParam
           nRet := ExecuteEvent( "OnGetDayState", Self )

      CASE ::Parent:hdr:code == MCN_SELCHANGE
           ::NmSelChange := (struct NMSELCHANGE*) nlParam
           ::xDate := STOD( STRZERO(::NmSelChange:stSelStart:wYear,4) + STRZERO(::NmSelChange:stSelStart:wMonth,2) + STRZERO(::NmSelChange:stSelStart:wDay,2) )
           nRet := ExecuteEvent( "OnSelChange", Self )

      CASE ::Parent:hdr:code == MCN_SELECT
           ::NmSelChange := (struct NMSELCHANGE*) nlParam
           ::xDate := STOD( STRZERO(::NmSelChange:stSelStart:wYear,4) + STRZERO(::NmSelChange:stSelStart:wMonth,2) + STRZERO(::NmSelChange:stSelStart:wDay,2) )
           nRet := ExecuteEvent( "OnSelect", Self )

   ENDCASE
RETURN nRet

METHOD SetCurSel( dDate ) CLASS MonthCalendar
   LOCAL st := (struct SYSTEMTIME)
   IF ::hWnd != NIL
      st:wYear         := YEAR( dDate )
      st:wMonth        := MONTH( dDate )
      st:wDayOfWeek    := DOW( dDate )
      st:wDay          := DAY( dDate )

      ::SendMessage( MCM_SETCURSEL, 0, st )
   ENDIF
   ::xDate := dDate
RETURN Self

METHOD SetRange( nMinMax ) CLASS MonthCalendar
   LOCAL MinSt, MaxSt
   DEFAULT nMinMax TO GDTR_MIN | GDTR_MAX

   MinSt := (struct SYSTEMTIME)
   MaxSt := (struct SYSTEMTIME)

   MinSt:wYear         := ::MinRange:Year
   MinSt:wMonth        := ::MinRange:Month
   MinSt:wDayOfWeek    := ::MinRange:DayOfWeek
   MinSt:wDay          := ::MinRange:Day
   MinSt:wHour         := ::MinRange:Hour
   MinSt:wMinute       := ::MinRange:Minute
   MinSt:wSecond       := ::MinRange:Second
   MinSt:wMilliseconds := ::MinRange:Milliseconds

   MaxSt:wYear         := ::MaxRange:Year
   MaxSt:wMonth        := ::MaxRange:Month
   MaxSt:wDayOfWeek    := ::MaxRange:DayOfWeek
   MaxSt:wDay          := ::MaxRange:Day
   MaxSt:wHour         := ::MaxRange:Hour
   MaxSt:wMinute       := ::MaxRange:Minute
   MaxSt:wSecond       := ::MaxRange:Second
   MaxSt:wMilliseconds := ::MaxRange:Milliseconds

   MonthCal_SetRange( ::hWnd, nMinMax, MinSt:Value, MaxSt:Value )
RETURN Self

METHOD SetToday( dDate ) CLASS MonthCalendar
   LOCAL st := (struct SYSTEMTIME)
   IF ::hWnd != NIL
      st:wYear         := YEAR( dDate )
      st:wMonth        := MONTH( dDate )
      st:wDayOfWeek    := DOW( dDate )
      st:wDay          := DAY( dDate )

      ::SendMessage( MCM_SETTODAY, 0, st )
   ENDIF
   ::xToday := dDate
RETURN Self

#endif