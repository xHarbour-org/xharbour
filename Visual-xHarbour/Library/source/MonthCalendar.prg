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

CLASS MonthCalendar INHERIT Control

   PROPERTY DayState          INDEX MCS_DAYSTATE      READ xDayState          WRITE SetStyle         DEFAULT .F. PROTECTED
   PROPERTY MultiSelect       INDEX MCS_MULTISELECT   READ xMultiSelect       WRITE SetStyle         DEFAULT .F. PROTECTED
   PROPERTY NoToday           INDEX MCS_NOTODAY       READ xNoToday           WRITE SetStyle         DEFAULT .F. PROTECTED
   PROPERTY NoTodayCircle     INDEX MCS_NOTODAYCIRCLE READ xNoTodayCircle     WRITE SetStyle         DEFAULT .F. PROTECTED
   PROPERTY WeekNumbers       INDEX MCS_WEEKNUMBERS   READ xWeekNumbers       WRITE SetStyle         DEFAULT .F. PROTECTED

   PROPERTY Date                                      READ xDate              WRITE SetCurSel        DEFAULT DATE() PROTECTED
   PROPERTY Today                                     READ xToday             WRITE SetToday         DEFAULT DATE() PROTECTED

   PROPERTY BackColor         INDEX MCSC_MONTHBK      READ xBackColor         WRITE SetCalendarColor PROTECTED
   PROPERTY ForeColor         INDEX MCSC_TEXT         READ xForeColor         WRITE SetCalendarColor PROTECTED
   PROPERTY TitleBackColor    INDEX MCSC_TITLEBK      READ xTitleBackColor    WRITE SetCalendarColor PROTECTED
   PROPERTY TitleForeColor    INDEX MCSC_TITLETEXT    READ xTitleForeColor    WRITE SetCalendarColor PROTECTED
   PROPERTY TrailingTextColor INDEX MCSC_TRAILINGTEXT READ xTrailingTextColor WRITE SetCalendarColor PROTECTED
   PROPERTY Border            INDEX WS_BORDER         READ xBorder            WRITE SetStyle         DEFAULT .F. PROTECTED

   DATA xCaption               EXPORTED  INIT ""
   ACCESS Caption              INLINE    ::xCaption PERSISTENT
   ASSIGN Caption(c)           INLINE    ::xCaption := c, IIF( ::IsWindow(), ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER),)

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
   IF oParent:__ClassInst != NIL
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
   LOCAL st, nRet := 0
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
   LOCAL MinSt, MaxSt, aSysTime
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