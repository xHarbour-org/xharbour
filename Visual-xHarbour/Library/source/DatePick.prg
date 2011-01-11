/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// DatePick.prg                                                                                             *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#Include 'inkey.ch'
#Include 'vxh.ch'

//-----------------------------------------------------------------------------------------------

CLASS DateTimePicker INHERIT Control

   DATA OnDTNCloseUp          EXPORTED
   DATA OnDTNDateTimeChange   EXPORTED
   DATA OnDTNDropDown         EXPORTED
   DATA OnDTNFormat           EXPORTED
   DATA OnDTNFormatQuery      EXPORTED
   DATA OnDTNUserString       EXPORTED
   DATA OnDTNWMKeyDown        EXPORTED
   DATA OnDTNKillFocus        EXPORTED
   DATA OnDTNSetFocus         EXPORTED

   DATA MinRange              EXPORTED
   DATA MaxRange              EXPORTED

   DATA MinDate               EXPORTED
   DATA MinTime               EXPORTED
   DATA MaxDate               EXPORTED
   DATA MaxTime               EXPORTED

   DATA SmallCaption          PROTECTED INIT .F.
   ACCESS Caption              INLINE    IIF( ! ::IsWindow() .OR. ::__IsInstance, ::xCaption, _GetWindowText( ::hWnd ) )

   DATA xDate                 EXPORTED  INIT DATE()
   ACCESS Date                INLINE    ::xDate PERSISTENT
   ASSIGN Date(d)             INLINE    ::xDate := d, ::SetSystemTime()

   DATA xTime                 EXPORTED  INIT TIME()
   ACCESS Time                INLINE    ::xTime PERSISTENT
   ASSIGN Time(c)             INLINE    ::xTime := c, ::SetSystemTime()

   PROPERTY UpDown     INDEX DTS_UPDOWN      READ xUpDown       WRITE SetStyle  DEFAULT .F. PROTECTED
   PROPERTY Parse      INDEX DTS_APPCANPARSE READ xParse        WRITE SetStyle  DEFAULT .F. PROTECTED
   PROPERTY Format                           READ xFormat       WRITE SetFormat DEFAULT __GetSystem():DateTimeFormat:Short  PROTECTED
   PROPERTY CustomFormat                     READ xCustomFormat WRITE SetCustomFormat       PROTECTED
   PROPERTY Border       INDEX WS_BORDER      READ xBorder     WRITE SetStyle  DEFAULT .F.  PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD GetCalendarColor( nPos )         INLINE ::SendMessage( DTM_GETMCCOLOR, nPos, 0 )
   METHOD GetCalendarFont()                INLINE ::SendMessage( DTM_GETMCFONT, 0, 0 )
   METHOD GetCalendarHandle()              INLINE ::SendMessage( DTM_GETMONTHCAL, 0, 0 )
   METHOD GetRange()                       INLINE DateTime_GetRange( ::hWnd )
   METHOD SetRange()
   METHOD GetSystemTime()
   METHOD SetSystemTime()

   METHOD SetFormat()
   METHOD SetCustomFormat()

   METHOD SetCalendarColor( nPos, nColor ) INLINE ::SendMessage( DTM_SETMCCOLOR, nPos, nColor )
   METHOD SetCalendarFont( nPos, hFont )   INLINE ::SendMessage( DTM_SETMCFONT, nPos, hFont )

   METHOD SetBkColor( nColor )             INLINE ::SetCalendarColor( MCSC_TITLEBK,   nColor )
   METHOD SetTextColor( nColor )           INLINE ::SetCalendarColor( MCSC_TEXT,      nColor )
   METHOD SetTitleTextColor( nColor )      INLINE ::SetCalendarColor( MCSC_TITLETEXT, nColor )
   METHOD SetTitleBackColor( nColor )      INLINE ::SetCalendarColor( MCSC_TITLEBK,   nColor )
   METHOD SetMonthBackColor( nColor )      INLINE ::SetCalendarColor( MCSC_MONTHBK,   nColor )
   METHOD SetTrailingTextColor( nColor )   INLINE ::SetCalendarColor( MCSC_TRAILINGTEXT, nColor )

   METHOD OnParentNotify()
   METHOD OnCloseUp()         VIRTUAL
   METHOD OnDateTimeChange()  VIRTUAL
   METHOD OnDropDown          VIRTUAL
   METHOD OnFormat            VIRTUAL
   METHOD OnFormatQuery       VIRTUAL
   METHOD OnUserString        VIRTUAL
   METHOD OnWMKeyDown         VIRTUAL
   METHOD OnKillFocus         VIRTUAL
   METHOD OnSetFocus          VIRTUAL

ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS DateTimePicker
   InitCommonControlsEx( ICC_DATE_CLASSES )
   ::xHeight := 20
   ::xWidth  := 90
   DEFAULT ::__xCtrlName TO "DateTimePicker"
   ::ThemeName  := "combobox"
   ::ClsName    := DATETIMEPICK_CLASS
   ::Super:Init( oParent )
   ::Style     := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::MinRange  := SysTime()
   ::MaxRange  := SysTime()

   ::Events := {}
   IF oParent:__ClassInst != NIL
      ::Events := { ;
                  {"General",     {;
                                  { "OnInit"            , "", "" },;
                                  { "OnCreate"          , "", "" },;
                                  { "OnCloseUp"         , "", "" },;
                                  { "OnDateTimeChange"  , "", "" } } },;
                  {"Keyboard",    {;
                                  { "OnKeyDown"         , "", "" } } } }
   ENDIF

RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS DateTimePicker
   LOCAL aRange
   ExecuteEvent( "OnInit", Self )

   ::Super:Create()
   aRange := ::GetRange()

   IF ::Format != NIL
      ::SetFormat( ::Format )
   ENDIF
   ::MinRange:Year         := aRange[1][1]
   ::MinRange:Month        := MAX( aRange[1][2], 1 )
   ::MinRange:DayOfWeek    := MAX( aRange[1][3], 1 )
   ::MinRange:Day          := MAX( aRange[1][4], 1 )
   ::MinRange:Hour         := aRange[1][5]
   ::MinRange:Minute       := aRange[1][6]
   ::MinRange:Second       := aRange[1][7]
   ::MinRange:Milliseconds := aRange[1][8]

   ::MaxRange:Year         := aRange[2][1]
   ::MaxRange:Month        := MAX( aRange[2][2], 1 )
   ::MaxRange:DayOfWeek    := MAX( aRange[2][3], 1 )
   ::MaxRange:Day          := MAX( aRange[2][4], 1 )
   ::MaxRange:Hour         := aRange[2][5]
   ::MaxRange:Minute       := aRange[2][6]
   ::MaxRange:Second       := aRange[2][7]
   ::MaxRange:Milliseconds := aRange[2][8]

   ::MinDate := STOD( STRZERO(::MinRange:Year,4) + STRZERO(::MinRange:Month,2) + STRZERO(::MinRange:Day,2) )
   ::MaxDate := STOD( STRZERO(::MaxRange:Year,4) + STRZERO(::MaxRange:Month,2) + STRZERO(::MaxRange:Day,2) )

   ::MinTime := STRZERO(::MinRange:Hour,2) +":"+ STRZERO(::MinRange:Minute,2) +":"+ STRZERO(::MinRange:Second,2)
   ::MaxTime := STRZERO(::MinRange:Hour,2) +":"+ STRZERO(::MinRange:Minute,2) +":"+ STRZERO(::MinRange:Second,2)

   ::SetSystemTime()
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD GetSystemTime( nIndex ) CLASS DateTimePicker
   LOCAL st := (struct SYSTEMTIME)
   SendMessage( ::hWnd, DTM_GETSYSTEMTIME, 0, @st )
   IF nIndex == 1
      RETURN STOD( STRZERO(st:wYear,4) + STRZERO(st:wMonth,2) + STRZERO(st:wDay,2) )
    ELSE
      RETURN STRZERO(st:wHour,2) +":"+ STRZERO(st:wMinute,2) +":"+ STRZERO(st:wSecond,2)
   ENDIF
RETURN st

//-----------------------------------------------------------------------------------------------

METHOD SetSystemTime() CLASS DateTimePicker
   LOCAL st := (struct SYSTEMTIME)

   st:wYear         := YEAR( ::xDate )
   st:wMonth        := MONTH( ::xDate )
   st:wDayOfWeek    := DOW( ::xDate )
   st:wDay          := DAY( ::xDate )

   st:wHour         := VAL( SUBSTR( ::xTime, 1, 2 ) )
   st:wMinute       := VAL( SUBSTR( ::xTime, 4, 2 ) )
   st:wSecond       := VAL( SUBSTR( ::xTime, 7, 2 ) )
   st:wMilliseconds := 0
   SendMessage( ::hWnd, DTM_SETSYSTEMTIME, GDT_VALID, st )
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetRange( nMinMax ) CLASS DateTimePicker
   LOCAL MinSt, MaxSt, aSysTime
   DEFAULT nMinMax TO GDTR_MIN+GDTR_MAX
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

   DateTime_SetRange( ::hWnd, nMinMax, MinSt:Value, MaxSt:Value )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetCustomFormat( cCustomFormat ) CLASS DateTimePicker
   IF ::xFormat == 20
      ::SendMessage( DTM_SETFORMAT, 0, cCustomFormat )
    ELSE
      cCustomFormat := ::xCustomFormat
   ENDIF
RETURN Self

METHOD SetFormat( nFormat ) CLASS DateTimePicker
   LOCAL cFormat, xValue
   IF nFormat < 20
      IF ::xFormat == 20
         ::SendMessage( DTM_SETFORMAT, 0, NIL )
      ENDIF
      FOR EACH xValue IN ::System:DateTimeFormat:Values
          ::Style := ::Style & NOT( xValue )
      NEXT
      ::Style := ::Style | nFormat

      IF ::IsWindow()
         ::SetWindowLong( GWL_STYLE, ::Style )
         ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
         ::RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
      ENDIF
    ELSEIF ::xCustomFormat != NIL
      ::SendMessage( DTM_SETFORMAT, 0, ::xCustomFormat )
   ENDIF
RETURN Self


METHOD OnParentNotify( nwParam, nlParam ) CLASS DateTimePicker
   LOCAL nRet, nmd, dDate, cTime, st
   DO CASE
      CASE ::Parent:hdr:code == DTN_CLOSEUP
           nRet := ::OnCloseUp( ::Parent:hdr, nlParam )
           nRet := __Evaluate( ::Action, Self, ::Parent:hdr, nlParam, nRet )
           nRet := ExecuteEvent( "OnCloseUp", Self )

      CASE ::Parent:hdr:code == DTN_DATETIMECHANGE
           nmd := (struct NMDATETIMECHANGE*) nlParam
           nRet := ::OnDateTimeChange( nmd, nlParam )
           nRet := __Evaluate( ::OnDTNDateTimeChange, Self, nmd, nlParam, nRet )
           nRet := ExecuteEvent( "OnDateTimeChange", Self )

           ::xDate := STOD( STRZERO(nmd:st:wYear,4) + STRZERO(nmd:st:wMonth,2) + STRZERO(nmd:st:wDay,2) )
           ::xTime := STRZERO(nmd:st:wHour,2) +":"+ STRZERO(nmd:st:wMinute,2) +":"+ STRZERO(nmd:st:wSecond,2)
/*
           IF ::MinDate != NIL .AND. VAL(DTOS(dDate)) <= VAL(DTOS(::MinDate))

              st := (struct SYSTEMTIME)
              st:wYear  := YEAR( ::MinDate )
              st:wMonth := MONTH( ::MinDate )
              st:wDay   := DAY( ::MinDate )

              st:wDayOfWeek    := nmd:st:wDayOfWeek
              st:wHour         := nmd:st:wHour
              st:wMinute       := nmd:st:wMinute
              st:wSecond       := nmd:st:wSecond
              st:wMilliseconds := nmd:st:wMilliseconds
              Application:Yield()
              RETURN ::SetSystemTime( st )

            ELSEIF ::MaxDate != NIL .AND. dDate > ::MaxDate

              st := (struct SYSTEMTIME)
              st:wYear  := YEAR( ::MaxDate )
              st:wMonth := MONTH( ::MaxDate )
              st:wDay   := DAY( ::MaxDate )

              st:wDayOfWeek    := nmd:st:wDayOfWeek
              st:wHour         := nmd:st:wHour
              st:wMinute       := nmd:st:wMinute
              st:wSecond       := nmd:st:wSecond
              st:wMilliseconds := nmd:st:wMilliseconds
              ::SetSystemTime( st )
              ::SetSystemTime( st )
           ENDIF
*/

      CASE ::Parent:hdr:code == DTN_DROPDOWN
           nRet := ::OnDropDown( ::Parent:hdr, nlParam )
           nRet := __Evaluate( ::OnDTNDropDown, Self, ::Parent:hdr, nlParam, nRet )

      CASE ::Parent:hdr:code == DTN_FORMAT
           nmd := (struct NMDATETIMEFORMAT*) nlParam
           nRet := ::OnFormat( nmd, nlParam )
           nRet := __Evaluate( ::OnDTNFormat, Self, nmd, nlParam, nRet )

      CASE ::Parent:hdr:code == DTN_FORMATQUERY
           nmd := (struct NMDATETIMEFORMATQUERY*) nlParam
           nRet := ::OnFormatQuery( nmd, nlParam )
           nRet := __Evaluate( ::OnDTNFormatQuery, Self, nmd, nlParam, nRet )

      CASE ::Parent:hdr:code == DTN_USERSTRING
           nmd := (struct NMDATETIMESTRING*) nlParam
           nRet := ::OnUserString( nmd, nlParam )
           nRet := __Evaluate( ::OnDTNUserString, Self, nmd, nlParam, nRet )

      CASE ::Parent:hdr:code == DTN_WMKEYDOWN
           nmd := (struct NMDATETIMEWMKEYDOWN*) nlParam
           nRet := ::OnWMKeyDown( nmd, nlParam )
           nRet := __Evaluate( ::OnDTNWMKeyDown, Self, nmd, nlParam, nRet )

      CASE ::Parent:hdr:code == NM_KILLFOCUS
           nRet := ::OnKillFocus( ::Parent:hdr, nlParam )
           nRet := __Evaluate( ::OnDTNKillFocus, Self, ::Parent:hdr, nlParam, nRet )

      CASE ::Parent:hdr:code == NM_SETFOCUS
           nRet := ::OnSetFocus( ::Parent:hdr, nlParam )
           nRet := __Evaluate( ::OnDTNSetFocus, Self, ::Parent:hdr, nlParam, nRet )

   ENDCASE
RETURN NIL

//-----------------------------------------------------------------------------------------------

CLASS SysTime
   DATA Year          EXPORTED
   DATA Month         EXPORTED
   DATA DayOfWeek     EXPORTED
   DATA Day           EXPORTED
   DATA Hour          EXPORTED
   DATA Minute        EXPORTED
   DATA Second        EXPORTED
   DATA Milliseconds  EXPORTED
ENDCLASS


//-----------------------------------------------------------------------------------------------
