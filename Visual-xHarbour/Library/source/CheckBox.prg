/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// CheckBox.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

#define BP_PUSHBUTTON            1
#define BP_RADIOBUTTON           2
#define BP_CHECKBOX              3
#define BP_GROUPBOX              4
#define BP_USERBUTTON            5

#define CBS_UNCHECKEDNORMAL      1
#define CBS_UNCHECKEDHOT         2
#define CBS_UNCHECKEDPRESSED     3
#define CBS_UNCHECKEDDISABLED    4
#define CBS_CHECKEDNORMAL        5
#define CBS_CHECKEDHOT           6
#define CBS_CHECKEDPRESSED       7
#define CBS_CHECKEDDISABLED      8
#define CBS_MIXEDNORMAL          9
#define CBS_MIXEDHOT            10
#define CBS_MIXEDPRESSED        11
#define CBS_MIXEDDISABLED       12

#define PBS_NORMAL       1
#define PBS_HOT          2
#define PBS_PRESSED      3
#define PBS_DISABLED     4
#define PBS_DEFAULTED    5


#define TMT_BTNTEXT   1619
//-----------------------------------------------------------------------------------------------

CLASS CheckBox INHERIT Control
   DATA ImageList  EXPORTED
   DATA ImageIndex PROTECTED
   DATA Transparent PUBLISHED INIT .T.

   PROPERTY Group      INDEX WS_GROUP  READ xGroup      WRITE SetStyle      PROTECTED DEFAULT .F.
   PROPERTY CheckStyle                 READ xCheckStyle WRITE SetCheckStyle PROTECTED DEFAULT 1
   PROPERTY State                      READ xState      WRITE SetState      PROTECTED DEFAULT BST_UNCHECKED
   PROPERTY Border     INDEX WS_BORDER READ xBorder     WRITE SetStyle      PROTECTED DEFAULT .F. 

   DATA ImageIndex
   DATA DefaultButton  EXPORTED INIT .F.
   DATA Check_Styles   EXPORTED INIT { "AutoCheckBox", "Auto3State" }
   DATA EnumState      EXPORTED INIT {{ "Unchecked", "Checked", "Indeterminate" }, {BST_UNCHECKED,BST_CHECKED,BST_INDETERMINATE} }


   METHOD Init()           CONSTRUCTOR
   METHOD SetParent( oParent ) INLINE IIF( ::__hBrush != NIL, ( DeleteObject( ::__hBrush ), ::__hBrush := NIL ), ), ::Super:SetParent( oParent ), ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   METHOD Create()             INLINE ::Super:Create(), ::SetState( ::xState )

   METHOD OnDestroy()          INLINE IIF( ::__hBrush != NIL, DeleteObject( ::__hBrush ), ), NIL
   METHOD __WindowDestroy()    INLINE ::Super:__WindowDestroy(), ::CloseThemeData(), Self
   METHOD DrawFrame()
   METHOD SetCheckStyle()
   METHOD OnCtlColorStatic()
   METHOD OnParentNotify()
   METHOD SetState()
   METHOD OnEraseBkGnd() INLINE 1
   METHOD Checked() INLINE ( ::SendMessage( BM_GETSTATE, 0, 0 ) & BST_CHECKED ) != 0
   METHOD Check()   INLINE ::State := BST_CHECKED
   METHOD UnCheck() INLINE ::State := BST_UNCHECKED
   METHOD Indeterminate() INLINE ::State := BST_INDETERMINATE
ENDCLASS

METHOD Init( oParent ) CLASS CheckBox
   DEFAULT ::__xCtrlName TO "CheckBox"
   DEFAULT ::Style TO WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_AUTOCHECKBOX | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ThemeName := "button"
   ::ClsName := "button"
   ::Super:Init( oParent )
   ::Width  := 100
   ::Height := 15
   ::OpenThemeData()
   IF ::__ClassInst != NIL
      ::__PropFilter := { "HIGHLIGHTCAPTION", "SMALLCAPTION", "ALLOWMAXIMIZE" }
   ENDIF
RETURN Self

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS CheckBox
   LOCAL nRet, cd, aRect, lDisabled, lSelected, lFocus, nColor
   LOCAL sz, nStatus, hBkGnd, nFlags := DFCS_BUTTONCHECK
   (nwParam)
   DO CASE
      CASE hdr:code==NM_CUSTOMDRAW
           nRet := CDRF_DODEFAULT
           cd := (struct NMCUSTOMDRAW *) nlParam

           DO CASE
              CASE cd:dwDrawStage == CDDS_PREERASE
              CASE cd:dwDrawStage == CDDS_PREPAINT
                   lDisabled := cd:uItemState & CDIS_DISABLED != 0
                   lSelected := cd:uItemState & CDIS_SELECTED != 0
                   lFocus    := cd:uItemState & CDIS_FOCUS != 0

                   IF lDisabled
                      SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, CDRF_DODEFAULT )
                      RETURN CDRF_DODEFAULT
                   ENDIF
                   nColor := NIL
                   IF ::ForeColor != NIL .AND. !( ::ForeColor == ::ForeSysColor )
                      nColor := SetTextColor( cd:hDC, ::ForeColor )
                   ENDIF
                   SetBkMode( cd:hdc, TRANSPARENT )
                   
                   IF ::Parent:ClsName == "GroupBox"
                      hBkGnd := ::BkBrush
                      DEFAULT hBkGnd TO ::__hBrush
                      DEFAULT hBkGnd TO ::Parent:BkBrush
                      FillRect( cd:hDC, cd:rc, hBkGnd )

                      aRect := {0,0,15,15}

                      nStatus := ::SendMessage( BM_GETCHECK, 0, 0 )

                      DO CASE
                         CASE nStatus == BST_UNCHECKED
                              nStatus := 0
                              IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                                 nStatus := CBS_UNCHECKEDNORMAL
                              ENDIF

                         CASE nStatus == BST_CHECKED
                              nStatus := DFCS_CHECKED
                              IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                                 nStatus := CBS_CHECKEDNORMAL
                              ENDIF

                         CASE nStatus == BST_INDETERMINATE
                              nStatus := DFCS_BUTTON3STATE+DFCS_CHECKED
                              IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                                 nStatus := CBS_MIXEDNORMAL
                              ENDIF

                      ENDCASE
                      nFlags := nFlags | nStatus

                      IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                         DrawThemeBackground( ::hTheme, cd:hDC, BP_CHECKBOX, nStatus, aRect, aRect )
                       ELSE
                         _DrawFrameControl( cd:hDC, aRect, DFC_BUTTON, nFlags )
                      ENDIF
                   ENDIF
                   cd:rc:left += 16

                   DrawText( cd:hDC, ::Caption, cd:rc, DT_LEFT | DT_VCENTER | DT_SINGLELINE )
                   IF nColor != NIL
                      SetTextColor( cd:hDC, nColor )
                   ENDIF
                   IF lFocus
                      SetBkMode( cd:hdc, OPAQUE )
                      SetBkColor( cd:hdc, RGB(0,0,0))
                      SetTextColor( cd:hdc, RGB(255,255,255))

                      sz := (struct SIZE)
                      GetTextExtentPoint32( cd:hDC, ::Caption, @sz )

                      cd:rc:left -= 2
                      cd:rc:right  := sz:cx + cd:rc:left + 4
                      cd:rc:top    := ( ::Height - sz:cy ) / 2
                      cd:rc:bottom := sz:cy + cd:rc:top

                      DrawFocusRect( cd:hDC, cd:rc )
                   ENDIF
                   nRet := CDRF_SKIPDEFAULT
           ENDCASE
           SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, nRet )
           RETURN nRet

   ENDCASE
RETURN NIL

METHOD DrawFrame( oDrawing, aRect, nAlign, nWidth, nHeight, nStatus, lDraw ) CLASS CheckBox
   LOCAL nFlags := DFCS_BUTTONCHECK
   DEFAULT lDraw TO TRUE
   IF nStatus != NIL
      DO CASE
         CASE nStatus == BST_UNCHECKED
              nStatus := 0
              IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                 nStatus := CBS_UNCHECKEDNORMAL
              ENDIF

         CASE nStatus == BST_CHECKED
              nStatus := DFCS_CHECKED
              IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                 nStatus := CBS_CHECKEDNORMAL
              ENDIF

         CASE nStatus == BST_INDETERMINATE
              nStatus := DFCS_BUTTON3STATE+DFCS_CHECKED
              IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                 nStatus := CBS_MIXEDNORMAL
              ENDIF

      ENDCASE
      nFlags := nFlags | nStatus
   ENDIF
   DO CASE
      CASE nAlign == DT_LEFT
           aRect[3] := aRect[1]+nWidth
      CASE nAlign == DT_RIGHT
           aRect[1] := aRect[3]-nWidth-1
           aRect[3]--
   ENDCASE
   DEFAULT nHeight TO ::Height
   nHeight := MIN( nHeight, ::Height )

   aRect[2] := aRect[2] + (( aRect[4]-aRect[2]-nHeight )/2)//-1
   aRect[4] := aRect[2] + nHeight -1
   IF lDraw
      IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
         oDrawing:DrawThemeBackground( ::hTheme, BP_CHECKBOX, nStatus, aRect, aRect )
       ELSE
         oDrawing:DrawFrameControl( aRect, DFC_BUTTON, nFlags )
      ENDIF
   ENDIF
RETURN aRect

METHOD SetCheckStyle( nStyle ) CLASS CheckBox
   LOCAL nCurStyle
   ::Style := ::Style & NOT( BS_AUTOCHECKBOX ) & NOT( BS_AUTO3STATE )
   SWITCH nStyle
      CASE 1
          nCurStyle := BS_AUTOCHECKBOX
          EXIT
      CASE 2
          nCurStyle := BS_AUTO3STATE
          EXIT
   END
   ::Style := ::Style | BS_CHECKBOX | nCurStyle
   IF ::hWnd != NIL
      SetWindowLong( ::hWnd, GWL_STYLE, ::Style )
      ::SetState( ::xState )
   ENDIF
RETURN Self

METHOD OnCtlColorStatic( nwParam ) CLASS CheckBox
   LOCAL nBack, hBkGnd := ::BkBrush
   DEFAULT hBkGnd TO ::__hBrush
   DEFAULT hBkGnd TO ::Parent:BkBrush

   IF ::ForeColor != NIL
      SetTextColor( nwParam, ::ForeColor )
   ENDIF
   
   nBack := ::BackColor
   DEFAULT nBack TO ::Parent:BackColor
   IF nBack != NIL
      SetBkColor( nwParam, nBack )
   ENDIF
   SetBkMode( nwParam, TRANSPARENT )
RETURN hBkGnd

METHOD SetState( nState ) CLASS CheckBox
   // Patch
   //IF ::__ClassInst != NIL
   //   ::__ClassInst:xState := ::xState
   //ENDIF
   //::xState := nState
   IF ::IsWindow()
      ::SendMessage( BM_SETCHECK, nState, 0 )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
