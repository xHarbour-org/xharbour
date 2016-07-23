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
   PROPERTY TextAlignment ROOT "Appearance" SET ::Redraw(v)      DEFAULT DT_LEFT
   PROPERTY VertAlignment ROOT "Appearance" SET ::Redraw(v)      DEFAULT DT_VCENTER

   DATA ImageList   EXPORTED
   DATA ImageIndex  PROTECTED

   //PROPERTY Transparent ROOT "Appearance" SET ::__SetTransp(v)        DEFAULT .F.
   PROPERTY Group       ROOT "Behavior"   SET ::SetStyle(WS_GROUP,v)  DEFAULT .F.
   PROPERTY CheckStyle  ROOT "Behavior"   SET ::SetCheckStyle(v)      DEFAULT 1
   PROPERTY State       ROOT "Behavior"   SET ::SetState(v)           DEFAULT BST_UNCHECKED
   PROPERTY AutoSize    ROOT "Behavior"   SET ::__SetSize(v)          DEFAULT .F.
   PROPERTY RightAlign  ROOT "Appearance"                             DEFAULT .F.
   PROPERTY BoxSize     ROOT "Appearance"                             DEFAULT 15

   PROPERTY Checked     GET ::IsChecked() SET ::SetState(v)           DEFAULT .F.

   DATA ImageIndex
   DATA DefaultButton  EXPORTED INIT .F.

   DATA EnumCheckStyle    EXPORTED INIT {{ "AutoCheckBox", "Auto3State" }, {1,2} }
   DATA EnumState         EXPORTED INIT {{ "Unchecked", "Checked", "Indeterminate" }, {BST_UNCHECKED,BST_CHECKED,BST_INDETERMINATE} }
   DATA EnumTextAlignment EXPORTED INIT { { "Left", "Center", "Right" }, { DT_LEFT, DT_CENTER, DT_RIGHT } }
   DATA EnumVertAlignment EXPORTED INIT { { "Bottom", "Center", "Top" }, { DT_BOTTOM, DT_VCENTER, DT_TOP } }


   METHOD Init()           CONSTRUCTOR
   METHOD SetParent( oParent ) INLINE ::Super:SetParent( oParent ), ::RedrawWindow( , , ( RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW ) )
   METHOD Create()

   METHOD DrawFrame()
   METHOD SetCheckStyle()
   METHOD OnCtlColorStatic()
   METHOD OnParentNotify()
   METHOD SetState()
   METHOD OnEraseBkGnd()  INLINE 1
   METHOD GetState()      INLINE ::SendMessage( BM_GETSTATE, 0, 0 )
   METHOD Check()         INLINE ::State := BST_CHECKED
   METHOD UnCheck()       INLINE ::State := BST_UNCHECKED
   METHOD Indeterminate() INLINE ::State := BST_INDETERMINATE
   METHOD IsChecked()     INLINE ( ::SendMessage( BM_GETSTATE, 0, 0 ) & BST_CHECKED ) != 0

   METHOD __SetSize()
   //METHOD __SetTransp(lSet)    INLINE IIF( lSet, ::Parent:__RegisterTransparentControl( Self ), ::Parent:__UnregisterTransparentControl( Self ) )
   METHOD ResetFrame() INLINE    ::SetWindowPos(,0,0,0,0, ( SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER ) )
   METHOD OnParentCommand()
ENDCLASS

METHOD Init( oParent ) CLASS CheckBox
   DEFAULT ::__xCtrlName TO "CheckBox"
   DEFAULT ::Style TO ( WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_AUTOCHECKBOX | WS_CLIPCHILDREN | WS_CLIPSIBLINGS )
   ::ClsName := "button"
   ::Super:Init( oParent )
   ::Width  := 100
   ::Height := 16
   IF ::DesignMode
      ::__PropFilter := { "ALLOWMAXIMIZE" }
   ENDIF
   ::bSetValue := {|lChecked| ::State := IIF( lChecked, BST_CHECKED, BST_UNCHECKED )}
   ::bGetValue := {||::Checked()}
RETURN Self

METHOD __SetSize( lAuto ) CLASS CheckBox
   LOCAL aSize
   IF lAuto
      aSize := ::Drawing:GetTextExtentPoint32( ::Text )
      ::xWidth := aSize[1]+ :: BoxSize + 3
      ::MoveWindow()
      ::Redraw()
   ENDIF
RETURN Self

METHOD Create() CLASS CheckBox
   LOCAL aSize
   ::Super:Create()
   IF ::AutoSize
      aSize := ::Drawing:GetTextExtentPoint32( ::Text )
      ::Width := aSize[1]+ :: BoxSize + 3
      ::Redraw()
   ENDIF
   ::Height := MAX( 16, ::Height )
   ::SetState( ::xState )
RETURN Self

METHOD OnParentCommand() CLASS CheckBox
   LOCAL bChanged
   IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
      bChanged := ::Parent:bChanged
   ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
      bChanged := ::Form:bChanged
   ENDIF
   IF bChanged != NIL
      Eval( bChanged, Self )
   ENDIF
RETURN NIL

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS CheckBox
   LOCAL nRet, cd, aRect, lDisabled, lSelected, lFocus, nColor, lHot
   LOCAL sz, nStatus, hBkGnd, nFlags := DFCS_BUTTONCHECK
   (nwParam)
   DO CASE
      CASE hdr:code==NM_CUSTOMDRAW
           nRet := CDRF_DODEFAULT
           cd := (struct NMCUSTOMDRAW *) nlParam

           DO CASE
              CASE cd:dwDrawStage == CDDS_PREERASE
              CASE cd:dwDrawStage == CDDS_PREPAINT
                   lDisabled := ( cd:uItemState & CDIS_DISABLED ) != 0
                   lSelected := ( cd:uItemState & CDIS_SELECTED ) != 0
                   lFocus    := ( cd:uItemState & CDIS_FOCUS    ) != 0
                   lHot      := ( cd:uItemState & CDIS_HOT      ) != 0

                   nColor := NIL
                   IF lDisabled
                      nColor := SetTextColor( cd:hDC, GetSysColor( COLOR_GRAYTEXT ) )
                    ELSEIF ::ForeColor != NIL .AND. !( ::ForeColor == ::__SysForeColor )
                      nColor := SetTextColor( cd:hDC, ::ForeColor )
                   ENDIF

                   SetBkMode( cd:hdc, TRANSPARENT )

                   hBkGnd := ::BkBrush
                   IF hBkGnd == NIL
                      hBkGnd := ::Parent:BkBrush
                      SetBrushOrgEx( cd:hDC, ::Parent:ClientWidth-::Left, ::Parent:ClientHeight-::Top )
                   ENDIF
                   DEFAULT hBkGnd TO GetSysColorBrush( COLOR_BTNFACE )

                   FillRect( cd:hDC, cd:rc, hBkGnd )

                   aRect := {0,0,::BoxSize,::BoxSize}
                   IF ::RightAlign
                      aRect := { cd:rc:Right-(::BoxSize-1),0,cd:rc:Right,::BoxSize}
                   ENDIF
                   IF ::VertAlignment == DT_VCENTER
                      aRect[2] := ( cd:rc:bottom - ::BoxSize ) / 2
                    ELSEIF ::VertAlignment == DT_BOTTOM
                      aRect[2] := cd:rc:bottom - ::BoxSize
                   ENDIF
                   aRect[4] := aRect[2] + ::BoxSize

                   nStatus := ::SendMessage( BM_GETCHECK, 0, 0 )

                   DO CASE
                      CASE nStatus == BST_UNCHECKED
                           nStatus := IIF( lDisabled, DFCS_INACTIVE, 0 )
                           IF ::Application:OsVersion:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                              nStatus := IIF( lDisabled, CBS_UNCHECKEDDISABLED, IIF( lHot, CBS_UNCHECKEDHOT, CBS_UNCHECKEDNORMAL ) )
                           ENDIF

                      CASE nStatus == BST_CHECKED
                           nStatus := IIF( lDisabled, ( DFCS_INACTIVE | DFCS_CHECKED ), DFCS_CHECKED )
                           IF ::Application:OsVersion:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                              nStatus := IIF( lDisabled, CBS_CHECKEDDISABLED, IIF( lHot, CBS_CHECKEDHOT, CBS_CHECKEDNORMAL ) )
                           ENDIF

                      CASE nStatus == BST_INDETERMINATE
                           nStatus := IIF( lDisabled, ( DFCS_INACTIVE | DFCS_BUTTON3STATE | DFCS_CHECKED ), ( DFCS_BUTTON3STATE | DFCS_CHECKED ) )
                           IF ::Application:OsVersion:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                              nStatus := IIF( lDisabled, CBS_MIXEDDISABLED, IIF( lHot, CBS_MIXEDHOT, CBS_MIXEDNORMAL ) )
                           ENDIF
                   ENDCASE
                   nFlags := ( nFlags | nStatus )

                   IF ::Application:OsVersion:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                      DrawThemeBackground( ::System:hButtonTheme, cd:hDC, BP_CHECKBOX, nStatus, aRect, aRect )
                    ELSE
                      _DrawFrameControl( cd:hDC, aRect, DFC_BUTTON, nFlags )
                   ENDIF

                   IF ::RightAlign
                      cd:rc:left := 2
                      cd:rc:Right -= ( ::BoxSize + 2 )
                    ELSE
                      cd:rc:left += ( ::BoxSize + 1 )
                   ENDIF

                   IF ::VertAlignment == DT_BOTTOM
                      cd:rc:top ++
                      cd:rc:bottom --
                    ELSEIF ::VertAlignment == DT_TOP
                      cd:rc:top --
                   ENDIF

                   DrawText( cd:hDC, ::Text, cd:rc, ( ::TextAlignment | ::VertAlignment | DT_SINGLELINE ) )
                   IF nColor != NIL
                      SetTextColor( cd:hDC, nColor )
                   ENDIF
                   IF lFocus
                      SetBkMode( cd:hdc, OPAQUE )
                      SetBkColor( cd:hdc, RGB(0,0,0))
                      SetTextColor( cd:hdc, RGB(255,255,255))

                      sz := (struct SIZE)
                      GetTextExtentPoint32( cd:hDC, ::Text, @sz )

                      IF ::TextAlignment == DT_LEFT
                         cd:rc:left  -= 1
                       ELSEIF ::TextAlignment == DT_CENTER
                         cd:rc:left  := ( ( cd:rc:right + cd:rc:left - sz:cx ) / 2 ) - 1
                       ELSEIF ::TextAlignment == DT_RIGHT
                         cd:rc:left  := cd:rc:right - sz:cx - 1
                      ENDIF
                      cd:rc:right := cd:rc:left + sz:cx + 2

                      IF ::VertAlignment == DT_TOP
                         cd:rc:top   := 0
                       ELSEIF ::VertAlignment == DT_VCENTER
                         cd:rc:top   := ( cd:rc:bottom - sz:cy ) / 2
                       ELSEIF ::VertAlignment == DT_BOTTOM
                         cd:rc:top   := cd:rc:bottom - sz:cy
                      ENDIF
                      cd:rc:bottom := cd:rc:top + sz:cy + 1


                      DrawFocusRect( cd:hDC, cd:rc )
                   ENDIF
                   nRet := CDRF_SKIPDEFAULT
           ENDCASE
           RETURN nRet

   ENDCASE
RETURN NIL

METHOD DrawFrame( hDC, aRect, nAlign, nWidth, nHeight, nStatus, lDraw ) CLASS CheckBox
   LOCAL nFlags := DFCS_BUTTONCHECK
   DEFAULT lDraw TO TRUE
   IF nStatus != NIL
      DO CASE
         CASE nStatus == BST_UNCHECKED
              nStatus := 0
              IF ::Application:OsVersion:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                 nStatus := CBS_UNCHECKEDNORMAL
              ENDIF

         CASE nStatus == BST_CHECKED
              nStatus := DFCS_CHECKED
              IF ::Application:OsVersion:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                 nStatus := CBS_CHECKEDNORMAL
              ENDIF

         CASE nStatus == BST_INDETERMINATE
              nStatus := DFCS_BUTTON3STATE+DFCS_CHECKED
              IF ::Application:OsVersion:dwMajorVersion > 4 .AND. ::Application:ThemeActive
                 nStatus := CBS_MIXEDNORMAL
              ENDIF

      ENDCASE
      nFlags := ( nFlags | nStatus )
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
      IF ::Application:OsVersion:dwMajorVersion > 4 .AND. ::Application:ThemeActive
         DrawThemeBackground( ::System:hButtonTheme, hDC, BP_CHECKBOX, nStatus, aRect, aRect )
       ELSE
         DrawFrameControl( hDC, aRect, DFC_BUTTON, nFlags )
      ENDIF
   ENDIF
RETURN aRect

METHOD SetCheckStyle( nStyle ) CLASS CheckBox
   LOCAL nCurStyle
   ::Style := ( ::Style & NOT( BS_AUTOCHECKBOX ) & NOT( BS_AUTO3STATE ) )
   SWITCH nStyle
      CASE 1
          nCurStyle := BS_AUTOCHECKBOX
          EXIT
      CASE 2
          nCurStyle := BS_AUTO3STATE
          EXIT
   END
   ::Style := ( ::Style | BS_CHECKBOX | nCurStyle )
   IF ::hWnd != NIL
      SetWindowLong( ::hWnd, GWL_STYLE, ::Style )
      ::SetState( ::xState )
   ENDIF
RETURN Self

METHOD OnCtlColorStatic( nwParam ) CLASS CheckBox
   LOCAL hBkGnd := ::GetBkBrush()
   IF ::ForeColor != NIL
      SetTextColor( nwParam, ::ForeColor )
   ENDIF
   SetBkMode( nwParam, TRANSPARENT )
RETURN hBkGnd

METHOD SetState( nState ) CLASS CheckBox
   IF VALTYPE( nState ) == "L"
      nState := IIF( nState, BST_CHECKED, BST_UNCHECKED )
   ENDIF
   IF ::IsWindow()
      ::SendMessage( BM_SETCHECK, nState, 0 )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
