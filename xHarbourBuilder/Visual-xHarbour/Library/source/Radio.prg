/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Radio.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#define RBS_UNCHECKEDNORMAL   1
#define RBS_UNCHECKEDHOT      2
#define RBS_UNCHECKEDPRESSED  3
#define RBS_UNCHECKEDDISABLED 4
#define RBS_CHECKEDNORMAL     5
#define RBS_CHECKEDHOT        6
#define RBS_CHECKEDPRESSED    7
#define RBS_CHECKEDDISABLED   8

#include "debug.ch"
#include "vxh.ch"

#define BP_RADIOBUTTON           2

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

//-----------------------------------------------------------------------------------------------

CLASS RadioButton INHERIT Control

   //PROPERTY Transparent   SET ::__SetTransp(v)              DEFAULT .F.
   PROPERTY Group         SET ::SetStyle( WS_GROUP, v )     DEFAULT .F.
   PROPERTY OwnerDraw     SET ::SetStyle( BS_OWNERDRAW, v ) DEFAULT .F.
   PROPERTY InitialState  SET ::__SetInitialState( v )      DEFAULT BST_UNCHECKED

   DATA DefaultButton INIT .F.
   DATA States       INIT { "Unchecked", "Checked" }
   DATA State     EXPORTED

   ACCESS Checked INLINE ::GetState() == BST_CHECKED

   DATA ImageList  EXPORTED
   DATA ImageIndex EXPORTED

   METHOD Init()  CONSTRUCTOR
   METHOD Create()             INLINE ::Super:Create(), ::SendMessage( BM_SETCHECK, ::xInitialState, 0 )

   METHOD OnDestroy()          INLINE Super:OnDestroy(), ::CloseThemeData(), Self
   METHOD OnEraseBkGnd()       INLINE 1
   METHOD OnParentNotify()

   METHOD GetState()
   METHOD SetState(nState)     INLINE ::SendMessage( BM_SETCHECK, nState, 0 )
   METHOD __SetInitialState()
   METHOD OnCtlColorStatic()
ENDCLASS

METHOD Init( oParent ) CLASS RadioButton
   DEFAULT ::__xCtrlName TO "RadioButton"
   DEFAULT ::Style TO (WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_AUTORADIOBUTTON | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::ClsName   := "Button"
   ::Super:Init( oParent )
   ::Width     := 100
   ::Height    := 15
   ::ThemeName := "button"
   ::OpenThemeData()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetInitialState( nState ) CLASS RadioButton
   LOCAL lChecked
   IF ::IsWindow()
      ::SendMessage( BM_SETCHECK, nState, 0 )
   ENDIF
   lChecked  := (nState & BST_CHECKED) == BST_CHECKED
   ::xInitialState := IIF( lChecked, BST_CHECKED, BST_UNCHECKED )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD GetState() CLASS RadioButton
   LOCAL nState := ::SendMessage( BM_GETCHECK, 0, 0 )
RETURN IIF( ( nState & BST_CHECKED ) == BST_CHECKED, BST_CHECKED, BST_UNCHECKED )

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS RadioButton
   LOCAL nRet, cd, nState, aRect, lHot, lDisabled, lSelected, lFocus, nColor, lChecked, lPressed
   LOCAL sz
   LOCAL hBkGnd := ::BkBrush
   (nwParam)
   DO CASE
      CASE hdr:code==NM_CUSTOMDRAW
           nRet := CDRF_DODEFAULT
           cd := (struct NMCUSTOMDRAW *) nlParam

           DO CASE
              CASE cd:dwDrawStage == CDDS_PREERASE
              CASE cd:dwDrawStage == CDDS_PREPAINT

                   nState := ::SendMessage( BM_GETSTATE, 0, 0 )

                   lDisabled := (cd:uItemState & CDIS_DISABLED) != 0
                   lSelected := (cd:uItemState & CDIS_SELECTED) != 0
                   lFocus    := (cd:uItemState & CDIS_FOCUS)    != 0
                   lHot      := (cd:uItemState & CDIS_HOT)      != 0
                   lChecked  := (nState & BST_CHECKED) == BST_CHECKED
                   lPressed  := (nState & BST_PUSHED)  == BST_PUSHED

                   hBkGnd := ::BkBrush
                   IF hBkGnd == NIL
                      hBkGnd := ::Parent:BkBrush
                      SetBrushOrgEx( cd:hdc, ::Parent:ClientWidth-::Left, ::Parent:ClientHeight-::Top )
                   ENDIF
                   DEFAULT hBkGnd TO GetSysColorBrush( COLOR_BTNFACE )

                   IF hBkGnd == NIL
                      RETURN nRet
                   ENDIF

                   FillRect( cd:hdc, cd:rc, hBkGnd )

                   aRect := cd:rc:Array
                   aRect[3] := 15

                   nState := RBS_UNCHECKEDNORMAL

                   IF ::Theming .AND. ::Application:IsThemedXP
                      IF lDisabled
                         nState := IIF( lChecked, RBS_CHECKEDDISABLED, RBS_UNCHECKEDDISABLED )
                       ELSE
                         IF lHot
                            nState := IIF( lChecked, RBS_CHECKEDHOT, RBS_UNCHECKEDHOT )
                          ELSEIF !lPressed
                            nState := IIF( lChecked, RBS_CHECKEDNORMAL, RBS_UNCHECKEDNORMAL )
                          ELSE
                            nState := IIF( lChecked, RBS_CHECKEDPRESSED, RBS_UNCHECKEDPRESSED )
                         ENDIF
                      ENDIF
                      DrawThemeBackground( ::hTheme, cd:hdc, BP_RADIOBUTTON, nState, aRect, aRect )
                    ELSE
                      nState := DFCS_BUTTONRADIO
                      IF lPressed
                         nState := (nState | DFCS_PUSHED)
                      ENDIF
                      IF lHot
                         nState := (nState | DFCS_HOT)
                      ENDIF
                      IF lDisabled
                         nState := (nState | DFCS_INACTIVE)
                      ENDIF
                      _DrawFrameControl( aRect, DFC_BUTTON, nState )
                   ENDIF

                   nColor := NIL
                   IF lDisabled
                      nColor := SetTextColor( cd:hDC, GetSysColor( COLOR_GRAYTEXT ) )
                    ELSEIF ::ForeColor != NIL .AND. !lDisabled
                      nColor := SetTextColor( cd:hDC, ::ForeColor )
                   ENDIF

                   cd:rc:left += 17
                   SetBkMode( cd:hdc, TRANSPARENT )
                   DrawText( cd:hDC, ::Caption, cd:rc, (DT_LEFT | DT_VCENTER | DT_SINGLELINE) )

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
           RETURN nRet

   ENDCASE
RETURN NIL

METHOD OnCtlColorStatic( nwParam ) CLASS RadioButton
   LOCAL nBack, hBkGnd := ::GetBkBrush()
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
