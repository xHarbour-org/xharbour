/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Button.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"
#include "colors.ch"

#define PBS_NORMAL       1
#define PBS_HOT          2
#define PBS_PRESSED      3
#define PBS_DISABLED     4
#define PBS_DEFAULTED    5
#define BP_PUSHBUTTON    1

#define TMT_BTNTEXT   1619

//-----------------------------------------------------------------------------------------------

CLASS Button INHERIT Control

   DATA ImageAlign        PUBLISHED INIT __GetSystem():TextAlignment:Center
   PROPERTY Group         INDEX WS_GROUP         READ xGroup         WRITE SetStyle         DEFAULT .F. PROTECTED
   PROPERTY OwnerDraw     INDEX BS_OWNERDRAW     READ xOwnerDraw     WRITE SetStyle         DEFAULT .F. PROTECTED
   PROPERTY ImageIndex                           READ xImageIndex    WRITE SetImageIndex    DEFAULT  0  PROTECTED
   PROPERTY Border        INDEX WS_BORDER        READ xBorder        WRITE SetStyle         DEFAULT .F. PROTECTED
   PROPERTY DefaultButton INDEX BS_DEFPUSHBUTTON READ xDefaultButton WRITE SetStyle         DEFAULT .F. PROTECTED
   PROPERTY Enabled       INDEX WS_DISABLED      READ xEnabled       WRITE SetStyle         DEFAULT .T.       PROTECTED

   DATA ImgInst           EXPORTED
   DATA ImageIndent       EXPORTED INIT 3
   DATA DrawTheme         EXPORTED INIT .T.
   DATA DrawFocus         EXPORTED INIT .T.
   DATA AllowUnDock       EXPORTED INIT FALSE
   DATA AllowClose        EXPORTED INIT FALSE
   DATA ShortCutKey       PUBLISHED
   ACCESS Checked INLINE ( ::GetState() == BST_CHECKED)

   DATA xState            PROTECTED INIT BST_UNCHECKED

   METHOD Init()    CONSTRUCTOR
   METHOD GetState()
   //METHOD SetStyle(n,l)       INLINE ::Super:SetStyle( n, l ), IIF( ::IsWindow(), ::InvalidateRect(), NIL )
   METHOD OnParentDrawItem()
   METHOD OnLButtonDblClk( nwParam, x, y ) INLINE ::SendMessage( WM_LBUTTONDOWN, nwParam, MAKELONG( x,y ) )
   METHOD DrawFrame()
   METHOD __WindowDestroy()   INLINE ::Super:__WindowDestroy(), ::CloseThemeData(), Self
   METHOD Click()             INLINE SetActiveWindow( ::Parent:hWnd ), ::SendMessage( BM_CLICK, 0, 0 )
   METHOD Push()              INLINE ::SendMessage( BM_SETSTATE, .T., 0 )
   METHOD Release()           INLINE ::SendMessage( BM_SETSTATE, .F., 0 )
   METHOD IsPushed()          INLINE ::SendMessage( BM_GETSTATE, 0, 0 ) & BST_PUSHED != 0
   METHOD OnCtlColorBtn()
   METHOD OnCtlColorStatic()
   METHOD SetFocus()          INLINE ::Super:SetFocus(), ::DefaultButton := .T.
   METHOD SetState()
   METHOD Create()
   METHOD SetBackColor()
   METHOD SetForeColor()
   METHOD SetImageIndex()
   METHOD OnMouseHover()
   METHOD OnMouseLeave()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Button
   DEFAULT ::__xCtrlName TO "Button"
   ::ImgInst   := ::Instance
   ::ClsName   := "Button"
   DEFAULT ::Style TO WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Super:Init( oParent )
   ::Width     := 80
   ::Height    := 25
   ::ThemeName := "button"
   ::OpenThemeData()
   IF ::__ClassInst != NIL
      ::__PropFilter := { "HIGHLIGHTCAPTION", "SMALLCAPTION", "ALLOWMAXIMIZE" }
   ENDIF
   ::ShortCutKey   := __MenuStripItemShortCut( Self )
RETURN Self

METHOD Create() CLASS Button
   IF ::__ClassInst != NIL
      __DeleteEvents( ::Events,{ "OnCtlColorEdit",;
                                 "OnCtlColorListBox",;
                                 "OnCtlColorScrollBar",;
                                 "OnCtlColorStatic",;
                                 "OnSysColorChange",;
                                 "OnClear",;
                                 "OnCopy",;
                                 "OnCut",;
                                 "OnPaste",;
                                 "OnUndo",;
                                 "OnNCActivate",;
                                 "OnNCCalcSize",;
                                 "OnNCCreate",;
                                 "OnNCDestroy",;
                                 "OnNCHitTest",;
                                 "OnNCLButtonDown",;
                                 "OnNCLButtonUp",;
                                 "OnNCLButtonDblClk",;
                                 "OnNCRButtonUp",;
                                 "OnNCRButtonDown",;
                                 "OnNCRButtonDblClk",;
                                 "OnNCMButtonUp",;
                                 "OnNCMButtonDown",;
                                 "OnNCMButtonDblClk",;
                                 "OnNCXButtonUp",;
                                 "OnNCXButtonDown",;
                                 "OnNCXButtonDblClk",;
                                 "OnNCMouseHover",;
                                 "OnNCMouseLeave",;
                                 "OnNCMouseMove",;
                                 "OnNCPaint" } )
   ENDIF
   IF ( ::ImageIndex != NIL .AND. ::ImageIndex > 0 ) .OR. ( ::ForeColor != NIL .AND. ::ForeColor != ::ForeSysColor ) .OR. ( ::BackColor != NIL .AND. ::BackColor != ::BackSysColor ) .OR. ::Parent:__xCtrlName == "GroupBox" .AND. !::OwnerDraw
      ::Style := ::Style | BS_OWNERDRAW
   ENDIF
   ::Super:Create()
   
   ::SetImageIndex( ::ImageIndex )
   ::ShortCutKey:SetAccel()
   
   IF !(::Style & BS_OWNERDRAW) == BS_OWNERDRAW
      ::DefaultButton := ::xDefaultButton
   ENDIF
RETURN Self

METHOD SetBackColor( nColor, lRepaint ) CLASS Button
   IF ( nColor == NIL .OR. nColor == ::BackSysColor ) .AND. !::__ForceSysColor .AND. ::Parent:__xCtrlName != "GroupBox"
      ::SetStyle( BS_OWNERDRAW, .F. )
    ELSEIF ::__ForceSysColor .OR. ( ::Style & BS_OWNERDRAW == 0 ) .AND. ( nColor != NIL .AND. nColor != ::BackSysColor ) .OR. ::Parent:__xCtrlName == "GroupBox"
      ::SetStyle( BS_OWNERDRAW, .T. )
   ENDIF
   Super:SetBackColor( nColor, lRepaint )
RETURN SELF

METHOD SetForeColor( nColor, lRepaint ) CLASS Button
   IF ( nColor == NIL .OR. nColor == ::ForeSysColor ) .AND. !::__ForceSysColor .AND. ::Parent:__xCtrlName != "GroupBox"
      ::SetStyle( BS_OWNERDRAW, .F. )
    ELSEIF ::__ForceSysColor .OR. ( ::Style & BS_OWNERDRAW == 0 ) .AND. ( nColor != NIL .AND. nColor != ::ForeSysColor ) .OR. ::Parent:__xCtrlName == "GroupBox"
      ::SetStyle( BS_OWNERDRAW, .T. )
   ENDIF
   Super:SetForeColor( nColor, lRepaint )
RETURN SELF

METHOD SetImageIndex( n ) CLASS Button
   DEFAULT n TO ::xImageIndex
   IF ( ( ::Parent:ImageList != NIL .AND. n > 0 ) .OR. ( ::ForeColor != NIL .AND. ::ForeColor != ::ForeSysColor ) .OR. ( ::BackColor != NIL .AND. ( ::BackColor != ::BackSysColor  .OR. ::__ForceSysColor ) ) )  .OR. ::Parent:__xCtrlName == "GroupBox"
      ::Style := ::Style | BS_OWNERDRAW
    ELSEIF !::OwnerDraw
      ::Style := ::Style & NOT( BS_OWNERDRAW )
   ENDIF
   IF ::IsWindow()
      ::SetWindowLong( GWL_STYLE, ::Style )
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
      ::RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD GetState() CLASS Button

   LOCAL nState
   IF ::hWnd != NIL
      nState := ::SendMessage( BM_GETSTATE, 0, 0 )

      DO CASE
         CASE nState & BST_CHECKED == BST_CHECKED
              nState := BST_CHECKED

         CASE nState & BST_INDETERMINATE == BST_INDETERMINATE
              nState := BST_INDETERMINATE

         CASE nState & BST_UNCHECKED == BST_UNCHECKED
              nState := BST_UNCHECKED

         CASE nState & BST_FOCUS == BST_FOCUS
              nState := BST_FOCUS

         CASE nState & BST_PUSHED == BST_PUSHED
              nState := BST_PUSHED
      ENDCASE
   ENDIF
   DEFAULT nState TO ::xState
RETURN nState

//-----------------------------------------------------------------------------------------------

METHOD SetState( nState ) CLASS Button
   // Patch
   IF ::__ClassInst != NIL
      ::__ClassInst:xState := ::xState
   ENDIF
   ::xState := nState
   IF ::IsWindow()
      ::SendMessage( BM_SETCHECK, nState, 0 )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD DrawFrame( oDrawing, aRect, nAlign, nWidth, nStatus ) CLASS Button

   LOCAL nFlags := DFCS_BUTTONPUSH

   IF nStatus != NIL
      nFlags := nFlags | nStatus
   ENDIF
   DO CASE
      CASE nAlign == DT_LEFT
           aRect[3] := aRect[1]+nWidth

      CASE nAlign == DT_RIGHT
           aRect[1] := aRect[3]-nWidth
   ENDCASE
   oDrawing:DrawFrameControl( aRect, DFC_BUTTON, nFlags )
RETURN nWidth

//-----------------------------------------------------------------------------------------------

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS Button
   LOCAL nLeft, nTop, aRect, nStyle, lDisabled, lSelected, lFocus, aTextRect, nTextFlags, nColor, n, lDefault
   (nwParam)
   (nlParam)
   IF !( ::__xCtrlName == "Button" ) .OR. ::OwnerDraw
      RETURN NIL
   ENDIF
   IF dis:CtlType & ODT_BUTTON != 0 .AND. ( ( ::Parent:ImageList != NIL .AND. ::ImageIndex > 0 ) .OR. ( ::ForeColor != NIL .AND. ( ::ForeColor != ::ForeSysColor .OR. ::__ClassInst != NIL) ) .OR. ( ::BackColor != NIL .AND. ::BackColor != ::BackSysColor )  .OR. ::Parent:__xCtrlName == "GroupBox" .OR. ::__ForceSysColor )

      nTop := 5
      nLeft:= 3
      aRect := { dis:rcItem:Left, dis:rcItem:Top, dis:rcItem:Right, dis:rcItem:Bottom }

      lDisabled := dis:itemState & ODS_DISABLED != 0
      lSelected := dis:itemState & ODS_SELECTED != 0
      lFocus    := dis:itemState & ODS_FOCUS    != 0
      lDefault  := dis:itemState & ODS_DEFAULT  != 0

      aTextRect  := aClone( aRect )
      nTextFlags := DT_CENTER + DT_VCENTER + DT_SINGLELINE

      IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
         DO CASE
            CASE ::ImageAlign == DT_LEFT
                 nTop  := ( aRect[4] / 2 ) - ( ::Parent:ImageList:IconHeight / 2 )
                 aTextRect[1] := nLeft + ::Parent:ImageList:IconWidth + ::ImageIndent
                 nTextFlags := DT_LEFT + DT_VCENTER + DT_SINGLELINE

            CASE ::ImageAlign == DT_RIGHT
                 nLeft := ( aRect[3] - ::Parent:ImageList:IconWidth - ::ImageIndent )
                 nTop  := ( aRect[4] / 2 ) - ( ::Parent:ImageList:IconHeight / 2 )
                 aTextRect[3] := nLeft - ::ImageIndent
                 nTextFlags := DT_RIGHT  + DT_VCENTER + DT_SINGLELINE

            CASE ::ImageAlign == DT_CENTER
                 nLeft := ( aRect[3] / 2 ) - ( ::Parent:ImageList:IconWidth / 2 )
                 n := _GetTextExtentPoint32( dis:hDC, ::Caption )[2]
                 nTop  := ( aRect[4] / 2 ) - ( ::Parent:ImageList:IconHeight / 2 ) - ( n / 2 )


                 aTextRect[2] += ::Parent:ImageList:IconHeight
                 nTextFlags := DT_CENTER + DT_VCENTER + DT_SINGLELINE

         ENDCASE
         IF EMPTY( ::Caption )
            nTop := ( aRect[4] / 2 ) - ( ::Parent:ImageList:IconHeight / 2 )
         ENDIF
      ENDIF

      IF ::Theming .AND. ::OsVer:dwMajorVersion > 4 .AND. ::DrawTheme .AND. IsThemeActive()
         nStyle := PBS_NORMAL
         IF ::DefaultButton
//            nStyle := nStyle | PBS_DEFAULTED
         ENDIF
         IF lDisabled
            nStyle := PBS_DISABLED
         ENDIF
         IF lSelected
            nStyle := PBS_PRESSED
         ENDIF
         IF dis:itemState & ODS_HOTLIGHT != 0 .OR. ( ::__lMouseHover .AND. !lSelected )
            nStyle := PBS_HOT
         ENDIF

         DrawThemeBackground( ::hTheme, dis:hDC, BP_PUSHBUTTON, nStyle, aRect, aRect )
         _InFlateRect( @aRect, -2, -2 )
         IF ::BkBrush != NIL
            _FillRect( dis:hDC, aRect, ::BkBrush )
         ENDIF
       ELSE

         IF lFocus .AND. lSelected
            SelectObject( dis:hDC, GetSysColorBrush( COLOR_BTNSHADOW ) )
            Rectangle( dis:hDC, aRect[1], aRect[2], aRect[3], aRect[4] )
            aRect[1]+=2
            aRect[2]+=2
            aRect[3]-=2
            aRect[4]-=2
            _FillRect( dis:hDC, aRect, IIF( ::BkBrush != NIL, ::BkBrush, GetSysColorBrush( COLOR_BTNFACE ) ) )
          ELSE
            IF lFocus
               SelectObject( dis:hDC, IIF( ::BkBrush != NIL, ::BkBrush, GetSysColorBrush( COLOR_BTNFACE ) ) )
               Rectangle( dis:hDC, aRect[1], aRect[2], aRect[3], aRect[4] )
               aRect[1]+=1
               aRect[2]+=1
               aRect[3]-=1
               aRect[4]-=1
            ENDIF
            _FillRect( dis:hDC, aRect, IIF( ::BkBrush != NIL, ::BkBrush, GetSysColorBrush( COLOR_BTNFACE ) ) )

            nColor := ::BackColor
            IF nColor != NIL .AND. ( nColor != ::BackSysColor  .OR. ::__ForceSysColor )
               nColor := LightenColor( nColor, 100 )
               __Draw3dRect( dis:hDC, aRect, IIF( !lSelected, nColor, GetSysColor(COLOR_3DDKSHADOW) ), GetSysColor(COLOR_3DDKSHADOW) )
             ELSE
               _DrawEdge( dis:hDC, aRect, IF( lSelected, EDGE_SUNKEN, EDGE_RAISED ), BF_RECT + BF_SOFT)
            ENDIF
         ENDIF
      ENDIF
      IF lSelected .AND. lFocus
         nLeft++
         nTop++
         OffSet( @aTextRect, 1, 1 )
      ENDIF
      IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
         IF lDisabled
            ::Parent:ImageList:DrawDisabled( dis:hDC, ::ImageIndex, nLeft, nTop )
          ELSE
            ::Parent:ImageList:DrawImage( dis:hDC, ::ImageIndex, nLeft, nTop )
         ENDIF
      ENDIF
      IF lFocus .AND. ::DrawFocus
         SetTextColor( dis:hDC, RGB(0,0,0) )
         SetBkColor( dis:hDC, ::BackColor )
         _DrawFocusRect( dis:hDC, { dis:rcItem:Left + 4, dis:rcItem:Top + 4, dis:rcItem:Right - 4, dis:rcItem:Bottom - 4 } )
      ENDIF
      SetBkMode( dis:hDC, TRANSPARENT )

      SetTextColor( dis:hDC, GetThemeSysColor( ::hWnd, TMT_BTNTEXT ) )

      IF ::Application != NIL .AND. lDisabled
         OffSet( @aTextRect, 1, 1 )
         SetTextColor( dis:hDC, ::System:Colors:BtnHighlight )
         _DrawText( dis:hDC, ::Caption, aTextRect, nTextFlags )
         OffSet( @aTextRect, -1, -1 )
         SetTextColor( dis:hDC, ::System:Colors:GrayText )
       ELSE
         SetTextColor( dis:hDC, ::ForeColor )
      ENDIF
      _DrawText( dis:hDC, ::Caption, aTextRect, nTextFlags )
   ENDIF

RETURN NIL

METHOD OnCtlColorBtn( nwParam, nlParam ) CLASS Button
   LOCAL hBkGnd := ::Parent:BkBrush
   (nlParam)
   IF ::Parent:ClsName == "DataGrid"
      hBkGnd := GetSysColorBrush( COLOR_HIGHLIGHT )
   ENDIF
   SetTextColor( nwParam, ::ForeColor )
RETURN hBkGnd

METHOD OnCtlColorStatic( nwParam, nlParam ) CLASS Button
   LOCAL hBkGnd := ::BkBrush
   (nlParam)
   DEFAULT hBkGnd TO ::Parent:BkBrush

   IF ::Parent:ClsName == "ToolBarWindow32"
      SetBkMode( nwParam, TRANSPARENT )
      RETURN NIL//GetStockObject( NULL_BRUSH )
   ENDIF

   SetTextColor( nwParam, GetThemeSysColor( ::hWnd, TMT_BTNTEXT ) )

   IF ::ForeColor != NIL .AND. !(::ForeColor == ::ForeSysColor)
      SetTextColor( nwParam, ::ForeColor )
   ENDIF
   IF hBkGnd != NIL
      SetBkMode( nwParam, TRANSPARENT )
      RETURN hBkGnd
    ELSEIF ::ForeColor != NIL .AND. ::ForeColor != ::ForeSysColor
      SetBkMode( nwParam, TRANSPARENT )
      IF ::BackColor == ::BackSysColor
         RETURN GetSysColorBrush( COLOR_BTNFACE )
      ENDIF
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF
RETURN NIL

METHOD OnMouseHover() CLASS Button
   IF !( ::__xCtrlName == "Button" ) .OR. ::OwnerDraw
      RETURN NIL
   ENDIF
   ::InvalidateRect(, .F. )
RETURN NIL

METHOD OnMouseLeave() CLASS Button
   IF !( ::__xCtrlName == "Button" ) .OR. ::OwnerDraw
      RETURN NIL
   ENDIF
   ::InvalidateRect(, .F. )
RETURN NIL
