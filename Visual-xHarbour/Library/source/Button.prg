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

#define BS_COMMANDLINK 0x0000000E
#define BS_SPLITBUTTON 0x0000000C

#define BCM_FIRST 0x1600
#define BCM_SETNOTE (BCM_FIRST + 0x0009)

#define TMT_BTNTEXT   1619

//-----------------------------------------------------------------------------------------------

CLASS Button INHERIT Control

   PROPERTY Alignment                       SET ::Redraw(v)      DEFAULT DT_CENTER
   DATA EnumAlignment    EXPORTED INIT { { "Left", "Center", "Right" }, { DT_LEFT, DT_CENTER, DT_RIGHT } }

   PROPERTY ImageAlign    ROOT "Appearance"                                      DEFAULT __GetSystem():TextAlignment:Center
   PROPERTY MenuArrow     ROOT "Appearance"                                      DEFAULT .F.

   PROPERTY ImageList     ROOT "Appearance" GET __ChkComponent( Self, @::xImageList )
   PROPERTY ImageIndex    ROOT "Appearance" SET ::SetImageIndex(v)               DEFAULT  0

   PROPERTY ShortCutKey   ROOT "Behavior"
   PROPERTY Group         ROOT "Behavior"   SET ::SetStyle( WS_GROUP, v )        DEFAULT .F.
   PROPERTY DefaultButton ROOT "Behavior"   SET ::SetStyle( BS_DEFPUSHBUTTON, v) DEFAULT .F.
   PROPERTY Enabled       ROOT "Behavior"   SET ::SetStyle( WS_DISABLED, v )     DEFAULT .T.
   PROPERTY MultiLine     ROOT "Behavior"   SET ::SetStyle( BS_MULTILINE, v )    DEFAULT .F. PROTECTED
   PROPERTY Split                           SET ::SetStyle( BS_SPLITBUTTON, v )  DEFAULT .F.
   PROPERTY DrawFocus     ROOT "Appearance" DEFAULT .T.
   PROPERTY ImageIndent   DEFAULT 3

   DATA ImgInst           EXPORTED
   DATA DrawTheme         EXPORTED INIT .T.
   DATA AllowUnDock       EXPORTED INIT FALSE
   DATA AllowClose        EXPORTED INIT FALSE
   ACCESS Checked INLINE ( ::GetState() == BST_CHECKED)

   ACCESS __IsOD  INLINE ( ::ImageIndex != NIL .AND. ::ImageIndex > 0 ) .OR.;
                         ( ::ForeColor != NIL .AND. ::ForeColor != ::__SysForeColor ) .OR.;
                         ( ::BackColor != NIL .AND. ::BackColor != ::__SysBackColor ) .OR.;
                         ::Parent:__xCtrlName == "GroupBox" .OR.;
                         ( ::MenuArrow .AND. ::ContextMenu != NIL )


   DATA xState            PROTECTED INIT BST_UNCHECKED

   METHOD Init()    CONSTRUCTOR
   METHOD GetState()
   //METHOD SetStyle(n,l)       INLINE ::Super:SetStyle( n, l ), IIF( ::IsWindow(), ::InvalidateRect(), NIL )
   METHOD OnParentDrawItem()
   METHOD OnLButtonDblClk( nwParam, x, y ) INLINE ::SendMessage( WM_LBUTTONDOWN, nwParam, MAKELONG( x,y ) )
   METHOD DrawFrame()
   METHOD Click()             INLINE SetActiveWindow( ::Parent:hWnd ), ::SendMessage( BM_CLICK, 0, 0 )
   METHOD Push()              INLINE ::SendMessage( BM_SETSTATE, .T., 0 )
   METHOD Release()           INLINE ::SendMessage( BM_SETSTATE, .F., 0 )
   METHOD IsPushed()          INLINE ( ::SendMessage( BM_GETSTATE, 0, 0 ) & BST_PUSHED ) != 0
   METHOD OnCtlColorBtn()
   METHOD OnCtlColorStatic()
   METHOD SetState()
   METHOD Create()
   METHOD SetBackColor()
   METHOD SetForeColor()
   METHOD SetImageIndex()
   METHOD OnMouseHover()
   METHOD OnMouseLeave()
   METHOD OnParentCommand()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Button
   DEFAULT ::__xCtrlName TO "Button"
   ::ImgInst   := ::Instance
   ::ClsName   := "Button"
   DEFAULT ::Style TO ( WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS )
   ::Super:Init( oParent )
   ::Width     := 80
   ::Height    := 25
   IF ::DesignMode
      ::__PropFilter := { "ALLOWMAXIMIZE" }
   ENDIF
   ::ShortCutKey   := __MenuStripItemShortCut( Self )
RETURN Self

METHOD Create() CLASS Button
   IF ::DesignMode
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
   ::SetStyle( BS_OWNERDRAW, ::__IsOD )
   ::Super:Create()

   ::SetImageIndex( ::ImageIndex )
   ::ShortCutKey:SetAccel()
RETURN Self

METHOD SetBackColor( nColor, lRepaint ) CLASS Button
   ::xBackColor := nColor
   ::SetStyle( BS_OWNERDRAW, ::__IsOD )
   Super:SetBackColor( nColor, lRepaint )
RETURN SELF

METHOD SetForeColor( nColor, lRepaint ) CLASS Button
   ::xForeColor := nColor
   ::SetStyle( BS_OWNERDRAW, ::__IsOD )
   Super:SetForeColor( nColor, lRepaint )
RETURN SELF

METHOD SetImageIndex( n ) CLASS Button
   DEFAULT n TO ::xImageIndex
   ::SetStyle( BS_OWNERDRAW, ::__IsOD )
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
         CASE ( nState & BST_CHECKED ) == BST_CHECKED
              nState := BST_CHECKED

         CASE ( nState & BST_INDETERMINATE ) == BST_INDETERMINATE
              nState := BST_INDETERMINATE

         CASE ( nState & BST_UNCHECKED ) == BST_UNCHECKED
              nState := BST_UNCHECKED

         CASE ( nState & BST_FOCUS ) == BST_FOCUS
              nState := BST_FOCUS

         CASE ( nState & BST_PUSHED ) == BST_PUSHED
              nState := BST_PUSHED
      ENDCASE
   ENDIF
   DEFAULT nState TO ::xState
RETURN nState

//-----------------------------------------------------------------------------------------------

METHOD SetState( nState ) CLASS Button
   // Patch
   IF ::DesignMode
      __SetInitialValues( Self, "State" )
   ENDIF
   ::xState := nState
   IF ::IsWindow()
      ::SendMessage( BM_SETCHECK, nState, 0 )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD DrawFrame( hDC, aRect, nAlign, nWidth, nStatus ) CLASS Button

   LOCAL nFlags := DFCS_BUTTONPUSH

   IF nStatus != NIL
      nFlags := ( nFlags | nStatus )
   ENDIF
   DO CASE
      CASE nAlign == DT_LEFT
           aRect[3] := aRect[1]+nWidth

      CASE nAlign == DT_RIGHT
           aRect[1] := aRect[3]-nWidth
   ENDCASE
   DrawFrameControl( hDC, aRect, DFC_BUTTON, nFlags )
RETURN nWidth

//-----------------------------------------------------------------------------------------------

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS Button
   LOCAL nLeft, nTop, aRect, nStyle, lDisabled, lSelected, lFocus, aTextRect, nTextFlags, nColor, n, lDefault, oImageList
   (nwParam)
   (nlParam)
   IF !( ::__xCtrlName == "Button" )
      RETURN NIL
   ENDIF
   oImageList := ::ImageList
   DEFAULT oImageList TO ::Parent:ImageList
   IF ( dis:CtlType & ODT_BUTTON ) != 0 .AND. ( ( ::MenuArrow .AND. ::ContextMenu != NIL ) .OR. ( oImageList != NIL .AND. ::ImageIndex > 0 ) .OR. ( ::ForeColor != NIL .AND. ( ::ForeColor != ::__SysForeColor .OR. ::DesignMode) ) .OR. ( ::BackColor != NIL .AND. ::BackColor != ::__SysBackColor )  .OR. ::Parent:__xCtrlName == "GroupBox" .OR. ::__ForceSysColor )
      nTop  := 5
      nLeft := 6

      aRect := { dis:rcItem:Left, dis:rcItem:Top, dis:rcItem:Right, dis:rcItem:Bottom }

      lDisabled := ( dis:itemState & ODS_DISABLED ) != 0
      lSelected := ( dis:itemState & ODS_SELECTED ) != 0
      lFocus    := ( dis:itemState & ODS_FOCUS    ) != 0
      lDefault  := ( dis:itemState & ODS_DEFAULT  ) != 0

      aTextRect  := aClone( aRect )
      nTextFlags := ( ::Alignment | DT_VCENTER | DT_SINGLELINE )

      IF oImageList != NIL .AND. ::ImageIndex > 0
         DO CASE
            CASE ::ImageAlign == DT_LEFT
                 nTop  := ( aRect[4] / 2 ) - ( oImageList:IconHeight / 2 )
                 aTextRect[1] := nLeft + oImageList:IconWidth + ::ImageIndent
                 //nTextFlags := DT_LEFT + DT_VCENTER + DT_SINGLELINE

            CASE ::ImageAlign == DT_RIGHT
                 nLeft := ( aRect[3] - oImageList:IconWidth - ::ImageIndent )
                 nTop  := ( aRect[4] / 2 ) - ( oImageList:IconHeight / 2 )
                 aTextRect[3] := nLeft - ::ImageIndent
                 //nTextFlags := DT_RIGHT  + DT_VCENTER + DT_SINGLELINE

            CASE ::ImageAlign == DT_CENTER
                 nLeft := ( aRect[3] / 2 ) - ( oImageList:IconWidth / 2 )
                 n := _GetTextExtentPoint32( dis:hDC, ::xText )[2]
                 nTop  := ( aRect[4] / 2 ) - ( oImageList:IconHeight / 2 ) - ( n / 2 )

                 aTextRect[2] += oImageList:IconHeight
                 //nTextFlags := DT_CENTER + DT_VCENTER + DT_SINGLELINE
         ENDCASE
         IF EMPTY( ::xText )
            nTop := ( aRect[4] / 2 ) - ( oImageList:IconHeight / 2 )
         ENDIF
      ENDIF

      IF ::Theming  .AND. ::Application:OsVersion:dwMajorVersion > 4 .AND. ::DrawTheme .AND. IsThemeActive()
         nStyle := PBS_NORMAL

         IF lDisabled
            nStyle := PBS_DISABLED
          ELSEIF lSelected
            nStyle := PBS_PRESSED
          ELSEIF ( dis:itemState & ODS_HOTLIGHT ) != 0 .OR. ( ::__lMouseHover .AND. !lSelected )
            nStyle := PBS_HOT
          ELSEIF lDefault
            nStyle := PBS_DEFAULTED
         ENDIF

         DrawThemeBackground( ::System:hButtonTheme, dis:hDC, BP_PUSHBUTTON, nStyle, aRect, aRect )
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
            IF nColor != NIL .AND. ( nColor != ::__SysBackColor  .OR. ::__ForceSysColor )
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
      IF oImageList != NIL .AND. ::ImageIndex > 0
         IF lDisabled
            oImageList:DrawDisabled( dis:hDC, ::ImageIndex, nLeft, nTop )
          ELSE
            oImageList:DrawImage( dis:hDC, ::ImageIndex, nLeft, nTop )
         ENDIF
      ENDIF
      IF lFocus .AND. ::DrawFocus .AND. ! ::MenuArrow
         SetTextColor( dis:hDC, RGB(0,0,0) )
         SetBkColor( dis:hDC, ::BackColor )
         _DrawFocusRect( dis:hDC, { dis:rcItem:Left + 4, dis:rcItem:Top + 4, dis:rcItem:Right - 4, dis:rcItem:Bottom - 4 } )
      ENDIF
      IF ! EMPTY( ::xText )
         SetBkMode( dis:hDC, TRANSPARENT )
         SetTextColor( dis:hDC, GetThemeSysColor( ::hWnd, TMT_BTNTEXT ) )

         IF ::Application != NIL .AND. lDisabled
            OffSet( @aTextRect, 1, 1 )
            SetTextColor( dis:hDC, ::System:Colors:BtnHighlight )
            _DrawText( dis:hDC, ::xText, aTextRect, nTextFlags )
            OffSet( @aTextRect, -1, -1 )
            SetTextColor( dis:hDC, ::System:Colors:GrayText )
          ELSE
            SetTextColor( dis:hDC, ::ForeColor )
         ENDIF
      ENDIF
      IF ::MenuArrow .AND. ::ContextMenu != NIL
         ::DrawArrow( dis:hDC, {aTextRect[3]-22,aTextRect[2],aTextRect[3],aTextRect[4]} )
         aTextRect[1] := 6
         //nTextFlags := DT_LEFT + DT_VCENTER + DT_SINGLELINE
      ENDIF
      IF ! EMPTY( ::xText )
         IF ::xMultiLine
            _DrawText( dis:hDC, ::xText, @aRect, DT_CALCRECT )
            aTextRect  := {aTextRect[1],(::xHeight-aRect[4])/2,::xWidth,::xHeight}
            IF lSelected .AND. lFocus
               aTextRect[1]++
               aTextRect[2]++
            ENDIF
            nTextFlags := ( ::Alignment | DT_VCENTER )
         ENDIF
         _DrawText( dis:hDC, ::xText, aTextRect, nTextFlags )
      ENDIF
   ENDIF

RETURN 0

METHOD OnParentCommand( nId, nCode, nlParam ) CLASS Button
   LOCAL pt
   (nId, nCode)
   IF ::MenuArrow .AND. ::ContextMenu != NIL .AND. nlParam == ::hWnd
      pt := (struct POINT)
      pt:x := ::Left
      pt:y := ::Top+::Height
      ClientToScreen( ::Parent:hWnd, @pt )
      ::InvalidateRect()
      ::ContextMenu:Show( pt:x, pt:y )
      ::InvalidateRect()
      RETURN 0
   ENDIF
RETURN NIL

METHOD OnCtlColorBtn( nwParam, nlParam ) CLASS Button
   LOCAL hBkGnd := ::Parent:BkBrush
   (nlParam)
   SetBrushOrgEx( nwParam, ::Parent:ClientWidth-::Left, ::Parent:ClientHeight-::Top )
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

   IF ::ForeColor != NIL .AND. !(::ForeColor == ::__SysForeColor)
      SetTextColor( nwParam, ::ForeColor )
   ENDIF
   IF hBkGnd != NIL
      SetBkMode( nwParam, TRANSPARENT )
      RETURN hBkGnd
    ELSEIF ::ForeColor != NIL .AND. ::ForeColor != ::__SysForeColor
      SetBkMode( nwParam, TRANSPARENT )
      IF ::BackColor == ::__SysBackColor
         RETURN GetSysColorBrush( COLOR_BTNFACE )
      ENDIF
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF
RETURN NIL

METHOD OnMouseHover() CLASS Button
   IF !( ::__xCtrlName == "Button" )
      RETURN NIL
   ENDIF
   ::__lMouseHover := .T.
   ::InvalidateRect(, .F. )
RETURN NIL

METHOD OnMouseLeave() CLASS Button
   IF !( ::__xCtrlName == "Button" )
      RETURN NIL
   ENDIF
   ::__lMouseHover := .F.
   ::InvalidateRect(, .F. )
RETURN NIL

//-------------------------------------------------------------------------------------------------------------------------------------

CLASS CommandLink INHERIT Control
   PROPERTY ShortCutKey   ROOT "Behavior"
   PROPERTY Note          ROOT "behavior" SET ::__SetNote(v) DEFAULT ""
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD __SetNote()
   METHOD OnCtlColorBtn()
ENDCLASS

METHOD Init( oParent ) CLASS CommandLink
   ::__xCtrlName := "CommandLink"
   ::ClsName   := "Button"
   DEFAULT ::Style TO ( WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_COMMANDLINK )
   ::Super:Init( oParent )
   ::Width     := 180
   ::Height    := 45
   IF ::DesignMode
      ::__PropFilter := { "ALLOWMAXIMIZE","BACKCOLOR", "FORECOLOR", "TRANSPARENT", "CLIPCHOLDREN", "CLIPSIBLINGS", "BORDER", "NOACTIVATE", "THEMING" }
   ENDIF
   ::ShortCutKey   := __MenuStripItemShortCut( Self )
RETURN Self

METHOD Create() CLASS CommandLink
   Super:Create()
   ::__SetNote( ::xNote )
RETURN Self

METHOD __SetNote( cNote ) CLASS CommandLink
   IF ::IsWindow()
      ::SendMessage( BCM_SETNOTE, 0, AnsiToWide(cNote) )
   ENDIF
RETURN Self

METHOD OnCtlColorBtn( hDC ) CLASS CommandLink
   LOCAL hBkGnd := ::Parent:BkBrush
   SetBrushOrgEx( hDC, ::Parent:ClientWidth-::Left, ::Parent:ClientHeight-::Top )
RETURN hBkGnd
