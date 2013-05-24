/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Label.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

//-----------------------------------------------------------------------------------------------

CLASS Label INHERIT Control
   DATA Transparent  PUBLISHED INIT .F.

   DATA ImageList    EXPORTED
   DATA ImageIndex   PROTECTED
   DATA AllowUnDock  EXPORTED INIT .F.
   DATA AllowClose   EXPORTED INIT .F.

   DATA EnumAlignment   EXPORTED INIT { {"Left", "Center", "Right"}, {SS_LEFT,SS_CENTER,SS_RIGHT} }

   PROPERTY Alignment                         READ xAlignment  WRITE SetAlign  DEFAULT SS_LEFT PROTECTED INVERT

   PROPERTY Sunken       INDEX SS_SUNKEN      READ xSunken     WRITE SetStyle  DEFAULT .F.     PROTECTED
   PROPERTY Simple       INDEX SS_SIMPLE      READ xSimple     WRITE SetStyle  DEFAULT .F.     PROTECTED
   PROPERTY Noprefix     INDEX SS_NOPREFIX    READ xNoprefix   WRITE SetStyle  DEFAULT .F.     PROTECTED
   PROPERTY Border       INDEX WS_BORDER      READ xBorder     WRITE SetStyle  DEFAULT .F.     PROTECTED
   PROPERTY SunkenText                        READ xSunkenText WRITE SetSText  DEFAULT .F.     PROTECTED
   PROPERTY VertCenter                        READ xVertCenter WRITE SetVCent  DEFAULT .F.     PROTECTED

   // Backward compatibility
   ACCESS CenterText    INLINE ::Alignment == SS_CENTER
   ASSIGN CenterText(l) INLINE ::Alignment := IIF( l, SS_CENTER, SS_LEFT )

   ACCESS RightAlign    INLINE ::Alignment == SS_RIGHT
   ASSIGN RightAlign(l) INLINE ::Alignment := IIF( l, SS_RIGHT, SS_LEFT )

   METHOD Init()  CONSTRUCTOR
   METHOD OnCtlColorStatic()
   METHOD SetParent( oParent ) INLINE IIF( ::__hBrush != NIL, ( DeleteObject( ::__hBrush ), ::__hBrush := NIL ), ), ::Super:SetParent( oParent ), ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   METHOD OnSize(w,l)          INLINE Super:OnSize( w, l ), ::InvalidateRect(, .F. ), NIL
   METHOD Create()             INLINE IIF( ::Transparent, ::Parent:__RegisterTransparentControl( Self ), ), Super:Create()
   METHOD OnParentDrawItem()
   METHOD OnEraseBkGnd()       INLINE 1
   METHOD SetVCent(l)          INLINE ::SetStyle( SS_OWNERDRAW, IIF( ! l .AND. ::SunkenText, .T., l ) )
   METHOD SetStext(l)          INLINE ::SetStyle( SS_OWNERDRAW, IIF( ! l .AND. ::VertCenter, .T., l ) )
   METHOD SetAlign(n)          INLINE IIF( ::hWnd != NIL, ( ::SetStyle( SS_LEFT, .F. ), ::SetStyle( SS_RIGHT, .F. ), ::SetStyle( SS_CENTER, .F. ) ),), ::SetStyle( n, .T. )
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Label
   DEFAULT ::__xCtrlName TO "Label"
   ::ClsName    := "static"
   ::Style      := WS_CHILD | WS_VISIBLE | SS_NOTIFY | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | SS_LEFT
   ::Super:Init( oParent )
   ::Width      := 80
   ::Height     := 16
   ::Events     := ;
          { ;
            {"Command",     {;
                            { "OnClick"            , "", "" } } },;
            {"Color",       {;
                            { "OnCtlColorStatic"   , "", "" } } },;
            {"Drawing",     {;
                            { "OnEraseBkGnd"       , "", "" },;
                            { "OnPaint"            , "", "" } } },;
            {"Layout",      { ;
                            { "OnEnterSizeMove"    , "", "" },;
                            { "OnExitSizeMove"     , "", "" },;
                            { "OnMove"             , "", "" },;
                            { "OnSize"             , "", "" } } },;
            {"Parent",      {;
                            { "OnParentDrawItem"   , "", "" } } },;
            {"Mouse",       {;
                            { "OnLButtonDblClk"    , "", "" },;
                            { "OnLButtonDown"      , "", "" },;
                            { "OnLButtonUp"        , "", "" },;
                            { "OnMButtonDown"      , "", "" },;
                            { "OnMButtonUp"        , "", "" },;
                            { "OnMouseActivate"    , "", "" },;
                            { "OnMouseHover"       , "", "" },;
                            { "OnMouseLeave"       , "", "" },;
                            { "OnMouseMove"        , "", "" },;
                            { "OnRButtonDown"      , "", "" },;
                            { "OnRButtonUp"        , "", "" },;
                            { "OnMouseWheel"       , "", "" } } },;
            {"Control",     {;
                            { "OnCreate"           , "", "" },;
                            { "OnDestroy"          , "", "" },;
                            { "OnEnable"           , "", "" },;
                            { "OnSetText"          , "", "" } } } }
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS Label
   LOCAL nAlign, hBkGnd := ::GetBkBrush(), aRect := {0,0,::xWidth,::xHeight}
   ( nwParam, nlParam )
   IF dis:CtlType == ODT_STATIC .AND. (dis:itemAction & ODA_DRAWENTIRE) == ODA_DRAWENTIRE
      _FillRect( dis:hDC, aRect, hBkGnd  )
      IF ::Sunken
         __Draw3dRect( dis:hDC, aRect, GetSysColor(COLOR_3DLIGHT), GetSysColor(COLOR_3DDKSHADOW) )
      ENDIF
      SetBkMode( dis:hDC, TRANSPARENT )
      
      IF ::SunkenText
         nAlign := ::xAlignment | DT_WORDBREAK
         IF ::xVertCenter
            nAlign := nAlign | DT_VCENTER | DT_SINGLELINE
         ENDIF
         aRect[1] += 1
         aRect[2] += 1
         SetTextColor( dis:hDC, ::System:Color:White )
         _DrawText( dis:hDC, ::xText, aRect, nAlign )
         aRect[1] -= 1
         aRect[2] -= 1
      ENDIF
      SetTextColor( dis:hDC, ::ForeColor )
      _DrawText( dis:hDC, ::xText, aRect, nAlign )
   ENDIF
RETURN NIL

METHOD OnCtlColorStatic( nwParam ) CLASS Label
   LOCAL hBkGnd := ::GetBkBrush()

   IF ::ForeColor != NIL
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

   IF ::Parent:ClsName == "ToolBarWindow32" .OR. ::Transparent
      SetBkMode( nwParam, TRANSPARENT )
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF
RETURN NIL


CLASS Line INHERIT CONTROL
   PROPERTY Lenght         READ xLenght   WRITE __SetLenght   DEFAULT 150 INVERT
   PROPERTY Sunken         READ xSunken   WRITE __SetSunken   DEFAULT .T. INVERT
   PROPERTY Vertical       READ xVertical WRITE __SetVertical DEFAULT .F.

   ACCESS Width            INLINE ::xWidth
   ACCESS Height           INLINE ::xHeight

   DATA xText              EXPORTED  INIT ""
   DATA Text               EXPORTED  INIT ""

   DATA Weight             EXPORTED  INIT 2
   DATA Border             EXPORTED
   DATA IndexOrder         EXPORTED  INIT 0

   DATA Font               EXPORTED
   DATA TabStop            EXPORTED
   DATA BackColor          EXPORTED
   DATA ForeColor          EXPORTED
   DATA AllowClose         EXPORTED
   DATA AllowUndock        EXPORTED
   DATA ClientEdge         EXPORTED
   DATA ClipChildren       EXPORTED
   DATA ClipSiblings       EXPORTED
   DATA OwnerDraw          EXPORTED  INIT .F.
   DATA StaticEdge         EXPORTED
   DATA Transparent        EXPORTED

   DATA HorzScrollSize     EXPORTED  INIT 0
   DATA VertScrollSize     EXPORTED  INIT 0
   DATA AcceptFiles        EXPORTED  INIT .F.
   DATA AnimationStyle     EXPORTED  INIT 0
   DATA ContextMenu        EXPORTED
   DATA AllowMaximize      EXPORTED  INIT .F.
   DATA Enabled            EXPORTED  INIT .T.
   DATA TabOrder           EXPORTED  INIT 0  
   DATA NoActivate         EXPORTED  INIT .F.
   DATA Theming            EXPORTED  INIT .F.

   DATA Color              PUBLISHED INIT GetSysColor( COLOR_BTNSHADOW )

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD OnEraseBkGnd()
   METHOD OnSize()

   METHOD __SetVertical()
   METHOD __SetSunken()
   METHOD __SetLenght()
ENDCLASS

METHOD Init( oParent ) CLASS Line
   DEFAULT ::__xCtrlName TO "Line"
   ::ClsName       := "VxhLine"
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::__lResizeable := {.F.,.T.,.F.,.F.,.F.,.T.,.F.,.F.}
   ::Super:Init( oParent )
   ::__IsStandard  := .F.
   ::Width         := ::Lenght
   ::Height        := ::Weight
   ::Events        := ;
          { ;
            {"Layout",      { ;
                            { "OnEnterSizeMove"    , "", "" },;
                            { "OnExitSizeMove"     , "", "" },;
                            { "OnMove"             , "", "" },;
                            { "OnSize"             , "", "" } } },;
            {"Control",     {;
                            { "OnCreate"           , "", "" },;
                            { "OnDestroy"          , "", "" },;
                            { "OnEnable"           , "", "" } } } }
RETURN Self

METHOD Create() CLASS Line
   Super:Create()
RETURN Self

METHOD __SetVertical( lSet ) CLASS Line
   ::__lResizeable := {.F.,!lSet,.F.,lSet,.F.,!lSet,.F.,lSet}
   IF lSet
      ::xHeight := ::xLenght
      ::xWidth  := ::Weight
    ELSE
      ::xWidth  := ::xLenght
      ::xHeight := ::Weight
   ENDIF
   IF ::hWnd != NIL
      ::MoveWindow()
   ENDIF
RETURN NIL

METHOD __SetSunken( lSet ) CLASS Line
   IF ::xSunken != lSet
      ::Weight := IIF( lSet, 2, 1 )
      IF ::xVertical
         ::xWidth := ::Weight
       ELSE
         ::xHeight := ::Weight
      ENDIF
   ENDIF
   IF ::hWnd != NIL
      ::MoveWindow()
   ENDIF
RETURN NIL

METHOD __SetLenght( nLen ) CLASS Line
   IF ::xLenght != nLen
      IF ::xVertical
         ::xHeight := nLen
       ELSE
         ::xWidth  := nLen
      ENDIF
      IF ::hWnd != NIL
         ::MoveWindow()
      ENDIF
   ENDIF
RETURN NIL

METHOD OnSize( nwParam, nlParam ) CLASS Line
   Super:OnSize( nwParam, nlParam )
   IF ::__ClassInst != NIL
      ::xLenght := IIF( ::xVertical, ::Height, ::Width )
   ENDIF
RETURN NIL
   
METHOD OnEraseBkGnd( hDC ) CLASS Line
   LOCAL lVert := ::xVertical, hBrush := CreateSolidBrush( ::Color )
   _FillRect( hDC, { 0, 0, IIF( lVert, 1, ::Width ), IIF( lVert, ::Height, 1 ) }, hBrush )
   DeleteObject( hBrush )
   IF ::Sunken
      _FillRect( hDC, { IIF( lVert, 1, 0 ), IIF( lVert, 0, 1 ), IIF( lVert, 2, ::Width ), IIF( lVert, ::Height, 2 ) }, GetStockObject( WHITE_BRUSH ) )
   ENDIF
RETURN 1
