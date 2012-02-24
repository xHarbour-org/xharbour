/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// ToolStrip.prg                                                                                        *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

GLOBAL s_lExecuting := .F.
GLOBAL s_CurrFocus
GLOBAL s_CurrentObject
GLOBAL s_lKey := .F.
GLOBAL s_lOpenMenu := .T.
GLOBAL s_hKeyMenuHook

#include "debug.ch"
#include "vxh.ch"

#define DG_ADDCONTROL             1

#define FROMARGB(r,g,b,a)  ((((b)<<16)|(((g)<<16)|(((r)<<16)|(((a)<<16)))

static s_nx, s_ncx, s_nmw, s_Shadow := .F., s_aPixels, s_aRect, s_mousex := 0, s_mousey := 0
static s_oCurrMenuItem, s_PrevFocus
static s_hMenuDialogHook
static s_MenuhWnd
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ToolStripContainer INHERIT Control
   PROPERTY Position                                READ xPosition      WRITE __SetPosition DEFAULT 2

   PROPERTY Left          INDEX 1                   READ xLeft          WRITE __SetSizePos  DEFAULT 0   HIDDEN
   PROPERTY Top           INDEX 2                   READ xTop           WRITE __SetSizePos  DEFAULT 0   HIDDEN
   PROPERTY Width         INDEX 3                   READ xWidth         WRITE __SetSizePos  DEFAULT 100 HIDDEN
   PROPERTY Height        INDEX 4                   READ xHeight        WRITE __SetSizePos  DEFAULT 0   HIDDEN

   DATA ImageList        EXPORTED
   DATA Border           EXPORTED INIT .T.
   DATA Dock             EXPORTED
   DATA Anchor           EXPORTED
   DATA Cursor           EXPORTED
   DATA SmallCaption     EXPORTED INIT .F.
   DATA XPTheming        EXPORTED INIT .T.
   DATA AllowMaximize    EXPORTED INIT .F.
   DATA ContextMenu      EXPORTED
   DATA BackColor        EXPORTED
   DATA ForeColor        EXPORTED
   DATA TabOrder         EXPORTED
   DATA ClientEdge       EXPORTED INIT .F.
   DATA ClipChildren     EXPORTED INIT .T.
   DATA ClipSiblings     EXPORTED INIT .T.
   DATA StaticEdge       EXPORTED INIT .F.
   DATA TabStop          EXPORTED
   DATA Transparent      EXPORTED INIT .F.
   DATA Visible          EXPORTED INIT .T.
   DATA HighLightCaption EXPORTED INIT .F.
   DATA __aStrips        EXPORTED INIT {}
   
   DATA __aVertex        PROTECTED
   DATA __aMesh          PROTECTED
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnPaint()
   METHOD OnEraseBkGnd()      INLINE 1
   METHOD OnThemeChanged()    INLINE ::__SetVertex(), ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT ), AEVAL( ::Children, {|o| o:InvalidateRect() } ), NIL
   METHOD OnSize()
   METHOD OnParentSysCommand()
   METHOD OnSysKeyDown()
   METHOD OnParentCommand()
   METHOD __SetVertex()
   METHOD __RefreshLayout()
   METHOD __RefreshPosNo()
   METHOD __AddToolStrip()
   METHOD __SetPosition()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS ToolStripContainer
   ::__xCtrlName   := "ToolStripContainer"
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ClsName       := "ToolStripContainer"
   ::Super:Init( oParent )
   ::ExStyle       := WS_EX_NOACTIVATE
   ::IsContainer   := .T.
   ::Events        := {}
   ::__IsStandard  := .F.
   ::__lResizeable :=  {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
   ::__lMoveable   := .F.
   ::__aVertex     := { {=>}, {=>} }
   ::__aMesh       := { {=>} }
   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "&Add MenuStrip", {|| ::__AddToolStrip( "MenuStrip" ) } },;
                                   { "&Add ToolStrip", {|| ::__AddToolStrip( "ToolStrip" ) } } }
   ENDIF
   ::Dock:Margin := 0
   IF ::Height == 0 .AND. ::__ClassInst != NIL
      ::Height := 30
   ENDIF
RETURN Self

METHOD Create() CLASS ToolStripContainer
   Super:Create()
   ::__SetVertex()
   ::__SetPosition()
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnSize( n, x, y ) CLASS ToolStripContainer
   Super:OnSize( n, x, y )
   ::__SetVertex()
   ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   AEVAL( ::Children, {|o| o:InvalidateRect(, .F. ) } )
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD __SetVertex() CLASS ToolStripContainer
   LOCAL nColor1, nColor2
   nColor1 := ::System:CurrentScheme:ToolStripPanelGradientBegin
   nColor2 := ::System:CurrentScheme:ToolStripPanelGradientEnd

   ::__aVertex[1]:x     := 0
   ::__aVertex[1]:y     := 0
   ::__aVertex[1]:Alpha := 0
   ::__aVertex[1]:Red   := GetRValue( nColor1 ) * 256
   ::__aVertex[1]:Green := GetGValue( nColor1 ) * 256
   ::__aVertex[1]:Blue  := GetBValue( nColor1 ) * 256

   ::__aVertex[2]:x     := ::ClientWidth
   ::__aVertex[2]:y     := ::ClientHeight
   ::__aVertex[2]:Alpha := 0
   ::__aVertex[2]:Red   := GetRValue( nColor2 ) * 256
   ::__aVertex[2]:Green := GetGValue( nColor2 ) * 256
   ::__aVertex[2]:Blue  := GetBValue( nColor2 ) * 256
   
   ::__aMesh[1]:UpperLeft  := 0
   ::__aMesh[1]:LowerRight := 1
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnSysKeyDown( nwParam, nlParam ) CLASS ToolStripContainer
   LOCAL n, nRet
   FOR n := 1 TO LEN( ::Children )
       IF ( nRet := ::Children[n]:OnParentSysKeyDown( nwParam, nlParam ) ) != NIL
          EXIT
       ENDIF
   NEXT n
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnParentSysCommand( nwParam, nlParam ) CLASS ToolStripContainer
   LOCAL n, i, nRet
   IF nwParam == SC_KEYMENU
      FOR n := 1 TO LEN( ::Children )
          IF ( nRet := ::Children[n]:OnParentSysCommand( nwParam, nlParam ) ) == NIL
             FOR i := 1 TO LEN( ::Children[n]:Children )
                 IF ( nRet := ::Children[n]:Children[i]:OnParentSysCommand( nwParam, nlParam ) ) != NIL
                    EXIT
                 ENDIF
             NEXT i
           ELSE
             EXIT
          ENDIF
      NEXT n
   ENDIF
RETURN nRet

//-------------------------------------------------------------------------------------------------------
METHOD OnParentCommand( nId, nCode, nlParam ) CLASS ToolStripContainer
   LOCAL n, i, nRet
   FOR n := 1 TO LEN( ::Children )
       IF ( nRet := ::Children[n]:OnParentCommand( nId, nCode, nlParam ) ) == NIL
          FOR i := 1 TO LEN( ::Children[n]:Children )
              IF ( nRet := ::Children[n]:Children[i]:OnParentCommand( nId, nCode, nlParam ) ) != NIL
                 EXIT
              ENDIF
          NEXT i
        ELSE
          EXIT
       ENDIF
   NEXT n
RETURN nRet

//-------------------------------------------------------------------------------------------------------
METHOD __AddToolStrip( cStrip ) CLASS ToolStripContainer
   ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., Self, cStrip,,,1, {}, } }, ::Application:Project:aUndo )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnPaint( hDC, hMemDC ) CLASS ToolStripContainer
   LOCAL hMemBitmap, hOldBitmap, oChild, hOldBitmap1, hMemDC1
   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ENDIF

   __GradientFill( hMemDC, ::__aVertex, 2, ::__aMesh, 1, 0 )

   IF hMemBitmap != NIL
      FOR EACH oChild IN ::Children
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
          ENDIF

          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )

          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )

          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin, SRCCOPY )

          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )

          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )

      NEXT
   ENDIF

   IF hDC != NIL
      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )
      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF
RETURN 0

//-------------------------------------------------------------------------------------------------------
METHOD __SetPosition() CLASS ToolStripContainer
   LOCAL oBottom, n
   SWITCH ::xPosition
      CASE 1 // Left
         ::Dock:Left   := ::Parent
         ::Dock:Top    := ::Parent
         ::Dock:Right  := NIL
         ::Dock:Bottom := ::Parent
         EXIT

      CASE 2 // Top
         ::Dock:Left   := ::Parent
         ::Dock:Top    := ::Parent
         ::Dock:Right  := ::Parent
         ::Dock:Bottom := NIL
         EXIT

      CASE 3 // Right
         ::Dock:Left   := NIL
         ::Dock:Top    := ::Parent
         ::Dock:Right  := ::Parent
         ::Dock:Bottom := ::Parent
         EXIT

      CASE 4 // Bottom
         ::Dock:Left   := ::Parent
         ::Dock:Top    := NIL
         ::Dock:Right  := ::Parent
         oBottom := ::Parent
         IF ( n := ASCAN( ::Parent:Children, {|o| o:ClsName == "msctls_statusbar32"} ) ) > 0
            oBottom := ::Parent:Children[n]
         ENDIF
         ::Dock:Bottom := oBottom
         EXIT
   END
   IF ::hWnd != NIL
      ::__RefreshLayout()
      ::DockIt()
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __RefreshPosNo() CLASS ToolStripContainer
   LOCAL n, x
   FOR n := 1 TO LEN( ::__aStrips )
       FOR x := 1 TO LEN( ::__aStrips[n] )
           ::__aStrips[n][x]:xRow := n
       NEXT x
   NEXT n
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __RefreshLayout( lLeft ) CLASS ToolStripContainer
   LOCAL aStrip, n, nTop, nLeft, nHeight, nRow, x
   DEFAULT lLeft TO .F.

   nHeight := 0
   nTop    := 2
   nRow    := 1
   FOR EACH aStrip IN ::__aStrips
       IF !lLeft
          nLeft := 2
       ENDIF
       
       x := 0
       FOR n := 1 TO LEN( aStrip )
           aStrip[n]:Top  := nTop
           aStrip[n]:xRow := nRow
           IF !lLeft
              aStrip[n]:Left := nLeft
              nLeft += aStrip[n]:Width + 2
           ENDIF

           x := MAX( x, aStrip[n]:Height )
           aStrip[n]:MoveWindow()
       NEXT
       nRow ++
       nTop += ( x + 2 )//aStrip[1]:Height + 2
       nHeight += ( x + 2 )//MAX( nHeight, aStrip[1]:Top + aStrip[1]:Height + 2 )
   NEXT
   IF nHeight != ::Height
      ::Height := nHeight + 2
      //::Parent:SendMessage( WM_SIZE, 0, MAKELONG( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ToolStrip INHERIT Control
   PROPERTY ImageList   READ xImageList      WRITE __SetImageList PROTECTED
   PROPERTY ShowChevron READ xShowChevron    WRITE __ShowChevron  PROTECTED DEFAULT .T. INVERT
   PROPERTY ShowGrip    READ xShowGrip       WRITE __ShowGrip     PROTECTED DEFAULT .T. INVERT

   PROPERTY ImagePadding READ xImagePadding                       PROTECTED DEFAULT 4
   
   DATA xRow PROTECTED INIT 1
   ACCESS Row         INLINE ::xRow PERSISTENT
   ASSIGN Row(n)      INLINE ::__SetRow(n), ::xRow := n, IIF( ::Parent != NIL .AND. ::Parent:ClsName == "ToolStripContainer",;
                             (::Parent:__RefreshPosNo(), ::Parent:__RefreshLayout(), ::MoveWindow()),)

   ACCESS __lMoveable INLINE ::Parent:ClsName != "ToolStripContainer"
   ACCESS ImageList   INLINE __ChkComponent( Self, ::xImageList )     PERSISTENT
   
   PROPERTY Cursor    READ xCursor WRITE __SetWindowCursor DEFAULT IDC_ARROW PROTECTED HIDDEN
   DATA Border         EXPORTED INIT .T.
   //DATA Dock           EXPORTED
   DATA Anchor         EXPORTED
   DATA SmallCaption   EXPORTED INIT .F.
   DATA XPTheming      EXPORTED INIT .T.
   DATA AllowMaximize  EXPORTED INIT .F.
   DATA ContextMenu    EXPORTED
   DATA BackColor      EXPORTED
   DATA ForeColor      EXPORTED
   DATA TabOrder       EXPORTED
   DATA ClientEdge     EXPORTED INIT .F.
   DATA ClipChildren   EXPORTED INIT .T.
   DATA ClipSiblings   EXPORTED INIT .T.
   DATA StaticEdge     EXPORTED INIT .F.
   DATA TabStop        EXPORTED
   DATA Transparent    EXPORTED INIT .F.
   DATA Visible        EXPORTED INIT .T.
   DATA HighLightCaption EXPORTED INIT .F.
   
   DATA __lIsMenu      EXPORTED INIT .F.
   DATA __nLeft        EXPORTED
   DATA __aChevron1    PROTECTED
   DATA __aChevron2    PROTECTED
   DATA __aChevron3    PROTECTED
   DATA __aVertex1     PROTECTED
   DATA __aVertex2     PROTECTED
   DATA __aVertex3     PROTECTED
   DATA __aMesh        PROTECTED
   DATA __OnGripper    PROTECTED INIT .F.
   DATA __GripperPos   EXPORTED  INIT 3
   DATA __ChevronWidth PROTECTED INIT 13
   DATA __PrevPos      PROTECTED
   DATA __LastLeft     PROTECTED INIT 0
   DATA __LastTop      PROTECTED INIT 0
   DATA __PrevRow      PROTECTED INIT 0
   DATA __lOnCaption   PROTECTED INIT .F.
   DATA __nWidth       PROTECTED INIT 0
   DATA __PrevSize     PROTECTED INIT 0
   DATA __LastY        PROTECTED INIT 0
   DATA __LastX        PROTECTED INIT 0
   DATA __DesignAddNew EXPORTED
   
   PROPERTY Height         READ xHeight WRITE __SetHeight DEFAULT 25 MIN 25

   PROPERTY Left   INDEX 1 READ xLeft   WRITE __SetSizePos DEFAULT 2   //HIDDEN
   PROPERTY Top    INDEX 2 READ xTop    WRITE __SetSizePos DEFAULT 2   //HIDDEN
   PROPERTY Width  INDEX 3 READ xWidth  WRITE __SetSizePos DEFAULT 20  HIDDEN
   PROPERTY Float          READ xFloat  WRITE __SetFloat   DEFAULT .F. HIDDEN
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnEraseBkGnd() INLINE 1
   METHOD OnPaint()
   METHOD OnThemeChanged()    INLINE ::__SetVertex(),;
                                     ::SetWindowPos( , 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER ),;
                                     ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT ),;
                                     AEVAL( ::Children, {|o| o:InvalidateRect() } ), NIL
   METHOD OnParentSysKeyDown() VIRTUAL
   METHOD OnParentCommand()
   METHOD OnSize()
   METHOD OnMove()
   METHOD OnMouseMove()
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnNCCalcSize()
   METHOD OnNCPaint()
   METHOD OnNCHitTest()
   METHOD OnNCLButtonDown()
   METHOD OnNCMouseMove()
   METHOD OnNCLButtonUp()
   METHOD OnExitSizeMove()
   METHOD OnDestroy()
   METHOD __AddToolStripItem()
   METHOD __DrawChevron()
   METHOD __SetChevronVertex()
   METHOD __SetVertex()
   METHOD __SetRow()
   METHOD __SetFloat()
   METHOD __SetImageList()    INLINE IIF( ::hWnd != NIL, AEVAL( ::Children, {|o| o:InvalidateRect() } ),)
   METHOD __UpdateWidth()
   METHOD __SetHeight()
   METHOD __OnParentSize()
   METHOD __ShowChevron()
   METHOD __ShowGrip()
   METHOD __Enable( lEnable )
ENDCLASS

METHOD __Enable( lEnable ) CLASS ToolStrip
   IF ::hWnd != NIL 
      EnableWindow( ::hWnd, lEnable )
      ::InvalidateRect()
      AEVAL( ::Children, {|o| o:InvalidateRect()} )
   ENDIF
RETURN lEnable

//-------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS ToolStrip
   DEFAULT ::__xCtrlName TO "ToolStrip"
   DEFAULT ::ClsName     TO "ToolStrip"
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Super:Init( oParent )
   ::ExStyle       := WS_EX_NOACTIVATE
   ::IsContainer   := .T.
   ::__IsStandard  := .F.
   ::__lResizeable :=  {.F.,.F.,.F.,.F.,.F., .F. ,.F.,.F.}
   ::__lMoveable   := .F.
   ::Events        := {}
   ::__aVertex1    := { {=>}, {=>} }
   ::__aVertex2    := { {=>}, {=>} }
   ::__aVertex3    := { {=>}, {=>} }
   ::__aMesh       := { {=>} }
   ::__aChevron1   := { {=>}, {=>} }
   ::__aChevron2   := { {=>}, {=>} }
   ::__aChevron3   := { {=>}, {=>} }
   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "Add ToolStrip&Button",   {|| ::__AddToolStripItem( "ToolStripButton" ) } },;
                                   { "Add ToolStrip&ComboBox", {|| ::__AddToolStripItem( "ToolStripComboBox" ) } },;
                                   { "Add ToolStrip&Label",    {|| ::__AddToolStripItem( "ToolStripLabel" ) } } }
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create() CLASS ToolStrip
   Super:Create()

   ::Font:Bold := .T.
   ::__SetVertex()
   IF ::Parent:ClsName == "ToolStripContainer"
      IF !::__lIsMenu
         IF LEN( ::Parent:__aStrips ) < ::Row
            AADD( ::Parent:__aStrips, { Self } )
          ELSE
            AADD( ::Parent:__aStrips[ ::Row ], Self )
         END
       ELSE
         ::xWidth := ::Parent:Width - IIF( ::xShowGrip, (::__GripperPos + 3), 0 )

         IF LEN( ::Parent:__aStrips ) > 0
            AINS( ::Parent:__aStrips, 1, { Self }, .T. )
          ELSE
            AADD( ::Parent:__aStrips, { Self } )
         END
         ::xRow := 1
         ::Parent:__RefreshPosNo()
      ENDIF
      ::Parent:__RefreshLayout()
    ELSE
      ::ShowGrip := .F.
   ENDIF
   ::MoveWindow()
   ::Form:SendMessage( WM_SIZE, 0, MAKELONG( ::Form:ClientWidth, ::Form:ClientHeight ) )

   ::__nWidth := ::Width

   IF ::__ClassInst != NIL
      WITH OBJECT ::__DesignAddNew := IIF( !::__lIsMenu, ToolStripButton(), MenuStripItem() )
         :Caption     := "[ Add New Item ]"
         :Init( Self )
         :Events      := {}
         :Font:Bold   := .T.
         :Action      := {|o| o:Parent:__AddToolStripItem( IIF( !::__lIsMenu, "ToolStripButton", "MenuStripItem" ) ) }
         :Create()
      END
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnParentCommand( nId, nCode, nlParam ) CLASS ToolStrip 
   LOCAL n, i, nRet
   FOR n := 1 TO LEN( ::Children )
       IF ( nRet := ::Children[n]:OnParentCommand( nId, nCode, nlParam ) ) == NIL
          FOR i := 1 TO LEN( ::Children[n]:Children )
              IF ( nRet := ::Children[n]:Children[i]:OnParentCommand( nId, nCode, nlParam ) ) != NIL
                 EXIT
              ENDIF
          NEXT i
        ELSE
          EXIT
       ENDIF
   NEXT n
RETURN nRet

//-------------------------------------------------------------------------------------------------------
METHOD OnDestroy() CLASS ToolStrip
   IF s_CurrentObject != NIL
      __ReleaseMenu( s_CurrentObject, s_CurrentObject:__hMenu )
      s_CurrentObject := NIL
   ENDIF
   IF s_hKeyMenuHook != NIL
      UnhookWindowsHookEx( s_hKeyMenuHook )
      s_hKeyMenuHook := NIL
   ENDIF
   IF s_hMenuDialogHook != NIL
      UnhookWindowsHookEx( s_hMenuDialogHook )
      s_hMenuDialogHook := NIL
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD __SetHeight( x ) CLASS ToolStrip
   ::__SetSizePos( 4, x )
   IF ::hWnd != NIL
      IF ::Parent:ClsName == "ToolStripContainer"
         ::Parent:__RefreshLayout( .T. )
      ENDIF
      AEVAL( ::Children, {|o| IIF( o:__xCtrlName != "ToolStripComboBox", ( o:Height := o:Parent:Height - 3, o:MoveWindow() ), /*o:SelectionHeight := o:Parent:Height - 10*/ (o:Top := ((o:Parent:Height-5) - o:SelectionHeight)/2, o:MoveWindow()) ) } )
   ENDIF
RETURN x

//-------------------------------------------------------------------------------------------------------
METHOD __AddToolStripItem( cType ) CLASS ToolStrip
   ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., Self, cType,,,1, {}, } }, ::Application:Project:aUndo )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __ShowChevron( lShow ) CLASS ToolStrip
   IF lShow != ::xShowChevron
      ::xShowChevron := lShow
      IF lShow
         ::xWidth += (::__ChevronWidth-2)
       ELSE
         ::xWidth -= (::__ChevronWidth-2)
      ENDIF
      IF ::Parent:ClsName == "ToolStripContainer"
         ::Parent:__RefreshLayout()
      ENDIF
      ::MoveWindow()
      ::__nWidth := ::Width
   ENDIF
RETURN Self

METHOD __ShowGrip( lShow ) CLASS ToolStrip
   IF lShow != ::xShowGrip
      ::xShowGrip := lShow
      IF lShow
         ::xWidth += (::__GripperPos + 3)
       ELSE
         ::xWidth -= (::__GripperPos + 3)
      ENDIF
      IF ::Parent:ClsName == "ToolStripContainer"
         ::Parent:__RefreshLayout()
      ENDIF
      ::MoveWindow()
      ::__nWidth := ::Width
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __SetRow( nRow ) CLASS ToolStrip
   LOCAL n, i, nPos, oStrip, x
   IF ::hWnd != NIL .AND. ::Parent:ClsName == "ToolStripContainer"
      IF ( n := ASCAN( ::Parent:__aStrips[ ::Row ], Self,,, .T. ) ) > 0
         FOR x := 1 TO LEN( ::Parent:__aStrips[ ::Row ] )
             IF ::Parent:__aStrips[ ::Row ][x]:__nLeft != NIL
                ::Parent:__aStrips[ ::Row ][x]:Left    := ::Parent:__aStrips[ ::Row ][x]:__nLeft // Reset previous Left
                ::Parent:__aStrips[ ::Row ][x]:__nLeft := NIL
             ENDIF
         NEXT

         ADEL( ::Parent:__aStrips[ ::Row ], n, .T. )
         IF EMPTY( ::Parent:__aStrips[ ::Row ] )
            ADEL( ::Parent:__aStrips, ::Row, .T. )
         ENDIF
         
         IF nRow > 0
            IF LEN( ::Parent:__aStrips ) < nRow
               AADD( ::Parent:__aStrips, { Self } )
             ELSE
               IF /*::__ClassInst == NIL .AND.*/ ( i := ASCAN( ::Parent:__aStrips[ nRow ], {|o| o:Left >= ::Left } ) ) > 0
                  IF ::Parent:__aStrips[ nRow ][i]:Left < ::Left + ::Width + 2
                     ::Parent:__aStrips[ nRow ][i]:__nLeft := ::Parent:__aStrips[ nRow ][i]:Left // Set Previous Left
                     ::Parent:__aStrips[ nRow ][i]:Left := ::Left + ::Width + 2
                     IF ::__ClassInst != NIL
                        ::Parent:__aStrips[ nRow ][i]:MoveWindow()
                     ENDIF
                     oStrip := ::Parent:__aStrips[ nRow ][i]
                     FOR x  := i+1 TO LEN( ::Parent:__aStrips[ nRow ] )
                         IF oStrip:Left + oStrip:Width + 2 >= ::Parent:__aStrips[ nRow ][x]:Left
                            ::Parent:__aStrips[ nRow ][x]:__nLeft := ::Parent:__aStrips[ nRow ][x]:Left // Set Previous Left
                            ::Parent:__aStrips[ nRow ][x]:Left := oStrip:Left + oStrip:Width + 2
                            IF ::__ClassInst != NIL
                               ::Parent:__aStrips[ nRow ][x]:MoveWindow()
                            ENDIF
                         ENDIF
                         oStrip := ::Parent:__aStrips[ nRow ][x]
                     NEXT

                  ENDIF
                  AINS( ::Parent:__aStrips[ nRow ], i, Self, .T. )
                ELSE
                  AADD( ::Parent:__aStrips[ nRow ], Self )
               ENDIF
               IF /*::__ClassInst == NIL .AND.*/ ( nPos := ASCAN( ::Parent:__aStrips[ nRow ], Self,,, .T. ) ) > 1
                  IF ::Left <= ::Parent:__aStrips[ nRow ][nPos-1]:Left + ::Parent:__aStrips[ nRow ][nPos-1]:Width + 2
                     ::__nLeft := ::Left
                     ::Left := ::Parent:__aStrips[ nRow ][nPos-1]:Left + ::Parent:__aStrips[ nRow ][nPos-1]:Width + 2
                     IF ::__ClassInst != NIL
                        ::Parent:__aStrips[ nRow ][i]:MoveWindow()
                     ENDIF
                     
                     oStrip := Self
                     FOR x  := nPos+1 TO LEN( ::Parent:__aStrips[ nRow ] )
                         IF oStrip:Left + oStrip:Width + 2 >= ::Parent:__aStrips[ nRow ][x]:Left
                            ::Parent:__aStrips[ nRow ][x]:__nLeft := ::Parent:__aStrips[ nRow ][x]:Left // Set previous Left
                            ::Parent:__aStrips[ nRow ][x]:Left := ::Parent:__aStrips[ nRow ][x]:__nLeft := oStrip:Left + oStrip:Width + 2
                            IF ::__ClassInst != NIL
                               ::Parent:__aStrips[ nRow ][x]:MoveWindow()
                            ENDIF
                         ENDIF
                         oStrip := ::Parent:__aStrips[ nRow ][x]
                     NEXT

                  ENDIF
               ENDIF
            END
         ENDIF
         IF ::Parent:ClsName == "ToolStripContainer"
            ::Parent:__RefreshLayout( .T. )
         ENDIF
         ::MoveWindow()
      ENDIF
      ::Form:PostMessage( WM_SIZE, 0, MAKELPARAM( ::Form:ClientWidth, ::Form:ClientHeight ) )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __OnParentSize(x,y,hDef) CLASS ToolStrip
   LOCAL oStrip, nPos, i
   IF ::Row > 0 .AND. ::ShowChevron .AND. ::__ClassInst == NIL
      ::Parent:GetClientrect()
      IF ::Parent:ClientWidth < ::Left + ::Width + 2
         //Sizing down, need to check if there is some wasted space
         IF ::Parent:ClientWidth > ::Left
            ::xLeft := MAX( 2, ::Parent:ClientWidth - ::Width - 2 )
            IF ::Parent:ClsName == "ToolStripContainer" .AND. ( nPos := ASCAN( ::Parent:__aStrips[ ::Row ], Self,,, .T. ) ) > 1
               oStrip  := Self
               FOR i  := nPos-1 TO 1 STEP -1
                   IF oStrip:Left <= ::Parent:__aStrips[ ::Row ][i]:Left + ::Parent:__aStrips[ ::Row ][i]:Width + 2

                      IF ::Parent:__aStrips[ ::Row ][1]:Left > 4
                         ::Parent:__aStrips[ ::Row ][i]:Left := oStrip:Left - ::Parent:__aStrips[ ::Row ][i]:Width - 2
                       ELSE
                         // no more room, shrink the strip
                         oStrip:xLeft := ::Parent:__aStrips[ ::Row ][i]:Left + ::Parent:__aStrips[ ::Row ][i]:Width + 2
                      ENDIF

                   ENDIF
                   oStrip := ::Parent:__aStrips[ ::Row ][i]
               NEXT
            ENDIF
            ::MoveWindow()

            // spaces reused
            IF ::Parent:ClientWidth < ::Left + ::Width + 2
               // still smaller, shrink the ToolStrip
               ::Width := ::Parent:ClientWidth - 2 - ::Left
            ENDIF
         ENDIF
       ELSEIF ::Width < ::__nWidth
         ::Parent:GetClientRect()
         ::Width := MIN( ::Parent:ClientWidth - 2 - ::Left, ::__nWidth )
      ENDIF
    ELSEIF UPPER( ::Parent:ClsName ) != "TOOLSTRIPCONTAINER"
      Super:__OnParentSize( x, y, hDef )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnMouseMove( n, x, y ) CLASS ToolStrip
   LOCAL pt, nLeft, nWidth, i, nPos, oStrip, nCur
   
   //::__SetChevronVertex()
   
   IF ::Row > 0 .AND. ::xShowGrip .AND. ::Parent:ClsName == "ToolStripContainer"
      IF x < ::__GripperPos + 4 .AND. !::__OnGripper
         ::__OnGripper := .T.
         ::Cursor := ::System:Cursor:SizeAll
         ::__PrevPos := NIL

       ELSEIF x >= ::__GripperPos + 4 .AND. ::__OnGripper .AND. n != MK_LBUTTON
         ::__OnGripper := .F.
         ::Cursor := ::System:Cursor:Arrow
         ::__PrevPos := NIL
      ENDIF

      IF n == MK_LBUTTON .AND. ::__OnGripper .AND. !EMPTY( ::Parent:__aStrips )
         pt := (struct POINT)
         GetCursorPos( @pt )
         ::Parent:ScreenToClient( @pt )

         IF pt:x > 1 //::__GripperPos
            
            DEFAULT ::__PrevPos TO { pt:x, pt:y }
            IF ::__PrevPos[1] != pt:x
            
               nLeft  := ::Left + ( pt:x - ::__PrevPos[1] )
               
               ::xLeft := MAX( 2, nLeft )
               IF ::xLeft == 2
                  ::MoveWindow()
                  ::Redraw()
                  RETURN 0
               ENDIF
               IF ::xLeft + ::xWidth > ::Parent:ClientWidth - 2
                  ::Left := ::Parent:Width - ::Width - 2
                  RETURN 0
               ENDIF
               
               nPos   := ASCAN( ::Parent:__aStrips[ ::Row ], {|o| o:hWnd == ::hWnd} )
               oStrip := Self
               
               IF pt:x > ::__PrevPos[1] // moving to the right
                  FOR i  := nPos+1 TO LEN( ::Parent:__aStrips[ ::Row ] )
                      IF oStrip:Left + oStrip:Width + 2 >= ::Parent:__aStrips[ ::Row ][i]:Left
                         
                         nWidth := oStrip:Left + oStrip:Width
                         FOR nCur := i TO LEN( ::Parent:__aStrips[ ::Row ] )
                             nWidth += ::Parent:__aStrips[ ::Row ][nCur]:Width + 2
                         NEXT nCur
                         
                         IF nWidth <= ::Parent:ClientWidth - 2
                            ::Parent:__aStrips[ ::Row ][i]:Left := oStrip:Left + oStrip:Width + 2
                          ELSE
                            oStrip:xLeft := ::Parent:__aStrips[ ::Row ][i]:Left - oStrip:Width - 2
                         ENDIF
                         
                      ENDIF
                      oStrip := ::Parent:__aStrips[ ::Row ][i]
                  NEXT
                ELSE //IF nPos > 1 // moving to the left

                  FOR i  := nPos-1 TO 1 STEP -1
                      IF oStrip:Left <= ::Parent:__aStrips[ ::Row ][i]:Left + ::Parent:__aStrips[ ::Row ][i]:Width + 2

                         nLeft := 2
                         FOR nCur := i-1 TO 1 STEP -1
                             nLeft += ::Parent:__aStrips[ ::Row ][nCur]:Width + 2
                         NEXT nCur
                         
                         IF ::Parent:__aStrips[ ::Row ][i]:Left >= nLeft
                            ::Parent:__aStrips[ ::Row ][i]:Left := oStrip:Left - ::Parent:__aStrips[ ::Row ][i]:Width - 2
                          ELSE
                            oStrip:xLeft := ::Parent:__aStrips[ ::Row ][i]:Left + ::Parent:__aStrips[ ::Row ][i]:Width + 2
                         ENDIF

                      ENDIF
                      oStrip := ::Parent:__aStrips[ ::Row ][i]
                  NEXT
               ENDIF
               ::MoveWindow()
               //::InvalidateRect()
               ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
            ENDIF

            DEFAULT ::__PrevPos TO { x, y }
            IF y >= ::Height + 10 .OR. y <= -10
               IF pt:y <= 0 .AND. ::__PrevPos[2] > pt:y .AND. LEN( ::Parent:__aStrips[ ::Row ] ) == 1 // up
                  ::__LastX := x
                  ::Float := .T.

                ELSEIF pt:y <= -10 .AND. ::__PrevPos[2] > pt:y .AND. LEN( ::Parent:__aStrips[ ::Row ] ) > 1 // up
                  DEFAULT nPos TO ASCAN( ::Parent:__aStrips[ ::Row ], {|o| o:hWnd == ::hWnd} )

                  ADEL( ::Parent:__aStrips[ ::Row ], nPos, .T. )
                  AINS( ::Parent:__aStrips, 1, { Self }, .T. )
                  ::xRow  := 1
                  ::Parent:__RefreshPosNo()
                  ::Parent:__RefreshLayout( .T. )
                
                ELSEIF pt:y >= ::Parent:Height .AND. ::__PrevPos[2] < pt:y .AND. LEN( ::Parent:__aStrips[ ::Row ] ) == 1// down
                  // detatch
                  ::__LastX := x
                  ::Float := .T.

                ELSEIF pt:y >= ::Parent:Height
                  ::__SetRow( ::Row+1 )
                  ::Parent:__RefreshPosNo()

                ELSEIF ::__PrevPos[2] != pt:y 
                  IF ::__PrevPos[2] > pt:y .AND. ::Row > 1
                     ::__SetRow( ::Row - IIF( ::Parent:__aStrips[ ::Row-1 ][1]:__lIsMenu, 2, 1 ) )
                     ::Parent:__RefreshPosNo()
                   ELSEIF ::Row < LEN( ::Parent:__aStrips )
                     ::__SetRow( ::Row + IIF( ::Parent:__aStrips[ ::Row+1 ][1]:__lIsMenu, 2, 1 ) )
                     ::Parent:__RefreshPosNo()
                  ENDIF
                     
               ENDIF
            ENDIF
            
            ::__PrevPos := { pt:x, pt:y }
         ENDIF
      ENDIF
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnPaint( hDC, hMemDC ) CLASS ToolStrip
   LOCAL hOldPen, aRect := Array(4)
   LOCAL y, n, nDots := ( ::Height - 6 ) / 4
   LOCAL hMemBitmap, hOldBitmap, oChild, hOldBitmap1, hMemDC1, hSepLight, hSepDark, nLeft, nTop, nBottom

   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ENDIF
   
   IF ::Row > 0
      // Round
      aRect[1] := ::Parent:Drawing:GetPixel( ::Left-1,             ::Top-1 )
      aRect[2] := ::Parent:Drawing:GetPixel( ::Left + ::Width + 1, ::Top-1 )
   ENDIF
   
   __GradientFill( hMemDC, ::__aVertex1, 2, ::__aMesh, 1, 1 )
   __GradientFill( hMemDC, ::__aVertex2, 2, ::__aMesh, 1, 1 )
   __GradientFill( hMemDC, ::__aVertex3, 2, ::__aMesh, 1, 1 )

   IF ::Row > 0
      IF ::xShowChevron
         ::__DrawChevron( hMemDC, ::__ChevronWidth, ::System:CurrentScheme:OverflowButtonGradientBegin,,,::System:CurrentScheme:OverflowButtonGradientEnd )
      ENDIF
      IF ::Parent:ClsName == "ToolStripContainer"
         SetPixel( hMemDC, 0,           0,            aRect[1] )
         SetPixel( hMemDC, 0,           ::Height - 1, aRect[1] )
         SetPixel( hMemDC, ::Width - 1, 0,            aRect[2] )
         SetPixel( hMemDC, ::Width - 1, ::Height - 1, aRect[2] )
      ENDIF
      // Gripper
      IF ::ShowGrip
         y := 4
         FOR n := 1 TO nDots  
             SetPixel( hMemDC, ::__GripperPos + 1, y + 1, ::System:CurrentScheme:GripLight )
             SetPixel( hMemDC, ::__GripperPos + 1, y + 2, ::System:CurrentScheme:GripLight )
             SetPixel( hMemDC, ::__GripperPos + 2, y + 1, ::System:CurrentScheme:GripLight )
             SetPixel( hMemDC, ::__GripperPos + 2, y + 2, ::System:CurrentScheme:GripLight )

             SetPixel( hMemDC, ::__GripperPos,     y + 0, ::System:CurrentScheme:GripDark )
             SetPixel( hMemDC, ::__GripperPos,     y + 1, ::System:CurrentScheme:GripDark )
             SetPixel( hMemDC, ::__GripperPos + 1, y + 0, ::System:CurrentScheme:GripDark )
             SetPixel( hMemDC, ::__GripperPos + 1, y + 1, ::System:CurrentScheme:GripDark )
             y += 4
         NEXT
      ENDIF
   ENDIF
   
   // Border
   hOldPen   := SelectObject( hMemDC, ::System:CurrentScheme:Pen:ToolStripBorder )
   MoveTo( hMemDC, 2,   ::Height-1   )
   LineTo( hMemDC, ::Width-IIF( ::xShowChevron, ::__ChevronWidth, 2 ), ::Height-1  )
   
   IF !::xShowChevron
      MoveTo( hMemDC, ::Width-1,   2 )
      LineTo( hMemDC, ::Width-1, ::Height-2  )
      SetPixel( hMemDC, ::Width-2, ::Height-2, ::System:CurrentScheme:ToolStripBorder )
   ENDIF
   
   SelectObject( hMemDC, hOldPen )

   hSepDark  := ::System:CurrentScheme:Pen:SeparatorDark
   IF ::System:CurrentScheme:SeparatorLight != NIL
      hSepLight := ::System:CurrentScheme:Pen:SeparatorLight
   ENDIF
   nLeft   := IIF( ::ShowGrip, ::__GripperPos + 4, 1 )
   nTop    := 5
   nBottom := ::Height - nTop
   IF ::Float
      nLeft := 0
      nBottom -= 21
   ENDIF
   
   IF hMemBitmap != NIL
      FOR EACH oChild IN ::Children
          DEFAULT oChild:__nSeparator TO 0
          IF oChild:__nSeparator > 0
             hOldPen   := SelectObject( hMemDC, hSepDark )
             MoveTo( hMemDC, nLeft + 2, nTop   )
             LineTo( hMemDC, nLeft + 2, nBottom  )
             SelectObject( hMemDC, hOldPen )
             IF hSepLight != NIL
                hOldPen   := SelectObject( hMemDC, hSepLight )
                MoveTo( hMemDC, nLeft + 3, nTop+1   )
                LineTo( hMemDC, nLeft + 3, nBottom-1  )
                SelectObject( hMemDC, hOldPen )
             ENDIF
          ENDIF
          
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
             oChild:__hBrush := NIL
          ENDIF

          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )

          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )

          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin, SRCCOPY )

          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )

          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )
          
          nLeft += oChild:Width + oChild:__nSeparator
      NEXT
   ENDIF

   IF hDC != NIL
      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )
      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF

RETURN 0

//-------------------------------------------------------------------------------------------------------
METHOD __DrawChevron( hDC, nWidth, nColor1, nColor2, nColor3, nColor4, nColor5 ) CLASS ToolStrip
   LOCAL n

   DEFAULT nColor2 TO nColor1
   DEFAULT nColor3 TO MidColor( nColor1, ::Parent:Drawing:GetPixel( ::Left + ::ClientWidth, 0 ) )
   DEFAULT nColor5 TO nColor4

   __GradientFill( hDC, ::__aChevron1, 2, ::__aMesh, 1, 1 )
   __GradientFill( hDC, ::__aChevron2, 2, ::__aMesh, 1, 1 )
   __GradientFill( hDC, ::__aChevron3, 2, ::__aMesh, 1, 1 )

   SetPixel( hDC, ::Width - (nWidth-1), 0, nColor1 ) 
   SetPixel( hDC, ::Width - (nWidth-0), 0, nColor2 ) 
   SetPixel( hDC, ::Width - (nWidth-1), 1, nColor2 ) 

   SetPixel( hDC, ::Width - 2, 0, nColor3 )
   SetPixel( hDC, ::Width - 2, 1, nColor1 )
   SetPixel( hDC, ::Width - 1, 1, nColor3 )

   SetPixel( hDC, ::Width - (nWidth-1), ::Height - 1, nColor4 )   
   SetPixel( hDC, ::Width - (nWidth-0), ::Height - 1, nColor5 )
   SetPixel( hDC, ::Width - (nWidth-1), ::Height - 2, nColor5 )

   SetPixel( hDC, ::Width - 2, ::Height - 2, nColor4 )
   SetPixel( hDC, ::Width - 1, ::Height - 2, nColor5 )
   SetPixel( hDC, ::Width - 2, ::Height - 1, nColor5 )

   MoveTo( hDC, ::Width - 8, ::Height - 10 )
   LineTo( hDC, ::Width - 3, ::Height - 10 )

   SelectObject( hDC, GetStockObject( WHITE_PEN ) )
   MoveTo( hDC, ::Width - 7, ::Height -  9 )
   LineTo( hDC, ::Width - 2, ::Height -  9 )

   SelectObject( hDC, GetStockObject( BLACK_PEN ) )
   MoveTo( hDC, ::Width - 8, ::Height -  7 )
   LineTo( hDC, ::Width - 3, ::Height -  7 )
   
   MoveTo( hDC, ::Width - 7, ::Height -  6 )
   LineTo( hDC, ::Width - 4, ::Height -  6 )
   
   MoveTo( hDC, ::Width - 6, ::Height -  5 )
   LineTo( hDC, ::Width - 5, ::Height -  5 )

   FOR n := 1 TO 3
       IF n > 1
          SetPixel( hDC, ::Width - (7-n), ::Height -  (3+n), RGB( 255, 255, 255 ) )
       ENDIF
       SetPixel( hDC, ::Width - (6-n), ::Height -  (3+n), RGB( 255, 255, 255 ) )
   NEXT
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __SetChevronVertex() CLASS ToolStrip
   LOCAL aChevron := { ::System:CurrentScheme:OverflowButtonGradientBegin,;
                       ::System:CurrentScheme:OverflowButtonGradientMiddle,;
                       ::System:CurrentScheme:OverflowButtonGradientEnd }
                       
   ::__aChevron1[1]:x     := ::ClientWidth - 11
   ::__aChevron1[1]:y     := 0
   ::__aChevron1[1]:Alpha := 0
   ::__aChevron1[1]:Red   := GetRValue( aChevron[1] ) * 256
   ::__aChevron1[1]:Green := GetGValue( aChevron[1] ) * 256
   ::__aChevron1[1]:Blue  := GetBValue( aChevron[1] ) * 256

   ::__aChevron1[2]:x     := ::ClientWidth
   ::__aChevron1[2]:y     := 12
   ::__aChevron1[2]:Alpha := 0
   ::__aChevron1[2]:Red   := GetRValue( aChevron[2] ) * 256
   ::__aChevron1[2]:Green := GetGValue( aChevron[2] ) * 256
   ::__aChevron1[2]:Blue  := GetBValue( aChevron[2] ) * 256


   ::__aChevron2[1]:x     := ::ClientWidth - 11
   ::__aChevron2[1]:y     := 12
   ::__aChevron2[1]:Alpha := 0
   ::__aChevron2[1]:Red   := GetRValue( aChevron[2] ) * 256
   ::__aChevron2[1]:Green := GetGValue( aChevron[2] ) * 256
   ::__aChevron2[1]:Blue  := GetBValue( aChevron[2] ) * 256

   ::__aChevron2[2]:x     := ::ClientWidth
   ::__aChevron2[2]:y     := ::ClientHeight - 12
   ::__aChevron2[2]:Alpha := 0
   ::__aChevron2[2]:Red   := GetRValue( aChevron[2] ) * 256
   ::__aChevron2[2]:Green := GetGValue( aChevron[2] ) * 256
   ::__aChevron2[2]:Blue  := GetBValue( aChevron[2] ) * 256


   ::__aChevron3[1]:x     := ::ClientWidth - 11
   ::__aChevron3[1]:y     := ::ClientHeight - 12
   ::__aChevron3[1]:Alpha := 0
   ::__aChevron3[1]:Red   := GetRValue( aChevron[2] ) * 256
   ::__aChevron3[1]:Green := GetGValue( aChevron[2] ) * 256
   ::__aChevron3[1]:Blue  := GetBValue( aChevron[2] ) * 256

   ::__aChevron3[2]:x     := ::ClientWidth
   ::__aChevron3[2]:y     := ::ClientHeight
   ::__aChevron3[2]:Alpha := 0
   ::__aChevron3[2]:Red   := GetRValue( aChevron[3] ) * 256
   ::__aChevron3[2]:Green := GetGValue( aChevron[3] ) * 256
   ::__aChevron3[2]:Blue  := GetBValue( aChevron[3] ) * 256
RETURN Self

METHOD __SetVertex() CLASS ToolStrip
   LOCAL aStrip   := { ::System:CurrentScheme:ToolStripGradientBegin,;
                       ::System:CurrentScheme:ToolStripGradientMiddle,;
                       ::System:CurrentScheme:ToolStripGradientEnd }
                       

   ::__aVertex1[1]:x     := 0
   ::__aVertex1[1]:y     := 0
   ::__aVertex1[1]:Alpha := 0
   ::__aVertex1[1]:Red   := GetRValue( aStrip[1] ) * 256
   ::__aVertex1[1]:Green := GetGValue( aStrip[1] ) * 256
   ::__aVertex1[1]:Blue  := GetBValue( aStrip[1] ) * 256

   ::__aVertex1[2]:x     := ::ClientWidth
   ::__aVertex1[2]:y     := 12
   ::__aVertex1[2]:Alpha := 0
   ::__aVertex1[2]:Red   := GetRValue( aStrip[2] ) * 256
   ::__aVertex1[2]:Green := GetGValue( aStrip[2] ) * 256
   ::__aVertex1[2]:Blue  := GetBValue( aStrip[2] ) * 256


   ::__aVertex2[1]:x     := 0
   ::__aVertex2[1]:y     := 12
   ::__aVertex2[1]:Alpha := 0
   ::__aVertex2[1]:Red   := GetRValue( aStrip[2] ) * 256
   ::__aVertex2[1]:Green := GetGValue( aStrip[2] ) * 256
   ::__aVertex2[1]:Blue  := GetBValue( aStrip[2] ) * 256

   ::__aVertex2[2]:x     := ::ClientWidth
   ::__aVertex2[2]:y     := ::ClientHeight - 12
   ::__aVertex2[2]:Alpha := 0
   ::__aVertex2[2]:Red   := GetRValue( aStrip[2] ) * 256
   ::__aVertex2[2]:Green := GetGValue( aStrip[2] ) * 256
   ::__aVertex2[2]:Blue  := GetBValue( aStrip[2] ) * 256
   
   
   ::__aVertex3[1]:x     := 0
   ::__aVertex3[1]:y     := ::ClientHeight - 12
   ::__aVertex3[1]:Alpha := 0
   ::__aVertex3[1]:Red   := GetRValue( aStrip[2] ) * 256
   ::__aVertex3[1]:Green := GetGValue( aStrip[2] ) * 256
   ::__aVertex3[1]:Blue  := GetBValue( aStrip[2] ) * 256

   ::__aVertex3[2]:x     := ::ClientWidth
   ::__aVertex3[2]:y     := ::ClientHeight
   ::__aVertex3[2]:Alpha := 0
   ::__aVertex3[2]:Red   := GetRValue( aStrip[3] ) * 256
   ::__aVertex3[2]:Green := GetGValue( aStrip[3] ) * 256
   ::__aVertex3[2]:Blue  := GetBValue( aStrip[3] ) * 256

   ::__aMesh[1]:UpperLeft  := 0
   ::__aMesh[1]:LowerRight := 1
   ::__SetChevronVertex()
RETURN Self



//-------------------------------------------------------------------------------------------------------
METHOD OnLButtonDown( nwParam, x, y ) CLASS ToolStrip
   ::__PrevPos := NIL

   ::__LastY := y
   ::__LastX := x
   
   IF ::Row > 0 .AND. ::xShowGrip .AND. ::Parent:ClsName == "ToolStripContainer"
      ::SetCapture()
      IF ::__PrevRow > 0 .AND. nwParam == 1500
         ::__PrevRow    := 0 
         ::Left := ::__LastLeft
         ::Top  := IIF( ::__LastTop > 0, ::Parent:Height - ::Height - 2, 2 )
      ENDIF
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnLButtonUp() CLASS ToolStrip
   LOCAL n, i
   IF ::Row > 0 .AND. ::Parent:ClsName == "ToolStripContainer"
      ::ReleaseCapture()
      ::PostMessage( WM_NCLBUTTONUP, HTCAPTION, MAKELONG( 1, 1 ) )
      FOR n := 1 TO LEN( ::Parent:__aStrips )
          FOR i := 1 TO LEN( ::Parent:__aStrips[n] )
              ::Parent:__aStrips[ n ][i]:__nLeft := NIL
          NEXT i
      NEXT n
   ENDIF
RETURN NIL


//-------------------------------------------------------------------------------------------------------
METHOD OnSize( n, x, y ) CLASS ToolStrip
   (n, y)
   ::__PrevSize := x
   IF ::Row > 0 .AND. ::__PrevRow == 0 
      ::__SetVertex()
      ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnMove( x, y ) CLASS ToolStrip
   IF ::Row == 0 .AND. ::__lOnCaption .AND. ::__PrevRow > 0 .AND. ::Parent:ClsName == "ToolStripContainer"
      ::__LastLeft := x
      ::__LastTop  := y
      DEFAULT ::__PrevPos TO { x, y }
      IF y > 0 .AND. ::__PrevPos[2] > y .AND. y - 10 <= ::Parent:Height .AND. x > 0 .AND. x <= ::Parent:Width
         ::Float := .F.
       ELSEIF y <= 0 .AND. y + ::Height - 10 >= 0 .AND. y > ::__PrevPos[2] .AND. x > 0 .AND. x <= ::Parent:Width
         ::Float := .F.
      ENDIF
      ::__PrevPos := { x, y }
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD __SetFloat( lFloat ) CLASS ToolStrip
   LOCAL pt := (struct POINT), pt2 := (struct POINT)
   IF ::__ClassInst != NIL
      RETURN NIL
   ENDIF
   GetCursorPos( @pt )
   IF !lFloat
      ::xRow := ::__PrevRow
      
      ::SendMessage( WM_LBUTTONUP, 0, MAKELPARAM( pt:x, pt:y ) )

      ::SetWindowLong( GWL_EXSTYLE, WS_EX_NOACTIVATE )
      ::SetWindowLong( GWL_STYLE, WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS )

      SetParent( ::hWnd, ::Parent:hWnd )

      ::xWidth  += IIF( ::xShowChevron, ::__ChevronWidth + 1, 3 )
      ::xHeight -= 21
      ::xLeft   := ::__LastLeft
      ::MoveWindow()
      IF ::xShowGrip
         AEVAL( ::Children, {|o| o:Left += (::__GripperPos + 4) } )
      ENDIF
      IF ::__LastTop > 0
         AADD( ::Parent:__aStrips, { Self } )
         ::xRow := LEN( ::Parent:__aStrips )
       ELSE
         AINS( ::Parent:__aStrips, 1, { Self }, .T. )
         ::xRow := 1
         ::Parent:__RefreshPosNo()
      ENDIF
      ::Parent:__RefreshLayout(.T.)
      
      ::__lOnCaption := .F.
      ::__OnGripper := .T.

      ::PostMessage( WM_LBUTTONDOWN, 1500, MAKELPARAM( pt:x, ::__LastY ) )

    ELSE
      ::ReleaseCapture()

      ::__OnGripper := .F.
      ::Cursor := ::System:Cursor:Arrow
      ::__PrevPos := NIL

      ::__PrevRow := ::Row
      ::Row := 0

      ::xWidth  -= IIF( ::xShowChevron, ::__ChevronWidth + 1, 3 )
      ::xHeight += 21
      ::xLeft   := pt:x - ::__LastX
      ::xTop    := pt:y //- MIN( ::__LastY, 15 )
      ::MoveWindow()

      IF ::xShowGrip
         AEVAL( ::Children, {|o| o:Left -= (::__GripperPos + 4) } )
      ENDIF

      SetParent( ::hWnd, NIL )
      ::SetWindowLong( GWL_EXSTYLE, WS_EX_TOOLWINDOW )
      ::SetWindowLong( GWL_STYLE, WS_POPUP | WS_VISIBLE )
      ::SetWindowPos( HWND_TOPMOST, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOSIZE | SWP_NOMOVE )

      ::PostMessage( WM_NCLBUTTONDOWN, HTCAPTION, MAKELPARAM( ::xLeft, ::xTop ) )

   ENDIF
   ::Form:PostMessage( WM_SIZE, 0, MAKELPARAM( ::Form:ClientWidth, ::Form:ClientHeight ) )

RETURN NIL


//-------------------------------------------------------------------------------------------------------
METHOD OnNCPaint() CLASS ToolStrip
   LOCAL hDC, hOldPen, hRegion, hPen, hOldBrush, nColor, aAlign, y, hOldFont, nForeColor, nBackColor
   IF ::Row == 0
      nColor  := ::System:CurrentScheme:MenuBorder
      
      hPen    := CreatePen( PS_SOLID, 1, GetSysColor( COLOR_BTNFACE ) )

      hRegion := CreateRectRgn( 0, 0, ::Width, ::Height )
      hdc := GetDCEx( ::hWnd, hRegion, DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_CLIPCHILDREN | DCX_VALIDATE )

      hOldPen   := SelectObject( hDC, ::System:CurrentScheme:Pen:MenuBorder )
      hOldBrush := SelectObject( hDC, GetStockObject( NULL_BRUSH ) )

      Rectangle( hDC, 0, 0, ::Width, ::Height )
      Rectangle( hDC, 1, 1, ::Width-1, ::Height-1 )

      SelectObject( hDC, hPen )
      hOldFont  := SelectObject( hDC, ::Font:Handle )
      
      Rectangle( hDC, 2, 2, ::Width - 2, ::Height - 2 )
     
      SetPixel( hDC, 2,           2,            nColor )
      SetPixel( hDC, ::Width - 3, 2,            nColor )
      SetPixel( hDC, ::Width - 3, ::Height - 3, nColor )
      SetPixel( hDC, 2,           ::Height - 3, nColor )

      nBackColor := SetBkColor( hDC, nColor )
      nForeColor := SetTextColor( hDC, GetSysColor( COLOR_WINDOW ) )

      aAlign := _GetTextExtentPoint32( hDC, ::Caption )
      y := ( 21 / 2 ) - ( aAlign[2] / 2 )
      _ExtTextOut( hDC, 5, y, ETO_CLIPPED | ETO_OPAQUE, { 3, 3, ::Width - 3, 20 }, ::Caption )

      nBackColor := SetBkColor( hDC, nBackColor )
      nForeColor := SetTextColor( hDC, nForeColor )

      SelectObject( hDC, hOldFont )
      SelectObject( hDC, hOldpen )
      SelectObject( hDC, hOldBrush )
      
      ReleaseDC( ::hWnd, hDC )
      
      DeleteObject( hPen )
      DeleteObject( hRegion )
      
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnNCCalcSize( nwParam, nlParam ) CLASS ToolStrip
   IF ::Row == 0
      TOOLSTRIP_NCCALCSIZE_PARAMS( nlParam, nwParam )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnNCHitTest( x, y ) CLASS ToolStrip
   LOCAL pt
   IF ::Row == 0
      pt := (struct POINT)
      pt:x := x
      pt:y := y
      ::ScreenToClient( @pt )
      IF pt:y < 0
         RETURN HTCAPTION
      ENDIF
   ENDIF
RETURN HTCLIENT

//-------------------------------------------------------------------------------------------------------
METHOD OnNCLButtonDown() CLASS ToolStrip
   IF ::Row == 0 .AND. ::wParam == HTCAPTION .AND. !::__lOnCaption
      ::__lOnCaption := .T.
      ::Cursor := ::System:Cursor:SizeAll
      RETURN 0
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnNCLButtonUp() CLASS ToolStrip
   IF ::Row == 0
      ::__lOnCaption := .F.
      ::Cursor := ::System:Cursor:Arrow
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnNCMouseMove() CLASS ToolStrip
   IF ::Row == 0 .AND. ::__lOnCaption
      ::SendMessage( WM_SYSCOMMAND, SC_MOVE + 2 )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnExitSizeMove() CLASS ToolStrip
   IF ::__lOnCaption .AND. ::Row == 0
      ::__lOnCaption := .F.
      ::Cursor := ::System:Cursor:Arrow
   ENDIF
RETURN NIL

METHOD __UpdateWidth() CLASS ToolStrip
   LOCAL nWidth
   IF LEN( ::Children ) > 0
      AEVAL( ::Children, {|o| nWidth := o:Left + o:Width } )
      ::xWidth := nWidth + IIF( ::xShowChevron, ::__ChevronWidth, 2 ) + 3
      //::xWidth := ATAIL( ::Children ):Left + ATAIL( ::Children ):Width + IIF( ::xShowChevron, ::__ChevronWidth, 2 )
    ELSE
      ::xWidth := 20
   ENDIF
   
   ::MoveWindow()
   ::__nWidth := ::Width
RETURN NIL

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ToolStripItem INHERIT Control
   // PUBLISHED PROPERTIES
   PROPERTY BeginGroup  READ xBeginGroup WRITE __SetBeginGroup DEFAULT .F.                       PROTECTED
   PROPERTY ImageAlign  READ xImageAlign WRITE __SetImageAlign DEFAULT __GetSystem():TextAlignment:Left PROTECTED
   //PROPERTY Cursor                READ xCursor WRITE __SetWindowCursor DEFAULT IDC_ARROW PROTECTED

   DATA Role              PUBLISHED INIT 1
   DATA ShortCutKey       PUBLISHED

   // INTERNALS   
   PROPERTY Left          INDEX 1 READ xLeft    WRITE __SetSizePos      DEFAULT 0   HIDDEN
   PROPERTY Top           INDEX 2 READ xTop     WRITE __SetSizePos      DEFAULT 1   HIDDEN
   PROPERTY Width         INDEX 3 READ xWidth   WRITE __SetSizePos      DEFAULT 25  HIDDEN
   PROPERTY Height        INDEX 4 READ xHeight  WRITE __SetSizePos      DEFAULT 0   HIDDEN
   
   ACCESS ColorSelectedBegin  INLINE ::System:CurrentScheme:ButtonSelectedGradientBegin
   ACCESS ColorSelectedEnd    INLINE ::System:CurrentScheme:ButtonSelectedGradientEnd
   ACCESS ColorSelectedBorder INLINE ::System:CurrentScheme:ButtonSelectedBorder

   ACCESS ColorPressedBegin   INLINE ::System:CurrentScheme:ButtonPressedGradientBegin
   ACCESS ColorPressedEnd     INLINE ::System:CurrentScheme:ButtonPressedGradientEnd
   ACCESS ColorPressedBorder  INLINE ::System:CurrentScheme:ButtonPressedBorder
   
   ACCESS ColorCheckedBegin   INLINE ::System:CurrentScheme:ButtonCheckedGradientBegin
   ACCESS ColorCheckedEnd     INLINE ::System:CurrentScheme:ButtonCheckedGradientEnd
   
   ACCESS ColorPressedText    INLINE ::System:CurrentScheme:ButtonPressedHighlightBorder
   
   DATA __aVertex          PROTECTED
   DATA __aMesh            PROTECTED
   DATA __lSelected        EXPORTED  INIT .F.
   DATA __lPushed          PROTECTED INIT .F.
   DATA __lHidden          PROTECTED INIT .F.
   DATA __hKeyMenuHook     PROTECTED
   DATA __nSeparator       EXPORTED  INIT 0
   DATA __Roles            EXPORTED  INIT { "Button", "CkeckButton", "GroupCheck" }
   DATA Action             EXPORTED
   
   // REMOVED PROPERTIES
   DATA Cursor             EXPORTED
   DATA Border             EXPORTED  INIT .T.
   DATA Dock               EXPORTED
   DATA Anchor             EXPORTED
   DATA SmallCaption       EXPORTED  INIT .F.
   DATA XPTheming          EXPORTED  INIT .T.
   DATA AllowMaximize      EXPORTED  INIT .F.
   DATA ContextMenu        EXPORTED
   DATA BackColor          EXPORTED
   DATA ForeColor          EXPORTED
   DATA TabOrder           EXPORTED
   DATA ClientEdge         EXPORTED  INIT .F.
   DATA ClipChildren       EXPORTED  INIT .T.
   DATA ClipSiblings       EXPORTED  INIT .T.
   DATA StaticEdge         EXPORTED  INIT .F.
   DATA TabStop            EXPORTED
   DATA Transparent        EXPORTED  INIT .F.
   DATA Visible            EXPORTED  INIT .T.
   DATA HighLightCaption   EXPORTED  INIT .F.

   // PRIVATE METHODS - DO NOT PUBLISH
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnNCDestroy()        INLINE ::CallWindowProc(), ::Parent:__UpdateWidth(), NIL
   METHOD OnMouseHover()
   METHOD OnMouseLeave()
   METHOD OnMouseMove()
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnParentCommand()
   METHOD Show()               INLINE ShowWindow( ::hWnd, SW_SHOW ), ::__lHidden := .F.
   METHOD Hide()               INLINE ShowWindow( ::hWnd, SW_HIDE ), ::__lHidden := .T.
   METHOD __OnParentSize()
   METHOD __SetVertex()
   METHOD __SetBeginGroup(l)   INLINE ::__nSeparator := IIF( l, 6, 0 ), ::Parent:__UpdateWidth()
   METHOD __SetImageAlign()
   METHOD __AddToolStripItem()
   METHOD Cancel()
   //METHOD GetControlName()
   METHOD Destroy()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS ToolStripItem
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ExStyle       := WS_EX_NOACTIVATE
   ::__aVertex     := { {=>}, {=>} }
   ::__aMesh       := { {=>} }

   ::Super:Init( oParent )
   ::IsContainer   := .F.
   ::__IsStandard  := .F.
   ::__lResizeable :=  {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
   ::__lMoveable   := .F.
   ::ShortCutKey   := __MenuStripItemShortCut( Self )

   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "&Add MenuStripItem", {|| ::__AddToolStripItem( "MenuStripItem" ) } } }
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create() CLASS ToolStripItem
   LOCAL nLeft

   IF ::__ClassInst != NIL .AND. ::Caption != "[ Add New Item ]" .AND. ::Parent:__DesignAddNew != NIL
      ::Parent:__DesignAddNew:Destroy()
      ::Parent:__DesignAddNew := NIL
   ENDIF


   AEVAL( ::Parent:Children, {|o| nLeft := o:Left + o:Width } )

   DEFAULT nLeft TO IIF( ::Parent:ShowGrip, ::Parent:__GripperPos + 4, 1 )

   ::xLeft := nLeft + ::__nSeparator

   IF ::__xCtrlName != "ToolStripComboBox"
      ::Height := 19
      IF !::Parent:__lIsMenu
         ::Height := ::Parent:Height - 3
      ENDIF
   ENDIF

   ::xCursor := ::System:Cursor:Arrow

   Super:Create()
   
   IF ::__xCtrlName == "ToolStripComboBox"
      //::SelectionHeight := ::Parent:Height - 10
      ::Top := ((::Parent:Height-5) - ::SelectionHeight)/2
   ENDIF

   ::Parent:__UpdateWidth()
   IF ::__xCtrlName != "ToolStripComboBox"
      ::ShortCutKey:SetAccel()
   ENDIF  
   IF ::__ClassInst != NIL .AND. ::Parent:__DesignAddNew == NIL

      WITH OBJECT ::Parent:__DesignAddNew := ToolStripButton()
         :Caption     := "[ Add New Item ]"
         :Init( ::Parent )
         :Font:Bold   := .T.
         :Events      := {}
         :Action      := {|o| o:Parent:__AddToolStripItem( ::ClsName ) }
         :Create()
      END

   ENDIF

RETURN Self

METHOD Destroy() CLASS ToolStripItem
   LOCAL nWidth, n, nPos := ASCAN( ::Parent:Children, Self,,, .T. )
   nWidth := ::Width
   Super:Destroy()
   FOR n := nPos TO LEN( ::Parent:Children )
       ::Parent:Children[n]:xLeft -= nWidth
       ::Parent:Children[n]:MoveWindow()
   NEXT
   ::Parent:__UpdateWidth()
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------
//METHOD GetControlName( cName, lIde ) CLASS ToolStripItem
//   LOCAL o, cProp, n := 1, oControl, lCont, lComp := .T., oForm := ::Form
//   WHILE ::Application != NIL .AND. oForm != NIL
//      cProp := cName + XSTR( n )
//      IF hScan( oForm:Property, {|c| UPPER(c) == UPPER(cProp) } ) == 0
//         EXIT
//      ENDIF
//      n ++
//   ENDDO
//RETURN n
//-----------------------------------------------------------------------------------------------------------------------------

METHOD Cancel() CLASS ToolStripItem
   IF s_lExecuting
      SendMessage( s_CurrentObject:Form:hWnd, WM_CANCELMODE, 0, 0 )
   ENDIF
   IF s_CurrFocus != NIL
      s_CurrFocus:__lSelected := .F.
      s_CurrFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
      s_CurrFocus := NIL
   ENDIF
   UnhookWindowsHookEx( s_hKeyMenuHook )
   s_CurrentObject := NIL
   s_hKeyMenuHook := NIL
   s_lExecuting := .F.
   s_lKey       := .F.
   s_lOpenMenu  := .T.
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __AddToolStripItem( cType ) CLASS ToolStripItem
   ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., Self, cType,,,1, {}, } }, ::Application:Project:aUndo )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnParentCommand( nId, nCode, nlParam ) CLASS ToolStripItem
   LOCAL n, i, nRet
   IF ::Id == nId
      IF VALTYPE( ::Action ) == "B"
         nRet := EVAL( ::Action, Self )
       ELSE
         nRet := ExecuteEvent( "OnClick", Self )
      ENDIF
    ELSE
      FOR n := 1 TO LEN( ::Children )
          IF ( nRet := ::Children[n]:OnParentCommand( nId, nCode, nlParam ) ) == NIL
             FOR i := 1 TO LEN( ::Children[n]:Children )
                 IF ( nRet := ::Children[n]:Children[i]:OnParentCommand( nId, nCode, nlParam ) ) != NIL
                    EXIT
                 ENDIF
             NEXT i
           ELSE
             EXIT
          ENDIF
      NEXT n
   ENDIF
RETURN nRet

//-------------------------------------------------------------------------------------------------------
METHOD __SetImageAlign() CLASS ToolStripItem
   ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __OnParentSize() CLASS ToolStripItem
   IF ::Parent:Row > 0
      IF ::Parent:__PrevSize > ::Parent:Width .AND. !::__lHidden .AND. ::IsWindowVisible() .AND. ::Parent:ClientWidth - IIF( ::Parent:ShowChevron, ::Parent:__ChevronWidth, 0 ) <= ::Left + ::Width + IIF( ::Parent:ShowChevron, 2, IIF( ::Parent:ShowGrip, -1, 0 ) )
         ::Hide()
       ELSEIF ::Parent:__PrevSize <= ::Parent:Width .AND. ::__lHidden .AND. ::Parent:ClientWidth - IIF( ::Parent:ShowChevron, ::Parent:__ChevronWidth, 0 ) >= ::Left + ::Width
         ::Show()
      ENDIF
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD __SetVertex( nColorBegin, nColorEnd, x, y, cx, cy ) CLASS ToolStripItem
   DEFAULT nColorBegin  TO ::ColorSelectedBegin 
   DEFAULT nColorEnd    TO ::ColorSelectedEnd   
   DEFAULT x            TO 0
   DEFAULT y            TO 0
   DEFAULT cx           TO ::ClientWidth
   DEFAULT cy           TO ::ClientHeight
   
   ::__aVertex[1]:x     := x
   ::__aVertex[1]:y     := y
   ::__aVertex[1]:Alpha := 0
   ::__aVertex[1]:Red   := GetRValue( nColorBegin ) * 256
   ::__aVertex[1]:Green := GetGValue( nColorBegin ) * 256
   ::__aVertex[1]:Blue  := GetBValue( nColorBegin ) * 256

   ::__aVertex[2]:x     := cx
   ::__aVertex[2]:y     := cy
   ::__aVertex[2]:Alpha := 0
   ::__aVertex[2]:Red   := GetRValue( nColorEnd ) * 256
   ::__aVertex[2]:Green := GetGValue( nColorEnd ) * 256
   ::__aVertex[2]:Blue  := GetBValue( nColorEnd ) * 256
   
   ::__aMesh[1]:UpperLeft  := 0
   ::__aMesh[1]:LowerRight := 1
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnMouseHover( nwParam ) CLASS ToolStripItem
   IF !s_lKey
      ::__lPushed   := ::__lSelected .AND. nwParam == MK_LBUTTON .AND. !::Parent:__lIsMenu .AND. ::DropDown == 1
      ::__lSelected := .T.

      IF s_CurrFocus != NIL
         s_CurrFocus:__lSelected := .F.
         s_CurrFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
         s_CurrFocus := NIL
      ENDIF
      
      IF s_hKeyMenuHook != NIL
         UnhookWindowsHookEx( s_hKeyMenuHook )
         s_hKeyMenuHook := NIL
      ENDIF

      ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnMouseMove( nwParam, x, y ) CLASS ToolStripItem
   IF nwParam == MK_LBUTTON
      ::__lSelected := _PtInRect( { 0, 0, ::Width, ::Height }, { x, y } )
      ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnMouseLeave() CLASS ToolStripItem
   IF !s_lKey .AND. ( !s_lExecuting .OR. ( !::Parent:__lIsMenu .OR. EMPTY( ::Children ) ) ) .AND. s_hMenuDialogHook == NIL
      ::__lSelected := ::__lPushed
      ::__lPushed   := .F.
      ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnLButtonDown() CLASS ToolStripItem
   ::__lPushed := .T.
   ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   ::SetCapture()
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnLButtonUp() CLASS ToolStripItem
   LOCAL pt
   ::ReleaseCapture()
   s_lExecuting := .T.

   IF ::__lPushed .AND. ::DropDown < 3 .AND. ( !::Parent:__lIsMenu  .OR. EMPTY( ::Children ) )
      
      ::__lPushed   := .F.
      ::__lSelected  := .F.
      
      IF ::Role == 2
         ::xChecked := !::xChecked
      ENDIF

      pt := (struct POINT)
      GetCursorPos( @pt )
      ::ScreenToClient( @pt )

      ::__lSelected := _PtInRect( { 0, 0, ::Width, ::Height }, { pt:x, pt:y } )
      
      IF ::__lSelected
         IF VALTYPE( ::Action ) == "B"
            EVAL( ::Action, Self )
          ELSE
            ExecuteEvent( "OnClick", Self )
         ENDIF
      ENDIF

   ENDIF
   ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   s_lExecuting := .F.
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ToolStripButton INHERIT ToolStripItem
   PROPERTY ImageList  READ xImageList     WRITE __SetImageList              PROTECTED
   PROPERTY ImageIndex READ xImageIndex    WRITE __SetImageIndex DEFAULT  0  PROTECTED
   
   PROPERTY DropDown   READ xDropDown      WRITE __SetDropDown   DEFAULT  1  PROTECTED
   PROPERTY Checked    READ xChecked                             DEFAULT .F.

   ACCESS ImageList        INLINE __ChkComponent( Self, ::xImageList )     PERSISTENT
   
   DATA __DropDown          EXPORTED INIT { "None", "Partial", "Full" }

   DATA __pObjPtr       PROTECTED
   DATA __hMenu         PROTECTED
   DATA __nDropDown     PROTECTED INIT 0
   DATA __hMenu         EXPORTED
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnPaint()
   METHOD OnEraseBkGnd()   INLINE 1
   METHOD SetWindowText()
   METHOD OnLButtonDblClk() INLINE 0
   METHOD OnUserMsg()
   METHOD OnLButtonDown()

   // Private methods
   METHOD __OpenMenu()
   METHOD __CloseMenu()
   METHOD __SetImageIndex()
   METHOD __DrawStripe()
   METHOD __SetDropDown()
   METHOD __DrawShadow()
   METHOD __SetImageList()    INLINE IIF( ::hWnd != NIL, AEVAL( ::Children, {|o| o:InvalidateRect() } ),)
   METHOD __UpdateWidth()     INLINE NIL // Dummy
   METHOD __GetObjById()
   METHOD OnDestroy() INLINE DestroyMenu( ::__hMenu ), NIL
   METHOD __Enable()
ENDCLASS

METHOD __Enable( lEnable ) CLASS ToolStripButton
   IF ::hWnd != NIL
      EnableWindow( ::hWnd, lEnable )
      ::InvalidateRect()
   ENDIF
RETURN lEnable

//-------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS ToolStripButton
   DEFAULT ::__xCtrlName TO "ToolStripButton"
   DEFAULT ::ClsName     TO "ToolStripButton"
   ::Super:Init( oParent )
   ::Width  := 100
   ::Events := {  {"General", { { "OnClick", "", "" } } } }
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create() CLASS ToolStripButton
   LOCAL lpMenuInfo, nAdd, aSize := ::Drawing:GetTextExtentPoint32( STRTRAN( ::Caption, "&" ) )
   nAdd := 12
   IF !::Parent:__lIsMenu
      nAdd := ::Parent:ImagePadding * 2
   ENDIF
   ::xWidth := aSize[1]+nAdd
   IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0 
      IF ::ImageAlign != DT_CENTER
         ::xWidth += ::Parent:ImageList:IconWidth + 1
       ELSE
         ::xWidth := MAX( ::Parent:ImageList:IconWidth + nAdd, ::xWidth )
      ENDIF
   ENDIF
   IF ::DropDown > 1
      ::xWidth += 11
   ENDIF
   Super:Create()
   ::__hMenu := CreatePopupMenu()
   lpMenuInfo := (struct MENUINFO)

   lpMenuInfo:cbSize := lpMenuInfo:SizeOf()
   lpMenuInfo:fMask  := MIM_STYLE
   lpMenuInfo:dwStyle:= MNS_NOTIFYBYPOS
   SetMenuInfo( ::__hMenu, lpMenuInfo )
   lpMenuInfo := NIL
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonDown( nwParam, x, y ) CLASS ToolStripButton
   DO CASE
      CASE ::DropDown == 1
           RETURN Super:OnLButtonDown( nwParam, x, y )
           
      CASE ::DropDown == 2
           IF ::Width - x <= 11
              ::__OpenMenu()
            ELSE
              RETURN Super:OnLButtonDown( nwParam, x, y )
           ENDIF
           
      CASE ::DropDown == 3
           ::__OpenMenu()
   ENDCASE
RETURN 0

//-------------------------------------------------------------------------------------------------------
METHOD __SetDropDown( n ) CLASS ToolStripButton
   LOCAL nLeft
   IF n > 1
      IF ::__nDropDown == 0
         ::__nDropDown := 11
         ::Width += 11
      ENDIF
    ELSE
      IF ::__nDropDown == 11
         ::__nDropDown := 0
         ::Width -= 11
      ENDIF
   ENDIF
   nLeft := IIF( ::Parent:ShowGrip, ::Parent:__GripperPos + 4, 1 )
   AEVAL( ::Parent:Children, {|o| o:xLeft := nLeft + o:__nSeparator, o:MoveWindow(), nLeft := o:Left + o:Width } )
   ::Parent:__UpdateWidth()
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnPaint( hDC, hMemDC ) CLASS ToolStripButton
   LOCAL hOldBrush, hOldPen, aRect := Array(4)
   LOCAL x, n, nDots := ( ::Height - 6 ) / 4
   LOCAL hMemBitmap, hOldBitmap, hPrevFont, nLeft, nTop, nTextFlags, aTextRect, hBorderPen, lEnabled
   static lPaint := .F.
   
   lEnabled := ::Enabled .AND. ::Parent:Enabled

   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ENDIF

   IF ( ::__lSelected .OR. ::xChecked ) .AND. !::ClsName == "ToolStripLabel"
   
      hBorderPen := ::System:CurrentScheme:Pen:ButtonSelectedBorder //::ColorSelectedBorder
      IF ::__lPushed
         IF ::DropDown == 1 .OR. s_hMenuDialogHook == NIL
            ::__SetVertex( ::ColorPressedBegin, ::ColorPressedEnd )
            IF s_hMenuDialogHook == NIL
               hBorderPen := ::System:CurrentScheme:Pen:ButtonSelectedBorder//::ColorSelectedBorder
             ELSE
               hBorderPen := ::System:CurrentScheme:Pen:MenuBorder
            ENDIF
          ELSE
            ::__SetVertex( ::System:CurrentScheme:MenuItemPressedGradientBegin, ::System:CurrentScheme:MenuItemPressedGradientEnd )
            hBorderPen := ::System:CurrentScheme:Pen:MenuBorder
         ENDIF

       ELSEIF !::xChecked
         ::__SetVertex()

       ELSE
         IF ::__lSelected
            ::__SetVertex( ::ColorPressedBegin, ::ColorPressedEnd )
          ELSE
            ::__SetVertex( ::ColorCheckedBegin, ::ColorCheckedEnd )
         ENDIF
      ENDIF

      // Border
      hOldPen   := SelectObject( hMemDC, hBorderPen )
      hOldBrush := SelectObject( hMemDC, GetStockObject( NULL_BRUSH ) )

      __GradientFill( hMemDC, ::__aVertex, 2, ::__aMesh, 1, 1 )
      Rectangle( hMemDC, 0, 0, ::Width, ::Height )
      
      //Paint DropDown
      IF ::DropDown == 2 .AND. s_hMenuDialogHook == NIL
         ::__SetVertex( , , ::Width - 11 )
         __GradientFill( hMemDC, ::__aVertex, 2, ::__aMesh, 1, 1 )
         Rectangle( hMemDC, ::Width - 12, 0, ::Width, ::Height )
      ENDIF
      SelectObject( hMemDC, hOldBrush )
      SelectObject( hMemDC, hOldPen )

    ELSE
      IF !lPaint
         lPaint := .T.
         ::Parent:OnPaint( , hMemDC )
      ENDIF
      //SendMessage( ::Parent:hWnd, WM_PRINTCLIENT, hMemDC, PRF_CLIENT | PRF_ERASEBKGND )
      IF ::__hBrush != NIL
         _FillRect( hMemDC, {0,0,::Width,::Height}, ::__hBrush )
      ENDIF
   ENDIF
   SetBkMode( hMemDC, TRANSPARENT )
   hPrevFont := SelectObject( hMemDC, ::Font:Handle )

   aTextRect  := { 0, 0, ::Width-::__nDropDown, ::Height }   
   nTextFlags := DT_CENTER | DT_VCENTER | DT_SINGLELINE
   x          := 0
   nTop       := 5
   nLeft      := 3

   IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
      DO CASE
         CASE ::ImageAlign == DT_LEFT .AND. !EMPTY( ::Caption )
              nTop  := ( ::Height - ::Parent:ImageList:IconHeight ) / 2
              aTextRect[1] := nLeft + ::Parent:ImageList:IconWidth + 3
              nTextFlags := DT_LEFT | DT_VCENTER | DT_SINGLELINE

         CASE ::ImageAlign == DT_RIGHT
              nLeft := ( ::Width - ::Parent:ImageList:IconWidth ) - 3 - ::__nDropDown
              nTop  := ( ::Height - ::Parent:ImageList:IconHeight ) / 2
              aTextRect[3] := nLeft - 3
              nTextFlags := DT_RIGHT | DT_VCENTER | DT_SINGLELINE

         CASE ::ImageAlign == DT_CENTER .OR. EMPTY( ::Caption )
              nLeft := ( ( ::Width+::__nDropDown - ::Parent:ImageList:IconWidth ) / 2 ) - ::__nDropDown
              n := _GetTextExtentPoint32( hMemDC, ::Caption )[2]
              nTop  := ( ::Height - ::Parent:ImageList:IconHeight - n ) / 2
              aTextRect[2] += ::Parent:ImageList:IconHeight
              nTextFlags := DT_CENTER | DT_VCENTER | DT_SINGLELINE

      ENDCASE
      IF EMPTY( ::Caption )
         nTop := ( ::Height - ::Parent:ImageList:IconHeight ) / 2
      ENDIF

      
      //y := ( ( ::Height - ::Top ) - ::Parent:ImageList:IconHeight ) / 2
      //::Parent:ImageList:DrawImage( hMemDC, ::ImageIndex, 3, y, ILD_TRANSPARENT )
      //x := ::Parent:ImageList:IconWidth + 3
      
      IF !lEnabled
         ::Parent:ImageList:DrawDisabled( hMemDC, ::ImageIndex, Int( nLeft ), Int( nTop ) )
       ELSE
         ::Parent:ImageList:DrawImage( hMemDC, ::ImageIndex, Int( nLeft ), Int( nTop ) )
      ENDIF
   ENDIF

   IF !lEnabled
      SetTextColor( hMemDC, GetSysColor( COLOR_GRAYTEXT ) )
   ENDIF

   _DrawText( hMemDC, ::Caption, /*{x,0,::ClientWidth,::ClientHeight}*/ aTextRect, nTextFlags /*DT_SINGLELINE | DT_CENTER | DT_VCENTER*/ )

   IF ::DropDown > 1
      MoveTo( hMemDC, ::Width - 9, ( ::Height / 2 ) - 1 )
      LineTo( hMemDC, ::Width - 4, ( ::Height / 2 ) - 1 )

      MoveTo( hMemDC, ::Width - 8, ( ::Height / 2 ) - 0 )
      LineTo( hMemDC, ::Width - 5, ( ::Height / 2 ) - 0 )

      MoveTo( hMemDC, ::Width - 7, ( ::Height / 2 ) + 1 )
      LineTo( hMemDC, ::Width - 6, ( ::Height / 2 ) + 1 )
   ENDIF
   
   SelectObject( hMemDC, hPrevFont )
   
   IF hDC != NIL
      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )
      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF

RETURN 0

//-------------------------------------------------------------------------------------------------------
METHOD __SetImageIndex() CLASS ToolStripButton
   LOCAL aSize, nLeft, nAdd
   IF ::hWnd != NIL
      aSize := ::Drawing:GetTextExtentPoint32( STRTRAN( ::Caption, "&" ) )
      nAdd := 12
      IF !::Parent:__lIsMenu
         nAdd := ::Parent:ImagePadding * 2
      ENDIF

      ::xWidth := aSize[1]+nAdd
      IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0 
         IF ::ImageAlign != DT_CENTER
            ::xWidth += ::Parent:ImageList:IconWidth + 1
          ELSE
            ::xWidth := MAX( ::Parent:ImageList:IconWidth + nAdd, ::xWidth )
         ENDIF
      ENDIF
      ::MoveWindow()

      nLeft := IIF( ::Parent:ShowGrip, ::Parent:__GripperPos + 4, 1 )
      AEVAL( ::Parent:Children, {|o| o:xLeft := nLeft, o:MoveWindow(), nLeft := o:Left + o:Width } )
   
      ::Parent:__UpdateWidth()
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
METHOD SetWindowText( cText ) CLASS ToolStripButton
   LOCAL aSize, nAdd, nDiff, n, nPos
   IF VALTYPE( cText ) == "C"
      ::xCaption := cText
   ENDIF
   IF ::hWnd != NIL .AND. !::__IsInstance
      IF VALTYPE( cText ) == "C"
         SetWindowText( ::hWnd, cText )
      ENDIF
      aSize := ::Drawing:GetTextExtentPoint32( STRTRAN( cText, "&" ) )
      nAdd := 12
      IF !::Parent:__lIsMenu
         nAdd := ::Parent:ImagePadding * 2
      ENDIF
      nDiff := (aSize[1]+nAdd) - ::Width
      
      ::Width := aSize[1]+nAdd

      IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0 
         IF ::ImageAlign != DT_CENTER
            ::Width += ::Parent:ImageList:IconWidth + 1
          ELSE
            ::Width := MAX( ::Parent:ImageList:IconWidth + nAdd, ::xWidth )
         ENDIF
      ENDIF
      nPos := ASCAN( ::Parent:Children, Self,,, .T. )
      FOR n := nPos + 1 TO LEN( ::Parent:Children )
          ::Parent:Children[n]:Left += nDiff
      NEXT

      ::Parent:__UpdateWidth()
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
static FUNCTION __ReleaseMenu( Self, hMenu )
   LOCAL oItem
   FOR EACH oItem IN ::Children
       IF oItem:__hMenu != NIL
          DestroyMenu( oItem:__hMenu )
          oItem:__hMenu := NIL
       ENDIF
       IF IsMenu( hMenu )
          DeleteMenu( hMenu, 0, MF_BYPOSITION )
       ENDIF
       IF oItem:__pObjPtr != NIL
          ReleaseArrayPointer( oItem:__pObjPtr )
          oItem:__pObjPtr := NIL
       ENDIF
       __ReleaseMenu( oItem, oItem:__hMenu )
   NEXT
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
STATIC FUNCTION __SetSubMenu( Self, hMenu )
   LOCAL n, mii, oItem
   
   IF ::__ClassInst != NIL 
      IF ( n := ASCAN( ::Children, {|o| o:Caption == "[ Add New Item ]"} ) ) > 0
         oItem := ::Children[n]
         ADEL( ::Children, n, .T. )
         AADD( ::Children, oItem )
      ENDIF
   ENDIF
   
   FOR EACH oItem IN ::Children
       IF oItem:__pObjPtr != NIL
          EXIT
       ENDIF

       IF ::__ClassInst != NIL .AND. oItem:Caption != "[ Add New Item ]" .AND. ASCAN( oItem:Children, {|o| o:Caption == "[ Add New Item ]"} ) == 0
          WITH OBJECT MenuStripItem()
             :Caption   := "[ Add New Item ]"
             :GenerateMember := .F.
             :Init( oItem )
             :Font:Bold := .T.
             :Events    := {}
             //:Action    := {|o| ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., o:Parent, "MenuStripItem",,,1, {}, } }, ::Application:Project:aUndo ) }
             :Create()
          END
       ENDIF
       IF LEN( oItem:Children ) > 0
          oItem:__hMenu := CreateMenu()
       ENDIF

       mii := {=>}
       mii:fMask         := MIIM_DATA | MIIM_ID | MIIM_STATE | MIIM_TYPE
       mii:hSubMenu      := oItem:__hMenu
       IF LEN( oItem:Children ) > 0
          mii:fMask      := mii:fMask | MIIM_SUBMENU
       ENDIF
       mii:wID           := oItem:Id
       mii:fType         := MFT_OWNERDRAW
       mii:fState        := IIF( oItem:Enabled, MFS_ENABLED, MFS_DISABLED )
       mii:hbmpChecked   := 0
       mii:hbmpUnchecked := 0
       mii:dwTypeData    := oItem:Caption
       mii:hBmpItem      := NIL
       mii:dwItemData    := oItem:__pObjPtr := ArrayPointer( oItem )

       __InsertMenuStripItem( hMenu, -1, .T., mii:fMask, mii:hSubMenu, mii:wID, mii:dwTypeData, mii:dwItemData, mii:fState )
   NEXT
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
METHOD __OpenMenu() CLASS ToolStripButton
RETURN ___OpenMenu( Self )

FUNCTION ___OpenMenu( Self )
   LOCAL hMonitor, mix, lChildren, aPt
   
   s_PrevFocus := NIL

   lChildren := !EMPTY( ::Children )
   s_lOpenMenu := .T.
   IF !lChildren .OR. ::Children[1]:__pObjPtr != NIL
      IF lChildren .AND. ::Children[1]:__pObjPtr != NIL
         ::__lSelected  := .T.
         ::__lPushed    := .F.
         s_lExecuting   := .F.
         ::ReleaseCapture()
      ENDIF
      ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
      RETURN Super:OnLButtonDown()
   ENDIF
   
   ::__lPushed   := .T.
   ::__lSelected := .T.
   s_hMenuDialogHook := 0
   ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )

   ::SetCapture()
   s_lExecuting := .T.
   
   __SetSubMenu( Self, ::__hMenu )

   aPt := { ::Left, ::Top + ::Height - 1 }
   _ClientToScreen( ::Parent:hWnd, @aPt )
   

   IF ( hMonitor := _MonitorFromPoint( aPt, MONITOR_DEFAULTTONEAREST ) ) != 0
      IF GetMonitorInfo( hMonitor, @mix )
         aPt[1] := MAX( aPt[1], mix:rcMonitor:Left )
         
         s_nmw := 0
         s_nx  := aPt[1]
         s_ncx := mix:rcMonitor:right
      ENDIF
   ENDIF

   //----------------------------------------------------------------------------------------------------------------------------------
   s_CurrFocus     := NIL
   s_CurrentObject := Self

   s_hMenuDialogHook := SetWindowsHookEx( WH_CALLWNDPROC, ( @__MenuDialogHook() ), NIL, GetCurrentThreadId() )
   DEFAULT s_hKeyMenuHook TO SetWindowsHookEx( WH_MSGFILTER, ( @__KeyMenuHook() ), NIL, GetCurrentThreadId() )

   TrackPopupMenu( ::__hMenu, TPM_LEFTALIGN | TPM_TOPALIGN, aPt[1], aPt[2], 0, ::Form:hWnd )
   s_nmw := 0

   IF !( "Aero" $ ::System:CurrentScheme:Theme )
      RestoreShadow()
   ENDIF
   
   UnhookWindowsHookEx( s_hMenuDialogHook )
   s_hMenuDialogHook := NIL
   
   IF s_CurrFocus == NIL .AND. s_hKeyMenuHook != NIL
      UnhookWindowsHookEx( s_hKeyMenuHook )
      s_hKeyMenuHook    := NIL
   ENDIF
   s_oCurrMenuItem := NIL
   //----------------------------------------------------------------------------------------------------------------------------------
   
   s_lExecuting   := .F.
   ::__lPushed    := .F.
   ::__lSelected  := .F.
   
   s_aPixels := NIL
   s_aRect   := NIL
   
   ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   ::PostMessage( WM_USER + 1027, 0, 0 )
RETURN Self

METHOD __CloseMenu() CLASS ToolStripButton
   //DestroyMenu( ::__hMenu )
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
METHOD OnUserMsg() CLASS ToolStripButton
   DO CASE
      CASE ::Msg == WM_USER + 1027
           __ReleaseMenu( Self, ::__hMenu )
           
      CASE ::Msg == WM_USER + 1028
           ::__OpenMenu()
           
      CASE ::Msg == WM_USER + 1029
           ::__lSelected := .T.
           ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
           s_PrevFocus := s_CurrFocus
   ENDCASE
RETURN NIL

//--------------------------------------------------------------------------------------------------------------------------------
METHOD __DrawStripe( hDC, x, y, cx, cy ) CLASS ToolStripButton
   LOCAL __aVertex1, __aVertex2, __aMesh, nColor1, nColor2, nColor3, n := Int(cx/2)
   // Paint side stripe
   
   nColor1 := ::System:CurrentScheme:ToolStripGradientBegin
   nColor2 := ::System:CurrentScheme:ToolStripGradientMiddle
   nColor3 := ::System:CurrentScheme:ToolStripGradientEnd

   __aMesh       := { {=>} }
   __aMesh[1]:UpperLeft  := 0
   __aMesh[1]:LowerRight := 1

   __aVertex1    := { {=>}, {=>} }
   __aVertex2    := { {=>}, {=>} }

   __aVertex1[1]:x     := x
   __aVertex1[1]:y     := y
   __aVertex1[1]:Alpha := 0
   __aVertex1[1]:Red   := GetRValue( nColor1 ) * 256
   __aVertex1[1]:Green := GetGValue( nColor1 ) * 256
   __aVertex1[1]:Blue  := GetBValue( nColor1 ) * 256

   __aVertex1[2]:x     := x + n
   __aVertex1[2]:y     := cy
   __aVertex1[2]:Alpha := 0
   __aVertex1[2]:Red   := GetRValue( nColor2 ) * 256
   __aVertex1[2]:Green := GetGValue( nColor2 ) * 256
   __aVertex1[2]:Blue  := GetBValue( nColor2 ) * 256

   __aVertex2[1]:x     := x + n
   __aVertex2[1]:y     := y
   __aVertex2[1]:Alpha := 0
   __aVertex2[1]:Red   := GetRValue( nColor2 ) * 256
   __aVertex2[1]:Green := GetGValue( nColor2 ) * 256
   __aVertex2[1]:Blue  := GetBValue( nColor2 ) * 256

   __aVertex2[2]:x     := cx
   __aVertex2[2]:y     := cy
   __aVertex2[2]:Alpha := 0
   __aVertex2[2]:Red   := GetRValue( nColor3 ) * 256
   __aVertex2[2]:Green := GetGValue( nColor3 ) * 256
   __aVertex2[2]:Blue  := GetBValue( nColor3 ) * 256

   __GradientFill( hDC, __aVertex1, 2, __aMesh, 1, 0 )
   __GradientFill( hDC, __aVertex2, 2, __aMesh, 1, 0 )
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
STATIC FUNCTION __MenuDialogProc( hWnd, nMsg, nwParam, nlParam )
   LOCAL aPt, aRect, hdc, pProc, hOldBrush, hOldPen, n, Self := s_CurrentObject
   
   pProc := GetProp( hWnd, "PROP_WND_PROC" )

   SWITCH nMsg
      CASE WM_NCCALCSIZE
           SET_NCCALCSIZE_PARAMS( nlParam )
           RETURN 0

      CASE WM_WINDOWPOSCHANGING
           n := 0
           IF s_nx != NIL .AND. ( s_nx + s_nmw ) > s_ncx
              n := s_ncx - s_nmw + 5
           ENDIF
           SET_WINDOWPOS( nlParam, n, 5, 2 )
           RETURN 0

           
      CASE WM_NCPAINT
           //IF nwParam == 1
              aRect := _GetWindowRect( hWnd )

              hdc       := GetWindowDC( hWnd )
              hOldPen   := SelectObject( hDC, ::System:CurrentScheme:Pen:MenuBorder )
              hOldBrush := SelectObject( hDC, ::System:CurrentScheme:Brush:ToolStripDropDownBackground )
              
              Rectangle( hDC, 0, 0, aRect[3]-aRect[1], aRect[4]-aRect[2] )
              
              SelectObject( hDC, hOldPen )
              SelectObject( hDC, hOldBrush )

              aPt := { ::Left + 1, ::Top }
              _ClientToScreen( ::Parent:hWnd, @aPt )
              _ScreenToClient( hWnd, @aPt )
              
              // merge menu to strip item
              hOldPen   := SelectObject( hDC, ::System:CurrentScheme:Pen:ToolStripDropDownBackground )
              Rectangle( hDC, aPt[1] + 1, 0, aPt[1] + ::Width - 1, 1 )
              
              SelectObject( hDC, hOldPen )
              ReleaseDC( hWnd, hdc)
           //ENDIF
           RETURN 0

      CASE WM_ERASEBKGND
           IF !( "Aero" $ ::System:CurrentScheme:Theme )
              ::__DrawShadow()
           ENDIF
           RETURN 1

      CASE WM_NCDESTROY
           SetWindowLong( hWnd, GWL_WNDPROC, pProc )
           EXIT
   END
RETURN CallWindowProc( pProc, hWnd, nMsg, nwParam, nlParam )

//--------------------------------------------------------------------------------------------------------------------------------

METHOD __DrawShadow() CLASS ToolStripButton
   LOCAL hDC, pixel, x, y, ix, iy, n, pt
   LOCAL hMemDC, hMemBitmap, hOldBitmap
   // temporarily disabled until moved to C for better performance

   pt := (struct POINT)
   pt:x := ::Left+::Width
   pt:y := ::Top
   ::Parent:ClientToScreen( @pt )
   s_aRect := { pt:x, pt:y, 4, ::Height-1 }

   hDC        := CreateDC( "DISPLAY" )
   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, s_aRect[3]+1, s_aRect[4] )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap )
   BitBlt( hMemDC, 0, 0, s_aRect[3]+1, s_aRect[4]+1, hDC, s_aRect[1], s_aRect[2], SRCCOPY )

   ix := 4
   iy := 0 

   DEFAULT s_aPixels TO {}
   
   FOR x := 1 TO 4
       FOR y := 4 TO 7
           IF ( n := ASCAN( s_aPixels, {|a| a[1]==ix-x .AND. a[2]==y+iy} ) ) > 0
              pixel := s_aPixels[n][3]
            ELSE
              pixel := GetPixel( hMemDC, ix-x, y+iy )
              AADD( s_aPixels, { ix-x, y+iy, pixel } )
           ENDIF
           SetPixel( hMemDC, ix-x, y+iy, DarkenColorXP( 2* 3 * x * (y - 3), pixel ) )
       NEXT
       FOR y := 8 TO s_aRect[4]-1
           IF ( n := ASCAN( s_aPixels, {|a| a[1]==ix-x .AND. a[2]==y+iy} ) ) > 0
              pixel := s_aPixels[n][3]
            ELSE
              pixel := GetPixel( hMemDC, ix-x, y+iy )
              AADD( s_aPixels, { ix-x, y+iy, pixel } )
           ENDIF
           SetPixel( hMemDC, ix-x, y+iy, DarkenColorXP( 2*15 * x, pixel ) )
       NEXT
   NEXT
   BitBlt( hDC, s_aRect[1], s_aRect[2], s_aRect[1]+s_aRect[3]+1, s_aRect[2]+s_aRect[4]+1, hMemDC, 0, 0, SRCCOPY )

   SelectObject( hMemDC, hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )
   DeleteDC( hDC )

RETURN NIL           


//-------------------------------------------------------------------------------------------------------
FUNCTION __KeyMenuHook( nCode, nwParam, nlParam )
   LOCAL pt, hWnd, oObj, pPtr, n, ms_hwnd, ms_message, ms_wParam, ms_lParam, mii_dwItemData
   LOCAL aParams, oMenu, nItem, hMenu, oItem, i, nCurr

   aParams    := __GetMSG( nlParam )
   ms_hwnd    := aParams[1]
   ms_message := aParams[2]
   ms_wParam  := aParams[3]
   ms_lParam  := aParams[4]

   SWITCH ms_message
      CASE WM_MENUSELECT
           mii_dwItemData := MENUITEMINFOITEMDATA( ms_lParam, LOWORD( ms_wParam ), ( HIWORD( ms_wParam ) & MF_POPUP ) == MF_POPUP )
           s_oCurrMenuItem := NIL
           IF mii_dwItemData != NIL .AND. mii_dwItemData <> 0 .AND. ( s_oCurrMenuItem := ArrayFromPointer( mii_dwItemData ) ) != NIL
              __SetSubMenu( s_oCurrMenuItem, s_oCurrMenuItem:__hMenu )
              //__GetApplication():Project:CurrentForm:SelectControl( s_oCurrMenuItem, .F. )
              //__GetApplication():ObjectTree:Set( s_oCurrMenuItem )
           ENDIF
           EXIT

      CASE WM_MOUSEMOVE
           IF s_CurrFocus == NIL .OR. s_PrevFocus != NIL
              pt := _GetCursorPos()

              IF s_mousex != pt[1] .OR. s_mousey != pt[2]
                 s_mousex := pt[1]
                 s_mousey := pt[2]

                 hWnd := _WindowFromPoint( pt )
                 IF hWnd != 0 .AND. s_CurrentObject:Parent != NIL
                    IF !( hWnd == s_CurrentObject:hWnd ) .AND. GetParent( hWnd ) == s_CurrentObject:Parent:hWnd

                       pPtr := GetProp( hWnd, "PROP_CLASSOBJECT" )

                       IF pPtr != NIL .AND. pPtr != 0
                          oObj := ArrayFromPointer( pPtr )
                          IF VALTYPE( oObj ) == "O" .AND. oObj:Parent:HasMessage("__lIsMenu")

                             IF s_PrevFocus != NIL
                                s_PrevFocus:__lSelected := .F.
                                s_PrevFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
                                s_PrevFocus := NIL
                             ENDIF

                             SendMessage( ms_hWnd, WM_CANCELMODE, 0, 0 )
                             s_lKey := .F.
                             IF !EMPTY( oObj:Children ) .AND. ( oObj:DropDown > 1 .OR. oObj:Parent:__lIsMenu )
                                oObj:PostMessage( WM_USER + 1028 )
                             ENDIF
                             oObj := NIL
                             RETURN 1
                          ENDIF
                       ENDIF

                    ENDIF
                 ENDIF
              ENDIF
           ENDIF
           EXIT

      CASE WM_LBUTTONDOWN
           pt := (struct POINT)
           GetCursorPos( @pt )
           hWnd := WindowFromPoint( pt )
           IF GetClassName( hWnd ) != "#32768" .AND. hWnd != s_CurrentObject:hWnd
              IF s_lExecuting
                 SendMessage( s_CurrentObject:Form:hWnd, WM_CANCELMODE, 0, 0 )
              ENDIF
              IF s_CurrFocus != NIL
                 s_CurrFocus:__lSelected := .F.
                 s_CurrFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
                 s_CurrFocus := NIL
              ENDIF
              UnhookWindowsHookEx( s_hKeyMenuHook )
              s_CurrentObject := NIL
              s_hKeyMenuHook := NIL
              s_lExecuting := .F.
              s_lKey       := .F.
              s_lOpenMenu  := .T.
              RETURN 0
           ENDIF
           EXIT

      CASE WM_LBUTTONUP
           IF s_oCurrMenuItem != NIL .AND. s_oCurrMenuItem:Caption == "[ Add New Item ]"
              __AddNewMenuItem( s_CurrentObject, s_oCurrMenuItem:Parent )
              RedrawWindow( hWnd, , , RDW_FRAME | RDW_UPDATENOW | RDW_INTERNALPAINT )
              RETURN 1
           ENDIF
           EXIT

      CASE WM_SYSKEYDOWN
           IF ms_wParam == VK_MENU
              SendMessage( s_CurrentObject:Form:hWnd, WM_CANCELMODE, 0, 0 )
              s_CurrentObject:__lSelected := .F.
              s_CurrentObject:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
              IF s_CurrFocus == NIL
                 s_CurrentObject := NIL
              ENDIF
              s_CurrFocus := NIL
              UnhookWindowsHookEx( s_hKeyMenuHook )

              s_hKeyMenuHook := NIL

              s_lExecuting := .F.
              s_lKey       := .F.
              s_lOpenMenu  := .T.
              RETURN 0
           ENDIF
           EXIT

      CASE WM_GETDLGCODE
           RETURN DLGC_WANTMESSAGE | DLGC_WANTALLKEYS

      CASE WM_CHAR
           oMenu := s_CurrentObject:Parent
           IF s_oCurrMenuItem != NIL
              oMenu := s_oCurrMenuItem:Parent
           ENDIF

           IF oMenu != NIL
           
              FOR n := 1 TO LEN( oMenu:Children )
                  IF s_oCurrMenuItem == oMenu:Children[n]
                     nCurr := n
                     EXIT
                  ENDIF
              NEXT
              
              IF ( nItem := __GetHotItem( oMenu, nCurr, ms_wParam ) ) > 0
                 oItem := oMenu:Children[ nItem ]
                 
                 IF s_lOpenMenu

                    IF nItem < nCurr
                       FOR i := 1 TO ( nCurr-nItem )
                           PostMessage( s_MenuhWnd, WM_KEYDOWN, VK_UP )
                       NEXT
                     ELSE
                       FOR i := 1 TO ( nItem-nCurr )
                           PostMessage( s_MenuhWnd, WM_KEYDOWN, VK_DOWN )
                       NEXT
                    ENDIF

                    hMenu := oMenu:__hMenu
                    DEFAULT hMenu TO oMenu:__hMenu

                    IF oItem:Enabled
                       IF LEN( oItem:Children ) == 0

                          SendMessage( oMenu:Form:hWnd, WM_CANCELMODE, 0, 0 )
                          PostMessage( oMenu:Form:hWnd, WM_MENUCOMMAND, nItem-1, hMenu )
                          /*
                          oItem:Cancel()
                          IF HGetPos( oItem:EventHandler, "OnClick" ) != 0
                             oForm := oItem:Form
                             oForm:&( oItem:EventHandler[ "OnClick" ] )( oItem )
                           ELSEIF oItem:ClsName == "MenuStripItem" .AND. VALTYPE( oItem:Action ) == "B"
                             EVAL( oItem:Action, oItem )
                          ENDIF
                          */
                        ELSE
                          PostMessage( s_MenuhWnd, WM_KEYDOWN, VK_RIGHT )
                       ENDIF
                    ENDIF
                  ELSE
                    IF oItem:Enabled

                       IF !EMPTY( oItem:Children ) .AND. ( oItem:DropDown > 1 .OR. oItem:Parent:__lIsMenu )
                          IF s_CurrentObject != NIL
                             s_CurrentObject:__lSelected := .F.
                             s_CurrentObject:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
                             s_CurrentObject := NIL
                          ENDIF
                          oItem:PostMessage( WM_USER + 1028 )
                       ENDIF

                    ENDIF
                 ENDIF                 
                 RETURN 1

              ENDIF



           ENDIF
           EXIT

      CASE WM_KEYDOWN
           SWITCH ms_wParam
                   
              CASE VK_LEFT
                   oObj := s_CurrFocus
                   DEFAULT oObj TO s_CurrentObject
                   IF LEN( oObj:Parent:Children ) > 1 .AND. ( s_oCurrMenuItem == NIL .OR. s_oCurrMenuItem:Parent == oObj )
                      IF ( n := ASCAN( oObj:Parent:Children, {|o| o == oObj } ) - 1 ) <= 0
                         n := LEN( oObj:Parent:Children )
                      ENDIF
                      IF s_CurrFocus != NIL
                         s_CurrFocus:__lSelected := .F.
                         s_CurrFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
                         s_CurrFocus := NIL
                       ELSE
                         SendMessage( oObj:Form:hWnd, WM_CANCELMODE, 0, 0 )
                      ENDIF
                      s_lKey := .T.
                      IF s_lOpenMenu .AND. !EMPTY( oObj:Parent:Children[n]:Children ) .AND. ( oObj:Parent:Children[n]:DropDown > 1 .OR. oObj:Parent:__lIsMenu )
                         s_lExecuting := .T.
                         s_CurrFocus  := NIL
                         oObj:Parent:Children[n]:PostMessage( WM_USER + 1028 )
                       ELSE
                         s_lExecuting := .F.
                         s_CurrFocus  := oObj:Parent:Children[n]
                         oObj:Parent:Children[n]:PostMessage( WM_USER + 1029 )
                         s_CurrentObject := oObj:Parent:Children[n]
                      ENDIF
                   ENDIF
                   RETURN 0
                   
              CASE VK_RIGHT
                   oObj := s_CurrFocus
                   DEFAULT oObj TO s_CurrentObject
                   IF LEN( oObj:Parent:Children ) > 1 .AND. ( s_oCurrMenuItem == NIL .OR. LEN( s_oCurrMenuItem:Children ) == 0 )
                      IF ( n := ASCAN( oObj:Parent:Children, {|o| o == oObj } ) + 1 ) > LEN( oObj:Parent:Children )
                         n := 1
                      ENDIF
                      IF s_CurrFocus != NIL
                         s_CurrFocus:__lSelected := .F.
                         s_CurrFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
                         s_CurrFocus := NIL
                       ELSE
                         SendMessage( oObj:Form:hWnd, WM_CANCELMODE, 0, 0 )
                      ENDIF
                      s_lKey := .T.
                      
                      IF s_lOpenMenu .AND. !EMPTY( oObj:Parent:Children[n]:Children ) .AND. ( oObj:Parent:Children[n]:DropDown > 1 .OR. oObj:Parent:__lIsMenu )
                         s_lExecuting := .T.
                         s_CurrFocus  := NIL
                         oObj:Parent:Children[n]:PostMessage( WM_USER + 1028 )
                       ELSE
                         s_lExecuting := .F.
                         s_CurrFocus  := oObj:Parent:Children[n]
                         oObj:Parent:Children[n]:PostMessage( WM_USER + 1029 )
                         s_CurrentObject := oObj:Parent:Children[n]
                      ENDIF
                   ENDIF
                   RETURN 0
                   
              CASE VK_DOWN
                   oObj := s_CurrFocus
                   DEFAULT oObj TO s_CurrentObject
              
                   IF !s_lOpenMenu .AND. !EMPTY( oObj:Children ) .AND. ( oObj:DropDown > 1 .OR. oObj:Parent:__lIsMenu )
                      s_lKey := .T.

                      s_lExecuting := .T.
                      oObj:PostMessage( WM_USER + 1028 )

                      IF s_CurrFocus != NIL
                         s_CurrFocus:__lSelected := .F.
                         s_CurrFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
                         s_CurrFocus := NIL
                      ENDIF
                      RETURN 1
                   ENDIF
                   EXIT
                   
              CASE VK_ESCAPE
                   s_lExecuting := .F.
                   s_lKey       := .F.
                   s_lOpenMenu  := .T.
                   
                   IF s_CurrFocus != NIL
                      s_CurrFocus:__lSelected := .F.
                      s_CurrFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
                      s_CurrFocus := NIL
                      s_CurrentObject := NIL
                      UnhookWindowsHookEx( s_hKeyMenuHook )
                      s_hKeyMenuHook := NIL

                      s_lExecuting := .F.
                      s_lKey       := .F.
                      s_lOpenMenu  := .T.
                      
                    ELSEIF s_CurrentObject:ClsName != "ContextStrip"
                      SendMessage( s_CurrentObject:Form:hWnd, WM_CANCELMODE, 0, 0 )
                      s_CurrFocus  := s_CurrentObject
                      s_lOpenMenu  := .F.
                      s_CurrentObject:PostMessage( WM_USER + 1029 )
                      s_oCurrMenuItem := NIL
                      s_lKey := .T.
                   ENDIF
                   RETURN 0
                   
           END
           EXIT
   END

RETURN CallNextHookEx( s_hKeyMenuHook, nCode, nwParam, nlParam)


FUNCTION __GetHotItem( oMenu, nCurr, nKey )
   LOCAL n, oItem, x, nItem := 0
   x := 1
   WHILE .T.
      FOR n := x TO LEN( oMenu:Children )
          oItem := oMenu:Children[n]

          IF VALTYPE( oItem:Caption ) == "C"
             IF AT( "&"+UPPER( CHR( nKey ) ), UPPER( oItem:Caption ) ) > 0

                nItem := n
                EXIT
             ENDIF
          ENDIF
      NEXT
      IF nItem == nCurr .AND. nCurr != x
         x := nCurr+1
         LOOP
      ENDIF
      EXIT
   ENDDO
RETURN nItem

//--------------------------------------------------------------------------------------------------------------------------------
METHOD __GetObjById( nId ) CLASS ToolStripButton
   LOCAL n, oObj
   IF ::Id == nId
      RETURN Self
   ENDIF
   FOR n := 1 TO LEN( ::Children )
       IF ( oObj := ::Children[n]:__GetObjById( nId ) ) != NIL
          RETURN oObj
       ENDIF
   NEXT
RETURN NIL

//--------------------------------------------------------------------------------------------------------------------------------
FUNCTION __MenuDialogHook( nCode, nwParam, nlParam )
   LOCAL cwp_hwnd, cwp_message, cwp_wParam, cwp_lParam
   LOCAL pWndProc
   static pCallBack

   KEYMENUHOOKCWP( nlParam, @cwp_hwnd, @cwp_message, @cwp_wParam, @cwp_lParam )

   SWITCH cwp_message
      CASE WM_MENUSELECT
           s_MenuhWnd := cwp_hwnd
           EXIT
           
      CASE WM_INITMENUPOPUP
           IF s_lKey 
              IF s_oCurrMenuItem == NIL
                 PostMessage( cwp_hWnd, WM_KEYDOWN, VK_DOWN, 0)
                 //HiLiteMenuItem( cwp_hWnd, ::__hMenu, 0, MF_BYPOSITION | MF_HILITE )
              ENDIF
           ENDIF
           EXIT

      CASE 0x01E2
           WHILE GetClassName( cwp_hwnd ) == "#32768"
              IF GetProp( cwp_hwnd, "PROP_WND_PROC" ) > 0 .OR. LoWord( nlParam ) == -4352
                 EXIT
              ENDIF
              IF ( pWndProc := GetWindowLong( cwp_hwnd, GWL_WNDPROC ) ) == 0
                  EXIT
              ENDIF
              SetProp( cwp_hwnd, "PROP_WND_PROC", pWndProc )
              IF pCallBack != NIL
                 FreeCallBackPointer( pCallBack )
              ENDIF
              pCallBack := WinCallBackPointer( @__MenuDialogProc() )
              IF SetWindowLong( cwp_hwnd, GWL_WNDPROC, pCallBack ) == 0
                 RemoveProp( cwp_hwnd, "PROP_WND_PROC" )
                 EXIT
              ENDIF
              EXIT
           ENDDO
           EXIT
   END
RETURN CallNextHookEx( s_hMenuDialogHook, nCode, nwParam, nlParam)

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ToolStripComboBox INHERIT ComboBox, ToolStripItem
   PROPERTY Width         INDEX 3 READ xWidth  WRITE __SetSizePos      DEFAULT 25
   PROPERTY Height        INDEX 4 READ xHeight WRITE __SetSizePos      DEFAULT 0
   PROPERTY Alignment     READ xAlignment DEFAULT 1

   DATA __Alignments        EXPORTED INIT { "Left", "Right" }

   DATA ShortCutKey       EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD OnMouseHover()  INLINE NIL
   METHOD OnMouseLeave()  INLINE NIL
   METHOD OnMouseMove()   INLINE NIL
   METHOD OnLButtonDown() INLINE NIL
   METHOD OnLButtonUp()   INLINE NIL
ENDCLASS

METHOD Init( oParent ) CLASS ToolStripComboBox
   ::__xCtrlName   := "ToolStripComboBox"
   ::Super:Init( oParent )
   ::Width         := 100
   ::Events        := {  {"Command", { { "OnCBNSelEndOk", "", "" } } } }
RETURN Self

METHOD Create() CLASS ToolStripComboBox
   ::ToolStripItem:Create()
   ::Top := ((::Parent:Height-5) - ::SelectionHeight)/2
   ::SelectionHeight := ::SelectionHeight//::Parent:Height - 10
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ToolStripLabel INHERIT ToolStripButton
   PROPERTY Width         INDEX 3 READ xWidth  WRITE __SetSizePos      DEFAULT 80
   PROPERTY Height        INDEX 4 READ xHeight WRITE __SetSizePos      DEFAULT 0
   PROPERTY Alignment     READ xAlignment DEFAULT 1

   DATA __Alignments        EXPORTED INIT { "Left", "Right" }
   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD OnMouseHover()  INLINE NIL
   METHOD OnMouseLeave()  INLINE NIL
   METHOD OnMouseMove()   INLINE NIL
   METHOD OnLButtonDown() INLINE NIL
   METHOD OnLButtonUp()   INLINE NIL
ENDCLASS

METHOD Init( oParent ) CLASS ToolStripLabel
   ::__xCtrlName   := "ToolStripLabel"
   ::ClsName       := "ToolStripLabel"
   ::Super:Init( oParent )
   ::Width         := 100
   ::Events        := {}
RETURN Self

METHOD Create() CLASS ToolStripLabel
   LOCAL aSize := ::Drawing:GetTextExtentPoint32( ::Caption )
   ::xWidth := aSize[1]+4
   ::xHeight := aSize[2]+2
   ::ToolStripItem:Create()
RETURN Self


//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS MenuStripItem INHERIT ToolStripButton

   ACCESS ColorPressedBegin   INLINE IIF( ( ::Parent:__lIsMenu .AND. !EMPTY( ::Children ) ), ::System:CurrentScheme:MenuItemPressedGradientBegin, ::System:CurrentScheme:ButtonPressedGradientBegin )
   ACCESS ColorPressedEnd     INLINE IIF( ( ::Parent:__lIsMenu .AND. !EMPTY( ::Children ) ), ::System:CurrentScheme:MenuItemPressedGradientEnd,   ::System:CurrentScheme:ButtonPressedGradientEnd )
   
   DATA Checked      PUBLISHED INIT .F.
   DATA ShortCutText PUBLISHED
   DATA RadioCheck   PUBLISHED INIT .F.
   DATA Position     EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD __OnParentSize() INLINE NIL
   METHOD OnLButtonUp()
   METHOD OnLButtonDblClk() INLINE ::OnLButtonDown()
   METHOD OnMeasureItem()
   METHOD OnDrawItem()
   METHOD OnLButtonDown() INLINE ::__OpenMenu()
ENDCLASS


//--------------------------------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS MenuStripItem
   ::__xCtrlName   := "MenuStripItem"
   ::ClsName       := "MenuStripItem"
   ::Super:Init( oParent )
   ::Events        := {  {"General", { { "OnClick", "", "" } } } }
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS MenuStripItem
   IF ::Parent:ClsName != ::ClsName .AND. ! ::Parent:ClsName IN { "ToolStripButton", "ContextStrip" }
      Super:Create()
    ELSE
      IF ::Position != NIL
         AINS( ::Parent:Children, ::Position, Self, .T. )
       ELSE
         AADD( ::Parent:Children, Self )
      ENDIF
      ::ShortCutKey:SetAccel()
      ::Object:Create()
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------------------------------
METHOD OnDrawItem( nwParam, nlParam, dis ) CLASS MenuStripItem
   LOCAL lDisabled, lSelected, lChecked, hSepDark, n, cText := ::Caption
   LOCAL xIcon := 0, yIcon := 0, hOldPen, hOldBrush, hFont
   (nwParam)
   (nlParam)
   yIcon := 22
   
   IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
      xIcon := ::Parent:ImageList:IconWidth
      yIcon := ::Parent:ImageList:IconHeight
   ENDIF
   xIcon := 24

   lDisabled  := dis:itemState & ODS_GRAYED != 0
   lSelected  := dis:itemState & ODS_SELECTED != 0
   lChecked   := dis:itemState & ODS_CHECKED != 0

   xIcon += 8 //MAX( xIcon, GetSystemMetrics( SM_CXMENUCHECK )+2 ) + 6

   IF ::BeginGroup //Separator
      hSepDark := SelectObject( dis:hDC, ::System:CurrentScheme:Pen:SeparatorDark )
      MoveTo( dis:hDC, xIcon, dis:rcItem:top + 1 )
      LineTo( dis:hDC, dis:rcItem:right, dis:rcItem:top + 1  )
      SelectObject( dis:hDC, hSepDark )
      dis:rcItem:top += 3
   ENDIF

   IF lSelected //.AND. !lDisabled
      hOldPen   := SelectObject( dis:hDC, ::System:CurrentScheme:Pen:MenuItemBorder )
      hOldBrush := SelectObject( dis:hDC, IIF( lDisabled, ::System:CurrentScheme:Brush:ToolStripDropDownBackground, ::System:CurrentScheme:Brush:MenuItemSelected ) )
      
      Rectangle( dis:hDC, dis:rcItem:left+1, dis:rcItem:top, dis:rcItem:right-2, dis:rcItem:bottom )
      
      SelectObject( dis:hDC, hOldPen )
      SelectObject( dis:hDC, hOldBrush )
    ELSE
      
      ::__DrawStripe( dis:hDC, 0, dis:rcItem:top - IIF( ::BeginGroup, 3, 0 ), 24, dis:rcItem:bottom )
      dis:rcItem:left := xIcon - 8
      FillRect( dis:hDC, dis:rcItem, ::System:CurrentScheme:Brush:ToolStripDropDownBackground )
      dis:rcItem:left := 0
   ENDIF

   IF ::Checked
      hOldPen   := SelectObject( dis:hDC, IIF( lSelected, ::System:CurrentScheme:Pen:ButtonPressedBorder, ::System:CurrentScheme:Pen:ButtonSelectedBorder ) )
      hOldBrush := SelectObject( dis:hDC, IIF( lSelected, ::System:CurrentScheme:Brush:ButtonPressedGradientBegin, ::System:CurrentScheme:Brush:ButtonCheckedGradientEnd ) )
      Rectangle( dis:hDC, 2, dis:rcItem:Top+1, xIcon-10, dis:rcItem:Bottom-1 )
      SelectObject( dis:hDC, hOldBrush )
      SelectObject( dis:hDC, hOldPen )

      FOR n := 0 TO 1
          MoveTo( dis:hDC,  8, dis:rcItem:Top + 10 + n )
          LineTo( dis:hDC, 10, dis:rcItem:Top + 12 + n )
          LineTo( dis:hDC, 15, dis:rcItem:Top +  7 + n )
      NEXT
   ENDIF

   IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
      IF lDisabled
         SelectObject( dis:hDC, GetSysColorBrush( COLOR_GRAYTEXT ) )
         ::Parent:ImageList:DrawDisabled( dis:hDC, ::ImageIndex, 5, dis:rcItem:top + 3, ILD_TRANSPARENT )
       ELSE
         ::Parent:ImageList:DrawImage( dis:hDC, ::ImageIndex, 5, dis:rcItem:top + 3, ILD_TRANSPARENT )
      ENDIF
   ENDIF
   
   IF lDisabled
      SetTextColor( dis:hDC, GetSysColor( COLOR_GRAYTEXT ) )
   ENDIF

   SetBkMode( dis:hDC, TRANSPARENT )

   IF ::__ClassInst != NIL .AND. !::GenerateMember .AND. cText == "[ Add New Item ]"
      hOldBrush := SelectObject( dis:hDC, ::System:CurrentScheme:Brush:ButtonPressedGradientBegin )
      Rectangle( dis:hDC, dis:rcItem:Left+24, dis:rcItem:Top+2, dis:rcItem:Right-4, dis:rcItem:Bottom-2 )
      SelectObject( dis:hDC, hOldBrush )
      cText := "Add New Item"
   ENDIF
   
   dis:rcItem:Left += xIcon

   hFont := SelectObject( dis:hDC, ::Font:handle )
   _DrawText( dis:hDC, cText, dis:rcItem:Array, DT_SINGLELINE | DT_VCENTER )

   IF !EMPTY( ::ShortCutText )
      dis:rcItem:right -= 20
      _DrawText( dis:hDC, ::ShortCutText, dis:rcItem:Array, DT_SINGLELINE | DT_VCENTER | DT_RIGHT )
   ENDIF
   SelectObject( dis:hDC, hFont )
RETURN 0

//--------------------------------------------------------------------------------------------------------------------------------
METHOD OnMeasureItem( nwParam, nlParam, mi ) CLASS MenuStripItem
   LOCAL hOld, aRect, hDC, aExt, cText
   LOCAL xIcon := 0
   LOCAL yIcon := 0
   (nwParam)
   (nlParam)
   IF mi:CtlType == ODT_MENU
   
      hDC   := GetDC( NIL )
      aRect := {0,0,0,0}

      IF ::Font:Handle != NIL
         hOld := SelectObject( hDC, ::Font:Handle )
       ELSE
         hOld := SelectObject( hDC, ::Form:Font:Handle )
      ENDIF
      IF ::Caption != NIL
         cText := ::Caption + IIF( !EMPTY( ::ShortCutText ), SPACE(3)+::ShortCutText,"")
         aExt := _GetTextExtentPoint32( hDC, cText )
         _DrawText( hDC, cText, @aRect, DT_SINGLELINE + DT_CALCRECT )
         aRect[3] := aExt[1]+20
         aRect[4] := aExt[2]+((aExt[2]*10)/100)
      ENDIF
      SelectObject( hDC, hOld )

      ReleaseDC( NIL, hDC )

      IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
         xIcon := ::Parent:ImageList:IconWidth
         yIcon := ::Parent:ImageList:IconHeight
      ENDIF

      xIcon := 24
      yIcon := 22

      aRect[4] := MAX( aRect[4], yIcon )

      mi:itemWidth  := xIcon + 8 + aRect[3]-IIF( ::Caption != NIL, GetSystemMetrics( SM_CXMENUCHECK ), 0 )//+6
      mi:itemHeight := MAX( aRect[4], GetSystemMetrics( SM_CYMENU ) )
      
      IF ::BeginGroup //Separator
         mi:itemHeight += 3
      ENDIF

      mi:CopyTo( nlParam )
   ENDIF
RETURN 0

//--------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonUp( nwParam, x, y ) CLASS MenuStripItem
   ::ReleaseCapture()
   IF EMPTY( ::Children )
      RETURN ::Super:OnLButtonUp( nwParam, x, y )
   ENDIF
RETURN Self


FUNCTION RestoreShadow()
   LOCAL hDC, n
   IF s_aPixels != NIL
      hDC := CreateDC( "DISPLAY" )
      FOR n := 1 TO LEN( s_aPixels )
          SetPixel( hDC, s_aRect[1]+s_aPixels[n][1], s_aRect[2]+s_aPixels[n][2], s_aPixels[n][3] )
      NEXT
      DeleteDC( hDC )
   ENDIF
   s_aPixels := NIL
RETURN NIL

CLASS __MenuStripItemShortCut
   PROPERTY Shift READ xShift WRITE __SetShortcut DEFAULT .F.
   PROPERTY Ctrl  READ xCtrl  WRITE __SetShortcut DEFAULT .F.
   PROPERTY Alt   READ xAlt   WRITE __SetShortcut DEFAULT .F.
   PROPERTY Key   READ xKey   WRITE __SetShortcut DEFAULT 0
   
   DATA ClsName     EXPORTED  INIT ""
   DATA Parent      EXPORTED
   DATA __ClassInst EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD GetShortcutText()
   METHOD __SetShortcut INLINE ::GetShortcutText()
   METHOD SetAccel()
ENDCLASS

METHOD Init( oParent ) CLASS __MenuStripItemShortCut
   ::Parent := oParent
   IF ::Parent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
   ENDIF
RETURN Self

METHOD SetAccel() CLASS __MenuStripItemShortCut
   LOCAL nAccel
   IF ::__ClassInst == NIL .AND. ::xKey <> 0
      nAccel := FVIRTKEY
      IF ::xCtrl
         nAccel := nAccel | FCONTROL
      ENDIF
      IF ::xShift
         nAccel := nAccel | FSHIFT
      ENDIF
      IF ::xAlt
         nAccel := nAccel | FALT
      ENDIF
      ::Parent:Form:AddAccelerator( nAccel, ::Key, ::Parent:Id )
   ENDIF
RETURN Self

METHOD GetShortcutText( lCtrl, lAlt, lShift, nKey ) CLASS __MenuStripItemShortCut
   LOCAL oItem, cText, cAccel := ""
   DEFAULT lCtrl  TO ::Ctrl
   DEFAULT lAlt   TO ::Alt
   DEFAULT lShift TO ::Shift
   DEFAULT nKey   TO ::Key
   IF lCtrl
      cAccel += "Ctrl"
   ENDIF
   IF lAlt
      IF !EMPTY( cAccel )
         cAccel += "+"
      ENDIF
      cAccel += "Alt"
   ENDIF
   IF lShift
      IF !EMPTY( cAccel )
         cAccel += "+"
      ENDIF
      cAccel += "Shift"
   ENDIF
   IF nKey != 0
      IF !EMPTY( cAccel )
         cAccel += "+"
      ENDIF
      GetRealKeyName( nKey, @cText, 40)
      cAccel += cText
   ENDIF
   IF !lAlt .AND. !lCtrl .AND. !lShift .AND. nKey == VK_ESCAPE
      cAccel := ""
      ::xKey := 0
   ENDIF
   IF __GetApplication() != NIL .AND. ::Parent:__ClassInst != NIL
      WITH OBJECT __GetApplication():ObjectManager
         IF ( oItem := FindTreeItem( :Items, TVGetSelected( :hWnd ) ) ) != NIL
            oItem:Owner:ColItems[1]:Value := cAccel
            :InvalidateRect(, .F. )
         ENDIF
      END
   ENDIF
RETURN cAccel

FUNCTION __GradientFill( hDC, aVertex, nVertex, aMesh, nCount, ulMode )
   LOCAL pRect, pVertex, n,__aVertex := {}, __aMesh := {}
   FOR n := 1 TO LEN( aVertex )
       pVertex := ARRAY(5)
       pVertex[1] := aVertex[n]:x
       pVertex[2] := aVertex[n]:y
       pVertex[3] := aVertex[n]:Red
       pVertex[4] := aVertex[n]:Green
       pVertex[5] := aVertex[n]:Blue
       AADD( __aVertex, pVertex )
   NEXT
   FOR n := 1 TO LEN( aMesh )
       pRect := Array(2)
       pRect[1] := aMesh[n]:UpperLeft 
       pRect[2] := aMesh[n]:LowerRight
       AADD( __aMesh, pRect )
   NEXT
   xGradientFill( hDC, __aVertex, nVertex, __aMesh, nCount, ulMode )
   aVertex := NIL
   aMesh   := NIL
RETURN NIL

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ContextStrip INHERIT Component
   DATA __hMenu   EXPORTED
   DATA Left, Top, Width EXPORTED INIT 0
   DATA ImageList PUBLISHED
   METHOD Init() CONSTRUCTOR
   METHOD Show()
   METHOD Create()
   METHOD __DrawShadow() INLINE NIL
   METHOD Destroy() INLINE __ReleaseMenu( Self, ::__hMenu ), DestroyMenu( ::__hMenu )
ENDCLASS

METHOD Init( oParent ) CLASS ContextStrip
   DEFAULT ::__xCtrlName   TO "ContextStrip"
   DEFAULT ::ComponentType TO "ContextMenu"
   DEFAULT ::ClsName       TO "ContextStrip"
   Super:Init( oParent )
   ::Parent := oParent
RETURN Self

METHOD Create() CLASS ContextStrip
   LOCAL lpMenuInfo := (struct MENUINFO)
   Super:Create()
   ::__hMenu := CreatePopupMenu()
   lpMenuInfo:cbSize := lpMenuInfo:SizeOf()
   lpMenuInfo:fMask  := MIM_STYLE
   lpMenuInfo:dwStyle:= MNS_NOTIFYBYPOS
   SetMenuInfo( ::__hMenu, lpMenuInfo )
   ::lCreated := .T.
   IF ::__ClassInst != NIL
      WITH OBJECT MenuStripItem()
         :GenerateMember := .F.
         :Caption     := "[ Add New Item ]"
         :Init( Self )
         :Events      := {}
         :Font:Bold   := .T.
         :Create()
      END
   ENDIF
   ::hWnd := ::Form:hWnd
RETURN Self

METHOD Show( x, y ) CLASS ContextStrip
   LOCAL nStyle, nRes := 0, rc, pt := (struct POINT)
   LOCAL lpMenuInfo := (struct MENUINFO)

   __ReleaseMenu( Self, ::__hMenu )
   
   nStyle := TPM_LEFTALIGN | TPM_TOPALIGN
   IF ::__ClassInst != NIL
      GetWindowRect( ::Application:MainForm:FormEditor1:hWnd, @rc )
      x := ( rc:left + rc:right ) / 2
      y := ( rc:top + rc:bottom ) / 2
      nStyle := TPM_CENTERALIGN | TPM_LEFTBUTTON
   ENDIF

   __SetSubMenu( Self, ::__hMenu )

   s_CurrFocus     := NIL
   s_CurrentObject := Self

   s_hMenuDialogHook := SetWindowsHookEx( WH_CALLWNDPROC, ( @__MenuDialogHook() ), NIL, GetCurrentThreadId() )
   DEFAULT s_hKeyMenuHook TO SetWindowsHookEx( WH_MSGFILTER, ( @__KeyMenuHook() ), NIL, GetCurrentThreadId() )

   TrackPopupMenu( ::__hMenu, nStyle, X, Y, 0, ::Form:hWnd )
   s_nmw := 0

   UnhookWindowsHookEx( s_hMenuDialogHook )
   s_hMenuDialogHook := NIL
   
   IF s_CurrFocus == NIL .AND. s_hKeyMenuHook != NIL
      UnhookWindowsHookEx( s_hKeyMenuHook )
      s_hKeyMenuHook    := NIL
   ENDIF

RETURN nRes

FUNCTION __AddNewMenuItem( Self, oParent )
   LOCAL oItem, mii, cText

   oItem := MenuStripItem( oParent )
   oItem:Position := LEN( oParent:Children )
   oItem:Caption := oItem:Name
   oItem:Create()

   oItem:__hMenu := CreateMenu()

   mii := {=>}
   mii:fMask         := MIIM_DATA | MIIM_ID | MIIM_STATE | MIIM_TYPE | MIIM_SUBMENU
   mii:hSubMenu      := oItem:__hMenu
   mii:wID           := oItem:Id
   mii:fType         := MFT_OWNERDRAW
   mii:fState        := IIF( oItem:Enabled, MFS_ENABLED, MFS_DISABLED )
   mii:dwItemData    := oItem:__pObjPtr := ArrayPointer( oItem )

   __InsertMenuStripItem( oParent:__hMenu, oItem:Position-1, .T., mii:fMask, mii:hSubMenu, mii:wID, oItem:Caption, mii:dwItemData, mii:fState )

   WITH OBJECT MenuStripItem()
      :Caption   := "[ Add New Item ]"
      :GenerateMember := .F.
      :Init( oItem )
      :Font:Bold := .T.
      :Events    := {}
      :Create()

      mii := {=>}
      mii:fMask         := MIIM_DATA | MIIM_ID | MIIM_STATE | MIIM_TYPE
      mii:wID           := :Id
      mii:fState        := MFS_ENABLED

      __InsertMenuStripItem( :__hMenu, -1, .T., mii:fMask, NIL, mii:wID, :Caption, NIL, mii:fState )
   END
   
   ::Application:Project:Modified := .T.
   ::Form:__lModified := .T.

   cText := oItem:ClassName
   WITH OBJECT ::Application:Props[ "ComboSelect" ]
      :AddItem( oItem:Name + CHR(9) + Upper( cText[1] ) + SubStr( Lower( cText ), 2 ) )
      AADD( :aItems, oItem )
   END
RETURN NIL
