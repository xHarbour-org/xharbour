/*
 * $Id$
 */
#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#ifdef VXH_PROFESSIONAL

#include "vxh.ch"
#include "colors.ch"
#include "debug.ch"

/* Color rendering properties */
#define TMT_BORDERCOLOR             3801
#define TMT_FILLCOLOR               3802
#define TMT_TEXTCOLOR               3803
#define TMT_EDGELIGHTCOLOR          3804
#define TMT_EDGEHIGHLIGHTCOLOR      3805
#define TMT_EDGESHADOWCOLOR         3806
#define TMT_EDGEDKSHADOWCOLOR       3807
#define TMT_EDGEFILLCOLOR           3808
#define TMT_TRANSPARENTCOLOR        3809
#define TMT_GRADIENTCOLOR1          3810
#define TMT_GRADIENTCOLOR2          3811
#define TMT_GRADIENTCOLOR3          3812
#define TMT_GRADIENTCOLOR4          3813
#define TMT_GRADIENTCOLOR5          3814
#define TMT_SHADOWCOLOR             3815
#define TMT_GLOWCOLOR               3816
#define TMT_TEXTBORDERCOLOR         3817
#define TMT_TEXTSHADOWCOLOR         3818
#define TMT_GLYPHTEXTCOLOR          3819
#define TMT_GLYPHTRANSPARENTCOLOR   3820
#define TMT_FILLCOLORHINT           3821
#define TMT_BORDERCOLORHINT         3822
#define TMT_ACCENTCOLORHINT         3823

#define TMT_DEFAULTPANESIZE         5002

/* Int rendering properties */
#define TMT_IMAGECOUNT          2401
#define TMT_ALPHALEVEL          2402
#define TMT_BORDERSIZE          2403
#define TMT_ROUNDCORNERWIDTH    2404
#define TMT_ROUNDCORNERHEIGHT   2405
#define TMT_GRADIENTRATIO1      2406
#define TMT_GRADIENTRATIO2      2407
#define TMT_GRADIENTRATIO3      2408
#define TMT_GRADIENTRATIO4      2409
#define TMT_GRADIENTRATIO5      2410
#define TMT_PROGRESSCHUNKSIZE   2411
#define TMT_PROGRESSSPACESIZE   2412
#define TMT_SATURATION          2413
#define TMT_TEXTBORDERSIZE      2414
#define TMT_ALPHATHRESHOLD      2415
#define TMT_WIDTH               2416
#define TMT_HEIGHT              2417
#define TMT_GLYPHINDEX          2418
#define TMT_TRUESIZESTRETCHMARK 2419
#define TMT_MINDPI1             2420
#define TMT_MINDPI2             2421
#define TMT_MINDPI3             2422
#define TMT_MINDPI4             2423
#define TMT_MINDPI5             2424

#define DG_ADDCONTROL             1

/* EXPLORERBAR parts */
#define EBP_HEADERBACKGROUND       1
#define EBP_HEADERCLOSE            2
#define EBP_HEADERPIN              3
#define EBP_IEBARMENU              4
#define EBP_NORMALGROUPBACKGROUND  5
#define EBP_NORMALGROUPCOLLAPSE    6
#define EBP_NORMALGROUPEXPAND      7
#define EBP_NORMALGROUPHEAD        8
#define EBP_SPECIALGROUPBACKGROUND 9
#define EBP_SPECIALGROUPCOLLAPSE   10
#define EBP_SPECIALGROUPEXPAND     11
#define EBP_SPECIALGROUPHEAD       12

/* EXPLORERBAR HEADERCLOSE states */
#define EBHC_NORMAL  1
#define EBHC_HOT     2
#define EBHC_PRESSED 3

/* EXPLORERBAR HEADERPIN states */
#define EBHP_NORMAL          1
#define EBHP_HOT             2
#define EBHP_PRESSED         3
#define EBHP_SELECTEDNORMAL  4
#define EBHP_SELECTEDHOT     5
#define EBHP_SELECTEDPRESSED 6

/* EXPLORERBAR IEBARMENU states */
#define EBM_NORMAL  1
#define EBM_HOT     2
#define EBM_PRESSED 3

/* EXPLORERBAR NORMALGROUPCOLLAPSE states */
#define EBNGC_NORMAL  1
#define EBNGC_HOT     2
#define EBNGC_PRESSED 3

/* EXPLORERBAR NORMALGROUPEXPAND states */
#define EBNGE_NORMAL  1
#define EBNGE_HOT     2
#define EBNGE_PRESSED 3

/* EXPLORERBAR SPECIALGROUPCOLLAPSE states */
#define EBSGC_NORMAL  1
#define EBSGC_HOT     2
#define EBSGC_PRESSED 3

/* EXPLORERBAR SPECIALGROUPEXPAND states */
#define EBSGE_NORMAL  1
#define EBSGE_HOT     2
#define EBSGE_PRESSED 3

#define TMT_RECT 209
//-----------------------------------------------------------------------------------------------

CLASS ExplorerBar INHERIT Control
   DATA BackColor          EXPORTED
   DATA ForeColor          EXPORTED
   
   DATA __nHeight          EXPORTED INIT 0
   DATA __hImageListTitle  EXPORTED
   DATA __hImageListButton EXPORTED

   PROPERTY ImageList  GET __ChkComponent( Self, ::xImageList )

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnPaint()
   METHOD OnSize(n,x,y)    INLINE Super:OnSize(n,x,y), ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT ), NIL
   METHOD OnVertScroll()   INLINE ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT ), AEVAL( ::Children, {|o| o:Redraw() } ), NIL
   METHOD OnEraseBkGnd()   INLINE 1
   METHOD OnThemeChanged()
   METHOD OnDestroy()      INLINE ImageListDestroy( ::__hImageListTitle ), ImageListDestroy( ::__hImageListButton ), ::CloseThemeData(), NIL
   METHOD __AddTaskPanel()
ENDCLASS

//---------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ExplorerBar
   LOCAL n, aRect := ARRAY(4)
   ::__xCtrlName   := "ExplorerBar"
   ::ClassBrush    := GetStockObject( WHITE_BRUSH )
   ::Super:Init( oParent )
   ::ClsName       := "ExplorerBar"
   ::__IsStandard  := .F.
   ::__IsControl   := .T.
   ::IsContainer   := .T.
   ::CaptionHeight := 0
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ExStyle       := WS_EX_CONTROLPARENT
   ::VertScroll    := .T.
   ::HorzScroll    := .F.
   ::Left          := 0
   ::Top           := 0
   ::Height        := 400
   ::__lResizeable := {.F.,.F.,.F.,.T.,.F.,.F.,.F.,.T.}
   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "&Add Expando", {|| ::__AddTaskPanel()} } }
   ENDIF
   ::ThemeName := "EXPLORERBAR"
RETURN Self

METHOD __AddTaskPanel() CLASS ExplorerBar
   ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., Self, "Expando",,,1, {}, } }, ::Application:Project:aUndo )
RETURN( Self )

//---------------------------------------------------------------------------------------------------

METHOD Create() CLASS ExplorerBar
   ::OnThemeChanged( .F. )
   ::xWidth := ::System:ExplorerBar:headernormal:iHeaderBmpWidth + ::System:ExplorerBar:headernormal:rcTLPadding:left + ::System:ExplorerBar:headernormal:rcTLPadding:right
   ::MinWidth := ::xWidth
   Super:Create()
   ::OpenThemeData()
RETURN Self

METHOD OnThemeChanged( lClean ) CLASS ExplorerBar
   DEFAULT lClean TO .T.
   IF lClean .AND. ::__hImageListTitle != NIL
      ImageListDestroy( ::__hImageListTitle )
   ENDIF
   IF ::Application:IsThemedXP
      ::__hImageListTitle := ImageListCreate( ::System:ExplorerBar:headernormal:iHeaderBmpWidth, ::System:ExplorerBar:headernormal:iHeaderBmpHeight, ILC_COLORDDB | ILC_MASK, 1, 0 )
      IF ::System:ExplorerBar:headernormal:hAlphaBmpListHeader != 0
         ImageListAddMasked( ::__hImageListTitle, ::System:ExplorerBar:headernormal:hAlphaBmpListHeader, C_BLACK )
         ImageListAddMasked( ::__hImageListTitle, ::System:ExplorerBar:headerspecial:hAlphaBmpListHeader, C_BLACK )
      ENDIF

      ::__hImageListButton := ImageListCreate( ::System:ExplorerBar:headernormal:iBmpArrowWidth, ::System:ExplorerBar:headernormal:iBmpArrowHeight, ILC_COLORDDB | ILC_MASK, 1, 0 )
      IF ::System:ExplorerBar:headernormal:hAlphaBmpArrowUp[1] != 0
         ImageListAddMasked( ::__hImageListButton, ::System:ExplorerBar:headernormal:hAlphaBmpArrowUp[1], C_BLACK )
         ImageListAddMasked( ::__hImageListButton, ::System:ExplorerBar:headernormal:hAlphaBmpArrowUp[2], C_BLACK )
         ImageListAddMasked( ::__hImageListButton, ::System:ExplorerBar:headernormal:hAlphaBmpArrowDn[1], C_BLACK )
         ImageListAddMasked( ::__hImageListButton, ::System:ExplorerBar:headernormal:hAlphaBmpArrowDn[2], C_BLACK )

         ImageListAddMasked( ::__hImageListButton, ::System:ExplorerBar:headerspecial:hAlphaBmpArrowUp[1], C_BLACK )
         ImageListAddMasked( ::__hImageListButton, ::System:ExplorerBar:headerspecial:hAlphaBmpArrowUp[2], C_BLACK )
         ImageListAddMasked( ::__hImageListButton, ::System:ExplorerBar:headerspecial:hAlphaBmpArrowDn[1], C_BLACK )
         ImageListAddMasked( ::__hImageListButton, ::System:ExplorerBar:headerspecial:hAlphaBmpArrowDn[2], C_BLACK )
      ENDIF
   ENDIF
   IF lClean
      ::InvalidateRect()
   ENDIF
RETURN Self

METHOD OnPaint( hDC, hMemDC ) CLASS ExplorerBar
   LOCAL hMemBitmap, hOldBitmap, rc := (struct RECT)
   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ENDIF

   rc:left   := 0
   rc:top    := 0
   rc:right  := ::Width
   rc:bottom := ::Height

   IF ::Application:IsThemedXP
      FillGradient( hMemDC, rc, ::System:ExplorerBar:taskpane:crBackground1, ::System:ExplorerBar:taskpane:crBackground2, .F. )
    ELSE
      FillRect( hMemDC, rc, GetSysColorBrush( COLOR_WINDOW ) )
   ENDIF
   IF hDC != NIL
      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF
RETURN 0


//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------

CLASS Expando INHERIT Button
   DATA Caption               PUBLISHED INIT ""
   DATA Special               PUBLISHED INIT .F.

   PROPERTY ImageList  GET __ChkComponent( Self, ::xImageList )

   DATA HeaderHeight          EXPORTED
   DATA PaneHeight            PUBLISHED INIT 0

   DATA __MouseIn             PROTECTED INIT .F.
   DATA __PrevHeight          PROTECTED INIT 0
   DATA Dock                  EXPORTED
   DATA Anchor                EXPORTED
   DATA __Position            PROTECTED

   DATA xLeft                 EXPORTED
   ACCESS Left                INLINE ::xLeft
   ASSIGN Left(n)             INLINE ::xLeft := n

   DATA xTop                  EXPORTED
   ACCESS Top                 INLINE ::xTop
   ASSIGN Top(n)              INLINE ::xTop := n

   DATA xWidth                EXPORTED
   ACCESS Width               INLINE ::xWidth
   ASSIGN Width(n)            INLINE ::xWidth := n

   DATA __nHeight             EXPORTED
   DATA __lSave               PROTECTED INIT .T.
   DATA BackColor             EXPORTED
   DATA ForeColor             EXPORTED

   PROPERTY Expanded READ xExpanded WRITE SetExpand PROTECTED DEFAULT .T.

   DATA Visible               EXPORTED INIT .F.

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD __OnParentSize()
   METHOD OnEraseBkGnd()   INLINE 1
   METHOD OnPaint()
   METHOD OnParentDrawItem()
   METHOD OnMouseMove()
   METHOD OnMouseLeave()
   METHOD Expand()
   METHOD OnSize()
   METHOD GetRectangle() INLINE { ::Left,;
                                  ::Top  + ::Parent:VertScrollPos,;
                                  ::Left + ::Width,;
                                  ::Top  + ::Height + ::Parent:VertScrollPos}
   METHOD __SetSizePos()
   METHOD OnThemeChanged()
   METHOD SetExpand( lExpand )  INLINE ::Expand( lExpand, .T. )
   METHOD IsWindowEnabled()     INLINE IsWindowEnabled( ::hWnd ) .AND. IsWindowEnabled( ::Parent:hWnd )
ENDCLASS

METHOD Init( oParent ) CLASS Expando
   ::__xCtrlName := "Expando"
   Super:Init( oParent )
   ::IsContainer   := .T.
   ::__lResizeable := {.F.,.F.,.F.,.T.,.F.,.F.,.F.,.F.}
   ::__lMoveable   := .F.
   ::xHeight       := 60
   ::Action        := {|o| o:Expand() }
   ::__Position    := LEN( ::Parent:Children ) + 1
RETURN Self

METHOD Create() CLASS Expando
   LOCAL oHeader, oTask, n := 0

   IF ::Special
      oHeader := ::System:ExplorerBar:headerspecial
      oTask   := ::System:ExplorerBar:taskspecial
    ELSE
      oHeader := ::System:ExplorerBar:headernormal
      oTask   := ::System:ExplorerBar:tasknormal
   ENDIF
   ::HeaderHeight := oHeader:iHeaderBmpHeight + oHeader:rcBorderThicknessHeader:top + oHeader:rcBorderThicknessHeader:bottom
   ::BackColor := oHeader:crTLbackground

   IF LEN( ::Parent:Children ) > 0
      n := ATAIL( ::Parent:Children ):Top + ATAIL( ::Parent:Children ):Height
      ::Dock:Top := ATAIL( ::Parent:Children )
   ENDIF

   ::xLeft   := ::System:ExplorerBar:headernormal:rcTLPadding:left
   ::xTop    := ::System:ExplorerBar:headernormal:rcTLPadding:top + n
   ::xWidth  := ::System:ExplorerBar:headernormal:iHeaderBmpWidth
   ::Style   := WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_OWNERDRAW | WS_CLIPCHILDREN | WS_GROUP
   ::ExStyle := WS_EX_CONTROLPARENT
   Super:Create()
   ::Font:Height    := oHeader:iFontSize
   ::Font:Weight    := oHeader:iFontWeightHeader
   ::Font:FaceName  := oHeader:szFontFace:AsString()
   ::PaneHeight     := ::Height
   ::SetExpand( ::Expanded )

   ::Parent:OriginalRect[4] := ( ::Top + ::Height + ::System:ExplorerBar:headernormal:rcTLPadding:top )
   //IF ::__ClassInst == NIL //::Parent:OriginalRect[4] > ::Parent:Height
      ::Parent:__SetScrollBars()
   //ENDIF
RETURN Self

METHOD OnThemeChanged() CLASS Expando
   LOCAL oHeader, oTask

   IF ::Special
      oHeader := ::System:ExplorerBar:headerspecial
      oTask   := ::System:ExplorerBar:taskspecial
    ELSE
      oHeader := ::System:ExplorerBar:headernormal
      oTask   := ::System:ExplorerBar:tasknormal
   ENDIF
   ::HeaderHeight := oHeader:iHeaderBmpHeight + oHeader:rcBorderThicknessHeader:top + oHeader:rcBorderThicknessHeader:bottom
   ::InvalidateRect()
RETURN Self

METHOD Expand( lExpand, lForce ) CLASS Expando
   LOCAL n := ::__Position + 1
   DEFAULT lExpand TO !::xExpanded
   DEFAULT lForce  TO .F.
   IF ::__MouseIn .OR. lForce
      IF ::hWnd != NIL
         IF !lExpand
            ::PaneHeight := ::Height
            IF !lForce
               ::Parent:OriginalRect[4] -= ( ::Height - ::HeaderHeight )
            ENDIF
            IF ::__ClassInst != NIL
               ::__nHeight := ::Height
               ::__lSave := .F.
            ENDIF
            ::Height := IIF( ::Parent:ImageList != NIL .AND. ::ImageIndex > 0 .AND. ::Parent:ImageList:IconHeight > ::HeaderHeight, ::Parent:ImageList:IconHeight, ::HeaderHeight )
          ELSE
            IF !lForce
               ::Parent:OriginalRect[4] += ( ::PaneHeight - ::HeaderHeight )
            ENDIF
            ::Height := ::PaneHeight
         ENDIF
         ::UpdateWindow()
         ::Parent:__SetScrollBars()
         ::InvalidateRect()
      ENDIF
      ::xExpanded := lExpand
      AEVAL( ::Children, {|o| o:Visible := lExpand } )
      ::__lResizeable := {.F.,.F.,.F., lExpand,.F.,.F.,.F.,.F.}
   ENDIF
RETURN 0

METHOD OnSize( x, y ) CLASS Expando
   LOCAL n
   ::InvalidateRect()
   IF ::__ClassInst != NIL .AND. ::__lSave
      ::__nHeight := ::Height
   ENDIF
   ::__lSave := .T.
   FOR n := ::__Position + 1 TO LEN( ::Parent:Children )
       ::Parent:Children[n]:xTop := ::Parent:Children[n-1]:xTop + ::Parent:Children[n-1]:xHeight + ::System:ExplorerBar:headernormal:rcTLPadding:top
       ::Parent:Children[n]:MoveWindow()
       ::Parent:Children[n]:Redraw()
   NEXT
RETURN 0

METHOD __OnParentSize( x, y, hDef ) CLASS Expando
   ::xWidth := ::System:ExplorerBar:headernormal:iHeaderBmpWidth
   IF ::__ClassInst == NIL .AND. ::Parent:Height < ::Parent:OriginalRect[4] .AND. ( x - GetSystemMetrics( SM_CXVSCROLL ) ) < ::System:ExplorerBar:headernormal:iHeaderBmpWidth + ::System:ExplorerBar:headernormal:rcTLPadding:right + ::System:ExplorerBar:headernormal:rcTLPadding:left
      ::xWidth := ::System:ExplorerBar:headernormal:iHeaderBmpWidth - GetSystemMetrics( SM_CXVSCROLL )
   ENDIF
   ::MoveWindow()
RETURN 0

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS Expando
   LOCAL lDisabled, lSelected, lFocus
   lDisabled := dis:itemState & ODS_DISABLED != 0
   lSelected := dis:itemState & ODS_SELECTED != 0
   lFocus    := dis:itemState & ODS_FOCUS != 0
RETURN 0

METHOD OnMouseMove( nwParam, x, y ) CLASS Expando
   LOCAL nHeight := IIF( ::Parent:ImageList != NIL .AND. ::ImageIndex > 0 .AND. ::Parent:ImageList:IconHeight > ::HeaderHeight, ::Parent:ImageList:IconHeight, ::HeaderHeight )
   IF !::__MouseIn .AND. y <= nHeight
      ::Cursor := ::System:Cursor:LinkSelect
      ::__MouseIn := .T.
      ::InvalidateRect()
    ELSEIF y > nHeight .AND. ::__MouseIn
      ::Cursor := NIL
      ::__MouseIn := .F.
      ::InvalidateRect()
   ENDIF
RETURN NIL

METHOD OnMouseLeave( nwParam, x, y ) CLASS Expando
   IF ::__MouseIn
      ::Cursor := NIL
      ::__MouseIn := .F.
      ::InvalidateRect()
   ENDIF
RETURN NIL

METHOD OnPaint( hDC, hMemDC ) CLASS Expando
   LOCAL x := 0, y := 0, cx, cy, nHeight := ::HeaderHeight
   LOCAL aArrow, nArrow, hPen, hPen1, hBrush1, oHeader, aSize, hTitle, hMemBitmap, hOldBitmap, hBrush, nImage, hButton, nButton, nMargin, nState, hFont, oTask, nPart, nIconX, nIconY
   LOCAL oChild, hMemBitmap1, hOldBitmap1, hMemDC1, hOldPen, hOldBrush, nLeft, nTop, nRight, nBottom
   IF !::IsWindow()
      RETURN 0
   ENDIF
   IF ::Special
      oTask   := ::System:ExplorerBar:taskspecial
      oHeader := ::System:ExplorerBar:headerspecial
      nImage  := 1
      nButton := IIF( ::__MouseIn, 7, 6 )
    ELSE
      oTask   := ::System:ExplorerBar:tasknormal
      oHeader := ::System:ExplorerBar:headernormal
      nImage  := 0
      nButton := IIF( ::__MouseIn, 3, 2 )
   ENDIF

   nMargin := oHeader:rcBorderThicknessHeader:top + oHeader:rcBorderThicknessHeader:bottom

   IF ::Expanded
      nButton -= 2
   ENDIF

   cx := ::Left + ::Width
   cy := ::Top + ::Height

   IF hMemDC == NIL
      x := ::Left
      y := ::Top

      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::Parent:ClientWidth, ::Parent:ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
      SendMessage( ::Parent:hWnd, WM_PRINT, hMemDC, PRF_CLIENT | PRF_ERASEBKGND )
   ENDIF

   nIconX := 0
   nIconY := 0
   IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
      nIconX := ::Parent:ImageList:IconWidth
      IF ::Parent:ImageList:IconHeight > ::HeaderHeight
         nIconY := ::Parent:ImageList:IconHeight - nHeight
         y += nIconY
      ENDIF
   ENDIF

   hFont := SelectObject( hMemDC, ::Font:Handle )

   IF ::Application:IsThemedXP .AND. ::System:ExplorerBar:headernormal:hAlphaBmpListHeader != 0
      hPen   := CreatePen( PS_SOLID, 1, oHeader:crTLBorder )
      hBrush := CreateSolidBrush( oHeader:crTLbackground )

      hOldPen   := SelectObject( hMemDC, hPen )
      hOldBrush := SelectObject( hMemDC, hBrush )
      Rectangle( hMemDC, x, y + oHeader:iHeaderBmpHeight, cx, cy )

      hTitle := ImageListGetIcon( ::Parent:__hImageListTitle, nImage, ILD_TRANSPARENT )
      DrawIconEx( hMemDC, x, y, hTitle, ::Width, nHeight, 0, NIL,  DI_NORMAL )
      DestroyIcon( hTitle )

      hButton := ImageListGetIcon( ::Parent:__hImageListButton, nButton, ILD_TRANSPARENT )
      DrawIconEx( hMemDC, ::Width-(oHeader:iBmpArrowWidth/2), y+(nMargin), hButton, oHeader:iBmpArrowWidth, oHeader:iBmpArrowHeight, 0, NIL,  DI_NORMAL )
      DestroyIcon( hButton )

    ELSE

      hPen   := CreatePen( PS_SOLID, 1, oHeader:crTLBorder )
      hBrush := CreateSolidBrush( oHeader:crTLbackground )

      hOldPen   := SelectObject( hMemDC, hPen )
      hOldBrush := SelectObject( hMemDC, GetSysColorBrush( COLOR_WINDOW ) )

      Rectangle( hMemDC, x, y+nHeight-1, cx, cy )

      _FillRect( hMemDC, {x, y, cx, y + nHeight}, GetSysColorBrush( IIF( ::Special, COLOR_HIGHLIGHT, COLOR_BTNFACE ) ) )

      nLeft   := ::Width-10
      nTop    := y + ( nHeight / 2 )
      nRight  := nLeft + 7
      nBottom := nTop + 4

      hPen1   := CreatePen( PS_SOLID, 1, IIF( ::__MouseIn, oHeader:crHeaderHot, oHeader:crHeaderNormal ) )
      hOldPen := SelectObject( hMemDC, hPen1 )

      IF ::Expanded
         aArrow := { nLeft, nBottom-1,;
                     nLeft + 3, nTop,;
                     nRight, nBottom,;
                     nLeft + 1, nBottom-1,;
                     nLeft + 3, nTop+1,;
                     nRight-1, nBottom }
       ELSE
         aArrow := { nLeft, nTop+1,;
                     nLeft + 3, nBottom,;
                     nRight, nTop,;
                     nLeft + 1, nTop+1,;
                     nLeft + 3, nBottom-1,;
                     nRight-1, nTop }
      ENDIF
      FOR nArrow := 0 TO 1
          MoveTo( hMemDC, aArrow[ 1], aArrow[ 2]-(nArrow*4) )
          LineTo( hMemDC, aArrow[ 3], aArrow[ 4]-(nArrow*4) )
          LineTo( hMemDC, aArrow[ 5], aArrow[ 6]-(nArrow*4) )

          MoveTo( hMemDC, aArrow[ 7], aArrow[ 8]-(nArrow*4) )
          LineTo( hMemDC, aArrow[ 9], aArrow[10]-(nArrow*4) )
          LineTo( hMemDC, aArrow[11], aArrow[12]-(nArrow*4) )
      NEXT
      SelectObject( hMemDC, hOldPen )
      DeleteObject( hPen1 )

   ENDIF
   SetBkMode( hMemDC, TRANSPARENT )
   SetTextColor( hMemDC, IIF( ::__MouseIn, oHeader:crHeaderHot, oHeader:crHeaderNormal ) )
   _DrawText( hMemDC, ::Caption, { x + nIconX + IIF( nIconX == 0, oHeader:rcTLPadding:right, 0 ), y, cx, y + nHeight }, DT_VCENTER | DT_SINGLELINE )
   IF nIconX > 0
      ::Parent:ImageList:DrawImage( hMemDC, ::ImageIndex, x, y-nIconY, ILD_TRANSPARENT )
   ENDIF

   IF hMemBitmap != NIL
      FOR EACH oChild IN ::Children
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
          ENDIF

          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )

          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )

          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, x + oChild:Left+oChild:__BackMargin, y-nIconY + oChild:Top+oChild:__BackMargin, SRCCOPY )

          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )

          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )

      NEXT
   ENDIF

   SelectObject( hMemDC, hFont )
   SelectObject( hMemDC, hOldPen )
   SelectObject( hMemDC, hOldBrush )
   DeleteObject( hPen )
   DeleteObject( hBrush )

   IF hMemBitmap != NIL
      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, x, y-nIconY, SRCCOPY )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF
RETURN 0

METHOD __SetSizePos( nPos, nVal ) CLASS Expando
   IF nPos > 2 .AND. nVal < 0
      nVal := 0
   ENDIF
   SWITCH nPos
      CASE 1
         ::xLeft := nVal
         EXIT
      CASE 2
         ::xTop := nVal
         EXIT
      CASE 3
         ::xWidth := nVal
         EXIT
      CASE 4
         IF ( procname(3) == "__OBJSENDMSG" .AND. !::xExpanded )
            nVal := ::Height
         ENDIF
         ::xHeight := nVal
         IF ::__ClassInst != NIL
            ::Application:ObjectManager:CheckValue( "Height", "Size", ::PaneHeight )
         ENDIF
         EXIT
   END

   IF ::hWnd != NIL
      ::MoveWindow()
   ENDIF
RETURN Self

#endif