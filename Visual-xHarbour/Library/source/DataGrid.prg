/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// DataGrid.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"
#include "dbinfo.ch"

#define WHEEL_PAGESCROLL 00041
#define CS_DROPSHADOW 0x00020000
#define BP_PUSHBUTTON            1
#define BP_RADIOBUTTON           2
#define BP_CHECKBOX              3
#define BP_GROUPBOX              4
#define BP_USERBUTTON            5

#define PBS_NORMAL       1
#define PBS_HOT          2
#define PBS_PRESSED      3
#define PBS_DISABLED     4
#define PBS_DEFAULTED    5

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

/* HEADER parts */
#define HP_HEADERITEM      1
#define HP_HEADERITEMLEFT  2
#define HP_HEADERITEMRIGHT 3
#define HP_HEADERSORTARROW 4

/* HEADER HEADERITEM states */
#define HIS_NORMAL  1
#define HIS_HOT     2
#define HIS_PRESSED 3

#define CREP_NORMAL      1
#define CREP_PROGRESSBAR 2
#define CREP_CHECKBOX    3
#define CREP_BUTTON      4

#define HEADERSIZEGAP    3
#define MENUBTNWIDTH    12

#define HIS_ICONNORMAL   7
#define HIS_ICONHOT      8
#define HIS_ICONPRESSED  9

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

#ifndef SPI_GETDROPSHADOW
   #define SPI_GETDROPSHADOW   0x1024
#endif

#xtranslate CEIL( <x> ) => ( if( <x> - Int( <x> ) > 0 , Int( <x> )+1, Int( <x> ) ) )

CLASS DataGrid INHERIT TitleControl
   PROPERTY ItemHeight                                                                              DEFAULT 18
   PROPERTY FullRowSelect                                                                           DEFAULT .F.
   PROPERTY ShadowRow                                                                               DEFAULT .T.
   PROPERTY AutoHorzScroll                                                                          DEFAULT .T.
   PROPERTY AutoVertScroll                                                                          DEFAULT .T.
   PROPERTY FreezeColumn                                                                            DEFAULT 0
   PROPERTY ShowHeaders                                                                             DEFAULT .T.
   PROPERTY HeaderHeight                                                                            DEFAULT 20
   PROPERTY ShowSelection                                                                           DEFAULT .T.
   PROPERTY ShowSelectionBorder                                                                     DEFAULT .T.
   PROPERTY AnchorColumn                                                                            DEFAULT -1
   PROPERTY ConvertOem                                                                              DEFAULT .F.

   PROPERTY SchemeHeader         ROOT "Appearance"                                                  DEFAULT .F.
   PROPERTY WrapHeaderText       ROOT "Appearance"                                                  DEFAULT .F.
   PROPERTY GridColor            ROOT "Colors"                                                      DEFAULT RGB(196,192,192)
   PROPERTY HighlightBorderColor ROOT "Colors"
   PROPERTY HighlightColor       ROOT "Colors"
   PROPERTY HighlightTextColor   ROOT "Colors"

   PROPERTY HoverRow             ROOT "Behavior"                                                    DEFAULT .T.
   PROPERTY SimpleRowDrag        ROOT "Behavior"                                                    DEFAULT .F.

   PROPERTY Columns
   PROPERTY AllowDragRecords                                                                        DEFAULT .F.
   PROPERTY DragImagePadding                                                                        DEFAULT 20

   PROPERTY ExtVertScrollBar                                                                        DEFAULT .F.
   PROPERTY MultipleSelection                                                                       DEFAULT .F.
   PROPERTY RightClickSelect                                                                        DEFAULT .F.
   PROPERTY ImageList           GET __ChkComponent( Self, @::xImageList )
   PROPERTY DataSource          GET __ChkComponent( Self, @::xDataSource ) SET ::__SetDataSource(v)

   PROPERTY ShowGrid            SET ::InvalidateRect()                                              DEFAULT .T.
   PROPERTY Striping            SET ::InvalidateRect()                                              DEFAULT .F.

   ASSIGN GradientHeader(l)     INLINE ::SchemeHeader := l

   DATA ColPos                  EXPORTED INIT 1
   DATA RowPos                  EXPORTED INIT 1
   DATA RowCountVisible         EXPORTED
   DATA RowCountUsable          EXPORTED
   DATA RowHeight               EXPORTED
   DATA bRowChanged             EXPORTED
   DATA aSelected               EXPORTED INIT {}
   DATA aTagged                 EXPORTED  INIT {}
   DATA CurPos                  EXPORTED INIT 1
   DATA CurTag                  EXPORTED
   DATA bSearchKey              EXPORTED

   // backward compatibility
   DATA HoverBackColor          EXPORTED
   DATA HoverForeColor          EXPORTED
   DATA HoverBorderColor        EXPORTED
   DATA TagRecords              EXPORTED INIT .f.
   DATA SmallCaption            EXPORTED INIT .T.

   ACCESS RowCount              INLINE LEN( ::__DisplayArray )
   ACCESS ColCount              INLINE LEN( ::Children )
   ACCESS HorzScrollPos         INLINE ABS( ::__HorzScrolled )
   ACCESS VertScrollPos         INLINE ABS( ::__VertScrolled )
   ACCESS AutoUpdate            INLINE IIF( ::__nUpdtTimer==0, .F., .T. )
   ASSIGN AutoUpdate(n)         INLINE ::__nUpdtTimer := n, IIF( n>0, ::SetTimer( 15, n*1000 ), ::KillTimer(15) )
   ACCESS Record                INLINE ::__GetPosition()
   ACCESS IsDelIndexOn          INLINE !EMPTY( ::DataSource ) .AND. ( "DELETED()" IN UPPER( ::DataSource:OrdKey() ) )
   ACCESS HoverHeader           INLINE ::__SelCol

   ACCESS HitTop                INLINE ::DataSource == NIL .OR. ::DataSource:Bof()
   ACCESS HitBottom             INLINE ::DataSource == NIL .OR. ::DataSource:eof()

   ACCESS DataHeight            INLINE ::__DataHeight
   DATA __HoverBackColor        PROTECTED

   DATA __SysGridColor          EXPORTED  INIT RGB(196,192,192)
   DATA __SysHighlightColor     EXPORTED
   DATA __SysHighlightTextColor EXPORTED

   DATA __HScrollUnits          PROTECTED INIT 15
   DATA __lCreated              PROTECTED INIT .F.
   DATA __bGoTop                PROTECTED INIT {||.F.}
   DATA __bGoBottom             PROTECTED INIT {||.F.}
   DATA __bRecNo                PROTECTED
   DATA __LinePen               PROTECTED
   DATA __SelBorderPen          PROTECTED
   DATA __HoverBorderPen        PROTECTED
   DATA __DataHeight            PROTECTED
   DATA __DataWidth             PROTECTED INIT 0
   DATA __VertScrolled          PROTECTED INIT 1
   DATA __HorzScrolled          PROTECTED INIT 0
   DATA __DisplayArray          PROTECTED INIT {}
   DATA __InactiveHighlight     PROTECTED
   DATA __CurControl            EXPORTED
   DATA __TrackColumn           PROTECTED
   DATA __DragColumn            PROTECTED INIT 0
   DATA __lResizing             PROTECTED INIT FALSE
   DATA __lMoving               PROTECTED INIT FALSE
   DATA __nUpdtTimer            PROTECTED INIT 0
   DATA __CheckPos              PROTECTED
   DATA __nScrolled             PROTECTED INIT 0
   DATA __lMouseDown            PROTECTED INIT .F.
   DATA __MenuReturn            PROTECTED INIT 1
   DATA __lSizeMouseDown        PROTECTED INIT .F.
   DATA __lMoveMouseDown        PROTECTED INIT .F.
   DATA __SelCol                PROTECTED INIT 0
   DATA __SelWidth              PROTECTED INIT 0
   DATA __SelLeft               PROTECTED INIT 0
   DATA __hDragImageList        PROTECTED
   DATA __prevDrag              PROTECTED INIT 0
   DATA __prevHotRow            PROTECTED INIT 0
   DATA __prevHotCol            PROTECTED INIT 0
   DATA __prevCol               PROTECTED INIT 1
   DATA __lHot                  PROTECTED INIT .F.
   DATA __nVPage                PROTECTED
   DATA __nVMax                 PROTECTED
   DATA __nVPos                 PROTECTED
   DATA __nHPage                PROTECTED
   DATA __nHMax                 PROTECTED
   DATA __nHPos                 PROTECTED
   DATA __hDragRecImage         PROTECTED
   DATA __nDragTop              PROTECTED INIT 0
   DATA __cTip                  PROTECTED
   DATA __nDragRec              PROTECTED INIT -1
   DATA __nDragPos              PROTECTED INIT -1
   DATA __hDragBrush            PROTECTED
   DATA __hPrevCursor           PROTECTED
   DATA __aPixels               PROTECTED
   DATA __aRect                 PROTECTED
   DATA __cSearch               PROTECTED INIT ""
   DATA __aLastHover            PROTECTED INIT {0,0}
   DATA __nHotHeader            PROTECTED
   DATA __cEditText             PROTECTED
   DATA __aEditCol              PROTECTED
   DATA __nXold                 PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Update()
   METHOD AutoAddColumns()
   METHOD ArrowRight()
   METHOD ArrowLeft()
   METHOD PageDown()
   METHOD PageUp()
   METHOD Home()
   METHOD End()
   METHOD Up()
   METHOD Down()

   METHOD GetItemRect()
   METHOD UpdateRow()
   METHOD DeleteColumn()

   METHOD GoBottom()     INLINE ::PostMessage( WM_KEYDOWN, VK_END ), Self
   METHOD GoTop()        INLINE ::PostMessage( WM_KEYDOWN, VK_HOME ), Self
   METHOD Edit()         INLINE ::SetFocus(), ::PostMessage( WM_KEYDOWN, VK_RETURN ), Self

   METHOD Skip( n )
   METHOD GetControl()   INLINE ::__CurControl

   METHOD OnRowChanging()     VIRTUAL
   METHOD OnColChanging()     VIRTUAL
   METHOD OnRowChanged()     VIRTUAL
   METHOD OnColChanged()     VIRTUAL
   METHOD OnClick()          VIRTUAL

   METHOD __DrawMultiText()
   METHOD __SetBlocks()
   METHOD __ResetDataSource()
   METHOD __SetDataSource()
   METHOD __GetHeaderHeight() INLINE IIF( ::ShowHeaders, ::HeaderHeight, 0 )
   METHOD __Update()
   METHOD __SizeUpdate()
   METHOD __DisplayData()
   METHOD __UpdateHScrollBar()
   METHOD __UpdateVScrollBar()
   METHOD __ScrollUp()
   METHOD __ScrollDown()
   METHOD __GetPosition()
   METHOD __ResetRecordPos()
   METHOD __UpdateHeight()
   METHOD __ControlSaveData()
   METHOD __Edit()
   METHOD __FillCol()
   METHOD __FillRow()
   METHOD __GoToRec()
   METHOD __SkipRecords()
   METHOD __ResetControl()
   METHOD __SetColPos()
   METHOD __SetColWidth()
   METHOD __GetDataValue(n)  INLINE &( ::Children[n]:Data )
   METHOD __CheckData()
   METHOD __GetDataWidth()

   METHOD Refresh() INLINE ::InvalidateRect()
   METHOD ColFromPos()

   MESSAGE OnParentSysCommand METHOD __OnParentSysCommand()
   MESSAGE OnTimer            METHOD __OnTimer()

   METHOD OnPaint()
   METHOD OnHorzScroll()
   METHOD OnVertScroll()

   METHOD SaveLayout()
   METHOD RestoreLayout()

   METHOD OnDestroy()
   METHOD OnKeyDown()
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnChar()
   METHOD OnSize()
   METHOD OnKillFocus()
   METHOD OnSetFocus()
   //METHOD OnGetDlgCode() INLINE DLGC_WANTMESSAGE
   METHOD OnGetDlgCode()// msg ) INLINE IIF( Len(::Children) == 0 .OR. ! ::Children[ ::ColPos ]:AutoEdit .AND. msg != NIL .AND. msg:message == WM_KEYDOWN .AND. msg:wParam IN {VK_RETURN,VK_ESCAPE}, NIL, DLGC_WANTMESSAGE )
   METHOD OnLButtonDblClk()
   METHOD OnItemChanged()
   METHOD OnEraseBkGnd( hDC ) INLINE IIF( LEN( ::Children ) > 0, 1, Super:OnEraseBkGnd( hDC ) )
   METHOD OnHeaderClick()
   METHOD OnMouseLeave()
   METHOD __DrawRepresentation()
   METHOD OnMouseMove()
   METHOD GetPosition()
   METHOD OnMouseWheel()
   METHOD CreateDragImage()
   METHOD GetRecordCount()
   METHOD DeselectAll()
   METHOD ToggleSelection()

   METHOD ResetSearch()    INLINE ::KillTimer( 10 ), ::__cSearch := ""
   METHOD ResetFrame()     INLINE ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER))
ENDCLASS

METHOD Init( oParent ) CLASS DataGrid
   DEFAULT ::__xCtrlName TO "DataGrid"
   ::ClsName                 := "DataGrid"
   ::__SysBackColor          := GetSysColor( COLOR_WINDOW )
   ::BackColor               := GetSysColor( COLOR_WINDOW )
   ::Style                   := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::Border                  := WS_BORDER
   ::Super:Init( oParent )
   ::Width                   := 340
   ::Height                  := 240

   ::EmptyLeft               := 0
   ::__IsStandard            := .F.
   ::IsContainer             := .F.

   ::ForeColor               := ::System:Colors:WindowText
   ::HighlightColor          := ::System:Colors:Highlight
   ::HighlightTextColor      := ::System:Colors:HighlightText

   ::__SysForeColor          := ::System:Colors:WindowText
   ::__SysHighlightColor     := ::System:Colors:Highlight
   ::__SysHighlightTextColor := ::System:Colors:HighlightText

   ::__InactiveHighlight     := RGB(240,240,240)
   ::__lCreateAfterChildren  := .T.
   ::DeferRedraw             := FALSE
   IF ::DesignMode
      AINS( ::Events, 1, {"Navigation", {;
                                          { "OnRowChanging",    "", "" },;
                                          { "OnRowChanged",     "", "" },;
                                          { "OnRowDragged",     "", "" },;
                                          { "OnBeginDragging",  "", "nCol" },;
                                          { "OnSearchNotFound", "", "" },;
                                          { "OnColChanging",    "", "" },;
                                          { "OnColChanged",     "", "" } } }, .T. )

      AADD( ::Events[3][2], { "OnQueryBackColor", "", "" } )
      AADD( ::Events[3][2], { "OnQueryForeColor", "", "" } )
      //AADD( ::Events[3][2], { "GetItemHeight", "", "" } )
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS DataGrid
   ::__SetBlocks()
   ::__LinePen      := CreatePen( PS_SOLID, 0, ::GridColor )
   IF ::HighlightBorderColor != NIL
      ::__SelBorderPen := CreatePen( PS_SOLID, 0, ::HighlightBorderColor )
   ENDIF

   IF ::HoverRow
      ::__HoverBackColor := LightenColor( ::HighlightColor, 180 )
      ::__HoverBorderPen := CreatePen( PS_SOLID, 0, IIF( ::HighlightBorderColor != NIL, LightenColor( ::HighlightBorderColor, 180 ), ::__HoverBackColor ) )
   ENDIF

   ::__hDragBrush   := CreateSolidBrush( DarkenColor( ::BackColor, 10 ) )

   ::xWidth  := MIN( ::xWidth,  ::Parent:ClientWidth  - ::Left - 5 )
   ::xHeight := MIN( ::xHeight, ::Parent:ClientHeight - ::Top - 5 )

   Super:Create()
   ::__GetDataWidth()
   ::__DataHeight   := ::ClientHeight - ::__GetHeaderHeight()

   ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )

   ::__Update()

   ::AutoUpdate := ::__nUpdtTimer
   ::__lCreated := .T.
   ::__UpdateHScrollBar()
   ::__UpdateVScrollBar()
   ::__hPrevCursor := ::Cursor
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD OnGetDlgCode( msg ) CLASS DataGrid
   IF msg != NIL
      IF Len(::Children) == 0 //.OR. ( ! ::Children[ ::ColPos ]:AutoEdit .AND. msg != NIL .AND. msg:message == WM_KEYDOWN .AND. msg:wParam == VK_RETURN )
         RETURN NIL
      ENDIF
      IF msg:message == WM_KEYDOWN .AND. msg:wParam == VK_ESCAPE
         RETURN NIL
      ENDIF
   ENDIF
RETURN DLGC_WANTMESSAGE

METHOD OnMouseLeave() CLASS DataGrid
   ::Super:OnMouseLeave()

   IF ::__nHotHeader != NIL .AND. ! ::__lMouseDown
      ::Children[ ::__nHotHeader ]:DrawHeader()
      ::__nHotHeader := NIL
   ENDIF
   IF ::__HoverBackColor != NIL .AND. ::__aLastHover[1] > 0
      ::__DisplayData( ::__aLastHover[1], , ::__aLastHover[1] )
      ::__aLastHover[1] := 0
   ENDIF
RETURN NIL

METHOD __DrawMultiText( hDC, aText, aData, nRight, zLeft, nWImg, nAlign, y, lHeader, nImgAlign, lSelected ) CLASS DataGrid
   LOCAL z, rc, nDif, aAlign, iLen, x, cx, pt := (struct POINT)

   IF lHeader .AND. ::WrapHeaderText .AND. LEN( aData ) == 1 .AND. ::__GetHeaderHeight() > y*2 .AND. _GetTextExtentPoint32( hDC, ALLTRIM(aData[1]) )[1] > aText[3]-aText[1]
      rc := (struct RECT)
      rc:left   := aText[1]
      rc:top    := aText[2]
      rc:right  := aText[3]
      rc:bottom := aText[4]
      DrawText( hDC, aData[1], @rc, ( IIF( nAlign == ALIGN_CENTER, DT_CENTER,0) | DT_CALCRECT|DT_WORDBREAK ) )
      aText[2]  := ( aText[4]-rc:bottom ) / 2
      aText[4]  := aText[2] + rc:bottom

      _DrawText( hDC, aData[1], aText, ( IIF( nAlign == ALIGN_CENTER, DT_CENTER,0) | DT_WORDBREAK ) )
    ELSE
      FOR z := 1 TO LEN( aData )
          aAlign := _GetTextExtentExPoint( hDC, ALLTRIM(aData[z]), aText[3]-aText[1]-nWimg, @iLen )
          IF aAlign != NIL
             nDif := (aAlign[1]) - (aText[3]-aText[1]-nWimg)
             IF nDif > 0 .AND. !EMPTY( ALLTRIM( aData[z] ) )
                aData[z] := ALLTRIM( LEFT( aData[z], iLen-2 ) )+ "..."
             ENDIF
             IF nAlign == ALIGN_LEFT
                x := zLeft + 4 + IIF( nImgAlign == ALIGN_LEFT, nWimg, 0 )
              ELSEIF nAlign == ALIGN_RIGHT
                aAlign := _GetTextExtentPoint32( hDC, ALLTRIM(aData[z]) )
                x := nRight - aAlign[1]-4 - IIF( nImgAlign == ALIGN_RIGHT, nWimg, 0 ) + IIF( nImgAlign == ALIGN_LEFT, nWimg, 0 )
              ELSEIF nAlign == ALIGN_CENTER
                aAlign := _GetTextExtentPoint32( hDC, ALLTRIM(aData[z]) )
                x := zLeft + ((nRight-zLeft)/2) - (aAlign[1]/2)
             ENDIF

             cx := aText[3]-( aText[1]-zLeft )
             IF ::Transparent .AND. ! lHeader .AND. ! lSelected
                SetBrushOrgEx( hDC, ::Parent:ClientWidth-::Left-1, ::Parent:ClientHeight-::Top-::TitleHeight-1, @pt )
                _FillRect( hDC, {aText[1],aText[2],cx,aText[4]}, ::Parent:BkBrush )
                SetBrushOrgEx( hDC, pt:x, pt:y )
             ENDIF
             _ExtTextOut( hDC, x, y, ETO_CLIPPED+IIF( z==1 .AND. ! lHeader .AND. ( lSelected .OR. ! ::Transparent ), ETO_OPAQUE, 0 ), {aText[1],aText[2],cx,aText[4]},aData[z])
             y += aAlign[2]
          ENDIF
      NEXT
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------------------------------
METHOD __GetDataWidth( lSetPos, lFixCol ) CLASS DataGrid
   LOCAL n, lHidden := .F., nLeft := 0
   DEFAULT lSetPos TO .F.
   DEFAULT lFixCol TO .F.
   ::__DataWidth := 0
   FOR n := 1 TO LEN( ::Children )
       IF ! lHidden
          lHidden := ::Children[n]:Width < ::__HorzScrolled .OR. nLeft > ::ClientWidth-::__HorzScrolled
          IF lFixCol .AND. ::Children[n]:__lHidden
             IF ! lHidden
                ::__FillCol(n)
             ENDIF
          ENDIF
       ENDIF
       ::Children[n]:__lHidden := lHidden

       nLeft += ::Children[n]:Width
       IF lSetPos
          ::Children[n]:xPosition := n
       ENDIF
       IF ::Children[n]:Visible
          ::__DataWidth += ::Children[n]:Width
       ENDIF
   NEXT
RETURN ::__DataWidth

//---------------------------------------------------------------------------------------------------------------------------
METHOD __GetPosition() CLASS DataGrid
   LOCAL nRec, lDeleted, nKeyNo, nDel := 0, nPos := 0
   IF ::DataSource != NIL
      IF ::DataSource:ClsName == "MemoryTable" .OR. ( ::DataSource:Driver IN { "SQLRDD", "SQLEX" } ) .OR. ::ExtVertScrollBar
         nRec   := ::DataSource:Recno()
         nKeyNo := ::DataSource:OrdKeyNo()
         nPos   := nKeyNo
         IF ! ::IsDelIndexOn .AND. nKeyNo > 1
            ::DataSource:Skip(-1)
            IF ::DataSource:Bof()
               ::DataSource:GoTop()
               nPos := 1
             ELSE
               lDeleted := Set( _SET_DELETED, .F. )
               IF ! lDeleted
                  ::DataSource:Goto( nRec )
                  nPos := nKeyNo
                ELSE
                  ::DataSource:Gotop()
                  WHILE ! ::DataSource:eof() .AND. ::DataSource:recno() != nRec
                     IF ::DataSource:Deleted()
                        nDel++
                     ENDIF
                     ::DataSource:Skip()
                  ENDDO
                  ::DataSource:Goto( nRec )
                  nPos := nKeyNo - nDel
               ENDIF
               Set( _SET_DELETED, lDeleted )
            ENDIF
         ENDIF
       ELSE
         nPos := Int( Round( ::DataSource:OrdKeyRelPos()*::DataSource:Recno(), 0 ) )
      ENDIF
   ENDIF
RETURN nPos

//---------------------------------------------------------------------------------------------------------------------------
METHOD GetPosition() CLASS DataGrid
RETURN ASCAN( ::__DisplayArray, {|a|a[2]==::DataSource:Recno()} )

//---------------------------------------------------------------------------------------------------------------------------
METHOD OnMouseWheel( nwParam, nlParam ) CLASS DataGrid
   LOCAL nLines, nScroll, nDelta, nPage, pt, rc, n, si, cBuffer
   pt := (struct POINT)
   pt:x := LOWORD(nlParam)
   pt:y := HIWORD(nlParam)
   ScreenToClient( ::hWnd, @pt )

   rc := (struct RECT)
   rc:left   := 0
   rc:top    := ::ClientHeight
   rc:right  := ::ClientWidth
   rc:bottom := ::Height

   SystemParametersInfo( SPI_GETWHEELSCROLLLINES, 0, @nLines, 0)
   IF nLines == 0
      nLines := 3
   ENDIF

   IF nLines > 0
      IF ::__aLastHover[1] > 0
         ::__DisplayData( ::__aLastHover[1], , ::__aLastHover[1] )
         ::__aLastHover := {0,0}
      ENDIF

      nDelta  := GETWHEELDELTA( nwParam )
      nScroll := WM_VSCROLL
      nPage   := IIF( ::__nVPage != NIL, ::__nVPage, ::ClientHeight )
      IF PtInRect( rc, pt )
         nScroll := WM_HSCROLL
         nPage   := ::__nHPage
      ENDIF
      si := (struct SCROLLINFO)
      cBuffer := _GetScrollInfo( ::hWnd, SB_VERT )
      si:Buffer( cBuffer, .T. )
      IF si:nPage != NIL .AND. si:nMax != NIL .AND. si:nPage < si:nMax
         IF nLines == WHEEL_PAGESCROLL
            IF nDelta > 0
               ::SendMessage( nScroll, SB_PAGEUP, 0 )
             ELSEIF nDelta < 0
               ::SendMessage( nScroll, SB_PAGEDOWN, 0 )
            ENDIF
          ELSE
            IF nDelta > 0
               FOR n := 1 TO nLines * ABS( nDelta )
                   ::SendMessage( nScroll, SB_LINEUP, 0 )
               NEXT
             ELSE
               FOR n := 1 TO nLines * ABS( nDelta )
                   ::SendMessage( nScroll, SB_LINEDOWN, 0 )
               NEXT
            ENDIF
         ENDIF
      ENDIF
      ::OnMouseMove( 0, MAKELPARAM(pt:x,pt:y), .F. )
   ENDIF
RETURN 0

//---------------------------------------------------------------------------------------------------------------------------
METHOD CreateDragImage(y) CLASS DataGrid
   LOCAL hImageList, hMemBitmap, nTop
   nTop       := ::__GetHeaderHeight() + ( ::ItemHeight*(::RowPos-1) )
   hMemBitmap := GetScreenBitmap( { 0, nTop, ::ClientWidth, nTop + ::ItemHeight }, ::hWnd )
   hImageList := ImageListCreate( ::ClientWidth, ::ItemHeight, (ILC_COLORDDB | ILC_MASK), 1, 0 )
   ImageListAdd( hImageList, hMemBitmap )
   DeleteObject( hMemBitmap )
   ::__nDragTop := y-nTop
RETURN hImageList

//---------------------------------------------------------------------------------------------------------------------------
METHOD OnMouseMove( wParam, lParam, lSuper ) CLASS DataGrid
   LOCAL n, nRow, nCol, nWidth, nDrag, nTop, nDragPos, nDragRec, x, y
   DEFAULT lSuper TO .T.

   IF lSuper
      ::Super:OnMouseMove( wParam, lParam )
   ENDIF

   x := LOWORD( lParam )
   y := HIWORD( lParam )

   IF ::__CurControl != NIL .AND. ::__CurControl:ClsName == "Edit" .AND. ::__CurControl:Button
      ::__CurControl:RedrawWindow(,, RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
   ENDIF

   IF ::ShowHeaders
      IF !::__lSizeMouseDown .AND. !::__lMoveMouseDown
         IF ( nRow := Int( Ceiling( (y-::__GetHeaderHeight() ) / ::ItemHeight ) ) ) <= 0
            IF ::__HoverBackColor != NIL .AND. ::__aLastHover[1] > 0
               ::__DisplayData( ::__aLastHover[1], , ::__aLastHover[1] )
               ::__aLastHover[1] := 0
            ENDIF

            nWidth := 0
            FOR n := 1 TO LEN( ::Children )
                nWidth += ::Children[n]:Width
                IF nWidth >= x
                   EXIT
                ENDIF
            NEXT

            IF n > ::FreezeColumn
               nWidth := ::__HorzScrolled
               FOR n := 1 TO LEN( ::Children )
                   nWidth += ::Children[n]:Width
                   IF nWidth >= x
                      EXIT
                   ENDIF
               NEXT
            ENDIF

            IF nWidth-x > HEADERSIZEGAP
               nWidth -= ::Children[n]:Width
               n--
            ENDIF

            IF ABS( nWidth-x ) <= HEADERSIZEGAP
               IF n <= LEN( ::Children ) .AND. n > 0 .AND. ::Children[n]:AllowSize
                  ::Cursor := ::System:Cursor:SizeWE
                ELSE
                  ::Cursor := NIL
               ENDIF
             ELSE
               ::Cursor := NIL
            ENDIF
            IF ::Cursor == NIL
               IF n+1 > 0 .AND. n+1 <= LEN(::Children)
                  IF ! EMPTY( ::Children[n+1]:HeaderTooltip ) .AND. ::Tooltip:Text != ::Children[n+1]:HeaderTooltip
                     IF ::__cTip == NIL
                        ::__cTip := ::Tooltip:Text
                     ENDIF
                     IF ! ::ToolTip:IsWindow()
                        ::ToolTip:Create()
                     ENDIF
                     ::Tooltip:Text := ::Children[n+1]:HeaderTooltip
                  ENDIF
                  IF ::__nHotHeader <> n+1//wParam == MK_LBUTTON
                     IF ::__nHotHeader != NIL
                        ::Children[ ::__nHotHeader ]:DrawHeader()
                     ENDIF
                     ::Children[n+1]:DrawHeader(,,,,, .T.)
                  ENDIF
                  ::__nHotHeader := n+1
               ENDIF
            ENDIF
         ELSE
            IF ::__nHotHeader != NIL
               ::Children[ ::__nHotHeader ]:DrawHeader()
               ::__nHotHeader := NIL
            ENDIF
            IF ::__cTip != NIL
               ::Tooltip:Text := ::__cTip
               ::__cTip := NIL
            ENDIF
            IF wParam == MK_LBUTTON .AND. ::AllowDragRecords .AND. ::__nDragRec > -1
               nTop := y + ::DragImagePadding - ::__nDragTop

               IF ::__hDragRecImage == NIL
                  ::__hDragRecImage := ::CreateDragImage(y)
                  ::__DisplayData( ::RowPos, , ::RowPos, )

                  ImageListBeginDrag( ::__hDragRecImage, 0, 0, 0 )
                  ImageListDragEnter( ::hWnd, 0, nTop )
                  ImageListDragShowNolock(.T.)
               ENDIF

               nDragPos := MIN( LEN( ::__DisplayArray ), Int( Ceiling((y-::__GetHeaderHeight()) /::ItemHeight) ) )
               nDragRec := ::__DisplayArray[ nDragPos ][2]

               IF nDragRec != ::__nDragRec
                  ::__nDragRec := nDragRec
                  ::__nDragPos := nDragPos

                  ImageListDragShowNolock(.F.)
                  ::__DisplayData( nDragPos-1, , nDragPos+1,  )
                  ImageListDragShowNolock(.T.)
               ENDIF
               ImageListDragMove( x-::__nXold, nTop )
               ::UpdateWindow()

            ELSEIF wParam != MK_LBUTTON .AND. ::__HoverBackColor != NIL

               nCol := ::ColFromPos(x)
               IF ::__aLastHover[1] <> nRow .OR. ( ! ::FullRowSelect .AND. ::__aLastHover[2] <> nCol )
                  IF ::__aLastHover[1] > 0
                     ::__DisplayData( ::__aLastHover[1], IIF( ! ::FullRowSelect, ::__aLastHover[2],), ::__aLastHover[1], IIF( ! ::FullRowSelect, ::__aLastHover[2],) )
                  ENDIF
                  IF nRow <= ::RowCountUsable
                     ::__DisplayData( nRow, IIF( ! ::FullRowSelect, nCol,), nRow, IIF( ! ::FullRowSelect, nCol,),, nRow <= ::RowCountUsable )
                     ::__aLastHover[1] := nRow
                     ::__aLastHover[2] := nCol
                  ELSE
                     ::__aLastHover := {0,0}
                  ENDIF
               ENDIF
            ENDIF
            ::Cursor := ::__hPrevCursor
         ENDIF

       ELSEIF ::__lSizeMouseDown
         ::__SelWidth := 0
         IF ::__SelCol > ::FreezeColumn
            ::__SelWidth := ::__HorzScrolled
         ENDIF

         FOR n := 1 TO ::__SelCol
             ::__SelWidth += ::Children[n]:Width
         NEXT
         ::__SetColWidth( ::__SelCol, MAX( 1, x - (::__SelWidth-::Children[::__SelCol]:Width) ) )
         ::UpdateWindow()

       ELSEIF ::__lMoveMouseDown

         nDrag  := 0
         nWidth := 0

         FOR n := 1 TO LEN( ::Children )
             nWidth += ::Children[n]:Width
             IF nWidth > x .OR. n >= LEN( ::Children )
                nDrag := n
                EXIT
             ENDIF
         NEXT
         IF nDrag > ::FreezeColumn .AND. ::__HorzScrolled < 0
            nWidth := ::__HorzScrolled
            FOR n := 1 TO LEN( ::Children )
                nWidth += ::Children[n]:Width
                IF nWidth > x .OR. n >= LEN( ::Children )
                   nDrag := n
                   EXIT
                ENDIF
            NEXT
         ENDIF

         IF nDrag > 0 .AND. ::__DragColumn != nDrag .AND. nDrag <> ::__prevDrag
            ImageListDragShowNolock( .F. )
            IF ::__DragColumn > 0
               ::Children[ ::__DragColumn ]:DrawHeader()
            ENDIF

            ::__DragColumn := nDrag

            IF ::Children[ nDrag ]:AllowDrag
               ::Children[ nDrag ]:DrawHeader(,,,, .T. )
            ENDIF

            ImageListDragShowNolock( .T. )
            ::__prevDrag := nDrag
         ENDIF
         IF ::__SelCol > 0
            n := x - ::__SelLeft - ::Children[::__SelCol]:Width + 1
            ImageListDragMove( n, IIF( !EMPTY( ::xText ), ::__nCaptionHeight, 0 )+1 )
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------------------------------
METHOD SaveLayout( cIniFile, cSection ) CLASS DataGrid
   LOCAL oIni, n, cEntry := ""
   FOR n := 1 TO LEN( ::Children )
       IF !EMPTY( cEntry )
          cEntry += "|"
       ENDIF
       cEntry += ::Children[n]:Name  + ", " + XSTR(::Children[n]:Width) + ", " + XSTR(::Children[n]:Position)
   NEXT
   IF EMPTY( cIniFile )
      oIni := ::Application:IniFile
    ELSE
      oIni := IniFile( cIniFile )
   ENDIF
   oIni:WriteString( cSection, ::Application:Name + "_" + ::Form:Name + "_" + ::Name, cEntry )
   Super:SaveLayout( cIniFile, cSection )
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD RestoreLayout( cIniFile, cSection )  CLASS DataGrid
   LOCAL aColumns, x, n, aColumn, c, oIni
   IF EMPTY( cIniFile )
      oIni := ::Application:IniFile
    ELSE
      oIni := IniFile( cIniFile )
   ENDIF
   c := oIni:ReadString( cSection, ::Application:Name + "_" + ::Form:Name + "_" + ::Name, "" )
   IF !EMPTY(c)
      aColumns := hb_atokens(c, "|" )

      FOR n := 1 TO LEN( aColumns )
          aColumn := hb_atokens(aColumns[n], "," )

          IF ( x := ASCAN( ::Children, {|o| UPPER(o:Name) == UPPER(aColumn[1]) } ) ) > 0
             ::Children[x]:Width    := VAL(aColumn[2])
             ::Children[x]:Position := VAL(aColumn[3])
          ENDIF
      NEXT
   ENDIF
   ::Update()
   Super:RestoreLayout( cIniFile, cSection )
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD __CheckData( cData ) CLASS DataGrid
   LOCAL lFailed := .F.
   TRY
      cData := &cData
    catch
      lFailed := .T.
   END
RETURN lFailed

//---------------------------------------------------------------------------------------------------------------------------
METHOD __SetDataSource( oSource ) CLASS DataGrid
   LOCAL n, hWnd, lUpdate := .F.

   oSource := __ChkComponent( Self, oSource )

   IF ( hWnd := GetWindow( ::hWnd, (GW_CHILD | GW_HWNDFIRST) ) ) > 0
      DestroyWindow( hWnd )
   ENDIF

   IF ::Children != NIL
      FOR n := 1 TO LEN( ::Children )
          ::Children[n]:Destroy()
          n--
      NEXT
      ::Children    := {}
   ENDIF
   ::__DisplayArray := {}

   IF VALTYPE(oSource) == "O"
      ::__SetBlocks()
      oSource:bOnFileNameChanged := {|o| Self:__ResetDataSource( o ) }
      oSource:bOnFileClosed := {|| Self:__SetDataSource( NIL ) }
    ELSE
      ::InvalidateRect()
   ENDIF
   ::__DataHeight   := ::ClientHeight - ::__GetHeaderHeight()
   IF oSource == NIL
      ::__DataWidth := 0
      ::__UpdateHScrollBar( .T., .T. )
      ::__UpdateVScrollBar( .T., .T. )
      ::__DisplayData()
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD __ResetDataSource( oSource ) CLASS DataGrid
   ::__SetDataSource( oSource )
   IF ::DesignMode
      ::AutoAddColumns()
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD __GoToRec( nRec ) CLASS DataGrid
   IF nRec != NIL
      ::DataSource:Goto( nRec )
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD __SkipRecords( n ) CLASS DataGrid
   IF n <> 0
      ::DataSource:Skip( n )
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD __SetBlocks() CLASS DataGrid
   IF !VALTYPE( ::DataSource ) == "O" .OR. ::DataSource:Fields == NIL .OR. !::DataSource:IsOpen
      RETURN NIL
   ENDIF
   ::__VertScrolled := ::Record
   ::__bGoTop    := <||
                      IF ::DataSource != NIL
                         ::DataSource:Gotop()
                      ENDIF
                    >

   ::__bGoBottom := <||
                      IF ::DataSource != NIL
                         ::DataSource:GoBottom()
                      ENDIF
                    >

   ::__bRecNo    := {|| ::DataSource:RecNo() }
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD __OnTimer( nID ) CLASS DataGrid
   ::KillTimer( nID )
   DO CASE
      CASE nID == 20
           IF ::__CurControl != NIL
              ::__CurControl:BackColor := ::__aEditCol[1]
              ::__CurControl:ForeColor := ::__aEditCol[2]
              ::__aEditCol := NIL
           ENDIF

      CASE nID == 15
           ::Update()
           ::SetTimer( 15, ::__nUpdtTimer*1000 )

      CASE nID == 10
           ::__cSearch := ""

   ENDCASE
RETURN 0

//---------------------------------------------------------------------------------------------------------------------------
METHOD Skip( n ) CLASS DataGrid
   DEFAULT n TO 1
   ::__SkipRecords( n )
   IF ::RowPos< ::RowCountUsable
      ::RowPos++
    ELSE
      ::__VertScrolled++
   ENDIF
   ::__ResetRecordPos()
   //::__DisplayData( ::RowPos, , ::RowPos,  )
RETURN Self

//---------------------------------------------------------------------------------

METHOD __SetColPos( nCol, nPos ) CLASS DataGrid
   LOCAL n, oCol
   IF nPos > 0 .AND. nPos <= LEN( ::Children )
      oCol := ::Children[ nCol ]

      ADEL( ::Children, nCol, .T. )
      AINS( ::Children, nPos, oCol, .T. )

      n := 1
      AEVAL( ::Children, {|o|o:xPosition := n++} )

      ::__Update()
      ::InvalidateRect()
      ::__UpdateHScrollBar()
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------

METHOD OnHeaderClick(iItem) CLASS DataGrid
   LOCAL nRet := ExecuteEvent( "OnHeaderClick", ::Children[ iItem ] )
RETURN nRet

//---------------------------------------------------------------------------------

METHOD DeleteColumn( nCol, lDisplay ) CLASS DataGrid
   LOCAL n := 1
   DEFAULT nCol TO ::ColPos

   IF nCol <= 0 .OR. nCol > ::ColCount
      RETURN(.F.)
   ENDIF

   ::Children[ nCol ]:RemoveProperty()

   IF ::Children[ nCol ]:TreeItem != NIL
      ::Children[ nCol ]:TreeItem:Delete()
      ::Children[ nCol ]:TreeItem := NIL
   ENDIF

   ::Children[ nCol ]:Font:Delete()
   ::Children[ nCol ]:Font:Owner := NIL
   ::Children[ nCol ]:Font := NIL

   ::Children[ nCol ]:HeaderFont:Delete()
   ::Children[ nCol ]:HeaderFont:Owner := NIL
   ::Children[ nCol ]:HeaderFont := NIL

   aDel( ::Children, nCol, .T. )

   ::__GetDataWidth(.T.)
   IF ::IsWindow()
      TRY
         ::__Update( lDisplay )
         ::__UpdateHScrollBar()
         IF ::DataSource != NIL
            ::__DisplayData()
         ENDIF
         ::InvalidateRect()
      CATCH
      END
   ENDIF
RETURN(.T.)

//---------------------------------------------------------------------------------

METHOD OnItemChanged( nSize ) CLASS DataGrid
   IF nSize > 0 .AND. ::__TrackColumn != NIL .AND. ::__TrackColumn > 0
      RETURN ::__SetColWidth( ::__TrackColumn, nSize )
   ENDIF
RETURN 0

//---------------------------------------------------------------------------------

METHOD UpdateRow() CLASS DataGrid
   ::__FillRow()
   ::__DisplayData( ::RowPos, , ::RowPos,  )
RETURN Self

//---------------------------------------------------------------------------------

METHOD __SetColWidth( nCol, nWidth ) CLASS DataGrid
   LOCAL i, nDiff, lPrev := .F.
   IF nCol != NIL .AND. LEN( ::Children ) >= nCol
      IF ::Children[ nCol ]:xWidth == 0 .AND. nCol > 1
         lPrev := .T.
      ENDIF
      IF ::Children[ nCol ]:MaxWidth > 0
         nWidth := MIN( nWidth, ::Children[ nCol ]:MaxWidth )
      ENDIF
      IF ::Children[ nCol ]:MinWidth > 0
         nWidth := MAX( nWidth, ::Children[ nCol ]:MinWidth )
      ENDIF

      nDiff := nWidth - ::Children[ nCol ]:xWidth
      ::Children[ nCol ]:xWidth += nDiff
      ::__DataWidth += nDiff

      FOR i := nCol+1 TO ::ColCount
          IF ::Children[i]:__lHidden
             ::Children[i]:__lHidden := ::Children[i]:Width < ::__HorzScrolled .OR. (::Children[i]:__nLeft-::Children[i]:Width) > ::ClientWidth
             IF ! ::Children[i]:__lHidden
                ::__FillCol(i)
             ENDIF
          ENDIF
          ::Children[i]:__nLeft += nDiff
      NEXT

      ::InvalidateRect( , .F. )
      ::__UpdateHScrollBar(.T.)
   ENDIF
   ::__GetDataWidth(,.T.)
RETURN 1

//---------------------------------------------------------------------------------

METHOD __ResetControl() CLASS DataGrid
   IF ::__CurControl != NIL
      IF ::Children[ ::ColPos ]:ControlValid != NIL
         ::__CurControl:IsValid := FALSE
      ENDIF
      ::__CurControl:Destroy()
      ::__CurControl:= NIL
      RETURN TRUE
   ENDIF
RETURN FALSE

//---------------------------------------------------------------------------------

METHOD OnSize( nwParam, nlParam ) CLASS DataGrid
   LOCAL n, lRefresh := .T., aPerc := {}
   ::Super:OnSize( nwParam, nlParam )
   IF ::DataSource != NIL .AND. !::DataSource:IsOpen
      RETURN 0
   ENDIF

   IF ! ::DesignMode .AND. ::AnchorColumn > 0 .AND. LEN( ::Children ) >= ::AnchorColumn .AND. ( ::__DataWidth <> ::ClientWidth )
      ::Children[ ::AnchorColumn ]:xWidth := ( ::ClientWidth - ::__DataWidth ) + ::Children[ ::AnchorColumn ]:xWidth
      ::__GetDataWidth()
      ::__DisplayData()

   ELSEIF ! ::DesignMode .AND. ::AnchorColumn == 0
      FOR n := 1 TO LEN( ::Children )
          AADD( aPerc, ::Children[n]:Width / ::__DataWidth )
      NEXT
      FOR n := 1 TO LEN( aPerc )
          ::Children[n]:xWidth := ::ClientWidth * aPerc[n]
      NEXT
      ::__GetDataWidth()
      ::__DisplayData()
   ENDIF

   ::__DataHeight   := ::ClientHeight - ::__GetHeaderHeight()

   IF EMPTY( ::__DisplayArray )
      ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
      ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )
      ::__Update( FALSE )
      IF ::Visible .AND. EMPTY( ::__DisplayArray )
         ::__DisplayData()
      ENDIF
    ELSE
      ::__GetDataWidth(,.T.)
      ::__SizeUpdate()
      IF ::__DataWidth < ::ClientWidth
         ::InvalidateRect( { ::__DataWidth, 0, ::ClientWidth, ::ClientHeight } )
      ENDIF
   ENDIF

   ::__UpdateHScrollBar( lRefresh )
   ::__UpdateVScrollBar( lRefresh )

   IF ::__nHPage != NIL
      IF ::__nHMax-::__nHPage+1 < ABS( ::__HorzScrolled )
         IF ::__nHMax != 0
            ::OnHorzScroll( SB_THUMBTRACK, ABS( ::__DataWidth-::__nHPage+1 ),, TRUE )
          ELSE
            IF ::__HorzScrolled != 0
               ::__HorzScrolled := 0
               ::__DisplayData()
               ::ValidateRect()
            ENDIF
         ENDIF
      ENDIF
    ELSE
      ::__HorzScrolled := 0
      ::__DisplayData()
      ::ValidateRect()
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------

METHOD __SizeUpdate( x, y ) CLASS DataGrid
   LOCAL aRect, nRowsV, nRowsU, nRec, n, nSkiped
   LOCAL nLast, nFirst, nRows

   IF x == NIL
      aRect := _GetClientRect( ::hWnd )
      x := aRect[3]
      y := aRect[4]
   ENDIF
   ::ClientWidth  := x
   ::ClientHeight := y

   ::__DataHeight  := y - ::__GetHeaderHeight()

   nRowsV := Ceil( ::__DataHeight / ::ItemHeight )
   nRowsU := Int(  ::__DataHeight / ::ItemHeight )

   IF nRowsV <= ::RowCountVisible .AND. !EMPTY( ::__DisplayArray )

      IF nRowsV < ::RowCountVisible .AND. nRowsV < LEN( ::__DisplayArray )
         ASIZE( ::__DisplayArray, nRowsV )
         ::RowCountVisible := nRowsV
         ::RowCountUsable  := MIN( nRowsU, ::RowCount )
         ::__DisplayData()
         RETURN -1
      ENDIF
      RETURN 1
   ENDIF

   IF nRowsV > ::RowCount .AND. !EMPTY( ::__DisplayArray ) .AND. ::DataSource != NIL
      nRec  := ::DataSource:Recno()
      nLast := ATAIL( ::__DisplayArray )[2]
      nFirst:= ::__DisplayArray[1][2]

      ::__GoToRec( nLast )

      nRows := nRowsV - ::RowCount

      nSkiped := 0
      IF !::HitBottom

         FOR n := 1 TO nRows
             ::__SkipRecords( 1 )
             IF ::HitBottom
                EVAL( ::__bGoBottom )
                EXIT
             ENDIF
             nSkiped++
             AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )
             ::__FillRow( ::RowCount )
         NEXT

      ENDIF

      nRows := nRowsU - ::RowCount

      IF nRows > 0
         nFirst:= ::__DisplayArray[1][2]
         ::__GoToRec( nFirst )
         IF !::HitTop
            FOR n := 1 TO nRows

                ::__SkipRecords( -1 )
                IF ::HitTop
                   EVAL( ::__bGoTop )
                   EXIT
                ENDIF

                AINS( ::__DisplayArray, 1, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() }, TRUE )
                ::RowPos++
                ::__VertScrolled--
                ::__FillRow( 1 )
            NEXT n
         ENDIF
      ENDIF

      ::__GoToRec( nRec )
      ::RowCountVisible := nRowsV
      ::RowCountUsable  := MIN( nRowsU, ::RowCount )

      IF ::RowCount < ::RowCountVisible
         ::__DisplayData()
      ENDIF
      RETURN 2

   ENDIF
   ::RowCountVisible := nRowsV
   ::RowCountUsable  := MIN( nRowsU, ::RowCount )
   ::__ResetRecordPos(.F.)
RETURN 0

//----------------------------------------------------------------------------------

METHOD OnChar( nKey ) CLASS DataGrid
   IF EMPTY( ::__DisplayArray )
      RETURN NIL
   ENDIF
   IF nKey != 27 .AND. nKey != 9
      ::__Edit(1, 0, 0, GRID_CHAR, nKey )
      IF ::__CurControl != NIL
         RETURN 0
      ENDIF
   ENDIF
   IF (nKey IN {13,9}) .AND. ::__CurControl == NIL .AND. ::Action != NIL
      __Evaluate( ::Action, Self )
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------

METHOD OnLButtonDblClk( nwParam, xPos, yPos ) CLASS DataGrid
   LOCAL nClickRow  := Ceiling((yPos-::__GetHeaderHeight()) /::ItemHeight)
   (nwParam)
   (xPos)

   IF ::__hDragRecImage != NIL
      ImageListDestroy( ::__hDragRecImage )
      ::__hDragRecImage := NIL
      ImageListEndDrag()
   ENDIF

   IF nClickRow <= ::RowCountUsable
      ::__ResetControl()
      IF ::Action != NIL
         __Evaluate( ::Action, Self )
      ENDIF
      RETURN NIL
   ENDIF
RETURN 0

//----------------------------------------------------------------------------------
METHOD OnLButtonUp( nwParam, xPos, yPos ) CLASS DataGrid
   LOCAL lMouse, lDrag := .F., nPos, aDrag, aMove, i, nRec, aData := {}
   (nwParam)
   (xPos)

   lMouse := ::__lMouseDown
   ::__lMouseDown := .F.

   ::ReleaseCapture()
   IF ::__hDragImageList != NIL
      ImageListDestroy( ::__hDragImageList )
      ::__hDragImageList := NIL
      ImageListEndDrag()
   ENDIF

   nPos := Int( Ceiling( (yPos-::__GetHeaderHeight() ) / ::ItemHeight ) )
   nPos := MAX( 1, MIN( nPos, ::RowCountUsable ) )

   IF ::__hDragRecImage != NIL
      ImageListEndDrag()
      ImageListDestroy( ::__hDragRecImage )
      ::__hDragRecImage := NIL
      lDrag := .T.
   ENDIF

   ::__nDragRec := -1
   ::__nDragPos := -1

   IF nPos <> ::RowPos .AND. lDrag .AND. nPos > 0
      aDrag := ARRAY( LEN( ::DataSource:Structure ) )
      aMove := ARRAY( LEN( ::DataSource:Structure ) )
      nRec  := ::DataSource:Recno()

      aEval( aDrag, {|a,n| (a), aDrag[n] := ::DataSource:FieldGet(n) } )

      IF ::DataSource:FileLock()
         IF nPos > ::RowPos // Drag down

            FOR i := ::RowPos+1 TO nPos
                ::DataSource:Skip()
                aEval( aMove, {|a,n| (a), aMove[n] := ::DataSource:FieldGet(n) } )
                AADD( aData, ACLONE( aMove ) )
            NEXT
            AADD( aData, ACLONE( aDrag ) )
            ::DataSource:Goto( nRec )

            FOR i := 1 TO LEN( aData )
                aEval( aData[i], {|a,n| (a), ::DataSource:FieldPut(n, aData[i][n] ) } )
                ::DataSource:Skip()
            NEXT
            ::DataSource:Skip(-1)

          ELSE //Drag up

            FOR i := ::RowPos-1 TO nPos STEP -1
                ::DataSource:Skip(-1)
                aEval( aMove, {|a,n| (a), aMove[n] := ::DataSource:FieldGet(n) } )
                AADD( aData, ACLONE( aMove ) )
            NEXT
            AADD( aData, ACLONE( aDrag ) )
            ::DataSource:Goto( nRec )

            FOR i := 1 TO LEN( aData )
                aEval( aData[i], {|a,n| (a), ::DataSource:FieldPut(n, aData[i][n] ) } )
                ::DataSource:Skip(-1)
            NEXT
            IF nPos > 1
               ::DataSource:Skip()
            ENDIF

         ENDIF
         ::DataSource:Unlock()
      ENDIF
      ::Update()
      ExecuteEvent( "OnRowDragged", Self )

   ELSEIF LEN( ::__DisplayArray ) > 0 .AND. nPos > 0
      IF ::__nDragRec != -1
         ::__nDragRec := -1
         ::__nDragPos := -1
         ::__DisplayData()
      ENDIF
      IF nPos > 1
         ::UpdateRow()
      ENDIF
      IF lMouse .AND. nPos == ::RowPos .AND. ::Children[ ::ColPos ]:Representation == CREP_BUTTON
         ExecuteEvent( "ButtonClicked", ::Children[ ::ColPos ] )
      ENDIF
   ENDIF

   IF ::__lMoveMouseDown .AND. ::__DragColumn > 0 .AND. LEN( ::Children ) > 0
      IF ::__DragColumn <> ::__SelCol .AND. ::Children[ ::__DragColumn ]:AllowDrag
         ::Children[ ::__SelCol ]:Position := ::__DragColumn
      ENDIF
      ::Children[ ::__DragColumn ]:DrawHeader()
   ENDIF

   IF !::__lSizeMouseDown .AND. (::__DragColumn == 0 .OR. ::__DragColumn == ::__SelCol) .AND. ::__SelCol > 0 .AND. Len( ::Children ) >= ::__SelCol
      IF ::Children[ ::__SelCol ]:HeaderMenu == NIL
         // TEST AREA
         ::__lSizeMouseDown := .F.
         ::__lMoveMouseDown := .F.
         ::__DragColumn := 0
         ::__SelWidth   := 0
         ::OnHeaderClick( ::__SelCol )
      ENDIF
   ENDIF
   ::__lSizeMouseDown := .F.
   ::__lMoveMouseDown := .F.
   ::__DragColumn := 0
   ::__SelCol     := 0
   ::__SelWidth   := 0
RETURN NIL

//----------------------------------------------------------------------------------
METHOD ColFromPos(xPos) CLASS DataGrid
   LOCAL n, nClickCol := 0, nWidth
   nWidth := 0
   FOR n := 1 TO LEN( ::Children )
       nWidth += ::Children[n]:Width
       IF nWidth > xPos-::__HorzScrolled
          nClickCol := n
          EXIT
       ENDIF
   NEXT
RETURN nClickCol

//----------------------------------------------------------------------------------
METHOD DeselectAll() CLASS DataGrid
   IF ! Empty( ::aSelected )
      ::aSelected := {}
      ::__DisplayData()
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------
METHOD ToggleSelection( nRec ) CLASS DataGrid
   LOCAL n
   DEFAULT nRec TO ::DataSource:Recno()
   IF ( n := ASCAN( ::aSelected, nRec ) ) > 0
      ADEL( ::aSelected, n, .T. )
    ELSE
      AADD( ::aSelected, nRec )
   ENDIF
   ::__DisplayData( ::RowPos, , ::RowPos )
RETURN Self

//----------------------------------------------------------------------------------
METHOD OnLButtonDown( nwParam, xPos, yPos ) CLASS DataGrid
   LOCAL nCol, nRow, n, lRes, lDescending, cTag, rc, nShadow
   LOCAL nClickRow, lUpdt := .F.
   LOCAL nClickCol, pt //, lShift, lCtrl, i
   LOCAL lLineChange:=.F.
   LOCAL lSameRow := .F.
   (nwParam)
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )

   ::__nXold := xPos

   nClickRow  := Ceiling((yPos-::__GetHeaderHeight()) /::ItemHeight)
   IF nClickRow < 1
      ::SetCapture()

      nClickCol := 1
      ::__SelWidth := 0

      FOR n := 1 TO LEN( ::Children )
          ::__SelWidth += ::Children[n]:Width
          IF ::__SelWidth > xPos .OR. n >= LEN( ::Children )
             nClickCol := n
             EXIT
          ENDIF
      NEXT
      IF nClickCol > 0
         IF nClickCol > ::FreezeColumn .AND. ::__HorzScrolled < 0
            ::__SelWidth := ::__HorzScrolled

            FOR n := 1 TO LEN( ::Children )
                ::__SelWidth += ::Children[n]:Width
                IF ::__SelWidth > xPos .OR. n >= LEN( ::Children )
                   nClickCol := n
                   EXIT
                ENDIF
            NEXT
         ENDIF

         IF ::__SelWidth-xPos > HEADERSIZEGAP .AND. ::Cursor == ::System:Cursor:SizeWE
            ::__SelWidth -= ::Children[nClickCol]:Width
            nClickCol--
         ENDIF

         ::__ResetControl()
         IF nClickCol > 0

            ::__SelCol := nClickCol

            IF ABS( ::__SelWidth-xPos ) <= HEADERSIZEGAP
               IF !::Children[ nClickCol ]:AllowSize
                  RETURN NIL
               ENDIF
               ::__lSizeMouseDown := .T.
            ELSEIF nClickCol <= LEN( ::Children )

               IF ::Children[ ::__SelCol ]:HeaderMenu != NIL
                  rc := (struct RECT)
                  rc:left   := ::Children[::__SelCol]:aSelRect[3] - MENUBTNWIDTH
                  rc:top    := 0
                  rc:right  := ::Children[::__SelCol]:aSelRect[3]
                  rc:bottom := ::HeaderHeight

                  pt := (struct POINT)
                  GetCursorPos( @pt )
                  pt:x := xPos
                  pt:y := yPos

                  IF PtInRect( rc, pt )
                     pt := (struct POINT)
                     pt:x := ::Children[::__SelCol]:aSelRect[3]
                     pt:y := ::HeaderHeight
                     ClientToScreen( ::hWnd, @pt )

                     SystemParametersInfo( SPI_GETDROPSHADOW, 0, @nShadow, 0)
                     ::__lMouseDown := .T.
                     ::Children[ ::__SelCol ]:DrawHeader(,,,,.T.,.T.)
                     ::Children[ ::__SelCol ]:HeaderMenu:Show( pt:x+if( nShadow==1, 5, 0 ), pt:y, (TPM_RIGHTALIGN | TPM_TOPALIGN) )
                     ::__lMouseDown := .F.
                     RETURN NIL
                  ENDIF
               ENDIF
               cTag := ::Children[ nClickCol ]:Tag
               IF ! Empty( cTag )
                  lDescending := ::DataSource:OrdDescend( cTag )

                  IF ::__prevCol <> nClickCol .OR. Empty( ::CurTag )
                     ::DataSource:OrdSetFocus( cTag )
                  ELSE
                     lDescending := ! lDescending
                     ::DataSource:OrdDescend( cTag, lDescending )
                  ENDIF

                  ::CurTag    := cTag
                  ::Children[ ::__SelCol ]:DrawHeader()
                  ::Children[ nClickCol ]:__SetSortArrow( IIF( lDescending, 1, 2 ) )
                  ::Update()
               ENDIF

               ::__prevCol := nClickCol

               IF !::Children[ nClickCol ]:AllowDrag
                  RETURN NIL
               ENDIF

               ::ReleaseCapture()

               ::__SelLeft := xPos - ::__SelWidth

               ::__lMoveMouseDown := .T.
               ::__hDragImageList := ::Children[ nClickCol ]:CreateDragImage( ::__SelWidth - ::Children[nClickCol]:Width )
               ImageListBeginDrag( ::__hDragImageList, 0, 0, 0 )
               ImageListDragEnter( ::hWnd, ::__SelWidth-::Children[nClickCol]:Width+1, IIF( !EMPTY( ::xText ), ::__nCaptionHeight, 0 )+1 )
            ENDIF

         ENDIF
         RETURN NIL
      ENDIF
   ENDIF

   IF LEN( ::__DisplayArray ) == 0 .OR. ::__DisplayArray[1] == NIL
      RETURN NIL
   ENDIF

   IF nClickRow > LEN( ::__DisplayArray ) .OR. xPos > ::ClientWidth .OR. xPos > ::__DataWidth
      ::SetFocus()
      RETURN NIL
   ENDIF

   nClickCol := 1
   FOR n := 1 TO LEN( ::Children )
       IF ::Children[n]:__nLeft <= xPos-::__HorzScrolled
          nClickCol := n
       ENDIF
   NEXT

   IF ::AllowDragRecords .AND. ( nClickRow == ::RowPos .OR. ::SimpleRowDrag ) .AND. ( nClickCol == ::ColPos .OR. ::FullRowSelect )
      lRes := ExecuteEvent( "OnBeginDragging", Self, nClickCol )
      DEFAULT lRes TO .T.
      IF lRes
         ::__nDragRec := ::__DisplayArray[ nClickRow ][2]
      ENDIF
   ENDIF

   nCol := ::ColPos
   nRow := ::RowPos

   ::__ResetControl()

   IF nClickRow != ::RowPos
      lRes := ::OnRowChanging()
      DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
      IF ValType( lRes ) != "L"
         lRes := NIL
      ENDIF
      DEFAULT lRes TO .T.
      IF !lRes
         RETURN NIL
      ENDIF
   ENDIF
   IF nClickCol != ::ColPos
      lRes := ::OnColChanging()
      DEFAULT lRes TO ExecuteEvent( "OnColChanging", Self )
      IF ValType( lRes ) != "L"
         lRes := NIL
      ENDIF
      DEFAULT lRes TO .T.
      IF !lRes
         RETURN NIL
      ENDIF
   ENDIF

   ::RowPos := ASCAN( ::__DisplayArray, {|a|a[2]==::DataSource:Recno()} )

   nRow := ::RowPos
   IF nClickCol == -1
      nClickCol := ::ColCount
   ENDIF

   IF nClickRow > ::RowCountUsable
      nClickRow := ::RowCountUsable
      lLineChange := TRUE
   ENDIF

   ::__GoToRec( ::__DisplayArray[ nClickRow ][2] )

   lSameRow := nClickRow == ::RowPos

   ::RowPos := nClickRow

   IF nCol > 0
      ::ColPos := nClickCol
      IF !::FullRowSelect
         IF nClickCol > nCol
            IF !::ArrowRight(.F.)
               RETURN NIL
            ENDIF
            ::OnColChanged()
            ExecuteEvent( "OnColChanged", Self )
          ELSEIF nClickCol < nCol
            IF !::ArrowLeft(.F.)
               RETURN NIL
            ENDIF
            ::OnColChanged()
            ExecuteEvent( "OnColChanged", Self )
         ENDIF
      ENDIF
   ENDIF

   IF nRow > 0
      ::__DisplayData( nRow, , nRow, )
   ENDIF
   IF lLineChange
      ::OnKeyDown( VK_DOWN )
      ::RowPos := ::RowCountUsable
   ENDIF
   IF ::hWnd != GetFocus()
      ::SetFocus()
   ENDIF
   IF ::ColPos == 0
      RETURN .F.
   ENDIF
   ::OnClick( ::ColPos, ::RowPos )
   IF nRow != ::RowPos
      ::__DisplayData( ::RowPos, , ::RowPos,  )
      IF ::bRowChanged != NIL
         EVAL( ::bRowChanged, Self )
      ENDIF
      ::OnRowChanged()
      ExecuteEvent( "OnRowChanged", Self )
   ENDIF
   ExecuteEvent( "OnClick", Self )

   IF ::MultipleSelection
      IF IsKeyDown( VK_CONTROL )
         ::ToggleSelection()
      ELSE
         ::DeselectAll()
      ENDIF
   ENDIF

   ::__DisplayData( ::RowPos, , ::RowPos,  )

   ::__lMouseDown := .T.
   IF ::Children[ ::ColPos ]:Representation == CREP_BUTTON
      ::__lMouseDown := lSameRow
   ENDIF
   ::__DisplayData( nClickRow, , nClickRow,  )

   IF ::Children[ ::ColPos ]:Representation == CREP_BUTTON
      IF ::__lMouseDown .AND. ::Children[ ::ColPos ]:ButtonMenu != NIL
         IF ::__MenuReturn > 0
            pt := (struct POINT)
            pt:x := ::Children[::ColPos]:aSelRect[1]
            pt:y := ::Children[::ColPos]:aSelRect[4]
            ClientToScreen( ::hWnd, @pt )
            ::Children[ ::ColPos ]:ButtonMenu:Show( pt:x, pt:y )
            TRY
               ::__MenuReturn := ::Children[ ::ColPos ]:ButtonMenu:Menu:ItemID
            CATCH
               ::__MenuReturn := 1
            END
          ELSE
            ::__MenuReturn := 1
         ENDIF
         ::__lMouseDown := .F.
         ::__DisplayData( ::RowPos, , ::RowPos,  )
       ELSE
         ::__MenuReturn := 1
      ENDIF
   ENDIF
   ::__Edit( 1, xPos, yPos, GRID_LCLICK )
RETURN NIL

//----------------------------------------------------------------------------------
/*
   Multiselection must check for shift and control keys, The selection WILL DEPEND on the programmer
   wich will have to save a flag in the DataBase / Array for DataGrid to know what to HighlightColor as
   selected
*/
//----------------------------------------------------------------------------------
METHOD __OnParentSysCommand()
   IF ::Parent:wParam == SC_CLOSE
      ::KillTimer(15)
   ENDIF
   IF ::__CurControl != NIL
      IF ::__CurControl:Validating
         RETURN 0
      ENDIF
      ::__CurControl:IsValid := FALSE
      ::__CurControl:Destroy()
      ::__CurControl:=NIL
   ENDIF
   IF ::Parent:wParam != SC_SIZE
      ::__DisplayData( ::RowPos,, ::RowPos )
   ENDIF
RETURN NIL

METHOD OnKillFocus() CLASS DataGrid
   ::Super:OnKillFocus()
   ::__lMouseDown := .F.
   IF ::__CurControl != NIL .AND. GetFocus()!=::__CurControl:hWnd
      IF ::__CurControl:Validating
         RETURN 0
      ENDIF
      ::__CurControl:IsValid := FALSE
      ::__CurControl:Destroy()
      ::__CurControl:=NIL
   ENDIF
   ::__DisplayData( ::RowPos,, ::RowPos )
RETURN NIL

//----------------------------------------------------------------------------------

METHOD OnSetFocus( lRedraw ) CLASS DataGrid
   LOCAL nCol := ::ColPos, nLen := LEN( ::Children )

   IF nLen > 0 .AND. nCol > 0
      WHILE ::Children[nCol]:Locked
         nCol++
         IF nCol > nLen
            nCol := 0
            EXIT
         ENDIF
      ENDDO
      ::ColPos := nCol
   ENDIF

   IF VALTYPE( lRedraw ) == "L" //.OR. ::Parent:Modal
      ::__DisplayData()
    ELSE
      ::__DisplayData( ::RowPos,, ::RowPos )
   ENDIF
   ::Super:OnSetFocus()
RETURN NIL

//----------------------------------------------------------------------------------

METHOD __ResetRecordPos( lRefresh ) CLASS DataGrid
   LOCAL nRec, n, nColumns

   DEFAULT lRefresh TO .T.

   ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )

   TRY
      IF lRefresh
         ::__GoToRec( ::DataSource:Recno() )
      ENDIF
      ::RowPos := ASCAN( ::__DisplayArray, {|a|a[2]==::DataSource:Recno()} )

      IF ::RowPos == 0 .AND. ::RowCount > 0
         ::RowPos := 1
         nColumns := LEN( ::Children )
         nRec     := ::DataSource:Recno()
         IF nRec > ::__DisplayArray[ ::RowCountUsable ][2]
            ::RowPos := ::RowCountUsable
            ::__SkipRecords( -(::RowCountUsable-1) )
         ENDIF

         ::__DisplayArray := {}
         FOR n := 1 TO ::RowCountVisible
             AADD( ::__DisplayArray,{ ARRAY( nColumns ), ::DataSource:Recno(), ::DataSource:Deleted(), ::ItemHeight } )
             ::__FillRow( n )
             ::__SkipRecords( 1 )
             IF ::HitBottom
                EVAL( ::__bGoBottom )
                EXIT
             ENDIF
         NEXT
         ::__GoToRec( nRec )
         ::__DisplayData()
      ENDIF
   CATCH
   END
RETURN Self

//----------------------------------------------------------------------------------

METHOD OnKeyDown( nwParam, nlParam ) CLASS DataGrid
   LOCAL nRec, nKey, h, nCount, lShift, x, cSearch, lVUpdate := FALSE

   IF ( x := ::Super:OnKeyDown( nwParam, nlParam ) ) != NIL .OR. ::DataSource == NIL
      IF ::DataSource == NIL .AND. nwParam == VK_TAB
         lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
         IF ( h := GetNextDlgTabItem( ::Form:hWnd , ::hWnd, lShift ) ) # 0
            SetFocus(h)
            PostMessage( ::Form:hWnd, WM_NEXTDLGCTL, h, MAKELPARAM( 1, 0 ) )
            RETURN 0
         ENDIF
      ENDIF
      RETURN x
   ENDIF
   ::__ResetControl()

   nCount := 1//LOWORD( nlParam )

   ::AutoUpdate := 0

   DO CASE
      CASE nwParam==VK_INSERT
           ::ResetSearch()
           ::AutoUpdate := ::__nUpdtTimer
           Set( _SET_INSERT, ! Set( _SET_INSERT ) )
           RETURN 0

      CASE nwParam==VK_TAB
           ::ResetSearch()
           lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
           IF ( h := GetNextDlgTabItem( ::Form:hWnd , ::hWnd, lShift ) ) # 0
              SetFocus(h)
              PostMessage( ::Form:hWnd, WM_NEXTDLGCTL, h, MAKELPARAM( 1, 0 ) )
              ::AutoUpdate := ::__nUpdtTimer
              RETURN 0
           ENDIF

      CASE nwParam==VK_UP
           ::ResetSearch()
           nKey := GRID_UP
           ::Up( nCount )
           ::CurPos--
           ::CurPos := MAX( 1, ::CurPos )
           lVUpdate := .T.

      CASE nwParam==VK_DOWN
           ::ResetSearch()
           nKey := GRID_DOWN
           ::Down( nCount )
           ::CurPos++
           lVUpdate := .T.

      CASE nwParam == VK_NEXT
           ::ResetSearch()
           ::PageDown( nCount )
           ::CurPos += ::RowCountUsable
           lVUpdate := .T.

      CASE nwParam == VK_PRIOR
           ::ResetSearch()
           ::PageUp( nCount )
           ::CurPos -= ::RowCountUsable
           lVUpdate := .T.

      CASE nwParam == VK_END
           ::ResetSearch()
           ::End( .F. )
           ::CurPos := ::GetRecordCount()
           lVUpdate := .T.

      CASE nwParam == VK_HOME
           ::ResetSearch()
           ::Home()
           ::CurPos := 1
           lVUpdate := .T.

      CASE nwParam == VK_LEFT
           ::ResetSearch()
           nKey := GRID_LEFT
           ::ArrowLeft()

      CASE nwParam == VK_RIGHT
           ::ResetSearch()
           nKey := GRID_RIGHT
           ::ArrowRight()

      CASE nwParam == VK_SPACE
           nKey := GRID_SPACE

           IF ! EMPTY( ::__cSearch )
              ::KillTimer( 10 )
              ::__cSearch += CHR( nwParam )

              nRec    := ::DataSource:recno()
              cSearch := ""
              IF ::bSearchKey != NIL
                 cSearch := EVAL( ::bSearchKey, Self )
              ENDIF
              IF ::DataSource:Seek( cSearch + ::__cSearch )
                 ::Update()
                 ::OnRowChanged()
                 ExecuteEvent( "OnRowChanged", Self )
               ELSE
                 ::DataSource:Goto( nRec )
                 ExecuteEvent( "OnSearchNotFound", Self )
              ENDIF
              ::SetTimer( 10, 2000 )

            ELSEIF ::MultipleSelection
              IF IsKeyDown( VK_CONTROL )
                 ::ToggleSelection()
              ELSE
                 ::DeselectAll()
              ENDIF

           ENDIF

      OTHERWISE
           IF ! ( nwParam IN { VK_RETURN, VK_DELETE, VK_TAB, VK_SHIFT } ) .AND. ! Empty( ::CurTag )
              IF EMPTY( ::__cSearch )
                 // Start auto clearing timer
                 ::SetTimer( 10, 2000 )
              ENDIF
              IF nwParam == VK_BACK
                 IF ! EMPTY( ::__cSearch )
                    ::__cSearch := LEFT( ::__cSearch, LEN( ::__cSearch )-1 )
                 ENDIF
               ELSE
                 ::KillTimer( 10 )
                 ::__cSearch += CHR( nwParam )
              ENDIF
              IF EMPTY( ::__cSearch )
                 ::KillTimer( 10 )
               ELSE
                 nRec    := ::DataSource:recno()
                 cSearch := ""
                 IF ::bSearchKey != NIL
                    cSearch := EVAL( ::bSearchKey, Self )
                 ENDIF
                 IF ::DataSource:Seek( cSearch + ::__cSearch )
                    ::Update()
                    ::OnRowChanged()
                    ExecuteEvent( "OnRowChanged", Self )
                  ELSE
                    ::DataSource:Goto( nRec )
                    ExecuteEvent( "OnSearchNotFound", Self )
                 ENDIF
                 ::SetTimer( 10, 2000 )
              ENDIF
           ELSE
              ::ResetSearch()
           ENDIF
   ENDCASE
   IF lVUpdate
      ::__VertScrolled := ::Record - ::RowPos + 1
      ::__UpdateVScrollBar()
   ENDIF
   IF nKey != NIL .AND. nKey != GRID_DOWN
      ::__Edit(0, 0, 0, nKey, nwParam )
   ENDIF
   ::AutoUpdate := ::__nUpdtTimer
RETURN NIL

//----------------------------------------------------------------------------------
METHOD OnPaint() CLASS DataGrid
   LOCAL hMemBitmap, hOldBitmap, lPaint, hMemDC
   LOCAL hDC := ::BeginPaint()

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::xWidth, ::xHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   lPaint := ::__DisplayData(,,,, hMemDC )

   IF lPaint
      BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )
   ENDIF
   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )

   ::EndPaint()
RETURN 0

//----------------------------------------------------------------------------------
METHOD __DisplayData( nRow, nCol, nRowEnd, nColEnd, hMemDC, lHover, lPressed, lHot, lHeader ) CLASS DataGrid
   LOCAL n, i, cData, x, y, nY, nRec, nRecno, lHide, aText, lSelected, nHScroll, iRight, iLeft, zLeft
   LOCAL nLeft, nTop, nRight, nBottom, hOldFont, hOldPen, nWImg, nHImg, nInd, nAlign, aAlign, aGrid, lFreeze, nHeaderRight
   LOCAL xLeft, nStatus, lDeleted, nPos, lDC, lData
   LOCAL nFocRow, aData, lDrawControl, nCtrl, nRep, aRect, lEnabled := ::IsWindowEnabled()
   LOCAL lHighLight, lBorder, hBrush, nLine, nRecPos := 0, hPen, nImgX
   LOCAL nForeColor, nBackColor, lFocus := GetFocus() == ::hWnd, /*lFocusRect,*/ lShadow
   LOCAL nBackGrid, nForeGrid, nOffset, nImgAlign, xRight, aColors//, nSel

   IF LEN( ::Children ) == 0 .OR. ::hWnd == NIL .OR. !IsWindow( ::hWnd ) .OR. ::hWnd == 0
      RETURN .F.
   ENDIF
   DEFAULT lHeader TO .F.
   lFreeze := ::FreezeColumn > 0
   IF ::RowPos < 1
      ::RowPos := 1
   ENDIF
   IF !EMPTY( ::__DisplayArray ) .AND. ::DataSource != NIL .AND. ::DataSource:IsOpen
      nRecno   := ::DataSource:Recno()
    ELSE
      nRecno := 0
   ENDIF
   lDC := hMemDC != NIL
   DEFAULT hMemDC TO GetDC( ::hWnd )
   DEFAULT lHover TO .F.

   hOldFont := SelectObject( hMemDC, ::Font:Handle )
   hOldPen  := SelectObject( hMemDC, ::__LinePen )

   DEFAULT nRow    TO 1
   DEFAULT nRowEnd TO ::RowCountVisible
   DEFAULT nCol    TO 1
   IF nRow < 1
      nRow := 1
   ENDIF
   IF nCol < 1
      nCol := 1
   ENDIF

   DEFAULT nColEnd TO LEN( ::Children )

   nHScroll := ::__HorzScrolled

   xLeft := nHScroll
   FOR n := 1 TO nCol-1
       xLeft += ::Children[n]:Width
   NEXT

   iRight := 0
   FOR n := 1 TO MIN( ::FreezeColumn, LEN( ::Children ) )
       iRight += ::Children[n]:xWidth
   NEXT

   SetBkMode( hMemDC, TRANSPARENT )
   DEFAULT nRowEnd TO 0

   IF nRowEnd == 0 .AND. ::ShowHeaders
      nRowEnd := 1
   ENDIF

   nBackGrid := ::BackColor
   DEFAULT nBackGrid TO ::__SysBackColor

   nForeGrid := ::ForeColor
   DEFAULT nForeGrid TO ::__SysForeColor

   FOR nLine := nRow TO nRowEnd
       IF nLine <= LEN( ::__DisplayArray ) .AND. ::__DisplayArray[nLine] == NIL
          LOOP
       ENDIF
       nLeft   := xLeft
       iLeft   := 0

       FOR n := 1 TO nCol-1
           iLeft += ::Children[n]:Width
       NEXT

       nTop    := ::__GetHeaderHeight() + ( ( nLine-1 ) * ::ItemHeight )
       nBottom := nTop + ::ItemHeight - 1
       lSelected := .F.
       nFocRow := 99999

       lData := nLine <= LEN( ::__DisplayArray )

       IF lData
          nRec := ::__DisplayArray[nLine][2]
       ENDIF

       IF ::Striping .AND. lData
          ::DataSource:Goto( nRec )
          nRecPos := ::DataSource:OrdKeyNo()
          ::DataSource:Goto( nRecno )
       ENDIF

       IF lData .AND. ::ShowSelection
          lSelected := ( ! ::MultipleSelection .AND. nRec == nRecno ) .OR. ( ::MultipleSelection .AND. ASCAN( ::aSelected, nRec ) > 0 )
       ENDIF

       IF lData .AND. nRec == ::__nDragRec .AND. ::__hDragRecImage != NIL
          _FillRect( hMemDC, { nLeft, nTop, ::Width, nBottom }, ::__hDragBrush )
          LOOP
       ENDIF

       nPos := nLine
       IF ::__nDragPos > -1
          IF nLine < ::__nDragPos
             IF ::__nDragPos > ::RowPos
                nPos := ::__nDragPos
             ENDIF
           ELSE
             IF ::__nDragPos <= ::RowPos
                nPos := ::__nDragPos
             ENDIF
          ENDIF
          lSelected := .F.
       ENDIF

       lDeleted   := IIF( lData, ::__DisplayArray[nPos][3], .F. )

       FOR i := nCol TO nColEnd
           IF nLeft > ::ClientWidth .OR. ( lData .AND. LEN(::__DisplayArray[nLine][1])<i ) // avoid painting non-visible columns
              EXIT
           ENDIF
           IF LEN( ::Children ) >= i .AND. ::Children[ i ]:Visible .AND. ! ::Children[ i ]:__lHidden

              lHighLight := ::ShowSelection .AND. ( lFocus .OR. (::MultipleSelection .AND. lSelected .AND. LEN( ::aSelected ) > 0 ) ) .AND. lData .AND. lSelected .AND. ( ( ( ! ::FullRowSelect .AND. ::ColPos == i ) .OR. (::MultipleSelection .AND. lSelected .AND. LEN( ::aSelected ) > 0 ) ) .OR. ::FullRowSelect )

              lShadow    := ::Enabled .AND. /*! ::FullRowSelect .AND.*/ lData .AND. ::ShowSelection .AND. ::ShadowRow .AND. ( nRec == nRecno .AND. ( ::ColPos <> i .OR. ! lFocus ) )

              cData  := IIF( lData, ::__DisplayArray[nPos][1][i][ 1], " " )
              nInd   := IIF( lData, ::__DisplayArray[nPos][1][i][ 2], 0 )
              nWImg  := IIF( lData, IIF( ::ImageList != NIL, ::__DisplayArray[nPos][1][i][ 3], 2 ), 0 )
              nAlign := IIF( lData, ::__DisplayArray[nPos][1][i][ 4], ALIGN_LEFT )
              nHImg  := IIF( lData, ::__DisplayArray[nPos][1][i][ 5], 0 )

              nBackColor := IIF( lHover, ::__HoverBackColor, IIF( lData .AND. ::__DisplayArray[nPos][1][i][ 6] != NIL, ::__DisplayArray[nPos][1][i][ 6], IIF( ::Children[i]:BackColor != NIL, ::Children[i]:BackColor, nBackGrid )) )
              IF VALTYPE( nBackColor ) == "B"
                 nBackColor := EVAL( nBackColor, ::Children[i] )
              ENDIF
              DEFAULT nBackColor TO nBackGrid

              IF lData .AND. ::Striping .AND. ( nRecPos / 2 ) > Int( nRecPos / 2 )
                 nBackColor := DarkenColor( nBackColor, 25 )
              ENDIF

              nForeColor := IIF( lHover, nForeGrid, IIF( lData .AND. ::__DisplayArray[nPos][1][i][ 7] != NIL, ::__DisplayArray[nPos][1][i][ 7], IIF( ::Children[i]:ForeColor != NIL, ::Children[i]:ForeColor, nForeGrid )) )
              IF VALTYPE( nForeColor ) == "B"
                 nForeColor := EVAL( nForeColor, ::Children[i] )
              ENDIF
              DEFAULT nForeColor TO nForeGrid

              nStatus    := IIF( lData, ::__DisplayArray[nPos][1][i][ 8], 0 )
              nRep       := IIF( lData, ::Children[i]:Representation, 1 )

              IF lData
                 hOldFont := SelectObject( hMemDC, ::__DisplayArray[nPos][1][i][9] )
              ENDIF
              nImgAlign  := IIF( lData, ::__DisplayArray[nPos][1][i][10], ALIGN_DEFAULT )

              zLeft := nLeft
              IF lFreeze .AND. i <= ::FreezeColumn
                 zLeft := iLeft
              ENDIF
              nRight := zLeft + ::Children[i]:xWidth

              IF lFreeze .AND. i > ::FreezeColumn
                 IF nRight < iRight
                    nLeft  += ::Children[i]:Width
                    //iLeft  += ::Children[i]:Width
                    LOOP
                 ENDIF
              ENDIF

              SWITCH VALTYPE( cData )
                 CASE "N"
                      DEFAULT nAlign TO ALIGN_RIGHT
                      cData := ALLTRIM( TRANSFORM( cData, ::Children[ i ]:Picture ) ) //ALLTRIM( STR( cData ) )
                      EXIT

                 CASE "D"
                      DEFAULT nAlign TO ALIGN_CENTER
                      cData := ALLTRIM(DTOC( cData ))
                      EXIT

                 CASE "L"
                      DEFAULT nAlign TO ALIGN_CENTER
                      cData := ALLTRIM(IIF( cData, "<True>", "<False>" ))
                      EXIT

                 CASE "C"
                      DEFAULT nAlign TO ALIGN_LEFT
                      cData := ALLTRIM( TRANSFORM( cData, ::Children[ i ]:Picture ) ) //ALLTRIM( cData )
                      EXIT

                 CASE "B"
                      DEFAULT nAlign TO ALIGN_CENTER
                      cData := "<block>"
                      EXIT

                 CASE "A"
                      DEFAULT nAlign TO ALIGN_LEFT
                      WHILE VALTYPE( cData ) == "A"
                         cData := cData[1]
                         DEFAULT cData TO ""
                      ENDDO
                      cData := ALLTRIM( TRANSFORM( cData, ::Children[ i ]:Picture ) ) //ALLTRIM( cData )
                      EXIT
              END
              DEFAULT cData TO ""
              IF VALTYPE( cData ) != "A"
                 aData := hb_aTokens( cData, CHR(13)+CHR(10) )
               ELSE
                 aData := cData
              ENDIF
              IF EMPTY( aData )
                 AADD( aData, "" )
              ENDIF

              aColors := {nForeColor,nBackColor}

              IF ! lEnabled
                 nForeColor := ::System:Color:Gray
               ELSEIF ::__hDragRecImage == NIL
                 IF lHighLight
                    nBackColor := ::HighlightColor
                    nForeColor := ::HighlightTextColor
                  ELSEIF lShadow .AND. ! lHover
                    nBackColor := ::__InactiveHighlight
                 ENDIF
              ENDIF

              SetBkColor( hMemDC, nBackColor )
              SetTextColor( hMemDC, nForeColor )

              nHeaderRight := nRight-1
              aText := { zLeft, nTop, nRight-IIF( ( lSelected .AND. ::FullRowSelect .AND. i<nColEnd ) .OR. !::xShowGrid, 0, 1 ), nBottom+IIF(::xShowGrid,0,1) }

              IF lFreeze .AND. i > ::FreezeColumn .AND. zLeft < iRight
                 aText[1] := iRight
                 aText[3] += iRight - zLeft
              ENDIF

              // Header ---------------------------------------------------------
              IF nLine == 1 .AND. ::ShowHeaders
                 ::Children[i]:DrawHeader( hMemDC, aText[1], nHeaderRight, x, lPressed, lHot, zLeft, nImgAlign, aText[3] )
                 IF lHeader
                    EXIT
                 ENDIF
              ENDIF
              //-----------------------------------------------------------------

              aAlign := _GetTextExtentPoint32( hMemDC, ALLTRIM( aData[1] ) )
              DEFAULT aAlign TO {1,1}
              DEFAULT nWImg TO 0

              y := nTop + ((nBottom-nTop)/(LEN( aData )+1)) - (aAlign[2]/2)

              lHide := ::Children[i]:ControlHide
              IF VALTYPE( lHide ) == "B"
                 lHide := EVAL( ::Children[i]:ControlHide, Self, nRec )
              ENDIF

              nCtrl := 0
              lDrawControl := .F.

              IF ::Children[i]:SelOnlyRep .AND. !lSelected
                 nRep := 1
              ENDIF
              ::Children[i]:aSelRect := { zLeft, nTop, nRight, nBottom }
              IF ( lSelected .OR. ::Children[i]:ShowControls ) .AND. ::Children[i]:Control != NIL
                 DEFAULT ::Children[i]:ControlObject TO EVAL( ::Children[i]:Control, Self, nRec, i )
                 IF !lHide .AND. ::Children[i]:ControlObject != NIL .AND. RIGHT( ::Children[i]:ControlObject:ClsName, 4 ) != "Edit" .AND. VALTYPE( nStatus ) != "B"
                    IF ::Children[i]:ControlAlign == DT_LEFT
                       nCtrl += ::Children[i]:ControlWidth + 1
                       x+=::Children[i]:ControlWidth+1
                    ENDIF
                    lDrawControl := .T.
                 ENDIF
              ENDIF

              IF nRep > 1
                 _ExtTextOut( hMemDC, aText[1], y, ( ETO_CLIPPED | ETO_OPAQUE ), {aText[1],aText[2],nRight,aText[4]}, "")

               ELSE

                 nOffset := NIL
                 IF nWImg > 0 .AND. ::ImageList != NIL
                    nY := nTop + ( (nBottom-nTop-nHImg)/2) + 1

                    IF nImgAlign == ALIGN_LEFT
                       nImgX := zLeft+nCtrl+1
                     ELSEIF nImgAlign == ALIGN_CENTER
                       nImgX := Int( zLeft + (((aText[3]-aText[1])-nWImg)/2) )
                     ELSEIF nImgAlign == ALIGN_RIGHT
                       nImgX := zLeft + (aText[3]-aText[1])-nWImg
                    ENDIF

                    nOffset := IIF( nImgX-aText[1] > 0, 0, aText[1]-nImgX )
                 ENDIF

                 ::__DrawMultiText( hMemDC, aText, aData, nRight, zLeft, nWImg, nAlign, y, .F., nImgAlign, lHighLight .OR. lShadow .OR. lHover )

                 IF nOffset != NIL
                    ::ImageList:DrawIndirect( hMemDC, nInd, nImgX+nOffset, nY, nOffset,, ! ::xEnabled )
                 ENDIF

              ENDIF

              // Draw Grid
              IF ::xShowGrid
                 aGrid := { {zLeft,nBottom}, {nRight,nBottom}, {nRight-1,nBottom}, {nRight-1,nTop-1} }
                 _PolyLine( hMemDC, aGrid )
              ENDIF

              IF ::ShowSelectionBorder .AND. ( lHighLight .OR. ( ! ::ShowSelection .AND. nRec == nRecno .AND. ( i == ::ColPos .OR. ::FullRowSelect ) ) )
                 nFocRow := nPos
              ENDIF

              IF nRep == 1

                 IF lDrawControl
                    aRect := ::Children[i]:ControlObject:DrawFrame( hMemDC, {zLeft+IIF(::Children[i]:ControlAlign==DT_LEFT,1,0),nTop+1,nRight-2,MAX(nBottom-1,nTop+::ItemHeight)}, ::Children[i]:ControlAlign, ::Children[i]:ControlWidth, ::Children[i]:ControlHeight, nStatus )
                    IF lSelected
                       ::__CheckPos := aRect
                    ENDIF
                 ENDIF

               ELSEIF nRep > 1

                 x := zLeft + ((nRight-zLeft+nWImg)/2) - (aAlign[1]/2)

                 IF nRep == 3
                    IF nAlign == ALIGN_LEFT
                       aRect  := { aText[1], aText[2], aText[1]+17, aText[4] }
                       xRight := zLeft + 17
                     ELSEIF nAlign == ALIGN_RIGHT
                       aRect  := { aText[3]-17, aText[2], aText[3], aText[4] }
                       xRight := nRight
                     ELSEIF nAlign == ALIGN_CENTER
                       aRect  := aText
                       xRight := nRight
                    ENDIF
                  ELSE
                    aRect := aText
                 ENDIF

                 ::__DrawRepresentation( hMemDC, nRep, aRect, aData[1], nBackColor, aColors[1], nAlign, zLeft, xRight, x, y, aAlign, ::__DisplayArray[nPos][1][i][ 1], i, nRec == nRecno )

              ENDIF

              lBorder := lData .AND. ::ShowSelectionBorder .AND. nRec == nRecno .AND. i == ::ColPos .AND. ! ::FullRowSelect

              IF lBorder .AND. lFocus .AND. ::__hDragRecImage == NIL
                 IF ::__SelBorderPen != NIL
                    hPen := SelectObject( hMemDC, ::__SelBorderPen )
                    hBrush  := SelectObject( hMemDC, GetStockObject( NULL_BRUSH ) )

                    Rectangle( hMemDC, aText[1], nTop, nRight-IIF( ( lSelected .AND. ::FullRowSelect .AND. i<nColEnd ) .OR. !::xShowGrid, 0, 1 ), nBottom+IIF(::xShowGrid,0,1) )
                    SelectObject( hMemDC, hBrush )
                    SelectObject( hMemDC, hPen )
                  ELSE
                    _DrawFocusRect( hMemDC, {aText[1], nTop, nRight-IIF( ( lSelected .AND. ::FullRowSelect .AND. i<nColEnd ) .OR. !::xShowGrid, 0, 1 ), nBottom+IIF(::xShowGrid,0,1)} )
                 ENDIF
              ENDIF

              nLeft += ::Children[i]:Width
              IF lFreeze .AND. i <= ::FreezeColumn
                 iLeft  += ::Children[i]:Width
              ENDIF
           ENDIF
       NEXT

       IF ::FullRowSelect .AND. ( nLine == nFocRow .OR. lHover .OR. ( ::MultipleSelection .AND. nRec == nRecno ) )

          TRY
             nLeft   := nHScroll + ::Children[nCol]:__nLeft
             nTop    := ::__GetHeaderHeight() + ( ( nLine-1 ) * ::ItemHeight ) + IIF( ::xShowGrid, 0, 1 )
             nBottom := nTop + ::ItemHeight - 1

             IF ::ShowSelectionBorder .OR. lHover
                IF ::__SelBorderPen != NIL .OR. ( lHover .AND. ::__HoverBorderPen != NIL .AND. ! lHighLight )
                   hPen := SelectObject( hMemDC, IIF( lHover .AND. nLine <> nFocRow, ::__HoverBorderPen, ::__SelBorderPen ) )
                   hBrush := SelectObject( hMemDC, GetStockObject( NULL_BRUSH ) )
                   Rectangle( hMemDC, nLeft, nTop-IIF(::xShowGrid,0,1), nRight-IIF( ::xShowGrid, 1, 0 ), nBottom )
                   SelectObject( hMemDC, hBrush )
                   SelectObject( hMemDC, hPen )
                 ELSEIF nRec == nRecno .AND. lFocus

                   _DrawFocusRect( hMemDC, { nLeft, nTop-IIF(::xShowGrid,0,1), nRight-IIF( ::xShowGrid, 1, 0 ), nBottom } )
                ENDIF
             ENDIF
           CATCH
          END
       ENDIF

   NEXT
   IF ::__DataWidth - ABS( nHScroll ) < ::ClientWidth
      x := ::__DataWidth - ABS( nHScroll )
      y := 0

      SetBkColor( hMemDC, nBackGrid )
      SetTextColor( hMemDC, nForeGrid )

      _ExtTextOut( hMemDC, x, y, (ETO_CLIPPED | ETO_OPAQUE), { x, y, ::ClientWidth, ::ClientHeight }," ")
   ENDIF

   SelectObject( hMemDC, hOldFont )
   SelectObject( hMemDC, hOldPen )

   IF ! lDC
      ReleaseDC( ::hWnd, hMemDC )
   ENDIF

RETURN .T.

METHOD __DrawRepresentation( hDC, nRep, aRect, cText, nBkCol, nTxCol, nAlign, zLeft, nRight, x, y, aAlign, xVal, i, lRec ) CLASS DataGrid
   LOCAL nWidth, aClip, hBrush, nStatus, nFlags, lXP

   lXP    := ::Application:IsThemedXP .AND. ::Theming .AND. ::System:hButtonTheme != NIL
   nFlags := DFCS_BUTTONCHECK

   IF nRep == 2
      nWidth := ( ::Children[i]:xWidth * VAL( cText ) ) / 100

      hBrush := CreateSolidBrush( nTxCol )
      _FillRect( hDC, { aRect[1]+1, aRect[2]+1, aRect[1]+Max( 1, nWidth-1-(aRect[1]-zLeft) ), aRect[4]-1 }, hBrush )
      DeleteObject( hBrush )

      SetTextColor(hDC, nTxCol )
      SetBkColor(hDC, nBkCol )

      x := zLeft + ((::Children[i]:xWidth)/2) - (aAlign[1]/2)

      _ExtTextOut(hDC, x, y, ETO_CLIPPED, {aRect[1],aRect[2],aRect[3],aRect[4]}, cText)

      SetTextColor(hDC, nBkCol )
      SetBkColor(hDC, nTxCol )
      aClip := { zLeft, aRect[2], zLeft+nWidth, aRect[4] }
      _ExtTextOut(hDC, x, y, ETO_CLIPPED, {aRect[1],aRect[2],aRect[1]+Max( 1, nWidth-(aRect[1]-zLeft) ),aRect[4]}, cText)

    ELSEIF nRep == 3
      IF VALTYPE( xVal ) == "L"
         IF xVal
            xVal := BST_CHECKED
          ELSE
            xVal := BST_UNCHECKED
         ENDIF
       ELSEIF VALTYPE( xVal ) != "N"
         xVal := BST_UNCHECKED
      ENDIF
      nStatus := xVal
      DO CASE
         CASE xVal == BST_UNCHECKED
              nStatus := 0
              IF lXP
                 nStatus := IIF( ::__lHot, CBS_UNCHECKEDHOT, CBS_UNCHECKEDNORMAL )
              ENDIF

         CASE xVal == BST_CHECKED
              nStatus := DFCS_CHECKED
              IF lXP
                 nStatus := IIF( ::__lHot, CBS_CHECKEDHOT, CBS_CHECKEDNORMAL )
              ENDIF

         CASE xVal == BST_INDETERMINATE
              nStatus := (DFCS_BUTTON3STATE | DFCS_CHECKED)
              IF lXP
                 nStatus := IIF( ::__lHot, CBS_MIXEDHOT, CBS_MIXEDNORMAL )
              ENDIF

      ENDCASE

      nFlags := (nFlags | nStatus)

      IF lXP
         aClip  := { aRect[1], aRect[2], aRect[3], aRect[4] }
         IF nAlign == ALIGN_RIGHT
            aRect[1] := nRight-17
            aRect[3] := nRight
          ELSE
            aRect[1] := zLeft
            aRect[3] := nRight
         ENDIF
         DrawThemeBackground( ::System:hButtonTheme, hDC, BP_CHECKBOX, nStatus, aRect, aClip )
       ELSE
         aRect[1] := aRect[1] + (( aRect[3]-aRect[1]-15 )/2)
         aRect[2] := aRect[2] + (( aRect[4]-aRect[2]-15 )/2)

         aRect[3] := aRect[1] + 15
         aRect[4] := aRect[2] + 15

         _DrawFrameControl( hDC, aRect, DFC_BUTTON, nFlags )
      ENDIF

    ELSEIF nRep == 4
      IF lXP
         nStatus := PBS_NORMAL
         IF ::__lMouseDown  .AND. i == ::ColPos
            nStatus := PBS_PRESSED
         ENDIF
         DrawThemeBackground( ::System:hButtonTheme, hDC, BP_PUSHBUTTON, nStatus, aRect )
       ELSE
         nStatus := DFCS_BUTTONPUSH
         IF ::__lMouseDown  .AND. i == ::ColPos
            nStatus := (nStatus | DFCS_PUSHED)
         ENDIF
         _DrawFrameControl( hDC, aRect, DFC_BUTTON, nStatus )
      ENDIF
      IF ::ShowSelectionBorder .AND. i == ::ColPos .AND. lRec
         _DrawFocusRect( hDC, {aRect[1]+3, aRect[2]+3, aRect[3]-3, aRect[4]-3} )
      ENDIF
      IF ! EMPTY( ::Children[i]:ButtonText )
         cText := ::Children[i]:ButtonText
      ENDIF
      _DrawText( hDC, cText, aRect, (DT_CENTER | DT_VCENTER | DT_SINGLELINE) )

   ENDIF

RETURN Self

//----------------------------------------------------------------------------------

METHOD __UpdateHeight() CLASS DataGrid
   LOCAL n, nRec, nHeight, nVisible, nUsable

   nHeight  := ::__GetHeaderHeight()
   nUsable  := 0
   nVisible := 0

   IF ::DataSource != NIL
      nRec  := ::DataSource:Recno()

      n := 1
      WHILE nHeight <= ::ClientHeight
          nHeight += IIF( VALTYPE( ::RowHeight ) == "B", EVAL( ::RowHeight, Self, EVAL(::__bRecNo) ), ::ItemHeight )
          IF nHeight <= ::ClientHeight
             nUsable ++
          ENDIF
          nVisible ++
          ::__SkipRecords( 1 )
          n++
      ENDDO

      ::__GoToRec( nRec )
   ENDIF
RETURN {nVisible, nUsable}

//----------------------------------------------------------------------------------
METHOD __Update( lDisplay, lFillData ) CLASS DataGrid
   LOCAL n, nRec
   DEFAULT lDisplay TO TRUE
   DEFAULT lFillData TO .T.

   IF ::DataSource == NIL .OR. ( VALTYPE( ::DataSource )=="O" .AND. !::DataSource:IsOpen )
      RETURN Self
   ENDIF

   ::__SetBlocks()
   ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )
   ::__DisplayArray  := {}
   nRec              := ::DataSource:Recno()
   DEFAULT nRec TO 1
   ::__SkipRecords( -(::RowPos-1) )

   IF ::HitTop
      EVAL( ::__bGoTop )
   ENDIF
   IF !::HitTop .AND. !::HitBottom
      FOR n := 1 TO ::RowCountVisible
          AADD( ::__DisplayArray,{ ARRAY( ::ColCount ), ::DataSource:Recno(), ::DataSource:Deleted() } )
          IF lFillData
             ::__FillRow( n )
          ENDIF
          ::__SkipRecords( 1 )
          IF ::HitBottom .AND. ::__bGoBottom != NIL
             EVAL( ::__bGoBottom )
             EXIT
          ENDIF
      NEXT
   ENDIF
   ::__GoToRec( nRec )

   IF lDisplay
      ::__DisplayData()
      IF ::__DataWidth < ::ClientWidth
         ::InvalidateRect( { ::__DataWidth, 0, ::ClientWidth, ::ClientHeight } )
      ENDIF
   ENDIF
   ::CurPos := ::Record
RETURN Self

//----------------------------------------------------------------------------------
METHOD Update() CLASS DataGrid
   LOCAL n, nRec, nUse, nIns, cTag, lDescending

   ::__GetDataWidth()

   IF ::DataSource == NIL .OR. ! ::DataSource:IsOpen
      ::__DisplayData()
      RETURN Self
   ENDIF

   IF EMPTY( ::__DisplayArray )
      ::__Update( .F. )
   ENDIF

   IF EMPTY( ::__DisplayArray )
      RETURN Self
   ENDIF
   ::__DataHeight   := ::ClientHeight - ::__GetHeaderHeight()

   ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )
   nUse := Int(  ::__DataHeight/::ItemHeight )
   IF ::DataSource:Eof()
      ::DataSource:GoBottom()
      IF ::DataSource:Eof()
         ::DataSource:GoTop()
      ENDIF
   ENDIF
   //--------------------------------------------------------------

   nRec     := ::DataSource:Recno()

   ::RowPos := MAX( 1, ASCAN( ::__DisplayArray, {|a| a[2] == nRec } ) )
   ::__VertScrolled := MAX( 1, ::Record - ::RowPos + 1 )

   IF ::RowPos > 0
      ::DataSource:Skip( -(::RowPos-1) )
      IF ::HitTop
         EVAL( ::__bGoTop )
      ENDIF
   ENDIF
   ::__DisplayArray := {}

   IF ::RowCountUsable > 0 .AND. ! ( ::DataSource:Bof() .AND. ::DataSource:Eof() )
      nIns := 0

      FOR n := 1 TO ::RowCountVisible
          AADD( ::__DisplayArray,{ ARRAY( ::ColCount ), ::DataSource:Recno(), ::DataSource:Deleted(), ::ItemHeight } )
          ::__FillRow( n )
          ::__SkipRecords( 1 )
          IF ::HitBottom .AND. ::__bGoBottom != NIL
             EVAL( ::__bGoBottom )
             nIns := nUse - n
             EXIT
          ENDIF
      NEXT

      IF nIns > 0
         ::__GoToRec( ::__DisplayArray[1][2] )
         FOR n := 1 TO nIns
            ::__SkipRecords( -1 )
            IF ::HitTop
               EXIT
            ENDIF
            AINS( ::__DisplayArray, 1, { ARRAY( ::ColCount ), ::DataSource:Recno(), ::DataSource:Deleted(), ::ItemHeight }, .T. )
            ::__FillRow( 1 )
            ::RowPos++
            ::__VertScrolled++
         NEXT
      ENDIF

   ENDIF

   ::__GoToRec( nRec )

   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )

   ::__UpdateVScrollBar(.T.)
   ::__UpdateHScrollBar( .T. )

   IF Empty( ::CurTag ) .AND. ::xDataSource != NIL
      cTag := ::DataSource:OrdSetFocus()
      IF cTag != NIL .AND. ( n := Ascan( ::Children, {|o| ! Empty(o:Tag) .AND. Upper(o:Tag) == Upper(cTag)} ) ) > 0
         ::CurTag := cTag
         ::__prevCol := n
         lDescending := ::DataSource:OrdDescend( cTag )
         ::Children[ n ]:__SetSortArrow( IIF( lDescending, 1, 2 ) )
         ::Children[ n ]:DrawHeader()
      ENDIF
   ENDIF

   ::__DisplayData()
RETURN Self

//----------------------------------------------------------------------------------

METHOD __ScrollUp( nScroll ) CLASS DataGrid
   LOCAL nRec, n, aScroll, aClip, nPos, nNew

   DEFAULT nScroll TO ::__VertScrolled + 1
   IF ::__VertScrolled + ::RowCountUsable <= ::GetRecordCount() //::DataSource:OrdKeyCount()

      nNew := nScroll - ::__VertScrolled
      ::__VertScrolled := nScroll
      ::__UpdateVScrollBar()

      nPos := ::__VertScrolled + ::RowCountVisible - nNew - 1
      nRec := ::DataSource:Recno()

      IF nNew > ::RowCountVisible
         nPos := nScroll - 1
         nNew := ::RowCountVisible
      ENDIF

      ::__SkipRecords( nPos - ::Record )

      FOR n := 1 TO nNew
          ADEL( ::__DisplayArray, 1 )
          ::__SkipRecords( 1 )
          IF ::HitBottom
             ::__VertScrolled -= ( nNew-n )
             ADEL( ::__DisplayArray, ::RowCountVisible, TRUE )
             EVAL( ::__bGoBottom )
             EXIT
          ENDIF

             // Retrieve next Record
          TRY
            ::__DisplayArray[ ::RowCountVisible ] := { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() }
            ::__FillRow( ::RowCountVisible )
           CATCH
            ::__DisplayArray[ ::RowCountUsable ] := { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() }
            ::__FillRow( ::RowCountUsable )
          END
      NEXT
      IF !::HitBottom .AND. n > 1
         n--
      ENDIF

      n := MIN( n, ::RowCountVisible-1 )

      // Reposition and display new __Records
      ::__GoToRec( nRec )

      IF ( nPos := ASCAN( ::__DisplayArray, {|a| a != NIL .AND. a[2] != NIL .AND. a[2]==nRec } ) ) > 0
         ::RowPos := nPos
      ENDIF

      IF ::Transparent .OR. ::IsCovered( ::__GetHeaderHeight() )
         ::__DisplayData()
       ELSE
         // Scroll up
         aScroll := { 0, ::__GetHeaderHeight(), ::ClientWidth, (::RowCountVisible*::ItemHeight) + ::__GetHeaderHeight() }
         aClip   := { 0, ::__GetHeaderHeight(), ::ClientWidth, (::RowCountVisible*::ItemHeight) + ::__GetHeaderHeight() }
         ::ScrollWindow( 0, -(::ItemHeight*n), aScroll, aClip )
         ::ValidateRect()
         ::__DisplayData( ::RowCountVisible-n,, ::RowCountVisible )
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------

METHOD __ScrollDown( nScroll ) CLASS DataGrid
   LOCAL nRec, n, aScroll, aClip, nPos, nNew

   DEFAULT nScroll TO ::__VertScrolled - 1

   IF ::__VertScrolled > 1

      nNew := ::__VertScrolled - nScroll

      ::__VertScrolled := nScroll
      ::__UpdateVScrollBar()

      nPos := ::__VertScrolled
      nRec := ::DataSource:Recno()

      ::__SkipRecords( nPos - ::Record )

      WHILE LEN( ::__DisplayArray ) < ::RowCountVisible
         AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )
      ENDDO

      FOR n := 1 TO MIN( ::RowCountVisible, nNew )
          AINS( ::__DisplayArray, n, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )
          ::__FillRow( n )
          ::__SkipRecords( 1 )
          IF ::Hittop
             EXIT
          ENDIF
      NEXT

      IF n > 1
         n--
      ENDIF

      // Reposition and display new __Records
      ::__GoToRec( nRec )

      IF ::Transparent .OR. ::IsCovered( ::__GetHeaderHeight() )
         ::__DisplayData()
       ELSE
         // Scroll up
         aScroll := { 0, ::__GetHeaderHeight(), ::ClientWidth, (::RowCountVisible*::ItemHeight) + ::__GetHeaderHeight() }
         aClip   := { 0, ::__GetHeaderHeight(), ::ClientWidth, (::RowCountVisible*::ItemHeight) + ::__GetHeaderHeight() }
         ::ScrollWindow( 0, ::ItemHeight*n, aScroll, aClip )
         ::ValidateRect()

         ::__DisplayData( 1,, n )
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------
METHOD GetRecordCount() CLASS DataGrid
   LOCAL nCount, nRec

   IF !::IsDelIndexOn
      nCount := ::DataSource:OrdKeyCount()
    ELSE
      nRec := ::DataSource:Recno()
      ::DataSource:GoBottom()
      nCount := ::DataSource:OrdKeyNo()
      ::DataSource:Goto( nRec )
   ENDIF
RETURN nCount

//----------------------------------------------------------------------------------

METHOD __UpdateVScrollBar( lRedraw, lForce ) CLASS DataGrid
   LOCAL nFlags := SIF_ALL
   LOCAL nMin := 1, nPage := 0, nMax  := 0, nPos  := 0

   DEFAULT lRedraw TO TRUE
   DEFAULT lForce  TO FALSE

   IF ::DataSource != NIL .AND. ::DataSource:IsOpen .AND. ( ::AutoVertScroll .OR. lForce )

      nPage := Int(  ::__DataHeight/::ItemHeight )
      IF ::DataSource:ClsName == "MemoryTable" .OR. ( ::DataSource:Driver IN { "SQLRDD", "SQLEX" } ) .OR. ::ExtVertScrollBar
         nMax  := ::GetRecordCount() //::DataSource:OrdKeyCount()
         nPos  := IIF( nMax < nPage, 0, ::__VertScrolled )
         IF nMax <= nPage .AND. ::AutoVertScroll
            nMax := 0
         ENDIF
       ELSE
         nMax  := 100
         nPos  := ::DataSource:OrdKeyRelPos()*100
         nFlags := (SIF_POS | SIF_RANGE)
         IF ::GetRecordCount() <= nPage
            nMax := 0
         ENDIF
      ENDIF
      ::__nVPage := nPage
      ::__nVMax  := nMax
      ::__nVPos  := nPos
      __SetScrollInfo( ::hWnd, SB_VERT, nMin, nMax, nPage, nPos, nFlags, lRedraw )

    ELSEIF ::hWnd != NIL
      __SetScrollInfo( ::hWnd, SB_VERT, nMin, nMax, nPage, nPos, nFlags, lRedraw )
   ENDIF

RETURN Self

//----------------------------------------------------------------------------------

METHOD __UpdateHScrollBar( lRedraw ) CLASS DataGrid
   LOCAL nFlags := SIF_ALL
   LOCAL nMin := 0, nPage := 0, nMax  := 0, nPos  := 0
   DEFAULT lRedraw TO TRUE

   IF ::AutoHorzScroll .AND. IsWindow( ::hWnd )
      nMax   := ::__DataWidth
      nPage  := ::ClientWidth + 1
      nPos   := -::__HorzScrolled
      IF ( nMax == NIL .OR. nMax <= ::ClientWidth ) .AND. ::AutoHorzScroll
         nMax := 0
      ENDIF
      __SetScrollInfo( ::hWnd, SB_HORZ, nMin, nMax, nPage, nPos, nFlags, lRedraw )
   ENDIF
   ::__nHPage := nPage
   ::__nHMax  := nMax
   ::__nHPos  := nPos
RETURN Self


//------------------------------------------------------------------------------------------------

METHOD OnHorzScroll( nCode, nPos, nlParam, lDraw ) CLASS DataGrid
   LOCAL nPor, aScroll, aClip, nRight, n
   (nlParam)

   //DEFAULT nCode TO LoWord( Sender:wParam )
   //DEFAULT nPos  TO HiWord( Sender:wParam )

   ::__ResetControl()

   SWITCH nCode
      CASE SB_LINELEFT
           nPos := ABS( ::__HorzScrolled )-::__HScrollUnits
           ::OnHorzScroll( SB_THUMBTRACK, MAX( 0, nPos ) )
           RETURN 1

      CASE SB_LINERIGHT
           nPos := ABS( ::__HorzScrolled )+::__HScrollUnits
           ::OnHorzScroll( SB_THUMBTRACK, MIN( nPos, ::__DataWidth-::ClientWidth  ) )
           RETURN 1

      CASE SB_LEFT
           ::__HorzScrolled := 0
           ::__DisplayData()
           ::__UpdateHScrollBar()
           RETURN 1

      CASE SB_RIGHT
           nPos := ::__DataWidth - ::ClientWidth
           ::__HorzScrolled := -nPos
           ::__GetDataWidth(,.T.)
           ::__DisplayData()
           ::__UpdateHScrollBar()
           EXIT

      CASE SB_PAGELEFT
           nPor := ( ::ClientWidth / ::__DataWidth ) * 100
           nPos := ( ::__DataWidth * nPor ) / 100
           nPos := MIN( nPos, ABS( ::__HorzScrolled ) )
           ::__HorzScrolled += nPos

           ::ScrollWindow( nPos, 0 )
           ::ValidateRect()

           ::__DisplayData()
           ::__UpdateHScrollBar()
           ::__nScrolled -= nPos
           RETURN 1

      CASE SB_PAGERIGHT
           nPor := ( ::ClientWidth / ::__DataWidth ) * 100
           nPos := ( ::__DataWidth * nPor ) / 100
           nPos := MIN( nPos, ::__DataWidth - ABS( ::__HorzScrolled ) - nPos  )
           ::__HorzScrolled -= nPos

           ::__GetDataWidth(,.T.)

           ::ScrollWindow( -nPos, 0 )
           ::ValidateRect()

           ::__DisplayData()
           ::__UpdateHScrollBar()
           ::__nScrolled += nPos
           RETURN 1

      CASE SB_THUMBTRACK
           IF ::__HorzScrolled == -nPos
              ::ValidateRect()
              RETURN 0
           ENDIF
           ::__HorzScrolled := -nPos
           ::__GetDataWidth(,.T.)
           ::__UpdateHScrollBar()

           IF ::FreezeColumn > 0
              nRight := 0
              FOR n := 1 TO ::FreezeColumn
                  nRight += ::Children[n]:xWidth
              NEXT
              aClip := { nRight, 0, ::Width, ::__GetHeaderHeight() }
              lDraw := .T.
           ENDIF

           IF ::Transparent
              ::__DisplayData()
            ELSE
              ::ScrollWindow( ::__nScrolled-nPos, 0, aScroll, aClip )
              ::ValidateRect()


              IF lDraw == NIL .OR. lDraw
                 DEFAULT lDraw TO ::IsCovered( ::__GetHeaderHeight() )
                 IF lDraw
                    ::__DisplayData()
                  ELSE
                    IF nPos > ::__nScrolled
                       aClip := { ::ClientWidth-(nPos-::__nScrolled)-1, 0, ::ClientWidth, ::ClientHeight }
                     ELSE
                       aClip := { 0, 0, (::__nScrolled-nPos)+1, ::ClientHeight }
                    ENDIF
                    ::InvalidateRect( aClip, FALSE )
                 ENDIF
                 ::UpdateWindow()
                 ::__DisplayData( ::RowPos, , ::RowPos,  )
                 ::ValidateRect()
              ENDIF
           ENDIF
           ::__nScrolled := nPos
   END
RETURN 0


//----------------------------------------------------------------------------------

METHOD OnVertScroll( nCode, nPos ) CLASS DataGrid
   ::__ResetControl()

   ::AutoUpdate := 0

   DO CASE
      CASE nCode == SB_LINEUP
           IF ::DataSource:ClsName == "MemoryTable" .OR. (::DataSource:Driver IN { "SQLRDD", "SQLEX" }) .OR. ::ExtVertScrollBar
              ::__ScrollDown()
            ELSE
              ::OnKeyDown( VK_UP )
           ENDIF
      CASE nCode == SB_LINEDOWN
           IF ::DataSource:ClsName == "MemoryTable" .OR. (::DataSource:Driver IN { "SQLRDD", "SQLEX" }) .OR. ::ExtVertScrollBar
              ::__ScrollUp()
            ELSE
              ::OnKeyDown( VK_DOWN )
           ENDIF

      CASE nCode == SB_PAGEDOWN
           IF ::DataSource:ClsName == "MemoryTable" .OR. (::DataSource:Driver IN { "SQLRDD", "SQLEX" }) .OR. ::ExtVertScrollBar
              ::__ScrollUp( ::__VertScrolled + ::RowCountUsable )
            ELSE
              ::OnKeyDown( VK_NEXT )
           ENDIF

      CASE nCode == SB_PAGEUP
           IF ::DataSource:ClsName == "MemoryTable" .OR. (::DataSource:Driver IN { "SQLRDD", "SQLEX" }) .OR. ::ExtVertScrollBar
              ::__ScrollDown( ::__VertScrolled - ::RowCountUsable )
            ELSE
              ::OnKeyDown( VK_PRIOR )
           ENDIF
      CASE nCode == SB_TOP
      CASE nCode == SB_BOTTOM
      CASE nCode == SB_THUMBPOSITION
           IF !( ::DataSource:ClsName == "MemoryTable" ) .AND. !(::DataSource:Driver IN { "SQLRDD", "SQLEX" }) .AND. !::ExtVertScrollBar
              ::DataSource:OrdKeyRelPos( nPos/100 )
              ::Update()
           ENDIF

      CASE nCode == SB_THUMBTRACK
           IF ::DataSource:ClsName == "MemoryTable" .OR. (::DataSource:Driver IN { "SQLRDD", "SQLEX" }) .OR. ::ExtVertScrollBar
              IF nPos > ::__VertScrolled
                 ::__ScrollUp( nPos )
               ELSEIF nPos < ::__VertScrolled
                 ::__ScrollDown( nPos )
              ENDIF
           ENDIF
      CASE nCode == SB_ENDSCROLL
   ENDCASE
   ::AutoUpdate := ::__nUpdtTimer
RETURN 0

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD __FillCol( nCol ) CLASS DataGrid
   LOCAL nRow, nRec
   IF ::DataSource != NIL
      nRec := ::DataSource:Recno()
      ::Children[ nCol ]:__lHidden := .F.
      FOR nRow := 1 TO LEN( ::__DisplayArray )
          ::__GoToRec( ::__DisplayArray[nRow][2] )
          ::__FillRow( nRow, nCol )
      NEXT
      ::__GoToRec( nRec )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD __FillRow( nPos, nCol ) CLASS DataGrid
   EXTERN hb_QSelf
   LOCAL nImageWidth, nImageHeight, nImageIndex, n, nColBkColor, nColTxColor, nStatus, nAlign, cData, nRet
   LOCAL hFont, oFont, nGridBkColor, nGridTxColor, e, nImgAlign

   IF nCol == NIL
      ::__DataWidth := 0
   ENDIF

   DEFAULT nPos TO ::RowPos

   IF LEN( ::__DisplayArray ) < nPos
      RETURN .F.
   ENDIF

   nRet := ExecuteEvent( "OnQueryBackColor", Self )
   IF VALTYPE( nRet ) == "N"
      nGridBkColor :=  nRet
   ENDIF
   nRet := ExecuteEvent( "OnQueryForeColor", Self )
   IF VALTYPE( nRet ) == "N"
      nGridTxColor :=  nRet
   ENDIF


   FOR n := 1 TO LEN( ::Children )
       IF nCol == NIL .OR. nCol == n // Loading new column?
          IF nCol == NIL
             ::Children[n]:__nLeft := ::__DataWidth
          ENDIF
          IF ::Children[n]:__lHidden // Column is not into VIEW
             ::__DisplayArray[ nPos ][1][n] := ARRAY( 10 )
             ::__DisplayArray[ nPos ][1][n][1] := ""

           ELSE
             nImageWidth := 0
             nImageHeight:= 0
             nImageIndex := IIF( VALTYPE( ::Children[n]:ImageIndex ) == "B", EVAL( ::Children[n]:ImageIndex, Self, EVAL( ::__bRecNo ) ), ::Children[n]:ImageIndex )
             nRet := ExecuteEvent( "OnQueryImageIndex", ::Children[n] )

             IF VALTYPE( nRet ) == "N"
                nImageIndex := nRet
             ENDIF

             IF nImageIndex > 0 .AND. ::ImageList != NIL
                nImageWidth  := IIF( ::ImageList:IconWidth  != NIL, ::ImageList:IconWidth,  nImageWidth )
                nImageHeight := IIF( ::ImageList:IconHeight != NIL, ::ImageList:IconHeight, nImageHeight )
             ENDIF

             nColBkColor := NIL
             nColTxColor := NIL

             nRet := ExecuteEvent( "OnQueryBackColor", ::Children[n] )
             IF VALTYPE( nRet ) == "N"
                nColBkColor :=  nRet
             ENDIF
             nRet := ExecuteEvent( "OnQueryForeColor", ::Children[n] )
             IF VALTYPE( nRet ) == "N"
                nColTxColor :=  nRet
             ENDIF

             nAlign := ::Children[n]:Alignment

             IF VALTYPE( nColBkColor ) == "B"
                nColBkColor := EVAL( nColBkColor, Self, ::Children[n] )
             ENDIF
             IF VALTYPE( nColTxColor ) == "B"
                nColTxColor := EVAL( nColTxColor, Self, ::Children[n] )
             ENDIF

             DEFAULT nColBkColor TO nGridBkColor
             DEFAULT nColTxColor TO nGridTxColor

             IF VALTYPE( nAlign ) == "B"
                nAlign := EVAL( nAlign, Self, ::Children[n] )
             ENDIF

             nStatus := ::Children[n]:ControlStatus

             IF VALTYPE( ::Children[n]:ControlStatus )=="B"
                nStatus := EVAL( ::Children[n]:ControlStatus, Self, ::DataSource:Recno() )
             ENDIF

             IF ! EMPTY( ::Children[n]:Data )
                cData := ::Children[n]:Data
                IF VALTYPE( cData ) == "B"
                   cData := EVAL( cData, Self )
                 ELSE
                   DEFAULT cData TO "' '"
                   TRY
                      cData := &cData
                   catch e
                      cData := e:Description
                   END
                   IF VALTYPE( cData ) == "B"
                      cData := EVAL( cData, Self )
                   ENDIF
                ENDIF
              ELSEIF ::Children[n]:FieldPos > 0 .AND. ::DataSource:Structure != NIL
                cData := ::DataSource:FieldGet( ::Children[n]:FieldPos )
              ELSE
                cData := ""
             ENDIF
             IF ::Children[n]:Representation == 5 .AND. VALTYPE( cData ) == "D"
             ENDIF

             IF ::ConvertOem .AND. VALTYPE( cData ) == "C"
                cData := OemToChar( cData )
             ENDIF

             hFont := ::Children[n]:Font:Handle
             oFont := ExecuteEvent( "OnCellFont", ::Children[n] )

             IF VALTYPE(oFont) == "O" .AND. UPPER( oFont:ClassName ) == "FONT"
                hFont := oFont:Handle
             ENDIF
             nImgAlign := IIF( ::Children[n]:xImageAlignment > 0, ::Children[n]:xImageAlignment, IIF( ::Children[n]:xHeaderAlignment == 0, ::Children[n]:xAlignment, ::Children[n]:xHeaderAlignment ) )

             ::__DisplayArray[ nPos ][1][n] := { cData,;
                                                 nImageIndex,;
                                                 nImageWidth,;
                                                 nAlign,;
                                                 nImageHeight,;
                                                 nColBkColor,;
                                                 nColTxColor,;
                                                 nStatus,;
                                                 hFont,;
                                                 nImgAlign }
          ENDIF
       ENDIF
       IF nCol == NIL .AND. ::Children[n]:Visible
          ::__DataWidth += ::Children[n]:Width
       ENDIF
   NEXT

RETURN .T.

//----------------------------------------------------------------------------------

METHOD ArrowLeft( lMove ) CLASS DataGrid
   LOCAL lRes, nScroll, nCol, nCur, nPos := 0, nWidth, n
   DEFAULT lMove TO .T.
   IF ::FullRowSelect .OR. ::ColPos == 0
      nPos := MIN( ::__HorzScrolled + ( ::__HScrollUnits * IIF( CheckBit( GetKeyState( VK_CONTROL ) ), 5, 1 ) ), 0 )
      ::OnHorzScroll( SB_THUMBTRACK, -nPos )
      RETURN .F.
    ELSE
      lRes := ::OnColChanging()
      DEFAULT lRes TO ExecuteEvent( "OnColChanging", Self )
      DEFAULT lRes TO .T.
      IF !lRes
         RETURN .F.
      ENDIF
   ENDIF
   IF ::ColPos > 1 .OR. !lMove
      nCur := ::ColPos
      IF lMove
         nCol := ::ColPos-1
         WHILE nCol > 0 .AND. ::Children[ nCol ]:Locked
            nCol--
         ENDDO
         IF nCol <= 0
            RETURN .F.
         ENDIF
         ::ColPos := nCol
      ENDIF

      IF ::FreezeColumn > 0 .AND. ::ColPos > ::FreezeColumn
         nWidth := 0
         FOR n := 1 TO ::FreezeColumn
             nWidth += ::Children[n]:Width
         NEXT

         IF ( ::Children[::ColPos]:Left-ABS( ::__HorzScrolled) ) < nWidth
            ::__HorzScrolled += ( nWidth - ( ::Children[::ColPos]:Left-ABS( ::__HorzScrolled) ) )
            ::__UpdateHScrollBar(.T.)
            ::__DisplayData( , ::ColPos, , )
         ENDIF
      ENDIF

      IF ::Children[::ColPos]:__nLeft + ::Children[::ColPos]:Width > ::ClientWidth+ABS(::__HorzScrolled)
         nScroll := (::Children[::ColPos]:__nLeft + ::Children[::ColPos]:Width) - (::ClientWidth+ABS(::__HorzScrolled))
         //needs to scroll left instead of right
         ::OnHorzScroll( SB_THUMBTRACK, ABS(::__HorzScrolled) + nScroll,, FALSE )
         ::__DisplayData()
      ELSE
         nScroll := ::Children[::ColPos]:__nLeft - ABS(::__HorzScrolled)
      ENDIF
      IF nScroll < 0
         ::OnHorzScroll( SB_THUMBTRACK, ABS(::__HorzScrolled) - ABS(nScroll),, FALSE )
         IF ::Transparent .OR. ::IsCovered( ::__GetHeaderHeight() )
            ::__DisplayData()
          ELSE
            ::__DisplayData(, ::ColPos,, nCur )
         ENDIF
       ELSE
         ::__DisplayData( ::RowPos, ::ColPos, ::RowPos, nCur )
      ENDIF
   ENDIF
   ::OnColChanged()
   ExecuteEvent( "OnColChanged", Self )
RETURN .T.

//----------------------------------------------------------------------------------

METHOD ArrowRight( lMove ) CLASS DataGrid
   LOCAL nScroll := 0, nCol, nCur, nPos := 0, lREs

   DEFAULT lMove TO .T.

   IF ::FullRowSelect .OR. ::ColPos == 0
      IF ::__DataWidth > ::ClientWidth
         nPos := ABS( ::__HorzScrolled )+ ( ::__HScrollUnits * IIF( CheckBit( GetKeyState( VK_CONTROL ) ), 5, 1 ) )
         ::OnHorzScroll( SB_THUMBTRACK, MIN( nPos, ::__DataWidth-::ClientWidth  ) )
      ENDIF
      RETURN .F.
    ELSE
      lRes := ::OnColChanging()
      DEFAULT lRes TO ExecuteEvent( "OnColChanging", Self )
      DEFAULT lRes TO .T.
      IF !lRes
         RETURN .F.
      ENDIF
   ENDIF

   IF ::ColPos+1 <= LEN( ::Children ) .OR. !lMove
      nCur := ::ColPos
      IF lMove
         nCol := ::ColPos+1
         WHILE nCol <= LEN( ::Children ) .AND. ::Children[ nCol ]:Locked
            nCol++
         ENDDO
         IF nCol > LEN( ::Children )
            RETURN .F.
         ENDIF

         ::ColPos := nCol
      ENDIF

      IF ::Children[ ::ColPos ]:__lHidden
         ::__FillCol( ::ColPos )
      ENDIF

      IF ::Children[nCur]:__nLeft + ::Children[nCur]:Width > ::ClientWidth+ABS(::__HorzScrolled)
         nScroll := (::Children[nCur]:__nLeft + ::Children[nCur]:Width) - (::ClientWidth+ABS(::__HorzScrolled))
         ::ColPos := nCur
       ELSEIF ::Children[::ColPos]:__nLeft < ABS(::__HorzScrolled)
         nScroll := ::Children[::ColPos]:__nLeft - ABS(::__HorzScrolled)
         ::OnHorzScroll( SB_THUMBTRACK, ABS(::__HorzScrolled) + nScroll,, FALSE )
         ::__DisplayData()
       ELSE
         nScroll := ( ::Children[::ColPos]:__nLeft + ::Children[ ::ColPos ]:Width ) - ::ClientWidth - ABS(::__HorzScrolled)
      ENDIF

      IF nScroll > 0
         IF ::Children[ ::ColPos ]:Width > ::ClientWidth
            nScroll -= ( ::Children[ ::ColPos ]:Width - ::ClientWidth )
         ENDIF
         ::OnHorzScroll( SB_THUMBTRACK, ABS(::__HorzScrolled) + nScroll,, FALSE )
         IF ::Transparent .OR. ::IsCovered( ::__GetHeaderHeight() ) .OR. ::ColPos == nCur
            ::__DisplayData()
          ELSE
            ::__DisplayData(, nCur,, ::ColPos )
         ENDIF
       ELSE
         ::__DisplayData( ::RowPos, nCur, ::RowPos, ::ColPos )
      ENDIF
   ENDIF

   ::OnColChanged()
   ExecuteEvent( "OnColChanged", Self )
RETURN .T.

//----------------------------------------------------------------------------------

METHOD Down() CLASS DataGrid
   LOCAL lRes, aScroll, aClip

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   IF ValType( lRes ) != "L"
      lRes := NIL
   ENDIF
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )

   IF ::MultipleSelection .AND. ! IsKeyDown( VK_CONTROL )
      ::DeselectAll()
   ENDIF

   ::__ResetRecordPos(.F.)
   ::__SkipRecords( 1 )

   IF ::HitBottom
      EVAL( ::__bGoBottom )
      RETURN NIL
   ENDIF

   ::RowPos ++

   IF ::RowPos > ::RowCountUsable
      ::RowPos := ::RowCountUsable

      // Retrieve next Record
      IF ::RowCountUsable != ::RowCountVisible
         ::__SkipRecords( 1 )
      ENDIF

      IF ::HitBottom
         ADEL( ::__DisplayArray, 1, TRUE )
         EVAL( ::__bGoBottom )
       ELSE
         ADEL( ::__DisplayArray, 1 )
         IF ::RowCountVisible > 0 .AND. LEN( ::__DisplayArray ) >= ::RowCountVisible
            ::__DisplayArray[ ::RowCountVisible ] := { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() }
            ::__FillRow( ::RowCountVisible )

            IF ::RowCountUsable != ::RowCountVisible
               ::__SkipRecords( -1 )
            ENDIF
         ENDIF
      ENDIF

      IF ::IsCovered( ::__GetHeaderHeight() )
         ::__DisplayData()

         IF ::bRowChanged != NIL
            EVAL( ::bRowChanged, Self )
         ENDIF

         ::OnRowChanged()
         ExecuteEvent( "OnRowChanged", Self )
         RETURN Self
      ENDIF

      // Scroll up
      IF ::Transparent
         ::__DisplayData()
       ELSE
         aScroll := { 0, ::__GetHeaderHeight(), ::ClientWidth, (::RowCountUsable-1)*::ItemHeight + ::__GetHeaderHeight() }
         aClip   := { 0, ::__GetHeaderHeight(), ::ClientWidth, (::RowCountUsable-1)*::ItemHeight + ::__GetHeaderHeight() }
         ::ScrollWindow( 0, -::ItemHeight, aScroll, aClip )
         ::ValidateRect()
         ::__DisplayData( MIN( ::RowCountUsable-1, ::RowCountVisible ),, ::RowCountVisible )
      ENDIF
    ELSE
      ::__DisplayData( ::RowPos-1,, ::RowPos )
   ENDIF
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )

RETURN Self

//----------------------------------------------------------------------------------

METHOD Up() CLASS DataGrid
   LOCAL lRes, aScroll, aClip

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   IF ValType( lRes ) != "L"
      lRes := NIL
   ENDIF
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   IF ::MultipleSelection .AND. ! IsKeyDown( VK_CONTROL )
      ::DeselectAll()
   ENDIF

   ::__ResetRecordPos(.F.)
   ::__SkipRecords( -1)

   IF ::HitTop
      EVAL( ::__bGoTop )
      RETURN NIL
   ENDIF

   ::RowPos --

   IF ::RowPos <= 0
      ::RowPos := 1

      IF ::RowCount < ::RowCountVisible
         AADD( ::__DisplayArray, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )
      ENDIF

      // Insert next first visible Record
      AINS( ::__DisplayArray, 1, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )//, TRUE )

      ::__FillRow( 1 )

      // Delete last Record
      IF ::IsCovered( ::__GetHeaderHeight() )
         ::__DisplayData()
         IF ::bRowChanged != NIL
            EVAL( ::bRowChanged, Self )
         ENDIF
         ::OnRowChanged()
         ExecuteEvent( "OnRowChanged", Self )
         RETURN Self
      ENDIF

      IF ::Transparent
         ::__DisplayData()
       ELSE
         // Scroll Down
         aScroll := { 0, ::__GetHeaderHeight()+::ItemHeight, ::ClientWidth, ( ::RowCountVisible * ::ItemHeight ) + ::__GetHeaderHeight() }
         aClip   := { 0, ::__GetHeaderHeight()+::ItemHeight, ::ClientWidth, ( ::RowCountVisible * ::ItemHeight ) + ::__GetHeaderHeight() }
         ::ScrollWindow(  0, ::ItemHeight, aScroll, aClip )
         ::ValidateRect()
      ENDIF
   ENDIF
   ::__DisplayData( ::RowPos,, ::RowPos+1 )
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------
METHOD OnDestroy() CLASS DataGrid
   WHILE LEN( ::Children ) > 0
       ATAIL( ::Children ):Destroy()
   ENDDO
   ::xDataSource := NIL
   IF ::__LinePen != NIL
      DeleteObject( ::__LinePen )
   ENDIF
   IF ::__SelBorderPen != NIL
      DeleteObject( ::__SelBorderPen )
   ENDIF
   IF ::__HoverBorderPen != NIL
      DeleteObject( ::__HoverBorderPen )
   ENDIF

   DeleteObject( ::__hDragBrush )

   Super:OnDestroy()
RETURN NIL

//----------------------------------------------------------------------------------

METHOD PageDown( nCount ) CLASS DataGrid
   LOCAL lRes, i, n, nColumns, nRec

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   IF ValType( lRes ) != "L"
      lRes := NIL
   ENDIF
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   ::__ResetRecordPos(.F.)
   IF ::RowPos < ::RowCountUsable
      ::__SkipRecords( ::RowCountUsable-::RowPos )
      ::__DisplayData( ::RowPos,, ::RowPos )
      ::RowPos := ::RowCountUsable
      ::__DisplayData( ::RowCountUsable,, ::RowCountUsable )
      IF ::bRowChanged != NIL
         EVAL( ::bRowChanged, Self )
      ENDIF
      ExecuteEvent( "OnRowChanged", Self )
      RETURN 0
   ENDIF

   ::__SkipRecords( 1 )
   IF ::HitBottom
      EVAL( ::__bGoBottom )
      RETURN Self
   ENDIF
   ::__SkipRecords( -1 )

   nColumns      := LEN( ::Children )
   ::__DisplayArray:= {}

   IF nCount > 1
      FOR n := 1 TO ::RowCountVisible*(nCount-1)
          ::__SkipRecords( 1 )
          IF ::HitBottom
             EVAL( ::__bGoBottom )
             EXIT
          ENDIF
      NEXT
   ENDIF

   FOR n := 1 TO ::RowCountVisible
       ::__SkipRecords( 1 )
       IF ::HitBottom
          EVAL( ::__bGoBottom )
          EXIT
       ENDIF
       AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )
       ::__FillRow( n )
   NEXT
   n--
   IF ::RowCountVisible > ::RowCountUsable .AND. ::RowCountVisible == n
      ::__SkipRecords( -1 )
      IF ::HitTop
         EVAL( ::__bGoTop )
      ENDIF
   ENDIF

   IF n < ::RowCountVisible
      // have reached the bottom need to insert __Records to fill it up
      nRec := ::DataSource:Recno()
      ::__SkipRecords( -(n-1) )
      FOR i := 1 TO ::RowCountUsable-n
          ::__SkipRecords( -1 )
          IF ::HitTop
             EVAL( ::__bGoTop )
             EXIT
          ENDIF
          AINS( ::__DisplayArray, 1, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() }, TRUE )
          ::__FillRow( 1 )
      NEXT
      ::__GoToRec( nRec )
   ENDIF
   ::__DisplayData()
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD PageUp() CLASS DataGrid
   LOCAL lRes, n, nColumns

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   IF ValType( lRes ) != "L"
      lRes := NIL
   ENDIF
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   ::__ResetRecordPos(.F.)
   IF ::RowPos > 1
      ::__SkipRecords( -(::RowPos-1) )
      ::__DisplayData( ::RowPos,, ::RowPos )
      ::RowPos := 1
      ::__DisplayData( ::RowPos,, ::RowPos )
      IF ::bRowChanged != NIL
         EVAL( ::bRowChanged, Self )
      ENDIF
      ExecuteEvent( "OnRowChanged", Self )
      RETURN 0
   ENDIF

   ::__SkipRecords( -1 )
   IF ::HitTop
      ::RowPos := 1
      EVAL( ::__bGoTop )
      RETURN Self
   ENDIF
   ::__SkipRecords( 1 )

   nColumns      := LEN( ::Children )
   ::__DisplayArray:= {}

   ::__SkipRecords( -( ::RowCountUsable ) )
   IF ::Hittop
      EVAL( ::__bGoTop )
   ENDIF

   FOR n := 1 TO ::RowCountVisible
       AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )
       ::__FillRow( n )
       ::__SkipRecords( 1 )
       IF ::HitBottom
          EVAL( ::__bGoBottom )
          EXIT
       ENDIF
   NEXT

   ::__GoToRec( ::__DisplayArray[1][2] )

   ::__DisplayData()
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD Home() CLASS DataGrid
   LOCAL lRes, n, nColumns

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   IF ValType( lRes ) != "L"
      lRes := NIL
   ENDIF
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   ::__ResetRecordPos(.F.)
   ::RowPos := 1
   ::__VertScrolled := 1

   ::__SkipRecords( -1 )
   IF ::HitTop
      EVAL( ::__bGoTop )
      RETURN Self
   ENDIF
   ::__SkipRecords( 1 )

   nColumns      := LEN( ::Children )
   ::__DisplayArray:= {}

   EVAL( ::__bGoTop )
   FOR n := 1 TO ::RowCountVisible
       AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )
       ::__FillRow( n )
       ::__SkipRecords( 1 )
       IF ::HitBottom
          EVAL( ::__bGoBottom )
          EXIT
       ENDIF
   NEXT
   EVAL( ::__bGoTop )
   ::__DisplayData()
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD End( lVUpdate ) CLASS DataGrid
   LOCAL lRes, n, nColumns
   DEFAULT lVUpdate TO .T.

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   IF ValType( lRes ) != "L"
      lRes := NIL
   ENDIF
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   ::__ResetRecordPos(.F.)

   ::__SkipRecords( 1 )
   IF ::HitBottom
      EVAL( ::__bGoBottom )
      RETURN Self
   ENDIF
   ::__SkipRecords( -1 )

   nColumns      := LEN( ::Children )
   ::__DisplayArray:= {}

   EVAL( ::__bGoBottom )
   ::__SkipRecords( -(::RowCountUsable-1) )
   IF ::HitTop
      EVAL( ::__bGoTop )
   ENDIF
   FOR n := 1 TO ::RowCountUsable
       AADD( ::__DisplayArray, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno(), ::DataSource:Deleted() } )
       ::__FillRow( n )
       ::__SkipRecords( 1 )
       IF ::HitBottom
          EVAL( ::__bGoBottom )
          EXIT
       ENDIF
   NEXT
   EVAL( ::__bGoBottom )

   ::RowPos := MIN( ::RowCount, ::RowCountUsable )
   ::__DisplayData()

   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD AutoAddColumns( lEdit, aExclude ) CLASS DataGrid
   LOCAL aField, n, oCol, lCol := .F.

   IF VALTYPE( lEdit ) == "A"
      aExclude := lEdit
      lEdit := .F.
   ENDIF

   DEFAULT lEdit TO .F.

   IF VALTYPE( ::DataSource )=="O" .AND. ::DataSource:IsOpen
      ::__SetBlocks()

      FOR n := 1 TO LEN( ::DataSource:Structure )
          aField := ::DataSource:Structure[n]

          IF Empty( aExclude ) .OR. ASCAN( aExclude, {|c| Upper(c)==Upper(aField[1])} ) == 0
             oCol := GridColumn( Self )
             oCol:xText     := __Proper( aField[1] )

             oCol:FieldPos  := n

             //oCol:Data      := "hb_QSelf():DataSource:Fields:" + aField[1]

             oCol:AllowSize := .T.
             oCol:AllowDrag := .T.
             oCol:Create()
             oCol:Width     := MAX( aField[3], LEN(oCol:xText)+2 )*7

             DO CASE
                CASE aField[2]=="C"
                     IF lEdit
                        oCol:Picture := "@k"
                        oCol:Control := {|o|MaskEdit( o )  }
                     ENDIF
                CASE aField[2]=="D"
                     IF lEdit
                        oCol:Control := {|o|MaskEdit( o )  }
                     ENDIF
                     oCol:Alignment := ALIGN_CENTER
                CASE aField[2]=="L"
                     oCol:Width := MAX( 6, LEN(oCol:xText)+2 )*7
                     oCol:Alignment := ALIGN_CENTER
                CASE aField[2]=="N"
                     IF lEdit
                        oCol:Control := {|o|MaskEdit( o )  }
                     ENDIF
                     oCol:Alignment := ALIGN_RIGHT
             ENDCASE

             IF lEdit
                oCol:ControlAccessKey := (GRID_CHAR | GRID_LCLICK)
                oCol:OnSave := {|xData| ::DataSource:Fields:Put( xData, ::System:TypeCast( Alltrim(aField[1], aField[2] ) ) ) }
             ENDIF
          ENDIF
      NEXT
      IF ::__lCreated
         ::__Update()
      ENDIF
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------

METHOD __Edit( n, xPos, yPos, nMessage, nwParam ) CLASS DataGrid
   LOCAL aRect, x, xValue, nAlign, oSelf := Self, nCol := ::ColPos
   (xPos)
   (yPos)

   IF ::__CurControl == NIL .AND. ( nCol > 0 .OR. ::FullRowSelect )
      IF ::FullRowSelect
         nCol := ASCAN( ::Children, {|o|o:Representation==3} )
         IF nCol == 0
            nCol := ::ColPos
         ENDIF
      ENDIF
      IF ::Children[nCol]:Representation == 3 .AND. nMessage == GRID_LCLICK .OR. (nwParam IN {VK_SPACE,VK_RETURN})
         xValue := ::__DisplayArray[ ::RowPos ][ 1 ][ nCol ][ 1 ]
         IF VALTYPE( xValue ) == "L"
            IF (nwParam IN {VK_SPACE,VK_RETURN}) .AND. n == 0
               RETURN Self
            ENDIF
            IF HGetPos( ::Children[nCol]:EventHandler, "OnSave" ) > 0
               xValue := ::__DisplayArray[ ::RowPos ][ 1 ][ nCol ][ 1 ]
               //ExecuteEvent( "OnSave", ::Children[nCol], ! xValue, nwParam )
               ::Form:&( ::Children[nCol]:EventHandler[ "OnSave" ] )( Self, ! xValue, nwParam )
               ::UpdateRow()
               RETURN Self
            ENDIF
         ENDIF
      ENDIF
      IF ::FullRowSelect
         nCol := ASCAN( ::Children, {|o|o:Control != NIL .AND. ( o:ControlAccessKey & nMessage ) != 0 } )
         IF nCol == 0
            nCol := ::ColPos
         ENDIF
      ENDIF
      IF ::Children[nCol]:Control != NIL .AND. (::Children[nCol]:ControlAccessKey & nMessage) != 0
         ::__CurControl := EVAL( ::Children[nCol]:Control, Self, ::DataSource:Recno() )
         aRect := ::GetItemRect()
         IF aRect == NIL
            RETURN Self
         ENDIF
      ENDIF

      IF ::__CurControl == NIL
         IF nwParam == VK_RETURN .AND. ( n := HSCAN( ::Form:__hObjects, {|a,o| (a), o:__xCtrlName == "Button" .AND. o:IsWindowVisible() .AND. o:DefaultButton } ) ) > 0
            ExecuteEvent( "OnClick", HGetValueAt( ::Form:__hObjects, n ) )
            RETURN NIL
         ENDIF
       ELSE
         IF ::__CurControl:__xCtrlName == "MaskEdit"
            ::__CurControl:Picture := ::Children[::ColPos]:Picture
         ENDIF

         ::__CurControl:Left   := aRect[1]-1
         ::__CurControl:Width  := aRect[3]-aRect[1]+2
         ::__CurControl:Top    := aRect[2]-1
         ::__CurControl:Height := aRect[4]-aRect[2]+2

         ::__CurControl:SetStyle( ES_MULTILINE )
         ::__CurControl:SetStyle( ES_AUTOHSCROLL )
         ::__CurControl:SetStyle( WS_BORDER, .T. )
         ::__CurControl:ExStyle := 0

         nAlign := ::Children[::ColPos]:Alignment
         IF VALTYPE( nAlign ) == "B"
            nAlign := EVAL( nAlign, Self, ::Children[x] )
         ENDIF
         xValue := ::__DisplayArray[ ::RowPos ][ 1 ][ ::ColPos ][ 1 ]

         IF nAlign == NIL
            SWITCH VALTYPE( xValue )
               CASE "N"
                    xValue := STR( xValue )
                    nAlign := ALIGN_RIGHT
                    EXIT

               CASE "D"
                    nAlign := ALIGN_CENTER
                    EXIT

               CASE "L"
                    nAlign := ALIGN_CENTER
                    EXIT

               CASE "C"
                    nAlign := ALIGN_LEFT
                    EXIT
            END
         ENDIF
         DO CASE
            CASE nAlign == ALIGN_RIGHT
                 ::__CurControl:SetStyle( ES_RIGHT )
            CASE nAlign == ALIGN_CENTER
                 ::__CurControl:SetStyle( ES_CENTER )
         ENDCASE
         IF ::__CurControl:__xCtrlName == "EditBox"
            ::__CurControl:xText := ALLTRIM( XSTR( xValue ) )
          ELSE
            ::__CurControl:Text := xValue
         ENDIF
         ::__CurControl:OnWMKeyDown   := <|o,n|
                                           IF ::__aEditCol != NIL
                                              ::KillTimer(20)
                                              o:BackColor := ::__aEditCol[1]
                                              o:ForeColor := ::__aEditCol[2]
                                              ::__aEditCol := NIL
                                           ENDIF
                                           IF n == 27
                                              ::__ResetControl()
                                              RETURN 0
                                            ELSEIF (n IN {13,9})
                                              IF ! ::__ControlSaveData(o,n)
                                                 RETURN 0
                                              ENDIF
                                              ::__ResetControl()
                                           ENDIF
                                           RETURN NIL
                                         >

         ::__CurControl:OnWMKillFocus := {|o| ::__cEditText := o:Text, ::__ResetControl() }
         ::__CurControl:Transparent := ::Transparent
         ::__CurControl:Create()

         IF ::__CurControl:__xCtrlName == "EditBox"
            ::__CurControl:PostMessage( EM_SETSEL, 0, -1 )
         ENDIF

         IF n == 1 .AND. nwParam != NIL .AND. ! (nwParam IN {13,9})
            ::__CurControl:PostMessage( WM_CHAR, nwParam, 0 )
         ENDIF

         ::__CurControl:SetFocus()
      ENDIF

   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------

METHOD GetItemRect() CLASS DataGrid
   LOCAL aRect, nLeft, nTop, nBottom, nRight
   IF ::RowPos> 0 .AND. ::RowPos<=::RowCountUsable
      nLeft   := ::__HorzScrolled + ::Children[::ColPos]:__nLeft
      nTop    := ::__GetHeaderHeight() + ( ( ::RowPos-1 ) * ::ItemHeight )
      nBottom := nTop + ::ItemHeight - 1
      nRight  := nLeft + ::Children[ ::ColPos ]:Width - 1
      aRect:={ nLeft, nTop, nRight, nBottom }
   ENDIF
RETURN aRect

//---------------------------------------------------------------------------------------------------------------------------

METHOD __ControlSaveData( oEdit, nKey ) CLASS DataGrid
   LOCAL oCol, lRefresh := .T.
   oCol := ::Children[::ColPos]
   DEFAULT oEdit TO ::__CurControl

   IF oEdit:__xCtrlName == "MaskEdit" .AND. !oEdit:validating
      IF oEdit:oGet:BadDate
         oEdit:ToolTip:Text    := "Invalid Date"
         oEdit:ToolTip:Balloon := .T.
         oEdit:ToolTip:Popup()

         ::__aEditCol := { oEdit:BackColor, oEdit:ForeColor }

         oEdit:BackColor := RGB( 255, 0, 0 )
         oEdit:ForeColor := RGB( 255, 255, 255 )

         ::SetTimer( 20, 1000 )
         MessageBeep( MB_OK )
         oEdit:SendMessage( EM_SETSEL, 0, 0 )
         RETURN .F.
      ENDIF
      IF ( oEdit:lInvalid .OR. oCol:ControlValid == NIL )
         oEdit:oGet:Assign()
         IF oCol:OnSave != NIL
            lRefresh := EVAL( oCol:OnSave, Self, oEdit:oGet:VarGet(), nKey )
         ENDIF
         IF HGetPos( oCol:EventHandler, "OnSave" ) != 0
            lRefresh := oCol:Form:&( oCol:EventHandler[ "OnSave" ] )( Self, oEdit:oGet:VarGet(), nKey )
         ENDIF
      ENDIF
    ELSEIF oEdit:__xCtrlName == "EditBox"
      IF oCol:ControlValid == NIL
         IF oCol:OnSave != NIL
            lRefresh := EVAL( oCol:OnSave, Self, ::System:TypeCast( IIF( nKey == VK_TAB, ::__cEditText, oEdit:Text ), VALTYPE( oCol:CellData ) ), nKey )
         ENDIF
         IF HGetPos( oCol:EventHandler, "OnSave" ) > 0
            lRefresh := oCol:Form:&( oCol:EventHandler[ "OnSave" ] )( Self, ::System:TypeCast( IIF( nKey == VK_TAB, ::__cEditText, oEdit:Text ), VALTYPE( oCol:CellData ) ), nKey  )
         ENDIF
      ENDIF
   ENDIF
   ::SetFocus()

   IF VALTYPE( lRefresh ) != "L"
      lRefresh := .T.
   ENDIF
   IF GetFocus() != ::hWnd
      ::PostMessage( WM_KILLFOCUS, 0, 0 )
   ENDIF
   IF lRefresh
      ::__FillRow( ::RowPos )
      ::__DisplayData( ::RowPos, ::ColPos, ::RowPos, ::ColPos )
   ENDIF
RETURN .T.

//----------------------------------------------------------------------------------
//------------------------------  COLUMN CLASS -------------------------------------
//----------------------------------------------------------------------------------

CLASS GridColumn INHERIT Object
   PROPERTY ContextMenu GET __ChkComponent( Self, @::xContextMenu )
   PROPERTY ButtonMenu  GET __ChkComponent( Self, @::xButtonMenu )
   PROPERTY HeaderMenu  GET __ChkComponent( Self, @::xHeaderMenu )

   PROPERTY ImageAlignment    SET ::Refresh(v)             DEFAULT ALIGN_DEFAULT
   PROPERTY Alignment         SET ::SetAlignment(v)        DEFAULT ALIGN_LEFT
   PROPERTY Width             SET ::SetWidth(v)
   PROPERTY BackColor         ROOT "Colors" SET ::SetColor(1,v)
   PROPERTY ForeColor         ROOT "Colors" SET ::SetColor(2,v)
   PROPERTY ImageIndex        SET ::SetImageIndex(v)
   PROPERTY Text              SET ::SetText(v)
   PROPERTY HeaderImageIndex  SET ::Refresh(v)             DEFAULT 0
   PROPERTY Representation    SET ::SetRepresentation(v)   DEFAULT 1
   PROPERTY AutoEdit          SET ::__SetAutoEdit(v)       DEFAULT .F.

   PROPERTY AllowSize         DEFAULT .F.
   PROPERTY AllowDrag         DEFAULT .F.
   PROPERTY Locked            DEFAULT .F.
   PROPERTY Picture
   PROPERTY Visible           DEFAULT .T.
   PROPERTY Data
   PROPERTY MinWidth          DEFAULT 0
   PROPERTY MaxWidth          DEFAULT 0

   PROPERTY ButtonText

   PROPERTY Tag
   PROPERTY SelOnlyRep        DEFAULT .T.
   PROPERTY HeaderTooltip
   PROPERTY HeaderFont
   PROPERTY HeaderAlignment   SET ::Refresh(v) DEFAULT ALIGN_DEFAULT
   PROPERTY Font
   PROPERTY Type              DEFAULT "C"
   PROPERTY FieldPos          DEFAULT 0

   PROPERTY HeaderBackColor   ROOT "Colors" GET IIF( ::xHeaderBackColor == NIL, ::__SysHeaderBackColor, ::xHeaderBackColor )
   PROPERTY HeaderForeColor   ROOT "Colors" GET IIF( ::xHeaderForeColor == NIL, ::__SysHeaderForeColor, ::xHeaderForeColor )

   DATA aSelRect      EXPORTED

   DATA xLeft         EXPORTED
   ACCESS Left        INLINE ::GetSize(1)
   ASSIGN Left(n)     INLINE ::xLeft := n

   DATA xTop          EXPORTED
   ACCESS Top         INLINE ::GetSize(2)
   ASSIGN Top(n)      INLINE ::xTop := n

   DATA xHeight       EXPORTED
   ACCESS Height      INLINE ::GetSize(4)
   ASSIGN Height(n)   INLINE ::xHeight := n

   DATA xPosition     EXPORTED
   ACCESS Position    INLINE ::xPosition
   ASSIGN Position(n) INLINE ::SetPosition( n )

   ACCESS xCaption    INLINE ::xText
   ASSIGN xCaption(c) INLINE ::xText := c

   ACCESS Caption     INLINE ::xText
   ASSIGN Caption(c)  INLINE ::xText := c

   DATA IsContainer EXPORTED INIT .F.

   ACCESS EnumFieldPos               INLINE ::__GetFields()

   DATA __SysHeaderBackColor           EXPORTED INIT __GetSystem():CurrentScheme:ToolStripPanelGradientBegin
   DATA __SysHeaderForeColor           EXPORTED INIT __GetSystem():CurrentScheme:ToolStripBorder

   ACCESS CellData                   INLINE ::Parent:__DisplayArray[ ::Parent:RowPos ][1][ ::Parent:ColPos ][1]

   DATA __lResizeable                EXPORTED  INIT {.F.,.F.,.F.,.F.,.F.,.T.,.F.,.F.}
   DATA __lMoveable                  EXPORTED  INIT .F.
   DATA __lCopyCut                   EXPORTED  INIT .F.
   DATA __lAllowCopy                 EXPORTED  INIT .F.
   DATA __lCreateAfterChildren       EXPORTED  INIT .F.
   DATA __IdeImageIndex              EXPORTED  INIT 3
   DATA __TempRect                   EXPORTED
   DATA __IsControl                  EXPORTED  INIT .F.
   DATA __PropFilter                 EXPORTED  INIT {}
   DATA __lHidden                    EXPORTED  INIT .F.

   DATA EnumImageAlignment           EXPORTED  INIT { { "Column Default", "Left", "Center", "Right" }, {ALIGN_DEFAULT,ALIGN_LEFT,ALIGN_CENTER,ALIGN_RIGHT} }
   DATA EnumHeaderAlignment          EXPORTED  INIT { { "Column Default", "Left", "Center", "Right" }, {ALIGN_DEFAULT,ALIGN_LEFT,ALIGN_CENTER,ALIGN_RIGHT} }
   DATA EnumRepresentation           EXPORTED  INIT { { "Normal", "ProgressBar", "CheckBox", "Button", "Long Date" }, {1,2,3,4,5} }
   DATA EnumAlignment                EXPORTED  INIT { { "Left", "Center", "Right" }, {ALIGN_LEFT,ALIGN_CENTER,ALIGN_RIGHT} }

   DATA Parent                       EXPORTED
   DATA ClsName                      EXPORTED  INIT "GridColumn"
   DATA Owner                        EXPORTED

   DATA ToolTip                      EXPORTED
   DATA Control                      EXPORTED
   DATA ControlAlign                 EXPORTED  INIT DT_RIGHT
   DATA ControlStatus                EXPORTED
   DATA ControlWidth                 EXPORTED  INIT 13
   DATA ControlHeight                EXPORTED  INIT 16
   DATA ControlAccessKey             EXPORTED  INIT GRID_LCLICK
   DATA ControlValid                 EXPORTED
   DATA ControlMultiline             EXPORTED  INIT TRUE
   DATA ControlSaveArrayPos          EXPORTED
   DATA ControlBackColor             EXPORTED
   DATA ControlForeColor             EXPORTED
   DATA ControlObject                EXPORTED
   DATA ControlHide                  EXPORTED  INIT FALSE
   DATA ShowControls                 EXPORTED  INIT FALSE
   DATA OnSave                       EXPORTED
   DATA ImageList                    EXPORTED

   DATA Events                       EXPORTED
   DATA Cargo                        EXPORTED

   DATA Children                     EXPORTED
   DATA hWnd                         EXPORTED
   DATA Active                       EXPORTED  INIT .F.

   DATA EventHandler                 EXPORTED
   DATA MDIContainer                 EXPORTED  INIT .F.

   DATA Theming                      EXPORTED  INIT .F.
   DATA __nLeft                      EXPORTED  INIT 0
   DATA __nSortArrow                 EXPORTED  INIT 0

   DATA SortArrow                    EXPORTED  INIT 0 // compatibility remove

   DATA __HeaderLeft                 PROTECTED INIT 0
   DATA __HeaderRight                PROTECTED INIT 0
   DATA __HeaderX                    PROTECTED INIT 0
   DATA __aVertex                    PROTECTED
   DATA __aMesh                      PROTECTED


   METHOD Init() CONSTRUCTOR
   METHOD SetColor()
   METHOD Create()
   METHOD MoveWindow()               INLINE Self
   METHOD InvalidateRect()           INLINE ::DrawHeader()
   METHOD UpdateWindow()             INLINE Self
   METHOD IsWindowVisible()          INLINE .T.
   METHOD IsWindow()                 INLINE .T.
   METHOD SetWidth( n )              INLINE ::Parent:__SetColWidth( ::xPosition, n )

   METHOD SetText()                  INLINE NIL
   METHOD SetPosition( n )           INLINE ::Parent:__SetColPos( ::xPosition, n )
   METHOD GetRectangle()             INLINE { ::Left, ::Top, ::Left + ::Width, ::Top + ::Height }
   METHOD GetChildFromPoint()        INLINE Self
   METHOD SetSize()                  INLINE Self
   METHOD __OnParentSize()           INLINE 0
   METHOD Refresh()                  INLINE ::Parent:__DisplayData(, ::xPosition,, ::xPosition )
   METHOD GetEditValue()             INLINE ::Parent:__CurControl:oGet:VarGet()
   METHOD SetWindowPos(h, x, y, cx ) INLINE (h), (x), (y), ::Width := cx, ::Parent:Update()

   METHOD DrawHeader()
   METHOD CreateDragImage()
   METHOD Destroy()
   METHOD GetRect()
   METHOD GetSize()
   METHOD SetImageIndex()
   METHOD SetAlignment()
   METHOD SetRepresentation()
   METHOD __SetSortArrow()
   METHOD __SetAutoEdit()
   METHOD __GetFields()
ENDCLASS

METHOD Init( oParent ) CLASS GridColumn
   LOCAL nColor1, nColor2
   ::Children    := {}
   ::Parent      := oParent
   ::xImageIndex := 0
   ::xPosition   := LEN( ::Parent:Children )+1
   IF oParent:DesignMode
      __SetInitialValues( Self )
   ENDIF
   //::Form       := oParent:Form

   ::HeaderFont := Font( Self )
   ::Font       := Font( Self )

   nColor1 := ::ColorScheme:ButtonSelectedGradientBegin
   nColor2 := ::ColorScheme:ButtonSelectedGradientEnd

   ::__aMesh    := { {=>} }
   ::__aMesh[1]:UpperLeft  := 0
   ::__aMesh[1]:LowerRight := 1

   ::__aVertex  := { {=>}, {=>} }

   ::__aVertex[1]:Alpha := 0
   ::__aVertex[2]:Alpha := 0

   ::Font:Create()

   ::HeaderFont:Create()

   ::EventHandler := Hash()
   HSetCaseMatch( ::EventHandler, .F. )
   ::__xCtrlName := "GridColumn"
   ::__CreateProperty( "GridColumn" )
   ::Events := {}
   IF oParent:DesignMode
      ::Events := { ;
                  {"Object",      {;
                                  { "OnInit"             , "", "" } } },;
                  {"Data",        {;
                                  { "OnSave"            , "", "xValue", "nKey" },;
                                  { "ButtonClicked"     , "", "" } } },;
                  {"Color",       {;
                                  { "OnQueryBackColor"  , "", "" },;
                                  { "OnQueryForeColor"  , "", "" } } },;
                  {"Image",       {;
                                  { "OnQueryImageIndex" , "", "" } } },;
                  {"General",     {;
                                  { "OnCreate"          , "", "" },;
                                  { "OnContextMenu"     , "", "" },;
                                  { "OnCellFont"        , "", "" },;
                                  { "OnHeaderClick"     , "", "" } } } }
   ENDIF
   ::Font:FaceName  := oParent:Font:FaceName
   ::Font:Bold      := oParent:Font:Bold
   ::Font:Italic    := oParent:Font:Italic
   ::Font:Underline := oParent:Font:Underline
   ::Font:PointSize := oParent:Font:PointSize
   ::Font:FileName  := oParent:Font:FileName
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS GridColumn
   LOCAL cText
   IF ::hWnd != NIL
      RETURN Self
   ENDIF

   IF ! ::DesignMode .AND. ! ::Application:__Vxh
      cText := ::Text

      IF VALTYPE(cText)=="C" .AND. LEFT(cText,2)=="{|"
         cText := &cText
      ENDIF
      IF VALTYPE(cText)=="B"
         cText := EVAL(cText)
      ENDIF
      ::xText := cText
   ENDIF
   IF ::DesignMode
      IF ::Parent:TreeItem == NIL
         ::Application:ObjectTree:Set( ::Parent )
      ENDIF
      ::Application:ObjectTree:Set( Self )
   ENDIF

   IF VALTYPE( ::xImageIndex ) == "N"
      ::xImageIndex := MAX( 0, ::xImageIndex )
   ENDIF

   ExecuteEvent( "OnInit", Self )

   AADD( ::Parent:Children, Self )

   ::xPosition := LEN( ::Parent:Children )
   ::hWnd := Seconds()
   DEFAULT ::xWidth TO 10 * LEN( ::xText )
   ::Parent:__UpdateHScrollBar()

   TRY
      IF ::AutoEdit
         ::Control := {|o| IIF( ValType(::CellData) $ "MC", EditBox(o), MaskEdit(o) ) }
         ::ControlAccessKey := (GRID_CHAR | GRID_LCLICK)
      ENDIF
   CATCH
   END
   ExecuteEvent( "OnCreate", Self )
   ::Parent:Update()
   WITH OBJECT ::Parent
      IF ! :DesignMode .AND. :AnchorColumn == LEN( :Children )
         :Children[ :AnchorColumn ]:xWidth := ( :Width - :__DataWidth ) + :Children[ :AnchorColumn ]:xWidth
         :__GetDataWidth()
         :__DisplayData()
      ENDIF
   END
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD __GetFields() CLASS GridColumn
   LOCAL n, aFields := {{},{}}, aStruct

   IF VALTYPE( ::Parent:DataSource ) == "O" .AND. ::Parent:DataSource:Structure != NIL
      aStruct := ::Parent:DataSource:Structure

      AADD( aFields[1], "" )
      AADD( aFields[2], 0 )
      FOR n := 1 TO LEN( aStruct )
          AADD( aFields[1], aStruct[n][1] )
          AADD( aFields[2], n )
      NEXT
   ENDIF
RETURN aFields

//---------------------------------------------------------------------------------------------------------------------------
METHOD CreateDragImage( nLeft ) CLASS GridColumn
   LOCAL hImageList, hMemBitmap, nWidth

   nWidth := MIN( ::Parent:ClientWidth, nLeft + ::Width ) - nLeft
   nWidth ++

   hMemBitmap := GetScreenBitmap( {nLeft,0,nLeft+nWidth, ::Parent:ClientHeight}, ::Parent:hWnd )

   hImageList := ImageListCreate( nWidth, ::Parent:ClientHeight, (ILC_COLORDDB | ILC_MASK), 1, 0 )
   ImageListAdd( hImageList, hMemBitmap )

   DeleteObject( hMemBitmap )
RETURN hImageList

//---------------------------------------------------------------------------------------------------------------------------
METHOD __SetSortArrow(n) CLASS GridColumn
   LOCAL i
   IF ::Parent:IsWindow()
      FOR i := 1 TO LEN( ::Parent:Children )
          ::Parent:Children[i]:__nSortArrow := 0
          ::Parent:Children[i]:DrawHeader()
      NEXT
      ::__nSortArrow := n
      ::DrawHeader()
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------------------------------
METHOD DrawHeader( hDC, nLeft, nRight, x, lPressed, lHot, zLeft, nImgAlign, xRight ) CLASS GridColumn
   LOCAL aAlign, nColor, hOldPen, hOldBrush, hOldFont, n, aRect, nH := 5, nx := 0, aText
   LOCAL nTop, nImgX, nTxColor, nImage := ::xHeaderImageIndex, y := 0
   LOCAL hBorderPen, nColor1, nColor2, nBackColor, nBorder, nShadow, hPenShadow, hPenLight, z, i, nPrevColor, lDC, hBrush, aTxAlign, a, nOffset
   LOCAL nWImg, hMemBitmap, hOldBitmap, hMemDC, nHeaderAlign, nMenuWidth := 0

   IF ::Parent:__GetHeaderHeight() <= 0
      RETURN NIL
   ENDIF

   IF hDC == NIL .AND. nLeft == NIL .AND. nRight == NIL
      RETURN ::Parent:__DisplayData( 1, ::xPosition, 1, ::xPosition,,, lPressed, lHot, .T. )
   ENDIF

   nHeaderAlign := IIF( ::xHeaderAlignment == 0, ::xAlignment, ::xHeaderAlignment )

   DEFAULT nImgAlign TO IIF( ::xImageAlignment > 0, ::xImageAlignment, nHeaderAlign )

   DEFAULT lPressed TO .F.
   DEFAULT lHot     TO .F.
   DEFAULT nLeft    TO ::__HeaderLeft
   DEFAULT nRight   TO ::__HeaderRight
   DEFAULT x        TO ::__HeaderX
   DEFAULT nImage   TO 0
   DEFAULT zLeft    TO nLeft - ::Parent:HorzScrollPos
   DEFAULT xRight   TO nRight

   IF hDC == 0
      hDC := NIL
   ENDIF

   lDC := hDC != NIL
   DEFAULT hDC TO GetDC( ::Parent:hWnd )

   aRect := {nLeft, 0, nRight+1, ::Parent:__GetHeaderHeight()}

   IF ! lDC
      hDC        := GetDC( ::Parent:hWnd )
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, aRect[3], aRect[4] )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ELSE
      hMemDC     := hDC
   ENDIF

   IF ::__nSortArrow > 0
      nx := 18
   ENDIF

   nTxColor   := ::HeaderForeColor
   IF nTxColor == ::__SysHeaderForeColor
      nTxColor := ::System:Colors:BtnText
   ENDIF

   ::__HeaderLeft  := nLeft
   ::__HeaderRight := nRight
   ::__HeaderX     := x

   hOldFont := SelectObject( hMemDC, ::HeaderFont:Handle )

   aText := hb_atokens( ::xText, CRLF )
   aAlign := {0,0}
   aTxAlign := {}
   FOR n := 1 TO LEN( aText )
       a := _GetTextExtentPoint32( hMemDC, aText[n] )
       aAlign[1] := MAX( a[1], aAlign[1] )
       aAlign[2] := MAX( a[2], aAlign[2] )
       AADD( aTxAlign, a )
   NEXT

   y := (::Parent:__GetHeaderHeight() - (aAlign[2]*LEN(aTxAlign)) ) / 2

   nMenuWidth := IIF( ::HeaderMenu != NIL, MENUBTNWIDTH, 0 )

   IF ::Parent:SchemeHeader .AND. ::__SysHeaderBackColor == ::HeaderBackColor
      ::__aVertex[1]:x := aRect[1]-1
      ::__aVertex[1]:y := aRect[2]
      ::__aVertex[2]:x := aRect[3]
      ::__aVertex[2]:y := aRect[4]

      IF ! lPressed
         IF lHot
            nColor1 := ::ColorScheme:ButtonCheckedGradientBegin
            nColor2 := ::ColorScheme:ButtonCheckedGradientEnd
            hBorderPen := ::ColorScheme:Pen:ButtonSelectedBorder
          ELSE
            nColor1 := ::ColorScheme:ButtonSelectedGradientBegin
            nColor2 := ::ColorScheme:ButtonSelectedGradientEnd
            hBorderPen := ::ColorScheme:Pen:ButtonSelectedBorder
         ENDIF
      ELSE
         nColor1 := ::ColorScheme:ButtonPressedGradientBegin
         nColor2 := ::ColorScheme:ButtonPressedGradientEnd
         hBorderPen := ::ColorScheme:Pen:ButtonSelectedBorder
      ENDIF

      ::__aVertex[1]:Red   := GetRValue( nColor1 ) * 256
      ::__aVertex[1]:Green := GetGValue( nColor1 ) * 256
      ::__aVertex[1]:Blue  := GetBValue( nColor1 ) * 256

      ::__aVertex[2]:Red   := GetRValue( nColor2 ) * 256
      ::__aVertex[2]:Green := GetGValue( nColor2 ) * 256
      ::__aVertex[2]:Blue  := GetBValue( nColor2 ) * 256

      hOldPen   := SelectObject( hMemDC, hBorderPen )
      hOldBrush := SelectObject( hMemDC, GetStockObject( NULL_BRUSH ) )

      __GradientFill( hMemDC, ::__aVertex, 2, ::__aMesh, 1, 1 )
      Rectangle( hMemDC, aRect[1]-1, aRect[2]-1, aRect[3], aRect[4] )

   ELSE
      nBorder    := ::HeaderForeColor
      nBackColor := ::HeaderBackColor
      IF lPressed
         nBorder    := ::ColorScheme:MenuItemBorder
         nBackColor := ::ColorScheme:MenuItemSelected
      ENDIF

      nShadow := DarkenColor( nBackColor, 100 )
      nColor  := LightenColor( nBackColor, 100 )

      IF ::Parent:SchemeHeader
         hOldPen := SelectObject( hMemDC, CreatePen( PS_SOLID, 0, nShadow ) )
         hBrush  := CreateSolidBrush( nColor )
       ELSE
         hBrush  := CreateSolidBrush( nBackColor )
      ENDIF
      hOldBrush := SelectObject( hMemDC, hBrush )

      IF ! ::Parent:SchemeHeader
         Rectangle( hMemDC, aRect[1], aRect[2], aRect[3], aRect[4] )
         __Draw3dRect( hMemDC, aRect, nColor, nShadow )
       ELSE
         Rectangle( hMemDC, aRect[1]-1, aRect[2]-1, aRect[3], aRect[4] )
         DeleteObject( SelectObject( hMemDC, hOldPen ) )
         hOldPen := NIL
      ENDIF
   ENDIF
   IF nMenuWidth > 0 .AND. ( lHot .OR. lPressed )
      Rectangle( hMemDC, nRight-nMenuWidth, aRect[2]-1, aRect[3], aRect[4] )
      __DrawDnArrow( hMemDC, {nRight-nMenuWidth, aRect[2], aRect[3], aRect[4]}, .F. )
   ENDIF


   SelectObject( hMemDC, hOldBrush )
   IF hBrush != NIL
      DeleteObject( hBrush )
   ENDIF

   nImgX := 0
   nWImg := 0

   IF nImage > 0 .AND. VALTYPE( ::Parent:ImageList ) == "O"
      nWImg := ::Parent:ImageList:IconWidth
      nTop  := Int( ( aRect[4] - ::Parent:ImageList:IconHeight ) / 2 )

      IF nImgAlign == ALIGN_LEFT
         nImgX := zLeft+1

       ELSEIF nImgAlign == ALIGN_CENTER
         nImgX := Int( zLeft + (((aRect[3]+1-aRect[1])-nWImg)/2) )

       ELSEIF nImgAlign == ALIGN_RIGHT
         nImgX := nRight-nWImg-nMenuWidth

      ENDIF

      nOffset := IIF( nImgX-aRect[1] > 0, 0, aRect[1]-nImgX )

      ::Parent:ImageList:DrawIndirect( hMemDC, nImage, nImgX+nOffset, nTop, nOffset,, ! ::Parent:xEnabled )
   ENDIF

   IF ! ::Parent:xEnabled
      nTxColor := GetSysColor( COLOR_GRAYTEXT )
   ENDIF
   SetBkMode( hMemDC, TRANSPARENT )

   nPrevColor := SetTextColor( hMemDC, nTxColor )
   DEFAULT zLeft TO nLeft
   ::Parent:__DrawMultiText( hMemDC, {aRect[1],aRect[2],xRight,aRect[4]},aText, nRight, zLeft, nWImg + IIF( ::__nSortArrow > 0 .AND. nHeaderAlign == ALIGN_RIGHT, 15, 0 ) + IIF( nHeaderAlign == ALIGN_RIGHT, nMenuWidth, 0 ),;
                                  nHeaderAlign, y, .T., nImgAlign, .f. )

   SetTextColor( hMemDC, nPrevColor )

   SelectObject( hMemDC, hOldFont )
   SelectObject( hMemDC, hOldPen )

   IF nImgAlign <> ALIGN_RIGHT
      nWImg := 0
   ENDIF

   IF ::__nSortArrow > 0 .AND. aRect[3]-nMenuWidth-nWImg > nLeft .AND. aRect[3]-nMenuWidth-nWImg > x + aAlign[1]
      IF ::Parent:SchemeHeader
         hOldPen    := SelectObject( hMemDC, GetStockObject( BLACK_PEN ) )
         FOR n := 1 TO nH
             x := IIF( ::__nSortArrow == 1,n,nH-n+1)
             y := (aRect[4]-nH)/2

             MoveTo( hMemDC, aRect[3] - nWImg - (15-x) - nMenuWidth, y+n-1 )
             LineTo( hMemDC, aRect[3] - nWImg - ( 4+x) - nMenuWidth, y+n-1 )
         NEXT
         SelectObject( hMemDC, hOldPen )
       ELSE
         hPenShadow := CreatePen( PS_SOLID, 0, nShadow )
         hPenLight  := CreatePen( PS_SOLID, 0, nColor )

         hOldPen := SelectObject( hMemDC, hPenLight )
         z := 1
         FOR i := 1 TO 2
             FOR n := 1 TO nH
                 x := IIF( ::__nSortArrow == 1,n,nH-n+1)
                 y := (aRect[4]-nH)/2

                 MoveTo( hMemDC, aRect[3] - nWImg - (15-x) - nMenuWidth, y+n+z )
                 LineTo( hMemDC, aRect[3] - nWImg - ( 4+x) - nMenuWidth, y+n+z )
             NEXT
             SelectObject( hMemDC, hPenShadow )
             z := 0
             aRect[3]--
         NEXT
         SelectObject( hMemDC, hOldPen )

         DeleteObject( hPenShadow )
         DeleteObject( hPenLight )
      ENDIF
   ENDIF
   IF ! lDC
      BitBlt( hDC, 0, 0, aRect[3], aRect[4], hMemDC, 0, 0, SRCCOPY )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
      ReleaseDC( ::Parent:hWnd, hDC )
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------------------------------
METHOD GetSize( nPos ) CLASS GridColumn
   LOCAL n, nLeft := 0
   IF !EMPTY( ::Parent:Children )
      FOR n := 1 TO ::xPosition-1
          nLeft += ::Parent:Children[n]:Width
      NEXT
      SWITCH nPos
          CASE 1
               RETURN nLeft
            CASE 2
               RETURN 0
            CASE 3
               RETURN ::Width
            CASE 4
               RETURN ::Parent:__GetHeaderHeight()
      END
   ENDIF
RETURN 0

METHOD GetRect() CLASS GridColumn
   LOCAL pt, n, nLeft := 0, rc := (struct RECT)
   FOR n := 1 TO ::xPosition-1
       nLeft += ::Parent:Children[n]:Width
   NEXT

   rc:top    := 0
   rc:left   := nLeft
   rc:right  := nLeft + ::Width
   rc:bottom := ::Parent:__GetHeaderHeight()

   pt := (struct POINT)
   pt:x := rc:left
   pt:y := rc:top
   ClientToScreen( ::Parent:hWnd, @pt )
   rc:left := pt:x - ::Parent:HorzScrollPos
   rc:top  := pt:y

   pt:x := rc:right
   pt:y := rc:bottom
   ClientToScreen( ::Parent:hWnd, @pt )
   rc:right  := pt:x - ::Parent:HorzScrollPos
   rc:bottom := pt:y
RETURN rc

//---------------------------------------------------------------------------------------------------------------------------
METHOD __SetAutoEdit( lEdit ) CLASS GridColumn
   IF lEdit != ::AutoEdit
      IF lEdit
         ::Control := {|o| IIF( ValType(::CellData) $ "MC", EditBox(o), MaskEdit(o) ) }
         ::ControlAccessKey := (GRID_CHAR | GRID_LCLICK)
       ELSE
         ::Control := NIL
         ::ControlAccessKey := NIL
      ENDIF
   ENDIF
RETURN lEdit

//---------------------------------------------------------------------------------------------------------------------------
METHOD SetImageIndex(n) CLASS GridColumn
   LOCAL x
   ::xImageIndex := n
   TRY
      IF ::Parent:DesignMode
         ::Parent:Update()
       ELSE
         IF LEN( ::Parent:Children ) >= ::xPosition
            FOR x := 1 TO LEN( ::Parent:__DisplayArray )
                ::Parent:__DisplayArray[x][1][::xPosition][ 2] := n
            NEXT
            ::Parent:__DisplayData( ,::xPosition, , ::xPosition )
         ENDIF
      ENDIF
   CATCH
   END
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD Destroy() CLASS GridColumn
   ::Parent:DeleteColumn( ::Position, .T. )
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD SetAlignment(nAlign) CLASS GridColumn
   LOCAL n
   ::xAlignment := nAlign
   FOR n := 1 TO LEN( ::Parent:__DisplayArray )
       TRY
          ::Parent:__DisplayArray[n][1][::xPosition][ 4] := ::Alignment
        CATCH
       END
   NEXT
   TRY
      ::Parent:__DisplayData( ,::xPosition, , ::xPosition )
    CATCH
   END
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD SetRepresentation(n) CLASS GridColumn
   ::xRepresentation := n
   ::Parent:__DisplayData( ,::xPosition, , ::xPosition )
RETURN Self

//---------------------------------------------------------------------------------------------------------------------------
METHOD SetColor( nInd, nColor ) CLASS GridColumn
   IF nInd == 1
      ::xBackColor := nColor
    ELSE
      ::xForeColor := nColor
   ENDIF
   IF ::Parent:hWnd != NIL .AND. ::xPosition != NIL
      ::Parent:__DisplayData( ,::xPosition, , ::xPosition )
   ENDIF
RETURN Self


//---------------------------------------------------------------------------------------------------------------------------
STATIC FUNCTION Ceiling( x );RETURN( If( x - Int( x ) > 0, Int( x ) + 1, x ) )
