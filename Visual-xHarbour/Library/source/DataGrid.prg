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

/* HEADER parts */
#define HP_HEADERITEM      1
#define HP_HEADERITEMLEFT  2
#define HP_HEADERITEMRIGHT 3
#define HP_HEADERSORTARROW 4

/* HEADER HEADERITEM states */
#define HIS_NORMAL  1
#define HIS_HOT     2
#define HIS_PRESSED 3

#define HEADERSIZEGAP  5

#xtranslate CEIL( <x> ) => ( if( <x> - Int( <x> ) > 0 , Int( <x> )+1, Int( <x> ) ) )
#xtranslate nArrayElement => HB_EnumIndex()

CLASS DataGrid INHERIT Control
   DATA ItemHeight              PUBLISHED INIT 19
   DATA FullRowSelect           PUBLISHED INIT FALSE
   DATA ShadowRow               PUBLISHED INIT TRUE
   DATA GridColor               PUBLISHED
   DATA AutoHorzScroll          PUBLISHED INIT TRUE
   DATA AutoVertScroll          PUBLISHED INIT TRUE
   DATA FreezeColumn            PUBLISHED INIT 0

   DATA ShowHeaders             PUBLISHED INIT .T.
   DATA HeaderHeight            PUBLISHED INIT 20
   DATA ShowSelection           PUBLISHED INIT .T.
   DATA ShowSelectionBorder     PUBLISHED INIT .T.

   PROPERTY ImageList  GET __ChkComponent( Self, ::xImageList )
   PROPERTY DataSource GET __ChkComponent( Self, ::xDataSource ) SET __SetDataSource 

   PROPERTY ShowGrid      READ xShowGrid    WRITE InvalidateRect    DEFAULT .T.
   PROPERTY Striping      READ xStriping    WRITE InvalidateRect    DEFAULT .F.

   DATA ConvertOem              PUBLISHED INIT .F.
   DATA HighlightSysColor       EXPORTED
   DATA HighlightTextSysColor   EXPORTED
   DATA GridSysColor            EXPORTED
   DATA HighlightColor          PUBLISHED
   DATA HighlightTextColor      PUBLISHED
   DATA Columns                 PUBLISHED
   DATA AllowDragRecords        PUBLISHED INIT .F.
   DATA ExtVertScrollBar        PUBLISHED INIT .F.
   DATA MultipleSelection       PUBLISHED INIT .F.

   DATA ColPos                  EXPORTED INIT 1
   DATA RowPos                  EXPORTED INIT 1
   DATA HitTop                  EXPORTED
   DATA HitBottom               EXPORTED
   DATA RowCountVisible         EXPORTED
   DATA RowCountUsable          EXPORTED
   DATA RowHeight               EXPORTED
   DATA bRowChanged             EXPORTED
   DATA aSelected               EXPORTED INIT {}

   ACCESS RowCount              INLINE LEN( ::__DisplayArray )
   ACCESS ColCount              INLINE LEN( ::Children )
   ACCESS HorzScrollPos         INLINE ABS( ::__HorzScrolled )
   ACCESS VertScrollPos         INLINE ABS( ::__VertScrolled )
   ACCESS AutoUpdate            INLINE IIF( ::__nUpdtTimer==0, .F., .T. )
   ASSIGN AutoUpdate(n)         INLINE ::__nUpdtTimer := n, IIF( n>0, ::SetTimer( 15, n*1000 ), ::KillTimer(15) )
   ACCESS Record                INLINE ::__GetPosition()

   DATA __HScrollUnits          PROTECTED INIT 15
   DATA __HorzScroll            PROTECTED INIT FALSE
   DATA __VertScroll            PROTECTED INIT FALSE
   DATA __PrevSource            PROTECTED
   DATA __lCreated              PROTECTED INIT .F.
   DATA __bGoTop                PROTECTED
   DATA __bGoBottom             PROTECTED
   DATA __bRecNo                PROTECTED
   DATA __LinePen               PROTECTED
   DATA __DataHeight            PROTECTED
   DATA __DataWidth             PROTECTED INIT 0
   DATA __VertScrolled          PROTECTED INIT 1
   DATA __HorzScrolled          PROTECTED INIT 0
   DATA __DisplayArray          PROTECTED AS ARRAY INIT {}
   DATA __InactiveHighlight     PROTECTED
   DATA __InactiveHighlightText PROTECTED
   DATA __CurControl            PROTECTED
   DATA __TrackColumn           PROTECTED
   DATA __DragColumn            PROTECTED INIT 0
   DATA __lResizing             PROTECTED INIT FALSE
   DATA __lMoving               PROTECTED INIT FALSE
   DATA __nUpdtTimer            PROTECTED INIT 0
   DATA __CheckPos              PROTECTED
   DATA __nScrolled             PROTECTED INIT 0
   DATA __lSizeMouseDown        PROTECTED INIT .F.
   DATA __lMoveMouseDown        PROTECTED INIT .F.
   DATA __SelCol                PROTECTED INIT 0
   DATA __SelWidth              PROTECTED INIT 0
   DATA __SelLeft               PROTECTED INIT 0
   DATA __hDragImageList        PROTECTED
   DATA __prevDrag              PROTECTED INIT 0
   DATA __prevHotRow            PROTECTED INIT 0
   DATA __prevHotCol            PROTECTED INIT 0
   DATA __lHot                  PROTECTED INIT .F.
   DATA __nVPage                PROTECTED
   DATA __nVMax                 PROTECTED
   DATA __nVPos                 PROTECTED
   DATA __nHPage                PROTECTED
   DATA __nHMax                 PROTECTED
   DATA __nHPos                 PROTECTED
   DATA __hDragRecImage         PROTECTED
   DATA __nDragTop              PROTECTED INIT 0
   DATA __aSel                  PROTECTED INIT {}

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
   METHOD Edit()         INLINE ::PostMessage( WM_KEYDOWN, VK_RETURN ), Self
   
   METHOD Skip( n )
   METHOD GetControl()   INLINE ::__CurControl

   METHOD OnRowChanging()     VIRTUAL
   METHOD OnColChanging()     VIRTUAL
   METHOD OnRowChanged()     VIRTUAL
   METHOD OnColChanged()     VIRTUAL
   METHOD OnClick()          VIRTUAL

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
   METHOD __FillRow()
   METHOD __GoToRec()
   METHOD __SkipRecords()
   METHOD __ResetControl()
   METHOD __SetColPos()
   METHOD __SetColWidth()
   METHOD __GetDataValue(n)  INLINE &( ::Children[n]:Data )
   METHOD __CheckData()

   METHOD Refresh() INLINE ::InvalidateRect()
   METHOD ColFromPos()
   
   MESSAGE OnSize             METHOD __OnSize()
   MESSAGE OnParentSysCommand METHOD __OnParentSysCommand()
   MESSAGE OnTimer            METHOD __OnTimer()
   
   METHOD OnPaint()
   METHOD OnHorzScroll()
   METHOD OnVertScroll()

   METHOD SaveLayout()
   METHOD RestoreLayout()

   METHOD OnNCDestroy()
   METHOD OnKeyDown()
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnChar()
   METHOD OnKillFocus()
   METHOD OnSetFocus()
   METHOD OnGetDlgCode() INLINE DLGC_WANTMESSAGE
//   METHOD OnGetDlgCode( msg ) INLINE IIF( msg != NIL .AND. msg:message == WM_KEYDOWN .AND. msg:wParam IN {VK_ESCAPE}, NIL, DLGC_WANTMESSAGE )
   METHOD OnLButtonDblClk()
   METHOD OnItemChanged()
   METHOD OnEraseBkGnd()
   METHOD OnHeaderClick()
   METHOD __DrawRepresentation()
   METHOD OnMouseMove()
   METHOD GetPosition()
   METHOD OnMouseWheel()
   METHOD CreateDragImage()
ENDCLASS

//----------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS DataGrid
   DEFAULT ::__xCtrlName TO "DataGrid"
   ::ClsName                 := "DataGrid"
   ::BackSysColor            := GetSysColor( COLOR_WINDOW )
   ::BackColor               := GetSysColor( COLOR_WINDOW )
   ::Style                   := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Super:Init( oParent )
   ::Width                   := 340
   ::Height                  := 240
   ::StaticEdge   := .T.
   IF ::__ClassInst != NIL
      ::__ClassInst:xStaticEdge := .T.
   ENDIF

   ::SmallCaption            := TRUE
   ::EmptyLeft               := 0
   ::__IsStandard            := .F.
   ::IsContainer             := .F.
   ::GridSysColor            := RGB(196,192,192)
   ::GridColor               := RGB(196,192,192)
   ::ForeSysColor            := ::System:Colors:WindowText
   ::ForeColor               := ::System:Colors:WindowText
   ::HighlightColor          := ::System:Colors:Highlight
   ::HighlightTextColor      := ::System:Colors:HighlightText
   ::HighlightSysColor       := ::System:Colors:Highlight
   ::HighlightTextSysColor   := ::System:Colors:HighlightText
   ::__InactiveHighlight     := RGB(224,224,224)
   ::__InactiveHighlightText := ::System:Colors:WindowText
   ::__lCreateAfterChildren  := .T.
   ::DeferRedraw             := FALSE
   IF ::__ClassInst != NIL
      AINS( ::Events, 1, {"Navigation", {;
                                          { "OnRowChanging", "", "" },;
                                          { "OnRowChanged",  "", "" },;
                                          { "OnColChanging", "", "" },;
                                          { "OnColChanged",  "", "" } } }, .T. )
      AADD( ::Events[3][2], { "OnQueryBackColor", "", "" } )
      AADD( ::Events[3][2], { "OnQueryForeColor", "", "" } )
      //AADD( ::Events[3][2], { "GetItemHeight", "", "" } )
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------

METHOD Create() CLASS DataGrid
   LOCAL oColumn, nAlign

   ::__SetBlocks()
   ::__LinePen := CreatePen( PS_SOLID, 0, ::GridColor )

   ::xWidth  := MIN( ::xWidth,  ::Parent:ClientWidth  - ::Left - 5 )
   ::xHeight := MIN( ::xHeight, ::Parent:ClientHeight - ::Top - 5 )

   Super:Create()
   ::__DataWidth := 0
   FOR EACH oColumn IN ::Children
      IF oColumn:ClsName == "GridColumn"
         ::__DataWidth += oColumn:Width
      ENDIF
   NEXT

   ::__DataHeight   := ::ClientHeight - ::__GetHeaderHeight()

   ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )
   ::__Update()

   ::AutoUpdate := ::__nUpdtTimer
   ::__lCreated := .T.
   IF !EMPTY( ::Caption )
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
   ENDIF
   ::__UpdateHScrollBar()
   ::__UpdateVScrollBar()
RETURN Self

METHOD __GetPosition() CLASS DataGrid
   LOCAL nPos := 0
   IF ::DataSource != NIL
      IF ::DataSource:ClsName == "MemoryTable" .OR. ::DataSource:Driver IN { "SQLRDD", "SQLEX" } .OR. ::ExtVertScrollBar
         nPos := ::DataSource:OrdKeyNo()
       ELSE
         nPos := Int( Round( ::DataSource:OrdKeyRelPos()*::DataSource:Recno(), 0 ) )
      ENDIF
   ENDIF
RETURN nPos

METHOD GetPosition() CLASS DataGrid
RETURN ASCAN( ::__DisplayArray, {|a|a[2]==::DataSource:Recno()} )

METHOD OnMouseWheel( nwParam, nlParam ) CLASS DataGrid
   LOCAL nLines, nScroll, nDelta, nPage, pt, rc, n
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
   IF nLines == 0 .AND. ::__xCtrlName == "DataGrid"
      nLines := 3
   ENDIF
   
   IF nLines > 0
      nDelta  := GETWHEELDELTA( nwParam )
      nScroll := WM_VSCROLL
      nPage   := IIF( ::__nVPage != NIL, ::__nVPage, ::ClientHeight )
      IF PtInRect( rc, pt )
         nScroll := WM_HSCROLL
         nPage   := ::__nHPage
      ENDIF

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
RETURN 0

METHOD CreateDragImage(y) CLASS DataGrid
   LOCAL hImageList, hMemBitmap, nTop
   nTop       := ::__GetHeaderHeight() + ( ::ItemHeight*(::RowPos-1) )
   hMemBitmap := GetScreenBitmap( { 0, nTop, ::ClientWidth, nTop + ::ItemHeight }, ::hWnd )
   hImageList := ImageListCreate( ::ClientWidth, ::ItemHeight, ILC_COLOR32 | ILC_MASK, 1, 0 )
   ImageListAdd( hImageList, hMemBitmap )
   DeleteObject( hMemBitmap )
   ::__nDragTop := y-nTop
RETURN hImageList

METHOD OnMouseMove(wParam,x,y) CLASS DataGrid
   LOCAL nRow, n, nWidth, pt, nClickCol, nDrag, nCol, nTop
   ::Super:OnMouseMove(wParam,x,y)
   IF ::__CurControl != NIL .AND. ::__CurControl:ClsName == "Edit" .AND. ::__CurControl:Button
      ::__CurControl:RedrawWindow(,, RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
   ENDIF

   IF ::ShowHeaders 

      IF !::__lSizeMouseDown .AND. !::__lMoveMouseDown
         IF ( n := Ceiling( (y-::__GetHeaderHeight() ) / ::ItemHeight ) ) <= 0
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
               ::Cursor := NIL//::System:Cursor:LinkSelect
            ENDIF
          
          ELSE
            IF wParam == MK_LBUTTON .AND. ::AllowDragRecords .AND. ::__hDragRecImage != NIL
               nTop := y + ::__GetHeaderHeight() - ::__nDragTop
               ImageListDragMove( 0, nTop )
            ENDIF
            ::Cursor := NIL
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

         IF nDrag > 0 .AND. ::__DragColumn != nDrag .AND. nDrag != ::__prevDrag
            ImageListDragShowNolock( .F. )
            IF ::__DragColumn > 0
               ::Children[ ::__DragColumn ]:DrawHeader( ::Drawing:hDC )
            ENDIF
            
            ::__DragColumn := nDrag
            
            IF ::Children[ nDrag ]:AllowDrag
               ::Children[ nDrag ]:DrawHeader( ::Drawing:hDC,,,, .T. )
            ENDIF
            
            ImageListDragShowNolock( .T. )
            ::__prevDrag := nDrag
         ENDIF
         IF ::__SelCol > 0
            n := x - ::__SelLeft - ::Children[::__SelCol]:Width + 1
            ImageListDragMove( n, IIF( !EMPTY( ::Caption ) .AND. ::SmallCaption, ::CaptionHeight, 0 )+1 )
         ENDIF
      ENDIF
   ENDIF
RETURN NIL
   

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
RETURN Self

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
RETURN Self

METHOD __CheckData( cData ) CLASS DataGrid
   LOCAL lFailed := .F.
   TRY
      cData := &cData
    catch
      lFailed := .T.
   END
RETURN lFailed
//----------------------------------------------------------------------------------

METHOD __SetDataSource( oSource ) CLASS DataGrid
   LOCAL n, hWnd, oPrevSource, Column, lUpdate := .F.

   oSource := __ChkComponent( Self, oSource )

   IF ( hWnd := GetWindow( ::hWnd, GW_CHILD | GW_HWNDFIRST ) ) > 0
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

   IF oSource != NIL
      ::__SetBlocks()
      oSource:bOnFileNameChanged := {|o| Self:__ResetDataSource( o ) }
      oSource:bOnFileClosed := {|o| Self:__SetDataSource( NIL ) }

      IF ::MultipleSelection .AND. EMPTY( ::aSelected )
         ::aSelected := { oSource:Recno() }
         ::__aSel    := { { oSource:Recno(), .F., .F., -1 } }
      ENDIF

    ELSE
      ::InvalidateRect()
   ENDIF
   ::__DataHeight   := ::ClientHeight - ::__GetHeaderHeight()
   IF oSource == NIL
      ::__DataWidth := 0
      ::__UpdateHScrollBar( .T., .T. )
   ENDIF
   //::__PrevSource := oSource
RETURN Self

METHOD __ResetDataSource( oSource ) CLASS DataGrid
   ::__SetDataSource( oSource )
   IF ::__ClassInst != NIL
      ::AutoAddColumns()
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------

METHOD __GoToRec( nRec ) CLASS DataGrid
   IF nRec != NIL
      ::DataSource:Goto( nRec )
      ::HitTop    := ::DataSource:Bof()
      ::HitBottom := ::DataSource:eof()
   ENDIF
RETURN Self

METHOD __SkipRecords( n ) CLASS DataGrid
   IF n <> 0
      ::DataSource:Skip( n )
   ENDIF
   ::HitTop    := ::DataSource:Bof()
   ::HitBottom := ::DataSource:eof()
RETURN Self

//----------------------------------------------------------------------------------

METHOD __SetBlocks() CLASS DataGrid

   IF !VALTYPE( ::DataSource ) == "O" .OR. ::DataSource:Fields == NIL .OR. !::DataSource:IsOpen
      RETURN NIL
   ENDIF

   ::HitTop    := ::DataSource:Bof()
   ::HitBottom := ::DataSource:eof()
   ::__VertScrolled := ::Record
   ::__bGoTop    :={||::DataSource:Gotop(),;
                      ::HitTop   := ::DataSource:Bof(),;
                      ::HitBottom:= ::DataSource:eof() }

   ::__bGoBottom :={|n| ::DataSource:GoBottom(),;
                      ::HitTop    := ::DataSource:Bof(),;
                      ::HitBottom := ::DataSource:eof() }

   ::__bRecNo    := {|| ::DataSource:RecNo() }
RETURN Self

//----------------------------------------------------------------------------------

METHOD __OnTimer( Sender ) CLASS DataGrid
   ::KillTimer( 15 )
   IF Sender:wParam == 15
      ::Update()
   ENDIF
   ::SetTimer( 15, ::__nUpdtTimer*1000 )
RETURN 0

//----------------------------------------------------------------------------------

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
   LOCAL oColumn, hOldBrush, oCol, n := 1
   DEFAULT nCol TO ::ColPos

   IF nCol <= 0 .OR. nCol > ::ColCount
      RETURN(.F.)
   ENDIF

   ::Children[ nCol ]:RemoveProperty()
   
   IF ::Children[ nCol ]:TreeItem != NIL
      ::Children[ nCol ]:TreeItem:Delete()
      ::Children[ nCol ]:TreeItem := NIL
   ENDIF
   
   aDel( ::Children, nCol, .T. )

   ::__DataWidth := 0
   AEVAL( ::Children, {|o|o:xPosition := n++, ::__DataWidth += o:Width} )

   ::__Update( lDisplay )
   ::__UpdateHScrollBar()
   IF ::DataSource != NIL
      ::__DisplayData()
   ENDIF
   ::InvalidateRect()
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
   LOCAL n, i, nDiff, lPrev := .F.
   IF nCol != NIL .AND. LEN( ::Children ) >= nCol
      IF ::Children[ nCol ]:xWidth == 0 .AND. nCol > 1
         lPrev := .T.
      ENDIF
      nDiff := nWidth - ::Children[ nCol ]:xWidth
      ::Children[ nCol ]:xWidth += nDiff
      ::__DataWidth += nDiff
      FOR i := nCol+1 TO ::ColCount
          FOR n := 1 TO MIN( ::RowCount, ::RowCountVisible )
              ::__DisplayArray[n][1][i][6] += nDiff
          NEXT
      NEXT
      
      IF ( nCol > ::FreezeColumn )
         IF lPrev
            ::__DisplayData(, nCol-1,, nCol )
          ELSE
            ::__DisplayData(, nCol, )
         ENDIF
       ELSE
         ::__DisplayData()
      ENDIF
      ::InvalidateRect( { ::__DataWidth, 0, ::ClientWidth, ::ClientHeight } )
      ::__UpdateHScrollBar(.T.)
   ENDIF
   ::__DataWidth := 0
   AEVAL( ::Children, {|o| ::__DataWidth += o:Width} )
RETURN 1

//---------------------------------------------------------------------------------

METHOD __ResetControl() CLASS DataGrid
   IF ::__CurControl != NIL
      IF ::Children[ ::ColPos ]:ControlValid != NIL
         ::__CurControl:IsValid := FALSE
      ENDIF
      ::KeepActiveCaption  := .F.
      ::__CurControl:Destroy()
      ::DataSource:UnLock()
      ::__CurControl:= NIL
      RETURN TRUE
   ENDIF
RETURN FALSE

//---------------------------------------------------------------------------------

METHOD __OnSize() CLASS DataGrid
   LOCAL aCount, lRefresh := .T.
   IF ::DataSource != NIL .AND. !::DataSource:IsOpen
      RETURN 0
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
   LOCAL aRect, nRowsV, nRowsU, j, nRec, nPos, n, xRec, nSkiped, nDel, nDeleted
   LOCAL nLast, nFirst, nRows
   LOCAL nDelTop, aScroll, aClip, aCount, nHeight

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
             AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )
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

                AINS( ::__DisplayArray, 1, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() }, TRUE )
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
RETURN 0

//----------------------------------------------------------------------------------

METHOD OnChar( nKey ) CLASS DataGrid
   IF nKey != 27 .AND. nKey != 9
      ::__Edit(1, 0, 0, GRID_CHAR, nKey )
   ENDIF
   IF nKey == 13 .AND. ::__CurControl == NIL .AND. ::Action != NIL
      __Evaluate( ::Action, Self )
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------

METHOD OnLButtonDblClk( nwParam, xPos, yPos ) CLASS DataGrid
   LOCAL nClickRow  := Ceiling((yPos-::__GetHeaderHeight()) /::ItemHeight)
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
   LOCAL n, nPos, nClickCol := 1, aDrag, aMove, i, nRec, aData := {}

   ::ReleaseCapture()
   IF ::__hDragImageList != NIL
      ImageListDestroy( ::__hDragImageList )
      ::__hDragImageList := NIL
      ImageListEndDrag()
   ENDIF

   nPos := Ceiling( (yPos-::__GetHeaderHeight() ) / ::ItemHeight )
   nPos := MIN( nPos, ::RowCountUsable )
   nPos := MAX( nPos, 1 )
   IF nPos <> ::RowPos .AND. ::__hDragRecImage != NIL
   
      ImageListDestroy( ::__hDragRecImage )
      ::__hDragRecImage := NIL
      ImageListEndDrag()
      
      aDrag := ARRAY( LEN( ::DataSource:Structure ) )
      aMove := ARRAY( LEN( ::DataSource:Structure ) )
      nRec  := ::DataSource:Recno()
      
      aEval( aDrag, {|a,n| aDrag[n] := ::DataSource:FieldGet(n) } )

      IF ::DataSource:FileLock()
         IF nPos > ::RowPos // Drag down

            FOR i := ::RowPos+1 TO nPos
                ::DataSource:Skip()
                aEval( aMove, {|a,n| aMove[n] := ::DataSource:FieldGet(n) } )
                AADD( aData, ACLONE( aMove ) )
            NEXT
            AADD( aData, ACLONE( aDrag ) )
            ::DataSource:Goto( nRec )

            FOR i := 1 TO LEN( aData )
                aEval( aData[i], {|a,n| ::DataSource:FieldPut(n, aData[i][n] ) } )
                ::DataSource:Skip()
            NEXT
            ::DataSource:Skip(-1)

          ELSE //Drag up

            FOR i := ::RowPos-1 TO nPos STEP -1
                ::DataSource:Skip(-1)
                aEval( aMove, {|a,n| aMove[n] := ::DataSource:FieldGet(n) } )
                AADD( aData, ACLONE( aMove ) )
            NEXT
            AADD( aData, ACLONE( aDrag ) )
            ::DataSource:Goto( nRec )

            FOR i := 1 TO LEN( aData )
                aEval( aData[i], {|a,n| ::DataSource:FieldPut(n, aData[i][n] ) } )
                ::DataSource:Skip(-1)
            NEXT
            IF nPos > 1
               ::DataSource:Skip()
            ENDIF
          
         ENDIF
         ::DataSource:Unlock()
      ENDIF
      ::Update()
   ENDIF

   IF ::__lMoveMouseDown .AND. ::__DragColumn > 0 .AND. LEN( ::Children ) > 0
      IF ::__DragColumn <> ::__SelCol .AND. ::Children[ ::__DragColumn ]:AllowDrag
         ::Children[ ::__SelCol ]:Position := ::__DragColumn
      ENDIF
      ::Children[ ::__DragColumn ]:DrawHeader( ::Drawing:hDC )
   ENDIF
   
   IF !::__lSizeMouseDown .AND. ::__DragColumn == 0 .AND. ::__SelCol > 0
      ::OnHeaderClick( ::__SelCol )
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
METHOD OnLButtonDown( nwParam, xPos, yPos ) CLASS DataGrid
   LOCAL aRect, nCol, nRow, n, lRes, nWidth, pt, i
   LOCAL nClickRow
   LOCAL nClickCol, lShift, lCtrl
   LOCAL lLineChange:=.F.

   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )

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
               IF !::Children[ nClickCol ]:AllowDrag
                  RETURN NIL
               ENDIF
               
               ::ReleaseCapture()

               ::__SelLeft := xPos - ::__SelWidth

               ::__prevDrag := nClickCol

               ::__lMoveMouseDown := .T.
               ::__hDragImageList := ::Children[ nClickCol ]:CreateDragImage( ::__SelWidth - ::Children[nClickCol]:Width )
               ImageListBeginDrag( ::__hDragImageList, 0, 0, 0 )
               ImageListDragEnter( ::hWnd, ::__SelWidth-::Children[nClickCol]:Width+1, IIF( !EMPTY( ::Caption ) .AND. ::SmallCaption, ::CaptionHeight, 0 )+1 )
            ENDIF

         ENDIF
         RETURN NIL
      ENDIF
    ELSEIF nClickRow == ::RowPos .AND. ::AllowDragRecords
      ::__hDragRecImage := ::CreateDragImage(yPos)
      ImageListBeginDrag( ::__hDragRecImage, 0, 0, 0 )
      ImageListDragEnter( ::hWnd, 0, ::__GetHeaderHeight() + ( ::ItemHeight*(::RowPos) )+1 )
   ENDIF

   IF LEN( ::__DisplayArray ) == 0 .OR. ::__DisplayArray[1] == NIL
      RETURN NIL
   ENDIF

   IF nClickRow > LEN( ::__DisplayArray ) .OR. xPos > ::ClientWidth .OR. xPos > ::__DataWidth
      ::SetFocus()
      RETURN NIL
   ENDIF

   IF ::MultipleSelection
      lShift := CheckBit( GetKeyState( VK_SHIFT ) )
      lCtrl  := CheckBit( GetKeyState( VK_CONTROL ) )
      IF !lShift .AND. !lCtrl
         ::aSelected := {::__DisplayArray[ nClickRow ][2] }
         ::__aSel    := { { ::__DisplayArray[ nClickRow ][2], lShift, lCtrl, -1 } }
         ::Update()
       ELSE
         IF lCtrl
            IF ( n := ASCAN( ::aSelected, ::__DisplayArray[ nClickRow ][2] ) ) == 0
               AADD( ::aSelected, ::__DisplayArray[ nClickRow ][2] )
               AADD( ::__aSel,  { ::__DisplayArray[ nClickRow ][2], lShift, lCtrl, -1 } )
             ELSE
               ADEL( ::aSelected, n, .T. )
               ADEL( ::__aSel, n, .T. )
            ENDIF
            ::Update()

          ELSEIF lShift
            IF nClickRow > ::RowPos
               FOR i := ::RowPos TO nClickRow
                   IF ( n := ASCAN( ::aSelected, ::__DisplayArray[i][2] ) ) == 0
                      AADD( ::aSelected, ::__DisplayArray[i][2] )
                      AADD( ::__aSel,  { ::__DisplayArray[i][2], lShift, lCtrl, -1 } )
                    ELSE
                      ADEL( ::aSelected, n, .T. )
                      ADEL( ::__aSel, n, .T. )
                   ENDIF
               NEXT
             ELSE
               FOR i := ::RowPos TO nClickRow STEP -1
                   IF ( n := ASCAN( ::aSelected, ::__DisplayArray[i][2] ) ) == 0
                      AADD( ::aSelected, ::__DisplayArray[i][2] )
                      AADD( ::__aSel,  { ::__DisplayArray[i][2], lShift, lCtrl, -1 } )
                    ELSE
                      ADEL( ::aSelected, n, .T. )
                      ADEL( ::__aSel, n, .T. )
                   ENDIF
               NEXT
            ENDIF
            ::Update()
         ENDIF
      ENDIF
   ENDIF

   nClickCol := 1
   FOR n := 1 TO LEN( ::__DisplayArray[1][1] )
       IF ::__DisplayArray[1][1][n][6] <= xPos-::__HorzScrolled
          nClickCol := n
       ENDIF
   NEXT

   nCol := ::ColPos
   nRow := ::RowPos

   ::__ResetControl()
      
   IF nClickRow != ::RowPos
      lRes := ::OnRowChanging()
      DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
      DEFAULT lRes TO .T.
      IF !lRes
         RETURN NIL
      ENDIF
   ENDIF
   IF nClickCol != ::ColPos
      lRes := ::OnColChanging()
      DEFAULT lRes TO ExecuteEvent( "OnColChanging", Self )
      DEFAULT lRes TO .T.
      IF !lRes
         RETURN NIL
      ENDIF
   ENDIF

//   nClickCol  := aScan( ::__DisplayArray[ 1 ][1], {|a| a[6] > xPos-::__HorzScrolled} )-1

   ::RowPos := ASCAN( ::__DisplayArray, {|a|a[2]==::DataSource:Recno()} )
   nRow := ::RowPos
/*
   IF ::hWnd != GetFocus()
      ::SetFocus()

      IF ::IsCovered
         ::__DisplayData()
       ELSE
         ::__DisplayData( ::RowPos,, ::RowPos )
      ENDIF
   ENDIF
*/
   IF nClickCol == -1
      nClickCol := ::ColCount
   ENDIF

   IF nClickRow > ::RowCountUsable
      IF nClickRow > ::RowCount
         nClickRow := ::RowCountUsable
      ENDIF
      lLineChange := TRUE
   ENDIF

   //::Record := ::__DisplayArray[ nClickRow ][2]
   //::__GoToRec( ::Record )

   ::__GoToRec( ::__DisplayArray[ nClickRow ][2] )
   //::Record := ::__DisplayArray[ nClickRow ][2]

   ::RowPos := nClickRow
   
   IF nCol > 0
      IF !::FullRowSelect
         ::ColPos := nClickCol
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
       ELSE
         ::ColPos := nClickCol
//         ::ColPos := ASCAN( ::__DisplayArray[1][1], {|a|a[6]> xPos} )-1
      ENDIF
   ENDIF
   IF nRow > 0
      ::__DisplayData( nRow, , nRow, )
   ENDIF
   IF lLineChange
      IF ::DataSource:ClsName == "MemoryTable" .OR. ::DataSource:Driver IN { "SQLRDD", "SQLEX" } .OR. ::ExtVertScrollBar
         ::__ScrollUp()
      ENDIF
      ::RowPos := ::RowCountUsable
   ENDIF
   IF ::hWnd != GetFocus()
      ::SetFocus()
   ENDIF
   ::OnClick( ::ColPos, ::RowPos )
   IF nRow != ::RowPos
      IF ::bRowChanged != NIL
         EVAL( ::bRowChanged, Self )
      ENDIF
      ::OnRowChanged()
   ENDIF
   ExecuteEvent( "OnRowChanged", Self )
   ExecuteEvent( "OnClick", Self )

   ::__DisplayData( ::RowPos, , nClickRow,  )
   ::__Edit( 1, xPos, yPos, GRID_LCLICK )
RETURN NIL

//----------------------------------------------------------------------------------
/*
   Multiselection must check for shift and control keys, The selection WILL DEPEND on the programmer
   wich will have to save a flag in the DataBase / Array for DataGrid to know what to HighlightColor as
   selected
*/
//----------------------------------------------------------------------------------
METHOD __OnParentSysCommand( Sender )
   IF ::Parent:wParam == SC_CLOSE
      ::KillTimer(15)
   ENDIF
   IF ::__CurControl != NIL
      IF ::__CurControl:Validating
         RETURN 0
      ENDIF
      ::__CurControl:IsValid := FALSE
      ::KeepActiveCaption := .F.
      ::__CurControl:Destroy()
      ::DataSource:UnLock()
      ::__CurControl:=NIL
   ENDIF
   ::__DisplayData( ::RowPos,, ::RowPos )
RETURN NIL

METHOD OnKillFocus() CLASS DataGrid
   ::Super:OnKillFocus()
   IF ::__CurControl != NIL .AND. GetFocus()!=::__CurControl:hWnd
      IF ::__CurControl:Validating
         RETURN 0
      ENDIF
      ::__CurControl:IsValid := FALSE
      ::KeepActiveCaption := .F.
      ::__CurControl:Destroy()
      ::DataSource:UnLock()
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
   LOCAL nRec, n, x, lFirst, nColumns

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
             AADD( ::__DisplayArray,{ ARRAY( nColumns ), ::DataSource:Recno(), ::ItemHeight } )
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

   LOCAL nKey, h, nCount, lShift, x, aScroll, aClip, nPor, nPos, lVUpdate := FALSE

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
           ::AutoUpdate := ::__nUpdtTimer
           Set( _SET_INSERT, ! Set( _SET_INSERT ) )
           RETURN 0

      CASE nwParam==VK_TAB
           lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
           IF ( h := GetNextDlgTabItem( ::Form:hWnd , ::hWnd, lShift ) ) # 0
              SetFocus(h)
              PostMessage( ::Form:hWnd, WM_NEXTDLGCTL, h, MAKELPARAM( 1, 0 ) )
              ::AutoUpdate := ::__nUpdtTimer
              RETURN 0
           ENDIF

      CASE nwParam==VK_UP
           nKey := GRID_UP
           ::Up( nCount )
           lVUpdate := .T.

      CASE nwParam==VK_DOWN
           nKey := GRID_DOWN
           ::Down( nCount )
           lVUpdate := .T.

      CASE nwParam == VK_NEXT
           ::PageDown( nCount )
           lVUpdate := .T.

      CASE nwParam == VK_PRIOR
           ::PageUp( nCount )
           lVUpdate := .T.

      CASE nwParam == VK_END
           ::End( .F. )
           lVUpdate := .T.

      CASE nwParam == VK_HOME
           ::Home()
           lVUpdate := .T.

      CASE nwParam == VK_LEFT
           nKey := GRID_LEFT
           ::ArrowLeft()

      CASE nwParam == VK_RIGHT
           nKey := GRID_RIGHT
           ::ArrowRight()

      CASE nwParam == VK_SPACE
           nKey := GRID_SPACE

   ENDCASE
   IF lVUpdate
      ::__VertScrolled := ::Record - ::RowPos + 1
      ::__UpdateVScrollBar()
   ENDIF
   IF nKey != NIL .AND. nKey != GRID_DOWN
      ::__Edit(0, 0, 0, nKey )
   ENDIF
   ::AutoUpdate := ::__nUpdtTimer
RETURN NIL

//----------------------------------------------------------------------------------

METHOD OnEraseBkGnd() CLASS DataGrid
RETURN IIF( LEN( ::Children ) > 0, 1, NIL )

METHOD OnPaint( hDC, hMemDC ) CLASS DataGrid
   LOCAL hMemBitmap, hOldBitmap, lPaint
   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ENDIF
   lPaint := ::__DisplayData(,,,, hMemDC )
   IF hDC != NIL
      IF lPaint
         BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )
      ENDIF
      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF
RETURN 0

//----------------------------------------------------------------------------------
METHOD __DisplayData( nRow, nCol, nRowEnd, nColEnd, hMemDC ) CLASS DataGrid
   LOCAL n, i, cData, x, y, nY, nRec, nRecno, lHide, aText, lSelected, nHScroll, iRight, iLeft, zLeft
   LOCAL nLeft, nTop, nRight, nBottom, hOldFont, hOldPen, nWImg, nHImg, nInd, nAlign, aAlign, nOldAlign, aGrid, lFreeze, nHeaderRight
   LOCAL nBkCol, nTxCol, xLeft, nStatus, nItemHeight, xTop
   LOCAL nDif, nAve, aTextExt, nFocRow, aData, z, lDrawControl, nCtrl, nRep, aRect, lDis := !::IsWindowEnabled()
   LOCAL iLen, lHighLight, lBorder, hBrush, nLine, nRecPos := 0, hPen, nImgX
   IF LEN( ::Children ) == 0 .OR. ::hWnd == NIL .OR. !IsWindow( ::hWnd ) .OR. ::hWnd == 0
      RETURN .F.
   ENDIF

   lFreeze := ::FreezeColumn > 0
   IF ::RowPos < 1
      ::RowPos := 1
   ENDIF
   IF !EMPTY( ::__DisplayArray ) .AND. ::DataSource != NIL
      nRecno   := ::DataSource:Recno()
    ELSE
      nRecno := 0
   ENDIF
   DEFAULT hMemDC TO ::Drawing:hDC
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
   
   aTextExt := _GetTextExtentPoint32( hMemDC, 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' )
   FOR nLine := nRow TO nRowEnd
       IF nLine <= LEN( ::__DisplayArray ) .AND. ::__DisplayArray[nLine] == NIL
          LOOP
       ENDIF
       nLeft   := xLeft
       iLeft   := 0
       
       nTop    := ::__GetHeaderHeight() + ( ( nLine-1 ) * ::ItemHeight )
       nBottom := nTop + ::ItemHeight - 1
       lSelected := .F.
       nFocRow := 99999
       

       IF nLine <= LEN( ::__DisplayArray )
          
          nRec    := ::__DisplayArray[nLine][2]
          
          IF ::Striping
             ::DataSource:Goto( nRec )
             nRecPos := ::DataSource:OrdKeyNo()
             ::DataSource:Goto( nRecno )
          ENDIF
          
          IF nRec == nRecno .AND. ::ShowSelection
             lSelected := .T.
          ENDIF
          IF ::MultipleSelection
             lSelected := ASCAN( ::aSelected, nRec ) > 0 .AND. ::ShowSelection
          ENDIF
          FOR i := nCol TO nColEnd

              IF nLeft > ::ClientWidth .OR. LEN(::__DisplayArray[nLine][1])<i// avoid painting non-visible columns
                 EXIT
              ENDIF

              cData  := ::__DisplayArray[nLine][1][i][ 1]
              nInd   := ::__DisplayArray[nLine][1][i][ 2]
              nWImg  := IIF( ::ImageList != NIL, ::__DisplayArray[nLine][1][i][ 3], 2 )
              nAlign := ::__DisplayArray[nLine][1][i][ 4]
              nHImg  := ::__DisplayArray[nLine][1][i][ 5]
              
              nBkCol := ::__DisplayArray[nLine][1][i][ 7]
              
              IF ::Striping .AND. ( nRecPos / 2 ) > Int( nRecPos / 2 )
                 nBkCol := DarkenColor( nBkCol, 25 )
              ENDIF
              
              nTxCol := ::__DisplayArray[nLine][1][i][ 8]
              nStatus:= ::__DisplayArray[nLine][1][i][10]
              nRep   := ::__DisplayArray[nLine][1][i][11]

              zLeft := nLeft
              IF lFreeze .AND. i <= ::FreezeColumn
                 zLeft := iLeft
              ENDIF
              nRight := zLeft + ::Children[i]:xWidth

              IF lFreeze .AND. i > ::FreezeColumn .AND. nRight < iRight
                 nLeft  += ::Children[i]:Width
                 iLeft  += ::Children[i]:Width
                 LOOP
              ENDIF

              SWITCH VALTYPE( cData )
                 CASE "N"
                      DEFAULT nAlign TO 2
                      cData := ALLTRIM( TRANSFORM( cData, ::Children[ i ]:Picture ) ) //ALLTRIM( STR( cData ) )
                      EXIT

                 CASE "D"
                      DEFAULT nAlign TO 3
                      cData := ALLTRIM(DTOC( cData ))
                      EXIT

                 CASE "L"
                      DEFAULT nAlign TO 3
                      cData := ALLTRIM(IIF( cData, "<True>", "<False>" ))
                      EXIT

                 CASE "C"
                      DEFAULT nAlign TO 1
                      cData := ALLTRIM( TRANSFORM( cData, ::Children[ i ]:Picture ) ) //ALLTRIM( cData )
                      EXIT

                 CASE "B"
                      DEFAULT nAlign TO 3
                      cData := "<block>"
                      EXIT

                 CASE "A"
                      DEFAULT nAlign TO 1
                      EXIT
              END

              IF VALTYPE( cData ) != "A"
                 aData := __str2a( cData, CHR(13)+CHR(10) )
               ELSE
                 aData := cData
              ENDIF
              IF EMPTY( aData )
                 AADD( aData, "" )
              ENDIF

              lHighLight := .F.
              IF ::DataSource:Deleted()
                 IF lSelected .AND. ( i == ::ColPos .OR. ::FullRowSelect )
                 
                    lHighLight := ::HasFocus .OR. ::__CurControl != NIL
                 
                    SetBkColor( hMemDC, IIF( ::HasFocus .OR. ::__CurControl != NIL, ::HighlightColor, nBkCol ) )
                    SetTextColor( hMemDC, IIF( ::HasFocus .OR. ::__CurControl != NIL, ::HighlightTextColor, nTxCol ) )
                  ELSE
                    SetBkColor( hMemDC, nBkCol )
                    SetTextColor( hMemDC, nTxCol )
                 ENDIF
               ELSE
                 IF lDis .OR. ( GetFocus() != ::hWnd .AND. ::FullRowSelect .AND. lSelected ) .OR. ( lSelected .AND. nRec <> nRecno )
                    SetBkColor( hMemDC, IIF( ::ShadowRow, ::__InactiveHighlight, nBkCol ) )
                    SetTextColor( hMemDC, ::__InactiveHighlightText )
                  ELSE
                    IF lSelected .AND. ( i == ::ColPos .OR. ::FullRowSelect ) //.AND. nRecno == nRec
                       lHighLight := ::HasFocus .OR. ::__CurControl != NIL
                       SetBkColor( hMemDC, IIF( ::HasFocus .OR. ::__CurControl != NIL, ::HighlightColor, IIF( ::ShadowRow, ::__InactiveHighlight, nBkCol ) ) )
                       SetTextColor( hMemDC, IIF( ::HasFocus .OR. ::__CurControl != NIL, ::HighlightTextColor, IIF( ::ShadowRow, ::__InactiveHighlightText, nTxCol ) ) )
                     ELSE
                       IF lSelected .AND. !::FullRowSelect .AND. i != ::ColPos .AND. ::ShadowRow
                          SetBkColor( hMemDC, ::__InactiveHighlight )
                          SetTextColor( hMemDC, ::__InactiveHighlightText )
                        ELSE
                          SetBkColor( hMemDC, nBkCol )
                          SetTextColor( hMemDC, nTxCol )
                       ENDIF
                    ENDIF
                 ENDIF
              ENDIF
              
              nHeaderRight := nRight-1
              aText := { zLeft, nTop/*-IIF(::xShowGrid,0,1)*/, nRight-IIF( ( lSelected .AND. ::FullRowSelect .AND. i<nColEnd ) .OR. !::xShowGrid, 0, 1 ), nBottom+IIF(::xShowGrid,0,1) }
              
              IF lFreeze .AND. i > ::FreezeColumn .AND. zLeft < iRight
                 aText[1] := iRight
                 aText[3] += iRight - zLeft
                 //nHeaderRight += iRight - zLeft
              ENDIF
              IF nLine == 1 .AND. ::ShowHeaders
                 aAlign := _GetTextExtentPoint32( hMemDC, ::Children[i]:Caption )
                 x := zLeft + 3

                 SWITCH nAlign
                    CASE 2
                         x := nRight - aAlign[1]-3
                         EXIT

                    CASE 3
                         x:= zLeft + ((nRight-zLeft)/2) - (aAlign[1]/2)
                         EXIT
                 END
                 ::Children[i]:DrawHeader( hMemDC, aText[1], nHeaderRight, x )
              ENDIF

              aAlign := _GetTextExtentPoint32( hMemDC, ALLTRIM( aData[1] ) )
              
              x := zLeft + IIF( ::Children[i]:ImageAlignment == 1, nWImg, 0 )
              
              y := nTop + ((nBottom-nTop)/(LEN( aData )+1)) - (aAlign[2]/2)

              SWITCH nAlign
                 CASE 2
                      x := nRight - aAlign[1]-4
                      IF ::Children[i]:ImageAlignment == 3
                         x -= nWImg
                      ENDIF
                      nAlign := DT_RIGHT
                      EXIT

                 CASE 3
                      x:= zLeft + ((nRight-zLeft+IIF( ::Children[i]:ImageAlignment == 1, nWImg, 0 ))/2) - (aAlign[1]/2)
                      nAlign := DT_CENTER
                      EXIT
              END
              
              nAve := Int( ( Int( aTextExt[ 1 ] / 26 ) + 1 ) / 2 )

              lHide := ::Children[i]:ControlHide
              IF VALTYPE( lHide ) == "B"
                 lHide := EVAL( ::Children[i]:ControlHide, Self, nRec )
              ENDIF

              nCtrl := 0
              lDrawControl := .F.
              IF nRep == 1 .OR. nRep == 3

                 TRY
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
                 CATCH
                 END

                 IF nRep == 1
                    FOR z := 1 TO LEN( aData )
                        aAlign := _GetTextExtentExPoint( hMemDC, ALLTRIM(aData[z]), aText[3]-aText[1], @iLen )
                        IF aAlign != NIL
                           IF nAlign == 1
                              nDif := (aAlign[1]+nWImg) - (aText[3]-aText[1])
                              IF nDif > 0 .AND. !EMPTY( ALLTRIM( aData[z] ) )
                                 aData[z] := ALLTRIM( LEFT( aData[z], iLen - 3 ) )+ "..."
                              ENDIF
                           ENDIF
                           _ExtTextOut( hMemDC, x, y, ETO_CLIPPED+IIF( z==1,ETO_OPAQUE,0), aText,aData[z])
                           y += aAlign[2]
                        ENDIF
                    NEXT
                  ELSE
                   _ExtTextOut( hMemDC, x, y, ETO_CLIPPED | ETO_OPAQUE, aText, " ")
                 ENDIF
              ENDIF
              // Draw Grid
              IF ::xShowGrid
                 aGrid := { {zLeft,nBottom}, {nRight,nBottom}, {nRight-1,nBottom}, {nRight-1,nTop-1} }
                 _PolyLine( hMemDC, aGrid )
              ENDIF

              IF ::ShowSelectionBorder .AND. ( lHighLight .OR. ( !::ShowSelection .AND. nRec == nRecno .AND. ( i == ::ColPos .OR. ::FullRowSelect ) ) )
                 nFocRow := nLine
              ENDIF

              IF nRep == 1

                 // Draw the icon if it is part visible
                 IF nWImg > 2 /*.AND. zLeft + nWImg > 0*/ .AND. ::ImageList != NIL
                    nY := nTop + ( (nBottom-nTop-nHImg)/2) + 1

                    nImgX := zLeft+nCtrl+1
                    
                    IF ::Children[i]:ImageAlignment == 2
                       nImgX := zLeft + (((aText[3]-aText[1])-nWImg)/2)
                     ELSEIF ::Children[i]:ImageAlignment == 3
                       nImgX := zLeft + ((aText[3]-aText[1])-nWImg)
                    ENDIF
                    
                    IF nImgX >= zLeft
                       IF !::xEnabled
                          ::ImageList:DrawDisabled( hMemDC, nInd, nImgX, nY )
                        ELSE
                          ::ImageList:DrawImage( hMemDC, nInd, nImgX, nY, ILD_TRANSPARENT )
                       ENDIF
                    ENDIF

                    //::ImageList:DrawImage( hMemDC, nInd, IIF( nAlign == 2, nRight-2-::ImageList:IconWidth, zLeft+nCtrl+1 ), nY, ILD_TRANSPARENT )
                 ENDIF

                 IF lDrawControl
                    aRect := ::Children[i]:ControlObject:DrawFrame( ::Drawing, {zLeft+IIF(::Children[i]:ControlAlign==DT_LEFT,1,0),nTop+1,nRight-2,MAX(nBottom-1,nTop+::ItemHeight)}, ::Children[i]:ControlAlign, ::Children[i]:ControlWidth, ::Children[i]:ControlHeight, nStatus )
                    IF lSelected
                       ::__CheckPos := aRect
                    ENDIF
                 ENDIF

               ELSEIF nRep > 1

                 x:= zLeft + ((nRight-zLeft+nWImg)/2) - (aAlign[1]/2)

                 IF nRep == 3
                    aRect := {zLeft+IIF(::Children[i]:ControlAlign==DT_LEFT,1,0),nTop+1,nRight-2,MAX(nBottom-1,nTop+::ItemHeight)}
                 ENDIF
                 ::__DrawRepresentation( hMemDC, nRep, aText, aData[1], nBkCol, nTxCol, x, y, aAlign, ::__DisplayArray[nLine][1][i][ 1] )

              ENDIF

              lBorder := ::ShowSelectionBorder .AND. ( nRec == nRecno .AND. ( i == ::ColPos .OR. ::FullRowSelect ) )

              IF ::ShadowRow .AND. !::FullRowSelect .AND. !::ShowSelection .AND. ::ShowSelectionBorder .AND. nRec == nRecno 
                 hPen := SelectObject( hMemDC, GetStockObject( BLACK_PEN ) )
                 IF i == ::ColPos
                    hBrush  := SelectObject( hMemDC, GetStockObject( NULL_BRUSH ) )
                    Rectangle( hMemDC, zLeft, nTop, nRight, nBottom )
                    SelectObject( hMemDC, hBrush )
                  ELSE
                    MoveTo( hMemDC, zLeft, nTop )
                    LineTo( hMemDC, nRight, nTop )
                    MoveTo( hMemDC, zLeft, nBottom-1 )
                    LineTo( hMemDC, nRight, nBottom-1 )
                 ENDIF
                 SelectObject( hMemDC, hPen )
                 IF lBorder
                    _DrawFocusRect( hMemDC, {aText[1]+1, aText[2]+1, aText[3], aText[4]-1} )
                    lBorder := .F.
                 ENDIF
               ELSE
                 IF !lHighLight .AND. nRec == nRecno .AND. !lSelected .AND. ::MultipleSelection .AND. ( i == ::ColPos .OR. ::FullRowSelect )
                    lHighLight := .T.
                 ENDIF
                 lBorder := ::ShowSelectionBorder .AND. ( lHighLight .OR. ( !::ShowSelection .AND. nRec == nRecno .AND. ( i == ::ColPos .OR. ::FullRowSelect ) ) )
              ENDIF
              

              IF !::FullRowSelect .AND. lBorder 
                 _DrawFocusRect( hMemDC, aText )
              ENDIF

              nLeft  += ::Children[i]:Width
              iLeft  += ::Children[i]:Width
          NEXT

        ELSE

          nBkCol := ::BackColor
          IF ::Striping
             nRecPos ++
             IF ( nRecPos / 2 ) > Int( nRecPos / 2 )
                nBkCol := DarkenColor( nBkCol, 25 )
             ENDIF
          ENDIF
          FOR i := nCol TO nColEnd

              IF nLeft > ::ClientWidth // avoid painting non-visible columns
                 EXIT
              ENDIF
              
              zLeft := nLeft
              IF lFreeze .AND. i <= ::FreezeColumn
                 zLeft := iLeft
              ENDIF
              nRight := zLeft + ::Children[i]:xWidth

              IF lFreeze .AND. i > ::FreezeColumn .AND. nRight < iRight
                 nLeft  += ::Children[i]:Width
                 iLeft  += ::Children[i]:Width
                 LOOP
              ENDIF

              //nRight := nLeft + ::Children[ i ]:xWidth

              x := zLeft
              y := nTop

              SetBkColor( hMemDC, nBkCol )
              SetTextColor( hMemDC, ::ForeColor )

              // Draw empty last line
              _ExtTextOut( hMemDC, x, y, ETO_CLIPPED+ETO_OPAQUE, { zLeft, nTop, nRight+1, nBottom+1 }," ")

              IF ::xShowGrid
                 aGrid := { {zLeft,nBottom}, {nRight,nBottom}, {nRight-1,nBottom}, {nRight-1,nTop-1} }
                 _PolyLine( hMemDC, aGrid )
              ENDIF

              aText := { zLeft, nTop, nRight-IIF( ( ::FullRowSelect .AND. i<nColEnd ), 0, 1 ), nBottom+1 }

              nHeaderRight := nRight-1

              IF lFreeze .AND. i > ::FreezeColumn .AND. zLeft < iRight //.AND. v == 1
                 aText[1] := iRight
                 aText[3] += iRight - zLeft
              ENDIF

              IF nLine == 1 .AND. ::ShowHeaders
                 aAlign := _GetTextExtentPoint32( hMemDC, ::Children[i]:Caption )
                 nAlign := ::Children[i]:xAlignment
                 x := zLeft + 3

                 SWITCH nAlign
                    CASE 2
                         x := nRight - aAlign[1]-3
                         EXIT

                    CASE 3
                         x:= zLeft + ((nRight-zLeft)/2) - (aAlign[1]/2)
                         EXIT
                 END
                 ::Children[i]:DrawHeader( hMemDC, aText[1], nHeaderRight, x )
              ENDIF


              nLeft  += ::Children[i]:Width
              iLeft  += ::Children[i]:Width
          NEXT

       ENDIF

       IF ::FullRowSelect .AND. nLine == nFocRow
          TRY
             nLeft   := nHScroll + ::__DisplayArray[::RowPos][1][nCol][6]
             nTop    := ::__GetHeaderHeight() + ( ( nFocRow-1 ) * ::ItemHeight ) + IIF( ::xShowGrid, 0, 1 )
             nBottom := nTop + ::ItemHeight - 1

             IF ::ShowSelectionBorder
                 _DrawFocusRect( hMemDC, { nLeft, nTop-IIF(::xShowGrid,0,1), nRight-IIF( ::xShowGrid, 1, 0 ), nBottom } )
             ENDIF
           CATCH
          END
       ENDIF

   NEXT
   IF ::__DataWidth - ABS( nHScroll ) < ::ClientWidth
      x := ::__DataWidth - ABS( nHScroll )
      y := 0

      SetBkColor( hMemDC, ::BackColor )
      SetTextColor( hMemDC, ::ForeColor )

      _ExtTextOut( hMemDC, x, y, ETO_CLIPPED | ETO_OPAQUE, { x, y, ::ClientWidth, ::ClientHeight }," ")
   ENDIF

   IF !::ClientEdge .AND. !::StaticEdge .AND. !::Border
      hOldPen := SelectObject( hMemDC, ::__LinePen )
      hBrush  := SelectObject( hMemDC, GetStockObject( NULL_BRUSH ) )
      Rectangle( hMemDC, 0, 0, ::Width, ::Height )
      SelectObject( hMemDC, hBrush )
   ENDIF
   
   SelectObject( hMemDC, hOldFont )
   SelectObject( hMemDC, hOldPen )
RETURN .T.

METHOD __DrawRepresentation( hDC, nRep, aRect, cText, nBkCol, nTxCol, x, y, aMetrics, xVal ) CLASS DataGrid
   LOCAL nX, nY, nWidth, aClip, n, nFore, nBack, hPen, hOP, hOB, hBrush, hOld, hTheme, nStatus, nFlags, lXP

   lXP    := ::Application:IsThemedXP .AND. ::Theming
   nFlags := DFCS_BUTTONCHECK

   IF nRep == 2
      hPen := CreatePen( PS_SOLID, 0, nBkCol )

      nFore := GetTextColor(hDC)
      nBack := GetBkColor(hDC)

      nWidth := aRect[1] + ( ( ( aRect[3] - aRect[1] ) * VAL( cText ) ) / 100 )
      aClip  := { x, aRect[2], X + aMetrics[1], aRect[4] }

      IF nWidth < aRect[3]
         hBrush := CreateSolidBrush( nBkCol )
         _FillRect( hDC, { nWidth, aRect[2], aRect[3], aRect[4] }, hBrush )
         DeleteObject( hBrush )
      ENDIF
      hBrush := CreateSolidBrush( nTxCol )
      _FillRect( hDC, { aRect[1], aRect[2], MIN( nWidth, aRect[3] ), aRect[4] }, hBrush )
      DeleteObject( hBrush )

      hOP := SelectObject( hDC, hPen )
      hOB := SelectObject( hDC, GetStockObject( NULL_BRUSH ) )

      Rectangle( hDC, aRect[1], aRect[2], aRect[3], aRect[4] )

      SetTextColor(hDC, nTxCol )
      SetBkColor(hDC, nBkCol )
      _ExtTextOut(hDC, x, y, ETO_CLIPPED, aClip, cText)

      IF x<=nWidth
         SetTextColor(hDC, nBkCol )
         SetBkColor(hDC, nTxCol )
         aClip:={ x, aRect[2], nWidth, aRect[4] }
         _ExtTextOut(hDC, x, y, ETO_CLIPPED, aClip, cText)
      END
      SelectObject( hDC, hOP )
      SelectObject( hDC, hOB )
      DeleteObject( hPen )
    ELSE
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
              nStatus := DFCS_BUTTON3STATE | DFCS_CHECKED
              IF lXP
                 nStatus := IIF( ::__lHot, CBS_MIXEDHOT, CBS_MIXEDNORMAL )
              ENDIF

      ENDCASE

      nFlags := nFlags | nStatus

      IF lXP
         hTheme := OpenThemeData(,ToUnicode("button"))
         DrawThemeBackground( hTheme, hDC, BP_CHECKBOX, nStatus, aRect, aRect )
         CloseThemeData( hTheme )
       ELSE
         aRect[1] := aRect[1] + (( aRect[3]-aRect[1]-15 )/2)
         aRect[2] := aRect[2] + (( aRect[4]-aRect[2]-15 )/2)

         aRect[3] := aRect[1] + 15
         aRect[4] := aRect[2] + 15

         _DrawFrameControl( hDC, aRect, DFC_BUTTON, nFlags )
      ENDIF

   ENDIF

RETURN Self

//----------------------------------------------------------------------------------

METHOD __UpdateHeight() CLASS DataGrid
   LOCAL n, x, nColumns, nRec, nHeight, nVisible, nUsable

   nHeight  := ::__GetHeaderHeight()
   nUsable  := 0
   nVisible := 0

   IF ::DataSource != NIL
      nRec     := ::DataSource:Recno()

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
METHOD __Update( lDisplay ) CLASS DataGrid
   LOCAL n, nRec, oCol, nAlign
   DEFAULT lDisplay TO TRUE
   IF ::DataSource == NIL .OR. ( VALTYPE( ::DataSource )=="O" .AND. !::DataSource:IsOpen )
      RETURN Self
   ENDIF

   ::__SetBlocks()
   ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )
   ::__DisplayArray    := {}
   nRec              := ::DataSource:Recno()
   DEFAULT nRec TO 1
   ::__SkipRecords( -(::RowPos-1) )

   IF ::HitTop
      EVAL( ::__bGoTop )
   ENDIF
   IF !::HitTop .AND. !::HitBottom
      FOR n := 1 TO ::RowCountVisible
          AADD( ::__DisplayArray,{ ARRAY( ::ColCount ), ::DataSource:Recno() } )
          ::__FillRow( n )
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
RETURN Self

METHOD Update() CLASS DataGrid
   LOCAL n, nRec, nRecs, nFirst, nLast, x, nPos, nSkip, nUse, nIns

   ::__DataWidth := 0
   AEVAL( ::Children, {|o| ::__DataWidth += o:Width} )

   IF ::DataSource == NIL //.OR. EMPTY( ::__DisplayArray )
      ::__DisplayData()
      RETURN Self
   ENDIF
   IF EMPTY( ::__DisplayArray )
      ::__Update(.F.)
   ENDIF
   IF EMPTY( ::__DisplayArray )
      RETURN Self
   ENDIF
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

   ::RowPos := ASCAN( ::__DisplayArray, {|a| a[2] == nRec } )
   ::__VertScrolled := MAX( 1, ::Record - ::RowPos + 1 )

   IF ::RowPos > 0
      ::DataSource:Skip( -(::RowPos-1) )
   ENDIF
   ::__DisplayArray := {}
   
   IF ::RowCountUsable > 0
      nIns := 0

      FOR n := 1 TO ::RowCountVisible
          AADD( ::__DisplayArray,{ ARRAY( ::ColCount ), ::DataSource:Recno(), ::ItemHeight } )
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
            AINS( ::__DisplayArray, 1, { ARRAY( ::ColCount ), ::DataSource:Recno(), ::ItemHeight }, .T. )
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


   ::__DisplayData()
RETURN Self

//----------------------------------------------------------------------------------

METHOD __ScrollUp( nScroll ) CLASS DataGrid
   LOCAL nRec
   LOCAL n, x, aScroll, aClip, nPos, nNew, nRow
   DEFAULT nScroll TO ::__VertScrolled + 1

   IF ::__VertScrolled + ::RowCountUsable <= ::DataSource:OrdKeyCount()

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
            ::__DisplayArray[ ::RowCountVisible ] := { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() /*::Record*/ }
            ::__FillRow( ::RowCountVisible )
           CATCH
            ::__DisplayArray[ ::RowCountUsable ] := { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() /*::Record*/ }
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

      IF ::IsCovered( ::__GetHeaderHeight() )
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
   LOCAL nRec, nPor
   LOCAL n, x, aScroll, aClip, nPos, nNew
   DEFAULT nScroll TO ::__VertScrolled - 1

   IF ::__VertScrolled > 1

      nNew := ::__VertScrolled - nScroll

      ::__VertScrolled := nScroll
      ::__UpdateVScrollBar()

      nPos := ::__VertScrolled
      nRec := ::DataSource:Recno()

      ::__SkipRecords( nPos - ::Record )

      WHILE LEN( ::__DisplayArray ) < ::RowCountVisible
         AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )
      ENDDO

      FOR n := 1 TO MIN( ::RowCountVisible, nNew )
          AINS( ::__DisplayArray, n, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )
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

      IF ::IsCovered( ::__GetHeaderHeight() )
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

METHOD __UpdateVScrollBar( lRedraw, lForce ) CLASS DataGrid
   LOCAL nFlags := SIF_ALL
   LOCAL nMin := 1, nPage := 0, nMax  := 0, nPos  := 0

   DEFAULT lRedraw TO TRUE
   DEFAULT lForce  TO FALSE

   IF ::DataSource != NIL .AND. ::DataSource:IsOpen .AND. ( ::__VertScroll .OR. ::AutoVertScroll .OR. lForce )

      IF ::DataSource:ClsName == "MemoryTable" .OR. ::DataSource:Driver IN { "SQLRDD", "SQLEX" } .OR. ::ExtVertScrollBar
         nPage := Int(  ::__DataHeight/::ItemHeight )
         nMax  := ::DataSource:OrdKeyCount()
         nPos  := IIF( nMax < nPage, 0, ::__VertScrolled )
         IF nMax <= nPage .AND. ::AutoVertScroll
            nMax := 0
         ENDIF
       ELSE
         nMax  := 100
         nPos  := ::DataSource:OrdKeyRelPos()*100
         nFlags := SIF_POS | SIF_RANGE
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

   IF (::__HorzScroll .OR. ::AutoHorzScroll) .AND. IsWindow( ::hWnd )
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

   LOCAL nDelta, si, nPor, aScroll, aClip, nRight, n, ScrollInfo, cBuffer, rc

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
           ::__UpdateHScrollBar()

           IF ::FreezeColumn > 0
              nRight := 0
              FOR n := 1 TO ::FreezeColumn
                  nRight += ::Children[n]:xWidth
              NEXT
              aClip   := { nRight, 0, ::Width, ::__GetHeaderHeight() }
              lDraw := .T.
           ENDIF
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

           ::__nScrolled := nPos
   END
RETURN 0


//----------------------------------------------------------------------------------

METHOD OnVertScroll( nCode, nPos, hSBar ) CLASS DataGrid
   ::__ResetControl()

   ::AutoUpdate := 0

   DO CASE
      CASE nCode == SB_LINEUP
           IF ::DataSource:ClsName == "MemoryTable" .OR. ::DataSource:Driver IN { "SQLRDD", "SQLEX" } .OR. ::ExtVertScrollBar
              ::__ScrollDown()
            ELSE
              ::OnKeyDown( VK_UP )
           ENDIF
      CASE nCode == SB_LINEDOWN
           IF ::DataSource:ClsName == "MemoryTable" .OR. ::DataSource:Driver IN { "SQLRDD", "SQLEX" } .OR. ::ExtVertScrollBar
              ::__ScrollUp()
            ELSE
              ::OnKeyDown( VK_DOWN )
           ENDIF

      CASE nCode == SB_PAGEDOWN
           IF ::DataSource:ClsName == "MemoryTable" .OR. ::DataSource:Driver IN { "SQLRDD", "SQLEX" } .OR. ::ExtVertScrollBar
              ::__ScrollUp( ::__VertScrolled + ::RowCountUsable )
            ELSE
              ::OnKeyDown( VK_NEXT )
           ENDIF

      CASE nCode == SB_PAGEUP
           IF ::DataSource:ClsName == "MemoryTable" .OR. ::DataSource:Driver IN { "SQLRDD", "SQLEX" } .OR. ::ExtVertScrollBar
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
           IF ::DataSource:ClsName == "MemoryTable" .OR. ::DataSource:Driver IN { "SQLRDD", "SQLEX" } .OR. ::ExtVertScrollBar
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

//----------------------------------------------------------------------------------

METHOD __FillRow( nPos ) CLASS DataGrid
   EXTERN hb_QSelf
   LOCAL nImageWidth, nImageHeight, nImageIndex, x, nColBkColor, nColTxColor, nStatus, nAlign, cData, nRet, n, i, aData, nHeight

   LOCAL nBack, nFore
   
   ::__DataWidth := 0
   DEFAULT nPos TO ::RowPos
   
   //aData := ::DataSource:Gather()

   nBack := ExecuteEvent( "OnQueryBackColor", Self )
   nFore := ExecuteEvent( "OnQueryForeColor", Self )
/*
   nHeight := ExecuteEvent( "GetItemHeight", Self )
   IF VALTYPE( nHeight ) != "N"
      nHeight := ::ItemHeight
   ENDIF
   IF LEN( ::__DisplayArray[ nPos ] ) < 3
      AADD( ::__DisplayArray[ nPos ], NIL )
   ENDIF
   ::__DisplayArray[ nPos ][3] := nHeight

   ::RowCountVisible := Ceil( (::__DataHeight-::ItemHeight+nHeight)/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  (::__DataHeight-::ItemHeight+nHeight)/::ItemHeight ), ::RowCount )
*/
   FOR x := 1 TO LEN( ::Children )
       nImageWidth := 0
       nImageHeight:= 0
       nImageIndex := IIF( VALTYPE( ::Children[x]:ImageIndex ) == "B", EVAL( ::Children[x]:ImageIndex, Self, EVAL( ::__bRecNo ) ), ::Children[x]:ImageIndex )
       nRet := ExecuteEvent( "OnQueryImageIndex", ::Children[x] )

       IF VALTYPE( nRet ) == "N"
          nImageIndex :=  nRet
       ENDIF

       IF nImageIndex > 0 .AND. ::ImageList != NIL
          nImageWidth := ::ImageList:IconWidth
          nImageHeight:= ::ImageList:IconHeight
       ENDIF

       nColBkColor := ::Children[x]:BackColor
       nColTxColor := ::Children[x]:ForeColor
       
       nColBkColor := nBack
       nRet := ExecuteEvent( "OnQueryBackColor", ::Children[x] )
       IF VALTYPE( nRet ) == "N"
          nColBkColor :=  nRet
       ENDIF

       nColTxColor := nFore
       nRet := ExecuteEvent( "OnQueryForeColor", ::Children[x] )
       IF VALTYPE( nRet ) == "N"
          nColTxColor :=  nRet
       ENDIF

       nAlign := ::Children[x]:Alignment

       IF VALTYPE( nColBkColor ) == "B"
          nColBkColor := EVAL( nColBkColor, Self, ::Children[x] )
       ENDIF

       IF VALTYPE( nColTxColor ) == "B"
          nColTxColor := EVAL( nColTxColor, Self, ::Children[x] )
       ENDIF

       IF VALTYPE( nAlign ) == "B"
          nAlign := EVAL( nAlign, Self, ::Children[x] )
       ENDIF

       DEFAULT nColBkColor TO ::BackColor
       DEFAULT nColTxColor TO ::ForeColor

       nStatus := ::Children[x]:ControlStatus

       IF VALTYPE( ::Children[x]:ControlStatus )=="B"
          nStatus := EVAL( ::Children[x]:ControlStatus, Self, ::DataSource:Recno() )
       ENDIF

       cData := ::Children[x]:Data
       //IF ( n := RAT( ":", cData ) ) > 0
       //   cData := SUBSTR( cData, n+1 )
       //   IF ( i := ASCAN( ::DataSource:Structure, {|a| a[1]==cData } ) ) > 0
       //      cData := aData[i]
       //   ENDIF
       // ELSE
          IF VALTYPE( cData ) == "B"
             cData := EVAL( cData, Self )
           ELSE
             DEFAULT cData TO "' '"
             TRY
                cData := &cData
               catch
                cData := ""
             END
          ENDIF
       //ENDIF

       IF ::ConvertOem .AND. VALTYPE( cData ) == "C"
          cData := OemToChar( cData )
       ENDIF
       ::__DisplayArray[ nPos ][1][x] := { cData,;
                                         nImageIndex,;
                                         nImageWidth + 2,;
                                         nAlign,;
                                         nImageHeight,;
                                         ::__DataWidth,;
                                         nColBkColor,;
                                         nColTxColor,;
                                         ::Children[x]:Width,;
                                         nStatus,;
                                         ::Children[x]:Representation }
       ::__DataWidth += ::Children[x]:Width
   NEXT

RETURN Self

//----------------------------------------------------------------------------------

METHOD ArrowLeft( lMove ) CLASS DataGrid
   LOCAL lRes, nScroll, nCol, nCur, nPos := 0
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
      nScroll := ::__DisplayArray[1][1][::ColPos][6] - ABS(::__HorzScrolled)
      IF nScroll < 0
         ::OnHorzScroll( SB_THUMBTRACK, ABS(::__HorzScrolled) - ABS(nScroll),, FALSE )
         IF ::IsCovered( ::__GetHeaderHeight() )
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
   LOCAL nScroll, nCol, nCur, nPos := 0, lREs

   DEFAULT lMove TO .T.

   IF ::FullRowSelect .OR. ::ColPos == 0
      IF ::sih != NIL .AND. ::sih:nMax <> 0
         nPos := MAX( ::__HorzScrolled - ( ::__HScrollUnits * IIF( CheckBit( GetKeyState( VK_CONTROL ) ), 5, 1 ) ), -(::__DataWidth - ::ClientWidth) )
         ::OnHorzScroll( SB_THUMBTRACK, -nPos )
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
      nScroll := ( ::__DisplayArray[1][1][::ColPos][6] + ::Children[ ::ColPos ]:Width ) - ::ClientWidth - ABS(::__HorzScrolled)

      IF ::Children[ ::ColPos ]:Width > ::ClientWidth
         nScroll -= ( ::Children[ ::ColPos ]:Width - ::ClientWidth )
      ENDIF
      IF nScroll > 0
         ::OnHorzScroll( SB_THUMBTRACK, ABS(::__HorzScrolled) + nScroll,, FALSE )
         IF ::IsCovered( ::__GetHeaderHeight() )
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
   LOCAL lRes, x, nImageWidth, aScroll, aClip, lSel, n
   LOCAL lShift := CheckBit( GetKeyState( VK_SHIFT ) )

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   ::RowCountVisible := Ceil( ::__DataHeight/::ItemHeight )
   ::RowCountUsable  := MIN( Int(  ::__DataHeight/::ItemHeight ), ::RowCount )

   IF lShift .AND. ::MultipleSelection
      IF ( n := ASCAN( ::__aSel, {|a| a[1]==::DataSource:Recno() } ) ) > 0
         IF ::__aSel[n][4]==VK_UP
            ADEL( ::aSelected, n, .T. )
            ADEL( ::__aSel, n, .T. )
         ENDIF
       ELSEIF LEN( ::aSelected ) > 1
         ::aSelected := { ::DataSource:Recno() }
         ::__aSel    := { { ::DataSource:Recno(), .T., .F., VK_DOWN } }
         ::Update()
      ENDIF
   ENDIF

   ::__ResetRecordPos(.F.)
   ::__SkipRecords( 1 )

   IF ::HitBottom
      EVAL( ::__bGoBottom )
      RETURN NIL
   ENDIF

   IF ::MultipleSelection
      IF lShift
         IF ( n := ASCAN( ::aSelected, ::DataSource:Recno() ) ) == 0
            AADD( ::aSelected, ::DataSource:Recno() )
            AADD( ::__aSel,  { ::DataSource:Recno(), .T., .F., VK_DOWN } )
         ENDIF
       ELSE
         lSel := LEN( ::aSelected ) > 1
         ::aSelected := { ::DataSource:Recno() }
         ::__aSel    := { { ::DataSource:Recno(), .F., .F., -1 } }
         IF lSel
            ::Update()
         ENDIF
      ENDIF
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

         ::__DisplayArray[ ::RowCountVisible ] := { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() }

         ::__FillRow( ::RowCountVisible )

         IF ::RowCountUsable != ::RowCountVisible
            ::__SkipRecords( -1 )
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
      aScroll := { 0, ::__GetHeaderHeight(), ::ClientWidth, (::RowCountUsable-1)*::ItemHeight + ::__GetHeaderHeight() }
      aClip   := { 0, ::__GetHeaderHeight(), ::ClientWidth, (::RowCountUsable-1)*::ItemHeight + ::__GetHeaderHeight() }
      ::ScrollWindow( 0, -::ItemHeight, aScroll, aClip )
      ::ValidateRect()
      ::__DisplayData( MIN( ::RowCountUsable-1, ::RowCountVisible ),, ::RowCountVisible )
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
   LOCAL lRes, x, nImageWidth, aScroll, aClip, lSel, n
   LOCAL lShift := CheckBit( GetKeyState( VK_SHIFT ) )
   
   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   IF lShift .AND. ::MultipleSelection
      IF ( n := ASCAN( ::__aSel, {|a| a[1]==::DataSource:Recno() } ) ) > 0
         IF ::__aSel[n][4]==VK_DOWN
            ADEL( ::aSelected, n, .T. )
            ADEL( ::__aSel, n, .T. )
         ENDIF
       ELSEIF LEN( ::aSelected ) > 1
         ::aSelected := { ::DataSource:Recno() }
         ::__aSel    := { { ::DataSource:Recno(), .T., .F., VK_UP } }
         ::Update()
      ENDIF
   ENDIF

   ::__ResetRecordPos(.F.)
   ::__SkipRecords( -1)

   IF ::HitTop
      EVAL( ::__bGoTop )
      RETURN NIL
   ENDIF

   IF ::MultipleSelection
      IF lShift
         IF ( n := ASCAN( ::aSelected, ::DataSource:Recno() ) ) == 0
            AADD( ::aSelected, ::DataSource:Recno() )
            AADD( ::__aSel,  { ::DataSource:Recno(), lShift, .F., VK_UP } )
         ENDIF
       ELSE
         lSel := LEN( ::aSelected ) > 1
         ::aSelected := { ::DataSource:Recno() }
         ::__aSel    := { { ::DataSource:Recno(), lShift, .F., -1 } }
         IF lSel
            ::Update()
         ENDIF
      ENDIF
   ENDIF

   ::RowPos --

   IF ::RowPos <= 0
      ::RowPos := 1

      IF ::RowCount < ::RowCountVisible
         AADD( ::__DisplayArray, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )
      ENDIF

      // Insert next first visible Record
      AINS( ::__DisplayArray, 1, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )//, TRUE )

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

      // Scroll Down
      aScroll := { 0, ::__GetHeaderHeight()+::ItemHeight, ::ClientWidth, ( ::RowCountVisible * ::ItemHeight ) + ::__GetHeaderHeight() }
      aClip   := { 0, ::__GetHeaderHeight()+::ItemHeight, ::ClientWidth, ( ::RowCountVisible * ::ItemHeight ) + ::__GetHeaderHeight() }
      ::ScrollWindow(  0, ::ItemHeight, aScroll, aClip )
      ::ValidateRect()
   ENDIF
   ::__DisplayData( ::RowPos,, ::RowPos+1 )
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD OnNCDestroy() CLASS DataGrid
   Super:OnNCDestroy()
   IF ::__LinePen != NIL
      DeleteObject(::__LinePen)
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------

METHOD PageDown( nCount ) CLASS DataGrid
   LOCAL lRes, i, n, x, nImageWidth, nColumns, nRec

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   ::__ResetRecordPos(.F.)
   IF ::RowPos < ::RowCountUsable
      ::__SkipRecords( ::RowCountUsable-::RowPos )
      IF ::MultipleSelection
         ::aSelected := { ::DataSource:Recno() }
         ::__aSel    := { { ::DataSource:Recno(), .F., .F., VK_NEXT } }
      ENDIF
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
      IF ::MultipleSelection
         ::aSelected := { ::DataSource:Recno() }
         ::__aSel    := { { ::DataSource:Recno(), .F., .F., VK_NEXT } }
      ENDIF
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
       AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )
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
          AINS( ::__DisplayArray, 1, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() }, TRUE )
          ::__FillRow( 1 )
      NEXT
      ::__GoToRec( nRec )
   ENDIF
   IF ::MultipleSelection
      ::aSelected := { ::DataSource:Recno() }
      ::__aSel    := { { ::DataSource:Recno(), .F., .F., VK_NEXT } }
   ENDIF
   ::__DisplayData()
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD PageUp( nCount ) CLASS DataGrid
   LOCAL lRes, i, n, x, nImageWidth, nColumns, nRec

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
   DEFAULT lRes TO .T.
   IF !lRes
      RETURN NIL
   ENDIF

   ::__ResetRecordPos(.F.)
   IF ::RowPos > 1
      ::__SkipRecords( -(::RowPos-1) )
      IF ::MultipleSelection
         ::aSelected := { ::DataSource:Recno() }
         ::__aSel    := { { ::DataSource:Recno(), .F., .F., VK_PRIOR } }
      ENDIF
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
      IF ::MultipleSelection
         ::aSelected := { ::DataSource:Recno() }
         ::__aSel    := { { ::DataSource:Recno(), .F., .F., VK_PRIOR } }
      ENDIF
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
       AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )
       ::__FillRow( n )
       ::__SkipRecords( 1 )
       IF ::HitBottom
          EVAL( ::__bGoBottom )
          EXIT
       ENDIF
   NEXT

   ::__GoToRec( ::__DisplayArray[1][2] )

   IF ::MultipleSelection
      ::aSelected := { ::DataSource:Recno() }
      ::__aSel    := { { ::DataSource:Recno(), .F., .F., VK_PRIOR } }
   ENDIF

   ::__DisplayData()
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD Home() CLASS DataGrid
   LOCAL lRes, i, n, x, nImageWidth, nColumns, nRec

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
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
       AADD( ::__DisplayArray,{ ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )
       ::__FillRow( n )
       ::__SkipRecords( 1 )
       IF ::HitBottom
          EVAL( ::__bGoBottom )
          EXIT
       ENDIF
   NEXT
   EVAL( ::__bGoTop )
   IF ::MultipleSelection
      ::aSelected := { ::DataSource:Recno() }
      ::__aSel    := { { ::DataSource:Recno(), .F., .F., VK_HOME } }
   ENDIF
   ::__DisplayData()
   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD End( lVUpdate ) CLASS DataGrid
   LOCAL lRes, i, n, x, nImageWidth,  nColumns, nRec
   DEFAULT lVUpdate TO .T.

   lRes := ::OnRowChanging()
   DEFAULT lRes TO ExecuteEvent( "OnRowChanging", Self )
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
       AADD( ::__DisplayArray, { ARRAY( LEN( ::Children ) ), ::DataSource:Recno() } )
       ::__FillRow( n )
       ::__SkipRecords( 1 )
       IF ::HitBottom
          EVAL( ::__bGoBottom )
          EXIT
       ENDIF
   NEXT
   EVAL( ::__bGoBottom )
   IF ::MultipleSelection
      ::aSelected := { ::DataSource:Recno() }
      ::__aSel    := { { ::DataSource:Recno(), .F., .F., VK_END } }
   ENDIF

   ::RowPos := MIN( ::RowCount, ::RowCountUsable )
   ::__DisplayData()

   IF ::bRowChanged != NIL
      EVAL( ::bRowChanged, Self )
   ENDIF
   ::OnRowChanged()
   ExecuteEvent( "OnRowChanged", Self )
RETURN Self

//----------------------------------------------------------------------------------

METHOD AutoAddColumns( lEdit ) CLASS DataGrid
   LOCAL aField, nAlign, oCol, cField, lCol := .F.
   DEFAULT lEdit TO .F.
   
   IF VALTYPE( ::DataSource )=="O" .AND. ::DataSource:IsOpen
      ::__SetBlocks()

      FOR EACH aField IN ::DataSource:Structure

          oCol := GridColumn( Self )
          oCol:xCaption  := __Proper( aField[1] )

          oCol:Data      := "hb_QSelf():DataSource:Fields:" + aField[1]

          oCol:AllowSize := .T.
          oCol:AllowDrag := .T.
          oCol:Create()
          oCol:Width    := MAX( aField[3], LEN(oCol:Caption)+2 )*7

          DO CASE
             CASE aField[2]=="C"
                  IF lEdit
                     oCol:Picture := "@k"
                     oCol:Control := {|o, n|MaskEdit( o )  }
                  ENDIF
             CASE aField[2]=="D"
                  IF lEdit
                     oCol:Control := {|o, n|MaskEdit( o )  }
                  ENDIF
                  oCol:Alignment := 3
             CASE aField[2]=="L"
                  oCol:Width := MAX( 6, LEN(oCol:Caption)+2 )*7
                  oCol:Alignment := 3
             CASE aField[2]=="N"
                  IF lEdit
                     oCol:Control := {|o, n|MaskEdit( o )  }
                  ENDIF
                  oCol:Alignment := 2
          ENDCASE

          IF lEdit
             oCol:ControlAccessKey := GRID_CHAR | GRID_LCLICK
             oCol:OnSave := {|oCol, oGrid, xData| oGrid:DataSource:Fields:Put( xData, Alltrim(aField[1] ) ) }
          ENDIF
      NEXT
      IF ::__lCreated
         ::__Update()
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------

METHOD __Edit( n, xPos, yPos, nMessage, nwParam ) CLASS DataGrid
   LOCAL aRect, nState, nVal, aKeys, oCtrl, oCol, x
   LOCAL xValue, nAlign

   IF ::__CurControl == NIL .AND. ( ::ColPos > 0 .OR. ::FullRowSelect ) 
      IF !::FullRowSelect .AND. ::Children[::ColPos]:Control != NIL .AND. ::Children[::ColPos]:ControlAccessKey & nMessage != 0
         ::__CurControl := EVAL( ::Children[::ColPos]:Control, Self, ::DataSource:Recno() )
         aRect := ::GetItemRect()
         IF aRect == NIL
            RETURN Self
         ENDIF
       ELSEIF ::FullRowSelect
         FOR x := 1 TO LEN( ::Children )
             IF ::Children[x]:Control != NIL .AND. ::Children[x]:ControlAccessKey & nMessage != 0
                ::__CurControl := EVAL( ::Children[x]:Control, Self, ::DataSource:Recno() )
                IF ::__CurControl != NIL .AND. RIGHT( ::__CurControl:__xCtrlName, 4 ) != "Edit"
                   ::ColPos     := x
                   aRect := ::GetItemRect()
                   EXIT
                ENDIF
             ENDIF
         NEXT
      ENDIF

      IF ::__CurControl != NIL .AND. ::DataSource:RecLock()
         ::__CurControl:OnWMKillFocus  := {|o|o:Parent:KeepActiveCaption := .F.,;
                                           o:Destroy(),;
                                           o:Parent:DataSource:UnLock(),;
                                           o:Parent:__CurControl := NIL,;
                                           o:Parent:__DisplayData( ::RowPos, ::ColPos, ::RowPos, ::ColPos ) }

         ::__CurControl:Left   := aRect[1]+1
         ::__CurControl:Width  := aRect[3]-aRect[1]-IIF( ::xShowGrid, 2, 1 )
         ::__CurControl:Top    := aRect[2]+1
         ::__CurControl:Height := ( aRect[4]-aRect[2] )-2
         ::__CurControl:SetStyle( ES_MULTILINE )
         ::__CurControl:SetStyle( ES_AUTOHSCROLL )
         ::__CurControl:SetStyle( WS_BORDER, FALSE )
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
                    nAlign := 2
                    EXIT

               CASE "D"
                    nAlign := 3
                    EXIT

               CASE "L"
                    nAlign := 3
                    EXIT

               CASE "C"
                    nAlign := 1
                    EXIT
            END
         ENDIF
         DO CASE
            CASE nAlign == 2
                 ::__CurControl:SetStyle( ES_RIGHT )
            CASE nAlign == 3
                 ::__CurControl:SetStyle( ES_CENTER )
         ENDCASE
         ::__CurControl:Caption := XSTR( xValue )
         ::__CurControl:Create()

         DEFAULT ::__CurControl:OnWMKeyDown   TO {|o,n| IIF( n==27, (o:Destroy(),o:Parent:DataSource:UnLock(),0), IIF( n==13, (o:Parent:__ControlSaveData(),o:Destroy(),o:Parent:DataSource:UnLock()), NIL ) )}
         DEFAULT ::__CurControl:OnWMKillFocus TO {|o|o:Parent:__ControlSaveData() }

         ::__CurControl:BackColor     := ::Children[::ColPos]:ControlBackColor
         ::__CurControl:ForeColor     := ::Children[::ColPos]:ControlForeColor

         IF n == 1 .AND. nwParam != NIL .AND. nwParam != 13
            ::__CurControl:SendMessage( WM_CHAR, nwParam, 0 )
         ENDIF

         ::__CurControl:SetFocus()

      ENDIF

   ENDIF
RETURN Self

//----------------------------------------------------------------------------------

METHOD GetItemRect() CLASS DataGrid
   LOCAL aRect, nLeft, nTop, nBottom, nRight
   IF ::RowPos> 0 .AND. ::RowPos<=::RowCountUsable
      nLeft   := ::__HorzScrolled + ::__DisplayArray[ ::RowPos ][ 1 ][ ::ColPos ][ 6 ]
      nTop    := ::__GetHeaderHeight() + ( ( ::RowPos-1 ) * ::ItemHeight )
      nBottom := nTop + ::ItemHeight - 1
      nRight  := nLeft + ::Children[ ::ColPos ]:Width - 1
      aRect:={ nLeft, nTop, nRight, nBottom }
   ENDIF
RETURN aRect

//----------------------------------------------------------------------------------

METHOD __ControlSaveData() CLASS DataGrid
   LOCAL oCtrl, nPos, cField, n, lRefresh := .T.

   IF ::__CurControl:__xCtrlName == "MaskEdit" .AND. !::__CurControl:validating
      IF ::__CurControl:IsValid
         IF ( ::__CurControl:lInvalid .OR. ::Children[::ColPos]:ControlValid == NIL )
            IF ::Children[::ColPos]:OnSave != NIL
               lRefresh := EVAL( ::Children[::ColPos]:OnSave, ::Children[::ColPos], Self, ::__CurControl:oGet:VarGet() )
            ENDIF
            IF HGetPos( ::Children[::ColPos]:EventHandler, "OnSave" ) != 0
               oCtrl := ::Children[::ColPos]
               
               ::__CurControl:VarPut( ::__CurControl:oGet:Buffer )
               lRefresh := oCtrl:Form:&( oCtrl:EventHandler[ "OnSave" ] )( ::__CurControl, Self, ::__CurControl:oGet:VarGet() )
            ENDIF
         ENDIF
      ENDIF
    ELSEIF ::__CurControl:__xCtrlName == "Edit" .OR. ::__CurControl:__xCtrlName == "EditBox"
      IF ::Children[::ColPos]:ControlValid == NIL
         IF ::Children[::ColPos]:OnSave != NIL
            lRefresh := EVAL( ::Children[::ColPos]:OnSave, ::Children[::ColPos], Self, ::__CurControl:Caption )
         ENDIF
         IF HGetPos( ::Children[::ColPos]:EventHandler, "OnSave" ) != 0
            oCtrl := ::Children[::ColPos]
            lRefresh := oCtrl:Form:&( oCtrl:EventHandler[ "OnSave" ] )( ::__CurControl, Self, ::__CurControl:Caption )
         ENDIF
      ENDIF
   ENDIF

   IF VALTYPE( lRefresh ) != "L"
      lRefresh := .T.
   ENDIF
   IF GetFocus() != ::hWnd
      ::PostMessage( WM_KILLFOCUS, 0, 0 )
   ENDIF
   IF ::__CurControl != NIL
      ::__CurControl:Destroy()
      ::DataSource:UnLock()
      ::__CurControl := NIL
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

   DATA IsContainer EXPORTED INIT .F.
   DATA AllowSize                    PUBLISHED INIT .F.
   DATA AllowDrag                    PUBLISHED INIT .F.
   DATA Locked                       PUBLISHED INIT FALSE
   DATA Picture                      PUBLISHED
   DATA Caption                      PUBLISHED
   DATA Data                         PUBLISHED
   
   DATA EnumImageAlignment    EXPORTED INIT { {"Left", "Center", "Right"}, {1,2,3} }

   PROPERTY ContextMenu GET __ChkComponent( Self, ::xContextMenu )

   PROPERTY ImageAlignment    READ xImageAlignment   WRITE Refresh DEFAULT 1
   PROPERTY Alignment         READ xAlignment        WRITE SetAlignment  DEFAULT 1
   PROPERTY Width             READ xWidth            WRITE SetWidth
   PROPERTY Caption           READ xCaption          WRITE SetCaption
   PROPERTY BackColor INDEX 1 READ xBackColor        WRITE SetColor
   PROPERTY ForeColor INDEX 2 READ xForeColor        WRITE SetColor
   PROPERTY ImageIndex        READ xImageIndex       WRITE SetImageIndex
   PROPERTY SortArrow         READ xSortArrow        WRITE __SetSortArrow      DEFAULT 0
   PROPERTY HeaderImageIndex  READ xHeaderImageIndex WRITE SetHeaderImageIndex DEFAULT 0
   PROPERTY Representation    READ xRepresentation   WRITE SetRepresentation   DEFAULT 1
   PROPERTY AutoEdit          READ xAutoEdit         WRITE __SetAutoEdit       DEFAULT .F. INVERT

   DATA HeaderFont                   PUBLISHED INIT Font()

   DATA HeaderBackSysColor           EXPORTED INIT __GetSystem():CurrentScheme:ToolStripPanelGradientBegin
   DATA HeaderForeSysColor           EXPORTED INIT __GetSystem():CurrentScheme:ToolStripBorder

   DATA xHeaderBackColor             EXPORTED
   ACCESS HeaderBackColor            INLINE IIF( ::xHeaderBackColor == NIL, ::HeaderBackSysColor, ::xHeaderBackColor ) PERSISTENT
   ASSIGN HeaderBackColor( n )       INLINE ::xHeaderBackColor := n

   DATA xHeaderForeColor             EXPORTED
   ACCESS HeaderForeColor            INLINE IIF( ::xHeaderForeColor == NIL, ::HeaderForeSysColor, ::xHeaderForeColor ) PERSISTENT
   ASSIGN HeaderForeColor( n )       INLINE ::xHeaderForeColor := n



   DATA __lResizeable                EXPORTED INIT {.F.,.F.,.F.,.F.,.F.,.T.,.F.,.F.}
   DATA __lMoveable                  EXPORTED INIT .F.
   DATA __lCopyCut                   EXPORTED INIT .F.
   DATA __lAllowCopy                 EXPORTED INIT .F.
   DATA __lCreateAfterChildren       EXPORTED INIT .F.
   DATA __IdeImageIndex              EXPORTED INIT 3
   DATA __TempRect                   EXPORTED
   DATA __IsControl                  EXPORTED INIT .F.
   DATA __PropFilter                 EXPORTED INIT {}
   DATA __Representation             EXPORTED  INIT { "Normal", "ProgressBar", "CheckBox" }

   DATA __Alignments                 EXPORTED  INIT { "Left", "Right", "Center" }
   DATA Parent                       EXPORTED
   DATA ClsName                      EXPORTED  INIT "GridColumn"
   DATA Owner                        EXPORTED
   DATA Type                         EXPORTED  INIT "C"

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

   DATA Events                       EXPORTED
   DATA Cargo                        EXPORTED

   DATA BackSysColor                 EXPORTED  INIT RGB( 255, 255, 255 )
   DATA ForeSysColor                 EXPORTED  INIT RGB( 0, 0, 0 )

   DATA Children                     EXPORTED
   DATA hWnd                         EXPORTED
   DATA Active                       EXPORTED  INIT .F.

   DATA EventHandler                 EXPORTED
   DATA MDIContainer                 EXPORTED  INIT .F.

   DATA __HeaderLeft                 PROTECTED INIT 0
   DATA __HeaderRight                PROTECTED INIT 0
   DATA __HeaderX                    PROTECTED INIT 0
   
   METHOD Init() CONSTRUCTOR
   METHOD SetColor()
   METHOD Create()
   METHOD MoveWindow()        INLINE Self
   METHOD InvalidateRect()    INLINE ::DrawHeader()
   METHOD UpdateWindow()      INLINE Self
   METHOD IsWindowVisible()   INLINE .T.
   METHOD IsWindow()          INLINE .T.
   METHOD SetWidth( n )       INLINE ::Parent:__SetColWidth( ::xPosition, n )

   METHOD SetCaption( c )     INLINE NIL
   METHOD SetPosition( n )    INLINE ::Parent:__SetColPos( ::xPosition, n )
   METHOD GetRectangle()      INLINE { ::Left, ::Top, ::Left + ::Width, ::Top + ::Height }
   METHOD GetChildFromPoint() INLINE Self
   METHOD SetSize()           INLINE Self
   METHOD __OnparentSize()    INLINE 0
   METHOD Refresh()           INLINE ::Parent:__DisplayData(, ::xPosition,, ::xPosition )
   METHOD GetEditValue()      INLINE ::Parent:__CurControl:oGet:VarGet()
   METHOD SetWindowPos(h, x, y, cx, cy ) INLINE ::Width := cx, ::Parent:Update()
   METHOD SetHeaderImageIndex()
   METHOD DrawHeader()
   METHOD CreateDragImage()
   METHOD Destroy()
   METHOD GetRect()
   METHOD GetSize()
   METHOD SetImageIndex()
   METHOD SetAlignment()
   METHOD SetRepresentation()
   METHOD __SetSortArrow(n)   INLINE AEVAL( ::Parent:Children, {|o|o:xSortArrow:=0} ), ::xSortArrow := n
   METHOD __SetAutoEdit()
ENDCLASS

METHOD CreateDragImage( nLeft ) CLASS GridColumn
   LOCAL hImageList, hMemDC, hMemBitmap, hOldBitmap, nWidth := MIN( ::Parent:ClientWidth, nLeft + ::Width ) - nLeft

   hMemBitmap := GetScreenBitmap( {nLeft,0,nLeft+nWidth, ::Parent:ClientHeight}, ::Parent:hWnd )

   hImageList := ImageListCreate( nWidth, ::Parent:ClientHeight, ILC_COLOR32 | ILC_MASK, 1, 0 )
   ImageListAdd( hImageList, hMemBitmap )
   
   DeleteObject( hMemBitmap )
RETURN hImageList


METHOD DrawHeader( hDC, nLeft, nRight, x, lHot ) CLASS GridColumn
   LOCAL aAlign, z, y, i, nColor, nShadow, hOldPen, hOldBrush, nBorder, nBackColor, hOldFont, n, aAlignment, aRect, nH := 5, nx := 0
   LOCAL nTop, nIcoLeft, hPenShadow, hPenLight, nTxColor, nImage := ::xHeaderImageIndex
   
   DEFAULT lHot   TO .F.
   DEFAULT nLeft  TO ::__HeaderLeft
   DEFAULT nRight TO ::__HeaderRight
   DEFAULT x      TO ::__HeaderX
   DEFAULT nImage TO 0
   DEFAULT hDC    TO ::Parent:Drawing:hDC
   
   aRect := {nLeft, 0, nRight+1, ::Parent:__GetHeaderHeight()}

   IF ::SortArrow > 0
      nx := 13
   ENDIF
   
   nTxColor   := ::HeaderForeColor
   IF nTxColor == ::HeaderForeSysColor
      nTxColor := ::System:Colors:BtnText
   ENDIF
   
   nBorder    := ::HeaderForeColor
   nBackColor := ::HeaderBackColor
   IF lHot
      nBorder    := ::System:CurrentScheme:MenuItemBorder
      nBackColor := ::System:CurrentScheme:MenuItemSelected
   ENDIF
   
   ::__HeaderLeft  := nLeft
   ::__HeaderRight := nRight
   ::__HeaderX     := x

   hOldFont := SelectObject( hDC, ::HeaderFont:Handle )

   aAlign := _GetTextExtentPoint32( hDC, ::Caption )
   y := (::Parent:__GetHeaderHeight() - aAlign[2] ) / 2


   hOldBrush := SelectObject( hDC, CreateSolidBrush( nBackColor ) )

   Rectangle( hDC, aRect[1], aRect[2], aRect[3], aRect[4] )

   nColor  := LightenColor( nBackColor, 100 )
   nShadow := DarkenColor( nBackColor, 100 )

   hPenShadow := CreatePen( PS_SOLID, 0, nShadow )
   hPenLight  := CreatePen( PS_SOLID, 0, nColor )

   __Draw3dRect( hDC, aRect, nColor, nShadow )
   
   nColor := SetTextColor( hDC, nTxColor )
   n := ::Alignment
   
   IF n == 2
      x := x - nx
    ELSEIF n == 3
      x := x - (nx/2)
   ENDIF
   
   IF nImage > 0 .AND. VALTYPE( ::Parent:ImageList ) == "O"
      nTop  := ( aRect[4] - ::Parent:ImageList:IconHeight ) / 2

      IF n == 1 // Left
         //nIcoLeft := x
         //x += ::Parent:ImageList:IconWidth + 3
         nIcoLeft := nLeft + 1
         x += ::Parent:ImageList:IconWidth + 1

       ELSEIF n == 2 // Right
         //nIcoLeft := aRect[3]-::Parent:ImageList:IconWidth-3
         //x -= ::Parent:ImageList:IconWidth+3
         nIcoLeft := nLeft + 1

       ELSEIF n == 3 // Center
         nIcoLeft := nLeft + 1
         x += ( ::Parent:ImageList:IconWidth + 1 ) / 2
      ENDIF
      IF !::Parent:xEnabled
         ::Parent:ImageList:DrawDisabled( hDC, nImage, Int( nIcoLeft ), Int( nTop ) )
       ELSE
         ::Parent:ImageList:DrawImage( hDC, nImage, Int( nIcoLeft ), Int( nTop ) )
      ENDIF
   ENDIF

   IF !::Parent:xEnabled
      SetTextColor( hDC, GetSysColor( COLOR_GRAYTEXT ) )
   ENDIF
   
   _ExtTextOut( hDC, x, y, ETO_CLIPPED, aRect, ::Caption )
   SetTextColor( hDC, nColor )

   IF ::SortArrow > 0 .AND. aRect[3]-15 > nLeft .AND. aRect[3]-15 > x + aAlign[1]
      hOldPen := SelectObject( hDC, hPenLight )
      z := 1
      FOR i := 1 TO 2
          FOR n := 1 TO nH
              x := IIF( ::SortArrow == 1,n,nH-n+1)
              y := (aRect[4]-nH)/2
              
              MoveTo( hDC, aRect[3] - (15-x), y+n+z )
              LineTo( hDC, aRect[3] - ( 4+x), y+n+z )
          NEXT
          SelectObject( hDC, hPenShadow )
          z := 0
          aRect[3]--
      NEXT
      SelectObject( hDC, hOldPen )
   ENDIF
/*
   IF ::SortArrow == 1
      
      hOldPen := SelectObject( hDC, hPenShadow )
      
      MoveTo( hDC, aRect[3] - nx,     (aRect[4]-nH)/2 )
      LineTo( hDC, aRect[3] - nx + 8, (aRect[4]-nH)/2 )

      MoveTo( hDC, aRect[3] - nx,     (aRect[4]-nH)/2 )
      LineTo( hDC, aRect[3] - nx + 4, ((aRect[4]-nH)/2)+8 )
      
      SelectObject( hDC, hPenLight )
      
      MoveTo( hDC, aRect[3] - nx + 7, ((aRect[4]-nH)/2)+1 )
      LineTo( hDC, aRect[3] - nx + 3, ((aRect[4]-nH)/2)+8 )

      SelectObject( hDC, hOldPen )

    ELSEIF ::SortArrow == 2

      hOldPen := SelectObject( hDC, hPenLight )
      
      MoveTo( hDC, aRect[3] - nx,     (aRect[4]+nH)/2 )
      LineTo( hDC, aRect[3] - nx + 8, (aRect[4]+nH)/2 )

      MoveTo( hDC, aRect[3] - nx + 7, ((aRect[4]+nH)/2)-1 )
      LineTo( hDC, aRect[3] - nx + 3, ((aRect[4]+nH)/2)-8 )
      
      SelectObject( hDC, hPenShadow )
      
      MoveTo( hDC, aRect[3] - nx + 3, ((aRect[4]+nH)/2)-7 )
      LineTo( hDC, aRect[3] - nx,     (aRect[4]+nH)/2 )

      SelectObject( hDC, hOldPen )

   ENDIF
*/   
   SelectObject( hDC, hOldFont )

   DeleteObject( SelectObject( hDC, hOldBrush ) )
   DeleteObject( hPenShadow )
   DeleteObject( hPenLight )
RETURN NIL

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

//----------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS GridColumn
//   DEFAULT bData    TO {||""}

   ::HeaderFont:Parent := Self
   
   ::Children    := {}
   ::Parent      := oParent
   ::xImageIndex := 0
   ::xPosition   := LEN( ::Parent:Children )+1
   IF oParent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
   ENDIF
   //::Form       := oParent:Form

   ::EventHandler := Hash()
   HSetCaseMatch( ::EventHandler, .F. )
   ::__xCtrlName := "GridColumn"
   ::__CreateProperty( "GridColumn" )
   ::Events := {}
   IF oParent:__ClassInst != NIL
      ::Events := { ;
                  {"Object",      {;
                                  { "OnInit"             , "", "" } } },;
                  {"Data",        {;
                                  { "OnSave"            , "", "" } } },;
                  {"Color",       {;
                                  { "OnQueryBackColor"  , "", "" },;
                                  { "OnQueryForeColor"  , "", "" } } },;
                  {"Image",       {;
                                  { "OnQueryImageIndex" , "", "" } } },;
                  {"General",     {;
                                  { "OnCreate"          , "", "" },;
                                  { "OnContextMenu"     , "", "" },;
                                  { "OnHeaderClick"     , "", "" } } } }
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------

METHOD Create() CLASS GridColumn
   LOCAL cType, cData
   IF ::hWnd != NIL
      RETURN Self
   ENDIF
   IF VALTYPE( ::xImageIndex ) == "N"
      ::xImageIndex := MAX( 0, ::xImageIndex )
   ENDIF

   ::HeaderFont:Create():Modify()
   ExecuteEvent( "OnInit", Self )
   AADD( ::Parent:Children, Self )
   ::xPosition := LEN( ::Parent:Children )
   ::hWnd := Seconds()
   DEFAULT ::xWidth TO 10 * LEN( ::Caption )
   ::Parent:__UpdateHScrollBar()

   TRY
      IF ::AutoEdit
         ::Control := {|o, n|MaskEdit( o )  }
         ::ControlAccessKey := GRID_CHAR | GRID_LCLICK
      ENDIF
   CATCH
   END
   ExecuteEvent( "OnCreate", Self )
   ::Parent:Update()
RETURN Self

METHOD __SetAutoEdit( lEdit ) CLASS GridColumn
   IF lEdit != ::AutoEdit
      IF lEdit
         ::Control := {|o, n|MaskEdit( o )  }
         ::ControlAccessKey := GRID_CHAR | GRID_LCLICK
       ELSE
         ::Control := NIL
         ::ControlAccessKey := NIL
      ENDIF
   ENDIF
RETURN lEdit

METHOD SetImageIndex(n) CLASS GridColumn
   LOCAL x
   ::xImageIndex := n
   TRY
      IF ::Parent:__ClassInst != NIL
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

METHOD SetHeaderImageIndex(n) CLASS GridColumn
   ::xHeaderImageIndex := n
RETURN Self


//----------------------------------------------------------------------------------

METHOD Destroy() CLASS GridColumn
   ::Parent:DeleteColumn( ::Position, .T. )
RETURN Self

//----------------------------------------------------------------------------------

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

METHOD SetRepresentation(n) CLASS GridColumn
   ::xRepresentation := n
   FOR n := 1 TO LEN( ::Parent:__DisplayArray )
       //IF !EMPTY( ::Parent:__DisplayArray[n][1] )
          ::Parent:__DisplayArray[n][1][::xPosition][11] := ::xRepresentation
       //ENDIF
   NEXT
   ::Parent:__DisplayData( ,::xPosition, , ::xPosition )
RETURN Self

//----------------------------------------------------------------------------------

METHOD SetColor( nInd, nColor ) CLASS GridColumn
   LOCAL n
   IF nInd == 1
      DEFAULT nColor TO ::BackSysColor
      ::xBackColor := nColor
    ELSE
      DEFAULT nColor TO ::ForeSysColor
      ::xForeColor := nColor
   ENDIF
   IF ::Parent:hWnd != NIL .AND. ::xPosition != NIL
      FOR n := 1 TO ::Parent:RowCountVisible
          TRY
             ::Parent:__DisplayArray[n][1][ ::xPosition ][6+nInd] := nColor
          CATCH
          END
      NEXT
      ::Parent:__DisplayData( ,::xPosition, , ::xPosition )
   ENDIF
RETURN Self


*-----------------------------------------------------------------------------*

Function Ceiling( x )
Return( If( x - Int( x ) > 0, Int( x ) + 1, x ) )
