/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Edit.prg                                                                                             *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
#include "debug.ch"

#Include 'inkey.ch'
#Include 'vxh.ch'


#Define ECM_FIRST  0x1500

#Define EM_SETCUEBANNER    (ECM_FIRST + 1)
#Define EM_GETCUEBANNER    (ECM_FIRST + 2)
#Define EM_SHOWBALLOONTIP  (ECM_FIRST + 3)
#Define EM_HIDEBALLOONTIP  (ECM_FIRST + 4)


#Define WM_INVALID WM_USER + 50
#Define WM_CARET   WM_USER + 51

//-----------------------------------------------------------------------------------------------
CLASS EditBox INHERIT Control
   DATA FullSelectOnClick PUBLISHED INIT .F.
   DATA EnterNext  PUBLISHED INIT .F.
   DATA EnumLayout                     EXPORTED INIT { { "None",;
                                                         "Text, Image, Arrow",;
                                                         "Text, Arrow, Image",;
                                                         "Image, Text, Arrow",;
                                                         "Image, Arrow, Text",;
                                                         "Arrow, Text, Image",; 
                                                         "Arrow, Image, Text" }, {1,2,3,4,5,6,7} }
 
   PROPERTY Layout                                      READ xLayout           WRITE __SetLayout  DEFAULT 1   PROTECTED
   PROPERTY AutoVScroll   INDEX ES_AUTOVSCROLL          READ xAutoVScroll      WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY AutoHScroll   INDEX ES_AUTOHSCROLL          READ xAutoHScroll      WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY MultiLine     INDEX ES_MULTILINE            READ xMultiLine        WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY Password      INDEX ES_PASSWORD             READ xPassword         WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY NoHideSel     INDEX ES_NOHIDESEL            READ xNoHideSel        WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY OemConvert    INDEX ES_OEMCONVERT           READ xOemConvert       WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY ReadOnly                                    READ xReadOnly         WRITE SetReadOnly  DEFAULT .F. PROTECTED
   PROPERTY WantReturn    INDEX ES_WANTRETURN           READ xWantReturn       WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY HorzScroll    INDEX WS_HSCROLL              READ xHorzScroll       WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY VertScroll    INDEX WS_VSCROLL              READ xVertScroll       WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY Case                                        READ xCase             WRITE SetCase      DEFAULT 1   PROTECTED
   PROPERTY Border        INDEX WS_BORDER               READ xBorder           WRITE SetStyle     DEFAULT !__GetApplication():IsThemedXP PROTECTED
   PROPERTY Number        INDEX ES_NUMBER               READ xNumber           WRITE SetStyle     DEFAULT .F.

   PROPERTY ClientEdge    INDEX WS_EX_CLIENTEDGE        READ xClientEdge       WRITE SetExStyle   DEFAULT .T. PROTECTED

   PROPERTY ContextMenu                                 GET __ChkComponent( Self, ::xContextMenu ) SET __SetContextMenu
   
   PROPERTY ContextArrow                                READ xContextArrow     WRITE __SetContextMenu DEFAULT .F. PROTECTED
   PROPERTY ImageIndex                                  READ xImageIndex       WRITE __SetImageIndex  DEFAULT 0   PROTECTED

   PROPERTY DataSource    GET __ChkComponent( Self, ::xDataSource )

   DATA Transparent                    PUBLISHED INIT .F.
   DATA ImageIndex                     PUBLISHED INIT 0

   DATA DataSearchField                PUBLISHED INIT 1
   DATA DataSearchWidth                PUBLISHED INIT 0
   DATA DataSearchRecords              PUBLISHED INIT 0
   //--------------------------------------------------------------------------

   DATA ImageList                      EXPORTED
   DATA AllowUnDock                    EXPORTED INIT FALSE
   DATA AllowClose                     EXPORTED INIT FALSE

   DATA Button                         EXPORTED  INIT .F.
   DATA ButtonPushed                   PROTECTED INIT .F.
   DATA ButtonAction                   EXPORTED

   DATA BackSysColor                   EXPORTED INIT GetSysColor( COLOR_WINDOW )
   DATA ForeSysColor                   EXPORTED INIT GetSysColor( COLOR_WINDOWTEXT )

   DATA xSelForeColor                  EXPORTED
   ACCESS SelForeColor                 INLINE ::xSelForeColor PERSISTENT //IIF( ::xSelForeColor == NIL, ::ForeSysColor, ::xSelForeColor ) PERSISTENT
   ASSIGN SelForeColor( n )            INLINE ::xSelForeColor := n, ::SetSelColor( ::SelForeColor, ::SelBackColor )

   DATA xSelBackColor                  EXPORTED
   ACCESS SelBackColor                 INLINE ::xSelBackColor PERSISTENT //IIF( ::xSelBackColor == NIL, ::BackSysColor, ::xSelBackColor ) PERSISTENT
   ASSIGN SelBackColor( n )            INLINE ::xSelBackColor := n, ::SetSelColor( ::SelForeColor, ::SelBackColor )

   DATA CaseTypes                      EXPORTED  INIT { "Mixed Case", "Upper Case", "Lower Case" }

   DATA __Alignments                   EXPORTED  INIT { "Left", "Center", "Right" }
   ACCESS Alignment                    INLINE ::xAlignment PERSISTENT
   ASSIGN Alignment(n)                 INLINE ::SetAlignment(n)

   DATA __BackMargin                   EXPORTED INIT 2

   ACCESS Modified                     INLINE ::SendMessage( EM_GETMODIFY, 0, 0 ) == 1
   ASSIGN Modified(l)                  INLINE ::SendMessage( EM_SETMODIFY, l, 0 )

   DATA LastKey                        EXPORTED INIT 0

   DATA __aArrowPos                    PROTECTED INIT {0,0}
   DATA __aImagePos                    PROTECTED INIT {0,0}
   DATA __oDataGrid                    PROTECTED

   DATA __BkCursor                     PROTECTED
   
   DATA nImageSize INIT 0
   DATA nArrowSize INIT 0

   METHOD Init()     CONSTRUCTOR
   METHOD Create()

   METHOD SetCase()
   METHOD SetAlignment()

   METHOD OnCtlColorEdit()
   METHOD OnCtlColorStatic()

   METHOD OnDestroy()                  INLINE IIF( ::__hBrush != NIL, DeleteObject( ::__hBrush ), ), NIL
   METHOD CanUndo()                    INLINE ::SendMessage( EM_CANUNDO, 0, 0 )
   METHOD CharFromPos(x,y)             INLINE ::SendMessage( EM_CHARFROMPOS, L2Bin(x)+L2Bin(y) )
   METHOD EmptyUndoBuffer()            INLINE ::SendMessage( EM_EMPTYUNDOBUFFER, 0, 0 )
   METHOD FmtLines(lSet)               INLINE ::SendMessage( EM_FMTLINES, lSet, 0 )
   METHOD GetCueBanner()
   METHOD GetFirstVisibleLine()        INLINE ::SendMessage( EM_GETFIRSTVISIBLELINE, 0, 0 )
   METHOD GetHandle()                  INLINE ::SendMessage( EM_GETHANDLE, 0, 0 )
   METHOD GetImeStatus(nType)          INLINE ::SendMessage( EM_GETIMESTATUS, IF( nType==NIL,EMSIS_COMPOSITIONSTRING ,nType), 0 )
   METHOD GetLimitText()               INLINE ::SendMessage( EM_GETLIMITTEXT, 0, 0 )
   METHOD GetLine( n, c )
   METHOD GetLineCount()               INLINE ::SendMessage( EM_GETLINECOUNT, 0, 0 )
   METHOD GetMargins()                 INLINE ::SendMessage( EM_GETMARGINS, 0, 0 )
   METHOD GetModify()                  INLINE ::SendMessage( EM_GETMODIFY, 0, 0 )
   METHOD GetPasswordChar()            INLINE ::SendMessage( EM_GETPASSWORDCHAR, 0, 0 )
   METHOD GetEditRect(rc)
   METHOD GetSel(x,y)
   METHOD GetThumb()                   INLINE ::SendMessage( EM_GETTHUMB, 0, 0 )
   METHOD GetWordBreakProc()           INLINE ::SendMessage( EM_GETWORDBREAKPROC, 0, 0 )
   METHOD HideBalloonTip()             INLINE ::SendMessage( EM_HIDEBALLOONTIP, 0, 0 ) // XP only
   METHOD LimitText(n)                 INLINE ::SendMessage( EM_LIMITTEXT, n, 0 )
   METHOD LineFromChar(n)              INLINE ::SendMessage( EM_LINEFROMCHAR, n, 0 )
   METHOD LineIndex(n)                 INLINE ::SendMessage( EM_LINEINDEX, n, 0 )
   METHOD LineLength(n)                INLINE ::SendMessage( EM_LINELENGTH, n, 0 )
   METHOD LineScroll(x, y )            INLINE ::SendMessage( EM_LINESCROLL, x, y ) //RE different
   METHOD PosFromChar(x,y)             INLINE ::SendMessage( EM_POSFROMCHAR, x, y ) // RE different
   METHOD ReplaceSel(l, c )            INLINE ::SendMessage( EM_REPLACESEL, l, c )
   METHOD Scroll( nSB )                INLINE ::SendMessage( EM_SCROLL, nSB, 0 )
   METHOD ScrollCaret()                INLINE ::SendMessage( EM_SCROLLCARET, 0, 0 )
   METHOD SetCueBanner(cText,lFocus)   INLINE EditSetCueBannerText( ::hWnd, IIF( lFocus == NIL, .F., lFocus ), cText ) //XP only
   METHOD SetHandle(h)                 INLINE ::SendMessage( EM_SETHANDLE, h, 0 )
   METHOD SetImeStatus(nType, nStatus) INLINE ::SendMessage( EM_SETIMESTATUS,IF( nType==NIL,EMSIS_COMPOSITIONSTRING ,nType), nStatus )
   METHOD SetLimitText(n)              INLINE ::SendMessage( EM_SETLIMITTEXT, n, 0 ) // RE different
   METHOD SetMargins(ntype, nData)     INLINE ::SendMessage( EM_SETMARGINS, nType, nData )
   METHOD SetModify(lSet)              INLINE ::SendMessage( EM_SETMODIFY, lSet, 0 )
   METHOD SetPasswordChar(nAsc)        INLINE ::SendMessage( EM_SETPASSWORDCHAR, nAsc, 0 )
   METHOD SetReadOnly(lSet)            INLINE ::SetStyle( ES_READONLY, lSet ), ::SendMessage( EM_SETREADONLY, lSet, 0 )
   METHOD SetSel(nStart,nEnd)          INLINE ::SendMessage( EM_SETSEL, nStart-IIF(nStart>0,1,0), nEnd-IIF(nEnd>0,1,0) )
   METHOD SetWordBreakProc(nProc)      INLINE ::SendMessage( EM_SETWORDBREAKPROC, 0, nProc )
   METHOD Undo()                       INLINE ::SendMessage( EM_UNDO, 0, 0 )
   METHOD OnEraseBkGnd()               INLINE IIF( ::Transparent, 1, NIL )
   METHOD SetParent( oParent )         INLINE IIF( ::__hBrush != NIL, ( DeleteObject( ::__hBrush ), ::__hBrush := NIL ), ), ::Super:SetParent( oParent ), ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   METHOD SetRect(aRect, lRelative)
   METHOD SetRectNP(aRect, lRelative)
   METHOD SetTabStops( nTabs, aTabs )
   METHOD ShowBalloonTip( EBT )
   METHOD SetSelColor()
   METHOD OnParentCommand()
   METHOD OnNCCalcSize()
   METHOD OnNCPaint()
   METHOD OnNCLButtonDown()
   METHOD OnNCLButtonUp()
   METHOD OnNCHitTest()

   METHOD OnHScroll()                  INLINE NIL
   METHOD OnVScroll()                  INLINE NIL
   METHOD OnHorzScroll()               INLINE NIL
   METHOD OnVertScroll()               INLINE NIL
   METHOD __SetScrollBars()            INLINE NIL
   METHOD __SetContextMenu()
   METHOD OnKeyDown()
   METHOD OnGetDlgCode()
   METHOD OnKillFocus()                INLINE ::Redraw(), NIL
   METHOD OnSetFocus()                 INLINE ::Redraw(), NIL
   METHOD OnLButtonDown()
   METHOD OnChar()
   METHOD OnContextMenu                INLINE IIF( ::ContextArrow, ( ::CallWindowProc(), 0 ), NIL )
   METHOD __SetLayout()
   METHOD __SetImageIndex()
   METHOD __UpdateDataGrid()
   METHOD __ChkGridKeys()
   METHOD OnNCMouseMove()
   METHOD OnMouseMove()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS EditBox
   LOCAL pWi
   DEFAULT ::__xCtrlName TO "EditBox"
   ::ClsName   := "Edit"
   ::ThemeName := "EDIT"

   ::Super:Init( oParent )
   ::Width        := 80
   ::Height       := 22
   ::Style        := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | IIF( !::Application:IsThemedXP, WS_BORDER, 0 )
   ::ExStyle      := WS_EX_CLIENTEDGE
   ::BackSysColor := GetSysColor( COLOR_WINDOW )
   ::ForeSysColor := GetSysColor( COLOR_WINDOWTEXT )

   IF ::__ClassInst != NIL
      ::__PropFilter := { "HIGHLIGHTCAPTION", "SMALLCAPTION", "ALLOWMAXIMIZE" }
      ::Events := ;
             { ;
               {"Object",      {;
                               { "OnInit"             , "", "" } } },;
               {"Color",       {;
                               { "OnCtlColorEdit"     , "", "" },;
                               { "OnSysColorChange"   , "", "" } } },;
               {"Drawing",     {;
                               { "OnEraseBkGnd"       , "", "" },;
                               { "OnPaint"            , "", "" } } },;
               {"Notification",{;
                               { "OnEn_Align_Ltr_Ec"  , "", "" },;
                               { "OnEn_Align_Rtl_Ec"  , "", "" },;
                               { "OnEn_Change"        , "", "" },;
                               { "OnEn_ErrSpace"      , "", "" },;
                               { "OnEn_HScroll"       , "", "" },;
                               { "OnEn_KillFocus"     , "", "" },;
                               { "OnEn_SetFocus"      , "", "" },;
                               { "OnEn_Update"        , "", "" } } },;
               {"Editing",     {;
                               { "OnClear"            , "", "" },;
                               { "OnCopy"             , "", "" },;
                               { "OnCut"              , "", "" },;
                               { "OnPaste"            , "", "" },;
                               { "OnUndo"             , "", "" } } },;
               {"Keyboard",    {;
                               { "OnChar"             , "", "" },;
                               { "OnKeyDown"          , "", "" },;
                               { "OnKeyUp"            , "", "" },;
                               { "OnSysKeyDown"       , "", "" },;
                               { "OnGetDlgCode"       , "", "" } } },;
               {"Layout",      { ;
                               { "OnEnterSizeMove"    , "", "" },;
                               { "OnExitSizeMove"     , "", "" },;
                               { "OnMove"             , "", "" },;
                               { "OnSize"             , "", "" } } },;
               {"Mouse",       {;
                               { "OnLButtonDblClk"    , "", "" },;
                               { "OnLButtonDown"      , "", "" },;
                               { "OnLButtonUp"        , "", "" },;
                               { "OnMButtonDown"      , "", "" },;
                               { "OnMButtonUp"        , "", "" },;
                               { "OnMouseHover"       , "", "" },;
                               { "OnMouseleave"       , "", "" },;
                               { "OnMouseMove"        , "", "" },;
                               { "OnRButtonDown"      , "", "" },;
                               { "OnRButtonUp"        , "", "" },;
                               { "OnImageClick"       , "", "" } } },;
               {"Control",     {;
                               { "OnActivate"         , "", "" },;
                               { "OnCreate"           , "", "" },;
                               { "OnDestroy"          , "", "" },;
                               { "OnEnable"           , "", "" },;
                               { "OnHideWindow"       , "", "" },;
                               { "OnKillFocus"        , "", "" },;
                               { "OnMessage"          , "", "" },;
                               { "OnSetCursor"        , "", "" },;
                               { "OnSetFocus"         , "", "" },;
                               { "OnSetFont"          , "", "" },;
                               { "OnSetText"          , "", "" },;
                               { "OnUserMsg"          , "", "" } } } }
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS EditBox
   LOCAL pWi
   ::Super:Create()
   pWi := ::GetWindowInfo()
   ::__BackMargin += pWi:cxWindowBorders
   ::SetWindowPos(, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
   //::__UnSubclass()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __UpdateDataGrid() CLASS EditBox
   LOCAL nRecs, cData
   
   IF ::__oDataGrid == NIL
      
      WITH OBJECT ( ::__oDataGrid := DataGrid( ::Parent ) )
         :AutoVertScroll  := .F.
         :ShowHeaders := .F.

         WITH OBJECT ( :DataSource  := MemoryTable( Self ) )
            :Structure := { ::DataSource:Structure[ ::DataSearchField ] }
            :Table     := {}
            :Create()
         END

         :Left        := ::Left
         :Top         := ::Top + ::Height - 1
         :Width       := IIF( ::DataSearchWidth > 0, ::DataSearchWidth, ::Width )
         :NoActivate  := .T.
         :ShadowRow   := .F.
         :ShowGrid    := .F.
         :StaticEdge  := .F.
         :ShowSelectionBorder := .F.

         :OnWMKeyDown := {|o, nKey| ::__ChkGridKeys( o, nKey ) }
         :OnWMLButtonDblClk := {|o| ::__ChkGridKeys( o, VK_RETURN ), 0 }
         :Create()
         :BringWindowToTop()
         
         WITH OBJECT GridColumn( :this )
            :xCaption  := ""
            :Data      := "hb_QSelf():DataSource:Fields:" + ::__oDataGrid:DataSource:Structure[1][1]
            :AllowSize := .F.
            :AllowDrag := .F.
            :Create()
            :Width     := :Parent:ClientWidth
         END
      END
   ENDIF

   ::__oDataGrid:DataSource:Zap()
   
   ::DataSource:GoTop()
   nRecs := 0
   WHILE ( nRecs < ::DataSearchRecords .OR. ::DataSearchRecords == 0 ) .AND. !::DataSource:Eof()
      cData := ALLTRIM( ::DataSource:Fields:FieldGet( ::DataSearchField ) )
      IF UPPER( ALLTRIM( ::Caption ) ) IN UPPER( cData )
         AADD( ::__oDataGrid:DataSource:Table, { cData } )
         nRecs ++
      ENDIF
      ::DataSource:Skip()
   ENDDO

   ::__oDataGrid:Height := MIN( (nRecs * ::__oDataGrid:ItemHeight), ::Parent:ClientHeight - ::__oDataGrid:Top )
   IF ::__oDataGrid:Height < ::__oDataGrid:ItemHeight
      ::__oDataGrid:Height := 0
   ENDIF

   ::__oDataGrid:DataSource:GoTop()
   ::__oDataGrid:Update()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __ChkGridKeys( o, nKey ) CLASS EditBox
   IF nKey == VK_UP
      IF o:GetPosition() == 1
         ::SetFocus()
      ENDIF
    ELSEIF nKey == VK_RETURN
      ::Caption := ALLTRIM( ::__oDataGrid:DataSource:Fields:FieldGet( 1 ) )
      ::SetFocus()
      ::__oDataGrid:Height := 0
      ::PostMessage( WM_KEYDOWN, VK_END )
    ELSEIF nKey == VK_ESCAPE
      ::SetFocus()
      ::__oDataGrid:Height := 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnLButtonDown() CLASS EditBox
   IF ! ( GetFocus() == ::hWnd ) .AND. ::FullSelectOnClick
      ::CallWindowProc()
      ::PostMessage( EM_SETSEL, 0, -1 )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnGetDlgCode() CLASS EditBox
   IF ::wParam == VK_RETURN .AND. ::EnterNext
      ::LastKey := ::wParam
      ::PostMessage( WM_KEYDOWN, VK_TAB, ::lParam )
      RETURN 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD __SetLayout( nLayout ) CLASS EditBox
   IF ::IsWindow()
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetImageIndex(n) CLASS EditBox
   IF ::__ClassInst != NIL .AND. n > 0 .AND. ::xLayout == 1
      ::Layout := 2
      IF ::__ClassInst != NIL
         ::Application:ObjectManager:CheckValue( "Layout", "General", ::Layout )
      ENDIF
   ENDIF
RETURN Self
   
//-----------------------------------------------------------------------------------------------
METHOD SetCase( nCase ) CLASS EditBox
   SWITCH nCase
      CASE 1
         ::SetStyle( ES_LOWERCASE, .F. )
         ::SetStyle( ES_UPPERCASE, .F. )
         EXIT
      CASE 2
         ::SetStyle( ES_LOWERCASE, .F. )
         ::SetStyle( ES_UPPERCASE, .T. )
         EXIT
      CASE 3
         ::SetStyle( ES_UPPERCASE, .F. )
         ::SetStyle( ES_LOWERCASE, .T. )
         EXIT
   END
   ::SetWindowText( ::xCaption )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetAlignment( n ) CLASS EditBox
   LOCAL x
   ::xAlignment := n
   FOR x := 1 TO LEN( ::__Alignments )
       IF x != n
          ::SetStyle( x-1, .F. )
       ENDIF
   NEXT
   ::SetStyle( n-1, .T. )
RETURN n

//-----------------------------------------------------------------------------------------------
METHOD SetSelColor( nFore, nColor, lRepaint ) CLASS EditBox

   DEFAULT lRepaint TO TRUE

   ::xSelForeColor := nFore
   ::xSelBackColor := nColor
   IF ::SelBkBrush != NIL
      DeleteObject( ::SelBkBrush )
      ::SelBkBrush := NIL
   ENDIF
   IF nColor != NIL
      ::SelBkBrush := CreateSolidBrush( nColor )
   ENDIF
   IF lRepaint .AND. ::IsWindowVisible() .AND. GetFocus() == ::hWnd
      ::InvalidateRect()
   ENDIF

RETURN SELF

//-----------------------------------------------------------------------------------------------
METHOD OnCtlColorStatic( nwParam, nlParam )
   LOCAL hBkGnd, nBkColor

   hBkGnd   := ::BkBrush
   DEFAULT hBkGnd TO ::Parent:BkBrush

   nBkColor := ::BackColor
   DEFAULT nBkColor TO ::Parent:BackColor

   IF nBkColor != NIL
      SetBkColor( nwParam, nBkColor )
   ENDIF
   IF ::ForeColor != NIL .AND. ::ForeColor != ::ForeSysColor
      SetTextColor( nwParam, ::ForeColor )
   ENDIF
   IF hBkGnd != NIL
      IF ::ReadOnly
         SetBkMode( nwParam, TRANSPARENT )
      ENDIF
      RETURN hBkGnd
    ELSEIF ::ForeColor != NIL .AND. ::ForeColor != ::ForeSysColor
      SetBkMode( nwParam, TRANSPARENT )
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD __SetContextMenu() CLASS EditBox
   ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER )
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnMouseMove() CLASS EditBox
   IF ::xLayout > 1 .AND. ::__BkCursor != NIL
      ::__hCursor := IIF( ::__BkCursor == 0, NIL, ::__BkCursor )
      ::__BkCursor := NIL
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnNCMouseMove() CLASS EditBox
   IF ::xLayout > 1 .AND. ::__BkCursor == NIL
      IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
         ::__BkCursor := ::__hCursor
         DEFAULT ::__BkCursor TO 0
         ::Cursor := ::System:Cursor:LinkSelect
      ENDIF
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnNCCalcSize( nwParam, nlParam ) CLASS EditBox
   LOCAL n, nccs
   ::nImageSize := 0
   ::nArrowSize := 0

   IF ::xLayout > 1 .OR. ::Button
      IF ::Button .OR. ( ::ContextArrow .AND. ::ContextMenu != NIL ) .OR. ( ::Parent:ImageList != NIL .AND. ::ImageIndex > 0 )
         nccs := (struct NCCALCSIZE_PARAMS)
         nccs:Pointer( nlParam )

         IF ::Button 
            nccs:rgrc[1]:right -= 16
          ELSE      
            n := 2
            IF valtype( ::Parent:ImageList ) == "O" .AND. ::ImageIndex > 0
               ::nImageSize := ::Parent:ImageList:IconWidth
            ENDIF
            IF ::ContextArrow .AND. ::ContextMenu != NIL
               ::nArrowSize := 13
            ENDIF
            DO CASE 
               CASE ::xLayout == 2 // "Text, Image, Arrow"
                    nccs:rgrc[1]:right -= ( ::nArrowSize + ::nImageSize )
                    ::__aImagePos := { ::Width - ( ::nImageSize + ::nArrowSize ) - n, ::nImageSize }
                    ::__aArrowPos := { ::Width - ::nArrowSize - n, ::nArrowSize }

               CASE ::xLayout == 3 // "Text, Arrow, Image"
                    nccs:rgrc[1]:right -= ( ::nArrowSize + ::nImageSize )
                    ::__aArrowPos := { ::Width - ( ::nArrowSize + ::nImageSize ) - n, ::nArrowSize }
                    ::__aImagePos := { ::Width - ::nImageSize - n, ::nImageSize }

               CASE ::xLayout == 4 // "Image, Text, Arrow"
                    nccs:rgrc[1]:left  += ::nImageSize
                    nccs:rgrc[1]:right -= ::nArrowSize
                    ::__aImagePos := { n, ::nImageSize }
                    ::__aArrowPos := { ::Width - ::nArrowSize - n, ::nArrowSize }

               CASE ::xLayout == 5 // "Image, Arrow, Text"
                    nccs:rgrc[1]:left  += ( ::nArrowSize + ::nImageSize )
                    ::__aImagePos := { n, ::nImageSize }
                    ::__aArrowPos := { n + ::nImageSize, ::nArrowSize }

               CASE ::xLayout == 6 // "Arrow, Text, Image"
                    nccs:rgrc[1]:left  += ::nArrowSize
                    nccs:rgrc[1]:right -= ::nImageSize
                    ::__aArrowPos := { n, ::nArrowSize }
                    ::__aImagePos := { ::Width - ::nImageSize - n, ::nImageSize }

               CASE ::xLayout == 7 // "Arrow, Image, Text"
                    nccs:rgrc[1]:left  += ( ::nArrowSize + ::nImageSize )
                    ::__aArrowPos := { n, ::nArrowSize, -( ::nArrowSize + ::nImageSize ) }
                    ::__aImagePos := { n + ::nArrowSize, ::nImageSize }
            ENDCASE
         ENDIF 
         nccs:CopyTo( nlParam )
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnNCPaint( nwParam, nlParam ) CLASS EditBox
   LOCAL aRect, nWidth := 0, hBrush, hdc, nLeft, nTop, hRegion, pWi, nStyle, n := 3

   IF ::Button .OR. ( ::xLayout > 1 .AND. ( ::__aArrowPos[2] > 0 .OR. ::__aImagePos[2] > 0 ) )


//      _FillRect( hDC, { 1, 1, ::Width-1, ::Height-1 }, hBrush )
   
      nStyle := GetWindowLong( ::hWnd, GWL_EXSTYLE )
      IF nStyle & WS_EX_CLIENTEDGE == 0
         n := 0
      ENDIF

      IF ::Button
      
         aRect := {::Width-16-n, n, ::Width-n, ::Height-n}
         hRegion := CreateRectRgn( aRect[1], aRect[2], aRect[3], aRect[4] )
         hdc := GetDCEx( ::hWnd, hRegion, DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE )
         
         _DrawFrameControl( hDC, aRect, DFC_BUTTON, DFCS_BUTTONPUSH )
         SetBkMode( hDC, TRANSPARENT )
         _DrawText( hDC, "...", aRect, DT_CENTER + DT_SINGLELINE + DT_VCENTER )

         ReleaseDC(::hWnd, hdc)
         DeleteObject( hRegion )
       ELSE

         hBrush := ::BkBrush
         IF ::HasFocus .AND. ::SelBkBrush != NIL
            hBrush := ::SelBkBrush
         ENDIF
         DEFAULT hBrush TO GetSysColorBrush( COLOR_WINDOW )
      
         IF ::__aArrowPos[2] > 0

            aRect := { ::__aArrowPos[1], 1, ::__aArrowPos[1] + ::__aArrowPos[2], ::Height-1 }
            hRegion := CreateRectRgn( aRect[1], aRect[2], aRect[3], aRect[4] )
            hdc := GetDCEx( ::hWnd, hRegion, DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE )

            _FillRect( hDC, aRect, hBrush )

            SelectObject( hDC, GetStockObject( BLACK_PEN ) )
            nLeft := ::__aArrowPos[1] + 9
            nTop  := (::Height / 2)-2

            FOR n := 1 TO 4
                MoveTo( hDC, nLeft - ( 6 - n ), nTop + (n-1) )
                LineTo( hDC, nLeft + ( 3 - n ), nTop + (n-1) )
            NEXT
            ReleaseDC(::hWnd, hdc)
            DeleteObject( hRegion )
         ENDIF
         
         IF ::__aImagePos[2] > 0
            aRect := { ::__aImagePos[1], 1, ::__aImagePos[1] + ::__aImagePos[2], ::Height-1 }
            hRegion := CreateRectRgn( aRect[1], aRect[2], aRect[3], aRect[4] )
            hdc := GetDCEx( ::hWnd, hRegion, DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE )
            nTop  := ( ::Height - ::Parent:ImageList:IconHeight ) / 2
            _FillRect( hDC, aRect, hBrush )
            ::Parent:ImageList:DrawImage( hDC, ::ImageIndex, ::__aImagePos[1], nTop )
            ReleaseDC(::hWnd, hdc)
            DeleteObject( hRegion )
         ENDIF

      ENDIF
      

   ENDIF
   
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnNCLButtonDown( nwParam, x, y ) CLASS EditBox
   LOCAL nWidth, nStyle, hRegion, hdc, aRect, n := 3
   LOCAL nLeft, rc, pt, aPt := {x,y}

   IF ::xLayout == 1 .AND. !::Button
      RETURN NIL
   ENDIF
   
   IF nwParam == HTHELP
      _ScreenToClient( ::hWnd, @aPt )
      
      IF ::Button 
         ::ButtonPushed := .T.
         nStyle := GetWindowLong( ::hWnd, GWL_EXSTYLE )
         IF nStyle & WS_EX_CLIENTEDGE == 0
            n := 0
         ENDIF

         hRegion := CreateRectRgn( ::Width-16-n, n, ::Width-n, ::Height-n )
         hdc := GetDCEx( ::hWnd, hRegion, DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE )

         _DrawFrameControl( hDC, {::Width-16-n, n, ::Width-n, ::Height-n}, DFC_BUTTON, DFCS_BUTTONPUSH | DFCS_PUSHED )
         SetBkMode( hDC, TRANSPARENT )
         _DrawText( hDC, "...", {::Width-16-n, n, ::Width-n, ::Height-n}, DT_CENTER | DT_SINGLELINE | DT_VCENTER )

         ReleaseDC(::hWnd, hdc)
         DeleteObject( hRegion )

       ELSEIF ::__aArrowPos[2] > 0
         DO CASE 

            CASE ::xLayout == 3 // "Image, Text, Arrow"
                 x += ::nImageSize

            CASE ::xLayout == 4 // "Image, Arrow, Text"
                 x += ( ::nImageSize + ::nArrowSize )

            CASE ::xLayout == 5 // "Arrow, Text, Image"
                 x += ::nArrowSize

            CASE ::xLayout == 6 // "Arrow, Image, Text"
                 x += ( ::nImageSize + ::nArrowSize )
         ENDCASE

         rc := (struct RECT)
         rc:left   := ::__aArrowPos[1]
         rc:top    := 0
         rc:right  := ::__aArrowPos[1] + ::__aArrowPos[2]
         rc:bottom := ::Height 
         
         pt := (struct POINT)
         pt:x := x
         pt:y := y
         
         ScreenToClient( ::hWnd, @pt )

         IF PtInRect( rc, pt )

            pt := (struct POINT)
            pt:x := ::left
            pt:y := ::top
            
            IF ::xLayout < 4
               ::ContextMenu:Menu:Style := TPM_RIGHTALIGN | TPM_TOPALIGN
               pt:x := ::left + ::Width
            ENDIF
            
            ClientToScreen( ::Parent:hWnd, @pt )
            
            ::ContextMenu:Show( pt:x, pt:y + ::Height - 1)
            RETURN 0
         ENDIF
      ENDIF
   ENDIF
RETURN nwParam

//---------------------------------------------------------------------------------------------------
METHOD OnNCLButtonUp( nwParam, x, y ) CLASS EditBox
   LOCAL nLeft, nStyle, hRegion, hdc, aRect, n := 3
   LOCAL aPt := {x,y}

   _ScreenToClient( ::hWnd, @aPt )
   IF ::Button

      IF ::ButtonPushed .AND. _PtInRect( {::ClientWidth, 0, ::ClientWidth+16, ::ClientHeight}, aPt )
         ::ButtonPushed := .F.
         nStyle := GetWindowLong( ::hWnd, GWL_EXSTYLE )
         IF nStyle & WS_EX_CLIENTEDGE == 0
            n := 0
         ENDIF

         hRegion := CreateRectRgn( ::Width-16-n, n, ::Width-n, ::Height-n )
         hdc := GetDCEx( ::hWnd, hRegion, DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE )

         _DrawFrameControl( hDC, {::Width-16-n, n, ::Width-n, ::Height-n}, DFC_BUTTON, DFCS_BUTTONPUSH )
         SetBkMode( hDC, TRANSPARENT )
         _DrawText( hDC, "...", {::Width-16-n, n, ::Width-n, ::Height-n}, DT_CENTER | DT_SINGLELINE | DT_VCENTER )

         ReleaseDC(::hWnd, hdc)
         DeleteObject( hRegion )

         IF ::ButtonAction != NIL
            EVAL( ::ButtonAction, Self )
         ENDIF
      ENDIF
    ELSEIF ::__aImagePos[2] > 0
      ExecuteEvent( "OnImageClick", Self )
   ENDIF
RETURN nwParam

//---------------------------------------------------------------------------------------------------
METHOD OnNCHitTest( x, y ) CLASS EditBox
   LOCAL nStyle, hRegion, hdc, aPt2, aRect, aPt1, aPt := {x,y}, n := 3
   
   IF ::Button .OR. ::__aArrowPos[2] > 0 .OR. ::__aImagePos[2] > 0 
      _ScreenToClient( ::hWnd, @aPt )
      
      IF aPt[1] < 0 .OR. aPt[1] > ::ClientWidth
         RETURN HTHELP
      ENDIF
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnCtlColorEdit( nwParam, nlParam ) CLASS EditBox
   LOCAL hBrush, nFore, nBack, hOldParentMapMode, hOldThisMapMode, hParentDC, hMemDC, hMemBitmap, hOldBitmap, pt := (struct POINT)

   nFore := ::ForeColor
   nBack := ::BackColor

   IF ::HasFocus
      nFore := ::SelForeColor
      nBack := ::SelBackColor
      DEFAULT nFore TO ::ForeColor
      DEFAULT nBack TO ::BackColor
   ENDIF

   IF nFore != NIL
      SetTextColor( nwParam, nFore )
   ENDIF
   IF nBack != NIL
      SetBkColor( nwParam, nBack )
   ENDIF

   IF ::Transparent
      IF ::__hBrush != NIL
         SelectObject( nwParam, ::__hBrush )
         IF ::ReadOnly
            SetBkMode( nwParam, TRANSPARENT )
         ENDIF
         RETURN ::__hBrush
      ENDIF
   ENDIF

   hBrush := ::BkBrush

   IF ::HasFocus .AND. ::SelBkBrush != NIL
      hBrush := ::SelBkBrush
   ENDIF
   IF hBrush != NIL
      IF ::ReadOnly
         SetBkMode( nwParam, TRANSPARENT )
      ENDIF
      RETURN hBrush
   ENDIF
   DEFAULT hBrush TO GetSysColorBrush( COLOR_WINDOW )
RETURN hBrush

//-----------------------------------------------------------------------------------------------
METHOD GetCueBanner() CLASS EditBox
   LOCAL cWText := Replicate( chr(0), 2048 )
   IF SendMessage( ::hWnd, EM_GETCUEBANNER, 0, @cWText )
      RETURN cWText
   ENDIF
RETURN(NIL)

//-----------------------------------------------------------------------------------------------
METHOD GetLine( nLine, cLine ) CLASS EditBox
   LOCAL nLen := Int( Len( cline ) / 2  )
   cLine := I2Bin( nLen )+ Replicate( chr(0), (nLen-1)*2 )
RETURN SendMessage( ::hWnd, EM_GETLINE, nLine, @cLine )

//-----------------------------------------------------------------------------------------------
METHOD GetEditRect() CLASS EditBox
   LOCAL rc := (struct RECT)
   SendMessage( ::hWnd, EM_GETRECT, 0, @rc )
RETURN {rc:Left, rc:Top, rc:Right, rc:Bottom }

//-----------------------------------------------------------------------------------------------
METHOD GetSel( nStart, nEnd ) CLASS EditBox
   LOCAL cStart
   LOCAL cEnd
   LOCAL nRet

   DEFAULT nStart TO 0
   DEFAULT nEnd   TO 0

   cStart := L2Bin( nStart)
   cEnd   := L2Bin( nEnd )

   nRet := SendMessage( ::hWnd, EM_GETSEL, @cStart, @cEnd )

   nStart := Bin2L( cStart )
   nEnd   := Bin2L( cEnd )
RETURN nRet


//-----------------------------------------------------------------------------------------------
METHOD SetRect( aRect, lRelative ) CLASS EditBox
   LOCAL rc := (struct RECT)

   rc:Left   := aRect[1]
   rc:Top    := aRect[2]
   rc:Right  := aRect[3]
   rc:Bottom := aRect[4]

   DEFAULT lRelative TO FALSE
RETURN SendMessage( ::hwnd, EM_SETRECT, lRelative, rc )

//-----------------------------------------------------------------------------------------------
METHOD SetRectNP( aRect, lRelative ) CLASS EditBox
   LOCAL rc := (struct RECT)

   rc:Left   := aRect[1]
   rc:Top    := aRect[2]
   rc:Right  := aRect[3]
   rc:Bottom := aRect[4]

   DEFAULT lRelative TO FALSE
RETURN SendMessage( ::hWnd, EM_SETRECTNP, lRelative, rc )

//-----------------------------------------------------------------------------------------------
METHOD SetTabStops( nTabs, aTabs ) CLASS EditBox
   LOCAL cSize := ""
   LOCAL n
   FOR EACH n IN aTabs
       cSize += L2Bin( n )
   NEXT
RETURN ::SendMessage( EM_SETTABSTOPS, nTabs, cSize )

//-----------------------------------------------------------------------------------------------
METHOD ShowBalloonTip( cTitle, cText, nIcon ) CLASS EditBox
   LOCAL ebt := (STRUCT EDITBALLOONTIP)
   ebt:cbStruct := ebt:SizeOf()

   IF Valtype(cTitle)=="C"
      ebt:pszTitle := cTitle
   ENDIF
   IF Valtype( cText)=="C"
      ebt:pszText := cText
   ENDIF
   DEFAULT nIcon TO TTI_NONE
   ebt:ttiIcon := nIcon
RETURN SendMessage( ::hWnd, EM_SHOWBALLOONTIP, 0, ebt )

//-----------------------------------------------------------------------------------------------
METHOD OnParentCommand( nId, nCode, nlParam ) CLASS EditBox
   LOCAL nRet
   DO CASE
      CASE nCode ==EN_ALIGN_LTR_EC
         nRet := ExecuteEvent( "OnEn_Align_Ltr_Ec", Self )

      CASE nCode ==EN_ALIGN_RTL_EC
         nRet := ExecuteEvent( "OnEn_Align_Rtl_Ec", Self )

      CASE nCode ==EN_CHANGE
         nRet := ExecuteEvent( "OnEn_Change", Self )

      CASE nCode ==EN_ERRSPACE
         nRet := ExecuteEvent( "OnEn_ErrSpace", Self )

      CASE nCode ==EN_HSCROLL
         nRet := ExecuteEvent( "OnEn_HScroll", Self )

      CASE nCode ==EN_KILLFOCUS
         nRet := ExecuteEvent( "OnEn_KillFocus", Self )

      CASE nCode ==EN_MAXTEXT
         nRet := ExecuteEvent( "OnEn_MaxText", Self )

      CASE nCode ==EN_SETFOCUS
         nRet := ExecuteEvent( "OnEn_SetFocus", Self )

      CASE nCode ==EN_UPDATE
         nRet := ExecuteEvent( "OnEn_Update", Self )

      CASE nCode ==EN_VSCROLL
         nRet := ExecuteEvent( "OnEn_VScroll", Self )

   END
   IF VALTYPE( nRet ) == "O"
      nRet := NIL
   ENDIF
RETURN IIF( nRet == NIL, 0, nRet )

//-----------------------------------------------------------------------------------------------
METHOD OnKeyDown( nKey ) CLASS EditBox
   LOCAL h, lShift, oEdit
   ::LastKey := nKey
   IF ::Transparent
      ::InvalidateRect(, .F.)
   ENDIF
   
   IF ::Style & ES_MULTILINE == ES_MULTILINE .AND. nKey == VK_TAB
      lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
      IF ( h := GetNextDlgTabItem( ::Form:hWnd, ::hWnd, lShift ) ) # 0
         IF ::Form:Modal
            PostMessage( ::Form:hWnd, WM_NEXTDLGCTL, h, MAKELPARAM( 1, 0 ) )
          ELSE
            SetFocus(h)
         ENDIF
      ENDIF
   ENDIF
   IF ::DataSource != NIL .AND. ::__oDataGrid != NIL .AND. ::__oDataGrid:Height > 0
      SWITCH nKey 
         CASE VK_DELETE
            ::CallWindowProc()
            ::__UpdateDataGrid()
            RETURN 0
            
         CASE VK_DOWN
            ::__oDataGrid:SetFocus()
            ::DataSource:GoTop()
            ::__oDataGrid:Update()
            RETURN 0
            
         CASE VK_ESCAPE
            ::__oDataGrid:Height := 0
            RETURN 0
      END
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnChar( nKey, nlParam ) CLASS EditBox
   LOCAL aKeys := {VK_BACK}
   IF ::DataSource != NIL .AND. ( ( nKey >= 32 .AND. nKey <= 168 ) .OR. ( nKey >= 224 .AND. nKey <= 253 ) .OR. ( nKey IN aKeys ) )
      ::CallWindowProc()
      ::__UpdateDataGrid()
      RETURN 0
   ENDIF
RETURN NIL

CLASS Edit INHERIT EditBox
ENDCLASS