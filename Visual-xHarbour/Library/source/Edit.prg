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

#define CS_DROPSHADOW 0x00020000


#Define WM_CARET   WM_USER + 51

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

//-----------------------------------------------------------------------------------------------
CLASS EditBox INHERIT Control
   PROPERTY Transparent       ROOT "Appearance"                                            DEFAULT .F.
   PROPERTY MenuArrow         ROOT "Appearance" SET ::__SetMenuArrow(v)                    DEFAULT .F.
   PROPERTY Layout            ROOT "Appearance" SET ::__SetLayout(v)                       DEFAULT 1
   PROPERTY Password          ROOT "Appearance" SET ::SetStyle( ES_PASSWORD, v )           DEFAULT .F.
   PROPERTY Case              ROOT "Appearance" SET ::SetCase(v)                           DEFAULT 1
   PROPERTY CueBanner         ROOT "Appearance" SET ::SetCueBanner(v)
   PROPERTY ImageList         ROOT "Appearance" GET __ChkComponent( Self, @::xImageList )
   PROPERTY ImageIndex        ROOT "Appearance" SET ::__SetImageIndex(v)                   DEFAULT 0

   PROPERTY DropCalendar      ROOT "Behavior"                                              DEFAULT .F.
   PROPERTY FullSelectOnClick ROOT "Behavior"                                              DEFAULT .F.
   PROPERTY EnterNext         ROOT "Behavior"                                              DEFAULT .F.
   PROPERTY AutoVScroll       ROOT "Behavior"   SET ::__SetAutoScroll( ES_AUTOVSCROLL, v ) DEFAULT .F.
   PROPERTY AutoHScroll       ROOT "Behavior"   SET ::__SetAutoScroll( ES_AUTOHSCROLL, v ) DEFAULT .F.
   PROPERTY MultiLine         ROOT "Behavior"   SET ::SetStyle( ES_MULTILINE, v )          DEFAULT .F.
   PROPERTY NoHideSel         ROOT "Behavior"   SET ::SetStyle( ES_NOHIDESEL, v )          DEFAULT .F.
   PROPERTY OemConvert        ROOT "Behavior"   SET ::SetStyle( ES_OEMCONVERT, v )         DEFAULT .F.
   PROPERTY ReadOnly          ROOT "Behavior"   SET ::SetReadOnly( v )                     DEFAULT .F. HELP "Prevents the user from typing or editing text in the edit control."
   PROPERTY WantReturn        ROOT "Behavior"   SET ::SetStyle( ES_WANTRETURN, v )         DEFAULT .F.
   PROPERTY HorzScroll        ROOT "Behavior"   SET ::SetStyle( WS_HSCROLL, v )            DEFAULT .F.
   PROPERTY VertScroll        ROOT "Behavior"   SET ::SetStyle( WS_VSCROLL, v )            DEFAULT .F.
   PROPERTY Number            ROOT "Behavior"   SET ::SetStyle( ES_NUMBER, v )             DEFAULT .F.
   PROPERTY ContextMenu       ROOT "Behavior"   GET __ChkComponent( Self, @::xContextMenu ) SET ::__SetContextMenu(v)
   PROPERTY Alignment         ROOT "Behavior"   SET ::SetAlignment(v)                      DEFAULT 1
   PROPERTY MaxChars          ROOT "Behavior"   SET ::SetLimitText(v)                      DEFAULT 0

   PROPERTY SelForeColor      ROOT "Colors"
   PROPERTY SelBackColor      ROOT "Colors"     SET ::__SetSelColor(v)
   PROPERTY BorderColor       ROOT "Colors"

   PROPERTY DataSearchField   ROOT "Data"                                                  DEFAULT 1
   PROPERTY DataSearchWidth   ROOT "Data"                                                  DEFAULT 0
   PROPERTY DataSearchRecords ROOT "Data"                                                  DEFAULT 0
   PROPERTY DataSource        ROOT "Data"       GET __ChkComponent( Self, @::xDataSource )

   DATA EnumLayout                     EXPORTED INIT { { "None",;
                                                         "Text, Image, Arrow",;
                                                         "Text, Arrow, Image",;
                                                         "Image, Text, Arrow",;
                                                         "Image, Arrow, Text",;
                                                         "Arrow, Text, Image",;
                                                         "Arrow, Image, Text" }, {1,2,3,4,5,6,7} }

   DATA AllowUnDock                    EXPORTED INIT FALSE
   DATA AllowClose                     EXPORTED INIT FALSE
   DATA Button                         EXPORTED INIT .F.
   DATA ButtonAction                   EXPORTED
   DATA CalendarWindow                 EXPORTED
   DATA __SysBackColor                 EXPORTED INIT GetSysColor( COLOR_WINDOW )
   DATA __SysForeColor                 EXPORTED INIT GetSysColor( COLOR_WINDOWTEXT )
   DATA LastKey                        EXPORTED INIT 0

   DATA EnumCase                       EXPORTED  INIT { { "Mixed Case", "Upper Case", "Lower Case" }, {1,2,3} }
   DATA EnumAlignment                  EXPORTED  INIT { { "Left", "Center", "Right" }, { 1, 2, 3 } }

   DATA __BackMargin                   EXPORTED INIT 2

   ACCESS Modified                     INLINE ::SendMessage( EM_GETMODIFY, 0, 0 ) == 1
   ASSIGN Modified(l)                  INLINE ::SendMessage( EM_SETMODIFY, l, 0 )

   DATA __ButtonPushed                 PROTECTED INIT .F.
   DATA __aArrowPos                    PROTECTED INIT {0,0}
   DATA __aImagePos                    PROTECTED INIT {0,0}
   DATA __oDataGrid                    PROTECTED
   DATA __BkCursor                     PROTECTED
   DATA __nImageSize                   PROTECTED INIT 0
   DATA __nArrowSize                   PROTECTED INIT 0

   ACCESS ContextArrow                 INLINE ::xMenuArrow
   ASSIGN ContextArrow(l)              INLINE ::MenuArrow(l)

   ACCESS InDataGrid                   INLINE ::Parent:ClsName == "DataGrid"

   METHOD Init()     CONSTRUCTOR
   METHOD Create()

   METHOD SetCase()
   METHOD SetAlignment()

   METHOD OnCtlColorEdit()
   METHOD OnCtlColorStatic()

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
   METHOD SetCueBanner()
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
   METHOD OnEraseBkGnd()
   METHOD SetParent( oParent )         INLINE ::Super:SetParent( oParent ), ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
   METHOD SetRect(aRect, lRelative)
   METHOD SetRectNP(aRect, lRelative)
   METHOD SetTabStops( nTabs, aTabs )
   METHOD ShowBalloonTip( EBT )
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
   METHOD __SetSelColor()
   METHOD OnKeyDown()
   METHOD OnGetDlgCode()
   METHOD OnKillFocus()

   METHOD OnSetFocus()                 INLINE ::InvalidateRect(,.F.), NIL
   METHOD OnLButtonDown()
   METHOD OnChar()
   METHOD OnPaste()
   METHOD OnUndo()
   METHOD OnCut()
   METHOD OnContextMenu()              INLINE IIF( ::MenuArrow, ( ::CallWindowProc(), 0 ), NIL )
   METHOD __SetLayout()
   METHOD __SetImageIndex()
   METHOD __UpdateDataGrid()
   METHOD __ChkGridKeys()
   METHOD OnNCMouseMove()
   METHOD OnMouseMove()
   METHOD __SetAutoScroll()
   METHOD __SetMenuArrow()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS EditBox
   DEFAULT ::__xCtrlName TO "EditBox"
   ::ClsName   := "Edit"
   ::ThemeName := "EDIT"

   ::Style        := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS) //| IIF( ! ::Application:IsThemedXP, WS_BORDER, 0 )
   ::Border       := WS_EX_CLIENTEDGE

   ::Super:Init( oParent )
   ::Width        := 80
   ::Height       := 22

   IF ::DesignMode
      ::__PropFilter := { "ALLOWMAXIMIZE" }
      ::Events := ;
             { ;
               {"Object",      {;
                               { "OnInit"             , "", "" } } },;
               {"Color",       {;
                               { "OnCtlColorEdit"     , "", "" },;
                               { "OnSysColorChange"   , "", "" } } },;
               {"Data",        {;
                               { "OnDataSelected"     , "", "" } } },;
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
               {"Scroll",      {;
                               { "OnHorzScroll"       , "", "" },;
                               { "OnVertScroll"       , "", "" } } },;
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
   ::bSetValue := {|cValue| ::Text := AllTrim(cValue), IIF( ValType(::bOnSetValue)=="B", Eval( ::bOnSetValue, Self, cValue ),) }
   ::bGetValue := {|| AllTrim(::Text) }
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetMenuArrow() CLASS EditBox
   IF ::xLayout == 0 .AND. ::xMenuArrow
      ::xLayout := 1
   ENDIF
   IF ::IsWindow()
      //::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
      ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS EditBox
   LOCAL pWi, n, oImageList
   ::Super:Create()
   pWi := ::GetWindowInfo()
   ::__BackMargin += pWi:cxWindowBorders

   IF ::xLayout > 1 .OR. ::Button
      oImageList := ::ImageList
      DEFAULT oImageList TO ::Parent:ImageList
      IF ::Button .OR. /*::DropCalendar .OR.*/ ::MenuArrow .OR. ( oImageList != NIL .AND. ::ImageIndex > 0 )
         ::SetWindowPos(, 0, 0, 0, 0, (SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER) )
      ENDIF
   ENDIF
   IF ! ::DesignMode
      IF ( n := ASCAN( ::Parent:Children, {|o| o:ClsName == UPDOWN_CLASS .AND. VALTYPE(o:xBuddy)=="C" .AND. o:xBuddy == ::Name } ) ) > 0
         ::Parent:Children[n]:xBuddy := Self
      ENDIF
      IF ( n := ASCAN( ::Parent:Children, {|o| o:ClsName == UPDOWN_CLASS .AND. o:xBuddy != NIL .AND. o:xBuddy == Self} ) ) > 0
         ::Parent:Children[n]:__SetBuddy()
      ENDIF
   ENDIF
   IF ! Empty( ::xCueBanner )
      ::SetCueBanner()
   ENDIF
   ::__SetAutoScroll( ES_AUTOVSCROLL, ::xAutoVScroll )
   ::__SetAutoScroll( ES_AUTOHSCROLL, ::xAutoHScroll )
   ::SetLimitText( ::MaxChars )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnEraseBkGnd() CLASS EditBox
   IF ::Transparent
      RETURN 1
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD SetCueBanner( cText, lFocus ) CLASS EditBox
   DEFAULT cText TO ::xCueBanner
   IF ! ::DesignMode
      IF VALTYPE( cText )=="C" .AND. LEFT( cText, 2 ) == "{|"
         cText := &cText
      ENDIF
      IF VALTYPE( cText ) == "B"
         cText := EVAL(cText)
      ENDIF
   ENDIF
   IF ::IsWindow()
      EditSetCueBannerText( ::hWnd, IIF( lFocus == NIL, .F., lFocus ), cText )
   ENDIF
RETURN Self


//-----------------------------------------------------------------------------------------------
METHOD __SetAutoScroll( nIndex, lSet ) CLASS EditBox
   ::SetStyle( nIndex, lSet )
   //::SetWindowPos(, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __UpdateDataGrid() CLASS EditBox
   LOCAL nRecs, cData, nOrder

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
         :Border      := WS_BORDER
         :ShowSelectionBorder := .F.

         :OnWMKeyDown := {|o, nKey| ::__ChkGridKeys( o, nKey ) }
         :OnWMGetDlgCode := {|| DLGC_WANTALLKEYS }
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

//   TRY
//      nOrder := ::DataSource:SetOrder( ::DataSearchField )
//   CATCH
//   END
   ::DataSource:Seek( ::Text )
   WHILE ( nRecs < ::DataSearchRecords .OR. ::DataSearchRecords == 0 ) .AND. !::DataSource:Eof()
      cData := ALLTRIM( ::DataSource:Fields:FieldGet( ::DataSearchField ) )
      IF ( UPPER( ALLTRIM( ::Caption ) ) IN UPPER( cData ) )
         AADD( ::__oDataGrid:DataSource:Table, { cData } )
      ENDIF
      nRecs ++
      ::DataSource:Skip()
   ENDDO
   IF nOrder != NIL
      ::DataSource:SetOrder( nOrder )
   ENDIF

   nRecs := MIN( nRecs, LEN( ::__oDataGrid:DataSource:Table ) )

   ::__oDataGrid:Height := MIN( (nRecs * ::__oDataGrid:ItemHeight)+2, ::Parent:ClientHeight - ::__oDataGrid:Top )
   IF ::__oDataGrid:Height < ::__oDataGrid:ItemHeight
      ::__oDataGrid:Height := 0
   ENDIF

   ::__oDataGrid:DataSource:GoTop()
   ::__oDataGrid:Update()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnKillFocus() CLASS EditBox
   ::InvalidateRect(,.F.)
   IF ::__oDataGrid != NIL .AND. ::__oDataGrid:Height > 0 .AND. ! ( ::LastKey IN {VK_UP,VK_DOWN,VK_ESCAPE,VK_RETURN} )
      ::__ChkGridKeys( NIL, VK_RETURN, .F. )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD __ChkGridKeys( o, nKey, lFocus ) CLASS EditBox
   IF nKey == VK_UP
      IF o:GetPosition() == 1
         ::SetFocus()
      ENDIF
    ELSEIF nKey == VK_RETURN
      DEFAULT lFocus TO .T.
      ::Caption := ALLTRIM( ::__oDataGrid:DataSource:Fields:FieldGet( 1 ) )
      ExecuteEvent( "OnDataSelected", Self )
      ::__oDataGrid:Height := 0
      IF lFocus
         ::SetFocus()
         ::PostMessage( WM_KEYDOWN, VK_END )
      ENDIF
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
   LOCAL n, nRet
   IF ::wParam == VK_RETURN .AND. ::Parent:ClsName != "DataGrid"
      IF ( n := HSCAN( ::Form:__hObjects, {|a,o| (a), o:__xCtrlName == "Button" .AND. o:IsWindowVisible() .AND. o:DefaultButton } ) ) > 0
         nRet := ExecuteEvent( "OnClick", HGetValueAt( ::Form:__hObjects, n ) )
         RETURN NIL
      ENDIF
      IF ::EnterNext
         ::LastKey := ::wParam
         ::PostMessage( WM_KEYDOWN, VK_TAB, ::lParam )
         RETURN 0
      ENDIF
   ENDIF
RETURN IIF( ::Multiline .AND. ::wParam == VK_ESCAPE, 0, NIL )

//-----------------------------------------------------------------------------------------------
METHOD __SetLayout() CLASS EditBox
   IF ::IsWindow() .AND. ::DesignMode
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetImageIndex(n) CLASS EditBox
   IF ::DesignMode .AND. n > 0 .AND. ::xLayout == 1
      ::Layout := 2
      IF ::DesignMode
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
   FOR x := 1 TO LEN( ::EnumAlignment )
       IF x != n
          ::SetStyle( x-1, .F. )
       ENDIF
   NEXT
   ::SetStyle( n-1, .T. )
RETURN n

//-----------------------------------------------------------------------------------------------
METHOD __SetSelColor( nColor ) CLASS EditBox
   IF ::SelBkBrush != NIL
      DeleteObject( ::SelBkBrush )
      ::SelBkBrush := NIL
   ENDIF
   IF nColor != NIL
      ::SelBkBrush := CreateSolidBrush( nColor )
   ENDIF
   IF ::IsWindowVisible() .AND. GetFocus() == ::hWnd
      ::InvalidateRect()
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------
METHOD OnCtlColorStatic( nwParam ) CLASS EditBox
   LOCAL hBkGnd := ::GetBkBrush()

   IF ::ForeColor != NIL .AND. ::ForeColor != ::__SysForeColor
      SetTextColor( nwParam, ::ForeColor )
   ENDIF
   IF hBkGnd != NIL
      RETURN hBkGnd
    ELSEIF ::ForeColor != NIL .AND. ::ForeColor != ::__SysForeColor
      SetBkMode( nwParam, TRANSPARENT )
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD __SetContextMenu() CLASS EditBox
   //::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER )
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnMouseMove( nwParam, nlParam ) CLASS EditBox
   ::Super:OnMouseMove( nwParam, nlParam )
   IF ::xLayout > 1 .AND. ::__BkCursor != NIL
      ::__hCursor := IIF( ::__BkCursor == 0, NIL, ::__BkCursor )
      ::__BkCursor := NIL
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnNCMouseMove() CLASS EditBox
   LOCAL oImageList
   ::Super:OnNCMouseMove()
   IF ::xLayout > 1 .AND. ::__BkCursor == NIL
      oImageList := ::ImageList
      DEFAULT oImageList TO ::Parent:ImageList

      IF oImageList != NIL .AND. ::ImageIndex > 0
         ::__BkCursor := ::__hCursor
         DEFAULT ::__BkCursor TO 0
         ::Cursor := ::System:Cursor:LinkSelect
      ENDIF
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnNCCalcSize( nwParam, nlParam ) CLASS EditBox
   LOCAL n, nccs, oImageList
   (nwParam)
   ::__nImageSize := 0
   ::__nArrowSize := 0

   IF ::xLayout > 1 .OR. ::Button
      oImageList := ::ImageList
      DEFAULT oImageList TO ::Parent:ImageList
      IF ::Button .OR. /*::DropCalendar .OR.*/ ::MenuArrow .OR. ( oImageList != NIL .AND. ::ImageIndex > 0 )
         nccs := (struct NCCALCSIZE_PARAMS)
         nccs:Pointer( nlParam )

         IF ::Button
            nccs:rgrc[1]:right -= 16
          ELSE
            n := 2
            IF valtype( oImageList ) == "O" .AND. ::ImageIndex > 0
               ::__nImageSize := oImageList:IconWidth
            ENDIF
            IF /*::DropCalendar .OR.*/ ::MenuArrow
               ::__nArrowSize := 13
            ENDIF
            DO CASE
               CASE ::xLayout == 2 // "Text, Image, Arrow"
                    nccs:rgrc[1]:right -= ( ::__nArrowSize + ::__nImageSize )
                    ::__aImagePos := { ::Width - ( ::__nImageSize + ::__nArrowSize ) - n, ::__nImageSize }
                    ::__aArrowPos := { ::Width - ::__nArrowSize - n, ::__nArrowSize }

               CASE ::xLayout == 3 // "Text, Arrow, Image"
                    nccs:rgrc[1]:right -= ( ::__nArrowSize + ::__nImageSize )
                    ::__aArrowPos := { ::Width - ( ::__nArrowSize + ::__nImageSize ) - n, ::__nArrowSize }
                    ::__aImagePos := { ::Width - ::__nImageSize - n, ::__nImageSize }

               CASE ::xLayout == 4 // "Image, Text, Arrow"
                    nccs:rgrc[1]:left  += ::__nImageSize
                    nccs:rgrc[1]:right -= ::__nArrowSize
                    ::__aImagePos := { n, ::__nImageSize }
                    ::__aArrowPos := { ::Width - ::__nArrowSize - n, ::__nArrowSize }

               CASE ::xLayout == 5 // "Image, Arrow, Text"
                    nccs:rgrc[1]:left  += ( ::__nArrowSize + ::__nImageSize )
                    ::__aImagePos := { n, ::__nImageSize }
                    ::__aArrowPos := { n + ::__nImageSize, ::__nArrowSize }

               CASE ::xLayout == 6 // "Arrow, Text, Image"
                    nccs:rgrc[1]:left  += ::__nArrowSize
                    nccs:rgrc[1]:right -= ::__nImageSize
                    ::__aArrowPos := { n, ::__nArrowSize }
                    ::__aImagePos := { ::Width - ::__nImageSize - n, ::__nImageSize }

               CASE ::xLayout == 7 // "Arrow, Image, Text"
                    nccs:rgrc[1]:left  += ( ::__nArrowSize + ::__nImageSize )
                    ::__aArrowPos := { n, ::__nArrowSize, -( ::__nArrowSize + ::__nImageSize ) }
                    ::__aImagePos := { n + ::__nArrowSize, ::__nImageSize }
            ENDCASE
         ENDIF
         nccs:CopyTo( nlParam )
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnNCPaint() CLASS EditBox
   LOCAL aRect, nWidth := 0, hBrush, hdc, nLeft, nTop, hRegion, nStyle, n := 3, hOldPen, nColor, hOldBrush
   LOCAL oImageList

   IF ::Button .OR. ( ::xLayout > 1 .AND. ( ::__aArrowPos[2] > 0 .OR. ::__aImagePos[2] > 0 ) )
      nStyle := GetWindowLong( ::hWnd, GWL_EXSTYLE )
      IF (nStyle & WS_EX_CLIENTEDGE) == 0
         n := 0
      ENDIF

      IF ::Button

         aRect := {::Width-16-n, n, ::Width-n, ::Height-n}
         hRegion := CreateRectRgn( aRect[1], aRect[2], aRect[3], aRect[4] )
         hdc := GetDCEx( ::hWnd, hRegion, (DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE) )

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
            hdc := GetDCEx( ::hWnd, hRegion, (DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE) )

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
            hdc := GetDCEx( ::hWnd, hRegion, (DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE) )

            oImageList := ::ImageList
            DEFAULT oImageList TO ::Parent:ImageList

            IF oImageList != NIL
               nTop  := ( ::Height - oImageList:IconHeight ) / 2
            ENDIF
            _FillRect( hDC, aRect, IIF( ! ::Enabled, ::Parent:BkBrush, hBrush ) )
            IF oImageList != NIL
               oImageList:DrawImage( hDC, ::ImageIndex, ::__aImagePos[1], nTop )
            ENDIF
            ReleaseDC(::hWnd, hdc)
            DeleteObject( hRegion )
         ENDIF

      ENDIF
   ENDIF

   IF ::HasFocus
      nColor := ::System:EditBoxBorderColor:Focus
    ELSEIF ::__lMouseHover
      nColor := ::System:EditBoxBorderColor:Hover
   ENDIF
   DEFAULT nColor TO ::BorderColor
   DEFAULT nColor TO ::System:EditBoxBorderColor:Normal

   IF nColor != NIL
      hdc       := GetWindowDC( ::hWnd )
      hOldPen   := SelectObject( hDC, CreatePen( PS_SOLID, 1, nColor ) )
      hOldBrush := SelectObject( hDC, GetStockObject( NULL_BRUSH ) )
      Rectangle( hDC, 0, 0, ::Width, ::Height )
      SelectObject( hDC, hOldBrush )
      DeleteObject( SelectObject( hDC, hOldPen ) )
      ReleaseDC(::hWnd, hdc)
      RETURN 0
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnNCLButtonDown( nwParam, nlParam ) CLASS EditBox
   LOCAL nStyle, hRegion, hdc, nAlign, n := 3
   LOCAL rc, pt, x, y, aPt := {LOWORD( nlParam ),HIWORD( nlParam )}

   x := aPt[1]
   y := aPt[2]

   ::Super:OnNCLButtonDown( nwParam, nlParam )

   IF ::xLayout == 1 .AND. !::Button
      RETURN NIL
   ENDIF

   IF nwParam == HTSYSMENU
      _ScreenToClient( ::hWnd, @aPt )

      IF ::Button
         ::__ButtonPushed := .T.
         nStyle := GetWindowLong( ::hWnd, GWL_EXSTYLE )
         IF (nStyle & WS_EX_CLIENTEDGE) == 0
            n := 0
         ENDIF

         hRegion := CreateRectRgn( ::Width-16-n, n, ::Width-n, ::Height-n )
         hdc := GetDCEx( ::hWnd, hRegion, (DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE) )

         _DrawFrameControl( hDC, {::Width-16-n, n, ::Width-n, ::Height-n}, DFC_BUTTON, (DFCS_BUTTONPUSH | DFCS_PUSHED) )
         SetBkMode( hDC, TRANSPARENT )
         _DrawText( hDC, "...", {::Width-16-n, n, ::Width-n, ::Height-n}, (DT_CENTER | DT_SINGLELINE | DT_VCENTER) )

         ReleaseDC(::hWnd, hdc)
         DeleteObject( hRegion )

#ifdef VXH_PROFESSIONAL

       ELSEIF ::DropCalendar

         pt := (struct POINT)
         pt:x := ::Left
         pt:y := ::Top + ::Height
         ClientToScreen( ::Parent:hWnd, @pt )
         WITH OBJECT ( ::CalendarWindow := FloatCalendar( ::Parent ) )
            :Left   := pt:x
            :Top    := pt:y
            :Width  := 200
            :Height := 200
            :Cargo  := Self
            :Create()
         END
         RETURN 0
#endif

       ELSEIF ::__aArrowPos[2] > 0
         DO CASE

            CASE ::xLayout == 3 // "Image, Text, Arrow"
                 x += ::__nImageSize

            CASE ::xLayout == 4 // "Image, Arrow, Text"
                 x += ( ::__nImageSize + ::__nArrowSize )

            CASE ::xLayout == 5 // "Arrow, Text, Image"
                 x += ::__nArrowSize

            CASE ::xLayout == 6 // "Arrow, Image, Text"
                 x += ( ::__nImageSize + ::__nArrowSize )
         ENDCASE

         IF ::ContextMenu != NIL
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
                  nAlign := (TPM_RIGHTALIGN | TPM_TOPALIGN)
                  pt:x := ::left + ::Width
               ENDIF

               ClientToScreen( ::Parent:hWnd, @pt )

               ::ContextMenu:Show( pt:x, pt:y + ::Height - 1, nAlign )
               RETURN 0
            ENDIF
         ENDIF
      ENDIF

   ENDIF
RETURN nwParam

//---------------------------------------------------------------------------------------------------
METHOD OnNCLButtonUp( nwParam, nlParam ) CLASS EditBox
   LOCAL xRet, nStyle, hRegion, hdc, n := 3
   LOCAL aPt := {LOWORD( nlParam ),HIWORD( nlParam )}

   ::Super:OnNCLButtonUp( nwParam, nlParam )

   _ScreenToClient( ::hWnd, @aPt )
   IF ::Button

      IF ::__ButtonPushed .AND. _PtInRect( {::ClientWidth, 0, ::ClientWidth+16, ::ClientHeight}, aPt )
         ::__ButtonPushed := .F.
         nStyle := GetWindowLong( ::hWnd, GWL_EXSTYLE )
         IF (nStyle & WS_EX_CLIENTEDGE) == 0
            n := 0
         ENDIF

         hRegion := CreateRectRgn( ::Width-16-n, n, ::Width-n, ::Height-n )
         hdc := GetDCEx( ::hWnd, hRegion, (DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE) )

         _DrawFrameControl( hDC, {::Width-16-n, n, ::Width-n, ::Height-n}, DFC_BUTTON, DFCS_BUTTONPUSH )
         SetBkMode( hDC, TRANSPARENT )
         _DrawText( hDC, "...", {::Width-16-n, n, ::Width-n, ::Height-n}, (DT_CENTER | DT_SINGLELINE | DT_VCENTER) )

         ReleaseDC(::hWnd, hdc)
         DeleteObject( hRegion )

         IF ::ButtonAction != NIL
            EVAL( ::ButtonAction, Self )
         ENDIF
      ENDIF
    ELSEIF ::__aImagePos[2] > 0
      xRet := ExecuteEvent( "OnImageClick", Self )
   ENDIF
RETURN nwParam

//---------------------------------------------------------------------------------------------------
METHOD OnNCHitTest( x, y ) CLASS EditBox
   LOCAL aPt := {x,y}, n := 3

   IF ::Button .OR. ::__aArrowPos[2] > 0 .OR. ::__aImagePos[2] > 0
      _ScreenToClient( ::hWnd, @aPt )

      IF aPt[1] < 0 .OR. aPt[1] > ::ClientWidth
         RETURN HTSYSMENU
      ENDIF
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnCtlColorEdit( nwParam ) CLASS EditBox
   LOCAL hBrush, oParent, nFore, nBack, n

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

   hBrush := ::BkBrush

   IF ::Transparent
      oParent := IIF( ::Parent:ClsName == "DataGrid", ::Parent:Parent, ::Parent )
      hBrush := oParent:BkBrush
      SelectObject( nwParam, hBrush )
      SetBkMode( nwParam, TRANSPARENT )
      n := (::Width - ::ClientWidth)/2
      SetBrushOrgEx( nwParam, oParent:ClientWidth-::Left-n, oParent:ClientHeight-::Top-n )
      RETURN hBrush
   ENDIF


   IF ::xSelBackColor != NIL
      DEFAULT ::SelBkBrush TO CreateSolidBrush( ::xSelBackColor )
   ENDIF

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

   nRet   := SendMessage( ::hWnd, EM_GETSEL, @cStart, @cEnd )

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
   (nId)
   (nlParam)
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
   LOCAL h, lShift, bChanged
#ifdef VXH_PROFESSIONAL
   LOCAL pt
#endif
   ::LastKey := nKey
   IF nKey == VK_F1 .AND. IsKeyDown( VK_CONTROL ) .AND. HGetPos( ::EventHandler, "OnImageClick" ) > 0
      ExecuteEvent( "OnImageClick", Self )
      RETURN 0
   ENDIF

#ifdef VXH_PROFESSIONAL
   IF nKey == VK_DOWN .AND. ::DropCalendar
      pt := (struct POINT)
      pt:x := ::Left
      pt:y := ::Top + ::Height
      ClientToScreen( ::Parent:hWnd, @pt )
      WITH OBJECT ( ::CalendarWindow := FloatCalendar( ::Parent ) )
         :Left   := pt:x
         :Top    := pt:y
         :Width  := 200
         :Height := 200
         :Cargo  := Self
         :Create()
      END
      RETURN 0
   ENDIF
#endif

   IF ::Transparent
      ::InvalidateRect(, .F.)
   ENDIF

   IF ( ::Style & ES_MULTILINE ) == ES_MULTILINE .AND. nKey == VK_TAB
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
            IF ! ::ReadOnly
               IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
                  bChanged := ::Parent:bChanged
               ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
                  bChanged := ::Form:bChanged
               ENDIF
               IF bChanged != NIL
                  Eval( bChanged, Self )
               ENDIF
            ENDIF
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

   ELSEIF ( nKey IN { VK_DELETE } ) .AND. ! ::ReadOnly
      IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
         bChanged := ::Parent:bChanged
      ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
         bChanged := ::Form:bChanged
      ENDIF
      IF bChanged != NIL
         ::CallWindowProc()
         Eval( bChanged, Self )
         RETURN 0
      ENDIF


   ENDIF

RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnChar( nKey ) CLASS EditBox
   LOCAL lProc := .F., aKeys := {VK_BACK}, bChanged
   IF ::DataSource != NIL .AND. ( ( nKey >= 32 .AND. nKey <= 168 ) .OR. ( nKey >= 224 .AND. nKey <= 253 ) .OR. ( nKey IN aKeys ) )
      ::CallWindowProc()
      ::__UpdateDataGrid()
      lProc := .T.
   ENDIF

   IF ! ::ReadOnly
      IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
         bChanged := ::Parent:bChanged
      ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
         bChanged := ::Form:bChanged
      ENDIF

      IF bChanged != NIL
         IF IsKeyDown( VK_CONTROL ) .OR. IsKeyDown( VK_MENU )
            RETURN NIL
         ENDIF
         IF ! lProc
            ::CallWindowProc()
            lProc := .T.
         ENDIF
         Eval( bChanged, Self )
      ENDIF
   ENDIF
RETURN IIF( lProc, 0, NIL )

//-----------------------------------------------------------------------------------------------
METHOD OnUndo() CLASS EditBox
   LOCAL bChanged
   IF ! ::ReadOnly
      IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
         bChanged := ::Parent:bChanged
      ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
         bChanged := ::Form:bChanged
      ENDIF
      IF bChanged != NIL
         ::CallWindowProc()
         Eval( bChanged, Self )
         RETURN 0
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnPaste() CLASS EditBox
   LOCAL bChanged
   IF ! ::ReadOnly
      IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
         bChanged := ::Parent:bChanged
      ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
         bChanged := ::Form:bChanged
      ENDIF
      IF bChanged != NIL
         ::CallWindowProc()
         Eval( bChanged, Self )
         RETURN 0
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnCut() CLASS EditBox
   LOCAL bChanged
   IF ! ::ReadOnly
      IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
         bChanged := ::Parent:bChanged
      ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
         bChanged := ::Form:bChanged
      ENDIF
      IF bChanged != NIL
         ::CallWindowProc()
         Eval( bChanged, Self )
         RETURN 0
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
CLASS Edit INHERIT EditBox
ENDCLASS


#ifdef VXH_PROFESSIONAL

CLASS FloatCalendar INHERIT Window
   DATA Calendar  EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD FloatOnSelect()
   METHOD FloatClose() INLINE ::Close()
   METHOD FloatGetDlgCode() INLINE DLGC_WANTMESSAGE
ENDCLASS

METHOD Init( oParent ) CLASS FloatCalendar
   ::__IsControl  := .F.
   ::__IsStandard := .F.

   ::Super:Init( oParent )

   ::Style   := WS_POPUP | WS_CLIPSIBLINGS
   ::ExStyle := WS_EX_NOACTIVATE | WS_EX_TOPMOST | WS_EX_TOOLWINDOW | WS_EX_CONTROLPARENT

   ::ClassStyle := CS_DROPSHADOW | CS_OWNDC | CS_DBLCLKS | CS_SAVEBITS

   ::ClsName := "FloatShadow"

   ::Calendar := MonthCalendar( Self )
   ::Calendar:ControlParent := .T.
   ::Calendar:xHeight := 0
   ::Calendar:xWidth  := 0
   ::Calendar:EventHandler[ "OnSelect" ]     := "FloatOnSelect"
   ::Calendar:EventHandler[ "OnKillFocus" ]  := "FloatClose"
   ::Calendar:EventHandler[ "OnGetDlgCode" ] := "FloatGetDlgCode"
   ::Calendar:OnWMKeyDown := {|, nKey| IIF( nKey == VK_ESCAPE, ( ::Cargo:SetFocus(), ::Close() ), ) }

RETURN Self

METHOD Create() CLASS FloatCalendar
   LOCAL pt := (struct POINT)
   Super:Create()

   pt:x := ::Cargo:Left
   pt:y := ::Cargo:Top + ::Cargo:Height
   ClientToScreen( ::Cargo:Parent:hWnd, @pt )

   ::Calendar:Date    := CTOD( xStr( ::Cargo:Text ) )
   ::Calendar:Create()
   ::Calendar:SetFocus()

   ::xWidth  := ::Calendar:xWidth
   ::xHeight := ::Calendar:xHeight

   IF ::Top + ::Height > GetSystemMetrics( SM_CYSCREEN )
      ::Top := pt:y - ::Height - ::Cargo:Height
   ENDIF

   ::MoveWindow()
   ::Show()
RETURN Self

METHOD FloatOnSelect() CLASS FloatCalendar
   LOCAL bChanged
   ::Cargo:Text := IIF( ValType( ::Cargo:Text ) == "D", ::Calendar:Date, DTOC( ::Calendar:Date ) )

   IF ::Cargo:Parent:HasMessage( "bChanged" ) .AND. ::Cargo:Parent:bChanged != NIL
      bChanged := ::Cargo:Parent:bChanged
   ELSEIF ::Cargo:Form != NIL .AND. ::Cargo:Form:HasMessage( "bChanged" ) .AND. ::Cargo:Form:bChanged != NIL
      bChanged := ::Cargo:Form:bChanged
   ENDIF
   IF bChanged != NIL
      Eval( bChanged, Self )
   ENDIF

   ExecuteEvent( "Valid", ::Cargo )

   ::Close()
   ::Cargo:SetFocus()
RETURN Self

#endif
