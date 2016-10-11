/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// ComboBox.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"
#include "commdlg.ch"
#include "uxtheme.ch"

#define CB_SETMINVISIBLE 0x1701
#define CS_DROPSHADOW 131072

#define DRIVE_UNKNOWN     0
#define DRIVE_NO_ROOT_DIR 1
#define DRIVE_REMOVABLE   2
#define DRIVE_FIXED       3
#define DRIVE_REMOTE      4
#define DRIVE_CDROM       5
#define DRIVE_RAMDISK     6

#define SHGFI_ICON              0x000000100     // get icon
#define SHGFI_DISPLAYNAME       0x000000200     // get display name
#define SHGFI_TYPENAME          0x000000400     // get type name
#define SHGFI_ATTRIBUTES        0x000000800     // get attributes
#define SHGFI_ICONLOCATION      0x000001000     // get icon location
#define SHGFI_EXETYPE           0x000002000     // return exe type
#define SHGFI_SYSICONINDEX      0x000004000     // get system icon index
#define SHGFI_LINKOVERLAY       0x000008000     // put a link overlay on icon
#define SHGFI_SELECTED          0x000010000     // show icon in selected state
#define SHGFI_ATTR_SPECIFIED    0x000020000     // get only specified attributes
#define SHGFI_LARGEICON         0x000000000     // get large icon
#define SHGFI_SMALLICON         0x000000001     // get small icon
#define SHGFI_OPENICON          0x000000002     // get open icon
#define SHGFI_SHELLICONSIZE     0x000000004     // get shell size icon
#define SHGFI_PIDL              0x000000008     // pszPath is a pidl
#define SHGFI_USEFILEATTRIBUTES 0x000000010     // use passed dwFileAttribute

#define FILE_ATTRIBUTE_DIRECTORY            0x00000010

#define CB_GETCOMBOBOXINFO      0x0164

//--------------------------------------------------------------------------------------------------------------

CLASS ComboBox FROM Control

   DATA ImageIndex PROTECTED
   DATA AllowUnDock          EXPORTED INIT FALSE
   DATA AllowClose           EXPORTED INIT FALSE

   ACCESS CurSel             INLINE ::GetCurSel()
   ASSIGN Cursel(n)          INLINE ::SetCurSel(n)

   // temporary ONLY for compatibility
   ACCESS AutoHorzScroll     INLINE ::AutoEditHorzScroll
   ASSIGN AutoHorzScroll(l)  INLINE ::AutoEditHorzScroll := l

   //-----------------------------------------------------------

   PROPERTY SelectionHeight     ROOT "Appearance" SET ::SetItemHeight( -1, v )               DEFAULT 15
   PROPERTY Sort                ROOT "Appearance" SET ::SetStyle( CBS_SORT, v )              DEFAULT .F.
   PROPERTY UpperCase           ROOT "Appearance" SET ::SetStyle( CBS_UPPERCASE, v )         DEFAULT .F.
   PROPERTY LowerCase           ROOT "Appearance" SET ::SetStyle( CBS_LOWERCASE, v )         DEFAULT .F.
   PROPERTY HasStrings          ROOT "Appearance" SET ::SetStyle( CBS_HASSTRINGS, v )        DEFAULT .F.
   PROPERTY OemConvert          ROOT "Appearance" SET ::SetStyle( CBS_OEMCONVERT, v )        DEFAULT .F.
   PROPERTY ItemHeight          ROOT "Appearance" SET ::SetItemHeight( 1, v )
   PROPERTY ItemToolTips        ROOT "Appearance" SET ::__SetItemToolTips(v)                 DEFAULT .F.
   PROPERTY Flat                ROOT "Appearance"                                            DEFAULT .F.

   PROPERTY FitToolBar          ROOT "Behavior"                                              DEFAULT .T.
   PROPERTY VertScroll          ROOT "Behavior"   SET ::SetStyle( WS_VSCROLL, v )            DEFAULT .F.
   PROPERTY DisableNoScroll     ROOT "Behavior"   SET ::SetStyle( CBS_DISABLENOSCROLL, v )   DEFAULT .T.
   PROPERTY NoIntegralHeight    ROOT "Behavior"   SET ::SetStyle( CBS_NOINTEGRALHEIGHT, v )  DEFAULT .F.
   PROPERTY DropDownStyle       ROOT "Behavior"   SET ::SetDropDownStyle( v )                DEFAULT __GetSystem():DropDownStyle:DropDownList
   PROPERTY HorzScroll          ROOT "Behavior"   SET ::SetStyle( WS_HSCROLL, v )            DEFAULT .F.
   PROPERTY OwnerDrawFixed      ROOT "Behavior"   SET ::SetStyle( CBS_OWNERDRAWFIXED, v )    DEFAULT .F.
   PROPERTY OwnerDrawVariable   ROOT "Behavior"   SET ::SetStyle( CBS_OWNERDRAWVARIABLE, v ) DEFAULT .F.
   PROPERTY AutoEditHorzScroll  ROOT "Behavior"   SET ::SetStyle( CBS_AUTOHSCROLL, v )       DEFAULT .F.

   DATA OnCBNSelEndOk     EXPORTED
   DATA OnCBNSelEndCancel EXPORTED
   DATA OnCBNKillFocus    EXPORTED
   DATA cbi               EXPORTED
   DATA __pListCallBack   EXPORTED
   DATA __pTipCallBack    EXPORTED
   DATA __nListProc       EXPORTED
   DATA __nTipProc        EXPORTED
   DATA __OriginalSel     EXPORTED INIT LB_ERR
   DATA hEdit             EXPORTED

   DATA __nWidth        PROTECTED INIT 0
   DATA __tipWnd        PROTECTED
   DATA __isEnter       PROTECTED INIT .F.
   DATA __pCallBackEdit PROTECTED
   DATA __nProcEdit     PROTECTED

   METHOD Init()  CONSTRUCTOR

   METHOD GetString()
   //METHOD OnGetDlgCode() INLINE DLGC_WANTMESSAGE | DLGC_WANTALLKEYS

   METHOD AddString()
   METHOD AddItem( cText )                  INLINE ::AddString( cText )
   METHOD InsertString(nLine,cText )        INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_INSERTSTRING, nLine-1, cText ), NIL )
   METHOD DeleteString(nLine)               INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_DELETESTRING, nLine-1, 0), NIL )
   METHOD SetCurSel(nLine)                  INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SETCURSEL, nLine-1, 0), NIL )
   METHOD FindString(nStart,cStr)           INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_FINDSTRING, IFNIL(nStart,-1,nStart-1), cStr)+1, NIL )
   METHOD FindStringExact()
   METHOD GetCount()                        INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETCOUNT, 0, 0), NIL )
   METHOD GetCurSel()                       INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETCURSEL, 0, 0)+1, NIL )
   METHOD Dir(nAttr, cFileSpec)             INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_DIR, nAttr, cFileSpec), NIL )
   METHOD GetDroppedWidth()                 INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETDROPPEDWIDTH, 0, 0), NIL )
   METHOD GetEditText()
   METHOD ResetContent()                    INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_RESETCONTENT, 0, 0 ) , NIL )
   METHOD GetEditSel(nStart,nEnd)           INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETEDITSEL, nStart,nEnd ), NIL )
   METHOD GetLBText( nLine, cBuffer )       INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETLBTEXT,nLine-1, @cBuffer ), NIL )
   METHOD GetLBTextLen( nLine )             INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETLBTEXTLEN, nLine-1, 0 ), NIL )
   METHOD SelectString( nLine, cText )      INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SELECTSTRING, nLine-1, cText ), NIL )
   METHOD GetTopIndex( nLine )              INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETTOPINDEX, nLine-1, 0 ) , NIL )
   METHOD GetHorizontalExtent()             INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETHORIZONTALEXTENT, 0, 0 ) , NIL )
   METHOD SetHorizontalExtent(nWidth)       INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SETHORIZONTALEXTENT, nWidth, 0 ) , NIL )
   METHOD SetTopIndex( nLine )              INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SETTOPINDEX, nLine-1, 0 ), NIL )
   METHOD GetItemData( nLine )              INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETITEMDATA, nLine-1, 0 ) , NIL )
   METHOD SetItemData( nLine, cData )       INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SETITEMDATA, nLine-1, cData ) , NIL )

   METHOD GetTopIndex()                     INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETTOPINDEX, 0, 0 ) , NIL )

   METHOD GetItemHeight( nLine )            INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETITEMHEIGHT, nLine-1, 0 ) , ::xSelectionHeight )

   METHOD SetLocale( nID )                  INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SETLOCALE, nID, 0 ) , NIL )
   METHOD GetLocale()                       INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETLOCALE, 0, 0 ), NIL )
   METHOD InitStorage( nItems, nBytes )     INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_INITSTORAGE, nItems, nBytes ) , NIL )
   METHOD GetDroppedState()                 INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETDROPPEDSTATE, 0, 0 ) , NIL )
   METHOD ShowDropDown(l)                   INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SHOWDROPDOWN, IIF( l==NIL, .T., l), 0 ) , NIL )
   METHOD SetExtendedUi(l)                  INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SETEXTENDEDUI, IIF( l==NIL, .T., l), 0 ) , NIL )
   METHOD HideDropDown()                    INLINE ::ShowDropDown(.F.)
   METHOD GetComboBoxInfo()
   METHOD GetSelString()
   METHOD DrawFrame()
   METHOD OnParentCommand()
   METHOD Create()
   METHOD SetItemHeight()
   METHOD __SetScrollBars()                INLINE Self
   METHOD SetDropDownStyle()
   METHOD OnDestroy()                      INLINE Super:OnDestroy(), ::__SetItemToolTips(.F.), ::__ResetEdit()
   METHOD OnWindowPosChanged()             INLINE ::CallWindowProc(), ::SetItemHeight( -1, ::xSelectionHeight ), ::SetItemHeight( 2, ::xItemHeight ), 0
   METHOD OnKillFocus()                    INLINE IIF( ::DropDownStyle <> CBS_DROPDOWNLIST, 0, NIL )
   METHOD __ListCallBack()
   METHOD __TipCallBack()
   METHOD __ListboxMouseMove()
   METHOD __TrackMouseEvent()
   METHOD __HandleOnPaint()
   METHOD __HandleOnTimer()
   METHOD __SetItemToolTips()
   METHOD __ComboBoxEditProc()
   METHOD __ResetEdit()
   METHOD __SetSizePos()
   METHOD OnParentDrawItem()
ENDCLASS

//--------------------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ComboBox
   DEFAULT ::__xCtrlName TO "ComboBox"
   ::ClsName    := "ComboBox"
   ::ThemeName  := "combobox"
   ::Style      := ( WS_CHILD | WS_VISIBLE | WS_TABSTOP | CBS_HASSTRINGS | CBS_DROPDOWNLIST | CBS_DISABLENOSCROLL  | WS_CLIPCHILDREN | WS_CLIPSIBLINGS )
   ::Super:Init( oParent )
   ::Width      := 100
   ::Height     := 100
   IF !EMPTY( ::Events )
      AADD( ::Events[3][2], { "OnCBNSelEndOk" , "", "" } )
      AADD( ::Events[3][2], { "OnSelChange" , "", "" } )
   ENDIF
   ::bSetValue := {|nValue| ::SetCurSel( nValue )}
   ::bGetValue := {||::GetCurSel()}
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD __SetSizePos( nPos, nVal ) CLASS ComboBox
   IF nPos == 4 .AND. ::hWnd != NIL .AND. ! ::DesignMode
      ::SendMessage( CB_SETMINVISIBLE, nVal/::xItemHeight )
   ENDIF
RETURN Super:__SetSizePos( nPos, nVal )

//----------------------------------------------------------------------------------------------------------------
METHOD GetEditText() CLASS ComboBox
   LOCAL cText, hWnd := FindWindowEx( ::hwnd, 0, NIL, NIL )
   IF IsWindow( hWnd )
      cText := _GetWindowText( hWnd )
   ENDIF
RETURN cText

//----------------------------------------------------------------------------------------------------------------
METHOD SetDropDownStyle( nDrop ) CLASS ComboBox
   ::Style := ( ::Style & NOT( CBS_DROPDOWNLIST ) )
   ::Style := ( ::Style & NOT( CBS_DROPDOWN ) )
   ::Style := ( ::Style & NOT( CBS_SIMPLE ) )
   ::Style := ( ::Style | nDrop )
   IF ::IsWindow()
      ::SetWindowLong( GWL_STYLE, ::Style )
      IF ::DesignMode
         ::SetWindowPos(, 0, 0, 0, 0, (SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER) )
         ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS ComboBox
   LOCAL cbi
   ::Super:Create()

   DEFAULT ::xItemHeight TO ::SendMessage( CB_GETITEMHEIGHT, 0, 0 )
   IF ::DropDownStyle <> CBS_SIMPLE
      ::SetItemHeight( -1, ::xSelectionHeight )
      ::SetItemHeight( 2, ::xItemHeight )
      ::SendMessage( CB_SETMINVISIBLE, ::xHeight/::xItemHeight )
   ENDIF

   IF ::ItemToolTips
      ::__SetItemToolTips( .T. )
   ENDIF

   cbi := ::GetComboBoxInfo()
   ::hEdit := cbi:hwndItem
   IF IsWindow( ::hEdit )
      ::__pCallBackEdit := WinCallBackPointer( HB_ObjMsgPtr( Self, "__ComboBoxEditProc" ), Self )
      ::__nProcEdit    := SetWindowLong( ::hEdit, GWL_WNDPROC, ::__pCallBackEdit )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD __ResetEdit() CLASS ComboBox
   IF IsWindow( ::hEdit )
      SetWindowLong( ::hEdit, GWL_WNDPROC, ::__nProcEdit )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD __ComboBoxEditProc( hWnd, nMsg, nwParam, nlParam ) CLASS ComboBox
   DO CASE
      CASE nMsg == WM_KEYUP
         IF nwParam == VK_RETURN
            __Evaluate( ::Action, Self, _GetWindowText(::hEdit),,,)
         ENDIF
   ENDCASE
RETURN CallWindowProc( ::__nProcEdit, hWnd, nMsg, nwParam, nlParam )

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS ComboBox
   LOCAL lSelected, nLen, itemTxt
   ( nwParam, nlParam )
   IF dis != NIL .AND. dis:hwndItem == ::hWnd
      lSelected := (dis:itemState & ODS_SELECTED) != 0
      IF (dis:itemAction & ODA_DRAWENTIRE) != 0 .OR. (dis:itemAction & ODA_SELECT) != 0
         SetTextColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )

         itemTxt := Space( nLen + 1 )
         SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )

         itemTxt := Left( itemTxt, nLen )

         //DrawText( dis:hDC, itemTxt, dis:rcItem, (DT_VCENTER | DT_SINGLELINE) )

         ExtTextOut( dis:hDC, 5, dis:rcItem:Top, ETO_OPAQUE + ETO_CLIPPED, dis:rcItem, itemTxt )
      ENDIF
      IF (dis:itemState & ODS_FOCUS) != 0 .OR. (dis:itemAction & ODA_FOCUS) != 0
         DrawfocusRect( dis:hDC, dis:rcItem )
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD __SetItemToolTips( lTips ) CLASS ComboBox
   LOCAL wcex
   IF lTips

      IF IsWindow( ::hWnd )
         ::cbi := ::GetComboBoxInfo()
         IF IsWindow( ::cbi:hwndList )
            ::__pListCallBack := WinCallBackPointer( HB_ObjMsgPtr( Self, "__ListCallBack" ), Self )
            ::__nListProc := SetWindowLong( ::cbi:hwndList, GWL_WNDPROC, ::__pListCallBack )
         ENDIF
         wcex := (struct WNDCLASSEX)
         wcex:cbSize         := wcex:SizeOf()
         wcex:style          := (CS_OWNDC | CS_DBLCLKS | CS_SAVEBITS | CS_DROPSHADOW)
         wcex:hInstance      := ::AppInstance
         wcex:hbrBackground  := COLOR_BTNFACE+1
         wcex:lpszClassName  := "CBTT"
         wcex:hCursor        := LoadCursor(, IDC_ARROW )
         wcex:lpfnWndProc    := DefWindowProcAddress()
         RegisterClassEx( wcex )

         ::__tipWnd := CreateWindowEx( WS_EX_TOOLWINDOW, "CBTT", "", WS_POPUP, 0, 0, 0, 0, 0, 0, ::AppInstance )

         IF IsWindow( ::__tipWnd )
            ::__pTipCallBack := WinCallBackPointer( HB_ObjMsgPtr( Self, "__TipCallBack" ), Self )
            ::__nTipProc := SetWindowLong( ::__tipWnd, GWL_WNDPROC, ::__pTipCallBack )
         ENDIF
      ENDIF

    ELSE

      IF IsWindow( ::__tipWnd ) .AND. ::__nTipProc != NIL
         SetWindowLong( ::__tipWnd, GWL_WNDPROC, ::__nTipProc )
         ::__nTipProc := NIL
         ::Parent:PostMessage( WM_VXH_FREECALLBACK, ::__pTipCallBack )

         DestroyWindow( ::__tipWnd )

         IF IsWindow( ::cbi:hwndList ) .AND. ::__nListProc != NIL
            SetWindowLong( ::cbi:hwndList, GWL_WNDPROC, ::__nListProc )
            ::__nListProc := NIL
            ::Parent:PostMessage( WM_VXH_FREECALLBACK, ::__pListCallBack )
         ENDIF
      ENDIF

   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD SetItemHeight(nLine, n) CLASS ComboBox
   IF nLine == -1
      ::xSelectionHeight := n
   ENDIF
   IF ::hWnd != NIL
      ::SendMessage( CB_SETITEMHEIGHT, IIF( nLine != -1, nLine-1, -1), n )
      IF nLine == 1
         ::SendMessage( CB_SETMINVISIBLE, ::xHeight/n )
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD FindStringExact( nStart, cStr ) CLASS ComboBox
   LOCAL nItem
   IF ::hWnd != NIL
      DEFAULT nStart TO -1
      nItem := ::SendMessage( CB_FINDSTRINGEXACT, nStart, cStr ) + 1
   ENDIF
RETURN nItem

//----------------------------------------------------------------------------------------------------------------
METHOD AddString( cText, lSel ) CLASS ComboBox
   LOCAL n
   IF ::hWnd != NIL
      ::SendMessage( CB_ADDSTRING, 0, cText )
      IF ::HorzScroll
         n := ::Drawing:GetTextExtentPoint32( cText )[1]
         IF n > ::__nWidth
            ::SetHorizontalExtent( n + 3 )
            ::__nWidth := n
         ENDIF
      ENDIF
      DEFAULT lSel TO .F.
      IF lSel
         ::SetCurSel( ::GetCount() )
      ENDIF
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------------------
METHOD GetSelString() CLASS ComboBox
   LOCAL cStr, n := ::GetCurSel()
   cStr := ::GetString( n )
RETURN cStr

//----------------------------------------------------------------------------------------------------------------
METHOD GetComboBoxInfo() CLASS ComboBox
   LOCAL cbi := (struct COMBOBOXINFO)
   cbi:cbSize := cbi:SizeOf()
   SendMessage( ::hWnd, CB_GETCOMBOBOXINFO, 0, @cbi )
RETURN cbi

//----------------------------------------------------------------------------------------------------------------
METHOD DrawFrame( hDC, aRect, nAlign, nWidth, nStatus ) CLASS ComboBox
   LOCAL hTheme, nFlags := DFCS_SCROLLCOMBOBOX
   (nAlign)
   (nWidth)
   IF nStatus != NIL
      IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
         nStatus := CBXS_NORMAL
      ENDIF
      nFlags := (nFlags | nStatus)
   ENDIF
   aRect[1] := aRect[3] - GetSystemMetrics( SM_CXVSCROLL )
   IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
      hTheme := OpenThemeData(,"combobox")
      aRect[4]-=2
      DrawThemeBackground( hTheme, hDC, CP_DROPDOWNBUTTON, nStatus, aRect, aRect )
      CloseThemeData( hTheme )
    ELSE
      DrawFrameControl( hDC, aRect, DFC_SCROLL, nFlags )
   ENDIF
RETURN GetSystemMetrics( SM_CXVSCROLL )

//----------------------------------------------------------------------------------------------------------------
METHOD GetString(nLine) CLASS ComboBox
   LOCAL nLen
   LOCAL cBuf
   DEFAULT nLine TO ::CurSel
   cBuf := Space( SendMessage(::hWnd, CB_GETLBTEXTLEN, nLine-1, 0 ) + 1 )
   nLen := SendMessage(::hWnd, CB_GETLBTEXT, nLine-1, @cBuf )
RETURN( if(nLen == CB_ERR, nil, left(cBuf, nLen) ) )

//----------------------------------------------------------------------------------------------------------------
METHOD OnParentCommand( nId, nCode ) CLASS ComboBox
   LOCAL bChanged, nRet := NIL
   (nId)
   DO CASE
      CASE nCode == CBN_SELENDOK
           nRet := __Evaluate( ::OnCBNSelEndOk, Self,,,,)
           nRet := __Evaluate( ::Action, Self,,,,)
           nRet := ExecuteEvent( "OnCBNSelEndOk", Self )
           nRet := 0
           IF ::Parent != NIL
              IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
                 bChanged := ::Parent:bChanged
              ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
                 bChanged := ::Form:bChanged
              ENDIF
              IF bChanged != NIL
                 Eval( bChanged, Self )
              ENDIF
           ENDIF

      CASE nCode == CBN_SELENDCANCEL
           __Evaluate( ::OnCBNSelEndCancel, Self,,,,)

      CASE nCode == CBN_SELCHANGE
           nRet := ExecuteEvent( "OnSelChange", Self )

      CASE nCode == CBN_DBLCLK
      CASE nCode == CBN_ERRSPACE
      CASE nCode == CBN_KILLFOCUS
           __Evaluate( ::OnCBNKillFocus, Self,,,,)
           IF ::DropDownStyle <> CBS_DROPDOWNLIST
              nRet := ExecuteEvent( "OnKillFocus", Self )
           ENDIF

      CASE nCode == CBN_SETFOCUS

   ENDCASE
RETURN nRet

//----------------------------------------------------------------------------------------------------------------
METHOD __ListCallBack( hWnd, nMsg, nwParam, nlParam ) CLASS ComboBox
   LOCAL aPt, aRect
   SWITCH nMsg
      CASE WM_MOUSEMOVE
           aPt := { LOWORD( nlParam ), HIWORD( nlParam ) }
           aRect := _GetClientRect( hWnd )
           IF _PtInRect( aRect, aPt )
              ::__ListboxMouseMove( hWnd, nwParam, aPt )
            ELSE
              SendMessage( hWnd, WM_MOUSELEAVE, nwParam, nlParam )
           ENDIF

           IF ! ::__isEnter
              ::__TrackMouseEvent( hWnd, (TME_HOVER|TME_LEAVE) )
           ENDIF
           EXIT

      CASE WM_MOUSELEAVE
           ::__OriginalSel := LB_ERR
           ::__isEnter := .F.
           ShowWindow( ::__tipWnd, SW_HIDE )
           EXIT

//      CASE WM_CAPTURECHANGED
//           RETURN 1

//      CASE WM_LBUTTONDOWN
//      CASE WM_LBUTTONDBLCLK
//            aPt := { LOWORD(nlParam), HIWORD( nlParam ) }
//            aRect := _GetClientRect( hWnd )
//            IF _PtInRect( aRect, aPt )
//               nCurSel := SendMessage( hWnd, LB_ITEMFROMPOINT, 0, MAKELONG( aPt[1], aPt[2] ) )
//               IF nCurSel != LB_ERR
//                  RETURN 1
//               ENDIF
//            ENDIF
//            EXIT

   END
RETURN CallWindowProc( ::__nListProc, hWnd, nMsg, nwParam, nlParam )

//----------------------------------------------------------------------------------------------------------------
METHOD __TrackMouseEvent( hWnd, nFlags ) CLASS ComboBox
   LOCAL tme
   tme := (struct TRACKMOUSEEVENT)
   tme:cbSize      := tme:SizeOf()
   tme:dwFlags     := nFlags
   tme:hwndTrack   := hWnd
   tme:dwHoverTime := HOVER_DEFAULT
   TrackMouseEvent( tme )
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD __TipCallBack( hWnd, nMsg, nwParam, nlParam ) CLASS ComboBox
   SWITCH nMsg
      CASE WM_PAINT
           RETURN ::__HandleOnPaint( hWnd )

      CASE WM_TIMER
           ::__HandleOnTimer( nwParam )
           EXIT
      //CASE WM_SHOWWINDOW
      //     IF nwParam == 0
      //        ReleaseCapture()
      //     ENDIF
   END
RETURN CallWindowProc( ::__nTipProc, hWnd, nMsg, nwParam, nlParam )

//----------------------------------------------------------------------------------------------------------------
METHOD __HandleOnPaint( hWnd ) CLASS ComboBox
   LOCAL hDC, cPaint, aRect, cText, hOldFont, hTheme
   hDC := _BeginPaint( hWnd, @cPaint )
   aRect := _GetClientRect( hWnd )

   hTheme := OpenThemeData(,"TOOLTIP")
   DrawThemeBackground( hTheme, hDC, TTP_STANDARD, 0, { 0, 0, aRect[3], aRect[4] } )
   CloseThemeData( hTheme )

//   SelectObject( hDC, GetSysColorbrush( COLOR_INFOBK ) )
//   Rectangle( hDC, 0, 0, aRect[3], aRect[4] )

   cText := _GetWindowText( hWnd )
   SetBkMode( hDC, TRANSPARENT )
   hOldFont := SelectObject( hDC, ::Font:Handle )

   _DrawText( hDC, cText, aRect, (DT_SINGLELINE|DT_CENTER|DT_VCENTER|DT_NOPREFIX) )

   SelectObject( hDC, hOldFont )
   _EndPaint( hWnd, cPaint)
RETURN 0

//----------------------------------------------------------------------------------------------------------------
METHOD __HandleOnTimer( nwParam ) CLASS ComboBox
   KillTimer( ::__tipWnd, nwParam )
   ShowWindow( ::__tipWnd, SW_HIDE)
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD __ListboxMouseMove( hList, nwParam, aPt ) CLASS ComboBox
   LOCAL hDC, hOldFont, cBuf, pt := (struct POINT)
   LOCAL rcBounds := (struct RECT)
   LOCAL rcDraw := (struct RECT)
   LOCAL cRect := space(16)
   LOCAL nCurSel := SendMessage( hList, LB_ITEMFROMPOINT, 0, MAKELONG( aPt[1], aPt[2] ) )
   (nwParam)
   IF ::__OriginalSel == nCurSel
      RETURN NIL
   ENDIF

   IF nCurSel == LB_ERR .OR. nCurSel < 0 .OR. nCurSel >= SendMessage( hList, LB_GETCOUNT, 0, 0 )
      RETURN NIL
   ENDIF

   ::__OriginalSel := nCurSel

   hDC := GetDC( ::__tipWnd )
   hOldFont := SelectObject( hDC, ::Font:Handle )

   cBuf := space( SendMessage( hList, LB_GETTEXTLEN, nCurSel, 0 ) + 1 )
   SendMessage( hList, LB_GETTEXT, nCurSel, @cBuf)

   SendMessage( hList, LB_GETITEMRECT, nCurSel, @rcBounds)
   rcDraw:left   := rcBounds:left
   rcDraw:top    := rcBounds:top
   rcDraw:right  := rcBounds:right
   rcDraw:bottom := rcBounds:bottom

   DrawText( hDC, cBuf, @rcDraw, (DT_CALCRECT|DT_SINGLELINE|DT_CENTER|DT_VCENTER|DT_NOPREFIX) )

   SelectObject( hDC, hOldFont )
   ReleaseDC( ::__tipWnd, hDC )

   IF rcDraw:right <= rcBounds:right
      ShowWindow( ::__tipWnd, SW_HIDE )
      RETURN NIL
   ENDIF

   InflateRect( @rcDraw, 2, 2 )

   pt:x := rcDraw:left
   pt:y := rcDraw:top
   ClientToScreen( hList, @pt )
   rcDraw:left := pt:x
   rcDraw:top  := pt:y

   pt:x := rcDraw:right
   pt:y := rcDraw:bottom
   ClientToScreen( hList, @pt )
   rcDraw:right  := pt:x
   rcDraw:bottom := pt:y

   SetWindowText( ::__tipWnd, cBuf )

   ShowWindow( ::__tipWnd, SW_HIDE )

   //IF GetCapture() != hList
   //   SetCapture( hList )
   //ENDIF

   SetWindowPos( ::__tipWnd, HWND_TOPMOST, rcDraw:left+1, rcDraw:top, rcDraw:Right-rcDraw:left+4, rcDraw:Bottom-rcDraw:top, (SWP_NOACTIVATE | SWP_SHOWWINDOW) )
   SetTimer( ::__tipWnd, 1, 9000, NIL )
RETURN NIL



//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------


CLASS DriveCombobox INHERIT ComboBox
   DATA Drives EXPORTED INIT {}
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnParentDrawItem()
   METHOD OnDestroy()   INLINE AEVAL( ::Drives, {|a|DestroyIcon( a[3] )} ), Super:OnDestroy()
ENDCLASS

METHOD Init( oParent ) CLASS DriveCombobox
   LOCAL cDrives, n, cType, shfi
   ::__xCtrlName := "DriveComboBox"
   ::Super:Init( oParent )
   ::Style     := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   IF !::Application:IsThemedXP
      ::Border := 0
   ENDIF
   shfi := (struct SHFILEINFO)
   ::HorzScroll := .T.
   ::VertScroll := .T.
   cDrives := SPACE( 256 )
   GetLogicalDriveStrings( 256, @cDrives )
   STRTRAN( cDrives, CHR(0) )
   FOR n := 1 TO LEN( ALLTRIM(cDrives ))-1
//       cBuffer := shfi:Value()
       SHGetFileInfo( SUBSTR( cDrives, n, 3 ),, @shfi, (SHGFI_ICON | SHGFI_SMALLICON | SHGFI_DISPLAYNAME) )
//       shfi:Buffer( cBuffer )
       cType := shfi:szDisplayName:AsString()//:Value()
       AADD( ::Drives, { SUBSTR( cDrives, n, 2 ), cType, shfi:hIcon } )
       n += 3
   NEXT
   IF ::DesignMode
      ::__PropFilter := { "ALLOWMAXIMIZE" }
   ENDIF
RETURN Self

METHOD Create() CLASS DriveCombobox
   LOCAL n
   ::Super:Create()
   ::SendMessage( CB_SETITEMHEIGHT, -1, ::xSelectionHeight )
   ::SendMessage( CB_SETITEMHEIGHT, 0, 18 )

   FOR n := 1 TO LEN( ::Drives )
       ::AddItem( ::Drives[n][1] + CHR(9) + ::Drives[n][2] )
   NEXT
RETURN Self

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS DriveCombobox
   LOCAL n, x, lSelected, aClip, nLen, itemTxt, cText, aRect, nField, y
   ( nwParam, nlParam )
   IF dis != NIL .AND. dis:hwndItem == ::hWnd
      lSelected := (dis:itemState & ODS_SELECTED) != 0
      aClip     := { dis:rcItem:Left+20,  dis:rcItem:Top, ;
                     dis:rcItem:Right, dis:rcItem:Bottom  }
      IF (dis:itemAction & ODA_DRAWENTIRE) != 0 .OR. (dis:itemAction & ODA_SELECT) != 0
         SetTextColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )

         itemTxt := Left( itemTxt, nLen )
         cText   := ""
         aRect   := ACLONE(aClip)
         nField  := 1

         _ExtTextOut( dis:hDC, 0, dis:rcItem:Top, ETO_OPAQUE + ETO_CLIPPED, { dis:rcItem:Left, dis:rcItem:Top, dis:rcItem:Right, dis:rcItem:Bottom }, " " )
         FOR n := 3 to nLen + 1
             IF SubStr( itemTxt, n, 1) == chr(9) .or. n == nLen + 1
                x := aRect[1] + 2
                _DrawText( dis:hDC, cText, {x, aRect[2], aRect[3], aRect[4] }, (DT_VCENTER | DT_SINGLELINE) )
                cText := ""
                aRect[1] += 4
                nField ++

                LOOP
             ENDIF

             cText += SubStr( itemTxt, n, 1 )
         NEXT

         n := ASCAN( ::Drives, {|a|a[1] == LEFT( itemTxt, 2 ) } )
         IF n > 0
            y := dis:rcItem:Top + ((dis:rcItem:Bottom-dis:rcItem:Top)/2) - (16/2)
            DrawIconEx( dis:hDC, 3, y, ::Drives[ n ][3], 16, 16, 0, NIL,  DI_NORMAL )
         ENDIF

      ENDIF
      IF (dis:itemState & ODS_FOCUS) != 0 .OR. (dis:itemAction & ODA_FOCUS) != 0
         aClip := { dis:rcItem:Left,  dis:rcItem:Top, ;
                    dis:rcItem:Right, dis:rcItem:Bottom  }
         _DrawfocusRect( dis:hDC, aclip )
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------


CLASS ColorPicker INHERIT ComboBox
   PROPERTY AllowCustomColor   DEFAULT .F.
   PROPERTY AllowSystemDefault DEFAULT .F.

   DATA ColorSelected   EXPORTED
   DATA Colors          EXPORTED INIT {}
   DATA SysDefault      EXPORTED
   DATA Custom          EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnParentDrawItem()
//   METHOD OnPaint()
   METHOD OnParentCommand()
   METHOD SelectColor()
ENDCLASS

METHOD Init( oParent ) CLASS ColorPicker
   ::__xCtrlName := "ColorPicker"
   ::Super:Init( oParent )
   ::Style := (WS_CHILD | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
RETURN Self

METHOD SelectColor( nColor ) CLASS ColorPicker
   LOCAL n
   ::ColorSelected := nColor
   IF ( n := ASCAN( ::Colors, {|a| a[1] == nColor } ) ) > 0
      ::SetCurSel(n)
    ELSEIF ::AllowCustomColor
      ::Custom := nColor
      ::Colors[ LEN( ::Colors ) ][1] := nColor
      ::SetCurSel(LEN( ::Colors ))
   ENDIF
RETURN n

METHOD Create( lBlank ) CLASS ColorPicker
   LOCAL n, cColor
   DEFAULT lBlank TO .F.
   ::Colors := {}
   FOR EACH cColor IN ::System:Color:Keys
       AADD( ::Colors, { ::System:Color[ cColor ], cColor } )
   NEXT
   IF ::AllowSystemDefault
      AADD( ::Colors, { ::SysDefault, "System Default..." } )
   ENDIF
   IF ::AllowCustomColor
      AADD( ::Colors, { ::Custom, "Custom..." } )
   ENDIF
   IF lBlank
      AINS( ::Colors, 1, { NIL, "None" }, .T. )
   ENDIF
   ::Super:Create()
   FOR n := 1 TO LEN( ::Colors )
       ::AddItem( ::Colors[n][2] )
   NEXT
   ::Show()
RETURN Self

METHOD OnParentCommand( nId, nCode ) CLASS ColorPicker
   LOCAL cSel, nColor, nRet
   (nId)
   DO CASE
      CASE nCode == CBN_SELENDOK
           cSel := ::Colors[ ::GetCurSel() ][2]
           DO CASE
              CASE cSel == "System Default..."
                   ::ColorSelected := NIL

              CASE cSel == "Custom..."
                   nColor := ::ColorSelected //::Colors[ ::GetCurSel()+1 ][1]
                   IF _ChooseColor( ::Parent:hWnd, @nColor, ::Application:CustomColors, (CC_ANYCOLOR | CC_RGBINIT | CC_SOLIDCOLOR | CC_FULLOPEN ) )
                      TRY
                        ::Colors[ ::GetCurSel() ][1] := nColor
                      CATCH
                      END
                   ENDIF
                   ::ColorSelected := nColor

              OTHERWISE
                  ::ColorSelected := ::Colors[ ::GetCurSel() ][1]

           ENDCASE
           __Evaluate( ::OnCBNSelEndOk, Self,,,,)
           __Evaluate( ::Action, Self,,,,)
           nRet := ExecuteEvent( "OnCBNSelEndOk", Self )

      CASE nCode == CBN_SELENDCANCEL
           __Evaluate( ::OnCBNSelEndCancel, Self,,,,)

      CASE nCode == CBN_SELCHANGE

      CASE nCode == CBN_DBLCLK

      CASE nCode == CBN_ERRSPACE

      CASE nCode == CBN_KILLFOCUS
           __Evaluate( ::OnCBNKillFocus, Self,,,,)

      CASE nCode == CBN_SETFOCUS

   ENDCASE
RETURN nRet

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS ColorPicker
   LOCAL n, lSelected, aClip, nLen, itemTxt, hBrush, hOld, z
   (nwParam, nlParam)
   IF dis != NIL .AND. dis:hwndItem == ::hWnd
      lSelected := (dis:itemState & ODS_SELECTED) != 0
      aClip     := { dis:rcItem:Left+20,  dis:rcItem:Top, ;
                     dis:rcItem:Right, dis:rcItem:Bottom  }

      IF (dis:itemAction & ODA_DRAWENTIRE) != 0 .OR. (dis:itemAction & ODA_SELECT) != 0 .OR. (dis:itemAction & ODA_FOCUS) != 0
         SetTextColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )
         itemTxt := left( itemTxt, nLen )

         z := 0
         IF (dis:itemState & ODS_COMBOBOXEDIT) != 0
            z := 2
         ENDIF
         //_ExtTextOut( dis:hDC, IIF( itemTxt != "None", 28, 3 ), dis:rcItem:Top-z, ETO_OPAQUE + ETO_CLIPPED, { dis:rcItem:Left, dis:rcItem:Top, dis:rcItem:Right, dis:rcItem:Bottom }, itemTxt )

         FillRect( dis:hDC, dis:rcItem, GetSysColorBrush( IIF( lselected, COLOR_HIGHLIGHT, COLOR_WINDOW )) )
         IF itemTxt != "None"
            dis:rcItem:Left += 28
          ELSE
            dis:rcItem:Left += 3
         ENDIF
         DrawText( dis:hDC, itemTxt, dis:rcItem, (DT_LEFT | DT_VCENTER | DT_SINGLELINE ) )
         IF itemTxt != "None"
            dis:rcItem:Left -= 28
          ELSE
            dis:rcItem:Left -= 3
         ENDIF

         n := dis:itemID +1 //ASCAN( ::Colors, {|a|a[2] == itemTxt} )

         IF n > 0
            z := 2
            IF (dis:itemState & ODS_COMBOBOXEDIT) != 0
               z := 0
            ENDIF
            IF itemTxt != "None"
               hBrush := CreateSolidBrush( ::Colors[n][1] )
               hOld := SelectObject( dis:hDC, hBrush )
               Rectangle( dis:hDC, dis:rcItem:Left+z, dis:rcItem:Top+z, dis:rcItem:Left+22+z, dis:rcItem:Bottom-z )
               SelectObject( dis:hDC, hOld )
               DeleteObject( hBrush )
            ENDIF
         ENDIF
      ENDIF

      //IF dis:itemState & ODS_COMBOBOXEDIT == 0
      //   IF dis:itemState & ODS_FOCUS != 0 .OR. dis:itemAction & ODA_FOCUS != 0
      //      aClip := { dis:rcItem:Left,  dis:rcItem:Top, ;
      //                 dis:rcItem:Right, dis:rcItem:Bottom  }
      //      _DrawfocusRect( dis:hDC, aclip )
      //   ENDIF
      //ENDIF
   ENDIF
RETURN 0


//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------


CLASS CursorComboBox INHERIT ComboBox
   DATA Cursors EXPORTED INIT {}
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnParentDrawItem()
ENDCLASS

METHOD Init( oParent ) CLASS CursorComboBox
   ::__xCtrlName := "CursorComboBox"
   ::Super:Init( oParent )
   ::Style     := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS )
RETURN Self

METHOD Create() CLASS CursorComboBox
   LOCAL n, hCursor, aSize, aCursors := ::System:GetEnumCursor()
   ::Super:Create()
   ::Cursors := {}
   FOR n := 1 TO LEN( aCursors[1] )
       hCursor := NIL
       aSize   := {0,0}
       IF aCursors[2][n] != NIL
          hCursor := ::System:Cursor[ aCursors[1][n] ]
          aSize := __GetIconSize( hCursor )
       ENDIF
       AADD( ::Cursors, { aCursors[1][n], hCursor, aSize[1], aSize[2] } )
       ::AddItem( aCursors[1][n] )
   NEXT
   ::ItemHeight := 32
RETURN Self


METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS CursorComboBox
   LOCAL n, y, lSelected, aClip, nLen, itemTxt, aSize
   (nwParam, nlParam)
   IF dis:hwndItem == ::hWnd
      lSelected := (dis:itemState & ODS_SELECTED) != 0
      aClip     := { dis:rcItem:Left+20,  dis:rcItem:Top, ;
                     dis:rcItem:Right, dis:rcItem:Bottom  }
      IF (dis:itemAction & ODA_DRAWENTIRE) != 0 .OR. (dis:itemAction & ODA_SELECT) != 0
         SetTextColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )
         itemTxt := Left( itemTxt, nLen )
         n := dis:itemID +1 //ASCAN( ::Cursors, {|a|a[1] == itemTxt } )

         aSize := _GetTextExtentPoint32( dis:hDC, itemTxt )
         y := dis:rcItem:Top + ((dis:rcItem:Bottom-dis:rcItem:Top)/2) - (aSize[2]/2)

         IF n > 0
            ExtTextOut( dis:hDC, ::Cursors[n][3]+10, y, ETO_OPAQUE + ETO_CLIPPED, dis:rcItem, itemTxt )
            IF (dis:itemState & ODS_COMBOBOXEDIT) == 0 .AND. ::Cursors[n][2] != NIL
               DrawIcon( dis:hDC, 3, dis:rcItem:Top, ::Cursors[n][2] )
            ENDIF
          ELSE
            ExtTextOut( dis:hDC, ( dis:rcItem:right - aSize[1] )/2, y, ETO_OPAQUE + ETO_CLIPPED, dis:rcItem, itemTxt )
         ENDIF

      ENDIF
      IF (dis:itemState & ODS_FOCUS) != 0 .OR. (dis:itemAction & ODA_FOCUS) != 0
         aClip := { dis:rcItem:Left,  dis:rcItem:Top, ;
                    dis:rcItem:Right, dis:rcItem:Bottom  }
         _DrawfocusRect( dis:hDC, aclip )
      ENDIF
   ENDIF
RETURN 0



//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------


CLASS FontComboBox INHERIT ComboBox
   DATA Fonts EXPORTED INIT {}
   DATA Owner EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnParentDrawItem()
   METHOD OnDestroy() INLINE Super:OnDestroy(), ::Owner := NIL, ::Fonts := NIL, hb_gcAll(), NIL
ENDCLASS

METHOD Init( oParent ) CLASS FontComboBox
   ::__xCtrlName := "FontComboBox"
   ::Super:Init( oParent )
   ::Style := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
RETURN Self

METHOD Create() CLASS FontComboBox
   LOCAL n, cFont := ::Cargo
   ::Super:Create()
   ::Fonts := ::Form:Drawing:EnumFonts()
   ::Form:Drawing:CleanEnumFont()
   ASORT( ::Fonts,,, {|a,b| a[1]:lfFaceName:AsString() <  b[1]:lfFaceName:AsString() } )

   FOR n := 1 TO LEN( ::Fonts )
       ::Fonts[n][1]:lfHeight := ::Parent:Font:Height //::Owner:&cFont:Height
       IF cFont != NIL
          ::Fonts[n][1]:lfWeight := ::Owner:&cFont:Weight
          ::Fonts[n][1]:lfWidth  := ::Owner:&cFont:Width
          ::Fonts[n][1]:lfItalic := ::Owner:&cFont:nItalic
        ELSE
          ::Fonts[n][1]:lfWeight := ::Parent:Font:Weight
          ::Fonts[n][1]:lfWidth  := ::Parent:Font:Width
       ENDIF
       ::AddItem( ::Fonts[n][1]:lfFaceName:AsString() )
       ::SendMessage( CB_SETITEMHEIGHT, n-1, 15 )
   NEXT
   ::ItemToolTips:= .T.
RETURN Self

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS FontComboBox
   LOCAL n, y, lSelected, aClip, nLen, itemTxt, aSize, hFont, hOld
   (nwParam, nlParam)
   IF dis:hwndItem == ::hWnd
      lSelected := (dis:itemState & ODS_SELECTED) != 0
      aClip     := { dis:rcItem:Left+20,  dis:rcItem:Top, ;
                     dis:rcItem:Right, dis:rcItem:Bottom  }
      IF (dis:itemAction & ODA_DRAWENTIRE) != 0 .OR. (dis:itemAction & ODA_SELECT) != 0
         SetTextColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )
         itemTxt := Left( itemTxt, nLen )
         n := dis:itemID +1

         aSize := _GetTextExtentPoint32( dis:hDC, itemTxt )
         y := dis:rcItem:Top + ((dis:rcItem:Bottom-dis:rcItem:Top)/2) - (aSize[2]/2)

         IF n > 0
            hFont := CreateFontIndirect( ::Fonts[n][1] )
            hOld  := SelectObject( dis:hDC, hFont )

            FillRect( dis:hDC, dis:rcItem, GetSysColorBrush( IIF( lselected, COLOR_HIGHLIGHT, COLOR_WINDOW )) )
            DrawText( dis:hDC, itemTxt, dis:rcItem, (DT_LEFT | DT_VCENTER | DT_SINGLELINE) )

            //ExtTextOut( dis:hDC, 10, y, ETO_OPAQUE + ETO_CLIPPED, dis:rcItem, itemTxt )
            SelectObject( dis:hDC, hOld )
            DeleteObject( hFont )
         ENDIF
      ENDIF
      IF (dis:itemState & ODS_FOCUS) != 0 .OR. (dis:itemAction & ODA_FOCUS) != 0
         aClip := { dis:rcItem:Left,  dis:rcItem:Top, ;
                    dis:rcItem:Right, dis:rcItem:Bottom  }
         _DrawfocusRect( dis:hDC, aclip )
      ENDIF
   ENDIF
RETURN 0

//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------

CLASS ComboBoxEx INHERIT ComboBox
   PROPERTY ImageList GET __ChkComponent( Self, @::xImageList ) SET ::SetImageList(v)

   DATA hControl   EXPORTED

   METHOD Init() CONSTRUCTOR
   MESSAGE AddString METHOD AddItem
   METHOD AddItem()
   METHOD Create()
   METHOD SetImageList()
   METHOD OnSize()
   METHOD SetItemHeight()
   METHOD SetHorizontalExtent(nWidth)  INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_SETHORIZONTALEXTENT, nWidth, 0 ) , NIL )
   METHOD OnMove()
ENDCLASS

METHOD Init( oParent ) CLASS ComboBoxEx
   ::Super:Init( oParent )
   ::__xCtrlName := "ComboBoxEx"
   ::ClsName     := WC_COMBOBOXEX
   ::Style       := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::ExStyle     := 0
RETURN Self

METHOD Create() CLASS ComboBoxEx
   LOCAL h
   ::Super:Create()
   IF ::ImageList != NIL
      ::SetImageList( ::ImageList )
   ENDIF
   h := ::SendMessage( CBEM_GETCOMBOCONTROL, 0, 0 )
   SendMessage( h, CB_SETMINVISIBLE, ::xHeight/::xItemHeight )
   ::hControl := ::SendMessage( CBEM_GETCOMBOCONTROL, 0, 0 )
   IF ::DesignMode
      ::BackColor := ::Parent:BackColor
   ENDIF
RETURN Self

METHOD AddItem( cText, nImage ) CLASS ComboBoxEx
   LOCAL cbei := (struct COMBOBOXEXITEM)
   DEFAULT nImage TO ::GetCount()+1
   cbei:mask           := (CBEIF_TEXT | CBEIF_INDENT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE)
   cbei:iItem          := -1
   cbei:pszText        := cText
   cbei:cchTextMax     := LEN( cbei:pszText )+1
   cbei:iImage         := nImage-1
   cbei:iSelectedImage := nImage-1
   cbei:iIndent        := 0
   SendMessage( ::hWnd, CBEM_INSERTITEM, 0, cbei )
RETURN Self

METHOD SetImageList( oList ) CLASS ComboBoxEx
   LOCAL hWnd
   IF ::hWnd != NIL
      ::SendMessage( CBEM_SETIMAGELIST, 0, IIF( oList != NIL, oList:Handle, NIL ) )
      IF oList != NIL
         hWnd := ::SendMessage( CBEM_GETCOMBOCONTROL, 0, 0 )
         ::xItemHeight := oList:IconHeight
         SendMessage( hWnd, CB_SETMINVISIBLE, ::xHeight/::xItemHeight )
      ENDIF
   ENDIF
RETURN Self

METHOD OnSize() CLASS ComboBoxEx
   LOCAL n := ::GetItemHeight( 1 )
   MoveWindow( ::hControl, 0, 0, ::ClientWidth, n )
   ::BackColor := ::Parent:BackColor
RETURN NIL

METHOD OnMove() CLASS ComboBoxEx
   ::BackColor := ::Parent:BackColor
RETURN NIL

METHOD SetItemHeight(nLine, n) CLASS ComboBoxEx
   LOCAL h
   IF nLine == -1
      ::xSelectionHeight := n
   ENDIF
   IF ::hWnd != NIL
      h := ::SendMessage( CBEM_GETCOMBOCONTROL, 0, 0 )
      SendMessage( h, CB_SETITEMHEIGHT, IIF( nLine != -1, nLine-1, -1), n )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------

CLASS FormComboBox INHERIT ComboBox
   DATA hFont1 EXPORTED
   DATA hFont2 EXPORTED
   DATA Owner  EXPORTED
   DATA aItems EXPORTED INIT {}
   METHOD Init() CONSTRUCTOR
   METHOD OnParentDrawItem()
   METHOD Reset()
   METHOD OnDestroy() INLINE Super:OnDestroy(), DeleteObject( ::hFont1 ), DeleteObject( ::hFont2 ), NIL
   METHOD Create()
   METHOD OnParentCommand()
   METHOD SelectControl()
   METHOD ResetContent()
ENDCLASS

METHOD Init( oParent ) CLASS FormComboBox
   ::__xCtrlName := "FormComboBox"
   ::Super:Init( oParent )
RETURN Self

METHOD Create() CLASS FormComboBox
   ::Style  := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::hFont1 := __GetMessageFont( 700 )
   ::hFont2 := __GetMessageFont( 400 )
   Super:Create()
RETURN Self

METHOD ResetContent() CLASS FormComboBox
   LOCAL n
   FOR n := 1 TO LEN( ::aItems )
       ::aItems[n] := NIL
   NEXT
   ::aItems := {}
   Super:ResetContent()
RETURN NIL

METHOD SelectControl( oControl ) CLASS FormComboBox
   LOCAL n := ASCAN( ::aItems, oControl,,, .T. )//MAX( ::FindString(, oControl:Name ), 1 )
   ::SetCurSel( MAX(n,1))
RETURN Self

METHOD Reset( oControl ) CLASS FormComboBox
   LOCAL oForm, aObj, oCtrl
   ::SetRedraw(.F.)
   ::ResetContent()
   IF ::Application:Project:Properties != NIL
      IF ( oForm := ::Application:Project:CurrentForm ) != NIL
         aObj := {::Application:Project:CurrentForm}

         GetObjChildren( @aObj, oForm )

         ASORT( aObj,,, {|a,b| Upper(a:Name) < Upper(b:Name) } )

         FOR EACH oCtrl IN aObj
             TRY
                ::AddItem( oCtrl:Name + CHR(9) + oCtrl:__xCtrlName )
                AADD( ::aItems, oCtrl )
             CATCH
             END
         NEXT

         aObj := NIL
      ELSE
         IF ! Empty( ::Application:Project:Forms )
            ::AddItem( "Application" + CHR(9) + "Visual xHarbour" )
            AADD( ::aItems, ::Application:Project:AppObject )
         ENDIF
         ::AddItem( ::Application:Project:Properties:Name + CHR(9) + "Project" )
         AADD( ::aItems, ::Application:Project:Properties )
      ENDIF
      IF oControl != NIL
         ::Application:Yield()
         ::SelectControl( oControl )
       ELSE
         ::SetCurSel(1)
      ENDIF
   ENDIF
   ::SetRedraw(.T.)
RETURN Self

FUNCTION GetObjChildren( aObj, oObj )
   LOCAL oCtrl
   IF oObj != NIL
      FOR EACH oCtrl IN oObj:Components
          AADD( aObj, oCtrl )
      NEXT
      TRY
         FOR EACH oCtrl IN oObj:Children
             AADD( aObj, oCtrl )
             GetObjChildren( @aObj, oCtrl )
         NEXT
      CATCH
      END
   ENDIF
RETURN NIL

METHOD OnParentDrawItem( nwParam, nlParam, dis ) CLASS FormComboBox
   LOCAL n, lSelected, nLen, itemTxt, cText, aSize, hDC, hOld
   (nwParam, nlParam)
   IF dis != NIL .AND. dis:hwndItem == ::hWnd
      hDC := dis:hDC
      lSelected := (dis:itemState & ODS_SELECTED) != 0

      IF (dis:itemAction & ODA_DRAWENTIRE) != 0 .OR. (dis:itemAction & ODA_SELECT) != 0
         SetTextColor( hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )

         itemTxt := Left( itemTxt, nLen )
         n := AT( chr(9), itemTxt )
         cText := SUBSTR( itemTxt, 1, n-1 )

         SelectObject( hDC, ::hFont1 )

         aSize := _GetTextExtentPoint32( hDC, cText )

         _ExtTextOut( hDC, 5, dis:rcItem:Top, ETO_OPAQUE + ETO_CLIPPED, dis:rcItem:Array, cText )

         hOld  := SelectObject( hDC, ::hFont2 )
         cText := SUBSTR( itemTxt, n+1 )
         _ExtTextOut( hDC, aSize[1]+15, dis:rcItem:Top, ETO_CLIPPED, dis:rcItem:Array, cText )
         SelectObject( hOld )
      ENDIF
      IF (dis:itemState & ODS_FOCUS) != 0 .OR. (dis:itemAction & ODA_FOCUS) != 0
         _DrawfocusRect( hDC, dis:rcItem:Array )
      ENDIF
   ENDIF
RETURN 0

METHOD OnParentCommand( nId, nCode ) CLASS FormComboBox
   LOCAL nSel, nRet := 0
   (nId)
   DO CASE
      CASE nCode == CBN_SELENDOK
           nSel := ::GetCurSel()
           TRY
              ::Application:Project:CurrentForm:SelectControl( ::aItems[nSel] )
           CATCH
              ::Application:ObjectManager:ResetProperties( {{::aItems[nSel]}} )
              ::Application:EventManager:ResetEvents( {{::aItems[nSel]}} )
           END

      CASE nCode == CBN_SELENDCANCEL
           __Evaluate( ::OnCBNSelEndCancel, Self,,,,)

      CASE nCode == CBN_SELCHANGE
           RETURN NIL
      CASE nCode == CBN_DBLCLK
      CASE nCode == CBN_ERRSPACE
      CASE nCode == CBN_KILLFOCUS
           __Evaluate( ::OnCBNKillFocus, Self,,,,)

      CASE nCode == CBN_SETFOCUS

   ENDCASE
RETURN nRet
