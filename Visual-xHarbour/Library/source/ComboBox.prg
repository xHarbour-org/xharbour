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

   ACCESS CurSel            INLINE ::GetCurSel()
   ASSIGN Cursel(n)         INLINE ::SetCurSel(n)

   // temporary ONLY for compatibility
   ACCESS AutoHorzScroll INLINE ::AutoEditHorzScroll
   ASSIGN AutoHorzScroll(l) INLINE ::AutoEditHorzScroll := l
   //-----------------------------------------------------------
   PROPERTY HorzScroll         INDEX WS_HSCROLL            READ xHorzScroll         WRITE SetStyle          PROTECTED DEFAULT .F. 
   PROPERTY Border             INDEX WS_BORDER             READ xBorder             WRITE SetStyle                    DEFAULT .F.
   PROPERTY SelectionHeight    INDEX -1                    READ xSelectionHeight    WRITE SetItemHeight     PROTECTED DEFAULT 15
   PROPERTY OwnerDrawFixed     INDEX CBS_OWNERDRAWFIXED    READ xOwnerDrawFixed     WRITE SetStyle          PROTECTED DEFAULT .F.
   PROPERTY OwnerDrawVariable  INDEX CBS_OWNERDRAWVARIABLE READ xOwnerDrawVariable  WRITE SetStyle          PROTECTED DEFAULT .F.
   PROPERTY AutoEditHorzScroll INDEX CBS_AUTOHSCROLL       READ xAutoEditHorzScroll WRITE SetStyle          PROTECTED DEFAULT .F.
   PROPERTY Sort               INDEX CBS_SORT              READ xSort               WRITE SetStyle          PROTECTED DEFAULT .F.

   PROPERTY DropDownStyle                                  READ xDropDownStyle      WRITE SetDropDownStyle  PROTECTED DEFAULT __GetSystem():DropDownStyle:DropDownList

   PROPERTY UpperCase          INDEX CBS_UPPERCASE         READ xUpperCase          WRITE SetStyle          PROTECTED DEFAULT .F.
   PROPERTY LowerCase          INDEX CBS_LOWERCASE         READ xLowerCase          WRITE SetStyle          PROTECTED DEFAULT .F.

   PROPERTY HasStrings         INDEX CBS_HASSTRINGS        READ xHasStrings         WRITE SetStyle          PROTECTED DEFAULT .F.
   PROPERTY DisableNoScroll    INDEX CBS_DISABLENOSCROLL   READ xDisableNoScroll    WRITE SetStyle          PROTECTED DEFAULT .T.
   PROPERTY NoIntegralHeight   INDEX CBS_NOINTEGRALHEIGHT  READ xNoIntegralHeight   WRITE SetStyle          PROTECTED DEFAULT .F.
   PROPERTY OemConvert         INDEX CBS_OEMCONVERT        READ xOemConvert         WRITE SetStyle          PROTECTED DEFAULT .F.
   PROPERTY ItemHeight         INDEX 1                     READ xItemHeight         WRITE SetItemHeight     PROTECTED
   PROPERTY VertScroll         INDEX WS_VSCROLL            READ xVertScroll         WRITE SetStyle          PROTECTED DEFAULT .F.

   PROPERTY ClientEdge         INDEX WS_EX_CLIENTEDGE      READ xClientEdge         WRITE SetExStyle        PROTECTED DEFAULT .F. 
   PROPERTY ItemToolTips                                   READ xItemToolTips       WRITE __SetItemToolTips PROTECTED DEFAULT .F.

   DATA FitToolBar        PUBLISHED INIT .T.
   DATA Flat              PUBLISHED INIT .F.

   DATA OnCBNSelEndOk     EXPORTED
   DATA OnCBNSelEndCancel EXPORTED
   DATA OnCBNKillFocus    EXPORTED
   DATA cbi               EXPORTED   
   DATA __pListCallBack   EXPORTED
   DATA __pTipCallBack    EXPORTED
   DATA __nListProc       EXPORTED
   DATA __nTipProc        EXPORTED
   DATA __OriginalSel     EXPORTED INIT LB_ERR

   DATA __nWidth   PROTECTED INIT 0
   DATA __aCustom  PROTECTED
   DATA __tipWnd   PROTECTED
   DATA __isEnter  PROTECTED INIT .F.

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

   METHOD ResetContent()                    INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_RESETCONTENT, 0, 0 ) , NIL )
   METHOD GetEditSel(nLine)                 INLINE IIF( ::hWnd != NIL, ::SendMessage( CB_GETEDITSEL, nLine-1, 0 ), NIL )
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
   METHOD OnDestroy()                      INLINE ::__SetItemToolTips(.F.), NIL
   METHOD OnWindowPosChanged()             INLINE ::CallWindowProc(), ::SetItemHeight( -1, ::xSelectionHeight ), ::SetItemHeight( 2, ::xItemHeight ), 0
   METHOD OnKillFocus()                    INLINE IIF( ::DropDownStyle <> CBS_DROPDOWNLIST, 0, NIL )
   METHOD __ListCallBack()
   METHOD __TipCallBack()
   METHOD __ListboxMouseMove()
   METHOD __TrackMouseEvent()
   METHOD __HandleOnPaint()
   METHOD __HandleOnTimer()
   METHOD __SetItemToolTips()
ENDCLASS

//--------------------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ComboBox
   DEFAULT ::__xCtrlName TO "ComboBox"
   ::ClsName    := "ComboBox"
   ::ThemeName  := "combobox"
   ::Style      := WS_CHILD | WS_VISIBLE | WS_TABSTOP | CBS_DROPDOWNLIST | CBS_DISABLENOSCROLL  | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Super:Init( oParent )
   ::Width      := 100
   ::Height     := 100
   IF !EMPTY( ::Events )
      AADD( ::Events[3][2], { "OnCBNSelEndOk" , "", "" } )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD SetDropDownStyle( nDrop ) CLASS ComboBox
   ::Style := ::Style & NOT( CBS_DROPDOWNLIST )
   ::Style := ::Style & NOT( CBS_DROPDOWN )
   ::Style := ::Style & NOT( CBS_SIMPLE )
   ::Style := ::Style | nDrop
   IF ::IsWindow()
      ::SetWindowLong( GWL_STYLE, ::Style )
      ::SetWindowPos(, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
      ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS ComboBox
   ::Super:Create()
   ::SetItemHeight( -1, ::xSelectionHeight )
   ::SetItemHeight( 2, ::xItemHeight )
   DEFAULT ::xItemHeight TO ::SendMessage( CB_GETITEMHEIGHT, 0, 0 )
   ::SendMessage( CB_SETMINVISIBLE, ::xHeight/::xItemHeight )
   ::ClientEdge := ::xClientEdge
   
   IF ::ItemToolTips
      ::__SetItemToolTips( .T. )
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
         wcex:style          := CS_OWNDC | CS_DBLCLKS | CS_SAVEBITS | CS_DROPSHADOW
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
         FreeCallBackPointer( ::__pTipCallBack )
         ::__pTipCallBack := NIL

         DestroyWindow( ::__tipWnd )

         IF IsWindow( ::cbi:hwndList ) .AND. ::__nListProc != NIL
            SetWindowLong( ::cbi:hwndList, GWL_WNDPROC, ::__nListProc )
            ::__nListProc := NIL
            FreeCallBackPointer( ::__pListCallBack )
            ::__pListCallBack := NIL
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
METHOD DrawFrame( oDrawing, aRect, nAlign, nWidth, nStatus ) CLASS ComboBox
   LOCAL hTheme, nFlags := DFCS_SCROLLCOMBOBOX
   IF nStatus != NIL
      IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
         nStatus := CBXS_NORMAL
      ENDIF
      nFlags := nFlags | nStatus
   ENDIF
   aRect[1] := aRect[3] - GetSystemMetrics( SM_CXVSCROLL )
   IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive
      hTheme := OpenThemeData(,"combobox")
      aRect[4]-=2
      oDrawing:DrawThemeBackground( hTheme, CP_DROPDOWNBUTTON, nStatus, aRect, aRect )
      CloseThemeData( hTheme )
    ELSE
      oDrawing:DrawFrameControl( aRect, DFC_SCROLL, nFlags )
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
METHOD OnParentCommand( nId, nCode, nlParam ) CLASS ComboBox
   LOCAL nRet := NIL
   DO CASE
      CASE nCode == CBN_SELENDOK
           nRet := __Evaluate( ::OnCBNSelEndOk, Self,,,,)
           nRet := __Evaluate( ::Action, Self,,,,)
           nRet := ExecuteEvent( "OnCBNSelEndOk", Self )
           nRet := 0
      CASE nCode == CBN_SELENDCANCEL
           __Evaluate( ::OnCBNSelEndCancel, Self,,,,)

      CASE nCode == CBN_SELCHANGE

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
   LOCAL xPos, yPos, aPt, aRect, nCurSel
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
              ::__TrackMouseEvent( hWnd, TME_HOVER|TME_LEAVE )
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

   _DrawText( hDC, cText, aRect, DT_SINGLELINE|DT_CENTER|DT_VCENTER|DT_NOPREFIX )

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
   LOCAL hDC, hOldFont, cBuf, nLen, pt := (struct POINT)
   LOCAL rcBounds := (struct RECT)
   LOCAL rcDraw := (struct RECT)
   LOCAL cRect := space(16)
   LOCAL nCurSel := SendMessage( hList, LB_ITEMFROMPOINT, 0, MAKELONG( aPt[1], aPt[2] ) )

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

   DrawText( hDC, cBuf, @rcDraw, DT_CALCRECT|DT_SINGLELINE|DT_CENTER|DT_VCENTER|DT_NOPREFIX )

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

   SetWindowPos( ::__tipWnd, HWND_TOPMOST, rcDraw:left+1, rcDraw:top, rcDraw:Right-rcDraw:left+4, rcDraw:Bottom-rcDraw:top, SWP_NOACTIVATE | SWP_SHOWWINDOW )
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
   METHOD __WindowDestroy()
ENDCLASS

METHOD Init( oParent ) CLASS DriveCombobox
   LOCAL cDrives, n, nType, cType, shfi, cBuffer
   ::__xCtrlName := "DriveComboBox"
   ::Super:Init( oParent )
   ::Style     := WS_CHILD | WS_VISIBLE | WS_TABSTOP | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   IF !::Application:IsThemedXP
      ::Border := .F.
   ENDIF
   shfi := (struct SHFILEINFO)
   ::HorzScroll := .T.
   ::VertScroll := .T.
   cDrives := SPACE( 256 )
   GetLogicalDriveStrings( 256, @cDrives )
   STRTRAN( cDrives, CHR(0) )
   FOR n := 1 TO LEN( ALLTRIM(cDrives ))-1
//       cBuffer := shfi:Value()
       SHGetFileInfo( SUBSTR( cDrives, n, 3 ),, @shfi, SHGFI_ICON | SHGFI_SMALLICON | SHGFI_DISPLAYNAME)
//       shfi:Buffer( cBuffer )
       cType := shfi:szDisplayName:AsString()//:Value()
       AADD( ::Drives, { SUBSTR( cDrives, n, 2 ), cType, shfi:hIcon } )
       n += 3
   NEXT
   IF ::__ClassInst != NIL
      ::__PropFilter := { "HIGHLIGHTCAPTION", "SMALLCAPTION", "ALLOWMAXIMIZE" }
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

METHOD OnParentDrawItem( nwParam, nlParam ) CLASS DriveCombobox
   LOCAL n, x, lSelected, aClip, nLen, itemTxt, cText, aRect, nField, y
   IF ::Parent:DrawItemStruct != NIL .AND. ::Parent:DrawItemStruct:hwndItem == ::hWnd
      lSelected := ::Parent:DrawItemStruct:itemState & ODS_SELECTED != 0
      aClip     := { ::Parent:DrawItemStruct:rcItem:Left+20,  ::Parent:DrawItemStruct:rcItem:Top, ;
                     ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }
      IF ::Parent:DrawItemStruct:itemAction & ODA_DRAWENTIRE != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_SELECT != 0
         SetTextColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXTLEN, ::Parent:DrawItemStruct:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXT, ::Parent:DrawItemStruct:itemID, @itemTxt )

         itemTxt := Left( itemTxt, nLen )
         cText   := ""
         aRect   := ACLONE(aClip)
         nField  := 1

         _ExtTextOut( ::Parent:DrawItemStruct:hDC, 0, ::Parent:DrawItemStruct:rcItem:Top, ETO_OPAQUE + ETO_CLIPPED, { ::Parent:DrawItemStruct:rcItem:Left, ::Parent:DrawItemStruct:rcItem:Top, ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom }, " " )
         FOR n := 3 to nLen + 1
             IF SubStr( itemTxt, n, 1) == chr(9) .or. n == nLen + 1
                x := aRect[1] + 2
                _DrawText( ::Parent:DrawItemStruct:hDC, cText, {x, aRect[2], aRect[3], aRect[4] }, DT_VCENTER | DT_SINGLELINE )
                cText := ""
                aRect[1] += 4 
                nField ++

                LOOP
             ENDIF

             cText += SubStr( itemTxt, n, 1 )
         NEXT

         n := ASCAN( ::Drives, {|a|a[1] == LEFT( itemTxt, 2 ) } )
         IF n > 0
            y := ::Parent:DrawItemStruct:rcItem:Top + ((::Parent:DrawItemStruct:rcItem:Bottom-::Parent:DrawItemStruct:rcItem:Top)/2) - (16/2)
            DrawIconEx( ::Parent:DrawItemStruct:hDC, 3, y, ::Drives[ n ][3], 16, 16, 0, NIL,  DI_NORMAL )
         ENDIF

      ENDIF
      IF ::Parent:DrawItemStruct:itemState & ODS_FOCUS != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_FOCUS != 0
         aClip := { ::Parent:DrawItemStruct:rcItem:Left,  ::Parent:DrawItemStruct:rcItem:Top, ;
                    ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }
         _DrawfocusRect( ::Parent:DrawItemStruct:hDC, aclip )
      ENDIF
   ENDIF
RETURN Self

METHOD __WindowDestroy() CLASS DriveCombobox
   LOCAL aItem
   ::Super:__WindowDestroy()
   FOR EACH aItem IN ::Drives
       DestroyIcon( aItem[3] )
   NEXT
RETURN NIL

//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------


CLASS ColorPicker INHERIT ComboBox
   DATA ColorSelected   EXPORTED
   DATA Colors          EXPORTED INIT {}
   DATA SysDefault      EXPORTED
   DATA Custom          EXPORTED
   DATA AllowCustomColor   PUBLISHED INIT .F.
   DATA AllowSystemDefault PUBLISHED INIT .F.
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnParentDrawItem()
//   METHOD OnPaint()
   METHOD OnParentCommand()
   METHOD SelectColor()
ENDCLASS

METHOD Init( oParent ) CLASS ColorPicker
   LOCAL aColors, n, nType, cType, shfi, cBuffer
   ::__xCtrlName := "ColorPicker"
   ::Super:Init( oParent )
   ::Style     := WS_CHILD | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
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

METHOD OnParentCommand( nId, nCode, nlParam ) CLASS ColorPicker
   LOCAL cSel, nColor, nRet

   DO CASE
      CASE nCode == CBN_SELENDOK
           cSel := ::Colors[ ::GetCurSel() ][2]
           DO CASE
              CASE cSel == "System Default..."
                   ::ColorSelected := NIL

              CASE cSel == "Custom..."
                   nColor := ::ColorSelected //::Colors[ ::GetCurSel()+1 ][1]
                   DEFAULT ::__aCustom TO ARRAY(16)
                   IF _ChooseColor( ::Parent:hWnd, @nColor, ::__aCustom, CC_ANYCOLOR | CC_RGBINIT | CC_SOLIDCOLOR | CC_FULLOPEN )
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

METHOD OnParentDrawItem( nwParam, nlParam ) CLASS ColorPicker
   LOCAL n, x, lSelected, aClip, nLen, itemTxt, cText, nField, hBrush, hOld, z
   IF ::Parent:DrawItemStruct != NIL .AND. ::Parent:DrawItemStruct:hwndItem == ::hWnd
      lSelected := ::Parent:DrawItemStruct:itemState & ODS_SELECTED != 0
      aClip     := { ::Parent:DrawItemStruct:rcItem:Left+20,  ::Parent:DrawItemStruct:rcItem:Top, ;
                     ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }

      IF ::Parent:DrawItemStruct:itemAction & ODA_DRAWENTIRE != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_SELECT != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_FOCUS != 0
         SetTextColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXTLEN, ::Parent:DrawItemStruct:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXT, ::Parent:DrawItemStruct:itemID, @itemTxt )
         itemTxt := left( itemTxt, nLen )

         z := 0
         IF ::Parent:DrawItemStruct:itemState & ODS_COMBOBOXEDIT != 0
            z := 2
         ENDIF
         //_ExtTextOut( ::Parent:DrawItemStruct:hDC, IIF( itemTxt != "None", 28, 3 ), ::Parent:DrawItemStruct:rcItem:Top-z, ETO_OPAQUE + ETO_CLIPPED, { ::Parent:DrawItemStruct:rcItem:Left, ::Parent:DrawItemStruct:rcItem:Top, ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom }, itemTxt )

         FillRect( ::Parent:DrawItemStruct:hDC, ::Parent:DrawItemStruct:rcItem, GetSysColorBrush( IIF( lselected, COLOR_HIGHLIGHT, COLOR_WINDOW )) )
         IF itemTxt != "None"
            ::Parent:DrawItemStruct:rcItem:Left += 28
          ELSE
            ::Parent:DrawItemStruct:rcItem:Left += 3
         ENDIF
         DrawText( ::Parent:DrawItemStruct:hDC, itemTxt, ::Parent:DrawItemStruct:rcItem, DT_LEFT | DT_VCENTER | DT_SINGLELINE )
         IF itemTxt != "None"
            ::Parent:DrawItemStruct:rcItem:Left -= 28
          ELSE
            ::Parent:DrawItemStruct:rcItem:Left -= 3
         ENDIF

         n := ::Parent:DrawItemStruct:itemID +1 //ASCAN( ::Colors, {|a|a[2] == itemTxt} )

         IF n > 0
            z := 2
            IF ::Parent:DrawItemStruct:itemState & ODS_COMBOBOXEDIT != 0
               z := 0
            ENDIF
            IF itemTxt != "None"
               hBrush := CreateSolidBrush( ::Colors[n][1] )
               hOld := SelectObject( ::Parent:DrawItemStruct:hDC, hBrush )
               Rectangle( ::Parent:DrawItemStruct:hDC, ::Parent:DrawItemStruct:rcItem:Left+z, ::Parent:DrawItemStruct:rcItem:Top+z, ::Parent:DrawItemStruct:rcItem:Left+22+z, ::Parent:DrawItemStruct:rcItem:Bottom-z )
               SelectObject( ::Parent:DrawItemStruct:hDC, hOld )
               DeleteObject( hBrush )
            ENDIF
         ENDIF
      ENDIF

      //IF ::Parent:DrawItemStruct:itemState & ODS_COMBOBOXEDIT == 0
      //   IF ::Parent:DrawItemStruct:itemState & ODS_FOCUS != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_FOCUS != 0
      //      aClip := { ::Parent:DrawItemStruct:rcItem:Left,  ::Parent:DrawItemStruct:rcItem:Top, ;
      //                 ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }
      //      _DrawfocusRect( ::Parent:DrawItemStruct:hDC, aclip )
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
   LOCAL cDrives, n, nType, cType, shfi, cBuffer
   ::__xCtrlName := "CursorComboBox"
   ::Super:Init( oParent )
   ::Style     := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
RETURN Self

METHOD Create() CLASS CursorComboBox
   LOCAL n, hCursor, aSize, cKey, ii := (struct ICONINFO)

   ::Super:Create()
   ::Cursors := {}
   FOR n := 1 TO LEN( ::__Cursors )
       hCursor := HGetValueAt( ::System:Cursor, HAAGetRealPos( ::System:Cursor, n ) )
       aSize := __GetIconSize( hCursor )
       AADD( ::Cursors, { ::__Cursors[n], hCursor, aSize[1], aSize[2] } )
       ::AddItem( ::__Cursors[n] )
   NEXT
   ::ItemHeight := 32
RETURN Self


METHOD OnParentDrawItem( nwParam, nlParam ) CLASS CursorComboBox
   LOCAL n, x, y, lSelected, aClip, nLen, itemTxt, cText, aRect, nField, aSize
   IF ::Parent:DrawItemStruct:hwndItem == ::hWnd
      lSelected := ::Parent:DrawItemStruct:itemState & ODS_SELECTED != 0
      aClip     := { ::Parent:DrawItemStruct:rcItem:Left+20,  ::Parent:DrawItemStruct:rcItem:Top, ;
                     ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }
      IF ::Parent:DrawItemStruct:itemAction & ODA_DRAWENTIRE != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_SELECT != 0
         SetTextColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXTLEN, ::Parent:DrawItemStruct:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXT, ::Parent:DrawItemStruct:itemID, @itemTxt )
         itemTxt := Left( itemTxt, nLen )
         n := ::Parent:DrawItemStruct:itemID +1 //ASCAN( ::Cursors, {|a|a[1] == itemTxt } )

         aSize := _GetTextExtentPoint32( ::Parent:DrawItemStruct:hDC, itemTxt )
         y := ::Parent:DrawItemStruct:rcItem:Top + ((::Parent:DrawItemStruct:rcItem:Bottom-::Parent:DrawItemStruct:rcItem:Top)/2) - (aSize[2]/2)

         IF n > 0
            ExtTextOut( ::Parent:DrawItemStruct:hDC, ::Cursors[n][3]+10, y, ETO_OPAQUE + ETO_CLIPPED, ::Parent:DrawItemStruct:rcItem, itemTxt )
            IF ::Parent:DrawItemStruct:itemState & ODS_COMBOBOXEDIT == 0
               DrawIcon( ::Parent:DrawItemStruct:hDC, 3, ::Parent:DrawItemStruct:rcItem:Top, ::Cursors[n][2] )
            ENDIF
          ELSE
            ExtTextOut( ::Parent:DrawItemStruct:hDC, ( ::Parent:DrawItemStruct:rcItem:right - aSize[1] )/2, y, ETO_OPAQUE + ETO_CLIPPED, ::Parent:DrawItemStruct:rcItem, itemTxt )
         ENDIF

      ENDIF
      IF ::Parent:DrawItemStruct:itemState & ODS_FOCUS != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_FOCUS != 0
         aClip := { ::Parent:DrawItemStruct:rcItem:Left,  ::Parent:DrawItemStruct:rcItem:Top, ;
                    ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }
         _DrawfocusRect( ::Parent:DrawItemStruct:hDC, aclip )
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
ENDCLASS

METHOD Init( oParent ) CLASS FontComboBox
   LOCAL cDrives, n, nType, cType, shfi, cBuffer
   ::__xCtrlName := "FontComboBox"
   ::Super:Init( oParent )
   ::Style     := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
RETURN Self

METHOD Create() CLASS FontComboBox
   LOCAL n, Font, aSize, cKey, nHeight, cFont := ::Cargo
   ::Super:Create()
   ::Fonts := ::Form:Drawing:EnumFonts()
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

METHOD OnParentDrawItem( nwParam, nlParam ) CLASS FontComboBox
   LOCAL n, x, y, lSelected, aClip, nLen, itemTxt, cText, aRect, nField, aSize, hFont, hOld
   IF ::Parent:DrawItemStruct:hwndItem == ::hWnd
      lSelected := ::Parent:DrawItemStruct:itemState & ODS_SELECTED != 0
      aClip     := { ::Parent:DrawItemStruct:rcItem:Left+20,  ::Parent:DrawItemStruct:rcItem:Top, ;
                     ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }
      IF ::Parent:DrawItemStruct:itemAction & ODA_DRAWENTIRE != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_SELECT != 0
         SetTextColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXTLEN, ::Parent:DrawItemStruct:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXT, ::Parent:DrawItemStruct:itemID, @itemTxt )
         itemTxt := Left( itemTxt, nLen )
         n := ::Parent:DrawItemStruct:itemID +1

         aSize := _GetTextExtentPoint32( ::Parent:DrawItemStruct:hDC, itemTxt )
         y := ::Parent:DrawItemStruct:rcItem:Top + ((::Parent:DrawItemStruct:rcItem:Bottom-::Parent:DrawItemStruct:rcItem:Top)/2) - (aSize[2]/2)

         IF n > 0
            hFont := CreateFontIndirect( ::Fonts[n][1] )
            hOld  := SelectObject( ::Parent:DrawItemStruct:hDC, hFont )
            
            FillRect( ::Parent:DrawItemStruct:hDC, ::Parent:DrawItemStruct:rcItem, GetSysColorBrush( IIF( lselected, COLOR_HIGHLIGHT, COLOR_WINDOW )) )
            DrawText( ::Parent:DrawItemStruct:hDC, itemTxt, ::Parent:DrawItemStruct:rcItem, DT_LEFT | DT_VCENTER | DT_SINGLELINE )
            
            //ExtTextOut( ::Parent:DrawItemStruct:hDC, 10, y, ETO_OPAQUE + ETO_CLIPPED, ::Parent:DrawItemStruct:rcItem, itemTxt )
            SelectObject( ::Parent:DrawItemStruct:hDC, hOld )
            DeleteObject( hFont )
         ENDIF
      ENDIF
      IF ::Parent:DrawItemStruct:itemState & ODS_FOCUS != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_FOCUS != 0
         aClip := { ::Parent:DrawItemStruct:rcItem:Left,  ::Parent:DrawItemStruct:rcItem:Top, ;
                    ::Parent:DrawItemStruct:rcItem:Right, ::Parent:DrawItemStruct:rcItem:Bottom  }
         _DrawfocusRect( ::Parent:DrawItemStruct:hDC, aclip )
      ENDIF
   ENDIF
RETURN 0

//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------

CLASS ComboBoxEx INHERIT ComboBox
   PROPERTY ImageList GET __ChkComponent( Self, ::xImageList ) SET SetImageList

   DATA ClientEdge EXPORTED INIT .F.
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
   ::Style       := WS_CHILD | WS_VISIBLE | WS_TABSTOP | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ExStyle     := 0
RETURN Self

METHOD Create() CLASS ComboBoxEx
   LOCAL h
   ::ClientEdge  := .F.
   ::Super:Create()
   IF ::ImageList != NIL
      ::SetImageList( ::ImageList )
   ENDIF
   h := ::SendMessage( CBEM_GETCOMBOCONTROL, 0, 0 )
   SendMessage( h, CB_SETMINVISIBLE, ::xHeight/::xItemHeight )
   ::hControl := ::SendMessage( CBEM_GETCOMBOCONTROL, 0, 0 )
   IF ::__ClassInst != NIL
      ::BackColor := ::Parent:BackColor
   ENDIF
RETURN Self

METHOD AddItem( cText, nImage ) CLASS ComboBoxEx
   LOCAL hDC, hFont, n, nPos, cbei := (struct COMBOBOXEXITEM)
   DEFAULT nImage TO ::GetCount()+1
   cbei:mask           := CBEIF_TEXT | CBEIF_INDENT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE
   cbei:iItem          := -1
   cbei:pszText        := cText
   cbei:cchTextMax     := LEN( cbei:pszText )+1
   cbei:iImage         := nImage-1
   cbei:iSelectedImage := nImage-1
   cbei:iIndent        := 0
   SendMessage( ::hWnd, CBEM_INSERTITEM, 0, cbei )
RETURN Self

METHOD SetImageList( oList ) CLASS ComboBoxEx
   LOCAL hChild, h
   IF ::hWnd != NIL
      ::SendMessage( CBEM_SETIMAGELIST, 0, IIF( oList != NIL, oList:Handle, NIL ) )
      IF oList != NIL
         h := ::SendMessage( CBEM_GETCOMBOCONTROL, 0, 0 )
         ::xItemHeight := oList:IconHeight
         SendMessage( h, CB_SETMINVISIBLE, ::xHeight/::xItemHeight )
      ENDIF
   ENDIF
RETURN Self

METHOD OnSize( nwParam, x, y ) CLASS ComboBoxEx
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
ENDCLASS

METHOD Init( oParent ) CLASS FormComboBox
   LOCAL cDrives, n, nType, cType, shfi, cBuffer
   ::__xCtrlName := "FormComboBox"
   ::Super:Init( oParent )
RETURN Self

METHOD Create() CLASS FormComboBox
   ::Style  := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::hFont1 := __GetMessageFont( 700 )
   ::hFont2 := __GetMessageFont( 400 )
   Super:Create()
RETURN Self

METHOD SelectControl( oControl ) CLASS FormComboBox
   LOCAL n := ASCAN( ::aItems, oControl,,, .T. )//MAX( ::FindString(, oControl:Name ), 1 )
   ::SetCurSel(n)
RETURN Self

METHOD Reset( oControl ) CLASS FormComboBox
   LOCAL cCtrl, oForm, cText
   ::aItems := {}
   ::ResetContent()
   IF ::Application:Project:Properties != NIL
      ::AddItem( "Application" + CHR(9) + "Visual xHarbour" )
      AADD( ::aItems, ::Application:Project:AppObject )
      
      ::AddItem( ::Application:Project:Properties:Name + CHR(9) + "Project" )
      AADD( ::aItems, ::Application:Project:Properties )

      IF ( oForm := ::Application:Project:CurrentForm ) != NIL
         ::AddItem( oForm:Name + CHR(9) + IIF( !oForm:lCustom, "Forms", "Custom Controls" ) )
         AADD( ::aItems, ::Application:Project:CurrentForm )

         IF oForm:Property != NIL
            FOR EACH cCtrl IN oForm:Property:Keys
                cText := oForm:Property[ cCtrl ]:ClassName
                ::AddItem( cCtrl + CHR(9) + Upper( cText[1] ) + SubStr( Lower( cText ), 2 ) )
                AADD( ::aItems, oForm:Property[ cCtrl ] )
            NEXT
         ENDIF
      ENDIF
      ::Application:Yield()
      IF oControl != NIL
         ::SelectControl( oControl )
       ELSE
         ::SetCurSel(1)
      ENDIF
   ENDIF
RETURN Self


METHOD OnParentDrawItem( nwParam, nlParam ) CLASS FormComboBox
   LOCAL n, lSelected, nLen, itemTxt, cText, aSize, hDC, hOld
   IF ::Parent:DrawItemStruct != NIL .AND. ::Parent:DrawItemStruct:hwndItem == ::hWnd
      hDC := ::Parent:DrawItemStruct:hDC
      lSelected := ::Parent:DrawItemStruct:itemState & ODS_SELECTED != 0

      IF ::Parent:DrawItemStruct:itemAction & ODA_DRAWENTIRE != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_SELECT != 0
         SetTextColor( hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXTLEN, ::Parent:DrawItemStruct:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXT, ::Parent:DrawItemStruct:itemID, @itemTxt )

         itemTxt := Left( itemTxt, nLen )
         n := AT( chr(9), itemTxt )
         cText := SUBSTR( itemTxt, 1, n-1 )

         SelectObject( hDC, ::hFont1 )

         aSize := _GetTextExtentPoint32( hDC, cText )

         _ExtTextOut( hDC, 5, ::Parent:DrawItemStruct:rcItem:Top, ETO_OPAQUE + ETO_CLIPPED, ::Parent:DrawItemStruct:rcItem:Array, cText )

         hOld  := SelectObject( hDC, ::hFont2 )
         cText := SUBSTR( itemTxt, n+1 )
         _ExtTextOut( hDC, aSize[1]+15, ::Parent:DrawItemStruct:rcItem:Top, ETO_CLIPPED, ::Parent:DrawItemStruct:rcItem:Array, cText )
         SelectObject( hOld )
      ENDIF
      IF ::Parent:DrawItemStruct:itemState & ODS_FOCUS != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_FOCUS != 0
         _DrawfocusRect( hDC, ::Parent:DrawItemStruct:rcItem:Array )
      ENDIF
   ENDIF
RETURN 0

METHOD OnParentCommand( nId, nCode, nlParam ) CLASS FormComboBox
   LOCAL n, oSel, cStr, nSel, nRet := 0
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
