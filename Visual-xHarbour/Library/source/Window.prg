/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Window.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
static aDef := {"BMP","ICO"}

#include "vxh.ch"
#include "colors.ch"
#include "debug.ch"
#include "error.ch"
#include "uxTheme.ch"

#define CTYPE_BOOL                 9

#define HKEY_CLASSES_ROOT            (0x80000000)
#define HKEY_CURRENT_USER            (0x80000001)
#define HKEY_LOCAL_MACHINE           (0x80000002)
#define HKEY_USERS                   (0x80000003)
#define HKEY_PERFORMANCE_DATA        (0x80000004)
#define HKEY_CURRENT_CONFIG          (0x80000005)
#define HKEY_DYN_DATA                (0x80000006)

#define REG_OPTION_NON_VOLATILE       0x00000000L
#define REG_OPTION_VOLATILE           0x00000001L
#define REG_OPTION_CREATE_LINK        0x00000002L
#define REG_OPTION_BACKUP_RESTORE     0x00000004L
#define REG_OPTION_OPEN_LINK          0x00000008L

#define REG_CREATED_NEW_KEY           0x00000001L
#define REG_OPENED_EXISTING_KEY       0x00000002L

#define REG_NONE                         0
#define REG_SZ                           1
#define REG_EXPAND_SZ                    2
#define REG_BINARY                       3
#define REG_DWORD                        4
#define REG_DWORD_LITTLE_ENDIAN          4
#define REG_DWORD_BIG_ENDIAN             5
#define REG_LINK                         6
#define REG_MULTI_SZ                     7
#define REG_RESOURCE_LIST                8
#define REG_FULL_RESOURCE_DESCRIPTOR     9
#define REG_RESOURCE_REQUIREMENTS_LIST  10
#define REG_QWORD                       11
#define REG_QWORD_LITTLE_ENDIAN         11

#define REG_NOTIFY_CHANGE_NAME        0x00000001L
#define REG_NOTIFY_CHANGE_ATTRIBUTES  0x00000002L
#define REG_NOTIFY_CHANGE_LAST_SET    0x00000004L
#define REG_NOTIFY_CHANGE_SECURITY    0x00000008L

#define KEY_QUERY_VALUE               0x0001
#define KEY_SET_VALUE                 0x0002
#define KEY_CREATE_SUB_KEY            0x0004
#define KEY_ENUMERATE_SUB_KEYS        0x0008
#define KEY_NOTIFY                    0x0010
#define KEY_CREATE_LINK               0x0020



#Define WT_DIALOG     0
#Define WT_WINDOW     1
#Define WT_MDIFrame   2
#Define WT_MDICHILD   4

//#define CS_DROPSHADOW 0x00020000
#define CS_DROPSHADOW 131072

#define ABN_POSCHANGED        0x0000001
#define ABN_FULLSCREENAPP     0x0000002
#define ABM_WINDOWPOSCHANGED  0x0000009

#define WM_MDICHILDSIZED      4500
#define WM_CHILDDESTROYED     5501

#define ETDT_DISABLE        0x00000001
#define ETDT_ENABLE         0x00000002
#define ETDT_USETABTEXTURE  0x00000004
#define ETDT_ENABLETAB      (ETDT_ENABLE | ETDT_USETABTEXTURE)

#define ATL_IDM_FIRST_MDICHILD 50000
#define IDM_MDI_BASE      (ATL_IDM_FIRST_MDICHILD - 5)
#define IDM_MDI_ICON      (IDM_MDI_BASE + 0)
#define IDM_MDI_GAP       (IDM_MDI_BASE + 1)
#define IDM_MDI_MINIMIZE  (IDM_MDI_BASE + 2)
#define IDM_MDI_RESTORE   (IDM_MDI_BASE + 3)
#define IDM_MDI_CLOSE     (IDM_MDI_BASE + 4)
#define IDM_MDI_FIRST     IDM_MDI_ICON
#define IDM_MDI_LAST      IDM_MDI_CLOSE

#define TID_POLLMOUSE 100
#define MOUSE_POLL_DELAY 10
#define WHEEL_PAGESCROLL 00041

#define SC_RESTORE2  0xF122
#define SC_MAXIMIZE2 0xF032

#xtranslate NTRIM( < n > ) = > ALLTRIM( STR( < n > ) )

#xcommand ODEFAULT <v> TO <x> [, <vN> TO <xN>]         ;
       => IF <v> == nil .OR. VALTYPE( <v> ) == "O"; <v> := <x> ; END            ;
          [; IF <vN> == nil ; <vN> := <xN> ; END]

//-----------------------------------------------------------------------------------------------

CLASS Window INHERIT Object
   // IDE Published properties

   PROPERTY ContextMenu GET __ChkComponent( Self, ::xContextMenu )

   PROPERTY Left          INDEX 1                   READ xLeft          WRITE __SetSizePos
   PROPERTY Top           INDEX 2                   READ xTop           WRITE __SetSizePos
   PROPERTY Width         INDEX 3                   READ xWidth         WRITE __SetSizePos
   PROPERTY Height        INDEX 4                   READ xHeight        WRITE __SetSizePos
   PROPERTY Cursor                                  READ xCursor        WRITE __SetWindowCursor DEFAULT IDC_ARROW PROTECTED
   PROPERTY StaticEdge    INDEX WS_EX_STATICEDGE    READ xStaticEdge    WRITE SetExStyle        DEFAULT .F.       PROTECTED
   PROPERTY ClientEdge    INDEX WS_EX_CLIENTEDGE    READ xClientEdge    WRITE SetExStyle        DEFAULT .F.       PROTECTED
   PROPERTY ControlParent INDEX WS_EX_CONTROLPARENT READ xControlParent WRITE SetExStyle        DEFAULT .F.       PROTECTED

   PROPERTY Visible       INDEX WS_VISIBLE          READ xVisible       WRITE SetStyle          DEFAULT .T.       PROTECTED
   PROPERTY Enabled       INDEX WS_DISABLED         READ xEnabled       WRITE SetStyle          DEFAULT .T.       PROTECTED
   PROPERTY Border        INDEX WS_BORDER           READ xBorder        WRITE SetStyle          DEFAULT .F.       PROTECTED

   PROPERTY Center                                  READ xCenter        WRITE CenterWindow      DEFAULT .F.       PROTECTED
   PROPERTY ClipChildren  INDEX WS_CLIPCHILDREN     READ xClipChildren  WRITE SetStyle          DEFAULT .T.       PROTECTED
   PROPERTY ClipSiblings  INDEX WS_CLIPSIBLINGS     READ xClipSiblings  WRITE SetStyle          DEFAULT .T.       PROTECTED
   PROPERTY TabOrder                                READ xTabOrder      WRITE SetTabOrder                         INVERT
   PROPERTY AcceptFiles   INDEX WS_EX_ACCEPTFILES   READ xAcceptFiles   WRITE SetExStyle        DEFAULT .F.       PROTECTED
   PROPERTY AnimationStyle                          READ xAnimationStyle                        DEFAULT 0
   PROPERTY NoActivate    INDEX WS_EX_NOACTIVATE    READ xNoActivate    WRITE SetExStyle        DEFAULT .F.       PROTECTED


//   DATA xText                  EXPORTED  INIT ""
//   ACCESS Text                 INLINE    IIF( ! ::IsWindow() .OR. ::__IsInstance, ::xText, _GetWindowText( ::hWnd ) ) PERSISTENT
//   ASSIGN Text(c)              INLINE    ::SetWindowText( c )


   DATA xCaption               EXPORTED  INIT ""
   ACCESS Caption              INLINE    IIF( ! ::IsWindow() .OR. ::__IsInstance, ::xCaption, _GetWindowText( ::hWnd ) ) PERSISTENT
   ASSIGN Caption(c)           INLINE    ::SetWindowText( c )


   DATA Font                   PUBLISHED
   DATA ToolTip                PUBLISHED

   DATA xAnimation             EXPORTED
   ASSIGN Animation(o)         INLINE    ::xAnimation := o
   ACCESS Animation            INLINE    IIF( ::xAnimation == NIL, ::xAnimation := __Animation( Self ),), ::xAnimation //PERSISTENT

   DATA ClientWidth            EXPORTED  INIT 0
   DATA ClientHeight           EXPORTED  INIT 0

   DATA DisableParent          PUBLISHED INIT .F.
   DATA AutoClose              EXPORTED INIT .T.
   DATA VertScroll             EXPORTED INIT .F.
   DATA HorzScroll             EXPORTED INIT .F.

   DATA OriginalRect           EXPORTED
   DATA Msg                    EXPORTED
   DATA wParam                 EXPORTED
   DATA lParam                 EXPORTED

   DATA ShowMode               EXPORTED  INIT 1
   DATA Id                     EXPORTED
   DATA Drawing                EXPORTED
   DATA KeepParentActive       EXPORTED  INIT .F.
   DATA KeepActive             EXPORTED  INIT .F.

   DATA StatusBar              EXPORTED

   DATA WindowPos              EXPORTED
   DATA DrawItemStruct         EXPORTED
   DATA MeasureItemStruct      EXPORTED
   DATA hdr                    EXPORTED
   DATA ScrollInfo             EXPORTED

   DATA LeftSplitter           EXPORTED
   DATA TopSplitter            EXPORTED
   DATA RightSplitter          EXPORTED
   DATA BottomSplitter         EXPORTED

   DATA AutoVertScroll         EXPORTED  INIT .F.
   DATA AutoHorzScroll         EXPORTED  INIT .F.
   DATA BkBrush                EXPORTED
   DATA SelBkBrush             EXPORTED

   DATA MinLeft                EXPORTED  INIT 0
   DATA MinTop                 EXPORTED  INIT 0

   DATA MinWidth               EXPORTED  INIT 0
   DATA MinHeight              EXPORTED  INIT 0
   DATA MaxWidth               EXPORTED  INIT 0
   DATA MaxHeight              EXPORTED  INIT 0

   DATA SetChildren            EXPORTED  INIT .T.

   DATA ClassBrush             EXPORTED  INIT COLOR_BTNFACE+1
   DATA Style                  EXPORTED
   DATA ExStyle                EXPORTED  INIT 0
   DATA ClassStyle             EXPORTED  INIT CS_OWNDC | CS_DBLCLKS | CS_SAVEBITS

   DATA TopMargin              EXPORTED  INIT 0
   DATA RightMargin            EXPORTED  INIT 0
   DATA BottomMargin           EXPORTED  INIT 0
   DATA LeftMargin             EXPORTED  INIT 0

   DATA Active                 EXPORTED  INIT .F.
   DATA DeferRedraw            EXPORTED  INIT .T.
   DATA HelpId                 EXPORTED
   DATA SmallCaptionFont       EXPORTED
   DATA IsContainer            EXPORTED  INIT .T.
   DATA TabValidate            EXPORTED  INIT .T.

   // Private Properties
   DATA MDIClient                PROTECTED
   DATA __Docked                 PROTECTED INIT .T.
   DATA __aCltRect               PROTECTED
   DATA __hCursor                EXPORTED

   DATA __MenuBar                EXPORTED  INIT .F.
   DATA __Splitting              EXPORTED  INIT .F.
   DATA __lPopTip                EXPORTED  INIT .F.
   DATA __lReqBrush              PROTECTED INIT .F.
   DATA __lShown                 PROTECTED INIT .F.
   DATA __KeyPressed             EXPORTED  INIT 0
   DATA __CurrentPos             EXPORTED  INIT 1
   DATA __Timers                 EXPORTED  INIT 1
   DATA __lAllowCopy             EXPORTED  INIT .T.
   DATA __lInitialized           EXPORTED  INIT .F.
   DATA __IdeContextMenuItems    EXPORTED  INIT {}
   DATA __nProc                  EXPORTED
   DATA __lOnPaint               EXPORTED  INIT .F.
   DATA __hAccelTable            EXPORTED
   DATA __lMouseHover            EXPORTED INIT .F.
   DATA __hMemBitmap             EXPORTED
   DATA __IsInstance             EXPORTED  INIT .F.
   DATA __pCallBackPtr           EXPORTED
   DATA __lCreateAfterChildren   EXPORTED  INIT .F.
   DATA __MDIFrame               EXPORTED
   DATA __lResizeable            EXPORTED  INIT {.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.}
   DATA __lMoveable              EXPORTED  INIT .T.
   DATA __Registered             EXPORTED  INIT .F.
   DATA __Accelerators           EXPORTED  INIT {}
   DATA __InstMsg                EXPORTED
   DATA __ClientStruct           EXPORTED
   DATA __IsMemDC                EXPORTED  INIT .F.
   DATA __PrevRect               EXPORTED
   DATA __IdeImageIndex          EXPORTED INIT 0
   DATA __TempRect               EXPORTED
   DATA __hParent                EXPORTED
   DATA __lKeyDown               EXPORTED INIT .F.   
   DATA __PropFilter             EXPORTED INIT {}
   DATA __Cursors                EXPORTED INIT { "Arrow"       ,;
                                                 "Help"        ,;
                                                 "Working"     ,;
                                                 "Busy"        ,;
                                                 "Cross"       ,;
                                                 "TextSelect"  ,;
                                                 "Unavailable" ,;
                                                 "SizeNS"      ,;
                                                 "SizeWE"      ,;
                                                 "SizeNESW"    ,;
                                                 "SizeNWSE"    ,;
                                                 "SizeAll"     ,;
                                                 "UpArrow"    ,;
                                                 "LinkSelect"  }

   DATA __CursorValues           EXPORTED INIT { IDC_ARROW        ,;
                                                 IDC_HELP         ,;
                                                 IDC_APPSTARTING  ,;
                                                 IDC_WAIT         ,;
                                                 IDC_CROSS        ,;
                                                 IDC_IBEAM        ,;
                                                 IDC_NO           ,;
                                                 IDC_SIZENS       ,;
                                                 IDC_SIZEWE       ,;
                                                 IDC_SIZENESW     ,;
                                                 IDC_SIZENWSE     ,;
                                                 IDC_SIZEALL      ,;
                                                 IDC_UPARROW      ,;
                                                 IDC_HAND          }

   DATA Hidden                 EXPORTED  INIT .F.
   ACCESS OsVer                INLINE    IIF( ::Application != NIL, ::Application:OsVersion, __GetOsVersion() )

   DATA xAlignment             EXPORTED  INIT 1
   ACCESS Alignment            INLINE    ::xAlignment
   ASSIGN Alignment(n)         INLINE    ::SetAlignment(n)

   DATA FilesDroped            EXPORTED INIT {}
   DATA VertScrollPos          EXPORTED INIT 0
   DATA HorzScrollPos          EXPORTED INIT 0
   DATA Anchor                 EXPORTED
   DATA Modal        AS LOGIC  EXPORTED INIT .F.
   DATA Flat                   EXPORTED INIT .F.
   DATA AutoDock               EXPORTED INIT .T.
   DATA Dock                   EXPORTED AS OBJECT
   DATA HelpInfo               EXPORTED

   ACCESS Handle               INLINE ::hWnd
   ACCESS HasFocus             INLINE GetFocus() == ::hWnd
   ACCESS IsChild              INLINE ::Style & WS_CHILD != 0

   ACCESS Child                INLINE ::Style & WS_CHILD != 0 //PERSISTENT
   ASSIGN Child(l)             INLINE ::SetStyle( WS_CHILD, l )

   DATA BackSysColor           EXPORTED INIT GetSysColor( COLOR_BTNFACE )
   DATA ForeSysColor           EXPORTED INIT GetSysColor( COLOR_BTNTEXT )

   DATA xBackColor             EXPORTED
   ACCESS BackColor            INLINE IIF( ::xBackColor == NIL, ::BackSysColor, ::xBackColor ) PERSISTENT
   ASSIGN BackColor( n )       INLINE ::xBackColor := n, ::SetBackColor( n )

   DATA xForeColor             EXPORTED
   ACCESS ForeColor            INLINE IIF( ::xForeColor == NIL, ::ForeSysColor, ::xForeColor ) PERSISTENT
   ASSIGN ForeColor( n )       INLINE ::xForeColor := n, ::SetForeColor( n ) //IIF( ::IsWindow() .AND. ::IsWindowVisible(), ::InvalidateRect(), NIL )

   DATA xMDIChild              EXPORTED INIT .F.
   DATA xMdiContainer          EXPORTED INIT .F.

   DATA aPrevSize              EXPORTED

   DATA OnWMSysCommand         EXPORTED
   DATA OnWMSize               EXPORTED
   DATA OnWMShowWindow         EXPORTED
   DATA OnWMSysColorChange     EXPORTED
   DATA OnWMThemeChanged       EXPORTED
   DATA OnWMSetFocus           EXPORTED
   DATA OnWMKillFocus          EXPORTED
   DATA OnWMKeyDown            EXPORTED
   DATA OnWMKeyUp              EXPORTED
   DATA OnWMLButtonUp          EXPORTED
   DATA OnWMLButtonDown        EXPORTED
   DATA OnWMChar               EXPORTED
   DATA OnWMInitDialog         EXPORTED
   DATA OnWMPaste              EXPORTED
   DATA OnWMGetDlgCode         EXPORTED
   DATA OnWMDestroy            EXPORTED
   DATA OnWMClose              EXPORTED
   DATA OnWMLButtonDblClk      EXPORTED

   DATA Siv                    PROTECTED
   DATA Sih                    PROTECTED
   DATA BitmapMask             PROTECTED
   DATA BitmapMaskColor        PROTECTED
   DATA __TaskBarParent        PROTECTED
   DATA __WndProc              PROTECTED INIT "__ControlProc"
   DATA __HideResized          PROTECTED INIT .F.
   DATA __WindowStyle          PROTECTED INIT WT_WINDOW
   DATA __lOnWindowPaint       PROTECTED INIT .F.
   DATA __ClientRect           PROTECTED
   DATA __WidthPerc            PROTECTED
   DATA __HeightPerc           PROTECTED
   DATA __hRegion              PROTECTED
   DATA __hBmpRgn              PROTECTED
   DATA __lNCMouseHover        PROTECTED INIT .F.
   DATA __IsForm               PROTECTED INIT .F.
   DATA __ArrayPointer         PROTECTED
   DATA __lSubClass            EXPORTED  INIT .T.
   ACCESS AppInstance          INLINE IIF( ::Form:DllInstance != NIL, ::Form:DllInstance, ::Application:Instance )


   METHOD Init( oParent ) CONSTRUCTOR
   METHOD Create()

   //DESTRUCTOR __WinDestruct

   METHOD __Register()
   METHOD __ControlProc()
   METHOD __CreateMDI()
   METHOD __OnParentSize()
   METHOD __ResetHandle()       INLINE ::hWnd := NIL, ::__nProc := NIL
   METHOD __WindowDestroy()
   METHOD __WinProc(hWnd, nMsg, nwParam, nlParam) INLINE DefWindowProc(hWnd, nMsg, nwParam, nlParam )
   METHOD __SetScrollBars()
   METHOD __SubClass()
   METHOD __UnSubClass()
   METHOD __GetShowMode()
   METHOD __SetFrameStyle()
   METHOD __SetWindowCursor()
   METHOD __SetSizePos()
   METHOD __GetBrush()         VIRTUAL
   METHOD __PaintBakgndImage() VIRTUAL

   METHOD EnableThemeDialogTexture( nFlags ) INLINE EnableThemeDialogTexture( ::hWnd, nFlags )
   METHOD SetInvStyle( n, l )     INLINE ::SetStyle( n, !l )
   METHOD DragAcceptFiles(l)      INLINE IIF( ::hWnd != NIL, DragAcceptFiles( ::hWnd, l ), NIL )

   METHOD CenterWindow()
   METHOD Animate()

   METHOD Destroy()
   METHOD Disable()               INLINE ::Enabled := .F.
   METHOD Enable()                INLINE ::Enabled := .T.
   METHOD Close()

   METHOD Hide()
   METHOD Show()
   METHOD MessageWait()
   METHOD MessageBox( cText, cCaption, nFlags ) INLINE MessageBox( ::hWnd, XSTR( cText ), XSTR( cCaption ), nFlags )
   METHOD IsCovered()
   METHOD LockWindowUpdate()      INLINE LockWindowUpdate( ::hWnd )
   METHOD UnlockWindowUpdate()    INLINE LockWindowUpdate()

   METHOD HideCaret()             INLINE HideCaret( ::hWnd )
   METHOD ShowCaret()             INLINE ShowCaret( ::hWnd )
   METHOD DestroyCaret()          INLINE DestroyCaret()
   METHOD CreateCaret( hBmp, nWidth, nHeight )  INLINE CreateCaret( ::hWnd, hBmp, nWidth, nHeight )
   METHOD SetCapture()            INLINE SetCapture( ::hWnd )
   METHOD GetCapture()            INLINE GetCapture() == ::hWnd
   METHOD ReleaseCapture()        INLINE ReleaseCapture()
   METHOD OpenClipboard()         INLINE OpenClipboard( ::hWnd )
   METHOD IsWindow()              INLINE IsWindow( ::hWnd )
   METHOD IsWindowVisible()       INLINE IsWindowVisible( ::hWnd )
   METHOD SetFocus()              INLINE SetFocus( ::hWnd),self
   METHOD GetWindowRect()
   METHOD SetParent()
   METHOD ScrollWindow( x, y, rRect, rClip ) INLINE _ScrollWindow( ::hWnd, x, y, rRect, rClip )
   METHOD SetActiveWindow()       INLINE SetActiveWindow( ::hWnd )
   METHOD GetClientRect()
   METHOD SetWindowText(cText)    INLINE ::xCaption := cText, IIF( ::hWnd != NIL, SetWindowText( ::hWnd, cText ),)
   METHOD BringWindowToTop()      INLINE BringWindowToTop( ::hWnd )
   METHOD ScreenToClient( pt )    INLINE ScreenToClient( ::hWnd, @pt )
   METHOD ClientToScreen( pt )    INLINE ClientToScreen( ::hWnd, @pt )
   METHOD SetWindowPos( hAfter, x, y, w, h , n)             INLINE SetWindowPos( ::hWnd, hAfter, x, y, w, h , n )
   METHOD DeferWindowPos( hDef, hAfter, x, y, w, h , n)     INLINE DeferWindowPos( hDef, ::hWnd, hAfter, x, y, w, h , n )
   METHOD SendMessage( nMsg, nwParam, nlParam )             INLINE SendMessage( ::hWnd, nMsg, nwParam, nlParam )
   METHOD SendDlgItemMessage( nId, nMsg, nwParam, nlParam ) INLINE SendDlgItemMessage( ::hWnd, nId, nMsg, nwParam, nlParam )
   METHOD PostMessage( nMsg, nwParam, nlParam )             INLINE _PostMessage( ::hWnd, nMsg, nwParam, nlParam )
   METHOD GetWindowLong(n)        INLINE GetWindowLong( ::hWnd, n )
   METHOD SetWindowLong(n,nSt)    INLINE SetWindowLong( ::hWnd, n, nSt )
   METHOD RedrawWindow( rc, hRgn, nFlags ) INLINE _RedrawWindow( ::hWnd, rc, hRgn, nFlags )
   METHOD TrackMouseEvent()
   METHOD DefWindowProc( nMsg, nwParam, nlParam ) INLINE DefWindowProc( ::hWnd, nMsg, nwParam, nlParam )
   METHOD GetWindowTextLength()   INLINE GetWindowTextLength( ::hWnd )
   METHOD GetWindowPlacement()
   METHOD GetWindowInfo()
   METHOD GetFont()               INLINE ::SendMessage( WM_GETFONT, 0, 0 )
   METHOD GetIcon( nIcon )        INLINE ::SendMessage( WM_GETICON, nIcon, 0 )
   METHOD SetIcon( nIcon, hIcon ) INLINE ::SendMessage( WM_SETICON, nIcon, hIcon )
   METHOD SetStyle()
   METHOD GetStyle(n)             INLINE IIF( ::IsWindow(), ( ::GetWindowLong( GWL_STYLE ) & n ) == n, ::Style & n == n )
   METHOD SetExStyle()

   METHOD SetRedraw(lRed)         INLINE SendMessage( ::hWnd, WM_SETREDRAW,IIF(lRed,1,0),0)
   METHOD IsWindowEnabled()       INLINE IsWindowEnabled( ::hWnd )
   METHOD SetBackColor()
   METHOD SetForeColor()
   METHOD ValidateRect( rc )      INLINE _ValidateRect( ::hWnd, rc )
   METHOD InvalidateRect( rc, lErase )      INLINE IIF( ::hWnd != NIL, _InvalidateRect( ::hWnd, rc, lErase ),)
   METHOD MapWindowPoints( hWndDest, pt, nPoint ) INLINE MapWindowPoints( ::hWnd, hWndDest, @pt, nPoint )
   METHOD MoveWindow()

   METHOD GetRectangle()          INLINE { ::Left, ::Top, ::Left + ::Width, ::Top + ::Height }
   METHOD GetRect()

   METHOD CallWindowProc()        INLINE CallWindowProc( ::__nProc, ::hWnd, ::Msg, ::wParam, ::lParam )
   METHOD UpdateWindow()          INLINE UpdateWindow( ::hWnd ), Self

   METHOD Refresh()
   METHOD ReCreate()
   METHOD GetHeight()             INLINE ::xHeight
   METHOD IsChildren()
   METHOD DockIt()
   METHOD UpdateLayout()          INLINE ::PostMessage( WM_SIZE, 0, MAKELONG( ::ClientWidth, ::ClientHeight ) )
   METHOD SetTabOrder()

   //METHOD DefControlProc()

   METHOD DockToParent()          INLINE ::Dock:Left   := ::Parent,;
                                         ::Dock:Top    := ::Parent,;
                                         ::Dock:Right  := ::Parent,;
                                         ::Dock:Bottom := ::Parent,;
                                         ::DockIt()

   METHOD HandleEvent( cEvent )   INLINE ExecuteEvent( cEvent, Self )

   METHOD OnParentSize()        VIRTUAL
   METHOD OnNCDestroy()         VIRTUAL
   METHOD OnChildChar()         VIRTUAL
   METHOD OnChildGetDlgCode()   VIRTUAL
   METHOD OnChildKeyDown()      VIRTUAL
   METHOD OnChar()              VIRTUAL
   METHOD OnSysChar()           VIRTUAL
   METHOD OnClose()             VIRTUAL
   METHOD OnCommand()           VIRTUAL
   METHOD OnGetMinMaxInfo()     VIRTUAL
   METHOD OnHotKey()            VIRTUAL
   METHOD OnWindowPaint()       VIRTUAL
   METHOD OnPaint()             VIRTUAL
   METHOD OnDestroy()           VIRTUAL
   METHOD OnParentDrawItem()    VIRTUAL
   METHOD OnParentMeasureItem() VIRTUAL
   METHOD OnEraseBkGnd()        VIRTUAL
   METHOD OnGetDlgCode()        VIRTUAL
   METHOD OnKeyDown()           VIRTUAL
   METHOD OnKeyUp()             VIRTUAL
   METHOD OnInitMenuPopup()     VIRTUAL
   METHOD OnKillFocus()         VIRTUAL
   METHOD OnLButtonDown()       VIRTUAL
   METHOD OnLButtonUp()         VIRTUAL
   METHOD OnLButtonDblClk()     VIRTUAL
   METHOD OnMButtonDown()       VIRTUAL
   METHOD OnMButtonUp()         VIRTUAL
   METHOD OnMenuSelect()        VIRTUAL
   METHOD OnMouseMove()         VIRTUAL
   METHOD OnMove()              VIRTUAL
   METHOD OnMoving()            VIRTUAL
   METHOD OnNcMouseMove()       VIRTUAL
   METHOD OnRButtonDown()       VIRTUAL
   METHOD OnRButtonUp()         VIRTUAL
   METHOD OnSetFocus()          VIRTUAL
   METHOD OnSize()              VIRTUAL
   METHOD OnSysCommand()        VIRTUAL
   METHOD OnTimer()             VIRTUAL
   METHOD OnCtlColorListBox()   VIRTUAL
   METHOD OnCtlColorDlg()       VIRTUAL
   METHOD OnCtlColorEdit()      VIRTUAL
   METHOD OnCtlColorStatic()    VIRTUAL
   METHOD OnCtlColorBtn()       VIRTUAL
   METHOD OnCtlColorScrollBar() VIRTUAL
   METHOD OnNotify()            VIRTUAL
   METHOD OnCreate()            VIRTUAL
   METHOD OnInitDialog()        VIRTUAL
   METHOD OnActivate()          VIRTUAL
   METHOD OnUserMsg()           VIRTUAL
   METHOD OnMessage()           VIRTUAL
   METHOD OnSetFont()           VIRTUAL
   METHOD OnNCCreate()          VIRTUAL
   METHOD OnNCActivate()        VIRTUAL
   METHOD OnNCHitTest()         VIRTUAL
   METHOD OnNCPaint()           VIRTUAL
   METHOD OnWindowPosChanged()  VIRTUAL
   METHOD OnWindowPosChanging() VIRTUAL
   METHOD OnSysColorChange()    VIRTUAL
   METHOD OnSysKeyDown()        VIRTUAL
   METHOD OnSysKeyUp()          VIRTUAL
   METHOD OnSizing()            VIRTUAL
   METHOD OnShowWindow()        VIRTUAL
   METHOD OnLoad()              VIRTUAL
   METHOD OnHideWindow()        VIRTUAL
   METHOD OnSetCursor()         VIRTUAL
   METHOD OnEnable()            VIRTUAL
   METHOD OnContextMenu()       VIRTUAL
   METHOD OnDropFiles()         VIRTUAL
   METHOD OnThemeChanged()      VIRTUAL
   METHOD OnSetText()           VIRTUAL
   METHOD OnNCCalcSize()        VIRTUAL
   METHOD OnCancelMode()        VIRTUAL
   METHOD OnMouseHover()        VIRTUAL
   METHOD OnMouseleave()        VIRTUAL
   METHOD OnNCMouseHover()      VIRTUAL
   METHOD OnNCMouseleave()      VIRTUAL
   METHOD OnNCLButtonUp()       VIRTUAL
   METHOD OnNCLButtonDown()     VIRTUAL
   METHOD OnNCLButtonDblClk()   VIRTUAL
   METHOD OnNCRButtonUp()       VIRTUAL
   METHOD OnNCRButtonDown()     VIRTUAL
   METHOD OnNCRButtonDblClk()   VIRTUAL
   METHOD OnNCMButtonUp()       VIRTUAL
   METHOD OnNCMButtonDown()     VIRTUAL
   METHOD OnNCMButtonDblClk()   VIRTUAL
   METHOD OnNCXButtonUp()       VIRTUAL
   METHOD OnNCXButtonDown()     VIRTUAL
   METHOD OnNCXButtonDblClk()   VIRTUAL
   METHOD OnMenuCommand()       VIRTUAL
   METHOD OnNextMenu()          VIRTUAL
   METHOD OnMenuChar()          VIRTUAL
   METHOD OnEnterMenuLoop()     VIRTUAL
   METHOD OnExitMenuLoop()      VIRTUAL
   METHOD OnDrawClipboard()     VIRTUAL
   METHOD OnChangeCbChain()     VIRTUAL
   METHOD OnParentMove()        VIRTUAL
   METHOD OnParentSysCommand()  VIRTUAL
   METHOD OnParentCommand()     VIRTUAL
   METHOD OnParentNotify()      VIRTUAL
   METHOD OnOk()                VIRTUAL
   METHOD OnCancel()            VIRTUAL
   METHOD OnToolTipNotify()     VIRTUAL
   METHOD OnPaste()             VIRTUAL
   METHOD OnCopy()              VIRTUAL
   METHOD OnCut()               VIRTUAL
   METHOD OnClear()             VIRTUAL
   METHOD OnUndo()              VIRTUAL
   METHOD OnPrint()             VIRTUAL
   METHOD OnPrintClient()       VIRTUAL
   METHOD OnMouseActivate()     VIRTUAL
   METHOD OnClick()             VIRTUAL
   METHOD OnVertScroll()        VIRTUAL
   METHOD OnHorzScroll()        VIRTUAL
   METHOD OnEnterSizeMove()     VIRTUAL
   METHOD OnExitSizeMove()      VIRTUAL
   METHOD OnGetText()           VIRTUAL
   METHOD OnMouseWheel()        VIRTUAL

   METHOD OnHScroll()
   METHOD OnVScroll()

   METHOD OnMeasureItem()
   METHOD OnDrawItem()

   METHOD GetChildFromPoint()
   METHOD SetTimer(nId,nElapse,hProc)               INLINE SetTimer( ::hWnd, nId, nElapse,hProc )
   METHOD KillTimer(nId)                            INLINE KillTimer( ::hWnd, nId )
   METHOD OpenThemeData()                           INLINE IIF( ::hTheme == NIL, ::hTheme := OpenThemeData( ::hWnd, ToUnicode( ::ThemeName ) ), )
   METHOD CloseThemeData()                          INLINE CloseThemeData( ::hTheme ), ::hTheme := NIL
   METHOD AddAccelerator()
   METHOD UpdateChildren()                          INLINE AEVAL( ::Children, {|o| IIF( VALTYPE(o)=="O",o:Redraw(),)} ), Self

   // Backward compatibility
   METHOD Disable() INLINE ::Enabled := .F.
   METHOD Enable()  INLINE ::Enabled := .T.

   METHOD GetCCTL()
   METHOD __FixDocking()
   METHOD SaveLayout()
   METHOD RestoreLayout()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD SaveLayout( cIniFile, cSection ) CLASS Window
   LOCAL oIni
   IF EMPTY( cIniFile )
      oIni := ::Application:IniFile
    ELSE
      oIni := IniFile( cIniFile )
   ENDIF
   DEFAULT cSection TO "Forms"
   oIni:WriteString( cSection, ::Application:Name + "_" + ::Name, xSTR( ::Left ) + ", " + xSTR( ::Top ) + ", " + xSTR( ::Width ) + ", " + xSTR( ::Height ) )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD RestoreLayout( cIniFile, cSection, lAllowOut ) CLASS Window
   LOCAL c, oIni, aRect, aPos, hMonitor
   DEFAULT lAllowOut TO .T.
   IF EMPTY( cIniFile )
      oIni := ::Application:IniFile
    ELSE
      oIni := IniFile( cIniFile )
   ENDIF
   c := oIni:ReadString( cSection, ::Application:Name + "_" + ::Name, "" )
   IF !EMPTY(c)
      aPos := {&c}
      DEFAULT aPos[1] TO 0
      DEFAULT aPos[2] TO 0
      DEFAULT aPos[3] TO 0
      DEFAULT aPos[4] TO 0
      ::xLeft   := aPos[1]
      ::xTop    := aPos[2]
      ::xWidth  := aPos[3]
      ::xHeight := aPos[4]

      ::MoveWindow()
   ENDIF
RETURN Self


//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Window

   LOCAL n, aApp, aBeh, aEve, aSiz, Topic
   DEFAULT ::__lInitialized  TO .F.
   DEFAULT ::DisableParent TO .F.
   IF ::__lInitialized
      MessageBox( GetActiveWindow(), ::Name+" has already been initialized returning previous instance." + CHR(13)+;
                                                "Use the prevously created instance to avoid this Message", ::Name, MB_ICONEXCLAMATION )
      RETURN Self
   ENDIF
   ::__lInitialized := .T.
   ::hdr               := {=>}
   ::WindowPos         := {=>}
   //::MeasureItemStruct := {=>}

   DEFAULT ::ThemeName    TO "window"
   DEFAULT ::Style        TO WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   DEFAULT ::xLeft        TO 0
   DEFAULT ::xTop         TO 0
   DEFAULT ::xWidth       TO CW_USEDEFAULT
   DEFAULT ::xHeight      TO CW_USEDEFAULT
   DEFAULT ::__xCtrlName  TO "Window"
   DEFAULT ::ClsName      TO "Window"

   ::Parent       := oParent

   IF ::__ClassInst == NIL .AND. ::Parent != NIL .AND. ::Parent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      ::__ClassInst:__IsInstance := .T.
   ENDIF

   ::Font := Font()
   ::Font:Parent := Self
   IF ::__ClassInst != NIL
      ::Font:__ClassInst := __ClsInst( ::Font:ClassH )
      ::Font:__ClassInst:__IsInstance := .T.
   ENDIF

   ::Font:Create()

   IF ! ( ::ClsName == TOOLTIPS_CLASS )
      ::Drawing := Drawing( Self )
      ::ToolTip := ToolTip( Self )
      IF VALTYPE( oParent ) == "O"
         ::__CreateProperty()
         DEFAULT ::Dock   TO __WindowDock( Self )
         DEFAULT ::Anchor TO __AnchorSet( Self )
         ::xTabOrder    := LEN( ::Parent:Children )+1
         IF ::__ClassInst != NIL
            ::__ClassInst:xTabOrder := ::xTabOrder
         ENDIF
      ENDIF
   ENDIF

   DEFAULT ::EventHandler TO Hash()

   IF ::Application != NIL .AND. ::Application:IdeActive //.AND. ::__ClassInst != NIL
      DEFAULT ::Events TO aEvents()
      aSort( ::Events,,,{|x, y| x[1] < y[1]})
      FOR EACH Topic IN ::Events
          aSort( Topic[2],,,{|x, y| x[1] < y[1]})
      NEXT
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------
/*
PROCEDURE __WinDestruct CLASS Window
   LOCAL pCallBack
   IF ::__pCallBackPtr != NIL
      FreeCallBackPointer( ::__pCallBackPtr )
      ::__pCallBackPtr := NIL
   ENDIF
   HB_GCALL(.T.)
RETURN
*/

//-----------------------------------------------------------------------------------------------

METHOD GetCCTL() CLASS Window
   LOCAL oParent := ::Parent
   WHILE oParent:ClsName != "CCTL"
      oParent := oParent:Parent
   ENDDO
RETURN oParent

METHOD __Register( cClass ) CLASS Window

   LOCAL wcex, lRet, hBrush, aSize, pCallBack

   IF cClass == NIL

      IF ::xMdiContainer
         ::__WindowStyle := WT_MDIFrame
      ENDIF

      wcex := (struct WNDCLASSEX)
      wcex:cbSize         := wcex:SizeOf()
      wcex:style          := ::ClassStyle
      wcex:hInstance      := ::Instance

      wcex:hbrBackground  := ::ClassBrush
      wcex:lpszClassName  := ::ClsName
      wcex:hCursor        := IIF( ::__hCursor != NIL, ::__hCursor, LoadCursor(, IDC_ARROW ) )

      IF ::ClsName == "MDIChild"
         wcex:lpfnWndProc  := DefMDIChildProcAddress()
       ELSE
         wcex:lpfnWndProc  := DefWindowProcAddress()
      ENDIF

      ::__Registered := RegisterClassEx( wcex ) > 0
      IF !::__Registered
         MessageBox( 0, "Failed to register class "+::ClsName )
         IF ::Application:MainForm == Self
            PostQuitMessage(0)
         ENDIF
      ENDIF
      RETURN ::__Registered
   ENDIF
   ::__Registered := .F.
RETURN(.T.)

//-----------------------------------------------------------------------------------------------
METHOD Refresh( lPaint ) CLASS Window
   LOCAL Child
   ::InvalidateRect(,lPaint)
   FOR EACH Child IN ::Children
       IF VALTYPE( Child ) == "O"
          Child:Refresh( lPaint )
       ENDIF
   NEXT
RETURN Self

METHOD ReCreate() CLASS Window
   LOCAL Child
   ::hWnd := NIL
   ::Create()
   FOR EACH Child IN ::Children
       Child:ReCreate()
   NEXT
RETURN Self

METHOD __SetSizePos( nPos, nVal ) CLASS Window
   DEFAULT nVal TO 0
   IF nPos > 2 .AND. nVal < 0
      nVal := 0
   ENDIF
   IF ::__ClassInst == NIL .AND. ::hWnd != NIL .AND. ::Parent == NIL
      ::GetWindowRect()
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
         ::xHeight := nVal
         EXIT
   END

   IF ( ::__ClassInst == NIL .OR. ::__CustomOwner .OR. ::__xCtrlName == "Expando" ) .AND. ::hWnd != NIL
      ::MoveWindow()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Close() CLASS Window
   LOCAL nRet := SendMessage( ::hWnd, WM_SYSCOMMAND, SC_CLOSE )
   IF nRet <> 0
      IF ::Application != NIL .AND. ::Application:MainForm != NIL .AND. ::Application:MainForm:hWnd == ::hWnd
         ::Application:Exit()
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD Create( oParent ) CLASS Window
   LOCAL hParent, nLeft, nTop, hDef, oChild, x, y, hBmp, hRgn, cClass, wc, n, aRect
   LOCAL oObj, nSeq, err, hIcon, nError, cError, aSize, oControl, oTop, oBottom, oLeft
   LOCAL cBmpMask

   IF ! ::MDIContainer
      ::MDIClient := NIL
   ENDIF
   
   IF ::__ClassInst != NIL .AND. ::__CustomOwner .AND. ::hWnd != NIL
      RETURN Self
   ENDIF

   IF ::__ClassInst != NIL .AND. ( ::__CustomOwner .OR. UPPER( ::ClassName ) == "CUSTOMCONTROL" )

      __ResetClassInst( Self )

      IF ::BackSysColor != ::xBackColor .AND. ::xBackColor != NIL
         ::BackSysColor := ::xBackColor
         ::__ForceSysColor := .T.
      ENDIF
      IF ::ForeSysColor != ::xForeColor .AND. ::xForeColor != NIL
         ::ForeSysColor := ::xForeColor
         ::__ForceSysColor := .T.
      ENDIF

      IF ::__CustomOwner
         ::__OriginalName := ::Name
      ENDIF
   ENDIF



   IF ::hWnd != NIL
      RETURN Self
   ENDIF
   ::Super:Create()

   IF ::__OnInitCanceled
      RETURN NIL
   ENDIF

   ::__lOnPaint   := __ClsMsgAssigned( Self, "OnPaint" ) .OR. HGetPos( ::EventHandler, "OnPaint" ) != 0
   ::__lOnWindowPaint := __ClsMsgAssigned( Self, "OnWindowPaint" ) .OR. HGetPos( ::EventHandler, "OnWindowPaint" ) != 0
   IF VALTYPE( oParent ) == "N"
      hParent := oParent

    ELSEIF VALTYPE( oParent ) == "O"
      hParent := oParent:hWnd

    ELSE
      IF ::Parent == NIL
         hParent :=  IIF( ::Application:MainForm == Self, NIL, GetActiveWindow() )
       ELSE
         hParent := ::Parent:hWnd
      ENDIF
   ENDIF

   IF ::Application != NIL
      DEFAULT ::ClsName TO ::Application:ClsName
   ENDIF

   IF !::__IsStandard .AND. ::ClsName != "__VideoCapture"
      GetClassInfo( ::Instance, ::ClsName, @wc )
      IF !::__Register( wc )
         RETURN NIL
      ENDIF
   ENDIF

   IF ! ( ::ClsName == TOOLTIPS_CLASS )
      DEFAULT ::ToolTip TO ToolTip( Self )
   ENDIF

   ::xLeft   := IFNIL( ::xLeft  , 0            , ::xLeft  )
   ::xTop    := IFNIL( ::xTop   , 0            , ::xTop   )
   ::xWidth  := IFNIL( ::xWidth , CW_USEDEFAULT, ::xWidth )
   ::xHeight := IFNIL( ::xHeight, CW_USEDEFAULT, ::xheight)

   IF ::__ClassInst != NIL
      ::Style := ::Style | WS_VISIBLE
   ENDIF

   IF ::__ClassInst == NIL .AND. !EMPTY( ::BitmapMask )
      ::Style := WS_POPUP
      ::ExStyle := 0
      DEFAULT ::BitmapMaskColor TO RGB( 0, 0, 0)

      cBmpMask := ::BitmapMask
      IF VALTYPE( cBmpMask ) == "A"
         cBmpMask := cBmpMask[2]
         ::__hBmpRgn := LoadImage( ::Instance, cBmpMask, IMAGE_BITMAP, 0, 0, LR_VGACOLOR )
       ELSE
         IF RIGHT( UPPER( ::BitmapMask ), 4 ) == ".BMP"
            ::__hBmpRgn := LoadImage( ::Instance, ::BitmapMask, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_VGACOLOR )
         ELSE
            ::__hBmpRgn := LoadBitmap( ::Instance, ::BitmapMask )
         ENDIF
      ENDIF
      ::BkBrush   := CreatePatternBrush( ::__hBmpRgn )
      ::__hRegion := BitmapToRegion( ::__hBmpRgn, ::BitmapMaskColor )
      aSize       := GetBmpSize( ::__hBmpRgn )
      ::xWidth    := aSize[1]
      ::xHeight   := aSize[2]
   ENDIF

   IF ::__hParent != NIL
      hParent := ::__hParent
   ENDIF


   IF ::ClsName != "__VideoCapture"
      IF ::Parent != NIL .AND. ::Parent:ClsName == "StatusBarPanel"
         hParent := ::Parent:Parent:hWnd
      ENDIF
      ::hWnd := CreateWindowEx( ::ExStyle, ::ClsName, ::Caption, ::Style, ::Left, ::Top, ::Width, ::Height, hParent, ::Id, ::AppInstance, ::__ClientStruct )
    ELSE
      ::hWnd := capCreateCaptureWindow( "CaptureWindow", WS_CHILD | WS_VISIBLE, ::Left, ::Top, ::Width, ::Height, hParent, 0 )
   ENDIF
   IF ::hWnd == 0
      nError := GetLastError()
      cError := FormatMessage( ,,nError )
      cError := STRTRAN( cError, CHR(13)+CHR(10) )
      Throw( ErrorNew( "Window.prg", EG_CREATE, hParent, ProcName(), cError, ;
                       { ::ExStyle, ::ClsName, ::Caption, ::Style, ::Left, ::Top, ::Width, ::Height, hParent, ::Id, ::AppInstance, ::__ClientStruct } ) )
   ENDIF

   IF ::xCursor != IDC_ARROW
      ::__SetWindowCursor( ::xCursor )
   ENDIF

   IF ::Application != NIL .AND. !EMPTY( ::__Accelerators )
      ::__hAccelTable := CreateAcceleratorTable( ::__Accelerators )
      ::Application:AddAccelerators( ::hWnd, ::__hAccelTable )
   ENDIF

   IF !::Theming
      ::RemoveWindowTheme()
   ENDIF
   IF ::Center
      ::CenterWindow()
   ENDIF

   ::__ArrayPointer := ARRAYPOINTER( Self )
   SetProp( ::hWnd, "PROP_CLASSOBJECT", ::__ArrayPointer )

   ::__SubClass()

   nLeft := ::Left
   nTop  := ::Top
   ::GetClientRect()
   ::GetWindowRect()
   ::xLeft := nLeft
   ::xTop  := nTop

   ::__aCltRect  := { ::Left, ::Top, ::Width, ::Height }
   ::__ClientRect   := { ::Left, ::Top, ::Width, ::Height }
   ::OriginalRect := { ::Left, ::Top, ::ClientWidth, ::ClientHeight }

   ::Font:Set( Self )

   TRY
      IF !EMPTY( ::Caption ) .AND. ::SmallCaption
         ::OriginalRect[4] -= MAX( ::CaptionHeight, IIF( ::Font != NIL, ABS( ::Font:Height ), ABS( ::Form:Font:Height ) ) + 8 )
      ENDIF
   CATCH
   END

   IF ::Parent != NIL /*.AND. ::__xCtrlName != "Splitter"*/ .AND. ::ClsName != TOOLTIPS_CLASS .AND. ::__xCtrlName != "CtrlMask" .AND. ::ClsName != "MDIClient"// .AND. ::ClsName != "Dialog"
      IF ::DisableParent .AND. ::__ClassInst == NIL
         ::Parent:Disable()
      ENDIF
      IF ::SetChildren .AND. ( !(::Parent:ClsName == WC_TABCONTROL) .OR. ::__xCtrlName == "TabPage" .OR. ::__ClassInst != NIL )
         AADD( ::Parent:Children, Self )
      ENDIF
   ENDIF

   IF ::__ClassInst == NIL .AND. ::__hRegion != NIL
      SetWindowRgn( ::hWnd, ::__hRegion, .T. )
      DeleteObject( ::__hBmpRgn )
      //DeleteObject( ::__hRegion )
   ENDIF

   IF ::ToolTip != NIL .AND. !EMPTY( ::ToolTip:Text )
      ::ToolTip:Create()
   ENDIF

   IF ::__ClassInst != NIL .AND. ::ClsName != TOOLTIPS_CLASS
      ::__ClassInst:hWnd      := ::hWnd
      ::__ClassInst:ClsName   := ::ClsName
      //::__ClassInst:Visible   := ::Visible
      ::__ClassInst:Style     := ::Style
      ::__ClassInst:ExStyle   := ::ExStyle
      ::__ClassInst:Autoclose := ::Autoclose

      IF ::xMdiContainer
         ::__ClassInst:MDIClient := __ClsInst( ::MDIClient:ClassH )
      ENDIF

   ENDIF
   IF ::xMdiContainer
      ::MDIClient:Create()

      IF ::__ClassInst != NIL
         ::__ClassInst:MDIClient := __ClsInst( ::MDIClient:ClassH )
       ELSEIF ::Application != NIL
         ::Application:MDIClient := ::MDIClient:hWnd
      ENDIF

    ELSEIF !::__IsControl
      ::MDIClient := NIL
   ENDIF

   IF ::Parent != NIL .AND. ( ::Parent:VertScroll .OR. ::Parent:HorzScroll )
      ::Parent:OriginalRect[3] := MAX( ::Parent:OriginalRect[3], ::Left + ::ClientWidth  )
      ::Parent:OriginalRect[4] := MAX( ::Parent:OriginalRect[4], ::Top  + ::ClientHeight )
      ::Parent:__SetScrollBars()
   ENDIF

   FOR EACH oObj IN ::Components
       IF oObj:__xCtrlName == "Timer" .AND. oObj:AutoRun
          oObj:Start()
       ENDIF
       IF oObj:__xCtrlName == "NotifyIcon"
          oObj:Visible := oObj:Visible
       ENDIF
   NEXT

   TRY
      IF ::Parent != NIL .AND. !::ClsName == "MDIChild"
         ::__OnParentSize( ::Parent:Width, ::Parent:Height /*::Parent:OriginalRect[3], ::Parent:OriginalRect[4]*/, NIL, .T. )
      ENDIF
   CATCH
   END

   IF !(::ClsName == "AtlAxWin")
      ::OnCreate()
      ExecuteEvent( "OnCreate", Self )
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------

METHOD AddAccelerator( nVirtKey, nKey, nId ) CLASS Window
   LOCAL n
   IF ( n := ASCAN( ::__Accelerators, {|a| a[1]==nVirtKey .AND. a[2]==nKey} ) ) > 0
      ::__Accelerators[n][3] := nId
    ELSE
      AADD( ::__Accelerators, { nVirtKey, nKey, nId } )
   ENDIF
   IF ::Application != NIL .AND. ::__hAccelTable != NIL
      ::Application:DelAccelerators( ::hWnd, ::__hAccelTable )
      DestroyAcceleratorTable( ::__hAccelTable )
   ENDIF
   ::__hAccelTable := CreateAcceleratorTable( ::__Accelerators )
   IF ::Application != NIL
      ::Application:AddAccelerators( ::hWnd, ::__hAccelTable )
   ENDIF
RETURN Self

METHOD Destroy() CLASS Window
   IF ::ClsName == "MDIChild"
      ::Parent:MdiDestroy( Self )
    ELSE
      DestroyWindow( ::hWnd )
   ENDIF
RETURN Self

METHOD SetTabOrder( nTabOrder ) CLASS Window
   LOCAL n, hAfter
   IF nTabOrder > 0 .AND. nTabOrder != ::xTabOrder
      TRY
         IF nTabOrder == 1
            hAfter := HWND_TOP
          ELSE
            hAfter := ::Parent:Children[ nTabOrder-1 ]:hWnd
         ENDIF
         IF ::__ClassInst != NIL
            ADEL( ::Parent:Children, ::xTabOrder, .T. )
            IF nTabOrder > LEN( ::Parent:Children )
               AADD( ::Parent:Children, Self )
             ELSE
               AINS( ::Parent:Children, nTabOrder, Self, .T. )
            ENDIF
            FOR n := 1 TO LEN( ::Parent:Children )
                ::Parent:Children[n]:xTabOrder := n
                IF ::Parent:Children[n]:__ClassInst != NIL
                   ::Parent:Children[n]:__ClassInst:xTabOrder := n
                ENDIF
            NEXT
         ENDIF
         IF ::hWnd != NIL
            SetWindowPos( ::hWnd, hAfter, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE )
         ENDIF
      CATCH
      END
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------
METHOD __SubClass() CLASS Window
   IF ::__lSubClass
      IF ::__nProc != NIL
         ::__UnSubClass()
      ENDIF
      ::__pCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, ::__WndProc ), Self )
      ::__nProc := SetWindowLong( ::hWnd, GWL_WNDPROC, ::__pCallBackPtr )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------
METHOD __UnSubClass() CLASS Window
   IF ::__lSubClass .AND. ::__nProc != NIL
      SetWindowLong( ::hWnd, GWL_WNDPROC, ::__nProc )
      ::__nProc := NIL
      FreeCallBackPointer( ::__pCallBackPtr )
      ::__pCallBackPtr := NIL
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __CreateMDI( lCreate ) CLASS Window
   LOCAL nW,nH, cc, oControl, oTop
   DEFAULT lCreate TO .T.
   ::__IdeImageIndex := 3
   IF ::hWnd != NIL

      IF lCreate .AND. ( ::MDIClient == NIL .OR. !IsWindow( ::MDIClient:hWnd ) )
         ::MDIClient := MDIClient( Self )
         ::MDIClient:Create()
         IF ::__ClassInst != NIL
            ::__ClassInst:MDIClient := __ClsInst( ::MDIClient:ClassH )
         ENDIF

         IF ::Application != NIL .AND. ::__ClassInst == NIL
            ::Application:MDIClient := ::MDIClient:hWnd
         ENDIF
         ::SendMessage( WM_SIZE, 0, MAKELONG( ::ClientWidth, ::ClientHeight ) )
       ELSE

         ::MDIClient:Destroy()
         ::MDIClient := NIL
         IF ::Application != NIL
            ::Application:MDIClient := NIL
         ENDIF
         ::__WindowStyle := WT_WINDOW
         IF ::__ClassInst != NIL
            AEVAL( ::Application:Project:Forms, {|o| o:MDIChild := .F.} )
         ENDIF
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetBackColor( nColor, lRepaint ) CLASS Window

   DEFAULT lRepaint TO .T.
   IF nColor == NIL .AND. ::__ForceSysColor
      nColor := ::BackSysColor
   ENDIF
   ::xBackColor := nColor

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
      ::BkBrush := NIL
   ENDIF
   IF nColor != NIL
      ::BkBrush := CreateSolidBrush( nColor )
   ENDIF
   IF ::IsWindowVisible()
      IF lRepaint
         ::Refresh()
       ELSE
         ::InValidateRect()
      ENDIF
   ENDIF
RETURN SELF

METHOD SetForeColor( nColor, lRepaint ) CLASS Window
   DEFAULT lRepaint TO .T.
   ::xForeColor := nColor
   IF lRepaint .AND. ::IsWindowVisible()
      ::Refresh()
   ENDIF
   IF ::IsWindowVisible()
      ::InValidateRect()
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD IsCovered( nTopClip) CLASS Window
   LOCAL lRet := FALSE
   LOCAL aClip, nRet
   DEFAULT nTopClip TO 0
   nRet := ::Drawing:GetClipBox( @aClip )
   IF aClip[4] < ::ClientHeight .OR. aClip[3] < ::ClientWidth .OR. aClip[1] > 0 .OR. aClip[2]-nTopClip > 0 .OR.nRet == 3
      RETURN TRUE
   ENDIF
RETURN FALSE

//-----------------------------------------------------------------------------------------------

METHOD IsChildren( hWnd ) CLASS Window

   LOCAL oChild

   IF ::hWnd == hWnd
      RETURN Self
   ENDIF
   FOR EACH oChild IN ::Children
      IF oChild:IsChildren( hWnd ) != NIL
         RETURN oChild
      ENDIF
   NEXT

RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD SetParent( oParent ) CLASS Window
   LOCAL hWnd, n
   IF ::Parent != NIL
      IF ( n := ASCAN( ::Parent:Children, {|o|o:hWnd==::hWnd} ) ) > 0
         ADEL( ::Parent:Children, n, .T. )
      ENDIF
   ENDIF
   ::Parent := oParent
   AADD( oParent:Children, Self )
   SetParent( ::hWnd, oParent:hWnd )
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetExStyle(nStyle,lAdd) CLASS Window
   DEFAULT lAdd TO .T.
   IF ::IsWindow()
      ::ExStyle := ::GetWindowLong( GWL_EXSTYLE )
   ENDIF
   IF lAdd
      ::ExStyle := ::ExStyle | nStyle
    ELSE
      ::ExStyle := ::ExStyle & NOT( nStyle )
   ENDIF
   IF ::IsWindow()
      ::SetWindowLong( GWL_EXSTYLE, ::ExStyle )
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
      ::RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD GetWindowPlacement() CLASS Window
   LOCAL pWp := (struct WINDOWPLACEMENT)
   GetWindowPlacement( ::hWnd, @pWp )
RETURN pWp

//-----------------------------------------------------------------------------------------------

METHOD GetWindowInfo() CLASS Window
   LOCAL pWi := (struct WINDOWINFO)
   GetWindowInfo( ::hWnd, @pWi )
RETURN pWi

//-----------------------------------------------------------------------------------------------

METHOD SetStyle( nStyle, lAdd ) CLASS Window
   LOCAL nSt, cStyle := "", n
   DEFAULT lAdd TO .T.
   IF ::__ClassInst != NIL .AND. ::ClsName == "Button" .AND. lAdd .AND. nStyle == BS_DEFPUSHBUTTON
      RETURN NIL
   ENDIF
   IF ::IsWindow()
      ::Style := ::GetWindowLong( GWL_STYLE )
   ENDIF
   IF nStyle == WS_DISABLED
      lAdd := !lAdd
   ENDIF
   IF lAdd
      ::Style := ::Style | nStyle
    ELSE
      ::Style := ::Style & NOT( nStyle )
   ENDIF
   IF ::IsWindow() //.AND. !::__CustomOwner
      SWITCH nStyle
         CASE WS_VISIBLE
              IF ::__ClassInst != NIL
                 RETURN NIL
              ENDIF
              IF lAdd
                 ::Show()
               ELSE
                 ::Hide()
              ENDIF
              EXIT
         CASE WS_DISABLED
              EnableWindow( ::hWnd, lAdd )
              //IF ::IsContainer .AND. ( ::Parent == NIL .OR. ::Parent:ClsName != "DLGEDT" )
              //   AEVAL( ::Children, {|o| o:SetStyle( nStyle, lAdd ) } )
              //ENDIF
              EXIT

      END
      ::SetWindowLong( GWL_STYLE, ::Style )
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER)
      ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   ENDIF
RETURN self

//-----------------------------------------------------------------------------------------------

FUNCTION ObjFromHandle( hWnd )
   LOCAL pPtr := GetProp( hWnd, "PROP_CLASSOBJECT" )
   IF pPtr != NIL .AND. pPtr != 0
      RETURN ArrayFromPointer( pPtr )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

FUNCTION CheckControlName( oParent, cName, oExclude )
   LOCAL oControl, lRet := .T.
   IF oParent != NIL
      FOR EACH oControl IN oParent:Children
          IF !lRet
             EXIT
          ENDIF
          IF !(oControl==oExclude) .AND. oControl:Name == cName
             lRet := .F.
             EXIT
          ENDIF
          lRet := CheckControlName( oControl, cName, oExclude )
      NEXT
   ENDIF
RETURN lRet

//-----------------------------------------------------------------------------------------------
FUNCTION ExecuteEvent( cEvent, oObj )
   LOCAL cFormEvent, nRet

   IF HGetPos( oObj:EventHandler, cEvent ) != 0
      cFormEvent := oObj:EventHandler[ cEvent ]
      IF !( oObj:ClassH == oObj:Form:ClassH )
         IF __objHasMsg( oObj, cFormEvent )
            nRet := oObj:&cFormEvent( oObj )
            RETURN nRet
         ENDIF
         IF __objHasMsg( oObj:Form, cFormEvent )
            nRet := oObj:Form:&cFormEvent( oObj )
            RETURN nRet
         ENDIF
         IF __objHasMsg( oObj:Parent, cFormEvent )
            nRet := oObj:Parent:&cFormEvent( oObj )
            RETURN nRet
         ENDIF
         IF oObj:Parent != NIL .AND. oObj:Parent:__xCtrlName == "TabPage"
            nRet := oObj:Parent:Parent:Form:&cFormEvent( oObj )
         ENDIF
       ELSE
         nRet := oObj:&cFormEvent( oObj )
      ENDIF
   ENDIF
RETURN nRet

//-----------------------------------------------------------------------------------------------
#define WM_THEMECHANGED   0x031A
#define WM_SOCKET_NOTIFY  0x0373
#define WM_SOCKET_DEAD    0x0374
#define WM_KICKIDLE       0x036A

#define WM_IDLE WM_USER + 1

METHOD __ControlProc( hWnd, nMsg, nwParam, nlParam ) CLASS Window

   LOCAL nRet, nCode, nId, n, cBuffer, oMdi, oObj
   LOCAL nW, nH, oChild, oItem, oSub, x, y, hDef, nLeft, nTop, nWidth, nHeight, lShift, h, nLines, nDelta, nScroll, nPage
   LOCAL lShow, lHide, hParent, pPtr, pProc, oCtrl, aRect, aPt, ps, mii, cProp
   LOCAL Band, msg, lHandled, aComp, oControl, oMenu, mmi, oTop, oBottom, oLeft, nFiles, oForm, aChildren

   LOCAL dis, si, rc, pt, hBrush, cFile, hwndFrom, idFrom, code, aParams
   LOCAL CtlType, CtlID, itemID, itemAction, itemState, hWndItem, hDC, rcItem, itemData
   LOCAL hMemDC, hMemBitmap, hOldBitmap, aCommand, cBmpHeader, cBmpInfo, cBmpBits, nAnimation
   LOCAL aProperties, aProperty
   
   LOCAL i, c, cProcStack
   static hCursor, hPrevCursor, hPrevWnd

   ::Msg    := nMsg
   ::wParam := nwParam
   ::lParam := nlParam

   IF nMsg == ::__InstMsg
      SetForeGroundWindow( hWnd )
      IF IsIconic( hWnd )
         ShowWindow( hWnd, SW_RESTORE )
      ENDIF
      IF nwParam != 0
         PostMessage( hWnd, nwParam, 0, 0 )
      ENDIF
      RETURN 0
   ENDIF
   SWITCH nMsg
      CASE WM_UPDATEUISTATE
           EXIT

      //CASE WM_IDLE
      //     HIWORD( GetQueueStatus( QS_INPUT ) )
      //     EXIT

      //CASE WM_PARENTNOTIFY
      //     nRet := ExecuteEvent( "OnParentNotify", Self )
      //     nRet := ::OnParentNotify( Self )
      //     EXIT

      CASE WM_DROPFILES
           ::FilesDroped := {}

           nFiles := DragQueryFile( nwParam, 0xFFFFFFFF ) - 1
           FOR n := 0 TO nFiles
               DragQueryFile( nwParam, n, @cFile, MAX_PATH )
               AADD( ::FilesDroped, cFile )
           NEXT

           nRet := ExecuteEvent( "OnDropFiles", Self )
           ODEFAULT nRet TO ::OnDropFiles( Self )
           EXIT

      CASE WM_NCMOUSEMOVE
           IF !::__lNCMouseHover
              ::__lNCMouseHover := .T.
              ::TrackMouseEvent( TME_NONCLIENT | TME_LEAVE | TME_HOVER )
           ENDIF
           nRet := ExecuteEvent( "OnNcMouseMove", Self )
           ODEFAULT nRet TO ::OnNcMouseMove( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_NCMOUSELEAVE
           ::TrackMouseEvent( TME_NONCLIENT | TME_HOVER )
           ::__lNCMouseHover := .F.
           nRet := ExecuteEvent( "OnNCMouseLeave", Self )
           ODEFAULT nRet TO ::OnNCMouseleave( nwParam, LoWord(nlParam), Hiword(nlparam) )
           EXIT

      CASE WM_NCMOUSEHOVER
           ::TrackMouseEvent( TME_NONCLIENT | TME_LEAVE )
           ::__lNCMouseHover := .T.
           nRet := ExecuteEvent( "OnNCMouseHover", Self )
           ODEFAULT nRet TO ::OnNCMouseHover( nwParam, LoWord(nlParam), Hiword(nlparam) )
           EXIT

      CASE WM_SETCURSOR
           nRet := ExecuteEvent( "OnSetCursor", Self )
           ODEFAULT nRet TO ::OnSetCursor( nwParam, nlParam )
           IF ::Application != NIL .AND. ::Application:Cursor != NIL
              WinSetCursor( ::Application:Cursor )
              RETURN .T.
           ENDIF
           IF ::__hCursor != NIL
              WinSetCursor( ::__hCursor )
              RETURN .T.
           ENDIF
           EXIT

      CASE WM_MOUSEMOVE
           IF !::__lMouseHover
              IF ::ToolTip != NIL .AND. ::__lPopTip
                 ::__lPopTip := .F.
                 ::ToolTip:SetTimer( 25, 1000 )
              ENDIF
              ::__lMouseHover := .T.
              nRet := ExecuteEvent( "OnMouseHover", Self )
              ODEFAULT nRet TO ::OnMouseHover( nwParam, LoWord(nlParam), Hiword(nlparam) )
              ::TrackMouseEvent( TME_LEAVE )
           ENDIF
           nRet := ExecuteEvent( "OnMouseMove", Self )
           ODEFAULT nRet TO ::OnMouseMove( nwParam, LOWORD(nlParam), HIWORD(nlparam) )
           EXIT

      CASE WM_MOUSELEAVE
           nRet := ExecuteEvent( "OnMouseLeave", Self )
           ODEFAULT nRet TO ::OnMouseleave( nwParam, LoWord(nlParam), Hiword(nlparam) )
           ::__lMouseHover := .F.
           EXIT
/*
      CASE WM_MOUSEHOVER
           ::__lMouseHover := .T.
           nRet := ExecuteEvent( "OnMouseHover", Self )
           ODEFAULT nRet TO ::OnMouseHover( nwParam, LoWord(nlParam), Hiword(nlparam) )
           IF ::ToolTip != NIL .AND. ::ToolTip:hWnd != NIL
              ::ToolTip:Popup()
           ENDIF
           ::TrackMouseEvent( TME_LEAVE )
           EXIT
*/
      CASE WM_MOUSEACTIVATE
           nRet := ExecuteEvent( "OnMouseActivate", Self )
           ODEFAULT nRet TO ::OnMouseActivate( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCLBUTTONUP
           nRet := ExecuteEvent( "OnNCLButtonUp", Self )
           ODEFAULT nRet TO ::OnNCLButtonUp( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCLBUTTONDOWN
           nRet := ExecuteEvent( "OnNCLButtonDown", Self )
           ODEFAULT nRet TO ::OnNCLButtonDown( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCLBUTTONDBLCLK
           nRet := ExecuteEvent( "OnNCLButtonDblClk", Self )
           ODEFAULT nRet TO ::OnNCLButtonDblClk( nwParam, LoWord( nlParam ), HiWord( nlParam ), hWnd )
           EXIT



      CASE WM_NCRBUTTONUP
           nRet := ExecuteEvent( "OnNCRButtonUp", Self )
           ODEFAULT nRet TO ::OnNCRButtonUp( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCRBUTTONDOWN
           nRet := ExecuteEvent( "OnNCRButtonDown", Self )
           ODEFAULT nRet TO ::OnNCRButtonDown( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCRBUTTONDBLCLK
           nRet := ExecuteEvent( "OnNCRButtonDblClk", Self )
           ODEFAULT nRet TO ::OnNCRButtonDblClk( nwParam, LoWord( nlParam ), HiWord( nlParam ), hWnd )
           EXIT



      CASE WM_NCMBUTTONUP
           nRet := ExecuteEvent( "OnNCMButtonUp", Self )
           ODEFAULT nRet TO ::OnNCMButtonUp( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCMBUTTONDOWN
           nRet := ExecuteEvent( "OnNCMButtonDown", Self )
           ODEFAULT nRet TO ::OnNCMButtonDown( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCMBUTTONDBLCLK
           nRet := ExecuteEvent( "OnNCMButtonDblClk", Self )
           ODEFAULT nRet TO ::OnNCMButtonDblClk( nwParam, LoWord( nlParam ), HiWord( nlParam ), hWnd )
           EXIT



      CASE WM_NCXBUTTONUP
           nRet := ExecuteEvent( "OnNCXButtonUp", Self )
           ODEFAULT nRet TO ::OnNCXButtonUp( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCXBUTTONDOWN
           nRet := ExecuteEvent( "OnNCXButtonDown", Self )
           ODEFAULT nRet TO ::OnNCXButtonDown( nwParam, LoWord(nlParam), HiWord(nlParam) )
           EXIT

      CASE WM_NCXBUTTONDBLCLK
           nRet := ExecuteEvent( "OnNCXButtonDblClk", Self )
           ODEFAULT nRet TO ::OnNCXButtonDblClk( nwParam, LoWord( nlParam ), HiWord( nlParam ), hWnd )
           EXIT



      CASE WM_NCHITTEST
           nRet := ExecuteEvent( "OnNCHitTest", Self )
           IF nRet == NIL
              nRet := ::OnNCHitTest( loword(nlParam), hiword(nlParam), nwParam )
              //IF nRet == NIL .AND. ::__ClassInst == NIL .AND. !EMPTY( ::BitmapMask )
              //   RETURN HTCAPTION
              //ENDIF
           ENDIF
           EXIT

      CASE WM_INITMENUPOPUP
           nRet := ExecuteEvent( "OnInitMenuPopup", Self )
           ODEFAULT nRet TO ::OnInitMenuPopup( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_LBUTTONDOWN
           nRet := ExecuteEvent( "OnLButtonDown", Self )
           ODEFAULT nRet TO ::OnLButtonDown( nwParam, LoWord( nlParam ), HiWord( nlParam ), hWnd )
           ODEFAULT nRet TO __Evaluate( ::OnWMLButtonDown, Self, LoWord( nlParam ), HiWord( nlParam ), nRet )
           EXIT

      CASE WM_LBUTTONUP
           nRet := ExecuteEvent( "OnLButtonUp", Self )
           ODEFAULT nRet TO ::OnLButtonUp( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           ODEFAULT nRet TO __Evaluate( ::OnWMLButtonUp, Self, LoWord( nlParam ), HiWord( nlParam ), nRet )
           EXIT

      CASE WM_LBUTTONDBLCLK
           nRet := ExecuteEvent( "OnLButtonDblClk", Self )
           ODEFAULT nRet TO ::OnLButtonDblClk( nwParam, LoWord( nlParam ), HiWord( nlParam ), hWnd )
           ODEFAULT nRet TO __Evaluate( ::OnWMLButtonDblClk, Self, nwParam, nlParam )
           EXIT

      CASE WM_MBUTTONDOWN
           nRet := ExecuteEvent( "OnMButtonDown", Self )
           ODEFAULT nRet TO ::OnMButtonDown( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_MBUTTONUP
           nRet := ExecuteEvent( "OnMButtonUp", Self )
           ODEFAULT nRet TO ::OnMButtonUp( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_MENUSELECT
           nRet := ExecuteEvent( "OnMenuSelect", Self )
           ODEFAULT nRet TO ::OnMenuSelect( LoWord(nwParam), Hiword(nwparam), nlParam )
           EXIT

      CASE WM_RBUTTONDOWN
           nRet := ExecuteEvent( "OnRButtonDown", Self )
           ODEFAULT nRet TO ::OnRButtonDown( nwParam, LoWord(nlParam), Hiword(nlparam) )
           EXIT

      CASE WM_RBUTTONUP
           nRet := ExecuteEvent( "OnRButtonUp", Self )
           ODEFAULT nRet TO ::OnRButtonUp( nwParam, LoWord(nlParam), Hiword(nlparam) )
           EXIT

      CASE WM_SIZE
           aChildren := IIF( ::__DockChildren != NIL, ::__DockChildren, ::Children )
           ::__lReqBrush := .T.
           IF EMPTY( ::__ClientRect )
              RETURN 0
           ENDIF
           ::ClientWidth  := GETXPARAM( nlParam )
           ::ClientHeight := GETYPARAM( nlParam )

           x := MAX( ::xWidth,  ::OriginalRect[3] )
           y := MAX( ::xHeight, ::OriginalRect[4] )

           IF ::__hMemBitmap != NIL
              DeleteObject( ::__hMemBitmap )
              ::__hMemBitmap := CreateCompatibleBitmap( ::Drawing:hDC, ::ClientWidth, ::ClientHeight )
           ENDIF

           IF ::ClsName != "DataGrid"
              hDef := BeginDeferWindowPos( LEN( aChildren ) )
              FOR EACH oChild IN aChildren
                  IF VALTYPE( oChild ) == "O"
                     oChild:__OnParentSize( x, y, @hDef, ,, ::__aCltRect[3], ::__aCltRect[4] )
                     IF oChild:__IsControl .AND. oChild:Anchor:Center
                        oChild:CenterWindow()
                     ENDIF
                  ENDIF
              NEXT
              EndDeferWindowPos( hDef )

              ::__ClientRect[3] := ::ClientWidth
              ::__ClientRect[4] := ::ClientHeight
              ::__aCltRect[3] := x
              ::__aCltRect[4] := y
           ENDIF

           nRet := ExecuteEvent( "OnSize", Self )
           ODEFAULT nRet TO ::OnSize( nwParam, ::ClientWidth, ::ClientHeight )
           ODEFAULT nRet TO __Evaluate( ::OnWMSize, Self, nwParam, nlParam, nRet )

           TRY
              IF (nwParam == 0 .or. nwParam == 2) .AND. ::MDIContainer .AND. ::MDIClient != NIL .AND. ::MDIClient:hWnd != NIL
                 ::LeftMargin   := 0
                 ::TopMargin    := 0
                 ::RightMargin  := ::ClientWidth
                 ::BottomMargin := ::ClientHeight

                 IF ::__ClassInst == NIL
                    cProp := ::MDIClient:AlignLeft
                    IF VALTYPE( cProp ) == "C" .AND. ( n := ASCAN( aChildren, {|o| o:Name == cProp } ) ) > 0
                       ::MDIClient:AlignLeft := aChildren[n]
                    ENDIF

                    cProp := ::MDIClient:AlignTop
                    IF VALTYPE( cProp ) == "C" .AND. ( n := ASCAN( aChildren, {|o| o:Name == cProp } ) ) > 0
                       ::MDIClient:AlignTop := aChildren[n]
                    ENDIF

                    cProp := ::MDIClient:AlignRight
                    IF VALTYPE( cProp ) == "C" .AND. ( n := ASCAN( aChildren, {|o| o:Name == cProp } ) ) > 0
                       ::MDIClient:AlignRight := aChildren[n]
                    ENDIF

                    cProp := ::MDIClient:AlignBottom
                    IF VALTYPE( cProp ) == "C" .AND. ( n := ASCAN( aChildren, {|o| o:Name == cProp } ) ) > 0
                       ::MDIClient:AlignBottom := aChildren[n]
                    ENDIF
                 ENDIF

                 IF VALTYPE( ::MDIClient:AlignLeft ) == "O"
                    ::LeftMargin := ::MDIClient:AlignLeft:Left + ::MDIClient:AlignLeft:Width + IIF( ::MDIClient:AlignLeft:RightSplitter != NIL, ::MDIClient:AlignLeft:RightSplitter:Weight, 0 )
                 ENDIF
                 IF VALTYPE( ::MDIClient:AlignTop ) == "O"
                    ::TopMargin := ::MDIClient:AlignTop:Top + ::MDIClient:AlignTop:Height + IIF( ::MDIClient:AlignTop:BottomSplitter != NIL, ::MDIClient:AlignTop:BottomSplitter:Weight, 0 )
                 ENDIF
                 IF VALTYPE( ::MDIClient:AlignRight ) == "O"
                    ::RightMargin := ::MDIClient:AlignRight:Left - IIF( ::MDIClient:AlignRight:LeftSplitter != NIL, ::MDIClient:AlignRight:LeftSplitter:Weight, 0 )
                 ENDIF
                 IF VALTYPE( ::MDIClient:AlignBottom ) == "O"
                    ::BottomMargin := ::MDIClient:AlignBottom:Top - IIF( ::MDIClient:AlignBottom:TopSplitter != NIL, ::MDIClient:AlignBottom:TopSplitter:Weight, 0 )
                 ENDIF
                 MoveWindow( ::MDIClient:hWnd, ::LeftMargin,;
                                               ::TopMargin,;
                                               ::RightMargin - ::LeftMargin,;
                                               ::BottomMargin - ::TopMargin, .T.)
              ENDIF
            CATCH
           END
           IF ( nwParam == SIZE_MAXIMIZED .OR. nwParam == SIZE_RESTORED ) //.AND. ::IsWindowVisible()
              ::__SetScrollBars()
           ENDIF
           EXIT

      CASE WM_MOVING
           nRet := ExecuteEvent( "OnMoving", Self )
           ODEFAULT nRet TO ::OnMoving( LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_MOVE
           ::__lReqBrush := .T.
           IF ::ClsName != TOOLTIPS_CLASS

              IF ::Parent != NIL

                 rc := (struct RECT)
                 GetWindowRect( hWnd, @rc )

                 pt := (struct POINT)
                 pt:x := rc:left
                 pt:y := rc:top
                 ScreenToClient( ::Parent:hWnd, @pt )

                 ::xLeft   := pt:x //+ ::Parent:HorzScrollPos
                 ::xTop    := pt:y //+ ::Parent:VertScrollPos
                 x := pt:x
                 y := pt:y

               ELSE
                 ::xLeft := GETXPARAM( nlParam )
                 ::xTop  := GETYPARAM( nlParam )
                 x := GETXPARAM( nlParam )
                 y := GETYPARAM( nlParam )
              ENDIF
            ELSE
              x := GETXPARAM( nlParam )
              y := GETYPARAM( nlParam )
           ENDIF

           nRet := ExecuteEvent( "OnMove", Self )
           ODEFAULT nRet TO ::OnMove( x, y )

           IF ::Parent != NIL .AND. ::Parent:ClsName == "DeskTop"
              ::Parent:GetClientRect()
           ENDIF

           IF ::ClsName != "DataGrid"
              TRY
              FOR EACH oChild IN ::Children
                  IF __ObjHasMsg( oChild, "Anchor" )
                     oChild:OnParentMove()
                     IF oChild:Anchor != NIL .AND. oChild:Anchor:Center
                        oChild:CenterWindow()
                     ENDIF
                  ENDIF
              NEXT
              CATCH
              END
           ENDIF
           EXIT

//--------------------------------------------------------------------------------------------------------------------------------------

      CASE WM_TIMER
           /*
           IF ::Application:RemoteSocket != NIL
              WITH OBJECT ::Application:RemoteSocket
                 IF nwParam == :nRecId
                    :RecData := NIL
                    :SockControlProc()

                    KillTimer( ::hWnd, :nRecId )

                    IF !EMPTY( :RecData )
                       aCommand := hb_aTokens( :RecData, "|" )
                       IF aCommand[1] == "WFCS"
                          DO CASE
                             CASE aCommand[2] == "MAINFORM"
                                  :Send( "VXHS|CREATE|"+::Name+"|"+xstr(::Left)+","+xstr(::Top)+","+xstr(::Width)+","+xstr(::Height) )

                             CASE aCommand[2] == "BITMAP"
                                  IF ::Name == aCommand[3]
                                     hDC := GetDC( ::hWnd )
                                     hMemDC     := CreateCompatibleDC( hDC )
                                     hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
                                     hOldBitmap := SelectObject( hMemDC, hMemBitmap)

                                     SendMessage( ::hWnd, WM_PRINT, hMemDC, PRF_CLIENT | PRF_ERASEBKGND | PRF_CHILDREN )

                                     GetBmpString( hMemBitmap, @cBmpHeader, @cBmpInfo, @cBmpBits )

                                     :Send( "VXHS|BITMAP|"+::Name+"|" + cBmpHeader + cBmpInfo + cBmpBits )
                                     
                                     SelectObject( hMemDC,  hOldBitmap )
                                     DeleteObject( hMemBitmap )
                                     DeleteDC( hMemDC )
                                     ReleaseDC( ::hWnd, hDC )
                                  ENDIF
                                  
                             CASE aCommand[2] == "DESTROY"
                                  IF ::Name == aCommand[3]
                                     ::Destroy()
                                  ENDIF
                          ENDCASE
                       ENDIF
                    ENDIF

                    SetTimer( ::hWnd, :nRecId, 250 )
                    RETURN 0

                 ENDIF
              END
           ENDIF
           */

           nRet := ExecuteEvent( "OnTimer", Self )
           ODEFAULT nRet TO ::OnTimer( nwParam, nlParam )
           //IF ::ClsName == "Vxh_Form"
           //   KillTimer( hWnd, nwParam )
           //endif
           IF ( !::ClsName == TOOLTIPS_CLASS .AND. !::ClsName == ANIMATE_CLASS .AND. ( !::ClsName == PROGRESS_CLASS .OR. (::__ClassInst != NIL .AND. ::Application:OsVersion:dwMajorVersion > 5) ) )
              RETURN 0
           ENDIF
           EXIT

      CASE WM_CHILDACTIVATE
           nRet := ExecuteEvent( "OnChildActivate", Self )
           EXIT

      CASE WM_ACTIVATE
           nRet := ExecuteEvent( "OnActivate", Self )
           ODEFAULT nRet TO ::OnActivate( nwParam, nlParam )
           EXIT

      CASE WM_SETFONT
           nRet := ExecuteEvent( "OnSetFont", Self )
           ODEFAULT nRet TO ::OnSetFont( nwParam, nlParam )
           EXIT

      CASE WM_APPCOMMAND
           EXIT

      CASE WM_NCXBUTTONUP
           EXIT

      CASE WM_NCCREATE
           nRet := ExecuteEvent( "OnNcCreate", Self )
           ODEFAULT nRet TO ::OnNcCreate( nwParam, nlParam )
           EXIT

      CASE WM_NCACTIVATE
           nRet := ExecuteEvent( "OnNcActivate", Self )
           ODEFAULT nRet TO ::OnNcActivate( nwParam,nlParam )

           ::Active := nwParam != 0

           IF nRet == NIL .AND. nwParam == 0 .AND. ::ClsName != "DataGrid"
              FOR EACH oChild IN ::Children
                 IF oChild:Active
                    oChild:Active := FALSE
                    oChild:InvalidateRect( {0,0, oChild:ClientWidth,14}, FALSE )
                 ENDIF
              NEXT
              IF ::KeepActive .OR. ( n := ASCAN( ::Children, {|o|o:KeepParentActive .AND. o:hWnd == nlParam } ) ) > 0
                 RETURN 1
              ENDIF
           ENDIF

           IF ::ClsName != "DataGrid"
              FOR EACH oChild IN ::Children
                 IF oChild:__xCtrlName == "CoolMenu" .AND. ::__ClassInst == NIL
                    oChild:ForeColor  := IIF( nwParam == 0, GetSysColor( COLOR_GRAYTEXT ), oChild:hBackupColor )
                    oChild:InvalidateRect(,FALSE)
                 ENDIF
              NEXT
           ENDIF
           EXIT

      CASE WM_NCCALCSIZE
           nRet := ExecuteEvent( "OnNcCalcSize", Self )
           ODEFAULT nRet TO ::OnNcCalcSize( nwParam, nlParam )
           EXIT

      CASE WM_DESTROY
           IF ::__ClassInst != NIL
              IF ::LeftSplitter != NIL
                 ::LeftSplitter:Destroy()
                 ::LeftSplitter := NIL
              ENDIF
              IF ::TopSplitter != NIL
                 ::TopSplitter:Destroy()
                 ::TopSplitter := NIL
              ENDIF
              IF ::RightSplitter != NIL
                 ::RightSplitter:Destroy()
                 ::RightSplitter := NIL
              ENDIF
              IF ::BottomSplitter != NIL
                 ::BottomSplitter:Destroy()
                 ::BottomSplitter := NIL
              ENDIF
           ENDIF
           ::HorzScroll := .F.
           ::VertScroll := .F.

           aComp := {}
           FOR n := 1 TO LEN( ::Components )
               IF ::Components[n]:Exists
                  AADD( aComp, ::Components[n] )
               ENDIF
           NEXT
           AEVAL( aComp, {|o| o:Destroy(.F.) } )

           ::Components := {}

           IF ::Parent != NIL .AND. ::__ClassInst == NIL .AND. ::Parent:Children != NIL
              FOR EACH oChild IN ::Parent:Children
                 IF oChild:__xCtrlName == "CoolBar"
                    FOR EACH Band IN oChild:Bands
                        IF Band:BandChild != NIL .AND. Band:BandChild:hWnd == ::hWnd
                           Band:BandChild := NIL
                        ENDIF
                    NEXT
                 ENDIF
              NEXT
           ENDIF

           nRet := ExecuteEvent( "OnDestroy", Self )
           ODEFAULT nRet TO ::OnDestroy( nwParam, nlParam )
           ODEFAULT nRet TO __Evaluate( ::OnWMDestroy, Self, nwParam, nlParam, nRet )
           IF ::Parent != NIL .AND. ( ::Parent:__xCtrlName == "ObjManager" .OR. ::Parent:__xCtrlName == "EvtManager" )
              ::Parent:KeepActiveCaption := .F.
              ::Parent:Redraw()
           ENDIF
//            aProperties := __ClsGetPropertiesAndValues( Self )
//            FOR EACH aProperty IN aProperties
//                oObj := __objSendMsg( Self, UPPER( aProperty[1] ) )
//                IF VALTYPE( oObj ) == "O"
//                   TRY
//                      __objSendMsg( Self, "_" + aProperty[1], NIL )
//                   CATCH
//                   END
//                ENDIF
//            NEXT
           EXIT

      CASE WM_NCDESTROY
           ::__WindowDestroy()

           nRet := ExecuteEvent( "OnNcDestroy", Self )
           ODEFAULT nRet TO ::OnNcDestroy( nwParam, nlParam )
           ::__lInitialized := .F.
           IF ::DisableParent .AND. ::__ClassInst == NIL
              ::Parent:Enable()
              ::Parent:BringWindowToTop()
           ENDIF

           IF ::__nProc != NIL
              SetWindowLong( ::hWnd, GWL_WNDPROC, ::__nProc )
              ::__nProc := NIL
           ENDIF

           RemoveProp( ::hWnd, "PROP_CLASSOBJECT" )
           ReleaseArrayPointer( ::__ArrayPointer )
           ::__ArrayPointer := NIL

           IF ::__pCallBackPtr != NIL
              FreeCallBackPointer( ::__pCallBackPtr )
              ::__pCallBackPtr := NIL
           ENDIF
           
           IF ::Application != NIL
              IF ::Application:MainForm != NIL .AND. ::Application:MainForm:hWnd == ::hWnd .AND. ::Application:__hMutex != NIL
                 CloseHandle( ::Application:__hMutex )
              ENDIF

              IF !::IsChild .AND.;
                 ::Application:MainForm != NIL .AND.;
                 ::Application:MainForm:hWnd == ::hWnd .AND.;
                 ::__InstMsg != NIL .AND.;
                 ( ::__WindowStyle != 0 .OR. ::Parent == NIL .OR. ::Parent:ClsName == "DeskTop" )
                 PostQuitMessage(0)
              ENDIF
           ENDIF
           IF ::__TaskBarParent != NIL
              DestroyWindow( ::__TaskBarParent )
           ENDIF
           ::hWnd := NIL
           EXIT

/*
      CASE WM_MDIACTIVATE
           IF nlParam == hWnd
              SendMessage( ::Parent:hWnd, WM_MDISETMENU, ::Menu:hMenu )
           ENDIF
*/
      CASE WM_INITDIALOG
           nRet := ExecuteEvent( "OnCreate", Self )
           ODEFAULT nRet TO ::OnCreate()
           ::hWnd := hWnd
           ::siv := (struct SCROLLINFO)
           ::siv:cbSize := ::siv:sizeof()
           ::siv:nMin   := 0

           ::sih := (struct SCROLLINFO)
           ::sih:cbSize := ::sih:sizeof()
           ::sih:nMin   := 0

           IF ::Parent != NIL .AND. !::Parent:Flat .AND. ::OsVer:dwMajorVersion >= 5 .AND. ::Parent:ClsName == "SysTabControl32" .AND. ::Application != NIL .AND. ::Application:IsThemedXP .AND. ::Theming
              ::EnableThemeDialogTexture( ETDT_ENABLETAB )
           ENDIF

           nLeft   := ::Left
           nTop    := ::Top
           nWidth  := ::Width
           nHeight := ::Height

           ::GetClientRect()
           ::GetWindowRect()

           ::xLeft  := nLeft
           ::xTop   := nTop

           DEFAULT nWidth TO ::xWidth
           DEFAULT nHeight TO ::xHeight

           ::xWidth := nWidth
           ::xHeight:= nHeight

           ::__ClientRect   := { nLeft, nTop, ::xWidth, ::xHeight }
           ::__aCltRect  := { nLeft, nTop, ::xWidth, ::xHeight }
           ::OriginalRect := { nLeft, nTop, ::xWidth, ::xHeight }

           ::InitDialogBox()
           ::__SetScrollBars()

           nRet := ExecuteEvent( "OnInitDialog", Self )
           ODEFAULT nRet TO ::OnInitDialog( nwParam, nlParam )
           ODEFAULT nRet TO __Evaluate( ::OnWMInitDialog, Self, nwParam, nlParam, nRet )

           ODEFAULT nRet TO 0
           IF ::Parent != NIL .AND. ::SetChildren
              AADD( ::Parent:Children, Self )
           ENDIF

           IF ::__ArrayPointer == NIL
              ::__ArrayPointer := ARRAYPOINTER( Self )
              SetProp( ::hWnd, "PROP_CLASSOBJECT", ::__ArrayPointer )
           ENDIF

           IF ::__xCtrlName == "TabPage"
              RETURN 0
           ENDIF

           IF ::Center
              ::CenterWindow()
           ENDIF

           FOR EACH oObj IN ::Components
               IF oObj:__xCtrlName == "Timer" .AND. oObj:AutoRun
                  oObj:Start()
               ENDIF
               IF oObj:__xCtrlName == "NotifyIcon"
                  oObj:Visible := oObj:Visible
               ENDIF
           NEXT
           IF EMPTY( ::__hIcon )
              SWITCH VALTYPE( ::Icon )
                 CASE "A"
                      IF ::__ClassInst == NIL .OR. EMPTY( ::Icon[1] )
                         ::__hIcon := LoadIcon( ::AppInstance, ::Icon[2] )
                         ::xIcon := ::Icon[2]
                       ELSE
                         ::__hIcon := LoadImage( ::AppInstance, ::Icon[1], IMAGE_ICON,,, LR_LOADFROMFILE )
                         ::xIcon := ::Icon[1]
                      ENDIF
                      EXIT

                 CASE "C"
                      ::__hIcon := LoadIcon( ::AppInstance, ::Icon )
                      EXIT

                 CASE "N"
                      ::__hIcon := ::Icon
                      EXIT
              END
           ENDIF
           ::SetIcon( ICON_SMALL, IIF( !EMPTY( ::__hIcon ), ::__hIcon, 0 ) )
           ::SetIcon( ICON_BIG, IIF( !EMPTY( ::__hIcon ), ::__hIcon, 0 ) )

           ::SetOpacity( ::xOpacity )

           IF ::BackgroundImage != NIL
              ::BackgroundImage:Create()
           ENDIF

           IF !::__lShown
              ::__lShown := .T.
              ::__FixDocking()

              nRet := ExecuteEvent( "OnLoad", Self )
              ODEFAULT nRet TO ::OnLoad( Self )

              IF ::Property != NIL .AND. ::__ClassInst == NIL

                 FOR n := 1 TO LEN( ::Property:Keys )
                     oObj := HGetValueAt( ::Property, n )
                     IF oObj:__xCtrlName == "TabPage" .AND. !oObj:Visible
                        oObj:__SetVisible( .F., .T. )
                     ENDIF
                 NEXT

              ENDIF
              IF ::AnimationStyle != 0 .AND. ::__ClassInst == NIL
                 RETURN ::Animate( 1000, ::AnimationStyle )
              ENDIF
           ENDIF
           ::Show( ::ShowMode )
           EXIT

      CASE WM_CHAR
           IF ::Parent != NIL
              nRet := ::Parent:OnChildChar( hWnd, nMsg, nwParam, nlParam )
              IF nRet != NIL
                 RETURN nRet
              ENDIF
           ENDIF
           nRet := ExecuteEvent( "OnChar", Self, nwParam, nlParam )
           ODEFAULT nRet TO ::OnChar( nwParam, nlParam )
           ODEFAULT nRet TO __Evaluate( ::OnWMChar, Self, nwParam, nlParam, nRet )
           EXIT

      CASE WM_CLOSE
           IF !::Modal
              nRet := ExecuteEvent( "OnClose", Self )
              ODEFAULT nRet TO ::OnClose( nwParam )
              ODEFAULT nRet TO __Evaluate( ::OnWMClose, Self, nwParam, nlParam )
           ENDIF

           IF nRet == NIL .AND. ::AnimationStyle != 0 .AND. ::__ClassInst == NIL
              nAnimation := ::AnimationStyle
              DO CASE
                 CASE nAnimation == ::System:WindowAnimation:SlideHorzPositive
                      nAnimation := ::System:WindowAnimation:SlideHorzNegative

                 CASE nAnimation == ::System:WindowAnimation:SlideHorzNegative
                      nAnimation := ::System:WindowAnimation:SlideHorzPositive

                 CASE nAnimation == ::System:WindowAnimation:SlideVertPositive
                      nAnimation := ::System:WindowAnimation:SlideVertNegative

                 CASE nAnimation == ::System:WindowAnimation:SlideVertNegative
                      nAnimation := ::System:WindowAnimation:SlideVertPositive
              ENDCASE
              ::Animate( 1000, AW_HIDE | nAnimation )
           ENDIF
           EXIT

      CASE WM_INITMENU
           EXIT

      CASE WM_COMMAND
           nCode := HIWORD( nwParam )
           nId   := ABS(LOWORD( nwParam ))
           nRet  := ExecuteEvent( "OnCommand", Self )

           IF nCode == 0
              nId := nwParam
           ENDIF
           IF nId == IDOK
              IF ( n := ASCAN( ::Children, {|o| o:__xCtrlName == "Button" .AND. o:DefaultButton } ) ) > 0
                 nId := ::Children[n]:Id
                 nlParam := ::Children[n]:hWnd
              ENDIF
           ENDIF

           nRet  := ::OnCommand( nId, nCode, nlParam )
           IF nRet == NIL .AND. ::AutoClose .AND. ::Style & WS_CHILD == 0 .AND. ::Modal
              IF nwParam == IDCANCEL

                 nRet := ExecuteEvent( "OnCancel", Self )
                 ODEFAULT nRet TO ::OnCancel()
                 ODEFAULT nRet TO 1
                 IF VALTYPE( nRet ) == "L"
                    nRet := IIF( nRet, 1, 0 )
                  ELSEIF VALTYPE( nRet ) == "O"
                    nRet := 1
                 ENDIF
                 IF nRet == 1
                    ::Close( IDCANCEL )
                    RETURN 1
                 ENDIF
                ELSEIF nwParam == IDOK

                 nRet := ExecuteEvent( "OnOk", Self )
                 ODEFAULT nRet TO ::OnOk()
              ENDIF
           ENDIF
           //------------------------- Search for Controls Actions ----------------------------
           IF nlParam != 0
              pPtr := GetProp( nlParam, "PROP_CLASSOBJECT" )
           ENDIF

           IF nCode == 0 .AND. pPtr != NIL .AND. pPtr != 0
              oCtrl := ArrayFromPointer( pPtr )

              IF oCtrl:__xCtrlName == "LinkLabel"
                 oCtrl:LinkVisited := .T.
              ENDIF
              lHandled := .F.

              IF __ObjHasMsg( oCtrl, "DropDown" ) .AND. oCtrl:DropDown == 3
                 RETURN 0
              ENDIF

              IF HGetPos( oCtrl:EventHandler, "OnClick" ) != 0
                 IF ::ClsName == "CCTL"
                    TRY
                       oForm := oCtrl:Form
                       nRet := oForm:&( oCtrl:EventHandler[ "OnClick" ] )( oCtrl )
                     CATCH
                       oForm := Self
                       nRet := oForm:&( oCtrl:EventHandler[ "OnClick" ] )( oCtrl )
                    END
                  ELSE
                    oForm := oCtrl:Form
                    nRet := oForm:&( oCtrl:EventHandler[ "OnClick" ] )( oCtrl )
                 ENDIF
                 lHandled := .T.
              END

              IF lHandled
                 IF nRet != NIL
                    RETURN nRet
                 ENDIF
                 RETURN CallWindowProc( ::__nProc, hWnd, nMsg, nwParam, nlParam )
              ENDIF

              IF nRet == NIL
                 nRet := oCtrl:OnClick( oCtrl )
              ENDIF

              IF nRet == NIL .AND. oCtrl:Action != NIL
                 nRet := __Evaluate( oCtrl:Action, oCtrl,,, nRet )
              ENDIF

              IF nRet == NIL
                 nRet := oCtrl:OnParentCommand( nId, nCode, nlParam )
              ENDIF
            ELSEIF pPtr != NIL .AND. pPtr != 0

              oCtrl := ArrayFromPointer( pPtr )
              nRet := oCtrl:OnParentCommand( nId, nCode, nlParam )
              IF nCode == CBN_SELENDOK .AND. oCtrl:__xCtrlName == "ToolStripComboBox"
                 ExecuteEvent( "OnCBNSelEndOk", oCtrl )
              ENDIF
           ENDIF
           ODEFAULT nRet TO ::OnParentCommand( nId, nCode, nlParam )

           IF nRet == NIL
              //--- notify children ----------------------------
              IF nCode == 1
                 FOR EACH oChild IN ::Children
                     IF oChild:Id == nId
                        IF HGetPos( oChild:EventHandler, "OnClick" ) != 0
                           oForm := oChild:Form
                           IF ::ClsName == "CCTL"
                              oForm := Self
                           ENDIF
                           nRet := oForm:&( oChild:EventHandler[ "OnClick" ] )( oChild )
                        ENDIF
                        IF nRet == NIL
                           nRet := oChild:OnClick( oChild )
                        ENDIF
                        IF nRet == NIL .AND. oChild:Action != NIL
                           IF nCode == CBN_SELENDOK .OR. oCtrl:ClsName != "ComboBox"
                              nRet := __Evaluate( oChild:Action, oChild,,, nRet )
                           ENDIF
                        ENDIF
                        EXIT
                     ENDIF

                     IF __ObjHasMsg( oChild, "OnParentCommand" )
                        nRet := oChild:OnParentCommand( nId, nCode, nlParam )
                        IF nRet != NIL
                           EXIT
                        ENDIF
                     ENDIF
                 NEXT
                 IF ::ContextMenu != NIL .AND. nlParam == 0 .AND. nId > 0
                    // Situation never contemplated: Accelerator from ContextMenu

                    IF ( oItem := ::ContextMenu:Menu:GetMenuById( nId ) ) != NIL
                       IF HGetPos( oItem:EventHandler, "OnClick" ) != 0
                          oForm := oItem:Form
                          IF ::ClsName == "CCTL"
                             oForm := Self
                          ENDIF
                          nRet := oForm:&( oItem:EventHandler[ "OnClick" ] )( oItem )
                        ELSEIF oItem:ClsName == "MenuStripItem" .AND. VALTYPE( oItem:Action ) == "B"
                          EVAL( oItem:Action, oItem )
                        ELSE
                          ODEFAULT nRet TO __Evaluate( oItem:Action, oItem,,, nRet )
                          oItem:OnClick( oItem )
                       ENDIF
                       oItem:Cancel()
                    ENDIF
                 ENDIF
              ENDIF
              IF nRet != NIL
                 RETURN nRet
              ENDIF
           ENDIF
           //---------------------------- Search for Menu Actions ------------------------------
           IF nRet == NIL
              IF ::ClsName == "ToolBarWindow32"
                 nRet := ExecuteEvent( "OnParentCommand", Self )
                 ODEFAULT nRet TO ::OnParentCommand( nId, nCode, nlParam )
              ENDIF
           ENDIF
           EXIT

      CASE WM_HELP
           ::HelpInfo  := (struct HELPINFO *) nlParam
           nRet := ExecuteEvent( "OnHelp", Self )
           EXIT

      CASE WM_HOTKEY
           nRet := ExecuteEvent( "OnHotKey", Self )
           ODEFAULT nRet TO ::OnHotKey( nwParam, nlParam )
           EXIT

      CASE WM_GETMINMAXINFO
           mmi  := (struct MINMAXINFO *) nlParam
           nRet := ExecuteEvent( "OnGetMinMaxInfo", Self )

           IF nRet == NIL
              IF ::MinWidth > 0
                 mmi:ptMinTrackSize:x := ::MinWidth
                 nRet := 0
              ENDIF
              IF ::MinHeight > 0
                 mmi:ptMinTrackSize:y := ::MinHeight
                 nRet := 0
              ENDIF

              IF ::MaxWidth > 0
                 mmi:ptMaxTrackSize:x := ::MaxWidth
                 nRet := 0
              ENDIF
              IF ::MaxHeight > 0
                 mmi:ptMaxTrackSize:y := ::MaxHeight
                 nRet := 0
              ENDIF
              IF nRet == 0
                 mmi:CopyTo( nlParam )
              ENDIF
           ENDIF
           EXIT

      CASE WM_PRINTCLIENT
           //DefWindowProc( hWnd, WM_PAINT, nwParam, nlParam )
           SendMessage( hWnd, WM_PAINT, nwParam, nlParam )
           nRet := ExecuteEvent( "OnPrintClient", Self )
           ODEFAULT nRet TO ::OnPrintClient( nwParam, nlParam )
           EXIT

      CASE WM_PRINT
           nRet := ExecuteEvent( "OnPrint", Self )
           ODEFAULT nRet TO ::OnPrint( nwParam, nlParam )
           EXIT

      CASE WM_PAINT
           IF nwParam != 0
              ::lParam := -1
              nRet := ExecuteEvent( "OnPaint", Self )
              ODEFAULT nRet TO ::OnPaint( , nwParam )
              IF nRet == NIL .AND. !::__lOnPaint .AND. !::__lOnWindowPaint .AND. ::__WindowStyle != WT_DIALOG
                 _FillRect( nwParam, { ::LeftMargin, ::TopMargin, ::ClientWidth, ::ClientHeight }, IIF( ::BkBrush != NIL, ::BkBrush, ::ClassBrush ) )
              ENDIF
              RETURN 0
           ENDIF
           IF ::__lOnWindowPaint
              nRet := ExecuteEvent( "OnWindowPaint", Self )
              ODEFAULT nRet TO ::OnWindowPaint( nwParam )
              IF nRet != NIL
                 RETURN nRet
              ENDIF
           ENDIF
           IF ::__lOnPaint
              ::Drawing:BeginPaint()
              nRet := ExecuteEvent( "OnPaint", Self )
              ODEFAULT nRet TO ::OnPaint( ::Drawing:hDC )
              ::Drawing:EndPaint()
           ENDIF

           IF ::__ClassInst != NIL .AND. ::Application != NIL
              TRY
                 ::Application:Project:CurrentForm:CtrlMask:InValidateRect( ::Application:Project:CurrentForm:GetSelRect() ,.f.)
               CATCH
              END
           ENDIF
           EXIT

      CASE WM_MENUGETOBJECT
           EXIT

      CASE WM_ERASEBKGND
           IF ::__WindowStyle != WT_DIALOG .AND. ::BkBrush != NIL .AND. ::ClsName != "Button" .AND. VALTYPE( ::BackColor ) == "N" .AND. ( ::BackSysColor != ::BackColor .OR. ::__ForceSysColor )
              SetBkColor( nwParam, ::BackColor )
           ENDIF
           nRet := ExecuteEvent( "OnEraseBkGnd", Self )
           ODEFAULT nRet TO ::OnEraseBkGnd( nwParam )
           IF nRet == NIL .AND. ::__WindowStyle != WT_DIALOG .AND. ::BkBrush != NIL .AND. ::ClsName != "Button" //Style & WS_CHILD == 0
              ::GetClientRect()
              IF ::__PaintBakgndImage( nwParam ) == NIL
                 _FillRect( nwParam, { ::LeftMargin, ::TopMargin, ::ClientWidth, ::ClientHeight }, ::BkBrush )
              ENDIF
              RETURN 1
           ENDIF
           DEFAULT nRet TO ::__PaintBakgndImage( nwParam )
           EXIT

      CASE WM_GETDLGCODE
           IF ! ( nlParam == 0 )
              //msg := (struct MSG*) nlParam
              aParams    := __GetMSG( nlParam )
              msg := {=>}
              msg:hwnd    := aParams[1]
              msg:message := aParams[2]
              msg:wParam  := aParams[3]
              msg:lParam  := aParams[4]
           ENDIF
           IF ::Parent != NIL
              nRet := ::Parent:OnChildGetDlgCode( msg )
              IF nRet != NIL
                 RETURN nRet
              ENDIF
           ENDIF
           nRet := ExecuteEvent( "OnGetDlgCode", Self )
           ODEFAULT nRet TO ::OnGetDlgCode( msg )
           ODEFAULT nRet TO __Evaluate( ::OnWMGetDlgCode, Self, msg, , nRet )
           EXIT

      CASE WM_VKEYTOITEM
           nRet := ExecuteEvent( "OnChar", Self )
           IF nRet == NIL
              pPtr := GetProp( nlParam, "PROP_CLASSOBJECT" )
              IF pPtr != NIL .AND. pPtr != 0
                 oCtrl := ArrayFromPointer( pPtr )
                 IF HGetPos( oCtrl:EventHandler, "OnChar" ) != 0
                    nRet := ::&( oCtrl:EventHandler[ "OnChar" ] )( Self )
                 ENDIF
              ENDIF
           ENDIF
           EXIT

      CASE WM_KEYDOWN
           IF ::Parent != NIL
              nRet := ::Parent:OnChildKeyDown( hWnd, nMsg, nwParam, nlParam )
              IF nRet != NIL
                 RETURN nRet
              ENDIF
           ENDIF
           nRet := ExecuteEvent( "OnKeyDown", Self, nwParam, nlParam )
           ODEFAULT nRet TO __Evaluate( ::OnWMKeyDown, Self, nwParam, nlParam, nRet )
           ODEFAULT nRet TO ::OnKeyDown( nwParam, nlParam )
           EXIT

      CASE WM_NEXTDLGCTL
           EXIT

      CASE WM_KEYUP
           IF ::hWnd == hWnd
              nRet := ExecuteEvent( "OnKeyUp", Self, nwParam, nlParam )
              ODEFAULT nRet TO ::OnKeyUp( nwParam, nlParam )
              ODEFAULT nRet TO __Evaluate( ::OnWMKeyUp, Self, nwParam, nlParam, nRet )
           ENDIF
           EXIT

      CASE WM_KILLFOCUS
           nRet := ::OnKillFocus( nwParam )
           ODEFAULT nRet TO __Evaluate( ::OnWMKillFocus, Self, nwParam, nlParam, nRet )
           ODEFAULT nRet TO nRet := ExecuteEvent( "OnKillFocus", Self )

           IF ::Parent != NIL .AND. ::Parent:ClsName == "PanelBox" .AND. ASCAN( ::Parent:Children, {|o| o:hWnd == nwParam } ) == 0
              ::Parent:oLastFocus := Self
              ::Parent:SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
           ENDIF

           EXIT

      CASE WM_SETFOCUS
           nRet := ExecuteEvent( "OnSetFocus", Self )
           ODEFAULT nRet TO ::OnSetFocus( nwParam )
           ODEFAULT nRet TO __Evaluate( ::OnWMSetFocus, Self, nwParam, nlParam, nRet )
           IF ::Parent != NIL .AND. ::Parent:ClsName == "PanelBox"
              ::Parent:SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
           ENDIF
           EXIT

      CASE WM_MEASUREITEM
           ::MeasureItemStruct := (struct MEASUREITEMSTRUCT*) nlParam
           //aParams := __GetMEASUREITEMSTRUCT( nlParam )
           //::MeasureItemStruct:CtlType    := aParams[1]
           //::MeasureItemStruct:CtlID      := aParams[2]
           //::MeasureItemStruct:itemID     := aParams[3]
           //::MeasureItemStruct:itemWidth  := aParams[4]
           //::MeasureItemStruct:itemHeight := aParams[5]
           //::MeasureItemStruct:itemData   := aParams[6]

           IF ::MeasureItemStruct:CtlType == ODT_MENU .AND. ::MeasureItemStruct:itemData != NIL .AND. ::MeasureItemStruct:itemData <> 0
              IF ( oCtrl := ArrayFromPointer( ::MeasureItemStruct:itemData ) ) != NIL
                 nRet := oCtrl:OnMeasureItem( nwParam, nlParam, ::MeasureItemStruct )
              ENDIF
            ELSE
              nRet := ExecuteEvent( "OnMeasureItem", Self )
              ODEFAULT nRet TO ::OnMeasureItem( nwParam, nlParam )

              IF nRet == NIL .AND. ( n := ASCAN( ::Children, {|o| o:Id == ::MeasureItemStruct:itemID} ) ) > 0
                 oCtrl := ::Children[n]
                 IF HGetPos( oCtrl:EventHandler, "OnParentMeasureItem" ) != 0
                    nRet := ::&( oCtrl:EventHandler[ "OnParentMeasureItem" ] )( Self )
                 ENDIF
                 ODEFAULT nRet TO oCtrl:OnParentMeasureItem(nwParam,nlParam, ::MeasureItemStruct)
              ENDIF

           ENDIF
           EXIT

      CASE WM_DRAWITEM
           ::DrawItemStruct := (struct DRAWITEMSTRUCT*) nlParam
           /*
           rcItem := Array(4)
           DrawItemStructure( nlParam, @CtlType, @CtlID, @itemID, @itemAction, @itemState, @hWndItem, @hDC, @rcItem, @itemData )

           ::DrawItemStruct:CtlType       := CtlType
           ::DrawItemStruct:CtlID         := CtlID
           ::DrawItemStruct:itemID        := itemID
           ::DrawItemStruct:itemAction    := itemAction
           ::DrawItemStruct:itemState     := itemState
           ::DrawItemStruct:hWndItem      := hWndItem
           ::DrawItemStruct:hDC           := hDC
           ::DrawItemStruct:rcItem:Left   := rcItem[1]
           ::DrawItemStruct:rcItem:Top    := rcItem[2]
           ::DrawItemStruct:rcItem:Right  := rcItem[3]
           ::DrawItemStruct:rcItem:Bottom := rcItem[4]
           ::DrawItemStruct:rcItem:Array  := rcItem
           ::DrawItemStruct:itemData      := itemData
           */
           IF ::DrawItemStruct:CtlType == ODT_MENU .AND. ::DrawItemStruct:itemData != NIL .AND. ::DrawItemStruct:itemData <> 0
              IF ( oCtrl := ArrayFromPointer( ::DrawItemStruct:itemData ) ) != NIL .AND. VALTYPE( oCtrl ) == "O"
                 nRet := oCtrl:OnDrawItem( nwParam, nlParam, ::DrawItemStruct )
              ENDIF
            ELSE

              nRet := ExecuteEvent( "OnDrawItem", Self )
              ODEFAULT nRet TO ::OnDrawItem( nwParam, nlParam )
              IF nRet == NIL
                 pPtr := GetProp( ::DrawItemStruct:hwndItem, "PROP_CLASSOBJECT" )
                 IF pPtr != NIL .AND. pPtr != 0
                    oCtrl := ArrayFromPointer( pPtr )

                    IF HGetPos( oCtrl:EventHandler, "OnParentDrawItem" ) != 0
                       nRet := ::&( oCtrl:EventHandler[ "OnParentDrawItem" ] )( Self )
                    ENDIF
                    ODEFAULT nRet TO oCtrl:OnParentDrawItem(nwParam,nlParam, ::DrawItemStruct)
                 ENDIF
              ENDIF

           ENDIF
           EXIT

      CASE WM_ENTERSIZEMOVE
           nRet := ExecuteEvent( "OnEnterSizeMove", Self )
           ODEFAULT nRet TO ::OnEnterSizeMove()
           EXIT

      CASE WM_EXITSIZEMOVE
           nRet := ExecuteEvent( "OnExitSizeMove", Self )
           ODEFAULT nRet TO ::OnExitSizeMove()
           EXIT

      CASE WM_DRAWCLIPBOARD
           nRet := ExecuteEvent( "OnDrawClipboard", Self )
           ODEFAULT nRet TO ::OnDrawClipboard( nwParam, nlParam )
           EXIT

      CASE WM_CHANGECBCHAIN
           nRet := ExecuteEvent( "OnChangeCbChain", Self )
           ODEFAULT nRet TO ::OnChangeCbChain( nwParam, nlParam )
           EXIT

      CASE WM_MOUSEWHEEL
           nRet := ::OnMouseWheel( nwParam, nlParam )
           IF nRet == NIL .AND. ::sih != NIL .OR. ::siv != NIL
              pt := (struct POINT)
              pt:x := LOWORD( nlParam )
              pt:y := HIWORD( nlParam )

              nRet := ExecuteEvent( "OnMouseWheel", Self )

              IF ( !::__IsControl .OR. ::__xCtrlName == "DataGrid" .OR. !::__IsStandard )
                 IF nRet == NIL .AND. ::HasMessage( "VertScroll" ) .AND. ::VertScroll .OR. ( ::HasMessage( "AutoVertScroll" ) .AND. ::AutoVertScroll .AND. ::siv != NIL .AND. ::siv:nMax > 0 )
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
                       nPage   := IIF( ::siv != NIL, ::siv:nPage, ::ClientHeight )
                       IF ::sih != NIL .AND. PtInRect( rc, pt )
                          nScroll := WM_HSCROLL
                          nPage   := ::sih:nPage
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
                 ENDIF
              ENDIF
           ENDIF
           EXIT

      CASE WM_HSCROLL
           nRet := ExecuteEvent( "OnHorzScroll", Self )
           ODEFAULT nRet TO ::OnHorzScroll( LoWord( nwParam ), HiWord( nwParam ), nlParam )

           IF nRet == NIL .AND. ( ::HorzScroll .OR. ::AutoHorzScroll ) .AND. ( !::__IsControl .OR. ::__xCtrlName == "DataGrid" .OR. !::__IsStandard )
              nRet := ExecuteEvent( "OnHScroll", Self )
              nRet := ::OnHScroll( LoWord( nwParam ), HiWord( nwParam ), nlParam )
           ENDIF
           EXIT

      CASE WM_VSCROLL

           IF ( ::VertScroll .OR. ::AutoVertScroll ) .AND. ( !::__IsControl .OR. ::__xCtrlName == "DataGrid" .OR. !::__IsStandard )

              IF LOWORD( nwParam ) == SB_THUMBTRACK
                 // workaround to beat the 16 bits thumb position
                 ::ScrollInfo := (struct SCROLLINFO)
                 cBuffer := _GetScrollInfo( hWnd, SB_VERT )
                 ::ScrollInfo:Buffer( cBuffer, .T. )
                 nRet := ExecuteEvent( "OnVertScroll", Self )
                 IF nRet == NIL
                    nRet := ::OnVertScroll( LoWord( nwParam ), ::ScrollInfo:nTrackPos, nlParam )
                 ENDIF
                 IF nRet == NIL
                    nRet := ::OnVScroll( LoWord( nwParam ), ::ScrollInfo:nTrackPos, nlParam )
                 ENDIF
               ELSE
                 nRet := ExecuteEvent( "OnVertScroll", Self )
                 IF nRet == NIL
                    nRet := ::OnVertScroll( LoWord( nwParam ), HiWord( nwParam ), nlParam )
                 ENDIF
                 IF nRet == NIL
                    nRet := ::OnVScroll( LoWord( nwParam ), HiWord( nwParam ), nlParam )
                 ENDIF
              ENDIF
            ELSE
              nRet := ExecuteEvent( "OnVertScroll", Self )
              ODEFAULT nRet TO ::OnVertScroll( LoWord( nwParam ), HiWord( nwParam ), nlParam )
           ENDIF

           EXIT

      CASE WM_SYSCOMMAND
           IF ::ClsName != "DataGrid"
              FOR EACH oChild IN ::Children

                  nRet := ExecuteEvent( "OnParentSysCommand", oChild )
                  TRY
                     ODEFAULT nRet TO oChild:OnParentSysCommand( nwParam, nlParam )
                   catch
                  END
                  IF nRet != NIL
                     RETURN nRet
                  ENDIF
              NEXT
           ENDIF
           nRet := ExecuteEvent( "OnSysCommand", Self )
           ODEFAULT nRet TO ::OnSysCommand( nwParam, nlParam )
           ODEFAULT nRet TO __Evaluate( ::OnWMSysCommand,  Self, nwParam, nlParam, nRet )

           IF nwParam != SC_CLOSE
              ::PostMessage( WM_USER + 3025 )
           ENDIF
           EXIT

      CASE WM_SYSDEADCHAR
           EXIT

      CASE WM_CTLCOLORSCROLLBAR
           nRet := ExecuteEvent( "OnCtlColorScrollBar", Self )
           ODEFAULT nRet TO ::OnCtlColorScrollBar( nwParam, nlParam )
           IF ( n := ASCAN( ::Children, {|o|o:hWnd == nlParam} ) ) > 0
              nRet := ::Children[n]:OnCtlColorScrollBar( nwParam, nlParam )
           ENDIF
           EXIT

      CASE WM_CTLCOLORBTN
           nRet := ExecuteEvent( "OnCtlColorBtn", Self )
           ODEFAULT nRet TO ::OnCtlColorBtn( nwParam, nlParam )

           pPtr := GetProp( nlParam, "PROP_CLASSOBJECT" )
           IF pPtr != NIL .AND. pPtr != 0
              oCtrl := ArrayFromPointer( pPtr )
              nRet := oCtrl:OnCtlColorBtn( nwParam, nlParam )
           ENDIF
           EXIT

      CASE WM_CTLCOLORSTATIC
           nRet := ExecuteEvent( "OnCtlColorStatic", Self )
           ODEFAULT nRet TO ::OnCtlColorStatic( nwParam, nlParam )
           IF nRet == NIL
              pPtr := GetProp( nlParam, "PROP_CLASSOBJECT" )
              IF pPtr != NIL .AND. pPtr != 0
                 oCtrl := ArrayFromPointer( pPtr )
                 nRet := oCtrl:OnCtlColorStatic( nwParam, nlParam )

                 //IF ( n := ASCAN( ::Children, {|o|o:hWnd == nlParam} ) ) > 0
                 //   nRet := ::Children[n]:OnCtlColorStatic( nwParam, nlParam )
              ENDIF
           ENDIF
           EXIT

      CASE WM_CTLCOLOREDIT
           nRet := ExecuteEvent( "OnCtlColorEdit", Self )
           ODEFAULT nRet TO ::OnCtlColorEdit( nwParam, nlParam )
           IF nRet == NIL
              IF ( n := ASCAN( ::Children, {|o|o:hWnd == nlParam} ) ) > 0
                 nRet := ::Children[n]:OnCtlColorEdit( nwParam, nlParam )
              ENDIF
           ENDIF
           EXIT

      CASE WM_CTLCOLORDLG
           nRet := ExecuteEvent( "OnCtlColorDlg", Self )
           ODEFAULT nRet TO ::OnCtlColorDlg( nwParam, nlParam )
           IF nRet == NIL .AND. ::BkBrush != NIL
              RETURN( ::BkBrush )
           ENDIF
           EXIT

      CASE WM_CTLCOLORLISTBOX
           nRet := ExecuteEvent( "OnCtlColorListBox", Self )
           ODEFAULT nRet TO ::OnCtlColorListBox( nwParam, nlParam )
           IF nRet == NIL
              IF ( n := ASCAN( ::Children, {|o|o:hWnd == nlParam} ) ) > 0
                 nRet := ::Children[n]:OnCtlColorListBox( nwParam, nlParam )
              ENDIF
           ENDIF
           EXIT

      CASE WM_NOTIFY
           __GetNMHDR( nlParam, @hwndFrom, @idFrom, @code )
           ::hdr:hwndFrom := hwndFrom
           ::hdr:idFrom   := idFrom
           ::hdr:code     := code

           //::hdr := (struct NMHDR)
           //::hdr:Pointer( nlParam )

           nRet := ExecuteEvent( "OnNotify", Self )
           ODEFAULT nRet TO ::OnNotify( nwParam, nlParam, ::hdr )

           IF nRet == NIL
              pPtr := GetProp( ::hdr:hwndFrom, "PROP_CLASSOBJECT" )
              IF pPtr != NIL .AND. pPtr != 0
                 oCtrl := ArrayFromPointer( pPtr )
                 IF HGetPos( oCtrl:EventHandler, "OnParentNotify" ) != 0
                    nRet := ::&( oCtrl:EventHandler[ "OnParentNotify" ] )( oCtrl )
                 END
                 ODEFAULT nRet TO oCtrl:OnParentNotify( nwParam, nlParam, ::hdr )
                 IF VALTYPE( nRet ) == "O"
                    nRet := NIL
                 ENDIF
              ENDIF

              IF nRet == NIL .AND. ::hdr:code == TTN_NEEDTEXT
                 IF ::ClsName != "DataGrid"
                    FOR EACH oChild IN ::Children
                        IF HGetPos( oChild:EventHandler, "OnToolTipNotify" ) != 0
                           nRet := ::&( oChild:EventHandler[ "OnToolTipNotify" ] )( oChild )
                        END
                        IF __objHasMsg( oChild, "OnToolTipNotify" )
                           ODEFAULT nRet TO oChild:OnToolTipNotify( nwParam, nlParam, ::hdr )
                        ENDIF
                    NEXT
                    IF VALTYPE( nRet ) == "O"
                       nRet := NIL
                    ENDIF
                 ENDIF
              ENDIF

           ENDIF
           EXIT

      CASE WM_NCPAINT
           nRet := ExecuteEvent( "OnNCPaint", Self )
           ODEFAULT nRet TO ::OnNCPaint( nwParam, nlParam )
           EXIT

      CASE WM_WINDOWPOSCHANGED
           aParams := __GetWINDOWPOS( nlParam )
           ::WindowPos:hwnd            := aParams[1]
           ::WindowPos:hwndInsertAfter := aParams[2]
           ::WindowPos:x               := aParams[3]
           ::WindowPos:y               := aParams[4]
           ::WindowPos:cx              := aParams[5]
           ::WindowPos:cy              := aParams[6]
           ::WindowPos:flags           := aParams[7]

           //::WindowPos := (struct WINDOWPOS)
           //::WindowPos:Pointer( nlParam )

           IF ::Parent != NIL
              rc := (struct RECT)

              aRect := _GetWindowRect( hWnd )

              rc:Left   := aRect[1]
              rc:Top    := aRect[2]
              rc:Right  := aRect[3]
              rc:Bottom := aRect[4]

              // Temporary workaround with Fast API
              aPt := { rc:left, rc:top }

              IF ::__ClassInst != NIL .AND. ::Parent:hWnd != GetParent( hWnd )
                 _ScreenToClient( GetParent( hWnd ), aPt )
                 ::__TempRect := { aPt[1], aPt[2], aPt[1]+::WindowPos:cx, aPt[2]+::WindowPos:cy }
                 RETURN NIL
              ENDIF

              _ScreenToClient( ::Parent:hWnd, aPt )
              ::__TempRect := { aPt[1], aPt[2], aPt[1]+::WindowPos:cx, aPt[2]+::WindowPos:cy }
              ::xLeft   := aPt[1] //+ ::Parent:HorzScrollPos
              ::xTop    := aPt[2] //+ ::Parent:VertScrollPos

              IF ::ClsName == "ListBox"
                 IF ::WindowPos:cy != ::xHeight
                    RETURN 0
                 ENDIF
              ENDIF
              //-----------------------------------

            ELSE
              ::xLeft   := ::WindowPos:x
              ::xTop    := ::WindowPos:y
           ENDIF
           ::xWidth  := ::WindowPos:cx

           IF ::ClsName != "ComboBox"
              ::xHeight := ::WindowPos:cy
           ENDIF

           nRet := ExecuteEvent( "OnWindowPosChanged", Self )
           ODEFAULT nRet TO ::OnWindowPosChanged( nwParam, nlParam )

           IF ::__xCtrlName != "Splitter"
              lShow := ::WindowPos:flags & SWP_SHOWWINDOW == SWP_SHOWWINDOW
              lHide := ::WindowPos:flags & SWP_HIDEWINDOW == SWP_HIDEWINDOW

              IF lHide
                 ::Hidden := .T.
               ELSEIF lShow
                 ::Hidden := .F.
              ENDIF

              IF lShow .OR. lHide
                 IF ::IsChild .AND. ::Parent != NIL .AND. ::Parent:ClsName != "DataGrid"
                    hDef := BeginDeferWindowPos( LEN( ::Parent:Children ) )
                    FOR EACH oChild IN ::Parent:Children
                        IF oChild != NIL .AND. oChild:hWnd != ::hWnd
                           oChild:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight, @hDef )
                           oChild:InvalidateRect(, .F. )
                        ENDIF
                    NEXT
                    EndDeferWindowPos( hDef )
                    IF ::LeftSplitter != NIL
                       ::LeftSplitter:__OnParentSize( x, y, @hDef )
                    ENDIF
                    IF ::TopSplitter != NIL
                       ::TopSplitter:__OnParentSize( x, y, @hDef )
                    ENDIF
                    IF ::RightSplitter != NIL
                       ::RightSplitter:__OnParentSize( x, y, @hDef )
                    ENDIF
                    IF ::BottomSplitter != NIL
                       ::BottomSplitter:__OnParentSize( x, y, @hDef )
                    ENDIF
                    RETURN 0
                 ENDIF
              ENDIF
           ENDIF
           EXIT

      CASE WM_WINDOWPOSCHANGING
           //::WindowPos := NIL
           //::WindowPos := (struct WINDOWPOS*) nlParam

           aParams := __GetWINDOWPOS( nlParam )
           ::WindowPos:hwnd            := aParams[1]
           ::WindowPos:hwndInsertAfter := aParams[2]
           ::WindowPos:x               := aParams[3]
           ::WindowPos:y               := aParams[4]
           ::WindowPos:cx              := aParams[5]
           ::WindowPos:cy              := aParams[6]
           ::WindowPos:flags           := aParams[7]

           nRet := ExecuteEvent( "OnWindowPosChanging", Self )
           ODEFAULT nRet TO ::OnWindowPosChanging( nwParam, nlParam )
           IF ::Parent != NIL .AND. ::ClsName == "MDIChild" .AND. ::WindowPos:flags != 20
              IF ( n := ASCAN( ::Parent:Parent:Children, {|o|o:__xCtrlName == "CoolMenu"} ) ) > 0
                 PostMessage( ::Parent:hWnd, WM_MDICHILDSIZED, n )
                 RETURN 0
              ENDIF
           ENDIF
           RETURN NIL

      CASE WM_THEMECHANGED
           IF ! ( ::ClsName == TOOLTIPS_CLASS )
              IF ::Application != NIL .AND. ::Application:MainForm:hWnd == hWnd
                 ::Application:ThemeActive := IsThemeActive()
                 ::System:UpdateColorSchemes()
                 ::System:Update()
              ENDIF
              IF ::hTheme != NIL
                 ::CloseThemeData()
                 ::OpenThemeData()
              ENDIF
              nRet := ExecuteEvent( "OnThemeChanged", Self )
              ODEFAULT nRet TO ::OnThemeChanged( Self )
              ODEFAULT nRet TO __Evaluate( ::OnWMThemeChanged,  Self, nwParam, nlParam, nRet )
           ENDIF
           EXIT

      CASE WM_SYSCOLORCHANGE
           IF ! ( ::ClsName == TOOLTIPS_CLASS )
              nRet := ExecuteEvent( "OnSysColorChange", Self )
              ODEFAULT nRet TO ::OnSysColorChange( nwParam, nlParam )
              ODEFAULT nRet TO __Evaluate( ::OnWMSysColorChange,  Self, nwParam, nlParam, nRet )
           ENDIF
           EXIT

      CASE WM_SYSCHAR
           nRet := ExecuteEvent( "OnSysChar", Self )
           ODEFAULT nRet TO ::OnSysChar( nwParam, nlParam )

           IF nRet == NIL
              hParent := GetParent( hWnd )
              IF hParent != NIL
                 PostMessage( hParent, nMsg, nwParam, nlParam )
              ENDIF
              RETURN 0
           ENDIF
           EXIT

      CASE WM_SYSDEADCHAR
           EXIT

      CASE WM_SYSKEYDOWN
           nRet := ExecuteEvent( "OnSysKeyDown", Self )
           ODEFAULT nRet TO ::OnSysKeyDown( nwParam, nlParam )
           IF nRet == NIL .AND. ::Form != NIL
              FOR EACH oChild IN ::Form:Children
                  IF oChild:__xCtrlName IN {"CoolMenu","MenuStrip","ToolStripContainer"}
                     nRet := oChild:OnSysKeyDown( nwParam, nlParam )
                     IF nRet != NIL
                        RETURN nRet
                     ENDIF
                  ENDIF
              NEXT
           ENDIF
           EXIT

      CASE WM_SYSKEYUP
           nRet := ExecuteEvent( "OnSysKeyUp", Self )
           ODEFAULT nRet TO ::OnSysKeyUp( nwParam, nlParam )
           EXIT

      CASE WM_SIZING
           nRet := ExecuteEvent( "OnSizing", Self )
           ODEFAULT nRet TO ::OnSizing( nwParam, nlParam )
           RETURN 0

      CASE WM_SHOWWINDOW
           DO CASE
              CASE nwParam == 0 // Hide
                   nRet := ExecuteEvent( "OnHideWindow", Self )
                   ODEFAULT nRet TO ::OnHideWindow( nlParam )
                   ::Hidden := .T.
              CASE nwParam == 1 // Show
                   nRet := ExecuteEvent( "OnShowWindow", Self )
                   ODEFAULT nRet TO ::OnShowWindow( nlParam )
                   ODEFAULT nRet TO __Evaluate( ::OnWMShowWindow,  Self, nwParam, nlParam, nRet )
                   ::Hidden := .F.
              OTHER
                  RETURN 0
           ENDCASE
           IF nRet == NIL .AND. ::IsChild .AND. ::Parent != NIL .AND. ::AutoDock .AND. ::__xCtrlName != "MDIClient"
              ::Parent:SendMessage( 4, 0, MAKELONG( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
           ENDIF
           EXIT

      CASE WM_ENABLE
           nRet := ExecuteEvent( "OnEnable", Self )
           ODEFAULT nRet TO ::OnEnable( nwParam, nlParam )
           EXIT

      CASE WM_CONTEXTMENU
           nRet := ExecuteEvent( "OnContextMenu", Self )
           ODEFAULT nRet TO ::OnContextMenu( LOWORD(nlparam) , HIWORD(nlparam) )
           oObj := ObjFromHandle( nwParam )

           IF oObj != NIL .AND. VALTYPE( nRet ) $ "UO"
              IF oObj:__xCtrlName == "DataGrid"
                 pt := (struct POINT)
                 pt:x := LOWORD(nlparam)
                 pt:y := 0
                 ClientToScreen( oObj:Form:hWnd, @pt ) // BUG, it has to test for HWND
                 ScreenToClient( nwParam, @pt )
                 
                 IF ( n := oObj:ColFromPos( pt:x ) ) > 0

                    nRet := ExecuteEvent( "OnContextMenu", oObj:Children[ n ] )

                    IF VALTYPE( nRet ) $ "UO" .AND. oObj:Children[ n ]:ContextMenu != NIL
                       oObj := oObj:Children[ n ]
                    ENDIF

                 ENDIF
              ENDIF
              IF oObj:ContextMenu != NIL .AND. VALTYPE( nRet ) $ "UO"
                 oObj:ContextMenu:Show( LOWORD(nlparam) , HIWORD(nlparam) )
                 RETURN 0
              ENDIF
           ENDIF
           EXIT

      CASE WM_SETTEXT
           nRet := ExecuteEvent( "OnSetText", Self )
           ODEFAULT nRet TO ::OnSetText( nwParam, nlParam )
           EXIT

      CASE WM_GETTEXT
           nRet := ExecuteEvent( "OnGetText", Self )
           ODEFAULT nRet TO ::OnGetText( nwParam, nlParam )
           EXIT

      CASE WM_CANCELMODE
           nRet := ExecuteEvent( "OnCancelMode", Self )
           ODEFAULT nRet TO ::OnCancelMode( nwParam, nlParam )
           EXIT

      CASE WM_MENUCOMMAND
           nRet := ExecuteEvent( "OnMenuCommand", Self )
           ODEFAULT nRet TO ::OnMenuCommand( nwParam, nlParam )
           IF ::Application != NIL .AND. ::Application:OsVersion:dwMajorVersion < 5
              oItem := ::Application:oCurMenu:GetMenuById( LOWORD( nwParam ) )
            ELSEIF ::Application != NIL

              TRY
                 mii := (struct MENUITEMINFO)
                 mii:cbSize := mii:SizeOf()
                 mii:fMask  := MIIM_DATA
                 _GetMenuItemInfo( nlParam, nwParam, .T., mii:Value )
                 mii:Devalue()
                 IF mii:dwItemData != NIL .AND. mii:dwItemData <> 0
                    oItem := ArrayFromPointer( mii:dwItemData )
                 ENDIF
               CATCH
              END
              IF oItem == NIL
                 TRY
                    IF ( oMenu := ::Application:oCurMenu:GetMenuByHandle( nlParam ) ) != NIL
                       oItem := oMenu:aItems[ nwParam + 1 ]
                    ENDIF
                  CATCH
                 END
              ENDIF
           ENDIF
           IF oItem != NIL
              IF HGetPos( oItem:EventHandler, "OnClick" ) != 0
                 oForm := oItem:Form
                 IF ::ClsName == "CCTL"
                    oForm := Self
                 ENDIF
                 nRet := oForm:&( oItem:EventHandler[ "OnClick" ] )( oItem )
               ELSEIF oItem:ClsName == "MenuStripItem" .AND. VALTYPE( oItem:Action ) == "B"
                 EVAL( oItem:Action, oItem )
               ELSE
                 ODEFAULT nRet TO __Evaluate( oItem:Action, oItem,,, nRet )
                 oItem:OnClick( oItem )
              ENDIF
              oItem:Cancel()
           ENDIF
           oItem := NIL
           EXIT

      CASE WM_NEXTMENU
           nRet := ExecuteEvent( "OnNextMenu", Self )
           //nm := (struct MDINEXTMENU*) nlParam
           //nRet := ::OnNextMenu( nwParam, nlParam, nm )
           EXIT

      CASE WM_ENTERMENULOOP
           nRet := ExecuteEvent( "OnEnterMenuLoop", Self )
           ODEFAULT nRet TO ::OnEnterMenuLoop( nwParam, nlParam )
           EXIT

      CASE WM_EXITMENULOOP
           nRet := ExecuteEvent( "OnExitMenuLoop", Self )
           ODEFAULT nRet TO ::OnExitMenuLoop( nwParam, nlParam )
           EXIT

      CASE WM_UNINITMENUPOPUP
           nRet := ExecuteEvent( "OnUnInitMenuPopup", Self )
           EXIT

      CASE WM_MENUCHAR
           nRet := ExecuteEvent( "OnMenuChar", Self )
           ODEFAULT nRet TO ::OnMenuChar( nwParam, nlParam )
           EXIT

      CASE WM_PASTE
           nRet := ExecuteEvent( "OnPaste", Self )
           ODEFAULT nRet TO ::OnPaste( nwParam, nlParam )
           ODEFAULT nRet TO __Evaluate( ::OnWMPaste, Self, nwParam, nlParam, nRet )
           EXIT

      CASE WM_COPY
           nRet := ExecuteEvent( "OnCopy", Self )
           ODEFAULT nRet TO ::OnCopy( nwParam, nlParam )
           EXIT

      CASE WM_CUT
           nRet := ExecuteEvent( "OnCut", Self )
           ODEFAULT nRet TO ::OnCut( nwParam, nlParam )
           EXIT

      CASE WM_CLEAR
           nRet := ExecuteEvent( "OnClear", Self )
           ODEFAULT nRet TO ::OnClear( nwParam, nlParam )
           EXIT

      CASE WM_UNDO
           nRet := ExecuteEvent( "OnUndo", Self )
           ODEFAULT nRet TO ::OnUndo( nwParam, nlParam )
           EXIT

      CASE WM_MDICHILDSIZED
           lShow := ::GetWindowLong( GWL_STYLE ) & WS_MAXIMIZE != 0
           ::Parent:Children[nwParam]:UpdateMenu( lShow )
           RETURN 1

      DEFAULT
           IF nMsg >= WM_USER
              IF nMsg == WM_USER + 3025
                 ::ShowMode := ::__GetShowMode()
               ELSEIF LOWORD( nlParam ) == WM_RBUTTONDOWN
                 FOR EACH oObj IN ::Components
                     IF oObj:__xCtrlName == "NotifyIcon"
                        IF oObj:ContextMenu != NIL
                           pt := (struct POINT)
                           GetCursorPos( @pt )
                           oObj:ContextMenu:Show( pt:x , pt:y )
                        ENDIF
                        nRet := ExecuteEvent( "OnRButtonDown", oObj )
                        EXIT
                     ENDIF
                 NEXT
               ELSEIF LOWORD( nlParam ) == WM_LBUTTONDOWN
                 FOR EACH oObj IN ::Components
                     IF oObj:__xCtrlName == "NotifyIcon"
                        nRet := ExecuteEvent( "OnLButtonDown", oObj )
                        EXIT
                     ENDIF
                 NEXT
               ELSEIF LOWORD( nlParam ) == WM_LBUTTONUP
                 FOR EACH oObj IN ::Components
                     IF oObj:__xCtrlName == "NotifyIcon"
                        nRet := ExecuteEvent( "OnLButtonUp", oObj )
                        EXIT
                     ENDIF
                 NEXT
               ELSEIF LOWORD( nlParam ) == WM_RBUTTONUP
                 FOR EACH oObj IN ::Components
                     IF oObj:__xCtrlName == "NotifyIcon"
                        nRet := ExecuteEvent( "OnRButtonUp", oObj )
                        EXIT
                     ENDIF
                 NEXT
               ELSE
                 nRet := ExecuteEvent( "OnUserMsg", Self )
                 ODEFAULT nRet TO  ::OnUserMsg( hWnd, nMsg, nwParam, nlParam)
              ENDIF
            ELSE
              nRet := ExecuteEvent( "OnMessage", Self )
              ODEFAULT nRet TO ::OnMessage( nMsg, nwParam, nlParam)
           ENDIF
           EXIT
   END
   IF VALTYPE( nRet ) == "O"
      nRet := NIL
   ENDIF
   IF nRet != NIL
      IF ::Modal
         IF VALTYPE( nRet ) == "L"
            nRet := IIF( nRet, 1, 0 )
         ENDIF
         SetWindowLong( ::hWnd, DWL_MSGRESULT, nRet )
      ENDIF
      RETURN( nRet )
   ENDIF

   IF nMsg != WM_SIZE .AND. ::xMdiContainer .AND. ::MDIClient != NIL .AND. ::MDIClient:hWnd != NIL
      RETURN DefFrameProc( hWnd, ::MDIClient:hWnd, nMsg, nwParam, nlParam )
   ENDIF

   IF ! Empty( ::__nProc )
      RETURN CallWindowProc( ::__nProc, hWnd, nMsg, nwParam, nlParam )
   ENDIF

   IF ::xMdiContainer .AND. ::MDIClient != NIL .AND. ::MDIClient:hWnd != NIL
      RETURN DefFrameProc( hWnd, ::MDIClient:hWnd, nMsg, nwParam, nlParam )
   ENDIF
RETURN ::__WinProc(hWnd, nMsg, nwParam, nlParam)

//-----------------------------------------------------------------------------------------------

METHOD __WindowDestroy() CLASS Window

   LOCAL aProperty, aProperties, n, oObj, hDef, lRedraw := .F.

   IF ::Parent != NIL .AND. !(::Parent:__xCtrlName == "CoolBar")
      IF ( n := ASCAN( ::Parent:Children, {|o|o:hWnd == ::hWnd} ) ) > 0
         ADEL( ::Parent:Children, n, .T. )
      ENDIF
   ENDIF

   IF ::Drawing != NIL
      ::Drawing:Destroy()
   ENDIF

   IF ::__hMemBitmap != NIL
      DeleteObject( ::__hMemBitmap )
   ENDIF

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
   ENDIF

   IF ::SelBkBrush != NIL
      DeleteObject( ::SelBkBrush )
   ENDIF

   IF ::SmallCaptionFont != NIL .AND. VALTYPE( ::SmallCaptionFont ) == "O"
      ::SmallCaptionFont:Delete()
      ::SmallCaptionFont := NIL
   ENDIF

   IF ::Parent != NIL .AND. ::ClsName == "MDIChild"
      IF ( n := ASCAN( ::Parent:Children, {|o|o:__xCtrlName == "CoolMenu"} ) ) > 0
         IF ::Parent:Children[n]:aItems[1]:Id == IDM_MDI_ICON
            ::Parent:Children[n]:aItems[1]:Delete()
            ATAIL(::Parent:Children[n]:aItems):Delete()
            ATAIL(::Parent:Children[n]:aItems):Delete()
            ATAIL(::Parent:Children[n]:aItems):Delete()
            ATAIL(::Parent:Children[n]:aItems):Delete()
            ::Parent:Children[n]:InvalidateRect()
         ENDIF
      ENDIF
   ENDIF
   IF ::Form != NIL .AND. ::Name != NIL
      TRY
         HDel( ::Form:Property, ::xName )
      CATCH
      END
   ENDIF
   IF !EMPTY( ::__hIcon )
      DestroyIcon( ::__hIcon )
   ENDIF
   IF ::ToolTip != NIL
      ::ToolTip:Destroy()
      ::ToolTip := NIL
   ENDIF
   IF VALTYPE( ::Font ) == "O" .AND. !::Font:Shared
      ::Font:Delete()
      IF ::Font:FileName != NIL
         RemoveFontResource( ::Font:FileName )//, FR_PRIVATE | FR_NOT_ENUM )
      ENDIF
      ::Font:ncm := NIL
      ::Font := NIL
   ENDIF

   ::siv               := NIL
   ::sih               := NIL
   ::DrawItemStruct    := NIL
   ::MeasureItemStruct := NIL
   ::ScrollInfo        := NIL
   ::hdr               := NIL
   ::WindowPos         := NIL

   ::Msg               := NIL
   ::wParam            := NIL
   ::lParam            := NIL
   ::Drawing           := NIL

   IF ::__ClassInst != NIL .AND. ::Parent != NIL
      TRY
         FOR n := 1 TO LEN( ::Parent:Children )
             ::Parent:Children[n]:xTabOrder := n
             ::Parent:Children[n]:__ClassInst:xTabOrder := n
         NEXT
      CATCH
      END
   ENDIF

   ::__ClassInst := NIL
   ::Property := NIL
   ::Children := NIL
   IF UPPER( ::ClassName ) IN { "WINFORM", "TABPAGE", "WINDOWEDIT" } .AND. ::BackgroundImage != NIL
      ::BackgroundImage:Destroy()
      ::BackgroundImage := NIL
   ENDif
   ::Dock := NIL
   ::Anchor := NIL

RETURN 0

//----------------------------------------------------------------------------------------------------

METHOD CenterWindow( lDesk ) CLASS Window
   LOCAL oWin, aRect
   DEFAULT lDesk TO .F.
   IF ::hWnd == NIL .OR. ::__ClassInst != NIL
      RETURN Self
   ENDIF
   IF ::Parent != NIL .AND. !lDesk
      ::GetWindowRect()

      IF ::IsChild()
         aRect := _GetClientRect( ::Parent:hWnd )
         ::xLeft := ( ( aRect[3] - ::xWidth ) / 2 )
         ::xTop  := ( ( aRect[4] - ::xHeight ) / 2 )
       ELSE
         aRect := _GetWindowRect( ::Parent:hWnd )
         ::xLeft := ( ( aRect[1] + aRect[3] ) / 2 ) - ( ::xWidth / 2 )
         ::xTop  := ( ( aRect[2] + aRect[4] ) / 2 ) - ( ::xHeight / 2 )
      ENDIF
    ELSE
      IF ::__hParent != NIL
         oWin  := ::__hParent
       ELSE
         oWin  := GetDeskTopWindow()
      ENDIF
      aRect := _GetWindowRect( oWin )
      ::GetWindowRect()
      ::xLeft := ( ( aRect[1] + aRect[3] ) / 2 ) - ( ::xWidth / 2 )
      ::xTop  := ( ( aRect[2] + aRect[4] ) / 2 ) - ( ::xHeight / 2 )
   ENDIF
   ::MoveWindow()
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD OnMeasureItem( nwParam, nlParam ) CLASS Window
   LOCAL n, oItem, oButton, oSub, oMenu

   IF ::Application != NIL
      oMenu := ::Application:oCurMenu
   ENDIF

//   IF ( n := ASCAN( ::Components, {|o| o:__xCtrlName=="ContextMenu".AND.o:Menu:hMenu==::MeasureItemStruct:itemData} ) ) > 0
//      oMenu := ::Components[n]:Menu
//   ENDIF
//   DEFAULT oMenu TO ::Menu

   IF ::MeasureItemStruct:CtlType == ODT_MENU .AND. oMenu != NIL
      FOR EACH oButton IN oMenu:aItems
         IF oButton:MenuItemInfo:fType & BTNS_SEP == 0 .AND. oButton:Menu != NIL
            IF ( n := ASCAN( oButton:Menu:aItems, {|o|o:Id==::MeasureItemStruct:itemID } ) ) > 0
               RETURN oButton:Menu:aItems[n]:MeasureItem( ::MeasureItemStruct, nlParam )
            ENDIF
            // it is NOT the 1st level, let's see the rest
            FOR EACH oSub IN oButton:Menu:aItems
               IF ( oItem := oSub:GetMenuById( ::MeasureItemStruct:itemID ) )!= NIL
                  RETURN oItem:MeasureItem( ::MeasureItemStruct, nlParam )
               ENDIF
            NEXT
         ENDIF
      NEXT
   ENDIF
RETURN NIL

METHOD OnDrawItem( nwParam, nlParam ) CLASS Window
   LOCAL n, oItem, oButton, oSub, oMenu

   oMenu := ::Application:oCurMenu

//   IF ( n := ASCAN( ::Components, {|o| o:__xCtrlName=="ContextMenu".AND.o:Menu:hMenu==::DrawItemStruct:itemData} ) ) > 0
//      oMenu := ::Components[n]:Menu
//   ENDIF
//   DEFAULT oMenu TO ::Menu

   IF ::DrawItemStruct:CtlType == ODT_MENU .AND. oMenu != NIL
      IF ::DrawItemStruct:itemState > 200
         ::DrawItemStruct:itemState -= 256
      ENDIF
      FOR EACH oButton IN oMenu:aItems
         IF oButton:MenuItemInfo:fType & BTNS_SEP == 0 .AND. oButton:Menu != NIL
            IF ( n := ASCAN( oButton:Menu:aItems, {|o|o:Id==::DrawItemStruct:itemID } ) ) > 0
               oButton:Menu:aItems[n]:DrawItem( ::DrawItemStruct, .F. )
               RETURN 1
            ENDIF
            // it is NOT the 1st level, let's see the rest
            FOR EACH oSub IN oButton:Menu:aItems
               IF ( oItem := oSub:GetMenuById(::DrawItemStruct:itemID ) )!= NIL
                  oItem:DrawItem( ::DrawItemStruct, .F. )
                  RETURN 1
               ENDIF
            NEXT
         ENDIF
      NEXT
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD GetClientRect() CLASS Window

   LOCAL aRect:=_GetClientRect( ::hWnd )

   ::ClientWidth  := aRect[3]
   ::ClientHeight := aRect[4]

RETURN {aRect[3],aRect[4]}

//-----------------------------------------------------------------------------------------------

METHOD GetWindowRect() CLASS Window
/*
   static rc, pt

   DEFAULT rc TO (struct RECT)
   GetWindowRect( ::hWnd, @rc )

   IF ::Parent != NIL

      DEFAULT pt TO (struct POINT)
      pt:x := rc:left
      pt:y := rc:top
      ScreenToClient( ::Parent:hWnd, @pt )

      ::xLeft   := pt:x
      ::xTop    := pt:y
    ELSE
     ::xLeft   := rc:left //aRect[1]
     ::xTop    := rc:top //aRect[2]
   ENDIF
   ::xWidth  := rc:right - rc:left
   ::xHeight := rc:bottom - rc:top

RETURN { rc:left, rc:top, rc:right, rc:bottom }
*/
   LOCAL aPt, aRect

   aRect := _GetWindowRect( ::hWnd )

   IF ::Parent != NIL

      aPt := { aRect[1], aRect[2] }
      _ScreenToClient( ::Parent:hWnd, @aPt )

      ::xLeft   := aPt[1]// + ::Parent:HorzScrollPos
      ::xTop    := aPt[2]// + ::Parent:VertScrollPos
    ELSE
      ::xLeft   := aRect[1]
      ::xTop    := aRect[2]

   ENDIF
   ::xWidth  := aRect[3] - aRect[1]
   IF ::ClsName != "ComboBox"
      ::xHeight := aRect[4] - aRect[2]
   ENDIF
RETURN aRect

//-----------------------------------------------------------------------------------------------

METHOD __SetScrollBars() CLASS Window

   STATIC lBusy := .F.

   LOCAL nDelta
   // added variables
   LOCAL sbi
   LOCAL cBytes
   LOCAL nWidth       := ::ClientWidth
   LOCAL nHeight      := ::ClientHeight
   LOCAL nVertSize    := ::ClientHeight
   LOCAL nHorzSize    := ::ClientWidth

   IF lBusy .OR. ::__ClassInst != NIL
      //TraceLog( "Nested Recursion!" )
      RETURN NIL
   ELSE
      lBusy := .T.
   ENDIF

/*
   IF ::HorzScroll
      IF ::Height-::ClientHeight > GetSystemMetrics( SM_CYCAPTION ) + (GetSystemMetrics( SM_CYFRAME )*2)
         nVertSize += GetSystemMetrics( SM_CXHSCROLL )
      ENDIF
   ENDIF

   IF ::VertScroll
      IF ::Width-::ClientWidth > (GetSystemMetrics( SM_CXFRAME )*2)
         nHorzSize += GetSystemMetrics( SM_CYVSCROLL )
      ENDIF
   ENDIF
*/

   IF ::HorzScroll
      IF nHorzSize > ::OriginalRect[3]
         IF nVertSize < ::OriginalRect[4]
            nHorzSize := ::ClientWidth
         ENDIF
      ELSE
         nHorzSize := ::ClientWidth
      ENDIF
   ENDIF

   IF ::VertScroll
      IF nVertSize > ::OriginalRect[4]
         IF nHorzSize < ::OriginalRect[3]
            nVertSize := ::ClientHeight
         ENDIF
      ELSE
         nVertSize := ::ClientHeight
      ENDIF
   ENDIF

   IF ::VertScroll
      IF ::siv == NIL
         ::siv := (struct SCROLLINFO)
         ::siv:cbSize := ::siv:sizeof()
         ::siv:nMin   := 0
      ENDIF

      IF ::ClientHeight + ::VertScrollPos > ::OriginalRect[4] .AND. ::VertScrollPos > 0
         nDelta := Max( Int( ::OriginalRect[4] - nHeight - ::VertScrollPos+1 ), - ::VertScrollPos )
         _ScrollWindow( ::hWnd, 0,-nDelta)
         ::VertScrollPos += nDelta
      ENDIF

      IF ::siv:nPage != nVertSize .OR. ::siv:nPos != ::VertScrollPos .OR. ::OriginalRect[4] != ::siv:nMax
         ::siv:nMax   := ::OriginalRect[4] -1
         ::siv:nPage  := nVertSize
         ::siv:nPos   := ::VertScrollPos
         ::siv:fMask  := SIF_ALL
         SetScrollInfo( ::hWnd, SB_VERT, ::siv, ::IsWindowVisible() )
      ENDIF

   ENDIF

   IF ::HorzScroll
      IF ::sih == NIL
         ::sih := (struct SCROLLINFO)
         ::sih:cbSize := ::sih:sizeof()
         ::sih:nMin   := 0
      ENDIF
      IF ::ClientWidth + ::HorzScrollPos > ::OriginalRect[3] .AND. ::HorzScrollPos > 0
         nDelta := Max( Int( ::OriginalRect[3] - nWidth - ::HorzScrollPos+1 ),- ::HorzScrollPos )
         _ScrollWindow( ::hWnd, -nDelta, 0 )
         ::HorzScrollPos += nDelta
      ENDIF

      IF ::sih:nPage != nHorzSize .OR. ::sih:nPos != ::HorzScrollPos .OR. ::OriginalRect[3] != ::sih:nMax
         ::sih:nMax   := ::OriginalRect[3] -1
         ::sih:nPage  := nHorzSize
         ::sih:nPos   := ::HorzScrollPos
         ::sih:fMask  := SIF_ALL
         SetScrollInfo( ::hWnd, SB_HORZ, ::sih, .T. )
      ENDIF

   ENDIF

   lBusy := .F.

   IF ::HorzScroll .OR. ::VertScroll
      RETURN 0
   ENDIF

RETURN NIL


//------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------

METHOD DockIt() CLASS Window
   IF ::IsWindow()
      ::Parent:UpdateWindow()
      ::__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight, NIL, .T. )
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------

METHOD Hide() CLASS Window
   LOCAL hDef, oChild, oSplitter, nAnimation
   IF ::hWnd != NIL
      IF ::AnimationStyle != NIL .AND. ::AnimationStyle <> 0 .AND. ::__ClassInst == NIL
         nAnimation := ::AnimationStyle
         DO CASE
            CASE nAnimation == ::System:WindowAnimation:SlideHorzPositive
                 nAnimation := ::System:WindowAnimation:SlideHorzNegative

            CASE nAnimation == ::System:WindowAnimation:SlideHorzNegative
                 nAnimation := ::System:WindowAnimation:SlideHorzPositive

            CASE nAnimation == ::System:WindowAnimation:SlideVertPositive
                 nAnimation := ::System:WindowAnimation:SlideVertNegative

            CASE nAnimation == ::System:WindowAnimation:SlideVertNegative
                 nAnimation := ::System:WindowAnimation:SlideVertPositive
         ENDCASE
         RETURN ::Animate( 1000, AW_HIDE | nAnimation )
      ENDIF
      ShowWindow( ::hWnd, SW_HIDE )
   ENDIF
   IF ::LeftSplitter != NIL
      ::LeftSplitter:Hide()
   ENDIF
   IF ::TopSplitter != NIL
      ::TopSplitter:Hide()
   ENDIF
   IF ::RightSplitter != NIL
      ::RightSplitter:Hide()
   ENDIF
   IF ::BottomSplitter != NIL
      ::BottomSplitter:Hide()
   ENDIF
   ::Style := ::Style & NOT( WS_VISIBLE )
   IF ::Parent != NIL
      ::Parent:SendMessage( WM_SIZE, 0, MAKELONG( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
   ENDIF
RETURN Self

METHOD Animate( nSpeed, nFlags ) CLASS Window
   AnimateWindow( ::hWnd, nSpeed, nFlags )
   ::xVisible := ( nFlags & AW_HIDE ) == 0
RETURN Self

METHOD Show( nShow ) CLASS Window
   LOCAL hDef, oChild, nRet
   DEFAULT nShow TO ::ShowMode

   IF nShow == SW_HIDE
      RETURN ::Hide()
   ENDIF

   IF ::hWnd != NIL
      IF ::AnimationStyle != NIL .AND. ::AnimationStyle <> 0 .AND. ::__ClassInst == NIL
         ::SendMessage( WM_SIZE, 0, MAKELONG( ::ClientWidth, ::ClientHeight ) )
         ::UpdateChildren()
         ::Animate( 1000, ::AnimationStyle )
         ::UpdateChildren()
       ELSE
         ShowWindow( ::hWnd, IIF( ::__ClassInst == NIL, nShow, SW_SHOW ) )
      ENDIF
   ENDIF
   IF ::LeftSplitter != NIL
      ::LeftSplitter:Show()
   ENDIF
   IF ::TopSplitter != NIL
      ::TopSplitter:Show()
   ENDIF
   IF ::RightSplitter != NIL
      ::RightSplitter:Show()
   ENDIF
   IF ::BottomSplitter != NIL
      ::BottomSplitter:Show()
   ENDIF
   ::Style := ::Style | WS_VISIBLE
   IF ::Parent != NIL
      ::Parent:SendMessage( WM_SIZE, 0, MAKELONG( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------

METHOD __FixDocking() CLASS Window
   LOCAL oObj, cObj
   IF ::Property != NIL
      FOR EACH cObj IN ::Property:Keys
          oObj := ::Property[ cObj ]
          IF oObj:HasMessage( "Dock" ) .AND. oObj:Dock != NIL
             IF VALTYPE( oObj:Dock:Left ) == "C"
                oObj:Dock:Left   := IIF( oObj:Dock:Left == oObj:Parent:Name, oObj:Parent, ::Property[ oObj:Dock:Left ] )
             ENDIF
             IF VALTYPE( oObj:Dock:Top ) == "C"
                oObj:Dock:Top    := IIF( oObj:Dock:Top == oObj:Parent:Name, oObj:Parent, ::Property[ oObj:Dock:Top ] )
             ENDIF
             IF VALTYPE( oObj:Dock:Right ) == "C"
                oObj:Dock:Right  := IIF( oObj:Dock:Right == oObj:Parent:Name, oObj:Parent, ::Property[ oObj:Dock:Right ] )
             ENDIF
             IF VALTYPE( oObj:Dock:Bottom ) == "C"
                oObj:Dock:Bottom := IIF( oObj:Dock:Bottom == oObj:Parent:Name, oObj:Parent, ::Property[ oObj:Dock:Bottom ] )
             ENDIF
          ENDIF
      NEXT
   ENDIF
RETURN Self

METHOD __OnParentSize( x, y, hDef, lMoveNow, lNoMove, nParX, nParY ) CLASS Window
   LOCAL n, a, aP, nWidth, nCurLeft, nCurTop, aDock, aPt, oLeft, oTop, oRight, oBottom, lAnchor, lDock, nHeight, rc

   DEFAULT lMoveNow TO FALSE
   DEFAULT lNoMove TO FALSE
   DEFAULT x TO ::Parent:Width
   DEFAULT y TO ::Parent:Height
   DEFAULT nParX TO ::Parent:__aCltRect[3]
   DEFAULT nParY TO ::Parent:__aCltRect[4]

   ::OnParentSize( x, y, hDef, lMoveNow, lNoMove )

   IF ::__HideResized .OR. !::IsChild .OR. !IsWindow( ::hWnd ) .OR. ::hWnd == NIL .OR. !::__Docked
      RETURN Self
   ENDIF

   IF ::__ClassInst != NIL .AND. !::Application:ShowDocking
      RETURN Self
   ENDIF

   lAnchor := VALTYPE( ::Anchor ) == "O" .AND. ( ::Anchor:ProportionalLeft .OR.;
                                                 ::Anchor:ProportionalTop .OR.;
                                                 ::Anchor:Left .OR.;
                                                 ::Anchor:Top .OR.;
                                                 ::Anchor:Right .OR.;
                                                 ::Anchor:Bottom  )

   lDock   := VALTYPE( ::Dock ) == "O"   .AND. ( ::Dock:Left != NIL .OR.;
                                                 ::Dock:Top != NIL .OR.;
                                                 ::Dock:Right != NIL .OR.;
                                                 ::Dock:Bottom != NIL )

   //IF lAnchor .AND. ::__ClassInst != NIL
   //   RETURN Self
   //ENDIF

   IF ( lAnchor .OR. lDock ) //( VALTYPE( ::Dock ) == "O" .OR. VALTYPE( ::Anchor ) == "O" ) .AND. ( ::Anchor:ProportionalLeft .OR. ::Anchor:ProportionalTop .OR. ::Anchor:Left .OR. ::Anchor:Top .OR. ::Anchor:Right .OR. ::Anchor:Bottom .OR. ::Dock:Left != NIL .OR. ::Dock:Top != NIL .OR. ::Dock:Right != NIL .OR. ::Dock:Bottom != NIL ) //.AND. LEFT( ::ClsName, 11 ) != "Splitter"
      DEFAULT ::__ClientRect   TO { ::Left, ::Top, ::Width, ::Height }
      DEFAULT ::__aCltRect     TO { ::Left, ::Top, ::Width, ::Height }
      DEFAULT ::OriginalRect   TO { ::Left, ::Top, ::ClientWidth, ::ClientHeight }

      nCurLeft := ::Left
      nCurTop  := ::Top

      oLeft   := ::Dock:Left
      IF VALTYPE( oLeft ) == "C"
         IF oLeft == ::Parent:Name
            oLeft := ::Parent
          ELSE
            oLeft := ::Form:&oLeft
         ENDIF
      ENDIF
      IF VALTYPE( oLeft ) == "B"
         oLeft := EVAL( oLeft, Self )
      ENDIF

      oTop    := ::Dock:Top
      IF VALTYPE( oTop ) == "C"
         IF oTop == ::Parent:Name
            oTop := ::Parent
          ELSE
            oTop := ::Form:&oTop
         ENDIF
      ENDIF
      IF VALTYPE( oTop ) == "B"
         oTop := EVAL( oTop, Self )
      ENDIF

      oRight  := ::Dock:Right
      IF VALTYPE( oRight ) == "C"
         IF oRight == ::Parent:Name
            oRight := ::Parent
          ELSE
            oRight := ::Form:&oRight
         ENDIF
      ENDIF
      IF VALTYPE( oRight ) == "B"
         oRight := EVAL( oRight, Self )
      ENDIF

      oBottom := ::Dock:Bottom
      IF VALTYPE( oBottom ) == "C"
         IF oBottom == ::Parent:Name
            oBottom := ::Parent
          ELSE
            oBottom := ::Form:&oBottom
         ENDIF
      ENDIF
      IF VALTYPE( oBottom ) == "B"
         oBottom := EVAL( oBottom, Self )
      ENDIF

      IF oLeft != NIL .AND. oLeft:AutoDock .AND. ( oLeft:Hidden .OR. !oLeft:__Docked .OR. oLeft:Style & WS_VISIBLE == 0 )
         WHILE oLeft != NIL .AND. oLeft:Parent != NIL .AND. oLeft != NIL .AND. oLeft:hWnd != ::Parent:hWnd .AND. ( oLeft:Hidden .OR. !oLeft:__Docked .OR. !oLeft:IsWindow() .OR. oLeft:Style & WS_VISIBLE == 0 ) //.AND. oLeft:IsWindow()
            IF oLeft:Dock != NIL
               oLeft := oLeft:Dock:Left
               IF VALTYPE( oLeft ) == "C"
                  IF oLeft == ::Parent:Name
                     oLeft := ::Parent
                   ELSE
                     oLeft := ::Form:&oLeft
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      IF oTop != NIL  .AND. oTop:AutoDock .AND. ( oTop:Hidden .OR. !oTop:__Docked .OR. oTop:Style & WS_VISIBLE == 0 )
         WHILE oTop != NIL .AND. oTop:Parent != NIL .AND. oTop != NIL .AND. oTop:hWnd != ::Parent:hWnd .AND. ( oTop:Hidden .OR. !oTop:__Docked .OR. !oTop:IsWindow() .OR. oTop:Style & WS_VISIBLE == 0 ) //.AND. oTop:IsWindow()
            IF oTop:Dock != NIL
               oTop := oTop:Dock:Top
               IF VALTYPE( oTop ) == "C"
                  IF oTop == ::Parent:Name
                     oTop := ::Parent
                   ELSE
                     oTop := ::Form:&oTop
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      IF oRight != NIL .AND. oRight:AutoDock .AND. ( oRight:Hidden .OR. !oRight:__Docked .OR. oRight:Style & WS_VISIBLE == 0 )
         WHILE oRight != NIL .AND. oRight:Parent != NIL .AND. oRight != NIL .AND. oRight:hWnd != ::Parent:hWnd .AND. ( oRight:Hidden .OR. !oRight:__Docked .OR. !oRight:IsWindow() .OR. oRight:Style & WS_VISIBLE == 0 )//.AND. oRight:IsWindow()
            IF oRight:Dock != NIL
               oRight := oRight:Dock:Right
               IF VALTYPE( oRight ) == "C"
                  IF oRight == ::Parent:Name
                     oRight := ::Parent
                   ELSE
                     oRight := ::Form:&oRight
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      IF oBottom != NIL .AND. oBottom:AutoDock .AND. ( oBottom:Hidden .OR. !oBottom:__Docked .OR. oBottom:Style & WS_VISIBLE == 0 )
         WHILE oBottom != NIL .AND. oBottom:Parent != NIL .AND. oBottom != NIL .AND. oBottom:hWnd != ::Parent:hWnd .AND. ( oBottom:Hidden .OR. !oBottom:__Docked .OR. !oBottom:IsWindow() .OR. oBottom:Style & WS_VISIBLE == 0 ) //.AND. oBottom:IsWindow()
            IF oBottom:Dock != NIL
               oBottom := oBottom:Dock:Bottom
               IF VALTYPE( oBottom ) == "C"
                  IF oBottom == ::Parent:Name
                     oBottom := ::Parent
                   ELSE
                     oBottom := ::Form:&oBottom
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      IF !::__Splitting
         // Get Standard positions
         ::xLeft := IIF( ::Anchor:Left .AND. ::Parent:__aCltRect != NIL .AND. x != NIL, x - (::Parent:__aCltRect[3] - ::xLeft), ::xLeft )
         IF ::Anchor:Top .AND. ::Dock:Bottom != NIL
            ::xTop  := IIF( ::Parent:__ClientRect != NIL, y - (::Parent:__ClientRect[4] - ::xTop), ::xTop )
            IF ::xTop <= ::Parent:TopMargin .AND. ::Parent:ClientHeight > 0
               ::xTop  := ::Parent:TopMargin
            ENDIF
          ELSE
            ::xTop  := IIF( ::Anchor:Top .AND. ::Parent:__aCltRect != NIL .AND. y != NIL, y - (::Parent:__aCltRect[4] - ::xTop), ::xTop )
         ENDIF
         ::xWidth  := ::xWidth  + IIF( ::Anchor:Right .AND. ::Parent:__aCltRect != NIL .AND. x != NIL, x - ::Parent:__aCltRect[3], 0 )
         IF ::ClsName != "ComboBox"
            ::xHeight := ::xHeight + IIF( ::Anchor:Bottom .AND. ::Parent:__aCltRect != NIL .AND. y != NIL, y - ::Parent:__aCltRect[4], 0 )
         ENDIF
      ENDIF

      // override Left
      IF oLeft != NIL
         IF oLeft:hWnd == ::Parent:hWnd
            ::xLeft := ::Dock:LeftMargin
          ELSEIF oLeft:IsChild
            ::xLeft := oLeft:xLeft + oLeft:xWidth + ::Dock:LeftMargin + IIF( oLeft:RightSplitter != NIL, oLeft:RightSplitter:Weight, 0 )
         ENDIF
       ELSEIF ::Dock:JoinLeft != NIL
         ::xLeft := ::Dock:JoinLeft:Left
         oLeft := ::Dock:JoinLeft
      ENDIF

      // override top
      IF oTop != NIL
         IF oTop:hWnd == ::Parent:hWnd
            ::xTop  := ::Dock:TopMargin
          ELSEIF oTop:IsChild
            IF oTop:ClsName == "ComboBox"
               rc := (struct RECT)
               GetClientRect( oTop:hWnd, @rc )
               n := rc:bottom
             ELSE
               n := oTop:Height
            ENDIF
            ::xTop  := oTop:Top + n + ::Dock:TopMargin + IIF( oTop:BottomSplitter != NIL, oTop:BottomSplitter:Weight, 0 )
         ENDIF
      ENDIF

      IF oRight != NIL
         // Right Docking
         IF oLeft == NIL .AND. !::__Splitting // move to the right
            IF oRight:hWnd == ::Parent:hWnd
               ::xLeft := ::Parent:ClientWidth - ::xWidth - ::Dock:RightMargin
             ELSEIF oRight:IsChild
               ::xLeft := oRight:xLeft - ::xWidth - ::Dock:RightMargin - IIF( oRight:LeftSplitter != NIL, oRight:LeftSplitter:Weight, 0 )
            ENDIF
          ELSE // resize to the right
            IF oRight:hWnd == ::Parent:hWnd
               ::xWidth := oRight:ClientWidth - ::xLeft - ::Dock:RightMargin
             ELSEIF oRight:IsChild
               ::xWidth := oRight:xLeft - ::xLeft - ::Dock:RightMargin - IIF( oRight:LeftSplitter != NIL, oRight:LeftSplitter:Weight, 0 )
            ENDIF
         ENDIF
      ENDIF

      IF oBottom != NIL
         // Bottom Docking
         IF oTop == NIL
            IF oBottom:hWnd == ::Parent:hWnd
               ::xTop := ::Parent:ClientHeight - ::xHeight - ::Dock:BottomMargin
             ELSEIF oBottom:IsChild
               ::xTop := oBottom:Top - ::xHeight - ::Dock:BottomMargin - IIF( oBottom:TopSplitter != NIL, oBottom:TopSplitter:Weight, 0 )
            ENDIF
          ELSEIF ::ClsName != "ComboBox"
            IF oBottom:hWnd == ::Parent:hWnd
               ::xHeight := oBottom:ClientHeight - ::xTop - ::Dock:BottomMargin
             ELSEIF oBottom:IsChild
               ::xHeight := oBottom:Top  - ::xTop - ::Dock:BottomMargin - IIF( oBottom:TopSplitter != NIL, oBottom:TopSplitter:Weight, 0 )
            ENDIF
         ENDIF

      ENDIF

      IF !EMPTY(::xWidth) .AND. !EMPTY(::Parent:Width)
         DEFAULT ::__WidthPerc  TO ( ::xWidth  / ::Parent:Width )
      ENDIF
      IF !EMPTY(::xHeight) .AND. !EMPTY(::Parent:Height)
         DEFAULT ::__HeightPerc TO ( ::xHeight / ::Parent:Height )
      ENDIF

      // proportional controls
      IF ::Anchor:ProportionalLeft
         ::xLeft   := Int( IIF( oLeft == NIL, ::OriginalRect[1] * x / ::Parent:__aCltRect[3], ::xLeft ) )
         ::xWidth  := Int( ( ::Parent:ClientWidth  ) * ::__WidthPerc )
       ELSEIF ::Parent:__Splitting
         IF oRight == NIL .AND. ::Anchor:Right
            IF ::LeftSplitter != NIL .AND. ::LeftSplitter:lSizing
               ::xWidth  := ::aPrevSize[3] + ( ::aPrevSize[1]-::xLeft )
             ELSE
               ::xWidth  += ( nCurLeft - ::xLeft )
            ENDIF
         ENDIF
         IF oLeft == NIL .AND. ::Anchor:Left
            ::xWidth  += ( ::xLeft-nCurLeft )
            ::xLeft  -= ( ::xLeft-nCurLeft )
         ENDIF
      ENDIF
      IF ::Anchor:ProportionalTop
         ::xTop    := IIF( oTop  == NIL, ::OriginalRect[2] * y / ::Parent:__aCltRect[4], ::xTop )
         IF ::ClsName != "ComboBox"
            ::xHeight := Int( ( ::Parent:Height - ::xTop ) * ::__HeightPerc )
         ENDIF
       ELSEIF ::Parent:__Splitting
         IF oBottom == NIL .AND. ::Anchor:Bottom .AND. ::ClsName != "ComboBox"
            IF ::TopSplitter != NIL .AND. ::TopSplitter:lSizing
               ::xHeight := ::aPrevSize[4] + ( ::aPrevSize[2]-::xTop )
             ELSE
               ::xHeight += ( nCurTop - ::xTop )
            ENDIF
         ENDIF
         IF oTop == NIL .AND. ::Anchor:Top
            IF ::ClsName != "ComboBox"
               ::xHeight += ( ::xTop-nCurTop )
            ENDIF
            ::xTop    -= ( ::xTop-nCurTop )
         ENDIF
      ENDIF
      IF !lNoMove

         IF ::ClsName == "ComboBox"
            rc := (struct RECT)
            GetClientRect( ::hWnd, @rc )
            nHeight := rc:bottom
          ELSE
            nHeight := ::xHeight
         ENDIF

         IF lMoveNow .OR. !::DeferRedraw
            n := SWP_NOOWNERZORDER | SWP_NOZORDER
            IF ::DeferRedraw
               n := n | SWP_NOACTIVATE | SWP_DEFERERASE
            ENDIF
            SetWindowPos( ::hWnd, , ::xLeft, ::xTop, ::xWidth, nHeight, n )
          ELSE
            DeferWindowPos( hDef, ::hWnd, , ::xLeft, ::xTop, ::xWidth, nHeight, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER ) //| IIF( ::OsVer:dwMajorVersion < 5, SWP_DEFERERASE, 0 ) )
         ENDIF
         //::UpdateWindow()
      ENDIF
      RETURN Self
   ENDIF

RETURN NIL

//---------------------------------------------------------------------------------------------

METHOD TrackMouseEvent( nFlags, nHoverTimeOut ) CLASS Window
   LOCAL tme
   tme := (struct TRACKMOUSEEVENT)
   tme:cbSize      := tme:SizeOf()
   tme:dwFlags     := nFlags
   tme:hwndTrack   := ::hWnd
   tme:dwHoverTime := NIL
   IF nFlags & TME_HOVER == TME_HOVER
      DEFAULT nHoverTimeOut TO HOVER_DEFAULT
      tme:dwHoverTime := nHoverTimeOut
   ENDIF
   TrackMouseEvent( tme )
RETURN Self

//---------------------------------------------------------------------------------------------

METHOD MoveWindow( x, y, w, h, lRep ) CLASS Window
   LOCAL aRect, aPt
   DEFAULT x    TO ::xLeft
   DEFAULT y    TO ::xTop
   DEFAULT w    TO ::xWidth
   DEFAULT h    TO ::xHeight
   DEFAULT lRep TO ::IsWindowVisible()
   ::xLeft  := x
   ::xTop   := y
   ::xWidth := w
   ::xHeight:= h
   MoveWindow( ::hWnd, ::xLeft, ::xTop, ::xWidth, ::xHeight, lRep )
RETURN Self
//---------------------------------------------------------------------------------------------

METHOD MessageWait( cText, nTimeOut, nColor ) CLASS Window
   LOCAL oWnd, oLabel

   oWnd := MsgWait( Self )
   oWnd:Caption := cText
   oWnd:Style := WS_POPUP + WS_DLGFRAME

   oWnd:xLeft   := 0
   oWnd:xTop    := 0
   oWnd:xWidth  := oWnd:Drawing:GetTextExtentPoint32( cText )[1] + 20

   oWnd:xHeight := 75
   oWnd:Center  := .T.
   oWnd:Create()
   oWnd:Show()

   IF nTimeOut != NIL
      oWnd:SetTimer(1, nTimeOut)
      oWnd:OnWMTimer := {|o|o:Destroy(), oWnd:KillTimer(1)}
   ENDIF
RETURN oWnd

//----------------------------------------------------------------------------------------------------

METHOD OnVScroll( nSBCode, nPos, nlParam ) CLASS Window

   LOCAL nDelta
   LOCAL nMaxPos := ::OriginalRect[4]// - ::xHeight

   SWITCH nSBCode
      CASE SB_LINEDOWN
           IF ::VertScrollPos + ::Siv:nPage > nMaxPos
              RETURN 0
           ENDIF
           nDelta := min(nMaxPos/100,nMaxPos - ::Siv:nPage -::VertScrollPos)
           EXIT

      CASE SB_LINEUP
           IF ::VertScrollPos <= 0
              RETURN 0
           ENDIF
           nDelta := -min(nMaxpos/100,::VertScrollPos)
           EXIT

      CASE SB_PAGEDOWN
           IF ::VertScrollPos + ::Siv:nPage  >= nMaxPos
              RETURN 0
           ENDIF
           nDelta := min(nMaxPos/10,nMaxPos- ::Siv:nPage - ::VertScrollPos)
           EXIT

      CASE SB_THUMBPOSITION
      CASE SB_THUMBTRACK
           nDelta := nPos - ::VertScrollPos
           EXIT

      CASE SB_PAGEUP
           IF ::VertScrollPos <= 0
              RETURN 0
           ENDIF
           nDelta := -min(nMaxPos/10,::VertScrollPos)
           EXIT

      DEFAULT
           RETURN 0

   END

   nDelta := Int(nDelta)
   ::VertScrollPos += nDelta

   ::siv:nPos   := ::VertScrollPos
   ::siv:fMask  := SIF_POS

   SetScrollInfo( ::hWnd, SB_VERT, ::siv, .T. )
   _ScrollWindow( ::hWnd, 0, -nDelta )

   IF ::Active
      ::InvalidateRect()
   ENDIF

RETURN 0


//----------------------------------------------------------------------------------------------------

METHOD OnHScroll( nSBCode, nPos, nlParam ) CLASS Window

   LOCAL nDelta
   LOCAL nMaxPos := ::OriginalRect[3]// - ::xHeight
   LOCAL aRect   := _GetClientRect( ::hWnd )

   SWITCH nSBCode
      CASE SB_LINEDOWN
           IF ::HorzScrollPos + ::Sih:nPage > nMaxPos
              RETURN 0
           ENDIF
           nDelta := min(nMaxpos/100,nMaxPos - ::Sih:nPage - ::HorzScrollPos)
           EXIT

      CASE SB_LINEUP
           IF ::HorzScrollPos <= 0
              RETURN 0
           ENDIF
           nDelta := -min(nMaxpos/100,::HorzScrollPos)
           EXIT

      CASE SB_PAGEDOWN
           IF ::HorzScrollPos  + ::Sih:nPage >= nMaxPos
              RETURN 0
           ENDIF
           nDelta := min(nMaxPos/10,nMaxPos - ::Sih:nPage - ::HorzScrollPos)
           EXIT

      CASE SB_THUMBPOSITION
      CASE SB_THUMBTRACK
           nDelta := nPos - ::HorzScrollPos
           EXIT

      CASE SB_PAGEUP
           IF ::HorzScrollPos <= 0
              RETURN 0
           ENDIF
           nDelta := -min(nMaxPos/10,::HorzScrollPos)
           EXIT

      DEFAULT
           RETURN 0

   END

   nDelta := Int( nDelta )
   ::HorzScrollPos += nDelta

   ::sih:nPos   := ::HorzScrollPos
   ::sih:fMask  := SIF_POS

   SetScrollInfo( ::hWnd, SB_HORZ, ::sih, .T.)
   _ScrollWindow( ::hWnd, -nDelta, 0)

RETURN 0

METHOD GetRect() CLASS Window
   LOCAL rc := (struct RECT)
   GetWindowRect( ::hWnd, @rc )
RETURN rc

METHOD GetChildFromPoint( pt, bAction, aSel ) CLASS Window
   LOCAL o, rc, oCtrl, Control, Band

   FOR EACH Control IN ::Children
       IF aSel == NIL .OR. ASCAN( aSel, {|a| a[1]:hWnd == Control:hWnd} ) == 0
          IF Control:IsWindowVisible()
             IF bAction == NIL .or. EVAL( bAction, Control )
                rc := Control:GetRect()
                IF ptInRect( rc, pt )
                   IF Control:ClsName == "CoolBarBand" .AND. Control:BandChild != NIL
                      o := Control:BandChild:GetChildFromPoint( pt, bAction, aSel )
                    ELSE
                      o := Control:GetChildFromPoint( pt, bAction, aSel )
                   ENDIF
                   IF o != NIL
                      oCtrl := o
                    ELSE
                      oCtrl := Control
                   ENDIF
                ENDIF
             ENDIF
          ENDIF
       ENDIF
   NEXT

   DEFAULT oCtrl TO Self
RETURN oCtrl

METHOD __GetShowMode() CLASS Window
   LOCAL wndpl := (struct WINDOWPLACEMENT)
   GetWindowPlacement( ::hWnd, @wndpl )
RETURN wndpl:showCmd

METHOD __SetFrameStyle(n) CLASS Window
   SWITCH n
      CASE 1
         ::Style := ::Style & NOT( WS_POPUP )
         ::Style := ::Style & NOT( WS_CHILD )
         ::Style := ::Style | WS_OVERLAPPED
         EXIT
      CASE 2
         ::Style := ::Style & NOT( WS_OVERLAPPED )
         ::Style := ::Style & NOT( WS_CHILD )
         ::Style := ::Style | WS_POPUP
         EXIT
      CASE 3
         ::Style := ::Style & NOT( WS_OVERLAPPED )
         ::Style := ::Style & NOT( WS_POPUP )
         ::Style := ::Style | WS_CHILD
         EXIT
   END
   IF ::hWnd != NIL .AND. ::__ClassInst == NIL
      SetWindowLong( ::hWnd, GWL_STYLE, ::Style )
      SetWindowPos( ::hWnd,, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
      RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   ENDIF
RETURN Self

METHOD __SetWindowCursor( nCursor ) CLASS Window
   IF ::__ClassInst == NIL
      ::__hCursor := IIF( nCursor != NIL .AND. nCursor <= 32651, LoadCursor(, nCursor ), nCursor )
      IF ::hWnd != NIL .AND. IsWindowVisible( ::hWnd ) .AND. ( ::hWnd == GetFocus() .OR. ( ::ExStyle & WS_EX_NOACTIVATE ) == WS_EX_NOACTIVATE )
         ::SendMessage( WM_SETCURSOR )
      ENDIF
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------

FUNCTION __Evaluate( iVar, Param1, Param2, Param3, nRet )

   IF nRet == NIL
      IF VALTYPE( iVar ) == "B"
         nRet := EVAL( iVar, Param1, Param2, Param3 )
       ELSEIF VALTYPE( iVar ) $ "NP"
         nRet := HB_Exec( iVar,, Param1, Param2, Param3 )
      ENDIF
   ENDIF

RETURN nRet

//---------------------------------------------------------------------------------------------

CLASS MsgWait FROM WinForm
   METHOD Init() CONSTRUCTOR
   METHOD OnEraseBkGnd()
   METHOD Create()
ENDCLASS

METHOD Init( oParent ) CLASS MsgWait
   ::__xCtrlName := "MessageWait"
RETURN ::Super:Init( oParent )

METHOD Create() CLASS MsgWait
   LOCAL aAlign
   ::Super:Create()
RETURN Self

METHOD OnEraseBkGnd( hDC ) CLASS MsgWait
   LOCAL aRect := _GetClientRect( ::hWnd )
   _FillRect( hDC, aRect, GetSysColorBrush( COLOR_BTNFACE ) )
   aRect[1]+=5
   aRect[2]+=5
   aRect[3]-=5
   aRect[4]-=5
   SetBkMode( hDC, TRANSPARENT )
   SelectObject( hDC, ::Font:Handle )
   _DrawText( hDC, ::Caption, @aRect, DT_LEFT+DT_CENTER+DT_VCENTER+DT_SINGLELINE )
RETURN 1
//---------------------------------------------------------------------------------------------

CLASS __WindowDock
   PROPERTY Left    INDEX 1 READ xLeft    WRITE SetDock    PROTECTED INVERT
   PROPERTY Top     INDEX 2 READ xTop     WRITE SetDock    PROTECTED INVERT
   PROPERTY Right   INDEX 3 READ xRight   WRITE SetDock    PROTECTED INVERT
   PROPERTY Bottom  INDEX 4 READ xBottom  WRITE SetDock    PROTECTED INVERT
   PROPERTY Margins         READ xMargins WRITE SetMargins PROTECTED

   DATA LeftMargin   EXPORTED INIT 0
   DATA TopMargin    EXPORTED INIT 0
   DATA RightMargin  EXPORTED INIT 0
   DATA BottomMargin EXPORTED INIT 0

   DATA JoinLeft     EXPORTED

   DATA BackLeft     EXPORTED
   DATA BackTop      EXPORTED
   DATA BackRight    EXPORTED
   DATA BackBottom   EXPORTED

   DATA Type         EXPORTED INIT 0

   DATA Owner        EXPORTED
   DATA __ClassInst  EXPORTED
   DATA ClsName      EXPORTED INIT "Dock"


   DATA xMargin      PROTECTED INIT 2
   ACCESS Margin     INLINE ::xMargin
   ASSIGN Margin(n)  INLINE ::Margins := alltrim( str( n ) ) + "," + alltrim( str( n ) ) + "," + alltrim( str( n ) ) + "," + alltrim( str( n ) )

   ACCESS Parent          INLINE ::Owner
   ACCESS Form            INLINE ::Owner:Form
   ACCESS Application     INLINE __GetApplication()

   METHOD Init() CONSTRUCTOR
   METHOD SetDock()
   METHOD Update()
   METHOD SetMargins()
ENDCLASS

METHOD Init( oOwner ) CLASS __WindowDock
   ::Owner := oOwner
   IF oOwner:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
   ENDIF
RETURN Self

METHOD SetMargins( cMargins ) CLASS __WindowDock
   LOCAL oItem, n, aMargins := hb_atokens( cMargins, "," )
   ::LeftMargin   := 0
   ::TopMargin    := 0
   ::RightMargin  := 0
   ::BottomMargin := 0

   IF LEN( aMargins ) == 1
      ASIZE( aMargins, 4 )
      FOR n := 2 TO 4
          IF aMargins[n] == NIL
             aMargins[n] := aMargins[1]
          ENDIF
      NEXT
    ELSE
      ASIZE( aMargins, 4 )
      FOR n := 1 TO 4
          IF aMargins[n] == NIL
             aMargins[n] := "0"
          ENDIF
      NEXT
   ENDIF

   ::LeftMargin   := VAL( aMargins[1] )
   ::TopMargin    := VAL( aMargins[2] )
   ::RightMargin  := VAL( aMargins[3] )
   ::BottomMargin := VAL( aMargins[4] )

   ::xMargins := Alltrim( Str( ::LeftMargin ) )+ "," + Alltrim( Str( ::TopMargin ) ) + "," + Alltrim( Str( ::RightMargin ) ) + "," + Alltrim( Str( ::BottomMargin ) )

   ::Update()

   IF ::__ClassInst != NIL .AND. ::Application:ObjectManager != NIL
      ::Application:ObjectManager:PostMessage( WM_USER + 4767 )
   ENDIF

RETURN Self

METHOD Update() CLASS __WindowDock
   IF ::Owner:IsWindowVisible()
      ::Owner:DockIt()
      IF ::Owner:__ClassInst != NIL
         PostMessage( ::Owner:Parent:hWnd, WM_SIZE, 0, MAKELPARAM( ::Owner:Parent:ClientWidth, ::Owner:Parent:ClientHeight ) )
         ::Form:InvalidateRect()
         ::Form:CtrlMask:InvalidateRect()
      ENDIF
   ENDIF
RETURN Self

METHOD SetDock( x, oDock ) CLASS __WindowDock
   IF x != NIL
      ::Type += x
   ENDIF
   ::Update()
RETURN Self

CLASS __AnchorSet
   PROPERTY Left   INDEX 1 READ xLeft   WRITE SetAnchor DEFAULT .F. PROTECTED
   PROPERTY Top    INDEX 2 READ xTop    WRITE SetAnchor DEFAULT .F. PROTECTED
   PROPERTY Right  INDEX 3 READ xRight  WRITE SetAnchor DEFAULT .F. PROTECTED
   PROPERTY Bottom INDEX 4 READ xBottom WRITE SetAnchor DEFAULT .F. PROTECTED
   PROPERTY Center INDEX 5 READ xCenter WRITE SetAnchor DEFAULT .F. PROTECTED

   DATA ProportionalLeft  EXPORTED INIT .F.
   DATA ProportionalTop   EXPORTED INIT .F.

   DATA xProportionalLeft PROTECTED INIT .F.
   DATA xProportionalTop  PROTECTED INIT .F.
   DATA __ClassInst       EXPORTED
   DATA Owner             EXPORTED
   DATA ClsName           EXPORTED INIT "Anchor"
   ACCESS Parent          INLINE ::Owner
   ACCESS Form            INLINE ::Owner:Form

   ACCESS Application     INLINE __GetApplication()

   METHOD Init() CONSTRUCTOR
   METHOD CenterWindow(l) INLINE IIF( l, ::Owner:CenterWindow(), ), Self
   METHOD SetAnchor()
ENDCLASS

METHOD Init( oOwner ) CLASS __AnchorSet
   ::Owner := oOwner
   IF oOwner:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
   ENDIF
RETURN Self

METHOD SetAnchor( nPos, lSet ) CLASS __AnchorSet
   LOCAL oItem, Item, aValue, n, cVar
   SWITCH nPos
      CASE 1
         IF lSet
            ::xRight  := .F.
            ::xCenter := .F.
            aValue := {"Right", "Center"}
         ENDIF
         EXIT
      CASE 3
         IF lSet
            ::xLeft   := .F.
            ::xCenter := .F.
            aValue := {"Left", "Center"}
         ENDIF
         EXIT
      CASE 2
         IF lSet
            ::xBottom := .F.
            ::xCenter := .F.
            aValue := {"Bottom", "Center"}
         ENDIF
         EXIT
      CASE 4
         IF lSet
            ::xTop    := .F.
            ::xCenter := .F.
            aValue := {"Top", "Center"}
         ENDIF
         EXIT
      CASE 5
         IF lSet
            ::xRight  := .F.
            ::xLeft   := .F.
            ::xBottom := .F.
            ::xTop    := .F.
            aValue := {"Left", "Top", "Right", "Bottom"}
         ENDIF
         EXIT
   END
   IF ::Application != NIL .AND. ::__ClassInst != NIL .AND. lSet
      WITH OBJECT ::Application:ObjectManager
         IF :ActiveObject == ::Owner
            IF ( oItem := FindTreeItem( :Items, TVGetSelected( :hWnd ) ) ) != NIL
               FOR EACH Item IN oItem:Owner:Items
                   IF ( n := ASCAN( aValue, Item:Caption ) ) > 0
                      cVar := Item:Caption
                      Item:ColItems[1]:Value := :ActiveObject:Anchor:&cVar
                   ENDIF
               NEXT
               :InvalidateRect(, .F. )
            ENDIF
         ENDIF
      END
   ENDIF
RETURN Self


//--------------------------------------------------------------------------------------------------
//------------------------------------------ UTILITIES ---------------------------------------------
//--------------------------------------------------------------------------------------------------

FUNCTION HSLtoRGB( Hue, Saturation, Luminance )
   LOCAL nHue,nSat,nLum,nRed,nGreen,nBlue,temp2,temp3,temp1,n

   temp3 := {0,0,0}

   nHue := Hue / 239
   nSat := Saturation / 239
   nLum := Luminance / 239

   IF nSat == 0
      nRed   := nLum
      nGreen := nLum
      nBlue  := nLum
    ELSE
      IF nLum < 0.5
         temp2 := nLum * (1 + nSat)
       ELSE
         temp2 := nLum + nSat - nLum * nSat
      ENDIF
      temp1 := 2 * nLum - temp2

      temp3[1] := nHue + 1 / 3
      temp3[2] := nHue
      temp3[3] := nHue - 1 / 3

      FOR n = 1 To 3
          IF temp3[n] < 0
             temp3[n] := temp3[n] + 1
          ENDIF
          IF temp3[n] > 1
             temp3[n] := temp3[n] - 1
          ENDIF
          IF 6 * temp3[n] < 1
             temp3[n] := temp1 + (temp2 - temp1) * 6 * temp3[n]
           ELSE
             IF 2 * temp3[n] < 1
                temp3[n] := temp2
              ELSE
                IF 3 * temp3[n] < 2
                   temp3[n] := temp1 + (temp2 - temp1) * ((2 / 3) - temp3[n]) * 6
                 ELSE
                   temp3[n] := temp1
                ENDIF
             ENDIF
          ENDIF
      NEXT
      nRed   := temp3[1]
      nGreen := temp3[2]
      nBlue  := temp3[3]
   ENDIF
RETURN RGB( Int(nRed*255), Int(nGreen*255), Int(nBlue*255) )

FUNCTION RGBtoHSL( nColor )
   LOCAL nRed,nGreen,nBlue, pMax,pMin,nLum,nSat,nHue

   nRed   := GetRValue( nColor ) / 255
   nGreen := GetGValue( nColor ) / 255
   nBlue  := GetBValue( nColor ) / 255

   IF nRed > nGreen
      IF nRed > nBlue
         pMax := nRed
       ELSE
         pMax := nBlue
      ENDIF
    ELSEIF nGreen > nBlue
      pMax := nGreen
    ELSE
      pMax := nBlue
   ENDIF

   IF nRed < nGreen
      IF nRed < nBlue
         pMin := nRed
       ELSE
         pMin := nBlue
      ENDIF
    ELSEIF nGreen < nBlue
      pMin := nGreen
    ELSE
      pMin := nBlue
   ENDIF

   nLum := ( pMax + pMin ) / 2

   IF pMax == pMin
      nSat := 0
      nHue := 0
    ELSE
      IF nLum < 0.5
         nSat := (pMax - pMin) / (pMax + pMin)
       ELSE
         nSat = (pMax - pMin) / (2 - pMax - pMin)
      ENDIF

      DO CASE
         CASE pMax == nRed
              nHue := (nGreen - nBlue) / (pMax - pMin)
         CASE pMax == nGreen
              nHue := 2 + (nBlue - nRed) / (pMax - pMin)
         CASE pMax == nBlue
              nHue := 4 + (nRed - nGreen) / (pMax - pMin)
       ENDCASE
   ENDIF

   nHue := Round( nHue * 240 / 6, 0 )
   IF nHue < 0
      nHue := nHue + 240
   ENDIF
   nSat := Round(nSat * 240,0)
   nLum := Round(nLum * 240,0)
RETURN {nHue, nSat, nLum}

FUNCTION __FadeColor( nClr, nProcess )
   LOCAL nPer, nR, nG, nB

   nR := GetRValue( nClr )
   nG := GetGValue( nClr )
   nB := GetBValue( nClr )

   IF nProcess > 0 // to white
      nPer := (255*nProcess)/1000

      nR := MIN( nR+nPer, 255 )
      nG := MIN( nG+nPer, 255 )
      nB := MIN( nB+nPer, 255 )

    ELSE // to black

      nProcess := ABS( nProcess )

      nPer := (nR*nProcess)/1000
      nR := MAX( nR-nPer, 0 )

      nPer := (nG*nProcess)/1000
      nG := MAX( nG-nPer, 0 )

      nPer := (nB*nProcess)/1000
      nB := MAX( nB-nPer, 0 )
   ENDIF

RETURN RGB( Int(nR), Int(nG), Int(nB) )

FUNCTION StrTrim( n )
RETURN ALLTRIM(STR(n))

FUNCTION __GetIconSize( hIcon )
   LOCAL ii, cBuffer, aSize
   ii := (struct ICONINFO)
   GetIconInfo( hIcon, @ii )
   aSize := GetBmpSize( ii:hbmMask )
   AADD( aSize, ii:xHotspot )
   AADD( aSize, ii:yHotspot )
RETURN aSize

FUNCTION __FontCreate(cFont,nSize,lBold,lItalic,lVert,lUnder)
   local aFont,hFont,nAngle
   DEFAULT lItalic TO .F.
   DEFAULT lBold   TO .T.
   DEFAULT lUnder  TO .F.
   DEFAULT lVert   TO .F.
   nAngle:=IIF(lVert,900,0)
   aFont := {nSize, 0, nAngle, nAngle, IIF(lBold,700,0), lItalic, lUnder, .F., 1, 1, 0, 0, 0, cFont}
   hFont := _CreateFont(aFont)
RETURN hFont

FUNCTION __str2a( string, parser )
   LOCAL retar    := { }
   LOCAL commapos := 0
   IF parser == NIL
      parser := ','
   ENDIF
   DO WHILE Len( string ) > 0
      commapos := at( parser, string )
      IF commapos > 0
         aAdd( retar, Left( string, commapos - 1 ) )
         string := SubStr( string, commapos + Len( parser ) )
      ELSE
         aAdd( retar, string )
         string := ''
      ENDIF
   ENDDO
RETURN retar

FUNCTION __a2str( a, parser )
  LOCAL retstr := ''
  LOCAL i
  IF parser == NIL
     parser := ','
  ENDIF
  FOR i := 1 TO Len( a )
      IF i # 1
         retstr += parser
      ENDIF
      retstr += __asstring( a[ i ] )
  NEXT
RETURN retstr

FUNCTION __asString( x )
   LOCAL v := ValType( x )
   DO CASE
      CASE v == "C"
      CASE v == "N"
         RETURN AllTrim( str( x ) )
      CASE v == "L"
         IF x
            RETURN ".T."
          ELSE
            RETURN ".F."
         ENDIF
      CASE v == "D"
         RETURN dtoc( x )
      CASE v == "U"
         RETURN "NIL"
      CASE v == "A"
         RETURN "<Array>"
      CASE v == "O"
         RETURN x:classname()
      CASE v == "B"
         RETURN "<Block>"
      OTHERWISE
         RETURN ""
   END CASE
RETURN x

FUNCTION __Proper(cStr)
   local n,ch,nLen
   local c:=""
   local l:=.T.
   //cStr:=strtran(lower(alltrim(cStr)),"_"," ")
   cStr:=lower(alltrim(cStr))
   nlen:=len(cStr)
   FOR n:=1 TO nLen
      ch:=substr(cStr,n,1)
      c+=if(l,upper(ch),ch)
      l:=(ch==" ".or.ch=="_")
   NEXT
RETURN(c)

FUNCTION __GetMessageFont( nWeight )
   LOCAL cBuff, n, ncm := (struct NONCLIENTMETRICS)
   ncm:cbSize := ncm:Sizeof()

   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, ncm:Sizeof(), @ncm, 0 )

   IF nWeight != NIL
      ncm:lfMessageFont:lfWeight := nWeight
   ENDIF

RETURN CreateFontIndirect( ncm:lfMessageFont )

FUNCTION __DrawSpecialChar( hDC, aRect, nSign, lBold, nPoint )
   //  48 Min
   //  49 Max
   //  50 Restore
   //  98 Checkmark
   // 105 Bullet
   // 114 Close
   LOCAL hFont, hOldFont, nOldMode
   LOCAL lf := (struct LOGFONT)
   DEFAULT nPoint TO -( aRect[4]-aRect[2]-5 )
   hFont    := __FontCreate( "Marlett", nPoint, lBold, .F. )
   hOldFont := SelectObject ( hDC, hFont )
   nOldMode := SetBkMode( hDC, TRANSPARENT )
   _DrawText( hDC, CHR(nSign), aRect, DT_CENTER+DT_SINGLELINE+DT_VCENTER )
   SetBkMode( hDC, nOldMode )
   SelectObject( hDC, hOldFont )
   DeleteObject( hFont )
RETURN NIL

FUNCTION __Draw3DRect( hdc, rcItem, oTopLeftColor, oBottomRightColor, lPen )
   LOCAL hPen
   LOCAL hPenOld
   DEFAULT lPen TO .F.
   IF !lPen
      hPen := CreatePen( PS_SOLID, 1, oTopLeftColor )
    ELSE
      hPen := oTopLeftColor
   ENDIF
   hPenOld := SelectObject( hdc, hPen )
   MoveToEx( hdc, rcItem[1], rcItem[4] - 1 )
   LineTo( hdc, rcItem[1], rcItem[2] )
   LineTo( hdc, rcItem[3] - 1, rcItem[2] )
   SelectObject( hdc, hPenOld )
   IF !lPen
      DeleteObject( hPen )
   ENDIF
   IF rcItem[1] <> rcItem[3]
      IF !lPen
         hPen := CreatePen(PS_SOLID, 1, oBottomRightColor )
       ELSE
         hPen := oBottomRightColor
      ENDIF
      hPenOld := SelectObject(hdc, hPen)
      LineTo( hdc, rcItem[3] - 1, rcItem[4] - 1 )
      LineTo( hdc, rcItem[1], rcItem[4] - 1 )
      SelectObject( hdc, hPenOld )
      IF !lPen
         DeleteObject( hPen )
      ENDIF
   END
RETURN NIL

FUNCTION OffSet( aArray, x, y )
   LOCAL n
   FOR n := 1 TO LEN( aArray ) STEP 2
      aArray[n]   += x
      aArray[n+1] += y
   NEXT
RETURN NIL

FUNCTION ToUnicode( cString )
   local i, cTemp := ""
   IF cString == NIL
      RETURN NIL
   ENDIF
   FOR i = 1 to LEN(cString)
       cTemp += SUBSTR( cString, i, 1 ) + chr( 0 )
   NEXT
RETURN cTemp + chr( 0 )

FUNCTION isChildOfActiveWindow(hWnd)
   LOCAL hNowActive := GetActiveWindow()
   LOCAL lRet       := isChild( hNowActive, hWnd )
   LOCAL hParent
   DO WHILE lRet
      hParent := GetParent( hWnd )
      IF hNowActive != hParent
         IF GetWindowLong( hParent, GWL_STYLE ) & WS_CHILD != 0
            hWnd:=hParent
          ELSE
            lRet:=.F.
         ENDIF
       ELSE
         EXIT
      ENDIF
   ENDDO
RETURN lRet

FUNCTION __GetOsVersion()
   LOCAL OsVersion := (struct OSVERSIONINFOEX)
   GetVersionEx( @OsVersion )
RETURN OsVersion

STATIC FUNCTION cTypes2aTypes(cTypes)
   LOCAL aTypes:={}
   LOCAL cType
   LOCAL i,j

   ctypes:=strtran(ctypes," ","")
   cTypes:=substr(cTypes,2)
   DO While !EMPTY(cTypes)
      IF LEFT(ctypes,1)=="{"
         AADD(atypes,ctypes2atypes(@ctypes))
         cTypes:=SUBSTR(ctypes,1)
      ELSE
         if (i :=AT( ",", cTypes )) > 0
            ctype:=Left( cTypes, i - 1 )
         else
            ctype:=ctypes
         endif
         IF (j:=AT("}",ctype)) > 0

            // TBD: add multiple arrays!!

             ctype:=LEFT(ctype,j-1)
             IF !EMPTY(ctype)
               AADD(atypes,ctype)
             endif
             cTypes := SubStr( cTypes, j + 1 )
             exit
         Endif
         if !EMPTY(cType)
            AADD(atypes,ctype)
         endif
         cTypes := SubStr( cTypes, i + 1 )
      Endif
   enddo
RETURN(aTypes)

FUNCTION __DeleteEvents( aEvents, aDelEvents )
   LOCAL n, x, cEvent
   FOR EACH cEvent IN aDelEvents
       TRY
          FOR n := 1 TO LEN( aEvents )
              FOR x := 1 TO LEN( aEvents[n][2] )
                  IF UPPER( aEvents[n][2][x][1] ) == UPPER( cEvent )
                     ADEL( aEvents[n][2], x, .T. )
                     IF EMPTY( aEvents[n][2] )
                        ADEL( aEvents, n, .T. )
                     ENDIF
                     BREAK
                  ENDIF
              NEXT
          NEXT
        catch
      END
   NEXT
RETURN NIL


FUNCTION aEvents()
   RETURN { ;
            {"Clipboard",   {;
                            { "OnChangeCbChain"    , "", "" },;
                            { "OnDrawClipboard"    , "", "" } } },;
            {"Command",     {;
                            { "OnClick"            , "", "" },;
                            { "OnCancel"           , "", "" },;
                            { "OnClose"            , "", "" },;
                            { "OnCommand"          , "", "" },;
                            { "OnOk"               , "", "" },;
                            { "OnNotify"           , "", "" },;
                            { "OnParentCommand"    , "", "" },;
                            { "OnSysCommand"       , "", "" },;
                            { "OnToolTipNotify"    , "", "" } } },;
            {"Color",       {;
                            { "OnCtlColorBtn"      , "", "" },;
                            { "OnCtlColorDlg"      , "", "" },;
                            { "OnCtlColorEdit"     , "", "" },;
                            { "OnCtlColorListBox"  , "", "" },;
                            { "OnCtlColorScrollBar", "", "" },;
                            { "OnCtlColorStatic"   , "", "" },;
                            { "OnSysColorChange"   , "", "" } } },;
            {"Drag & Drop", {;
                            { "OnDropFiles"        , "", "" } } },;
            {"Drawing",     {;
                            { "OnDrawItem"         , "", "" },;
                            { "OnEraseBkGnd"       , "", "" },;
                            { "OnPaint"            , "", "" } } },;
            {"Editing",     {;
                            { "OnClear"            , "", "" },;
                            { "OnCopy"             , "", "" },;
                            { "OnCut"              , "", "" },;
                            { "OnPaste"            , "", "" },;
                            { "OnUndo"             , "", "" } } },;
            {"Keyboard",    {;
                            { "OnChar"             , "", "" },;
                            { "OnChildChar"        , "", "" },;
                            { "OnChildGetDlgCode"  , "", "" },;
                            { "OnChildKeyDown"     , "", "" },;
                            { "OnGetDlgCode"       , "", "" },;
                            { "OnHelp"             , "", "" },;
                            { "OnKeyDown"          , "", "" },;
                            { "OnKeyUp"            , "", "" },;
                            { "OnHotKey"           , "", "" },;
                            { "OnSysChar"          , "", "" },;
                            { "OnSysKeyDown"       , "", "" },;
                            { "OnSysKeyUp"         , "", "" } } },;
            {"Layout",      { ;
                            { "OnEnterSizeMove"    , "", "" },;
                            { "OnExitSizeMove"     , "", "" },;
                            { "OnGetMinMaxInfo"    , "", "" },;
                            { "OnMeasureItem"      , "", "" },;
                            { "OnMove"             , "", "" },;
                            { "OnMoving"           , "", "" },;
                            { "OnParentMove"       , "", "" },;
                            { "OnParentSize"       , "", "" },;
                            { "OnSize"             , "", "" },;
                            { "OnSizing"           , "", "" },;
                            { "OnWindowPaint"      , "", "" },;
                            { "OnWindowPosChanged" , "", "" },;
                            { "OnWindowPosChanging", "", "" } } },;
            {"Menu",        {;
                            { "OnCancelMode"       , "", "" },;
                            { "OnContextMenu"      , "", "" },;
                            { "OnEnterMenuLoop"    , "", "" },;
                            { "OnExitMenuLoop"     , "", "" },;
                            { "OnInitMenuPopup"    , "", "" },;
                            { "OnMenuChar"         , "", "" },;
                            { "OnMenuCommand"      , "", "" },;
                            { "OnMenuSelect"       , "", "" },;
                            { "OnNextMenu"         , "", "" } } },;
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
            {"Non Client",  { ;
                            { "OnNCActivate"       , "", "" },;
                            { "OnNCCalcSize"       , "", "" },;
                            { "OnNCCreate"         , "", "" },;
                            { "OnNCDestroy"        , "", "" },;
                            { "OnNCHitTest"        , "", "" },;
                            { "OnNCLButtonDown"    , "", "" },;
                            { "OnNCLButtonUp"      , "", "" },;
                            { "OnNCLButtonDblClk"  , "", "" },;
                            { "OnNCRButtonUp"      , "", "" },;
                            { "OnNCRButtonDown"    , "", "" },;
                            { "OnNCRButtonDblClk"  , "", "" },;
                            { "OnNCMButtonUp"      , "", "" },;
                            { "OnNCMButtonDown"    , "", "" },;
                            { "OnNCMButtonDblClk"  , "", "" },;
                            { "OnNCXButtonUp"      , "", "" },;
                            { "OnNCXButtonDown"    , "", "" },;
                            { "OnNCXButtonDblClk"  , "", "" },;
                            { "OnNCMouseHover"     , "", "" },;
                            { "OnNCMouseLeave"     , "", "" },;
                            { "OnNCMouseMove"      , "", "" },;
                            { "OnNCPaint"          , "", "" } } },;
            {"Object",      {;
                            { "OnInit"             , "", "" } } },;
            {"Parent",      {;
                            { "OnParentDrawItem"   , "", "" },;
                            { "OnParentMeasureItem", "", "" },;
                            { "OnParentNotify"     , "", "" },;
                            { "OnParentSysCommand" , "", "" } } },;
            {"Scroll",      {;
                            { "OnHorzScroll"       , "", "" },;
                            { "OnVertScroll"       , "", "" } } },;
            {"Timer",       {;
                            { "OnTimer"            , "", "" } } },;
            {"Interface",   {;
                            { "OnLoad"             , "", "" },;
                            { "OnActivate"         , "", "" },;
                            { "OnCreate"           , "", "" },;
                            { "OnDestroy"          , "", "" },;
                            { "OnEnable"           , "", "" },;
                            { "OnHideWindow"       , "", "" },;
                            { "OnInitDialog"       , "", "" },;
                            { "OnKillFocus"        , "", "" },;
                            { "OnMessage"          , "", "" },;
                            { "OnSetCursor"        , "", "" },;
                            { "OnSetFocus"         , "", "" },;
                            { "OnSetFont"          , "", "" },;
                            { "OnSetText"          , "", "" },;
                            { "OnShowWindow"       , "", "" },;
                            { "OnUserMsg"          , "", "" } } },;
            {"User",        {;
                            { "UserMethod1"        , "", "" },;
                            { "UserMethod2"        , "", "" },;
                            { "UserMethod3"        , "", "" },;
                            { "UserMethod4"        , "", "" },;
                            { "UserMethod5"        , "", "" },;
                            { "UserMethod6"        , "", "" },;
                            { "UserMethod7"        , "", "" },;
                            { "UserMethod8"        , "", "" },;
                            { "UserMethod9"        , "", "" },;
                            { "UserMethod10"       , "", "" },;
                            { "UserMethod11"       , "", "" },;
                            { "UserMethod12"       , "", "" },;
                            { "UserMethod13"       , "", "" },;
                            { "UserMethod14"       , "", "" },;
                            { "UserMethod15"       , "", "" },;
                            { "UserMethod16"       , "", "" },;
                            { "UserMethod17"       , "", "" },;
                            { "UserMethod18"       , "", "" },;
                            { "UserMethod19"       , "", "" } } } }

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS WinForm INHERIT Window
   DATA Sizeable               EXPORTED INIT .T.  // to avoid crash loading old projects

   DATA Params                 EXPORTED
   DATA ControlId              PROTECTED  INIT 101
   DATA TabOrder               EXPORTED
   DATA DllInstance            EXPORTED
   ACCESS IsMDIContainter  INLINE ::MDIContainter

   DATA Modal                   PUBLISHED INIT .F.
   DATA UserVariables           PUBLISHED INIT ""
   DATA ShowMode                PUBLISHED INIT 1
   DATA AutoClose               PUBLISHED INIT .T.
   DATA VertScroll              PUBLISHED INIT .F.
   DATA HorzScroll              PUBLISHED INIT .F.
   DATA MDIClient               PUBLISHED
   DATA MinWidth                PUBLISHED INIT 0

   DATA MinHeight               PUBLISHED INIT 0
   DATA MaxWidth                PUBLISHED INIT 0
   DATA MaxHeight               PUBLISHED INIT 0
   DATA ShowInTaskBar           PUBLISHED INIT .T.

   PROPERTY ToolWindow    INDEX WS_EX_TOOLWINDOW    READ xToolWindow      WRITE SetExStyle      DEFAULT .F. PROTECTED
   PROPERTY TopMost       INDEX WS_EX_TOPMOST       READ xTopMost         WRITE SetExStyle      DEFAULT .F. PROTECTED
   PROPERTY ThickFrame    INDEX WS_THICKFRAME       READ xThickFrame      WRITE SetStyle        DEFAULT .T. PROTECTED
   PROPERTY Resizable     INDEX WS_THICKFRAME       READ xResizable       WRITE SetStyle        DEFAULT .T. PROTECTED
   PROPERTY MaximizeBox   INDEX WS_MAXIMIZEBOX      READ xMaximizeBox     WRITE SetStyle        DEFAULT .T. PROTECTED
   PROPERTY MinimizeBox   INDEX WS_MINIMIZEBOX      READ xMinimizeBox     WRITE SetStyle        DEFAULT .T. PROTECTED
   PROPERTY CaptionBar    INDEX WS_CAPTION          READ xCaptionBar      WRITE SetStyle        DEFAULT .T. PROTECTED
   PROPERTY SysMenu       INDEX WS_SYSMENU          READ xSysMenu         WRITE SetStyle        DEFAULT .T. PROTECTED
   PROPERTY FrameStyle                              READ xFrameStyle      WRITE __SetFrameStyle DEFAULT 1   PROTECTED
   PROPERTY DlgModalFrame INDEX WS_EX_DLGMODALFRAME READ xDlgModalFrame   WRITE SetExStyle      DEFAULT .F. PROTECTED
   PROPERTY Icon                                    READ xIcon            WRITE SetFormIcon                 PROTECTED INVERT
   PROPERTY Opacity                                 READ xOpacity         WRITE SetOpacity      DEFAULT 100 PROTECTED
   PROPERTY ImageList                               READ xImageList       WRITE SetImageList                PROTECTED
   PROPERTY BitmapMask                              READ xBitmapMask      WRITE __SetBitmapMask             PROTECTED INVERT
   PROPERTY BitmapMaskColor                         READ xBitmapMaskColor WRITE __SetBitmapMaskColor        PROTECTED

   //compatibility ONLY, forms do not set "Border" property
   DATA Border                 EXPORTED INIT .F.
   DATA Property               EXPORTED
   DATA xMDIChild              EXPORTED INIT .F.
   ACCESS MDIChild             INLINE IIF( ::ClsName == "MDIChild", ::ExStyle & WS_EX_MDICHILD != 0, ::xMDIChild ) PERSISTENT
   ASSIGN MDIChild(l)          INLINE IIF( ::__ClassInst != NIL .AND. l .AND. ::Modal, ::Modal := .F., ), ::xMDIChild := l, IIF( ::ClsName == "MDIChild", ::SetExStyle( WS_EX_MDICHILD, l ), )

   DATA xMdiContainer          EXPORTED  INIT .F.
   ACCESS MdiContainer         INLINE    ::xMdiContainer PERSISTENT
   ASSIGN MdiContainer(l)      INLINE    ::xMdiContainer := l, ::IsContainer := .F., ::__CreateMDI(l)

   DATA BackgroundImage        PUBLISHED



   DATA Visible                EXPORTED  INIT .T.
   DATA MaskBitmap             EXPORTED
   DATA __Frame_Styles         EXPORTED  INIT {"Overlapped", "PopUp"}
   DATA __Show_Modes           EXPORTED  INIT { "Normal", "Minimized", "Maximized", "NoActivate" }
   DATA Ole                    EXPORTED
   DATA AppParam               EXPORTED
   
   ACCESS Form                 INLINE Self

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD GetNextControlId()

   // MDI Messages
   METHOD MdiTileHorizontal( lTileDisable )         INLINE lTileDisable := IFNIL(lTileDisable,.F.,lTileDisable),;
                                                           SendMessage( ::MDIClient:hWnd, WM_MDITILE, MDITILE_HORIZONTAL + IIF( !lTileDisable, MDITILE_SKIPDISABLED, 0 ) )
   METHOD MdiTileVertical( lTileDisable )           INLINE lTileDisable := IFNIL(lTileDisable,.F.,lTileDisable),;
                                                           SendMessage( ::MDIClient:hWnd, WM_MDITILE, MDITILE_VERTICAL + IIF( !lTileDisable, MDITILE_SKIPDISABLED, 0 ) )
   METHOD MdiCascade( lTileDisable )                INLINE lTileDisable := IFNIL(lTileDisable,.F.,lTileDisable),;
                                                           SendMessage( ::MDIClient:hWnd, WM_MDICASCADE, IIF( !lTileDisable, MDITILE_SKIPDISABLED, 0 ) )

   METHOD MdiIconArrange()                          INLINE SendMessage( ::MDIClient:hWnd, WM_MDIICONARRANGE )
   METHOD MdiNext()                                 INLINE SendMessage( ::Application:MainForm:MDIClient:hWnd, WM_MDINEXT )

   METHOD MdiActivate()
   METHOD MdiDestroy()
   METHOD MdiClose()
   METHOD MdiGetActive()

   METHOD MdiMaximize()
   METHOD MdiRestore()
   METHOD SetFormIcon()

   METHOD Maximize()            INLINE ShowWindow( ::hWnd, SW_SHOWMAXIMIZED)
   METHOD Minimize()            INLINE ShowWindow( ::hWnd, SW_SHOWMINIMIZED)
   METHOD Restore()             INLINE ShowWindow( ::hWnd, SW_RESTORE)
   METHOD SetOpacity()
   METHOD Show()

   METHOD SaveLayout()
   METHOD RestoreLayout()
   METHOD OnSize()

   ACCESS ActiveForm            INLINE ObjFromHandle( GetActiveWindow() )

   METHOD __RefreshPosNo() INLINE NIL // Compatibility
   METHOD __SetBitmapMask()
   METHOD __SetBitmapMaskColor()
   METHOD __PaintBakgndImage()
   METHOD __PrcMdiMenu()
   METHOD SetImageList()

   METHOD SetBackColor( nColor, lRepaint ) INLINE ::Super:SetBackColor( nColor, lRepaint ), IIF( ::BackgroundImage != NIL .AND. ::BackgroundImage:hDIB != NIL, ::BackgroundImage:__SetImageName( ::BackgroundImage:xImageName ), )

   METHOD OnSysCommand()
   METHOD SetInstance()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent, aParameters, cProjectName ) CLASS WinForm
   LOCAL oApp, hInst, hPointer
   
   IF VALTYPE( oParent ) == "N"
      ::__hParent := oParent
      oParent := NIL
   ENDIF
   
   IF ::DllInstance == NIL .AND. cProjectName != NIL
      IF ::Application == NIL
         hInst := GetModuleHandle( cProjectName )
         hPointer := HB_FuncPtr( "__" + cProjectName )
         HB_Exec( hPointer, ,NIL, hInst )
         ::DllInstance := hInst
       ELSE
         ::DllInstance := GetModuleHandle( cProjectName )
      ENDIF
   ENDIF
   
   IF ::Application != NIL
      DEFAULT ::Application:MainForm TO Self
   ENDIF

   DEFAULT ::__xCtrlName TO "Form"
   DEFAULT ::ClsName     TO "Vxh_Form"

   ::Params   := aParameters
   ::Property := Hash()
   HSetCaseMatch( ::Property, .F. )

   Super:Init( oParent )

   IF !::ClsName == "MDIChild"
      ::MDIClient := MDIClient( Self )
   ENDIF
   ::__lCopyCut := .T.
   ::__IsForm   := .T.

   __DeleteEvents( ::Events,{ "OnClick" } )

RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetInstance( cProjectName, oOle ) CLASS WinForm
   LOCAL hInst, hPointer
   ::Ole := oOle
   IF ::DllInstance == NIL
      hInst := GetModuleHandle( cProjectName )
      hPointer := HB_FuncPtr( "__" + cProjectName )
      HB_Exec( hPointer, ,NIL, hInst )
      ::DllInstance := hInst
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create( hoParent ) CLASS WinForm
   LOCAL hIcon
   
   IF ::__hParent != NIL
      hoParent := ::__hParent
   ENDIF
   
   IF ::__ClassInst != NIL .AND. VALTYPE( ::xIcon ) == "A"
      ::xIcon := ::xIcon[1]
   ENDIF

   IF !::ShowInTaskBar .AND. ::Parent == NIL .AND. ::__ClassInst == NIL .AND. ::Application:DllInstance == NIL
      ::__TaskBarParent := CreateDialogIndirect( ::AppInstance, __GetTemplate( Self ), 0, NIL )
      IF !EMPTY( ::__hIcon )
         SendMessage( ::__TaskBarParent, WM_SETICON, ICON_BIG, ::__hIcon )
      ENDIF
      hoParent := ::__TaskBarParent
   ENDIF

   Super:Create( hoParent )
   IF ::__OnInitCanceled
      RETURN NIL
   ENDIF

   ::SetIcon( ICON_SMALL, IIF( !EMPTY( ::__hIcon ), ::__hIcon, LoadIcon( 0, IDI_WINLOGO ) ) )
   ::SetIcon( ICON_BIG, IIF( !EMPTY( ::__hIcon ), ::__hIcon, LoadIcon( 0, IDI_WINLOGO ) ) )
   ::SetOpacity( ::xOpacity )

   IF ::BackgroundImage != NIL
      ::BackgroundImage:Create()
   ENDIF

   #ifdef VXH_ENTERPRISE
   //IF VALTYPE( ::Application:Params ) == "A" .AND. LEN( ::Application:Params ) == 3 .AND. ::Application:Params[1] == "REMOTE"
   //   ::Application:RemoteSocket := WinSock( Self, .F. )
   //   ::Application:RemoteSocket:LocalPort := VAL( ::Application:Params[3] )
   //   ::Application:RemoteSocket:Listen()
   //ENDIF
   #endif
RETURN Self

METHOD OnSize() CLASS WinForm
   IF ::BackgroundImage != NIL .AND. !EMPTY( ::BackgroundImage:ImageName )
      ::CallWindowProc()
      ::InvalidateRect()
      ::RedrawWindow( , , RDW_UPDATENOW )
      AEVAL( ::Children, {|o| o:InvalidateRect()} )
      RETURN 0
   ENDIF
RETURN Self

METHOD OnSysCommand( nwParam, nlParam ) CLASS WinForm
   LOCAL oChild, hDef
   IF nwParam IN {SC_MAXIMIZE,SC_MAXIMIZE2,SC_RESTORE,SC_RESTORE2}
      ::CallWindowProc()

      hDef := BeginDeferWindowPos( LEN( ::Children ) )
      FOR EACH oChild IN ::Children
          IF oChild != NIL .AND. oChild:hWnd != ::hWnd
             oChild:__OnParentSize( ::ClientWidth, ::ClientHeight, @hDef )
             oChild:InvalidateRect(, .F. )
          ENDIF
      NEXT
      EndDeferWindowPos( hDef )
      RETURN 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD SaveLayout( cIniFile, cSection ) CLASS WinForm
   LOCAL oIni, rc := (struct RECT)
   GetWindowRect( ::hWnd, @rc )
   IF EMPTY( cIniFile )
      oIni := ::Application:IniFile
    ELSE
      oIni := IniFile( cIniFile )
   ENDIF
   DEFAULT cSection TO "Main"
   oIni:WriteString( cSection, ::Application:Name + "_" + ::Name, xSTR( rc:Left ) + ", " + xSTR( rc:Top ) + ", " + xSTR( ::Width ) + ", " + xSTR( ::Height ) + ", " + xSTR( ::__GetShowMode() ) )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD RestoreLayout( cIniFile, cSection, lAllowOut ) CLASS WinForm
   LOCAL c, oIni, aRect, aPos, hMonitor
   DEFAULT lAllowOut TO .T.
   IF EMPTY( cIniFile )
      oIni := ::Application:IniFile
    ELSE
      oIni := IniFile( cIniFile )
   ENDIF
   c := oIni:ReadString( cSection, ::Application:Name + "_" + ::Name, "" )
   IF !EMPTY(c)
      aPos := {&c}
      TRY
         DEFAULT aPos[1] TO 0
         DEFAULT aPos[2] TO 0
         DEFAULT aPos[3] TO 0
         DEFAULT aPos[4] TO 0
         DEFAULT aPos[5] TO 1
      CATCH
      END
      IF LEN( aPos ) > 4
         ::ShowMode := aPos[5]
         IF aPos[5] == 3
            RETURN Self
         ENDIF
      endif
      ::xLeft   := aPos[1]
      ::xTop    := aPos[2]
      ::xWidth  := aPos[3]
      ::xHeight := aPos[4]

      IF !lAllowOut
         aRect := GetDesktopRect()

         ::xLeft := MAX( 0, ::xLeft )
         ::xTop  := MAX( 0, ::xTop )

         IF ::xLeft + ::xWidth > aRect[3] .AND. ::xLeft > 0
            ::xLeft -= ( ::xLeft + ::xWidth ) - aRect[3]
         ENDIF
         IF ::xTop + ::xHeight > aRect[4] .AND. ::xTop > 0
            ::xTop  -= ( ::xTop + ::xHeight ) - aRect[4]
         ENDIF

         ::xLeft := MAX( 0, ::xLeft )
         ::xTop  := MAX( 0, ::xTop )
      ENDIF
      ::MoveWindow()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __PaintBakgndImage( hDC ) CLASS WinForm
   LOCAL cPaint, hBrush, hMemDC1, hOldBitmap1, oChild
   LOCAL hMemBitmap, hOldBitmap, hMemDC
   IF ::BackgroundImage != NIL .AND. ::BackgroundImage:hDIB != NIL
      DEFAULT hDC TO ::Drawing:hDC

      hBrush := ::BkBrush
      IF !::ClsName == "Dialog"
         DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )
      ENDIF

      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)

      IF hBrush == NIL .AND. ::ClsName == "Dialog" .AND. ::Application:OsVersion:dwMajorVersion >= 5
         ::OpenThemeData()
         DrawThemeBackground( ::hTheme, hMemDC, TABP_BODY, 0, { 0, 0, ::Width, ::Height } )
         ::CloseThemeData()
       ELSE
         _FillRect( hMemDC, { ::LeftMargin, ::TopMargin, ::ClientWidth, ::ClientHeight }, hBrush )
      ENDIF

      ::BackgroundImage:Draw( hMemDC )

      BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )

      FOR EACH oChild IN ::Children
          IF oChild:IsChild
             IF oChild:__hBrush != NIL
                DeleteObject( oChild:__hBrush )
             ENDIF
             DEFAULT oChild:__BackMargin TO 0
             DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )
             hMemDC1      := CreateCompatibleDC( hDC )
             hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )
             BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin+oChild:CaptionHeight, SRCCOPY )
             oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )
             SelectObject( hMemDC1,  hOldBitmap1 )
             DeleteDC( hMemDC1 )
          ENDIF
      NEXT

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )

      RETURN 1
   ENDIF
RETURN NIL

METHOD Show( nShow ) CLASS WinForm
   LOCAL hDef, oChild, nRet, hDC, hMemDC, hMemDC1, hOldBitmap1, hOldBitmap, hMemBitmap, hBrush, o, n
   DEFAULT nShow TO ::ShowMode
   IF ::__OnInitCanceled
      RETURN NIL
   ENDIF

   IF nShow == SW_HIDE
      RETURN ::Hide()
   ENDIF

   IF ::hWnd != NIL
      IF !::__lShown
         ::__lShown := .T.
         IF ::MDIContainer
            ::MDIClient:Show()
         ENDIF

         IF ::__hBmpRgn != NIL
            hDC        := ::Drawing:hDC
            hBrush     := ::BkBrush

            hMemDC     := CreateCompatibleDC( hDC )
            hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
            hOldBitmap := SelectObject( hMemDC, hMemBitmap)

            _FillRect( hMemDC, { ::LeftMargin, ::TopMargin, ::ClientWidth, ::ClientHeight }, hBrush )

            BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )

            FOR EACH oChild IN ::Children
                IF oChild:__hBrush != NIL
                   DeleteObject( oChild:__hBrush )
                ENDIF
                DEFAULT oChild:__BackMargin TO 0
                DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )
                hMemDC1      := CreateCompatibleDC( hDC )
                hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )
                BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin, SRCCOPY )
                oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )
                SelectObject( hMemDC1,  hOldBitmap1 )
                DeleteDC( hMemDC1 )
            NEXT

            SelectObject( hMemDC,  hOldBitmap )
            DeleteObject( hMemBitmap )
            DeleteDC( hMemDC )
         ENDIF

         ::__FixDocking()
         
         nRet := ExecuteEvent( "OnLoad", Self )

         IF ::Property != NIL .AND. ::__ClassInst == NIL

            FOR n := 1 TO LEN( ::Property:Keys )
                o := HGetValueAt( ::Property, n )
                IF o:__xCtrlName == "TabPage" .AND. !o:Visible
                   o:__SetVisible( .F., .T. )
                ENDIF
            NEXT

         ENDIF
         
         ODEFAULT nRet TO ::OnLoad( Self )
         nShow := ::ShowMode
      ENDIF
      IF ::AnimationStyle != 0 .AND. ::__ClassInst == NIL
         ::SendMessage( WM_SIZE, 0, MAKELONG( ::ClientWidth, ::ClientHeight ) )
         ::UpdateChildren()
         ::Animate( 1000, ::AnimationStyle )
         ::UpdateChildren()
       ELSE
         ShowWindow( ::hWnd, IIF( ::__ClassInst == NIL, nShow, SW_SHOW ) )
         
         IF ::MDIContainer
            ::PostMessage( WM_SIZE, 0, MAKELPARAM( ::ClientWidth, ::ClientHeight ) )
         ENDIF
      ENDIF
   ENDIF
   IF ::LeftSplitter != NIL
      ::LeftSplitter:Show()
   ENDIF
   IF ::TopSplitter != NIL
      ::TopSplitter:Show()
   ENDIF
   IF ::RightSplitter != NIL
      ::RightSplitter:Show()
   ENDIF
   IF ::BottomSplitter != NIL
      ::BottomSplitter:Show()
   ENDIF
   ::Style := ::Style | WS_VISIBLE
   IF ::Parent != NIL
      ::Parent:SendMessage( WM_SIZE, 0, MAKELONG( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __PrcMdiMenu( nId ) CLASS WinForm
   LOCAL oMdi := ::MDIClient:GetActive()

   IF oMdi != NIL
      DO CASE
         CASE nId == SC_MINIMIZE
              oMdi:Minimize()

         CASE nId == SC_NEXTWINDOW
              oMdi:MdiNext()

         CASE nId == SC_RESTORE
              oMdi:MdiRestore()

         CASE nId == SC_CLOSE
              oMdi:MdiClose()
      ENDCASE
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD GetNextControlId() CLASS WinForm
   ::ControlId++
   WHILE ASCAN( ::Children, {|o| o:Id == ::ControlId } ) > 0
      ::ControlId++
   END
RETURN ::ControlId

//-----------------------------------------------------------------------------------------------
METHOD MdiActivate( oMdi ) CLASS WinForm
   IF oMdi == NIL
      SendMessage( ::Parent:hWnd, WM_MDIACTIVATE, ::hWnd )
    ELSE
      ::SendMessage( WM_MDIACTIVATE, oMdi:hWnd )
   ENDIF
RETURN Self
//-----------------------------------------------------------------------------------------------
METHOD MdiClose( oMdi ) CLASS WinForm
   IF oMdi == NIL
      SendMessage( ::hWnd, WM_SYSCOMMAND, SC_CLOSE )
    ELSE
      SendMessage( oMdi:hWnd, WM_SYSCOMMAND, SC_CLOSE )
   ENDIF
RETURN Self
//-----------------------------------------------------------------------------------------------

METHOD MdiDestroy( oMdi ) CLASS WinForm
   IF oMdi == NIL
      SendMessage( ::Parent:hWnd, WM_MDIDESTROY, ::hWnd )
    ELSE
      ::SendMessage( WM_MDIDESTROY, oMdi:hWnd )
   ENDIF
RETURN Self
//-----------------------------------------------------------------------------------------------
METHOD MdiMaximize( oMdi ) CLASS WinForm
   IF oMdi == NIL
      SendMessage( ::Parent:hWnd, WM_MDIMAXIMIZE, ::hWnd )
    ELSE
      ::SendMessage( WM_MDIMAXIMIZE, oMdi:hWnd )
   ENDIF
RETURN Self
//-----------------------------------------------------------------------------------------------
METHOD MdiRestore( oMdi ) CLASS WinForm
   IF oMdi == NIL
      SendMessage( ::Parent:hWnd, WM_MDIRESTORE, ::hWnd )
    ELSE
      ::SendMessage( WM_MDIRESTORE, oMdi:hWnd )
   ENDIF
RETURN Self
//-----------------------------------------------------------------------------------------------
METHOD MdiGetActive() CLASS WinForm
   LOCAL n, hWnd := SendMessage( ::MDIClient:hWnd, WM_MDIGETACTIVE )
   IF( n := ASCAN( ::MDIClient:Children, {|o|o:hWnd == hWnd} ) ) > 0
      RETURN ::MDIClient:Children[n]
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD SetFormIcon( cIcon ) CLASS WinForm
   IF VALTYPE( cIcon ) == "A"
      cIcon := IIF( ::__ClassInst != NIL .AND. VALTYPE( cIcon[1] ) == "C", cIcon[1], cIcon[2] )
   ENDIF
   IF !EMPTY( ::__hIcon )
      DestroyIcon( ::__hIcon )
      ::__hIcon := NIL
      IF ::__ClassInst != NIL
         IF !EMPTY( ::xIcon )
            ::Application:Project:RemoveImage( ::xIcon, Self )
         ENDIF
      ENDIF
   ENDIF

   IF VALTYPE( cIcon ) == "C"
      IF AT( ".", cIcon ) == 0
         ::__hIcon := LoadIcon( ::AppInstance, cIcon )
       ELSE
         ::__hIcon := LoadImage( ::AppInstance, cIcon, IMAGE_ICON,,, LR_LOADFROMFILE )
      ENDIF
    ELSEIF VALTYPE( cIcon ) == "N"
      ::__hIcon := cIcon
   ENDIF
   IF ::__hIcon == 0
      ::__hIcon := NIL
   ENDIF
   IF ::hWnd != NIL
      ::SetIcon( ICON_SMALL, ::__hIcon )
      ::SetIcon( ICON_BIG, ::__hIcon )
   ENDIF

   IF ::__ClassInst != NIL
      IF !EMPTY( cIcon )
         ::Application:Project:AddImage( cIcon, IMAGE_ICON, Self, .T., ::hWnd == ::Application:Project:Forms[1]:hWnd )
      ENDIF
   ENDIF
RETURN Self

METHOD __SetBitmapMaskColor( nColor ) CLASS WinForm
   LOCAL cBmp, hBitmap
   IF ::__ClassInst == NIL .AND. ::hWnd != NIL
      cBmp := ::xBitmapMask
      IF VALTYPE( ::xBitmapMask ) == "A"
         cBmp := ::xBitmapMask[2]
      ENDIF

      IF ::BkBrush != NIL
         DeleteObject( ::BkBrush )
         ::BkBrush := NIL
      ENDIF

      SetWindowRgn( ::hWnd, NIL )
      ::InvalidateRect(, .T.)
      ::Application:Yield()

      IF RIGHT( UPPER( cBmp ), 4 ) == ".BMP"
         hBitmap := LoadImage( ::Instance, cBmp, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_VGACOLOR )
       ELSE
         hBitmap := LoadBitmap( ::Instance, cBmp )
      ENDIF

      ::BkBrush   := CreatePatternBrush( hBitmap )

      ::__hRegion := BitmapToRegion( hBitmap, nColor )
      SetWindowRgn( ::hWnd, ::__hRegion, .T. )

      DeleteObject( hBitmap )
      InvalidateRgn( ::hWnd, ::__hRegion, .T. )

      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
      ::RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
      InvalidateRgn( ::hWnd, ::__hRegion, .T. )
   ENDIF
RETURN Self

METHOD __SetBitmapMask( cBmp ) CLASS WinForm
   LOCAL aSize, hBitmap
   IF ::__ClassInst != NIL
      IF VALTYPE( ::xBitmapMask ) == "A"
         ::xBitmapMask := ::xBitmapMask[1]
      ENDIF
      IF !EMPTY( ::xBitmapMask )
         ::Application:Project:RemoveImage( ::xBitmapMask, Self )
      ENDIF
      IF VALTYPE( cBmp ) == "A"
         cBmp := cBmp[1]
      ENDIF
      IF !EMPTY( cBmp )
         ::Application:Project:AddImage( cBmp, IMAGE_BITMAP, Self )
      ENDIF
    ELSE
      IF ::hWnd != NIL
         IF ::BkBrush != NIL
            DeleteObject( ::BkBrush )
            ::BkBrush := NIL
         ENDIF

         SetWindowRgn( ::hWnd, NIL )
         ::InvalidateRect(, .T.)
         ::Application:Yield()

         IF RIGHT( UPPER( cBmp ), 4 ) == ".BMP"
            hBitmap := LoadImage( ::Instance, cBmp, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_VGACOLOR )
          ELSE
            hBitmap := LoadBitmap( ::Instance, cBmp )
         ENDIF
         aSize       := GetBmpSize( hBitmap )
         ::Width     := aSize[1]
         ::Height    := aSize[2]

         ::BkBrush   := CreatePatternBrush( hBitmap )

         ::__hRegion := BitmapToRegion( hBitmap, ::BitmapMaskColor )
         SetWindowRgn( ::hWnd, ::__hRegion, .T. )

         DeleteObject( hBitmap )
         InvalidateRgn( ::hWnd, ::__hRegion, .T. )

         ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
         ::RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
         InvalidateRgn( ::hWnd, ::__hRegion, .T. )

      ENDIF

   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetOpacity( n ) CLASS WinForm
   LOCAL nStyle
   IF ::hWnd != NIL
      nStyle := GetWindowLong( ::hWnd, GWL_EXSTYLE )
      IF n < 100
         nStyle := nStyle | WS_EX_LAYERED
       ELSE
         nStyle := nStyle & NOT( WS_EX_LAYERED )
      ENDIF
      SetWindowLong( ::hWnd, GWL_EXSTYLE, nStyle )
      SetLayeredWindowAttributes( ::hWnd, NIL, ( 255 * n ) / 100, LWA_ALPHA )
   ENDIF
RETURN Self


//-----------------------------------------------------------------------------------------------
METHOD SetImageList() CLASS WinForm
   ::xImageList := __ChkComponent( Self, ::xImageList )
   __BrowseChildren( Self )
RETURN Self

//-----------------------------------------------------------------------------------------------
FUNCTION __BrowseChildren( oObj )
   LOCAL oChild
   IF oObj != NIL .AND. oObj:Children != NIL
      FOR EACH oChild IN oObj:Children
          IF oChild:ClsName != "DataGrid"
             TRY
                oChild:ImageIndex := oChild:ImageIndex
                oChild:SetWindowPos(, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
              catch
             END
             __BrowseChildren( oChild )
          ENDIF
      NEXT
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

FUNCTION XStr( xValue )
   LOCAL cType := VALTYPE( xValue )
   SWITCH cType
      CASE "P"
           xValue := ALLTRIM( STR( xValue ) )
           EXIT
      CASE "N"
           xValue := ALLTRIM( STR( xValue ) )
           EXIT
      CASE "D"
           xValue := DTOC( xValue )
           EXIT
      CASE "O"
           xValue := xValue:ClassName
           EXIT
      CASE "L"
           xValue := IIF( xValue, ".T.", ".F." )
           EXIT
      CASE "U"
           xValue := "NIL"
           EXIT
      CASE "B"
           xValue := "{|| block }"
           EXIT
   END
RETURN xValue

FUNCTION AssociateWith( cExt, cCode, cFile, cDesc, nIcon )
   LOCAL cKey, cVal, nErr, hKey, hSubKey, nDisp, cIcon
   DEFAULT nIcon TO 0

   cIcon := Alltrim( Str( nIcon ) )
   cKey := cCode
   cVal := cDesc
   IF (nErr := RegCreateKey( HKEY_CLASSES_ROOT, cKey, @hKey )) == 0
      nErr := RegSetValueEx(hKey, "",, REG_SZ, cVal )
      RegCloseKey(hKey)
   ENDIF

   cKey := cExt
   cVal := cCode
   IF (nErr:=RegCreateKey( HKEY_CLASSES_ROOT, cKey, @hKey )) == 0
      nErr := RegSetValueEx(hKey, "",, REG_SZ, cVal )
      RegCloseKey(hKey)
   ENDIF

   cKey := cCode
   cVal := cFile
   IF (nErr:=RegCreateKey( HKEY_CLASSES_ROOT, cKey, @hKey )) == 0
      IF (nErr:=RegCreateKey(hKey,"DefaultIcon", @hSubKey )) == 0
          nErr := RegSetValueEx(hSubKey, "",, REG_SZ, cVal+","+cIcon, MAX_PATH)
          RegCloseKey(hSubKey)
      ENDIF
      IF (nErr:=RegCreateKey(hKey,"shell", @hKey )) == 0
         IF (nErr:=RegCreateKey(hKey,"open", @hKey )) == 0
            IF (nErr:=RegCreateKey(hKey,"command", @hKey )) == 0
               nErr := RegSetValueEx(hKey, "",, REG_SZ, cVal+" %1", MAX_PATH)
            ENDIF
         ENDIF
      ENDIF
      RegCloseKey(hKey)
   ENDIF
RETURN(NIL)

CLASS __Animation
   DATA Owner        EXPORTED
   DATA __ClassInst  EXPORTED
   DATA ClsName      EXPORTED INIT "__Animation"

   DATA Type          PUBLISHED INIT 0
   DATA Speed         PUBLISHED INIT 1000
   DATA HideInvert    PUBLISHED INIT .T.

   ACCESS Parent          INLINE ::Owner
   ACCESS Form            INLINE ::Owner:Form
   ACCESS Application     INLINE __GetApplication()

   METHOD Init() CONSTRUCTOR
ENDCLASS

METHOD Init( oOwner ) CLASS __Animation
   ::Owner := oOwner
   IF oOwner:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
   ENDIF
RETURN Self
/*
FUNCTION MakePath( cPath )
   LOCAL n, aDirs, cDir
   aDirs := hb_atokens(cPath, "\" )
   cDir  := aDirs[1]
   FOR n := 2 TO LEN( aDirs )
       cDir += "\" + aDirs[n]
       IF !IsDirectory( cDir )
          MakeDir( cDir )
       ENDIF
   NEXT
RETURN NIL
*/
/*
FUNCTION __ResetClassInst( oObj )
   LOCAL aProperties, aProperty, cProperty, aObjProperties, cObjProperty, xObjValue, xValue, xObjInst, xInst

   aProperties := __ClsGetPropertiesAndValues( oObj )
   FOR EACH cProperty IN aProperties
       cProperty := cProperty[1]

       xValue := __objSendMsg( oObj, UPPER( cProperty ) )
       xInst  := __objSendMsg( oObj:__ClassInst, UPPER( cProperty ) )

       IF VALTYPE( xValue ) != "O"

          IF !xValue == xInst .AND. __objHasMsg( oObj, "X" + cProperty )
             __objSendMsg( oObj:__ClassInst, "_X" + cProperty, xValue )
          END

        ELSE
          //__ResetClassInst( xValue )

          aObjProperties := __ClsGetPropertiesAndValues( xValue )
          FOR EACH cObjProperty IN aObjProperties
              cObjProperty := cObjProperty[1]

              xObjValue := __objSendMsg( xValue, UPPER( cObjProperty ) )
              xObjInst  := __objSendMsg( xValue:__ClassInst, UPPER( cObjProperty ) )
              IF !xObjValue == xObjInst
                 __objSendMsg( xValue:__ClassInst, "_" + cObjProperty, xObjValue )
              ENDIF
          NEXT

       ENDIF
   NEXT

RETURN NIL
*/
FUNCTION __ResetClassInst( oObj )
   LOCAL aProperties, aProperty, cProperty, aObjProperties, cObjProperty, xObjValue, xValue, xObjInst, xInst
   LOCAL cPre
   aProperties := __ClsGetPropertiesAndValues( oObj )
   FOR EACH cProperty IN aProperties
       cProperty := cProperty[1]

       xValue := __objSendMsg( oObj, UPPER( cProperty ) )
       xInst  := __objSendMsg( oObj:__ClassInst, UPPER( cProperty ) )

       IF VALTYPE( xValue ) != "O"

          IF !xValue == xInst
             TRY
                cPre := "X"
                IF !__objHasMsg( oObj, "X" + cProperty )
                   cPre := ""
                ENDIF
                IF __objHasMsg( oObj, cPre + cProperty )
                   __objSendMsg( oObj:__ClassInst, "_" + cPre + cProperty, xValue )
                ENDIF
              CATCH
             END
          END

        ELSE
          __ResetClassInst( xValue )
       ENDIF
   NEXT

RETURN NIL

FUNCTION GetDesktopRect()
   LOCAL aDesktopRect := array(4)
   aDesktopRect[1] := GetSystemMetrics( SM_XVIRTUALSCREEN )
   aDesktopRect[2] := GetSystemMetrics( SM_YVIRTUALSCREEN )
   aDesktopRect[3] := aDesktopRect[1] + GetSystemMetrics( SM_CXVIRTUALSCREEN )
   aDesktopRect[4] := aDesktopRect[2] + GetSystemMetrics( SM_CYVIRTUALSCREEN )
RETURN aDesktopRect

/*
FUNCTION __ChkComponent( oObj, ocCompo )
   IF VALTYPE( ocCompo ) == "C" 
      IF oObj:Form != NIL .AND. oObj:Form:Property != NIL .AND. ASCAN( oObj:Form:Property:Keys, {|c| UPPER(c) == UPPER( ocCompo ) } ) > 0
         ocCompo := oObj:Form:Property[ ocCompo ]
       ELSE
         IF oObj:__ClassInst != NIL
            IF oObj:Application:Project:Forms[1]:Property != NIL .AND. ASCAN( oObj:Application:Project:Forms[1]:Property:Keys, {|c| UPPER(c) == UPPER( ocCompo ) } ) > 0
               ocCompo := oObj:Application:Project:Forms[1]:Property[ ocCompo ]
            ENDIF
          ELSE
            IF oObj:Application:MainForm:Property != NIL .AND. ASCAN( oObj:Application:MainForm:Property:Keys, {|c| UPPER(c) == UPPER( ocCompo ) } ) > 0
              ocCompo := oObj:Application:MainForm:Property[ ocCompo ]
            ENDIF
         ENDIF
      ENDIF
   ENDIF
RETURN ocCompo
*/

FUNCTION __ChkComponent( oObj, ocCompo )
   LOCAL oForm, n

   IF VALTYPE( ocCompo ) == "C" 

      IF oObj:__ClassInst == NIL 
      
         oForm := oObj:Form
         IF oForm:Property != NIL .AND. ASCAN( oForm:Property:Keys, {|c| UPPER(c) == UPPER( ocCompo ) } ) == 0
            oForm := oObj:Application:MainForm
         ENDIF
         IF oForm:Property != NIL .AND. ASCAN( oForm:Property:Keys, {|c| UPPER(c) == UPPER( ocCompo ) } ) > 0
            ocCompo := oForm:Property[ ocCompo ]
         ENDIF
        
      ELSE

         oForm := oObj:Form
         IF oForm:Property != NIL .AND. ASCAN( oForm:Property:Keys, {|c| UPPER(c) == UPPER( ocCompo ) } ) == 0
            oForm := oObj:Application:Project:Forms[1]
         ENDIF
         IF oForm:Property != NIL .AND. ASCAN( oForm:Property:Keys, {|c| UPPER(c) == UPPER( ocCompo ) } ) > 0
            ocCompo := oForm:Property[ ocCompo ]
         ENDIF
         
      ENDIF
   ENDIF
RETURN ocCompo
