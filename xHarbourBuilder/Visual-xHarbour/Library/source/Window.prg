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

#include "vxh.ch"
#include "colors.ch"
#include "debug.ch"
#include "error.ch"
#include "uxTheme.ch"
#include "dbinfo.ch"

#define HH_HELP_CONTEXT         15
#define SC_HELP        61824

#define ETDT_DISABLE        0x00000001
#define ETDT_ENABLE         0x00000002
#define ETDT_USETABTEXTURE  0x00000004
#define ETDT_ENABLETAB      (ETDT_ENABLE | ETDT_USETABTEXTURE)

#define CTYPE_BOOL                 9
#define PP_MOVEOVERLAY 8
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

static aMessages := {;
                    { WM_SIZE,            "OnSize"            },;
                    { WM_MOVE,            "OnMove"            },;
                    { WM_KEYDOWN,         "OnKeyDown"         },;
                    { WM_KEYUP,           "OnKeyUp"           },;
                    { WM_DROPFILES,       "OnDropFiles"       },;
                    { WM_MOUSEMOVE,       "OnMouseMove"       },;
                    { WM_NCMOUSEMOVE,     "OnNCMouseMove"     },;
                    { WM_NCMOUSELEAVE,    "OnNCMouseLeave"    },;
                    { WM_NCMOUSEHOVER,    "OnNCMouseHover"    },;
                    { WM_MOUSEWHEEL,      "OnMouseWheel"      },;
                    { WM_SETCURSOR,       "OnSetCursor"       },;
                    { WM_SETFOCUS,        "OnSetFocus"        },;
                    { WM_COMMAND,         "OnCommand"         },;
                    { WM_PAINT,           "OnPaint"           },;
                    { WM_NCPAINT,         "OnNCPaint"         },;
                    { WM_INITDIALOG,      "OnInitDialog"      },;
                    { WM_DESTROY,         "OnDestroy"         },;
                    { WM_ENTERSIZEMOVE,   "OnEnterSizeMove"   },;
                    { WM_EXITSIZEMOVE,    "OnExitSizeMove"    },;
                    { WM_NCACTIVATE,      "OnNCActivate"      },;
                    { WM_NCCREATE,        "OnNCCreate"        },;
                    { WM_NCCALCSIZE,      "OnNCCalcSize"      },;
                    { WM_NCDESTROY,       "OnNCDestroy"       },;
                    { WM_MOUSELEAVE,      "OnMouseLeave"      },;
                    { WM_MOUSEACTIVATE,   "OnMouseActivate"   },;
                    { WM_NCLBUTTONUP,     "OnNCLButtonUp"     },;
                    { WM_NCLBUTTONDOWN,   "OnNCLButtonDown"   },;
                    { WM_ERASEBKGND,      "OnEraseBkgnd"      },;
                    { WM_NCRBUTTONUP,     "OnNCRButtonUp"     },;
                    { WM_NCRBUTTONDOWN,   "OnNCRButtonDown"   },;
                    { WM_NCRBUTTONDBLCLK, "OnNCRButtonDblClk" },;
                    { WM_NCLBUTTONDBLCLK, "OnNCLButtonDblClk" },;
                    { WM_NCMBUTTONUP,     "OnNCMButtonUp"     },;
                    { WM_NCMBUTTONDOWN,   "OnNCMButtonDown"   },;
                    { WM_NCMBUTTONDBLCLK, "OnNCMButtonDblClk" },;
                    { WM_NCXBUTTONUP,     "OnNCXButtonUp"     },;
                    { WM_NCXBUTTONDOWN,   "OnNCXButtonDown"   },;
                    { WM_NCXBUTTONDBLCLK, "OnNCXButtonDblClk" },;
                    { WM_TIMER,           "OnTimer"           },;
                    { WM_UNDO,            "OnUndo"            },;
                    { EM_UNDO,            "OnUndo"            },;
                    { WM_CLOSE,           "__OnClose"         } ;
                    }

//-----------------------------------------------------------------------------------------------

CLASS Window INHERIT Object
   // Object Manager properties ----------------------------------------------------------------------------------------------------------------------------
   PROPERTY BackColor     ROOT "Colors"   GET IIF( ::xBackColor == NIL, ::__SysBackColor, ::xBackColor ) SET ::SetBackColor(v)
   PROPERTY ForeColor     ROOT "Colors"   GET IIF( ::xForeColor == NIL, ::__SysForeColor, ::xForeColor ) SET ::SetForeColor(v)
   PROPERTY ContextMenu   ROOT "Behavior" GET __ChkComponent( Self, @::xContextMenu ) HELP "ContectMenu that will show on right click"

   PROPERTY Left          ROOT "Position" SET ::__SetSizePos( 1, v )
   PROPERTY Top           ROOT "Position" SET ::__SetSizePos( 2, v )
   PROPERTY Width         ROOT "Size"     SET ::__SetSizePos( 3, v )
   PROPERTY Height        ROOT "Size"     SET ::__SetSizePos( 4, v )

   PROPERTY Cursor                        SET ::__SetWindowCursor(v)                 DEFAULT IDC_ARROW PROTECTED
   PROPERTY ControlParent                 SET ::SetExStyle( WS_EX_CONTROLPARENT, v ) DEFAULT .F.       PROTECTED

   PROPERTY Visible                       SET ::SetStyle( WS_VISIBLE, v )          DEFAULT .T.
   PROPERTY Enabled                       SET ::SetStyle( WS_DISABLED, v )         DEFAULT .T.

   PROPERTY ClipChildren                  SET ::SetStyle( WS_CLIPCHILDREN, v )     DEFAULT .T.
   PROPERTY ClipSiblings                  SET ::SetStyle( WS_CLIPSIBLINGS, v )     DEFAULT .T.
   PROPERTY AcceptFiles                   SET ::SetExStyle( WS_EX_ACCEPTFILES, v ) DEFAULT .F.
   PROPERTY NoActivate                    SET ::SetExStyle( WS_EX_NOACTIVATE, v )  DEFAULT .F.

   PROPERTY Theming                       SET ::__SetTheming(v)                    DEFAULT .T.
   PROPERTY Text                          SET ::SetWindowText(v)                   DEFAULT ""
   PROPERTY Font
   PROPERTY ToolTip
   PROPERTY HelpID        ROOT "Help" DEFAULT 0
   PROPERTY AllowDrop                                                              DEFAULT .F. HELP "Enables accepting dropped files from the file system"

   METHOD MessageWait()
   METHOD Animate()

//   METHOD MessageBox( cText, cCaption, nFlags ) INLINE MessageBox( ::hWnd, IIF( cText == NIL, "", XSTR( cText )), IIF( cCaption == NIL, "", XSTR( cCaption ) ), nFlags )

   METHOD MessageBox( cText, cCaption, nFlags, cIcon, nHelpID ) INLINE MessageBoxIndirect( ::hWnd, IIF( cText == NIL, "", XSTR( cText )), IIF( cCaption == NIL, "", XSTR( cCaption ) ), nFlags, cIcon, nHelpID )

   METHOD Destroy()
   METHOD Disable()               INLINE ::Enabled := .F.
   METHOD Enable()                INLINE ::Enabled := .T.
   METHOD Close()
   METHOD Hide()                  INLINE ShowWindow( ::hWnd, SW_HIDE )

   //-------------------------------------------------------------------------------------------------------------------------------------------------------

   DATA __SysBackColor EXPORTED INIT GetSysColor( COLOR_BTNFACE )
   DATA __SysForeColor EXPORTED INIT GetSysColor( COLOR_BTNTEXT )


   ACCESS xCaption       INLINE ::xText
   ASSIGN xCaption(c)    INLINE ::xText := c

   ACCESS Caption        INLINE ::Text
   ASSIGN Caption(c)     INLINE ::Text := c

   DATA xAnimation             EXPORTED
   ASSIGN Animation(o)         INLINE    ::xAnimation := o
   ACCESS Animation            INLINE    IIF( ::xAnimation == NIL, ::xAnimation := __Animation( Self ),), ::xAnimation

   DATA ClientWidth            EXPORTED  INIT 0
   DATA ClientHeight           EXPORTED  INIT 0

   DATA AutoClose              EXPORTED INIT .T.

   DATA VertScroll             EXPORTED INIT .F.
   DATA HorzScroll             EXPORTED INIT .F.
   DATA ScrollOnChildFocus     EXPORTED INIT .F.

   DATA OriginalRect           EXPORTED
   DATA Msg                    EXPORTED
   DATA wParam                 EXPORTED
   DATA lParam                 EXPORTED

   DATA Id                     EXPORTED
   DATA Drawing                EXPORTED

   DATA StatusBar              EXPORTED

   DATA WindowPos              EXPORTED
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
   DATA LastEvent              EXPORTED

   DATA ClassBrush             EXPORTED  INIT COLOR_BTNFACE+1
   DATA Style                  EXPORTED
   DATA ExStyle                EXPORTED  INIT 0
   DATA ClassStyle             EXPORTED  INIT (CS_OWNDC | CS_DBLCLKS | CS_SAVEBITS)

   DATA TopMargin              EXPORTED  INIT 0
   DATA RightMargin            EXPORTED  INIT 0
   DATA BottomMargin           EXPORTED  INIT 0
   DATA LeftMargin             EXPORTED  INIT 0

   DATA Active                 EXPORTED  INIT .F.
   DATA DeferRedraw            EXPORTED  INIT .T.
   DATA IsContainer            EXPORTED  INIT .T.
   DATA TabValidate            EXPORTED  INIT .T.

   // Private Properties
   DATA MDIClient                PROTECTED
   DATA __Docked                 PROTECTED INIT .T.
   DATA __aCltRect               PROTECTED
   DATA __cPaint                 PROTECTED
   DATA __lShown                 PROTECTED INIT .F.

   DATA __nCaptionHeight         EXPORTED  INIT 0
   DATA __lRegTrans              EXPORTED  INIT .F.
   DATA __aValues                EXPORTED  INIT {}
   DATA __hCursor                EXPORTED
   DATA __MenuBar                EXPORTED  INIT .F.
   DATA __Splitting              EXPORTED  INIT .F.
   DATA __lPopTip                EXPORTED  INIT .F.
   DATA __KeyPressed             EXPORTED  INIT 0
   DATA __CurrentPos             EXPORTED  INIT 1
   DATA __Timers                 EXPORTED  INIT 1
   DATA __lAllowCopy             EXPORTED  INIT .T.
   DATA __lInitialized           EXPORTED  INIT .F.
   DATA __nProc                  EXPORTED
   DATA __hAccelTable            EXPORTED
   DATA __lMouseHover            EXPORTED  INIT .F.
   DATA __IsInstance             EXPORTED  INIT .F.
   DATA __pCallBackPtr           EXPORTED
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
   DATA __oDlg                   EXPORTED
   DATA __hGridBmp               EXPORTED

   ACCESS EnumCursor             INLINE ::System:GetEnumCursor()


   DATA xAlignment             EXPORTED INIT 1
   ACCESS Alignment            INLINE   ::xAlignment
   ASSIGN Alignment(n)         INLINE   ::SetAlignment(n)

   // Compatibility --------------------------
   DATA VertScrollSize         EXPORTED INIT 0
   DATA HorzScrollSize         EXPORTED INIT 0
   // ----------------------------------------

   DATA Hidden                 EXPORTED INIT .F.
   DATA DragDrop               EXPORTED INIT {=>}
   DATA VertScrollPos          EXPORTED INIT 0
   DATA HorzScrollPos          EXPORTED INIT 0
   DATA Anchor                 EXPORTED
   DATA Modal        AS LOGIC  EXPORTED INIT .F.
   DATA AutoDock               EXPORTED INIT .T.
   DATA Dock                   EXPORTED AS OBJECT
   DATA HelpInfo               EXPORTED


   ACCESS Handle               INLINE ::hWnd
   ACCESS HasFocus             INLINE GetFocus() == ::hWnd
   ACCESS IsChild              INLINE (::Style & WS_CHILD) != 0

   ACCESS Child                INLINE (::Style & WS_CHILD) != 0
   ASSIGN Child(l)             INLINE ::SetStyle( WS_CHILD, l )

   DATA xMDIChild              EXPORTED INIT .F.
   DATA xMdiContainer          EXPORTED INIT .F.

   DATA aPrevSize              EXPORTED

   DATA OnWMSysCommand         EXPORTED
   DATA OnWMSize               EXPORTED
   DATA OnWMDestroy            EXPORTED
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
   DATA OnWMLButtonDblClk      EXPORTED

   DATA Siv                    PROTECTED
   DATA Sih                    PROTECTED
   DATA BitmapMask             PROTECTED
   DATA BitmapMaskColor        PROTECTED
   DATA __TaskBarParent        PROTECTED
   DATA __WndProc              PROTECTED INIT "__ControlProc"
   DATA __HideResized          PROTECTED INIT .F.
   DATA __WindowStyle          PROTECTED INIT WT_WINDOW
   DATA __ClientRect           PROTECTED
   DATA __WidthPerc            PROTECTED
   DATA __HeightPerc           PROTECTED
   DATA __hRegion              PROTECTED
   DATA __aMinRect             PROTECTED
   DATA __hBmpRgn              PROTECTED
   DATA __lSizeChanged         PROTECTED INIT .F.
   DATA __lNCMouseHover        PROTECTED INIT .F.
   DATA __IsForm               PROTECTED INIT .F.
   DATA __lSubClass            EXPORTED  INIT .T.
   DATA __aHotKey              PROTECTED INIT {}
   DATA __aDock                EXPORTED  INIT {}
   DATA __lCallbackReleased    PROTECTED INIT .F.

   ACCESS AppInstance          INLINE IIF( ::Form:DllInstance != NIL, ::Form:DllInstance, ::Application:Instance )

   METHOD Init( oParent ) CONSTRUCTOR
   DESTRUCTOR __WinDestruct

   METHOD Create()

   METHOD UpdateScrollArea()

   METHOD __Register()
   METHOD __ControlProc()
   METHOD __CreateMDI()
   METHOD __OnParentSize()
   METHOD __ResetHandle()       INLINE ::hWnd := NIL, ::__nProc := NIL
   METHOD __WinProc(hWnd, nMsg, nwParam, nlParam) INLINE DefWindowProc(hWnd, nMsg, nwParam, nlParam )
   METHOD __SetScrollBars()
   METHOD __SubClass()
   METHOD __UnSubClass()
   METHOD __GetShowMode()
   METHOD __SetFrameStyle()
   METHOD __SetWindowCursor()
   METHOD __SetVertScrollSize(n) INLINE IIF( ::OriginalRect != NIL, (::OriginalRect[4] := n, IIF( ::IsWindow(), ::__SetScrollBars(),)),)
   METHOD __SetHorzScrollSize(n) INLINE IIF( ::OriginalRect != NIL, (::OriginalRect[3] := n, IIF( ::IsWindow(), ::__SetScrollBars(),)),)
   METHOD __SetSizePos()
   METHOD __GetBrush()         VIRTUAL
   METHOD __PaintBakgndImage() VIRTUAL
   METHOD __GC()               VIRTUAL
   METHOD __SetTheming()

   METHOD EnableThemeDialogTexture( nFlags ) INLINE EnableThemeDialogTexture( ::hWnd, nFlags )
   METHOD __SetInvStyle( n, l )   INLINE ::SetStyle( n, !l )
   METHOD DragAcceptFiles(l)      INLINE IIF( ::hWnd != NIL, DragAcceptFiles( ::hWnd, l ), NIL )

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
   METHOD SetWindowText()
   METHOD BringWindowToTop()      INLINE BringWindowToTop( ::hWnd )
   METHOD ScreenToClient( pt )    INLINE ScreenToClient( ::hWnd, @pt )
   METHOD ClientToScreen( pt )    INLINE ClientToScreen( ::hWnd, @pt )
   METHOD SetWindowPos( hAfter, x, y, w, h , n)             INLINE IIF( ::ClsName == "VXH_FORM_IDE" .AND. ::DesignMode, (x:=10-::Parent:HorzScrollPos,y:=10-::Parent:VertScrollPos),), SetWindowPos( ::hWnd, hAfter, x, y, w, h, n )
   METHOD DeferWindowPos( hDef, hAfter, x, y, w, h , n)     INLINE IIF( ::ClsName == "VXH_FORM_IDE" .AND. ::DesignMode, (x:=10-::Parent:HorzScrollPos,y:=10-::Parent:VertScrollPos),), DeferWindowPos( hDef, ::hWnd, hAfter, x, y, w, h, n )
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
   METHOD GetStyle(n)             INLINE IIF( ::IsWindow(), ( ::GetWindowLong( GWL_STYLE ) & n ) == n, (::Style & n) == n )
   METHOD SetExStyle()

   METHOD SetRedraw(lRed)         INLINE SendMessage( ::hWnd, WM_SETREDRAW, lRed )
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

   METHOD Show( nShow )           INLINE ShowWindow( ::hWnd, IIF( ! ::DesignMode .AND. nShow != NIL, nShow, SW_SHOW ) )
   METHOD Refresh()               INLINE ::InvalidateRect(,.T.)
   METHOD ReCreate()
   METHOD GetHeight()             INLINE ::xHeight
   METHOD DockIt()
   METHOD UpdateLayout()          INLINE ::PostMessage( WM_SIZE, 0, MAKELONG( ::ClientWidth, ::ClientHeight ) )

   METHOD DockToParent()          INLINE ::Dock:Left   := ::Parent,;
                                         ::Dock:Top    := ::Parent,;
                                         ::Dock:Right  := ::Parent,;
                                         ::Dock:Bottom := ::Parent,;
                                         ::DockIt()

   METHOD HandleEvent( cEvent )   INLINE ExecuteEvent( cEvent, Self )
   METHOD DockControls()          INLINE ::OnSize( 0, MAKELPARAM( ::ClientWidth, ::ClientHeight ) )

   METHOD SaveValues()
   METHOD RestoreValues()

   METHOD RegisterDocking()

   METHOD OnChildChar()         VIRTUAL
   METHOD OnChildGetDlgCode()   VIRTUAL
   METHOD OnChar()              VIRTUAL
   METHOD OnSysChar()           VIRTUAL
   METHOD OnGetMinMaxInfo()     VIRTUAL
   METHOD OnHotKey()            VIRTUAL
   METHOD OnParentDrawItem()    VIRTUAL
   METHOD OnParentMeasureItem() VIRTUAL
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
   METHOD OnMove()              VIRTUAL
   METHOD OnMoving()            VIRTUAL
   METHOD OnRButtonDown()       VIRTUAL
   METHOD OnRButtonUp()         VIRTUAL
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
   METHOD PreInitDialog()       VIRTUAL
   METHOD PostInitDialog()      VIRTUAL

   METHOD OnActivate()          VIRTUAL
   METHOD OnUserMsg()           VIRTUAL
   METHOD OnMessage()           VIRTUAL
   METHOD OnSystemMessage()     VIRTUAL
   METHOD OnSetFont()           VIRTUAL
   METHOD OnNCCreate()          VIRTUAL
   METHOD OnNCActivate()

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
   METHOD OnEnable()            VIRTUAL
   METHOD OnContextMenu()       VIRTUAL
   METHOD OnThemeChanged()      VIRTUAL
   METHOD OnSetText()           VIRTUAL
   METHOD OnNCCalcSize()        VIRTUAL
   METHOD OnCancelMode()        VIRTUAL
   METHOD OnMouseHover()        VIRTUAL
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
   METHOD InitDialogBox()       VIRTUAL
   METHOD OnDestroy()           VIRTUAL
   METHOD OnPaint()             VIRTUAL

   METHOD OnMouseWheel()

   METHOD OnHScroll()
   METHOD OnVScroll()

   METHOD OnMeasureItem()
   METHOD OnDrawItem()
   METHOD OnSetFocus()

   METHOD GetChildFromPoint()
   METHOD SetTimer(nId,nElapse,hProc)               INLINE SetTimer( ::hWnd, nId, nElapse,hProc )
   METHOD KillTimer(nId)                            INLINE KillTimer( ::hWnd, nId )
   METHOD OpenThemeData()                           INLINE IIF( ::hTheme == NIL, ::hTheme := OpenThemeData( ::hWnd, ::ThemeName ), )
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

   METHOD OnDropFiles()
   METHOD OnMouseMove()
   METHOD OnMouseLeave()
   METHOD OnNcMouseMove()
   METHOD OnNCMouseHover()
   METHOD OnNCMouseLeave()
   METHOD OnSize()
   METHOD OnSetCursor()
   METHOD OnCommand()
   METHOD BeginPaint()
   METHOD EndPaint()
   METHOD GetControl()
   METHOD IsChildOf()

   METHOD OnNCDestroy()
   METHOD OnEraseBkGnd()
   METHOD __CreateBkBrush() VIRTUAL
   METHOD OnClose()         VIRTUAL
   METHOD __OnClose()       VIRTUAL
ENDCLASS

PROCEDURE __WinDestruct CLASS Window
   IF ::__pCallBackPtr != NIL .AND. ! IsWindow( ::hWnd )
      VXH_FreeCallbackPointer( ::__pCallBackPtr )
      IF __ObjHasMsg( Self, "__pCallBackEdit" ) .AND. ::__pCallBackEdit != NIL
         VXH_FreeCallbackPointer( ::__pCallBackEdit )
      ENDIF
   ENDIF
RETURN

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent, lInitValues ) CLASS Window
   LOCAL Topic
   DEFAULT ::__lInitialized  TO .F.
   DEFAULT lInitValues TO .T.

   IF ::__lInitialized
      MessageBox( GetActiveWindow(), ::Name+" has already been initialized returning previous instance." + CHR(13)+;
                                                "Use the prevously created instance to avoid this Message", ::Name, MB_ICONEXCLAMATION )
      RETURN Self
   ENDIF

   ::__lInitialized := .T.

   DEFAULT ::ThemeName    TO "window"
   DEFAULT ::Style        TO (WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   DEFAULT ::xLeft        TO 0
   DEFAULT ::xTop         TO 0
   DEFAULT ::xWidth       TO CW_USEDEFAULT
   DEFAULT ::xHeight      TO CW_USEDEFAULT
   DEFAULT ::__xCtrlName  TO "Window"
   DEFAULT ::ClsName      TO "Window"

   ::Parent := oParent

   IF lInitValues
      __SetInitialValues( Self )
   ENDIF

   ::Font := Font( Self )

   IF ! ( ::ClsName == TOOLTIPS_CLASS )
      ::ToolTip := ToolTip( Self )
      ::Drawing := Drawing( Self )
      IF VALTYPE( oParent ) == "O"
         ::__CreateProperty()
         DEFAULT ::Dock   TO __WindowDock( Self )
         DEFAULT ::Anchor TO __AnchorSet( Self )
         IF ::Parent:Children != NIL
            ::xTabOrder := LEN( ::Parent:Children )+1
         ENDIF
         IF ::DesignMode
            __SetInitialValues( Self, "TabOrder" )
         ENDIF
      ENDIF
   ENDIF
   DEFAULT ::EventHandler TO Hash()

   IF oParent != NIL .AND. oParent:DesignMode .OR. (::ClsName IN { "VXH_FORM_IDE", "CCTL" })
      DEFAULT ::Events TO aEvents()
      aSort( ::Events,,,{|x, y| x[1] < y[1]})
      FOR EACH Topic IN ::Events
          aSort( Topic[2],,,{|x, y| x[1] < y[1]})
      NEXT
   ENDIF
   oParent := NIL

RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD GetControl( cName ) CLASS Window
   LOCAL n := ASCAN( ::Children, {|o| o:Name == cName } )
RETURN IIF( n > 0, ::Children[n], NIL )

//-----------------------------------------------------------------------------------------------
METHOD __SetTheming( lSet ) CLASS Window
   IF ::hWnd != NIL
      IF !lSet
         ::RemoveWindowTheme()
       ELSEIF ::ThemeName != NIL
         ::SetWindowTheme()
      ENDIF
      AEVAL( ::Children, {|o|o:Theming := lSet } )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SaveValues() CLASS Window
   LOCAL n, xValue
   ::__aValues := {}
   FOR n := 1 TO LEN( ::Children )
       xValue := NIL
       DO CASE
          CASE (::Children[n]:__xCtrlName IN { "EditBox", "MaskEdit", "CheckBox", "RadioButton" })
               xValue := ::Children[n]:Text

          CASE ::Children[n]:__xCtrlName == "ComboBox"
               xValue := ::Children[n]:GetSelString()

          CASE ::Children[n]:__xCtrlName == "ListBox"
               xValue := ::Children[n]:GetText()
       ENDCASE
       IF xValue != NIL
          AADD( ::__aValues, { ::Children[n]:Name, xValue, ::Children[n]:__xCtrlName } )
       ENDIF
   NEXT
RETURN ::__aValues

//-----------------------------------------------------------------------------------------------
METHOD RestoreValues( aValues ) CLASS Window
   LOCAL oCtrl, n
   DEFAULT aValues TO ::__aValues
   FOR n := 1 TO LEN( aValues )
       oCtrl := ::GetControl( aValues[n][1] )
       IF oCtrl != NIL
          DO CASE
             CASE (aValues[n][3] IN { "EditBox", "MaskEdit", "CheckBox", "RadioButton" })
                  oCtrl:Text := aValues[n][2]

             CASE (aValues[n][3] IN { "ComboBox", "ListBox" })
                  oCtrl:SetCurSel( oCtrl:FindStringExact( -1, aValues[n][2] ) )
          ENDCASE
       ENDIF
   NEXT
RETURN Self

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
METHOD IsChildOf( hWnd ) CLASS Window
   LOCAL oParent := ::Parent

   WHILE oParent != NIL
      IF oParent:hWnd == hWnd
         RETURN .T.
      ENDIF
      IF oParent:Parent == NIL
         EXIT
      ENDIF
      oParent := oParent:Parent
   ENDDO
RETURN .F.

//-----------------------------------------------------------------------------------------------
METHOD SetWindowText( cText ) CLASS Window
   LOCAL xText := cText
   IF ! ::DesignMode .AND. ! ::Application:__Vxh
      TRY
         IF VALTYPE(cText)=="C" .AND. LEFT(cText,2)=="{|"
            cText := &cText
         ENDIF
         IF VALTYPE(cText)=="B"
            cText := EVAL(cText)
         ENDIF
      CATCH
         cText := xText
      END
   ENDIF
   ::xText := cText
   IF ::hWnd != NIL
      SetWindowText( ::hWnd, cText )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD RestoreLayout( cIniFile, cSection, lAllowOut ) CLASS Window
   LOCAL c, oIni, aPos
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
METHOD GetCCTL() CLASS Window
   LOCAL oParent := ::Parent
   WHILE oParent:ClsName != "CCTL"
      oParent := oParent:Parent
   ENDDO
RETURN oParent

METHOD __Register( cClass ) CLASS Window
   LOCAL wcex

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
      wcex:hCursor        := LoadCursor(, IDC_ARROW )
      wcex:cbWndExtra     := 16

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
   IF ::ClsName == "VXH_FORM_IDE" .AND. nPos <= 2
      RETURN NIL
   ENDIF
   IF nPos > 2 .AND. nVal < 0
      nVal := 0
   ENDIF
   IF ! ::DesignMode .AND. ::hWnd != NIL .AND. ::Parent == NIL
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

   IF ( ! ::DesignMode .OR. ::__CustomOwner .OR. ::__xCtrlName == "Expando" ) .AND. ::hWnd != NIL
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
   LOCAL hParent, nLeft, nTop
   LOCAL oObj, nError, cError, aSize
   LOCAL cBmpMask, cText

   IF ! __ObjHasMsg( Self, "MDIContainer" ) .OR. ! ::MDIContainer
      ::MDIClient := NIL
   ENDIF

   IF ::DesignMode .AND. ::__CustomOwner .AND. ::hWnd != NIL
      RETURN Self
   ENDIF

   IF ::DesignMode .AND. ( ::__CustomOwner .OR. UPPER( ::ClassName ) == "CUSTOMCONTROL" )

      //__ResetClassInst( Self ) // xxx

      IF ::__SysBackColor != ::xBackColor .AND. ::xBackColor != NIL
         ::__SysBackColor := ::xBackColor
         ::__ForceSysColor := .T.
      ENDIF
      IF ::__SysForeColor != ::xForeColor .AND. ::xForeColor != NIL
         ::__SysForeColor := ::xForeColor
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

   ::RegisterDocking()

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

   IF ! ::__IsStandard .AND. ::ClsName != "__VideoCapture" .AND. ! IsRegistered( ::Instance, ::ClsName )
      IF ! ::__Register()
         RETURN NIL
      ENDIF
   ENDIF

   IF ! ( ::ClsName == TOOLTIPS_CLASS )
      DEFAULT ::ToolTip TO ToolTip( Self )
   ENDIF

   ::xLeft   := IIF( ::ClsName == "MDIChild" .AND. ::xLeft == 0, CW_USEDEFAULT, ::xLeft  )
   ::xTop    := IIF( ::ClsName == "MDIChild" .AND. ::xTop == 0, CW_USEDEFAULT, ::xTop   )

   ::xWidth  := IFNIL( ::xWidth , CW_USEDEFAULT, ::xWidth )
   ::xHeight := IFNIL( ::xHeight, CW_USEDEFAULT, ::xheight)

   IF ::DesignMode
      ::Style := (::Style | WS_VISIBLE)
   ENDIF

   IF ! ::DesignMode .AND. !EMPTY( ::BitmapMask )
      ::Style := WS_POPUP
      ::ExStyle := 0
      DEFAULT ::BitmapMaskColor TO RGB( 0, 0, 0)

      cBmpMask := ::BitmapMask
      IF VALTYPE( cBmpMask ) == "A"
         cBmpMask := cBmpMask[2]
         ::__hBmpRgn := LoadImage( ::Instance, cBmpMask, IMAGE_BITMAP, 0, 0, LR_VGACOLOR )
       ELSE
         IF RIGHT( UPPER( ::BitmapMask ), 4 ) == ".BMP"
            ::__hBmpRgn := LoadImage( ::Instance, ::BitmapMask, IMAGE_BITMAP, 0, 0, (LR_LOADFROMFILE | LR_VGACOLOR) )
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

      IF ! ::DesignMode .AND. ! ::Application:__Vxh
         cText := ::xText
         IF VALTYPE(cText)=="C" .AND. LEFT(cText,2)=="{|"
            TRY
               cText := &cText
            CATCH
            END
         ENDIF
         IF VALTYPE(cText)=="B"
            ::xText := EVAL(cText)
         ENDIF
      ENDIF

      nLeft := ::xLeft
      nTop  := ::xTop
      IF ::ClsName == "VXH_FORM_IDE"
         nLeft := 10-::Parent:HorzScrollPos
         nTop  := 10-::Parent:VertScrollPos
      ENDIF
      //IF ::ClsName == "#32768"
      //   hParent := NIL
      //ENDIF
      ::hWnd := CreateWindowEx( ::ExStyle, ::ClsName, ::Caption, ::Style, nLeft, nTop, ::Width, ::Height, hParent, ::Id, ::AppInstance, ::__ClientStruct )
    ELSE
      ::hWnd := capCreateCaptureWindow( "CaptureWindow", (WS_CHILD | WS_VISIBLE), ::Left, ::Top, ::Width, ::Height, hParent, 0 )
   ENDIF
   IF ::hWnd == 0
      nError := GetLastError()
      cError := FormatMessage( ,,nError )
      cError := STRTRAN( cError, CHR(13)+CHR(10) )
      Throw( ErrorNew( "Window.prg", EG_CREATE, hParent, ProcName(), cError, ;
                       { ::ExStyle, ::ClsName, ::Caption, ::Style, ::Left, ::Top, ::Width, ::Height, hParent, ::Id, ::AppInstance, ::__ClientStruct } ) )
   ENDIF

   __SetWindowObjPtr( Self )

   IF ::xCursor != NIL .AND. ::xCursor != IDC_ARROW
      ::__SetWindowCursor( ::xCursor )
   ENDIF

   IF ::Application != NIL .AND. !EMPTY( ::__Accelerators )
      ::__hAccelTable := CreateAcceleratorTable( ::__Accelerators )
      ::Application:AddAccelerators( ::hWnd, ::__hAccelTable )
   ENDIF

   IF ! ::Theming
      SetWindowTheme( ::hWnd, "", "" )
   ENDIF

   IF ::AllowDrop
      ::DragAcceptFiles(.T.)
   ENDIF

   ::__SubClass()

   nLeft := ::Left
   nTop  := ::Top
   ::GetClientRect()
   ::GetWindowRect()
   ::xLeft := nLeft
   ::xTop  := nTop

   ::__aCltRect   := { ::Left, ::Top, ::Width, ::Height }
   ::__ClientRect := { ::Left, ::Top, ::Width, ::Height }
   ::OriginalRect := { ::Left, ::Top, ::ClientWidth, ::ClientHeight }

   ::Font:Create()

   IF ::Parent != NIL .AND. ::ClsName != TOOLTIPS_CLASS .AND. ::__xCtrlName != "CtrlMask" .AND. ::ClsName != "MDIClient"
      IF ::SetChildren .AND. ( !(::Parent:ClsName == WC_TABCONTROL) .OR. ::__xCtrlName == "TabPage" .OR. ::DesignMode )
         IF ::Parent:ClsName != "DataGrid" .OR. ::ClsName == "GridColumn"
            AADD( ::Parent:Children, Self )
         ENDIF
      ENDIF
   ENDIF

   IF ! ::DesignMode .AND. ::__hRegion != NIL
      SetWindowRgn( ::hWnd, ::__hRegion, .T. )
      DeleteObject( ::__hBmpRgn )
   ENDIF

   IF ::ToolTip != NIL .AND. !EMPTY( ::ToolTip:Text )
      ::ToolTip:Create()
   ENDIF

   IF ::MDIClient != NIL
      ::MDIClient:Create()
      ::SendMessage( WM_SIZE, 0, MAKELONG( ::ClientWidth, ::ClientHeight ) )

      IF ! ::DesignMode .AND. ::Application != NIL
         ::Application:MDIClient := ::MDIClient:hWnd
      ENDIF

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
      IF ::Parent != NIL .AND. ! ::ClsName == "MDIChild"
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

METHOD AddAccelerator( nVirtKey, nKey, nId, bAction ) CLASS Window
   LOCAL n
   IF ( n := ASCAN( ::__Accelerators, {|a| a[1]==nVirtKey .AND. a[2]==nKey} ) ) > 0
      ::__Accelerators[n][3] := nId
      ::__Accelerators[n][4] := bAction
    ELSE
      AADD( ::__Accelerators, { nVirtKey, nKey, nId, bAction } )
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

METHOD RegisterDocking() CLASS Window
   IF ::Dock != NIL
      IF ::Parent != NIL
         DEFAULT ::Parent:__aDock TO {}
         IF ( ::Dock:Left != NIL .OR. ::Dock:Top != NIL .OR. ::Dock:Right != NIL .OR. ::Dock:Bottom != NIL .OR. ::ClsName == "ToolStripContainer" ) .AND. ASCAN( ::Parent:__aDock, {|o| o:hWnd == ::hWnd} ) == 0
            AADD( ::Parent:__aDock, Self )
         ENDIF
      ENDIF
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------
METHOD __SubClass() CLASS Window
   IF ::__lSubClass .AND. ::__nProc == NIL .AND. ::__pCallBackPtr == NIL
      ::__pCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, ::__WndProc ), Self )
      ::__nProc := SetWindowLong( ::hWnd, GWL_WNDPROC, ::__pCallBackPtr )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------
METHOD __UnSubClass() CLASS Window
   IF ::__nProc != NIL
      SetWindowLong( ::hWnd, GWL_WNDPROC, ::__nProc )
      ::__nProc := NIL
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __CreateMDI( lCreate ) CLASS Window
   DEFAULT lCreate TO .T.
   ::__IdeImageIndex := 3
   IF ::hWnd != NIL

      IF lCreate .AND. ( ::MDIClient == NIL .OR. !IsWindow( ::MDIClient:hWnd ) )
         ::MDIClient := MDIClient( Self )
         ::MDIClient:Create()

         IF ::Application != NIL .AND. ! ::DesignMode
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
         IF ::DesignMode
            AEVAL( ::Application:Project:Forms, {|o| o:MDIChild := .F.} )
         ENDIF
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetBackColor( nColor, lRepaint ) CLASS Window
   DEFAULT lRepaint TO .T.
   IF nColor == NIL .AND. ::__ForceSysColor
      nColor := ::__SysBackColor
   ENDIF
   ::xBackColor := nColor

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
      ::BkBrush := NIL
   ENDIF
   IF nColor != NIL
      ::BkBrush := CreateSolidBrush( nColor )
   ENDIF
   IF lRepaint .AND. ::IsWindowVisible()
      ::InValidateRect()
   ENDIF
RETURN SELF

METHOD SetForeColor( nColor, lRepaint ) CLASS Window
   DEFAULT lRepaint TO .T.
   ::xForeColor := nColor
   IF lRepaint .AND. ::IsWindowVisible()
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

METHOD SetParent( oParent ) CLASS Window
   LOCAL n
   IF ::Parent != NIL
      IF ( n := ASCAN( ::Parent:Children, {|o|o:hWnd==::hWnd} ) ) > 0
         ADEL( ::Parent:Children, n, .T. )
      ENDIF
   ENDIF
   n := ASCAN( ::Parent:__aDock, {|o| o == Self} )
   IF n > 0
      ADEL( ::Parent:__aDock, n, .T. )
      AADD( oParent:__aDock, Self )
   ENDIF
   ::Dock:Left   := NIL
   ::Dock:Top    := NIL
   ::Dock:Right  := NIL
   ::Dock:Bottom := NIL
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
      ::ExStyle := (::ExStyle | nStyle)
    ELSE
      ::ExStyle := (::ExStyle & NOT( nStyle ))
   ENDIF
   IF ::hWnd != NIL .AND. ::IsWindow()
      ::SetWindowLong( GWL_EXSTYLE, ::ExStyle )
      IF ::IsWindowVisible()
         ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER))
         ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
      ENDIF
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
   LOCAL cStyle := ""
   DEFAULT lAdd TO .T.
   IF nStyle == NIL
      RETURN NIL
   ENDIF
   IF ::DesignMode .AND. ::ClsName == "Button" .AND. lAdd .AND. nStyle == BS_DEFPUSHBUTTON
      RETURN NIL
   ENDIF
   IF ::IsWindow()
      ::Style := ::GetWindowLong( GWL_STYLE )
   ENDIF
   IF nStyle == WS_DISABLED
      lAdd := !lAdd
   ENDIF
   IF lAdd
      ::Style := (::Style | nStyle)
    ELSE
      ::Style := (::Style & NOT( nStyle ))
   ENDIF
   IF ::IsWindow()
      SWITCH nStyle
         CASE WS_VISIBLE
              IF ::DesignMode
                 RETURN self
              ENDIF
              IF lAdd
                 ::Show()
               ELSE
                 ::Hide()
              ENDIF
              EXIT
         CASE WS_DISABLED
              EnableWindow( ::hWnd, lAdd )
              EXIT

      END
      ::SetWindowLong( GWL_STYLE, ::Style )
      IF ::IsWindowVisible()
         ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER))
         ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
      ENDIF
   ENDIF
RETURN self

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
FUNCTION ExecuteEvent( cEvent, oObj, xParam1, xParam2, xParam3, xParam4, xParam5 )
   LOCAL cFormEvent, nRet

   IF oObj:EventHandler != NIL .AND. HGetPos( oObj:EventHandler, cEvent ) > 0
      IF ( cFormEvent := oObj:EventHandler[ cEvent ] ) != NIL

         IF Valtype( cFormEvent ) == "B"
            RETURN Eval( cFormEvent, oObj, xParam1, xParam2, xParam3, xParam4, xParam5 )
         ENDIF

         IF ! ( oObj == oObj:Form )
            IF __objHasMsg( oObj, cFormEvent )
               nRet := hb_ExecFromArray( oObj, cFormEvent, {oObj, @xParam1, @xParam2, @xParam3, @xParam4, @xParam5} )
             ELSEIF __objHasMsg( oObj:Form, cFormEvent )
               nRet := hb_ExecFromArray( oObj:Form, cFormEvent, {oObj, @xParam1, @xParam2, @xParam3, @xParam4, @xParam5} )
             ELSEIF __objHasMsg( oObj:Parent, cFormEvent )
               nRet := hb_ExecFromArray( oObj:Parent, cFormEvent, {oObj, @xParam1, @xParam2, @xParam3, @xParam4, @xParam5} )
             ELSEIF oObj:Parent != NIL .AND. oObj:Parent:__xCtrlName == "TabPage"
               nRet := hb_ExecFromArray( oObj:Parent:Parent:Form, cFormEvent, {oObj, @xParam1, @xParam2, @xParam3, @xParam4, @xParam5} )
            ENDIF
          ELSE
            nRet := hb_ExecFromArray( oObj, cFormEvent, {oObj, @xParam1, @xParam2, @xParam3, @xParam4, @xParam5} )
         ENDIF
      ENDIF
   ENDIF
RETURN nRet


//-----------------------------------------------------------------------------------------------
#define WM_THEMECHANGED   0x031A
#define WM_SOCKET_NOTIFY  0x0373
#define WM_SOCKET_DEAD    0x0374
#define WM_KICKIDLE       0x036A

#define WM_IDLE WM_USER + 1

//-----------------------------------------------------------------------------------------------
METHOD OnDropFiles( nwParam, nlParam ) CLASS Window
   LOCAL nFiles, n, cFile, pt := (struct POINT)
   (nlParam)

   nFiles := DragQueryFile( nwParam, 0xFFFFFFFF )

   IF nFiles > 0
      DragQueryPoint( nwParam, @pt )

      ::DragDrop := {=>}
      HSetCaseMatch( ::DragDrop, .F. )

      ::DragDrop[ "Files"    ] := {}
      ::DragDrop[ "Position" ] := pt

      FOR n := 1 TO nFiles
          DragQueryFile( nwParam, n-1, @cFile, MAX_PATH )
          AADD( ::DragDrop:Files, cFile )
      NEXT
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnMouseLeave() CLASS Window
   ::__lMouseHover := .F.
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnMouseMove( nwParam, nlParam ) CLASS Window
   LOCAL nRet
   IF ! ::__lMouseHover
      IF ::ToolTip != NIL .AND. ::__lPopTip
         ::__lPopTip := .F.
         ::ToolTip:SetTimer( 25, 1000 )
      ENDIF
      ::__lMouseHover := .T.
      nRet := ExecuteEvent( "OnMouseHover", Self )
      ODEFAULT nRet TO ::OnMouseHover( nwParam, nlParam )
      ::TrackMouseEvent( TME_LEAVE )
   ENDIF
RETURN nRet

//-----------------------------------------------------------------------------------------------
METHOD OnNCMouseMove() CLASS Window
   IF !::__lNCMouseHover
      ::__lNCMouseHover := .T.
      ::TrackMouseEvent( TME_NONCLIENT | TME_LEAVE | TME_HOVER )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnNCMouseHover() CLASS Window
   ::TrackMouseEvent( TME_NONCLIENT | TME_LEAVE )
   ::__lNCMouseHover := .T.
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnNCMouseLeave() CLASS Window
   ::TrackMouseEvent( TME_NONCLIENT | TME_HOVER )
   ::__lNCMouseHover := .F.
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnSetCursor() CLASS Window
   IF ::Application != NIL .AND. ::Application:Cursor != NIL
      WinSetCursor( ::Application:Cursor )
      RETURN .T.
   ENDIF
   IF ::__hCursor != NIL
      WinSetCursor( ::__hCursor )
      RETURN .T.
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnSize( nwParam, nlParam ) CLASS Window
   LOCAL x, y, aChildren, hDef, oChild

   ::__lSizeChanged := .T.

   IF EMPTY( ::__ClientRect )
      RETURN 0
   ENDIF
   ::ClientWidth  := LOWORD( nlParam )
   ::ClientHeight := HIWORD( nlParam )

   x := MAX( ::xWidth,  ::OriginalRect[3] )
   y := MAX( ::xHeight, ::OriginalRect[4] )

   IF ::ClsName != "DataGrid"
      aChildren := IIF( ::__DockChildren != NIL, ::__DockChildren, ::__aDock )
      IF ! Empty( aChildren )
         hDef := BeginDeferWindowPos( LEN( aChildren ) )
         FOR EACH oChild IN aChildren
             IF VALTYPE( oChild ) == "O"
                IF oChild:__IsControl .AND. oChild:Anchor != NIL .AND. oChild:Anchor:Center
                   oChild:Center()
                 ELSE
                   oChild:__OnParentSize( x, y, @hDef, ,, ::__aCltRect[3], ::__aCltRect[4] )
                ENDIF
                oChild:UpdateWindow()
             ENDIF
         NEXT
         EndDeferWindowPos( hDef )
      ENDIF
      ::__ClientRect[3] := ::ClientWidth
      ::__ClientRect[4] := ::ClientHeight
      ::__aCltRect[3] := x
      ::__aCltRect[4] := y
   ENDIF

   IF ( nwParam == SIZE_MAXIMIZED .OR. nwParam == SIZE_RESTORED ) .AND. ( ::HorzScroll .OR. ::VertScroll )
      ::__SetScrollBars()
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnNCActivate( nwParam, nlParam ) CLASS Window
   LOCAL oWnd
   IF nwParam == 0 .AND. IsWindow( nlParam )
      oWnd := ObjFromHandle( nlParam )
      IF oWnd != NIL .AND. oWnd:ClsName == "FloatShadow"
         RETURN .T.
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnMouseWheel( nwParam, nlParam ) CLASS Window
   LOCAL pt, rc, n, nLines, nPage, nDelta, nScroll
   IF ::sih != NIL .OR. ::siv != NIL
      pt := (struct POINT)
      pt:x := LOWORD( nlParam )
      pt:y := HIWORD( nlParam )

      IF ( !::__IsControl .OR. ::__xCtrlName == "DataGrid" .OR. !::__IsStandard )
         IF ::HasMessage( "VertScroll" ) .AND. ::VertScroll .OR. ( ::HasMessage( "AutoVertScroll" ) .AND. ::AutoVertScroll .AND. ::siv != NIL .AND. ::siv:nMax > 0 )
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
RETURN NIL

METHOD OnCommand( nwParam, nlParam ) CLASS Window
   LOCAL nCode, nId, nRet, oCtrl, lHandled, oForm, oChild, oItem
   nCode := HIWORD( nwParam )
   nId   := LOWORD( nwParam )
   IF (nCode IN {0,1}) .AND. nlParam == 0
      oItem := __ObjFromID( nID, ::hWnd )
      IF oItem != NIL

         IF HGetPos( oItem:EventHandler, "OnClick" ) != 0
            IF oItem:ClsName == "MenuStripItem" .AND. oItem:Role == 2
               oItem:Checked := ! oItem:Checked
            ENDIF

            oForm := oItem:Form
            IF ::ClsName == "CCTL"
               oForm := Self
            ENDIF
            nRet := hb_ExecFromArray( oForm, oItem:EventHandler[ "OnClick" ], {oItem} )
          ELSEIF VALTYPE( oItem:Action ) == "B"
            EVAL( oItem:Action, oItem )
         ENDIF
         IF __objHasMsg( oItem, "Cancel" )
            oItem:Cancel()
         ENDIF
         RETURN 0
      ENDIF
   ENDIF

   IF (::Style & WS_CHILD) == 0
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
            IF ::Modal .OR. ::AutoClose
               ::Close( IDCANCEL )
               RETURN 0
            ENDIF
         ENDIF
       ELSEIF nwParam == IDOK

         nRet := ExecuteEvent( "OnOk", Self )
         ODEFAULT nRet TO ::OnOk()
      ENDIF
   ENDIF
   //------------------------- Search for Controls Actions ----------------------------

   IF nlParam != 0
      oCtrl := ObjFromHandle( nlParam )
   ENDIF

   IF nCode == 0 .AND. oCtrl != NIL
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
         RETURN CallWindowProc( ::__nProc, ::hWnd, ::Msg, ::wParam, ::lParam )
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

    ELSEIF oCtrl != NIL
      ODEFAULT nRet TO oCtrl:OnParentCommand( nId, nCode, nlParam )
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
   IF nRet == NIL .AND. nCode == 1 .AND. __objHasMsg( Self, "ActiveMenuBar" ) .AND. ::ActiveMenuBar != NIL
      IF ( oItem := ::ActiveMenuBar:GetMenuById( nId ) ) != NIL
         ExecuteEvent( "OnClick", oItem )
      ENDIF
   ENDIF
RETURN NIL

METHOD BeginPaint( hDC ) CLASS Window
   LOCAL cPaint
   ::__cPaint := NIL
   IF hDC == NIL
      hDC := _BeginPaint( ::hWnd, @cPaint )
      ::__cPaint := cPaint
   ENDIF
RETURN hDC

METHOD EndPaint() CLASS Window
   IF ::__cPaint != NIL
      _EndPaint( ::hWnd, ::__cPaint )
      ::__cPaint := NIL
   ENDIF
RETURN NIL

METHOD OnNCDestroy() CLASS Window
   LOCAL n, aComp, aProperty, aProperties
   aComp := {}
   FOR n := 1 TO LEN( ::Components )
       IF ::Components[n]:Exists
          AADD( aComp, ::Components[n] )
       ENDIF
   NEXT
   AEVAL( aComp, {|o| o:Destroy(.F.) } )

   IF ::HasMessage( "BackgroundImage" ) .AND. ::BackgroundImage != NIL
      ::BackgroundImage:Destroy()
      ::BackgroundImage := NIL
   ENDif

   IF ::Parent != NIL
      IF ( n := ASCAN( ::Parent:__aDock, {|o| o:hWnd == ::hWnd} ) ) > 0
         ADEL( ::Parent:__aDock, n, .T. )
      ENDIF

      IF ::Parent:Children != NIL
         FOR n := 1 TO LEN( ::Parent:Children )
             IF __objHasMsg( ::Parent:Children[n], "Dock" ) .AND. ::Parent:Children[n]:Dock != NIL
                IF VALTYPE(::Parent:Children[n]:Dock:Left)=="O" .AND. ::Parent:Children[n]:Dock:Left:hWnd == ::hWnd
                   ::Parent:Children[n]:Dock:Left := NIL
                ENDIF
                IF VALTYPE(::Parent:Children[n]:Dock:Top)=="O" .AND. ::Parent:Children[n]:Dock:Top:hWnd == ::hWnd
                   ::Parent:Children[n]:Dock:Top := NIL
                ENDIF
                IF VALTYPE(::Parent:Children[n]:Dock:Right)=="O" .AND. ::Parent:Children[n]:Dock:Right:hWnd == ::hWnd
                   ::Parent:Children[n]:Dock:Right := NIL
                ENDIF
                IF VALTYPE(::Parent:Children[n]:Dock:Bottom)=="O" .AND. ::Parent:Children[n]:Dock:Bottom:hWnd == ::hWnd
                   ::Parent:Children[n]:Dock:Bottom := NIL
                ENDIF
             ENDIF
         NEXT
      ENDIF
   ENDIF

   IF ::Drawing != NIL
      ::Drawing:Destroy()
      ::Drawing:Owner := NIL
      ::Drawing := NIL
   ENDIF
   IF ::Dock != NIL
      ::Dock:Destroy()
      ::Dock := NIL
   ENDIF
   IF ::Anchor != NIL
      ::Anchor:Destroy()
      ::Anchor := NIL
   ENDIF
   IF ::DesignMode .AND. ::TreeItem != NIL
      IF ::TreeItem:Cargo != NIL
         ::TreeItem:Cargo:Owner := NIL
         ::TreeItem:Cargo := NIL
      ENDIF
      ::TreeItem := NIL
   ENDIF

   ::Components := {}
   IF ::DesignMode
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

   ::RemoveProperty()

   IF ::DesignMode
      aProperties := __clsGetPropertiesAndValues( Self )
      FOR EACH aProperty IN aProperties
          IF __objHasMsg( Self, "__a_"+aProperty[1] )
             __objSendMsg( Self, "__a_"+aProperty[1] )[4] := NIL
          ENDIF
      NEXT
   ENDIF

   IF ::Parent != NIL .AND. !(::Parent:__xCtrlName == "CoolBar")
      IF ( n := ASCAN( ::Parent:Children, {|o|o:hWnd == ::hWnd} ) ) > 0
         ADEL( ::Parent:Children, n, .T. )
      ENDIF
   ENDIF
   IF ::ToolTip != NIL
      ::ToolTip:Destroy()
      ::ToolTip := NIL
   ENDIF
   IF ::DesignMode .AND. ::Parent != NIL
      TRY
         FOR n := 1 TO LEN( ::Parent:Children )
             ::Parent:Children[n]:xTabOrder := n
             __SetInitialValues( ::Parent:Children[n], "TabOrder" )
         NEXT
      CATCH
      END
   ENDIF
   IF ::Application != NIL .AND. ::__hAccelTable != NIL
      ::Application:DelAccelerators( ::hWnd, ::__hAccelTable )
      DestroyAcceleratorTable( ::__hAccelTable )
      ::__hAccelTable := NIL
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
   IF ::Font != NIL
      ::Font:Owner := NIL
      IF ! ::Font:Shared
         ::Font:Delete()
         IF ::Font:FileName != NIL
            RemoveFontResource( ::Font:FileName )//, FR_PRIVATE | FR_NOT_ENUM )
         ENDIF
      ENDIF
      ::Font:ncm := NIL
      ::Font := NIL
   ENDIF

   IF !EMPTY( ::__hIcon )
      DestroyIcon( ::__hIcon )
   ENDIF

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
      ::BkBrush := NIL
   ENDIF

   IF ::SelBkBrush != NIL
      DeleteObject( ::SelBkBrush )
      ::SelBkBrush := NIL
   ENDIF

   IF ::DesignMode
      ::Events := NIL
   ENDIF

   ::__UnSubClass()

   ObjFromHandle( ::hWnd, .T. )

   ::MDIClient := NIL

   ::__hObjects        := NIL
   ::Children          := NIL
   ::Parent            := NIL
   ::siv               := NIL
   ::sih               := NIL
   ::ScrollInfo        := NIL
   ::hdr               := NIL
   ::WindowPos         := NIL
   //::Cargo             := NIL
   ::Msg               := NIL
   ::wParam            := NIL
   ::lParam            := NIL
   ::__lInitialized    := .F.
   ::__aDock           := NIL

   IF ::__TaskBarParent != NIL
      DestroyWindow( ::__TaskBarParent )
   ENDIF

   IF ! ::DesignMode
      IF ::Application != NIL
         IF ::Application:MainForm != NIL .AND. ::Application:MainForm:hWnd == ::hWnd
            IF ::Application:__hMutex != NIL
               CloseHandle( ::Application:__hMutex )
            ENDIF
            ::Application:Exit()
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnEraseBkgnd( hDC ) CLASS Window
   LOCAL nRet

   IF ::BkBrush == NIL .AND. ::ClsName == "Dialog" .AND. ::Modal .AND. ::xBackColor != NIL .AND. ::xBackColor <> ::__SysBackColor
      ::BkBrush := CreateSolidBrush( ::xBackColor )
   ENDIF

   IF ::BkBrush != NIL
      ::GetClientRect()
      _FillRect( hDC, { 0, 0, ::ClientWidth, ::ClientHeight }, ::BkBrush )
      nRet := 1
   ENDIF
RETURN nRet

//-----------------------------------------------------------------------------------------------
METHOD __ControlProc( hWnd, nMsg, nwParam, nlParam ) CLASS Window
   LOCAL nRet, n, cBuffer, oObj, oChild, oItem, cBlock
   LOCAL lShow, hParent, oCtrl, aRect, aPt, msg, mmi, oForm
   LOCAL pt, code, nMess, mis, dis, bBlock, oMdi, aMenu

   DEFAULT ::hWnd TO hWnd

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

   IF ( nMess := aScan( aMessages, {|a| a[1] == nMsg} ) ) > 0
      nRet := NIL
      ::LastEvent := aMessages[nMess][2]
      IF nMsg == WM_INITDIALOG
         ::hWnd := hWnd
         ::PreInitDialog()
      ENDIF
      cBlock := Left( aMessages[nMess][2], 2 ) + "WM" + SubStr( aMessages[nMess][2], 3 )
      IF __ObjHasMsg( Self, cBlock ) .AND. ( bBlock := __objSendMsg( Self, cBlock ) ) != NIL
         nRet := Eval( bBlock, Self, nwParam, nlParam )
      ENDIF
      IF nRet == NIL
         nRet := hb_ExecFromArray( Self, aMessages[nMess][2], {nwParam, nlParam} )
      ENDIF
      ExecuteEvent( aMessages[nMess][2], Self )
      IF nMsg == WM_INITDIALOG
         ::PostInitDialog()
      ENDIF
   ELSE
      SWITCH nMsg
         CASE WM_NCHITTEST
              nRet := ExecuteEvent( "OnNCHitTest", Self )
              IF nRet == NIL
                 nRet := ::OnNCHitTest( loword(nlParam), hiword(nlParam), nwParam )
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

         CASE WM_MOVING
              nRet := ExecuteEvent( "OnMoving", Self )
              ODEFAULT nRet TO ::OnMoving( LoWord( nlParam ), HiWord( nlParam ) )
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

         CASE WM_INITMENU
              EXIT

         CASE WM_HELP
              nRet := ExecuteEvent( "OnHelp", Self )
              IF nRet == NIL .AND. ::HelpID > 0 .AND. File( ::Application:Path + "\" + ::Application:Name + ".chm" )
                 HTMLHelp( 0, ::Application:Path + "\" + ::Application:Name + ".chm", HH_HELP_CONTEXT, ::HelpID )
                 RETURN .T.
              ENDIF
              EXIT

         CASE WM_HOTKEY
              IF ( n := ASCAN( ::__aHotKey, {|a| a[2]==LOWORD( nlParam ) .AND. a[3]==HIWORD( nlParam ) } ) ) > 0
                 ::PostMessage( WM_COMMAND, MAKELONG(::__aHotKey[n][1],1) )
              ENDIF
              nRet := ExecuteEvent( "OnHotKey", Self )
              ODEFAULT nRet TO ::OnHotKey( nwParam, nlParam )
              EXIT

         CASE WM_GETMINMAXINFO
              mmi  := (struct MINMAXINFO *) nlParam
              nRet := ExecuteEvent( "OnGetMinMaxInfo", Self )

              IF nRet == NIL .AND. ! ::DesignMode
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

         CASE WM_MENUGETOBJECT
              EXIT

         CASE WM_GETDLGCODE
              IF ! ( nlParam == 0 )
                 msg := (struct MSG*) nlParam
                 //aParams    := __GetMSG( nlParam )
                 //msg := {=>}
                 //msg:hwnd    := aParams[1]
                 //msg:message := aParams[2]
                 //msg:wParam  := aParams[3]
                 //msg:lParam  := aParams[4]
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
                 oCtrl := ObjFromHandle( nlParam )
                 IF oCtrl != NIL
                    IF HGetPos( oCtrl:EventHandler, "OnChar" ) != 0
                       nRet := ::&( oCtrl:EventHandler[ "OnChar" ] )( Self )
                    ENDIF
                 ENDIF
              ENDIF
              EXIT

         CASE WM_NEXTDLGCTL
              EXIT

         CASE WM_KILLFOCUS
              nRet := ::OnKillFocus( nwParam, nlParam )
              ODEFAULT nRet TO __Evaluate( ::OnWMKillFocus, Self, nwParam, nlParam, nRet )
              ODEFAULT nRet TO nRet := ExecuteEvent( "OnKillFocus", Self )

              IF ::Parent != NIL .AND. ::Parent:ClsName == "PanelBox" .AND. ASCAN( ::Parent:Children, {|o| o:hWnd == nwParam } ) == 0
                 ::Parent:oLastFocus := Self
                 ::Parent:SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
              ENDIF
              IF .F. //::Parent != NIL
                 oCtrl := Self
                 WHILE oCtrl != NIL
                    IF __objHasMsg( oCtrl, "SetActive" )
                       oCtrl:SetActive(.F.)
                    ENDIF
                    oCtrl := oCtrl:Parent
                 ENDDO
              ENDIF
              EXIT

         CASE WM_MEASUREITEM
              mis := (struct MEASUREITEMSTRUCT*) nlParam

              IF mis:CtlType == ODT_MENU .AND. mis:itemData != NIL .AND. mis:itemData <> 0
                 IF ( oCtrl := __ObjFromPtr( mis:itemData ) ) != NIL
                    IF __objHasMsg( oCtrl, "OnMeasureItem" )
                       nRet := oCtrl:OnMeasureItem( nwParam, nlParam, mis )
                     ELSE
                       nRet := ::OnMeasureItem( nwParam, nlParam )
                    ENDIF
                 ENDIF
               ELSE
                 nRet := ExecuteEvent( "OnMeasureItem", Self )
                 ODEFAULT nRet TO ::OnMeasureItem( nwParam, nlParam, mis )

                 IF nRet == NIL .AND. ( n := ASCAN( ::Children, {|o| o:Id == mis:itemID} ) ) > 0
                    oCtrl := ::Children[n]
                    IF HGetPos( oCtrl:EventHandler, "OnParentMeasureItem" ) != 0
                       nRet := ::&( oCtrl:EventHandler[ "OnParentMeasureItem" ] )( Self )
                    ENDIF
                    ODEFAULT nRet TO oCtrl:OnParentMeasureItem(nwParam,nlParam, mis)
                 ENDIF

              ENDIF
              EXIT

         CASE WM_DRAWITEM
              dis := (struct DRAWITEMSTRUCT*) nlParam
              IF dis:CtlType == ODT_MENU .AND. dis:itemData != NIL .AND. dis:itemData <> 0
                 IF ( oCtrl := __ObjFromPtr( dis:itemData ) ) != NIL .AND. VALTYPE( oCtrl ) == "O"
                    IF __objHasMsg( oCtrl, "OnMeasureItem" )
                       nRet := oCtrl:OnDrawItem( nwParam, nlParam, dis )
                     ELSE
                       nRet := ::OnDrawItem( nwParam, nlParam, dis )
                    ENDIF
                 ENDIF
               ELSE

                 nRet := ExecuteEvent( "OnDrawItem", Self )
                 ODEFAULT nRet TO ::OnDrawItem( nwParam, nlParam, dis )
                 IF nRet == NIL
                    oCtrl := ObjFromHandle( dis:hwndItem )
                    IF oCtrl != NIL
                       IF HGetPos( oCtrl:EventHandler, "OnParentDrawItem" ) != 0
                          nRet := ::&( oCtrl:EventHandler[ "OnParentDrawItem" ] )( oCtrl, dis )
                       ENDIF
                       ODEFAULT nRet TO oCtrl:OnParentDrawItem( nwParam, nlParam, dis )
                    ENDIF
                 ENDIF

              ENDIF
              EXIT

         CASE WM_DRAWCLIPBOARD
              nRet := ExecuteEvent( "OnDrawClipboard", Self )
              ODEFAULT nRet TO ::OnDrawClipboard( nwParam, nlParam )
              EXIT

         CASE WM_CHANGECBCHAIN
              nRet := ExecuteEvent( "OnChangeCbChain", Self )
              ODEFAULT nRet TO ::OnChangeCbChain( nwParam, nlParam )
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
              IF nwParam == SC_HELP
                 IF ::HelpID > 0 .AND. File( ::Application:Path + "\" + ::Application:Name + ".chm" )
                    HTMLHelp( 0, ::Application:Path + "\" + ::Application:Name + ".chm", HH_HELP_CONTEXT, ::HelpID )
                    RETURN .F.
                 ENDIF
              ELSE
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
                    IF nwParam == SC_MINIMIZE
                       ::__aMinRect := {::xLeft,::xTop,::xWidth,::xHeight}
                    ENDIF
                    ::PostMessage( WM_VXH_SHOWMODE )
                 ENDIF
              ENDIF
              EXIT

         CASE WM_SYSDEADCHAR
              EXIT

         CASE WM_CTLCOLORSCROLLBAR
              nRet := ExecuteEvent( "OnCtlColorScrollBar", Self )
              ODEFAULT nRet TO ::OnCtlColorScrollBar( nwParam, nlParam )
              //IF ( n := ASCAN( ::Children, {|o|o:hWnd == nlParam} ) ) > 0
              //   nRet := ::Children[n]:OnCtlColorScrollBar( nwParam, nlParam )
              //ENDIF
              EXIT

         CASE WM_CTLCOLORBTN
              nRet := ExecuteEvent( "OnCtlColorBtn", Self )
              ODEFAULT nRet TO ::OnCtlColorBtn( nwParam, nlParam )
              oCtrl := ObjFromHandle( nlParam )
              IF oCtrl != NIL
                 nRet := oCtrl:OnCtlColorBtn( nwParam, nlParam )
              ENDIF
              EXIT

         CASE WM_CTLCOLORSTATIC
              nRet := ExecuteEvent( "OnCtlColorStatic", Self )
              ODEFAULT nRet TO ::OnCtlColorStatic( nwParam, nlParam )
              IF nRet == NIL
                 oCtrl := ObjFromHandle( nlParam )
                 IF oCtrl != NIL
                    nRet := oCtrl:OnCtlColorStatic( nwParam, nlParam )
                 ENDIF
              ENDIF
              EXIT

         CASE WM_CTLCOLOREDIT
              nRet := ExecuteEvent( "OnCtlColorEdit", Self )
              ODEFAULT nRet TO ::OnCtlColorEdit( nwParam, nlParam )
              IF nRet == NIL
                 IF ( oObj := ObjFromHandle( nlParam ) ) != NIL
                    nRet := oObj:OnCtlColorEdit( nwParam, nlParam )
                 ELSEIF ::__xCtrlName == "DataGrid" .AND. ::__CurControl != NIL
                    nRet := ::__CurControl:OnCtlColorEdit( nwParam, nlParam )
                 ENDIF
              ENDIF
              EXIT

         CASE WM_CTLCOLORDLG
//              nRet := ExecuteEvent( "OnCtlColorDlg", Self )
//              ODEFAULT nRet TO ::OnCtlColorDlg( nwParam, nlParam )
//              IF nRet == NIL .AND. ::BkBrush != NIL
//                 RETURN( ::BkBrush )
//              ENDIF
              EXIT

         CASE WM_CTLCOLORLISTBOX
              nRet := ExecuteEvent( "OnCtlColorListBox", Self )
              ODEFAULT nRet TO ::OnCtlColorListBox( nwParam, nlParam )
              IF nRet == NIL
                 IF ( oObj := ObjFromHandle( nlParam ) ) != NIL
                    nRet := oObj:OnCtlColorListBox( nwParam, nlParam )
                 ENDIF
              ENDIF
              EXIT

         CASE WM_NOTIFY
              ::hdr := (struct NMHDR*) nlParam

              nRet := ExecuteEvent( "OnNotify", Self )
              nRet := ::OnNotify( nwParam, nlParam, code )

              IF nRet == NIL
                 oCtrl := ObjFromHandle( ::hdr:hwndFrom )
                 IF oCtrl != NIL
                    IF HGetPos( oCtrl:EventHandler, "OnParentNotify" ) != 0
                       nRet := ::&( oCtrl:EventHandler[ "OnParentNotify" ] )( oCtrl )
                    END
                    ODEFAULT nRet TO oCtrl:OnParentNotify( nwParam, nlParam, ::hdr )
                    IF VALTYPE( nRet ) == "O"
                       nRet := NIL
                    ENDIF
                 ENDIF
                 IF nRet == NIL .AND. ::hdr != NIL .AND. ::hdr:code $ { TTN_GETDISPINFOW, TTN_GETDISPINFO }
                    IF ::ClsName != "DataGrid"
                       IF __objHasMsg( Self, "OnToolTipNotify" )
                          ODEFAULT nRet TO ::OnToolTipNotify( nwParam, nlParam, ::hdr )
                       ELSE
                          FOR EACH oChild IN ::Children
                              IF HGetPos( oChild:EventHandler, "OnToolTipNotify" ) != 0
                                 nRet := ::&( oChild:EventHandler[ "OnToolTipNotify" ] )( oChild )
                              END
                              IF __objHasMsg( oChild, "OnToolTipNotify" )
                                 ODEFAULT nRet TO oChild:OnToolTipNotify( nwParam, nlParam, ::hdr )
                              ENDIF
                          NEXT
                       ENDIF
                       IF VALTYPE( nRet ) == "O"
                          nRet := NIL
                       ENDIF
                    ENDIF
                 ENDIF

              ENDIF
              EXIT

         CASE WM_WINDOWPOSCHANGED
              ::WindowPos := (struct WINDOWPOS*) nlParam
              IF ::ClsName != "VXH_FORM_IDE"
                 ::xLeft   := ::WindowPos:x
                 ::xTop    := ::WindowPos:y
              ENDIF

              ::xWidth  := ::WindowPos:cx
              IF ::ClsName != "ComboBox" //.OR. (::Style & CBS_SIMPLE) == CBS_SIMPLE
                 ::xHeight := ::WindowPos:cy
              ENDIF

              IF ::Parent != NIL .AND. ::ClassName == "WINDOWEDIT"
                 aRect := _GetWindowRect( ::hWnd )
                 aPt := { aRect[1], aRect[2] }
                 IF ::DesignMode .AND. ::Parent:hWnd != GetParent( ::hWnd )
                    _ScreenToClient( GetParent( ::hWnd ), @aPt )
                    ::__TempRect := { aPt[1], aPt[2], aPt[1]+::WindowPos:cx, aPt[2]+::WindowPos:cy }
                    RETURN NIL
                 ENDIF
                 _ScreenToClient( ::Parent:hWnd, @aPt )
                 ::__TempRect := { aPt[1], aPt[2], aPt[1]+::WindowPos:cx, aPt[2]+::WindowPos:cy }
              ENDIF

              nRet := ExecuteEvent( "OnWindowPosChanged", Self )
              ODEFAULT nRet TO ::OnWindowPosChanged( nwParam, nlParam )
              EXIT

         CASE WM_WINDOWPOSCHANGING
              nRet := ExecuteEvent( "OnWindowPosChanging", Self )
              ODEFAULT nRet TO ::OnWindowPosChanging( nwParam, nlParam )
              IF ::Parent != NIL .AND. ::ClsName == "MDIChild"
                 ::WindowPos := (struct WINDOWPOS*) nlParam
                 IF ::WindowPos:flags != 20
                    IF ( n := ASCAN( ::Parent:Parent:Children, {|o|o:__xCtrlName == "CoolMenu"} ) ) > 0
                       PostMessage( ::Parent:hWnd, WM_MDICHILDSIZED, n )
                       RETURN 0
                    ENDIF
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

         CASE WM_SYSKEYDOWN
              nRet := ExecuteEvent( "OnSysKeyDown", Self )
              ODEFAULT nRet TO ::OnSysKeyDown( nwParam, nlParam )
              IF nRet == NIL .AND. ::Form != NIL
                 FOR EACH oChild IN ::Form:Children
                     IF (oChild:__xCtrlName IN {"CoolMenu","MenuStrip","ToolStripContainer"})
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

              IF ( aMenu := __GetMenuItemInfo( nlParam, nwParam, .T. ) ) != NIL
                 IF aMenu[1] <> 0
                    oItem := __ObjFromPtr( aMenu[1] )
                 ENDIF
              ENDIF

              IF oItem != NIL
                 IF HGetPos( oItem:EventHandler, "OnClick" ) != 0
                    IF oItem:ClsName == "MenuStripItem" .AND. oItem:Role == 2
                       oItem:Checked := ! oItem:Checked
                    ENDIF

                    oForm := oItem:Form
                    IF ::ClsName == "CCTL"
                       oForm := Self
                    ENDIF
                    nRet := hb_ExecFromArray( oForm, oItem:EventHandler[ "OnClick" ], {oItem} )
                  ELSEIF oItem:ClsName == "MenuStripItem" .AND. VALTYPE( oItem:Action ) == "B"
                    EVAL( oItem:Action, oItem )
                  ELSE
                    ODEFAULT nRet TO __Evaluate( oItem:Action, oItem,,, nRet )
                    IF __objHasMsg( oItem, "OnClick" )
                       oItem:OnClick( oItem )
                    ENDIF
                 ENDIF
                 IF __objHasMsg( oItem, "Cancel" )
                    oItem:Cancel()
                 ENDIF

              ELSE

                 IF ::MdiContainer .AND. ( oMDI := ::MdiGetActive() ) != NIL
                    IF nwParam == 8 //SC_NEXTWINDOW
                       oMdi:MdiNext()

                     ELSE
                       SWITCH aMenu[2]
                          CASE HBMMENU_POPUP_MINIMIZE
                          CASE HBMMENU_MBAR_MINIMIZE
                               oMdi:Minimize()
                               EXIT

                          CASE HBMMENU_POPUP_RESTORE
                          CASE HBMMENU_MBAR_RESTORE
                               oMdi:Restore()
                               EXIT

                          CASE HBMMENU_POPUP_CLOSE
                          CASE HBMMENU_MBAR_CLOSE
                               oMdi:Close()
                               EXIT
                       END
                    ENDIF
                 ENDIF

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

         CASE WM_MDICHILDSIZED
              lShow := (::GetWindowLong( GWL_STYLE ) & WS_MAXIMIZE) != 0
              ::Parent:Children[nwParam]:UpdateMenu( lShow )
              RETURN 1

         DEFAULT
              IF nMsg >= WM_USER .AND. nMsg < WM_APP
                 IF LOWORD( nlParam ) == WM_RBUTTONDOWN
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
               ELSEIF nMsg >= WM_APP .AND. nMsg <= 49151
                 TRY
                    DO CASE
                       CASE nMsg == WM_VXH_SHOWMODE
                            ::ShowMode := ::__GetShowMode()

                       OTHERWISE
                            ODEFAULT nRet TO ::OnSystemMessage( nMsg, nwParam, nlParam)
                    ENDCASE
                 CATCH
                 END
               ELSE
                 nRet := ExecuteEvent( "OnMessage", Self )
                 ODEFAULT nRet TO ::OnMessage( nMsg, nwParam, nlParam)
              ENDIF
              EXIT
      END
   ENDIF

   IF ! IsWindow( ::hWnd ) .OR. ! IsWindow( hWnd )
      RETURN 0
   ENDIF
   IF VALTYPE( nRet ) == "O"
      nRet := NIL
   ENDIF
   IF nRet != NIL
      IF ::__WindowStyle == WT_DIALOG //::Modal
         IF VALTYPE( nRet ) == "L"
            nRet := IIF( nRet, 1, 0 )
         ENDIF
         SetWindowLong( hWnd, DWL_MSGRESULT, nRet )
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

METHOD OnMeasureItem( nwParam, nlParam, mis ) CLASS Window
   LOCAL n, oItem, oButton, oSub, oMenu
   (nwParam)
   IF ::Application != NIL
      oMenu := ::Application:oCurMenu
   ENDIF

   IF mis:CtlType == ODT_MENU .AND. oMenu != NIL
      FOR EACH oButton IN oMenu:aItems
         IF (oButton:MenuItemInfo:fType & BTNS_SEP) == 0 .AND. oButton:Menu != NIL
            IF ( n := ASCAN( oButton:Menu:aItems, {|o|o:Id==mis:itemID } ) ) > 0
               RETURN oButton:Menu:aItems[n]:MeasureItem( mis, nlParam )
            ENDIF
            // it is NOT the 1st level, let's see the rest
            FOR EACH oSub IN oButton:Menu:aItems
               IF ( oItem := oSub:GetMenuById( mis:itemID ) )!= NIL
                  RETURN oItem:MeasureItem( mis, nlParam )
               ENDIF
            NEXT
         ENDIF
      NEXT
   ENDIF
RETURN NIL

METHOD OnSetFocus() CLASS Window
   LOCAL aRect, aPt, n, nRet, aParent
   IF ::Parent != NIL .AND. ::Parent:VertScroll

      IF ::Parent:ScrollOnChildFocus
         aRect    := _GetWindowRect( ::hWnd )

         aPt := { aRect[1], aRect[2] }
         _ScreenToClient( ::Parent:hWnd, @aPt )
         aRect[1] := aPt[1]
         aRect[2] := aPt[2]

         aPt := { aRect[3], aRect[4] }
         _ScreenToClient( ::Parent:hWnd, @aPt )
         aRect[3] := aPt[1]
         aRect[4] := aPt[2]

         aParent  := _GetClientRect( ::Parent:hWnd )

         // Set Vertical Position
         IF aRect[4] > aParent[4]
            n := aParent[2]+( aRect[4]-aParent[4] ) - aRect[2]
            ::Parent:OnVScroll( SB_THUMBTRACK, ::Parent:VertScrollPos + ( aRect[4]-aParent[4] )- MAX(n,0), 0 )
          ELSEIF aRect[2] < 0
            ::Parent:OnVScroll( SB_THUMBTRACK, ::Parent:VertScrollPos + ( aRect[2] ), 0 )
         ENDIF

         // Set Horizontal Position
         IF aRect[3] > aParent[3]
            n := aParent[1]+( aRect[3]-aParent[3] ) - aRect[1]
            ::Parent:OnHScroll( SB_THUMBTRACK, ::Parent:HorzScrollPos + ( aRect[3]-aParent[3] ) - MAX(n,0), 0 )
          ELSEIF aRect[1] < 0
            ::Parent:OnHScroll( SB_THUMBTRACK, ::Parent:HorzScrollPos + ( aRect[1] ), 0 )
         ENDIF
      ENDIF
   ENDIF
RETURN nRet

METHOD OnDrawItem( nwParam, nlParam, dis ) CLASS Window
   LOCAL n, oItem, oButton, oSub, oMenu
   (nwParam, nlParam)
   oMenu := ::Application:oCurMenu
   IF dis:CtlType == ODT_MENU .AND. oMenu != NIL
      IF dis:itemState > 200
         dis:itemState -= 256
      ENDIF
      FOR EACH oButton IN oMenu:aItems
         IF (oButton:MenuItemInfo:fType & BTNS_SEP) == 0 .AND. oButton:Menu != NIL
            IF ( n := ASCAN( oButton:Menu:aItems, {|o|o:Id==dis:itemID } ) ) > 0
               oButton:Menu:aItems[n]:DrawItem( dis, .F. )
               RETURN 1
            ENDIF
            // it is NOT the 1st level, let's see the rest
            FOR EACH oSub IN oButton:Menu:aItems
               IF ( oItem := oSub:GetMenuById(dis:itemID ) )!= NIL
                  oItem:DrawItem( dis, .F. )
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
   LOCAL aPt, aRect

   aRect := _GetWindowRect( ::hWnd )

   IF ::Parent != NIL

      aPt := { aRect[1], aRect[2] }
      _ScreenToClient( ::Parent:hWnd, @aPt )

      ::xLeft   := aPt[1]
      ::xTop    := aPt[2]
    ELSE
      ::xLeft   := aRect[1]
      ::xTop    := aRect[2]

   ENDIF
   ::xWidth  := aRect[3] - aRect[1]
   IF ::ClsName != "ComboBox" //.OR. (::Style & CBS_SIMPLE) == CBS_SIMPLE
      ::xHeight := aRect[4] - aRect[2]
   ENDIF
RETURN aRect

//-----------------------------------------------------------------------------------------------

METHOD __SetScrollBars() CLASS Window
   STATIC lBusy := .F.
   LOCAL nDelta

   // added variables
   LOCAL nWidth       := ::ClientWidth
   LOCAL nHeight      := ::ClientHeight
   LOCAL nVertSize    := ::ClientHeight
   LOCAL nHorzSize    := ::ClientWidth

   IF lBusy .OR. ::DesignMode
      //TraceLog( "Nested Recursion!" )
      RETURN NIL
   ELSE
      lBusy := .T.
   ENDIF

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
   IF ::IsWindow() .AND. ::Parent != NIL
      ::RegisterDocking()
      ::Parent:UpdateWindow()
      ::__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight, NIL, .T. )
   ENDIF
RETURN Self

METHOD Animate( nSpeed, nFlags ) CLASS Window
   AnimateWindow( ::hWnd, nSpeed, nFlags )
   ::xVisible := ( nFlags & AW_HIDE ) == 0
RETURN Self

//------------------------------------------------------------------------------------------------------

METHOD __FixDocking() CLASS Window
   LOCAL oObj, cObj
   IF ::__hObjects != NIL
      FOR EACH cObj IN ::__hObjects:Keys
          oObj := ::__hObjects[ cObj ]
          IF oObj:HasMessage( "Dock" ) .AND. oObj:Dock != NIL
             IF VALTYPE( oObj:Dock:Left ) == "C" .AND. HGetPos( ::__hObjects, oObj:Dock:Left ) > 0
                oObj:Dock:Left   := IIF( oObj:Dock:Left == oObj:Parent:Name, oObj:Parent, ::__hObjects[ oObj:Dock:Left ] )
             ENDIF
             IF VALTYPE( oObj:Dock:Top ) == "C" .AND. HGetPos( ::__hObjects, oObj:Dock:Top ) > 0
                oObj:Dock:Top    := IIF( oObj:Dock:Top == oObj:Parent:Name, oObj:Parent, ::__hObjects[ oObj:Dock:Top ] )
             ENDIF
             IF VALTYPE( oObj:Dock:Right ) == "C" .AND. HGetPos( ::__hObjects, oObj:Dock:Right ) > 0
                oObj:Dock:Right  := IIF( oObj:Dock:Right == oObj:Parent:Name, oObj:Parent, ::__hObjects[ oObj:Dock:Right ] )
             ENDIF
             IF VALTYPE( oObj:Dock:Bottom ) == "C" .AND. HGetPos( ::__hObjects, oObj:Dock:Bottom ) > 0
                oObj:Dock:Bottom := IIF( oObj:Dock:Bottom == oObj:Parent:Name, oObj:Parent, ::__hObjects[ oObj:Dock:Bottom ] )
             ENDIF
          ENDIF
      NEXT
   ENDIF
RETURN Self


METHOD __OnParentSize( x, y, hDef, lMoveNow, lNoMove, nParX, nParY ) CLASS Window
   LOCAL n, nCurLeft, nCurTop, oLeft, oTop, oRight, oBottom, lAnchor, lDock, nHeight, nMargin, aRect

   DEFAULT lMoveNow TO FALSE
   DEFAULT lNoMove TO FALSE
   IF ::Parent != NIL
      DEFAULT x TO ::Parent:Width
      DEFAULT y TO ::Parent:Height
      DEFAULT nParX TO ::Parent:__aCltRect[3]
      DEFAULT nParY TO ::Parent:__aCltRect[4]
   ENDIF

   IF ::__HideResized .OR. !::IsChild .OR. !IsWindow( ::hWnd ) .OR. ::hWnd == NIL .OR. !::__Docked
      RETURN Self
   ENDIF

   IF ::DesignMode .AND. !::Application:ShowDocking .AND. ::ClsName != "ToolStripContainer" .AND. ::ClsName != "ToolStrip"
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

   //IF lAnchor .AND. ::DesignMode
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
          ELSEIF HGetPos( ::Form:__hObjects, oLeft ) > 0
            oLeft := ::Form:__hObjects[ oLeft ]
         ENDIF
      ENDIF
      IF VALTYPE( oLeft ) == "B"
         oLeft := EVAL( oLeft, Self )
      ENDIF

      oTop    := ::Dock:Top
      IF VALTYPE( oTop ) == "C"
         IF oTop == ::Parent:Name
            oTop := ::Parent
          ELSEIF HGetPos( ::Form:__hObjects, oTop ) > 0
            oTop := ::Form:__hObjects[ oTop ]
         ENDIF
      ENDIF
      IF VALTYPE( oTop ) == "B"
         oTop := EVAL( oTop, Self )
      ENDIF

      oRight  := ::Dock:Right
      IF VALTYPE( oRight ) == "C"
         IF oRight == ::Parent:Name
            oRight := ::Parent
          ELSEIF HGetPos( ::Form:__hObjects, oRight ) > 0
            oRight := ::Form:__hObjects[ oRight ]
         ENDIF
      ENDIF
      IF VALTYPE( oRight ) == "B"
         oRight := EVAL( oRight, Self )
      ENDIF

      oBottom := ::Dock:Bottom
      IF VALTYPE( oBottom ) == "C"
         IF oBottom == ::Parent:Name
            oBottom := ::Parent
          ELSEIF HGetPos( ::Form:__hObjects, oBottom ) > 0
            oBottom := ::Form:__hObjects[ oBottom ]
         ENDIF
      ENDIF
      IF VALTYPE( oBottom ) == "B"
         oBottom := EVAL( oBottom, Self )
      ENDIF

      IF ! ::DesignMode
         IF ISOBJECT( oLeft ) .AND. oLeft:AutoDock .AND. ( oLeft:Hidden .OR. !oLeft:__Docked .OR. oLeft:Style & WS_VISIBLE == 0 )
            WHILE oLeft != NIL .AND. oLeft:Parent != NIL .AND. oLeft != NIL .AND. oLeft:hWnd != ::Parent:hWnd .AND. ( oLeft:Hidden .OR. !oLeft:__Docked .OR. !oLeft:IsWindow() .OR. oLeft:Style & WS_VISIBLE == 0 ) //.AND. oLeft:IsWindow()
               IF oLeft:Dock != NIL
                  oLeft := oLeft:Dock:Left
                  IF VALTYPE( oLeft ) == "C"
                     IF oLeft == ::Parent:Name
                        oLeft := ::Parent
                      ELSE
                        oLeft := ::Form:__hObjects[ oLeft ]
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

         IF ISOBJECT( oTop ) .AND. oTop:AutoDock .AND. ( oTop:Hidden .OR. !oTop:__Docked .OR. oTop:Style & WS_VISIBLE == 0 )
            WHILE oTop != NIL .AND. oTop:Parent != NIL .AND. oTop != NIL .AND. oTop:hWnd != ::Parent:hWnd .AND. ( oTop:Hidden .OR. !oTop:__Docked .OR. !oTop:IsWindow() .OR. oTop:Style & WS_VISIBLE == 0 ) //.AND. oTop:IsWindow()
               IF oTop:Dock != NIL
                  oTop := oTop:Dock:Top
                  IF VALTYPE( oTop ) == "C"
                     IF oTop == ::Parent:Name
                        oTop := ::Parent
                      ELSE
                        oTop := ::Form:__hObjects[ oTop ]
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

         IF ISOBJECT( oRight ) .AND. oRight:AutoDock .AND. ( oRight:Hidden .OR. !oRight:__Docked .OR. oRight:Style & WS_VISIBLE == 0 )
            WHILE oRight != NIL .AND. oRight:Parent != NIL .AND. oRight != NIL .AND. oRight:hWnd != ::Parent:hWnd .AND. ( oRight:Hidden .OR. !oRight:__Docked .OR. !oRight:IsWindow() .OR. oRight:Style & WS_VISIBLE == 0 )//.AND. oRight:IsWindow()
               IF oRight:Dock != NIL
                  oRight := oRight:Dock:Right
                  IF VALTYPE( oRight ) == "C"
                     IF oRight == ::Parent:Name
                        oRight := ::Parent
                      ELSE
                        oRight := ::Form:__hObjects[ oRight ]
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

         IF ISOBJECT( oBottom ) .AND. oBottom:AutoDock .AND. ( oBottom:Hidden .OR. !oBottom:__Docked .OR. oBottom:Style & WS_VISIBLE == 0 )
            WHILE oBottom != NIL .AND. oBottom:Parent != NIL .AND. oBottom != NIL .AND. oBottom:hWnd != ::Parent:hWnd .AND. ( oBottom:Hidden .OR. !oBottom:__Docked .OR. !oBottom:IsWindow() .OR. oBottom:Style & WS_VISIBLE == 0 ) //.AND. oBottom:IsWindow()
               IF oBottom:Dock != NIL
                  oBottom := oBottom:Dock:Bottom
                  IF VALTYPE( oBottom ) == "C"
                     IF oBottom == ::Parent:Name
                        oBottom := ::Parent
                      ELSE
                        oBottom := ::Form:__hObjects[ oBottom ]
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
/*
      IF oLeft != NIL .AND. oLeft:hWnd != ::Parent:hWnd .AND. oLeft:Dock:IsDocked()
         oLeft:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight )
      ENDIF
      IF oTop != NIL .AND. oTop:hWnd != ::Parent:hWnd .AND. oTop:Dock:IsDocked()
         oTop:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight )
      ENDIF
      IF oRight != NIL .AND. oRight:hWnd != ::Parent:hWnd .AND. oRight:Dock:IsDocked()
         oRight:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight )
      ENDIF
      IF oBottom != NIL .AND. oBottom:hWnd != ::Parent:hWnd .AND. oBottom:Dock:IsDocked()
         oBottom:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight )
      ENDIF
*/
      IF ! ::__Splitting
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
         IF ::ClsName != "ComboBox" //.OR. (::Style & CBS_SIMPLE) == CBS_SIMPLE
            ::xHeight := ::xHeight + IIF( ::Anchor:Bottom .AND. ::Parent:__aCltRect != NIL .AND. y != NIL, y - ::Parent:__aCltRect[4], 0 )
         ENDIF
      ENDIF

      // override Left
      IF ISOBJECT( oLeft )
         IF oLeft:hWnd == ::Parent:hWnd
            ::xLeft := ::Dock:LeftMargin
          ELSEIF oLeft:IsChild
            nMargin := ::Dock:LeftMargin
            IF oLeft:RightSplitter != NIL
               nMargin := MAX( nMargin, oLeft:RightSplitter:Weight )
            ENDIF
            ::xLeft := oLeft:xLeft + oLeft:xWidth + nMargin
         ENDIF
       ELSEIF ::Dock:JoinLeft != NIL
         ::xLeft := ::Dock:JoinLeft:Left
         oLeft := ::Dock:JoinLeft
      ENDIF

      // override top
      IF ISOBJECT( oTop )
         IF oTop:hWnd == ::Parent:hWnd
            ::xTop  := ::Dock:TopMargin
          ELSEIF oTop:IsChild
            IF oTop:ClsName == "ComboBox" //.AND. (oTop:Style & CBS_SIMPLE) == 0
               aRect := _GetClientRect( oTop:hWnd )
               n := aRect[4]
             ELSE
               n := oTop:Height
            ENDIF
            nMargin := ::Dock:TopMargin
            IF oTop:BottomSplitter != NIL
               nMargin := MAX( nMargin, oTop:BottomSplitter:Weight )
            ENDIF
            ::xTop  := oTop:Top + n + ::Dock:TopMargin + nMargin //IIF( oTop:BottomSplitter != NIL, oTop:BottomSplitter:Weight, 0 )
         ENDIF
      ENDIF

      IF ISOBJECT( oRight )
         // Right Docking
         IF oLeft == NIL .AND. !::__Splitting // move to the right
            IF oRight:hWnd == ::Parent:hWnd
               ::xLeft := ::Parent:ClientWidth - ::xWidth - ::Dock:RightMargin
             ELSEIF oRight:IsChild
               nMargin := ::Dock:RightMargin
               IF oRight:LeftSplitter != NIL
                  nMargin := MAX( nMargin, oRight:LeftSplitter:Weight )
               ENDIF
               ::xLeft := oRight:xLeft - ::xWidth - nMargin //::Dock:RightMargin - IIF( oRight:LeftSplitter != NIL, oRight:LeftSplitter:Weight, 0 )
            ENDIF
          ELSE // resize to the right
            IF oRight:hWnd == ::Parent:hWnd
               ::xWidth := oRight:ClientWidth - ::xLeft - ::Dock:RightMargin
             ELSEIF oRight:IsChild
               nMargin := ::Dock:RightMargin
               IF oRight:LeftSplitter != NIL
                  nMargin := MAX( nMargin, oRight:LeftSplitter:Weight )
               ENDIF
               IF ::__xCtrlName == "TabControl" .AND. ::Theming .AND. ::Application:ThemeActive
                  nMargin -= 2 // Theme shadow?
               ENDIF
               ::xWidth := oRight:xLeft - ::xLeft - nMargin //::Dock:RightMargin - //IIF( oRight:LeftSplitter != NIL, oRight:LeftSplitter:Weight, 0 )
            ENDIF
         ENDIF
       ELSEIF oLeft != NIL .AND. ::Dock:RightProportional
         DEFAULT ::Dock:__nWidthPerc TO ( ::xWidth  / ( ::Parent:ClientWidth - -::xLeft ) )
         ::xWidth  := Int( ( ::Parent:ClientWidth  ) * ::Dock:__nWidthPerc )
      ENDIF

      IF ISOBJECT( oBottom )
         // Bottom Docking
         IF oTop == NIL
            IF oBottom:hWnd == ::Parent:hWnd
               ::xTop := ::Parent:ClientHeight - ::xHeight - ::Dock:BottomMargin
             ELSEIF oBottom:IsChild
               ::xTop := oBottom:Top - ::xHeight - ::Dock:BottomMargin - IIF( oBottom:TopSplitter != NIL, oBottom:TopSplitter:Weight, 0 )
            ENDIF
          ELSEIF ::ClsName != "ComboBox" //.OR. (::Style & CBS_SIMPLE) == CBS_SIMPLE
            IF oBottom:hWnd == ::Parent:hWnd
               ::xHeight := oBottom:ClientHeight - ::xTop - ::Dock:BottomMargin
             ELSEIF oBottom:IsChild
               nMargin := ::Dock:BottomMargin
               IF oBottom:TopSplitter != NIL
                  nMargin := MAX( nMargin, oBottom:TopSplitter:Weight )
               ENDIF
               ::xHeight := oBottom:Top  - ::xTop - nMargin
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
         IF ::ClsName != "ComboBox" //.OR. (::Style & CBS_SIMPLE) == CBS_SIMPLE
            ::xHeight := Int( ( ::Parent:Height - ::xTop ) * ::__HeightPerc )
         ENDIF
       ELSEIF ::Parent:__Splitting
         IF oBottom == NIL .AND. ::Anchor:Bottom .AND. ( ::ClsName != "ComboBox" /* .OR. (::Style & CBS_SIMPLE) == CBS_SIMPLE*/ )
            IF ::TopSplitter != NIL .AND. ::TopSplitter:lSizing
               ::xHeight := ::aPrevSize[4] + ( ::aPrevSize[2]-::xTop )
             ELSE
               ::xHeight += ( nCurTop - ::xTop )
            ENDIF
         ENDIF
         IF oTop == NIL .AND. ::Anchor:Top
            IF ::ClsName != "ComboBox" //.OR. (::Style & CBS_SIMPLE) == CBS_SIMPLE
               ::xHeight += ( ::xTop-nCurTop )
            ENDIF
            ::xTop    -= ( ::xTop-nCurTop )
         ENDIF
      ENDIF
      IF !lNoMove

         IF ::ClsName == "ComboBox" //.AND. (::Style & CBS_SIMPLE) <> CBS_SIMPLE
            aRect := _GetClientRect( ::hWnd )
            nHeight := aRect[4]
          ELSE
            nHeight := ::xHeight
         ENDIF

         IF lMoveNow .OR. !::DeferRedraw .OR. hDef == NIL
            n := (SWP_NOOWNERZORDER | SWP_NOZORDER)
            IF ::DeferRedraw
               n := (n | SWP_NOACTIVATE | SWP_DEFERERASE)
            ENDIF
            SetWindowPos( ::hWnd, , ::xLeft, ::xTop, ::xWidth, nHeight, n )
          ELSE
            DeferWindowPos( hDef, ::hWnd, , ::xLeft, ::xTop, ::xWidth, nHeight, (SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER) ) //| IIF( ::Application:OsVersion:dwMajorVersion < 5, SWP_DEFERERASE, 0 ) )
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
   IF (nFlags & TME_HOVER) == TME_HOVER
      DEFAULT nHoverTimeOut TO HOVER_DEFAULT
      tme:dwHoverTime := nHoverTimeOut
   ENDIF
   TrackMouseEvent( tme )
RETURN Self

//---------------------------------------------------------------------------------------------

METHOD MoveWindow( x, y, w, h, lRep ) CLASS Window
   DEFAULT x    TO ::xLeft
   DEFAULT y    TO ::xTop
   DEFAULT w    TO ::xWidth
   DEFAULT h    TO ::xHeight

   DEFAULT lRep TO ::IsWindowVisible()

   ::xLeft  := x
   ::xTop   := y
   ::xWidth := w
   ::xHeight:= h

   IF ::ClsName == "VXH_FORM_IDE" .AND. ::DesignMode
      x := 10-::Parent:HorzScrollPos
      y := 10-::Parent:VertScrollPos
   ENDIF

   MoveWindow( ::hWnd, x, y, w, h, lRep )
RETURN Self
//---------------------------------------------------------------------------------------------

METHOD MessageWait( cText, cTitle, lProgress, cCancel, lMarquee, nMaxRange ) CLASS Window
   LOCAL oWnd
   DEFAULT lProgress TO .F.
   oWnd := MessageWait( cText, cTitle, lProgress, cCancel, lMarquee, ::hWnd, nMaxRange )
RETURN oWnd

//----------------------------------------------------------------------------------------------------

METHOD OnVScroll( nSBCode, nPos ) CLASS Window

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

   IF ::siv == NIL
      ::siv := (struct SCROLLINFO)
      ::siv:cbSize := ::siv:sizeof()
      ::siv:nMin   := 0
   ENDIF

   ::siv:nPos   := ::VertScrollPos
   ::siv:fMask  := SIF_POS

   SetScrollInfo( ::hWnd, SB_VERT, ::siv, .T. )
   _ScrollWindow( ::hWnd, 0, -nDelta )

   IF ::Active
      ::InvalidateRect()
   ENDIF

RETURN 0

//----------------------------------------------------------------------------------------------------
METHOD UpdateScrollArea( nWidth, nHeight ) CLASS Window
   LOCAL x, y, oSelf := IIF( ::__oDlg != NIL, ::__oDlg, Self )
   IF oSelf:HorzScroll
      WITH OBJECT oSelf
         IF nWidth != NIL
            x := nWidth
         ENDIF
         x := 0
         y := 0
         AEVAL( ::Children, {|o| IIF( GetParent(o:hWnd)==oSelf:hWnd, ( x := Max(x,o:Left+o:Width), y := Max(y,o:Top+o:Height) ),) } )
         :OriginalRect[3] := x
         :HorzScrollPos := 0
      END
   ENDIF
   IF oSelf:VertScroll
      WITH OBJECT oSelf
         IF nHeight != NIL
            y := nHeight
         ENDIF
         IF y == NIL
            y := 0
            AEVAL( ::Children, {|o| IIF( GetParent(o:hWnd)==oSelf:hWnd, y := Max(y,o:Top+o:Height),) } )
         ENDIF
         :OriginalRect[4] := y
         :VertScrollPos := 0
      END
   ENDIF
   IF oSelf:VertScroll
      oSelf:__SetScrollBars()
   ENDIF
   IF oSelf:HorzScroll
      oSelf:__SetScrollBars()
   ENDIF
   IF ::__oDlg != NIL
      ::__oDlg:RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------
METHOD OnHScroll( nSBCode, nPos ) CLASS Window

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
   LOCAL o, rc, oCtrl, Control

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
         ::Style := (::Style & NOT( WS_POPUP ))
         ::Style := (::Style & NOT( WS_CHILD ))
         ::Style := (::Style | WS_OVERLAPPED)
         EXIT
      CASE 2
         ::Style := (::Style & NOT( WS_OVERLAPPED ))
         ::Style := (::Style & NOT( WS_CHILD ))
         ::Style := (::Style | WS_POPUP)
         EXIT
      CASE 3
         ::Style := (::Style & NOT( WS_OVERLAPPED ))
         ::Style := (::Style & NOT( WS_POPUP ))
         ::Style := (::Style | WS_CHILD)
         EXIT
   END
   IF ::hWnd != NIL .AND. ! ::DesignMode
      SetWindowLong( ::hWnd, GWL_STYLE, ::Style )
      SetWindowPos( ::hWnd,, 0, 0, 0, 0, (SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER) )
      ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
   ENDIF
RETURN Self

METHOD __SetWindowCursor( nCursor ) CLASS Window
   IF ! ::DesignMode
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

CLASS __WindowDock
   PROPERTY Left     SET ::SetDock( 1,v )
   PROPERTY Top      SET ::SetDock( 2,v )
   PROPERTY Right    SET ::SetDock( 3,v )
   PROPERTY Bottom   SET ::SetDock( 4,v )
   PROPERTY Margins  SET ::SetMargins(v)

   PROPERTY RightProportional DEFAULT .F.

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
   DATA ClsName      EXPORTED INIT "Dock"

   DATA __nWidthPerc EXPORTED

   DATA xMargin      PROTECTED INIT 2
   ACCESS Margin     INLINE ::xMargin
   ASSIGN Margin(n)  INLINE ::Margins := alltrim( str( n ) ) + "," + alltrim( str( n ) ) + "," + alltrim( str( n ) ) + "," + alltrim( str( n ) )

   ACCESS Parent          INLINE ::Owner
   ACCESS Form            INLINE ::Owner:Form
   ACCESS Application     INLINE __GetApplication()
   ACCESS DesignMode      INLINE ::Owner:DesignMode

   METHOD Init() CONSTRUCTOR
   METHOD SetDock()
   METHOD Update()
   METHOD SetMargins()
   METHOD Destroy()
   METHOD IsDocked() INLINE ::Left != NIL .OR. ::Top != NIL .OR. ::Right != NIL .OR. ::Bottom != NIL
ENDCLASS

METHOD Init( oOwner ) CLASS __WindowDock
   ::Owner := oOwner
   __SetInitialValues( Self )
RETURN Self

METHOD Destroy() CLASS __WindowDock
   ::xLeft   := NIL
   ::xTop    := NIL
   ::xRight  := NIL
   ::xBottom := NIL
   ::Owner   := NIL
RETURN NIL

METHOD SetMargins( cMargins ) CLASS __WindowDock
   LOCAL n, aMargins := hb_atokens( cMargins, "," )
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

   IF ::DesignMode .AND. ::Application:ObjectManager != NIL
      ::Application:ObjectManager:PostMessage( WM_USER + 4767 )
   ENDIF
RETURN Self

METHOD Update() CLASS __WindowDock
   IF ::Owner:IsWindowVisible()
      ::Owner:DockIt()
      IF ::DesignMode
         PostMessage( ::Owner:Parent:hWnd, WM_SIZE, 0, MAKELPARAM( ::Owner:Parent:ClientWidth, ::Owner:Parent:ClientHeight ) )
         ::Form:InvalidateRect()
         ::Form:CtrlMask:InvalidateRect()
      ENDIF
   ENDIF
RETURN Self

METHOD SetDock( x ) CLASS __WindowDock
   IF x != NIL
      ::Type += x
   ENDIF
   ::Owner:RegisterDocking()
   ::Update()
RETURN Self

CLASS __AnchorSet
   PROPERTY Left         SET ::SetAnchor( 1, v ) DEFAULT .F.
   PROPERTY Top          SET ::SetAnchor( 2, v ) DEFAULT .F.
   PROPERTY Right        SET ::SetAnchor( 3, v ) DEFAULT .F.
   PROPERTY Bottom       SET ::SetAnchor( 4, v ) DEFAULT .F.
   PROPERTY Center       SET ::SetAnchor( 4, v ) DEFAULT .F.

   DATA ProportionalLeft  EXPORTED INIT .F.
   DATA ProportionalTop   EXPORTED INIT .F.

   DATA xProportionalLeft PROTECTED INIT .F.
   DATA xProportionalTop  PROTECTED INIT .F.
   DATA Owner             EXPORTED
   DATA ClsName           EXPORTED INIT "Anchor"
   ACCESS Parent          INLINE ::Owner
   ACCESS Form            INLINE ::Owner:Form

   ACCESS Application     INLINE __GetApplication()
   ACCESS DesignMode      INLINE ::Owner:DesignMode

   METHOD Init() CONSTRUCTOR
   METHOD CenterWindow(l) INLINE IIF( l, ::Owner:CenterWindow(), ), Self
   METHOD SetAnchor()
   METHOD Destroy()
ENDCLASS

METHOD Init( oOwner ) CLASS __AnchorSet
   ::Owner := oOwner
   __SetInitialValues( Self )
RETURN Self

METHOD Destroy() CLASS __AnchorSet
   ::xLeft   := NIL
   ::xTop    := NIL
   ::xRight  := NIL
   ::xBottom := NIL
   ::Owner   := NIL
RETURN NIL

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
   IF ::Application != NIL .AND. ::DesignMode .AND. lSet
      WITH OBJECT ::Application:ObjectManager
         IF :ActiveObject == ::Owner
            IF ( oItem := FindTreeItem( :Items, TVGetSelected( :hWnd ) ) ) != NIL
               FOR EACH Item IN oItem:Owner:Items
                   IF ( n := ASCAN( aValue, Item:Caption ) ) > 0
                      cVar := Item:Caption
                      Item:ColItems[1]:Value := __objSendMsg( :ActiveObject:Anchor, cVar )
                   ENDIF
               NEXT
               :InvalidateRect(, .F. )
            ENDIF
         ENDIF
      END
   ENDIF
   IF lSet
      IF ASCAN( ::Owner:Parent:__aDock, {|o| o:hWnd == ::Owner:hWnd} ) == 0
         AADD( ::Owner:Parent:__aDock, ::Owner )
      ENDIF
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
   LOCAL ii, aSize
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

FUNCTION __Draw3DRect( hdc, aRect, nColor, nShadow, lPen )
   LOCAL hPen
   LOCAL hPenOld
   DEFAULT lPen TO .F.
   IF !lPen
      hPen := CreatePen( PS_SOLID, 1, nColor )
    ELSE
      hPen := nColor
   ENDIF
   hPenOld := SelectObject( hdc, hPen )
   MoveToEx( hdc, aRect[1], aRect[4] - 1 )
   LineTo( hdc, aRect[1], aRect[2] )
   LineTo( hdc, aRect[3] - 1, aRect[2] )
   SelectObject( hdc, hPenOld )
   IF !lPen
      DeleteObject( hPen )
   ENDIF
   IF aRect[1] <> aRect[3]
      IF !lPen
         hPen := CreatePen(PS_SOLID, 1, nShadow )
       ELSE
         hPen := nShadow
      ENDIF
      hPenOld := SelectObject(hdc, hPen)
      LineTo( hdc, aRect[3] - 1, aRect[4] - 1 )
      LineTo( hdc, aRect[1], aRect[4] - 1 )
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

FUNCTION isChildOfActiveWindow(hWnd)
   LOCAL hNowActive := GetActiveWindow()
   LOCAL lRet       := isChild( hNowActive, hWnd )
   LOCAL hParent
   DO WHILE lRet
      hParent := GetParent( hWnd )
      IF hNowActive != hParent
         IF (GetWindowLong( hParent, GWL_STYLE ) & WS_CHILD) != 0
            hWnd:=hParent
          ELSE
            lRet:=.F.
         ENDIF
       ELSE
         EXIT
      ENDIF
   ENDDO
RETURN lRet

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

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS WinForm INHERIT Window
   DATA Sizeable               EXPORTED INIT .T.  // to avoid crash loading old projects

   DATA Params                 EXPORTED
   DATA ControlId              EXPORTED  INIT 101
   DATA TabOrder               EXPORTED
   DATA DllInstance            EXPORTED

   DATA EnumShowMode           EXPORTED  INIT { { "Normal", "Minimized", "Maximized", "NoActivate" }, {1,2,3,4} }
   DATA EnumFrameStyle         EXPORTED  INIT { {"Overlapped", "PopUp"}, {1,2} }

   ACCESS IsMDIContainter  INLINE ::MDIContainter
   DATA __lResizeable            EXPORTED  INIT {.F.,.F.,.F.,.T.,.T.,.T.,.F.,.F.}
   DATA __lMoveable              EXPORTED  INIT .F.

   PROPERTY GenerateMembers      DEFAULT .T.
   PROPERTY Modal                DEFAULT .F.
   PROPERTY UserVariables        DEFAULT ""
   PROPERTY ShowMode             DEFAULT 1
   PROPERTY AutoClose            DEFAULT .F.
   PROPERTY VertScroll           DEFAULT .F.
   PROPERTY HorzScroll           DEFAULT .F.
   PROPERTY MDIClient
   PROPERTY MinWidth             DEFAULT 0
   PROPERTY MinHeight            DEFAULT 0
   PROPERTY MaxWidth             DEFAULT 0
   PROPERTY MaxHeight            DEFAULT 0
   PROPERTY ShowInTaskBar        DEFAULT .T.
   PROPERTY AnimationStyle       DEFAULT 0
   PROPERTY ScrollOnChildFocus   DEFAULT .F.
   PROPERTY BackgroundImage
   PROPERTY ToolWindow           SET ::SetExStyle( WS_EX_TOOLWINDOW, v )   DEFAULT .F.
   PROPERTY AlwaysOnTop          SET ::SetExStyle( WS_EX_TOPMOST, v )      DEFAULT .F.
   PROPERTY ThickFrame           SET ::SetStyle( WS_THICKFRAME, v )        DEFAULT .T.
   PROPERTY Resizable            SET ::SetStyle( WS_THICKFRAME, v )        DEFAULT .T.
   PROPERTY MaximizeBox          SET ::SetStyle( WS_MAXIMIZEBOX, v )       DEFAULT .T.
   PROPERTY MinimizeBox          SET ::SetStyle( WS_MINIMIZEBOX, v )       DEFAULT .T.
   PROPERTY CaptionBar           SET ::SetStyle( WS_CAPTION, v )           DEFAULT .T.
   PROPERTY SysMenu              SET ::SetStyle( WS_SYSMENU, v )           DEFAULT .T.
   PROPERTY FrameStyle           SET ::__SetFrameStyle(v)                  DEFAULT 1
   PROPERTY DlgModalFrame        SET ::SetExStyle( WS_EX_DLGMODALFRAME, v) DEFAULT .F.
   PROPERTY Icon                 SET ::SetFormIcon(v)
   PROPERTY Opacity              SET ::SetOpacity(v)                       DEFAULT 100
   PROPERTY BitmapMask           SET ::__SetBitmapMask(v)
   PROPERTY BitmapMaskColor      SET ::__SetBitmapMaskColor(v)
   PROPERTY ImageList            GET __ChkComponent( Self, @::xImageList )     SET ::SetImageList(v)
   PROPERTY ActiveMenuBar        GET __ChkComponent( Self, @::xActiveMenuBar ) SET ::__SetActiveMenuBar(v)

   PROPERTY MDIChild             GET IIF( ::ClsName == "MDIChild", (::ExStyle & WS_EX_MDICHILD) != 0, ::xMDIChild );
                                 SET ( IIF( ::DesignMode .AND. v .AND. ::Modal, ::Modal := .F., ), ::xMDIChild := v, IIF( ::ClsName == "MDIChild", ::SetExStyle( WS_EX_MDICHILD, v ), )) DEFAULT .F.
   PROPERTY MdiContainer         SET (::xMdiContainer := v, ::IsContainer := .F., ::__CreateMDI(v)) DEFAULT .F.
   PROPERTY Center               SET ::CenterWindow(v)                     DEFAULT .F.
   PROPERTY VertScrollTopMargin  DEFAULT 0

   PROPERTY HelpBox              ROOT "Help" SET ::SetExStyle( WS_EX_CONTEXTHELP, v )       DEFAULT .F.

   //compatibility ONLY, forms do not set "Border" property
   ACCESS TopMost              INLINE ::AlwaysOnTop
   ASSIGN TopMost(l)           INLINE ::AlwaysOnTop := l

   DATA Border                 EXPORTED INIT 0

   //DATA Visible                EXPORTED  INIT .T.
   DATA MaskBitmap             EXPORTED
   DATA __hObjects             EXPORTED
   DATA Ole                    EXPORTED
   DATA AppParam               EXPORTED

   DATA __lLoading             EXPORTED INIT .F.
   DATA __aPostCreateProc      EXPORTED INIT {}
   DATA OnWMClose              EXPORTED

   DATA bChanged               EXPORTED

   ACCESS Form                 INLINE Self

   METHOD Init() CONSTRUCTOR

   METHOD Create()

   // MDI Messages
   METHOD MdiTileHorizontal( lTileDisable ) INLINE lTileDisable := IFNIL(lTileDisable,.F.,lTileDisable),;
                                                   SendMessage( ::MDIClient:hWnd, WM_MDITILE, MDITILE_HORIZONTAL + IIF( !lTileDisable, MDITILE_SKIPDISABLED, 0 ) )
   METHOD MdiTileVertical( lTileDisable )   INLINE lTileDisable := IFNIL(lTileDisable,.F.,lTileDisable),;
                                                   SendMessage( ::MDIClient:hWnd, WM_MDITILE, MDITILE_VERTICAL + IIF( !lTileDisable, MDITILE_SKIPDISABLED, 0 ) )
   METHOD MdiCascade( lTileDisable )        INLINE lTileDisable := IFNIL(lTileDisable,.F.,lTileDisable),;
                                                   SendMessage( ::MDIClient:hWnd, WM_MDICASCADE, IIF( !lTileDisable, MDITILE_SKIPDISABLED, 0 ) )

   METHOD MdiIconArrange()                  INLINE SendMessage( ::MDIClient:hWnd, WM_MDIICONARRANGE )
   METHOD MdiNext()                         INLINE SendMessage( ::MDIClient:hWnd, WM_MDINEXT )

   METHOD MdiActivate()
   METHOD MdiDestroy()
   METHOD MdiClose()
   METHOD MdiGetActive()

   METHOD MdiMaximize()
   METHOD MdiRestore()
   METHOD SetFormIcon()

   METHOD Maximize()                        INLINE ShowWindow( ::hWnd, SW_SHOWMAXIMIZED)
   METHOD Minimize()                        INLINE ShowWindow( ::hWnd, SW_SHOWMINIMIZED)
   METHOD Restore()                         INLINE ShowWindow( ::hWnd, SW_RESTORE)
   METHOD SetOpacity()
   METHOD Show()

   METHOD SaveLayout()
   METHOD RestoreLayout()
   METHOD OnSize()

   ACCESS ActiveForm                        INLINE ObjFromHandle( GetActiveWindow() )

   METHOD __RefreshPosNo()                  INLINE NIL // Compatibility
   METHOD __SetBitmapMask()
   METHOD __SetBitmapMaskColor()
   METHOD __PaintBakgndImage()
   METHOD __PrcMdiMenu()
   METHOD __SetActiveMenuBar()
   METHOD __CreateBkBrush()

   METHOD SetImageList()
   METHOD SetBackColor()
   METHOD CenterWindow()

   METHOD OnNCDestroy()
   METHOD __OnClose()
   METHOD OnSysCommand()
   METHOD SetInstance()
   METHOD Redraw( aRect )                   INLINE ::RedrawWindow( , aRect, (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN) ),::UpdateWindow()
   METHOD RegisterHotKey( nId, nMod, nKey ) INLINE IIF( RegisterHotKey( ::hWnd, nId, nMod, nKey ), AADD( ::__aHotKey, { nId, nMod, nKey } ),)
   METHOD InvalidateRect(a,l)               INLINE Super:InvalidateRect(a,l), IIF( ::xMDIContainer .AND. ::MDIClient != NIL, ::MDIClient:InvalidateRect(), )
   METHOD __CreateProperty()
   METHOD RegisterDocking()                 INLINE NIL
   METHOD Hide()
   METHOD SetForeground()                   INLINE SetForegroundWindow( ::hWnd )
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent, aParameters, cProjectName ) CLASS WinForm
   LOCAL hInst, hPointer, n
   ::SetChildren := .F.
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
   ::__hObjects := Hash()
   HSetCaseMatch( ::__hObjects, .F. )

   Super:Init( oParent )

   IF ! ::DesignMode
      n := ::GetControlName( ::__xCtrlName )
      ::Application:__SetAsProperty( ::__xCtrlName + ALLTRIM( STR( n ) ), Self )
   ENDIF

   IF ! (Upper(::ClsName) IN {"MDICHILD","MDICLIENT"})
      ::MDIClient := MDIClient( Self )
   ENDIF
   ::__lCopyCut := .T.
   ::__IsForm   := .T.

   __DeleteEvents( ::Events,{ "OnClick" } )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __CreateProperty( cBaseName ) CLASS WinForm
   LOCAL n
   DEFAULT cBaseName TO ::__xCtrlName
   IF EMPTY( ::xName ) .AND. ::GenerateMember
      n := ::GetControlName( cBaseName )
      ::Application:__SetAsProperty( cBaseName + ALLTRIM( STR( n ) ), Self )
   ENDIF
RETURN SELF

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
   LOCAL lVertScroll, lHorzScroll
   IF ::__hParent != NIL
      hoParent := ::__hParent
   ENDIF

   IF ::DesignMode
      IF VALTYPE( ::xIcon ) == "A"
         ::xIcon := ::xIcon[1]
      ENDIF
      IF ::MDIChild
         ::__aExcludeProperties := { "SHOWINTASKBAR", "MODAL", "SHOWMODE", "TOOLWINDOW", "CONTROLPARENT", "ANIMATIONSTYLE", "ALWAYSONTOP", "ACTIVEMENUBAR", "AUTOCLOSE", "BITMAPMASK", "BITMAPMASKCOLOR", "DLGMODALFRAME", "FRAMESTYLE" }
      ENDIF
   ENDIF

   IF !::ShowInTaskBar .AND. ::Parent == NIL .AND. ! ::DesignMode .AND. ::Application:DllInstance == NIL
      ::__TaskBarParent := CreateDialogIndirect( ::AppInstance, __GetTemplate( Self ), 0, NIL )
      IF !EMPTY( ::__hIcon )
         SendMessage( ::__TaskBarParent, WM_SETICON, ICON_BIG, ::__hIcon )
      ENDIF
      hoParent := ::__TaskBarParent
   ENDIF

   lVertScroll := ::VertScroll
   lHorzScroll := ::HorzScroll

   IF ! ::DesignMode
      IF ::VertScrollTopMargin > 0
         ::VertScroll := .F.
         ::HorzScroll := .F.
      ENDIF
   ENDIF

   Super:Create( hoParent )
   IF ::__OnInitCanceled
      IF VALTYPE( ::Font ) == "O" .AND. ! ::Font:Shared
         ::Font:Delete()
      ENDIF
      ::Font := NIL
      RETURN NIL
   ENDIF

   ::SetIcon( ICON_SMALL, IIF( !EMPTY( ::__hIcon ), ::__hIcon, LoadIcon( NIL, IDI_WINLOGO ) ) )
   ::SetIcon( ICON_BIG, IIF( !EMPTY( ::__hIcon ), ::__hIcon, LoadIcon( NIL, IDI_WINLOGO ) ) )
   ::SetOpacity( ::xOpacity )

   IF ::BackgroundImage != NIL
      ::BackgroundImage:Create()
   ENDIF
   AEVAL( ::__aPostCreateProc, {|a| hb_ExecFromArray( a[1], a[2] )} )
   IF ::ActiveMenuBar != NIL
      ::__SetActiveMenuBar( ::ActiveMenuBar )
   ENDIF
   IF ::Center .AND. ! ::DesignMode
      ::CenterWindow()
   ENDIF

   IF ! ::DesignMode
      IF ::VertScrollTopMargin > 0
         ::__oDlg := Dialog( Self )
         WITH OBJECT ::__oDlg
            :Style       := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
            :ExStyle     := WS_EX_CONTROLPARENT

            :Top         := ::VertScrollTopMargin

            :VertScroll  := lVertScroll
            :HorzScroll  := lHorzScroll

            :SetChildren := .F.
            :Modal       := .F.
            :BackColor   := ::BackColor
            :Create()
            :OriginalRect[4] := 0
         END
      ENDIF
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------
METHOD Hide() CLASS WinForm
   LOCAL nAnimation
   IF ::hWnd != NIL
      IF ::AnimationStyle != NIL .AND. ::AnimationStyle <> 0 .AND. ! ::DesignMode
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
         RETURN ::Animate( 1000, (AW_HIDE | nAnimation) )
      ENDIF
      ShowWindow( ::hWnd, SW_HIDE )
   ENDIF
   ::Style := (::Style & NOT( WS_VISIBLE ))
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __OnClose( nwParam, nlParam ) CLASS WinForm
   LOCAL nRet, nAnimation
   IF ! ::Modal
      nRet := ExecuteEvent( "OnClose", Self )
      ODEFAULT nRet TO ::OnClose( nwParam )
      ODEFAULT nRet TO __Evaluate( ::OnWMClose, Self, nwParam, nlParam )
   ENDIF
   IF nRet == NIL .AND. ::AnimationStyle != 0 .AND. ! ::DesignMode
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
      ::Animate( 1000, (AW_HIDE | nAnimation) )
   ENDIF
RETURN nRet

//-----------------------------------------------------------------------------------------------
METHOD CenterWindow( lDesk ) CLASS WinForm
   LOCAL oWin, aRect
   DEFAULT lDesk TO .F.
   IF ::hWnd == NIL .OR. ::DesignMode
      RETURN Self
   ENDIF
   IF ::Parent != NIL .AND. !lDesk
      ::GetWindowRect()

      IF ::IsChild() .OR. ::xMDIChild
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
METHOD OnNCDestroy() CLASS WinForm
   ::Super:OnNCDestroy()
   hb_gcAll( .t. )
   IF ::xAnimation != NIL .AND. ::xAnimation:Owner != NIL
      ::xAnimation:Owner := NIL
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD __CreateBkBrush( hDC ) CLASS WinForm
   LOCAL lDC
   IF ::BackgroundImage != NIL .AND. ::BackgroundImage:hDIB != NIL
      lDC := ( hDC == NIL )
      DEFAULT hDC TO GetDC( ::hWnd )
      ::__PaintBakgndImage( hDC )
      IF lDC
         ReleaseDC( ::hWnd, hDC )
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD __SetActiveMenuBar( oMenu ) CLASS WinForm
   IF ::hWnd != NIL .AND. VALTYPE( oMenu ) != "C"
      SetMenu( ::hWnd, IIF( oMenu != NIL, oMenu:hMenu, NIL ) )
      IF oMenu != NIL
         oMenu:SetBackColor()
         DrawMenuBar( ::hWnd )
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnSize( nwParam, nlParam ) CLASS WinForm

   ::ClientWidth  := LOWORD(nlParam)
   ::ClientHeight := HIWORD(nlParam)

   IF ::__oDlg != NIL
      ::__oDlg:MoveWindow( 0, ::VertScrollTopMargin, ::ClientWidth, ::ClientHeight - ::VertScrollTopMargin - IIF( ::StatusBar != NIL, ::StatusBar:Height, 0 ), .T. )
      ::__oDlg:RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
   ENDIF

   Super:OnSize( nwParam, nlParam )
   IF (nwParam == 0 .or. nwParam == 2) .AND. ::MDIContainer .AND. ::MDIClient != NIL
      ::MDIClient:MoveWindow()
   ENDIF

   IF ::IsWindowVisible() .AND. ::BackgroundImage != NIL .AND. !EMPTY( ::BackgroundImage:ImageName )
      ::CallWindowProc()
      ::__CreateBkBrush()
      ::InvalidateRect()
      ::Redraw()
      RETURN 0
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnSysCommand( nwParam ) CLASS WinForm
   LOCAL oChild, hDef
   IF (nwParam IN {SC_MAXIMIZE,SC_MAXIMIZE2,SC_RESTORE,SC_RESTORE2}) .AND. ! EMPTY( ::__aDock )
      ::CallWindowProc()
      hDef := BeginDeferWindowPos( LEN( ::__aDock ) )
      FOR EACH oChild IN ::__aDock
          IF oChild != NIL //.AND. oChild:hWnd != ::hWnd
             oChild:__OnParentSize( ::ClientWidth, ::ClientHeight, @hDef )
             oChild:InvalidateRect(, .F. )
          ENDIF
      NEXT
      EndDeferWindowPos( hDef )
      RETURN 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD SetBackColor( nColor, lRepaint ) CLASS WinForm
   ::Super:SetBackColor( nColor, lRepaint )
   IF ::BackgroundImage != NIL .AND. ::BackgroundImage:hDIB != NIL
      ::BackgroundImage:__SetImageName( @::BackgroundImage:xImageName )
   ENDIF
   IF ::xMDIContainer .AND. ::MDIClient != NIL
      ::MDIClient:InvalidateRect()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SaveLayout( cIniFile, cSection, lAllowOut, lAllowMinimized ) CLASS WinForm
   LOCAL nShow, oIni, rc := (struct RECT)
   DEFAULT lAllowOut TO .T.
   DEFAULT lAllowMinimized TO .T.
   nShow := ::__GetShowMode()
   IF ! IsIconic( ::hWnd )
      GetWindowRect( ::hWnd, @rc )
    ELSE
      rc:left   := ::__aMinRect[1]
      rc:top    := ::__aMinRect[2]
      rc:right  := ::__aMinRect[3]+::__aMinRect[1]
      rc:bottom := ::__aMinRect[4]+::__aMinRect[2]

      // Main Form cannot start minimized can it?
      IF nShow == 2 .AND. ! lAllowMinimized
         nShow := 1
      ENDIF
   ENDIF
   IF EMPTY( cIniFile )
      oIni := ::Application:IniFile
    ELSE
      oIni := IniFile( cIniFile )
   ENDIF
   DEFAULT cSection TO "Main"
   IF ! lAllowOut
      rc:left := MAX( 0, rc:left )
      rc:top  := MAX( 0, rc:top )
   ENDIF
   oIni:WriteString( cSection, ::Application:Name + "_" + ::Name, xSTR( rc:Left )  + ", " +;
                                                                  xSTR( rc:Top )   + ", " +;
                                                                  xSTR( rc:right-rc:Left )  + ", " +;
                                                                  xSTR( rc:bottom-rc:Top )  + ", " +;
                                                                  xSTR( nShow ) )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD RestoreLayout( cIniFile, cSection, lAllowOut, lAllowMinimized ) CLASS WinForm
   LOCAL c, oIni, aRect, aPos
   DEFAULT lAllowOut TO .T.
   DEFAULT lAllowMinimized TO .T.
   IF EMPTY( cIniFile )
      oIni := ::Application:IniFile
    ELSE
      oIni := IniFile( cIniFile )
   ENDIF
   DEFAULT cSection TO "Main"
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
         IF aPos[5] == 2 .AND. ! lAllowMinimized
            aPos[5] := 1
         ENDIF
         ::ShowMode := aPos[5]
         IF aPos[5] == 3
            RETURN Self
         ENDIF
      endif
      ::xLeft   := aPos[1]
      ::xTop    := aPos[2]
      IF ::Resizable
         ::xWidth  := aPos[3]
         ::xHeight := aPos[4]
      ENDIF
      IF ::xWidth <= 0 .OR. ::xHeight <= 0
         ::ShowMode := 3
         RETURN Self
      ENDIF

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
METHOD __PaintBakgndImage( hDC, lBitBlt ) CLASS WinForm
   LOCAL hMemBitmap, hOldBitmap, hMemDC, hBrush

   DEFAULT hDC TO ::Drawing:hDC
   DEFAULT lBitBlt TO .F.

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   IF ::xBackColor != NIL
      hBrush := CreateSolidBrush( ::xBackColor )
   ENDIF
   _FillRect( hMemDC, { 0, 0, ::ClientWidth, ::ClientHeight }, IIF( hBrush != NIL, hBrush, GetSysColorBrush(COLOR_BTNFACE) ) )

   IF ::BackgroundImage != NIL
      ::BackgroundImage:Draw( hMemDC )
   ENDIF

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
   ENDIF

   ::BkBrush   := CreatePatternBrush( hMemBitmap )

   //IF lBitBlt
   //   BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )
   //ENDIF

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )
   IF hBrush != NIL
      DeleteObject( hBrush )
   ENDIF
RETURN NIL

METHOD Show( nShow ) CLASS WinForm
   LOCAL nRet, hDC, hMemDC, hOldBitmap, hMemBitmap, hBrush
   DEFAULT nShow TO ::ShowMode
   IF ::__OnInitCanceled
      IF VALTYPE( ::Font ) == "O" .AND. ! ::Font:Shared
         ::Font:Delete()
      ENDIF
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

            _FillRect( hMemDC, { 0, 0, ::ClientWidth, ::ClientHeight }, hBrush )

            BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )

            //::__SetTransparentChildren( hDC, hMemDC )

            SelectObject( hMemDC,  hOldBitmap )
            DeleteObject( hMemBitmap )
            DeleteDC( hMemDC )
         ENDIF
         //::Application:DoEvents()

         ::__FixDocking()
         ::__SetScrollBars()

         nRet := ExecuteEvent( "OnLoad", Self )

         ODEFAULT nRet TO ::OnLoad( Self )
         nShow := ::ShowMode
      ENDIF
      IF ::AnimationStyle != 0 .AND. ! ::DesignMode
         ::SendMessage( WM_SIZE, 0, MAKELONG( ::ClientWidth, ::ClientHeight ) )
         ::UpdateChildren()
         ::Animate( 1000, ::AnimationStyle )
         ::UpdateChildren()
       ELSEIF ::Visible
         ShowWindow( ::hWnd, IIF( ! ::DesignMode, nShow, SW_SHOW ) )
         ::UpdateWindow()

         IF ::MDIContainer
            ::PostMessage( WM_SIZE, 0, MAKELPARAM( ::ClientWidth, ::ClientHeight ) )
         ENDIF
      ENDIF
   ENDIF
   ::Style := (::Style | WS_VISIBLE)
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
      cIcon := IIF( ::DesignMode .AND. VALTYPE( cIcon[1] ) == "C", cIcon[1], cIcon[2] )
   ENDIF
   IF !EMPTY( ::__hIcon )
      DestroyIcon( ::__hIcon )
      ::__hIcon := NIL
      IF ::DesignMode
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

   IF ::DesignMode
      IF !EMPTY( cIcon )
         ::Application:Project:AddImage( cIcon, IMAGE_ICON, Self, .T., ::hWnd == ::Application:Project:Forms[1]:hWnd )
      ENDIF
   ENDIF
RETURN Self

METHOD __SetBitmapMaskColor( nColor ) CLASS WinForm
   LOCAL cBmp, hBitmap
   IF ! ::DesignMode .AND. ::hWnd != NIL
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
         hBitmap := LoadImage( ::Instance, cBmp, IMAGE_BITMAP, 0, 0, (LR_LOADFROMFILE | LR_VGACOLOR) )
       ELSE
         hBitmap := LoadBitmap( ::Instance, cBmp )
      ENDIF

      ::BkBrush   := CreatePatternBrush( hBitmap )

      ::__hRegion := BitmapToRegion( hBitmap, nColor )
      SetWindowRgn( ::hWnd, ::__hRegion, .T. )

      DeleteObject( hBitmap )
      InvalidateRgn( ::hWnd, ::__hRegion, .T. )

      ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER))
      ::RedrawWindow( , , ( RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW ) )
      InvalidateRgn( ::hWnd, ::__hRegion, .T. )
   ENDIF
RETURN Self

METHOD __SetBitmapMask( cBmp ) CLASS WinForm
   LOCAL aSize, hBitmap
   IF ::DesignMode
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
            hBitmap := LoadImage( ::Instance, cBmp, IMAGE_BITMAP, 0, 0, (LR_LOADFROMFILE | LR_VGACOLOR ))
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

         ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER))
         ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW ))
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
         nStyle := (nStyle | WS_EX_LAYERED)
       ELSE
         nStyle := (nStyle & NOT( WS_EX_LAYERED ))
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
                oChild:SetWindowPos(, 0, 0, 0, 0, (SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER) )
              catch
             END
             __BrowseChildren( oChild )
          ENDIF
      NEXT
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

FUNCTION xStr( xValue, xType )
   LOCAL cType := VALTYPE( xValue )
   SWITCH cType
      CASE "C"
           IF xType != NIL
              SWITCH xType
                 CASE "N"
                      xValue := VAL( xValue )
                      EXIT
                 CASE "D"
                      xValue := CTOD( xValue )
                      EXIT
                 CASE "L"
                 CASE "B"
                      xValue := &xValue
                      EXIT
                 CASE "U"
                      xValue := NIL
                      EXIT
              END
           ENDIF
           EXIT
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
   LOCAL cKey, cVal, nErr, hKey, hSubKey, cIcon
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
   PROPERTY Type          DEFAULT 0
   PROPERTY Speed         DEFAULT 1000
   PROPERTY HideInvert    DEFAULT .T.

   DATA Owner        EXPORTED
   DATA ClsName      EXPORTED INIT "__Animation"

   ACCESS Parent          INLINE ::Owner
   ACCESS Form            INLINE ::Owner:Form
   ACCESS Application     INLINE __GetApplication()

   ACCESS DesignMode      INLINE ::Owner:DesignMode
   METHOD Init() CONSTRUCTOR
ENDCLASS

METHOD Init( oOwner ) CLASS __Animation
   ::Owner := oOwner
   __SetInitialValues( Self )
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

FUNCTION __SetInitialValues( oObj, cProp, xValue, lReset )
   LOCAL aProperties, aProperty
   IF oObj:DesignMode
      IF cProp != NIL
         IF __objHasMsg( oObj, "__a_"+cProp )
             __objSendMsg( oObj, "__a_"+cProp )[4] := IIF( xValue != NIL, xValue, __objSendMsg( oObj, cProp ) )
         ENDIF
      ELSE
         DEFAULT lReset TO .F.
         aProperties := __clsGetPropertiesAndValues( oObj )
         FOR EACH aProperty IN aProperties
             IF __objHasMsg( oObj, "__a_"+aProperty[1] ) //.AND. VALTYPE( aProperty[2] ) != "O"
                __objSendMsg( oObj, "__a_"+aProperty[1] )[4] := IIF( lReset, NIL, __objSendMsg( oObj, aProperty[1] ) )
             ENDIF
         NEXT
      ENDIF
   ENDIF
RETURN NIL

FUNCTION GetDesktopRect()
   LOCAL aDesktopRect := array(4)
   aDesktopRect[1] := GetSystemMetrics( SM_XVIRTUALSCREEN )
   aDesktopRect[2] := GetSystemMetrics( SM_YVIRTUALSCREEN )
   aDesktopRect[3] := aDesktopRect[1] + GetSystemMetrics( SM_CXVIRTUALSCREEN )
   aDesktopRect[4] := aDesktopRect[2] + GetSystemMetrics( SM_CYVIRTUALSCREEN )
RETURN aDesktopRect

FUNCTION __ChkComponent( oObj, cComp, lClear )
   LOCAL oForm, n
   DEFAULT lClear TO .T.
   IF VALTYPE( cComp ) == "C"
      oForm := oObj:Form

      IF oForm:__hObjects != NIL

         IF HGetPos( oForm:__hObjects, cComp ) > 0
            IF oForm:__hObjects[ cComp ] != NIL
               cComp := oForm:__hObjects[ cComp ]
            ENDIF
          ELSE
            IF ! oObj:DesignMode
               oForm := oObj:Application:MainForm
             ELSE
               oForm := oObj:Application:Project:Forms[1]
            ENDIF
            IF oForm:__hObjects != NIL .AND. HGetPos( oForm:__hObjects, cComp ) > 0
               IF oForm:__hObjects[ cComp ] != NIL
                  cComp := oForm:__hObjects[ cComp ]
               ENDIF
            ENDIF
         ENDIF

      ENDIF

      IF VALTYPE( cComp ) == "C"
         IF ( n := Ascan( oForm:Components, {|o| o:Name != NIL .AND. lower(o:Name) == lower(cComp) } ) ) > 0
            cComp := oForm:Components[n]
         ENDIF
      ENDIF
      IF VALTYPE( cComp ) == "C"
         IF ( n := Ascan( oObj:Parent:Children, {|o| o:Name != NIL .AND. lower(o:Name) == lower(cComp) } ) ) > 0
            cComp := oObj:Parent:Children[n]
         ENDIF
      ENDIF
      IF VALTYPE( cComp ) == "C"
         IF ( n := Ascan( oObj:Parent:Components, {|o| o:Name != NIL .AND. lower(o:Name) == lower(cComp) } ) ) > 0
            cComp := oObj:Parent:Children[n]
         ENDIF
      ENDIF

   ENDIF
   IF lClear .AND. VALTYPE( cComp ) != "O" .AND. ! oObj:DesignMode
      cComp := NIL
   ENDIF
RETURN cComp

FUNCTION TraceDbg( ... )
   LOCAL cStr, nLevel := SET( _SET_TRACESTACK ), xParam
   IF nLevel > 0
      cStr := '[ '  + ProcName( 1 ) + ' ] (' + Str( Procline(1), 5 ) + ')'
      hb_outDebug( cStr )
   ENDIF
   FOR EACH xParam IN HB_aParams()
      cStr :=  '>>>' + CStr( xParam ) + '<<<'
      hb_outDebug( cStr )
   NEXT
RETURN .T.

FUNCTION KeyCountRaw( xOrder, cBag )
RETURN dbOrderInfo( DBOI_KEYCOUNTRAW, cBag, xOrder )

FUNCTION aEvents()
RETURN { ;
                  {"Clipboard",   {;
                                  { "OnCut"              , "", "" },;
                                  { "OnCopy"             , "", "" },;
                                  { "OnPaste"            , "", "" },;
                                  { "OnChangeCbChain"    , "", "" },;
                                  { "OnDrawClipboard"    , "", "" } } },;
                  {"Command",     {;
                                  { "OnClick"            , "", "" },;
                                  { "OnCancel"           , "", "" },;
                                  { "OnClose"            , "", "" },;
                                  { "OnUndock"           , "", "" },;
                                  { "OnRedock"           , "", "" },;
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
                                  { "OnSize"             , "", "" },;
                                  { "OnSizing"           , "", "" },;
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

