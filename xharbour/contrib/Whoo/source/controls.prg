/*
 * $Id: controls.prg,v 1.7 2004/03/29 14:21:23 lculik Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib control classes
 *
 * Copyright 2002 Ron Pinkas [ron@ronpinkas.com]
 * Copyright 2002 Francesco Saverio Giudice [info@fsgiudice.com]
 * Copyright 2002 Augusto Infante [augusto@2vias.com.ar]
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */

#include "classex.ch"

#include "winuser.ch"
#include "commctrl.ch"
#include "what32.ch"
#include "HbClass.ch"

#include "wintypes.ch"
#Include "cstruct.ch"
#include "debug.ch"

#include "classex.ch"

GLOBAL EXTERNAL Application
#define CM_BASE                    45056
#define CM_TEXTCHANGED             CM_BASE + 18
CLASS TControl FROM TComponent

  PROPERTY Parent READ FParent WRITE SetParent

  DATA FWinControls  INIT {}

  DATA HintWindow     AS OBJECT

  DATA Action
  DATA ActionLink                 PROTECTED
  DATA Align
  DATA Anchors
  DATA AutoSize                   PROTECTED
  DATA BiDiMode
  DATA BoundsRect
  DATA Caption                    PROTECTED
  DATA ClientHeight
  DATA ClientOrigin
  DATA ClientRect                 PROTECTED
  DATA ClientWidth
  DATA Color                      PROTECTED
  DATA Constraints
  DATA ControlState
  DATA ControlStyle
  DATA Cursor
  DATA DesktopFont                PROTECTED
  DATA DockOrientation
  DATA DragCursor                 PROTECTED
  DATA DragKind                   PROTECTED
  DATA DragMode                   PROTECTED
  DATA Enabled
  DATA Floating                   READONLY
  DATA FloatingDockSiteClass
  DATA Font                       PROTECTED
  DATA HelpContext
  DATA HelpKeyword
  DATA HelpType
/*
  ACCESS Hint                     INLINE     IIF( ::HintWindow == NIL, "", ::HintWindow:Caption )
  ASSIGN Hint( x )                INLINE     IIF( ::HintWindow == NIL, ;
                                                  ::HintWindow := THintWindow():Create( hb_qself() ),;
                                                  NIL ),;
                                             ::HintWindow:Caption := x
*/
  DATA HostDockSite
  DATA IsControl                  PROTECTED

//--------------------------------------------------------------------------------------------

  PROPERTY Left   READ FLeft   WRITE SetLeft
  PROPERTY Top    READ FTop    WRITE SetTop
  PROPERTY Width  READ FWidth  WRITE SetWidth
  PROPERTY Height READ FHeight WRITE SetHeight
  PROPERTY Hint   READ FHint   WRITE SetHint

  METHOD SetLeft( n )   INLINE ::FLeft   := n, IIF( ::FHandle == NIL, , ::Move( , , , , .T. ) )
  METHOD SetTop( n )    INLINE ::FTop    := n, IIF( ::FHandle == NIL, , ::Move( , , , , .T. ) )
  METHOD SetWidth( n )  INLINE ::FWidth  := n, IIF( ::FHandle == NIL, , ::Move( , , , , .T. ) )
  METHOD SetHeight( n ) INLINE ::FHeight := n, IIF( ::FHandle == NIL, , ::Move( , , , , .T. ) )
  METHOD SetHint( c )   INLINE ::FHint   := c, IIF( ::HintWindow == NIL,;
                                                  ( ::HintWindow := THintWindow():Create( hb_qself() ),;
                                                    ::HintWindow:SetParent( hb_qself() ) ),;
                                                  NIL )
  METHOD SetParentComponent( oParent )
  METHOD SetParent( oParent )
  METHOD SetName( sName )

  DATA LRDockWidth
  DATA MouseCapture               PROTECTED

  DATA Name
  DATA WinClass                   PROTECTED
  DATA ControlName                PROTECTED

  DATA ParentBiDiMode             PROTECTED
  DATA ParentColor                PROTECTED
  DATA ParentFont                 PROTECTED
  DATA ParentShowHint             PROTECTED
  DATA PopupMenu                  PROTECTED
  DATA ScalingFlags               PROTECTED
  DATA ShowHint
  DATA TBDockHeight
  DATA Text           INIT " "    PROTECTED
  DATA UndockHeight
  DATA UndockWidth

  PROPERTY Visible READ FVisible WRITE SetVisible

  DATA WindowProc
  DATA WindowText                 PROTECTED

  METHOD ActionChange               VIRTUAL
  METHOD AdjustSize                 VIRTUAL
  METHOD AssignTo                   VIRTUAL
  METHOD BeginAutoDrag              VIRTUAL
  METHOD BeginDrag                  VIRTUAL
  METHOD BringToFront               VIRTUAL
  METHOD CanAutoSize                VIRTUAL
  METHOD CanResize                  VIRTUAL
  METHOD Changed                    VIRTUAL
  METHOD ChangeScale                VIRTUAL
  METHOD Click                      VIRTUAL
  METHOD ClientToParent             VIRTUAL
  METHOD ClientToScreen             VIRTUAL
  METHOD ConstrainedResize          VIRTUAL
  METHOD DblClick                   VIRTUAL
  METHOD DefaultDockImage           VIRTUAL
  METHOD DefaultHandler             VIRTUAL
  METHOD DefineProperties           VIRTUAL
  METHOD Destroy                    VIRTUAL
  METHOD Dock                       VIRTUAL
  METHOD DockTrackNoTarget          VIRTUAL
  METHOD DoContextPopup             VIRTUAL
  METHOD DoDock                     VIRTUAL
  METHOD DoEndDock                  VIRTUAL
  METHOD DoEndDrag                  VIRTUAL
  METHOD DoMouseWheel               VIRTUAL
  METHOD DoMouseWheelDown           VIRTUAL
  METHOD DoMouseWheelUp             VIRTUAL
  METHOD DoStartDock                VIRTUAL
  METHOD DoStartDrag                VIRTUAL
  METHOD DragCanceled               VIRTUAL
  METHOD DragDrop                   VIRTUAL
  METHOD Dragging                   VIRTUAL
  METHOD DragOver                   VIRTUAL
  METHOD DrawDragDockImage          VIRTUAL
  METHOD DrawTextBiDiModeFlags      VIRTUAL
  METHOD DrawTextBiDiModeFlagsReadingOnly VIRTUAL
  METHOD EndDrag                    VIRTUAL
  METHOD EraseDragDockImage         VIRTUAL
  METHOD GetActionLinkClass         VIRTUAL
  METHOD GetClientOrigin            VIRTUAL
  METHOD GetClientRect              VIRTUAL
  METHOD GetControlsAlignment       VIRTUAL
  METHOD GetDeviceContext           VIRTUAL
  METHOD GetDockEdge                VIRTUAL
  METHOD GetDragImages              VIRTUAL
  METHOD GetEnabled                 VIRTUAL
  METHOD GetFloating                VIRTUAL
  METHOD GetFloatingDockSiteClass   VIRTUAL
  METHOD GetPalette                 VIRTUAL
  METHOD GetParentComponent         VIRTUAL
  METHOD GetPopupMenu               VIRTUAL
  METHOD GetTextBuf                 VIRTUAL
  METHOD GetTextLen                 VIRTUAL
  METHOD HasParent                  VIRTUAL
  METHOD Hide                       VIRTUAL
  METHOD InitiateAction             VIRTUAL
  METHOD Invalidate                 VIRTUAL
  METHOD IsRightToLeft              VIRTUAL
  METHOD Loaded                     VIRTUAL
  METHOD ManualDock                 VIRTUAL
  METHOD ManualFloat                VIRTUAL
  METHOD MouseDown                  VIRTUAL
  METHOD MouseMove                  VIRTUAL
  METHOD MouseUp                    VIRTUAL
  METHOD Notification               VIRTUAL
  METHOD PaletteChanged             VIRTUAL
  METHOD ParentToClient             VIRTUAL
  METHOD Perform                    VIRTUAL
  METHOD PositionDockRect           VIRTUAL
  METHOD ReadState                  VIRTUAL
  METHOD Refresh                    VIRTUAL
  METHOD Repaint                    VIRTUAL
  METHOD ReplaceDockedControl       VIRTUAL
  METHOD RequestAlign
  METHOD Resize                     VIRTUAL
  METHOD ScreenToClient             VIRTUAL
  METHOD SendCancelMode             VIRTUAL
  METHOD SendDockNotification       VIRTUAL
  METHOD SendToBack                 VIRTUAL
  METHOD SetBiDiMode                VIRTUAL
  METHOD SetBounds                  VIRTUAL
  METHOD SetDragMode                VIRTUAL
  METHOD SetEnabled                 VIRTUAL
  METHOD SetParentBiDiMode          VIRTUAL
  METHOD SetParentComponent         VIRTUAL
  METHOD SetTextBuf                 VIRTUAL
  METHOD SetZOrder                  VIRTUAL
  METHOD Show                       VIRTUAL
  METHOD Update                     VIRTUAL
  METHOD UpdateBoundsRect           VIRTUAL
  METHOD UseRightToLeftAlignment    VIRTUAL
  METHOD UseRightToLeftReading      VIRTUAL
  METHOD UseRightToLeftScrollBar    VIRTUAL
  METHOD VisibleChanging            VIRTUAL
  METHOD WndProc                    VIRTUAL

  DATA OnActivate                 //: TNotifyEvent;
  DATA OnCanResize                //stored IsForm;
  DATA OnClick                    //stored IsForm;
  DATA OnClose                    //: TCloseEvent;
  DATA OnCloseQuery               //: TCloseQueryEvent;
  DATA OnCreate                   //: TNotifyEvent;
  DATA OnChange
  DATA OnDblClick                 //stored IsForm;
  DATA OnDestroy                  //: TNotifyEvent;
  DATA OnDeactivate               //: TNotifyEvent;
  DATA OnDragDrop                 //stored IsForm;
  DATA OnDragOver                 //stored IsForm;
  DATA OnHelp                     //: THelpEvent;
  DATA OnHide                     //: TNotifyEvent;
  DATA OnKeyDown                  //stored IsForm;
  DATA OnKeyPress                 //stored IsForm;
  DATA OnKeyUp                    //stored IsForm;
  DATA OnMouseDown                //stored IsForm;
  DATA OnMouseMove                //stored IsForm;
  DATA OnMouseUp                  //stored IsForm;
  DATA OnPaint                    //: TNotifyEvent;
  DATA OnResize                   //stored IsForm;
  DATA OnShortCut                 //: TShortCutEvent;
  DATA OnShow                     //: TNotifyEvent;

  METHOD SetVisible( Value )

ENDCLASS


METHOD SetName( Value ) CLASS TControl

   LOCAL bChangeText

   //bChangeText := And( csSetCaption, ::ControlStyle ) != 0 .AND. ( ::Name == ::Text ) .AND. ;
   //               ( ( ::Owner == NIL ) ( ::Owner:ClassName != "TCONTROL" ) .OR. ;
   //               ! (csLoading in ::Owner:ComponentState ) )

   bChangeText := ::Name == ::Text

  ::SetName( Value )

  IF bChangeText
     ::Text := Value
  ENDIF

RETURN NIL

METHOD SetParentComponent( Value ) CLASS TControl

   IF Value:IsDerivedFrom( "TWinControl" )
      ::SetParent( Value )
   ENDIF

RETURN NIL

METHOD SetParent( oParent ) CLASS TControl

   //TraceLog( ::ClassName() )

   IF ! ::FParent == oParent
      IF ::FParent == Self
         //raise EInvalidOperation.Create(SControlParentSetToSelf)
      ENDIF

      IF ::FParent != NIL
         ::FParent:RemoveControl( Self )
      ENDIF

      IF oParent != NIL
         oParent:InsertControl( Self )
         //Self:FParentWindow := oParent:FHandle
      ENDIF
   ENDIF

RETURN NIL

METHOD SetVisible( Value ) CLASS TControl

   IF ::FVisible != Value
      //VisibleChanging()
      ::FVisible := Value
      //Perform(CM_VISIBLECHANGED, Ord(Value), 0)
      ::RequestAlign()
   ENDIF

RETURN NIL

METHOD RequestAlign() CLASS TControl

   IF ::FParent != NIL
      ::FParent:AlignControl( Self )
   ENDIF

RETURN NIL

#Define WT_DIALOG     0
#Define WT_WINDOW     1
#Define WT_MDIFRAME   2
#Define WT_MDICHILD   4

pragma pack(4)

typedef struct {;
    HWND hwndFrom;
    UINT idFrom;
    UINT code;
} NMHDR

typedef struct { ;
    UINT    style;
    WNDPROC lpfnWndProc;
    int     cbClsExtra;
    int     cbWndExtra;
    HANDLE  hInstance;
    HICON   hIcon;
    HCURSOR hCursor;
    HBRUSH  hbrBackground;
    LPCTSTR lpszMenuName;
    LPCTSTR lpszClassName;
} WNDCLASS

typedef struct _RECT { ;
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECT

typedef struct tagDRAWITEMSTRUCT {;
    UINT  CtlType;
    UINT  CtlID;
    UINT  itemID;
    UINT  itemAction;
    UINT  itemState;
    HWND  hwndItem;
    HDC   hDC;
    RECT  rcItem;
    DWORD itemData;
} DRAWITEMSTRUCT

typedef struct tagPOINT {;
   LONG x;
   LONG y;
} POINT

typedef struct tagMENUITEMINFO {;
  UINT    cbSize;
  UINT    fMask;
  UINT    fType;
  UINT    fState;
  UINT    wID;
  HANDLE  hSubMenu;
  HBITMAP hbmpChecked;
  HBITMAP hbmpUnchecked;
  ULONG   dwItemData;
  LPTSTR  dwTypeData;
  UINT    cch;
  HBITMAP hbmpItem;
} MENUITEMINFO

typedef struct _OSVERSIONINFOEX {;
  DWORD dwOSVersionInfoSize;
  DWORD dwMajorVersion;
  DWORD dwMinorVersion;
  DWORD dwBuildNumber;
  DWORD dwPlatformId;
  TCHAR szCSDVersion[128];
  WORD wServicePackMajor;
  WORD wServicePackMinor;
  WORD wSuiteMask;
  BYTE wProductType;
  BYTE wReserved;
} OSVERSIONINFOEX

typedef struct MEASUREITEMSTRUCT {;
    UINT CtlType;
    UINT CtlID;
    UINT itemID;
    UINT itemWidth;
    UINT itemHeight;
    ULONG itemData;
} MEASUREITEMSTRUCT

typedef struct tagLOGBRUSH { ;
  UINT     lbStyle;
  COLORREF lbColor;
  LONG     lbHatch;
} LOGBRUSH

//----------------------------------------------------------------------------//

CLASS TWinControl FROM TControl
   private:
   DATA FTabStop AS LOGICAL PROTECTED
   public: 
   DATA Icon         EXPORTED
   DATA Color        EXPORTED INIT COLOR_BTNFACE+1
   DATA Style        PROTECTED INIT WS_OVERLAPPEDWINDOW
   DATA ExStyle      PROTECTED INIT 0

   PROPERTY Caption      READ FCaption WRITE SetText
   PROPERTY Text         READ GetText  WRITE SetTextx

   PROPERTY Handle       READ GetHandle
   PROPERTY ParentWindow READ FParentWindow WRITE SetParentWindow

   DATA FTabList     PRIVATE INIT {}

//   DATA Handle       PROTECTED

   DATA Id           
   DATA ResName      PROTECTED
   DATA Modal        PROTECTED INIT .F.
   DATA Instance     PROTECTED
   DATA InstMsg      PROTECTED
   DATA FrameWnd     PROTECTED AS LOGIC INIT .F.
   DATA MenuName     PROTECTED
   DATA WindowMenu   PROTECTED
   DATA nProc        PROTECTED
   DATA WndProc      PROTECTED   INIT "FormProc"
   DATA Msgs         PROTECTED   INIT -1
   DATA Parent       PROTECTED
   DATA FormType     PROTECTED
   DATA WindowState  PROTECTED   INIT SW_SHOW
   DATA Align        PROTECTED   INIT 0
   DATA lRegister    PROTECTED   INIT .T.
   DATA lControl     PROTECTED   INIT .F.

   DATA hBkBrush     PROTECTED
   DATA lHaveProc    PROTECTED   INIT .F.
   DATA Controls     PROTECTED   AS ARRAY INIT {}
   DATA Action       PROTECTED

   DATA FVisible     PROTECTED   INIT .T.

   DATA FShowing     PRIVATE  INIT .F.

   ACCESS Showing    INLINE ::FShowing
   ASSIGN Showing( lShow ) INLINE ::FShowing := lShow

   // New Propertie
   property TabStop as LOGICAL read FTabStop write SetTabStop default .F.


   METHOD Register()
   METHOD Create() CONSTRUCTOR
   METHOD FormProc()
   METHOD AlignControl()
   METHOD AlignControls()
   METHOD MsgBox()
   METHOD Center()

   METHOD GetHandle()           INLINE IIF( ::FHandle == NIL, ::CreateWnd(), ::FHandle )
   METHOD CreateWnd()
   METHOD DestroyHandle()

   METHOD Close()               INLINE SendMessage( ::handle, WM_SYSCOMMAND, SC_CLOSE )
   METHOD DestroyWindowHandle() INLINE DestroyWindow( ::handle )
   METHOD End(n)                INLINE EndDialog( ::handle, n )
   METHOD ResetProcedure()      INLINE ResetProcedure( ::handle )
   METHOD SetWindowMenu()       INLINE ::WindowMenu:Parent := Self, ::WindowMenu:Set()
   METHOD Maximize()            INLINE ShowWindow( ::handle, SW_SHOWMAXIMIZED)
   METHOD Minimize()            INLINE ShowWindow( ::handle, SW_SHOWMINIMIZED)
   METHOD Restore()             INLINE ShowWindow( ::handle, SW_RESTORE)
   METHOD Disable()             INLINE EnableWindow( ::handle, .f.)
   METHOD Enable()              INLINE EnableWindow( ::handle, .t.)
   METHOD Hide()                INLINE ShowWindow( ::handle, SW_HIDE)
   METHOD Show(n)               INLINE ::FShowing := .T., ShowWindow( ::handle, n)
   METHOD Shows()               INLINE ::FShowing := .T., ShowWindow( ::handle, SW_SHOW)
   METHOD SetFocus()            INLINE SetFocus( ::handle),self
   METHOD SetWindowFocus()      INLINE ::SetFocus()
   METHOD WindowRect()          INLINE GetWindowRect( ::FHandle )
   METHOD ClientRect()
   METHOD SetText(cText)        INLINE TraceLog( ::ClassName, ::FHandle, cText ), ::FCaption := cText, IIF( ::FHandle != NIL, SetWindowText( ::FHandle, cText ), )
   METHOD SetTextx(cText)       INLINE ::FText := cText, IIF( ::FHandle != NIL, ::SetTextBuf(cText ), )

   METHOD BringToTop()          INLINE BringWindowToTop( ::handle )
   METHOD ScreenToClient( aPt)  INLINE ScreenToClient( ::handle, @aPt )
   METHOD SetWindowPos( hAfter, x, y, w, h , n) INLINE SetWindowPos( ::Handle, hAfter, x, y, w, h , n )
   METHOD SendMessage( nMsg, nwParam, nlParam ) INLINE SendMessage( ::Handle, nMsg, nwParam, nlParam )
   METHOD PostMessage( nMsg, nwParam, nlParam ) INLINE PostMessage( ::Handle, nMsg, nwParam, nlParam )
   METHOD GetLong(nLong)        INLINE GetWindowLong( ::handle, nLong )
   METHOD SetLong(nLong,nSt)    INLINE SetWindowLong( ::handle, nLong, nSt )

   METHOD SetStyle(nStyle,lAdd) INLINE lAdd:=IFNIL(lAdd,.T.,lAdd),;
                                       ::SetLong( GWL_STYLE,;
                                       IIF(lAdd, OR(::GetLong(GWL_STYLE),nStyle ),;
                                                AND(::GetLong(GWL_STYLE),NOT(nStyle) ) ) )

   METHOD SetExStyle(nStyle,lAdd) INLINE lAdd:=IFNIL(lAdd,.T.,lAdd),;
                                       ::SetLong( GWL_EXSTYLE,;
                                       IIF(lAdd, OR(::GetLong(GWL_EXSTYLE),nStyle ),;
                                                AND(::GetLong(GWL_EXSTYLE),NOT(nStyle) ) ) )

   METHOD SetRedraw(lRed)       INLINE ::SendMessage( WM_SETREDRAW,IIF(lRed,1,0),0)
   METHOD SetBkBrush(hBrush)    INLINE IIF( !::ClassName()=='TPANEL',SetClassLong( ::handle, GCL_HBRBACKGROUND, hBrush ), ::hBkBrush := hBrush ),;
                                       ::InvalidateRect()
   METHOD InvalidateRect()      INLINE InvalidateRect( ::handle )
   METHOD Move( x, y, w, h, lRep ) INLINE ;
                                      ::FLeft  := IFNIL( x, ::FLeft, x ),;
                                      ::FTop   := IFNIL( y, ::FTop, y ),;
                                      ::Fwidth := IFNIL( w, ::Fwidth, w),;
                                      ::Fheight:= IFNIL( h, ::Fheight, h),;
                                      MoveWindow( ::handle, ::FLeft, ::FTop, ::FWidth, ::FHeight,;
                                      IIF( lRep != nil, lRep, .f.) )

   METHOD SetMethod( cMsg, FuncOrBlock, nScope ) INLINE IIF( __objHasMsg( Self, cMsg ), ;
                                                 __ClsModMsg( HB_QSelf(), cMsg, FuncOrBlock ), ;
                                                 __ClsAddMsg( HB_QSelf(), cMsg, FuncOrBlock, ;
                                                              IIF( ValType( FuncOrBlock ) == 'B', HB_OO_MSG_INLINE, HB_OO_MSG_METHOD ), ;
                                                              NIL, nScope ) )

   METHOD SetProc() INLINE ::nProc := SetProcedure( ::handle, HB_ObjMsgPtr( self, ::WndProc ), ::Msgs, self)

   METHOD SetParentWindow( oControl )
   METHOD Insert( oControl )
   METHOD InsertControl( oControl )
   METHOD Remove( oControl )
   METHOD RemoveControl( oControl )

   METHOD CreateHandle()
   METHOD UpdateControlState()
   METHOD UpdateShowing()

   METHOD HandleNeeded()
   METHOD HandleAllocated()
   METHOD Repaint()
   METHOD Update()
   METHOD Invalidate()
   METHOD GetTopParentHandle()
   METHOD GetParentHandle()
   METHOD CreateParentedControl( ParentWindow )
   METHOD CreateParented( ParentWindow )

   METHOD GetExported()
   METHOD DrawItem()            VIRTUAL
   METHOD Notify()              VIRTUAL

   METHOD WMChar()              VIRTUAL
   METHOD WMClose()             VIRTUAL
   METHOD WMCommand()           VIRTUAL
   METHOD WMGetMinMaxInfo()     VIRTUAL
   METHOD WMPaint()             VIRTUAL
   METHOD WMDestroy()           VIRTUAL
   METHOD WMDrawItem()          VIRTUAL
   METHOD WMEraseBkGnd()        VIRTUAL
   METHOD WMGetDlgCode()        VIRTUAL
   METHOD WMHScroll()           VIRTUAL
   METHOD WMKeyDown()           VIRTUAL
   METHOD WMKeyUp()             VIRTUAL
   METHOD WMInitMenuPopup()     VIRTUAL
   METHOD WMKillFocus()         VIRTUAL
   METHOD WMLButtonDown()       VIRTUAL
   METHOD WMLButtonUp( nwParam, nlParam1, nlParam2 ) INLINE IIF( __ClsMsgAssigned( Self, "OnClick" ), ::OnClick( nwParam, nlParam1, nlParam2 ), NIL )
   METHOD WMLButtonDblClk()     VIRTUAL
   METHOD WMMButtonDown()       VIRTUAL
   METHOD WMMButtonUp()         VIRTUAL
   METHOD WMMeasureItem()       VIRTUAL
   METHOD WMMenuSelect()        VIRTUAL
   METHOD WMMouseMove()         VIRTUAL
   METHOD WMMove()              VIRTUAL
   METHOD WMMoving()            VIRTUAL
   METHOD WMNcMouseMove()       VIRTUAL
   METHOD WMRButtonDown()       VIRTUAL
   METHOD WMRButtonUp()         VIRTUAL
   METHOD WMSetFocus()          VIRTUAL
   METHOD WMVScroll()           VIRTUAL
   METHOD WMSize()              VIRTUAL
   METHOD WMSizeMaxHide()       VIRTUAL
   METHOD WMSizeMaximized()     VIRTUAL
   METHOD WMSizeMaxShow()       VIRTUAL
   METHOD WMSizeMinimized()     VIRTUAL
   METHOD WMSizeRestored()      VIRTUAL
   METHOD WMSysCommand()        VIRTUAL
   METHOD WMTimer()             VIRTUAL
   METHOD WMCtlColor()          VIRTUAL
   METHOD WMNotify()            VIRTUAL
   METHOD WMCreate()            VIRTUAL
   METHOD WMCreation()          VIRTUAL
   METHOD WMInitDialog()        VIRTUAL
   METHOD WMActivate()          VIRTUAL
   METHOD WMUserMsg()           VIRTUAL
   METHOD WMMessage()           VIRTUAL
   METHOD WMSetFont()           VIRTUAL
   METHOD WMNCCreate()          VIRTUAL
   METHOD WMNCActivate()        VIRTUAL
   METHOD WMNCHitTest()         VIRTUAL
   METHOD WMNCPaint()           VIRTUAL
   METHOD WMNCDestroy()         VIRTUAL
   METHOD WMWindowPosChanged()  VIRTUAL
   METHOD WMWindowPosChanging() VIRTUAL
   METHOD WMSysColorChange()    VIRTUAL
   METHOD WMSysKeyDown()        VIRTUAL
   METHOD WMSysKeyUp()          VIRTUAL
   METHOD WMSizing()            VIRTUAL
   METHOD WMShowWindow()        VIRTUAL
   METHOD WMSetCursor()         VIRTUAL
   METHOD WMSetCursor()         VIRTUAL
   METHOD WMSetCursor()         VIRTUAL
   METHOD WMEnable()            VIRTUAL
   METHOD WMContextMenu()       VIRTUAL
   METHOD WMDropFiles()         VIRTUAL
   METHOD WMSetText()           VIRTUAL
   METHOD WMNCCalcSize()        VIRTUAL
   METHOD WMNCLButtonUp()       VIRTUAL
   METHOD WMNCLButtonDown()     VIRTUAL
   METHOD WMNotify()            VIRTUAL
   // especialized method (for retriving text on tedit controls
   METHOD Perform ( nMsg, nwParam, nlParam )
   METHOD GetTextLen()
   METHOD SetTextBuf(Buffer) 

   METHOD GetText()
 
   METHOD GetTextBuf(Buffer,BufSize)


   METHOD DefaultHandler( hWnd, nMsg, nwParam, nlParam )
   METHOD SetTabStop(Value) 
ENDCLASS



//----------------------------------------------------------------------------
METHOD Register() CLASS TWinControl

   LOCAL wc

   LOCAL c

   IF ( c := GetClassInfo( Application:Instance, ::ClassName ) ) == NIL

      wc IS WNDCLASS
      wc:lpszClassName := ::ClassName
      wc:hIcon         := ::Icon
      wc:hCursor       := ::Cursor
      wc:hbrBackground := ::Color
      wc:hInstance     := Application:Instance
      IF ValType( ::MenuName ) == "C"
         wc:lpszMenuName  := ::MenuName
      ENDIF

      IF ::Parent == NIL
         ::lHaveProc := .T.
         RETURN( RegisterClass( wc , ::FormType, HB_ObjMsgPtr( Self, ::WndProc ), ::Msgs, self ) )
      ELSE
         RETURN( RegisterClass( wc , ::FormType ) )
      ENDIF

   ENDIF

RETURN(.T.)

//----------------------------------------------------------------------------
METHOD CreateWnd() CLASS TWinControl

   LOCAL ParentHandle


   IF ::FParent == NIL
      ParentHandle := 0
   ELSE
      ParentHandle := ::FParent:Handle
   ENDIF

   IF ::lRegister
      IF !::Register()
         RETURN NIL
      ENDIF
   ENDIF

   ::FLeft   := IFNIL( ::FLeft  , 0            , ::FLeft  )
   ::FTop    := IFNIL( ::FTop   , 0            , ::FTop   )
   ::FWidth  := IFNIL( ::FWidth , CW_USEDEFAULT, ::FWidth )
   ::FHeight := IFNIL( ::Fheight, CW_USEDEFAULT, ::Fheight)

   IF ::FormType == WT_DIALOG
      DEFAULT ::resname TO MakeDlgTemplate( ::FCaption, ::Style, ( ::FLeft * 4 )  /LOWORD(GetDialogBaseUnits()),;
                                                                 ( ::FTop * 4 )   /LOWORD(GetDialogBaseUnits()),;
                                                                 ( ::FWidth * 4 ) /LOWORD(GetDialogBaseUnits()),;
                                                                 ( ::FHeight * 4 )/LOWORD(GetDialogBaseUnits()) )
      IF ::Modal
         DialogBox( ::Instance, ::resname, ParentHandle, HB_ObjMsgPtr( self, ::WndProc ), self )
      ELSE
         ::FHandle := CreateDialog( ::Instance, ::resname, ParentHandle, HB_ObjMsgPtr( self, ::WndProc ), self )
         IF ::FWidth > 0
            ::Move( ::FLeft, ::FTop, ::FWidth, ::FHeight, .T. )
         ENDIF

         ::WMCreate()
         ::Show( ::WindowState )

      ENDIF
   ELSE

      ::FHandle := CreateWindowEx( ::ExStyle, ::ClassName, ::FCaption, ::Style, ;
                                  ::FLeft, ::FTop, ::FWidth, ::FHeight, ;
                                  ParentHandle, 0, ::Instance )

      IF !::lHaveProc
         ::nProc := SetProcedure( ::FHandle, HB_ObjMsgPtr( self, ::WndProc ), ::Msgs, self)
      ENDIF

      ::WMCreate()
      ::Show( ::WindowState )
   ENDIF
RETURN ::FHandle

//----------------------------------------------------------------------------
METHOD Create( oOwner ) CLASS TWinControl

   ::Super:Create( oOwner )

   ::Instance := oOwner:Instance

   IF ::FormType == WT_DIALOG .AND. ! ::Modal .AND. ::FWidth > 0
      ::Move( , , , , .T. )
   ENDIF

RETURN Self

//----------------------------------------------------------------------------//

METHOD MsgBox(cMsg, cTitle, nFlags) CLASS TWinControl
   DEFAULT cTitle TO ::FCaption
RETURN MessageBox( GetActiveWindow(), cMsg, cTitle, nFlags)


METHOD GetExported() CLASS TWinControl
RETURN( __objGetMsgList( self ) )

//----------------------------------------------------------------------------//

METHOD FormProc( hWnd, nMsg, nwParam, nlParam ) CLASS TWinControl

   local cPaint, hDC, nRet, oMenuItem, nCode, nId, aRect, dis, n, hdr, aPt, msg, lpMenuItemInfo, cBuffer
   local SubMenuItem, MenuItem, OsVer, mi,cText,ncode1,nid1
   IF ::Parent != NIL .AND. ::Parent:InstMsg != NIL
      IF nMsg == ::Parent:InstMsg
         SetForegroundWindow( hWnd )
      ENDIF
   ENDIF

   SWITCH nMsg
      CASE WM_ACTIVATE
          SWITCH nwParam
              CASE WA_ACTIVE
              CASE WA_CLICKACTIVE
                 IF __ClsMsgAssigned( Self, "OnActivate" )
                    nRet := ::OnActivate()
                 ELSE
                    nRet := ::WMActivate( nwParam, nlParam )
                 ENDIF
                 EXIT

              CASE WA_INACTIVE
                 IF __ClsMsgAssigned( Self, "OnDeActivate" )
                    nRet := ::OnDeActivate
                 ELSE
                    nRet := ::WMActivate( nwParam, nlParam )
                 ENDIF
                 EXIT

              DEFAULT
                nRet := ::WMActivate( nwParam, nlParam )
           END
           EXIT

      CASE WM_SETFONT
           nRet := ::WMSetFont()
           EXIT

      CASE WM_NCLBUTTONUP
           nRet := ::WMNCLButtonUp( nwParam, nlParam )
           EXIT

      CASE WM_NCLBUTTONDOWN
           nRet := ::WMNCLButtonDown( nwParam, nlParam )
           EXIT

      CASE WM_NCCREATE
           nRet := ::WMNcCreate()
           EXIT

      CASE WM_NCACTIVATE
           nRet := ::WMNcActivate()
           EXIT

      CASE WM_NCCALCSIZE
           nRet := ::WMNcCalcSize()
           EXIT

      CASE WM_NCDESTROY
           nRet := ::WMNcDestroy()
           EXIT

      CASE WM_NCHITTEST
           nRet := ::WMNCHitTest( loword(nlParam), hiword(nlParam) )
           EXIT

      CASE WM_CREATE
           DEFAULT ::FHandle TO hWnd
           nRet := ::WMCreation( nwParam, nlParam )
           EXIT

      CASE WM_INITDIALOG
           DEFAULT ::FHandle TO hWnd
           nRet := ::WMInitDialog( nwParam, nlParam )
           EXIT

      CASE WM_CHAR
           nRet := ::WMChar( nwParam, nlParam )
           IF nRet == NIL
              IF ValType( ::Controls[n]:OnKeyPress ) == "B"
                 nRet := EVAL( ::Controls[n]:OnKeyPress, ::Controls[n],nwParam)
              ELSEIF ValType( ::Controls[n]:OnKeyPress ) == "N"
                 nRet := HB_Exec( ::Controls[n]:OnKeyPress, ::Controls[n],nwParam)
              ENDIF
           ENDIF
           EXIT

      CASE WM_CLOSE
           nRet := ::WMClose( nwParam )
           EXIT

      CASE WM_MENUCOMMAND
/*
           IF ::FMenu != NIL
              OsVer IS OSVERSIONINFOEX
              cBuffer := OsVer:value
              GetVersionEx( @cBuffer )
              OsVer:Buffer( cBuffer )

              IF OsVer:dwMajorVersion > 4
                 nId := GetMenuItemId( nlParam, nwParam )
               ELSE
                 nId   := LOWORD( nwParam )
              ENDIF
              FOR EACH MenuItem IN ::FMenu:aItems
                  IF MenuItem:Command == nId
                     IF ValType( MenuItem:Action ) == "B"
                        nRet := EVAL( MenuItem:Action, MenuItem )
                     ELSEIF ValType( MenuItem:Action ) == "N"
                        nRet := HB_Exec( MenuItem:Action, MenuItem )
                     ENDIF
                  ENDIF
                  FOR EACH SubMenuItem IN MenuItem:aItems
                      IF SubMenuItem:Command == nId
                         IF ValType( SubMenuItem:Action ) == "B"
                            nRet := EVAL( SubMenuItem:Action, SubMenuItem )
                         ELSEIF ValType( SubMenuItem:Action ) == "N"
                            nRet := HB_Exec( SubMenuItem:Action, SubMenuItem )
                         ENDIF
                      ENDIF
                  NEXT
              NEXT
              IF nRet!=nil
                 RETURN nRet
              ENDIF
           ENDIF
*/
           EXIT

      CASE WM_COMMAND
           nCode := HIWORD( nwParam )
           nId   := LOWORD( nwParam )
           nCode1 := HIWORD( nlParam )
           nId1   := LOWORD( nlParam )
           
           IF ( n := ASCAN( ::Controls, {|o| o:handle == nlParam } ) ) > 0

              IF ValType( ::Controls[n]:OnClick ) == "B"
                 nRet := EVAL( ::Controls[n]:OnClick, ::Controls[n] )
              ELSEIF ValType( ::Controls[n]:OnClick ) == "N"
                 nRet := HB_Exec( ::Controls[n]:OnClick, ::Controls[n] )
              ENDIF

              IF ValType( ::Controls[n]:OnChange ) == "B"
                 nRet := EVAL( ::Controls[n]:OnChange, ::Controls[n] )
              ELSEIF ValType( ::Controls[n]:OnChange ) == "N"
                 nRet := HB_Exec( ::Controls[n]:OnChange, ::Controls[n] )
              ENDIF

              IF nRet!=nil
                 RETURN nRet
              ENDIF

           ENDIF

           IF nCode == 0 .AND. __ClsMsgAssigned( self, "Menu" ) .AND. ::Menu != NIL

              FOR EACH MenuItem IN ::FMenu:aItems
                  IF MenuItem:Command == nId
                     IF ValType( MenuItem:Action ) == "B"
                        nRet := EVAL( MenuItem:Action, MenuItem )
                     ELSEIF ValType( MenuItem:Action ) == "N"
                        nRet := HB_Exec( MenuItem:Action, MenuItem )
                     ENDIF
                  ENDIF

                  FOR EACH SubMenuItem IN MenuItem:aItems
                      IF SubMenuItem:Command == nId
                         IF ValType( SubMenuItem:Action ) == "B"
                            nRet := EVAL( SubMenuItem:Action, SubMenuItem )
                         ELSEIF ValType( SubMenuItem:Action ) == "N"
                            nRet := HB_Exec( SubMenuItem:Action, SubMenuItem )
                         ENDIF
                      ENDIF
                  NEXT
              NEXT
/*

             .AND. ( oMenuItem := ::Menu:GetItem( nwParam ) ) != NIL ;
             .AND. oMenuItem:Action != NIL
              nRet := Eval( oMenuItem:Action, oMenuItem )
*/
           ELSE
              nRet := ::WMCommand( nwParam, nlParam )
           ENDIF
           EXIT

      CASE WM_GETMINMAXINFO
           nRet := ::WMGetMinMaxInfo( nlParam )
           EXIT

      CASE WM_PAINT
           hDC  := BeginPaint( ::FHandle, @cPaint )
           nRet := ::WMPaint( hDC )
           EndPaint( ::FHandle, cPaint)
           EXIT

      CASE WM_DESTROY
           nRet := ::WMDestroy()
           EXIT

      CASE WM_DRAWITEM
           dis IS DRAWITEMSTRUCT
           dis:buffer( peek( nlParam, dis:sizeof() ) )

           IF (n:=ASCAN(::Controls,{|o|o:handle==dis:hwndItem}))>0
              nRet:=::Controls[n]:DrawItem(dis)
           ELSE
              nRet := ::WMDrawItem(dis,nlParam,nwParam)
           ENDIF
           EXIT

      CASE WM_ERASEBKGND
           nRet := ::WMEraseBkGnd( nwParam )
           EXIT

      CASE WM_GETDLGCODE
           nRet := ::WMGetDlgCode()
           EXIT

      CASE WM_HSCROLL
           nRet := ::WMHScroll( nwParam, nlParam )
           EXIT

      CASE WM_KEYDOWN
           nRet := ::WMKeyDown( nwParam, nlParam )
           EXIT

      CASE WM_KEYUP
           nRet := ::WMKeyUp( nwParam, nlParam )
           EXIT

      CASE WM_INITMENUPOPUP
           nRet := ::WMInitMenuPopup( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_KILLFOCUS
           nRet := ::WMKillFocus( nwParam )
           EXIT

      CASE WM_LBUTTONDOWN
           nRet := ::WMLButtonDown( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_LBUTTONUP
           nRet := ::WMLButtonUp( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_LBUTTONDBLCLK
           nRet := ::WMLButtonDblClk( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_MBUTTONDOWN
           nRet := ::WMMButtonDown( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_MBUTTONUP
           nRet := ::WMMButtonUp( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_MEASUREITEM
           DEFAULT ::handle TO hWnd

           mi IS MEASUREITEMSTRUCT
           mi:Pointer( nlParam ) //buffer( peek( nlParam, mi:sizeof() ) )

           nRet := ::WMMeasureItem( mi, nlParam )
           EXIT

      CASE WM_MENUSELECT
           nRet := ::WMMenuSelect( LoWord(nwParam), Hiword(nwparam), nlParam )
           EXIT

      CASE WM_MOUSEMOVE
           nRet := ::WMMouseMove( nwParam, LoWord(nlParam), Hiword(nlparam) )
           EXIT

      CASE WM_NCMOUSEMOVE
           nRet := ::WMNcMouseMove( nwParam, LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_RBUTTONDOWN
           nRet := ::WMRButtonDown( nwParam, LoWord(nlParam), Hiword(nlparam) )
           EXIT

      CASE WM_RBUTTONUP
           nRet := ::WMRButtonUp( nwParam, LoWord(nlParam), Hiword(nlparam) )
           EXIT

      CASE WM_SIZE
           aRect := ::WindowRect()
           ::Fwidth := aRect[3]-aRect[1]
           ::Fheight:= aRect[4]-aRect[2]
           nRet := ::WMSize( nwParam, LoWord(nlParam), Hiword(nlparam) )
           EXIT

      CASE WM_MOVING
           aRect := ::WindowRect()
           ::FLeft:= aRect[1]
           ::FTop := aRect[2]
           nRet := ::WMMoving( LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_MOVE
           aRect := ::WindowRect()
           ::FLeft:= aRect[1]
           ::FTop := aRect[2]
           nRet := ::WMMove( LoWord( nlParam ), HiWord( nlParam ) )
           EXIT

      CASE WM_SETFOCUS
           nRet := ::WMSetFocus( nwParam )
           EXIT

      CASE WM_VSCROLL
           nRet := ::WMVScroll( nwParam, nlParam )
           EXIT

      CASE WM_SYSCOMMAND
           nRet := ::WMSysCommand( nwParam, nlParam )

           IF nRet == NIL .AND. nwParam == SC_CLOSE

              IF ValType( ::OnCloseQuery ) == "B"
                 nRet := EVAL( ::OnCloseQuery, Self )
              ELSEIF ValType( ::OnCloseQuery ) == "N"
                 nRet := HB_Exec( ::OnCloseQuery, Self )
              ENDIF

              IF nRet == NIL
                 IF ::FormType == WT_DIALOG
                    IF ::Modal
                       ::End( IDCANCEL )
                    ELSE
                       ::DestroyWindowHandle()
                    ENDIF
                 ENDIF

                 nRet := Application:RemoveForm( self )
                 IF nRet != NIL
                    Application:Terminate()
                 ENDIF

              ENDIF
           ENDIF
           EXIT

      CASE WM_TIMER
           nRet := ::WMTimer( nwParam, nlParam )
           EXIT

      CASE WM_CTLCOLORSTATIC
      CASE WM_CTLCOLOREDIT
      CASE WM_CTLCOLORDLG
      CASE WM_CTLCOLORLISTBOX
           nRet := ::WMCtlColor( nMsg, nwParam, nlParam )

           IF nRet == NIL .AND. ::hBkBrush != NIL .AND. nMsg == WM_CTLCOLORDLG
              RETURN( ::hBkBrush )
           ENDIF
           EXIT

      CASE WM_NOTIFY
           hdr IS NMHDR
           hdr:Pointer( nlParam )

           IF __ClsMsgAssigned( Self, "WMNotify" )
              nRet := ::WMNotify( hdr, nlParam )
           ELSEIF ( n := aScan( ::Controls, {|o| o:FHandle == hdr:hwndFrom } ) ) > 0
              nRet := ::Controls[n]:Notify( hdr, nlParam )
           ENDIF
           EXIT

      CASE WM_NCPAINT
           nRet := ::WMNCPaint( nwParam, nlParam )
           EXIT

      CASE WM_WINDOWPOSCHANGED
           nRet := ::WMWindowPosChanged( nwParam, nlParam )
           EXIT

      CASE WM_WINDOWPOSCHANGING
           nRet := ::WMWindowPosChanging( nwParam, nlParam )
           EXIT

      CASE WM_SYSCOLORCHANGE
           nRet := ::WMSysColorChange( nwParam, nlParam )
           EXIT

      CASE WM_SYSKEYDOWN
           nRet := ::WMSysKeyDown( nwParam, nlParam )
           EXIT

      CASE WM_SYSKEYUP
           nRet := ::WMSysKeyUp( nwParam, nlParam )
           EXIT

      CASE WM_SIZING
           nRet := ::WMSizing( nwParam, nlParam )
           EXIT

      CASE WM_SHOWWINDOW
           nRet := ::WMShowWindow( nwParam, nlParam )
           EXIT

      CASE WM_SETCURSOR
           nRet := ::WMSetCursor( nwParam, nlParam )
           EXIT

      CASE WM_ENABLE
           nRet := ::WMEnable( nwParam, nlParam )
           EXIT

      CASE WM_CONTEXTMENU
           nRet := ::WMContextMenu( LOWORD(nlparam) , HIWORD(nlparam) )
           EXIT

      CASE WM_DROPFILES
           nRet := ::WMDropFiles( nwParam, nlParam )
           EXIT

/*      CASE WM_SETTEXT
           nRet := ::WMSetText( nwParam, nlParam )
           EXIT

      CASE  WM_GETTEXTLENGTH
           IF Valtype(::FText ) != "C"
              nRet :=  0
           ELSE
              nRet := Len(::FText)
           ENDIF
           EXIT

      CASE WM_SETTEXT
           cText :=  nlParam
           ::fText := Nil
           ::fText := cText
           nRet := 1
           exit
      CASE WM_GETTEXT
          IF ::FText != NIL
             cText := ::FText
          ELSE
             cText := ''
          ENDIF

          nRet := len( nlParam := Left(cText, nWParam - 1))
          EXIT
*/
      DEFAULT
           IF nMsg >= WM_USER
              nRet := ::WMUserMsg( nMsg, nwParam, nlParam)
           ELSEIF ::Parent != NIL .AND. nMsg == ::Parent:InstMsg
              nRet:= SetForegroundWindow( hWnd )
           ELSE
              nRet := ::WMMessage( nMsg, nwParam, nlParam)
           ENDIF
   END

   IF nRet != NIL
      RETURN  nRet
   ENDIF

   IF ::nProc == nil // no SetProcedure
      SWITCH ::FormType
         CASE WT_MDICHILD
              RETURN DefMDIChildProc( hWnd, nMsg, nwParam, nlParam )
              EXIT

         CASE WT_MDIFRAME
              RETURN DefFrameProc( hWnd, nMsg, nwParam, nlParam )
              EXIT

         CASE WT_WINDOW
              RETURN DefWindowProc( hWnd, nMsg, nwParam, nlParam )
              EXIT

         CASE WT_DIALOG
              RETURN 0
      END
   ENDIF

   RETURN( CallWindowProc( ::nProc, ::handle, nMsg, nwParam, nlParam ) )

METHOD Center( NewX, NewY, hParent ) CLASS TWinControl

   Local hWndParent
   Local aChild_[ 4 ]
   Local iCWidth
   Local iCHeight
   Local aParent_[ 4 ]
   Local aPoint_[ 2 ]

   aChild_ := ::WindowRect()
   iCWidth := aChild_[ 3 ] - aChild_[ 1 ]
   iCHeight := aChild_[ 4 ] - aChild_[ 2 ]
   If hparent == NIL
      hWndParent := GetWindow( ::handle, GW_OWNER )
   Else
      hWndParent := hparent
   EndIf
   if hWndParent == 0  // Center in the desktop if no Owner
       hWndParent := GetDesktopWindow()
   endif
   aParent_ := GetClientRect( hWndParent )
   aPoint_ := { ( aParent_[ 3 ] / 2 ) , ( aParent_[ 4 ] / 2 ) }
   ClientToScreen( hWndParent, aPoint_ )
   aPoint_[ 1 ] -= ( iCWidth / 2 )
   aPoint_[ 2 ] -= ( iCHeight / 2 )
   ScreenToClient( hWndParent, aPoint_ )
   aPoint_[ 1 ] := Max( 0, aPoint_[ 1 ] )
   aPoint_[ 2 ] := Max( 0, aPoint_[ 2 ] )
   ClientToScreen( hWndParent, aPoint_ )

   If NewX # NIL .AND. NewY # NIL
      ::Move( NewX, NewY, iCWidth, iCHeight, .t. )
     Else
      ::Move( aPoint_[ 1 ] , aPoint_[ 2 ] , iCWidth, iCHeight, .t. )
   EndIf

   RETURN( NIL )

//----------------------------------------------------------------------------//

METHOD ClientRect() CLASS TWinControl
local n
local aRect:=GetClientRect( ::handle )
FOR n:=1 to len(::Controls)
    do CASE
       CASE ::Controls[n]:ClassName()=='TCOOLBAR'
            aRect[2]+=::Controls[n]:Width
       CASE ::Controls[n]:ClassName()=='TSTATUSBAR'
            aRect[4]-=::Controls[n]:Width
    ENDCASE
NEXT

RETURN aRect

//----------------------------------------------------------------------------//


METHOD SetParentWindow( Value ) CLASS TWinControl

   IF ( ::FParent == NIL ) .AND. ( ::FParentWindow <> Value)
      IF ( ::FHandle != 0 ) .AND. ( ::FParentWindow <> 0 ) .AND. ( Value != 0 )
         ::FParentWindow := Value
         SetParent( ::FHandle, Value )
      ELSE
         ::DestroyHandle()
         ::FParentWindow := Value
      ENDIF

      ::UpdateControlState()
   ENDIF

RETURN NIL

METHOD Insert( oControl ) CLASS TWinControl

   IF oControl != NIL
      IF oControl:IsDerivedFrom( "TWinControl" )
         aAdd( ::FWinControls, oControl)
         aAdd( ::FTabList, oControl )
      ELSE
         aAdd( ::FControls, oControl )
      ENDIF

      oControl:FParent := Self
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------//

METHOD Remove( oControl ) CLASS TWinControl

   IF oControl:IsDerivedFrom( "TWinControl" )
      aDel( ::FTabList, aScan( ::FTabList, oControl ), .T. )
      aDel( ::FWinControls, aScan( ::FWinControls, oControl), .T. )
   ELSE
      aDel( ::FControls, aScan( ::FControls, oControl ), .T. )
   ENDIF

   oControl:FParent := NIL

RETURN NIL

//----------------------------------------------------------------------------//
METHOD InsertControl( oControl ) CLASS TWinControl

   //oControl.ValidateContainer( Self )
   //::Perform( CM_CONTROLLISTCHANGE, Integer( oControl), Integer( .T. ) );

   ::TWinControl:Insert( oControl )

   IF .T. //And( csReadingState, oControl:ControlState ) == 0
      //oControl:Perform( CM_PARENTCOLORCHANGED, 0, 0 )
      //oControl:Perform( CM_PARENTFONTCHANGED, 0, 0 )
      //oControl:Perform( CM_PARENTSHOWHINTCHANGED, 0, 0 )
      //oControl:Perform( CM_PARENTBIDIMODECHANGED, 0, 0 )

      IF oControl:IsDerivedFrom( "TWinControl" )
         //oControl:Perform( CM_PARENTCTL3DCHANGED, 0, 0 )
         ::UpdateControlState()
         ::AlignControl( oControl )
      ELSE
         IF ::FHandle != NIL
            oControl:Invalidate()
         ENDIF
      ENDIF
   ENDIF

   //::Perform( CM_CONTROLCHANGE, Integer( oControl ), Integer( .T. ) )

RETURN NIL

//----------------------------------------------------------------------------//
METHOD RemoveControl( oControl ) CLASS TWinControl

  //::Perform( CM_CONTROLCHANGE, Integer( oControl ), Integer( .F. ) )

  IF oControl:IsDerivedFrom( "TWinControl" )
     WITH OBJECT oControl
        :RemoveFocus( .T. )
        :DestroyHandle()
     END WITH
  ELSE
    IF ::FHandle != NIL
       oControl:InvalidateControl( oControl:Visible, .F. )
    ENDIF
  ENDIF

  ::TWinControl:Remove( oControl )

  //::Perform( CM_CONTROLLISTCHANGE, Integer( oControl), Integer( .F. ) )

  ::Realign()

RETURN NIL
//----------------------------------------------------------------------------//

METHOD UpdateShowing() CLASS TWinControl

  LOCAL ShowControl
  LOCAL I

  ShowControl :=  ::FVisible //.OR. And( csDesigning, ComponentState ) .AND. ;
                   //! ( And( csNoDesignVisible in ControlStyle ) ) .AND. ;
                   //! And( csReadingState in ControlState )

  //TraceLog( ::ClassName, ShowControl )

  IF ShowControl
     IF ::FHandle == NIL
        ::CreateHandle()
     ENDIF

     IF ::FWinControls <> nil
        aEval( ::FWinControls, {|o| o:UpdateShowing() } )
     ENDIF
  ENDIF

  IF ::FHandle != NIL
     IF ! ::FShowing == ShowControl
        ::FShowing := ShowControl
        BEGIN SEQUENCE
          // Perform(CM_SHOWINGCHANGED, 0, 0)
        RECOVER
           ::FShowing := ! ShowControl
           //raise
        END SEQUENCE
     ENDIF
  ENDIF

RETURN NIL

METHOD UpdateControlState() CLASS TWinControl

   LOCAL Control

   Control := Self

   WHILE Control:Parent != NIL
      Control := Control:Parent

      IF ! Control:Showing
         EXIT
      ENDIF
   ENDDO

   IF Control:IsDerivedFrom( "TCustomForm" ).OR. ( Control:FParentWindow != NIL )
      ::UpdateShowing()
   ENDIF

RETURN NIL

METHOD CreateHandle() CLASS TWinControl

   IF ::FHandle == NIL
      ::CreateWnd()

      //SetProp(FHandle, MakeIntAtom(ControlAtom), THandle(Self));
      //SetProp(FHandle, MakeIntAtom(WindowAtom), THandle(Self));

      IF ::FParent != NIL
         //SetWindowPos( ::FHandle, ::FParent:PrecedingWindow( Self ), 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE + SWP_NOACTIVATE )
      ENDIF

   ENDIF

RETURN NIL

METHOD CreateParented( ParentWindow ) CLASS TWinControl

   ::FParentWindow := ParentWindow

   Create( NIL )

RETURN Self

METHOD CreateParentedControl( ParentWindow ) CLASS TWinControl

   LOCAL oControl := TWinControl()

   oControl:FParentWindow := ParentWindow

   oControl:Create( NIL )

RETURN oControl

METHOD GetParentHandle() CLASS TWinControl

   IF ::Parent != NIL
      RETURN ::Parent:Handle
   ENDIF

RETURN ::ParentWindow

METHOD GetTopParentHandle() CLASS TWinControl

   LOCAL oControl := Self, Result

   WHILE oControl:Parent != NIL
      oControl := oControl:Parent
   ENDDO

   Result := oControl:ParentWindow

   IF Result == 0
      Result := oControl:Handle
   ENDIF

RETURN Result

METHOD Invalidate() CLASS TWinControl

   IF ::HandleAllocated
      IF ::Parent != NIL
         ::Parent:Invalidate()
      ENDIF

//      IF Message.WParam == 0
         InvalidateRect( ::FHandle, NIL, And( WS_EX_TRANSPARENT, ::ControlStyle ) )
//      ENDIF

   ENDIF

RETURN NIL

METHOD Update() CLASS TWinControl

   IF ::HandleAllocated
      UpdateWindow( ::FHandle )
   ENDIF

RETURN NIL

METHOD Repaint() CLASS TWinControl

   ::Invalidate()
   ::Update()

RETURN NIL

METHOD HandleAllocated() CLASS TWinControl

RETURN ::FHandle != NIL

METHOD DestroyHandle() CLASS TWinControl

   IF ::FHandle != NIL
      aEval( ::FWinControls, {|o| o:DestroyHandle() } )
      ::DestroyWindowHandle()
   ENDIF

RETURN NIL

METHOD HandleNeeded CLASS TWinControl

   IF ::FHandle = NIL
      IF ::Parent != NIL
         ::Parent:HandleNeeded()
      ENDIF

      ::CreateHandle()
   ENDIF

RETURN NIL

METHOD AlignControls( oControl, Rect ) CLASS TWinControl

RETURN NIL

METHOD AlignControl( oControl ) CLASS TWinControl

   //::AlignControls( oControl, ::GetClientRect() )

   IF oControl:FHandle == NIL
      oControl:CreateWnd()
   ENDIF

RETURN NIL

/* NEW METHODS TO GET  SPECIFIC DATAS */
METHOD SetTabStop(Value) CLASS TWinControl
LOCAL  Style

  IF ::FTabStop <> Value 

    ::FTabStop := Value
    IF ::HandleAllocated() 

      Style := GetWindowLong(::FHandle, GWL_STYLE) .and. ! WS_TABSTOP
      IF Value
         Style := Style .or. WS_TABSTOP
      ENDIF
      SetWindowLong(::FHandle, GWL_STYLE, Style)

//      ::Perform(CM_TABSTOPCHANGED, 0, 0)
    ENDIF
  ENDIF
RETURN SELF


METHOD DefaultHandler( hWnd, nMsg, nwParam, nlParam ) CLASS TWinControl
   LOCAL nret, cPaint, hDC, aRect, n, hdr,P

      Switch nMsg  

      CASE WM_GETTEXT
        
          if ::FText != nil
             P := ::FText
          else
             P := ''
          endif
          nret := 1
          ::fText:=left(p,nwParam)

        exit
      CASE WM_GETTEXTLENGTH
        if ::FText == nil
           nRet := 0
        else
           nRet := Len(::FText)
        endif
        exit
      CASE WM_SETTEXT
          nRet := 0
       exit 
       Default
         nRet := ::ControlProc(hWnd, nMsg, nwParam, nlParam)
        end
    RETURN nRet


METHOD Perform( nMsg, nwParam, nlParam ) CLASS TWinControl
   LOCAL nRet
   nret := ::&( ::WndProc )( ::handle, nMsg, nwParam, nlParam )

RETURN nRet

METHOD GetTextLen() CLASS TWinControl
RETURN iif(VALTYPE(::fText)=="U",0,SendMessage(::Handle,WM_GETTEXTLENGTH,0,0))


METHOD  SetTextBuf(Buffer) CLASS TWinControl

  ::Perform(WM_SETTEXT, 0, @Buffer)
  ::Perform(CM_TEXTCHANGED, 0, 0)
RETURN self

METHOD GetText() CLASS TWinControl

  LOCAL  Len
  LOCAL  cBuf

  Len := ::GetTextLen()

  cBuf := Space( Len )
  if Len <> 0
   ::GetTextBuf(@cBuf, Len + 1)
endif
RETURN cBuf

METHOD GetTextBuf(Buffer,BufSize) CLASS TWinControl
   Local Ret := SendMessage(::Handle,WM_GETTEXT, BufSize, @Buffer)
   ::Perform(WM_GETTEXT,BufSize,@Buffer)
RETURN  Ret




*------------------------------------------------------------------------------*
IMPORT C STRUCTURE NMHDR
*------------------------------------------------------------------------------*

CLASS TCustomControl FROM TWinControl
   DATA Id  EXPORTED INIT 0

   METHOD Create() CONSTRUCTOR
   METHOD Delete()
   METHOD DelControl()
   METHOD WMDestroy() INLINE if( ::Font > 0, DeleteObject( ::Font ),), super:WMDestroy()
   METHOD DrawItem() VIRTUAL
   METHOD ControlProc()
   METHOD CreateWnd()
   METHOD Perform( nMsg, nwParam, nlParam )

ENDCLASS

*------------------------------------------------------------------------------*

METHOD Create( oOwner ) CLASS TCustomControl

   ::Super:Create( oOwner )

   ::Instance := oOwner:Instance
   ::lControl := .T.

   ::WndProc := IFNIL(::WndProc,'ControlProc',::WndProc)


   IF ! oOwner:ClassName() == "OCTRLMASK"

      IF __objHasMethod( oOwner, "Add" )
         oOwner:Add( Self )
      ENDIF

   ENDIF

RETURN( self )

*------------------------------------------------------------------------------*

METHOD CreateWnd() CLASS TCustomControl

   ::FHandle := CreateWindowEx( ::ExStyle, ::WinClass, ::FCaption, ::Style, ;
                                ::FLeft, ::FTop, ::FWidth, ::FHeight, ;
                                ::FParent:Handle, ::Id, ::Instance )

   IF ::ClassName != "THintWindow"
      IF ::Font == NIL
         ::Font := SendMessage( ::FParent:Handle, WM_GETFONT, 0, 0 )
      ENDIF
      IF ::Font == 0
         ::Font := GetMessageFont()
      ENDIF
      SendMessage( ::Fhandle, WM_SETFONT, ::Font, 0 )
   ENDIF

   ::WMCreate()

   ::nProc := SetProcedure( ::Fhandle, HB_ObjMsgPtr( self, ::WndProc ), ::Msgs, self)
   ::Show( SW_SHOWNORMAL )


RETURN ::FHandle

*------------------------------------------------------------------------------*

METHOD Delete() CLASS TCustomControl
   local n
   IF( n := ascan( ::Parent:Controls, {|o|o:handle==::handle} ) )>0
      __objDelData( ::Parent, UPPER(::name ))
      adel( ::Parent:Controls, n, .t. )
      ::DestroyWindowHandle()
   ENDIF
   RETURN(self)

*------------------------------------------------------------------------------*

METHOD DelControl() CLASS TCustomControl
   local n
   IF( n := ascan( ::Parent:Controls, {|o|o:handle==::handle} ) )>0
      __objDelData( ::Parent, UPPER(::name ))
      adel( ::Parent:Controls, n, .t. )
      ::DestroyWindowHandle()
   ENDIF
   RETURN(self)

*------------------------------------------------------------------------------*

METHOD ControlProc( hWnd, nMsg, nwParam, nlParam ) CLASS TCustomControl
   LOCAL nret, cPaint, hDC, aRect, n, hdr
   Local cText


    Do Case

      CASE nMsg == WM_COMMAND
           nRet := ::WMCommand( nwParam, nlParam )


           IF nRet == NIL
              IF ( n := ASCAN( ::Controls, {|o| o:handle == nlParam } ) ) > 0
                 IF ValType( ::Controls[n]:OnClick ) == "B"
                    nRet := EVAL( ::Controls[n]:OnClick, ::Controls[n] )
                 ELSEIF ValType( ::Controls[n]:OnClick ) == "N"
                    nRet := HB_Exec( ::Controls[n]:OnClick, ::Controls[n] )
                 ENDIF

                 IF ValType( ::Controls[n]:OnChange ) == "B"
                    nRet := EVAL( ::Controls[n]:OnChange, ::Controls[n] )
                 ELSEIF ValType( ::Controls[n]:OnChange ) == "N"
                    nRet := HB_Exec( ::Controls[n]:OnChange, ::Controls[n] )
                 ENDIF


                 IF nRet != NIL
                    RETURN nRet
                 ENDIF

              ENDIF
           ENDIF

      CASE nMsg == WM_NOTIFY

           hdr IS NMHDR
           hdr:Pointer( nlParam )

           IF __ClsMsgAssigned( Self, "WMNotify" )
              nRet := ::WMNotify( hdr, nlParam )
           ELSEIF ( n := aScan( ::Controls, {|o| o:FHandle == hdr:hwndFrom } ) ) > 0
              nRet := ::Controls[n]:Notify( hdr, nlParam )
           ENDIF
      CASE nMsg == WM_CHAR
           nRet := ::WMChar( nwParam, nlParam )

           IF nRet == NIL
              IF ValType( ::Controls[n]:OnKeyPress ) == "B"
                 nRet := EVAL( ::Controls[n]:OnKeyPress, ::Controls[n],nwParam)
              ELSEIF ValType( ::Controls[n]:OnKeyPress ) == "N"
                 nRet := HB_Exec( ::Controls[n]:OnKeyPress, ::Controls[n],nwParam)
              ENDIF
           ENDIF


      CASE nMsg == WM_PAINT
           IF __ClsMsgAssigned( self, "WMPaint" )
              hDC  := BeginPaint( ::FHandle, @cPaint )
              nRet := ::WMPaint( hDC )
              EndPaint( ::FHandle, cPaint)
           ENDIF

      CASE nMsg == WM_DESTROY
           nRet := ::WMDestroy()

      CASE nMsg == WM_GETDLGCODE
           nRet := ::WMGetDlgCode()

      CASE nMsg == WM_HSCROLL
           nRet := ::WMHScroll( nwParam, nlParam )

      CASE nMsg == WM_KEYDOWN
           nRet := ::WMKeyDown( nwParam, nlParam )

      CASE nMsg == WM_KEYUP
           nRet := ::WMKeyUp( nwParam, nlParam )

      CASE nMsg == WM_KILLFOCUS
           nRet := ::WMKillFocus( nwParam )

      CASE nMsg == WM_NCHITTEST
           nRet := ::WMNCHitTest( nwParam, nlParam )

      CASE nMsg == WM_MOUSEMOVE
           nRet := ::WMMouseMove( nwParam, LoWord(nlParam), Hiword(nlparam) )

      CASE nMsg == WM_MOVE
           ::FLeft := LoWord( nlParam )
           ::FTop  := HiWord( nlParam )
           nRet := ::WMMove( LoWord( nlParam ), HiWord( nlParam ) )

      CASE nMsg == WM_SIZE
           aRect := ::WindowRect()
           ::Fwidth := aRect[3]-aRect[1]
           ::Fheight:= aRect[4]-aRect[2]
           nRet := ::WMSize( nwParam, LoWord(nlParam), Hiword(nlparam) )

      CASE nMsg == WM_SETFOCUS
           nRet := ::WMSetFocus( nwParam )

      CASE nMsg == WM_SETTEXT
           nRet := ::WMSetText( nwParam, nlParam )

      CASE nMsg ==   WM_GETTEXTLENGTH


           IF Valtype(::FText ) != "C"
              nRet :=  0
           ELSE
              nRet := Len(::FText)
           ENDIF


      CASE nMsg ==  WM_SETTEXT
           cText :=  nlParam
           ::fText := Nil
           ::fText := cText
           nRet := 1

      CASE nMsg == WM_GETTEXT
          IF ::FText != nil
             cText := ::FText
          ELSE
             cText := ''
          ENDIF

          nRet := Len( nlParam := Left( cText, nWParam - 1 ) )



      //For Text
   ENDCASE

   IF nRet != nil
      RETURN( nret )
   ENDIF

RETURN CallWindowProc( ::nProc, ::handle, nMsg, nwParam, nlParam )


METHOD Perform( nMsg, nwParam, nlParam ) CLASS TCustomControl
   LOCAL nRet
   nret := ::&( ::WndProc )( ::handle, nMsg, nwParam, nlParam )

RETURN nRet

// THintWindow ------------------------------------------------------------------------------
pragma pack(4)

IMPORT C STRUCTURE RECT
typedef struct tagTOOLINFO{;
    UINT      cbSize;
    UINT      uFlags;
    HWND      hwnd;
    UINT      uId;
    RECT      rect;
    HINSTANCE hinst;
    LPTSTR    lpszText;
    LPARAM lParam;
} TOOLINFO

CLASS THintWindow  FROM TCustomControl

   DATA FCaption INIT "ToolTip"
   DATA FLeft   PROTECTED  INIT CW_USEDEFAULT
   DATA FTop    PROTECTED  INIT CW_USEDEFAULT
   DATA FWidth  PROTECTED  INIT CW_USEDEFAULT
   DATA FHeight PROTECTED  INIT CW_USEDEFAULT

   DATA Style   INIT  WS_POPUP+TTS_ALWAYSTIP
   DATA ExStyle INIT  WS_EX_TOOLWINDOW

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_SIZE,WM_MOVE,WM_MOUSEMOVE}

   DATA Tip       PROTECTED

   DATA WinClass    PROTECTED INIT TOOLTIPS_CLASS
   DATA ControlName EXPORTED INIT "ToolTip"

   METHOD Create()
   METHOD WMCreate()
   METHOD TrackHint()
   METHOD Popup()                INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_POP, 0, 0 ), NIL )
   METHOD Activate(lAct)         INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_ACTIVATE, lAct, 0), NIL )
   METHOD SetTitle(nIcon,cTitle) INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_SETTITLE, nIcon, cTitle ), NIL )
   METHOD SetInitial(nmSec)      INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_SETDELAYTIME, TTDT_INITIAL,   nmSec ), NIL )
   METHOD SetAutoPop(nmSec)      INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_SETDELAYTIME, TTDT_AUTOPOP,   nmSec ), NIL )
   METHOD SetReShow(nmSec)       INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_SETDELAYTIME, TTDT_RESHOW,    nmSec ), NIL )
   METHOD SetAutomatic(nmSec)    INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_SETDELAYTIME, TTDT_AUTOMATIC, nmSec ), NIL )
   METHOD SetMaxTipWidth(nMax)   INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_SETMAXTIPWIDTH, 0, nMax ), NIL )
   METHOD TrackPosition(x,y)     INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_TRACKPOSITION , 0, MAKELONG( x, y ) ), NIL )
   METHOD Update()               INLINE IF( ::FHandle != NIL, ::SendMessage( TTM_UPDATE, 0, 0 ), NIL )
   METHOD SetText()
ENDCLASS

METHOD Create( oParent ) CLASS THintWindow

   ::Super:Create( oParent )
RETURN( self )

METHOD WMCreate() CLASS THintWindow
   local ti IS TOOLINFO
   SetWindowPos( ::handle, HWND_TOPMOST,0, 0, 0, 0, SWP_NOMOVE+SWP_NOSIZE+SWP_NOACTIVATE)

   ti:cbSize      := ti:sizeof()
   ti:uFlags      := TTF_SUBCLASS + TTF_IDISHWND
   ti:hwnd        := ::FParent:Handle

   ti:uId         := ::FParent:Handle
   ti:lpszText    := ::FCaption
   ti:rect:left   := ::FParent:FLeft
   ti:rect:top    := ::FParent:FTop
   ti:rect:right  := ::FParent:FWidth
   ti:rect:bottom := ::FParent:FHeight

   SendMessage( ::handle, TTM_ADDTOOL, 0, ti:value )
   ::Tip := ti
RETURN(nil)

METHOD TrackHint(n) CLASS THintWindow

   local ti IS TOOLINFO

   ti:cbSize      := ti:sizeof()
   ti:uFlags      := TTF_TRACK+TTF_ABSOLUTE + TTF_IDISHWND
   ti:hwnd        := ::FParent:Handle

   ti:uId         := ::FParent:Handle
   ti:lpszText    := ::FCaption
   ti:rect:left   := ::FParent:FLeft
   ti:rect:top    := ::FParent:FTop
   ti:rect:right  := ::FParent:FWidth
   ti:rect:bottom := ::FParent:FHeight

   SendMessage( ::handle, TTM_TRACKACTIVATE, n, ti:value )

RETURN Self

METHOD SetText(c) CLASS THintWindow
   local ti IS TOOLINFO

   ::FCaption := c

   ti:lpszText    := ::FCaption
   ti:cbSize      := ti:sizeof()
   ti:uFlags      := TTF_SUBCLASS + TTF_IDISHWND
   ti:hwnd        := ::FParent:Handle
   ti:uId         := ::FParent:Handle
   ti:lpszText    := ::FCaption
   ti:rect:left   := ::FParent:FLeft
   ti:rect:top    := ::FParent:FTop
   ti:rect:right  := ::FParent:FWidth
   ti:rect:bottom := ::FParent:FHeight

   SendMessage( ::handle, TTM_UPDATETIPTEXT , 0, ti:value )
RETURN(nil)

*-----------------------------------------------------------------------------*
// *** Not implemented yet ***.
#ifdef NOT_YET
CLASS TImageList FROM TComponent

  PROTECTED:
    METHOD   AssignTo                   // OVERRIDE
    METHOD   Change
    METHOD   DefineProperties           // OVERRIDE
    METHOD   DoDraw                        VIRTUAL
    METHOD   GetImages
    METHOD   HandleNeeded
    METHOD   Initialize                    VIRTUAL
  PUBLIC:
    METHOD   Create                        CONSTRUCTOR // OVERRIDE
    METHOD   CreateSize                    CONSTRUCTOR
    METHOD   Destroy                    // DESTRUCTOR OVERRIDE
    METHOD   Assign                     // OVERRIDE
    METHOD   Add
    METHOD   AddIcon
    METHOD   AddImages
    METHOD   AddMasked
    METHOD   Clear
    METHOD   Delete
    METHOD   Draw                       // OVERLOAD
    //METHOD   Draw(Canvas: TCanvas; X, Y, Index: Integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType; Enabled: Boolean = True); overload;
    METHOD   DrawOverlay                // OVERLOAD
    //METHOD   DrawOverlay(Canvas: TCanvas; X, Y: Integer; ImageIndex: Integer; Overlay: TOverlay; ADrawingStyle: TDrawingStyle; AImageType: TImageType; Enabled: Boolean = True); overload;
    METHOD   FileLoad
    METHOD   GetBitmap
    METHOD   GetHotSpot                    VIRTUAL
    METHOD   GetIcon                    // OVERLOAD
    //METHOD   GetIcon(Index: Integer; Image: TIcon; ADrawingStyle: TDrawingStyle; AImageType: TImageType); overload;
    METHOD   GetImageBitmap
    METHOD   GetMaskBitmap
    METHOD   GetResource
    METHOD   GetInstRes                 // OVERLOAD
    //METHOD   GetInstRes(Instance: THandle; ResType: TResType; ResID: DWORD; Width: Integer; LoadFlags: TLoadResources; MaskColor: TColor): Boolean; overload;
    METHOD   HandleAllocated
    METHOD   Insert
    METHOD   InsertIcon
    METHOD   InsertMasked
    METHOD   Move
    METHOD   Overlay
    METHOD   RegisterChanges
    METHOD   ResourceLoad
    METHOD   ResInstLoad
    METHOD   Replace
    METHOD   ReplaceIcon
    METHOD   ReplaceMasked
    METHOD   UnRegisterChanges
    PROPERTY Count             AS NUMERIC  READONLY
    PROPERTY Handle            //: HImageList;
  PUBLIC:
    PROPERTY AllocBy           AS NUMERIC  DEFAULT 4
    PROPERTY BlendColor                    //DEFAULT clNone
    PROPERTY BkColor                       //DEFAULT clNone
    PROPERTY DrawingStyle      TYPE TDrawingStyle  DEFAULT dsNormal
    PROPERTY Height            AS NUMERIC  DEFAULT 16
    PROPERTY ImageType                     DEFAULT itImage
    PROPERTY Masked            AS LOGICAL  DEFAULT TRUE
    PROPERTY ShareImages       AS LOGICAL  DEFAULT FALSE
    PROPERTY Width             AS NUMERIC  DEFAULT 16
    PROPERTY OnChange

ENDCLASS
#endif

*-----------------------------------------------------------------------------*

*-----------------------------------------------------------------------------*

