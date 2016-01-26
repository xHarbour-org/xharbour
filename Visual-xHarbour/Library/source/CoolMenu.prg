/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// CoolMenu.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

#define ATL_IDM_FIRST_MDICHILD 50000
#define IDM_MDI_BASE      (ATL_IDM_FIRST_MDICHILD - 5)
#define IDM_MDI_ICON      (IDM_MDI_BASE + 0)
#define IDM_MDI_GAP       (IDM_MDI_BASE + 1)
#define IDM_MDI_MINIMIZE  (IDM_MDI_BASE + 2)
#define IDM_MDI_RESTORE   (IDM_MDI_BASE + 3)
#define IDM_MDI_CLOSE     (IDM_MDI_BASE + 4)
#define IDM_MDI_FIRST     IDM_MDI_ICON
#define IDM_MDI_LAST      IDM_MDI_CLOSE
#define MBM_POPUPMENU  (WM_USER + 1801)

#define SM_CXSHADOW 4
#define DG_ADDCONTROL             1
//-------------------------------------------------------------------------------------------------------
CLASS CoolMenu INHERIT ToolBar
   PROPERTY HotImageList GET __ChkComponent( Self, @::xHotImageList )
   PROPERTY ImageList    GET __ChkComponent( Self, @::xImageList )

   DATA Editor          EXPORTED

   DATA hBackupColor    EXPORTED


   METHOD Init( oParent ) CONSTRUCTOR
   METHOD Create()
   METHOD GetItem()
   METHOD GetItemCount()      INLINE LEN( ::aItems )
   METHOD OnParentCommand()
   METHOD OnGetDlgCode() INLINE DLGC_WANTMESSAGE + DLGC_WANTCHARS + DLGC_WANTARROWS + DLGC_HASSETSEL
   METHOD OnMenuChar()
   METHOD OnSysChar()
   METHOD DeleteMenuItem()
   METHOD CheckMenuItem()
   METHOD EnableMenuItem()
   METHOD OnLButtonDown()
   METHOD OnParentSysCommand()
   METHOD OnSysKeyDown()
   METHOD OnSysKeyUp()
   METHOD OnDestroy() INLINE AEVAL( ::aItems, {|o| IIF( o != NIL, o:Delete(), ) } ), ::Super:OnDestroy(), Self
ENDCLASS

//-------------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS CoolMenu
   ::__xCtrlName        := "CoolMenu"
   ::Super:Init( oParent )
   IF ::DesignMode
      __SetInitialValues( Self, "ForeColor" )
   ENDIF
   ::Height             := 21
   ::List               := .T.
   ::__MenuBar          := .T.
   ::NoThemeCheck       := .T.
   ::HideClippedButtons := .T.
   ::__IsControl          := .F.
   ::SetStyle( CCS_ADJUSTABLE, .F. )
RETURN SELF


//-------------------------------------------------------------------------------------------------------

METHOD Create() CLASS CoolMenu
   ::hBackupColor := ::xForeColor
   DEFAULT ::hBackupColor TO GetSysColor( COLOR_BTNTEXT )
   ::Super:Create()

   ::Border          := 0
   ::SetBitmapSize( 0, ::Height-6)
   IF ( ::Application:OsVersion:dwMajorVersion >= 5 .AND. ::Theming )
      SetWindowTheme( ::hWnd, NIL, NIL )
   ENDIF

   IF ::DesignMode
      ::Application:ObjectTree:Set( Self )
      ::__IdeContextMenuItems := { { "&Add CoolMenuItem", {|| ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., Self, "CoolMenuItem",,,1, {}, } }, ::Application:Project:aUndo ) } } }
   ENDIF
RETURN SELF

//-------------------------------------------------------------------------------------------------------

METHOD GetItem( nId ) CLASS CoolMenu
   LOCAL oSubMenu, oMenu
   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:Menu:GetItem(nId) ) != NIL
         RETURN oMenu
      ENDIF
   NEXT
RETURN NIL

//-------------------------------------------------------------------------------------------------------

METHOD OnSysKeyDown( nwParam ) CLASS Coolmenu
   LOCAL n, nHot := SendMessage( ::hWnd, TB_GETHOTITEM, 0, 0 )

   // close the menu on ALT KEY y it is selected
   IF nwParam == VK_MENU .AND. nHot >= 0
      ::nPressed := -1
      SendMessage( ::hWnd, TB_SETHOTITEM, -1, 0 )
      RETURN 1
    ELSEIF nwParam != VK_MENU

      FOR n := 1 TO LEN( ::aItems )
         IF AT( "&"+UPPER( CHR( nwParam ) ), UPPER( ::aItems[n]:Caption ) ) > 0
            ::PrevFoc := GetFocus()
            ::nPressed := n-1
            PostMessage( ::hWnd, MBM_POPUPMENU)
            RETURN 0
         ENDIF
      NEXT

   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnSysKeyUp( nwParam, nlParam ) CLASS Coolmenu
   IF nwParam == VK_MENU
      PostMessage( ::Parent:hWnd, WM_SYSKEYUP, nwParam, nlParam )
      RETURN 0
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------

METHOD OnMenuChar( nwParam, nlParam ) CLASS CoolMenu
   LOCAL nItem, oMenu, n
   oMenu := ::oMenu:GetSubMenu( nlParam )
   nItem := -1
   IF oMenu != NIL
      FOR n := 1 TO LEN( oMenu:aItems )
         IF VALTYPE( oMenu:aItems[n]:Caption ) == "C"
            IF AT( "&"+UPPER( CHR( LoWord( nwParam ) ) ), UPPER( oMenu:aItems[n]:Caption ) ) > 0
               nItem := n-1
               EXIT
            ENDIF
         ENDIF
      NEXT
      IF nItem >= 0
         RETURN MAKELONG( nItem, MNC_EXECUTE )
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnSysChar( nwParam ) CLASS CoolMenu
   LOCAL n

   FOR n := 1 TO LEN( ::aItems )
      IF AT( "&"+UPPER( CHR( LoWord( nwParam ) ) ), UPPER( ::aItems[n]:Caption ) ) > 0
         ::PrevFoc := GetFocus()
         SendMessage( ::hWnd, TB_PRESSBUTTON, ::aItems[n]:Id, MAKELONG(1,0) )
         ::nPressed := n-1
         ::lKeyboard := .T.
         ::OpenPopup( ::hWnd )
         RETURN 0
      ENDIF
   NEXT
RETURN 0

//-----------------------------------------------------------------------------------------------

METHOD DeleteMenuItem( nId ) CLASS CoolMenu
   LOCAL oBtn
   FOR EACH oBtn IN ::aItems
       IF oBtn:Menu:DeleteItem( nId )
          EXIT
       ENDIF
   NEXT
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD CheckMenuItem( nId, lCheck ) CLASS CoolMenu
   LOCAL oBtn
   FOR EACH oBtn IN ::aItems
       IF oBtn:Menu:CheckItem( nId, lCheck )
          EXIT
       ENDIF
   NEXT
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD EnableMenuItem( nId, lEnable ) CLASS CoolMenu
   LOCAL oBtn
   FOR EACH oBtn IN ::aItems
       IF oBtn:Menu:EnableItem( nId, lEnable )
          EXIT
       ENDIF
   NEXT
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD OnLButtonDown( nwParam ) CLASS CoolMenu
   LOCAL n
   IF nwParam != -1
      SetFocus( ::hWnd )
      n := SendMessage( ::hWnd, TB_GETHOTITEM, 0, 0 )
    ELSE
      n := 0
      ::nPressed := -1
   ENDIF
   IF n >= 0
      IF ::Application:__hMenuHook == NIL
         ::nPressed := -1
      ENDIF

      IF LEN(::aItems[n+1]:Children ) > 0 .OR. n == 0
         ::nPressed  := n
         ::lKeyboard := .F.
         ::SelPopup  := .F.
         ::OpenPopup( ::hWnd )
         RETURN 0
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnParentSysCommand( nwParam,nlParam ) CLASS CoolMenu
   IF nwParam == SC_KEYMENU .AND. !CheckBit( GetKeyState( VK_SPACE ) , 32768 )
      ::PrevFoc := GetFocus()
      IF nlParam > 0
         PostMessage( ::hWnd, WM_SYSCHAR, nlParam, 0 ) // will receive TBN_HOTITEMCHANGE
         RETURN 0
      ENDIF
      ::Parent:SendMessage( WM_CANCELMODE, 0, 0 )
      //SetFocus( ::hWnd )
      ::nPressed := 0
      SendMessage( ::hWnd, TB_SETHOTITEM, 0, 0 )
      ::Application:__CurCoolMenu := Self
      RETURN 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnParentCommand( nId ) CLASS CoolMenu
   LOCAL oMdi
   IF ::Parent:MdiContainer
      oMdi := ::Parent:MDIClient:GetActive()
      IF oMdi != NIL
         DO CASE
            CASE nId == IDM_MDI_MINIMIZE
                 oMdi:Minimize()
                 RETURN 0

            CASE nId == IDM_MDI_RESTORE
                 oMdi:MdiRestore()
                 RETURN 0

            CASE nId == IDM_MDI_CLOSE
                 oMdi:MdiClose()
                 RETURN 0
         ENDCASE
         IF nID <= 2
            SendMessage( ::Parent:MDIClient:hWnd, IIF( nID == 2, WM_MDICASCADE, WM_MDITILE ), nID, 0 )
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

//-----------------------------------------------------------------------------------------------

CLASS CoolMenuItem INHERIT ToolButton
   PROPERTY MDIList     DEFAULT .F.

   DATA Index           EXPORTED
   DATA Item            EXPORTED
   DATA DropDown        EXPORTED
   DATA WholeDropDown   EXPORTED
   DATA ToolTip         EXPORTED
   DATA Components      EXPORTED INIT {}
   DATA SetChildren     EXPORTED INIT .T.
   DATA Font            EXPORTED
   DATA MixedButtons    EXPORTED
   DATA AutoSize        EXPORTED INIT .T.
   DATA CheckGroup      EXPORTED INIT .F.
   DATA ShowText        EXPORTED INIT .F.
   DATA Check           EXPORTED INIT .F.
   DATA Width           EXPORTED
   DATA ImageIndex      EXPORTED INIT -2
   DATA __Temprect      EXPORTED
   DATA TreeItem        EXPORTED

   DATA __hBrush        EXPORTED

   // compatibility
   ACCESS MDIMenu      INLINE ::MDIList
   ASSIGN MDIMenu(l)   INLINE ::MDIList := l

   METHOD Init() CONSTRUCTOR
   METHOD Create()
ENDCLASS

METHOD Init( oParent ) CLASS CoolMenuItem
   ::IsMenuItem   := .T.
   ::__IsControl  := .F.
   ::__xCtrlName  := "CoolMenuItem"
   ::Super:Init( oParent )
   ::Index        := LEN( ::Parent:aItems ) + 1
   ::ImageIndex   := 0
   IF oParent:DesignMode
      __SetInitialValues( Self )
   ENDIF
RETURN Self

METHOD Create( nPos ) CLASS CoolMenuItem
   LOCAL lpMenuInfo := (struct MENUINFO)

   DEFAULT ::xCaption TO ::Name
   DEFAULT nPos TO -1

   ::Super:Create(nPos)

   IF ::SetChildren
      IF nPos < 0
         AADD( ::Parent:Children, Self )
       ELSE
         AINS( ::Parent:Children, nPos+1, Self, .T. )
      ENDIF
   ENDIF

   IF ::Parent:Owner != NIL
      ::Parent:Owner:SetChevron()
   ENDIF

   IF ::hMenu == NIL
      ::hMenu := CreatePopupMenu()

      IF ::Parent:BackColor != NIL
         ::__hBrush := CreateSolidBrush( ::Parent:BackColor )
         VXH_SetMenuBackColor( ::hMenu, ::__hBrush )
      ENDIF
      //lpMenuInfo:cbSize := lpMenuInfo:SizeOf()
      //lpMenuInfo:fMask  := MIM_STYLE
      //lpMenuInfo:dwStyle:= MNS_NOTIFYBYPOS
      //SetMenuInfo( ::hMenu, lpMenuInfo )
   ENDIF
RETURN Self

