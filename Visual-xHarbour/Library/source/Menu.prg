/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Menu.prg                                                                                             *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
static s_oItem

#include "debug.ch"
#include "vxh.ch"

#define DG_DELCONTROL      2

//-------------------------------------------------------------------------------------------------------

EXIT PROCEDURE __CleanMenu()
   s_oItem := NIL
RETURN


CLASS Menu INHERIT Object

   DATA aItems        EXPORTED
   DATA hMenu         EXPORTED
   DATA Parent        EXPORTED
   DATA Left          EXPORTED
   DATA Top           EXPORTED
   DATA Style         EXPORTED
   DATA CoolBar       EXPORTED INIT .F.
   DATA ImageBkColor  EXPORTED
   DATA ImageBkBorder EXPORTED INIT .T.
   DATA ThemeActive   EXPORTED INIT .F.
   DATA ParProc       PROTECTED
   DATA ClsName       EXPORTED INIT "Menu"
   DATA Children      EXPORTED
   DATA __ClassInst   EXPORTED
   DATA __PropFilter                 EXPORTED INIT {}
   //DATA __pCallBackPtr  EXPORTED
   ACCESS hWnd        INLINE ::hMenu
   DATA ImageList     EXPORTED
   DATA HotImageList  EXPORTED
   DATA Property      EXPORTED
   DATA ItemID        EXPORTED INIT 0

   METHOD Init()      CONSTRUCTOR
   //DESTRUCTOR MenuDest
   METHOD Create()
   METHOD Context()
   METHOD EnableItem()
   METHOD GetSubMenu()
   METHOD GetMenuByHandle()
   METHOD GetMenuById()
   METHOD AddMenuItem()
   METHOD CheckItem()
   METHOD DeleteItem()
   METHOD ODProc()
   METHOD GetItem()

   METHOD Set()             INLINE SetMenu( ::Parent:hWnd, ::hMenu )
   METHOD Destroy()         INLINE DestroyMenu( ::hMenu )
   METHOD DisableItem(nId ) INLINE ::EnableItem( nId, .F. )
ENDCLASS

//-------------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Menu
   ::Children := {}
   ::aItems:= {}
   ::Parent:= oParent
   ::Property     := Hash()
   HSetCaseMatch( ::Property, .F. )

   IF oParent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
   ENDIF
   ::ImageList    := ::Parent:ImageList
   ::HotImageList := ::Parent:HotImageList
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Create() CLASS Menu
   LOCAL Popup

   ::hMenu := CreateMenu()
   FOR EACH Popup IN ::aItems
       Popup:Create()
   NEXT
   SetMenu( ::Parent:hWnd, ::hMenu )
RETURN NIL

//-------------------------------------------------------------------------------------------------------

METHOD Context( hWnd, x, y ) CLASS Menu
    ::Application:oCurMenu := Self
    DEFAULT hWnd TO ::Parent:hWnd
    DEFAULT x    TO ::Left
    DEFAULT y    TO ::Top
    ::ItemID := 0
RETURN TrackPopupMenu( ::hMenu, ::Style, x, y, 0, hWnd )

//-------------------------------------------------------------------------------------------------------

METHOD OdProc( hWnd,nMsg,nwParam,nlParam ) CLASS Menu

   LOCAL mi
   LOCAL dis
   LOCAL oSub
   LOCAL oItem

   DO CASE
      CASE nMsg == WM_MEASUREITEM
           mi := (struct MEASUREITEMSTRUCT*) nlParam
           IF mi:CtlType == ODT_MENU
              FOR EACH oSub IN ::Parent:Menu:aItems
                 IF ( oItem := oSub:GetMenuById( mi:itemID ) )!= NIL
                    oItem:MeasureItem( mi, nlParam )
                    EXIT
                 ENDIF
              NEXT
              RETURN 1
           ENDIF

      CASE nMsg == WM_DRAWITEM
           dis := (struct DRAWITEMSTRUCT*) nlParam
           IF dis:CtlType == ODT_MENU
              FOR EACH oSub IN ::Parent:Menu:aItems
                 IF ( oItem := oSub:GetMenuById( dis:itemID ) )!= NIL
                    oItem:DrawItem( dis )
                    EXIT
                 ENDIF
              NEXT
              RETURN 1
           ENDIF

   ENDCASE

RETURN CallWindowProc(::ParProc,hWnd,nMsg,nwParam,nlParam)


//-----------------------------------------------------------------------------------------------------

METHOD GetSubMenu( hMenu ) CLASS Menu

   LOCAL oSubMenu
   LOCAL oMenu

   IF ::hMenu == hMenu
      RETURN Self
   ENDIF

   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetSubMenu( hMenu ) ) != NIL
         RETURN oMenu
      ENDIF
   NEXT

RETURN NIL

METHOD GetMenuById( nId ) CLASS Menu

   LOCAL oSubMenu
   LOCAL oMenu

   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetMenuById( nId ) ) != NIL
         RETURN oMenu
      ENDIF
   NEXT

RETURN NIL

METHOD GetMenuByHandle( hMenu ) CLASS Menu

   LOCAL oSubMenu
   LOCAL oMenu

   IF hMenu == ::hMenu
      RETURN Self
   ENDIF

   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetMenuByHandle( hMenu ) ) != NIL
         RETURN oMenu
      ENDIF
   NEXT

RETURN NIL

//-------------------------------------------------------------------------------------------------------

METHOD DeleteItem( nId ) CLASS Menu

   LOCAL oSubMenu
   LOCAL oMenu
   LOCAL lRet := .F.

   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetMenuById(nId) ) != NIL
         oMenu:Delete()
         lRet := .T.
         EXIT
      ENDIF
   NEXT

RETURN lRet


//-------------------------------------------------------------------------------------------------------

METHOD CheckItem( nId, lCheck )

   LOCAL oSubMenu
   LOCAL oMenu
   LOCAL lRet := .F.

   DEFAULT lCheck TO .T.

   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetMenuById(nId) ) != NIL
         oMenu:Check(lCheck)
         lRet := .T.
         EXIT
      ENDIF
   NEXT

RETURN lRet


//-------------------------------------------------------------------------------------------------------

METHOD EnableItem( nId, lEnable ) CLASS Menu

   LOCAL oSubMenu
   LOCAL oMenu
   LOCAL lRet := .F.

   DEFAULT lEnable TO .F.

   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetMenuById(nId) ) != NIL
         IF !lEnable
            oMenu:Disable()
         ELSE
            oMenu:Enable()
         ENDIF
         lRet := .T.
         EXIT
      ENDIF
   NEXT

RETURN lRet


//-----------------------------------------------------------------------------------------------------

METHOD GetItem( nId ) CLASS Menu

   LOCAL oSubMenu
   LOCAL oMenu

   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetMenuById(nId) ) != NIL
         RETURN oMenu
      ENDIF
   NEXT

RETURN NIL


//-----------------------------------------------------------------------------------------------------

METHOD AddMenuItem( cCaption, nId, bnAction, nImgInd, cToolTip, cMessage, cAccel, lRadio, lCheck, lDefault, nPos ) CLASS Menu

   LOCAL oItem := CMenuItem( Self,,,, nPos )
   DEFAULT lCheck   TO .F.
   DEFAULT lRadio   TO .F.
   DEFAULT lDefault TO .F.

   oItem:Id        := nId
   oItem:Caption   := cCaption
   oItem:ImageIndex:= nImgInd
   oItem:Action    := bnAction
   oItem:ShortCut  := cAccel
   oItem:RadioCheck:= lRadio
   oItem:Default   := lDefault
   oItem:ToolTip   := cToolTip
   oItem:Message   := cMessage
   IF lCheck
      oItem:Check()
   ENDIF
RETURN oItem


//-------------------------------------------------------------------------------------------------------

CLASS MenuPopup FROM Menu
   METHOD Init()      CONSTRUCTOR
   METHOD Create()
ENDCLASS

METHOD Init( oParent ) CLASS MenuPopup
   ::Children := {}
   ::aItems:= {}
   ::Parent:= oParent
   ::Property     := Hash()
   HSetCaseMatch( ::Property, .F. )

   IF oParent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
   ENDIF
   IF __ObjHasMsg( ::Parent, "ImageList" )
      ::ImageList    := ::Parent:ImageList
   ENDIF
   IF __ObjHasMsg( ::Parent, "HotImageList" )
      ::HotImageList := ::Parent:HotImageList
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Create() CLASS MenuPopup
   LOCAL lpMenuInfo := (struct MENUINFO)

   ::hMenu := CreatePopupMenu()

   lpMenuInfo:cbSize := lpMenuInfo:SizeOf()
   lpMenuInfo:fMask  := MIM_STYLE
   lpMenuInfo:dwStyle:= MNS_NOTIFYBYPOS
   SetMenuInfo( ::hMenu, lpMenuInfo )
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ContextMenu INHERIT Component
   DATA hMenuHook       EXPORTED
   DATA Parent          EXPORTED
   DATA Menu            EXPORTED
   DATA SelPopup        PROTECTED INIT .F.
   DATA MenuWnd         PROTECTED
   DATA Caption         EXPORTED INIT "ContextMenu"
   DATA Result          EXPORTED
   DATA xImageList      EXPORTED
   ACCESS ImageList     INLINE __ChkComponent( Self, ::xImageList ) PERSISTENT
   ASSIGN ImageList(o)  INLINE ::xImageList := o
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Show()
   METHOD MenuDesignHook()
   METHOD __AddNewItemMenuItem()
   METHOD AddMenuItem()
   METHOD Cancel()    INLINE ::Parent:SendMessage( WM_CANCELMODE, 0, 0 )
ENDCLASS

METHOD Init( oParent ) CLASS ContextMenu
   ::Property     := Hash()
   HSetCaseMatch( ::Property, .F. )

   DEFAULT ::__xCtrlName   TO "ContextMenu"
   DEFAULT ::ComponentType TO "ContextMenu"
   DEFAULT ::ClsName       TO "ContextMenu"
   Super:Init( oParent )
   ::Parent := oParent
   ::Menu := MenuPopup( ::Owner )
   ::Menu:Style := TPM_LEFTALIGN | TPM_TOPALIGN
   IF ::__ClassInst != NIL
      ::Menu:Style := TPM_CENTERALIGN | TPM_LEFTBUTTON
   ENDIF
   ::lCreated := .T.
RETURN Self

METHOD Create() CLASS ContextMenu
   ::lCreated := .T.
RETURN Self

METHOD Show( x, y ) CLASS ContextMenu
   LOCAL nRes := 0, oItem, rc, pt := (struct POINT)

   ::Menu:Create()

   FOR EACH oItem IN ::Menu:aItems
       IF oItem:Visible .OR. ::__ClassInst != NIL
          IF ::__ClassInst != NIL
             IF oItem:Caption != "[ Add New Item ]"
                oItem:CheckForDefaultItem()
             ENDIF
          ENDIF
          oItem:Create()
       ENDIF
   NEXT

   IF ::__ClassInst != NIL // For CONTEXT MENU
      IF ASCAN( ::Menu:aItems, {|o| o:Caption == "[ Add New Item ]" } ) == 0
         oItem := CMenuItem()
         oItem:GenerateMember := .F.
         oItem:Init( Self, .T. )
         oItem:Caption := "[ Add New Item ]"
         oItem:Font:Bold   := .T.
         oItem:Default := .T.
         oItem:Create()
      ENDIF
   ENDIF

   IF ::__ClassInst != NIL
      GetWindowRect( ::Application:MainForm:FormEditor1:hWnd, @rc )

      ::Menu:Left := ( rc:left + rc:right ) / 2
      ::Menu:Top  := ( rc:top + rc:bottom ) / 2

      ::hMenuHook := SetWindowsHookEx( WH_MSGFILTER, HB_ObjMsgPtr( Self, "MenuDesignHook" ), NIL, GetCurrentThreadId(), Self )
      nRes := ::Menu:Context()
      UnhookWindowsHookEx( ::hMenuHook )
    ELSE
      ::Menu:Left := x
      ::Menu:Top  := y
      nRes := ::Menu:Context()
   ENDIF
   DestroyMenu( ::Menu:hMenu )

RETURN nRes


METHOD MenuDesignHook(nCode,nwParam,nlParam) CLASS ContextMenu
   LOCAL o, nMenu, Item
   LOCAL pt := (struct POINT)
   LOCAL ms := (struct MSG*) nlParam

   static hSubMenu, nwp, hMenu, nMouse := 0

   DO CASE
      CASE ms:message == WM_MENUSELECT
           ::SelPopup:= HiWord( ms:wParam ) & MF_POPUP != 0

           hMenu := ms:lParam
           nMenu := 1

           hSubMenu := GetSubMenu( ms:lParam, LoWord( ms:wParam ) )
           nwp   := LoWord( ms:wParam )

           s_oItem := NIL

           IF ::Menu:aItems[nMenu]:Menu != NIL
              IF ::SelPopup
                 FOR EACH Item IN ::Menu:aItems[nMenu]:Menu:aItems
                     IF hSubMenu == Item:hMenu
                        o := Item
                        EXIT
                     ENDIF
                     IF ( o := Item:GetMenuByHandle( hSubMenu ) ) != NIL
                        EXIT
                     ENDIF
                 NEXT
               ELSE
                 o := ::Menu:aItems[nMenu]:Menu:GetItem( LoWord( ms:wParam ) )
              ENDIF
              IF o!= NIL
                 s_oItem := o
              ENDIF
           ENDIF

           IF s_oItem != NIL .AND. s_oItem:Caption != "[ Add New Item ]"
              ::Application:Project:CurrentForm:SelectControl( s_oItem, .F. )
              ::Application:ObjectTree:Set( s_oItem )
           ENDIF

      CASE ms:message == WM_LBUTTONDBLCLK
           RETURN 1

      CASE ms:message == WM_LBUTTONDOWN

      CASE ms:message == WM_LBUTTONUP
           IF s_oItem != NIL .AND. s_oItem:Caption == "[ Add New Item ]"
              ::__AddNewItemMenuItem( s_oItem:Parent )
              RETURN 1
           ENDIF

      CASE ms:message == WM_MOUSEMOVE
           //IF ms:wParam & MK_LBUTTON == 0
           //   RETURN 1
           //ENDIF

      CASE ms:message == WM_KEYDOWN
           DO CASE
              CASE ms:wParam == VK_RETURN
                   IF s_oItem != NIL
                      IF s_oItem:Caption != "[ Add New Item ]"
                         ::Application:Project:CurrentForm:SelectControl( s_oItem, .F. )
                       ELSE
                         ::__AddNewItemMenuItem( s_oItem:Parent )
                      ENDIF
                      RETURN 1
                   ENDIF

              CASE ms:wParam == VK_ESCAPE
                   SendMessage( ::Owner:hWnd, WM_CANCELMODE, 0, 0 )
                   SetFocus( ::Owner:hWnd )
                   RETURN 1

           ENDCASE
   ENDCASE

RETURN CallNextHookEx( ::hMenuHook, nCode, nwParam, nlParam)

METHOD AddMenuItem( cCaption, nId, bnAction, nImgInd, cToolTip, cMessage, cAccel, lRadio, lCheck, lDefault, nPos ) CLASS ContextMenu
   LOCAL oItem := CMenuItem( ::Menu,,,, nPos )
   DEFAULT lCheck   TO .F.
   DEFAULT lRadio   TO .F.
   DEFAULT lDefault TO .F.

   oItem:Id        := nId
   oItem:Caption   := cCaption
   oItem:ImageIndex:= nImgInd
   oItem:Action    := bnAction
   oItem:ShortCut  := cAccel
   oItem:RadioCheck:= lRadio
   oItem:Default   := lDefault
   oItem:ToolTip   := cToolTip
   oItem:Message   := cMessage
   IF lCheck
      oItem:Check()
   ENDIF
   oItem:Create()
RETURN oItem


METHOD __AddNewItemMenuItem( oParent ) CLASS ContextMenu
   LOCAL oItem, oSubItem

   oItem := CMenuItem( oParent )
   oItem:Position := LEN( oItem:Menu:aItems )-2

   oItem:Caption := oItem:Name

   oSubItem := CMenuItem()
   oSubItem:GenerateMember := .F.
   oSubItem:Init( oItem, .T. )

   oSubItem:Caption   := "[ Add New Item ]"
   oSubItem:Font:Bold := .T.
   oSubItem:Default   := .T.
   oSubItem:Create()

   oItem:Create()

   ::Application:ObjectManager:ActiveObject := oItem
   ::Application:ObjectManager:ResetProperties()

   ::Application:EventManager:ResetEvents()

   ::Application:Project:Modified := .T.
   ::Form:__lModified := .T.

RETURN Self
