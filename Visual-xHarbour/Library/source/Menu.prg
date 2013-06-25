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
   METHOD Create()
   METHOD GetSubMenu()
   METHOD GetMenuByHandle()
   METHOD GetMenuById()
   METHOD CheckItem()
   METHOD DeleteItem()
   METHOD ODProc()
   METHOD GetItem()

   METHOD Set()             INLINE SetMenu( ::Parent:hWnd, ::hMenu )
   METHOD Destroy()         INLINE DestroyMenu( ::hMenu )
   METHOD DisableItem(nId ) INLINE ::EnableItem( nId, .F. )
   METHOD Context()
ENDCLASS

//-------------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Menu
   ::Children := {}
   ::aItems:= {}
   ::Parent:= oParent

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


//-------------------------------------------------------------------------------------------------------

CLASS MenuPopup FROM Menu
   METHOD Init()      CONSTRUCTOR
   METHOD Create()
ENDCLASS

METHOD Init( oParent ) CLASS MenuPopup
   ::Children := {}
   ::aItems:= {}
   ::Parent:= oParent
   ::__hObjects     := Hash()
   HSetCaseMatch( ::__hObjects, .F. )

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
CLASS MenuBar INHERIT Component
   DATA hMenu                  EXPORTED
   DATA __IdeContextMenuItems EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD __AddMenuItem()   
ENDCLASS

METHOD Init( oParent ) CLASS MenuBar
   DEFAULT ::__xCtrlName   TO "MenuBar"
   DEFAULT ::ComponentType TO "MenuBar"
   DEFAULT ::ClsName       TO "MenuBar"
   Super:Init( oParent )
   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "&Add MenuItem", {|| ::__AddMenuItem() } } }
   ENDIF
RETURN Self

METHOD Create() CLASS MenuBar
   LOCAL lpMenuInfo := (struct MENUINFO)

   ::hMenu := CreatePopupMenu()

   lpMenuInfo:cbSize := lpMenuInfo:SizeOf()
   lpMenuInfo:fMask  := MIM_STYLE
   lpMenuInfo:dwStyle:= MNS_NOTIFYBYPOS
   SetMenuInfo( ::hMenu, lpMenuInfo )

   SetMenu( ::Parent:hWnd, ::hMenu )
RETURN Self

METHOD __AddMenuItem() CLASS MenuBar
   ::Application:Project:SetAction( { { 1, 0, 0, 0, .T., Self, "MenuItem",,,1, {}, } }, ::Application:Project:aUndo )
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS ContextMenu INHERIT Component
   DATA Menu            EXPORTED
   DATA Text            EXPORTED INIT "ContextMenu"
   DATA xImageList      EXPORTED
   DATA __IdeContextMenuItems EXPORTED
   
   ACCESS ImageList     INLINE __ChkComponent( Self, ::xImageList ) PERSISTENT
   ASSIGN ImageList(o)  INLINE ::xImageList := o
   
   ACCESS Caption     INLINE ::Text
   ASSIGN Caption(c)  INLINE ::Text := c

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Show()
   METHOD __AddMenuItem()   
   METHOD Cancel()    INLINE ::Parent:SendMessage( WM_CANCELMODE, 0, 0 )
ENDCLASS

METHOD Init( oParent ) CLASS ContextMenu
   ::__hObjects     := Hash()
   HSetCaseMatch( ::__hObjects, .F. )

   DEFAULT ::__xCtrlName   TO "ContextMenu"
   DEFAULT ::ComponentType TO "ContextMenu"
   DEFAULT ::ClsName       TO "ContextMenu"
   ::Parent := oParent
   Super:Init( oParent )
   ::Parent := oParent
   ::Menu := MenuPopup( ::Owner )
   ::Menu:Style := TPM_LEFTALIGN | TPM_TOPALIGN
   IF ::__ClassInst != NIL
      ::Menu:Style := TPM_CENTERALIGN | TPM_LEFTBUTTON
      ::__IdeContextMenuItems := { { "&Add MenuItem", {|| ::__AddMenuItem() } } }
   ENDIF
   ::lCreated := .T.
RETURN Self

METHOD __AddMenuItem() CLASS ContextMenu
   ::Application:Project:SetAction( { { 1, 0, 0, 0, .T., Self, "CMenuItem",,,1, {}, } }, ::Application:Project:aUndo )
RETURN Self

METHOD Create() CLASS ContextMenu
   ::lCreated := .T.
RETURN Self

METHOD Show( x, y ) CLASS ContextMenu
   LOCAL nRes := 0, oItem

   ::Menu:Create()

   FOR EACH oItem IN ::Menu:aItems
       IF oItem:Visible .OR. ::__ClassInst != NIL
          oItem:Create()
       ENDIF
   NEXT

   ::Menu:Left := x
   ::Menu:Top  := y
   nRes := ::Menu:Context()
   ::Menu:Destroy()
RETURN nRes
