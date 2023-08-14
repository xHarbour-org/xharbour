/*
 * $Id$:
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// MenuItem.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
#include "debug.ch"
#Include "vxh.ch"

#define MENU_POPUPITEM     14

#define MPI_NORMAL          1
#define MPI_HOT             2
#define MPI_DISABLEDHOT     4
#define TMT_TEXTCOLOR               3803

#define MBM_ACCEL      (WM_USER + 1802)
//-------------------------------------------------------------------------------------------------------

CLASS MenuItem INHERIT Object
   PROPERTY ImageList   GET __ChkComponent( Self, @::xImageList, .F. )
   PROPERTY ImageIndex                        DEFAULT 0
   PROPERTY ShortCutKey
   PROPERTY Text        SET ::__SetText(v)
   PROPERTY Enabled     SET ::__SetEnabled(v) DEFAULT .T.
   PROPERTY Checked     SET ::__SetChecked(v) DEFAULT .F.
   PROPERTY RadioCheck                        DEFAULT .F.
   PROPERTY Separator                         DEFAULT .F.
   PROPERTY RightJustified                    DEFAULT .F.
   PROPERTY Visible     SET ::__SetVisible(v) DEFAULT .T.
   PROPERTY MDIList     DEFAULT .F.
   PROPERTY Alignment   DEFAULT 0

   ACCESS Caption      INLINE ::Text
   ASSIGN Caption(c)   INLINE ::Text := c

   DATA hMenu          EXPORTED
   DATA __lResizeable  EXPORTED INIT {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
   DATA __lMoveable    EXPORTED INIT .F.
   DATA __lCopyCut     EXPORTED INIT .F.
   DATA __TempRect     EXPORTED
   DATA Id             EXPORTED
   DATA xImageList     EXPORTED
   DATA __hBitmap      EXPORTED
   DATA __pObjPtr      EXPORTED
   DATA EnumAlignment  EXPORTED  INIT { { "Left", "Right" }, {0,MFT_RIGHTJUSTIFY} }

   DATA __mii          PROTECTED

   // compatibility
   ACCESS MDIMenu      INLINE ::MDIList
   ASSIGN MDIMenu(l)   INLINE ::MDIList := l

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD ReCreate()
   METHOD __AddMenuItem()
   METHOD __SetEnabled()
   METHOD __SetChecked()
   METHOD __ResetImageList()
   METHOD __SetVisible()
   METHOD __SetText()
   METHOD Destroy()
   METHOD Delete()        INLINE ::Destroy()
   METHOD GetMenuById()


   // Backward compatibility
   DATA HotImageList
   DATA ForeColor
   DATA ShortcutText
   METHOD Uncheck()       INLINE ::Checked := .F.
   METHOD Check()         INLINE ::Checked := .T.
   METHOD Enable()        INLINE ::Enabled := .T.
   METHOD Disable()       INLINE ::Enabled := .F.
   METHOD IsEnabled()     INLINE ::Enabled
   METHOD IsChecked()     INLINE ::Checked
   METHOD SetText(cText)  INLINE ::__SetText(cText)
ENDCLASS

METHOD Init( oParent ) CLASS MenuItem
   ::ClsName     := "MenuItem"
   ::__xCtrlName := "MenuItem"

   ::__IsControl  := .T.
   ::__IsStandard := .T.
   ::Parent       := oParent
   ::Id           := ::Form:ControlId++
   IF ::DesignMode
      __SetInitialValues( Self )
   ENDIF
   ::Events       := { ;
                        {"Mouse",     {;
                                      { "OnClick"     , "", "" } } } }
   DEFAULT ::EventHandler TO Hash()
   ::__CreateProperty()
   Super:Init( oParent )
   ::ShortCutKey  := __MenuStripItemShortCut( Self )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD __ResetImageList() CLASS MenuItem

   LOCAL oSubMenu, mii

   IF ::ImageList != NIL
      FOR EACH oSubMenu IN ::Children
          WITH OBJECT oSubMenu
             IF :ImageIndex > 0 .AND. ! :xChecked
                DEFAULT :__hBitmap TO ::ImageList:GetBitmap( :ImageIndex, GetSysColorBrush( COLOR_MENU ) )

                mii := (struct MENUITEMINFO)
                mii:cbSize   := mii:SizeOf()
                mii:fMask    := MIIM_BITMAP
                mii:hbmpItem := :__hBitmap //HBMMENU_CALLBACK
                SetMenuItemInfo( ::hMenu, :Id, .F., mii )
             ENDIF
          END
      NEXT
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD GetMenuById( nId ) CLASS MenuItem
   LOCAL oSubMenu, oMenu
   FOR EACH oSubMenu IN ::Children
       IF oSubMenu:Id == nId
          oMenu := oSubMenu
          EXIT
        ELSEIF ( oMenu := oSubMenu:GetMenuById( nId ) ) != NIL
          EXIT
       ENDIF
   NEXT
RETURN oMenu

//-------------------------------------------------------------------------------------------------------
METHOD ReCreate() CLASS MenuItem
   ::Create( .F. )
   AEVAL( ::Children, {|o| o:ReCreate() } )
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD Create( lAdd ) CLASS MenuItem
   LOCAL cShort, cText, mii
   DEFAULT lAdd TO .T.
   ::__mii := (struct MENUITEMINFO)

   IF ::Parent:hMenu == NIL
      ::Parent:hMenu := CreatePopupMenu()
   ENDIF

   IF ::Parent:ClsName == "MenuItem"
      ::__mii:cbSize     := ::__mii:SizeOf()
      ::__mii:hSubMenu   := ::Parent:hMenu
      ::__mii:fMask      := MIIM_SUBMENU
      SetMenuItemInfo( ::Parent:Parent:hMenu, ::Parent:Id, .F., ::__mii )
   ENDIF
   ::__mii := (struct MENUITEMINFO )
   ::__mii:cbSize  := ::__mii:SizeOf()
   ::__mii:wID     := ::Id
   ::__mii:fMask   := (MIIM_DATA | MIIM_ID | MIIM_STATE | MIIM_TYPE)
   ::__mii:fType   := MFT_STRING
   ::__mii:fState  := (IIF( ::xEnabled, MFS_ENABLED, MFS_DISABLED ) | IIF( ::xChecked, MFS_CHECKED, MFS_UNCHECKED ))

   IF ::RadioCheck
      ::__mii:fType := (::__mii:fType | MFT_RADIOCHECK)
   ENDIF
   IF ::RightJustified
      ::__mii:fType := (::__mii:fType | MFT_RIGHTJUSTIFY)
   ENDIF
   IF ::Separator .OR. ::Text == "-"
      ::__mii:fMask   := (MIIM_ID | MIIM_STATE | MIIM_TYPE)
      ::__mii:fType   := MFT_SEPARATOR
    ELSE
      IF ::DesignMode
         ::__IdeContextMenuItems := { { "&Add MenuItem", {|| ::__AddMenuItem() } } }
         ::Application:ObjectTree:Set( Self )
      ENDIF
      cText  := ::Text
      cShort := ::ShortCutKey:GetShortcutText()

      ::__mii:dwTypeData := cText + IIF( ! EMPTY(cShort), CHR(9) + cShort, "" )
      ::__mii:dwItemData := ::__pObjPtr := __ObjPtr( Self )
   ENDIF
   ::hMenu := CreateMenu()

   IF ::Alignment <> 0
      ::__mii:fType := (::__mii:fType | ::Alignment)
   ENDIF

   IF ::Visible
      InsertMenuItem( ::Parent:hMenu, -1, .T., ::__mii )
   ENDIF

   IF ::ImageIndex > 0 .AND. VALTYPE( ::Parent:ImageList ) == "O" .AND. ! ::xChecked
      ::__hBitmap := ::Parent:ImageList:GetBitmap( ::ImageIndex, GetSysColorBrush( COLOR_MENU ) )
   ENDIF
   IF ::__hBitmap != NIL
      mii := (struct MENUITEMINFO)
      mii:cbSize   := mii:SizeOf()
      mii:fMask    := MIIM_BITMAP
      mii:hbmpItem := ::__hBitmap //HBMMENU_CALLBACK
      SetMenuItemInfo( ::Parent:hMenu, ::Id, .F., mii )
   ENDIF
   IF VALTYPE( ::xImageList ) == "C"
      AADD( ::Form:__aPostCreateProc, { Self, "__ResetImageList" } )
   ENDIF
   ::ShortCutKey:SetAccel()
   IF lAdd
      AADD( ::Parent:Children, Self )
   ENDIF
RETURN NIL

METHOD __SetVisible( lVisible ) CLASS MenuItem
   LOCAL i, n, mii
   IF ::hMenu != NIL .AND. lVisible != ::xVisible
      n := ASCAN( ::Parent:Children, {|o| o:hMenu == ::hMenu } )
      IF lVisible
         FOR i := 1 TO n-1
             IF ! ::Parent:Children[i]:Visible
                n--
             ENDIF
         NEXT
         IF ! IsMenu( ::hMenu )
            ::hMenu := CreateMenu()
         ENDIF
         InsertMenuItem( ::Parent:hMenu, n-1, .T., ::__mii )

         mii := (struct MENUITEMINFO)
         mii:cbSize := mii:SizeOf()
         mii:fMask  := MIIM_STATE
         mii:fState := IIF( ::xChecked, MFS_CHECKED, MFS_UNCHECKED )

         IF ::ImageIndex > 0 .AND. VALTYPE( ::Parent:ImageList ) == "O" .AND. ! ::xChecked
            DEFAULT ::__hBitmap TO ::Parent:ImageList:GetBitmap( ::ImageIndex, GetSysColorBrush( COLOR_MENU ) )
            mii:fMask    := MIIM_BITMAP
            mii:hbmpItem := ::__hBitmap
         ENDIF

         IF LEN( ::Children ) > 0
            mii:fMask    := (mii:fMask | MIIM_SUBMENU)
            mii:hSubMenu := ::hMenu
         ENDIF

         SetMenuItemInfo( ::Parent:hMenu, ::Id, .F., mii )
         AEVAL( ::Children, {|o| o:Visible := lVisible } )

       ELSE
         DeleteMenu( ::Parent:hMenu, ::Parent:Children[n]:Id, MF_BYCOMMAND )
      ENDIF
   ENDIF
RETURN NIL

METHOD __SetChecked( lCheck ) CLASS MenuItem
   LOCAL mii
   IF ::__pObjPtr != NIL .AND. lCheck != ::xChecked
      mii := (struct MENUITEMINFO)
      mii:cbSize   := mii:SizeOf()
      mii:fMask    := (MIIM_STATE | MIIM_BITMAP)
      mii:hbmpItem := IIF( lCheck, NIL, ::__hBitmap )
      mii:fState   := IIF( lCheck, MFS_CHECKED, MFS_UNCHECKED )
      mii:fState   := (IIF( ::Enabled, MFS_ENABLED, MFS_DISABLED ) | IIF( lCheck, MFS_CHECKED, MFS_UNCHECKED ))
      SetMenuItemInfo( ::Parent:hMenu, ::Id, .F., mii )
   ENDIF
RETURN NIL

METHOD __SetEnabled( lEnabled ) CLASS MenuItem
   LOCAL mii
   IF ::__pObjPtr != NIL .AND. lEnabled != ::xEnabled
      mii := (struct MENUITEMINFO)
      mii:cbSize   := mii:SizeOf()
      mii:fMask    := (MIIM_STATE | MIIM_BITMAP)
      mii:hbmpItem := IIF( ::xChecked, NIL, ::__hBitmap )
      mii:fState   := (IIF( lEnabled, MFS_ENABLED, MFS_DISABLED ) | IIF( ::xChecked, MFS_CHECKED, MFS_UNCHECKED ) )
      SetMenuItemInfo( ::Parent:hMenu, ::Id, .F., mii )
   ENDIF
RETURN NIL

METHOD __SetText( cText ) CLASS MenuItem
   LOCAL cShort
   IF ::__mii != NIL
      cShort := ::ShortCutKey:GetShortcutText()
      ::__mii:dwTypeData := cText + IIF( ! EMPTY(cShort), CHR(9) + cShort, "" )
      SetMenuItemInfo( ::Parent:hMenu, ::Id, 0, ::__mii )
   ENDIF
RETURN SELF


METHOD Destroy() CLASS MenuItem
   LOCAL n
   FOR n := 1 TO LEN( ::Children )
       ::Children[n]:Destroy()
       n--
   NEXT
   IF ::__pObjPtr != NIL
      __ObjRelPtr( ::__pObjPtr )
   ENDIF
   IF ( n := ASCAN( ::Parent:Children, {|o| o:hMenu == ::hMenu } ) ) > 0
      ADEL( ::Parent:Children, n, .T. )
   ENDIF
   IF IsMenu( ::hMenu )
      DestroyMenu( ::hMenu )
   ENDIF
   IF ::__hBitmap != NIL
      DeleteObject( ::__hBitmap )
   ENDIF
   DeleteMenu( ::Parent:hMenu, ::Id, MF_BYCOMMAND )
RETURN Self

METHOD __AddMenuItem() CLASS MenuItem
   ::Application:Project:SetAction( { { 1, 0, 0, 0, .T., Self, "MenuItem",,,1, {}, } }, ::Application:Project:aUndo )
RETURN Self

// Backwards compatibility
CLASS CMenuItem INHERIT MenuItem
ENDCLASS
