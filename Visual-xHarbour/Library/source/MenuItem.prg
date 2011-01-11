/*
 * $Id$
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

#define MBM_ACCEL      (WM_USER + 1802)
#define MENU_POPUPITEM 14
//-------------------------------------------------------------------------------------------------------

CLASS CMenuItem INHERIT Object

   DATA __lResizeable            EXPORTED INIT {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
   DATA __lMoveable              EXPORTED INIT .F.
   DATA __lCopyCut               EXPORTED INIT .F.
   DATA __IsControl              EXPORTED INIT .T.
   DATA __lCreateAfterChildren   EXPORTED INIT .F.
   DATA __IdeImageIndex          EXPORTED INIT 8
   DATA __TempRect               EXPORTED

   DATA Caption                  PUBLISHED
   DATA ImageIndex               PUBLISHED INIT -1
   DATA RadioCheck               PUBLISHED INIT .F.
   DATA ShortCutText             PUBLISHED
   DATA Font                     PUBLISHED
   DATA HotImageList             PUBLISHED
   DATA Message                  PUBLISHED
   DATA ShortCutKey              PUBLISHED

   DATA xImageList      EXPORTED
   ACCESS ImageList     INLINE __ChkComponent( Self, ::xImageList ) PERSISTENT
   ASSIGN ImageList(o)  INLINE ::xImageList := o

   DATA Visible                  PUBLISHED INIT .T.

   PROPERTY Enabled INDEX MFS_ENABLED INDEX1 MFS_DISABLED READ xEnabled WRITE SetStatus DEFAULT .T. PROTECTED

   DATA Id                       EXPORTED
   DATA Cargo                    EXPORTED
   DATA Action                   EXPORTED AS CODEBLOCK
   DATA Parent                   EXPORTED
   DATA Position                 EXPORTED INIT -1
   DATA Menu                     EXPORTED
   DATA hMenu                    EXPORTED
   DATA lInserted                EXPORTED INIT .F.
   DATA aItems                   EXPORTED
   DATA Style                    EXPORTED
   DATA MenuItemInfo             EXPORTED AS OBJECT
   DATA MenuBreak                EXPORTED INIT 0
   DATA Status                   EXPORTED INIT MFS_ENABLED
   DATA ShortCut                 EXPORTED //PUBLISHED INIT ""
   DATA Default                  EXPORTED INIT .F.
   DATA BkBrush                  EXPORTED INIT GetSysColorBrush( COLOR_MENU )
   DATA Type                     EXPORTED
   DATA Bitmap                   EXPORTED
   DATA ToolTip                  EXPORTED
   DATA BmpItem                  EXPORTED
   DATA Children                 EXPORTED
   DATA ClsName                  EXPORTED INIT "CMenuItem"
   DATA Left                     EXPORTED INIT 0
   DATA Top                      EXPORTED INIT 0
   DATA Width                    EXPORTED INIT 0
   DATA Height                   EXPORTED INIT 0
   DATA Events                   EXPORTED INIT {  {"General", { { "OnClick"       , "", "" } } } }

   DATA __IdeContextMenuItems    EXPORTED INIT {}
   DATA __PropFilter             EXPORTED INIT {}
   DATA Components               EXPORTED INIT {}
   DATA SetChildren              EXPORTED INIT .T.

   DATA EventHandler             EXPORTED
   DATA HorzScrollPos            EXPORTED INIT 0
   DATA VertScrollPos            EXPORTED INIT 0
   DATA ForeSysColor             EXPORTED INIT GetSysColor( COLOR_MENUTEXT )

   DATA xForeColor               EXPORTED
   ACCESS ForeColor              INLINE IIF( ::xForeColor == NIL, ::ForeSysColor, ::xForeColor ) PERSISTENT
   ASSIGN ForeColor( n )         INLINE ::xForeColor := n

   ACCESS hWnd                   INLINE ::hMenu

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD Enable()               INLINE ::Enabled := .T.
   METHOD Disable()              INLINE ::Enabled := .F.
   METHOD IsEnabled()            INLINE ::Enabled
   METHOD GetItem( nId )         INLINE ::GetMenuById( nId )
   METHOD UnCheck()              INLINE ::Check(.F.)
   METHOD IsChecked()            INLINE ::Status & MFS_CHECKED != 0
   METHOD SetDefaultItem()       INLINE ::Default :=.T., SetMenuDefaultItem( ::Menu:hMenu, ::Id, 0 )
   METHOD Select()               INLINE Self
   METHOD Destroy()              INLINE ::Delete()
   METHOD GetRectangle()         INLINE {0,0,0,0}
   METHOD GetClientRect()        INLINE {0,0,0,0}
   METHOD GetWindowRect()        INLINE {0,0,0,0}
   METHOD __OnParentSize()       INLINE 0
   METHOD InvalidateRect()       INLINE 0

   METHOD SetSubMenu()
   METHOD SetText()
   METHOD IsMenuItem()
   METHOD GetMenuById()
   METHOD GetMenuByKey()
   METHOD GetMenuByHandle()
   METHOD AddMenuItem()
   METHOD GetSubMenu()
   METHOD SetStatus()
   METHOD Check()
   METHOD Delete()
   METHOD MeasureItem()
   METHOD HiLite()
   METHOD DrawItem()
   METHOD DrawItemText()
   METHOD OnClick()  VIRTUAL
   METHOD CheckForDefaultItem()
   METHOD Cancel()
ENDCLASS

//-------------------------------------------------------------------------------------------------------

METHOD Init( oParent, lAdd, nPos ) CLASS CMenuItem
   LOCAL n, cProp, x

   DEFAULT lAdd TO .F.
   DEFAULT nPos TO -1
   DEFAULT ::__xCtrlName TO "CoolMenuItem"
   
   ::Position     := nPos
   ::Parent       := oParent
   ::ForeSysColor := GetSysColor( COLOR_MENUTEXT )

   IF oParent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      IF ::Parent:ClsName == "CMenuItem"
         ::Parent:__IdeImageIndex := 8
      ENDIF
   ENDIF

   ::ImageIndex   := -1
   ::Children     := {}
   ::aItems       := {}
   ::EventHandler := Hash()
   HSetCaseMatch( ::EventHandler, .F. )
   ::Menu         := IIF( ::Parent:ClsName == "CMenuItem", ::Parent, ::Parent:Menu )
   ::MenuItemInfo := (struct MENUITEMINFO)

   ::Font := Font()
   ::Font:Parent := Self
   ::Font:Create()
   
   ::__CreateProperty()

   IF ::Position > -1
      IF !lAdd
         aIns( oParent:Children, ::Position, Self, .T. )
      ENDIF
      n := ::Position
      IF ( x := ASCAN( ::Menu:aItems, {|o|o:Caption == "[ Add New Item ]"} ) ) > 0
         n++
      ENDIF
      aIns( ::Menu:aItems, n, Self, .T. )
     ELSE
      IF !lAdd
         aAdd( oParent:Children, Self )
      ENDIF
      aAdd( ::Menu:aItems, Self )
   ENDIF
   ::ShortCutKey   := __MenuStripItemShortCut( Self )
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Create() CLASS CMenuItem
   LOCAL oSubMenu, nId, xIcon, x, n, nAccel

   IF IsMenu( ::hMenu )
      DestroyMenu( ::hMenu )
   ENDIF

   ::hMenu := CreateMenu()

   DEFAULT ::Id TO ::Form:GetNextControlId()

   DEFAULT ::ImageList    TO ::Parent:ImageList
   DEFAULT ::HotImageList TO ::Parent:ImageList

   DEFAULT ::Caption to "-"
   DEFAULT ::ImageIndex TO -1

   ::MenuItemInfo:hSubMenu      := IIF( LEN( ::aItems ) > 0, ::hMenu, 0 )
   ::MenuItemInfo:cbSize        := ::MenuItemInfo:SizeOf()
   ::MenuItemInfo:fMask         := MIIM_CHECKMARKS | MIIM_DATA | MIIM_ID | MIIM_STATE | MIIM_SUBMENU | MIIM_TYPE
   ::MenuItemInfo:wID           := ::Id
   IF ::Caption == "-"
      ::MenuItemInfo:fType      := MFT_SEPARATOR
    ELSE
      ::MenuItemInfo:fType      := MFT_OWNERDRAW
   ENDIF
   ::MenuItemInfo:fType := ::MenuItemInfo:fType | ::MenuBreak
   IF ::RadioCheck
      ::MenuItemInfo:fType := ::MenuItemInfo:fType | MFT_RADIOCHECK
   ENDIF

   ::MenuItemInfo:fState        := ::Status
   ::MenuItemInfo:hbmpChecked   := 0
   ::MenuItemInfo:hbmpUnchecked := 0
   ::MenuItemInfo:dwTypeData    := ::Caption
   ::MenuItemInfo:hBmpItem      := ::BmpItem

   //::MenuItemInfo:dwItemData   := ARRAYPOINTER( Self ) //::Menu:hMenu

   n := -1//::Position
   IF n < 0
      IF ::__ClassInst != NIL .AND. ::Caption != "[ Add New Item ]"
         x := ASCAN( ::Menu:aItems, {|o| o:Caption == "[ Add New Item ]" } )
         n := GetMenuItemCount( ::Menu:hMenu )
         IF x <= n
            n--
         ENDIF
      ENDIF
   ENDIF
   InsertMenuItem( ::Menu:hMenu, n, .T., ::MenuItemInfo )
   //::Position := -1
   ::lInserted := .T.
   IF ::Default
      ::SetDefaultItem()
   ENDIF
   
   IF ::__ClassInst == NIL .AND. ::ShortCut != NIL
      ::Form:AddAccelerator( ::ShortCut[1], ::ShortCut[2], ::Id )
   ENDIF
   
   ::ShortCutKey:SetAccel()

   FOR EACH oSubMenu IN ::aItems
      IF IsMenu( oSubMenu:hMenu )
         DestroyMenu( oSubMenu:hMenu )
      ENDIF
      oSubMenu:Create()
   NEXT

   IF ::__ClassInst != NIL
      ::Application:ObjectTree:Set( Self )
   ENDIF

RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD Cancel() CLASS CMenuItem
   SendMessage( ::Form:hWnd, WM_CANCELMODE, 0, 0 )
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD HiLite( lHilite ) CLASS CMenuItem

   DEFAULT lHilite TO .T.

   IF lHilite
      ::Status := ::Status & NOT( MFS_UNHILITE )
      ::Status := ::Status | MFS_HILITE
   ELSE
      ::Status := ::Status & NOT( MFS_HILITE )
      ::Status := ::Status | MFS_UNHILITE
   ENDIF

   ::MenuItemInfo:fState := ::Status
   SetMenuItemInfo( ::Menu:hMenu, ::Id, 0, ::MenuItemInfo )

RETURN SELF

//-------------------------------------------------------------------------------------------------------
METHOD Check(lCheck) CLASS CMenuItem

   DEFAULT lCheck TO .T.

   IF lCheck
      ::Status := ::Status & NOT( MFS_UNCHECKED )
      ::Status := ::Status | MFS_CHECKED
   ELSE
      ::Status := ::Status & NOT( MFS_CHECKED )
      ::Status := ::Status | MFS_UNCHECKED
   ENDIF

   ::MenuItemInfo:fState := ::Status
   SetMenuItemInfo( ::Menu:hMenu, ::Id, 0, ::MenuItemInfo )

RETURN SELF


//-------------------------------------------------------------------------------------------------------

METHOD SetStatus( nStatus, nStatus1, lMode ) CLASS CMenuItem

   //TraceLog( nStatus, nstatus1, lMode, ::xEnabled )

   DEFAULT lMode TO .T.

   IF lMode
      ::Status := ::Status | nStatus
      ::Status := ::Status & NOT( nStatus1 )
   ELSE
      ::Status := ::Status & NOT( nStatus )
      ::Status := ::Status | nStatus1
   ENDIF

   ::MenuItemInfo:fState := ::Status

   SetMenuItemInfo( ::Menu:hMenu, ::Id, 0, ::MenuItemInfo )

RETURN Self


//-------------------------------------------------------------------------------------------------------

METHOD Delete() CLASS CMenuItem

   LOCAL n
   AEVAL( ::aItems, {|o| IIF( o == NIL, , o:Delete() ) } )
   DestroyMenu( ::hMenu )
   DeleteMenu( ::Menu:hMenu, ::Id, MF_BYCOMMAND )
   
   IF ( n := ASCAN( ::Menu:aItems, {|o|o:Id == ::Id} ) ) > 0
      aDel( ::Menu:aItems, n, .T. )
   ENDIF
   IF ( n := ASCAN( ::Parent:Children, {|o|o:Id == ::Id} ) ) > 0
      aDel( ::Parent:Children, n, .T. )
   ENDIF
   IF ::Name != NIL .AND. ::GenerateMember
      HDel( ::Form:Property, ::Name )
   ENDIF
   IF ::MenuItemInfo:dwItemData != NIL
      ReleaseArrayPointer( ::MenuItemInfo:dwItemData )
   ENDIF

RETURN SELF

//-------------------------------------------------------------------------------------------------------

METHOD SetText( cText ) CLASS CMenuItem
   ::Caption := ctext
   ::MenuItemInfo:dwTypeData := ::Caption
   SetMenuItemInfo( ::Menu:hMenu, ::Id, 0, ::MenuItemInfo )
RETURN SELF

//-----------------------------------------------------------------------------------------------------

METHOD SetSubMenu() CLASS CMenuItem
   ::MenuItemInfo:hSubMenu := ::hMenu
   SetMenuItemInfo( ::Menu:hMenu, ::Id, 0, ::MenuItemInfo )
RETURN NIL

//-----------------------------------------------------------------------------------------------------

METHOD IsMenuItem( nId ) CLASS CMenuItem
   LOCAL oSubMenu
   IF nId == ::Id
      RETURN .T.
   ENDIF
   FOR EACH oSubMenu IN ::aItems
      IF oSubMenu:IsMenuItem( nId )
         RETURN .T.
      ENDIF
   NEXT
RETURN .F.

//-----------------------------------------------------------------------------------------------------

METHOD CheckForDefaultItem() CLASS CMenuItem
   LOCAL oItem, Item, nId

   FOR EACH Item IN ::aItems
       IF Item:Caption != "[ Add New Item ]"
          Item:CheckForDefaultItem()
       ENDIF
   NEXT
   IF ASCAN( ::aItems, {|o| o:Caption == "[ Add New Item ]" } ) == 0
      oItem := CMenuItem()
      oItem:GenerateMember := .F.
      oItem:Init( Self, .T. )

      oItem:Caption := "[ Add New Item ]"
      oItem:Font:Bold   := .T.
      oItem:Default := .T.
      oItem:Create()
   ENDIF

RETURN NIL

//-----------------------------------------------------------------------------------------------------

METHOD GetMenuById( nId ) CLASS CMenuItem

   LOCAL oSubMenu
   LOCAL oMenu
   IF nId == ::Id
      RETURN Self
   ENDIF

   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetMenuById( nId ) ) != NIL
         RETURN oMenu
      ENDIF
   NEXT

RETURN NIL

//-----------------------------------------------------------------------------------------------------

METHOD GetMenuByHandle( hMenu ) CLASS CMenuItem
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

//-----------------------------------------------------------------------------------------------------

METHOD GetMenuByKey( nKey ) CLASS CMenuItem

   LOCAL oSubMenu
   LOCAL oMenu

   IF AT( "&"+CHR(nKey), ::Caption ) > 0
      RETURN Self
   ENDIF
   FOR EACH oSubMenu IN ::aItems
      IF ( oMenu:=oSubMenu:GetMenuByKey( nKey ) ) != NIL
         RETURN oMenu
      ENDIF
   NEXT

RETURN NIL


//-----------------------------------------------------------------------------------------------------

METHOD AddMenuItem( cCaption, nId, bnAction, nImgInd, cToolTip, cMessage, cAccel, lRadio, lCheck, lDefault ) CLASS CMenuItem

   LOCAL oItem := CMenuItem( Self )

   DEFAULT lCheck   TO FALSE
   DEFAULT lRadio   TO FALSE
   DEFAULT lDefault TO FALSE

   // assign values
   oItem:Id           := nId
   oItem:Caption      := cCaption
   oItem:ImageIndex   := nImgInd
   oItem:Action       := bnAction
   oItem:ShortCutText := cAccel
   oItem:RadioCheck   := lRadio
   oItem:Default      := lDefault
   oItem:ToolTip      := cToolTip
   oItem:Message      := cMessage

   IF lCheck
      oItem:Check()
   ENDIF

RETURN oItem


//-----------------------------------------------------------------------------------------------------

METHOD GetSubMenu( hMenu ) CLASS CMenuItem
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


//-------------------------------------------------------------------------------------------------------

METHOD MeasureItem( mi, nlParam ) CLASS CMenuItem
   LOCAL lSeparator, hOld, aRect, n, hDC, aPt, aExt
   LOCAL xIcon := 0
   LOCAL yIcon := 0
   lSeparator  := ::MenuItemInfo:fType & MFT_SEPARATOR != 0

   hDC   := CreateCompatibleDC()
   aRect := {0,0,0,0}

   IF ::Font:Handle != NIL
      hOld := SelectObject( hDC, ::Font:Handle )
    ELSE
      SelectObject( hDC, ::Form:Font:Handle )
   ENDIF
   IF ::Caption != NIL
      aExt := _GetTextExtentPoint32( hDC, ::Caption + IIF( ::ShortCutText != NIL, SPACE(3)+::ShortCutText,"") )
      _DrawText( hDC, ::Caption + IIF( ::ShortCutText != NIL, SPACE(3)+::ShortCutText,""), @aRect, DT_SINGLELINE + DT_CALCRECT )
      aRect[3] := aExt[1]+35
      aRect[4] := aExt[2]+((aExt[2]*10)/100)
   ENDIF
   IF ::Font:Handle != NIL
      SelectObject( hDC, hOld )
   ENDIF

   DeleteDC( hDC )
   IF ::Parent:ImageList != NIL .AND. ::ImageIndex >= 0
      xIcon := ::Parent:ImageList:IconWidth
      yIcon := ::Parent:ImageList:IconHeight
   ENDIF

   xIcon := MAX( xIcon, GetSystemMetrics( SM_CXMENUCHECK )+2 )+6
   yIcon := MAX( 2+yIcon+2, GetSystemMetrics( SM_CYMENUCHECK ) )

   IF ::Caption == NIL
      xIcon -= 6
   ENDIF

   aRect[4] := MAX( aRect[4], yIcon )

   mi:itemWidth := xIcon+aRect[3]-IIF( ::Caption != NIL, GetSystemMetrics( SM_CXMENUCHECK ), 0 )//+6

   IF ::Style != NIL
      IF ::Style & BTNS_DROPDOWN > 0
         mi:itemWidth += GetSystemMetrics( SM_CXMENUCHECK )+4
      ENDIF
   ENDIF

   mi:itemHeight:= MAX(aRect[4], GetSystemMetrics( SM_CYMENU ) )
   IF lSeparator
      mi:itemHeight := 8
   ENDIF

   mi:CopyTo( nlParam )
RETURN 1

//-------------------------------------------------------------------------------------------------------

METHOD DrawItem( dis, l3D ) CLASS CMenuItem
   LOCAL lSeparator, xIcon:=0, yIcon:=0, aRect, hTheme
   LOCAL hDC, hPen, nIcon, nOff, lDisabled, lRadio, lSelected, lChecked, hBrush, nTop, lCheck := .F., nWidth, oItem, x

   DEFAULT l3D TO .F.

   hDC   := dis:hDC
   aRect := { dis:rcItem:Left,dis:rcItem:Top,dis:rcItem:Right,dis:rcItem:Bottom}
   lSeparator := ::MenuItemInfo:fType & MFT_SEPARATOR != 0

   IF !lSeparator .AND. ::Parent:ImageList != NIL .AND. ::ImageIndex >= 0
      xIcon := ::Parent:ImageList:IconWidth
      yIcon := ::Parent:ImageList:IconHeight
   ENDIF

   nIcon := xIcon
   xIcon := MAX( xIcon, GetSystemMetrics( SM_CXMENUCHECK )+2 )//+6
   yIcon := MAX( yIcon, GetSystemMetrics( SM_CYMENUCHECK ) )

   nWidth := xIcon
   IF ::Parent:ImageList != NIL
      FOR EACH oItem IN ::Menu:aItems
          IF oItem:ImageIndex >= 0
             nWidth := MAX( nWidth, ::Parent:ImageList:IconWidth )
          ENDIF
      NEXT
   ENDIF
   nWidth += 3


   IF nIcon == 0
      nIcon := xIcon
   ENDIF
   nOff := Int(xIcon/2)-Int(nIcon/2)

   aRect[3] := xIcon + aRect[3]

   aRect[4] := MAX( aRect[4], 2+yIcon+2 )
   aRect[4] := MAX( aRect[4], GetSystemMetrics( SM_CYMENU ) )

   lDisabled  := dis:itemState & ODS_GRAYED != 0
   lSelected  := dis:itemState & ODS_SELECTED != 0
   lChecked   := dis:itemState & ODS_CHECKED != 0
   lRadio     := ::MenuItemInfo:fType & MFT_RADIOCHECK != 0

   IF lSelected //.AND. !lDisabled
      IF !l3D
         SetTextColor( hDC, GetSysColor( COLOR_HIGHLIGHTTEXT ) )
         _FillRect( hDC, { aRect[1],    aRect[2],  aRect[3]-(xIcon),   aRect[4]  }, GetSysColorBrush( COLOR_HIGHLIGHT ) )
         _FillRect( hDC, { aRect[1]+ 1, aRect[2]+1,aRect[3]-(xIcon+1), aRect[4]-1}, GetSysColorBrush( IIF( ::Application:IsThemedXP, 29, COLOR_HIGHLIGHT ) ) )

         //hTheme := OpenThemeData(, ToUnicode( "MENU" ) )
         //DrawThemeBackground( hTheme, hDC, 16, dis:itemState, { aRect[1]+ 1, aRect[2]+1,aRect[3]-(xIcon+1), aRect[4]-1}, { aRect[1]+ 1, aRect[2]+1,aRect[3]-(xIcon+1), aRect[4]-1} )
         //CloseThemeData( hTheme )

       ELSE
         aRect[3]:=aRect[3]-xIcon
         _DrawEdge( hDC, aRect, IIF( dis:itemAction==ODA_DRAWENTIRE .AND. !lCheck, BDR_SUNKENOUTER, BDR_RAISEDINNER), BF_RECT )
      ENDIF
    ELSE
      SetTextColor( hDC, ::ForeColor )
      hBrush := ::BkBrush
      _FillRect( hDC, aRect, hBrush )
      IF lSeparator
         _DrawEdge( hDC, { dis:rcItem:Left+2, aRect[2]+3,dis:rcItem:Right-2,aRect[4]-3}, BDR_SUNKENOUTER, BF_RECT )
         RETURN 1
      ENDIF
   ENDIF

   nTop := aRect[2] + ( ( (aRect[4]-aRect[2]) - yIcon) / 2 )
   SetBkMode( hDC, 1 )

   IF lChecked
      SetBkColor( hDC, GetSysColorBrush( COLOR_BTNFACE ) )
      __DrawSpecialChar( hDC, { 2+nOff, aRect[2], xIcon, aRect[4]}, IIF( lRadio, 105, 98 ), .F. )
    ELSEIF ::Parent:ImageList != NIL .AND. ::ImageIndex >= 0 .AND. dis:itemState & ODS_CHECKED==0
      IF dis:itemState & ODS_DISABLED != 0
         IF !::Application:IsThemedXP
            ::Parent:ImageList:DrawDisabled( hDC, ::ImageIndex, 2+nOff, nTop )
          ELSE
            ::Parent:ImageList:DrawDisabled( hDC, ::ImageIndex, 2+nOff, nTop, GetSysColorBrush( IIF( ::Application:IsThemedXP, 29, COLOR_HIGHLIGHT ) ) ) //GetSysColorBrush( COLOR_GRAYTEXT ) )
         ENDIF
       ELSE
         x := 0
         ::Parent:ImageList:DrawImage( hDC, ::ImageIndex, 1+nOff - x, nTop - x, ILD_TRANSPARENT )
      ENDIF

   ENDIF

   aRect[1]:=nWidth + 2 //2+xIcon+4+nOff

   ::DrawItemText( hDC, dis, aRect, lDisabled, nOff, xIcon )

RETURN aRect


//-------------------------------------------------------------------------------------------------------

METHOD DrawItemText( hDC, dis, aRect, lDisabled, nOff, xIcon ) CLASS CMenuItem

   LOCAL hOld
   LOCAL aAccRect, aAlign, x, y

   IF ::Font:Handle != NIL
      hOld := SelectObject( hDC, ::Font:Handle )
   ENDIF

   IF lDisabled
      IF !::Application:IsThemedXP //.AND. !::Theming
         _OffsetRect( @aRect, 1, 1 )
         SetTextColor( hDC, GetSysColor( COLOR_3DHIGHLIGHT ) )
         IF ::ShortCutText != NIL
            _DrawText( hDC, ::ShortCutText, {aRect[1]+1,aRect[2],dis:rcItem:Left+dis:rcItem:Right-5-IIF( LEN( ::aItems ) > 0, GetSystemMetrics( SM_CXMENUCHECK ), 0 )+1,aRect[4]}, DT_RIGHT + DT_SINGLELINE + DT_VCENTER  )
         ENDIF
         _DrawText( hDC, ::Caption, @aRect, DT_SINGLELINE + DT_VCENTER )
         _OffsetRect( @aRect, -1, -1 )
      ENDIF
      SetTextColor( hDC, GetSysColor( COLOR_GRAYTEXT ) )
   ENDIF

   IF ::Id == SC_RESTORE
      __DrawSpecialChar( hDC, { 2+nOff, aRect[2], xIcon, aRect[4]}, 50, .F. )
    ELSEIF ::Id == SC_CLOSE
      __DrawSpecialChar( hDC, { 2+nOff, aRect[2], xIcon, aRect[4]}, 114, .F. )
    ELSEIF ::Id == SC_MINIMIZE
      __DrawSpecialChar( hDC, { 2+nOff, aRect[2], xIcon, aRect[4]}, 48, .F. )
    ELSEIF ::Id == SC_MAXIMIZE
      __DrawSpecialChar( hDC, { 2+nOff, aRect[2], xIcon, aRect[4]}, 49, .F. )
   ENDIF

   IF ::ShortCutText != NIL
      aAccRect := {aRect[1],aRect[2],dis:rcItem:Left+dis:rcItem:Right-5-IIF( LEN( ::aItems ) > 0, GetSystemMetrics( SM_CXMENUCHECK ), 0 ),aRect[4]}
      _DrawText( hDC, ::ShortCutText, aAccRect, DT_RIGHT + DT_SINGLELINE + DT_VCENTER  )
   ENDIF

   _DrawText( hDC, ::Caption, aRect, DT_SINGLELINE | DT_VCENTER )

   IF ::Font:Handle != NIL
      SelectObject( hDC, hOld )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
