/*
 * $Id:
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TMenu CLASS
 *
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

#include "winuser.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "classex.ch"
#include "debug.ch"
#include "wintypes.ch"
#include "cstruct.ch"

pragma pack(4)

typedef struct tagMENUINFO {;
  DWORD   cbSize;
  DWORD   fMask;
  DWORD   dwStyle;
  UINT    cyMax;
  HBRUSH  hbrBack;
  DWORD   dwContextHelpID;
  ULONG  dwMenuData;
} MENUINFO

*-----------------------------------------------------------------------------*

CLASS TMenu FROM TComponent

   DATA Popup  AS OBJECT
   DATA FOwnerDraw INIT .F.
   
   DATA aItems PROTECTED INIT {}

   PROPERTY Items        READ FItems DEFAULT {}
   PROPERTY OnChange     READ FOnChange
   PROPERTY Handle       READ GetHandle
   PROPERTY WindowHandle READ FWindowHandle WRITE SetWindowHandle
   PROPERTY OwnerDraw    READ FOwnerDraw
   PROPERTY Images       READ FImages

  
   METHOD Create() CONSTRUCTOR
   METHOD GetHandle()
   METHOD SetWindowHandle()
   METHOD UpdateImage()
   METHOD IsOwnerDraw()
   METHOD DeployMenu()
   METHOD AddPopUp()
   METHOD Set()
   METHOD GetItem(nId)
   METHOD GetPos(nId)
   METHOD Check(nId)   INLINE CheckMenuItem(  ::handle, nId, MF_BYCOMMAND + MF_CHECKED )
   METHOD UnCheck(nId) INLINE CheckMenuItem(  ::handle, nId, MF_BYCOMMAND + MF_UNCHECKED )
   METHOD Enable(nId)  INLINE EnableMenuItem( ::handle, nId, MF_BYCOMMAND + MF_ENABLED )
   METHOD Disable(nId) INLINE EnableMenuItem( ::handle, nId, MF_BYCOMMAND + MF_DISABLED )
   METHOD Gray(nId)    INLINE EnableMenuItem( ::handle, nId, MF_BYCOMMAND + MF_GRAYED )

ENDCLASS

METHOD SetWindowHandle( Value ) CLASS TMenu

  ::FWindowHandle := Value
  ::UpdateImage()
  
  IF ( Value <> 0 )
     IF ::FParentBiDiMode
//        ::ParentBiDiModeChanged()
     ELSE
//        ::AdjustBiDiBehavior()
     ENDIF
  ENDIF

RETURN NIL

METHOD UpdateImage() CLASS TMenu
/*
var
  Image: array[0..511] of Char;

  procedure BuildImage(Menu: HMENU);
  var
    P, ImageEnd: PChar;
    I, C: Integer;
    State: Word;
  begin
    C := GetMenuItemCount(Menu);
    P := Image;
    ImageEnd := @Image[SizeOf(Image) - 5];
    I := 0;
    while (I < C) and (P < ImageEnd) do
    begin
      DoGetMenuString(Menu, I, P, ImageEnd - P, MF_BYPOSITION);
      P := StrEnd(P);
      State := GetMenuState(Menu, I, MF_BYPOSITION);
      if State and MF_DISABLED <> 0 then P := StrECopy(P, '$');
      if State and MF_MENUBREAK <> 0 then P := StrECopy(P, '@');
      if State and MF_GRAYED <> 0 then P := StrECopy(P, '#');
      P := StrECopy(P, ';');
      Inc(I);
    end;
  end;
*/
RETURN NIL

*-----------------------------------------------------------------------------*

METHOD Create( oParent ) CLASS TMenu

  ::FItems := TMenuItem():Create( Self )
  ::FItems:FOnChange := HB_ObjMsgPtr( Self, "MenuChanged" )
  ::FItems:FMenu := Self

  //::FImageChangeLink := TChangeLink():Create
  //::FImageChangeLink:OnChange := HB_ObjMsgPtr( Self, "ImageListChange" )
  //::FParentBiDiMode := .T.

  ::Super:Create( oParent )

  //::ParentBiDiModeChanged()

Return Self

*-----------------------------------------------------------------------------*

METHOD AddPopUp( cText ) CLASS TMenu

   if ::Popup != NIL
      ::Popup:Add()
   endif

   ::PopUp := TPopup():New( self, cText)

   return(self)

*-----------------------------------------------------------------------------*

METHOD Set() CLASS TMenu

   if ::Popup != NIL
      ::Popup:Add()
   endif

   SetMenu( ::Parent:handle, ::handle )

   return( self )

*-----------------------------------------------------------------------------*

METHOD GetItem( nId ) CLASS TMenu

   local n := aScan( ::aItems,{|o|o:id == nId} )
   if n > 0
      return( ::aItems[n] )
   endif

   return( nil )

*-----------------------------------------------------------------------------*
METHOD GetPos( nId ) CLASS TMenu

Return( aScan( ::aItems,{|o|o:id == nId} ) )

METHOD GetHandle() CLASS TMenu
   LOCAL hMenu, lpMenuInfo
   IF ::FHandle == NIL
      lpMenuInfo IS MENUINFO
      
      lpMenuInfo:cbsize  := lpMenuInfo:SizeOf()
      lpMenuInfo:fMask   := MIM_STYLE
      lpMenuInfo:dwStyle := MNS_NOTIFYBYPOS
      
      hMenu := ::FItems:GetHandle()
      view hMenu, SetMenuInfo( hMenu, lpMenuInfo:value )
   ENDIF
RETURN hMenu


METHOD IsOwnerDraw() CLASS TMenu
   LOCAL Result 
   IF ::FOwnerDraw == NIL
      ::FOwnerDraw := .F.
   ENDIF
   Result := ( ::OwnerDraw .OR. ::Images != NIL )
RETURN Result


METHOD DeployMenu() CLASS TMenu
   LOCAL MenuItem, SubMenu
   FOR EACH MenuItem IN ::aItems
      MenuItem:AppendTo( ::FHandle )
      FOR EACH SubMenu IN MenuItem:aItems
         MenuItem:AppendTo( SubMenu:FMenu:FHandle )
      NEXT
   NEXT
RETURN NIL
