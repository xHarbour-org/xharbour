/*
 * $Id: TMenuItem.prg,v 1.15 2003/01/27 09:34:17 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TMenuItem CLASS
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
#include "debug.ch"
#include "classex.ch"
#include "wintypes.ch"
#include "cstruct.ch"

STATIC Checks     := { MF_UNCHECKED, MF_CHECKED }
STATIC Enables    := { MF_DISABLED + MF_GRAYED, MF_ENABLED }
STATIC Breaks     := { 0, MF_MENUBREAK, MF_MENUBARBREAK }
STATIC Separators := { MF_STRING, MF_SEPARATOR }


pragma pack(4)

IMPORT C STRUCTURE MENUITEMINFO

CLASS TMenuItem FROM TComponent

   CLASSDATA   FCommandID HIDDEN INIT 0

   PROPERTY Handle READ GetHandle

   PROPERTY Items READ FItems WRITE SetItems DEFAULT {}
   PROPERTY Count READ GetCount

   PROPERTY Visible READ FVisible WRITE SetVisible DEFAULT .T.

   PROPERTY Caption READ FCaption WRITE SetCaption

   PROPERTY OnChange READ FOnChange

   DATA FMenu    AS OBJECT PRIVATE
   DATA Id
   DATA Action  AS CODEBLOCK
   DATA Checked
   DATA FParent
   DATA FBitmap

   DATA FRadioItem INIT .F.
   DATA FBreak     INIT .F.
   DATA FChecked   INIT .F.
   DATA FEnabled   INIT .T.
   DATA FDefault   INIT .F.

   DATA PopUp
   DATA aItems PROTECTED INIT {}
   
   PROPERTY Enabled    READ FEnabled WRITE SetEnabled
   PROPERTY Command    READ FCommand WRITE SetCommand
   PROPERTY ImageIndex READ FImageIndex WRITE SetImageIndex
   
   PROPERTY Parent READ FParent
   
   DATA MenuRightToLeft INIT .F.

   DATA MenuItemInfo AS OBJECT PROTECTED

   ACCESS Checked     INLINE ::MenuSet( MF_CHECKED, MF_UNCHECKED)
   ACCESS Disabled    INLINE ::MenuSet( MF_DISABLED, MF_ENABLED)
   ACCESS Enabled     INLINE ::MenuSet( MF_ENABLED, MF_DISABLED)
   ACCESS Grayed      INLINE ::MenuSet( MF_GRAYED, MF_ENABLED)

   METHOD Create() CONSTRUCTOR
   METHOD MenuSet()
   METHOD AppendTo( HMenu, bRightToLeft )
   METHOD GetHandle
   METHOD UniqueCommand  INLINE ::FCommandID++
//   METHOD PopulateMenu() INLINE  ::AppendTo( ::FHandle, ::MenuRightToLeft )
   METHOD GetCount()     INLINE Len( ::Items )
   METHOD MenuChanged( Rebuild )
   METHOD RebuildHandle()
   METHOD PopulateMenu()

   METHOD SetEnabled()
   METHOD SetVisible()
   METHOD SetImageIndex()
   METHOD SetCaption( Value )
   METHOD SetCommand( Value )
   METHOD GetParentMenu()
   METHOD SetParentComponent( Value )
   METHOD Add()

   METHOD Insert()

ENDCLASS

*-----------------------------------------------------------------------------*
METHOD Create( oMenu ) CLASS TMenuItem

   ::Super:Create( oMenu )

   ::FVisible := .T.
   ::FCommand := ::UniqueCommand()
   ::FImageIndex := -1

   ::FMenu  := oMenu
   ::FRadioItem := .F.
   ::FBreak     := .F.
   ::FChecked   := .F.
   ::FEnabled   := .T.
   ::FDefault   := .F.
   ::MenuItemInfo IS MENUITEMINFO

Return Self

*-----------------------------------------------------------------------------*
METHOD MenuSet( nTest, nOther) CLASS TMenuItem

   LOCAL nMask := Or(nTest, nOther)

Return AND(GetMenuState(::oMenu:Fhandle, ::Id, MF_BYCOMMAND), nMask) == nTest

METHOD AppendTo( HMenu, ARightToLeft ) CLASS TMenuItem

  LOCAL IBreaks     := { MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK }
  LOCAL IChecks     := { MFS_UNCHECKED, MFS_CHECKED }
  LOCAL IDefaults   := { 0, MFS_DEFAULT }
  LOCAL IEnables    := { MFS_GRAYED, MFS_ENABLED }
  LOCAL IRadios     := { MFT_STRING, MFT_RADIOCHECK }
  LOCAL ISeparators := { MFT_STRING, MFT_SEPARATOR }
  LOCAL IRTL        := { 0, MFT_RIGHTORDER + MFT_RIGHTJUSTIFY }
  LOCAL IOwnerDraw  := { MFT_STRING, MFT_OWNERDRAW }

  LOCAL Caption
  LOCAL NewFlags
  LOCAL IsOwnerDraw
  LOCAL ParentMenu
  
  DEFAULT ARightToLeft TO .F.
  
  IF ::FVisible
     Caption := ::FCaption

     ::MenuItemInfo:hSubMenu := 0

     DEFAULT Caption TO "?"

     ::SetParentComponent( Self )
    
     ::MenuItemInfo:cbSize := ::MenuItemInfo:SizeOf() //44 // Required for Windows 95
     ::MenuItemInfo:fMask := MIIM_CHECKMARKS + MIIM_DATA + MIIM_ID + MIIM_STATE + MIIM_SUBMENU + MIIM_TYPE
     ParentMenu := ::GetParentMenu()
     IsOwnerDraw := .F. //ParentMenu != NIL .AND. Or( ParentMenu:IsOwnerDraw, ::FBitmap != NIL ) > 0 .AND. ! ::FBitmap:Empty
     
     
     ::MenuItemInfo:fType :=IRadios[ IIF( ::FRadioItem, 2, 1 ) ] + IBreaks[ IIF( ::FBreak, 2, 1 ) ] + ;
                            ISeparators[ IIF( ::FCaption == '-', 2, 1 ) ] + IRTL[ IIF( ARightToLeft, 2, 1 ) ] + ;
                            IOwnerDraw[ IIF( IsOwnerDraw, 2, 1 ) ]


     ::MenuItemInfo:fState        := IChecks[ IIF( ::FChecked, 2, 1 ) ] + IEnables[ IIF( ::FEnabled, 2, 1 ) ] + IDefaults[ IIF( ::FDefault, 2, 1 ) ]
     ::MenuItemInfo:wID           := ::FCommand
     ::MenuItemInfo:hbmpChecked   := 0
     ::MenuItemInfo:hbmpUnchecked := 0
     ::MenuItemInfo:dwTypeData    := Caption

     IF ::GetCount > 0
        ::MenuItemInfo:hSubMenu := ::GetHandle()
     ENDIF

     InsertMenuItem( HMenu, -1, .T., ::MenuItemInfo:value )

     AADD( ::FMenu:aItems, Self )
     
  ENDIF

Return NIL

METHOD GetHandle() CLASS TMenuItem

   IF ::FHandle == NIL
      IF ::Owner:ClassName == "TPOPUPMENU"
         ::FHandle := CreatePopupMenu()
      ELSE
         ::FHandle := CreateMenu()
      ENDIF
//      ::PopulateMenu()
   ENDIF

Return ::FHandle

METHOD PopulateMenu() CLASS TMenuItem
/*
   LOCAL MenuItem

   FOR EACH MenuItem IN ::FMenu:aItems
      view MenuItem:Caption
      MenuItem:AppendTo( MenuItem:FMenu:Handle )
   NEXT
*/
Return NIL

METHOD Insert( Index, Item ) CLASS TMenuItem

  IF Item:FOwner <> NIL
    //raise EMenuError.Create(SMenuReinserted);
  ENDIF

  IF ::FItems == NIL
     ::FItems := {}
  ENDIF
  IF ( Index >= 0 ) .AND. ( Index < Len( ::FItems ) )
    IF Item:GroupIndex < ::FItems[ Index]:GroupIndex
      Item:GroupIndex := ::FItems[Index]:GroupIndex
    ENDIF
  ENDIF

  //::VerifyGroupIndex( Index, Item.GroupIndex )

  aIns( ::FItems, Index, Item, .T. )
  Item:FOwner := Self
  Item:FOnChange := HB_ObjMsgPtr( Self, "SubItemChanged" )

  IF ::FHandle <> NIL
     ::RebuildHandle()
  ENDIF

  ::MenuChanged( ::Count == 1 )

Return NIL

METHOD MenuChanged( Rebuild ) CLASS TMenuItem

   LOCAL Source

   IF ( ::Owner == NIL ) .AND. ( ::Owner:ClassName == "TMENU" )
      //Source := NIL
   ELSE
      Source := Self
   ENDIF

   IF ValType( ::FOnChange ) == 'B'
//      Eval( ::FOnChange, Self, Source, Rebuild )
   ELSE
//      HB_Exec( ::FOnChange, Self, Source, Rebuild )
   ENDIF

Return NIL

METHOD RebuildHandle() CLASS TMenuItem

   /*
   IF And( csDestroying, ::ComponentState )
      RETURN NIL
   ENDIF

   IF And( csReading, ::ComponentState )
      ::FStreamedRebuild := .T.
   ELSE
      IF ::FMergedWith <> NIL
         ::FMergedWith:`RebuildHandle()
      ELSE
         WHILE ::GetMenuItemCount( Handle ) > 0
            ::RemoveMenu( Handle, 0, MF_BYPOSITION)
         END

         ::PopulateMenu()
         ::MenuChanged( .F. )
      ENDIF
   ENDIF
   */

Return NIL

METHOD SetEnabled( Value ) CLASS TMenuItem

  Return NIL

  IF ::FEnabled <> Value
    ::FEnabled := Value

    //IF ((Win32Platform() == VER_PLATFORM_WIN32_NT ) .AND. ( ::Count <> 0 ) ) .OR. (( ::Owner <> NIL ) .AND. Assigned( ::Owner.FMergedWith ) )
       ::MenuChanged( .T. )
    //ELSE
       IF ( ::FParent <> NIL ) //.AND. ! ( And( csReading, ComponentState ) )
          EnableMenuItem( ::FParent:Handle, ::FCommand, /* Or( MF_BYCOMMAND, ::Enables[Value] ) */ )
       ENDIF

       ::MenuChanged( .F. )
    //END
  END

Return NIL

METHOD SetVisible( Value ) CLASS TMenuItem

   IF Value <> ::FVisible
      ::FVisible := Value
      ::MenuChanged( .T. )
   ENDIF

Return NIL

METHOD SetImageIndex( Value ) CLASS TMenuItem

  IF Value <> ::FImageIndex
    ::FImageIndex := Value
    ::MenuChanged( .T. )
  ENDIF

Return NIL

METHOD SetCaption( Value ) CLASS TMenuItem

   IF Value != NIL .AND. ::FCaption != Value
      ::FCaption := Value
      ::MenuItemInfo:dwTypeData := ::FCaption
      ::MenuItemInfo:fMask := MIIM_DATA
      ::MenuItemInfo:fMask := MIIM_CHECKMARKS + MIIM_DATA + MIIM_ID + MIIM_STATE + MIIM_SUBMENU + MIIM_TYPE

      SetMenuItemInfo( ::FMenu:Handle, ::MenuItemInfo:wID, .T., ::MenuItemInfo:value )
      ::MenuChanged( .T. )
   ENDIF

Return NIL

METHOD SetCommand( Value ) CLASS TMenuItem

   IF Value != NIL .AND. ::FCommand != Value
      ::FCommand := Value
      ::MenuItemInfo:wID   := ::FCommand
      ::MenuItemInfo:fMask := MIIM_ID
      SetMenuItemInfo( ::FMenu:Handle, ::MenuItemInfo:wID, .T., ::MenuItemInfo:value )
      ::MenuChanged( .T. )
   ENDIF

Return NIL


METHOD SetParentComponent( Value ) CLASS TMenuItem

   IF ::FParent != NIL
      ::FParent:Remove( Self )
   ENDIF
   IF Value != NIL
      IF Value:Owner:IsDerivedFrom( "TMenu" )
         aAdd( Value:Items, Self )
      ELSEIF Value:Owner:ClassName == "TMENUITEM"
         TMenuItem( Value ):Add( Self )
      ENDIF
   ENDIF

RETURN NIL

METHOD GetParentMenu() CLASS TMenuItem

   LOCAL MenuItem := Self

   WHILE MenuItem:FParent != NIL
      MenuItem := MenuItem:FParent
   ENDDO

RETURN MenuItem:FMenu

METHOD Add( Item ) CLASS TMenuItem

  ::Insert( ::GetCount, Item)

RETURN NIL

FUNCTION Log2Int( b )
   IF b
      RETURN 2
   ENDIF
RETURN 1