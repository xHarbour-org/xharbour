/*
 * $Id: TMenuItem.prg,v 1.7 2002/10/11 03:53:16 what32 Exp $
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

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "debug.ch"
#include "classex.ch"

STATIC Checks     := { MF_UNCHECKED, MF_CHECKED }
STATIC Enables    := { MF_DISABLED + MF_GRAYED, MF_ENABLED }
STATIC Breaks     := { 0, MF_MENUBREAK, MF_MENUBARBREAK }
STATIC Separators := { MF_STRING, MF_SEPARATOR }

CLASS TMenuItem FROM TComponent

   CLASSDATA   FCommandID HIDDEN INIT 0

   PROPERTY Handle READ GetHandle

   PROPERTY Items READ FItems WRITE SetItems
   PROPERTY Count READ GetCount

   PROPERTY Visible READ FVisible WRITE SetVisible DEFAULT .T.

   DATA Menu    AS OBJECT
   DATA Id
   DATA Action  AS CODEBLOCK
   DATA Checked

   PROPERTY Enabled    READ FEnabled WRITE SetEnabled
   PROPERTY Command    READ FCommand
   PROPERTY ImageIndex READ FImageIndex WRITE SetImageIndex

   DATA MenuRightToLeft INIT .F.

   ACCESS Checked     INLINE ::MenuSet( MF_CHECKED, MF_UNCHECKED)
   ACCESS Disabled    INLINE ::MenuSet( MF_DISABLED, MF_ENABLED)
   ACCESS Enabled     INLINE ::MenuSet( MF_ENABLED, MF_DISABLED)
   ACCESS Grayed      INLINE ::MenuSet( MF_GRAYED, MF_ENABLED)

   METHOD Create() CONSTRUCTOR
   METHOD MenuSet()
   METHOD AppendTo( oMenu, bRightToLeft )
   METHOD GetHandle
   METHOD UniqueCommand  INLINE ::FCommandID++
   METHOD PopulateMenu() INLINE  ::AppendTo( ::FHandle, ::MenuRightToLeft )
   METHOD GetCount()     INLINE Len( ::Items )
   METHOD MenuChanged( Rebuild )
   METHOD RebuildHandle()
   METHOD PopulateMenu()
   METHOD Insert()
   METHOD SetEnabled()
   METHOD SetVisible()
   METHOD SetImageIndex()

ENDCLASS

*-----------------------------------------------------------------------------*
METHOD Create( oMenu ) CLASS TMenuItem

  ::Super:Create( oMenu )

  ::FVisible := .T.
  ::FEnabled := .T.
  ::FCommand := ::UniqueCommand()
  ::FImageIndex := -1

   ::Menu  := oMenu

   //aAdd( oMenu:aItems, self )

Return Self

*-----------------------------------------------------------------------------*
METHOD MenuSet( nTest, nOther) CLASS TMenuItem

   LOCAL nMask := Or(nTest, nOther)

Return AND(GetMenuState(::oMenu:handle, ::Id, MF_BYCOMMAND), nMask) == nTest

METHOD AppendTo( HMenu, ARightToLeft ) CLASS TMenuItem

  /*
  LOCAL IBreaks     := { MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK }
  LOCAL IChecks     := { MFS_UNCHECKED, MFS_CHECKED }
  LOCAL IDefaults   := { 0, MFS_DEFAULT }
  LOCAL IEnables    := { Or( MFS_DISABLED, MFS_GRAYED, MFS_ENABLED ) }
  LOCAL IRadios     := { MFT_STRING, MFT_RADIOCHECK }
  LOCAL ISeparators := { MFT_STRING, MFT_SEPARATOR }
  LOCAL IRTL        := { 0, Or( MFT_RIGHTORDER, MFT_RIGHTJUSTIFY ) }
  LOCAL IOwnerDraw  := { MFT_STRING, MFT_OWNERDRAW }

  LOCAL MenuItemInfo IS TMenuItemInfo

  LOCAL Caption
  LOCAL NewFlags
  LOCAL IsOwnerDraw
  LOCAL ParentMenu

  IF ::FVisible
    Caption := ::FCaption

    IF ::GetCount() > 0
       MenuItemInfo:hSubMenu := ::GetHandle
    ELSEIF ( ::FShortCut <> scNone ) .AND. ( ( ::Parent == NIL ) .OR. ;
           ( ::Parent:Parent <> NIL ) .OR. ! ( ::Parent:Owner:ClassName == "TMAINMENU" ) )
      Caption := Caption + Chr( 9 ) + ShortCutToText( ::FShortCut )
    ENDIF

    IF Lo( GetVersion() ) >= 4
       MenuItemInfo:cbSize := 44 // Required for Windows 95
       MenuItemInfo:fMask := Or( MIIM_CHECKMARKS, MIIM_DATA, MIIM_ID, MIIM_STATE, MIIM_SUBMENU, MIIM_TYPE )
       ParentMenu := ::GetParentMenu()
       IsOwnerDraw := Assigned(ParentMenu) .AND. Or( ParentMenu:IsOwnerDraw, Assigned( ::FBitmap ) ) > 0 .AND. ! ::FBitmap.Empty
       MenuItemInfo:fType := Or( IRadios[::FRadioItem], IBreaks[::FBreak] , ;
                             ISeparators[::FCaption = '-'], IRTL[ARightToLeft], ;
                             IOwnerDraw[::IsOwnerDraw] )

       MenuItemInfo:fState := Or( IChecks[::FChecked], IEnables[::FEnabled], IDefaults[::FDefault] )
       MenuItemInfo:wID := ::Command
       MenuItemInfo:hSubMenu := 0
       MenuItemInfo:hbmpChecked := 0
       MenuItemInfo:hbmpUnchecked := 0
       MenuItemInfo:dwTypeData := Caption

       IF ::GetCount > 0
          MenuItemInfo:hSubMenu := ::GetHandle()
       ENDIF

       InsertMenuItem( ::Menu, DWORD(-1), .T., MenuItemInfo )
    ELSE
      NewFlags := Or( Breaks[ IIF( ::FBreak, 2, 1 ) ], ;
                      Checks[ IIF( ::FChecked, 2, 1 ) ],;
                      Enables[ IIF( ::FEnabled, 2, 1 ) ], ;
                      Separators[ IIF( ::FCaption == '-', 2, 1 ) ], ;
                      MF_BYPOSITION )

      IF ::GetCount > 0
        InsertMenu( ::Menu, DWORD(-1), Or( MF_POPUP, NewFlags ), ::GetHandle, ::FCaption )
      ELSE
        InsertMenu( ::Menu, DWORD(-1), NewFlags, ::Command, Caption )
      ENDIF
    ENDIF
  ENDIF
  */

  ::PopUp := TPopup():New( Self, ::Caption )

Return NIL

METHOD GetHandle()

    IF ::FHandle == NIL
      IF ::Owner:ClassName == "TPOPUPMENU"
         ::FHandle := CreatePopupMenu()
      ELSE
         ::FHandle := CreateMenu()
      ENDIF

      IF ::FHandle == 0
         // Raise Error
      ENDIF

      ::PopulateMenu()
   ENDIF

Return ::FHandle

METHOD PopulateMenu

   LOCAL MenuItem

   FOR EACH MenuItem IN ::FItems
      MenuItem:AppendTo( ::FHandle, .F. )
   NEXT

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
      Eval( ::FOnChange, Self, Source, Rebuild )
   ELSE
      HB_Exec( ::FOnChange, Self, Source, Rebuild )
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
