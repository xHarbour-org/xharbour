/*
 * xHarbour Project source code:
 *
 * Whoo.lib TMenu CLASS
 *
 * Copyright 2002 Augusto Infante [systems@quesoro.com]
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

*-----------------------------------------------------------------------------*

CLASS TMenu

   DATA Parent AS OBJECT   READONLY
   DATA handle
   DATA Popup  AS OBJECT
   DATA aItems AS ARRAY INIT {}
   DATA name
   METHOD New() CONSTRUCTOR
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

*-----------------------------------------------------------------------------*
   
METHOD New( oParent ) CLASS TMenu

   ::handle := CreateMenu()
   ::Parent := oParent

   return( self )

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
   local n:= aScan( ::aItems,{|o|o:id == nId} )
   if n>0
      return( ::aItems[n] )
   endif
   return( nil )

*-----------------------------------------------------------------------------*

METHOD GetPos( nId ) CLASS TMenu
   return( aScan( ::aItems,{|o|o:id == nId} ) )

