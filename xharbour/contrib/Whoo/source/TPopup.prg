/*
 * xHarbour Project source code:
 *
 * Whoo.lib TPopup CLASS
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

CLASS TPopup

   DATA Parent AS OBJECT   READONLY
   DATA handle
   DATA oMenu  AS OBJECT
   DATA Text   AS CHARACTER
   
   METHOD New() CONSTRUCTOR
   METHOD Add()
   METHOD AddItem()
   METHOD AddSeparator() INLINE ::AddItem()
ENDCLASS

*-----------------------------------------------------------------------------*
   
METHOD New( oMenu, cText ) CLASS TPopup

   ::handle := CreatePopupMenu()
   ::oMenu  := oMenu
   ::Text   := cText

   return( self )

*-----------------------------------------------------------------------------*

METHOD Add() CLASS TPopup

   AppendMenu( ::oMenu:handle, MF_ENABLED + MF_POPUP , ::handle, ::Text )

   return( self )

*-----------------------------------------------------------------------------*

METHOD AddItem( cText, nId, bAction ) CLASS TPopup

   local oItem

   oItem := TMenuItem():New( cText, nId, bAction, self )

   return( oItem )

