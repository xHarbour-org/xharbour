/*
 * $Id: xTree.prg,v 1.4 2002/10/10 02:51:46 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TTVItem CLASS for TreeView Items
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
#include "hbclass.ch"
#include "windows.ch"
#include "what32.ch"

#define TV_FIRST          4352   // 0x1100
#define TVM_EXPAND      TV_FIRST + 2

#define TVE_EXPAND           2

//----------------------------------------------------------------------------//

CLASS TTVItem
   DATA   handle
   DATA   Items
   DATA   Tree
   DATA   Cargo
   METHOD New() CONSTRUCTOR
   METHOD Add()
   METHOD Expand() INLINE SendMessage( ::Tree:handle, TVM_EXPAND, TVE_EXPAND, ::handle )
ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( hItem, oTree, Cargo ) CLASS TTVItem
   ::Items  := {}
   ::handle := hItem
   ::Tree   := oTree
   ::Cargo  := Cargo
return Self

//----------------------------------------------------------------------------//

METHOD Add( cPrompt, nImage, Cargo ) CLASS TTVItem
   local oItem := TTVItem():New( TVInsertItem( ::Tree:handle, cPrompt, ::handle, nImage ), ::Tree, Cargo )
   AAdd( ::Items, oItem )
return oItem

//----------------------------------------------------------------------------//
