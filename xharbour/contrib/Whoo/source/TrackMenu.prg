/*
 * $Id: TrackMenu.prg,v 1.5 2002/10/11 03:53:16 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TTrackPopupMenu CLASS
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

CLASS TTrackPopupMenu

   DATA handle
   METHOD New() CONSTRUCTOR
   METHOD Create()
   METHOD AddItem()
   METHOD AddSeparator() INLINE ::AddItem()
   METHOD Destroy()
ENDCLASS

*-----------------------------------------------------------------------------*
   
METHOD New() CLASS TTrackPopupMenu
   ::handle := CreatePopupMenu()
return( self )

*-----------------------------------------------------------------------------*

METHOD AddItem( cText, nId ) CLASS TTrackPopupMenu
   IF cText==NIL.AND.nId==NIL==NIL
      AppendMenu( ::handle, MF_SEPARATOR)
     else
      AppendMenu( ::handle, MF_ENABLED + MF_STRING, nId, cText)
   endif
return(self)

*-----------------------------------------------------------------------------*

METHOD Create( hWnd, x, y ) CLASS TTrackPopupMenu
   TrackPopupMenu( ::handle, TPM_TOPALIGN + TPM_HORIZONTAL, x, y, 0, hWnd )
return(self)

METHOD Destroy() CLASS TTrackPopupMenu
   DestroyMenu(::handle)
return(self)
