/*
 * xHarbour Project source code:
 *
 * Whoo.lib TMenuItem CLASS
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
#include "debug.ch"

CLASS TMenuItem

   DATA Text    AS CHARACTER
   DATA oMenu   AS OBJECT
   DATA Id
   DATA Action  AS CODEBLOCK
   DATA Checked
   DATA Enabled

   ACCESS Checked     INLINE ::MenuSet( MF_CHECKED, MF_UNCHECKED)
   ACCESS Disabled    INLINE ::MenuSet( MF_DISABLED, MF_ENABLED)
   ACCESS Enabled     INLINE ::MenuSet( MF_ENABLED, MF_DISABLED)
   ACCESS Grayed      INLINE ::MenuSet( MF_GRAYED, MF_ENABLED)

   METHOD New() CONSTRUCTOR
   METHOD MenuSet()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( cText, nId, bAction, oMenu ) CLASS TMenuItem

   ::Text   := cText
   ::Id     := nId
   ::Action := bAction
   ::oMenu  := oMenu
   aAdd( ::oMenu:oMenu:aItems, self )
   IF cText==NIL.AND.nId==NIL.AND.bAction==NIL
      AppendMenu( oMenu:handle, MF_SEPARATOR)
     else
      AppendMenu( oMenu:handle, MF_ENABLED + MF_STRING, nId, cText)
   endif

   return(self)

*-----------------------------------------------------------------------------*

METHOD MenuSet( nTest, nOther) CLASS TMenuItem
   local nMask := Or(nTest, nOther)
return AND(GetMenuState(::oMenu:handle, ::Id, MF_BYCOMMAND), nMask) == nTest
