/*
 * $Id:
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TMenu CLASS
 *
 * Copyright 2002 Augusto Infante [augusto@2vias.com.ar]
 * Copyright 2002 Francesco Saverio Giudice [info@fsgiudice.com]
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

*-----------------------------------------------------------------------------*

CLASS TMainMenu FROM TMenu

  PROTECTED:
    METHOD   MenuChanged               VIRTUAL // override
//    METHOD   GetHandle                 VIRTUAL // override

  PUBLIC:
    METHOD   Merge                     VIRTUAL
    METHOD   Unmerge                   VIRTUAL
    METHOD   PopulateOle2Menu          VIRTUAL
    METHOD   GetOle2AcceleratorTable   VIRTUAL
    METHOD   SetOle2MenuHandle         VIRTUAL

  PUBLISHED:
    PROPERTY AutoHotkeys
    PROPERTY AutoLineReduction
    PROPERTY BiDiMode
    PROPERTY AutoMerge              AS LOGICAL DEFAULT FALSE
    PROPERTY Images
    PROPERTY OwnerDraw
    PROPERTY ParentBiDiMode DEFAULT .F.
    PROPERTY OnChange

ENDCLASS

/*

FUNCTION NewMenu(Owner: TComponent; const AName: string; const Items: array of TMenuItem)

The Owner parameter specifies the component that is responsible for freeing the menu (typically the form).
The AName parameter specifies the name of the menu, which is used to refer to it in code.
The Items parameter is an array of menu items that make up the top level menu items. To create the menu items for the Items parameter, use the NewItem function.
*/
FUNCTION NewMenu( oOwner, cName, aItems )
   LOCAL oTMM := TMainMenu()

   oTMM:Owner := oOwner
   oTMM:Name  := cName
   oTMM:Items := aItems

RETURN oTMM

