/*
 * $Id: TabControl.Prg,v 1.24 2002/10/29 02:12:38 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TabControl CLASS
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

#Include "windows.ch"
#include "hbclass.ch"
#Include 'debug.ch'
#Include "tabctrl.ch"
#Include "wintypes.ch"
#Include "cstruct.ch"
#Include "winGdi.ch"
#Include "what32.ch"
#include "error.ch"

*-----------------------------------------------------------------------------*

CLASS TTabSheet FROM TPanel

   DATA Highlighted  EXPORTED
   DATA ImageIndex   EXPORTED
   DATA PageControl  EXPORTED
   DATA PageIndex    EXPORTED
   DATA TabIndex     EXPORTED
   DATA TabVisible   EXPORTED
      
   METHOD New() CONSTRUCTOR
   METHOD OnCommand()
/*
   METHOD Create()        
   METHOD CreateParams()
   METHOD Destroy()
   METHOD DoHide()
   METHOD DoShow()
   METHOD ReadState()
*/
ENDCLASS

METHOD New( oParent, cCaption, cRes, nImgPos ) CLASS TTabSheet

   super:new( oParent )

   ::Left    := 0
   ::Top     := 0
   ::width   := 10
   ::height  := 10
   ::style   := WS_CHILD + WS_VISIBLE
   ::modal   := .F.
   ::resname := cRes
   ::Caption := cCaption
   ::ImageIndex := nImgPos

return( self )
