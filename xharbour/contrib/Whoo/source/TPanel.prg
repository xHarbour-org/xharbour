/*
 * $Id: TPanel.prg,v 1.19 2003/01/29 10:26:45 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TPanel CLASS for DialogBox
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
#include "winuser.ch"
#include "what32.ch"
#include "classex.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

*-----------------------------------------------------------------------------*

CLASS TPanel FROM TForm
   
   DATA FLeft   INIT  0
   DATA FTop    INIT  0
   DATA FWidth  INIT 10
   DATA FHeight INIT 10
   
   PROPERTY Menu READ FMenu
//-------------------------------------------------------------------------------------------
   ACCESS biSystemMenu    INLINE AND( ::Style, WS_SYSMENU ) # 0
   ASSIGN biSystemMenu(l) INLINE ::SetStyle(WS_SYSMENU,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE ),l

   ACCESS biMinimize      INLINE AND( ::Style, WS_MAXIMIZEBOX ) # 0
   ASSIGN biMinimize(l)   INLINE ::SetStyle(WS_MAXIMIZEBOX,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE )

   ACCESS biMaximize      INLINE AND( ::Style, WS_MINIMIZEBOX ) # 0
   ASSIGN biMaximize(l)   INLINE ::SetStyle(WS_MINIMIZEBOX,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE )

//-------------------------------------------------------------------------------------------
   DATA WinClass    PROTECTED INIT "Panel"
   DATA ControlName EXPORTED INIT "Panel"

   METHOD Create()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD Create( oOwner ) CLASS TPanel
   
   ::Super:Create( oOwner )

   ::WndProc   := IFNIL(::WndProc,'FormProc',::WndProc)
   ::Msgs      := IFNIL(::Msgs,-1,::Msgs)
   ::FrameWnd  := .F.
   ::Style     := IFNIL(::Style,WS_OVERLAPPEDWINDOW,::Style)
   ::FormType  := RCF_DIALOG
   ::lRegister := IFNIL(::lRegister,.T.,::lRegister)
   ::lControl  := .F.
   ::ExStyle   := IFNIL(::ExStyle,0,::ExStyle)
   ::Modal     := IFNIL(::Modal,.F.,::Modal)

RETURN( Self )

*-----------------------------------------------------------------------------*
