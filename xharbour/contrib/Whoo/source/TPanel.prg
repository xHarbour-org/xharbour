/*
 * $Id: TPanel.prg,v 1.10 2002/10/12 09:40:53 what32 Exp $
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
#include "windows.ch"
#include "what32.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

*-----------------------------------------------------------------------------*

CLASS TPanel FROM TForm
   DATA biSystemMenu EXPORTED INIT .T.
   DATA biMinimize   EXPORTED INIT .T.
   DATA biMaximize   EXPORTED INIT .T.
   
   DATA Left INIT 0
   DATA Top  INIT 0
   DATA Width INIT 10
   DATA Height INIT 10
   ASSIGN biSystemMenu(l) INLINE ::SetStyle(WS_SYSMENU,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE ),l
   
   ASSIGN biMinimize(l)   INLINE ::SetStyle(WS_MAXIMIZEBOX,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE ),l

   ASSIGN biMaximize(l)   INLINE ::SetStyle(WS_MINIMIZEBOX,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE ),l

   METHOD New()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS TPanel
   
   ::WndProc   := IFNIL(::WndProc,'FormProc',::WndProc)
   ::Msgs      := IFNIL(::Msgs,-1,::Msgs)
   ::FrameWnd  := .F.
   ::Style     := IFNIL(::Style,WS_OVERLAPPEDWINDOW,::Style)
   ::FormType  := RCF_DIALOG
   ::lRegister := IFNIL(::lRegister,.T.,::lRegister)
   ::lControl  := .F.
   ::ExStyle   := IFNIL(::ExStyle,0,::ExStyle)
   ::Modal     := IFNIL(::Modal,.F.,::Modal)

   RETURN( super:New( oParent ) )

*-----------------------------------------------------------------------------*