/*
 * $Id: TCButton.prg,v 1.24 2003/01/09 08:21:02 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TButton CLASS
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
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TButton FROM TCustomControl

   DATA FLeft    EXPORTED INIT   0
   DATA FTop     EXPORTED INIT   0
   DATA FWidth   EXPORTED INIT  80
   DATA FHeight  EXPORTED INIT  24
   
   DATA Style   INIT  WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_PUSHBUTTON

   DATA lRegister EXPORTED INIT .F.
   DATA lControl  EXPORTED INIT .T.
   DATA Msgs      EXPORTED INIT {WM_DESTROY,WM_SIZE,WM_MOVE,WM_MOUSEMOVE}
   DATA WndProc   EXPORTED INIT 'ControlProc'

   DATA WinClass    EXPORTED INIT "button"
   DATA ControlName EXPORTED INIT "Button"
   
ENDCLASS
