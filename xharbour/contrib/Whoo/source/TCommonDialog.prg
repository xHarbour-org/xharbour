/*
 * $Id: TCommonDialog.prg,v 1.1 2002/10/28 04:01:57 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TCommonDialog CLASS
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

CLASS TCommonDialog FROM TComponent
   DATA Ctl3D       PROTECTED
   DATA Handle      PROTECTED
   DATA HelpContext PROTECTED
   DATA Template    PROTECTED

   METHOD Create()            VIRTUAL
   METHOD DefaultHandler()    VIRTUAL
   METHOD Destroy()           VIRTUAL
   METHOD DoClose()           VIRTUAL
   METHOD DoShow()            VIRTUAL
   METHOD Execute()           VIRTUAL
   METHOD MessageHook()       VIRTUAL
   METHOD TaskModalDialog()   VIRTUAL
   METHOD WndProc()           VIRTUAL
ENDCLASS

