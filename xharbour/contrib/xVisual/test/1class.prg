/*
 * $Id: xide.prg,v 1.89 2002/10/16 17:56:57 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * xIDE Main Module
 *
 * Copyright 2002 Augusto Infante [systems@quesoro.com] Andy Wos [andrwos@aust1.net] Ron Pinkas [ron@ronpinkas.com]
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

//#include "windows.ch"
//#include "wingdi.ch"
//#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
//#include "what32.ch"
//#Include "toolbar.ch"
//#Include "winlview.ch"
//#include "wintypes.ch"
//#include "cstruct.ch"

CLASS FirstClass
   DATA data_published PUBLISHED
   DATA data_protected PROTECTED
   DATA data_hidden    HIDDEN
   DATA data_exported
   DATA data_RO

   METHOD New()    CONSTRUCTOR
   METHOD Modify()
   METHOD Print()
ENDCLASS

METHOD New()
   ::data_published := "1A"
   ::data_protected := "1B"
   ::data_hidden    := "1C"
   ::data_exported  := "1D"
   ::data_RO        := "1E"
RETURN Self

METHOD Modify()
   ::data_published := 1
   ::data_protected := 2
   ::data_hidden    := 3
   ::data_exported  := 4
   ::data_RO        := 5
RETURN Self

METHOD Print()
   ? "data_published = ", ::data_published ; view ::data_published
   ? "data_protected = ", ::data_protected ; view ::data_protected
   ? "data_hidden    = ", ::data_hidden    ; view ::data_hidden
   ? "data_exported  = ", ::data_exported  ; view ::data_exported
   ? "data_RO        = ", ::data_RO        ; VIEW ::data_RO
RETURN Self

