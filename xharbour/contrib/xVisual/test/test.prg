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

#define CRLF CHR(13)+CHR(10)

PROCEDURE Main()

  LOCAL FC := FirstClass():New()
  LOCAL SC := SecondClass():New()

  clear screen

  ? "Original Data from FirstClass"
  FC:Print()

  ? CRLF

  ? "Modified Data internally from FirstClass"
  FC:Modify()
  FC:Print()

  ? CRLF
  ? "Modified FirstClass Data externally"

  FC:data_published := "AA"
  FC:data_protected := "BB"
  FC:data_hidden    := "CC"
  FC:data_exported  := "DD"
  FC:data_RO        := "EE"

  FC:Print()
  ? CRLF

  ? "Original Data from SecondClass"
  SC:Print()
  ? CRLF

  ? "Modified Data internally from SecondClass"
  SC:Modify()
  SC:Print()

  ? CRLF

  ? "Modified SecondClass Data externally"

  SC:data_published := "AA"
  SC:data_protected := "BB"
  SC:data_hidden    := "CC"
  SC:data_exported  := "DD"
  SC:data_RO        := "EE"

  SC:Print()
  ? CRLF

RETURN 