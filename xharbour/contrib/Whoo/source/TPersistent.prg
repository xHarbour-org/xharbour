/*
 * $Id: TPersistent.prg,v 1.3 2002/10/16 21:17:00 fsgiudice Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TPersistent CLASS
 *
 * Copyright 2002 Francesco Saverio Giudice [info@fsgiudice.com]
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
#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

CLASS TPersistent FROM TObject

   METHOD New() CONSTRUCTOR

   METHOD Assign                   VIRTUAL
   METHOD AssignTo                 VIRTUAL   // PROTECTED
   METHOD DefineProperties         VIRTUAL   // PROTECTED
   METHOD Destroy                  VIRTUAL
   METHOD GetNamePath              VIRTUAL
   METHOD GetOwner                 INLINE NIL PROTECTED

ENDCLASS

METHOD New() CLASS TPersistent
RETURN Self

