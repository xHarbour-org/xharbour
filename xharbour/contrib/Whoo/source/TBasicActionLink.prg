/*
 * $Id:  Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TBasicActionLink CLASS
 *
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

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "classex.ch"

*-----------------------------------------------------------------------------*

CLASS TBasicActionLink FROM TObject

  PROTECTED:
    DATA     FAction
    METHOD   AssignClient       VIRTUAL
    METHOD   Change             VIRTUAL
    METHOD   IsOnExecuteLinked  VIRTUAL
    METHOD   SetAction          VIRTUAL
    METHOD   SetOnExecute       VIRTUAL

  PUBLIC:
    METHOD   Create             VIRTUAL // CONSTRUCTOR
    METHOD   Destroy            //DESTRUCTOR OVERRIDE
    METHOD   Execute  virtual
    METHOD   Update   virtual
    PROPERTY Action
    PROPERTY OnChange


ENDCLASS

*-----------------------------------------------------------------------------*
*-----------------------------------------------------------------------------*
