/*
 * $Id: TPopup.prg,v 1.6 2002/10/11 03:53:16 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TCustomFrame CLASS
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
#include "types.ch"


CLASS TCustomFrame FROM TScrollingWinControl

  PROTECTED:
    METHOD CreateParams           virtual  //(var Params: TCreateParams); override;
    METHOD GetChildren            virtual  //(Proc: TGetChildProc; Root: TComponent); override;
    METHOD Notification           virtual  //(AComponent: TComponent; Operation: TOperation); override;
    METHOD SetParent              virtual  //(AParent: TWinControl); override;
  PUBLIC:
    METHOD Create                 virtual  //CONSTRUCTOR (AOwner: TComponent); override;

ENDCLASS

