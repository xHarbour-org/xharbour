/*
 * $Id: TScrollingWinControl.prg,v 1.2 2002/11/11 18:43:32 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TScrollingWinControl CLASS
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

#include "winuser.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "classex.ch"
#include "types.ch"


CLASS TScrollingWinControl FROM TWinControl
/*
  PROTECTED:
    METHOD   AdjustClientRect       virtual   //; override;
    METHOD   AlignControls          virtual   //; override;
    METHOD   AutoScrollEnabled      VIRTUAL
    METHOD   AutoScrollInView       VIRTUAL
    METHOD   ChangeScale            virtual   //; override;
    METHOD   CreateParams           virtual   //; override;
    METHOD   CreateWnd              virtual   //; override;
    METHOD   DoFlipChildren         virtual   //; override;
    PROPERTY AutoScroll             AS LOGICAL DEFAULT TRUE
    METHOD   Resizing               VIRTUAL
  PUBLIC:
//    METHOD   Create     CONSTRUCTOR // OVERRIDE
    METHOD   Destroy                virtual   //destructor; override;
    METHOD   DisableAutoRange       virtual
    METHOD   EnableAutoRange        virtual
    METHOD   ScrollInView           virtual
  PUBLISHED:
    PROPERTY HorzScrollBar          // : TControlScrollBar;
    PROPERTY VertScrollBar          // : TControlScrollBar;
*/
ENDCLASS

