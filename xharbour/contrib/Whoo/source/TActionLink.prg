/*
 * $Id: TActionLink.prg,v 1.1 2002/11/07 02:45:06 fsgiudice Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TActionLink CLASS
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

*-----------------------------------------------------------------------------*

CLASS TActionLink FROM TBasicActionLink

  PROTECTED:
    METHOD IsCaptionLinked       VIRTUAL
    METHOD IsCheckedLinked       VIRTUAL
    METHOD IsEnabledLinked       VIRTUAL
    METHOD IsGroupIndexLinked    VIRTUAL
    METHOD IsHelpContextLinked   VIRTUAL
    METHOD IsHelpLinked          VIRTUAL
    METHOD IsHintLinked          VIRTUAL
    METHOD IsImageIndexLinked    VIRTUAL
    METHOD IsShortCutLinked      VIRTUAL
    METHOD IsVisibleLinked       VIRTUAL
    METHOD SetAutoCheck          VIRTUAL
    METHOD SetCaption            VIRTUAL
    METHOD SetChecked            VIRTUAL
    METHOD SetEnabled            VIRTUAL
    METHOD SetGroupIndex         VIRTUAL
    METHOD SetHelpContext        VIRTUAL
    METHOD SetHelpKeyword        VIRTUAL
    METHOD SetHelpType           VIRTUAL
    METHOD SetHint               VIRTUAL
    METHOD SetImageIndex         VIRTUAL
    METHOD SetShortCut           VIRTUAL
    METHOD SetVisible            VIRTUAL

ENDCLASS

*-----------------------------------------------------------------------------*

*-----------------------------------------------------------------------------*
