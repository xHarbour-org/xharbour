/*
 * $Id:  Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TMenuActionLink CLASS
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

CLASS TMenuActionLink FROM TActionLink

  PROTECTED:
    DATA   FClient
    METHOD AssignClient        // override
    METHOD IsAutoCheckLinked      VIRTUAL
    METHOD IsCaptionLinked     // override
    METHOD IsCheckedLinked     // override
    METHOD IsEnabledLinked     // override
    METHOD IsHelpContextLinked // override
    METHOD IsHintLinked        // override
    METHOD IsGroupIndexLinked  // override
    METHOD IsImageIndexLinked  // override
    METHOD IsShortCutLinked    // override
    METHOD IsVisibleLinked     // override
    METHOD IsOnExecuteLinked   // override
    METHOD SetAutoCheck        // override
    METHOD SetCaption          // override
    METHOD SetChecked          // override
    METHOD SetEnabled          // override
    METHOD SetHelpContext      // override
    METHOD SetHint             // override
    METHOD SetImageIndex       // override
    METHOD SetShortCut         // override
    METHOD SetVisible          // override
    METHOD SetOnExecute        // override

ENDCLASS

*-----------------------------------------------------------------------------*

*-----------------------------------------------------------------------------*

