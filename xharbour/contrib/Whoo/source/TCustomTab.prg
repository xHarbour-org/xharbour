/*
 * $Id: TCustomTab.prg,v 1.1 2002/10/31 04:08:43 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TCustomControl CLASS
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

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"
#Include "tabctrl.ch"

*------------------------------------------------------------------------------*

CLASS TCustomTabControl FROM TWinControl

   DATA Canvas          PROTECTED
   DATA DisplayRect     PROTECTED
   DATA HotTrack        PROTECTED
   DATA Images          PROTECTED
   DATA MultiLine       EXPORTED  INIT .F.
   DATA MultiSelect     EXPORTED  INIT .F.
   DATA OwnerDraw       EXPORTED  INIT .F.
   DATA RaggedRight     EXPORTED  INIT .F.
   DATA ScrollOpposite  EXPORTED  INIT .T.
   DATA Style           PROTECTED INIT  WS_CHILD + WS_VISIBLE + WS_TABSTOP
   DATA TabHeight       EXPORTED  INIT 20
   DATA TabIndex        PROTECTED INIT  0
   DATA TabPosition     PROTECTED INIT  { 0, 0, 160, 160 }
   DATA Tabs            PROTECTED INIT  0
   DATA TabWidth        PROTECTED INIT 40
   
   METHOD AdjustClientRect() VIRTUAL
   METHOD CanChange()        VIRTUAL
   METHOD CanShowTab()       VIRTUAL
   METHOD Change()           VIRTUAL
   METHOD Create()
   METHOD CreateParams()     VIRTUAL
   METHOD CreateWnd()        VIRTUAL
   METHOD Destroy()          VIRTUAL
   METHOD DrawTab()          VIRTUAL
   METHOD GetHitTestInfoAt() VIRTUAL
   METHOD GetImageIndex()    VIRTUAL
   METHOD IndexOfTabAt()     VIRTUAL
   METHOD Loaded()           VIRTUAL
   METHOD Notification()     VIRTUAL
   METHOD RowCount()         VIRTUAL
   METHOD ScrollTabs()       VIRTUAL
   METHOD TabRect()          VIRTUAL
   METHOD UpdateTabImages()  VIRTUAL

ENDCLASS

METHOD Create() CLASS TCustomTabControl
   ::FHandle := CreateWindowEx( ::ExStyle, WC_TABCONTROL, , ::Style, ;
                               ::TabPosition[1], ::TabPosition[2], ;
                               ::TabPosition[3], ::TabPosition[4], ;
                               ::Parent:Handle, ::Id, ::Instance )
RETURN Self
