/*
 * $Id: TCEdit.prg,v 1.18 2002/11/05 21:39:58 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TEdit CLASS
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

*------------------------------------------------------------------------------*

CLASS TEdit FROM TCustomControl

   DATA FLeft   PROTECTED INIT 0
   DATA FTop    PROTECTED INIT 0
   DATA FWidth  PROTECTED INIT 80
   DATA FHeight PROTECTED INIT 20

   DATA Style   INIT  WS_CHILD+WS_VISIBLE+WS_BORDER+WS_TABSTOP+ES_AUTOHSCROLL + ;
                      ES_AUTOVSCROLL+ES_LEFT+ES_WANTRETURN+ES_MULTILINE

   DATA ExStyle INIT  WS_EX_CLIENTEDGE

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_DESTROY,WM_SIZE,WM_MOVE}
   DATA WndProc   PROTECTED INIT 'ControlProc'

   DATA WinClass    PROTECTED INIT "edit"
   DATA ControlName PROTECTED INIT "Edit"

   METHOD New() CONSTRUCTOR
   METHOD SetText( c ) INLINE SetDlgItemText( ::Parent:handle, ::id, c )
ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TEdit

   ::FCaption   := IFNIL(cCaption, ::FCaption, cCaption)
   ::id         := nId
   ::FLeft      := nLeft
   ::FTop       := nTop
   ::FWidth     := IFNIL( nWidth , ::FWidth , nWidth )
   ::FHeight    := IFNIL( nHeight, ::FHeight, nHeight)

   RETURN( super:new( oParent ) )

*------------------------------------------------------------------------------*
