/*
 * $Id: TCCombo.prg,v 1.21 2002/10/23 02:19:41 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TComboBox CLASS
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
#include "wingdi.ch"
#include "debug.ch"
#include "wintypes.ch"
#include "cstruct.ch"

*------------------------------------------------------------------------------*

CLASS TComboBox FROM TCustomControl

   DATA Caption PROTECTED INIT ""
   DATA xxLeft   PROTECTED  INIT   0
   DATA xxTop    PROTECTED  INIT   0
   DATA xxWidth  PROTECTED  INIT  80
   DATA xxHeight PROTECTED  INIT  20
   
   DATA Style   INIT  WS_CHILD+WS_VISIBLE+WS_BORDER+WS_TABSTOP+CBS_DROPDOWNLIST+WS_VSCROLL+CBS_HASSTRINGS

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_DESTROY,WM_SIZE,WM_MOVE,WM_COMMAND}
   DATA WndProc   PROTECTED INIT 'ControlProc'

   METHOD New() CONSTRUCTOR
   METHOD AddString( cText )        INLINE ::SendMessage( CB_ADDSTRING, 0, cText)
   METHOD InsertString(cText,nLine) INLINE ::SendMessage( CB_INSERTSTRING, nLine, cText )
   METHOD DeleteString(nLine)       INLINE ::SendMessage( CB_DELETESTRING, nLine, 0)
   METHOD SetCurSel(nLine)          INLINE ::SendMessage( CB_SETCURSEL, nLine, 0)
   METHOD FindString(nStart,cStr)   INLINE ::SendMessage( CB_FINDSTRING, IFNIL(nStart,-1,nStart), cStr)
   METHOD FindExact(nStart,cStr)    INLINE ::SendMessage( CB_FINDSTRINGEXACT, IFNIL(nStart,-1,nStart), cStr)
   METHOD GetCount()                INLINE ::SendMessage( CB_GETCOUNT, 0, 0)
   METHOD GetCurSel()               INLINE ::SendMessage( CB_GETCURSEL, 0, 0)
   METHOD Dir(nAttr, cFileSpec)     INLINE ::SendMessage( CB_DIR, nAttr, cFileSpec)
   METHOD SetItemHeight(n,nHeight)  INLINE ::SendMessage( CB_SETITEMHEIGHT, n, nHeight )
ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, nId, nLeft, nTop, nWidth, nHeight) CLASS TComboBox

   ::WinClass    := "combobox"
   ::ControlName := "ComboBox"

   ::id        := nId
   ::Left      := nLeft
   ::Top       := nTop
   ::Width     := IFNIL( nWidth , ::width , nWidth )
   ::Height    := IFNIL( nHeight, ::height, nHeight)

   RETURN( super:new( oParent ) )
