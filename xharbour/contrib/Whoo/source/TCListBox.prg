/*
 * $Id: TCListBox.prg,v 1.18 2002/10/29 02:12:37 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TListBox CLASS
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
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"

#define LB_OKAY             0
#define LB_ERR              (-1)
#define LB_ERRSPACE         (-2)

IMPORT C STRUCTURE RECT

*------------------------------------------------------------------------------*

CLASS TListBox FROM TCustomControl

   DATA Caption   PROTECTED INIT ""

   DATA xxLeft    PROTECTED INIT    0
   DATA xxTop     PROTECTED INIT    0
   DATA xxWidth   PROTECTED INIT  160
   DATA xxHeight  PROTECTED INIT  160

   DATA Style   INIT  WS_CHILD + WS_VISIBLE + WS_TABSTOP + LBS_STANDARD
   DATA ExStyle INIT  WS_EX_CLIENTEDGE

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_DESTROY,WM_SIZE,WM_MOVE}
   DATA WndProc   PROTECTED INIT 'ControlProc'

   DATA Items     EXPORTED INIT TStrings():New()
   
   ACCESS CurSel  INLINE ::GetCurSel()

   DATA WinClass    PROTECTED INIT "listbox"
   DATA ControlName PROTECTED INIT "ListBox"

   METHOD New() CONSTRUCTOR
   
   METHOD GetString()
   METHOD GetItemRect()
   METHOD GetSelItems()
   METHOD AddItem( cText )          INLINE ::SendMessage( LB_ADDSTRING, 0, cText)
   METHOD Clear()                   INLINE ::SendMessage( LB_RESETCONTENT, 0, 0 )
   METHOD InsertItem(cText,nLine)   INLINE ::SendMessage( LB_INSERTSTRING, nLine, cText )
   METHOD DeleteItem(nLine)         INLINE ::SendMessage( LB_DELETESTRING, nLine, 0)
   METHOD SetCurSel(nLine)          INLINE ::SendMessage( LB_SETCURSEL, nLine, 0)
   METHOD SetSel(nLine,lSel)        INLINE ::SendMessage( LB_SETSEL, if(lSel,1,0), MAKELPARAM(nLine, 0))
   METHOD FindString(nStart,cStr)   INLINE ::SendMessage( LB_FINDSTRING, IFNIL(nStart,-1,nStart), cStr)
   METHOD FindExact(nStart,cStr)    INLINE ::SendMessage( LB_FINDSTRINGEXACT, IFNIL(nStart,-1,nStart), cStr)
   METHOD GetCount()                INLINE ::SendMessage( LB_GETCOUNT, 0, 0)
   METHOD GetCurSel()               INLINE ::SendMessage( LB_GETCURSEL, 0, 0)
   METHOD Dir(nAttr, cFileSpec)     INLINE ::SendMessage( LB_DIR, nAttr, cFileSpec)
   METHOD GetSelCount()             INLINE ::SendMessage( LB_GETSELCOUNT, 0, 0)
   METHOD OnCreate()
ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, nId, nLeft, nTop, nWidth, nHeight ) CLASS TListBox

   ::id        := nId
   ::Left      := IFNIL( nLeft,    ::Left,    nLeft    )
   ::Top       := IFNIL( nTop,     ::Top,     nTop     )
   ::Width     := IFNIL( nWidth ,  ::Width,   nWidth   )
   ::Height    := IFNIL( nHeight,  ::height,  nHeight  )

   RETURN( super:new( oParent ) )

*------------------------------------------------------------------------------*

METHOD GetString(nLine) CLASS TListBox

   LOCAL nLen
   LOCAL cBuf

   cBuf := space(SendMessage(::handle, LB_GETTEXTLEN, nLine, 0) + 1)
   nLen := SendMessage(::handle, LB_GETTEXT, nLine, @cBuf)

   RETURN( if(nLen == LB_ERR, nil, left(cBuf, nLen) ) )

*------------------------------------------------------------------------------*

METHOD GetItemRect( nLine) CLASS TListBox

   LOCAL rc IS RECT
   LOCAL cRect := space(16)

   SendMessage( ::handle, LB_GETITEMRECT, nLine, @cRect)
   rc:buffer( cRect )

   RETURN(rc:value)

*------------------------------------------------------------------------------*

METHOD GetSelItems() CLASS TListBox

   LOCAL n    := ::GetSelCount()
   LOCAL cBuf := space(n * 4)
   SendMessage( ::handle, LB_GETSELITEMS, n, @cBuf)

   RETURN( bin2array(cBuf, "int[" + str(n) + "]") )

*------------------------------------------------------------------------------*

METHOD OnCreate() CLASS TListBox
   
   LOCAL cStr

   FOR EACH cStr IN ::Items:Text
      ::AddItem( cStr )
   NEXT

RETURN Self

