/*
 * $Id: TTreeview.prg,v 1.9 2002/10/14 01:36:55 fsgiudice Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TTreeView CLASS
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

#include "hbclass.ch"
#include "windows.ch"
#include "what32.ch"
#include "TreeView.ch"
#include "wingdi.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"

//----------------------------------------------------------------------------//

//CLASS TTreeView FROM TWinControl
CLASS TTreeView FROM TCustomControl

   DATA Items     PROTECTED INIT {}
   DATA ImageList PROTECTED
   DATA bChanged  PROTECTED

   DATA Caption PROTECTED INIT ""
   DATA Left    INIT    0
   DATA Top     INIT    0
   DATA Width   INIT  160
   DATA Height  INIT  160

   DATA Style   INIT  WS_CHILD+WS_VISIBLE+WS_TABSTOP+TVS_HASBUTTONS+TVS_HASLINES+TVS_LINESATROOT
   DATA ExStyle INIT  WS_EX_CLIENTEDGE

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_DESTROY,WM_NOTIFY,WM_SIZE,WM_MOVE}
   DATA WndProc   PROTECTED INIT 'ControlProc'
   DATA Name      PROTECTED INIT "SysTreeView32"

   METHOD New() CONSTRUCTOR

   METHOD Add()
   METHOD Expand()               INLINE aEval( ::Items, { | oItem | oItem:Expand() } )
   METHOD GetSelected()
   METHOD GetSelText()           INLINE TVGetSelText( ::handle )
   METHOD SelChanged()           INLINE If( ::bChanged != nil, Eval( ::bChanged, Self ), nil )
   METHOD SetBkColor( nColor )   INLINE ::SendMessage( TVM_SETBKCOLOR, 0, nColor )
   METHOD SetTextColor( nColor ) INLINE ::SendMessage( TVM_SETTEXTCOLOR, 0, nColor )
   METHOD SetImageList()
   METHOD Notify()
   METHOD OnSelChange() VIRTUAL
ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oParent, nId, nLeft, nTop, nWidth, nHeight ) CLASS TTreeView
   ::id        := nId
   ::Left      := IFNIL( nLeft,   ::Left,   nLeft  )
   ::Top       := IFNIL( nTop,    ::Top,    nTop   )
   ::width     := IFNIL( nWidth,  ::width , nWidth )
   ::Height    := IFNIL( nHeight, ::height, nHeight)
return( super:New( oParent ) )

//----------------------------------------------------------------------------//

METHOD Add( cPrompt, nImage ) CLASS TTreeView
   local oItem
   oItem := TTVItem():New( TVInsertItem( ::handle, cPrompt,, nImage ), Self )
   AAdd( ::Items, oItem )
return oItem

//----------------------------------------------------------------------------//

STATIC FUNCTION FindItem( aItems, hItem )

   LOCAL oItem, oReturn

   FOR EACH oItem IN aItems
      IF Len( oItem:Items ) > 0
         IF ( oReturn := FindItem( oItem:Items, hItem ) ) != NIL
            RETURN oReturn
         ENDIF
      ENDIF

      IF oItem:Handle == hItem
         RETURN oItem
      ENDIF
   NEXT

RETURN NIL

//----------------------------------------------------------------------------//

METHOD GetSelected() CLASS TTreeView
return FindItem( ::Items, TVGetSelected( ::handle ) )

//----------------------------------------------------------------------------//

METHOD SetImageList( oImageList ) CLASS TTreeView
   ::oImageList = oImageList
   TVSetImageList( ::handle, oImageList:handle, 0 )
return nil

//----------------------------------------------------------------------------//

METHOD Notify( hdr ) CLASS TTreeView
   DO CASE
      CASE Hdr:code == TVN_BEGINDRAG
      CASE Hdr:code == TVN_BEGINLABELEDIT
      CASE Hdr:code == TVN_BEGINRDRAG
      CASE Hdr:code == TVN_DELETEITEM
      CASE Hdr:code == TVN_ENDLABELEDIT
      CASE Hdr:code == TVN_GETDISPINFO
      CASE Hdr:code == TVN_GETINFOTIP
      CASE Hdr:code == TVN_ITEMEXPANDED
      CASE Hdr:code == TVN_ITEMEXPANDING
      CASE Hdr:code == TVN_KEYDOWN
      CASE Hdr:code == TVN_SELCHANGED
      CASE Hdr:code == TVN_SELCHANGING
      CASE Hdr:code == TVN_SETDISPINFO
      CASE Hdr:code == TVN_SINGLEEXPAND
      CASE Hdr:code == TVS_EDITLABELS
   ENDCASE
return(0)
