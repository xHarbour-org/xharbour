/*
 * $Id: TTreeview.prg,v 1.20 2003/01/09 08:21:58 what32 Exp $
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
#include "winuser.ch"
#include "what32.ch"
#include "commctrl.ch"
#include "wingdi.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"


//----------------------------------------------------------------------------//

CLASS TTreeView FROM TCustomControl

   DATA Items     EXPORTED INIT {}
   DATA ImageList EXPORTED
   DATA bChanged  EXPORTED

   DATA FCaption EXPORTED  INIT ""
   DATA FLeft    EXPORTED  INIT    0
   DATA FTop     EXPORTED  INIT    0
   DATA FWidth   EXPORTED  INIT  160
   DATA FHeight  EXPORTED  INIT  160

   DATA Style   INIT  WS_CHILD+WS_VISIBLE+WS_TABSTOP+TVS_HASBUTTONS+TVS_HASLINES+TVS_LINESATROOT+TVS_SHOWSELALWAYS
   DATA ExStyle INIT  WS_EX_CLIENTEDGE

   DATA lRegister EXPORTED INIT .F.
   DATA lControl  EXPORTED INIT .T.
   DATA Msgs      EXPORTED INIT {WM_DESTROY,WM_SIZE,WM_MOVE,WM_MOUSEMOVE}
   DATA WndProc   EXPORTED INIT 'ControlProc'

   DATA WinClass    EXPORTED INIT "SysTreeView32"
   DATA ControlName EXPORTED INIT "TreeView"

   METHOD Add()
   METHOD Expand()               INLINE aEval( ::Items, { | oItem | oItem:Expand() } )
   METHOD GetSelected()
   METHOD GetItem()
   METHOD GetSelText()           INLINE TVGetSelText( ::handle )
   METHOD SelChanged()           INLINE If( ::bChanged != nil, Eval( ::bChanged, Self ), nil )
   METHOD SetBkColor( nColor )   INLINE ::SendMessage( TVM_SETBKCOLOR, 0, nColor )
   METHOD SetTextColor( nColor ) INLINE ::SendMessage( TVM_SETTEXTCOLOR, 0, nColor )
   METHOD SelectItem( hItem )    INLINE ::SendMessage( TVM_SELECTITEM, TVGN_CARET, hItem )
    
   METHOD SetImageList()
   METHOD Notify()
   METHOD OnChange() VIRTUAL
   METHOD OnDelete() VIRTUAL
ENDCLASS

//----------------------------------------------------------------------------//

METHOD Add( cPrompt, nImage, cargo ) CLASS TTreeView
   local oItem
   oItem := TTVItem():New( TVInsertItem( ::handle, cPrompt,, nImage ), Self )
   oItem:Caption := cPrompt
   oItem:Cargo   := cargo
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

METHOD GetItem(n) CLASS TTreeView
return( ::Items[n] )

//----------------------------------------------------------------------------//

METHOD SetImageList( oImageList ) CLASS TTreeView
   ::oImageList = oImageList
   TVSetImageList( ::handle, oImageList:handle, 0 )
return nil

//----------------------------------------------------------------------------//

METHOD Notify( hdr, nlParam ) CLASS TTreeView

   LOCAL oItem
   
   DO CASE
      CASE Hdr:code == TVN_BEGINDRAG
      CASE Hdr:code == TVN_BEGINLABELEDIT
      CASE Hdr:code == TVN_BEGINRDRAG
      CASE Hdr:code == TVN_DELETEITEM
           oItem := FindItem( ::Items, TVGetSelected( ::handle ) )
           ::OnDelete( oItem )
      
      CASE Hdr:code == TVN_ENDLABELEDIT
      CASE Hdr:code == TVN_GETDISPINFO
      CASE Hdr:code == TVN_GETINFOTIP
      CASE Hdr:code == TVN_ITEMEXPANDED
      CASE Hdr:code == TVN_ITEMEXPANDING
      CASE Hdr:code == TVN_KEYDOWN
      CASE Hdr:code == TVN_SELCHANGED
           oItem := FindItem( ::Items, TVGetSelected( ::handle ) )
           ::OnChange( oItem )

      CASE Hdr:code == TVN_SELCHANGING
      CASE Hdr:code == TVN_SETDISPINFO
      CASE Hdr:code == TVN_SINGLEEXPAND
      CASE Hdr:code == TVS_EDITLABELS
   ENDCASE
RETURN NIL
