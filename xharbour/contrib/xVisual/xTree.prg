/*
 * $Id: xTree.prg,v 1.22 2002/11/14 07:59:28 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * xIDE ObjectTree Module
 *
 * Copyright 2002 Augusto Infante [systems@quesoro.com] Andy Wos [andrwos@aust1.net] Ron Pinkas [ron@ronpinkas.com]
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
#include "imglist.ch"
#include "debug.ch"
#include "TreeView.ch"

GLOBAL EXTERNAL MainForm
GLOBAL EXTERNAL FormEdit
GLOBAL EXTERNAL ObjInspect

CLASS ObjTree FROM TForm

   VAR TreeRoot AS OBJECT
   DATA oTree PROTECTED
   
   METHOD Create()
   METHOD WMCreate()
   METHOD WMSize(n,x,y)  INLINE IIF( ! ::oTree == NIL, ::oTree:Move( , , x, y, .t. ), ), NIL

ENDCLASS

METHOD Create( oParent ) CLASS ObjTree

   ::Super:Create( oParent )

   ::FCaption := "Object Tree"
   ::Name     := "ObjTree"
   ::FLeft    := 0
   ::FTop     := 125
   ::FWidth   := 200
   ::FHeight  := 150
   ::ExStyle  := WS_EX_TOOLWINDOW

   ::SetParent( MainForm )
   ::HandleNeeded()
RETURN( Self )


METHOD WMCreate() CLASS ObjTree

   LOCAL o,hImg,hBmp
   
   hImg := ImageList_Create( 16, 16, ILC_COLORDDB+ILC_MASK )
   hBmp := LoadImage( hInstance(), "OBJTREE", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
   ImageList_AddMasked( hImg, hBmp, RGB( 0, 255, 255 ) )
   DeleteObject(hBmp)

   ::oTree := TreeObj():Create( self )

   ::oTree:FWidth := ::FWidth
   ::oTree:FHeight:= ::FHeight

   ::oTree:SetParent( Self )

   TVSetImageList( ::oTree:FHandle, hImg, 0 )

RETURN Self

//------------------------------------------------------------------------------------------------------------------

CLASS TreeObj FROM TTreeView
   METHOD Add()
   METHOD OnChange()
ENDCLASS

METHOD Add( cText, nImg, hObj ) CLASS TreeObj
   local o
   if empty( ::Parent:TreeRoot )
      o:=::Parent:TreeRoot:=super:Add( cText, nImg, hObj )
     else
      o:=::Parent:TreeRoot:Add( cText, nImg, hObj )
   endif
return(o)

METHOD OnChange( oItem ) CLASS TreeObj

   LOCAL n := aScan( ObjInspect:Objects, {|o|o:FHandle == oItem:cargo} )

   IF n > 0
      ObjInspect:ComboBox1:SetCurSel( n - 1 )
   ENDIF

return(0)
