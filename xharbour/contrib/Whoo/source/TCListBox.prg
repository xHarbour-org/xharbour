/*
 * $Id: TCListBox.prg,v 1.24 2002/11/19 03:47:35 what32 Exp $
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

#include "classex.ch"
#include "winuser.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"

IMPORT C STRUCTURE RECT

*------------------------------------------------------------------------------*

CLASS TListBox FROM TCustomListBox
   METHOD Create()
ENDCLASS

METHOD Create( oOwner ) CLASS TListBox
   ::Super:Create( oOwner )
RETURN Self
//--------------------------------------------------------------------------------------------------------------

CLASS TCustomListBox FROM TCustomControl

   DATA FCaption PROTECTED INIT ""

   DATA FLeft    PROTECTED INIT    0
   DATA FTop     PROTECTED INIT    0
   DATA FWidth   PROTECTED INIT  160
   DATA FHeight  PROTECTED INIT  160

   DATA Style   INIT  WS_CHILD + WS_VISIBLE + WS_TABSTOP + LBS_STANDARD
   DATA ExStyle INIT  WS_EX_CLIENTEDGE

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_DESTROY,WM_SIZE,WM_MOVE}
   DATA WndProc   PROTECTED INIT 'ControlProc'

   PROPERTY Items READ FItems WRITE SetItems DEFAULT TListBoxStrings()

   ACCESS CurSel  INLINE ::GetCurSel()

   DATA WinClass    PROTECTED INIT "listbox"
   DATA ControlName PROTECTED INIT "ListBox"

   METHOD GetString()
   METHOD GetItemRect()
   METHOD GetSelItems()
   METHOD Add( cText )              INLINE IF( ::FHandle != NIL, ::SendMessage( LB_ADDSTRING, 0, cText), NIL )
   METHOD Insert(cText,nLine)       INLINE IF( ::FHandle != NIL, ::SendMessage( LB_INSERTSTRING, nLine, cText ), NIL )
   METHOD Delete(nLine)             INLINE IF( ::FHandle != NIL, ::SendMessage( LB_DELETESTRING, nLine, 0), NIL )
   METHOD SetCurSel(nLine)          INLINE IF( ::FHandle != NIL, ::SendMessage( LB_SETCURSEL, nLine, 0), NIL )
   METHOD SetSel(nLine,lSel)        INLINE IF( ::FHandle != NIL, ::SendMessage( LB_SETSEL, if(lSel,1,0), MAKELPARAM(nLine, 0)), NIL )
   METHOD FindString(nStart,cStr)   INLINE IF( ::FHandle != NIL, ::SendMessage( LB_FINDSTRING, IFNIL(nStart,-1,nStart), cStr), NIL )
   METHOD FindExact(nStart,cStr)    INLINE IF( ::FHandle != NIL, ::SendMessage( LB_FINDSTRINGEXACT, IFNIL(nStart,-1,nStart), cStr), NIL )
   METHOD GetCount()                INLINE IF( ::FHandle != NIL, ::SendMessage( LB_GETCOUNT, 0, 0), NIL )
   METHOD GetCurSel()               INLINE IF( ::FHandle != NIL, ::SendMessage( LB_GETCURSEL, 0, 0), NIL )
   METHOD Dir(nAttr, cFileSpec)     INLINE IF( ::FHandle != NIL, ::SendMessage( LB_DIR, nAttr, cFileSpec), NIL )
   METHOD GetSelCount()             INLINE IF( ::FHandle != NIL, ::SendMessage( LB_GETSELCOUNT, 0, 0), NIL )
   METHOD Create()
   METHOD SetItems()
ENDCLASS

*------------------------------------------------------------------------------*

METHOD GetString(nLine) CLASS TCustomListBox

   LOCAL nLen
   LOCAL cBuf

   cBuf := space(SendMessage(::handle, LB_GETTEXTLEN, nLine, 0) + 1)
   nLen := SendMessage(::handle, LB_GETTEXT, nLine, @cBuf)

   RETURN( if(nLen == LB_ERR, nil, left(cBuf, nLen) ) )

*------------------------------------------------------------------------------*

METHOD GetItemRect( nLine) CLASS TCustomListBox

   LOCAL rc IS RECT
   LOCAL cRect := space(16)

   SendMessage( ::handle, LB_GETITEMRECT, nLine, @cRect)
   rc:buffer( cRect )

   RETURN(rc:value)

*------------------------------------------------------------------------------*

METHOD GetSelItems() CLASS TCustomListBox

   LOCAL n    := ::GetSelCount()
   LOCAL cBuf := space(n * 4)
   SendMessage( ::handle, LB_GETSELITEMS, n, @cBuf)

   RETURN( bin2array(cBuf, "int[" + str(n) + "]") )

*------------------------------------------------------------------------------*

METHOD Create( oOwner ) CLASS TCustomListBox

   ::Super:Create( oOwner )
   ::FItems := TListBoxStrings()
   ::FItems:ListBox := Self

RETURN Self

//----------------------------------------------------------------------------------------------------------------

METHOD SetItems( Value )

  ::Items := Value

RETURN Self

//----------------------------------------------------------------------------------------------------------------


CLASS TListBoxStrings FROM TStrings

   DATA ListBox PROTECTED

   METHOD Add()
   METHOD GetCount()
   METHOD Clear()     INLINE  ::Strings := {}, ::ListBox:SendMessage( LB_RESETCONTENT, 0, 0 )
ENDCLASS

//----------------------------------------------------------------------------------------------------------------

METHOD GetCount() CLASS TListBoxStrings
RETURN ::ListBox:GetCount()

//----------------------------------------------------------------------------------------------------------------

METHOD Add( cStr ) CLASS TListBoxStrings
   LOCAL nRes

   IF ( nRes := SendMessage( ::ListBox:handle, LB_ADDSTRING, 0, cStr ) ) < 0

      MessageBox( GetActiveWindow(), "Out of resource","Error" )

   ENDIF

RETURN super:Add( cStr )

//----------------------------------------------------------------------------------------------------------------


/*
function TListBoxStrings.Add(const S: string): Integer;
begin
  Result := SendMessage(ListBox.Handle, LB_ADDSTRING, 0, Longint(PChar(S)));
  if Result < 0 then raise EOutOfResources.Create(SInsertLineError);
end;

procedure TListBoxStrings.Insert(Index: Integer; const S: string);
begin
  if SendMessage(ListBox.Handle, LB_INSERTSTRING, Index,
    Longint(PChar(S))) < 0 then
    raise EOutOfResources.Create(SInsertLineError);
end;

procedure TListBoxStrings.Delete(Index: Integer);
begin
  ListBox.DeleteString(Index);
end;
*/

