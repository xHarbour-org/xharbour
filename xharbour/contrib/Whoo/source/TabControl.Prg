/*
 * $Id: TabControl.Prg,v 1.27 2002/11/07 23:01:33 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TabControl CLASS
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

#Include "windows.ch"
#include "hbclass.ch"
#Include 'debug.ch'
#Include "tabctrl.ch"
#Include "wintypes.ch"
#Include "cstruct.ch"
#Include "winGdi.ch"
#Include "what32.ch"
#include "error.ch"

// move that structure to WinStruc.ch

IMPORT C STRUCTURE NMHDR

*-----------------------------------------------------------------------------*

CLASS TTabControl FROM TCustomControl

   DATA Caption   PROTECTED INIT ""
   
   DATA FLeft    PROTECTED INIT    0
   DATA FTop     PROTECTED INIT    0
   DATA FWidth   PROTECTED INIT  160
   DATA FHeight  PROTECTED INIT  160

   DATA CurSel  INIT    1

   DATA Style     PROTECTED INIT  WS_CHILD + WS_VISIBLE + WS_TABSTOP
   DATA ExStyle   PROTECTED INIT  0

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_SIZE,WM_MOVE}
   DATA WndProc   PROTECTED INIT 'ControlProc'
   
   DATA Cargo            PROTECTED
   DATA SendTo AS OBJECT PROTECTED

   DATA WinClass    PROTECTED INIT WC_TABCONTROL
   DATA ControlName PROTECTED INIT "TabControl"

   METHOD AddTab()
   METHOD InsertTab()
   METHOD DeleteTab()
   METHOD Configure()
   METHOD AdjustRect()
   METHOD DeleteAllTabs()
   METHOD DeselectAll()
   METHOD GetCurFocus()
   METHOD GetCurSel()
   METHOD GetExtendedStyle()
   METHOD GetImageList()
   METHOD GetItem()
   METHOD GetItemCount()
   METHOD GetItemRect()
   METHOD GetRowCount()
   METHOD GetToolTips()
   METHOD GetUnicodeFormat()
   METHOD HighlightItem()
   METHOD HitTest()
   METHOD RemoveImage()
   METHOD SetCurFocus()
   METHOD SetCurSel()
   METHOD SetExtendedStyle()
   METHOD SetImageList()
   METHOD SetItem()
   METHOD SetItemExtra()
   METHOD SetItemSize()
   METHOD SetMinTabWidth()
   METHOD SetPadding()
   METHOD SetToolTips()
   METHOD SetUnicodeFormat()
   METHOD Notify()
   METHOD OnSize()
   METHOD GetObj()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD Notify( hdr ) CLASS TTabControl
   LOCAL nSel
   nSel:=TabCtrl_GetCurSel( ::handle )+1
   IF ::Cursel <> nSel
      ShowWindow(::Controls[::CurSel]:handle, SW_HIDE)
      ::CurSel:=nSel
      ShowWindow(::Controls[::CurSel]:handle, SW_SHOW)
   endif
   return(nil)

*-----------------------------------------------------------------------------*

METHOD AddTab( cName, oPage, nImgPos ) CLASS TTabControl
   LOCAL hTab, cRes
   local hClass, nSeq
   
   if VALTYPE( oPage ) == "C"
      cRes := oPage
      oPage:= NIL
   endif
   
   DEFAULT oPage TO TabPage():Create( self )
   
   if cRes != NIL
      oPage:resname := cRes
      oPage:ImageIndex := nImgPos
   endif
   oPage:Name := cName
   oPage:Caption := cName

   if (hTab:=TabCtrl_AddItem(::FHandle,oPage:Caption,oPage:ImageIndex)) > -1
      AADD(::Controls,oPage )
      __objAddData( self, cName, HB_OO_CLSTP_PROTECTED )
      __ObjSetValueList( self, { { cName, oPage } } )
   ENDIF

   IF oPage:FHandle == NIL
      oPage:CreateWnd()
   ENDIF

   IF Len( ::Controls ) > 1
      ShowWindow(oPage:handle,SW_HIDE)
   ENDIF

   RETURN(hTab)

*-----------------------------------------------------------------------------*

METHOD InsertTab(nPos,cText,cRes,bProc) CLASS TTabControl

   if TabCtrl_InsertItem(::handle,cText,nPos) > -1
      aIns(::Controls,nPos,NIL,.T.)
      RETURN(.T.)
   ENDIF

   return(.F.)

*-----------------------------------------------------------------------------*

METHOD DeleteTab(nPos) CLASS TTabControl

   Local nCount:=LEN(::Controls)
   if nPos > 0 .and. nPos <= nCount
      IF nPos <= ::CurSel     // verify !!!!!
        ::CurSel--
      ENDIF
      TabCtrl_DeleteItem(nPos-1)
      if isWindow(::Controls[nPos]:handle)
         DestroyWindow(::Controls[nPos]:handle)
      endif
      ADel(::Controls,nPos,.t.)
      return(.T.)
   Endif

   RETURN(.F.)

*-----------------------------------------------------------------------------*

METHOD Configure() CLASS TTabControl

   LOCAL aTab //:=GetClientRect(::FHandle)
   local acRect:={0,0,0,0}
   LOCAL aTemp
   LOCAL aWnd:={}
   LOCAL hCtrl
   LOCAL i
   LOCAL aPt
   LOCAL bBlock
   
   aTab := {::FLeft, ::FTop, ::FWidth, ::FHeight }

   IF LEN(::Controls) > 0
   
      acRect := TabCtrl_GetItemRect( ::handle, 0 )
      
      FOR i := 1 TO Len( ::Controls )-1
         aTemp := TabCtrl_GetItemRect( ::handle, i )

         acRect[1] := MIN( acRect[1], aTemp[1] )
         acRect[2] := MIN( acRect[2], aTemp[2] )
         
         acRect[3] := MAX( acRect[3], aTemp[3] )
         acRect[4] := MAX( acRect[4], aTemp[4] )
      NEXT
   
   ENDIF

   FOR i:=1 TO LEN(::Controls)
      IF ::Controls[i]!=NIL
         IF Empty( ::Controls[i]:FHandle )
            ::GetHandle()
         ENDIF
         ::Controls[i]:FLeft   := acRect[1]
         ::Controls[i]:FTop    := acRect[2]+acRect[4]
         ::Controls[i]:FWidth  := aTab[3]-6
         ::Controls[i]:FHeight := aTab[4]-acRect[2]-acRect[4]-4
 
         ::Controls[i]:Move( , , , , .T. )
         
      endif
   next

   RETURN(self)

*-----------------------------------------------------------------------------*
METHOD AdjustRect(lDisplay,aRect) CLASS TTabControl

   TabCtrl_AdjustRect(::handle,lDisplay,@aRect)

   RETURN(aRect)

*-----------------------------------------------------------------------------*
METHOD DeleteAllTabs() CLASS TTabControl

  Local lRet:=TabCtrl_DeleteAllItems(::handle)

  AEVAL(::Controls,{|hWnd| IF(isWindow(hWnd),DestroyWindow(hWnd),)})
  ::Controls:={}
  ::aDlg:={}
  ::CurSel:=0

  RETURN(lRet)

*-----------------------------------------------------------------------------*
METHOD DeselectAll(lExcludeFocus) CLASS TTabControl

   TabCtrl_DeselectAll(::handle,lExcludeFocus)

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD GetCurFocus() CLASS TTabControl

  RETURN TabCtrl_GetCurFocus(::handle )+1

*-----------------------------------------------------------------------------*
METHOD GetCurSel() CLASS TTabControl

   RETURN TabCtrl_GetCurSel(::handle)+1

*-----------------------------------------------------------------------------*
METHOD GetExtendedStyle() CLASS TTabControl

   RETURN TabCtrl_GetExtendedStyle(::handle)

*-----------------------------------------------------------------------------*
METHOD GetImageList() CLASS TTabControl

   RETURN NIL //TabCtrl_GetImageList(::handle)

*-----------------------------------------------------------------------------*
METHOD GetItem(nItem,ptrItem) CLASS TTabControl

   RETURN TabCtrl_GetItem(::handle,nItem-1,@ptrItem)

*-----------------------------------------------------------------------------*
METHOD GetItemCount() CLASS TTabControl

   RETURN TabCtrl_GetItemCount(::handle)

*-----------------------------------------------------------------------------*
METHOD GetItemRect(nItem) CLASS TTabControl

   RETURN TabCtrl_GetItemRect(::handle,nItem-1)

*-----------------------------------------------------------------------------*
METHOD GetRowCount() CLASS TTabControl

   RETURN TabCtrl_GetRowCount(::handle)

*-----------------------------------------------------------------------------*
METHOD GetToolTips() CLASS TTabControl

   RETURN TabCtrl_GetToolTips(::handle)

*-----------------------------------------------------------------------------*
METHOD GetUnicodeFormat() CLASS TTabControl

   RETURN TabCtrl_GetUnicodeFormat(::handle)

*-----------------------------------------------------------------------------*
METHOD HighlightItem(nItem,nHighlight) CLASS TTabControl

   RETURN TabCtrl_HighlightItem(::handle,nItem-1,nHighlight)

*-----------------------------------------------------------------------------*
METHOD HitTest(nPtrHitTestInfo) CLASS TTabControl

   RETURN TabCtrl_HitTest(::handle,nPtrHitTestInfo) + 1

*-----------------------------------------------------------------------------*
METHOD RemoveImage(nImageIndex) CLASS TTabControl

   RETURN TabCtrl_RemoveImage(::handle, nImageIndex-1)

*-----------------------------------------------------------------------------*
METHOD SetCurFocus(nItem) CLASS TTabControl

   TabCtrl_SetCurFocus(::handle, nItem-1)

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetCurSel(nItem) CLASS TTabControl

   RETURN TabCtrl_SetCurSel(::handle, nItem-1) + 1

*-----------------------------------------------------------------------------*
METHOD SetExtendedStyle(nExStyle) CLASS TTabControl

   RETURN TabCtrl_SetExtendedStyle(::handle,nExStyle)

*-----------------------------------------------------------------------------*
METHOD SetImageList(hImageList) CLASS TTabControl

   RETURN TabCtrl_SetImageList(::handle, hImageList)

*-----------------------------------------------------------------------------*
METHOD SetItem(nItem, cText) CLASS TTabControl

   RETURN TabCtrl_SetItem(::handle, nItem-1, cText )

*-----------------------------------------------------------------------------*
METHOD SetItemExtra(nBytes) CLASS TTabControl

   RETURN TabCtrl_SetItemExtra(::handle, nBytes)

*-----------------------------------------------------------------------------*
METHOD SetItemSize(x,y) CLASS TTabControl

   RETURN TabCtrl_SetItemSize(::handle, x, y )

*-----------------------------------------------------------------------------*
METHOD SetMinTabWidth(dx) CLASS TTabControl

   RETURN TabCtrl_SetMinTabWidth( ::handle, dx )

*-----------------------------------------------------------------------------*
METHOD SetPadding( cx, cy ) CLASS TTabControl

   TabCtrl_SetPadding( ::handle, cx, cy )

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetToolTips( hToolTips ) CLASS TTabControl

   TabCtrl_SetToolTips( ::handle, hToolTips )

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetUnicodeFormat( lUnicode ) CLASS TTabControl

   RETURN TabCtrl_SetUnicodeFormat( ::handle, lUnicode )

*-----------------------------------------------------------------------------*

METHOD OnSize( nwParam,x , y ) CLASS TTabControl

   LOCAL n, aRect, oCtrl

   ::Configure()

return( nil )

*-----------------------------------------------------------------------------*

METHOD GetObj( cName ) CLASS TTabControl
   local n:= ASCAN( ::Controls,{|o|o:Name==cName} )
   IF n>0
      return( ::Controls[n] )
   ENDIF
return(nil)

*-----------------------------------------------------------------------------*

CLASS TabPage FROM TPanel
   DATA ImageIndex
   METHOD Create() CONSTRUCTOR
   METHOD OnCommand()
ENDCLASS

METHOD Create( oParent ) CLASS TabPage
   ::FLeft    := 0
   ::FTop     := 0
   ::FWidth   := 10
   ::FHeight  := 10
   ::style    := WS_CHILD + WS_VISIBLE
   ::modal    := .F.
   ::resname  := NIL
   ::ImageIndex := 0
   super:Create( oParent )
return( Self )

METHOD OnCommand(nwParam,nlParam) CLASS TabPage
   IF ::Parent:SendTo != NIL
      RETURN(::Parent:SendTo:SendMessage( WM_COMMAND, nwParam, nlParam ))
   ENDIF
return(0)

