/*
 * xHarbour Project source code:
 *
 * Whoo.lib TRebar CLASS
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
#Include "wintypes.ch"
#Include "cstruct.ch"

pragma pack(4)

#Include "winstruc.ch"
#Include 'what32.ch'
#Include "toolbar.ch"
#Include "rbstruct.ch"
#Include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TRebar FROM TForm

   VAR nrProc
   VAR rect

   METHOD New() CONSTRUCTOR
   METHOD AddBand()
   METHOD RebarProc()
   METHOD OnCreate() INLINE ::OnCreateRebar()
   METHOD OnCreateRebar()

ENDCLASS

*------------------------------------------------------------------------------*

METHOD OnCreateRebar() CLASS TRebar

   ::nrProc := SetProcedure(::Parent:handle,{|hWnd, nMsg,nwParam,nlParam|;
                            ::RebarProc(nMsg,nwParam,nlParam)},{WM_SIZE})
   ::RebarProc(WM_SIZE,0,0)

   RETURN(super:OnCreate())

*------------------------------------------------------------------------------*

METHOD RebarProc(nMsg,nwParam,nlParam) CLASS TRebar

   LOCAL acRect
   LOCAL aRect

   IF nMsg==WM_SIZE
      acRect:=GetClientRect(::Parent:handle)
      aRect:=GetWindowRect(::handle)
      MoveWindow(::handle,0,0,acRect[3],aRect[4]-aRect[2],.t.)
   endif
   
   RETURN( CallWindowProc(::nrProc,::Parent:handle,nMsg,nwParam,nlParam))

*------------------------------------------------------------------------------*

METHOD New( oParent ) CLASS TRebar
   
   super:new( oParent )
    
   InitCommonControlsEx(ICC_COOL_CLASSES)

   ::Name      := REBARCLASSNAME
   ::lRegister := .F.
   ::lControl  := .T.
   ::WndProc   := ""
   ::lHaveProc := .T.
   ::Caption   := ""
   ::Left      := 0
   ::Top       := 0
   ::Width     := 200   
   ::Height    := 100
   ::ExStyle   := WS_EX_TOOLWINDOW
   ::Style     := WS_VISIBLE+WS_BORDER+WS_CHILD+WS_CLIPCHILDREN+WS_CLIPSIBLINGS+;
                  RBS_VARHEIGHT+RBS_BANDBORDERS+CCS_NODIVIDER+CCS_NOPARENTALIGN+CCS_TOP

   RETURN( self )

*------------------------------------------------------------------------------*

METHOD addband(nMask,nStyle,hChild,cxMin,cyMin,cx,cText,hBmp,nPos)

   LOCAL rbBand IS REBARBANDINFO
   LOCAL aRect:=GetWindowRect(hChild)

   rbBand:Reset()
   rbBand:cbSize     := rbBand:sizeof()
   rbBand:fMask      := IFNIL(nMask,RBBIM_TEXT+RBBIM_STYLE +RBBIM_CHILDSIZE+RBBIM_SIZE+RBBIM_CHILD,nMask)
   rbBand:fStyle     := IFNIL(nStyle,RBBS_GRIPPERALWAYS+RBBS_NOVERT,nStyle)
   rbBand:hwndChild  := IFNIL(hChild,0,hChild)
   
   rbBand:cxMinChild := IFNIL(cxMin,aRect[3]-aRect[1],cxMin)
   rbBand:cyMinChild := IFNIL(cyMin,aRect[4]-aRect[2],cyMin)
   
   rbBand:cx         := IFNIL(cx,GetClientRect(::Parent:handle)[3],cx)
   
   rbBand:lpText     := IFNIL(cText,"Test",cText)
   rbBand:hbmBack    := IFNIL(hBmp,0,hBmp)

   RETURN( ::SendMessage( RB_INSERTBAND, -1, rbBand:value ) <> 0 )

*------------------------------------------------------------------------------*
