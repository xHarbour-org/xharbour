/*
 * $Id: TCRebar.prg,v 1.25 2002/11/07 20:05:56 what32 Exp $
 */
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

CLASS TRebar FROM TCustomControl

   DATA Caption  PROTECTED  INIT ""
   DATA FLeft   PROTECTED  INIT    0
   DATA FTop    PROTECTED  INIT    0
   DATA FWidth  PROTECTED  INIT  200
   DATA FHeight PROTECTED  INIT  100

   DATA Style   INIT  WS_VISIBLE+WS_BORDER+WS_CHILD+WS_CLIPCHILDREN+WS_CLIPSIBLINGS+;
                      /*RBS_VARHEIGHT+*/RBS_BANDBORDERS+CCS_NODIVIDER+CCS_NOPARENTALIGN+CCS_TOP
   DATA ExStyle INIT  WS_EX_TOOLWINDOW

   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_NOTIFY}
   DATA WndProc   PROTECTED INIT 'FormProc'

   VAR nrProc     PROTECTED

   DATA WinClass    PROTECTED INIT REBARCLASSNAME
   DATA ControlName PROTECTED INIT "Rebar"

   METHOD Create() CONSTRUCTOR
   METHOD AddBand()
   METHOD RebarProc()
   METHOD Delete()
   METHOD DelControl()

ENDCLASS

METHOD Delete() CLASS TRebar
   local n
   if( n := ascan( ::Parent:Controls, {|o|o:handle==::handle} ) )>0
      __objDelData( ::Parent, UPPER(::name ))
      adel( ::Parent:Controls, n, .t. )
      ::Destroy()
   endif
   RETURN(self)


METHOD DelControl() CLASS TRebar
   local n
   if( n := ascan( ::Parent:Controls, {|o|o:handle==::handle} ) )>0
      __objDelData( ::Parent, UPPER(::name ))
      adel( ::Parent:Controls, n, .t. )
      ::Destroy()
   endif
   RETURN(self)

*------------------------------------------------------------------------------*


METHOD RebarProc(nMsg,nwParam,nlParam) CLASS TRebar

   LOCAL acRect
   LOCAL aRect

   acRect:=GetClientRect(::Parent:handle)
   aRect :=GetWindowRect(::handle)
      
   ::FWidth := acRect[3]
   ::FHeight:= aRect[4]-aRect[2]
      
   ::Move( , , , , .T. )
      
   RETURN 0

*------------------------------------------------------------------------------*

METHOD Create( oParent ) CLASS TRebar

   InitCommonControlsEx(ICC_COOL_CLASSES)
   super:Create( oParent )

   ::FHeight := 20
   ::nrProc := SetProcedure(::Parent:handle,{|hWnd, nMsg,nwParam,nlParam|;
                            ::RebarProc(nMsg,nwParam,nlParam)}, WM_SIZE )
   ::RebarProc( WM_SIZE,0,0)
   
RETURN( self )

*------------------------------------------------------------------------------*

METHOD addband(nMask,nStyle,hChild,cxMin,cyMin,cx,cText,hBmp,nPos)

   LOCAL rbBand IS REBARBANDINFO
   LOCAL aRect:=GetWindowRect(hChild)
   local nBand
   
   rbBand:Reset()
   rbBand:cbSize     := rbBand:sizeof()
   rbBand:fMask      := IFNIL(nMask,RBBIM_TEXT+RBBIM_STYLE+RBBIM_CHILDSIZE+RBBIM_SIZE+RBBIM_CHILD,nMask)
   rbBand:fStyle     := IFNIL(nStyle,RBBS_GRIPPERALWAYS+RBBS_NOVERT,nStyle)
   rbBand:hwndChild  := IFNIL(hChild,0,hChild)
   
   rbBand:cxMinChild := IFNIL(cxMin,aRect[3]-aRect[1],cxMin)
   rbBand:cyMinChild := IFNIL(cyMin,aRect[4]-aRect[2],cyMin)
   
   rbBand:cx         := IFNIL(cx,GetClientRect(::Parent:handle)[3],cx)
   
   rbBand:lpText     := IFNIL(cText,"Test",cText)
   rbBand:hbmBack    := IFNIL(hBmp,0,hBmp)

   nBand := ::SendMessage( RB_INSERTBAND, -1, rbBand:value )
   RETURN( nBand <> 0 )

*------------------------------------------------------------------------------*
