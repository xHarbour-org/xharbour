/*
 * $Id: TCRebar.prg,v 1.29 2002/11/15 01:56:34 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TCoolBar CLASS
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
#Include "classex.ch"

*------------------------------------------------------------------------------*

CLASS TCoolBar FROM TCustomControl

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
   DATA ControlName PROTECTED INIT "CoolBar"
   DATA Bands       PROTECTED INIT {}

   METHOD CreateWnd()
   METHOD CoolBarProc()
   METHOD Delete()
   METHOD DelControl()

ENDCLASS

METHOD Delete() CLASS TCoolBar
   local n
   if( n := ascan( ::Parent:Controls, {|o|o:handle==::handle} ) )>0
      __objDelData( ::Parent, UPPER(::name ))
      adel( ::Parent:Controls, n, .t. )
      ::Destroy()
   endif
   RETURN(self)


METHOD DelControl() CLASS TCoolBar
   
   LOCAL n
   
   IF( n := aScan( ::Parent:Controls, {|o|o:Handle==::Handle} ) ) > 0
      __objDelData( ::Parent, UPPER( ::Name ) )
      aDel( ::Parent:Controls, n, .t. )
      ::Destroy()
   ENDIF
   
RETURN(self)

*------------------------------------------------------------------------------*


METHOD CoolBarProc(nMsg,nwParam,nlParam) CLASS TCoolBar

   LOCAL acRect
   LOCAL aRect

   acRect:=GetClientRect(::Parent:handle)
   aRect :=GetWindowRect(::handle)
      
   ::FWidth := acRect[3]
   ::FHeight:= aRect[4]-aRect[2]
      
   ::Move( , , , , .T. )
      
   RETURN 0

*------------------------------------------------------------------------------*

METHOD CreateWnd() CLASS TCoolBar
   ::Super:CreateWnd()
   
   ::nrProc := SetProcedure( ::Parent:FHandle,{|hWnd, nMsg,nwParam,nlParam|;
                            ::CoolBarProc(nMsg,nwParam,nlParam)}, WM_SIZE )
   ::CoolBarProc( WM_SIZE,0,0)
   
RETURN NIL

//---------------------------------------------------------------------------------------------

CLASS TCoolBand
   DATA oStruct PROTECTED
   DATA Index   PROTECTED
   DATA Parent  PROTECTED

   DATA BorderStyles    AS ARRAY INIT { 0, RBBS_CHILDEDGE }
   DATA BreakStyles     AS ARRAY INIT { 0, RBBS_BREAK }
   DATA FixedBmpStyles  AS ARRAY INIT { 0, RBBS_FIXEDBMP }
   DATA FixedSizeStyles AS ARRAY INIT { 0, RBBS_FIXEDSIZE }
   DATA GripperStyles   AS ARRAY INIT { RBBS_GRIPPERALWAYS, RBBS_NOGRIPPER }
   
   PROPERTY MinHeight READ FMinHeight WRITE SetMinHeight
   PROPERTY MinWidth  READ FMinWidth  WRITE SetMinWidth
   PROPERTY Width     READ FWidth     WRITE SetWidth
   PROPERTY Text      READ FText      WRITE SetText 
   PROPERTY Grippers  READ FGrippers  WRITE SetGrippers

   METHOD Create()
   METHOD SetChild()
   METHOD SetWidth()
   METHOD SetMinWidth()
   METHOD SetMinHeight()
   METHOD SetText()
   METHOD SetGrippers()
ENDCLASS

METHOD Create( oOwner ) CLASS TCoolBand

   ::Parent := oOwner
   ::oStruct IS REBARBANDINFO

   ::oStruct:cbSize     := ::oStruct:sizeof()
   ::oStruct:fMask      := RBBIM_STYLE
   ::oStruct:fStyle     := RBBS_GRIPPERALWAYS + RBBS_NOVERT

   ::Index := oOwner:SendMessage( RB_GETBANDCOUNT, 0, 0 )
   oOwner:SendMessage( RB_INSERTBAND, -1, ::oStruct:value )

   aAdd( oOwner:Bands, Self )

RETURN Self

METHOD SetGrippers( Value ) CLASS TCoolBand
   IF Value
      ::oStruct:fStyle := OR( ::oStruct:fStyle, RBBS_GRIPPERALWAYS )
   ELSE
      ::oStruct:fStyle := AND( ::oStruct:fStyle, NOT( RBBS_GRIPPERALWAYS ) )
      ::oStruct:fStyle := OR( ::oStruct:fStyle, RBBS_NOGRIPPER )
   ENDIF
   ::oStruct:fMask := RBBIM_STYLE
   ::Parent:SendMessage( RB_SETBANDINFO, ::Index, ::oStruct:value )
RETURN Self

METHOD SetChild( oChild ) CLASS TCoolBand
   ::oStruct:fMask      := RBBIM_CHILD
   ::oStruct:hwndChild  := oChild:Handle
   ::Parent:SendMessage( RB_SETBANDINFO, ::Index, ::oStruct:value )
RETURN Self

METHOD SetMinWidth( Value ) CLASS TCoolBand
   ::oStruct:fMask      := RBBIM_CHILDSIZE
   ::oStruct:cxMinChild := Value
   ::Parent:SendMessage( RB_SETBANDINFO, ::Index, ::oStruct:value )
RETURN Self

METHOD SetMinHeight( Value ) CLASS TCoolBand
   ::oStruct:fMask      := RBBIM_CHILDSIZE
   ::oStruct:cyMinChild := Value
   ::Parent:SendMessage( RB_SETBANDINFO, ::Index, ::oStruct:value )
RETURN Self

METHOD SetWidth( Value ) CLASS TCoolBand
   ::oStruct:fMask      := RBBIM_SIZE
   ::oStruct:cx := Value
   ::Parent:SendMessage( RB_SETBANDINFO, ::Index, ::oStruct:value )
RETURN Self

METHOD SetText( Value ) CLASS TCoolBand
   ::oStruct:fMask      := RBBIM_TEXT
   ::oStruct:lpText := Value
   ::Parent:SendMessage( RB_SETBANDINFO, ::Index, ::oStruct:value )
RETURN Self

