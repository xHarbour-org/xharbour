/*
 * $Id: TForm.prg,v 1.42 2002/10/13 11:16:29 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TForm CLASS
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
#include "debug.ch"
#include "what32.ch"

#include "error.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

*-----------------------------------------------------------------------------*

CLASS TForm FROM TWinControl
   DATA WindowMenu   EXPORTED
   DATA Modal        PROTECTED INIT .F.
   DATA resname      PROTECTED
   DATA xhBorder     PROTECTED

//-------------------------------------------------------------------------------------------
   ACCESS biSystemMenu    INLINE AND( ::Style, WS_SYSMENU ) # 0
   ASSIGN biSystemMenu(l) INLINE ::SetStyle(WS_SYSMENU,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE ),l

   ACCESS biMinimize      INLINE AND( ::Style, WS_MAXIMIZEBOX ) # 0
   ASSIGN biMinimize(l)   INLINE ::SetStyle(WS_MAXIMIZEBOX,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE )

   ACCESS biMaximize      INLINE AND( ::Style, WS_MINIMIZEBOX ) # 0
   ASSIGN biMaximize(l)   INLINE ::SetStyle(WS_MINIMIZEBOX,l),;
                                 ::Style := GetWindowLong( ::handle, GWL_STYLE )

//-------------------------------------------------------------------------------------------

   ACCESS bsSizeable    INLINE {WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+WS_MINIMIZEBOX+;
                                         WS_MAXIMIZEBOX+WS_THICKFRAME,  0 }
   ACCESS bsSingle      INLINE {WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+WS_MINIMIZEBOX+;
                                         WS_MAXIMIZEBOX, 0 }

   ACCESS bsDialog      INLINE {WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+DS_MODALFRAME, 0 }

   ACCESS bsNone        INLINE {WS_POPUP+WS_VISIBLE, 0 }
   ACCESS bsSizeToolWin INLINE {WS_OVERLAPPEDWINDOW, WS_EX_TOOLWINDOW }
   ACCESS bsToolWindow  INLINE {WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+;
                                         DS_MODALFRAME, WS_EX_TOOLWINDOW }


   ACCESS BorderStyle    INLINE iif( ::xhBorder != NIL, ::xhBorder, "bsSizeable" )
   ASSIGN BorderStyle(c) INLINE ::xhBorder := c,;
                                ::SetLong( GWL_STYLE, __objSendMsg( self, c )[1] ),;
                                ::SetLong( GWL_EXSTYLE, __objSendMsg( self, c )[2] ),;
                                ::Style := ::GetLong( GWL_STYLE ),;
                                ::ExStyle := ::GetLong( GWL_EXSTYLE ),;
                                InvalidateRect( ::handle )

//-------------------------------------------------------------------------------------------

   METHOD New()
   METHOD Add()
   METHOD Del()
   METHOD ChildFromHandle( hHandle )
   METHOD ChildFromId( hHandle )
   METHOD GetObj()
   METHOD SetLink()
ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS TForm

   ::WndProc   := IFNIL(::WndProc,'FormProc',::WndProc)
   ::Msgs      := IFNIL(::Msgs,-1,::Msgs)
   ::FrameWnd  := .F.
   ::Style     := IFNIL(::Style,WS_OVERLAPPEDWINDOW,::Style)
   ::FormType  := IFNIL(::FormType,RCF_WINDOW,::FormType)
   ::lRegister := IFNIL(::lRegister,.T.,::lRegister)
   ::lControl  := .F.
   ::ExStyle   := IFNIL(::ExStyle,0,::ExStyle)
   ::PropName  := IFNIL(::PropName,"TForm",::PropName)
   InitCommonControls()

   RETURN( super:New( oParent ) )

*-----------------------------------------------------------------------------*

METHOD Add( cName, oObj, lCreate ) CLASS TForm
   LOCAL nSeq, hClass

   DEFAULT lCreate TO .T.

   oObj:propname := cName

   __objAddData( self, cName, HB_OO_CLSTP_PROTECTED )
   __ObjSetValueList( self, { { cName, oObj } } )

   IF lCreate
      oObj:Create()
   endif
   AADD( ::Controls, oObj )
   RETURN( oObj )

*-----------------------------------------------------------------------------*

METHOD Del( cName ) CLASS TForm

   local n
   if (n := aScan( ::Controls, {|o| lower(o:name) == lower(cName)} ) ) > 0
      ::Controls[n]:Delete()
   endif
   RETURN( .t. )

*-----------------------------------------------------------------------------*

METHOD ChildFromHandle( hHandle ) CLASS TForm

   local n
   if (n := aScan( ::Controls, {|o| o:handle == hHandle} ) ) > 0
      return( ::Controls[n] )
   endif
   RETURN(nil)

*-----------------------------------------------------------------------------*

METHOD ChildFromId( nId ) CLASS TForm

   local n
   if (n := aScan( ::Controls, {|o| o:id == nId} ) ) > 0
      return( ::Controls[n] )
   endif
   return(nil)

*-----------------------------------------------------------------------------*

METHOD GetObj( cName ) CLASS TForm
   local n:= ASCAN( ::Controls,{|o|o:propname==cName} )
   if n>0
      return( ::Controls[n] )
   endif
return(nil)

*-----------------------------------------------------------------------------*

METHOD SetLink( cName, oObj ) CLASS TForm
   __objAddData( self, cName )
   __ObjSetValueList( self, { { cName, oObj } } )
return( oObj )
