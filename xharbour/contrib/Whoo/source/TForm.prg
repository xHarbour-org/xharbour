/*
 * $Id: xTree.prg,v 1.4 2002/10/10 02:51:46 what32 Exp $
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

CLASS TForm FROM TWindow
   DATA WindowMenu   EXPORTED
   DATA Modal        PROTECTED INIT .F.
   DATA resname      PROTECTED
   DATA biSystemMenu EXPORTED INIT .T.
   DATA biMinimize   EXPORTED INIT .T.
   DATA biMaximize   EXPORTED INIT .T.
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
