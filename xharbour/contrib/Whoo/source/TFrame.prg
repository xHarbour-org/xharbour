/*
 * xHarbour Project source code:
 *
 * Whoo.lib TFrame CLASS
 *
 * Copyright 2002 Augusto Infante [systems@quesoro.com]
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
#include "error.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

*-----------------------------------------------------------------------------*

CLASS TFrame FROM TWindow
   DATA WindowMenu   EXPORTED
   DATA Modal        EXPORTED INIT .F.
   DATA biSystemMenu EXPORTED INIT .T.
   DATA biMinimize   EXPORTED INIT .T.
   DATA biMaximize   EXPORTED INIT .T.
   METHOD New()
   METHOD Add()
   METHOD SetLink()
   METHOD GetObj()
ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS TFrame

   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .T.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::ExStyle   := WS_EX_APPWINDOW
   ::FormType  := RCF_WINDOW
   ::lRegister := .T.
   InitCommonControls()

   return( super:New( oParent ) )

*-----------------------------------------------------------------------------*

METHOD Add( cName, oObj, lCreate ) CLASS TFrame
   
   DEFAULT lCreate TO .T.
   oObj:propname := cName
   ::SetLink( cName, oObj )
   IF lCreate
      oObj:Create()
   endif
   AADD( ::Controls, oObj )

   return( oObj )

*-----------------------------------------------------------------------------*

METHOD SetLink( cName, oObj ) CLASS TFrame
   local hClass, nSeq
   IF !ISCHARACTER( cName )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
     ELSEIF !__objHasMsg( self, cName ) .AND. !__objHasMsg( self, "_" + cName )
      hClass := ::ClassH
      nSeq   := __cls_IncData( hClass )
      __clsAddMsg( hClass,       cName, nSeq, HB_OO_MSG_DATA, NIL, HB_OO_CLSTP_PROTECTED )
      __clsAddMsg( hClass, "_" + cName, nSeq, HB_OO_MSG_DATA, NIL, HB_OO_CLSTP_PROTECTED )
   ENDIF
   __ObjSetValueList( self, { { cName, oObj } } )
return( oObj )

*-----------------------------------------------------------------------------*

METHOD GetObj( cName ) CLASS TFrame
   local n:= ASCAN( ::Controls,{|o|o:propname==cName} )
   if n>0
      return( ::Controls[n] )
   endif
return(nil)
