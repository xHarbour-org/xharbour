/*
 * xHarbour Project source code:
 *
 * Whoo.lib TFrame CLASS
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
#include "what32.ch"
#include "error.ch"
#include "debug.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

*-----------------------------------------------------------------------------*

CLASS TFrame FROM TWinControl
   DATA Modal        EXPORTED INIT .F.

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
   DATA WinClass    PROTECTED INIT "Frame"
   DATA ControlName PROTECTED INIT "Frame"

   METHOD Create()
   METHOD Add()
   METHOD SetLink()
   METHOD GetObj()
ENDCLASS

*-----------------------------------------------------------------------------*

METHOD Create( oOwner ) CLASS TFrame

   ::Super:Create( oOwner )

   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .T.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::ExStyle   := WS_EX_APPWINDOW
   ::FormType  := RCF_WINDOW
   ::lRegister := .T.

Return Self

*-----------------------------------------------------------------------------*

METHOD Add( oObj ) CLASS TFrame

   LOCAL oCtrl, nInst := 1

   //oObj:propname := cName

   IF oObj:Name == NIL
      FOR EACH oCtrl IN ::Controls
          IF oCtrl:ControlName == oObj:ControlName
             nInst++
          ENDIF
      NEXT

      oObj:Name := oObj:ControlName + AllTrim( Str( nInst ) )
      view oObj:Name
   ENDIF

   ::SetLink( oObj )

   AADD( ::Controls, oObj )

   return( oObj )

*-----------------------------------------------------------------------------*

METHOD SetLink( oObj ) CLASS TFrame

   LOCAL hClass, nSeq, cName := oObj:Name

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
