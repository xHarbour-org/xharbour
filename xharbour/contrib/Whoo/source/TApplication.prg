/*
 * xHarbour Project source code:
 *
 * Whoo.lib TApplication CLASS module
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


GLOBAL oAppl

#include "hbclass.ch"
#include "what32.ch"
#include "windows.ch"
#include "debug.ch"
#include "wingdi.ch"

GLOBAL EXTERNAL lPrevInstance

*------------------------------------------------------------------------------*

CLASS Application

   DATA Instance
   DATA handle
   DATA nFormCount            INIT 0
   DATA FrameCreated AS LOGIC INIT .F.
   DATA MultiInstance         INIT .F.
   DATA InstMsg               INIT NIL 

   METHOD Initialize() CONSTRUCTOR
   METHOD Run()
   METHOD CreateForm()
   METHOD CreateFrame()
   METHOD Terminate()                            INLINE PostQuitMessage(0)
   METHOD MessageBox( cText, cCaption, nFlags )  INLINE MessageBox( GetActiveWindow(), cText, cCaption, nFlags )

ENDCLASS

*------------------------------------------------------------------------------*

METHOD Initialize() CLASS Application

   LOCAL nId, cMsg

   IF !::MultiInstance
      ::InstMsg := RegisterWindowMessage( GetModuleFileName() )
      // AllowSetForegroundWindow( -1 )
      IF lPrevInstance
         SendMessage( HWND_BROADCAST, ::InstMsg, 0, 0)
         PostQuitMessage(0)
         QUIT
         return(0)
      ENDIF
   ENDIF
   ::Instance := hInstance()
  
   oAppl := Self

   RETURN(self)

*------------------------------------------------------------------------------*

METHOD Run() CLASS Application

   LOCAL cMsg

   DO WHILE GetMessage( @cMsg, 0, 0, 0 )
      IF !IsDialogMessage( , cMsg )
         TranslateMessage( cMsg )
         DispatchMessage( cMsg )
      ENDIF
   ENDDO

   RETURN(0)

*------------------------------------------------------------------------------*

METHOD CreateForm( cForm, oForm, oParent ) CLASS Application

   LOCAL n

   DEFAULT oParent TO self

   __objAddData( self, cForm )
   oForm := if( oForm != NIL, oForm:New( oParent ), TForm():New( oParent ) )
   __ObjSetValueList( self, { { cForm, oForm } } )
   oForm:propname:=cForm
   oForm:Create()

   RETURN( oForm )

*------------------------------------------------------------------------------*

METHOD CreateFrame( cName, oFrame ) CLASS Application

   LOCAL n

   IF ::FrameCreated
      MessageBox(, 'Frame is already created',MB_ICONEXCLAMATION )
      RETURN( NIL )
   ENDIF
   ::FrameCreated := .T.
   __objAddData( self, cName )
   oFrame := if( oFrame != NIL, oFrame:New( self ), TFrame():New( self ) )
   __ObjSetValueList( self, { { cName, oFrame } } )
   oFrame:propname:=cName
   oFrame:Create()
   
   RETURN( oFrame )


*------------------------------------------------------------------------------*
