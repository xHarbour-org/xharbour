/*
 * $Id: TApplication.prg,v 1.26 2002/10/11 03:53:16 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TApplication CLASS module
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


GLOBAL oAppl

#include "hbclass.ch"
#include "what32.ch"
#include "windows.ch"
#include "debug.ch"
#include "wingdi.ch"
#include "tabctrl.ch"

GLOBAL EXTERNAL lPrevInstance

*------------------------------------------------------------------------------*

CLASS Application

   DATA Instance
   DATA handle
   DATA nFormCount            INIT 0
   DATA FrameCreated AS LOGIC INIT .F.
   DATA MultiInstance         INIT .F.
   DATA InstMsg               INIT NIL
   DATA aForms                INIT {}
   
   METHOD Initialize() CONSTRUCTOR
   METHOD Run()
   METHOD CreateForm()
   METHOD RemoveForm()
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

   LOCAL aVars, aVar
   
   DEFAULT oParent TO self

   __objAddData( self, cForm )
   oForm := if( oForm != NIL, oForm:New( oParent ), TForm():New( oParent ) )
   __ObjSetValueList( self, { { cForm, oForm } } )
   oForm:propname := cForm
   oForm:Create()
   
   IF oParent:handle == ::handle
      aAdd( ::aForms, oForm )
   ENDIF
   
   aVars := __objGetValueList( oForm, NIL, HB_OO_CLSTP_EXPORTED )
   FOR EACH aVar IN aVars
      IF ValType( aVar[2] ) == 'O'
         aAdd( oForm:Controls, aVar[2] )
         WITH OBJECT aVar[2]
            :Parent    := oForm
            :Instance  := oForm:Instance
            :Create()
         END WITH
      ENDIF
   NEXT

RETURN( oForm )

*------------------------------------------------------------------------------*

METHOD RemoveForm( oForm ) CLASS Application
   local nRet, n
   IF( n:= aScan( ::aForms,{|o|o:handle == oForm:handle} ) )>0
      aDel( ::aForms, n, .T. )
      IF LEN( ::aForms ) == 0
         nRet := 1
      ENDIF
   ENDIF
return(nRet)

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

FUNCTION WinClass( cClass )

   DO CASE
    CASE cClass == "TBUTTON"
       RETURN "button"

    CASE cClass == "TEDIT"
       RETURN "edit"

    CASE cClass == "TSTATIC"
       RETURN "static"

    CASE cClass == "TCHECK"
       RETURN "edit"

    CASE cClass == "TRADIO"
       RETURN "button"

    CASE cClass == "TCOMBOBOX"
       RETURN "combobox"

    CASE cClass == "TLISTBOX"
       RETURN "listbox"

    CASE cClass == "TSTATUSBAR"
       RETURN "msctls_statusbar32"

    CASE cClass == "TTABCONTROL"
       RETURN WC_TABCONTROL

    CASE cClass == "TGROUPBOX"
       RETURN "button"
   END CASE

 RETURN NIL

*------------------------------------------------------------------------------*
