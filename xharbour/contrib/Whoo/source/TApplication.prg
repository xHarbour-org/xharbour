/*
 * $Id: TApplication.prg,v 1.41 2002/11/07 20:15:58 ronpinkas Exp $
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
#include "classex.ch"

GLOBAL EXTERNAL lPrevInstance

*------------------------------------------------------------------------------*

CLASS Application

   PROPERTY OnIdle READ FOnIdle WRITE FOnIdle

   DATA Instance
   DATA InstMsg
   DATA handle
   DATA nFormCount            INIT 0
   DATA FrameCreated AS LOGIC INIT .F.
   DATA MultiInstance         INIT .F.
   DATA InstMsg               INIT NIL
   DATA aForms                INIT {}
   DATA AppForms              INIT {}

   PROPERTY MainForm READ FMainForm

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

/*
   DO WHILE .T.
      IF PeekMessage( @cMsg ) //, 0, 0, 0 )
         IF !IsDialogMessage( , cMsg )
            TranslateMessage( cMsg )
            DispatchMessage( cMsg )
         ENDIF
      ELSE
         IF ValType( ::FOnIdle ) == 'B'
            Eval( ::FOnIdle )
         ELSEIF ValType( ::FOnIdle ) == 'N'
            HB_Exec( ::FOnIdle )
         ELSE
            WaitMessage()
         ENDIF
      ENDIF
   ENDDO
*/
   RETURN(0)

*------------------------------------------------------------------------------*

METHOD CreateForm( oForm, oTarget ) CLASS Application

   LOCAL aVars, aVar

   //TraceLog( cForm, oForm, oForm:PropName )

   oForm:Create( Self )

   //oForm:Name := oForm:ClassName() //ControlName + AllTrim( Str( Len( ::AppForms ) + 1 ) )
   //__objAddData( Self, oForm:Name )
   //__ObjSetValueList( self, { { oForm:Name, oForm } } )

   //TraceLog( :Caption, :Top, :Left, :Height, :Width )

   aVars := __objGetValueList( oForm, NIL, HB_OO_CLSTP_EXPORTED )
   FOR EACH aVar IN aVars
      IF ValType( aVar[2] ) == 'O'
         aVar[2]:Create( oForm )
      ENDIF
   NEXT

   IF ::FMainForm == NIL
      oForm:GetHandle()
      ::FMainForm := oForm
   ENDIF

   oTarget := oForm

RETURN oForm

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

*------------------------------------------------------------------------------*
#pragma BEGINDUMP

#define _WIN32_WINNT   0x0400

#include<windows.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

HB_FUNC( PROCESSMESSAGES )
{
   MSG msg ;

   while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
   {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
   }
}
#pragma ENDDUMP
