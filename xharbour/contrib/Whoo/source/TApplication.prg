// Augusto Infante
// Whoo.lib


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
   InitCommonControls()
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
   oFrame:Create()
   
   RETURN( oFrame )


*------------------------------------------------------------------------------*