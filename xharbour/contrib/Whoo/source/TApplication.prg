static oAppl

#include "hbclass.ch"
#include "what32.ch"

CLASS Application
   
   DATA Instance
   DATA handle
   
   METHOD New() CONSTRUCTOR
   METHOD Run()
   METHOD CreateForm()
   
ENDCLASS


METHOD New() CLASS Application

   ::Instance := hInstance()

   InitCommonControls()

   oAppl := Self

RETURN(self)


METHOD Run() CLASS Application
   
   local cMsg
   
   DO WHILE GetMessage( @cMsg, 0, 0, 0 )
      IF !IsDialogMessage( , cMsg )
         TranslateMessage( cMsg )
         DispatchMessage( cMsg )
      ENDIF
   ENDDO

RETURN(0)

METHOD CreateForm( cForm, oForm, oParent ) CLASS Application

   DEFAULT oParent TO self
   
   __objAddData( self, cForm )
   
   oForm := if( oForm != NIL, oForm:New( oParent ), TForm():New( oParent ) )

   __ObjSetValueList( self, { { cForm, oForm } } )

   oForm:Create()
   
return( oForm )
