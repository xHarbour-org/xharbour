static oAppl

#include "hbclass.ch"

CLASS Application
   
   DATA Instance
   DATA handle
   DATA aForms    INIT {}
   
   METHOD New() CONSTRUCTOR
   METHOD Run()
   METHOD AddData( cSymbol ) INLINE __objAddData( self, cSymbol )
   
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

METHOD CreateForm( cForm, oForm ) CLASS Application

   __objAddData( self, cForm )
   
   oForm := if( oForm != NIL, oForm:New( self ), TForm():New(self) )

   __ObjSetValueList( self, { {cForm, oForm} } )

   oForm:Create()
   
return( oForm )
