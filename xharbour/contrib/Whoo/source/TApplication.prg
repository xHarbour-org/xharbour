static oAppl

#include "hbclass.ch"

CLASS Application
   
   DATA Instance

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

METHOD CreateForm( oForm ) CLASS Application
   
   oForm:New()
  
   oForm:Create()

return(oForm)

function App() ; return( oAppl )
