static oAppl

#include "hbclass.ch"
#include "what32.ch"
#include "windows.ch"

CLASS Application
   
   DATA Instance
   DATA handle
   DATA aForms                INIT {}
   DATA nFormCount            INIT 0
   DATA FrameCreated AS LOGIC INIT .F.
   METHOD New() CONSTRUCTOR
   METHOD Run()
   METHOD CreateForm()
   METHOD CreateFrame()
   
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
   local n
   DEFAULT oParent TO self
   
   IF ( n := aScan( ::aForms, {|a|a[1] == cForm} ) ) > 0
      IF ::aForms[n][2] == NIL
         MessageBox(, 'Form '+cForm+' is acting as the Frame',MB_ICONEXCLAMATION )
         return( nil )
      endif
      ::aForms[n][2]:Create()
      return( ::aForms[n][2] )
   endif
   
   __objAddData( self, cForm )
   oForm := if( oForm != NIL, oForm:New( oParent ), TForm():New( oParent ) )
   __ObjSetValueList( self, { { cForm, oForm } } )
   
   aAdd( ::aForms, {cForm,oForm} )
   oForm:Create()
   
return( oForm )

METHOD CreateFrame( cName, oFrame ) CLASS Application
   local n
   if ::FrameCreated
      IF ( n := aScan( ::aForms, {|a|a[2] == NIL } ) ) > 0
         MessageBox(, 'Frame '+::aForms[1]+' is already created',MB_ICONEXCLAMATION )
         return( nil )
      endif
   endif
   ::FrameCreated := .T.
   __objAddData( self, cName )
   oFrame := if( oFrame != NIL, oFrame:New( self ), TFrame():New( self ) )
   __ObjSetValueList( self, { { cName, oFrame } } )
   aAdd( ::aForms, {cName,nil} )
   oFrame:Create()
return( oFrame )
