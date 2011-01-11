PROCEDURE Main()

   LOCAL oServer, xByRef

   TRY
      oServer := GetActiveObject( "SampleOleServer" )
   CATCH
    TRY
         oServer := CreateObject( "SampleOleServer" )
      CATCH
        ? ValToPrg( "ERROR! SampleOleserver not avialable. [" + Ole2TxtError()+ "]" )
         RETURN
      END
   END

   oServer:MyMethod( "test" )
   ? ValToPrg( oServer:MyProperty )

   oServer:MyMethod( .F. )
   ? ValToPrg( oServer:MyProperty )

   oServer:MyMethod( Date() )
   ? ValToPrg( oServer:MyProperty )

   oServer:MyMethod( {1} )
   ? ValToPrg( oServer:MyProperty )

   ?
   ? "Now ByRef..."

   xbYRef := "Hello"
   oServer:MyMethod( @xByRef )
   ? ValToPrg( xByRef )

   xbYRef := 1
   oServer:MyMethod( @xByRef )
   ? ValToPrg( xByRef )

   xbYRef := Date()
   oServer:MyMethod( @xByRef )
   ? ValToPrg( xByRef )

   xbYRef := { 1 }
   oServer:MyMethod( @xByRef )
   ? ValToPrg( xByRef )

   WAIT
RETURN