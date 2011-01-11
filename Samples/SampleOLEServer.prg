#pragma BEGINDUMP
   #define CLS_Name "SampleOleServer"
   #define CLS_ID   "{D385CFF0-01BE-42ea-8272-336CDE8ABA5A}"
   #include "OleServer.h"
#pragma ENDDUMP

#include "hbclass.ch"

#define DLL_PROCESS_ATTACH  1
#define DLL_PROCESS_DETACH  0

REQUEST HB_GT_NUL_DEFAULT

/*
  OPTIONAL Procedure - Here we can create PUBLIC Variables to be exported
  and intialize any values. This Procedure will be called once when loading
  the Dll with nReason == DLL_PROCESS_ATTACH, and once again when unloading
  the Dll with nReason == DLL_PROCESS_DETACH.
*/
PROCEDURE DllMain( hInstance, nReason )

   SWITCH nReason
      CASE DLL_PROCESS_ATTACH
         TraceLog( "Dll Loaded." )
         EXIT

      CASE DLL_PROCESS_DETACH
         TraceLog( "Dll UNloaded." )
         EXIT

      DEFAULT
         TraceLog( "UNEXPECTED nReaon in DllMain!" )
   END

RETURN

/*
  OPTIONAL FUNCTION - if exists it will set the Server to a specific Object Instance.
  or you may return the same Object previously created if you want all OLE intanaces
  to refer to just one Object.
*/
FUNCTION CreateInstance

   LOCAL oOleServer := MyOleServer()

   // Transfer all errors to the Client
   ErrorBlock( {|e| Break(e) } )

   /*
     If you return an Object, it becomes *the* Server, otherwise the non ststic PROCEDURES and FUNCTIONS
     of this module will be the exported Methods, and PUBLICS MEMVARS will be the exported Properties.
    */

RETURN oOleServer

CLASS MyOleServer
   PUBLIC:

   DATA MyProperty

   METHOD MyMethod( x )
   METHOD ThrowError() INLINE Throw( ErrorNew( [SampleServer], 1001, [ThrowError], [SampleError], { "Demo" } )  )
ENDCLASS

METHOD MyMethod( x ) CLASS MyOleServer

   ::MyProperty := x

   IF HB_IsByRef( @x )
      SWITCH ValType( x )
         CASE 'C'
            x := "Modified!"
            EXIT

         CASE 'D'
         CASE 'N'
            x++
            EXIT

         CASE 'A'
            x := { "Modified!" }
            EXIT
      END
   ENDIF

RETURN Self
