#pragma BEGINDUMP
   #define CLS_Name "HTTPData"
   #define CLS_ID   "{820503AA-BFD0-4bb6-8FFE-24908EDBBDA5}"
   #include "OleServer.h"
#pragma ENDDUMP

#include "hbclass.ch"

#define DLL_PROCESS_ATTACH  1
#define DLL_PROCESS_DETACH  0

PROCEDURE DllMain( hInstance, nReason )

   SWITCH nReason
      CASE DLL_PROCESS_ATTACH
         TraceLog( "Dll Loaded at " + Time() )
         EXIT

      CASE DLL_PROCESS_DETACH
         TraceLog( "Dll UNloaded at " + Time() )
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

   LOCAL oOleServer := HTTPData()
   
   TraceLog("In CreateInstance()")

   // Transfer all errors to the Client
   ErrorBlock( {|e| Break(e) } )
   

RETURN oOleServer