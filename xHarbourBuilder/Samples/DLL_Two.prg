#include "hbclass.ch"

static s_cInitMessage := "Init from DllTwo!"

FUNCTION TestInterDllTwo( bInterDll )

RETURN Eval( bInterDll )

CLASS DllTwo

   VAR cText INIT "Some text"

   METHOD New( cText ) CONSTRUCTOR

ENDCLASS

METHOD New( cText ) CLASS DllTwo

   ::cText := cText

RETURN Self

// Init Procedure inside a DLL.
INIT PROCEDURE InitDllTwo()

   // Using Static
   Alert( s_cInitMessage )

RETURN
