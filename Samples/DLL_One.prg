#include "hbclass.ch"

MEMVAR g_cExitMessage

FUNCTION TestInterDllOne( bInterDll )

RETURN Eval( bInterDll )

CLASS DllOne

   VAR cText INIT "Some text"

   METHOD New( cText ) CONSTRUCTOR

ENDCLASS

METHOD New( cText ) CLASS DllOne

   ::cText := cText

RETURN Self

EXIT PROCEDURE ExitDllOne()

   Alert( g_cExitMessage )

RETURN
