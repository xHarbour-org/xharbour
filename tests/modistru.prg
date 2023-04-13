PROCEDURE Main( cFile )

   SetMode( 25, 80 )

   IF Empty( cFile )
      ? "Syntax: modistru <cFile>"
      QUIT
   ENDIF

   IF Alert( "Modify structure of: " + cFile + " are you sure?", { "No", "Yes" }  ) == 2
      IF dbModifyStructure( cFile )
         Alert( "File modified successfuly" )
      ELSE
         Alert( "Sorry, Structure modification failed" )
      ENDIF
   ELSE
      ? "Program aborted by user."
   ENDIF

RETURN

