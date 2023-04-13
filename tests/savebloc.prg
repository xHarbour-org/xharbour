/*
  NOTE: This sample must be linked with restbloc.prg
*/

#include "hbclass.ch"

STATIC s_Name := ", Some Static"

PROCEDURE Main()

   LOCAL cLocal, bBlock, SavedBlock, oErr, oObj

   // Note Block accesses a Local Paramater, a Local Function, an External Function, and a Module Static!
   bBlock := {|_1| StrTran( TestFunction(), " ", " [and External] " ) + s_Name + _1 }

   CLS

   TRY      
      ? "Evaluating native Block..."
      ? Eval( bBlock, ", and Local Param." )

      ?
      ? "Saving Block..."
      SavedBlock := HB_SaveBlock( bBlock )

      ? "Releasing Block..."
      bBlock := NIL

      ? "Restoring Block..."
      bBlock := HB_RestoreBlock( SavedBlock )

      ?
      ? "Evaluating Restored Block..."
      ? Eval( bBlock, ", and Local Param." )      

      ?
      ? "Forward persisted block to foriegn module restblock.prg"
      ? EvalSavedBlock( SavedBlock )
      
      ?
      ? "Save/Restore Block completed successfuly."
      ?

   CATCH oErr
      ? "Caught:", oErr:Description
   END

RETURN

FUNCTION TestFunction()

RETURN "Local Function"