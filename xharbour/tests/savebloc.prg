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
      ? "Creating Object..."
      oObj := MyClass()

      ? "Setting value using native Object Data Block..."
      Eval( oObj:Data, "Test" )

      ? "Getting value using native Object Data Block..."
      ? Eval( oObj:Data )

      ?
      ? "Saving Object Data Block..."
      SavedBlock := HB_SaveBlock( oObj:Data )

      ? "Releasing Object Data Block..."
      bBlock := NIL

      ? "Restoring Object Data Block..."
      oObj:Data := HB_RestoreBlock( SavedBlock )

      ?
      ? "Getting value using Restored Object Data Block..."
      ? Eval( oObj:Data )

      ?
      ? "Save/Restore Block completed successfuly."
      ?

   CATCH oErr
      ? "Caught:", oErr:Description
   END

RETURN

FUNCTION TestFunction()

RETURN "Local Function"

CLASS MyClass

   DATA cData PRIVATE
   DATA Data INIT {|c| TraceLog( HB_QSelf() ), IIF( PCount() == 0, HB_QSelf():cData, HB_QSelf():cData := c ) }

ENDCLASS
