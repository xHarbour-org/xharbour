#include "hbclass.ch"

Procedure Main()

   LOCAL oChild := MyChild()

   oChild:SetMethod( "OnTest", {|| QOut( oChild:ClassName ) } )
   oChild:OnTest()

   oChild:SetMethod( "OnTest", @TestFunc() )
   oChild:OnTest()

   oChild:SetMethod( "DynMethod", @TestFunc() )
   oChild:DynMethod()

   __ObjSetMethod( oChild, "OnTest", {|| QOut( "Function Level" ) } )
   oChild:OnTest()

   __ObjSetMethod( oChild, "NewTest", {|| QOut( "Function Level" ) } )
   oChild:NewTest()

Return

PROCEDURE TestFunc()

   ? ProcName()

Return

CLASS MyParent
   METHOD SetMethod( cMsg, FuncOrBlock, nScope ) INLINE IIF( __objHasMsg( Self, cMsg ), ;
                                                 __ClsModMsg( ::ClassH, cMsg, FuncOrBlock ), ;
                                                 __ClsAddMsg( ::ClassH, cMsg, FuncOrBlock, ;
                                                              IIF( ValType( FuncOrBlock ) == 'B', HB_OO_MSG_INLINE, HB_OO_MSG_METHOD ), ;
                                                              NIL, nScope ) )
   METHOD OnTest VIRTUAL
END CLASS

CLASS MyChild FROM MyParent
   VAR MyVar
END CLASS
