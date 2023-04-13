#include "hbclass.ch"

Procedure Main()

    // Locally defined CLASS.
    LOCAL oTest := Test()

    // Get Pointer to Message Test of oTest.
    LOCAL nPtr := HB_ObjMsgPtr( oTest, "Test" )
    LOCAL cVar := "Hello"

    // Execute: oTest:Test( 1, 2, 3 )
    ? HB_Exec( nPtr, oTest, 1, 2, 3 )

    // RTL Class TGet.
    oTest := GetNew( 2, 10, {|_1| IIF( _1 == NIL, cVar, cVar := _1 ) }, "CVAR" )
    // Get Pointer to Message VarGet of oTest.
    nPtr := HB_ObjMsgPtr( oTest, "VarGet" )
    // Execute: oTest:VarGet()
    ? HB_Exec( nPtr, oTest )

    // Get Pointer to FUNCTION MyTest()
    nPtr := HB_FuncPtr( "MyTest" )
    // Execute: MyTest( 10, 20 )
    ? HB_Exec( nPtr, , 10, 20 )

Return

Function MyTest( p1, p2, p3 )

   TraceLog( QSelf(), p1, p2, p3 )

Return 7

CLASS TEST
    METHOD Test
END CLASS

METHOD TEST( p1, p2, p3 ) CLASS TEST

   TraceLog( QSelf(), p1, p2, p3 )

RETURN 77
