**********************************************
* Class debug demo.
*
* This program demonstrates how to debug a class using
* __SetClassScope(), __objGetMsgFullList() and __objGetValueFullList()
* functions.
*
* On Windows best viewed with DebugView (from www.sysinternals.com)
*
* (C) 2003 Francesco Saverio Giudice
*
* $Id: service.prg,v 1.3 2003/10/20 02:39:29 jonnymind Exp $
*

#include "common.ch"
#include "hbclass.ch"

#xcommand IF <x> THEN <*y*> => IF <x> ;; <y> ;; END

PROCEDURE Main( cNoWait )

  LOCAL a := TAssociativeArray()
  LOCAL b := TAssociativeArray()
  LOCAL t
  LOCAL lWait := TRUE

  CLEAR SCREEN

  IF cNoWait <> NIL .AND. Upper( cNoWait ) == "/N"
     lWait := FALSE
  ENDIF

  t := Test( 100 )  // Declaring t as Test Object autoinizializing with a value.
                    // Look at CONSTRUCTOR in class definition

  a['Test1'] := Date()
  a['TEST2'] := "Francesco"
  a['Test3'] := 10

  b:Var1 := 10             // This will be stored in UPPER case
  b:Var2 := .t.            // This will be stored in UPPER case
  b:SendKey( "Var3", 20 )  // This will be stored as sent
  b['VaR4'] := { "This", "is", "an", "array", ;  // As above
                 "of", 11, "items", Date(), .T., { "Array", "of", 4, "items" }, {"AssocArray" => .t. } }
  b['Var5'] := ErrorNew()  // An object

  t:Add( "x"  , 20 )
  t:Add( "one", { "This", "is", "an", "array", "of", 11, "items", Date(), .T., { "Array", "of", 4, "items" }, {"AssocArray" => .t. } } )
  t:Add( "two", 40 )

  //? a['Test2']       // Uncomment this to get error because TEST2 is declared in UPPER case

  Write( "Display value of: a" )
  Write( HB_DumpVar( a ) )
  Write( "" )
  IF lWait THEN Write( "Press any key to continue" )
  IF lWait THEN Inkey( 0 )
  Write( "" )
  Write( "Display keys of var a" )
  Write( HB_DumpVar( a:Keys ) )
  Write( "" )
  IF lWait THEN Write( "Press any key to continue" )
  IF lWait THEN Inkey( 0 )
  Write( "" )
  Write( "Display value of var b" )
  Write( HB_DumpVar( b ) )
  Write( "" )
  IF lWait THEN Write( "Press any key to continue" )
  IF lWait THEN Inkey( 0 )
  Write( "" )
  Write( "Display value of var b recurively" )
  Write( HB_DumpVar( b,, TRUE ) )
  Write( "" )
  IF lWait THEN Write( "Press any key to continue" )
  IF lWait THEN Inkey( 0 )
  Write( "" )
  Write( "Display value of var t that is an OBJECT" )
  Write( HB_DumpVar( t ) )
  Write( "" )
  IF lWait THEN Write( "Press any key to continue" )
  IF lWait THEN Inkey( 0 )
  Write( "" )
  Write( "Display value of: t:var3" )
  t:Dump()
  Write( "" )
  IF lWait THEN Write( "Press any key to continue" )
  IF lWait THEN Inkey( 0 )
  Write( "" )
  Write( "Display the object TASSOCIATIVEARRAY itself" )
  Write( HB_DumpVar( a, TRUE ) )
  Write( "" )
  IF lWait THEN Write( "Press any key to end" )
  IF lWait THEN Inkey( 0 )
  Write( "" )
  Write( "End of test" )

RETURN

CLASS TestParent
   METHOD MyConstructorParent CONSTRUCTOR
ENDCLASS

METHOD MyConstructorParent() CLASS TestParent
  // Notinhg to do
RETURN Self

CLASS Test FROM TestParent

   DATA nVar1 INIT 1
   DATA cVar2 INIT "Test"

   METHOD Test()
   CONSTRUCTOR Test() // Here we can have any method name.
                      // CONSTRUCTOR Test() syntax is equal to METHOD Test() CONSTRUCTOR
   METHOD Dump()
   METHOD Add( cKey, xVal )  INLINE ::aVar3:SendKey( cKey, xVal )

 PUBLISHED:
   DATA bVar4 INIT {|| TRUE }

   METHOD MyPublishedMethod()   VIRTUAL

 PROTECTED:
   DATA aVar5 INIT { 1, 2, "3" }

   METHOD MyProtectedMethod()

 HIDDEN:
   DATA lVar6 INIT FALSE
   DATA nVar7
   DATA aVar3 INIT TAssociativeArray()

   METHOD MyHiddenMethod()      INLINE Self  // On dump look at inline method type

ENDCLASS

METHOD Test( nInt ) CLASS Test
   ::aVar3 := TAssociativeArray()
   ::nVar7 := nInt
RETURN Self

METHOD Dump() CLASS Test
   Write( HB_DumpVar( ::aVar3 ) )
RETURN Self

METHOD MyProtectedMethod() CLASS Test
RETURN Self



#define CRLF() (CHR(13)+CHR(10))

// ********************************************* ------------------------------- *************************

STATIC PROCEDURE Write( x )
  x += CRLF()
  OutStd( x )
  HB_OutDebug( x )
RETURN

