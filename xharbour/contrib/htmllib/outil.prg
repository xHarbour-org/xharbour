/*
**
**    (c) 1999-2000 Manos Aspradakis, Greece
**    eMail : maspr@otenet.gr
**
*/

#include "hbclass.ch"
#include "html.ch"
#include "default.ch"

/****
*
*    aoData()
*
*
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION aoData( oObject )

   //컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
   LOCAL aInfo    := Asort( __ClassSel( oObject:ClassH() ) )
   LOCAL aData    := {}
   LOCAL aMethods := {}
   LOCAL i        := 1
   LOCAL lExact   := Set( _SET_EXACT, .t. )

   WHILE Substr( aInfo[ i ], 1, 1 ) != "_"

      IF Ascan( aInfo, "_" + Substr( aInfo[ i ], 1, 9 ), i + 1 ) != 0
         Aadd( aData, aInfo[ i ] )
      ELSE
         Aadd( aMethods, aInfo[ i ] )
      ENDIF

      i ++
   END

   Set( _SET_EXACT, lExact )

RETURN { aData, aMethods }

/****
*
*    __aoData()
*
*
*
*/

STATIC FUNCTION __aoDATA( oObject )

   LOCAL aInfo  := Asort( __ClassSel( oObject:ClassH() ) )
   LOCAL aData  := {}
   LOCAL i      := 1
   LOCAL lExact := Set( _SET_EXACT, .t. )

   WHILE Substr( aInfo[ i ], 1, 1 ) != "_"

      IF Ascan( aInfo, "_" + Substr( aInfo[ i ], 1, 9 ), i + 1 ) != 0
         Aadd( aData, aInfo[ i ] )
      ENDIF

      i ++
   END

   Set( _SET_EXACT, lExact )

RETURN aData

/****
*
*    getOData()
*
*
*
*/

STATIC FUNCTION GetOData( o )

   LOCAL i
   LOCAL aObjData := aoData( o )
   LOCAL aData    := aObjData[ 1 ]
   LOCAL aMeth    := aObjData[ 2 ]
   LOCAL aInline  := {}
   LOCAL aRet     := {}

   LOCAL slIsOObject := .F.

   IF Ascan( aData, "DICT" ) > 0        // Is oObject class !!!

      IF Valtype( o:Dict ) == "A"
         slIsOObject := .T.
      ENDIF

   ENDIF

   // oObject Classes
   IF slIsOObject == .T.

      FOR i := 1 TO Len( o:Dict[ _CLASS_DATA ] )

         IF Valtype( o:Dict[ _CLASS_DATA, i, 3 ] ) == "B"   // INLINE-BLOCK
            Aadd( aInline, Lower( o:Dict[ _CLASS_DATA, i, 1 ] ) )
         ELSE
            Aadd( aRet, { o:Dict[ _CLASS_DATA, i, 1 ], 1, 0 } )
         ENDIF

      NEXT

      Aadd( aRet, { "<font color='white'><b>" + "METHODS</font></b>", 4, 0 } )
      Aeval( aInline, { | e | Aadd( aRet, { Lower( e ), 2, 1 } ) } )
      Aeval( o:Dict[ _CLASS_METHODS ], { | e | Aadd( aRet, { Lower( e[ 1 ] ), 2, 0 } ) } )

      // Normal classes
   ELSE
      Aeval( aData, { | e | Aadd( aRet, { e, 1, 0 } ) } )
      Aeval( aMeth, { | e | Aadd( aRet, { Lower( e ), 2, 0 } ) } )
   ENDIF

RETURN aRet

/****
*
*      aoMethods()
*
*
*
*/

STATIC FUNCTION aoMETHODS( oObject )

   LOCAL aInfo  := Asort( __ClassSel( oObject:ClassH() ) )
   LOCAL aData  := {}
   LOCAL i      := 1
   LOCAL lExact := Set( _SET_EXACT, .t. )

   WHILE Substr( aInfo[ i ], 1, 1 ) != "_"

      IF Ascan( aInfo, "_" + Substr( aInfo[ i ], 1, 9 ), i + 1 ) == 0
         Aadd( aData, aInfo[ i ] )
      ENDIF

      i ++
   END

   Set( _SET_EXACT, lExact )

RETURN aData

/****
*
*     oGetData()
*
*
*
*/

STATIC FUNCTION oGETDATA( oObject, cIVar )

   LOCAL oErr := Errorblock( { | o | Break( o ) } )
   LOCAL xRet

   BEGIN SEQUENCE
      xRet := Eval( &( "{ | o | o:" + cIVar + "}" ), oObject )
   RECOVER USING oErr
      xRet := "<error>"
   END SEQUENCE

   Errorblock( oErr )
RETURN xRet

/****
*
*      oSetData()
*
*
*
*/

STATIC FUNCTION oSETDATA( oObject, cIVar, xValue )

   Eval( &( "{ | o, x | o:_" + cIVar + "( x ) }" ), oObject, xValue )

RETURN ( Nil )

/****
*
*     oTransform()
*
*
*
*/

STATIC FUNCTION oTRANSFORM( xVal )

   LOCAL cType := Valtype( xVal )
   SWITCH cType 
      CASE "C"
         RETURN '"' + xVal + '"'
         EXIT
      CASE  "N"
         RETURN Ltrim( Str( xVal ) )
         EXIT
      CASE "D"
         RETURN "CTOD('" + Dtoc( xVal ) + "')"
         EXIT
      CASE IIF( "A"
         RETURN "{ ... }"
         EXIT
      CASE IIF( "B"
         RETURN "{|| ... }"
         EXIT
      CASE IIF( "L"
         RETURN Iif( xVal, ".T.", ".F." )
         EXIT
      CASE IIF( "M"
         RETURN "<Memo>"
         EXIT
      CASE IIF( "O"
         RETURN "-Object-"
         EXIT
   END
   RETURN "-NIL-"

RETURN NIL

// ** EOF ***

