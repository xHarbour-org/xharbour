****************************************************
* Serialize.prg
* $Id: serialize.prg,v 1.7 2003/07/24 01:55:59 jonnymind Exp $
* Test for the hb_serial family function
*
* This serialization functions allow to store consistently any
* basic datatype (including nested array) into a linear string,
* a file or any kind of stream. The functions are designed to be
* pretty fast and efficent, so that passing back and forth
* serialized data is not too burdensome.
*
* This example shows how it is simple to store a whole set of
* variables into a unique string (cTmp), and then how the
* iterative deserialization process can be done using the
* hb_DeserialBegin() and HB_DeserialNext().
*
* This two functions are just shortcuts (handled more efficenty)
* to call iteratively
* 1) HB_Deserialize( cSerial ): return deserialized code
* 2) HB_SerialNext( cSerial ): return the whole length of the
*      serialized data in the beginning of cSerial
* 3) Substr( cSerial, HB_SerialNext( cSerial ) + 1) remove first
*      serialized data into cSerial
*
* Giancarlo Niccolai
*

#include "hbclass.ch"

PROCEDURE MAIN()
   LOCAL cTmp, xVal
   LOCAL cSerial

   CLEAR SCREEN
   @ 1, 10 SAY "X H A R B O U R - Serialization and deserialization tests"

   cTmp := HB_Serialize( "A string" )
   cTmp += HB_Serialize( 12.4 )
   cTmp += HB_Serialize( Directory( "serialize.prg") ) 
   cTmp += HB_Serialize( CtoD( "2/2/2001" ) )
   cTmp += HB_Serialize( { 1, 2, { "a", "b" }, 3 } )
   cTmp += HB_Serialize( 2100010101 )
   cTmp += HB_Serialize( {'a':>'1', 'b':> 2, 10 :>{1,2},  'z' :>{1:>2, 0:>3}  } )
   cTmp += HB_Serialize( SomeClass():New("A parameter") )
   cTmp += HB_Serialize( "Last String, closing test" )

   /* now we deserialize */
   cSerial := HB_DeserialBegin( cTmp )

   xVal := HB_DeserialNext( cSerial )

   ?
   DO WHILE xVal != NIL
      SWITCH ValType( xVal )
         CASE 'H'
            ? ValToPrg( xVal )
            EXIT

         CASE 'A'
            ArrayDump( xVal )
            EXIT

         CASE 'O'
            xVal:SomeMethod()
            EXIT

         DEFAULT
            ? "*("+ValType(xVal)+"):", xVal
      END

      xVal := HB_DeserialNext( cSerial )
   ENDDO

   ? "Done. Press any key to terminate."
   Inkey( 0 )
   ?
   
RETURN

PROCEDURE ArrayDump( aData, nLevel )

   LOCAL xElement

   IF nLevel == NIL
      nLevel := 0
   ENDIF

   ? Space( 3 * nLevel - 1 ) + ':' + "ARRAY #" + LTrim( Str( nLevel ) )

   FOR EACH xElement IN aData
      IF ValType( xElement ) == "A"
         ArrayDump( xElement, nLevel + 1 )
      ELSE
         ? Space( 3 * nLevel - 1 ) + ':', xElement
      ENDIF
   NEXT

   ? Space( 3 * nLevel - 1 ) + ':' + "END ARRAY #" + LTrim( Str( nLevel ) )

RETURN

CLASS SomeClass

   DATA cData PERSISTENT

   METHOD New( cDt ) CONSTRUCTOR

   METHOD SomeMethod()

ENDCLASS

METHOD SomeMethod() CLASS SomeClass

   ? ProcName() + '[' + LTrim( Str( ProcLine() ) ) + ']', ::cData

RETURN .T.

METHOD New( cData ) CLASS SomeClass

   ::cData := cData

RETURN .T.
