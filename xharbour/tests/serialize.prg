****************************************************
* Serialize.prg
* $Id: serialize.prg,v 1.2 2003/04/13 20:40:45 jonnymind Exp $
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

Class AClass
   DATA aData PERSISTENT

   METHOD aMethod()
   METHOD New( cDt ) CONSTRUCTOR
   HIDDEN:
   Data aHIdden
ENDCLASS

METHOD aMethod() CLASS aClass
   ? "Method from class ", ::aData
RETURN .T.

METHOD New( cData ) CLASS aClass
   ::aData := cData
RETURN .T.


Procedure MAIN()
   LOCAL cTmp, oData
   LOCAL cSerial

   CLEAR SCREEN
   @1,10 SAY "X H A R B O U R - Serialization and deserialization tests"

   cTmp := HB_Serialize( "A string" )
   cTmp += HB_Serialize( 12.4 )
   cTmp += HB_Serialize( CtoD( "2/2/2001" ) )
   cTmp += HB_Serialize( { 1, 2, { "a", "b" }, 3 } )
   cTmp += HB_Serialize( 20 )
   cTmp += HB_Serialize( AClass():New("A parameter") )
   cTmp += HB_Serialize( "Last String, closing test" )

   /* now we deserialize */
   cSerial := HB_DeserialBegin( cTmp )

   cTmp := HB_DeserialNext( cSerial )
   DO WHILE cTmp != NIL

      DO CASE
         CASE ValType( cTmp ) == "A"
            ArrayDump( cTmp, 0 )
         CASE ValType( cTmp ) == "O"
            cTmp:aMethod()
         OTHERWISE
            ? "*", cTmp
      ENDCASE

      cTmp := HB_DeserialNext( cSerial )
   ENDDO

   ?
   ?" DONE - press a key to terminate"
   Inkey(0)
RETURN


PROCEDURE ArrayDump( aData, nLevel )
   LOCAL cElem

   ? "ARRAY:" + rTrim( Str( nLevel ) )
   FOR EACH cElem IN aData
      IF ValType( cElem ) == "A"
         ArrayDump( cElem, nLevel + 1 )
      ELSE
         ? cElem
      ENDIF
   NEXT
   ? "END ARRAY"
RETURN
