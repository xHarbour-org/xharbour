// CHAR.PRG - return a char string given any value,;
//            or a value given a type & a string

FUNCTION WG_Char( xValue )
   LOCAL cType := ValType( xValue )
   LOCAL cValue := ""
   DO CASE
      CASE cType $  "CM";  cValue := xValue
      CASE cType == "N" ;  cValue := LTrim( Str( xValue, 20 ) )
      CASE cType == "D" ;  cValue := DToS( xValue )
      CASE cType == "L" ;  cValue := IIf( xValue, "T", "F" )
      CASE cType == "A" ;  cValue := WG_AToC( xValue )
      CASE cType $  "UE";  cValue := "NIL"
      CASE cType == "B" ;  cValue := "{|| ... }"
      CASE cType == "O";   cValue := "{" + xValue:className + "}"
   ENDCASE
RETURN cValue

FUNCTION WG_Value( cValue, cType )
   LOCAL xValue
   DO CASE
      CASE cType $  "CM";  xValue := cValue
      CASE cType == "D" ;  xValue := SToD( cValue )
      CASE cType == "N" ;  xValue := Val( cValue )
      CASE cType == "L" ;  xValue := ( cValue == 'T' )
      CASE cType == "A" ;  xValue := WG_CToA( cValue )
      OTHERWISE;           xValue := NIL                     // nil, block, object
   ENDCASE
RETURN xValue
                                     
FUNCTION SToD( sDate )
RETURN CToD( SubStr( sDate, 7, 2 ) + "/" +;
             Substr( sDate, 5, 2 ) + "/" +;
             SubStr( sDate, 1, 4 ) )

                                     