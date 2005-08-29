#include "hbclass.ch"

PROCEDURE Main()

   LOCAL cVar := "Funny", nVar := 1

   ASSOCIATE CLASS MyStringClass  WITH TYPE CHARACTER
   ASSOCIATE CLASS MyNumericClass WITH TYPE NUMERIC

   ? cVar:AsString
   ? cVar:Super:AsString
   ? cVar:Character:AsString
   ? cVar:Sub( 1, 3 )

   ? nVar + cVar

RETURN

CLASS MyStringClass FROM CHARACTER

   METHOD AsString INLINE "MyString: >" + Self + "<"

   METHOD Sub( nFrom, nLen ) INLINE SubStr( Self, nFrom, nLen )

ENDCLASS

CLASS MyNumericClass FROM NUMERIC

   OPERATOR "+" ARG xArg INLINE Alert( "Adding type: " + ValType( xArg ) ) , Self := Self + Val( xArg )

ENDCLASS
