#include "hbclass.ch"

PROCEDURE Main()

   LOCAL cVar := "Funny", nVar := 1

   ASSOCIATE CLASS MyStringClass  WITH TYPE CHARACTER
   ASSOCIATE CLASS MyNumericClass WITH TYPE NUMERIC

   ? cVar:ClassName
   ? nVar:ClassName

   ? cVar:AsString
   ? cVar:Super:AsString
   ? cVar:Character:AsString
   ? cVar:Sub( 1, 3 )
   ? cVar:Version

   cVar := "23"
   ? nVar + cVar

RETURN

CLASS MyStringClass FROM CHARACTER

   // CLASSDATA are allowed, but DATAs are not, as we don't have a real instance for storing such properties.
   CLASSDATA Version INIT "1.0"

   METHOD AsString INLINE "MyString: >" + Self + "<"

   METHOD Sub( nFrom, nLen ) INLINE SubStr( Self, nFrom, nLen )

ENDCLASS

CLASS MyNumericClass FROM NUMERIC

   OPERATOR "+" ARG xArg INLINE Alert( "Adding type: " + ValType( xArg ) ) , Self := Self + Val( xArg )

ENDCLASS
