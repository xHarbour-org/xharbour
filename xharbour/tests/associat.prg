#include "hbclass.ch"

PROCEDURE Main()

   LOCAL cVar := "Funny"

   ASSOCIATE CLASS MyStringClass WITH TYPE CHARACTER

   ? cVar:AsString
   ? cVar:Super:AsString
   ? cVar:Character:AsString
   ? cVar:Sub( 1, 3 )

RETURN

CLASS MyStringClass FROM CHARACTER

   METHOD AsString INLINE "MyString: >" + Self + "<"

   METHOD Sub( nFrom, nLen ) INLINE SubStr( Self, nFrom, nLen )

ENDCLASS
