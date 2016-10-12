#include "apollo.ch"
function main()
LOCAL f

request SIX
rddRegister( "SIX", 1 )
rddsetdefault( "SIX" )
SET FILETYPE TO CDX

CLEAR SCREEN

? "Start"
? sx_Version(4)+", "+sx_Version() ; ?
?

? "Creating sixCDX..."
dbCreate( "SixCDX", { { "first","N",2,0 }, { "info","M",10,0 } } )
use SixCDX
?? " done"

? "Appending 10 records to the database..."
for f=1 to 10
   APPEND BLANK
   field->first:=f
   field->info:="This is record nr. "+LTrim(Str(f))
next
?? " done"

? "Indexing on field 'first'..."
index on first tag t1
?? " done"

? "Show all records..."
GO TOP
WHILE !Eof()
   ? first, info
   SKIP
ENDDO
? " done"

? "Closing database"

USE

?
? "Done! ;-)"
?

wait

return nil