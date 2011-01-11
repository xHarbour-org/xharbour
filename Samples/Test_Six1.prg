#include "apollo.ch"

PROCEDURE Main()
LOCAL f,s

 request SIX
 rddRegister( "SIX", 1 )
 rddsetdefault( "SIX" )
 SET FILETYPE TO NSX

 ? "ApolloRDD: "+sx_Version(4)+", "+sx_Version()
 ?

 ? "Start"
 ?

 fErase("Six1.dbf")
 fErase("Six1.smt")
 fErase("Six1.nsx")
 fErase("Temp1.nsx")
 fErase("Temp2.nsx")

 s:=Seconds()

 dbCreate( "Six1", { { "Date","D",8,0 }, { "numberA","N",8,0 }, { "numberB","N",8,0 }, { "info","M",10,0 } } )
 use Six1

 ? Seconds()-s

 for f=1 to 1000
    append blank
    ? date()
    field->date:=Date()-f
    field->numberA:=f
    field->numberB:=15-f
    field->info:=str(field->numberA)+Str(field->numberB)+DtoC(field->date)
 next

 ? Seconds()-s

 index on field->date    tag t1

 ? Seconds()-s

 index on field->numberA tag t2

 ? Seconds()-s

 ?
 ? "indexed on numberA, all for field->numberA>=5"
 index on field->numberA to "temp1" for field->numberA>=5
 go top

 while !Eof()
    ? field->numberA, field->numberB
    skip
 enddo

 ? Seconds()-s

 ?
 ? "OrdDescend()"
 sx_Descend()
 go top

 while !Eof()
    ? field->numberA, field->numberB
    skip
 enddo

 OrdDescend()

 ?
 ? "index on numberB, all for field->numberA<5"
 index on field->numberB to "temp2" for field->numberA<5
 go top

 while !Eof()
    ? field->numberA, field->numberB
    skip
 enddo

 ? Seconds()-s

 ?
 ? "all for field->numberA>=5, index on numberA"
 set index to "temp1"
 go top

 while !Eof()
    ? field->numberA, field->numberB
    skip
 enddo

 ? Seconds()-s
 ?

 ? "End"

 wait

return nil