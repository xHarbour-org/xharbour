//
// $Id: iotest2.prg,v 1.1 1999/10/04 18:46:29 vszel Exp $
//

// Testing Harbour file io features
// using freadstr instead of fread

function Main()

   local h    := 0
   local cstr := " "
   local ntmp := 0

   h := FCreate( "test.txt")
   qout('create handle',h)

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   h := FOpen("test.txt")
   qout('open handle',h)
   qout()
   /* try to read what is there */
   do while asc(cstr) # 0
      cstr := FReadstr( h, 1)
      qqout(cstr)
   enddo

   FClose( h )

return nil
