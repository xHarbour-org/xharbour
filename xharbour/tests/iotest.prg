//
// $Id: iotest.prg,v 1.1 1999/10/04 18:46:29 vszel Exp $
//

// Testing Harbour file io features

function Main()

   local h    := 0
   local cstr := " "
   local ntmp := 1

   h := FCreate( "test.txt")
   qout('create handle',h)

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   h := FOpen("test.txt")
   qout('open handle',h)
   qout()
   /* try to read what is there */
   do while ntmp # 0
      ntmp := FRead( h, @cstr, 1)
      if ntmp > 0
         qqout(cstr)
      endif
   enddo
   qout()

   FClose( h )

return nil
