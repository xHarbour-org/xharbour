//
// $Id: menutest.prg,v 1.2 2002/11/20 23:45:34 lculik Exp $
//

#include "inkey.ch"

PROCEDURE main()
   memvar ptestvar
   local testvar

   SET EVENTMASK TO INKEY_ALL
   SetKey(K_F8, {|| RECURSE( 30 ) } )

   clear screen

   @  1, 10 prompt 'Menu Item 1' message 'Menu Message 1' Color "w/b,w+/b,w/r,n/w"
   @  2, 10 prompt 'Menu Item 2' message 'Menu Message 2' Color "bg/r,bg+/b,w/r,n/w"
   @  3, 10 prompt 'Menu Item 3' message 'Menu Message 3' Color "r/g,r+/b,w/r,n/w"
   @  4, 10 prompt 'Menu Item 4' message 'Menu Message 4' Color "g/bg,g+/b,w/r,n/w"

   @  6, 10 say 'Testing with LOCAL parameter'
   @  7, 10 say 'Press F8 to recurse into MENU TO'

   menu to testvar

   @  9, 10 say 'Your Choice = ' + str( testvar, 1 )

   Inkey(0)

   clear screen

   @  1, 10 prompt 'Menu Item 1' message 'Menu Message 1' Color "w/b,w+/b,w/r,n/w"
   @  2, 10 prompt 'Menu Item 2' message 'Menu Message 2' Color "bg/r,bg+/b,w/r,n/w"
   @  3, 10 prompt 'Menu Item 3' message 'Menu Message 3' Color "r/g,r+/b,w/r,n/w"
   @  4, 10 prompt 'Menu Item 4' message 'Menu Message 4' Color "g/bg,g+/b,w/r,n/w"

   @  6, 10 say 'Testing with MEMVAR parameter'
   @  7, 10 say 'Press F8 to recurse into MENU TO'

   menu to ptestvar

   @  9, 10 say 'Your Choice = ' + str( ptestvar, 1 )

   return

procedure RECURSE( nLevel )
   local testvar

   if ( nLevel > MaxCol() - 11 )
      RETURN
   ENDIF

   set key K_F8 to

   @  6, 10 say '                                '

   @  1, nLevel prompt 'Menu Item 1' message 'Menu Message 1' Color "w/b,w+/b,w/r,n/w"
   @  2, nLevel prompt 'Menu Item 2' message 'Menu Message 2' Color "bg/r,bg+/b,w/r,n/w"
   @  3, nLevel prompt 'Menu Item 3' message 'Menu Message 3' Color "r/g,r+/b,w/r,n/w"
   @  4, nLevel prompt 'Menu Item 4' message 'Menu Message 4' Color "g/bg,g+/b,w/r,n/w"

   menu to testvar

   @1,nLevel clear to 4,nlevel+11
   @  7, 10 say 'Press F8 to recurse into MENU TO'
   @  9, 50 say 'Your Choice = ' + str( testvar, 1 )

   SetKey(K_F8, {|| RECURSE(30) } )

   return
