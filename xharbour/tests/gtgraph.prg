**************************************************
* gtgraph.prg
* $Id: regex.prg,v 1.4 2003/05/27 20:05:54 jonnymind Exp $
* Test for GT based graphical functions.
*
* Giancarlo Niccolai
*


PROCEDURE Main()
   SET COLOR TO w+/b

   CLEAR SCREEN
   GtPoint( 50,20, "r+" )
   GtPoint( 52,20, "w+" )
   GtPoint( 54,20, "n" )
   GtLine( 0,0,100,100, "r+" )
   GtLine( 100,0,0,100, "g+" )
   GtRectangle( 100,0, 100,100, "r" )
   GtSquare( 200,0, 100,100, "G+" )
   GtDisk( 300,0, 100,100, "R+" )
   GtCircle( 400, 0, 100,100,  "g+" )
   GtText( 30, 50, "Hello World", "b+" )

   @10,10 SAY "Press any key to continue"
   Inkey(0)

   CLEAR SCREEN
   GtText( 30, 50, "Hello World", "gr+" )
   GtPoint( 50,20, "n+" )
   GtPoint( 52,20, "g+" )
   GtPoint( 54,20, "r+" )
   GtLine( 0,0,100,100, "b+" )
   GtLine( 100,0,0,100, "w" )
   GtCircle( 400, 0, 100,100,  "rb+" )
   GtDisk( 300,0, 100,100, "bg+" )
   GtRectangle( 100,0, 100,100, "r+" )
   GtSquare( 200,0, 100,100, "G" )

   @10,10 SAY "Press any key to terminate"
   Inkey(0)
RETURN
