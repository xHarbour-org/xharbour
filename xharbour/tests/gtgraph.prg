**************************************************
* gtgraph.prg
* $Id: gtgraph.prg,v 1.3 2004/01/25 19:15:53 jonnymind Exp $
* Test for GT based graphical functions.
*
* Giancarlo Niccolai
*


PROCEDURE Main()

   //SET GTMODE TO INLINE

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
   GtText( 30, 50, "Hello World", GtRgb( 0, 0.6, 0.1 ) )
   GtPoint( 50,20, GtRgb( 0.302, 0.301, 0.305 ) )
   GtPoint( 52,20, "g+" )
   GtPoint( 54,20, "r+" )
   GtLine( 0,0,100,100, GtRgb( 0.302, 0.601, 0.405 ) )
   GtLine( 100,0,0,100, GtRgb( 1.0, 1.0, 0.8 ) )
   GtCircle( 400, 0, 100,100,  "rb+" )
   GtDisk( 300,0, 100,100, "bg+" )
   GtRectangle( 100,0, 100,100, "r+" )
   GtSquare( 200,0, 100,100, "G" )

   @10,10 SAY "Press any key to continue"
   Inkey(0)

   CLEAR SCREEN
   TestSquares()
   @10,10 SAY "Press any key to continue"
   Inkey(0)
   TestScroll()

   @9,0 clear to 11,70
   @10,2 SAY "Press any key to terminate"
   Inkey(0)
RETURN

PROCEDURE TestSquares()
   LOCAL nCount

   * This demonstrates that a rectangle drawn AFTER is displayed ON TOP
   * of the previously drawn rectangles
   FOR nCount := 255 TO 1 STEP -1
      GtRectangle( 300 + (255-nCount),50 + (255-nCount), nCount, nCount,;
         GtRGB( (255 - nCount) *256, ( 255 - nCount) * 128, ( 255 - nCount) * 64 ) )
   NEXT
RETURN

PROCEDURE TestScroll()
   LOCAL nCount
   
   FOR nCount := 1 TO 100
      ? Replicate("-Pad-",5),"Scrolling test", nCount
   NEXT

RETURN
