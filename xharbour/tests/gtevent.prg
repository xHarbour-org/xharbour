***********************************************************
* gtevent.prg
* $Id: gtclose.prg,v 1.2 2004/01/15 00:17:56 fsgiudice Exp $
* Test for gt events caused by asynchronous GT operations
*
* Giancarlo Niccolai
*

PROCEDURE Main()
   LOCAL nKey, bAction

   SET COLOR TO w+/b
   SET CONFIRM ON

   SetMode( 15,60 )
   SetCloseEvent( 2050 )
   SetGtResizeEvent( 2051 )

   // fake a first resize
   nKey := 2051

   DO WHILE .T.
      IF nKey == 2051
         DispBegin()
         CLEAR SCREEN
         @ 1, 1, MaxRow()-1, MaxCol()-1 BOX "******** " COLOR "W+/B"
         @2,2 SAY Padc("X H A R B O U R - GT Handlers test", Maxcol()-3 )
         @3,2 SAY Padc( "This is just a test to demonstrate GT event callbacks.", Maxcol()-3 )
         @4,3 SAY "Rows:" + LTrim(Str(MaxRow()+1)) + " Cols:"+ LTrim(Str(MaxCol()+1)) +  "  "
         DispEnd()
      ENDIF

      nKey := Inkey(0)

      IF nKey == 2050 .or. nKey == 27
         EXIT
      ENDIF
      @6,3 SAY "Pressed: "+ alltrim( str( nKey ) ) +  "    "
   ENDDO

   SET COLOR TO w/n
   CLEAR SCREEN
RETURN