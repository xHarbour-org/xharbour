/********************************************************
 * Test to query/set properties for graphics gts
 * Under textual gts it should only properly quit
 *
 * $Id$
 */
#include "gtinfo.ch"

Function Main
Local nOp

   If GTInfo(GTI_ISGRAPHIC) == 0
     ?
     ? "This test is intended to run under graphics capable gts only"
     ?
     Quit
   End

   SetColor("w+/b,b/w")

   While .T.
      CLEAR SCREEN
      @ 3, 5 Say "Desktop Resolution: " + ;
               AllTrim(Str(GTInfo(GTI_DESKTOPWIDTH))) + "x" + ;
            AllTrim(Str(GTInfo(GTI_DESKTOPHEIGHT))) + "x" + ;
            AllTrim(Str(GTInfo(GTI_DESKTOPDEPTH))) + "bpp   "
      @ 2, 5 Say "Current GT Resolution: " + ;
                 AllTrim(Str(GTInfo(GTI_SCREENWIDTH))) + "x" + ;
		 AllTrim(Str(GTInfo(GTI_SCREENHEIGHT))) + "x" + ;
		 AllTrim(Str(GTInfo(GTI_SCREENDEPTH))) + "bpp   "
      @ 5, ( MaxCol() / 2 ) - 9 Prompt " Change Resolution "
      @ 6, ( MaxCol() / 2 ) - 9 Prompt " Change Depth      "
      @ 7, ( MaxCol() / 2 ) - 9 Prompt " Change Font Size  "
      @ 8, ( MaxCol() / 2 ) - 9 Prompt " Change Font Width "
      @ 9, ( MaxCol() / 2 ) - 3 Prompt " Quit "
      Menu To nOp
      Do Case
         Case nOp == 0 .Or. nOp == 5
	    Exit
	 Case nOp == 1
	    nOp := Alert("Select desired resolution;NOTE: not all resolution will work on all environments",;
	                {"320x200", "640x400", "640x480", "800x600", "1024x768"})
	    Do Case
	       Case nOp == 1
	          GTInfo(GTI_SCREENWIDTH, 320)
		       GTInfo(GTI_SCREENHEIGHT, 200)
	       Case nOp == 2
	          GTInfo(GTI_SCREENWIDTH, 640)
		       GTInfo(GTI_SCREENHEIGHT, 400)
	       Case nOp == 3
	          GTInfo(GTI_SCREENWIDTH, 640)
		       GTInfo(GTI_SCREENHEIGHT, 480)
	       Case nOp == 4
	          GTInfo(GTI_SCREENWIDTH, 800)
		       GTInfo(GTI_SCREENHEIGHT, 600)
	       Case nOp == 5
	          GTInfo(GTI_SCREENWIDTH, 1024)
		       GTInfo(GTI_SCREENHEIGHT, 768)
	    End
         Case nOp == 2
	    nOp := Alert("Select desired depth", {"  8 ", " 15 ", " 16 ", " 24 ", " 32 "})
	    Do Case
	       Case nOp == 1
	          GTInfo(GTI_SCREENDEPTH, 8)
	       Case nOp == 2
	          GTInfo(GTI_SCREENDEPTH, 15)
	       Case nOp == 3
	          GTInfo(GTI_SCREENDEPTH, 16)
	       Case nOp == 4
	          GTInfo(GTI_SCREENDEPTH, 24)
	       Case nOp == 5
	          GTInfo(GTI_SCREENDEPTH, 32)
	    End
         Case nOp == 3
	    nOp := Alert("Select desired font size", {"  8 ", " 16 ", " 24 "})
	    Do Case
	       Case nOp == 1
	          GTInfo(GTI_FONTSIZE, 8)
	       Case nOp == 2
	          GTInfo(GTI_FONTSIZE, 16)
	       Case nOp == 3
	          GTInfo(GTI_FONTSIZE, 24)
	    End
         Case nOp == 4
	    nOp := Alert("Select desired font width", {"  4 ", " 8 ", " 16 "})
	    Do Case
	       Case nOp == 1
	          GTInfo(GTI_FONTWIDTH, 4)
	       Case nOp == 2
	          GTInfo(GTI_FONTWIDTH, 8)
	       Case nOp == 3
	          GTInfo(GTI_FONTWIDTH, 16)
	    End
      End
   End
Return Nil
