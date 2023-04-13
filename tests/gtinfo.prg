/********************************************************
 * Test to query/set properties for graphics gts
 * Under textual gts it should only properly quit
 *
 * $Id$
 */

#include "hbgtinfo.ch"

Function Main
   Local nOp

   // try to set anyhow the name of the console
   hb_gtInfo( GTI_WINTITLE, "GT - Info functionality test" )

   If hb_gtInfo(GTI_ISGRAPHIC) == 0
     ?
     ? "You are using a non graphics capable gt:"
     ? hb_gtVersion()
     ? "Press a key to end."
     ?
     Inkey(0)
     Quit
   End

   SetColor("w+/b,b/w")
   hb_idleAdd({|| updKeys()})

   While .T.
      CLEAR SCREEN
//      hb_gtInfo(GTI_WINTITLE, "hb_gtInfo() api demonstration program")
      updKeys(.T.)
      @ 3, 5 Say "Desktop Resolution: " + ;
               AllTrim(Str(hb_gtInfo(GTI_DESKTOPWIDTH))) + "x" + ;
            AllTrim(Str(hb_gtInfo(GTI_DESKTOPHEIGHT))) + "x" + ;
            AllTrim(Str(hb_gtInfo(GTI_DESKTOPDEPTH))) + "bpp   "
      @ 2, 5 Say "Current GT Resolution: " + ;
                 AllTrim(Str(hb_gtInfo(GTI_SCREENWIDTH))) + "x" + ;
		 AllTrim(Str(hb_gtInfo(GTI_SCREENHEIGHT))) + "x" + ;
		 AllTrim(Str(hb_gtInfo(GTI_SCREENDEPTH))) + "bpp   "
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
	          hb_gtInfo(GTI_SCREENWIDTH, 320)
		       hb_gtInfo(GTI_SCREENHEIGHT, 200)
	       Case nOp == 2
	          hb_gtInfo(GTI_SCREENWIDTH, 640)
		       hb_gtInfo(GTI_SCREENHEIGHT, 400)
	       Case nOp == 3
	          hb_gtInfo(GTI_SCREENWIDTH, 640)
		       hb_gtInfo(GTI_SCREENHEIGHT, 480)
	       Case nOp == 4
	          hb_gtInfo(GTI_SCREENWIDTH, 800)
		       hb_gtInfo(GTI_SCREENHEIGHT, 600)
	       Case nOp == 5
	          hb_gtInfo(GTI_SCREENWIDTH, 1024)
		       hb_gtInfo(GTI_SCREENHEIGHT, 768)
	    End
         Case nOp == 2
	    nOp := Alert("Select desired depth", {"  8 ", " 15 ", " 16 ", " 24 ", " 32 "})
	    Do Case
	       Case nOp == 1
	          hb_gtInfo(GTI_SCREENDEPTH, 8)
	       Case nOp == 2
	          hb_gtInfo(GTI_SCREENDEPTH, 15)
	       Case nOp == 3
	          hb_gtInfo(GTI_SCREENDEPTH, 16)
	       Case nOp == 4
	          hb_gtInfo(GTI_SCREENDEPTH, 24)
	       Case nOp == 5
	          hb_gtInfo(GTI_SCREENDEPTH, 32)
	    End
         Case nOp == 3
	    nOp := Alert("Select desired font size", {"  8 ", " 16 ", " 24 "})
	    Do Case
	       Case nOp == 1
	          hb_gtInfo(GTI_FONTSIZE, 8)
	       Case nOp == 2
	          hb_gtInfo(GTI_FONTSIZE, 16)
	       Case nOp == 3
	          hb_gtInfo(GTI_FONTSIZE, 24)
	    End
         Case nOp == 4
	    nOp := Alert("Select desired font width", {"  4 ", " 8 ", " 16 "})
	    Do Case
	       Case nOp == 1
	          hb_gtInfo(GTI_FONTWIDTH, 4)
	       Case nOp == 2
	          hb_gtInfo(GTI_FONTWIDTH, 8)
	       Case nOp == 3
	          hb_gtInfo(GTI_FONTWIDTH, 16)
	    End
      End
   End
Return Nil

#define WINTIT "hb_gtInfo() API demonstration program"

Function updKeys(lPar)
Static aKeys := Nil, lInit := .F.
Static cStr := "", lDir := .T., nSec := Nil
Local i, aNewKeys, nY := Row(), nX := Col()

  If !lInit .Or. !Empty(lPar)
    @ MaxRow(), MaxCol() - 36 Say "SHF   CTR   ALT   NUM   CAP   SCR   " Color "w+/b"
    aKeys := {0, 0, 0, 0, 0, 0}
    lInit := .T.
    cStr := ""
    lDir := .T.
    nSec := Seconds()
  End

  i := hb_gtInfo(GTI_KBDSHIFTS)
  aNewKeys := {i & GTI_KBD_SHIFT,;
               i & GTI_KBD_CTRL,;
	       i & GTI_KBD_ALT,;
	       i & GTI_KBD_NUMLOCK,;
	       i & GTI_KBD_CAPSLOCK,;
	       i & GTI_KBD_SCROLOCK}

  For i := 1 To Len(aNewKeys)
    If aNewKeys[i] != aKeys[i]
      @ MaxRow(), MaxCol() - 2 - ((6 - i) * 6) Say IIf(aNewKeys[i] == 0, ' ', '*') Color "g+/b"
      aKeys[i] := aNewKeys[i]
    End
  Next

  SetPos(nY, nX)

  If Seconds() - nSec > .1
    nSec := Seconds()
    If lDir
      cStr += SubStr(WINTIT, Len(cStr) + 1, 1)
      If Len(cStr) == Len(WINTIT)
        lDir := .F.
        nSec += 2
      End
    Else
      cStr := Left(cStr, Len(cStr) - 1)
      If Empty(cStr)
        lDir := .T.
        nSec += 2
      End
    End

    hb_gtInfo(GTI_WINTITLE, cStr)
  End
Return Nil
