/*
 * $Id$
 *
 */

#include "hbgtinfo.ch"
#include "hbgfx.ch"

#define WELCOME "Welcome to the World of xHarbour multiplatform Graphics!"

Function Main
Local nScreenWidth, nFontHeight, nFontWidth
Local nTop, nLeft, nHeight, nWidth, nColor, nSec := Seconds()

  If hb_gtInfo(GTI_ISGRAPHIC) == 0
     ?
     ? "You are using a non graphics capable gt:"
     ? hb_gtVersion()
     ?
     Quit
  End

  If hb_gtInfo(GTI_DESKTOPWIDTH) > 1000
    hb_gtInfo(GTI_FONTSIZE, 24)
  End

  nScreenWidth := hb_gtInfo(GTI_SCREENWIDTH)
  nFontHeight := hb_gtInfo(GTI_FONTSIZE)
  nFontWidth := hb_gtInfo(GTI_FONTWIDTH)

  SetColor("n/w")
  @ 0, 0 Say Space(MaxCol() + 1)
  @ 1, 0 Say PadC(WELCOME, MaxCol() + 1)
  @ 2, 0 Say Space(MaxCol() + 1)

  hb_gtInfo(GTI_WINTITLE, "Cross-GT, multiplatform graphics demo")

  PutFrame(nFontHeight / 2,;
           MaxCol() / 2 * nFontWidth - Len(WELCOME) / 2 * nFontWidth - nFontWidth,;
           nFontHeight * 2 + nFontHeight / 2,;
           nFontWidth + MaxCol() / 2 * nFontWidth + Len(WELCOME) / 2 * nFontWidth,;
           gfxMakeColor(0, 0, 0), gfxMakeColor(255, 255, 255))
  
  While Inkey() == 0
    nTop := Int(hb_Random(3.1 * nFontHeight, hb_gtInfo(GTI_SCREENHEIGHT)))
    nLeft := Int(hb_Random(hb_gtInfo(GTI_SCREENWIDTH)))
    nHeight := Int(hb_Random(251))
    nWidth := Int(hb_Random(251))
    nColor := gfxMakeColor(Int(hb_Random(32, 256)), Int(hb_Random(32, 256)), Int(hb_Random(32, 256)))

    Switch Int(hb_Random(1, 9))
      Case 1
        gfxLine(nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor)
        Exit
      Case 2
        gfxRect(nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor)
        Exit
      Case 3
        gfxFilledRect(nTop, nLeft, nTop + nHeight, nLeft + nWidth, nColor)
        Exit
      Case 4
        nTop += nHeight
        gfxCircle(nTop, nLeft, nHeight, nColor)
        Exit
      Case 5
        nTop += nHeight
        gfxFilledCircle(nTop, nLeft, nHeight, nColor)
        Exit
      Case 6
        nTop += nHeight
        gfxEllipse(nTop, nLeft, nHeight, nWidth, nColor)
        Exit
      Case 7
        nTop += nHeight
        gfxFilledEllipse(nTop, nLeft, nHeight, nWidth, nColor)
        Exit
      Case 8
        nHeight %= 64
        If nHeight % 2 == 1
          nHeight++
        End
        hb_gfxText(nTop, nLeft, "Hello", nColor, nHeight)
        Exit
    End
    If Seconds() - nSec > 3
      gfxFloodFill(0, 0, nColor)
      nSec := Seconds()
    End
  End
Return Nil

Function PutFrame(nTop, nLeft, nBottom, nRight, nColor1, nColor2)

  gfxRect(ntop, nLeft, nBottom, nRight, nColor1)
  gfxRect(ntop + 1, nLeft + 1, nBottom - 1, nRight - 1, nColor2)
/*  gfxLine(nTop + 1, nLeft + 1, nTop + 1, nRight - 1, nColor2)
  gfxLine(nTop + 2, nLeft + 1, nBottom - 1, nLeft + 1, nColor2) */
Return Nil
