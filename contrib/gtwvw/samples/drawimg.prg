/* $Id$
   image.prg
   compile and link this file together with wutil.prg
   (use drawimg.bat to build)

   This is a sample of how to display an image file (.bmp,.gif,.jpg)
   in four ways:
   1. Image stretched to fit a region
      (all four coordinates are defined)
   2. Height is defined, width is proportional to height
      (right coordinate is passed as NIL)
   3. Width is defined, height is proportional to width
      (bottom coordinate is passed as NIL)
   4. Actual image size
      (bottom and right coordinate are NIL)

   Also shown in this sample:
   1. Image Caching
      Image is read from file only once, and then cached.
      Test: After image is displayed, delete the image file.
            Image should be still displayed with no problem,
            since gtwvw already store it in memory.
   2. Transparency
      If this option is used, topleft pixel is used as the transparent mask
      of the image.

   Remarks:
   Image drawing and wvw_paint management are performed by WUTIL.PRG.
   WUTIL.PRG is a simple "middle module" interfacing an application with
   gtwvw.
 */
#include "common.ch"
#include "wutil.ch"

proc main
local ntop := 7,;
      nleft:= 3,;
      nbot := maxrow()-2,;
      nrig := maxcol()-2,;
      nmidver:=INT((ntop+nbot)/2),;
      nmidhor:=INT((nleft+nrig)/2)
local cpict := "vouch1.gif",;
      ltransp := .f.,;
      nMaxCache := wvw_SetMaxBMcache()
local i,j,oWPaint
local getlist := {}
   setcolor("N/W,N/GR*,,,N/W*")
   wvw_setcodepage(,255)
   wg_ResetWPaintObj( 0 )
   do while .t.
      CLS
      setcursor(1)
      cpict := padr(cpict,40)
      @ 0,0 say "FileName  :" get cpict pict "@K" valid file(alltrim(cpict))
      @ 1,0 say "Transpar? :" get ltransp pict "Y"
      @ 2,0 say "Max Cache :" get nMaxCache pict "999"
      @ 3,0 say "NumOfCache=" + trans(wvw_numBMcache(),"999") +;
                ", Max NumOfCache=" + trans(wvw_SetMaxBMcache(),"999")
      read
      if lastkey()==27
         exit
      endif
      wvw_SetMaxBMcache(nMaxCache)
      @ 3,0 say "NumOfCache=" + trans(wvw_numBMcache(),"999") +;
                ", Max NumOfCache=" + trans(wvw_SetMaxBMcache(),"999")

      @ 5,0 say "TOPLEFT: stretched image                 TOPRIGHT: fit vertically (proportional)"
      @ 6,0 say "BOTLEFT: fit horizontally (proportional) BOTRIGHT: actual image size"

      cpict := alltrim(cpict)

      //wvw_loadpicture( 1, cpict ) //20060707

      setcursor(0)
      DISPBEGIN()
      for i := ntop to nbot
         for j := nleft to nrig
            @ i,j say "X"
         next
      next
      @ ntop,nmidhor to nbot,nmidhor
      @ nmidver,nleft to nmidver,nrig
      @ ntop,nleft to nbot,nrig
      DISPEND()

      * topleft panel, stretch/fit to panel
      oWPaint := wPaintObj():New(0, WPAINTOBJ_IMAGE, "TOPLEFT", ntop+1, nleft+1, nmidver-1, nmidhor-1, NIL, ltransp)
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      * topright panel, fit vertically
      oWPaint := wPaintObj():New(0, WPAINTOBJ_IMAGE, "TOPRIGHT", ntop+1, nmidhor+1, nmidver-1, NIL, NIL, ltransp)
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      * botleft panel, fit horizontally
      oWPaint := wPaintObj():New(0, WPAINTOBJ_IMAGE, "BOTLEFT", nmidver+1, nleft+1, NIL, nmidhor-1, NIL, ltransp)
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      * botright panel, actual image size
      oWPaint := wPaintObj():New(0, WPAINTOBJ_IMAGE, "BOTRIGHT", nmidver+1, nmidhor+1, NIL, NIL, NIL, ltransp)
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      inkey(0)

      * delete all image objects
      wg_DelWPaintObj(0, WPAINTOBJ_IMAGE, NIL)
   enddo //while .t.
   setcursor(1)
return


*************** simple wpaint organizer *******
