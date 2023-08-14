/*
 * $Id$
 */

/*
 * Copyright 2004-2006 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * GD test file
 */


#include "gd.ch"
#include "common.ch"

#define IMAGES_IN  "images_in/"
#define IMAGES_OUT "images_out/"

#define FILLED     TRUE
#define NOT_FILLED FALSE
#define TC_WIDTH   800
#define TC_HEIGHT  600

PROCEDURE Main()

   LOCAL im, im2
   // Colors variables
   LOCAL black, white, blue, red, green, cyan, gray, yellow

   LOCAL oTrueColor, oX

   LOCAL aClip, color, font, aRect
   LOCAL oI, oI2, oI3, oI4, nThick, n, nSecs
   LOCAL oI5
   LOCAL oB
   LOCAL x, y, center_x, center_y

   // Check output directory
   IF !ISDirectory( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF

   ? "GD Power - Demo of GD Library under xHarbour"
   ?

   ? "Create a true color image in memory"
   oX := GDImage():CreateTrueColor( 300, 300 )

   // Now work on image
   WITH OBJECT oX

        ? "Background color (true color comes with black background, we have to fill it)"
        // Allocate white truecolor
        white := :GetTrueColor( 255, 255, 255 )
        // Fill entire area
        :Rectangle( 0, 0, :Width(), :Height(), FILLED, white ) // FILLED = TRUE

        ? "Create xHarbour Logo"

        ? "Begin allocating colors"
        yellow := :GetTrueColor( 255, 255, 74 )
        black  := :GetTrueColor( 0, 0, 0 )
        gray   := :GetTrueColor( 204, 204, 204 )

        x := :CenterWidth()
        y := :CenterHeight()
        //? x, y, :Width(), :Height()

        ? "Create shadow"
        :SetAntiAliased( gray )
        :Circle( x + 10, y + 10, 200, FILLED, gdAntiAliased )

        ? "Draw 2 circles"
        :SetAntiAliased( black )
        :Circle( x, y, 200, FILLED, gdAntiAliased )
        :SetAntiAliased( yellow )
        :Circle( x, y, 160, FILLED, gdAntiAliased )

        ? "Create X as a polygon"
        x := 50
        y := 50
        /* Draw polygon */
        :AddPoint( x +  60, y +  60 )
        :AddPoint( x +  85, y + 100 )
        :AddPoint( x +  60, y + 140 )
        :AddPoint( x +  85, y + 140 )
        :AddPoint( x + 100, y + 115 )
        :AddPoint( x + 115, y + 140 )
        :AddPoint( x + 140, y + 140 )
        :AddPoint( x + 115, y + 100 )
        :AddPoint( x + 140, y +  60 )
        :AddPoint( x + 115, y +  60 )
        :AddPoint( x + 100, y +  85 )
        :AddPoint( x +  85, y +  60 )
        :AddPoint( x +  60, y +  60 )
        :SetAntiAliased( black )
        :Polygon(, FILLED, gdAntiAliased )

        // OK, finished circled X
        ? "Crop Image at 40,40 width 220, height 220"
        :Crop( 40, 40, 220, 220 )

        // Now I save image on disk
        ? "Saving X"
        :SaveJpeg( IMAGES_OUT + "xh_x.jpg" )

   END

   // Start final Image, playing with logo
   ?
   ? "Create a true color image in memory"
   oTrueColor := GDImage():CreateTrueColor( 800, 600 )

   // Now work on image
   WITH OBJECT oTrueColor

        //? "Background color (true color comes with black background, we have to fill it)"
        // Allocate white truecolor
        white := :GetTrueColor( 255, 255, 255 )
        // Fill entire area
        :Rectangle( 0, 0, :Width(), :Height(), FILLED, white ) // FILLED = TRUE

        ? "Paste X Logo"
        oX:CopyMerge( , , , , 0, 0, 30, oTrueColor ) // Merging 50%
        oX:CopyMerge( , , , , :Width() - oX:Width(), :Height() - oX:Height(), 70, oTrueColor ) // Merging 50%
        oX:CopyResized( , , , , :CenterWidth() - ( oX:Width() * 1.5 ) / 2, :CenterHeight() - ( oX:Height() * 1.5 ) / 2, oX:Width() * 1.5, oX:Height() * 1.5, oTrueColor )

        ? "Draw a rectangle"
        blue   := :GetTrueColor( 0, 0, 255 )

        nThick := :SetThickness( 3 )
        :Rectangle( 450, 0, 800, 150, NOT_FILLED, blue ) // FILLED = TRUE
        :SetThickness( nThick )

        ? "Load full logo"
        oLogo := GDImage():LoadFromGif( IMAGES_IN + "xHarbour Logo_1024.gif" )

        ? "Zoom"
        // Resize it to width 200 and height prop
        x :=  300 * 100 / oLogo:Width()
        //? x, oLogo:Width(), oLogo:Height(), oLogo:Width() * x / 100, oLogo:Height() * x / 100
        oLogo:Resize( oLogo:Width() * x / 100, oLogo:Height() * x / 100 )
        oLogo:Copy( , , , , 480, 25, oTrueColor )

        ? "Fill with tile a limited area"
        // Set clipping area
        :SetClippingArea( 0, 450, 300, 600 )

        // Reduce X to little x
        oLogo:Resize( 90, 30 )
        oLogo:Rotate( 30 )

        // Set as tile
        :SetTile( oLogo )

        // Fill region with a color, then with tile
        :Fill( 10, 460, blue )
        :Fill( 10, 460, gdTiled )

        // Free Clip Area
        :SetClippingArea( 0, 0, 800, 600 )

        ? "Begin allocating colors"
        red    := :GetTrueColor( 255, 0, 0 )
        black  := :GetTrueColor( 0, 0, 0 )
        blue   := :GetTrueColor( 0, 0, 255 )

        ? "GD Power"
        /* Draw a character. */
        :SetFontGiant()
        //:SetFontMediumBold()
        :SetColor( blue )

        FOR n := 0 TO 70 STEP 15
            :Say( 10 + n, 370 + n, 'xHarbour GD Power')
        NEXT

        :SetColor( red )
        FOR n := 0 TO 70 STEP 15
            :SayVertical( 10 + n, 360, 'xHarbour GD Power')
        NEXT

        :SetThickness( 8 )
        :AddStyle( red )
        :AddStyle( red )
        :AddStyle( red )
        :AddStyle( gdTransparent )
        :AddStyle( gdTransparent )
        :AddStyle( gdTransparent )
        :SetStyle()
        :Line( 90, 360, 90, 210, gdStyled )

        // Now I save image on disk
        ? "Saving Logo"
        :SaveJpeg( IMAGES_OUT + "xh_logo.jpg" )

   END

   /* Destroy images in memory */
   // Class does it auto

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?

RETURN

