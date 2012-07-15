/*
 * $Id$
 *
 * Test program for PDFLib
 *
 * Andi Jahja
 */

#message "/ ---------------------------------------------------------\"
#message "| Demo to convert images to pdf                            |"
#message "| Currrently support: PNG, BMP, GIF formats                |"
#message "| This program requires: jpeg.lib, tiff.lib, pdflite.lib   |"
#message "\ ---------------------------------------------------------/"

PROCEDURE MAIN( cImage )

   LOCAL pdffilename := "pdflib1.pdf"
   LOCAL resolution := 320
   LOCAL graylevel := 1.0
   LOCAL current_page := 1
   LOCAL image, parent
   LOCAL imageno := 1
   LOCAL aPicture := {"mygif.gif","mybmp.bmp","mypng.png"}

   if PDF_new()

      PDF_begin_document( pdffilename )
      PDF_set_info( "Creator", "xHarbour-Image2PDF" )
      PDF_set_parameter( "warning", "false" )

      FOR imageno := 1 TO Len( aPicture )
         cImage := aPicture[ imageno ]
         image := PDF_load_image( cImage )
         IF image != -1
            PDF_begin_page_ext( 20, 20 )
            PDF_setcolor( "fill", "gray", graylevel )
            PDF_rect( 0, 0, 10000, 10000)
            PDF_fill()
            PDF_create_bookmark( "Page " + ltrim(str( current_page++ )) )
            PDF_fit_image( image, 0.0, 0.0, sprintf( "dpi %d adjustpage", resolution) )
            PDF_end_page_ext()
            PDF_close_image( image )
         else
            ? "Error Loading: " + cImage
         endif
      NEXT

      PDF_end_document()
      PDF_delete()
   endif

   RETURN

