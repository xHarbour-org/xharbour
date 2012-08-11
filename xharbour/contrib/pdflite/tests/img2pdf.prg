/*
 * $Id$
 *
 * Test program for PDFLib
 *
 * Andi Jahja
 */

#message "/ ---------------------------------------------------------\"
#message "| Demo to convert images to pdf                            |"
#message "| Currrently support: PNG, BMP, GIF, JPEG, TIFF formats    |"
#message "| This program requires: jpeg.lib, tiff.lib, png.lib       |"
#message "| and pdflite.lib aside the regular libraries              |"
#message "\ ---------------------------------------------------------/"

PROCEDURE MAIN()

   LOCAL pdffilename := "pdflib1.pdf"
   LOCAL resolution := 320
   LOCAL graylevel := 1.0
   LOCAL current_page := 1
   LOCAL image
   LOCAL imageno := 1
   LOCAL aPicture := {"mygif.gif","mybmp.bmp","mypng.png", "myjpg.jpg", "mytiff.tiff"}

   if PDF_new()

      IF PDF_begin_document( pdffilename )

         PDF_set_info( "Creator", "xHarbour-Image2PDF" )
         PDF_set_parameter( "warning", "false" )

         FOR imageno := 1 TO Len( aPicture )
            IF ( image := PDF_load_image( aPicture[ imageno ] ) ) != -1
               PDF_begin_page_ext( 20, 20 )
               PDF_setcolor( "fill", "gray", graylevel )
               PDF_rect( 0, 0, 10000, 10000)
               PDF_fill()
               PDF_create_bookmark( "Page " + ltrim(str( current_page++ )) )
               PDF_fit_image( image, 0.0, 0.0, sprintf( "dpi %d adjustpage", resolution) )
               PDF_end_page_ext()
               PDF_close_image( image )
            ELSE
               ? "Error Loading: " + aPicture[ imageno ]
            ENDIF
         NEXT

         PDF_end_document()

      ENDIF

      PDF_delete()

   endif

   RETURN

