/*
 * $Id$
 *
 * Test program for PDFLib
 *
 * Andi Jahja
 */

#define BUFLEN   120

PROCEDURE MAIN()

   LOCAL x, y, handle, sz := space( BUFLEN )
   LOCAL font
   LOCAL fontname := "Courier-Bold"
   LOCAL height := 842
   LOCAL margin := 20
   LOCAL pdffilename := "pdflib.pdf"
   LOCAL fontsize := 12
   LOCAL width := 595
   LOCAL cTextFile := "pdflib.txt"

   if PDF_new()

      if ( handle := fopen( cTextFile ) ) != -1

         x := margin
         y := height - margin

         PDF_begin_document( pdffilename )
         PDF_set_info( "Title",   "Converted text" )
         PDF_set_info( "Creator", "Text2pdf" )

         while HB_freadline ( handle, @sz, BUFLEN ) == 0

            if (y < margin)
                y := height - margin
                PDF_end_page_ext()
            endif

            if ( y == height - margin )
                font := PDF_findfont( fontname )
                PDF_begin_page_ext(width, height )
                PDF_setfont( font, fontsize )
                PDF_set_text_pos( x, y )
                y -= fontsize
            endif

            PDF_continue_text(sz)
            y -= fontsize

         enddo

         fclose( handle )

         if (y != height - margin)
            PDF_end_page_ext()
         endif

      endif

      PDF_end_document()
      PDF_delete()

   endif

   RETURN
