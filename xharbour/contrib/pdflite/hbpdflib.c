/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Wrapper for PDFLib Runtime
 *
 * Copyright 2012 Andi Jahja <xharbour /at/ telkon.net.id>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#if defined( _MSC_VER ) && ( _MSC_VER>=1300 )
   #pragma warning (disable:4995)
#endif


#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "pdflib.h"
#if defined( HB_OS_WIN )
   #include "windows.h"
#endif

static PDF* pPDFLib = NULL;

HB_FUNC( PDF_NEW )
{
   pPDFLib = PDF_new();
   hb_retl( pPDFLib ? TRUE : FALSE );
}

HB_FUNC( PDF_DELETE )
{
   PDF_delete( pPDFLib );
   pPDFLib = NULL;
}

HB_FUNC( PDF_BEGIN_DOCUMENT )
{
   if( ISCHAR(1) )
   {
      hb_retl( PDF_begin_document(pPDFLib,
         hb_parc( 1 ),
         ISNUM(2)  ? hb_parni(2) : 0,
         ISCHAR(3) ? hb_parc(3) : "" ) != -1 );
      return;
   }
   hb_retl( FALSE );
}

HB_FUNC( PDF_SET_INFO )
{
    PDF_set_info(pPDFLib, hb_parc(1), hb_parc(2) );
}

HB_FUNC( PDF_BEGIN_PAGE_EXT )
{
    PDF_begin_page_ext(pPDFLib, hb_parnd(1), hb_parnd(2), hb_parc(3) );
}

HB_FUNC( PDF_END_PAGE_EXT )
{
   PDF_end_page_ext(pPDFLib, hb_parc(1) );
}

HB_FUNC( PDF_LOAD_FONT )
{
   hb_retni( PDF_load_font(pPDFLib, hb_parc(1), hb_parni(2), hb_parc(3), hb_parc(4) ) );
}

HB_FUNC( PDF_SETFONT )
{
   PDF_setfont(pPDFLib, hb_parni(1), hb_parnd(2) );
}

HB_FUNC( PDF_FINDFONT )
{
   hb_retni( PDF_findfont( pPDFLib, hb_parc(1), ISCHAR(2) ? hb_parc(2) : "host", hb_parni(3) ) );
}

HB_FUNC( PDF_SET_TEXT_POS )
{
   PDF_set_text_pos(pPDFLib, hb_parnd(1), hb_parnd(2) );
}

HB_FUNC( PDF_CONTINUE_TEXT )
{
   PDF_continue_text(pPDFLib, hb_parc(1) );
}

HB_FUNC( PDF_END_DOCUMENT )
{
   PDF_end_document( pPDFLib, hb_parc(1) );
}

HB_FUNC( PDF_SET_PARAMETER )
{
   PDF_set_parameter(pPDFLib, hb_parc(1), hb_parc(2) );
}

HB_FUNC( PDF_CLOSE_IMAGE )
{
   PDF_close_image(pPDFLib, hb_parni(1) );
}

HB_FUNC( PDF_LOAD_IMAGE )
{
   if ( ISCHAR(1) )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_parcx( 1 ) );

      if( pFileName->szExtension )
      {
         char *szExt = hb_strlow( (char*) pFileName->szExtension );
         char *szImageType = NULL;

         if ( strcmp( szExt, ".gif" ) == 0 )
            szImageType = "gif";
         else if ( strcmp( szExt, ".bmp" ) == 0 )
            szImageType = "bmp";
         else if ( strcmp( szExt, ".png" ) == 0 )
            szImageType = "png";
         else if ( strcmp( szExt, ".jpg" ) == 0 || strcmp( szExt, ".jpeg" ) == 0 )
            szImageType = "jpeg";
         else if ( strcmp( szExt, ".tif" ) == 0 || strcmp( szExt, ".tiff" ) == 0 )
            szImageType = "tiff";
#if 0
         /* PDFLib Lite does not support JPEG2000. This format requires
            commercial version */
         else if ( strcmp( szExt, ".jpx" ) == 0 || strcmp( szExt, ".jp2" ) == 0 ||
                   strcmp( szExt, ".jpf" ) == 0 || strcmp( szExt, ".jpm" ) == 0 ||
                   strcmp( szExt, ".j2k" ) == 0 )
            szImageType = "jpeg2000";
#endif
         hb_xfree( pFileName );

         if ( szImageType )
         {
            hb_retni( PDF_load_image( pPDFLib, szImageType, hb_parcx(1), 0, hb_parcx(2) ) );
            return;
         }
      }
      hb_xfree( pFileName );
   }

   hb_retni( -1 );
}

HB_FUNC( PDF_GET_ERRMSG )
{
   hb_retc( PDF_get_errmsg(pPDFLib ) );
}

HB_FUNC( PDF_SETCOLOR )
{
   PDF_setcolor(pPDFLib, hb_parc(1), hb_parc(2), hb_parnd(3), hb_parnd(4), hb_parnd(5), hb_parnd(6) );
}

HB_FUNC( PDF_RECT )
{
   PDF_rect(pPDFLib, hb_parnd(1), hb_parnd(2), hb_parnd(3), hb_parnd(4) );
}

HB_FUNC( PDF_FILL )
{
   PDF_fill(pPDFLib);
}

HB_FUNC( PDF_CREATE_BOOKMARK )
{
   hb_retni( PDF_add_bookmark(pPDFLib, hb_parc(1), hb_parni(2), hb_parni(3) ) );
}

HB_FUNC( PDF_FIT_IMAGE )
{
   PDF_fit_image( pPDFLib, hb_parni(1), hb_parnd(2), hb_parnd(3), hb_parc(4) );
}
