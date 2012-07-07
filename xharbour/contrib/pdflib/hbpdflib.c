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

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "pdflib.h"

static PDF* pPDFLib = NULL;

HB_FUNC( PDF_NEW )
{
   hb_retni( ( pPDFLib = PDF_new() ) ? 1 : 0 );
}

HB_FUNC( PDF_DELETE )
{
   PDF_delete( pPDFLib );
   pPDFLib = NULL;
}

HB_FUNC( PDF_BEGIN_DOCUMENT )
{
   hb_retni( PDF_begin_document(pPDFLib, hb_parc(1), hb_parni(2), hb_parc(3) ) );
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
   hb_retni( PDF_findfont( pPDFLib, hb_parc(1), hb_parc(2), hb_parni(3) ) );
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
