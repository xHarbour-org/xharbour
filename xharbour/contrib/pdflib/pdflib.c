/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Dynamic Import Library for PDFLib Runtime
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
#include "hbapierr.h"
#include "hbvm.h"
#include "hbinit.h"
#include "pdflib.h"

typedef size_t (*writeproc_t)(PDF *p1, void *data, size_t size);
typedef void   (*errorproc_t)(PDF *p1, int errortype, const char *msg);
typedef void*  (*allocproc_t)(PDF *p2, size_t size, const char *caller);
typedef void*  (*reallocproc_t)(PDF *p3, void *mem, size_t size, const char *caller);
typedef void   (*freeproc_t)(PDF *p4, void *mem);
typedef int    (PDFLIB_CALL *PDF_FINDFONT) (PDF *p, const char *fontname, const char *encoding, int embed);
typedef void   (PDFLIB_CALL *PDF_SET_INFO) (PDF *p, const char *key, const char *value);
typedef int    (PDFLIB_CALL *PDF_OPEN_FILE) (PDF *p, const char *filename);
typedef void   (PDFLIB_CALL *PDF_SET_VALUE) (PDF *p, const char *key, double value);
typedef PDF*   (PDFLIB_CALL *PDF_NEW) (void);
typedef void   (PDFLIB_CALL *PDF_SHOW_XY) (PDF *p, const char *text, double x, double y);
typedef void   (PDFLIB_CALL *PDF_SETFONT) (PDF *p, int font, double fontsize);
typedef void   (PDFLIB_CALL *PDF_BEGIN_PAGE) (PDF *p, double width, double height);
typedef void   (PDFLIB_CALL *PDF_END_PAGE) (PDF *p);
typedef void   (PDFLIB_CALL *PDF_DELETE) (PDF *);
typedef void   (PDFLIB_CALL *PDF_CLOSE) (PDF *p);
typedef void   (PDFLIB_CALL *PDF_SHOW) (PDF *p, const char *text);
typedef void   (PDFLIB_CALL *PDF_SET_PARAMETER) (PDF *p, const char *key, const char *value);
typedef void   (PDFLIB_CALL *PDF_SETRGBCOLOR) (PDF *p, double red, double green, double blue);
typedef void   (PDFLIB_CALL *PDF_ADD_LOCALLINK) (PDF *p, double llx, double lly, double urx, double ury, int page, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_SET_BORDER_COLOR) (PDF *p, double red, double green, double blue);
typedef int    (PDFLIB_CALL *PDF_ADD_BOOKMARK) (PDF *p, const char *text, int parent, int open);
typedef double (PDFLIB_CALL *PDF_STRINGWIDTH) (PDF *p, const char *text, int font, double fontsize);
typedef int    (PDFLIB_CALL *PDF_SHOW_BOXED) (PDF *p, const char *text, double left, double top, double width, double height, const char *hmode, const char *feature);
typedef void   (PDFLIB_CALL *PDF_STROKE) (PDF *p);
typedef void   (PDFLIB_CALL *PDF_RECT) (PDF *p, double x, double y, double width, double height);
typedef void   (PDFLIB_CALL *PDF_ACTIVATE_ITEM)(PDF *p, int id);
typedef int    (PDFLIB_CALL *PDF_ADD_BOOKMARK2) (PDF *p, const char *text, int len, int parent, int open);
typedef void   (PDFLIB_CALL *PDF_ADD_LAUNCHLINK)(PDF *p, double llx, double lly, double urx, double ury, const char *filename);
typedef void   (PDFLIB_CALL *PDF_ADD_NAMEDDEST) (PDF *p, const char *name, int len, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_ADD_NOTE) (PDF *p, double llx, double lly, double urx, double ury, const char *contents, const char *title, const char *icon, int open);
typedef void   (PDFLIB_CALL *PDF_ADD_NOTE2) (PDF *p, double llx, double lly, double urx, double ury, const char *contents, int len_cont, const char *title, int len_title, const char *icon, int open);
typedef void   (PDFLIB_CALL *PDF_ADD_PDFLINK) (PDF *p, double llx, double lly, double urx, double ury,  const char *filename, int page, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_ADD_THUMBNAIL) (PDF *p, int image);
typedef void   (PDFLIB_CALL *PDF_ADD_WEBLINK) (PDF *p, double llx, double lly, double urx, double ury, const char *url);
typedef void   (PDFLIB_CALL *PDF_ARC) (PDF *p, double x, double y, double r, double alpha, double beta);
typedef void   (PDFLIB_CALL *PDF_ARCN) (PDF *p, double x, double y, double r, double alpha, double beta);
typedef void   (PDFLIB_CALL *PDF_ATTACH_FILE) (PDF *p, double llx, double lly, double urx, double ury, const char *filename, const char *description, const char *author, const char *mimetype, const char *icon);
typedef void   (PDFLIB_CALL *PDF_ATTACH_FILE2) (PDF *p, double llx, double lly, double urx, double ury, const char *filename, int len_filename, const char *description, int len_descr, const char *author, int len_auth, const char *mimetype, const char *icon);
typedef int    (PDFLIB_CALL *PDF_BEGIN_DOCUMENT)(PDF *p, const char *filename, int len, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_BEGIN_DOCUMENT_CALLBACK)(PDF *p, writeproc_t writeproc, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_BEGIN_FONT)(PDF *p, const char *fontname, int len, double a, double b, double c, double d, double e, double f, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_BEGIN_GLYPH)(PDF *p, const char *glyphname, double wx, double llx, double lly, double urx, double ury);
typedef int    (PDFLIB_CALL *PDF_BEGIN_ITEM)(PDF *p, const char *tag, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_BEGIN_LAYER)(PDF *p, int layer);
typedef void   (PDFLIB_CALL *PDF_BEGIN_MC)(PDF *p, const char *tag, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_BEGIN_PAGE_EXT)(PDF *p, double width, double height, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_BEGIN_PATTERN)(PDF *p, double width, double height, double xstep, double ystep, int painttype);
typedef int    (PDFLIB_CALL *PDF_BEGIN_TEMPLATE)(PDF *p, double width, double height);
typedef void   (PDFLIB_CALL *PDF_BOOT)(void);
typedef void   (PDFLIB_CALL *PDF_CIRCLE)(PDF *p, double x, double y, double r);
typedef void   (PDFLIB_CALL *PDF_CLIP)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_CLOSE_IMAGE)(PDF *p, int image);
typedef void   (PDFLIB_CALL *PDF_CLOSE_PDI)(PDF *p, int doc);
typedef void   (PDFLIB_CALL *PDF_CLOSE_PDI_PAGE)(PDF *p, int page);
typedef void   (PDFLIB_CALL *PDF_CLOSEPATH)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_CLOSEPATH_FILL_STROKE)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_CLOSEPATH_STROKE)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_CONCAT)(PDF *p, double a, double b, double c, double d, double e, double f);
typedef void   (PDFLIB_CALL *PDF_CONTINUE_TEXT)(PDF *p, const char *text);
typedef void   (PDFLIB_CALL *PDF_CONTINUE_TEXT2)(PDF *p, const char *text, int len);
typedef int    (PDFLIB_CALL *PDF_CREATE_ACTION)(PDF *p, const char *type, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_CREATE_ANNOTATION)(PDF *p, double llx, double lly, double urx, double ury, const char *type, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_CREATE_BOOKMARK)(PDF *p, const char *text, int len, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_CREATE_FIELD) (PDF *p, double llx, double lly, double urx, double ury, const char *name, int len, const char *type, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_CREATE_FIELDGROUP) (PDF *p, const char *name, int len, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_CREATE_GSTATE) (PDF *p, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_CREATE_PVF)(PDF *p, const char *filename, int len, const void *data, size_t size, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_CREATE_TEXTFLOW)(PDF *p, const char *text, int len, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_CURVETO)(PDF *p, double x_1, double y_1, double x_2, double y_2, double x_3, double y_3);
typedef int    (PDFLIB_CALL *PDF_DEFINE_LAYER)(PDF *p, const char *name, int len, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_DELETE_PVF)(PDF *p, const char *filename, int len);
typedef void   (PDFLIB_CALL *PDF_DELETE_TEXTFLOW)(PDF *p, int textflow);
typedef void   (PDFLIB_CALL *PDF_ENCODING_SET_CHAR) (PDF *p, const char *encoding, int slot, const char *glyphname, int uv);
typedef void   (PDFLIB_CALL *PDF_END_DOCUMENT)(PDF *p, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_END_FONT)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_END_GLYPH)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_END_ITEM)(PDF *p, int id);
typedef void   (PDFLIB_CALL *PDF_END_LAYER)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_END_MC)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_END_PAGE_EXT)(PDF *p, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_END_PATTERN)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_END_TEMPLATE)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_ENDPATH)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_FILL)(PDF *P);
typedef int    (PDFLIB_CALL *PDF_FILL_IMAGEBLOCK) (PDF *p, int page, const char *blockname, int image, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_FILL_PDFBLOCK) (PDF *p, int page, const char *blockname, int contents, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_FILL_STROKE)(PDF *p);
typedef const char * (PDFLIB_CALL * PDF_FIT_TEXTFLOW)(PDF *p, int textflow, double llx, double lly, double urx, double ury, const char *optlist);
typedef const PDFlib_api * (PDFLIB_CALL *PDF_GET_API)(void);
typedef const char * (PDFLIB_CALL *PDF_GET_APINAME) (PDF *p);
typedef const char * (PDFLIB_CALL *PDF_GET_BUFFER)(PDF *p, long *size);
typedef const char * (PDFLIB_CALL *PDF_GET_ERRMSG) (PDF *p);
typedef const char * (PDFLIB_CALL *PDF_GET_PARAMETER)(PDF *p, const char *key, double modifier);
typedef const char * (PDFLIB_CALL *PDF_GET_PDI_PARAMETER)(PDF *p, const char *key, int doc, int page, int reserved, int *len);
typedef int    (PDFLIB_CALL *PDF_FILL_TEXTBLOCK) (PDF *p, int page, const char *blockname, const char *text, int len, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_FIT_IMAGE) (PDF *p, int image, double x, double y, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_FIT_PDI_PAGE) (PDF *p, int page, double x, double y, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_FIT_TEXTLINE)(PDF *p, const char *text, int len, double x, double y, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_GET_ERRNUM) (PDF *p);
typedef int    (PDFLIB_CALL *PDF_GET_MINORVERSION)(void);
typedef int    (PDFLIB_CALL *PDF_GET_MAJORVERSION)(void);
typedef void * (PDFLIB_CALL *PDF_GET_OPAQUE)(PDF *p);
typedef double (PDFLIB_CALL *PDF_GET_PDI_VALUE)(PDF *p, const char *key, int doc, int page, int reserved);
typedef double (PDFLIB_CALL *PDF_GET_VALUE)(PDF *p, const char *key, double modifier);
typedef double (PDFLIB_CALL *PDF_INFO_TEXTFLOW)(PDF *p, int textflow, const char *keyword);
typedef void   (PDFLIB_CALL *PDF_INITGRAPHICS)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_LINETO)(PDF *p, double x, double y);
typedef int    (PDFLIB_CALL *PDF_LOAD_FONT)(PDF *p, const char *fontname, int len, const char *encoding, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_LOAD_ICCPROFILE)(PDF *p, const char *profilename, int len, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_LOAD_IMAGE) (PDF *p, const char *imagetype, const char *filename, int len, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_MAKESPOTCOLOR)(PDF *p, const char *spotname, int len);
typedef void   (PDFLIB_CALL *PDF_MC_POINT)(PDF *p, const char *tag, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_MOVETO)(PDF *p, double x, double y);
typedef PDF*   (PDFLIB_CALL *PDF_NEW2)(errorproc_t errorhandler, allocproc_t allocproc, reallocproc_t reallocproc, freeproc_t freeproc, void *opaque);
typedef int    (PDFLIB_CALL *PDF_OPEN_CCITT)(PDF *p, const char *filename, int width, int height, int BitReverse, int K, int BlackIs1);
typedef int    (PDFLIB_CALL *PDF_OPEN_IMAGE)(PDF *p, const char *imagetype, const char *source, const char *data, long length, int width, int height, int components, int bpc, const char *params);
typedef int    (PDFLIB_CALL *PDF_OPEN_IMAGE_FILE)(PDF *p, const char *imagetype, const char *filename, const char *stringparam, int intparam);
typedef void   (PDFLIB_CALL *PDF_OPEN_MEM)(PDF *p, writeproc_t writeproc);
typedef int    (PDFLIB_CALL *PDF_OPEN_PDI)(PDF *p, const char *filename, const char *optlist, int len);
typedef int    (PDFLIB_CALL *PDF_OPEN_PDI_CALLBACK) (PDF *p, void *opaque, size_t filesize, size_t (*readproc)(void *opaque, void *buffer, size_t size), int (*seekproc)(void *opaque, long offset), const char *optlist);
typedef int    (PDFLIB_CALL *PDF_OPEN_PDI_PAGE)(PDF *p, int doc, int pagenumber, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_PLACE_IMAGE)(PDF *p, int image, double x, double y, double scale);
typedef void   (PDFLIB_CALL *PDF_PLACE_PDI_PAGE)(PDF *p, int page, double x, double y, double sx, double sy);
typedef int    (PDFLIB_CALL *PDF_PROCESS_PDI)(PDF *p, int doc, int page, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_RESTORE)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_RESUME_PAGE)(PDF *p, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_ROTATE)(PDF *p, double phi);
typedef void   (PDFLIB_CALL *PDF_SAVE)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_SCALE)(PDF *p, double sx, double sy);
typedef void   (PDFLIB_CALL *PDF_SET_BORDER_DASH)(PDF *p, double b, double w);
typedef void   (PDFLIB_CALL *PDF_SET_BORDER_STYLE)(PDF *p, const char *style, double width);
typedef void   (PDFLIB_CALL *PDF_SET_GSTATE) (PDF *p, int gstate);
typedef void   (PDFLIB_CALL *PDF_SET_INFO2) (PDF *p, const char *key, const char *value, int len);
typedef void   (PDFLIB_CALL *PDF_SET_LAYER_DEPENDENCY)(PDF *p, const char *type, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_SET_TEXT_POS)(PDF *p, double x, double y);
typedef void   (PDFLIB_CALL *PDF_SETCOLOR)(PDF *p, const char *fstype, const char *colorspace, double c1, double c2, double c3, double c4);
typedef void   (PDFLIB_CALL *PDF_SETDASH)(PDF *p, double b, double w);
typedef void   (PDFLIB_CALL *PDF_SETDASHPATTERN) (PDF *p, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_SETFLAT)(PDF *p, double flatness);
typedef void   (PDFLIB_CALL *PDF_SETGRAY)(PDF *p, double gray);
typedef void   (PDFLIB_CALL *PDF_SETGRAY_STROKE)(PDF *p, double gray);
typedef void   (PDFLIB_CALL *PDF_SETGRAY_FILL)(PDF *p, double gray);
typedef void   (PDFLIB_CALL *PDF_SETLINECAP)(PDF *p, int linecap);
typedef void   (PDFLIB_CALL *PDF_SETLINEJOIN)(PDF *p, int linejoin);
typedef void   (PDFLIB_CALL *PDF_SETLINEWIDTH)(PDF *p, double width);
typedef void   (PDFLIB_CALL *PDF_SETMATRIX)(PDF *p, double a, double b, double c, double d, double e, double f);
typedef void   (PDFLIB_CALL *PDF_SETMITERLIMIT)(PDF *p, double miter);
typedef void   (PDFLIB_CALL *PDF_SETPOLYDASH)(PDF *p, float *dasharray, int length);
typedef void   (PDFLIB_CALL *PDF_SETRGBCOLOR_FILL)(PDF *p, double red, double green, double blue);
typedef void   (PDFLIB_CALL *PDF_SETRGBCOLOR_STROKE)(PDF *p, double red, double green, double blue);
typedef int    (PDFLIB_CALL *PDF_SHADING) (PDF *p, const char *shtype, double x_0, double y_0, double x_1, double y_1, double c_1, double c_2, double c_3, double c_4, const char *optlist);
typedef int    (PDFLIB_CALL *PDF_SHADING_PATTERN) (PDF *p, int shading, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_SHFILL) (PDF *p, int shading);
typedef void   (PDFLIB_CALL *PDF_SHOW2)(PDF *p, const char *text, int len);
typedef int    (PDFLIB_CALL *PDF_SHOW_BOXED2)(PDF *p, const char *text, int len, double left, double top, double width, double height, const char *hmode, const char *feature);
typedef void   (PDFLIB_CALL *PDF_SHOW_XY2)(PDF *p, const char *text, int len, double x, double y);
typedef void   (PDFLIB_CALL *PDF_SHUTDOWN)(void);
typedef void   (PDFLIB_CALL *PDF_SKEW)(PDF *p, double alpha, double beta);
typedef double (PDFLIB_CALL *PDF_STRINGWIDTH2)(PDF *p, const char *text, int len, int font, double fontsize);
typedef const char * (PDFLIB_CALL *PDF_UTF16_TO_UTF8) (PDF *p, const char *utf16string, int len, int *size);
typedef const char * (PDFLIB_CALL *PDF_UTF8_TO_UTF16) (PDF *p, const char *utf8string, const char *format, int *size);
typedef pdf_jmpbuf * (PDFLIB_CALL *PDF_JBUF)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_RETHROW)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_SUSPEND_PAGE)(PDF *p, const char *optlist);
typedef void   (PDFLIB_CALL *PDF_TRANSLATE)(PDF *p, double tx, double ty);
typedef void   (PDFLIB_CALL *PDF_XSHOW)(PDF *p, const char *text, int len, const double *xadvancelist);
typedef int    (PDFLIB_CALL *PDF_CATCH)(PDF *p);
typedef void   (PDFLIB_CALL *PDF_EXIT_TRY)(PDF *p);
typedef int    (PDFLIB_CALL *PDF_GET_MAJORVERSION)(void);
typedef int    (PDFLIB_CALL *PDF_GET_MINORVERSION)(void);

#if defined( HB_OS_WIN )
   #define PDFLIBDLL_NAME "pdflib5.dll"
#endif

static HMODULE hModule = NULL;

static FARPROC PDFLib_GetProcAddress( char* szFuncName )
{
   FARPROC pFunc = GetProcAddress( hModule, szFuncName );

   if ( pFunc )
   {
      return pFunc;
   }
   else
   {
      char __szError[256];
      hb_snprintf( __szError, sizeof( __szError ), "Cannot find function: %s in %s", szFuncName, PDFLIBDLL_NAME );
      hb_errInternal( 5178, __szError, NULL, NULL );
      return NULL;
   }
}

int PDF_findfont (PDF *p, const char *fontname, const char *encoding, int embed )
{
#if defined(__cplusplus)
    static PDF_FINDFONT
#else
    static PDF_FINDFONT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_FINDFONT) PDFLib_GetProcAddress( "PDF_findfont" );

    return pFunc( p, fontname, encoding, embed );
}

int PDF_load_font(PDF *p, const char *fontname, int len, const char *encoding, const char *optlist )
{
#if defined(__cplusplus)
    static PDF_LOAD_FONT
#else
    static PDF_LOAD_FONT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_LOAD_FONT) PDFLib_GetProcAddress( "PDF_load_font" );

    return pFunc( p, fontname, len, encoding, optlist );
}

void PDF_set_info (PDF *p, const char *key, const char *value)
{
#if defined(__cplusplus)
    static PDF_SET_INFO
#else
    static PDF_SET_INFO pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_INFO) PDFLib_GetProcAddress( "PDF_set_info" );

    pFunc( p, key, value );
}

int PDF_open_file (PDF *p, const char *filename)
{
#if defined(__cplusplus)
    static PDF_OPEN_FILE
#else
    static PDF_OPEN_FILE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_OPEN_FILE) PDFLib_GetProcAddress( "PDF_open_file" );

    return pFunc( p, filename );
}

void PDF_set_value (PDF *p, const char *key, double value)
{
#if defined(__cplusplus)
    static PDF_SET_VALUE
#else
    static PDF_SET_VALUE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_VALUE) PDFLib_GetProcAddress( "PDF_set_value" );

    pFunc( p, key, value );
}

PDF* PDF_new ( void )
{
#if defined(__cplusplus)
    static PDF_NEW
#else
    static PDF_NEW pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_NEW) PDFLib_GetProcAddress( "PDF_new" );

    return pFunc();
}

void PDF_show_xy(PDF *p, const char *text, double x, double y)
{
#if defined(__cplusplus)
    static PDF_SHOW_XY
#else
    static PDF_SHOW_XY pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHOW_XY) PDFLib_GetProcAddress( "PDF_show_xy" );

    pFunc(p, text, x, y);
}

void PDF_setfont (PDF *p, int font, double fontsize)
{
#if defined(__cplusplus)
    static PDF_SETFONT
#else
    static PDF_SETFONT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETFONT) PDFLib_GetProcAddress( "PDF_setfont" );

    pFunc( p, font, fontsize );
}

void PDF_begin_page(PDF *p, double width, double height)
{
#if defined(__cplusplus)
    static PDF_BEGIN_PAGE
#else
    static PDF_BEGIN_PAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_PAGE) PDFLib_GetProcAddress( "PDF_begin_page" );

    pFunc( p, width, height );
}

void PDF_end_page(PDF *p)
{
#if defined(__cplusplus)
    static PDF_END_PAGE
#else
    static PDF_END_PAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_PAGE) PDFLib_GetProcAddress( "PDF_end_page" );

    pFunc( p );
}

void PDF_delete(PDF *p)
{
#if defined(__cplusplus)
    static PDF_DELETE
#else
    static PDF_DELETE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_DELETE) PDFLib_GetProcAddress( "PDF_delete" );

    pFunc( p );
}

void PDF_close(PDF *p)
{
#if defined(__cplusplus)
    static PDF_CLOSE
#else
    static PDF_CLOSE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CLOSE) PDFLib_GetProcAddress( "PDF_close" );

    pFunc( p );
}

void PDF_show(PDF *p, const char *text)
{
#if defined(__cplusplus)
    static PDF_SHOW
#else
    static PDF_SHOW pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHOW) PDFLib_GetProcAddress( "PDF_show" );

    pFunc( p, text );
}

void PDF_set_parameter (PDF *p, const char *key, const char *value)
{
#if defined(__cplusplus)
    static PDF_SET_PARAMETER
#else
    static PDF_SET_PARAMETER pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_PARAMETER) PDFLib_GetProcAddress( "PDF_set_parameter" );

    pFunc( p, key, value );
}

void PDF_setrgbcolor(PDF *p, double red, double green, double blue)
{
#if defined(__cplusplus)
    static PDF_SETRGBCOLOR
#else
    static PDF_SETRGBCOLOR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETRGBCOLOR) PDFLib_GetProcAddress( "PDF_setrgbcolor" );

    pFunc( p, red, green, blue );
}

void PDF_add_locallink(PDF *p, double llx, double lly, double urx, double ury, int page, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_ADD_LOCALLINK
#else
    static PDF_ADD_LOCALLINK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_LOCALLINK) PDFLib_GetProcAddress( "PDF_add_locallink" );

    pFunc( p, llx, lly, urx, ury, page, optlist );
}

void PDF_set_border_color (PDF *p, double red, double green, double blue)
{
#if defined(__cplusplus)
    static PDF_SET_BORDER_COLOR
#else
    static PDF_SET_BORDER_COLOR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_BORDER_COLOR) PDFLib_GetProcAddress( "PDF_set_border_color" );

    pFunc( p, red, green, blue );
}

int PDF_add_bookmark(PDF *p, const char *text, int parent, int open)
{
#if defined(__cplusplus)
    static PDF_ADD_BOOKMARK
#else
    static PDF_ADD_BOOKMARK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_BOOKMARK) PDFLib_GetProcAddress( "PDF_add_bookmark" );

    return pFunc( p, text, parent, open );
}

double PDF_stringwidth (PDF *p, const char *text, int font, double fontsize)
{
#if defined(__cplusplus)
    static PDF_STRINGWIDTH
#else
    static PDF_STRINGWIDTH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_STRINGWIDTH) PDFLib_GetProcAddress( "PDF_stringwidth" );

    return pFunc( p, text, font, fontsize );
}

int PDF_show_boxed (PDF *p, const char *text, double left, double top, double width, double height, const char *hmode, const char *feature)
{
#if defined(__cplusplus)
    static PDF_SHOW_BOXED
#else
    static PDF_SHOW_BOXED pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHOW_BOXED) PDFLib_GetProcAddress( "PDF_show_boxed" );

    return pFunc( p, text, left, top, width, height, hmode, feature );
}

void PDF_stroke (PDF *p)
{
#if defined(__cplusplus)
    static PDF_STROKE
#else
    static PDF_STROKE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_STROKE) PDFLib_GetProcAddress( "PDF_stroke" );

    pFunc( p );
}

void PDF_rect (PDF *p, double x, double y, double width, double height)
{
#if defined(__cplusplus)
    static PDF_RECT
#else
    static PDF_RECT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_RECT) PDFLib_GetProcAddress( "PDF_rect" );

    pFunc( p, x, y, width, height );
}

void PDF_activate_item(PDF *p, int id)
{
#if defined(__cplusplus)
    static PDF_ACTIVATE_ITEM
#else
    static PDF_ACTIVATE_ITEM pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ACTIVATE_ITEM) PDFLib_GetProcAddress( "PDF_activate_item" );

    pFunc( p, id );
}

int PDF_add_bookmark2 (PDF *p, const char *text, int len, int parent, int open)
{
#if defined(__cplusplus)
    static PDF_ADD_BOOKMARK2
#else
    static PDF_ADD_BOOKMARK2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_BOOKMARK2) PDFLib_GetProcAddress( "PDF_add_bookmark2" );

    return pFunc( p, text, len, parent, open);
}

void PDF_add_launchlink (PDF *p, double llx, double lly, double urx, double ury, const char *filename)
{
#if defined(__cplusplus)
    static PDF_ADD_LAUNCHLINK
#else
    static PDF_ADD_LAUNCHLINK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_LAUNCHLINK) PDFLib_GetProcAddress( "PDF_add_launchlink" );

    pFunc( p, llx, lly, urx, ury, filename);
}

void PDF_add_nameddest (PDF *p, const char *name, int len, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_ADD_NAMEDDEST
#else
    static PDF_ADD_NAMEDDEST pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_NAMEDDEST) PDFLib_GetProcAddress( "PDF_add_nameddest" );

    pFunc( p, name, len, optlist );
}

void PDF_add_note(PDF *p, double llx, double lly, double urx, double ury, const char *contents, const char *title, const char *icon, int open)
{
#if defined(__cplusplus)
    static PDF_ADD_NOTE
#else
    static PDF_ADD_NOTE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_NOTE) PDFLib_GetProcAddress( "PDF_add_note" );

    pFunc( p, llx, lly, urx, ury, contents, title, icon, open );
}

void PDF_add_note2 (PDF *p, double llx, double lly, double urx, double ury, const char *contents, int len_cont, const char *title, int len_title, const char *icon, int open)
{
#if defined(__cplusplus)
    static PDF_ADD_NOTE2
#else
    static PDF_ADD_NOTE2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_NOTE2) PDFLib_GetProcAddress( "PDF_add_note2" );

    pFunc( p, llx, lly, urx, ury, contents, len_cont, title, len_title, icon, open );
}

void PDF_add_pdflink (PDF *p, double llx, double lly, double urx, double ury, const char *filename, int page, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_ADD_PDFLINK
#else
    static PDF_ADD_PDFLINK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_PDFLINK) PDFLib_GetProcAddress( "PDF_add_pdflink" );

    pFunc( p, llx, lly, urx, ury, filename, page, optlist);
}

void PDF_add_thumbnail (PDF *p, int image)
{
#if defined(__cplusplus)
    static PDF_ADD_THUMBNAIL
#else
    static PDF_ADD_THUMBNAIL pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_THUMBNAIL) PDFLib_GetProcAddress( "PDF_add_thumbnail" );

    pFunc( p, image );
}

void PDF_add_weblink (PDF *p, double llx, double lly, double urx, double ury, const char *url)
{
#if defined(__cplusplus)
    static PDF_ADD_WEBLINK
#else
    static PDF_ADD_WEBLINK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ADD_WEBLINK) PDFLib_GetProcAddress( "PDF_add_weblink" );

    pFunc( p, llx, lly, urx, ury, url );
}

void PDF_arc (PDF *p, double x, double y, double r, double alpha, double beta)
{
#if defined(__cplusplus)
    static PDF_ARC
#else
    static PDF_ARC pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ARC) PDFLib_GetProcAddress( "PDF_arc" );

    pFunc( p, x, y, r, alpha, beta);
}

void PDF_arcn (PDF *p, double x, double y, double r, double alpha, double beta)
{
#if defined(__cplusplus)
    static PDF_ARCN
#else
    static PDF_ARCN pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ARCN) PDFLib_GetProcAddress( "PDF_arcn" );

    pFunc( p, x, y, r, alpha, beta );
}

void PDF_attach_file (PDF *p, double llx, double lly, double urx, double ury, const char *filename, const char *description, const char *author, const char *mimetype, const char *icon)
{
#if defined(__cplusplus)
    static PDF_ATTACH_FILE
#else
    static PDF_ATTACH_FILE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ATTACH_FILE) PDFLib_GetProcAddress( "PDF_attach_file" );

    pFunc( p, llx, lly, urx, ury, filename, description, author, mimetype, icon );
}

void PDF_attach_file2 (PDF *p, double llx, double lly, double urx, double ury, const char *filename, int len_filename, const char *description, int len_descr, const char *author, int len_auth, const char *mimetype, const char *icon)
{
#if defined(__cplusplus)
    static PDF_ATTACH_FILE2
#else
    static PDF_ATTACH_FILE2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ATTACH_FILE2) PDFLib_GetProcAddress( "PDF_attach_file2" );

    pFunc( p, llx, lly, urx, ury, filename, len_filename, description, len_descr, author, len_auth, mimetype, icon);
}

int PDF_begin_document (PDF *p, const char *filename, int len, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_BEGIN_DOCUMENT
#else
    static PDF_BEGIN_DOCUMENT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_DOCUMENT) PDFLib_GetProcAddress( "PDF_begin_document" );

    return pFunc( p, filename, len, optlist );
}

void PDF_begin_document_callback (PDF *p, writeproc_t writeproc, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_BEGIN_DOCUMENT_CALLBACK
#else
    static PDF_BEGIN_DOCUMENT_CALLBACK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_DOCUMENT_CALLBACK) PDFLib_GetProcAddress( "PDF_begin_document_callback" );

    pFunc( p, writeproc, optlist );
}

void PDF_begin_font (PDF *p, const char *fontname, int len, double a, double b, double c, double d, double e, double f, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_BEGIN_FONT
#else
    static PDF_BEGIN_FONT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_FONT) PDFLib_GetProcAddress( "PDF_begin_font" );

    pFunc( p, fontname, len, a, b, c, d, e, f, optlist );
}

void PDF_begin_glyph (PDF *p, const char *glyphname, double wx, double llx, double lly, double urx, double ury)
{
#if defined(__cplusplus)
    static PDF_BEGIN_GLYPH
#else
    static PDF_BEGIN_GLYPH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_GLYPH) PDFLib_GetProcAddress( "PDF_open_file" );

    pFunc( p, glyphname, wx, llx, lly, urx, ury );
}

int PDF_begin_item (PDF *p, const char *tag, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_BEGIN_ITEM
#else
    static PDF_BEGIN_ITEM pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_ITEM) PDFLib_GetProcAddress( "PDF_begin_item" );

    return pFunc( p, tag, optlist );
}

void PDF_begin_layer (PDF *p, int layer)
{
#if defined(__cplusplus)
    static PDF_BEGIN_LAYER
#else
    static PDF_BEGIN_LAYER pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_LAYER) PDFLib_GetProcAddress( "PDF_begin_layer" );

    pFunc( p, layer );
}

void PDF_begin_mc (PDF *p, const char *tag, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_BEGIN_MC
#else
    static PDF_BEGIN_MC pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_MC) PDFLib_GetProcAddress( "PDF_begin_mc" );

    pFunc( p, tag, optlist);
}

void PDF_begin_page_ext (PDF *p, double width, double height, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_BEGIN_PAGE_EXT
#else
    static PDF_BEGIN_PAGE_EXT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_PAGE_EXT) PDFLib_GetProcAddress( "PDF_begin_page_ext" );

    pFunc( p, width, height, optlist );
}

int PDF_begin_pattern (PDF *p, double width, double height, double xstep, double ystep, int painttype)
{
#if defined(__cplusplus)
    static PDF_BEGIN_PATTERN
#else
    static PDF_BEGIN_PATTERN pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_PATTERN) PDFLib_GetProcAddress( "PDF_begin_pattern" );

    return pFunc( p, width, height, xstep, ystep, painttype  );
}

int PDF_begin_template (PDF *p, double width, double height)
{
#if defined(__cplusplus)
    static PDF_BEGIN_TEMPLATE
#else
    static PDF_BEGIN_TEMPLATE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BEGIN_TEMPLATE) PDFLib_GetProcAddress( "PDF_begin_template" );

    return pFunc( p, width, height);
}

void PDF_boot (void)
{
#if defined(__cplusplus)
    static PDF_BOOT
#else
    static PDF_BOOT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_BOOT) PDFLib_GetProcAddress( "PDF_boot" );

    pFunc();
}

void PDF_circle (PDF *p, double x, double y, double r)
{
#if defined(__cplusplus)
    static PDF_CIRCLE
#else
    static PDF_CIRCLE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CIRCLE) PDFLib_GetProcAddress( "PDF_circle" );

    pFunc( p, x, y, r );
}

void PDF_clip (PDF *p)
{
#if defined(__cplusplus)
    static PDF_CLIP
#else
    static PDF_CLIP pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CLIP) PDFLib_GetProcAddress( "PDF_clip" );

    pFunc( p );
}

void PDF_close_image (PDF *p, int image)
{
#if defined(__cplusplus)
    static PDF_CLOSE_IMAGE
#else
    static PDF_CLOSE_IMAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CLOSE_IMAGE) PDFLib_GetProcAddress( "PDF_close_image" );

    pFunc( p, image );
}

void PDF_close_pdi (PDF *p, int doc)
{
#if defined(__cplusplus)
    static PDF_CLOSE_PDI
#else
    static PDF_CLOSE_PDI pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CLOSE_PDI) PDFLib_GetProcAddress( "PDF_close_pdi" );

    pFunc( p, doc );
}

void PDF_close_pdi_page (PDF *p, int page)
{
#if defined(__cplusplus)
    static PDF_CLOSE_PDI_PAGE
#else
    static PDF_CLOSE_PDI_PAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CLOSE_PDI_PAGE) PDFLib_GetProcAddress( "PDF_close_pdi_page" );

    pFunc( p, page );
}

void PDF_closepath (PDF *p)
{
#if defined(__cplusplus)
    static PDF_CLOSEPATH
#else
    static PDF_CLOSEPATH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CLOSEPATH) PDFLib_GetProcAddress( "PDF_closepath" );

    pFunc( p );
}

void PDF_closepath_fill_stroke (PDF *p)
{
#if defined(__cplusplus)
    static PDF_CLOSEPATH_FILL_STROKE
#else
    static PDF_CLOSEPATH_FILL_STROKE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CLOSEPATH_FILL_STROKE) PDFLib_GetProcAddress( "PDF_closepath_fill_stroke" );

    pFunc( p );
}

void PDF_closepath_stroke (PDF *p)
{
#if defined(__cplusplus)
    static PDF_CLOSEPATH_STROKE
#else
    static PDF_CLOSEPATH_STROKE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CLOSEPATH_STROKE) PDFLib_GetProcAddress( "PDF_closepath_stroke" );

    pFunc( p );
}

void PDF_concat (PDF *p, double a, double b, double c, double d, double e, double f)
{
#if defined(__cplusplus)
    static PDF_CONCAT
#else
    static PDF_CONCAT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CONCAT) PDFLib_GetProcAddress( "PDF_concat" );

    pFunc( p, a, b, c, d, e, f );
}

void PDF_continue_text (PDF *p, const char *text)
{
#if defined(__cplusplus)
    static PDF_CONTINUE_TEXT
#else
    static PDF_CONTINUE_TEXT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CONTINUE_TEXT) PDFLib_GetProcAddress( "PDF_continue_text" );

    pFunc( p, text );
}

void PDF_continue_text2 (PDF *p, const char *text, int len)
{
#if defined(__cplusplus)
    static PDF_CONTINUE_TEXT2
#else
    static PDF_CONTINUE_TEXT2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CONTINUE_TEXT2) PDFLib_GetProcAddress( "PDF_continue_text2" );

    pFunc( p, text, len );
}

int PDF_create_action (PDF *p, const char *type, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_CREATE_ACTION
#else
    static PDF_CREATE_ACTION pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CREATE_ACTION) PDFLib_GetProcAddress( "PDF_create_action" );

    return pFunc( p, type, optlist );
}

void PDF_create_annotation (PDF *p, double llx, double lly, double urx, double ury, const char *type, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_CREATE_ANNOTATION
#else
    static PDF_CREATE_ANNOTATION pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CREATE_ANNOTATION) PDFLib_GetProcAddress( "PDF_create_annotation" );

    pFunc( p, llx, lly, urx, ury, type, optlist );
}

int PDF_create_bookmark (PDF *p, const char *text, int len, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_CREATE_BOOKMARK
#else
    static PDF_CREATE_BOOKMARK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CREATE_BOOKMARK) PDFLib_GetProcAddress( "PDF_create_bookmark" );

    return pFunc( p, text, len, optlist );
}

void PDF_create_field (PDF *p, double llx, double lly, double urx, double ury, const char *name, int len, const char *type, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_CREATE_FIELD
#else
    static PDF_CREATE_FIELD pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CREATE_FIELD) PDFLib_GetProcAddress( "PDF_create_field" );

    pFunc( p, llx, lly, urx, ury, name, len, type, optlist );
}

void PDF_create_fieldgroup (PDF *p, const char *name, int len, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_CREATE_FIELDGROUP
#else
    static PDF_CREATE_FIELDGROUP pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CREATE_FIELDGROUP) PDFLib_GetProcAddress( "PDF_create_fieldgroup" );

    pFunc( p, name, len, optlist );
}

int PDF_create_gstate (PDF *p, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_CREATE_GSTATE
#else
    static PDF_CREATE_GSTATE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CREATE_GSTATE) PDFLib_GetProcAddress( "PDF_create_gstate" );

    return pFunc( p, optlist );
}

void PDF_create_pvf (PDF *p, const char *filename, int len, const void *data, size_t size, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_CREATE_PVF
#else
    static PDF_CREATE_PVF pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CREATE_PVF) PDFLib_GetProcAddress( "PDF_create_pvf" );

    pFunc( p, filename, len, data, size, optlist );
}

int PDF_create_textflow (PDF *p, const char *text, int len, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_CREATE_TEXTFLOW
#else
    static PDF_CREATE_TEXTFLOW pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CREATE_TEXTFLOW) PDFLib_GetProcAddress( "PDF_create_textflow" );

    return pFunc( p, text, len, optlist );
}

void PDF_curveto (PDF *p, double x_1, double y_1, double x_2, double y_2, double x_3, double y_3)
{
#if defined(__cplusplus)
    static PDF_CURVETO
#else
    static PDF_CURVETO pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CURVETO) PDFLib_GetProcAddress( "PDF_curveto" );

    pFunc( p, x_1, y_1, x_2, y_2, x_3, y_3 );
}

int PDF_define_layer (PDF *p, const char *name, int len, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_DEFINE_LAYER
#else
    static PDF_DEFINE_LAYER pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_DEFINE_LAYER) PDFLib_GetProcAddress( "PDF_define_layer" );

    return pFunc( p, name, len, optlist );
}

int PDF_delete_pvf (PDF *p, const char *filename, int len)
{
#if defined(__cplusplus)
    static PDF_DELETE_PVF
#else
    static PDF_DELETE_PVF pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_DELETE_PVF) PDFLib_GetProcAddress( "PDF_delete_pvf" );

    return pFunc( p, filename, len );
}

void PDF_delete_textflow (PDF *p, int textflow)
{
#if defined(__cplusplus)
    static PDF_DELETE_TEXTFLOW
#else
    static PDF_DELETE_TEXTFLOW pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_DELETE_TEXTFLOW) PDFLib_GetProcAddress( "PDF_delete_textflow" );

    pFunc( p, textflow );
}

void PDF_encoding_set_char (PDF *p, const char *encoding, int slot, const char *glyphname, int uv)
{
#if defined(__cplusplus)
    static PDF_ENCODING_SET_CHAR
#else
    static PDF_ENCODING_SET_CHAR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ENCODING_SET_CHAR) PDFLib_GetProcAddress( "PDF_encoding_set_char" );

    pFunc( p, encoding, slot, glyphname, uv );
}

void PDF_end_document (PDF *p, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_END_DOCUMENT
#else
    static PDF_END_DOCUMENT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_DOCUMENT) PDFLib_GetProcAddress( "PDF_end_document" );

    pFunc( p, optlist );
}

void PDF_end_font (PDF *p)
{
#if defined(__cplusplus)
    static PDF_END_FONT
#else
    static PDF_END_FONT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_FONT) PDFLib_GetProcAddress( "PDF_end_font" );

    pFunc( p );
}

void PDF_end_glyph (PDF *p)
{
#if defined(__cplusplus)
    static PDF_END_GLYPH
#else
    static PDF_END_GLYPH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_GLYPH) PDFLib_GetProcAddress( "PDF_end_glyph" );

    pFunc( p );
}

void PDF_end_item (PDF *p, int id)
{
#if defined(__cplusplus)
    static PDF_END_ITEM
#else
    static PDF_END_ITEM pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_ITEM) PDFLib_GetProcAddress( "PDF_end_item" );

    pFunc( p, id );
}

void PDF_end_layer (PDF *p)
{
#if defined(__cplusplus)
    static PDF_END_LAYER
#else
    static PDF_END_LAYER pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_LAYER) PDFLib_GetProcAddress( "PDF_end_layer" );

    pFunc( p );
}

void PDF_end_mc (PDF *p)
{
#if defined(__cplusplus)
    static PDF_END_MC
#else
    static PDF_END_MC pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_MC) PDFLib_GetProcAddress( "PDF_end_mc" );

    pFunc( p );
}

void PDF_end_page_ext (PDF *p, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_END_PAGE_EXT
#else
    static PDF_END_PAGE_EXT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_PAGE_EXT) PDFLib_GetProcAddress( "PDF_end_page_ext" );

    pFunc( p, optlist );
}

void PDF_end_pattern (PDF *p)
{
#if defined(__cplusplus)
    static PDF_END_PATTERN
#else
    static PDF_END_PATTERN pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_PATTERN) PDFLib_GetProcAddress( "PDF_end_pattern" );

    pFunc( p );
}

void PDF_end_template (PDF *p)
{
#if defined(__cplusplus)
    static PDF_END_TEMPLATE
#else
    static PDF_END_TEMPLATE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_END_TEMPLATE) PDFLib_GetProcAddress( "PDF_end_template" );

    pFunc( p );
}

void PDF_endpath (PDF *p)
{
#if defined(__cplusplus)
    static PDF_ENDPATH
#else
    static PDF_ENDPATH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ENDPATH) PDFLib_GetProcAddress( "PDF_endpath" );

    pFunc( p );
}

void PDF_fill (PDF *p)
{
#if defined(__cplusplus)
    static PDF_FILL
#else
    static PDF_FILL pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_FILL) PDFLib_GetProcAddress( "PDF_fill" );

    pFunc( p );
}

int PDF_fill_imageblock (PDF *p, int page, const char *blockname, int image, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_FILL_IMAGEBLOCK
#else
    static PDF_FILL_IMAGEBLOCK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_FILL_IMAGEBLOCK) PDFLib_GetProcAddress( "PDF_fill_imageblock" );

    return pFunc( p, page, blockname, image, optlist );
}

int PDF_fill_pdfblock (PDF *p, int page, const char *blockname, int contents, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_FILL_PDFBLOCK
#else
    static PDF_FILL_PDFBLOCK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_FILL_PDFBLOCK) PDFLib_GetProcAddress( "PDF_fill_pdfblock" );

    return pFunc( p, page, blockname, contents, optlist );
}

void PDF_fill_stroke (PDF *p)
{
#if defined(__cplusplus)
    static PDF_FILL_STROKE
#else
    static PDF_FILL_STROKE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_FILL_STROKE) PDFLib_GetProcAddress( "PDF_fill_stroke" );

    pFunc( p );
}

int PDF_open_image ( PDF *p, const char *imagetype, const char *source, const char *data, long length, int width, int height, int components, int bpc, const char *params)
{
#if defined(__cplusplus)
    static PDF_OPEN_IMAGE
#else
    static PDF_OPEN_IMAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_OPEN_IMAGE) PDFLib_GetProcAddress( "PDF_open_image" );

    return pFunc( p, imagetype, source, data, length, width, height, components, bpc, params);
}

int PDF_open_image_file (PDF *p, const char *imagetype, const char *filename, const char *stringparam, int intparam)
{
#if defined(__cplusplus)
    static PDF_OPEN_IMAGE_FILE
#else
    static PDF_OPEN_IMAGE_FILE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_OPEN_IMAGE_FILE) PDFLib_GetProcAddress( "PDF_open_image_file" );

    return pFunc( p, imagetype, filename, stringparam, intparam);
}

void PDF_open_mem (PDF *p, writeproc_t writeproc)
{
#if defined(__cplusplus)
    static PDF_OPEN_MEM
#else
    static PDF_OPEN_MEM pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_OPEN_MEM) PDFLib_GetProcAddress( "PDF_open_mem" );

    pFunc( p, writeproc);
}

int PDF_open_pdi (PDF *p, const char *filename, const char *optlist, int len)
{
#if defined(__cplusplus)
    static PDF_OPEN_PDI
#else
    static PDF_OPEN_PDI pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_OPEN_PDI) PDFLib_GetProcAddress( "PDF_open_pdi" );

    return pFunc( p, filename, optlist, len);
}

#if 0
int PDF_open_pdi_callback (PDF *p, void *opaque, size_t filesize, size_t *readproc, int *seekproc, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_OPEN_PDI_CALLBACK
#else
    static PDF_OPEN_PDI_CALLBACK pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_OPEN_PDI_CALLBACK) PDFLib_GetProcAddress( "PDF_open_pdi_callback" );

    return pFunc( p, opaque, filesize, readproc, seekproc, optlist );
}
#endif

int PDF_open_pdi_page (PDF *p, int doc, int pagenumber, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_OPEN_PDI_PAGE
#else
    static PDF_OPEN_PDI_PAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_OPEN_PDI_PAGE) PDFLib_GetProcAddress( "PDF_open_pdi_page" );

    return pFunc( p, doc, pagenumber, optlist);
}

void PDF_place_image (PDF *p, int image, double x, double y, double scale)
{
#if defined(__cplusplus)
    static PDF_PLACE_IMAGE
#else
    static PDF_PLACE_IMAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_PLACE_IMAGE) PDFLib_GetProcAddress( "PDF_place_image" );

    pFunc( p, image, x, y, scale );
}

void PDF_place_pdi_page (PDF *p, int page, double x, double y, double sx, double sy)
{
#if defined(__cplusplus)
    static PDF_PLACE_PDI_PAGE
#else
    static PDF_PLACE_PDI_PAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_PLACE_PDI_PAGE) PDFLib_GetProcAddress( "PDF_place_pdi_page" );

    pFunc( p, page, x, y, sx, sy );
}

int PDF_process_pdi (PDF *p, int doc, int page, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_PROCESS_PDI
#else
    static PDF_PROCESS_PDI pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_PROCESS_PDI) PDFLib_GetProcAddress( "PDF_process_pdi" );

    return pFunc( p, doc, page, optlist );
}

void PDF_restore (PDF *p)
{
#if defined(__cplusplus)
    static PDF_RESTORE
#else
    static PDF_RESTORE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_RESTORE) PDFLib_GetProcAddress( "PDF_restore" );

    pFunc( p );
}

void PDF_resume_page (PDF *p, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_RESUME_PAGE
#else
    static PDF_RESUME_PAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_RESUME_PAGE) PDFLib_GetProcAddress( "PDF_resume_page" );

    pFunc( p, optlist );
}

void PDF_rotate (PDF *p, double phi)
{
#if defined(__cplusplus)
    static PDF_ROTATE
#else
    static PDF_ROTATE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_ROTATE) PDFLib_GetProcAddress( "PDF_rotate" );

    pFunc( p, phi );
}

void PDF_save (PDF *p)
{
#if defined(__cplusplus)
    static PDF_SAVE
#else
    static PDF_SAVE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SAVE) PDFLib_GetProcAddress( "PDF_save" );

    pFunc( p );
}

void PDF_scale (PDF *p, double sx, double sy)
{
#if defined(__cplusplus)
    static PDF_SCALE
#else
    static PDF_SCALE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SCALE) PDFLib_GetProcAddress( "PDF_scale" );

    pFunc(p, sx, sy );
}

void PDF_set_border_dash (PDF *p, double b, double w)
{
#if defined(__cplusplus)
    static PDF_SET_BORDER_DASH
#else
    static PDF_SET_BORDER_DASH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_BORDER_DASH) PDFLib_GetProcAddress( "PDF_set_border_dash" );

    pFunc( p, b, w );
}

void PDF_set_border_style (PDF *p, const char *style, double width)
{
#if defined(__cplusplus)
    static PDF_SET_BORDER_STYLE
#else
    static PDF_SET_BORDER_STYLE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_BORDER_STYLE) PDFLib_GetProcAddress( "PDF_set_border_style" );

    pFunc( p, style, width );
}

void PDF_set_gstate (PDF *p, int gstate)
{
#if defined(__cplusplus)
    static PDF_SET_GSTATE
#else
    static PDF_SET_GSTATE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_GSTATE) PDFLib_GetProcAddress( "PDF_set_gstate" );

    pFunc( p, gstate );
}

void PDF_set_info2 (PDF *p, const char *key, const char *value, int len)
{
#if defined(__cplusplus)
    static PDF_SET_INFO2
#else
    static PDF_SET_INFO2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_INFO2) PDFLib_GetProcAddress( "PDF_set_info2" );

    pFunc( p, key, value, len );
}

void PDF_set_layer_dependency (PDF *p, const char *type, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_SET_LAYER_DEPENDENCY
#else
    static PDF_SET_LAYER_DEPENDENCY pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_LAYER_DEPENDENCY) PDFLib_GetProcAddress( "PDF_set_layer_dependency" );

    pFunc( p, type, optlist );
}

void PDF_set_text_pos (PDF *p, double x, double y)
{
#if defined(__cplusplus)
    static PDF_SET_TEXT_POS
#else
    static PDF_SET_TEXT_POS pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SET_TEXT_POS) PDFLib_GetProcAddress( "PDF_set_text_pos" );

    pFunc( p, x, y );
}

void PDF_setcolor (PDF *p, const char *fstype, const char *colorspace, double c1, double c2, double c3, double c4)
{
#if defined(__cplusplus)
    static PDF_SETCOLOR
#else
    static PDF_SETCOLOR pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETCOLOR) PDFLib_GetProcAddress( "PDF_setcolor" );

    pFunc( p, fstype, colorspace, c1, c2, c3, c4 );
}

void PDF_setdash (PDF *p, double b, double w)
{
#if defined(__cplusplus)
    static PDF_SETDASH
#else
    static PDF_SETDASH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETDASH) PDFLib_GetProcAddress( "PDF_setdash" );

    pFunc( p, b, w );
}

void PDF_setdashpattern (PDF *p, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_SETDASHPATTERN
#else
    static PDF_SETDASHPATTERN pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETDASHPATTERN) PDFLib_GetProcAddress( "PDF_setdashpattern" );

    pFunc( p, optlist );
}

void PDF_setflat (PDF *p, double flatness)
{
#if defined(__cplusplus)
    static PDF_SETFLAT
#else
    static PDF_SETFLAT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETFLAT) PDFLib_GetProcAddress( "PDF_setflat" );

    pFunc( p, flatness );
}

void PDF_setgray (PDF *p, double gray)
{
#if defined(__cplusplus)
    static PDF_SETGRAY
#else
    static PDF_SETGRAY pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETGRAY) PDFLib_GetProcAddress( "PDF_setgray" );

    pFunc( p, gray );
}

void PDF_setgray_stroke (PDF *p, double gray)
{
#if defined(__cplusplus)
    static PDF_SETGRAY_STROKE
#else
    static PDF_SETGRAY_STROKE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETGRAY_STROKE) PDFLib_GetProcAddress( "PDF_setgray_stroke" );

    pFunc( p, gray );
}

void PDF_setgray_fill (PDF *p, double gray)
{
#if defined(__cplusplus)
    static PDF_SETGRAY_FILL
#else
    static PDF_SETGRAY_FILL pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETGRAY_FILL) PDFLib_GetProcAddress( "PDF_setgray_fill" );

    pFunc( p, gray );
}

void PDF_setlinecap (PDF *p, int linecap)
{
#if defined(__cplusplus)
    static PDF_SETLINECAP
#else
    static PDF_SETLINECAP pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETLINECAP) PDFLib_GetProcAddress( "PDF_setlinecap" );

    pFunc( p, linecap );
}

void PDF_setlinejoin (PDF *p, int linejoin)
{
#if defined(__cplusplus)
    static PDF_SETLINEJOIN
#else
    static PDF_SETLINEJOIN pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETLINEJOIN) PDFLib_GetProcAddress( "PDF_setlinejoin" );

    pFunc( p, linejoin );
}

void PDF_setlinewidth (PDF *p, double width)
{
#if defined(__cplusplus)
    static PDF_SETLINEWIDTH
#else
    static PDF_SETLINEWIDTH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETLINEWIDTH) PDFLib_GetProcAddress( "PDF_setlinewidth" );

    pFunc( p, width );
}

void PDF_setmatrix (PDF *p, double a, double b, double c, double d, double e, double f)
{
#if defined(__cplusplus)
    static PDF_SETMATRIX
#else
    static PDF_SETMATRIX pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETMATRIX) PDFLib_GetProcAddress( "PDF_setmatrix" );

    pFunc( p, a, b, c, d, e, f );
}

void PDF_setmiterlimit (PDF *p, double miter)
{
#if defined(__cplusplus)
    static PDF_SETMITERLIMIT
#else
    static PDF_SETMITERLIMIT pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETMITERLIMIT) PDFLib_GetProcAddress( "PDF_setmiterlimit" );

    pFunc( p, miter );
}

void PDF_setpolydash (PDF *p, float *dasharray, int length)
{
#if defined(__cplusplus)
    static PDF_SETPOLYDASH
#else
    static PDF_SETPOLYDASH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETPOLYDASH) PDFLib_GetProcAddress( "PDF_setpolydash" );

    pFunc( p, dasharray, length);
}

void PDF_setrgbcolor_fill (PDF *p, double red, double green, double blue)
{
#if defined(__cplusplus)
    static PDF_SETRGBCOLOR_FILL
#else
    static PDF_SETRGBCOLOR_FILL pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETRGBCOLOR_FILL) PDFLib_GetProcAddress( "PDF_setrgbcolor_fill" );

    pFunc( p, red, green, blue );
}

void PDF_setrgbcolor_stroke (PDF *p, double red, double green, double blue)
{
#if defined(__cplusplus)
    static PDF_SETRGBCOLOR_STROKE
#else
    static PDF_SETRGBCOLOR_STROKE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SETRGBCOLOR_STROKE) PDFLib_GetProcAddress( "PDF_setrgbcolor_stroke" );

    pFunc( p, red, green, blue );
}

int PDF_shading (PDF *p, const char *shtype, double x_0, double y_0, double x_1, double y_1, double c_1, double c_2, double c_3, double c_4, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_SHADING
#else
    static PDF_SHADING pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHADING) PDFLib_GetProcAddress( "PDF_shading" );

    return pFunc( p, shtype, x_0, y_0, x_1, y_1, c_1, c_2, c_3, c_4, optlist);
}

int PDF_shading_pattern (PDF *p, int shading, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_SHADING_PATTERN
#else
    static PDF_SHADING_PATTERN pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHADING_PATTERN) PDFLib_GetProcAddress( "PDF_shading_pattern" );

    return pFunc( p, shading, optlist);
}

void PDF_shfill (PDF *p, int shading)
{
#if defined(__cplusplus)
    static PDF_SHFILL
#else
    static PDF_SHFILL pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHFILL) PDFLib_GetProcAddress( "PDF_shfill" );

    pFunc( p, shading );
}

void PDF_show2 (PDF *p, const char *text, int len)
{
#if defined(__cplusplus)
    static PDF_SHOW2
#else
    static PDF_SHOW2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHOW2) PDFLib_GetProcAddress( "PDF_show2" );

    pFunc( p, text, len);
}

int PDF_show_boxed2 (PDF *p, const char *text, int len, double left, double top, double width, double height, const char *hmode, const char *feature)
{
#if defined(__cplusplus)
    static PDF_SHOW_BOXED2
#else
    static PDF_SHOW_BOXED2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHOW_BOXED2) PDFLib_GetProcAddress( "PDF_show_boxed2" );

    return pFunc( p, text, len, left, top, width, height, hmode, feature );
}

void PDF_show_xy2 (PDF *p, const char *text, int len, double x, double y)
{
#if defined(__cplusplus)
    static PDF_SHOW_XY2
#else
    static PDF_SHOW_XY2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHOW_XY2) PDFLib_GetProcAddress( "PDF_show_xy2" );

    pFunc( p, text, len, x, y );
}

void PDF_shutdown (void)
{
#if defined(__cplusplus)
    static PDF_SHUTDOWN
#else
    static PDF_SHUTDOWN pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SHUTDOWN) PDFLib_GetProcAddress( "PDF_shutdown" );

    pFunc();
}

void PDF_skew (PDF *p, double alpha, double beta)
{
#if defined(__cplusplus)
    static PDF_SKEW
#else
    static PDF_SKEW pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SKEW) PDFLib_GetProcAddress( "PDF_skew" );

    pFunc( p, alpha, beta );
}

double PDF_stringwidth2 (PDF *p, const char *text, int len, int font, double fontsize)
{
#if defined(__cplusplus)
    static PDF_STRINGWIDTH2
#else
    static PDF_STRINGWIDTH2 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_STRINGWIDTH2) PDFLib_GetProcAddress( "PDF_stringwidth2" );

    return pFunc( p, text, len, font, fontsize);
}

void PDF_suspend_page(PDF *p, const char *optlist)
{
#if defined(__cplusplus)
    static PDF_SUSPEND_PAGE
#else
    static PDF_SUSPEND_PAGE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_SUSPEND_PAGE) PDFLib_GetProcAddress( "PDF_suspend_page" );

    pFunc( p, optlist);
}

void PDF_translate(PDF *p, double tx, double ty)
{
#if defined(__cplusplus)
    static PDF_TRANSLATE
#else
    static PDF_TRANSLATE pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_TRANSLATE) PDFLib_GetProcAddress( "PDF_translate" );

    pFunc( p, tx, ty );
}

const char * PDF_utf16_to_utf8 (PDF *p, const char *utf16string, int len, int *size)
{
#if defined(__cplusplus)
    static PDF_UTF16_TO_UTF8
#else
    static PDF_UTF16_TO_UTF8 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_UTF16_TO_UTF8) PDFLib_GetProcAddress( "PDF_utf16_to_utf8" );

    return pFunc( p, utf16string, len, size );
}

const char * PDF_utf8_to_utf16 (PDF *p, const char *utf8string, const char *format, int *size)
{
#if defined(__cplusplus)
    static PDF_UTF8_TO_UTF16
#else
    static PDF_UTF8_TO_UTF16 pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_UTF8_TO_UTF16) PDFLib_GetProcAddress( "PDF_utf8_to_utf16" );

    return pFunc( p, utf8string, format, size );
}

int PDF_get_majorversion(void)
{
#if defined(__cplusplus)
    static PDF_GET_MAJORVERSION
#else
    static PDF_GET_MAJORVERSION pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_GET_MAJORVERSION) PDFLib_GetProcAddress( "PDF_get_majorversion" );

    return pFunc();
}

int PDF_get_minorversion(void)
{
#if defined(__cplusplus)
    static PDF_GET_MINORVERSION
#else
    static PDF_GET_MINORVERSION pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_GET_MINORVERSION) PDFLib_GetProcAddress( "PDF_get_minorversion" );

    return pFunc();
}

void PDF_xshow(PDF *p, const char *text, int len, const double *xadvancelist)
{
#if defined(__cplusplus)
    static PDF_XSHOW
#else
    static PDF_XSHOW pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_XSHOW) PDFLib_GetProcAddress( "PDF_xshow" );

    pFunc( p, text, len, xadvancelist );
}

int pdf_catch(PDF *p)
{
#if defined(__cplusplus)
    static PDF_CATCH
#else
    static PDF_CATCH pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_CATCH) PDFLib_GetProcAddress( "pdf_catch" );

    return pFunc( p );
}

void pdf_exit_try(PDF *p)
{
#if defined(__cplusplus)
    static PDF_EXIT_TRY
#else
    static PDF_EXIT_TRY pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_EXIT_TRY) PDFLib_GetProcAddress( "pdf_exit_try" );

    pFunc( p );
}

pdf_jmpbuf * pdf_jbuf(PDF *p)
{
#if defined(__cplusplus)
    static PDF_JBUF
#else
    static PDF_JBUF pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_JBUF) PDFLib_GetProcAddress( "pdf_jbuf" );

    return pFunc( p );
}

void pdf_rethrow(PDF *p)
{
#if defined(__cplusplus)
    static PDF_RETHROW
#else
    static PDF_RETHROW pFunc = NULL;
    if ( !pFunc )
#endif
       pFunc = (PDF_RETHROW) PDFLib_GetProcAddress( "pdf_rethrow" );

    pFunc( p );
}

static void hb_pdflibInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if ( !hModule )
   {
      hModule = LoadLibrary( PDFLIBDLL_NAME );

      if ( !hModule )
      {
         char __szError[256];
         hb_snprintf( __szError, sizeof( __szError ), "Cannot load %s", PDFLIBDLL_NAME );
         hb_errInternal( 5178, __szError, NULL, NULL );
      }
   }
}

static void hb_pdflibExit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hModule )
      FreeLibrary( hModule );
}


#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

HB_CALL_ON_STARTUP_BEGIN( _hb_pdflib_init_ )
   hb_vmAtInit( hb_pdflibInit, NULL );
   hb_vmAtExit( hb_pdflibExit, NULL );
HB_CALL_ON_STARTUP_END( _hb_pdflib_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_pdflib_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( _hb_pdflib_init_ )
   #include "hbiniseg.h"
#endif

