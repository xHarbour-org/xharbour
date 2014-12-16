/*---------------------------------------------------------------------------*
 |              PDFlib - A library for generating PDF on the fly             |
 +---------------------------------------------------------------------------+
 | Copyright (c) 1997-2006 Thomas Merz and PDFlib GmbH. All rights reserved. |
 +---------------------------------------------------------------------------+
 |                                                                           |
 |    This software is subject to the PDFlib license. It is NOT in the       |
 |    public domain. Extended versions and commercial licenses are           |
 |    available, please check http://www.pdflib.com.                         |
 |                                                                           |
 *---------------------------------------------------------------------------*/

/* $Id$
 *
 * PDFlib PDF object functions
 *
 */

#define P_OBJECT_C

#include "config.h"
#include "p_intern.h"
#include "p_image.h"



static const pdc_keyconn pdf_scope_keylist[] =
{
    {"object",       pdf_state_object},
    {"document",     pdf_state_document},
    {"page",         pdf_state_page},
    {"pattern",      pdf_state_pattern},
    {"template",     pdf_state_template},
    {"path",         pdf_state_path},
    {"font",         pdf_state_font},
    {"glyph",        pdf_state_glyph},
    {"glyphmetrics", pdf_state_glyphmetrics},
    {"glyphignore",  pdf_state_glyphignore},
    {"error",        pdf_state_error},
    {NULL, 0}
};

static pdc_error_info   pdf_errors[] =
{
#define         pdf_genInfo     1
#include        "p_generr.h"
};

#define N_PDF_ERRORS    (sizeof pdf_errors / sizeof (pdc_error_info))

const char *
pdf_current_scope(PDF *p)
{
    const char *scopename =
        pdc_get_keyword(PDF_GET_STATE(p), pdf_scope_keylist);

    if (!scopename)
        pdc_error(p->pdc, PDF_E_INT_BADSCOPE,
            pdc_errprintf(p->pdc, " (0x%08X)", PDF_GET_STATE(p)), 0, 0, 0);

    return (char *) scopename;	/* be happy, compiler! */
}

/* p may be NULL on the first call - we don't use it anyway */
static void *
default_malloc(PDF *p, size_t size, const char *caller)
{
    void *ret = malloc(size);

    (void) p;
    (void) caller;

    return ret;
}

static void *
default_realloc(PDF *p, void *mem, size_t size, const char *caller)
{
    void *ret = realloc(mem, size);

    (void) p;
    (void) caller;

    return ret;
}

static void
default_free(PDF *p, void *mem)
{
    (void) p;

    free(mem);
}

PDF *
pdf__new(
    void  (*errorhandler)(PDF *p, int type, const char *msg),
    void* (*allocproc)(PDF *p, size_t size, const char *caller),
    void* (*reallocproc)(PDF *p, void *mem, size_t size, const char *caller),
    void  (*freeproc)(PDF *p, void *mem),
    void   *opaque)
{
    PDF *	p;
    pdc_core *	pdc;

    /* If allocproc is NULL, all entries are supplied internally by PDFlib */
    if (allocproc == NULL) {
	allocproc	= default_malloc;
	reallocproc	= default_realloc;
	freeproc	= default_free;
    }

    p = (PDF *) (*allocproc) (NULL, sizeof(PDF), "PDF_new");

    if (p == NULL)
	return NULL;

    /*
     * Guard against crashes when PDF_delete is called without any
     * PDF_open_*() in between.
     */
    memset((void *)p, 0, (size_t) sizeof(PDF));

    /* these two are required by PDF_get_opaque() */
    p->magic = PDC_MAGIC;
    p->opaque = opaque;

    pdc = pdc_new_core(
	(pdc_error_fp) errorhandler,
	(pdc_alloc_fp) allocproc,
	(pdc_realloc_fp) reallocproc,
	(pdc_free_fp) freeproc, p,
        PDFLIB_PRODUCTNAME,
        PDFLIB_VERSIONSTRING);

    if (pdc == NULL)
    {
	(*freeproc)(p, p);
	return NULL;
    }

    pdc_register_errtab(pdc, PDC_ET_PDFLIB, pdf_errors, N_PDF_ERRORS);
    fnt_register_errtab(pdc);

    PDC_TRY(pdc)
    {
        p->freeproc	= freeproc;
        p->pdc		= pdc;
        p->compatibility    = PDF_DEF_COMPATIBILITY;
        p->errorpolicy      = errpol_legacy;

        p->userinfo         = NULL;
        p->document         = NULL;

        p->errorhandler	= errorhandler;

        p->flush	    = pdc_flush_page;

        p->hypertextencoding= pdc_invalidenc;
        p->hypertextformat  = pdc_auto;
        p->hypertextcodepage= 0;
        p->usercoordinates  = pdc_false;
        p->usehyptxtenc     = pdc_false;

        p->currfo           = NULL;
        p->curr_ppt         = NULL;

        p->glyphcheck = text_nocheck;
        p->textformat       = pdc_auto;
        p->in_text	    = pdc_false;


        p->rendintent       = AutoIntent;
        p->preserveoldpantonenames = pdc_false;
        p->spotcolorlookup  = pdc_true;
        p->ydirection       = 1;
        p->names	    = NULL;
        p->names_capacity   = 0;
        p->xobjects     = NULL;
        p->state_sp	= 0;
        p->doc_pages    = NULL;

        p->actions = NULL;





        PDF_SET_STATE(p, pdf_state_object);

        /* all debug flags are cleared by default
         * because of the above memset... */

        /* ...but warning messages for non-fatal errors should be set,
         * as well as font warnings -- the client must explicitly disable these.
         */
        p->debug[(int) 'e'] = pdc_true;
        p->debug[(int) 'F'] = pdc_true;
        p->debug[(int) 'I'] = pdc_true;

        pdf_init_stringlists(p);
        pdf_init_font_options(p, NULL);

        p->out = pdc_boot_output(p->pdc);


    }
    PDC_CATCH(pdc)
    {
        pdc_delete_core(pdc);
        return (PDF *) 0;
    }
    return p;
} /* pdf__new */


/*
 * PDF_delete must be called for cleanup in case of error,
 * or when the client is done producing PDF.
 * It should never be called more than once for a given PDF, although
 * we try to guard against duplicated calls.
 *
 * Note: all pdf_cleanup_*() functions may safely be called multiple times.
 */

void
pdf__delete(PDF *p)
{
    /*
     * Close the output stream, because it could be open
     */
    pdc_close_output(p->out);

    /*
     * Clean up page-related stuff if necessary. Do not raise
     * an error here since we may be called from the error handler.
     */
    pdf_cleanup_document(p);
    pdf_cleanup_stringlists(p);
    pdf_cleanup_font_curroptions(p);
    pdc_cleanup_output(p->out, pdc_false);




    if (p->out)
    {
	pdc_free(p->pdc, p->out);
        p->out = NULL;
    }

    /* we never reach this point if (p->pdc == NULL).
    */
    pdc_delete_core(p->pdc);

    /* free the PDF structure and try to protect against duplicated calls */

    p->magic = 0L;		/* we don't reach this with the wrong magic */
    (*p->freeproc)(p, (void *) p);
}

