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
 * PDFlib document related routines
 *
 */

/* Pacify MSVS2005 and above */
#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #define _CRT_SECURE_NO_WARNINGS
   #define _CRT_SECURE_NO_DEPRECATE
#endif

#define P_DOCUMENT_C

/* For checking the beta expiration date */
#include <time.h>

#include "config.h"
#include "p_intern.h"
#include "p_image.h"
#include "p_page.h"


#if (defined(WIN32) || defined(OS2)) && !defined(WINCE)
#include <fcntl.h>
#include <io.h>
#endif


/* file attachment structure */
typedef struct
{
    char *filename;
    char *name;
    char *description;
    char *mimetype;
    pdc_off_t filesize;
}
pdf_attachments;

#define PDF_MAX_LANGCODE  8

/* Document open modes */

typedef enum
{
    open_auto,
    open_none,
    open_bookmarks,
    open_thumbnails,
    open_fullscreen,
    open_attachments,
    open_layers
}
pdf_openmode;

static const pdc_keyconn pdf_openmode_keylist[] =
{
    {"none",        open_none},
    {"bookmarks",   open_bookmarks},
    {"thumbnails",  open_thumbnails},
    {"fullscreen",  open_fullscreen},
    {"attachments", open_attachments},
    {"layers",      open_layers},
    {NULL, 0}
};

static const pdc_keyconn pdf_openmode_pdfkeylist[] =
{
    {"UseNone",        open_auto},
    {"UseNone",        open_none},
    {"UseOutlines",    open_bookmarks},
    {"UseThumbs",      open_thumbnails},
    {"FullScreen",     open_fullscreen},
    {"UseAttachments", open_attachments},
    {"UseOC",          open_layers},
    {NULL, 0}
};


/* Document page layout */

typedef enum
{
    layout_default,
    layout_singlepage,
    layout_onecolumn,
    layout_twocolumnleft,
    layout_twocolumnright,
    layout_twopageleft,
    layout_twopageright
}
pdf_pagelayout;

static const pdc_keyconn pdf_pagelayout_pdfkeylist[] =
{
    {"Default",        layout_default},
    {"SinglePage",     layout_singlepage},
    {"OneColumn",      layout_onecolumn},
    {"TwoColumnLeft",  layout_twocolumnleft},
    {"TwoColumnRight", layout_twocolumnright},
    {"TwoPageLeft",    layout_twopageleft},
    {"TwoPageRight",   layout_twopageright},
    {NULL, 0}
};


/* NonFullScreenPageMode */

static const pdc_keyconn pdf_nonfullscreen_keylist[] =
{
    {"none",        open_none},
    {"bookmarks",   open_bookmarks},
    {"thumbnails",  open_thumbnails},
    {"layers",      open_layers},
    {NULL, 0}
};

typedef enum
{
    doc_none,
    doc_l2r,
    doc_r2l,
    doc_appdefault,
    doc_simplex,
    doc_duplexflipshortedge,
    doc_duplexfliplongedge
}
pdf_viewerprefence;

/* Direction */

static const pdc_keyconn pdf_textdirection_pdfkeylist[] =
{
    {"L2R",   doc_l2r},
    {"R2L",   doc_r2l},
    {NULL, 0}
};

/* PrintScaling */

static const pdc_keyconn pdf_printscaling_pdfkeylist[] =
{
    {"None",        doc_none},
    {"AppDefault",  doc_appdefault},
    {NULL, 0}
};

/* Duplex */

static const pdc_keyconn pdf_duplex_pdfkeylist[] =
{
    {"None",                doc_none},
    {"Simplex",             doc_simplex},
    {"DuplexFlipShortEdge", doc_duplexflipshortedge},
    {"DuplexFlipLongEdge",  doc_duplexfliplongedge},
    {NULL, 0}
};



static const pdc_keyconn pdf_pdfa_keylist[] =
{
    {NULL, 0}
};



static const pdc_keyconn pdf_pdfx_keylist[] =
{
    {NULL, 0}
};


/* configurable flush points */

static const pdc_keyconn pdf_flush_keylist[] =
{
    {"none",    pdc_flush_none},
    {"page",    pdc_flush_page},
    {"content", pdc_flush_content},
    {"heavy",   pdc_flush_heavy},
    {NULL, 0}
};

static const pdc_keyconn pl_pwencoding_keylist[] =
{
    {"ebcdic",          pdc_ebcdic},
    {"ebcdic_37",       pdc_ebcdic_37},
    {"ebcdic_winansi",  pdc_ebcdic_winansi},
    {"pdfdoc",          pdc_pdfdoc},
    {"winansi",         pdc_winansi},
    {"macroman",        pdc_macroman_apple},
    {NULL, 0}
};

#define PDF_MAXPW 0
static const pdc_keyconn pdc_permissions_keylist[] =
{
    {NULL, 0}
};

#define PDF_PDFA_FLAG  PDC_OPT_UNSUPP

#define PDF_SECURITY_FLAG  PDC_OPT_UNSUPP

#define PDF_LINEARIZE_FLAG  PDC_OPT_UNSUPP

#define PDF_ICC_FLAG  PDC_OPT_UNSUPP

#define PDF_TAGGED_FLAG  PDC_OPT_UNSUPP

#define PDF_METADATA_FLAG  PDC_OPT_UNSUPP

#define PDF_UPDATE_FLAG  PDC_OPT_UNSUPP

#define PDF_DOCUMENT_OPTIONS1 \
\
    {"pdfa", pdc_keywordlist, PDF_PDFA_FLAG, 1, 1, \
      0.0, 0.0, pdf_pdfa_keylist}, \
\
    {"pdfx", pdc_keywordlist, PDF_ICC_FLAG, 1, 1, \
      0.0, 0.0, pdf_pdfx_keylist}, \
\
    {"compatibility", pdc_keywordlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, pdf_compatibility_keylist}, \
\
    {"flush", pdc_keywordlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, pdf_flush_keylist}, \
\
    {"passwordencoding", pdc_keywordlist, PDF_SECURITY_FLAG, 1, 1, \
      0.0, 0.0, pl_pwencoding_keylist}, \
\
    {"attachmentpassword", pdc_stringlist,  PDF_SECURITY_FLAG, 1, 1, \
      0.0, PDF_MAXPW, NULL}, \
\
    {"masterpassword", pdc_stringlist,  PDF_SECURITY_FLAG, 1, 1, \
      0.0, PDF_MAXPW, NULL}, \
\
    {"userpassword", pdc_stringlist,  PDF_SECURITY_FLAG, 1, 1, \
      0.0, PDF_MAXPW, NULL}, \
\
    {"permissions", pdc_keywordlist, \
      PDF_SECURITY_FLAG | PDC_OPT_BUILDOR | PDC_OPT_DUPORIGVAL, 1, 9,\
      0.0, 0.0, pdc_permissions_keylist}, \
\
    {"update", pdc_booleanlist, PDF_UPDATE_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"tagged", pdc_booleanlist, PDF_TAGGED_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"lang", pdc_stringlist,  PDF_TAGGED_FLAG, 1, 1, \
      0.0, PDF_MAX_LANGCODE, NULL}, \
\
    {"search", pdc_stringlist,  PDC_OPT_NONE, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    {"groups", pdc_stringlist,  PDC_OPT_NONE, 1, PDC_USHRT_MAX, \
      0.0, PDF_MAX_NAMESTRING, NULL}, \
\
    {"optimize", pdc_booleanlist, PDF_LINEARIZE_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"linearize", pdc_booleanlist, PDF_LINEARIZE_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"inmemory", pdc_booleanlist, PDF_LINEARIZE_FLAG, 1, 1,\
      0.0, 0.0, NULL}, \
\
    {"tempdirname", pdc_stringlist,  PDF_LINEARIZE_FLAG, 1, 1, \
      4.0, 400.0, NULL}, \

#if defined(MVS) || defined(MVS_TEST)
#define PDF_DOCUMENT_OPTIONS10 \
\
    {"filemode", pdc_stringlist,  PDC_OPT_NONE, 1, 1, \
      0.0, PDF_MAX_NAMESTRING, NULL}, \
\
    {"recordsize", pdc_integerlist, PDF_LINEARIZE_FLAG, 1, 1, \
      0.0, 32768.0, NULL}, \
\
    {"tempfilenames", pdc_stringlist,  PDF_LINEARIZE_FLAG, 2, 2, \
      4.0, 400.0, NULL}, \

#endif


#define PDF_DOCUMENT_OPTIONS2 \
\
    {"hypertextencoding", pdc_stringlist,  PDC_OPT_NONE, 1, 1, \
      0.0, PDF_MAX_NAMESTRING, NULL}, \
\
    {"moddate", pdc_booleanlist, PDC_OPT_NONE, 1, 1,\
      0.0, 0.0, NULL}, \
\
    {"destination", pdc_stringlist, PDC_OPT_NONE, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    {"destname", pdc_stringlist, PDC_OPT_IGNOREIF1, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    {"action", pdc_stringlist, PDC_OPT_NONE, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    {"labels", pdc_stringlist,  PDC_OPT_NONE, 1, PDC_USHRT_MAX, \
      0.0, PDC_USHRT_MAX, NULL}, \
\
    {"openmode", pdc_keywordlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, pdf_openmode_keylist}, \
\
    {"pagelayout", pdc_keywordlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, pdf_pagelayout_pdfkeylist}, \
\
    {"uri", pdc_stringlist, PDC_OPT_NONE, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    {"viewerpreferences", pdc_stringlist, PDC_OPT_NONE, 1, 1, \
      0.0, PDC_USHRT_MAX, NULL}, \
\
    {"autoxmp", pdc_booleanlist, PDF_METADATA_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"metadata", pdc_stringlist, PDF_METADATA_FLAG, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    {"attachments", pdc_stringlist, PDC_OPT_NONE, 1, PDC_USHRT_MAX, \
      0.0, PDC_INT_MAX, NULL}, \


/* document struct */

struct pdf_document_s
{
    int compatibility;               /* PDF version number * 10 */
    pdc_flush_state flush;           /* output flushing points */






#if defined(MVS) || defined(MVS_TEST)
    char *fopenparams;               /* additional fopen() parameter string */
    char **tempfilenames;            /* 2 temporary file names */
#endif

    pdc_bool moddate;                /* modified date will be created */
    char lang[PDF_MAX_LANGCODE + 1]; /* default natural language */
    char *action;                    /* document actions */
    pdf_dest *dest;                  /* destination as open action */
    char *uri;                       /* document's base url */
    char *viewerpreferences;         /* option list with viewer preferences */
    pdc_bool writevpdict;            /* viewer preferences dictionary
                                      * must be written */
    pdf_openmode openmode;           /* document open mode */
    pdf_pagelayout pagelayout;       /* page layout within document */

    char *searchindexname;           /* file name for search index */
    char *searchindextype;           /* type for search index */

    pdf_attachments *attachments;    /* temporarily file attachments */
    int nattachs;                    /* number of file attachments */


    char *filename;                  /* file name of document */
    size_t (*writeproc)(PDF *p, void *data, size_t size);
                                     /* output procedure */
    FILE *fp;                        /* file id - deprecated */
    int len;                         /* length of custom */
};

static pdf_document *
pdf_init_get_document(PDF *p)
{
    static const char fn[] = "pdf_init_get_document";

    if (p->document == NULL)
    {
        pdf_document *doc = (pdf_document *)
                                pdc_malloc(p->pdc, sizeof(pdf_document), fn);

        doc->compatibility = PDF_DEF_COMPATIBILITY;
        doc->flush = pdc_flush_page;






#if defined(MVS) || defined(MVS_TEST)
        doc->fopenparams = NULL;
        doc->tempfilenames = NULL;
#endif

        doc->moddate = pdc_false;
        doc->lang[0] = 0;
        doc->action = NULL;
        doc->dest = NULL;
        doc->uri = NULL;
        doc->viewerpreferences = NULL;
        doc->writevpdict = pdc_false;
        doc->openmode = open_auto;
        doc->pagelayout = layout_default;

        doc->searchindexname = NULL;
        doc->searchindextype = NULL;

        doc->attachments = NULL;
        doc->nattachs = 0;


        doc->fp = NULL;
        doc->filename = NULL;
        doc->writeproc = NULL;
        doc->len = 0;

        p->document = doc;
    }

    return p->document;
}

static void
pdf_cleanup_document_internal(PDF *p)
{
    pdf_document *doc = (pdf_document *) p->document;

    if (doc)
    {
        pdf_cleanup_destination(p, doc->dest);
        doc->dest = NULL;

        if (doc->action)
        {
            pdc_free(p->pdc, doc->action);
            doc->action = NULL;
        }

        if (doc->uri)
        {
            pdc_free(p->pdc, doc->uri);
            doc->uri = NULL;
        }

        if (doc->viewerpreferences)
        {
            pdc_free(p->pdc, doc->viewerpreferences);
            doc->viewerpreferences = NULL;
        }



#if defined(MVS) || defined(MVS_TEST)
        if (doc->fopenparams)
        {
            pdc_free(p->pdc, doc->fopenparams);
            doc->fopenparams = NULL;
        }

        if (doc->tempfilenames)
        {
            pdc_cleanup_optstringlist(p->pdc, doc->tempfilenames, 2);
            doc->tempfilenames = NULL;
        }
#endif


        if (doc->searchindexname)
        {
            pdc_free(p->pdc, doc->searchindexname);
            doc->searchindexname = NULL;
        }

        if (doc->searchindextype)
        {
            pdc_free(p->pdc, doc->searchindextype);
            doc->searchindextype = NULL;
        }

        if (doc->filename)
        {
            pdc_free(p->pdc, doc->filename);
            doc->filename = NULL;
        }

        pdc_free(p->pdc, doc);
        p->document = NULL;
    }
}


/* ---------------------------- PDFA / PDFX -------------------------- */



void
pdf_fix_openmode(PDF *p)
{
    pdf_document *doc = pdf_init_get_document(p);

    if (doc->openmode == open_auto)
        doc->openmode = open_bookmarks;
}



pdc_bool
pdf_get_plainmetadata(PDF *p)
{
    (void) p;
    return pdc_false;
}


/* ------------------------- viewerpreferences ----------------------- */

static const pdc_defopt pdf_viewerpreferences_options[] =
{
    {"centerwindow", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0, NULL},

    {"direction", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_textdirection_pdfkeylist},

    {"displaydoctitle", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0, NULL},

    {"fitwindow", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0, NULL},

    {"hidemenubar", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0, NULL},

    {"hidetoolbar", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0, NULL},

    {"hidewindowui", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0, NULL},

    {"nonfullscreenpagemode", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_nonfullscreen_keylist},

    {"viewarea", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_usebox_keylist},

    {"viewclip", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_usebox_keylist},

    {"printarea", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_usebox_keylist},

    {"printclip", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_usebox_keylist},

    {"printscaling", pdc_keywordlist, PDC_OPT_PDC_1_6, 1, 1,
      0.0, 0.0, pdf_printscaling_pdfkeylist},

    {"duplex", pdc_keywordlist, PDC_OPT_PDC_1_7, 1, 1,
      0.0, 0.0, pdf_duplex_pdfkeylist},

    {"picktraybypdfsize", pdc_booleanlist, PDC_OPT_PDC_1_7, 1, 1,
      0.0, 0, NULL},

    {"printpagerange", pdc_integerlist, PDC_OPT_PDC_1_7 | PDC_OPT_EVENNUM,
      1, PDC_USHRT_MAX, 1.0, PDC_INT_MAX, NULL}, \

    {"numcopies", pdc_integerlist, PDC_OPT_PDC_1_7, 1, 1, \
      1.0, 5.0, NULL}, \

    PDC_OPT_TERMINATE
};

static int
pdf_parse_and_write_viewerpreferences(PDF *p, const char *optlist,
                                      pdc_bool output)
{
    pdc_resopt *resopts = NULL;
    pdc_clientdata cdata;
    char **strlist;
    pdc_bool writevpdict = pdc_false;
    pdc_bool flag;
    int i, nv, inum;

    /* parsing option list */
    pdf_set_clientdata(p, &cdata);
    resopts = pdc_parse_optionlist(p->pdc, optlist,
                  pdf_viewerpreferences_options, &cdata, pdc_true);

    if (pdc_get_optvalues("hidetoolbar", resopts, &flag, NULL) && flag)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/HideToolbar true\n");
    }

    if (pdc_get_optvalues("hidemenubar", resopts, &flag, NULL) && flag)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/HideMenubar true\n");
    }

    if (pdc_get_optvalues("hidewindowui", resopts, &flag, NULL) && flag)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/HideWindowUI true\n");
    }

    if (pdc_get_optvalues("fitwindow", resopts, &flag, NULL) && flag)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/FitWindow true\n");
    }

    if (pdc_get_optvalues("centerwindow", resopts, &flag, NULL) && flag)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/CenterWindow true\n");
    }

    if (pdc_get_optvalues("displaydoctitle", resopts, &flag, NULL) && flag)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/DisplayDocTitle true\n");
    }

    if (pdc_get_optvalues("nonfullscreenpagemode", resopts, &inum, NULL) &&
        inum != (int) open_none)
    {
        if (inum == (int) open_layers)
            pdc_error(p->pdc, PDF_E_UNSUPP_LAYER, 0, 0, 0, 0);
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/NonFullScreenPageMode/%s\n",
                   pdc_get_keyword(inum, pdf_openmode_pdfkeylist));
    }


    if (pdc_get_optvalues("direction", resopts, &inum, NULL) &&
        inum != (int) doc_l2r)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/Direction/%s\n",
                   pdc_get_keyword(inum, pdf_textdirection_pdfkeylist));
    }

    if (pdc_get_optvalues("viewarea", resopts, &inum, NULL) &&
        inum != (int) pdc_pbox_crop)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/ViewArea%s\n",
                   pdc_get_keyword(inum, pdf_usebox_pdfkeylist));
    }

    if (pdc_get_optvalues("viewclip", resopts, &inum, NULL) &&
        inum != (int) pdc_pbox_crop)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/ViewClip%s\n",
                   pdc_get_keyword(inum, pdf_usebox_pdfkeylist));
    }

    if (pdc_get_optvalues("printarea", resopts, &inum, NULL) &&
        inum != (int) pdc_pbox_crop)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/PrintArea%s\n",
                   pdc_get_keyword(inum, pdf_usebox_pdfkeylist));
    }

    if (pdc_get_optvalues("printclip", resopts, &inum, NULL) &&
        inum != (int) pdc_pbox_crop)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/PrintClip%s\n",
                   pdc_get_keyword(inum, pdf_usebox_pdfkeylist));
    }

    if (pdc_get_optvalues("printscaling", resopts, &inum, NULL) &&
        inum != (int) doc_appdefault)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/PrintScaling/%s\n",
                   pdc_get_keyword(inum, pdf_printscaling_pdfkeylist));
    }

    if (pdc_get_optvalues("duplex", resopts, &inum, NULL) &&
        inum != (int) doc_none)
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/Duplex/%s\n",
                   pdc_get_keyword(inum, pdf_duplex_pdfkeylist));
    }

    if (pdc_get_optvalues("picktraybypdfsize", resopts, &flag, NULL))
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/PickTrayByPDFSize %s\n",
                               PDC_BOOLSTR(flag));
    }

    nv = pdc_get_optvalues("printpagerange", resopts, NULL, &strlist);
    if (nv)
    {
        writevpdict = pdc_true;
        if (output)
        {
            int *prs = (int *) strlist;

            pdc_printf(p->out, "/PrintPageRange");
            pdc_begin_array(p->out);
            for (i = 0; i < nv; i++)
                /* because of bug #1623: -1 */
                pdc_printf(p->out, "%d ", prs[i] - 1);
            pdc_end_array(p->out);
        }
    }

    if (pdc_get_optvalues("numcopies", resopts, &inum, NULL))
    {
        writevpdict = pdc_true;
        if (output) pdc_printf(p->out, "/NumCopies %d\n", inum);
    }

    pdc_cleanup_optionlist(p->pdc, resopts);

    return writevpdict;
}


/* ------------------------- search ----------------------- */

static const pdc_defopt pdf_search_options[] =
{
    {"filename", pdc_stringlist, PDC_OPT_REQUIRED, 1, 1,
      1.0, PDC_FILENAMELEN, NULL},

    {"indextype", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDF_MAX_NAMESTRING, NULL},

    PDC_OPT_TERMINATE
};

static void
pdf_parse_search_optlist(PDF *p, const char *optlist,
                         pdc_encoding htenc, int htcp)
{
    pdf_document *doc = p->document;
    pdc_resopt *resopts = NULL;

    /* parsing option list */
    resopts = pdc_parse_optionlist(p->pdc, optlist,
                              pdf_search_options, NULL, pdc_true);

    if (pdf_get_opt_textlist(p, "filename", resopts, htenc, htcp,
                             pdc_true, NULL, &doc->searchindexname, NULL))
        pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    if (pdc_get_optvalues("indextype", resopts, NULL, NULL))
        doc->searchindextype =
            (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
    else
        doc->searchindextype = pdc_strdup(p->pdc, "PDX");

    pdc_cleanup_optionlist(p->pdc, resopts);
}

static void
pdf_write_search_indexes(PDF *p)
{
    pdf_document *doc = p->document;

    if (doc->searchindexname != NULL)
    {
        pdc_puts(p->out, "/Search");
        pdc_begin_dict(p->out);                         /* Search */
        pdc_puts(p->out, "/Indexes");
        pdc_begin_array(p->out);
        pdc_begin_dict(p->out);                         /* Indexes */
        pdc_puts(p->out, "/Name");
        pdc_printf(p->out, "/%s", doc->searchindextype);
        pdc_puts(p->out, "/Index");
        pdc_begin_dict(p->out);                         /* Index */
        pdc_puts(p->out, "/Type/Filespec");
        pdc_puts(p->out, "/F");
        pdf_put_pdffilename(p, doc->searchindexname);
        if (p->compatibility >= PDC_1_7)
        {
            pdc_printf(p->out, "/UF");
            pdf_put_pdfunifilename(p, doc->searchindexname);
        }
        pdc_end_dict(p->out);                           /* Index */
        pdc_end_dict(p->out);                           /* Indexes */
        pdc_end_array(p->out);
        pdc_end_dict(p->out);                           /* Search */
    }
}


/* ---------------------- file attachements -------------------- */

static void
pdc_cleanup_attachments_tmp(void *opaque, void *mem)
{
    if (mem)
    {
        PDF *p = (PDF *) opaque;
        pdf_document *doc = p->document;
        int i;

        if (doc != NULL)
        {
            for (i = 0; i < doc->nattachs; i++)
            {
                pdf_attachments *fat = &doc->attachments[i];

                if (fat->filename != NULL)
                    pdc_free(p->pdc, fat->filename);
                if (fat->name != NULL)
                    pdc_free(p->pdc, fat->name);
                if (fat->description != NULL)
                    pdc_free(p->pdc, fat->description);
                if (fat->mimetype != NULL)
                    pdc_free(p->pdc, fat->mimetype);
            }

            doc->attachments = NULL;
            doc->nattachs = 0;
        }
    }
}

static const pdc_defopt pdf_attachments_options[] =
{
    {"filename", pdc_stringlist, PDC_OPT_REQUIRED, 1, 1,
      1.0, PDC_FILENAMELEN, NULL},

    {"description", pdc_stringlist, PDC_OPT_PDC_1_6, 1, 1,
      0.0, PDC_INT_MAX, NULL},

    {"name", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"mimetype", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDF_MAX_NAMESTRING, NULL},

    PDC_OPT_TERMINATE
};

static void
pdf_parse_attachments_optlist(PDF *p, char **optlists, int ns,
                              pdc_encoding htenc, int htcp)
{
    static const char fn[] = "pdf_parse_attachments_optlist";
    pdf_document *doc = p->document;
    pdc_resopt *resopts = NULL;
    pdc_clientdata cdata;
    int i;

    doc->attachments = (pdf_attachments *) pdc_malloc_tmp(p->pdc,
                               ns * sizeof(pdf_attachments), fn,
                               p, pdc_cleanup_attachments_tmp);
    doc->nattachs = ns;

    pdf_set_clientdata(p, &cdata);

    for (i = 0; i < ns; i++)
    {
        pdf_attachments *fat = &doc->attachments[i];

        fat->filename = NULL;
        fat->name = NULL;
        fat->description = NULL;
        fat->mimetype = NULL;
        fat->filesize = 0;
    }

    for (i = 0; i < ns; i++)
    {
        pdf_attachments *fat = &doc->attachments[i];

        /* parsing option list */
        resopts = pdc_parse_optionlist(p->pdc, optlists[i],
                            pdf_attachments_options, &cdata, pdc_true);

        if (pdf_get_opt_textlist(p, "filename", resopts, htenc, htcp,
                 /* bug #2344 */ pdc_undef, NULL, &fat->filename, NULL))
            pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

        if (pdf_get_opt_textlist(p, "description", resopts, htenc, htcp,
                                 pdc_true, NULL, &fat->description, NULL))
            pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

        if (pdf_get_opt_textlist(p, "name", resopts, htenc, htcp,
                                 pdc_true, NULL, &fat->name, NULL))
            pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

        if (pdc_get_optvalues("mimetype", resopts, NULL, NULL))
            fat->mimetype =
                (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

        pdc_cleanup_optionlist(p->pdc, resopts);

        fat->filesize = pdf_check_file(p, fat->filename, pdc_true);
    }
}

static void
pdf_write_attachments(PDF *p)
{
    static const char fn[] = "pdf_write_attachments";
    pdf_document *doc = p->document;
    pdc_id attachment_id, obj_id;
    const char *basename;
    char *name;
    int i;

    for (i = 0; i < doc->nattachs; i++)
    {
        pdf_attachments *fat = &doc->attachments[i];

        if (fat->filesize > 0)
        {
            /* create file specification dictionary */
            attachment_id = pdc_begin_obj(p->out, PDC_NEW_ID);
            pdc_begin_dict(p->out);                 /* FS dict */

            /* see bug #1439 */
            basename = pdc_file_strip_dirs(fat->filename);

            pdc_puts(p->out, "/Type/Filespec\n");
            pdc_printf(p->out, "/F");
            pdf_put_pdffilename(p, basename);
            pdc_puts(p->out, "\n");
            if (p->compatibility >= PDC_1_7)
            {
                pdc_printf(p->out, "/UF");
                pdf_put_pdfunifilename(p, basename);
                pdc_puts(p->out, "\n");
            }

            if (fat->description != NULL)
            {
                pdc_puts(p->out, "/Desc");
                pdf_put_hypertext(p, fat->description);
                pdc_puts(p->out, "\n");
            }

            obj_id = pdc_alloc_id(p->out);
            pdc_puts(p->out, "/EF");
            pdc_begin_dict(p->out);
            pdc_objref(p->out, "/F", obj_id);
            pdc_end_dict(p->out);

            pdc_end_dict(p->out);                   /* FS dict */
            pdc_end_obj(p->out);

            /* embed file */
            pdf_embed_file(p, obj_id, fat->filename, fat->mimetype,
                           fat->filesize);

            /* insert name in tree */
            if (fat->name == NULL)
                name = pdc_strdup_ext(p->pdc, basename, 0, fn);
            else
                name = pdc_strdup_ext(p->pdc, fat->name, 0, fn);
            pdf_insert_name(p, name, names_embeddedfiles, attachment_id);
        }
    }
}

pdc_off_t
pdf_check_file(PDF *p, const char *filename, pdc_bool verbose)
{
    pdc_off_t filesize = 0;
    const char *qualifier = "attachment ";
    pdc_file *fp;

    fp = pdc_fsearch_fopen(p->pdc, filename, NULL, qualifier,
                                     PDC_FILE_BINARY);
    if (fp == NULL)
    {
        if (verbose)
            pdc_error(p->pdc, -1, 0, 0, 0, 0);
    }
    else
    {
        filesize = pdc_file_size(fp);
        pdc_fclose(fp);

        if (filesize == 0)
        {
            pdc_set_errmsg(p->pdc, PDC_E_IO_FILE_EMPTY, qualifier, filename,
                           0, 0);
            if (verbose)
                pdc_error(p->pdc, -1, 0, 0, 0, 0);
        }
    }

    return filesize;
}

void
pdf_embed_file(PDF *p, pdc_id obj_id, const char *filename,
               const char *mimetype, pdc_off_t filesize)
{
    pdc_id length_id;
    PDF_data_source src;

    pdc_begin_obj(p->out, obj_id);
    pdc_begin_dict(p->out);                    /* F dict */

    pdc_puts(p->out, "/Type/EmbeddedFile\n");

    if (mimetype && *mimetype)
    {
        pdc_puts(p->out, "/Subtype");
        pdf_put_pdfname(p, mimetype);
        pdc_puts(p->out, "\n");
    }

    pdc_puts(p->out, "/Params");
    pdc_begin_dict(p->out);                    /* Params */
    pdc_printf(p->out, "/Size %lld", filesize);
    pdc_end_dict(p->out);                      /* Params */

    if (pdc_get_compresslevel(p->out))
    {
	pdc_puts(p->out, "/Filter/FlateDecode\n");
    }

    length_id = pdc_alloc_id(p->out);
    pdc_objref(p->out, "/Length", length_id);

    pdc_end_dict(p->out);                    /* F dict */

    /* write the file in the PDF */
    src.private_data = (void *) filename;
    src.init = pdf_data_source_file_init;
    src.fill = pdf_data_source_file_fill;
    src.terminate = pdf_data_source_file_terminate;
    src.length = (long) 0;
    src.offset = (long) 0;


    pdf_copy_stream(p, &src, pdc_true);


    pdc_end_obj(p->out);

    pdc_put_pdfstreamlength(p->out, length_id);

    if (p->flush & pdc_flush_content)
        pdc_flush_stream(p->out);
}


/* ---------------------- linearize -------------------- */



/* ------------------ document options ----------------- */

static void
pdf_get_document_common_options(PDF *p, pdc_resopt *resopts, int fcode)
{
    pdf_document *doc = p->document;
    pdc_encoding htenc;
    int htcp;
    char **strlist;
    int i, inum, ns;


    htenc =
        pdf_get_hypertextencoding_opt(p, resopts, &htcp, pdc_true);

    if (pdc_get_optvalues("destination", resopts, NULL, &strlist))
    {
        if (doc->dest)
            pdc_free(p->pdc, doc->dest);
        doc->dest = pdf_parse_destination_optlist(p, strlist[0], 1,
                                                  pdf_openaction);
    }
    else
    {
        pdf_dest *dest = pdf_get_option_destname(p, resopts, htenc, htcp);
        if (dest)
        {
            if (doc->dest)
                pdc_free(p->pdc, doc->dest);
            doc->dest = dest;
        }
    }

    if (pdc_get_optvalues("action", resopts, NULL, NULL))
    {
        if (doc->action)
            pdc_free(p->pdc, doc->action);
        doc->action = (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
        pdf_parse_and_write_actionlist(p, event_document, NULL, doc->action);
    }

    inum = pdc_get_optvalues("labels", resopts, NULL, &strlist);
    for (i = 0; i < inum; i++)
	pdf_set_pagelabel(p, strlist[i], fcode);

    if (pdc_get_optvalues("openmode", resopts, &inum, NULL))
        doc->openmode = (pdf_openmode) inum;

        if (doc->openmode ==  open_layers)
            pdc_error(p->pdc, PDF_E_UNSUPP_LAYER, 0, 0, 0, 0);

    if (doc->openmode == open_attachments && p->compatibility < PDC_1_6)
        pdc_error(p->pdc, PDC_E_OPT_VERSION, "openmode=attachments",
                  pdc_get_pdfversion(p->pdc, p->compatibility), 0, 0);

    if (pdc_get_optvalues("pagelayout", resopts, &inum, NULL))
        doc->pagelayout = (pdf_pagelayout) inum;
    if (p->compatibility < PDC_1_5)
    {
        if (doc->pagelayout == layout_twopageleft)
            pdc_error(p->pdc, PDC_E_OPT_VERSION, "pagelayout=TwoPageLeft",
                      pdc_get_pdfversion(p->pdc, p->compatibility), 0, 0);
        if (doc->pagelayout == layout_twopageright)
            pdc_error(p->pdc, PDC_E_OPT_VERSION, "pagelayout=TwoPageRight",
                      pdc_get_pdfversion(p->pdc, p->compatibility), 0, 0);
    }

    if (pdc_get_optvalues("uri", resopts, NULL, NULL))
    {
        if (doc->uri)
            pdc_free(p->pdc, doc->uri);
        doc->uri = (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
    }

    if (pdc_get_optvalues("viewerpreferences", resopts, NULL, NULL))
    {
        if (doc->viewerpreferences)
            pdc_free(p->pdc, doc->viewerpreferences);
        doc->viewerpreferences =
            (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
        doc->writevpdict |=
            pdf_parse_and_write_viewerpreferences(p, doc->viewerpreferences,
                                                  pdc_false);
    }

    if (pdc_get_optvalues("search", resopts, NULL, &strlist))
        pdf_parse_search_optlist(p, strlist[0], htenc, htcp);


    pdc_get_optvalues("moddate", resopts, &doc->moddate, NULL);

    ns = pdc_get_opt_utf8strings(p->pdc, "attachments", resopts, 0, &strlist);
    if (ns)
        pdf_parse_attachments_optlist(p, strlist, ns, htenc, htcp);
}

static const pdc_defopt pdf_begin_document_options[] =
{
    PDF_DOCUMENT_OPTIONS1
#if defined(MVS) || defined(MVS_TEST)
    PDF_DOCUMENT_OPTIONS10
#endif
    PDF_DOCUMENT_OPTIONS2
    PDF_ERRORPOLICY_OPTION
    PDC_OPT_TERMINATE
};


/*
 * The external callback interface requires a PDF* as the first argument,
 * while the internal interface uses pdc_output* and doesn't know about PDF*.
 * We use a wrapper to bridge the gap, and store the PDF* within the
 * pdc_output structure opaquely.
 */

static size_t
writeproc_wrapper(pdc_output *out, void *data, size_t size)
{
    size_t ret;

    PDF *p = (PDF *) pdc_get_opaque(out);

    ret = (p->writeproc)(p, data, size);
    pdc_logg_cond(p->pdc, 1, trc_api,
                       "/* writeproc(data[%p], %d)[%d] */\n", data, size, ret);
    return ret;
}



/* ---------------------------- begin document -------------------------- */

static int
pdf_begin_document_internal(PDF *p, const char *optlist, pdc_bool callback)
{
    pdf_document *doc = p->document;
    pdc_resopt *resopts = NULL;
    char **groups = NULL;
    int n_groups = 0;
    pdc_bool verbose = p->debug[(int) 'o'];
    pdc_outctl oc;

    (void) callback;

    verbose = pdf_get_errorpolicy(p, NULL, verbose);

    /* parsing option list */
    if (optlist && *optlist)
    {
        int inum;

        resopts = pdc_parse_optionlist(p->pdc, optlist,
                                   pdf_begin_document_options, NULL, pdc_true);

        verbose = pdf_get_errorpolicy(p, resopts, verbose);

        pdc_get_optvalues("compatibility", resopts, &doc->compatibility, NULL);

        if (pdc_get_optvalues("flush", resopts, &inum, NULL))
            doc->flush = (pdc_flush_state) inum;

        pdc_get_optvalues("lang", resopts, doc->lang, NULL);








#if defined(MVS) || defined(MVS_TEST)
        if (pdc_get_optvalues("filemode", resopts, NULL, NULL))
        {
            doc->fopenparams =
                (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
        }
#endif

	n_groups = pdc_get_optvalues("groups", resopts, NULL, &groups);
    }

    /* copy for easy access */
    p->compatibility = doc->compatibility;
    p->pdc->compatibility = doc->compatibility;
    p->flush = doc->flush;





    /*
     * None of these functions must call pdc_alloc_id() or generate
     * any output since the output machinery is not yet initialized!
     */

    pdf_init_pages(p, (const char **) groups, n_groups);

    /* common options */
    pdf_get_document_common_options(p, resopts, PDF_FC_BEGIN_DOCUMENT);


    /* create document digest */
    pdc_init_digest(p->out);

    if (!p->pdc->ptfrun)
    {
	if (doc->fp)
	    pdc_update_digest(p->out, (pdc_byte *) doc->fp, doc->len);
	else if (doc->writeproc)
	    pdc_update_digest(p->out, (pdc_byte *) &doc->writeproc, doc->len);
	else if (doc->filename)
	    pdc_update_digest(p->out, (pdc_byte *) doc->filename, doc->len);
    }

    pdf_feed_digest_info(p);

    if (!p->pdc->ptfrun)
    {
        pdc_update_digest(p->out, (pdc_byte *) &p, sizeof(PDF*));
        pdc_update_digest(p->out, (pdc_byte *) p, sizeof(PDF));
    }


    pdc_finish_digest(p->out, !p->pdc->ptfrun);

    /* preparing output struct */
    pdc_init_outctl(&oc);
    oc.flush = doc->flush;

    if (doc->fp)
        oc.fp = doc->fp;
    else if (doc->writeproc)
    {
        oc.writeproc = writeproc_wrapper;
        p->writeproc = doc->writeproc;
    }
    else if (doc->filename)
        oc.filename = doc->filename;
    else
        oc.filename = "";


#if defined(MVS) || defined(MVS_TEST)
    oc.fopenparams = doc->fopenparams;
#endif


    PDC_TRY(p->pdc)
    {
        if (!pdc_init_output((void *) p, p->out, doc->compatibility, &oc))
        {
            if (oc.filename && *oc.filename)
            {
                pdc_set_fopen_errmsg(p->pdc,
                    pdc_get_fopen_errnum(p->pdc, PDC_E_IO_WROPEN), "PDF ",
                    pdc_errprintf(p->pdc, "%.*s", PDC_ERR_MAXSTRLEN,
                                  oc.filename));
                if (verbose)
                {
                    pdf_cleanup_document_internal(p);
                    PDC_RETHROW(p->pdc);
                }
            }

            pdf_cleanup_document_internal(p);
            PDC_EXIT_TRY(p->pdc);
            return -1;
        }
    }
    PDC_CATCH(p->pdc)
    {
        pdf_cleanup_document_internal(p);
        if (verbose)
            PDC_RETHROW(p->pdc);

        return -1;
    }

    /* deprecated */
    p->bookmark_dest = pdf_init_destination(p);

    pdf_init_images(p);
    pdf_init_xobjects(p);
    pdf_init_fonts(p);
    pdf_init_outlines(p);
    pdf_init_annot_params(p);
    pdf_init_colorspaces(p);
    pdf_init_pattern(p);
    pdf_init_shadings(p);
    pdf_init_extgstates(p);





    /* Write the constant /ProcSet array once at the beginning */
    p->procset_id = pdc_begin_obj(p->out, PDC_NEW_ID);
    pdc_puts(p->out, "[/PDF/ImageB/ImageC/ImageI/Text]\n");
    pdc_end_obj(p->out);

    pdf_init_pages2(p);

    pdf_write_attachments(p);

    return 1;
}

#if defined(_MSC_VER) && defined(_MANAGED)
#pragma unmanaged
#endif
int
pdf__begin_document(PDF *p, const char *filename, int len, const char *optlist)
{
    pdf_document *doc;
    pdc_bool verbose = p->debug[(int) 'o'];
    int retval;

    verbose = pdf_get_errorpolicy(p, NULL, verbose);


    doc = pdf_init_get_document(p);

    /* file ID or filename */
    if (len == -1)
    {
        FILE *fp = (FILE *) filename;

        /*
         * It is the callers responsibility to open the file in binary mode,
         * but it doesn't hurt to make sure it really is.
         * The Intel version of the Metrowerks compiler doesn't have setmode().
         */
#if !defined(__MWERKS__) && (defined(WIN32) || defined(OS2))
#if !defined(__BORLANDC__) && !defined(OS2)
        _setmode(_fileno(fp), _O_BINARY);
#else
        setmode(fileno(fp), O_BINARY);
#endif
#endif

        doc->fp = fp;
        doc->len = sizeof(FILE);
    }
    else if (filename && (*filename || len > 0))
    {
        filename = pdf_convert_filename(p, filename, len, "filename",
                                        PDC_CONV_WITHBOM);
        doc->filename = pdc_strdup(p->pdc, filename);
        doc->len = (int) strlen(doc->filename);
    }

    retval = pdf_begin_document_internal(p, optlist, pdc_false);

    if (retval > -1)
        PDF_SET_STATE(p, pdf_state_document);

    if (!p->pdc->smokerun)
        pdc_logg_cond(p->pdc, 1, trc_api, "[Begin document]\n");

    return retval;
}
#if defined(_MSC_VER) && defined(_MANAGED)
#pragma managed
#endif

void
pdf__begin_document_callback(PDF *p,
    size_t (*i_writeproc)(PDF *p, void *data, size_t size), const char *optlist)
{
    size_t (*writeproc)(PDF *, void *, size_t) = i_writeproc;
    pdf_document *doc;

    if (writeproc == NULL)
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "writeproc", 0, 0, 0);

    doc = pdf_init_get_document(p);

    /* initializing and opening the document */
    doc->writeproc = writeproc;
    doc->len = sizeof(writeproc);

    (void) pdf_begin_document_internal(p, optlist, pdc_true);

    PDF_SET_STATE(p, pdf_state_document);

    if (!p->pdc->smokerun)
        pdc_logg_cond(p->pdc, 1, trc_api, "[Begin document]\n");
}

/* -----------------------------  name tree  ----------------------------- */

struct pdf_name_s
{
    pdc_id              obj_id;         /* id of this name object */
    char *              name;           /* name string */
    pdf_nametree_type   type;           /* name tree type */
};

static void
pdf_cleanup_names(PDF *p)
{
    int i;

    if (p->names == NULL)
        return;

    for (i = 0; i < p->names_number; i++)
    {
        pdc_free(p->pdc, p->names[i].name);
    }

    pdc_free(p->pdc, p->names);
    p->names_number = 0;
    p->names = NULL;
}

void
pdf_insert_name(PDF *p, const char *name, pdf_nametree_type type, pdc_id obj_id)
{
    static const char fn[] = "pdf_insert_name";
    int i;

    if (p->names == NULL || p->names_number == p->names_capacity)
    {
        if (p->names == NULL)
        {
            p->names_number = 0;
            p->names_capacity = NAMES_CHUNKSIZE;
            p->names = (pdf_name *) pdc_malloc(p->pdc,
                sizeof(pdf_name) * p->names_capacity, fn);
        }
        else
        {
            p->names_capacity *= 2;
            p->names = (pdf_name *) pdc_realloc(p->pdc, p->names,
                sizeof(pdf_name) * p->names_capacity, fn);
        }
        for (i = p->names_number; i < p->names_capacity; i++)
        {
            p->names[i].obj_id = PDC_BAD_ID;
            p->names[i].name = NULL;
            p->names[i].type = names_undef;
        }
    }

    /* check identity */
    for (i = 0; i < p->names_number; i++)
    {
        if (p->names[i].type == type && !strcmp(p->names[i].name, name))
        {
            pdc_free(p->pdc, p->names[i].name);
            p->names[i].name = (char *) name;
            return;
        }
    }

    p->names[i].obj_id = obj_id;
    p->names[i].name = (char *) name;
    p->names[i].type = type;
    p->names_number++;
}

pdc_id
pdf_get_id_from_nametree(PDF *p, pdf_nametree_type type, const char *name)
{
    int i;

    for (i = 0; i < p->names_number; i++)
    {
        if (p->names[i].type == type && !strcmp(name, p->names[i].name))
            return p->names[i].obj_id;
    }

    return PDC_BAD_ID;
}

#define PDF_TREE_LEAF_SIZE 32

static char *
pdf_get_numbered_name(PDF *p, pdf_nametree_type type, int ia, int *in, int nn)
{
    int i, j = ia, n = 0;

    for (i = ia; i < p->names_number; i++)
    {
        if (p->names[i].type == type)
        {
            n++;
            if (n == nn)
            {
                if (in != NULL)
                    *in = i;

                return p->names[i].name;
            }
            j = i;
        }
    }

    return (in != NULL) ? NULL : p->names[j].name;
}

static pdc_id
pdf_write_names(PDF *p, pdf_nametree_type type)
{
    static const char fn[] = "pdf_write_names";
    pdc_id ret = PDC_BAD_ID;
    int i, nnames = 0;

    for (i = 0; i < p->names_number; i++)
    {
        if (p->names[i].type == type)
        {
            nnames++;
        }
    }

    if (nnames)
    {
        char *name;
        int nleafs, nnodes, ik, il, ia, nn;
        pdc_id *idlist;

        nnodes = nnames / PDF_TREE_LEAF_SIZE;
        if (!nnodes)
            nleafs = nnames;
        else
            nleafs = PDF_TREE_LEAF_SIZE;
        if (nnames > nnodes * nleafs)
            nnodes++;

        idlist = (pdc_id *) pdc_malloc(p->pdc,
                                (size_t) (nnodes * sizeof(pdc_id)), fn);

        for (i = 0; i < nnodes; i++)
            idlist[i] = pdc_alloc_id(p->out);

        ret = pdc_begin_obj(p->out, PDC_NEW_ID);    /* Names object */
        pdc_begin_dict(p->out);

        pdc_puts(p->out, "/Kids");
        pdc_begin_array(p->out);
        for (i = 0; i < nnodes; i++)
            pdc_objref_c(p->out, idlist[i]);
        pdc_end_array(p->out);

        pdc_end_dict(p->out);
        pdc_end_obj(p->out);                        /* Names object */

        ia = 0;
        for (ik = 0; ik < nnodes; ik++)
        {
            pdc_begin_obj(p->out, idlist[ik]);
            pdc_begin_dict(p->out);

            pdc_puts(p->out, "/Limits");
            pdc_begin_array(p->out);

            name = pdf_get_numbered_name(p, type, ia, NULL, 1);
            pdc_put_pdfstring(p->out, name, ( int ) pdc_strlen(name));

            nn = (ik == nnodes - 1) ? p->names_number : nleafs;
            name = pdf_get_numbered_name(p, type, ia, NULL, nn);
            pdc_put_pdfstring(p->out, name, ( int ) pdc_strlen(name));

            pdc_end_array(p->out);

            pdc_puts(p->out, "/Names");
            pdc_begin_array(p->out);

            for (il = 0; il < nn; il++)
            {
                name = pdf_get_numbered_name(p, type, ia, &ia, 1);
                if (name == NULL)
                    break;

                pdc_put_pdfstring(p->out, name, ( int ) pdc_strlen(name));
                pdc_objref(p->out, "", p->names[ia].obj_id);
                ia++;
            }

            pdc_end_array(p->out);

            pdc_end_dict(p->out);
            pdc_end_obj(p->out);
        }

        pdc_free(p->pdc, idlist);

    }

    return ret;
}

static int
name_compare( const void*  a, const void*  b)
{
    pdf_name *p1 = (pdf_name *) a;
    pdf_name *p2 = (pdf_name *) b;

    return pdc_wstrcmp(p1->name, p2->name);
}

/* ---------------------------- write document -------------------------- */

static pdc_id
pdf_write_pages_and_catalog(PDF *p, pdc_id orig_root_id)
{
    pdf_document *doc = p->document;
    pdc_bool openact = pdc_false;
    pdc_bool forpdfa = pdc_false;
    pdc_id act_idlist[PDF_MAX_EVENTS];
    pdc_id root_id = PDC_BAD_ID;
    pdc_id names_dests_id = PDC_BAD_ID;
    pdc_id names_javascript_id = PDC_BAD_ID;
    pdc_id names_ap_id = PDC_BAD_ID;
    pdc_id names_embeddedfiles_id = PDC_BAD_ID;
    pdc_id outintents1_id = PDC_BAD_ID;
    pdc_id outintents2_id = PDC_BAD_ID;

    pdc_id pages_id = pdf_write_pages_tree(p);
    pdc_id labels_id = pdf_write_pagelabels(p);



    (void) orig_root_id;

    /* name tree dictionaries */
    if (p->names_number)
    {
        char *name;
        int i, outlen, inlen;

        for (i = 0; i < p->names_number; i++)
        {
            inlen = ( int ) strlen(p->names[i].name);
            name = pdf_convert_pdfstring(p, p->names[i].name, inlen,
                                PDC_CONV_WITHBOM | PDC_CONV_TRYBYTES, &outlen);

            if (name != p->names[i].name)
                pdc_free(p->pdc, p->names[i].name);

            p->names[i].name = name;
        }

        qsort(p->names, (size_t) p->names_number, sizeof(pdf_name),
              name_compare);

        names_dests_id = pdf_write_names(p, names_dests);
        names_javascript_id = pdf_write_names(p, names_javascript);
        names_ap_id = pdf_write_names(p, names_ap);
        names_embeddedfiles_id = pdf_write_names(p, names_embeddedfiles);
    }

    (void) forpdfa;




    /* write action objects */
    if (doc->action)
        pdf_parse_and_write_actionlist(p, event_document, act_idlist,
                                       (const char *) doc->action);

    root_id = pdc_begin_obj(p->out, PDC_NEW_ID);	/* Catalog */
    pdc_begin_dict(p->out);
    pdc_puts(p->out, "/Type/Catalog\n");

    pdc_objref(p->out, "/Pages", pages_id);		/* Pages object */


    if (labels_id != PDC_BAD_ID)
    {
        pdc_objref(p->out, "/PageLabels", labels_id);
    }

    if (p->names_number)
    {
        pdc_printf(p->out, "/Names");
        pdc_begin_dict(p->out);                         /* Names */

        if (names_dests_id != PDC_BAD_ID)
            pdc_objref(p->out, "/Dests", names_dests_id);
        if (names_javascript_id != PDC_BAD_ID)
            pdc_objref(p->out, "/JavaScript", names_javascript_id);
        if (names_ap_id != PDC_BAD_ID)
            pdc_objref(p->out, "/AP", names_ap_id);
        if (names_embeddedfiles_id != PDC_BAD_ID)
            pdc_objref(p->out, "/EmbeddedFiles", names_embeddedfiles_id);

        pdc_end_dict(p->out);                           /* Names */
    }

    if (doc->writevpdict)
    {
        pdc_printf(p->out, "/ViewerPreferences\n");
        pdc_begin_dict(p->out);                         /* ViewerPreferences */
        pdf_parse_and_write_viewerpreferences(p,
                       doc->viewerpreferences, pdc_true);
        pdc_end_dict(p->out);                           /* ViewerPreferences */
    }

    if (doc->pagelayout != layout_default)
        pdc_printf(p->out, "/PageLayout/%s\n",
                pdc_get_keyword(doc->pagelayout, pdf_pagelayout_pdfkeylist));

    if (doc->openmode != open_auto && doc->openmode != open_none)
        pdc_printf(p->out, "/PageMode/%s\n",
                pdc_get_keyword(doc->openmode, pdf_openmode_pdfkeylist));

    pdf_write_outline_root(p);  /* /Outlines */

    if (doc->action)  /* /AA */
        openact = pdf_write_action_entries(p, event_document, act_idlist);

    if (doc->dest && !openact)
    {
        pdc_puts(p->out, "/OpenAction");
        pdf_write_destination(p, doc->dest);
    }

    if (doc->uri)
    {
        pdc_puts(p->out, "/URI");
        pdc_begin_dict(p->out);
        pdc_printf(p->out, "/Base");
        pdf_put_hypertext(p, doc->uri);
        pdc_end_dict(p->out);
    }


    if (doc->lang[0])
    {
        pdc_puts(p->out, "/Lang");
        pdf_put_hypertext(p, doc->lang);
        pdc_puts(p->out, "\n");
    }

    /* /StructTreeRoot /MarkInfo */

    /* /OCProperties */

    if (outintents1_id != PDC_BAD_ID || outintents2_id != PDC_BAD_ID)
    {
        pdc_puts(p->out, "/OutputIntents");
	pdc_begin_array(p->out);
        if (outintents1_id != PDC_BAD_ID)
            pdc_objref(p->out, "", outintents1_id);
        if (outintents2_id != PDC_BAD_ID)
            pdc_objref(p->out, "", outintents2_id);
	pdc_end_array(p->out);
    }

    /* /Search */
    pdf_write_search_indexes(p);

    /* /Metadata */

    /* not supported: /Threads /PieceInfo /Perms /Legal */

    pdc_end_dict(p->out);                               /* Catalog */
    pdc_end_obj(p->out);

    return root_id;
}


static void
pdf_write_document(PDF *p)
{
    if (PDF_GET_STATE(p) != pdf_state_error)
    {
        pdf_document *doc = p->document;
        pdc_id info_id = PDC_BAD_ID;
        pdc_id root_id = PDC_BAD_ID;

        if (pdf_last_page(p) == 0)
            pdc_error(p->pdc, PDF_E_DOC_EMPTY, 0, 0, 0, 0);

        pdf_write_attachments(p);


        /* Write all pending document information up to xref table + trailer */
	info_id = pdf_write_info(p, doc->moddate);

        pdf_write_doc_fonts(p);                 /* font objects */
        pdf_write_doc_colorspaces(p);           /* color space resources */
        pdf_write_doc_extgstates(p);            /* ExtGState resources */
	root_id = pdf_write_pages_and_catalog(p, root_id);
        pdf_write_outlines(p);
        pdc_write_xref(p->out);

        pdc_write_trailer(p->out, info_id, root_id, 0, -1, -1, -1);
    }

    pdc_close_output(p->out);
}

/* ------------------------------ end document ---------------------------- */

void
pdf_cleanup_document(PDF *p)
{
    pdf_cleanup_pages(p);

    if (PDF_GET_STATE(p) != pdf_state_object)
    {
        /* Don't call pdc_cleanup_output() here because we may still need
         * the buffer contents for pdf__get_buffer() after pdf__end_document().
         */

        pdf_delete_actions(p);

        pdf_cleanup_destination(p, p->bookmark_dest); /* deprecated */
        p->bookmark_dest = NULL;
        pdf_cleanup_document_internal(p);
        pdf_cleanup_info(p);
        pdf_cleanup_fonts(p);
        pdf_cleanup_outlines(p);
        pdf_cleanup_annot_params(p);
        pdf_cleanup_names(p);
        pdf_cleanup_colorspaces(p);
        pdf_cleanup_pattern(p);
        pdf_cleanup_shadings(p);
        pdf_cleanup_images(p);
        pdf_cleanup_xobjects(p);
        pdf_cleanup_extgstates(p);









        pdf_cleanup_stringlists(p);

        PDF_SET_STATE(p, pdf_state_object);
    }
}

static const pdc_defopt pdf_end_document_options[] =
{
    PDF_DOCUMENT_OPTIONS2
    PDC_OPT_TERMINATE
};

void
pdf__end_document(PDF *p, const char *optlist)
{
    /* pdf_document *doc; */

    /* check if there are any suspended pages left.
    */
    pdf_check_suspended_pages(p);

    /* get document pointer */
    /* doc = */ pdf_init_get_document(p);

    if (optlist && *optlist)
    {
        pdc_resopt *resopts = NULL;
        pdc_clientdata cdata;

        /* parsing option list */
        pdf_set_clientdata(p, &cdata);
        resopts = pdc_parse_optionlist(p->pdc, optlist,
                                  pdf_end_document_options, &cdata, pdc_true);

        /* get options */
        pdf_get_document_common_options(p, resopts, PDF_FC_END_DOCUMENT);

    }

    pdf_write_document(p);


    pdf_cleanup_document(p);

    if (!p->pdc->smokerun)
        pdc_logg_cond(p->pdc, 1, trc_api, "[End document]\n\n");
}

const char *
pdf__get_buffer(PDF *p, long *size)
{
    const char *ret;
    pdc_off_t llsize;


    ret = pdc_get_stream_contents(p->out, &llsize);

    if (llsize > LONG_MAX)
	pdc_error(p->pdc, PDF_E_DOC_GETBUF_2GB, 0, 0, 0, 0);

    *size = (long) llsize;
    return ret;
}




/*****************************************************************************/
/**               deprecated historical document functions                  **/
/*****************************************************************************/

void
pdf_set_flush(PDF *p, const char *flush)
{
    if (p->pdc->binding != NULL && strcmp(p->pdc->binding, "C++"))
        return;

    if (flush != NULL && *flush)
    {
        int i = pdc_get_keycode_ci(flush, pdf_flush_keylist);
        if (i != PDC_KEY_NOTFOUND)
        {
            pdf_document *doc = pdf_init_get_document(p);

            doc->flush = (pdc_flush_state) i;
            p->flush = doc->flush;
            return;
        }
        pdc_error(p->pdc, PDC_E_PAR_ILLPARAM, flush, "flush",
                  0, 0);
    }
}

void
pdf_set_uri(PDF *p, const char *uri)
{
    pdf_document *doc = pdf_init_get_document(p);

    if (doc->uri)
        pdc_free(p->pdc, doc->uri);
    doc->uri = pdc_strdup(p->pdc, uri);
}


void
pdf_set_compatibility(PDF *p, const char *compatibility)
{

    if (compatibility != NULL && *compatibility)
    {
        int i = pdc_get_keycode_ci(compatibility, pdf_compatibility_keylist);
        if (i != PDC_KEY_NOTFOUND)
        {
            pdf_document *doc = pdf_init_get_document(p);

            p->compatibility = i;
            doc->compatibility = i;
            p->pdc->compatibility = i;
            return;
        }
        pdc_error(p->pdc, PDC_E_PAR_ILLPARAM, compatibility, "compatibility",
                  0, 0);
    }
}

void
pdf_set_openaction(PDF *p, const char *openaction)
{
    pdf_document *doc = pdf_init_get_document(p);

    if (openaction != NULL && *openaction)
    {
        pdf_cleanup_destination(p, doc->dest);
        doc->dest = pdf_parse_destination_optlist(p, openaction, 1,
                                                  pdf_openaction);
    }
}

void
pdf_set_openmode(PDF *p, const char *openmode)
{
    int i;

    if (openmode == NULL || !*openmode)
        openmode = "none";

    i = pdc_get_keycode_ci(openmode, pdf_openmode_keylist);
    if (i != PDC_KEY_NOTFOUND)
    {
        pdf_document *doc = pdf_init_get_document(p);

        doc->openmode = (pdf_openmode) i;
    }
    else
        pdc_error(p->pdc, PDC_E_PAR_ILLPARAM, openmode, "openmode", 0, 0);
}

void
pdf_set_viewerpreference(PDF *p, const char *viewerpreference)
{
    static const char fn[] = "pdf_set_viewerpreference";
    pdf_document *doc = pdf_init_get_document(p);
    char *optlist;
    size_t nb1 = 0, nb2 = 0;

    if (doc->viewerpreferences)
        nb1 = strlen(doc->viewerpreferences) * sizeof(char *);
    nb2 = strlen(viewerpreference) * sizeof(char *);

    optlist = (char *) pdc_malloc(p->pdc, nb1 + nb2 + 2, fn);
    optlist[0] = 0;
    if (doc->viewerpreferences)
    {
        strcat(optlist, doc->viewerpreferences);
        strcat(optlist, " ");
    }
    strcat(optlist, viewerpreference);

    if (doc->viewerpreferences)
        pdc_free(p->pdc, doc->viewerpreferences);
    doc->viewerpreferences = optlist;
    doc->writevpdict |=
        pdf_parse_and_write_viewerpreferences(p, optlist, pdc_false);
}








