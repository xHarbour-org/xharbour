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
 * Resource routines
 *
 */

#ifndef PC_RESOURCE_H
#define PC_RESOURCE_H

#ifdef PC_RESOURCE_C
#if !defined(WIN32) && !defined(AS400) && !defined(MVS)

/* default SearchPath list for UNIX like systems
*/

/* %s: $HOME
*/
static const char *rootdirectories[] =
{
    "/usr/local",
    "%s",
    NULL
};

/*
 * first  %s: root directory
 * second %s: pdc->prodname
 * third  %s: pdc->version without revision: <major>.<minor>
 */
static const char *defsearchpathlist[] =
{
    "%s/PDFlib",
    "%s/PDFlib/%s",
    "%s/PDFlib/%s/%s",
    "%s/PDFlib/%s/%s/resource/cmap",
    "%s/PDFlib/%s/%s/resource/fonts",
    "%s/PDFlib/%s/%s/resource/icc",
    NULL
};

#endif /* !WIN32 && !AS400 && !MVS */
#endif /* PC_RESOURCE_C */

/* pdcore logg classes (maximal PDC_CLASSLIST_SIZE) */
typedef enum
{
    trc_other = 0,      /* other classes */
    trc_api,            /* API function call logging */
    trc_encoding,       /* encoding, cmap end textformat logging */
    trc_digsig,         /* digital signatures */
    trc_filesearch,     /* file search logging */
    trc_font,           /* font logging */
    trc_image,          /* image and template logging */
    trc_memory,         /* memory logging */
    trc_optlist,        /* optlist logging */
    trc_pcos,           /* pcos logging */
    trc_pdi,            /* pdi logging */
    trc_resource,       /* resource logging */
    trc_shadow,         /* shadow logging */
    trc_text,           /* text logging */
    trc_textflow,       /* textflow logging */
    trc_table,          /* table logging */
    trc_user,           /* user logging */
    trc_warning,        /* logging of disabled warnings */
    trc_wordfinder,     /* word finder logging */
    trc_xmp,            /* xmp logging */
    trc_zones,          /* zones logging */

    trc_numclasses      /* number of classes */
}
pdc_logg_class;

/* string code kinds */
typedef enum
{
    strform_readable,
    strform_readable0,
    strform_octal,
    strform_hexa,
    strform_java
}
pdc_strform_kind;


typedef struct pdc_res_s pdc_res;
typedef struct pdc_category_s pdc_category;
typedef struct pdc_reslist_s pdc_reslist;
typedef struct pdc_virtfile_s pdc_virtfile;
typedef struct pdc_loggdef_s pdc_loggdef;


/* -------------------------- resource handling ----------------------------- */

pdc_reslist *pdc_new_reslist(pdc_core *pdc);
void pdc_delete_reslist(pdc_core *pdc);
void pdc_set_resourcefile(pdc_core *pdc, const char *filename);
void pdc_add_resource_ext(pdc_core *pdc, const char *category,
        const char *resname, const char *resvalue, pdc_encoding enc,
        int codepage);
void pdc_add_resource(pdc_core *pdc, const char *category,
        const char *resname, const char *resvalue);
const char *pdc_find_resource(pdc_core *pdc, const char *category,
        const char *name);
const char *pdc_find_resource_nr(pdc_core *pdc, const char *category, int nr);
const char *pdc_get_resourcefile(pdc_core *pdc);



/* ----------------------- virtual file handling ---------------------------- */

void pdc__create_pvf(pdc_core *pdc, const char *filename,
        const void *data, size_t size, const char *optlist);
int pdc__delete_pvf(pdc_core *pdc, const char *filename);
void pdc_lock_pvf(pdc_core *pdc, const char *filename);
void pdc_unlock_pvf(pdc_core *pdc, const char *filename);
void pdc_delete_filesystem(pdc_core *pdc);


/* ----------------------- logging file handling ---------------------------- */

void pdc_delete_logg(pdc_core *pdc);
void pdc_set_logg_options(pdc_core *pdc, const char *optlist);
const char *pdc_print_loggstring(pdc_core *pdc, const char *str, int len);
pdc_bool pdc_enter_api_logg(pdc_core *pdc, const char *funame,
        pdc_bool enter_api, const char *fmt, va_list args);
void pdc_logg_exit_api(pdc_core *pdc, pdc_bool cleanup,
                           const char *fmt, ...);
void pdc_logg_enable(pdc_core *pdc, pdc_bool enable);
pdc_bool pdc_logg_is_enabled(pdc_core *pdc, int level, int pclass);
void pdc_logg(pdc_core *pdc, const char *fmt, ...);
void pdc_logg_cond(pdc_core *pdc, int level, int pclass,
                           const char *fmt, ...);
void pdc_logg_bitarr(pdc_core *pdc, const char *msg, const char *bitarr,
        int nbit);
void pdc_logg_hexdump(pdc_core *pdc, const char *msg,  const char *prefix,
        const char *text, int tlen);
void pdc_warning(pdc_core *pdc, int errnum, const char *parm1,
        const char *parm2, const char *parm3, const char *parm4);

void pdc_logg_unichar(pdc_core *pdc, int unichar, pdc_bool kfill,
        pdc_bool newline);
void pdc_logg_unitext(pdc_core *pdc, pdc_ushort *ustext, int len,
        pdc_bool newline);

int pdc_logg_getlevel(pdc_core *pdc, int pclass);

#endif  /* PC_RESOURCE_H */

