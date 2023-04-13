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
 * PDFlib actions handling routines
 *
 */

#define P_ACTIONS_C

#include "p_intern.h"

typedef enum
{
   pdf_allact       = -1,
   pdf_goto         = (1<<0),
   pdf_gotor        = (1<<1),
   pdf_launch       = (1<<2),
   pdf_uri          = (1<<3),
   pdf_hide         = (1<<4),
   pdf_named        = (1<<5),
   pdf_submitform   = (1<<6),
   pdf_resetform    = (1<<7),
   pdf_importdata   = (1<<8),
   pdf_javascript   = (1<<9),
   pdf_setocgstate  = (1<<10),
   pdf_trans        = (1<<11),
   pdf_goto3dview   = (1<<12),
   pdf_movie        = (1<<13)
}
pdf_actiontype;

static const pdc_keyconn pdf_action_pdfkeylist[] =
{
    {"GoTo",        pdf_goto},
    {"GoToR",       pdf_gotor},
    {"Launch",      pdf_launch},
    {"URI",         pdf_uri},
    {"Hide",        pdf_hide},
    {"Named",       pdf_named},
    {"SubmitForm",  pdf_submitform},
    {"ResetForm",   pdf_resetform},
    {"ImportData",  pdf_importdata},
    {"JavaScript",  pdf_javascript},
    {"SetOCGState", pdf_setocgstate},
    {"Trans",       pdf_trans},
    {"GoTo3DView",  pdf_goto3dview},
    {"Movie",       pdf_movie},
    {NULL, 0}
};


typedef enum
{
    /* values are identical with PDF values */
    pdf_exp_fdf         = (1<<1),
    pdf_exp_html        = (1<<2),
    pdf_exp_getrequest  = (1<<3),
    pdf_exp_coordinates = (1<<4),
    pdf_exp_xfdf        = (1<<5),
    pdf_exp_updates     = (1<<6),
    pdf_exp_annotfields = (1<<7),
    pdf_exp_pdf         = (1<<8),
    pdf_exp_onlyuser    = (1<<10),
    pdf_exp_exclurl     = (1<<11)
}
pdf_exportmethod;

/* allowed combinations of exportmethod keywords */
static pdf_exportmethod pdf_allfdf = (pdf_exportmethod)
                                     (pdf_exp_fdf |
                                      pdf_exp_updates |
                                      pdf_exp_exclurl |
                                      pdf_exp_annotfields |
                                      pdf_exp_onlyuser);

static pdf_exportmethod pdf_allhtml = (pdf_exportmethod)
                                      (pdf_exp_html |
                                       pdf_exp_getrequest |
                                       pdf_exp_coordinates);

static pdf_exportmethod pdf_allxfdf = pdf_exp_xfdf;

static pdf_exportmethod pdf_allpdf = (pdf_exportmethod)
                                     (pdf_exp_pdf |
                                      pdf_exp_getrequest);

static const pdc_keyconn pdf_exportmethod_keylist[] =
{
    {"fdf",         pdf_exp_fdf},
    {"html",        pdf_exp_html},
    {"xfdf",        pdf_exp_xfdf},
    {"pdf",         pdf_exp_pdf},
    {"getrequest",  pdf_exp_getrequest},
    {"coordinates", pdf_exp_coordinates},
    {"updates",     pdf_exp_updates},
    {"annotfields", pdf_exp_annotfields},
    {"onlyuser",    pdf_exp_onlyuser},
    {"exclurl",     pdf_exp_exclurl},
    {NULL, 0}
};

static const pdc_keyconn pdf_filename_keylist[] =
{
    {"filename",  (pdf_actiontype) (pdf_gotor | pdf_launch | pdf_importdata)},
    {"url",       (pdf_actiontype) (pdf_uri | pdf_submitform)},
    {NULL, 0}
};

#define PDF_MIN_MOVIEKEY 3

static const pdc_keyconn pdf_operation_pdfkeylist[] =
{
    /* Launch */
    {"open",    1},
    {"print",   2},

    /* Movie */
    {"Play",    PDF_MIN_MOVIEKEY},
    {"Stop",    4},
    {"Pause",   5},
    {"Resume",  6},

    {NULL, 0}
};


static const pdc_keyconn pdf_3dview_keylist[] =
{
    {NULL, 0}
};






#define PDF_LAYER_FLAG PDC_OPT_UNSUPP

#define PDF_JAVASCRIPT_FLAG PDC_OPT_UNSUPP
#define PDF_3DVIEW_FLAG PDC_OPT_UNSUPP

static const pdc_defopt pdf_create_action_options[] =
{
    /* deprecated */
    {"actionwarning", pdc_booleanlist, PDC_OPT_PDFLIB_7, 1, 1,
      0.0, 0.0, NULL},

    {"hypertextencoding", pdc_stringlist,  PDC_OPT_NONE, 1, 1,
      0.0, PDF_MAX_NAMESTRING, NULL},

    {"destination", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"destname", pdc_stringlist, PDC_OPT_IGNOREIF1, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"filename", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_FILENAMELEN, NULL},

    {"url", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"parameters", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"operation", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdf_operation_pdfkeylist},

    {"defaultdir", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"menuname", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"script", pdc_stringlist, PDF_JAVASCRIPT_FLAG, 1, 1,
      0.0, PDC_INT_MAX, NULL},

    {"scriptname", pdc_stringlist, PDF_JAVASCRIPT_FLAG, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"namelist", pdc_stringlist, PDC_OPT_NONE, 1, PDF_MAXARRAYSIZE,
      0.0, PDC_USHRT_MAX, NULL},

    {"exportmethod", pdc_keywordlist, PDC_OPT_BUILDOR, 1, 10,
      0.0, 0.0, pdf_exportmethod_keylist},

    {"newwindow", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"ismap", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"hide", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"exclude", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"submitemptyfields", pdc_booleanlist, PDC_OPT_PDC_1_4, 1, 1,
      0.0, 0.0, NULL},

    {"canonicaldate", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"layerstate", pdc_stringlist, PDF_LAYER_FLAG | PDC_OPT_EVENNUM, 1, 100,
      1.0, 8.0, NULL},

    {"preserveradio", pdc_booleanlist, PDF_LAYER_FLAG, 1, 1,
      0.0, 0.0, NULL},

    {"3dview", pdc_3dviewhandle, PDF_3DVIEW_FLAG, 1, 1,
      0.0, 0.0, pdf_3dview_keylist},

    {"target", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_USHRT_MAX, NULL},

    {"transition", pdc_keywordlist, PDC_OPT_PDC_1_5, 1, 1,
      0.0, 0.0, pdf_transition_keylist},

    {"duration", pdc_scalarlist, PDC_OPT_PDC_1_5, 1, 1,
      0.0, PDC_FLOAT_MAX, NULL},

    PDF_ERRORPOLICY_OPTION

    PDC_OPT_TERMINATE
};

typedef struct pdf_action_s
{
    pdc_id obj_id;
    pdf_actiontype atype;
    pdf_dest *dest;
    pdc_encoding hypertextencoding;
    char *filename;
    char *nativefilename;
    char *parameters;
    char *operation;
    char *defaultdir;
    char *menuname;
    char *script;
    char *scriptname;
    char **namelist;
    int nsnames;
    pdc_bool newwindow;
    pdc_bool ismap;
    pdc_bool hide;
    pdc_bool exclude;
    pdc_bool submitemptyfields;
    pdc_bool canonicaldate;
    pdf_exportmethod exportmethod;
    int transition;
    double duration;
}
pdf_action;

static void
pdf_reclaim_action(void *item)
{
    pdf_action *action = (pdf_action *) item;

    action->obj_id = PDC_BAD_ID;
    action->atype = (pdf_actiontype) 0;
    action->dest = NULL;
    action->hypertextencoding = pdc_invalidenc;
    action->filename = NULL;
    action->nativefilename = NULL;
    action->parameters = NULL;
    action->operation = NULL;
    action->defaultdir = NULL;
    action->menuname = NULL;
    action->script = NULL;
    action->scriptname = NULL;
    action->namelist = NULL;
    action->nsnames = 0;
    action->newwindow = pdc_undef;
    action->ismap = pdc_false;
    action->hide = pdc_true;
    action->exclude = pdc_false;
    action->submitemptyfields = pdc_false;
    action->canonicaldate = pdc_false;
    action->exportmethod = pdf_exp_fdf;
    action->transition = (int) trans_replace;
    action->duration = 1;
}

static void
pdf_release_action(void *context, void *item)
{
    PDF *p = (PDF *) context;
    pdf_action *action = (pdf_action *) item;

    pdf_cleanup_destination(p, action->dest);
    action->dest = NULL;

    if (action->filename)
    {
        pdc_free(p->pdc, action->filename);
        action->filename = NULL;
    }

    if (action->nativefilename)
    {
        pdc_free(p->pdc, action->nativefilename);
        action->nativefilename = NULL;
    }

    if (action->parameters)
    {
        pdc_free(p->pdc, action->parameters);
        action->parameters = NULL;
    }

    if (action->defaultdir)
    {
        pdc_free(p->pdc, action->defaultdir);
        action->defaultdir = NULL;
    }

    if (action->menuname)
    {
        pdc_free(p->pdc, action->menuname);
        action->menuname = NULL;
    }

    if (action->script)
    {
        pdc_free(p->pdc, action->script);
        action->script = NULL;
    }

    if (action->namelist)
    {
        pdc_cleanup_optstringlist(p->pdc, action->namelist, action->nsnames);
        action->namelist = NULL;
    }


}

static pdc_ced pdf_action_ced =
{
    sizeof(pdf_action), pdf_reclaim_action, pdf_release_action, NULL
};

static pdc_vtr_parms pdf_action_parms =
{
    0, 10, 10
};

static pdf_action *
pdf_new_action(PDF *p)
{
    pdf_action *result;

    if (p->actions == NULL)
        p->actions = pdc_vtr_new(p->pdc, &pdf_action_ced, p, &pdf_action_parms);

    result = pdc_vtr_incr(p->actions, pdf_action);
    result->hypertextencoding = p->hypertextencoding;
    return result;
}

void
pdf_delete_actions(PDF *p)
{
    if (p->actions != NULL)
    {
        pdc_vtr_delete(p->actions);
        p->actions = NULL;
    }
}

int
pdf_get_max_action(PDF *p)
{
    return (p->actions == NULL) ? -1 : pdc_vtr_size(p->actions) - 1;
}

static pdc_id pdf_write_action(PDF *p, pdf_action *action, pdc_id next_id);


static int
pdf_opt_effectless(PDF *p, const char *keyword, pdf_actiontype curratype,
                   pdf_actiontype intendatypes)
{
    if ((pdf_actiontype) !(intendatypes & curratype))
    {
        const char *type = pdc_get_keyword(curratype, pdf_action_pdfkeylist);
        pdc_warning(p->pdc, PDF_E_ACT_OPTIGNORE_FORTYPE, keyword,type, 0, 0);
        return 1;
    }
    return 0;
}

int
pdf__create_action(PDF *p, const char *type, const char *optlist)
{
    pdc_resopt *resopts = NULL;
    pdc_clientdata data;
    pdf_action *action;
    pdf_actiontype atype;
    pdf_dest *dest = NULL;
    pdc_bool verbose = pdc_true;
    pdc_bool hasdest = pdc_false;
    pdc_encoding htenc;
    int htcp;
    const char *keyword;
    char **strlist;
    int i, k, ns;

    if (type == NULL || *type == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "type", 0, 0, 0);

    k = pdc_get_keycode_ci(type, pdf_action_pdfkeylist);
    if (k == PDC_KEY_NOTFOUND)
        pdc_error(p->pdc, PDC_E_ILLARG_STRING, "type", type, 0, 0);
    atype = (pdf_actiontype) k;


    if (atype == pdf_javascript)
        pdc_error(p->pdc, PDF_E_UNSUPP_JAVASCRIPT, 0, 0, 0, 0);

    /* compatibility */
    if (p->compatibility < PDC_1_6 && atype == pdf_goto3dview)
    {
        pdc_error(p->pdc, PDC_E_PAR_VERSION, type,
                  pdc_get_pdfversion(p->pdc, PDC_1_6), 0, 0);
    }
    if (p->compatibility < PDC_1_5 &&
        (atype == pdf_setocgstate || atype == pdf_trans))
    {
        pdc_error(p->pdc, PDC_E_PAR_VERSION, type,
                  pdc_get_pdfversion(p->pdc, PDC_1_5), 0, 0);
    }

    /* new action */
    action = pdf_new_action(p);
    action->atype = atype;

    /* Parsing option list */
    pdf_set_clientdata(p, &data);
    resopts = pdc_parse_optionlist(p->pdc, optlist, pdf_create_action_options,
                                   &data, pdc_true);

    keyword = "actionwarning";
    pdc_get_optvalues(keyword, resopts, &verbose, NULL);
    verbose = pdf_get_errorpolicy(p, resopts, verbose);

    htenc = pdf_get_hypertextencoding_opt(p, resopts, &htcp, pdc_true);

    keyword = "destination";
    if (pdc_get_optvalues(keyword, resopts, NULL, &strlist))
    {
        if (!pdf_opt_effectless(p, keyword, atype,
                          (pdf_actiontype) (pdf_goto | pdf_gotor)))
        {
            action->dest = pdf_parse_destination_optlist(p, strlist[0],
                    (atype == pdf_goto) ? 0 : 1,
                    (atype == pdf_goto) ? pdf_locallink : pdf_remotelink);
            hasdest = pdc_true;
        }
    }
    else
    {
        keyword = "destname";
        if (atype == pdf_goto || atype == pdf_gotor)
            dest = pdf_get_option_destname(p, resopts, htenc, htcp);
        else if (pdc_get_optvalues(keyword, resopts, NULL, NULL))
            pdf_opt_effectless(p, keyword, atype,
                           (pdf_actiontype) (pdf_goto | pdf_gotor));
        if (dest)
        {
            action->dest = dest;
            hasdest = pdc_true;
        }
    }

    /* filename or url */
    for (i = 0; ; i++)
    {
        keyword = pdf_filename_keylist[i].word;
        if (keyword == NULL)
            break;

        if (!pdc_get_optvalues(keyword, resopts, NULL, NULL) ||
            pdf_opt_effectless(p, keyword, atype,
                               (pdf_actiontype) pdf_filename_keylist[i].code))
            continue;

        /* DON'T change order */

        /* native filename */
        if (!i)
            action->nativefilename = pdf_get_opt_filename(p, keyword, resopts,
                                                          htenc, htcp);

        /* Unicode filename */
        pdf_get_opt_textlist(p, keyword, resopts, htenc, htcp, pdc_true,
                             NULL, &action->filename, NULL);

        pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
    }

    keyword = "parameters";
    if (pdc_get_optvalues(keyword, resopts, NULL, NULL) &&
        !pdf_opt_effectless(p, keyword, atype, pdf_launch))
        action->parameters =
            (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    keyword = "operation";
    if (pdc_get_optvalues(keyword, resopts, &k, NULL) &&
        !pdf_opt_effectless(p, keyword, atype,
                            (pdf_actiontype) (pdf_launch | pdf_movie)))
    {
        if ((atype == pdf_launch && k >= PDF_MIN_MOVIEKEY) ||
            (atype == pdf_movie && k < PDF_MIN_MOVIEKEY))
        {
            pdc_error(p->pdc, PDC_E_OPT_ILLKEYWORD, keyword,
                      pdc_get_keyword(k, pdf_operation_pdfkeylist), 0, 0);
        }
        action->operation =
            (char *) pdc_get_keyword(k, pdf_operation_pdfkeylist);
    }

    keyword = "defaultdir";
    if (pdc_get_optvalues(keyword, resopts, NULL, NULL) &&
        !pdf_opt_effectless(p, keyword, atype, pdf_launch))
        action->defaultdir =
            (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);

    keyword = "menuname";
    if (pdc_get_optvalues(keyword, resopts, NULL, NULL) &&
        !pdf_opt_effectless(p, keyword, atype, pdf_named))
    {
        action->menuname =
            (char *) pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
    }

    keyword = "namelist";
    ns = pdc_get_optvalues(keyword, resopts, NULL, NULL);
    if (ns && !pdf_opt_effectless(p, keyword, atype,
                  (pdf_actiontype) (pdf_hide | pdf_submitform | pdf_resetform)))
    {
        action->namelist = (char **) pdc_save_lastopt(resopts, PDC_OPT_SAVEALL);
        action->nsnames = ns;
    }


    keyword = "exportmethod";
    if (pdc_get_optvalues(keyword, resopts, &k, NULL))
    {
        action->exportmethod = (pdf_exportmethod) k;
        if (!pdf_opt_effectless(p, keyword, atype, pdf_submitform))
        {
            if ((action->exportmethod & pdf_exp_fdf &&
                 (action->exportmethod | pdf_allfdf) != pdf_allfdf) ||
                (action->exportmethod & pdf_exp_html &&
                 (action->exportmethod | pdf_allhtml) != pdf_allhtml) ||
                (action->exportmethod & pdf_exp_xfdf &&
                 (action->exportmethod | pdf_allxfdf) != pdf_allxfdf) ||
                (action->exportmethod & pdf_exp_pdf &&
                 (action->exportmethod | pdf_allpdf) != pdf_allpdf))
            {
                pdc_error(p->pdc, PDC_E_OPT_ILLCOMB, keyword, 0, 0, 0);
            }
            if (action->exportmethod & pdf_exp_fdf)
                action->exportmethod = (pdf_exportmethod)
                    (action->exportmethod & ~pdf_exp_fdf);
        }
    }

    keyword = "newwindow";
    if (pdc_get_optvalues(keyword, resopts, &action->newwindow, NULL))
        pdf_opt_effectless(p, keyword, atype,
                           (pdf_actiontype) (pdf_gotor | pdf_launch));

    keyword = "ismap";
    if (pdc_get_optvalues(keyword, resopts, &action->ismap, NULL))
        pdf_opt_effectless(p, keyword, atype, pdf_uri);

    keyword = "hide";
    if (pdc_get_optvalues(keyword, resopts, &action->hide, NULL))
        pdf_opt_effectless(p, keyword, atype, pdf_hide);

    keyword = "exclude";
    if (pdc_get_optvalues(keyword, resopts, &action->exclude, NULL))
        pdf_opt_effectless(p, keyword, atype,
                (pdf_actiontype) (pdf_submitform | pdf_resetform));

    keyword = "submitemptyfields";
    if (pdc_get_optvalues(keyword, resopts, &action->submitemptyfields, NULL))
        pdf_opt_effectless(p, keyword, atype, pdf_submitform);

    keyword = "canonicaldate";
    if (pdc_get_optvalues(keyword, resopts, &action->canonicaldate, NULL))
        pdf_opt_effectless(p, keyword, atype, pdf_submitform);

    keyword = "transition";
    if (pdc_get_optvalues(keyword, resopts, &action->transition, NULL))
        pdf_opt_effectless(p, keyword, atype, pdf_trans);

    keyword = "duration";
    if (pdc_get_optvalues(keyword, resopts, &action->duration, NULL))
        pdf_opt_effectless(p, keyword, atype, pdf_trans);



    /* required options */
    keyword = NULL;
    if (!hasdest &&
        (atype == pdf_goto || atype == pdf_gotor))
        keyword = "destination";
    if (!action->filename &&
        (atype == pdf_gotor || atype == pdf_launch || atype == pdf_importdata))
        keyword = "filename";
    if (!action->menuname && atype == pdf_named)
        keyword = "menuname";
    if (!action->namelist && atype == pdf_hide)
        keyword = "namelist";
    if (!action->filename &&
        (atype == pdf_uri || atype == pdf_submitform))
        keyword = "url";
    if (keyword)
        pdc_error(p->pdc, PDC_E_OPT_NOTFOUND, keyword, 0, 0, 0);


    return pdf_get_max_action(p);
}


static pdc_id
pdf_write_action(PDF *p, pdf_action *action, pdc_id next_id)
{
    pdc_id ret_id = PDC_BAD_ID;
    int i, flags = 0;


    ret_id = pdc_begin_obj(p->out, PDC_NEW_ID);         /* Action object */
    pdc_begin_dict(p->out);                             /* Action dict */

    pdc_puts(p->out, "/Type/Action\n");
    pdc_printf(p->out, "/S/%s\n",
               pdc_get_keyword(action->atype, pdf_action_pdfkeylist));

    /* next action */
    if (next_id != PDC_BAD_ID)
	pdc_objref(p->out, "/Next", next_id);
    else
        action->obj_id = ret_id;

    /* destination */
    switch (action->atype)
    {
        case pdf_goto:
        case pdf_gotor:

        pdc_puts(p->out, "/D");
        pdf_write_destination(p, action->dest);

        default:
        break;
    }

    /* file specification */
    switch (action->atype)
    {
        case pdf_gotor:
        case pdf_launch:
        if (action->newwindow != pdc_undef)
            pdc_printf(p->out, "/NewWindow %s\n",
                PDC_BOOLSTR(action->newwindow));
        case pdf_importdata:

        if (action->parameters || action->operation || action->defaultdir)
        {
            /* Windows-specific launch parameters */
            pdc_puts(p->out, "/Win");
            pdc_begin_dict(p->out);                     /* Win dict */
            pdc_puts(p->out, "/F");
            pdf_put_pdffilename(p, action->nativefilename);
            pdc_puts(p->out, "\n");
            if (p->compatibility >= PDC_1_7)
            {
                pdc_puts(p->out, "/UF");
                pdf_put_pdfunifilename(p, action->filename);
                pdc_puts(p->out, "\n");
            }
            if (action->parameters)
            {
                pdc_puts(p->out, "/P");
                pdf_put_hypertext(p, action->parameters);
                pdc_puts(p->out, "\n");
                pdc_free(p->pdc, action->parameters);
                action->parameters = NULL;
            }
            if (action->operation)
            {
                pdc_puts(p->out, "/O");
                pdf_put_hypertext(p, action->operation);
                pdc_puts(p->out, "\n");
                action->operation = NULL;
            }
            if (action->defaultdir)
            {
                pdc_puts(p->out, "/D");
                pdf_put_hypertext(p, action->defaultdir);
                pdc_puts(p->out, "\n");
                pdc_free(p->pdc, action->defaultdir);
                action->defaultdir = NULL;
            }
            pdc_end_dict(p->out);                       /* Win dict */
        }
        else
        {
            pdc_puts(p->out, "/F");
            pdc_begin_dict(p->out);                     /* F dict */
            pdc_puts(p->out, "/Type/Filespec\n");
            pdc_puts(p->out, "/F");
            pdf_put_pdffilename(p, action->nativefilename);
            pdc_puts(p->out, "\n");
            if (p->compatibility >= PDC_1_7)
            {
                pdc_puts(p->out, "/UF");
                pdf_put_pdfunifilename(p, action->filename);
                pdc_puts(p->out, "\n");
            }
            pdc_end_dict(p->out);                       /* F dict */
        }

        default:
        break;
    }

    /* URI */
    switch (action->atype)
    {
        case pdf_uri:
        pdc_puts(p->out, "/URI");
        pdf_put_hypertext(p, action->filename);
        pdc_puts(p->out, "\n");

        /* IsMap */
        if (action->ismap == pdc_true)
            pdc_puts(p->out, "/IsMap true\n");

        default:
        break;
    }

    /* Named */
    switch (action->atype)
    {
        case pdf_named:
        pdc_puts(p->out, "/N");
	pdf_put_pdfname(p, action->menuname);
        pdc_puts(p->out, "\n");

        default:
        break;
    }

    /* name list */
    switch (action->atype)
    {
        case pdf_hide:
        if (action->hide == pdc_false)
            pdc_puts(p->out, "/H false\n");
        case pdf_submitform:
        case pdf_resetform:

        if (action->nsnames)
        {
            pdc_printf(p->out, "/%s",
                       (action->atype == pdf_hide) ? "T" : "Fields");
            pdc_begin_array(p->out);
            for (i = 0; i < action->nsnames; i++)
            {
                pdf_put_hypertext(p, action->namelist[i]);
                if (i < action->nsnames - 1)
                    pdc_puts(p->out, "\n");
                else
                    pdc_end_array(p->out);
            }
        }

        default:
        break;
    }

    /* URL */
    switch (action->atype)
    {
        case pdf_submitform:
        pdc_puts(p->out, "/F");
        pdc_begin_dict(p->out);                     /* F dict */
        pdc_puts(p->out, "/FS/URL\n");
        pdc_printf(p->out, "/F");
        pdf_put_hypertext(p, action->filename);
        pdc_puts(p->out, "\n");
        pdc_end_dict(p->out);                       /* F dict */

        default:
        break;
    }

    /* Trans */
    switch (action->atype)
    {
        case pdf_trans:
        pdc_puts(p->out, "/Trans");
        pdc_begin_dict(p->out);                     /* Trans dict */
        pdc_puts(p->out, "/Type/Trans\n");
        if (action->transition != trans_replace)
            pdc_printf(p->out, "/S/%s",
                pdc_get_keyword(action->transition, pdf_transition_pdfkeylist));
        if (action->duration > 0)
            pdc_printf(p->out, "/D %f\n", action->duration);
        pdc_end_dict(p->out);                       /* Trans dict */

        default:
        break;
    }

    /* Flags */
    switch (action->atype)
    {
        case pdf_submitform:
        flags = (int) action->exportmethod;
        if (action->submitemptyfields)
            flags |= (1<<1);
        if (action->canonicaldate)
            flags |= (1<<9);
        case pdf_resetform:

        if (action->exclude)
            flags |= (1<<0);
        if (flags)
            pdc_printf(p->out, "/Flags %d\n", flags);

        default:
        break;
    }

    /* Movie */


    pdc_end_dict(p->out);                              /* Action dict */
    pdc_end_obj(p->out);                               /* Action object */

    return ret_id;
}



/* ---- Annotations events ---- */

static const pdc_keyconn pdf_annotevent_keylist[] =
{
    {"activate",   0},
    {"enter",      1},
    {"exit",       2},
    {"down",       3},
    {"up",         4},
    {"focus",      5},
    {"blur",       6},
    {"open",       7},
    {"close",      8},
    {"visible",    9},
    {"invisible", 10},
    {NULL, 0}
};

static const pdc_keyconn pdf_annotevent_pdfkeylist[] =
{
    {"A",   0},
    {"E",   1},
    {"X",   2},
    {"D",   3},
    {"U",   4},
    {"Fo",  5},
    {"Bl",  6},
    {"PO",  7},
    {"PC",  8},
    {"PV",  9},
    {"PI", 10},
    {NULL,  0}
};

static int pdf_annotevent_beginjava = 99;

static const pdc_defopt pdf_annotevent_options[] =
{
    {"activate", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"enter", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"exit", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"down", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"up", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"focus", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"blur", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"open", pdc_actionhandle, PDC_OPT_PDC_1_5, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"close", pdc_actionhandle, PDC_OPT_PDC_1_5, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"visible", pdc_actionhandle, PDC_OPT_PDC_1_5, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"invisible", pdc_actionhandle, PDC_OPT_PDC_1_5, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    PDC_OPT_TERMINATE
};


/* ---- Bookmark events ---- */

static const pdc_keyconn pdf_bookmarkevent_keylist[] =
{
    {"activate",   0},
    {NULL, 0}
};

static const pdc_keyconn pdf_bookmarkevent_pdfkeylist[] =
{
    {"A", 0},
    {NULL, 0}
};

static int pdf_bookmarkevent_beginjava = 99;

static const pdc_defopt pdf_bookmarkevent_options[] =
{
    {"activate", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    PDC_OPT_TERMINATE
};


/* ---- Document events ---- */

static const pdc_keyconn pdf_documentevent_keylist[] =
{
    {"open",        0},
    {"didprint",    1},
    {"didsave",     2},
    {"willclose",   3},
    {"willprint",   4},
    {"willsave",    5},
    {NULL, 0}
};

static const pdc_keyconn pdf_documentevent_pdfkeylist[] =
{
    {"OpenAction", 0},
    {"DP",         1},
    {"DS",         2},
    {"WC",         3},
    {"WP",         4},
    {"WS",         5},
    {NULL, 0}
};

static int pdf_documentevent_beginjava = 1;

static const pdc_defopt pdf_documentevent_options[] =
{
    {"open", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"didprint", pdc_actionhandle, PDC_OPT_PDC_1_4, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"didsave", pdc_actionhandle, PDC_OPT_PDC_1_4, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"willclose", pdc_actionhandle, PDC_OPT_PDC_1_4, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"willprint", pdc_actionhandle, PDC_OPT_PDC_1_4, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"willsave", pdc_actionhandle, PDC_OPT_PDC_1_4, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    PDC_OPT_TERMINATE
};


/* ---- Page events ---- */

static const pdc_keyconn pdf_pageevent_keylist[] =
{
    {"",        0},
    {"open",    1},
    {"close",   2},
    {NULL, 0}
};

static const pdc_keyconn pdf_pageevent_pdfkeylist[] =
{
    {"",   0},
    {"O",  1},
    {"C",  2},
    {NULL, 0}
};

static int pdf_pageevent_beginjava = 99;

static const pdc_defopt pdf_pageevent_options[] =
{
    {"open", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    {"close", pdc_actionhandle, PDC_OPT_NONE, 1, PDC_USHRT_MAX,
      0.0, 0.0, NULL},

    PDC_OPT_TERMINATE
};


pdc_bool
pdf_parse_and_write_actionlist(PDF *p, pdf_event_object eventobj,
                     pdc_id *act_idlist, const char *optlist)
{
    const pdc_defopt *defopttable = NULL;
    const pdc_keyconn *keyconntable = NULL;
    pdc_resopt *resopts = NULL;
    pdc_clientdata data;
    pdc_id ret_id = PDC_BAD_ID;
    pdf_action *action = NULL;
    pdc_bool calcevent = pdc_false;
    const char *keyword, *type;
    char **strlist;
    int *actlist;
    int i, code, nsact, beginjava = 0;

    switch(eventobj)
    {

        case event_annotation:
        defopttable = pdf_annotevent_options;
        keyconntable = pdf_annotevent_keylist;
        beginjava = pdf_annotevent_beginjava;
        break;

        case event_bookmark:
        defopttable = pdf_bookmarkevent_options;
        keyconntable = pdf_bookmarkevent_keylist;
        beginjava = pdf_bookmarkevent_beginjava;
        break;

        case event_document:
        defopttable = pdf_documentevent_options;
        keyconntable = pdf_documentevent_keylist;
        beginjava = pdf_documentevent_beginjava;
        break;

        case event_page:
        defopttable = pdf_pageevent_options;
        keyconntable = pdf_pageevent_keylist;
        beginjava = pdf_pageevent_beginjava;
        break;

        default:
        break;
    }

    /* parsing option list */
    pdf_set_clientdata(p, &data);
    resopts = pdc_parse_optionlist(p->pdc, optlist, defopttable,
                                   &data, pdc_true);

    /* write actions and saving action ids */
    for (code = 0; ; code++)
    {
        keyword = pdc_get_keyword(code, keyconntable);
        if (keyword)
        {
            nsact = pdc_get_optvalues(keyword, resopts, NULL, &strlist);
            actlist = (int *) strlist;

            /* Not activate event */
            if (code && nsact)
            {
                /* additional action type check */
                for (i = 0; i < nsact; i++)
                {
                    action = (pdf_action *) &pdc_vtr_at(p->actions, actlist[i],
                                                        pdf_action);
                    if (code >= beginjava && action->atype != pdf_javascript)
                    {
                        type = pdc_get_keyword(action->atype,
                                               pdf_action_pdfkeylist);
                        pdc_error(p->pdc, PDF_E_ACT_BADACTTYPE,
                                  type, keyword, 0, 0);
                    }
                }

                /* saving calculation event */
                if (!strcmp(keyword, "calculate"))
                    calcevent = pdc_true;
            }

            /* write action objects */
            if (act_idlist != NULL)
            {
                ret_id = PDC_BAD_ID;
                if (nsact == 1)
                {
                    action = (pdf_action *) &pdc_vtr_at(p->actions, actlist[0],
                                                        pdf_action);
                    if (action->obj_id == PDC_BAD_ID)
                        ret_id = pdf_write_action(p, action, ret_id);
                    else
                        ret_id = action->obj_id;
                }
                else if (nsact > 1)
                {
                    for (i = nsact-1; i >= 0; i--)
                    {
                        action = (pdf_action *) &pdc_vtr_at(p->actions,
                                                        actlist[i], pdf_action);
                        ret_id = pdf_write_action(p, action, ret_id);
                    }
                }
                act_idlist[code] = ret_id;
            }
        }
        else
            break;
    }

    return calcevent;
}

pdc_bool
pdf_write_action_entries(PDF *p, pdf_event_object eventobj, pdc_id *act_idlist)
{
    const pdc_keyconn *keyconntable = NULL;
    const char *keyword;
    pdc_id act_id = PDC_BAD_ID;
    pdc_bool adict = pdc_false;
    pdc_bool aadict = pdc_false;
    int code;


    switch(eventobj)
    {

        case event_annotation:
        keyconntable = pdf_annotevent_pdfkeylist;
        break;

        case event_bookmark:
        keyconntable = pdf_bookmarkevent_pdfkeylist;
        break;

        case event_document:
        keyconntable = pdf_documentevent_pdfkeylist;
        break;

        case event_page:
        keyconntable = pdf_pageevent_pdfkeylist;
        break;

        default:
        break;
    }

    for (code = 0; ; code++)
    {
        keyword = pdc_get_keyword(code, keyconntable);
        if (keyword)
        {
            act_id = act_idlist[code];
            if (act_id != PDC_BAD_ID)
            {
                if (code && !aadict)
                {
                    pdc_puts(p->out, "/AA");
                    pdc_begin_dict(p->out);                 /* AA dict */
                    aadict = pdc_true;
                }
                else if (!code)
                    adict = pdc_true;
                pdc_printf(p->out, "/%s", keyword);
                pdc_objref_c(p->out, act_id);
            }
        }
        else
            break;
    }
    if (aadict)
        pdc_end_dict(p->out);                               /* AA dict */
    else if (adict)
        pdc_puts(p->out, "\n");

    return adict;
}
