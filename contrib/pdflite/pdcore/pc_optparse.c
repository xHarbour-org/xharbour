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
 * Parser options routines
 *
 */

#include "pc_util.h"
#include "pc_file.h"
#include "pc_geom.h"
#include "pc_ctype.h"

#define PDC_PCBITS_SIZE   32
#define PDC_MAX_PERCENTS  (8 * PDC_PCBITS_SIZE)

/* result of an option */
struct pdc_resopt_s
{
    int               numdef;  /* number of definitions */
    const pdc_defopt *defopt;  /* pointer to option definition */
    int               num;     /* number of parsed values */
    void             *val;     /* list of parsed values */
    char             *origval; /* original value as string */
    int               flags;   /* flags */
    char              pcbits[PDC_PCBITS_SIZE];  /* percentage bits */
    int               currind; /* index of current option */
    int               lastind; /* index of last option */
    pdc_bool          isutf8;  /* optionlist UTF-8 encoded */
};

/* sizes of option types. must be parallel to pdc_opttype */
static const size_t pdc_typesizes[] =
{
    sizeof (pdc_bool),
    sizeof (char *),
    sizeof (int),
    sizeof (int),
    sizeof (float),
    sizeof (double),
    sizeof (pdc_scalar),
    sizeof (int),
    sizeof (pdc_polyline),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
    sizeof (int),
};

static const pdc_keyconn pdc_handletypes[] =
{
    {"3ddata",     pdc_3ddatahandle},
    {"3dview",     pdc_3dviewhandle},
    {"action",     pdc_actionhandle},
    {"bookmark",   pdc_bookmarkhandle},
    {"color",      pdc_colorhandle},
    {"document",   pdc_documenthandle},
    {"font",       pdc_fonthandle},
    {"gstate",     pdc_gstatehandle},
    {"iccprofile", pdc_iccprofilehandle},
    {"image",      pdc_imagehandle},
    {"layer",      pdc_layerhandle},
    {"page",       pdc_pagehandle},
    {"pattern",    pdc_patternhandle},
    {"shading",    pdc_shadinghandle},
    {"table",      pdc_tablehandle},
    {"template",   pdc_templatehandle},
    {"textflow",   pdc_textflowhandle},
    {"string",     pdc_stringhandle},
    {NULL, 0}
};

int
pdc_get_keycode(const char *keyword, const pdc_keyconn *keyconn)
{
    int i;

    for (i = 0; keyconn[i].word != NULL; i++)
    {
        const char *s1 = keyword;
        const char *s2 = keyconn[i].word;

        for (; *s1; ++s1, ++s2)
        {
            if (*s1 != *s2)
                break;
        }

        if (*s1 == *s2)
            return keyconn[i].code;
    }

    return PDC_KEY_NOTFOUND;
}

int
pdc_get_keycode_ci(const char *keyword, const pdc_keyconn *keyconn)
{
    int i;

    for (i = 0; keyconn[i].word != NULL; i++)
    {
        const char *s1 = keyword;
        const char *s2 = keyconn[i].word;

        for (; *s1; ++s1, ++s2)
        {
            if (pdc_tolower(*s1) != pdc_tolower(*s2))
                break;
        }

        if (pdc_tolower(*s1) == pdc_tolower(*s2))
            return keyconn[i].code;
    }

    return PDC_KEY_NOTFOUND;
}

int
pdc_get_keycode_unique(const char *keyword, const pdc_keyconn *keyconn)
{
    int i, j;

    size_t len = strlen(keyword);

    for (i = 0; keyconn[i].word != NULL; i++)
    {
        if (!strncmp(keyword, keyconn[i].word, len))
        {
            for (j = i + 1; keyconn[j].word != 0; j++)
                if (!strncmp(keyword, keyconn[j].word, len))
                    return PDC_KEY_NOTUNIQUE;
            return keyconn[i].code;
        }
    }

    return PDC_KEY_NOTFOUND;
}

int
pdc_get_keymask_ci(pdc_core *pdc, const char *option,
                   const char *keywordlist, const pdc_keyconn *keyconn)
{
    char **keys = NULL;
    int nkeys, i, j, k = 0;

    nkeys = pdc_split_stringlist(pdc, keywordlist, NULL, 0, &keys);

    for (j = 0; j < nkeys; j++)
    {
        for (i = 0; keyconn[i].word != NULL; i++)
            if (!pdc_stricmp(keys[j], keyconn[i].word))
                break;

        if (keyconn[i].word == NULL)
        {
            const char *stemp = pdc_errprintf(pdc, "%.*s",
                                              PDC_ERR_MAXSTRLEN, keys[j]);
            pdc_cleanup_stringlist(pdc, keys);
            pdc_set_errmsg(pdc, PDC_E_OPT_ILLKEYWORD, option, stemp, 0, 0);
            return PDC_KEY_NOTFOUND;
        }

        k |= keyconn[i].code;
    }

    pdc_cleanup_stringlist(pdc, keys);
    return k;
}

/*
 * flags: PDC_INT_HEXADEC, PDC_INT_CASESENS
 */
int
pdc_get_keycode_num(pdc_core *pdc, const char *option, const char *i_keyword,
                    int flags, const pdc_keyconn *keyconn, int *o_num)
{
    static const char *fn = "pdc_get_keycode_num";
    char *keyword;
    int i, len, keycode;

    keyword = pdc_strdup_ext(pdc, i_keyword, 0, fn);
    len = (int) strlen(keyword);
    *o_num = -1;

    /* parse number */
    for (i = 0; i < len; i++)
    {
        if (pdc_isdigit(keyword[i]))
        {
            if (pdc_str2integer(&keyword[i], flags, o_num))
            {
                keyword[i] = 0;
            }
            else
            {
                pdc_set_errmsg(pdc, PDC_E_OPT_ILLINTEGER, option, &keyword[i],
                               0, 0);
            }
            break;
        }
    }

    if (flags & PDC_INT_CASESENS)
        keycode = pdc_get_keycode(keyword, keyconn);
    else
        keycode = pdc_get_keycode_ci(keyword, keyconn);

    if (keycode == PDC_KEY_NOTFOUND)
        pdc_set_errmsg(pdc, PDC_E_OPT_ILLKEYWORD, option, keyword, 0, 0);

    pdc_free(pdc, keyword);

    return keycode;
}

const char *
pdc_get_keyword(int keycode, const pdc_keyconn *keyconn)
{
    int i;

    for (i = 0; keyconn[i].word != NULL; i++)
    {
        if (keycode == keyconn[i].code)
            return keyconn[i].word;
    }

    return NULL;
}

const char *
pdc_get_int_keyword(const char *keyword, const pdc_keyconn *keyconn)
{
    int i;

    for (i = 0; keyconn[i].word != NULL; i++)
    {
        const char *s1 = keyword;
        const char *s2 = keyconn[i].word;

        for (; *s1; ++s1, ++s2)
        {
            if (pdc_tolower(*s1) != pdc_tolower(*s2))
                break;
        }

        if (pdc_tolower(*s1) == pdc_tolower(*s2))
            return keyconn[i].word;
    }

    return NULL;
}

void
pdc_cleanup_optstringlist(pdc_core *pdc, char **stringlist, int ns)
{
    int j;

    for (j = 0; j < ns; j++)
    {
        if (stringlist[j] != NULL)
            pdc_free(pdc, stringlist[j]);
    }
    pdc_free(pdc, stringlist);
}

static void
pdc_delete_optvalue(pdc_core *pdc, pdc_resopt *resopt)
{
    if (resopt->val && !(resopt->flags & PDC_OPT_SAVEALL))
    {
        int j;
        int ja = (resopt->flags & PDC_OPT_SAVE1ELEM) ? 1 : 0;

        if (resopt->defopt->type == pdc_stringlist)
        {
            char **s = (char **) resopt->val;
            for (j = ja; j < resopt->num; j++)
                if (s[j] != NULL)
                    pdc_free(pdc, s[j]);
        }
        else if (resopt->defopt->type == pdc_polylinelist)
        {
            pdc_polyline *pl = (pdc_polyline *) resopt->val;
            for (j = ja; j < resopt->num; j++)
                if (pl[j].p != NULL)
                    pdc_free(pdc, pl[j].p);
        }
        pdc_free(pdc, resopt->val);
        resopt->val = NULL;
    }
    if (resopt->origval && !(resopt->flags & PDC_OPT_SAVEORIG))
    {
        pdc_free(pdc, resopt->origval);
        resopt->origval = NULL;
    }
    resopt->num = 0;
}

static int
pdc_optname_compare(const void *a, const void *b)
{
    return (strcmp(((pdc_resopt *)a)->defopt->name,
                   ((pdc_resopt *)b)->defopt->name));
}

/* destructor function for freeing temporary memory */
static void
pdc_cleanup_optionlist_tmp(void *opaque, void *mem)
{
    if (mem)
    {
        pdc_core *pdc = (pdc_core *) opaque;
        pdc_resopt *resopt = (pdc_resopt *) mem;
        int i;

        for (i = 0; i < resopt[0].numdef; i++)
            pdc_delete_optvalue(pdc, &resopt[i]);
    }
}

pdc_resopt *
pdc_parse_optionlist(pdc_core *pdc, const char *optlist,
                     const pdc_defopt *defopt,
                     const pdc_clientdata *clientdata, pdc_bool verbose)
{
    static const char *fn = "pdc_parse_optionlist";
    pdc_bool logg5 = pdc_logg_is_enabled(pdc, 5, trc_optlist);
    const char *stemp1 = NULL, *stemp2 = NULL, *stemp3 = NULL, *s1, *s2;
    char **items = NULL, *keyword = NULL;
    char **values = NULL, *value = NULL, **strings = NULL;
    int i, j, k, nd, is, iss, it, iv, icoord;
    int numdef, nitems = 0, nvalues, ncoords = 0, errcode = 0;
    void *resval;
    double dz, maxval;
    int retval, iz;
    pdc_sint32 lz = 0;
    pdc_uint32 ulz = 0;
    size_t len;
    const pdc_defopt *dopt = NULL;
    pdc_resopt *resopt = NULL;
    pdc_bool ignore = pdc_false;
    pdc_bool boolval = pdc_false;
    pdc_bool tocheck = pdc_false;
    pdc_bool issorted = pdc_true;
    pdc_bool ishandle = pdc_true;
    pdc_bool isutf8 = pdc_false;

    pdc_logg_cond(pdc, 1, trc_optlist, "\n\tOption list: \"%T\"\n",
                      optlist ? optlist : "", 0);

    /* split option list */
    if (optlist != NULL)
    {
        nitems = pdc_split_stringlist(pdc, optlist, PDC_OPT_LISTSEPS,
                                      PDC_SPLIT_ISOPTLIST, &items);
        isutf8 = pdc_is_utf8_bytecode(optlist);
    }
    if (nitems < 0)
    {
        keyword = (char *) optlist;
        errcode = PDC_E_OPT_NOTBAL;
        goto PDC_OPT_SYNTAXERROR;
    }

    /* initialize result list */
    for (numdef = 0; defopt[numdef].name != NULL; numdef++)
    {
	/* */ ;
    }

    /* allocate temporary memory for option parser result struct */
    resopt = (pdc_resopt *) pdc_calloc_tmp(pdc, numdef * sizeof(pdc_resopt),
                                    fn, pdc, pdc_cleanup_optionlist_tmp);
    for (i = 0; i < numdef; i++)
    {
        resopt[i].numdef = numdef;
        resopt[i].defopt = &defopt[i];

        if (defopt[i].flags & PDC_OPT_IGNOREIF1 ||
            defopt[i].flags & PDC_OPT_IGNOREIF2 ||
            defopt[i].flags & PDC_OPT_REQUIRIF1 ||
            defopt[i].flags & PDC_OPT_REQUIRIF2 ||
            defopt[i].flags & PDC_OPT_REQUIRED)
            tocheck = pdc_true;

        if (i && issorted)
            issorted = (strcmp(defopt[i-1].name, defopt[i].name) <= 0) ?
                       pdc_true : pdc_false;
    }

    /* loop over all option list elements */
    for (is = 0; is < nitems; is++)
    {
        pdc_bool isequal = pdc_false;

        /* search keyword */
        boolval = pdc_undef;
        keyword = items[is];
        for (it = 0; it < numdef; it++)
        {
            s1 = keyword;
            s2 = defopt[it].name;

            /* if (!pdc_stricmp(keyword, defopt[it].name))
             *     isequal = pdc_true;
             */
            for (; *s1; ++s1, ++s2)
            {
                if (pdc_tolower(*s1) != pdc_tolower(*s2))
                    break;
            }
            if (pdc_tolower(*s1) == pdc_tolower(*s2))
                isequal = pdc_true;

            /* special handling for booleans */
            if (defopt[it].type == pdc_booleanlist)
            {
                if (isequal ||
                    (keyword[1] != 0 &&
                     !pdc_stricmp(&keyword[2], defopt[it].name)))
                {
                    iss = is + 1;
                    if (iss == nitems ||
                        (pdc_stricmp(items[iss], "true") &&
                         pdc_stricmp(items[iss], "false")))
                    {
                        i = pdc_strincmp(defopt[it].name, "no", 2) ? 0 : 2;
                        if (!pdc_strincmp(&keyword[i], "no", 2))
                        {
                            boolval = pdc_false;
                            break;
                        }
                        else if (isequal)
                        {
                            boolval = pdc_true;
                            break;
                        }
                    }
                }
            }

            if (isequal)
                break;
        }

        if (logg5)
            pdc_logg(pdc, "\t\t\toption \"%s\" specified: ", keyword);

        if (it == numdef)
        {
            errcode = PDC_E_OPT_UNKNOWNKEY;
            goto PDC_OPT_SYNTAXERROR;
        }

        /* initialize */
        dopt = &defopt[it];
        ignore = pdc_false;
        nvalues = 1;
        values = NULL;
        ishandle = pdc_true;

        /* compatibility */
        if (clientdata && clientdata->compatibility)
        {
            int compatibility = clientdata->compatibility;

            for (iv = PDC_1_3; iv <= PDC_X_X_LAST; iv++)
            {
                if (logg5 && (dopt->flags & (1L<<iv)))
                    pdc_logg(pdc, "(compatibility >= %s) ",
                             pdc_get_pdfversion(pdc, iv));

                if ((dopt->flags & (1L<<iv)) && compatibility < iv)
                {
                    if (logg5)
                        pdc_logg(pdc, "\n");
                    stemp2 = pdc_get_pdfversion(pdc, compatibility);
                    errcode = PDC_E_OPT_VERSION;
                    goto PDC_OPT_SYNTAXERROR;
                }
            }
        }

        /* not supported */
        if (dopt->flags & PDC_OPT_UNSUPP)
        {
            if (logg5)
                pdc_logg(pdc, "(unsupported)\n");

            keyword = (char *) dopt->name;
            errcode = PDC_E_OPT_UNSUPP;
            goto PDC_OPT_SYNTAXERROR;
        }

        /* parse values */
        if (boolval == pdc_undef)
        {
            is++;
            if (is == nitems)
            {
                errcode = PDC_E_OPT_NOVALUES;
                goto PDC_OPT_SYNTAXERROR;
            }
            if (!ignore)
            {
                if (dopt->type == pdc_stringlist &&
                    pdc_is_utf8_bytecode(items[is]))
                    resopt[it].flags |= PDC_OPT_ISUTF8;

                if (dopt->type != pdc_stringlist || dopt->maxnum > 1)
                    nvalues = pdc_split_stringlist(pdc, items[is],
                                    (dopt->flags & PDC_OPT_SUBOPTLIST) ?
                                    PDC_OPT_LISTSEPS : NULL,
                                    PDC_SPLIT_ISOPTLIST, &values);

                if (dopt->flags & PDC_OPT_DUPORIGVAL)
                    resopt[it].origval = pdc_strdup(pdc, items[is]);
            }
        }

        /* ignore */
        if (ignore) continue;

        /* number of values check */
        if (nvalues < dopt->minnum)
        {
            stemp2 = pdc_errprintf(pdc, "%d", dopt->minnum);
            errcode = PDC_E_OPT_TOOFEWVALUES;
            goto PDC_OPT_SYNTAXERROR;
        }
        else if (nvalues > dopt->maxnum)
        {
            stemp2 = pdc_errprintf(pdc, "%d", dopt->maxnum);
            errcode = PDC_E_OPT_TOOMANYVALUES;
            goto PDC_OPT_SYNTAXERROR;
        }

        /* number of values must be even */
        if (dopt->flags & PDC_OPT_EVENNUM && (nvalues % 2))
        {
            errcode = PDC_E_OPT_ODDNUM;
            goto PDC_OPT_SYNTAXERROR;
        }

        /* number of values must be odd */
        if (dopt->flags & PDC_OPT_ODDNUM && !(nvalues % 2))
        {
            errcode = PDC_E_OPT_EVENNUM;
            goto PDC_OPT_SYNTAXERROR;
        }

        /* deprecated option since PDFlib 7 */
        if (dopt->flags & PDC_OPT_PDFLIB_7)
        {
            pdc_logg_cond(pdc, 2, trc_api,
                  "[Option \"%s\" is deprecated since PDFlib 7]\n",
                  keyword);
        }

        /* option already exists */
        if (resopt[it].num)
        {
            pdc_delete_optvalue(pdc, &resopt[it]);
        }

        /* no values */
        if (!nvalues ) continue;

        /* maximal value */
        switch (dopt->type)
        {
            case pdc_3ddatahandle:
            maxval = clientdata->max3ddata;
            break;

            case pdc_3dviewhandle:
            maxval = clientdata->max3dview;
            break;

            case pdc_actionhandle:
            maxval = clientdata->maxaction;
            break;

            case pdc_bookmarkhandle:
            maxval = clientdata->maxbookmark;
            break;

            case pdc_colorhandle:
            maxval = clientdata->maxcolor;
            break;

            case pdc_documenthandle:
            maxval = clientdata->maxdocument;
            break;

            case pdc_fonthandle:
            maxval = clientdata->maxfont;
            break;

            case pdc_gstatehandle:
            maxval = clientdata->maxgstate;
            break;

            case pdc_iccprofilehandle:
            maxval = clientdata->maxiccprofile;
            break;

            case pdc_imagehandle:
            maxval = clientdata->maximage;
            break;

	    case pdc_layerhandle:
            maxval = clientdata->maxlayer;
            break;

            case pdc_pagehandle:
            maxval = clientdata->maxpage;
            break;

            case pdc_patternhandle:
            maxval = clientdata->maxpattern;
            break;

            case pdc_shadinghandle:
            maxval = clientdata->maxshading;
            break;

            case pdc_tablehandle:
            maxval = clientdata->maxtable;
            break;

            case pdc_templatehandle:
            maxval = clientdata->maxtemplate;
            break;

            case pdc_textflowhandle:
            maxval = clientdata->maxtextflow;
            break;

            case pdc_stringhandle:
            maxval = clientdata->maxstring;
            break;

            case pdc_polylinelist:
            ncoords = 0;

            default:
            maxval = dopt->maxval;
            ishandle = pdc_false;
            break;
        }

        /* allocate value array */
        resopt[it].val = pdc_calloc(pdc,
                            (size_t) (nvalues * pdc_typesizes[dopt->type]), fn);
        resopt[it].num = nvalues;
        resopt[it].currind = it;

        if (dopt->flags & PDC_OPT_PERCENT)
            memset(resopt[it].pcbits, 0, PDC_PCBITS_SIZE);

        if (logg5)
            pdc_logg(pdc, "{");

        /* analyze type */
        resval = resopt[it].val;
        for (iv = 0; iv < nvalues; iv++)
        {
            errcode = 0;
            if (dopt->maxnum > 1 && nvalues)
                value = values[iv];
            else
                value = items[is];
            if (logg5)
                pdc_logg(pdc, "%s{%T}", iv ? " " : "", value, 0);
            switch (dopt->type)
            {
                /* boolean list */
                case pdc_booleanlist:
                if (boolval == pdc_true || !pdc_stricmp(value, "true"))
                {
                    *(pdc_bool *) resval = pdc_true;
                }
                else if (boolval == pdc_false || !pdc_stricmp(value, "false"))
                {
                    *(pdc_bool *) resval = pdc_false;
                }
                else
                {
                    errcode = PDC_E_OPT_ILLBOOLEAN;
                }
                break;

                /* string list */
                case pdc_stringlist:
                if (dopt->flags & PDC_OPT_NOSPACES)
                {
                    if (pdc_split_stringlist(pdc, value, NULL, 0, &strings) > 1)
                        errcode = PDC_E_OPT_ILLSPACES;
                    pdc_cleanup_stringlist(pdc, strings);
                }
                if (!errcode)
                {
                    len = strlen(value);
                    dz = (double) len;
                    if (dz < dopt->minval)
                    {
                        stemp3 = pdc_errprintf(pdc, "%d", (int) dopt->minval);
                        errcode = PDC_E_OPT_TOOSHORTSTR;
                    }
                    else if (dz > maxval)
                    {
                        stemp3 = pdc_errprintf(pdc, "%d", (int) maxval);
                        errcode = PDC_E_OPT_TOOLONGSTR;
                    }

                    if (dopt->flags & PDC_OPT_CONVUTF8)
                    {
                        int flags = PDC_CONV_EBCDIC | PDC_CONV_WITHBOM;

                        if (isutf8 || (resopt[it].flags & PDC_OPT_ISUTF8))
                            flags |= PDC_CONV_ISUTF8;

                        *((char **) resval) =
                            pdc_convert_name(pdc, value, 0, flags);
                    }
                    else
                    {
                        *((char **) resval) = pdc_strdup(pdc, value);
                    }
                }
                break;

                /* keyword list */
                case pdc_keywordlist:
                if (dopt->flags & PDC_OPT_CASESENS)
                    iz = pdc_get_keycode(value, dopt->keylist);
                else
                    iz = pdc_get_keycode_ci(value, dopt->keylist);
                if (iz == PDC_KEY_NOTFOUND)
                {
                    errcode = PDC_E_OPT_ILLKEYWORD;
                }
                else
                {
                    *(int *) resval = iz;
                }
                break;

                /* character list */
                case pdc_unicharlist:
                iz = pdc_string2unicode(pdc, value, dopt->flags, dopt->keylist,
                                        pdc_false);
                if (iz < 0)
                {
                    errcode = PDC_E_OPT_ILLCHAR;
                    break;
                }
                dz = iz;
                if (dz < dopt->minval)
                {
                    stemp3 = pdc_errprintf(pdc, "%g", dopt->minval);
                    errcode = PDC_E_OPT_TOOSMALLVAL;
                }
                else if (dz > maxval)
                {
                    stemp3 = pdc_errprintf(pdc, "%g", maxval);
                    errcode = PDC_E_OPT_TOOBIGVAL;
                }
                *(int *) resval = iz;
                break;

                /* string list */
                case pdc_polylinelist:
                {
                    int np = pdc_split_stringlist(pdc, value, NULL, 0,
                                                  &strings);
                    pdc_polyline *pl = (pdc_polyline *) resval;

                    pl->np = np / 2;
                    pl->p = NULL;

                    /* number of coordinates must be even */
                    if (np % 2)
                    {
                        errcode = PDC_E_OPT_ODDNUM;
                        np = 0;
                    }

                    /* polyline must be a box */
                    else if ((dopt->flags & PDC_OPT_ISBOX) && np != 4)
                    {
                        errcode = PDC_E_OPT_ILLBOX;
                        np = 0;
                    }

                    /* polyline will be closed */
                    else if ((dopt->flags & PDC_OPT_CLOSEPOLY) && np <= 4)
                    {
                        errcode = PDC_E_OPT_ILLPOLYLINE;
                        np = 0;
                    }

                    /* polyline not empty */
                    if (np)
                    {
                        if (dopt->flags & PDC_OPT_CLOSEPOLY)
                            pl->np += 1;
                        pl->p = (pdc_vector *) pdc_malloc(pdc,
                                        pl->np * sizeof(pdc_vector), fn);

                        iz = PDC_KEY_NOTFOUND;
                        j = 0;
                        icoord = ncoords;
                        for (i = 0; i < np; i++)
                        {
                            char *sk = strings[i];

                            if (dopt->keylist)
                            {
                                /* optional keyword list */
                                if (dopt->flags & PDC_OPT_CASESENS)
                                    iz = pdc_get_keycode(sk, dopt->keylist);
                                else
                                    iz = pdc_get_keycode_ci(sk, dopt->keylist);
                            }
                            if (iz == PDC_KEY_NOTFOUND)
                            {
                                /* percentage */
                                if (dopt->flags & PDC_OPT_PERCENT)
                                {
                                    k = (int) strlen(sk) - 1;
                                    if (sk[k] == '%')
                                    {
                                        sk[k] = 0;
                                        if (ncoords < PDC_MAX_PERCENTS)
                                        {
                                            pdc_setbit(resopt[it].pcbits,
                                                       ncoords);
                                        }
                                        else
                                        {
                                            errcode = PDC_E_OPT_TOOMANYPERCVALS;
                                        }
                                    }
                                }

                                retval = pdc_str2double(sk, &dz);
                                if (!retval)
                                {
                                    errcode = PDC_E_OPT_ILLNUMBER;
                                }
                                else if (pdc_getbit(resopt[it].pcbits, ncoords))
                                {
                                    if (dopt->flags & PDC_OPT_PERCRANGE)
                                    {
                                        if (dz < 0)
                                            errcode = PDC_E_OPT_TOOSMALLPERCVAL;
                                        if (dz > 100)
                                            errcode = PDC_E_OPT_TOOBIGPERCVAL;
                                    }
                                    dz /= 100.0;
                                }
                            }
                            else
                            {
                                dz = (double) iz;
                            }

                            if (!(i % 2))
                            {
                                pl->p[j].x = dz;
                            }
                            else
                            {
                                pl->p[j].y = dz;
                                j++;
                            }
                            ncoords++;
                        }

                        if (dopt->flags & PDC_OPT_CLOSEPOLY)
                        {
                            pl->p[pl->np - 1] = pl->p[0];
                            if (pdc_getbit(resopt[it].pcbits, icoord))
                                pdc_setbit(resopt[it].pcbits, ncoords);
                            ncoords++;
                            if (pdc_getbit(resopt[it].pcbits, icoord + 1))
                                pdc_setbit(resopt[it].pcbits, ncoords);
                            ncoords++;
                        }
                    }
                    pdc_cleanup_stringlist(pdc, strings);
                }
                break;

                /* number list */
                case pdc_3ddatahandle:
                case pdc_3dviewhandle:
                case pdc_actionhandle:
                case pdc_bookmarkhandle:
                case pdc_colorhandle:
                case pdc_documenthandle:
                case pdc_fonthandle:
                case pdc_gstatehandle:
                case pdc_iccprofilehandle:
                case pdc_imagehandle:
                case pdc_layerhandle:
                case pdc_pagehandle:
                case pdc_patternhandle:
                case pdc_shadinghandle:
                case pdc_tablehandle:
                case pdc_templatehandle:
                case pdc_textflowhandle:
                case pdc_integerlist:
                case pdc_floatlist:
                case pdc_doublelist:
                case pdc_scalarlist:

                if (dopt->keylist &&
                    (!(dopt->flags & PDC_OPT_KEYLIST1) || !iv))
                {
                    /* optional keyword and/or allowed integer list */
                    if (dopt->flags & PDC_OPT_CASESENS)
                        iz = pdc_get_keycode(value, dopt->keylist);
                    else
                        iz = pdc_get_keycode_ci(value, dopt->keylist);
                    if (iz == PDC_KEY_NOTFOUND)
                    {
                        if (dopt->flags & PDC_OPT_INTLIST)
                        {
                            errcode = PDC_E_OPT_ILLINTEGER;
                            break;
                        }
                    }
                    else
                    {
                        switch (dopt->type)
                        {
                            default:
                            case pdc_integerlist:
                            *(int *) resval = iz;
                            break;

                            case pdc_floatlist:
                            *(float *) resval = (float) iz;
                            break;

                            case pdc_doublelist:
                            *(double *) resval = (double) iz;
                            break;

                            case pdc_scalarlist:
                            *(pdc_scalar *) resval = (pdc_scalar) iz;
                            break;
                        }
                        break;
                    }
                }

                /* percentage */
                if (dopt->flags & PDC_OPT_PERCENT)
                {
                    i = (int) strlen(value) - 1;
                    if (value[i] == '%')
                    {
                        value[i] = 0;
                        if (iv < PDC_MAX_PERCENTS)
                        {
                            pdc_setbit(resopt[it].pcbits, iv);
                        }
                        else
                        {
                            errcode = PDC_E_OPT_TOOMANYPERCVALS;
                        }
                    }
                }

                case pdc_stringhandle:

                if (dopt->type == pdc_floatlist ||
                    dopt->type == pdc_doublelist ||
                    dopt->type == pdc_scalarlist)
                {
                    retval = pdc_str2double(value, &dz);
                }
                else
                {
                    if (dopt->minval >= 0)
                    {
                        retval = pdc_str2integer(value, PDC_INT_UNSIGNED, &ulz);
                        dz = ulz;
                    }
                    else
                    {
                        retval = pdc_str2integer(value, 0, &lz);
                        dz = lz;
                    }

                    if (retval && ishandle && pdc->hastobepos &&
                        dopt->type != pdc_bookmarkhandle &&
                        dopt->type != pdc_stringhandle)
                    {
                        dz -= 1;
                        lz = (pdc_sint32) dz;
                        ulz = (pdc_uint32) dz;
                    }
                }
                if (!retval)
                {
                    errcode = PDC_E_OPT_ILLNUMBER;
                }
                else
                {
                    if (pdc_getbit(resopt[it].pcbits, iv))
                    {
                        if (dopt->flags & PDC_OPT_PERCRANGE)
                        {
                            if (dz < 0)
                                errcode = PDC_E_OPT_TOOSMALLPERCVAL;
                            if (dz > 100)
                                errcode = PDC_E_OPT_TOOBIGPERCVAL;
                        }
                        dz /= 100.0;
                    }

                    if (errcode == 0)
                    {
                        if (dz < dopt->minval)
                        {
                            if (ishandle)
                            {
                                stemp3 = pdc_get_keyword(dopt->type,
                                                         pdc_handletypes);
                                errcode = PDC_E_OPT_ILLHANDLE;
                            }
                            else
                            {
                                stemp3 = pdc_errprintf(pdc, "%g", dopt->minval);
                                errcode = PDC_E_OPT_TOOSMALLVAL;
                            }
                        }
                        else if (dz > maxval)
                        {
                            if (ishandle)
                            {
                                stemp3 = pdc_get_keyword(dopt->type,
                                                         pdc_handletypes);
                                errcode = PDC_E_OPT_ILLHANDLE;
                            }
                            else
                            {
                                stemp3 = pdc_errprintf(pdc, "%g", maxval);
                                errcode = PDC_E_OPT_TOOBIGVAL;
                            }
                        }
                        else if (dopt->flags & PDC_OPT_NOZERO &&
                                 fabs(dz) < PDC_FLOAT_PREC)
                        {
                            errcode = PDC_E_OPT_ZEROVAL;
                        }
                        else if (dopt->type == pdc_scalarlist)
                        {
                            *(pdc_scalar *) resval = dz;
                        }
                        else if (dopt->type == pdc_doublelist)
                        {
                            *(double *) resval = dz;
                        }
                        else if (dopt->type == pdc_floatlist)
                        {
                            *(float *) resval = (float) dz;
                        }
                        else
                        {
                            if (dopt->minval >= 0)
                                *(pdc_uint32 *) resval = ulz;
                            else
                                *(pdc_sint32 *) resval = lz;
                        }
                    }
                }
                break;
            }

            if (errcode)
            {
                stemp2 = pdc_errprintf(pdc, "%.*s", PDC_ERR_MAXSTRLEN, value);
                goto PDC_OPT_SYNTAXERROR;
            }

            /* increment value pointer */
            resval = (void *) ((char *)(resval) + pdc_typesizes[dopt->type]);
        }
        pdc_cleanup_stringlist(pdc, values);
        values = NULL;

        if (logg5)
            pdc_logg(pdc, "}\n");

        /* build OR bit pattern */
        if (dopt->flags & PDC_OPT_BUILDOR && nvalues > 1)
        {
            int *bcode = (int *) resopt[it].val;
            for (iv = 1; iv < nvalues; iv++)
            {
                bcode[0] |= bcode[iv];
            }
            resopt[it].num = 1;
        }
    }
    pdc_cleanup_stringlist(pdc, items);
    items = NULL;

    /* required and to be ignored options */
    for (is = 0; tocheck && is < numdef; is++)
    {
        /* to be ignored option */
        if (resopt[is].num)
        {
            nd = 0;
            if (defopt[is].flags & PDC_OPT_IGNOREIF1) nd = 1;
            if (defopt[is].flags & PDC_OPT_IGNOREIF2) nd = 2;
            for (it = is - 1; it >= is - nd && it >= 0; it--)
            {
                if (resopt[it].num)
                {
                    pdc_delete_optvalue(pdc, &resopt[is]);
                    if (verbose)
                        pdc_warning(pdc, PDC_E_OPT_IGNORE, defopt[is].name,
                                    defopt[it].name, 0, 0);
                }
            }
        }

        /* required option */
        if (!resopt[is].num &&
            ((defopt[is].flags & PDC_OPT_REQUIRED) ||
             (defopt[is].flags & PDC_OPT_REQUIRIF1 && resopt[is-1].num) ||
             (defopt[is].flags & PDC_OPT_REQUIRIF2 &&
              (resopt[is-1].num || resopt[is-2].num))))
        {
            keyword = (char *) defopt[is].name;
            errcode = PDC_E_OPT_NOTFOUND;
            goto PDC_OPT_SYNTAXERROR;
        }
    }

    /* is no sorted */
    if (!issorted)
    {
        qsort((void *)resopt, (size_t) numdef, sizeof(pdc_resopt),
              pdc_optname_compare);
    }

    /* global UTF-8 check after sort */
    if (isutf8)
        resopt[0].isutf8 = pdc_true;

    /* index of last got option */
    resopt[0].lastind = -1;

    /* protocol */
    if (pdc_logg_is_enabled(pdc, 1, trc_optlist))
    {
        for (is = 0; is < numdef; is++)
        {
            if (resopt[is].num)
                pdc_logg(pdc, "\tOption \"%s\": %d value%s found\n",
                         resopt[is].defopt->name, resopt[is].num,
                         resopt[is].num == 1 ? "" : "s");
            else if (logg5)
                pdc_logg(pdc, "\t\t\toption \"%s\" not specified\n",
                         resopt[is].defopt->name);
            for (iv = 0; iv < resopt[is].num; iv++)
            {
                switch (resopt[is].defopt->type)
                {
                    case pdc_booleanlist:
                    case pdc_keywordlist:
                    case pdc_integerlist:
                    case pdc_3ddatahandle:
                    case pdc_3dviewhandle:
                    case pdc_actionhandle:
                    case pdc_bookmarkhandle:
                    case pdc_colorhandle:
                    case pdc_documenthandle:
                    case pdc_fonthandle:
                    case pdc_gstatehandle:
                    case pdc_iccprofilehandle:
                    case pdc_imagehandle:
                    case pdc_layerhandle:
                    case pdc_pagehandle:
                    case pdc_patternhandle:
                    case pdc_shadinghandle:
                    case pdc_tablehandle:
                    case pdc_templatehandle:
                    case pdc_textflowhandle:
                    case pdc_stringhandle:
                    pdc_logg(pdc, "\tValue %d: %d\n",
                             iv + 1, *((int *) resopt[is].val + iv));
                    break;

                    case pdc_stringlist:
                    pdc_logg(pdc, "\tValue %d: \"%T\"\n",
                             iv + 1, *((char **) resopt[is].val + iv), 0);
                    break;

                    case pdc_floatlist:
                    pdc_logg(pdc, "\tValue %d: %f\n",
                             iv + 1, *((float *) resopt[is].val + iv));
                    break;

                    case pdc_doublelist:
                    pdc_logg(pdc, "\tValue %d: %f\n",
                             iv + 1, *((double *) resopt[is].val + iv));
                    break;

                    case pdc_scalarlist:
                    pdc_logg(pdc, "\tValue %d: %f\n",
                             iv + 1, *((pdc_scalar *) resopt[is].val + iv));
                    break;

                    case pdc_unicharlist:
                    pdc_logg(pdc, "\tValue %d: %d\n",
                             iv + 1, *((int *) resopt[is].val + iv));
                    break;

                    case pdc_polylinelist:
                    pdc_logg(pdc, "\t\t#%d: ", iv + 1);
                    {
                        pdc_polyline *pl = (pdc_polyline *) resopt[is].val + iv;

                        for (j = 0; j < pl->np; j++)
                            pdc_logg(pdc, "%f,%f  ", pl->p[j].x, pl->p[j].y);
                        pdc_logg(pdc, "\n");
                    }
                    break;
                }
            }
        }
    }

    return resopt;

    PDC_OPT_SYNTAXERROR:
    stemp1 = pdc_errprintf(pdc, "%.*s", PDC_ERR_MAXSTRLEN, keyword);
    pdc_cleanup_stringlist(pdc, items);
    pdc_cleanup_stringlist(pdc, values);

    pdc_set_errmsg(pdc, errcode, stemp1, stemp2, stemp3, 0);
    if (verbose)
        pdc_error(pdc, -1, 0, 0, 0, 0);

    return NULL;
}

int
pdc_get_optvalues(const char *keyword, pdc_resopt *resopt,
                  void *lvalues, char ***mvalues)
{
    pdc_resopt *ropt = NULL;
    void *values = NULL;
    int nvalues = 0;
    size_t nbytes;
    if (mvalues) *mvalues = NULL;

    if (resopt)
    {
        const char *s1, *s2;
        int i, cmp;
        int lo = 0;
        int hi = resopt[0].numdef;

        while (lo < hi)
        {
            i = (lo + hi) / 2;

            s1 = keyword;
            s2 = resopt[i].defopt->name;

            for (; *s1; ++s1, ++s2)
            {
                if (*s1 != *s2)
                    break;
            }
            cmp = (*s1 - *s2);

            /* keyword found */
            if (cmp == 0)
            {
                ropt = &resopt[i];
                nvalues = ropt->num;
                values = ropt->val;
                resopt[0].lastind = i;
                break;
            }

            if (cmp < 0)
                hi = i;
            else
                lo = i + 1;
        }
    }

    if (nvalues)
    {
        /* copy values */
        if (lvalues)
        {
            if (ropt->defopt->type == pdc_stringlist &&
                ropt->defopt->maxnum == 1)
            {
                strcpy((char *)lvalues, *((char **) values));
            }
            else
            {
                nbytes = (size_t) (nvalues * pdc_typesizes[ropt->defopt->type]);
                memcpy(lvalues, values, nbytes);
            }
        }

        /* copy pointer */
        if (mvalues)
        {
            *mvalues = (char **) values;
        }
    }

    return nvalues;
}

void *
pdc_save_lastopt(pdc_resopt *resopt, int flags)
{
    int i = resopt[0].lastind;

    if (i > -1 && resopt[i].num)
    {
        if (flags & PDC_OPT_SAVEALL)
        {
            resopt[i].flags |= PDC_OPT_SAVEALL;
            return resopt[i].val;
        }
        else if (resopt[i].defopt->type == pdc_stringlist &&
                 (flags & PDC_OPT_SAVE1ELEM))
        {
            char **s = (char **) resopt[i].val;
            resopt[i].flags |= PDC_OPT_SAVE1ELEM;
            return (void *) s[0];
        }
        else if (flags & PDC_OPT_SAVEORIG)
        {
            resopt[i].flags |= PDC_OPT_SAVEORIG;
            return (void *) resopt[i].origval;
        }
    }

    return NULL;
}

int
pdc_get_lastopt_index(pdc_resopt *resopt)
{
    int i = resopt[0].lastind;

    if (i > -1)
        return resopt[i].currind;
    else
        return -1;
}

pdc_bool
pdc_is_lastopt_percent(pdc_resopt *resopt, int ind)
{
    int i = resopt[0].lastind;

    if (i > -1 && resopt[i].num < PDC_MAX_PERCENTS)
        return pdc_getbit(resopt[i].pcbits, ind) ? pdc_true : pdc_false;
    else
        return pdc_false;
}

pdc_bool
pdc_is_lastopt_utf8(pdc_resopt *resopt)
{
    int i = resopt[0].lastind;

    if (i > -1)
        return resopt[0].isutf8 ||
               ((resopt[i].flags & PDC_OPT_ISUTF8) ? pdc_true : pdc_false);
    else
        return pdc_false;
}

int
pdc_get_opt_utf8strings(pdc_core *pdc, const char *keyword, pdc_resopt *resopt,
                        int flags, char ***strings)
{
    int ns = pdc_get_optvalues(keyword, resopt, NULL, strings);

    if (ns)
    {
        if (pdc_is_lastopt_utf8(resopt))
        {
            int i = resopt[0].lastind;
            pdc_resopt *ropt = &resopt[i];
            char **s = (char **) ropt->val;
            int j;

            for (j = 0; j < ropt->num; j++)
            {
                char *sb = pdc_strdup_withbom(pdc, s[j]);

                if (s[j] != NULL)
                    pdc_free(pdc, s[j]);
                s[j] = sb;
            }
        }

        pdc_save_lastopt(resopt, flags);
    }

    return ns;
}

const char *
pdc_get_opt_filename(pdc_core *pdc, const char *keyword, pdc_resopt *resopts)
{
    const char *filename = NULL;
    char **strlist;

    if (pdc_get_optvalues(keyword, resopts, NULL, &strlist))
    {
        pdc_bool isutf8 = pdc_is_lastopt_utf8(resopts);
        int flags = PDC_CONV_WITHBOM;

        if (isutf8)
            flags |= PDC_CONV_ISUTF8;

        filename = pdc_convert_filename(pdc, strlist[0], 0, keyword, flags);
    }

    return filename;
}

void
pdc_cleanup_optionlist(pdc_core *pdc, pdc_resopt *resopt)
{
    if (resopt != NULL)
        pdc_free_tmp(pdc, resopt);
}

const char *
pdc_get_handletype(pdc_opttype type)
{
    return pdc_get_keyword(type, pdc_handletypes);
}

