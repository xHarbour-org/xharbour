/*---------------------------------------------------------------------------*
 |              PDFlib - A library for generating PDF on the fly             |
 +---------------------------------------------------------------------------+
 | Copyright (c) 1997-2008 Thomas Merz and PDFlib GmbH. All rights reserved. |
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

#define PC_RESOURCE_C

#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #ifndef _CRT_SECURE_NO_WARNINGS
      #define _CRT_SECURE_NO_WARNINGS
   #endif
   #ifndef _CRT_SECURE_NO_DEPRECATE
      #define _CRT_SECURE_NO_DEPRECATE
   #endif
#endif

#include <errno.h>

#include "pc_util.h"
#include "pc_file.h"
#include "pc_resource.h"
#include "pc_ctype.h"

#if defined(WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#endif


/* -------------------------- resource handling ----------------------------- */

struct pdc_res_s
{
    char *name;
    char *value;
    pdc_res *prev;
    pdc_res *next;
};

struct pdc_category_s
{
    char *category;
    pdc_res *kids;
    pdc_category *next;
};

struct pdc_reslist_s
{
    pdc_category *resources;    /* anchor for the category resource list */
    pdc_bool filepending;       /* to read resource file is pending */
    char *filename;             /* name of the resource file */
};

typedef enum
{
    pdc_FontOutline,
    pdc_FontAFM,
    pdc_FontPFM,
    pdc_HostFont,
    pdc_Encoding,
    pdc_ICCProfile,
    pdc_StandardOutputIntent,
    pdc_SearchPath,
    pdc_CMap,
    pdc_GlyphList,
    pdc_CodeList
}
pdc_rescategory;

static const pdc_keyconn pdc_rescategories[] =
{
    {"FontOutline",          pdc_FontOutline},
    {"FontAFM",              pdc_FontAFM},
    {"FontPFM",              pdc_FontPFM},
    {"HostFont",             pdc_HostFont},
    {"Encoding",             pdc_Encoding},
    {"ICCProfile",           pdc_ICCProfile},
    {"StandardOutputIntent", pdc_StandardOutputIntent},
    {"SearchPath",           pdc_SearchPath},
    {"CMap",                 pdc_CMap},
    {"GlyphList",            pdc_GlyphList},
    {"CodeList",             pdc_CodeList},
    {NULL, 0}
};

#define RESOURCEFILEENVNAME     "%sRESOURCEFILE"

#ifndef MVS
#define DEFAULTRESOURCEFILE     "%s.upr"
#else
#define DEFAULTRESOURCEFILE     "upr"
#endif

#ifdef WIN32
#define REGISTRYKEY     "Software\\PDFlib\\%s\\%s"
#endif

pdc_reslist *
pdc_new_reslist(pdc_core *pdc)
{
    static const char fn[] = "pdc_new_reslist";

    pdc_reslist *resl = (pdc_reslist *) pdc_malloc(pdc, sizeof(pdc_reslist),fn);

    resl->resources = NULL;
    resl->filepending = pdc_true;
    resl->filename = NULL;

    pdc->reslist = resl;

    return resl;
}

static pdc_category *
pdc_delete_rescategory(pdc_core *pdc, pdc_category *prevcat, pdc_category *cat,
                       pdc_bool empty)
{
    pdc_category *nextcat;
    pdc_res *res, *lastres;

    for (res = cat->kids; res != NULL; /* */)
    {
        lastres = res;
        res = lastres->next;
        pdc_free(pdc, lastres->name);
        lastres->name = NULL;
        if (lastres->value)
        {
            pdc_free(pdc, lastres->value);
            lastres->value = NULL;
        }
        pdc_free(pdc, lastres);
    }
    nextcat = cat->next;

    if (empty)
    {
        cat->kids = NULL;
    }
    else
    {
        pdc_free(pdc, cat->category);
        cat->category = NULL;
        pdc_free(pdc, cat);
        cat = NULL;

        if (prevcat != NULL)
        {
            pdc_reslist *resl = pdc->reslist;

            if (prevcat != cat)
                prevcat->next = nextcat;
            else
                resl->resources = nextcat;
        }
    }

    return nextcat;
}

void
pdc_delete_reslist(pdc_core *pdc)
{
    pdc_reslist *resl = pdc->reslist;

    if (resl != NULL)
    {
        pdc_category *cat;

        for (cat = resl->resources; cat != NULL; /* */)
            cat = pdc_delete_rescategory(pdc, NULL, cat, pdc_false);

        if (resl->filename)
            pdc_free(pdc, resl->filename);

        pdc_free(pdc, resl);
        pdc->reslist = NULL;
    }
}

static pdc_reslist *
pdc_get_reslist(pdc_core *pdc)
{
    pdc_reslist *resl = pdc->reslist;

    if (resl == NULL)
        resl = pdc_new_reslist(pdc);

    return resl;
}

void
pdc_set_resourcefile(pdc_core *pdc, const char *filename)
{
    if (filename != NULL && *filename)
    {
        pdc_reslist *resl = pdc_get_reslist(pdc);

        if (resl->filename)
            pdc_free(pdc, resl->filename);

        resl->filename = pdc_strdup(pdc, filename);
        resl->filepending = pdc_true;
    }
}

const char *
pdc_get_resourcefile(pdc_core *pdc)
{
    pdc_reslist *resl = pdc_get_reslist(pdc);

    return (resl->filename);
}

static void
pdc_read_resourcefile(pdc_core *pdc, const char *filename)
{
    pdc_bool logg1 = pdc_logg_is_enabled(pdc, 1, trc_resource);
    pdc_reslist *resl = pdc_get_reslist(pdc);
    pdc_file *fp = NULL;
    char **linelist;
    char *line;
    char *category = NULL;
    const char *uprfilename = NULL;
    char tmpname[PDC_FILENAMELEN];
    char prodname[32];
    char prodversion[32];
    char *c;
#define BUFSIZE 2048
    char buffer[BUFSIZE];
#ifdef WIN32
    char regkey[128];
    HKEY hKey = NULL;
    DWORD size, lType;
#endif
    int il, ip, nlines = 0, nextcat, begin;

    if (logg1)
        pdc_logg(pdc, "\n\tSearching for resource file...\n");

    /* product name */
    strcpy(prodname, pdc->prodname);

    /* product version: <major>.<minor> */
    strcpy(prodversion, pdc->version);
    if (strlen(pdc->version))
    {
        c = strchr(prodversion, '.');
        if (c != NULL)
        {
            c++;
            if (*c)
            {
                c++;
                if (pdc_isdigit(*c))
                    c++;
                *c = 0;
            }
        }
    }

    if (logg1)
        pdc_logg(pdc, "\tProduct name=%s, version=%s\n",
                 prodname, prodversion);

#ifdef WIN32

/* don't add patchlevel's to registry searchpath */
#define stringiz1(x)  #x
#define stringiz(x)  stringiz1(x)

    sprintf(regkey, REGISTRYKEY, pdc->prodname, pdc->version);

#endif  /* WIN32 */

#ifdef AS400
    if (logg1)
        pdc_logg(pdc, "\tSet AS400 default SearchPath entries\n");

    sprintf(buffer, "/%s/%s/fonts", prodname, pdc->version);
    pdc_add_resource(pdc, "SearchPath", buffer, "");

    sprintf(buffer, "/%s/%s/bind/data", prodname, pdc->version);
    pdc_add_resource(pdc, "SearchPath", buffer, "");
#endif  /* AS400 */

#ifdef MVS
    (void) buffer;
    (void) ip;
#endif

#ifdef WIN32
    /* process registry entries */
    if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, regkey, 0L,
        (REGSAM) KEY_QUERY_VALUE, &hKey) == ERROR_SUCCESS)
    {
        if (logg1)
            pdc_logg(pdc, "\tRead registry key \"%s\":\n", regkey);

        size = BUFSIZE - 2;
        if (RegQueryValueExA(hKey, "searchpath", (LPDWORD) NULL,
                             &lType, (LPBYTE) buffer, &size)
            == ERROR_SUCCESS && *buffer)
        {
            char **pathlist;
            int np;

            if (logg1)
                pdc_logg(pdc, "\tsearchpath entry: \"%s\"\n", buffer);

            np = pdc_split_stringlist(pdc, buffer, ";", 0, &pathlist);
            for (ip = 0; ip < np; ip++)
            {
                /* TODO: we should only accessible directories */
                pdc_add_resource(pdc, "SearchPath", pathlist[ip], "");
            }
            pdc_cleanup_stringlist(pdc, pathlist);
        }

        RegCloseKey(hKey);
    }
#endif  /* WIN32 */

#if !defined(WIN32) && !defined(AS400) && !defined(MVS)
    if (logg1)
        pdc_logg(pdc, "\tSet UNIX default SearchPath entries\n");

    for (il = 0; rootdirectories[il] != NULL; il++)
    {
        const char *home = pdc_getenv_filename(pdc, "HOME");

        if (home != NULL)
             sprintf(tmpname, rootdirectories[il], home);
        else
             strcpy(tmpname, rootdirectories[il]);

        /* we allow only accessible root directories */
        if (access(tmpname, X_OK) != -1)
        {
            for (ip = 0; defsearchpathlist[ip] != NULL; ip++)
            {
                sprintf(buffer, defsearchpathlist[ip],
                        tmpname, prodname, prodversion);
                pdc_add_resource(pdc, "SearchPath", buffer, "");
            }
        }
    }
#endif /* !WIN32 && !AS400 && !MVS */

    /* searching for name of upr file */
    uprfilename = filename;
    if (uprfilename == NULL || *uprfilename == '\0')
    {
        /* upr file name via environment variable */
        sprintf(tmpname, RESOURCEFILEENVNAME, pdc->prodname);
        pdc_strtoupper(tmpname);
        uprfilename = pdc_getenv(pdc, tmpname);

#ifdef WIN32
        /* registry upr file name */
        if (uprfilename == NULL || *uprfilename == '\0')
        {
            if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, regkey, 0L,
                (REGSAM) KEY_QUERY_VALUE, &hKey) == ERROR_SUCCESS)
            {
                size = BUFSIZE - 2;
                if (RegQueryValueExA(hKey, "resourcefile", (LPDWORD) NULL,
                                     &lType, (LPBYTE) buffer, &size)
                    == ERROR_SUCCESS && *buffer)
                {
                    if (logg1)
                        pdc_logg(pdc, "\tresourcefile entry: \"%s\"\n", buffer);

                    uprfilename = buffer;
                }

                RegCloseKey(hKey);
            }
        }
#endif  /* WIN32 */

        /* default upr file name */
        if (uprfilename == NULL || *uprfilename == '\0')
        {
            sprintf(tmpname, DEFAULTRESOURCEFILE, pdc->prodname);
            uprfilename = pdc_strtolower(tmpname);

            /* user-supplied upr file */
            fp = pdc_fsearch_fopen(pdc, uprfilename, NULL, "UPR ", 0);
            if (fp == NULL)
            {
                uprfilename = NULL;
            }
        }
    }

    if (uprfilename != NULL && *uprfilename != '\0')
    {
        char *resfilename = resl->filename;

        if (logg1)
            pdc_logg(pdc, "\tRead resource file \"%s\":\n", uprfilename);

        resl->filename = pdc_strdup(pdc, uprfilename);
        if (resfilename)
            pdc_free(pdc, resfilename);

        /* read upr file */
        if (fp == NULL)
        {
            fp = pdc_fsearch_fopen(pdc, resl->filename, NULL, "UPR ",
                                   PDC_FILE_TEXT);
            if (fp == NULL)
                pdc_error(pdc, -1, 0, 0, 0, 0);
        }

        nlines = pdc_read_textfile(pdc, fp, 0, &linelist);
        pdc_fclose(fp);

        if (nlines)
        {
            /* Lines loop */
            begin = 1;
            nextcat = 0;
            for (il = 0; il < nlines; il++)
            {
                line = linelist[il];

                /* Next category */
                if (line[0] == '.' && strlen(line) == 1)
                {
                    begin = 0;
                    nextcat = 1;
                    continue;
                }

                /* Skip category list */
                if (begin) continue;

                /* Category expected */
                if (nextcat)
                {
                    /* Ressource Category */
                    category = line;
                    nextcat = 0;
                    continue;
                }

                /* Add resource */
                if (strlen(line))
                    pdc_add_resource(pdc, category, line, NULL);
            }

            pdc_cleanup_stringlist(pdc, linelist);
        }
    }
}

/*
 * pdc_add_resource_ext add a new resource to the resource data base
 * for specified resource category.
 *
 * resvalue == NULL:
 * resname string has the format "resname [= resvalue]"
 * Then string splitting has to be performed.
 * [EBCDIC-]UTF-8 must be specified by a BOM: ﻿resname = resvalue
 *
 * Otherwise resname and resvalue are [EBCDIC-]UTF-8 encoded.
 *
 */
void
pdc_add_resource_ext(pdc_core *pdc, const char *category, const char *resname,
                     const char *resvalue, pdc_encoding enc, int codepage)
{
    static const char fn[] = "pdc_add_resource";
    pdc_bool logg1 = pdc_logg_is_enabled(pdc, 1, trc_resource);
    pdc_reslist *resl = pdc_get_reslist(pdc);
    pdc_rescategory rescat;
    pdc_category *cat = NULL, *lastcat = NULL;
    pdc_res *res = NULL, *lastres = NULL;
    char *resnamutf8 = NULL;
    char *resvalutf8 = NULL;
    int resnamflags = PDC_CONV_EBCDIC | PDC_CONV_TMPALLOC;
    int resvalflags = PDC_CONV_EBCDIC | PDC_CONV_TMPALLOC;
    int k;

    if (logg1)
    {
        if (!resvalue || !strlen(resvalue))
            pdc_logg(pdc, "\tAdd \"%s\" to resource category \"%s\"\n",
                     resname, category);
        else
            pdc_logg(pdc, "\tAdd \"%s=%s\" to resource category \"%s\"\n",
                     resname, resvalue, category);
    }

    /* We no longer raise an error but silently ignore unknown categories */
    k = pdc_get_keycode_ci(category, pdc_rescategories);
    if (k == PDC_KEY_NOTFOUND)
    {
        pdc_warning(pdc, PDC_E_RES_BADCAT, category, 0, 0, 0);
        return;
    }
    rescat = (pdc_rescategory) k;

    /* Read resource configuration file if it is pending */
    if (resl->filepending)
    {
        resl->filepending = pdc_false;
        pdc_read_resourcefile(pdc, resl->filename);
    }

    /* Find start of this category's resource list, if the category exists */
    lastcat = resl->resources;
    for (cat = lastcat; cat != NULL; cat = cat->next)
    {
        if (!pdc_stricmp(cat->category, category))
            break;
        lastcat = cat;
    }
    if (cat == NULL)
    {
        cat = (pdc_category *) pdc_malloc(pdc, sizeof(pdc_category), fn);
        cat->category = pdc_strdup(pdc, category);
        cat->kids = NULL;
        cat->next = NULL;
        if (lastcat != NULL)
            lastcat->next = cat;
        else
            resl->resources = cat;
    }

    /* resvalue */
    if (resvalue == NULL)
    {
        char **strlist = NULL;

        /* splitting of resname string */
        int ns = pdc_split_stringlist(pdc, resname, "=", 0, &strlist);

        if (ns >= 1)
            pdc_str2trim(strlist[0]);
        if (ns == 2)
            pdc_str2trim(strlist[1]);
        if (ns > 2 ||
            (rescat != pdc_SearchPath && (ns == 0 || !strlen(strlist[0]))))
        {
            pdc_cleanup_stringlist(pdc, strlist);
            pdc_error(pdc, PDC_E_RES_BADRES, resname, category, 0, 0);
        }

        /* resource name */
        if (ns > 0)
        {
            if (pdc_is_utf8_bytecode(resname))
                resnamflags |= PDC_CONV_ISUTF8;
            resnamutf8 = pdc_convert_name_ext(pdc, strlist[0], 0,
                                              enc, codepage, resnamflags);
        }

        /* resource value */
        if (ns == 2)
        {
            resvalflags = resnamflags;
            resvalutf8 = pdc_convert_name_ext(pdc, strlist[1], 0,
                                              enc, codepage, resvalflags);
        }
        else
        {
            resvalutf8 = pdc_strdup_ext(pdc, "", PDC_CONV_TMPALLOC, fn);
        }
        pdc_cleanup_stringlist(pdc, strlist);
    }
    else
    {
        resnamflags |= PDC_CONV_ISUTF8;
        resnamutf8 = pdc_convert_name_ext(pdc, resname, 0,
                                          enc, codepage, resnamflags);

        resvalflags |= PDC_CONV_ISUTF8;
        resvalutf8 = pdc_convert_name_ext(pdc, resvalue, 0,
                                          enc, codepage, resvalflags);
    }

    switch (rescat)
    {
        case pdc_FontOutline:
        case pdc_FontAFM:
        case pdc_FontPFM:
        case pdc_HostFont:
        case pdc_Encoding:
        case pdc_ICCProfile:
        case pdc_CMap:
        case pdc_GlyphList:
        case pdc_CodeList:
        {
            if (!strlen(resnamutf8) || !strlen(resvalutf8))
            {
                if (resvalue == NULL)
                    pdc_error(pdc, PDC_E_RES_BADRES, resname, category, 0, 0);
                else
                    pdc_error(pdc, PDC_E_RES_BADRES2, resname, resvalue,
                              category, 0);
            }

            /* file name check */
            resvalutf8 = pdc_check_filename(pdc, resvalutf8);
        }
        break;

        case pdc_SearchPath:
        {
            if (strlen(resvalutf8))
            {
                pdc_error(pdc, PDC_E_RES_BADRES, resname, category, 0, 0);
            }

            if (resvalutf8 != NULL)
            {
                pdc_free_tmp(pdc, resvalutf8);
                resvalutf8 = NULL;
            }

            /* file name check */
            if (resnamutf8 != NULL && strlen(resnamutf8))
            {
                resnamutf8 = pdc_check_filename(pdc, resnamutf8);
            }
            else
            {
                /* delete all entries */
                if (resnamutf8 != NULL)
                    pdc_free_tmp(pdc, resnamutf8);
                pdc_delete_rescategory(pdc, lastcat, cat, pdc_true);

                if (logg1)
                    pdc_logg(pdc, "\tResource category \"%s\" removed\n",
                             category);

                return;
            }
        }
        break;

        case pdc_StandardOutputIntent:
        break;
    }

    /* Find resource name in resource list */
    lastres = NULL;
    for (res = cat->kids; res != NULL; res = res->next)
    {
        if (!strcmp(res->name, resnamutf8))
            break;
        lastres = res;
    }

    /* New resource */
    if (res == NULL)
    {
        res = (pdc_res *) pdc_calloc(pdc, sizeof(pdc_res), fn);
        if (lastres)
            lastres->next = res;
        else
            cat->kids = res;
        res->prev = lastres;
        res->name = pdc_strdup(pdc, resnamutf8);
    }
    else
    {
        pdc_free_tmp(pdc, resnamutf8);
    }

    /* New value */
    if (res->value)
        pdc_free(pdc, res->value);
    res->value = pdc_strdup(pdc, resvalutf8);

    if (logg1)
    {
        if (res->value && strlen(res->value))
            pdc_logg(pdc, "\tNew category.resource: \"%s.%s = %s\"\n",
                     category, res->name, res->value);
        else
            pdc_logg(pdc, "\tNew category.resource: \"%s.%s\"\n",
                     category, res->name);
    }
}

void
pdc_add_resource(pdc_core *pdc, const char *category, const char *resname,
                 const char *resvalue)
{
    pdc_add_resource_ext(pdc, category, resname, resvalue, pdc_invalidenc, 0);
}

const char *
pdc_find_resource(pdc_core *pdc, const char *category, const char *name)
{
    pdc_reslist *resl = pdc_get_reslist(pdc);
    pdc_category *cat;
    pdc_res *res;

    /* Read resource configuration file if it is pending */
    if (resl->filepending)
    {
        resl->filepending = pdc_false;
        pdc_read_resourcefile(pdc, resl->filename);
    }

    for (cat = resl->resources; cat != (pdc_category *) NULL; cat = cat->next)
    {
        if (!pdc_stricmp(cat->category, category))
        {
            for (res = cat->kids; res != (pdc_res *)NULL; res = res->next)
            {
                if (!strcmp(res->name, name))
                {
                    if (pdc_logg_is_enabled(pdc, 1, trc_resource))
                    {
                        const char *resval = res->name, *separ = "";

                        if (res->value && strlen(res->value))
                        {
                            resval = res->value;
                            separ = " = ";
                        }

                        pdc_logg(pdc,
                                 "\tFound category.resource: \"%s.%s%s%s\"\n",
                                 category, res->name, separ, resval);
                    }

                    return res->value;
                }
            }
        }
    }

    return NULL;
}

const char *
pdc_find_resource_nr(pdc_core *pdc, const char *category, int nr)
{
    pdc_reslist *resl = pdc_get_reslist(pdc);
    pdc_category *cat;
    pdc_rescategory rescat;
    pdc_res *res;
    int n = 0;

    /* Read resource configuration file if it is pending */
    if (resl->filepending)
    {
        resl->filepending = pdc_false;
        pdc_read_resourcefile(pdc, resl->filename);
    }

    rescat = (pdc_rescategory) pdc_get_keycode_ci(category, pdc_rescategories);

    for (cat = resl->resources; cat != (pdc_category *) NULL; cat = cat->next)
    {
        if (!pdc_stricmp(cat->category, category))
        {
            for (res = cat->kids; res != (pdc_res *)NULL; res = res->next)
            {
                n++;
                if (n == nr)
                {
                    char *resname = (char *) "", *resval = res->name;
                    const char *separ = "", *retval;
                    pdc_bool tobefree = pdc_false;

                    if (res->value && strlen(res->value))
                    {
                        resname = res->name;
                        resval = res->value;
                        separ = "=";
                    }

                    pdc_logg_cond(pdc, 1, trc_resource,
                                      "\tFound %d. category.resource: "
                                      "\"%s.%s%s%s\"\n",
                                      nr, category, resname, separ, resval);

                    /* conversion of host encoded file names back to UTF-8 */
                    switch (rescat)
                    {
                        case pdc_StandardOutputIntent:
                        break;

                        default:
                        resval = pdc_get_filename(pdc, resval);
                        tobefree = pdc_true;
                        break;
                    }

                    retval =
                        pdc_errprintf(pdc, "%s%s%s", resname, separ, resval);

                    if (tobefree)
                        pdc_free_tmp(pdc, resval);

                    return retval;
                }
            }
        }
    }

    return "";
}


/* ----------------------- virtual file handling ---------------------------- */

struct pdc_virtfile_s
{
    char *name;
    const void *data;
    size_t size;
    pdc_bool iscopy;
    int lockcount;
    pdc_virtfile *next;
};

static pdc_virtfile *
pdc_find_pvf(pdc_core *pdc, const char *filename, pdc_virtfile **lastvfile)
{
    pdc_virtfile *vfile;
    pdc_virtfile *filesystem = pdc->filesystem;

    if (lastvfile != NULL)
        *lastvfile = NULL;
    for (vfile = filesystem; vfile != NULL; vfile = vfile->next)
    {
        if (!strcmp(vfile->name, filename))
        {
            pdc_logg_cond(pdc, 1, trc_filesearch,
                "\n\tVirtual file \"%s\" found\n", filename);
            return vfile;
        }
        if (lastvfile != NULL)
            *lastvfile = vfile;
    }
    return NULL;
}

/* definitions of pvf options */
static const pdc_defopt pdc_create_pvf_options[] =
{
    {"copy", pdc_booleanlist, 0, 1, 1, 0.0, 0.0, NULL},

    PDC_OPT_TERMINATE
};

void
pdc__create_pvf(pdc_core *pdc, const char *filename,
                const void *data, size_t size, const char *optlist)
{
    static const char fn[] = "pdc__create_pvf";
    pdc_bool iscopy = pdc_false;
    pdc_virtfile *vfile, *lastvfile = NULL;
    const char *stemp = NULL;
    pdc_resopt *results;

    if (data == NULL)
        stemp = "data = NULL";

    if (!size)
        stemp = "size = 0";

    if (stemp != NULL)
        pdc_error(pdc, PDC_E_PAR_NODATA, stemp, 0, 0, 0);

    /* Parse optlist */
    results = pdc_parse_optionlist(pdc, optlist, pdc_create_pvf_options,
                                   NULL, pdc_true);
    pdc_get_optvalues("copy", results, &iscopy, NULL);
    pdc_cleanup_optionlist(pdc, results);

    /* Find virtual file in file system */
    vfile = pdc_find_pvf(pdc, filename, &lastvfile);

    /* Name already exists */
    if (vfile != NULL)
        pdc_error(pdc, PDC_E_PVF_NAMEEXISTS, filename, 0, 0, 0);

    /* New virtual file */
    vfile = (pdc_virtfile *) pdc_calloc(pdc, sizeof(pdc_virtfile), fn);
    if (lastvfile)
        lastvfile->next = vfile;
    else
        pdc->filesystem = vfile;

    /* Fill up file struct */
    vfile->name = pdc_strdup(pdc, filename);
    if (iscopy == pdc_true)
    {
        vfile->data = (const void *) pdc_malloc(pdc, size, fn);
        memcpy((void *) vfile->data, data, size);
    }
    else
    {
        vfile->data = data;
    }
    vfile->size = size;
    vfile->iscopy = iscopy;
    vfile->lockcount = 0;
    vfile->next = NULL;

    pdc_logg_cond(pdc, 1, trc_filesearch,
        "\n\tVirtual file \"%s\" created\n", filename);
}

int
pdc__delete_pvf(pdc_core *pdc, const char *filename)
{
    pdc_virtfile *vfile, *lastvfile = NULL;

    /* Find virtual file in file system */
    vfile = pdc_find_pvf(pdc, filename, &lastvfile);
    if (vfile)
    {
        /* File exists but locked */
        if (vfile->lockcount > 0)
        {
            return pdc_undef;
        }

        /* Delete */
        if (vfile->iscopy == pdc_true)
        {
            pdc_free(pdc, (void *) vfile->data);
            vfile->data = NULL;
        }
        pdc_free(pdc, vfile->name);
        if (lastvfile)
            lastvfile->next = vfile->next;
        else
            pdc->filesystem = vfile->next;
        pdc_free(pdc, vfile);

        pdc_logg_cond(pdc, 1, trc_filesearch,
            "\tVirtual file \"%s\" deleted\n", filename);
    }

    return pdc_true;
}

void
pdc_lock_pvf(pdc_core *pdc, const char *filename)
{
    pdc_virtfile *vfile = pdc_find_pvf(pdc, filename, NULL);
    if (vfile)
    {
        (vfile->lockcount)++;

        pdc_logg_cond(pdc, 1, trc_filesearch,
            "\tVirtual file \"%s\" locked\n", filename);
    }
}

void
pdc_unlock_pvf(pdc_core *pdc, const char *filename)
{
    pdc_virtfile *vfile = pdc_find_pvf(pdc, filename, NULL);
    if (vfile)
    {
        (vfile->lockcount)--;

        pdc_logg_cond(pdc, 1, trc_filesearch,
            "\tVirtual file \"%s\" unlocked\n", filename);
    }
}

void
pdc_delete_filesystem(pdc_core *pdc)
{
    pdc_virtfile *vfile, *nextvfile;
    pdc_virtfile *filesystem = pdc->filesystem;

    for (vfile = filesystem; vfile != NULL; /* */)
    {
        nextvfile = vfile->next;
        if (vfile->iscopy == pdc_true && vfile->data)
            pdc_free(pdc, (void *) vfile->data);
        if (vfile->name)
            pdc_free(pdc, vfile->name);
        pdc_free(pdc, vfile);
        vfile = nextvfile;
    }
    pdc->filesystem = NULL;
}


/* ------------------------ file search handling ---------------------------- */

#if defined(_MSC_VER) && defined(_MANAGED)
#pragma unmanaged
#endif
pdc_file *
pdc_fsearch_fopen(pdc_core *pdc, const char *filename, char *fullname,
                  const char *qualifier, int flags)
{
    pdc_reslist *resl = pdc_get_reslist(pdc);
    char fullname_s[PDC_FILENAMELEN];
    const pdc_byte *data = NULL;
    pdc_file *sfp = NULL;
    size_t size = 0;
    pdc_virtfile *vfile;

    if (fullname == NULL)
        fullname = fullname_s;
    strcpy(fullname, filename);

    vfile = pdc_find_pvf(pdc, filename, NULL);
    if (vfile)
    {
        size = vfile->size;
        data = (const pdc_byte *) vfile->data;
        sfp = pdc_fopen(pdc, filename, qualifier, data, size, flags);
    }
    else
    {
        pdc_category *cat;

        /* Bad filename */
        if (!*filename || !strcmp(filename, ".") || !strcmp(filename, ".."))
        {
            pdc_set_errmsg(pdc, PDC_E_IO_ILLFILENAME, filename, 0, 0, 0);
            return NULL;
        }


        /* Read resource configuration file if it is pending */
        if (resl->filepending)
        {
            resl->filepending = pdc_false;
            pdc_read_resourcefile(pdc, resl->filename);
        }

        pdc_logg_cond(pdc, 1, trc_filesearch,
            "\n\tSearching for file \"%s\":\n", filename);

        /* Searching resource category */
        for (cat = resl->resources;
             cat != (pdc_category *) NULL;
             cat = cat->next)
            if (!pdc_stricmp(cat->category, "SearchPath")) break;

        if (!cat)
        {
            /* No resource category */
            sfp = pdc_fopen(pdc, filename, qualifier, NULL, 0, flags);
        }
        else
        {
            pdc_res *res = cat->kids;
            pdc_res *lastres = cat->kids;
            char *pathname = NULL;
            FILE *fp = NULL;
            int errnum = PDC_E_IO_RDOPEN_NF;
            pdc_bool fatal = pdc_false;

            /* Find last SearchPath entry */
            while (res != (pdc_res *) NULL)
            {
                lastres = res;
                res = res->next;
            }

            /* First local search and then search with concatenated
             * filename with search paths one after another backwards
             */
            while (1)
            {
                /* Test opening */
                pdc_file_fullname(pdc, pathname, filename, fullname);

                if (pathname != NULL)
                    pdc_logg_cond(pdc, 1, trc_filesearch,
                        "\tin directory \"%s\": \"%s\"\n", pathname, fullname);

                fp = pdc_fopen_logg(pdc, fullname, READBMODE);
                if (fp)
                {
                    /* File found */
                    pdc_fclose_logg(pdc, fp);
                    sfp = pdc_fopen(pdc, fullname, qualifier, NULL, 0,flags);
                    break;
                }
                errnum = pdc_get_fopen_errnum(pdc, PDC_E_IO_RDOPEN);
                if (errno != 0 && errnum != PDC_E_IO_RDOPEN_NF)
                {
                    fatal = pdc_true;
                    pdc_set_fopen_errmsg(pdc, PDC_E_IO_RDOPEN,
                                         qualifier, fullname);
                }

#if defined(WIN32)
                /* file name beginning with a drive letter: 'x:'
                 * is already full specified.
                 */
                if (pdc_isalpha(filename[0]) && filename[1] == ':')
                    break;
#endif

                if (lastres == (pdc_res *) NULL)
                    break;

                pathname = lastres->name;
                lastres = lastres->prev;
            }

            if (sfp == NULL && !fatal)
                pdc_set_fopen_errmsg(pdc, PDC_E_IO_RDOPEN,
                                     qualifier, filename);
            else
                filename = fullname;
        }
    }

    pdc_logg_cond(pdc, 1, trc_filesearch,
        "\tFile \"%s\" %sfound\n", fullname, sfp == NULL ? "not " : "");
    return sfp;
}
#if defined(_MSC_VER) && defined(_MANAGED)
#pragma managed
#endif


/* ----------------------- logging file handling ---------------------------- */

# ifndef DEFAULTLOGFILE
#  if defined(MVS)
#   define DEFAULTLOGFILE       "pdflog"
#  elif defined(MAC) || defined(AS400)
#   define DEFAULTLOGFILE       "/%s.log"
#  elif defined(WIN32)
#   define DEFAULTLOGFILE       "\\%s.log"
#  else
#   define DEFAULTLOGFILE       "/tmp/%s.log"
#  endif
# endif

#define LOGFILEENVNAME          "%sLOGFILE"
#define LOGGINGENVNAME          "%sLOGGING"

#define PDC_CLASSLIST_SIZE      32
#define PDC_CLASSLIST_MAX       10

struct pdc_loggdef_s
{
    pdc_bool enabled;           /* logging enabled */
    char *filename;             /* name of the logging file */
    pdc_bool fromenviron;       /* logging file name defined environment */
    pdc_bool header;            /* with header and separation line */
    pdc_bool flush;             /* logging file will be opened and
                                 * and closed immediately */
    FILE *fp;                   /* flush = false: file handle */
    pdc_strform_kind strform;   /* format for logging strings */
    int maxchar;                /* maximal number of characters
                                 * of logging strings */
    int sri;                    /* first index in classlist for save/restore */
    char classlist[PDC_CLASSLIST_MAX][PDC_CLASSLIST_SIZE];
                                /* list array of levels for logging classes */
    pdc_bool classapi;          /* only api class has level = 1
                                 * and warning class has level = 1 */
};

static pdc_loggdef *
pdc_new_logg(pdc_core *pdc)
{
    static const char fn[] = "pdc_new_logg";
    const char *filename;
    char envname[32];

    pdc_loggdef *logg = (pdc_loggdef *)
                              pdc_malloc(pdc, sizeof(pdc_loggdef), fn);

    logg->enabled = pdc_false;
    logg->filename = NULL;
    logg->fromenviron = pdc_false;
    logg->header = pdc_true;
    logg->flush = pdc_false;
    logg->fp = NULL;
    logg->strform = strform_readable;
    logg->maxchar = 0;
    logg->sri = 0;
    memset(logg->classlist[0], 0, PDC_CLASSLIST_SIZE);
    logg->classlist[0][trc_api] = 1;
    logg->classlist[0][trc_warning] = 1;
    logg->classapi = pdc_true;

    pdc->logg = logg;

    /* logging file name defined by environment variable */
    sprintf(envname, LOGFILEENVNAME, pdc->prodname);
    pdc_strtoupper(envname);
    filename = pdc_getenv(pdc, envname);
    if (filename != NULL)
    {
        logg->filename = pdc_strdup(pdc, filename);
        logg->fromenviron = pdc_undef; /* not yet initialized */
    }

    return logg;
}

void
pdc_delete_logg(pdc_core *pdc)
{
    if (pdc->logg != NULL)
    {
        pdc_loggdef *logg = pdc->logg;

        logg->enabled = pdc_false;

        /* close file */
        if (logg->fp != NULL && logg->fp != stdout && logg->fp != stderr)
        {
            fclose(logg->fp);
            logg->fp = NULL;
        }

        if (logg->filename != NULL)
        {
            pdc_free(pdc, logg->filename);
            logg->filename = NULL;
        }

        pdc_free(pdc, logg);
        pdc->logg = NULL;
    }
}

static pdc_loggdef *
pdc_get_logg(pdc_core *pdc)
{
    pdc_loggdef *logg = pdc->logg;

    if (logg == NULL)
        logg = pdc_new_logg(pdc);

    return logg;
}

static const pdc_keyconn pdc_strform_keylist[] =
{
    {"readable",   strform_readable},
    {"readable0",  strform_readable0},
    {"octal",      strform_octal},
    {"hex",        strform_hexa},
    {"java",       strform_java},
    {NULL, 0}
};

static const pdc_keyconn pdf_protoclass_keylist[] =
{
    {"api",        trc_api},
    {"encoding",   trc_encoding},
    {"digsig",     trc_digsig},
    {"filesearch", trc_filesearch},
    {"font",       trc_font},
    {"image",      trc_image},
    {"memory",     trc_memory},
    {"optlist",    trc_optlist},
    {"other",      trc_other},
    {"pcos",       trc_pcos},
    {"pdi",        trc_pdi},
    {"resource",   trc_resource},
    {"shadow",     trc_shadow},
    {"table",      trc_table},
    {"text",       trc_text},
    {"textflow",   trc_textflow},
    {"user",       trc_user},
    {"warning",    trc_warning},
    {"wordfinder", trc_wordfinder},
    {"xmp",        trc_xmp},
    {"zones",      trc_zones},
    {NULL, 0}
};

#define PDC_FILENAMELEN 1024

static const pdc_defopt pdc_logg_options[] =
{
    {"header", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"enable", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"disable", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"flush", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"remove", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"filename", pdc_stringlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_FILENAMELEN, NULL},

    {"restore", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"save", pdc_booleanlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, NULL},

    {"stringformat", pdc_keywordlist, PDC_OPT_NONE, 1, 1,
      0.0, 0.0, pdc_strform_keylist},

    {"stringlimit", pdc_integerlist, PDC_OPT_NONE, 1, 1,
      0.0, PDC_INT_MAX, NULL},

    {"classes", pdc_stringlist, PDC_OPT_EVENNUM |PDC_OPT_SUBOPTLIST,
      1, 2 * PDC_CLASSLIST_SIZE, 1.0, 64, NULL},

    PDC_OPT_TERMINATE
};

#define PDC_SEPARSTR_LEN 80

static const char *separstr =
    "[---------------------------------------"
    "---------------------------------------]\n";

void
pdc_set_logg_options(pdc_core *pdc, const char *optlist)
{
    pdc_loggdef *logg = pdc_get_logg(pdc);
    pdc_resopt *resopts = NULL;
    char **strlist;
    char filename[PDC_FILENAMELEN+1];
    const char *keyword;
    pdc_bool sare = pdc_false;
    pdc_bool enable = pdc_true;
    pdc_bool remfile = pdc_false;
    pdc_char level;
    int inum, i, pclass = 0, sumlevel = 0;

    filename[0] = 0;
    if (optlist && strlen(optlist))
    {
        resopts = pdc_parse_optionlist(pdc, optlist, pdc_logg_options,
                                       NULL, pdc_true);

        if (pdc_get_optvalues("save", resopts, &sare, NULL) && sare)
        {
            i = logg->sri + 1;
            if (i >= PDC_CLASSLIST_MAX)
                pdc_error(pdc, PDC_E_INT_TOOMUCH_SARE, 0, 0, 0, 0);
            memcpy(logg->classlist[i], logg->classlist[logg->sri],
                   PDC_CLASSLIST_SIZE);
            logg->sri = i;
        }

        if (pdc_get_optvalues("restore", resopts, &sare, NULL) && sare)
        {
            i = logg->sri - 1;
            if (i < 0)
                pdc_error(pdc, PDC_E_INT_TOOMUCH_SARE, 0, 0, 0, 0);
            logg->sri = i;
        }

        if (pdc_get_optvalues("disable", resopts, &inum, NULL))
            enable = inum ? pdc_false : pdc_true;

        pdc_get_optvalues("header", resopts, &logg->header, NULL);

        pdc_get_optvalues("flush", resopts, &logg->flush, NULL);

        pdc_get_optvalues("remove", resopts, &remfile, NULL);

        if (logg->fromenviron == pdc_false)
        {
            const char *fname = pdc_get_opt_filename(pdc, "filename", resopts);

            if (fname != NULL)
                strcpy(filename, fname);

        }

        if (pdc_get_optvalues("stringformat", resopts, &inum, NULL))
            logg->strform = (pdc_strform_kind) inum;

        pdc_get_optvalues("stringlimit", resopts, &logg->maxchar, NULL);

        inum = pdc_get_optvalues("classes", resopts, NULL, &strlist);
        if (inum)
        {
            for (i = 0; i < inum; i++, i++)
            {
                if (!pdc_stricmp(strlist[i], "other"))
                {
                    i++;
                    if (pdc_str2integer(strlist[i],
                                        PDC_INT_CHAR | PDC_INT_UNSIGNED,
                                        &level))
                    {
                        memset(logg->classlist[logg->sri], (int)level,
                               PDC_CLASSLIST_SIZE);
                    }
                    break;
                }
            }
            for (i = 0; i < inum; i++)
            {
                keyword = strlist[i];
                pclass = pdc_get_keycode_ci(keyword, pdf_protoclass_keylist);
                if (pclass == PDC_KEY_NOTFOUND)
                    pdc_error(pdc, PDC_E_OPT_ILLKEYWORD,
                              "classes", keyword, 0, 0);
                i++;
                if (!pdc_str2integer(strlist[i],
                                     PDC_INT_CHAR | PDC_INT_UNSIGNED,
                                     &level))
                    pdc_error(pdc, PDC_E_OPT_ILLINTEGER,
                              keyword, strlist[i], 0, 0);

                logg->classlist[logg->sri][pclass] = level;
            }

            for (i = 0; i < PDC_CLASSLIST_SIZE; i++)
                sumlevel += (int) logg->classlist[logg->sri][i];
            logg->classapi =
                (sumlevel == 2 &&
                 logg->classlist[logg->sri][trc_api] &&
                 logg->classlist[logg->sri][trc_warning]) ?
                pdc_true : pdc_false;
        }

        pdc_cleanup_optionlist(pdc, resopts);
    }

    /* disable */
    if (logg->enabled && logg->header && !enable)
    {
        pdc_logg(pdc, "\n");
        pdc_logg(pdc, separstr);
    }

    /* no new logging file name specified */
    if (!strlen(filename))
    {
        if (logg->filename == NULL)
        {
            char tmpname[PDC_FILENAMELEN];

            sprintf(tmpname, DEFAULTLOGFILE, pdc->prodname);
            pdc_strtolower(tmpname);
            strcpy(filename, tmpname);
        }
        else
        {
            strcpy(filename, logg->filename);
            if (logg->fromenviron == pdc_undef)
            {
                logg->fromenviron = pdc_true;
                pdc_free(pdc, logg->filename);
                logg->filename = NULL;
            }
        }
    }

    /* new logging file */
    if (pdc_strcmp(logg->filename, filename))
    {
        pdc_time ltime;

        /* close file */
        if (logg->fp != stdout && logg->fp != stderr && logg->filename != NULL)
        {
            pdc_localtime(&ltime);
            pdc_logg(pdc, "[%04d-%02d-%02d %02d:%02d:%02d]\n",
                ltime.year + 1900, ltime.month + 1, ltime.mday,
                ltime.hour, ltime.minute, ltime.second);
            if (logg->fp != NULL)
                fclose(logg->fp);
        }
        logg->enabled = pdc_false;

        /* remove file */
        if (remfile && strcmp(filename, "stdout") && strcmp(filename, "stderr"))
            remove(filename);

        /* file name */
        if (logg->filename != NULL)
            pdc_free(pdc, logg->filename);
        logg->filename = pdc_strdup(pdc, filename);

        /* open file */
        if (!logg->flush)
        {
            /* due to honorlang, codeset of LANG: UTF-8 */
            i = pdc_is_utf8_bytecode(logg->filename) ? 3 : 0;

            if (!strcmp(logg->filename, "stdout"))
                logg->fp = stdout;
            else if (!strcmp(logg->filename, "stderr"))
                logg->fp = stderr;
            else
                logg->fp = fopen(&logg->filename[i], APPENDMODE);
            if (logg->fp == NULL)
            {
                pdc_error(pdc, PDC_E_IO_WROPEN, "log ", logg->filename,
                          0, 0);
            }
        }
        else
        {
            logg->fp = NULL;
        }


        /* header line */
        logg->enabled = enable;
        if (logg->enabled && logg->header && pdc->prodname != NULL)
        {
            char binding[64], buf[256], *bp;

            pdc_logg(pdc, separstr);

            pdc_localtime(&ltime);
            binding[0] = 0;
            if (pdc->binding)
            {
                strcat(binding, pdc->binding);
                strcat(binding, " binding ");
            }

            sprintf(buf, "[ %s %s  %son %s (%s) %04d-%02d-%02d %02d:%02d:%02d",
                    pdc->prodname, pdc->version, binding,
                    PDF_PLATFORM, PDC_ISBIGENDIAN ? "be" : "le",
                    ltime.year + 1900, ltime.month + 1, ltime.mday,
                    ltime.hour, ltime.minute, ltime.second);

            i = MAX(PDC_SEPARSTR_LEN - (int) strlen(buf) - 1, 1);
            pdc_logg(pdc, "%s%*s]\n", buf, i, " ");

            bp = buf;
            bp += sprintf(buf, "[ Classes:");
            for (pclass = 0; pclass < trc_numclasses; pclass++)
            {
                level = logg->classlist[logg->sri][pclass];
                if (level)
                {
                    keyword = pdc_get_keyword(pclass, pdf_protoclass_keylist);
                    bp += sprintf(bp, " %s=%d", keyword, level);
                }
            }
            i = MAX(PDC_SEPARSTR_LEN - (int) strlen(buf) - 1, 1);
            pdc_logg(pdc, "%s%*s]\n", buf, i, " ");

            if (logg->classapi)
            {
                strcpy(buf, "[ Use  %s/\\[[^]]*\\]//g  and  %s/)$/);"
                             "/  in vi to compile it");
                i = MAX(PDC_SEPARSTR_LEN - (int) strlen(buf) - 1, 1);
                pdc_logg(pdc, "%s%*s]\n", buf, i, " ");
            }

            pdc_logg(pdc, separstr);
        }
    }
    else
    {
        logg->enabled = enable;
    }
}

const char *
pdc_print_loggstring(pdc_core *pdc, const char *str, int len)
{
    pdc_strform_kind strform = strform_readable0;
    int maxchar = 0;

    if (pdc->logg != NULL && pdc->logg->enabled)
    {

        maxchar = pdc->logg->maxchar;
        strform = pdc->logg->strform;
    }

    str = pdc_strprint(pdc, str, len, maxchar, strform);


    return str;
}

/* logging function without any class level check and decorations
 */
static void
pdc_logg_output(pdc_core *pdc, const char *fmt, va_list ap)
{
    pdc_loggdef *logg = pdc->logg;

    if (logg->flush)
    {
        FILE *fp = NULL;

        /* due to honorlang, codeset of LANG: UTF-8 */
        int i = pdc_is_utf8_bytecode(logg->filename) ? 3 : 0;

        if (!strcmp(logg->filename, "stdout"))
            fp = stdout;
        else if (!strcmp(logg->filename, "stderr"))
            fp = stderr;
        else
            fp = fopen(&logg->filename[i], APPENDMODE);

        if (fp == NULL)
        {
            logg->enabled = pdc_false;
            pdc_error(pdc, PDC_E_IO_WROPEN, "log ", logg->filename,
                      0, 0);
        }

        pdc_vfprintf(pdc, pdc_false, fp, fmt, ap);

        if (fp != stdout && fp != stderr)
            fclose(fp);
    }
    else
    {
        pdc_vfprintf(pdc, pdc_false, logg->fp, fmt, ap);
        fflush(logg->fp);
    }
}


/* standard logging protocol functions for api function calls
 */
pdc_bool
pdc_enter_api_logg(pdc_core *pdc, const char *funame, pdc_bool enter_api,
                   const char *fmt, va_list args)
{
    pdc_bool retval = pdc_true;

    if (enter_api)
        retval = pdc_enter_api(pdc, funame);

    if (retval)
    {
        /* logging option list defined by environment variable */
        if (pdc->loggenv == pdc_false)
        {
            char envname[32];
            const char *envval = NULL;

            pdc->loggenv = pdc_true;
            sprintf(envname, LOGGINGENVNAME, pdc->prodname);
            pdc_strtoupper(envname);
            envval = pdc_getenv(pdc, envname);
            if (envval != NULL)
            {
                pdc_set_logg_options(pdc, envval);
            }
#if defined(WIN32)
            else
            {
                char buffer[BUFSIZE];
                char regkey[128];
                HKEY hKey = NULL;
                DWORD size, lType;

                sprintf(regkey, REGISTRYKEY, pdc->prodname, pdc->version);

                /* process registry entries */
                if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, regkey, 0L,
                    (REGSAM) KEY_QUERY_VALUE, &hKey) == ERROR_SUCCESS)
                {
                    size = BUFSIZE - 2;
                    pdc_strtolower(envname);
                    if (RegQueryValueExA(hKey, envname, (LPDWORD) NULL,
                                         &lType, (LPBYTE) buffer, &size)
                        == ERROR_SUCCESS && *buffer)
                    {
                        pdc_set_logg_options(pdc, buffer);
                    }

                    RegCloseKey(hKey);
                }
            }
    #endif
        }

        if (pdc->logg != NULL &&
            pdc->logg->enabled &&
            pdc->logg->classlist[pdc->logg->sri][trc_api])
        {
            /* time stamp */
            if (pdc->logg->classlist[pdc->logg->sri][trc_api] > 1)
            {
                pdc_time ltime;

                if (funame[0] == '\n')
                {
                    pdc_logg(pdc, "\n");
                    funame++;
                }

                pdc_localtime(&ltime);
                pdc_logg(pdc, "[%02d:%02d:%02d] ",
                          ltime.hour, ltime.minute, ltime.second);
            }

            /* function name */
            pdc_logg(pdc, "%s", funame);

            /* function arg list */
            pdc_logg_output(pdc, fmt, args);
        }
    }

    return retval;
}

void
pdc_logg_exit_api(pdc_core *pdc, pdc_bool cleanup, const char *fmt, ...)
{
    if (fmt != NULL && pdc != NULL &&
        pdc->logg != NULL &&
        pdc->logg->enabled &&
        pdc->logg->classlist[pdc->logg->sri][trc_api])
    {
        va_list ap;

        va_start(ap, fmt);
        pdc_logg_output(pdc, fmt, ap);
        va_end(ap);
    }

    if (cleanup)
        pdc_tmlist_cleanup(pdc);
}

/*
 * General logging functions
 */

/*
 * pdc_logg_enable() enables/disables logging
 */
void
pdc_logg_enable(pdc_core *pdc, pdc_bool enable)
{
    if (pdc != NULL && pdc->logg != NULL && pdc->logg->filename != NULL)
    {
        pdc->logg->enabled = enable;
    }
}

/*
 * pdc_logg_is_enabled() returns pdc_true
 * if logging is enabled for logging class 'pclass' and level 'level'.
 * Otherwise pdc_false will be returned.
 */
pdc_bool
pdc_logg_is_enabled(pdc_core *pdc, int level, int pclass)
{
    return pdc->logg != NULL &&
           pdc->logg->enabled &&
           level <= pdc->logg->classlist[pdc->logg->sri][pclass];
}


/*
 * pdc_logg() writes formatted text in the current logging file
 * without checking whether logging is enabled.
 * This function should only be used in connection with
 * pdc_logg_is_enabled():
 */
void
pdc_logg(pdc_core *pdc, const char *fmt, ...)
{
    if (pdc != NULL && pdc->logg != NULL && pdc->logg->enabled)
    {
        va_list ap;

        va_start(ap, fmt);
        pdc_logg_output(pdc, fmt, ap);
        va_end(ap);
    }
}

/*
 * pdc_logg_cond() writes formatted text in the current logging file
 * if logging is enabled for logging class 'pclass' and level 'level'.
 */
void
pdc_logg_cond(pdc_core *pdc, int level, int pclass, const char *fmt, ...)
{
    if (pdc != NULL && pdc->logg != NULL &&
        pdc->logg->enabled &&
        level <= pdc->logg->classlist[pdc->logg->sri][pclass])
    {
        va_list ap;

        va_start(ap, fmt);
        pdc_logg_output(pdc, fmt, ap);
        va_end(ap);
    }
}


/*
 * pdc_logg_getlevel() returns the current level for a logging class
 */
int
pdc_logg_getlevel(pdc_core *pdc, int pclass)
{
    if (pdc->logg != NULL && pdc->logg->enabled)
        return (int) pdc->logg->classlist[pdc->logg->sri][pclass];
    else
        return 0;
}

/*
 * pdc_logg_bitarr() writes the literal representation of a bit array
   (including a descriptive message) in 1 line in the current logging file.
   variable nbit must be <=32.
 */
void
pdc_logg_bitarr(pdc_core *pdc, const char *msg, const char *bitarr, int nbit)
{
    int i;

    pdc_logg(pdc,"%s = ", msg);

    nbit = MIN(nbit, 32);
    for (i = 0; i <= nbit; i++)
    {
        if (!(i%8))
        {
            pdc_logg(pdc, "|");
        }
        if (i == nbit)
        {
            if (nbit == 8)
                pdc_logg(pdc, "  (%02X)", bitarr[0]);
            else if (nbit == 16)
                pdc_logg(pdc, "  (%04X)", *((pdc_uint16 *) &bitarr[0]));
            else if (nbit == 32)
                pdc_logg(pdc, "  (%08X)", *((pdc_uint32 *) &bitarr[0]));
            pdc_logg(pdc, "\n");
        }
        else
        {
            pdc_logg(pdc, "%s", pdc_getbit(bitarr, i) ? "1" : "0");
        }
    }
}


/*
 * pdc_logg_hexdump() writes the hexadecimal output of the specified buffer
   (including a descriptive message) in the current logging file.
 */
void
pdc_logg_hexdump(pdc_core *pdc, const char *msg, const char *prefix,
                 const char *text, int tlen)
{
    int i, k;
    pdc_byte ct;

    if (tlen == 1)
    {
        ct = (pdc_byte) text[0];
        pdc_logg(pdc, "%s%s: %02X '%c'\n", prefix, msg, ct,
                 pdc_logg_isprint((int) ct) ? ct : '.');
    }
    else
    {
        pdc_logg(pdc,"%s%s:\n", prefix, msg);

        for (i = 0; i < tlen; i += 16)
        {
            pdc_logg(pdc,"%s", prefix);
            for (k = 0; k < 16; ++k)
            {
                if (i + k < tlen)
                {
                    ct = (pdc_byte) text[i + k];
                    pdc_logg(pdc,"%02X ", ct);
                }
                else
                    pdc_logg(pdc,"   ");
            }

            pdc_logg(pdc," ");
            for (k = 0; k < 16; ++k)
            {
                if (i + k < tlen)
                {
                    ct = (pdc_byte) text[i + k];
                    pdc_logg(pdc,"%c", pdc_logg_isprint((int)ct) ? ct : '.');
                }
                else
                    pdc_logg(pdc,"   ");
            }

            pdc_logg(pdc,"\n");
        }
    }
}

/*
 * pdc_warning() is the former function of PDFlib exception handling.
 * Now, pdc_warning() calls the function pdc_set_warnmsg(), which writes
 * only a warning message generated from a xx_generr.h file into the logfile,
 * if warning = 1.
 */
void
pdc_warning(pdc_core *pdc, int errnum, const char *parm1, const char *parm2,
            const char *parm3, const char *parm4)
{
    if (!pdc->smokerun)
        pdc_set_warnmsg(pdc, errnum, parm1, parm2, parm3, parm4);
}

/*
 * utility function for logging a Unicode character
 *
 */
void
pdc_logg_unichar(pdc_core *pdc, int unichar, pdc_bool kfill, pdc_bool newline)
{
    if (unichar > 0xFFFF)
    {
        pdc_logg(pdc, "U+%05X", unichar);
    }
    else
    {
        pdc_logg(pdc, "U+%04X", unichar);

        if ((unichar >= PDC_UNICODE_SPACE && unichar <= PDC_UNICODE_DELETE) ||
            (unichar >= PDC_UNICODE_NBSP  && unichar <= PDC_UNICODE_MAXLATIN1))
        {
            char c = (char) unichar;


            pdc_logg(pdc, " [%c]", c);
        }
        else if (kfill)
        {
            pdc_logg(pdc, "    ");
        }
    }

    if (newline)
        pdc_logg(pdc, "\n");
}

/*
 * utility function for logging a Unicode string
 *
 */
static const pdc_keyconn pdc_ascii_escape_keylist[] =
{
    {"a",    0x07},
    {"b",    0x08},
    {"e",    0x1B},
    {"f",    0x0C},
    {"n",    0x0A},
    {"r",    0x0D},
    {"t",    0x09},
    {"v",    0x0B},
    {NULL, 0}
};

void
pdc_logg_unitext(pdc_core *pdc, pdc_ushort *ustext, int len, pdc_bool newline)
{
    int i;
    pdc_ushort usv;
    const char *s;
    char c;

    pdc_logg(pdc, "\"");
    for (i = 0; i < len; i++)
    {
        usv = ustext[i];
        if (usv > PDC_UNICODE_MAXLATIN1)
        {
            pdc_logg(pdc, "\\u%04X", usv);
        }
        else
        {
            if (usv < PDC_UNICODE_SPACE)
            {
                s = pdc_get_keyword((int) usv, pdc_ascii_escape_keylist);
                if (s != NULL)
                {
                    pdc_logg(pdc, "\\%s", s);
                    continue;
                }
            }

            if ((usv >= PDC_UNICODE_SPACE && usv <= PDC_UNICODE_DELETE) ||
                     (usv >= PDC_UNICODE_NBSP  && usv <= PDC_UNICODE_MAXLATIN1))
            {
                c = (char) usv;


                pdc_logg(pdc, "%c", c);
            }
            else
            {
                pdc_logg(pdc, "\\x%02X", usv);
            }
        }
    }

    pdc_logg(pdc, "\"");
    if (newline)
        pdc_logg(pdc, "\n");
}


