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
 * PDFlib utility functions
 *
 */

#define P_UTIL_C

#include "p_intern.h"
#include "p_font.h"
#include "p_image.h"

/* ------------------------- [hyper]textformat -----------------------------*/

void
pdf_check_textformat(PDF *p, pdc_text_format textformat)
{
    if (!p->pdc->ptfrun && p->pdc->unicaplang && textformat != pdc_auto2)
        pdc_error(p->pdc, PDF_E_ENC_UNSUPPENCFORMAT, "textformat", 0, 0, 0);

    pdc_logg_cond(p->pdc, 1, trc_encoding, "\tTextformat: \"%s\"\n",
                       pdc_get_keyword(textformat, pdf_textformat_keylist));
}

void
pdf_check_hypertextformat(PDF *p, pdc_text_format hypertextformat)
{
    if (!p->pdc->ptfrun && p->pdc->unicaplang && hypertextformat != pdc_auto2)
        pdc_error(p->pdc, PDF_E_ENC_UNSUPPENCFORMAT, "hypertextformat",
                  0, 0, 0);

    pdc_logg_cond(p->pdc, 1, trc_encoding, "\tHypertextformat: \"%s\"\n",
		   pdc_get_keyword(hypertextformat, pdf_textformat_keylist));
}


/* ------------------------- hypertextencoding -----------------------------*/

void
pdf_check_hypertextencoding(PDF *p, pdc_encoding hypertextencoding)
{
    if (!p->pdc->ptfrun && p->pdc->unicaplang &&
        hypertextencoding != pdc_unicode)
        pdc_error(p->pdc, PDF_E_ENC_UNSUPPENCFORMAT, "hypertextencoding",
                  0, 0, 0);

    pdc_logg_cond(p->pdc, 1, trc_encoding, "\tHypertextencoding: \"%s\"\n",
                       pdc_get_user_encoding(p->pdc, hypertextencoding));
}


/* ----------------------- string utility functions -----------------------*/

static void
pdf_set_convertflags(PDF *p, int *convflags)
{
    (void) p;
    (void) convflags;

}

const char *
pdf__utf16_to_utf8(PDF *p, const char *utf16string, int len, int *size)
{
    char *utf8string;
    int convflags = PDC_CONV_WITHBOM | PDC_CONV_EBCDIC | PDC_CONV_TRY7BYTES;

    pdf_set_convertflags(p, &convflags);
    utf8string = pdc_utf16_to_utf8(p->pdc, utf16string, len, convflags, size);

    pdf_insert_utilstring(p, utf8string, pdc_false);

    return utf8string;
}

const char *
pdf__utf32_to_utf16(PDF *p, const char *utf32string, int len,
                    const char *ordering, int *outlen)
{
    char *utf16string = pdc_utf32_to_utf16(p->pdc, utf32string, len,
                                           ordering, 0, outlen);

    pdf_insert_utilstring(p, utf16string, pdc_false);

    return utf16string;
}

const char *
pdf__utf8_to_utf16(PDF *p, const char *utf8string, const char *ordering,
                   int *outlen)
{
    char *utf16string;
    int convflags = PDC_CONV_EBCDIC;

    pdf_set_convertflags(p, &convflags);
    utf16string = pdc_utf8_to_utf16(p->pdc, utf8string, ordering,
                                    convflags, outlen);

    pdf_insert_utilstring(p, utf16string, pdc_false);

    return utf16string;
}

void
pdf_init_stringlists(PDF *p)
{
    p->stringlists_number = 0;
    p->stringlists_capacity = 0;
    p->stringlists = NULL;
    p->stringlistsizes = NULL;
    p->utilstrlist_index = -1;
    p->utilstring_number = 0;
}

int
pdf_insert_stringlist(PDF *p, char **stringlist, int ns)
{
    static const char fn[] = "pdf_insert_stringlist";
    int i;

    if (p->stringlists_number == p->stringlists_capacity)
    {
        int j = p->stringlists_capacity;

        if (!p->stringlists_capacity)
        {
            p->stringlists_capacity = STRINGLISTS_CHUNKSIZE;

            p->stringlists = (char ***) pdc_malloc(p->pdc,
                sizeof(char **) * p->stringlists_capacity, fn);

            p->stringlistsizes = (int *) pdc_malloc(p->pdc,
                sizeof(int) * p->stringlists_capacity, fn);
        }
        else
        {
            p->stringlists_capacity *= 2;
            p->stringlists = (char ***) pdc_realloc(p->pdc, p->stringlists,
                sizeof(char **) * p->stringlists_capacity, fn);

            p->stringlistsizes = (int *) pdc_realloc(p->pdc, p->stringlistsizes,
                sizeof(int) * p->stringlists_capacity, fn);
        }
        for (i = j; i < p->stringlists_capacity; i++)
        {
            p->stringlists[i] = NULL;
            p->stringlistsizes[i] = 0;
        }
    }

    i = p->stringlists_number;
    p->stringlists[i] = stringlist;
    p->stringlistsizes[i] = ns;
    p->stringlists_number++;

    return i;
}

void
pdf_cleanup_stringlists(PDF *p)
{
    int i;

    if (p->stringlists)
    {
        for (i = 0; i < p->stringlists_number; i++)
        {
            if (p->stringlists[i])
                pdc_cleanup_optstringlist(p->pdc,
                    p->stringlists[i], p->stringlistsizes[i]);
        }
        pdc_free(p->pdc, p->stringlists);
        pdc_free(p->pdc, p->stringlistsizes);
    }

    pdf_init_stringlists(p);
}

#define PDF_MAX_UTILSTRLISTS 10

int
pdf_insert_utilstring(PDF *p, const char *utilstring, pdc_bool kdup)
{
    static const char fn[] = "pdf_insert_utilstring";
    char **utilstrlist;
    int i = 0;

    if (p->utilstrlist_index == -1)
    {
        utilstrlist = (char **) pdc_calloc(p->pdc,
                                    PDF_MAX_UTILSTRLISTS * sizeof (char *), fn);
        p->utilstrlist_index =
            pdf_insert_stringlist(p, utilstrlist, PDF_MAX_UTILSTRLISTS);
    }
    utilstrlist = p->stringlists[p->utilstrlist_index];

    if (p->utilstring_number >= PDF_MAX_UTILSTRLISTS)
        p->utilstring_number = 0;
    i = p->utilstring_number;
    if (utilstrlist[i] != NULL)
        pdc_free(p->pdc, utilstrlist[i]);
    if (kdup)
        utilstrlist[i] = pdc_strdup_ext(p->pdc, utilstring, 0, fn);
    else
        utilstrlist[i] = (char *) utilstring;
    p->utilstring_number++;

    return i;
}

const char *
pdf_get_utilstring(PDF *p, int i)
{
    if (p->utilstrlist_index > -1 && i > -1 && i < p->utilstring_number)
    {
        char **utilstrlist = p->stringlists[p->utilstrlist_index];
        return utilstrlist[i];
    }

    return NULL;
}

/* ------------------------ name treatment -------------------------------*/

void
pdf_put_pdfname(PDF *p, const char *name)
{
    char *ascname = (char *) name;
    int len = (int) strlen(ascname);


    pdc_put_pdfname(p->out, ascname, len);

}


/* ---------------------- hyper text treatment -------------------------------*/

pdc_encoding
pdf_get_hypertextencoding_opt(PDF *p, pdc_resopt *resopts, int *codepage,
                              pdc_bool verbose)
{
    char **strlist;
    pdc_encoding htenc;

    if (pdc_get_optvalues("hypertextencoding", resopts, NULL, &strlist))
    {
        int cp;

        htenc = pdf_get_hypertextencoding(p, strlist[0], &cp, verbose);
        pdf_check_hypertextencoding(p, htenc);

        if (codepage)
            *codepage = cp;
    }
    else
    {
        htenc = pdf_get_hypertextencoding_param(p, codepage);
    }

    return htenc;
}

/* hypertext conversion for deprecated functions */
char *
pdf_convert_hypertext_depr(PDF *p, const char *text, int len)
{
    int outlen;

    pdf_get_hypertextencoding_param(p, NULL);

    return pdf_convert_hypertext(p, text, len, p->hypertextformat,
                      p->hypertextencoding, p->hypertextcodepage, &outlen,
                      PDC_UTF8_FLAG, pdc_true);
}

/*
 * Conversion to PDFDoc/EBCDIC or UTF-16/[EBCDIC-]UTF-8
 */
char *
pdf_convert_hypertext(PDF *p, const char *text, int len,
    pdc_text_format hypertextformat, pdc_encoding hypertextencoding,
    int codepage, int *outlen, pdc_bool oututf8, pdc_bool verbose)
{
    pdc_encodingvector *inev = NULL, *outev = NULL;
    pdc_byte *intext = (pdc_byte *) text, *outtext = NULL;
    pdc_text_format textformat = pdc_utf16be;
    int convflags = PDC_CONV_WITHBOM | PDC_CONV_TRYBYTES;

    *outlen = 0;

    if (text == NULL)
        return NULL;

    if (len == 0)
        len = (int) strlen(text);

    /* incoming encoding */
    if (hypertextencoding >= 0)
    {
        inev = pdc_get_encoding_vector(p->pdc, hypertextencoding);
    }

    /* PDFDocEncoding */
    outev = pdc_get_encoding_vector(p->pdc, pdc_pdfdoc);

    /* conversion to UTF-16-BE or PDFDocEncoding / EBCDIC */
    pdf_set_convertflags(p, &convflags);
    if (pdc_logg_is_enabled(p->pdc, 3, trc_text))
        convflags |= PDC_CONV_LOGGING;

    pdc_convert_string(p->pdc, hypertextformat, codepage, inev,
                       intext, len,
                       &textformat, outev, &outtext, outlen,
                       convflags, verbose);


    /* conversion to UTF-8 if Unicode */
    if (oututf8 && textformat == pdc_utf16be)
    {
        pdc_text_format outtextformat = PDC_UTF8;
        pdc_byte *newtext = NULL;

        convflags = PDC_CONV_WITHBOM;
        if (pdc_logg_is_enabled(p->pdc, 3, trc_text))
            convflags |= PDC_CONV_LOGGING;

        pdc_convert_string(p->pdc, textformat, 0, NULL, outtext, *outlen,
                           &outtextformat, NULL, &newtext, outlen,
                           convflags, verbose);
        pdc_free(p->pdc, outtext);
        outtext = newtext;
    }

    return (char *) outtext;
}


/*
 * Conversion from [EBCDIC-]UTF-8 to UTF-16 and from EBCDIC to PDFDoc
 */

char *
pdf_convert_pdfstring(PDF *p, const char *text, int inlen, int convflags,
                      int *outlen)
{
    pdc_byte *newtext = NULL;

    if (pdc_is_utf8_bytecode(text))
    {
        pdc_text_format textformat = PDC_UTF8;
        pdc_text_format outtextformat = pdc_utf16be;
        pdc_encodingvector *outev = pdc_get_encoding_vector(p->pdc, pdc_pdfdoc);

        pdf_set_convertflags(p, &convflags);

        pdc_convert_string(p->pdc, textformat, 0, NULL,
                           (pdc_byte *) text, inlen,
                           &outtextformat, outev, &newtext, outlen,
                           convflags, pdc_true);
    }
    else
    {
        newtext = (pdc_byte *) text;
        *outlen = inlen;
    }

    return (char *) newtext;
}

void
pdf_put_hypertext(PDF *p, const char *text)
{
    int convflags = PDC_CONV_WITHBOM | PDC_CONV_TRYBYTES;
    int inlen = (int) pdc_strlen(text);
    int outlen;

    char *newtext = pdf_convert_pdfstring(p, text, inlen, convflags, &outlen);

    pdc_put_pdfstring(p->out, newtext, outlen);

    if (newtext != text)
        pdc_free(p->pdc, newtext);
}

void
pdf_put_pdffilename(PDF *p, const char *text)
{
    int convflags = PDC_CONV_FILENAME | PDC_CONV_TRYBYTES;
    int inlen = (int) pdc_strlen(text);
    int outlen;

    char *newtext = pdf_convert_pdfstring(p, text, inlen, convflags, &outlen);

    pdc_put_pdffilename(p->out, newtext, outlen);

    if (newtext != text)
        pdc_free(p->pdc, newtext);
}

void
pdf_put_pdfunifilename(PDF *p, const char *text)
{
    int convflags = PDC_CONV_WITHBOM | PDC_CONV_TRYBYTES;
    int inlen = (int) pdc_strlen(text);
    int outlen;

    char *newtext = pdf_convert_pdfstring(p, text, inlen, convflags, &outlen);

    pdc_put_pdffilename(p->out, newtext, outlen);

    if (newtext != text)
        pdc_free(p->pdc, newtext);
}


/* ------------------------ name strings -------------------------------*/

static void
pdf_prepare_name_string(PDF *p, const char *name, int len, int maxlen,
                        char **newname, int *newlen,
                        pdc_encoding *htenc, int *htcp)
{
    if (name == NULL)
    {
        len = 0;
        name = "";
    }

    if (len < 0 || len > maxlen)
    {
        pdc_error(p->pdc, PDC_E_ILLARG_STRINGLEN,
                  pdc_errprintf(p->pdc, "%d", len),
                  pdc_errprintf(p->pdc, "%d", PDC_SHRT_MAX), 0, 0);
    }

    *newname = (char *) name;
    *newlen = len;
    *htenc = pdc_invalidenc;
    *htcp = 0;

    if (p->usehyptxtenc && !len && !pdc_is_utf8_bytecode(name))
    {
        *htenc = pdf_get_hypertextencoding_param(p, htcp);

    }
}

char *
pdf_convert_name(PDF *p, const char *name, int len, int flags)
{
    char *resname;
    char *newname;
    int newlen;
    pdc_encoding htenc;
    int htcp;

    pdf_prepare_name_string(p, name, len, PDC_SHRT_MAX,
                            &newname, &newlen, &htenc, &htcp);

    flags |= PDC_CONV_EBCDIC;
    if (pdc_logg_is_enabled(p->pdc, 3, trc_text))
        flags |= PDC_CONV_LOGGING;

    resname = pdc_convert_name_ext(p->pdc, newname, newlen, htenc, htcp, flags);
    if (newname != name)
        pdc_free(p->pdc, newname);

    return resname;
}

const char *
pdf_convert_filename(PDF *p, const char *filename, int len,
                     const char *paramname, int flags)
{
    const char *resfilename;
    char *newfilename;
    int newlen;
    pdc_encoding htenc;
    int htcp;

    pdf_prepare_name_string(p, filename, len, PDC_FILENAMELEN - 1,
                            &newfilename, &newlen, &htenc, &htcp);

    flags |= PDC_CONV_EBCDIC;
    if (pdc_logg_is_enabled(p->pdc, 3, trc_filesearch))
        flags |= PDC_CONV_LOGGING;

    resfilename = pdc_convert_filename_ext(p->pdc, newfilename, len,
                                           paramname, htenc, htcp, flags);
    if (newfilename != filename)
        pdc_free(p->pdc, newfilename);

    return resfilename;
}

void
pdf_add_pdflib_resource(PDF *p, const char *category, const char *resname)
{
    char *newresname;
    int newlen;
    pdc_encoding htenc;
    int htcp;

    pdf_prepare_name_string(p, resname, 0, PDC_FILENAMELEN,
                            &newresname, &newlen, &htenc, &htcp);
    if (newlen)
    {
        char *tmpresname = pdc_utf16_to_utf8(p->pdc, newresname, newlen,
                                             PDC_CONV_EBCDIC | PDC_CONV_WITHBOM,
                                             &newlen);
        pdc_free(p->pdc, newresname);
        newresname = tmpresname;
        newlen = 0;
    }

    pdc_add_resource_ext(p->pdc, category, newresname, NULL, htenc, htcp);

    if (newresname != resname)
        pdc_free(p->pdc, newresname);
}

/* ------------------------ option text list -------------------------------*/

int
pdf_get_opt_textlist(PDF *p, const char *keyword, pdc_resopt *resopts,
                     pdc_encoding enc, int codepage, pdc_bool ishypertext,
                     const char *fieldname, char **text, char ***textlist)
{
    pdc_bool logg1 = pdc_logg_is_enabled(p->pdc, 1, trc_optlist);
    int ns;
    char **strlist;

    ns = pdc_get_optvalues(keyword, resopts, NULL, &strlist);
    if (ns)
    {
        pdc_byte *string = NULL;
        pdc_encodingvector *inev = NULL, *outev = NULL;
        pdc_text_format intextformat = pdc_bytes;
        pdc_text_format outtextformat = pdc_utf16be;
        pdc_text_format textformat;
        int convflags = PDC_CONV_WITHBOM;
        pdc_bool isutf8;
        int i, outlen;

        /* whole option list or string list is in UTF-8 */
        isutf8 = pdc_is_lastopt_utf8(resopts);

        /* Encoding */
        if (ishypertext)
        {
            /* Initialize */
            if (!isutf8)
            {
                if (enc < 0 && enc != pdc_unicode && enc != pdc_cid)
                    enc = pdf_get_hypertextencoding(p, "auto", &codepage,
                                                    pdc_true);
                if (enc >= 0)
                    inev = pdc_get_encoding_vector(p->pdc, enc);
            }

            /* ugly solution of bug #2344 */
            if (ishypertext == pdc_true)
            {
                outev = pdc_get_encoding_vector(p->pdc, pdc_pdfdoc);

                /* conversion to PDFDocEncoding if possible */
                convflags |= PDC_CONV_TRYBYTES;
            }
            else if (ishypertext == pdc_undef)
            {
                /* filename as hypertext */
                outtextformat = PDC_UTF8;
                convflags |= PDC_CONV_TRY7BYTES;
            }
        }
        else
        {
            if (enc == pdc_invalidenc)
            {
                if (fieldname)
                {
                    pdc_cleanup_optionlist(p->pdc, resopts);
                    pdc_error(p->pdc, PDF_E_FF_FONTMISSING, fieldname, 0, 0, 0);
                }
                return 0;
            }
            else if (enc >= 0 && !isutf8)
            {
                /* bug #2069: always conversion to UTF-16BE */
                inev = pdc_get_encoding_vector(p->pdc, enc);
            }
        }

        if (logg1)
        {
            if (isutf8)
            {
                pdc_logg(p->pdc, "\tOption \"%s\" is "PDC_UTF8_STRG" encoded\n",
                         keyword);
            }
            else
            {
                pdc_logg(p->pdc, "\tOption \"%s\" is %s encoded\n",
                         keyword, pdc_get_user_encoding(p->pdc, enc));
            }
        }

        for (i = 0; i < ns; i++)
        {
            string = (pdc_byte *) strlist[i];

            {
                if (ishypertext || isutf8 || inev != NULL)
                {
                    intextformat = isutf8 ?  PDC_UTF8 : pdc_bytes;

                    if (pdc_logg_is_enabled(p->pdc, 3, trc_text))
                        convflags |= PDC_CONV_LOGGING;
                    pdf_set_convertflags(p, &convflags);
                    textformat = outtextformat;
                    pdc_convert_string(p->pdc, intextformat, codepage, inev,
                                string, (int) strlen((char *) string),
                                &textformat, outev, &string, &outlen,
                                convflags, pdc_true);
                    pdc_free(p->pdc, strlist[i]);
                    strlist[i] = (char *) string;
                }
            }
        }

        if (text)
            *text = strlist[0];
        else
            *textlist = strlist;

        if (fieldname)
        {
            strlist = (char **) pdc_save_lastopt(resopts, PDC_OPT_SAVEALL);
            pdf_insert_stringlist(p, strlist, ns);
        }
    }

    return ns;
}

char *
pdf_get_opt_filename(PDF *p, const char *keyword, pdc_resopt *resopts,
                     pdc_encoding enc, int codepage)
{
    pdc_bool logg1 = pdc_logg_is_enabled(p->pdc, 1, trc_optlist);
    pdc_bool logg3 = pdc_logg_is_enabled(p->pdc, 3, trc_text);
    pdc_byte *filename = NULL;
    char **strlist;

    if (pdc_get_optvalues(keyword, resopts, NULL, &strlist))
    {
        pdc_encodingvector *inev = NULL, *outev = NULL;
        pdc_text_format intextformat = pdc_bytes;
        pdc_text_format outtextformat = pdc_utf16; /* sic! */
        int convflags = PDC_CONV_NOBOM | PDC_CONV_TRYBYTES | PDC_CONV_NEWALLOC;
        pdc_bool isutf8;
        int ic, outlen;

        /* whole option list or string list is in UTF-8 */
        isutf8 = pdc_is_lastopt_utf8(resopts);

        if (!isutf8)
        {
            if (enc < 0 && enc != pdc_unicode && enc != pdc_cid)
                enc = pdf_get_hypertextencoding(p, "auto", &codepage,
                                                pdc_true);
            if (enc >= 0)
                inev = pdc_get_encoding_vector(p->pdc, enc);
        }
        else
        {
            intextformat = PDC_UTF8;
        }

        if (logg1)
        {
            if (isutf8)
            {
                pdc_logg(p->pdc, "\tOption \"%s\" is "PDC_UTF8_STRG" encoded\n",
                         keyword);
            }
            else
            {
                pdc_logg(p->pdc, "\tOption \"%s\" is %s encoded\n",
                         keyword, pdc_get_user_encoding(p->pdc, enc));
            }
        }

        outev = pdc_get_encoding_vector(p->pdc, pdc_winansi);

        if (logg3)
            convflags |= PDC_CONV_LOGGING;
        pdf_set_convertflags(p, &convflags);

        pdc_convert_string(p->pdc, intextformat, codepage, inev,
                    (pdc_byte *) strlist[0], (int) strlen(strlist[0]),
                    &outtextformat, outev, &filename, &outlen,
                    convflags, pdc_true);

        if (outtextformat == pdc_utf16)
        {
            pdc_ushort uv, *unifilename = (pdc_ushort *) filename;
            int code;

            if (p->compatibility < PDC_1_7)
                pdc_error(p->pdc, PDC_E_IO_UNSUPP_PDFUNINAME, 0, 0, 0, 0);

            /* we must replace non-WinAnsi characters by period
             * and omit the BOM to get a WinAnsi string.
             */
            outlen /= 2;
            for (ic = 0; ic < outlen; ic++)
            {
                uv = unifilename[ic];

                code = pdc_get_encoding_bytecode(p->pdc, outev, uv);
                if (code <= 0)
                    uv = PDC_UNICODE_PERIOD;

                filename[ic] = (char) uv;
            }
            filename[ic] = 0;
        }

        if (logg3)
            pdc_logg_hexdump(p->pdc, "output filename", "\t\t",
                             (char *) filename, ( int ) strlen((char *) filename));
    }

    return (char *) filename;
}

char *
pdf_get_opt_utf8name(PDF *p, const char *keyword, pdc_resopt *resopts)
{
    char **strlist = NULL;
    char *utf8name = NULL;

    if (pdc_get_opt_utf8strings(p->pdc, keyword, resopts, PDC_OPT_SAVE1ELEM,
                                &strlist))
    {
        utf8name = strlist[0];
    }

    return utf8name;
}


/* -------------------------- errorpolicy -------------------------------*/

pdc_bool
pdf_get_errorpolicy(PDF *p, pdc_resopt *resopts, pdc_bool verbose)
{
    int errpol = (int) p->errorpolicy;

    if (resopts != NULL)
        pdc_get_optvalues("errorpolicy", resopts, &errpol, NULL);

    if (errpol != (int) errpol_legacy)
        verbose = errpol;

    return verbose;
}


/* -------------------------- handle check -------------------------------*/

int
pdf_check_opt_handle(void *opaque, int handle, pdc_opttype type)
{
    PDF *p = (PDF*)opaque;
    int minval = 0, maxval = 0;
    pdc_bool empty = pdc_false;

    switch (type)
    {

        case pdc_actionhandle:
        maxval = pdf_get_max_action(p);
        break;

        case pdc_bookmarkhandle:
        maxval = p->outline_count;
        break;

        case pdc_colorhandle:
        maxval = p->colorspaces_number - 1;
        break;


        case pdc_fonthandle:
        maxval = p->fonts_number - 1;
        empty = !pdf_isvalid_font(p, handle);
        break;

        case pdc_gstatehandle:
        maxval = p->extgstates_number - 1;
        break;


        case pdc_imagehandle:
        maxval = p->images_capacity - 1;
        if (handle >= minval && handle <= maxval &&
            (!p->images[handle].in_use ||
              p->xobjects[p->images[handle].no].type == pdi_xobject))
            empty = pdc_true;
        break;


        case pdc_pagehandle:
        maxval = p->images_capacity - 1;
        if (handle >= minval && handle <= maxval &&
            (!p->images[handle].in_use ||
              p->xobjects[p->images[handle].no].type != pdi_xobject))
            empty = pdc_true;
        break;

        case pdc_patternhandle:
        maxval = p->pattern_number - 1;
        break;

        case pdc_shadinghandle:
        maxval = p->shadings_number - 1;
        break;


        case pdc_templatehandle:
        maxval = p->images_capacity - 1;
        if (handle >= minval && handle <= maxval &&
            (!p->images[handle].in_use ||
              p->xobjects[p->images[handle].no].type != form_xobject))
            empty = pdc_true;
        break;


        case pdc_stringhandle:
        if (p->utilstrlist_index == -1)
            empty = pdc_true;
        maxval = p->utilstring_number - 1;
        break;

        default:
        break;
    }

    if (handle < minval || handle > maxval || empty)
        return PDC_E_ILLARG_HANDLE;

    return 0;
}

void
pdf_check_handle(PDF *p, int handle, pdc_opttype type)
{
    if (pdf_check_opt_handle(p, handle, type))
    {
        if (p->pdc->hastobepos && type != pdc_stringhandle)
            handle++;

        pdc_error(p->pdc, PDC_E_ILLARG_HANDLE,
                  pdc_errprintf(p->pdc, "%.*s", PDC_ERR_MAXSTRLEN,
                                pdc_get_handletype(type)),
                  pdc_errprintf(p->pdc, "%d", handle), 0, 0);
    }
}

void
pdf_set_clientdata(PDF *p, pdc_clientdata *cdata)
{
    memset(cdata, 0, sizeof(pdc_clientdata));

    cdata->maxaction = pdf_get_max_action(p);
    cdata->maxbookmark = p->outline_count;
    cdata->maxcolor = p->colorspaces_number - 1;
    cdata->maxdocument = p->pdi_capacity - 1;
    cdata->maxfont = p->fonts_number - 1;
    cdata->maxgstate = p->extgstates_number - 1;
    cdata->maximage = p->images_capacity - 1;
    cdata->maxpage = p->images_capacity - 1;
    cdata->maxpattern = p->pattern_number - 1;
    cdata->maxshading = p->shadings_number - 1;
    cdata->maxtemplate = p->images_capacity - 1;
    cdata->maxstring = p->utilstring_number - 1;

    cdata->compatibility = p->compatibility;
}
