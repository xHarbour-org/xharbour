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
 * PDFlib AFM parsing routines
 *
 */

#include "p_intern.h"
#include "p_font.h"

#define AFM_GLYPH_SUPPL   3

#define AFM_LINEBUF       4096

#define AFM_SEPARATORS    "\f\n\r\t\v ,:;"

/* The values of each of these enumerated items correspond to an entry in the
 * table of strings defined below. Therefore, if you add a new string as
 * new keyword into the keyStrings table, you must also add a corresponding
 * pdf_afmkey AND it MUST be in the same position!
 *
 * IMPORTANT: since the sorting algorithm is a binary search, the strings of
 * keywords must be placed in lexicographical order, below. [Therefore, the
 * enumerated items are not necessarily in lexicographical order, depending
 * on the name chosen. BUT, they must be placed in the same position as the
 * corresponding key string.] The NOPE shall remain in the last position,
 * since it does not correspond to any key string.
 */

#ifndef PDFLIB_EBCDIC
typedef enum
{
    ASCENDER,
    CHARBBOX,
    CODE,
    COMPCHAR,
    CODEHEX,
    CAPHEIGHT,
    CHARWIDTH,
    CHARACTERSET,
    CHARACTERS,
    COMMENT,
    DESCENDER,
    ENCODINGSCHEME,
    ENDCHARMETRICS,
    ENDCOMPOSITES,
    ENDDIRECTION,
    ENDFONTMETRICS,
    ENDKERNDATA,
    ENDKERNPAIRS,
    ENDKERNPAIRS0,
    ENDKERNPAIRS1,
    ENDMASTERFONTMETRICS,
    ENDTRACKKERN,
    ESCCHAR,
    FAMILYNAME,
    FONTBBOX,
    FONTNAME,
    FULLNAME,
    ISBASEFONT,
    ISCIDFONT,
    ISFIXEDPITCH,
    ISFIXEDV,
    ITALICANGLE,
    KERNPAIR,
    KERNPAIRHAMT,
    KERNPAIRXAMT,
    KERNPAIRYAMT,
    LIGATURE,
    MAPPINGSCHEME,
    METRICSSETS,
    CHARNAME,
    NOTICE,
    COMPCHARPIECE,
    STARTCHARMETRICS,
    STARTCOMPFONTMETRICS,
    STARTCOMPOSITES,
    STARTDIRECTION,
    STARTFONTMETRICS,
    STARTKERNDATA,
    STARTKERNPAIRS,
    STARTKERNPAIRS0,
    STARTKERNPAIRS1,
    STARTMASTERFONTMETRICS,
    STARTTRACKKERN,
    STDHW,
    STDVW,
    TRACKKERN,
    UNDERLINEPOSITION,
    UNDERLINETHICKNESS,
    VVECTOR,
    VERSION,
    XYWIDTH,
    XY0WIDTH,
    X0WIDTH,
    Y0WIDTH,
    XY1WIDTH,
    X1WIDTH,
    Y1WIDTH,
    XWIDTH,
    YWIDTH,
    WEIGHT,
    XHEIGHT,
    NOPE
}
pdf_afmkey;

/* keywords for the system:
 * This a table of all of the current strings that are vaild AFM keys.
 * Each entry can be referenced by the appropriate pdf_afmkey value (an
 * enumerated data type defined above). If you add a new keyword here,
 * a corresponding pdf_afmkey MUST be added to the enumerated data type
 * defined above, AND it MUST be added in the same position as the
 * string is in this table.
 *
 * IMPORTANT: since the sorting algorithm is a binary search, the keywords
 * must be placed in lexicographical order. And, NULL should remain at the
 * end.
 */

static const char *keyStrings[] =
{
    "Ascender",
    "B",
    "C",
    "CC",
    "CH",
    "CapHeight",
    "CharWidth",
    "CharacterSet",
    "Characters",
    "Comment",
    "Descender",
    "EncodingScheme",
    "EndCharMetrics",
    "EndComposites",
    "EndDirection",
    "EndFontMetrics",
    "EndKernData",
    "EndKernPairs",
    "EndKernPairs0",
    "EndKernPairs1",
    "EndMasterFontMetrics",
    "EndTrackKern",
    "EscChar",
    "FamilyName",
    "FontBBox",
    "FontName",
    "FullName",
    "IsBaseFont",
    "IsCIDFont",
    "IsFixedPitch",
    "IsFixedV",
    "ItalicAngle",
    "KP",
    "KPH",
    "KPX",
    "KPY",
    "L",
    "MappingScheme",
    "MetricsSets",
    "N",
    "Notice",
    "PCC",
    "StartCharMetrics",
    "StartCompFontMetrics",
    "StartComposites",
    "StartDirection",
    "StartFontMetrics",
    "StartKernData",
    "StartKernPairs",
    "StartKernPairs0",
    "StartKernPairs1",
    "StartMasterFontMetrics",
    "StartTrackKern",
    "StdHW",
    "StdVW",
    "TrackKern",
    "UnderlinePosition",
    "UnderlineThickness",
    "VVector",
    "Version",
    "W",
    "W0",
    "W0X",
    "W0Y",
    "W1",
    "W1X",
    "W1Y",
    "WX",
    "WY",
    "Weight",
    "XHeight"
};

#else   /* !PDFLIB_EBCDIC */
#endif  /* PDFLIB_EBCDIC */

static pdc_bool
pdf_parse_afm(
    PDF *p,
    pdc_file *fp,
    pdf_font *font,
    const char *fontname,
    const char *filename)
{
    static const char fn[] = "pdf_parse_afm";
    fnt_font_metric *ftm = &font->ft.m;
    const char *afmtype = NULL;
    char **wordlist = NULL, *keyword, *arg1;
    char line[AFM_LINEBUF];
    int i, cmp, lo, hi, nwords, nglyphs = 0, nline = 0;
    int tablen = ((sizeof keyStrings) / (sizeof (char *)));
    pdc_sint32 iz;
    double dz;
    pdc_scalar charwidth = -1;
    pdf_afmkey keynumber;
    fnt_glyphwidth *glw;
    pdc_bool toskip = pdc_false;
    pdc_bool is_zadbfont = !strcmp(fontname, "ZapfDingbats");

    /* all new glyph names of AGL 2.0 are missing */
    font->missingglyphs = 0xFFFFFFFF;

    /* read loop. because of Mac files we use pdc_fgetline */
    while (pdc_fgetline(line, AFM_LINEBUF, fp) != NULL)
    {
        /* split line */
        nline++;
        nwords = pdc_split_stringlist(p->pdc, line, AFM_SEPARATORS, 0,
                                      &wordlist);
        if (!nwords) continue;
        keyword = wordlist[0];

        /* find keynumber */
        lo = 0;
        hi = tablen;
        keynumber = NOPE;
        while (lo < hi)
        {
            i = (lo + hi) / 2;
            cmp = strcmp(keyword, keyStrings[i]);

            if (cmp == 0)
            {
                keynumber = (pdf_afmkey) i;
                break;
            }

            if (cmp < 0)
                hi = i;
            else
                lo = i + 1;
        }

        /* unkown key */
        if (keynumber == NOPE)
        {
            pdc_warning(p->pdc, PDF_E_T1_AFMBADKEY, keyword, filename, 0,0);
            goto PDF_PARSECONTD;
        }
        if (keynumber == ENDDIRECTION)
            toskip = pdc_false;

        if (nwords == 1 || toskip == pdc_true)
            goto PDF_PARSECONTD;

        /* key switch */
        arg1 = wordlist[1];
        switch (keynumber)
        {
            case STARTDIRECTION:
            if (pdc_str2integer(arg1, 0, &iz) != pdc_true)
                goto PDF_SYNTAXERROR;
            if (iz)
                toskip = pdc_true;
            break;

            case STARTCOMPFONTMETRICS:
            afmtype = "ACFM";
            goto PDF_SYNTAXERROR;

            case STARTMASTERFONTMETRICS:
            afmtype = "AMFM";
            goto PDF_SYNTAXERROR;

            case ISCIDFONT:
            afmtype = "CID font";
            if (!strcmp(arg1, "true"))
                goto PDF_SYNTAXERROR;
            break;

            case FONTNAME:
            font->ft.name = pdc_strdup(p->pdc, arg1);
            ftm->name = pdc_strdup(p->pdc, arg1);
            pdc_logg_cond(p->pdc, 1, trc_font,
                "\tPostScript font name: \"%s\"\n", ftm->name);
            break;

            /* Recognize Multiple Master fonts by last part of name */
            case FAMILYNAME:
            if (!strcmp(wordlist[nwords-1], "MM"))
                ftm->type = fnt_MMType1;
            else
                ftm->type = fnt_Type1;
            break;

            /* Default: FontSpecific */
            case ENCODINGSCHEME:
            if (!pdc_stricmp(arg1, "StandardEncoding") ||
                !pdc_stricmp(arg1, "AdobeStandardEncoding"))
                font->ft.issymbfont = pdc_false;
            break;

            case STDHW:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            ftm->StdHW = (int) dz;
            break;

            case STDVW:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            ftm->StdVW = (int) dz;
            break;

            case WEIGHT:
            font->ft.weight = fnt_check_weight(fnt_weightname2weight(arg1));
            break;

            case ISFIXEDPITCH:
            if (!pdc_stricmp(arg1, "false"))
                ftm->isFixedPitch = pdc_false;
            else
                ftm->isFixedPitch = pdc_true;
            break;

            /* New AFM 4.1 keyword "CharWidth" implies fixed pitch */
            case CHARWIDTH:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            charwidth = dz;
            ftm->isFixedPitch = pdc_true;
            break;

            case ITALICANGLE:
            {
                if (pdc_str2double(arg1, &dz) != pdc_true)
                    goto PDF_SYNTAXERROR;
                ftm->italicAngle = dz;
            }
            break;

            case UNDERLINEPOSITION:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            ftm->underlinePosition = (int) dz;
            break;

            case UNDERLINETHICKNESS:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            ftm->underlineThickness = (int) dz;
            break;

            case FONTBBOX:
            {
                if (nwords != 5)
                    goto PDF_SYNTAXERROR;
                for (i = 1; i < nwords; i++)
                {
                    if (pdc_str2double(wordlist[i], &dz) != pdc_true)
                        goto PDF_SYNTAXERROR;
                    if (i == 1)
                        ftm->llx = dz;
                    else if (i == 2)
                        ftm->lly = dz;
                    else if (i == 3)
                        ftm->urx = dz;
                    else if (i == 4)
                        ftm->ury = dz;
                }
            }
            break;

            case CAPHEIGHT:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            ftm->capHeight = (int) dz;
            break;

            case XHEIGHT:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            ftm->xHeight = (int) dz;
            break;

            case DESCENDER:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            ftm->descender = (int) dz;
            break;

            case ASCENDER:
            if (pdc_str2double(arg1, &dz) != pdc_true)
                goto PDF_SYNTAXERROR;
            ftm->ascender = (int) dz;
            break;

            /* Character widths */

            case STARTCHARMETRICS:
            if (pdc_str2integer(arg1, PDC_INT_UNSIGNED, (pdc_sint32 *) &nglyphs)
                != pdc_true || nglyphs <= 0)
                goto PDF_SYNTAXERROR;
            ftm->glw = (fnt_glyphwidth *) pdc_calloc(p->pdc,
                            (size_t) nglyphs * sizeof(fnt_glyphwidth), fn);
            break;

            /* Character code */
            case CODE:
            case CODEHEX:
            if (!nglyphs || !ftm->glw)
                goto PDF_SYNTAXERROR;
            if (font->ft.numglyphs >= nglyphs)
            {
                nglyphs++;
                ftm->glw = (fnt_glyphwidth *) pdc_realloc(p->pdc, ftm->glw,
                                (size_t) nglyphs * sizeof(fnt_glyphwidth), fn);
            }
            glw = &ftm->glw[font->ft.numglyphs];
            if (keynumber == CODE)
            {
                if (pdc_str2integer(arg1, 0, &iz) != pdc_true)
                    goto PDF_SYNTAXERROR;
            }
            else
            {
                if (pdc_str2integer(arg1, PDC_INT_HEXADEC, &iz) != pdc_true)
                    goto PDF_SYNTAXERROR;
            }
            glw->code = (pdc_short) iz;
            glw->unicode = 0;
            glw->width = (pdc_ushort)
                (font->opt.monospace ? font->opt.monospace : charwidth);
            font->ft.numglyphs++;

            /* Character width and name */
            for (i = 2; i < nwords; i++)
            {
                if (!strcmp(wordlist[i], "WX") ||
                    !strcmp(wordlist[i], "W0X") ||
                    !strcmp(wordlist[i], "W"))
                {
                    i++;
                    if (i == nwords)
                        goto PDF_SYNTAXERROR;
                    if (pdc_str2double(wordlist[i], &dz) != pdc_true)
                        goto PDF_SYNTAXERROR;
                    glw->width = (pdc_ushort)
                         (font->opt.monospace ? font->opt.monospace : dz);
                }

                if (!strcmp(wordlist[i], "N"))
                {
                    i++;
                    if (i == nwords)
                        goto PDF_SYNTAXERROR;

                    /* Unicode value by means of AGL,
                     * internal and private table
                     */
                    glw->unicode = is_zadbfont ?
                         (pdc_ushort) pdc_zadb2unicode(wordlist[i]):
                         pdc_insert_glyphname(p->pdc, wordlist[i]);
                    pdc_delete_missingglyph_bit(glw->unicode,
                                                &font->missingglyphs);

                }
            }
            break;


            default:
            break;
        }

        PDF_PARSECONTD:
        pdc_cleanup_stringlist(p->pdc, wordlist);
        wordlist = NULL;

        if (keynumber == ENDFONTMETRICS)
            break;
    }

    /* necessary font struct members */
    if (font->ft.name == NULL || ftm->glw == NULL)
        goto PDF_SYNTAXERROR;

    pdc_fclose(fp);

    ftm->numglwidths = font->ft.numglyphs;
    return pdc_true;

    PDF_SYNTAXERROR:
    pdc_fclose(fp);
    pdc_cleanup_stringlist(p->pdc, wordlist);

    if (afmtype)
        pdc_set_errmsg(p->pdc, PDF_E_T1_UNSUPP_FORMAT, afmtype, 0, 0, 0);
    else
        pdc_set_errmsg(p->pdc, PDC_E_IO_ILLSYNTAX, "AFM ", filename,
                       pdc_errprintf(p->pdc, "%d", nline), 0);
    return pdc_false;
}

pdc_bool
pdf_process_metrics_data(
    PDF *p,
    pdf_font *font,
    const char *fontname)
{
    static const char fn[] = "pdf_process_metrics_data";
    fnt_font_metric *ftm = &font->ft.m;
    int width = 0;
    pdc_ushort uv;
    pdc_encoding enc = font->ft.enc;
    pdc_encodingvector *ev = NULL;
    int /* nalloc, */ foundglyphs = 0, i, j = 0, k;

    (void) j;

    /* Unallowed encoding */
    if (enc == pdc_cid || enc < pdc_builtin)
    {

	pdc_set_errmsg(p->pdc, PDF_E_FONT_BADENC, 0, 0, 0, 0);

        return pdc_false;
    }

    /* Determine the default character width (width of space character) */
    if (font->opt.monospace)
    {
        ftm->defwidth = font->opt.monospace;
    }
    else
    {
        width = fnt_get_glyphwidth((int) PDF_DEFAULT_CHAR, &font->ft);
        if (width != FNT_MISSING_WIDTH)
            ftm->defwidth = width;
        else
            ftm->defwidth = FNT_DEFAULT_WIDTH;
    }

    /* builtin font */
    if (font->ft.issymbfont == pdc_true && enc != pdc_builtin &&
        !strcmp(font->encapiname, "auto"))
    {
        enc = pdc_builtin;
        font->ft.enc = enc;
    }

    /* optimizing PDF output */
    if (enc == pdc_ebcdic ||
        enc == pdc_ebcdic_37 ||
        enc == pdc_ebcdic_winansi)
        font->towinansi = pdc_winansi;

    /* glyph name list for incore fonts */
    /* nalloc = font->ft.numglyphs + AFM_GLYPH_SUPPL; */

    /*
     * Generate character width according to the chosen encoding
     */

    {
        font->ft.numcodes = 256;
        font->ft.code2gid = (pdc_ushort *) pdc_calloc(p->pdc,
                                 font->ft.numcodes  * sizeof (pdc_ushort), fn);

        ftm->numwidths = font->ft.numcodes;
        ftm->widths = (int *)pdc_calloc(p->pdc,
                                        font->ft.numcodes * sizeof(int), fn);

        /* Given 8-bit encoding */
        if (enc >= 0)
        {
            ev = pdc_get_encoding_vector(p->pdc, enc);
            for (k = 0; k < font->ft.numcodes; k++)
            {
                uv = ev->codes[k];
                ftm->widths[k] = ftm->defwidth;
                if (uv)
                {
                    uv = pdc_get_alter_glyphname(uv, font->missingglyphs, NULL);
                    if (uv)
                    {
                        for (i = 0; i < ftm->numglwidths; i++)
                        {
                            if (ftm->glw[i].unicode == uv)
                            {
                                j = i + 1;
                                ftm->widths[k] = ftm->glw[i].width;
                                font->ft.code2gid[k] = (pdc_ushort) j;
                                foundglyphs++;
                            }
                        }
                    }
                }
            }

            if (ftm->ciw != NULL)
            {
                pdc_free(p->pdc, ftm->ciw);
                ftm->ciw = NULL;
            }

            pdc_logg_cond(p->pdc, 2, trc_font,
                "\t\t%d glyphs could be mapped to Unicode\n", foundglyphs);

            /* No characters found */
            if (!foundglyphs)
            {
                if (font->ft.issymbfont == pdc_false)
                {
                    pdc_set_errmsg(p->pdc, PDF_E_FONT_BADENC, 0, 0, 0, 0);
                    return pdc_false;
                }
                else
                {
                    /* We enforce builtin encoding */
                    pdc_warning(p->pdc, PDF_E_FONT_FORCEENC,
                                pdf_get_encoding_name(p, pdc_builtin, font),
                                0, 0, 0);
                    enc = pdc_builtin;
                    font->ft.enc = enc;
                    font->towinansi = pdc_invalidenc;
                }
            }
            else if (foundglyphs < PDF_MIN_GLYPHS)
            {
                pdc_warning(p->pdc, PDF_E_FONT_INAPPROPENC,
                            pdc_errprintf(p->pdc, "%d", foundglyphs), 0, 0, 0);
            }
        }

        /* built-in encoding */
        if (enc == pdc_builtin)
        {
            if (ftm->glw == NULL)
            {
                pdc_set_errmsg(p->pdc, PDF_E_FONT_BADENC, 0, 0, 0, 0);
                return pdc_false;
            }

            /* encoding for builtin */
            ev = pdf_create_font_encoding(p, enc, font, fontname, pdc_true);
            font->symenc = font->ft.enc;

           /***************************/
            font->ft.enc = pdc_builtin;
           /***************************/

            for (i = 0; i < font->ft.numcodes; i++)
            {
                ftm->widths[i] = ftm->defwidth;
            }

            for (i = 0; i < font->ft.numglyphs; i++)
            {
                pdc_short code = ftm->glw[i].code;

                if (code >= 0 && code < font->ft.numcodes)
                {
                    j = i + 1;
                    ftm->widths[code] = ftm->glw[i].width;
                    font->ft.code2gid[code] = (pdc_ushort) j;
                    if (ev != NULL)
                    {
                        ev->codes[code] = ftm->glw[i].unicode;
                    }
                }
            }
        }
    }


    if (ftm->glw != NULL)
    {
        pdc_free(p->pdc, ftm->glw);
        ftm->glw = NULL;
    }

    return pdc_true;
}

pdc_bool
pdf_get_metrics_afm(
    PDF *p,
    pdf_font *font,
    const char *fontname,
    pdc_encoding enc,
    const char *filename,
    pdc_bool requested)
{
    static const char fn[] = "pdf_get_metrics_afm";
    char fullname[PDC_FILENAMELEN];
    pdc_file *afmfile;

    /* open AFM file */
    afmfile = pdc_fsearch_fopen(p->pdc, filename, fullname, "AFM ",
                                PDC_FILE_TEXT);
    if (afmfile == NULL)
        return pdc_check_fopen_errmsg(p->pdc, requested);

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\tLoading AFM metric fontfile \"%s\":\n", fullname);

    /* parse AFM file */
    if (pdf_parse_afm(p, afmfile, font, fontname, fullname) == pdc_false)
        return pdc_false;

    /* members not fount */
    if (font->ft.m.type == fnt_unknownType)
        font->ft.m.type = fnt_Type1;
    if (font->ft.name == NULL)
    {
        font->ft.name = pdc_strdup(p->pdc, fontname);
        font->ft.m.name = pdc_strdup(p->pdc, fontname);
    }

    /* save full filename */
    font->metricfilename = pdc_strdup_ext(p->pdc, fullname, 0, fn);

    /* process metric data */
    font->ft.enc = enc;
    if (pdf_process_metrics_data(p, font, fontname) == pdc_false)
        return pdc_false;

    if (!pdf_make_fontflag(p, font))
        return pdc_false;

    return pdc_true;
}
