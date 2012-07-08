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
 * FONT in-core and basic font metric functions
 *
 */

#define FT_COREFONT_C

#include "ft_font.h"
#include "ft_corefont.h"


/* ------------------------------ core fonts ------------------------------ */

/* PDF basic font names */
static const char *fnt_base14_names[] =
{
  "Courier",
  "Courier-Bold",
  "Courier-Oblique",
  "Courier-BoldOblique",
  "Helvetica",
  "Helvetica-Bold",
  "Helvetica-Oblique",
  "Helvetica-BoldOblique",
  "Symbol",
  "Times-Roman",
  "Times-Bold",
  "Times-Italic",
  "Times-BoldItalic",
  "ZapfDingbats"
};

pdc_bool
fnt_is_standard_font(const char *fontname)
{
    int slot;

    for (slot = 0;
         slot < (int)(sizeof(fnt_base14_names) / sizeof(fnt_base14_names[0]));
         slot++)
    {
        if (!strcmp(fnt_base14_names[slot], fontname))
            return pdc_true;
    }

    return pdc_false;
}

/* abbreviations of PDF basic font names for form fields */
static const char *fnt_abb_base14_names[] =
{
  "Cour",
  "CoBo",
  "CoOb",
  "CoBO",
  "Helv",
  "HeBo",
  "HeOb",
  "HeBO",
  "Symb",
  "TiRo",
  "TiBo",
  "TiIt",
  "TiBI",
  "ZaDb"
};

const char *
fnt_get_abb_std_fontname(const char *fontname)
{
    int slot;

    for (slot = 0;
         slot < (int)(sizeof(fnt_base14_names) / sizeof(fnt_base14_names[0]));
         slot++)
    {
        if (!strcmp(fnt_base14_names[slot], fontname))
            return fnt_abb_base14_names[slot];
    }

    return NULL;
}


/* Basic fonts core metrics */
static const fnt_font_metric *fnt_base_font_metrics[] =
{
    &fnt_font_metric_01,
    &fnt_font_metric_02,
    &fnt_font_metric_03,
    &fnt_font_metric_04,
    &fnt_font_metric_05,
    &fnt_font_metric_06,
    &fnt_font_metric_07,
    &fnt_font_metric_08,
    &fnt_font_metric_09,
    &fnt_font_metric_10,
    &fnt_font_metric_11,
    &fnt_font_metric_12,
    &fnt_font_metric_13,
    &fnt_font_metric_14
};

const fnt_font_metric *
fnt_get_core_metric(const char *fontname)
{
#ifdef PDF_BUILTINMETRIC_SUPPORTED
    const fnt_font_metric *metric = NULL;
    int slot;

    for (slot = 0;
         slot < (int)(sizeof(fnt_base_font_metrics) /
                      sizeof(fnt_base_font_metrics[0]));
         slot++)
    {
        metric = fnt_base_font_metrics[slot];
        if (!strcmp(metric->name, fontname))
            return metric;
    }
#endif /* PDF_BUILTINMETRIC_SUPPORTED */
    return(NULL);
}


/* --------------------- Pre-installed CID fonts ---------------------- */

int
fnt_get_preinstalled_cidfont(const char *fontname,
                             const fnt_font_metric **fontmetric)
{
    int slot;

    for (slot = 0; slot < FNT_NUM_OF_CIDFONTS; slot++)
    {
        if (!strcmp(fnt_cid_metrics[slot].name, fontname))
        {
            if (fontmetric)
                *fontmetric = &fnt_cid_metrics[slot];
            return fnt_cid_metrics[slot].charcoll;
        }
    }

    if (fontmetric)
        *fontmetric = NULL;

    return (int) cc_none;
}

/* abbreviations of PDF basic CJK font names for form fields */
static const char *fnt_abb_cjk_names[] =
{
  "KaGo",
  "KaMi",
  "HyGo",
  "HySm",
  "MHei",
  "MSun",
  "STSo",
};

const char *
fnt_get_abb_cjk_fontname(const char *fontname)
{
    int slot;

    for (slot = 0;
         slot < (int)(sizeof(fnt_abb_cjk_names) / sizeof(fnt_abb_cjk_names[0]));
         slot++)
    {
        if (!strcmp(fnt_cid_metrics[slot].name, fontname))
            return fnt_abb_cjk_names[slot];
    }

    return NULL;
}

#ifdef PDF_CJKFONTWIDTHS_SUPPORTED

const char **
fnt_get_cid_widths_array(pdc_core *pdc, fnt_font *font)
{
    int slot, slotm;

    (void) pdc;

    /* search for font name */
    slotm = 100;
    for (slot = 0; slot < slotm; slot += FNT_CIDMETRIC_INCR)
    {
        if (!strcmp(fnt_cid_width_arrays[slot], font->name))
            break;
    }

    return &fnt_cid_width_arrays[slot + 1];  /* skip font name */
}

static void
fnt_parse_cid_widths(pdc_core *pdc, fnt_font *font)
{
    static const char fn[] = "fnt_parse_cid_widths";
    int slot, slota, slotm;
    const char *chunk;
    char **strlist = NULL, **sstrlist = NULL, *str;
    int cid = 0, cidfirst, cidlast, width;
    int il, is, ns, nss = 0;
    int wformat = 2;

    /* search for font name */
    slotm = 100;
    for (slot = 0; slot < slotm; slot += FNT_CIDMETRIC_INCR)
    {
        if (!strcmp(fnt_cid_width_arrays[slot], font->name))
            break;
    }
    if (slot == slotm)
        return;

    /* we take the maximum */
    font->m.numwidths = fnt_get_maxcid(font->m.charcoll, -1) + 1;
    font->m.widths = (int *) pdc_malloc(pdc,
                                 font->m.numwidths * sizeof(int), fn);

    slota = slot + 1;                       /* skip font name  */
    slotm = slot + FNT_CIDMETRIC_INCR;
    for (slot = slota; slot < slotm; slot++)
    {
        chunk = fnt_cid_width_arrays[slot];

        ns = pdc_split_stringlist(pdc, chunk, " \n", 0, &strlist);
        for (is = 0; is < ns; is++)
        {
            str = strlist[is];

            /* check for next format 1 chunk */
            if (wformat == 2 && strchr(str, '['))
            {
                nss = pdc_split_stringlist(pdc, str, " [", 0, &sstrlist);
                str = sstrlist[0];
                pdc_str2integer(str, 0, &cidfirst);
                for (; cid < cidfirst; cid++)
                    font->m.widths[cid] = FNT_DEFAULT_CIDWIDTH;
                str = sstrlist[1];
                wformat = 1;
            }

            /* format 1:  cid [width_1 width_2 ... width_n] */
            if (wformat == 1)
            {
                il = (int) strlen(str) - 1;
                if (str[il] == ']')
                {
                    str[il] = 0;
                    wformat = 2;
                }

                pdc_str2integer(str, 0, &font->m.widths[cid]);
                cid++;

                if (nss)
                {
                    pdc_cleanup_stringlist(pdc, sstrlist);
                    nss = 0;
                }
            }
            else
            {
                /* format 2:  cid_first cid_last width */
                pdc_str2integer(str, 0, &cidfirst);
                is++;
                str = strlist[is];
                pdc_str2integer(str, 0, &cidlast);
                is++;
                str = strlist[is];
                pdc_str2integer(str, 0, &width);

                for (; cid < cidfirst; cid++)
                    font->m.widths[cid] = FNT_DEFAULT_CIDWIDTH;
                for (; cid <= cidlast; cid++)
                    font->m.widths[cid] = width;
            }
        }

        pdc_cleanup_stringlist(pdc, strlist);
    }

    for (; cid < font->m.numwidths; cid++)
        font->m.widths[cid] = FNT_DEFAULT_CIDWIDTH;

    if (pdc_logg_is_enabled(pdc, 5, trc_font))
    {
        for (cid = 0; cid < font->m.numwidths; cid++)
            pdc_logg(pdc, "\t\t\tCID width[%d]: %d\n",
                     cid, font->m.widths[cid]);
    }
}

#endif /* PDF_CJKFONTWIDTHS_SUPPORTED */


/* ------------------------- general functions -------------------------- */

/*
 * Fill font metric struct from core metric struct
 */
void
fnt_fill_font_metric(pdc_core *pdc, fnt_font *font, pdc_bool kerning,
                     const fnt_font_metric *metric)
{
    static const char fn[] = "fnt_fill_font_metric";

    (void) kerning;

    /* Fill font metric struct. Font struct must be initialized */
    font->m = *metric;
    font->m.charcoll = abs(font->m.charcoll);
    font->m.name = pdc_strdup(pdc, metric->name);
    font->name = pdc_strdup(pdc, metric->name);

    /* Fill glyph widths array (double mapping Unicode <-> code <-> width) */
    if (font->m.numglwidths)
    {
        font->m.glw = (fnt_glyphwidth *) pdc_calloc(pdc,
                          metric->numglwidths * sizeof(fnt_glyphwidth), fn);
        memcpy(font->m.glw, metric->glw,
               metric->numglwidths * sizeof(fnt_glyphwidth));
    }

    /* Fill glyph width array (mapping Unicode interval <-> width) */
    if (metric->numinters)
    {
        /* We must convert */
        if (font->m.type == fnt_Type1)
        {
            int i, j, iw, iwe;
            pdc_ushort uv;

            for (i = 0; i < metric->numinters; i++)
            {
                if (i && metric->ciw[i-1].width != 0)
                    font->m.numglwidths += metric->ciw[i].startcode -
                                           metric->ciw[i-1].startcode;
            }
            font->m.glw = (fnt_glyphwidth *) pdc_calloc(pdc,
                              font->m.numglwidths * sizeof(fnt_glyphwidth), fn);

            j = 0;
            iw = 0;
            for (i = 0; i < metric->numinters; i++)
            {
                if (i && metric->ciw[j].width != 0)
                {
                    uv = metric->ciw[j].startcode;
                    iwe = iw + metric->ciw[i].startcode - uv;
                    for (; iw < iwe; iw++)
                    {
                        font->m.glw[iw].unicode = uv;
                        font->m.glw[iw].width = metric->ciw[j].width;
                        uv++;
                    }
                }
                j = i;
            }
            font->m.numinters = 0;
            font->m.ciw = NULL;
        }
        else
        {
            font->m.ciw = (fnt_interwidth *) pdc_calloc(pdc,
                                font->m.numinters * sizeof(fnt_interwidth), fn);
            memcpy(font->m.ciw, metric->ciw,
                   metric->numinters * sizeof(fnt_interwidth));
        }
    }

#ifdef PDF_CJKFONTWIDTHS_SUPPORTED
    /* Fill glyph width array (mapping CID -> width) */
    if (font->m.type == fnt_CIDFontType0)
        fnt_parse_cid_widths(pdc, font);
#endif /* PDF_CJKFONTWIDTHS_SUPPORTED */

    /* Number of glyphs */
    if (font->m.type == fnt_Type1)
        font->numglyphs = font->m.numglwidths;


    /* font weight */
    font->weight = fnt_stemv2weight(font->m.StdVW);

    /* standard Adobe font */
    font->isstdfont = pdc_true;

    /* symbol font */
    if (!(font->m.flags & FNT_SYMBOL))
        font->issymbfont = pdc_false;
}



