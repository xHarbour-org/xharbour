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
 * FONT basic font functions
 *
 */

#define FT_FONT_C

#include "ft_font.h"


static pdc_error_info fnt_errors[] =
{
#define         fnt_genInfo     1
#include        "ft_generr.h"
};

#define N_FNT_ERRORS    (sizeof fnt_errors / sizeof (pdc_error_info))

void
fnt_register_errtab(pdc_core *pdc)
{
    pdc_register_errtab(pdc, PDC_ET_FONT, fnt_errors, N_FNT_ERRORS);
}

static void
fnt_init_font_metric(fnt_font_metric *metric)
{
    metric->name = NULL;
    metric->flags = 0L;
    metric->type = fnt_unknownType;
    metric->charcoll = (int) cc_none;

    /*
     * Fill in some reasonable default values in global font info in
     * case they're missing from the metrics data.
     */
    metric->italicAngle = 0;
    metric->isFixedPitch = pdc_false;
    metric->llx = FNT_MISSING_FONTVAL;
    metric->lly = FNT_MISSING_FONTVAL;
    metric->urx = FNT_MISSING_FONTVAL;
    metric->ury = FNT_MISSING_FONTVAL;
    metric->underlinePosition = -100;
    metric->underlineThickness = FNT_DEFAULT_UNDERLINEWIDTH;
    metric->ascender = FNT_MISSING_FONTVAL;
    metric->descender = FNT_MISSING_FONTVAL;
    metric->capHeight = FNT_MISSING_FONTVAL;
    metric->xHeight = FNT_MISSING_FONTVAL;
    metric->StdHW = 0;
    metric->StdVW = 0;

    metric->defwidth = FNT_DEFAULT_WIDTH;
    metric->numwidths = 0;
    metric->widths = NULL;
    metric->numinters = 0;
    metric->ciw = NULL;
    metric->numglwidths = 0;
    metric->glw = NULL;

}

void
fnt_init_font(fnt_font *font)
{
    fnt_init_font_metric(&font->m);

    font->name = NULL;
    font->utf8name = NULL;
    font->filename = NULL;
    font->isstdfont = pdc_false;
    font->ishostfont = pdc_false;
    font->issymbfont = pdc_true;
    font->hasdescr = pdc_false;
    font->vertical = pdc_false;
    font->spacechar = 0;
    font->spacewidth = 0;
    font->linegap = FNT_MISSING_FONTVAL;
    font->weight = 0;
    font->vertical = pdc_false;
    pdc_identity_matrix(&font->matrix);
    font->bbox.llx = 0;
    font->bbox.lly = 0;
    font->bbox.urx = 0;
    font->bbox.ury = 0;
    font->fsscale = 1.0;
    font->enc = pdc_invalidenc;
    font->numglyphs = 0;
    font->numcodes = 0;
    font->gid2code = NULL;
    font->code2gid = NULL;
    font->embedded = pdc_false;
    font->cmapname = NULL;
    font->imgname = NULL;
    font->filelen = 0;
    font->img = NULL;
}

static void
fnt_cleanup_font_metric(pdc_core *pdc, fnt_font_metric *metric)
{
    if (metric->name != NULL)
    {
        pdc_free(pdc, metric->name);
        metric->name = NULL;
    }

    if (metric->widths != NULL)
    {
        pdc_free(pdc, metric->widths);
        metric->widths = NULL;
    }

    if (metric->ciw != NULL)
    {
        pdc_free(pdc, metric->ciw);
        metric->ciw = NULL;
    }

    if (metric->glw != NULL)
    {
        pdc_free(pdc, metric->glw);
        metric->glw = NULL;
    }


}

void
fnt_cleanup_fontimg(pdc_core *pdc, fnt_font *font)
{
    if (font->img != NULL && font->imgname == NULL)
    {
        pdc_free(pdc, font->img);
        font->img = NULL;
    }

    if (font->imgname != NULL)
    {
        pdc_free(pdc, font->imgname);
        font->imgname = NULL;
    }
}


void
fnt_cleanup_font(pdc_core *pdc, fnt_font *font)
{
    int i = 0;

    (void) i;

    fnt_cleanup_font_metric(pdc, &font->m);

    if (font->name != NULL)
    {
        pdc_free(pdc, font->name);
        font->name = NULL;
    }

    if (font->utf8name != NULL)
    {
        pdc_free(pdc, font->utf8name);
        font->utf8name = NULL;
    }

    if (font->filename != NULL)
    {
        pdc_free(pdc, font->filename);
        font->filename = NULL;
    }

    /* delete font specific encoding vector */
    if (font->enc >= pdc_firstvarenc)
    {
        pdc_encodingvector *ev = pdc_get_encoding_vector(pdc, font->enc);

        if (ev != NULL && ev->flags & PDC_ENC_FONT)
            pdc_remove_encoding_vector(pdc, (int) font->enc);
    }

    if (font->gid2code != NULL)
    {
        pdc_free(pdc, font->gid2code);
        font->gid2code = NULL;
    }

    if (font->code2gid != NULL)
    {
        pdc_free(pdc, font->code2gid);
        font->code2gid = NULL;
    }



    if (font->cmapname != NULL)
    {
        pdc_free(pdc, font->cmapname);
        font->cmapname = NULL;
    }


    fnt_cleanup_fontimg(pdc, font);
}

/*
 * we assume:
 * code!=0 --> gid=0 --> gid=-1  or  code=0 <--> gid=0
 *
 */
int
fnt_get_glyphid(int code, fnt_font *font)
{
    if (code >= 0 && code < font->numcodes)
    {
        if (font->code2gid != NULL)
        {
            int gid = font->code2gid[code];

            if (gid)
                return gid;
        }
        else
        {
            /* this is temporary. for Type1 fonts there is no information
             * about glyphs at present. we assume identity code = glyph id.
             */
            return code;
        }
    }

    if (!code)
        return 0;

    return -1;
}

/*
 * we assume:
 * gid!=0 --> code=0 --> code=-1  or  gid=0 <--> code=0
 *
 */
int
fnt_get_code(int gid, fnt_font *font)
{
    if (gid >= 0 && gid < font->numglyphs)
    {
        if (font->gid2code != NULL)
        {
            int code = font->gid2code[gid];

            if (code)
                return code;
        }
    }

    if (!gid)
        return 0;

    return -1;
}

int
fnt_get_glyphwidth(int code, fnt_font *font)
{
    int i;

    if (font->m.widths != NULL)
    {
        if (code < font->m.numwidths)
            return font->m.widths[code];
    }
    else if (font->m.ciw != NULL)
    {
        fnt_interwidth *wd = font->m.ciw;
        int lo = 0;
        int hi = font->m.numinters - 1;

        while (lo < hi)
        {
            i = (lo + hi) / 2;

            if (code >= wd[i].startcode && code < wd[i+1].startcode)
                return (int) wd[i].width;

            if (code < wd[i].startcode)
                hi = i;
            else
                lo = i + 1;
        }
    }
    else if (font->m.glw != NULL)
    {
        for (i = 0; i < font->m.numglwidths; i++)
        {
            if (font->m.glw[i].unicode == (pdc_ushort) code)
            {
                return font->m.glw[i].width;
            }
        }
    }

    return FNT_MISSING_WIDTH;
}

void
fnt_font_logg_widths(pdc_core *pdc, fnt_font *font)
{
    if (font != NULL &&
        pdc_logg_is_enabled(pdc, 2, trc_font))
    {
        int code, width;

        for (code = 0; code < PDC_NUM_UNIVAL; code++)
        {
            width = fnt_get_glyphwidth(code, font);
            if (width == FNT_MISSING_WIDTH)
                break;
            pdc_logg(pdc,
                  "\t\tWidth[%d]: %d\n", code, width);
        }
    }
}

static const pdc_keyconn pdf_fonttype_pdfkeylist[] =
{
    {"Type0",         fnt_Type0},
    {"Type1",         fnt_Type1},
    {"MMType1",       fnt_MMType1},
    {"TrueType",      fnt_TrueType},
    {"CIDFontType2",  fnt_CIDFontType2},
    {"Type1C",        fnt_Type1C},
    {"CIDFontType0",  fnt_CIDFontType0},
    {"CIDFontType0C", fnt_CIDFontType0C},
    {"OpenType",      fnt_OpenType},
    {"OpenType",      fnt_OpenTypeC},
    {"Type3",         fnt_Type3},
    {"(unknown)",     fnt_unknownType},
    {NULL, 0}
};

int
fnt_get_pdf_fonttype_code(const char *typenam)
{
    int type = pdc_get_keycode(typenam, pdf_fonttype_pdfkeylist);
    return (type != PDC_KEY_NOTFOUND) ? type : fnt_unknownType;
}

const char *
fnt_get_pdf_fonttype_name(int typecode)
{
    const char *name = pdc_get_keyword(typecode, pdf_fonttype_pdfkeylist);
    return name ? name : "";
}

static const pdc_keyconn pdf_fonttype_descrkeylist[] =
{
    /*                                        Acrobat 7 names for comparison */
    {"Composite",       fnt_Type0},		/* - */
    {"Type 1",          fnt_Type1},		/* Type 1 */
    {"Multiple Master", fnt_MMType1},		/* MM */
    {"TrueType",        fnt_TrueType},		/* TrueType */
    {"TrueType (CID)",  fnt_CIDFontType2},	/* TrueType (CID) */
    {"Type 1 CFF",      fnt_Type1C},		/* Type 1 */
    {"Type 1 (CID)",    fnt_CIDFontType0},	/* Type 1 (CID) */
    {"Type 1 CFF (CID)",fnt_CIDFontType0C},	/* Type 1 (CID) */
    {"OpenType",        fnt_OpenType},		/* OpenType */
    {"OpenType",        fnt_OpenTypeC},
    {"Type 3",          fnt_Type3},		/* Type 3 */
    {"(unknown)",       fnt_unknownType},
    {NULL, 0}
};

const char *
fnt_get_pdf_fonttype_desc(int typecode)
{
    const char *name = pdc_get_keyword(typecode, pdf_fonttype_descrkeylist);
    return name ? name : "";
}

pdc_encodingvector *
fnt_create_font_ev(pdc_core *pdc, fnt_font *font)
{
    pdc_encodingvector *ev = NULL;
    char encname[PDC_GEN_BUFSIZE];

    pdc->uniqueno++;
    pdc_sprintf(pdc, pdc_false, encname, "encoding_%s_%d",
                font->name, pdc->uniqueno);
    ev = pdc_new_encoding(pdc, encname);
    pdc_insert_encoding_vector(pdc, ev);
    font->enc = pdc_find_encoding(pdc, encname);
    ev->flags |= PDC_ENC_FONT;

    return ev;
}

int
fnt_check_weight(int weight)
{
    if (weight == PDC_KEY_NOTFOUND)
        weight = FNT_FW_NORMAL;

    if (weight > 1000)
        weight = 1000;

    if (weight <= 10)
        weight *= 100;
    else
        weight = 100 * (weight / 100);

    return weight;
}

static const pdc_keyconn fnt_fontweight_keylist[] =
{
    {"None",        FNT_FW_DONTCARE},
    {"Thin",        FNT_FW_THIN},
    {"Extralight",  FNT_FW_EXTRALIGHT},
    {"Ultralight",  FNT_FW_ULTRALIGHT},
    {"Light",       FNT_FW_LIGHT},
    {"Normal",      FNT_FW_NORMAL},
    {"Regular",     FNT_FW_REGULAR},
    {"",            FNT_FW_REGULAR},
    {"Medium",      FNT_FW_MEDIUM},
    {"Semibold",    FNT_FW_SEMIBOLD},
    {"Semi",        FNT_FW_SEMIBOLD},
    {"Demibold",    FNT_FW_DEMIBOLD},
    {"Bold",        FNT_FW_BOLD},
    {"Extrabold",   FNT_FW_EXTRABOLD},
    {"Extra",       FNT_FW_EXTRABOLD},
    {"Ultrabold",   FNT_FW_ULTRABOLD},
    {"Heavy",       FNT_FW_HEAVY},
    {"Black",       FNT_FW_BLACK},
    {"0",           FNT_FW_DONTCARE},
    {"100",         FNT_FW_THIN},
    {"200",         FNT_FW_EXTRALIGHT},
    {"300",         FNT_FW_LIGHT},
    {"400",         FNT_FW_NORMAL},
    {"500",         FNT_FW_MEDIUM},
    {"600",         FNT_FW_SEMIBOLD},
    {"700",         FNT_FW_BOLD},
    {"800",         FNT_FW_EXTRABOLD},
    {"900",         FNT_FW_BLACK},

    {NULL, 0}
};

int
fnt_weightname2weight(const char *weightname)
{
    return pdc_get_keycode_ci(weightname, fnt_fontweight_keylist);
}

const char *
fnt_weight2weightname(int weight)
{
    return pdc_get_keyword(weight, fnt_fontweight_keylist);
}

int
fnt_macfontstyle2weight(int macfontstyle)
{
    return (macfontstyle & (1<<0)) ? FNT_FW_BOLD : FNT_FW_NORMAL;
}

#define FNT_STEMV_WEIGHT 65.0

int
fnt_weight2stemv(int weight)
{
    double w = weight / FNT_STEMV_WEIGHT;
    return (int) (FNT_STEMV_MIN + w * w + 0.5);
}

int
fnt_stemv2weight(int stemv)
{
    double w;
    int weight = 0;

    w = (double) (stemv - FNT_STEMV_MIN);
    if (w > 0)
        weight = (int) (FNT_STEMV_WEIGHT * sqrt(w) + 0.5);

    return weight;
}

void
fnt_font_logg_protocol(pdc_core *pdc, fnt_font *font)
{
    if (font != NULL &&
        pdc_logg_is_enabled(pdc, 2, trc_font))
    {
        const char *wname = fnt_weight2weightname(font->weight);
        char dwname[16];

        dwname[0] = 0;
        if (wname && *wname)
            sprintf(dwname, " (%s)", wname);

        pdc_logg(pdc,
                  "\n"
                  "\t\tFont type: %s\n"
                  "\t\tFlags: %d\n"
                  "\t\tFontBBox: %g,%g  %g,%g\n"
                  "\t\titalicAngle: %g\n"
                  "\t\tisFixedPitch: %d\n"
                  "\t\tunderlinePosition: %d\n"
                  "\t\tunderlineThickness: %d\n"
                  "\t\tcapHeight: %d\n"
                  "\t\txHeight: %d\n"
                  "\t\tascender: %d\n"
                  "\t\tdescender: %d\n"
                  "\t\tlinegap: %d\n"
                  "\t\tweight: %d%s\n"
                  "\t\tStdVW: %d\n"
                  "\t\tStdHW: %d\n"
                  "\t\tdefWidth: %d\n",
                  fnt_get_pdf_fonttype_name(font->m.type),
                  font->m.flags,
                  font->m.llx, font->m.lly, font->m.urx, font->m.ury,
                  font->m.italicAngle, font->m.isFixedPitch,
                  font->m.underlinePosition, font->m.underlineThickness,
                  font->m.capHeight, font->m.xHeight, font->m.ascender,
                  font->m.descender, font->linegap, font->weight,
                  dwname,
                  font->m.StdVW, font->m.StdHW,
                  font->m.defwidth);
    }
}


