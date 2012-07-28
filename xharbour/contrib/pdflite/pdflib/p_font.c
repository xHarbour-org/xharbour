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
 * PDFlib font handling routines
 *
 */

#define P_FONT_C

#include "p_intern.h"
#include "p_color.h"
#include "p_defopt.h"
#include "p_font.h"
#include "ft_truetype.h"


#define PDF_TTC_SEPARATOR ':'

static const pdc_keyconn pdf_fonttype_pdfkeylist[] =
{
    {"Type1",    fnt_Type1},
    {"MMType1",  fnt_MMType1},
    {"TrueType", fnt_TrueType},
    {"Type0",    fnt_CIDFontType2},
    {"Type1",    fnt_Type1C},
    {"Type0",    fnt_CIDFontType0},
    {"Type3",    fnt_Type3},
    {NULL, 0}
};

typedef enum
{
    font_afm  = 1,
    font_pfm  = 2,
    font_ttot = 3,
    font_pfab = 4
}
pdf_fontfile_type;

static const pdc_keyconn pdf_extension_names[] =
{
    {".tte", font_ttot},
    {".ttf", font_ttot},
    {".otf", font_ttot},
    {".afm", font_afm},
    {".pfm", font_pfm},
    {".ttc", font_ttot},
    {".TTE", font_ttot},
    {".TTF", font_ttot},
    {".OTF", font_ttot},
    {".AFM", font_afm},
    {".PFM", font_pfm},
    {".TTC", font_ttot},
    {".pfa", font_pfab},
    {".pfb", font_pfab},
    {".PFA", font_pfab},
    {".PFB", font_pfab},
    {NULL, 0}
};

static const pdc_keyconn pdf_fontoption_keylist[] =
{
    {"fontname",       fo_fontname},
    {"encoding",       fo_encoding},
    {"fontstyle",      fo_fontstyle},
    {"monospace",      fo_monospace},
    {NULL, 0}
};


void
pdf_init_font(PDF *p, pdf_font *font, pdf_font_options *fo)
{
    (void) p;

    /* font metric */
    fnt_init_font(&font->ft);

    /* font options */
    font->opt = *fo;
    font->verbose = fo->fontwarning;

    font->apiname = NULL;
    font->filename = NULL;
    font->metricfilename = NULL;

    font->used_in_formfield = pdc_false;
    font->used_in_current_doc = pdc_false;
    font->used_on_current_page = pdc_false;
    font->obj_id = PDC_BAD_ID;

    font->cff_offset = 0;
    font->cff_length = 0;

    font->t3font = NULL;
    font->hasoriginal = pdc_false;

    font->encapiname = NULL;
    font->outcmapname = NULL;
    font->codepage = 0;
    font->towinansi = pdc_invalidenc;
    font->hasnomac = pdc_false;
    font->passthrough = pdc_false;
    font->unibyte = pdc_false;
    font->asciispace = pdc_false;
    font->issemantic = pdc_false;
    font->widthsmissing = pdc_false;
    font->missingglyphs = 0;
    font->metricflags = 0;
    font->supplement = 0;
    font->symenc = pdc_invalidenc;
    font->replacementchar = -1;
    font->replacementcode = -1;

    font->codesize = 1;
    font->lastcode = -1;
    font->gid0code = -1;
    font->usedgids = NULL;
    font->expectglyphs = pdc_false;
    font->iscidfont = pdc_false;

    font->widths = NULL;
    font->numwidths = 0;
    font->konlydef = pdc_false;

}

void
pdf_cleanup_font(PDF *p, pdf_font *font)
{
    if (font->ft.imgname)
        pdc_unlock_pvf(p->pdc, font->ft.imgname);

    /* font metric */
    fnt_cleanup_font(p->pdc, &font->ft);

    if (font->apiname != NULL)
    {
        pdc_free(p->pdc, font->apiname);
        font->apiname = NULL;
    }

    if (font->metricfilename != NULL)
    {
        pdc_free(p->pdc, font->metricfilename);
        font->metricfilename = NULL;
    }

    if (font->encapiname != NULL)
    {
        pdc_free(p->pdc, font->encapiname);
        font->encapiname = NULL;
    }

    if (font->outcmapname != NULL)
    {
        pdc_free(p->pdc, font->outcmapname);
        font->outcmapname = NULL;
    }


    if (font->usedgids != NULL)
    {
        pdc_free(p->pdc, font->usedgids);
        font->usedgids = NULL;
    }

    /* Type3 font */
    if (font->t3font != NULL && font->hasoriginal)
    {
        pdf_cleanup_t3font(p, font->t3font);
        pdc_free(p->pdc, font->t3font);
        font->t3font = NULL;
    }

    if (font->widths != NULL)
    {
        pdc_free(p->pdc, font->widths);
        font->widths = NULL;
    }

}

void
pdf_init_fonts(PDF *p)
{
    p->fonts = NULL;
    p->fonts_number = 0;
    p->fonts_capacity = 0;
    p->t3slot = -1;


    pdc_init_encoding_info_ids(p->pdc);
}

void
pdf_cleanup_fonts(PDF *p)
{
    int slot;

    if (p->fonts != NULL)
    {
        for (slot = 0; slot < p->fonts_number; slot++)
            pdf_cleanup_font(p, &p->fonts[slot]);

        pdc_free(p->pdc, p->fonts);
        p->fonts = NULL;
    }

}

int
pdf_insert_font(PDF *p, pdf_font *font)
{
    static const char fn[] = "pdf_insert_font";
    int slot = p->fonts_number;

    /* insert font */
    if (p->fonts_number == p->fonts_capacity)
    {
        if (p->fonts_capacity == 0)
        {
            p->fonts_capacity = FONTS_CHUNKSIZE;
            p->fonts = (pdf_font *) pdc_calloc(p->pdc,
                        sizeof(pdf_font) * p->fonts_capacity, fn);
        }
        else
        {
            p->fonts_capacity *= 2;
            p->fonts = (pdf_font *) pdc_realloc(p->pdc, p->fonts,
                        sizeof(pdf_font) * p->fonts_capacity, fn);
        }
    }
    p->fonts[slot] = *font;

    p->fonts_number++;

    return slot;
}


const char *
pdf_get_pdf_fontname(pdf_font *font)
{
    const char *fontname;

    fontname = fnt_get_abb_std_fontname(font->ft.name);
    if (fontname == NULL)
        fontname = fnt_get_abb_cjk_fontname(font->ft.name);
    if (fontname == NULL)
        fontname = font->ft.name;

    return (const char *) fontname;
}

const char *
pdf_get_encoding_name(PDF *p, pdc_encoding enc, pdf_font *font)
{
    const char *apiname = pdc_get_fixed_encoding_name(enc);
    if (!apiname[0] && enc >= 0)
    {
        pdc_encodingvector *ev = pdc_get_encoding_vector(p->pdc, enc);
        apiname = (const char *) ev->apiname;
    }
    else if (enc == pdc_cid && font != NULL && font->outcmapname != NULL)
        apiname = (const char *) font->outcmapname;
    return apiname;
}

char *
pdf_get_encoding_adaptname(PDF *p, pdc_encoding enc, pdf_font *font,
                           const char *fontname)
{
    static const char *fn = "pdf_get_encoding_adaptname";
    char *encname = (char *) pdf_get_encoding_name(p, enc, font);
    char *adaptname = NULL;
    size_t len;

    len = strlen(encname) + 1 + strlen(fontname) + 1;
    adaptname = (char *) pdc_malloc_tmp(p->pdc, len, fn, 0, 0);
    strcpy(adaptname, encname);
    strcat(adaptname, PDC_ENC_MODSEPAR);
    strcat(adaptname, fontname);

    return adaptname;
}

pdc_encodingvector *
pdf_create_font_encoding(PDF *p, pdc_encoding enc, pdf_font *font,
                         const char *fontname, pdc_bool kreg)
{
    pdc_encodingvector *ev = NULL;
    char *adaptname = NULL;

    adaptname = pdf_get_encoding_adaptname(p, enc, font, fontname);

    /* search for a registered encoding */
    enc = pdc_find_encoding(p->pdc, adaptname);
    if (enc != pdc_invalidenc)
    {
        font->ft.enc = enc;
    }
    else
    {
        /* create a font encoding */
        ev = pdc_new_encoding(p->pdc, adaptname);
        ev->flags |= PDC_ENC_FONT;
        ev->flags |= PDC_ENC_SETNAMES;

        if (kreg)
        {
            enc = pdc_insert_encoding_vector(p->pdc, ev);
            font->ft.enc = enc;
        }
    }

    pdc_free_tmp(p->pdc, adaptname);

    return ev;
}

const char *
pdf_get_font_char_option(PDF *p, pdf_font_optflags fflags)
{
    pdf_text_options *to = p->curr_ppt->currto;
    pdf_font *currfont;

    if (p->fonts_number == 0 || to->font == -1)
        pdc_error(p->pdc, PDF_E_TEXT_NOFONT_PAR,
                  pdc_get_keyword(fflags, pdf_fontoption_keylist), 0, 0, 0);
    currfont = &p->fonts[to->font];

    switch (fflags)
    {
        case fo_fontname:
        return (const char *) currfont->ft.name;

        case fo_encoding:
        return pdf_get_encoding_name(p, currfont->ft.enc, currfont);

        case fo_fontstyle:
        return pdc_get_keyword(currfont->opt.fontstyle,
                               pdf_fontstyle_pdfkeylist);

        default:
        return NULL;
    }
}

double
pdf_get_font_float_option(PDF *p, pdf_font_optflags fflags)
{
    pdf_text_options *to = p->curr_ppt->currto;
    pdf_font *currfont;

    if (p->fonts_number == 0 || to->font == -1)
        pdc_error(p->pdc, PDF_E_TEXT_NOFONT_PAR,
                  pdc_get_keyword(fflags, pdf_fontoption_keylist), 0, 0, 0);
    currfont = &p->fonts[to->font];

    switch (fflags)
    {
        case fo_monospace:
        return (double) currfont->opt.monospace;

        default:
        return 0;
    }
}

static const pdc_keyconn pdf_courier_keylist[] =
{
    {"Courier",               fnt_Normal},
    {"Courier-Bold",          fnt_Bold},
    {"Courier-Oblique",       fnt_Italic},
    {"Courier-BoldOblique",   fnt_BoldItalic},
    {NULL, 0}
};

static const pdc_keyconn pdf_helvetica_keylist[] =
{
    {"Helvetica",             fnt_Normal},
    {"Helvetica-Bold",        fnt_Bold},
    {"Helvetica-Oblique",     fnt_Italic},
    {"Helvetica-BoldOblique", fnt_BoldItalic},
    {NULL, 0}
};

static const pdc_keyconn pdf_times_keylist[] =
{
    {"Times-Roman",           fnt_Normal},
    {"Times-Bold",            fnt_Bold},
    {"Times-Italic",          fnt_Italic},
    {"Times-BoldItalic",      fnt_BoldItalic},
    {NULL, 0}
};

static const char *
pdf_get_fontname_core(pdf_font *font, const char *fontname, pdc_bool checktimes)
{
    const char *fname = NULL;

    /* font style for core fonts */
    if (font->opt.fontstyle != fnt_Normal)
    {
        if (!strcmp(fontname, "Courier"))
            fname = pdc_get_keyword(font->opt.fontstyle, pdf_courier_keylist);
        else if (!strcmp(fontname, "Helvetica"))
            fname = pdc_get_keyword(font->opt.fontstyle, pdf_helvetica_keylist);
        else if (!strcmp(fontname, "Times-Roman"))
            fname = pdc_get_keyword(font->opt.fontstyle, pdf_times_keylist);
    }

    if (checktimes)
    {
        if (!strcmp(fontname, "Times"))
            fname = pdc_get_keyword(font->opt.fontstyle, pdf_times_keylist);
    }

    return fname;
}

static pdc_bool
pdf_get_metrics_core(PDF *p, pdf_font *font, const char *fontname,
             const char *outfilename, pdc_encoding enc, pdc_bool checktimes)
{
    static const char fn[] = "pdf_get_metrics_core";
    const char *fname = NULL;
    const fnt_font_metric *ftm;

    fname = pdf_get_fontname_core(font, fontname, checktimes);
    if (fname != NULL)
    {
        fontname = fname;
        font->opt.fontstyle = fnt_Normal;

        if (font->apiname != NULL)
        {
            pdc_free(p->pdc, font->apiname);
            font->apiname = pdc_strdup_ext(p->pdc, fontname, 0, fn);
        }
    }

    ftm = fnt_get_core_metric(fontname);
    if (ftm != NULL && (!font->opt.embedding || outfilename != NULL))
    {
        pdc_logg_cond(p->pdc, 1, trc_font,
            "\tLoading metrics data for core font \"%s\":\n", fontname);

        /* Fill up the font struct */
        fnt_fill_font_metric(p->pdc, &font->ft,
                             pdc_false,
                             ftm);
        font->ft.enc = enc;

        /* all new glyph names of AGL 2.0 are missing */
        font->missingglyphs = 0xFFFFFFFF;

        /* Process metrics data */
        if (pdf_process_metrics_data(p, font, fontname))
        {
            if (pdf_make_fontflag(p, font))
            {
                if (!font->opt.monospace)
                    return pdc_true;
                else
                    pdc_set_errmsg(p->pdc, PDC_E_OPT_IGNORED, "monospace",
                                   0, 0, 0);
            }
        }

        return pdc_false;
    }

    return pdc_undef;
}

void
pdf_font_set_missvalues(PDF *p, pdf_font *font)
{
    pdf_font_options *fo = &font->opt;
    fnt_font_metric *ftm = &font->ft.m;

    (void) p;

    if (ftm->descender > 0)
        ftm->descender = -(ftm->descender);

    if (fo->mask & (1L << fo_ascender))
    {
        font->metricflags |= font_ascender;
        ftm->ascender = fo->ascender;
    }
    else if (ftm->ascender <= 0)
    {
        font->metricflags |= font_ascender;
        ftm->ascender = 720;
    }

    if (fo->mask & (1L << fo_descender))
    {
        font->metricflags |= font_descender;
        ftm->descender = fo->descender;
    }
    else if (ftm->descender == FNT_MISSING_FONTVAL)
    {
        font->metricflags |= font_descender;
        ftm->descender = (int) PDC_ROUND(-0.25 * ftm->ascender);
    }

    if (fo->mask & (1L << fo_capheight))
    {
        font->metricflags |= font_capheight;
        ftm->capHeight = fo->capheight;
    }
    else if (ftm->capHeight <= 0)
    {
        font->metricflags |= font_capheight;
        ftm->capHeight = (int) PDC_ROUND(0.93 * ftm->ascender);
    }

    if (fo->mask & (1L << fo_xheight))
    {
        font->metricflags |= font_xheight;
        ftm->xHeight = fo->xheight;
    }
    else if (ftm->xHeight <= 0)
    {
        font->metricflags |= font_xheight;
        ftm->xHeight = (int) PDC_ROUND(0.66 * ftm->ascender);
    }

    if (fo->mask & (1L << fo_linegap))
    {
        font->metricflags |= font_linegap;
        font->ft.linegap = fo->linegap;
    }
    else if (font->ft.linegap == FNT_MISSING_FONTVAL)
    {
        font->metricflags |= font_linegap;
        font->ft.linegap = (int) PDC_ROUND(0.23 * ftm->ascender);
    }

    if (ftm->llx == FNT_MISSING_FONTVAL)
        ftm->llx = -50;
    if (ftm->lly == FNT_MISSING_FONTVAL)
        ftm->lly = ftm->descender;
    if (ftm->urx == FNT_MISSING_FONTVAL)
        ftm->urx = 1000;
    if (ftm->ury == FNT_MISSING_FONTVAL)
        ftm->ury = ftm->ascender;

    /* try to fix broken entries */
    if (ftm->lly > ftm->ury)
        ftm->ury = ftm->lly + ftm->ascender;
    if (ftm->llx > ftm->urx)
        ftm->urx = ftm->llx + 1000;
}

pdc_bool
pdf_font_get_is_faked(pdf_font *font, pdf_font_values flag)
{
    return (font->metricflags & flag) ? pdc_true : pdc_false;
}

double
pdf_font_get_metric_value(int value)
{
    return (double) value / 1000.0;
}


/* --------------------------- font processing ---------------------------- */

pdc_bool
pdf_make_fontflag(PDF *p, pdf_font *font)
{
    int errcode = 0;

    if (font->ft.m.type != fnt_Type3)
    {
        if (font->ft.m.isFixedPitch)
            font->ft.m.flags |= FNT_FIXEDWIDTH;

        if (font->ft.issymbfont == pdc_false ||
            font->ft.enc == pdc_winansi ||
            font->ft.enc == pdc_macroman ||
            font->ft.enc == pdc_ebcdic ||
            font->ft.enc == pdc_ebcdic_37 ||
            font->ft.enc == pdc_ebcdic_winansi)
            font->ft.m.flags |= FNT_ADOBESTANDARD;
        else
            font->ft.m.flags |= FNT_SYMBOL;

        if (font->ft.m.italicAngle < 0 ||
            font->opt.fontstyle == fnt_Italic ||
            font->opt.fontstyle == fnt_BoldItalic)
            font->ft.m.flags |= FNT_ITALIC;
        if (font->ft.m.italicAngle == 0 &&
            font->ft.m.flags & FNT_ITALIC)
            font->ft.m.italicAngle = FNT_DEF_ITALICANGLE;

        /* heuristic to identify (small) caps fonts */
        if (font->ft.name &&
            (strstr(font->ft.name, "Caps") ||
            !strcmp(font->ft.name + strlen(font->ft.name) - 2, "SC")))
            font->ft.m.flags |= FNT_SMALLCAPS;

        if (font->opt.fontstyle == fnt_Bold ||
            font->opt.fontstyle == fnt_BoldItalic)
            font->ft.weight = FNT_FW_BOLD;

        if (strstr(font->ft.name, "Bold") ||
            font->ft.weight >= FNT_FW_BOLD)
            font->ft.m.flags |= FNT_FORCEBOLD;

        /* determine values for FontWeight to StemV */
        if (font->ft.m.StdVW == 0)
            font->ft.m.StdVW = fnt_weight2stemv(font->ft.weight);
        else if (font->ft.weight == 0)
            font->ft.weight = fnt_stemv2weight(font->ft.m.StdVW);
    }

    fnt_font_logg_protocol(p->pdc, &font->ft);

    switch(font->ft.m.type)
    {
        case fnt_Type1:
        case fnt_MMType1:
        case fnt_Type3:
        if (font->opt.fontstyle == fnt_Bold ||
            font->opt.fontstyle == fnt_BoldItalic)
        {
            font->metricflags |= font_bold;
        }

        if (font->opt.fontstyle == fnt_Italic ||
            font->opt.fontstyle == fnt_BoldItalic)
        {
            font->metricflags |= font_italic;
        }

        break;

        default:
        if (font->opt.embedding)
        {
            if (font->opt.fontstyle == fnt_Bold ||
                font->opt.fontstyle == fnt_BoldItalic)
            {
                font->metricflags |= font_bold;
            }

            if (font->opt.fontstyle == fnt_Italic ||
                font->opt.fontstyle == fnt_BoldItalic)
            {
                font->metricflags |= font_italic;
            }
        }
        break;
    }


    return errcode ? pdc_false : pdc_true;
}

int
pdf_get_code_or_glyphid(PDF *p, pdf_font *font, pdc_encodingvector *ev,
                        pdc_ushort uv)
{
    if (ev != NULL)
    {
        int code = pdc_get_encoding_bytecode(p->pdc, ev, uv);

        if (code >= 0)
        {
            if (fnt_get_glyphid(code, &font->ft) <= 0)
                code = 0;
        }
        return code;
    }

    return fnt_get_glyphid((int) uv, &font->ft);
}

void
pdf_set_replchar(PDF *p, pdf_font *font)
{
    pdc_encoding enc = font->ft.enc;

    switch (enc)
    {
        case pdc_glyphid:
        case pdc_cid:
        return;

        case pdc_builtin:
            font->replacementcode = 0;
        return;

        case pdc_unicode:
        default:
        {
            pdc_encodingvector *ev = pdc_get_encoding_vector(p->pdc, enc);
            pdc_ushort uv = 0;
            int cg = 0;
                uv = PDC_UNICODE_NBSP;
                cg = pdf_get_code_or_glyphid(p, font, ev, uv);
                if (cg <= 0)
                {
                    uv = PDC_UNICODE_SPACE;
                    cg = pdf_get_code_or_glyphid(p, font, ev, uv);
                    if (cg <= 0)
                    {
                        uv = 0;
                        cg = 0;
                    }
                }

            font->replacementchar = (int) uv;
            font->replacementcode = cg;
        }
        return;
    }
}

void
pdf_font_issemantic(PDF *p, pdf_font *font)
{
    pdc_encoding enc = font->ft.enc;
    pdc_ushort spacechar = 0;

    /* Flag: encoding with ASCII space for wordspacing */
    if (enc >= 0)
    {
        pdc_encodingvector *ev = pdc_get_encoding_vector(p->pdc, enc);
        int i;

        ev->flags |= PDC_ENC_USED;
        i = pdc_get_encoding_bytecode(p->pdc, ev, PDC_UNICODE_SPACE);
        if (i > -1)
        {
            spacechar = (pdc_ushort) i;
            if (spacechar == PDC_UNICODE_SPACE)
                font->asciispace = pdc_true;
        }
    }

    /* Flag: encoding is Unicode interpretable */
    if ((enc >= 0) ||
        (enc == pdc_cid && font->codesize == 2) ||
        (enc == pdc_unicode))
        font->issemantic = pdc_true;

    /* determine code of space character */
    switch(enc)
    {
        case pdc_cid:
        if (font->codesize == 2)
            font->ft.spacechar = PDC_UNICODE_SPACE;
        break;

        case pdc_unicode:
        font->ft.spacechar = PDC_UNICODE_SPACE;
        break;

        case pdc_glyphid:
        font->ft.spacechar =
            (pdc_ushort) MAX(fnt_get_glyphid(PDC_UNICODE_SPACE, &font->ft), 0);
        break;

        default:
        font->ft.spacechar = spacechar;
        break;
    }
}

/* definitions of font options */
static const pdc_defopt pdf_load_font_options[] =
{
    PDF_FONT_OPTIONS2
    PDF_FONT_OPTIONS3
    PDF_ERRORPOLICY_OPTION
    PDC_OPT_TERMINATE
};

void
pdf_init_font_options(PDF *p, pdf_font_options *fo)
{
    static const char fn[] = "pdf_init_font_options";

    if (fo == NULL)
    {
        p->currfo = (pdf_font_options *) pdc_malloc(p->pdc,
                        sizeof(pdf_font_options), fn);


        fo = p->currfo;
    }
    else
    {
    }


    fo->embedding = pdc_false; /* default true if CID custom font */
    fo->encoding = NULL;
    fo->flags = 0;
    fo->fontname = NULL;
    fo->fontstyle = fnt_Normal;
    fo->fontwarning = p->debug[(int) 'F'];
    fo->fontwarning = pdf_get_errorpolicy(p, NULL, fo->fontwarning);
    fo->mask = 0;
    fo->monospace = 0;
    fo->ascender = 0;
    fo->descender = 0;
    fo->capheight = 0;
    fo->xheight = 0;
    fo->linegap = 0;
    fo->auxiliary = pdc_false;
    fo->dropcorewidths = pdc_false;

}

void
pdf_cleanup_font_curroptions(PDF *p)
{
    if (p->currfo)
    {
        pdc_free(p->pdc, p->currfo);
        p->currfo = NULL;
    }
}

void
pdf_cleanup_font_options(PDF *p, pdf_font_options *fo)
{
    if (fo->fontname != NULL)
    {
        pdc_free(p->pdc, fo->fontname);
        fo->fontname = NULL;
    }

    if (fo->encoding != NULL)
    {
        pdc_free(p->pdc, fo->encoding);
        fo->encoding = NULL;
    }
}

void
pdf_parse_font_options(PDF *p, const char *optlist)
{
    pdc_resopt *resopts = pdc_parse_optionlist(p->pdc, optlist,
                           pdf_load_font_options, NULL, pdc_true);

    pdf_get_font_options(p, p->currfo, resopts);
    pdc_cleanup_optionlist(p->pdc, resopts);
}

void
pdf_get_font_options(PDF *p, pdf_font_options *fo, pdc_resopt *resopts)
{
    int inum;

    (void) p;

    if (fo->flags & is_block ||
        fo->flags & is_textline ||
        fo->flags & is_textflow)
    {
        if (pdc_get_optvalues("fontname", resopts, NULL, NULL))
        {
            fo->fontname = (char *)pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
            fo->mask |= (1L << fo_fontname);
        }

        if (pdc_get_optvalues("encoding", resopts, NULL, NULL))
        {
            fo->encoding = (char *)pdc_save_lastopt(resopts, PDC_OPT_SAVE1ELEM);
            fo->mask |= (1L << fo_encoding);
        }
    }

    if (pdc_get_optvalues("fontwarning", resopts, &fo->fontwarning, NULL))
        fo->mask |= (1L << fo_fontwarning);
    fo->fontwarning = pdf_get_errorpolicy(p, resopts, fo->fontwarning);

    if (pdc_get_optvalues("embedding", resopts, &fo->embedding, NULL))
        fo->mask |= (1L << fo_embedding);


    if (pdc_get_optvalues("fontstyle", resopts, &inum, NULL))
    {
        fo->fontstyle = (fnt_fontstyle) inum;
        fo->mask |= (1L << fo_fontstyle);
    }

    if (pdc_get_optvalues("monospace", resopts, &fo->monospace, NULL))
        fo->mask |= (1L << fo_monospace);

    if (pdc_get_optvalues("ascender", resopts, &fo->ascender, NULL))
        fo->mask |= (1L << fo_ascender);

    if (pdc_get_optvalues("descender", resopts, &fo->descender, NULL))
        fo->mask |= (1L << fo_descender);

    if (pdc_get_optvalues("capheight", resopts, &fo->capheight, NULL))
        fo->mask |= (1L << fo_capheight);

    if (pdc_get_optvalues("xheight", resopts, &fo->xheight, NULL))
        fo->mask |= (1L << fo_xheight);

    if (pdc_get_optvalues("linegap", resopts, &fo->linegap, NULL))
        fo->mask |= (1L << fo_linegap);


        pdc_get_optvalues("dropcorewidths", resopts, &fo->dropcorewidths, NULL);
}

int
pdf__load_font(PDF *p, const char *fontname, int len,
               const char *encoding, const char *optlist)
{
    int slot;
    pdf_font_options fo;

    if (encoding == NULL || *encoding == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "encoding", 0, 0, 0);

    /* initialize */
    pdf_init_font_options(p, &fo);

    /* Converting fontname */
    fo.fontname = (char *) pdf_convert_name(p, fontname, len,
                                            PDC_CONV_WITHBOM);
    if (fo.fontname == NULL || *fo.fontname == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "fontname", 0, 0, 0);

    /* encoding */
    fo.encoding = (char *) pdc_strdup(p->pdc, encoding);

    /* parsing option list */
    if (optlist && strlen(optlist))
    {
        pdc_resopt *resopts;
        pdc_clientdata data;

        pdf_set_clientdata(p, &data);
        resopts = pdc_parse_optionlist(p->pdc, optlist,
                           pdf_load_font_options, &data, pdc_true);
        if (!resopts)
        {
            pdf_cleanup_font_options(p, &fo);
            return -1;
        }

        pdf_get_font_options(p, &fo, resopts);
        pdc_cleanup_optionlist(p->pdc, resopts);
    }

    slot = pdf_load_font_internal(p, &fo);
    return slot;
}

static void
pdf_check_font_identical(PDF *p, pdf_font *font, int *slot)
{
    pdf_font *oldfont = &p->fonts[*slot];
    const char *optname = NULL;

    if (!oldfont->opt.embedding && font->opt.embedding)
    {
        optname = "embedding";
        if (p->errorpolicy == errpol_legacy)
        {
            pdc_warning(p->pdc, PDF_E_FONT_NOTFULFILL, optname, optname,
                        0, 0);
        }
        else
        {
            pdc_set_errmsg(p->pdc, PDF_E_FONT_NOTFULFILL, optname, optname,
                           0, 0);
            *slot = -1;
        }
    }

}

pdc_bool
pdf_check_font_embedding(PDF *p, pdf_font *font, const char *fontname)
{
    (void) p;
    (void) font;
    (void) fontname;



    return pdc_true;
}

int
pdf_load_font_internal(PDF *p, pdf_font_options *fo)
{
    pdc_bool logg1 = pdc_logg_is_enabled(p->pdc, 1, trc_font);
    pdc_bool logg2 = pdc_logg_is_enabled(p->pdc, 2, trc_font);
    const char *fontname;
    const char *encoding;
    const char *encoding_aux;
    pdc_encoding enc = pdc_invalidenc;
    pdf_font tmpfont, *font;
    const char *filename = NULL;
    const char *extension = NULL;
    const char *outfilename = NULL;
    char *fontname_p = NULL;
    char testfilename[PDF_MAX_FONTNAME + 5];
    char *sf, *mmparam, mastername[PDF_MAX_FONTNAME + 1];
    char ittc;
    size_t len;
    pdc_bool retval = pdc_false;
    int slot = -1, i;

    /* register and skip at sign '@' in font name */
    if (fo->fontname[0] == PDF_VERTICAL_SIGN ||
        (pdc_is_utf8_bytecode(fo->fontname) &&
         fo->fontname[3] == PDF_VERTICAL_SIGN))
    {

        i = (fo->fontname[0] == PDF_VERTICAL_SIGN) ? 1 : 4;
        len = strlen(fo->fontname) + 1 - i;
        memmove(&fo->fontname[i - 1], &fo->fontname[i], len);
    }

    /* host or UTF-8 encoded font name without BOM */
    fontname_p = pdc_utf8_to_hostbytes(p->pdc, pdc_false, fo->fontname);
    if (fontname_p == NULL)
    {
        fontname = pdc_utf8strprint(p->pdc, fo->fontname);
    }
    else
    {
        fontname = pdc_utf8strprint(p->pdc, fontname_p);
        pdc_free_tmp(p->pdc, fontname_p);
    }
    fontname_p = NULL;

    if (logg1)
        pdc_logg(p->pdc, "\tCanonical font name: \"%s\"\n",
                 fontname);

    /* font encoding */
    encoding = fo->encoding;
    encoding_aux = encoding;

    /* initialize font struct */
    font = &tmpfont;
    pdf_init_font(p, font, fo);

    /* error message prefix */
    pdc_push_errmsg(p->pdc, PDF_E_FONT_PREFIX, fontname, encoding, 0, 0);




    /* API font name */
    font->apiname = pdc_strdup(p->pdc, fontname);

    /* UTF-8 font name with BOM */
    font->ft.utf8name = pdc_strdup(p->pdc, fo->fontname);

    if (logg1)
        pdc_logg(p->pdc, "\tFont UTF-8 name: \"%s\"\n",
                 font->ft.utf8name);

    /* specified encoding name */
    font->encapiname = pdc_strdup(p->pdc, encoding);

    /* search for a registered encoding */
    enc = pdc_find_encoding(p->pdc, encoding);

    if (enc == pdc_unicode || enc == pdc_glyphid)
    {
        pdc_set_errmsg(p->pdc, PDF_E_UNSUPP_UNICODE, 0, 0, 0, 0);
        goto PDF_PREMATURE_EXIT;
    }

    if (enc == pdc_invalidenc || enc == pdc_unicode)
    {
        /* search for a predefined CMap and registered fonts */
        if (!pdf_handle_cidfont(p, fontname, encoding, enc, font, &slot, &enc))
            goto PDF_PREMATURE_EXIT;

        if (enc == pdc_invalidenc)
        {
            /* search for a new encoding */
            enc = pdc_insert_encoding(p->pdc, encoding, &font->codepage,
                                      font->verbose);
            if (enc == pdc_invalidenc)
                goto PDF_PREMATURE_EXIT;
        }
        else if (enc == pdc_cid)
        {
            if (slot == -1)
                goto PDF_NEWFONT_EXIT;
            else
                goto PDF_PREMATURE_EXIT;
        }
        else if (enc == pdc_glyphid)
        {
            encoding_aux = "glyphid";
        }
        else if (enc == pdc_unicode)
        {
            encoding_aux = "unicode";
        }
    }

    if (pdc_strcmp(font->encapiname, encoding))
    {
        pdc_push_errmsg(p->pdc, PDF_E_FONT_PREFIX2,
                        fontname, font->encapiname, encoding, 0);
    }
    encoding = encoding_aux;

    encoding = pdc_get_user_encoding(p->pdc, enc);
    pdc_logg_cond(p->pdc, 1, trc_encoding, "\tFont encoding: \"%s\"\n",
                      encoding);

    if (enc == pdc_unicode || enc == pdc_glyphid)
    {
        pdc_set_errmsg(p->pdc, PDF_E_UNSUPP_UNICODE, 0, 0, 0, 0);
        goto PDF_PREMATURE_EXIT;
    }

    /*
     * Look whether font is already in the cache.
     * Look first for the auxiliary font (obj_id == -1).
     * If a font with same encoding and same relevant options is found,
     * return its handle.
     * If a Type 3 font with the same name but different encoding
     * is found, make a copy in a new slot and attach the requested encoding.
     */

    if (logg1)
        pdc_logg(p->pdc, "\tFont will be searched in the PDFlib font cache\n");
    for (slot = 0; slot < p->fonts_number; slot++)
    {
        if (p->fonts[slot].obj_id == PDC_BAD_ID &&
            p->fonts[slot].ft.m.type != fnt_Type3)
        {
            if (font->opt.auxiliary)
                goto PDF_PREMATURE_EXIT;
        }
        else if (!font->opt.auxiliary &&
                 !pdc_strcmp(p->fonts[slot].apiname, fontname) &&
                 p->fonts[slot].opt.fontstyle == font->opt.fontstyle)
        {
            if (p->fonts[slot].ft.m.type == fnt_Type3)
            {
                if (logg2)
                    pdc_logg(p->pdc, "\t\tType3 font [%d] found\n", slot);

                if (enc < pdc_winansi && enc != pdc_unicode)
                {
                    pdc_set_errmsg(p->pdc, PDF_E_FONT_BADENC, 0, 0, 0, 0);

                    slot = -1;
                    goto PDF_PREMATURE_EXIT;
                }

                if (p->fonts[slot].ft.enc != enc)
                {
                    if (!pdf_handle_t3font(p, fontname, enc, font, &slot))
                    {
                        slot = -1;
                        goto PDF_PREMATURE_EXIT;
                    }
                    if (slot > -1)
                        font = &p->fonts[slot];
                    goto PDF_NEWFONT_EXIT;
                }

                goto PDF_PREMATURE_EXIT;
            }
            else if (p->fonts[slot].opt.monospace == font->opt.monospace
                    )
            {
                if (p->fonts[slot].ft.enc == enc &&
                    p->fonts[slot].codepage == font->codepage)
                {
                    if (logg2)
                        pdc_logg(p->pdc,
                                  "\t\tfont [%d] with same encoding found\n",
                                  slot);

                    pdf_check_font_identical(p, font, &slot);
                    goto PDF_PREMATURE_EXIT;
                }
                else
                {
                    char *adaptname;
                    int kc;

                    /* Comparing apiname of encoding */
                    if (!pdc_stricmp(font->encapiname,
                                     p->fonts[slot].encapiname) &&
                        !pdc_stricmp(font->ft.cmapname,
                                     p->fonts[slot].ft.cmapname))
                    {
                        if (logg2)
                            pdc_logg(p->pdc,
                                      "\t\tfont [%d] with same encoding "
                                      "apiname '%s' found\n", slot, encoding);

                        pdf_check_font_identical(p, font, &slot);
                        goto PDF_PREMATURE_EXIT;
                    }

                    /* Name of adapted to font encoding */
                    adaptname =
                        pdf_get_encoding_adaptname(p, enc, font, fontname);
                    kc = strcmp(adaptname, pdf_get_encoding_name(p,
                                p->fonts[slot].ft.enc, &p->fonts[slot]));
                    if (!kc)
                    {
                        if (logg2)
                            pdc_logg(p->pdc,
                                      "\t\tfont [%d] with same internal "
                                      "encoding name '%s' found\n",
                                      slot, adaptname);
                        pdc_free_tmp(p->pdc, adaptname);

                        pdf_check_font_identical(p, font, &slot);
                        goto PDF_PREMATURE_EXIT;
                    }
                    pdc_free_tmp(p->pdc, adaptname);
                }
            }
        }
        else if (!font->opt.auxiliary &&
                 p->fonts[slot].ft.m.type == fnt_Type1 &&
                 p->fonts[slot].ft.isstdfont && p->fonts[slot].ft.enc == enc)
        {
            /* different core font specifications */
            const char *fname = pdf_get_fontname_core(font, fontname, pdc_true);

            if ((fname != NULL && !strcmp(fname, p->fonts[slot].ft.name) &&
                 p->fonts[slot].opt.fontstyle == fnt_Normal) ||
                (!strcmp(fontname, p->fonts[slot].ft.name) &&
                 p->fonts[slot].opt.fontstyle == font->opt.fontstyle))
            {
                if (logg2)
                    pdc_logg(p->pdc,
                              "\t\tfont [%d] with same font style '%s' found\n",
                              slot, pdc_get_keyword(font->opt.fontstyle,
                                                    pdf_fontstyle_pdfkeylist));

                pdf_check_font_identical(p, font, &slot);
                goto PDF_PREMATURE_EXIT;
            }
        }
    }

    slot = -1;
    if (logg1)
        pdc_logg(p->pdc, "\tFont not found in the PDFlib font cache\n");

    /* embedding check */
    if (!pdf_check_font_embedding(p, font, fontname))
    {
        goto PDF_PREMATURE_EXIT;
    }

    /* Multiple Master handling:
     * - strip MM parameters to build the master name
     * - the master name is used to find the metrics
     * - the instance name (client-supplied font name) is used in all places
     * - although the master name is used for finding the metrics, the
     *   instance name is stored in the font struct.
     */

    len = strlen(fontname);
    if (len > PDF_MAX_FONTNAME)
    {
        pdc_set_errmsg(p->pdc, FNT_E_FONT_NAMETOOLONG,
            pdc_errprintf(p->pdc, "%d", PDF_MAX_FONTNAME), 0, 0, 0);
        goto PDF_PREMATURE_EXIT;
    }
    strcpy(mastername, fontname);

    /* A Multiple Master font was requested */
    if ((mmparam = strstr(mastername, "MM_")) != NULL)
    {
        if (font->opt.embedding)
        {
            pdc_set_errmsg(p->pdc, PDF_E_FONT_EMBEDMM, 0, 0, 0, 0);
            goto PDF_PREMATURE_EXIT;
        }
        mmparam[2] = '\0';      /* strip the parameter from the master name */
    }
    fontname_p = mastername;

    /* protocol */
    if (logg1)
        pdc_logg(p->pdc, "\tPDFlib font name: \"%s\"\n", fontname_p);

    /* Font file search hierarchy
     * - Check "FontOutline" resource entry and check TrueType font
     * - Check "FontAFM" resource entry
     * - Check "FontPFM" resource entry
     * - Check "HostFont" resource entry
     * - Check available in-core metrics
     * - Check host font
     */
    retval = pdc_false;
    while (1)
    {
        if (font->opt.auxiliary)
        {
            /* only in-core fonts are possible */
            retval = pdf_get_metrics_core(p, font, fontname_p, "", enc,
                                          pdc_false);
            break;
        }

#ifdef PDF_TRUETYPE_SUPPORTED
        /* Check specified TrueType file */
        filename = pdc_find_resource(p->pdc, "FontOutline", fontname_p);
        if (!filename)
        {
            /* check for TTC font names with index */
            ittc = PDF_TTC_SEPARATOR;
            sf = strrchr(fontname_p, ittc);

            if (sf != NULL)
            {
                *sf = 0;
                filename = pdc_find_resource(p->pdc, "FontOutline", fontname_p);
                *sf = ittc;
            }
        }
        if (filename)
        {
            outfilename = filename;
            retval = fnt_check_tt_font(p->pdc, filename, fontname_p, &font->ft,
                                       pdc_false);
            if (retval == pdc_true)
            {
                retval = pdf_get_metrics_tt(p, font, fontname_p, enc, filename);
                break;
            }
            else if (retval == pdc_undef &&
                     pdc_get_errnum(p->pdc) == PDC_E_IO_RDOPEN_NF)
            {
                /* file must be exist */
                retval = pdc_false;
            }
            if (retval == pdc_false)
                break;
        }
#endif /* PDF_TRUETYPE_SUPPORTED */

        /* Check specified AFM file */
        filename = pdc_find_resource(p->pdc, "FontAFM", fontname_p);
        if (filename)
        {
            retval = pdf_get_metrics_afm(p, font, fontname_p, enc, filename,
                                         pdc_true);
            break;
        }

        /* Check specified PFM file */
        filename = pdc_find_resource(p->pdc, "FontPFM", fontname_p);
        if (filename)
        {
            retval = pdf_get_metrics_pfm(p, font, fontname_p, enc, filename,
                                         pdc_true);
            break;
        }



        /* Check available in-core metrics - will be skipped
         * in the case of embedding and missing outline file
         * to check the possibility of an host font in the next step.
         */
        retval = pdf_get_metrics_core(p, font, fontname_p, outfilename, enc,
                                      pdc_false);
        if (retval != pdc_undef)
            break;
        retval = pdc_false;


        /* Check available in-core metrics */
        retval = pdf_get_metrics_core(p, font, fontname_p, "", enc, pdc_true);
        if (retval != pdc_undef)
            break;
        retval = pdc_false;

        /* Searching for a metric file */
        if (logg1)
            pdc_logg(p->pdc, "\tSearching for font metrics data file:\n");

        filename = testfilename;
        for (i = 0; i < 100; i++)
        {
            extension = pdf_extension_names[i].word;
            if (!extension)
                break;

            strcpy(testfilename, fontname_p);
            sf = strrchr(testfilename, PDF_TTC_SEPARATOR);
            if (sf != NULL)
                *sf = 0;
            strcat(testfilename, extension);

            switch (pdf_extension_names[i].code)
            {
#ifdef PDF_TRUETYPE_SUPPORTED
                case font_ttot:
                retval = fnt_check_tt_font(p->pdc, filename, fontname_p,
                                           &font->ft, pdc_false);
                if (retval == pdc_true)
                    retval = pdf_get_metrics_tt(p, font, fontname_p, enc,
                                                filename);
                break;
#endif /* PDF_TRUETYPE_SUPPORTED */

                case font_afm:
                retval = pdf_get_metrics_afm(p, font, fontname_p, enc,
                                             filename, pdc_false);
                break;

                case font_pfm:
                retval = pdf_get_metrics_pfm(p, font, fontname_p, enc,
                                             filename, pdc_false);
                break;

                default:
                break;
            }

            /* file found or error */
            if (retval != pdc_undef)
            {
                if (retval == pdc_true)
                    if (pdf_extension_names[i].code == font_ttot)
                        outfilename = filename;
                break;
            }
        }

        if (retval == pdc_undef)
	{
            retval = pdc_false;

            if (logg1)
                pdc_logg(p->pdc,
                         "\tMetric data file for font \"%s\" not available\n",
                         fontname_p);
            pdc_set_errmsg(p->pdc, PDF_E_FONT_NOMETRICS, 0, 0, 0, 0);
        }

        break;
    }

    /* metrics data search finished */

    if (retval == pdc_false)
    {
        goto PDF_PREMATURE_EXIT;
    }

    /* store instance name instead of master name in the font structure */
    if (mmparam)
    {
        pdc_free(p->pdc, font->ft.name);
        font->ft.name = pdc_strdup(p->pdc, fontname);
        pdc_free(p->pdc, font->ft.m.name);
        font->ft.m.name = pdc_strdup(p->pdc, fontname);
    }

    /* If embedding was requested, check font file (or raise an exception) */
    if (font->opt.embedding)
    {
        if (font->ft.img == NULL)
        {
            retval = pdc_undef;

            if (outfilename)
            {
                /* Font outline file specified */
                if (font->ft.m.type == fnt_Type1 ||
                    font->ft.m.type == fnt_MMType1)
                {
                    retval = pdf_t1open_fontfile(p, font, outfilename, NULL,
                                                 pdc_true);
                }
                else
                {
                    retval = fnt_check_tt_font(p->pdc, outfilename, NULL,
                                               &font->ft, pdc_true);
                }
            }
            else
            {
                /* Searching font outline file */
                if (logg1)
                    pdc_logg(p->pdc,
                             "\tSearching for font outline data file:\n");

                outfilename = testfilename;
                for (i = 0; i < 100; i++)
                {
                    extension = pdf_extension_names[i].word;
                    if (!extension)
                        break;

                    strcpy(testfilename, fontname_p);
                    strcat(testfilename, extension);

                    if (font->ft.m.type == fnt_Type1 ||
                        font->ft.m.type == fnt_MMType1)
                    {
                        if (pdf_extension_names[i].code == font_pfab)
                        {
                            retval = pdf_t1open_fontfile(p, font, outfilename,
                                                         NULL, pdc_false);
                        }
                    }
                    else if (pdf_extension_names[i].code == font_ttot)
                    {
                        retval = fnt_check_tt_font(p->pdc, outfilename,
                                                   NULL, &font->ft, pdc_false);
                    }

                    /* file found or error */
                    if (retval != pdc_undef)
                        break;
                }

                if (retval == pdc_undef)
		{
                    retval = pdc_false;
                    if (font->ft.m.type == fnt_Type1 ||
                        font->ft.m.type == fnt_MMType1)
                        pdc_set_errmsg(p->pdc, PDF_E_FONT_NOOUTLINE_PS,
                                       0, 0, 0, 0);
                    else
                        pdc_set_errmsg(p->pdc, PDF_E_FONT_NOOUTLINE_TT,
                                       0, 0, 0, 0);
                }
            }

            if (retval == pdc_false)
            {
                    if (logg1)
                        pdc_logg(p->pdc,
                            "\tOutline data file for font \"%s\" not found\n",
                            fontname_p);
            }
            else
            {
                if (!font->ft.img)
                    font->filename = font->ft.filename;

                    if (logg1)
                        pdc_logg(p->pdc,
                                "\tFont outline data file \"%s\" available\n",
                                font->filename ?
                                font->filename : font->ft.imgname);
            }
        }
    }
    else if (font->ft.img)
    {
        if (!font->ft.imgname)
            pdc_free(p->pdc, font->ft.img);
        else
        {
            pdc_unlock_pvf(p->pdc, font->ft.imgname);
            pdc_free(p->pdc, font->ft.imgname);
            font->ft.imgname = NULL;
        }
        font->ft.img = NULL;
        font->ft.filelen = 0;
    }

    if (retval && font->opt.monospace && font->opt.embedding)
    {
        pdc_set_errmsg(p->pdc, PDC_E_OPT_IGNORED, "monospace", 0, 0, 0);
        retval = pdc_false;
    }

    if (retval == pdc_false)
    {
        goto PDF_PREMATURE_EXIT;
    }

    PDF_NEWFONT_EXIT:

    pdf_cleanup_font_options(p, fo);

    encoding = pdc_get_user_encoding(p->pdc, font->ft.enc);
    if (pdc_strcmp(font->encapiname, encoding))
        pdc_logg_cond(p->pdc, 1, trc_encoding,
                          "\tDetermined font encoding: \"%s\"\n", encoding);

    /* set missing font metrics values */
    pdf_font_set_missvalues(p, font);

    /* font is semantic (Unicode compatible) */
    pdf_font_issemantic(p, font);

    /* set replacement character and code */
    pdf_set_replchar(p, font);

    /* font object ID */
    if (!font->opt.auxiliary)
        font->obj_id = pdc_alloc_id(p->out);

    /* Now everything is fine, insert font */
    if (slot == -1)
        slot = pdf_insert_font(p, font);


    pdc_pop_errmsg(p->pdc);

    return slot;


    PDF_PREMATURE_EXIT:

    pdf_cleanup_font_options(p, fo);
    pdf_cleanup_font(p, font);

    if (slot == -1)
    {
        if (font->verbose)
            pdc_error(p->pdc, -1, 0, 0, 0, 0);
    }

    pdc_pop_errmsg(p->pdc);

    return slot;
}


/* --------------------------- font writing ---------------------------- */

static char *
pdf_code2fontglyphname(pdf_font *font, pdc_encodingvector *ev, int code)
{
    char *glyphname;

    glyphname = ev->chars[code];
    pdc_get_alter_glyphname(ev->codes[code], font->missingglyphs,
                            &glyphname);

    return glyphname ? glyphname : (char *) pdc_get_notdef_glyphname();
}

void
pdf_transform_fontwidths(PDF *p, pdf_font *font, pdc_encodingvector *evto,
                         pdc_encodingvector *evfrom)
{
    int widths[256];
    pdc_ushort code2gid[256];
    int i, j;

    for (i = 0; i < 256; i++)
    {
        widths[i] = font->ft.m.defwidth;
        code2gid[i] = 0;
    }

    for (i = 0; i < 256; i++)
    {
        j = (int) pdc_transform_bytecode(p->pdc, evto, evfrom, (pdc_byte)i);
        widths[j] = font->ft.m.widths[i];
        if (font->ft.code2gid != NULL)
            code2gid[j] = font->ft.code2gid[i];
    }

    widths[0] = font->ft.m.defwidth;
    memcpy(font->ft.m.widths, widths, 256 * sizeof(int));
    if (font->ft.code2gid != NULL)
        memcpy(font->ft.code2gid, code2gid, 256 * sizeof(pdc_ushort));
}

void
pdf_prepare_fontwidths(PDF *p, pdf_font *font, int nusedgids)
{

    (void) p;
    (void) nusedgids;

    if (font->towinansi != pdc_invalidenc || font->widths != NULL ||
        (font->iscidfont && (font->ft.isstdfont || font->opt.monospace)))
    {
        return;
    }

    /* exchange widths pointer */
    if (!font->iscidfont && font->ft.enc != pdc_unicode)
    {
        font->widths = font->ft.m.widths;
        font->numwidths = font->ft.m.numwidths;
        font->ft.m.widths = NULL;
        font->ft.m.numwidths = 0;
        return;
    }

    /* already defined or no basic data */
    if (font->ft.m.widths == NULL && font->ft.m.ciw == NULL
       )
    {
        return;
    }

}



static void
pdf_write_fontdescriptor(
    PDF *p,
    pdf_font *font,
    int missingwidth,
    pdc_id fontdescriptor_id,
    pdc_id cidset_id,
    pdc_id fontfile_id,
    int nusedgids)
{
    (void) cidset_id;
    (void) nusedgids;

    /*
     * Font descriptor object
     */
    pdc_begin_obj(p->out, fontdescriptor_id);   /* font descriptor obj */
    pdc_begin_dict(p->out);                     /* font descriptor dict */

    pdc_puts(p->out, "/Type/FontDescriptor\n");
    pdc_printf(p->out, "/Flags %ld\n", font->ft.m.flags);


    pdc_printf(p->out, "/Ascent %d\n", font->ft.m.ascender);
    pdc_printf(p->out, "/CapHeight %d\n", font->ft.m.capHeight);
    pdc_printf(p->out, "/Descent %d\n", font->ft.m.descender);

    if (font->ft.m.type != fnt_Type3)
        pdc_printf(p->out, "/FontBBox[%d %d %d %d]\n",
                        (int) font->ft.m.llx, (int) font->ft.m.lly,
                        (int) font->ft.m.urx, (int) font->ft.m.ury);

    pdc_printf(p->out, "/FontName");
    pdf_put_pdfname(p, font->ft.m.name);
    pdc_puts(p->out, "\n");

    pdc_printf(p->out, "/ItalicAngle %d\n", (int) (font->ft.m.italicAngle));

    if (font->ft.m.type != fnt_Type3)
        pdc_printf(p->out, "/StemV %d\n", font->ft.m.StdVW);

    if (font->ft.m.StdHW > 0)
        pdc_printf(p->out, "/StemH %d\n", font->ft.m.StdHW);

    if (font->ft.m.xHeight > 0)
        pdc_printf(p->out, "/XHeight %d\n", font->ft.m.xHeight);

    if (missingwidth > 0)
        pdc_printf(p->out, "/MissingWidth %d\n", missingwidth);

    if (fontfile_id != PDC_BAD_ID)
    {
        switch(font->ft.m.type)
        {
            case fnt_Type1:
            case fnt_MMType1:
            pdc_objref(p->out, "/FontFile", fontfile_id);
            break;

#ifdef PDF_TRUETYPE_SUPPORTED
            case fnt_TrueType:
            case fnt_CIDFontType2:
            pdc_objref(p->out, "/FontFile2", fontfile_id);
            break;

            case fnt_Type1C:
            case fnt_CIDFontType0:
            pdc_objref(p->out, "/FontFile3", fontfile_id);
            break;
#endif /* PDF_TRUETYPE_SUPPORTED */

            default:
            break;
        }
    }


    pdc_end_dict(p->out);                       /* font descriptor dict */
    pdc_end_obj(p->out);                        /* font descriptor obj */
}

static void
pdf_put_font(PDF *p, pdf_font *font)
{
    const char        *fontname = font->ft.name;
    fnt_fonttype       fonttype = font->ft.m.type;
    pdc_id             fontdescriptor_id = PDC_BAD_ID;
    pdc_id             fontfile_id = PDC_BAD_ID;
    pdc_id             encoding_id = PDC_BAD_ID;
    pdc_id             cidset_id = PDC_BAD_ID;
    pdc_id             length_id = PDC_BAD_ID;
    pdc_id             descendant_id = PDC_BAD_ID;
    pdc_encoding       enc = font->ft.enc;
    const char        *encoding;
    pdc_encoding_info *encinfo = NULL;
    pdc_bool           comp_font = pdc_false;
    pdc_bool           acro_fontstyle = pdc_false;
    pdc_scalar         a = 1.0;
    PDF_data_source    src;
    int                nusedgids = 0;

    /* save font struct members */
    pdc_encodingvector *ev = NULL;
    pdc_encoding       font_encoding = font->ft.enc;
    pdc_encoding       font_towinansi = font->towinansi;
    int                font_numcodes = font->ft.numcodes;
    int                font_codesize = font->codesize;

    int                missingwidth = 0;
    int                i;

    encoding = pdc_get_user_encoding(p->pdc, enc);
    if (!pdc_strcmp(font->encapiname, encoding))
    {
        pdc_push_errmsg(p->pdc, PDF_E_FONT_PREFIX,
                        font->apiname, font->encapiname, 0, 0);
    }
    else
    {
        pdc_push_errmsg(p->pdc, PDF_E_FONT_PREFIX2,
                        font->apiname, font->encapiname, encoding, 0);
    }


    /* ID for embedded font */
    if (font->opt.embedding)
    {
        switch(fonttype)
        {
            case fnt_Type1:
            case fnt_MMType1:
            case fnt_TrueType:
            case fnt_CIDFontType2:
            case fnt_Type1C:
            case fnt_CIDFontType0:
            fontfile_id = pdc_alloc_id(p->out);
            break;

            default:
            break;
        }
    }

    /*
     * Font dictionary
     */
    pdc_begin_obj(p->out, font->obj_id);                /* font obj */
    pdc_begin_dict(p->out);                             /* font dict */
    pdc_puts(p->out, "/Type/Font\n");

    /* /Subtype */
    pdc_printf(p->out, "/Subtype/%s\n",
        pdc_get_keyword(fonttype, pdf_fonttype_pdfkeylist));
    comp_font = fonttype == fnt_CIDFontType0 || fonttype == fnt_CIDFontType2;

    /* Acrobat font style */
    acro_fontstyle = font->opt.fontstyle != fnt_Normal &&
                     !(font->metricflags & (font_bold | font_italic));

    /* /Name */
    if (fonttype == fnt_Type3 || font->used_in_formfield)
    {
        /*
         * The name is optional, but if we include it it will show up
         * in Acrobat's font info box. However, if the same font name
         * is used with different encodings Acrobat 4 will not be
         * able to distinguish both. For this reason we add the
         * encoding name to make the font name unique.
         */

        const char *name = fontname;

        if (font->used_in_formfield)
            name = pdf_get_pdf_fontname(font);

        pdc_puts(p->out, "/Name");
        pdf_put_pdfname(p, name);
        pdc_puts(p->out, "\n");
    }

    /* /BaseFont */
    switch (fonttype)
    {
        case fnt_Type1:
        case fnt_MMType1:
        case fnt_TrueType:
        case fnt_Type1C:
        case fnt_CIDFontType2:
        case fnt_CIDFontType0:
        {
            pdc_puts(p->out, "/BaseFont");
            pdf_put_pdfname(p, fontname);
            if (font->outcmapname)
                pdc_printf(p->out, "-%s", font->outcmapname);
            if (acro_fontstyle && !comp_font)
                pdc_printf(p->out, ",%s", pdc_get_keyword(font->opt.fontstyle,
                                                     pdf_fontstyle_pdfkeylist));
            pdc_puts(p->out, "\n");
        }
        break;

        /* /FontBBox, /FontMatrix, /CharProcs /Resources */
        case fnt_Type3:
        if (font->t3font->charprocs_id == PDC_BAD_ID)
            pdc_error(p->pdc, PDF_E_T3_OUTLINESMISSING, fontname, 0, 0, 0);

        pdc_printf(p->out, "/FontBBox[%f %f %f %f]\n",
            font->ft.bbox.llx, font->ft.bbox.lly,
            font->ft.bbox.urx, font->ft.bbox.ury);

        pdc_printf(p->out, "/FontMatrix[%f %f %f %f %f %f]\n",
            font->ft.matrix.a, font->ft.matrix.b,
            font->ft.matrix.c, font->ft.matrix.d,
            font->ft.matrix.e, font->ft.matrix.f);
        pdc_objref(p->out, "/CharProcs", font->t3font->charprocs_id);
        pdc_objref(p->out, "/Resources", font->t3font->res_id);

        /* We must apply a correctional factor since Type 3 fonts not
         * necessarily use 1000 units per em. We apply the correction
         * here, and store the 1000-based width values in the font in
         * order to speed up PDF_stringwidth().
         */
        a = 1000 * font->ft.matrix.a;
        break;

        default:
        break;
    }

    /* changing 8-bit font encoding to builtin */
    if (enc >= 0 && font->symenc != pdc_invalidenc)
    {
        ev = NULL;
        enc = pdc_builtin;
        font->ft.enc = enc;
    }

    /* changing 8-bit font encoding to winansi */
    if (font->towinansi != pdc_invalidenc)
    {
        pdc_encodingvector *evfrom;

        ev = pdc_get_encoding_vector(p->pdc, font->towinansi);
        evfrom = pdc_get_encoding_vector(p->pdc, enc);
        pdf_transform_fontwidths(p, font, ev, evfrom);

        enc = font->towinansi;
        font->ft.enc = enc;
        font->towinansi = pdc_invalidenc;
    }

    /* preparation for font widths array */
    pdf_prepare_fontwidths(p, font, nusedgids);

    /* /FontDescriptor, /FirstChar, /LastChar, /Widths */
    switch (fonttype)
    {
        case fnt_Type1:
        /* disabled, because of PDF 1.7 reference
        if (font->ft.isstdfont == pdc_true) break;
        */
        case fnt_MMType1:
        case fnt_TrueType:
        case fnt_Type1C:
        case fnt_Type3:
        {
            int firstchar = 0;
            int lastchar = 255;
            int defwidth = 0;

            if (fonttype != fnt_Type3
               )
            {
                fontdescriptor_id = pdc_alloc_id(p->out);
                pdc_objref(p->out, "/FontDescriptor", fontdescriptor_id);

                /* bug #1036 */
                if (font->ft.isstdfont == pdc_true && font->opt.dropcorewidths)
                    break;
            }

            /* determine missing width.
             * Only for embedded fonts because of a bug in Acrobat,
             * which arises if the font is not installed at host.
             */
            if (font->opt.embedding && !font->used_in_formfield)
            {
                if (fonttype != fnt_Type3)
                    defwidth = font->widths[0];

                {
                    for (i = 1; i < 255; i++)
                    {
                        if (font->widths[i] != defwidth)
                            break;
                    }
                    if (i > 1)
                        firstchar = i;
                    for (i = 255; i > 0; i--)
                    {
                        if (i == firstchar || font->widths[i] != defwidth)
                            break;
                    }
                    lastchar = i;
                }

                if (firstchar > 0 || lastchar < 255)
                    missingwidth = (int) (defwidth / a + 0.5);
            }

            pdc_printf(p->out, "/FirstChar %d\n", firstchar);
            pdc_printf(p->out, "/LastChar %d\n", lastchar);

            pdc_puts(p->out, "/Widths");
            pdc_begin_array(p->out);
            for (i = firstchar; i <= lastchar; i++)
            {
                pdc_printf(p->out, "%d",
                           (int) (font->widths[i] / a + .5));
                if (i < 255)
                    pdc_printf(p->out, "%s", ((i + 1) % 16) ? " " : "\n");
            }
            pdc_end_array(p->out);
        }
        break;

        default:
        break;
    }

    /* /Encoding */
    switch (fonttype)
    {
        case fnt_Type1:
        case fnt_MMType1:
        case fnt_TrueType:
        case fnt_Type1C:
        if (!font->used_in_formfield)
        {
            if (enc == pdc_winansi)
            {
                pdc_printf(p->out, "/Encoding/WinAnsiEncoding\n");
                break;
            }
            if (enc == pdc_macroman && font->hasnomac == pdc_false)
            {
                pdc_printf(p->out, "/Encoding/MacRomanEncoding\n");
                break;
            }
        }
        case fnt_Type3:
        if (enc >= 0)
        {
            encinfo = pdc_get_encoding_info(p->pdc, enc);
            if (encinfo->id == PDC_BAD_ID)
                encinfo->id = pdc_alloc_id(p->out);
            encoding_id = encinfo->id;
        }

        if (encoding_id != PDC_BAD_ID)
            pdc_objref(p->out, "/Encoding", encoding_id);

        if (encinfo != NULL)
        {
            if (!encinfo->stored)
                encinfo->stored = pdc_true;
            else
                encoding_id = PDC_BAD_ID;
        }

        break;

        case fnt_CIDFontType2:
        case fnt_CIDFontType0:
        if (font->outcmapname)
        {
                pdc_printf(p->out, "/Encoding/%s\n", font->outcmapname);
        }
        break;

        default:
        break;
    }

    /* /ToUnicode . Only reasonable if nusedgids != 1
     * (== 1: only notdef character in a font subset)
     */


    /* /DescendantFonts */
    if (comp_font == pdc_true)
    {
        descendant_id = pdc_alloc_id(p->out);
        pdc_puts(p->out, "/DescendantFonts");
        pdc_begin_array(p->out);
        pdc_objref(p->out, "", descendant_id);
        pdc_end_array(p->out);
    }

    pdc_end_dict(p->out);                               /* font dict */
    pdc_end_obj(p->out);                                /* font obj */

    /*
     * Encoding dictionary
     */
    if (encoding_id != PDC_BAD_ID)
    {
        char *glyphname;

        pdc_begin_obj(p->out, encoding_id);             /* encoding obj */
        pdc_begin_dict(p->out);                         /* encoding dict */

        pdc_puts(p->out, "/Type/Encoding\n");

        {
            pdc_encodingvector *evb = NULL;

            pdc_set_encoding_glyphnames(p->pdc, enc);
            ev = pdc_get_encoding_vector(p->pdc, enc);

            /* See Implementation Note 46. The restrictions described there
             * are also valid for Acrobat versions up to now.
             */
            if (fonttype != fnt_Type3 && !font->used_in_formfield)
            {
                if (!strncmp(ev->apiname, PDC_ENC_MODWINANSI,
                             strlen(PDC_ENC_MODWINANSI)) ||
                    !strncmp(ev->apiname, PDC_ENC_ISO8859,
                             strlen(PDC_ENC_ISO8859)) ||
                    !strncmp(ev->apiname, PDC_ENC_CP125,
                             strlen(PDC_ENC_CP125)))
                {
                    pdc_puts(p->out, "/BaseEncoding/WinAnsiEncoding\n");
                    evb = pdc_get_encoding_vector(p->pdc, pdc_winansi);
                }
                else if (!strncmp(ev->apiname, PDC_ENC_MODMACROMAN,
                             strlen(PDC_ENC_MODMACROMAN)))
                {
                    pdc_puts(p->out, "/BaseEncoding/MacRomanEncoding\n");
                    evb = pdc_get_encoding_vector(p->pdc, pdc_macroman);
                }
                else
                {
                    /* /BaseEncoding/StandardEncoding */
                    evb = pdc_get_encoding_vector(p->pdc, pdc_stdenc);
                }
            }

            if (evb != NULL)
            {
                int iv = -1;
                for (i = 0; i < font->ft.numcodes; i++)
                {
                    glyphname = pdf_code2fontglyphname(font, ev, i);

                    /* enforce first three names because of bug in Acrobat 6 */
                    if (i < 3 ||
                        (glyphname && !evb->chars[i]) ||
                        (!glyphname && evb->chars[i]) ||
                        (glyphname && evb->chars[i] &&
                         strcmp(glyphname, evb->chars[i])))
                    {
                        if (iv == -1)
                            pdc_puts(p->out, "/Differences[0");
                        if (i > iv + 1)
                            pdc_printf(p->out, "%d", i);
                        pdf_put_pdfname(p, glyphname);
                        pdc_puts(p->out, "\n");
                        iv = i;
                    }
                }
                if (iv > -1)
                    pdc_end_array(p->out);
            }
            else
            {
                pdc_puts(p->out, "/Differences[0");
                for (i = 0; i < font->ft.numcodes; i++)
                {
                    glyphname = pdf_code2fontglyphname(font, ev, i);
                    pdf_put_pdfname(p, glyphname);
                    pdc_puts(p->out, "\n");
                }
                pdc_end_array(p->out);
            }
        }

        pdc_end_dict(p->out);                           /* encoding dict */
        pdc_end_obj(p->out);                            /* encoding obj */

        if (p->flush & pdc_flush_content)
            pdc_flush_stream(p->out);
    }


    /*
     * CID fonts dictionary
     */
    if (descendant_id != PDC_BAD_ID)
    {
        pdc_begin_obj(p->out, descendant_id);           /* CID font obj */
        pdc_begin_dict(p->out);                         /* CID font dict */
        pdc_puts(p->out, "/Type/Font\n");

        /* /Subtype */
        if (fonttype == fnt_CIDFontType0)
            pdc_puts(p->out, "/Subtype/CIDFontType0\n");
        if (fonttype == fnt_CIDFontType2)
            pdc_puts(p->out, "/Subtype/CIDFontType2\n");

        /* /BaseFont */
        pdc_puts(p->out, "/BaseFont");
        pdf_put_pdfname(p, fontname);
        if (acro_fontstyle)
            pdc_printf(p->out, ",%s",
                pdc_get_keyword(font->opt.fontstyle, pdf_fontstyle_pdfkeylist));
        pdc_puts(p->out, "\n");

        /* /CIDSystemInfo */
        pdc_puts(p->out, "/CIDSystemInfo<</Registry");
        pdf_put_hypertext(p, "Adobe");
        pdc_puts(p->out, "/Ordering");
        pdf_put_hypertext(p, fnt_get_ordering_cid(font->ft.m.charcoll));
        pdc_printf(p->out, "/Supplement %d>>\n", MAX(font->supplement, 0));

        /* /FontDescriptor */
        fontdescriptor_id = pdc_alloc_id(p->out);
        pdc_objref(p->out, "/FontDescriptor", fontdescriptor_id);



        /* /DW /W */
#ifdef PDF_CJKFONTWIDTHS_SUPPORTED
        if (font->ft.isstdfont)
            pdf_put_cidglyph_widths(p, font);
#endif /* PDF_CJKFONTWIDTHS_SUPPORTED */


        pdc_end_dict(p->out);                           /* CID font dict */
        pdc_end_obj(p->out);                            /* CID font obj */

    }


    /*
     * FontDescriptor dictionary
     */
    if (fontdescriptor_id != PDC_BAD_ID)
        pdf_write_fontdescriptor(p, font, missingwidth, fontdescriptor_id,
                                 cidset_id, fontfile_id, nusedgids);



    /*
     * Font embedding
     */
    if (fontfile_id != PDC_BAD_ID)
    {
        pdc_id    length1_id = PDC_BAD_ID;
        pdc_id    length2_id = PDC_BAD_ID;
        pdc_id    length3_id = PDC_BAD_ID;
        pdc_bool  compress = pdc_false;

        /* Prepare embedding */
        switch(fonttype)
        {
            case fnt_Type1:
            case fnt_MMType1:
            {
                pdf_make_t1src(p, font, &src);
                length1_id = pdc_alloc_id(p->out);
                length2_id = pdc_alloc_id(p->out);
                length3_id = pdc_alloc_id(p->out);
            }
            break;

#ifdef PDF_TRUETYPE_SUPPORTED
            case fnt_TrueType:
            case fnt_CIDFontType2:
            {
                length1_id = pdc_alloc_id(p->out);
            }
            case fnt_Type1C:
            case fnt_CIDFontType0:
            case fnt_OpenType:
            {
                src.private_data = (void *) font->filename;
                if (font->filename)
                {
                    /* Read the font from file */
                    src.init = pdf_data_source_file_init;
                    src.fill = pdf_data_source_file_fill;
                    src.terminate = pdf_data_source_file_terminate;
                    switch(fonttype)
                    {
                        case fnt_TrueType:
                        case fnt_CIDFontType2:
                        case fnt_OpenType:
                        src.offset = (long) 0;
                        src.length = (long) 0;
                        break;

                        case fnt_Type1C:
                        case fnt_CIDFontType0:
                        src.offset = font->cff_offset;
                        src.length = (long) font->cff_length;
                        break;

                        default:
                        break;
                    }
                }
                else
                {
                    /* Read the font from memory */
                    src.init = NULL;
                    src.fill = pdf_data_source_buf_fill;
                    src.terminate = NULL;
                    switch(fonttype)
                    {
                        case fnt_TrueType:
                        case fnt_CIDFontType2:
                        case fnt_OpenType:
                        src.buffer_start = font->ft.img;
                        src.buffer_length = font->ft.filelen;
                        break;

                        case fnt_Type1C:
                        case fnt_CIDFontType0:
                        src.buffer_start = font->ft.img + font->cff_offset;
                        src.buffer_length = font->cff_length;
                        break;

                        default:
                        break;
                    }
                    src.bytes_available = 0;
                    src.next_byte = NULL;
                }
            }
            break;
#endif /* PDF_TRUETYPE_SUPPORTED */

            default:
            break;
        }

        /* Embedded font stream dictionary */
        pdc_begin_obj(p->out, fontfile_id);     /* Embedded font stream obj */
        pdc_begin_dict(p->out);                 /* Embedded font stream dict */

        /* /Length, /Filter */
        length_id = pdc_alloc_id(p->out);
        pdc_objref(p->out, "/Length", length_id);
        switch(fonttype)
        {
            case fnt_Type1:
            case fnt_MMType1:
            break;

#ifdef PDF_TRUETYPE_SUPPORTED
            case fnt_TrueType:
            case fnt_CIDFontType2:
            case fnt_Type1C:
            case fnt_CIDFontType0:
            case fnt_OpenType:
            if (font->ft.filelen != 0L)
            {
                compress = pdc_true;
                if (pdc_get_compresslevel(p->out))
                    pdc_puts(p->out, "/Filter/FlateDecode\n");
            }
            break;
#endif /* PDF_TRUETYPE_SUPPORTED */

            default:
            break;
        }

        /* /Length1, /Length2, Length3 */
        if (length1_id != PDC_BAD_ID)
            pdc_objref(p->out, "/Length1", length1_id);
        if (length2_id != PDC_BAD_ID)
            pdc_objref(p->out, "/Length2", length2_id);
        if (length3_id != PDC_BAD_ID)
            pdc_objref(p->out, "/Length3", length3_id);

#ifdef PDF_TRUETYPE_SUPPORTED
        /* /Subtype */
        if(fonttype == fnt_Type1C)
            pdc_puts(p->out, "/Subtype/Type1C\n");
        if (fonttype == fnt_CIDFontType0)
            pdc_puts(p->out, "/Subtype/CIDFontType0C\n");
        if (fonttype == fnt_OpenType)
            pdc_puts(p->out, "/Subtype/OpenType\n");
#endif /* PDF_TRUETYPE_SUPPORTED */


        pdc_end_dict(p->out);                   /* Embedded font stream dict */

        /* Stream */
        pdf_copy_stream(p, &src, compress);

        pdc_end_obj(p->out);                    /* Embedded font stream obj */

        pdc_put_pdfstreamlength(p->out, length_id);

        /* Length objects */
        switch(fonttype)
        {
            case fnt_Type1:
            case fnt_MMType1:
            pdf_put_length_objs(p, &src, length1_id, length2_id, length3_id);
            break;

#ifdef PDF_TRUETYPE_SUPPORTED
            case fnt_TrueType:
            case fnt_CIDFontType2:
            if (compress)
            {
                pdc_begin_obj(p->out, length1_id);      /* Length1 obj */
                pdc_printf(p->out, "%ld\n", (long) font->ft.filelen);
                pdc_end_obj(p->out);                    /* Length1 obj */
            }
            else
            {
                /* same as /Length */
                pdc_put_pdfstreamlength(p->out, length1_id);
            }
            break;
#endif /* PDF_TRUETYPE_SUPPORTED */

            default:
            break;
        }
    }

    if (p->flush & pdc_flush_content)
        pdc_flush_stream(p->out);

    /* restore font struct members */
    font->ft.enc = font_encoding;
    font->towinansi = font_towinansi;
    font->ft.numcodes = font_numcodes;
    font->codesize = font_codesize;

    pdc_pop_errmsg(p->pdc);
}

void
pdf_write_doc_fonts(PDF *p)
{
    int slot;
    pdc_bool logg1 = pdc_logg_is_enabled(p->pdc, 1, trc_font);

    /* output pending font objects */
    for (slot = 0; slot < p->fonts_number; slot++)
    {
        pdf_font *font = &p->fonts[slot];

        switch(p->fonts[slot].ft.m.type)
        {
            case fnt_Type1:
            case fnt_MMType1:
#ifdef PDF_TRUETYPE_SUPPORTED
            case fnt_TrueType:
            case fnt_CIDFontType2:
            case fnt_Type1C:
#endif /* PDF_TRUETYPE_SUPPORTED */
            case fnt_CIDFontType0:
            case fnt_Type3:
            if (font->obj_id != PDC_BAD_ID)
            {
                if (logg1)
                {
                    pdc_logg(p->pdc,
                             "\tProcessing font %d: \"%s\" "
                             "with encoding \"%s\" and PDF object id %ld",
                             slot, font->ft.name,
                             pdf_get_encoding_name(p, font->ft.enc, font),
                             font->obj_id);
                }

                if (font->ft.enc == pdc_invalidenc ||
                    font->used_in_current_doc == pdc_false)
                {
                    if (logg1)
                        pdc_logg(p->pdc, " - but not used\n", font->obj_id);

                    /*
                     * This font has been defined, but never used in the
                     * document. Ignore it. However, the font's object id
                     * has already been allocated, so we mark the object
                     * as free in order to avoid a complaint of the object
                     * machinery.
                     */
                    pdc_mark_free(p->out, font->obj_id);
                }
                else
                {
                    if (logg1)
                        pdc_logg(p->pdc, "\n");

                    pdf_put_font(p, font);
                }
            }
            break;

            default:
            break;
        }
    }
}

void
pdf_write_page_fonts(PDF *p)
{
    int i, total = 0;

    /* This doesn't really belong here, but all modules which write
     * font resources also need this, so we include it here.
     * Note that keeping track of ProcSets is considered obsolete
     * starting with PDF 1.4, so we always include the full set which
     * is written as a constant object at the beginning of the file.
     */

    pdc_objref(p->out, "/ProcSet",  p->procset_id);

    for (i = 0; i < p->fonts_number; i++)
        if (p->fonts[i].used_on_current_page == pdc_true)
            total++;

    if (total > 0)
    {
        pdc_puts(p->out, "/Font");
        pdc_begin_dict(p->out);         /* font resource dict */
    }

    if (total > 0)
    {
        for (i = 0; i < p->fonts_number; i++)
	{
            if (p->fonts[i].used_on_current_page == pdc_true) {
                p->fonts[i].used_on_current_page = pdc_false;   /* reset */
                pdc_printf(p->out, "/F%d", i);
                pdc_objref(p->out, "", p->fonts[i].obj_id);
            }
	}

	    pdc_end_dict(p->out);	/* font resource dict */
    }
}

void
pdf_get_page_fonts(PDF *p, pdf_reslist *rl)
{
    int i;

    for (i = 0; i < p->fonts_number; i++)
    {
        if (p->fonts[i].used_on_current_page)
        {
            p->fonts[i].used_on_current_page = pdc_false; /* reset */
            pdf_add_reslist(p, rl, i);
        }
    }
}

void
pdf_mark_page_font(PDF *p, int ft)
{
    p->fonts[ft].used_on_current_page = pdc_true;
}
