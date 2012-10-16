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
 * Routines for Type 3 (user-defined) fonts
 *
 */

#include "p_intern.h"
#include "p_font.h"
#include "p_image.h"

int
pdf_get_t3colorized(PDF *p)
{
    return p->fonts[p->t3slot].t3font->colorized;
}

static void
pdf_init_t3font(PDF *p, pdf_t3font *t3font, int glyph_capacity)
{
    static char fn[] = "pdf_init_t3font";
    int i;

    /* statement order is critical for cleanup!
    */
    t3font->curr_glyph = 0;
    t3font->next_glyph = 0;
    t3font->capacity = glyph_capacity;
    t3font->glyphs = (pdf_t3glyph *)
        pdc_malloc(p->pdc, t3font->capacity * sizeof (pdf_t3glyph), fn);

    for (i = 0; i < t3font->capacity; i++)
        t3font->glyphs[i].name = NULL;

    t3font->charprocs_id = PDC_BAD_ID;
    t3font->pass = 0;

}

void
pdf_cleanup_t3font(PDF *p, pdf_t3font *t3font)
{
    int i;

    for (i = 0; i < t3font->next_glyph; i++)
    {
        if (t3font->glyphs[i].name)
        {
            pdc_free(p->pdc, t3font->glyphs[i].name);
            t3font->glyphs[i].name = NULL;
        }
    }

    pdc_free(p->pdc, t3font->glyphs);
    t3font->glyphs = NULL;
}

static void
pdf_type3_protocol(PDF *p, pdf_font *font, pdc_encodingvector *ev)
{
    /* logging protocol */
    if (pdc_logg_is_enabled(p->pdc, 2, trc_font))
    {
        char *glyphname;
        pdc_ushort uv = 0;
        int gid, code, width = 0;

        for (gid = 0; gid < font->t3font->next_glyph; gid++)
        {
            glyphname = NULL;

            pdc_logg(p->pdc, "\t\tGlyph%4d: ", gid);

            if (ev != NULL)
            {
                code = font->ft.gid2code[gid];
                uv = ev->codes[code];
                if (glyphname == NULL)
                    glyphname = ev->chars[code];
                width = fnt_get_glyphwidth(code, &font->ft);

                pdc_logg(p->pdc, "code=%3d  ", code);
            }

            if (width == FNT_MISSING_WIDTH)
                width = 0;

            pdc_logg(p->pdc, "U+%04X  width=%4d  \"%s\"\n",
                     uv, width, glyphname);
        }
    }
}

/*
 * We found a Type 3 font in the cache, but its encoding doesn't match.
 * Make a copy of the font in a new slot, and attach the new encoding.
 */

pdc_bool
pdf_handle_t3font(PDF *p, const char *fontname, pdc_encoding enc,
                  pdf_font *font, int *slot)
{
    static const char fn[] = "pdf_handle_t3font";
    const char *encname;
    char *fname;
    size_t namlen;
    pdf_font *deffont = &p->fonts[*slot];
    pdc_encodingvector *ev = pdc_get_encoding_vector(p->pdc, enc);
    fnt_font_metric *ftm = &font->ft.m;
    size_t nalloc;
    int code, gid;
    pdc_bool newinst = pdc_false;

    /* font name incl. encoding name */
    encname = pdc_get_user_encoding(p->pdc, enc);
    namlen = strlen(fontname) + strlen(encname) + 2;
    fname = (char *) pdc_malloc(p->pdc, namlen, fn);
    pdc_sprintf(p->pdc, pdc_false, fname, "%s.%s", fontname, encname);

    /* we have to copy the available font.
     * otherwise the original font will be changed
     */
    newinst = deffont->ft.enc != pdc_invalidenc;

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\n\tType3 font \"%s\" with %d glyphs found\n",
        fontname, deffont->t3font->next_glyph);

    if (newinst)
    {
        pdc_logg_cond(p->pdc, 1, trc_font,
            "\tInstance with specified encoding will be created\n");

    }

    /* copy data from available font (see pdf__begin_font()) */
    font->ft.m.type = fnt_Type3;
    font->ft.matrix = deffont->ft.matrix;
    font->ft.bbox = deffont->ft.bbox;
    font->t3font = deffont->t3font;
    font->ft.numglyphs = deffont->t3font->next_glyph;
    nalloc = (size_t) font->ft.numglyphs;

    ftm->name = fname;
    font->ft.name = pdc_strdup(p->pdc, fname);
    font->ft.enc = enc;
    font->ft.issymbfont = pdc_false;
    font->opt.embedding = pdc_true;

    if (enc >= pdc_winansi)
    {
        font->codesize = 1;
        font->ft.numcodes = 256;
        font->lastcode = -1;

        ftm->widths = (int *) pdc_calloc(p->pdc,
                                  (size_t) font->ft.numcodes * sizeof(int), fn);
        ftm->numwidths = font->ft.numcodes;
    }

    font->ft.code2gid = (pdc_ushort *) pdc_calloc(p->pdc,
                           (size_t) font->ft.numcodes * sizeof(pdc_ushort), fn);

    font->ft.gid2code = (pdc_ushort *) pdc_calloc(p->pdc,
                                          nalloc * sizeof (pdc_ushort), fn);

    /* fill up font arrays */
    for (gid = 0; gid < font->ft.numglyphs; gid++)
    {
        const char *str = NULL, *glyphname = font->t3font->glyphs[gid].name;

        if (enc >= pdc_winansi)
        {
            /* search for code */
            for (code = 0; code < font->ft.numcodes; code++)
            {
                if (ev->chars[code] != NULL)
                    str = ev->chars[code];
                else if (ev->codes[code])
                    str = pdc_unicode2glyphname(p->pdc, ev->codes[code]);

                if (str != NULL && !pdc_strcmp(glyphname, str))
                    break;
            }

            /* code found */
            if (code < font->ft.numcodes)
            {
                font->ft.code2gid[code] = (pdc_ushort) gid;
                font->ft.gid2code[gid] = (pdc_ushort) code;

                if (!gid)
                    font->gid0code = code;

                if (font->opt.monospace)
                    ftm->widths[code] = font->opt.monospace;
                else
                    ftm->widths[code] =
                          (int) (font->t3font->glyphs[gid].width + 0.5);
            }
        }
    }


    pdf_type3_protocol(p, font, ev);

    /* font flags */
    if (!pdf_make_fontflag(p, font))
        return pdc_false;

    if (newinst)
    {
        *slot = -1;
    }
    else
    {
        if (deffont->apiname != NULL)
            pdc_free(p->pdc, deffont->apiname);
        *deffont = *font;
        deffont->hasoriginal = pdc_true;
    }

    return pdc_true;
}

pdc_bool
pdf_isvalid_font(PDF *p, int slot)
{
    if (slot > -1 && slot < p->fonts_number)
    {
        pdf_font *font = &p->fonts[slot];
        if (!font->opt.auxiliary &&
            (font->t3font == NULL || font->t3font->pass != 2))
            return pdc_true;
    }

    return pdc_false;
}

#define PDF_FAMILYNAME_FLAG PDC_OPT_UNSUPP
#define PDF_STRETCH_FLAG PDC_OPT_UNSUPP
#define PDF_WEIGHT_FLAG PDC_OPT_UNSUPP

/*
 * internal font stretch values
 */
#define PDF_FS_ULTRACONDENSED  1
#define PDF_FS_EXTRACONDENSED  2
#define PDF_FS_CONDENSED       3
#define PDF_FS_SEMICONDENSED   4
#define PDF_FS_NORMAL          5
#define PDF_FS_SEMIEXPANDED    6
#define PDF_FS_EXPANDED        7
#define PDF_FS_EXTRAEXPANDED   8
#define PDF_FS_ULTRAEXPANDED   9

static const pdc_keyconn pdf_fontstretch_keylist[] =
{
    {"UltraCondensed",  PDF_FS_ULTRACONDENSED},
    {"ExtraCondensed",  PDF_FS_EXTRACONDENSED},
    {"Condensed",       PDF_FS_CONDENSED},
    {"SemiCondensed",   PDF_FS_SEMICONDENSED},
    {"Normal",          PDF_FS_NORMAL},
    {"SemiExpanded",    PDF_FS_SEMIEXPANDED},
    {"Expanded",        PDF_FS_EXPANDED},
    {"ExtraExpanded",   PDF_FS_EXTRAEXPANDED},
    {"UltraExpanded",   PDF_FS_ULTRAEXPANDED},
    {NULL, 0}
};

/* conf. with fnt_fontweight_keylist[] in ft_font.c */
static const pdc_keyconn pdf_fontweight_keylist[] =
{
    {"thin",        FNT_FW_THIN},
    {"extralight",  FNT_FW_EXTRALIGHT},
    {"light",       FNT_FW_LIGHT},
    {"normal",      FNT_FW_NORMAL},
    {"medium",      FNT_FW_MEDIUM},
    {"semibold",    FNT_FW_SEMIBOLD},
    {"bold",        FNT_FW_BOLD},
    {"extrabold",   FNT_FW_EXTRABOLD},
    {"black",       FNT_FW_BLACK},
    {NULL, 0}
};

/* definitions of begin font options */
static const pdc_defopt pdf_begin_font_options[] =
{
    {"colorized", pdc_booleanlist, 0, 1, 1, 0.0, 0.0, NULL},

    {"widthsonly", pdc_booleanlist, 0, 1, 1, 0.0, 0.0, NULL},

    {"familyname", pdc_stringlist, PDF_FAMILYNAME_FLAG, 1, 1,
      1.0, PDF_MAX_FONTNAME, NULL},

    {"stretch", pdc_keywordlist, PDF_STRETCH_FLAG, 1, 1,
      0.0, 0.0, pdf_fontstretch_keylist},

    {"weight", pdc_keywordlist, PDF_WEIGHT_FLAG, 1, 1,
      0.0, 0.0, pdf_fontweight_keylist},

    PDC_OPT_TERMINATE
};

void
pdf__begin_font(
    PDF *p,
    const char *fontname, int len,
    pdc_scalar a, pdc_scalar b, pdc_scalar c, pdc_scalar d,
    pdc_scalar e, pdc_scalar f,
    const char *optlist)
{
    static const char fn[] = "pdf__begin_font";
    pdc_resopt *results;
    pdf_font tmpfont, *font;
    pdf_font_options fo;
    pdc_scalar det;
    pdc_clientdata cdata;
    int colorized = pdc_false;
    int metricsonly = pdc_false;
    int slot;

    if (fontname == NULL)
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "fontname", 0, 0, 0);

    /* Converting fontname */
    fontname = pdf_convert_name(p, fontname, len,
                                PDC_CONV_WITHBOM | PDC_CONV_TMPALLOC);
    if (fontname == NULL || *fontname == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "fontname", 0, 0, 0);

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\tBegin of Type3 font \"%s\"\n", fontname);

    /* error message prefix */
    pdc_push_errmsg(p->pdc, PDF_E_T3_FONT_PREFIX, fontname, 0, 0, 0);

    /* look for an already existing font */
    for (slot = 0; slot < p->fonts_number; slot++)
    {
        if (!pdc_strcmp(p->fonts[slot].apiname, fontname))
        {
            if (p->fonts[slot].t3font->pass == 1)
            {
                pdc_logg_cond(p->pdc, 1, trc_font,
                    "\tType3 font [%d] with metric definition found\n", slot);

                PDF_CHECK_STATE(p, pdf_state_document);

                p->fonts[slot].t3font->pass = 2;
                p->t3slot = slot;

                pdc_pop_errmsg(p->pdc);

                pdf_pg_suspend(p);
                PDF_SET_STATE(p, pdf_state_font);
                return;
            }

            pdc_error(p->pdc, PDF_E_T3_FONTEXISTS, 0, 0, 0, 0);
        }
    }

    pdc_check_number(p->pdc, "a", a);
    pdc_check_number(p->pdc, "b", b);
    pdc_check_number(p->pdc, "c", c);
    pdc_check_number(p->pdc, "d", d);
    pdc_check_number(p->pdc, "e", e);
    pdc_check_number(p->pdc, "f", f);

    det = a*d - b*c;

    if (det == 0)
        pdc_error(p->pdc, PDC_E_ILLARG_MATRIX,
            pdc_errprintf(p->pdc, "%f %f %f %f %f %f", a, b, c, d, e, f),
            0, 0, 0);

    /* parsing optlist */
    pdf_set_clientdata(p, &cdata);
    results = pdc_parse_optionlist(p->pdc, optlist, pdf_begin_font_options,
                                   &cdata, pdc_true);

    pdc_get_optvalues("colorized", results, &colorized, NULL);
    pdc_get_optvalues("widthsonly", results, &metricsonly, NULL);


    pdc_cleanup_optionlist(p->pdc, results);

    /* initialize font struct */
    font = &tmpfont;
    pdf_init_font_options(p, &fo);
    pdf_init_font(p, font, &fo);

    /*
     * We store the new font in a font slot marked with "invalidenc" encoding.
     * When the font is used for the first time we modify the encoding.
     * Later uses will make a copy if the encoding is different.
     */

    /* API font name */
    font->apiname = pdc_strdup(p->pdc, fontname);

    font->ft.m.type = fnt_Type3;
    font->hasoriginal = pdc_true;

    font->ft.matrix.a = a;
    font->ft.matrix.b = b;
    font->ft.matrix.c = c;
    font->ft.matrix.d = d;
    font->ft.matrix.e = e;
    font->ft.matrix.f = f;

    font->t3font = (pdf_t3font*) pdc_malloc(p->pdc, sizeof(pdf_t3font), fn);
    pdf_init_t3font(p, font->t3font, T3GLYPHS_CHUNKSIZE);

    font->t3font->colorized = colorized;


    /* the resource id is needed until the font dict is written */
    font->t3font->res_id = pdc_alloc_id(p->out);

    /* Now everything is fine, insert Type3 font with invalid encoding */
    slot = pdf_insert_font(p, font);

    /*
     * We must store a pointer to the current font because its glyph
     * definitions may use other fonts and we would be unable to find
     * "our" current font again. This pointer lives throughout the
     * font definition, and will be reset in PDF_end_font() below.
     */
    p->t3slot = slot;

    if (metricsonly)
    {
        font->t3font->pass = 1;
        pdc_logg_cond(p->pdc, 2, trc_font,
                          "\t\tonly for metric definition\n");
    }
    else
    {
        pdf_pg_suspend(p);
    }

    pdc_pop_errmsg(p->pdc);

    PDF_SET_STATE(p, pdf_state_font);

    if (!p->pdc->smokerun)
        pdc_logg_cond(p->pdc, 1, trc_api, "[Begin font %d]\n", p->t3slot);
}

void
pdf__end_font(PDF *p)
{
    int ig;
    pdf_font *font;
    pdf_t3font *t3font;

    PDF_SET_STATE(p, pdf_state_document);

    font = &p->fonts[p->t3slot];
    t3font = font->t3font;

    /* error message prefix */
    pdc_push_errmsg(p->pdc, PDF_E_T3_FONT_PREFIX, font->apiname, 0, 0, 0);

    if (t3font->pass == 0)
    {
        pdf_t3glyph glyph0 = t3font->glyphs[0];

        /* search for .notdef glyph */
        if (pdc_strcmp(glyph0.name, (char *) pdc_get_notdef_glyphname()))
        {
            for (ig = 0; ig < t3font->next_glyph; ig++)
            {
                if (!pdc_strcmp(t3font->glyphs[ig].name,
                                (char *) pdc_get_notdef_glyphname()))
                    break;
            }

            if (ig < t3font->next_glyph)
            {
                pdc_logg_cond(p->pdc, 2, trc_font,
                    "\tGlyph id %d: \"%s\" will be exchanged "
                    "with glyph id 0: \"%s\"\n",
                    ig, t3font->glyphs[ig].name, glyph0.name);

                t3font->glyphs[0] = t3font->glyphs[ig];
                t3font->glyphs[ig] = glyph0;
            }
            else
            {
                pdc_warning(p->pdc, PDF_E_T3_MISSNOTDEF, 0, 0, 0, 0);
            }
        }
    }

    if (t3font->pass != 1)
    {
        t3font->charprocs_id = pdc_alloc_id(p->out);
        pdc_begin_obj(p->out, t3font->charprocs_id); /* CharProcs dict */
        pdc_begin_dict(p->out);

        for (ig = 0; ig < t3font->next_glyph; ig++)
        {
            pdf_t3glyph *glyph = &t3font->glyphs[ig];

            if (glyph->charproc_id != PDC_BAD_ID)
            {
                pdf_put_pdfname(p, glyph->name);
                pdc_objref(p->out, "", glyph->charproc_id);
            }
        }

        pdc_end_dict(p->out);
        pdc_end_obj(p->out);                        /* CharProcs dict */

        pdc_begin_obj(p->out, t3font->res_id);
        pdc_begin_dict(p->out);                     /* Resource dict */

        pdf_write_page_fonts(p);                    /* Font resources */

        pdf_write_page_colorspaces(p);              /* Color space resources */

        pdf_write_page_pattern(p);                  /* Pattern resources */

        pdf_write_xobjects(p);                      /* XObject resources */

        pdc_end_dict(p->out);                       /* Resource dict */
        pdc_end_obj(p->out);                        /* Resource object */

        pdf_pg_resume(p, -1);

        if (p->flush & pdc_flush_content)
            pdc_flush_stream(p->out);

        /* see pdf__begin_glyph */
        pdf_init_tstate(p);
        pdf_init_gstate(p);
        pdf_init_cstate(p);
    }

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\tEnd of Type3 font \"%s\"\n", font->apiname);

    pdc_pop_errmsg(p->pdc);

    if (!p->pdc->smokerun)
        pdc_logg_cond(p->pdc, 1, trc_api, "[End font %d]\n", p->t3slot);

    p->t3slot = -1;
}

void
pdf__begin_glyph(
    PDF *p,
    const char *glyphname,
    pdc_scalar wx,
    pdc_scalar llx, pdc_scalar lly, pdc_scalar urx, pdc_scalar ury)
{
    static const char fn[] = "pdf__begin_glyph";
    pdf_font *font;
    pdf_t3font *t3font;
    pdf_t3glyph *glyph = NULL;
    pdc_scalar tbc;
    int ig;

    if (glyphname == NULL || *glyphname == '\0')
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "glyphname", 0, 0, 0);

    font = &p->fonts[p->t3slot];
    t3font = font->t3font;

    /* error message prefix */
    pdc_push_errmsg(p->pdc, PDF_E_T3_FONT_PREFIX, font->apiname, 0, 0, 0);

    for (ig = 0; ig < t3font->next_glyph; ig++)
    {
        glyph = &t3font->glyphs[ig];
        if (!pdc_strcmp(glyph->name, glyphname))
        {
            if (t3font->pass == glyph->pass)
                pdc_error(p->pdc, PDF_E_T3_GLYPH, glyphname, 0, 0, 0);
            else
                break;
        }
    }

    /* new glyph */
    if (ig == t3font->next_glyph)
    {
        if (t3font->pass == 2)
            pdc_error(p->pdc, PDF_E_T3_UNKOWNGLYPH, glyphname, 0, 0, 0);

        pdc_check_number(p->pdc, "wx", wx);
        pdc_check_number(p->pdc, "llx", llx);
        pdc_check_number(p->pdc, "lly", lly);
        pdc_check_number(p->pdc, "urx", urx);
        pdc_check_number(p->pdc, "ury", ury);

        if (t3font->colorized == pdc_true &&
            (llx != 0 || lly != 0 ||
             urx != 0 || ury != 0))
            pdc_error(p->pdc, PDF_E_T3_BADBBOX, 0, 0, 0, 0);


        if (urx < llx)
        {
            tbc = llx;
            llx = urx;
            urx = tbc;
        }

        if (ury < lly)
        {
            tbc = lly;
            lly = ury;
            ury = tbc;
        }

        if (ig == t3font->capacity)
        {
            t3font->capacity *= 2;
            t3font->glyphs = (pdf_t3glyph *) pdc_realloc(p->pdc, t3font->glyphs,
                t3font->capacity * sizeof (pdf_t3glyph), fn);
        }

        glyph = &t3font->glyphs[ig];
        glyph->charproc_id = PDC_BAD_ID;
        glyph->name = pdc_strdup(p->pdc, glyphname);
        glyph->wx = wx;
        glyph->llx = llx;
        glyph->lly = lly;
        glyph->urx = urx;
        glyph->ury = ury;

        /* see comment in p_font.c for explanation */
        glyph->width = 1000 * wx * font->ft.matrix.a;

        /* if the strdup above fails, cleanup won't touch this slot. */
        t3font->next_glyph++;
    }
    glyph->pass = t3font->pass;
    t3font->curr_glyph = ig;

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\tBegin of Type3 font glyph \"%s\"\n", glyphname);

    if (t3font->pass != 1)
    {
            if (t3font->pass == 2)
                pdc_logg_cond(p->pdc, 2, trc_font,
                              "\t\tglyph [%d] was used in text\n", ig);

            glyph->charproc_id = pdc_begin_obj(p->out, PDC_NEW_ID);
            pdc_begin_dict(p->out);

            p->length_id = pdc_alloc_id(p->out);
            pdc_objref(p->out, "/Length", p->length_id);

            if (pdc_get_compresslevel(p->out))
                pdc_puts(p->out, "/Filter/FlateDecode\n");

            pdc_end_dict(p->out);

            pdc_begin_pdfstream(p->out);

            if (t3font->colorized == pdc_true)
                pdc_printf(p->out, "%f 0 d0\n", glyph->wx);
            else
            {
                pdc_printf(p->out, "%f 0 %f %f %f %f d1\n",
                    glyph->wx, glyph->llx, glyph->lly, glyph->urx, glyph->ury);

                /* adjust the font's bounding box */
                if (glyph->llx < font->ft.bbox.llx)
                    font->ft.bbox.llx = glyph->llx;
                if (glyph->lly < font->ft.bbox.lly)
                    font->ft.bbox.lly = glyph->lly;
                if (glyph->urx > font->ft.bbox.urx)
                    font->ft.bbox.urx = glyph->urx;
                if (glyph->ury > font->ft.bbox.ury)
                    font->ft.bbox.ury = glyph->ury;
            }

            /* we must initialize the text, graphics and color state
             * otherwise the user get unpredictable appearance of a
             * glyph because of optimizing outputting graphics properties.
             * Following statements were inserted due to bug #719
             */
            pdf_init_tstate(p);
            pdf_init_gstate(p);
            pdf_init_cstate(p);

            PDF_SET_STATE(p, pdf_state_glyph);
    }
    else
    {
        PDF_SET_STATE(p, pdf_state_glyphmetrics);
    }

    pdc_pop_errmsg(p->pdc);

    if (!p->pdc->smokerun)
        pdc_logg_cond(p->pdc, 1, trc_api, "[Begin glyph %d]\n", ig);
}

void
pdf__end_glyph(PDF *p)
{
    pdf_t3font *t3font = p->fonts[p->t3slot].t3font;
    pdf_t3glyph *glyph = &t3font->glyphs[t3font->curr_glyph];
    int ig = t3font->curr_glyph;

    if (t3font->pass != 1 && glyph->charproc_id != PDC_BAD_ID)
    {
        /* check whether pdf__save() and pdf__restore() calls are balanced */
        if (p->curr_ppt->sl > 0)
            pdc_error(p->pdc, PDF_E_GSTATE_UNMATCHEDSAVE, 0, 0, 0, 0);

        pdf_end_text(p);
        pdc_end_pdfstream(p->out);              /* glyph description stream */
        pdc_end_obj(p->out);                    /* glyph description */

        pdc_put_pdfstreamlength(p->out, p->length_id);
    }

    PDF_SET_STATE(p, pdf_state_font);

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\tEnd of Type3 font glyph \"%s\"\n",
        t3font->glyphs[ig].name);

    if (!p->pdc->smokerun)
        pdc_logg_cond(p->pdc, 1, trc_api, "[End glyph %d]\n", ig);
}


void
pdf_init_type3(PDF *p)
{
    p->t3slot = -1;
}

