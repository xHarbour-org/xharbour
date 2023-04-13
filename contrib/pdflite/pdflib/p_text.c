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
 * PDFlib text routines
 *
 */

#define P_TEXT_C

#include "p_intern.h"
#include "p_color.h"
#include "p_defopt.h"
#include "p_font.h"

/* --------------------- Text state and options functions ------------------- */

struct pdf_tstate_s
{
    pdc_bool    glyphinit;      /* glyph description initialized */
    pdc_bool    hsinit;         /* horizontal scaling initialized */
    int         mask;           /* bit mask for text options */
    int         font;           /* slot number of the current font */
    int         trm;            /* text rendering mode */
    pdc_scalar  fs;             /* font size */
    pdc_scalar  ld;             /* leading */
    pdc_scalar  cs;             /* character spacing */
    pdc_scalar  ws;             /* word spacing */
    pdc_scalar  hs;             /* horizontal scaling */
    pdc_scalar  ia;             /* italic angle */
    pdc_bool    fb;              /* fake bold */
    pdc_scalar  rise;           /* text rise */
    pdc_scalar  ulw;            /* underline width */
    pdc_scalar  ulp;            /* underline position */

    pdc_bool    newpos;         /* new text position */
    pdc_scalar  currtx;         /* x coordinate of current text position */
    pdc_scalar  currty;         /* y coordinate of current text position */
    pdc_scalar  prevtx;         /* x coordinate of previous text position */
    pdc_scalar  prevty;         /* y coordinate of previous text position */
    pdc_scalar  linetx;         /* x coordinate of text line start position */
    pdc_scalar  refptx;         /* x and y coordinate of reference position */
    pdc_scalar  refpty;         /* for moving to next text line start position*/
};

/* Initialize the text state at the beginning of each page */
void
pdf_init_tstate(PDF *p)
{
    static const char fn[] = "pdf_init_tstate";

    /* text state */
    pdf_ppt *ppt = p->curr_ppt;
    pdf_tstate *ts;

    if (!p->curr_ppt->tstate)
    {
	p->curr_ppt->tstate = (pdf_tstate *) pdc_malloc(p->pdc,
                                   PDF_MAX_SAVE_LEVEL * sizeof(pdf_tstate), fn);
	ppt->currto = (pdf_text_options *) pdc_malloc(p->pdc,
                          sizeof(pdf_text_options), fn);
    }

    ts = &ppt->tstate[ppt->sl];

    ts->glyphinit = pdc_undef;
    ts->hsinit = (p->ydirection == -1) ? pdc_false : pdc_true;

    ts->mask = 0;
    ts->font = -1;
    ts->trm = 0;
    ts->fs = PDC_FLOAT_MIN;
    ts->ld = 0;
    ts->cs = 0;
    ts->ws = 0;
    ts->hs = 1;
    ts->ia = 0;
    ts->fb = pdc_false;
    ts->rise = 0;
    ts->ulw = PDF_UNDERLINEWIDTH_AUTO;
    ts->ulp = PDF_UNDERLINEPOSITION_AUTO;

    ts->newpos = pdc_false;
    ts->currtx = 0;
    ts->currty = 0;
    ts->prevtx = 0;
    ts->prevty = 0;
    ts->linetx = 0;
    ts->refptx = 0;
    ts->refpty = 0;

    /* current text options */
    pdf_init_text_options(p, ppt->currto);
}

void
pdf_cleanup_page_tstate(PDF *p, pdf_ppt *ppt)
{
    if (ppt->tstate != NULL)
    {
        pdc_free(p->pdc, ppt->tstate);
        pdc_free(p->pdc, ppt->currto);
	ppt->tstate = NULL;
	ppt->currto = NULL;
    }
}

void
pdf_save_tstate(PDF *p)
{
    pdf_ppt *ppt = p->curr_ppt;
    int sl = ppt->sl;

    memcpy(&ppt->tstate[sl + 1], &ppt->tstate[sl], sizeof(pdf_tstate));
}

void
pdf_restore_currto(PDF *p)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_text_options *currto = ppt->currto;
    pdf_tstate *ts = &ppt->tstate[ppt->sl];

    currto->mask = ts->mask;
    currto->font = ts->font;
    currto->textrendering = ts->trm;
    currto->fontsize = ts->fs;
    currto->leading = ts->ld;
    currto->charspacing = ts->cs;
    currto->wordspacing = ts->ws;
    currto->horizscaling = ts->hs;
    currto->italicangle = ts->ia;
    currto->fakebold = ts->fb;
    currto->textrise = ts->rise;
    currto->underlinewidth = ts->ulw;
    currto->underlineposition = ts->ulp;
}

void
pdf_set_tstate(PDF *p, pdc_scalar value, pdf_text_optflags tflag)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_tstate *ts = &ppt->tstate[ppt->sl];
    pdf_text_options *currto = ppt->currto;
    int ivalue = (int) value;
    pdc_scalar prevvalue;

    /* text state parameter values can never be percentages */

    switch (tflag)
    {
        case to_font:
        pdf_check_handle(p, ivalue, pdc_fonthandle);
        prevvalue = ts->font;
        ts->font = currto->font = ivalue;
        if (prevvalue != -1 &&
            (p->fonts[(int) prevvalue].metricflags & font_italic) !=
            (p->fonts[currto->font].metricflags & font_italic))
            currto->mask |= (1 << to_italicangle);
        break;

        case to_textrendering:
        if (ivalue < 0 || ivalue > PDF_LAST_TRMODE)
            pdc_error(p->pdc, PDC_E_ILLARG_INT,
                "textrendering", pdc_errprintf(p->pdc, "%d", ivalue),
                0, 0);
        prevvalue = ts->trm;
        ts->trm = currto->textrendering = ivalue;
        break;

        case to_fontsize:
        pdc_check_number_zero(p->pdc, "fontsize", value);
        prevvalue = ts->ld;
        ts->ld = currto->leading = value;
        if (!PDC_FLOAT_ISNULL(value - prevvalue))
            currto->mask |= (1 << to_leading);
        prevvalue = ts->fs;
        ts->fs = currto->fontsize = value;
        break;

        case to_leading:
        prevvalue = ts->ld;
        ts->ld = currto->leading = value;
        break;

        case to_charspacing:
        prevvalue = ts->cs;
        ts->cs = currto->charspacing = value;
        break;

        case to_wordspacing:
        prevvalue = ts->ws;
        ts->ws = currto->wordspacing = value;
        break;

        case to_underlinewidth:
        prevvalue = ts->ulw;
        ts->ulw = currto->underlinewidth = value;
        break;

        case to_underlineposition:
        prevvalue = ts->ulp;
        ts->ulp = currto->underlineposition = value;
        break;

        case to_horizscaling:
        pdc_check_number_zero(p->pdc, "horizscaling", value);
        prevvalue = ts->hs;
        ts->hs = currto->horizscaling = value;
        break;

        case to_italicangle:
        pdc_check_number_limits(p->pdc, "italicangle", value,
                                -90 + PDC_FLOAT_PREC, 90 + PDC_FLOAT_MAX);
        prevvalue = ts->ia;
        ts->ia = currto->italicangle = value;
        break;

        case to_fakebold:
        prevvalue = ts->fb;
        ts->fb = currto->fakebold = (pdc_bool) ivalue;
        return;

        case to_textrise:
        prevvalue = ts->rise;
        ts->rise = currto->textrise = value;
        break;


        case to_overline:
        currto->overline = (pdc_bool) ivalue;
        return;

        case to_strikeout:
        currto->strikeout = (pdc_bool) ivalue;
        return;

        case to_underline:
        currto->underline = (pdc_bool) ivalue;
        return;

        case to_textformat:
        currto->textformat = (pdc_text_format) ivalue;
        return;

        case to_charref:
        currto->charref = (pdc_bool) ivalue;
        return;

        case to_escapesequence:
        currto->escapesequence = (pdc_bool) ivalue;
        return;

        case to_glyphcheck:
        currto->glyphcheck = (pdc_glyphcheck) ivalue;
        return;

        case to_glyphwarning:
        currto->glyphwarning = (pdc_bool) ivalue;
        return;

        default:
        return;
    }

    if (!PDC_FLOAT_ISNULL(value - prevvalue))
        currto->mask |= (1 << tflag);
    ts->mask = currto->mask;
}

void
pdf__setfont(PDF *p, int font, pdc_scalar fontsize)
{
    pdf_set_tstate(p, (pdc_scalar) font, to_font);
    pdf_set_tstate(p, fontsize, to_fontsize);
}

void
pdf__set_text_pos(PDF *p, pdc_scalar x, pdc_scalar y)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_tstate *ts = &ppt->tstate[ppt->sl];

    pdc_check_number(p->pdc, "x", x);
    pdc_check_number(p->pdc, "y", y);

    ts->newpos = pdc_true;
    ts->currtx = x;
    ts->currty = y;
    ts->prevtx = ts->refptx;
    ts->prevty = ts->refpty;
    ts->linetx = x;
}

double
pdf_get_tstate(PDF *p, pdf_text_optflags tflag)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_text_options *currto = ppt->currto;

    switch (tflag)
    {
        case to_font:
        return (double) currto->font;

        case to_textrendering:
        return (double) currto->textrendering;

        case to_fontsize:
        return (double) currto->fontsize;

        case to_leading:
        return (double) currto->leading;

        case to_charspacing:
        return (double) currto->charspacing;

        case to_wordspacing:
        return (double) currto->wordspacing;

        case to_horizscaling:
        return (double) currto->horizscaling;

        case to_italicangle:
        return (double) currto->italicangle;

        case to_fakebold:
        return (double) currto->fakebold;

        case to_textrise:
        return (double) currto->textrise;

        case to_underlinewidth:
        return (double) currto->underlinewidth;

        case to_underlineposition:
        return (double) currto->underlineposition;


        case to_overline:
        return (double) currto->overline;

        case to_strikeout:
        return (double) currto->strikeout;

        case to_underline:
        return (double) currto->underline;

        case to_textx:
        return (double) ppt->tstate[ppt->sl].currtx;

        case to_texty:
        return (double) ppt->tstate[ppt->sl].currty;

        default:
        break;
    }

    return 0;
}

int
pdf_get_font(PDF *p)
{
    if (p->curr_ppt)
        return (int) pdf_get_tstate(p, to_font);
    return -1;
}

void
pdf_init_text_options(PDF *p, pdf_text_options *to)
{
    to->mask = 0;
    to->pcmask = 0;
    to->font = -1;
    to->fontsize = PDC_FLOAT_MIN;
    to->fontsize_pc = 0;
    to->fontsize_st = (int) text_fontsize;
    to->fontset = 0;
    to->leading = 0;
    to->leading_pc = 0;
    to->textrendering = 0;
    to->charspacing = 0;
    to->charspacing_pc = 0;
    to->horizscaling = 1;
    to->italicangle = 0;
    to->fakebold = pdc_false;
    to->textrise = 0;
    to->textrise_pc = 0;
    to->wordspacing = 0;
    to->wordspacing_pc = 0;
    to->underlinewidth = PDF_UNDERLINEWIDTH_AUTO;
    to->underlineposition = PDF_UNDERLINEPOSITION_AUTO;
    to->overline = pdc_false;
    to->strikeout = pdc_false;
    to->underline = pdc_false;
    to->text = NULL;
    to->textlen = 0;
    to->textformat = p->textformat;
    to->charref = p->pdc->charref;
    to->escapesequence = p->pdc->escapesequ;
    to->glyphcheck = p->glyphcheck;
    to->glyphwarning = p->debug[(int) 'g'];
    to->glyphwarning = pdf_get_errorpolicy(p, NULL, to->glyphwarning);
    pdf_init_coloropt(p, &to->fillcolor);
    pdf_init_coloropt(p, &to->strokecolor);
    to->strokewidth = PDF_UNDERLINEWIDTH_AUTO;
    to->dasharray[0] = 0;
    to->dasharray[1] = 0;
    to->xadvancelist = NULL;
    to->nglyphs = 0;
    to->link = NULL;
}

static pdf_text_optflags pdf_toptflags[] =
{
    to_font, to_fontsize, to_textrendering, to_charspacing,
    to_horizscaling, to_italicangle, to_fakebold, to_wordspacing,
    to_textrise, to_underlinewidth, to_underlineposition
};

void
pdf_set_text_options(PDF *p, pdf_text_options *to)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_text_options *currto = p->curr_ppt->currto;
    pdf_tstate *ts = &ppt->tstate[ppt->sl];
    pdf_text_optflags tflag;
    int i, n;

    /* we synchronize both text state and text options */

    n = sizeof(pdf_toptflags) / sizeof(pdf_text_optflags);
    for (i = 0; i < n; i++)
    {
        tflag = pdf_toptflags[i];
        if (to->mask & (1 << tflag))
        {
            switch (tflag)
            {
                case to_font:
                if (!(currto->mask & (1 << tflag)) &&
                    to->font == currto->font)
                    break;
                if (currto->font != -1 &&
                    (p->fonts[to->font].metricflags & font_italic) !=
                    (p->fonts[currto->font].metricflags & font_italic))
                {
                    to->mask |= (1 << to_italicangle);
                    currto->mask = to->mask; /* otherwise order dependent */
                }
                ts->font = currto->font = to->font;
                continue;

                case to_fontsize:
                if (!(currto->mask & (1 << tflag)) &&
                    PDC_FLOAT_ISNULL(to->fontsize - currto->fontsize))
                    break;
                ts->fs = currto->fontsize = to->fontsize;
                continue;

                case to_textrendering:
                if (!(currto->mask & (1 << tflag)) &&
                    to->textrendering == currto->textrendering)
                    break;
                ts->trm = currto->textrendering = to->textrendering;
                continue;

                /* to->leading is never used */

                case to_charspacing:
                if (!(currto->mask & (1 << tflag)) &&
                    PDC_FLOAT_ISNULL(to->charspacing - currto->charspacing))
                    break;
                ts->cs = currto->charspacing = to->charspacing;
                continue;

                case to_horizscaling:
                if (!(currto->mask & (1 << tflag)) &&
                    PDC_FLOAT_ISNULL(to->horizscaling - currto->horizscaling))
                    break;
                ts->hs = currto->horizscaling = to->horizscaling;
                continue;

                case to_italicangle:
                if (!(currto->mask & (1 << tflag)) &&
                    PDC_FLOAT_ISNULL(to->italicangle - currto->italicangle))
                    break;
                ts->ia = currto->italicangle = to->italicangle;
                continue;

                case to_fakebold:
                ts->fb = currto->fakebold = to->fakebold;
                continue;

                case to_wordspacing:
                if (!(currto->mask & (1 << tflag)) &&
                    PDC_FLOAT_ISNULL(to->wordspacing - currto->wordspacing))
                    break;
                ts->ws = currto->wordspacing = to->wordspacing;
                continue;

                case to_textrise:
                if (!(currto->mask & (1 << tflag)) &&
                    PDC_FLOAT_ISNULL(to->textrise - currto->textrise))
                    break;
                ts->rise = currto->textrise = to->textrise;
                continue;

                case to_underlinewidth:
                if (!(currto->mask & (1 << tflag)) &&
                    PDC_FLOAT_ISNULL(to->underlinewidth -
                                     currto->underlinewidth))
                    break;
                ts->ulw = currto->underlinewidth = to->underlinewidth;
                continue;

                case to_underlineposition:
                if (!(currto->mask & (1 << tflag)) &&
                    PDC_FLOAT_ISNULL(to->underlineposition -
                                     currto->underlineposition))
                    break;
                ts->ulp = currto->underlineposition = to->underlineposition;
                continue;

                default:
                continue;
            }

            to->mask &= ~(1 << tflag);
        }
    }

    ts->mask = currto->mask = to->mask;
}

int
pdf_get_fontsize_option(PDF *p, int font, pdc_resopt *resopts,
                        pdc_scalar *fontsize)
{
    pdc_scalar fs[2], fm ;
    int ns;

    /* *fontsize is initialized from outside */

    fs[0] = 0;   /* see "auto" */
    fs[1] = 0;
    ns = pdc_get_optvalues("fontsize", resopts, (pdc_scalar *) fs, NULL);
    if (ns == 1)
    {
        *fontsize = fs[0];
    }
    else if (ns == 2)
    {
        int kind = (int) fs[0];

        pdf_check_handle(p, font, pdc_fonthandle);

        switch(kind)
        {
            case text_xheight:
            fm = (pdc_scalar) p->fonts[font].ft.m.xHeight;
            break;

            case text_capheight:
            fm = (pdc_scalar) p->fonts[font].ft.m.capHeight;
            break;

            case text_ascender:
            fm = (pdc_scalar) p->fonts[font].ft.m.ascender;
            break;

            default:
            fm = 1000.0;
            break;
        }

        *fontsize = fs[1] * 1000.0 / fm;
    }

    return ns;
}

pdc_bool
pdf_calculate_text_options(PDF *p, pdf_text_options *to, pdc_bool force,
                           pdc_scalar fontscale, pdc_scalar minfontsize,
                           pdc_scalar fontsizeref)
{
    pdc_bool kminfs = pdc_false;

    if (to->mask & (1U << to_fontsize) ||
        (force && to->fontsize != PDC_FLOAT_MIN))
    {
        pdc_scalar fontsize;

        if (fontsizeref == 0)
            fontsizeref = to->fontsize;

        if (to->pcmask & (1 << to_fontsize))
            fontsize = to->fontsize_pc * fontsizeref;
        else
            fontsize = fontscale * to->fontsize;

        if (to->fontsize_st != (int) text_fontsize)
        {
            pdf_font *currfont = &p->fonts[to->font];
            pdc_scalar fm;

            switch(to->fontsize_st)
            {
                case text_xheight:
                fm = (pdc_scalar) currfont->ft.m.xHeight;
                break;

                case text_capheight:
                fm = (pdc_scalar) currfont->ft.m.capHeight;
                break;

                case text_ascender:
                fm = (pdc_scalar) currfont->ft.m.ascender;
                break;

                default:
                fm = 1000.0;
                break;
            }

            fontsize *= 1000.0 / fm;
        }

        if (fontscale < 1.0 && PDC_ABS(fontsize) < minfontsize)
        {
            fontsize = PDC_SIGN(fontsize) * minfontsize;
            kminfs = pdc_true;
        }
        to->fontsize = fontsize;

        /* we reset relative fontsize specifications */
        if (to->mask & (1L << to_fontsize))
        {
            to->pcmask &= ~(1 << to_fontsize);
            to->fontsize_st = (int) text_fontsize;
        }
    }

    if ((to->mask & (1 << to_charspacing) || force) &&
        (to->pcmask & (1 << to_charspacing)))
    {
        to->charspacing = to->charspacing_pc * to->fontsize;
    }

    if ((to->mask & (1 << to_wordspacing) || force) &&
        (to->pcmask & (1 << to_wordspacing)))
    {
        to->wordspacing = to->wordspacing_pc * to->fontsize;
    }

    if ((to->mask & (1 << to_textrise) || force) &&
        (to->pcmask & (1 << to_textrise)))
    {
        to->textrise = to->textrise_pc * to->fontsize;
    }

    /* maybe used in a future version */
    if ((to->mask & (1 << to_leading) || force) &&
        (to->pcmask & (1 << to_leading)))
    {
        to->leading = to->leading_pc * to->fontsize;
    }

    return kminfs;
}

void
pdf_get_text_options(PDF *p, pdf_text_options *to, pdc_resopt *resopts)
{
    char **strlist;
    int i, inum;
    pdc_scalar fs[2];

    if (pdc_get_optvalues("glyphwarning", resopts, &to->glyphwarning, NULL))
        to->mask |= (1L << to_glyphwarning);
    to->glyphwarning = pdf_get_errorpolicy(p, resopts, to->glyphwarning);

    if (pdc_get_optvalues("font", resopts, &to->font, NULL))
    {
        pdf_check_handle(p, to->font, pdc_fonthandle);
        to->mask |= (1L << to_font);
        to->fontset |= (1L << to_font);
    }

    fs[0] = 0;   /* see "auto" */
    fs[1] = 0;
    inum = pdc_get_optvalues("fontsize", resopts, (pdc_scalar *) fs, NULL);
    if (inum)
    {
        i = inum - 1;
        to->fontsize = fs[i];
        if (inum == 2)
            to->fontsize_st = (int) fs[0];
        else
            to->fontsize_st = (int) text_fontsize;
        to->mask |= (1L << to_fontsize);
        to->mask |= (1L << to_fontsize_st);

        if (pdc_is_lastopt_percent(resopts, i))
        {
            to->pcmask |= (1 << to_fontsize);
            to->fontsize_pc = to->fontsize;
        }
        else
            to->pcmask &= ~(1 << to_fontsize);

        to->fontset |= (1L << to_fontsize);
    }

    if (pdc_get_optvalues("charref", resopts, &to->charref, NULL))
        to->mask |= (1L << to_charref);

    if (pdc_get_optvalues("escapesequence", resopts, &to->escapesequence, NULL))
        to->mask |= (1L << to_escapesequence);

    if (pdc_get_optvalues("glyphcheck", resopts, &inum, NULL))
    {
        to->glyphcheck = (pdc_glyphcheck) inum;
        to->mask |= (1L << to_glyphcheck);
    }

    if (pdc_get_optvalues("charspacing", resopts, &to->charspacing, NULL))
    {
        if (pdc_is_lastopt_percent(resopts, 0))
        {
            to->pcmask |= (1 << to_charspacing);
            to->charspacing_pc = to->charspacing;
        }
        else
            to->pcmask &= ~(1 << to_charspacing);
        to->mask |= (1L << to_charspacing);
    }

    if (pdc_get_optvalues("horizscaling", resopts, &to->horizscaling, NULL))
    {
        if (!pdc_is_lastopt_percent(resopts, 0))
            to->horizscaling /= 100.0;
        to->mask |= (1L << to_horizscaling);
    }

    if (pdc_get_optvalues("italicangle", resopts, &to->italicangle, NULL))
        to->mask |= (1L << to_italicangle);

    if (pdc_get_optvalues("fakebold", resopts, &to->fakebold, NULL))
        to->mask |= (1L << to_fakebold);


    if (pdc_get_optvalues("overline", resopts, &to->overline, NULL))
        to->mask |= (1L << to_overline);

    if (pdc_get_optvalues("strikeout", resopts, &to->strikeout, NULL))
        to->mask |= (1L << to_strikeout);

    if (pdc_get_optvalues("textformat", resopts, &inum, NULL))
    {
        to->textformat = (pdc_text_format) inum;
        to->mask |= (1L << to_textformat);
        pdf_check_textformat(p, to->textformat);
    }

    if (pdc_get_optvalues("textrendering", resopts, &to->textrendering, NULL))
        to->mask |= (1L << to_textrendering);

    if (pdc_get_optvalues("textrise", resopts, &to->textrise, NULL))
    {
        if (pdc_is_lastopt_percent(resopts, 0))
        {
            to->pcmask |= (1 << to_textrise);
            to->textrise_pc = to->textrise;
        }
        else
            to->pcmask &= ~(1 << to_textrise);
        to->mask |= (1L << to_textrise);
    }

    if (pdc_get_optvalues("underline", resopts, &to->underline, NULL))
        to->mask |= (1L << to_underline);

    if (pdc_get_optvalues("wordspacing", resopts, &to->wordspacing, NULL))
    {
        if (pdc_is_lastopt_percent(resopts, 0))
        {
            to->pcmask |= (1 << to_wordspacing);
            to->wordspacing_pc = to->wordspacing;
        }
        else
            to->pcmask &= ~(1 << to_wordspacing);
        to->mask |= (1L << to_wordspacing);
    }

    if (pdc_get_optvalues("underlinewidth", resopts, &to->underlinewidth, NULL))
    {
        if (pdc_is_lastopt_percent(resopts, 0))
        {
            to->pcmask |= (1 << to_underlinewidth);
        }
        else
            to->pcmask &= ~(1 << to_underlinewidth);
        to->mask |= (1L << to_underlinewidth);
    }

    if (pdc_get_optvalues("underlineposition", resopts,
                          &to->underlineposition, NULL))
    {
        if (pdc_is_lastopt_percent(resopts, 0))
        {
            to->pcmask |= (1 << to_underlineposition);
        }
        else
            to->pcmask &= ~(1 << to_underlineposition);
        to->mask |= (1L << to_underlineposition);
    }

    inum = pdc_get_optvalues("fillcolor", resopts, NULL, &strlist);
    if (inum)
    {
        pdf_parse_coloropt(p, "fillcolor", strlist, inum, (int) color_max,
                           &to->fillcolor);
        to->mask |= (1L << to_fillcolor);
    }

    inum = pdc_get_optvalues("strokecolor", resopts, NULL, &strlist);
    if (inum)
    {
        pdf_parse_coloropt(p, "strokecolor", strlist, inum, (int) color_max,
                           &to->strokecolor);
        to->mask |= (1L << to_strokecolor);
    }

    if (pdc_get_optvalues("strokewidth", resopts, &to->strokewidth, NULL))
    {
        if (pdc_is_lastopt_percent(resopts, 0))
        {
            to->pcmask |= (1 << to_strokewidth);
        }
        else
            to->pcmask &= ~(1 << to_strokewidth);
        to->mask |= (1L << to_strokewidth);
    }

    inum = pdc_get_optvalues("dasharray", resopts, to->dasharray, NULL);
    if (inum)
    {
        if (inum == 1)
            to->dasharray[1] = to->dasharray[0];
        to->mask |= (1L << to_dasharray);
    }

    inum = pdc_get_optvalues("xadvancelist", resopts, NULL, &strlist);
    if (inum)
    {
        to->xadvancelist = (pdc_scalar *) strlist;
        to->nglyphs = inum;
    }

    /*
     * deprecated
     */
    if (pdc_get_optvalues("weblink", resopts, NULL, &strlist))
    {
        to->link = strlist[0];
        to->linktype = "URI";
    }
    else if (pdc_get_optvalues("locallink", resopts, NULL, &strlist))
    {
        to->link = strlist[0];
        to->linktype = "GoTo";
    }
    else if (pdc_get_optvalues("pdflink", resopts, NULL, &strlist))
    {
        to->link = strlist[0];
        to->linktype = "GoToR";
    }
}

/* ------------------------ Text object functions -------------------------- */

static void
pdf_begin_text(PDF *p)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_tstate *ts = &ppt->tstate[ppt->sl];
    pdf_text_options *currto = ppt->currto;
    pdf_font *currfont = NULL;

    if (currto->font > -1)
        currfont = &p->fonts[currto->font];

    /* end text object if italicangle changed */
    if (currto->mask & (1L << to_italicangle))
        pdf_end_text(p);

    /* begin text object */
    if (!p->in_text)
    {
        p->in_text = pdc_true;
        pdc_puts(p->out, "BT\n");
    }

    if (PDF_FORCE_OUTPUT() && ts->glyphinit == pdc_undef)
        ts->glyphinit = pdc_false;

    if (currfont &&
        ((currto->mask & (1L << to_font)) ||
         (currto->mask & (1L << to_fontsize)) || !ts->glyphinit))
    {
        pdc_printf(p->out, "/F%d %f Tf\n",
            currto->font, p->ydirection * currto->fontsize);

        currfont->used_in_current_doc = pdc_true;
        currfont->used_on_current_page = pdc_true;
    }

    if (currto->mask & (1L << to_textrendering) || !ts->glyphinit)
        pdc_printf(p->out, "%d Tr\n", currto->textrendering);

    if (currto->mask & (1L << to_leading) || !ts->glyphinit)
        pdc_printf(p->out, "%f TL\n", p->ydirection * currto->leading);

    if (currto->mask & (1L << to_charspacing) || !ts->glyphinit)
        pdc_printf(p->out, "%f Tc\n", p->ydirection * currto->charspacing);

    if (!ts->hsinit || currto->mask & (1L << to_horizscaling) || !ts->glyphinit)
        pdc_printf(p->out, "%f Tz\n",
                   100 * p->ydirection * currto->horizscaling);

    if (currto->mask & (1L << to_textrise) || !ts->glyphinit)
       pdc_printf(p->out, "%f Ts\n", p->ydirection * currto->textrise);

    /* initialize */
    if (!ts->glyphinit)
        ts->glyphinit = pdc_true;
    ts->hsinit = pdc_true;
    ts->mask = currto->mask = 0;
}

void
pdf_end_text(PDF *p)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_tstate *ts = &ppt->tstate[ppt->sl];

    if (p->in_text)
    {
        p->in_text = pdc_false;
        pdc_puts(p->out, "ET\n");

        ts->newpos = pdc_false;
        ts->prevtx = 0;
        ts->prevty = 0;
        ts->refptx = 0;
        ts->refpty = 0;
    }
}

void
pdf_reset_tstate(PDF *p)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_tstate *ts = &ppt->tstate[ppt->sl];

    pdf_set_tstate(p, 0, to_textrendering);
    pdf_set_tstate(p, 0, to_leading);
    pdf_set_tstate(p, 0, to_charspacing);
    pdf_set_tstate(p, 0, to_wordspacing);
    pdf_set_tstate(p, 1, to_horizscaling);
    pdf_set_tstate(p, 0, to_italicangle);
    pdf_set_tstate(p, 0, to_fakebold);
    pdf_set_tstate(p, 0, to_textrise);
    pdf_set_tstate(p, PDF_UNDERLINEWIDTH_AUTO, to_underlinewidth);
    pdf_set_tstate(p, PDF_UNDERLINEPOSITION_AUTO, to_underlineposition);

    ts->hsinit = (p->ydirection == -1) ? pdc_false : pdc_true;
    if (ts->mask || !ts->hsinit)
    {
        pdc_scalar ydirection = p->ydirection;
        p->ydirection = 1;
        pdf_begin_text(p);
        pdf_end_text(p);
        p->ydirection = ydirection;
    }
}


/* ------------------- Text string checking function ---------------------- */

struct pdf_fitres_s
{
    pdc_bool verbose;
    pdc_vector start;           /* text start position */
    pdc_vector end;             /* text end position */
    pdc_vector writingdir;      /* unit vector of text writing direction */
    pdc_vector perpendiculardir;/* unit vector perpendicular to writing dir. */
    pdc_vector scale;           /* x/y scaling */
    pdc_scalar angle;           /* rotation angle of writingdir in degree */
    pdc_scalar width;           /* textline width */
    pdc_scalar height;          /* textline height */
    pdc_scalar mwidth;          /* textline width with margins */
    pdc_scalar mheight;         /* textline height with margins */
    pdc_scalar ascender;        /* textline ascender */
    pdc_scalar capheight;       /* textline capheight */
    pdc_scalar xheight;         /* textline xheight */
    pdc_scalar descender;       /* textline descender */
    int unmappedchars;          /* number of characters not contained
                                 * in the encoding and not in the font */
    int replacedchars;          /* number of characters replaced by
                                 * typografically similar characters */
    int unknownchars;           /* number of characters replaced by
                                 * given replcement character */
};

typedef struct pdf_ligat_s pdf_ligat;

struct pdf_ligat_s
{
    pdf_ligat  *next;
    int         icp;    /* text position */
    int         nb;     /* number of parts */
    pdc_byte    culist[2 * PDC_MAX_UVLIST];
                        /* ligature parts */
};

static pdf_ligat *
pdf_register_ligat(PDF *p, pdf_ligat *ligatlist, int icp, int nv,
                   pdc_ushort *culist, int charlen)
{
    static const char fn[] = "pdf_hook_ligat";
    int i;

    pdf_ligat *ligat =
        (pdf_ligat *) pdc_malloc_tmp(p->pdc, sizeof(pdf_ligat), fn, NULL, NULL);

    if (ligatlist == NULL)
    {
        ligatlist = ligat;
    }
    else
    {
        pdf_ligat *next = ligatlist;

        while (next->next != NULL)
            next = next->next;
        next->next = ligat;
    }

    ligat->next = NULL;
    ligat->icp = charlen * icp;
    nv--;
    ligat->nb = charlen * nv;

    if (charlen == 1)
    {
        for (i = 0; i < nv; i++)
            ligat->culist[i] = (pdc_byte) culist[i+1];
    }
    else
    {
        memcpy(ligat->culist, &culist[1], (size_t) ligat->nb);
    }
    return ligatlist;
}

static void
pdf_cleanup_ligat(PDF *p, pdf_ligat *list)
{
    pdf_ligat *next;

    while (list != NULL)
    {
        next = list->next;
        pdc_free_tmp(p->pdc, list);
        list = next;
    }
}


#define PDF_MAX_GLYPHCHECKS 8

int
pdf_get_approximate_uvlist(PDF *p, pdf_font *currfont, pdc_encodingvector *ev,
                    int usv, pdc_bool replace, pdf_fitres *fitres,
                    pdc_ushort *uvlist, pdc_ushort *cglist)
{
    int cg = 0, nv = 1;

    (void) p;
    (void) ev;
    (void) usv;


    if (cg <= 0)
    {
        if (replace)
        {
            cglist[0] = (pdc_ushort) currfont->replacementcode;
            uvlist[0] = (pdc_ushort) currfont->replacementchar;
        }
        else
        {
            cglist[0] = 0;
            uvlist[0] = 0;
        }
        nv = 1;

        if (fitres != NULL)
            fitres->unknownchars++;
    }
    else if (fitres != NULL)
    {
        fitres->replacedchars++;
    }

    return nv;
}

static void
pdf_logg_glyph_replacement(PDF *p, int ic, int code,
                           pdc_encoding enc, int charlen,
                           pdc_ushort *uvlist, pdc_ushort *cglist, int nv)
{
    const char *glyphname;
    int i;

    pdc_logg(p->pdc, "\t\tat text position %d: ", ic);

    if (charlen == 1)
        pdc_logg(p->pdc, "code x%02X ", code);
    else
        pdc_logg(p->pdc, "U+%04X ", code);

    pdc_logg(p->pdc, "was replaced by: ");
    if (nv > 1)
        pdc_logg(p->pdc, "\n");

    for (i = 0; i < nv; i++)
    {
        if (nv > 1)
            pdc_logg(p->pdc, "\t\t\t");

        if (charlen == 1)
            pdc_logg(p->pdc, "code x%02X ", cglist[i]);
        else
            pdc_logg(p->pdc, "U+%04X ", uvlist[i]);

        if (enc >= 0)
        {
            if (charlen == 1)
                pdc_logg(p->pdc, "U+%04X ", uvlist[i]);
            else
                pdc_logg(p->pdc, "code x%02X ", cglist[i]);
        }

        glyphname = pdc_unicode2glyphname(p->pdc, uvlist[i]);
        if (glyphname && strlen(glyphname))
            pdc_logg(p->pdc, "\"%s\"", glyphname);

        pdc_logg(p->pdc, "\n");
    }
}

void
pdf_get_input_textformat(pdf_font *currfont,
        pdc_text_format *intextformat, int *convflags)
{
    /* input text format */
    if (currfont->unibyte)
    {
        /* encoding=unicode, but 8-bit encoding ev is available */
        *convflags |= PDC_CONV_FORCEUTF16;
    }
    else if (currfont->codesize <= 1)
    {
        /* we must force bytes[2] source input format
         * for 8-bit encodings because of "pass through mode".
         */
        if (*intextformat == pdc_auto)
            *intextformat = pdc_bytes;
        else if (*intextformat == pdc_auto2)
            *intextformat = pdc_bytes2;
    }
}

/* Converts and checks input text string.
 *
 * The function returns a pointer to an allocated temporary memory
 * (pdc_malloc_tmp, pdc_free_tmp) containing the converted string
 * if flag PDF_USE_TMPALLOC is set.
 *
 * If return value is pdc_false an error was occurred, otherwise pdc_true.
 *
 */

pdc_bool
pdf_check_textstring(PDF *p, const char *text, int len, int flags,
                     pdf_text_options *to, pdf_fitres *fitres,
                     pdc_byte **outtext_p, int *outlen,
                     int *outcharlen, pdc_bool verbose)
{
    static const char fn[] = "pdf_check_textstring";

    pdc_bool logg1 = pdc_logg_is_enabled(p->pdc, 1, trc_text);
    pdc_bool logg2 = pdc_false;
    pdf_font *currfont = &p->fonts[to->font];
    pdc_encoding enc = currfont->ft.enc;
    pdc_encodingvector *ev = pdc_get_encoding_vector(p->pdc, enc);
    pdc_encodingvector *inev = NULL;
    pdc_encodingvector *outev = NULL;

    pdc_text_format intextformat = to->textformat;
    pdc_text_format outtextformat = pdc_utf16;

    int maxlen = PDF_MAXDICTSIZE;
    int charlen = 1, newcharlen = 1;
    int convflags = PDC_CONV_NOBOM;

    pdf_ligat *ligat, *ligatlist = NULL;
    pdc_byte *intext = (pdc_byte *) text, *outtext = NULL;
    int newlen = -1, replchar;

    if (logg1)
    {
        logg2 = pdc_logg_is_enabled(p->pdc, 2, trc_text);

        if (logg2)
            pdc_logg_hexdump(p->pdc, "input text", "\t\t", (char *) text, len);
        else
            pdc_logg(p->pdc,
                "\tText will be checked and converted: \"%T\"\n", text, len);

        if (logg2)
        {
            pdc_logg(p->pdc,
                "\t\tfont: \"%s\"\n"
                "\t\tencoding: \"%s\"\n"
                "\t\ttextformat: %s\n"
                "\t\tcharref: %s\n"
                "\t\tescapesequence: %s\n"
                "\t\tglyphwarning: %s\n"
                "\t\tglyphcheck: %s\n",
                currfont->ft.name,
                pdf_get_encoding_name(p, enc, currfont),
                pdc_get_keyword(intextformat, pdf_textformat_keylist),
                PDC_BOOLSTR(to->charref),
                PDC_BOOLSTR(to->escapesequence),
                PDC_BOOLSTR(to->glyphwarning),
                pdc_get_keyword(to->glyphcheck, pdf_glyphcheck_keylist));

            convflags |= PDC_CONV_LOGGING;
        }
    }

    /* text is passed through for CID fonts with non Unicode CMap */
    if (currfont->passthrough)
    {
        if (logg2)
            pdc_logg(p->pdc, "\t\ttext is passed through as is\n");

        if (intextformat > pdc_bytes2)
            pdc_warning(p->pdc, PDF_E_TEXT_BADTEXTFORMAT,
                        pdc_get_textformat(intextformat), 0, 0, 0);

        outtext = (pdc_byte *) ((flags & PDF_USE_TMPALLOC) ?
             pdc_malloc_tmp(p->pdc, (size_t) len + 2, fn, NULL, NULL) :
             pdc_malloc(p->pdc, (size_t) len + 2, fn));
        memcpy(outtext, text, (size_t) len);
        outtext[len] = 0;
        outtext[len + 1] = 0;
        *outlen = len;

        outtextformat = pdc_bytes;
    }
    else
    {

        if (flags & PDF_FORCE_NEWALLOC)
            convflags |= PDC_CONV_NEWALLOC;

        if (flags & PDF_USE_TMPALLOC)
            convflags |= PDC_CONV_TMPALLOC;

        if (to->glyphcheck == (int) text_replace)
        {
            replchar = currfont->replacementchar;
        }
        else
        {
            replchar = (int) to->glyphcheck;
            if (to->glyphcheck == text_error)
            {
                convflags |= PDC_CONV_ENCERROR;
                if (flags & PDF_KEEP_CONTROL)
                    convflags |= PDC_CONV_KEEPLBCHAR;
            }
        }

        if (flags & PDF_KEEP_UNICODE || to->glyphcheck != text_nocheck)
            inev = ev;


        /* "Pass through mode" for 8-bit text.
         * The encoding vector must be specified, because
         * the text could emerge as a Unicode text due to a BOM.
         */
        if ((enc >= 0 && inev == NULL) ||
            (enc == pdc_builtin && !(flags & PDF_KEEP_UNICODE)))
        {
            inev = ev;
            outev = ev;
            outtextformat = pdc_bytes;
        }

        /* input text format */
        pdf_get_input_textformat(currfont, &intextformat, &convflags);

        /* convert to 8-bit or UTF-16 text string */
        if (pdc_convert_textstring(p->pdc, intextformat, currfont->codepage,
                        inev,
                        NULL, 0,
                        replchar, intext, len,
                        &outtextformat, outev, &outtext, outlen,
                        convflags, pdc_false))
        {
            if (newlen > -1)
                pdc_free_tmp(p->pdc, intext);
            goto PDF_CHECK_TEXT_ERROR;
        }
    }

    if (newlen > -1)
        pdc_free_tmp(p->pdc, intext);

    /* check text string */
    if (outtext != NULL && *outlen)
    {
        pdc_ushort *usouttext = (pdc_ushort *) outtext;
        pdc_ushort uvlist[PDC_MAX_UVLIST];
        pdc_ushort cglist[PDC_MAX_UVLIST];
        pdc_bool kcheck = pdc_true;
        int i = 0, nv = 1, icp = 0, usvp;
        int code, gid, usv, ic;

        (void) i;

        /* storage length of a character */
        if (outtextformat == pdc_utf16)
        {
            charlen = 2;
            newcharlen = 2;
        }

        /* maximal text string length - found out emprirically! */
        if (*outlen > maxlen && !(flags & PDF_KEEP_TEXTLEN))
        {
            int textlen = *outlen;

            if (!to->kerning && to->wordspacing == 0.0)
            {
                if (currfont->codesize == 1)
                    textlen /= charlen;
                maxlen = PDF_MAXTEXTSIZE;
            }
            else
            {
                if (currfont->codesize == 2)
                    maxlen *= charlen;
            }

            if (textlen > maxlen)
            {
                pdc_set_errmsg(p->pdc, PDF_E_TEXT_TOOLONG,
                               pdc_errprintf(p->pdc, "%d", textlen),
                               pdc_errprintf(p->pdc, "%d", maxlen), 0, 0);
                goto PDF_CHECK_TEXT_ERROR;
            }
        }

        len = *outlen / charlen;
        switch (enc)
        {


            /*
             * builtin
             */
            case pdc_builtin:
            if (charlen == 1 || !(flags & PDF_KEEP_UNICODE))
                newcharlen = 1;
            for (ic = 0; ic < len; ic++)
            {
                if (charlen == 1)
                    code = (int) outtext[ic];
                else
                    code = (int) usouttext[ic];

                if (code)
                {
                    gid = fnt_get_glyphid(code, &currfont->ft);

                    /* glyph id for code value not available */
                    if (gid <= 0)
                    {
                        if (to->glyphcheck == text_error)
                        {
                            pdc_set_errmsg(p->pdc, PDF_E_FONT_CODENOTFOUND1,
                                       pdc_errprintf(p->pdc, "x%02X", code),
                                       currfont->ft.name, 0, 0);
                            goto PDF_CHECK_TEXT_ERROR;
                        }
                        else if (to->glyphcheck == text_replace)
                        {
                            pdc_warning(p->pdc, PDF_E_FONT_CODENOTFOUNDREP1,
                                       pdc_errprintf(p->pdc, "x%02X", code),
                                       currfont->ft.name, 0, 0);
                            code = currfont->replacementcode;

                            if (fitres != NULL)
                                fitres->unknownchars++;
                        }

                        if (fitres != NULL)
                            fitres->unmappedchars++;
                    }
                }

                {
                    if (newcharlen == 1)
                        outtext[icp] = (pdc_byte) code;
                    else
                        usouttext[icp] = (pdc_ushort) code;
                    icp++;
                }
            }

            break;


            /*
             * cid
             */
            case pdc_cid:
            /*
             * pass through. check and temporary conversion in
             * pdf_calculate_textsize(), because we want to keep native code.
             */
            break;


            /*
             * encoding vector
             */
            default:
            if (charlen == 1 || !(flags & PDF_KEEP_UNICODE))
                newcharlen = 1;
            for (ic = 0; ic < len; ic++)
            {
                if (charlen == 1)
                {
                    code = (int) outtext[ic];
                    usv = ev->codes[code];
                    kcheck = code > 0;
                }
                else
                {
                    usv = (int) usouttext[ic];
                    code = pdc_get_encoding_bytecode(p->pdc, ev,
                                                     (pdc_ushort) usv);
                    if (code < 0)
                        code = 0;
                    kcheck = usv > 0;
                }

                if ((flags & PDF_KEEP_CONTROL) &&
                    pdc_is_linebreaking_relchar((pdc_ushort) usv))
                {
                    kcheck = pdc_false;
                }

                /* glyph check */
                if (kcheck)
                {
                    /* encoding vector hasn't defined [Uni]code */
                    if (usv <= 0 || code <= 0)
                    {
                        if (to->glyphcheck == text_error)
                        {
                            if (usv <= 0)
                            {
                                pdc_set_errmsg(p->pdc, PDC_E_ENC_NOTDEF_CODE,
                                           pdc_errprintf(p->pdc, "x%02X", code),
                                           ev->apiname, 0, 0);
                                goto PDF_CHECK_TEXT_ERROR;
                            }
                            else if (code <= 0)
                            {
                                pdc_set_errmsg(p->pdc, PDC_E_ENC_NOTDEF_UNICODE,
                                           pdc_errprintf(p->pdc, "%04X", usv),
                                           ev->apiname, 0, 0);
                                goto PDF_CHECK_TEXT_ERROR;
                            }
                        }
                        else if (to->glyphcheck == text_replace)
                        {
                            usvp = (usv <= 0) ? code : usv;
                            nv = pdf_get_approximate_uvlist(p, currfont, ev,
                                                            usv, pdc_true,
                                                            fitres,
                                                            uvlist, cglist);
                            usv = (int) uvlist[0];
                            code = (int) cglist[0];

                            if (logg2)
                            {
                                pdf_logg_glyph_replacement(p, ic, usvp,
                                              enc, charlen, uvlist, cglist, nv);
                            }
                        }

                        if (fitres != NULL)
                            fitres->unmappedchars++;
                    }
                    else
                    {
                        gid = fnt_get_glyphid(code, &currfont->ft);

                        /* glyph id for code not available */
                        if (gid <= 0 && currfont->gid0code != code)
                        {
                            if (to->glyphcheck == text_error)
                            {
                                pdc_set_errmsg(p->pdc, PDF_E_FONT_CODENOTFOUND2,
                                         pdc_errprintf(p->pdc, "x%02X", code),
                                         pdc_errprintf(p->pdc, "%04X", usv),
                                         currfont->ft.name, 0);
                                goto PDF_CHECK_TEXT_ERROR;
                            }
                            else if (to->glyphcheck == text_replace)
                            {
                                pdc_warning(p->pdc, PDF_E_FONT_CODENOTFOUNDREP2,
                                         pdc_errprintf(p->pdc, "x%02X", code),
                                         pdc_errprintf(p->pdc, "%04X", usv),
                                         currfont->ft.name, 0);

                                usvp = (usv <= 0) ? code : usv;
                                nv = pdf_get_approximate_uvlist(p, currfont, ev,
                                                                usv, pdc_true,
                                                                fitres,
                                                                uvlist, cglist);
                                usv = (int) uvlist[0];
                                code = (int) cglist[0];

                                if (logg2)
                                {
                                    pdf_logg_glyph_replacement(p, ic, usvp,
                                              enc, charlen, uvlist, cglist, nv);
                                }
                            }

                            if (fitres != NULL)
                                fitres->unmappedchars++;
                        }
                    }
                }

                {
                    if (newcharlen == 1)
                    {
                        outtext[icp] = (pdc_byte) code;
                    }
                    else
                    {
                        usouttext[icp] = (pdc_ushort) usv;
                    }
                    if (nv > 1)
                    {
                        if (newcharlen == 1)
                            ligatlist = pdf_register_ligat(p, ligatlist,
                                                  icp, nv, cglist, newcharlen);
                        else
                            ligatlist = pdf_register_ligat(p, ligatlist,
                                                  icp, nv, uvlist, newcharlen);
                        nv = 1;
                    }
                    icp++;
                }
            }

            break;
        }

        if (icp)
        {
            /* calculate complete text length */
            len = newcharlen * icp;
            if (ligatlist != NULL)
            {
                ligat = ligatlist;
                while (ligat != NULL)
                {
                    len += ligat->nb;
                    ligat = ligat->next;
                }
            }

            if (len != *outlen)
            {
                *outlen = len;
                if (flags & PDF_USE_TMPALLOC)
                    outtext = (pdc_byte *) pdc_realloc_tmp(p->pdc, outtext,
                                           (size_t) (*outlen + newcharlen), fn);
                else
                    outtext = (pdc_byte *) pdc_realloc(p->pdc, outtext,
                                           (size_t) (*outlen + newcharlen), fn);
                outtext[*outlen] = 0;
                if (newcharlen == 2)
                    outtext[*outlen + 1] = 0;
            }

            /* insert ligature parts */
            if (ligatlist != NULL)
            {
                int nbrest, nbgap, nbmove = 0;

                len = newcharlen * icp;
                ligat = ligatlist;
                while (ligat != NULL)
                {
                    nbgap = ligat->nb;
                    icp = ligat->icp + nbmove;
                    nbrest = len - icp;
                    icp += newcharlen;
                    ic = icp + nbgap;

                    memmove(&outtext[ic], &outtext[icp], (size_t) nbrest);
                    memcpy(&outtext[icp], ligat->culist, (size_t) nbgap);

                    nbmove += nbgap;
                    len += nbgap;

                    ligat = ligat->next;
                }

                pdf_cleanup_ligat(p, ligatlist);
            }
        }
        else if (enc != pdc_cid)
        {
            *outlen = 0;
        }
    }

    *outtext_p = outtext;
    *outcharlen = newcharlen;

    if (logg1)
    {
        if (logg2)
            pdc_logg_hexdump(p->pdc, "converted text", "\t\t",
                             (char *) outtext, *outlen);
        else
            pdc_logg(p->pdc,
                "\tChecked and converted text of length %d: \"%T\"\n",
                *outlen, outtext, *outlen);
    }

    if (*outlen)
        return pdc_true;

    verbose = pdc_false;

    PDF_CHECK_TEXT_ERROR:

    if (outtext != NULL)
    {
        if (flags & PDF_USE_TMPALLOC)
            pdc_free_tmp(p->pdc, outtext);
        else
            pdc_free(p->pdc, outtext);
    }

    pdf_cleanup_ligat(p, ligatlist);

    if (verbose)
        PDC_RETHROW(p->pdc);

    *outtext_p = NULL;
    *outlen = 0;

    return pdc_false;
}


/* ------------------------ Text width functions ------------------------ */

/* Calculates the geometrical width and height of input text string
 * depending on
 *
 *      to->font, to->fontsize, to->kerning,
 *      to->charspacing, to->horizscaling, to->wordspacing,
 *      to->xadvancelist
 *
 * In the case of vertical writing mode the width is the maximum
 * of all glyph widths and height the height of the text string.
 *
 */

pdc_scalar
pdf_calculate_textsize(PDF *p, const pdc_byte *text, int len, int charlen,
                       pdf_text_options *to, int breakchar, pdc_scalar *height,
                       pdc_bool verbose)
{
    pdf_font *currfont = &p->fonts[to->font];
    pdc_encoding enc = currfont->ft.enc;
    pdc_byte *tmpstring = (pdc_byte *) text;
    pdc_ushort *ustext = (pdc_ushort *) text;
    pdc_scalar glwidth = 0, width = 0;
    pdc_scalar font2user = to->fontsize / 1000.0;
    pdc_bool kbreak = pdc_false;
    int usv, ic, icc, numglyphs = 0, numspaces = 0;

    /* We cannot handle empty strings or fonts without widths info */
    if (!len || currfont->widthsmissing)
    {
        if (height)
            *height = 0.0;
        return width;
    }

    if (enc != pdc_cid)
        len /= charlen;

    for (ic = 0; ic < len; )
    {
        icc = ic;

        {
            if (charlen == 1)
            {
                usv = (int) text[ic];
            }
            else if (enc == pdc_unicode)
            {
                usv = pdc_char16_to_char32(p->pdc, ustext, &ic, len, verbose);
            }
            else
            {
                usv = (int) ustext[ic];
            }

            /* count spaces */
            if (usv == (int) currfont->ft.spacechar)
                numspaces++;

            /* break character */
            if (breakchar > 0)
                kbreak = (usv == breakchar);

            ic++;
        }

        /* start by adding in the width of the character */
        if (currfont->opt.monospace)
        {
            glwidth = (pdc_scalar) currfont->opt.monospace;
        }
        else
        {
            glwidth = (pdc_scalar) fnt_get_glyphwidth(usv, &currfont->ft);
            if (glwidth == FNT_MISSING_WIDTH)
                glwidth = currfont->ft.m.defwidth;
        }

        /* count glyphs */
        numglyphs++;

        /* horizontal or vertical writing mode */
        if (!currfont->ft.vertical)
        {
            width += glwidth;


            /* supplied glyph widths */
            if (icc < to->nglyphs)
            {
                pdc_scalar shift = to->xadvancelist[icc] / font2user - glwidth;
                width += shift;
                if (p->pdc->ptfrun)
                    shift = PDC_ROUND(1e10 * shift) / 1e10;
                shift = PDC_ROUND(1e1 * shift) / 1e1;
                to->xadvancelist[icc] = shift;
            }
        }
        else
        {
            /* maximum of width */
            if (glwidth > width)
                width = glwidth;
        }

        /* length of text part ranging to decimal character */
        if (kbreak)
            break;
    }

    if (breakchar > 0 && !kbreak)
        return 0;

    /* charspacing and wordspacing */
    if (!currfont->ft.vertical)
    {
        if (to->charspacing)
            width += numglyphs * to->charspacing / font2user;
        if (to->wordspacing)
            width += numspaces * to->wordspacing / font2user;
        if (height)
            *height = 0.0;
    }
    else
    {
        /* height for positive y direction.
         * Acrobat calculates with negative direction (see pdf_place_text).
         */
        *height = numglyphs * (to->fontsize + -to->charspacing) +
                  numspaces * (-to->wordspacing);
    }

    /* take horizontal scaling factor and font scaling factor into account */
    width *= font2user * to->horizscaling;

    if (tmpstring != text)
        pdc_free_tmp(p->pdc, tmpstring);

    return width;

}

pdc_scalar
pdf_trim_textwidth(pdc_scalar width, pdf_text_options *to)
{
    if (!PDC_FLOAT_ISNULL(width))
         width -= to->horizscaling * to->charspacing;

    return width;
}


pdc_scalar
pdf__stringwidth(PDF *p, const char *text, int len, int font,
                 pdc_scalar fontsize)
{
    pdc_byte *utext;
    int charlen;
    pdc_scalar width = 0, height = 0;
    pdf_text_options to = *p->curr_ppt->currto;

    len = pdc_check_text_length(p->pdc, &text, len, PDF_MAXTEXTSIZE);
    if (!len)
        return 0.0;

    pdf_check_handle(p, font, pdc_fonthandle);

    pdc_check_number_zero(p->pdc, "fontsize", fontsize);

    /* convert text string */
    to.font = font;
    to.fontsize = fontsize;
    if (pdf_check_textstring(p, text, len, PDF_KEEP_TEXTLEN | PDF_USE_TMPALLOC,
                             &to, NULL, &utext, &len, &charlen, pdc_true))
    {
        width = pdf_calculate_textsize(p, utext, len, charlen,
                                       &to, -1, &height, pdc_true);
    }

    return width;
}


/* ------------------------ Text output functions ------------------------ */

static void
pdf_convert_text_towinansi(PDF *p, const pdc_byte *fromtext, int len,
                           pdc_byte *totext, pdf_font *currfont)
{
    pdc_encodingvector *evfrom =
        pdc_get_encoding_vector(p->pdc, currfont->ft.enc);
    pdc_encodingvector *evto =
        pdc_get_encoding_vector(p->pdc, currfont->towinansi);
    int i;

    for (i = 0; i < len; i++)
        totext[i] = pdc_transform_bytecode(p->pdc, evto, evfrom, fromtext[i]);
}

void
pdf_put_fieldtext(PDF *p, const char *text, int font)
{
    if (pdc_is_utf8_bytecode(text))
    {
        pdf_put_hypertext(p, text);
    }
    else if (font > -1)
    {
        static const char fn[] = "pdf_put_fieldtext";
        pdf_font *currfont = &p->fonts[font];
        char *tmpstring = (char *) text;
        int len = (int) pdc_strlen(text);

        if (len && currfont->towinansi != pdc_invalidenc &&
            !pdc_is_utf16be_unicode(text))
        {
            /* Convert 8-bit code string to winansi */
            tmpstring = (char *) pdc_malloc_tmp(p->pdc,
                                                (size_t) len, fn, NULL, NULL);
            pdf_convert_text_towinansi(p, (pdc_byte *) text, len,
                                       (pdc_byte *) tmpstring, currfont);
        }

        pdc_put_pdfstring(p->out, tmpstring, len);
        if (tmpstring != text)
            pdc_free_tmp(p->pdc, tmpstring);
    }
}

static void
pdf_put_textstring(PDF *p, const pdc_byte *text, int len, int charlen,
                   pdf_font *currfont)
{
    static const char fn[] = "pdf_put_textstring";
    pdc_byte *tmpstring = (pdc_byte *) text;

    (void) charlen;

    if (len)
    {

            /* Convert 8-bit code string to winansi */
            if (currfont->towinansi != pdc_invalidenc)
            {
                tmpstring = (pdc_byte *) pdc_malloc_tmp(p->pdc,
                                             (size_t) len, fn, NULL, NULL);
                pdf_convert_text_towinansi(p, text, len, tmpstring, currfont);
            }

    }

    pdc_put_pdfstring(p->out, (char *) tmpstring, len);
    if (tmpstring != text)
        pdc_free_tmp(p->pdc, tmpstring);
}

static void
pdf_put_textstring_shift(PDF *p, pdc_byte *text, int len, int charlen,
                         pdf_text_options *to, pdc_scalar spaceshift)
{
    pdf_font *currfont = &p->fonts[to->font];
    pdc_ushort *ustext = (pdc_ushort *) text;
    pdc_byte *currtext;
    pdc_scalar shift;
    pdc_bool isutf16;
    int currlen, nchars;
    int leftchar = 0, rightchar;
    int ic, icp, incr = charlen;

    currlen = 0;
    currtext = text;
    nchars = len/charlen;
    isutf16 = charlen == 2 &&
              currfont->codesize == 2 &&
              currfont->ft.enc == pdc_unicode;
    for (ic = 0; ic < nchars; ic++)
    {
        if (charlen == 1)
        {
            rightchar = (int) text[ic];
        }
        else if(!isutf16)
        {
            rightchar = (int) ustext[ic];
        }
        else
        {
            icp = ic;
            rightchar =
                pdc_char16_to_char32(p->pdc, ustext, &ic, nchars, pdc_true);
            incr = (1 + ic - icp) * charlen;
        }

        if (ic)
        {
            /* PDF wants the inverse shift amount
             * (positive numbers move left, negative move right!) */

            if (spaceshift != 0 && leftchar == (int) currfont->ft.spacechar)
                shift = -spaceshift;
            else
                shift = 0;


            if (ic <= to->nglyphs)
                shift -= to->xadvancelist[ic-1];

            if (shift)
            {
                pdf_put_textstring(p, currtext, currlen, charlen, currfont);
                pdc_printf(p->out, "%f", shift);
                currtext = &text[charlen * ic];
                currlen = 0;
            }
        }
        leftchar = rightchar;
        currlen += incr;
    }

    pdf_put_textstring(p, currtext, currlen, charlen, currfont);

    if (to->nglyphs && to->nglyphs >= nchars)
        pdc_printf(p->out, "%f", -to->xadvancelist[nchars - 1]);

}


/* --------------------- General text placing function --------------------- */


#define PDF_RENDERMODE_FILLCLIP   4
#define PDF_ITALICANGLE_DEFAULT -12

static void
pdf_place_singletext(PDF *p, pdc_byte *text, int len, int charlen,
                     pdf_text_options *to, pdc_scalar tx, pdc_scalar ty,
                     pdc_scalar width, pdc_scalar height, pdc_scalar leading,
                     pdc_bool cont)
{
    pdf_tstate *ts = &p->curr_ppt->tstate[p->curr_ppt->sl];
    pdf_font *currfont = &p->fonts[to->font];
    pdc_scalar dx, dy, spaceshift = 0;
    pdc_scalar font2user = to->fontsize / 1000.0;
    pdc_scalar linewidth = 0;
    pdc_scalar deflinewidth = 0;
    pdc_bool hasdeco = to->underline || to->overline || to->strikeout;
    pdc_bool takeTJ = pdc_false;

    /* fill and stroke color */
    if (to->mask & (1 << to_fillcolor))
        pdf_set_coloropt(p, (int) pdf_fill, &to->fillcolor);
    if (to->mask & (1 << to_strokecolor))
        pdf_set_coloropt(p, (int) pdf_stroke, &to->strokecolor);

    if (to->mask & (1 << to_dasharray))
        pdf__setdash(p, to->dasharray[0], to->dasharray[1]);

    /* text decoration */
    if (width && hasdeco)
    {
        pdc_scalar scale = fabs(to->horizscaling);
        pdc_scalar delta, fs, trise, lineheight;
        pdc_scalar txe = 0, tye = 0;
        pdc_scalar lineposition = 0;

        fs = p->ydirection * font2user;
        trise = p->ydirection * to->textrise;
        lineheight = fs * currfont->ft.m.ascender;
        delta = scale * (fs * currfont->ft.m.underlinePosition + trise);

        pdf__save(p);

        if (currfont->ft.m.underlineThickness == 0)
            currfont->ft.m.underlineThickness = 50;
        deflinewidth = fabs(to->horizscaling * font2user *
                            currfont->ft.m.underlineThickness);

        if (to->underlinewidth == PDF_UNDERLINEWIDTH_AUTO)
        {
            linewidth = deflinewidth;
        }
        else
        {
            linewidth = to->underlinewidth;
            if ((to->pcmask & (1 << to_underlinewidth)))
                linewidth *= fabs(to->fontsize);
        }

        if (to->underlineposition == PDF_UNDERLINEPOSITION_AUTO)
        {
            lineposition = delta;
        }
        else
        {
            lineposition = p->ydirection * to->underlineposition;
            if ((to->pcmask & (1 << to_underlineposition)))
                lineposition *= to->fontsize;
        }

        if (!currfont->ft.vertical)
        {
            txe = tx + width;
        }
        else
        {
            txe = tx - width / 2.0;
            tye = ty - p->ydirection *  height;
            lineposition *= p->ydirection;
            delta = fabs(delta);
        }

        pdf__setlinecap(p, 0);
        if (!(to->mask & (1 << to_dasharray)))
            pdf__setdash(p, 0, 0);

        if (to->underline)
        {
            pdf__setlinewidth(p, linewidth);
            if (!currfont->ft.vertical)
            {
                pdf__moveto(p, tx,  ty + lineposition);
                pdf__lineto(p, txe, ty + lineposition);
            }
            else
            {
                pdf__moveto(p, txe + lineposition, ty);
                pdf__lineto(p, txe + lineposition, tye);
            }
            pdf__stroke(p);
        }

        if (to->strikeout)
        {
            pdf__setlinewidth(p, deflinewidth);
            if (!currfont->ft.vertical)
            {
                pdf__moveto(p, tx,  ty + lineheight/2 + delta);
                pdf__lineto(p, txe, ty + lineheight/2 + delta);
            }
            else
            {
                pdf__moveto(p, tx, ty);
                pdf__lineto(p, tx, tye);
            }
            pdf__stroke(p);
        }

        if (to->overline)
        {
            pdf__setlinewidth(p, deflinewidth);
            if (!currfont->ft.vertical)
            {
                delta = scale * (fs * currfont->ft.m.underlinePosition - trise);
                pdf__moveto(p, tx,  ty + lineheight - delta);
                pdf__lineto(p, txe, ty + lineheight - delta);
            }
            else
            {
                pdf__moveto(p, txe + width + delta, ty);
                pdf__lineto(p, txe + width + delta, tye);
            }
            pdf__stroke(p);
        }

        pdf__restore(p);
    }

    /* stroke width and dasharray for stroked text */
    if (to->mask & (1 << to_strokewidth) || to->textrendering)
    {
        if (to->strokewidth == PDF_UNDERLINEWIDTH_AUTO)
        {
            linewidth = PDC_ABS(to->horizscaling * font2user * 30);
        }
        else
        {
            linewidth = to->strokewidth;
            if ((to->pcmask & (1 << to_strokewidth)))
                linewidth *= fabs(to->fontsize);
        }
        pdf__setlinewidth(p, linewidth);
    }



    /* wordspacing */
    if (!PDC_FLOAT_ISNULL(to->wordspacing))
    {
        spaceshift = to->wordspacing / font2user;
        if (p->pdc->ptfrun)
            spaceshift = PDC_ROUND(1e10 * spaceshift) / 1e10;
        spaceshift = PDC_ROUND(1e1 * spaceshift) / 1e1;
        takeTJ = PDC_FLOAT_ISNULL(spaceshift) ? pdc_false : pdc_true;
    }


    /* supplied glyph widths */
    if (!takeTJ)
        takeTJ = to->nglyphs;

    /* begin text object */
    pdf_begin_text(p);

    /* italic angle - realized by Tm operator */
    if (!PDC_FLOAT_ISNULL(to->italicangle) ||
        currfont->metricflags & font_italic)
    {
        if (!currfont->ft.vertical)
        {
            pdc_scalar italicangle = -p->ydirection * to->italicangle;

            if (currfont->metricflags & font_italic && italicangle == 0)
                italicangle = -p->ydirection * PDF_ITALICANGLE_DEFAULT;

            if (ts->hs < 0)
                italicangle = -italicangle;

            pdc_printf(p->out, "1 0 %f 1 %f %f Tm\n",
                       tan(italicangle * PDC_DEG2RAD), tx, ty);

            cont = pdc_false;
            ts->newpos = pdc_false;
            ts->refptx = tx;
            ts->refpty = ty;
        }
        else
        {
            pdc_error(p->pdc, PDF_E_TEXT_ITALUNSUPP, 0, 0, 0, 0);
        }
    }
    else
    {
        /* components of text displacement vector */
        if (!cont)
        {
            dx = tx - ts->prevtx;
            dy = ty - ts->prevty;
        }
        else
        {
            dx = tx - ts->refptx;
            dy = ty - ts->refpty + leading;
        }

        /* condition for text displacement operator Td */
        if (!PDC_FLOAT_ISNULL(dx) || !PDC_FLOAT_ISNULL(dy) ||
            ts->newpos || (cont && takeTJ))
        {
            if (cont)
            {
                dy -= leading;
                cont = pdc_false;
            }
            pdc_printf(p->out, "%f %f Td\n", dx, dy);

            /* new reference position for next line */
            ts->newpos = pdc_false;
            ts->refptx = tx;
            ts->refpty = ty;
        }
        else
        {
            ts->refpty -= leading;
        }
    }

    /* show text */
    if (!takeTJ)
    {
        pdf_put_textstring(p, text, len, charlen, currfont);
        if (!cont)
            pdc_puts(p->out, "Tj\n");
        else
            pdc_puts(p->out, "'\n");
    }
    else
    {
        pdc_puts(p->out, "[");
        pdf_put_textstring_shift(p, text, len, charlen, to, spaceshift);
        pdc_puts(p->out, "]TJ\n");
    }

    /* new text position */
    if (!currfont->ft.vertical)
    {
        ts->currtx = tx + width;
        ts->currty = ty;
    }
    else
    {
        ts->currtx = tx;
        ts->currty = ty - p->ydirection *  height;
    }
    ts->prevtx = ts->currtx;
    ts->prevty = ts->currty;

    if (to->textrendering >= PDF_RENDERMODE_FILLCLIP)
        pdf_end_text(p);
}

#define PDF_FAKEBOLD_OFFSET 0.03    /* 3% of font size */

void
pdf_place_text(PDF *p, pdc_byte *text, int len, int charlen,
               pdf_text_options *to, pdc_scalar width, pdc_scalar height,
               pdc_bool cont)
{
    pdf_tstate *ts = &p->curr_ppt->tstate[p->curr_ppt->sl];
    pdf_font *currfont = &p->fonts[to->font];
    pdc_scalar tx, ty, leading = 0;

    /* text position */
    if (!cont)
    {
        tx = ts->currtx;
        ty = ts->currty;
    }
    else
    {
        leading = p->ydirection * to->leading;
        tx = ts->linetx;
        ty = ts->currty - leading;
    }

    pdf_place_singletext(p, text, len, charlen, to, tx, ty,
                         width, height, leading, cont);

    /* text bolding */
    if (to->fakebold || currfont->metricflags & font_bold)
    {
        static const pdc_scalar fx[] = {0, 0.70711, 1};
        static const pdc_scalar fy[] = {1, 0.70711, 0};
        pdc_scalar offset, currtx, currty, linetx;
        int it, nt = 3;

        offset = PDF_FAKEBOLD_OFFSET * to->fontsize;

        linetx = ts->linetx;
        currtx = ts->currtx;
        currty = ts->currty;
        for (it = 0; it < nt; it++)
        {
            pdf__set_text_pos(p, tx + fx[it] * offset,
                              ty + p->ydirection * fy[it] * offset);
            pdf_place_singletext(p, text, len, charlen, to,
                                 ts->currtx, ts->currty,
                                 width, height, leading, pdc_false);
        }
        pdf__set_text_pos(p, currtx, currty);
        ts->linetx = linetx;
    }
}

/* --------------------- Simple text showing functions --------------------- */

void
pdf__show_text(
    PDF *p,
    const char *text,
    int len,
    pdc_bool cont)
{
    static const char *fn = "pdf__show_text";
    pdf_text_options *currto = p->curr_ppt->currto;
    pdc_byte *utext = NULL;
    int charlen = 1;
    pdc_scalar width = 0, height = 0;

    len = pdc_check_text_length(p->pdc, &text, len, PDF_MAXTEXTSIZE);
    if (!len && !cont)
        return;

    /* no font set */
    if (currto->font == -1)
        pdc_error(p->pdc, PDF_E_TEXT_NOFONT, 0, 0, 0, 0);

    if (len)
    {
        /* convert text string */
        if (pdf_check_textstring(p, text, len, PDF_USE_TMPALLOC,
                        currto, NULL, &utext, &len, &charlen, pdc_true))
        {
            /* width and height of text string */
            width = pdf_calculate_textsize(p, utext, len, charlen,
                                           currto, -1, &height, pdc_true);
        }
        else if (!cont)
        {
            return;
        }
    }
    else
    {
        utext = (pdc_byte *) pdc_calloc_tmp(p->pdc, 2, fn, NULL, NULL);
    }


    /* place text */
    pdf_place_text(p, utext, len, charlen, currto, width, height, cont);
}


/* ---------- Text showing function with explicit glyph widths  ---------- */

void
pdf__xshow(PDF *p, const char *text, int len, const pdc_scalar *xadvancelist)
{
    static const char *fn = "pdf__xshow";
    pdf_text_options *currto = p->curr_ppt->currto;
    pdc_byte *utext = NULL;
    int charlen = 1;
    size_t nbytes = 0;
    pdc_scalar width, height;

    len = pdc_check_text_length(p->pdc, &text, len, PDF_MAXTEXTSIZE);
    if (!len)
        return;

    /* no font set */
    if (currto->font == -1)
        pdc_error(p->pdc, PDF_E_TEXT_NOFONT, 0, 0, 0, 0);

    /* convert text string */
    if (pdf_check_textstring(p, text, len, PDF_USE_TMPALLOC,
                             currto, NULL, &utext, &len, &charlen, pdc_true))
    {
        /* allocating glyph widths arrays */
        nbytes = (size_t) (len / charlen) * sizeof(pdc_scalar);
        currto->xadvancelist = (pdc_scalar *) pdc_malloc_tmp(p->pdc,
                                                 nbytes, fn, NULL, NULL);
        memcpy(currto->xadvancelist, xadvancelist, nbytes);
        currto->nglyphs = len / charlen;

        /* length of text */
        width = pdf_calculate_textsize(p, utext, len, charlen,
                                       currto, -1, &height, pdc_true);


        /* place text */
        pdf_place_text(p, utext, len, charlen, currto, width, height,
                       pdc_false);

        currto->xadvancelist = NULL;
        currto->nglyphs = 0;
    }
}


/* --------------------------- Leader functions ---------------------------- */



/* ----------------------- Text fitting function ------------------------ */


/* definitions of fit text options */
static const pdc_defopt pdf_fit_textline_options[] =
{
    PDF_TEXT_OPTIONS

    {"xadvancelist", pdc_scalarlist, PDC_OPT_NOZERO, 0, PDC_USHRT_MAX,
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL},

    PDF_FIT_OPTIONS1
    PDF_FIT_OPTIONS2
    PDF_FIT_OPTIONS6

    PDF_FONT_OPTIONS1
    PDF_FONT_OPTIONS2

    PDF_ERRORPOLICY_OPTION
    PDC_OPT_TERMINATE
};

pdc_resopt *
pdf_parse_fittextline_optlist(PDF *p, pdf_text_options *to,
                              pdf_fit_options *fit, const char *optlist)
{
    pdc_resopt *resopts = NULL;
    pdf_font_options fo;

    /* *to must be initialized */

    /* initialize fit options */
    pdf_init_fit_options(p, pdc_false, fit);
    fit->flags |= is_textline;

    /* initialize font options */
    pdf_init_font_options(p, &fo);
    fo.flags |= is_textline;

    /* parsing option list */
    if (optlist && strlen(optlist))
    {
        pdc_clientdata data;

        pdf_set_clientdata(p, &data);
        resopts = pdc_parse_optionlist(p->pdc, optlist,
                      pdf_fit_textline_options, &data, pdc_true);

        pdf_get_font_options(p, &fo, resopts);
        pdf_get_text_options(p, to, resopts);
        pdf_get_fit_options(p, pdc_false, fit, resopts);
    }

    /* font options specified */
    if (fo.mask & (1 << fo_fontname) && fo.mask & (1 << fo_encoding))
    {
        to->font = pdf_load_font_internal(p, &fo);
        to->mask |= (1L << to_font);
        to->fontset |= (1L << to_font);
    }
    else
    {
        pdf_cleanup_font_options(p, &fo);
    }

    return resopts;
}

static pdc_bool
pdf_parse_textline_options(PDF *p, const char *text, int len,
                           pdf_text_options *to, pdf_fit_options *fit,
                           const char *optlist)
{
    pdf_ppt *ppt = p->curr_ppt;

    len = pdc_check_text_length(p->pdc, &text, len, PDF_MAXTEXTSIZE);
    if (!len)
        return pdc_false;

    /* initialize text options */
    *to = *ppt->currto;
    to->text = (char *) text;
    to->textlen = len;

    /* parsing option list */
    pdf_parse_fittextline_optlist(p, to, fit, optlist);

    /* no font set */
    if (to->font == -1)
        pdc_error(p->pdc, PDF_E_TEXT_NOFONT, 0, 0, 0, 0);

    /* no font size set */
    if (to->fontsize == PDC_FLOAT_MIN)
    {
            pdc_error(p->pdc, PDF_E_TEXT_NOFONTSIZESET, 0, 0, 0, 0);
    }

    return pdc_true;
}

pdc_bool
pdf_fit_textline_internal(PDF *p, pdf_fitres *fitres,
                          pdf_text_options *to, pdf_fit_options *fit,
                          pdc_matrix *matrix)
{
    pdc_bool logg5 = pdc_logg_is_enabled(p->pdc, 5, trc_text);
    pdf_ppt *ppt = p->curr_ppt;
    pdf_font *currfont = &p->fonts[to->font];
    pdc_byte *utext = (pdc_byte *) "";
    int len, charlen;
    pdc_bool blind = (fitres != NULL) ? pdc_true : pdc_false;
    pdc_bool vertical = currfont->ft.vertical;

    pdc_matrix ctm = ppt->gstate[ppt->sl].ctm;
    pdc_matrix m, mm;
    pdc_vector elemsize, elemscale, elemmargin, textrelpos, fitrelpos;
    pdc_vector polyline[5];
    pdc_scalar textyextent[2];
    pdc_box fitbox, elembox;
    pdc_scalar ss, width, height, boxwidth, boxheight, fontsizeref;
    pdc_scalar ascender, capheight, xheight, descender;
    pdc_scalar x, y, tx = 0, ty = 0, basey = 0;
    /* pdc_bool hasfitbox = pdc_false; */
    /* pdc_bool hasboxwidth = pdc_false; */
    pdc_bool verbose = pdc_true;
    pdc_scalar font2user;
    int indangle = fit->orientate / 90;
    int i;

    (void) ppt;

    /* box size */
    boxwidth = fit->boxsize[0];
    boxheight = fit->boxsize[1];

    /* reference for font size as percentage */
    if (indangle % 2)
        fontsizeref = boxwidth;
    else
        fontsizeref = boxheight;

    /* calculate and set text options */
    pdf_calculate_text_options(p, to, pdc_false, 1.0, PDC_FLOAT_PREC,
                               fontsizeref);
    if (!blind)
        pdf_set_text_options(p, to);
    else
        verbose = fitres->verbose;

    /* convert text string */
    if (!pdf_check_textstring(p, to->text, to->textlen, PDF_USE_TMPALLOC,
                              to, fitres, &utext, &len, &charlen, verbose))
        return pdc_false;

    if (to->nglyphs && len/charlen != to->nglyphs)
        pdc_warning(p->pdc, PDF_E_TEXT_SIZENOMATCH,
                    pdc_errprintf(p->pdc, "%d", to->nglyphs),
                    pdc_errprintf(p->pdc, "%d", len/charlen), 0, 0);

    /* width and height of text */
    width = pdf_calculate_textsize(p, utext, len, charlen,
                                   to, -1, &height, pdc_true);
    width = pdf_trim_textwidth(width, to);

    /* incredible bug #1451
    if (PDC_FLOAT_ISNULL(width))
        return -1;
    */

    /* font specifics */
    font2user = to->fontsize / 1000.0;
    ascender = font2user * currfont->ft.m.ascender;
    capheight = font2user * currfont->ft.m.capHeight;
    xheight = font2user * currfont->ft.m.xHeight;
    descender = font2user * currfont->ft.m.descender;

    /* margin lower left corner */
    elemmargin.x = fit->margin[0];
    elemmargin.y = fit->margin[1];

    /* new box size */
    boxwidth -= 2 * elemmargin.x;
    if (boxwidth < 0)
        boxwidth = 0;
    boxheight -= 2 * elemmargin.y;
    if (boxheight < 0)
        boxheight = 0;
    /* hasboxwidth = boxwidth > PDC_FLOAT_PREC; */
    /* hasfitbox = hasboxwidth && boxheight > PDC_FLOAT_PREC; */

    /* kind of text box */
    pdf_get_mbox_boxheight(p, fit->matchbox, textyextent);

    /* text x size */
    elemsize.x = width;

    /* TODO: for vertical text */
    /* text y size */
    if (!vertical)
    {
        height = 0;
        for (i = 0; i < 2; i++)
        {
            basey = 0;
            if (textyextent[i] <= 0)
            {
                switch ((int) textyextent[i])
                {
                    case text_none:
                    break;

                    case text_ascender:
                    basey = ascender;
                    break;

                    case text_capheight:
                    basey = capheight;
                    break;

                    case text_xheight:
                    basey = xheight;
                    break;

                    case text_descender:
                    basey = -descender;
                    break;

                    case text_textrise:
                    basey = to->textrise;
                    break;

                    case text_fontsize:
                    basey = to->fontsize;
                    break;

                    case text_leading:
                    basey = to->leading;
                    break;
                }
            }
            else
            {
                basey = textyextent[i];
            }
            height += basey;
        }
    }
    elemsize.y = height;

    /* orientation */
    if (indangle % 2)
    {
        ss = elemsize.x;
        elemsize.x = elemsize.y;
        elemsize.y = ss;
    }

    /* box for fitting */
    fitbox.ll.x = 0;
    fitbox.ll.y = 0;
    fitbox.ur.x = boxwidth;
    fitbox.ur.y = boxheight;

    /* relativ position in fit and text box */
    fitrelpos.x = fit->position[0] / 100.0;
    fitrelpos.y = fit->position[1] / 100.0;
    textrelpos = fitrelpos;

    /* decimal character and position */
    if (fit->alignchar)
    {
        pdc_encoding enc = currfont->ft.enc;
        pdc_encodingvector *ev = pdc_get_encoding_vector(p->pdc, enc);
        pdc_scalar decwidth, decheight, pos1, pos2;
        int poschar = (int) fit->alignchar;

        switch(enc)
        {
            case pdc_cid:
            /* non-Unicode compatible CMaps seem to work (see bug #1605)
            if (currfont->codesize != 2)
                poschar = -1;
            */
            break;

            case pdc_glyphid:
            poschar = fnt_get_glyphid(poschar, &currfont->ft);
            break;

            case pdc_builtin:
            poschar = -1;
            break;

            default:
            if (ev != NULL && charlen == 1)
            {
                poschar = pdc_get_encoding_bytecode(p->pdc, ev,
                                                    (pdc_ushort) poschar);
            }
            break;
        }

        if (to->glyphcheck == text_error && poschar == -1)
        {
            pdc_set_errmsg(p->pdc, PDF_E_TEXT_ALIGNCHARNOTFOUND,
                           pdc_errprintf(p->pdc, "%04X", fit->alignchar),
                                         0, 0, 0);
            if (verbose)
                PDC_RETHROW(p->pdc);

            return pdc_false;
        }

        /* width and height of text part ranging to decimal character */
        decwidth = pdf_calculate_textsize(p, utext, len, charlen,
                                          to, poschar, &decheight, pdc_true);

        /* position found */
        if (decwidth > 0 && !PDC_FLOAT_ISNULL(width))
        {
            /* relative position of position character */
            pos1 = decwidth / width;
            pos2 = 1 - pos1;
            i = vertical ? ((indangle + 3) % 4) : indangle;
            switch (i)
            {
                case 0:
                textrelpos.x = pos1;
                break;

                case 1:
                textrelpos.y = pos1;
                break;

                case 2:
                textrelpos.x = pos2;
                break;

                case 3:
                textrelpos.y = pos2;
                break;
            }
        }
    }

    /* calculate image box */
    pdc_place_element(fit->fitmethod, fit->shrinklimit, &fitbox, &fitrelpos,
                      &elemsize, &textrelpos, &elembox, &elemscale);

    if (logg5)
        pdc_logg(p->pdc,
                 "\t\tFitting input parameter:\n"
                 "\t\t\tfitmethod = %s\n"
                 "\t\t\tshrinklimit = %f\n"
                 "\t\t\trefpoint = %f, %f\n"
                 "\t\t\tboxsize = %f, %f\n"
                 "\t\t\tfitrelpos = %f, %f\n"
                 "\t\t\telemsize = %f, %f\n"
                 "\t\t\ttextrelpos = %f, %f\n"
                 "\t\tFitting output parameter:\n"
                 "\t\t\telembox = %f, %f, %f, %f\n"
                 "\t\t\telemscale = %f, %f\n",
                 pdc_get_keyword(fit->fitmethod, pdf_fitmethod_keylist),
                 fit->shrinklimit, fit->refpoint[0], fit->refpoint[1],
                 fitbox.ur.x, fitbox.ur.y, fitrelpos.x, fitrelpos.y,
                 elemsize.x, elemsize.y, textrelpos.x, textrelpos.y,
                 elembox.ll.x, elembox.ll.y, elembox.ur.x, elembox.ur.y,
                 elemscale.x, elemscale.y);

    /* reference point */
    pdc_translation_matrix(fit->refpoint[0], fit->refpoint[1], &mm);
    if (matrix == NULL)
    {
        if (blind)
        {
            m = ctm;
            pdc_multiply_matrix(&mm, &m);
        }
        else
            m = mm;
    }
    else
    {
        m = *matrix;
        pdc_multiply_matrix(&mm, &m);
    }

    /* optional rotation */
    if (fabs(fit->rotate) > PDC_FLOAT_PREC)
    {
        pdc_rotation_matrix(p->ydirection * fit->rotate, &mm);
        pdc_multiply_matrix(&mm, &m);
    }

    /* output after translation and rotation */
    if (!blind)
    {
        /* new CTM */
        if (fit->showborder ||
            fit->fitmethod == pdc_clip || fit->fitmethod == pdc_slice)
        {
            pdf_concat_raw(p, &m);
            pdc_identity_matrix(&m);
        }

        /* show border */
        if (fit->showborder)
        {
            pdf__rect(p, elemmargin.x, p->ydirection * elemmargin.y,
                      boxwidth, boxheight);
            pdf__rect(p, 0, 0, fit->boxsize[0], fit->boxsize[1]);
            pdf__stroke(p);
        }

        /* clipping */
        if (
            (fit->fitmethod == pdc_clip || fit->fitmethod == pdc_slice))
        {
            pdc_scalar cw = fit->boxsize[0];
            pdc_scalar ch = fit->boxsize[1];

            if (cw < PDC_FLOAT_PREC)
                cw = PDF_ACRO_MAXPAGE;
            if (ch < PDC_FLOAT_PREC)
                ch = PDF_ACRO_MAXPAGE;
            pdf__rect(p, 0, 0, cw, ch);
            pdf__clip(p);
        }
    }

    /* reference point for elembox */
    if (elemmargin.x > PDC_FLOAT_PREC || elemmargin.y > PDC_FLOAT_PREC)
    {
        tx = elemmargin.x;
        if (boxwidth < PDC_FLOAT_PREC)
            tx *= 1.0 - 2 * fitrelpos.x;
        ty = elemmargin.y;
        if (boxheight < PDC_FLOAT_PREC)
            ty *= 1.0 - 2 * fitrelpos.y;

        pdc_translation_matrix(tx, p->ydirection * ty, &mm);
        pdc_multiply_matrix(&mm, &m);
    }


    /* translation of element box */
    elembox.ll.y *= p->ydirection;
    elembox.ur.y *= p->ydirection;
    pdc_box2polyline(NULL, &elembox, polyline);
    tx = polyline[indangle].x;
    ty = polyline[indangle].y;
    pdc_translation_matrix(tx, ty, &mm);
    pdc_multiply_matrix(&mm, &m);
    boxwidth = elembox.ur.x - elembox.ll.x;
    boxheight = elembox.ur.y - elembox.ll.y;

    /* orientation of text */
    if (fit->orientate != 0)
    {
        pdc_rotation_matrix(p->ydirection * fit->orientate, &mm);
        pdc_multiply_matrix(&mm, &m);
        if (indangle % 2)
        {
            ss = elemscale.x;
            elemscale.x = elemscale.y;
            elemscale.y = ss;

            ss = boxwidth;
            boxwidth = p->ydirection * boxheight;
            boxheight = p->ydirection * ss;

            ss = elemmargin.x;
            elemmargin.x = p->ydirection * elemmargin.y;
            elemmargin.y = p->ydirection * ss;
        }
    }

    /* matchbox */
    if (!blind && fit->matchbox)
    {
        pdc_rectangle matchrect;

        pdf_concat_raw(p, &m);
        pdc_identity_matrix(&m);

        matchrect.llx = 0;
        matchrect.lly = 0;
        matchrect.urx = boxwidth;
        matchrect.ury = boxheight;

        pdf_set_mbox_rectangle(p, fit->matchbox, &matchrect, 0);
        pdf_draw_mbox_rectangle(p, fit->matchbox,
                                mbox_saverestore | mbox_area | mbox_border);

        pdf_add_page_mbox(p, fit->matchbox);
    }

    /* scaling */
    if (elemscale.x != 1 || elemscale.y != 1)
    {
        pdc_scale_matrix(elemscale.x, elemscale.y, &mm);
        pdc_multiply_matrix(&mm, &m);
    }

    /* relative text position */
    if (!vertical)
    {
        x = 0;
        y = p->ydirection * basey;
    }
    else
    {
        x = width / 2.0;
        y = p->ydirection * height;
    }

    if (logg5)
        pdc_logg(p->pdc,
               "\t\tReference point:\n"
               "\t\t\tx = %f\n"
               "\t\t\ty = %f\n"
               "\t\tEmbedding matrix components of textline fitting:\n"
               "\t\t\ta = %f\n"
               "\t\t\tb = %f\n"
               "\t\t\tc = %f\n"
               "\t\t\td = %f\n"
               "\t\t\te = %f\n"
               "\t\t\tf = %f\n",
               x, y, m.a, m.b, m.c, m.d, m.e, m.f);

    /*
     * blind mode: pdf__info_textline
     */
    if (blind)
    {
        pdc_scalar mwidth = 2 * fabs(elemmargin.x);
        pdc_scalar mheight = 2 * fabs(elemmargin.y);
        pdc_scalar wlen, plen;

        /* start position */
        pdc_transform_point(&m, x, y, &tx, &ty);
        fitres->start.x = tx;
        fitres->start.y = ty;

        /* end position */
        if (!vertical)
        {
            tx = x + width;
            ty = y;
        }
        else
        {
            tx = x;
            ty = y - p->ydirection * height;
        }
        pdc_transform_point(&m, tx, ty, &tx, &ty);
        fitres->end.x = tx;
        fitres->end.y = ty;

        /* relative vector from start to end  */
        tx = fitres->end.x - fitres->start.x;
        ty = fitres->end.y - fitres->start.y;
        wlen = sqrt(tx * tx + ty * ty);
        if (!vertical)
        {
            /* width and x scaling */
            fitres->width = wlen;
            fitres->mwidth = wlen + mwidth;
        }
        else
        {
            /* height and y scaling */
            fitres->height = wlen;
            fitres->mheight = wlen + mheight;
        }

        /* unit vector of writingdir line */
        if (!PDC_FLOAT_ISNULL(wlen))
        {
            fitres->writingdir.x = tx / wlen;
            fitres->writingdir.y = ty / wlen;
        }
        else
        {
            fitres->writingdir.x = 0;
            fitres->writingdir.y = 0;
        }

        if (!vertical)
        {
            /* relative vector of fontsize */
            tx = x;
            ty = y + p->ydirection * height;
            pdc_transform_point(&m, tx, ty, &tx, &ty);
            tx -= fitres->start.x;
            ty -= fitres->start.y;
            plen = sqrt(tx * tx + ty * ty);

            /* height and y scaling */
            fitres->height = plen;
            fitres->mheight = plen + mheight;
        }
        else
        {

            /* relative vector of width */
            tx = x + width;
            ty = y;
            pdc_transform_point(&m, tx, ty, &tx, &ty);
            tx -= fitres->start.x;
            ty -= fitres->start.y;
            plen = sqrt(tx * tx + ty * ty);

            /* width ans x scaling */
            fitres->width = plen;
            fitres->mwidth = plen + mwidth;
        }

        /* unit vector of perpendiculardir line */
        if (!PDC_FLOAT_ISNULL(plen))
        {
            fitres->perpendiculardir.x = tx / plen;
            fitres->perpendiculardir.y = ty / plen;
        }
        else
        {
            fitres->perpendiculardir.x = 0;
            fitres->perpendiculardir.y = 0;
        }

        if (!PDC_FLOAT_ISNULL(width))
            fitres->scale.x = fitres->width / width;
        else
            fitres->scale.x = 1.0;

        if (!PDC_FLOAT_ISNULL(height))
            fitres->scale.y = fitres->height / height;
        else
            fitres->scale.y = 1.0;

        /* rotation angle of base line */
        fitres->angle = atan2(fitres->writingdir.y, fitres->writingdir.x) /
                        PDC_DEG2RAD;

        /* font specifics */
        fitres->ascender = pdc_transform_scalar(&m, ascender);
        fitres->capheight = pdc_transform_scalar(&m, capheight);
        fitres->xheight = pdc_transform_scalar(&m, xheight);
        fitres->descender = pdc_transform_scalar(&m, descender);
    }
    else
    {
        /* CTM */
        pdf_concat_raw(p, &m);

        /* set text position */
        pdf__set_text_pos(p, x, y);


        /* place text */
        pdf_place_text(p, utext, len, charlen, to, width, height, pdc_false);


        /* create a link - deprecated - */
        if (to->link)
        {
            if (pdf_check_textstring(p, to->text, to->textlen,
                                 PDF_USE_TMPALLOC | PDF_KEEP_UNICODE,
                                 to, NULL, &utext, &len, &charlen, pdc_true))
            {
                pdf_create_link(p, to->linktype,
                                x, y + p->ydirection * descender,
                                x + width, y + p->ydirection * to->fontsize,
                                to->link, (char *) utext, len);
                pdc_free_tmp(p->pdc, utext);
            }
        }
    }

    return pdc_true;
}

void
pdf_calculate_textline_size(PDF *p, pdf_text_options *to, pdf_fit_options *fit,
                    pdc_scalar *width, pdc_scalar *height)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_fitres fitres;
    pdc_matrix ctminv;

    /* calculate textline size for table cells */
    fitres.verbose = pdc_true;
    fitres.width = 0.0;
    fitres.height = 0.0;
    pdf_fit_textline_internal(p, &fitres, to, fit, NULL);

    pdc_invert_matrix(p->pdc, &ctminv, &ppt->gstate[ppt->sl].ctm);
    if (width)
        *width = pdc_transform_scalar(&ctminv, fitres.mwidth);
    if (height)
        *height = pdc_transform_scalar(&ctminv, fitres.mheight);
}

void
pdf__fit_textline(PDF *p, const char *text, int len, pdc_scalar x, pdc_scalar y,
                  const char *optlist)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_tstate *ts;
    pdf_text_options to;
    pdf_fit_options fit;
    pdc_matrix ctminv;
    pdc_scalar currtx, currty;

    pdc_check_number(p->pdc, "x", x);
    pdc_check_number(p->pdc, "y", y);

    /* parse options */
    if (!pdf_parse_textline_options(p, text, len, &to, &fit, optlist))
        return;

    fit.refpoint[0] = x;
    fit.refpoint[1] = y;

    pdf__save(p);

    /* output text line */
    pdf_fit_textline_internal(p, NULL, &to, &fit, NULL);
    pdf_cleanup_fit_options(p, &fit);

    ts = &ppt->tstate[ppt->sl];
    pdc_transform_point(&ppt->gstate[ppt->sl].ctm,
                        ts->currtx, ts->currty, &currtx, &currty);

    pdf__restore(p);

    /* calculate current text position*/
    pdc_invert_matrix(p->pdc, &ctminv, &ppt->gstate[ppt->sl].ctm);
    pdc_transform_point(&ctminv, currtx, currty, &currtx, &currty);
    pdf__set_text_pos(p, currtx, currty);
}

static const pdc_keyconn pdf_info_keylist[] =
{
    {"startx",            1},
    {"starty",            2},
    {"endx",              3},
    {"endy",              4},
    {"writingdirx",       5},
    {"writingdiry",       6},
    {"perpendiculardirx", 7},
    {"perpendiculardiry", 8},
    {"scalex",            9},
    {"scaley",           10},
    {"width",            11},
    {"height",           12},
    {"ascender",         13},
    {"capheight",        14},
    {"xheight",          15},
    {"descender",        16},
    {"angle",            17},

    {"unmappedglyphs",   20}, /* deprecated */
    {"unmappedchars",    20},
    {"replacedchars",    21},
    {"unknownchars",     22},
    {"wellformed",       23},
    {NULL, 0}
};

double
pdf__info_textline(PDF *p, const char *text, int len, const char *keyword,
                   const char *optlist)
{
    pdf_ppt *ppt = p->curr_ppt;
    pdf_text_options to;
    pdf_fit_options fit;
    pdf_fitres fitres;
    double tinfo = 0.0;
    int retval, infokey;

    if (!keyword || !*keyword)
        pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "keyword", 0, 0, 0);

    infokey = pdc_get_keycode_ci(keyword, pdf_info_keylist);
    if (infokey == PDC_KEY_NOTFOUND)
        pdc_error(p->pdc, PDC_E_ILLARG_STRING, "keyword", keyword, 0, 0);

    /* parse options */
    retval = pdf_parse_textline_options(p, text, len, &to, &fit, optlist);

    if (retval)
    {
        /* calculate textline */
        fitres.verbose = to.glyphwarning;
        fitres.unmappedchars = 0;
        fitres.replacedchars = 0;
        fitres.unknownchars = 0;
        retval = pdf_fit_textline_internal(p, &fitres, &to, &fit, NULL);
        pdf_cleanup_fit_options(p, &fit);

        if (retval)
        {
            pdf_font *currfont = &p->fonts[to.font];
            pdc_matrix ctminv;

            pdc_invert_matrix(p->pdc, &ctminv, &ppt->gstate[ppt->sl].ctm);

            switch(infokey)
            {
                case 1:
                case 2:
                pdc_transform_vector(&ctminv, &fitres.start, NULL);
                break;

                case 3:
                case 4:
                pdc_transform_vector(&ctminv, &fitres.end, NULL);
                break;

                case 5:
                case 6:
                pdc_transform_rvector(&ctminv, &fitres.writingdir, NULL);
                break;

                case 7:
                case 8:
                pdc_transform_rvector(&ctminv, &fitres.perpendiculardir, NULL);
                break;
            }

            pdc_logg_cond(p->pdc, 1, trc_text,
                "\tInfo textline%s:\n"
                "\tstartx = %f\n"
                "\tstarty = %f\n"
                "\tendx = %f\n"
                "\tendy = %f\n"
                "\twritingdirx = %f\n"
                "\twritingdiry = %f\n"
                "\tperpendiculardirx = %f\n"
                "\tperpendiculardiry = %f\n"
                "\tscalex = %f\n"
                "\tscaley = %f\n"
                "\twidth = %f\n"
                "\theight = %f\n"
                "\tascender = %f\n"
                "\tcapheight = %f\n"
                "\txheight = %f\n"
                "\tdescender = %f\n",
                currfont->ft.vertical ? " (vertical writing mode)" : "",
                fitres.start.x, fitres.start.y,
                fitres.end.x, fitres.end.y,
                fitres.writingdir.x, fitres.writingdir.y,
                fitres.perpendiculardir.x, fitres.perpendiculardir.y,
                fitres.scale.x, fitres.scale.y,
                fitres.width, fitres.height,
                fitres.ascender, fitres.capheight,
                fitres.xheight,fitres.descender);

            switch(infokey)
            {
                case 1:
                tinfo = (double) fitres.start.x;
                break;

                case 2:
                tinfo = (double) fitres.start.y;
                break;

                case 3:
                tinfo = (double) fitres.end.x;
                break;

                case 4:
                tinfo = (double) fitres.end.y;
                break;

                case 5:
                tinfo = (double) fitres.writingdir.x;
                break;

                case 6:
                tinfo = (double) fitres.writingdir.y;
                break;

                case 7:
                tinfo = (double) fitres.perpendiculardir.x;
                break;

                case 8:
                tinfo = (double) fitres.perpendiculardir.y;
                break;

                case 9:
                tinfo = (double) fitres.scale.x;
                break;

                case 10:
                tinfo = (double) fitres.scale.y;
                break;

                case 11:
                tinfo = (double) fitres.width;
                break;

                case 12:
                tinfo = (double) fitres.height;
                break;

                case 13:
                tinfo = (double) fitres.ascender;
                break;

                case 14:
                tinfo = (double) fitres.capheight;
                break;

                case 15:
                tinfo = (double) fitres.xheight;
                break;

                case 16:
                tinfo = (double) fitres.descender;
                break;

                case 17:
                tinfo = (double) fitres.angle;
                break;

                case 20:
                tinfo = (double) fitres.unmappedchars;
                break;

                case 21:
                tinfo = (double) fitres.replacedchars;
                break;

                case 22:
                tinfo = (double) fitres.unknownchars;
                break;

                /* wellformed */
                case 23:
                tinfo = (double) 1;
                break;
            }
        }
    }

    return tinfo;
}




/*****************************************************************************/
/**         deprecated historical text formatting function                  **/
/*****************************************************************************/

/* this helper function returns the width of the null-terminated string
** 'text' for the current font and size EXCLUDING the last character's
** additional charspacing.
*/
static pdc_scalar
pdf_swidth(PDF *p, const char *text)
{
    pdf_text_options *currto = p->curr_ppt->currto;

    pdc_scalar width = pdf_calculate_textsize(p,
                        (pdc_byte *) text, (int)strlen(text), 1,
                        currto, -1, NULL, pdc_true);
    return (width - currto->horizscaling * currto->charspacing);
}

static void
pdf_show_aligned(PDF *p, const char *text, pdc_scalar x, pdc_scalar y,
                 pdc_scalar wordspacing, pdf_alignment mode)
{
    if (!text)
	return;

    switch (mode) {
        default:
        case text_left:
        case text_justify:
        case text_fulljustify:
	    /* nothing extra here... */
	    break;

        case text_right:
	    x -= pdf_swidth(p, text);
	    break;

        case text_center:
	    x -= pdf_swidth(p, text) / 2;
	    break;
    }

    pdf__set_text_pos(p, x, y);
    pdf_set_tstate(p, wordspacing, to_wordspacing);
    pdf__show_text(p, text, (int) strlen(text), pdc_false);
}

int
pdf__show_boxed(
    PDF *p,
    const char *text, int len,
    pdc_scalar left,
    pdc_scalar bottom,
    pdc_scalar width,
    pdc_scalar height,
    const char *hmode,
    const char *feature)
{
    pdc_scalar  old_wordspacing, wordspacing, textwidth, curx, cury;
    pdc_bool	prematureexit;	/* return because box is too small */
    int		curTextPos;	/* character currently processed */
    int		lastdone;	/* last input character processed */
    int         toconv = len;
    pdf_text_options *currto = p->curr_ppt->currto;
    pdf_font *currfont;
    pdc_byte *utext = NULL;
    pdc_text_format old_textformat;
    pdf_alignment mode = text_left;
    pdc_bool blind = pdc_false;

    len = pdc_check_text_length(p->pdc, &text, len, PDF_MAXTEXTSIZE);
    if (!len)
        return 0;

    pdc_check_number(p->pdc, "left", left);
    pdc_check_number(p->pdc, "bottom", bottom);
    pdc_check_number(p->pdc, "width", width);
    pdc_check_number(p->pdc, "height", height);

    if (hmode == NULL || *hmode == '\0')
	pdc_error(p->pdc, PDC_E_ILLARG_EMPTY, "hmode", 0, 0, 0);

    if (!strcmp(hmode, "left"))
        mode = text_left;
    else if (!strcmp(hmode, "right"))
        mode = text_right;
    else if (!strcmp(hmode, "center"))
        mode = text_center;
    else if (!strcmp(hmode, "justify"))
        mode = text_justify;
    else if (!strcmp(hmode, "fulljustify"))
        mode = text_fulljustify;
    else
	pdc_error(p->pdc, PDC_E_ILLARG_STRING, "hmode", hmode, 0, 0);

    if (feature != NULL && *feature != '\0')
    {
	if (!strcmp(feature, "blind"))
	    blind = pdc_true;
	else
	    pdc_error(p->pdc, PDC_E_ILLARG_STRING, "feature", feature, 0, 0);
    }

    /* no font set */
    if (currto->font == -1)
        pdc_error(p->pdc, PDF_E_TEXT_NOFONT, 0, 0, 0, 0);
    currfont = &p->fonts[currto->font];

    if (width == 0 && height != 0)
        pdc_error(p->pdc, PDC_E_ILLARG_FLOAT,
            "width", pdc_errprintf(p->pdc, "%f", width), 0, 0);

    if (width != 0 && height == 0)
        pdc_error(p->pdc, PDC_E_ILLARG_FLOAT,
            "height", pdc_errprintf(p->pdc, "%f", height), 0, 0);

    if (currfont->ft.vertical)
    {
        pdc_error(p->pdc, PDF_E_DOC_FUNCUNSUPP, "vertical writing mode",
                  0, 0, 0);
    }

    /* we cannot handle several encodings */
    if (currfont->ft.enc == pdc_unicode)
    {
        pdc_error(p->pdc, PDF_E_DOC_FUNCUNSUPP, "Unicode", 0, 0, 0);
    }

    if (currfont->ft.enc == pdc_glyphid)
    {
        pdc_error(p->pdc, PDF_E_DOC_FUNCUNSUPP, "glyphid", 0, 0, 0);
    }

    if (currfont->ft.enc == pdc_cid)
    {
	pdc_error(p->pdc, PDF_E_DOC_FUNCUNSUPP, "CID", 0, 0, 0);
    }

    if (currfont->ft.enc == pdc_ebcdic ||
        currfont->ft.enc == pdc_ebcdic_37 ||
        currfont->ft.enc == pdc_ebcdic_winansi)
    {
	pdc_error(p->pdc, PDF_E_DOC_FUNCUNSUPP, "EBCDIC", 0, 0, 0);
    }

    /* old wordspacing */
    old_textformat = currto->textformat;

    /* convert text string */
    if (toconv)
    {
        int charlen;

        /* convert text string */
        if (!pdf_check_textstring(p, text, len,
                    PDF_KEEP_CONTROL | PDF_KEEP_TEXTLEN | PDF_USE_TMPALLOC,
                    currto, NULL, &utext, &len, &charlen, pdc_true))
            return 0;

        utext[len] = 0;
        text = (const char *) utext;
        currto->textformat = pdc_bytes;
    }

    /* old wordspacing */
    old_wordspacing = currto->wordspacing;

    /* special case for a single aligned line */
    if (width == 0 && height == 0)
    {
        if (!blind)
            pdf_show_aligned(p, text, left, bottom, old_wordspacing, mode);

        if (toconv)
            currto->textformat = old_textformat;
        return 0;
    }

    curx = left;
    cury = bottom + p->ydirection * height;
    prematureexit = pdc_false;
    curTextPos = 0;
    lastdone = 0;

    /* switch curx for right and center justification */
    if (mode == text_right)
	curx += width;
    else if (mode == text_center)
	curx += (width / 2);

#define	MAX_CHARS_IN_LINE	2048

    /* loop until all characters processed, or box full */

    while ((curTextPos < len) && !prematureexit)
    {
	/* buffer for constructing the line */
	char	linebuf[MAX_CHARS_IN_LINE];
	int	curCharsInLine = 0;	/* # of chars in constructed line */
	int	lastWordBreak = 0;	/* the last seen space char */
	int	wordBreakCount = 0;	/* # of blanks in this line */

	/* loop over the input string */
        while (curTextPos < len)
        {
	    if (curCharsInLine >= MAX_CHARS_IN_LINE)
		pdc_error(p->pdc, PDC_E_ILLARG_TOOLONG, "(text line)",
		    pdc_errprintf(p->pdc, "%d", MAX_CHARS_IN_LINE-1), 0, 0);

	    /* abandon DOS line-ends */
	    if (text[curTextPos] == PDF_RETURN &&
		text[curTextPos+1] == PDF_NEWLINE)
		    curTextPos++;

	    /* if it's a forced line break draw the line */
	    if (text[curTextPos] == PDF_NEWLINE ||
		text[curTextPos] == PDF_RETURN)
            {
                cury -= p->ydirection * currto->leading;

                if (p->ydirection * (cury - bottom) < 0) {
		    prematureexit = pdc_true;	/* box full */
		    break;
		}

                linebuf[curCharsInLine] = 0;    /* terminate the line */

		/* check whether the line is too long */
                wordspacing = 0;
                pdf_set_tstate(p, wordspacing, to_wordspacing);
                textwidth = pdf_swidth(p, linebuf);

		/* the forced break occurs too late for this line */
		if (textwidth > width)
                {
		    if (wordBreakCount == 0) {	/* no blank found */
			prematureexit = pdc_true;
			break;
		    }
                    linebuf[lastWordBreak] = 0;   /* terminate at last blank */
		    if (curTextPos > 0 && text[curTextPos-1] == PDF_RETURN)
			--curTextPos;
		    curTextPos -= (curCharsInLine - lastWordBreak);

		    if (!blind)
                    {
                        textwidth = pdf_swidth(p, linebuf);
			if (wordBreakCount != 1 &&
                                (mode == text_justify ||
                                 mode == text_fulljustify))
                        {
                            wordspacing = (width - textwidth) /
                                ((wordBreakCount - 1) * currto->horizscaling);
			}
                        pdf_show_aligned(p, linebuf, curx, cury, wordspacing,
                                         mode);
		    }
		}
                else if (!blind)
                {
                    if (mode == text_fulljustify && wordBreakCount > 0)
                    {
                        wordspacing = (width - textwidth) /
                                  (wordBreakCount * currto->horizscaling);
		    }
                    pdf_show_aligned(p, linebuf, curx, cury, wordspacing, mode);
		}

		lastdone = curTextPos;
		curCharsInLine = lastWordBreak = wordBreakCount = 0;
		curTextPos++;

	    }
            else if (text[curTextPos] == PDF_SPACE)
            {
                linebuf[curCharsInLine] = 0;    /* terminate the line */

		/* line too long ==> break at last blank */
                wordspacing = 0;
                pdf_set_tstate(p, wordspacing, to_wordspacing);
                if (pdf_swidth(p, linebuf) > width)
                {
                    cury -= p->ydirection * currto->leading;

                    if (p->ydirection * (cury - bottom) < 0)
                    {
			prematureexit = pdc_true; 	/* box full */
			break;
		    }

                    linebuf[lastWordBreak] = 0; /* terminate at last blank */
		    curTextPos -= (curCharsInLine - lastWordBreak - 1);

		    if (lastWordBreak == 0)
			curTextPos--;

		    /* LATER: * force break if wordBreakCount == 1,
		     * i.e., no blank
		     */
		    if (wordBreakCount == 0)
                    {
			prematureexit = pdc_true;
			break;
		    }

		    /* adjust word spacing for full justify */
                    if (wordBreakCount != 1 && (mode == text_justify ||
                                                mode == text_fulljustify))
                    {
                        textwidth = pdf_swidth(p, linebuf);
                        wordspacing = (width - textwidth) /
                            ((wordBreakCount - 1) * currto->horizscaling);
		    }

		    lastdone = curTextPos;
		    if (!blind)
                    {
                        pdf_show_aligned(p, linebuf, curx, cury, wordspacing,
                                         mode);
                    }
		    curCharsInLine = lastWordBreak = wordBreakCount = 0;
		}
                else
                {
		    /* blank found, and line still fits */
		    wordBreakCount++;
		    lastWordBreak = curCharsInLine;
		    linebuf[curCharsInLine++] = text[curTextPos++];
		}
	    }
            else
            {
		/* regular character ==> store in buffer */
		linebuf[curCharsInLine++] = text[curTextPos++];
	    }
	}

	if (prematureexit) {
	    break;		/* box full */
	}

	/* if there is anything left in the buffer, draw it */
        if (curTextPos >= len && curCharsInLine != 0)
        {
            cury -= p->ydirection * currto->leading;

            if (p->ydirection * (cury - bottom) < 0)
            {
		prematureexit = pdc_true; 	/* box full */
		break;
	    }

            linebuf[curCharsInLine] = 0;        /* terminate the line */

	    /* check if the last line is too long */
            wordspacing = 0;
            pdf_set_tstate(p, wordspacing, to_wordspacing);
            textwidth = pdf_swidth(p, linebuf);

	    if (textwidth > width)
            {
		if (wordBreakCount == 0)
		{
		    prematureexit = pdc_true;
		    break;
		}

                linebuf[lastWordBreak] = 0;     /* terminate at last blank */
		curTextPos -= (curCharsInLine - lastWordBreak - 1);

		/* recalculate the width */
                textwidth = pdf_swidth(p, linebuf);

		/* adjust word spacing for full justify */
                if (wordBreakCount != 1 && (mode == text_justify ||
                                            mode == text_fulljustify))
                {
                    wordspacing = (width - textwidth) /
                        ((wordBreakCount - 1) * currto->horizscaling);
		}
	    }
            else if (!blind)
            {
                if (mode == text_fulljustify && wordBreakCount)
                {
                    wordspacing = (width - textwidth) /
                              (wordBreakCount * currto->horizscaling);
                }
            }

	    lastdone = curTextPos;
	    if (!blind)
            {
                pdf_show_aligned(p, linebuf, curx, cury, wordspacing, mode);
            }
	    curCharsInLine = lastWordBreak = wordBreakCount = 0;
	}
    }

    pdf_set_tstate(p, old_wordspacing, to_wordspacing);

    /* return number of remaining characters  */

    while (text[lastdone] == PDF_SPACE)
	++lastdone;

    if ((text[lastdone] == PDF_RETURN ||
	text[lastdone] == PDF_NEWLINE) && text[lastdone+1] == 0)
	    ++lastdone;

    if (toconv)
        currto->textformat = old_textformat;

    return (int) (len - lastdone);
}
