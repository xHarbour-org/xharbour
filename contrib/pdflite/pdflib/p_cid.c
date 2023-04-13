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
 * PDFlib CID font handling routines
 *
 */

#include "p_intern.h"
#include "p_font.h"

#include "ft_cid.h"


/*
** Returns CMap slot and for standard CJK fonts: fontandle.
**
** pdc_invalidenc: predefined CMap not found
** pdc_cid or pdc_unicode: predefined CMap found
**
** *o_slot:
** >= 0:  standard font found
**  < 0:  |error code|
*/
pdc_bool
pdf_handle_cidfont(PDF *p, const char *fontname, const char *encoding,
                   pdc_encoding enc, pdf_font *font, int *o_slot,
                   pdc_encoding *newenc)
{
    const char *encapiname = encoding;
    fnt_cmap_info cmapinfo;
    const fnt_font_metric *fontmetric;
    pdc_bool isidentity = pdc_false;
    /* pdc_bool isstdfont = pdc_false; */
    pdc_bool iscjkcp = pdc_false;
    int charcoll, slot;

    (void) enc;
    (void) iscjkcp;
    (void) encapiname;

    *o_slot = -1;
    *newenc = pdc_invalidenc;


    /*
     * Look whether font is already in the cache.
     * If font with same name and encoding is found,
     * returns its slot number.
     */

    for (slot = 0; slot < p->fonts_number; slot++)
    {
        if (p->fonts[slot].ft.enc == pdc_cid &&
            p->fonts[slot].opt.fontstyle == font->opt.fontstyle &&
            p->fonts[slot].opt.embedding == font->opt.embedding &&
            !strcmp(p->fonts[slot].apiname, fontname) &&
            !strcmp(p->fonts[slot].ft.cmapname, encoding))
        {
            *o_slot = slot;
            *newenc = pdc_cid;
            return pdc_true;
        }
    }

    /* Check the requested CMap */
    charcoll = fnt_get_predefined_cmap_info(encoding, &cmapinfo);
    if (charcoll == (int) cc_none)
        return pdc_true;

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\tPredefined CMap \"%s\" found\n", encoding);

    /* Check whether this CMap is supported in the desired PDF version */
    if (cmapinfo.compatibility > p->compatibility)
    {
        pdc_set_errmsg(p->pdc, PDF_E_DOC_PDFVERSION,
            encoding, pdc_get_pdfversion(p->pdc, p->compatibility), 0, 0);
        return pdc_false;
    }

    /* For Unicode capable language wrappers only UCS2/UTF16 CMaps allowed */
    if (cmapinfo.codesize == 0 && p->pdc->unicaplang)
    {
        pdc_set_errmsg(p->pdc, PDF_E_FONT_NEEDUCS2, 0, 0, 0, 0);
        return pdc_false;
    }

    /* Check whether the font name is among the known Acrobat CJK fonts */
    charcoll = fnt_get_preinstalled_cidfont(fontname, &fontmetric);
    isidentity = cmapinfo.charcoll == (int) cc_identity;
    if (isidentity)
        cmapinfo.charcoll = abs(charcoll);

    /* known Acrobat CID font */
    if (charcoll != (int) cc_none)
    {
        pdc_logg_cond(p->pdc, 1, trc_font,
            "\tStandard CJK font \"%s\" found\n", fontname);

        /* Selected CMap and known standard font don't match */
        if ((cmapinfo.charcoll != abs(charcoll) ||
             (charcoll == (int) cc_japanese && cmapinfo.codesize == -2)))
        {
            pdc_set_errmsg(p->pdc, PDF_E_CJK_UNSUPP_CHARCOLL,
                           0, 0, 0, 0);
            return pdc_false;
        }
        /* isstdfont = pdc_true; */


        /* Embedding not possible */
        if (font->opt.embedding)
        {
            pdc_set_errmsg(p->pdc, PDF_E_FONT_EMBEDCMAP, 0, 0, 0, 0);
            return pdc_false;
        }
    }
#ifdef WIN32
    else if (iscjkcp && !p->pdc->ptfrun)
    {
        return pdc_true;
    }
#endif


    /* embedding check */
    if (!pdf_check_font_embedding(p, font, fontname))
        return pdc_false;

    /* supplement number, number of codes = (maximal) number of CIDs */
    font->supplement = fnt_get_supplement(&cmapinfo, p->compatibility);
    if (isidentity)
        font->supplement = -1;
    font->ft.numcodes = fnt_get_maxcid(cmapinfo.charcoll, font->supplement) + 1;

    {
        font->passthrough = pdc_true;
        font->codesize = 0;
    }

    /* CMap and default settings */
    font->ft.vertical = cmapinfo.vertical;
    font->ft.cmapname = pdc_strdup(p->pdc, encoding);
    if (font->outcmapname == NULL)
        font->outcmapname = pdc_strdup(p->pdc, encoding);
    font->ft.enc = pdc_cid;
    font->iscidfont = pdc_true;

    /* Fill up the font struct */
    fnt_fill_font_metric(p->pdc, &font->ft, pdc_false, fontmetric);

    /* CID widths not available */
        font->widthsmissing = pdc_true;

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\n\t%s CJK font: \"%s\"\n\tPredefined CMap: \"%s\"\n"
        "\tOrdering: \"%s\"\n\tSupplement: %d\n",
        font->ft.isstdfont ? "Adobe Standard" : "Custom", fontname, encoding,
        fnt_get_ordering_cid(font->ft.m.charcoll), font->supplement);

    *newenc = pdc_cid;

    return pdc_true;
}

#ifdef PDF_CJKFONTWIDTHS_SUPPORTED
void
pdf_put_cidglyph_widths(PDF *p, pdf_font *font)
{
    if (font->opt.monospace)
    {
        if (font->opt.monospace != FNT_DEFAULT_CIDWIDTH)
        pdc_printf(p->out, "/DW %d\n", font->opt.monospace);
    }
    else
    {
        const char **widths = fnt_get_cid_widths_array(p->pdc, &font->ft);
        int i;

        pdc_puts(p->out, "/W");
        pdc_begin_array(p->out);
        for (i = 0; i < FNT_CIDMETRIC_INCR - 1; i++)
            pdc_puts(p->out, widths[i]);
        pdc_end_array(p->out);
    }
}
#endif /* PDF_CJKFONTWIDTHS_SUPPORTED */
