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
 * PDFlib TrueType handling routines
 *
 */

#include "p_intern.h"
#include "p_font.h"

#include "ft_truetype.h"

#ifdef PDF_TRUETYPE_SUPPORTED


pdc_bool
pdf_get_metrics_tt(PDF *p, pdf_font *font, const char *fontname,
                   pdc_encoding enc, const char *filename)
{
    pdc_bool logg1 = pdc_logg_is_enabled(p->pdc, 1, trc_font);
    pdc_bool logg2 = pdc_logg_is_enabled(p->pdc, 2, trc_font);
    /* int filesize = 0; */
    /* double kbfilesize = 0; */
    int foundglyphs, flags = 0;
    tt_file *ttf;
    pdc_bool retval;
    pdc_encoding enc_req;
    /* pdc_encodingvector *ev = NULL; */
    pdc_bool isotf;
    int errcode = 0;

    (void) logg2;

    /*
     * Initialisation
     */
    ttf = fnt_new_tt(p->pdc, &font->ft);
    ttf->filename = filename;
    ttf->fontname = fontname;
    ttf->verbose = font->verbose;
    ttf->incore = pdc_true;
    ttf->monospace = font->opt.monospace;
    /* filesize = font->ft.filelen; */
    /* kbfilesize = filesize / 1024.0; */

    /*
     * Read font file
     */
    retval = fnt_read_tt(ttf);
    if (retval == pdc_false)
        goto PDF_TRUETYPE_ERROR2;

    /*
     * Font type
     */
    if (ttf->tab_CFF_)
    {
        isotf = pdc_true;
        font->ft.m.type = fnt_Type1C;
	font->cff_offset = (long) ttf->tab_CFF_->offset;
	font->cff_length = ttf->tab_CFF_->length;
    }
    else
    {
        isotf = pdc_false;
        font->ft.m.type = fnt_TrueType;
        TT_IOCHECK(ttf, tt_tag2idx(ttf, fnt_str_glyf) != -1);
        TT_IOCHECK(ttf, tt_tag2idx(ttf, fnt_str_loca) != -1);
    }

    /* Number of Glyphs */
    if (ttf->numGlyphs <= 1)
    {
        errcode = FNT_E_TT_NOGLYFDESC;
        goto PDF_TRUETYPE_ERROR1;
    }


    /*
     * Encoding
     */
    if (isotf)
    {
        /* OpenType font with CFF table */
        if (ttf->charcoll != cc_none)
        {
            /* CID font */
            if (font->ft.m.charcoll != cc_none)
            {
                if (!ttf->regisadobe)
                {
                    errcode = PDF_E_CJK_UNSUPP_REGISTRY;
                    goto PDF_TRUETYPE_ERROR1;
                }

                if (font->ft.m.charcoll != ttf->charcoll)
                {
                    errcode = PDF_E_CJK_UNSUPP_CHARCOLL;
                    goto PDF_TRUETYPE_ERROR1;
                }


                if (font->outcmapname != NULL)
                    enc = pdc_cid;

                if (logg1)
                    pdc_logg(p->pdc, "\tCID font ordering: \"%s\"\n",
                             fnt_get_ordering_cid(ttf->charcoll));
            }
            else if (enc == pdc_unicode || enc == pdc_glyphid)
            {
                font->ft.m.charcoll = ttf->charcoll;
                font->supplement = ttf->supplement;
            }
            else
            {
                errcode = PDF_E_FONT_ONLY_CMAP;
                goto PDF_TRUETYPE_ERROR1;
            }
        }
        else if (font->ft.m.charcoll != cc_none)
        {
            /* SID font */
            errcode = PDF_E_FONT_UNSUPP_CMAP;
            goto PDF_TRUETYPE_ERROR1;
        }
    }
    else
    {
        if (font->ft.m.charcoll != cc_none)
        {
            int i;
            pdc_bool iscjk = pdc_false;

            for (i = 0; i < PDC_NUMCHARCOLL; i++)
            {
                if (ttf->tab_OS_2->charcolls[i])
                    iscjk = pdc_true;

                if (ttf->tab_OS_2->charcolls[i] == font->ft.m.charcoll)
                    break;
            }
            if (i == PDC_NUMCHARCOLL)
            {
                if (iscjk)
                {
                    /* CJK font */
                    errcode = PDF_E_CJK_UNSUPP_CHARCOLL;
                    goto PDF_TRUETYPE_ERROR1;
                }
                else
                {
                    /* no CJK font */
                    errcode = PDF_E_FONT_UNSUPP_CMAP;
                    goto PDF_TRUETYPE_ERROR1;
                }
            }
            else
            {
                if (font->outcmapname != NULL)
                {
                    ttf->charcoll = font->ft.m.charcoll;
                    enc = pdc_cid;
                }
            }
        }
    }

    /* encoding vector */
    enc_req = fnt_get_tt_encoding_key(ttf, enc);
    if (enc_req == pdc_invalidenc)
    {
        errcode = FNT_E_TT_BADCMAP;
        goto PDF_TRUETYPE_ERROR1;
    }
    else if (enc_req != enc)
    {
        if (strcmp(font->encapiname, "auto"))
        {
            pdc_warning(p->pdc, PDF_E_FONT_FORCEENC,
                        pdf_get_encoding_name(p, enc_req, NULL),
                        0, 0, 0);
        }
        enc = enc_req;
    }
    if (enc >= 0)
        /* ev = */ pdc_get_encoding_vector(p->pdc, enc);
    font->ft.enc = enc;
    font->ft.issymbfont = ttf->issymbol;
    font->hasnomac = !ttf->tab_cmap || !ttf->tab_cmap->mac;


    /* builtin encoding */
    if (enc == pdc_builtin)
    {
        if (font->ft.issymbfont == pdc_false)
        {
            errcode = PDF_E_FONT_BADENC;
            goto PDF_TRUETYPE_ERROR1;
        }
        else
        {
            /* encoding vector for builtin */
            /* ev = */ pdf_create_font_encoding(p, enc, font, fontname, pdc_true);
            font->symenc = font->ft.enc;
        }
    }

    {
        /* optimizing PDF output */
        if (enc == pdc_ebcdic ||
            enc == pdc_ebcdic_37 ||
            enc == pdc_ebcdic_winansi)
            font->towinansi = pdc_winansi;

    }

    /* /FontName in FontDescriptor */
    font->ft.m.name = pdc_strdup(p->pdc, ttf->tab_name->englishname4);

    /* /BaseFont name */
    font->ft.name = pdc_strdup(p->pdc, ttf->tab_name->englishname6);


#define PDF_RESTRICTED_TT_EMBEDDING	0x02
    /* check embedding flags */
    if ((font->opt.embedding) && ttf->tab_OS_2 &&
	ttf->tab_OS_2->fsType == PDF_RESTRICTED_TT_EMBEDDING)
    {
        errcode = FNT_E_TT_EMBED;
        goto PDF_TRUETYPE_ERROR1;
    }


    if (logg1)
    {
        pdc_logg(p->pdc,
            "\tFull font name: \"%s\"\n"
            "\tPostScript font name: \"%s\"\n"
            "\tFont embedding: %s\n"
            "\tVertical font: %s\n",
            font->ft.name, font->ft.m.name,
            PDC_BOOLSTR(font->opt.embedding),
            PDC_BOOLSTR(font->ft.vertical));

        if (ttf->tab_name->producer != NULL)
            pdc_logg(p->pdc, "\tFont producer: \"%s\"\n",
                     ttf->tab_name->producer);

        pdc_logg(p->pdc, "\tNumber of Glyphs: %d\n", ttf->numGlyphs);
    }

    /* Save font values */
    fnt_set_tt_fontvalues(ttf);

    /* Flags for creating font arrays */
    flags = TT_FONT_code2gid | TT_FONT_m_widths;



    /* Create font mapping and width arrays */
    foundglyphs = fnt_set_tt_fontarrays(ttf, flags);

   /***********************************/
    if (font->symenc != pdc_invalidenc)
        font->ft.enc = pdc_builtin;
   /***********************************/

    if (!foundglyphs)
    {
        errcode = PDF_E_FONT_BADENC;
        goto PDF_TRUETYPE_ERROR1;
    }


    fnt_delete_tt(ttf);

    if (!pdf_make_fontflag(p, font))
        return pdc_false;

    return pdc_true;

    PDF_TRUETYPE_ERROR1:
    pdc_set_errmsg(p->pdc, errcode, 0, 0, 0, 0);

    PDF_TRUETYPE_ERROR2:
    fnt_delete_tt(ttf);

    return pdc_false;
}


#endif /* PDF_TRUETYPE_SUPPORTED */
