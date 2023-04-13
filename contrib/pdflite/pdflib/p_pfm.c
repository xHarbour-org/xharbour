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
 * PDFlib routines for fast reading of PFM font metrics files
 *
 */

#include "p_intern.h"
#include "p_font.h"

/* read data types from the PFM */
#define PFM_BYTE(offset)  pfm[offset]
#define PFM_WORD(offset)  PDC_GET_WORD(&pfm[offset])
#define PFM_SHORT(offset) PDC_GET_SHORT(&pfm[offset])
#define PFM_DWORD(offset) PDC_GET_DWORD3(&pfm[offset])

/* Offsets in the buffer containing the various PFM structures */
#define header_base		0
#define header_dfVersion	(PFM_WORD(header_base + 0))
#define header_dfSize		(PFM_DWORD(header_base + 2))
#define header_dfAscent		(PFM_WORD(header_base + 74))
#define header_dfItalic		(PFM_BYTE(header_base + 80))
#define header_dfWeight		(PFM_WORD(header_base + 83))
#define header_dfCharSet	(PFM_BYTE(header_base + 85))
#define header_dfPitchAndFamily	(PFM_BYTE(header_base + 90))
#define header_dfMaxWidth	(PFM_WORD(header_base + 93))
#define header_dfFirstChar	(PFM_BYTE(header_base + 95))
#define header_dfLastChar	(PFM_BYTE(header_base + 96))
#define header_dfDefaultChar	(PFM_BYTE(header_base + 97))

#define ext_base		117
#define ext_dfExtentTable	(PFM_DWORD(ext_base + 6))
#define ext_dfKernPairs		(PFM_DWORD(ext_base + 14))
#define ext_dfKernTrack		(PFM_DWORD(ext_base + 18))
#define ext_dfDriverInfo	(PFM_DWORD(ext_base + 22))

#define etm_base		147
#define etmCapHeight		(PFM_SHORT(etm_base + 14))
#define etmXHeight		(PFM_SHORT(etm_base + 16))
#define etmLowerCaseAscent	(PFM_SHORT(etm_base + 18))
#define etmLowerCaseDescent	(PFM_SHORT(etm_base + 20))
#define etmSlant		(PFM_SHORT(etm_base + 22))
#define etmUnderlineOffset	(PFM_SHORT(etm_base + 32))
#define etmUnderlineWidth	(PFM_SHORT(etm_base + 34))

#define dfDevice		199

/* Windows font descriptor flags */
#define PDF_FIXED_PITCH         0x01  /* Fixed width font; rarely used flag */

#define PDF_DONTCARE            0x00  /* Don't care or don't know. */
#define PDF_ROMAN               0x10  /* Variable stroke width, serifed */
#define PDF_SWISS               0x20  /* Variable stroke width, sans-serifed */
#define PDF_MODERN              0x30  /* fixed pitch */
#define PDF_SCRIPT              0x40  /* Cursive, etc. */
#define PDF_DECORATIVE          0x50  /* Old English, etc. */

/* Windows character set flags */
#define PFM_ANSI_CHARSET          0
#define PFM_SYMBOL_CHARSET        2
#define PFM_GREEK_CHARSET       161
#define PFM_TURKISH_CHARSET     162
#define PFM_VIETNAMESE_CHARSET  163
#define PFM_HEBREW_CHARSET      177
#define PFM_ARABIC_CHARSET      178
#define PFM_BALTIC_CHARSET      186
#define PFM_RUSSIAN_CHARSET     204
#define PFM_THAI_CHARSET        222
#define PFM_EASTEUROPE_CHARSET  238
#define PFM_OEM_CHARSET         255

static const pdc_keyconn pdf_charset_keylist[] =
{
    {"winansi", PFM_ANSI_CHARSET      },
    {"",        PFM_SYMBOL_CHARSET    },
    {"cp1253",  PFM_GREEK_CHARSET     },
    {"cp1254",  PFM_TURKISH_CHARSET   },
    {"cp1258",  PFM_VIETNAMESE_CHARSET},
    {"cp1255",  PFM_HEBREW_CHARSET    },
    {"cp1256",  PFM_ARABIC_CHARSET    },
    {"cp1257",  PFM_BALTIC_CHARSET    },
    {"cp1251",  PFM_RUSSIAN_CHARSET   },
    {"cp874",   PFM_THAI_CHARSET      },
    {"cp1250",  PFM_EASTEUROPE_CHARSET},
    {"",        PFM_OEM_CHARSET       },
    {NULL, 0},
};

#define PDF_STRING_PostScript	\
	((const char*) "\120\157\163\164\123\143\162\151\160\164")

/*
 *  Kerning pairs
 */
typedef struct kern_
{
    pdc_byte   first;           /* First character */
    pdc_byte   second;          /* Second character */
    pdc_byte   kern[2];         /* Kern distance */
}
KERN;

pdc_bool
pdf_check_pfm_encoding(PDF *p, pdf_font *font, pdc_encoding enc)
{
    const char *encname =
        pdc_errprintf(p->pdc, "%.*s", PDC_ERR_MAXSTRLEN,
                      pdf_get_encoding_name(p, enc, font));
    const char *intencname = NULL;
    pdc_encoding intenc = pdc_invalidenc;
    pdc_bool issymbfont = pdc_undef;

    pdc_logg_cond(p->pdc, 2, trc_font,
        "\tFont internal charset (dfCharSet): %d\n", font->ft.enc);

    /* Font encoding */
    intencname = pdc_get_keyword(font->ft.enc, pdf_charset_keylist);
    if (intencname == NULL)
    {
        pdc_set_errmsg(p->pdc, PDF_E_T1_BADCHARSET,
            pdc_errprintf(p->pdc, "%d", font->ft.enc), 0, 0, 0);
        return pdc_false;
    }

    if (strlen(intencname))
    {
        int codepage = 0;

        pdc_logg_cond(p->pdc, 2, trc_font,
            "\tFont internal encoding \"%s\" found\n", intencname);

        intenc = pdc_find_encoding(p->pdc, intencname);
        if (intenc == pdc_invalidenc)
            intenc = pdc_insert_encoding(p->pdc, intencname, &codepage,
                                         pdc_true);

        font->ft.issymbfont = pdc_false;
    }
    else
    {
        pdc_logg_cond(p->pdc, 2, trc_font, "\tSymbol font\n");

        font->ft.issymbfont = pdc_true;
        intenc = pdc_builtin;

        /* auto */
        if (!strcmp(font->encapiname, "auto"))
        {
            issymbfont = pdc_true;
            enc = pdc_builtin;
        }
    }

    /* builtin */
    if (enc == pdc_builtin)
        issymbfont = pdc_true;

    /* unicode */
    if (enc == pdc_unicode)
    {
        font->unibyte = pdc_true;
        issymbfont = pdc_false;
        enc = intenc;
    }

    /* encoding is subset of 8-bit encoding */
    if (enc >= pdc_winansi && intenc >= pdc_winansi)
    {
        if (pdc_is_encoding_subset(p->pdc, pdc_get_encoding_vector(p->pdc, enc),
                                   pdc_get_encoding_vector(p->pdc, intenc)))
        {
            if (enc != pdc_winansi && intenc == pdc_winansi &&
                strcmp(encname, "iso8859-1"))
            {
                font->towinansi = intenc;
            }
            else
            {
                enc = intenc;
            }
            issymbfont = pdc_false;
        }
    }

    /* illegal encoding */
    if (issymbfont == pdc_undef || font->ft.issymbfont == pdc_undef)
    {
        pdc_set_errmsg(p->pdc, PDF_E_FONT_BADENC, 0, 0, 0, 0);
        return pdc_false;
    }

    font->ft.enc = enc;
    if (issymbfont && !font->ft.issymbfont)
    {
        pdc_warning(p->pdc, PDF_E_FONT_FORCEENC,
                    pdf_get_encoding_name(p, intenc, NULL),
                    0, 0, 0);
        font->ft.enc = intenc;
    }
    if (!issymbfont && font->ft.issymbfont)
    {
        pdc_warning(p->pdc, PDF_E_FONT_FORCEENC,
                    pdf_get_encoding_name(p, pdc_builtin, NULL),
                    0, 0, 0);
        font->ft.enc = pdc_builtin;
        font->towinansi = pdc_invalidenc;
    }

    if (font->towinansi != pdc_invalidenc)
        pdf_transform_fontwidths(p, font,
                        pdc_get_encoding_vector(p->pdc, font->ft.enc),
                        pdc_get_encoding_vector(p->pdc, font->towinansi));

    return pdc_true;
}


/*
 * Currently we do not populate the following fields correctly:
 * - serif flag
 */

static pdc_bool
pdf_parse_pfm(PDF *p, pdc_file *fp, pdf_font *font)
{
    static const char fn[] = "pdf_parse_pfm";
    fnt_font_metric *ftm = &font->ft.m;
    size_t length;
    pdc_byte *pfm;
    pdc_bool ismem;
    int i, dfFirstChar, dfLastChar, default_width;
    unsigned long dfExtentTable;

    /* read whole file and close it */
    pfm = (pdc_byte *) pdc_freadall(fp, &length, &ismem);
    pdc_fclose(fp);

    /* check whether this is really a valid PostScript PFM file */
    if (pfm == NULL ||
	(header_dfVersion != 0x100 && header_dfVersion != 0x200) ||
	dfDevice > length ||
	strncmp((const char *) pfm + dfDevice, PDF_STRING_PostScript, 10) ||
	ext_dfDriverInfo > length)
    {
	if (!ismem)
	    pdc_free(p->pdc, pfm);
        return pdc_false;
    }

    /* fetch relevant data from the PFM */
    ftm->type = fnt_Type1;

    font->ft.name = pdc_strdup(p->pdc, (const char *)pfm + ext_dfDriverInfo);
    ftm->name = pdc_strdup(p->pdc, font->ft.name);

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\tPostScript font name: \"%s\"\n", ftm->name);

    switch (header_dfPitchAndFamily & 0xF0)
    {
	case PDF_ROMAN:
            ftm->flags |= FNT_SERIF;
	    break;
	case PDF_MODERN:
	    /* Has to be ignored, contrary to MS's specs */
	    break;
	case PDF_SCRIPT:
            ftm->flags |= FNT_SCRIPT;
	    break;
	case PDF_DECORATIVE:
	    /* the dfCharSet flag lies in this case... */
	    header_dfCharSet = PFM_SYMBOL_CHARSET;
	    break;
	case PDF_SWISS:
	case PDF_DONTCARE:
	default:
	    break;
    }

    /* temporarily */
    font->ft.enc = (pdc_encoding) header_dfCharSet;

    dfFirstChar = header_dfFirstChar;
    dfLastChar = header_dfLastChar;
    dfExtentTable = ext_dfExtentTable;

    /*
     * Some rare PFMs do not contain any ExtentTable if the fixed pitch flag
     * is set. Use the dfMaxWidth entry for all glyphs in this case.
     * If the user forced the font to be monospaced we use this value instead.
     */
    if ((!(header_dfPitchAndFamily & PDF_FIXED_PITCH) && dfExtentTable == 0) ||
        font->opt.monospace)
    {
        ftm->isFixedPitch = pdc_true;
        default_width = font->opt.monospace ? font->opt.monospace :
				(int) header_dfMaxWidth;
    }
    else
    {
	/* default values -- don't take the width of the default character */
        default_width = FNT_DEFAULT_WIDTH;
    }

    font->ft.numcodes = 256;
    ftm->numwidths = font->ft.numcodes;
    ftm->widths = (int *) pdc_calloc(p->pdc, ftm->numwidths * sizeof(int), fn);
    for (i = 0; i < font->ft.numcodes; i++)
        ftm->widths[i] = default_width;

    if (!ftm->isFixedPitch)
    {
	if (ext_dfExtentTable == 0 ||
	    ext_dfExtentTable + 2 * (header_dfLastChar-header_dfFirstChar) + 1 >
		length)
        {
	    if (!ismem)
		pdc_free(p->pdc, pfm);
	    return pdc_false;
	}

	for (i = dfFirstChar; i <= dfLastChar; i++)
            ftm->widths[i] =
                          (int) PFM_WORD(dfExtentTable + 2 * (i - dfFirstChar));
	/*
         * Check whether the font is actually opt.monospaced
	 * (the fixed pitch flag is not necessarily set)
	 */
        default_width = ftm->widths[dfFirstChar];

	for (i = dfFirstChar+1; i <= dfLastChar; i++)
            if (default_width != ftm->widths[i])
		break;

	if (i == dfLastChar + 1)
            ftm->isFixedPitch = pdc_true;
    }

    font->ft.weight = fnt_check_weight(header_dfWeight);
    ftm->defwidth = default_width;
    ftm->italicAngle = (header_dfItalic ? etmSlant/(10.0) : 0.0);
    ftm->capHeight = etmCapHeight;
    ftm->xHeight = etmXHeight;
    ftm->descender = -etmLowerCaseDescent;
    ftm->ascender = (int) header_dfAscent;

    ftm->underlinePosition = -etmUnderlineOffset;
    ftm->underlineThickness = etmUnderlineWidth;

    ftm->urx = header_dfMaxWidth;


    if (!ismem)
        pdc_free(p->pdc, pfm);

    return pdc_true;
}

pdc_bool
pdf_get_metrics_pfm(
    PDF *p,
    pdf_font *font,
    const char *fontname,
    pdc_encoding enc,
    const char *filename,
    pdc_bool requested)
{
    static const char fn[] = "pdf_get_metrics_pfm";
    char fullname[PDC_FILENAMELEN];
    pdc_file *pfmfile;

    (void) fontname;

    /* open PFM file */
    pfmfile = pdc_fsearch_fopen(p->pdc, filename, fullname, "PFM ",
                                PDC_FILE_BINARY);
    if (pfmfile == NULL)
        return pdc_check_fopen_errmsg(p->pdc, requested);

    pdc_logg_cond(p->pdc, 1, trc_font,
        "\tLoading PFM metric fontfile \"%s\":\n", fullname);

    /* Read PFM metrics */
    if (!pdf_parse_pfm(p, pfmfile, font))
    {
        pdc_set_errmsg(p->pdc, PDF_E_FONT_CORRUPT, "PFM", fullname, 0, 0);
        return pdc_false;
    }

    /* save full filename */
    font->metricfilename = pdc_strdup_ext(p->pdc, fullname, 0, fn);

    /* Check encoding */
    if (!pdf_check_pfm_encoding(p, font, enc))
        return pdc_false;

    if (!pdf_make_fontflag(p, font))
        return pdc_false;

    return pdc_true;
}
