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
 * Header file for the PDFlib font subsystem
 *
 */

#ifndef P_FONT_H
#define P_FONT_H

#define PDF_DEFAULT_CHAR PDC_UNICODE_SPACE

/* internal maximal length of fontnames */
#define PDF_MAX_FONTNAME        128

/* last text rendering mode number */
#define  PDF_LAST_TRMODE  7

/* minimal number of glyphs for appropriate encoding */
#define  PDF_MIN_GLYPHS  5

/* sign for vertical writing mode on Windows */
#define  PDF_VERTICAL_SIGN  '@'

typedef enum
{
    font_ascender  = (1<<0),
    font_descender = (1<<1),
    font_capheight = (1<<2),
    font_xheight   = (1<<3),
    font_linegap   = (1<<4),

    font_italic    = (1<<8),
    font_bold      = (1<<9)
}
pdf_font_values;

typedef struct pdf_t3glyph_s pdf_t3glyph;

/* font options */
struct pdf_font_options_s
{
    pdc_bool embedding;
    char *encoding;
    int flags;
    char *fontname;
    fnt_fontstyle fontstyle;
    pdc_bool fontwarning;
    int mask;
    int monospace;
    int ascender;
    int descender;
    int capheight;
    int xheight;
    int linegap;
    pdc_bool auxiliary;
    pdc_bool dropcorewidths;
};

/* Type3 font structures */
struct pdf_t3glyph_s
{
    char *name;
    pdc_id charproc_id;
    pdc_scalar wx;
    pdc_scalar llx;
    pdc_scalar lly;
    pdc_scalar urx;
    pdc_scalar ury;
    pdc_scalar width;
    int pass;                   /* 0, 1, 2 */
};

struct pdf_t3font_s
{
    pdf_t3glyph *glyphs;        /* dynamically growing glyph table */
    int capacity;               /* current number of slots */
    int next_glyph;             /* next available slot */
    int curr_glyph;             /* slot of current glyph */

    pdc_id charprocs_id;        /* id of /CharProcs dict */
    pdc_id res_id;              /* id of /Resources dict */
    pdc_bool colorized;         /* glyphs colorized */
    int pass;                   /* 0, 1, 2 */

};

/* pdflib font structure */
struct pdf_font_s
{
    /* pdcore font structure */
    fnt_font ft;

    /* font options */
    pdf_font_options opt;       /* pdflib font options */
    pdc_bool verbose;           /* put out warning/error messages */

    /* special font names */
    char *apiname;              /* font name specified in API call */
    const char *filename;       /* name of font file, copy of ft.filename */
    char *metricfilename;       /* name of metric font file */

    /* font control */
    pdc_bool used_in_formfield;    /* this font is in use in form field */
    pdc_bool used_in_current_doc;  /* this font is in use in current doc. */
    pdc_bool used_on_current_page; /* this font is in use on current page */
    pdc_id obj_id;                 /* object id of this font */

    /* CFF table */
    long cff_offset;            /* start of CFF table in font */
    size_t cff_length;          /* length of CFF table in font */

    /* Type3 font */
    pdf_t3font *t3font;         /* Type3 font data */
    pdc_bool hasoriginal;       /* has the original Type3 font data */

    /* pdflib encoding and CMap properties */
    char *encapiname;           /* encoding name specified in API call */
    char *outcmapname;          /* output CMap name */
    int codepage;               /* OEM multi byte code-page number */
    pdc_encoding towinansi;     /* convert to 'towinansi' enc. for output */
    pdc_bool hasnomac;          /* TT font has no macroman cmap */
    pdc_bool passthrough;       /* text will be passed through as is */
    pdc_bool unibyte;           /* take Unicode encoding as byte encoding */
    pdc_bool asciispace;        /* encoding has space at x20 */
    pdc_bool issemantic;        /* encoding is Unicode interpretable */
    pdc_bool widthsmissing;     /* GID widths not available */
    pdc_ulong missingglyphs;    /* bit mask for missing new AGL glyphs */
    int metricflags;            /* flags for faked font values */
    int supplement;             /* supplement number of CMap
                                 * = -1: Identity-H/V */
    pdc_encoding symenc;        /* font encoding for symbol fonts */
    int replacementchar;        /* replacement character */
    int replacementcode;        /* replacement code or glyph id resp. */

    /* encoding and glyph control */
    int codesize;               /* code size */
                                /* = 0: unknown, no Unicode CMap */
                                /* = 1: 1 byte encoding */
                                /* = 2: 2 byte encoding */
    int lastcode;               /* AFM: last byte code for generating runtime */
                                /* byte encoding. = -1: ignore */
    int gid0code;               /* code für gid 0 (because of Type3 fonts) */
    pdc_byte *usedgids;         /* used Glyph IDs for font subsetting */
    pdc_bool expectglyphs;      /* TT: glyph id text strings are expected */
    pdc_bool iscidfont;         /* is CID font */

    int *widths;                /* temporary glyph widths: code/gid -> width */
    int numwidths;              /* number of entries in widths */
    pdc_bool konlydef;          /* only default widths */

};

/* p_truetype.c */
pdc_bool        pdf_get_metrics_tt(PDF *p, pdf_font *font,
                    const char *fontname, pdc_encoding enc,
                    const char *filename);
int             pdf_check_tt_hostfont(PDF *p, const char *hostname);

/* p_afm.c */
pdc_bool        pdf_process_metrics_data(PDF *p, pdf_font *font,
                    const char *fontname);
pdc_bool        pdf_get_metrics_afm(PDF *p, pdf_font *font,
                    const char *fontname, pdc_encoding enc,
                    const char *filename, pdc_bool requested);

/* p_pfm.c */
pdc_bool        pdf_check_pfm_encoding(PDF *p, pdf_font *font,
                       pdc_encoding enc);
pdc_bool        pdf_get_metrics_pfm(PDF *p, pdf_font *font,
                    const char *fontname, pdc_encoding enc,
                    const char *filename, pdc_bool requested);

/* p_cid.c */
pdc_bool pdf_handle_cidfont(PDF *p, const char *fontname,
        const char *encoding, pdc_encoding enc, pdf_font *font, int *o_slot,
        pdc_encoding *newenc);
void pdf_put_cidglyph_widths(PDF *p, pdf_font *font);

/* p_font.c */
void pdf_get_page_fonts(PDF *p, pdf_reslist *rl);
void pdf_parse_font_options(PDF *p, const char *optlist);
double pdf_get_font_float_option(PDF *p, pdf_font_optflags fflags);
pdc_bool pdf_check_font_embedding(PDF *p, pdf_font *font, const char *fontname);
pdc_bool pdf_make_fontflag(PDF *p, pdf_font *font);
int pdf_get_code_or_glyphid(PDF *p, pdf_font *font, pdc_encodingvector *ev,
        pdc_ushort uv);
void pdf_set_replchar(PDF *p, pdf_font *font);
void pdf_font_issemantic(PDF *p, pdf_font *font);
void pdf_font_set_missvalues(PDF *p, pdf_font *font);
pdc_bool pdf_font_get_is_faked(pdf_font *font, pdf_font_values flag);
double pdf_font_get_metric_value(int value);
const char *pdf_get_encoding_name(PDF *p, pdc_encoding enc, pdf_font *font);
const char *pdf_get_font_char_option(PDF *p, pdf_font_optflags fflags);
const char *pdf_get_pdf_fontname(pdf_font *font);
char *pdf_get_encoding_adaptname(PDF *p, pdc_encoding enc, pdf_font *font,
        const char *fontname);
pdc_encodingvector *pdf_create_font_encoding(PDF *p, pdc_encoding enc,
        pdf_font *font, const char *fontname, pdc_bool kreg);
void pdf_transform_fontwidths(PDF *p, pdf_font *font,
        pdc_encodingvector *evto, pdc_encodingvector *evfrom);
void pdf_prepare_fontwidths(PDF *p, pdf_font *font, int nusedgids);

/* p_type1.c */
pdc_bool pdf_t1open_fontfile(PDF *p, pdf_font *font, const char *fontname,
                             PDF_data_source *t1src, pdc_bool requested);
pdc_bool pdf_make_t1src(PDF *p, pdf_font *font, PDF_data_source *t1src);
void     pdf_put_length_objs(PDF *p, PDF_data_source *t1src,
		     pdc_id length1_id, pdc_id length2_id, pdc_id length3_id);

/* p_type3.c */
void pdf_cleanup_t3font(PDF *p, pdf_t3font *t3font);
void pdf_init_type3(PDF *p);
pdc_bool pdf_handle_t3font(PDF *p, const char *fontname, pdc_encoding enc,
                      pdf_font *font, int *slot);
pdc_bool pdf_isvalid_font(PDF *p, int slot);

/* p_subsett.c */
int pdf_prepare_ttfont(PDF *p, pdf_font *font);


#endif  /* P_FONT_H */

