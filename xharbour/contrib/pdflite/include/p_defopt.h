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
 * PDFlib shared option definitions and structures
 *
 */

#ifndef P_DEFOPT_H
#define P_DEFOPT_H

#define PDF_KEEP_TEXTLEN    (1L<<0)    /* keep text length */
#define PDF_KEEP_CONTROL    (1L<<1)    /* keep special control character */
#define PDF_KEEP_UNICODE    (1L<<2)    /* keep Unicode text */
#define PDF_FORCE_NEWALLOC  (1L<<3)    /* force alloc for new text */
#define PDF_USE_TMPALLOC    (1L<<4)    /* use temporary memory */

#define PDF_RETURN_BOXEMPTY  "_boxempty"
#define PDF_RETURN_BOXFULL   "_boxfull"
#define PDF_RETURN_NEXTPAGE  "_nextpage"
#define PDF_RETURN_STOP      "_stop"
#define PDF_RETURN_ATMARK    "_mark%d"

typedef enum
{
    is_block    = (1L<<0),
    is_image    = (1L<<1),
    is_textline = (1L<<2),
    is_textflow = (1L<<3),
    is_inline   = (1L<<4)
}
pdf_elemflags;

struct pdf_text_options_s
{
    pdc_scalar charspacing;
    pdc_scalar charspacing_pc;
    pdf_coloropt fillcolor;
    int font;
    pdc_scalar fontsize;
    pdc_scalar fontsize_pc;
    int fontsize_st;
    pdc_bool glyphwarning;
    pdc_scalar horizscaling;
    pdc_scalar italicangle;
    pdc_bool fakebold;
    pdc_bool kerning;
    unsigned int mask;
    unsigned int pcmask;
    unsigned int fontset;
    pdc_bool overline;
    pdc_bool strikeout;
    pdf_coloropt strokecolor;
    pdc_scalar strokewidth;
    pdc_scalar dasharray[2];
    char *text;
    int textlen;
    pdc_text_format textformat;
    int textrendering;
    pdc_scalar textrise;
    pdc_scalar textrise_pc;
    pdc_scalar leading;
    pdc_scalar leading_pc;
    pdc_bool underline;
    pdc_scalar wordspacing;
    pdc_scalar wordspacing_pc;
    pdc_scalar underlinewidth;
    pdc_scalar underlineposition;
    pdc_scalar *xadvancelist;
    int nglyphs;
    char *link;
    char *linktype;
    pdc_bool charref;
    pdc_bool escapesequence;
    pdc_glyphcheck glyphcheck;
};

typedef enum
{
    xo_filename,
    xo_ignoreorientation,
    xo_imagewarning,
    xo_dpi,
    xo_page,
    xo_scale
}
pdf_xobject_optflags;

typedef struct
{
    pdc_bool adjustpage;
    pdc_bool blind;
    char *filename;
    int flags;
    pdc_bool imagewarning;
    pdc_bool ignoreorientation;
    unsigned int mask;
    int im;
    int page;
    pdc_scalar dpi[2];
    pdc_scalar scale[2];
}
pdf_xobject_options;

typedef enum
{
    fit_boxsize,
    fit_fitmethod,
    fit_margin,
    fit_shrinklimit,
    fit_position,
    fit_orientate,
    fit_rotate,
    fit_matchbox,
    fit_alignchar,
    fit_refpoint
}
pdf_fit_optflags;


typedef struct
{
    pdc_scalar boxsize[2];
    pdc_fitmethod fitmethod;
    int flags;
    pdc_scalar margin[2];
    unsigned int mask;
    unsigned int pcmask;
    pdc_scalar shrinklimit;
    pdc_scalar position[2];
    int orientate;
    pdc_scalar refpoint[2];
    pdc_scalar rotate;
    pdc_bool showborder;
    pdf_mbox *matchbox;
    pdc_ushort alignchar;
}
pdf_fit_options;

typedef struct pdf_fitres_s pdf_fitres;


/* font option definitions */

#define PDF_KERNING_FLAG PDC_OPT_UNSUPP
#define PDF_SUBSETTING_FLAG PDC_OPT_UNSUPP
#define PDF_AUTOCIDFONT_FLAG PDC_OPT_UNSUPP
#define PDF_EMBEDOPENTYPE_FLAG PDC_OPT_UNSUPP
#define PDF_SKIPPOSTTABLE_FLAG PDC_OPT_UNSUPP
#define PDF_CHARREF_FLAG PDC_OPT_UNSUPP
#define PDF_ESCAPESEQU_FLAG PDC_OPT_UNSUPP
#define PDF_GLYPHCHECK_FLAG PDC_OPT_UNSUPP
#define PDF_VERTICAL_FLAG PDC_OPT_UNSUPP
#define PDF_REPLCHAR_FLAG PDC_OPT_UNSUPP
#define PDF_KEEPNATIVE_FLAG PDC_OPT_UNSUPP
#define PDF_STAMP_FLAG PDC_OPT_UNSUPP
#define PDF_LEADER_FLAG PDC_OPT_UNSUPP

#define PDF_METADATA_FLAG  PDC_OPT_UNSUPP

#define PDF_CLIPPATH_FLAG PDC_OPT_UNSUPP

#define PDF_FONT_OPTIONS1 \
\
    {"encoding", pdc_stringlist,  PDC_OPT_NONE, 1, 1, \
      0.0, PDF_MAX_NAMESTRING, NULL}, \
\
    {"fontname", pdc_stringlist, PDC_OPT_NONE | PDC_OPT_CONVUTF8, 1, 1, \
      1.0, PDF_MAX_FONTNAME, NULL}, \


#define PDF_FONT_OPTIONS2 \
\
    {"autocidfont", pdc_booleanlist, PDF_AUTOCIDFONT_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"autosubsetting", pdc_booleanlist, PDF_SUBSETTING_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"embedding", pdc_booleanlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"fontstyle", pdc_keywordlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, pdf_fontstyle_pdfkeylist}, \
\
    /* deprecated */ \
    {"fontwarning", pdc_booleanlist, PDC_OPT_PDFLIB_7, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"monospace", pdc_integerlist, PDC_OPT_NONE, 1, 1, \
      1.0, FNT_MAX_METRICS, NULL}, \
\
    {"ascender", pdc_integerlist, PDC_OPT_NONE, 1, 1, \
      -FNT_MAX_METRICS, FNT_MAX_METRICS, NULL}, \
\
    {"descender", pdc_integerlist, PDC_OPT_NONE, 1, 1, \
      -FNT_MAX_METRICS, FNT_MAX_METRICS, NULL}, \
\
    {"capheight", pdc_integerlist, PDC_OPT_NONE, 1, 1, \
      -FNT_MAX_METRICS, FNT_MAX_METRICS, NULL}, \
\
    {"xheight", pdc_integerlist, PDC_OPT_NONE, 1, 1, \
      -FNT_MAX_METRICS, FNT_MAX_METRICS, NULL}, \
\
    {"linegap", pdc_integerlist, PDC_OPT_NONE, 1, 1, \
      -FNT_MAX_METRICS, FNT_MAX_METRICS, NULL}, \
\
    {"subsetlimit", pdc_doublelist, PDF_SUBSETTING_FLAG|PDC_OPT_PERCENT, 1, 1, \
      0.0, 100.0, NULL}, \
\
    {"subsetminsize", pdc_doublelist, PDF_SUBSETTING_FLAG, 1, 1, \
      0.0, PDC_FLOAT_MAX, NULL}, \
\
    {"subsetting", pdc_booleanlist, PDF_SUBSETTING_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"unicodemap", pdc_booleanlist, PDF_AUTOCIDFONT_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"embedopentype", pdc_booleanlist, PDF_EMBEDOPENTYPE_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"skipposttable", pdc_booleanlist, PDF_SKIPPOSTTABLE_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"dropcorewidths", pdc_booleanlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"keepnative", pdc_booleanlist, PDF_KEEPNATIVE_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"vertical", pdc_booleanlist, PDF_VERTICAL_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"replacementchar", pdc_unicharlist, PDF_REPLCHAR_FLAG, 1, 1, \
      0.0, PDC_MAX_UNIVAL, NULL}, \
\
    {"metadata", pdc_stringlist, PDF_METADATA_FLAG, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \


#define PDF_FONT_OPTIONS3 \
\
    {"kerning", pdc_booleanlist, PDF_KERNING_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \


/* text option definitions */

#define PDF_TEXT_OPTIONS \
\
    {"charspacing", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL}, \
\
    /* deprecated */ \
    {"glyphwarning", pdc_booleanlist, PDC_OPT_PDFLIB_7, 1, 1, \
     0.0, 0.0, NULL}, \
\
    {"fillcolor", pdc_stringlist, PDC_OPT_NONE, 1, 5, \
      0.0, PDF_MAX_NAMESTRING, NULL}, \
\
    {"font", pdc_fonthandle, PDC_OPT_NONE, 1, 1, \
      0, 0, NULL}, \
\
    {"fontsize", pdc_scalarlist, \
      PDC_OPT_PERCENT | PDC_OPT_SUBOPTLIST | PDC_OPT_KEYLIST1, 1, 2, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, pdf_fontsize_keylist}, \
\
    {"horizscaling", pdc_scalarlist,  PDC_OPT_PERCENT, 1, 1, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL}, \
\
    {"italicangle", pdc_scalarlist,  PDC_OPT_NONE, 1, 1, \
      -89.99, 89.99, NULL}, \
\
    {"fakebold", pdc_booleanlist,  PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"kerning", pdc_booleanlist, PDF_KERNING_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"overline", pdc_booleanlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"strikeout", pdc_booleanlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"strokecolor", pdc_stringlist, PDC_OPT_NONE, 1, 5, \
      0.0, PDF_MAX_NAMESTRING, NULL}, \
\
    {"strokewidth", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1, \
      0.0, PDC_FLOAT_MAX, pdf_underlinewidth_keylist}, \
\
    {"dasharray", pdc_scalarlist, PDC_OPT_NONE, 1, 2, \
      0.0, PDC_FLOAT_MAX, NULL}, \
\
    {"textformat", pdc_keywordlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, pdf_textformat_keylist}, \
\
    {"textrendering", pdc_integerlist, PDC_OPT_NONE, 1, 1, \
      0, PDF_LAST_TRMODE, NULL}, \
\
    {"textrise", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL}, \
\
    {"underline", pdc_booleanlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"wordspacing", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL}, \
\
    {"underlinewidth", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1, \
      0.0, PDC_FLOAT_MAX, pdf_underlinewidth_keylist}, \
\
    {"underlineposition", pdc_scalarlist, PDC_OPT_PERCENT, 1, 1, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, pdf_underlineposition_keylist}, \
\
    /* deprecated */ \
    {"weblink", pdc_stringlist, PDC_OPT_PDFLIB_7, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    /* deprecated */ \
    {"locallink", pdc_stringlist, PDC_OPT_PDFLIB_7, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    /* deprecated */ \
    {"pdflink", pdc_stringlist, PDC_OPT_PDFLIB_7, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \
\
    {"charref", pdc_booleanlist, PDF_CHARREF_FLAG, 1, 1, \
     0.0, 0.0, NULL}, \
\
    {"escapesequence", pdc_booleanlist, PDF_ESCAPESEQU_FLAG, 1, 1, \
     0.0, 0.0, NULL}, \
\
    {"glyphcheck", pdc_keywordlist, PDF_GLYPHCHECK_FLAG, 1, 1, \
     0.0, 0.0, pdf_glyphcheck_keylist}, \
\


/* xobject option definitions */

#define PDF_XOBJECT_OPTIONS1 \
\
    {"adjustpage", pdc_booleanlist, PDC_OPT_PDC_1_3, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"blind", pdc_booleanlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \


#define PDF_XOBJECT_OPTIONS2 \
\
    {"ignoreorientation", pdc_booleanlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"ignoreclippingpath", pdc_booleanlist, PDF_CLIPPATH_FLAG, 1, 1, \
      0.0, 0.0, NULL}, \
\
    /* deprecated */ \
    {"imagewarning", pdc_booleanlist, PDC_OPT_PDFLIB_7, 1, 1, \
      0.0, 0.0, NULL}, \
\
    {"dpi", pdc_scalarlist, PDC_OPT_NONE, 1, 2, \
      0.0, PDC_INT_MAX, pdf_dpi_keylist}, \


#define PDF_XOBJECT_OPTIONS3 \
\
    {"scale", pdc_scalarlist, PDC_OPT_NOZERO, 1, 2, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL}, \


/* general fit option definitions  */

#define PDF_FIT_OPTIONS1 \
\
    {"boxsize", pdc_scalarlist, PDC_OPT_NONE, 2, 2, \
      0, PDC_FLOAT_MAX, NULL}, \
\
    {"margin", pdc_scalarlist, PDC_OPT_NONE, 1, 2, \
      0, PDC_FLOAT_MAX, NULL}, \
\
    {"shrinklimit", pdc_scalarlist, PDC_OPT_PERCENT | PDC_OPT_PERCRANGE, 1, 1, \
      0.0, 100.0, NULL}, \
\
    {"position", pdc_scalarlist, PDC_OPT_NONE, 1, 2, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, pdf_position_keylist}, \
\
    {"matchbox", pdc_stringlist, PDC_OPT_NONE, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \


#define PDF_FIT_OPTIONS2 \
\
    {"fitmethod", pdc_keywordlist, PDC_OPT_NONE, 1, 1,  \
      0.0, 0.0, pdf_fitmethod_keylist}, \
\
    {"rotate", pdc_scalarlist, PDC_OPT_NONE, 1, 1, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL}, \
\
    {"orientate", pdc_keywordlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, pdf_orientate_keylist}, \
\
    {"showborder", pdc_booleanlist, PDC_OPT_NONE, 1, 1, \
      0.0, 0.0, NULL}, \


#define PDF_FIT_OPTIONS6 \
\
    {"alignchar", pdc_unicharlist, PDC_OPT_NONE, 1, 1, \
      0.0, PDC_USHRT_MAX, pdf_charname_keylist}, \
\
    {"stamp", pdc_keywordlist, PDF_STAMP_FLAG, 1, 1, \
      0.0, 0.0, pdf_stampdir_keylist}, \
\
    {"leader", pdc_stringlist, PDF_LEADER_FLAG, 1, 1, \
      0.0, PDC_INT_MAX, NULL}, \


#define PDF_FIT_OPTIONS3 \
\
    {"refpoint", pdc_scalarlist, PDC_OPT_NONE, 2, 2, \
      PDC_FLOAT_MIN, PDC_FLOAT_MAX, NULL}, \



/* p_font.c */
void pdf_get_font_options(PDF *p, pdf_font_options *fo, pdc_resopt *resopts);
int pdf_load_font_internal(PDF *p, pdf_font_options *fo);

/* p_image.c */
void pdf_init_xobject_options(PDF *p, pdf_xobject_options *xo);
void pdf_get_xobject_options(PDF *p, pdf_xobject_options *xo,
        pdc_resopt *resopts);
pdc_resopt *pdf_parse_fitxobject_optlist(PDF *p, int im,
        pdf_xobject_options *xo, pdf_fit_options *fit, const char *optlist);
void pdf_fit_xobject_internal(PDF *p, pdf_xobject_options *xo,
        pdf_fit_options *fit, pdc_matrix *immatrix);

/* p_mbox.c */
void pdf_init_fit_options(PDF *p, pdc_bool fortflow, pdf_fit_options *fit);
void pdf_cleanup_fit_options(PDF *p, pdf_fit_options *fit);
void pdf_get_fit_options(PDF *p, pdc_bool fortflow, pdf_fit_options *fit,
        pdc_resopt *resopts);
void pdf_get_mbox_boxheight(PDF *p, pdf_mbox *mbox,
        pdc_scalar *boxheight);
pdc_bool pdf_get_mbox_clipping(PDF *p, pdf_mbox *mbox,
        pdc_scalar width, pdc_scalar height, pdc_box *clipbox);
double pdf_get_mbox_info(PDF *p, pdf_mbox *mbox, const char *keyword);

/* p_text.c */
pdc_bool pdf_calculate_text_options(PDF *p, pdf_text_options *to,
        pdc_bool force, pdc_scalar fontscale, pdc_scalar minfontsize,
        pdc_scalar fontsizeref);
void pdf_set_text_options(PDF *p, pdf_text_options *to);
void pdf_init_text_options(PDF *p, pdf_text_options *to);
void pdf_get_text_options(PDF *p, pdf_text_options *to, pdc_resopt *resopts);
pdc_resopt *pdf_parse_fittextline_optlist(PDF *p, pdf_text_options *to,
        pdf_fit_options *fit, const char *optlist);
pdc_bool pdf_fit_textline_internal(PDF *p, pdf_fitres *fitres,
        pdf_text_options *to, pdf_fit_options *fit, pdc_matrix *matrix);
void pdf_calculate_textline_size(PDF *p, pdf_text_options *to,
        pdf_fit_options *fit, pdc_scalar *width, pdc_scalar *height);
pdc_bool pdf_is_horiz_orientated(pdf_fit_options *fit);
int pdf_calculate_leader_pos(PDF *p, pdf_alignment alignment,
        pdf_text_options *to, int nchars,pdc_scalar *xstart, pdc_scalar *xstop,
        pdc_scalar width, pdc_bool left);
void pdf_draw_leader_text(PDF *p, pdc_scalar xstart, pdc_scalar ybase,
        pdc_scalar width, int nchars, pdc_byte *utext, int len, int charlen,
        pdf_text_options *to);

int pdf_get_approximate_uvlist(PDF *p, pdf_font *currfont,
        pdc_encodingvector *ev, int usv, pdc_bool replace, pdf_fitres *fitres,
        pdc_ushort *uvlist, pdc_ushort *cglist);
void pdf_get_input_textformat(pdf_font *currfont,
        pdc_text_format *intextformat, int *convflags);
pdc_bool pdf_check_textstring(PDF *p, const char *text, int len, int flags,
        pdf_text_options *to, pdf_fitres *fittext, pdc_byte **outtext,
        int *outlen, int *outcharlen, pdc_bool verbose);
pdc_scalar pdf_calculate_textsize(PDF *p, const pdc_byte *text, int len,
        int charlen, pdf_text_options *to, int breakchar, pdc_scalar *height,
        pdc_bool verbose);
pdc_scalar pdf_trim_textwidth(pdc_scalar width, pdf_text_options *to);
void pdf_place_text(PDF *p, pdc_byte *text, int len, int charlen,
        pdf_text_options *to, pdc_scalar width, pdc_scalar height,
        pdc_bool cont);




#endif  /* P_DEFOPT_H */

