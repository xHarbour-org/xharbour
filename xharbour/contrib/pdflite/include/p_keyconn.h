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
 * PDFlib shared keys connection lists
 *
 */

#ifndef P_KEYCONN_H
#define P_KEYCONN_H

/*
 * ------------- enumerations for pdc_keyconn tables ------------------
 */

typedef enum
{
    pdf_state_object       = (1<<0),    /* outside any document */
    pdf_state_document     = (1<<1),    /* document */
    pdf_state_page         = (1<<2),    /* page description in a document */
    pdf_state_pattern      = (1<<3),    /* pattern in a document */
    pdf_state_template     = (1<<4),    /* template in a document */
    pdf_state_path         = (1<<5),    /* path in a page description */
    pdf_state_font         = (1<<6),    /* font definition */
    pdf_state_glyph        = (1<<7),    /* glyph description in a Type3 font */
    pdf_state_glyphmetrics = (1<<8),    /* glyph metrics in a Type3 font */
    pdf_state_glyphignore  = (1<<9),    /* glyph ignored without error */
    pdf_state_error        = (1<<10)    /* in error cleanup */
}
pdf_state;

typedef enum
{
    errpol_legacy        = -1,
    errpol_return  = 0,
    errpol_exception     = 1
}
pdf_errpol;

typedef enum
{
    names_undef = 0,
    names_annots,           /* internal for named 3D or Movie annotations */
    names_dests,
    names_javascript,
    names_ap,
    names_embeddedfiles
}
pdf_nametree_type;

typedef enum
{
    event_formfield,
    event_annotation,
    event_bookmark,
    event_page,
    event_document
}
pdf_event_object;

typedef enum
{
    pdf_openaction,
    pdf_bookmark,
    pdf_remotelink,
    pdf_locallink,
    pdf_nameddest
}
pdf_destuse;

typedef enum
{
    pdf_3dview_first    = -1,
    pdf_3dview_last     = -2,
    pdf_3dview_next     = -3,
    pdf_3dview_previous = -4,
    pdf_3dview_default  = -5
}
pdf_3dviewoptions;

typedef enum
{
    pdf_none = 0,
    pdf_fill,
    pdf_stroke,
    pdf_fillstroke
}
pdf_drawmode;

typedef enum
{
    pdf_fill_winding,
    pdf_fill_evenodd
}
pdf_fillrule;

typedef enum
{
    NoColor = -1,
    DeviceGray = 0,
    DeviceRGB,
    DeviceCMYK,
    CalGray,
    CalRGB,
    Lab,
    ICCBased,
    Indexed,
    PatternCS,
    Separation,
    DeviceN
}
pdf_colorspacetype;

typedef enum
{
    color_undefgray = -1,
    color_none = 0,
    color_gray,
    color_rgb,
    color_cmyk,
    color_spotname,
    color_spot,
    color_pattern,
    color_iccbasedgray,
    color_iccbasedrgb,
    color_iccbasedcmyk,
    color_lab,

    color_max  /* for pdf_parse_coloropt */
}
pdf_colortype;

typedef enum
{
    AutoIntent = 0,
    AbsoluteColorimetric,
    RelativeColorimetric,
    Saturation,
    Perceptual
}
pdf_renderingintent;

/* only up to 32 values permitted! */
typedef enum
{
    fo_autocidfont,
    fo_autosubsetting,
    fo_embedding,
    fo_encoding,
    fo_fontname,
    fo_fontstyle,
    fo_fontwarning,
    fo_kerning,
    fo_monospace,
    fo_subsetlimit,
    fo_subsetminsize,
    fo_subsetting,
    fo_unicodemap,
    fo_embedopentype,
    fo_skipposttable,
    fo_vertical,
    fo_keepnative,
    fo_replacementchar,
    fo_ascender,
    fo_descender,
    fo_capheight,
    fo_xheight,
    fo_linegap
}
pdf_font_optflags;

/* only up to 32 values permitted! */
typedef enum
{
    to_charspacing,
    to_fillcolor,
    to_font,
    to_fontsize,
    to_fontsize_st,
    to_deffont,
    to_glyphwarning,
    to_horizscaling,
    to_italicangle,
    to_fakebold,
    to_kerning,
    to_overline,
    to_strikeout,
    to_strokecolor,
    to_strokewidth,
    to_dasharray,
    to_text,
    to_textformat,
    to_textrendering,
    to_textrise,
    to_leading,
    to_underline,
    to_wordspacing,
    to_underlinewidth,
    to_underlineposition,
    to_charref,
    to_escapesequence,
    to_glyphcheck,

    to_textx,
    to_texty
}
pdf_text_optflags;

typedef enum
{
    border_solid,
    border_dashed,
    border_beveled,
    border_inset,
    border_underline
}
pdf_borderstyle;

typedef enum
{
    label_none,
    label_123,
    label_IVX,
    label_ivx,
    label_ABC,
    label_abc
}
pdf_labelstyle;

typedef enum {
    BM_None             = 0,
    BM_Normal           = (1<<0),
    BM_Multiply         = (1<<1),
    BM_Screen           = (1<<2),
    BM_Overlay          = (1<<3),
    BM_Darken           = (1<<4),
    BM_Lighten          = (1<<5),
    BM_ColorDodge       = (1<<6),
    BM_ColorBurn        = (1<<7),
    BM_HardLight        = (1<<8),
    BM_SoftLight        = (1<<9),
    BM_Difference       = (1<<10),
    BM_Exclusion        = (1<<11),
    BM_Hue              = (1<<12),
    BM_Saturation       = (1<<13),
    BM_Color            = (1<<14),
    BM_Luminosity       = (1<<15)
}
pdf_blendmode;

/* these values are used directly as indices into
** a page's boxes[] array.
*/
typedef enum
{
    pdf_artbox,
    pdf_bleedbox,
    pdf_cropbox,
    pdf_mediabox,
    pdf_trimbox
} pdf_pagebox;

typedef enum
{
    tabs_none,
    tabs_fitbox,
    tabs_validarea
}
pdf_showtabs;

typedef enum
{
    text_noalign,
    text_left,
    text_center,
    text_right,
    text_justify,
    text_lastauto,
    text_fulljustify,
    text_decimal,
    text_top,
    text_bottom,
    text_grid
}
pdf_alignment;

typedef enum
{
    text_nofit,
    text_clip,
    text_shrink,
    text_split,
    text_spread,
    text_auto
}
pdf_adjustmethod;

typedef enum
{
    text_relative,
    text_typewriter,
    text_ruler
}
pdf_hortabmethod;

typedef enum
{
    text_none      = -90000,
    text_textrise  = -70000,
    text_xheight   = -60000,
    text_descender = -50000,
    text_capheight = -40000,
    text_ascender  = -30000,
    text_fontsize  = -20000,
    text_leading   = -10000
}
pdf_charmetric;

typedef enum
{
    mbox_none        = 0,
    mbox_openleft    = (1<<0),
    mbox_openright   = (1<<1),
    mbox_openbottom  = (1<<2),
    mbox_opentop     = (1<<3),
    mbox_border      = (1<<4),
    mbox_area        = (1<<5),
    mbox_saverestore = (1<<6),
    mbox_statleft    = (1<<7),
    mbox_statright   = (1<<8),
    mbox_statbottom  = (1<<9),
    mbox_stattop     = (1<<10)
}
pdf_mbox_flags;

typedef enum
{
    quadd_left   = 0,
    quadd_center = 1,
    quadd_right  = 2
}
pdf_quadding;

typedef enum
{
    disp_visible = (1<<2),
    disp_hidden  = (1<<1),
    disp_noview  = (1<<5),
    disp_noprint = 0
}
pdf_display;

typedef enum
{
    high_none,
    high_invert,
    high_outline,
    high_push
}
pdf_highlight;

typedef enum
{
    pos_left    = 1000,
    pos_bottom  = 2000,
    pos_center  =   50,
    pos_right   = 1100,
    pos_top     = 2100
}
pdf_position;

typedef enum
{
    dpi_none = -999999,
    dpi_internal = 0
}
pdf_dpi_states;

typedef enum
{
    trans_none,
    trans_split,
    trans_blinds,
    trans_box,
    trans_wipe,
    trans_dissolve,
    trans_glitter,
    trans_replace,

    TRANS_1_5,
    trans_fly = TRANS_1_5,
    trans_push,
    trans_cover,
    trans_uncover,
    trans_fade
}
pdf_transition;


/*
 * -------- pdc_keyconn tables shared by more than one c file ----------
 */

#if defined(P_MBOX_C)

static const pdc_keyconn pdf_mbox_keylist[] =
{
    {"all",  -1},
    {NULL, 0}
};

#endif /* P_MBOX_C */


#if defined(P_PAGE_C)

static const pdc_keyconn pdf_tgroup_cs_pdfkeylist[] =
{
    {"DeviceGray",   color_gray},
    {"DeviceRGB",    color_rgb},
    {"DeviceCMYK",   color_cmyk},
    {NULL, 0}
};

#endif /* P_PAGE_C || P_TEMPLATE_C */


#if defined(P_DOCUMENT_C) || defined(P_PARAMS_C)

static const pdc_keyconn pdf_compatibility_keylist[] =
{
    {"1.3", PDC_1_3},
    {"1.4", PDC_1_4},
    {"1.5", PDC_1_5},
    {"1.6", PDC_1_6},
    {"1.7", PDC_1_7},
    {NULL, 0}
};

#endif /* P_DOCUMENT_C || P_PARAMS_C */


#if defined(P_ACTIONS_C) || defined(P_PAGE_C)

static const pdc_keyconn pdf_transition_keylist[] =
{
    {"none",      trans_none},
    {"split",     trans_split},
    {"blinds",    trans_blinds},
    {"box",       trans_box},
    {"wipe",      trans_wipe},
    {"dissolve",  trans_dissolve},
    {"glitter",   trans_glitter},
    {"replace",   trans_replace},
    {"fly",       trans_fly},
    {"push",      trans_push},
    {"cover",     trans_cover},
    {"uncover",   trans_uncover},
    {"fade",      trans_fade},
    {NULL, 0}
};

static const pdc_keyconn pdf_transition_pdfkeylist[] =
{
    {"R",         trans_none},
    {"Split",     trans_split},
    {"Blinds",    trans_blinds},
    {"Box",       trans_box},
    {"Wipe",      trans_wipe},
    {"Dissolve",  trans_dissolve},
    {"Glitter",   trans_glitter},
    {"R",         trans_replace},
    {"Fly",       trans_fly},
    {"Push",      trans_push},
    {"Cover",     trans_cover},
    {"Uncover",   trans_uncover},
    {"Fade",      trans_fade},
    {NULL, 0}
};

#endif /* P_ACTIONS_C || P_PAGE_C */


#if defined(P_IMAGE_C) || defined(P_PARAMS_C) || defined(P_XGSTATE_C)

static const pdc_keyconn pdf_renderingintent_pdfkeylist[] =
{
    {"Auto",                 AutoIntent},
    {"AbsoluteColorimetric", AbsoluteColorimetric},
    {"RelativeColorimetric", RelativeColorimetric},
    {"Saturation",           Saturation},
    {"Perceptual",           Perceptual},
    {NULL, 0}
};

#endif /* P_IMAGE_C || P_PARAMS_C || P_XGSTATE_C */


#if defined(P_MBOX_C) || defined(P_TABLE_C) || defined(P_XGSTATE_C)

static const pdc_keyconn pdf_linecap_keylist[] =
{
    {"butt",       0},
    {"round",      1},
    {"projecting", 2},
    {NULL, 0}
};

static const pdc_keyconn pdf_linejoin_keylist[] =
{
    {"miter",   0},
    {"round",   1},
    {"bevel",   2},
    {NULL, 0}
};

#endif /* P_MBOX_C || P_TABLE_C || P_XGSTATE_C */


#if defined(P_PARAMS_C) || defined(P_TEXTFLOW_C)

static const pdc_keyconn pdf_fillrule_keylist[] =
{
    {"winding",   pdf_fill_winding },
    {"evenodd",   pdf_fill_evenodd },
    {NULL, 0}
};

#endif /* P_PARAMS_C || P_TEXTFLOW_C */


#if defined(P_DOCUMENT_C) || defined(P_PARAMS_C) || defined(P_PDI_C)

static const pdc_keyconn pdf_usebox_keylist[] =
{
    {"art",   pdc_pbox_art},
    {"bleed", pdc_pbox_bleed},
    {"crop",  pdc_pbox_crop},
    {"media", pdc_pbox_media},
    {"trim",  pdc_pbox_trim},
    {NULL, 0}
};

#endif /* P_DOCUMENT_C || P_PARAMS_C || P_PDI_C */

#if defined(P_DOCUMENT_C) || defined(P_PDI_C)

static const pdc_keyconn pdf_usebox_pdfkeylist[] =
{
    {"/ArtBox",   pdc_pbox_art   },
    {"/BleedBox", pdc_pbox_bleed },
    {"/CropBox",  pdc_pbox_crop  },
    {"/MediaBox", pdc_pbox_media },
    {"/TrimBox",  pdc_pbox_trim  },
    {NULL, 0}
};

#endif /* P_DOCUMENT_C || P_PDI_C */


#if defined(P_BLOCK_C) || defined(P_IMAGE_C)

static const pdc_keyconn pdf_dpi_keylist[] =
{
    {"none",     dpi_none},
    {"internal", dpi_internal},
    {NULL, 0}
};

#endif /* P_BLOCK_C || P_IMAGE_C */

#if defined(P_BLOCK_C) || defined(P_TEXT_C)

static const pdc_keyconn pdf_stampdir_keylist[] =
{
    {NULL, 0}
};

#endif /* P_BLOCK_C || P_TEXT_C */




#if defined(P_MBOX_C) || defined(P_TEXTFLOW_C)
static const pdc_keyconn pdf_boxheight_keylist[] =
{
    {"none",       text_none},
    {"baseline",   text_none},
    {"textrise",   text_textrise},
    {"xheight",    text_xheight},
    {"descender",  text_descender},
    {"capheight",  text_capheight},
    {"ascender",   text_ascender},
    {"fontsize",   text_fontsize},
    {"leading",    text_leading},
    {NULL, 0}
};

#endif /* P_MBOX_C || P_TEXTFLOW_C */


#if defined(P_BLOCK_C) || defined(P_TEXT_C) || defined(P_TEXTFLOW_C)

static const pdc_keyconn pdf_charname_keylist[] =
{
    {"none", 0},
    {NULL, 0}
};

#define PDF_UNDERLINEWIDTH_AUTO 0
static const pdc_keyconn pdf_underlinewidth_keylist[] =
{
    {"auto",       PDF_UNDERLINEWIDTH_AUTO},
    {NULL, 0}
};

#define PDF_UNDERLINEPOSITION_AUTO 1000000
static const pdc_keyconn pdf_underlineposition_keylist[] =
{
    {"auto",       PDF_UNDERLINEPOSITION_AUTO},
    {NULL, 0}
};

#endif /* P_BLOCK_C || P_TEXT_C || P_TEXTFLOW_C */


#if defined(P_BLOCK_C)|| \
    defined(P_TEXT_C) || defined(P_TEXTFLOW_C)

static const pdc_keyconn pdf_glyphcheck_keylist[] =
{
    {"none",         text_nocheck},
    {"error",        text_error},
    {"replace",      text_replace},
    {NULL, 0}
};

#endif /* P_BLOCK_C || P_PARAMS_C || P_TEXT_C || P_TEXTFLOW_C */


#if defined(P_ANNOTS_C) || defined(P_BLOCK_C) || \
    defined(P_IMAGE_C) || defined(P_TEXT_C)

static const pdc_keyconn pdf_position_keylist[] =
{
    {"left",    pos_left},
    {"bottom",  pos_bottom},
    {"center",  pos_center},
    {"right",   pos_right},
    {"top",     pos_top},
    {NULL, 0}
};

#endif /* P_ANNOTS_C || P_BLOCK_C || P_IMAGE_C || P_TEXT_C */


#if defined(P_BLOCK_C) || \
    defined(P_IMAGE_C) || defined(P_TABLE_C) || \
    defined(P_TEXT_C) || defined(P_TEXTFLOW_C)

static const pdc_keyconn pdf_fitmethod_keylist[] =
{
    {"nofit",       pdc_nofit},
    {"clip",        pdc_clip},
    {"auto",        pdc_tauto},
#if !defined (P_TEXTFLOW_C)
    {"slice",       pdc_slice},
    {"meet",        pdc_meet},
    {"entire",      pdc_entire},
#endif
    {NULL, 0}
};

#endif /* P_BLOCK_C || P_IMAGE_C || P_TABLE_C ||
          P_TEXT_C || P_TEXTFLOW_C */


#if defined(P_ANNOTS_C) || defined(P_BLOCK_C) || \
    defined(P_IMAGE_C) || defined(P_TEXT_C) || defined(P_TEXTFLOW_C)

static const pdc_keyconn pdf_orientate_keylist[] =
{
    {"north",   0},
    {"west",   90},
    {"south", 180},
    {"east",  270},
    {NULL, 0}
};

#endif /* P_ANNOTS_C || P_BLOCK_C || 
          P_IMAGE_C || P_TEXT_C || P_TEXTFLOW_C */


#if defined(P_ANNOTS_C) || defined(P_BLOCK_C) || \
    defined(P_MBOX_C) || defined(P_TEXT_C) || defined(P_TEXTFLOW_C)

static const pdc_keyconn pdf_fontsize_keylist[] =
{
    {"auto",       0},
    {"xheight",    text_xheight},
    {"capheight",  text_capheight},
    {"ascender",   text_ascender},
    {"bodyheight", text_fontsize},
    {NULL, 0}
};

#endif /* P_ANNOTS_C P_BLOCK_C ||
          P_MBOX_C || P_TEXT_C || P_TEXTFLOW_C */


#if defined(P_BLOCK_C) || defined(P_FONT_C) ||  defined(P_HYPER_C) || \
    defined(P_MBOX_C) || defined(P_TEXT_C) || defined(P_TEXTFLOW_C)

static const pdc_keyconn pdf_fontstyle_pdfkeylist[] =
{
    {"Normal",     fnt_Normal},
    {"Bold",       fnt_Bold},
    {"Italic",     fnt_Italic},
    {"BoldItalic", fnt_BoldItalic},
    {NULL, 0}
};

#endif /* P_BLOCK_C || P_FONT_C || P_HYPER_C ||
          P_MBOX_C || P_TEXT_C || P_TEXTFLOW_C */


#if defined(P_ANNOTS_C)

static const pdc_keyconn pdf_quadding_keylist[] =
{
    {"left",     quadd_left},
    {"center",   quadd_center},
    {"right",    quadd_right},
    {NULL, 0}
};

static const pdc_keyconn pdf_display_keylist[] =
{
    {"visible",   disp_visible},
    {"hidden",    disp_hidden},
    {"noview",    disp_noview},
    {"noprint",   disp_noprint},
    {NULL, 0}
};

static const pdc_keyconn pdf_highlight_keylist[] =
{
    {"none",      high_none},
    {"invert",    high_invert},
    {"outline",   high_outline},
    {"push",      high_push},
    {NULL, 0}
};

static const pdc_keyconn pdf_highlight_pdfkeylist[] =
{
    {"N",   high_none},
    {"I",   high_invert},
    {"O",   high_outline},
    {"P",   high_push},
    {NULL, 0}
};

static const pdc_keyconn pdf_borderstyle_keylist[] =
{
    {"solid",       border_solid},
    {"dashed",      border_dashed},
    {"beveled",     border_beveled},
    {"inset",       border_inset},
    {"underline",   border_underline},
    {NULL, 0}
};

static const pdc_keyconn pdf_borderstyle_pdfkeylist[] =
{
    {"S",  border_solid},
    {"D",  border_dashed},
    {"B",  border_beveled},
    {"I",  border_inset},
    {"U",  border_underline},
    {NULL, 0}
};

#endif /* P_ANNOTS_C */


#if defined(P_3D_C) || defined(P_BLOCK_C) || \
    defined(P_HYPER_C) || defined(P_LAYER_C) || defined(P_PARAMS_C) || \
    defined(P_TEXT_C) || defined(P_TEXTFLOW_C) || defined(P_UTIL_C) || \
    defined(P_XMP_C)

/* original in pc_unicode.h */
static const pdc_keyconn pdf_textformat_keylist[] =
{
    {"auto",       pdc_auto},
    {"auto2",      pdc_auto2},
    {"bytes",      pdc_bytes},
    {"bytes2",     pdc_bytes2},
    {"utf8",       pdc_utf8},
    {"utf16",      pdc_utf16},
    {"utf16be",    pdc_utf16be},
    {"utf16le",    pdc_utf16le},
    {NULL, 0}
};

#endif /* P_3D_C || P_BLOCK_C || P_HYPER_C ||
          P_LAYER_C || P_PARAMS_C || P_TEXT_C || P_TEXTFLOW_C ||
          P_UTIL_C || P_XMP_C */


#if defined(P_DOCUMENT_C) || \
    defined(P_3D_C) || \
    defined(P_ACTIONS_C) || \
    defined(P_BLOCK_C) || \
    defined(P_FONT_C) || \
    defined(P_ICC_C) || \
    defined(P_IMAGE_C) || \
    defined(P_PARAMS_C) || \
    defined(P_PDI_C) || \
    defined(P_TABLE_C) || \
    defined(P_TEMPLATE_C) || \
    defined(P_TEXT_C) || \
    defined(P_TEXTFLOW_C)

static const pdc_keyconn pdf_errpol_keylist[] =
{
    {"legacy",        errpol_legacy},
    {"return",        errpol_return},
    {"exception",     errpol_exception},
    {NULL, 0}
};

#define PDF_ERRORPOLICY_OPTION \
\
    {"errorpolicy", pdc_keywordlist, PDC_OPT_NONE, 1, 1, \
      0, 0, pdf_errpol_keylist}, \

#endif


#endif  /* P_KEYCONN_H */

