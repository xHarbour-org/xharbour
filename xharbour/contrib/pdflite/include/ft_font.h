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
 * Header file for font handling
 *
 */

#ifndef FT_FONT_H
#define FT_FONT_H

#if defined(__BORLANDC__)
   #pragma warn -prc
   #pragma warn -pia
   #pragma warn -rch
   #pragma warn -csu
   #pragma warn -aus
   #pragma warn -ccc
   #pragma warn -def
   #pragma warn -sig
   #pragma warn -sus
   #pragma warn -use
   #pragma warn -eff
#endif

#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #define _CRT_SECURE_NO_WARNINGS
   #define _CRT_SECURE_NO_DEPRECATE
#endif

#include "pc_util.h"
#include "pc_geom.h"
#include "pc_file.h"
#include "ft_cid.h"

#define FNT_DEFAULT_WIDTH         250   /* some reasonable default */
#define FNT_DEFAULT_CIDWIDTH     1000   /* for CID fonts */
#define FNT_MISSING_WIDTH -1234567890   /* missing width value */

#define FNT_DEFAULT_UNDERLINEWIDTH 50   /* default value of underlineThickness*/

#define FNT_MAX_METRICS        2048.0   /* maximal font metrics value */

/*
 * these are the font weight values of Microsoft
 * see LOGFONT structure member lfWeight
 */
#define FNT_FW_DONTCARE     0
#define FNT_FW_THIN         100
#define FNT_FW_EXTRALIGHT   200
#define FNT_FW_ULTRALIGHT   200
#define FNT_FW_LIGHT        300
#define FNT_FW_NORMAL       400
#define FNT_FW_REGULAR      400
#define FNT_FW_MEDIUM       500
#define FNT_FW_SEMIBOLD     600
#define FNT_FW_DEMIBOLD     600
#define FNT_FW_BOLD         700
#define FNT_FW_EXTRABOLD    800
#define FNT_FW_ULTRABOLD    800
#define FNT_FW_HEAVY        900
#define FNT_FW_BLACK        900

/*
 * these defaults are used when the stem value
 * must be derived from the name (unused)
 */
#define FNT_STEMV_MIN        50     /* minimum StemV value */
#define FNT_STEMV_LIGHT      71     /* light StemV value */
#define FNT_STEMV_NORMAL    109     /* normal StemV value */
#define FNT_STEMV_MEDIUM    125     /* mediumbold StemV value */
#define FNT_STEMV_SEMIBOLD  135     /* semibold StemV value */
#define FNT_STEMV_BOLD      165     /* bold StemV value */
#define FNT_STEMV_EXTRABOLD 201     /* extrabold StemV value */
#define FNT_STEMV_BLACK     241     /* black StemV value */

/*
 * Bit positions for the font descriptor flag
 */
#define FNT_FIXEDWIDTH      (long) (1L<<0)
#define FNT_SERIF           (long) (1L<<1)
#define FNT_SYMBOL          (long) (1L<<2)
#define FNT_SCRIPT          (long) (1L<<3)
#define FNT_ADOBESTANDARD   (long) (1L<<5)
#define FNT_ITALIC          (long) (1L<<6)
#define FNT_SMALLCAPS       (long) (1L<<17)
#define FNT_FORCEBOLD       (long) (1L<<18)

#define FNT_FI_ITALIC        255
#define FNT_FI_ITALICNAME    "Italic"
#define FNT_DEF_ITALICANGLE  -12           /* default italic angle */

#define FNT_MISSING_FONTVAL  PDC_SHRT_MIN  /* missing font value */

/* start sequence of PFA files */
#define FNT_PFA_STARTSEQU   "%!PS"

/* missing file name for font outline data */
#define FNT_MISSING_FILENAME  "__missing__filename__"

/* Font types */
typedef enum
{
    fnt_Type0,          /* Type0 fonts */
    fnt_Type1,          /* Type1 fonts */
    fnt_MMType1,        /* Multiple master fonts */
    fnt_TrueType,       /* TrueType fonts for 1-byte encoding */
    fnt_CIDFontType2,   /* TrueType fonts for 2-byte encoding */
    fnt_Type1C,         /* CFF PostScript fonts for 1-byte encoding */
    fnt_CIDFontType0,   /* OpenType fonts with CFF_ table for 2-byte encoding */
    fnt_CIDFontType0C,  /* CFF PostScript fonts for 2-byte encoding */
    fnt_OpenType,       /* OpenType fonts for 1-byte encoding */
    fnt_OpenTypeC,      /* OpenType fonts for 2-byte encoding */
    fnt_Type3,          /* Type3 fonts */
    fnt_unknownType     /* for initialization only */
}
fnt_fonttype;

/* Font styles */
typedef enum
{
    fnt_Normal,
    fnt_Bold,
    fnt_Italic,
    fnt_BoldItalic
}
fnt_fontstyle;

typedef struct fnt_interwidth_s fnt_interwidth;
typedef struct fnt_interwidth4_s fnt_interwidth4;
typedef struct fnt_glyphwidth_s fnt_glyphwidth;
typedef struct fnt_font_metric_s fnt_font_metric;
typedef struct fnt_font_s fnt_font;

/* Code interval for glyph width */
struct fnt_interwidth_s
{
    pdc_ushort startcode;   /* start code of interval */
    pdc_short width;        /* width of glyphs in the code interval */
};

struct fnt_interwidth4_s
{
    int startcode;          /* start UTF-32 Unicode of interval */
    pdc_short width;        /* width of glyphs in the code interval */
};

/* Code and Unicode for glyph width */
struct fnt_glyphwidth_s
{
    pdc_ushort unicode;     /* UTF-16 Unicode of glyph */
    pdc_short code;         /* builtin 8-bit code */
    pdc_short width;        /* glyph width */
};


/* Font metric exchange structure */
struct fnt_font_metric_s
{
    char *name;                 /* font name (/FontName) */
    pdc_ulong flags;            /* font flags of font descriptor */
    fnt_fonttype type;          /* type of font */
    int charcoll;               /* supported CID character collection */
                                /* < 0: Halfwidth Latin-1 character */
    /* font metric */
    pdc_scalar italicAngle;     /* AFM key: ItalicAngle */
    int isFixedPitch;           /* AFM key: IsFixedPitch */
    pdc_scalar llx;             /* AFM key: FontBBox */
    pdc_scalar lly;             /* AFM key: FontBBox */
    pdc_scalar urx;             /* AFM key: FontBBox */
    pdc_scalar ury;             /* AFM key: FontBBox */
    int underlinePosition;      /* AFM key: UnderlinePosition */
    int underlineThickness;     /* AFM key: UnderlineThickness */
    int capHeight;              /* AFM key: CapHeight */
    int xHeight;                /* AFM key: XHeight */
    int ascender;               /* AFM key: Ascender */
    int descender;              /* AFM key: Descender */
    int StdVW;                  /* AFM key: StdVW */
    int StdHW;                  /* AFM key: StdHW */

    /* glyph widths */
    int defwidth;               /* default width */
    int numwidths;              /* number of entries in widths */
    int *widths;                /* ptr to glyph widths (enumerated by codes) */
    int numinters;              /* number of entries in ciw */
    fnt_interwidth *ciw;        /* ptr to code intervals for widths array */
    int numglwidths;            /* number of entries in glw */
    fnt_glyphwidth *glw;        /* ptr to glyph widths array */


};

/* Font exchange structure */
struct fnt_font_s
{
    char *name;                 /* font name (/BaseFont or /Name or 'font_#') */
    char *utf8name;             /* UTF-8 encoded font name (maybe with BOM) */
    char *filename;             /* font file name */

    fnt_font_metric m;          /* name, type, flags, charcoll and metric */
    pdc_bool isstdfont;         /* is an incore font
                                 * or standard CJK font in pdflib */
    pdc_bool ishostfont;        /* is an host font */
    pdc_bool hasdescr;          /* has font descriptor */
    pdc_bool vertical;          /* vertical writing mode */

    pdc_ushort spacechar;       /* code of space character depending on enc */
    int spacewidth;             /* width of space character */
    int linegap;                /* OpenType lineGap */
    int weight;                 /* font weight value 0-1000 */

    pdc_matrix matrix;          /* Type3 font matrix */
    pdc_rectangle bbox;         /* Type3 font bounding box */
    pdc_scalar fsscale;         /* Type3 fontsize scaling */

    pdc_bool issymbfont;        /* is a symbol font */
    pdc_encoding enc;           /* font encoding shortcut */

    int numglyphs;              /* number of glyphs */
    int numcodes;               /* number of codes */

    pdc_ushort *gid2code;       /* mapping glyph ID -> [Uni]code or NULL */
    pdc_ushort *code2gid;       /* mapping [Uni]code -> glyph ID or NULL */
    char *cmapname;             /* CID CMap name */


    /* font in memory */
    pdc_bool embedded;          /* embedded font */
    char *imgname;              /* name of virtual file containing *img */
    size_t filelen;             /* length of (uncompressed) font data */
    pdc_byte *img;              /* font (or CFF table) data */

};

/* font error numbers.
*/
enum
{
#define         fnt_genNames    1
#include        "ft_generr.h"

    FNT_E_dummy
};

/* ft_font.c */
void fnt_register_errtab(pdc_core *pdc);
void fnt_init_font(fnt_font *font);
void fnt_cleanup_font(pdc_core *pdc, fnt_font *font);
void fnt_cleanup_font_alien(pdc_core *pdc, fnt_font *font);
void fnt_cleanup_fontimg(pdc_core *pdc, fnt_font *font);
int fnt_get_glyphid(int code, fnt_font *font);
int fnt_get_code(int gid, fnt_font *font);
int fnt_get_glyphwidth(int code, fnt_font *font);
int fnt_get_pdf_fonttype_code(const char *typenam);
const char *fnt_get_pdf_fonttype_name(int typecode);
const char *fnt_get_pdf_fonttype_desc(int typecode);
pdc_encodingvector *fnt_create_font_ev(pdc_core *pdc, fnt_font *font);
int fnt_check_weight(int weight);
int fnt_weightname2weight(const char *weightname);
int fnt_stemv2weight(int stemv);
const char *fnt_weight2weightname(int weight);
int fnt_macfontstyle2weight(int macfontstyle);
int fnt_weight2stemv(int weight);
void fnt_font_logg_widths(pdc_core *pdc, fnt_font *font);
void fnt_font_logg_protocol(pdc_core *pdc, fnt_font *font);

/* ft_corefont.c */
pdc_bool fnt_is_standard_font(const char *fontname);
const char *fnt_get_abb_std_fontname(const char *fontname);
void fnt_fill_font_metric(pdc_core *pdc, fnt_font *font, pdc_bool kerning,
        const fnt_font_metric *metric);
const fnt_font_metric *fnt_get_core_metric(const char *fontname);
const char *fnt_get_abb_cjk_fontname(const char *fontname);
int fnt_get_preinstalled_cidfont(const char *fontname,
        const fnt_font_metric **fontmetric);
const char **fnt_get_cid_widths_array(pdc_core *pdc, fnt_font *font);


/* ft_type1.c */
pdc_bool fnt_test_type1_font(pdc_core *pdc, const pdc_byte *img);
pdc_bool fnt_get_type1_encoding(pdc_core *pdc, fnt_font *font, int glyphflags);

#endif  /* FT_FONT_H */
