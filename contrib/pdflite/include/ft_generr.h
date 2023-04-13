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
 * FONT error messages
 *
 */

#define FT_GENERR_H

#if     fnt_genNames
#define gen(n, num, nam, msg)	FNT_E_##nam = num,
#elif   fnt_genInfo
#define gen(n, num, nam, msg)	{ n, num, msg, (const char *) 0 },

#else
#error	invalid inclusion of generator file
#endif


/* -------------------------------------------------------------------- */
/* Font                                                     (70xx)      */
/* -------------------------------------------------------------------- */

gen(1, 7000, FONT_ILLFONTSTYLE, "Illegal fontstyle '$1' in font name")

gen(1, 7001, FONT_PREFIX, "Font '$1': ")

gen(0, 7002, FONT_HOSTNOTFOUND, "Host font not found")

gen(0, 7003, FONT_HOSTNOTFOUND2,
    "Host font not found (maybe unsupported QuickDraw font name specified)")

gen(1, 7004, FONT_UNSUPP_FORMAT, "Font format '$1' not supported")

gen(2, 7006, FONT_HOSTNOTLOADED,
    "Couldn't load $1 host font (system error code $2)")

gen(1, 7007, FONT_NAMETOOLONG,
    "Font name too long (max. $1 characters)")

gen(0, 7060, TT_BITMAP, "TrueType bitmap font not supported")

gen(1, 7062, TT_NOFONT, "Font file '%s' is not a TrueType or OpenType font")

gen(0, 7064, TT_BADCMAP, "Font contains unknown encodings (cmaps) only")

gen(0, 7066, TT_SYMBOLOS2, "Symbol font does not contain OS/2 table")

gen(0, 7068, TT_EMBED,
    "Font cannot be embedded due to licensing restrictions in the font file")

gen(0, 7070, TT_ASSERT1, "TrueType parser error")

gen(0, 7071, TT_CORRUPT1, "Corrupt TrueType font")

gen(1, 7072, TT_ASSERT2, "TrueType parser error in font file '$1'")

gen(1, 7073, TT_CORRUPT2, "Corrupt TrueType font file '$1'")

gen(1, 7074, TTC_NOTFOUND,
    "Font not found in TrueType Collection file '$1'")

gen(0, 7076, TT_NOGLYFDESC,
    "TrueType font does not contain any character outlines")

gen(1, 7077, TT_GLYPHIDNOTFOUND,
    "Couldn't find glyph id for Unicode value U+$1")

gen(0, 7078, TT_NONAME,
    "TrueType font contains only unsupported records in 'name' table")

gen(1, 7079, TT_BADPOST,
    "TrueType 'post' table has unknown reserved character index $1")

gen(1, 7080, OT_CHARSET,
    "OpenType font with predefined charset '$1' in CFF table not supported")

gen(0, 7081, OT_MULTIFONT,
    "OpenType font with multiple font entries not supported")

gen(0, 7082, OT_CHARSTRINGS,
    "OpenType font has no CharStrings data in CFF table")

gen(0, 7083, OT_TOPDICT,
    "OpenType CID font has no Top DICT INDEX in CFF table")

gen(0, 7085, OT_NO_ORDERING,
    "OpenType CID font has no ordering string in CFF table")

gen(1, 7086, OT_ILL_CHARCOLL,
    "OpenType CID font has unknown character collection '$1'")







#undef	gen
#undef  fnt_genNames
#undef  fnt_genInfo

