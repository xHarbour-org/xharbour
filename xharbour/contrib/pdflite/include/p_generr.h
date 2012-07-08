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
 * PDFlib error messages
 *
 */

#define P_GENERR_H

#if	pdf_genNames
#define gen(n, num, nam, msg)	PDF_E_##nam = num,
#elif	pdf_genInfo
#define gen(n, num, nam, msg)	{ n, num, msg, (const char *) 0 },

#else
#error	invalid inclusion of generator file
#endif


/* -------------------------------------------------------------------- */
/* Configuration 					(20xx)		*/
/* -------------------------------------------------------------------- */

gen(0, 2000, UNSUPP_CRYPT, "Encryption not supported in PDFlib Lite")

gen(0, 2002, UNSUPP_KERNING, "Kerning not supported in PDFlib Lite")

gen(0, 2003, UNSUPP_FONTINFO, "Font info not supported in PDFlib Lite")

gen(0, 2004, UNSUPP_SUBSET, "Subsetting not supported in PDFlib Lite")

gen(0, 2006, UNSUPP_PDFX, "PDF/X not supported in PDFlib Lite")

gen(1, 2008, UNSUPP_IMAGE, "$1 images not supported in this configuration")

gen(0, 2010, UNSUPP_ICC, "ICC profiles not supported in PDFlib Lite")

gen(0, 2012, UNSUPP_UNICODE,
    "Unicode and glyph id addressing not supported in PDFlib Lite")

gen(0, 2014, UNSUPP_SPOTCOLOR, "Spot colors not supported in PDFlib Lite")

gen(0, 2015, UNSUPP_ICCBASEDCOLOR,
    "ICC based or Lab colors not supported in PDFlib Lite")

gen(0, 2016, UNSUPP_PDI, "PDF import (PDI) not supported in PDFlib Lite")

gen(0, 2017, UNSUPP_PDI_CONFIG,
    "PDF import (PDI) not supported in this configuration")

gen(0, 2018, UNSUPP_BLOCK,
    "Personalization with blocks not supported in PDFlib Lite")

gen(0, 2019, UNSUPP_BLOCK_CONFIG,
    "Personalization with blocks not supported in this configuration")

gen(0, 2020, UNSUPP_FORMFIELDS, "Form fields not supported in PDFlib Lite")

gen(0, 2021, UNSUPP_JAVASCRIPT, "JavaScript not supported in PDFlib Lite")

gen(0, 2022, UNSUPP_MC, "Marked content not supported in PDFlib Lite")

gen(0, 2024, UNSUPP_TAGGED, "Tagged PDF not supported in PDFlib Lite")

gen(0, 2026, UNSUPP_LAYER,
    "Optional content (layers) not supported in PDFlib Lite")

gen(0, 2028, UNSUPP_TEXTFLOWS, "Textflow not supported in PDFlib Lite")

gen(0, 2029, UNSUPP_GLYPHCHECK,
    "Glyph check not supported in PDFlib Lite")

gen(0, 2030, UNSUPP_CHARREF,
    "Character references not supported in PDFlib Lite")

gen(0, 2031, UNSUPP_ESCAPESEQU,
    "Escape sequences not supported in PDFlib Lite")

gen(0, 2032, UNSUPP_JPEG2000,
    "JPEG2000 images not supported in PDFlib Lite")

gen(0, 2033, UNSUPP_TABLES, "Tables not supported in PDFlib Lite")

gen(0, 2034, UNSUPP_3DANNOT, "3D annotations not supported in PDFlib Lite")

gen(0, 2035, UNSUPP_HONORLANG,
    "LANG environment variable not supported in PDFlib Lite")

gen(0, 2098, BETA_EXPIRED,
    "PDFlib 6 beta expired -- get latest version at www.pdflib.com")


/* -------------------------------------------------------------------- */
/* Document, page, scoping, resource and matchbox       (21xx)          */
/* -------------------------------------------------------------------- */

gen(1, 2100, DOC_SCOPE,	"Function must not be called in '$1' scope")

gen(1, 2102, DOC_FUNCUNSUPP, "Function not supported for '$1'")

gen(2, 2104, DOC_PDFVERSION, "$1 not supported in PDF $2")

gen(0, 2106, DOC_EMPTY,	"Generated document doesn't contain any pages")

gen(0, 2110, PAGE_SIZE_ACRO, "Page size incompatible with Acrobat")

gen(2, 2112, PAGE_BADBOX, "Illegal $1 [$2]")

gen(0, 2114, PAGE_ILLCHGSIZE,
    "Page size cannot be changed in top-down coordinate system")

gen(2, 2120, RES_BADRES, "Bad resource specification '$1' for category '$2'")

gen(2, 2122, DOC_SCOPE_GET, "Can't get parameter '$1' in '$2' scope")

gen(2, 2124, DOC_SCOPE_SET, "Can't set parameter '$1' in '$2' scope")

gen(1, 2126, PAGE_NOSUSPEND, "Page number $1 has not been suspended")

gen(2, 2128, PAGE_NOSUSPEND2,
    "Page number $1 in group '$2' has not been suspended")

gen(1, 2130, PAGE_ILLNUMBER, "Illegal page number $1")

gen(1, 2132, PAGE_NOTEXIST, "Page number $1 does not exist")

gen(2, 2134, PAGE_NOTEXIST2, "Page number $1 in group '$2' does not exist")

gen(0, 2136, PAGE_NEEDGROUP, "No page group specified")

gen(0, 2138, PAGE_NEEDGROUP2,
    "No page group specified (use PDF_begin_page_ext)")

gen(1, 2140, DOC_UNKNOWNGROUP, "Unknown page group '$1'")

gen(1, 2142, DOC_GROUPMISSING,
    "Page group '$1' is missing in list of option 'grouporder'")

gen(0, 2144, DOC_OPTGROUPORDER,
    "Option 'grouporder' is illegal (no page groups are specified)")

gen(1, 2146, DOC_DUPLGROUP,
    "Duplicate definition of group '$1' in option 'grouporder'")

gen(1, 2148, DOC_ILL_LABELOPT,
    "Label option '$1' is illegal for this function")

gen(1, 2150, DOC_NEED_LABELOPT,
    "Option 'labels' requires suboption '$1' if used with this function")

gen(1, 2152, PAGE_TRANS_COMPAT,
    "Page transition '$1' requires PDF 1.5")

gen(1, 2154, PAGE_ILLROTATE, "Option 'rotate' has illegal value $1")

gen(0, 2156, PAGE_SUSPEND_TAGGED,
    "This function must not be used in Tagged PDF mode")

gen(0, 2158, PAGE_SEP_NOSPOT,
    "Option 'separationinfo' requires 'spotname' or 'spotcolor'")

gen(0, 2160, PAGE_SEP_ILLPAGES,
    "Option 'separationinfo' must not use 'pages' if not first page in group")

gen(0, 2162, PAGE_SEP_NOPAGES, "Option 'separationinfo' requires 'pages'")

gen(0, 2164, PAGE_SEP_NOINFO, "Option 'separationinfo' missing")

gen(0, 2166, DOC_SEP_INCOMPLETE, "Incomplete separation group")

gen(0, 2168, PAGE_TOPDOWN_NODIMS,
    "Must specify page dimensions with option 'topdown'")

gen(0, 2170, PAGE_NODIMS, "No dimensions specified for this page")

gen(0, 2172, DOC_GETBUF_2GB,
    "Can't process buffers larger than 2GB on this platform")

gen(1, 2174, PAGE_SUSPENDED, "Page number $1 is still suspended")

gen(0, 2176, DOC_GETBUF_LIN,
    "Don't fetch buffer contents before PDF_end_document() when linearizing")

gen(1, 2178, PAGE_ILLREF,
    "A link annotation refers to non-existing page '$1'")

gen(0, 2180, DOC_NOLINOPT_ENCATT,
    "Optimization/linearization not supported for encrypted file attachments")

gen(1, 2190, MBOX_NOTFOUND,
    "Matchbox '$1' not found")


/* -------------------------------------------------------------------- */
/* Graphics and Text					(22xx)		*/
/* -------------------------------------------------------------------- */

gen(0, 2200, GSTATE_UNMATCHEDSAVE, "Unmatched save level")

gen(0, 2202, GSTATE_RESTORE, "Invalid restore (no matching save level)")

gen(1, 2204, GSTATE_SAVELEVEL, "Too many save levels (max. $1)")

gen(0, 2210, PATTERN_SELF, "Can't use a pattern within its own definition")

gen(0, 2212, SHADING13, "Smooth shadings are not supported in PDF 1.3")

gen(1, 2220, TEMPLATE_SELF,
    "Can't place template handle $1 within its own definition")

gen(1, 2222, TEXT_ALIGNCHARNOTFOUND,
    "Character U+$1 for option 'alignchar' not found in font or encoding")

gen(1, 2223, TEXT_BADTEXTFORMAT, "Bad textformat '$1' (must be 'bytes')")

/* UNUSED
gen(1, 2230, TEXT_UNICODENOTSHOW,
    "Can't show character with Unicode value U+$1")

gen(1, 2232, TEXT_GLYPHIDNOTSHOW, "Can't show character with glyph id $1")

gen(1, 2233, TEXT_BUILTINNOTSHOW,
    "Can't show 16-bit character $1 for builtin encoding")
*/

gen(2, 2234, TEXT_TOOLONG, "Text with $1 bytes is too long (max. $2)")

gen(2, 2235, TEXT_SIZENOMATCH,
    "Size ($1) of glyphwidths list doesn't match size ($2 characters) of text")

gen(0, 2236, TEXT_TOOMANYCODES, "Too many different unicode values (> 256)")

gen(0, 2237, TEXT_NOFONTSIZESET, "Font size not specified for text")

gen(0, 2238, TEXT_NOFONT, "No font set for text")

gen(0, 2239, TEXT_ITALUNSUPP,
    "Parameter 'italicangle' not supported for vertical writing mode")

gen(1, 2240, TEXT_NOFONT_PAR, "No font set for parameter '$1'")






/* -------------------------------------------------------------------- */
/* Color						(23xx)		*/
/* -------------------------------------------------------------------- */

gen(0, 2300, COLOR_SPOT,
"Spot color can not be based on a Pattern, Indexed, or Separation color space")

gen(2, 2302, COLOR_BADSPOT, "Color name '$1' not found in $2 table")

gen(1, 2304, COLOR_SPOTBW,
    "Alternate color for custom spot color '$1' is black or white")

gen(1, 2306, COLOR_UNLIC_SPOTCOLOR, "$1 spot colors not licensed")

gen(0, 2308, COLOR_UNSUPP_SPOTNAME, "Unicode spot color names not supported")

gen(2, 2309, COLOR_REDEFINE_SPOTNAME,
    "Option '$1': spot color '$2' has already an alternative color")


gen(1, 2350, COLOR_INVALPATT,
    "Pattern with painttype 2 can not be colorized with another pattern")

gen(0, 2352, COLOR_INVALSPEC,
    "Color specification not allowed while defining a pattern with painttype 2")

/* -------------------------------------------------------------------- */
/* Image						(24xx)		*/
/* -------------------------------------------------------------------- */

gen(2, 2400, IMAGE_CORRUPT, "Corrupt $1 image file '$2'")

gen(4, 2401, IMAGE_TOO_LARGE, "$1 image '$2' too large ($3 x $4)")

gen(3, 2402, IMAGE_NOPAGE, "Requested page $1 in $2 image '$3' not found")

gen(2, 2404, IMAGE_BADDEPTH,
    "Bad number of bits per pixel ($1) in image file '$2'")

gen(1, 2406, IMAGE_BADMASK,
    "Image '$1' not suitable as mask (more than one color component)")

gen(2, 2407, IMAGE_NOMATCH,
    "Image '$1' not suitable as mask for image '$2' (different orientation)")

gen(1, 2408, IMAGE_MASK1BIT13,
    "Image '$1' with more than 1 bit not supported as mask in PDF 1.3")

gen(1, 2410, IMAGE_COLORMAP, "Couldn't read colormap in image '$1'")

gen(2, 2412, IMAGE_BADCOMP,
    "Bad number of color components ($1) in image '$2'")

gen(1, 2414, IMAGE_COLORIZE,
    "Can't colorize image '$1' with more than 1 component")

gen(1, 2416, IMAGE_ICC, "Couldn't handle embedded ICC profile in image '$1'")

gen(1, 2418, IMAGE_ICC2,
    "ICC profile for image file '$1' doesn't match image data")

gen(0, 2420, IMAGE_THUMB, "More than one thumbnail for this page")

gen(1, 2422, IMAGE_THUMB_MULTISTRIP,
    "Can't use multi-strip image $1 as thumbnail")

gen(1, 2424, IMAGE_THUMB_CS,
    "Unsupported color space in thumbnail image handle $1")

gen(2, 2426, IMAGE_THUMB_SIZE, "Thumbnail image $1 larger than $2 pixels")

gen(2, 2428, IMAGE_OPTUNSUPP,
    "Option '$1' for image type '$2' not supported")

gen(2, 2430, IMAGE_OPTUNREAS,
    "Option '$1' for image type '$2' doesn't have any effect")

gen(2, 2432, IMAGE_OPTBADMASK, "Option '$1' has bad image mask $2")

gen(1, 2434, IMAGE_UNKNOWN, "Unknown image type in file '$1'")

gen(0, 2436, IMAGE_NOADJUST,
    "Option 'adjustpage' must not be used in top-down system")

gen(1, 2437, IMAGE_OPI_ILLRECT, "Option '$1' has bad rectangle")

gen(2, 2438, IMAGE_OPI_ILLMAPSIZE, "Option '$1': Number of values must be $2")

gen(1, 2439, IMAGE_OPI_ILLPARALL, "Option '$1' has bad parallelogram")

gen(2, 2440, RAW_ILLSIZE,
    "Size ($1 bytes) of raw image file '$2' doesn't match specified options")

gen(2, 2442, IMAGE_TYPUNSUPP, "Image type '$1' is not supported in PDF $2")

gen(1, 2444, BMP_VERSUNSUPP,
    "Version of BMP image file '$1' not supported")

gen(1, 2446, BMP_COMPUNSUPP,
    "Compression in BMP image file '$1' not supported")

gen(2, 2450, JPEG_COMPRESSION,
    "JPEG compression scheme $1 in file '$2' not supported in PDF")

/* UNUSED
gen(1, 2452, JPEG_MULTISCAN,
    "JPEG file '$1' contains multiple scans, which is not supported in PDF")
*/

gen(2, 2454, JPEG_TRANSCODE,
    "Problems during JPEG transcoding in file '$1' ($2)")

/* UNUSED
gen(1, 2460, GIF_LZWOVERFLOW, "LZW code size overflow in GIF file '$1'")

gen(1, 2462, GIF_LZWSIZE,
    "Color palette in GIF file '$1' with fewer than 128 colors not supported")

gen(1, 2464, GIF_INTERLACED, "Interlaced GIF image '$1' not supported")

gen(2, 2470, TIFF_UNSUPP_CS,
    "Couldn't open TIFF image '$1' (unsupported color space; photometric $2)")

gen(2, 2472, TIFF_UNSUPP_PREDICT,
    "Couldn't open TIFF image '$1' (unsupported predictor tag $2)")

gen(1, 2474, TIFF_UNSUPP_LZW, "Couldn't open LZW-compressed TIFF image '$1')")

gen(1, 2476, TIFF_UNSUPP_LZW_PLANES,
    "Couldn't open TIFF image '$1' (separate planes with LZW compression)")

gen(1, 2478, TIFF_UNSUPP_LZW_ALPHA,
    "Couldn't open TIFF image '$1' (alpha channel with LZW compression)")

gen(2, 2480, TIFF_UNSUPP_JPEG,
    "Couldn't open TIFF image '$1' (JPEG compression scheme $2)")

gen(1, 2482, TIFF_UNSUPP_JPEG_TILED,
    "Couldn't open TIFF image '$1' (tiled image with JPEG compression)")
*/

gen(2, 2483, TIFF_UNSUPP_COLORSPACE,
    "Color space (photometric) $1 in TIFF image '$2' not supported")

gen(1, 2484, TIFF_UNSUPP_SEPARATE,
    "Couldn't open TIFF image '$1' (separate image planes not supported)")

gen(1, 2485, TIFF_TILE_UNSUPP,
    "Couldn't open TIFF image '$1' "
    "(separate image planes with tiles not supported)")

gen(2, 2486, TIFF_UNSUPP_SEP_NONCMYK,
    "Couldn't open TIFF image '$1' (unsupported inkset tag $2)")

gen(1, 2488, TIFF_MASK_MULTISTRIP, "Can't mask multistrip TIFF image '$1'")

gen(2, 2489, TIFF_CLIPPPATH_NOTFOUND,
    "Couldn't find clipping path '$1' in TIFF image '$2'")

gen(1, 2490, TIFF_16BITCMYK_UNSUPP,
    "Compressed 16-bit CMYK TIFF image '$1' not supported")

gen(1, 2491, TIFF_16BIT_UNSUPP,
    "More than 16 bits per component in TIFF image '$1' not supported")

gen(1, 2492, TIFF_CMYK_MASK, "Couldn't open TIFF image '$1' (CMYK with mask)")

gen(2, 2493, TIFF_UNSUPP_COMPRESSION,
    "Compression scheme $1 in TIFF image '$2' not supported")

gen(1, 2494, JPX_FORMATUNSUPP,
    "JPEG2000 flavor in image file '$1' not supported")

gen(1, 2496, JPX_RAWDATAUNSUPP,
    "Raw JPEG2000 code stream in image file '$1' not supported")

gen(1, 2498, JPX_COMPOUNDUNSUPP,
    "Compound JPEG2000 (JPM) image file '$1' not supported")

gen(1, 2499, TIFF_BPC_UNSUPP,
"TIFF image '$1' contains bpc that not equal to 1,2, 4, 8, 16, not supported")

/* -------------------------------------------------------------------- */
/* Font							(25xx)		*/
/* -------------------------------------------------------------------- */

gen(2, 2500, FONT_CORRUPT, "Corrupt $1 font file $2")

gen(2, 2501, FONT_PREFIX, "Font '$1' with encoding '$2': ")

gen(0, 2502, FONT_BADENC, "Font doesn't support encoding")

gen(3, 2503, FONT_PREFIX2, "Font '$1' with encoding '$2' (changed to '$3'): ")

gen(1, 2504, FONT_FORCEENC,
    "Font doesn't support encoding (encoding '$1' will be used)")

gen(0, 2505, FONT_NEEDUCS2,
    "Encoding not supported (use Unicode-compatible CMap)")

/* UNUSED
gen(0, 2506, FONT_FORCEEMBED,
    "Font will be embedded (encoding requires CID font")
*/

gen(1, 2507, FONT_INAPPROPENC,
    "Encoding not appropriate for the font (only $1 glyphs found)")

/* UNUSED
gen(1, 2508, FONT_BADTEXTFORM,
    "Current text format not allowed for builtin encoding")
*/

gen(0, 2509, FONT_FORCECVTUNI,
    "Native text code (keepnative) for this font configuration will be ignored")

gen(0, 2514, FONT_EMBEDMM, "Multiple Master font cannot be embedded")

gen(0, 2516, FONT_NOMETRICS,
    "Font file (AFM, PFM, TTF, OTF etc.) or host font not found")

gen(0, 2517, FONT_NOOUTLINE_PS,
    "Font cannot be embedded (PFA or PFB font file not found)")

/* Unused
gen(0, 2518, FONT_NOOUTLINE, "File with outline data not found")
*/

gen(0, 2519, FONT_NOOUTLINE_TT,
    "Font cannot be embedded (TTF or OTF font file not found)")

/* Unused
gen(0, 2520, FONT_NOGLYPHID, "Font doesn't contain any glyph IDs")
*/

gen(0, 2522, FONT_FORCEEMBED2, "Metadata requires embedding")

gen(0, 2523, FONT_CORRUPT_PFA,
    "Corrupt PFA font data (invalid hex code after 'currentfile eexec')")

gen(0, 2530, CJK_UNSUPP_REGISTRY,
    "Font not supported (contains non-Adobe registry in CMap)")

gen(0, 2531, CJK_UNSUPP_CHARCOLL,
    "CJK font doesn't support encoding (use a compatible predefined CMap)")

gen(0, 2532, FONT_EMBEDCMAP,
    "Standard CJK font with predefined CMap cannot be embedded")

gen(0, 2533, FONT_ONLY_CMAP,
    "Font doesn't support encoding (use predefined CMap or 'unicode' encoding)")

/* Unused
gen(0, 2534, FONT_EMBEDSTYLE,
    "Option 'fontstyle' not allowed for embedded fonts")
*/

gen(0, 2535, FONT_UNSUPP_CMAP,
    "Font doesn't support predefined CMap")

gen(2, 2536, FONT_UNSUPPOPTION,
    "Option '$1' not supported for font type '$2'")

gen(0, 2538, FONT_IGNOREVERTICAL,
    "Option 'vertical' ignored because of predefined CMap or font name resp.")

gen(1, 2540, T3_BADBBOX,
    "Bounding box values must be 0 for colorized font")

gen(1, 2541, T3_FONT_PREFIX, "Type3 font '$1': ")

gen(1, 2542, T3_GLYPH, "Glyph '$1' already defined")

gen(0, 2544, T3_FONTEXISTS, "Already exists")

gen(0, 2545, T3_FONTSUBSET, "Font with subsetting can only be loaded once")

gen(1, 2546, T3_GLYPHMISSING, "Outlines of glyph '$1' not defined")

gen(1, 2547, T3_OUTLINESMISSING, "Outlines of Type3 font '$1' missing")

gen(1, 2548, T3_UNKOWNGLYPH, "Glyph '$1' unknown")

gen(1, 2549, T3_MISSNOTDEF, "Glyph for character '.notdef' is missing")

gen(1, 2550, T1_BADCHARSET,
    "PDFlib doesn't support encoding (dfCharSet $1 in PFM file unknown)")

gen(1, 2551, T1_UNSUPP_FORMAT, "'$1' metrics file type not supported")

gen(2, 2554, T1_AFMBADKEY, "Unknown key '$1' in AFM file '$2'")

gen(1, 2558, T1_NOFONT, "'$1' is not a PostScript Type1 font file")

gen(1, 2559, T1_RESOURCENOTLOADED,
    "Couldn't load PostScript font resource file (system error code $1)")

gen(2, 2560, FONT_CODENOTFOUND1,
    "Glyph with character code $1 not found in font '$2'")

gen(2, 2561, FONT_CODENOTFOUNDREP1,
    "Glyph with character code $1 not found in font '$2' (will be replaced)")

gen(2, 2562, FONT_UNICODENOTFOUND,
    "Glyph with Unicode value U+$1 not found in font '$2'")

gen(2, 2563, FONT_UNICODENOTFOUNDREP,
    "Glyph with Unicode value U+$1 not found in font '$2' (will be replaced)")

gen(2, 2564, FONT_GLYPHIDNOTFOUND,
    "Glyph with id $1 not found in font '$2'")

gen(3, 2566, FONT_CODENOTFOUND2,
    "Glyph with code $1 and Unicode value U+$2 not found in font '$3'")

gen(3, 2567, FONT_CODENOTFOUNDREP2,
    "Glyph with code $1 and Unicode value U+$2 not found in font '$3' "
    "(will be replaced)")

gen(2, 2569, FONT_NOTFULFILL,
    "Option '$1' cannot be fulfilled (same font already loaded with no$1)")

/* -------------------------------------------------------------------- */
/* Encoding						(26xx)		*/
/* -------------------------------------------------------------------- */

/* MOVED to pc_generr.h, #1552
gen(1, 2600, ENC_NOTFOUND, "Couldn't find encoding '$1'")
*/

gen(1, 2602, ENC_UNSUPP, "Code page '$1' not supported")

gen(1, 2606, ENC_CANTQUERY, "Can't query encoding '$1'")

gen(1, 2608, ENC_CANTCHANGE, "Can't change encoding '$1'")

gen(1, 2610, ENC_INUSE,
    "Encoding '$1' can't be changed since it has already been used")

/* MOVED to pc_generr.h, #1550
gen(2, 2612, ENC_TOOLONG, "Encoding name '$1' too long (max. $2)")

   MOVED to pc_generr.h, #1551
gen(2, 2614, ENC_BADLINE, "Syntax error in encoding file '$1' (line '$2')")
*/

gen(0, 2616, ENC_GLYPHORCODE, "Glyph name or Unicode value required")

gen(3, 2618, ENC_BADGLYPH,
    "Glyph name '$1' for Unicode value U+$2 differs from AGL name '$3'")

gen(3, 2620, ENC_BADUNICODE,
    "Unicode value U+$1 for glyph name '$2' differs from AGL value U+$3")

gen(2, 2622, ENC_BADFONT,
    "Current font $1 wasn't specified with encoding '$2'")

gen(1, 2640, ENC_BADHYPTEXTENC, "Bad hypertext encoding '$1'")

gen(1, 2650, ENC_UNSUPPENCFORMAT,
    "Parameter or option '$1' not supported in Unicode-capable languages")

/* Unused
gen(2, 2670, CMAP_ILLCODESEQU, "Illegal code sequence in '$1' for CMap '$2'")
*/



/* -------------------------------------------------------------------- */
/* Hypertext, form fields, actions, annotations		(28xx)		*/
/* -------------------------------------------------------------------- */

gen(2, 2802, HYP_OPTIGNORE_FORTYPE,
    "Option '$1' for destination type '$2' doesn't have any effect")

gen(1, 2804, HYP_OPTIGNORE_FORELEM,
    "Option '$1' for hypertext function will be ignored")

gen(2, 2820, FF_OPTEFFLESS_FORTYPE,
    "Option '$1' for field type '$2' doesn't have any effect")

gen(1, 2822, FF_GROUPMISSING,
    "Required field group missing for radio button field '$1'")

gen(1, 2824, FF_FONTMISSING, "Font not specified for field '$1'")

gen(2, 2826, FF_OPTIONMISSING,
    "Option '$1' not specified for field '$2'")

gen(1, 2828, FF_CIDFONT,
    "Specified font '$1' not allowed for fields (encoding not supported)")

gen(1, 2830, FF_NOEMBEDFONT,
    "Specified font '$1' not allowed for fields (must be embedded)")

gen(1, 2832, FF_SUBSETTFONT,
    "Specified font '$1' not allowed for fields (must not be subset)")

gen(2, 2833, FF_NOPDFDOCFONT,
    "Specified encoding '$1' of font '$2' not recommended for fields "
    "(please use encoding 'pdfdoc')")

gen(1, 2834, FF_CAPTMISSING, "No caption or icon specified for field '$1'")

gen(0, 2836, FF_DIFFSTRLISTS,
    "Options 'itemnamelist' and 'itemtextlist' contain "
    "different numbers of strings")

gen(2, 2838, FF_INVALINDEX, "Option '$1' has invalid list index '$2'")

gen(2, 2840, FF_NOTFOUND,
    "Illegal field pathname '$1' (name '$2' not found)")

gen(2, 2842, FF_NAMEEXISTS,
    "Illegal field pathname '$1' (name '$2' already exists)")

gen(2, 2844, FF_NOTGROUP,
    "Illegal field pathname '$1' ('$2' is not a field group)")

gen(3, 2846, FF_TYPENOTMATCH,
    "Type '$1' of field '$2' doesn't match type '$3' of group")

gen(0, 2848, FF_ITEMNAMEORNOT,
"Either all or none of the buttons/checkboxes in a group can have item names")

gen(2, 2850, FF_OPTEFFONLY,
    "Option '$1' for field type '$2' only has an effect for highlight=push")

gen(2, 2852, FF_ILLUNINAME,
    "Illegal field name '$1' (Unicode names are not supported in PDF $2")

gen(0, 2854, FF_DEMOLIMIT,
    "No more than 10 fields can be created with the evaluation version")

gen(0, 2856, FF_RICHTEXT,
    "fontsize 0 not supported for fields with rich text")

gen(2, 2860, ACT_OPTIGNORE_FORTYPE,
    "Option '$1' for action type '$2' doesn't have any effect")

gen(2, 2862, ACT_BADACTTYPE, "Action type '$1' for event '$2' not allowed")


gen(2, 2880, ANN_OPTEFFLESS_FORTYPE,
    "Option '$1' for annotation type '$2' doesn't have any effect")

gen(1, 2882, ANN_NOSTDFONT,
    "Font '$1' not allowed for annotations (not a core or standard CJK font)")

gen(1, 2884, ANN_BADNUMCOORD, "Option '$1' has bad number of coordinates")

gen(1, 2886, ANN_OPTALRDEF,
    "Option '$1' already defined in option 'custom'")

gen(1, 2888, ANN_ILLCUSTOMKEY,
    "Option 'custom' uses illegal key '$1' (already defined in PDF)")

gen(0, 2890, ANN_ILLTEMPLATE,
    "Option 'movieposter' has bad template handle "
    "(only images with RGB or Gray color spaces supported)")

gen(1, 2892, ANN_ACTIONNOTALLOWED,
    "Option 'action' not allowed for annotation type '$1'")

/* -------------------------------------------------------------------- */
/* Internal 						(29xx)		*/
/* -------------------------------------------------------------------- */

gen(1, 2900, INT_BADSCOPE, "Bad scope '$1'")

gen(1, 2902, INT_BADANNOT, "Bad annotation type '$1'")

gen(3, 2904, INT_BADCS,
"Unknown color space (function $1, index $2, type $3)")

gen(1, 2906, INT_BADALTERNATE, "Bad alternate color space $1")

gen(1, 2908, INT_BADPROFILE, "Unknown number of profile components ($1)")

gen(1, 2910, INT_SSTACK_OVER, "State stack overflow in function '$1'")

gen(1, 2912, INT_SSTACK_UNDER, "State stack underflow in function '$1'")

gen(3, 2914, INT_WRAPPER, "Error in PDFlib $1 wrapper function $2 ($3)")

gen(0, 2990, OT_UNSUPP_SID2CID,
"Accented characters not supported; use autocidfont=false in PDF_load_font()")







#undef	gen
#undef	pdf_genNames
#undef	pdf_genInfo



