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
 * PDFlib parameter table
 *
 */

#if	pdf_gen_parm_enum
#define pdf_gen1(code, name, zero, check, depr, scope)  PDF_PARAMETER_##code,
#elif	pdf_gen_parm_descr
#define pdf_gen1(code, name, zero, check, depr, scope)          \
        { name, zero, check, depr, scope },
#else
#error	invalid inclusion of generator file
#endif

/*
 * Deprecated and unsupported parameters (see variable depr = i):
 *   i = 0: Neither deprecated nor unsupported
 *   i > 0: Deprecated since PDFlib i
 *   i =-1: Unsupported (internal use, dysfunctional, or other)
 */

/*
   List of unsupported control characters for the "debug" parameter:
   2    disable the search for the Windows color directory via mscms.dll
   e    extract embedded ICC profiles from image files
   F    throw an exception in PDF_load_font (depr = 7);
   g    throw an exception in PDF_fit_textline and PDF_show (depr = 7)
   h    disable host font processing
   i    throw an exception in PDF_load_image (depr = 7)
   I    throw an exception in PDF_load_iccprofile (depr = 7);
   o    throw an exception in PDF_begin_document (depr = 7)
   p    throw an exception in PDF_open_pdi (depr = 7)

   On by default:  e F I
*/


/*
 * ----------------------------------------------------------------------
 * Setup
 * ----------------------------------------------------------------------
 */

pdf_gen1(OPENWARNING,   "openwarning",  1, 1, 6, pdf_state_all)
pdf_gen1(COMPRESS,      "compress",     1, 1, 0,
         pdf_state_page | pdf_state_document)
pdf_gen1(FLUSH,         "flush",        1, 1, 6, pdf_state_all)
pdf_gen1(RESOURCEFILE,  "resourcefile", 1, 1, 0, pdf_state_all)
pdf_gen1(COMPATIBILITY, "compatibility",1, 0, 6, pdf_state_object)
pdf_gen1(PDFX,          "pdfx",         1, 0, 6, pdf_state_object)
pdf_gen1(SEARCHPATH,    "SearchPath",   0, 1, 0, pdf_state_all)
pdf_gen1(ASCIIFILE,     "asciifile",    1, 1, 0, pdf_state_all)
pdf_gen1(WARNING,       "warning",      1, 1, 7, pdf_state_all)
pdf_gen1(ERRORPOLICY,   "errorpolicy",  1, 1, 0, pdf_state_all)
pdf_gen1(NODEMOSTAMP,   "nodemostamp",  1, 0, 0, pdf_state_object)
pdf_gen1(LICENSE,       "license",      1, 0, 0, pdf_state_object)
pdf_gen1(LICENCE,       "licence",      1, 0,-1, pdf_state_object)
pdf_gen1(LICENSEFILE,   "licensefile",  1, 0, 0, pdf_state_object)
pdf_gen1(LICENCEFILE,   "licencefile",  1, 0,-1, pdf_state_object)
pdf_gen1(TRACE,         "trace",        1, 1, 7, pdf_state_all)
pdf_gen1(TRACEFILE,     "tracefile",    1, 1, 7, pdf_state_all)
pdf_gen1(TRACEMSG,      "tracemsg",     1, 1, 7, pdf_state_all)
pdf_gen1(LOGGING,       "logging",      1, 1, 0, pdf_state_all)
pdf_gen1(LOGMSG,        "logmsg",       1, 1, 0, pdf_state_all)
pdf_gen1(CHARREF,       "charref",      1, 1, 0, pdf_state_all)
pdf_gen1(ESCAPESEQUENCE,"escapesequence",1,1, 0, pdf_state_all)
pdf_gen1(HONORLANG,     "honorlang",    1, 1,-1, pdf_state_all)
pdf_gen1(STRING,        "string",       0, 1, 0, pdf_state_all)
pdf_gen1(SCOPE,         "scope",        1, 1, 0, pdf_state_all)

pdf_gen1(SERIAL,        "serial",       1, 0,-1, pdf_state_object)
pdf_gen1(FLOATDIGITS,   "floatdigits",  1, 1,-1, pdf_state_all)
pdf_gen1(BINDING,       "binding",      1, 1,-1, pdf_state_all)
pdf_gen1(OBJORIENT,     "objorient",    1, 1,-1, pdf_state_all)
pdf_gen1(HASTOBEPOS,    "hastobepos",   1, 1,-1, pdf_state_all)
pdf_gen1(UNICAPLANG,    "unicaplang",   1, 1,-1, pdf_state_all)
pdf_gen1(DEBUG,         "debug",        1, 1,-1, pdf_state_all)
pdf_gen1(NODEBUG,       "nodebug",      1, 1,-1, pdf_state_all)
pdf_gen1(PTFRUN,        "ptfrun",       1, 1,-1, pdf_state_all)
pdf_gen1(SMOKERUN,      "smokerun",     1, 1,-1, pdf_state_all)
pdf_gen1(CONFIGURATION, "configuration",1, 1,-1, pdf_state_all)
pdf_gen1(PRODUCT,       "product",      1, 1,-1, pdf_state_all)
pdf_gen1(MAXFILEHANDLES,"maxfilehandles",1, 1,-1, pdf_state_object)


/*
 * ----------------------------------------------------------------------
 * Versioning (cf. pdflib.c)
 * ----------------------------------------------------------------------
 */

pdf_gen1(MAJOR,         "major",        1, 1, 0, pdf_state_all)
pdf_gen1(MINOR,         "minor",        1, 1, 0, pdf_state_all)
pdf_gen1(REVISION,      "revision",     1, 1, 0, pdf_state_all)
pdf_gen1(VERSION,       "version",      1, 1, 0, pdf_state_all)


/*
 * ----------------------------------------------------------------------
 * Page
 * ----------------------------------------------------------------------
 */

/* all of the following group are dep6 */

pdf_gen1(PAGEWIDTH,    "pagewidth",    1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(PAGEHEIGHT,   "pageheight",   1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(CROPBOX_LLX,  "CropBox/llx",  1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(CROPBOX_LLY,  "CropBox/lly",  1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(CROPBOX_URX,  "CropBox/urx",  1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(CROPBOX_URY,  "CropBox/ury",  1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(BLEEDBOX_LLX, "BleedBox/llx", 1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(BLEEDBOX_LLY, "BleedBox/lly", 1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(BLEEDBOX_URX, "BleedBox/urx", 1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(BLEEDBOX_URY, "BleedBox/ury", 1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(TRIMBOX_LLX,  "TrimBox/llx",  1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(TRIMBOX_LLY,  "TrimBox/lly",  1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(TRIMBOX_URX,  "TrimBox/urx",  1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(TRIMBOX_URY,  "TrimBox/ury",  1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(ARTBOX_LLX,   "ArtBox/llx",   1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(ARTBOX_LLY,   "ArtBox/lly",   1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(ARTBOX_URX,   "ArtBox/urx",   1, 1, 0, pdf_state_page | pdf_state_path)
pdf_gen1(ARTBOX_URY,   "ArtBox/ury",   1, 1, 0, pdf_state_page | pdf_state_path)


/*
 * ----------------------------------------------------------------------
 * Font
 * ----------------------------------------------------------------------
 */

pdf_gen1(FONTAFM,       "FontAFM",      0, 1, 0, pdf_state_all)
pdf_gen1(FONTPFM,       "FontPFM",      0, 1, 0, pdf_state_all)
pdf_gen1(FONTOUTLINE,   "FontOutline",  0, 1, 0, pdf_state_all)
pdf_gen1(HOSTFONT,      "HostFont",     0, 1, 0, pdf_state_all)
pdf_gen1(ENCODING,      "Encoding",     0, 1, 0, pdf_state_all)
pdf_gen1(FONTWARNING,   "fontwarning",  1, 1, 7, pdf_state_all)
pdf_gen1(FONT,          "font",         1, 1, 0, pdf_state_content)
pdf_gen1(FONTSIZE,      "fontsize",     1, 1, 0, pdf_state_content)

pdf_gen1(SUBSETLIMIT,   "subsetlimit",  1, 1, 7, pdf_state_all)
pdf_gen1(SUBSETMINSIZE, "subsetminsize",1, 1, 7, pdf_state_all)
pdf_gen1(AUTOSUBSETTING,"autosubsetting",1,1, 7, pdf_state_all)
pdf_gen1(AUTOCIDFONT,   "autocidfont",  1, 1, 7, pdf_state_all)
pdf_gen1(UNICODEMAP,    "unicodemap",   1, 1, 7, pdf_state_all)

pdf_gen1(FONTNAME,      "fontname",     1, 1, 7, pdf_state_content)
pdf_gen1(FONTSTYLE,     "fontstyle",    1, 1, 7, pdf_state_content)
pdf_gen1(FONTENCODING,  "fontencoding", 1, 1, 7, pdf_state_content)
pdf_gen1(MONOSPACE,     "monospace",    1, 1, 7, pdf_state_content)
pdf_gen1(FONTMAXCODE,   "fontmaxcode",  0, 1, 7, pdf_state_all)
pdf_gen1(ASCENDER,      "ascender",     0, 1, 7, pdf_state_all)
pdf_gen1(DESCENDER,     "descender",    0, 1, 7, pdf_state_all)
pdf_gen1(CAPHEIGHT,     "capheight",    0, 1, 7, pdf_state_all)
pdf_gen1(XHEIGHT,       "xheight",      0, 1, 7, pdf_state_all)
pdf_gen1(ASCENDERFAKED, "ascenderfaked",0, 1, 7, pdf_state_all)
pdf_gen1(DESCENDERFAKED,"descenderfaked",0,1, 7, pdf_state_all)
pdf_gen1(CAPHEIGHTFAKED,"capheightfaked",0,1, 7, pdf_state_all)
pdf_gen1(XHEIGHTFAKED,  "xheightfaked",  0,1, 7, pdf_state_all)


/*
 * ----------------------------------------------------------------------
 * Text
 * ----------------------------------------------------------------------
 */

pdf_gen1(TEXTX,         "textx",        1, 1, 0, pdf_state_content)
pdf_gen1(TEXTY,         "texty",        1, 1, 0, pdf_state_content)
pdf_gen1(LEADING,       "leading",      1, 1, 0, pdf_state_content)
pdf_gen1(TEXTRISE,      "textrise",     1, 1, 0, pdf_state_content)
pdf_gen1(HORIZSCALING,  "horizscaling", 1, 1, 0,
         pdf_state_content | pdf_state_document)
pdf_gen1(TEXTRENDERING,	"textrendering",1, 1, 0, pdf_state_content)
pdf_gen1(CHARSPACING,   "charspacing",  1, 1, 0,
         pdf_state_content | pdf_state_document)
pdf_gen1(WORDSPACING,   "wordspacing",  1, 1, 0,
         pdf_state_content | pdf_state_document)
pdf_gen1(ITALICANGLE,   "italicangle",  1, 1, 0,
         pdf_state_content | pdf_state_document)
pdf_gen1(FAKEBOLD,      "fakebold",     1, 1, 0,
         pdf_state_content | pdf_state_document)
pdf_gen1(UNDERLINEWIDTH,"underlinewidth", 1, 1, 0,
         pdf_state_content | pdf_state_document)
pdf_gen1(UNDERLINEPOSITION,"underlineposition", 1, 1, 0,
         pdf_state_content | pdf_state_document)
pdf_gen1(UNDERLINE,     "underline",    1, 1, 0, pdf_state_content)
pdf_gen1(OVERLINE,      "overline",     1, 1, 0, pdf_state_content)
pdf_gen1(STRIKEOUT,     "strikeout",    1, 1, 0, pdf_state_content)
pdf_gen1(KERNING,       "kerning",      1, 1, 0, pdf_state_all)
pdf_gen1(TEXTFORMAT,    "textformat",   1, 1, 0, pdf_state_all)
pdf_gen1(GLYPHWARNING,  "glyphwarning", 1, 1, 7, pdf_state_all)
pdf_gen1(GLYPHCHECK,    "glyphcheck",   1, 1, 0, pdf_state_all)


/*
 * ----------------------------------------------------------------------
 * Graphics
 * ----------------------------------------------------------------------
 */

pdf_gen1(CURRENTX,      "currentx",     1, 1, 0,
         pdf_state_content | pdf_state_path)
pdf_gen1(CURRENTY,      "currenty",     1, 1, 0,
         pdf_state_content | pdf_state_path)
pdf_gen1(FILLRULE,      "fillrule",     1, 1, 0, pdf_state_content)
pdf_gen1(TOPDOWN,       "topdown",      1, 0, 0, pdf_state_document)
pdf_gen1(CTM_A,         "ctm_a",        1, 1, 0, pdf_state_content)
pdf_gen1(CTM_B,         "ctm_b",        1, 1, 0, pdf_state_content)
pdf_gen1(CTM_C,         "ctm_c",        1, 1, 0, pdf_state_content)
pdf_gen1(CTM_D,         "ctm_d",        1, 1, 0, pdf_state_content)
pdf_gen1(CTM_E,         "ctm_e",        1, 1, 0, pdf_state_content)
pdf_gen1(CTM_F,         "ctm_f",        1, 1, 0, pdf_state_content)


/*
 * ----------------------------------------------------------------------
 * Color
 * ----------------------------------------------------------------------
 */

pdf_gen1(SETCOLOR_ICCPROFILEGRAY,      "setcolor:iccprofilegray", 1, 1, 0,
         pdf_state_document | pdf_state_content)
pdf_gen1(SETCOLOR_ICCPROFILERGB,       "setcolor:iccprofilergb",  1, 1, 0,
         pdf_state_document | pdf_state_content)
pdf_gen1(SETCOLOR_ICCPROFILECMYK,      "setcolor:iccprofilecmyk", 1, 1, 0,
         pdf_state_document | pdf_state_content)
pdf_gen1(IMAGE_ICCPROFILE,"image:iccprofile",   0, 1, 0,
         pdf_state_path | pdf_state_content | pdf_state_document)
pdf_gen1(ICCWARNING,      "iccwarning",         1, 1, 7,
         pdf_state_all)
pdf_gen1(HONORICCPROFILE, "honoriccprofile",    1, 1, 0, pdf_state_all)
pdf_gen1(ICCCOMPONENTS,   "icccomponents",      0, 1, 0, pdf_state_all)
pdf_gen1(ICCPROFILE,      "ICCProfile",         0, 1, 0, pdf_state_all)
pdf_gen1(STANDARDOUTPUTINTENT, "StandardOutputIntent", 0, 1, 0, pdf_state_all)
pdf_gen1(RENDERINGINTENT, "renderingintent",    1, 1, 0, pdf_state_all)

/* 3 x dep6 */
pdf_gen1(DEFAULTRGB,    "defaultrgb",   1, 1, 0,
         pdf_state_content | pdf_state_path)
pdf_gen1(DEFAULTGRAY,   "defaultgray",  1, 1, 0,
         pdf_state_content | pdf_state_path)
pdf_gen1(DEFAULTCMYK,   "defaultcmyk",  1, 1, 0,
         pdf_state_content | pdf_state_path)

pdf_gen1(PRESERVEOLDPANTONENAMES, "preserveoldpantonenames", 1, 1, 0,
         pdf_state_all)
pdf_gen1(SPOTCOLORLOOKUP,         "spotcolorlookup",         1, 1, 0,
         pdf_state_all)

/*
 * ----------------------------------------------------------------------
 * Image
 * ----------------------------------------------------------------------
 */

pdf_gen1(IMAGEWARNING,  "imagewarning", 1, 1, 7, pdf_state_all)
pdf_gen1(IMAGEWIDTH,    "imagewidth",   0, 1, 0,
	 pdf_state_path | pdf_state_content | pdf_state_document)
pdf_gen1(IMAGEHEIGHT,   "imageheight",  0, 1, 0,
	 pdf_state_path | pdf_state_content | pdf_state_document)
pdf_gen1(RESX,          "resx",         0, 1, 0,
	 pdf_state_path | pdf_state_content | pdf_state_document)
pdf_gen1(RESY,          "resy",         0, 1, 0,
         pdf_state_path | pdf_state_content | pdf_state_document)
pdf_gen1(ORIENTATION,   "orientation",  0, 1, 0,
         pdf_state_path | pdf_state_content | pdf_state_document)

pdf_gen1(INHERITGSTATE, "inheritgstate",1, 1, 6, pdf_state_all)


/*
 * ----------------------------------------------------------------------
 * PDI
 * ----------------------------------------------------------------------
 */

pdf_gen1(PDI,           "pdi",          1, 1, 0, pdf_state_all)
pdf_gen1(PDIWARNING,    "pdiwarning",   1, 1, 7, pdf_state_all)
pdf_gen1(PDIUSEBOX,     "pdiusebox",    1, 1, 6, pdf_state_all)
pdf_gen1(PDISTRICT,     "pdistrict",    1, 1,-1, pdf_state_all)


/*
 * ----------------------------------------------------------------------
 * Hypertext
 * ----------------------------------------------------------------------
 */

pdf_gen1(HYPERTEXTFORMAT,   "hypertextformat",   1, 1, 0, pdf_state_all)
pdf_gen1(HYPERTEXTENCODING, "hypertextencoding", 1, 1, 0, pdf_state_all)
pdf_gen1(USERCOORDINATES,   "usercoordinates",   1, 1, 0, pdf_state_all)
pdf_gen1(USEHYPERTEXTENCODING,   "usehypertextencoding",   1, 1, 0,
                pdf_state_all)

pdf_gen1(HIDETOOLBAR,   "hidetoolbar",  1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(HIDEMENUBAR,   "hidemenubar",  1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(HIDEWINDOWUI,  "hidewindowui", 1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(FITWINDOW,     "fitwindow",    1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(CENTERWINDOW,  "centerwindow", 1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(DISPLAYDOCTITLE, "displaydoctitle", 1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(NONFULLSCREENPAGEMODE, "nonfullscreenpagemode", 1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(DIRECTION,     "direction",    1, 1, 6,
		pdf_state_content | pdf_state_document)

pdf_gen1(VIEWAREA,      "viewarea",     1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(VIEWCLIP,      "viewclip",     1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(PRINTAREA,     "printarea",    1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(PRINTCLIP,     "printclip",    1, 1, 6,
		pdf_state_content | pdf_state_document)

pdf_gen1(OPENACTION,    "openaction",   1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(OPENMODE,      "openmode",     1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(BOOKMARKDEST,  "bookmarkdest", 1, 1, 6,
		pdf_state_content | pdf_state_document)
pdf_gen1(TRANSITION,    "transition",   1, 1, 6, pdf_state_all)
pdf_gen1(DURATION,      "duration",     1, 1, 6, pdf_state_all)
pdf_gen1(BASE,          "base",         1, 1, 6,
		pdf_state_content | pdf_state_document)

pdf_gen1(LAUNCHLINK_PARAMETERS, "launchlink:parameters", 1, 1, 6,
		pdf_state_all)
pdf_gen1(LAUNCHLINK_OPERATION,  "launchlink:operation",  1, 1, 6,
		pdf_state_all)
pdf_gen1(LAUNCHLINK_DEFAULTDIR, "launchlink:defaultdir", 1, 1, 6,
		pdf_state_all)


/*
 * ----------------------------------------------------------------------
 * Security (all dep6)
 * ----------------------------------------------------------------------
 */

pdf_gen1(USERPASSWORD,  "userpassword", 1, 1, 6, pdf_state_object)
pdf_gen1(MASTERPASSWORD,"masterpassword",1,1, 6, pdf_state_object)
pdf_gen1(PERMISSIONS,   "permissions",  1, 1, 6, pdf_state_object)


/*
 * ----------------------------------------------------------------------
 * Tagged PDF
 * ----------------------------------------------------------------------
 */

pdf_gen1(AUTOSPACE, "autospace", 1, 1, 0, pdf_state_all)


#undef	pdf_gen1
#undef	pdf_gen2
