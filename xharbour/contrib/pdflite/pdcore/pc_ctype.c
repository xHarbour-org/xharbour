/*---------------------------------------------------------------------------*
 |           PDFlib - A library for generating PDF on the fly                |
 +---------------------------------------------------------------------------+
 |          Copyright (c) 1997-2006 PDFlib GmbH. All rights reserved.        |
 *---------------------------------------------------------------------------*
 |          Proprietary source code -- do not redistribute!                  |
 *---------------------------------------------------------------------------*/

/* $Id$ */

#include "pc_ctype.h"


#undef	LOWER
#undef	UPPER
#undef	DIGIT
#undef	PUNCT
#undef	SPACE

#undef	OCT
#undef	HEX
#undef	DELIM
#undef	NUM0
#undef	PDFSP	

#define LOWER	PDC_ISLOWER
#define UPPER	PDC_ISUPPER
#define DIGIT	PDC_ISDIGIT
#define PUNCT	PDC_ISPUNCT
#define SPACE	PDC_ISSPACE
#define SPACE2	PDC_ISSPACE2

#define OCT	PDC_ISOCT
#define HEX	PDC_ISXDIGIT
#define DELIM	PDC_ISDELIM
#define NUM0	PDC_ISNUM0
#define PDFSP	PDC_ISPDFSP


const unsigned short pdc_ctype[256] =
{
    PDFSP,				/* 0x00 = NUL	*/

    0, 0, 0, 0, 0, 0, 0, 0,		/* 0x01 .. 0x08 */

    SPACE | PDFSP,			/* 0x09 = HT	*/
    SPACE | PDFSP,			/* 0x0A = NL	*/
    SPACE,				/* 0x0B = VT	*/
    SPACE | PDFSP,			/* 0x0C = FF	*/
    SPACE | PDFSP,			/* 0x0D = CR	*/
    0,					/* 0x0E		*/
    0,					/* 0x0F		*/

    0, 0, 0, 0, 0, 0, 0, 0,		/* 0x10 .. 0x17 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0x18 .. 0x1F */

    SPACE | SPACE2 | PDFSP,		/* 0x20 = ' '	*/
    PUNCT,				/* 0x21 = '!'	*/
    PUNCT,				/* 0x22 = '"'	*/
    PUNCT,				/* 0x23 = '#'	*/
    PUNCT,				/* 0x24 = '$'	*/
    PUNCT | DELIM,			/* 0x25 = '%'	*/
    PUNCT,				/* 0x26 = '&'	*/
    PUNCT,				/* 0x27 = '''	*/
    PUNCT | DELIM,			/* 0x28 = '('	*/
    PUNCT | DELIM,			/* 0x29 = ')'	*/
    PUNCT,				/* 0x2A = '*'	*/
    PUNCT | NUM0,			/* 0x2B = '+'	*/
    PUNCT,				/* 0x2C = ','	*/
    PUNCT | NUM0,			/* 0x2D = '-'	*/
    PUNCT | NUM0,			/* 0x2E = '.'	*/
    PUNCT | DELIM,			/* 0x2F = '/'	*/

    DIGIT | NUM0 | HEX | OCT,		/* 0x30 = '0'	*/
    DIGIT | NUM0 | HEX | OCT,		/* 0x31 = '1'	*/
    DIGIT | NUM0 | HEX | OCT,		/* 0x32 = '2'	*/
    DIGIT | NUM0 | HEX | OCT,		/* 0x33 = '3'	*/
    DIGIT | NUM0 | HEX | OCT,		/* 0x34 = '4'	*/
    DIGIT | NUM0 | HEX | OCT,		/* 0x35 = '5'	*/
    DIGIT | NUM0 | HEX | OCT,		/* 0x36 = '6'	*/
    DIGIT | NUM0 | HEX | OCT,		/* 0x37 = '7'	*/
    DIGIT | NUM0 | HEX,			/* 0x38 = '8'	*/
    DIGIT | NUM0 | HEX,			/* 0x39 = '9'	*/

    PUNCT,				/* 0x3A = ':'	*/
    PUNCT,				/* 0x3B = ';'	*/
    PUNCT | DELIM,			/* 0x3C = '<'	*/
    PUNCT,				/* 0x3D = '='	*/
    PUNCT | DELIM,			/* 0x3E = '>'	*/
    PUNCT,				/* 0x3F = '?'	*/
    PUNCT,				/* 0x40 = '@'	*/

    UPPER | HEX,			/* 0x41 = 'A'	*/
    UPPER | HEX,			/* 0x42 = 'B'	*/
    UPPER | HEX,			/* 0x43 = 'C'	*/
    UPPER | HEX,			/* 0x44 = 'D'	*/
    UPPER | HEX,			/* 0x45 = 'E'	*/
    UPPER | HEX,			/* 0x46 = 'F'	*/
    UPPER,				/* 0x47 = 'G'	*/
    UPPER,				/* 0x48 = 'H'	*/
    UPPER,				/* 0x49 = 'I'	*/
    UPPER,				/* 0x4A = 'J'	*/
    UPPER,				/* 0x4B = 'K'	*/
    UPPER,				/* 0x4C = 'L'	*/
    UPPER,				/* 0x4D = 'M'	*/
    UPPER,				/* 0x4E = 'N'	*/
    UPPER,				/* 0x4F = 'O'	*/

    UPPER,				/* 0x50 = 'P'	*/
    UPPER,				/* 0x51 = 'Q'	*/
    UPPER,				/* 0x52 = 'R'	*/
    UPPER,				/* 0x53 = 'S'	*/
    UPPER,				/* 0x54 = 'T'	*/
    UPPER,				/* 0x55 = 'U'	*/
    UPPER,				/* 0x56 = 'V'	*/
    UPPER,				/* 0x57 = 'W'	*/
    UPPER,				/* 0x58 = 'X'	*/
    UPPER,				/* 0x59 = 'Y'	*/
    UPPER,				/* 0x5A = 'Z'	*/

    PUNCT | DELIM,			/* 0x5B = '['	*/
    PUNCT,				/* 0x5C = '\'	*/
    PUNCT | DELIM,			/* 0x5D = ']'	*/
    PUNCT,				/* 0x5E = '^'	*/
    PUNCT,				/* 0x5F = '_'	*/
    PUNCT,				/* 0x60 = '`'	*/

    LOWER | HEX,			/* 0x61 = 'a'	*/
    LOWER | HEX,			/* 0x62 = 'b'	*/
    LOWER | HEX,			/* 0x63 = 'c'	*/
    LOWER | HEX,			/* 0x64 = 'd'	*/
    LOWER | HEX,			/* 0x65 = 'e'	*/
    LOWER | HEX,			/* 0x66 = 'f'	*/
    LOWER,				/* 0x67 = 'g'	*/
    LOWER,				/* 0x68 = 'h'	*/
    LOWER,				/* 0x69 = 'i'	*/
    LOWER,				/* 0x6A = 'j'	*/
    LOWER,				/* 0x6B = 'k'	*/
    LOWER,				/* 0x6C = 'l'	*/
    LOWER,				/* 0x6D = 'm'	*/
    LOWER,				/* 0x6E = 'n'	*/
    LOWER,				/* 0x6F = 'o'	*/

    LOWER,				/* 0x70 = 'p'	*/
    LOWER,				/* 0x71 = 'q'	*/
    LOWER,				/* 0x72 = 'r'	*/
    LOWER,				/* 0x73 = 's'	*/
    LOWER,				/* 0x74 = 't'	*/
    LOWER,				/* 0x75 = 'u'	*/
    LOWER,				/* 0x76 = 'v'	*/
    LOWER,				/* 0x77 = 'w'	*/
    LOWER,				/* 0x78 = 'x'	*/
    LOWER,				/* 0x79 = 'y'	*/
    LOWER,				/* 0x7A = 'z'	*/

    PUNCT | DELIM,			/* 0x7B = '{'	*/
    PUNCT,				/* 0x7C = '|'	*/
    PUNCT | DELIM,			/* 0x7D = '}'	*/
    PUNCT,				/* 0x7E = '~'	*/
    0,					/* 0x7F		*/

    0, 0, 0, 0, 0, 0, 0, 0,		/* 0x80 .. 0x87 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0x88 .. 0x8F */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0x90 .. 0x97 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0x98 .. 0x9F */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xA0 .. 0xA7 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xA8 .. 0xAF */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xB0 .. 0xB7 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xB8 .. 0xBF */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xC0 .. 0xC7 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xC8 .. 0xCF */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xD0 .. 0xD7 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xD8 .. 0xDF */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xE0 .. 0xE7 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xE8 .. 0xEF */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xF0 .. 0xF7 */
    0, 0, 0, 0, 0, 0, 0, 0,		/* 0xF8 .. 0xFF */
}; /* pdc_ctype */
