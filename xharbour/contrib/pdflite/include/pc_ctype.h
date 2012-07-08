/*---------------------------------------------------------------------------*
 |           PDFlib - A library for generating PDF on the fly                |
 +---------------------------------------------------------------------------+
 |          Copyright (c) 1997-2006 PDFlib GmbH. All rights reserved.        |
 *---------------------------------------------------------------------------*
 |          Proprietary source code -- do not redistribute!                  |
 *---------------------------------------------------------------------------*/

/* $Id$ */

#ifndef PC_CTYPE_H_INCLUDED
#define PC_CTYPE_H_INCLUDED

#include "pc_util.h"

extern const unsigned short pdc_ctype[];

#define PDC_ISLOWER	0x0001
#define PDC_ISUPPER	0x0002
#define PDC_ISDIGIT	0x0004
#define PDC_ISPUNCT	0x0008
#define PDC_ISSPACE	0x0010
#define PDC_ISSPACE2	0x0020

#define PDC_ISOCT	0x0100
#define PDC_ISXDIGIT	0x0200
#define PDC_ISDELIM	0x0400
#define PDC_ISNUM0	0x0800		/* '+'  '-'  '.'  '0'..'9'	*/
#define PDC_ISPDFSP	0x1000		/* ' ' NUL HT NL CR FF		*/


#define PDC_CONVCHAR(c)		((pdc_byte) c)


/* these are the locale-free replacements for the standard library
** isXXX() functions.
*/
#define pdc_isalnum(c)			\
    ((pdc_ctype[PDC_CONVCHAR(c)] &	\
	(PDC_ISLOWER | PDC_ISUPPER | PDC_ISDIGIT)) != 0)

#define pdc_isalpha(c)	\
    ((pdc_ctype[PDC_CONVCHAR(c)] & (PDC_ISLOWER | PDC_ISUPPER)) != 0)

#define pdc_isdigit(c)	\
    ((pdc_ctype[PDC_CONVCHAR(c)] & PDC_ISDIGIT) != 0)

#define pdc_islower(c)	\
    ((pdc_ctype[PDC_CONVCHAR(c)] & PDC_ISLOWER) != 0)

#define pdc_isprint(c)			\
    ((pdc_ctype[PDC_CONVCHAR(c)] &	\
	(PDC_ISLOWER | PDC_ISUPPER |	\
	 PDC_ISDIGIT | PDC_ISPUNCT | PDC_ISSPACE2)) != 0)

#define pdc_ispunct(c)	\
    ((pdc_ctype[PDC_CONVCHAR(c)] & PDC_ISPUNCT) != 0)

#define pdc_isspace(c)	\
    ((pdc_ctype[PDC_CONVCHAR(c)] & PDC_ISSPACE) != 0)

#define pdc_isupper(c)	\
    ((pdc_ctype[PDC_CONVCHAR(c)] & PDC_ISUPPER) != 0)

#define pdc_isxdigit(c)	\
    ((pdc_ctype[PDC_CONVCHAR(c)] & PDC_ISXDIGIT) != 0)


#define pdc_tolower(c)	\
	(pdc_isupper(c) ? ((pdc_byte) ((c) + 0x20)) : (pdc_byte) (c))


#define pdc_toupper(c)	\
	(pdc_islower(c) ? ((pdc_byte) ((c) - 0x20)) : (pdc_byte) (c))


/* these macros are for the various flavors of the token scanner. they
** expect ASCII input even on EBCDIC platforms (thus the "_a" suffix),
** and they implement special rules for PDF character classification.
*/
#define pdc_isalnum_a(c)               \
    ((pdc_ctype[(pdc_byte) (c)] &      \
        (PDC_ISLOWER | PDC_ISUPPER | PDC_ISDIGIT)) != 0)

#define pdc_isalpha_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & (PDC_ISLOWER | PDC_ISUPPER)) != 0)

#define pdc_isdecdt_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & PDC_ISDIGIT) != 0)

#define pdc_isdelim_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & PDC_ISDELIM) != 0)

#define pdc_ishexdt_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & PDC_ISXDIGIT) != 0)

#define pdc_islower_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & PDC_ISLOWER) != 0)

#define pdc_isnum0_a(c)		\
    ((pdc_ctype[(pdc_byte) (c)] & PDC_ISNUM0) != 0)

#define pdc_isoctdt_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & PDC_ISOCT) != 0)

#define pdc_isspace_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & PDC_ISPDFSP) != 0)

#define pdc_isspecial_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & (PDC_ISPDFSP | PDC_ISDELIM)) != 0)

#define pdc_isupper_a(c)	\
    ((pdc_ctype[(pdc_byte) (c)] & PDC_ISUPPER) != 0)

#define pdc_isregular_a(c)	\
	((c) != -1 && !pdc_isspecial_a((pdc_byte) (c)))

#define pdc_tolower_a(c)  \
        (pdc_isupper_a(c) ? ((pdc_byte) ((c) + 0x20)) : (pdc_byte) (c))

#define pdc_toupper_a(c)  \
        (pdc_islower_a(c) ? ((pdc_byte) ((c) - 0x20)) : (pdc_byte) (c))

#endif	/* PC_CTYPE_H_INCLUDED */
