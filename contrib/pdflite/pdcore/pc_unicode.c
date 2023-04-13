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
 * PDFlib Unicode converting routines
 *
 */

#define PC_UNICODE_C

#include "pc_util.h"

#if defined(WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif /* WIN32 */

/*
 *  The following source is based on Unicode's original source
 *  code ConvertUTF.c. It has been adapted to PDFlib programming
 *  conventions.
 *
 *  The original file had the following notice:
 *
 *      Copyright 2001 Unicode, Inc.
 *
 *      Limitations on Rights to Redistribute This Code
 *
 *      Author: Mark E. Davis, 1994.
 *      Rev History: Rick McGowan, fixes & updates May 2001.
 *
 *
 *  Functions for conversions between UTF32, UTF-16, and UTF-8.
 *  These funtions forming a complete set of conversions between
 *  the three formats. UTF-7 is not included here.
 *
 *  Each of these routines takes pointers to input buffers and output
 *  buffers. The input buffers are const.
 *
 *  Each routine converts the text between *sourceStart and sourceEnd,
 *  putting the result into the buffer between *targetStart and
 *  targetEnd. Note: the end pointers are *after* the last item: e.g.
 *  *(sourceEnd - 1) is the last item.
 *
 *  The return result indicates whether the conversion was successful,
 *  and if not, whether the problem was in the source or target buffers.
 *  (Only the first encountered problem is indicated.)
 *
 *  After the conversion, *sourceStart and *targetStart are both
 *  updated to point to the end of last text successfully converted in
 *  the respective buffers.
 *
 *  Input parameters:
 *      sourceStart - pointer to a pointer to the source buffer.
 *              The contents of this are modified on return so that
 *              it points at the next thing to be converted.
 *      targetStart - similarly, pointer to pointer to the target buffer.
 *      sourceEnd, targetEnd - respectively pointers to the ends of the
 *              two buffers, for overflow checking only.
 *
 *  These conversion functions take a pdc_convers_flags argument. When this
 *  flag is set to strict, both irregular sequences and isolated surrogates
 *  will cause an error.  When the flag is set to lenient, both irregular
 *  sequences and isolated surrogates are converted.
 *
 *  Whether the flag is strict or lenient, all illegal sequences will cause
 *  an error return. This includes sequences such as: <F4 90 80 80>, <C0 80>,
 *  or <A0> in UTF-8, and values above 0x10FFFF in UTF-32. Conformant code
 *  must check for illegal sequences.
 *
 *  When the flag is set to lenient, characters over 0x10FFFF are converted
 *  to the replacement character; otherwise (when the flag is set to strict)
 *  they constitute an error.
 *
 *  Output parameters:
 *      The value "sourceIllegal" is returned from some routines if the input
 *      sequence is malformed.  When "sourceIllegal" is returned, the source
 *      value will point to the illegal value that caused the problem. E.g.,
 *      in UTF-8 when a sequence is malformed, it points to the start of the
 *      malformed sequence.
 *
 *  Author: Mark E. Davis, 1994.
 *  Rev History: Rick McGowan, fixes & updates May 2001.
 *
 */

/*
 * The following 4 definitions are compiler-specific.
 * The C standard does not guarantee that wchar_t has at least
 * 16 bits, so wchar_t is no less portable than unsigned short!
 * All should be unsigned values to avoid sign extension during
 * bit mask & shift operations.
 */

/* Unicode original:
typedef unsigned long   UTF32;   at least 32 bits
typedef unsigned short  UTF16;   at least 16 bits
*/

typedef unsigned int    UTF32;  /* 32 bits */
typedef unsigned short  UTF16;  /* 16 bits */
typedef unsigned char   UTF8;   /* typically 8 bits */

/* Some fundamental constants */
#define UNI_SUR_HIGH_START      (UTF32)0xD800
#define UNI_SUR_HIGH_END        (UTF32)0xDBFF
#define UNI_SUR_LOW_START       (UTF32)0xDC00
#define UNI_SUR_LOW_END         (UTF32)0xDFFF
#define UNI_REPLACEMENT_CHAR    (UTF32)0x0000FFFD
#define UNI_MAX_BMP             (UTF32)0x0000FFFF
#define UNI_MAX_UTF16           (UTF32)0x0010FFFF
#define UNI_MAX_UTF32           (UTF32)0x7FFFFFFF

static const int halfShift      = 10; /* used for shifting by 10 bits */

static const UTF32 halfBase     = 0x0010000UL;
static const UTF32 halfMask     = 0x3FFUL;


/* --------------------------------------------------------------------- */

static pdc_convers_result
pdc_convertUTF32toUTF16 (
                UTF32** sourceStart, const UTF32* sourceEnd,
                UTF16** targetStart, const UTF16* targetEnd,
                const pdc_convers_flags flags) {
    pdc_convers_result result = conversionOK;
    UTF32* source = *sourceStart;
    UTF16* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch;
        if (target >= targetEnd) {
            result = targetExhausted; break;
        }
        ch = *source++;
        if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
            if ((flags == strictConversion) &&
                (ch >= UNI_SUR_HIGH_START &&
                 ch <= UNI_SUR_LOW_END)) {
                --source; /* return to the illegal value itself */
                result = sourceIllegal;
                break;
            } else {
                *target++ = (UTF16) ch;     /* normal case */
            }
        } else if (ch > UNI_MAX_UTF16) {
            if (flags == strictConversion) {
                result = sourceIllegal;
            } else {
                *target++ = UNI_REPLACEMENT_CHAR;
            }
        } else {
            /* target is a character in range 0xFFFF - 0x10FFFF. */
            if (target + 1 >= targetEnd) {
                result = targetExhausted;
                break;
            }
            ch -= halfBase;
            *target++ = (UTF16) ((ch >> halfShift) + UNI_SUR_HIGH_START);
            *target++ = (UTF16) ((ch & halfMask) + UNI_SUR_LOW_START);
        }
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* --------------------------------------------------------------------- */

static pdc_convers_result
pdc_convertUTF16toUTF32 (
                UTF16** sourceStart, UTF16* sourceEnd,
                UTF32** targetStart, const UTF32* targetEnd,
                const pdc_convers_flags flags) {
    pdc_convers_result result = conversionOK;
    UTF16* source = *sourceStart;
    UTF32* target = *targetStart;
    UTF32 ch, ch2;
    while (source < sourceEnd) {
        ch = *source++;
        if (ch >= UNI_SUR_HIGH_START &&
            ch <= UNI_SUR_HIGH_END &&
            source < sourceEnd) {
            ch2 = *source;
            if (ch2 >= UNI_SUR_LOW_START && ch2 <= UNI_SUR_LOW_END) {
                ch = ((ch - UNI_SUR_HIGH_START) << halfShift)
                      + (ch2 - UNI_SUR_LOW_START) + halfBase;
                ++source;
            } else if (flags == strictConversion) {
                /* it's an unpaired high surrogate */
                --source; /* return to the illegal value itself */
                result = sourceIllegal;
                break;
            }
        } else if ((flags == strictConversion) &&
                   (ch >= UNI_SUR_LOW_START &&
                    ch <= UNI_SUR_LOW_END)) {
            /* an unpaired low surrogate */
            --source; /* return to the illegal value itself */
            result = sourceIllegal;
            break;
        }
        if (target >= targetEnd) {
            result = targetExhausted;
            break;
        }
        *target++ = ch;
    }
    *sourceStart = source;
    *targetStart = target;
#ifdef CVTUTF_DEBUG
if (result == sourceIllegal) {
    fprintf(stderr, "pdc_convertUTF16toUTF32 illegal seq 0x%04x,%04x\n",
            ch, ch2);
    fflush(stderr);
}
#endif
    return result;
}

/* --------------------------------------------------------------------- */

/*
 * Index into the table below with the first byte of a UTF-8 sequence to
 * get the number of trailing bytes that are supposed to follow it.
 */
static const char trailingBytesForUTF8[256] = {
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
};

#if 0
static const char
pdc_get_trailingBytesForUTF8(int i) {
    return (trailingBytesForUTF8[i]);
}
#endif

/*
 * Magic values subtracted from a buffer value during UTF8 conversion.
 * This table contains as many values as there might be trailing bytes
 * in a UTF-8 sequence.
 */
static const UTF32 offsetsFromUTF8[6] = {
    0x00000000UL, 0x00003080UL, 0x000E2080UL,
    0x03C82080UL, 0xFA082080UL, 0x82082080UL
};

/*
 * Once the bits are split out into bytes of UTF-8, this is a mask OR-ed
 * into the first byte, depending on how many bytes follow.  There are
 * as many entries in this table as there are UTF-8 sequence types.
 * (I.e., one byte sequence, two byte... six byte sequence.)
 */
static const UTF8 firstByteMark[7] = {
    0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC
};

/* --------------------------------------------------------------------- */

/* The interface converts a whole buffer to avoid function-call overhead.
 * Constants have been gathered. Loops & conditionals have been removed as
 * much as possible for efficiency, in favor of drop-through switches.
 * (See "Note A" at the bottom of the file for equivalent code.)
 * If your compiler supports it, the "pdc_islegalUTF8" call can be turned
 * into an inline function.
 */

/* --------------------------------------------------------------------- */

static pdc_convers_result
pdc_convertUTF16toUTF8 (
                UTF16** sourceStart, const UTF16* sourceEnd,
                UTF8** targetStart, const UTF8* targetEnd,
                const pdc_convers_flags flags) {
    pdc_convers_result result = conversionOK;
    UTF16* source = *sourceStart;
    UTF8* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch;
        unsigned short bytesToWrite = 0;
        const UTF32 byteMask = 0xBF;
        const UTF32 byteMark = 0x80;
        ch = *source++;
        /* If we have a surrogate pair, convert to UTF32 first. */
        if (ch >= UNI_SUR_HIGH_START &&
            ch <= UNI_SUR_HIGH_END &&
            source < sourceEnd) {
            UTF32 ch2 = *source;
            if (ch2 >= UNI_SUR_LOW_START &&
                ch2 <= UNI_SUR_LOW_END) {
                ch = ((ch - UNI_SUR_HIGH_START) << halfShift)
                        + (ch2 - UNI_SUR_LOW_START) + halfBase;
                ++source;
            } else if (flags == strictConversion) {
                /* it's an unpaired high surrogate */
                --source; /* return to the illegal value itself */
                result = sourceIllegal;
                break;
            }
        } else if ((flags == strictConversion) &&
                   (ch >= UNI_SUR_LOW_START &&
                    ch <= UNI_SUR_LOW_END)) {
            --source; /* return to the illegal value itself */
            result = sourceIllegal;
            break;
        }
        /* Figure out how many bytes the result will require */
        if (ch < (UTF32)0x80) {                 bytesToWrite = 1;
        } else if (ch < (UTF32)0x800) {         bytesToWrite = 2;
        } else if (ch < (UTF32)0x10000) {       bytesToWrite = 3;
        } else if (ch < (UTF32)0x200000) {      bytesToWrite = 4;
        } else {                                bytesToWrite = 2;
                                                ch = UNI_REPLACEMENT_CHAR;
        }

        target += bytesToWrite;
        if (target > targetEnd) {
            target -= bytesToWrite; result = targetExhausted; break;
        }
        switch (bytesToWrite) { /* note: everything falls through. */
            case 4: *--target = (UTF8) ((ch | byteMark) & byteMask); ch >>= 6;
            case 3: *--target = (UTF8) ((ch | byteMark) & byteMask); ch >>= 6;
            case 2: *--target = (UTF8) ((ch | byteMark) & byteMask); ch >>= 6;
            case 1: *--target = (UTF8) (ch | firstByteMark[bytesToWrite]);
        }
        target += bytesToWrite;
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* --------------------------------------------------------------------- */

/*
 * Utility routine to tell whether a sequence of bytes is legal UTF-8.
 * This must be called with the length pre-determined by the first byte.
 * If not calling this from pdc_convertUTF8to*, then the length can be set by:
 *      length = trailingBytesForUTF8[*source]+1;
 * and the sequence is illegal right away if there aren't that many bytes
 * available.
 * If presented with a length > 4, this returns pdc_false.  The Unicode
 * definition of UTF-8 goes up to 4-byte sequences.
 */

static pdc_bool
pdc_islegalUTF8(UTF8 *source, int length) {
    UTF8 a;
    UTF8 *srcptr = source+length;
    switch (length) {
    default: return pdc_false;
        /* Everything else falls through when "pdc_true"... */
    case 4: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return pdc_false;
    case 3: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return pdc_false;
    case 2: if ((a = (*--srcptr)) > 0xBF) return pdc_false;
        switch (*source) {
            /* no fall-through in this inner switch */
            case 0xE0: if (a < 0xA0) return pdc_false; break;
            case 0xF0: if (a < 0x90) return pdc_false; break;
            case 0xF4: if (a > 0x8F) return pdc_false; break;
            default:  if (a < 0x80) return pdc_false;
        }
    case 1: if (*source >= 0x80 && *source < 0xC2) return pdc_false;
            if (*source > 0xF4) return pdc_false;
    }
    return pdc_true;
}

/* --------------------------------------------------------------------- */

/*
 * Exported function to return whether a UTF-8 sequence is legal or not.
 * This is not used here; it's just exported.
 */
#if 0
static pdc_bool pdc_islegalUTF8sequence(UTF8 *source, UTF8 *sourceEnd) {
    int length = trailingBytesForUTF8[*source]+1;
    if (source+length > sourceEnd) {
        return pdc_false;
    }
    return pdc_islegalUTF8(source, length);
}
#endif

/* --------------------------------------------------------------------- */

static pdc_convers_result
pdc_convertUTF8toUTF16 (
                UTF8** sourceStart, UTF8* sourceEnd,
                UTF16** targetStart, const UTF16* targetEnd,
                const pdc_convers_flags flags) {
    pdc_convers_result result = conversionOK;
    UTF8* source = *sourceStart;
    UTF16* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch = 0L;
        unsigned short extraBytesToRead = trailingBytesForUTF8[*source];
        if (source + extraBytesToRead >= sourceEnd) {
            result = sourceExhausted;
            break;
        }
        /* Do this check whether lenient or strict */
        if (! pdc_islegalUTF8(source, extraBytesToRead+1)) {
            result = sourceIllegal;
            break;
        }
        /*
         * The cases all fall through. See "Note A" below.
         */
        switch (extraBytesToRead) {
            case 3: ch += *source++; ch <<= 6;
            case 2: ch += *source++; ch <<= 6;
            case 1: ch += *source++; ch <<= 6;
            case 0: ch += *source++;
        }
        ch -= offsetsFromUTF8[extraBytesToRead];

        if (target >= targetEnd) {
            result = targetExhausted;
            break;
        }
        if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
            if ((flags == strictConversion) &&
                (ch >= UNI_SUR_HIGH_START &&
                 ch <= UNI_SUR_LOW_END)) {
                --source; /* return to the illegal value itself */
                result = sourceIllegal;
                break;
            } else {
                *target++ = (UTF16) ch;     /* normal case */
            }
        } else if (ch > UNI_MAX_UTF16) {
            if (flags == strictConversion) {
                    result = sourceIllegal;
                    source -= extraBytesToRead; /* return to the start */
            } else {
                    *target++ = UNI_REPLACEMENT_CHAR;
            }
        } else {
            /* target is a character in range 0xFFFF - 0x10FFFF. */
            if (target + 1 >= targetEnd) {
                    result = targetExhausted;
                    break;
            }
            ch -= halfBase;
            *target++ = (UTF16) ((ch >> halfShift) + UNI_SUR_HIGH_START);
            *target++ = (UTF16) ((ch & halfMask) + UNI_SUR_LOW_START);
        }
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* --------------------------------------------------------------------- */

static pdc_convers_result
pdc_convertUTF32toUTF8 (
                UTF32** sourceStart, const UTF32* sourceEnd,
                UTF8** targetStart, const UTF8* targetEnd,
                const pdc_convers_flags flags) {
    pdc_convers_result result = conversionOK;
    UTF32* source = *sourceStart;
    UTF8* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch;
        unsigned short bytesToWrite = 0;
        const UTF32 byteMask = 0x000000BF;
        const UTF32 byteMark = 0x00000080;
        ch = *source++;
        /* surrogates of any stripe are not legal UTF32 characters */
        if (flags == strictConversion ) {
            if ((ch >= UNI_SUR_HIGH_START) && (ch <= UNI_SUR_LOW_END)) {
                --source; /* return to the illegal value itself */
                result = sourceIllegal;
                break;
            }
        }
        /* Figure out how many bytes the result will require */
        if (ch < (UTF32)0x80) {                 bytesToWrite = 1;
        } else if (ch < (UTF32)0x800) {         bytesToWrite = 2;
        } else if (ch < (UTF32)0x10000) {       bytesToWrite = 3;
        } else if (ch < (UTF32)0x200000) {      bytesToWrite = 4;
        } else {                                bytesToWrite = 2;
                                                ch = UNI_REPLACEMENT_CHAR;
        }

        target += bytesToWrite;
        if (target > targetEnd) {
            target -= bytesToWrite; result = targetExhausted; break;
        }
        switch (bytesToWrite) { /* note: everything falls through. */
            case 4: *--target = (UTF8) ((ch | byteMark) & byteMask); ch >>= 6;
            case 3: *--target = (UTF8) ((ch | byteMark) & byteMask); ch >>= 6;
            case 2: *--target = (UTF8) ((ch | byteMark) & byteMask); ch >>= 6;
            case 1: *--target = (UTF8) (ch | firstByteMark[bytesToWrite]);
        }
        target += bytesToWrite;
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* --------------------------------------------------------------------- */

static pdc_convers_result
pdc_convertUTF8toUTF32 (
                UTF8** sourceStart, UTF8* sourceEnd,
                UTF32** targetStart, const UTF32* targetEnd,
                const pdc_convers_flags flags) {
    pdc_convers_result result = conversionOK;
    UTF8* source = *sourceStart;
    UTF32* target = *targetStart;

    (void) flags;

    while (source < sourceEnd) {
        UTF32 ch = 0;
        unsigned short extraBytesToRead = trailingBytesForUTF8[*source];
        if (source + extraBytesToRead >= sourceEnd) {
            result = sourceExhausted; break;
        }
        /* Do this check whether lenient or strict */
        if (! pdc_islegalUTF8(source, extraBytesToRead+1)) {
            result = sourceIllegal;
            break;
        }
        /*
         * The cases all fall through. See "Note A" below.
         */
        switch (extraBytesToRead) {
            case 3: ch += *source++; ch <<= 6;
            case 2: ch += *source++; ch <<= 6;
            case 1: ch += *source++; ch <<= 6;
            case 0: ch += *source++;
        }
        ch -= offsetsFromUTF8[extraBytesToRead];

        if (target >= targetEnd) {
            result = targetExhausted;
            break;
        }
        if (ch <= UNI_MAX_UTF32) {
            *target++ = ch;
        } else if (ch > UNI_MAX_UTF32) {
            *target++ = UNI_REPLACEMENT_CHAR;
        } else {
            if (target + 1 >= targetEnd) {
                result = targetExhausted;
                break;
            }
            ch -= halfBase;
            *target++ = (ch >> halfShift) + UNI_SUR_HIGH_START;
            *target++ = (ch & halfMask) + UNI_SUR_LOW_START;
        }
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* ---------------------------------------------------------------------

        Note A.
        The fall-through switches in UTF-8 reading code save a
        temp variable, some decrements & conditionals.  The switches
        are equivalent to the following loop:
                {
                        int tmpBytesToRead = extraBytesToRead+1;
                        do {
                                ch += *source++;
                                --tmpBytesToRead;
                                if (tmpBytesToRead) ch <<= 6;
                        } while (tmpBytesToRead > 0);
                }
        In UTF-8 writing code, the switches on "bytesToWrite" are
        similarly unrolled loops.

   --------------------------------------------------------------------- */

const char *
pdc_get_textformat(int textformat)
{
    return pdc_get_keyword(textformat, pdc_textformat_keylist);
}

static const pdc_keyconn pdc_utfformat_keylist[] =
{
    {"8",     pdc_utf8},
    {"16",    pdc_utf16},
    {"32",    pdc_utf32},
    {NULL, 0}
};


/*
 *  pdc_convert_string converts a arbitrary encoded string (maybe UTF) to
 *  another encoded string.
 *
 *  The new converted string is allocated and terminated by the required
 *  number of zeros.
 *
 *  The caller is responsible for freeing the resulting string buffer.
 *
 *
 *  LBP: low byte picking
 *
 *  Input-Parameter:
 *
 *  inutf:      input string format (see pc_unicode.h):
 *
 *              pdc_auto:     If codepage != 0:
 *                                see above.
 *                            Otherwise:
 *                            If a BOM is recognized:
 *                                pdc_utf8 or pdc_utf16xx resp.
 *                            Otherwise if input encoding <inev> is specified
 *                            and flag PDC_CONV_FORCEUTF16 not set:
 *                                pdc_bytes
 *                            Otherwise:
 *                                pdc_utf16
 *
 *              pdc_auto2:    If input encoding is not specified:
 *                                pdc_utf16
 *                            Otherwise after successfull LBP:
 *                                pdc_auto
 *                            Otherwise:
 *                                pdc_utf16
 *
 *              pdc_bytes:    8-bit string. Encoding is <inev> if specified.
 *
 *              pdc_bytes2:   After successfull LBP:
 *                                pdc_bytes
 *                            Otherwise:
 *                                pdc_utf16
 *
 *              pdc_utf8:     UTF-8 formatted string.
 *
 *              pdc_ebcdicutf8: EBCDIC-UTF-8 formatted string.
 *
 *              pdc_utf16:    If a UTF16 BOM is recognized:
 *                                pdc_utf16be or pdc_utf16le
 *                            Otherwise UTF-16 machine byte ordered string.
 *
 *              pdc_utf16be   UTF-16 big endian formatted string.
 *
 *              pdc_utf16le   UTF-16 little endian formatted string.
 *
 *  codepage:   OEM multi byte code-page number. If > 0 and
 *              <inutf> = pdc_auto, text will be converted to UTF-16.
 *
 *  inev:       Encoding vector for input pdc_bytes string.
 *
 *  glyphtab:   Mapping table for character reference names
 *
 *  tabsize:    Size of mapping table
 *
 *  replchar:   Treatment of non resolvable character references:
 *              >= 0: replacement character
 *              == text_error: error message
 *              == text_nocheck: will be ignored
 *              (see also pdc_charref2unicodelist())
 *
 *  instring:   Input string.
 *
 *  inlen:      Length of input string in byte.
 *
 *  oututf:     Target format for output string.
 *              pdc_auto, pdc_auto2 and pdc_bytes2 are not supported.
 *
 *  outev:      Encoding vector for output pdc_bytes string.
 *
 *  flags:      PDC_CONV_FORCEUTF16:
 *              In the case of <inutf> = pdc_auto[2] and <inev> != NULL
 *              <inutf> = pdc_utf16 will be forced.
 *
 *              PDC_CONV_TRY7BYTES:
 *              UTF-8 output strings will have no BOM if each byte
 *              is smaller than x80.
 *              *oututf: pdc_byte.
 *
 *              PDC_CONV_TRYBYTES:
 *              UTF-UTF-16xx output strings will be converted by LBP
 *              if each character is smaller than x0100.
 *              *oututf: pdc_byte.
 *
 *              PDC_CONV_WITHBOM:
 *              UTF-8 or UTF-UTF-16xx output strings will be armed
 *              with an appropriate BOM.
 *
 *              PDC_CONV_NOBOM:
 *              In UTF-8 or UTF-UTF-16xx output strings any BOM sequence
 *              will be removed. PDC_CONV_WITHBOM is dominant.
 *
 *              PDC_CONV_AUTOBOM:
 *              BOM sequence will be set automatically if input string
 *              has a BOM.
 *
 *              PDC_CONV_ANALYZE:
 *              Only analyzing BOMs of input string and dissolving auto
 *              textformats.
 *
 *              PDC_CONV_TMPALLOC
 *              Temporary memory functions (pdc_malloc_tmp) are used
 *              rather than pdc_malloc etc.
 *
 *              PDC_CONV_HTMLCHAR
 *              If input encoding vector is specified HTML character
 *              entities will be substituted.
 *
 *              PDC_CONV_NEWALLOC
 *              Input string must be allocated at first to guarantee
 *              pointer alignment.
 *
 *              PDC_CONV_INFLATE
 *              Invalid UTF-8 to UTF-16xx conversion will not cause
 *              an exception but rather an inflated byte string will
 *              be output.
 *
 *              PDC_CONV_ESCSEQU
 *              Unicode sequences framed by escape character U+001B
 *              (found in PDF text strings) will be skipped.
 *
 *              PDC_CONV_BSSEQU
 *              Code sequences beginning with backslash '\'
 *              will be substituted.
 *
 *              PDC_CONV_ENCERROR
 *              If an 8-bit code cannot be converted to Unicode by <inev>
 *              or a Unicode cannot be converted to an 8-bit code by <outev>
 *              an error message will be created.
 *
 *              PDC_CONV_KEEPLBCHAR
 *              In the case of PDC_CONV_ENCERROR relevant characters for
 *              line breaking do not lead to an error message.
 *
 *              PDC_CONV_LOGGING
 *              Enables logging.
 *
 *  verbose:    Error messages are put out. Otherwise they are saved only.
 *
 *  Output-Parameter:
 *
 *  oututf:     Reached format for output string.
 *
 *  outstring:  Pointer of allocated output string
 *
 *  outlen:     Length of output string.
 *
 */

#if defined(_MSC_VER) && defined(_MANAGED)
#pragma unmanaged
#endif
int
pdc_convert_string(pdc_core *pdc,
                   pdc_text_format inutf, int codepage,
                   pdc_encodingvector *inev,
                   pdc_byte *instring, int inlen,
                   pdc_text_format *oututf_p, pdc_encodingvector *outev,
                   pdc_byte **outstring, int *outlen, int flags,
                   pdc_bool verbose)
{
    /* text_nocheck: see bug #1664 */
    return pdc_convert_textstring(pdc, inutf, codepage, inev,
                   NULL, 0, text_nocheck, instring, inlen, oututf_p, outev,
                   outstring, outlen, flags, verbose);
}

int
pdc_convert_textstring(pdc_core *pdc,
                   pdc_text_format inutf, int codepage,
                   pdc_encodingvector *inev,
                   const pdc_glyph_tab *glyphtab, int tabsize, int replchar,
                   pdc_byte *instring, int inlen,
                   pdc_text_format *oututf_p, pdc_encodingvector *outev,
                   pdc_byte **outstring, int *outlen, int flags,
                   pdc_bool verbose)
{
    static const char *fn = "pdc_convert_textstring";
    pdc_bool logg = flags & PDC_CONV_LOGGING;
    const char *stemp1 = NULL, *stemp2 = NULL;
    char sbuf[64];
    pdc_text_format oututf = *oututf_p;
    pdc_text_format oututf_s;
    pdc_ushort *usinstr = (pdc_ushort *) instring;
    pdc_ushort uv = 0;
    pdc_byte *instr = NULL;
    pdc_bool inalloc = pdc_false;
    pdc_bool hasbom = pdc_false;
    pdc_bool toswap = pdc_false;
    int errcode = 0;
    int i, j, n, len = 0;

    (void) glyphtab;
    (void) tabsize;
    (void) replchar;

    if (logg || pdc_logg_is_enabled(pdc, 5, trc_encoding))
    {
        pdc_logg(pdc, "\n");
        if (!logg)
            pdc_logg(pdc, "\t\ttext string of length %d will be converted...\n",
                     inlen);
        logg = pdc_true;
    }

    if (logg)
    {
        pdc_logg(pdc, "\t\tinput textformat for string conversion: %s\n",
                 pdc_get_keyword(inutf, pdc_textformat_keylist));

        if (inev != NULL)
            pdc_logg(pdc, "\t\tinput encoding: %s\n", inev->apiname);

        if (outev != NULL)
            pdc_logg(pdc, "\t\toutput encoding: %s\n", outev->apiname);
    }

    /* prophylactic */
    if (!inlen)
    {
        instring = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
             pdc_calloc_tmp(pdc, 4, fn, NULL, NULL) :
             pdc_calloc(pdc, 4, fn));

        inalloc = pdc_true;
    }
    else if ((flags & PDC_CONV_NEWALLOC) ||
             (flags & PDC_CONV_TMPALLOC) ||
             (flags & PDC_CONV_BSSEQU))
    {
        instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
             pdc_calloc_tmp(pdc, (size_t) (inlen + 2), fn, NULL, NULL) :
             pdc_calloc(pdc, (size_t) (inlen + 2), fn));
        memcpy(instr, instring, (size_t) inlen);

        inalloc = pdc_true;
        instring = instr;
        instr = NULL;
        usinstr = (pdc_ushort *) instring;
    }

    switch(inutf)
    {
        /* analyzing 2 byte textformat */
        case pdc_auto2:
        case pdc_bytes2:
        if ((inutf == pdc_auto2 &&
             (inev == NULL || (flags & PDC_CONV_FORCEUTF16))) ||
            (flags & PDC_CONV_ANALYZE))
        {
            inutf = pdc_utf16;
        }
        else
        {
            if (logg)
                pdc_logg(pdc, "\t\ttry to pick low bytes\n");

            len = inlen / 2;
            if (2 * len != inlen)
            {
                errcode = PDC_E_CONV_ILLUTF16;
                goto PDC_CONV_ERROR;
            }
            for (i = 0; i < len; i++)
                if (usinstr[i] > PDC_UNICODE_MAXLATIN1)
                    break;

            /* low byte picking */
            if (i == len)
            {
                instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
                     pdc_calloc_tmp(pdc, (size_t) (len + 2), fn, NULL, NULL) :
                     pdc_calloc(pdc, (size_t) (len + 2), fn));
                for (i = 0; i < len; i++)
                    instr[i] = (pdc_byte) usinstr[i];

                if (inalloc)
                {
                    if (flags & PDC_CONV_TMPALLOC)
                        pdc_free_tmp(pdc, instring);
                    else
                        pdc_free(pdc, instring);
                }

                inalloc = pdc_true;
                instring = instr;
                instr = NULL;
                inlen = len;

                if (inutf == pdc_bytes2)
                    inutf = pdc_bytes;
                else
                    inutf = pdc_auto;
            }
            else
            {
                inutf = pdc_utf16;
            }
        }
        break;

        /* OEM multi byte text strings */
        case pdc_auto:
        case pdc_bytes:
        if (codepage > 0)
        {
#if defined(WIN32)
            if (!(flags & PDC_CONV_ANALYZE) && inlen > 0)
            {
                if (logg)
                    pdc_logg(pdc,
                        "\t\tconverting according Windows codepage %d\n",
                        codepage);

                len = MultiByteToWideChar((UINT) codepage, (DWORD) 0,
                                          (LPCSTR) instring, inlen, NULL, 0);
                if (len == 0)
                {
                    DWORD lasterror = GetLastError();

                    stemp1 = pdc_errprintf(pdc, "cp%d", codepage);
                    if (lasterror == ERROR_INVALID_PARAMETER)
                    {
                        errcode = PDC_E_CONV_UNSUPP_MBTEXTFORM;
                    }
                    else
                    {
                        errcode = PDC_E_CONV_ILL_MBTEXTSTRING;
                    }
                    goto PDC_CONV_ERROR;
                }

                len *= 2;
                instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
                             pdc_calloc_tmp(pdc, (size_t) (len + 2), fn,
                                            NULL, NULL) :
                             pdc_calloc(pdc, (size_t) (len + 2), fn));
                MultiByteToWideChar((UINT) codepage, (DWORD) 0, (LPCSTR)
                                    instring, inlen,
                                    (LPWSTR) instr, len);

                if (inalloc)
                {
                    if (flags & PDC_CONV_TMPALLOC)
                        pdc_free_tmp(pdc, instring);
                    else
                        pdc_free(pdc, instring);
                }

                inalloc = pdc_true;
                instring = instr;
                instr = NULL;
                inlen = len;

                inutf = pdc_utf16;
            }
            else
            {
                inutf = pdc_bytes;
            }
#else   /* WIN32 */
            errcode = PDC_E_CONV_UNSUPP_MBTEXTFORM;
            goto PDC_CONV_ERROR;
#endif  /* !WIN32 */
        }
        break;

        default:
        break;
    }

    /* analyzing UTF-16 textformat */
    if (inutf == pdc_utf16)
    {
        if (pdc_is_utf16be_unicode(instring))
            inutf = pdc_utf16be;
        else if (pdc_is_utf16le_unicode(instring))
            inutf = pdc_utf16le;
    }

    /* analyzing auto textformat */
    else if (inutf == pdc_auto)
    {
        if (pdc_is_utf8_bytecode(instring))
            inutf = PDC_UTF8;
        else if (pdc_is_utf16be_unicode(instring))
            inutf = pdc_utf16be;
        else if (pdc_is_utf16le_unicode(instring))
            inutf = pdc_utf16le;
        else if (inev && !(flags & PDC_CONV_FORCEUTF16))
            inutf = pdc_bytes;
        else
            inutf = pdc_utf16;
    }

    if (logg)
        pdc_logg(pdc, "\t\tdetermined textformat: %s\n",
                 pdc_get_keyword(inutf, pdc_textformat_keylist));

    /* only analyzing */
    if (flags & PDC_CONV_ANALYZE)
        goto PDC_CONV_EXIT;

    /* conversion to UTF-16 by swapping */
    if ((inutf == pdc_utf16be  || inutf == pdc_utf16le) &&
        (inutf != oututf || flags & PDC_CONV_TRYBYTES ||
         flags & PDC_CONV_HTMLCHAR))
    {
        if (inlen &&
            ((inutf == pdc_utf16be && !PDC_ISBIGENDIAN) ||
             (inutf == pdc_utf16le &&  PDC_ISBIGENDIAN)))
        {
            if (inalloc)
                pdc_swap_bytes2((char *) instring, inlen, NULL);
            else
            {
                instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
                     pdc_calloc_tmp(pdc, (size_t) (inlen + 2), fn, NULL, NULL) :
                     pdc_calloc(pdc, (size_t) (inlen + 2), fn));
                pdc_swap_bytes2((char *) instring, inlen, (char *) instr);

                inalloc = pdc_true;
                instring = instr;
                instr = NULL;
            }
        }
        inutf = pdc_utf16;
    }

    /* conversion to UTF-32 by swapping */
    if (inlen && inutf == pdc_utf32)
    {

        if ((pdc_is_utf32be_unicode(instring) && !PDC_ISBIGENDIAN) ||
            (pdc_is_utf32le_unicode(instring) &&  PDC_ISBIGENDIAN))
        {
            if (inalloc)
                pdc_swap_bytes4((char *) instring, inlen, NULL);
            else
            {
                instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
                     pdc_calloc_tmp(pdc, (size_t) (inlen + 4), fn, NULL, NULL) :
                     pdc_calloc(pdc, (size_t) (inlen + 4), fn));
                pdc_swap_bytes4((char *) instring, inlen, (char *) instr);

                inalloc = pdc_true;
                instring = instr;
                instr = NULL;
            }
        }
    }

    /* illegal UTF-16 / UTF-32 */
    if (inutf >= pdc_utf16 && inlen % 2)
    {
        if (inutf == pdc_utf32 && inlen % 4)
            errcode = PDC_E_CONV_ILLUTF32;
        else
            errcode = PDC_E_CONV_ILLUTF16;
        goto PDC_CONV_ERROR;
    }


    /* conversion to UTF-16 by inflation or encoding vector */
    if (inutf == pdc_bytes &&
        (oututf != pdc_bytes || flags & PDC_CONV_HTMLCHAR || inev != outev))
    {
        if (logg)
        {
            if (flags & PDC_CONV_HTMLCHAR)
                pdc_logg(pdc, "\t\tbyte character entity substitution\n");
        }

        len = 2 * inlen;
        instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
             pdc_calloc_tmp(pdc, (size_t) (len + 2), fn, NULL, NULL) :
             pdc_calloc(pdc, (size_t) (len + 2), fn));
        usinstr = (pdc_ushort *) instr;

        j = 0;
        for (i = 0; i < inlen; i++)
        {
            uv = (pdc_ushort) instring[i];
            if (inev)
            {
                uv = inev->codes[uv];
                if (!uv && (flags & PDC_CONV_ENCERROR) &&
                    (!(flags & PDC_CONV_KEEPLBCHAR) ||
                     !pdc_is_linebreaking_relchar(uv)))
                {
                    errcode = PDC_E_ENC_NOTDEF_CODE;
                    stemp1 = pdc_errprintf(pdc, "x%02X", instring[i]);
                    stemp2 = inev->apiname;
                    goto PDC_CONV_ERROR;
                }
            }


            usinstr[j] = uv;
            j++;
        }

        if (inalloc)
        {
            if (flags & PDC_CONV_TMPALLOC)
                pdc_free_tmp(pdc, instring);
            else
                pdc_free(pdc, instring);
        }

        inalloc = pdc_true;
        instring = instr;
        instr = NULL;
        inlen = 2 * j;
        inutf = pdc_utf16;
    }



    /* UTF conversion */
    oututf_s = oututf;
    if ((oututf_s == pdc_bytes && inutf == pdc_utf8) ||
         oututf_s == pdc_utf16be || oututf_s == pdc_utf16le)
        oututf_s = pdc_utf16;
    if (inutf != oututf_s && oututf_s != pdc_bytes)
    {
        len = 4 * (inlen + 1);
        instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
             pdc_calloc_tmp(pdc, (size_t) len, fn, NULL, NULL) :
             pdc_calloc(pdc, (size_t) len, fn));

        if (inlen)
        {
            pdc_convers_result result = conversionOK;
            pdc_byte *instringa, *instra, *instringe, *instre;
            UTF8 *isa8 = NULL, *ise8 = NULL;
            UTF16 *isa16, *ise16;
            UTF32 *isa32, *ise32;

            if (logg)
               pdc_logg(pdc, "\t\tUTF conversion\n");

            instringa = instring;
            instringe = instring + inlen;
            instra = instr;
            instre = instr + len;

            if (inutf == pdc_utf8)
            {
                isa8 = (UTF8 *) instringa;
                ise8 = (UTF8 *) instringe;
                if (oututf_s == pdc_utf16)
                {
                    isa16 = (UTF16 *) instra;
                    ise16 = (UTF16 *) instre;
                    result = pdc_convertUTF8toUTF16(&isa8, ise8,
                                                    &isa16, ise16,
                                                    strictConversion);
                    instra = (pdc_byte *) isa16;
                    instre = (pdc_byte *) ise16;
                }
                else
                {
                    isa32 = (UTF32 *) instra;
                    ise32 = (UTF32 *) instre;
                    result = pdc_convertUTF8toUTF32(&isa8, ise8,
                                                    &isa32, ise32,
                                                    strictConversion);
                    instra = (pdc_byte *) isa32;
                    instre = (pdc_byte *) ise32;
                }
            }
            else if (inutf == pdc_utf16)
            {
                isa16 = (UTF16 *) instringa;
                ise16 = (UTF16 *) instringe;
                if (oututf_s == pdc_utf8)
                {
                    isa8 = (UTF8 *) instra;
                    ise8 = (UTF8 *) instre;
                    result = pdc_convertUTF16toUTF8(&isa16, ise16, &isa8, ise8,
                                                    strictConversion);
                    instra = (pdc_byte *) isa8;
                    instre = (pdc_byte *) ise8;
                }
                else
                {
                    isa32 = (UTF32 *) instra;
                    ise32 = (UTF32 *) instre;
                    result = pdc_convertUTF16toUTF32(&isa16, ise16,
                                                     &isa32, ise32,
                                                     strictConversion);
                    instra = (pdc_byte *) isa32;
                    instre = (pdc_byte *) ise32;
                }
            }
            else if (inutf == pdc_utf32)
            {
                isa32 = (UTF32 *) instringa;
                ise32 = (UTF32 *) instringe;
                if (oututf_s == pdc_utf8)
                {
                    isa8 = (UTF8 *) instra;
                    ise8 = (UTF8 *) instre;
                    result = pdc_convertUTF32toUTF8(&isa32, ise32,
                                                    &isa8, ise8,
                                                    strictConversion);
                    instra = (pdc_byte *) isa8;
                    instre = (pdc_byte *) ise8;
                }
                else
                {
                    isa16 = (UTF16 *) instra;
                    ise16 = (UTF16 *) instre;
                    result = pdc_convertUTF32toUTF16(&isa32, ise32,
                                                     &isa16, ise16,
                                                     strictConversion);
                    instra = (pdc_byte *) isa16;
                    instre = (pdc_byte *) ise16;
                }
            }

            switch (result)
            {
                case targetExhausted:
                errcode = PDC_E_CONV_MEMOVERFLOW;
                break;

                case sourceExhausted:
                case sourceIllegal:
                if (inutf == pdc_utf8)
                {
                    UTF8 *bp, *bpe;
                    char *sb = sbuf;

                    bpe = MIN(ise8 - 1, isa8 + 3);
                    for (bp = isa8; bp <= bpe; bp++)
                        sb += sprintf(sb, "\\x%02X", *bp);
                    if (*bp)
                        sb += sprintf(sb, "...");
                    sb += sprintf(sb, " (");
                    for (bp = isa8; bp <= bpe; bp++)
                        sb += sprintf(sb, "%c", *bp);
                    if (*bp)
                        sb += sprintf(sb, "...");
                    sb += sprintf(sb, ")");
                    stemp1 = sbuf;

                    stemp2 = pdc_errprintf(pdc, "%d", isa8 - (UTF8 *)instringa);

                    if (flags & PDC_CONV_INFLATE)
                    {
                        pdc_warning(pdc, PDC_E_CONV_ILLUTF8SEQU, stemp1, stemp2,
                                    0, 0);

                        pdc_inflate_ascii((char *) instring, inlen,
                                          (char *) instr, pdc_utf16);
                        instra = instr + 2 * inlen;
                    }
                    else
                    {
                        errcode = PDC_E_CONV_ILLUTF8SEQU;
                    }
                }
                else
                {
                    stemp1 = pdc_get_keyword((int)inutf, pdc_utfformat_keylist);
                    errcode = PDC_E_CONV_ILLUTF;
                }
                break;

                default:
                break;
            }

            if (errcode)
            {
                if (logg)
                   pdc_logg(pdc, "\t\tUTF conversion error %d\n", result);

                goto PDC_CONV_ERROR;
            }

            inlen = ( int ) ( instra - instr );
        }

        if (inalloc)
        {
            if (flags & PDC_CONV_TMPALLOC)
                pdc_free_tmp(pdc, instring);
            else
                pdc_free(pdc, instring);
        }

        len = (oututf == pdc_utf32) ? inlen + 4 : inlen + 2;
        if (inlen + 4 != len)
            instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
                 pdc_realloc_tmp(pdc, instr, (size_t) len, fn) :
                 pdc_realloc(pdc, instr, (size_t) len, fn));
        instr[inlen] = 0;
        instr[inlen + 1] = 0;
        if (oututf == pdc_utf32)
        {
            instr[inlen + 2] = 0;
            instr[inlen + 3] = 0;
        }

        inalloc = pdc_true;
        instring = instr;
        instr = NULL;
        inutf = oututf_s;
    }

    if (inutf == pdc_bytes)
    {
        if (!inalloc)
        {
            instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
                 pdc_calloc_tmp(pdc, (size_t) (inlen + 2), fn, NULL, NULL) :
                 pdc_calloc(pdc, (size_t) (inlen + 2), fn));
            memcpy(instr, instring, (size_t) inlen);

            inalloc = pdc_true;
            instring = instr;
            instr = NULL;
        }
    }

    /* trying to reduce UTF-16 string to bytes string */
    if (inutf == pdc_utf16 &&
        (oututf == pdc_bytes || flags & PDC_CONV_TRYBYTES))
    {
        if (logg)
           pdc_logg(pdc, "\t\ttry to reduce UTF-16 to bytes\n");

        if (pdc_is_utf16be_unicode(instring) ||
            pdc_is_utf16le_unicode(instring))
            n = 1;
        else
            n = 0;

        len = (inlen - n) / 2;
        instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
             pdc_calloc_tmp(pdc, (size_t) (len + 2), fn, NULL, NULL) :
             pdc_calloc(pdc, (size_t) (len + 2), fn));
        usinstr = (pdc_ushort *) instring;

        for (i = 0; i < len; i++)
        {
            uv = usinstr[i + n];
            if (outev && uv)
            {
                j = pdc_get_encoding_bytecode(pdc, outev, uv);
                if (j < 0 && (flags & PDC_CONV_ENCERROR) && oututf == pdc_bytes)
                {
                    errcode = PDC_E_ENC_NOTDEF_UNICODE;
                    stemp1 = pdc_errprintf(pdc, "%04X", uv);
                    stemp2 = outev->apiname;
                    goto PDC_CONV_ERROR;
                }
                uv = (pdc_ushort) j;
            }
            if (uv > PDC_UNICODE_MAXLATIN1)
                break;

            instr[i] = (pdc_byte) uv;
        }

        if (i == len)
        {
            if (inalloc)
            {
                if (flags & PDC_CONV_TMPALLOC)
                    pdc_free_tmp(pdc, instring);
                else
                    pdc_free(pdc, instring);
            }

            inalloc = pdc_true;
            instring = instr;
            instr = NULL;
            inlen = len;
            inutf = pdc_bytes;
        }
        else
        {
            if (flags & PDC_CONV_TMPALLOC)
                pdc_free_tmp(pdc, instr);
            else
                pdc_free(pdc, instr);
            instr = NULL;
        }
    }

    /* UTF-8 format */
    if (inutf == pdc_utf8)
    {
        hasbom = pdc_is_utf8_unicode(instring);

        if (flags & PDC_CONV_TRY7BYTES)
        {
            if (logg)
               pdc_logg(pdc, "\t\ttry to reduce UTF-8 to 7-bit\n");

            for (i = hasbom ? 3 : 0; i < inlen; i++)
                if (instring[i] > PDC_UNICODE_MAXASCII)
                    break;
            if (i == inlen)
            {
                flags &= ~PDC_CONV_WITHBOM;
                flags |= PDC_CONV_NOBOM;
                inutf = pdc_bytes;
            }
        }
        else if (hasbom && (flags & PDC_CONV_AUTOBOM))
        {
            flags &= ~PDC_CONV_NOBOM;
            flags |= PDC_CONV_WITHBOM;
        }
        else if ((flags & PDC_CONV_WITHBOM) && (flags & PDC_CONV_NOBOM))
        {
            flags &= ~PDC_CONV_NOBOM;
        }

        if (!inalloc || flags & PDC_CONV_WITHBOM || flags & PDC_CONV_NOBOM)
        {
            i = (flags & PDC_CONV_WITHBOM && !hasbom) ? 3 : 0;
            j = (flags & PDC_CONV_NOBOM && hasbom) ? 3 : 0;

            len = inlen + i - j;
            instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
                 pdc_calloc_tmp(pdc, (size_t) (len + 2), fn, NULL, NULL) :
                 pdc_calloc(pdc, (size_t) (len + 2), fn));
            memcpy(&instr[i], &instring[j], (size_t) (inlen - j));
            instr[len] = 0;

            if (inalloc)
            {
                if (flags & PDC_CONV_TMPALLOC)
                    pdc_free_tmp(pdc, instring);
                else
                    pdc_free(pdc, instring);
            }

            inalloc = pdc_true;
            instring = instr;
            instr = NULL;
            inlen = len;

            hasbom = (flags & PDC_CONV_WITHBOM);
        }

        if (hasbom)
        {
            instring[0] = PDF_BOM2;
            instring[1] = PDF_BOM3;
            instring[2] = PDF_BOM4;
        }

    }

    /* UTF-16 formats */
    if (inutf == pdc_utf16 || inutf == pdc_utf16be || inutf == pdc_utf16le)
    {
        hasbom = pdc_is_utf16be_unicode(instring) ||
                 pdc_is_utf16le_unicode(instring);

        if (hasbom && (flags & PDC_CONV_AUTOBOM))
        {
            flags &= ~PDC_CONV_NOBOM;
            flags |= PDC_CONV_WITHBOM;
        }
        else if ((flags & PDC_CONV_WITHBOM) && (flags & PDC_CONV_NOBOM))
        {
            flags &= ~PDC_CONV_NOBOM;
        }

        if (!inalloc || oututf == pdc_utf16be || oututf == pdc_utf16le ||
            flags & PDC_CONV_WITHBOM || flags & PDC_CONV_NOBOM)
        {
            i = (flags & PDC_CONV_WITHBOM && !hasbom) ? 2 : 0;
            j = (flags & PDC_CONV_NOBOM && hasbom) ? 2 : 0;

            len = inlen + i - j;
            instr = (pdc_byte *) ((flags & PDC_CONV_TMPALLOC) ?
                 pdc_calloc_tmp(pdc, (size_t) (len + 2), fn, NULL, NULL) :
                 pdc_calloc(pdc, (size_t) (len + 2), fn));
            memcpy(&instr[i], &instring[j], (size_t) (inlen - j));

            if (inalloc)
            {
                if (flags & PDC_CONV_TMPALLOC)
                    pdc_free_tmp(pdc, instring);
                else
                    pdc_free(pdc, instring);
            }

            instring = instr;
            instr = NULL;
            inlen = len;

            hasbom = (flags & PDC_CONV_WITHBOM);
        }

        i = hasbom ? 2 : 0;
        if (inutf == pdc_utf16)
        {
            if (oututf == pdc_utf16be)
            {
                inutf = pdc_utf16be;
                toswap = !PDC_ISBIGENDIAN;
            }
            if (oututf == pdc_utf16le)
            {
                inutf = pdc_utf16le;
                toswap = PDC_ISBIGENDIAN;
            }
            if (toswap)
                pdc_swap_bytes2((char *) &instring[i], inlen - i, NULL);
        }

        if (hasbom)
        {
            if (inutf == pdc_utf16be ||
                (inutf == pdc_utf16 && PDC_ISBIGENDIAN))
            {
                instring[0] = PDF_BOM0;
                instring[1] = PDF_BOM1;
            }
            if (inutf == pdc_utf16le ||
                (inutf == pdc_utf16 && !PDC_ISBIGENDIAN))
            {
                instring[0] = PDF_BOM1;
                instring[1] = PDF_BOM0;
            }
        }
    }

    if (logg)
        pdc_logg(pdc, "\t\ttextformat of converted string: %s\n",
                 pdc_get_keyword(inutf, pdc_textformat_keylist));

    PDC_CONV_EXIT:
    *oututf_p = inutf;
    if (outlen)
        *outlen = inlen;
    *outstring = instring;
    return 0;

    PDC_CONV_ERROR:
    if (outlen)
        *outlen = 0;
    *outstring = NULL;

    if (errcode > 0)
        pdc_set_errmsg(pdc, errcode, stemp1, stemp2, 0, 0);

    if (instr != NULL)
    {
        if (flags & PDC_CONV_TMPALLOC)
            pdc_free_tmp(pdc, instr);
        else
            pdc_free(pdc, instr);
    }

    if (inalloc)
    {
        if (flags & PDC_CONV_TMPALLOC)
            pdc_free_tmp(pdc, instring);
        else
            pdc_free(pdc, instring);
    }

    if (verbose)
        PDC_RETHROW(pdc);

    return errcode;
}
#if defined(_MSC_VER) && defined(_MANAGED)
#pragma managed
#endif


/*
 *  pdc_convert_name_ext converts a string of name data type to UTF-8
 *
 *  flags & PDC_CONV_EBCDIC: converts to EBCDIC-UTF-8
 *
 *  len == 0: If the string has a [EBCDIC-]UTF-8 BOM or
 *            flags & PDC_CONV_ISUTF8 is set the string will be duplicated.
 *            Otherwise the string has encoding enc and codepage
 *            codepage.
 *            If enc == pdc_unicode the string is "UTF-16" encoded.
 *            Otherwise: If enc < pdc_winansi the string is "host" encoded.
 *
 *  len  > 0: The string is a UTF-16 string of len bytes.
 *
 */
char *
pdc_convert_name_ext(pdc_core *pdc, const char *name, int len,
                     pdc_encoding enc, int codepage, int flags)
{
    static const char fn[] = "pdc_convert_name_ext";
    pdc_encodingvector *ev = NULL;
    pdc_text_format nameformat = pdc_utf16;
    pdc_text_format outnameformat = pdc_utf8;
    pdc_byte *convname;
    char *outname = NULL;
    int outlen;

    if (name == NULL)
        return NULL;

    if (len == 0)
    {
        /* already [EBCDIC-]UTF-8 encoded */
        if ((flags & PDC_CONV_ISUTF8) || pdc_is_utf8_bytecode(name))
        {
            if (!(flags & PDC_CONV_WITHBOM))
                flags |= PDC_CONV_NOBOM;

            if (!(flags & PDC_CONV_EBCDIC))
                flags |= PDC_CONV_ASCII;

            /* On EBCDIC platforms EBCDIC-UTF-8 name strings are expected */
            outname = pdc_strdup_ext(pdc, name, (flags & ~PDC_CONV_EBCDIC), fn);

            if (outname != NULL)
                return outname;
        }

        /* see bug #1486 */
        if (enc == pdc_unicode)
        {
            /* UTF-16 encoded string */
            len = (int) pdc_wstrlen(name);
        }
        else
        {
            /* 8-bit encoded string */
            nameformat = pdc_bytes;
            if (enc < pdc_winansi)
                ev = pdc_get_encoding_vector(pdc,pdc_find_encoding(pdc,"host"));
            else
                ev = pdc_get_encoding_vector(pdc, enc);

            len = (int) strlen(name);
        }
    }

    if (flags & PDC_CONV_EBCDIC)
        outnameformat = PDC_UTF8;

    flags |= PDC_CONV_TRY7BYTES;
    if (pdc->charref)
        flags |= PDC_CONV_HTMLCHAR;
    if (pdc->escapesequ)
        flags |= PDC_CONV_BSSEQU;

    /* convert to UTF-8 */
    pdc_convert_string(pdc, nameformat, codepage, ev, (pdc_byte *) name, len,
                &outnameformat, NULL, &convname, &outlen, flags,
                pdc_true);

    return (char *) convname;
}

char *
pdc_convert_name(pdc_core *pdc, const char *name, int len, int flags)
{
    return pdc_convert_name_ext(pdc, name, len, pdc_invalidenc, 0, flags);
}

/* returned string is temporary allocated
*/
char *
pdc_utf8_to_hostbytes(pdc_core *pdc, pdc_bool honorlang, char *name)
{
    static const char fn[] = "pdc_utf8_to_hostbytes";
    pdc_encoding outenc = pdc_invalidenc;
    pdc_encodingvector *outev = NULL;
    pdc_text_format informat = PDC_UTF8;
    pdc_text_format outformat = pdc_utf16;
    pdc_byte *outname = NULL;
    int len = (int) strlen(name);

    {
        (void) fn;
        (void) honorlang;
        outenc = pdc_find_encoding(pdc, "host");
    }

    outev = pdc_get_encoding_vector(pdc, outenc);

    pdc_convert_string(pdc, informat, 0, NULL, (pdc_byte *) name, len,
                &outformat, outev, &outname, &len,
                PDC_CONV_TRYBYTES | PDC_CONV_NOBOM | PDC_CONV_TMPALLOC,
                pdc_true);
    if (outformat == pdc_utf16)
    {
        pdc_free_tmp(pdc, outname);
        outname = NULL;
    }

    return (char *) outname;
}

/* returned string is temporary allocated
*/
char *
pdc_hostbytes_to_utf8(pdc_core *pdc, pdc_bool honorlang, char *name)
{
    static const char fn[] = "pdc_hostbytes_to_utf8";
    pdc_encoding inenc = pdc_invalidenc;
    pdc_encodingvector *inev = NULL;
    pdc_text_format informat = pdc_bytes;
    pdc_text_format outformat = PDC_UTF8;
    pdc_byte *outname = NULL;
    int len = (int) strlen(name);

    {
        (void) fn;
        (void) honorlang;
        inenc = pdc_find_encoding(pdc, "host");
    }

    inev = pdc_get_encoding_vector(pdc, inenc);

    pdc_convert_string(pdc, informat, 0, inev, (pdc_byte *) name, len,
                &outformat, NULL, &outname, &len,
                PDC_CONV_NOBOM | PDC_CONV_TMPALLOC, pdc_true);

    return (char *) outname;
}

/* --------------------- basic UTF conversion functions --------------------- */

char *
pdc_utf16_to_utf8(pdc_core *pdc, const char *utf16string, int len, int flags,
                  int *size)
{
    pdc_text_format outtextformat = pdc_utf8;
    pdc_byte *utf8string = NULL;
    int outlen;

    if (!utf16string)
        pdc_error(pdc, PDC_E_ILLARG_EMPTY, "utf16string", 0, 0, 0);

    if (flags & PDC_CONV_EBCDIC)
        outtextformat = PDC_UTF8;

    flags |= PDC_CONV_AUTOBOM;
    pdc_convert_string(pdc, pdc_utf16, 0, NULL,
                       (pdc_byte *) utf16string, len,
                       &outtextformat, NULL, &utf8string, &outlen,
                       flags, pdc_true);

    if (size) *size = outlen;

    return (char *) utf8string;
}

char *
pdc_utf8_to_utf16(pdc_core *pdc, const char *utf8string, const char *format,
                  int flags, int *size)
{
    pdc_text_format textformat = pdc_utf8;
    pdc_text_format outtextformat = pdc_utf16;
    pdc_byte *utf16string = NULL;
    int len;

    if (!utf8string)
        pdc_error(pdc, PDC_E_ILLARG_EMPTY, "utf8string", 0, 0, 0);
    len = (int) strlen(utf8string);

    if (format && *format)
    {
        int k = pdc_get_keycode_ci(format, pdc_textformat_keylist);

        /* see bug #2175 */
        if (k == PDC_KEY_NOTFOUND)
        {
            char **sfl;
            const char *sf;
            int ns, i;

            sf = NULL;
            ns = pdc_split_stringlist(pdc, format, NULL, 0, &sfl);
            for (i = 0; i < ns; i++)
            {
                if (!strcmp(sfl[i], "inflate"))
                    flags |= PDC_CONV_INFLATE;
                else
                    sf = sfl[i];
            }
            if (sf != NULL)
                k = pdc_get_keycode_ci(sf, pdc_textformat_keylist);
            else
                k = pdc_utf16;

            pdc_cleanup_stringlist(pdc, sfl);
        }

        if (k == PDC_KEY_NOTFOUND ||
            ((pdc_text_format) k != pdc_utf16 &&
             (pdc_text_format) k != pdc_utf16be &&
             (pdc_text_format) k != pdc_utf16le))
            pdc_error(pdc, PDC_E_ILLARG_STRING, "format", format, 0, 0);

        outtextformat = (pdc_text_format) k;
    }

    if (flags & PDC_CONV_EBCDIC)
        textformat = PDC_UTF8;

    if (outtextformat == pdc_utf16)
        flags |= PDC_CONV_AUTOBOM;
    else
        flags |= PDC_CONV_WITHBOM;
    pdc_convert_string(pdc, textformat, 0, NULL,
                      (pdc_byte *) utf8string, len,
                      &outtextformat, NULL, &utf16string, size,
                      flags, pdc_true);

    return (char *) utf16string;
}

char *
pdc_utf16_to_utf32(pdc_core *pdc, const char *utf16string, int len, int *size)
{
    pdc_text_format outtextformat = pdc_utf32;
    pdc_byte *utf32string = NULL;

    if (!utf16string)
        pdc_error(pdc, PDC_E_ILLARG_EMPTY, "utf16string", 0, 0, 0);

    pdc_convert_string(pdc, pdc_utf16, 0, NULL,
                       (pdc_byte *) utf16string, len,
                       &outtextformat, NULL, &utf32string, size,
                       0, pdc_true);

    return (char *) utf32string;
}

char *
pdc_utf32_to_utf8(pdc_core *pdc, const char *utf32string, int len, int flags,
                  int *size)
{
    pdc_text_format outtextformat = pdc_utf8;
    pdc_byte *utf8string = NULL;
    int outlen;

    if (!utf32string)
        pdc_error(pdc, PDC_E_ILLARG_EMPTY, "utf32string", 0, 0, 0);

    if (flags & PDC_CONV_EBCDIC)
        outtextformat = PDC_UTF8;

    flags |= PDC_CONV_AUTOBOM;
    pdc_convert_string(pdc, pdc_utf32, 0, NULL,
                       (pdc_byte *) utf32string, len,
                       &outtextformat, NULL, &utf8string, &outlen,
                       flags, pdc_true);

    if (size) *size = outlen;

    return (char *) utf8string;
}

char *
pdc_utf32_to_utf16(pdc_core *pdc, const char *utf32string, int len,
                   const char *format, int flags, int *size)
{
    pdc_text_format textformat = pdc_utf32;
    pdc_text_format outtextformat = pdc_utf16;
    pdc_byte *utf16string = NULL;

    if (!utf32string)
        pdc_error(pdc, PDC_E_ILLARG_EMPTY, "utf32string", 0, 0, 0);

    if (format && *format)
    {
        int k = pdc_get_keycode_ci(format, pdc_textformat_keylist);
        if (k == PDC_KEY_NOTFOUND ||
            ((pdc_text_format) k != pdc_utf16 &&
             (pdc_text_format) k != pdc_utf16be &&
             (pdc_text_format) k != pdc_utf16le))
            pdc_error(pdc, PDC_E_ILLARG_STRING, "format", format, 0, 0);
        outtextformat = (pdc_text_format) k;
    }

    if (outtextformat == pdc_utf16)
        flags |= PDC_CONV_AUTOBOM;
    else
        flags |= PDC_CONV_WITHBOM;
    pdc_convert_string(pdc, textformat, 0, NULL,
                      (pdc_byte *) utf32string, len,
                      &outtextformat, NULL, &utf16string, size,
                      flags, pdc_true);

    return (char *) utf16string;
}

int
pdc_char16_to_char32(pdc_core *pdc, const pdc_ushort *ustext, int *ic, int len,
                     pdc_bool verbose)
{
    pdc_ushort uvh = ustext[*ic];

    if (uvh < PDC_UNICODE_MINHIGHSUR || uvh > PDC_UNICODE_MAXLOWSUR)
    {
        return (int) uvh;
    }
    else
    {
        UTF16 *isa16 = (UTF16 *) &ustext[*ic];
        pdc_ushort uvl = 0;
        int icn = *ic + 1;

        if (icn < len)
        {
            uvl = ustext[icn];
            if (uvh <= PDC_UNICODE_MAXHIGHSUR)
            {
                if (uvl >= PDC_UNICODE_MINLOWSUR &&
                    uvl <= PDC_UNICODE_MAXLOWSUR)
                {
                    int usv;
                    UTF16 *ise16 = isa16 + 2;
                    UTF32 *isa32 = (UTF32 *) &usv;
                    UTF32 *ise32 = isa32 + 1;

                    pdc_convers_result result = pdc_convertUTF16toUTF32(
                                &isa16, ise16, &isa32, ise32, strictConversion);
                    if (result == conversionOK)
                    {
                        *ic = icn;
                        return usv;
                    }
                }
            }
        }

        pdc_set_errmsg(pdc, PDC_E_CONV_ILLUTF16SUR,
                       pdc_errprintf(pdc, "%04X", uvh),
                       pdc_errprintf(pdc, "%04X", uvl), 0, 0);

        if (verbose)
            pdc_error(pdc, -1, 0, 0, 0, 0);
    }

    return -1;
}

int
pdc_char32_to_char16(pdc_core *pdc, int usv, pdc_ushort *uvlist,
                     pdc_bool verbose)
{
    if (usv < PDC_NUM_BMPVAL)
    {
        uvlist[0] = (pdc_ushort) usv;
        return 1;
    }
    else
    {
        UTF32 *isa32 = (UTF32 *) &usv;
        UTF32 *ise32 = isa32 + 1;
        UTF16 *isa16 = (UTF16 *) uvlist;
        UTF16 *ise16 = isa16 + 2;

        pdc_convers_result result = pdc_convertUTF32toUTF16(
                    &isa32, ise32, &isa16, ise16, strictConversion);
        if (result == conversionOK)
        {
            return 2;
        }

        pdc_set_errmsg(pdc, PDC_E_CONV_ILLUTF32CHAR,
                       pdc_errprintf(pdc, "%05X", usv), 0, 0, 0);

        if (verbose)
            pdc_error(pdc, -1, 0, 0, 0, 0);
    }

    return 0;
}
