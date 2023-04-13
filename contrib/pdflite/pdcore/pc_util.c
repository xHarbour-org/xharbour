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
 * PDFlib various utility routines
 *
 */

#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #ifndef _CRT_SECURE_NO_WARNINGS
      #define _CRT_SECURE_NO_WARNINGS
   #endif
   #ifndef _CRT_SECURE_NO_DEPRECATE
      #define _CRT_SECURE_NO_DEPRECATE
   #endif
#endif

#include <errno.h>

#include "pc_util.h"
#include "pc_file.h"
#include "pc_ctype.h"

#ifdef AS400
#include <qp0z1170.h>   /* for getenv() emulation */
#endif

#ifdef __sun
#include <ieeefp.h>     /* for finite */
#endif

#if defined (isfinite)
#define PDC_ISFINITE isfinite
#else /* isfinite */

#if defined(WIN32)
#if 0
#if !defined(_UNICODE)
#define UNICODE
#endif
#endif
#include <windows.h>
#include <float.h>
#include <wchar.h>
#define PDC_ISFINITE _finite
#else /* WIN32 */

#ifdef OS_ZOS_SASC
#define PDC_ISFINITE isfinite
#else /* OS_ZOS_SASC */

#define PDC_ISFINITE finite
#endif
#endif
#endif


/* ---------------------- finite() workarounds -------------------------- */



/*
 * pdc_check_number checks whether a floating-point number
 * is valid and within the specified range. If not, an exception
 * will be thrown.
 */
void
pdc_check_number_limits(pdc_core *pdc, const char *paramname, double dz,
                        double dmin, double dmax)
{
    if (!PDC_ISFINITE(dz))
    {
        pdc_error(pdc, PDC_E_ILLARG_FLOAT_NAN, paramname, 0, 0, 0);
    }
    else if (dz < dmin)
    {
        pdc_error(pdc, PDC_E_ILLARG_FLOAT_TOOSMALL, paramname,
                  pdc_errprintf(pdc, "%f", dz),
                  pdc_errprintf(pdc, "%f", dmin), 0);
    }
    else if (dz > dmax)
    {
        pdc_error(pdc, PDC_E_ILLARG_FLOAT_TOOLARGE, paramname,
                  pdc_errprintf(pdc, "%f", dz),
                  pdc_errprintf(pdc, "%f", dmax), 0);
    }
}

void
pdc_check_number(pdc_core *pdc, const char *paramname, double dz)
{
    pdc_check_number_limits(pdc, paramname, dz, PDC_FLOAT_MIN, PDC_FLOAT_MAX);
}

void
pdc_check_number_zero(pdc_core *pdc, const char *paramname, double dz)
{
    pdc_check_number_limits(pdc, paramname, dz, PDC_FLOAT_MIN, PDC_FLOAT_MAX);

    if (PDC_FLOAT_ISNULL(dz))
    {
        pdc_error(pdc, PDC_E_ILLARG_FLOAT_ZERO, paramname,
                  pdc_errprintf(pdc, "%f", dz), 0, 0);
    }
}

int
pdc_check_text_length(pdc_core *pdc, const char **text, int len, int maxlen)
{
    if (*text == NULL)
    {
        len = 0;
        *text = "";
    }
    else if (len == 0)
    {
        len = (int) strlen(*text);
    }

    if (len < 0 || len > maxlen)
    {
        pdc_error(pdc, PDC_E_ILLARG_STRINGLEN,
                  pdc_errprintf(pdc, "%d", len),
                  pdc_errprintf(pdc, "%d", maxlen), 0, 0);
    }

    return len;
}


/* ---------------- "unsupported feature" error message ------------------ */

void
pdc_set_unsupp_error(pdc_core *pdc, int err_config, int err_lite,
                     pdc_bool warning)
{
    (void) err_config;
    (void) err_lite;

/* this feature is sufficient for non public version */
    if (warning)
        pdc_warning(pdc, err_lite, 0, 0, 0, 0);
    else
        pdc_error(pdc, err_lite, 0, 0, 0, 0);
}


/* ---------------- error message with ASCII strings -------------------- */

void
pdc_ascii_error(pdc_core *pdc, int errnum, int flags, const char *parm1,
                const char *parm2, const char *parm3, const char *parm4)
{
    if (flags & (1<<0))
    {
        parm1 = pdc_errprintf(pdc, "%a", parm1);
    }

    if (flags & (1<<1))
    {
        parm2 = pdc_errprintf(pdc, "%a", parm2);
    }

    if (flags & (1<<2))
    {
        parm3 = pdc_errprintf(pdc, "%a", parm3);
    }

    if (flags & (1<<3))
    {
        parm4 = pdc_errprintf(pdc, "%a", parm4);
    }

    pdc_error(pdc, errnum, parm1, parm2, parm3, parm4);
}


/* -------------------------- Time functions ------------------------------ */

#ifndef WINCE
#ifndef __USE_POSIX
#define __USE_POSIX
#endif
#include <time.h>
#else
#include <winbase.h>
#endif

/* our private localtime() function. this one circumvents platform
** quirks we found on WINCE and Solaris, and perhaps some more in
** the future.
*/
void
pdc_localtime(pdc_time *t)
{
#ifdef WINCE

    SYSTEMTIME  st;

    GetLocalTime (&st);

    t->second = st.wSecond;
    t->minute = st.wMinute;
    t->hour = st.wHour;
    t->mday = st.wDay;
    t->wday = st.wDayOfWeek;
    t->month = st.wMonth;
    t->year = st.wYear;

#else

    time_t      timer;
    struct tm   ltime;

    time(&timer);

#if defined(PDC_NEEDS_R_FUNCTIONS)

    /* the localtime() function isn't thread safe on this platform.
    ** a thread safe variant must be used instead.
    */
    (void) localtime_r(&timer, &ltime);

#else

    ltime = *localtime(&timer);

#endif /* !PDC_NEEDS_R_FUNCTIONS */

    t->second = ltime.tm_sec;
    t->minute = ltime.tm_min;
    t->hour = ltime.tm_hour;
    t->mday = ltime.tm_mday;
    t->wday = ltime.tm_wday;
    t->month = ltime.tm_mon;
    t->year = ltime.tm_year;

#endif /* !WINCE */
}

static void
pdc_localtime_r(const time_t *timer, struct tm *res)
{
#if defined(PDC_NEEDS_R_FUNCTIONS)
    (void) localtime_r(timer, res);
#else
    *res = *localtime(timer);
#endif
}

static void
pdc_gmtime_r(const time_t *timer, struct tm *res)
{
#if defined(PDC_NEEDS_R_FUNCTIONS)
    (void) gmtime_r(timer, res);
#else
    *res = *gmtime(timer);
#endif
}

void
pdc_get_timestr(char *str, pdc_bool ktoascii)
{
#ifndef WINCE
    time_t      timer, gtimer;
    struct tm   ltime;
    double      diffminutes;
    int         utcoffset;
#else
    SYSTEMTIME  st;
#endif

    (void) ktoascii;

#ifndef WINCE
    time(&timer);

#if !defined(I370)
    pdc_gmtime_r(&timer, &ltime);
    gtimer = mktime(&ltime);
    pdc_localtime_r(&timer, &ltime);
    ltime.tm_isdst = 0;
    diffminutes = difftime(mktime(&ltime), gtimer) / 60;
    if (diffminutes >= 0)
        utcoffset = (int)(diffminutes + 0.5);
    else
        utcoffset = (int)(diffminutes - 0.5);
#else
        utcoffset = 0;
#endif

    /* Get local time again, previous data is damaged by mktime(). */
    pdc_localtime_r(&timer, &ltime);

    if (utcoffset > 0)
        sprintf(str, "D:%04d%02d%02d%02d%02d%02d+%02d'%02d'",
            ltime.tm_year + 1900, ltime.tm_mon + 1, ltime.tm_mday,
            ltime.tm_hour, ltime.tm_min, ltime.tm_sec,
            utcoffset / 60, utcoffset % 60);
    else if (utcoffset < 0)
        sprintf(str, "D:%04d%02d%02d%02d%02d%02d-%02d'%02d'",
            ltime.tm_year + 1900, ltime.tm_mon + 1, ltime.tm_mday,
            ltime.tm_hour, ltime.tm_min, ltime.tm_sec,
            abs(utcoffset) / 60, abs(utcoffset) % 60);
    else
        sprintf(str, "D:%04d%02d%02d%02d%02d%02dZ",
            ltime.tm_year + 1900, ltime.tm_mon + 1, ltime.tm_mday,
            ltime.tm_hour, ltime.tm_min, ltime.tm_sec);

#else
    GetLocalTime (&st);
    sprintf(str, "D:%04d%02d%02d%02d%02d%02d",
            st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond);
#endif  /* !WINCE */


}


/* -------------------------- Environment ------------------------------ */

char *
pdc_getenv(pdc_core *pdc, const char *envname)
{
    char *envvalue = NULL;

    (void) pdc;
    (void) envname;

#ifdef HAVE_ENVVARS
    envvalue = getenv(envname);
    if (envvalue != NULL)
    {
        pdc_logg_cond(pdc, 1, trc_filesearch,
            "\tEnvironment variable \"%s=%s\"\n", envname, envvalue);
    }
#endif /* HAVE_ENVVARS */

    return envvalue;
}

char *
pdc_getenv_filename(pdc_core *pdc, const char *envname)
{
    static const char fn[] = "pdc_getenv_filename";
    char *envvalue = NULL;
    int flags = PDC_CONV_TMPALLOC;

#if defined(WIN32)
    size_t len = strlen(envname), wlen;
    #if defined(UNICODE)
       const wchar_t *wenvvalue;
       wchar_t       *wenvname;
    #else
       const char *wenvvalue;
       char       *wenvname;
    #endif

    wlen = 2 * (len + 1);
    #if defined(UNICODE)
       wenvname = (wchar_t *) pdc_calloc(pdc, wlen, fn);
    #else
       wenvname = (char *) pdc_calloc(pdc, wlen, fn);
    #endif
    pdc_inflate_ascii(envname, (int) len, (char *) wenvname, pdc_utf16);

    #if defined(UNICODE)
       wenvvalue = _wgetenv(wenvname);
    #else
       wenvvalue = getenv(wenvname);
    #endif

    pdc_free(pdc, wenvname);

    if (wenvvalue != NULL && pdc_wstrlen((char *) wenvvalue))
    {
        #if defined(UNICODE)
           wlen = 2 * wcslen(wenvvalue);
        #else
           wlen = 2 * strlen(wenvvalue);
        #endif
        pdc_logg_cond(pdc, 1, trc_filesearch,
            "\tEnvironment variable \"%s=%T\"\n", envname, wenvvalue, wlen);

        if (pdc_logg_is_enabled(pdc, 3, trc_filesearch))
            flags |= PDC_CONV_LOGGING;

        envvalue = pdc_convert_name(pdc, (char *) wenvvalue, ( int ) wlen, flags);
    }

#else /* WIN32 */

    envvalue = pdc_getenv(pdc, envname);
    if (envvalue != NULL && strlen(envvalue))
    {
        envvalue = pdc_strdup_ext(pdc, envvalue, flags, fn);
    }

#endif /* !WIN32 */

    return envvalue;
}



/* ------------------------ Language Code ------------------------------ */

/* ISO 639 Windows and Mac Language codes */
static const char lang_codes_ISO639[] =
    "ab aa af sq am ar hy as ay az ba eu bn dz bh bi br bg my be km ca zh co"
    "hr cs da nl en eo et fo fa fj fi fr fy gl gd gv ka de el kl gn gu ha he"
    "hi hu is id ia ie iu ik ga it ja jv kn ks kk rw ky rn ko ku lo la lv li"
    "ln lt mk mg ms ml mt mi mr mo mn na ne no oc or om ps pl pt pa qu rm ro"
    "ru sm sg sa sr sh st tn sn sd si ss sk sl so es su sw sv tl tg ta tt te"
    "th bo ti to ts tr tk tw ug uk ur uz vi vo cy wo xh yi yo zu"
    "pt-br en-gb en-us de-de de-ch";

pdc_bool
pdc_check_lang_code(pdc_core *pdc, const char* lang_code)
{
    pdc_bool valid = pdc_false;
    int     i;
    char*   country_code;
    char*   language;

    if ((lang_code != NULL) && *lang_code)
    {
        /* do not check for IANA or private languages */
        valid = ((lang_code[0] == 'i') || (lang_code[0] == 'x'));
        if (!valid)
        {
            language = pdc_strdup(pdc, lang_code);
            for (i = 0; i < (int)strlen(language); i++)
            {
                if (pdc_isupper(language[i]))
                {
                    language[i] = (char) pdc_tolower((int)language[i]);
                }
            }


            country_code = (char *)strstr(lang_codes_ISO639, language);
            valid = (country_code != NULL);

            if (!valid && (strlen(language) > 2))
            {
                country_code = strchr(language, '-');
                if (country_code != NULL)
                {
                    country_code[0] = '\0';

                    country_code = (char *)strstr(lang_codes_ISO639, language);
                    valid = (country_code != NULL);

                    if (!valid)
                    {
                        pdc_warning(pdc, PDC_E_ILLARG_LANG_CODE,
                                    lang_code, 0, 0, 0);
                    }
                }
            }

            pdc_free(pdc, language);
        }
    }

    return valid;
}


/* -------------------------- Bit arryas ------------------------------ */

/* set bit right to left within a byte */
void
pdc_setbit(char *bitarr, int bit)
{
    bitarr[bit/8] |= (char) (1<<(bit%8));
}

/* set bit left to right within a byte */
void
pdc_setbit_l2r(char *bitarr, int bit)
{
    bitarr[bit/8] |= (char) (0x80>>(bit%8));
}

pdc_bool
pdc_getbit(const char *bitarr, int bit)
{
    return (pdc_bool) (bitarr[bit/8] & (1<<(bit%8)));
}

void
pdc_setbit_text(char *bitarr, const pdc_byte *text, int len,
                int nbits, int size)
{
    int i, bit;
    pdc_ushort *ustext = (pdc_ushort *) text;

    for (i = 0; i < len; i += size)
    {
        if (size == sizeof(pdc_byte))
            bit = (int) text[i];
        else
            bit = ustext[i/size];
        if (bit < nbits) pdc_setbit(bitarr, bit);
    }
}


/* ---------- Get functions of integer binary data types --------------- */

pdc_short
pdc_get_le_short(const pdc_byte *data)
{
    return (pdc_short) ((pdc_short) (data[1] << 8) | data[0]);
}

pdc_ushort
pdc_get_le_ushort(const pdc_byte *data)
{
    return (pdc_ushort) ((data[1] << 8) | data[0]);
}

pdc_uint32
pdc_get_le_ulong3(const pdc_byte *data)
{
    return (pdc_uint32) (((((data[2]) << 8) | data[1]) << 8) | data[0]);
}

pdc_sint32
pdc_get_le_long(const pdc_byte *data)
{
    return ((pdc_sint32)
         (((((data[3] << 8) | data[2]) << 8) | data[1]) << 8) | data[0]);
}

pdc_uint32
pdc_get_le_ulong(const pdc_byte *data)
{
    return (pdc_uint32)
         ((((((data[3] << 8) | data[2]) << 8) | data[1]) << 8) | data[0]);
}

pdc_short
pdc_get_be_short(const pdc_byte *data)
{
    return (pdc_short) ((pdc_short) (data[0] << 8) | data[1]);
}

pdc_ushort
pdc_get_be_ushort(const pdc_byte *data)
{
    return (pdc_ushort) ((data[0] << 8) | data[1]);
}

pdc_uint32
pdc_get_be_ulong3(const pdc_byte *data)
{
    return (pdc_uint32) (((((data[0]) << 8) | data[1]) << 8) | data[2]);
}

pdc_sint32
pdc_get_be_long(const pdc_byte *data)
{
    return ((pdc_sint32)
         (((((data[0] << 8) | data[1]) << 8) | data[2]) << 8) | data[3]);
}

pdc_uint32
pdc_get_be_ulong(const pdc_byte *data)
{
    return (pdc_uint32)
        ((((((data[0] << 8) | data[1]) << 8) | data[2]) << 8) | data[3]);
}


/* ----------------- String handling for Unicode too ------------------- */

/* strlen() for wide character strings, which are double null terminated.
 * pdc_wstrlen() returns the number of bytes in the Unicode string,
 * NOT including the two terminating null bytes.
 */
size_t
pdc_wstrlen(const char *str)
{
    size_t len = 0;

    while(str[len] != 0 || str[len+1] != 0)
    {
        len += 2;
    }

    return len;
}

/*
 * This function returns the length in bytes for C and Unicode strings.
 */
size_t
pdc_strlen(const char *str)
{
    if (pdc_is_utf16be_unicode(str) || pdc_is_utf16le_unicode(str))
        return pdc_wstrlen(str);
    else
        return strlen(str);
}


/* Allocate a local buffer and copy the string including
 * the terminating sentinel. If the string starts with the Unicode BOM
 * it is considered a Unicode string, and must be terminated by
 * two null bytes. Otherwise it is considered a plain C string and
 * must be terminated by a single null byte.
 * The caller is responsible for freeing the buffer.
 *
 * The special functions pdc_strdup, pdc_strdup_tmp and pdc_strdup_withbom
 * should be replaced by the more sophisticated function pdc_strdup_ext.
 * There: flags (see pc_unicode.h):
 *
 * PDC_CONV_TMPALLOC, PDC_CONV_EBCDIC, PDC_CONV_ASCII,
 * PDC_CONV_WITHBOM, PDC_CONV_NOBOM, PDC_CONV_MAXSTRLEN
 *
 */
char *
pdc_strdup_ext(pdc_core *pdc, const char *text, int flags, const char *fn)
{
    char *buf = NULL;

    if (text != NULL)
    {
        size_t len = pdc_strlen(text) + 1;
        size_t is = 0, it = 0;

        if ((flags & PDC_CONV_MAXSTRLEN) && len > PDC_ERR_MAXSTRLEN)
            len = PDC_ERR_MAXSTRLEN;

        if ((flags & PDC_CONV_NOBOM) && pdc_is_utf8_bytecode(text))
            is = 3;

        if ((flags & PDC_CONV_WITHBOM) && !pdc_is_utf8_bytecode(text))
            it = 3;

        len += it - is;
        if (flags & PDC_CONV_TMPALLOC)
            buf = (char *) pdc_malloc_tmp(pdc, len + 1, fn, NULL, NULL);
        else
            buf = (char *) pdc_malloc(pdc, len + 1, fn);

        memcpy(&buf[it], &text[is], len - it);
        buf[len] = 0;


        if (it == 3)
            pdc_copy_utf8_bom(buf);

    }

    return buf;
}

/* Convenience functions
*/
char *
pdc_strdup_tmp(pdc_core *pdc, const char *text)
{
    static const char fn[] = "pdc_strdup_tmp";

    return pdc_strdup_ext(pdc, text, PDC_CONV_TMPALLOC, fn);
}

char *
pdc_strdup_withbom(pdc_core *pdc, const char *text)
{
    static const char fn[] = "pdc_strdup_withbom";

    return pdc_strdup_ext(pdc, text, PDC_CONV_WITHBOM, fn);
}

/* Rapid function
*/
char *
pdc_strdup(pdc_core *pdc, const char *text)
{
    char *buf = NULL;
    static const char fn[] = "pdc_strdup";

    if (text != NULL)
    {
        size_t len = pdc_strlen(text) + 1;

        buf = (char *) pdc_malloc(pdc, len + 1, fn);
        memcpy(buf, text, len);
        buf[len] = 0;
    }

    return buf;
}

char *
pdc_strdup2(pdc_core *pdc, const char *text, size_t len)
{
    char *buf = NULL;
    static const char fn[] = "pdc_strdup2";

    if (text != NULL)
    {
        buf = (char *) pdc_malloc(pdc, len + 1, fn);
        memcpy(buf, text, len);
        buf[len] = 0;
    }

    return buf;
}

/* Convert Pascal string to a null terminated C string.
 * Size of C string: at least 256 bytes
 */
int
pdc_convert_pascal_str(const char *pstr, char *cstr)
{
    int len = (int) *((pdc_byte *) pstr);

    memcpy(cstr, pstr + 1, (size_t) len);
    cstr[len] = 0;

    return len;
}

char *
pdc_strdup_convert(pdc_core *pdc, pdc_encoding encto, pdc_encoding encfrom,
                   const char *text, int flags, const char *fn)
{
    pdc_encodingvector *evfrom, *evto;
    char *buf;
    size_t len;
    int i;

    evto = pdc_get_encoding_vector(pdc, encto);
    evfrom = pdc_get_encoding_vector(pdc, encfrom);
    buf = pdc_strdup_ext(pdc, text, flags, fn);
    len = strlen(buf);

    for (i = 0; i < (int) len; i++)
        buf[i] = (char) pdc_transform_bytecode(pdc, evto, evfrom,
                                               (pdc_byte) text[i]);

    return buf;
}

pdc_bool
pdc_logg_isprint(int c)
{
    if (c < 0x20 || (c >= 0x7F && c < 0xA0))
        return pdc_false;
    return pdc_true;
}


/*
 * Put out an arbitrary string.
 *
 * strform = readable:  Direct byte output with replacing not
 *                      printable bytes by their octal codes.
 *         = readable0: Like readable, but byte 0 will be displayed as space.
 *         = octal:     All bytes will be put out as octal.
 *         = hexa:      All bytes will be put out as hexadecimal value.
 *         = java:      Like readable, but Unicode strings and not printable
 *                      bytes will be put out in Java notation \uxxxx,
 *
 * Output string is temporarily allocated.
 *
 */
char *
pdc_strprint(pdc_core *pdc, const char *str, int leni, int maxchar,
             pdc_strform_kind strform)
{
    static const char fn[] = "pdc_strprint";

    if (str != NULL)
    {
        pdc_bool isunicode = pdc_false;
        int len = leni;

        if (!leni)
            len = (int) strlen(str);

        if (len)
        {
            pdc_strform_kind sf = strform;
            char *ts, *tmpstr;
            pdc_byte c = ' ', cp = '.';
            pdc_ushort *ush = (pdc_ushort *) str;
            int i, im;

            /* because of strform_java: \uxxxx: factor 6 */
            tmpstr = (char *) pdc_calloc_tmp(pdc, (size_t) (6 * (len + 4)), fn,
                                             NULL, NULL);
            ts = tmpstr;

            if (strform == strform_java)
            {
                if (leni && !(leni % 2))
                    isunicode = pdc_true;
                else
                    strform = strform_readable;
            }

            if (maxchar <= 0)
                maxchar = len;
            im = (maxchar < len) ? maxchar : len;
            if (isunicode)
                im = im/2;
            for (i = 0; i < im; i++)
            {
                if (isunicode)
                {
                    if (ush[i] > PDC_UNICODE_MAXLATIN1)
                    {
                        sf = strform_java;
                    }
                    else
                    {
                        c = (pdc_byte) ush[i];
                        sf = strform;
                    }
                }
                else
                {
                    c = (pdc_byte) str[i];
                    sf = strform;
                }

                switch (sf)
                {
                    case strform_hexa:
                    ts += sprintf(ts, "\\x%02X", c);
                    break;

                    case strform_octal:
                    ts += sprintf(ts, "\\%03o", c);
                    break;

                    case strform_java:
                    ts += sprintf(ts, "\\u%04X", ush[i]);
                    break;

                    default:
                    if (c == 0x00 && sf == strform_readable0)
                    {
                        c = 0x20;

                        *ts = (char) c;
                        ts++;
                    }
                    else
                    {
                        if (!pdc_logg_isprint((int) c))
                        {
                            if (isunicode)
                                ts += sprintf(ts, "\\u%04X", c);
                            else
                                ts += sprintf(ts, "\\%03o", c);
                        }
                        else
                        {
                            if (c == '"')
                            {
                                *ts = '\\';
                                ts++;
                            }

                            *ts = (char) c;
                            ts++;
                        }

                    }
                }
            }

            if (maxchar < len)
            {
                switch (strform)
                {
                    case strform_hexa:
                    ts += sprintf(ts, "\\x%02X\\x%02X\\x%02X", cp, cp, cp);
                    break;

                    case strform_octal:
                    ts += sprintf(ts, "\\%03o\\%03o\\%03o", cp, cp, cp);
                    break;

                    case strform_java:
                    ts += sprintf(ts, "\\u%04X\\u%04X\\u%04X", cp, cp, cp);
                    break;

                    default:
                    ts += sprintf(ts, "%c%c%c", cp, cp, cp);
                    break;
                }
            }

            return tmpstr;
        }
    }

    return (char *) pdc_calloc_tmp(pdc, 1, fn, NULL, NULL);
}

/*
 * Returned string is temporary allocated.
 */
const char *
pdc_utf8strprint(pdc_core *pdc, const char *str)
{
    static const char fn[] = "pdc_utf8strprint";

    return pdc_strdup_ext(pdc, str,
                  PDC_CONV_TMPALLOC | PDC_CONV_NOBOM | PDC_CONV_MAXSTRLEN, fn);
}

/*
 * Split a given text string into single strings which are separated by
 * arbitrary characters. This characters must be specified in a string.
 * If this string is NULL, " \f\n\r\t\v" (standard white spaces) is assumed.
 *
 * There is the convention that text inside braces {} will be taken verbatim.
 * Inside brace expressions braces must exist only in pairs. Braces are
 * masked by backslash.
 *
 * The caller is responsible for freeing the resultated string list
 * by calling the function pdc_cleanup_stringlist.
 *
 * Not for unicode strings.
 *
 * Return value: Number of strings.
 *               If braces aren't balanced the number is negative.
 *
 */
int
pdc_split_stringlist(pdc_core *pdc, const char *text, const char *i_separstr,
                     int flags, char ***stringlist)
{
    static const char fn[] = "pdc_split_stringlist";
    const char *separstr = " \f\n\r\t\v";
    const char *oldtext;
    char **strlist = NULL, *newtext = NULL;
    int it, len, jt = 0, jtb = 0, maxk = 0, count = 0, inside = 0;
    int ns, nbs = 0, nbss;

    if (stringlist)
        *stringlist = NULL;
    if (i_separstr)
        separstr = i_separstr;

    if (text == NULL)
        return 0;

    /* check for empty string */
    ns = (int) strspn(text, separstr);
    oldtext = &text[ns];
    len = (int) strlen(oldtext);
    if (!len)
        return 0;

    /* check for UTF-8-BOM */
    if (pdc_is_utf8_bytecode(oldtext))
    {
        oldtext = &text[ns + 3];
        len -= 3;
        ns = (int) strspn(oldtext, separstr);
        oldtext = &oldtext[ns];
        len -= ns;
        if (!len)
            return 0;
    }

    /* new string */
    if (stringlist != NULL)
        newtext = (char *) pdc_malloc(pdc, (size_t) (len + 1), fn);
    for (it = 0; it <= len; it++)
    {
        /* check for separators */
        if (it == len)
            ns = 1;
        else if (inside <= 0)
            ns = (int) strspn(&oldtext[it], separstr);
        else
            ns = 0;

        /* close text part */
        if (ns)
        {
            if (stringlist != NULL)
            {
                newtext[jt] = 0;
                if (count == maxk)
                {
                    maxk += 16;
                    strlist = (char **) pdc_realloc(pdc, strlist,
                                                    maxk * sizeof(char *), fn);
                }
                strlist[count] = &newtext[jtb];
            }
            count++;

            /* Exit */
            it += ns;
            if (it >= len ) break;

            /* new text part */
            jt++;
            jtb = jt;
        }

        /* option list */
        if (flags & PDC_SPLIT_ISOPTLIST)
        {
            /* save backslash counter */
            nbss = nbs;

            /* backslash */
            if (oldtext[it] == '\\')
            {
                nbs++;
                if (!(nbs % 2) && inside <= 1)
                    continue;
            }
            else
            {
                nbs = 0;
            }

            /* open and close brace */
            if (oldtext[it] == '{')
            {
                if (!(nbss % 2))
                {
                    inside++;
                    if (inside == 1)
                        continue;
                }
                else if (inside <= 1)
                {
                    jt--;
                }
            }
            else if (oldtext[it] == '}')
            {
                if (!(nbss % 2))
                {
                    inside--;
                    if (inside == 0)
                        continue;
                }
                else if (inside <= 1)
                {
                    jt--;
                }
            }
        }

        /* argument list */
        else if (flags & PDC_SPLIT_ISARGLIST)
        {
            /* save backslash counter */
            nbss = nbs;

            /* backslash */
            if (oldtext[it] == '\\')
            {
                nbs++;
                if (!(nbs % 2))
                    continue;
            }
            else
            {
                nbs = 0;
            }

            /* open and close quotation mark */
            if (oldtext[it] == '"')
            {
                if (!(nbss % 2))
                {
                    inside = 1 - inside;
                    continue;
                }
                else
                {
                    jt--;
                }
            }
        }

        /* save character */
        if (stringlist != NULL)
        {
            newtext[jt] = oldtext[it];
            jt++;
        }
    }

    if (stringlist != NULL)
        *stringlist = strlist;

    return inside ? -count : count;
}

void
pdc_cleanup_stringlist(pdc_core *pdc, char **stringlist)
{
    if(stringlist != NULL)
    {
        if(stringlist[0] != NULL)
            pdc_free(pdc, stringlist[0]);

        pdc_free(pdc, stringlist);
    }
}


/*
 * Substitute a list of variables in a string by its values recursively.
 * A variable begins with the character 'vchar' and ends at a character
 * in 'delimiters' or at the end of string resp..
 *
 * The character 'vchar' must be masked by 'vchar'.
 *
 * If at least one of a variable was substituted, a new allocated null
 * terminated string is returned. Otherwise the original pointer.
 *
 * The caller is responsible for freeing the new string.
 *
 *   string     null terminated string with variables
 *   vchar      begin character for a variable
 *   delimiters string with characters delimiting a variable name
 *   varslist   list of variable names
 *   valslist   list of variable values
 *   nvars      number of variables
 *   errind[2]  contains index and length of an unkown variable in string
 *
 */

static char *
substitute_variables(pdc_core *pdc, char *string, int ibeg, int *level,
    const char **varslist, const char **valslist, int nvars, char vchar,
    const char *separstr, int *errind)
{
    static const char fn[] = "substitue_variables";
    int i, j, l;

    j = ibeg;
    for (i = ibeg; string[i] != 0; i++)
    {
        if (string[i] == vchar)
        {
            if (string[i + 1] == vchar)
                i++;
            else
                break;
        }

        string[j] = string[i];
        j++;
    }

    if (string[i] != 0)
    {
        char *s = &string[i + 1];
        size_t n = strcspn(s, separstr);

        for (l = 0; l < nvars; l++)
        {
            if (n == strlen(varslist[l]) && !strncmp(s, varslist[l], n))
            {
                char *newstring;
                int k = (int) (i + n + 1);
                size_t nv = strlen(valslist[l]);
                size_t nr = strlen(&string[k]);
                size_t nb = (size_t) j +  nv + nr + 1;

                newstring = (char *) pdc_malloc(pdc, nb, fn);
                strncpy(newstring, string, (size_t) j);
                strncpy(&newstring[j], valslist[l], nv);
                strcpy(&newstring[j + nv], &string[k]);

                pdc_free(pdc, string);
                (*level)++;

                string = substitute_variables(pdc, newstring, j, level,
                               varslist, valslist, nvars, vchar, separstr,
                               errind);
                break;
            }
        }
        if (l == nvars)
        {
            errind[0] = i;
            errind[1] = (int) (n + 1);
        }
    }
    else
    {
        string[j] = 0;
    }
    return string;
}

char *
pdc_substitute_variables(pdc_core *pdc, const char *string, char vchar,
    const char *delimiters, const char **varslist,
    const char **valslist, int nvars, int *errind)
{
    static const char fn[] = "pdc_substitue_variables";
    char *subststr, *newstring, separstr[64];
    int level = 0;

    newstring = pdc_strdup_ext(pdc, string, 0, fn);

    separstr[0] = vchar;
    separstr[1] = 0;
    strcat(separstr, delimiters);

    errind[0] = -1;
    errind[1] = 0;
    subststr = substitute_variables(pdc, newstring, 0, &level,
                         varslist, valslist, nvars, vchar, separstr, errind);

    return subststr;
}

/*
 * Compares its arguments and returns an integer less than,
 * equal to, or greater than zero, depending on whether s1
 * is lexicographically less than, equal to, or greater than s2.
 * Null pointer values for s1 and s2 are treated the same as pointers
 * to empty strings.
 *
 * Presupposition: basic character set
 *
 * Return value:  < 0  s1 <  s2;
 *                = 0  s1 == s2;
 *                > 0  s1 >  s2;
 *
 */
int
pdc_strcmp(const char *s1, const char *s2)
{
    if (s1 == s2) return (0);
    if (s1 == NULL) return (-1);
    if (s2 == NULL) return (1);

    return strcmp(s1, s2);
}

int
pdc_stricmp(const char *s1, const char *s2)
{
    if (s1 == s2) return (0);
    if (s1 == NULL) return (-1);
    if (s2 == NULL) return (1);

    for (; *s1; ++s1, ++s2)
    {
        if (pdc_tolower(*s1) != pdc_tolower(*s2))
            break;
    }

    return (pdc_tolower(*s1) - pdc_tolower(*s2));
}

int
pdc_stricmp_a(const char *s1, const char *s2)
{
    if (s1 == s2) return (0);
    if (s1 == NULL) return (-1);
    if (s2 == NULL) return (1);

    for (; *s1; ++s1, ++s2)
    {
        if (pdc_tolower_a(*s1) != pdc_tolower_a(*s2))
            break;
    }

    return (pdc_tolower_a(*s1) - pdc_tolower_a(*s2));
}

/*
 * Same like pdc_strcmp, except that the strings can be
 * wide character strings with nulls and double null terminated.
 */
int
pdc_wstrcmp(const char *s1, const char *s2)
{
    size_t len1, len2, len;
    int res;

    if (s1 == s2) return (0);
    if (s1 == NULL) return (-1);
    if (s2 == NULL) return (1);

    len1 = pdc_strlen(s1);
    len2 = pdc_strlen(s2);
    len = MIN(len1, len2);

    res = memcmp(s1, s2, len);

    if (!res && len1 != len2)
        res = (len1 < len2) ? -1 : 1;

    return res;
}

/*
 * Compares its arguments and returns an integer less than,
 * equal to, or greater than zero, depending on whether s1
 * is lexicographically less than, equal to, or greater than s2.
 * But only up to n characters compared (n less than or equal
 * to zero yields equality).Null pointer values for s1 and s2
 * are treated the same as pointers to empty strings.
 *
 * Presupposition: basic character set
 *
 * Return value:  < 0  s1 <  s2;
 *                = 0  s1 == s2;
 *                > 0  s1 >  s2;
 *
 */
int
pdc_strincmp(const char *s1, const char *s2, int n)
{
    int i;

    if (s1 == s2)   return (0);
    if (s1 == NULL) return (-1);
    if (s2 == NULL) return (1);

    for (i = 0; i < n && *s1 && *s2; ++i, ++s1, ++s2)
    {
        if (pdc_tolower(*s1) != pdc_tolower(*s2))
            break;
    }

    return (i == n) ? 0 : (pdc_tolower(*s1) - pdc_tolower(*s2));
}

/*
 * pdc_strtrim removes trailing white space characters from an input string.
 * pdc_str2trim removes leading and trailing white space characters from an
 * input string..
 */
char *
pdc_strtrim(char *str)
{
    int i, n;

    n = (int) strlen(str);
    for (i = n - 1; i >= 0; i--)
        if (!pdc_isspace(str[i])) break;
    str[i + 1] = '\0';

    return str;
}

char *
pdc_str2trim(char *str)
{
    int i, n;

    n = (int) strlen(str);
    for (i = n - 1; i >= 0; i--)
        if (!pdc_isspace(str[i])) break;
    str[i + 1] = '\0';

    for (i = 0; ; i++)
        if (!pdc_isspace(str[i])) break;
    if (i > 0)
        memmove(str, &str[i], strlen(&str[i]) + 1);

    return str;
}

char *
pdc_strtoupper(char *str)
{
    int i, n;

    n = (int) strlen(str);
    for (i = 0; i < n; i++)
        str[i] = (char) pdc_toupper(str[i]);

    return str;
}

char *
pdc_strtolower(char *str)
{
    int i, n;

    n = (int) strlen(str);
    for (i = 0; i < n; i++)
        str[i] = (char) pdc_tolower(str[i]);

    return str;
}

void
pdc_swap_bytes2(const char *instring, int inlen, char *outstring)
{
    pdc_ushort *inp, *outp;
    int i;

    if (instring == NULL)
        return;

    if (outstring == NULL)
        outstring = (char *) instring;

    inp = (pdc_ushort *) instring;
    outp = (pdc_ushort *) outstring;

    inlen /= sizeof(pdc_ushort);
    for (i = 0; i < inlen; i++)
    {
        outp[i] = (pdc_ushort) (((inp[i] & (pdc_ushort)0x00FFu) << 8) |
                                ((inp[i] & (pdc_ushort)0xFF00u) >> 8));
    }
}

void
pdc_swap_bytes4(const char *instring, int inlen, char *outstring)
{
    pdc_uint32 *inp, *outp;
    int i;

    if (instring == NULL)
        return;

    if (outstring == NULL)
        outstring = (char *) instring;

    inp = (pdc_uint32 *) instring;
    outp = (pdc_uint32 *) outstring;

    inlen /= sizeof(pdc_uint32);
    for (i = 0; i < inlen; i++)
    {
        outp[i] = (pdc_uint32) (((inp[i] & (pdc_uint32)0x000000FFu) << 24) |
                                ((inp[i] & (pdc_uint32)0x0000FF00u) <<  8) |
                                ((inp[i] & (pdc_uint32)0x00FF0000u) >>  8) |
                                ((inp[i] & (pdc_uint32)0xFF000000u) >> 24));
    }
}

void
pdc_inflate_ascii(const char *instring, int inlen, char *outstring,
                  pdc_text_format textformat)
{
    int i, j;
    pdc_bool is_bigendian = (textformat == pdc_utf16be) ||
                            (textformat == pdc_utf16 && PDC_ISBIGENDIAN);

    j = 0;
    for (i = 0; i < inlen; i++)
    {
        if (is_bigendian)
        {
            outstring[j] = 0;
            j++;
            outstring[j] = instring[i];
        }
        else
        {
            outstring[j] = instring[i];
            j++;
            outstring[j] = 0;
        }
        j++;
    }
}

/*
 *  pdc_stresc --
 *      Remove from a string containing escaped non-printable cha-
 *      racters. The string must follows the C-standard escape
 *      mechanism: an escaped character is preceeded by an escape
 *      character which is a backslash '\' character and followed
 *      by one or more characters to define the non-printable
 *      character to be inserted here. The supported escapes are
 *
 *            \a              bell            (ASCII/EBCDIC-BEL)
 *            \b              backspace       (ASCII/EBCDIC-BS)
 *            \e              escape charater (ASCII/EBCDIC-ESC)
 *            \f              formfeed        (ASCII/EBCDIC-FF)
 *            \n              linefeed        (ASCII/EBCDIC-LF)
 *            \r              return          (ASCII/EBCDIC-CR)
 *            \t              tab character   (ASCII/EBCDIC-TAB)
 *            \v              vertical tab    (ASCII/EBCDIC-VT)
 *            \\              the slash itself
 *            \xnn            two hex digits n to define a
 *                            character numerically as ASCII/EBCDIC value.
 *            \nnn            three octal digits n to define a
 *                            character numerically as ASCII/EBCDIC value.
 *
 *      For example: \x0A, \x0a or \012 has the same effect in ASCII
 *      as \n (i.e linefeed).
 *      Note, if the last character in a string is the backslash
 *      then the backslash is illegal.
 *      The special characters a,b,e,f, and so on are recognized in
 *      lower case only.
 *
 *      textformat:
 *      pdc_bytes:      Latin1 or EBCDIC bytes on EBCDIC platforms
 *      pdc_utf8:       Latin1
 *      pdc_ebcdicutf8: EBCDIC - only on EBCDIC platforms
 *      pdc_utf16:      2 bytes Latin1
 *
 *      If a illegal escaped sequence was detected an exception will
 *      be thrown (verbose == pdc_true) or the sequence will be taken
 *      as it (verbose == pdc_false).
 *
*/

static const pdc_keyconn pdc_ascii_escape_keylist[] =
{
    {"\\",   0x5C},
    {"a",    0x07},
    {"b",    0x08},
    {"e",    0x1B},
    {"f",    0x0C},
    {"n",    0x0A},
    {"r",    0x0D},
    {"t",    0x09},
    {"v",    0x0B},
    {"x",    0x78},
    {NULL, 0}
};

pdc_ushort
pdc_get_string_value(pdc_byte *str, int i, int charlen)
{
    pdc_ushort retval = 0;

    if (charlen == 1)
    {
        retval = (pdc_ushort) str[i];
    }
    else
    {
        pdc_ushort *ustr = (pdc_ushort *) str;

        retval = ustr[i];
    }

    return retval;
}

/* return value: length of new string. -1: error
*/
int
pdc_subst_backslash(pdc_core *pdc, pdc_byte *str, int len,
                    pdc_encodingvector *ev, pdc_text_format textformat,
                    pdc_bool verbose)
{
    pdc_ushort *ustr = (pdc_ushort *) str;
    int charlen = (textformat == pdc_utf16) ? 2 : 1;
    pdc_byte bschar = '\\';
    pdc_ushort uv;
    int i, j, k, code;

    if (ev != NULL)
    {
        code = pdc_get_encoding_bytecode(pdc, ev, PDC_UNICODE_BACKSLASH);
        if (code != -1)
            bschar = (pdc_byte) code;
    }


    j = 0;
    len /= charlen;
    for (i = 0; i < len; i++)
    {
        uv = pdc_get_string_value(str, i, charlen);
        if (uv > PDC_UNICODE_MAXLATIN1)
        {
            ustr[j] = uv;
            j++;
            continue;
        }

        /* backslash found */
        if (uv == bschar)
        {
            pdc_byte escseq[4], stemp[6];
            pdc_bool kerror = pdc_false;

            i++;
            if (i < len)
            {
                uv = pdc_get_string_value(str, i, charlen);
                if (uv > PDC_UNICODE_MAXLATIN1)
                    goto PDC_OVERFLOW_EXIT;

                escseq[0] = (pdc_byte) uv;
                escseq[1] = 0;

                code = pdc_get_keycode((char *) escseq,
                                       pdc_ascii_escape_keylist);
                if (code != PDC_KEY_NOTFOUND)
                {
                    /* hex number */
                    if (code == 0x78)
                    {
                        for (k = 0; k < 2; k++)
                        {
                            i++;
                            if (i < len)
                            {
                                uv = pdc_get_string_value(str, i, charlen);
                                if (uv > PDC_UNICODE_MAXLATIN1)
                                    goto PDC_OVERFLOW_EXIT;
                            }
                            else
                            {
                                uv = 0;
                            }
                            escseq[k] = (pdc_byte) uv;
                        }
                        escseq[k] = 0;
                        if (i >= len ||
                            !pdc_str2integer((char *) escseq, PDC_INT_UNICODE,
                                             &uv))
                        {
                            strcpy((char *) stemp, "\\x");
                            strcat((char *) stemp, (char *) escseq);
                            kerror = pdc_true;
                        }
                    }
                    else
                    {
                        pdc_char c = (pdc_char) code;
                        uv = (pdc_ushort) (pdc_byte) c;
                    }
                }
                else
                {
                    /* octal number */
                    for (k = 0; k < 3; k++)
                    {
                        if (k) i++;
                        if (i < len)
                        {
                            uv = pdc_get_string_value(str, i, charlen);
                            if (uv > PDC_UNICODE_MAXLATIN1)
                                goto PDC_OVERFLOW_EXIT;
                        }
                        else
                        {
                            uv = 0;
                        }
                        escseq[k] = (pdc_byte) uv;
                    }
                    escseq[k] = 0;
                    if (i >= len ||
                        !pdc_str2integer((char *) escseq,
                                        PDC_INT_SHORT |
                                        PDC_INT_UNSIGNED |
                                        PDC_INT_OCTAL,
                                        &uv) ||
                        (charlen == 1 && uv > 0xFF))
                    {
                        strcpy((char *) stemp, "\\");
                        strcat((char *) stemp, (char *) escseq);
                        kerror = pdc_true;
                    }
                }
            }
            else
            {
                strcpy((char *) stemp, "\\");
                kerror = pdc_true;
            }

            /* error message */
            if (kerror)
            {
                pdc_set_errmsg(pdc, PDC_E_STR_ILL_ESCSEQ, (char *) stemp,
                               0, 0, 0);

                if (verbose)
                    pdc_error(pdc, -1, 0, 0, 0, 0);

                return -1;
            }
        }

        if (charlen == 1)
            str[j] = (pdc_byte) uv;
        else
            ustr[j] = uv;

        j++;
    }

    if (charlen == 1)
        str[j] = 0;
    else
        ustr[j] = 0;

    return charlen * j;

    PDC_OVERFLOW_EXIT:

    pdc_set_errmsg(pdc, PDC_E_STR_ILL_UNIESCSEQ,
                   pdc_errprintf(pdc, "%04X", uv), 0, 0, 0);

    if (verbose)
        pdc_error(pdc, -1, 0, 0, 0, 0);

    return -1;
}


/* ----------------------- number converting ----------------------- */

/*
 * pdc_str2double converts a null terminated and trimmed string
 * to a double precision number
 */
pdc_bool
pdc_str2double(const char *string, double *o_dz)
{
    const char *s = string;
    double dz = 0;
    int is = 1, isd = 0;

    *o_dz = 0;

    /* sign */
    if (*s == '-')
    {
        is = -1;
        s++;
    }
    else if (*s == '+')
        s++;

    if (!*s)
        return pdc_false;

    /* places before decimal point */
    isd = pdc_isdigit(*s);
    if (isd)
    {
        do
        {
            dz = 10 * dz + *s - '0';
            s++;
        }
        while (pdc_isdigit(*s));
    }

    /* decimal point */
    if (*s == '.' || *s == ',')
    {
        const char *sa;
        double adz = 0;

        s++;
        isd = pdc_isdigit(*s);
        if (!isd)
            return pdc_false;

        /* places after decimal point */
        sa = s;
        do
        {
            adz = 10 * adz + *s - '0';
            s++;
        }
        while (pdc_isdigit(*s));
        dz += adz / pow(10.0, (double)(s - sa));
    }

    /* power sign */
    if (*s == 'e' || *s == 'E')
    {
        s++;
        if (!isd)
            return pdc_false;

        /* sign */
        if (!*s)
        {
            dz *= 10;
        }
        else
        {
            int isp = 1;
            double pdz = 0, pdl = log10(dz);

            if (*s == '-')
            {
                isp = -1;
                s++;
            }
            else if (*s == '+')
                s++;

            if (!pdc_isdigit(*s))
                return pdc_false;
            do
            {
                pdz = 10 * pdz + *s - '0';
                s++;
            }
            while (pdc_isdigit(*s));


            if (*s || fabs(pdl + pdz) > 300.0)
                return pdc_false;

            dz *= pow(10.0, isp * pdz);
        }
    }
    else if(*s)
    {
        return pdc_false;
    }

    *o_dz = is * dz;
    return pdc_true;
}

/*
 * pdc_str2integer converts a null terminated and trimmed string
 * to an hexadecimal or decimal integer number of arbitrary size
 */
pdc_bool
pdc_str2integer(const char *string, int flags, void *o_iz)
{
    const char *s = string;
    double dz = 0;
    pdc_char cz = 0;
    pdc_short sz = 0;
    pdc_sint32 lz = 0;
    pdc_byte ucz = 0;
    pdc_ushort usz = 0;
    pdc_uint32 ulz = 0;
    int is = 1, lzd;

    if (flags & PDC_INT_CHAR)
        memcpy(o_iz, &cz, sizeof(pdc_char));
    else if (flags & PDC_INT_SHORT)
        memcpy(o_iz, &sz, sizeof(pdc_short));
    else
        memcpy(o_iz, &lz, sizeof(pdc_sint32));

    /* sign */
    if (*s == '-')
    {
        if (flags & PDC_INT_UNSIGNED)
            return pdc_false;
        is = -1;
        s++;
    }
    else if (*s == '+')
        s++;

    if (!*s)
        return pdc_false;

    /* hexadecimal test */
    if (!(flags & PDC_INT_DEC))
    {
        const char *ss = s;

        if (*s == '<')
            s += 1;
        else if (*s == 'x' || *s == 'X')
            s += 1;
        else if (!strncmp(s, "0x", 2) || !strncmp(s, "0X", 2))
            s += 2;
        if (s > ss)
        {
            if (!*s)
                return pdc_false;
            flags |= PDC_INT_HEXADEC;
        }
    }

    /* hexadecimal */
    if (flags & PDC_INT_HEXADEC)
    {
        while (pdc_isxdigit(*s))
        {
            if (pdc_isalpha(*s))
                lzd = (pdc_isupper(*s) ? 'A' : 'a') - 10;
            else
                lzd = '0';
            dz = 16 * dz + *s - lzd;
            s++;
        }
        if (*string == '<')
        {
            if (*s == '>')
                s += 1;
            else
                return pdc_false;
        }
    }

    /* octal */
    if (flags & PDC_INT_OCTAL)
    {
        while (pdc_isdigit(*s) && *s < '8')
        {
            dz = 8 * dz + *s - '0';
            s++;
        }
    }

    /* decimal */
    else
    {
        while (pdc_isdigit(*s))
        {
            dz = 10 * dz + *s - '0';
            s++;
        }
    }
    if (*s)
        return pdc_false;

    dz *= is;
    if (flags & PDC_INT_CHAR)
    {
        if (flags & PDC_INT_UNSIGNED)
        {
            if (dz > PDC_UCHAR_MAX)
                return pdc_false;
            ucz = (pdc_byte) dz;
            memcpy(o_iz, &ucz, sizeof(pdc_byte));
        }
        else
        {
            if (dz < PDC_SCHAR_MIN || dz > PDC_SCHAR_MAX)
                return pdc_false;
            cz = (pdc_char) dz;
            memcpy(o_iz, &cz, sizeof(pdc_char));
        }
    }
    else if (flags & PDC_INT_SHORT)
    {
        if (flags & PDC_INT_UNSIGNED)
        {
            if (dz > PDC_USHRT_MAX)
                return pdc_false;
            usz = (pdc_ushort) dz;
            memcpy(o_iz, &usz, sizeof(pdc_ushort));
        }
        else
        {
            if (dz < PDC_SHRT_MIN || dz > PDC_SHRT_MAX)
                return pdc_false;
            sz = (pdc_short) dz;
            memcpy(o_iz, &sz, sizeof(pdc_short));
        }
    }
    else
    {
        if (flags & PDC_INT_UNSIGNED)
        {
            if (dz > PDC_UINT_MAX)
                return pdc_false;
            ulz = (pdc_uint32) dz;
            memcpy(o_iz, &ulz, sizeof(pdc_uint32));
        }
        else
        {
            if (dz < PDC_INT_MIN || dz > PDC_INT_MAX)
                return pdc_false;
            lz = (pdc_sint32) dz;
            memcpy(o_iz, &lz, sizeof(pdc_sint32));
        }
    }

    return pdc_true;
}


pdc_bool
pdc_str2integer_ext(pdc_core *pdc, const char *string, int len,
                    int dupflags, int flags, void *o_iz)
{
    static const char fn[] = "pdc_str2integer_ext";
    char *dupstr;
    pdc_bool retval;

    dupstr = pdc_strdup_ext(pdc, string, dupflags, fn);
    dupstr[len] = 0;
    retval = pdc_str2integer(dupstr, flags, o_iz);
    pdc_free(pdc, dupstr);
    return retval;
}


static const char digits[] = "0123456789ABCDEF";

static char *
pdc_ltoa(char *buf, long n, int width, char pad, int base)
{
    char        aux[100];
    int         k, i = sizeof aux;
    char *      dest = buf;
    pdc_bool    sign;

    if (n == 0)
    {
        if (width == 0)
            width = 1;

        for (k = 0; k < width; ++k)
            *(dest++) = '0';

        return dest;
    }

    if (n < 0 && base == 10)
    {
        --width;
        sign = pdc_true;
        aux[--i] = digits[- (n % base)];
        n = n / -base;
    }
    else
    {
        sign = pdc_false;
        aux[--i] = digits[n % base];
        n = n / base;
    }

    while (0 < n)
    {
        aux[--i] = digits[n % base];
        n = n / base;
    }

    width -= (int) (sizeof aux) - i;
    for (k = 0; k < width; ++k)
        *(dest++) = pad;

    if (sign)
        *(dest++) = '-';

    memcpy(dest, &aux[i], sizeof aux - i);
    return dest + sizeof aux - i;
} /* pdc_ltoa */


static char *
pdc_uoff_t2a(
    char *	buf,
    pdc_uoff_t	n,
    int		width,
    char	pad,
    int		base,
    pdc_bool	left_justify)
{
    char        aux[100];
    int         k, i = sizeof aux;
    char *      dest = buf;

    while (0 < n)
    {
        aux[--i] = digits[n % base];
        n = n / base;
    }

    width -= (int) (sizeof aux) - i;

    if (!left_justify)
    {
	for (k = 0; k < width; ++k)
	    *(dest++) = pad;
    }

    memcpy(dest, &aux[i], sizeof aux - i);
    dest += sizeof aux - i;

    if (left_justify)
    {
	for (k = 0; k < width; ++k)
	    *(dest++) = pad;
    }

    return dest;
} /* pdc_uoff_t2a */


static char *
pdc_off_t2a(
    char *	buf,
    pdc_off_t	n,
    int		width,
    char	pad,
    pdc_bool	left_justify,
    pdc_bool	pos_sign)
{
    char        aux[100];
    int         k, i = sizeof aux;
    char *      dest = buf;
    pdc_bool    sign;

    if (n < 0)
    {
        --width;
        sign = pdc_true;
        aux[--i] = digits[- (n % 10)];
        n = n / -10;
    }
    else
    {
	if (pos_sign)
	    --width;

        sign = pdc_false;
        aux[--i] = digits[n % 10];
        n = n / 10;
    }

    while (0 < n)
    {
        aux[--i] = digits[n % 10];
        n = n / 10;
    }

    width -= (int) (sizeof aux) - i;

    if (!left_justify)
    {
	for (k = 0; k < width; ++k)
	    *(dest++) = pad;
    }

    if (sign)
    {
        *(dest++) = '-';
    }
    else if (pos_sign)
    {
        *(dest++) = '+';
    }

    memcpy(dest, &aux[i], sizeof aux - i);
    dest += sizeof aux - i;

    if (left_justify)
    {
	for (k = 0; k < width; ++k)
	    *(dest++) = pad;
    }

    return dest;
} /* pdc_off_t2a */


/*
 * pdc_ftoa converts a floating point number to string
 *
 * Because of historical reason "%f" = "%.12g".
 *
 * The function calls sprintf() and replaces
 * decimal comma by decimal point.
 *
 * If the number is infinite or not a number
 * "nan" will be set.
 *
 */

static char *
pdc_ftoa(pdc_core *pdc, const char *format, char *buf, double x)
{
    char *dest = buf;
    char *cd;
    int n;

    (void) pdc;

    /* check whether the number is valid */
    if (!PDC_ISFINITE(x))
    {
        strcpy(dest, "nan");
        return dest + 3;
    }

    /* standard C convert */
    if (!strcmp(format, "%f"))
        n = sprintf(dest, "%.12g", x);
    else
        n = sprintf(dest, format, x);

    /* normalized to decimal point */
    cd = strchr(dest, ',');
    if (cd != NULL)
        *cd = '.';

    return dest + n;
} /* pdc_ftoa */

/*
 * pdc_ftoa_pdfconf converts a floating point number to string
 * PDF conforming
 *
 */

static char *
pdc_ftoa_pdfconf(pdc_core *pdc, char *buf, double x)
{
    static const long pow10[] = { 1, 10, 100, 1000, 10000, 100000, 1000000 };
    char *      dest = buf;
    double      integ, fract, powd;
    int         ifd;
    long        f;

    /* check whether the number is valid */
    if (!PDC_ISFINITE(x))
        pdc_error(pdc, PDC_E_INT_ILLFLOAT, 0, 0, 0, 0);

    /* small number will be mapped to 0 */
    if (x < PDF_SMALLREAL && x > -PDF_SMALLREAL)
    {
        *dest = '0';
        return dest + 1;
    }

    /* negative number */
    if (x < 0)
    {
        x = -x;
        *(dest++) = '-';
    }

    /* large number is invalid or will be mapped to integer */
    if (x >= PDF_BIGREAL)
    {
        if (x > PDF_BIGINT)
            pdc_error(pdc, PDC_E_INT_FLOATTOOLARGE,
                      pdc_errprintf(pdc, "%f", x), 0, 0, 0);

        return pdc_ltoa(dest, (long) (x + 0.5), 0, ' ', 10);
    }

    ifd = pdc->floatdigits;
    powd = pow10[ifd];

    /* number <= 1/powd will be mappepd to 1/powd */
    if (x <= 1 / powd)
    {
        *(dest++) = '0';
        *(dest++) = '.';
        while (--ifd)
            *(dest++) = '0';
        *(dest++) = '1';
        return dest;
    }

    fract = modf(x, &integ);
    f = (long) (fract * powd + 0.5);

    if (f == powd)
    {
        integ += 1.0;
        f = 0;
    }

    if (integ == 0 && f == 0)   /* avoid "-0" */
        dest = buf;

    dest = pdc_ltoa(dest, (long) integ, 0, ' ', 10);

    if (f != 0)
    {
        char *  aux;
        long    rem;

        *(dest++) = '.';

        do      /* avoid trailing zeros */
        {
            rem = f % 10;
            f = f / 10;
            --ifd;
        } while (rem == 0);

        aux = dest + ifd + 1;
        dest[ifd--] = digits[rem];

        for (; 0 <= ifd; --ifd)
        {
            dest[ifd] = digits[f % 10];
            f = f / 10;
        }

        return aux;
    }

    return dest;
} /* pdc_ftoa_pdfconf */


/* flags for formatting function pdc_vxprintf()
*/
typedef enum
{
    pdc_form_nolimit,   /* no buffer limit supplied, no overflow check */
    pdc_form_fixlimit,  /* fix buffer limit, buffer overflow causes exception */
    pdc_form_varlimit   /* buffer overflow causes string truncation */
}
pdc_limitkind;

/* write to string or file
*/
static char *
write_sf(
    pdc_core *pdc,
    FILE *fp,
    pdc_limitkind ltd,
    char *dst,
    char *limit,
    const char *src,
    int n)
{
    if (fp != (FILE *) 0)
    {
	pdc_fwrite_ascii(pdc, src, (size_t) n, fp);
    }
    else
    {
        if (ltd != pdc_form_nolimit)
        {
            int avail = (int) (limit - dst);

            if (avail < n)
            {
                if (ltd == pdc_form_fixlimit)
                {
                    pdc_error(pdc, PDC_E_INT_FORMOVERFLOW, 0, 0, 0, 0);
                }
                else
                {
                    n = MAX(avail, 0);
                }
            }
        }

        if (n > 0)
        {
            memcpy(dst, src, (size_t) n);
            dst += n;
        }
    }

    return dst;
} /* write2buf */

static int
pdc_vxprintf(
    pdc_core *pdc,
    pdc_bool pdfconf,
    pdc_limitkind ltd,
    char *cp,
    size_t size,
    FILE *fp,
    const char *format,
    va_list args)
{
    static const char fn[] = "pdc_vxprintf";

    char buf[1024];
    char *dest = buf;
    int result = 0;
    char *limit = (char *) 0;

    if (cp != (char *) 0 && ltd != pdc_form_nolimit)
	limit = cp + (int) (size - 1);

    for (/* */ ; /* */ ; /* */)
    {
        int             width = 0;	/* = no width specified		*/
        int             prec = -1;	/* = no precision specified	*/
        char            pad = ' ';
        pdc_bool        left_justify = pdc_false;
        pdc_bool        pos_sign = pdc_false;

	char		fbuf[100];	/* format buffer for %f and %g	*/
	char *		fscan = fbuf;

        /* as long as there is no '%', just print.
        */
        while (*format != 0 && *format != '%')
            *(dest++) = *(format++);

	if (dest > buf)
	{
	    int inbuf = (int) (dest - buf);

	    cp = write_sf(pdc, fp, ltd, cp, limit, buf, inbuf);
	    result += inbuf;
	    dest = buf;
	}

        if (*format == 0)
        {
            if (cp != (char *) 0)
                *cp = 0;

            return result;
        }

	*(fscan++) = *(format++);	/* '%' */

        /* get the "flags", if any.
        */
	while (*format && strchr("+- #0", *format))
	{
	    switch (*format)
	    {
		case '-':	left_justify = pdc_true;
				break;

		case '+':	pos_sign = pdc_true;
				break;

		case '0':	pad = '0';
				break;

		default:	break;
	    }

	    *(fscan++) = *(format++);
	}

        /* get the "width", if present.
        */
        if (*format == '*')
        {
            width = va_arg(args, int);
            ++format;

	    if (width < 0)
	    {
		width = -width;

		if (!left_justify)
		{
		    *(fscan++) = '-';
		    left_justify = pdc_true;
		}
	    }

	    fscan += sprintf(fscan, "%d", width);
        }
        else
        {
            while (pdc_isdigit(*format))
	    {
                width = 10 * width + *format - '0';
		*(fscan++) = *(format++);
	    }
        }

	if (left_justify)
	    pad = ' ';

        /* get the "precision", if present.
        */
        if (*format == '.')
        {
            ++format;

            if (*format == '*')
            {
                prec = va_arg(args, int);
                ++format;

		if (prec >= 0)		/* ignore negative precision */
		{
		    fscan += sprintf(fscan, ".%d", prec);
		}
            }
            else if (pdc_isdigit(*format))
            {
		prec = 0;
		*(fscan++) = '.';

		do
		{
                    prec = 10 * prec + *format - '0';
		    *(fscan++) = *(format++);
		} while (pdc_isdigit(*format));
            }
        }

	*(fscan++) = *format;
	*fscan = 0;

        switch (*format)
        {
            case 'x':
            case 'X':
                dest = pdc_uoff_t2a(
                        dest, (pdc_uoff_t) va_arg(args, pdc_uint),
                        width, pad, 16, left_justify);
                break;

            case 'c':
                *(dest++) = (char) va_arg(args, int);
                break;

            case 'd':
                dest = pdc_off_t2a(dest, (pdc_off_t) va_arg(args, int),
                                   width, pad, left_justify, pos_sign);
                break;

            case 'u':
                dest = pdc_uoff_t2a(
                        dest, (pdc_uoff_t) va_arg(args, pdc_uint),
                        width, pad, 10, left_justify);
                break;

            case 'g':
            case 'f':
                if (pdfconf)
                {
                    dest = pdc_ftoa_pdfconf(pdc, dest, va_arg(args, double));
                }
                else
                {
                    dest = pdc_ftoa(pdc, fbuf, dest, va_arg(args, double));
                }
                break;

            case 'l':
            {
                pdc_off_t	n = 0;
                pdc_uoff_t	u = 0;
		pdc_bool	ll = pdc_false;

                if (*(++format) == 'l')
                {
		    ll = pdc_true;
                    ++format;
                }

		if (strchr("xXu", *format))
		{
		    if (ll)
			u = va_arg(args, pdc_uoff_t);
		    else
			u = va_arg(args, pdc_ulong);
		}
		else if (*format == 'd')
		{
		    if (ll)
			n = va_arg(args, pdc_off_t);
		    else
			n = va_arg(args, long);
		}
		else
		{
		    pdc_error(pdc, PDC_E_INT_BADFORMAT,
			pdc_errprintf(pdc, "l%c",
			    pdc_isprint((int) *format) ? *format : '?'),
			pdc_errprintf(pdc, "0x%02X", *format),
			0, 0);
		}

                switch (*format)
                {
                    case 'x':
                    case 'X':
                        dest = pdc_uoff_t2a(
				    dest, u, width, pad, 16, left_justify);
                        break;

                    case 'd':
                        dest = pdc_off_t2a(dest, n, width, pad,
					    left_justify, pos_sign);
                        break;

                    case 'u':
                        dest = pdc_uoff_t2a(
				    dest, u, width, pad, 10, left_justify);
                        break;

                    default:
			break;
                }

                break;
            }

            case 'p':
            {
	        char tmp[64];
                void *ptr = va_arg(args, void *);

		sprintf(tmp, "%p", ptr);
#if defined(AIX)
		if (strncmp(tmp, "0x", 2))
		    dest += sprintf(dest, "0x");
#endif
                dest += sprintf(dest, "%s", tmp);
                break;
            }

            case 'a':
            case 's':
            case 'T':
            {
                char *str = va_arg(args, char *);
                const char *cstr = str;
                pdc_bool tobefree = pdc_false;
                size_t len;
                int llen;

                if (str != 0)
                {
                    if (*format == 'T')
                    {
                        llen = va_arg(args, int);
                        cstr = pdc_print_loggstring(pdc, str, llen);
                    }
                    else if (*format == 'a')
                    {
                        cstr = pdc_strdup_ext(pdc, str, PDC_CONV_EBCDIC, fn);
                        tobefree = pdc_true;
                    }
                }
                else
                {
                    cstr = "(NULL)";
                    if (*format == 'T')
                        llen = va_arg(args, int);
                }

		len = strlen(cstr);

                if (prec != -1 && prec < (int) len)
		{
		    len = prec;
		}

                if (!left_justify && len < (size_t) width)
                {
		    int inbuf = (int) (width - len);

                    memset(buf, pad, (size_t) inbuf);
                    cp = write_sf(pdc, fp, ltd, cp, limit, buf, inbuf);
		    result += inbuf;
                }

                if (len != 0)
                {
		    result += (int) len;

                    if (fp != (FILE *) 0)
                    {
                        pdc_fwrite_ascii(pdc, cstr, len, fp);
                    }
                    else if (ltd == pdc_form_nolimit || result < (int) size)
                    {
                        memcpy(cp, cstr, len);
                        cp += (int) len;
                    }
                    else if (ltd == pdc_form_fixlimit)
                    {
                        pdc_error(pdc, PDC_E_INT_FORMOVERFLOW, 0, 0, 0, 0);
                    }
                    else if (cp < limit)
                    {
                        memcpy(cp, cstr, (size_t) (limit - cp));
                        cp = limit;
                    }

                    if (tobefree)
                        pdc_free(pdc, (char *) cstr);
                }

                if (left_justify && len < (size_t) width)
                {
		    int inbuf = (int) (width - len);

                    memset(buf, pad, (size_t) inbuf);
                    cp = write_sf(pdc, fp, ltd, cp, limit, buf, inbuf);
		    result += inbuf;
                }

                break;
            }

            case '%':
                *(dest++) = '%';
                break;

            default:
                pdc_error(pdc, PDC_E_INT_BADFORMAT,
                    pdc_errprintf(pdc, "%c", pdc_isprint((int) *format) ?
                                  *format : '?'),
                    pdc_errprintf(pdc, "0x%02X", *format),
                    0, 0);
        } /* switch */

        ++format;
    } /* loop */
} /* pdc_vxprintf */


/* ----------------------- formatted output ----------------------- */

/*
 * formatted output to file
 */
int
pdc_vfprintf(pdc_core *pdc, pdc_bool pdfconf, FILE *fp,
             const char *format, va_list args)
{
    return pdc_vxprintf(pdc, pdfconf, pdc_form_nolimit,
                        NULL, 0, fp, format, args);
} /* pdc_vfprintf */

int
pdc_fprintf(pdc_core *pdc, pdc_bool pdfconf, FILE *fp,
            const char *format, ...)
{
    int result;
    va_list ap;

    va_start(ap, format);
    result = pdc_vxprintf(pdc, pdfconf, pdc_form_nolimit,
                          NULL, 0, fp, format, ap);
    va_end(ap);

    return result;
} /* pdc_fprintf */


/*
 * formatted output to character string
 */
int
pdc_vsprintf(pdc_core *pdc, pdc_bool pdfconf, char *buf,
             const char *format, va_list args)
{
    return pdc_vxprintf(pdc, pdfconf, pdc_form_fixlimit,
                        buf, PDC_GEN_BUFSIZE, NULL, format, args);
} /* pdc_vsprintf */

int
pdc_sprintf(pdc_core *pdc, pdc_bool pdfconf, char *buf,
            const char *format, ...)
{
    int result;
    va_list ap;

    va_start(ap, format);
    result = pdc_vxprintf(pdc, pdfconf, pdc_form_nolimit,
                          buf, 0, NULL, format, ap);
    va_end(ap);

    return result;
} /* pdc_sprintf */

int
pdc_vsnprintf(pdc_core *pdc, char *buf, size_t size,
              const char *format, va_list args)
{
    return pdc_vxprintf(pdc, pdc_false, pdc_form_varlimit,
                        buf, size, NULL, format, args);
} /* pdc_vsnprintf */


/* --------------------- name tree handling ----------------------- */

struct pdc_branch_s
{
    char        *name;     /* name - must be allocated pointer */
    void        *data;     /* private data - must be allocated pointer */
    int          nalloc;   /* number of allocated kid structs */
    int          nkids;    /* number of kids */
    pdc_branch **kids;     /* kids */
    pdc_branch  *parent;   /* parent branch */
};

pdc_branch *
pdc_init_tree(pdc_core *pdc)
{
    return pdc_create_treebranch(pdc, NULL, "__tree__root__",
                                 NULL, 0, 0, NULL, NULL);
}

pdc_branch *
pdc_create_treebranch(pdc_core *pdc, pdc_branch *root, const char *pathname,
                      void *data, int flags, int size,
                      pdc_branch_error *errcode, const char **name_p)
{
    static const char fn[] = "pdc_create_branch";
    char *name = NULL;
    pdc_branch *branch = NULL;
    pdc_branch *kid = NULL;
    pdc_branch *parent = NULL;
    char **namelist;
    int i, j, k, nnames, nkids;

    if (errcode) *errcode = tree_ok;
    if (name_p) *name_p = "";

    if (root)
    {
        /* search for parent branch */
        parent = root;
        nnames = pdc_split_stringlist(pdc, pathname, PDC_NAME_SEPARSTRG, 0,
                                      &namelist);
        for (i = 0; i < nnames; i++)
        {
            /* parent branch must not be a leaf branch */
            if (!parent->nalloc)
            {
                if (errcode) *errcode = tree_isleaf;
                pdc_cleanup_stringlist(pdc, namelist);
                return NULL;
            }
            if (i == nnames - 1)
                break;

            name = namelist[i];
            if (name_p)
            {
                *name_p = pdc_utf8strprint(pdc, name);
            }

            nkids = parent->nkids;
            for (j = 0; j < nkids; j++)
            {
                kid = parent->kids[j];
                k = pdc_is_utf8_bytecode(kid->name) ? 3 : 0;
                if (!strcmp(&kid->name[k], name))
                {
                    parent = kid;
                    break;
                }
            }
            if (j == nkids)
            {
                if (errcode) *errcode = tree_notfound;
                pdc_cleanup_stringlist(pdc, namelist);
                return NULL;
            }
        }

        if (pdc_is_utf8_bytecode(pathname))
            name = pdc_strdup_withbom(pdc, namelist[nnames - 1]);
        else
            name = pdc_strdup(pdc, namelist[nnames - 1]);
        pdc_cleanup_stringlist(pdc, namelist);

        /* kids must have different names */
        for (j = 0; j < parent->nkids; j++)
        {
            kid = parent->kids[j];
            if (!strcmp(kid->name, name))
            {
                if (errcode) *errcode = tree_nameexists;
                if (name_p) *name_p = pdc_utf8strprint(pdc, name);

                pdc_free(pdc, name);
                return NULL;
            }
        }
    }
    else
    {
        parent = NULL;
        name = pdc_strdup(pdc, pathname);
    }

    branch = (pdc_branch *) pdc_malloc(pdc, sizeof(pdc_branch), fn);
    branch->name = name;
    branch->data = data;
    if (flags & PDC_TREE_ISLEAF)
    {
        branch->nalloc = 0;
        branch->nkids = 0;
        branch->kids = NULL;
    }
    else
    {
        branch->nalloc = PDC_KIDS_CHUNKSIZE;
        branch->nkids = 0;
        branch->kids = (pdc_branch **) pdc_malloc(pdc,
                            branch->nalloc * sizeof(pdc_branch *), fn);
    }
    branch->parent = parent;

    /* insert kid */
    if (parent)
    {
        if (parent->nkids == parent->nalloc)
        {
            parent->nalloc *= 2;
            parent->kids = (pdc_branch **) pdc_realloc(pdc, parent->kids,
                                parent->nalloc * sizeof(pdc_branch *), fn);
        }
        parent->kids[parent->nkids] = branch;
        (parent->nkids)++;

        if ((flags & PDC_TREE_INHERIT) && parent->data)
            memcpy(branch->data, parent->data, (size_t) size);
    }

    return branch;
}

void
pdc_deactivate_name_treebranch(pdc_core *pdc, pdc_branch *branch)
{
    static const char fn[] = "pdc_deactivate_name_treebranch";
    size_t len = strlen(branch->name);

    branch->name = (char *) pdc_realloc(pdc, branch->name, len + 2, fn);
    branch->name[len] = PDC_NAME_SEPARSIGN;
    branch->name[len+1] = 0;
}

char *
pdc_get_name_treebranch(pdc_branch *branch)
{
    return branch->name;
}

pdc_branch *
pdc_get_parent_treebranch(pdc_branch *branch)
{
    return branch->parent;
}

void *
pdc_get_data_treebranch(pdc_branch *branch)
{
    return branch->data;
}

pdc_branch **
pdc_get_kids_treebranch(pdc_branch *branch, int *nkids)
{
    *nkids = branch->nkids;
    return branch->kids;
}

void
pdc_cleanup_treebranch(pdc_core *pdc, pdc_branch *branch)
{
    int i;

    if (branch->name)
        pdc_free(pdc, branch->name);

    if (branch->data)
        pdc_free(pdc, branch->data);

    if (branch->kids)
    {
        for(i = 0; i < branch->nkids; i++)
            pdc_cleanup_treebranch(pdc, branch->kids[i]);
        pdc_free(pdc, branch->kids);
    }

    pdc_free(pdc, branch);
}

/***************************** memory pools *****************************/

/* the data structures and functions in this section are more than
** confusing. the funny "mp_item" structure below makes them more
** readable, believe it or not.
*/
typedef struct mp_item_s mp_item;

struct mp_item_s
{
    mp_item *   next;
};

struct pdc_mempool_s
{
    pdc_core *  pdc;

    char **     pool_tab;
    mp_item *   free_list;

    size_t      pool_incr;      /* pool growth chunk size (items)       */

    size_t      ptab_cap;       /* total # of slots in pool_tab         */
    size_t      ptab_size;      /* used # of slots in pool_tab          */
    size_t      ptab_incr;      /* pool_tab growth chunk size (slots)   */

    size_t      item_size;      /* size of a single item (bytes)        */
};

#undef  COMMENT
#ifdef  COMMENT

    pool_incr   = 5
    ptab_incr   = 4
    ptab_cap    = 4     (1 * ptab_incr)


    +------+
    | free |
    +------+         +----------------------------------+
    | free |    +--> |      |      |      | free | free |
    +------+    |    +----------------------------------+
    |      | ---+
    +------+         +----------------------------------+
    |      | ------> |      |      |      |      |      |
    +------+         +----------------------------------+

    pool_tab

#endif  /* COMMENT */


pdc_mempool *
pdc_mp_new(pdc_core *pdc, size_t item_size)
{
    static const char fn[] = "pdc_mp_new";

    int m;
    pdc_mempool *mp = (pdc_mempool *)
        pdc_malloc(pdc, sizeof (pdc_mempool), fn);

    if (mp != (pdc_mempool *) 0)
    {
        /* round up 'item_size' to a multiple of 'sizeof (mp_item)'
        ** to ensure proper alignment.
        */
        if ((m = (int) (item_size % sizeof (mp_item))) != 0)
            item_size += sizeof (mp_item) - m;

        mp->pdc = pdc;

        mp->pool_tab = (char **) 0;
        mp->free_list = (mp_item *) 0;
        mp->pool_incr = 1000;

        mp->ptab_cap = 0;
        mp->ptab_size = 0;
        mp->ptab_incr = 100;

        mp->item_size = item_size;
    }

    return mp;
} /* pdc_mp_new */


void
pdc_mp_delete(pdc_mempool *mp)
{
    /* TODO: exception if there are still alloc'd items in the pool?    */
    /* or, the other way round, call destructors?                       */

    pdc_core *  pdc = mp->pdc;
    int         i;

    for (i = 0; i < (int) mp->ptab_size; ++i)
        pdc_free(pdc, mp->pool_tab[i]);

    if (mp->pool_tab)
        pdc_free(pdc, mp->pool_tab);

    pdc_free(pdc, mp);
} /* pdc_mp_delete */


void *
pdc_mp_alloc(pdc_mempool *mp)
{
    static const char fn[] = "pdc_mp_alloc";

    pdc_core *  pdc = mp->pdc;
    mp_item *   result;

    if (!mp->free_list)
    {
        char *  new_chunk;
        int     i;

        if (mp->ptab_size == mp->ptab_cap)
        {
            mp->ptab_cap += mp->ptab_incr;

            mp->pool_tab = (char **) pdc_realloc(pdc,
                mp->pool_tab, mp->ptab_cap * sizeof (char **), fn);
        }

        new_chunk = mp->pool_tab[mp->ptab_size] = (char *)
            pdc_malloc(pdc, mp->pool_incr * mp->item_size, fn);

        ++mp->ptab_size;
        mp->free_list = (mp_item *) new_chunk;
        mp->free_list->next = (mp_item *) 0;

        for (i = 1; i < (int) mp->pool_incr; ++i)
        {
            mp_item *scan = (mp_item *) (new_chunk + i * mp->item_size);

            scan->next = mp->free_list;
            mp->free_list = scan;
        }
    }

    result = mp->free_list;
    mp->free_list = result->next;

    return (void *) result;
} /* pdc_mp_alloc */


void
pdc_mp_free(pdc_mempool *mp, void *item)
{
    mp_item *mpi = (mp_item *) item;

    mpi->next = mp->free_list;
    mp->free_list = mpi;
} /* pdc_mp_free */


/***************************** miscellaneous ****************************/

/* search a sorted (strcmp order) array "names" of size "size"
** for string "name". return the index if found, otherwise -1.
*/
int
pdc_name2idx(const char **names, int size, const char *name)
{
    int lo = 0, hi = size;

    while (lo != hi)
    {
        int idx = (lo + hi) / 2;
        int cmp = strcmp(name, names[idx]);

        if (cmp == 0)
            return idx;

        if (cmp < 0)
            hi = idx;
        else
            lo = idx + 1;
    }

    return -1;
} /* pdc_name2idx */


/* linear search; see man page LSEARCH(3).
*/
void *
pdc_lfind(
    const void *key,
    const void *base,
    size_t *    nmemb,
    size_t      size,
    int       (*compar)(const void *, const void *))
{
    size_t i;

    for (i = 0; i < *nmemb; ++i)
    {
        const char *cp = (const char *) base + i * size;

        if (compar(key, (void *) cp) == 0)
            return (void *) cp;
    }

    return (void *) 0;
} /* pdc_lfind */


/********************* pseudo random numbers *********************/

int
pdc_rand(pdc_core *pdc)
{
    pdc->last_rand = pdc->last_rand * 1103515245 + 12345;

    return (pdc_uint)(pdc->last_rand / 65536) % 32768;
} /* pdc_rand */

void
pdc_srand(pdc_core *pdc, pdc_uint seed)
{
    pdc->last_rand = seed;
} /* pdc_srand */
