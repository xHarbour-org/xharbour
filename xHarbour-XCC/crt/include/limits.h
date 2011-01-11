#ifndef _LIMITS_H
#define _LIMITS_H

/* limits.h - C99 standard header */

#define CHAR_BIT    8                       /* number of bits in a char */
#ifdef _CHAR_UNSIGNED
#define CHAR_MAX    UCHAR_MAX               /* maximum char value */
#define CHAR_MIN    0                       /* minimum char value */
#else
#define CHAR_MAX    SCHAR_MAX               /* maximum char value */
#define CHAR_MIN    SCHAR_MIN               /* minimum char value */
#endif /* _CHAR_UNSIGNED */
#define SCHAR_MAX   0x7f                    /* maximum signed char value */
#define SCHAR_MIN   (-SCHAR_MAX - 1)        /* minimum signed char value */
#define UCHAR_MAX   0xff                    /* maximum unsigned char value */

#define MB_LEN_MAX  8                       /* maximum number of bytes in multibyte char */

#define INT_MAX     LONG_MAX                /* maximum signed int value */
#define INT_MIN     LONG_MIN                /* minimum signed int value */
#define SHRT_MAX    0x7fff                  /* maximum signed short value */
#define SHRT_MIN    (-SHRT_MAX - 1)         /* minimum signed short value */
#define LONG_MAX    0x7fffffff              /* maximum signed long value */
#define LONG_MIN    (-LONG_MAX - 1)         /* minimum signed long value */
#ifdef _M_ARM
#define LLONG_MAX   LONG_MAX                /* maximum signed long long value */
#define LLONG_MIN   LONG_MIN                /* minimum signed long long value */
#else
#define LLONG_MAX   0x7fffffffffffffff      /* maximum signed long long value */
#define LLONG_MIN   (-LLONG_MAX - 1)        /* minimum signed long long value */
#endif /* _M_ARM */

#define USHRT_MAX   0xffff                  /* maximum unsigned short value */
#define UINT_MAX    0xffffffff              /* maximum unsigned int value */
#define ULONG_MAX   0xffffffff              /* maximum unsigned long value */
#ifdef _M_ARM
#define ULLONG_MAX  0xffffffff              /* maximum unsigned long long value */
#else
#define ULLONG_MAX  0xffffffffffffffff      /* maximum unsigned long long value */
#endif /* _M_ARM */

/* MSVC compatibility. */

#define _I64_MIN    LLONG_MIN
#define _I64_MAX    LLONG_MAX
#define _UI64_MAX   ULLONG_MAX

#endif /* _LIMITS_H */

