/****************************************************************************
 *                                                                          *
 * File    : ieee754.c                                                      *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; IEEE 754 support.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define MANT_WORDS  6       /* 64 bits + 32 for accuracy == 96 bits */
#define MANT_DIGITS  28     /* 29 digits don't fit in 96 bits */

/* #define put(a,b) ((*(a)=(b)), ((a)[1]=(b)>>8)) */
__inline void put(uchar_t *a, long /* only 16 bits used */ b)
{
    a[0] = (uchar_t)(b);
    a[1] = (uchar_t)(b >> 8);
}

/* Static function prototypes */
static int to_float(const char *, long, uchar_t *);
static int to_double(const char *, long, uchar_t *);
static int to_longdouble(const char *, long, uchar_t *);
static void convert(const char *, ushort_t *, long *);
static void shr(ushort_t *, int);
static int round(ushort_t *, int);
static int mul(ushort_t *, ushort_t *);

/****************************************************************************
 *                                                                          *
 * Function: asmflt                                                         *
 *                                                                          *
 * Purpose : Convert a string to a floating-point value.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int asmflt(const char *number, long sign, uchar_t *result, int size)
{
    if (size == 4)
        return to_float(number, sign, result);
    else if (size == 8)
        return to_double(number, sign, result);
    else if (size == 10)
        return to_longdouble(number, sign, result);
    else
    {
        apperror(RCFATAL(ERROR_INTERNAL), "asmflt()");
        return 0;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: to_float                                                       *
 *                                                                          *
 * Purpose : Convert a string to a 'float' floating-point value.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int to_float(const char *str, long sign, uchar_t *result)
{
    ushort_t mant[MANT_WORDS];
    long exponent;

    sign = (sign < 0) ? 0x8000L : 0L;

    convert(str, mant, &exponent);
    if (mant[0] & 0x8000)
    {
        /*
         * Non-zero.
         */
        exponent--;
        if (exponent >= -126 && exponent <= 128)
        {
            /*
             * Normalised.
             */
            exponent += 127;
            shr(mant, 8);
            round(mant, 2);
            if (mant[0] & 0x100)  /* did we scale up by one? */
                shr(mant, 1), exponent++;
            mant[0] &= 0x7F;  /* remove leading one */
            put(result+2, (exponent << 7) | mant[0] | sign);
            put(result+0, mant[1]);
        }
        else if (exponent < -126 && exponent >= -149)
        {
            /*
             * Denormal.
             */
            int shift = -(exponent + 118);
            int sh = shift % 16, wds = shift / 16;
            shr(mant, sh);
            if (round(mant, 2-wds) || (sh > 0 && (mant[0] & (0x8000 >> (sh-1)))))
            {
                shr(mant, 1);
                if (sh == 0)
                    mant[0] |= 0x8000;
                exponent++;
            }
            put(result+2, (wds == 0 ? mant[0] : 0) | sign);
            put(result+0, (wds <= 1 ? mant[1-wds] : 0));
        }
        else
        {
            if (exponent > 0)
            {
                apperror(RCERROR(ERROR_OVERFLOW_IN_FP_CONSTANT), str, strlen(str));
                return 0;
            }
            else
            {
                memset(result, 0, 4);
            }
        }
    }
    else
    {
        /*
         * Zero.
         */
        memset(result, 0, 4);
    }

    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: to_double                                                      *
 *                                                                          *
 * Purpose : Convert a string to a 'double' floating-point value.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int to_double(const char *str, long sign, uchar_t *result)
{
    ushort_t mant[MANT_WORDS];
    long exponent;

    sign = (sign < 0) ? 0x8000L : 0L;

    convert(str, mant, &exponent);
    if (mant[0] & 0x8000)
    {
        /*
         * Non-zero.
         */
        exponent--;
        if (exponent >= -1022 && exponent <= 1024)
        {
            /*
             * Normalised.
             */
            exponent += 1023;
            shr(mant, 11);
            round(mant, 4);
            if (mant[0] & 0x20)  /* did we scale up by one? */
                shr(mant, 1), exponent++;
            mant[0] &= 0xF;  /* remove leading one */
            put(result+6, (exponent << 4) | mant[0] | sign);
            put(result+4, mant[1]);
            put(result+2, mant[2]);
            put(result+0, mant[3]);
        }
        else if (exponent < -1022 && exponent >= -1074)
        {
            /*
             * Denormal.
             */
            int shift = -(exponent + 1011);
            int sh = shift % 16, wds = shift / 16;
            shr(mant, sh);
            if (round(mant, 4-wds) || (sh > 0 && (mant[0] & (0x8000 >> (sh-1)))))
            {
                shr(mant, 1);
                if (sh == 0)
                    mant[0] |= 0x8000;
                exponent++;
            }
            put(result+6, (wds == 0 ? mant[0] : 0) | sign);
            put(result+4, (wds <= 1) ? mant[1-wds] : 0);
            put(result+2, (wds <= 2) ? mant[2-wds] : 0);
            put(result+0, (wds <= 3) ? mant[3-wds] : 0);
        }
        else
        {
            if (exponent > 0)
            {
                apperror(RCERROR(ERROR_OVERFLOW_IN_FP_CONSTANT), str, strlen(str));
                return 0;
            }
            else
            {
                memset(result, 0, 8);
            }
        }
    }
    else
    {
        /*
         * Zero.
         */
        memset(result, 0, 8);
    }

    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: to_longdouble                                                  *
 *                                                                          *
 * Purpose : Convert a string to a 'long double' floating-point value.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int to_longdouble(const char *str, long sign, uchar_t *result)
{
    ushort_t mant[MANT_WORDS];
    long exponent;

    sign = (sign < 0) ? 0x8000L : 0L;

    convert(str, mant, &exponent);
    if (mant[0] & 0x8000)
    {
        /*
         * Non-zero.
         */
        exponent--;
        if (exponent >= -16383 && exponent <= 16384)
        {
            /*
             * Normalised.
             */
            exponent += 16383;
            if (round(mant, 4))  /* did we scale up by one? */
                shr(mant, 1), mant[0] |= 0x8000, exponent++;
            put(result+8, exponent | sign);
            put(result+6, mant[0]);
            put(result+4, mant[1]);
            put(result+2, mant[2]);
            put(result+0, mant[3]);
        }
        else if (exponent < -16383 && exponent >= -16446)
        {
            /*
             * Denormal.
             */
            int shift = -(exponent+16383);
            int sh = shift % 16, wds = shift / 16;
            shr(mant, sh);
            if (round(mant, 4-wds) || (sh > 0 && (mant[0] & (0x8000 >> (sh-1)))))
            {
                shr(mant, 1);
                if (sh == 0)
                    mant[0] |= 0x8000;
                exponent++;
            }
            put(result+8, sign);
            put(result+6, (wds == 0) ? mant[0] : 0);
            put(result+4, (wds <= 1) ? mant[1-wds] : 0);
            put(result+2, (wds <= 2) ? mant[2-wds] : 0);
            put(result+0, (wds <= 3) ? mant[3-wds] : 0);
        }
        else
        {
            if (exponent > 0)
            {
                apperror(RCERROR(ERROR_OVERFLOW_IN_FP_CONSTANT), str, strlen(str));
                return 0;
            }
            else
            {
                memset(result, 0, 10);
            }
        }
    }
    else
    {
        /*
         * Zero.
         */
        memset(result, 0, 10);
    }

    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: convert                                                        *
 *                                                                          *
 * Purpose : Actually convert a string to a floating-point value.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void convert(const char *str, ushort_t *mant, long *exponent)
{
    char digits[MANT_DIGITS];
    char *p, *q, *r;
    ushort_t mult[MANT_WORDS], bit;
    ushort_t *m;
    long tenpwr, twopwr;
    int extratwos;
    bool_t seendot;
    bool_t started;

    p = digits;
    tenpwr = 0;
    started = seendot = FALSE;
    while (*str && *str != 'E' && *str != 'e')
    {
        if (*str == '.')
        {
            if (!seendot)
            {
                seendot = TRUE;
            }
            else
            {
                apperror(RCERROR(ERROR_BADLY_FORMED_FP_CONSTANT));
                return;
            }
        }
        else if (*str >= '0' && *str <= '9')
        {
            if (*str == '0' && !started)
            {
                if (seendot)
                    tenpwr--;
            }
            else
            {
                started = TRUE;

                if (p < digits+sizeof(digits))
                    *p++ = *str - '0';

                if (!seendot)
                    tenpwr++;
            }
        }
        else
        {
            apperror(RCERROR(ERROR_BADLY_FORMED_FP_CONSTANT));
            return;
        }

        str++;
    }

    if (*str)
    {
        str++;  /* eat the E */
        tenpwr += atoi(str);
    }

    /*
     * At this point, the memory interval [digits,p) contains a
     * series of decimal digits zzzzzzz such that our number X
     * satisfies.
     *
     * X = 0.zzzzzzz * 10^tenpwr
     */

    bit = 0x8000;
    for (m = mant; m < mant+MANT_WORDS; m++)
        *m = 0;
    m = mant;
    q = digits;
    started = FALSE;
    twopwr = 0;
    while (m < mant+MANT_WORDS)
    {
        ushort_t carry = 0;

        while (p > q && !p[-1])
            p--;

        if (p <= q)
            break;

        for (r = p; r-- > q ;)
        {
            int i;

            i = 2 * *r + carry;
            if (i >= 10)
                carry = 1, i -= 10;
            else
                carry = 0;
            *r = i;
        }

        if (carry)
            *m |= bit, started = TRUE;

        if (started)
        {
            if (bit == 1)
                bit = 0x8000, m++;
            else
                bit >>= 1;
        }
        else
        {
            twopwr--;
        }
    }
    twopwr += tenpwr;

    /*
     * At this point the 'mant' array contains the first six
     * fractional places of a base-2^16 real number, which when
     * multiplied by 2^twopwr and 5^tenpwr gives X. So now we
     * really do multiply by 5^tenpwr.
     */

    if (tenpwr < 0)
    {
        for (m = mult; m < mult+MANT_WORDS; m++)
            *m = 0xCCCC;
        extratwos = -2;
        tenpwr = -tenpwr;
    }
    else if (tenpwr > 0)
    {
        mult[0] = 0xA000;
        for (m = mult+1; m < mult+MANT_WORDS; m++)
            *m = 0;
        extratwos = 3;
    }
    else
    {
        extratwos = 0;
    }

    while (tenpwr)
    {
        if (tenpwr & 1)
            twopwr += extratwos + mul(mant, mult);
        extratwos = extratwos * 2 + mul(mult, mult);
        tenpwr >>= 1;
    }

    /*
     * Conversion is done. The elements of 'mant' contain the first
     * fractional places of a base-2^16 real number in [0.5,1)
     * which we can multiply by 2^twopwr to get X. Or, of course,
     * it contains zero.
     */
    *exponent = twopwr;
}

/****************************************************************************
 *                                                                          *
 * Function: shr                                                            *
 *                                                                          *
 * Purpose : Shift a mantissa to the right by given bits.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void shr(ushort_t *mant, int i)
{
    ushort_t n = 0, m;
    int j;

    for (j = 0; j < MANT_WORDS; j++)
    {
        m = (mant[j] << (16-i)) & 0xFFFF;
        mant[j] = (mant[j] >> i) | n;
        n = m;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: round                                                          *
 *                                                                          *
 * Purpose : Round off a mantissa after given words.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int round(ushort_t *mant, int i)
{
    if (mant[i] & 0x8000)
    {
        do {
            ++mant[--i];
            mant[i] &= 0xFFFF;
        } while (i > 0 && !mant[i]);
        return !i && !mant[i];
    }
    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: mul                                                            *
 *                                                                          *
 * Purpose : Floating-point multiplication.                                 *
 *                                                                          *
 * Comment : Guaranteed top bit of from is set, we only have to             *
 *           worry about *one* bit shift to the left.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int mul(ushort_t *to, ushort_t *from)
{
    ulong_t temp[MANT_WORDS*2];
    int i, j;

    for (i = 0; i < MANT_WORDS*2; i++)
        temp[i] = 0;

    for (i = 0; i < MANT_WORDS; i++)
    {
        for (j = 0; j < MANT_WORDS; j++)
        {
            ulong_t n;

            n = (ulong_t)to[i] * (ulong_t)from[j];
            temp[i+j] += n >> 16;
            temp[i+j+1] += n & 0xFFFF;
        }
    }

    for (i = MANT_WORDS*2; --i; )
    {
        temp[i-1] += temp[i] >> 16;
        temp[i] &= 0xFFFF;
    }

    if (temp[0] & 0x8000)
    {
        for (i = 0; i < MANT_WORDS; i++)
            to[i] = (ushort_t)(temp[i] & 0xFFFF);
        return 0;
    }
    else
    {
        for (i = 0; i < MANT_WORDS; i++)
            to[i] = (ushort_t)((temp[i] << 1) + !!(temp[i+1] & 0x8000));
        return -1;
    }
}

