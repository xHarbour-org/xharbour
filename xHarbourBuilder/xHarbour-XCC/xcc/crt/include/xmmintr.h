#ifndef _XMMINTR_H
#define _XMMINTR_H

/* xmmintr.h - private header for SSE, SSE2 extensions */

#ifndef _MMINTR_H
#include <mmintr.h>
#endif

#if __POCC__ >= 280 && !defined(_M_ARM)
typedef struct __declspec(align(16)) __m128 {
    float m128_f32[4];
    double m128_f64[2];
    signed char m128_i8[16];
    signed short m128_i16[8];
    signed int m128_i32[4];
    signed long long m128_i64[2];
    unsigned char m128_u8[16];
    unsigned short m128_u16[8];
    unsigned int m128_u32[4];
    unsigned long long m128_u64[2];
} __m128;

typedef struct __declspec(align(16)) __m128i {  /* subset of the above */
    signed char m128i_i8[16];
    signed short m128i_i16[8];
    signed int m128i_i32[4];
    signed long long m128i_i64[2];
    unsigned char m128i_u8[16];
    unsigned short m128i_u16[8];
    unsigned int m128i_u32[4];
    unsigned long long m128i_u64[2];
} __m128i;

typedef struct __declspec(align(16)) __m128d {    /* subset of the above */
    double m128d_f64[2];
} __m128d;
#endif

#endif /* _XMMINTR_H */
