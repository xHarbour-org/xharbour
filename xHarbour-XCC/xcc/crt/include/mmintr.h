#ifndef _MMINTR_H
#define _MMINTR_H

/* mmintr.h - private header for MMX extensions */

#if __POCC__ >= 280 && !defined(_M_ARM)
typedef union __declspec(align(8)) __m64 {
    unsigned long long m64_u64;
    float m64_f32[2];
    signed char m64_i8[8];
    signed short m64_i16[4];
    signed int m64_i32[2];
    signed long long m64_i64;
    unsigned char m64_u8[8];
    unsigned short m64_u16[4];
    unsigned int m64_u32[2];
} __m64;
#endif

#endif /* _MMINTR_H */
