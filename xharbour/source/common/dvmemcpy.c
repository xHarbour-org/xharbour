/*
 * $Id$
 */

/*
1. Introduction

A co-worker of mine, Fredrik Bredberg, once made an implementation of memcpy()
and he was very proud of the result. His implementation was faster than many
standardized C library routines found in the embedded market. When looking at
his code, I found several places where improvements could be made. I made an
implementation, which was quite a lot faster than Fredrik's and this started a
friendly competition to make the fastest portable C implementation of memcpy().
Both our implementations got better and better and looked more alike and finally
we had an implementation that was very fast and that beats both the native
library routines in Windows and Linux, especially when the memory to be copied
is not aligned on a 32 bit boundary.

The following paragraphs contain descriptions to some of the techniques used in
the final implementation.

2. Mimic the CPU's Architecture

One of the biggest advantages in the original Bredberg implementation was that
the C code was made to imitate the instruction sets on the target processor.
He discovered that different processors had different instructions for handling
memory pointers. On a Motorola 68K processor the code

        *dst8++ = *src8++;

that copies one byte from the address src8 to the address dst8 and increases
both pointers, compiled into a single instruction:

        MOV.B (A0)+, (A2)+

This piece of code can be put into a while loop and will copy memory from the
address src to the address dest:

        void *memcpy(void *dest, const void *src, size_t count) {
            char *dst8 = (char *)dest;
            char *src8 = (char *)src;

            while (count--) {
                *dst8++ = *src8++;
            }
            return dest;
        }

While this is pretty good for the Motorola processor, it is not very efficient
on a PowerPC that does not have any instructions for post incrementing pointers.
The PowerPC uses four instructions for the same task that only required one
instruction on the Motorola processor. However, the PowerPC has a set of
instructions to load and store data that utilize pre increment of pointers
which means that the following code only results in two instructions when
compiled on the PowerPC:

        *++dst8++ = *++src8;

In order to use this construction, the pointers have to be decreased before the
loop begins and the final code becomes:

        void *memcpy(void *dest, const void *src, size_t count) {
            char *dst8 = (char *)dest;
            char *src8 = (char *)src;

            --src8;
            --dst8;

            while (count--) {
                *++dst8 = *++src8;
            }
            return dest;
        }

Unfortunately the ARM processor has no instructions for either pre increment
or post increment of pointers. This means that the pointer needs to be
incremented manually. If the example above is compiled to an ARM processor,
the while loop would actually look something like:

        while (count--) {
            dst8[0] = src8[0];
            ++dst8;
            ++src8;
        }

The ARM processor luckily has another feature. It can read and write to memory
at a fixed offset from a base pointer. This resulted in a third way of
implementing the same task:

        void *memcpy(void *dest, const void *src, size_t count) {
            char *dst8 = (char *)dest;
            char *src8 = (char *)src;

            if (count & 1) {
                dst8[0] = src8[0];
                dst8 += 1;
                src8 += 1;
            }

            count /= 2;
            while (count--) {
                dst8[0] = src8[0];
                dst8[1] = src8[1];

                dst8 += 2;
                src8 += 2;
            }
            return dest;
        }

Here the number of turns the loop has to be executed is half of what it was in
the earlier examples and the pointers are only updated half as often.

3. Optimizing memory accesses

In most systems, the CPU clock runs at much higher frequency than the speed of
the memory bus. My first improvement to the Bredberg original was to read 32
bits at the time from the memory. It is of course possible to read larger chunks
of data on some targets with wider data bus and wider data registers.
The goal with the C implementation of memcpy() was to get portable code mainly
for embedded systems. On such systems it is often expensive to use data types
like double and some systems doesn't have a FPU (Floating Point Unit).

By trying to read and write memory in 32 bit blocks as often as possible, the
peed of the implementation is increased dramatically, especially when copying
data that is not aligned on a 32-bit boundary.

It is however quite tricky to do this. The accesses to memory need to be aligned
on 32-bit addresses. The implementation needs two temporary variables that
implement a 64-bit sliding window where the source data is kept temporary while
being copied into the destination. The example below shows how this can be done
when the destination buffer is aligned on a 32-bit address and the source buffer
is 8 bits off the alignment:

        srcWord = *src32++;

        while (len--) {
            dstWord  = srcWord << 8;
            srcWord  = *src32++;
            dstWord |= srcWord >> 24;
            *dst32++ = dstWord;
        }

4. Optimizing branches

Another improvement is to make it easier for the compiler to generate code that
utilizes the processors compare instructions in an efficient way. This means
creating loops that terminates when a compare value is zero. The loop

        while (++i > count)

often generates more complicated and inefficient code than

        while (count--)

Another thing that makes the code more efficient is when the CPU's native loop
instructions can be used. A compiler often generates better code for the loop
above than for the following loop expression

        while (count -= 2)

5. Conclusion

The techniques described here makes the C implementation of memcpy() a lot
faster and in many cases faster than commercial ones. The implementation can
probably be improved even more, especially by using wider data types when
available. If the target and the compiler supports 64-bit arithmetic operations
such as the shift operator, these techniques can be used to implement a 64-bit
version as well. I tried to find a compiler with this support for SPARC but I
didn't find one. If 64-bit operations can be made in one instruction, the
implementation will be faster than the native Solaris memcpy() which is probably
written in assembly.

Another improvement that can be to some 32-bit architectures made is that if
both the source and the destination buffer are 64-bit aligned or have the same
alignment, a double type can be used to speed up the copying. I tried this with
the gcc compiler for x86 and the result was a memcpy() that was faster in almost
all cases than the native Linux memcpy().

6. The complete source code

The following code contains the complete memcpy() implementation.
The code is configured for an intel x86 target but it is easy to change
configuration as desired.
*/


/********************************************************************
 ** File:     memcpy.c
 **
 ** Copyright (C) 2005 Daniel Vik
 **
 ** This software is provided 'as-is', without any express or implied
 ** warranty. In no event will the authors be held liable for any
 ** damages arising from the use of this software.
 ** Permission is granted to anyone to use this software for any
 ** purpose, including commercial applications, and to alter it and 
 ** redistribute it freely, subject to the following restrictions:
 ** 
 ** 1. The origin of this software must not be misrepresented; you
 **    must not claim that you wrote the original software. If you
 **    use this software in a product, an acknowledgment in the
 **    use this software in a product, an acknowledgment in the
 **    product documentation would be appreciated but is not
 **    required.
 **
 ** 2. Altered source versions must be plainly marked as such, and 
 **    must not be misrepresented as being the original software.
 ** 
 ** 3. This notice may not be removed or altered from any source 
 **    distribution.
 **
 ** 
 ** Description: Implementation of the standard library function memcpy.
 **             This implementation of memcpy() is ANSI-C89 compatible.
 **
 **             The following configuration options can be set:
 **
 **           LITTLE_ENDIAN   - Uses processor with little endian
 **                             addressing. Default is big endian.
 **
 **           PRE_INC_PTRS    - Use pre increment of pointers.
 **                             Default is post increment of
 **                             pointers.
 **
 **           INDEXED_COPY    - Copying data using array indexing.
 **                             Using this option, disables the
 **                             PRE_INC_PTRS option.
 **
 **
 ** Best Settings:
 **
 ** Intel x86:  LITTLE_ENDIAN and INDEXED_COPY
 **
 *******************************************************************/

/********************************************************************
 ** Configuration definitions.
 *******************************************************************/

#define LITTLE_ENDIAN
#define INDEXED_COPY

/********************************************************************
 ** Includes for size_t definition
 *******************************************************************/

#include <stddef.h>

/********************************************************************
 ** Typedefs
 *******************************************************************/

typedef unsigned char  u8;
typedef unsigned short u16;
typedef unsigned long  u32;

/********************************************************************
 ** Remove definitions when INDEXED_COPY is defined.
 *******************************************************************/

#if defined (INDEXED_COPY)
#if defined (PRE_INC_PTRS)
#undef PRE_INC_PTRS
#endif /*PRE_INC_PTRS*/
#endif /*INDEXED_COPY*/

/********************************************************************
 ** Definitions for pre and post increment of pointers.
 *******************************************************************/

#if defined (PRE_INC_PTRS)

#define INC_VAL(x) *++(x)
#define START_VAL(x) (x)--
#define CAST_32_TO_8(p, o)       (u8 *)((u32)p + o + 4)
#define WHILE_DEST_BREAK         3
#define PRE_LOOP_ADJUST        - 3
#define PRE_SWITCH_ADJUST      + 1

#else /*PRE_INC_PTRS*/

#define START_VAL(x)
#define INC_VAL(x) *(x)++
#define CAST_32_TO_8(p, o)       (u8 *)((u32)p + o)
#define WHILE_DEST_BREAK         0
#define PRE_LOOP_ADJUST
#define PRE_SWITCH_ADJUST

#endif /*PRE_INC_PTRS*/

/********************************************************************
 ** Definitions for endians
 *******************************************************************/

#if defined (LITTLE_ENDIAN)

#define SHL >>
#define SHR <<

#else /*LITTLE_ENDIAN*/

#define SHL <<
#define SHR >>

#endif /*LITTLE_ENDIAN*/

/********************************************************************
 ** Macros for copying 32 bit words of  different alignment.
 ** Uses incremening pointers.
 *******************************************************************/

#define CP32_INCR() {                       \
    INC_VAL(dst32) = INC_VAL(src32);        \
}

#define CP32_INCR_SH(shl, shr) {            \
    dstWord   = srcWord SHL shl;            \
    srcWord   = INC_VAL(src32);             \
    dstWord  |= srcWord SHR shr;            \
    INC_VAL(dst32) = dstWord;               \
}

/********************************************************************
 ** Macros for copying 32 bit words of  different alignment.
 ** Uses array indexes.
 *******************************************************************/

#define CP32_INDEX(idx) {                   \
    dst32[idx] = src32[idx];                \
}

#define CP32_INDEX_SH(x, shl, shr) {        \
    dstWord   = srcWord SHL shl;            \
    srcWord   = src32[x];                   \
    dstWord  |= srcWord SHR shr;            \
    dst32[x] = dstWord;                     \
}

/********************************************************************
 ** Macros for copying 32 bit words of different alignment.
 ** Uses incremening pointers or array indexes depending on
 ** configuration.
 *******************************************************************/

#if defined (INDEXED_COPY)

#define CP32(idx)               CP32_INDEX(idx)
#define CP32_SH(idx, shl, shr)  CP32_INDEX_SH(idx, shl, shr)

#define INC_INDEX(p, o)         ((p) += (o))

#else /*INDEXED_COPY*/

#define CP32(idx)               CP32_INCR()
#define CP32_SH(idx, shl, shr)  CP32_INCR_SH(shl, shr)

#define INC_INDEX(p, o)

#endif /*INDEXED_COPY*/

/********************************************************************
 **
 ** void *dv_memcpy(void *dest, const void *src, size_t count)
 **
 ** Args:     dest    - pointer to destination buffer
 **           src     - pointer to source buffer
 **           count   - number of bytes to copy
 **
 ** Return:   A pointer to destination buffer
 **
 ** Purpose:  Copies count bytes from src to dest. No overlap check
 **           is performed.
 **
 *******************************************************************/


#ifdef __cplusplus
extern "C" {
#endif

void *memcpy(void *dest, const void *src, size_t count);

#ifdef _MSC_VER
#pragma function( memcpy )
#endif

void *memcpy(void *dest, const void *src, size_t count)
{
    u8 *dst8 = (u8 *)dest;
    u8 *src8 = (u8 *)src;

    if (count < 8) {
        if (count >= 4 && ((((u32)src8 | (u32)dst8)) & 3) == 0) {
            *((u32 *)dst8) = *((u32 *)src8);
            dst8  += 4;
            src8  += 4;
            count -= 4;
        }

        START_VAL(dst8);
        START_VAL(src8);

        while (count--) {
            INC_VAL(dst8) = INC_VAL(src8);
        }

        return dest;
    }

    START_VAL(dst8);
    START_VAL(src8);

    while (((u32)dst8 & 3L) != WHILE_DEST_BREAK) {
        INC_VAL(dst8) = INC_VAL(src8);
        count--;
    }

    switch ((((u32)src8) PRE_SWITCH_ADJUST) & 3L) {
    default:
        {
            u32 *dst32 = (u32 *)(((u32)dst8) PRE_LOOP_ADJUST);
            u32 *src32 = (u32 *)(((u32)src8) PRE_LOOP_ADJUST);
            u32 length = count / 4;

            while (length & 7) {
                CP32_INCR();
                length--;
            }

            length /= 8;

            while (length--) {
                CP32(0);
                CP32(1);
                CP32(2);
                CP32(3);
                CP32(4);
                CP32(5);
                CP32(6);
                CP32(7);

                INC_INDEX(dst32, 8);
                INC_INDEX(src32, 8);
            }

            src8 = CAST_32_TO_8(src32, 0);
            dst8 = CAST_32_TO_8(dst32, 0);

            if (count & 2) {
                *dst8++ = *src8++;
                *dst8++ = *src8++;
            }

            if (count & 1) {
                *dst8 = *src8;
            }

            return dest;
        }

    case 1:
        {
            u32 *dst32  = (u32 *)((((u32)dst8) PRE_LOOP_ADJUST) & ~3L);
            u32 *src32  = (u32 *)((((u32)src8) PRE_LOOP_ADJUST) & ~3L);
            u32 length  = count / 4;
            u32 srcWord = INC_VAL(src32);
            u32 dstWord;

            while (length & 7) {
                CP32_INCR_SH(8, 24);
                length--;
            }

            length /= 8;

            while (length--) {
                CP32_SH(0, 8, 24);
                CP32_SH(1, 8, 24);
                CP32_SH(2, 8, 24);
                CP32_SH(3, 8, 24);
                CP32_SH(4, 8, 24);
                CP32_SH(5, 8, 24);
                CP32_SH(6, 8, 24);
                CP32_SH(7, 8, 24);

                INC_INDEX(dst32, 8);
                INC_INDEX(src32, 8);
            }

            src8 = CAST_32_TO_8(src32, -3);
            dst8 = CAST_32_TO_8(dst32, 0);

            if (count & 2) {
                *dst8++ = *src8++;
                *dst8++ = *src8++;
            }

            if (count & 1) {
                *dst8 = *src8;
            }

            return dest;
        }

    case 2:
        {
            u32 *dst32  = (u32 *)((((u32)dst8) PRE_LOOP_ADJUST) & ~3L);
            u32 *src32  = (u32 *)((((u32)src8) PRE_LOOP_ADJUST) & ~3L);
            u32 length  = count / 4;
            u32 srcWord = INC_VAL(src32);
            u32 dstWord;

            while (length & 7) {
                CP32_INCR_SH(16, 16);
                length--;
            }

            length /= 8;

            while (length--) {
                CP32_SH(0, 16, 16);
                CP32_SH(1, 16, 16);
                CP32_SH(2, 16, 16);
                CP32_SH(3, 16, 16);
                CP32_SH(4, 16, 16);
                CP32_SH(5, 16, 16);
                CP32_SH(6, 16, 16);
                CP32_SH(7, 16, 16);

                INC_INDEX(dst32, 8);
                INC_INDEX(src32, 8);
            }

            src8 = CAST_32_TO_8(src32, -2);
            dst8 = CAST_32_TO_8(dst32, 0);

            if (count & 2) {
                *dst8++ = *src8++;
                *dst8++ = *src8++;
            }

            if (count & 1) {
                *dst8 = *src8;
            }

            return dest;
        }

    case 3:
        {
            u32 *dst32  = (u32 *)((((u32)dst8) PRE_LOOP_ADJUST) & ~3L);
            u32 *src32  = (u32 *)((((u32)src8) PRE_LOOP_ADJUST) & ~3L);
            u32 length  = count / 4;
            u32 srcWord = INC_VAL(src32);
            u32 dstWord;

            while (length & 7) {
                CP32_INCR_SH(24, 8);
                length--;
            }

            length /= 8;

            while (length--) {
                CP32_SH(0, 24, 8);
                CP32_SH(1, 24, 8);
                CP32_SH(2, 24, 8);
                CP32_SH(3, 24, 8);
                CP32_SH(4, 24, 8);
                CP32_SH(5, 24, 8);
                CP32_SH(6, 24, 8);
                CP32_SH(7, 24, 8);

                INC_INDEX(dst32, 8);
                INC_INDEX(src32, 8);
            }

            src8 = CAST_32_TO_8(src32, -1);
            dst8 = CAST_32_TO_8(dst32, 0);

            if (count & 2) {
                *dst8++ = *src8++;
                *dst8++ = *src8++;
            }

            if (count & 1) {
                *dst8 = *src8;
            }

            return dest;
        }
    }
}

#ifdef __cplusplus
}
#endif
