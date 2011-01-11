/****************************************************************************
 *                                                                          *
 * File    : _ctype.c                                                       *
 *                                                                          *
 * Purpose : __ctypetab conversion table -- ASCII version.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-12  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <limits.h>
#include <stdio.h>

#if EOF != -1 || UCHAR_MAX != 255
#error WRONG CTYPE TABLE
#endif

/* static data */
#if 1
/* macros */
#define XBB  (_CNTRL|_WHITE)
#define XBL  (XBB|_BLANK)
#define XDI  (_DIGIT|_HEX)
#define XLO  (_LOWER|_HEX)
#define XUP  (_UPPER|_HEX)

static const short tab[257] = { 0,  /* EOF */
      0, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL,
    _CNTRL, XBL, XBB, XBB, XBB, XBB, _CNTRL, _CNTRL,
    _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL,
    _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL, _CNTRL,
    _SPACE, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT,
    _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT,
    XDI, XDI, XDI, XDI, XDI, XDI, XDI, XDI,
    XDI, XDI, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT,
    _PUNCT, XUP, XUP, XUP, XUP, XUP, XUP, _UPPER,
    _UPPER, _UPPER, _UPPER, _UPPER, _UPPER, _UPPER, _UPPER, _UPPER,
    _UPPER, _UPPER, _UPPER, _UPPER, _UPPER, _UPPER, _UPPER, _UPPER,
    _UPPER, _UPPER, _UPPER, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _PUNCT,
    _PUNCT, XLO, XLO, XLO, XLO, XLO, XLO, _LOWER,
    _LOWER, _LOWER, _LOWER, _LOWER, _LOWER, _LOWER, _LOWER, _LOWER,
    _LOWER, _LOWER, _LOWER, _LOWER, _LOWER, _LOWER, _LOWER, _LOWER,
    _LOWER, _LOWER, _LOWER, _PUNCT, _PUNCT, _PUNCT, _PUNCT, _CNTRL,
 };  /* rest all match nothing */
#else
static const short tab[257] = { 0,  /* EOF */
    /* 00 nul */        0,
    /* 01 soh */        _CNTRL,
    /* 02 stx */        _CNTRL,
    /* 03 etx */        _CNTRL,
    /* 04 eot */        _CNTRL,
    /* 05 enq */        _CNTRL,
    /* 06 ack */        _CNTRL,
    /* 07 bel */        _CNTRL,
    /* 08 bs  */        _CNTRL,
    /* 09 ht  */        _CNTRL|_WHITE|_BLANK,
    /* 0A nl  */        _CNTRL|_WHITE,
    /* 0B vt  */        _CNTRL|_WHITE,
    /* 0C ff  */        _CNTRL|_WHITE,
    /* 0D cr  */        _CNTRL|_WHITE,
    /* 0E so  */        _CNTRL,
    /* 0F si  */        _CNTRL,
    /* 10 dle */        _CNTRL,
    /* 11 dc1 */        _CNTRL,
    /* 12 dc2 */        _CNTRL,
    /* 13 dc3 */        _CNTRL,
    /* 14 dc4 */        _CNTRL,
    /* 15 nak */        _CNTRL,
    /* 16 syn */        _CNTRL,
    /* 17 etb */        _CNTRL,
    /* 18 can */        _CNTRL,
    /* 19 em  */        _CNTRL,
    /* 1A sub */        _CNTRL,
    /* 1B esc */        _CNTRL,
    /* 1C fs  */        _CNTRL,
    /* 1D gs  */        _CNTRL,
    /* 1E rs  */        _CNTRL,
    /* 1F us  */        _CNTRL,
    /* 20 sp  */        _SPACE,
    /* 21 !   */        _PUNCT,
    /* 22 "   */        _PUNCT,
    /* 23 #   */        _PUNCT,
    /* 24 $   */        _PUNCT,
    /* 25 %   */        _PUNCT,
    /* 26 &   */        _PUNCT,
    /* 27 '   */        _PUNCT,
    /* 28 (   */        _PUNCT,
    /* 29 )   */        _PUNCT,
    /* 2A *   */        _PUNCT,
    /* 2B +   */        _PUNCT,
    /* 2C ,   */        _PUNCT,
    /* 2D -   */        _PUNCT,
    /* 2E .   */        _PUNCT,
    /* 2F /   */        _PUNCT,
    /* 30 0   */        _DIGIT|_HEX,
    /* 31 1   */        _DIGIT|_HEX,
    /* 32 2   */        _DIGIT|_HEX,
    /* 33 3   */        _DIGIT|_HEX,
    /* 34 4   */        _DIGIT|_HEX,
    /* 35 5   */        _DIGIT|_HEX,
    /* 36 6   */        _DIGIT|_HEX,
    /* 37 7   */        _DIGIT|_HEX,
    /* 38 8   */        _DIGIT|_HEX,
    /* 39 9   */        _DIGIT|_HEX,
    /* 3A :   */        _PUNCT,
    /* 3B ;   */        _PUNCT,
    /* 3C <   */        _PUNCT,
    /* 3D =   */        _PUNCT,
    /* 3E >   */        _PUNCT,
    /* 3F ?   */        _PUNCT,
    /* 40 @   */        _PUNCT,
    /* 41 A   */        _UPPER|_HEX,
    /* 42 B   */        _UPPER|_HEX,
    /* 43 C   */        _UPPER|_HEX,
    /* 44 D   */        _UPPER|_HEX,
    /* 45 E   */        _UPPER|_HEX,
    /* 46 F   */        _UPPER|_HEX,
    /* 47 G   */        _UPPER,
    /* 48 H   */        _UPPER,
    /* 49 I   */        _UPPER,
    /* 4A J   */        _UPPER,
    /* 4B K   */        _UPPER,
    /* 4C L   */        _UPPER,
    /* 4D M   */        _UPPER,
    /* 4E N   */        _UPPER,
    /* 4F O   */        _UPPER,
    /* 50 P   */        _UPPER,
    /* 51 Q   */        _UPPER,
    /* 52 R   */        _UPPER,
    /* 53 S   */        _UPPER,
    /* 54 T   */        _UPPER,
    /* 55 U   */        _UPPER,
    /* 56 V   */        _UPPER,
    /* 57 W   */        _UPPER,
    /* 58 X   */        _UPPER,
    /* 59 Y   */        _UPPER,
    /* 5A Z   */        _UPPER,
    /* 5B [   */        _PUNCT,
    /* 5C \   */        _PUNCT,
    /* 5D ]   */        _PUNCT,
    /* 5E ^   */        _PUNCT,
    /* 5F _   */        _PUNCT,
    /* 60 `   */        _PUNCT,
    /* 61 a   */        _LOWER|_HEX,
    /* 62 b   */        _LOWER|_HEX,
    /* 63 c   */        _LOWER|_HEX,
    /* 64 d   */        _LOWER|_HEX,
    /* 65 e   */        _LOWER|_HEX,
    /* 66 f   */        _LOWER|_HEX,
    /* 67 g   */        _LOWER,
    /* 68 h   */        _LOWER,
    /* 69 i   */        _LOWER,
    /* 6A j   */        _LOWER,
    /* 6B k   */        _LOWER,
    /* 6C l   */        _LOWER,
    /* 6D m   */        _LOWER,
    /* 6E n   */        _LOWER,
    /* 6F o   */        _LOWER,
    /* 70 p   */        _LOWER,
    /* 71 q   */        _LOWER,
    /* 72 r   */        _LOWER,
    /* 73 s   */        _LOWER,
    /* 74 t   */        _LOWER,
    /* 75 u   */        _LOWER,
    /* 76 v   */        _LOWER,
    /* 77 w   */        _LOWER,
    /* 78 x   */        _LOWER,
    /* 79 y   */        _LOWER,
    /* 7A z   */        _LOWER,
    /* 7B {   */        _PUNCT,
    /* 7C |   */        _PUNCT,
    /* 7D }   */        _PUNCT,
    /* 7E ~   */        _PUNCT,
    /* 7F     */        _CNTRL,
};  /* rest all match nothing */
#endif

const short *__ctypetab = &tab[1];

