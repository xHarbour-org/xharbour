;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : xdiv.asm                                                       |
;|                                                                          |
;| Purpose : signed and unsigned divide and modulo.                         |
;|                                                                          |
;| History : Date      Reason                                               |
;|           01-01-10  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL _rt_sdiv
                GLOBAL _rt_udiv

                EXTERN  __imp___rt_sdiv
                EXTERN  __imp___rt_udiv

;+--------------------------------------------------------------------------+

                ; FUNCTION _rt_sdiv
_rt_sdiv:
                ldr     r2,[pc,#(.symbol-$-8)]
                ldr     r2,[r2,#0]
                mov     pc,r2

.symbol:        dcd     __imp___rt_sdiv

;+--------------------------------------------------------------------------+

                ; FUNCTION _rt_udiv
_rt_udiv:
                ldr     r2,[pc,#(.symbol-$-8)]
                ldr     r2,[r2,#0]
                mov     pc,r2

.symbol:        dcd     __imp___rt_udiv

;+--------------------------------------------------------------------------+

