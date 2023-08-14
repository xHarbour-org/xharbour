;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _llshr.asm                                                     |
;|                                                                          |
;| Purpose : long long shift right (signed).                                |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___llshr

;+--------------------------------------------------------------------------+

%define VALUE esp+8
%define SHIFT esp+4
%define ARGSZ 12

                FUNCTION ___llshr
___llshr:

; Handle shifts of 64 bits or more (if shifting 64 bits or more,
; the result depends only on the high order bit of edx).

                mov     eax,[VALUE]
                mov     edx,[VALUE+4]
                mov     ecx,[SHIFT]
                cmp     ecx,byte 64
                jae     short retsign

; Handle shifts of between 0 and 31 bits

                cmp     cl,32
                jae     short over32
                shrd    eax,edx,cl
                sar     edx,cl
                ret     ARGSZ

; Handle shifts of between 32 and 63 bits

                ALIGN   4

over32:
                mov     eax,edx
                sar     edx,31
                and     cl,31
                sar     eax,cl
                ret     ARGSZ

; Return double precision 0 or -1, depending on the sign of edx

                ALIGN   4

retsign:
                sar     edx,31
                mov     eax,edx
                ret     ARGSZ

                ALIGN   16


