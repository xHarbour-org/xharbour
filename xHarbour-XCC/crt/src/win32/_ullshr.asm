;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _ullshr.asm                                                    |
;|                                                                          |
;| Purpose : long long shift right (unsigned).                              |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___ullshr

;+--------------------------------------------------------------------------+

%define VALUE esp+8
%define SHIFT esp+4
%define ARGSZ 12

                FUNCTION ___ullshr
___ullshr:

; Handle shifts of 64 bits or more (if shifting 64 bits or more,
; the result depends only on the high order bit of edx).

                mov     eax,[VALUE]
                mov     edx,[VALUE+4]
                mov     ecx,[SHIFT]
                cmp     ecx,byte 64
                jae     short retzero

; Handle shifts of between 0 and 31 bits

                cmp     cl,32
                jae     short over32
                shrd    eax,edx,cl
                shr     edx,cl
                ret     ARGSZ

; Handle shifts of between 32 and 63 bits

                ALIGN   4

over32:
                mov     eax,edx
                xor     edx,edx
                and     cl,31
                shr     eax,cl
                ret     ARGSZ

; return 0 in edx:eax

                ALIGN   4

retzero:
                xor     eax,eax
                xor     edx,edx
                ret     ARGSZ

                ALIGN   16


