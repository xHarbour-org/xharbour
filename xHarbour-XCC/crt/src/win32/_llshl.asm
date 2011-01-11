;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _llshl.asm                                                     |
;|                                                                          |
;| Purpose : long long shift left (signed and unsigned).                    |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___llshl

;+--------------------------------------------------------------------------+

%define VALUE esp+8
%define SHIFT esp+4
%define ARGSZ 12

                FUNCTION ___llshl
___llshl:

; Handle shifts of 64 or more bits (all get 0)

                mov     eax,[VALUE]
                mov     edx,[VALUE+4]
                mov     ecx,[SHIFT]
                cmp     ecx,byte 64
                jae     short retzero

; Handle shifts of between 0 and 31 bits

                cmp     cl,32
                jae     short over32
                shld    edx,eax,cl
                shl     eax,cl
                ret     ARGSZ

; Handle shifts of between 32 and 63 bits

                ALIGN   4

over32:
                mov     edx,eax
                xor     eax,eax
                and     cl,31
                shl     edx,cl
                ret     ARGSZ

; return 0 in edx:eax

                ALIGN   4

retzero:
                xor     eax,eax
                xor     edx,edx
                ret     ARGSZ

                ALIGN   16

