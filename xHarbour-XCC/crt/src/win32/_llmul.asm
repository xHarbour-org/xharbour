;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _llmul.asm                                                     |
;|                                                                          |
;| Purpose : long long integer multiply (signed and unsigned).              |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___llmul

;+--------------------------------------------------------------------------+

%define A esp+12
%define B esp+4
%define ARGSZ 16

                FUNCTION ___llmul
___llmul:

;   AHI, BHI : upper 32 bits of A and B
;   ALO, BLO : lower 32 bits of A and B
;
;         ALO * BLO
;   ALO * BHI
; + BLO * AHI
; ---------------------

                mov     eax,[A+4]
                mov     ecx,[B+4]
                or      ecx,eax         ; test for both hiwords zero
                mov     ecx,[B]
                jnz     short hard      ; both are zero, just mult ALO and BLO

                mov     eax,[A]
                mul     ecx
                ret     ARGSZ

%undef A
%undef B

; must redefine A and B since <push ebx> changes esp

%define A esp+16
%define B esp+8

hard:
                push    ebx
                mul     ecx             ; eax has AHI, ecx has BLO, so AHI * BLO
                mov     ebx,eax         ; save result
                mov     eax,[A]
                mul     dword [B+4]     ; ALO * BHI
                add     ebx,eax         ; ebx = ((ALO * BHI) + (AHI * BLO))
                mov     eax,[A]         ; ecx = BLO
                mul     ecx             ; so edx:eax = ALO*BLO
                add     edx,ebx         ; now edx has all the LO*HI stuff
                pop     ebx
                ret     ARGSZ

                ALIGN   16

