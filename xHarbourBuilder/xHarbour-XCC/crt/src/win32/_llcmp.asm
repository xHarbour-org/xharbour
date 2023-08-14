;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _llcmp.asm                                                     |
;|                                                                          |
;| Purpose : long long integer compare (signed and unsigned).               |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___lllt
                GLOBAL  ___llle
                GLOBAL  ___llgt
                GLOBAL  ___llge
                GLOBAL  ___lleq
                GLOBAL  ___llne
                GLOBAL  ___ulllt
                GLOBAL  ___ullle
                GLOBAL  ___ullgt
                GLOBAL  ___ullge

;+--------------------------------------------------------------------------+

%define A esp+16
%define B esp+8
%define ARGSZ 16

;+--------------------------------------------------------------------------+

                FUNCTION ___lllt
___lllt:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                jg      short lt_false
                jl      short lt_true
                mov     eax,[A]
                cmp     eax,[B]
                jb      short lt_true
lt_false:
                pop     eax
                stc
                ret     ARGSZ
lt_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___llle
___llle:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                jg      short le_false
                jl      short le_true
                mov     eax,[A]
                cmp     eax,[B]
                jbe     short le_true
le_false:
                pop     eax
                stc
                ret     ARGSZ
le_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___llgt
___llgt:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                jg      short gt_true
                jl      short gt_false
                mov     eax,[A]
                cmp     eax,[B]
                ja      short gt_true
gt_false:
                pop     eax
                stc
                ret     ARGSZ
gt_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___llge
___llge:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                jg      short ge_true
                jl      short ge_false
                mov     eax,[A]
                cmp     eax,[B]
                jae     short ge_true
ge_false:
                pop     eax
                stc
                ret     ARGSZ
ge_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___lleq
___lleq:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                jne     short eq_false
                mov     eax,[A]
                cmp     eax,[B]
                je      short eq_true
eq_false:
                pop     eax
                stc
                ret     ARGSZ
eq_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___llne
___llne:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                jne     short ne_true
                mov     eax,[A]
                cmp     eax,[B]
                jne     short ne_true
ne_false:
                pop     eax
                stc
                ret     ARGSZ
ne_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___ulllt
___ulllt:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                ja      short ult_false
                jb      short ult_true
                mov     eax,[A]
                cmp     eax,[B]
                jb      short ult_true
ult_false:
                pop     eax
                stc
                ret     ARGSZ
ult_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___ullle
___ullle:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                ja      short ule_false
                jb      short ule_true
                mov     eax,[A]
                cmp     eax,[B]
                jbe     short ule_true
ule_false:
                pop     eax
                stc
                ret     ARGSZ
ule_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___ullgt
___ullgt:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                ja      short ugt_true
                jb      short ugt_false
                mov     eax,[A]
                cmp     eax,[B]
                ja      short ugt_true
ugt_false:
                pop     eax
                stc
                ret     ARGSZ
ugt_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___ullge
___ullge:
                push    eax
                mov     eax,[A+4]
                cmp     eax,[B+4]
                ja      short uge_true
                jb      short uge_false
                mov     eax,[A]
                cmp     eax,[B]
                jae     short uge_true
uge_false:
                pop     eax
                stc
                ret     ARGSZ
uge_true:
                pop     eax
                clc
                ret     ARGSZ

                ALIGN   16

;+--------------------------------------------------------------------------+
;+--------------------------------------------------------------------------+
;+--------------------------------------------------------------------------+
;+--------------------------------------------------------------------------+

%if 0
; This function does not work properly (OF flag).

                GLOBAL  __Llcmp

;+--------------------------------------------------------------------------+

%define B esp+20
%define A esp+12
%define ARGSZ 16

                FUNCTION __Llcmp
__Llcmp:
                push    eax
                push    ecx

                mov     ecx,[B+4]
                cmp     ecx,[A+4]
                jne     quit
                js      L1
                mov     ecx,[B]
                cmp     ecx,[A]
                jmp     short L2
L1:
                mov     ecx,[A]
                cmp     ecx,[B]
L2:
                lahf                    ; AH = SF:ZF:??:AF:??:PF:??:CF
                mov     al,ah
                and     al,1            ; AL = --:--:--:--:--:--:--:CF
                ror     al,1            ; AL = CF:--:--:--:--:--:--:--
                or      ah,al           ; AH = CF:ZF:??:AF:??:PF:??:CF
                sahf
quit:
                pop     ecx
                pop     eax
                ret     ARGSZ

                ALIGN   16

%endif
