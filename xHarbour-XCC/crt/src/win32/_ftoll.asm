;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _ftoll.asm                                                     |
;|                                                                          |
;| Purpose : Convert float to long long integer.                            |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___ftoll

;+--------------------------------------------------------------------------+

%define OLDCW esp+10
%define NEWCW esp+8
%define VALUE esp+0
%define LOCSZ 12

                FUNCTION ___ftoll
___ftoll:
                sub     esp,byte LOCSZ

                fnstcw  word [OLDCW]
                mov     ax,[OLDCW]
                or      ah,00001100B    ; Rounding control = chop (truncate toward zero)
                mov     [NEWCW],ax
                fldcw   word [NEWCW]
                fistp   qword [VALUE]
                fldcw   word [OLDCW]

                mov     eax,[VALUE]     ;
                mov     edx,[VALUE+4]   ; return long long value

                add     esp,byte LOCSZ
                ret

                ALIGN   16


