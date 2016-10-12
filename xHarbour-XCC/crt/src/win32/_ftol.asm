;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _ftol.asm                                                      |
;|                                                                          |
;| Purpose : Convert float to long integer.                                 |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___ftol

;+--------------------------------------------------------------------------+

%define OLDCW esp+6
%define NEWCW esp+4
%define VALUE esp+0
%define LOCSZ 8

                FUNCTION ___ftol
___ftol:
                sub     esp,byte LOCSZ

                fnstcw  word [OLDCW]
                mov     ax,[OLDCW]
                or      ah,00001100B    ; Rounding control = chop (truncate toward zero)
                mov     [NEWCW],ax
                fldcw   word [NEWCW]
                fistp   dword [VALUE]
                fldcw   word [OLDCW]

                mov     eax,[VALUE]

                add     esp,byte LOCSZ
                ret

                ALIGN   16


