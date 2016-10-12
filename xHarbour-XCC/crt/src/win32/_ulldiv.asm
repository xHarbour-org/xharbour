;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _ulldiv.asm                                                    |
;|                                                                          |
;| Purpose : long long integer divide (unsigned).                           |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___ulldiv

;+--------------------------------------------------------------------------+

                FUNCTION ___ulldiv
___ulldiv:
                push    ebx
                push    esi

; Set up the local stack and save the index registers.

%define DVND esp+20     ; stack address of dividend (a)
%define DVSR esp+12     ; stack address of divisor (b)
%define ARGSZ 16

; Now do the divide.  First look to see if the divisor is less than 4194304K.
; If so, then we can use a simple algorithm with word divides, otherwise
; things get a little more complex.

                mov     eax,[DVSR+4]    ; check to see if divisor < 4194304K
                or      eax,eax
                jnz     short L1        ; nope, gotta do this the hard way
                mov     ecx,[DVSR]      ; load divisor
                mov     eax,[DVND+4]    ; load high word of dividend
                xor     edx,edx
                div     ecx             ; get high order bits of quotient
                mov     ebx,eax         ; save high bits of quotient
                mov     eax,[DVND]      ; edx:eax <- remainder:lo word of dividend
                div     ecx             ; get low order bits of quotient
                mov     edx,ebx         ; edx:eax <- quotient hi:quotient lo
                jmp     short L2        ; restore stack and return

; Here we do it the hard way.  Remember, eax contains DVSRHI

L1:
                mov     ecx,eax         ; ecx:ebx <- divisor
                mov     ebx,[DVSR]
                mov     edx,[DVND+4]    ; edx:eax <- dividend
                mov     eax,[DVND]
L3:
                shr     ecx,1           ; shift divisor right one bit; hi bit <- 0
                rcr     ebx,1
                shr     edx,1           ; shift dividend right one bit; hi bit <- 0
                rcr     eax,1
                or      ecx,ecx
                jnz     short L3        ; loop until divisor < 4194304K
                div     ebx             ; now divide, ignore remainder
                mov     esi,eax         ; save quotient

; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.

                mul     dword [DVSR+4]  ; QUOT * HIWORD(DVSR)
                mov     ecx,eax
                mov     eax,[DVSR]
                mul     esi             ; QUOT * LOWORD(DVSR)
                add     edx,ecx         ; EDX:EAX = QUOT * DVSR
                jc      short L4        ; carry means Quotient is off by 1

; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we are ok, otherwise
; subtract one (1) from the quotient.

                cmp     edx,[DVND+4]    ; compare hi words of result and original
                ja      short L4        ; if result > original, do subtract
                jb      short L5        ; if result < original, we are ok
                cmp     eax,[DVND]      ; hi words are equal, compare lo words
                jbe     short L5        ; if less or equal we are ok, else subtract
L4:
                dec     esi             ; subtract 1 from quotient
L5:
                xor     edx,edx         ; edx:eax <- quotient
                mov     eax,esi

; Just the cleanup left to do.  edx:eax contains the quotient.
; Restore the saved registers and return.

L2:
                pop     esi
                pop     ebx
                ret     ARGSZ

                ALIGN   16

