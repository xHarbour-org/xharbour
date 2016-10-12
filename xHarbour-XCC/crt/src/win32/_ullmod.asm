;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _ullmod.asm                                                    |
;|                                                                          |
;| Purpose : long long integer remainder (unsigned).                        |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  ___ullmod

;+--------------------------------------------------------------------------+

                FUNCTION ___ullmod
___ullmod:
                push    ebx

; Set up the local stack and save the index registers.

%define DVND esp+16     ; stack address of dividend (a)
%define DVSR esp+8      ; stack address of divisor (b)
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
                div     ecx             ; edx <- remainder, eax <- quotient
                mov     eax,[DVND]      ; edx:eax <- remainder:lo word of dividend
                div     ecx             ; edx <- final remainder
                mov     eax,edx         ; edx:eax <- remainder
                xor     edx,edx
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

; We may be off by one, so to check, we will multiply the quotient
; by the divisor and check the result against the orignal dividend
; Note that we must also check for overflow, which can occur if the
; dividend is close to 2**64 and the quotient is off by 1.

                mov     ecx,eax         ; save a copy of quotient in ECX
                mul     dword [DVSR+4]
                xchg    ecx,eax         ; put partial product in ECX, get quotient in EAX
                mul     dword [DVSR]
                add     edx,ecx         ; EDX:EAX = QUOT * DVSR
                jc      short L4        ; carry means Quotient is off by 1

; do long compare here between original dividend and the result of the
; multiply in edx:eax.  If original is larger or equal, we're ok, otherwise
; subtract the original divisor from the result.

                cmp     edx,[DVND+4]    ; compare hi words of result and original
                ja      short L4        ; if result > original, do subtract
                jb      short L5        ; if result < original, we're ok
                cmp     eax,[DVND]      ; hi words are equal, compare lo words
                jbe     short L5        ; if less or equal we're ok, else subtract
L4:
                sub     eax,[DVSR]      ; subtract divisor from result
                sbb     edx,[DVSR+4]
L5:

; Calculate remainder by subtracting the result from the original dividend.
; Since the result is already in a register, we will perform the subtract in
; the opposite direction and negate the result to make it positive.

                sub     eax,[DVND]      ; subtract original dividend from result
                sbb     edx,[DVND+4]
                neg     edx             ; and negate it
                neg     eax
                sbb     edx,byte 0

; Just the cleanup left to do.  dx:ax contains the remainder.
; Restore the saved registers and return.

L2:
                pop     ebx
                ret     ARGSZ

                ALIGN   16

