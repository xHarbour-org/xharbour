;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : longjmp.asm                                                    |
;|                                                                          |
;| Purpose : longjmp function -- win32 version.                             |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|           02-01-10  Added "ALIGN 16" after functions.                    |
;|                                                                          |
;+--------------------------------------------------------------------------+

                %include "setjmp.inc"

                BITS 32

                SECTION .text

                GLOBAL  _longjmp

                EXTERN  __global_unwind2
                EXTERN  __local_unwind2
                EXTERN  __NLG_Notify
                EXTERN  __except_handler3
                EXTERN  __except_list

;+--------------------------------------------------------------------------+

; void __cdecl longjmp(jmp_buf, int);

%define VALUE esp+8
%define JUMPBUF esp+4

                FUNCTION _longjmp
_longjmp:
                mov     ebx,[JUMPBUF]
                mov     ebp,[ebx+jb_ebp]
                mov     esi,[ebx+jb_registration]
                cmp     esi,[fs:__except_list]
                je      short lj_label1
                push    esi
                call    __global_unwind2
                pop     ecx                         ; clear args off stack
lj_label1:
                test    esi,esi
                je      short lj_label3
                lea     eax,[ebx+jb_cookie]
                push    eax
                call    __rt_probe_read4@4
                test    eax,eax
                je      short lj_label2
                mov     eax,[ebx+jb_cookie]
                cmp     eax,SETJMP_COOKIE
                jne     short lj_label2
                mov     eax,[ebx+jb_unwind_func]
                test    eax,eax
                je      short lj_label3
                push    ebx
                call    eax
                jmp     short lj_label3
lj_label2:
                mov     eax,[ebx+jb_trylevel]
                push    eax
                push    esi
                call    __local_unwind2
                add     esp,byte 8
lj_label3:
                push    byte 0
                mov     eax,[ebx+jb_eip]
                call    __NLG_Notify
                mov     edx,ebx
                mov     ebx,[edx+jb_ebx]
                mov     edi,[edx+jb_edi]
                mov     esi,[edx+jb_esi]
                mov     eax,[VALUE]
                cmp     eax,byte 1
                adc     eax,byte 0                  ; 0 --> 1
                mov     esp,[edx+jb_esp]
                add     esp,byte 4
                jmp     dword [edx+jb_eip]

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION __rt_probe_read4@4
__rt_probe_read4@4:
                push    ebp
                mov     ebp,esp
                push    byte -1
                push    dword sehblock
                push    dword __except_handler3
                mov     eax,[fs:__except_list]
                push    eax
                mov     [fs:__except_list],esp
                sub     esp,byte 16
                push    ebx
                push    esi
                push    edi
                mov     [ebp-24],esp
                and     dword [ebp-4],byte 0
                mov     eax,[ebp+8]
                mov     eax,[eax]
                push    byte 1
                pop     eax
                mov     [ebp-28],eax
                jmp     rt_probe3

rt_probe1:
                mov     eax,[ebp-20]
                mov     eax,[eax]
                mov     eax,[eax]
                xor     ecx,ecx
                cmp     eax,0xC0000005
                sete    cl
                mov     eax,ecx
                ret

rt_probe2:
                mov     esp,[ebp-24]
                xor     eax,eax
rt_probe3:
                or      dword [ebp-4],-1
                mov     ecx,[ebp-16]
                mov     [fs:__except_list],ecx
                pop     edi
                pop     esi
                pop     ebx
                leave
                ret     4

                ALIGN   16

;+--------------------------------------------------------------------------+

                SECTION .rdata

sehblock:       dd      -1
                dd      rt_probe1
                dd      rt_probe2

