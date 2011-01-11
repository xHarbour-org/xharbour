;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : setjmp.asm                                                     |
;|                                                                          |
;| Purpose : setjmp function -- win32 version.                              |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|           02-01-10  Added "ALIGN 16" after functions.                    |
;|                                                                          |
;+--------------------------------------------------------------------------+

                %include "setjmp.inc"

                BITS 32

                SECTION .text

                GLOBAL  __setjmp3

                EXTERN  __except_list

;+--------------------------------------------------------------------------+

; int __cdecl _setjmp3(jmp_buf, int)

%define ARG5 esp+20
%define ARG4 esp+16
%define ARG3 esp+12
%define ARG2 esp+8
%define JUMPBUF esp+4

                FUNCTION __setjmp3
__setjmp3:
                mov     edx,[JUMPBUF]
                mov     [edx+jb_ebp],ebp
                mov     [edx+jb_ebx],ebx
                mov     [edx+jb_edi],edi
                mov     [edx+jb_esi],esi
                mov     [edx+jb_esp],esp
                mov     eax,[esp]
                mov     [edx+jb_eip],eax
                mov     dword [edx+jb_cookie],SETJMP_COOKIE
                mov     dword [edx+jb_unwind_func],0
                mov     eax,[fs:__except_list]
                mov     [edx+jb_registration],eax
                cmp     eax,byte -1
                jne     short sj_label1
                mov     dword [edx+jb_trylevel],0xFFFFFFFF
                jmp     short sj_return

sj_label1:
                mov     ecx,[ARG2]
                or      ecx,ecx
                je      short sj_label2
                mov     eax,[ARG3]
                mov     [edx+jb_unwind_func],eax
                dec     ecx
                jne     short sj_label3

sj_label2:
                mov     eax,[eax+12]                ; __except_list + 12
                mov     [edx+jb_trylevel],eax
                jmp     short sj_return

sj_label3:
                mov     eax,[ARG4]
                mov     [edx+jb_trylevel],eax
                dec     ecx
                je      sj_return
                push    esi
                push    edi
                lea     esi,[ARG5+8]                ; compensate for push edi, push esi
                lea     edi,[edx+jb_unwind_data]
                cmp     ecx,byte 6
                jbe     short sj_label4
                mov     ecx,6
sj_label4:      rep     movsd
                pop     edi
                pop     esi

sj_return:
                sub     eax,eax
                ret

                ALIGN   16

