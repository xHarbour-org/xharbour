;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : seh1.asm                                                       |
;|                                                                          |
;| Purpose : Structured Exception Handling (SEH) support.                   |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|           02-01-10  Added "ALIGN 16" after functions.                    |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .drectve

; Since the startup code uses SEH, this is a hack to force "kernel32.lib"
; to always end up in the default search list.

                DB      "-defaultlib:kernel32.lib", 0

;+--------------------------------------------------------------------------+

                SECTION .text

                GLOBAL  __global_unwind2
                GLOBAL  __local_unwind2
                GLOBAL  __NLG_Return2
                GLOBAL  __abnormal_termination
                GLOBAL  __NLG_Notify1
                GLOBAL  __NLG_Notify
                GLOBAL  __NLG_Dispatch
                GLOBAL  __NLG_Destination

                EXTERN  _RtlUnwind@16           ; from KERNEL32.DLL

;+--------------------------------------------------------------------------+

; thin wrapper around the undocumented API function:
; _RtlUnwind(PEXCEPTION_REGISTRATION pRegFrame, PVOID retAddress, PEXCEPTION_RECORD pExceptRec, DWORD eax_value)

                FUNCTION __global_unwind2
__global_unwind2:
                push    ebp
                mov     ebp,esp
                push    ebx
                push    esi
                push    edi
                push    ebp

                push    byte 0                  ; eax_value
                push    byte 0                  ; pExceptRec
                push    dword gu_return         ; retAddress
                push    dword [ebp+8]           ; pRegFrame
                call    _RtlUnwind@16

gu_return:
                pop     ebp
                pop     edi
                pop     esi
                pop     ebx
                mov     esp,ebp
                pop     ebp
                ret

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION __unwind_handler
__unwind_handler:
                mov     ecx,[esp+4]
                test    dword [ecx+4],6
                mov     eax,1
                je      uh_return
                mov     eax,[esp+8]
                mov     edx,[esp+16]
                mov     [edx],eax
                mov     eax,3
uh_return:
                ret

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION __local_unwind2
__local_unwind2:
                push    ebx
                push    esi
                push    edi
                mov     eax,dword [esp+16]
                push    eax
                push    byte -2
                push    dword __unwind_handler
                push    dword [fs:__except_list]
                mov     dword [fs:__except_list],esp

lu_again:
                mov     eax,[esp+32]
                mov     ebx,[eax+8]
                mov     esi,[eax+12]
                cmp     esi,-1
                je      lu_return
                cmp     esi,[esp+36]
                je      lu_return
                lea     esi,[esi+esi*2]
                mov     ecx,[ebx+esi*4]
                mov     [esp+8],ecx
                mov     [eax+12],ecx
                cmp     dword [ebx+esi*4+4],0
                jne     __NLG_Return2
                push    dword 0x00000101
                mov     eax,[ebx+esi*4+8]
                call    __NLG_Notify
                call    dword [ebx+esi*4+8]

__NLG_Return2:
                jmp     short lu_again

lu_return:
                pop     dword [fs:__except_list]
                add     esp,12
                pop     edi
                pop     esi
                pop     ebx
                ret

                ALIGN   16

;+--------------------------------------------------------------------------+

; The intrinsic function _abnormal_termination is available within a
; termination handler. It returns 0 if the body of the try-finally
; statement terminates sequentially. In all other cases, it returns 1.

                FUNCTION __abnormal_termination
__abnormal_termination:
                xor     eax,eax
                mov     ecx,dword [fs:__except_list]
                cmp     dword [ecx+4],__unwind_handler
                jne     at_return
                mov     edx,[ecx+12]
                mov     edx,[edx+12]
                cmp     [ecx+8],edx
                jne     at_return
                mov     eax,1
at_return:
                ret

                ALIGN   16

;+--------------------------------------------------------------------------+
; NLG = "non-local-goto" (setjmp/longjmp)

__NLG_Notify1:
                push    ebx
                push    ecx
                mov     ebx,__NLG_Destination
                jmp     nn_label

__NLG_Notify:
                push    ebx
                push    ecx
                mov     ebx,__NLG_Destination
                mov     ecx,[ebp+8]
nn_label:
                mov     [ebx+8],ecx
                mov     [ebx+4],eax
                mov     [ebx+12],ebp

__NLG_Dispatch:
                pop     ecx
                pop     ebx
                ret     4

                ALIGN   16

;+--------------------------------------------------------------------------+

                SECTION .data

; #define EH_MAGIC_NUMBER1 0x019930520
;
; NLG struct (debugging info)
;
; struct {
;    unsigned long dwSig;
;    unsigned long uoffDestination;
;    unsigned long dwCode;
;    unsigned long uoffFramePointer;
; } _NLG_Destination = {EH_MAGIC_NUMBER1,0,0,0};

__NLG_Destination:
                dd  0x19930520                  ; Magic number
                dd  0
                dd  0
                dd  0

;+--------------------------------------------------------------------------+

                ABSOLUTE 0

                GLOBAL  __except_list
__except_list:

