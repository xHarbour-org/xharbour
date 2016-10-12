;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : seh2.asm                                                       |
;|                                                                          |
;| Purpose : Structured Exception Handling (SEH) support.                   |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|           02-01-10  Added "ALIGN 16" after functions.                    |
;|                                                                          |
;+--------------------------------------------------------------------------+

; handler dispositions
DISPOSITION_DISMISS             equ     0       ; ExceptionContinueExecution
DISPOSITION_CONTINUE_SEARCH     equ     1       ; ExceptionContinueSearch
DISPOSITION_NESTED_EXCEPTION    equ     2       ; ExceptionNestedException
DISPOSITION_COLLIDED_UNWIND     equ     3       ; ExceptionCollidedUnwind

; filter return codes
FILTER_ACCEPT           equ     1               ; EXCEPTION_EXECUTE_HANDLER
FILTER_DISMISS          equ     -1              ; EXCEPTION_CONTINUE_EXECUTION
FILTER_CONTINUE_SEARCH  equ     0               ; EXCEPTION_CONTINUE_SEARCH

; handler flags settings..
EH_UNWINDING        equ     2
EH_EXIT_UNWIND      equ     4
EH_UNWIND_CONTEXT   equ     EH_UNWINDING|EH_EXIT_UNWIND

TRYLEVEL_NONE       equ     -1
TRYLEVEL_INVALID    equ     -2

;+--------------------------------------------------------------------------+

                BITS 32

                SECTION .text

                GLOBAL  __except_handler3
                GLOBAL  __seh_longjmp_unwind@4

                EXTERN  __global_unwind2
                EXTERN  __local_unwind2
                EXTERN  __NLG_Notify

;+--------------------------------------------------------------------------+

                DB      "VC20XC00"

;+--------------------------------------------------------------------------+

; typedef struct _EXCEPTION_REGISTRATION {
;     struct _EXCEPTION_REGISTRATION *prev;
;     void (*handler)(PEXCEPTION_RECORD, PEXCEPTION_REGISTRATION, PCONTEXT, PEXCEPTION_RECORD);
;     struct scopetable_entry *scopetable;
;     int trylevel;
;     int _ebp;
;     PEXCEPTION_POINTERS xpointers;
; } EXCEPTION_REGISTRATION;

; typedef struct _EXCEPTION_RECORD { // exr
;     DWORD ExceptionCode;
;     DWORD ExceptionFlags;
;     struct _EXCEPTION_RECORD *ExceptionRecord;
;     PVOID ExceptionAddress;
;     DWORD NumberParameters;
;     DWORD ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
; } EXCEPTION_RECORD;


; int __except_handler3(PEXCEPTION_RECORD pExceptRec, PEXCEPTION_REGISTRATION pRegFrame, PCONTEXT pContext, PVOID pDispatcherContext)

%define DISPATCHER ebp+20
%define CONTEXT ebp+16
%define REGFRAME ebp+12
%define EXCEPTION ebp+8

                FUNCTION __except_handler3
__except_handler3:
                push    ebp
                mov     ebp,esp
                sub     esp,8
                push    ebx
                push    esi
                push    edi
                push    ebp
                cld
                mov     ebx,[REGFRAME]
                mov     eax,[EXCEPTION]
                test    dword [eax+4],EH_UNWIND_CONTEXT         ; pExceptRec->ExceptionFlags
                jne     near unwind

                mov     [ebp-8],eax
                mov     eax,[CONTEXT]
                mov     [ebp-4],eax
                lea     eax,[ebp-8]
                mov     [ebx-4],eax
                mov     esi,[ebx+12]
                mov     edi,[ebx+8]

search_for_handler:
                cmp     esi,byte TRYLEVEL_NONE
                je      short no_more_tries
                lea     ecx,[esi+esi*2]
                cmp     dword [edi+ecx*4+4],0   ; any filter function ?
                je      continue_search         ; no, jump
                push    esi
                push    ebp
                lea     ebp,[ebx+16]            ; switch to *original* EBP
                call    dword [edi+ecx*4+4]     ; call filter function
                pop     ebp
                pop     esi
                mov     ebx,[REGFRAME]          ; pRegFrame
                test    eax,eax
                je      continue_search         ; jump if EXCEPTION_CONTINUE_SEARCH
                js      continue_execution      ; jump if EXCEPTION_CONTINUE_EXECUTION

                ; EXCEPTION_EXECUTE_HANDLER
                mov     edi,[ebx+8]             ; pScopetable
                push    ebx                     ; pRegFrame
                call    __global_unwind2
                add     esp,+4
                lea     ebp,[ebx+16]            ; switch to *original* EBP
                push    esi
                push    ebx
                call    __local_unwind2
                add     esp,+8
                lea     ecx,[esi+esi*2]
                push    byte 1
                mov     eax,[edi+ecx*4+8]
                call    __NLG_Notify            ; NLG = "non-local-goto" (setjmp/longjmp)
                mov     eax,[edi+ecx*4]
                mov     [ebx+12],eax
                call    dword [edi+ecx*4+8]     ; call handler

continue_search:
                mov     edi,[ebx+8]
                lea     ecx,[esi+esi*2]
                mov     esi,[edi+ecx*4]
                jmp     short search_for_handler

continue_execution:
                mov     eax,DISPOSITION_DISMISS
                jmp     short eh_return

unwind:
                push    ebp
                lea     ebp,[ebx+16]            ; switch to *original* EBP
                push    byte TRYLEVEL_NONE
                push    ebx                     ; pRegFrame
                call    __local_unwind2
                add     esp,+8
                pop     ebp

no_more_tries:
                mov     eax,DISPOSITION_CONTINUE_SEARCH

eh_return:
                pop     ebp
                pop     edi
                pop     esi
                pop     ebx
                mov     esp,ebp
                pop     ebp
                ret

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION __seh_longjmp_unwind@4
__seh_longjmp_unwind@4:
                push    ebp
                mov     ecx,[esp+8]
                mov     ebp,[ecx]
                mov     eax,[ecx+28]
                push    eax
                mov     eax,[ecx+24]
                push    eax
                call    __local_unwind2
                add     esp,+8
                pop     ebp
                ret     4

                ALIGN   16

