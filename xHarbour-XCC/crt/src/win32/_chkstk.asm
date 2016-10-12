;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _chkstk.asm                                                    |
;|                                                                          |
;| Purpose : Automatic stack checking for C procedures.                     |
;|                                                                          |
;| History : Date      Reason                                               |
;|           00-09-15  Created                                              |
;|           00-10-17  _alloca() runtime function added.                    |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

_PAGESIZE_      EQU     1000h   ; size of a page of memory
_ALIGN_         EQU     4       ; _alloca alignment

                SECTION .text

                GLOBAL  __alloca
                GLOBAL  ___chkstk

;+--------------------------------------------------------------------------+

                FUNCTION __alloca

__alloca:

; Allocates memory on the stack. The returned pointer is guaranteed
; to be suitably aligned for storage of any type of object.

                pop     ecx                 ; return address
                pop     eax                 ; function argument
                add     eax,BYTE (_ALIGN_-1)
                and     eax,BYTE ~(_ALIGN_-1)
                call    ___chkstk
                mov     eax,esp             ; return ptr to allocated block on stack
                push    ecx                 ; prepare return address
                ret

                ALIGN   16

;+--------------------------------------------------------------------------+

                FUNCTION ___chkstk
___chkstk:

; Provide stack checking on procedure entry. Method is to simply probe
; each page of memory required for the stack in descending order. This
; causes the necessary pages of memory to be allocated via the guard
; page scheme, if possible.

                push    ecx                 ; save ecx
                cmp     eax,_PAGESIZE_      ; more than one page requested?
                lea     ecx,[esp+8]         ;   compute new stack pointer in ecx
                                            ;   correct for return address and saved ecx
                jb      short lastpage      ; no

;------------

probepages:
                sub     ecx,_PAGESIZE_      ; yes, move down a page
                sub     eax,_PAGESIZE_      ; adjust request and...

                test    [ecx],eax           ; ...probe it

                cmp     eax,_PAGESIZE_      ; more than one page requested?
                jae     short probepages    ; no

lastpage:
                sub     ecx,eax             ; move stack down by eax
                mov     eax,esp             ; save current tos and do a...

                test    [ecx],eax           ; ...probe in case a page was crossed

                mov     esp,ecx             ; set the new stack pointer

                mov     ecx,[eax]           ; recover ecx
                mov     eax,[eax+4]         ; recover return address

                push    eax                 ; prepare return address
                                            ; ...probe in case a page was crossed
                ret

                ALIGN   16

