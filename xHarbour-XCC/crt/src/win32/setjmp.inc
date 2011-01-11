
; layout of the setjmp/longjmp jmp_buf structure.

STRUC jump_buffer
jb_ebp:             resd 1      ; +0
jb_ebx:             resd 1      ; +4
jb_edi:             resd 1      ; +8
jb_esi:             resd 1      ; +12
jb_esp:             resd 1      ; +16
jb_eip:             resd 1      ; +20
jb_registration:    resd 1      ; +24
jb_trylevel:        resd 1      ; +28
jb_cookie:          resd 1      ; +32
jb_unwind_func:     resd 1      ; +36
jb_unwind_data:     resd 6      ; +40
ENDSTRUC

%define SETJMP_COOKIE   0x56433230

