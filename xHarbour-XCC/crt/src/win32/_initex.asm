;+--------------------------------------------------------------------------+
;|                                                                          |
;| File    : _initex.asm                                                    |
;|                                                                          |
;| Purpose : Handle special startup and exit procedure calls.               |
;|                                                                          |
;| History : Date      Reason                                               |
;|           03-08-01  Created                                              |
;|                                                                          |
;+--------------------------------------------------------------------------+

                BITS 32

;
; Microsoft CRT declarations:
;
; typedef void (__cdecl *_PVFV)(void);
;
; _PVFV __xi_a[], __xi_z[];    /* C initializers */
; _PVFV __xc_a[], __xc_z[];    /* C++ initializers */
; _PVFV __xp_a[], __xp_z[];    /* C pre-terminators */
; _PVFV __xt_a[], __xt_z[];    /* C terminators */
;

                GLOBAL  ___xi_a
                GLOBAL  ___xi_z
                GLOBAL  ___xt_a
                GLOBAL  ___xt_z

                SECTION .CRT$XIA ALIGN=4
___xi_a:
                SECTION .CRT$XIZ ALIGN=4
___xi_z:

                SECTION .CRT$XTA ALIGN=4
___xt_a:
                SECTION .CRT$XTZ ALIGN=4
___xt_z:


