#ifndef _WINUSERM_H
#define _WINUSERM_H

/* Mobile extensions to winuser.h */

#ifdef __cplusplus
extern "C" {
#endif

#define VK_TSOFT1  VK_F1
#define VK_TSOFT2  VK_F2
#define VK_TTALK  VK_F3
#define VK_TEND  VK_F4
#define VK_THOME  VK_LWIN
#define VK_TBACK  VK_ESCAPE

#define VK_TACTION  VK_RETURN
#define VK_TRECORD  VK_F10
#define VK_TFLIP  VK_F17
#define VK_TPOWER  VK_F18
#define VK_TVOLUMEUP  VK_F6
#define VK_TVOLUMEDOWN  VK_F7

#define VK_TUP  VK_UP
#define VK_TDOWN  VK_DOWN
#define VK_TLEFT  VK_LEFT
#define VK_TRIGHT  VK_RIGHT

#define VK_T0  L'0'
#define VK_T1  L'1'
#define VK_T2  L'2'
#define VK_T3  L'3'
#define VK_T4  L'4'
#define VK_T5  L'5'
#define VK_T6  L'6'
#define VK_T7  L'7'
#define VK_T8  L'8'
#define VK_T9  L'9'
#define VK_TSTAR  VK_F8
#define VK_TPOUND  VK_F9

#define VK_SYMBOL  VK_F11

#define VK_REDKEY  VK_F19
#define VK_ROCKER  VK_F20
#define VK_DPAD  VK_F21
#define VK_ACTION  VK_F23

#define VK_DONE  VK_F6
#define MOD_DONE  (MOD_WIN|MOD_KEYUP)

#define VK_MOJI  VK_F7
#define MOD_MOJI  (MOD_WIN|MOD_KEYUP)

#define VK_APP1  0xC1 
#define VK_APP2  0xC2 
#define VK_APP3  0xC3
#define VK_APP4  0xC4
#define VK_APP5  0xC5
#define VK_APP6  0xC6

#define SILENT_REPEAT(uVKey)  (uVKey < VK_LEFT || uVKey > VK_DOWN)

#define EM_GETINPUTMODE  0x00DD
#define EM_SETINPUTMODE  0x00DE
#define EM_SETSYMBOLS  0x00DF
#define EM_SETEXTENDEDSTYLE  0x00E0
#define EM_GETEXTENDEDSTYLE  0x00E1

#define Edit_GetInputMode(hwndCtl, fActual)  ((int)(DWORD)SNDMSG((hwndCtl), EM_GETINPUTMODE, 0L, (LPARAM)(BOOL)(fActual)))
#define Edit_SetInputMode(hwndCtl, nInputMode)  ((BOOL)(DWORD)SNDMSG((hwndCtl), EM_SETINPUTMODE, 0L, (LPARAM)(int)(nInputMode)))
#define Edit_SetSymbols(hwndCtl, pszSymbols)  ((BOOL)(DWORD)SNDMSG((hwndCtl), EM_SETSYMBOLS, 0L, (LPARAM)(LPCTSTR)(pszSymbols)))
#define Edit_SetExtendedStyle(hwndCtl, dwMask, dwExStyle)  ((DWORD)SNDMSG((hwndCtl), EM_SETEXTENDEDSTYLE, (WPARAM)(DWORD)(dwMask), (LPARAM)(DWORD)(dwExStyle)))
#define Edit_GetExtendedStyle(hwndCtl)  ((DWORD)SNDMSG((hwndCtl), EM_GETEXTENDEDSTYLE, 0L, 0L))

#define ES_EX_CLEARONBACKPRESSHOLD  0x00000001

#define IM_SPELL  0
#define IM_AMBIG  1
#define IM_NUMBERS  2
#define IM_LAST  IM_NUMBERS
#define IM_MASK  0x0000FFFF

#define IMMF_SETCLR_SHIFT  0x00010000
#define IMMF_SETCLR_CAPSLOCK  0x00020000
#define IMMF_SHIFT  0x01000000
#define IMMF_CAPSLOCK  0x02000000
#define IMMF_MASK  0xFFFF0000

#define EIM_SPELL  IM_SPELL
#define EIM_AMBIG  IM_AMBIG
#define EIM_NUMBERS  IM_NUMBERS
#define EIM_TEXT  (IM_LAST+1)
#define EIM_MASK  IM_MASK
#define EIM_MODE(x)  (x & EIM_MASK)

#define EIMMF_SETCLR_SHIFT  IMMF_SETCLR_SHIFT
#define EIMMF_SETCLR_CAPSLOCK  IMMF_SETCLR_CAPSLOCK
#define EIMMF_SHIFT  IMMF_SHIFT
#define EIMMF_CAPSLOCK  IMMF_CAPSLOCK
#define EIMMF_MASK  IMMF_MASK
#define EIM_MODIFIERS(x)  (x & EIMMF_MASK)

#define LB_GETINPUTMODE  0x01C0
#define LB_SETINPUTMODE  0x01C1

#define ListBox_GetInputMode(hwndCtl, fActual)  ((int)(DWORD)SNDMSG((hwndCtl), LB_GETINPUTMODE, 0L, (WPARAM)(BOOL)(fActual)))
#define ListBox_SetInputMode(hwndCtl, nInputMode)  ((BOOL)(DWORD)SNDMSG((hwndCtl), LB_SETINPUTMODE, 0L, (LPARAM)(int)(nInputMode)))

#define LIM_SPELL  IM_SPELL
#define LIM_NUMBERS  IM_NUMBERS

#define IMR_ISIMEAWARE  0x1000

#define IMEAF_AWARE  0x00000001
#define IMEAF_SMART_CAPS  0x00000002

#define MOD_HOLD  0x8000

#define DM_RESETSCROLL  (WM_USER+2) 

#ifndef MFS_DEFAULT
#define MFS_DEFAULT  0x00001000L
#else
#pragma error("OS now supports MFS_DEFAULT. Get rid of this check/definition.")
#endif /* MFS_DEFAULT */

#ifndef MFT_NONOWNERSTRING
#define MFT_NONOWNERSTRING  0x80000000L
#else
#pragma error("OS now supports MFT_NONOWNERSTRING. Get rid of this check/definition.")
#endif /* MFT_NONOWNERSTRING */

#ifndef MIIM_FULLSTR
#define MIIM_FULLSTR  0x10000000L
#else
#pragma error("OS now supports MIIM_FULLSTR. Get rid of this check/definition.")
#endif /* MIIM_FULLSTR */

#define CF_HDROP  15

#ifdef __cplusplus
}
#endif

#endif /* _WINUSERM_H */
