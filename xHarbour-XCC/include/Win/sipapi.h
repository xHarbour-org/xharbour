#ifndef _SIPAPI_H
#define _SIPAPI_H

/* Soft Input Panel definitions (Windows CE) */

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2028)  /* Missing prototype */
#endif

DWORD WINAPI SipStatus();

#define SIP_STATUS_UNAVAILABLE  0
#define SIP_STATUS_AVAILABLE  1

#define SIPF_OFF  0x00000000
#define SIPF_ON  0x00000001
#define SIPF_DOCKED  0x00000002
#define SIPF_LOCKED  0x00000004

#define SPI_SETCOMPLETIONINFO  223
#define SPI_SETSIPINFO  224
#define SPI_GETSIPINFO  225
#define SPI_SETCURRENTIM  226
#define SPI_GETCURRENTIM  227

#define IM_POSITION  0
#define IM_WIDEIMAGE  1
#define IM_NARROWIMAGE  2

typedef struct tagSIPINFO {
    DWORD cbSize;
    DWORD fdwFlags;
    RECT rcVisibleDesktop;
    RECT rcSipRect;
    DWORD dwImDataSize;
    void *pvImData;
} SIPINFO;

typedef struct tagIMENUMINFO {
    TCHAR szName[MAX_PATH];
    CLSID clsid;
} IMENUMINFO, *PIMENUMINFO;

typedef int (*IMENUMPROC)(IMENUMINFO*);

typedef struct tagIMWINDOWPOS {
    int x;
    int y;
    int cx;
    int cy;
} IMWINDOWPOS;

BOOL WINAPI SipSetDefaultRect(RECT*);
BOOL WINAPI SipRegisterNotification(HWND);
BOOL WINAPI SipShowIM(DWORD);
BOOL WINAPI SipGetInfo(SIPINFO*);
BOOL WINAPI SipSetInfo(SIPINFO*);
int WINAPI SipEnumIM(IMENUMPROC);
BOOL WINAPI SipGetCurrentIM(CLSID*);
BOOL WINAPI SipSetCurrentIM(CLSID*);

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
}
#endif

#endif /* _SIPAPI_H */
