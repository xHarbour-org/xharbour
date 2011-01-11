#ifndef _PHONE_H
#define _PHONE_H

/* Phone API definitions (Windows CE - Smartphone) */

#ifdef __cplusplus
extern "C" {
#endif

#include <windows.h>

#define PMCF_DEFAULT  0x00000001
#define PMCF_PROMPTBEFORECALLING  0x00000002

typedef enum {
    CALLERIDTYPE_UNAVAILABLE,
    CALLERIDTYPE_BLOCKED,
    CALLERIDTYPE_AVAILABLE
} CALLERIDTYPE;

typedef enum {
    IOM_MISSED,
    IOM_INCOMING,
    IOM_OUTGOING
} IOM;

typedef enum { 
    CALLLOGSEEK_BEGINNING = 2,
    CALLLOGSEEK_END = 4
} CALLLOGSEEK;

typedef struct {
    DWORD cbSize;
    FILETIME ftStartTime;
    FILETIME ftEndTime;
    IOM iom;
    BOOL fOutgoing:1;
    BOOL fConnected:1;
    BOOL fEnded:1;
    BOOL fRoam:1;
    CALLERIDTYPE cidt;
    PTSTR pszNumber;
    PTSTR pszName;
    PTSTR pszNameType;
    PTSTR pszNote;
} CALLLOGENTRY, *PCALLLOGENTRY;

typedef struct tagPHONEMAKECALLINFO {
    DWORD cbSize;
    DWORD dwFlags;
    PCWSTR pszDestAddress;
    PCWSTR pszAppName;
    PCWSTR pszCalledParty;
    PCWSTR pszComment;
} PHONEMAKECALLINFO, *PPHONEMAKECALLINFO;

HRESULT PhoneOpenCallLog(HANDLE*);
HRESULT PhoneGetCallLogEntry(HANDLE,PCALLLOGENTRY);
HRESULT PhoneSeekCallLog(HANDLE,CALLLOGSEEK,DWORD,LPDWORD);
HRESULT PhoneCloseCallLog(HANDLE);
LONG PhoneMakeCall(PHONEMAKECALLINFO*);

#ifdef __cplusplus
}
#endif

#endif /* _PHONE_H */
