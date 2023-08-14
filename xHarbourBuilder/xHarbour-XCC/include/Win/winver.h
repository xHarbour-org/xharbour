#ifndef _WINVER_H
#define _WINVER_H

/* Windows version management definitions */

#ifdef __cplusplus
extern "C" {
#endif

#define VS_FILE_INFO  RT_VERSION
#define VS_VERSION_INFO  1
#define VS_USER_DEFINED  100

#define VS_FFI_SIGNATURE  0xFEEF04BD
#define VS_FFI_STRUCVERSION  0x00010000
#define VS_FFI_FILEFLAGSMASK  0x0000003F

#define VS_FF_DEBUG  0x00000001
#define VS_FF_PRERELEASE  0x00000002
#define VS_FF_PATCHED  0x00000004
#define VS_FF_PRIVATEBUILD  0x00000008
#define VS_FF_INFOINFERRED  0x00000010
#define VS_FF_SPECIALBUILD  0x00000020

#define VOS_UNKNOWN  0x00000000
#define VOS_DOS  0x00010000
#define VOS_OS216  0x00020000
#define VOS_OS232  0x00030000
#define VOS_NT  0x00040000

#define VOS__BASE  0x00000000
#define VOS__WINDOWS16  0x00000001
#define VOS__PM16  0x00000002
#define VOS__PM32  0x00000003
#define VOS__WINDOWS32  0x00000004

#define VOS_DOS_WINDOWS16  0x00010001
#define VOS_DOS_WINDOWS32  0x00010004
#define VOS_OS216_PM16  0x00020002
#define VOS_OS232_PM32  0x00030003
#define VOS_NT_WINDOWS32  0x00040004

#define VFT_UNKNOWN  0
#define VFT_APP  1
#define VFT_DLL  2
#define VFT_DRV  3
#define VFT_FONT  4
#define VFT_VXD  5
#define VFT_STATIC_LIB  7

#define VFT2_UNKNOWN  0
#define VFT2_DRV_PRINTER  1
#define VFT2_DRV_KEYBOARD  2
#define VFT2_DRV_LANGUAGE  3
#define VFT2_DRV_DISPLAY  4
#define VFT2_DRV_MOUSE  5
#define VFT2_DRV_NETWORK  6
#define VFT2_DRV_SYSTEM  7
#define VFT2_DRV_INSTALLABLE  8
#define VFT2_DRV_SOUND  9
#define VFT2_DRV_COMM  10
#define VFT2_DRV_INPUTMETHOD  11

#define VFT2_FONT_RASTER  1
#define VFT2_FONT_VECTOR  2
#define VFT2_FONT_TRUETYPE  3

#define VFFF_ISSHAREDFILE  1

#define VFF_CURNEDEST  0x0001
#define VFF_FILEINUSE  0x0002
#define VFF_BUFFTOOSMALL  0x0004

#define VIFF_FORCEINSTALL  0x0001
#define VIFF_DONTDELETEOLD  0x0002

#define VIF_TEMPFILE  0x00000001
#define VIF_MISMATCH  0x00000002
#define VIF_SRCOLD  0x00000004
#define VIF_DIFFLANG  0x00000008
#define VIF_DIFFCODEPG  0x00000010
#define VIF_DIFFTYPE  0x00000020
#define VIF_WRITEPROT  0x00000040
#define VIF_FILEINUSE  0x00000080
#define VIF_OUTOFSPACE  0x00000100
#define VIF_ACCESSVIOLATION  0x00000200
#define VIF_SHARINGVIOLATION  0x00000400
#define VIF_CANNOTCREATE  0x00000800
#define VIF_CANNOTDELETE  0x00001000
#define VIF_CANNOTRENAME  0x00002000
#define VIF_CANNOTDELETECUR  0x00004000
#define VIF_OUTOFMEMORY  0x00008000
#define VIF_CANNOTREADSRC  0x00010000
#define VIF_CANNOTREADDST  0x00020000
#define VIF_BUFFTOOSMALL  0x00040000

typedef struct tagVS_FIXEDFILEINFO {
    DWORD dwSignature;
    DWORD dwStrucVersion;
    DWORD dwFileVersionMS;
    DWORD dwFileVersionLS;
    DWORD dwProductVersionMS;
    DWORD dwProductVersionLS;
    DWORD dwFileFlagsMask;
    DWORD dwFileFlags;
    DWORD dwFileOS;
    DWORD dwFileType;
    DWORD dwFileSubtype;
    DWORD dwFileDateMS;
    DWORD dwFileDateLS;
} VS_FIXEDFILEINFO;

DWORD WINAPI VerFindFileA(DWORD,LPSTR,LPSTR,LPSTR,LPSTR,PUINT,LPSTR,PUINT);
DWORD WINAPI VerFindFileW(DWORD,LPWSTR,LPWSTR,LPWSTR,LPWSTR,PUINT,LPWSTR,PUINT);
DWORD WINAPI VerInstallFileA(DWORD,LPSTR,LPSTR,LPSTR,LPSTR,LPSTR,LPSTR,PUINT);
DWORD WINAPI VerInstallFileW(DWORD,LPWSTR,LPWSTR,LPWSTR,LPWSTR,LPWSTR,LPWSTR,PUINT);
DWORD WINAPI VerLanguageNameA(DWORD,LPSTR,DWORD);
DWORD WINAPI VerLanguageNameW(DWORD,LPWSTR,DWORD);
BOOL WINAPI VerQueryValueA(const PVOID,LPSTR,PVOID*,PUINT);
BOOL WINAPI VerQueryValueW(const PVOID,LPWSTR,PVOID*,PUINT);
DWORD WINAPI GetFileVersionInfoSizeA(LPSTR,PDWORD);
DWORD WINAPI GetFileVersionInfoSizeW(LPWSTR,PDWORD);
BOOL WINAPI GetFileVersionInfoA(LPSTR,DWORD,DWORD,PVOID);
BOOL WINAPI GetFileVersionInfoW(LPWSTR,DWORD,DWORD,PVOID);

#ifdef UNICODE
#define VerFindFile VerFindFileW
#define VerQueryValue VerQueryValueW
#define VerInstallFile VerInstallFileW
#define VerLanguageName VerLanguageNameW
#define VerQueryValue VerQueryValueW
#define GetFileVersionInfoSize GetFileVersionInfoSizeW
#define GetFileVersionInfo GetFileVersionInfoW
#else
#define VerQueryValue VerQueryValueA
#define VerFindFile VerFindFileA
#define VerInstallFile VerInstallFileA
#define VerLanguageName VerLanguageNameA
#define VerQueryValue VerQueryValueA
#define GetFileVersionInfoSize GetFileVersionInfoSizeA
#define GetFileVersionInfo GetFileVersionInfoA
#endif /* UNICODE */

#ifdef __cplusplus
}
#endif

#endif /* _WINVER_H */
