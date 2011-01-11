#ifndef _SHLWAPI_H
#define _SHLWAPI_H

/* Windows light-weight utility API definitions */

#include <objbase.h>

#ifndef _WINRESRC_
#ifndef _WIN32_IE
#define _WIN32_IE 0x0501
#else
#if (_WIN32_IE < 0x0400) && defined(_WIN32_WINNT) && (_WIN32_WINNT >= 0x0500)
#error _WIN32_IE setting conflicts with _WIN32_WINNT setting
#endif
#endif
#endif /* _WINRESRC_ */

#ifndef WINSHLWAPI
#define LWSTDAPI  EXTERN_C DECLSPEC_IMPORT HRESULT STDAPICALLTYPE
#define LWSTDAPI_(type)  EXTERN_C DECLSPEC_IMPORT type STDAPICALLTYPE
#define LWSTDAPIV  EXTERN_C DECLSPEC_IMPORT HRESULT STDAPIVCALLTYPE
#define LWSTDAPIV_(type)  EXTERN_C DECLSPEC_IMPORT type STDAPIVCALLTYPE
#endif /* WINSHLWAPI */

#include <pshpack1.h>

#ifdef __cplusplus
extern "C" {
#endif

#define SZ_CONTENTTYPE_HTMLA  "text/html"
#define SZ_CONTENTTYPE_HTMLW  L"text/html"
#define SZ_CONTENTTYPE_CDFA  "application/x-cdf"
#define SZ_CONTENTTYPE_CDFW  L"application/x-cdf"

#define STIF_DEFAULT  0x00000000L
#define STIF_SUPPORT_HEX  0x00000001L

#define GCT_INVALID  0x0000
#define GCT_LFNCHAR  0x0001
#define GCT_SHORTCHAR  0x0002
#define GCT_WILD  0x0004
#define GCT_SEPARATOR  0x0008

#define URL_UNESCAPE  0x10000000
#define URL_ESCAPE_UNSAFE  0x20000000
#define URL_PLUGGABLE_PROTOCOL  0x40000000
#define URL_WININET_COMPATIBILITY  0x80000000
#define URL_DONT_ESCAPE_EXTRA_INFO  0x02000000
#define URL_DONT_UNESCAPE_EXTRA_INFO  URL_DONT_ESCAPE_EXTRA_INFO
#define URL_BROWSER_MODE  URL_DONT_ESCAPE_EXTRA_INFO
#define URL_ESCAPE_SPACES_ONLY  0x04000000
#define URL_DONT_SIMPLIFY  0x08000000
#define URL_NO_META  URL_DONT_SIMPLIFY
#define URL_UNESCAPE_INPLACE  0x00100000
#define URL_CONVERT_IF_DOSPATH  0x00200000
#define URL_UNESCAPE_HIGH_ANSI_ONLY  0x00400000
#define URL_INTERNAL_PATH  0x00800000
#define URL_FILE_USE_PATHURL  0x00010000
#define URL_ESCAPE_PERCENT  0x00001000
#define URL_ESCAPE_SEGMENT_ONLY  0x00002000

#define URL_PARTFLAG_KEEPSCHEME  0x00000001

#define URL_APPLY_DEFAULT  0x00000001
#define URL_APPLY_GUESSSCHEME  0x00000002
#define URL_APPLY_GUESSFILE  0x00000004
#define URL_APPLY_FORCEAPPLY  0x00000008

#define SHREGSET_HKCU  0x00000001
#define SHREGSET_FORCE_HKCU  0x00000002
#define SHREGSET_HKLM  0x00000004
#define SHREGSET_FORCE_HKLM  0x00000008
#define SHREGSET_DEFAULT  (SHREGSET_FORCE_HKCU|SHREGSET_HKLM)

#if (_WIN32_IE >= 0x0500)
#define SHACF_DEFAULT  0x00000000
#define SHACF_FILESYSTEM  0x00000001
#define SHACF_URLALL  (SHACF_URLHISTORY|SHACF_URLMRU)
#define SHACF_URLHISTORY  0x00000002
#define SHACF_URLMRU  0x00000004
#define SHACF_USETAB  0x00000008
#define SHACF_FILESYS_ONLY  0x00000010

#define SHACF_AUTOSUGGEST_FORCE_ON  0x10000000
#define SHACF_AUTOSUGGEST_FORCE_OFF  0x20000000
#define SHACF_AUTOAPPEND_FORCE_ON  0x40000000
#define SHACF_AUTOAPPEND_FORCE_OFF  0x80000000
#endif /* _WIN32_IE >= 0x0500 */

#define CTF_INSIST  0x00000001
#define CTF_THREAD_REF  0x00000002
#define CTF_PROCESS_REF  0x00000004
#define CTF_COINIT  0x00000008

#define StrIntlEqNA(s1,s2,nChar)  StrIsIntlEqualA(TRUE,s1,s2,nChar)
#define StrIntlEqNW(s1,s2,nChar)  StrIsIntlEqualW(TRUE,s1,s2,nChar)
#define StrIntlEqNIA(s1,s2,nChar)  StrIsIntlEqualA(FALSE,s1,s2,nChar)
#define StrIntlEqNIW(s1,s2,nChar)  StrIsIntlEqualW(FALSE,s1,s2,nChar)

#define IntlStrEqNA(s1,s2,nChar)  IntlStrEqWorkerA(TRUE,s1,s2,nChar)
#define IntlStrEqNW(s1,s2,nChar)  IntlStrEqWorkerW(TRUE,s1,s2,nChar)
#define IntlStrEqNIA(s1,s2,nChar)  IntlStrEqWorkerA(FALSE,s1,s2,nChar)
#define IntlStrEqNIW(s1,s2,nChar)  IntlStrEqWorkerW(FALSE,s1,s2,nChar)

#define PathIsHTMLFileA(pszPath)  PathIsContentTypeA(pszPath,SZ_CONTENTTYPE_HTMLA)
#define PathIsHTMLFileW(pszPath)  PathIsContentTypeW(pszPath,SZ_CONTENTTYPE_HTMLW)

#define StrCatA  lstrcatA
#define StrCmpA  lstrcmpA
#define StrCmpIA  lstrcmpiA
#define StrCpyA  lstrcpyA
#define StrCpyNA  lstrcpynA

#define StrToLong  StrToInt
#define StrNCmp  StrCmpN
#define StrNCmpI  StrCmpNI
#define StrNCpy  StrCpyN
#define StrCatN  StrNCat

#define UrlEscapeSpaces(pszUrl,pszEscaped,pcchEscaped)  UrlCanonicalize(pszUrl,pszEscaped,pcchEscaped,URL_ESCAPE_SPACES_ONLY|URL_DONT_ESCAPE_EXTRA_INFO)
#define UrlUnescapeInPlace(pszUrl,dwFlags)  UrlUnescape(pszUrl,NULL,NULL,dwFlags|URL_UNESCAPE_INPLACE)

#define MAKEDLLVERULL(major,minor,build,qfe)  (((ULONGLONG)(major)<<48)|((ULONGLONG)(minor)<<32)|((ULONGLONG)(build)<<16)|((ULONGLONG)(qfe)<<0))

typedef enum {
    URL_SCHEME_INVALID = -1,
    URL_SCHEME_UNKNOWN = 0,
    URL_SCHEME_FTP,
    URL_SCHEME_HTTP,
    URL_SCHEME_GOPHER,
    URL_SCHEME_MAILTO,
    URL_SCHEME_NEWS,
    URL_SCHEME_NNTP,
    URL_SCHEME_TELNET,
    URL_SCHEME_WAIS,
    URL_SCHEME_FILE,
    URL_SCHEME_MK,
    URL_SCHEME_HTTPS,
    URL_SCHEME_SHELL,
    URL_SCHEME_SNEWS,
    URL_SCHEME_LOCAL,
    URL_SCHEME_JAVASCRIPT,
    URL_SCHEME_VBSCRIPT,
    URL_SCHEME_ABOUT,
    URL_SCHEME_RES,
    URL_SCHEME_MAXVALUE
} URL_SCHEME;

typedef enum {
    URL_PART_NONE = 0,
    URL_PART_SCHEME = 1,
    URL_PART_HOSTNAME,
    URL_PART_USERNAME,
    URL_PART_PASSWORD,
    URL_PART_PORT,
    URL_PART_QUERY,
} URL_PART;

typedef enum {
    URLIS_URL,
    URLIS_OPAQUE,
    URLIS_NOHISTORY,
    URLIS_FILEURL,
    URLIS_APPLIABLE,
    URLIS_DIRECTORY,
    URLIS_HASQUERY,
} URLIS;

typedef enum {
    SHREGDEL_DEFAULT = 0x00000000,
    SHREGDEL_HKCU = 0x00000001,
    SHREGDEL_HKLM = 0x00000010,
    SHREGDEL_BOTH = 0x00000011,
} SHREGDEL_FLAGS;

typedef enum {
    SHREGENUM_DEFAULT = 0x00000000,
    SHREGENUM_HKCU = 0x00000001,
    SHREGENUM_HKLM = 0x00000010,
    SHREGENUM_BOTH = 0x00000011,
} SHREGENUM_FLAGS;

typedef HANDLE HUSKEY;
typedef HUSKEY *PHUSKEY;

enum {
    ASSOCF_INIT_NOREMAPCLSID = 0x00000001,
    ASSOCF_INIT_BYEXENAME = 0x00000002,
    ASSOCF_OPEN_BYEXENAME = 0x00000002,
    ASSOCF_INIT_DEFAULTTOSTAR = 0x00000004,
    ASSOCF_INIT_DEFAULTTOFOLDER = 0x00000008,
    ASSOCF_NOUSERSETTINGS = 0x00000010,
    ASSOCF_NOTRUNCATE = 0x00000020,
    ASSOCF_VERIFY = 0x00000040,
    ASSOCF_REMAPRUNDLL = 0x00000080,
    ASSOCF_NOFIXUPS = 0x00000100,
    ASSOCF_IGNOREBASECLASS = 0x00000200,
};

typedef DWORD ASSOCF;

typedef enum {
    ASSOCSTR_COMMAND = 1,
    ASSOCSTR_EXECUTABLE,
    ASSOCSTR_FRIENDLYDOCNAME,
    ASSOCSTR_FRIENDLYAPPNAME,
    ASSOCSTR_NOOPEN,
    ASSOCSTR_SHELLNEWVALUE,
    ASSOCSTR_DDECOMMAND,
    ASSOCSTR_DDEIFEXEC,
    ASSOCSTR_DDEAPPLICATION,
    ASSOCSTR_DDETOPIC,
    ASSOCSTR_INFOTIP,
    ASSOCSTR_MAX
} ASSOCSTR;

typedef enum {
    ASSOCKEY_SHELLEXECCLASS = 1,
    ASSOCKEY_APP,
    ASSOCKEY_CLASS,
    ASSOCKEY_BASECLASS,
    ASSOCKEY_MAX
} ASSOCKEY;

typedef enum {
    ASSOCDATA_MSIDESCRIPTOR = 1,
    ASSOCDATA_NOACTIVATEHANDLER,
    ASSOCDATA_QUERYCLASSSTORE,
    ASSOCDATA_HASPERUSERASSOC,
    ASSOCDATA_MAX
} ASSOCDATA;

typedef enum {
    ASSOCENUM_NONE
} ASSOCENUM;

typedef struct _DLLVERSIONINFO {
    DWORD cbSize;
    DWORD dwMajorVersion;
    DWORD dwMinorVersion;
    DWORD dwBuildNumber;
    DWORD dwPlatformID;
} DLLVERSIONINFO;

#define DLLVER_PLATFORM_WINDOWS  0x00000001
#define DLLVER_PLATFORM_NT  0x00000002

#if (_WIN32_IE >= 0x0501)
typedef struct _DLLVERSIONINFO2 {
    DLLVERSIONINFO info1;
    DWORD dwFlags;
    ULONGLONG ullVersion;
} DLLVERSIONINFO2;

#define DLLVER_MAJOR_MASK  0xFFFF000000000000
#define DLLVER_MINOR_MASK  0x0000FFFF00000000
#define DLLVER_BUILD_MASK  0x00000000FFFF0000
#define DLLVER_QFE_MASK  0x000000000000FFFF
#endif /* _WIN32_IE >= 0x0501 */

typedef HRESULT (CALLBACK* DLLGETVERSIONPROC)(DLLVERSIONINFO *);

LWSTDAPI_(LPSTR) StrChrA(LPCSTR,WORD);
LWSTDAPI_(LPWSTR) StrChrW(LPCWSTR,WCHAR);
LWSTDAPI_(LPSTR) StrChrIA(LPCSTR,WORD);
LWSTDAPI_(LPWSTR) StrChrIW(LPCWSTR,WCHAR);
LWSTDAPI_(int) StrCmpNA(LPCSTR,LPCSTR,int);
LWSTDAPI_(int) StrCmpNW(LPCWSTR,LPCWSTR,int);
LWSTDAPI_(int) StrCmpNIA(LPCSTR,LPCSTR,int);
LWSTDAPI_(int) StrCmpNIW(LPCWSTR,LPCWSTR,int);
LWSTDAPI_(int) StrCSpnA(LPCSTR,LPCSTR);
LWSTDAPI_(int) StrCSpnW(LPCWSTR,LPCWSTR);
LWSTDAPI_(int) StrCSpnIA(LPCSTR,LPCSTR);
LWSTDAPI_(int) StrCSpnIW(LPCWSTR,LPCWSTR);
LWSTDAPI_(LPSTR) StrDupA(LPCSTR);
LWSTDAPI_(LPWSTR) StrDupW(LPCWSTR);
LWSTDAPI_(LPSTR) StrFormatByteSizeA(DWORD,LPSTR,UINT);
LWSTDAPI_(LPSTR) StrFormatByteSize64A(LONGLONG,LPSTR,UINT);
LWSTDAPI_(LPWSTR) StrFormatByteSizeW(LONGLONG,LPWSTR,UINT);
LWSTDAPI_(LPWSTR) StrFormatKBSizeW(LONGLONG,LPWSTR,UINT);
LWSTDAPI_(LPSTR) StrFormatKBSizeA(LONGLONG,LPSTR,UINT);
LWSTDAPI_(int) StrFromTimeIntervalA(LPSTR,UINT,DWORD,int);
LWSTDAPI_(int) StrFromTimeIntervalW(LPWSTR,UINT,DWORD,int);
LWSTDAPI_(BOOL) StrIsIntlEqualA(BOOL,LPCSTR,LPCSTR,int);
LWSTDAPI_(BOOL) StrIsIntlEqualW(BOOL,LPCWSTR,LPCWSTR,int);
LWSTDAPI_(LPSTR) StrNCatA(LPSTR,LPCSTR,int);
LWSTDAPI_(LPWSTR) StrNCatW(LPWSTR,LPCWSTR,int);
LWSTDAPI_(LPSTR) StrPBrkA(LPCSTR,LPCSTR);
LWSTDAPI_(LPWSTR) StrPBrkW(LPCWSTR,LPCWSTR);
LWSTDAPI_(LPSTR) StrRChrA(LPCSTR,LPCSTR,WORD);
LWSTDAPI_(LPWSTR) StrRChrW(LPCWSTR,LPCWSTR,WCHAR);
LWSTDAPI_(LPSTR) StrRChrIA(LPCSTR,LPCSTR,WORD);
LWSTDAPI_(LPWSTR) StrRChrIW(LPCWSTR,LPCWSTR,WCHAR);
LWSTDAPI_(LPSTR) StrRStrIA(LPCSTR,LPCSTR,LPCSTR);
LWSTDAPI_(LPWSTR) StrRStrIW(LPCWSTR,LPCWSTR,LPCWSTR);
LWSTDAPI_(int) StrSpnA(LPCSTR,LPCSTR);
LWSTDAPI_(int) StrSpnW(LPCWSTR,LPCWSTR);
LWSTDAPI_(LPSTR) StrStrA(LPCSTR,LPCSTR);
LWSTDAPI_(LPWSTR) StrStrW(LPCWSTR,LPCWSTR);
LWSTDAPI_(LPSTR) StrStrIA(LPCSTR,LPCSTR);
LWSTDAPI_(LPWSTR) StrStrIW(LPCWSTR,LPCWSTR);
LWSTDAPI_(int) StrToIntA(LPCSTR);
LWSTDAPI_(int) StrToIntW(LPCWSTR);
LWSTDAPI_(BOOL) StrToIntExA(LPCSTR,DWORD,int*);
LWSTDAPI_(BOOL) StrToIntExW(LPCWSTR,DWORD,int*);
LWSTDAPI_(BOOL) StrTrimA(LPSTR,LPCSTR);
LWSTDAPI_(BOOL) StrTrimW(LPWSTR,LPCWSTR);
LWSTDAPI_(LPWSTR) StrCatW(LPWSTR,LPCWSTR);
LWSTDAPI_(int) StrCmpW(LPCWSTR,LPCWSTR);
LWSTDAPI_(int) StrCmpIW(LPCWSTR,LPCWSTR);
LWSTDAPI_(LPWSTR) StrCpyW(LPWSTR,LPCWSTR);
LWSTDAPI_(LPWSTR) StrCpyNW(LPWSTR,LPCWSTR,int);
LWSTDAPI_(LPWSTR) StrCatBuffW(LPWSTR,LPCWSTR,int);
LWSTDAPI_(LPSTR) StrCatBuffA(LPSTR,LPCSTR,int);
LWSTDAPI_(BOOL) ChrCmpIA(WORD,WORD);
LWSTDAPI_(BOOL) ChrCmpIW(WCHAR,WCHAR);
LWSTDAPI_(int) wvnsprintfA(LPSTR,int,LPCSTR,va_list);
LWSTDAPI_(int) wvnsprintfW(LPWSTR,int,LPCWSTR,va_list);
LWSTDAPIV_(int) wnsprintfA(LPSTR,int,LPCSTR,...);
LWSTDAPIV_(int) wnsprintfW(LPWSTR,int,LPCWSTR,...);
LWSTDAPI StrRetToStrA(struct _STRRET*,const UNALIGNED struct _ITEMIDLIST*,LPSTR*);
LWSTDAPI StrRetToStrW(struct _STRRET*,const UNALIGNED struct _ITEMIDLIST*,LPWSTR*);
LWSTDAPI StrRetToBufA(struct _STRRET*,const UNALIGNED struct _ITEMIDLIST*,LPSTR,UINT);
LWSTDAPI StrRetToBufW(struct _STRRET*,const UNALIGNED struct _ITEMIDLIST*,LPWSTR,UINT);
LWSTDAPI SHStrDupA(LPCSTR,WCHAR**);
LWSTDAPI SHStrDupW(LPCWSTR,WCHAR**);
LWSTDAPI_(BOOL) IntlStrEqWorkerA(BOOL,LPCSTR,LPCSTR,int);
LWSTDAPI_(BOOL) IntlStrEqWorkerW(BOOL,LPCWSTR,LPCWSTR,int);
LWSTDAPI_(LPSTR) PathAddBackslashA(LPSTR);
LWSTDAPI_(LPWSTR) PathAddBackslashW(LPWSTR);
LWSTDAPI_(BOOL) PathAddExtensionA(LPSTR,LPCSTR);
LWSTDAPI_(BOOL) PathAddExtensionW(LPWSTR,LPCWSTR);
LWSTDAPI_(BOOL) PathAppendA(LPSTR,LPCSTR);
LWSTDAPI_(BOOL) PathAppendW(LPWSTR,LPCWSTR);
LWSTDAPI_(LPSTR) PathBuildRootA(LPSTR,int);
LWSTDAPI_(LPWSTR) PathBuildRootW(LPWSTR,int);
LWSTDAPI_(BOOL) PathCanonicalizeA(LPSTR,LPCSTR);
LWSTDAPI_(BOOL) PathCanonicalizeW(LPWSTR,LPCWSTR);
LWSTDAPI_(LPSTR) PathCombineA(LPSTR,LPCSTR,LPCSTR);
LWSTDAPI_(LPWSTR) PathCombineW(LPWSTR,LPCWSTR,LPCWSTR);
LWSTDAPI_(BOOL) PathCompactPathA(HDC,LPSTR,UINT);
LWSTDAPI_(BOOL) PathCompactPathW(HDC,LPWSTR,UINT);
LWSTDAPI_(BOOL) PathCompactPathExA(LPSTR,LPCSTR,UINT,DWORD);
LWSTDAPI_(BOOL) PathCompactPathExW(LPWSTR,LPCWSTR,UINT,DWORD);
LWSTDAPI_(int) PathCommonPrefixA(LPCSTR,LPCSTR,LPSTR);
LWSTDAPI_(int) PathCommonPrefixW(LPCWSTR,LPCWSTR,LPWSTR);
LWSTDAPI_(BOOL) PathFileExistsA(LPCSTR);
LWSTDAPI_(BOOL) PathFileExistsW(LPCWSTR);
LWSTDAPI_(LPSTR) PathFindExtensionA(LPCSTR);
LWSTDAPI_(LPWSTR) PathFindExtensionW(LPCWSTR);
LWSTDAPI_(LPSTR) PathFindFileNameA(LPCSTR);
LWSTDAPI_(LPWSTR) PathFindFileNameW(LPCWSTR);
LWSTDAPI_(LPSTR) PathFindNextComponentA(LPCSTR);
LWSTDAPI_(LPWSTR) PathFindNextComponentW(LPCWSTR);
LWSTDAPI_(BOOL) PathFindOnPathA(LPSTR,LPCSTR*);
LWSTDAPI_(BOOL) PathFindOnPathW(LPWSTR,LPCWSTR*);
LWSTDAPI_(LPSTR) PathGetArgsA(LPCSTR);
LWSTDAPI_(LPWSTR) PathGetArgsW(LPCWSTR);
LWSTDAPI_(LPCSTR) PathFindSuffixArrayA(LPCSTR,const LPCSTR*,int);
LWSTDAPI_(LPCWSTR) PathFindSuffixArrayW(LPCWSTR,const LPCWSTR*,int);
STDAPI_(BOOL) PathIsLFNFileSpecA(LPCSTR);
STDAPI_(BOOL) PathIsLFNFileSpecW(LPCWSTR);
LWSTDAPI_(UINT) PathGetCharTypeA(UCHAR);
LWSTDAPI_(UINT) PathGetCharTypeW(WCHAR);
LWSTDAPI_(int) PathGetDriveNumberA(LPCSTR);
LWSTDAPI_(int) PathGetDriveNumberW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsDirectoryA(LPCSTR);
LWSTDAPI_(BOOL) PathIsDirectoryW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsDirectoryEmptyA(LPCSTR);
LWSTDAPI_(BOOL) PathIsDirectoryEmptyW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsFileSpecA(LPCSTR);
LWSTDAPI_(BOOL) PathIsFileSpecW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsPrefixA(LPCSTR,LPCSTR);
LWSTDAPI_(BOOL) PathIsPrefixW(LPCWSTR,LPCWSTR);
LWSTDAPI_(BOOL) PathIsRelativeA(LPCSTR);
LWSTDAPI_(BOOL) PathIsRelativeW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsRootA(LPCSTR);
LWSTDAPI_(BOOL) PathIsRootW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsSameRootA(LPCSTR,LPCSTR);
LWSTDAPI_(BOOL) PathIsSameRootW(LPCWSTR,LPCWSTR);
LWSTDAPI_(BOOL) PathIsUNCA(LPCSTR);
LWSTDAPI_(BOOL) PathIsUNCW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsNetworkPathA(LPCSTR);
LWSTDAPI_(BOOL) PathIsNetworkPathW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsUNCServerA(LPCSTR);
LWSTDAPI_(BOOL) PathIsUNCServerW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsUNCServerShareA(LPCSTR);
LWSTDAPI_(BOOL) PathIsUNCServerShareW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsContentTypeA(LPCSTR,LPCSTR);
LWSTDAPI_(BOOL) PathIsContentTypeW(LPCWSTR,LPCWSTR);
LWSTDAPI_(BOOL) PathIsURLA(LPCSTR);
LWSTDAPI_(BOOL) PathIsURLW(LPCWSTR);
LWSTDAPI_(BOOL) PathMakePrettyA(LPSTR);
LWSTDAPI_(BOOL) PathMakePrettyW(LPWSTR);
LWSTDAPI_(BOOL) PathMatchSpecA(LPCSTR,LPCSTR);
LWSTDAPI_(BOOL) PathMatchSpecW(LPCWSTR,LPCWSTR);
LWSTDAPI_(int) PathParseIconLocationA(LPSTR);
LWSTDAPI_(int) PathParseIconLocationW(LPWSTR);
LWSTDAPI_(void) PathQuoteSpacesA(LPSTR);
LWSTDAPI_(void) PathQuoteSpacesW(LPWSTR);
LWSTDAPI_(BOOL) PathRelativePathToA(LPSTR,LPCSTR,DWORD,LPCSTR,DWORD);
LWSTDAPI_(BOOL) PathRelativePathToW(LPWSTR,LPCWSTR,DWORD,LPCWSTR,DWORD);
LWSTDAPI_(void) PathRemoveArgsA(LPSTR);
LWSTDAPI_(void) PathRemoveArgsW(LPWSTR);
LWSTDAPI_(LPSTR) PathRemoveBackslashA(LPSTR);
LWSTDAPI_(LPWSTR) PathRemoveBackslashW(LPWSTR);
LWSTDAPI_(void) PathRemoveBlanksA(LPSTR);
LWSTDAPI_(void) PathRemoveBlanksW(LPWSTR);
LWSTDAPI_(void) PathRemoveExtensionA(LPSTR);
LWSTDAPI_(void) PathRemoveExtensionW(LPWSTR);
LWSTDAPI_(BOOL) PathRemoveFileSpecA(LPSTR);
LWSTDAPI_(BOOL) PathRemoveFileSpecW(LPWSTR);
LWSTDAPI_(BOOL) PathRenameExtensionA(LPSTR,LPCSTR);
LWSTDAPI_(BOOL) PathRenameExtensionW(LPWSTR,LPCWSTR);
LWSTDAPI_(BOOL) PathSearchAndQualifyA(LPCSTR,LPSTR,UINT);
LWSTDAPI_(BOOL) PathSearchAndQualifyW(LPCWSTR,LPWSTR,UINT);
LWSTDAPI_(void) PathSetDlgItemPathA(HWND,int,LPCSTR);
LWSTDAPI_(void) PathSetDlgItemPathW(HWND,int,LPCWSTR);
LWSTDAPI_(LPSTR) PathSkipRootA(LPCSTR);
LWSTDAPI_(LPWSTR) PathSkipRootW(LPCWSTR);
LWSTDAPI_(void) PathStripPathA(LPSTR);
LWSTDAPI_(void) PathStripPathW(LPWSTR);
LWSTDAPI_(BOOL) PathStripToRootA(LPSTR);
LWSTDAPI_(BOOL) PathStripToRootW(LPWSTR);
LWSTDAPI_(void) PathUnquoteSpacesA(LPSTR);
LWSTDAPI_(void) PathUnquoteSpacesW(LPWSTR);
LWSTDAPI_(BOOL) PathMakeSystemFolderA(LPCSTR);
LWSTDAPI_(BOOL) PathMakeSystemFolderW(LPCWSTR);
LWSTDAPI_(BOOL) PathUnmakeSystemFolderA(LPCSTR);
LWSTDAPI_(BOOL) PathUnmakeSystemFolderW(LPCWSTR);
LWSTDAPI_(BOOL) PathIsSystemFolderA(LPCSTR,DWORD);
LWSTDAPI_(BOOL) PathIsSystemFolderW(LPCWSTR,DWORD);
LWSTDAPI_(void) PathUndecorateA(LPSTR);
LWSTDAPI_(void) PathUndecorateW(LPWSTR);
LWSTDAPI_(BOOL) PathUnExpandEnvStringsA(LPCSTR,LPSTR,UINT);
LWSTDAPI_(BOOL) PathUnExpandEnvStringsW(LPCWSTR,LPWSTR,UINT);
LWSTDAPI_(int) UrlCompareA(LPCSTR,LPCSTR,BOOL);
LWSTDAPI_(int) UrlCompareW(LPCWSTR,LPCWSTR,BOOL);
LWSTDAPI UrlCombineA(LPCSTR,LPCSTR,LPSTR,LPDWORD,DWORD);
LWSTDAPI UrlCombineW(LPCWSTR,LPCWSTR,LPWSTR,LPDWORD,DWORD);
LWSTDAPI UrlCanonicalizeA(LPCSTR,LPSTR,LPDWORD,DWORD);
LWSTDAPI UrlCanonicalizeW(LPCWSTR,LPWSTR,LPDWORD,DWORD);
LWSTDAPI_(BOOL) UrlIsOpaqueA(LPCSTR);
LWSTDAPI_(BOOL) UrlIsOpaqueW(LPCWSTR);
LWSTDAPI_(BOOL) UrlIsNoHistoryA(LPCSTR);
LWSTDAPI_(BOOL) UrlIsNoHistoryW(LPCWSTR);
#define UrlIsFileUrlA(pszURL) UrlIsA(pszURL,URLIS_FILEURL)
#define UrlIsFileUrlW(pszURL) UrlIsW(pszURL,URLIS_FILEURL)
LWSTDAPI_(BOOL) UrlIsA(LPCSTR,URLIS);
LWSTDAPI_(BOOL) UrlIsW(LPCWSTR,URLIS);
LWSTDAPI_(LPCSTR) UrlGetLocationA(LPCSTR);
LWSTDAPI_(LPCWSTR) UrlGetLocationW(LPCWSTR);
LWSTDAPI UrlUnescapeA(LPSTR,LPSTR,LPDWORD,DWORD);
LWSTDAPI UrlUnescapeW(LPWSTR,LPWSTR,LPDWORD,DWORD);
LWSTDAPI UrlEscapeA(LPCSTR,LPSTR,LPDWORD,DWORD);
LWSTDAPI UrlEscapeW(LPCWSTR,LPWSTR,LPDWORD,DWORD);
LWSTDAPI UrlCreateFromPathA(LPCSTR,LPSTR,LPDWORD,DWORD);
LWSTDAPI UrlCreateFromPathW(LPCWSTR,LPWSTR,LPDWORD,DWORD);
LWSTDAPI PathCreateFromUrlA(LPCSTR,LPSTR,LPDWORD,DWORD);
LWSTDAPI PathCreateFromUrlW(LPCWSTR,LPWSTR,LPDWORD,DWORD);
LWSTDAPI UrlHashA(LPCSTR,LPBYTE,DWORD);
LWSTDAPI UrlHashW(LPCWSTR,LPBYTE,DWORD);
LWSTDAPI UrlGetPartW(LPCWSTR,LPWSTR,LPDWORD,DWORD,DWORD);
LWSTDAPI UrlGetPartA(LPCSTR,LPSTR,LPDWORD,DWORD,DWORD);
LWSTDAPI UrlApplySchemeA(LPCSTR,LPSTR,LPDWORD,DWORD);
LWSTDAPI UrlApplySchemeW(LPCWSTR,LPWSTR,LPDWORD,DWORD);
LWSTDAPI HashData(LPBYTE,DWORD,LPBYTE,DWORD);
LWSTDAPI_(DWORD) SHDeleteEmptyKeyA(HKEY,LPCSTR);
LWSTDAPI_(DWORD) SHDeleteEmptyKeyW(HKEY,LPCWSTR);
LWSTDAPI_(DWORD) SHDeleteKeyA(HKEY,LPCSTR);
LWSTDAPI_(DWORD) SHDeleteKeyW(HKEY,LPCWSTR);
LWSTDAPI_(HKEY) SHRegDuplicateHKey(HKEY);
LWSTDAPI_(DWORD) SHDeleteValueA(HKEY,LPCSTR,LPCSTR);
LWSTDAPI_(DWORD) SHDeleteValueW(HKEY,LPCWSTR,LPCWSTR);
LWSTDAPI_(DWORD) SHGetValueA(HKEY,LPCSTR,LPCSTR,DWORD*,void*,DWORD*);
LWSTDAPI_(DWORD) SHGetValueW(HKEY,LPCWSTR,LPCWSTR,DWORD*,void*,DWORD*);
LWSTDAPI_(DWORD) SHSetValueA(HKEY,LPCSTR,LPCSTR,DWORD,LPCVOID,DWORD);
LWSTDAPI_(DWORD) SHSetValueW(HKEY,LPCWSTR,LPCWSTR,DWORD,LPCVOID,DWORD);
LWSTDAPI_(DWORD) SHQueryValueExA(HKEY,LPCSTR,DWORD*,DWORD*,void*,DWORD*);
LWSTDAPI_(DWORD) SHQueryValueExW(HKEY,LPCWSTR,DWORD*,DWORD*,void*,DWORD*);
LWSTDAPI_(LONG) SHEnumKeyExA(HKEY,DWORD,LPSTR,LPDWORD);
LWSTDAPI_(LONG) SHEnumKeyExW(HKEY,DWORD,LPWSTR,LPDWORD);
LWSTDAPI_(LONG) SHEnumValueA(HKEY,DWORD,LPSTR,LPDWORD,LPDWORD,void*,LPDWORD);
LWSTDAPI_(LONG) SHEnumValueW(HKEY,DWORD,LPWSTR,LPDWORD,LPDWORD,void*,LPDWORD);
LWSTDAPI_(LONG) SHQueryInfoKeyA(HKEY,LPDWORD,LPDWORD,LPDWORD,LPDWORD);
LWSTDAPI_(LONG) SHQueryInfoKeyW(HKEY,LPDWORD,LPDWORD,LPDWORD,LPDWORD);
LWSTDAPI_(DWORD) SHCopyKeyA(HKEY,LPCSTR,HKEY,DWORD);
LWSTDAPI_(DWORD) SHCopyKeyW(HKEY,LPCWSTR,HKEY,DWORD);
LWSTDAPI_(DWORD) SHRegGetPathA(HKEY,LPCSTR,LPCSTR,LPSTR,DWORD);
LWSTDAPI_(DWORD) SHRegGetPathW(HKEY,LPCWSTR,LPCWSTR,LPWSTR,DWORD);
LWSTDAPI_(DWORD) SHRegSetPathA(HKEY,LPCSTR,LPCSTR,LPCSTR,DWORD);
LWSTDAPI_(DWORD) SHRegSetPathW(HKEY,LPCWSTR,LPCWSTR,LPCWSTR,DWORD);
LWSTDAPI_(LONG) SHRegCreateUSKeyA(LPCSTR,REGSAM,HUSKEY,PHUSKEY,DWORD);
LWSTDAPI_(LONG) SHRegCreateUSKeyW(LPCWSTR,REGSAM,HUSKEY,PHUSKEY,DWORD);
LWSTDAPI_(LONG) SHRegOpenUSKeyA(LPCSTR,REGSAM,HUSKEY,PHUSKEY,BOOL);
LWSTDAPI_(LONG) SHRegOpenUSKeyW(LPCWSTR,REGSAM,HUSKEY,PHUSKEY,BOOL);
LWSTDAPI_(LONG) SHRegQueryUSValueA(HUSKEY,LPCSTR,LPDWORD,void*,LPDWORD,BOOL,void*,DWORD);
LWSTDAPI_(LONG) SHRegQueryUSValueW(HUSKEY,LPCWSTR,LPDWORD,void *,LPDWORD,BOOL,void*,DWORD);
LWSTDAPI_(LONG) SHRegWriteUSValueA(HUSKEY,LPCSTR,DWORD,const void*,DWORD,DWORD);
LWSTDAPI_(LONG) SHRegWriteUSValueW(HUSKEY,LPCWSTR,DWORD,const void*,DWORD,DWORD);
LWSTDAPI_(LONG) SHRegDeleteUSValueA(HUSKEY,LPCSTR,SHREGDEL_FLAGS);
LWSTDAPI_(LONG) SHRegDeleteEmptyUSKeyW(HUSKEY,LPCWSTR,SHREGDEL_FLAGS);
LWSTDAPI_(LONG) SHRegDeleteEmptyUSKeyA(HUSKEY,LPCSTR,SHREGDEL_FLAGS);
LWSTDAPI_(LONG) SHRegDeleteUSValueW(HUSKEY,LPCWSTR,SHREGDEL_FLAGS);
LWSTDAPI_(LONG) SHRegEnumUSKeyA(HUSKEY,DWORD,LPSTR,LPDWORD,SHREGENUM_FLAGS);
LWSTDAPI_(LONG) SHRegEnumUSKeyW(HUSKEY,DWORD,LPWSTR,LPDWORD,SHREGENUM_FLAGS);
LWSTDAPI_(LONG) SHRegEnumUSValueA(HUSKEY,DWORD,LPSTR,LPDWORD,LPDWORD,void*,LPDWORD,SHREGENUM_FLAGS);
LWSTDAPI_(LONG) SHRegEnumUSValueW(HUSKEY,DWORD,LPWSTR,LPDWORD,LPDWORD,void*,LPDWORD,SHREGENUM_FLAGS);
LWSTDAPI_(LONG) SHRegQueryInfoUSKeyA(HUSKEY,LPDWORD,LPDWORD,LPDWORD,LPDWORD,SHREGENUM_FLAGS);
LWSTDAPI_(LONG) SHRegQueryInfoUSKeyW(HUSKEY,LPDWORD,LPDWORD,LPDWORD,LPDWORD,SHREGENUM_FLAGS);
LWSTDAPI_(LONG) SHRegCloseUSKey(HUSKEY);
LWSTDAPI_(LONG) SHRegGetUSValueA(LPCSTR,LPCSTR,LPDWORD,void*,LPDWORD,BOOL,void*,DWORD);
LWSTDAPI_(LONG) SHRegGetUSValueW(LPCWSTR,LPCWSTR,LPDWORD,void*,LPDWORD,BOOL,void*,DWORD);
LWSTDAPI_(LONG) SHRegSetUSValueA(LPCSTR,LPCSTR,DWORD,const void*,DWORD,DWORD);
LWSTDAPI_(LONG) SHRegSetUSValueW(LPCWSTR,LPCWSTR,DWORD,const void*,DWORD,DWORD);
LWSTDAPI_(int)  SHRegGetIntW(HKEY,LPCWSTR,int);
LWSTDAPI_(BOOL) SHRegGetBoolUSValueA(LPCSTR,LPCSTR,BOOL,BOOL);
LWSTDAPI_(BOOL) SHRegGetBoolUSValueW(LPCWSTR,LPCWSTR,BOOL,BOOL);
LWSTDAPI AssocCreate(CLSID,REFIID,LPVOID*);
LWSTDAPI AssocQueryStringA(ASSOCF,ASSOCSTR,LPCSTR,LPCSTR,LPSTR,DWORD*);
LWSTDAPI AssocQueryStringW(ASSOCF,ASSOCSTR,LPCWSTR,LPCWSTR,LPWSTR,DWORD*);
LWSTDAPI AssocQueryStringByKeyA(ASSOCF,ASSOCSTR,HKEY,LPCSTR,LPSTR,DWORD*);
LWSTDAPI AssocQueryStringByKeyW(ASSOCF,ASSOCSTR,HKEY,LPCWSTR,LPWSTR,DWORD*);
LWSTDAPI AssocQueryKeyA(ASSOCF,ASSOCKEY,LPCSTR,LPCSTR,HKEY*);
LWSTDAPI AssocQueryKeyW(ASSOCF,ASSOCKEY,LPCWSTR,LPCWSTR,HKEY*);
LWSTDAPI_(struct IStream*) SHOpenRegStreamA(HKEY,LPCSTR,LPCSTR,DWORD);
LWSTDAPI_(struct IStream*) SHOpenRegStreamW(HKEY,LPCWSTR,LPCWSTR,DWORD);
LWSTDAPI_(struct IStream*) SHOpenRegStream2A(HKEY,LPCSTR,LPCSTR,DWORD);
LWSTDAPI_(struct IStream*) SHOpenRegStream2W(HKEY,LPCWSTR,LPCWSTR,DWORD);
LWSTDAPI SHCreateStreamOnFileA(LPCSTR,DWORD,struct IStream**);
LWSTDAPI SHCreateStreamOnFileW(LPCWSTR,DWORD,struct IStream**);
LWSTDAPI_(BOOL) SHCreateThread(LPTHREAD_START_ROUTINE,void*,DWORD,LPTHREAD_START_ROUTINE);
LWSTDAPI_(HPALETTE) SHCreateShellPalette(HDC);
#if (_WIN32_IE >= 0x0500)
LWSTDAPI SHAutoComplete(HWND,DWORD);
LWSTDAPI_(BOOL) SHSkipJunction(struct IBindCtx*,const CLSID*);
LWSTDAPI_(void) ColorRGBToHLS(COLORREF,WORD*,WORD*,WORD*);
LWSTDAPI_(COLORREF) ColorHLSToRGB(WORD,WORD,WORD);
LWSTDAPI_(COLORREF) ColorAdjustLuma(COLORREF,int,BOOL);
STDAPI SHSetThreadRef(IUnknown*);
STDAPI SHGetThreadRef(IUnknown**);
#endif /* _WIN32_IE >= 0x0500 */
STDAPI DllInstall(BOOL,LPCWSTR);

#ifdef UNICODE
#define SZ_CONTENTTYPE_HTML SZ_CONTENTTYPE_HTMLW
#define SZ_CONTENTTYPE_CDF SZ_CONTENTTYPE_CDFW
#define StrRetToStr StrRetToStrW
#define StrRetToBuf StrRetToBufW
#define SHStrDup SHStrDupW
#define StrChr StrChrW
#define StrRChr StrRChrW
#define StrChrI StrChrIW
#define StrRChrI StrRChrIW
#define StrCmpN StrCmpNW
#define StrCmpNI StrCmpNIW
#define StrStr StrStrW
#define StrStrI StrStrIW
#define StrDup StrDupW
#define StrRStrI StrRStrIW
#define StrCSpn StrCSpnW
#define StrCSpnI StrCSpnIW
#define StrSpn StrSpnW
#define StrToInt StrToIntW
#define StrPBrk StrPBrkW
#define StrToIntEx StrToIntExW
#define StrFromTimeInterval StrFromTimeIntervalW
#define StrIntlEqN StrIntlEqNW
#define StrIntlEqNI StrIntlEqNIW
#define StrFormatByteSize StrFormatByteSizeW
#define StrFormatByteSize64 StrFormatByteSizeW
#define StrFormatKBSize StrFormatKBSizeW
#define StrNCat StrNCatW
#define StrTrim StrTrimW
#define StrCatBuff StrCatBuffW
#define ChrCmpI ChrCmpIW
#define wvnsprintf wvnsprintfW
#define wnsprintf wnsprintfW
#define StrIsIntlEqual StrIsIntlEqualW
#define IntlStrEqN IntlStrEqNW
#define IntlStrEqNI IntlStrEqNIW
#define StrCat StrCatW
#define StrCmp StrCmpW
#define StrCmpI StrCmpIW
#define StrCpy StrCpyW
#define StrCpyN StrCpyNW
#define StrCatBuff StrCatBuffW
#define PathAddBackslash PathAddBackslashW
#define PathAddExtension PathAddExtensionW
#define PathBuildRoot PathBuildRootW
#define PathCombine PathCombineW
#define PathFileExists PathFileExistsW
#define PathFindExtension PathFindExtensionW
#define PathFindFileName PathFindFileNameW
#define PathFindNextComponent PathFindNextComponentW
#define PathGetArgs PathGetArgsW
#define PathFindSuffixArray PathFindSuffixArrayW
#define PathIsLFNFileSpec PathIsLFNFileSpecW
#define PathGetDriveNumber PathGetDriveNumberW
#define PathIsDirectory PathIsDirectoryW
#define PathIsDirectoryEmpty PathIsDirectoryEmptyW
#define PathIsFileSpec PathIsFileSpecW
#define PathIsPrefix PathIsPrefixW
#define PathIsRelative PathIsRelativeW
#define PathIsRoot PathIsRootW
#define PathIsSameRoot PathIsSameRootW
#define PathIsUNC PathIsUNCW
#define PathIsNetworkPath PathIsNetworkPathW
#define PathIsUNCServer PathIsUNCServerW
#define PathIsUNCServerShare PathIsUNCServerShareW
#define PathIsURL PathIsURLW
#define PathRemoveBackslash PathRemoveBackslashW
#define PathSkipRoot PathSkipRootW
#define PathStripPath PathStripPathW
#define PathStripToRoot PathStripToRootW
#define PathMakeSystemFolder PathMakeSystemFolderW
#define PathUnmakeSystemFolder PathUnmakeSystemFolderW
#define PathIsSystemFolder PathIsSystemFolderW
#define PathUndecorate PathUndecorateW
#define PathUnExpandEnvStrings PathUnExpandEnvStringsW
#define PathAppend PathAppendW
#define PathCanonicalize PathCanonicalizeW
#define PathCompactPath PathCompactPathW
#define PathCompactPathEx PathCompactPathExW
#define PathCommonPrefix PathCommonPrefixW
#define PathFindOnPath PathFindOnPathW
#define PathGetCharType PathGetCharTypeW
#define PathIsContentType PathIsContentTypeW
#define PathIsHTMLFile PathIsHTMLFileW
#define PathMakePretty PathMakePrettyW
#define PathMatchSpec PathMatchSpecW
#define PathParseIconLocation PathParseIconLocationW
#define PathQuoteSpaces PathQuoteSpacesW
#define PathRelativePathTo PathRelativePathToW
#define PathRemoveArgs PathRemoveArgsW
#define PathRemoveBlanks PathRemoveBlanksW
#define PathRemoveExtension PathRemoveExtensionW
#define PathRemoveFileSpec PathRemoveFileSpecW
#define PathRenameExtension PathRenameExtensionW
#define PathSearchAndQualify PathSearchAndQualifyW
#define PathSetDlgItemPath PathSetDlgItemPathW
#define PathUnquoteSpaces PathUnquoteSpacesW
#define UrlCompare UrlCompareW
#define UrlCombine UrlCombineW
#define UrlCanonicalize UrlCanonicalizeW
#define UrlIsOpaque UrlIsOpaqueW
#define UrlIsFileUrl UrlIsFileUrlW
#define UrlGetLocation UrlGetLocationW
#define UrlUnescape UrlUnescapeW
#define UrlEscape UrlEscapeW
#define UrlCreateFromPath UrlCreateFromPathW
#define PathCreateFromUrl PathCreateFromUrlW
#define UrlHash UrlHashW
#define UrlGetPart UrlGetPartW
#define UrlApplyScheme UrlApplySchemeW
#define UrlIs UrlIsW
#define SHDeleteEmptyKey SHDeleteEmptyKeyW
#define SHDeleteKey SHDeleteKeyW
#define SHDeleteValue SHDeleteValueW
#define SHGetValue SHGetValueW
#define SHSetValue SHSetValueW
#define SHQueryValueEx SHQueryValueExW
#define SHEnumKeyEx SHEnumKeyExW
#define SHEnumValue SHEnumValueW
#define SHQueryInfoKey SHQueryInfoKeyW
#define SHCopyKey SHCopyKeyW
#define SHRegGetPath SHRegGetPathW
#define SHRegSetPath SHRegSetPathW
#define SHRegCreateUSKey SHRegCreateUSKeyW
#define SHRegOpenUSKey SHRegOpenUSKeyW
#define SHRegQueryUSValue SHRegQueryUSValueW
#define SHRegWriteUSValue SHRegWriteUSValueW
#define SHRegDeleteUSValue SHRegDeleteUSValueW
#define SHRegDeleteEmptyUSKey SHRegDeleteEmptyUSKeyW
#define SHRegEnumUSKey SHRegEnumUSKeyW
#define SHRegEnumUSValue SHRegEnumUSValueW
#define SHRegQueryInfoUSKey SHRegQueryInfoUSKeyW
#define SHRegGetUSValue SHRegGetUSValueW
#define SHRegSetUSValue SHRegSetUSValueW
#define SHRegGetInt SHRegGetIntW
#define SHRegGetBoolUSValue SHRegGetBoolUSValueW
#define AssocQueryString AssocQueryStringW
#define AssocQueryStringByKey AssocQueryStringByKeyW
#define AssocQueryKey AssocQueryKeyW
#define SHOpenRegStream SHOpenRegStreamW
#define SHOpenRegStream2 SHOpenRegStream2W
#define SHCreateStreamOnFile SHCreateStreamOnFileW
#else /* !UNICODE */
#define SZ_CONTENTTYPE_HTML SZ_CONTENTTYPE_HTMLA
#define SZ_CONTENTTYPE_CDF SZ_CONTENTTYPE_CDFA
#define StrRetToStr StrRetToStrA
#define StrRetToBuf StrRetToBufA
#define SHStrDup SHStrDupA
#define StrChr StrChrA
#define StrRChr StrRChrA
#define StrChrI StrChrIA
#define StrRChrI StrRChrIA
#define StrCmpN StrCmpNA
#define StrCmpNI StrCmpNIA
#define StrStr StrStrA
#define StrStrI StrStrIA
#define StrDup StrDupA
#define StrRStrI StrRStrIA
#define StrCSpn StrCSpnA
#define StrCSpnI StrCSpnIA
#define StrSpn StrSpnA
#define StrToInt StrToIntA
#define StrPBrk StrPBrkA
#define StrToIntEx StrToIntExA
#define StrFromTimeInterval StrFromTimeIntervalA
#define StrIntlEqN StrIntlEqNA
#define StrIntlEqNI StrIntlEqNIA
#define StrFormatByteSize StrFormatByteSizeA
#define StrFormatByteSize64 StrFormatByteSize64A
#define StrFormatKBSize StrFormatKBSizeA
#define StrNCat StrNCatA
#define StrTrim StrTrimA
#define StrCatBuff StrCatBuffA
#define ChrCmpI ChrCmpIA
#define wvnsprintf wvnsprintfA
#define wnsprintf wnsprintfA
#define StrIsIntlEqual StrIsIntlEqualA
#define IntlStrEqN IntlStrEqNA
#define IntlStrEqNI IntlStrEqNIA
#define StrCat lstrcatA
#define StrCmp lstrcmpA
#define StrCmpI lstrcmpiA
#define StrCpy lstrcpyA
#define StrCpyN lstrcpynA
#define StrCatBuff StrCatBuffA
#define PathAddBackslash PathAddBackslashA
#define PathAddExtension PathAddExtensionA
#define PathBuildRoot PathBuildRootA
#define PathCombine PathCombineA
#define PathFileExists PathFileExistsA
#define PathFindExtension PathFindExtensionA
#define PathFindFileName PathFindFileNameA
#define PathFindNextComponent PathFindNextComponentA
#define PathGetArgs PathGetArgsA
#define PathFindSuffixArray PathFindSuffixArrayA
#define PathIsLFNFileSpec PathIsLFNFileSpecA
#define PathGetDriveNumber PathGetDriveNumberA
#define PathIsDirectory PathIsDirectoryA
#define PathIsDirectoryEmpty PathIsDirectoryEmptyA
#define PathIsFileSpec PathIsFileSpecA
#define PathIsPrefix PathIsPrefixA
#define PathIsRelative PathIsRelativeA
#define PathIsRoot PathIsRootA
#define PathIsSameRoot PathIsSameRootA
#define PathIsUNC PathIsUNCA
#define PathIsNetworkPath PathIsNetworkPathA
#define PathIsUNCServer PathIsUNCServerA
#define PathIsUNCServerShare PathIsUNCServerShareA
#define PathIsURL PathIsURLA
#define PathRemoveBackslash PathRemoveBackslashA
#define PathSkipRoot PathSkipRootA
#define PathStripPath PathStripPathA
#define PathStripToRoot PathStripToRootA
#define PathMakeSystemFolder PathMakeSystemFolderA
#define PathUnmakeSystemFolder PathUnmakeSystemFolderA
#define PathIsSystemFolder PathIsSystemFolderA
#define PathUndecorate PathUndecorateA
#define PathUnExpandEnvStrings PathUnExpandEnvStringsA
#define PathAppend PathAppendA
#define PathCanonicalize PathCanonicalizeA
#define PathCompactPath PathCompactPathA
#define PathCompactPathEx PathCompactPathExA
#define PathCommonPrefix PathCommonPrefixA
#define PathFindOnPath PathFindOnPathA
#define PathGetCharType PathGetCharTypeA
#define PathIsContentType PathIsContentTypeA
#define PathIsHTMLFile PathIsHTMLFileA
#define PathMakePretty PathMakePrettyA
#define PathMatchSpec PathMatchSpecA
#define PathParseIconLocation PathParseIconLocationA
#define PathQuoteSpaces PathQuoteSpacesA
#define PathRelativePathTo PathRelativePathToA
#define PathRemoveArgs PathRemoveArgsA
#define PathRemoveBlanks PathRemoveBlanksA
#define PathRemoveExtension PathRemoveExtensionA
#define PathRemoveFileSpec PathRemoveFileSpecA
#define PathRenameExtension PathRenameExtensionA
#define PathSearchAndQualify PathSearchAndQualifyA
#define PathSetDlgItemPath PathSetDlgItemPathA
#define PathUnquoteSpaces PathUnquoteSpacesA
#define UrlCompare UrlCompareA
#define UrlCombine UrlCombineA
#define UrlCanonicalize UrlCanonicalizeA
#define UrlIsOpaque UrlIsOpaqueA
#define UrlIsFileUrl UrlIsFileUrlA
#define UrlGetLocation UrlGetLocationA
#define UrlUnescape UrlUnescapeA
#define UrlEscape UrlEscapeA
#define UrlCreateFromPath UrlCreateFromPathA
#define PathCreateFromUrl PathCreateFromUrlA
#define UrlHash UrlHashA
#define UrlGetPart UrlGetPartA
#define UrlApplyScheme UrlApplySchemeA
#define UrlIs UrlIsA
#define SHDeleteEmptyKey SHDeleteEmptyKeyA
#define SHDeleteKey SHDeleteKeyA
#define SHDeleteValue SHDeleteValueA
#define SHGetValue SHGetValueA
#define SHSetValue SHSetValueA
#define SHQueryValueEx SHQueryValueExA
#define SHEnumKeyEx SHEnumKeyExA
#define SHEnumValue SHEnumValueA
#define SHQueryInfoKey SHQueryInfoKeyA
#define SHCopyKey SHCopyKeyA
#define SHRegGetPath SHRegGetPathA
#define SHRegSetPath SHRegSetPathA
#define SHRegCreateUSKey SHRegCreateUSKeyA
#define SHRegOpenUSKey SHRegOpenUSKeyA
#define SHRegQueryUSValue SHRegQueryUSValueA
#define SHRegWriteUSValue SHRegWriteUSValueA
#define SHRegDeleteUSValue SHRegDeleteUSValueA
#define SHRegDeleteEmptyUSKey SHRegDeleteEmptyUSKeyA
#define SHRegEnumUSKey SHRegEnumUSKeyA
#define SHRegEnumUSValue SHRegEnumUSValueA
#define SHRegQueryInfoUSKey SHRegQueryInfoUSKeyA
#define SHRegGetUSValue SHRegGetUSValueA
#define SHRegSetUSValue SHRegSetUSValueA
#define SHRegGetBoolUSValue SHRegGetBoolUSValueA
#define AssocQueryString AssocQueryStringA
#define AssocQueryStringByKey AssocQueryStringByKeyA
#define AssocQueryKey AssocQueryKeyA
#define SHOpenRegStream SHOpenRegStreamA
#define SHOpenRegStream2 SHOpenRegStream2A
#define SHCreateStreamOnFile SHCreateStreamOnFileA
#endif /* !UNICODE */

#undef SHOpenRegStream
#define SHOpenRegStream SHOpenRegStream2

#undef INTERFACE
#define INTERFACE IQueryAssociations
DECLARE_INTERFACE_(IQueryAssociations,IUnknown)
{
    STDMETHOD (QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD (Init)(THIS_ ASSOCF,LPCWSTR,HKEY,HWND) PURE;
    STDMETHOD (GetString)(THIS_ ASSOCF,ASSOCSTR,LPCWSTR,LPWSTR,DWORD*) PURE;
    STDMETHOD (GetKey)(THIS_ ASSOCF,ASSOCKEY,LPCWSTR,HKEY*) PURE;
    STDMETHOD (GetData)(THIS_ ASSOCF,ASSOCDATA,LPCWSTR,LPVOID,DWORD*) PURE;
    STDMETHOD (GetEnum)(THIS_ ASSOCF,ASSOCENUM,LPCWSTR,REFIID,LPVOID*) PURE;
};
#define CLSID_QueryAssociations IID_IQueryAssociations

#ifdef __cplusplus
}
#endif

#include <poppack.h>

#endif /* _SHLWAPI_H */

