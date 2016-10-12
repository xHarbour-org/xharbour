#ifndef _TCHAR_H
#define _TCHAR_H

/* tchar.h - private header for generic text functions */

#define _T(x)  __T(x)
#define _TEXT(x)  __T(x)

#ifdef _UNICODE

/* Map generic names to Unicode functions */

#include <wchar.h>

#define _TEOF  WEOF
#define __T(x)  L##x

typedef wchar_t _TCHAR;
typedef wint_t _TINT;

/* entry points */
#define _tmain  wmain
#define _tWinMain  wWinMain
#define __targv  __wargv

/* formatted input/output functions */
#define _ftprintf  fwprintf
#define _ftscanf  fwscanf
#define _stprintf  swprintf
#define _stscanf  swscanf
#define _vftprintf  vfwprintf
#define _vftscanf  vfwscanf
#define _vstprintf  vswprintf
#define _vstscanf  vswscanf
#define _vtprintf  vwprintf
#define _vtscanf  vwscanf
#define _tprintf  wprintf
#define _tscanf  wscanf

/* input/output functions */
#define _fgettc  fgetwc
#define _fgetts  fgetws
#define _fputtc  fputwc
#define _fputts  fputws
#define _gettc  getwc
#define _gettchar  getwchar
#define _puttc  putwc
#define _puttchar  putwchar
#define _ungettc  ungetwc

/* general utilities */
#define _tcstod  wcstod
#define _tcstof  wcstof
#define _tcstold  wcstold
#define _tcstol  wcstol
#define _tcstoll  wcstoll
#define _tcstoul  wcstoul
#define _tcstoull  wcstoull
#define _tcscpy  wcscpy
#define _tcsncpy  wcsncpy
#define _tcscat  wcscat
#define _tcsncat  wcsncat
#define _tcscmp  wcscmp
#define _tcscoll  wcscoll
#define _tcsncmp  wcsncmp
#define _tcsxfrm  wcsxfrm
#define _tcschr  wcschr
#define _tcscspn  wcscspn
#define _tcslen  wcslen
#define _tcspbrk  wcspbrk
#define _tcsrchr  wcsrchr
#define _tcsspn  wcsspn
#define _tcsstr  wcsstr
#define _tcstok  wcstok
#define _tmemchr  wmemchr
#define _tmemcmp  wmemcmp
#define _tmemcpy  wmemcpy
#define _tmemmove  wmemmove
#define _tmemset  wmemset

/* time conversion functions */
#define _tcsftime  wcsftime

/* conversion functions for greatest-width integer types */
#define _tcstoimax  wcstoimax
#define _tcstoumax  wcstoumax

/* character classification */
#define _istalnum  iswalnum
#define _istalpha  iswalpha
#define _istcntrl  iswcntrl
#define _istdigit  iswdigit
#define _istgraph  iswgraph
#define _istlower  iswlower
#define _istprint  iswprint
#define _istpunct  iswpunct
#define _istspace  iswspace
#define _istblank  iswblank
#define _istupper  iswupper
#define _istxdigit  iswxdigit
#define _totlower  towlower
#define _totupper  towupper

/* private extensions to standard C */
#define _tcsicmp  _wcsicmp
#define _tcsnicmp  _wcsnicmp

#else  /* _UNICODE */

/* Map generic names to string functions */

#include <string.h>

#define _TEOF  EOF
#define __T(x)  x

typedef char  _TCHAR;
typedef int  _TINT;

/* entry points */
#define _tmain  main
#define _tWinMain  WinMain
#define __targv  __argv

/* formatted input/output functions */
#define _ftprintf  fprintf
#define _ftscanf  fscanf
#define _stprintf  snprintf  /* 03-07-27: sprintf -> snprintf */
#define _stscanf  sscanf
#define _vftprintf  vfprintf
#define _vftscanf  vfscanf
#define _vstprintf  vsprintf
#define _vstscanf  vsscanf
#define _vtprintf  vprintf
#define _vtscanf  vscanf
#define _tprintf  printf
#define _tscanf  scanf

/* input/output functions */
#define _fgettc  fgetc
#define _fgetts  fgets
#define _fputtc  fputc
#define _fputts  fputs
#define _gettc  getc
#define _gettchar  getchar
#define _puttc  putc
#define _puttchar  putchar
#define _ungettc  ungetc

/* general utilities */
#define _tcstod  strtod
#define _tcstof  strtof
#define _tcstold  strtold
#define _tcstol  strtol
#define _tcstoll  strtoll
#define _tcstoul  strtoul
#define _tcstoull  strtoull
#define _tcscpy  strcpy
#define _tcsncpy  strncpy
#define _tcscat  strcat
#define _tcsncat  strncat
#define _tcscmp  strcmp
#define _tcscoll  strcoll
#define _tcsncmp  strncmp
#define _tcsxfrm  strxfrm
#define _tcschr  strchr
#define _tcscspn  strcspn
#define _tcslen  strlen
#define _tcspbrk  strpbrk
#define _tcsrchr  strrchr
#define _tcsspn  strspn
#define _tcsstr  strstr
#define _tcstok  strtok
#define _tmemchr  memchr
#define _tmemcmp  memcmp
#define _tmemcpy  memcpy
#define _tmemmove  memmove
#define _tmemset  memset

/* time conversion functions */
#define _tcsftime  strftime

/* conversion functions for greatest-width integer types */
#define _tcstoimax  strtoimax
#define _tcstoumax  strtoumax

/* character classification */
#define _istalnum  isalnum
#define _istalpha  isalpha
#define _istcntrl  iscntrl
#define _istdigit  isdigit
#define _istgraph  isgraph
#define _istlower  islower
#define _istprint  isprint
#define _istpunct  ispunct
#define _istspace  isspace
#define _istblank  isblank
#define _istupper  isupper
#define _istxdigit  isxdigit
#define _totlower  tolower
#define _totupper  toupper

/* private extensions to standard C */
#define _tcsicmp  _stricmp
#define _tcsnicmp  _strnicmp

#endif  /* _UNICODE */

#endif  /* _TCHAR_H */
