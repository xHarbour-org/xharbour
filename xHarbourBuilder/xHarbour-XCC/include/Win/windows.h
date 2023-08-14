#ifndef _WINDOWS_H
#define _WINDOWS_H

/* Windows API master include file */

#if defined(__i386__) && !defined(_M_IX86)
#define _M_IX86  300
#endif
#if !defined(_X86_) && defined(_M_IX86)
#define _X86_  _M_IX86
#endif
#if !defined(i386) && defined(_M_IX86)
#define i386
#endif

#if !defined(_ARM_) && defined(_M_ARM)
#define _ARM_  _M_ARM
#endif

#ifdef RC_INVOKED
/* winresrc.h includes the necessary headers */
#include <winresrc.h>
#else /* RC_INVOKED */

#ifdef __POCC__
/* Pelles C support nameless unions and structs */
#ifndef NONAMELESSUNION
#define _ANONYMOUS_UNION
#endif
#ifndef NONAMELESSSTRUCT
#define _ANONYMOUS_STRUCT
#endif
#elif defined(_MSC_VER)
/* MSVC support nameless unions and structs */
#ifndef NONAMELESSUNION
#define _ANONYMOUS_UNION
#endif
#ifndef NONAMELESSSTRUCT
#define _ANONYMOUS_STRUCT
#endif
#endif /* _MSC_VER */

#ifndef _ANONYMOUS_UNION
#define _UNION_NAME(x)  x
#define DUMMYUNIONNAME  u
#define DUMMYUNIONNAME2  u2
#define DUMMYUNIONNAME3  u3
#define DUMMYUNIONNAME4  u4
#define DUMMYUNIONNAME5  u5
#else
#define _UNION_NAME(x)
#define DUMMYUNIONNAME
#define DUMMYUNIONNAME2
#define DUMMYUNIONNAME3
#define DUMMYUNIONNAME4
#define DUMMYUNIONNAME5
#endif
#ifndef _ANONYMOUS_STRUCT
#define _STRUCT_NAME(x)  x
#define DUMMYSTRUCTNAME  s
#define DUMMYSTRUCTNAME2  s2
#define DUMMYSTRUCTNAME3  s3
#else
#define _STRUCT_NAME(x)
#define DUMMYSTRUCTNAME
#define DUMMYSTRUCTNAME2
#define DUMMYSTRUCTNAME3
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2121)  /* Redeclaration of 'type' */
#endif

#include <excpt.h>
#include <stdarg.h>

#include <windef.h>
#include <winbase.h>
#include <wingdi.h>
#include <winuser.h>
#include <winnls.h>
#include <wincon.h>
#include <winver.h>
#include <winreg.h>
#include <winnetwk.h>
#ifndef WIN32_LEAN_AND_MEAN
#include <cderr.h>
#include <dde.h>
#include <ddeml.h>
#include <dlgs.h>
#include <lzexpand.h>
#include <mmsystem.h>
#include <nb30.h>
#include <rpc.h>
#include <shellapi.h>
#include <winperf.h>
#if (_WIN32_WINNT >= 0x0400)
#include <winsock2.h>
#include <mswsock.h>
#else
#include <winsock.h>
#endif
#ifndef NOCRYPT
#include <wincrypt.h>
#include <winefs.h>
#include <winscard.h>
#endif
#include <winspool.h>
#include <ole2.h>
#include <commdlg.h>
#ifndef NOSERVICE
#include <winsvc.h>
#endif
#ifndef NOMCX
#include <mcx.h>
#endif
#ifndef NOIME
#include <imm.h>
#endif
#endif /* WIN32_LEAN_AND_MEAN */

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#endif /* RC_INVOKED */

#endif /* _WINDOWS_H */
