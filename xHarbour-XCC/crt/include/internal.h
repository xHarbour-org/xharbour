#ifndef _INTERNAL_H
#define _INTERNAL_H

/* internal.h - private header */

#if defined(_CRTDLL_)
#define _CRTAPI  __declspec(dllexport)
#elif defined(__DLL__)
#define _CRTAPI  __declspec(dllimport)
#else
#define _CRTAPI
#endif

#endif /* _INTERNAL_H */

