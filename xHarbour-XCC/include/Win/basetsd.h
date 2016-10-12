#ifndef _BASETSD_H
#define _BASETSD_H

/* Windows Basic sized type definitions (01-05-29) */

#ifdef __cplusplus
extern "C" {
#endif

typedef signed char INT8, *PINT8;
typedef signed short INT16, *PINT16;
typedef signed int INT32, *PINT32;
typedef signed __int64 INT64, *PINT64;
typedef unsigned char UINT8, *PUINT8;
typedef unsigned short UINT16, *PUINT16;
typedef unsigned int UINT32, *PUINT32;
typedef unsigned __int64 UINT64, *PUINT64;

typedef int LONG32, *PLONG32;

typedef unsigned int ULONG32, *PULONG32;
typedef unsigned int DWORD32, *PDWORD32;

#if !defined(_W64)
#define _W64
#endif

#ifdef _WIN64

typedef __int64 INT_PTR, *PINT_PTR;
typedef unsigned __int64 UINT_PTR, *PUINT_PTR;
typedef __int64 LONG_PTR, *PLONG_PTR;
typedef unsigned __int64 ULONG_PTR, *PULONG_PTR;
typedef unsigned __int64 HANDLE_PTR;
typedef unsigned int UHALF_PTR, *PUHALF_PTR;
typedef int HALF_PTR, *PHALF_PTR;
#define __int3264 __int64

#define ADDRESS_TAG_BIT  0x40000000000ULL

__inline unsigned long HandleToUlong(const void *h)  { return (unsigned long)h; }
__inline long HandleToLong(const void *h)  { return (long)h; }
__inline void *ULongToHandle(const unsigned long ul)  { return((void *)(UINT_PTR)ul ); }
__inline void *LongToHandle(const long h)  { return (void *)(INT_PTR)h; }
__inline unsigned long PtrToUlong(const void *p)  { return (unsigned long)p; }
__inline unsigned int PtrToUint(const void *p)  { return (unsigned int)p; }
__inline unsigned short PtrToUshort(const void *p)  { return (unsigned short)p; }
__inline long PtrToLong(const void *p)  { return (long)p; }
__inline int PtrToInt(const void *p)  { return (int)p; }
__inline short PtrToShort(const void *p)  { return (short)p; }
__inline void *IntToPtr(const int i)  { return (void *)(INT_PTR)i; }
__inline void *UIntToPtr(const unsigned int ui)  { return (void *)(UINT_PTR)ui; }
__inline void *LongToPtr(const long l)  { return (void *)(LONG_PTR)l; }
__inline void *ULongToPtr(const unsigned long ul)  { return (void *)(ULONG_PTR)ul; }

#else /* _WIN64 */

typedef _W64 int INT_PTR, *PINT_PTR;
typedef _W64 unsigned int UINT_PTR, *PUINT_PTR;
typedef _W64 long LONG_PTR, *PLONG_PTR;
typedef _W64 unsigned long ULONG_PTR, *PULONG_PTR;
typedef unsigned short UHALF_PTR, *PUHALF_PTR;
typedef short HALF_PTR, *PHALF_PTR;
typedef _W64 unsigned long HANDLE_PTR;
#define __int3264 __int32

#define ADDRESS_TAG_BIT  0x80000000UL

#define HandleToULong(h)  ((ULONG)(ULONG_PTR)(h))
#define HandleToLong(h)  ((LONG)(LONG_PTR)(h))
#define ULongToHandle(ul)  ((HANDLE)(ULONG_PTR)(ul))
#define LongToHandle(h)  ((HANDLE)(LONG_PTR)(h))
#define PtrToUlong(p)  ((ULONG)(ULONG_PTR)(p))
#define PtrToLong(p)  ((LONG)(LONG_PTR)(p))
#define PtrToUint(p)  ((UINT)(UINT_PTR)(p))
#define PtrToInt(p)  ((INT)(INT_PTR)(p))
#define PtrToUshort(p)  ((unsigned short)(ULONG_PTR)(p))
#define PtrToShort(p)  ((short)(LONG_PTR)(p))
#define IntToPtr(i)  ((void*)(INT_PTR)((int)i))
#define UIntToPtr(ui)  ((void*)(UINT_PTR)((unsigned int)ui))
#define LongToPtr(l)  ((void*)(LONG_PTR)((long)l))
#define ULongToPtr(ul)  ((void*)(ULONG_PTR)((unsigned long)ul))

#endif /* _WIN64 */

#define HandleToUlong(h)  HandleToULong(h)
#define UlongToHandle(ul)  ULongToHandle(ul)
#define UlongToPtr(ul)  ULongToPtr(ul)
#define UintToPtr(ui)  UIntToPtr(ui)

#define MAXUINT_PTR  (~((UINT_PTR)0))
#define MAXINT_PTR  ((INT_PTR)(MAXUINT_PTR>>1))
#define MININT_PTR  (~MAXINT_PTR)

#define MAXULONG_PTR  (~((ULONG_PTR)0))
#define MAXLONG_PTR  ((LONG_PTR)(MAXULONG_PTR>>1))
#define MINLONG_PTR  (~MAXLONG_PTR)

#define MAXUHALF_PTR  ((UHALF_PTR)~0)
#define MAXHALF_PTR  ((HALF_PTR)(MAXUHALF_PTR>>1))
#define MINHALF_PTR  (~MAXHALF_PTR)

typedef ULONG_PTR SIZE_T, *PSIZE_T;
typedef LONG_PTR SSIZE_T, *PSSIZE_T;

typedef ULONG_PTR DWORD_PTR, *PDWORD_PTR;

typedef __int64 LONG64, *PLONG64;

typedef unsigned __int64 ULONG64, *PULONG64;
typedef unsigned __int64 DWORD64, *PDWORD64;

typedef ULONG_PTR KAFFINITY;
typedef KAFFINITY *PKAFFINITY;

#ifdef __cplusplus
}
#endif

#endif /* _BASETSD_H */

