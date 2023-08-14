#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 475
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif

#ifndef _SHTYPES_H
#define _SHTYPES_H

#include "wtypes.h"

#ifdef __cplusplus
extern "C"{
#endif 

void * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free(void *); 

#include <pshpack1.h>
typedef struct _SHITEMID {
    USHORT cb;
    BYTE abID[1];
} SHITEMID;
#include <poppack.h>

#if defined(_M_IX86) || defined(_M_ARM)
#define __unaligned
#endif
typedef SHITEMID __unaligned *LPSHITEMID;
typedef const SHITEMID __unaligned *LPCSHITEMID;

#include <pshpack1.h>
typedef struct _ITEMIDLIST {
    SHITEMID mkid;
} ITEMIDLIST;
#include <poppack.h>

typedef BYTE_BLOB *wirePIDL;
typedef ITEMIDLIST __unaligned *LPITEMIDLIST;
typedef const ITEMIDLIST __unaligned *LPCITEMIDLIST;

typedef struct _WINBASE_DEFINED_STRUCT {
    int dummy;
} WINBASE_DEFINED_STRUCT;

#ifdef WINBASE_DEFINED_MIDL
typedef WINBASE_DEFINED_STRUCT WIN32_FIND_DATAA;
typedef WINBASE_DEFINED_STRUCT WIN32_FIND_DATAW;
#endif

typedef enum tagSTRRET_TYPE {
    STRRET_WSTR = 0,
    STRRET_OFFSET = 0x1,
    STRRET_CSTR = 0x2
} STRRET_TYPE;

#include <pshpack8.h>
typedef struct _STRRET {
    UINT uType;
    union {
        LPWSTR pOleStr;
        UINT uOffset;
        char cStr[260];
    } DUMMYUNIONNAME;
} STRRET, *LPSTRRET;
#include <poppack.h>

#include <pshpack1.h>
typedef struct _SHELLDETAILS {
    int fmt;
    int cxChar;
    STRRET str;
} SHELLDETAILS, *LPSHELLDETAILS;
#include <poppack.h>

extern RPC_IF_HANDLE __MIDL_itf_shtypes_0000_v0_0_c_ifspec;
extern RPC_IF_HANDLE __MIDL_itf_shtypes_0000_v0_0_s_ifspec;

#ifdef __cplusplus
}
#endif

#endif /* _SHTYPES_H */

