/*
* $Id: gtwvw.c 10017 2014-10-07 04:14:54Z ronpinkas $
 */

/*
 * New header to define CINTERFACE and genericly workaround missing
 * incomplete CINTERFACE support in Windows SDK's guiddef.h and propkeydef.h
 *
 * Copyright 2014 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.harbour-project.org
 *
 */

#ifndef CINTERFACE_H_
#define CINTERFACE_H_

#if !defined( NTDDI_VERSION ) && !defined( _WIN32_WINNT )
   #define NTDDI_VERSION 0x05010000
   #define _WIN32_WINNT 0x0501 
#endif

#ifndef _WIN32_IE
#define _WIN32_IE 0x0501
#endif

#if defined( _MSC_VER ) && _MSC_VER >= 1700
   #define _USING_V110_SDK71_
#endif

#if defined( __MINGW32__ )
   #include <initguid.h>
#endif

/*
Circumvent guiddef.h and propkeydef.h lacking support for CINTERFACE
Which makes it impossible to compile C style OLE code
in CPP mode
*/

#if defined( __cplusplus )

#define CINTERFACE 1

// Save
#ifdef __MIDL_CONST
   #define _PRESET___MIDL_CONST __MIDL_CONST 
   #undef __MIDL_CONST
#endif
#define __MIDL_CONST

/*
Clearly the subsequent:
#if defined( __cplusplus ) && !defined( CINTERFACE )
can NEVER yield TRUE, because of the outer defined( CINTERFACE) condition.
Obviously those #if conditions could be commented or ommited but are kept
to reflect what the original corrected PSDK's guiddef.h and propkeydef.h 
would have been, and make it easier to keep it in synch with the official
PSDK code.
*/

// guiddef.h
#ifndef _REFGUID_DEFINED
#define _REFGUID_DEFINED
#if defined( __cplusplus ) && !defined( CINTERFACE )
#define REFGUID const GUID &
#else
#define REFGUID const GUID * __MIDL_CONST
#endif
#endif

#ifndef _REFIID_DEFINED
#define _REFIID_DEFINED
#if defined( __cplusplus ) && !defined( CINTERFACE )
#define REFIID const IID &
#else
#define REFIID const IID * __MIDL_CONST
#endif
#endif

#ifndef _REFCLSID_DEFINED
#define _REFCLSID_DEFINED
#if defined( __cplusplus ) && !defined( CINTERFACE )
#define REFCLSID const IID &
#else
#define REFCLSID const IID * __MIDL_CONST
#endif
#endif

#ifndef _REFFMTID_DEFINED
#define _REFFMTID_DEFINED
#if defined( __cplusplus ) && !defined( CINTERFACE )
#define REFFMTID const IID &
#else
#define REFFMTID const IID * __MIDL_CONST
#endif
#endif

#define _NO_SYS_GUID_OPERATOR_EQ_
// End guiddef.h

//propkeydef.h
#ifndef REFPROPERTYKEY
#if defined( __cplusplus ) && !defined( CINTERFACE )
#define REFPROPERTYKEY const PROPERTYKEY &
#else // !__cplusplus
#define REFPROPERTYKEY const PROPERTYKEY * __MIDL_CONST
#endif // __cplusplus
#endif //REFPROPERTYKEY

#ifdef DEFINE_PROPERTYKEY
#undef DEFINE_PROPERTYKEY
#endif

#ifdef INITGUID
#define DEFINE_PROPERTYKEY(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8, pid) EXTERN_C const PROPERTYKEY DECLSPEC_SELECTANY name = { { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }, pid }
#else
#define DEFINE_PROPERTYKEY(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8, pid) EXTERN_C const PROPERTYKEY name
#endif // INITGUID

#ifndef IsEqualPropertyKey
#define IsEqualPropertyKey(a, b)   (((a).pid == (b).pid) && IsEqualIID(&((a).fmtid), &((b).fmtid)) )
#endif  // IsEqualPropertyKey

#ifndef _PROPERTYKEY_EQUALITY_OPERATORS_
#define _PROPERTYKEY_EQUALITY_OPERATORS_
#endif // _PROPERTYKEY_EQUALITY_OPERATORS_
// End propkeydef.h

// Restore
#undef __MIDL_CONST
#ifdef _PRESET___MIDL_CONST
   #define __MIDL_CONST _PRESET___MIDL_CONST 
#endif

#endif // defined( __cplusplus) && defined( CINTERFACE)

#endif  // HB_OLE_H_
