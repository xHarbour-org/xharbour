#ifndef _MSSIP_H
#define _MSSIP_H

/* Microsoft SIP Provider definitions */

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2198)  /* Nameless field is not standard */
#endif

#define MSSIP_FLAGS_PROHIBIT_RESIZE_ON_CREATE  0x00010000
#define MSSIP_FLAGS_USE_CATALOG  0x00020000

#define SPC_INC_PE_RESOURCES_FLAG  0x80
#define SPC_INC_PE_DEBUG_INFO_FLAG  0x40
#define SPC_INC_PE_IMPORT_ADDR_TABLE_FLAG  0x20

#define MSSIP_ADDINFO_NONE  0
#define MSSIP_ADDINFO_FLAT  1
#define MSSIP_ADDINFO_CATMEMBER  2
#define MSSIP_ADDINFO_BLOB  3
#define MSSIP_ADDINFO_NONMSSIP  500

#define SIP_MAX_MAGIC_NUMBER  4

#include <pshpack8.h>

typedef CRYPT_HASH_BLOB CRYPT_DIGEST_DATA;

typedef struct SIP_SUBJECTINFO_ {
    DWORD cbSize;
    GUID *pgSubjectType;
    HANDLE hFile;
    LPCWSTR pwsFileName;
    LPCWSTR pwsDisplayName;
    DWORD dwReserved1;
    DWORD dwIntVersion;
    HCRYPTPROV hProv;
    CRYPT_ALGORITHM_IDENTIFIER DigestAlgorithm;
    DWORD dwFlags;
    DWORD dwEncodingType;
    DWORD dwReserved2;
    DWORD fdwCAPISettings;
    DWORD fdwSecuritySettings;
    DWORD dwIndex;
    DWORD dwUnionChoice;
    union {
        struct MS_ADDINFO_FLAT_ *psFlat;
        struct MS_ADDINFO_CATALOGMEMBER_ *psCatMember;
        struct MS_ADDINFO_BLOB_ *psBlob;
    };
    LPVOID pClientData;
} SIP_SUBJECTINFO, *LPSIP_SUBJECTINFO;

typedef struct MS_ADDINFO_FLAT_ {
    DWORD cbStruct;
    struct SIP_INDIRECT_DATA_ *pIndirectData;
} MS_ADDINFO_FLAT, *PMS_ADDINFO_FLAT;

typedef struct MS_ADDINFO_CATALOGMEMBER_ {
    DWORD cbStruct;
    struct CRYPTCATSTORE_ *pStore;
    struct CRYPTCATMEMBER_ *pMember;
} MS_ADDINFO_CATALOGMEMBER, *PMS_ADDINFO_CATALOGMEMBER;

typedef struct MS_ADDINFO_BLOB_ {
    DWORD cbStruct;
    DWORD cbMemObject;
    BYTE *pbMemObject;
    DWORD cbMemSignedMsg;
    BYTE *pbMemSignedMsg;
} MS_ADDINFO_BLOB, *PMS_ADDINFO_BLOB;

typedef struct SIP_INDIRECT_DATA_ {
    CRYPT_ATTRIBUTE_TYPE_VALUE Data;
    CRYPT_ALGORITHM_IDENTIFIER DigestAlgorithm;
    CRYPT_HASH_BLOB Digest;
} SIP_INDIRECT_DATA, *PSIP_INDIRECT_DATA;

typedef struct SIP_ADD_NEWPROVIDER_ {
    DWORD cbStruct;
    GUID *pgSubject;
    WCHAR *pwszDLLFileName;
    WCHAR *pwszMagicNumber;
    WCHAR *pwszIsFunctionName;
    WCHAR *pwszGetFuncName;
    WCHAR *pwszPutFuncName;
    WCHAR *pwszCreateFuncName;
    WCHAR *pwszVerifyFuncName;
    WCHAR *pwszRemoveFuncName;
    WCHAR *pwszIsFunctionNameFmt2;
} SIP_ADD_NEWPROVIDER, *PSIP_ADD_NEWPROVIDER;

#include <poppack.h>

typedef BOOL (WINAPI *pCryptSIPGetSignedDataMsg)(SIP_SUBJECTINFO*,DWORD*,DWORD,DWORD*,BYTE*);
typedef BOOL (WINAPI *pCryptSIPPutSignedDataMsg)(SIP_SUBJECTINFO*,DWORD,DWORD*,DWORD,BYTE*);
typedef BOOL (WINAPI *pCryptSIPCreateIndirectData)(SIP_SUBJECTINFO*,DWORD*,SIP_INDIRECT_DATA*);
typedef BOOL (WINAPI *pCryptSIPVerifyIndirectData)(SIP_SUBJECTINFO*,SIP_INDIRECT_DATA*);
typedef BOOL (WINAPI *pCryptSIPRemoveSignedDataMsg)(SIP_SUBJECTINFO*,DWORD);
typedef BOOL (WINAPI *pfnIsFileSupported)(HANDLE,GUID*);
typedef BOOL (WINAPI *pfnIsFileSupportedName)(WCHAR*,GUID*);

typedef struct SIP_DISPATCH_INFO_ {
    DWORD cbSize;
    HANDLE hSIP;
    pCryptSIPGetSignedDataMsg pfGet;
    pCryptSIPPutSignedDataMsg pfPut;
    pCryptSIPCreateIndirectData pfCreate;
    pCryptSIPVerifyIndirectData pfVerify;
    pCryptSIPRemoveSignedDataMsg pfRemove;
} SIP_DISPATCH_INFO, *LPSIP_DISPATCH_INFO;

extern BOOL WINAPI CryptSIPGetSignedDataMsg(SIP_SUBJECTINFO*,DWORD*,DWORD,DWORD*,BYTE*);
extern BOOL WINAPI CryptSIPPutSignedDataMsg(SIP_SUBJECTINFO*,DWORD,DWORD*,DWORD,BYTE*);
extern BOOL WINAPI CryptSIPCreateIndirectData(SIP_SUBJECTINFO*,DWORD*,SIP_INDIRECT_DATA*);
extern BOOL WINAPI CryptSIPVerifyIndirectData(SIP_SUBJECTINFO*,SIP_INDIRECT_DATA*);
extern BOOL WINAPI CryptSIPRemoveSignedDataMsg(SIP_SUBJECTINFO*,DWORD);
extern BOOL WINAPI CryptSIPLoad(const GUID*,DWORD,SIP_DISPATCH_INFO*);
extern BOOL WINAPI CryptSIPRetrieveSubjectGuid(LPCWSTR,HANDLE,GUID*);
extern BOOL WINAPI CryptSIPRetrieveSubjectGuidForCatalogFile(LPCWSTR,HANDLE,GUID*);
extern BOOL WINAPI CryptSIPAddProvider(SIP_ADD_NEWPROVIDER*);
extern BOOL WINAPI CryptSIPRemoveProvider(GUID*);

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
}
#endif

#endif /* _MSSIP_H */
