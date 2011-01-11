#ifndef _SPORDER_H
#define _SPORDER_H

/* WinSock2 transport service provider reorder definitions */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef WSPAPI
#define WSPAPI __stdcall
#endif

typedef int (WSPAPI *LPWSCWRITEPROVIDERORDER)(LPDWORD,DWORD);
typedef int (WSPAPI *LPWSCWRITENAMESPACEORDER)(LPGUID,DWORD);

int WSPAPI WSCWriteProviderOrder(LPDWORD,DWORD);
int WSPAPI WSCWriteNameSpaceOrder(LPGUID,DWORD);

#ifdef __cplusplus
}
#endif

#endif /* _SPORDER_H */
