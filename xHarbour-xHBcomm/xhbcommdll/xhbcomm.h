#ifndef __XHBCOMM_H__
#define __XHBCOMM_H__
#include "windows.h"
#ifdef __cplusplus
extern "C"  {
#endif

#if defined (HBCOMM_HAS_DLL)
#    if defined (HBCOMM_BUILD_DLL)
#      define HBCOMM_API __declspec (dllexport)
#    else
#      define HBCOMM_API  __declspec (dllimport)
#    endif /* ZIP_BUILD_DLL */
#else
#  define HBCOMM_API
#endif     /* ZIP_HAS_DLL */


HBCOMM_API void * Tcomm_New( void );

HBCOMM_API void Tcomm_SetComPort( void * port, char * szPort);
HBCOMM_API void Tcomm_SetBaudRate( void * port, unsigned int newBaud );


HBCOMM_API void Tcomm_SetParity( void * port, BYTE newParity) ;


HBCOMM_API void Tcomm_SetByteSize( void * port, BYTE newByte) ;

HBCOMM_API BOOL Tcomm_SetStopBits( void * port, BYTE newStop) ;
HBCOMM_API BOOL Tcomm_WriteString( void * port,const char *outString) ;

HBCOMM_API BOOL Tcomm_WriteBuffer( void * port,BYTE  *buffer, unsigned int ByteCount) ;

HBCOMM_API void Tcomm_WriteBufferSlowly( void * port,BYTE  *buffer, unsigned int ByteCount) ;

HBCOMM_API int Tcomm_ReadString( void * port,char *string, unsigned int MaxBytes) ;

HBCOMM_API int Tcomm_ReadBytes( void * port,BYTE *bytes, unsigned int byteCount) ;

HBCOMM_API void Tcomm_DiscardBytes( void * port,unsigned int MaxBytes) ;

HBCOMM_API BOOL Tcomm_PurgeCommPort( void * port) ;

HBCOMM_API BOOL Tcomm_PurgeOCommPort( void * port) ;

HBCOMM_API void Tcomm_FlushCommPort(void * port) ;

HBCOMM_API void Tcomm_PutByte(void * port,BYTE value ) ;

HBCOMM_API unsigned int Tcomm_BytesAvailable(void * port) ;

HBCOMM_API unsigned int Tcomm_BytesOAvailable(void * port) ;

HBCOMM_API BOOL Tcomm_GetConnected(void * port) ;


HBCOMM_API BOOL Tcomm_OpenCommPort(void * port) ;
HBCOMM_API void Tcomm_CloseCommPort(void * port) ;
HBCOMM_API BOOL Tcomm_OpenPort(void * port) ;
HBCOMM_API BOOL Tcomm_ConfigDialog( void * Port );
HBCOMM_API BOOL Tcomm_SetCommOptions(void * Port,int iOption );
HBCOMM_API BOOL Tcomm_SetHandShake(void * port ,int  d) ;
HBCOMM_API BOOL Tcomm_GetModemStatus(void * port ,DWORD *ModemStatus) ;
HBCOMM_API BOOL Tcomm_SetDtr( void * port, unsigned int b);
#ifdef __cplusplus
}
#endif

#endif
