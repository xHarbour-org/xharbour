#ifndef _uuencode_h_
#define _uuencode_h_
#include <string.h>
#include <malloc.h>

#ifdef __cplusplus
extern "C"{
#endif

char *UUEncode(unsigned char *pInput, unsigned int iInputLen);
unsigned char *UUDecode(char *pszInput, unsigned int *pOutLen);

#ifdef __cplusplus
}
#endif

#endif

