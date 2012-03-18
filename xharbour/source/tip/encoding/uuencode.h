/*
 * $Id$
 */

#ifndef _uuencode_h_
#define _uuencode_h_
#include <string.h>
#include <malloc.h>

#ifdef __cplusplus
extern "C"{
#endif

char *UUEncode( const unsigned char *pInput, unsigned int iInputLen );
unsigned char *UUDecode( const char *pszInput, unsigned int *pOutLen );

#ifdef __cplusplus
}
#endif

#endif
