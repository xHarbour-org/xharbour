/*
 * $Id: base64.h 9448 2012-04-21 14:45:40Z patrickmast $
 */

#ifndef _base64_h_
#define _base64_h_
#include <string.h>
#ifdef HB_THREAD_SUPPORT
#include "thread.h"
#endif

#ifdef __cplusplus    
extern "C"{
#endif

char *Base64Encode(const unsigned char *pcCode, unsigned int uCodeLen);
unsigned char *Base64Decode(const char *pcszInput, unsigned int *puOutLen);

#ifdef __cplusplus
}
#endif

#endif
