#include <string.h>
#include <limits.h>
#include "malloc.h"
#include "hbapi.h"


/*
 * base64enc()
 *
 * Encode the data in 's' (length of the data is 'len') in BASE64. The returned
 * string should be freed when not used anymore.
 */

char *base64enc(char *s, size_t s_len)
{
    char b64chars[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    char *t;
    char *p;
    int x, y;
    int len;

    if (s_len > (size_t)INT_MAX)
    {
	   return NULL ; //die("data too long in base64enc()");
    }
    len = (int)s_len;
    t = (char*) hb_xgrab((4 * ((len + 2) / 3) + 1) * sizeof(char));
    p = t;
    while (len-- > 0)
    {
	x = *s++;
	*p++ = b64chars[(x >> 2) & 63];
	if (len-- <= 0)
	{
	    *p++ = b64chars[(x << 4) & 63];
    	    *p++ = '=';
	    *p++ = '=';
    	    break;
	}
	y = *s++;
	*p++ = b64chars[((x << 4) | ((y >> 4) & 15)) & 63];
	if (len-- <= 0)
	{
	    *p++ = b64chars[(y << 2) & 63];
	    *p++ = '=';
	    break;
	}
	x = *s++;
	*p++ = b64chars[((y << 2) | ((x >> 6) & 3)) & 63];
	*p++ = b64chars[x & 63];
    }
    *p = '\0';

    return t;
}

HB_FUNC(BUILDUSERPASSSTRING)
{
 char * s;
 char * szUser = hb_parcx(1);
 char * szPass = hb_parcx(2);
 size_t p_len= strlen(szPass);
 size_t u_len= strlen(szUser);
 s = (char * ) hb_xgrab((u_len + p_len + 3) * sizeof(char));
 s[0] = '\0';
 strcpy(s + 1, szUser);
 strcpy(s + u_len + 2, szPass);
 hb_retcAdopt( s);

}
HB_FUNC( HB_BASE64)
{
char * szItem = hb_parcx(1);
int nLen= hb_parni(2);
char * szRet =  base64enc ( szItem,nLen);
hb_retcAdopt( szRet);
//hb_xfree( szRet) ;
}
