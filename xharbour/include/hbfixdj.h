/*
 * $Id: hbfixdj.h,v 1.0 Exp $
 */


#ifdef __dj_include_inline_ctype_ha_

#undef isalnum(c)
#undef isalpha(c)
#undef iscntrl(c)
#undef isdigit(c)
#undef isgraph(c)
#undef islower(c)
#undef isprint(c)
#undef ispunct(c)
#undef isspace(c)
#undef isupper(c)
#undef isxdigit(c)

#undef tolower(c)
#undef toupper(c)

#define isalnum(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISALNUM)
#define isalpha(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISALPHA)
#define iscntrl(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISCNTRL)
#define isdigit(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISDIGIT)
#define isgraph(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISGRAPH)
#define islower(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISLOWER)
#define isprint(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISPRINT)
#define ispunct(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISPUNCT)
#define isspace(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISSPACE)
#define isupper(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISUPPER)
#define isxdigit(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISXDIGIT)

#define tolower(c) (__dj_ctype_tolower[(unsigned char)(c)+1])
#define toupper(c) (__dj_ctype_toupper[(unsigned char)(c)+1])

#endif /* __dj_include_inline_ctype_ha_ */
