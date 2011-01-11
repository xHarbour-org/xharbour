/* assert.h - C99 standard header */

#undef assert

#ifdef NDEBUG
#define assert(exp)  ((void)0)
#else
#define assert(exp)  ((exp) ? (void)0 : __assert(#exp, __FILE__, __LINE__, __func__))
void __cdecl __assert(char *, char *, int, const char *);
#endif /* NDEBUG */

