/* jconfig.bcc --- jconfig.h for Borland C (Turbo C) on MS-DOS or OS/2. */
/* see jconfig.doc for explanations */

#if defined(__WATCOMC__)
   #pragma disable_message ( 201 )
   #pragma disable_message ( 136 )
#elif defined( __POCC__ )
   #pragma warn(push)
   #pragma warn(disable:2154)
#elif defined(__BORLANDC__)
   #pragma warn -prc
   #pragma warn -pia
   #pragma warn -rch
   #pragma warn -csu
   #pragma warn -aus
   #pragma warn -ccc
   #pragma warn -def
   #pragma warn -sig
   #pragma warn -sus
   #pragma warn -use
   #pragma warn -eff
   #pragma warn -par
#endif

#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #define _CRT_SECURE_NO_WARNINGS
   #define _CRT_SECURE_NO_DEPRECATE
#endif

#define HAVE_PROTOTYPES
#define HAVE_UNSIGNED_CHAR
#define HAVE_UNSIGNED_SHORT
/* #define void char */
/* #define const */
#undef CHAR_IS_UNSIGNED
#define HAVE_STDDEF_H
#define HAVE_STDLIB_H
#undef NEED_BSD_STRINGS
#undef NEED_SYS_TYPES_H
#ifdef __MSDOS__
#define NEED_FAR_POINTERS	/* for small or medium memory model */
#endif
#undef NEED_SHORT_EXTERNAL_NAMES
#undef INCOMPLETE_TYPES_BROKEN	/* this assumes you have -w-stu in CFLAGS */

#ifdef JPEG_INTERNALS

#undef RIGHT_SHIFT_IS_UNSIGNED

#ifdef __MSDOS__
#define USE_MSDOS_MEMMGR	/* Define this if you use jmemdos.c */
#define MAX_ALLOC_CHUNK 65520L	/* Maximum request to malloc() */
#define USE_FMEM		/* Borland has _fmemcpy() and _fmemset() */
#endif

#endif /* JPEG_INTERNALS */

#ifdef JPEG_CJPEG_DJPEG

#define BMP_SUPPORTED		/* BMP image file format */
#define GIF_SUPPORTED		/* GIF image file format */
#define PPM_SUPPORTED		/* PBMPLUS PPM/PGM image file format */
#undef RLE_SUPPORTED		/* Utah RLE image file format */
#define TARGA_SUPPORTED		/* Targa image file format */

#define TWO_FILE_COMMANDLINE
#define USE_SETMODE		/* Borland has setmode() */
#ifdef __MSDOS__
#define NEED_SIGNAL_CATCHER	/* Define this if you use jmemdos.c */
#endif
#undef DONT_USE_B_MODE
#undef PROGRESS_REPORT		/* optional */

#endif /* JPEG_CJPEG_DJPEG */
