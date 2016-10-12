/****************************************************************************
 *                                                                          *
 * File    : rc.h                                                           *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; constants and definitions.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           98-03-22  Added some new typedef's; changed WCHAR to wchar_t.  *
 *           98-07-01  UNREFERENCED_PARAMETER macro added.                  *
 *           98-11-08  HTML token added.                                    *
 *           00-01-22  Changed to generic resource.h                        *
 *           00-01-23  PLUGPLAY, VXD tokens added.                          *
 *           00-02-12  C_LBRACE and C_RBRACE added.                         *
 *           01-05-24  MANIFEST token added.                                *
 *           01-11-09  Compiler converted to dynalink library.              *
 *           01-11-23  C_PLUS added.                                        *
 *           02-02-27  Added support for DIALOGEX resources.                *
 *           03-08-02  C_BORLAND added.                                     *
 *                                                                          *
 ****************************************************************************/

#ifndef _RC_H
#define _RC_H

#ifdef __cplusplus
extern "C" {
#endif

#include "resource.h"
#include "resdll.h"

/* Irritating warnings in level 4: */
/* warning C4200: nonstandard extension used : zero-sized array in struct/union */
/* warning C4201: nonstandard extension used : nameless struct/union */
/* warning C4244: '=' : conversion from 'xxx ' to 'xxx ', possible loss of data */
#pragma warning(disable: 4200 4201 4244)

enum rctoken
{
    C_NOTHING = 0,
    C_SYMBOL,
    C_STRING,
    C_UNISTRING,
    C_NUMBER,
    C_OR,
    C_PLUS,
    C_EOF,
    C_LBRACE,
    C_RBRACE,
    C_ACCELERATORS,
    C_ALT,
    C_ANICURSOR,
    C_ANIICON,
    C_ASCII,
    C_AUTO3STATE,
    C_AUTOCHECKBOX,
    C_AUTORADIOBUTTON,
    C_BEGIN,
    C_BITMAP,
    C_BLOCK,
    C_CAPTION,
    C_CHARACTERISTICS,
    C_CHECKBOX,
    C_CHECKED,
    C_CLASS,
    C_COMBOBOX,
    C_CONTROL,
    C_CTEXT,
    C_CURSOR,
    C_DEFPUSHBUTTON,
    C_DIALOG,
    C_DIALOGEX,
    C_DISCARDABLE,
    C_DLGINCLUDE,
    C_EDITTEXT,
    C_END,
    C_EXSTYLE,
    C_FILEFLAGS,
    C_FILEFLAGSMASK,
    C_FILEOS,
    C_FILESUBTYPE,
    C_FILETYPE,
    C_FILEVERSION,
    C_FIXED,
    C_FONT,
    C_GRAYED,
    C_GROUPBOX,
    C_HELP,
    C_HTML,
    C_ICON,
    C_IMPURE,
    C_INACTIVE,
    C_LANGUAGE,
    C_LISTBOX,
    C_LOADONCALL,
    C_LTEXT,
    C_MANIFEST,
    C_MENU,
    C_MENUBARBREAK,
    C_MENUBREAK,
    C_MENUEX,
    C_MENUITEM,
    C_MESSAGETABLE,
    C_MOVEABLE,
    C_NOINVERT,
    C_NOT,
    C_PLUGPLAY,
    C_POPUP,
    C_PRELOAD,
    C_PRODUCTVERSION,
    C_PURE,
    C_PUSHBOX,
    C_PUSHBUTTON,
    C_RADIOBUTTON,
    C_RCDATA,
    C_RTEXT,
    C_SCROLLBAR,
    C_SEPARATOR,
    C_SHIFT,
    C_STATE3,
    C_STRINGTABLE,
    C_STYLE,
    C_VALUE,
    C_VERSION,
    C_VERSIONINFO,
    C_VIRTKEY,
    C_VXD,
    C_LINESYNC,
    C_BORLAND
};

/* Current symbol information */
typedef struct _SYMBOL
{
    enum rctoken tok;               /* token (number, string, keyword ...) */
    int nchars;                     /* number of chars */
    bool_t is_long;                 /* true if long value ('nnL') */
    union {
        long num;                   /* numeric value */
        char str[4096];             /* string value, identifier etc */
    }; /* MS ext */
} SYMBOL;

/* Scanner state information */
typedef struct _SCANNER
{
    SYMBOL sym;                     /* current symbol */
    int nchars;                     /* number of chars in buffer */
    char *ip;                       /* current input ptr */
    char ch;                        /* "push-back" character */
    char filename[MAX_PATH];        /* current file */
    int line;                       /* current line in file */
} SCANNER;

/* Dialog box information */
typedef struct _DIALOGHEADER
{
    struct _CONTROLENTRY *next;     /* ptr to first control in dialog */
    bool_t extended;                /* extended dialog? */
    ulong_t exstyle;                /* dialog extended style */
    ulong_t style;                  /* dialog style */
    short x;                        /* dialog left coordinate */
    short y;                        /* dialog top coordinate */
    short width;                    /* dialog width */
    short height;                   /* dialog height */
    long helpid;                    /* extended dialog help identifier */
    struct {
        wchar_t *typeface;          /* typeface name */
        short ptsize;               /* font size in points */
        short weight;               /* extended dialog font weight */
        char italic;                /* extended dialog uses italic font? */
        char charset;               /* extended dialog charset */
    } font;
    wchar_t *menu;                  /* optional menu */
    wchar_t *class;                 /* optional class name */
    wchar_t *caption;               /* dialog caption */
} DIALOGHEADER;

/* Dialog box control information */
typedef struct _CONTROLENTRY
{
    struct _CONTROLENTRY *next;     /* ptr to next control in dialog */
    ulong_t exstyle;                /* control extended style */
    ulong_t style;                  /* control style */
    short x;                        /* control left coordinate */
    short y;                        /* control top coordinate */
    short width;                    /* control width */
    short height;                   /* control height */
    short id;                       /* control identifier */
    long helpid;                    /* extended control help identifier */
    wchar_t class[80];              /* class name (normally an ordinal, hence the static size) */
    wchar_t *caption;               /* control caption */
} CONTROLENTRY;

/* Accelerator table */
typedef struct _ACCELTABLE
{
    int num;                        /* number of array entries */
    int max;                        /* max number of array entries (before relocation) */
    RSRC_ACCENT **entries;          /* dynamic array of accelerator entries */
} ACCELTABLE;

/* Menu or extended menu description */
typedef struct _MENUTREE
{
    ushort_t flags;
    short id;                       /* identifier */
    ulong_t type;                   /* extended menu type */
    ulong_t state;                  /* extended menu state */
    long helpid;                    /* extended menu help identifier */
    int num;                        /* number of menu entries */
    int max;                        /* max number of menu entries (before relocation) */
    struct _MENUTREE **items;       /* dynamic array of menu items */
    wchar_t title[80];              /* menu text */
} MENUTREE;

/* Entry in stringtable */
typedef struct _STRINGENTRY
{
    RSRC_HDR_TAIL tail;
    int block;                      /* block number */
    int pos;                        /* position within block */
    wchar_t *string;                /* string value */
} STRINGENTRY;

/* Stringtable */
typedef struct _STRINGTABLE
{
    int num;                        /* number of array entries */
    int max;                        /* max number of array entries (before relocation) */
    STRINGENTRY **strings;          /* dynamic array of string entries */
} STRINGTABLE;

/* Entry in version "StringFileInfo" block */
typedef struct _VERSTRINGENTRY
{
    wchar_t *key;                   /* key string value */
    wchar_t *val;                   /* value string value */
} VERSTRINGENTRY;

/* Version "StringFileInfo" language block */
typedef struct _VERSTRINGTABLE
{
    struct _VERSTRINGTABLE *next;   /* ptr to next language block */
    wchar_t lang_charset[9];        /* language of this block */
    int num;                        /* number of array entries */
    int max;                        /* max number of array entries (before relocation) */
    VERSTRINGENTRY **strings;       /* dynamic array of version string entries */
} VERSTRINGTABLE;

/* Version "VarFileInfo" entries */
typedef struct _VERVARTABLE
{
    wchar_t *key;                   /* key string value (i.e. "Translation") */
    int num;                        /* number of array entries */
    int max;                        /* max number of array entries (before relocation) */
    ulong_t *val;                   /* dynamic array of value entries */
} VERVARTABLE;

/* Icon file header */
typedef struct _ICOHDR
{
    char width;                     /* width of icon image */
    char height;                    /* height of icon image */
    short ncolors;                  /* number of colors */
    short nplanes;                  /* number of planes */
    short nbitspixel;               /* number of bits per pixel */
    long ressize;                   /* size of image */
    long offset;                    /* offset to image */
} ICOHDR;

/* Cursor file header */
typedef struct _CURHDR
{
    char width;                     /* width of cursor image */
    char height;                    /* height of cursor image */
    short ncolors;                  /* number of colors */
    struct {                        /* hotspot coordinates */
        short x;
        short y;
    } hotspot;
    long ressize;                   /* size of image */
    long offset;                    /* offset to image */
} CURHDR;

/* IDE information (RC files) */
typedef struct _FILEINFO
{
    struct _FILEINFO *next;         /* ptr to next node */
    wchar_t *type;                  /* resource type */
    wchar_t *name;                  /* resource name */
    ushort_t language;              /* resource language */
    char *filename;                 /* file name */
} FILEINFO;

/* IDE information (#include files) */
typedef struct _INCLINFO
{
    struct _INCLINFO *next;         /* ptr to next node */
    char *filename;                 /* file name */
} INCLINFO;

/* Matches GetLastError() type */
typedef DWORD WINERR;

//Ron Pinkas commented #define UNREFERENCED_PARAMETER(P)  (P)

#define SWAP(type,a,b)  { type t; t = (a); (a) = (b); (b) = t; }
#define NELEMS(arr)     (sizeof(arr) / sizeof(arr[0]))

#include "cpp.h"
#include "msg.h"

#ifndef APPLICATION_ERROR_MASK
#define APPLICATION_ERROR_MASK        0x20000000
#define ERROR_SEVERITY_SUCCESS        0x00000000
#define ERROR_SEVERITY_INFORMATIONAL  0x40000000
#define ERROR_SEVERITY_WARNING        0x80000000
#define ERROR_SEVERITY_ERROR          0xC0000000
#endif

/* Error definitions */
#define ERROR_SEVERITY_FATAL     0x8000U
#define ERROR_SEVERITY_WARNING2  0x4000U
#define ERROR_SEVERITY_WARNING1  0x0000U

#define RCFATAL(err)    (ERROR_SEVERITY_FATAL|(err))
#define RCERROR(err)    (ERROR_SEVERITY_ERROR|(err))
#define RCWARNING(err)  (ERROR_SEVERITY_WARNING|(err))

#define ISFATAL(err)    (((err) & ERROR_SEVERITY_FATAL) ? 1 : 0)
#define ISERROR(err)    (((err) & 0xC0000000) == ERROR_SEVERITY_ERROR)
#define ISWARNING(err)  (((err) & 0xC0000000) == ERROR_SEVERITY_WARNING)

/* Support for multiple warning levels */
#define RCWARNING1(err) (ERROR_SEVERITY_WARNING|ERROR_SEVERITY_WARNING1|(err))
#define RCWARNING2(err) (ERROR_SEVERITY_WARNING|ERROR_SEVERITY_WARNING2|(err))

#define WARNLEVEL(err)  (((err) & ERROR_SEVERITY_WARNING2) ? 2 : 1)

#define MYOPENERROR(err) \
    ((((err) & APPLICATION_ERROR_MASK) == 0 && \
     (((err) & 0x00007FFF) == ERROR_FILE_NOT_FOUND || \
      ((err) & 0x00007FFF) == ERROR_PATH_NOT_FOUND)) ? \
     (((err) & 0xFFFF8000)|ERROR_FILE_NOT_FOUND2) : (err))

/* Private WINERR error codes */
#define ERROR_FILE_NOT_FOUND2           (APPLICATION_ERROR_MASK|MSG_FILE_NOT_FOUND)
#define ERROR_READ_FILE                 (APPLICATION_ERROR_MASK|MSG_READ_ERROR)
#define ERROR_WRITE_FILE                (APPLICATION_ERROR_MASK|MSG_WRITE_ERROR)

#define ERROR_INVALID_CODEPAGE          (APPLICATION_ERROR_MASK|MSG_INVALID_CODEPAGE)

#define ERROR_NOT_IMPLEMENTED           (APPLICATION_ERROR_MASK|MSG_NOT_IMPLEMENTED)
#define ERROR_INVALID_CONTROL_KEY       (APPLICATION_ERROR_MASK|MSG_INVALID_CONTROL_KEY)
#define ERROR_ACCEL_TYPE_NEEDED         (APPLICATION_ERROR_MASK|MSG_ACCEL_TYPE_NEEDED)
#define ERROR_INVALID_SYNTAX            (APPLICATION_ERROR_MASK|MSG_INVALID_SYNTAX)
#define ERROR_PLUS_NOT_ALLOWED          (APPLICATION_ERROR_MASK|MSG_PLUS_NOT_ALLOWED)

#define ERROR_DUPLICATE_STRING_ID       (APPLICATION_ERROR_MASK|MSG_DUPLICATE_STRING_ID)
#define ERROR_INVALID_FILE              (APPLICATION_ERROR_MASK|MSG_INVALID_FILE)

#define ERROR_NUMBER_LIMIT_EXCEEDED     (APPLICATION_ERROR_MASK|MSG_NUMBER_LIMIT_EXCEEDED)
#define ERROR_UNKNOWN_KEYWORD           (APPLICATION_ERROR_MASK|MSG_UNKNOWN_KEYWORD)
#define ERROR_NUMBER_EXPECTED           (APPLICATION_ERROR_MASK|MSG_NUMBER_EXPECTED)
#define ERROR_STRING_EXPECTED           (APPLICATION_ERROR_MASK|MSG_STRING_EXPECTED)
#define ERROR_BEGIN_EXPECTED            (APPLICATION_ERROR_MASK|MSG_BEGIN_EXPECTED)
#define ERROR_UNEXPECTED_EOF            (APPLICATION_ERROR_MASK|MSG_UNEXPECTED_EOF)

#define ERROR_UNTERM_INCLUDE_COND       (APPLICATION_ERROR_MASK|MSG_PP_UNTERM_INCLUDE_COND)
#define ERROR_UNTERM_IF_DIRECT          (APPLICATION_ERROR_MASK|MSG_PP_UNTERM_IF_DIRECT)
#define ERROR_INVALID_CONTROL_LINE      (APPLICATION_ERROR_MASK|MSG_PP_INVALID_CONTROL_LINE)
#define ERROR_UNKNOWN_PP_CONTROL        (APPLICATION_ERROR_MASK|MSG_PP_UNKNOWN_PP_CONTROL)
#define ERROR_IF_TOO_DEEPLY_NESTED      (APPLICATION_ERROR_MASK|MSG_PP_IF_TOO_DEEPLY_NESTED)
#define ERROR_UNDEF_SYNTAX_ERROR        (APPLICATION_ERROR_MASK|MSG_PP_UNDEF_SYNTAX_ERROR)
#define ERROR_ELIF_WITHOUT_IF           (APPLICATION_ERROR_MASK|MSG_PP_ELIF_WITHOUT_IF)
#define ERROR_ELIF_AFTER_ELSE           (APPLICATION_ERROR_MASK|MSG_PP_ELIF_AFTER_ELSE)
#define ERROR_ELSE_WITHOUT_IF           (APPLICATION_ERROR_MASK|MSG_PP_ELSE_WITHOUT_IF)
#define ERROR_ELSE_AFTER_ELSE           (APPLICATION_ERROR_MASK|MSG_PP_ELSE_AFTER_ELSE)
#define ERROR_ELSE_SYNTAX_ERROR         (APPLICATION_ERROR_MASK|MSG_PP_ELSE_SYNTAX_ERROR)
#define ERROR_ENDIF_WITHOUT_IF          (APPLICATION_ERROR_MASK|MSG_PP_ENDIF_WITHOUT_IF)
#define ERROR_ENDIF_SYNTAX_ERROR        (APPLICATION_ERROR_MASK|MSG_PP_ENDIF_SYNTAX_ERROR)
#define ERROR_ERROR_DIRECT              (APPLICATION_ERROR_MASK|MSG_PP_ERROR_DIRECT)
#define ERROR_LINE_SYNTAX_ERROR         (APPLICATION_ERROR_MASK|MSG_PP_LINE_SYNTAX_ERROR)
#define ERROR_LINE_NUMBER_OUT_OF_RANGE  (APPLICATION_ERROR_MASK|MSG_PP_LINE_NUMBER_OUT_OF_RANGE)
#define ERROR_CONTROL_SYNTAX_ERROR      (APPLICATION_ERROR_MASK|MSG_PP_CONTROL_SYNTAX_ERROR)
#define ERROR_CONTROL_NOT_IMPLEMENTED   (APPLICATION_ERROR_MASK|MSG_PP_CONTROL_NOT_IMPLEMENTED)

#define ERROR_IF_SYNTAX_ERROR           (APPLICATION_ERROR_MASK|MSG_PP_IF_SYNTAX_ERROR)
#define ERROR_INVALID_OPERATOR_IN_IF    (APPLICATION_ERROR_MASK|MSG_PP_INVALID_OPERATOR_IN_IF)
#define ERROR_BAD_OPERATOR_IN_IF        (APPLICATION_ERROR_MASK|MSG_PP_BAD_OPERATOR_IN_IF)
#define ERROR_BOTCH_IN_IF               (APPLICATION_ERROR_MASK|MSG_PP_BOTCH_IN_IF)
#define ERROR_UNDEF_EXPRESSION_VALUE    (APPLICATION_ERROR_MASK|MSG_PP_UNDEF_EXPRESSION_VALUE)
#define ERROR_BAD_COND_OPERATOR_IN_IF   (APPLICATION_ERROR_MASK|MSG_PP_BAD_COND_OPERATOR_IN_IF)
#define ERROR_EVAL_BOTCH                (APPLICATION_ERROR_MASK|MSG_PP_EVAL_BOTCH)
#define ERROR_BAD_DIGIT_IN_NUMBER       (APPLICATION_ERROR_MASK|MSG_PP_BAD_DIGIT_IN_NUMBER)
#define ERROR_BAD_NUMBER_IN_IF          (APPLICATION_ERROR_MASK|MSG_PP_BAD_NUMBER_IN_IF)
#define ERROR_WIDE_CHAR_VALUE_UNDEF     (APPLICATION_ERROR_MASK|MSG_PP_WIDE_CHAR_VALUE_UNDEF)
#define ERROR_UNDEF_ESC_IN_CHAR_CONST   (APPLICATION_ERROR_MASK|MSG_PP_UNDEF_ESC_IN_CHAR_CONST)
#define ERROR_EMPTY_CHAR_CONST          (APPLICATION_ERROR_MASK|MSG_PP_EMPTY_CHAR_CONST)
#define ERROR_MULTIBYTE_VALUE_UNDEF     (APPLICATION_ERROR_MASK|MSG_PP_MULTIBYTE_VALUE_UNDEF)
#define ERROR_SIGNED_CHAR_CONST         (APPLICATION_ERROR_MASK|MSG_PP_SIGNED_CHAR_CONST)
#define ERROR_STRING_IN_IF              (APPLICATION_ERROR_MASK|MSG_PP_STRING_IN_IF)

#define ERROR_INCLUDE_TOO_DEEPLY_NESTED (APPLICATION_ERROR_MASK|MSG_PP_INCLUDE_TOO_DEEPLY_NESTED)
#define ERROR_INCLUDE_FILE_NOT_FOUND    (APPLICATION_ERROR_MASK|MSG_PP_INCLUDE_FILE_NOT_FOUND)
#define ERROR_INCLUDE_SYNTAX_ERROR      (APPLICATION_ERROR_MASK|MSG_PP_INCLUDE_SYNTAX_ERROR)

#define ERROR_LEXICAL_BOTCH             (APPLICATION_ERROR_MASK|MSG_PP_LEXICAL_BOTCH)
#define ERROR_NO_NEWLINE_AT_EOF         (APPLICATION_ERROR_MASK|MSG_PP_NO_NEWLINE_AT_EOF)
#define ERROR_UNTERM_STRING_OR_CHAR     (APPLICATION_ERROR_MASK|MSG_PP_UNTERM_STRING_OR_CHAR)
#define ERROR_EOF_IN_STRING_OR_CHAR     (APPLICATION_ERROR_MASK|MSG_PP_EOF_IN_STRING_OR_CHAR)
#define ERROR_EOF_INSIDE_COMMENT        (APPLICATION_ERROR_MASK|MSG_PP_EOF_INSIDE_COMMENT)
#define ERROR_INPUT_BUFFER_OVERFLOW     (APPLICATION_ERROR_MASK|MSG_PP_INPUT_BUFFER_OVERFLOW)

#define ERROR_DEF_TOKEN_IS_NOT_A_NAME   (APPLICATION_ERROR_MASK|MSG_PP_DEF_TOKEN_IS_NOT_A_NAME)
#define ERROR_DEF_TOKEN_CANT_BE_REDEF   (APPLICATION_ERROR_MASK|MSG_PP_DEF_TOKEN_CANT_BE_REDEF)
#define ERROR_DUP_MACRO_ARGUMENT        (APPLICATION_ERROR_MASK|MSG_PP_DUP_MACRO_ARGUMENT)
#define ERROR_MACRO_ARGS_SYNTAX_ERROR   (APPLICATION_ERROR_MASK|MSG_PP_MACRO_ARGS_SYNTAX_ERROR)
#define ERROR_MACRO_REDEFINITION        (APPLICATION_ERROR_MASK|MSG_PP_MACRO_REDEFINITION)
#define ERROR_INVALID_D_OR_U_ARG        (APPLICATION_ERROR_MASK|MSG_PP_INVALID_D_OR_U_ARG)
#define ERROR_BAD_DEFINED_SYNTAX        (APPLICATION_ERROR_MASK|MSG_PP_BAD_DEFINED_SYNTAX)
#define ERROR_MACRO_ARG_DISAGREEMENT    (APPLICATION_ERROR_MASK|MSG_PP_MACRO_ARG_DISAGREEMENT)
#define ERROR_EOF_IN_MACRO_ARGLIST      (APPLICATION_ERROR_MASK|MSG_PP_EOF_IN_MACRO_ARGLIST)
#define ERROR_TOO_MANY_MACRO_ARGS       (APPLICATION_ERROR_MASK|MSG_PP_TOO_MANY_MACRO_ARGS)
#define ERROR_SHARP_WITHOUT_MACRO_ARG   (APPLICATION_ERROR_MASK|MSG_PP_SHARP_WITHOUT_MACRO_ARG)
#define ERROR_DSHARP_AT_BORDER          (APPLICATION_ERROR_MASK|MSG_PP_DSHARP_AT_BORDER)
#define ERROR_BAD_TOKEN_FROM_DSHARP     (APPLICATION_ERROR_MASK|MSG_PP_BAD_TOKEN_FROM_DSHARP)
#define ERROR_STR_MACRO_ARG_TOO_LONG    (APPLICATION_ERROR_MASK|MSG_PP_STR_MACRO_ARG_TOO_LONG)
#define ERROR_INTERNAL_MACRO_BOTCH      (APPLICATION_ERROR_MASK|MSG_PP_INTERNAL_MACRO_BOTCH)

#define ERROR_CANT_OPEN_INPUT_FILE      (APPLICATION_ERROR_MASK|MSG_PP_CANT_OPEN_INPUT_FILE)
#define ERROR_CANT_OPEN_OUTPUT_FILE     (APPLICATION_ERROR_MASK|MSG_PP_CANT_OPEN_OUTPUT_FILE)

#define ERROR_INTERNAL                  (APPLICATION_ERROR_MASK|MSG_INTERNAL_ERROR)

/* dllmain.c */
void apperror(WINERR, ...);
int sprintmsg(char *, int, ...);

/* rcompile.c */
void res_start(const char *);
void res_close(void);
WINERR write_stringtable(STRINGTABLE *);
WINERR named_accelerators(const char *, RSRC_HDR_TAIL *, ACCELTABLE *);
WINERR numbered_accelerators(wchar_t, RSRC_HDR_TAIL *, ACCELTABLE *);
WINERR named_anicursor(const char *, const char *, ushort_t);
WINERR numbered_anicursor(wchar_t, const char *, ushort_t);
WINERR named_aniicon(const char *, const char *, ushort_t);
WINERR numbered_aniicon(wchar_t, const char *, ushort_t);
WINERR named_bitmap(const char *, const char *, ushort_t);
WINERR numbered_bitmap(wchar_t, const char *, ushort_t);
WINERR named_literal_bitmap(const char *, uchar_t *, size_t, ushort_t);
WINERR numbered_literal_bitmap(wchar_t, uchar_t *, size_t, ushort_t);
WINERR named_cursor(const char *, const char *, ushort_t);
WINERR numbered_cursor(wchar_t, const char *, ushort_t);
WINERR named_literal_cursor(const char *, uchar_t *, size_t, ushort_t);
WINERR numbered_literal_cursor(wchar_t, uchar_t *, size_t, ushort_t);
WINERR named_html(const char *, const char *, ushort_t);
WINERR numbered_html(wchar_t, const char *, ushort_t);
WINERR named_icon(const char *, const char *, ushort_t);
WINERR numbered_icon(wchar_t, const char *, ushort_t);
WINERR named_literal_icon(const char *, uchar_t *, size_t, ushort_t);
WINERR numbered_literal_icon(wchar_t, uchar_t *, size_t, ushort_t);
WINERR named_dialog(const char *, RSRC_HDR_TAIL *, DIALOGHEADER *);
WINERR numbered_dialog(wchar_t, RSRC_HDR_TAIL *, DIALOGHEADER *);
WINERR named_dlginclude(const char *, const char *);
WINERR numbered_dlginclude(wchar_t, const char *);
WINERR named_manifest(const char *, const char *, ushort_t);
WINERR numbered_manifest(wchar_t, const char *, ushort_t);
WINERR named_menu(const char *, RSRC_HDR_TAIL *, MENUTREE *);
WINERR numbered_menu(wchar_t, RSRC_HDR_TAIL *, MENUTREE *);
WINERR named_menuex(const char *, RSRC_HDR_TAIL *, MENUTREE *);
WINERR numbered_menuex(wchar_t, RSRC_HDR_TAIL *, MENUTREE *);
WINERR named_msgtable(const char *, const char *);
WINERR numbered_msgtable(wchar_t, const char *);
WINERR named_plugplay(const char *, const char *, ushort_t);
WINERR numbered_plugplay(wchar_t, const char *, ushort_t);
WINERR named_rcdata(const char *, RSRC_HDR_TAIL *, uchar_t *, size_t);
WINERR numbered_rcdata(wchar_t, RSRC_HDR_TAIL *, uchar_t *, size_t);
WINERR named_version(const char *, RSRC_VERFIX *, VERSTRINGTABLE *, VERVARTABLE *);
WINERR numbered_version(wchar_t, RSRC_VERFIX *, VERSTRINGTABLE *, VERVARTABLE *);
WINERR named_vxd(const char *, const char *, ushort_t);
WINERR numbered_vxd(wchar_t, const char *, ushort_t);
WINERR named_generic_named_type(const char *, const char *, const char *, ushort_t);
WINERR named_generic_numbered_type(const char *, wchar_t, const char *, ushort_t);
WINERR numbered_generic_named_type(wchar_t, const char *, const char *, ushort_t);
WINERR numbered_generic_numbered_type(wchar_t, wchar_t, const char *, ushort_t);

/* rcmain.c */
void rc_main(const char *, const char *);

/* utils.c */
int bitcount(uint_t);
int getdigit(int);
uchar_t *newstring(const uchar_t *, size_t, size_t);
void *my_alloc(size_t);
void *my_realloc(void *, size_t);
void my_free(void *);
WINERR my_openfile(const char *, HANDLE *);
WINERR my_createfile(const char *, HANDLE *);
WINERR my_readfile(HANDLE, void *, ulong_t);
WINERR my_writefile(HANDLE, const void *, ulong_t);
WINERR my_seekfile(HANDLE, long, uint_t);
ulong_t my_tellfile(HANDLE);
WINERR my_filesize(const char *, ulong_t *);
WINERR my_deletefile(const char *);
WINERR my_renamefile(const char *, const char *);
bool_t my_isfile(const char *);
WINERR my_copystream(HANDLE, HANDLE);
size_t my_fullpath(char *, const char *, const char *);

/* scanner.c */
ulong_t accept_ulong_expression(ulong_t);
ulong_t accept_ulong(void);
short accept_short(void);
ushort_t accept_ushort(void);
bool_t accept_symbol(enum rctoken);
bool_t trysymbol(enum rctoken);
enum rctoken getsymbol(void);
void setup_scanner(void);

#define my_closefile(h)  CloseHandle(h)

#define ansiupper(a)  CharUpperA(a)

#define ansi2uni(a,b)  MultiByteToWideChar(codepage, 0, (b), -1, (a), 0x7FFFFFF)

#define unisize(s)  ((strlen(s)+1) * sizeof(wchar_t))

#define wstrlen  wcslen
#define wstrsize(s)  ((wstrlen(s)+1) * sizeof(wchar_t))

#ifdef _WIN32
#define PATH_DELIMITER  ";"
#else
#define PATH_DELIMITER  ":"
#endif

/* Globals */
extern SCANNER s;
extern RSRC_HDR_TAIL commontail;
extern HANDLE hmod;
extern HANDLE hfres;
extern int nerrs;
extern int nident;
extern int codepage;
extern int idemode;
extern int quitrun;
extern jmp_buf jumpbuf;
extern FILEINFO *fileinfo_list;
extern INCLINFO *inclinfo_list;
extern errfunc error_callback;
extern inffunc info_callback;
extern BOOL bWantsFile;
extern BOOL bDialog;

#ifdef PODEBUG
__inline void *my_alloc(size_t size)
{
    void *vp = malloc(size);

    if (vp == NULL)
        apperror(RCFATAL(ERROR_OUTOFMEMORY));

    /* Initialize to "known garbage" */
    memset(vp, 0xDD, size);

    return vp;
}

__inline void *my_realloc(void *vp, size_t newsize)
{
    vp = realloc(vp, newsize);

    if (vp == NULL)
        apperror(RCFATAL(ERROR_OUTOFMEMORY));

    return vp;
}

__inline void my_free(void *vp)
{
    if (vp) free(vp);
}
#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _RC_H */
