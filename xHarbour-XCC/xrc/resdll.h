/****************************************************************************
 *                                                                          *
 * File    : resdll.h                                                       *
 *                                                                          *
 * Purpose : Global definitions for Win32 Resource Compiler DLL.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *           01-11-21  New function enum_include_info() added.              *
 *                                                                          *
 ****************************************************************************/

#ifndef _RESDLL_H
#define _RESDLL_H

#if defined(_RESDLL_)
#define RESDLLAPI __declspec(dllexport)
#else
#define RESDLLAPI __declspec(dllimport)
#endif

/* Error classes */
enum errclass
{
    ERRCLASS_WARNING,
    ERRCLASS_ERROR,
    ERRCLASS_FATAL
};

/* Error reporting callback function */
typedef void (*errfunc)(enum errclass, const char *, int, const char *);

/* Information reporting callback function */
typedef void (*inffunc)(const char *);

/* Resource file enumeration callback function */
typedef int (*enumfilefunc)(const wchar_t *, const wchar_t *, unsigned short, const char *, void *);

/* Include file enumeration callback function */
typedef int (*enuminclfunc)(const char *, void *);

/****** Exported function prototypes ***************************************/

RESDLLAPI void WINAPI set_error_function(errfunc, int);
RESDLLAPI void WINAPI set_information_function(inffunc);
RESDLLAPI void WINAPI delete_standard_include_paths(void);
RESDLLAPI void WINAPI add_include_path(const char *);
RESDLLAPI int WINAPI set_codepage(int);
RESDLLAPI int WINAPI set_language(int);
RESDLLAPI void WINAPI define_symbol(const char *);
RESDLLAPI int WINAPI compile_script(const char *, const char *);
RESDLLAPI int WINAPI enum_file_info(enumfilefunc, void *);
RESDLLAPI int WINAPI enum_include_info(enuminclfunc, void *);
RESDLLAPI void WINAPI set_abort_flag(void);

/****** Exported function typedef's ****************************************/

typedef void (WINAPI *SET_ERROR_FUNCTION)(errfunc, int);
typedef void (WINAPI *SET_INFORMATION_FUNCTION)(inffunc);
typedef void (WINAPI *DELETE_STANDARD_INCLUDE_PATHS)(void);
typedef void (WINAPI *ADD_INCLUDE_PATH)(const char *);
typedef int (WINAPI *SET_CODEPAGE)(int);
typedef int (WINAPI *SET_LANGUAGE)(int);
typedef void (WINAPI *DEFINE_SYMBOL)(const char *);
typedef int (WINAPI *COMPILE_SCRIPT)(const char *, const char *);
typedef int (WINAPI *ENUM_FILE_INFO)(enumfilefunc, void *);
typedef int (WINAPI *ENUM_INCLUDE_INFO)(enuminclfunc, void *);
typedef void (WINAPI *SET_ABORT_FLAG)(void);

#endif // _RESDLL_H

