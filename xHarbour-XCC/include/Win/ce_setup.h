#ifndef _CE_SETUP_H
#define _CE_SETUP_H

/* WCELOAD.EXE - SETUP.DLL definitions (Windows CE) */

#ifdef __cplusplus
extern "C" {
#endif

const TCHAR szINSTALL_INIT[] = TEXT("Install_Init");
const TCHAR szINSTALL_EXIT[] = TEXT("Install_Exit");
const TCHAR szUNINSTALL_INIT[] = TEXT("Uninstall_Init");
const TCHAR szUNINSTALL_EXIT[] = TEXT("Uninstall_Exit");

typedef enum {
    codeINSTALL_INIT_CONTINUE = 0,
    codeINSTALL_INIT_CANCEL
} codeINSTALL_INIT;

typedef enum {
    codeINSTALL_EXIT_DONE = 0,
    codeINSTALL_EXIT_UNINSTALL
} codeINSTALL_EXIT;

typedef enum {
    codeUNINSTALL_INIT_CONTINUE = 0,
    codeUNINSTALL_INIT_CANCEL
} codeUNINSTALL_INIT;

typedef enum {
    codeUNINSTALL_EXIT_DONE = 0
} codeUNINSTALL_EXIT;

typedef codeINSTALL_INIT (*pfnINSTALL_INIT)(HWND,BOOL,BOOL,LPCTSTR);
typedef codeINSTALL_EXIT (*pfnINSTALL_EXIT)(HWND,LPCTSTR,WORD,WORD,WORD,WORD,WORD);
typedef codeUNINSTALL_INIT (*pfnUNINSTALL_INIT)(HWND,LPCTSTR);
typedef codeUNINSTALL_EXIT (*pfnUNINSTALL_EXIT)(HWND);

codeINSTALL_INIT Install_Init(HWND,BOOL,BOOL,LPCTSTR);
codeINSTALL_EXIT Install_Exit(HWND,LPCTSTR,WORD,WORD,WORD,WORD,WORD);
codeUNINSTALL_INIT Uninstall_Init(HWND,LPCTSTR);
codeUNINSTALL_EXIT Uninstall_Exit(HWND);

#ifdef __cplusplus
}
#endif

#endif /* _CE_SETUP_H */

