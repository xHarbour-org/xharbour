#ifndef _PROJECTS_H
#define _PROJECTS_H

/* Windows Project API definitions (Windows CE) */

#ifdef __cplusplus
extern "C" {
#endif

#define PA_MAX_PATHNAME  96

#ifndef CEOID
#define CEOID  DWORD
#endif

typedef enum tagENUM {
    PRJ_ENUM_MEMORY = 0x1,
    PRJ_ENUM_FLASH = 0x2,
    PRJ_ENUM_ALL_DEVICES = 0x4,
    PRJ_ENUM_ALL_PROJ = 0x10,
    PRJ_ENUM_HOME_PROJ = 0x100,
} PRJ_ENUM;

typedef BOOL (CALLBACK *PFNCOLCUSTOK)(HWND, LPVOID);
typedef BOOL (CALLBACK *PROJECTS_ENUMPROC)(DWORD, LPARAM);
typedef BOOL (CALLBACK *PROJECTSFILES_ENUMPROC)(DWORD dwOID, LPARAM lParam);

typedef enum _EFileIDType {
    FILE_ID_TYPE_OID = 0,
    FILE_ID_TYPE_PATH = 1,
    FILE_ID_LAST = 2,
} EFileIDType;

typedef struct _PAstruct {
    EFileIDType m_IDtype;
    union {
        CEOID m_fileOID;
        TCHAR m_szPathname[PA_MAX_PATHNAME];
    };
} PAstruct;

typedef BOOL (CALLBACK *PROJECTS_ENUMPROC_EX)(PAstruct*,LPARAM);
typedef BOOL (CALLBACK *PROJECTSFILES_ENUMPROC_EX)(PAstruct*,LPARAM);

int EnumProjects(PROJECTS_ENUMPROC,DWORD,DWORD,LPARAM);
int EnumProjectsEx(PROJECTS_ENUMPROC_EX,DWORD,DWORD,LPARAM);
int EnumProjectsFiles(PROJECTSFILES_ENUMPROC,DWORD,DWORD,LPTSTR,LPTSTR,LPARAM);
int EnumProjectsFilesEx(PROJECTSFILES_ENUMPROC_EX,DWORD,DWORD,LPTSTR,LPTSTR,LPARAM);
HANDLE FindFirstFlashCard(LPWIN32_FIND_DATA);
BOOL FindNextFlashCard(HANDLE,LPWIN32_FIND_DATA);
HANDLE FindFirstProjectFile(LPCTSTR,LPWIN32_FIND_DATA,DWORD,LPTSTR);
BOOL FindNextProjectFile(HANDLE,LPWIN32_FIND_DATA);

#ifdef __cplusplus
}
#endif

#endif /* _PROJECTS_H */
