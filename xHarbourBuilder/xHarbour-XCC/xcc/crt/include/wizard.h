/****************************************************************************
 *                                                                          *
 * File    : wizard.h                                                       *
 *                                                                          *
 * Purpose : Definitions for Pelles C Wizard API 1.0.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-04-18  Created                                              *
 *           03-11-23  Added support for Smartphone devices.                *
 *                                                                          *
 ****************************************************************************/

#ifndef _WIZARD_H
#define _WIZARD_H

#if defined(_IDE_)
#define WIZAPI __declspec(dllexport)
#else
#define WIZAPI __declspec(dllimport)
#pragma comment(lib, "wizard.lib")
#endif

/* Project types */
enum WizProjType {
    Project_Win32_GUI = 0,
    Project_Win32_DLL = 1,
    Project_Win32_Library = 2,
    Project_Win32_Console = 3,
    Project_WinCE_Pocket_PC_GUI = 4,
    Project_WinCE_Pocket_PC_DLL = 5,
    Project_WinCE_Pocket_PC_Library = 6,
    Project_WinCE_Smartphone_GUI = 7,
    Project_WinCE_Smartphone_DLL = 8,
    Project_WinCE_Smartphone_Library = 9,
    Project_WinCE_GUI = Project_WinCE_Pocket_PC_GUI,  /* compatibility */
    Project_WinCE_DLL = Project_WinCE_Pocket_PC_DLL,  /* compatibility */
    Project_WinCE_Library = Project_WinCE_Pocket_PC_Library  /* compatibility */
};

/* Step page notifications */
enum WizAction {
    Action_SetActive = 1,
    Action_KillActive = 2,
    Action_UpdateUI = 3
};

/* Step page callback procedure */
typedef BOOL (CALLBACK *WIZSTEPPROC)(HWND, enum WizAction);

/* Write file callback procedure */
typedef void (CALLBACK *WIZFILEPROCA)(PSTR, int);
typedef void (CALLBACK *WIZFILEPROCW)(PWSTR, int);

/****** Function prototypes ************************************************/

BOOL WINAPI WizAddStepA(LPCSTR, WIZSTEPPROC);
HINSTANCE WINAPI WizGetInstanceHandle(void);
BOOL WINAPI WizMain(void);
BOOL WINAPI WizShowSteps(void);
BOOL WINAPI WizWriteFileFromResourceA(LPCSTR, LPCSTR);
BOOL WINAPI WizWriteTextFileFromResourceA(LPCSTR, LPCSTR, WIZFILEPROCA);

WIZAPI BOOL WINAPI WizAddProjectFileA(LPCSTR);
WIZAPI BOOL WINAPI WizGetProjectNameA(LPSTR, int);
WIZAPI BOOL WINAPI WizGetProjectSymbolA(LPCSTR, LPSTR, int);
WIZAPI BOOL WINAPI WizSetProjectSymbolA(LPCSTR, LPCSTR);
WIZAPI BOOL WINAPI WizSetProjectType(enum WizProjType);

#ifdef UNICODE
#error Currently no support for UNICODE - sorry!
#define WizAddStep  WizAddStepW
#define WizAddProjectFile  WizAddProjectFileW
#define WizGetProjectName  WizGetProjectNameW
#define WizGetProjectSymbol  WizGetProjectSymbolW
#define WizSetProjectSymbol  WizSetProjectSymbolW
#define WizWriteFileFromResource WizWriteFileFromResourceW
#define WizWriteTextFileFromResource WizWriteTextFileFromResourceW
typedef WIZFILEPROCW WIZFILEPROC;
#else
#define WizAddStep  WizAddStepA
#define WizAddProjectFile  WizAddProjectFileA
#define WizGetProjectName  WizGetProjectNameA
#define WizGetProjectSymbol  WizGetProjectSymbolA
#define WizSetProjectSymbol  WizSetProjectSymbolA
#define WizWriteFileFromResource WizWriteFileFromResourceA
#define WizWriteTextFileFromResource WizWriteTextFileFromResourceA
typedef WIZFILEPROCA WIZFILEPROC;
#endif  /* UNICODE */

#endif /* _WIZARD_H */
