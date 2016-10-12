/******************************************************************************
*
*  File: DLLFunctions.h
*
*  Author:  Joel Alley
*
*  Date: November 9, 1998
*
*  Description:   This file contains the declaration of the standard, exported
*                 DLL functions.
*
*  Modifications:
******************************************************************************/
#ifndef _h_DllFunctions
#define _h_DllFunctions

//The global reference count on this DLL.
int g_cLock = 0;

STDAPI DllRegisterServer(void);
STDAPI DppUnregisterServer(void);

STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, LPVOID FAR * ppvObj);
STDAPI DllCanUnloadNow();

#endif
