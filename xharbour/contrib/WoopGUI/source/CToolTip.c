
/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

/*------------------------------------------------------------------------------
* Low Level C Generic Routines
*------------------------------------------------------------------------------*/
#define _WIN32_WINNT 0x0400
#define WINVER 0x0400
#define _WIN32_IE 0x0501

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

/* CREATE A TOOLTIP CONTROL OVER THE ENTIRE WINDOW AREA */
HB_FUNC ( SETTOOLTIP )
{
    HWND hwnd      = (HWND) hb_parnl(1);
    HWND hwndTT    = (HWND) hb_parnl(2);    // handle to the ToolTip control
    char *strTT    = (LPTSTR) hb_parc(3);   // Stringa del tooltip

          // struct specifying info about tool in ToolTip control

                 // struct specifying control classes to register
    INITCOMMONCONTROLSEX iccex;
    TOOLINFO ti;
    unsigned int uid = 0;       // for ti initialization
    LPTSTR lptstr = strTT;
    RECT rect;                  // for client area coordinates

    /* INITIALIZE COMMON CONTROLS */
    iccex.dwICC = ICC_WIN95_CLASSES;
    iccex.dwSize = sizeof(INITCOMMONCONTROLSEX);
    InitCommonControlsEx(&iccex);

    /* GET COORDINATES OF THE MAIN CLIENT AREA */
    GetClientRect (hwnd, &rect);

    /* INITIALIZE MEMBERS OF THE TOOLINFO STRUCTURE */
    ti.cbSize = sizeof(TOOLINFO);
    ti.uFlags = TTF_SUBCLASS;
    ti.hwnd = hwnd;
    //ti.hinst = ghThisInstance;
    ti.uId = uid; //GetDlgCtrlID(hwnd);
    ti.lpszText = lptstr;
        // ToolTip control will cover the whole window
    ti.rect.left = rect.left;
    ti.rect.top = rect.top;
    ti.rect.right = rect.right;
    ti.rect.bottom = rect.bottom;

    /* SEND AN ADDTOOL MESSAGE TO THE TOOLTIP CONTROL WINDOW */
    SendMessage(hwndTT, TTM_ADDTOOL, 0, (LPARAM) (LPTOOLINFO) &ti);
}

























































