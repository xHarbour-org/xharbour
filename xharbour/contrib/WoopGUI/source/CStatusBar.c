
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

// CreateStatusBar - creates a status bar and divides it into
//     the specified number of parts.
// Returns the handle to the status bar.
// hwndParent - parent window for the status bar.
// nStatusID - child window identifier.
// hinst - handle to the application instance.
// nParts - number of parts into which to divide the status bar.
HB_FUNC ( WG_CREATESTATUSBAR )
{
    HWND hwndParent = (HWND) hb_parnl(1);    // handle to the Parent window
    HWND hwndSB     = (HWND) hb_parnl(2);    // handle to the StatusBar control
    int  nParts     = ( !ISNIL(3) ? hb_parni(3) : 1 );

    RECT rcClient;
    HLOCAL hloc;
    LPINT lpParts;
    int i, nWidth;

    // Get the coordinates of the parent window's client area.
    GetClientRect(hwndParent, &rcClient);

    // Allocate an array for holding the right edge coordinates.
    hloc = LocalAlloc(LHND, sizeof(int) * nParts);
    lpParts = ( int *) LocalLock(hloc);

    // Calculate the right edge coordinate for each part, and
    // copy the coordinates to the array.
    nWidth = rcClient.right / nParts;
    for (i = 0; i < nParts; i++)
    {
        lpParts[i] = nWidth;
        nWidth += nWidth;
    }

    // Tell the status bar to create the window parts.
    SendMessage(hwndSB, SB_SETPARTS, (WPARAM) nParts, (LPARAM) lpParts);

    // Free the array, and return.
    LocalUnlock(hloc);
    LocalFree(hloc);
}


HB_FUNC ( SETSTATUSBAR )
{
    HWND hwnd     = (HWND)  hb_parnl (1);    // handle to the StatusBar control
    int  iPart    =         hb_parni (2)-1;  // part of statusbar to write
    int  uFlags   =         hb_parni (3);    // options flags
    LPSTR szText  = (LPSTR) hb_parcx(4);      // text to write

    SendMessage(hwnd, SB_SETTEXT, (WPARAM) iPart | uFlags , (LPARAM) (LPSTR) szText );
}

